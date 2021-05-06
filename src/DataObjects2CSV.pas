unit DataObjects2CSV;

interface

uses classes, DataObjects2, DataObjects2Streamers, SysUtils, RTTI, TypInfo, DataObjects2Utils, generics.collections;

type
{: This class will decode a CSV file into an array of frames. }
  TCSVStreamer = class(TDataObjStreamerBase)
  private
    FAutoDetectDataTypes: boolean;
    FAddEmptyValuesAsNilSlots: boolean;
    FFirstRowIsFieldNames: boolean;
    FConsumeAllRemainingDataIntoLastField: boolean;

    procedure RaiseParsingException(aMessage: string);
    procedure DoRead(var Buffer; Count: LongInt);
    procedure SetAutoDetectDataTypes(const Value: boolean);
    procedure SetAddEmptyValuesAsNilSlots(const Value: boolean);
    procedure SetFirstRowIsFieldNames(const Value: boolean);
    procedure SetConsumeAllRemainingDataIntoLastField(const Value: boolean);
  public
    constructor Create(aStream: TStream); override;

    class function FileExtension: string; override;
    class function GetFileFilter: string; override;
    class function IsFileExtension(aStr: string): boolean; override;
    class function ClipboardPriority: cardinal; override;

    function Clone: TDataObjStreamerBase; override;
    procedure Decode(aDataObj: TDataObj); override;
    procedure Encode(aDataObj: TDataObj); override;
    procedure ApplyOptionalParameters(aParams: TStrings); override;

    // If this is true, then we will try to convert dates, dateTimes, integers, floatingpoint numbers, GUIDs, etc. into their correct dataTypes.  Note that if the incoming field is surrounded
    // by double quotes then that field will only be converted to a string data type.
    property AutoDetectDataTypes: boolean read FAutoDetectDataTypes write SetAutoDetectDataTypes;
    property AddEmptyValuesAsNilSlots: boolean read FAddEmptyValuesAsNilSlots write SetAddEmptyValuesAsNilSlots;
    property FirstRowIsFieldNames: boolean read FFirstRowIsFieldNames write SetFirstRowIsFieldNames;
    property ConsumeAllRemainingDataIntoLastField: boolean read FConsumeAllRemainingDataIntoLastField write SetConsumeAllRemainingDataIntoLastField;
  end;

implementation

const
   BLOCK_SIZE         = 128;
   MAX_BLOCK_SIZE     = 1024 * 8;


   { parser state }
const
   PS_IN_NEW_LINE      = 1;
   PS_INSIDE_FIELD     = 2;
   PS_OUTSIDE_FIELD    = 3;



//ResourceString

procedure TCSVStreamer.RaiseParsingException(aMessage: string);
begin
  raise Exception.Create(aMessage+' when reading a CSV Stream at position='+intToStr(fStream.Position));
end;

procedure TCSVStreamer.SetAddEmptyValuesAsNilSlots(const Value: boolean);
begin
  FAddEmptyValuesAsNilSlots := Value;
end;

procedure TCSVStreamer.SetAutoDetectDataTypes(const Value: boolean);
begin
  FAutoDetectDataTypes := Value;
end;

procedure TCSVStreamer.SetConsumeAllRemainingDataIntoLastField( const Value: boolean);
begin
  FConsumeAllRemainingDataIntoLastField := Value;
end;

procedure TCSVStreamer.SetFirstRowIsFieldNames(const Value: boolean);
begin
  FFirstRowIsFieldNames := Value;
end;

function TCSVStreamer.Clone: TDataObjStreamerBase;
begin
  result := TCSVStreamer.create(nil);    // create instance and copy the properties.
  TCSVStreamer(result).FAutoDetectDataTypes := FAutoDetectDataTypes;
  TCSVStreamer(result).FAddEmptyValuesAsNilSlots := FAddEmptyValuesAsNilSlots;
  TCSVStreamer(result).FFirstRowIsFieldNames := FFirstRowIsFieldNames;
end;



constructor TCSVStreamer.Create(aStream: TStream);
begin
  inherited;

  //Setup our defaults.
  FAutoDetectDataTypes := true;
  FAddEmptyValuesAsNilSlots:=true;
  FFirstRowIsFieldNames:=true;   //if this true, the we produce an array of frames.  if this is false then we produce an array of arrays.
  FConsumeAllRemainingDataIntoLastField:=true;
end;

class function TCSVStreamer.GetFileFilter: string;
begin
  result := 'CSV Files (*.csv)|*.csv';
end;

class function TCSVStreamer.IsFileExtension(aStr: string): boolean;
begin
  result := SameText(aStr, '.csv') or SameText(aStr, 'csv');
end;


procedure TCSVStreamer.ApplyOptionalParameters(aParams: TStrings);
var
  i: Integer;
  lBoolean: boolean;
begin
  inherited;
  i := 0;
  while i < aParams.Count do
  begin
    if SameText(aParams[i], '-AutoDetectDataTypes') and (i < aParams.Count-1) then
    begin
      if TryStrToBool(aParams[i+1], lBoolean) then
        AutoDetectDataTypes := lBoolean;
      inc(i);
    end
    else if SameText(aParams[i], '-AddEmptyValuesAsNilSlots') and (i < aParams.Count-1) then
    begin
      if TryStrToBool(aParams[i+1], lBoolean) then
        AddEmptyValuesAsNilSlots := lBoolean;
      inc(i);
    end
    else if SameText(aParams[i], '-FirstRowIsFieldNames') and (i < aParams.Count-1) then
    begin
      if TryStrToBool(aParams[i+1], lBoolean) then
        FirstRowIsFieldNames := lBoolean;
      inc(i);
    end ;
    inc(i);
  end;
end;

class function TCSVStreamer.ClipboardPriority: cardinal;
begin
  result := 40;
end;


procedure TCSVStreamer.DoRead(var Buffer; Count: LongInt);
begin
  if fStream.Read(Buffer, Count) <> Count then
  begin
    RaiseParsingException('Premature end of stream trying to read '+intToStr(Count)+' bytes');
  end;
end;

procedure TCSVStreamer.Decode(aDataObj: TDataObj);
var
  lSL: TStringList;
  lFields: TStringList;
  lValues: TStringList;
  i,j,k: Integer;
  lDelimeter: Char;
  lFrame: TDataFrame;
  lArray: TDataArray;
  lString: string;
  lInteger: integer;
  lInt64: int64;
  lBoolean: boolean;
  lDouble: Double;
  lDateTime: TDateTime;
  lError: integer;

  procedure ParseLine(aLine: string; aValues: TStrings);
  var
    i: integer;
    lChar: Char;
    lState: integer;
    lValue: string;
    lWasInQuote: boolean;
  const
    cNormal = 0;
    cInQuote = 1;

    procedure AddValue;
    begin
      aValues.AddObject(lValue, TObject(lWasInQuote));    // if we were InQuote for this value, then we put that flag into the Object portion of the values.
      lValue := '';
    end;
  begin
    lState := 0;   // normal processing of characters
    lValue := '';
    i:=1;
    while i <= length(aLine) do
    begin
      lChar := aLine[i];

      case lState of
        cNormal: begin
          lWasInQuote := false;
          if lChar = lDelimeter then
          begin
            AddValue;
          end
          else if lChar = '"' then
          begin
            lState := cInQuote;
          end
          else
          begin
            lValue := lValue + lChar;
          end;
        end;

        cInQuote: begin
          lWasInQuote := true;
          // need to accept all characters until we hit the ending quote.
          if lChar = '"' then
          begin
            if i<length(aLine) then
            begin
              // look ahead to see if the next character is a double quote too.  If it is then this pair of double quotes should be taken as a double quote character in the body of the string.
              if aLine[i+1] = '"' then
              begin
                lValue := lValue + '"';
                inc(i);
              end
              else
              begin
                lState := cNormal;     // this double quote ended a double quoting pair
              end;
            end
            else
            begin
              lState := cNormal;  // last character on the line is the double quote so automatically accept it as the end of our double quote pairs.
            end;
          end
          else
          begin
            lValue := lValue + lChar;
          end;
        end;
      end;

      inc(i);
    end;

    AddValue;  // take any characters that were at the end of the line as the last field.
  end;


  function TryInteger: boolean;
  begin
    Val(lString, lInteger, lError);
    result := (lError = 0);
  end;

  function TryInt64: boolean;
  begin
    Val(lString, lInt64, lError);
    result := (lError = 0);
  end;

  function TryDouble: boolean;
  begin
    Val(lString, lDouble, lError);
    result := (lError = 0);
  end;

  function TryBoolean: boolean;
  begin
    lBoolean := false;
    result := sameText(lString, 'True') or sameText(lString, 'T');
    if result then
    begin
      lBoolean := true;
    end
    else
    begin
      result := sameText(lString, 'False') or sameText(lString, 'F');
    end;
  end;



begin
  lDelimeter := ',';   // hard coded for now

  // read a CSV File one line at a time.  Expect the first line to be the column names.
  lFields:=TStringList.create;    // holds the field header names
  lValues:=TStringList.create;    // holds the field values for a parsed line.
  lSL:=TStringList.create;
  try
    lSL.OwnsObjects := false;
    lSL.LoadFromStream(fStream);   // NOTE that this brings the whole file into memory which is not what we want to do long term.  Fix this.
    if lSL.Count>0 then
    begin
      if FFirstRowIsFieldNames then
      begin
        //Process into an Array of Frames.
        // read the fieldnames row
        ParseLine(lSL[0], lFields);

        // read the records
        for i := 1 to lSL.Count-1 do
        begin
          lValues.Clear;
          ParseLine(lSL[i], lValues);

          lFrame := aDataObj.AsArray.NewSlot.AsFrame;

          for j := 0 to lFields.count - 1 do
          begin
            if j < lValues.count then
            begin
              // If we are trying to load data on the very last field, then we have an option to allow the last field to completely load all the rest of the line, even if it contains commas that would normally delimit to more fields.
              if self.FConsumeAllRemainingDataIntoLastField and (j = lFields.count-1) then
              begin
                lString := '';
                for k := j to lValues.count-1 do
                begin
                  if k=j then
                    lString := lValues[k]
                  else
                    lString := lString + ',' + lValues[k];    // merge the rest of the remaining items back into one value.
                end;
              end
              else
              begin
                lString := lValues[j];
              end;

              if lString <> '' then
              begin
                if FAutoDetectDataTypes and (lValues.Objects[j] = nil) then     //lValues.objects is set to a non nil value if the string put in that position was down so being surrounded by double quotes.
                begin
                  if TryInteger then
                    lFrame.NewSlot(lFields[j]).AsInt32 := lInteger
                  else if TryInt64 then
                    lFrame.NewSlot(lFields[j]).AsInt64 := lInt64
                  else if TryBoolean then
                    lFrame.NewSlot(lFields[j]).AsBoolean := lBoolean
                  else if TryDouble then
                    lFrame.NewSlot(lFields[j]).AsDouble := lDouble
                  else if TryStrToDateTime(lString, lDateTime) then
                    lFrame.NewSlot(lFields[j]).AsDateTime := lDateTime
                  else if TryStrToDate(lString, lDateTime) then
                    lFrame.NewSlot(lFields[j]).AsDate := lDateTime
                  else if TryStrToTime(lString, lDateTime) then
                    lFrame.NewSlot(lFields[j]).AsTime := lDateTime
                  else
                    lFrame.NewSlot(lFields[j]).AsString := lString;
                end
                else
                  lFrame.NewSlot(lFields[j]).AsString := lString;
              end
              else
              begin
                if AddEmptyValuesAsNilSlots then
                  lFrame.NewSlot(lFields[j]);                        // add new slot and doesn't assign any data to it so it ends up being nil.
              end;

            end;
          end;
        end;
      end
      else
      begin
        //Process into an Array of Arrays.
        // read the records
        for i := 0 to lSL.Count-1 do
        begin
          lValues.Clear;
          ParseLine(lSL[i], lValues);

          lArray := aDataObj.AsArray.NewSlot.AsArray;

          for j := 0 to lValues.count - 1 do
          begin
            lString := lValues[j];
            if lString <> '' then
            begin
              if FAutoDetectDataTypes and (lValues.Objects[j] = nil) then     //lValues.objects is set to a non nil value if the string put in that position was down so being surrounded by double quotes.
              begin
                if TryInteger then
                  lArray.NewSlot.AsInt32 := lInteger
                else if TryInt64 then
                  lArray.NewSlot.AsInt64 := lInt64
                else if TryBoolean then
                  lArray.NewSlot.AsBoolean := lBoolean
                else if TryDouble then
                  lArray.NewSlot.AsDouble := lDouble
                else if TryStrToDateTime(lString, lDateTime) then
                  lArray.NewSlot.AsDateTime := lDateTime
                else if TryStrToDate(lString, lDateTime) then
                  lArray.NewSlot.AsDate := lDateTime
                else if TryStrToTime(lString, lDateTime) then
                  lArray.NewSlot.AsTime := lDateTime
                else
                  lArray.NewSlot.AsString := lString;     // right now we are only using all values as strings but we can improve this someday to do typing.
              end
              else
                lArray.NewSlot.AsString := lString;
            end
            else
            begin
              // in the case of array, empty strings must still be added no matter what the fAddEmptyValuesAsNilSlots is set to because we need to reserve that position.
              if AddEmptyValuesAsNilSlots then
                lArray.NewSlot                        // add new slot and doesn't assign any data to it so it ends up being nil.
              else
                lArray.NewSlot.AsString := '';        // add a new slot but make it be an empty string.
            end;
          end;
        end;
      end;
    end;
  finally
    lFields.Free;
    lValues.Free;
    lSL.Free;
  end;
end;


procedure TCSVStreamer.Encode(aDataObj: TDataObj);
var
  i: Integer;
  lItem: TDataObj;
  lFrame: TDataFrame;
  lSlot: TDataObj;

  j: Integer;
  lFieldNames: TStringList;
  lFieldNamesUpper: TStringList;

  function IsExportableSlot(aSlot: TDataObj): boolean;
  begin
    result := not((aSlot.DataType.Code = cDataTypeFrame) or (aSlot.DataType.Code = cDataTypeArray) or
                  (aSlot.DataType.Code = cDataTypeSparseArray) or (aSlot.DataType.Code = cDataTypeBinary) or
                  (aSlot.DataType.Code = cDataTypeObject));
  end;

  procedure WriteString(aStr: string; aEncloseInQuotes: boolean);
  var
    lStr: string;
    lBytes: TBytes;
  begin
    lStr := StringReplace(aStr, '"', '""', [rfReplaceAll]);
    lStr := StringReplace(lStr, #13+#10, '\n', [rfReplaceAll]);
    lStr := StringReplace(lStr, #13, '\n', [rfReplaceAll]);
    lStr := StringReplace(lStr, #10, '\n', [rfReplaceAll]);
    if aEncloseInQuotes then
      lStr := '"' + lStr + '"';
    lBytes := TEncoding.UTF8.GetBytes(lStr);          // finish, make the choice of encoding configurable.
    try
      fStream.Write(lBytes, length(lBytes));
    finally
      SetLength(lBytes, 0);
    end;
  end;

  procedure WriteStringLiteral(aStr: string);
  var
    lBytes: TBytes;
  begin
    lBytes := TEncoding.UTF8.GetBytes(aStr);          // finish, make the choice of encoding configurable.
    try
      fStream.Write(lBytes, length(lBytes));
    finally
      SetLength(lBytes, 0);
    end;
  end;

  function FieldExists(aFieldName: string): boolean;
  begin
    // we are going to use our own comparison mechanism here to be just a tad faster and to have more control
    result := lFieldNamesUpper.indexOf(UpperCase(aFieldName)) >= 0;
  end;

  procedure AddFieldName(aFieldName: string);
  begin
    lFieldNamesUpper.Add(UpperCase(aFieldName));
    lFieldNames.Add(aFieldName);
  end;

begin
  //Right now, we can really only encode an array of frames and we need to make a full pass the the dataObject to check that and to generate a set of columns.
  if aDataObj.DataType.Code <> cDataTypeArray then
    raise Exception.Create('Can not export to CSV because the data must be structured as an array of frames.  This object is a '+aDataObj.DataTypeString);

  lFieldNamesUpper:=TStringList.create;
  lFieldNames := TStringList.create;
  try
    for i := 0 to aDataObj.AsArray.Count-1 do
    begin
      lItem := aDataObj.AsArray.slots[i];
      if lItem.DataType.Code <> cDataTypeFrame then
        raise exception.Create('Can not export to CSV because the data must be structured as an array of frames and item '+intToStr(i)+' is not a frame');

      lFrame := lItem.AsFrame;
      for j := 0 to lFrame.Count-1 do
      begin
        lSlot := lFrame.Slots[j];
        if IsExportableSlot(lSlot) and not FieldExists(lFrame.Slotname(j)) then
        begin
          AddFieldName(lFrame.Slotname(j));
        end;
      end;
    end;

    // now that we have a full list of field names to export to the CSV, let's go through all the data and export it.
    for i := 0 to lFieldNames.Count-1 do
    begin
      if i>0 then
        WriteString(',', false);
      WriteString(lFieldNames[i], true);
    end;
    WriteStringLiteral(#13+#10);  // crlf

    for i := 0 to aDataObj.AsArray.Count-1 do
    begin
      lFrame := aDataObj.AsArray.slots[i].AsFrame;
      for j := 0 to lFieldNames.Count-1 do
      begin
        if j>0 then
          WriteString(',', false);
        if lFrame.FindSlot(lFieldNames[j], lSlot) then
        begin
          WriteString(lSlot.AsString, not ( (lSlot.DataType.Code = cDataTypeByte) or (lSlot.DataType.Code = cDataTypeInt32) or (lSlot.DataType.Code = cDataTypeInt64) or
                                            (lSlot.DataType.Code = cDataTypeSingle) or (lSlot.DataType.Code = cDataTypeDouble) or (lSlot.DataType.Code = cDataTypeDecimal128)) );
        end
      end;
      WriteStringLiteral(#13+#10);  // crlf
    end;
  finally
    lFieldNames.Free;
    lFieldNamesUpper.Free;
  end;
end;


class function TCSVStreamer.FileExtension: string;
begin
  result := 'csv';
end;

initialization
  RegisterDataObjStreamer(TCSVStreamer);

end.
