unit DataObjects2ICS;

interface

uses classes, DataObjects2, DataObjects2Streamers, SysUtils, RTTI, TypInfo, DataObjects2Utils, generics.collections, system.NetEncoding;

type
{: This class will decode an ICS calendar file into DataObject structure. 
   Note that the reading of this data is done in a generic way, not in a calendar specific way in that it doesn't just look for common calendar properties, it will load all properties.
   Note that even though this is meant to be generic, I have only tested it on a few of the ICS files that I have pulled from different online services.  
   I'm sure there are things that are not implemented.   For example, I didn't implement decoding escaped sequences like "" within a quoted text string. 
   I didn't implement much for date-time shifting based on time zone, etc. 
 }
  TICSStreamer = class(TDataObjStreamerBase)
  private
    procedure RaiseParsingException(aMessage: string);
    procedure DoRead(var Buffer; Count: LongInt);
  public
    constructor Create(aStream: TStream); override;

    class function FileExtension: string; override;
    class function Description: string; override;
    class function GetFileFilter: string; override;
    class function IsFileExtension(aStr: string): boolean; override;
    class function ClipboardPriority: cardinal; override;

    procedure Decode(aDataObj: TDataObj); override;
    procedure Encode(aDataObj: TDataObj); override;
    procedure ApplyOptionalParameters(aParams: TStrings); override;
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

procedure TICSStreamer.RaiseParsingException(aMessage: string);
begin
  raise Exception.Create(aMessage+' when reading an ICS Stream at position='+intToStr(fStream.Position));
end;


constructor TICSStreamer.Create(aStream: TStream);
begin
  inherited;
end;

class function TICSStreamer.GetFileFilter: string;
begin
  result := 'ICS Files (*.ics)|*.ics';
end;

class function TICSStreamer.IsFileExtension(aStr: string): boolean;
begin
  result := SameText(aStr, '.ics') or SameText(aStr, 'ics');
end;


procedure TICSStreamer.ApplyOptionalParameters(aParams: TStrings);
begin
  inherited;
  // no parameters yet so there's nothing to implement here.
end;

class function TICSStreamer.ClipboardPriority: cardinal;
begin
  result := 40;
end;


procedure TICSStreamer.DoRead(var Buffer; Count: LongInt);
begin
  if fStream.Read(Buffer, Count) <> Count then
  begin
    RaiseParsingException('Premature end of stream trying to read '+intToStr(Count)+' bytes');
  end;
end;

procedure TICSStreamer.Decode(aDataObj: TDataObj);
var
  lCurrentLine: integer;
  lSL: TStringList;

  procedure ParseLine(aLocalDataObj: TDataObj; aSectionName: string);
  var
    lLine: string;
    lPos: integer;
    lStartPos: integer;
    lName: string;
    lInQuote: boolean;
    lValue: string;
    lAttributes: string;
    i: Integer;
    lAttributeList: TStringList;
    lValueAtt: string;
    lSectionName: string;

    function GetNextLine: string;    // get the next line unfolded.  returns empty string if no line can be retrieved.
    var
      lLinePreFolding: string;
    begin
      result := '';
      while lCurrentLine<lSL.count do
      begin
        lLinePreFolding := lSL.Strings[lCurrentLine];
        inc(lCurrentLine);
        if lLinePreFolding[1] = ' ' then
        begin
          result := result + copy(lLinePreFolding, 2, length(lLinePreFolding));   // this line is a subsequent folding of the previous line. so merge them together.  The first character being a space identifies this.
        end
        else
        begin
          if result = '' then
          begin
            result := lLinePreFolding;
          end
          else
          begin
            dec(lCurrentLine);
            break;
          end;
        end;
      end;
    end;

    function LocalStrToDate(aStr: string): TDate;
    var
      lYear, lMonth, lDay: integer;
    begin
      // YYYYMMDD      
      lYear := StrToIntDef(copy(aStr,1,4), 0);
      lMonth := StrToIntDef(copy(aStr,5,2), 0);
      lDay := StrToIntDef(copy(aStr,7,2), 0);
      try
        result := EncodeDate(lYear, lMonth, lDay);
      except
        result := 0;
      end;
    end;

    function LocalStrToTime(aStr: string): TDateTime;
    var
      lHour, lMin, lSec: integer;
      
      function isNum(aChar: char): boolean;
      begin
        result := (aChar >= '0') and (aChar <= '9');
      end;
    begin
      result := 0;
      // YYYYMMDD         
      try
        if isNum(aStr[1]) and isNum(aStr[2]) and isNum(aStr[3]) and isNum(aStr[4]) and isNum(aStr[5]) and isNum(aStr[6]) then
        begin       
          lHour := StrToInt(copy(aStr,1,2));
          lMin := StrToInt(copy(aStr,3,2));
          lSec := StrToInt(copy(aStr,5,2));
        
          result := lHour/24 + lMin/24/60 + lSec/24/60/60;
        end;
      except
        result := 0;
      end;    
    end;

    function LocalStrToDateTime(aStr: string): TDateTime;
    begin
      // 19980119T070000Z   Note that the Z is optional and it declares it as standard datetime. 
      if aStr[9] = 'T' then           
        result := LocalStrToDate(copy(aStr, 1, 8)) + LocalStrToTime(copy(aStr, 10, 6))
      else
        result := LocalStrToDate(copy(aStr, 1, 8));
    end;

    function LocalStrToBoolean(aStr: string): boolean;
    begin
      result := SameText(aStr, 'TRUE');
    end;

    function LocalStrToInt(aStr: string): int64;
    begin
      result := StrToInt64(aStr);
    end;

    function LocalStrToDouble(aStr: string): double;
    begin
      result := StrToFloat(aStr);
    end;

    procedure LocalStrToBinary(aStr: string; aBinary: TDataBinary);
    var
      lStringStream: TStringStream;
    begin
      // expect aStr to be base64
      lStringStream:=TStringStream.Create(aStr);
      try
        TNetEncoding.Base64.Decode(lStringStream, aBinary);
      finally
        lStringStream.Free;
      end;
    end;

    procedure LocalStrToText(aStr: string; aDataObj: TDataObj);
    var
      i: integer;
      lChar: char;
      lCurrentDataObj: TDataObj;
      lString: String;

      procedure PostCurrentLine;
      begin
        if lCurrentDataObj.DataType.Code = cDataTypeStringList then
          lCurrentDataObj.AsStringList.Add(lLine)
        else if lCurrentDataObj.DataType.Code = cDataTypeArray then
          lCurrentDataObj.AsArray.NewSlot.AsString := lLine
        else if lCurrentDataObj.DataType.Code = cDataTypeString then
        begin
          // adding another string so we convert to a stringlist.
          lString := lCurrentDataObj.AsString;
          lCurrentDataObj.Clear;
          lCurrentDataObj.AsStringList.Add(lString);
          lCurrentDataObj.AsStringList.Add(lLine);
        end
        else
        begin
          lCurrentDataObj.AsString := lLine;
        end;

        lLine := '';
        lStartPos := i;
      end;

    begin
      // this normally takes aStr and puts it into aDataObj as a straight up string.  however, if a \n <newline> is found in the aStr, then
      // we will put aDataObj to be a stringList instead and each string separated by the newline will be in a newline in the stringlist. 

      //Also, if aStr actually contains more than one item.  IE) items separated by a comma character, then we will have an array of strings.
      //Note that each item in this collection could end up being a straight up string or it could end up being a string list all depending on if a \n was encountered or not. 


      i := 1;
      lLine := '';
      lStartPos := 1;
      lCurrentDataObj := aDataObj;
      while i <= length(aStr) do
      begin
        if aStr[i] = '\' then
        begin
          // we are getting an escaped character so get the next one to determine that. 
          inc(i);
          if i>length(aStr) then
          begin
            // here we are in a situation where the last character available is the \ char.  There should be a following char but there isn't.  error condition really but we will just take up to this point.

          end;

          lChar := aStr[i];
          //   "\\" / "\;" / "\," / "\N" / "\n")
          if (lChar='\') or (lChar=';') or (lChar=',') then
          begin
            lLine := lLine + copy(aStr, lStartPos, i-lStartPos-1) + lChar;
            lStartPos := i+1;
          end
          else if (lChar='N') or (lChar='n') then
          begin
            // if our current object is an array, then we just take this \n as a carriage return to put into the current line.
            // However, if our current object is a stringList or a string, then the carriage return becomes a new line in a stringList.
            if lcurrentDataObj.DataType.Code = cDataTypeArray then
            begin
              lLine := lLine + copy(aStr, lStartPos, i-lStartPos-1) + #13;
              lStartPos := i+1;
            end
            else
            begin
              lLine := lLine + copy(aStr, lStartPos, i-lStartPos-1);
              inc(i);
              PostCurrentLine;
            end;
          end;
          inc(i); 
        end
        else if aStr[i] = ',' then
        begin
          // we have a comma which means we are now going to be creating an array of strings.
          // now the currentDataObject could already be a stringList because we previously saw \n.
          // We need to apply are current line of data collected so far if there is any and put it into the data object as a string or add the string to the already started string list.
          // Then, we convert it to an array where the first item in the array is that stringList.    
          // Note that the .AsArray call will do that for us.   But, we must also add a new dataObject to the array to become our new currentObject that we put subsequent data into if there is any.
          lLine := lLine + copy(aStr, lStartPos, i-lStartPos);
          inc(i);
          PostCurrentLine;
          lCurrentDataObj.AsArray;   // converting the existing string that was created on the above post to an array if need be.
        end
        else
        begin
          // normal character just moves our index up to look at the next one.
          inc(i);
        end;
      end;

      if (i>lStartPos) or (length(lLine)>0) then                // pick up the last set of characters to make the last line unless the previous line ended on a \n or a , so there really is nothing left for a final line.
      begin
        lLine := lLine + copy(aStr, lStartPos, i-lStartPos);
        PostCurrentLine;
      end;
    end;
    
    
  begin
    lLine := GetNextLine;
    while lLine <> '' do
    begin
      if lLine.StartsWith('BEGIN:') then
      begin
        lSectionName := Trim(lLine.Substring(6));
        ParseLine(aLocalDataObj.AsFrame.NewSlot(lSectionName).AsArray.newSlot, lSectionName);              // RECURSION HAPPENING HERE.
      end
      else if lline.StartsWith('END:') then
      begin
        // Note that we really should be comparing to make sure that the END object identifier we are reading here matches the objectname that got us into this function.  Error if it doesn't match.
        lSectionName := Trim(lLine.Substring(4));

        if (lSectionName <> aSectionName) then
        begin
          raise exception.Create('Error parsing ICS.  Encountered END section '+lSectionName+' but expected END for section '+aSectionName);
        end;

        exit;
      end
      else
      begin
        lAttributeList := nil;
        try      
          // reading normal values for this object.
          //  look for the colon to break the line up into the nameIdentifier and it's value.
          lStartPos := 1;
          lInQuote := false;
          //The name could possibly have ;attribute=value attributes.  We should look at those attributes to see if it gives us instructions on how to treat the value we are reading.
          // Note that we could have attributes that have values in double quoted text so that the ; character can be within the attribute value.
          for i := 1 to length(lLine) do
          begin
            if lInQuote then
            begin
              if lLine[i] = '"' then
              begin
                // we can end the inquote section as long as we don't see an immediate next character that's a quote.   FINISH that check.
                lInQuote := false;
              end;
            end
            else
            begin
              if (lLine[i] = ';') or (lLine[i] = ':') then
              begin
                // have a separator.
                if not assigned(lAttributeList) then
                begin
                  lName := copy(lLine, 1, i-1);
                  lAttributeList := TStringList.create;
                end
                else
                begin
                  lAttributeList.Add(copy(lLine, lStartPos, i-lStartPos));
                end;
                lStartPos := i+1;

                if (lLine[i] = ':') then
                begin
                  lValue := copy(lLine, i+1, length(lLine));    // Everything from colon and beyond is the value.
                  break;
                end;
              end
              else if lLine[i] = '"' then
              begin
                lInQuote := true;
              end;
            end;
          end;


          if (lName<>'') and (lValue<>'') then
          begin
            // look at the attributes to see if there are some that give us some info on how to treat the value.
            if assigned(lAttributeList) then
            begin
              // see if we have a value attribute because that tells us if we need to treat the incoming value different than a string.
              lValueAtt := lAttributeList.Values['VALUE'];
            end;

            // possible value types: "BINARY" "BOOLEAN" "CAL-ADDRESS" "DATE" "DATE-TIME" "DURATION" "FLOAT" "INTEGER" "PERIOD" "RECUR"
            // "TEXT" "TIME" "URI" "UTC-OFFSET" x-name
            if sameText(lValueAtt, '') then
              LocalStrToText(lValue, aLocalDataObj.AsFrame.NewSlot(lName))    // This will be the majority of the items we load. Just for efficiency, we don't let it fall through to the else section below. 
            else if sameText(lValueAtt, 'DATE') then
              aLocalDataObj.AsFrame.NewSlot(lName).AsDate := LocalStrToDate(lValue)              
            else if sameText(lValueAtt, 'INTEGER') then
              aLocalDataObj.AsFrame.NewSlot(lName).AsDouble := LocalStrToInt(lValue)              
            else if sameText(lValueAtt, 'BOOLEAN') then
              aLocalDataObj.AsFrame.NewSlot(lName).Asboolean := LocalStrToBoolean(lValue)              
            else if sameText(lValueAtt, 'DATE-TIME') then
              aLocalDataObj.AsFrame.NewSlot(lName).AsDateTime := LocalStrToDateTime(lValue)              
            else if sameText(lValueAtt, 'TIME') then
              aLocalDataObj.AsFrame.NewSlot(lName).AsDateTime := LocalStrToTime(lValue)              
            else if sameText(lValueAtt, 'DATE') then
              aLocalDataObj.AsFrame.NewSlot(lName).AsDateTime := LocalStrToDate(lValue)              
            else if sameText(lValueAtt, 'FLOAT') then
              aLocalDataObj.AsFrame.NewSlot(lName).AsDouble := LocalStrToDouble(lValue)              
            else if sameText(lValueAtt, 'BINARY') then
            begin
              // first, to be compliant, we MUST have another attribute that labels this text as Base64 encoded. 
              if lAttributeList.Values['ENCODING'] = 'BASE64' then
                LocalStrToBinary(lValue,  aLocalDataObj.AsFrame.NewSlot(lName).AsBinary)
              else
                LocalStrToText(lValue, aLocalDataObj.AsFrame.NewSlot(lName));   // we have to fall back to loading this as just a string. 
            end
          //  else if sameText(lValueAtt, 'CAL-ADDRESS') then   Not Implemented
          //  else if sameText(lValueAtt, 'PERIOD') then        Not Implemented
          //  else if sameText(lValueAtt, 'RECUR') then         Not Implemented
          //  else if sameText(lValueAtt, 'URI') then           Not Implemented
          //  else if sameText(lValueAtt, 'UTC-OFFSET') then    Not Implemented
          //  else if sameText(lValueAtt, 'x-name') then        Not Implemented
          //                  ;
            else
            begin   
              LocalStrToText(lValue, aLocalDataObj.AsFrame.NewSlot(lName));
            end;
          end;
          
        finally
          FreeAndNil(lAttributeList);
        end;
      end;
      lLine := GetNextLine;
    end;
  end;


begin
  // see https://icalendar.org/RFC-Specifications/iCalendar-RFC-5545/

  lSL:=TStringList.create;
  try
    lSL.OwnsObjects := false;
    lSL.LoadFromStream(fStream);   // NOTE that this brings the whole file into memory which is not what we want to do long term.  Fix this.
    lCurrentLine:=0;
    ParseLine(aDataObj,'');
  finally
    lSL.Free;
  end;
end;


class function TICSStreamer.Description: string;
begin
  result := 'Internet Calendaring and Scheduling (iCalendar).  https://datatracker.ietf.org/doc/html/rfc5545';
end;

procedure TICSStreamer.Encode(aDataObj: TDataObj);
var
  i,j: integer;
  lItem: TDataObj;
  lSlot: TDataObj;
  lFrame: TDataFrame;
begin
  //Right now, we can really only encode an array of frames and we need to make a full pass the the dataObject to check that and to generate a set of columns.
  if aDataObj.DataType.Code <> cDataTypeArray then
    raise Exception.Create('Can not export to ICS because the data must be structured as an array of frames.  This object is a '+aDataObj.DataTypeString);

  try
    for i := 0 to aDataObj.AsArray.Count-1 do
    begin
      lItem := aDataObj.AsArray.slots[i];
      if lItem.DataType.Code <> cDataTypeFrame then
        raise exception.Create('Can not export to ICS because the data must be structured as an array of frames and item '+intToStr(i)+' is not a frame');

      lFrame := lItem.AsFrame;
      for j := 0 to lFrame.Count-1 do
      begin
        lSlot := lFrame.Slots[j];

        //FINISH
      end;
    end;
  finally
    //cleanup something?
  end;
end;


class function TICSStreamer.FileExtension: string;
begin
  result := 'ics';
end;

initialization
  RegisterDataObjStreamer(TICSStreamer);

end.
