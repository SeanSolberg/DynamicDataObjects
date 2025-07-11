unit DataObjConverters;

// This unit implements a set of converters that will convert data in a data object from one  type to another.  IE) convert a symbol to a string.  convert a TStringList to a string, etc.

interface
  uses Classes, SysUtils, DataObjects2, generics.collections, System.NetEncoding, System.DateUtils;

type
  TDataObjectConverterBase = class
  public
    class function Title: string; virtual; abstract;
    class function Description: string; virtual; abstract;
    class procedure ExecuteConversion(aDataObj: TDataObj); virtual; abstract;
    class function AppliesToDataType(aDataObj: TDataObj): boolean; virtual; abstract;
    class function AppliesToSpecifically(aDataObj: TDataObj): boolean; virtual; abstract;
  end;
  TDataObjectConverterBaseClass = class of TDataObjectConverterBase;

  TDataObjConverterRegistry = class(TList<TDataObjectConverterBaseClass>)
  public
    procedure CopyFrom(aSourceList: TDataObjConverterRegistry);
  end;



  TConvertStringListToArrayOfStrings = class(TDataObjectConverterBase)
  public
    class function Title: string; override;
    class function Description: string; override;
    class procedure ExecuteConversion(aDataObj: TDataObj); override;
    class function AppliesToDataType(aDataObj: TDataObj): boolean; override;
    class function AppliesToSpecifically(aDataObj: TDataObj): boolean; override;
  end;
  TConvertArrayOfStringsToStringList = class(TDataObjectConverterBase)
  public
    class function Title: string; override;
    class function Description: string; override;
    class procedure ExecuteConversion(aDataObj: TDataObj); override;
    class function AppliesToDataType(aDataObj: TDataObj): boolean; override;
    class function AppliesToSpecifically(aDataObj: TDataObj): boolean; override;
  end;
  TConvertBase64StringToBinary = class(TDataObjectConverterBase)
  public
    class function Title: string; override;
    class function Description: string; override;
    class procedure ExecuteConversion(aDataObj: TDataObj); override;
    class function AppliesToDataType(aDataObj: TDataObj): boolean; override;
    class function AppliesToSpecifically(aDataObj: TDataObj): boolean; override;
  end;
  TConvertBinaryToBase64String = class(TDataObjectConverterBase)
  public
    class function Title: string; override;
    class function Description: string; override;
    class procedure ExecuteConversion(aDataObj: TDataObj); override;
    class function AppliesToDataType(aDataObj: TDataObj): boolean; override;
    class function AppliesToSpecifically(aDataObj: TDataObj): boolean; override;
  end;
  TConvertToString = class(TDataObjectConverterBase)
  public
    class function Title: string; override;
    class function Description: string; override;
    class procedure ExecuteConversion(aDataObj: TDataObj); override;
    class function AppliesToDataType(aDataObj: TDataObj): boolean; override;
    class function AppliesToSpecifically(aDataObj: TDataObj): boolean; override;
  end;
  TConvertToInteger = class(TDataObjectConverterBase)
  public
    class function Title: string; override;
    class function Description: string; override;
    class procedure ExecuteConversion(aDataObj: TDataObj); override;
    class function AppliesToDataType(aDataObj: TDataObj): boolean; override;
    class function AppliesToSpecifically(aDataObj: TDataObj): boolean; override;
  end;
  TConvertTo64BitInteger = class(TDataObjectConverterBase)
  public
    class function Title: string; override;
    class function Description: string; override;
    class procedure ExecuteConversion(aDataObj: TDataObj); override;
    class function AppliesToDataType(aDataObj: TDataObj): boolean; override;
    class function AppliesToSpecifically(aDataObj: TDataObj): boolean; override;
  end;
  TConvertToIntegerTrunc = class(TDataObjectConverterBase)
  public
    class function Title: string; override;
    class function Description: string; override;
    class procedure ExecuteConversion(aDataObj: TDataObj); override;
    class function AppliesToDataType(aDataObj: TDataObj): boolean; override;
    class function AppliesToSpecifically(aDataObj: TDataObj): boolean; override;
  end;
  TConvertTo64BitIntegerTrunc = class(TDataObjectConverterBase)
  public
    class function Title: string; override;
    class function Description: string; override;
    class procedure ExecuteConversion(aDataObj: TDataObj); override;
    class function AppliesToDataType(aDataObj: TDataObj): boolean; override;
    class function AppliesToSpecifically(aDataObj: TDataObj): boolean; override;
  end;
  TConvertToSingle = class(TDataObjectConverterBase)
  public
    class function Title: string; override;
    class function Description: string; override;
    class procedure ExecuteConversion(aDataObj: TDataObj); override;
    class function AppliesToDataType(aDataObj: TDataObj): boolean; override;
    class function AppliesToSpecifically(aDataObj: TDataObj): boolean; override;
  end;
  TConvertToDouble = class(TDataObjectConverterBase)
  public
    class function Title: string; override;
    class function Description: string; override;
    class procedure ExecuteConversion(aDataObj: TDataObj); override;
    class function AppliesToDataType(aDataObj: TDataObj): boolean; override;
    class function AppliesToSpecifically(aDataObj: TDataObj): boolean; override;
  end;

  TConvertUTCDateTimeToString = class(TDataObjectConverterBase)
  public
    class function Title: string; override;
    class function Description: string; override;
    class procedure ExecuteConversion(aDataObj: TDataObj); override;
    class function AppliesToDataType(aDataObj: TDataObj): boolean; override;
    class function AppliesToSpecifically(aDataObj: TDataObj): boolean; override;
  end;
  TConvertToUTCDateTime = class(TDataObjectConverterBase)
  public
    class function Title: string; override;
    class function Description: string; override;
    class procedure ExecuteConversion(aDataObj: TDataObj); override;
    class function AppliesToDataType(aDataObj: TDataObj): boolean; override;
    class function AppliesToSpecifically(aDataObj: TDataObj): boolean; override;
  end;
  TConvertToGUID = class(TDataObjectConverterBase)
  public
    class function Title: string; override;
    class function Description: string; override;
    class procedure ExecuteConversion(aDataObj: TDataObj); override;
    class function AppliesToDataType(aDataObj: TDataObj): boolean; override;
    class function AppliesToSpecifically(aDataObj: TDataObj): boolean; override;
  end;
  TConvertToFrame = class(TDataObjectConverterBase)
  public
    class function Title: string; override;
    class function Description: string; override;
    class procedure ExecuteConversion(aDataObj: TDataObj); override;
    class function AppliesToDataType(aDataObj: TDataObj): boolean; override;
    class function AppliesToSpecifically(aDataObj: TDataObj): boolean; override;
  end;





  procedure RegisterConverterClass(aConverter: TDataObjectConverterBaseClass);

var
  gDataObjConverterRegistry: TDataObjConverterRegistry;
  gUTCDateTimeFormatSettings: TFormatSettings;           // used for converting UTCDateTimes with millisecond precision.


implementation

procedure RegisterConverterClass(aConverter: TDataObjectConverterBaseClass);
begin
  if not assigned(gDataObjConverterRegistry) then
     gDataObjConverterRegistry:=TDataObjConverterRegistry.create;

  gDataObjConverterRegistry.Add(aConverter);
end;



{ TConvertStringListToArrayOfStrings }

class function TConvertStringListToArrayOfStrings.AppliesToDataType(aDataObj: TDataObj): boolean;
begin
  result := aDataObj.DataType.Code = cDataTypeStringList;
end;

class function TConvertStringListToArrayOfStrings.AppliesToSpecifically(aDataObj: TDataObj): boolean;
begin
  result := aDataObj.DataType.Code = cDataTypeStringList;
end;

class function TConvertStringListToArrayOfStrings.Description: string;
begin
  result := 'Convert StringList to an Array of Strings';
end;

class procedure TConvertStringListToArrayOfStrings.ExecuteConversion(aDataObj: TDataObj);
var
  i: integer;
  lDataArray: TDataArray;
begin
  if aDataObj.DataType.Code = cDataTypeStringList then
  begin
    lDataArray:=TDataArray.create;
    for i := 0 to aDataObj.AsStringList.Count-1 do
    begin
      lDataArray.NewSlot.AsString := aDataObj.AsStringList.Strings[i];
    end;
    aDataObj.AsArray := lDataArray;     // This gets the aDataObj to take over ownership.
  end;
end;

class function TConvertStringListToArrayOfStrings.Title: string;
begin
  result := 'StringList to Array of Strings';
end;

{ TConvertArrayOfStringsToStringList }

class function TConvertArrayOfStringsToStringList.AppliesToDataType(aDataObj: TDataObj): boolean;
begin
  result := aDataObj.DataType.Code = cDataTypeArray;
end;

class function TConvertArrayOfStringsToStringList.AppliesToSpecifically(aDataObj: TDataObj): boolean;
var
  i: Integer;
  lArray: TDataArray;
begin
  result := aDataObj.DataType.Code = cDataTypeArray;
  if result then
  begin
    // need to go through all the slots in this array and only return true if all slots are strings (Includes Symbols)
    lArray := aDataObj.AsArray;
    for i := 0 to lArray.count-1 do
    begin
      if lArray.slots[i].DataType.Code <> cDataTypeString then
      begin
        result := false;
        break;
      end;
    end;
  end;
end;

class function TConvertArrayOfStringsToStringList.Description: string;
begin
  result := 'Convert an Array of Strings to a StringList.  This conversion will only take place if every item in the array is in fact a string.  If any items in the array are not strings, then nothing is converted.';
end;

class procedure TConvertArrayOfStringsToStringList.ExecuteConversion(aDataObj: TDataObj);
var
  i: integer;
  lSL: TDataStringList;
  lDataArray: TDataArray;
begin
  if aDataObj.DataType.Code = cDataTypeArray then
  begin
    lDataArray := aDataObj.AsArray;
    lSL:=TDataStringList.create;
    try
      for i := 0 to lDataArray.Count-1 do
      begin
        lSL.Add(lDataArray.Slots[i].AsString);
      end;
    finally
      aDataObj.AsStringList := lSL;     // This gets the aDataObj to take over ownership.
    end;
  end;
end;

class function TConvertArrayOfStringsToStringList.Title: string;
begin
  result := 'Array of Strings to StringList';
end;

{ TConvertBase64StringToBinary }

class function TConvertBase64StringToBinary.AppliesToDataType(aDataObj: TDataObj): boolean;
begin
  result := aDataObj.DataType.code = cDataTypeString;
end;

function IsValid(aChar: char): boolean; inline;
begin
  result := ((aChar >= 'A') and (aChar <= 'Z')) or
            ((aChar >= 'a') and (aChar <= 'z')) or
            ((aChar >= '0') and (aChar <= '9')) or
            (aChar = '/') or (aChar = '+') or (aChar = '=');
end;

class function TConvertBase64StringToBinary.AppliesToSpecifically(aDataObj: TDataObj): boolean;
var
  i: integer;
  lString: String;
begin
  result := aDataObj.DataType.code = cDataTypeString;
  if result then
  begin
    // need to scan through all of the characters in the string and see if they fall within the valid character set of Base64 characters.
    lString := aDataObj.AsString;
    for i := 1 to length(lString) do
    begin
      if not IsValid(lString[1]) then
      begin
        result := false;
        break;
      end;
    end;
  end;
end;

class function TConvertBase64StringToBinary.Description: string;
begin
  result := 'Convert a string that complies with the Base64 character set into a Binary DataObject.';
end;

class procedure TConvertBase64StringToBinary.ExecuteConversion(aDataObj: TDataObj);
var
  lEncoder : TBase64StringEncoding;
  lSS: TStringStream;
  lMS: TMemoryStream;
begin
  if aDataObj.DataType.Code = cDataTypeString then
  begin
    lSS:=TStringStream.create(aDataObj.AsString);    // get the source string accessible as a Stream.
    lMS:=TMemoryStream.Create;
    try
      lEncoder := TBase64StringEncoding.create;
      lEncoder.decode(lSS, lMS);

      // If the above call did not except out, then we can use what was produced.
      aDataObj.AsBinary.LoadFromStream(lMS);    // FINISH - Bettery way to ASSIGN the memory stream just so ownership can be taken over?

    finally
      lSS.Free;
      lMS.Free;
    end;
  end;
end;

class function TConvertBase64StringToBinary.Title: string;
begin
  result := 'Base64 String to Binary';
end;

{ TConvertBinaryToBase64String }

class function TConvertBinaryToBase64String.AppliesToDataType(aDataObj: TDataObj): boolean;
begin
  result := aDataObj.DataType.code = cDataTypeBinary;
end;

class function TConvertBinaryToBase64String.AppliesToSpecifically(aDataObj: TDataObj): boolean;
begin
  result := aDataObj.DataType.code = cDataTypeBinary;
end;

class function TConvertBinaryToBase64String.Description: string;
begin
  result := 'Convert a Binary DataObject to a string DataObject that contains the Base64 representation of the original Binary DataObject.';
end;

class procedure TConvertBinaryToBase64String.ExecuteConversion(aDataObj: TDataObj);
var
  lEncoder : TBase64StringEncoding;
  lSS: TStringStream;
begin
  if aDataObj.DataType.Code = cDataTypeBinary then
  begin
    lSS:=TStringStream.create();
    try
      lEncoder := TBase64StringEncoding.create;
      lEncoder.encode(aDataObj.AsBinary, lSS);

      // If the above call did not except out, then we can use what was produced.
      aDataObj.AsString := lSS.DataString;
    finally
      lSS.Free;
    end;
  end;
end;

class function TConvertBinaryToBase64String.Title: string;
begin
  result := 'Binary to Base64 String';
end;



{ TDataObjConverterRegistry }

// Note that this copyFrom method does not "clone" the items in the list, but rather makes a copy list that refers to the same items in the sourceList.
procedure TDataObjConverterRegistry.CopyFrom(aSourceList: TDataObjConverterRegistry);
var
  i: Integer;
begin
  if assigned(aSourceList) then
  begin
    for i := 0 to aSourceList.count-1 do
    begin
      add(aSourceList.items[i]);
    end;
  end;
end;

{ TConvertToString }

class function TConvertToString.AppliesToDataType(aDataObj: TDataObj): boolean;
begin
  result := (aDataObj.dataType.code <= cDataTypeString) or (aDataObj.dataType.code = cDataTypeStringList);
end;

class function TConvertToString.AppliesToSpecifically(aDataObj: TDataObj): boolean;
begin
  result := (aDataObj.dataType.code <= cDataTypeObjectID) or (aDataObj.dataType.code = cDataTypeStringList);
  // Maybe we can improve this?
end;

class function TConvertToString.Description: string;
begin
  result := 'Convert to a string representation';
end;

class procedure TConvertToString.ExecuteConversion(aDataObj: TDataObj);
begin
  if AppliesToDataType(aDataObj) then
    aDataObj.AsString := aDataObj.AsString;         // use the default DataObj internal conversion abilities.
end;

class function TConvertToString.Title: string;
begin
  result := 'To String';
end;

{ TConvertToInteger }

class function TConvertToInteger.AppliesToDataType(aDataObj: TDataObj): boolean;
begin
  result := false;
  case aDataObj.DataType.Code of
    cDataTypeNull,
    cDataTypeByte,
    cDataTypeInt32,
    cDataTypeInt64,
    cDataTypeBoolean,
    cDataTypeString,
    cDataTypeSingle,
    cDataTypeDouble: result := true;
  end;  // case
end;

class function TConvertToInteger.AppliesToSpecifically(aDataObj: TDataObj): boolean;
var
  lInteger: integer;
  lStr: string;
begin
  result := AppliesToDataType(aDataObj);
  if result then
  begin
    case aDataObj.DataType.Code of
      cDataTypeString: begin
        lStr := aDataObj.AsString;
        if lStr = '' then
          result := true    // empty string will convert to zero
        else
          result := TryStrToInt(lStr, lInteger);  // return true if the try is successful.
      end;
    end;  // case
  end;
end;

class function TConvertToInteger.Description: string;
begin
  result := 'Convert to an integer representation if the value can be converted.';
end;

class procedure TConvertToInteger.ExecuteConversion(aDataObj: TDataObj);
begin
  if AppliesToSpecifically(aDataObj) then
  begin
    aDataObj.AsInt32 := aDataObj.AsInt32;
  end;
end;

class function TConvertToInteger.Title: string;
begin
  result := 'To Integer';
end;

{ TConvertTo64BitInteger }

class function TConvertTo64BitInteger.AppliesToDataType(aDataObj: TDataObj): boolean;
begin
  result := false;
  case aDataObj.DataType.Code of
    cDataTypeNull,
    cDataTypeByte,
    cDataTypeInt32,
    cDataTypeInt64,
    cDataTypeBoolean,
    cDataTypeString,
    cDataTypeSingle,
    cDataTypeDouble: result := true;
  end;  // case
end;

class function TConvertTo64BitInteger.AppliesToSpecifically(aDataObj: TDataObj): boolean;
var
  lInt64: int64;
  lStr: string;
begin
  result := AppliesToDataType(aDataObj);
  if result then
  begin
    case aDataObj.DataType.Code of
      cDataTypeString: begin
        lStr := aDataObj.AsString;
        if lStr = '' then
          result := true    // empty string will convert to zero
        else
          result := TryStrToInt64(lStr, lInt64);  // return true if the try is successful.
      end;
    end;  // case
  end;
end;

class function TConvertTo64BitInteger.Description: string;
begin
  result := 'Convert to an 64-bit integer representation if the value can be converted.';
end;

class procedure TConvertTo64BitInteger.ExecuteConversion(aDataObj: TDataObj);
begin
  if AppliesToSpecifically(aDataObj) then
  begin
    aDataObj.AsInt64 := aDataObj.AsInt64;
  end;
end;

class function TConvertTo64BitInteger.Title: string;
begin
  result := 'To 64-bit Integer';
end;

{ TConvertToIntegerTrunc }

class function TConvertToIntegerTrunc.AppliesToDataType(aDataObj: TDataObj): boolean;
begin
  result := false;
  case aDataObj.DataType.Code of
    // not doing null here because the other integer converts can do that.
    cDataTypeSingle,
    cDataTypeDouble: result := true;
  end;  // case
end;

class function TConvertToIntegerTrunc.AppliesToSpecifically(aDataObj: TDataObj): boolean;
begin
  result := AppliesToDataType(aDataObj);
  if result then
  begin
    case aDataObj.DataType.Code of
      cDataTypeSingle: result := (aDataObj.AsSingle <= MaxInt) AND (aDataObj.AsSingle >= Integer($80000000));
      cDataTypeDouble: result := (aDataObj.AsDouble <= MaxInt) AND (aDataObj.AsDouble >= Integer($80000000));
    end;
  end;
end;

class function TConvertToIntegerTrunc.Description: string;
begin
  result := 'Convert to an 32-bit integer representation from a floating point number by truncating to just the whole number portion.';
end;

class procedure TConvertToIntegerTrunc.ExecuteConversion(aDataObj: TDataObj);
begin
  if AppliesToSpecifically(aDataObj) then
  begin
    case aDataObj.DataType.Code of
      cDataTypeSingle: aDataObj.AsInt32:=Int32(Trunc(aDataObj.AsSingle));
      cDataTypeDouble: aDataObj.AsInt32:=Int32(Trunc(aDataObj.AsDouble));
    end;
  end;
end;

class function TConvertToIntegerTrunc.Title: string;
begin
  result := 'Truncate to 32-bit Integer';
end;

{ TConvertTo64BitIntegerTrunc }

class function TConvertTo64BitIntegerTrunc.AppliesToDataType(aDataObj: TDataObj): boolean;
begin
  result := false;
  case aDataObj.DataType.Code of
    // not doing null here because the other integer converts can do that.
    cDataTypeSingle,
    cDataTypeDouble: result := true;
  end;  // case
end;

class function TConvertTo64BitIntegerTrunc.AppliesToSpecifically(aDataObj: TDataObj): boolean;
begin
  result := AppliesToDataType(aDataObj);
  if result then
  begin
    case aDataObj.DataType.Code of
      cDataTypeSingle: result := (aDataObj.AsSingle <= MaxInt) AND (aDataObj.AsSingle >= Int64($8000000000000000));
      cDataTypeDouble: result := (aDataObj.AsDouble <= MaxInt) AND (aDataObj.AsDouble >= Int64($8000000000000000));
    end;
  end;
end;

class function TConvertTo64BitIntegerTrunc.Description: string;
begin
  result := 'Convert to an 64-bit integer representation from a floating point number by truncating to just the whole number portion.';
end;

class procedure TConvertTo64BitIntegerTrunc.ExecuteConversion(aDataObj: TDataObj);
begin
  if AppliesToSpecifically(aDataObj) then
  begin
    case aDataObj.DataType.Code of
      cDataTypeSingle: aDataObj.AsInt64:=Trunc(aDataObj.AsSingle);
      cDataTypeDouble: aDataObj.AsInt64:=Trunc(aDataObj.AsDouble);
    end;
  end;
end;

class function TConvertTo64BitIntegerTrunc.Title: string;
begin
  result := 'Truncate To 64-bit Integer';
end;


{ TConvertToDouble }

class function TConvertToDouble.AppliesToDataType(aDataObj: TDataObj): boolean;
begin
  result := false;
  case aDataObj.DataType.Code of
    cDataTypeNULL,
    cDataTypeByte,
    cDataTypeInt32,
    cDataTypeInt64,
    cDataTypeBoolean,
    cDataTypeString,
    cDataTypeSingle,
    cDataTypeDouble: result := true;
  end;  // case
end;

class function TConvertToDouble.AppliesToSpecifically(aDataObj: TDataObj): boolean;
var
  lDouble: Double;
begin
  result := AppliesToDataType(aDataObj);
  if result then
  begin
    case aDataObj.DataType.Code of
      cDataTypeString: begin
        result := TryStrToFloat(aDataObj.AsString, lDouble);  // return true if the try is successful.
      end;
    end;
  end;
end;

class function TConvertToDouble.Description: string;
begin
  result := 'Convert to a Double precision floating point number';
end;

class procedure TConvertToDouble.ExecuteConversion(aDataObj: TDataObj);
begin
  if AppliesToSpecifically(aDataObj) then
  begin
    aDataObj.AsDouble := aDataObj.AsDouble;
  end;
end;

class function TConvertToDouble.Title: string;
begin
  result := 'To Double';
end;

{ TConvertToSingle }

class function TConvertToSingle.AppliesToDataType(aDataObj: TDataObj): boolean;
begin
  result := false;
  case aDataObj.DataType.Code of
    cDataTypeNULL,
    cDataTypeByte,
    cDataTypeInt32,
    cDataTypeInt64,
    cDataTypeBoolean,
    cDataTypeString,
    cDataTypeSingle,
    cDataTypeDouble: result := true;
  end;  // case
end;

class function TConvertToSingle.AppliesToSpecifically(aDataObj: TDataObj): boolean;
var
  lSingle: Single;
begin
  result := AppliesToDataType(aDataObj);
  if result then
  begin
    case aDataObj.DataType.Code of
      cDataTypeString: begin
        result := TryStrToFloat(aDataObj.AsString, lSingle);  // return true if the try is successful.
      end;
    end;
  end;
end;

class function TConvertToSingle.Description: string;
begin
  result := 'Convert to a Single precision floating point number';
end;

class procedure TConvertToSingle.ExecuteConversion(aDataObj: TDataObj);
begin
  if AppliesToSpecifically(aDataObj) then
  begin
    aDataObj.AsSingle := aDataObj.AsSingle;
  end;
end;

class function TConvertToSingle.Title: string;
begin
  result := 'To Single';
end;



{ TConvertUTCDateTimeToString }

class function TConvertUTCDateTimeToString.AppliesToDataType(aDataObj: TDataObj): boolean;
begin
  result := aDataObj.DataType.Code = cDataTypeUTCDateTime;
end;

class function TConvertUTCDateTimeToString.AppliesToSpecifically(aDataObj: TDataObj): boolean;
begin
  result := AppliesToDataType(aDataObj);
end;

class function TConvertUTCDateTimeToString.Description: string;
begin
  result := 'Convert to a string with millisecond formatting.';
end;

class procedure TConvertUTCDateTimeToString.ExecuteConversion(aDataObj: TDataObj);
var
  lDateTime: TDateTime;
begin
  if AppliesToSpecifically(aDataObj) then
  begin
    // NOte that the UnixToDateTime function in system.DateUtils has a bug where it uses incSeconds instead of incMilliSeconds.  So, we do it right here.
    lDateTime := IncMilliSecond(UnixDateDelta, aDataObj.AsUTCDateTime);
    aDataObj.AsString := DateTimetoStr(lDateTime, gUTCDateTimeFormatSettings);
  end;
end;

class function TConvertUTCDateTimeToString.Title: string;
begin
  result := 'To Millisecond formatted String';
end;


{ TConvertStringToToUTCDateTime }

class function TConvertToUTCDateTime.AppliesToDataType(aDataObj: TDataObj): boolean;
begin
  result := false;
  case aDataObj.DataType.code of
    cDataTypeNull,
    cDataTypeByte,
    cDataTypeInt32,
    cDataTypeInt64,
    cDataTypeDateTime,
    cDataTypeDate,
    cDataTypeTime,
    cDataTypeString: result := true;
  end;
end;

class function TConvertToUTCDateTime.AppliesToSpecifically(aDataObj: TDataObj): boolean;
var
  lDateTime: TDateTime;
begin
  result := false;
  if AppliesToDataType(aDataObj) then
  begin
    case aDataObj.DataType.code of
      cDataTypeNull,
      cDataTypeByte,
      cDataTypeInt32,
      cDataTypeInt64,
      cDataTypeDateTime,
      cDataTypeDate,
      cDataTypeTime: result := true;
      cDataTypeString: result := TryStrToDateTime(aDataObj.AsString, lDateTime);
    end;
  end;
end;

class function TConvertToUTCDateTime.Description: string;
begin
  result := 'Convert to a UTC DateTime.';
end;

class procedure TConvertToUTCDateTime.ExecuteConversion(aDataObj: TDataObj);
var
  lDateTime: TDateTime;
  lMilliSec: Int64;
begin
  if AppliesToSpecifically(aDataObj) then
  begin
    case aDataObj.DataType.code of
      cDataTypeNull,
      cDataTypeByte,
      cDataTypeInt32,
      cDataTypeInt64: begin
        aDataObj.AsUTCDateTime := aDataObj.AsInt64;
      end;
      cDataTypeDateTime,
      cDataTypeDate,
      cDataTypeTime: begin
        lDateTime := aDataObj.AsDateTime;
        lMilliSec := MilliSecondsBetween(UnixDateDelta, lDateTime);
        if lDateTime < UnixDateDelta then
          lMilliSec := -lMilliSec;
        aDataObj.AsUTCDateTime := lMilliSec;
      end;
      cDataTypeString: begin
        lDateTime := StrToDateTime(aDataObj.AsString);
        lMilliSec := MilliSecondsBetween(UnixDateDelta, lDateTime);
        if lDateTime < UnixDateDelta then
          lMilliSec := -lMilliSec;
        aDataObj.AsUTCDateTime := lMilliSec;
      end;
    end;

  end;
end;

class function TConvertToUTCDateTime.Title: string;
begin
  result := 'To UTCDateTime';
end;

{ TConvertToGUID }

class function TConvertToGUID.AppliesToDataType(aDataObj: TDataObj): boolean;
begin
  result := aDataObj.DataType.Code = cDataTypeString;
end;

class function TConvertToGUID.AppliesToSpecifically(aDataObj: TDataObj): boolean;
var
  lTestGUID: TGUID;
begin
  result := false;
  if AppliesToDataType(aDataObj) then
  begin
    try
      lTestGUID := TGUID.Create(aDataObj.AsString);

      result := sameText(lTestGUID.ToString, aDataObj.AsString);      // if we converted the string to a guid and that guid then produces the same string we came from, then we have a successful conversion.
    except
    end;
  end;
end;

class function TConvertToGUID.Description: string;
begin
  result := 'Convert to a strongly typed GUID';
end;

class procedure TConvertToGUID.ExecuteConversion(aDataObj: TDataObj);
begin
  aDataObj.AsGUID;
end;

class function TConvertToGUID.Title: string;
begin
  result := 'To GUID';
end;

{ TConvertToFrame }

class function TConvertToFrame.AppliesToDataType(aDataObj: TDataObj): boolean;
begin
  result := (aDataObj.DataType.code = cDataTypeArray) or (aDataObj.DataType.code = cDataTypeSparseArray);
end;

class function TConvertToFrame.AppliesToSpecifically(aDataObj: TDataObj): boolean;
begin
  result := AppliesToDataType(aDataObj);   // Any array can go to a frame.
end;

class function TConvertToFrame.Description: string;
begin
  result := 'Convert this array to a frame';
end;

class procedure TConvertToFrame.ExecuteConversion(aDataObj: TDataObj);
begin
  if AppliestoSpecifically(aDataObj) then
  begin
    aDataObj.AsFrame;    // As long as the data type was a array or sparse array, calling .AsFrame will internally perform the conversion.
  end;
end;

class function TConvertToFrame.Title: string;
begin
  result := 'To a Frame';
end;

initialization
  RegisterConverterClass(TConvertStringListToArrayOfStrings);
  RegisterConverterClass(TConvertArrayOfStringsToStringList);
  RegisterConverterClass(TConvertBase64StringToBinary);
  RegisterConverterClass(TConvertBinaryToBase64String);

  RegisterConverterClass(TConvertToString);
  RegisterConverterClass(TConvertToInteger);
  RegisterConverterClass(TConvertTo64BitInteger);
  RegisterConverterClass(TConvertToIntegerTrunc);
  RegisterConverterClass(TConvertTo64BitIntegerTrunc);
  RegisterConverterClass(TConvertToSingle);
  RegisterConverterClass(TConvertToDouble);
  RegisterConverterClass(TConvertUTCDateTimeToString);
  RegisterConverterClass(TConvertToUTCDateTime);
  RegisterConverterClass(TConvertToGUID);
  RegisterConverterClass(TConvertToFrame);


  gUTCDateTimeFormatSettings := TFormatSettings.Create;
  gUTCDateTimeFormatSettings.LongTimeFormat := 'h:mm:ss.zzz AMPM';

finalization
  gDataObjConverterRegistry.Free;

end.



