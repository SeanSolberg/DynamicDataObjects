unit DataObjects2Smile;

{********************************************************************************}
{                                                                                }
{                         Dynamic Data Objects Library                           }
{                                                                                }
{                                                                                }
{ MIT License                                                                    }
{                                                                                }
{ Copyright (c) 2022 Sean Solberg                                                }
{                                                                                }
{ Permission is hereby granted, free of charge, to any person obtaining a copy   }
{ of this software and associated documentation files (the "Software"), to deal  }
{ in the Software without restriction, including without limitation the rights   }
{ to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      }
{ copies of the Software, and to permit persons to whom the Software is          }
{ furnished to do so, subject to the following conditions:                       }
{                                                                                }
{ The above copyright notice and this permission notice shall be included in all }
{ copies or substantial portions of the Software.                                }
{                                                                                }
{ THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     }
{ IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       }
{ FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    }
{ AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         }
{ LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  }
{ OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  }
{ SOFTWARE.                                                                      }
{                                                                                }
{********************************************************************************}

interface

// FINISH - This whole unit needs to be finished,  just the basic framework has been put in here.
// See https://github.com/FasterXML/smile-format-specification

uses classes, DataObjects2, DataObjects2Streamers, SysUtils, RTTI, TypInfo, DataObjects2Utils, VarInt;

type
  TNumBytes = packed record
    fCode: byte;

    procedure SetSingleValue(aValue: Single);
    procedure SetDoubleValue(aValue: double);
  strict private
    case byte of
      0: (fSingleValue: Cardinal;);    //4-byte Byte-Swapped Cardinal representation of a Single
      1: (fDoubleValue: UInt64;);      //8-byte Byte-Swapped UInt64 representation of a Double
    end;

  TSmileStreamer = class(TDataObjStreamerBase)
  private
    fSharedStringValueEnabled: boolean;
    fSharedStringKeyEnabled: boolean;
    fRawBinaryEnabled: boolean;

    // buffers for holding the shared strings for backreferencing.
    fSharedKeyStrings: array[0..1023] of UTF8String;
    fSharedKeyStringCount: integer;
    fSharedValueStrings: array[0..1023] of UTF8String;
    fSharedValueStringCount: integer;

    function FindSharedKeyString(aString: UTF8String): integer;
    procedure AddSharedKeyString(aString: UTF8String);

    function FindSharedValueString(aString: UTF8String): integer;
    procedure AddSharedValueString(aString: UTF8String);

    procedure DoWriteString(aString: UTF8String);
    procedure WriteValueString(aValueString: String);
    procedure WriteKeyString(aKeyValue: string);
    procedure WriteDataObj(aDataObj: TDataObj);
    procedure WriteHeader;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    class function FileExtension: string; override;
    class function Description: string; override;
    class function GetFileFilter: string; override;
    class function IsFileExtension(aStr: string): boolean; override;
    class function ClipboardPriority: cardinal; override;

    procedure Decode(aDataObj: TDataObj); override;
    procedure Encode(aDataObj: TDataObj); override;

    property SharedStringValueEnabled: boolean read fSharedStringValueEnabled write fSharedStringValueEnabled;
    property SharedStringKeyEnabled: boolean read fSharedStringKeyEnabled write fSharedStringKeyEnabled;
    property RawBinaryEnabled: boolean read fRawBinaryEnabled write fRawBinaryEnabled;
  end;

implementation

//resourceString

procedure RaiseParsingException(aStream: TStream; aMessage: string);
begin
  raise Exception.Create(aMessage+' when reading a Smile Stream at position='+intToStr(aStream.Position));
end;


class function TSmileStreamer.GetFileFilter: string;
begin
  result := 'Smile Files (*.smile)|*.smile';
end;

class function TSmileStreamer.IsFileExtension(aStr: string): boolean;
begin
  result := SameText(aStr, '.smile') or SameText(aStr, 'smile');
end;

procedure TSmileStreamer.WriteHeader;
var
  lBytes: array[0..3] of byte;
begin
  lBytes[0] := $3A;  // ":" character
  lBytes[1] := $29;  // "): character
  lBytes[2] := $0A;  // linefeed

  lBytes[3] := 0;       // Bits 4-7 (4 MSB): 4-bit version number; 0x00 for current version (note: it is possible that some bits may be reused if necessary)
                        // Bits 3: Reserved
                        // Bit 2 (mask 0x04) Whether '''raw binary''' (unescaped 8-bit) values may be present in content
                        // Bit 1 (mask 0x02): Whether '''shared String value''' checking was enabled during encoding -- if header missing, default value of "false" must be assumed for decoding (meaning parser need not store decoded String values for back referencing)
                        // Bit 0 (mask 0x01): Whether '''shared property name''' checking was enabled during encoding -- if header missing, default value of "true" must be assumed for decoding (meaning parser MUST store seen property names for possible back references)
  if fRawBinaryEnabled then
    lBytes[3] := lBytes[3] or $4;
  if fSharedStringValueEnabled then
    lBytes[3] := lBytes[3] or $2;
  if fSharedStringKeyEnabled then
    lBytes[3] := lBytes[3] or $1;

  fStream.Write(lBytes[0], 4);
end;

// Will write a string to the stream and does nothing to check for back-referencing.
procedure TSmileStreamer.DoWriteString(aString: UTF8String);
var
  lLen: Cardinal;
  lByte: Byte;
begin
  if aString = '' then
  begin
    lByte := $20;             //special code for empty string.
    fStream.Write(lByte,1);   //done
  end
  else
  begin
    lLen := length(aString);
    if lLen <= 32 then
    begin
      // short length text.
      lByte := lLen;                 //code 0x00 or lLen
      fStream.WriteBuffer(lByte, 1);
      fStream.WriteBuffer(aString[1], lLen);
    end
    else
    begin
      // Long (variable length) text
      lByte := $E4;                 //code for this
      fStream.WriteBuffer(lByte, 1);
      fStream.WriteBuffer(aString[1], lLen);
      lByte := $FC;                 //code for end-of-string marker
      fStream.WriteBuffer(lByte, 1);
    end;
  end;
end;

procedure TSmileStreamer.WriteKeyString(aKeyValue: string);
var
  lUTF8String: UTF8String;
  lIndex: integer;
  lDoWriteString: boolean;
  lByte: byte;
  lTwoBytes: array[0..1] of byte;
begin
  // Smile optionally supports the concept of shared keys.   So, we may be streaming an actual string value here, or if that string value has been previously streamed, we could be
  // streaming this keyValue's index value instead.  This saves space.
  lUTF8String := UTF8String(aKeyValue);     //convert to UTF8String
  lDoWriteString := true;       // starting assumption.

  if fSharedStringKeyEnabled then
  begin
    // Only strings that are 64 bytes or less can be considered shareable strings.
    if length(lUTF8String) <= 64 then
    begin
      // See if this string is already present in our existing shared string buffer which has a total limit of 1024 strings.
      lIndex := FindSharedKeyString(lUTF8String);     // returns -1 if not found.  valid is 0-1023
      if lIndex >= 0 then
      begin
        // This string was found in the shared string buffer so write out a back reference value instead of the string.
        if lIndex < 64 then
        begin
          //  0x40 - 0x7F: "Short" shared key name reference; names 0 through 63.
          lByte := $40 + lIndex;
          fStream.Write(lByte, 1);
        end
        else
        begin
          // 0x30 - 0x33: "Long" shared key name reference (2 byte token); 2 LSBs of the first byte are used as 2 MSB of 10-bit reference (up to 1024) values to a shared name: second byte used for 8 LSB.
          lTwoBytes[0] := $30 + lIndex shr 8;  // base code value plus the two MSBs
          lTwoBytes[1] := lIndex;              // truncating to the 8LSBs
          fStream.Write(lTwoBytes, 2);
        end;
        lDoWriteString := false;   // Don't need to write the string below because we just wrote the back reference.
      end
      else
      begin
        // This string was not found in the shared string buffer so write this string to the stream and add it to the shared buffer.
        AddSharedKeyString(lUTF8String);
      end
    end;
  end;

  if lDoWriteString then
  begin
    // Write the string to the stream either because it is the first time writing this string and thus is not in the back-reference buffer yet,
    // or because it is not eligible to be back-referenced,
    // or because sharedString back referencing is turned off.
    DoWriteString(lUTF8String);
  end;
end;



procedure TSmileStreamer.WriteValueString(aValueString: String);
var
  lUTF8String: UTF8String;
  lIndex: integer;
  lDoWriteString: boolean;
  lByte: byte;
  lTwoBytes: array[0..1] of byte;
begin
  // Smile optionally supports the concept of shared keys.   So, we may be streaming an actual string value here, or if that string value has been previously streamed, we could be
  // streaming this keyValue's index value instead.  This saves space.
  lUTF8String := UTF8String(aValueString);     //convert to UTF8String
  lDoWriteString := true;       // starting assumption.

  if fSharedStringValueEnabled then
  begin
    // Only strings that are 64 bytes or less can be considered shareable strings.
    if length(lUTF8String) <= 64 then
    begin
      // See if this string is already present in our existing shared string buffer which has a total limit of 1024 strings.
      lIndex := FindSharedValueString(lUTF8String);     // returns -1 if not found.  valid is 0-1023
      if lIndex >= 0 then
      begin
        // This string was found in the shared string buffer so write out a back reference value instead of the string.
        if lIndex < 64 then
        begin
          //  0x40 - 0x7F: "Short" shared value name reference; names 0 through 63.
          lByte := $40 + lIndex;
          fStream.Write(lByte, 1);
        end
        else
        begin
          // 0x30 - 0x33: "Long" shared value name reference (2 byte token); 2 LSBs of the first byte are used as 2 MSB of 10-bit reference (up to 1024) values to a shared name: second byte used for 8 LSB.
          lTwoBytes[0] := $30 + lIndex shr 8;  // base code value plus the two MSBs
          lTwoBytes[1] := lIndex;              // truncating to the 8LSBs
          fStream.Write(lTwoBytes, 2);
        end;
        lDoWriteString := false;   // Don't need to write the string below because we just wrote the back reference.
      end
      else
      begin
        // This string was not found in the shared string buffer so write this string to the stream and add it to the shared buffer.
        AddSharedValueString(lUTF8String);
      end
    end;
  end;

  if lDoWriteString then
  begin
    // Write the string to the stream either because it is the first time writing this string and thus is not in the back-reference buffer yet,
    // or because it is not eligible to be back-referenced,
    // or because sharedString back referencing is turned off.
    DoWriteString(lUTF8String);
  end;

end;

procedure TSmileStreamer.Encode(aDataObj: TDataObj);
begin
  WriteHeader;
  WriteDataObj(aDataObj);
end;

procedure TSmileStreamer.WriteDataObj(aDataObj: TDataObj);
var
  i: integer;
  lStringList: TDataStringList;
  lFrame: TDataFrame;
  lArray: TDataArray;
  lSparseArray: TDataSparseArray;
  lBinary: TDataBinary;

  lByte: byte;
  lSize: Cardinal;
  lBytes: TNumBytes;
  lVarInt: TVarInt64;
begin
  case aDataObj.DataType.Code of
    cDataTypeNull: begin
      lByte := $21;
      fStream.Write(lByte,1);   //done
    end;

    cDataTypeBoolean: begin
      if aDataObj.AsBoolean then
        lByte := $23
      else
        lByte := $22;
      fStream.Write(lByte,1);    //done
    end;

    cDataTypeByte, cDataTypeInt32: begin     // Smile doesn't have a byte serialization so we have to use the 32bit int for it.
      lVarInt := aDataObj.AsInt64;
      lByte := $24;    // Integral number 32bit, zigzag encoded.
      fStream.Write(lByte,1);
      lVarInt.WriteToStream(fStream);         //done
    end;

    cDataTypeInt64: begin
      lVarInt := aDataObj.AsInt64;
      lByte := $25;    // Integral number 364bit, zigzag encoded.
      fStream.Write(lByte,1);
      lVarInt.WriteToStream(fStream);         //done
    end;

    cDataTypeSingle: begin
      lBytes.fCode := $28;    // 32bit floating point number code
      lBytes.SetSingleValue(aDataObj.AsSingle);
      fStream.Write(lBytes,5);               //done
    end;

    cDataTypeDouble: begin
      lBytes.fCode := $29;    // 64bit floating point number code
      lBytes.SetDoubleValue(aDataObj.AsDouble);
      fStream.Write(lBytes,9);               //done
    end;

//    cDataTypeDecimal128: aStringBuilder.Append(lStore.dataInt64);

    cDataTypeDateTime, cDataTypeUTCDateTime, cDataTypeDate, cDataTypeTime: begin
      WriteValueString(aDataObj.AsString);
      //FINISH
    end;

    cDataTypeGUID: begin
      WriteValueString(aDataObj.AsString);    // It would be nice to have a real extension for the GUID, but this will work for now.
    end;

    cDataTypeObjectID: begin
      WriteValueString(aDataObj.AsString);    // It would be nice to have a real extension for the ObjectID, but this will work for now.
    end;

    cDataTypeString: begin
      WriteValueString(aDataObj.AsString);
    end;

    cDataTypeStringList: begin
      // We are going to code the StringList data type as an array of Strings.
      lStringList := aDataObj.AsStringList;

      lByte := $F8;  // Start Array Marker
      fStream.Write(lByte,1);

      for i := 0 to lStringList.Count-1 do
      begin
        WriteValueString(lStringList.Strings[i]);
      end;

      lByte := $F9;  // End Array Marker
      fStream.Write(lByte,1);
    end;

    cDataTypeFrame: begin
      lFrame := aDataObj.AsFrame;

      lByte := $FA;  // Start Object Marker
      fStream.Write(lByte,1);

      for i := 0 to lFrame.Count-1 do
      begin
        // write out the name - value pair.
        WriteKeyString(lFrame.Slotname(i));
        WriteDataObj(lFrame.Slots[i]);     //recursion happening here.
      end;

      lByte := $FB;  // End Object Marker
      fStream.Write(lByte,1);
    end;

    cDataTypeArray: begin
      lArray := aDataObj.AsArray;

      lByte := $F8;  // Start Array Marker
      fStream.Write(lByte,1);

      for i := 0 to lArray.Count-1 do
      begin
        WriteDataObj(lArray.Slots[i]);   // recursion happening here.
      end;

      lByte := $F9;  // End Array Marker
      fStream.Write(lByte,1);
    end;

    cDataTypeSparseArray: begin
      // smile can't take numbers as key values (I don't think.  Hmmm.. maybe I will look into this deeper).  Anyway, to start, I will serialize the integer keys as strings.
      lSparseArray := aDataObj.AsSparseArray;

      lByte := $FA;  // Start Object Marker
      fStream.Write(lByte,1);

      for i := 0 to lSparseArray.Count-1 do
      begin
        WriteKeyString(intToStr(lSparseArray.SlotIndex(i)));
        WriteDataObj(lSparseArray.Slots[i]);   // recursion happening here.
      end;

      lByte := $FB;  // End OBject Marker
      fStream.Write(lByte,1);
    end;

    cDataTypeBinary: begin
      lBinary := aDataObj.AsBinary;
      lSize := lBinary.Size;
      //finish
    end;

    cDataTypeObject: begin
      //Finish
    end;

    cDataTypeTag: begin
      // FINISH - found that other implementations have an option to put tags into JSON by containing the tag using a Json Object (frame) with a certain slotName naming convention.
      //          maybe we should support this concept too.
      // see https://github.com/intel/tinycbor/commit/782f2545a07e707464c6e9b417768e8b980c8e13

      // For now, we are skipping over the tagging portion and just streaming out the contained DataObject to Smile
      WriteDataObj(aDataObj.AsTag.DataObj);
    end;
  end;
end;


class function TSmileStreamer.FileExtension: string;
begin
  result := 'smile';
end;

function TSmileStreamer.FindSharedKeyString(aString: UTF8String): integer;
var
  i: Integer;
begin
  result := -1;
  for i := 0 to fSharedKeyStringCount-1 do
  begin
    if fSharedKeyStrings[i] = aString then
    begin
      result := i;
      break;
    end;
  end;
end;

function TSmileStreamer.FindSharedValueString(aString: UTF8String): integer;
var
  i: Integer;
begin
  result := -1;
  for i := 0 to fSharedValueStringCount-1 do
  begin
    if fSharedValueStrings[i] = aString then
    begin
      result := i;
      break;
    end;
  end;
end;

procedure TSmileStreamer.AddSharedKeyString(aString: UTF8String);
begin
  if fSharedKeyStringCount >= 1024 then
  begin
    // Shared string buffer is full so we must completely clear the buffer and start over with shared key back reference indexes.
    fSharedKeyStringCount := 0;
  end;

  fSharedKeyStrings[fSharedKeyStringCount] := aString;
  inc(fSharedKeyStringCount);
end;

procedure TSmileStreamer.AddSharedValueString(aString: UTF8String);
begin
  if fSharedValueStringCount >= 1024 then
  begin
    // Shared string buffer is full so we must completely clear the buffer and start over with shared value back reference indexes.
    fSharedValueStringCount := 0;
  end;

  fSharedValueStrings[fSharedValueStringCount] := aString;
  inc(fSharedValueStringCount);
end;

procedure TSmileStreamer.AfterConstruction;
begin
  inherited;
  fSharedStringKeyEnabled := true;  //default
end;

class function TSmileStreamer.ClipboardPriority: cardinal;
begin
  result := 30;
end;

procedure TSmileStreamer.Decode(aDataObj: TDataObj);
var
  lByte: byte;
  lClass: byte;

  procedure InvalidCode;
  begin

  end;
begin
  //FINISH

  while fStream.Position < fStream.Size do
  begin
    fStream.Read(lByte, 1);
    lClass := lByte shr 5;    // class is in the 3MSB

    if (lByte = ord(':')) then
    begin
      // could this be a header?
    end
    else
    begin
      case lClass of
        0: begin

        end;
        1: begin
          // Simple Literals, numbers
          if lByte = $20 then         // empty string code.
            aDataObj.AsString := ''
          else if lByte = $21 then    // Null
            aDataObj.Clear
          else if lByte = $22 then
            aDataObj.AsBoolean := false
          else if lByte = $23 then
            aDataObj.AsBoolean := true
          else
            InvalidCode;
        end;
        2: begin

        end;
        3: begin

        end;
        4: begin

        end;
        5: begin

        end;
        6: begin

        end;
        7: begin

        end;
      end;
    end;
  end;
end;

class function TSmileStreamer.Description: string;
begin
  result := 'Smile Data Format.  https://github.com/FasterXML/smile-format-specification and https://en.wikipedia.org/wiki/Smile_(data_interchange_format)';
end;

destructor TSmileStreamer.Destroy;
begin
  inherited;
end;

procedure TNumBytes.SetSingleValue(aValue: Single);
begin
  fSingleValue := SwapBytesSingle(aValue);
end;

procedure TNumBytes.SetDoubleValue(aValue: double);
begin
  fDoubleValue := SwapBytesFromDouble(aValue);
end;


initialization
  RegisterDataObjStreamer(TSmileStreamer);

end.
