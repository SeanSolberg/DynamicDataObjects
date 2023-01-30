unit DataObjects2UBJSON;

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

uses classes, DataObjects2, DataObjects2Streamers, SysUtils, RTTI, TypInfo, DataObjects2Utils;

type
{: This class will decode and encode a UBJSON stream.   See http://ubjson.org/ for details. }
  TUBJSONStreamer = class(TDataObjStreamerBase)
  protected
    fNoopCount: integer;

    procedure RaiseParsingException(aMessage: string);
    procedure DoRead(var Buffer; Count: LongInt);
    procedure DecodeType(aType: byte; aDataObj: TDataObj); virtual;
    function ReadTypeByte: byte;
    function DecodeSize(aType: byte): int64;
  public
    class function FileExtension: string; override;
    class function Description: string; override;
    class function GetFileFilter: string; override;
    class function IsFileExtension(aStr: string): boolean; override;
    class function ClipboardPriority: cardinal; override;

    procedure Decode(aDataObj: TDataObj); override;
    procedure Encode(aDataObj: TDataObj); override;
  end;

implementation

//ResourceString

procedure TUBJSONStreamer.RaiseParsingException(aMessage: string);
begin
  raise Exception.Create(aMessage+' when reading a UBJSON Stream at position='+intToStr(fStream.Position));
end;


class function TUBJSONStreamer.GetFileFilter: string;
begin
  result := 'UBJSON Files (*.ubj)|*.ubj';
end;

class function TUBJSONStreamer.IsFileExtension(aStr: string): boolean;
begin
  result := SameText(aStr, '.ubj') or SameText(aStr, 'ubj');
end;


class function TUBJSONStreamer.ClipboardPriority: cardinal;
begin
  result := 40;
end;


procedure TUBJSONStreamer.DoRead(var Buffer; Count: LongInt);
begin
  if fStream.Read(Buffer, Count) <> Count then
  begin
    RaiseParsingException('Premature end of stream trying to read '+intToStr(Count)+' bytes');
  end;
end;

procedure TUBJSONStreamer.Decode(aDataObj: TDataObj);
begin
  DecodeType(ReadTypeByte, aDataObj);
end;


// This function will read the next Type byte.  fNoopCount is reset to zero as a part of this function.
// Then, if one or more no-op type bytes are found, they are essentially ignored except that the fNoopCount is incremented to the count of sequential no-ops encountered.
function TUBJSONStreamer.ReadTypeByte: byte;
begin
  fNoopCount := -1;
  repeat
    DoRead(result, 1);
    inc(fNoopCount);
  until result <> byte('N');
end;

function TUBJSONStreamer.DecodeSize(aType: byte): int64;
var
  lShortInt: shortInt;
  lByte: byte;
  lSmallInt: smallInt;
  lInt32: LongInt;
  lInt64: Int64;
begin
  result := 0;

  case aType of
    byte('i'): begin   // signed int8
      DoRead(lShortInt, 1);
      result := lShortInt;
    end;

    byte('U'): begin   // Unsigned byte
      DoRead(lByte, 1);
     result := lByte;
    end;

    byte('I'): begin   // int16
      DoRead(lSmallInt, 2);
      result := lSmallInt;
    end;

    byte('l'): begin   // Signed in32
      DoRead(lInt32, 4);
      result := lInt32;
    end;

    byte('L'): begin   // Signed int64
      DoRead(lInt64, 8);
      result := lInt64;
    end;

    else
    begin
      // we can only expect to see a number so anything else is an error condition
      RaiseParsingException('Read an invalid type code of $'+IntToHex(aType,2)+' but only number types are allowed when reading a size.');
    end;
  end;
end;


procedure TUBJSONStreamer.DecodeType(aType: byte; aDataObj: TDataObj);
var
  lTempType: Byte;
  lChildType: Byte;
  lSize: integer;
  lSlotName: string;
  
  lShortInt: ShortInt;  // signed 8 bit
  lSmallInt: SmallInt;  // signed 16 bit
  lByte: Byte;          // unsigned 8 bit
  lInt32: integer;
  lInt64: int64;
  lSingle: single;
  lDouble: double;
  lAnsiChar: char;
  lUTF8String: UTF8String;
  i: Integer;


  function ReadStringWithSizeType(aType: byte): string;
  var
    lSize: int64;
  begin
    lSize := DecodeSize(aType);

    if lSize > $7FFFFFFF  then
      RaiseParsingException('Size of '+Inttostr(lSize)+' is too large to use for a string length.');
    if lSize<0 then
      RaiseParsingException('Cannot use a negative size of '+Inttostr(lSize)+' for a string length.');

    if lSize>0 then
    begin
      SetLength(lUTF8String, lSize);
      fStream.Read(lUTF8String[1], lSize);
      result := string(lUTF8String);  // converting from UTF8-String to Unicode string.
    end
    else
    begin
      result := '';
    end;
  end;

  // If aSizeCode is passed in, then we have already read the size code for the string length and this parameter is it.   if it's zero, then
  function ReadString: string;
  begin
    result := ReadStringWithSizeType(ReadTypeByte);
  end;

  procedure AssertIsValidSizeType(aType: Byte);
  begin
    if not((aType = byte('i')) or (aType=byte('U')) or (aType=byte('I')) or (aType=byte('l')) or (aType=byte('L'))) then
    begin
      RaiseParsingException('Read an invalid type code of $'+IntToHex(aType,2)+' but only number types are allowed when reading a size.');
    end;
  end;

begin
  case aType of
    byte('Z'): begin   // NULL
      aDataObj.Clear;
    end;

    byte('N'): begin   // NO-OP
      // Nothing to do hear but set a flag so that the following type we are decoding can possibly use the preceding No-op to adjust it's decoding behavior.
    end;

    byte('T'): begin   // True
      aDataObj.AsBoolean := true;
    end;

    byte('F'): begin   // False
      aDataObj.AsBoolean := false;
    end;

    byte('i'): begin   // signed int8
      DoRead(lShortInt, 1);
      if lShortInt >=0 then
        aDataObj.AsByte := lShortInt
      else
        aDataObj.AsInt32 := lShortInt;    // if the shortInt is negative, then we must store in an Int32 because our internal byte data type is unsigned.
    end;

    byte('U'): begin   // Unsigned byte
      DoRead(lByte, 1);
      aDataObj.AsByte := lByte;
    end;

    byte('I'): begin   // int16
      DoRead(lSmallInt, 2);
      aDataObj.AsInt32 := lSmallInt;
    end;

    byte('l'): begin   // Signed in32
      DoRead(lInt32, 4);
      aDataObj.AsInt32 := lInt32;
    end;

    byte('L'): begin   // Signed int64
      DoRead(lInt64, 8);
      aDataObj.AsInt32 := lInt64;
    end;

    byte('d'): begin   // float32 (single)
      DoRead(lSingle, 4);
      aDataObj.AsSingle := lSingle;
    end;

    byte('D'): begin   // float64 (double)
      DoRead(lDouble, 8);
      aDataObj.AsDouble := lDouble;
    end;

    byte('H'): begin   // high precision number (Decimal 128 is our limit)
      // High Precision number is really read in string form.
      ReadString;  //FINISH - what are we going to do with this string. Need to finish Decimal 128 and deal with string numbers that could be bigger.
    end;

    byte('C'): begin   // char
      DoRead(lAnsiChar, 1);
      aDataObj.AsString := lAnsiChar;    // a char is really a one character string.
    end;

    byte('S'): begin   // String always UTF-8
      aDataObj.AsString := ReadString;
    end;

    byte('['): begin   // Array
      lChildType := 0;
      lSize := -1;  // means "not defined"

      // pick up the next type to see if its one of our special "optimizing types"
      lTempType := ReadTypeByte;
      if lTempType = byte('$') then
      begin
        // This means that all of the items in this array will be of the following type.  Note that reading a no-op means that all the types in the array will
        // be no-ops so we read the byte directly instead of going through ReadTypeByte because it ignores no-ops
        DoRead(lChildType, 1);
        // Read the next type.
        lTempType := ReadTypeByte;
      end;

      if lTempType = byte('#') then
      begin
        lInt64 := DecodeSize(ReadTypeByte);  // First read the size

        //Check that the size we read is in a useable range
        if lInt64 > $7FFFFFFF  then
          RaiseParsingException('Size of '+Inttostr(lint64)+' is too large to use for an optimized array length.');
        if lInt64<0 then
          RaiseParsingException('Cannot use a negative size of '+Inttostr(lint64)+' for an optimized array length.');
        //FINISH - Check to see if a huge size is given to us causing unrealistic memory allocation and thus a crash.

        lSize := lInt64;
      end
      else
      begin
        // if we did not read a # type code but we did read a $ type code, then we have an error condition because if the optimized type is defined, then the count must also be defined.
        if lChildType <> 0 then
        begin
          RaiseParsingException('Optimized type code($) wasn''t followed by an optimized count code(#).');
        end;
      end;

      if lSize >= 0 then
      begin
        // we know what the size is so loop through and read what is expected.
        // if the lChildType is defined then we already know the type and we just need to read the data portion
        if lChildType <> 0 then
        begin
          for i := 0 to lSize-1 do
          begin
            DecodeType(lChildType, aDataObj.AsArray.NewSlot);
          end;
          // Note there is no array ending byte when we know the count up front
        end
        else
        begin
          for i := 0 to lSize-1 do
          begin
            // we do not have optimized child type defined so we need to read the type for each item in the array because each item could possibly be a different type.
            lChildType := ReadTypeByte;
            DecodeType(lChildType, aDataObj.AsArray.NewSlot);
          end;
          // Note there is no array ending byte when we know the count up front
        end;
      end
      else
      begin
        // We do not have a defined size with the Optimized mechanism so read child elements until we hit the "]" type.
        lChildType := lTempType;
        while lChildType <> byte(']') do
        begin
          DecodeType(lChildType, aDataObj.AsArray.NewSlot);
          lChildType := ReadTypeByte;   // pickup the next type or the end type "]"
        end;
      end;
    end;

    byte('{'): begin   // Object (Frame)
      // The { type is a generic object.
      lChildType := 0;
      lSize := -1;  // means "not defined"

      // pick up the next type to see if its one of our special "optimizing types"
      lTempType := ReadTypeByte;
      if lTempType = byte('$') then
      begin
        // This means that all of the items in this array will be of the following type.  Note that reading a no-op means that all the types in the array will
        // be no-ops so we read the byte directly instead of going through ReadTypeByte because it ignores no-ops
        DoRead(lChildType, 1);
        // Read the next type.
        lTempType := ReadTypeByte;
      end;

      if lTempType = byte('#') then
      begin
        lInt64 := DecodeSize(ReadTypeByte);  // First read the size

        //Check that the size we read is in a useable range
        if lInt64 > $7FFFFFFF  then
          RaiseParsingException('Size of '+Inttostr(lint64)+' is too large to use for an optimized array length.');
        if lInt64<0 then
          RaiseParsingException('Cannot use a negative size of '+Inttostr(lint64)+' for an optimized array length.');
        //FINISH - Check to see if a huge size is given to us causing unrealistic memory allocation and thus a crash.

        lSize := lInt64;
      end
      else
      begin
        // if we did not read a # type code but we did read a $ type code, then we have an error condition because if the optimized type is defined, then the count must also be defined.
        if lChildType <> 0 then
        begin
          RaiseParsingException('Optimized type code($) wasn''t followed by an optimized count code(#).');
        end;
      end;

      if lSize >= 0 then
      begin
        // we know what the size is so loop through and read what is expected.
        // if the lChildType is defined then we already know the type and we just need to read the value portion
        if lChildType <> 0 then
        begin
          for i := 0 to lSize-1 do
          begin
            // First read the slotname
            lSlotName := ReadStringWithSizeType(lTempType);

            // then read the value but we already know the child's Type
            DecodeType(lChildType, aDataObj.AsFrame.NewSlot(lSlotName));
          end;
          // Note there is no frame ending byte when we know the count up front
        end
        else
        begin
          for i := 0 to lSize-1 do
          begin
            // we do not have optimized child type defined so we need to read the type for each item in the array because each item could possibly be a different type.
            // first read the slotname
            lSlotName := ReadStringWithSizeType(lTempType);

            // Then get the type for the data that goes in this slot.
            lChildType := ReadTypeByte;

            // then read the value with the given type
            DecodeType(lChildType, aDataObj.AsFrame.NewSlot(lSlotName));
          end;
          // Note there is no frame ending byte when we know the count up front
        end;
      end
      else
      begin
        // We do not have a defined size with the Optimized mechanism so read child elements until we hit the "}" type.
        while (lTempType <> byte('}')) do
        begin
          // the lTempType we are looking at MUST be a size type cause we are reading the slotName but the slotname has an implied [S] type so it's not in the encoding.
          // this lTempType is a string length, so other type bytes are error conditions
          AssertIsValidSizeType(lTempType);

          lSlotName := ReadStringWithSizeType(lTempType);

          // Then get the type for the data that goes in this slot.
          lChildType := ReadTypeByte;

          // then read the value with the given type
          DecodeType(lChildType, aDataObj.AsFrame.NewSlot(lSlotName));

          // Prepare for the next iteration by loading the next type byte which is either a sizing type for the slotname string which is next, or it's the end of the frame with a "}"
          lTempType := ReadTypeByte;
        end;
      end;
    end;

    else
    begin
      RaiseParsingException('Read invalid type code of '+IntToHex(aType,2));
    end;

  end;
end;




class function TUBJSONStreamer.Description: string;
begin
  result := 'Universal Binary JSON.  http://ubjson.org/ and https://github.com/ubjson/universal-binary-json';
end;

(* NOTE #1:
  So what we are doing here is putting in a NOOP in front of the string.  For all readers out there, this will simply be ignored.  But, for our reader,
  when we see a NOOP followed by a string type identifier, we are going to use that as a mechanism to tell our reader that it should try and process the string
  into a different data type such as this GUID data type based on trying to match the string to the particular pattern for a data type.  So without this NOOP
  preceding the String type identifier, the reader doesn't need to waste time trying to process to a different data type.

  NOTE #2:
  The same pattern described above in note #1 is applied to the UTCDateTime which is a 64 bit integer.  If it's preceded by a NOOP, then the Int64 should be "processed" into
  by our reader into a UTCDateTime.   This method of operation is not a part of the official UBJSON spec, it's just my way of trying to preserve data types to/from UBJSON
  which doesn't model these data types specifically.
*)

procedure TUBJSONStreamer.Encode(aDataObj: TDataObj);
var
  lType: AnsiChar;
  lByte: byte;
  lInt32: Integer;
  lInt64: int64;
  lDouble: double;
  lSingle: single;
  i: Integer;
  lBytes: array[0..3] of byte;

  Procedure WriteSize(aValue: int64);
  begin
    if aValue <= $FF then
    begin
      // Unsigned byte can be used for this string size.
      lType := 'U';
      fStream.Write(lType, 1);
      fStream.Write(aValue, 1);
    end
    else if aValue <= $7FFF then
    begin
      // Signed 16 bit integer
      lType := 'I';
      fStream.Write(lType, 1);
      fStream.Write(aValue, 2);  // I can get by with this because we are little endian
    end
    else if aValue <= $7FFFFFFF then
    begin
      // Signed 32 bit integer
      lType := 'l';
      fStream.Write(lType, 1);
      fStream.Write(aValue, 4);  // I can get by with this because we are little endian
    end
    else
    begin
      // signed 64 bit integer
      lType := 'L';
      fStream.Write(lType, 1);
      fStream.Write(aValue, 4);  // I can get by with this because we are little endian
    end;
  end;

  // NOTE:  this WriteString only writes the string data part, it does not write the string Type code
  procedure WriteString(aString: string);
  var
    lUTF8String: UTF8String;
    lSize: integer;
  begin
    // Convert this string to UTF8 Encoded string
    lUTF8String := UTF8String(aString);

    // Write the Size which is the number of bytes we are going to have in the string.
    lSize := length(lUTF8String);
    WriteSize(lSize);

    // Now write the string bytes.
    fStream.write(lUTF8String[1], lSize);
  end;

begin
  case aDataObj.DataType.Code of
    cDataTypeNull: begin
      lType := 'Z';
      fStream.Write(lType, 1);
    end;

    cDataTypeBoolean: begin
      if aDataObj.AsBoolean then
        lType := 'T'
      else
        lType := 'F';
      fStream.Write(lType, 1);
    end;

    cDataTypeByte: begin
      lType := 'U';
      fStream.Write(lType, 1);
      lByte := aDataObj.AsByte;
      fStream.Write(lByte, 1);
    end;

    cDataTypeInt32: begin
      lType := 'l';
      fStream.Write(lType, 1);
      lInt32 := aDataObj.AsInt32;
      fStream.Write(lInt32, 4);
    end;

    cDataTypeInt64: begin
      lType := 'L';
      fStream.Write(lType, 1);
      lInt64 := aDataObj.AsInt64;
      fStream.Write(lInt64, 8);
    end;

    cDataTypeSingle: begin
      lType := 'd';
      fStream.Write(lType, 1);
      lSingle := aDataObj.AsSingle;
      fStream.Write(lSingle, 4);
    end;

    cDataTypeDouble: begin
      lType := 'D';
      fStream.Write(lType, 1);
      lDouble := aDataObj.AsDouble;
      fStream.Write(lDouble, 8);
    end;

    cDataTypeDecimal128: begin
      // FINISH
    end;

    cDataTypeDateTime: begin
      //See note #1 above
      lBytes[0] := byte('N');
      lBytes[1] := byte('S');
      fStream.Write(lBytes[0], 2);

      WriteString(DateTimeToISO8601Str(aDataobj.AsDateTime));
    end;

    cDataTypeUTCDateTime: begin
      //See note #2 above
      lBytes[0] := byte('N');
      lBytes[1] := byte('L');
      fStream.Write(lBytes[0], 2);

      lInt64 := aDataObj.AsUTCDateTime;
      fStream.Write(lInt64, 8);
    end;

    cDataTypeDate: begin
      //See note #1 above
      lBytes[0] := byte('N');
      lBytes[1] := byte('S');
      fStream.Write(lBytes[0], 2);

      WriteString(DateToISO8601Str(aDataobj.AsDateTime));
    end;

    cDataTypeTime: begin
      //See note #1 above
      lBytes[0] := byte('N');
      lBytes[1] := byte('S');
      fStream.Write(lBytes[0], 2);

      WriteString(TimeToISO8601Str(aDataobj.AsDateTime));
    end;

    cDataTypeGUID: begin
      //See note #1 above
      lBytes[0] := byte('N');
      lBytes[1] := byte('S');
      fStream.Write(lBytes[0], 2);

      WriteString(aDataObj.AsString);
    end;

    cDataTypeObjectID: begin
      //See note #1 above
      lBytes[0] := byte('N');
      lBytes[1] := byte('S');
      fStream.Write(lBytes[0], 2);

      WriteString(aDataObj.AsString);
    end;

    cDataTypeString: begin
      lType := 'S';
      fStream.Write(lType, 1);

      WriteString(aDataObj.AsString);
    end;

    cDataTypeStringList: begin
      // We will generate an array of strings using the optimized structure for an array of strings.
      // this works good cause then when the decoder sees this pattern, it can read the data into a StringList again.
      lBytes[0] := byte('[');    // Array marker
      lBytes[1] := byte('$');    // Optimized type marker
      lBytes[2] := byte('S');    // Optimized type is string.
      lBytes[3] := byte('#');    // Optimized Count Marker

      fStream.Write(lBytes, 4);  // write the markers

      WriteSize(aDataObj.AsStringList.Count);

      for i := 0 to aDataObj.AsStringList.Count-1 do
      begin
        // Write each value.  Note, we do not need to write the type for each item because we are using the optimized form which does it once above.
        WriteString(aDataObj.AsStringList.Strings[i]);
      end;

      // NOTE: we do not write the end array marker with an optimized array.
    end;

    cDataTypeFrame: begin
      // NOTE:  In the future, we could scan the slots and if they are all of the same type we could do an optimized object serialization someday.
      lType := '{';
      fStream.Write(lType, 1);
      for i := 0 to aDataObj.AsFrame.Count-1 do
      begin
        // write the slotname
        WriteString(aDataObj.AsFrame.Slotname(i));

        // Then write the value.
        Encode(aDataObj.AsFrame.Slots[i]);    // recursion happening here.
      end;

      lType := '}';
      fStream.Write(lType, 1);
    end;

    cDataTypeArray: begin
      // NOTE:  In the future, we could scan the slots and if they are all of the same type we could do an optimized array serialization someday.
      lType := '[';
      fStream.Write(lType, 1);

      for i := 0 to aDataObj.AsArray.Count-1 do
      begin
        // Write each value
        Encode(aDataObj.AsArray.Slots[i]);    // recursion happening here.
      end;

      lType := ']';                            // Write the End of Array marker
      fStream.Write(lType, 1);
    end;

    cDataTypeSparseArray: begin
      //UBJSON does not support the concept of a sparse array.  So, we have to create a frame (object) with string representations of the index values
      lType := '{';
      fStream.Write(lType, 1);

      for i := 0 to aDataObj.AsSparseArray.Count-1 do
      begin
        // write the slotname which is the slotIndex as a string
        WriteString(IntToStr(aDataObj.AsSparseArray.SlotIndex(i)));

        // Write each value
        Encode(aDataObj.AsSparseArray.Slots[i]);    // recursion happening here.
      end;

      lType := '}';
      fStream.Write(lType, 1);                  // Write the End of Object Marker
    end;

    cDataTypeBinary: begin
      // We will generate an array of bytes using the optimized structure for an array of bytes.
      // see http://ubjson.org/type-reference/binary-data/

      lBytes[0] := byte('[');    // Array marker
      lBytes[1] := byte('$');    // Optimized type marker
      lBytes[2] := byte('U');    // Optimized type is unsigned byte.
      lBytes[3] := byte('#');    // Optimized Count Marker

      fStream.Write(lBytes, 4);  // write the markers

      WriteSize(aDataObj.AsBinary.Size);        // Write the count of bytes

      aDataObj.AsBinary.SaveToStream(fStream);  // Write the actual bytes.

      // NOTE: we do not write the end array marker with an optimized array.
    end;

    cDataTypeObject: begin
      //Finish - we could put this directly to a frame (document).
    end;

    cDataTypeTag: begin
      // Finish - for now we are skipping writing out the tags.  We will figure out how to do this at some point.
      // For now, just write out the embedded child DataObject
      Encode(aDataObj.AsTag.DataObj);   //recursion happening here.
    end;
  end;
end;


class function TUBJSONStreamer.FileExtension: string;
begin
  result := 'ubj';
end;

initialization
  RegisterDataObjStreamer(TUBJSONStreamer);

end.
