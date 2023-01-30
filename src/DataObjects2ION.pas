unit DataObjects2ION;

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

uses classes, DataObjects2, DataObjects2Streamers, SysUtils, RTTI, TypInfo, DataObjects2Utils, VarInt;

type
  TIonStreamer = class(TDataObjStreamerBase)
  private
    procedure WriteString(aString: String);
    procedure InternalEncode(aDataObj: TDataObj);

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

//resourceString

procedure RaiseParsingException(aStream: TStream; aMessage: string);
begin
  raise Exception.Create(aMessage+' when reading a ION Stream at position='+intToStr(aStream.Position));
end;




class function TIonStreamer.GetFileFilter: string;
begin
  result := 'Amazon ION Files (*.ion)|*.ion';
end;

class function TIonStreamer.IsFileExtension(aStr: string): boolean;
begin
  result := SameText(aStr, '.ion') or SameText(aStr, 'ion');
end;


procedure TIonStreamer.WriteString(aString: String);
var
  lLen: integer;
  lByte: byte;
  lLength: TUVarInt64;     // we could get by with a 32 bit version of this to be a tad more efficient but this works fine too.
  lUTF8String: UTF8String;
begin
  lUTF8String := UTF8String(aString);   // convert to UTF8String.
  lLen := length(lUTF8String);

  if lLen<14 then
  begin
    lByte := $80 or byte(lLen);   // size is 13 or less as 14 and 15 have special meaning
    fStream.Write(lByte,1);
    fStream.Write(lUTF8String[1],lLen);
  end
  else
  begin
    lLength := lLen;
    lByte := $8E;                      // code that means UTF8String and the length will follow
    lLength.WriteToStream(fStream);    // write out the string length
    fStream.Write(lUTF8String[1],lLen);    // write out the string
  end;
end;

procedure TIonStreamer.Encode(aDataObj: TDataObj);
const
  cBVM: array[0..3] of byte = ($E0, 1, 0, $EA);
begin
  // write out the Binary Version Marker first and foremost.
  fStream.Write(cBVM, 4);

  internalEncode(aDataObj);
end;



class function TIonStreamer.FileExtension: string;
begin
  result := 'ion';
end;

procedure TIonStreamer.InternalEncode(aDataObj: TDataObj);
type
  TFiveBytes = record
    TypeDescriptor: byte;
    case byte of
      0: (CardinalValue: cardinal;);
      1: (SingleValue: single;);
    end;

  TNineBytes = record
    TypeDescriptor: byte;
    case byte of
      0: (Int64Value: Int64;);
      1: (DoubleValue: double;);
    end;
    
var
  i: integer;
  lStringList: TDataStringList;
  lFrame: TDataFrame;
  lArray: TDataArray;
  lSparseArray: TDataSparseArray;
  lBinary: TDataBinary;

  lByte: byte;
  lBytes: array[0..5] of byte;
  lFiveBytes: TFiveBytes;
  lNineBytes: TNineBytes;
  lSize: Cardinal;
  lLength: TUVarInt64;
  lSymbolIDValue: TUVarInt64;

  lMemStream: TMemoryStream;
  lSavedStream: TStream;

  // Will find the symbolID for aSymbol in the current symbol table or it will add this symbol to the current symbol table and return it's new symbolID.
  function GetSymbolID(aSymbol: string): cardinal;
  begin
    //FINISH
    result := 0;
  end;

  procedure WriteSymbol(aSymbol: string);
  var
    lSymbolID: cardinal;
  begin
    // see http://amzn.github.io/ion-docs/docs/symbols.html for more information.
    lSymbolID := GetSymbolID(aSymbol);
    if lSymbolID <= $FF then
    begin
      lBytes[0] := $71;         // writing a symbol with a 1 byte symbolID
      lBytes[1] := byte(lSymbolID);
      fStream.Write(lBytes, 2);
    end
    else if lSymbolID <= $FFFF then
    begin
      lBytes[0] := $72;         // writing a symbol with a 2 byte symbolID
      lBytes[1] := byte(lSymbolID shr 8);   // MSB first (big-endian)
      lBytes[2] := byte(lSymbolID);         // LSB
      fStream.Write(lBytes, 3);
    end
    else if lSymbolID <= $FFFFFF then
    begin
      // NOTE, this is going to be extremely rare that we would ever have symbolIDs this big. We would have to have over 65535 unique symbols in our payload for this.
      lBytes[0] := $73;         // writing a symbol with a 3 byte symbolID
      lBytes[1] := byte(lSymbolID shr 16);  // MSB first (big-endian)
      lBytes[2] := byte(lSymbolID shr 8);
      lBytes[3] := byte(lSymbolID);         // LSB
      fStream.Write(lBytes, 4);
    end
    else
    begin
      // NOTE, this is pretty much impractical to get here as we would need to have 16,777,216 unique symbols in our payload
      lFiveBytes.TypeDescriptor := $74;         // writing a symbol with a 4 byte symbolID
      lFiveBytes.CardinalValue := lSymbolID;
      fStream.Write(lFiveBytes,5);
    end;
  end;

begin
  case aDataObj.DataType.Code of
    cDataTypeNull: begin
      lByte := $0F;
      fStream.Write(lByte,1);
    end;

    cDataTypeBoolean: begin
      if aDataObj.AsBoolean then
        lByte := $11
      else
        lByte := $10;
      fStream.Write(lByte,1);
    end;

    cDataTypeByte: begin
      lBytes[0] := $21;                 //type 2 with 1 byte of data.
      lBytes[1] := aDataObj.AsByte;     //NOTE:  in the dataObject byte data type, it is always unsigned.  0..255.
      fStream.Write(lBytes, 2);
    end;

    cDataTypeInt32: begin                          // we are treating the int32 as signed.
      if aDataObj.AsInt32 < 0 then
      begin
        lFiveBytes.TypeDescriptor := $34;             //type 3 with 4 bytes of data.
        lFiveBytes.CardinalValue := -aDataObj.AsInt32;    // negate this negative number because the "negative" part is defined by the 3 in the type.
        fStream.Write(lFiveBytes, 5);
      end
      else
      begin
        lFiveBytes.TypeDescriptor := $24;             //type 2 with 4 bytes of data.
        lFiveBytes.CardinalValue := aDataObj.AsInt32;
        fStream.Write(lFiveBytes, 5);
      end;
    end;

    cDataTypeInt64: begin
      if aDataObj.AsInt64<0 then
      begin
        lNineBytes.TypeDescriptor := $38;             //type 3 with 8 bytes of data.
        lNineBytes.Int64Value := -aDataObj.AsInt64;
        fStream.Write(lNineBytes, 9);
      end
      else
      begin
        lNineBytes.TypeDescriptor := $28;             //type 2 with 8 bytes of data.
        lNineBytes.Int64Value := aDataObj.AsInt64;
        fStream.Write(lNineBytes, 9);
      end;
    end;

    cDataTypeSingle: begin
      if aDataObj.AsSingle = 0 then
      begin
        // SO, in ION, how does one distinguish when reading a single float that is zero vs. a double float that is zero.  I don't think you can.
        lByte := $40;
        fStream.Write(lByte,1);    // special code for floating point number that is zero.
      end
      else
      begin
        lFiveBytes.TypeDescriptor := $44;             //type 4 float with 4 bytes of data.
        lFiveBytes.SingleValue := aDataObj.AsSingle;
        fStream.Write(lFiveBytes, 5);
      end;
    end;

    cDataTypeDouble: begin
      if aDataObj.AsDouble = 0 then
      begin
        // SO, in ION, how does one distinguish when reading a single float that is zero vs. a double float that is zero.  I don't think you can.
        lByte := $40;
        fStream.Write(lByte,1);    // special code for floating point number that is zero.
      end
      else
      begin
        lNineBytes.TypeDescriptor := $48;             //type 4 float with 8 bytes of data.
        lNineBytes.DoubleValue := aDataObj.AsDouble;
        fStream.Write(lNineBytes, 9);
      end;
    end;

//    cDataTypeDecimal128: aStringBuilder.Append(lStore.dataInt64);

    cDataTypeDateTime: begin
      //FINISH
    end;

    // for the unix UTC time do we publish in string notation or in the int64 notation.    I say in64 notation since that's the purpose of this data type
    cDataTypeUTCDateTime: begin
      //FINISH
    end;

    cDataTypeDate: begin
      //FINISH
    end;

    cDataTypeTime: begin
      //FINISH
    end;

    cDataTypeGUID: begin
      //FINISH
    end;

    cDataTypeObjectID: begin
      //FINISH
    end;

    cDataTypeString: begin
      WriteString(aDataObj.AsString);    // convert to UTF8 string and serialize
    end;

    cDataTypeStringList: begin
      // We are going to code the StringList data type as an array of Strings.
      // FINISH - maybe we can possibly wrap this in an annotation so the reader can know the that list it will see isn't just a generic list, but rather a list of strings to put into this data type.
      lStringList := aDataObj.AsStringList;

      lMemStream:=TMemoryStream.Create;
      lSavedStream := fStream;            // save the stream we are writing to so we can restore it below
      try
        fStream := lMemStream;            // we are now making our ecoder put into our new temporary memStream.

        // write out the symbol-value pairs into our temporary memory stream so we can then know the size of it.
        for i := 0 to lStringList.Count-1 do
        begin
          WriteString(lStringList[i]);// note: converting from string to UTF8String here.
        end;
                                          // now that we have written out the slots to a memory stream, we now know the size.
        lSize := lMemStream.Size;         // note, only supporting 4Gig stream here, not full int64 size

        if lSize <= 13 then
        begin
          lByte := $B0 or byte(lSize);    // writing out a type code of "list".
          fStream.Write(lByte, 1);
        end
        else
        begin
          lByte := $BE;                   // code that means the length will follow
          lLength := lSize;
          fStream.Write(lByte, 1);
          lLength.WriteToStream(fStream); // write out the number of items in this array (list)
        end;

        // this size has been written either in the L field or in the following Length field, so now write out the full content that we got in our temporary memStream.
        lMemStream.Seek(0, soBeginning);
        lSavedStream.CopyFrom(lMemStream, lSize);
      finally
        fStream := lSavedStream;       // restore our streamer back to the stream we had when we started this frame
        lMemStream.Free;
      end;
    end;

    cDataTypeFrame: begin
      lFrame := aDataObj.AsFrame;

      // FINISH - The first thing we need to write out is the type & Length.  But the length we write isn't the number of slots in this frame(struct), but rather
      //          the total number of bytes for all the content of this frame.  This sucks cause it means we must first serialize each of the SymbolID-Value pairs
      //          into a temporary stream in order to know what the length will be.

      { When L is 0, the value is an empty struct, and there’s no length or nested fields.
        When L is 1, the struct has at least one symbol/value pair, the length field exists, and the field name integers are sorted in increasing order.
        When L is 15, the value is null.struct, and there’s no length or nested fields.
        When 1 < L < 14 then there is no length field as L is enough to represent the struct size, and no assertion is made about field ordering.
        Otherwise, the length field exists, and no assertion is made about field ordering.}

      lMemStream:=TMemoryStream.Create;
      lSavedStream := fStream;          // save the stream we are writing to so we can restore it below
      try
        fStream := lMemStream;            // we are now making our ecoder put into our new temporary memStream.

        // write out the symbol-value pairs into our temporary memory stream so we can then know the size of it.
        for i := 0 to lFrame.Count-1 do
        begin
          lSymbolIDValue := GetSymbolID(lFrame.Slotname(i));
          lSymbolIDValue.WriteToStream(fStream);     // write out the symbolID for this slot.

          Encode(lFrame.Slots[i]);                // Write out the value of this slot.
        end;

        // now that we have written out the slots to a memory stream, we now know the size.
        lSize := lMemStream.Size;                 // note, only supporting 4Gig stream here, not full int64 size
        if lSize <= 13 then
        begin
          lByte := $D0 or byte(lSize);          // size could be 0 through 13.
          lSavedStream.Write(lByte, 1);
        end
        else
        begin
          lByte := $DE;                           // code that means the length will follow
          lSavedStream.Write(lByte, 1);
          lLength := lSize;
          lLength.WriteToStream(lSavedStream);    // write out the string length
        end;

        // this size has been written either in the L field or in the following Length field, so now write out the full content that we got in our temporary memStream.
        lMemStream.Seek(0, soBeginning);
        lSavedStream.CopyFrom(lMemStream, lSize);
      finally
        fStream := lSavedStream;       // restore our streamer back to the stream we had when we started this frame
        lMemStream.Free;
      end;
    end;

    cDataTypeArray: begin
      lArray := aDataObj.AsArray;

      lMemStream:=TMemoryStream.Create;
      lSavedStream := fStream;            // save the stream we are writing to so we can restore it below
      try
        fStream := lMemStream;            // we are now making our encoder put into our new temporary memStream.

        // write out the symbol-value pairs into our temporary memory stream so we can then know the size of it.
        for i := 0 to lArray.Count-1 do
        begin
          Encode(lArray.Slots[i]);        // recursion happening here.
        end;
                                          // now that we have written out the slots to a memory stream, we now know the size.
        lSize := lMemStream.Size;         // note, only supporting 4Gig stream here, not full int64 size

        if lSize <= 13 then
        begin
          lByte := $B0 or byte(lSize);
          fStream.Write(lByte, 1);
        end
        else
        begin
          lByte := $BE;                   // code that means the length will follow
          lLength := lSize;
          fStream.Write(lByte, 1);
          lLength.WriteToStream(fStream); // write out the number of items in this array (list)
        end;

        // this size has been written either in the L field or in the following Length field, so now write out the full content that we got in our temporary memStream.
        lMemStream.Seek(0, soBeginning);
        lSavedStream.CopyFrom(lMemStream, lSize);
      finally
        fStream := lSavedStream;       // restore our streamer back to the stream we had when we started this frame
        lMemStream.Free;
      end;
    end;

    cDataTypeSparseArray: begin
      lSparseArray := aDataObj.AsSparseArray;

      // ION doesn't have the concept of a sparse array, so we will have to write out the numerical slotIndex values as string symbols.   Then, we can
      // serialize like it is a struct.
      // FINISH - If we wrap this whole thing in an annotation with a special code, then our reading code can properly know that it should read this back
      //          into a sparseArray instead of a frame.

      lMemStream:=TMemoryStream.Create;
      lSavedStream := fStream;            // save the stream we are writing to so we can restore it below
      try
        fStream := lMemStream;            // we are now making our encoder put into our new temporary memStream.

        // write out the symbol-value pairs into our temporary memory stream so we can then know the size of it.
        for i := 0 to lSparseArray.Count-1 do
        begin
          // Make or lookup the symbol for this slotIndex as a string
          // for the ion struct, we don't write out a full "symbol", but rather we just write out the value of the symbol.  That's why we can't call WriteSymbol here.
          lSymbolIDValue := GetSymbolID(IntToStr(lSparseArray.SlotIndex(i)));
          lSymbolIDValue.WriteToStream(fStream);

          // Then write out the contained object.
          Encode(lSparseArray.Slots[i]);  // recursion happening here.
        end;
                                          // now that we have written out the slots to a memory stream, we now know the size.
        lSize := lMemStream.Size;         // note, only supporting 4Gig stream here, not full int64 size

        if lSize <= 13 then
        begin
          lByte := $D0 or byte(lSize);
          fStream.Write(lByte, 1);
        end
        else
        begin
          lByte := $DE;                   // code that means the length will follow
          lLength := lSize;
          fStream.Write(lByte, 1);
          lLength.WriteToStream(fStream); // write out the number of items in this array (list)
        end;

        // this size has been written either in the L field or in the following Length field, so now write out the full content that we got in our temporary memStream.
        lMemStream.Seek(0, soBeginning);
        lSavedStream.CopyFrom(lMemStream, lSize);
      finally
        fStream := lSavedStream;       // restore our streamer back to the stream we had when we started this frame
        lMemStream.Free;
      end;
    end;

    cDataTypeBinary: begin
      lBinary := aDataObj.AsBinary;
      if lBinary.Size <= 13 then
      begin
        lByte := $A0 or byte(lBinary.Size); // Code for blob with L included as a size from 0 to 13 only
        fStream.Write(lByte, 1);
      end
      else
      begin
        lByte := $AE;                       // Code for blob with Length field
        fStream.Write(lByte, 1);
        lLength := lBinary.Size;
        lLength.WriteToStream(fStream);
      end;
      fStream.CopyFrom(aDataObj.AsBinary, lBinary.Size);
    end;

    cDataTypeObject: begin
      //Finish
    end;

    cDataTypeTag: begin
      // FINISH - found that other implementations have an option to put tags into JSON by containing the tag using a Json Object (frame) with a certain slotName naming convention.
      //          maybe we should support this concept too.
      // see https://github.com/intel/tinycbor/commit/782f2545a07e707464c6e9b417768e8b980c8e13

      // For now, we are skipping over the tagging portion and just streaming out the contained DataObject to MsgPack
      Encode(aDataObj.AsTag.DataObj);
    end;

  end;

end;


class function TIonStreamer.ClipboardPriority: cardinal;
begin
  result := 30;
end;


procedure TIonStreamer.Decode(aDataObj: TDataObj);
begin
  try
//  fStream.Read(lMajorType, 1);

  except
//    RaiseParsingException();

  end;
end;



class function TIonStreamer.Description: string;
begin
  result := 'Amazon Ion Format.  https://amzn.github.io/ion-docs/';
end;

initialization
  RegisterDataObjStreamer(TIonStreamer);

end.
