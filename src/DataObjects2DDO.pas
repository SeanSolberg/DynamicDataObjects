unit DataObjects2DDO;

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
{ This unit reads the traditional *.ddo file stream format.  This unit pretty much works although it has not been exhautively tested for streaming all the ddo
  data types and not all the data types have had code written to convert out to something in DDO format.

  NOTE:  this streamer cannot be used by android apps in its existing form because it uses ansiString for serialization.
}

uses classes, DataObjects2, DataObjects2Streamers, SysUtils, RTTI, TypInfo, DataObjects2Utils;

type
  TDDOStreamer = class(TDataObjStreamerBase)
  private
    procedure GenerateException(aMessage: string);

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

resourceString
  StrException = '%s when reading a DDO Stream at position = %d';
  StrUnableToReadDDO = 'Unable to Read DDO from Stream: ';
  StrInvalidNumberOfBytes1 = 'Invalid number of bytes read while reading DDO slot type.';
  StrInvalidNumberOfBytes2 = 'Invalid number of bytes read while reading symbol/string size.';
  StrInvalidNumberOfBytes3 = 'Invalid number of bytes read while reading symbol/string data.';
  StrInvalidNumberOfBytes4 = 'Invalid number of bytes read while reading integer data.';
  StrInvalidNumberOfBytes5 = 'Invalid number of bytes read while reading boolean data.';
  StrInvalidNumberOfBytes6 = 'Invalid number of bytes read while reading float data.';
  StrInvalidNumberOfBytes7 = 'Invalid number of bytes read while reading datetime data.';
  StrInvalidNumberOfBytes8 = 'Invalid number of bytes read while reading array slot count.';
  StrInvalidNumberOfBytes9 = 'Invalid number of bytes read while reading byte data.';
  StrInvalidNumberOfBytes10 = 'Invalid number of bytes read while reading binary data size.';
  StrInvalidNumberOfBytes11 = 'Invalid number of bytes read while reading binary data.';
  StrInvalidNumberOfBytes12 = 'Invalid number of bytes read while reading stringlist data size.';
  StrInvalidNumberOfBytes13 = 'Invalid number of bytes read while reading geometry type.';
  StrInvalidNumberOfBytes14 = 'Invalid number of bytes read while reading point geometry.';
  StrInvalidNumberOfBytes15 = 'Invalid number of bytes read while reading int64 data.';
  StrInvalidNumberOfBytes16 = 'Invalid number of bytes read while reading unknown data type size.';
  StrInvalidPayloadSizeForNull = 'Invalid payload size of %d while reading a NULL value';
  StrErrorLoadingString = 'Error loading stringlist from stream.';
  StrErrorReadingSlotNameSize = 'Read slotname size of %d which is a size code for a slotname lookup index.  This capability is not supported on this reader.' ;

procedure TDDOStreamer.GenerateException(aMessage: string);
begin
  raise Exception.Create(format(strException, [aMessage, fStream.Position]));
end;


class function TDDOStreamer.GetFileFilter: string;
begin
  result := 'DDO Files (*.ddo)|*.ddo';
end;

class function TDDOStreamer.IsFileExtension(aStr: string): boolean;
begin
  result := SameText(aStr, '.ddo') or SameText(aStr, 'ddo');
end;



procedure TDDOStreamer.Encode(aDataObj: TDataObj);
var
  i: integer;
  lStringList: TDataStringList;
  lFrame: TDataFrame;
  lArray: TDataArray;
  lSparseArray: TDataSparseArray;
  lBinary: TDataBinary;
  lTempStream: TMemoryStream;

  lByte: byte;
  lSize: Cardinal;
  lAnsiStr: AnsiString;
  lInteger: integer;
  lBoolean: boolean;
  lDouble: double;
  lInt64: int64;
  lDateTime: TDateTime;

begin
  case aDataObj.DataType.Code of
    cDataTypeNull: begin
      lInteger := 0;  // code for nil
      fStream.Write(lInteger,4);    // Write out the slot type and thats all there is for this datatype
      fStream.Write(lInteger,4);    // NOTE:  The original DataObject serialization actually did something wierd in that it considered the NULL data type to be in the class of "Unknown" data types.
                                    //        As such, it would put into the stream a 0 for the data size (NULL Data size is zero) to make receiver happy.  SO, we do that here.  STUPID, but we are just trying to be compatible here.
    end;

    cDataTypeBoolean: begin
      lInteger := 3;  // code for boolean
      fStream.Write(lInteger,4);    // Write out the slot type

      lBoolean := aDataObj.AsBoolean;
      fStream.Write(lBoolean,1);
    end;

    cDataTypeByte: begin
      lInteger := 9;  // code for byte
      fStream.Write(lInteger,4);    // Write out the slot type

      lByte := aDataObj.AsByte;     //NOTE:  in the dataObject byte data type, it is always unsigned.  0..255.
      fStream.Write(lByte, 1);      // write the 4 bytes that make up the integer
    end;

    cDataTypeInt32: begin           // we are treating the int32 as signed.
      lInteger := 2;  // code for integer
      fStream.Write(lInteger,4);    // Write out the slot type

      lInteger := aDataObj.AsInt32;
      fStream.Write(lInteger, 4);   // write the 4 bytes that make up the integer
    end;

    cDataTypeInt64: begin
      lInteger := 13;  // code for int64
      fStream.Write(lInteger,4);    // Write out the slot type

      lInt64 := aDataObj.AsInt64;
      fStream.Write(lInt64, 8);
    end;

    cDataTypeSingle, cDataTypeDouble: begin                     // Note that DDO doesn't have a single floating point, so we have to convert it to a double.
      lInteger := 4;  // code for double float
      fStream.Write(lInteger,4);    // Write out the slot type

      lDouble := aDataObj.AsDouble;
      fStream.Write(lDouble, 8);
    end;

//    cDataTypeDecimal128: aStringBuilder.Append(lStore.dataInt64);
//  FUTURE

    cDataTypeDateTime: begin
      lInteger := 5;  // code for TDateTime
      fStream.Write(lInteger,4);    // Write out the slot type

      lDateTime:=aDataObj.AsDateTime;
      fStream.Write(lDateTime, 8);
    end;

    // for the unix UTC time do we publish in string notation or in the int64 notation.    I say int64 notation since that's the purpose of this data type
    cDataTypeUTCDateTime: begin
      //FUTURE - we could have a preference on whether we want to stream as TDateTime or as Int64.
      // For now, we are going to stream this as a TDateTime slot
      lInteger := 5;  // code for TDateTime
      fStream.Write(lInteger,4);    // Write out the slot type

      lDateTime:=aDataObj.AsDateTime;
      fStream.Write(lDateTime, 8);
    end;

    cDataTypeDate: begin
      lInteger := 5;  // code for TDateTime.  Note that DDO only has TDateTime, not TDate
      fStream.Write(lInteger,4);    // Write out the slot type

      lDateTime:=aDataObj.AsDate;
      fStream.Write(lDateTime, 8);
    end;

    cDataTypeTime: begin
      lInteger := 5;  // code for TDateTime.  Note that DDO only has TDateTime, not TTime
      fStream.Write(lInteger,4);    // Write out the slot type

      lDateTime:=aDataObj.AsTime;
      fStream.Write(lDateTime, 8);
    end;

    cDataTypeGUID, cDataTypeObjectID: begin
      //FUTURE - we could have a preference on whether we want to stream a GUID as a string formatted GUID or as a binary form GUID.  For now, we will do string GUID.
      //FUTURE - we could have a preference on whether we want to stream an ObjectID as a string formatted ObjectID or as a binary form ObjectID.  For now, we will do string ObjectID

      lInteger := 1;   // code for string
      fStream.Write(lInteger, 4);

      lAnsiStr := AnsiString(aDataObj.AsString);  //converting to ansiString.  potential data loss, but that's how DDO specs it.
      lSize:=Length(lAnsiStr);
      fStream.Write(lSize, 4);           // write 4 bytes representing the size of the string
      if lSize > 0 then
        fStream.Write(lAnsiStr[1], lSize);  // write the string data. Not Including the Null Byte.
    end;

    cDataTypeString: begin
      if aDataObj.DataType.SubClass=1 then    // NOTE that we have not defined a purpose for the values 2 and 3 yet, so we just treat them as strings by default.
        lInteger := 8   // code for symbol
      else
        lInteger := 1;   // code for string
      fStream.Write(lInteger, 4);

      lAnsiStr := AnsiString(aDataObj.AsString);  //converting to ansiString.  potential data loss, but that's how DDO specs it.
      lSize:=Length(lAnsiStr);
      fStream.Write(lSize, 4);           // write 4 bytes representing the size of the string
      if lSize > 0 then
        fStream.Write(lAnsiStr[1], lSize);  // write the string data. Not Including the Null Byte.
    end;


    cDataTypeStringList: begin
      lInteger := 11;   // code for stringList
      fStream.Write(lInteger, 4);

      lStringList := aDataObj.AsStringList;

      lTempStream:=TMemoryStream.create;
      try
        lStringList.SaveToStream(lTempStream);
        lSize:=lTempStream.Size;
        fStream.Write(lSize, 4);  // writes 4 bytes representing the size of the stringlist in the stream
        lTempStream.Seek(0, soFromBeginning);
        fStream.CopyFrom(lTempStream, lSize);
      finally
        lTempStream.Free;
      end;
    end;

    cDataTypeFrame: begin
      lFrame := aDataObj.AsFrame;
      lSize := lFrame.Count;

      lInteger := 6;   // code for frame
      fStream.Write(lInteger, 4);

      fStream.Write(lSize, 4);    // write 4 bytes representing the size of this frame (# of slots)
      if lSize>0 then             // need this check for zero to prevent the lSize-1 call below if lSize=0 because a range exception would fire.
      begin
        for i:=0 to lSize-1 do
        begin
          // first write the slot name which needs to be an ansiString to follow spec.
          lAnsiStr := AnsiString(aDataObj.AsFrame.Slotname(i));

          lSize := Length(lAnsiStr);

          if lSize <= 250 then      // Due to the spec for DDO, we need to cap the limit of the slotnames to 250 bytes.
            lByte := byte(lSize)
          else
            lByte := 250;

          fStream.Write(lByte, 1);  // write 1 byte representing the size of the slot name string
          if lSize > 0 then
            fStream.Write(lAnsiStr[1], lSize);  // write the slot Name. Not Including the Null Byte.

          // then write the slot data
          Encode(lFrame.Slots[i]);  // recursion happening here.
        end;
      end;
    end;

    cDataTypeArray: begin
      lArray := aDataObj.AsArray;
      lSize := lArray.Count;

      lInteger := 7;   // code for array
      fStream.Write(lInteger, 4);

      fStream.Write(lSize, 4);   // write 4 bytes representing the size of this array (# of slots)
      for i:=0 to lArray.count-1 do
      begin
        // then write the slot data
        Encode(lArray.Slots[i]);    // recursion happening here.
      end;
    end;

    cDataTypeSparseArray: begin
      lSparseArray := aDataObj.AsSparseArray;
      lSize := lSparseArray.Count;

      lInteger := 6;   // code for frame
      fStream.Write(lInteger, 4);

      //DDO doesn't have the concept of a sparse Array, so we will send the data as a frame where the slotnames in the frame are the string representations of the integer indexes from the sparse Array.
      fStream.Write(lSize, 4);    // write 4 bytes representing the size of this frame (# of slots)
      if lSize>0 then             // need this check for zero to prevent the lSize-1 call below if lSize=0 because a range exception would fire.
      begin
        for i:=0 to lSize-1 do
        begin
          // first write the slots Index in terms of a string which needs to be an ansiString to follow spec.
          lAnsiStr := AnsiString(IntToStr(lSparseArray.SlotIndex(i)));
          lSize := Length(lAnsiStr);
          lByte := byte(lSize);
          fStream.Write(lByte, 1);            // write 1 byte representing the size of the slot name string
          fStream.Write(lAnsiStr[1], lSize);  // write the slot Name. Not Including the Null Byte.

          // then write the slot data
          Encode(lSparseArray.Slots[i]);  // recursion happening here.
        end;
      end;
    end;

    cDataTypeBinary: begin
      lInteger := 10;   // code for binary
      fStream.Write(lInteger, 4);

      lBinary := aDataObj.AsBinary;
      lSize := lBinary.Size;      // NOTE:  DDO only support up to 32 bit length.  So, if it's actually more than that then we need a future error handling here.
      fStream.Write(lSize, 4);    // writes the 4 byte size
      aDataObj.AsBinary.Seek(0, soFromBeginning);   // just to make sure.
      fStream.CopyFrom(aDataObj.AsBinary, lSize);
    end;

    cDataTypeObject: begin
      //Finish
    end;
    cDataTypeTag: begin
      // FINISH - found that other implementations have an option to put tags into JSON by containing the tag using a Json Object (frame) with a certain slotName naming convention.
      //          maybe we should support this concept too.
      // see https://github.com/intel/tinycbor/commit/782f2545a07e707464c6e9b417768e8b980c8e13

      // For now, we are skipping over the tagging portion and just streaming out the contained DataObject to DDO
      Encode(aDataObj.AsTag.DataObj);
    end;
  end;

end;


class function TDDOStreamer.FileExtension: string;
begin
  result := 'ddo';
end;

class function TDDOStreamer.ClipboardPriority: cardinal;
begin
  result := 10;
end;

procedure TDDOStreamer.Decode(aDataObj: TDataObj);
var
  lSize: cardinal;
  lAnsiString: AnsiString;
  lNum: cardinal;
  lMemStream: TMemoryStream;
  lSlotType: LongInt;
  lInteger: integer;
  lBoolean: boolean;
  lDouble: double;
  lDateTime: double;
  i,j: Integer;
  lByte: byte;
  lStringList: TDataStringList;
  lFrame: TDataFrame;
  lPartArray: TDataArray;
  lPointCount: integer;
  lPartFlags: byte;
  lPointArray: TDataArray;
  lInt64: Int64;
begin
  try
    lNum:=fStream.Read(lSlotType, 4);
    if lNum<>4 then GenerateException(StrInvalidNumberOfBytes1);
    case lSlotType of
      0{null}: begin
        aDataObj.Clear;

        // Nothing more to read.  HOWEVER, the old DDO Streaming did something wierd where it considered the NULL data type as an "Unknown" data type, and
        // as such, it needs to read a 4-byte size of the payload, which of course is going to be all zeros because there is no payload.  This is Stupid, but we need this to be compatible.
        lNum:=fStream.Read(lSize, 4);    // get the size of the data which MUST be a zero.
        if lNum <> 4 then GenerateException(StrInvalidNumberOfBytes2);
        if lSize <> 0 then GenerateException(format(StrInvalidPayloadSizeForNull, [lSize]));
      end;

      1{string}, 8{symbol}: begin
        lNum:=fStream.Read(lSize, 4);    // get the size of the data
        if lNum <> 4 then GenerateException(StrInvalidNumberOfBytes2);
        SetLength(lAnsiString, lSize);       // allocate the space for the new ansiString
        if lSize > 0 then
          lNum:=fStream.Read(lAnsiString[1], lSize)
        else
          lNum := 0;

        if lNum<>lSize then GenerateException(StrInvalidNumberOfBytes3);
        if lSlotType = 1 then
          aDataObj.AsString := String(lAnsiString)
        else
          aDataObj.AsSymbol := String(lAnsiString);
      end;

      2{integer}: begin
        lNum:=fStream.Read(lInteger, 4);   // Integers are always 4 bytes long
        if lNum<>4 then GenerateException(StrInvalidNumberOfBytes4);
        aDataObj.AsInt32 := lInteger;      // the Int32 is the closest type to a DDO's integer
      end;

      3{boolean}: begin
        lNum:=fStream.Read(lBoolean, 1);
        if lNum<>1 then GenerateException(StrInvalidNumberOfBytes5);
        aDataObj.AsBoolean := lBoolean;
      end;

      4{float-double}: begin
        lNum:=fStream.Read(lDouble, 8);
        if lNum<>8 then GenerateException(StrInvalidNumberOfBytes6);
        aDataObj.AsDouble := lDouble;
      end;

      5{TDateTime}: begin
        lNum:=fStream.Read(lDateTime, 8);
        if lNum<>8 then GenerateException(StrInvalidNumberOfBytes7);
        aDataObj.AsDateTime := lDateTime;
      end;

      6{Frame}: begin
        fStream.Read(lSize, 4);
        if lSize>0 then             // need this check for zero to prevent the lSize-1 call below if lSize=0 because a range exception would fire.
        begin
          for i := 0 to lSize-1 do
          begin
            // first read the slot name
            fStream.Read(lByte, 1);    // get 1 byte representing the size of the string slotname
            // if b is 255 or 254 or 253, then we are getting a 1 byte or 2 byte or 3 byte lookup code.
            // We are NOT going to support this logic because it was pretty much never used anyway and to get it fully right would be very difficult.  not worth the effort.
            if lByte>=253 then
            begin
              GenerateException(format( StrErrorReadingSlotNameSize, [lByte]));
            end
            else
            begin
              SetLength(lAnsiString, lByte);
              if lByte > 0 then
              begin
                fStream.Read(lAnsiString[1], lByte);
              end;
            end;

            Decode(aDataObj.AsFrame.NewSlot(String(lAnsiString)));     // create a new slot and recursively load it from the stream
          end;
        end;
      end;

      7{Array}: begin
        lNum := fStream.Read(lSize, 4);    // read 4 bytes representing the size of this array (# of slots)
        if lNum<>4 then GenerateException(StrInvalidNumberOfBytes8);
        if (lSize>0) then      // we need this if statement because if the array is zero in size then the lSize-1 below will go to -1 which can't be true given this is a cardinal.  It would raise an exception if this happened.
        begin
          for i:=0 to lSize-1 do
          begin
            Decode(aDataobj.AsArray.NewSlot);    // recursively read the slot
          end;
        end;
      end;

      9{byte}: begin
        lNum:=fStream.Read(lByte, 1);
        if lNum<>1 then GenerateException(StrInvalidNumberOfBytes9);
        aDataObj.AsByte := lByte;
      end;

      10{binary}: begin
        lNum:=fStream.Read(lSize, 4);    // get the size of the data
        if lNum<>4 then GenerateException(StrInvalidNumberOfBytes10);

        if lSize > 0 then
        begin
          lNum := aDataObj.AsBinary.CopyFrom(fStream, lSize);
          if lNum <> lSize then GenerateException(StrInvalidNumberOfBytes11);
        end;

        aDataObj.AsBinary.Seek(0, soFromBeginning);
      end;

      11{StringList}: begin
        lNum:=fStream.Read(lSize, 4);    // get the size of the data
        if lNum<>4 then GenerateException(StrInvalidNumberOfBytes12);

        lStringList := aDataObj.AsStringList;
        if lSize>0 then
        begin
          try
            lMemStream:=TMemoryStream.create;
            try
              lMemStream.CopyFrom(fStream, lSize);
              lMemStream.Seek(0,soBeginning);
              lStringList.LoadFromStream(lMemStream);
            finally
              lMemStream.Free;
            end;
          except
            GenerateException(StrErrorLoadingString);
          end;
        end;

      end;

      12{Geometry}: begin
        // So, these dataObjects don't support native geometry yet so we don't have a direct home to put this data into.
        // However, we still need to read it out of the stream in order to be able to read the other regular data that follows in the stream, so we will
        // read it out and put it into a Frame/array structure.  Note that we don't have anything in our code to recognize data in this structure yet so we
        // won't write out geometry in the DDO format, rather, it will just stream out as frames and arrays.

        lNum:=fStream.Read(lByte,1);    // get the geometry type
        if lNum<>1 then GenerateException(StrInvalidNumberOfBytes13);
        case lByte of
          0{Null Geometry}: begin
            // Nothing more to read.
          end;

          1{Point Geometry}:
          begin
            // We will translate the point geometry into a frame with an X and Y
            aDataObj.AsFrame.NewSlot('GeomType').AsSymbol := 'Point';
            lNum := fStream.Read(lDouble,8);   // read X
            if lNum<>8 then GenerateException(StrInvalidNumberOfBytes14);
            aDataObj.AsFrame.NewSlot('X').AsDouble := lDouble;
            lNum := fStream.Read(lDouble,8);   // read Y
            if lNum<>8 then GenerateException(StrInvalidNumberOfBytes14);
            aDataObj.AsFrame.NewSlot('Y').AsDouble := lDouble;
          end;

          3{Polyline Geometry}, 5{Polygon Geometry}:     // Both Polylines and Polygons read the same fundamental structure of binary data.
          begin
            fStream.read(lByte, 1);    // read the point flags value (bit coded)
            fStream.read(lSize, 4);    // read the part count
            if lByte = 3 then
              aDataObj.AsFrame.NewSlot('GeomType').AsSymbol := 'Polyline'
            else
              aDataObj.AsFrame.NewSlot('GeomType').AsSymbol := 'Polygon';

            aDataObj.AsFrame.NewSlot('Flags').AsByte := lByte;
            lPartArray := aDataObj.AsFrame.NewSlot('Parts').AsArray;
            if lSize>0 then             // need this check for zero to prevent the lSize-1 call below if lSize=0 because a range exception would fire.
            begin
              for i:=0 to lSize-1 do
              begin
                lFrame := lPartArray.NewSlot.AsFrame;

                fStream.read(lPointCount, 4);

                // read the flags for the individual part.
                // This is optional as previous versions of data object streaming would write a flags longInt for the polygon but not a flags for each part.
                // Also, if a data object is streamed out and none of the parts have individual flags (holes), then we won't stream the flags at all.
                lPartFlags := 0;
                if (lByte and $8{cGeomCapsHasHoles}) <> 0 then
                begin
                  // this polyline has the possibilities for holes so we need to read the flags for this part.
                  fStream.Read(lPartFlags, 1);
                  lFrame.NewSlot('Flags').AsByte := lPartFlags;
                end;

                lPointArray := lFrame.NewSlot('Points').AsArray;
                for j:=0 to lPointCount-1 do
                begin
                  lFrame := lPointArray.NewSlot.AsFrame;
                  lNum := fStream.Read(lDouble,8);   // read X
                  if( lNum <> 8 ) then GenerateException(StrInvalidNumberOfBytes14);
                  lFrame.NewSlot('X').AsDouble := lDouble;

                  lNum := fStream.Read(lDouble,8);   // read Y
                  if( lNum <> 8 ) then GenerateException(StrInvalidNumberOfBytes14);
                  lFrame.NewSlot('Y').AsDouble := lDouble;

                  if (lByte and $1) <> 0 then    // Z Capability
                  begin
                    // this polyline has a Z in the points
                    fStream.read(lDouble,8);
                    lFrame.NewSlot('Z').AsDouble := lDouble;
                  end;

                  if (lByte and $2) <> 0 then    // M Capability
                  begin
                    // this polyline has a M (Measure) in the points
                    fStream.read(lDouble,8);
                    lFrame.NewSlot('M').AsDouble := lDouble;
                  end;

                  if (lByte and $4) <> 0 then    // B (Bulge) Capability
                  begin
                    // this polyline has a B (Bulge) in the points
                    fStream.read(lDouble,8);
                    lFrame.NewSlot('B').AsDouble := lDouble;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;

      13: {Int64}
      begin
        lNum := fStream.Read( lInt64, 8 );  //int64 is 8 bytes
        if( lNum <> 8 ) then GenerateException(StrInvalidNumberOfBytes15);
        aDataObj.AsInt64 := lInt64;
      end;

    else  // if the data type was of an unknown type.  Read the appropriate amount
      begin // of data to get it out of the stream, and continue on.  This helps us for backward compatibility.
        lNum:=fStream.Read(lSize ,4);     // get the size of the data
        if lNum<>4 then GenerateException(StrInvalidNumberOfBytes16);
        fStream.Seek(lSize, soCurrent);   // skip past some of the data.
      end;
    end; // case
  except
    on e: Exception do
      GenerateException(e.Message);
  end;
end;

class function TDDOStreamer.Description: string;
begin
  result := 'DDO format.';
end;

initialization
  RegisterDataObjStreamer(TDDOStreamer);

end.
