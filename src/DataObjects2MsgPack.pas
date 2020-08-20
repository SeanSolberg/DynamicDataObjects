unit DataObjects2MsgPack;

interface

// FINISH - This whole unit needs to be finished,  just the basic framework has been put in here.

uses classes, DataObjects2, DataObjectsStreamers, SysUtils, RTTI, TypInfo, DataObjectsUtils;

type


  TMsgPackStreamer = class(TDataObjStreamerBase)
  private
    procedure WriteString(aString: String);

  public
    class function FileExtension: string; override;
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
  raise Exception.Create(aMessage+' when reading a MsgPack Stream at position='+intToStr(aStream.Position));
end;


class function TMsgPackStreamer.GetFileFilter: string;
begin
  result := 'MsgPack Files (*.msgPack)|*.msgPack';
end;

class function TMsgPackStreamer.IsFileExtension(aStr: string): boolean;
begin
  result := SameText(aStr, '.msgPack') or SameText(aStr, 'msgPack');
end;


procedure TMsgPackStreamer.WriteString(aString: String);
var
  lLen: Cardinal;
  lByte: Byte;
  lBuff: array[0..8] of byte;
  lUTF8String: UTF8String;
begin
  lUTF8String := UTF8String(aString);    // convert to UTF8String
  lLen := length(lUTF8String);
  if lLen <= 31 then
  begin
    lByte := $A0 + lLen;
    fStream.WriteBuffer(lByte, 1);
  end
  else if lLen <= 255 then
  begin
    lBuff[0] := $D9;
    lBuff[1] := lLen;
    fStream.WriteBuffer(lBuff, 2);
  end
  else if lLen <= 65535 then
  begin
    lBuff[0] := $DA;
    lBuff[1] := lLen shr 8;  //msb
    lBuff[2] := lLen;        //lsb
    fStream.WriteBuffer(lBuff, 3);
  end
  else
  begin
    lBuff[0] := $DB;
    lBuff[1] := lLen shr 24;  //msb
    lBuff[2] := lLen shr 16;
    lBuff[3] := lLen shr 8;
    lBuff[4] := lLen;         //lsb
    fStream.WriteBuffer(lBuff, 5);
  end;
  fStream.WriteBuffer(lUTF8String[1], lLen);
end;



procedure TMsgPackStreamer.Encode(aDataObj: TDataObj);
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
  lInt64: int64;
begin
  case aDataObj.DataType.Code of
    cDataTypeNull: begin
      lByte := $C0;
      fStream.Write(lByte,1);
    end;

    cDataTypeBoolean: begin
      if aDataObj.AsBoolean then
        lByte := $C3
      else
        lByte := $C2;
      fStream.Write(lByte,1);
    end;

    cDataTypeByte: begin
      lByte := aDataObj.AsByte;     //NOTE:  in the dataObject byte data type, it is always unsigned.  0..255.
      if lByte <= $7F then
      begin
        // value is from 0 to 127 so we can stream it as one byte.
        fStream.Write(lByte,1);
      end
      else
      begin
        lBytes.Code := $CC;
        lBytes.SetByteValue(lByte);
        fStream.Write(lBytes,2);
      end;
    end;

    cDataTypeInt32: begin              // we are treating the int32 as signed.
      lBytes.Code := $D2;
      lBytes.SetIntValue(aDataObj.AsInt32);
      fStream.Write(lBytes,5);
    end;

    cDataTypeInt64: begin
      lBytes.Code := $D3;
      lBytes.SetInt64Value(aDataObj.AsInt64);
      fStream.Write(lBytes,9);
    end;

    cDataTypeSingle: begin
      lBytes.Code := $CA;
      lBytes.SetSingleValue(aDataObj.AsSingle);
      fStream.Write(lBytes,5);
    end;

    cDataTypeDouble: begin
      lBytes.Code := $CB;
      lBytes.SetDoubleValue(aDataObj.AsDouble);
      fStream.Write(lBytes,9);
    end;

//    cDataTypeDecimal128: aStringBuilder.Append(lStore.dataInt64);

    cDataTypeDateTime, cDataTypeUTCDateTime, cDataTypeDate, cDataTypeTime: begin
      // MessagePack only has one type for date/time which is the "Timestamp Extension Type"
      // There are three variations
      // 0xd6  |   -1   |   seconds in 32-bit unsigned int  |
      // 0xd7  |   -1   |nanoseconds in 30-bit unsigned int | seconds in 34-bit unsigned int |
      // 0xc7  |   12   |   -1   |nanoseconds in 32-bit unsigned int | seconds in 64-bit signed int |

      lInt64 := aDataObj.AsUTCDateTime;
      // Need to figure out how to serialize this for real by choosing the best serialization option.

      // for now, we are just going to serialize the largest option because it handles the most cases
      // sometime in the future, I should choose the tightest option
      lBytes.Code := $C7;
      lBytes.SetBigTimestampValue(lInt64 div 1000,                // convert from milliSeconds to seconds for the main part.
                                  (lInt64 mod 1000) * 1000000);   // get the remaining milliseconds and convert to nanoseconds.
      fStream.Write(lBytes, 15);   //Size = $C7,12,-1,4bytes,8bytes
    end;

    cDataTypeGUID: begin
      WriteString(aDataObj.AsString);    // It would be nice to have a real extension for the GUID, but this will work for now.
    end;

    cDataTypeObjectID: begin
      WriteString(aDataObj.AsString);    // It would be nice to have a real extension for the ObjectID, but this will work for now.
    end;

    cDataTypeString: begin
      WriteString(aDataObj.AsString);    // both string types are serialized in MessagePack as UTF8
    end;

    cDataTypeStringList: begin
      // We are going to code the StringList data type as an array of Strings.
      lStringList := aDataObj.AsStringList;

      lSize := lStringList.Count;

      if lSize <= $0F then
      begin
        lByte := $90 + lSize;
        fStream.Write(lByte, 1);
      end
      else if lSize <= $FFFF then
      begin
        lBytes.Code := $DC;
        lBytes.SetUnsignedInt16Value(lSize);
        fStream.Write(lBytes, 3);
      end
      else
      begin
        lBytes.Code := $DD;
        lBytes.SetUnsignedIntValue(lSize);
        fStream.Write(lBytes, 5);
      end;

      for i := 0 to lStringList.Count-1 do
      begin
        WriteString(lStringList.Strings[i]);    // strings are serialized in MessagePack as UTF8
      end;
    end;

    cDataTypeFrame: begin
      lFrame := aDataObj.AsFrame;
      lSize := lFrame.Count;

      if lSize <= $0F then
      begin
        lByte := $80 + lSize;
        fStream.Write(lByte, 1);
      end
      else if lSize <= $FFFF then
      begin
        lBytes.Code := $DE;
        lBytes.SetUnsignedInt16Value(lSize);
        fStream.Write(lBytes, 3);
      end
      else
      begin
        lBytes.Code := $DF;
        lBytes.SetUnsignedIntValue(lSize);
        fStream.Write(lBytes, 5);
      end;

      for i := 0 to lFrame.Count-1 do
      begin
        // write out the name - value pair.
        WriteString(lFrame.Slotname(i));
        Encode(lFrame.Slots[i]);
      end;
    end;

    cDataTypeArray: begin
      lArray := aDataObj.AsArray;
      lSize := lArray.Count;

      if lSize <= $0F then
      begin
        lByte := $90 + lSize;
        fStream.Write(lByte, 1);
      end
      else if lSize <= $FFFF then
      begin
        lBytes.Code := $DC;
        lBytes.SetUnsignedInt16Value(lSize);
        fStream.Write(lBytes, 3);
      end
      else
      begin
        lBytes.Code := $DD;
        lBytes.SetUnsignedIntValue(lSize);
        fStream.Write(lBytes, 5);
      end;

      for i := 0 to lArray.Count-1 do
      begin
        Encode(lArray.Slots[i]);   // recursion happening here.
      end;
    end;

    cDataTypeSparseArray: begin
      lSparseArray := aDataObj.AsSparseArray;
      lSize := lSparseArray.Count;

      if lSize <= $0F then
      begin
        lByte := $80 + lSize;          // same as a map(TDataFrame) except that the keys are integers instead of strings
        fStream.Write(lByte, 1);
      end
      else if lSize <= $FFFF then
      begin
        lBytes.Code := $DE;
        lBytes.SetUnsignedInt16Value(lSize);
        fStream.Write(lBytes, 3);
      end
      else
      begin
        lBytes.Code := $DF;
        lBytes.SetUnsignedIntValue(lSize);
        fStream.Write(lBytes, 5);
      end;

      for i := 0 to lSparseArray.Count-1 do
      begin
        // First write out the 32Bit Integer Index Value.  FINISH - NOTE:  We could write out smaller sized values for small integers here.  We probably should.
        lBytes.Code := $D2;
        lBytes.SetIntValue(lSparseArray.SlotIndex(i));
        fStream.Write(lBytes,5);

        // Then write out the contained object.
        Encode(lSparseArray.Slots[i]);
      end;
    end;

    cDataTypeBinary: begin
      lBinary := aDataObj.AsBinary;
      lSize := lBinary.Size;  // NOTE:  MsgPack only support up to 32 bit length.  So, if it's actually more than that then we need a future error handling here.

      if lSize <= $FF then
      begin
        lBytes.Code := $C4;
        lBytes.SetByteValue(lSize);
        fStream.Write(lBytes, 2);
      end
      else if lSize <= $FFFF then
      begin
        lBytes.Code := $C5;
        lBytes.SetUnsignedInt16Value(lSize);
        fStream.Write(lBytes, 3);
      end
      else
      begin
        lBytes.Code := $C6;
        lBytes.SetUnsignedIntValue(lSize);
        fStream.Write(lBytes, 5);
      end;

      if lSize >0 then
      begin
        aDataObj.AsBinary.seek(0, soBeginning);
        fStream.CopyFrom(aDataObj.AsBinary, lSize);
      end;
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


class function TMsgPackStreamer.FileExtension: string;
begin
  result := 'msgpack';
end;

class function TMsgPackStreamer.ClipboardPriority: cardinal;
begin
  result := 30;
end;

procedure TMsgPackStreamer.Decode(aDataObj: TDataObj);
var
  lType: Byte;
  lByte: byte;          // unsigned
  lWord: word;          // unsigned
  lCardinal: cardinal;  //unsigned
  lUInt64: UInt64;      //unsigned

  lInt8: shortInt;      //signed
  lInt16: smallint;     //signed
  lInt32: integer;      //signed
  lInt64: int64;      //signed

  lDouble: double;
  lSingle: single;
  lString: UTF8String;
  i: Integer;

  lExtType: shortInt;
  lExtArray: TBytes;


  procedure ProcessExt(aExtType: shortInt; aExtArray: TBytes);
  var
    lSeconds: int64;
    lNanoSeconds: cardinal;
  begin
    // There a quite a few mechanisms that read an Ext DataType below, but once they all ready their data, they all call this method to then deal with recognizing that data and deciding what to do with it.
    // FINISH - right now we are just ignoring Ext data if we don't process it as a well-known extension type.
    // In the future, we could someone retain unknown extension data generically.
    case aExtType of
      -1: begin
        // TimeStamp Extension Type.  There are three formats for this:  32bit, 64bit and 96bit
        case length(aExtArray) of
          4: begin
            // timestamp 32 stores the number of seconds that have elapsed since 1970-01-01 00:00:00 UTC in a 32-bit unsigned integer:
            // Timestamp 32 format can represent a timestamp in [1970-01-01 00:00:00 UTC, 2106-02-07 06:28:16 UTC) range. Nanoseconds part is 0.

            //FINISH
          end;
          8: begin
            // timestamp 64 stores the number of seconds and nanoseconds that have elapsed since 1970-01-01 00:00:00 UTC in 32-bit unsigned integers:
            // nanoseconds in 30-bit unsigned int
            // seconds in 34-bit unsigned int
            // Timestamp 64 format can represent a timestamp in [1970-01-01 00:00:00.000000000 UTC, 2514-05-30 01:53:04.000000000 UTC) range.
            // In timestamp 64 format, nanoseconds must not be larger than 999999999.

            // FINISH
            // result.tv_nsec = data64 >> 34;
            // result.tv_sec = data64 & 0x00000003ffffffffL;
          end;
          12: begin
            // timestamp 96 stores the number of seconds and nanoseconds that have elapsed since 1970-01-01 00:00:00 UTC in 64-bit signed integer and 32-bit unsigned integer:
            // nanoseconds in 32-bit unsigned int
            // seconds in 64-bit signed int
            // Timestamp 96 format can represent a timestamp in [-584554047284-02-23 16:59:44 UTC, 584554051223-11-09 07:00:16.000000000 UTC) range.
            // In timestamp 96 format, nanoseconds must not be larger than 999999999.

            //FINISH
            lSeconds := SwapBytes( PInt64(@aExtArray[4])^);
            lNanoSeconds := SwapBytes( PCardinal(@aExtArray[0])^);

            aDataObj.AsUTCDateTime := round(lSeconds * 1000 + lNanoSeconds / 1000000);
          end;
          else begin
            RaiseParsingException(fStream,  format('Parsing extension type -1(Timestamp) is only allowed for 4, 8 & 12 bytes of data.  Data size received was %d bytes',[intToStr(length(aExtArray))]) );
          end;
        end;
      end;
    end;
  end;


  Procedure ParseMap(aCount: cardinal);                 // pass in the count for the number of slots that we will have in this map
  var
    i: cardinal;
    lKeyObj: TDataObj;
    lKeyString: string;
    lKeyValue: int64;
  begin
    //Finish - temporary code to force the maptype we are going to load to be a frame because the SparseArray isn't finished yet.
    aDataObj.AsFrame;

    for i := 0 to aCount-1 do
    begin
      // First decode the key.
      lKeyObj := TDataObj.Create;
      try
        self.Decode(lKeyObj);             //recursion happening here.

        // If the key is valid, then decode the value.  Right now, we are only allowing String keys which maps to our internal TDataFrame and numerical keys which maps to our internal TDataArray.
        // The first key we read determines our container type.  Note that if we have a frame container, then reading subsequent numerical keys will convert to a string key of that numerical key.
        // if the container is a sparseArray that can only have numerical keys, then reading a string for a key will try to convert to a numerical key, but note that could generate an exception cause it's not a valid numerical string.
        // if this happens, then the container will get converted to a frame container for all the data previously read and then this key can have a home in the converted to frame container.

        //  Note: Each time we call NewSlot below with the given key name, that slot could already exist from previous data being read.  If that happens, we will consider that an error condition here.  It could be in the future that a parsing preference could allow this.
        case aDataObj.DataType.Code of
          cDataTypeFrame: begin
            // already have a frame so put this new key in as a string if possible.
            if (lKeyObj.DataType.code = cDataTypeString) then
            begin
              lKeyString := lKeyObj.AsString;
              if lKeyString <> '' then
              begin
                self.Decode(aDataObj.AsFrame.NewSlot(lKeyString, true));           // recursion happening here.  Load the value for this map item tuple.
              end
              else
              begin
                RaiseParsingException(fStream, 'Read a string Key for a Map but the key was an empty string which is not allowed.');
              end;
            end
            else
            begin
              // the key we got wasn't directly a string, but if we can convert it to a valid string, we could possibly use that then.
              lKeyString := lKeyObj.AsString;
              if lKeyString <> '' then
              begin
                self.Decode(aDataObj.AsFrame.NewSlot(lKeyString, true));           // recursion happening here.  Load the value for this map item tuple.
              end
              else
              begin
                RaiseParsingException(fStream, 'Reading a Map we encountered an unusable Key with data type of '+lKeyObj.DataTypeString);
              end;
            end;
          end;

          cDataTypeSparseArray: begin
            if (lKeyObj.DataType.code = cDataTypeInt32) or (lKeyObj.DataType.Code = cDataTypeByte) then
            begin
              self.Decode(aDataObj.AsSparseArray.NewSlot(lKeyObj.AsInt32, true));           // recursion happening here.  Load the value for this map item tuple.
            end

            else if (lKeyObj.DataType.Code = cDataTypeInt64) then
            begin
              lKeyValue := lKeyObj.AsInt64;
              // It's possible that the key value is too big for use to use.
              if lKeyValue > MaxInt then
              begin
                RaiseParsingException(fStream, 'Key Value '+IntToStr(lKeyValue)+' is too large to fit in a sparse Array');      // FINISH - consider converting to a TDataFrame ?
              end
              else if lKeyValue < -2147483648 then
              begin
                RaiseParsingException(fStream, 'Key Value '+IntToStr(lKeyValue)+' is too large of a negative number to fit in a sparse Array');      // FINISH - consider converting to a TDataFrame ?
              end
              else
              begin
                self.Decode(aDataObj.AsSparseArray.NewSlot(lKeyValue, true));           // recursion happening here.  Load the value for this map item tuple.
              end;
            end

            else
            begin
              // the key we got wasn't directly an integer so we can't use this key in a sparse array.
              // However, if we can possibly treat this key as a string, then we could convert our container from a sparseArray to a Frame
              lKeyString := lKeyObj.AsString;
              if lKeyString <> '' then
              begin
                self.Decode(aDataObj.AsFrame.NewSlot(lKeyString, true));    // NOTE: the call to .AsFrame will convert aDataObj from a sparseArray to a Frame.  Recursion happening here.  Load the value for this map item tuple.
              end
              else
              begin
                RaiseParsingException(fStream, 'Reading a Map we encountered an unusable Key with data type of '+lKeyObj.DataTypeString);
              end;
            end;
          end;

          else
          begin
            // This aDataObj hasn't been set as either a frame or a SparseArray yet.
            // So, the first type of data we read will define which type of container we will start with.

            if (lKeyObj.DataType.code = cDataTypeString) then
            begin
              lKeyString := lKeyObj.AsString;
              if lKeyString <> '' then
              begin
                self.Decode(aDataObj.AsFrame.NewSlot(lKeyString, true));           // recursion happening here.  Load the value for this map item tuple.
              end
              else
              begin
                RaiseParsingException(fStream, 'Read a string Key for a Map but the key was an empty string which is not allowed.');
              end;
            end

            else if (lKeyObj.DataType.code = cDataTypeInt32) or (lKeyObj.DataType.Code = cDataTypeByte) then
            begin
              self.Decode(aDataObj.AsSparseArray.NewSlot(lKeyObj.AsInt32, true));           // recursion happening here.  Load the value for this map item tuple.  Set as a SparseArray.
            end

            else if (lKeyObj.DataType.Code = cDataTypeInt64) then
            begin
              lKeyValue := lKeyObj.AsInt64;
              // It's possible that the key value is too big for use to use.
              if lKeyValue > MaxInt then
              begin
                RaiseParsingException(fStream, 'Key Value '+IntToStr(lKeyValue)+' is too large to fit in a sparse Array');      // FINISH - consider converting to a TDataFrame ?
              end
              else if lKeyValue < -2147483648 then
              begin
                RaiseParsingException(fStream, 'Key Value '+IntToStr(lKeyValue)+' is too large of a negative number to fit in a sparse Array');      // FINISH - consider converting to a TDataFrame ?
              end
              else
              begin
                self.Decode(aDataObj.AsSparseArray.NewSlot(lKeyValue, true));           // recursion happening here.  Load the value for this map item tuple.  Set as a SparseArray.
              end;
            end

          end;
        end;
      finally
        lKeyObj.free;
      end;
    end;
  end;


begin
  //try
    fStream.Read(lType, 1);
    case lType of
      $0..$7f: begin   //positive fixInt   0xxxxxxx where the x's represent the number.
        aDataObj.AsByte := lType;
      end;

      $80..$8f: begin  //fixMap
        lByte := lType and $0F;          // count is stored in the first 4 bits of the previously read type.
        ParseMap(lByte);
      end;

      $90..$9f: begin  //fixArray
        lByte := lType and $0F;        // count is stored in the first 4 bits of the previously read type.
        for i := 0 to lByte-1 do
        begin
          self.Decode(aDataObj.AsArray.newSlot);   //recursion happening here
        end
      end;

      $A0..$bf: begin  //fixStr
        lByte := lType and $1F;     // the size is in the first 5 bits of the type that we read above.
        SetLength(lString, lByte);
        fStream.Read(lString[1], lByte);
        aDataObj.asString := string(lString);
      end;

      $c0: begin   //nil
        aDataObj.ClearData;   // sets to nil if not already set to nil.
      end;

      $c1: begin
        //NEVER USED - ERROR
      end;

      $c2: begin   //false
        aDataobj.AsBoolean := false;
      end;

      $c3: begin   //true
        aDataobj.AsBoolean := true;
      end;

      $c4: begin  //bin 8
        fStream.Read(lByte, 1);                          // read the size which is the number of bytes to read into a binary dataobj
        if lByte>0 then
          aDataObj.AsBinary.CopyFrom(fStream, lByte);
      end;

      $c5: begin  //bin 16
        fStream.Read(lWord, 2);                          // read the size which is the number of bytes to read into a binary dataobj
        lWord := SwapBytes(lWord);
        if lWord>0 then
        aDataObj.AsBinary.CopyFrom(fStream, lWord);
      end;

      $c6: begin  //bin 32
        fStream.Read(lCardinal, 4);                      // read the size which is the number of bytes to read into a binary dataobj
        lCardinal := SwapBytes(lCardinal);
        if lCardinal > 0 then
        aDataObj.AsBinary.CopyFrom(fStream, lCardinal);
      end;

      $c7: begin  //ext 8
        fStream.Read(lByte, 1);             // size
        fStream.Read(lExtType, 1);
        setLength(lExtArray, lByte);        // read the number of bytes defined by size
        fStream.Read(lExtArray[0], lByte);
        ProcessExt(lExtType, lExtArray);
      end;

      $c8: begin  //ext 16
        fStream.Read(lWord, 2);             // size
        lWord := SwapBytes(lWord);
        fStream.Read(lExtType, 1);
        setLength(lExtArray, lWord);        // read the number of bytes defined by size
        fStream.Read(lExtArray[0], lWord);
        ProcessExt(lExtType, lExtArray);
      end;

      $c9: begin  // ext 32
        fStream.Read(lCardinal, 4);             // size
        lCardinal := SwapBytes(lCardinal);
        fStream.Read(lExtType, 1);
        setLength(lExtArray, lCardinal);        // read the number of bytes defined by size.  Yes, this could potentially be huge.  FINISH-Do we need to read this in blocks someday.  maybe.
        fStream.Read(lExtArray[0], lCardinal);
        ProcessExt(lExtType, lExtArray);
      end;

      $ca: begin  // float 32
        fStream.Read(lSingle, 4);
        lSingle := SwapBytes(lSingle);
        aDataObj.AsSingle := lSingle;
      end;

      $cb: begin  // float 64
        fStream.Read(lDouble, 8);
        lDouble := SwapBytes(lDouble);
        aDataObj.AsDouble := lDouble;
      end;

      $cc: begin  // uInt 8
        fStream.Read(lByte, 1);
        aDataObj.AsByte := lByte;
      end;

      $cd: begin  // uInt 16
        fStream.Read(lWord, 2);
        lWord := SwapBytes(lWord);
        aDataObj.asInt32 := lWord;
      end;

      $ce: begin  // uInt 32
        fStream.Read(lCardinal, 4);
        lCardinal := SwapBytes(lCardinal);
        if lCardinal <= $7FFFFFFF then
          aDataObj.asInt32 := lCardinal    // we can fit this number into an integer
        else
          aDataObj.AsInt64 := lCardinal;      // need to convert to 64 bit integer cause it won't fit in the 31 bits of a positive 32 bit integer
      end;

      $cf: begin  // uInt 64
        fStream.Read(lUInt64, 8);
        lUInt64 := SwapBytes(lUInt64);
        if lUInt64 <= $7FFFFFFFFFFFFFFF then
          aDataObj.AsInt64 := lUInt64      // if the unsigned integer won't fit into the 64 signed integer space, then maybe this will generate an exception?  need to test this.
        else
          raise Exception.Create(IntToHex(lUInt64)+' is too large of an unsigned 64-bit integer to fit in a signed 64-bit integer type.');
      end;

      $d0: begin  // int 8
        fStream.Read(lInt8, 1);
        aDataObj.asInt32 := lInt8;
      end;

      $d1: begin  // int 16
        fStream.Read(lInt16, 2);
        lInt16 := SwapBytes(lInt16);
        aDataObj.asInt32 := lInt16;
      end;

      $d2: begin  // int 32
        fStream.Read(lInt32, 4);
        lInt32 := SwapBytes(lInt32);
        aDataObj.asInt32 := lInt32;
      end;

      $d3: begin  // int 64
        fStream.Read(lInt64, 8);
        lInt64 := SwapBytes(lInt64);
        aDataObj.asInt64 := lInt64;
      end;

      $d4: begin  // fixext 1
        fStream.Read(lExtType, 1);
        setLength(lExtArray, 1);            // we could be more efficient with this but all the Ext reading code is done in a consistent way.
        fStream.Read(lExtArray[0], 1);
        ProcessExt(lExtType, lExtArray);
      end;

      $d5: begin  // fixext 2
        fStream.Read(lExtType, 1);
        setLength(lExtArray, 2);
        fStream.Read(lExtArray[0], 2);
        ProcessExt(lExtType, lExtArray);
      end;

      $d6: begin  // fixext 4
        fStream.Read(lExtType, 1);
        setLength(lExtArray, 4);
        fStream.Read(lExtArray[0], 4);
        ProcessExt(lExtType, lExtArray);
      end;

      $d7: begin  //  fixext 8
        fStream.Read(lExtType, 1);
        setLength(lExtArray, 8);
        fStream.Read(lExtArray[0], 8);
        ProcessExt(lExtType, lExtArray);
      end;

      $d8: begin  //  fixext 16
        fStream.Read(lExtType, 1);
        setLength(lExtArray, 16);
        fStream.Read(lExtArray[0], 16);
        ProcessExt(lExtType, lExtArray);
      end;

      $d9: begin  //  str 8
        fStream.Read(lByte, 1);        //Size
        SetLength(lString, lByte);
        fStream.Read(lString[1],lByte);
        aDataObj.asString := string(lString);
      end;

      $da: begin  //  str 16
        fStream.Read(lWord, 2);        //Size
        lWord := SwapBytes(lWord);
        SetLength(lString, lWord);
        fStream.Read(lString[1],lWord);
        aDataObj.asString := string(lString);
      end;

      $db: begin  //  str32
        fStream.Read(lCardinal, 4);        //Size
        lCardinal := SwapBytes(lCardinal);
        SetLength(lString, lCardinal);     // NOTE:  this is going to be a very big string potentially
        fStream.Read(lString[1],lCardinal);
        aDataObj.asString := string(lString);
      end;

      $dc: begin  //  array 16
        fStream.Read(lWord, 2);        //Count of items in the array
        lWord := SwapBytes(lWord);
        for i := 0 to lWord-1 do
        begin
          self.Decode(aDataObj.AsArray.newSlot);   //recursion happening here
        end
      end;

      $dd: begin  //  array 32
        fStream.Read(lCardinal, 4);        //Count of items in the array
        lCardinal := SwapBytes(lCardinal);
        for i := 0 to lCardinal-1 do
        begin
          self.Decode(aDataObj.AsArray.newSlot);
        end
      end;

      $de: begin  //  map 16
        fStream.Read(lWord, 2);
        lWord := SwapBytes(lWord);
        ParseMap(lWord);
      end;

      $df: begin  //  map 32
        fStream.Read(lCardinal, 4);
        lCardinal := SwapBytes(lCardinal);
        ParseMap(lCardinal);
      end;

      $e0..$ff: begin  //negative fixInt.
        lInt32 := integer($FFFFFF00);
        aDataObj.AsInt32 := lInt32 or lType;    // 111xxxxx  as a signed negative number.
      end;
    end;

//  except
//    on e: exception do
//    begin
//      RaiseParsingException(fStream, e.Message);
//    end;
//  end;
end;



initialization
  RegisterDataObjStreamer(TMsgPackStreamer);

end.
