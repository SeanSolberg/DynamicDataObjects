unit DataObjects2BSON;

interface

// This unit has been coded for almost all decode and encode operations.  However, very little has been tested.

uses classes, DataObjects2, DataObjects2Streamers, SysUtils, RTTI, TypInfo, DataObjects2Utils, IOUtils;

type
  TBSONStreamer = class(TDataObjStreamerBase)
  private
    procedure WriteElement(aDataObj: TDataObj; aSlotName: string; aStream: TStream);
    procedure WriteFrame(aFrame: TDataFrame; aStream: TStream);
    procedure WriteArray(aArray: TDataArray; aStream: TStream);
    procedure WriteSparseArray(aSparseArray: TDataSparseArray; aStream: TStream);
    Procedure WriteStringListArray(aStrings: TStrings; aStream: TStream);

    function ReadString: string;
    function ReadCString: string;
    procedure ReadElement(aType: byte; aDataObj: TDataObj);
    procedure ReadDocument(aDataObj: TDataObj; aTreatAsArray: boolean = false);
    procedure DoRead(var Buffer; Count: LongInt);

    procedure RaiseParsingException(aMessage: string);
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

procedure TBSONStreamer.RaiseParsingException(aMessage: string);
begin
  raise Exception.Create(aMessage+' when reading a BSON Stream at position='+intToStr(fStream.Position));
end;


class function TBSONStreamer.GetFileFilter: string;
begin
  result := 'BSON Files (*.bson)|*.bson';
end;

class function TBSONStreamer.IsFileExtension(aStr: string): boolean;
begin
  result := SameText(aStr, '.bson') or SameText(aStr, 'bson');
end;


class function TBSONStreamer.ClipboardPriority: cardinal;
begin
  result := 40;
end;

procedure TBSONStreamer.Decode(aDataObj: TDataObj);
begin
  ReadDocument(aDataObj);
end;

procedure TBSONStreamer.DoRead(var Buffer; Count: LongInt);
begin
  if fStream.Read(Buffer, Count) <> Count then
  begin
    RaiseParsingException('Premature end of stream trying to read '+intToStr(Count)+' bytes');
  end;
end;

function TBSONStreamer.ReadString: string;
var
  lSize: Cardinal;
  lUTF8String: UTF8String;
  lByte: byte;
begin
  // Read the UTF-8 String from the stream and return it as a unicode String.
  DoRead(lSize, 4);
  SetLength(lUTF8String, lSize-1);
  fStream.Read(lUTF8String[1], lSize-1);     // this should also read the null terminator byte from the stream.

  // Read the null terminating byte.
  DoRead(lByte, 1);

  if lByte <> 0 then
  begin
    RaiseParsingException('Expected a NULL byte at the end of reading a String but read $'+IntToHex(lByte, 2)+' instead');
  end;

  result := string(lUTF8String);    // convert to unicode string.
end;


(*  Sample code (that doesn't work) that will give a pattern for a faster implementation.  Finish this someday.
function TBSONStreamer.ReadCString: string;
const
  cBuffMax = 1000;
var
  tempChar: AnsiChar;
  tempChars: array[0..cBuffMax] of AnsiChar;
  index: Integer;
  lOutputStrTotal: String;
begin
  // Read the cString type from the stream which is a null terminated string.
  result := '';
  index := 0;
  while (read(tempChar,1)>0) and (tempChar<>#10) do
  begin
    if tempChar<>#13 then
    begin
      tempChars[index]:=tempChar;
      inc(index);
    end;
    if index = cBuffMax then   // our buffer is full so save what we have so far and start over with the buffer.
    begin
      lOutputStrTotal := lOutputStrTotal + StrPas(tempChars);
      FillChar(tempChars,cBuffMax+1,#0);
      index:=0;
    end;
  end;
  tempChars[index]:=#0;
  result:=lOutputStrTotal + StrPas(tempChars);
end;   *)



function TBSONStreamer.ReadCString: string;
var
  lMS: TMemoryStream;
  lByte: byte;
  lPtr: TPtrWrapper;
begin
  // to read a CString, we must first read all the bytes from the stream up to and including a null (0x00) terminator.
  // then, the bytes we read can be put into a UTF8 converter
  lMS:=TMemoryStream.Create;
  try
    repeat
      if Stream.read(lByte,1)<>1 then break;
      if lByte<>0 then
      begin
        lMS.Write(lByte,1);       // maybe not that efficient to do 1 byte at a time.
      end;
    until lByte = 0;

    lPtr.Create(lMS.memory);
    result := TMarshal.ReadStringAsUtf8(lPtr, lMS.Size);
  finally
    lMS.Free;
  end;
end;

procedure TBSONStreamer.ReadElement(aType: byte; aDataObj: TDataObj);
var
  lByte: byte;
  lInt64: int64;
  lUInt64: uInt64;
  lInt32: integer;
  lBuffer: TArray<byte>;
  lDouble: double;
begin
  // Now read the data content based on the type.
  case aType of
    $01: begin  //64-bit binary floating point (double)
      DoRead(lDouble, 8);
      aDataObj.AsDouble := lDouble;
    end;

    $02: begin  // UTF-8 String
      aDataObj.AsSymbol := ReadString;
    end;

    $03: begin  //Embedded Document
      ReadDocument( aDataObj, false);    //recursion happening here.
    end;

    $04: begin  //Array
      ReadDocument( aDataObj, true);    //recursion happening here.
    end;

    $05: begin  //Binary Data
      // read the size of the binary data
      DoRead(lInt32, 4);

      // read the SubType
      DoRead(lByte, 1);

      // now read the binary data.
      if lInt32 > 0 then
        aDataObj.AsBinary.CopyFrom(fStream, lInt32)
      else
        aDataObj.AsBinary;    // just to get the type set.
    end;

    $06: begin  //Undefined value (Depricated)
      // Nothing to read.  Will result in a Null DataObject.
      aDataObj.Clear;
    end;

    $07: begin  //ObjectID
      DoRead(aDataObj.AsObjectID.Data[0], 12);
    end;

    $08: begin  //Boolean
      DoRead(lByte, 1);
      if lByte <> 0 then
        aDataObj.AsBoolean := true
      else
        aDataObj.AsBoolean := false;
    end;

    $09: begin  //UTC DateTime
      DoRead(lInt64,8);
      aDataObj.AsUTCDateTime := lInt64;
    end;

    $0A: begin  //Null
      aDataObj.Clear;
    end;

    $0B: begin  //Regular Expression
      {The first cstring is the regex pattern, the second is the regex options string. Options are identified by characters, which must be stored in alphabetical
       order. Valid options are 'i' for case insensitive matching, 'm' for multiline matching, 'x' for verbose mode, 'l' to make \w, \W, etc. locale dependent,
       's' for dotall mode ('.' matches everything), and 'u' to make \w, \W, etc. match unicode.}

      aDataObj.AsFrame.NewSlot('RegEx').AsString := ReadCString;  // read the regexPattern
      aDataObj.AsFrame.NewSlot('Options').AsString := ReadCString;  // read the regex options string
    end;

    $0C: begin  //DB Pointer (Depricated)
      //Ignoring DB Pointers.  However, we do need to read the bytes from the stream.
      //FINISH - maybe someday we can put a tag on this type for the binary data to give it meaning
      ReadString;

      SetLength(lBuffer, 12);
      try
        DoRead(lBuffer, 12);
        aDataObj.AsBinary.Write(lBuffer, 12);
      finally
        SetLength(lBuffer, 0);
      end;
    end;

    $0D: begin  //JavaScript Code
      //FINISH - should we have either an attribute or a tag on this JavaScript code Import to give it meaning?
      aDataObj.AsStringList.Text := ReadString;
    end;

    $0E: begin  //Symbol (Depricated)
      aDataObj.AsSymbol := ReadString;
    end;

    $0F: begin  //JavaScript Code w/Scope
      DoRead(lInt32, 4);   //The int32 is the length in bytes of the entire code_w_s value.   We are not going to make use of this value.

      // The string is JavaScript code.
      aDataObj.AsFrame.NewSlot('JavaScript').AsStringList.Text := ReadString;

      // The document is a mapping from identifiers to values, representing the scope in which the JavaScript string should be evaluated.
      ReadDocument(aDataObj.AsFrame.NewSlot('Scope'));
    end;

    $10: begin  //32 bit int
      DoRead(lInt32, 4);
      aDataObj.AsInt64 := lInt32;
    end;

    $11: begin  //Timestamp
      DoRead(lUInt64, 8);
      aDataObj.AsInt64 := lUInt64;    //FINISH - We are putting this unsigned integer into a signed variable.  The bits will be preserved, but should we be tagging this to give it the proper meaning.
    end;

    $12: begin  //64-bit int
      DoRead(lInt64, 8);
      aDataObj.AsInt64 := lInt64;
    end;

    $13: begin  //Decimal
      //FINISH - we are reading the bytes out of the stream but we still need to internally implement the Decimal data type.
      SetLength(lBuffer, 16);
      DoRead(lbuffer, 16);
    end;

    $7F: begin  //MaxKey.  Nothing to read
    end;

    $FF: begin  //MinKey.  Nothing to read
    end;

    else
    begin
      RaiseParsingException('Read an invalid data type byte of '+IntToHex(aType, 2));
    end;
  end;
end;

procedure TBSONStreamer.ReadDocument(aDataObj: TDataObj; aTreatAsArray: boolean = false);
var
  lType: byte;
  lSize: integer;
  lStartPos: int64;
  lByte: byte;
  lSlotName: string;
begin
  // NOTE:  if ATreatAsArray is true.
  // An array document is just like a regular document except that the expectation is that each slotname MUST be a string that can be converted to an integer and each slotName
  // must be sequential starting from 0 to n. If we do not get a valid integer string or it's not sequential, then we have an error condition.


  // The initial read from a BSON stream must be a Document which starts out with the total document size, followed by the list of elements, and then finally with an end terminator.
  // Note that the lSize includes the 4 bytes of the size, the document data and the last null byte.
  DoRead(lSize, 4);

  // Now go in a loop and read the elements until we consume up the size we were given.
  dec(lSize, 5);    // 4 for the size and 1 for the null terminator.
  lStartPos := fStream.Position;
  while fStream.Position-lStartPos < lSize do
  begin
    fStream.Read(lType, 1);

    // Read the Element Name which in our terms is the SlotName
    lSlotName := ReadCString;

    // If aTreatAsArray is true then we need to check that the lSlotName we just read truly is the next array index.
    if aTreatAsArray then
    begin
      ReadElement(lType, aDataObj.AsArray.NewSlot);
    end
    else
    begin
      ReadElement(lType, aDataObj.AsFrame.NewSlot(lSlotName));
    end;
  end;

  // The next byte MUST be a NULL byte to finish up reading a document so let's read it and check it.
  DoRead(lByte, 1);

  if lByte <> $0 then
  begin
    RaiseParsingException('Expected a NULL byte at the end of reading a document but read $'+IntToHex(lByte, 2)+' instead');
  end;
end;



Procedure TBSONStreamer.WriteFrame(aFrame: TDataFrame; aStream: TStream);
var
  i: integer;
  lMemStream: TMemoryStream;
  lSize: integer;    // the bson spec calls for this to be a signed integer.  I don't see why it should be signed instead of unsigned, but that's the spec.  whatever.
  lByte: byte;
begin
  lMemStream:=TMemoryStream.create;      // create a memory stream to serialize our contained document into because we need to learn the size as the size needs to be written first.
  try
    for i := 0 to aFrame.Count-1 do
    begin
      // Write out the dataType, then the slotname, then the value for each of our slots in this frame.
      WriteElement(aFrame.Slots[i], aFrame.Slotname(i), lMemStream);
    end;

    // Write the size of the embedded document.
    lSize := lMemStream.Size;
    aStream.Write(lSize, 4);

    // Now write the embedded document data.
    lMemStream.Seek(0,soBeginning);
    aStream.CopyFrom(lMemStream, lSize);

    // write null terminator for this document
    lByte := 0;
    aStream.Write(lByte, 1);
  finally
    lMemStream.Free;
  end;
end;

Procedure TBSONStreamer.WriteArray(aArray: TDataArray; aStream: TStream);
var
  i: integer;
  lMemStream: TMemoryStream;
  lSize: integer;    // the bson spec calls for this to be a signed integer.  I don't see why it should be signed instead of unsigned, but that's the spec.  whatever.
  lByte: byte;
begin
  lMemStream:=TMemoryStream.create;      // create a memory stream to serialize our contained document into because we need to learn the size as the size needs to be written first.
  try
    for i := 0 to aArray.Count-1 do
    begin
      // Write out the dataType, then the slotname, then the value for each of our slots in this frame.
      WriteElement(aArray.Slots[i], IntToStr(i), lMemStream);
    end;

    // Write the size of the embedded document.
    lSize := lMemStream.Size;
    aStream.Write(lSize, 4);

    // Now write the embedded document data.
    lMemStream.Seek(0,soBeginning);
    aStream.CopyFrom(lMemStream, lSize);

    // write null terminator for this document
    lByte := 0;
    aStream.Write(lByte, 1);
  finally
    lMemStream.Free;
  end;
end;

Procedure TBSONStreamer.WriteStringListArray(aStrings: TStrings; aStream: TStream);
var
  i: integer;
  lMemStream: TMemoryStream;
  lSize: integer;    // the bson spec calls for this to be a signed integer.  I don't see why it should be signed instead of unsigned, but that's the spec.  whatever.
  lByte: byte;
  lSlotName: UTF8String;
  lUTF8String: UTF8String;
begin
  lMemStream:=TMemoryStream.create;      // create a memory stream to serialize our contained document into because we need to learn the size as the size needs to be written first.
  try
    for i := 0 to aStrings.Count-1 do
    begin
      // Write out the dataType, then the slotname, then the value for each of our strings
      lByte := $2;             // String type code
      lMemStream.Write(lByte,1);

      // Write the Slotname which for an array is the index of the slot as a string.
      lSlotName := UTF8String(IntToStr(i));      // convert from unicodeString to UTF8 String.
      lMemStream.Write(lSlotName[1], length(lSlotName)+1);   // +1 to include the null terminator

      // write the size of the string;
      lUTF8String := UTF8String(aStrings[i]);     // get the string and convert it to a UTF8String.
      lSize := length(lUTF8String) + 1;           // the +1 is for the null byte at the end.
      lMemStream.Write(lSize, 4);

      // Now write the string bytes including the null terminator byte
      lMemStream.Write(lUTF8String[1], lSize);
    end;

    // Write the size of the embedded document.
    lSize := lMemStream.Size;
    aStream.Write(lSize, 4);

    // Now write the embedded document data.
    lMemStream.Seek(0,soBeginning);
    aStream.CopyFrom(lMemStream, lSize);

    // write null terminator for this document
    lByte := 0;
    aStream.Write(lByte, 1);
  finally
    lMemStream.Free;
  end;
end;

Procedure TBSONStreamer.WriteSparseArray(aSparseArray: TDataSparseArray; aStream: TStream);
var
  i: integer;
  lMemStream: TMemoryStream;
  lSize: integer;    // the bson spec calls for this to be a signed integer.  I don't see why it should be signed instead of unsigned, but that's the spec.  whatever.
  lByte: byte;
begin
  lMemStream:=TMemoryStream.create;      // create a memory stream to serialize our contained document into because we need to learn the size as the size needs to be written first.
  try
    for i := 0 to aSparseArray.Count-1 do
    begin
      // Write out the dataType, then the slotname, then the value for each of our slots in this frame.
      WriteElement(aSparseArray.Slots[i], IntToStr(aSparseArray.SlotIndex(i)), lMemStream);
    end;

    // Write the size of the embedded document.
    lSize := lMemStream.Size;
    aStream.Write(lSize, 4);

    // Now write the embedded document data.
    lMemStream.Seek(0,soBeginning);
    aStream.CopyFrom(lMemStream, lSize);

    // write null terminator for this document
    lByte := 0;
    aStream.Write(lByte, 1);
  finally
    lMemStream.Free;
  end;
end;

procedure TBSONStreamer.WriteElement(aDataObj: TDataObj; aSlotName: string; aStream: TStream);
var
  lStringList: TDataStringList;

  lByte: byte;
  lInt32: integer;
  lInt64: int64;
  lDouble: double;
  lUTF8String: UTF8String;

  procedure WriteSlotName;
  var
    lSlotName: UTF8String;
  begin
    lSlotName := UTF8String(aSlotName);      // convert from unicodeString to UTF8 String.
    if lSlotName = '' then
      raise exception.create('Cannot write to BSON with an empty string as a slotname.');
    aStream.Write(lSlotName[1], length(lSlotName)+1);   // +1 to include the null terminator
  end;

begin
  case aDataObj.DataType.Code of
    cDataTypeNull: begin
      lByte := $0A;            //Null Code
      aStream.Write(lByte,1);
      WriteSlotName;
    end;

    cDataTypeBoolean: begin
      lByte := $08;            // Boolean type code
      aStream.Write(lByte,1);
      WriteSlotName;
      if aDataObj.AsBoolean then
        lByte := $01            // True
      else
        lByte := $00;            // False
      aStream.Write(lByte,1);
    end;

    cDataTypeByte, cDataTypeInt32: begin              // we are treating the int32 as signed.
      lByte := $10;             // int32 type code
      aStream.Write(lByte,1);
      WriteSlotname;
      lInt32 := aDataObj.AsInt32;
      aStream.Write(lInt32,4);
    end;

    cDataTypeInt64: begin
      lByte := $12;             // int64 type code
      aStream.Write(lByte,1);
      WriteSlotname;
      lInt64 := aDataObj.AsInt64;
      aStream.Write(lInt64,8);
    end;

    cDataTypeSingle, cDataTypeDouble: begin
      // BSON doesn't have a single float so we need to serialize as a double float.
      lByte := $01;             // double type code
      aStream.Write(lByte,1);
      WriteSlotname;
      lDouble := aDataObj.AsDouble;
      aStream.Write(lDouble,8);
    end;

    cDataTypeDecimal128: begin
(*        lByte := $13;             // Decimal128 type code.  FINISH - yeah, we don't really support this yet.
      aStream.Write(lByte,1);
      WriteSlotname;
      lDecimal := aDataObj.AsDecimal128
      aStream.Write(lDecimal, 16); *)
    end;

    // for the unix UTC time do we publish in string notation or in the int64 notation.    I say in64 notation since that's the purpose of this data type
    cDataTypeDateTime, cDataTypeUTCDateTime, cDataTypeDate, cDataTypeTime: begin
      lByte := $9;             // UTC DateTime type code
      aStream.Write(lByte,1);

      WriteSlotname;

      lInt64 := aDataobj.AsUTCDateTime;
      aStream.Write(lInt64, 8);
    end;

    cDataTypeGUID: begin
      // The GUID data type is serialized in BSON as binary data with a special subType code.
      lByte := $05;             // Binary type code
      aStream.Write(lByte,1);

      WriteSlotname;

      // Write the binary size which is 16 bytes for a UUID.
      lInt32 := 16;
      aStream.Write(lInt32, 4);

      // Write the Subtype code
      lByte := $04;             // $00=Generic binary subtype, $01=Function, $02=Binary (Old), $03=UUID (Old), $04=UUID, $05=MD5, $80=User defined
      aStream.Write(lByte,1);

      // Write the binary data.
      aStream.Write(aDataObj.AsGUID.GUID, 16);
    end;

    cDataTypeObjectID: begin
      lByte := $7;             // ObjectID type code
      aStream.Write(lByte,1);
      WriteSlotname;
      aStream.Write(aDataObj.AsObjectID.Data, 12);
    end;

    cDataTypeString: begin
      lByte := $2;             // String type code
      aStream.Write(lByte,1);

      WriteSlotname;

      // write the size of the string;
      lUTF8String := UTF8String(aDataObj.AsString);     // convert to UTF8String
      lInt32 := length(lUTF8String) + 1;   // the +1 is for the null byte at the end.
      aStream.Write(lInt32, 4);

      // Now write the string bytes including the null terminator byte
      aStream.Write(lUTF8String[1], lInt32);
    end;

    cDataTypeStringList: begin
      // We are going to code the StringList data type as an array of Strings.
      lStringList := aDataObj.AsStringList;

      lByte := $4;             // embedded Array Document type code
      aStream.Write(lByte,1);

      WriteSlotname;

      WriteStringListArray(lStringList, aStream);
    end;

    cDataTypeFrame: begin
      lByte := $3;             // embedded Document type code
      aStream.Write(lByte,1);

      WriteSlotname;

      WriteFrame(aDataObj.AsFrame, aStream);
    end;

    cDataTypeArray: begin
      //NOTE:  Array - The document for an array is a normal BSON document with integer values for the keys, starting with 0 and continuing sequentially.
      // For example, the array ['red', 'blue'] would be encoded as the document {'0': 'red', '1': 'blue'}. The keys must be in ascending numerical order.
      lByte := $4;             // embedded Array Document type code
      aStream.Write(lByte,1);

      WriteSlotname;

      WriteArray(aDataObj.AsArray, aStream);
    end;

    cDataTypeSparseArray: begin
      //NOTE:  Array - The document for an array is a normal BSON document with integer values for the keys, starting with 0 and continuing sequentially.
      // For example, the array ['red', 'blue'] would be encoded as the document {'0': 'red', '1': 'blue'}.
      // Note that for the BSON array, we must have sequential keys and in ascending order.  the purpose of the sparse array is to not follow that rule, so
      // we must encode the sparse Array as an embedded BSON document with the slotIndexes as string keys in the document
      lByte := $3;             // embedded Document type code
      aStream.Write(lByte,1);

      WriteSlotname;

      WriteSparseArray(aDataObj.AsSparseArray, aStream);
    end;

    cDataTypeBinary: begin
      lByte := $05;             // Binary type code
      aStream.Write(lByte,1);

      WriteSlotname;

      // FINISH - Put in a oversize check.

      // Write the binary size.  NOTE: Only supports 32 bit sizes, and I suppose negative size is no good so really we are at the 2 gig limit here.
      lInt32 := aDataObj.AsBinary.Size;
      aStream.Write(lInt32, 4);

      // Write the Subtype code
      lByte := $00;             // $00=Generic binary subtype, $01=Function, $02=Binary (Old), $03=UUID (Old), $04=UUID, $05=MD5, $80=User defined
      aStream.Write(lByte,1);

      // Write the binary data.
      aDataObj.AsBinary.Seek(0, soBeginning);
      if lInt32>0 then
        aStream.CopyFrom(aDataObj.AsBinary, lInt32);

    end;

    cDataTypeObject: begin
      //Finish
    end;

    cDataTypeTag: begin
      // FINISH - found that other implementations have an option to put tags into JSON by containing the tag using a Json Object (frame) with a certain slotName naming convention.
      //          maybe we should support this concept too.
      // see https://github.com/intel/tinycbor/commit/782f2545a07e707464c6e9b417768e8b980c8e13

      // For now, we are skipping over the tagging portion and just streaming out the contained DataObject so call recursively
      WriteElement(aDataObj.AsTag.DataObj, aSlotName, aStream);
    end;
  end;
end;




procedure TBSONStreamer.Encode(aDataObj: TDataObj);
begin
  // NOTE:  in BSON, only a top level document (Frame, Array or SparseArray in our case) can be serialized out directly.
  // The atomic types can not be serialized out except as an element within a document.
  // It's a shortcoming with BSON, but I get it--why serialize just an atomic value.
  case aDataObj.DataType.Code of

    cDataTypeFrame: begin
      WriteFrame(aDataObj.AsFrame, fStream);
    end;

    cDataTypeArray: begin
      WriteArray(aDataObj.AsArray, fStream);
    end;

    cDataTypeSparseArray: begin
      WriteSparseArray(aDataObj.AsSparseArray, fStream);
    end;

    cDataTypeObject: begin
      //Finish - we could put this directly to a frame (document).
    end;

    else
    begin
      // Any other type is an error condition, so raise some kind of error I guess.
      RaiseParsingException('Only a top level container (Frame, Array or SparseArray) can be serialized to BSON');
    end;

  end;
end;


class function TBSONStreamer.FileExtension: string;
begin
  result := 'bson';
end;

initialization
  RegisterDataObjStreamer(TBSONStreamer);

end.
