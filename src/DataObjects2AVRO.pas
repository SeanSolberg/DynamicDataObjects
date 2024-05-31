unit DataObjects2AVRO;

interface

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

// This unit will encode or decode AVRO serialization formats.
// The way that AVRO works is that the serialization of avro data MUST be paired with a schema in order to know how to interpret the
// bytes of the stream.  In an avro "file", the schema is stored once at the front of the file and then the rest of the file contains
// avro binary serializations.  This schema is stored in the JSON format.  Because of this, this code will also include the DataObjects2JSON
// streamer for serializing this JSON schema. This AVRO Streamer must also support the situation where the schema is loaded from a different
// source than what the AVRO Streamer is loading from or writing to.  SO, the stream we are working with may be a file, which should then be
// able to read/write the schema, or the stream may be some other kind of stream like a RPC network stream which is only sending avro binary
// streams and not schemas as the schema portion would have been shared previously.  So, this particular streamer needs to handle both
// use cases. When reading from a stream, if a schema has been assigned, then the stream is expected to just be reading avro objects, if the
// schema is not assigned, then it is expected to be reading an avro container file.

uses classes, DataObjects2, DataObjects2Streamers, SysUtils, RTTI, TypInfo, DataObjects2Utils, IOUtils, varInt, DataObjects2JSON;

type
  TAVRODataType = (dtUndefined,{primitiveTypes:}dtNull, dtBoolean, dtInt, dtLong, dtSingle, dtDouble, dtBinary, dtString, {ComplexTypes:}dtRecord, dtEnum, dtArray, dtMap, dtUnion, dtFixed);

  TAVROSchema = class  // Each schema object represents one data item.  Of course this data Item could be a map or array that contains more children.
  public
    DataType: TAVRODataType;
    Name: string;   // Note that Records, enums and Fixed types are Named Types.
    NameSpace: string;
    Doc: string;
    Children: array of TAVROSchema;
    Aliases: array of string;          // Usually not used.  However, this is a list of aliases that are alternate names for this schema type.
    Symbols: array of string;          // used for the enum type.  This is the list of string symbols that make up the definition of the enum.
    FixedSize: integer;                // number of bytes defined for a fixed size type.
    ChildSchema: TAVROSchema;          // If we have a container type such as a map or array, then this is the schema that will declare the type for those child objects.
    ChildSchemaCount: integer;         // number of child Schema items contained in Children

    constructor Create(aType: TAVRODataType);
    procedure BuildFromDataObj(aDataObj: TDataObj);
    procedure CopyToDataObj(aDataObj: TDataObj);
    function FullName: string;
    function AddChildSchema(aDataType: TAVRODataType): TAVROSchema;    // will create a child schema with the given dataType that is owned by this schema.
  end;

  TAVROStreamer = class(TDataObjStreamerBase)
  private
    fMetaData: TDataObj;
    fSchema: TAVROSchema;  // Must be set in order to Decode either by reading it from the AVRO Header within the metaData of an avro file stream, or it is setup before we initiate a decode.

    procedure WriteElement(aDataObj: TDataObj; aSlotName: string; aStream: TStream);
    procedure WriteFrame(aFrame: TDataFrame; aStream: TStream);
    procedure WriteArray(aArray: TDataArray; aStream: TStream);
    procedure WriteSparseArray(aSparseArray: TDataSparseArray; aStream: TStream);
    Procedure WriteStringListArray(aStrings: TStrings; aStream: TStream);

    function ReadInt64: int64;
    function ReadInt: integer;
    function ReadString: string;
    function ReadMap(aDataFrame: TDataFrame; aSchema: TAVROSchema): boolean;
    function ReadAvroHeader: boolean;  // returns true if successful

    function ReadCString: string;
    procedure ReadElement(aType: byte; aDataObj: TDataObj);
    procedure ReadDocument(aDataObj: TDataObj; aTreatAsArray: boolean = false);
    procedure DoRead(var Buffer; Count: LongInt);

    procedure RaiseParsingException(aMessage: string);
    function ReadBoolean: boolean;
    function ReadDouble: double;
    function ReadSingle: single;
    procedure ReadBinary(aMemStream: TDataBinary);
    function ReadRecord(aDataFrame: TDataFrame; aSchema: TAVROSchema): boolean;
    function ReadArray(aDataArray: TDataArray; aSchema: TAVROSchema): boolean;
    function ReadEnum(aSchema: TAVROSchema): string;
  public
    destructor Destroy; override;
    class function FileExtension: string; override;
    class function Description: string; override;
    class function Name: string; override;

    class function GetFileFilter: string; override;
    class function IsFileExtension(aStr: string): boolean; override;
    class function Priority: cardinal; override;

    procedure Decode(aDataObj: TDataObj); override;
    procedure Encode(aDataObj: TDataObj); override;
  end;

implementation

//resourceString

procedure TAVROStreamer.RaiseParsingException(aMessage: string);
begin
  raise Exception.Create(aMessage+' when reading an AVRO Stream at position='+intToStr(fStream.Position));
end;


class function TAVROStreamer.GetFileFilter: string;
begin
  result := 'AVRO Files (*.avro)|*.avro';
end;

class function TAVROStreamer.IsFileExtension(aStr: string): boolean;
begin
  result := SameText(aStr, '.avro') or SameText(aStr, 'avro');
end;


class function TAVROStreamer.Name: string;
begin
  result := 'Apache AVRO';
end;

class function TAVROStreamer.Priority: cardinal;
begin
  result := 40;
end;

procedure TAVROStreamer.Decode(aDataObj: TDataObj);
begin

end;

class function TAVROStreamer.Description: string;
begin
  result := 'Apache AVRO.  https://avro.apache.org/';
end;

destructor TAVROStreamer.Destroy;
begin
  FreeAndNil(fMetaData);
  inherited;
end;

procedure TAVROStreamer.DoRead(var Buffer; Count: LongInt);
begin
  if fStream.Read(Buffer, Count) <> Count then
  begin
    RaiseParsingException('Premature end of stream trying to read '+intToStr(Count)+' bytes');
  end;
end;


function TAVROStreamer.Readint64: int64;
var
  lVarInt: TVarInt64;
begin
  lVarInt.ReadFromStream(Stream);
  result := lVarInt;
end;

function TAVROStreamer.ReadSingle: single;
begin
  Stream.read(result,4);
end;

function TAVROStreamer.ReadDouble: double;
begin
  Stream.read(result,8);
end;

function TAVROStreamer.ReadBoolean: boolean;
var
  lByte: byte;
begin
  Stream.Read(lByte,1);
  result := lByte<>0;
end;

function TAVROStreamer.ReadInt: integer;
var
  lVarInt: TVarInt64;
  lInt64: int64;
begin
  lVarInt.ReadFromStream(Stream);
  lInt64 := lVarInt;
  if lInt64>high(integer) then
    RaiseParsingException('Schema expected to read an integer but the number was too large to fit in an integer size');
  if lInt64<low(integer) then
    RaiseParsingException('Schema expected to read an integer but the number was too large into the negative to fit in an integer size');
  result := lInt64;
end;

function TAVROStreamer.ReadString: string;
var
  lVarIntSize: TVarInt64;
  lSize: int64;
  lUTF8String: UTF8String;
  lByte: byte;
begin
  lVarIntSize.ReadFromStream(Stream);
  lSize := lVarIntsize;

  if (lSize > 0) then
  begin
    SetLength(lUTF8String, lSize);           // yes, could be zero.
    fStream.Read(lUTF8String[1], lSize);     // this does not read the null terminator byte from the stream.
  end;

  result := string(lUTF8String);    // convert to unicode string.
end;

procedure TAVROSTreamer.ReadBinary(aMemStream: TDataBinary);
var
  lVarIntSize: TVarint64;
begin
  lVarIntSize.ReadFromStream(Stream);
  aMemStream.CopyFrom(Stream, lVarIntSize);
end;

function TAVROStreamer.ReadRecord(aDataFrame: TDataFrame; aSchema: TAVROSchema): boolean;
begin
  //FINISH.
end;

function TAVROStreamer.ReadArray(aDataArray: TDataArray; aSchema: TAVROSchema): boolean;
begin
  //FINISH.
end;

function TAVROStreamer.ReadEnum(aSchema: TAVROSchema): string;
begin
  //FINISH.
end;


function TAVROStreamer.ReadMap(aDataFrame: TDataFrame; aSchema: TAVROSchema): boolean;
var
  lPairCount: int64;
  i: int64;
  lBlockSize: int64;
  lKey: string;
  lByte: byte;
begin
  // Maps are encoded as a series of blocks. Each block consists of a long count value, followed by that many key/value pairs.
  // A block with count zero indicates the end of the map. Each item is encoded per the map’s value schema.
  // Note that in an AVRO Map, each child item in the map MUST be the same data type as defined in the childType of schema.  Of course, that
  // childType could be another Map, Record or Union.  But, this behavior is different than a JSON map.  An Avro record is more equivalent to a JSON map.
  repeat
    lPairCount := ReadInt64;
    if lPairCount < 0 then
    begin
      //If a block’s count is negative, its absolute value is used, and the count is followed immediately by a long block size
      //indicating the number of bytes in the block.
      lPairCount := -lPairCount;
      lBlockSize := ReadInt64;   // need to get this off the stream, but not really going to use it because we don't have a feature to utilize skipping over a block.
    end;

    if lPairCount > 0 then
    begin
      for i := 1 to lPairCount do
      begin
        lKey := ReadString;

        // Now we need to decide which reader to call next based on the schema we have been given for this map.  It is assumed that the aSchema is already at least a map in order to be called
        // into this function in the first place.  But, in AVRO. Each map that is defined in a schema can only have one and only one type of data that is contained within the
        // elements of the map.  This is very different from other serializers (JSON, BSON, CBOR, pretty much all of them) where the fundamental definition of a map is that the data
        // item in a map element can be any of the possible data types supported by that serialization type.  Now, to get this type of dynamic type behavior, the schema would need to
        // define this map's child type to be a union.  Then, within that union schema item, there could be one or more different types of children allowed.
        case aSchema.ChildSchema.DataType of
          dtNull: begin aDataFrame.newSlot(lKey); end;  // nothing to do.  Doing nothing to aDataObj leaves it as null.
          dtBoolean: begin aDataFrame.newSlot(lKey).AsBoolean := ReadBoolean; end;
          dtInt: begin aDataFrame.newSlot(lKey).AsInt32 := ReadInt; end;
          dtLong: begin aDataFrame.newSlot(lKey).AsInt64 := ReadInt64; end;
          dtSingle: begin aDataFrame.newSlot(lKey).AsSingle := ReadSingle; end;
          dtDouble: begin aDataFrame.newSlot(lKey).AsDouble := ReadDouble; end;
          dtBinary: begin ReadBinary(aDataFrame.newSlot(lKey).AsBinary); end;
          dtString: begin aDataFrame.newSlot(lKey).AsString := ReadString; end;
          dtRecord: begin ReadRecord(aDataFrame.newSlot(lKey).AsFrame, aSchema.ChildSchema); end;
          dtEnum: begin aDataFrame.newSlot(lKey).AsSymbol := ReadEnum(aSchema.ChildSchema); end;
          dtArray: begin ReadArray(aDataFrame.newSlot(lKey).asArray, aSchema.ChildSchema); end;
          dtMap: begin ReadMap(aDataFrame.newSlot(lKey).AsFrame, aSchema.ChildSchema); end;
          dtUnion: begin {FINISH} end;
          dtFixed: begin {FINISH} end;
        end;
      end;
    end;

  until lPairCount = 0;
end;

function TAVROStreamer.ReadAvroHeader: boolean;
var
  lPreamble: array[0..3] of byte;
  lSyncMarker: array[0..15] of byte;
  lMetaSchema: TAVROSchema;
  lJsonStreamer: TJSONStreamer;
  lBinarySlot: TDataObj;
  lSchemaDataObj: TDataObj;
begin
  if Stream.Read(lPreamble,4)<>4 then
    RaiseParsingException('AVRO Stream Reading did not start with a proper AVRO preamble.');

  // Load the metadata next which includes the schema, but may include additional information.
  // this is written as a JSON payload that is contained within a structure as if it was serialized as {"type": "map", "values": "bytes"} which would be a Map
  lMetaSchema:=TAVROSchema.create(dtMap);
  try
    lMetaSchema.AddChildSchema(dtBinary);

    fMetaData := TDataObj.Create;
    ReadMap(fMetaData.AsFrame, lMetaSchema);

    // Once we have read the map that contains our top metaData, one of the items in that map should be the "avro.schema" which should contain the binary data that is the
    // JSON representation of the schema for the data items in this file.  So, parse this JSON schema using the JSONStreamer
    // Really, if we have a header, then we MUST have a schema in the header.  So, if we don't get this JSON parsed right or we don't get an avro.schema in the metadata, then we have an exception.
    if fMetaData.AsFrame.findSlot('avro.schema',lBinarySlot) then
    begin
      lJsonStreamer:=TJSONStreamer.create(lBinarySlot.AsBinary);
      try
        lSchemaDataObj:=TDataObj.Create;
        try
          lJsonStreamer.Decode(lSchemaDataObj);

          // Now that we decoded the JSON, turn the JSON into real Schema objects for use with the rest of the body parsing.
          fSchema := TAVROSchema.create(dtNull);

          fSchema.BuildFromDataObj(lSchemaDataObj);
        finally
          lSchemaDataObj.Free;
        end;

      finally
        lJsonStreamer.free;
      end;
    end
    else
    begin
      RaiseParsingException('Missing avro.schema from within the MetaData map of the avro header.');
    end;

  finally
    lMetaSchema.Free;
  end;

  if Stream.Read(lSyncMarker,16)<>16 then
    RaiseParsingException('AVRO Stream Reading could not read the 16byte sync marker from the header.');
end;

function TAVROStreamer.ReadCString: string;
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

procedure TAVROStreamer.ReadElement(aType: byte; aDataObj: TDataObj);
var
  lByte: byte;
  lInt64: int64;
  lUInt64: uInt64;
  lInt32: integer;
  lBuffer: TArray<byte>;
  lDouble: double;
  lDataFrame: TDataFrame;
  lBinary: TDataBinary;
begin
  // Now read the data content based on the type.
  case aType of
    $01: begin  //64-bit binary floating point (double)
      DoRead(lDouble, 8);
      aDataObj.AsDouble := lDouble;
    end;

    $02: begin  // UTF-8 String
      aDataObj.AsString := ReadString;
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
      lBinary := aDataObj.AsBinary;
      lBinary.SubTypeCode := lByte;
      if lInt32 > 0 then
        lBinary.CopyFrom(fStream, lInt32);
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
      aDataObj.AsBoolean := lByte <> 0;
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

       //FUTURE IMPROVEMENT - Maybe there is a CBOR tag for identifying RegExpressions that we can use here and then convert to a string tuple (IE) array of 2 elements.
      lDataFrame := aDataObj.AsFrame;
      lDataFrame.NewSlot('RegEx').AsString := ReadCString;  // read the regexPattern
      lDataFrame.NewSlot('Options').AsString := ReadCString;  // read the regex options string
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
      lDataFrame := aDataObj.AsFrame;
      lDataFrame.NewSlot('JavaScript').AsStringList.Text := ReadString;

      // The document is a mapping from identifiers to values, representing the scope in which the JavaScript string should be evaluated.
      ReadDocument(lDataFrame.NewSlot('Scope'));
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

procedure TAVROStreamer.ReadDocument(aDataObj: TDataObj; aTreatAsArray: boolean = false);
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
  dec(lSize, 5);    // The expected content size is going to be less 4 for the size Int32 and 1 for the trailing null terminator that we should expect.
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



Procedure TAVROStreamer.WriteFrame(aFrame: TDataFrame; aStream: TStream);
var
  i: integer;
  lMemStream: TMemoryStream;
  lSize: integer;    // the bson spec calls for this to be a signed integer.  I don't see why it should be signed instead of unsigned, but that's the spec.  whatever.
  lByte: byte;
  lStreamSize: integer;
begin
  lMemStream:=TMemoryStream.create;      // create a memory stream to serialize our contained document into because we need to learn the size as the size needs to be written first.
  try
    for i := 0 to aFrame.Count-1 do
    begin
      // Write out the dataType, then the slotname, then the value for each of our slots in this frame.
      WriteElement(aFrame.Slots[i], aFrame.Slotname(i), lMemStream);
    end;


    lStreamSize := lMemStream.Size;
    lSize := lStreamSize+4+1;     // Write the size of the embedded document, plus 4 bytes for the size Int32 and 1 byte for the null terminator streamed below.
    aStream.Write(lSize, 4);

    // Now write the embedded document data.
    lMemStream.Seek(0,soBeginning);
    aStream.CopyFrom(lMemStream, lStreamSize);

    // write null terminator for this document
    lByte := 0;
    aStream.Write(lByte, 1);
  finally
    lMemStream.Free;
  end;
end;

Procedure TAVROStreamer.WriteArray(aArray: TDataArray; aStream: TStream);
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

Procedure TAVROStreamer.WriteStringListArray(aStrings: TStrings; aStream: TStream);
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

Procedure TAVROStreamer.WriteSparseArray(aSparseArray: TDataSparseArray; aStream: TStream);
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

procedure TAVROStreamer.WriteElement(aDataObj: TDataObj; aSlotName: string; aStream: TStream);
var
  lStringList: TDataStringList;

  lByte: byte;
  lInt32: integer;
  lInt64: int64;
  lDouble: double;
  lUTF8String: UTF8String;
  lBinary: TDataBinary;

  procedure WriteSlotName;
  var
    lSlotName: UTF8String;
  begin
    lSlotName := UTF8String(aSlotName);      // convert from unicodeString to UTF8 String.
    if lSlotName = '' then
      raise exception.create('Cannot write to AVRO with an empty string as a slotname.');
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
      if lInt32 > 1 then
        aStream.Write(lUTF8String[1], lInt32)
      else
      begin
        lByte := 0;
        aStream.WriteData(lByte);    // we are writing an empty string which only contains the ending null byte.
      end;
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

      lBinary := aDataObj.AsBinary;
      // FINISH - Put in a oversize check.

      // Write the binary size.  NOTE: Only supports 32 bit sizes, and I suppose negative size is no good so really we are at the 2 gig limit here.
      lInt32 := lBinary.Size; // convert 64 bit int to 32bit int.
      aStream.Write(lInt32, 4);

      // Write the Subtype code
      lByte := lBinary.SubTypeCode;             // $00=Generic binary subtype, $01=Function, $02=Binary (Old), $03=UUID (Old), $04=UUID, $05=MD5, $06=Encrypted BSON, $07=Compressed BSON column, $80=User defined
      aStream.Write(lByte,1);

      // Write the binary data.
      lBinary.Seek(0, soBeginning);
      if lInt32>0 then
        aStream.CopyFrom(lBinary, lInt32);
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




procedure TAVROStreamer.Encode(aDataObj: TDataObj);
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
      RaiseParsingException('Only a top level container (Frame, Array or SparseArray) can be serialized to AVRO');
    end;

  end;
end;


class function TAVROStreamer.FileExtension: string;
begin
  result := 'avro';
end;

{ TAVROSchema }

function TAVROSchema.AddChildSchema(aDataType: TAVRODataType): TAVROSchema;
var
  lLen: integer;
begin
  lLen := Length(Children);
  if ChildSchemaCount >= Length(Children) then
  begin
    SetLength(Children, lLen);
    Children[ChildSchemaCount] := TAVROSchema.Create(aDataType);
    inc(ChildSchemaCount);
  end;
end;

procedure TAVROSchema.BuildFromDataObj(aDataObj: TDataObj);
var
  lTypeSlot: TDataObj;
  lType: TAVRODataType;

  procedure BadType;
  begin
    raise exception.Create('When loading schema, the schema definition encountered a bad type.  Only String, Map and Array can define AVRO types.');
  end;

  function TypeFromString(aStr: string): TAVRODataType;
  var
    lLogicalType: string;
  begin
    // Strings are used to define primitive types.
    if aStr = 'null' then
    begin
      result := dtNull;
    end
    else if aStr = 'boolean' then
    begin
      result := dtBoolean;
    end
    else if aStr = 'int' then
    begin
      result := dtInt;
    end
    else if aStr = 'long' then
    begin
      result := dtLong;
    end
    else if aStr = 'float' then
    begin
      result := dtSingle;
    end
    else if aStr = 'double' then
    begin
      result := dtDouble;
    end
    else if aStr = 'bytes' then
    begin
      result := dtBinary;
    end
    else if aStr = 'string' then
    begin
      result := dtString;
    end
    else if aStr = 'record' then
    begin
      result := dtRecord;
    end
    else if aStr = 'enum' then
    begin
      result := dtEnum;
    end
    else if aStr = 'array' then
    begin
      result := dtArray
    end
    // Union is excluded here because a union is declared by an array of types.  There will never be a type with the name union.
    else if aStr = 'map' then
    begin
      result := dtMap
    end
    else if aStr = 'fixed' then
    begin
      result := dtFixed
    end
    else
    begin
      result := dtUndefined;
    end;
  end;

  procedure TypeFromFrame(aDataFrame: TDataFrame);
  var
    lType: TAvroDataType;
    lLogicalTypeSlot: TDataObj;
  begin
    if aDataFrame.FindSlot('type', lTypeSlot) then
    begin
      lType := TypeFromString(lTypeSlot.AsString);

      // See if we have a LogicalType.

      // For some types, we may need to be picking up more attributes.
      if lType = dtUndefined then
      begin
        // the type we got was not a well known type. so, it is considered a "logical" type that MUST have a "logicalType" field in this frame
        if aDataFrame.FindSlot('logicalType', lLogicalTypeSlot) then
        begin
          lType := TypeFromString(lLogicalTypeSlot.AsString);

          if lType = dtUndefined then
          begin
            raise Exception.Create('Received an unknown type of '+lLogicalTypeSlot.AsString+' for a logicalType attribute.');
          end;
          self.DataType := lType;
//          self.LogicalType := lTypeSlot.AsString;

        end
        else
        begin
          raise Exception.Create('Received an unknown type of '+lTypeSlot.AsString+' but and associated logicalType was not included with it.');
        end;
      end
      else
      begin
        self.DataType := lType;
      end;

    end
    else
    begin
      Exception.Create('Type slot does not exist within Schema JSON.');
    end;
  end;

  procedure TypeFromArray(aDataArray: TDataArray);
  begin
    // an array is going to give us a union.  which is useful for declaring different types that may be within a map.
  end;

begin
  // See the AVRO Schema JSON definition at https://avro.apache.org/docs/1.11.1/specification/#schema-declaration
  // Note that from the JSON that parsed into aDataObj, the only types that can be used for scheme declaration are as follows:
  //   string->for primitive types,
  //   Object(map or frame)-> for complex types,
  //   array -> for unions of embedded types.
  case aDataObj.DataType.Code of
    cDataTypeString: self.DataType := TypeFromString(aDataObj.AsString);
    cDataTypeFrame: TypeFromFrame(aDataObj.AsFrame);  // Will end up being recursive.
    cDataTypeArray: TypeFromArray(aDataObj.AsArray);  // Will end up being recursive.
  else
    BadType();
  end;
end;

procedure TAVROSchema.CopyToDataObj(aDataObj: TDataObj);
begin

end;

constructor TAVROSchema.Create(aType: TAVRODataType);
begin
  inherited Create;
  DataType := aType;
end;

function TAVROSchema.FullName: string;
var
  lDotPos: integer;
begin
(* The fullname of a record, enum or fixed definition is determined by the required name and optional namespace attributes like this:

   A fullname is specified. If the name specified contains a dot, then it is assumed to be a fullname, and any namespace also specified is ignored.
   For example, use “name”: “org.foo.X” to indicate the fullname org.foo.X.

   A simple name (a name that contains no dots) and namespace are both specified.
   For example, one might use “name”: “X”, “namespace”: “org.foo” to indicate the fullname org.foo.X.

   A simple name only is specified (a name that contains no dots). In this case the namespace is taken from the most tightly enclosing named schema
   or protocol, and the fullname is constructed from that namespace and the name. For example, if “name”: “X” is specified, and this occurs within a
   field of the record definition of org.foo.Y, then the fullname is org.foo.X. This also happens if there is no enclosing namespace (i.e., the enclosing
   schema definition has the null namespace).  *)

  lDotPos := pos('.',Name);
  if lDotPos > 0 then
  begin
    result := NameSpace+'.'+Name;
  end
  else
  begin
    if namespace <> '' then
    begin
      result := NameSpace+'.'+Name;
    end
    else
    begin
      // FINISH - need to walk the parent to get the upper namespace
      result := Name;
    end;
  end;
end;

initialization
  RegisterDataObjStreamer(TAVROStreamer);

end.
