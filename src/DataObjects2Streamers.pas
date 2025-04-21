unit DataObjects2Streamers;

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

uses SysUtils, DataObjects2, classes, Generics.collections, VarInt, Generics.Defaults,
  StringBTree;

// The following constants set the default serialization properties for the TDataObjStreamer
const
  cDefaultUseSlotnameRefs = true;
  cDefaultUseStringRefs = true;
  cDefaultSerializeAsUTF8 = true;

type
  (* NOTE:  FINISH - one thing we could do is to register which core dataObject types are directly supported by each Streamer descendant class.
            Then the dataObject editor could possibly give some hints in the user interface that the users should avoid adding slots of data of certain types
            if the user is editing data that came from a certain file type. *)


  // This class registry will hold all registered TDataObjStreamerBase descendants that are added to the host application using this code library.
  // Each streamer included in a project should add itself to this registry (see gStreamerRegistry below) in its initialization section.
  TStreamerRegistry = class(TList<TDataObjStreamerClass>)
  public
    procedure Sort;
    function FindStreamerClassByFilenameExtension(aExtension: string): TDataObjStreamerClass;
    function FindStreamerClassByFilename(aFilename: string): TDataObjStreamerClass;
    function FindStreamerClassByClassname(aClassname: string): TDataObjStreamerClass;
    function CreateStreamerByFilenameExtension(aExtension: string): TDataObjStreamerBase;
    function CreateStreamerByFilename(aFilename: string): TDataObjStreamerBase;
    function AllStreamersFileDialogFilters: string;
  end;

  TStreamerRegistryComparer = class(TComparer<TDataObjStreamerClass>)
  public
    function Compare(const Left, Right: TDataObjStreamerClass): Integer; override;
  end;


  // This is the native DataObject streaming that directly fully supports reading and writing all data in the TDataObjects object model.
  // This format registers the "*.dataObj" file extension.
  TDataObjStreamer = class(TDataObjStreamerBase)
  private
    fUseStringRefs: boolean;
    fUseSlotnameRefs: boolean;
    fSerializeAsUTF8: boolean;
    fReadSlotNameRefs: TStringList;          // Only used when reading from a stream
    fReadStringRefs: TStringList;            // Only used when reading from a stream
    fWriteSlotNameRefs: TStringBinaryTree;   // Only used when writing to a stream if this option is turned on.
    fWriteStringRefs: TStringBinaryTree;     // Only used when writing to a stream if this option is turned on.

  strict private
    procedure EncodeInternal(aDataObj: TDataObj);
    procedure DecodeInternal(aDataobj: TDataObj);
  public
    class function FileExtension: string; override;
    class function Description: string; override;
    class function Name: string; override;
    class function GetFileFilter: string; override;
    class function IsFileExtension(aStr: string): boolean; override;
    class function Priority: Cardinal; override;


//    class function ClipboardPriority: cardinal; override;

    class procedure GetParameterInfo(aParameterPurpose: TDataObjParameterPurposes; aStrings: TStrings); override;
    procedure ApplyOptionalParameters(aParams: TStrings); override;

    constructor Create(aStream: TStream); override;
    destructor Destroy; override;
    function Clone: TDataObjStreamerBase; override;

    procedure Decode(aDataObj: TDataObj); override;
    procedure Encode(aDataobj: TDataObj); override;

    // If this property is turned on, then the encoding to a stream will send an ID of a string for any string that is repeat of one that has already been sent.
    // This can reduce the size of the stream greatly if the data is likely to have repetitive string values, but at a little bit of a hit to performance.
    property UseStringRefs: boolean read fUseStringRefs write fUseStringRefs;

    // If this property is turned on, then the encoding to a stream will send an ID of a slotname for any slotnames that are a repeat of one that has already been sent.
    // This can reduce the size of the stream greatly if the data is likely to have repetitive slotnames such as in a typical array of frames situation.
    // This comes at a little bit of a hit to processing performance, but can save greatly on size and thus improve performance downstream.
    property UseSlotnameRefs: boolean read fUseSlotnameRefs write fUseSlotnameRefs;

    // If this property is turned on, then strings in String slots or strings in StringList slots will be serialized as UTF8.
    // In general, UTF8 strings are about half the size of an equivalent unicode string, but additional processing is needed to convert the unicode string inside
    // the data objects into the UTF8 strings for serialization. If the dataObject has a lot of string content then this can cut the size of the payload almost in half.
    property SerializeAsUTF8: boolean read fSerializeAsUTF8 write fSerializeAsUTF8;

    // Thes properties are here just so we can pull these two statistics from this streamer after a dataObject has been serialized.
    function StringRefWriteCount: integer;
    function SlotnameRefWriteCount: integer;
  end;


  TSlotNameIDorSize = record   // This is a serializer to write the SlotName Size or ID using variable-sized numerical encoding.
  private
    IsID: boolean;             // if True, then the value is an ID, if false, the value is a Size.
    Value: Cardinal;
  public
    procedure WriteToStream(aStream: TStream);
    procedure ReadFromStream(aStream: TStream);
    procedure SetAsSize(aSize: Cardinal);
    procedure SetAsID(aID: Cardinal);
  end;


var
  gStreamerRegistry: TStreamerRegistry;

procedure RegisterDataObjStreamer(aStreamer: TDataObjStreamerClass);

ResourceString
  strUnableToReadDataObject = 'Unable to read DataObject(%s) from stream at offset = %d';
  strSlotNameOrSizeTooLarge =  'Slot name size or ID too large to serialize.';
  strReadInvalidDataType = 'Read an invalid data type of %s at offset = %d';

implementation

procedure TSlotNameIDorSize.ReadFromStream(aStream: TStream);
var
  lByte: byte;
  lValue: Cardinal;
begin
  IsID := false;
  if aStream.Read(lByte, 1) = 0 then
    raise EVarIntException.Create(strUnableToReadByte);

  if (lByte and $80) <> 0 then
  begin
    IsID := true; // The flag is set to signal a String Ref ID.
  end;
  Value := lByte and $3F;    // take our first 6 bits as the beginning of the value.

  if (lByte and $40) <> 0 then   // Check if the "more bytes follow" flag is set.
  begin
    if aStream.Read(lByte, 1) = 0 then
      raise EVarIntException.Create(strUnableToReadByte);
    lValue := (lByte and $7F);     // take 7 bits into value into the right place.
    lValue := lValue shl 6;
    Value := Value or lValue;

    if (lByte and $80) <> 0 then   // Check if the "more bytes follow" flag is set.
    begin
      if aStream.Read(lByte, 1) = 0 then
        raise EVarIntException.Create(strUnableToReadByte);
      lValue := (lByte and $7F);     // take 7 bits into value into the right place.
      lValue := lValue shl 13;
      Value := Value or lValue;

      if (lByte and $80) <> 0 then   // Check if the "more bytes follow" flag is set.
      begin
        if aStream.Read(lByte, 1) = 0 then
          raise EVarIntException.Create(strUnableToReadByte);
        lValue := lByte shl 20;     // take 8 bits into value into the right place.
        Value := Value or lValue;
      end;
    end;
  end;

end;

procedure TSlotNameIDorSize.SetAsID(aID: Cardinal);
begin
  self.Value := aID;
  self.IsID := true;
end;

procedure TSlotNameIDorSize.SetAsSize(aSize: Cardinal);
begin
  self.Value := aSize;
  self.IsID := false;
end;

procedure TSlotNameIDorSize.WriteToStream(aStream: TStream);
var
  lBuffer: array[0..5] of byte;
  lValue: Cardinal;
  lCount: byte;
begin
  // We are serializing either a Size or an ID.  The MSB bit is true on the first byte sent if it is an ID.
  // The 2nd MSB bit is used for the "more bytes to follow" flag within the first byte being sent.
  // The MSB bit is used for the "more bytes to follow" on all subsequent bytes sent.
  // SO, sizes and IDs that are 63 or less will fit in one byte.  The vast majority of slotnames will be under this size.
  // I would think a size above 16K (14bits) would never really happen.  I guess it's possible an ID above 16K could happen, but should be extremely rare.
  // we will support a total of 4 Bytes to transmit.  The 4th byte CANNOT have a "more bits to follow" flag so it can support 8 bits of value.
  // There is a size or ID limit of 6+7+7+8 = 28 bits = 268,435,455 which is HUGE and beyond reason.

  lValue := Value;
  if lValue > $07FFFFFF then
    raise EDataObj.Create(strSlotNameOrSizeTooLarge);

  lCount := 1;
  lBuffer[0] := lValue and $3F;        // bring in first 6 bits.
  if IsID then
    lBuffer[0] := lBuffer[0] or $80;   // set MSB for ID flag.

  if lValue > $3F then
  begin
    lBuffer[0] := lBuffer[0] or $40;   // set the "More to follow" flag.

    lValue := lValue shr 6;            // shift to the next block of bits in the number
    lBuffer[1] := lValue and $7F;      // bring in the next 7 bits
    lCount := 2;

    if lValue > $7F then
    begin
      lBuffer[1] := lBuffer[1] or $80;   // set the "More to follow" flag.

      lValue := lValue shr 7;            // shift to the next block of bits in the number
      lBuffer[2] := lValue and $7F;      // bring in the next 7 bits
      lCount := 3;

      if lValue > $7F then
      begin
        lBuffer[2] := lBuffer[2] or $80;   // set the "More to follow" flag.

        lValue := lValue shr 7;            // shift to the next block of bits in the number
        lBuffer[3] := lValue and $FF;      // bring in the last 8 bits
        lCount := 4;
      end;
    end;
  end;

  aStream.write(lBuffer,lCount);
end;


{ TDataObjStreamer }

(*class function TDataObjStreamer.ClipboardPriority: cardinal;
begin
  result := 1;    // highest priority
end;  *)

function TDataObjStreamer.Clone: TDataObjStreamerBase;
begin
  result := TDataObjStreamer.Create(nil);
  TDataObjStreamer(result).UseStringRefs := self.UseStringRefs;
  TDataObjStreamer(result).UseSlotnameRefs := self.UseSlotnameRefs;
  TDataObjStreamer(result).SerializeAsUTF8 := self.SerializeAsUTF8;
end;

constructor TDataObjStreamer.Create(aStream: TStream);
begin
  inherited;
  fUseStringRefs := cDefaultUseStringRefs;
  fUseSlotnameRefs := cDefaultUseSlotnameRefs;
  fSerializeAsUTF8 := cDefaultSerializeAsUTF8;
end;

procedure TDataObjStreamer.Decode(aDataobj: TDataObj);
begin
  if not assigned(fReadSlotNameRefs) then
    fReadSlotNameRefs := TStringList.Create
  else
    fReadSlotNameRefs.Clear;       // If we still have a StringRefs filled in with strings from a previous use of this streamer, we must clear it to start over.
  if not assigned(fReadStringRefs) then
    fReadStringRefs := TStringList.Create
  else
    fReadStringRefs.Clear;       // If we still have a StringRefs filled in with strings from a previous use of this streamer, we must clear it to start over.

  DecodeInternal(aDataObj);
end;

procedure TDataObjStreamer.DecodeInternal(aDataobj: TDataObj);
var
  i: Integer;
  lCount: integer;
  lIndex: int64;
  lReadCount: Integer;
  lStore: PTDataStore;
  lDataObj: TDataObj;
  lValue: byte;
  lStringIndex: TUVarInt64;
  lLen: integer;
  lSubClass: byte;

  procedure DoException;
  begin
    aDataObj.DataType.Code:=cDataTypeNull;
    raise EDataObj.Create(format(strUnableToReadDataObject, [aDataObj.DataTypeString,fStream.Position]));
  end;

  // Read a Unsigned UVarInt from the stream.
  function ReadUVarInt: UInt64;
  var
    lVarInt: TUVarInt64;
  begin
    lVarInt.ReadFromStream(fStream);
    result := lVarInt;
  end;

  // Read a Unsigned UVarInt from the stream.
  function ReadVarInt: Int64;
  var
    lVarInt: TVarInt64;
  begin
    lVarInt.ReadFromStream(fStream);
    result := lVarInt;
  end;

  function ReadUTF8String: string;
  var
    lVarIntSize: TUVarInt64;
    lSize: integer;
    lBytes: TBytes;
  begin
    lVarIntSize := ReadUVarInt;
    lSize := lVarIntSize;                                    // convert from varInt to integer;
    SetLength(lBytes, lSize);                                // allocate buffer to read the Unicode data into from the stream
    try
      if fStream.read(lBytes, lSize) <> lSize then
        DoException;

      result := TEncoding.UTF8.GetString(lBytes);            // performs the decoding from UTF8 to string.
    finally
      SetLength(lBytes, 0);                                  // Free what we allocated
    end;
  end;

  function ReadUnicodeString: string;
  var
    lVarIntSize: TUVarInt64;
    lSize: integer;
    lBytes: TBytes;
  begin
    lVarIntSize := ReadUVarInt;
    lSize := lVarIntSize;                                    // convert from varInt to integer;
    SetLength(lBytes, lSize);                                // allocate buffer to read the Unicode data into from the stream
    try
      if fStream.read(lBytes, lSize) <> lSize then
        DoException;

      result := TEncoding.Unicode.GetString(lBytes);         // Get the raw bytes brought in as a Unicode String
    finally
      SetLength(lBytes, 0);                                  // Free what we allocated
    end;
  end;

  // Read a string sequence from the stream specifically as a SlotName
  function ReadSlotName: string;
  var
    lIDorSize: TSlotNameIDorSize;
    lIndex: integer;
    lSize: integer;
    lBytes: TBytes;
  begin
    // Reading a slotname in a frame is serialized as either a UTF8String OR it is an ID of a previously read slotname string.
    lIDorSize.ReadFromStream(fStream);
    if lIDorSize.IsID then
    begin
      lIndex := lIDorSize.Value;
      result := fReadSlotNameRefs.Strings[lIndex];
    end
    else
    begin
      lSize := lIDorSize.Value;
      SetLength(lBytes, lSize);                                // allocate buffer to read the Unicode data into from the stream
      try
        fStream.read(lBytes, lSize);
        result := TEncoding.UTF8.GetString(lBytes);            // performs the decoding from UTF8 to string.
        // This string needs to be saved in our refList in case a subsequent read of a slotname refers to this string by ID.
        fReadSlotNameRefs.Add(result);
      finally
        SetLength(lBytes, 0);                                  // Free what we allocated
      end;
    end;
  end;

begin
  lStore := aDataObj.getStore;

  fStream.Read(lValue, 1);

  lSubClass := (lValue shr 5) and 3;   // pull the subclass out to preserve it.
  lValue := lValue and $9F;            // strip the subclass off for casing below.

  // NOTE: If lValue has MSB set, then we are receiving a special code that is a serialization variation of one of the other datatypes.
  // We are going to set the DataType here now onto the dataObj, but only if it's not a special code.  In either case, we must still
  // set the subClass.  So in the case of a special code, we will set the dataType to NULL with the subClass here, but down below where
  // special codes are handled, the DataType Code must get set.
  if lValue<$80 then
  begin
    aDataObj.SetDataTypeParts(TDataTypeCode(lValue and $1F), // 5  bits for the datatype code}
                             lSubClass);
  end
  else
  begin
    aDataObj.SetDataTypeParts(TDataTypeCode(0), // 5  bits for the datatype code - using NULL for now}
                             lSubClass);
  end;


  case lValue of
    ord(cDataTypeNull): begin end;

    ord(cDataTypeBoolean): begin end;

    ord(cDataTypeByte): begin
      lReadCount := fStream.Read(lStore.fDataByte, 1);
      if lReadCount <> 1 then DoException;
    end;

    ord(cDataTypeInt32): begin
      lReadCount := fStream.Read(lStore.fDataInt32, 4);
      if lReadCount <> 4 then DoException;
    end;

    ord(cDataTypeInt64): begin
      lReadCount := fStream.Read(lStore.fDataInt64, 8);
      if lReadCount <> 8 then DoException;
    end;

    ord(cDataTypeSingle): begin
      lReadCount := fStream.Read(lStore.fDataSingle, 4);
      if lReadCount <> 4 then DoException;
    end;

    ord(cDataTypeDouble): begin
      lReadCount := fStream.Read(lStore.fDataDouble, 8);
      if lReadCount <> 8 then DoException;
    end;

//    cDataTypeDecimal128: begin end;  not implemented yet.

    ord(cDataTypeDateTime): begin
      lReadCount := fStream.Read(lStore.fDataDateTime, 8);
      if lReadCount <> 8 then DoException;
    end;

    ord(cDataTypeUTCDateTime): begin
      lReadCount := fStream.Read(lStore.fDataInt64, 8);
      if lReadCount <> 8 then DoException;
    end;

    ord(cDataTypeDate): begin
      lReadCount := fStream.Read(lStore.fDataDateTime, 8);
      if lReadCount <> 8 then DoException;
    end;

    ord(cDataTypeTime): begin
      lReadCount := fStream.Read(lStore.fDataDateTime, 8);
      if lReadCount <> 8 then DoException;
    end;

    ord(cDataTypeGUID): begin
      lStore.dataGUID := TDataGUID.Create;
      lReadCount := fStream.ReadData(@lStore.dataGUID.GUID, 16);
      if lReadCount <> 16 then DoException;
    end;

    ord(cDataTypeObjectID): begin
      lStore.dataObjectID := TDataObjectID.Create;
      lReadCount := fStream.Read(TDataObjectID(lStore.dataObjectID).Data, 12);
      if lReadCount <> 12 then DoException;
    end;

      // $80 is for Attributes
      // $81 is for serializing a string as UTF8
      // $82 is for serializing a string as a StringRef ID.
      // $83 is for serializing a stringList as UTF8
      // SubTypeCodes need to be preserved during this situation.  SubType 01 represents a string "SYMBOL" and 10 and 11 are reserved for future use.

    ord(cDataTypeString): begin
      //we are reading a unicode string.
      lStore.fDataString := ReadUnicodeString;
      lLen := length(lStore.fDataString);
      if (lLen>0) then              // we do not include empty strings as referenceable strings.
        fReadStringRefs.Add(lStore.fDataString);      // save this string in case a future string read just gives us a reference to it.
    end;

    $81: begin
      // we are reading a UTF8 String;
      lStore.fDataString := ReadUTF8String;
      lLen := length(lStore.fDataString);
      aDataObj.SetDataTypeParts(cDataTypeString, lSubClass);  // when handling special codes, we must finalize the dataObject with it's right dataType code

      if (lLen>0) then              // we do not include empty strings as referenceable strings.
        fReadStringRefs.Add(lStore.fDataString);      // save this string in case a future string read just gives us a reference to it.
    end;

    $82: begin
      // we are reading a string ID that came in from a previous read of a unicode string or a UTF8 string.
      lStringIndex.ReadFromStream(fStream);
      aDataObj.SetDataTypeParts(cDataTypeString, lSubClass);  // when handling special codes, we must finalize the dataObject with it's right dataType code
      lStore.fDataString := fReadStringRefs.Strings[lStringIndex];     // if the index is out of range, then we will get an exception.
    end;

    ord(cDataTypeStringList): begin
      // we are reading Unicode Strings into a StringList.
      lCount := ReadUVarInt;
      lStore.dataStringList := TDataStringList.Create;
      for i := 0 to lCount-1 do
        TDataStringList(lStore.dataStringList).Add(ReadUnicodeString);
    end;

    $83: begin
      // we are reading UTF8 Strings into a StringList;
      lCount := ReadUVarInt;
      lStore.dataStringList := TDataStringList.Create;
      aDataObj.SetDataTypeParts(cDataTypeStringList, lSubClass);  // when handling special codes, we must finalize the dataObject with it's right dataType code
      for i := 0 to lCount-1 do
        TDataStringList(lStore.dataStringList).Add(ReadUTF8String);
    end;

    ord(cDataTypeFrame): begin
      lCount := ReadUVarInt;
      lStore.dataFrame := TDataFrame.Create;
      for i := 0 to lCount-1 do
      begin
        DecodeInternal(lStore.dataFrame.NewSlot(ReadSlotName));     // Note:  Recursion happening here.  TODO. in the future, we should limit how far deep we can nest.
      end;
    end;

    ord(cDataTypeArray): begin
      lCount := ReadUVarInt;
      lStore.dataArray := TDataArray.Create;
      for i := 0 to lCount-1 do
      begin
        lDataObj := TDataObj.Create;
        try
          DecodeInternal(lDataObj);     // Note:  Recursion happening here.  TODO. in the future, we should limit how far deep we can nest.
        except
          lDataObj.Free;
          raise;
        end;
        lStore.dataArray.Add(lDataObj);
      end;
    end;

    ord(cDataTypeSparseArray): begin
      lCount := ReadUVarInt;
      lStore.dataSparseArray := TDataSparseArray.create;
      for i := 0 to lCount-1 do
      begin
        lIndex := ReadVarInt;
        lDataObj := TDataObj.Create;
        try
          DecodeInternal(lDataObj);     // Note:  Recursion happening here.  TODO. in the future, we should limit how far deep we can nest.
        except
          lDataObj.Free;
        end;
        lStore.dataSparseArray.AppendSlot(lIndex, lDataObj);
      end;
    end;

    ord(cDataTypeBinary): begin
      lCount := ReadUVarInt;   // read the binary size.
      lStore.dataBinary := TDataBinary.Create;
      try
        if lCount > 0 then
        begin
          lStore.dataBinary.CopyFrom(fStream, lCount);
          lStore.dataBinary.seek(0, soBeginning);
        end;
      except
        lStore.dataBinary.Free;  // if the loading of the binary data failed, then we won't take any of it.
        DoException;
      end;
    end;

    ord(cDataTypeObject): begin
      // Using the RTTI from this object, we are going to serialize the same as a TDataFrame with a special flag set that defines that the content was serialized from a real TObject descendant.

      // FINISH.

    end;

    ord(cDataTypeTag): begin
      lStore.dataTag := TDataTag.Create;            //FINISH - THIS IS NOT TESTED.
      lStore.dataTag.TagValue := ReadUVarInt;
      DecodeInternal(lStore.dataTag.DataObj);          // recursion happening here.
    end;

(*    $80: begin
      // Read Attributes
    end; *)

  else
    begin
      // FINISH - need to generate a good exception here because we received a dataType that's not valid.
      raise EDataObj.Create(format(strReadInvalidDataType, [IntToHex(lValue,2), fStream.Position]));
    end;
  end;
end;



class function TDataObjStreamer.getFileFilter: string;
begin
  result := 'DataObject Files (*.dataObj)|*.dataObj';
end;

class function TDataObjStreamer.Description: string;
begin
  result := 'Dynamic Data Object binary serialization.  https://github.com/SeanSolberg/DynamicDataObjects';
end;

destructor TDataObjStreamer.Destroy;
begin
  FreeAndNil(fReadSlotNameRefs);
  FreeAndNil(fReadStringRefs);
  FreeAndNil(fWriteSlotNameRefs);
  FreeAndNil(fWriteStringRefs);
  inherited;
end;

class function TDataObjStreamer.IsFileExtension(aStr: string): boolean;
begin
  result := SameText(aStr, 'DataObj') or SameText(aStr, '.DataObj');
end;

class function TDataObjStreamer.Name: string;
begin
  result := 'DataObj';
end;

class function TDataObjStreamer.Priority: Cardinal;
begin
  result := 1;
end;

function TDataObjStreamer.SlotnameRefWriteCount: integer;
begin
  result := 0;
  if assigned(fWriteSlotNameRefs) then
    result := fWriteslotNameRefs.NodeCount;
end;

function TDataObjStreamer.StringRefWriteCount: integer;
begin
  result := 0;
  if assigned(fWriteStringRefs) then
    result := fWriteStringRefs.NodeCount;
end;

procedure TDataObjStreamer.Encode(aDataObj: TDataObj);
begin
    // Always need to start with a clean StringRef list because a streamer object instance can be re-used.
  FreeAndNil(fWriteSlotNameRefs);
  if fUseSlotnameRefs then
  begin
    fWriteSlotNameRefs := TStringBinaryTree.Create;
  end;

  FreeAndNil(fWriteStringRefs);
  if fUseStringRefs then
  begin
    fWriteStringRefs := TStringBinaryTree.Create;
  end;

  EncodeInternal(aDataObj);
end;

procedure TDataObjStreamer.EncodeInternal(aDataObj: TDataObj);
var
  lStore: PTDataStore;
  i: Integer;
  lPos: int64;
  lValue: byte;
  lIndexVarInt: TUVarInt64;
  lLen: integer;
  lNode: TStringBinaryTreeNode;
  lBuffer: array[0..17] of Byte;

  procedure WriteVarInt(aVarInt: int64);
  var
    lVarInt: TVarInt64;
  begin
    lVarInt := aVarInt;
    lVarInt.WriteToStream(fStream);
  end;

  procedure WriteUVarInt(aVarInt: UInt64);
  var
    lVarInt: TUVarInt64;
  begin
    lVarInt := aVarInt;
    lVarInt.WriteToStream(fStream);
  end;

  procedure WriteSlotName(aString: String);
  var
    lSizeOrID: TSlotNameIDorSize;
    lNode: TStringBinaryTreeNode;

    procedure LocalWriteOutTheString;
    var
      lBytes: TBytes;
      lSize: Cardinal;
    begin
      lBytes := TEncoding.UTF8.GetBytes(aString);
      lSize := length(lBytes);
      lSizeOrID.SetAsSize(lSize);
      lSizeOrID.WriteToStream(fStream);
      fStream.Write(lBytes, lSize);
    end;

  begin
    // Slotnames are serialized as UTF-8 Strings.
    // We are either writing a byte-size followed by the bytes of the UTF-8 string (without terminating null)
    //  OR
    // We are writing a numerical identifier to define the ID of the slotname. Which was previously passed.
    // If the MSB bit of the first byte sent is set, then the remaining bits represent a number which is the ID of the string.

    // Since MOST serializations will have a reasonably limited set of slotNames...We will use positive numbers to denote just sending the string length followed by the string bytes.
    // If we send a Negative number, then the positive version of that number is the ID of the string that was previously passed.  SO, -1 means to re-use string #1 that was previously passed.
    // 0 means we are sending a zero length string and this string does NOT count as a possibility in the referenced strings.
    // Using this approach means we are "somewhat" forwards compatible with the previous serialization that never used negative numbers.  However, this compatibility ends if the slotname
    // was over 127 characters because previously this was

    if self.fUseSlotnameRefs then
    begin
      lNode := fWriteSlotNameRefs.FindNode(aString);
      if assigned(lNode) then
      begin
        // This string is already in our SlotnameRef list which means it has already been serialized at least once.  So, just send its ID.
        lSizeOrID.SetAsID(lNode.fID);
        lSizeOrID.WriteToStream(fStream);
      end
      else
      begin
        // This string is not yet in our SlotnameRef list, so write the string and add it to the SlotnameRef List with the next sequential ID.
        LocalWriteOutTheString;
        fWriteSlotNameRefs.AddString(aString, fWriteSlotNameRefs.NodeCount);
      end;
    end
    else
    begin
      // Write out the String without adding it to the fSlotnameRefs because our preference is to not perform slotnameRefs.
      LocalWriteOutTheString;
    end;
  end;

  procedure WriteUnicodeString(aString: String);
  var
    lSize: TUVarInt64;                                  // Notice that since string lengths can't possibly be a negative number, we are using an Unsigned VarInt instead which is faster.
  begin
    lSize := length(aString)*2;                         // returns the number of bytes used in the string.  2 for each character.
    lSize.WriteToStream(fStream);
    if Integer(lSize) > 0 then
      fStream.Write(aString[1], lSize);       // Unicode is two bytes per character.
  end;

  procedure WriteUTF8String(aString: String);
  var
    lSize: TUVarInt64;                                  // Notice that since string lengths can't possibly be a negative number, we are using an Unsigned VarInt instead which is faster.
    lBytes: TBytes;
  begin
    lBytes := TEncoding.UTF8.GetBytes(aString);
    lSize := length(lBytes);                    // returns the number of bytes used in the string.  UTF8 is variable on the number of bytes per character.
    lSize.WriteToStream(fStream);
    if Integer(lSize) > 0 then
      fStream.Write(lBytes, lSize);
  end;

  procedure WriteString(aString: String);
  begin
    if fSerializeAsUTF8 then
    begin
      lValue := (lValue and $60) or $81;    // Bits 7,6,5,4,3,2,1,0 = 0,p,p,0,0,0,0,0       where p = preserve to preserve subTypeCode.   $81 is for serializing a string as UTF8
      fStream.Write(lValue, 1);
      WriteUTF8String(lStore.DataString);
    end
    else
    begin
      fStream.Write(lValue, 1);             // leave value as original String (Unicode string)
      WriteUnicodeString(lStore.DataString);
    end;
  end;

begin
  if aDataObj.HasAttributes then
  begin
    // Write the attributes frame first.
    //aDataObj.
  end;

  lStore := aDataObj.getStore;

  // make a dataType value byte for transmission by bit smashing some things together.
  lValue := (ord(aDataObj.DataType.Code) and $1F) or          // 5  bits for the datatype code.
            ((aDataObj.DataType.SubClass and $03) shl 5);     // 2  bits for the subClass


  case aDataObj.DataType.Code of
    cDataTypeNull: begin fStream.Write(lValue, 1); end;
    cDataTypeBoolean: begin fStream.Write(lValue, 1); {just write the code because the value is stored right in the datatype byte with a subcode} end;
    cDataTypeByte: begin
      lBuffer[0] := lValue;
      lBuffer[1] := lStore.fDataByte;
      fStream.Write(lBuffer,2);
    end;
    cDataTypeInt32: begin
      lBuffer[0] := lValue;
      PInteger(@lBuffer[1])^ := lStore.fDataInt32;
      fStream.Write(lBuffer,5);
    end;
    cDataTypeInt64: begin
      lBuffer[0] := lValue;
      PInt64(@lBuffer[1])^ := lStore.fDataInt64;
      fStream.Write(lBuffer,9);
    end;
    cDataTypeSingle: begin
      lBuffer[0] := lValue;
      PSingle(@lBuffer[1])^ := lStore.fDataSingle;
      fStream.Write(lBuffer,5);
    end;
    cDataTypeDouble: begin
      lBuffer[0] := lValue;
      PDouble(@lBuffer[1])^ := lStore.fDataDouble;
      fStream.Write(lBuffer,9);
    end;
//    cDataTypeDecimal128:    not implemented yet.
    cDataTypeDateTime: begin
      lBuffer[0] := lValue;
      PDateTime(@lBuffer[1])^ := lStore.fDataDateTime;
      fStream.Write(lBuffer,9);
    end;
    cDataTypeUTCDateTime: begin
      lBuffer[0] := lValue;
      PInt64(@lBuffer[1])^ := lStore.fDataInt64;
      fStream.Write(lBuffer,9);
    end;
    cDataTypeDate, cDataTypeTime: begin
      lBuffer[0] := lValue;
      PDateTime(@lBuffer[1])^ := lStore.fDataDateTime;
      fStream.Write(lBuffer,9);
    end;
    cDataTypeGUID: begin
      lBuffer[0] := lValue;
      PGUID(@lBuffer[1])^ := lStore.dataGUID.GUID;
      fStream.Write(lBuffer,17);
    end;
    cDataTypeObjectID: begin
      lBuffer[0] := lValue;
      PTDataObjectIDData(@lBuffer[1])^ := lStore.dataObjectID.Data;
      fStream.Write(lBuffer,13);
    end;
    cDataTypeString: begin
      // Normal Unicode String is serialized as the CodeValue (14) when we are serializing unicodeString.
      // When the MSB bit is set on the dataType byte, that signals that we have a special serialization variation.
      // $80 is for Attributes
      // $81 is for serializing a string as UTF8
      // $82 is for serializing a string as a StringRef ID.
      // $83 is for serializing a stringList as UTF8
      // SubTypeCodes need to be preserved during this situation.  SubType 01 represents a string "SYMBOL" and 10 and 11 are reserved for future use.

      // Note that the subclassing was originally intended to be useable for future use at a higher level and be preserved into the dataType of the DataObject.
      // However, here, we are ONLY using it for serialization flags.
      if self.fUseStringRefs then
      begin
        lNode := fWriteStringRefs.FindNode(lStore.DataString);
        if assigned(lNode) then
        begin
          // We already sent this string previously so we can just send the ID then.  Code=$82, but we must preserve the subTypeCode.
          lValue := (lValue and $60) or $82;    // Bits 7,6,5,4,3,2,1,0 = 0,p,p,0,0,0,0,0       where p = preserve.  and then bring in $82
          fStream.Write(lValue, 1);

          lIndexVarInt := lNode.fID;
          lIndexVarInt.WriteToStream(fStream);
        end
        else
        begin
          // we need to write the string either as UTF8 or as UNICODE, but then save this in the Reflist for future writes by ID.
          WriteString(lStore.DataString);

          lLen := length(lStore.fDataString);
          if (lLen>0) then              // we do not include empty strings as referenceable strings.
            fWriteStringRefs.AddString(lStore.DataString, fWriteStringRefs.NodeCount);
        end;
      end
      else
      begin
        WriteString(lStore.DataString);
      end;
    end;
    cDataTypeStringList: begin
      // We are either going to be writing UnicodeStrings or UTF-8 strings.
      if fSerializeAsUTF8 then
        lValue := (lValue and $60) or $83;    // Bits 7,6,5,4,3,2,1,0 = 0,p,p,0,0,0,0,0       where p = preserve to preserve subType code.  and then bring in $83
      fStream.Write(lValue, 1);

      //Write out the number of strings that will be included in this stringList.  Then, write out each of the strings.
      WriteUVarInt(lStore.dataStringList.Count);

      if fSerializeAsUTF8 then
      begin
        for i := 0 to lStore.dataStringList.Count-1 do
          WriteUtf8String(lStore.dataStringList.Strings[i]);
      end
      else
      begin
        for i := 0 to lStore.dataStringList.Count-1 do
          WriteUnicodeString(lStore.dataStringList.Strings[i]);
      end;
    end;
    cDataTypeFrame: begin
       fStream.Write(lValue, 1);
       //Write out the number of slots in this frame, then write out each slot (Slotname as a normal string, followed by the dataObject for the slot)
       WriteUVarInt(lStore.dataFrame.Count);
      for i := 0 to lStore.dataFrame.Count-1 do
      begin
        WriteSlotName(lStore.dataFrame.Slotname(i));
        EncodeInternal(lStore.dataFrame.Slots[i]);         // recursion happening here.     NOTE:  Someday, we will need to impose a limit for how deep we can go.
      end;
    end;
    cDataTypeArray: begin
      fStream.Write(lValue, 1);
      //Write out the number of slots in this frame, then write out each slot which is just a dataObject only.
      WriteUVarInt(lStore.dataArray.Count);
      for i := 0 to lStore.dataArray.Count-1 do
      begin
        EncodeInternal(lStore.dataArray.Items[i]);   // recursion happening here.     NOTE:  Someday, we will need to impose a limit for how deep we can go.
      end;
    end;
    cDataTypeSparseArray: begin
      fStream.Write(lValue, 1);
      //Write out the number of slots in this frame, then write out each slot (Slotname as a normal string, followed by the dataObject for the slot)
      WriteUVarInt(lStore.dataSparseArray.Count);
      for i := 0 to lStore.dataSparseArray.Count-1 do
      begin
        WriteVarInt(lStore.dataSparseArray.SlotIndex(i)); // write out the slotIndex value.
        EncodeInternal(lStore.dataSparseArray.items[i]);   // recursion happening here.     NOTE:  Someday, we will need to impose a limit for how deep we can go.
      end;
    end;
    cDataTypeBinary: begin
      fStream.Write(lValue, 1);
      WriteUVarInt(lStore.dataBinary.Size);
      lPos := lStore.dataBinary.Position;      // preserve the position
      lStore.dataBinary.Seek(0, soBeginning);
      fStream.CopyFrom(lStore.dataBinary, lStore.dataBinary.size);
      lStore.dataBinary.Position := lPos;      // preserve the position
    end;
    cDataTypeObject: begin
      // Using the RTTI from this object, we are going to serialize the same as a TDataFrame with a special flag set that defines that the content was serialized from a real TObject descendant.
      // Note, this only serializes the published properties.

      // FINISH.

    end;
    cDataTypeTag: begin
      fStream.Write(lValue, 1);
      WriteUVarInt(lStore.dataTag.TagValue);
      EncodeInternal(lStore.dataTag.DataObj);           // recursion happening here.
    end;
  end;
end;

class function TDataObjStreamer.FileExtension: string;
begin
  result := 'DataObj';
end;

class procedure TDataObjStreamer.GetParameterInfo(aParameterPurpose: TDataObjParameterPurposes; aStrings: TStrings);
begin
  if cppEncoding in aParameterPurpose then
  begin
    aStrings.AddPair('-UseStringRefs <1 or 0>', 'If this switch is on, then serializing will use reference IDs when sending a repeated string value.  Default = 1(true)');
    aStrings.AddPair('-UseSlotnameRefs <1 or 0>', 'If this switch is on, then serializing will use reference IDs when sending a repeated slot name.  Default = 1(true)');
    aStrings.AddPair('-UseUTF8Strings <1 or 0>', 'If this switch is on, then strings will be serialized as UTF8 instead of UNICODE.  Default = 1(true)');
  end;
end;

procedure TDataObjStreamer.ApplyOptionalParameters(aParams: TStrings);
var
  i: Integer;

  function GetBoolNextParam: boolean;
  var
    lParam: string;
  begin
    lParam := aParams[i+1];
    result := SameText(lParam, '1') or SameText(lParam, 'T') or SameText(lParam, 'True');
  end;

begin
  inherited;

  i := 0;
  while i < aParams.Count do
  begin
    if SameText(aParams[i], 'UseStringRefs') and (i < aParams.Count-1) then
    begin
      fUseStringRefs := GetBoolNextParam;    ;
      inc(i);
    end
    else if SameText(aParams[i], 'UseSlotnameRefs') and (i < aParams.Count-1) then
    begin
      fUseSlotnameRefs := (aParams[i+1] = '1');
      inc(i);
    end
    else if SameText(aParams[i], 'UseUTF8Strings') and (i < aParams.Count-1) then
    begin
      fSerializeAsUTF8 := (aParams[i+1] = '1');
      inc(i);
    end;

    inc(i);
  end;

end;



procedure RegisterDataObjStreamer(aStreamer: TDataObjStreamerClass);
begin
  if not assigned(gStreamerRegistry) then
    gStreamerRegistry := TStreamerRegistry.Create;
  gStreamerRegistry.Add(aStreamer);
end;


{ TStreamerRegistry }

function TStreamerRegistry.AllStreamersFileDialogFilters: string;
var
  i: integer;
  lLine: string;
begin
  for i := 0 to count-1 do
  begin
    lLine := items[i].GetFileFilter;
    if length(result)=0 then
      result := lLine
    else
      result := result+'|'+lLine;
  end;
  result := result+'|All files (*.*)|*.*';
end;

function TStreamerRegistry.CreateStreamerByFilename(aFilename: string): TDataObjStreamerBase;
var
  lSC: TDataObjStreamerClass;
begin
  result := nil;
  lSC := FindStreamerClassByFilename(aFilename);
  if assigned(lSC) then
    result := lSC.Create(nil);
end;

function TStreamerRegistry.CreateStreamerByFilenameExtension(aExtension: string): TDataObjStreamerBase;
var
  lSC: TDataObjStreamerClass;
begin
  result := nil;
  lSC := FindStreamerClassByFilenameExtension(aExtension);
  if assigned(lSC) then
    result := lSC.Create(nil);
end;

function TStreamerRegistry.FindStreamerClassByClassname( aClassname: string): TDataObjStreamerClass;
var
  i: integer;
begin
  result := nil;

  for i := 0 to gStreamerRegistry.Count-1 do
  begin
    if SameText(gStreamerRegistry.Items[i].ClassName, aClassName) then
    begin
      result := gStreamerRegistry.Items[i];
      break;
    end;
  end;
end;

function TStreamerRegistry.FindStreamerClassByFilename(aFilename: string): TDataObjStreamerClass;
begin
  result := FindStreamerClassByFilenameExtension(ExtractFileExt(aFilename));
end;

function TStreamerRegistry.FindStreamerClassByFilenameExtension(aExtension: string): TDataObjStreamerClass;
var
  i: integer;
begin
  result := nil;

  for i := 0 to gStreamerRegistry.Count-1 do
  begin
    if gStreamerRegistry.Items[i].IsFileExtension(aExtension) then
    begin
      result := gStreamerRegistry.Items[i];
      break;
    end;
  end;
end;

procedure TStreamerRegistry.Sort;
begin
  inherited Sort(TStreamerRegistryComparer.create);
end;

{ TStreamerRegistryComparer }

function TStreamerRegistryComparer.Compare(const Left, Right: TDataObjStreamerClass): Integer;
begin
  result := Integer(Left.Priority) - Integer(right.Priority);
end;

initialization
  RegisterDataObjStreamer(TDataObjStreamer);

finalization
  FreeAndNil(gStreamerRegistry);

end.
