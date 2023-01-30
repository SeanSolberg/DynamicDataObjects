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

uses SysUtils, DataObjects2, classes, Generics.collections, VarInt, Generics.Defaults;

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
    function CreateStreamerByFilenameExtension(aExtension: string): TDataObjStreamerBase;
    function CreateStreamerByFilename(aFilename: string): TDataObjStreamerBase;
  end;

  TStreamerRegistryComparer = class(TComparer<TDataObjStreamerClass>)
  public
    function Compare(const Left, Right: TDataObjStreamerClass): Integer; override;
  end;


  // This is the native DataObject streaming that directly fully supports reading and writing all data in the TDataObjects object model.
  // This format registers the "*.dataObj" file extension.
  TDataObjStreamer = class(TDataObjStreamerBase)
  public
    class function FileExtension: string; override;
    class function Description: string; override;
    class function GetFileFilter: string; override;
    class function IsFileExtension(aStr: string): boolean; override;
    class function ClipboardPriority: cardinal; override;

    destructor Destroy; override;

    procedure Decode(aDataObj: TDataObj); override;
    procedure Encode(aDataobj: TDataObj); override;
  end;

var
  gStreamerRegistry: TStreamerRegistry;

procedure RegisterDataObjStreamer(aStreamer: TDataObjStreamerClass);

implementation

{ TDataObjStreamer }

class function TDataObjStreamer.ClipboardPriority: cardinal;
begin
  result := 1;    // highest priority
end;

procedure TDataObjStreamer.Decode(aDataobj: TDataObj);
var
  i: Integer;
  lCount: integer;
  lIndex: integer;
  lReadCount: Integer;
  lStore: PTDataStore;
  lDataObj: TDataObj;
  lValue: byte;

  // Read a Unsigned UVarInt from the stream.
  function ReadUVarInt: UInt64;
  var
    lVarInt: TUVarInt64;
  begin
    lVarInt.ReadFromStream(fStream);
    result := lVarInt;
  end;

  // Read a string sequence from the stream
  function ReadUTF8String: UTF8String;
  var
    lVarIntSize: TUVarInt64;
    lSize: integer;
  begin
    lVarIntSize := ReadUVarInt;
    lSize := lVarIntSize;                                    // convert from varInt to integer;
    SetLength(result, lSize);                                // allocate buffer to read the UTF8 data into from the stream
    try
      fStream.read(result[1], lSize);
    except
      SetLength(result, 0);                                  // Free what we allocated
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
      fStream.read(lBytes, lSize);
      result := TEncoding.Unicode.GetString(lBytes);            // performs the decoding from UTF8 to string.
    finally
      SetLength(lBytes, 0);                                  // Free what we allocated
    end;
  end;

  procedure DoException;
  begin
    aDataObj.DataType.Code:=cDataTypeNull;
    raise EDataObj.Create('Unable to read DataObject('+aDataObj.DataTypeString+') from stream at offset = '+intToStr(fStream.Position));
  end;

begin
  lStore := aDataObj.getStore;

  fStream.Read(lValue, 1);

  aDataObj.SetDataTypeParts(TDataTypeCode(lValue and $1F), // 5  bits for the datatype code}
                           (lValue shr 5) and $03,        // 2  bits for the subClass
                           (lValue and $80)<>0);          // 1  bit for Has Attributes

  case aDataObj.DataType.Code of
    cDataTypeNull: begin end;

    cDataTypeBoolean: begin end;

    cDataTypeByte: begin
      lReadCount := fStream.Read(lStore.fDataByte, 1);
      if lReadCount <> 1 then DoException;
    end;

    cDataTypeInt32: begin
      lReadCount := fStream.Read(lStore.fDataInt32, 4);
      if lReadCount <> 4 then DoException;
    end;

    cDataTypeInt64: begin
      lReadCount := fStream.Read(lStore.fDataInt64, 8);
      if lReadCount <> 8 then DoException;
    end;

    cDataTypeSingle: begin
      lReadCount := fStream.Read(lStore.fDataSingle, 4);
      if lReadCount <> 4 then DoException;
    end;

    cDataTypeDouble: begin
      lReadCount := fStream.Read(lStore.fDataDouble, 8);
      if lReadCount <> 8 then DoException;
    end;

//    cDataTypeDecimal128: begin end;  not implemented yet.

    cDataTypeDateTime: begin
      lReadCount := fStream.Read(lStore.fDataDateTime, 8);
      if lReadCount <> 8 then DoException;
    end;

    cDataTypeUTCDateTime: begin
      lReadCount := fStream.Read(lStore.fDataInt64, 8);
      if lReadCount <> 8 then DoException;
    end;

    cDataTypeDate: begin
      lReadCount := fStream.Read(lStore.fDataDateTime, 8);
      if lReadCount <> 8 then DoException;
    end;

    cDataTypeTime: begin
      lReadCount := fStream.Read(lStore.fDataDateTime, 8);
      if lReadCount <> 8 then DoException;
    end;

    cDataTypeGUID: begin
      lStore.dataGUID := TDataGUID.Create;
      lReadCount := fStream.ReadData(@lStore.dataGUID.GUID, 16);
      if lReadCount <> 16 then DoException;
    end;

    cDataTypeObjectID: begin
      lStore.dataObjectID := TDataObjectID.Create;
      lReadCount := fStream.Read(TDataObjectID(lStore.dataObjectID).Data, 12);
      if lReadCount <> 12 then DoException;
    end;

    cDataTypeString: begin
      lStore.fdataString := ReadUnicodeString;
    end;

    cDataTypeStringList: begin
      lCount := ReadUVarInt;
      lStore.dataStringList := TDataStringList.Create;
      for i := 0 to lCount-1 do
      begin
        TDataStringList(lStore.dataStringList).Add(ReadUnicodeString);
      end;
    end;

    cDataTypeFrame: begin
      lCount := ReadUVarInt;
      lStore.dataFrame := TDataFrame.Create;
      for i := 0 to lCount-1 do
      begin
        Decode(lStore.dataFrame.NewSlot(string(ReadUTF8String)));     // Note:  Recursion happening here.  TODO. in the future, we should limit how far deep we can nest.
      end;
    end;

    cDataTypeArray: begin
      lCount := ReadUVarInt;
      lStore.dataArray := TDataArray.Create;
      for i := 0 to lCount-1 do
      begin
        lDataObj := TDataObj.Create;
        try
          Decode(lDataObj);     // Note:  Recursion happening here.  TODO. in the future, we should limit how far deep we can nest.
        except
          lDataObj.Free;
          raise;
        end;
        lStore.dataArray.Add(lDataObj);
      end;
    end;

    cDataTypeSparseArray: begin
      lCount := ReadUVarInt;
      lStore.dataSparseArray := TDataSparseArray.create;
      for i := 0 to lCount-1 do
      begin
        lIndex := ReadUVarInt;
        lDataObj := TDataObj.Create;
        try
          Decode(lDataObj);     // Note:  Recursion happening here.  TODO. in the future, we should limit how far deep we can nest.
        except
          lDataObj.Free;
        end;
        lStore.dataSparseArray.AppendSlot(lIndex, lDataObj);
      end;
    end;

    cDataTypeBinary: begin
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

    cDataTypeObject: begin
      // Using the RTTI from this object, we are going to serialize the same as a TDataFrame with a special flag set that defines that the content was serialized from a real TObject descendant.

      // FINISH.

    end;

    cDataTypeTag: begin
      lStore.dataTag := TDataTag.Create;            //FINISH - THIS IS NOT TESTED.
      lStore.dataTag.TagValue := ReadUVarInt;
      Decode(lStore.dataTag.DataObj);          // recursion happening here.
    end;
  else
    begin
      // FINISH - need to generate a good exception here because we received a dataType that's not valid.
      raise exception.Create('Read an invalid data type of '+IntToHex(lValue,2));
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
  inherited;
end;

class function TDataObjStreamer.IsFileExtension(aStr: string): boolean;
begin
  result := SameText(aStr, 'DataObj') or SameText(aStr, '.DataObj');
end;

procedure TDataObjStreamer.Encode(aDataObj: TDataObj);
var
  lStore: PTDataStore;
  i: Integer;
  lPos: int64;
  lValue: byte;

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

  procedure WriteUTF8String(aUTF8String: UTF8String);
  var
    lSize: TUVarInt64;                                      // Notice that since string lengths can't possibly be a negative number, we are using an Unsigned VarInt instead which is faster.
  begin
    lSize := length(aUTF8String);                           // returns the number of bytes used in the string.
    lSize.WriteToStream(fStream);
    fStream.Write(aUTF8String[1], lSize);
  end;

  procedure WriteUnicodeString(aString: String);
  var
    lSize: TUVarInt64;                                  // Notice that since string lengths can't possibly be a negative number, we are using an Unsigned VarInt instead which is faster.
  begin
    lSize := length(aString)*2;                         // returns the number of byres used in the string.  2 for each character.
    lSize.WriteToStream(fStream);
    fStream.Write(aString[1], lSize);       // Unicode is two bytes per character.
  end;

begin
  lStore := aDataObj.getStore;

  // make a dataType value byte for transmission by bit smashing some things together.
  lValue := (ord(aDataObj.DataType.Code) and $1F) or          // 5  bits for the datatype code.
            ((aDataObj.DataType.SubClass and $03) shl 5);     // 2  bits for the subClass
  if aDataObj.DataType.HasAttributes then
    lValue := lValue or $80;                                  // 1  bit for Has Attributes


  fStream.Write(lValue, 1);
  case aDataObj.DataType.Code of
    cDataTypeNull: begin end;
    cDataTypeBoolean: begin {nothing to write because the value is stored right in the datatype byte with a subcode} end;
    cDataTypeByte: fStream.Write(lStore.fDataByte,1);
    cDataTypeInt32: fStream.Write(lStore.fDataInt32,4);
    cDataTypeInt64: fStream.Write(lStore.fDataInt64,8);
    cDataTypeSingle: fStream.Write(lStore.fDataSingle,4);
    cDataTypeDouble: fStream.Write(lStore.fDataDouble,8);
//    cDataTypeDecimal128:    not implemented yet.
    cDataTypeDateTime: fStream.Write(lStore.fDataDateTime,8);
    cDataTypeUTCDateTime: fStream.Write(lStore.fDataInt64,8);
    cDataTypeDate: fStream.Write(lStore.fDataDateTime,8);
    cDataTypeTime: fStream.Write(lStore.fDataDateTime,8);
    cDataTypeGUID: fStream.Write(lStore.dataGUID.GUID, 16);
    cDataTypeObjectID: fStream.Write(lStore.dataObjectID.Data, 12);
    cDataTypeString: begin
      WriteUnicodeString(lStore.DataString);
    end;
    cDataTypeStringList: begin
      //Write out the number of strings that will be included in this stringList.  Then, write out each of the strings.
      WriteUVarInt(lStore.dataStringList.Count);
      for i := 0 to lStore.dataStringList.Count-1 do
      begin
        WriteUnicodeString(lStore.dataStringList.Strings[i]);
      end;
    end;
    cDataTypeFrame: begin
      //Write out the number of slots in this frame, then write out each slot (Slotname as a normal string, followed by the dataObject for the slot)
      WriteUVarInt(lStore.dataFrame.Count);
      for i := 0 to lStore.dataFrame.Count-1 do
      begin
        WriteUTF8String(UTF8String(lStore.dataFrame.Slotname(i)));      // write out the slotname
        Encode(lStore.dataFrame.Slots[i]);         // recursion happening here.     NOTE:  Someday, we will need to impose a limit for how deep we can go.
      end;
    end;
    cDataTypeArray: begin
      //Write out the number of slots in this frame, then write out each slot which is just a dataObject only.
      WriteUVarInt(lStore.dataArray.Count);
      for i := 0 to lStore.dataArray.Count-1 do
      begin
        Encode(lStore.dataArray.Items[i]);   // recursion happening here.     NOTE:  Someday, we will need to impose a limit for how deep we can go.
      end;
    end;
    cDataTypeSparseArray: begin
      //Write out the number of slots in this frame, then write out each slot (Slotname as a normal string, followed by the dataObject for the slot)
      WriteVarInt(lStore.dataSparseArray.Count);
      for i := 0 to lStore.dataSparseArray.Count-1 do
      begin
        WriteUVarInt(lStore.dataSparseArray.SlotIndex(i));         // write out the slotIndex value.
        Encode(lStore.dataSparseArray.items[i]);   // recursion happening here.     NOTE:  Someday, we will need to impose a limit for how deep we can go.
      end;
    end;
    cDataTypeBinary: begin
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
      WriteUVarInt(lStore.dataTag.TagValue);
      Encode(lStore.dataTag.DataObj);           // recursion happening here.
    end;
  end;
end;

class function TDataObjStreamer.FileExtension: string;
begin
  result := 'DataObj';
end;


procedure RegisterDataObjStreamer(aStreamer: TDataObjStreamerClass);
begin
  if not assigned(gStreamerRegistry) then
    gStreamerRegistry := TStreamerRegistry.Create;
  gStreamerRegistry.Add(aStreamer);
end;


{ TStreamerRegistry }

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
  result := Integer(Left.ClipboardPriority) - Integer(right.ClipboardPriority);
end;

initialization
  RegisterDataObjStreamer(TDataObjStreamer);

finalization
  FreeAndNil(gStreamerRegistry);

end.
