unit DataObjects2Clipboard;

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

uses
  Classes, generics.collections, generics.defaults, Windows, SysUtils, DataObjects2, DataObjects2Streamers;

type
  TClipboardItem = class(TPersistent)
  private
    {private declarations}
    fDataObj: TDataObj;
    fSlotName: String;
  public
    {public declarations}
    constructor Create;
    destructor Destroy; override;
    property DataObj: TDataObj read fDataObj;
  published
    {published properties}
    property SlotName: String read fSlotName write fSlotName;  //Does not apply when this is an element of an array
  end;

  TDataObjClipboard = class(TObjectList<TClipboardItem>)
  private
    procedure CopyFromClipboardWithStreamer(aStreamer: TDataObjStreamerBase);

  public
    {public declarations}
    function AddDataObj(ASlotName: String; ADataObj: TDataObj): TClipboardItem;

    // This will copy to the clipboard using all of the registered streaming formats.
    procedure CopyToClipboard;

    procedure CopyToClipboardWithStreamer(aStreamer: TDataObjStreamerBase);

    // This will copy from the clipboard and it will iterate through the registered formats in a prioritized way choosing the first one that it finds that is possible.
    procedure CopyFromClipboard;
  end;

  TDirectClipboardMemoryStream = class(TCustomMemoryStream)
  public
    Constructor Create(aMemory: Pointer; aSize: NativeUInt);
  end;

implementation

uses
  ClipBrd, Dialogs;

resourcestring
  StrDataObjectStreamerNoFormat = 'DataObject Streamer %s does not have a registered clipboard format';

{ TDataObjClipboard }

function TDataObjClipboard.AddDataObj(ASlotName: String; ADataObj: TDataObj): TClipboardItem;
begin
  Result:=TClipboardItem.create;
  Result.SlotName:=ASlotName;
  Result.DataObj.CopyFrom(ADataObj);
  add(Result);
end;



procedure TDataObjClipboard.CopyToClipboard;
var
  i: integer;
  lStreamer: TDataObjStreamerBase;
begin
  Clipboard.Open;
  try
    Clipboard.Clear; { Clear the clipboard first }

    // first, we need a list of the streamer registry classes and we need to sort that list by their Clipboard Priorities so we put the highest priority (lowest number) in the clipboard first.
    gStreamerRegistry.Sort;

    // now go through the sorted streamers from highest priority (lowest number) to the lowest priority (highest number) write to the clipboard using that format.
    for i := 0 to gStreamerRegistry.Count-1 do
    begin
      lStreamer := gStreamerRegistry.Items[i].Create(nil);  // create a streamer with default streaming properties if that streamer has configurable properties.
      try
        try
          CopyToClipboardWithStreamer(lStreamer);   // we trap all exceptions because some of them may have restrictions on what it can produce but we don't want it to skip out on the others.
        except end;
      finally
        lStreamer.Free;
      end;
    end;
  finally
    Clipboard.Close;
  end;
end;


procedure TDataObjClipboard.CopyToClipboardWithStreamer(aStreamer: TDataObjStreamerBase);
var
  i: integer;
  lIndex: TDataObj;
  lIndexFrame: TDataFrame;
  lIndexMem: TMemoryStream;
  lContentMem: TMemoryStream;
  lStartSize: Int64;
  lClipboardFormat: word;

  procedure WriteStreamToClipboard(aStream: TStream; aFormat: word);
  var
    lGlobalHandle: THandle;
    lGlobalPointer: Pointer;
  begin
    // Allocate the memory with a global lock
    lGlobalHandle := GlobalAlloc(GMEM_MOVEABLE, aStream.Size);
    //Lock the memory
    lGlobalPointer := GlobalLock(lGlobalHandle);    { Lock the allocated memory }
    try
      aStream.Seek(0, soFromBeginning);
      aStream.Read(lGlobalPointer^, aStream.Size);       // copy our memory stream to the globally allocated memory that will go to the clipboard.

      // Now apply this global memory to the clipboard.   It is expected that the caller already has opened the clipboard.
      Clipboard.SetAsHandle(aFormat, lGlobalHandle);  { Copy to clipboard }
    finally
      //unlock the allocate memory. But don't free it, it will be used by the clipboard
      GlobalUnlock(lGlobalHandle);
    end;
  end;

begin
  // To support having multiple dataObject items in a clipboard, we produce a DataObjectEditor clipboard type that has an
  // index to the actual dataObject serialization payloads.
  // This index is simply one dataObject serialization of that same serialization type that contains the information needed for each item in the clipboard format such as the caption and the size.
  // Each dataObject stream serialization has each of the serialization payloads put into the clipboard data stacked one after the other and the size of each one is told to the
  // receiver through the index
  // The Index portion is written as a separate Serialization under it's own clipboard format ID and it is currently formatted as follows:
  //  [caption: <text caption of the dataObject that we copied>, size: <integer>]
  // Note that this allows for expandability in the future as we could serialize more attributes in this index.
  // the number of items in this index denotes the number of serialization payloads that will be in the associated payload clipboard format
  // For text and binary types of serializations, each items size helps the receiver decode on the proper byte boundary
  // For text serializations where the receiver is another app that can take in text, that app will see the payload as one block of continuous text that may or may not have
  // crlf in between the individual items put into the clipboard.
  //example
  // [{caption: "Devices", size: 1234},
  //  {caption: "DeviceTypes", size: 4567}]

  Clipboard.Open;
  try
    //Save the collection of copied DataObjects and their slotnames to a chunk of memory in the given format using the given streamer
    lIndexMem:=TMemoryStream.Create;
    lContentMem:=TMemoryStream.create;
    lIndex:=TDataObj.Create;
    try
    // So we can handle having multiple selected slots in the clipboard, we are going to make a particularily structured dataObject that holds
    // the selected data objects in one container dataObject.  Then, we will stream that one data object to the clipboard memory.
      aStreamer.Stream := lContentMem;                 // link this streamer to the memStream that we are using for the clipboard.

      //put our parts into the content memory stream and build our index along the way.
      for i:=0 to (Count-1) do
      begin
        aStreamer.ClipboardEncode(items[i].fDataObj);

        lStartSize := lContentMem.size;
        lIndexFrame := lIndex.AsFrame.newSlot('Items').AsArray.NewSlot.AsFrame;
        lIndexFrame.newSlot('caption').AsString := Items[i].fSlotName;
        lIndexFrame.newSlot('size').AsInt64 := lContentMem.Size - lStartSize;   // give the index the number of bytes of just this item.
      end;

      // Now that all our parts are serialized into the content memory, we need to write two Formats to the clipboard, the content and the index.
      WriteStreamToclipboard(lContentMem, aStreamer.ClipboardFormat);

      aStreamer.Stream := lIndexMem;
      aStreamer.Encode(lIndex);
      lClipboardFormat := RegisterClipboardFormat(PWideChar('CF_DataObjectClipboardMeta'));
      WriteStreamToClipboard(lIndexMem, lClipboardFormat);
    finally
      lIndex.free;
      lContentMem.Free;
      lIndexMem.Free;
    end;
  finally
    Clipboard.Close;
  end;
end;

procedure TDataObjClipboard.CopyFromClipboard;
var
  i: integer;
  lStreamer: TDataObjStreamerBase;
begin
  // now go through the sorted streamers from highest priority (lowest number) to the lowest priority (highest number) and try to read from the clipboard
  // from the first one that can possibly do so.
  gStreamerRegistry.Sort;
  for i := 0 to gStreamerRegistry.Count-1 do
  begin
    lStreamer := gStreamerRegistry.Items[i].Create(nil);  // create a streamer with default streaming properties if that streamer has configurable properties.
    try
      try
        CopyFromClipboardWithStreamer(lStreamer);     // if this doesn't except out then it must have worked.
        break;
      except
        // Just trapping all exceptions from the CopyToClipboardWithStreamer call so we can try another streamer that might work.
      end;
    finally
      lStreamer.Free;
    end;
  end;

end;

procedure TDataObjClipboard.CopyFromClipboardWithStreamer(aStreamer: TDataObjStreamerBase);
var
  lSize: NativeUInt;
  lMem: TDirectClipboardMemoryStream;

  lFormat: word;
  lContainer: TDataObj;
  lContainerFrame: TDataFrame;

  lGlobalHandle: THandle;
  lGlobalPointer: Pointer;
  i: Integer;
begin
(* FINISH - MUST RESTORE REFACTORING THIS
  lFormat := RegisterClipboardFormat(PWideChar(aStreamer.GetClipboardFormatStr));  *)
  if lFormat = 0 then
    raise exception.Create(format(StrDataObjectStreamerNoFormat,[aStreamer.classname]));

  //Clear the current objects if there are any.
  Clear;

  //Be sure the clipboard has the proper format that we are asking for.
  if Clipboard.HasFormat(lFormat) then
  begin
    //Open the clipboard
    ClipBoard.Open;
    try
      //get the handle to the globally allocated memory that we are pasting from.
      lGlobalHandle := Clipboard.GetAsHandle(lFormat);

      //Lock it!
      lGlobalPointer := GlobalLock(lGlobalHandle);
      try
        lSize := GlobalSize(lGlobalHandle);

        //move the data from the global memory to our own memory stream.
        lContainer:=TDataObj.create;
        try
          lMem:=TDirectClipboardMemoryStream.Create(lGlobalPointer, lSize);
          try
            aStreamer.Stream := lMem;          // link the streamer to the memory stream so we can read from it.
            aStreamer.Decode(lContainer);

            // Now pull out the individual pieces of data that we loaded in our container and put them into individual items.
            lContainerFrame := lContainer.AsFrame;
            for i := 0 to lContainerFrame.Count-1 do
            begin
              Self.AddDataObj( lContainerFrame.Slotname(i), lContainerFrame.Slots[i] );  // will make a clone of the dataObject parameter because the container it's in will get freed below.
            end;
          finally
            lMem.Free;
          end;
        finally
          lContainer.Free;
        end;
      finally
        //unlock the global memory, but don't free it. This will be done by the clipboard itself,
        GlobalUnlock(lGlobalHandle);
      end;
    finally
      //Close the clipboard
      Clipboard.Close;
    end;
  end;
end;

{ TClipboardItem }

constructor TClipboardItem.Create;
begin
  inherited Create;
  fDataObj:=TDataObj.Create;
  fSlotName:='';
end;

destructor TClipboardItem.Destroy;
begin
  fDataObj.Free;
  inherited Destroy;
end;


{ TDirectClipboardMemoryStream }

constructor TDirectClipboardMemoryStream.Create(aMemory: Pointer; aSize: NativeUInt);
begin
  inherited Create;
  SetPointer(aMemory, aSize);
end;

end.
