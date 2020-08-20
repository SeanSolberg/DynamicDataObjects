unit DataObjectsClipboard;

interface

uses
  Classes, generics.collections, generics.defaults, Windows, SysUtils, DataObjects2, DataObjectsStreamers;

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
    procedure CopyToClipboardWithStreamer(aStreamer: TDataObjStreamerBase);
    procedure CopyFromClipboardWithStreamer(aStreamer: TDataObjStreamerBase);

  public
    {public declarations}
    function AddDataObj(ASlotName: String; ADataObj: TDataObj): TClipboardItem;

    // This will copy to the clipboard using all of the registered streaming formats.
    procedure CopyToClipboard;

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
        CopyToClipboardWithStreamer(lStreamer);
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
  lContainer: TDataObj;
  lContainerFrame: TDataFrame;
  lMem: TMemoryStream;

  lGlobalHandle: THandle;
  lGlobalPointer: Pointer;
  lFormat: word;
begin
  lFormat := RegisterClipboardFormat(PWideChar(aStreamer.GetClipboardFormatStr));
  if lFormat = 0 then
    raise exception.Create(format(StrDataObjectStreamerNoFormat,[aStreamer.classname]));

  //Save the collection of copied DataObjects and their slotnames to a chunk of memory in the given format using the given streamer
  lMem:=TMemoryStream.Create;

  // So we can handle having multiple selected slots in the clipboard, we are going to make a particularily structured dataObject that holds
  // the selected data objects in one container dataObject.  Then, we will stream that one data object to the clipboard memory.
  lContainer:=TDataObj.Create;
  try
    lContainerFrame := lContainer.AsFrame;

    //first, put our parts into the container DataObj
    for i:=0 to (Count-1) do
    begin
      lContainerFrame.NewSlot(Items[i].fSlotName).CopyFrom(Items[i].fDataObj);
    end;

    aStreamer.Stream := lMem;                 // link this streamer to the memStream that we are using for the clipboard.
    aStreamer.Encode(lContainer);

    // Allocate the memory with a global lock
    lGlobalHandle := GlobalAlloc(GMEM_MOVEABLE, lMem.Size);

    //Lock the memory
    lGlobalPointer := GlobalLock(lGlobalHandle);    { Lock the allocated memory }

    lMem.Seek(0, soFromBeginning);
    lMem.Read(lGlobalPointer^, lMem.Size);          // copy our memory stream to the globally allocated memory that will go to the clipboard.

    // Now apply this global memory to the clipboard.   It is expected that the caller already has opened the clipboard.
    Clipboard.SetAsHandle(lFormat, lGlobalHandle);  { Copy to clipboard }

    //unlock the allocate memory. But don't free it, it will be used by the clipboard
    GlobalUnlock(lGlobalHandle);
  finally
    lContainer.Free;
    lMem.Free;
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
  lFormat := RegisterClipboardFormat(PWideChar(aStreamer.GetClipboardFormatStr));
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
