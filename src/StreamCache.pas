unit StreamCache;

interface

uses
  classes,
  math,
  windows;   // for getTickCount

Type
  TCacheMemoryStream = class(TMemoryStream)
  public
    property Capacity;
  end;

  TCustomMemoryStreamClassHelper = class helper for TCustomMemoryStream
  public
    function GetFastSize: Int64;
  end;


  TStreamWriteCache = class(TStream)
  private
    fMemStream: TMemoryStream;
    fCachedByteCount: integer;
    fCacheSize: integer;
  protected
    fRefStream: TStream;
  public
    constructor Create(aStream: TStream);
    destructor Destroy; override;
    procedure Dispose; 
  protected
    function GetSize: Int64; override;
    procedure SetSize(const NewSize: Int64); override;
  public
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    function WriteStr(const aStr: string): LongInt;
    procedure Flush;
  end;

  // this allows only reading from a stream and it will cache the reads into blocks
  // It's very useful for reading from large files where you will be reading small amounts
  // of data at a time.
  TStreamReadCache = class(TStream)
  private
    fMemStream: TCacheMemoryStream;
    fCacheSize: integer;
    fCacheBeginningPosition: Int64;
    fPosition: Int64;
    fLastKnownRefStreamSize: int64;        // it's expensive to call fRefStream.Size because it performs a seek to do it, so we will call it once at the beginning and keep it here.

    procedure LoadCache;
    procedure setCacheSize(input: integer);
  public
    constructor Create(aStream: TStream);
    destructor Destroy; override;
  protected
    fRefStream: TStream;
    function GetSize: Int64; override;
    procedure SetSize(const NewSize: Int64); override;
  public
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    property CacheSize: integer read fCacheSize write setCacheSize;
  end;


implementation


  { TStreamWriteCache }

constructor TStreamWriteCache.Create(aStream: TStream);
begin
  inherited create;
  fCacheSize:=65536;  // Typically, hard drive block sizes are 4096 bytes so we will default to read 65536 bytes (16 blocks).  We did some speed tests to guess that this is the right number.
  fRefStream:=aStream;
  fMemStream:=TMemoryStream.create;
  fMemStream.SetSize(fCacheSize);
end;

destructor TStreamWriteCache.Destroy;
begin
  Dispose;

  inherited;
end;

procedure TStreamWriteCache.Flush;
begin
  if fCachedByteCount>0 then
  begin
    fMemStream.Seek(0,soFromBeginning);
    fRefStream.CopyFrom(fMemStream,fCachedByteCount);
    fMemStream.Seek(0,soFromBeginning);   // been written, so go back to the beginning.
    fCachedByteCount:=0;
  end;
end;

function TStreamWriteCache.GetSize: Int64;
begin
  result:=fRefStream.Size + fMemStream.Position;
end;

function TStreamWriteCache.Read(var Buffer; Count: Integer): Longint;
begin
  // not implemented yet.
  result:=0;
end;

function TStreamWriteCache.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  result:=fRefStream.Seek(Offset,Origin);
  // not really fully implemented yet but will need to in order for getsize to work right
  // need to account for the bytes in the mem cache
end;

procedure TStreamWriteCache.SetSize(const NewSize: Int64);
begin
  inherited;
  // not implemented yet.
end;

function TStreamWriteCache.Write(const Buffer; Count: Integer): Longint;
var
  lRoom: integer;
begin
  lRoom:=fMemStream.Size-fCachedByteCount;
  if Count>lRoom then
  begin
    // flush what we have in the cache so far.
    Flush;

    // now, we flushed the cache because there wasn't room for our new data.  our cache is now empty.
    // if the size of the data coming in is too big for the cache, then just write it out directly
    // Otherwise, put it in the cache
    if Count>fCacheSize then
    begin
      result := fRefStream.Write(buffer,count);
    end
    else
    begin
      result := fMemStream.Write(buffer,count);
      fCachedByteCount:=fCachedByteCount+count;
    end;
  end
  else
  begin
    // there's room in the cache for our new data so add it in.
    result := fMemStream.Write(buffer,count);
    fCachedByteCount:=fCachedByteCount+count;
  end;
end;

function TStreamWriteCache.WriteStr(const aStr: string): LongInt;
begin
  result:=Write(aStr[1],length(aStr));
end;

procedure TStreamWriteCache.Dispose;
begin
  Flush;
  fMemStream.free;
end;

{ TStreamReadCache }

(*procedure TStreamReadCache.ClearCacheAccess;
begin
  fCacheAccessTrack.Clear;
end;*)

constructor TStreamReadCache.Create(aStream: TStream);
begin
  inherited create;
  // about 10000.  We do not want this to be too large or else we will run into
  //memory fragmentation when allocating a bunch of these in a row which will
  //eventually cause out of memory.  
  fCacheSize:=512 * 16;     // 8K seems optimal.
  fRefStream:=aStream;
  fMemStream:=TCacheMemoryStream.create;
  fMemStream.Capacity:=fCacheSize;
  fCacheBeginningPosition:=0;
  fLastKnownRefStreamSize := aStream.Size;             // need to call this once and keep the answer around for later.
end;

destructor TStreamReadCache.Destroy;
begin
  fMemStream.free;
  inherited;
end;

function TStreamReadCache.GetSize: Int64;
begin
  result:=fRefStream.Size;
end;

procedure TStreamReadCache.LoadCache;
begin
  // load the current fRefStream data into the Cache based on the value of fPostion
  // load as much as we can and update the
  if fPosition <> fRefStream.position then
    fPosition:=fRefStream.Seek(fPosition, soBeginning);
  fMemStream.seek(0,soFromBeginning);             // bring cache to the beginning
  if fMemStream.Capacity <> fCacheSize then
    fMemStream.Capacity := fCacheSize;
  fCacheBeginningPosition := fPosition;
  fMemStream.CopyFrom(fRefStream,min(fCacheSize,fRefStream.Size-fPosition));     // will only read what it can and the fMemStream.size
  fMemStream.seek(0,soFromBeginning);
end;

type
  TByteArray = array[0..MaxInt - 1] of Byte;

function TStreamReadCache.Read(var Buffer; Count: Integer): Longint;
var
  lReadCount: Int64;
  lReadCount2: Int64;
  lMemSize: int64;
  lPointer: pointer;
begin
  // First see if the current position is in the space that we have to load from the master stream.
  if fPosition >= fLastKnownRefStreamSize then
  begin
    fLastKnownRefStreamSize := fRefStream.Size;   // need to re-acquire the fLastKnownRefStreamSize after we got it initially cause it's technically possible that the stream has grown since we started.
    if fPosition >= fLastKnownRefStreamSize then
    begin
      result := 0;
      exit;
    end;
  end;

  // see if our current position is in the cache.  If not, then we need to load the cache
  lMemSize := fMemStream.GetfastSize;

  if not((FPosition>=fCacheBeginningPosition) and (fPosition <= fCacheBeginningPosition+lMemSize)) or
    (lMemSize=0) then
  begin
    LoadCache;
  end;

  // now we need to read as much as we can from the cache.  If we didn't read all that we asked for from the cache,
  // then we need to load the next cache block, and complete the read.
  if fMemStream.Position <> FPosition-fCacheBeginningPosition then
    fMemStream.Position:=FPosition-fCacheBeginningPosition;
  lReadCount:=fMemStream.Read(Buffer,Count);
  FPosition:=FPosition+lReadCount;


  // if the ReadCount wasn't as much as we asked for, then we need to read the rest from the source strea
  // If the remaining amount to be read is smaller than the cache size, then Load the Cache and
  // complete the read from the cache.  If not, then just read the rest from the source and forget the
  // cache.  Don't load anything in the cache
  if lReadCount<Count then
  begin
    lPointer:=addr(TByteArray(buffer));
    lPointer:=Pointer(NativeInt(lPointer)+lReadCount);  // figure out where to put the rest of the bytes begin read.
    if Count-lReadCount > fMemStream.Size then
    begin
      // the remaining amount to read is bigger than the cache so we might as well just read the entire block
      // from the source stream and not waste time putting it into the cache.
      lReadCount2:=fRefStream.Read(lPointer^,Count-lReadCount);
      FPosition:=FPosition+lReadCount2;
      result := lReadCount+lReadCount2;
    end
    else
    begin
      LoadCache;
      lReadCount2:=fMemStream.Read(lPointer^,Count-lReadCount);
      FPosition:=FPosition+lReadCount2;
      result := lReadCount+lReadCount2;
    end;
  end
  else
    result := lReadCount;
end;

function TStreamReadCache.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  case Origin of
    soBeginning: FPosition := Offset;
    soCurrent: Inc(FPosition, Offset);
    soEnd: FPosition := FRefStream.Size + Offset;
  end;
  Result := FPosition;
end;

procedure TStreamReadCache.setCacheSize(input: integer);
begin
  fCacheSize:=input;
  //finish.  if the cache is already being used, we need to re-size and re-load?
end;

procedure TStreamReadCache.SetSize(const NewSize: Int64);
begin
  fRefStream.Size:=NewSize;
end;

function TStreamReadCache.Write(const Buffer; Count: Integer): Longint;
begin
  // write is not implemented.
  result:=0;
end;


{ TCustomMemoryStreamClassHelper }

function TCustomMemoryStreamClassHelper.GetFastSize: Int64;
begin
//  result := self.fSize;  would really want to do this, but now in Berlin, we can't
  result := self.Size;
end;

end.
