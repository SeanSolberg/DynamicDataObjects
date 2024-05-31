unit DataObjects2ZMQ;

{ This unit provides a ZMQ (Zero Message Queue) subscriber and provider that will send DataObjects
  over a message queue with two parts: a two-byte message type and the payload stream.  The payload stream can be any
  type of streaming that is supported by the DataObjects2 Streamers such as JSON, DataObj, CBOR, etc.  To choose the type of
  payload streaming used, we simply instantiate one of the DataObjectStreamer descendants and assign it to the Publisher or Subscriber.

  To use these classes, simply create a publisher with a binding string or a connect string and create a subscriber with a connect string.
  Sample Binding String: 'tcp://*:5678'
  Sample connect String: 'tcp://localhost:5678'

  Sample Publisher
    fPublisher := TZMQPublisher.create('tcp://*:8080');
    fPublisher.Publish(lMessageType, lDataObj);

  Sample Subscriber
    lStreamer := TJsonStreamer.create;
    lStreamer.
    fSubscriber := TZMQSubscriber.create('tcp://localhost:5678', ReceiveMessage, true);

    procedure TForm1.ReceiveMessage(Sender: TZMQSubscriber; aMessageType: word; aDataObj: TDataObj);
    begin
      // do something with aDataObj.  Note, sender still owns aDataObj.
    end;
}

interface

uses classes, ActiveX, DataObjects2, {DataObjects2DDO,} DataObjects2JSON, {bsonDoc, bsonUtils,} ZMQObjects, ZMQ, zmqapi, sysUtils;

type
  TReceiveDataObjMessageEvent = procedure(Sender: TObject; aMessageType: word; aDataObj: TDataObj) of object;

  TDataObjZMQPublisher = class(TZMQPublisherBase)
  private
    fStreamer: TDataObjStreamerBase;
    fOwnsStreamer: Boolean;
  public
    constructor CreateWithBinding(aBinding: AnsiString; aStreamer: TDataObjStreamerClass; aPrivateKey: ansiString = ''; aPublicKey: ansiString = ''); overload;
    constructor CreateWithBinding(aBinding: AnsiString; aStreamer: TDataObjStreamerBase; aPrivateKey: ansiString = ''; aPublicKey: ansiString = ''); overload;

    // Note that this version of CreateWithConnect will get in a class for the streamer which means internally it will instantiated the streamer from the class and use it's default serialization properties.
    constructor CreateWithConnect(aStreamer: TDataObjStreamerClass; aConnectStr: AnsiString; aPrivateKey: ansiString = ''; aPublicKey: ansiString = ''); overload;
    constructor CreateWithConnect(aStreamer: TDataObjStreamerBase; aConnectStr: AnsiString; aPrivateKey: ansiString = ''; aPublicKey: ansiString = ''); overload;
    destructor Destroy; override;
    procedure Publish(aMessageType: word; aDataObj: TDataObj);
    property Streamer: TDataObjStreamerBase read fStreamer write fStreamer;    // reference only, not owned
  end;

  TDataObjZMQSubscriber = class(TZMQSubscriberBase)
  private
    fOnReceiveMessage: TReceiveDataObjMessageEvent;
    fStreamer: TDataObjStreamerBase;
  protected
    procedure HandleReceivedMessage(Sender: TObject; aZMQMsg: TZMQmsg); override;   // called by the base class when we have received a message.  Either called in it's own reading thread or called in the mainVCL thread depending on a property set in the base class.
  public
    constructor CreateWithConnect(aConnectStr: AnsiString; aSubscription: word; aStreamer: TDataObjStreamerBase; aReceiveMsgProc: TReceiveDataObjMessageEvent; aSynchronized: boolean; aServerPublicKey: ansiString); overload;
    property OnReceiveMessage: TReceiveDataObjMessageEvent read fOnReceiveMessage write fOnReceiveMessage;
    property Streamer: TDataObjStreamerBase read fStreamer write fStreamer;    // reference only, not owned
  end;


implementation

{ TZMQPublisher }

function SwapBytes(aValue: word): word;
begin
  result := ((aValue and $FF) shl 8) or ((aValue and $FF00) shr 8);
end;

constructor TDataObjZMQPublisher.CreateWithBinding(aBinding: AnsiString; aStreamer: TDataObjStreamerBase; aPrivateKey: ansiString = ''; aPublicKey: ansiString = '');
begin
  inherited Create;

  fStreamer := aStreamer;
  fOwnsStreamer := false;
  PrivateKey := aPrivateKey;
  PublicKey := aPublicKey;

  Bind(aBinding);
end;

constructor TDataObjZMQPublisher.CreateWithBinding(aBinding: AnsiString; aStreamer: TDataObjStreamerClass; aPrivateKey, aPublicKey: ansiString);
begin
  inherited Create;

  fStreamer := aStreamer.Create(nil);
  fOwnsStreamer := true;
  PrivateKey := aPrivateKey;
  PublicKey := aPublicKey;

  Bind(aBinding);
end;

constructor TDataObjZMQPublisher.CreateWithConnect(aStreamer: TDataObjStreamerBase; aConnectStr: AnsiString; aPrivateKey: ansiString = ''; aPublicKey: ansiString = '');
begin
  inherited Create;

  fStreamer := aStreamer;
  fOwnsStreamer := false;
  PrivateKey := aPrivateKey;
  PublicKey := aPublicKey;

  Connect(aConnectStr);
end;

destructor TDataObjZMQPublisher.Destroy;
begin
  if fOwnsStreamer then
    FreeAndNil(fStreamer);
  inherited;
end;

constructor TDataObjZMQPublisher.CreateWithConnect(aStreamer: TDataObjStreamerClass; aConnectStr: AnsiString; aPrivateKey, aPublicKey: ansiString);
begin
  inherited Create;

  fStreamer := aStreamer.Create(nil);
  fOwnsStreamer := true;
  fOwnsStreamer := false;
  PrivateKey := aPrivateKey;
  PublicKey := aPublicKey;

  Connect(aConnectStr);
end;

procedure TDataObjZMQPublisher.Publish(aMessageType: word; aDataObj: TDataObj);
var
  lMsg: TZMQMsg;
  lMsgTypeFrame: TZMQFrame;
  lMsgFrame: TZMQFrame;
  lMemStream: TMemoryStream;
begin
  lMsg:=TZMQMsg.create;
  try
    // create the message Type frame
    lMsgTypeFrame := TZMQFrame.create;
    lMsgTypeFrame.rebuild( 2 );
    word(lMsgTypeFrame.data^) := SwapBytes(aMessageType);
    lMsg.add(lMsgTypeFrame);

    // create the DataObject serialize content message
    lMsgFrame:=TZMQFrame.create;

    lMemStream:=TMemoryStream.Create;
    try
      fStreamer.Stream := lMemStream;
      fStreamer.Encode(aDataObj);            // Can't use a pointer stream because we don't know how big it will be until we get it encoded.
      lMsgFrame.rebuild(lMemStream.size);
      lMemStream.Seek(0,0);
      lMemStream.ReadData(lMsgFrame.data, lMemStream.size);
    finally
      lMemStream.Free;
    end;

    lMsg.add(lMsgFrame);

    // Now send the message.
    fPubSocket.send(lMsg);
  finally
    lMsg.free;    // the lMsg owns the two message frames so this will free those too.
  end;
end;

{ TZMQSubscriberDDO }
(*
procedure TSubscriberThread.Execute;
var
  lMessageType: word;    // two byte message type.

  function ReadDataObj(aDataObj: TDataObj): boolean;    // could return false if we got a bad message, or it could except if it's a bad ddo stream.
  var
    lZMessage: TZMQMsg;
  begin
    result := false;
    lZMessage:=TZMQMsg.create;
    try
      fSubSocket.recv( lZMessage, [] );     // this will put us into a blocking state until we receive a message or until the socket is closed and we return with an exception.
      if lZMessage.size >= 2 then                   // Need at least two parts to the message (message type and the DDO message)
      begin
        if lZMessage.first.size = 2 then
        begin
          lMessageType := word(lZMessage.first.data^);
          lMessageType := SwapBytes(lMessageType);
          aDataObj.ReadFromMemoryBuffer(lZMessage.last.data, lZMessage.last.size);
          result := true;
        end;
      end;
    finally
      lZMessage.Free;
    end;
  end;

begin
  fReceiveDDO:=TDataObj(TDataObj_CT179.Create);
  fSubSocket:=fContext.Socket(stSub);
  try
    fSubSocket.Subscribe(SwapBytes(fSubscribeMessageType));                     // subscribe to all published messages if zero, otherwise just that message type.
    fSubSocket.connect(fConnectStr);
    NameThreadForDebugging(Self.classname+': '+fConnectStr+' ('+IntToStr(fSubscribeMessageType)+')', ThreadID);

    while not terminated do
    begin
      try
        fReceiveDDO.clear;
        if ReadDataObj(fReceiveDDO) then
        begin
          fReceivedMsgType := lMessageType;
          fReceivedDataObj := fReceiveDDO;

          if fSynchronized then
            Synchronize(SynchReceiveMessage)
          else
            SynchReceiveMessage;
        end
        else
        begin
          // we read a message but something about it didn't comply with a receivable message such as maybe not having enough message parts.
          // FINISH - not sure what to do about this error condition.
        end;
      except
        // FINISH - not sure what to do about this error condition.
      end;
    end;
  finally
    fSubSocket.Free;
    fReceiveDDO.free;
  end;
end;   *)

constructor TDataObjZMQSubscriber.CreateWithConnect(aConnectStr: AnsiString; aSubscription: word; aStreamer: TDataObjStreamerBase; aReceiveMsgProc: TReceiveDataObjMessageEvent; aSynchronized: boolean; aServerPublicKey: ansiString);
begin
  inherited Create;
  fStreamer := aStreamer;
  fOnReceiveMessage := aReceiveMsgProc;
  Connect(aConnectStr, aSubscription, self.HandleReceivedMessage, aSynchronized, aServerPublicKey);
end;

procedure TDataObjZMQSubscriber.HandleReceivedMessage(Sender: TObject; aZMQMsg: TZMQmsg);    // called by the base class when we have received a message.  Either called in it's own reading thread or called in the mainVCL thread depending on a property set in the base class.
var
  lMessageType: word;
  lDataObj: TDataObj;
  lPointerStream: TPointerStream;
begin
  if assigned(fOnReceiveMessage) then
  begin
    // now that we received a ZMQ message, pull it apart looking for the pieces:  MessageHeader and the DataObject payload.
    if aZMQMsg.size >= 2 then                   // Need at least two parts to the message (message type and the DDO message)
    begin
      if aZMQMsg.first.size = 2 then
      begin
        lMessageType := word(aZMQMsg.first.data^);
        lMessageType := SwapBytes(lMessageType);

        lDataObj:=TDataObj(TDataObj_CT180.Create);
        lPointerStream := TPointerStream.Create(aZMQMsg.last.data, aZMQMsg.last.size,true);
        try
          fStreamer.Stream := lPointerStream;
          fStreamer.Decode(lDataObj);
          fOnReceiveMessage(self, lMessageType, lDataObj);
        finally
          lPointerStream.Free;
          lDataObj.Free;    // we've given the dataObject to the receiver so we can now free it.
        end;
      end;
    end;
  end;
end;

end.
