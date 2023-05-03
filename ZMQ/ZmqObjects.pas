unit ZmqObjects;

interface

uses classes, ZMQ, zmqapi, sysUtils;

type
  TZMQSubscriberBase = class;
  TZMQMessageEvent = procedure(Sender: TObject; aMsg: TZMQMsg) of object;
  TZMQSubscribeType = (cSubscribeWord, cSubscribeAnsiString);

  // This is a base class ZMQ publisher.  Descendants of this are then specific to a certain kind of payload.
  TZMQPublisherBase = class
  private
    FPrivateKey: ansiString;
    FPublicKey: ansiString;
    function getHWM: integer;
    procedure setHWM(const Value: integer);
    procedure SetPrivateKey(const Value: ansiString);
    procedure SetPublicKey(const Value: ansiString);
  protected
    fContext: TZMQContext;
    fPubSocket: TZMQSocket;
  public
    constructor Create;
    destructor Destroy; override;
    property HWM: integer read getHWM write setHWM;
    property PublicKey: ansiString read FPublicKey write SetPublicKey;
    property PrivateKey: ansiString read FPrivateKey write SetPrivateKey;
    procedure Bind(aBinding: AnsiString);
    procedure Connect(aConnectStr: AnsiString);
  end;

  // This thread is the executing thread that receives data from a ZMQ subscription.
  TSubscriberThread = class(TThread)      // This thread is for internal use only.
  private
    fOwner: TZMQSubscriberBase;
    fConnectStr: AnsiString;

    // vars used for synchronize
    fSynchronized: boolean;
    fReceivedMsgType: word;
    fZReceivedMessage: TZMQMsg;
    fSubscribeType: TZMQSubscribeType;
    fSubscribeAnsiString: ansiString;
    fSubscribeWord: word;
    procedure setSubscribeWord(const Value: word);
    procedure setSubscribeAnsiString(const Value: ansiString);   //The received message.
  protected
    fContext: TZMQContext;
    fSubSocket: TZMQSocket;
    procedure Execute; override;
    procedure SynchReceiveMessage;
    procedure TerminatedSet; override;
  public
    Constructor Create;
    destructor Destroy; override;

    property SubscribeType: TZMQSubscribeType read fSubscribeType write fSubscribeType;
    property SubscribeWord: word read fSubscribeWord write setSubscribeWord;
    property SubscribeAnsiString: ansiString read fSubscribeAnsiString write setSubscribeAnsiString;
  end;


  // This is a base class ZMQ subscriber.  Descendants of this are then specific to a certain kind of payload.
  {$M+}
  TZMQSubscriberBase = class
  private
    FServerPublicKey: ansiString;
    FClientPublicKey: ansiString;
    FClientPrivateKey: ansiString;
    procedure SetServerPublicKey(const Value: ansiString);
    procedure SetClientPrivateKey(const Value: ansiString);
    procedure SetClientPublicKey(const Value: ansiString);

  protected
    fThread: TSubscriberThread;
    fConnectStr: AnsiString;
    fOnReceiveMessage: TZMQMessageEvent;
    fSynchronized: Boolean;                    // if true, then the OnReceiveMessage callback is called through a "Synchonrize"  if false, then it's called from the receiving thread.

    procedure HandleReceivedMessage(Sender: TObject; aZMQMsg: TZMQmsg); virtual;   // called by the base class when we have received a message.  Either called in it's own reading thread or called in the mainVCL thread depending on a property set in the base class.
  published
    property ServerPublicKey: ansiString read FServerPublicKey write SetServerPublicKey;
    property ClientPublicKey: ansiString read FClientPublicKey write SetClientPublicKey;
    property ClientPrivateKey: ansiString read FClientPrivateKey write SetClientPrivateKey;
  public
    constructor CreateWithConnect(aConnectStr: AnsiString; aSubscription: word; aReceiveMsgProc: TZMQMessageEvent; aSynchronized: boolean; aServerPublicKey: ansiString = ''); overload;
    constructor CreateWithConnect(aConnectStr: AnsiString; aSubscription: ansiString; aReceiveMsgProc: TZMQMessageEvent; aSynchronized: boolean; aServerPublicKey: ansiString = ''); overload;
    destructor Destroy; override;
    procedure Connect(aConnectStr: AnsiString; aSubscription: word; aReceiveMsgProc: TZMQMessageEvent; aSynchronized: boolean; aServerPublicKey: ansiString = ''); overload;
    procedure Connect(aConnectStr: AnsiString; aSubscription: ansiString; aReceiveMsgProc: TZMQMessageEvent; aSynchronized: boolean; aServerPublicKey: ansiString = ''); overload;

    property OnReceiveMessage: TZMQMessageEvent read fOnReceiveMessage write fOnReceiveMessage;
  end;



  function SwapBytes(aValue: word): word;


implementation



{ TZMQPublisher }

function SwapBytes(aValue: word): word;
begin
  result := ((aValue and $FF) shl 8) or ((aValue and $FF00) shr 8);
end;

procedure TZMQPublisherBase.Bind(aBinding: AnsiString);
var
  lBoolean: LongBool;
begin
  // In order to make this TCP binding use a secure layer, we have to set some of the CURVE options on the socket.
  // If we have been given a public and private key, then setup the CURVE
  if (length(FPrivateKey)>0) and (length(fPublicKey)>0) then
  begin
    lBoolean := true;
    zmq_setsockopt(fPubSocket, ZMQ_CURVE_SERVER, @lBoolean, 4);
    zmq_setsockopt(fPubSocket, ZMQ_CURVE_SECRETKEY, @fPrivateKey[1], length(fPrivateKey));
    zmq_setsockopt(fPubSocket, ZMQ_CURVE_PUBLICKEY, @fPublicKey[1], length(fPublicKey));
  end;

  fPubSocket.bind(aBinding);
end;

procedure TZMQPublisherBase.Connect(aConnectStr: AnsiString);
begin
  fPubSocket.Connect(aConnectStr);
end;


constructor TZMQPublisherBase.Create;
begin
  inherited;
  fContext:=TZMQContext.create;
  fPubSocket := fContext.Socket(stPub);
end;

destructor TZMQPublisherBase.Destroy;
begin
  fPubSocket.Free;
  fContext.Free;
  inherited;
end;


function TZMQPublisherBase.getHWM: integer;
begin
  result := fPubSocket.getHWM;
end;

procedure TZMQPublisherBase.setHWM(const Value: integer);
begin
  fPubSocket.setHWM(Value);
end;

procedure TZMQPublisherBase.SetPrivateKey(const Value: ansiString);
begin
  FPrivateKey := Value;
end;

procedure TZMQPublisherBase.SetPublicKey(const Value: ansiString);
begin
  FPublicKey := Value;
end;

{ TZMQSubscriberBase }

procedure TZMQSubscriberBase.Connect(aConnectStr, aSubscription: ansiString; aReceiveMsgProc: TZMQMessageEvent; aSynchronized: boolean; aServerPublicKey: ansiString = '');
begin
  fConnectStr := aConnectStr;
  fOnReceiveMessage := aReceiveMsgProc;
  fSynchronized := aSynchronized;
  FServerPublicKey := aServerPublicKey;

  fThread := TSubscriberThread.create;     // thread should not be freeOnTerminate cause we need to do some work on the destroy.
  fThread.fConnectStr := fConnectStr;
  fThread.fOwner := self;
  fThread.fSubscribeAnsiString := aSubscription;
//  fThread.OnReceiveMessage := ReceiveMessage;
  fThread.Start;
end;

procedure TZMQSubscriberBase.Connect(aConnectStr: AnsiString; aSubscription: word; aReceiveMsgProc: TZMQMessageEvent; aSynchronized: boolean; aServerPublicKey: ansiString = '');
begin
  fConnectStr := aConnectStr;
  fOnReceiveMessage := aReceiveMsgProc;
  fSynchronized := aSynchronized;
  FServerPublicKey := aServerPublicKey;

  fThread := TSubscriberThread.create;     // thread should not be freeOnTerminate cause we need to do some work on the destroy.
  fThread.fConnectStr := fConnectStr;
  fThread.fOwner := self;
  fThread.fSubscribeWord := aSubscription;
//  fThread.OnReceiveMessage := ReceiveMessage;
  fThread.Start;
end;

constructor TZMQSubscriberBase.CreateWithConnect(aConnectStr, aSubscription: ansiString; aReceiveMsgProc: TZMQMessageEvent; aSynchronized: boolean; aServerPublicKey: ansiString = '');
begin
  inherited Create;
  Connect(aConnectStr, aSubscription, aReceivemsgProc, aSynchronized);
end;

constructor TZMQSubscriberBase.CreateWithConnect(aConnectStr: AnsiString; aSubscription: word; aReceiveMsgProc: TZMQMessageEvent; aSynchronized: boolean; aServerPublicKey: ansiString = '');
begin
  inherited Create;
  Connect(aConnectStr, aSubscription, aReceivemsgProc, aSynchronized);
end;

destructor TZMQSubscriberBase.Destroy;
begin
  // need to tell the receiving thread that we want to terminate;
  fThread.terminate;
  fThread.WaitFor;
  fThread.free;

  inherited;
end;

procedure TZMQSubscriberBase.HandleReceivedMessage(Sender: TObject; aZMQMsg: TZMQmsg);
begin
  // by default, unless we are overridden, we will just send this back to an event handler.  Usually, a descendant will get this call instead.
  if assigned(fOnReceiveMessage) then
    fOnReceiveMessage(Self, aZMQMsg);
end;


procedure TZMQSubscriberBase.SetClientPrivateKey(const Value: ansiString);
begin
  FClientPrivateKey := Value;
end;

procedure TZMQSubscriberBase.SetClientPublicKey(const Value: ansiString);
begin
  FClientPublicKey := Value;
end;

procedure TZMQSubscriberBase.SetServerPublicKey(const Value: ansiString);
begin
  FServerPublicKey := Value;
end;

constructor TSubscriberThread.Create;
begin
  inherited Create(true);            // creating suspended.

  fContext:=TZMQContext.create;      // The context is
end;

destructor TSubscriberThread.Destroy;
begin
  fContext.free;             // destroy the context.
  inherited;
end;

procedure TSubscriberThread.Execute;

(*  function ReadDataObj(aDataObj: TDataObj): boolean;    // could return false if we got a bad message, or it could except if it's a bad ddo stream.
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
  end;  *)

begin
  fSubSocket:=fContext.Socket(stSub);
  try

    if (length(fOwner.ServerPublicKey)>0) then
    begin
      zmq_setsockopt(fSubSocket, ZMQ_CURVE_SERVERKEY, @fOwner.FServerPublicKey[1], length(fOwner.FServerPublicKey));
      zmq_setsockopt(fSubSocket, ZMQ_CURVE_SECRETKEY, @fOwner.FClientPrivateKey[1], length(fOwner.FClientPrivateKey));
      zmq_setsockopt(fSubSocket, ZMQ_CURVE_PUBLICKEY, @fOwner.FClientPublicKey[1], length(fOwner.FClientPublicKey));
    end;


    if fSubscribeType = cSubscribeAnsiString then
    begin
      fSubSocket.Subscribe(fSubscribeAnsiString);
      NameThreadForDebugging(Self.classname+': '+fConnectStr+' ('+fSubscribeAnsiString+')', ThreadID);
    end
    else
    begin
      fSubSocket.Subscribe(SwapBytes(fSubscribeWord));                     // subscribe to all published messages if zero, otherwise just that message type.
      NameThreadForDebugging(Self.classname+': '+fConnectStr+' ('+IntToStr(fSubscribeWord)+')', ThreadID);
    end;

    fSubSocket.connect(fConnectStr);

    while not terminated do
    begin
      try
        fZReceivedMessage:=TZMQMsg.create;
        try
          fSubSocket.recv( fZReceivedMessage, [] );             // this will put us into a blocking state until we receive a message or until the socket is closed and we return with an exception.

          if fZReceivedMessage.size >= 2 then                   // Need at least two parts to the message (message type and the DDO message)
          begin
            if fZReceivedMessage.first.size = 2 then
            begin
              fReceivedMsgType := SwapBytes(word(fZReceivedMessage.first.data^));
              // We received a good message so pass it to the owning object
              if fSynchronized then
                Synchronize(SynchReceiveMessage)
              else
                SynchReceiveMessage;
            end;
          end;
          // FINISH - what do we do if the message didn't have two parts or if the message had a bad first part.
        finally
          fZReceivedMessage.free;    // this message has been received and handled so free it as we'll get a new message object for the next iteration.
        end;
      except
        // FINISH - not sure what to do about this error condition.
      end;
    end;
  finally
    fSubSocket.Free;
  end;
end;


procedure TSubscriberThread.setSubscribeAnsiString(const Value: ansiString);
begin
  fSubscribeAnsiString := Value;
  fSubscribeType := cSubscribeAnsiString;
end;

procedure TSubscriberThread.setSubscribeWord(const Value: word);
begin
  fSubscribeWord := Value;
  fSubscribeType := cSubscribeWord;
end;


procedure TSubscriberThread.SynchReceiveMessage;
begin
  fOwner.HandleReceivedMessage(fOwner, fZReceivedMessage);   // give the received message up to the subscriber object to deal with.
end;

procedure TSubscriberThread.TerminatedSet;
begin
  inherited;

  // we need to notify the ZMQ that the read call that we are probably in a blocking state in has to be returned from.
  // it will do so with an exception, but the thread execute loop handles that.
  // in ZMQ4, this is a good way to do this so we can tear down without having to wait for a message to be received.
  fContext.shutdown;
end;


end.
