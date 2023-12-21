unit PerformanceTesterFrm;

{This test project will create a sample DataObject payload and then stream it many times to and from a stream to test the performance of each of the
 streamer classes included in this test case.  Then, basic statistics are published for each of the streamer classes' tests. }

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, DataObjects2, DateUtils, DataObjects2Streamers,
  DataObjects2CBOR, DataObjects2DDO, DataObjects2ION, DataObjects2JSON, DataObjects2MsgPack,
  DataObjects2BSON, DataObjects2UBJSON, DataObjects2Smile, DataObjects2CSV, VarInt, math,
  StringBTree;

type
  TForm19 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Button4: TButton;
    Button2: TButton;
    Button5: TButton;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    procedure MakeTestObject(aObj: TDataObj);
    procedure log(aStr: string);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form19: TForm19;

implementation

{$R *.dfm}

//returns the number of differences.
function CompareDataObjects(aDataObj1, aDataObj2: TDataObj; aConsiderSameIfDataTypesDifferent: boolean): integer;
begin
  result := 0;  //FINISH
end;


function MakeString(aSize: integer): string;
var
  i: Integer;
  lCharVal: word;
begin
  setLength(result, aSize);
  for i := 1 to aSize do
  begin
    lCharVal := (i mod (65535-32))+32;
    result[i] := char(lCharVal);
  end;
end;

function MakeRandomTypicalString(aSize: integer): string;
var
  i: Integer;
  // Most characters are 48 to 122.
  function GenChar: char;
  var
    lVal: byte;
  begin
    lVal := random (122-48)+48;
    result := char(lVal);
  end;
begin
  setLength(result, aSize);
  for i := 1 to aSize do
  begin
    result[i] := GenChar;
  end;
end;



procedure TForm19.Button1Click(Sender: TObject);
var
  i: integer;
  lDataObj: TDataObj;
  lDataObj2: TDataObj;
  lMemStream: TMemoryStream;
  lCount: integer;
  lStreamer: TDataObjStreamerBase;

  procedure m(aStr: string);
  begin
    memo1.Lines.Add(aStr);
  end;

  function PubTime(aTimeDiff: TDatetime): string;
  begin
    result := FloatToStrf(aTimeDiff*24*60*60, ffFixed, 10, 3);
  end;

  procedure PerformTest(aStreamer: TDataObjStreamerBase; aTestCount: integer; aExtraLabel: string = ' ');
  var
    i: integer;
    lStart: TDateTime;
    lStart2: TDateTime;
    lEncodeTime: TDateTime;
    lDecodetime: TDatetime;
    lReadDataObj: TDataObj;
    lEncodesize: integer;
    lStr: string;
  begin
    // Create the given streamer class and we will perform some tests on it to see how it performs.
    lMemStream.SetSize(0);
    aStreamer.Stream := lMemStream;

    lStart := now;
    try
      // Do a performance test for encoding to this format aTestCount times
      for i := 1 to aTestCount do
      begin
        lMemStream.seek(0,soBeginning);
        aStreamer.Encode(lDataObj);
      end;
      lEncodeTime := now;

      // Save this last test stream to a file.
      ForceDirectories('./TestOutputs');
      lMemStream.SaveToFile('./TestOutputs/'+aStreamer.ClassName+'.'+aStreamer.FileExtension);
      lEncodeSize := lMemStream.Size;
    except
      on e: exception do
        m(e.message);
    end;


    // Do a performance test for decoding from this format aTestCount times
    lStart2 := now;
    try
      lReadDataObj := TDataObj.Create;
      try
        try
          for i := 1 to aTestCount do
          begin
            lReadDataObj.Clear;           // note:  we are using up some time here doing the clear so the test will have a bit more time in it.
            lMemStream.Seek(0, soBeginning);
            aStreamer.Stream := lMemStream;
            aStreamer.Decode(lReadDataObj);
          end;

        except
          on e: exception do
          begin
            m(aStreamer.classname+':  Decoding Error: '+e.Message);
          end;
        end;
        lDecodeTime := now;
      finally
        lReadDataObj.Free;
      end;
    except
      on e: exception do
        m(e.message);
    end;

    // Now publish our timing results
    lStr := '';
    if aStreamer is TDataObjStreamer then
    begin
      if TDataObjStreamer(lStreamer).UseStringRefs then
      begin
        lStr := ', StringRefCount='+IntToStr(TDataObjStreamer(lStreamer).StringRefWriteCount);
      end;
      if TDataObjStreamer(lStreamer).UseSlotnameRefs then
      begin
        lStr := lStr+', SlotnameRefCount='+IntToStr(TDataObjStreamer(lStreamer).SlotnameRefWriteCount);
      end;
    end;
    m(aStreamer.classname+aExtraLabel+':  Encode in '+PubTime(lEncodeTime-lStart)+' seconds, Decode in '+PubTime(lDecodeTime-lStart2)+' seconds, Size='+InttoStr(lMemStream.Size)+lStr);

  end;

  procedure PerformTest3(aStreamer: TDataObjStreamerBase; aTestCount: integer; aExtraLabel: string = ' ');
  var
    lStart: TDateTime;
    lEncodeTime: TDateTime;
    lStr: string;
  begin
    try
      // Now make a bigger test where we only serialize once, but we are serializing a pretty big array of objects
      lMemStream.SetSize(0);
      lStart := now;
      lMemStream.seek(0,soBeginning);
      aStreamer.Encode(lDataObj);
      lEncodetime := now-lStart;
      lMemStream.seek(0,soBeginning);
      lMemStream.SaveToFile('./TestOutputs/'+aStreamer.ClassName+'_BIG.'+aStreamer.FileExtension);
    except
      on e: exception do
        m(e.message);
    end;


    // Now publish our timing results
    lStr := '';
    if aStreamer is TDataObjStreamer then
    begin
      if TDataObjStreamer(lStreamer).UseStringRefs then
      begin
        lStr := ', StringRefCount='+IntToStr(TDataObjStreamer(lStreamer).StringRefWriteCount);
      end;
      if TDataObjStreamer(lStreamer).UseSlotnameRefs then
      begin
        lStr := lStr+', SlotnameRefCount='+IntToStr(TDataObjStreamer(lStreamer).SlotnameRefWriteCount);
      end;
    end;

    m(aStreamer.classname+aExtraLabel+':  BigEncode in '+PubTime(lEncodeTime)+' seconds, Size='+InttoStr(lMemStream.Size)+lStr);
  end;


  procedure PerformTest2(aStreamer: TDataObjStreamerBase; aTestCount: integer; aExtraLabel: string = ' ');
  var
    i: integer;
    lStart: TDateTime;
    lStart2: TDateTime;
    lEncodeTime: TDateTime;
    lDecodeTime: TDateTime;
    lEncodeSize: integer;
    lReadDataObj: TDataObj;
    lStr: string;
  begin
    lMemStream.SetSize(0);
    aStreamer.Stream := lMemStream;

    lStart := now;
    try
      // Do a performance test for encoding to this format aTestCount times
      for i := 1 to aTestCount do
      begin
        lMemStream.seek(0,soBeginning);
        aStreamer.Encode(lDataObj);
      end;
      lEncodeTime := now;

      // Save this last test stream to a file.
//        ForceDirectories('./TestOutputs');
//        lMemStream.SaveToFile('./TestOutputs/'+lStreamer.ClassName+'.'+lStreamer.FileExtension);
      lEncodeSize := lMemStream.Size;
    except
      on e: exception do
        m(e.message);
    end;


    // Do a performance test for decoding from this format aTestCount times
    lStart2 := now;
    try
      lReadDataObj := TDataObj.Create;
      try
        try
          for i := 1 to aTestCount do
          begin
            lReadDataObj.Clear;           // note:  we are using up some time here doing the clear so the test will have a bit more time in it.
            lMemStream.Seek(0, soBeginning);
            aStreamer.Stream := lMemStream;
            aStreamer.Decode(lReadDataObj);
          end;

        except
          on e: exception do
          begin
            m(aStreamer.classname+':  Decoding Error: '+e.Message);
          end;
        end;
        lDecodeTime := now;
      finally
        lReadDataObj.Free;
      end;
    except
      on e: exception do
        m(e.message);
    end;

    // Now publish our timing results
    lStr := '';
    if aStreamer is TDataObjStreamer then
    begin
      if TDataObjStreamer(lStreamer).UseStringRefs then
      begin
        lStr := ', StringRefCount='+IntToStr(TDataObjStreamer(lStreamer).StringRefWriteCount);
      end;
      if TDataObjStreamer(lStreamer).UseSlotnameRefs then
      begin
        lStr := lStr+', SlotnameRefCount='+IntToStr(TDataObjStreamer(lStreamer).SlotnameRefWriteCount);
      end;
    end;

    m(aStreamer.classname+aExtraLabel+': Encoded in '+PubTime(lEncodeTime-lStart)+' seconds, Decoded in '+PubTime(lDecodeTime-lStart2)+' seconds,  Size='+IntToStr(lEncodesize)+lStr);

  end;

begin
  lDataObj:=TDataObj.create;
  lMemStream:=TMemoryStream.Create;
  try

    lCount := 100000;
    // Make our test dataobject that we will use for serializing to many different formats
    MakeTestObject(lDataObj);

    m('TEST #1: Small objects without repetitive slotnames or string values. Test is done '+inttostr(lCount)+' times.');

    lStreamer := TDataObjStreamer.Create(lMemStream);
    TDataObjStreamer(lStreamer).UseStringRefs := false;
    TDataObjStreamer(lStreamer).UseSlotnameRefs := false;
    TDataObjStreamer(lStreamer).SerializeAsUTF8 := false;
    PerformTest(lStreamer, lCount, ' USR=false,USN=false,UNICODE');
    FreeAndNil(lStreamer);

    lStreamer := TDataObjStreamer.Create(lMemStream);
    TDataObjStreamer(lStreamer).UseStringRefs := true;
    TDataObjStreamer(lStreamer).UseSlotnameRefs := false;
    TDataObjStreamer(lStreamer).SerializeAsUTF8 := false;
    PerformTest(lStreamer, lCount, ' USR=true,USN=false,UNICODE');
    FreeAndNil(lStreamer);

    lStreamer := TDataObjStreamer.Create(lMemStream);
    TDataObjStreamer(lStreamer).UseStringRefs := false;
    TDataObjStreamer(lStreamer).UseSlotnameRefs := false;
    TDataObjStreamer(lStreamer).SerializeAsUTF8 := true;
    PerformTest(lStreamer, lCount, ' USR=false,USN=false,UTF8');
    FreeAndNil(lStreamer);

    lStreamer := TDataObjStreamer.Create(lMemStream);
    TDataObjStreamer(lStreamer).UseStringRefs := true;
    TDataObjStreamer(lStreamer).UseSlotnameRefs := false;
    TDataObjStreamer(lStreamer).SerializeAsUTF8 := true;
    PerformTest(lStreamer, lCount, ' USR=true,USN=false,UTF8');
    FreeAndNil(lStreamer);

    lStreamer := TDataObjStreamer.Create(lMemStream);
    TDataObjStreamer(lStreamer).UseStringRefs := false;
    TDataObjStreamer(lStreamer).UseSlotnameRefs := true;
    TDataObjStreamer(lStreamer).SerializeAsUTF8 := false;
    PerformTest(lStreamer, lCount, ' USR=false,USN=true,UNICODE');
    FreeAndNil(lStreamer);

    lStreamer := TDataObjStreamer.Create(lMemStream);
    TDataObjStreamer(lStreamer).UseStringRefs := true;
    TDataObjStreamer(lStreamer).UseSlotnameRefs := true;
    TDataObjStreamer(lStreamer).SerializeAsUTF8 := false;
    PerformTest(lStreamer, lCount, ' USR=true,USN=true,UNICODE');
    FreeAndNil(lStreamer);

    lStreamer := TDataObjStreamer.Create(lMemStream);
    TDataObjStreamer(lStreamer).UseStringRefs := false;
    TDataObjStreamer(lStreamer).UseSlotnameRefs := true;
    TDataObjStreamer(lStreamer).SerializeAsUTF8 := true;
    PerformTest(lStreamer, lCount, ' USR=false,USN=true,UTF8');
    FreeAndNil(lStreamer);

    lStreamer := TDataObjStreamer.Create(lMemStream);
    TDataObjStreamer(lStreamer).UseStringRefs := true;
    TDataObjStreamer(lStreamer).UseSlotnameRefs := true;
    TDataObjStreamer(lStreamer).SerializeAsUTF8 := true;
    PerformTest(lStreamer, lCount, ' USR=true,USN=true,UTF8');
    FreeAndNil(lStreamer);


    lStreamer := TDDOStreamer.Create(lMemStream);
    PerformTest(lStreamer, lCount);
    FreeAndNil(lStreamer);

    lStreamer :=TJsonStreamer.Create(lMemStream);
    PerformTest(lStreamer, lCount);
    FreeAndNil(lStreamer);

    lStreamer := TCBORStreamer.Create(lMemStream);
    PerformTest(lStreamer, lCount);
    FreeAndNil(lStreamer);

    lStreamer := TBSONStreamer.Create(lMemStream);
    PerformTest(lStreamer, lCount);
    FreeAndNil(lStreamer);

    lStreamer := TUBJsonStreamer.Create(lMemStream);
    PerformTest(lStreamer, lCount);
    FreeAndNil(lStreamer);

    lStreamer := TMsgPackStreamer.Create(lMemStream);
    PerformTest(lStreamer, lCount);
    FreeAndNil(lStreamer);

    lStreamer := TSmileStreamer.Create(lMemStream);
    PerformTest(lStreamer, lCount);
    FreeAndNil(lStreamer);

//    PerformTest(TCSVStreamer, lCount);   The CSV Streamer works, but it is geared to only work with data that is structured as an array of frames, which this test is not.
//    PerformTest(TIonStreamer);  The ION Streamer is Not ready enough yet.
    m('DONE with Test #1');
    m('');
    m('');




    lCount := 2;
    m('TEST #2: Large collections of objects with repetitive slotnames and string values. Test is done '+inttostr(lCount)+' times.');
    lDataObj.Clear;
    // Try another bigger object test
    lDataObj.LoadFromFile('c:\temp\MiniremListener.ddo');

    lStreamer := TDataObjStreamer.Create(lMemStream);
    TDataObjStreamer(lStreamer).UseStringRefs := false;
    TDataObjStreamer(lStreamer).UseSlotnameRefs := false;
    TDataObjStreamer(lStreamer).SerializeAsUTF8 := false;
    PerformTest2(lStreamer, lCount, ' USR=false,USN=false,UNICODE');
    FreeAndNil(lStreamer);

    lStreamer := TDataObjStreamer.Create(lMemStream);
    TDataObjStreamer(lStreamer).UseStringRefs := true;
    TDataObjStreamer(lStreamer).UseSlotnameRefs := false;
    TDataObjStreamer(lStreamer).SerializeAsUTF8 := false;
    PerformTest2(lStreamer, lCount, ' USR=true,USN=false,UNICODE');
    FreeAndNil(lStreamer);

    lStreamer := TDataObjStreamer.Create(lMemStream);
    TDataObjStreamer(lStreamer).UseStringRefs := false;
    TDataObjStreamer(lStreamer).UseSlotnameRefs := false;
    TDataObjStreamer(lStreamer).SerializeAsUTF8 := true;
    PerformTest2(lStreamer, lCount, ' USR=false,USN=false,UTF8');
    FreeAndNil(lStreamer);

    lStreamer := TDataObjStreamer.Create(lMemStream);
    TDataObjStreamer(lStreamer).UseStringRefs := true;
    TDataObjStreamer(lStreamer).UseSlotnameRefs := false;
    TDataObjStreamer(lStreamer).SerializeAsUTF8 := true;
    PerformTest2(lStreamer, lCount, ' USR=true,USN=false,UTF8');
    FreeAndNil(lStreamer);

    lStreamer := TDataObjStreamer.Create(lMemStream);
    TDataObjStreamer(lStreamer).UseStringRefs := false;
    TDataObjStreamer(lStreamer).UseSlotnameRefs := true;
    TDataObjStreamer(lStreamer).SerializeAsUTF8 := false;
    PerformTest2(lStreamer, lCount, ' USR=false,USN=true,UNICODE');
    FreeAndNil(lStreamer);

    lStreamer := TDataObjStreamer.Create(lMemStream);
    TDataObjStreamer(lStreamer).UseStringRefs := true;
    TDataObjStreamer(lStreamer).UseSlotnameRefs := true;
    TDataObjStreamer(lStreamer).SerializeAsUTF8 := false;
    PerformTest2(lStreamer, lCount, ' USR=true,USN=true,UNICODE');
    FreeAndNil(lStreamer);

    lStreamer := TDataObjStreamer.Create(lMemStream);
    TDataObjStreamer(lStreamer).UseStringRefs := false;
    TDataObjStreamer(lStreamer).UseSlotnameRefs := true;
    TDataObjStreamer(lStreamer).SerializeAsUTF8 := true;
    PerformTest2(lStreamer, lCount, ' USR=false,USN=true,UTF8');
    FreeAndNil(lStreamer);

    lStreamer := TDataObjStreamer.Create(lMemStream);
    TDataObjStreamer(lStreamer).UseStringRefs := true;
    TDataObjStreamer(lStreamer).UseSlotnameRefs := true;
    TDataObjStreamer(lStreamer).SerializeAsUTF8 := true;
    PerformTest2(lStreamer, lCount, ' USR=true,USN=true,UTF8');
    FreeAndNil(lStreamer);


    lStreamer :=TDDOStreamer.Create(lMemStream);
    PerformTest2(lStreamer, lCount);
    FreeAndNil(lStreamer);

    lStreamer := TJsonStreamer.Create(lMemStream);
    PerformTest2(lStreamer, lCount);
    FreeAndNil(lStreamer);

    lStreamer := TCBORStreamer.Create(lMemStream);
    PerformTest2(lStreamer, lCount);
    FreeAndNil(lStreamer);

    lStreamer := TBSONStreamer.Create(lMemStream);
    PerformTest2(lStreamer, lCount);
    FreeAndNil(lStreamer);

    lStreamer :=TUBJsonStreamer.Create(lMemStream);
    PerformTest2(lStreamer, lCount);
    FreeAndNil(lStreamer);

    lStreamer := TMsgPackStreamer.Create(lMemStream);
    PerformTest2(lStreamer, lCount);
    FreeAndNil(lStreamer);

    lStreamer := TSmileStreamer.Create(lMemStream);
    PerformTest2(lStreamer, lCount);
    FreeAndNil(lStreamer);

    m('DONE with Test #2');
    m('');
    m('');



    lCount := 10;
    m('TEST #3: Collections of 10,000 repeated objects with repetitive slotnames and string values. Test is done '+inttostr(lCount)+' times.');
    lDataObj.Clear;
    lDataObj2 := TDataObj.Create;
    try
      MakeTestObject(lDataObj2);
      // Make a bigger test dataObject
      for i := 1 to 10000 do
      begin
        lDataObj.AsArray.newslot.CopyFrom(lDataObj2);
      end;
    finally
      lDataObj2.free;
    end;


    lStreamer := TDataObjStreamer.Create(lMemStream);
    TDataObjStreamer(lStreamer).UseStringRefs := false;
    TDataObjStreamer(lStreamer).UseSlotnameRefs := false;
    TDataObjStreamer(lStreamer).SerializeAsUTF8 := false;
    PerformTest3(lStreamer, lCount, ' USR=false,USN=false,UNICODE');
    FreeAndNil(lStreamer);

    lStreamer := TDataObjStreamer.Create(lMemStream);
    TDataObjStreamer(lStreamer).UseStringRefs := true;
    TDataObjStreamer(lStreamer).UseSlotnameRefs := false;
    TDataObjStreamer(lStreamer).SerializeAsUTF8 := false;
    PerformTest3(lStreamer, lCount, ' USR=true,USN=false,UNICODE');
    FreeAndNil(lStreamer);

    lStreamer := TDataObjStreamer.Create(lMemStream);
    TDataObjStreamer(lStreamer).UseStringRefs := false;
    TDataObjStreamer(lStreamer).UseSlotnameRefs := false;
    TDataObjStreamer(lStreamer).SerializeAsUTF8 := true;
    PerformTest3(lStreamer, lCount, ' USR=false,USN=false,UTF8');
    FreeAndNil(lStreamer);

    lStreamer := TDataObjStreamer.Create(lMemStream);
    TDataObjStreamer(lStreamer).UseStringRefs := true;
    TDataObjStreamer(lStreamer).UseSlotnameRefs := false;
    TDataObjStreamer(lStreamer).SerializeAsUTF8 := true;
    PerformTest3(lStreamer, lCount, ' USR=true,USN=false,UTF8');
    FreeAndNil(lStreamer);


    lStreamer := TDataObjStreamer.Create(lMemStream);
    TDataObjStreamer(lStreamer).UseStringRefs := false;
    TDataObjStreamer(lStreamer).UseSlotnameRefs := true;
    TDataObjStreamer(lStreamer).SerializeAsUTF8 := false;
    PerformTest3(lStreamer, lCount, ' USR=false,USN=true,UNICODE');
    FreeAndNil(lStreamer);

    lStreamer := TDataObjStreamer.Create(lMemStream);
    TDataObjStreamer(lStreamer).UseStringRefs := true;
    TDataObjStreamer(lStreamer).UseSlotnameRefs := true;
    TDataObjStreamer(lStreamer).SerializeAsUTF8 := false;
    PerformTest3(lStreamer, lCount, ' USR=true,USN=true,UNICODE');
    FreeAndNil(lStreamer);

    lStreamer := TDataObjStreamer.Create(lMemStream);
    TDataObjStreamer(lStreamer).UseStringRefs := false;
    TDataObjStreamer(lStreamer).UseSlotnameRefs := true;
    TDataObjStreamer(lStreamer).SerializeAsUTF8 := true;
    PerformTest3(lStreamer, lCount, ' USR=false,USN=true,UTF8');
    FreeAndNil(lStreamer);

    lStreamer := TDataObjStreamer.Create(lMemStream);
    TDataObjStreamer(lStreamer).UseStringRefs := true;
    TDataObjStreamer(lStreamer).UseSlotnameRefs := true;
    TDataObjStreamer(lStreamer).SerializeAsUTF8 := true;
    PerformTest3(lStreamer, lCount, ' USR=true,USN=true,UTF8');
    FreeAndNil(lStreamer);


    lStreamer :=TDDOStreamer.Create(lMemStream);
    PerformTest3(lStreamer, lCount);
    FreeAndNil(lStreamer);

    lStreamer := TJsonStreamer.Create(lMemStream);
    PerformTest3(lStreamer, lCount);
    FreeAndNil(lStreamer);

    lStreamer := TCBORStreamer.Create(lMemStream);
    PerformTest3(lStreamer, lCount);
    FreeAndNil(lStreamer);

    lStreamer := TBSONStreamer.Create(lMemStream);
    PerformTest3(lStreamer, lCount);
    FreeAndNil(lStreamer);

    lStreamer :=TUBJsonStreamer.Create(lMemStream);
    PerformTest3(lStreamer, lCount);
    FreeAndNil(lStreamer);

    lStreamer := TMsgPackStreamer.Create(lMemStream);
    PerformTest3(lStreamer, lCount);
    FreeAndNil(lStreamer);

    lStreamer := TSmileStreamer.Create(lMemStream);
    PerformTest3(lStreamer, lCount);
    FreeAndNil(lStreamer);


    m('DONE with Test #3');



  finally
    lMemStream.Free;
    lDataObj.Free;
  end;
end;


// MakeTestObject does just what it says.  It will fill aObj with a buch of test data.
procedure TForm19.Button2Click(Sender: TObject);
var
  lStr: string;
  lDataObj: TDataObj;
  lArray: TDataArray;
  lStrings: TDataStringList;
  i: integer;

  procedure BinaryFileCompare(aF1, aF2: string);
  var
    lFS1, lFS2: TMemoryStream;
    lLen: integer;
    i: integer;
    lFirstChangeIndex: integer;
    lChangeCount: integer;
    lP1, lP2: PByte;
  begin
    log(aF1);

    lFS1:=TMemoryStream.create;
    lFS2:=TMemoryStream.create;
    lFS1.LoadFromFile(aF1);
    lFS2.LoadFromFile(aF2);

    lLen := min(lFS1.Size, lFS2.Size);


    if lFS1.Size <> lFS2.size then
    begin
      log('Error: Sizes are different:  '+InttoStr(lFS1.Size)+', '+InttoStr(lFS2.Size));
    end;

    lFirstChangeIndex := -1;
    lChangeCount:=0;
    lP1 := lFS1.Memory;
    lP2 := lFS2.Memory;
    for i := 0 to lLen-1 do
    begin
      if lP1^ <> lP2^ then
      begin
        if lFirstChangeIndex = -1 then
          lFirstChangeIndex := i;
        inc(lChangeCount);
      end;

      inc(lP1);
      inc(lP2);
    end;

    log('There were '+IntToStr(lChangeCount)+' differences starting at '+inttoStr(lFirstChangeIndex));
    log('');



  end;

begin
  // This test will test a bunch of strings going in and out of dataObject serializations.
  // This is to exercise a variety of sizes and unicode characters.

  lDataObj:=TDataObj.Create;
  try
    lArray := lDataObj.AsFrame.newslot('Array').AsArray;
    lStrings := lDataObj.AsFrame.newslot('strings').AsStringList;

    i := 1;
    while i < 1000000 do
    begin
      lStr := MakeString(i);
      lArray.NewSlot.AsString := lStr;
      lStrings.Add(lStr);
      i := i*2;
    end;

    lDataObj.WriteToFile('c:\temp\SetOfStrings.dataObj');
    lDataObj.WriteToFile('c:\temp\SetOfStrings.ddo');
    lDataObj.WriteToFile('c:\temp\SetOfStrings.json');
    lDataObj.WriteToFile('c:\temp\SetOfStrings.cbor');
    lDataObj.WriteToFile('c:\temp\SetOfStrings.bson');

    lDataObj.Clear;
    lDataObj.LoadFromFile('c:\temp\SetOfStrings.dataObj');
    lDataObj.WriteToFile('c:\temp\SetOfStrings2.dataObj');

    lDataObj.Clear;
    lDataObj.LoadFromFile('c:\temp\SetOfStrings.ddo');
    lDataObj.WriteToFile('c:\temp\SetOfStrings2.ddo');

    lDataObj.Clear;
    lDataObj.LoadFromFile('c:\temp\SetOfStrings.json');
    lDataObj.WriteToFile('c:\temp\SetOfStrings2.json');

    lDataObj.Clear;
    lDataObj.LoadFromFile('c:\temp\SetOfStrings.cbor');
    lDataObj.WriteToFile('c:\temp\SetOfStrings2.cbor');

    lDataObj.Clear;
    lDataObj.LoadFromFile('c:\temp\SetOfStrings.bson');
    lDataObj.WriteToFile('c:\temp\SetOfStrings2.bson');

    BinaryFileCompare('c:\temp\SetOfStrings.dataObj', 'c:\temp\SetOfStrings2.dataObj');
    BinaryFileCompare('c:\temp\SetOfStrings.ddo', 'c:\temp\SetOfStrings2.ddo');
    BinaryFileCompare('c:\temp\SetOfStrings.json', 'c:\temp\SetOfStrings2.json');
    BinaryFileCompare('c:\temp\SetOfStrings.cbor', 'c:\temp\SetOfStrings2.cbor');
    BinaryFileCompare('c:\temp\SetOfStrings.bson', 'c:\temp\SetOfStrings2.bson');

    log('done');
  finally
    lDataObj.Free;
  end;
end;



procedure TForm19.Button3Click(Sender: TObject);
var
  n: integer;
  i: integer;
  k: integer;
  lStr: string;
  lSB: TStringBinaryTree;
  lNode: TStringBinaryTreeNode;
  lList: TStringList;
  lIndex: integer;
  lStart: TDateTime;
  lMasterlist: TStringList;
  lLoopCount: integer;
begin
  // This test compares performance between using a simple TStringList and a StringBTree for string ref adding and searching.
  // This test performs relatively small sizes of lists, but repeated many, many times to give the time some scale.
  lMasterlist:=TStringList.create;
  try
    for i := 0 to 500 do
    begin
      lStr := MakeRandomTypicalString(100);
      lMasterlist.Add(lStr);
    end;

    lLoopCount:=1000;
    n := 4;
    while n<= 100 do
    begin

      lStart:=now;
      for k := 1 to lLoopCount do    // number of times to run the test.
      begin
        lSB:=TStringBinaryTree.create;
        try
          // Populate the StringTree
          gInsBalanceCount := 0;
          for i := 0 to n-1 do
          begin
            lSB.AddString(lMasterList.Strings[i], i);
          end;

          // Testing finding in the BinaryTree
          for i := 0 to n-1 do
          begin
            lNode := lSB.FindNode(lMasterList.Strings[i]);
            if not assigned(lNode) then
            begin
              log('ERROR: NOT ABLE TO FIND STRING IN STRING TREE');
              exit;
            end;
          end;
        finally
          lSB.free;
        end;
      end;

      log('StringTree containing '+InttoStr(n)+' strings in '+FloatToStr((now-lStart)*24*60*60));
//      log('Balance Count = '+intToStr(gInsBalanceCount));



      lStart:=now;
      for k := 1 to lLoopCount do    // number of times to run the test.
      begin
        lList := TStringlist.Create;
        try
          //Populate the StringList
          for i := 0 to n-1 do
          begin
            lList.AddObject(lMasterList.Strings[i], pointer(i));
          end;

          // Testing finding each of the strings. in the StringList
          for i := 0 to n-1 do
          begin
            lIndex := lList.indexof(lMasterList.Strings[i]);
            if lIndex=-1 then
            begin
              log('ERROR: NOT ABLE TO FIND STRING IN LIST');
              exit;
            end;
          end;
        finally
          lList.Free;
        end;
      end;

      log('StringList containing '+InttoStr(n)+' strings in '+FloatToStr((now-lStart)*24*60*60));
      log('');

      n := n + 2;
    end;

  finally
    lMasterList.Free;
  end;

end;

procedure TForm19.Button4Click(Sender: TObject);
var
  LErrorCount: integer;

  procedure Do32test(aVal: integer);
  var
    lStream: TMemoryStream;
    lEncoded: cardinal;
    lDecoded: integer;
    lStr: string;
    lVarInt32: TVarInt32;
  begin
    lStream:=TMemoryStream.create;
    try
      // FIRST, JUST TEST THE ZIGZAG ENCODING
      lEncoded := TVarInt32.ZigZagEncode32(aVal);
      lDecoded := TVarInt32.ZigZagDecode32(lEncoded);
      if (aVal <> lDecoded) then
        lStr := 'ERROR: '
      else
        lStr := '';
      log(lStr+intToStr(aVal)+' = '+  intToStr(lEncoded) + ' = '+intToStr(lDecoded));

      // SECOND, TEST THE STREAMING
      lVarInt32 := aVal;
      lVarInt32.WriteToStream(lStream);
      lVarInt32 := 0;
      lStream.Seek(0, soFromBeginning);
      lVarInt32.ReadFromStream(lStream);
      if (integer(lVarInt32) <> aVal ) then
      begin
        log('STREAMING ERROR: '+intToStr(aVal)+' <> '+  intToStr(lVarInt32));
        inc(lErrorCount);
      end
      else
        log('Stream size: '+intToStr(lStream.size));

    finally
      lStream.Free;
    end;
  end;

  procedure Do64test(aVal: int64);
  var
    lEncoded64: UINT64;
    lDecoded64: int64;
    lStr: string;
    lStream: TMemoryStream;
    lVarInt64: TVarInt64;
  begin
    lStream:=TMemoryStream.create;
    try
      // FIRST, JUST TEST THE ZIGZAG ENCODING
      lEncoded64 := TVarInt64.ZigZagEncode64(aVal);
      lDecoded64 := TVarInt64.ZigZagDecode64(lEncoded64);

      if (aVal <> lDecoded64) then
        lStr := 'ERROR: '
      else
        lStr := '';
      log(lStr+intToStr(aVal)+' = '+  intToStr(lEncoded64) + ' = '+intToStr(lDecoded64));

      // SECOND, TEST THE STREAMING
      lVarInt64 := aVal;
      lVarInt64.WriteToStream(lStream);
      lVarInt64 := 0;
      lStream.Seek(0, soFromBeginning);
      lVarInt64.ReadFromStream(lStream);
      if (int64(lVarInt64) <> aVal ) then
      begin
        log('STREAMING ERROR: '+intToStr(aVal)+' <> '+  intToStr(lVarInt64));
        inc(lErrorCount);
      end
      else
        log('Stream size: '+intToStr(lStream.size));
    finally
      lStream.Free;
    end;
  end;

  procedure DoU64test(aVal: UInt64);
  var
    lStream: TMemoryStream;
    lUVarInt64: TUVarInt64;
  begin
    lStream:=TMemoryStream.create;
    try
      // TEST THE STREAMING
      lUVarInt64 := aVal;
      lUVarInt64.WriteToStream(lStream);
      lUVarInt64 := 0;
      lStream.Seek(0, soFromBeginning);
      lUVarInt64.ReadFromStream(lStream);
      if (UInt64(lUVarInt64) <> aVal ) then
      begin
        log('STREAMING ERROR: '+intToStr(aVal)+' <> '+  UIntToStr(lUVarInt64));
        inc(lErrorCount);
      end
      else
        log('Stream size: '+intToStr(lStream.size)+', value: '+UIntToStr(lUVarInt64));
    finally
      lStream.Free;
    end;
  end;

  procedure DoU32test(aVal: Cardinal);
  var
    lStream: TMemoryStream;
    lUVarInt32: TUVarInt32;
  begin
    lStream:=TMemoryStream.create;
    try
      // TEST THE STREAMING
      lUVarInt32 := aVal;
      lUVarInt32.WriteToStream(lStream);
      lUVarInt32 := 0;
      lStream.Seek(0, soFromBeginning);
      lUVarInt32.ReadFromStream(lStream);
      if (Cardinal(lUVarInt32) <> aVal ) then
      begin
        log('STREAMING ERROR: '+intToStr(aVal)+' <> '+  UIntToStr(lUVarInt32));
        inc(lErrorCount);
      end
      else
        log('Stream size: '+intToStr(lStream.size)+', value: '+UIntToStr(lUVarInt32));
    finally
      lStream.Free;
    end;
  end;



begin
  LErrorCount := 0;

  Do32test(low(integer));
  Do32test(-100000000);
  Do32test(-1000000);
  Do32test(-65535);
  Do32test(-255);
  Do32test(-2);
  Do32test(-1);
  Do32test(0);
  Do32test(1);
  Do32test(2);
  Do32test(255);
  Do32test(65535);
  Do32test(1000000);
  Do32test(100000000);
  Do32test(high(integer));
  log('DONE Testing 32Bit VarInt');
  log('');
  log('');



  Do64test(low(int64));
  Do64test(low(int64)+1);
  Do64test(int64(low(integer))-1);
  Do64test(low(integer));
  Do64test(-1000000000000000000);
  Do64test(-10000000000000000);
  Do64test(-100000000000000);
  Do64test(-1000000000000);
  Do64test(-10000000000);
  Do64test(-100000000);
  Do64test(-1000000);
  Do64test(-65535);
  Do64test(-255);
  Do64test(-2);
  Do64test(-1);
  Do64test(0);
  Do64test(1);
  Do64test(2);
  Do64test(255);
  Do64test(65535);
  Do64test(1000000);
  Do64test(100000000);
  Do64test(10000000000);
  Do64test(1000000000000);
  Do64test(100000000000000);
  Do64test(10000000000000000);
  Do64test(1000000000000000000);
  Do64test(high(integer));
  Do64test(int64(high(integer))+1);
  Do64test(high(int64)-1);
  Do64test(high(int64));
  log('DONE Testing 64Bit VarInt');
  log('');
  log('');


  DoU32test(0);
  DoU32test(1);
  DoU32test(2);
  DoU32test(255);
  DoU32test(65535);
  DoU32test(1000000);
  DoU32test(100000000);
  DoU32test(high(Cardinal));
  log('DONE Testing 32Bit Unsigned VarInt');
  log('');
  log('');


  DoU64test(0);
  DoU64test(1);
  DoU64test(2);
  DoU64test(255);
  DoU64test(65535);
  DoU64test(1000000);
  DoU64test(100000000);
  DoU64test(10000000000);
  DoU64test(1000000000000);
  DoU64test(100000000000000);
  DoU64test(10000000000000000);
  DoU64test(1000000000000000000);
  DoU64test(10000000000000000000);
  DoU64test(high(cardinal));
  DoU64test(int64(high(cardinal))+1);
  DoU64test(high(UInt64)-1);
  DoU64test(high(UInt64));
  log('DONE Testing 64Bit Unsigned VarInt');
  log('');
  log('TESTING PRODUCED '+IntToStr(lErrorCount)+' ERRORS');

  if lErrorcount > 0 then
  begin
    Showmessage('TESTING PRODUCED '+IntToStr(lErrorCount)+' ERRORS');
  end;


end;

procedure TForm19.Button5Click(Sender: TObject);
var
  n: integer;
  i: integer;
  lStr: string;
  lSB: TStringBinaryTree;
  lNode: TStringBinaryTreeNode;
  lList: TStringList;
  lIndex: integer;
  lStart: TDateTime;
  lMasterlist: TStringList;
begin
  // This test compares performance between using a simple TStringList and a StringBTree for string ref adding and searching.
  // and each time the test runs, it adds more and more items to the list.
  // Note that this test doesn't really show much for small lists, but it shows how the times grow as the list grows.

  n := 100;
  while n<= 500000 do
  begin
    lMasterlist:=TStringList.create;
    for i := 0 to n do
    begin
      lStr := MakeRandomTypicalString(1000);
      lMasterlist.Add(lStr);
    end;

    lSB:=TStringBinaryTree.create;
    lList:=TStringList.create;
    try

      // Populate the StringTree
      gInsBalanceCount := 0;
      lStart:=now;
      for i := 0 to lMasterList.Count-1 do
      begin
        lSB.AddString(lMasterList.Strings[i], i);
      end;
      lSB.AddString('THIS IS MY TEST STRING',99999999);
      log('Build StringTree containing '+InttoStr(n)+' strings in '+FloatToStr((now-lStart)*24*60*60));
      log('Balance Count = '+intToStr(gInsBalanceCount));

      // Testing finding in the BinaryTree
      lStart:=now;
      for i := 0 to 1000 do
      begin
        lNode := lSB.FindNode('THIS IS MY TEST STRING');
        if not assigned(lNode) then
        begin
          log('ERROR: NOT ABLE TO FIND STRING IN STRING TREE');
          break;
        end;
      end;
      log('Searched StringTree containing '+InttoStr(n)+' strings in '+FloatToStr((now-lStart)*24*60*60));




      //Populate the StringList
      lStart:=now;
      for i := 0 to lMasterList.Count-1 do
      begin
        lList.AddObject(lMasterList.Strings[i], pointer(i));
      end;
      lList.AddObject('THIS IS MY TEST STRING',pointer(99999999));
      log('Build StringList containing '+InttoStr(n)+' strings in '+FloatToStr((now-lStart)*24*60*60));

      // Testing finding in the StringList
      lStart:=now;
      for i := 0 to 1000 do
      begin
        lIndex := lList.indexof('THIS IS MY TEST STRING');
        if lIndex=-1 then
        begin
          log('ERROR: NOT ABLE TO FIND STRING IN LIST');
          break;
        end;
      end;
      log('Searched StringList containing '+InttoStr(n)+' strings in '+FloatToStr((now-lStart)*24*60*60));
      log('')


    finally
      lList.Free;
      lSB.free;
    end;

    n := n*2;
  end;
end;

procedure TForm19.log(aStr: string);
begin
  memo1.Lines.Add(aStr);
end;

procedure TForm19.MakeTestObject(aObj: TDataObj);
var
  i: integer;
  lGUID: TGUID;
  lbuffer: array[0..23] of byte;  // NOTE: that UBJSON does not like serializing binary data... very inefficient, but some of the other serialzations are more binary friendly.
begin
  with aObj.AsFrame do
  begin
    newSlot('Boolean').AsBoolean := true;
    newSlot('Byte').AsByte := 123;
    newSlot('Int32').AsInt32 := 12345;
    newSlot('Int64').AsInt64 := 123456789;
    newSlot('Single').AsSingle := 12345.6789;
    newSlot('Double').AsDouble := 98765.4321;
    newSlot('DateTime').AsDateTime := now;
    newSlot('UTCDateTime').AsUTCDateTime := DateTimeToUnix(now);
    newSlot('Date').AsDate := now;
    newSlot('Time').AsTime := now;

    lGUID.D1 := $FEDCBA09;
    lGUID.D2 := $1234;
    lGUID.D3 := $4321;
    lGUID.D4[0] := 9;
    lGUID.D4[1] := 8;
    lGUID.D4[2] := 7;
    lGUID.D4[3] := 6;
    lGUID.D4[4] := 5;
    lGUID.D4[5] := 4;
    lGUID.D4[6] := 3;
    lGUID.D4[7] := 2;
    newSlot('GUID').AsGUID.GUID := lGUID;

    with newSlot('ObjectId').AsObjectID do
    begin
      for i := 0 to 11 do
        Data[i] := i;
    end;

    newSlot('String').AsString := 'Hello World';
    newSlot('Symbol').AsSymbol := 'SYMBOL';
    with newSlot('StringList').AsStringList do
    begin
      Add('JSON (JavaScript Object Notation) is a lightweight data-interchange format.');
      Add('It is easy for humans to read and write.');
      Add('It is easy for machines to parse and generate.');
      Add('It is based on a subset of the JavaScript Programming Language, Standard ECMA-262 3rd Edition - December 1999.');
      Add('JSON is a text format that is completely language independent but uses conventions that are familiar to programmers of the C-family of languages, including C, C++, C#, Java, JavaScript, Perl, Python, and many others.');
      Add('These properties make JSON an ideal data-interchange language.');
    end;
    with newSlot('Frame').AsFrame do
    begin
      NewSlot('FirstName').AsString := 'Sean';
      Newslot('LastName').AsString := 'Solberg';
    end;

    with newSlot('Array').AsArray do
    begin
      for i := 0 to 19 do
        newSlot.AsInt32 := random($7fffffff);
    end;


(*  NOT READY YET - We have not fully baked the code for SparseArrays.
    with newSlot('SparseArray').AsSparseArray do
    begin
      for i := 0 to 19 do
        newSlot(random($7fffffff)).AsInteger := random($7fffffff);
    end;*)

    for i := low(lbuffer) to high(lbuffer) do
      lBuffer[i] := i;

    newSlot('Binary').AsBinary.Write(lbuffer, sizeof(lbuffer));

    // This is showing the ability to create a dataObject that is a TAG.  That tag then has a TagValue and an internal DataObj that can be set to anything.
    // This is mostly a feature to support Tags in CBOR.   See the CBOR streamer for details.
    with newSlot('tag').AsTag do
    begin
      TagValue := 32;
      DataObj.AsString := 'http://dataobjectspec.org';
    end;
  end;
end;

end.
