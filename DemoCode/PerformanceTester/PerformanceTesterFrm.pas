unit PerformanceTesterFrm;

{This test project will create a sample DataObject payload and then stream it many times to and from a stream to test the performance of each of the
 streamer classes included in this test case.  Then, basic statistics are published for each of the streamer classes' tests. }

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, DataObjects2, DateUtils, DataObjects2Streamers,
  DataObjects2CBOR, DataObjects2DDO, DataObjects2ION, DataObjects2JSON, DataObjects2MsgPack,
  DataObjects2BSON, DataObjects2UBJSON, DataObjects2Smile, DataObjects2CSV;

type
  TForm19 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    procedure MakeTestObject(aObj: TDataObj);
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


procedure TForm19.Button1Click(Sender: TObject);
var
  i: integer;
  lDataObj: TDataObj;
  lBigDataObj: TDataObj;
  lMemStream: TMemoryStream;
  lCount: integer;

  procedure m(aStr: string);
  begin
    memo1.Lines.Add(aStr);
  end;

  function PubTime(aTimeDiff: TDatetime): string;
  begin
    result := FloatToStrf(aTimeDiff*24*60*60, ffFixed, 10, 3);
  end;

  procedure PerformTest(aClass: TDataObjStreamerClass; aTestCount: integer);
  var
    lStreamer: TDataObjStreamerBase;
    i: integer;
    lStart: TDateTime;
    lStart2: TDateTime;
    lStart3: TDateTime;
    lEncodeTime: TDateTime;
    lDecodetime: TDatetime;
    lReadDataObj: TDataObj;
    lEncodesize: integer;
  begin
    // Create the given streamer class and we will perform some tests on it to see how it performs.
    lStreamer := aClass.Create(nil);
    try
      lMemStream.SetSize(0);
      lStreamer.Stream := lMemStream;

      try
        // Do a performance test for encoding to this format aTestCount times
        lStart := now;
        for i := 1 to aTestCount do
        begin
  //        lMemStream.Clear;
          lMemStream.seek(0,soBeginning);
          lStreamer.Encode(lDataObj);
        end;
        lEncodeTime := now;

        // Save this last test stream to a file.
        ForceDirectories('./TestOutputs');
        lMemStream.SaveToFile('./TestOutputs/'+lStreamer.ClassName+'.'+lStreamer.FileExtension);
        lEncodeSize := lMemStream.Size;
      except
        on e: exception do
          m(e.message);
      end;


      // Do a performance test for decoding from this format aTestCount times
      try
        lStart2 := now;
        lReadDataObj := TDataObj.Create;
        try
          try
            for i := 1 to aTestCount do
            begin
              lReadDataObj.Clear;           // note:  we are using up some time here doing the clear so the test will have a bit more time in it.
              lMemStream.Seek(0, soBeginning);
              lStreamer.Stream := lMemStream;
              lStreamer.Decode(lReadDataObj);
            end;

          except
            on e: exception do
            begin
              m(aClass.classname+':  Decoding Error: '+e.Message);
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

      try
        // Now make a bigger test where we only serialize once, but we are serializing a pretty big array of objects
        lMemStream.SetSize(0);
        lStart3 := now;
        lMemStream.seek(0,soBeginning);
        lStreamer.Encode(lBigDataObj);
        lMemStream.seek(0,soBeginning);
        lMemStream.SaveToFile('./TestOutputs/'+lStreamer.ClassName+'_BIG.'+lStreamer.FileExtension);
      except
        on e: exception do
          m(e.message);
      end;


      // Now publish our timing results
      m(aClass.classname+': Encoded in '+PubTime(lEncodeTime-lStart)+' seconds, Decoded in '+PubTime(lDecodeTime-lStart2)+' seconds,  Size='+IntToStr(lEncodesize)+
      '  BigEncode in '+PubTime(now-lStart3)+' seconds, Size='+InttoStr(lMemStream.Size));
      m('');
    finally
      lStreamer.Free;
    end;
  end;

begin
  lCount := 100000;
  lDataObj:=TDataObj.create;
  lMemStream:=TMemoryStream.Create;
  lBigDataObj:=TDataObj.Create;
  try
    // Make our test dataobject that we will use for serializing to many different formats
    MakeTestObject(lDataObj);

    // Make a second test dataObject that has the first object cloned 10,000 times for a big object write test.
    for i := 1 to 10000 do
    begin
      lBigDataObj.AsArray.newslot.CopyFrom(lDataObj);
    end;

    m('Each test is done '+inttostr(lCount)+' times.');

    PerformTest(TDataObjStreamer, lCount);
    PerformTest(TDDOStreamer, lCount);
    PerformTest(TJsonStreamer, lCount);
    PerformTest(TCBORStreamer, lCount);
    PerformTest(TBSONStreamer, lCount);
    PerformTest(TUBJsonStreamer, lCount);
    PerformTest(TMsgPackStreamer, lCount);
    PerformTest(TSmileStreamer, lCount);
//    PerformTest(TCSVStreamer, lCount);   The CSV Streamer works, but it is geared to only work with data that is structured as an array of frames, which this test is not.
//    PerformTest(TIonStreamer);  The ION Streamer is Not ready enough yet.
    m('DONE');
  finally
    lBigDataObj.Free;
    lMemStream.Free;
    lDataObj.Free;
  end;
end;


// MakeTestObject does just what it says.  It will fill aObj with a buch of test data.
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
