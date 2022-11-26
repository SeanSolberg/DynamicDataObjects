unit JSONComparisonMainFrm;

{This is a JSON comparison application that compares how the DataObjects JSON serializer compares to other JSON libraries performance-wise.
 In order to compile and run this application, you would need to install the other respective code libraries that we are comparing against.
 Then you need to update the Project's Options under the Delphi Compiler -> SearchPath option to point to where you installed them.
 Alternatively, if you want to skip some of these libraries from this test case, simply comment out the associated $DEFINE just below.

 DataObjects.pas - This is an old serializer that is no longer used, but I still like comparing performance to it.

 Grijjy can be cloned from: https://github.com/grijjy/GrijjyFoundation

 CleverComponents JSON-Serializer can be cloned from: https://github.com/CleverComponents/Json-Serializer.git

}

{$DEFINE cIncludeDDOTest}
{$DEFINE cIncludeGrijjyTest}
//{$DEFINE cIncludeCleverJSON}


interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, json, {Rest.json,}
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs,  Vcl.StdCtrls, db, DateUtils, varint,
    {$ifdef cIncludeDDOTest}
      DataObjects,
    {$endif}
    {$ifdef cIncludeGrijjyTest}
      Grijjy.bson,
    {$endif}
    {$ifdef cIncludeCleverJSON}
      clJsonSerializer, clJsonParser, clJsonSerializerBase,
    {$endif}
    DataObjects2, DataObjects2JSON;

const
  cMakeTestSize = 1000;   // The number of rounds for making test data in the Makexxxxx tests.

type
  TPerson = class
  private
    fAge: integer;
    fLastName: string;
    fFirstName: string;
    fWeight: single;
    fHeight: double;
    function getDisplayString: string;
  public
    property displayString: string read getDisplayString;
  published
    property FirstName: string read fFirstName write fFirstname;
    property Lastname: string read fLastName write fLastName;
    property Age: integer read fAge write fAge;
    property Weight: single read fWeight write fWeight;
    property Height: double read fHeight write fHeight;
  end;

  TTestPass = record

  end;

  TTestType=(cttTightUTF8, cttTightAscii, cttFormattedUTF8, cttFormattedAscii);


  TForm15 = class(TForm)
    Memo1: TMemo;
    Button4: TButton;
    btnDelphiLoadFromJSON: TButton;
    btnOldDDOLoadFromJSON: TButton;
    btnNewDataObjLoadFromJSON: TButton;
    Button24: TButton;
    btnEscapeTest: TButton;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    btnRunAllTests: TButton;
    Button3: TButton;
    procedure Button4Click(Sender: TObject);
    procedure btnDelphiLoadFromJSONClick(Sender: TObject);
    procedure btnOldDDOLoadFromJSONClick(Sender: TObject);
    procedure btnNewDataObjLoadFromJSONClick(Sender: TObject);
    procedure Button24Click(Sender: TObject);
    procedure btnEscapeTestClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure btnRunAllTestsClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
    fTestJSONString: string;
    fTestCount: integer;
    fIndent: integer;

    procedure m(aStr: string);
{$ifdef cIncludeDDOTest}
    procedure MakeSampleData(aDataObj: DataObjects.TDataObj);  overload;
{$endif}
    procedure MakeSampleData(aDataObj: DataObjects2.TDataObj); overload;
    procedure LoadTestFile(aFileName: string; aTestCount: integer);
    procedure LoadStoryJSON;
    function RunDelphiTest: string;
    function RunOldDDOTest: string;
    function RunNewDataObjTest: string;
    function RunGrijjiTest: string;
    function runJsonSerializerTest: string;
    procedure LoadTwitterJSON;
    procedure LoadMeshJSON;

    function MakeTestDataObj(aType: TTestType): string;
    function MakeTestDDO(aType: TTestType): string;
    function MakeTestGrijjy(aType: TTestType): string;
    function MakeTestDelphi(aType: TTestType): string;

  public
    { Public declarations }
    function CompareStreams(aStream1, aStream2: TStream): integer;   // returns the position in the streams where they are different.  returns -1 if they are the same.
    procedure PublishStreamBytes(aStream: TStream);
  end;

{$ifdef cIncludeDDOTest}
  procedure OldDataObjToNewDataObj(aOldDataObj: dataObjects.TDataObj; aNewDataObj: dataObjects2.TDataObj);
{$endif}

var
  Form15: TForm15;

implementation

const
  cTestArray: array[0..11] of byte = (1,2,3,4,5,6,7,8,9,10,11,12);


{$R *.dfm}

{$define cOldDDO}

procedure TForm15.btnDelphiLoadFromJSONClick(Sender: TObject);
var
  lString: string;
  lSS: TStringStream;
begin
//  LoadStoryJSON;
  LoadMeshJSON;

  lString := RunDelphiTest;

  lSS:=TStringStream.create(lString, TEncoding.ANSI);
  try
    ForceDirectories('c:\temp');
//    lSS.SaveToFile('c:\temp\story-Delphi.json');
    lSS.SaveToFile('c:\temp\mesh-Delphi.json');
  finally
    lSS.Free;
  end;
end;

function TForm15.RunDelphiTest: string;
var
   JSonValue: TJSonValue;
   lStart: TDateTime;
   i: integer;
   JSonObject: TJSonObject;
   lSS: TStringStream;
begin
   m('Starting Delphi JSON Test');

   lStart := now;
   for i := 1 to fTestCount do
   begin
     JSonObject := TJSonObject.Create;
     JSonObject.ParseJSONValue(fTestJSONString, true);
     JSonObject.Free;
   end;
   m('Delphi JSON Parse '+intToStr(fTestCount)+' times.  Total Time = '+FloatToStrf((now-lStart)*24*60*60, ffFixed, 10, 2));

   // Now re-write this test file to disk.
   JSonValue := TJSonObject.ParseJSONValue(fTestJSONString, true);
   try
     lStart := now;
     for i := 1 to fTestCount do
     begin
       result := JSonValue.ToJSON([TJSONAncestor.TJSONOutputOption.EncodeBelow32,TJSONAncestor.TJSONOutputOption.EncodeAbove127]);
     end;
     m('Delphi JSON Write '+intToStr(fTestCount)+' times.  Total Time = '+FloatToStrf((now-lStart)*24*60*60, ffFixed, 10, 2));

     lSS := TStringStream.Create;
     lSS.WriteString(result);
     lSS.SaveToFile('c:\temp\CreatedDelphiJSON.json');
     lSS.Free;


   finally
     JSonValue.Free;
   end;
   m('');
end;

procedure TForm15.btnOldDDOLoadFromJSONClick(Sender: TObject);
var
  lSS: TStringStream;
  lString: string;
begin
{$ifdef cIncludeDDOTest}
  LoadStoryJSON;
  lString := RunOldDDOTest;

  lSS:=TStringStream.create(lString,TEncoding.UTF8);
  try
    lSS.SaveToFile('c:\temp\story-OldDataObj.json');
  finally
    lSS.Free;
  end;
{$else}
  m('The Old DDO Test is not included in this build.');
{$endif}
end;

procedure TForm15.btnRunAllTestsClick(Sender: TObject);
begin
  fIndent := 0;
  LoadStoryJSON;
  fIndent := 2;
  RunDelphiTest;
  RunOldDDOTest;
  RunNewDataObjTest;
  RunGrijjiTest;
  RunJsonSerializerTest;

  fIndent := 0;
  m('');
  LoadTwitterJSON;
  fIndent := 2;
  RunDelphiTest;
  RunOldDDOTest;
  RunNewDataObjTest;
  RunGrijjiTest;
  RunJsonSerializerTest;

  fIndent := 0;
  m('');
  LoadMeshJSON;
  fIndent := 2;
  RunDelphiTest;
  RunOldDDOTest;
  RunNewDataObjTest;
  RunGrijjiTest;
  RunJsonSerializerTest;

  m('DONE.');

end;

function TForm15.RunOldDDOTest: string;
{$ifdef cIncludeDDOTest}
var
   lDataObj: dataObjects.TDataObj;
   lStart: TDateTime;
   i: integer;
{$endif}
begin
{$ifdef cIncludeDDOTest}
  m('Starting Old DDO JSON Test');
   lStart := now;
   for i := 1 to fTestCount do
   begin
     lDataObj := dataObjects.TDataObj.Create;
     lDataObj.JSON := fTestJSONString;
     lDataObj.Free;
   end;

   m('Old DDO Library JSON Parse '+intToStr(fTestCount)+' times.  Total Time = '+FloatToStrf((now-lStart)*24*60*60, ffFixed, 10, 2));

   // Now re-write this test file to disk.
   lDataObj := dataObjects.TDataObj.Create;
   try
     lDataObj.JSON := fTestJSONString;

     lStart := now;
     for i := 1 to fTestCount do
     begin
       result := lDataObj.JSON;
     end;
     m('Old DDO Library JSON Write '+intToStr(fTestCount)+' times.  Total Time = '+FloatToStrf((now-lStart)*24*60*60, ffFixed, 10, 2));
   finally
     lDataObj.Free;
   end;
   m('');
{$endif}
end;

procedure TForm15.btnNewDataObjLoadFromJSONClick(Sender: TObject);
var
  lString: string;
  lSS: TStringStream;
begin
  m('Staring DataObjects2 JSON Test');
//  LoadStoryJSON;
  LoadMeshJSON;

  lString := RunNewDataObjTest;

  lSS:=TStringStream.create(lString,TEncoding.UTF8);
  try
    lSS.SaveToFile('c:\temp\story-DataObjects.json');
  finally
    lSS.Free;
  end;
end;

function TForm15.RunNewDataObjTest: string;
var
  lDataObj: Dataobjects2.TDataObj;
  lStart: TDatetime;
  lSS: TStringStream;
  i: Integer;
  lJS: TJsonStreamer;
begin
  // Now parse this json string multiple times to see how fast it is
  m('Starting DataObjects2 test');
  lStart := now;
  for i := 1 to fTestCount do
  begin
    lDataObj := DataObjects2.TDataObj.Create;
    TJsonStreamer.JsonToDataObj(fTestJSONString, lDataObj);
    lDataObj.Free;
  end;
  m('DataObjects JsonStreamer JSON Parse '+intToStr(fTestCount)+' times.  Total Time = '+FloatToStrf((now-lStart)*24*60*60, ffFixed, 10, 2));

  // now write this json file using the new data objects library
  lDataObj := DataObjects2.TDataObj.Create;
  lSS := TStringStream.Create;
  lJS := TJsonStreamer.Create(lSS, TEncoding.UTF8);
  try
    TJsonStreamer.JsonToDataObj(fTestJSONString, lDataObj);   // JSON to DataObject

    // Test Creating JSON to string.
    lStart := now;
    for i := 1 to fTestCount do
    begin
      lSS.Clear;
      lJS.Encode(lDataObj);                             // DataObject to JSON
    end;
    m('DataObjects JsonStreamer JSON Encode '+intToStr(fTestCount)+' times.  Total Time = '+FloatToStrf((now-lStart)*24*60*60, ffFixed, 10, 2));

    result := lSS.DataString;
  finally
    lJS.Free;
    lSS.Free;
    lDataObj.Free;
  end;
  m('');
end;

procedure TForm15.Button1Click(Sender: TObject);
var
  lString: string;
  lSS: TStringStream;
begin
{$ifdef cINcludeGrijjyTest}
  LoadStoryJSON;

  lString := RunGrijjiTest;

  // Now re-write this test file to disk.
  lSS:=TStringStream.create(lString, TEncoding.UTF8);
  try
    lSS.SaveToFile('c:\temp\story-Grijjy.json');
  finally
    lSS.Free;
  end;
{$endif}
end;

function TForm15.RunGrijjiTest: string;
{$ifdef cIncludeGrijjyTest}
var
   lStart: TDateTime;
   i: integer;
   lDoc: TgoBsonDocument;
{$endif}
begin
{$ifdef cIncludeGrijjyTest}
   m('Starting GrijjyJSON Test');
   lStart := now;
   for i := 1 to fTestCount do
   begin
     lDoc := TgoBsonDocument.Parse(fTestJSONString);
   end;
   m('Grijji JSON Parse '+intToStr(fTestCount)+' times.  Total Time = '+FloatToStrf((now-lStart)*24*60*60, ffFixed, 10, 2));

   // Test Creating JSON to string.
   lStart := now;
   lDoc := TgoBsonDocument.Parse(fTestJSONString);
   for i := 1 to fTestCount do
   begin
     result := lDoc.ToJSON;
   end;
   m('Grijji JSON Write '+intToStr(fTestCount)+' times.  Total Time = '+FloatToStrf((now-lStart)*24*60*60, ffFixed, 10, 2));
   m('');
{$endif}
end;

procedure TForm15.Button24Click(Sender: TObject);
var
  lSS: TStringStream;
  lString: string;
  lOut: string;
  lDataObj: dataObjects2.TDataObj;
  lJS: TJsonStreamer;
begin
  lSS := TStringStream.Create;
  lSS.LoadFromFile('c:\temp\story.json');
  lString := lSS.DataString;
  lSS.Free;

  // putting this string to JSON and then back to a string will then produce a tight JSON string with whitespace (crlf) removed but strings in the JSON are encoded.
  lDataObj := dataObjects2.TDataObj.create;
  TJsonStreamer.JsonToDataObj(lString, lDataObj);

  lJS := TJsonStreamer.Create(DataObjects2JSON.cJsonTight);
  try
    lJS.EncodeNonAsciiCharacters := true;
    lJS.Encode(lDataObj);
    lOut := lJS.JSON;
  finally
    lJS.Free;
  end;

  lSS := TStringStream.Create('', TEncoding.UTF8);
  lSS.WriteString(lOut);
  lSS.SaveToFile('c:\temp\Story-PreDoubleEncoded.json');
  lSS.Free;


  lOut := StringReplace(lOut, '\', '\\', [rfReplaceAll]);

  lSS := TStringStream.Create('', TEncoding.UTF8);
  lSS.WriteString(lOut);
  lSS.SaveToFile('c:\temp\Story-DoubleEncoded.json');
  lSS.Free;
end;

procedure TForm15.Button2Click(Sender: TObject);
var
  lString: string;
  lSS: TStringStream;
begin
  LoadStoryJSON;

  lString := RunJsonSerializerTest;

  lSS:=TStringStream.create(lString, TEncoding.ANSI);
  try
    lSS.SaveToFile('c:\temp\story-Delphi.json');
  finally
    lSS.Free;
  end;
end;

procedure TForm15.Button3Click(Sender: TObject);
var
  lJSON: string;

  procedure SaveJSON(aFilename: string; aIsAscii: boolean);
  var
    lSS: TStringStream;
  begin
    if aIsAscii then
      lSS:=TStringStream.create('',TEncoding.Ascii)
    else
      lSS:=TStringStream.create('',TEncoding.UTF8);
    try
      lSS.WriteString(lJSON);
      lSS.SaveToFile(aFilename);
    finally
      lSS.Free;
    end;
  end;

  procedure DoARoundOfTests(aType: TTestType);
  var
    lSuffix: string;
    lIsAscii: boolean;
  begin
    lIsAscii := false;
    case aType of
      cttTightUTF8: begin lSuffix := 'TightUTF8'; m('Tight UTF8'); m('-----------------'); end;
      cttTightAscii: begin lSuffix := 'TightAscii'; m('Tight Ascii'); m('-----------------'); lIsAscii:=true; end;
      cttFormattedUTF8: begin lSuffix := 'FormattedUTF8'; m('Formatted UTF8'); m('-----------------'); end;
      cttFormattedAscii: begin lSuffix := 'FormattedAscii'; m('Formatted Ascii'); m('-----------------'); lIsAscii:=true; end;
    end;
    lJSON := MakeTestDataObj(aType);
    SaveJSON('c:\temp\MakeTestDataObj-'+lSuffix+'.json', lIsAscii);
    lJSON := MakeTestDDO(aType);
    SaveJSON('c:\temp\MakeTestDDO-'+lSuffix+'.json', lIsAscii);
    lJSON := MakeTestGrijjy(aType);
    SaveJSON('c:\temp\MakeTestGrijjy-'+lSuffix+'.json', lIsAscii);
    lJSON := MakeTestDelphi(aType);
    SaveJSON('c:\temp\MakeTestDelphi-'+lSuffix+'.json', lIsAscii);
    m('');
  end;


begin
  DoAroundOfTests(cttTightUTF8);
  DoAroundOfTests(cttTightAscii);
  DoAroundOfTests(cttFormattedUTF8);
  DoAroundOfTests(cttFormattedAscii);
end;

function TForm15.runJsonSerializerTest: string;
{$ifdef cIncludeCleverJSON}
var
  lStart: TDateTime;
  i: integer;
  lJSonObject: TclJSONObject;
{$endif}
begin
{$ifdef cIncludeCleverJSON}
  m('Starting CleverJSON Test');
  try
    lStart := now;
    for i := 1 to fTestCount do
    begin
      lJSonObject := TclJSONBase.ParseObject(fTestJSONString);
      lJSonObject.Free;
    end;
    m('TclJSONObject JSON Parse '+intToStr(fTestCount)+' times.  Total Time = '+FloatToStrf((now-lStart)*24*60*60, ffFixed, 10, 2));

    lJSonObject := TclJSONBase.ParseObject(fTestJSONString);
    try
      lStart := now;
      for i := 1 to fTestCount do
      begin
        result := lJSonObject.GetJSONString;
      end;
      m('TclJSONObject JSON Write '+intToStr(fTestCount)+' times.  Total Time = '+FloatToStrf((now-lStart)*24*60*60, ffFixed, 10, 2));
    finally
      lJSonObject.Free;
    end;
  except
    on e: exception do
      m(e.ClassName+': '+e.Message);
  end;
{$endif}
end;

procedure TForm15.btnEscapeTestClick(Sender: TObject);
var
  lStr: string;
  lDataObj: dataObjects2.TDataObj;
begin
  // Test for parsing a few escaped characters.
  lStr := '{"Test": "\"Hello\u2028World\""}';

  lDataObj := dataObjects2.TDataObj.create;
  try
    TJsonStreamer.JsonToDataObj(lStr, lDataObj);
    lStr := TJsonStreamer.DataObjToJson(lDataObj);
    memo1.Lines.Add(lStr);
  finally
    lDataObj.free;
  end;
end;

procedure TForm15.Button4Click(Sender: TObject);
var
  lStart: TDateTime;
  lStr: string;
  lAnsiString: ansiString;
  lUTF8String: UTF8String;
  i: Integer;
begin
  lStr := 'Alle mennesker er født frie og med samme menneskeverd og menneskerettigheter. De er utstyrt med fornuft og samvittighet og bør handle mot hverandre i brorskapets ånd.';
  lAnsiString := lStr;
  lUTF8String := lStr;
  m(lStr);
  m(lAnsiString);
  m(lUTF8String);

  lStart := now;
  for i := 1 to 1000000 do
  begin
    lUTF8String := lStr;
  end;
  m(FloatToStr((now-lStart)*24*60*60));

  lStart := now;
  for i := 1 to 1000000 do
  begin
    lAnsiString := lStr;
  end;
  m(FloatToStr((now-lStart)*24*60*60));
end;

function TForm15.CompareStreams(aStream1, aStream2: TStream): integer;
var
  lByte1, lByte2: Byte;
  i: Integer;
  lCount: integer;
begin
  aStream1.Seek(0, soFromBeginning);
  aStream2.Seek(0, soFromBeginning);

  result := -1;
  for i := 0 to aStream1.size-1 do
  begin
    lcount := aStream1.Read(lByte1,1);
    if lCount<>1 then
    begin
      result := i;
      break;
    end;
    lcount := aStream2.Read(lbyte2,2);
    if lCount<>1 then
    begin
      result := i;
      break;
    end;

    if lByte1<>lByte2 then
    begin
      result := i;
      break;
    end;
  end;
end;

procedure TForm15.PublishStreamBytes(aStream: TStream);
var
  lStr: string;
  lCount: integer;
  i: integer;
  lByte: byte;
begin
  lStr := '';
  lCount := 0;
  aStream.Seek(0, soFromBeginning);
  for I := 0 to aStream.Size-1 do
  begin
    aStream.Read(lByte,1);
    lStr := lStr + inttohex(lbyte, 2)+' ';
    inc(lCount);
    if lcount = 8 then
    begin
      lCount := 0;
      m(lStr);
      lStr := '';
    end;
  end;
  m(lStr);
end;

procedure TForm15.m(aStr: string);
begin
  memo1.lines.Add(StringOfChar(' ',fIndent)+aStr);
end;

{$ifdef cIncludeDDOTest}
procedure TForm15.MakeSampleData(aDataObj: DataObjects.TDataObj);
var
  lStr: string;
begin
    with aDataObj.AsFrame do
    begin
      NewSlot('FirstName').AsString := 'John';

      NewSlot('LastName').AsString := 'Solberg';
      NewSlot('FirstName').AsString := 'Sean';      //note:  overwriting.
      newSlot('Age').AsInteger := 123;
      newslot('weight').asFloat := 123.456;

      lStr := 'Alle mennesker er født frie og med samme menneskeverd og menneskerettigheter. De er utstyrt med fornuft og samvittighet og bør handle mot hverandre i brorskapets ånd.';
      newSlot('Norwegian1').AsString := lStr;

      lStr := 'alle mennesker er født frie og med samme menneskeverd og menneskerettigheter. de er utstyrt med fornuft og samvittighet og bør handle mot hverandre i brorskapets ånd.';
      newSlot('Norwegian2').AsString := lStr;

      lStr := 'ALLE MENNESKER ER FØDT FRIE OG MED SAMME MENNESKEVERD OG MENNESKERETTIGHETER. DE ER UTSTYRT MED FORNUFT OG SAMVITTIGHET OG BØR HANDLE MOT HVERANDRE I BRORSKAPETS ÅND.';
      newSlot('Norwegian3').AsString := lStr;


      newslot('BooleanT').AsBoolean := true;
      newslot('BooleanF').AsBoolean := false;
      newslot('Byte').AsByte := 255;
      newslot('Int32_1').AsInteger := $7FFFFFFF;
      newslot('Int32_2').AsInteger := $FFFFFFFF;
      newslot('Int32_3').AsInteger := $80000000;
      newSlot('Int64_1').AsInt64 := $7FFFFFFFFFFFFFFF;
      newslot('Int64_2').AsInt64 := $FFFFFFFFFFFFFFFF;
      newslot('Int64_3').AsInt64 := $8000000000000000;
      newSlot('VarInt1').AsInteger := $7FFFFFFFFFFFFFFF;
      newslot('VarInt2').AsInteger := $FFFFFFFFFFFFFFFF;
      newslot('VarInt3').AsInteger := $8000000000000000;
      newSlot('VarInt4').AsInteger := 1;
      newslot('VarInt5').AsInteger := 127;
      newslot('VarInt6').AsInteger := 128;
      newslot('VarInt7').AsInteger := 255;
      newslot('VarInt8').AsInteger := 256;
      newSlot('VarInt9').AsInteger := -1;
      newslot('VarInt10').AsInteger := -127;
      newslot('VarInt11').AsInteger := -128;
      newslot('VarInt12').AsInteger := -255;
      newslot('VarInt13').AsInteger := -256;

      newSlot('Single').AsFloat := 12345.6789123456789;
      newSlot('double').AsFloat := 12345.6789123456789;

      newSlot('DateTime').AsDateTime := now;
      newSlot('Date').AsDateTime := now;
      newSlot('Time').AsDateTime := now;

      newslot('UTCDateTime').AsInt64 := DateTimeToUnix(now);

      newslot('GUID').AsMemStream.Write('1234567890123456',16);
      newSlot('Symbol').AsSymbol := 'THIS IS A SYMBOL';
      newslot('ObjectID').AsMemStream.Write('123456789012',12);

  //    newSlot('Binary').AsMemStream.LoadFromFile('c:\temp\flower2.png');
      with newSlot('StringList').AsStringlist do
      begin
        add('hello');
        add('World');
        add('welcome to');
        add('   * DELPHI *   ');
      end;
    end;
end;
{$endif}

procedure TForm15.LoadStoryJSON;
begin
  LoadTestFile('..\..\..\..\Samplefiles\json\story.json', 5000);
end;

procedure TForm15.LoadMeshJSON;
begin
  LoadTestFile('..\..\..\..\Samplefiles\json\mesh.json', 50);
end;


procedure TForm15.LoadTestFile(aFileName: string; aTestCount: integer);
var
  lSS: TStringStream;
begin
  // Load the json file into a string
  lSS := TStringStream.Create;
  try
    m('Loading JSON test file '+aFileName);
    lSS.LoadFromFile(aFilename);
    fTestJSONString := lSS.DataString;
    fTestCount := aTestCount;
  finally
    lSS.Free;
  end;
end;

procedure TForm15.LoadTwitterJSON;
begin
  LoadTestFile('..\..\..\..\Samplefiles\json\twitter.json', 50);
end;


procedure TForm15.MakeSampleData(aDataObj: DataObjects2.TDataObj);
var
  lStr: string;
begin
  with aDataObj.AsFrame do
  begin
      NewSlot('FirstName').AsString := 'John';

      NewSlot('LastName').AsString := 'Solberg';
      NewSlot('FirstName').AsString := 'Sean';      //note:  overwriting.
      newSlot('Age').AsInt32 := 123;
      newslot('weight').AsDouble := 123.456;

      lStr := 'Alle mennesker er født frie og med samme menneskeverd og menneskerettigheter. De er utstyrt med fornuft og samvittighet og bør handle mot hverandre i brorskapets ånd.';
      newSlot('Norwegian1').AsString := lStr;

      lStr := 'alle mennesker er født frie og med samme menneskeverd og menneskerettigheter. de er utstyrt med fornuft og samvittighet og bør handle mot hverandre i brorskapets ånd.';
      newSlot('Norwegian2').AsString := lStr;

      lStr := 'ALLE MENNESKER ER FØDT FRIE OG MED SAMME MENNESKEVERD OG MENNESKERETTIGHETER. DE ER UTSTYRT MED FORNUFT OG SAMVITTIGHET OG BØR HANDLE MOT HVERANDRE I BRORSKAPETS ÅND.';
      newSlot('Norwegian3').AsString := lStr;



      newslot('BooleanT').AsBoolean := true;
      newslot('BooleanF').AsBoolean := false;
      newslot('Byte').AsByte := 255;
      newslot('Int32_1').AsInt32 := $7FFFFFFF;
      newslot('Int32_2').AsInt32 := $FFFFFFFF;
      newslot('Int32_3').AsInt32 := $80000000;
      newSlot('Int64_1').AsInt64 := $7FFFFFFFFFFFFFFF;
      newslot('Int64_2').AsInt32 := $FFFFFFFFFFFFFFFF;
      newslot('Int64_3').AsInt32 := $8000000000000000;
      newSlot('VarInt1').AsInt64 := $7FFFFFFFFFFFFFFF;
      newslot('VarInt2').AsInt64 := $FFFFFFFFFFFFFFFF;
      newslot('VarInt3').AsInt64 := $8000000000000000;
      newSlot('VarInt4').AsInt64 := 1;
      newslot('VarInt5').AsInt64 := 127;
      newslot('VarInt6').AsInt64 := 128;
      newslot('VarInt7').AsInt64 := 255;
      newslot('VarInt8').AsInt64 := 256;
      newSlot('VarInt9').AsInt64 := -1;
      newslot('VarInt10').AsInt64 := -127;
      newslot('VarInt11').AsInt64 := -128;
      newslot('VarInt12').AsInt64 := -255;
      newslot('VarInt13').AsInt64 := -256;

      newSlot('Single').AsSingle := 12345.6789123456789;
      newSlot('double').AsDouble := 12345.6789123456789;

      newSlot('DateTime').AsDateTime := now;
      newSlot('Date').AsDate := now;
      newSlot('Time').AsTime := now;

      newslot('UTCDateTime').AsUTCDateTime := DateTimeToUnix(now);

      newslot('GUID').AsGUID.GUID := TGUID.NewGuid;
      newSlot('Symbol').AsSymbol := 'THIS IS A SYMBOL';
      with newSlot('ObjectID') do
      begin
        AsObjectID.Seconds := $01020304;
        AsObjectID.MachineID := $050607;
        AsObjectID.ProcessID := $0809;
        AsObjectID.Counter := $0A0B0C;
      end;
 //     newSlot('Binary').AsBinary.LoadFromFile('c:\temp\flower2.png');
      with newSlot('StringList').AsStringlist do
      begin
        add('hello');
        add('World');
        add('welcome to');
        add('   * DELPHI *   ');
      end;
    end;
end;

function TForm15.MakeTestDataObj(aType: TTestType): string;
var
  lTop: TDataObj;
  lFrame: TDataFrame;
  lArray: TDataArray;
  lChildArray: TDataArray;
  i: Integer;
  j: Integer;
  lStart: TDateTime;
  lEnd: TDateTime;
begin
  lStart := now;
  lTop := TDataObj.Create;
  try
    lArray := lTop.AsArray;
    for i := 1 to cMakeTestSize do
    begin
      lFrame := lArray.NewSlot.AsFrame;
      lFrame.NewSlot('ID').AsInt64 := i*1000000;
      lFrame.NewSlot('FirstName').AsString := 'Sean';
      lFrame.NewSlot('LastName').AsString := 'Solberg';
      lFrame.NewSlot('Age').AsInteger := 18;
      lFrame.NewSlot('Height').AsDouble := 1234.56789;
      lFrame.NewSlot('DateTime').AsDateTime := now;
      lFrame.NewSlot('Description').AsString := 'This text is intentionally including " characters that need to be escaped. '+#13+#10+'Plus line feed characters too.'+#13+#10+#9+'This line starts with a tab character'+#13+#10+'This line ends with a higher unicode character "RollingEyes" Face Character 🙄';
      lChildArray := lFrame.NewSlot('Numbers').AsArray;
      for j := 0 to 999 do
      begin
        lChildArray.NewSlot.AsInteger := j;
        lChildArray.NewSlot.AsDouble := j*pi;
        lChildArray.NewSlot.AsString := 'CrazyStuff:#13+#10+"🙄"';
      end;
    end;

    case aType of
      cttTightUTF8: result := TJsonStreamer.DataObjToJson(lTop, cJsonTight, 2, false);
      cttTightAscii: result := TJsonStreamer.DataObjToJson(lTop, cJsonTight, 2, true);
      cttFormattedUTF8: result := TJsonStreamer.DataObjToJson(lTop, cJsonHumanReadable, 2, false);
      cttFormattedAscii: result := TJsonStreamer.DataObjToJson(lTop, cJsonHumanReadable, 2, true);
    end;
    lEnd := now;
  finally
    lTop.Free;
  end;

  m('Make DataObj to JSON-> Length:'+InttoStr(length(result))+' Time: '+FloatToStrf((lEnd-lStart)*24*60*60, ffFixed, 10, 2));
end;

function TForm15.MakeTestDDO(aType: TTestType): string;
var
  lTop: DataObjects.TDataObj;
  lFrame: DataObjects.TDataFrame;
  lArray: DataObjects.TDataArray;
  lChildArray: DataObjects.TDataArray;
  i: Integer;
  j: Integer;
  lStart: TDateTime;
  lEnd: TDateTime;
begin
  lStart := now;
  lTop := DataObjects.TDataObj.Create;
  try
    lArray := lTop.AsArray;
    for i := 1 to cMakeTestSize do
    begin
      lFrame := lArray.NewSlot.AsFrame;
      lFrame.NewSlot('ID').AsInt64 := i*1000000;
      lFrame.NewSlot('FirstName').AsString := 'Sean';
      lFrame.NewSlot('LastName').AsString := 'Solberg';
      lFrame.NewSlot('Age').AsInteger := 18;
      lFrame.NewSlot('Height').AsFloat := 1234.56789;
      lFrame.NewSlot('DateTime').AsDateTime := now;
      lFrame.NewSlot('Description').AsString := 'This text is intentionally including " characters that need to be escaped. '+#13+#10+'Plus line feed characters too.'+#13+#10+#9+'This line starts with a tab character'+#13+#10+'This line ends with a higher unicode character "RollingEyes" Face Character 🙄';
      lChildArray := lFrame.NewSlot('Numbers').AsArray;
      for j := 0 to 999 do
      begin
        lChildArray.NewSlot.AsInteger := j;
        lChildArray.NewSlot.AsFloat := j*pi;
        lChildArray.NewSlot.AsString := 'CrazyStuff:#13+#10+"🙄"';
      end;
    end;

    case aType of
      cttTightUTF8: result := lTop.JSON;       // this mechanism doesn't distinguish on character encoding options.
      cttTightAscii: result := lTop.JSON;
      cttFormattedUTF8: result := lTop.PrintToJSONReadable;
      cttFormattedAscii: result := lTop.PrintToJSONReadable;
    end;
    lEnd := now;
  finally
    lTop.Free;
  end;

  m('Make DDO to JSON-> Length:'+InttoStr(length(result))+' Time: '+FloatToStrf((lEnd-lStart)*24*60*60, ffFixed, 10, 2));
end;

function TForm15.MakeTestDelphi(aType: TTestType): string;
var
  lTop: TJSONArray;
  lFrame: TJSONObject;
  lChildArray: TJSONArray;
  i: Integer;
  j: Integer;
  lStart: TDateTime;
  lEnd: TDateTime;
  lInt64: Int64;
  lFloat: Double;

begin
  lStart := now;
  try
    lTop := TJSONArray.Create;
    for i := 1 to cMakeTestSize do
    begin
      lFrame := TJSONObject.Create;
      lTop.Add(lFrame);
      lInt64 := i*1000000;
      lFrame.AddPair('ID',lInt64) ;
      lFrame.AddPair('FirstName', 'Sean');
      lFrame.AddPair('LastName', 'Solberg');
      lFrame.AddPair('Age', 18);
      lFrame.AddPair('Height', 1234.56789);
      lFrame.AddPair('DateTime', now);
      lFrame.AddPair('Description','This text is intentionally including " characters that need to be escaped. '+#13+#10+'Plus line feed characters too.'+#13+#10+#9+'This line starts with a tab character'+#13+#10+'This line ends with a higher unicode character "RollingEyes" Face Character 🙄');
      lChildArray := TJSONArray.Create;
      lFrame.AddPair('Numbers',lChildArray);
      for j := 0 to 999 do
      begin
        lChildArray.Add(j);
        lFloat := j*pi;
        lChildArray.Add(lFloat);
        lChildArray.Add('CrazyStuff:#13+#10+"🙄"');
      end;
    end;

    case atype of
      cttTightUTF8: result := lTop.ToJSON([TJSONAncestor.TJSONOutputOption.EncodeBelow32]);
      cttTightAscii: result := lTop.ToJSON([TJSONAncestor.TJSONOutputOption.EncodeBelow32,TJSONAncestor.TJSONOutputOption.EncodeAbove127]);
      cttFormattedUTF8: result := lTop.Format(2);
      cttFormattedAscii: result := lTop.Format(2);   // Don't see a way to control the JSONOputputOptions with the format call.
    end;
    lEnd := now;
  finally
    lTop.Free;
  end;

  m('Make Delphi to JSON-> Length:'+InttoStr(length(result))+' Time: '+FloatToStrf((lEnd-lStart)*24*60*60, ffFixed, 10, 2));
end;

function TForm15.MakeTestGrijjy(aType: TTestType): string;
var
  lTop: TgoBsonArray;
  lFrame: TgoBsonDocument;
  lChildArray: TgoBsonArray;
  i: Integer;
  j: Integer;
  lStart: TDateTime;
  lEnd: TDateTime;
  lInt64: Int64;
  lFloat: Double;
  lSettings: TgoJsonWriterSettings;
begin
  lStart := now;
  try
    lTop := TgoBsonArray.Create;
    for i := 1 to cMakeTestSize do
    begin
      lFrame := TgoBsonDocument.Create;
      lTop.Add(lFrame);
      lInt64 := i*1000000;
      lFrame.Add('ID',lInt64) ;
      lFrame.Add('FirstName', 'Sean');
      lFrame.Add('LastName', 'Solberg');
      lFrame.Add('Age', 18);
      lFrame.Add('Height', 1234.56789);
      lFrame.Add('DateTime', now);
      lFrame.Add('Description','This text is intentionally including " characters that need to be escaped. '+#13+#10+'Plus line feed characters too.'+#13+#10+#9+'This line starts with a tab character'+#13+#10+'This line ends with a higher unicode character "RollingEyes" Face Character 🙄');
      lChildArray := TgoBsonArray.Create;
      lFrame.Add('Numbers',lChildArray);
      for j := 0 to 999 do
      begin
        lChildArray.Add(j);
        lFloat := j*pi;
        lChildArray.Add(lFloat);
        lChildArray.Add('CrazyStuff:#13+#10+"🙄"');
      end;
    end;

    case aType of
      cttTightUTF8: lSettings := TgoJsonWriterSettings.Create('', '', TgoJsonOutputMode.Strict);
      cttTightAscii: lSettings := TgoJsonWriterSettings.Create('', '', TgoJsonOutputMode.Strict);
      cttFormattedUTF8: lSettings := TgoJsonWriterSettings.Create('  ', #13#10, TgoJsonOutputMode.Strict);
      cttFormattedAscii: lSettings := TgoJsonWriterSettings.Create('  ', #13#10, TgoJsonOutputMode.Strict);
    end;

    result := lTop.ToJson(lSettings);
    lEnd := now;
  finally
//    lTop.Free;
  end;

  m('Make Grijjy to JSON-> Length:'+InttoStr(length(result))+' Time: '+FloatToStrf((lEnd-lStart)*24*60*60, ffFixed, 10, 2));
end;

{ TPerson }

function TPerson.getDisplayString: string;
begin
  result := fFirstName + ' '+fLastName;
end;





{$ifdef cIncludeDDOTest}
procedure OldDataObjToNewDataObj(aOldDataObj: dataObjects.TDataObj; aNewDataObj: dataObjects2.TDataObj);
var
  i: integer;
  lNewSlot: DataObjects2.TDataObj;
begin

  case aOldDataObj.DataType of
    cDataObjectSlotTypeString: aNewDataObj.AsString := aOldDataObj.AsString;
    cDataObjectSlotTypeInteger: aNewDataObj.AsInt32 := aOldDataObj.AsInteger;
    cDataObjectSlotTypeBoolean: aNewDataObj.AsBoolean := aOldDataObj.AsBoolean;
    cDataObjectSlotTypeFloat: aNewDataObj.AsDouble := aOldDataObj.AsFloat;
    cDataObjectSlotTypeDateTime: aNewDataObj.AsDateTime := aOldDataObj.AsDateTime;
    cDataObjectSlotTypeFrame: begin
      for i := 0 to aOldDataObj.AsFrame.Count-1 do
      begin
        lNewSlot := aNewDataObj.AsFrame.newSlot(aOldDataObj.AsFrame.SlotNames[i]);
        OldDataObjToNewDataObj(aOldDataObj.AsFrame.Slots[i], lNewSlot);    //recursion here.
      end;
    end;
    cDataObjectSlotTypeArray: begin
      for i := 0 to aOldDataObj.AsArray.count-1 do
      begin
        lNewSlot := aNewDataObj.AsArray.NewSlot;
        OldDataObjToNewDataObj(aOldDataObj.AsArray.Slots[i], lNewSlot);    // recursion here.
      end;
    end;
    cDataObjectSlotTypeSymbol: aNewDataObj.AsSymbol := aOldDataObj.AsSymbol;
    cDataObjectSlotTypeByte: aNewDataObj.AsByte := aOldDataObj.AsByte;
    cDataObjectSlotTypeBinary: begin
      aOldDataObj.AsMemStream.Seek(0, soFromBeginning);
      aNewDataObj.AsBinary.CopyFrom(aOldDataObj.AsMemStream, aOldDataObj.AsMemStream.Size);
    end;
    cDataObjectSlotTypeStringList: begin
      for i := 0 to aOldDataObj.AsStringList.Count-1 do
      begin
        aNewDataObj.AsStringList.Add( aOldDataObj.AsStringList.Strings[i] );
      end;
    end;
//      cDataObjectSlotTypeGeometry: (dataGeometry: TDataGeometry);
    cDataObjectSlotTypeInt64: aNewDataObj.AsInt64 := aOldDataObj.AsInt64;
  end;
end;
{$endif}


end.
