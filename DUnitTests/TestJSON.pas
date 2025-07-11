unit TestJSON;

interface

uses
  SysUtils, System.Types, DUnitX.TestFramework, DUnitX.DUnitCompatibility, DUnitX.Extensibility, DUnitX.Types, RTTI,
  Generics.collections, classes, DataObjects2, DataObjects2Streamers, DataObjects2JSON;

(* Note:  this test fixture will load a dataObj resource that defines the JSON serialization test cases.  Then, this fixture
          will dynamically add the test cases based on the list of tests defined in the dataObj file.   To add more test cases
          you will need to add more items to the validates.dataObj file that is linked into this build as a resource.  Some
          of the test cases are expected to be positive valid JSON decodes.  Some are expected to fail JSON serialization and
          thus the test case will expect a fail.  In this case, the test case actually results in a success then.*)

{$M+}

type
  TFixturesProviderPlugin = class(TInterfacedObject, IPlugin)
    procedure GetPluginFeatures(const context: IPluginLoadContext);
  end;

  TFixturesProvider = class(TInterfacedObject, IFixtureProvider)
  private class var
    FRttiContext: TRttiContext;
  protected
    procedure Execute(const context: IFixtureProviderContext);
  public
    class constructor InitContext;
    class destructor FreeContext;
  end;


  TTestJSON = class(TTestCase)
  private
    fDataObj: TDataObj;
    procedure LoadTests;
  public
    destructor Destroy; override;
  published
    procedure TestJSONParsing(aIndex: integer);
  end;

implementation

{ TFixturesProviderPlugin }

procedure TFixturesProviderPlugin.GetPluginFeatures(const context: IPluginLoadContext);
begin
  context.RegisterFixtureProvider(TFixturesProvider.Create);
end;

{ TFixturesProvider }

procedure TFixturesProvider.Execute(const context: IFixtureProviderContext);
var
  i: integer;
  RttiType: TRttiType;
  RttiMethod: TRttiMethod;
  F: ITestFixture;
  TestParams: TValueArray;

  lTestJSON: TTestJSON;
  lTests: TDataArray;
  lDesc: string;
begin
  lTestJSON := TTestJSON.create;
  try
    lTestJSON.LoadTests;

    RttiType := FRttiContext.GetType(TTestJSON);
    RttiMethod := RttiType.GetMethod('TestJSONParsing');

    F := context.CreateFixture(TObject, 'JSON', '');
    F := F.AddChildFixture(TTestJSON, 'Test JSON Decode', '');

    lTests := lTestJSON.fDataObj.AsArray;
    for i := 0 to lTests.count-1 do
    begin
      SetLength(TestParams, 1);
      TestParams[0] := TValue.From<integer>(i);
      lDesc := lTests[i].AsFrame.newSlot('desc').AsString;
      if lDesc ='' then
        lDesc := 'Test '+IntToStr(i);
      F.AddTestCase('TestJSONParsing', lDesc, 'Test'+IntToStr(i), '', RttiMethod, True, TestParams);
    end;

  finally
    lTestJSON.free;
  end;
end;

class constructor TFixturesProvider.InitContext;
begin
  FRttiContext := TRttiContext.Create;
end;

class destructor TFixturesProvider.FreeContext;
begin
  FRttiContext.Free;
end;


procedure TTestJSON.TestJSONParsing(aIndex: integer);
var
  lTestFrame: TDataFrame;
  lDataObj: TDataObj;
  lStreamer: TJSONStreamer;
  lDesc: string;
begin
  LoadTests;
  lTestFrame := fDataObj.AsArray.slots[aIndex].AsFrame;
  lStreamer:=TJSONStreamer.create(lTestFrame['file'].AsBinary);    // make a streamer linked to the Binary slot

  // Setup the JSON parser so that it has more strict JSON parsing rules.
  lStreamer.AllowParsingExtendedTypes := false;
  lStreamer.AllowParsingSymbols := false;
  lStreamer.AllowSingleQuoteStrings := false;
  lStreamer.AllowOverwritingExistingSlots := false;
  lStreamer.AllowParsingLeadingZeros := false;
  lStreamer.SlotnameIsCaseSensitive := true;

  // Pull out the one test data object that we are going to run this test against,  it should be a binary slot that contains the JSON stream we are going to test.

  if lTestFrame['PassOrFail'].AsString = 'PASS' then
  begin
    // This test case is expected to pass.  If it doesn't pass then we have a failed test.
    lDataObj:=TDataObj.create;
    try
      lDesc := lTestFrame['desc'].AsString;
      lStreamer.Decode(lDataObj);
    except
      on e: exception do
      begin
        assert.Fail(e.message +' on test '+lDesc);
      end;
    end;

    Assert.pass(lDesc);
  end
  else
  begin
    // This test case is expected to fail.  If it doesn't fail then we have a failed test.
    lDataObj:=TDataObj.create;
    try
      lDesc := lTestFrame['desc'].AsString;
      lStreamer.Decode(lDataObj);
    except
      on e: exception do
      begin
        assert.Pass(e.message +' on test '+lDesc);
      end;
    end;

    Assert.Fail('Test should have raised an exception but it didn''t: '+lDesc);
  end;
end;

{ TestVarInts }

destructor TTestJSON.Destroy;
begin
  fDataObj.free;
  inherited;
end;


procedure TTestJSON.LoadTests;
var
  lStream: TResourceStream;
begin
  if not assigned(fDAtaObj) then
  begin
    lStream := TResourceStream.Create(HInstance, 'JSONTESTS', RT_RCDATA);
    try
      fDataObj := TDataObj.Create;
      fDataObj.ReadFromStream(lStream, TDataObjStreamer);
    finally
      lStream.Free;
    end;
  end;
end;

initialization

  // Register any test cases with the test runner
  TDUnitX.RegisterPlugin(TFixturesProviderPlugin.Create);

end.

