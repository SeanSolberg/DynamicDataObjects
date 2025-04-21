unit TestCBOR;

interface

uses
  SysUtils, DUnitX.TestFramework, RTTI,
  DUnitX.Extensibility, DUnitX.Types, TestCBORData, DataObjects2, DataObjects2CBOR, DataObjects2JSON, DataObjConverters;

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

  TTestCBOR = class
  public
    procedure DecodeTest(const ArgsJSON: string);
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
  RttiType: TRttiType;
  RttiMethod: TRttiMethod;
  F: ITestFixture;
  TestParams: TValueArray;
  lAllTestObj: TDataObj;
  lTestObj: TDataObj;
  lcount: integer;
begin
  lcount := 0;
  lAllTestObj:=TDataObj.create;
  try
    lAllTestObj.json := cTestCBORJSON;

    RttiType := FRttiContext.GetType(TTestCBOR);
    RttiMethod := RttiType.GetMethod('DecodeTest');

    F := context.CreateFixture(TObject, 'CBOR', '');
    F := F.AddChildFixture(TTestCBOR, 'Test CBOR Decode', '');

    for lTestObj in lAllTestObj.AsArray do
    begin
      SetLength(TestParams, 1);
      TestParams[0] := TValue.From<string>(lTestObj.json);
      inc(lCount);
      F.AddTestCase('RunTest', intToStr(lCount), 'DecodeTest', '', RttiMethod, True, TestParams);
    end;
  finally
    lAllTestObj.free;
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


procedure TTestCBOR.DecodeTest(const ArgsJSON: string);
var
  lTestObj: TDataObj;
  lCbor: TDataObj;
  lPass: boolean;
  lCborStr: string;
  lDiag: TDataObj;
  lDiagStr: string;
  lDecodedStr: string;
  lExceptionObj: TDataObj;
const
  crlf = #13+#10;
begin
  lCbor := TDataObj.create;
  lTestObj := TDataObj.create;
  try
    lTestObj.Json := ArgsJSON;

    TConvertBase64StringToBinary.ExecuteConversion(lTestObj['cbor']);  // Convert the base64 text to actual binary.

    lPass := true;  // assume we have a success unless checking logic below signals a failure
    try
      lCbor.ReadFromStream( lTestObj['cbor'].AsBinary, TCBORStreamer);

      lCborStr := lCbor.PrintToString;
      lCborStr := StringReplace(lCborStr, 'simple(23)', 'undefined', [rfIgnoreCase]);
      // We either have a "decoded" field or we have a diagnostic field.
      if lTestObj.AsFrame.findSlot('diagnostic',lDiag) then
      begin
        lDiagStr := lDiag.AsString;
        lDiagStr := StringReplace(lDiagStr, '-Infinity','-INF', [rfIgnoreCase]);
        lDiagStr := StringReplace(lDiagStr, 'Infinity','INF', [rfIgnoreCase]);
        lDiagStr := StringReplace(lDiagStr, '-Nan','-NAN', [rfIgnoreCase]);
        lDiagStr := StringReplace(lDiagStr, 'Nan','NAN', [rfIgnoreCase]);

        // adjustments for matching the Diagnostic notation with the printToString dealing with space matching aournd frames.
        lDiagStr := StringReplace(lDiagStr, crlf,'', [rfIgnoreCase, rfReplaceAll]);
        lDiagStr := StringReplace(lDiagStr, '{ ','{', [rfIgnoreCase, rfReplaceAll]);
        lDiagStr := StringReplace(lDiagStr, '[ ','[', [rfIgnoreCase, rfReplaceAll]);
        lDiagStr := StringReplace(lDiagStr, ',  ',',', [rfIgnoreCase, rfReplaceAll]);
        lDiagStr := StringReplace(lDiagStr, ', ',',', [rfIgnoreCase, rfReplaceAll]);
        lDiagStr := StringReplace(lDiagStr, ': ',':', [rfIgnoreCase, rfReplaceAll]);

        lCborStr := StringReplace(lCborStr, crlf,'', [rfIgnoreCase, rfReplaceAll]);
        lCborStr := StringReplace(lCborStr, '{ ','{', [rfIgnoreCase, rfReplaceAll]);
        lCborStr := StringReplace(lCborStr, '[ ','[', [rfIgnoreCase, rfReplaceAll]);
        lCborStr := StringReplace(lCborStr, ',  ',',', [rfIgnoreCase, rfReplaceAll]);
        lCborStr := StringReplace(lCborStr, ', ',',', [rfIgnoreCase, rfReplaceAll]);
        lCborStr := StringReplace(lCborStr, ': ',':', [rfIgnoreCase, rfReplaceAll]);

        // Adjustments for matching the diagnostic notation around binary.  note that the diagnostic notation shows the _ character for "indefinite" bytes header and multiple blocks can be stitched together
        lDiagStr := StringReplace(lCborStr, ' ','', [rfIgnoreCase, rfReplaceAll]);
        lDiagStr := StringReplace(lCborStr, '_','', [rfIgnoreCase, rfReplaceAll]);
        lDiagStr := StringReplace(lCborStr, '(','', [rfIgnoreCase, rfReplaceAll]);
        lDiagStr := StringReplace(lCborStr, ')','', [rfIgnoreCase, rfReplaceAll]);
        lDiagStr := StringReplace(lCborStr, ''',h''','', [rfIgnoreCase, rfReplaceAll]);


        if not SameText(trim(lCborStr),lDiagStr) then
        begin
          Assert.Fail('DIFFERENCE'+crlf+lCbor.PrintToString+crlf+lTestObj.printToString);
          lPass := false;
        end;
      end
      else
      begin
        // now compare to see if the CBOR read gave us the same data content as the JSON original
        lDecodedStr:=lTestObj['decoded'].PrintToString;

        if lCborStr <> lDecodedStr then
        begin
          lDecodedStr := StringReplace(lDecodedStr, '"','',[rfReplaceAll]);      // make a second attempt to compare without possible double quotes
          if lCborStr <> lDecodedStr then
          begin
            Assert.Fail('DIFFERENCE'+crlf+lCbor.PrintToString+crlf+lTestObj.printToString);
            lPass := false;
          end;
        end;
      end;
    except
      on e: exception do
      begin
        // We can define that a particular test is expected to generate a CBOR parsing error.  If that is the case, then we have a valid successfull test
        // because the exception was expected.
        if lTestObj.AsFrame.FindSlot('ExpectedException', lExceptionObj) then
        begin
          if e.classname+': '+e.message <> lExceptionObj.AsString then
          begin
            Assert.Fail('EXCEPTION: '+e.classname+' '+e.message);
            lPass := false;
          end;
        end
        else
        begin
          Assert.Fail('EXCEPTION: '+e.classname+' '+e.message);
          lPass := false;
        end;
      end;
    end;

    if lPass then
      Assert.pass('PASS');

  finally
    lCbor.free;
    lTestObj.free;
  end;
end;



initialization
  TDUnitX.RegisterPlugin(TFixturesProviderPlugin.Create);

end.
