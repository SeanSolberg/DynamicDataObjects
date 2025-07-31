program DataObjectsUnitTests;
{$R *.res}


{$R *.dres}

uses
  Vcl.Forms,
  DUnitX.Loggers.GUI.VCL,
  TestCBORData in 'TestCBORData.pas',
  TestCBOR in 'TestCBOR.pas',
  TestDataObjects in 'TestDataObjects.pas',
  TestJSON in 'TestJSON.pas',
  IndexMap in '..\src\IndexMap.pas';

begin
  Application.Initialize;
  Application.Title := 'DUnitX';
  Application.CreateForm(TGUIVCLTestRunner, GUIVCLTestRunner);
  Application.Run;

end.
