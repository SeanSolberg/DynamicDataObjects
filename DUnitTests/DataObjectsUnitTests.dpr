program DataObjectsUnitTests;
{$R *.res}
uses
  Vcl.Forms,
  DUnitX.Loggers.GUI.VCL,
  TestCBORData in 'TestCBORData.pas',
  TestCBOR in 'TestCBOR.pas',
  TestDataObjects in 'TestDataObjects.pas';

begin
  Application.Initialize;
  Application.Title := 'DUnitX';
  Application.CreateForm(TGUIVCLTestRunner, GUIVCLTestRunner);
  Application.Run;

end.
