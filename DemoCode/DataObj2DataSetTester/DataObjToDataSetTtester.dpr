program DataObjToDataSetTtester;

uses
  Vcl.Forms,
  TestForm in 'TestForm.pas' {Form41},
  DataObjects2DataSet in '..\..\src\DataObjects2DataSet.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm41, Form41);
  Application.Run;
end.
