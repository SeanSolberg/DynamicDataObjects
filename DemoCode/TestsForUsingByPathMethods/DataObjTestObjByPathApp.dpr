program DataObjTestObjByPathApp;

uses
  Vcl.Forms,
  DataObjTestObjByPath in 'DataObjTestObjByPath.pas' {Form74};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm74, Form74);
  Application.Run;
end.
