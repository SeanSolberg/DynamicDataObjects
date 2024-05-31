program TestFloatingPointSwapBytes;

uses
  Vcl.Forms,
  TestFrm in 'TestFrm.pas' {Form68};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm68, Form68);
  Application.Run;
end.
