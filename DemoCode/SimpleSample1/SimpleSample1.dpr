program SimpleSample1;

uses
  Vcl.Forms,
  SimpleSample1Frm in 'SimpleSample1Frm.pas' {SimpleSample1Form};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TSimpleSample1Form, SimpleSample1Form);
  Application.Run;
end.
