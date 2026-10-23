program IteratorTestApp;

uses
  Vcl.Forms,
  IteratorTestFrm in 'IteratorTestFrm.pas' {IteratorTestForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TIteratorTestForm, IteratorTestForm);
  Application.Run;
end.
