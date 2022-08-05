program AssignToAssignFromTestApp;

uses
  Vcl.Forms,
  TestFrm in 'TestFrm.pas' {AssignToAssignFromTestForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TAssignToAssignFromTestForm, AssignToAssignFromTestForm);
  Application.Run;
end.
