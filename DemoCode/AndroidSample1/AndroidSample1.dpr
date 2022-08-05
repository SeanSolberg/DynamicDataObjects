program AndroidSample1;

uses
  System.StartUpCopy,
  FMX.Forms,
  AndroidSampleFrm in 'AndroidSampleFrm.pas' {Form25},
  DataObjects2 in '..\..\src\DataObjects2.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm25, Form25);
  Application.Run;
end.
