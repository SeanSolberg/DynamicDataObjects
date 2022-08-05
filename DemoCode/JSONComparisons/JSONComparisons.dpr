program JSONComparisons;

uses
  Vcl.Forms,
  JSONComparisonMainFrm in 'JSONComparisonMainFrm.pas' {Form15},
  DataObjects2JSON in '..\..\..\commonSource\src\DataObjects2JSON.pas',
  DataObjects in '..\..\..\commonSource\src\DataObjects.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm15, Form15);
  Application.Run;
end.
