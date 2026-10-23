program PerformanceTester;

uses
  Vcl.Forms,
  PerformanceTesterFrm in 'PerformanceTesterFrm.pas' {Form19},
  DataObjects2CBOR in '..\..\src\DataObjects2CBOR.pas',
  DataObjects2DDO in '..\..\src\DataObjects2DDO.pas',
  DataObjects2ION in '..\..\src\DataObjects2ION.pas',
  DataObjects2JSON in '..\..\src\DataObjects2JSON.pas',
  DataObjects2MsgPack in '..\..\src\DataObjects2MsgPack.pas',
  DataObjects2Smile in '..\..\src\DataObjects2Smile.pas',
  DataObjects2 in '..\..\src\DataObjects2.pas',
  DataObjects2CSV in '..\..\src\DataObjects2CSV.pas',
  DataObjects2BSON in '..\..\src\DataObjects2BSON.pas',
  DataObjects2Streamers in '..\..\src\DataObjects2Streamers.pas',
  DataObjects2Utils in '..\..\src\DataObjects2Utils.pas',
  DataObjects2UBJSON in '..\..\src\DataObjects2UBJSON.pas',
  VarInt in '..\..\src\VarInt.pas',
  StringBTree in '..\..\src\StringBTree.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm19, Form19);
  Application.Run;
end.
