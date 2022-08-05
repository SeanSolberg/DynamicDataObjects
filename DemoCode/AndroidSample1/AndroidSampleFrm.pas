unit AndroidSampleFrm;

{This is a very simple firemonkey app to test DataObjects2.pas to be able to write out a sample DataObj file and an equivalent JSON file.
 Then, it can read those two files back and publish the results to a Memo box.  This app should work on windows and android. }

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, DataObjects2, System.IOUtils,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation, FMX.StdCtrls,
  DataObjects2JSON, FMX.Memo.Types;

type
  TForm25 = class(TForm)
    btnTest: TButton;
    Memo1: TMemo;
    Button1: TButton;
    procedure btnTestClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form25: TForm25;

implementation

{$R *.fmx}

procedure TForm25.btnTestClick(Sender: TObject);
var
  lDataobj: TDataobj;
  lFilename: string;
//  lFS: TFileStream;
begin
  lDataobj:=TDataobj.create;
  try
    with lDataObj.AsFrame do
    begin
      newSlot('Hello').AsString := 'World';
      newslot('BigNum').AsInt64 := 1234567890;
      with newslot('nested').AsArray do
      begin
        with newSlot do
        begin
          AsFrame['one'].AsString := 'one';
          AsFrame['two'].AsString := 'two';
          AsFrame['three'].AsString := 'three';
        end;
      end;
    end;
    memo1.Lines.Add(lDataObj.PrintToString);


    lFilename := System.IOUtils.TPath.Combine(System.IOUtils.tpath.getdocumentspath,'DataObj.dataObj');
    memo1.Lines.Add(lFilename);
    lDataObj.WriteToFile(lFilename); // NOTE THAT THIS AUTOMATICALLY CHOOSES THE RIGHT SERIALIZER BASED ON THE FILE EXTENSION

    lFilename := System.IOUtils.TPath.Combine(System.IOUtils.tpath.getdocumentspath,'DataObj.json');
    memo1.Lines.Add(lFilename);
    lDataObj.WriteToFile(lFilename); // NOTE THAT THIS AUTOMATICALLY CHOOSES THE RIGHT SERIALIZER BASED ON THE FILE EXTENSION

  finally
    lDataobj.free;
  end;
end;

procedure TForm25.Button1Click(Sender: TObject);
var
  lDataobj: TDataobj;
  lSS: TStringStream;
begin
  lDataobj:=TDataobj.create;
  try
    memo1.Lines.clear;

    try
      lDataObj.LoadFromFile(System.IOUtils.TPath.Combine(System.IOUtils.tpath.getdocumentspath,'DataObj.dataObj'));
      memo1.Lines.Add(lDataObj.PrintToString);
    except
      on e: exception do
        memo1.Lines.add(e.message);
    end;

    try
      lDataObj.Clear;
      lDataObj.LoadFromFile(System.IOUtils.TPath.Combine(System.IOUtils.tpath.getdocumentspath,'DataObj.json'));
      memo1.Lines.Add(lDataObj.PrintToString);
    except
      on e: exception do
        memo1.Lines.add(e.message);
    end;

    lSS:=TStringStream.create;
    lSS.LoadFromFile(System.IOUtils.TPath.Combine(System.IOUtils.tpath.getdocumentspath,'DataObj.json'));
    memo1.Lines.Add(lSS.DataString);

  finally
    lDataobj.free;
  end;
end;

end.
