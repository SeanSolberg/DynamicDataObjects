unit IteratorTestFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, DataObjects2;

type
  TIteratorTestForm = class(TForm)
    btnTest: TButton;
    Memo1: TMemo;
    procedure btnTestClick(Sender: TObject);
  private
    { Private declarations }
    procedure log(aStr: string);
  public
    { Public declarations }
  end;

var
  IteratorTestForm: TIteratorTestForm;

implementation

{$R *.dfm}

procedure TIteratorTestForm.btnTestClick(Sender: TObject);
var
  lDataObj: TDataObj;
  lArraySlot: TDataObj;
  lFrameSlot: TDataFrameEnumeratorRec;
begin
  lDataObj := TDataObj.Create;
  try
    // Run a test on the Array Iterator
    lDataObj.AsArray.NewSlot.AsString :='One';
    lDataObj.AsArray.NewSlot.AsString :='Two';
    lDataObj.AsArray.NewSlot.AsString :='Three';

    for lArraySlot in lDataObj.AsArray do
    begin
       log(lArraySlot.AsString);
    end;


    log('');
    lDataObj.Clear;

    // Run a test on the Frame Iterator.
    // Since each item in a frame has two pieces:  a Slotname and a contained TDataObj, we need TDataFrameEnumeratorRec to hold these two pieces for us.
    lDataObj.AsFrame.NewSlot('One').AsString :='One Data';
    lDataObj.AsFrame.NewSlot('Two').AsString :='Two Data';
    lDataObj.AsFrame.NewSlot('Three').AsString :='Three Data';

    for lFrameSlot in lDataObj.AsFrame do
    begin
      log(lFrameslot.Slotname+': '+lFrameSlot.DataObj.AsString);
    end;
  finally
    lDataObj.Free;
  end;
end;

procedure TIteratorTestForm.log(aStr: string);
begin
  memo1.lines.add(aStr);
end;

end.
