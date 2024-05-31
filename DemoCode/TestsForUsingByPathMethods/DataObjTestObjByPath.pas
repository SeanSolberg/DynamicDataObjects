unit DataObjTestObjByPath;

interface

{ The purpose of this little test program is to test the usage of the NewSlotByPath and FindSlotByPath functions within TDataObj

}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, DataObjects2;

type
  TForm74 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    procedure log(aStr: string);
    procedure Test1;
    procedure Test2;
    procedure Test3;

  public
    { Public declarations }
  end;

var
  Form74: TForm74;

implementation

{$R *.dfm}

procedure TForm74.Button1Click(Sender: TObject);
begin
  Test1;
  Test2;
  Test3;
end;

procedure TForm74.Test1;
var
  lDataobj: TDataObj;
  lSlot: TDataObj;
begin
  log('TEST 1 - Test NewSlotByPath and FindSlotByPath on array and frame.');
  lDataobj:=TDataObj.create;
  try
    lDataObj.NewSlotByPath('Array\[0]\first\hello world\whatsup').AsString := 'Hello';
    lDataObj.NewSlotByPath('Array\[1]\second\hello world\whatsup').AsString := 'Hello';
    // Because we are skipping [2] and [3], when we make a [4] below, we will also be making a [2] and a [3]
    // that are each nil because we need them as placeholders to make 4
    lDataObj.NewSlotByPath('Array\[4]\fourth\hello world\whatsup').AsString := 'Hello';

    if lDataObj.FindSlotByPath('Array\[0]\First\hello world\whatsup', lSlot) then
      log('Correctly Found: '+lSlot.AsString)
    else
      log('INCORRECTLY Not Found');

    if lDataObj.FindSlotByPath('Array\[0]\First\hello world\NotGoingToFind', lSlot) then
      log(lSlot.AsString)
    else
      log('Correctly Not Found');

    log(lDataObj.PrintToString);
  finally
    lDataObj.Free;
  end;
  log('');
end;

procedure TForm74.Test2;
var
  lDataobj: TDataObj;
  lSlot: TDataObj;
begin
  log('TEST 2 - Same as test1, but with a custom delimeter.');
  lDataobj:=TDataObj.create;
  try
    lDataObj.NewSlotByPath('Array.[0].first.hello world.whatsup','.').AsString := 'Hello';
    lDataObj.NewSlotByPath('Array.[1].second.hello world.whatsup','.').AsString := 'Hello';
    // Because we are skipping [2] and [3], when we make a [4] below, we will also be making a [2] and a [3]
    // that are each nil because we need them as placeholders to make 4
    lDataObj.NewSlotByPath('Array.[4].fourth.hello world.whatsup','.').AsString := 'Hello';

    if lDataObj.FindSlotByPath('Array.[0].First.hello world.whatsup', lSlot,'.') then
      log('Correctly Found: '+lSlot.AsString)
    else
      log('INCORRECTLY Not FOUND');

    if lDataObj.FindSlotByPath('Array.[0].First.hello world.NotGoingToFind', lSlot,'.') then
      log(lSlot.AsString)
    else
      log('Correctly Not Found');

    log(lDataObj.PrintToString);
  finally
    lDataObj.Free;
  end;
  log('');
end;

procedure TForm74.Test3;
var
  lDataobj: TDataObj;
  lSlot: TDataObj;
begin
  log('TEST 3 - Test NewSlotByPath with a SparseArray');
  lDataobj:=TDataObj.create;
  try
    lDataObj.NewSlotByPath('SparseArray.(0).first.hello world.whatsup','.').AsString := 'Hello';
    lDataObj.NewSlotByPath('SparseArray.(1).second.hello world.whatsup','.').AsString := 'Hello';
    // Even though we are skipping [2] and [3], we can still make a [4] below because sparseArrays can skip array indexes.
    lDataObj.NewSlotByPath('SparseArray.(4).fourth.hello world.whatsup','.').AsString := 'Hello';

    if lDataObj.FindSlotByPath('SparseArray.(0).First.hello world.whatsup', lSlot,'.') then
      log('Correctly Found: '+lSlot.AsString)
    else
      log('INCORRECTLY Not FOUND');

    if lDataObj.FindSlotByPath('SparseArray.(0).First.hello world.NotGoingToFind', lSlot,'.') then
      log(lSlot.AsString)
    else
      log('Correctly Not Found');

    log(lDataObj.PrintToString);
  finally
    lDataObj.Free;
  end;
  log('');
end;



procedure TForm74.Button2Click(Sender: TObject);
var
  lDataObj: TDataObj;
  lSlot: TDataObj;
begin
  lDataObj:=TDataObj.Create;
  try
    with lDataObj.AsFrame do
    begin
      newslot('one').AsString := 'one';
      with newslot('array').AsArray do
      begin
        newslot.AsString := 'one';
        newslot.AsString := 'two';
        newslot.AsString := 'three';
        newslot.AsString := 'four';
        newslot.AsString := 'five';
        newslot.AsString := 'six';
      end;

      with newslot('Sparse').AsSparseArray do
      begin
        newslot(1).AsString := 'one';
        newslot(2).AsString := 'two';
        newslot(3).AsString := 'three';
        newslot(4).AsString := 'four';
        newslot(5).AsString := 'five';
        newslot(6).AsString := 'six';
      end;
    end;
    log(lDataObj.PrintToString);

    // now do some deletes
    with lDataObj.AsFrame do
    begin
      with newslot('array').AsArray do
      begin
        DeleteSlot(3);
      end;

      with newslot('Sparse').AsSparseArray do
      begin
        DeleteSlot(3);
      end;
      log(lDataObj.PrintToString);

      // Now test if we can properly not find 3 any more.
      if newSlot('Sparse').AsSparseArray.FindSlot(3, lSlot) then
      begin
        log('FOUND!  PROBLEM HERE');
      end
      else
      begin
        log('CORRECTLY NOT FOUND');
      end;
    end;


    // now let's re-add number 3
    lDataObj.NewSlotByPath('Sparse\(3)').AsString := 'three again';
    log(lDataObj.PrintToString);

  finally
    lDataObj.Free;
  end;
end;

procedure TForm74.log(aStr: string);
begin
  memo1.Lines.Add(aStr);
end;

end.
