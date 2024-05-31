unit TestFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, DataObjects2Utils, DataObjects2, DataObjects2CBOR;

type
  TForm68 = class(TForm)
    btnRunTest: TButton;
    Memo1: TMemo;
    Button1: TButton;
    procedure btnRunTestClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure log(aStr: string);
  end;

var
  Form68: TForm68;

implementation

{$R *.dfm}

procedure TForm68.btnRunTestClick(Sender: TObject);
var
  lExp: Cardinal;
  lVal: Cardinal;
  lVal2: Cardinal;

  procedure Sweep;
  var
    i: cardinal;
    lInput: Single;
    lOutput: Cardinal;
    lReturned: Single;
    lExceptCount: integer;
  begin
    lExceptCount := 0;
    for i := 0 to $7FFFFF do   // All Fraction bit patterns checked.
    begin
      try
        if lExceptCount > 1000 then
          exit;

        lVal := i or lExp;
        lVal2 := SwapBytes(lVal);
        lInput := PSingle(@lVal)^;
        lOutput := SwapBytesSingle(lInput);
        lReturned := SwapBytesSingle(lOutput);

        if lInput <> lReturned then
        begin
          raise exception.Create('Single did NOT round-trip');
        end;
      except
        on e: exception do
        begin
          log(e.classname+' '+e.message+': input Value = '+IntToHex(lVal)+', expected output value = '+intToHex(lVal2));
          inc(lExceptCount);
        end;

      end;
    end;
  end;

begin
  Log('Sweep 1: Sign bit 0, Exponent = $00');
  lExp := $00000000;   // Sign bit 0, Exponent = $00
  Sweep;

  Log('Sweep 2: Sign bit 1, Exponent = $00');
  lExp := $80000000;   // Sign bit 1, Exponent = $00
  Sweep;

  // the following will produce invalid floating point numbers.
  Log('Sweep 3: Sign bit 0, Exponent = $FF');
  lExp := $7F000000;    // Sign bit 0, Exponent = $FF
  Sweep;

  Log('Sweep 4: Sign bit 1, Exponent = $FF');
  lExp := $FF000000;    // Sign bit 1, Exponent = $FF
  Sweep;

end;

procedure TForm68.Button1Click(Sender: TObject);
var
  lDataObj: TDataObj;
  lSingle: Single;
  lDouble: Double;
begin
  lSingle := 1.23456789123456789;
  lDouble := 1.23456789123456789;
  lDataObj:=TDataObj.Create;
  lDataObj.AsFrame.NewSlot('Single').AsSingle := lSingle;
  lDataObj.AsFrame.NewSlot('Double').AsDouble := lDouble;
  lDataObj.WriteToFile('c:\temp\TESTFloats.cbor');

  lDataObj.Clear;
  lDataObj.LoadFromFile('c:\temp\TESTFloats.cbor');

  if lDataObj.AsFrame.NewSlot('Single').AsSingle = lSingle then
    log('single did a successful round-trip')
  else
    log('Single did not match');

  if lDataObj.AsFrame.NewSlot('Double').AsDouble = lDouble then
    log('Double did a successful round-trip')
  else
    log('Double did not match');


end;

procedure TForm68.log(aStr: string);
begin
  memo1.Lines.Add(aStr);
end;

end.
