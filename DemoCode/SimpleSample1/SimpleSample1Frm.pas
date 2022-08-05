unit SimpleSample1Frm;

{ Sample application written by Sean Solberg to show some examples of working with the DataObjects2 library. The only source file from the DataObjects2 code library
  is DataObjects2.pas. We are not doing any serialization in these examples as this set of examples just shows how to work with the TDataObj objects in code. }

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, DataObjects2;

type
  TSimpleSample1Form = class(TForm)
    Memo1: TMemo;
    btnGo1: TButton;
    tnGo2: TButton;
    btnGo3: TButton;
    btnGo4: TButton;
    btnGo5: TButton;
    btnGo6: TButton;
    Label1: TLabel;
    btnGo7: TButton;
    Button1: TButton;
    procedure btnGo1Click(Sender: TObject);
    procedure tnGo2Click(Sender: TObject);
    procedure btnGo3Click(Sender: TObject);
    procedure btnGo4Click(Sender: TObject);
    procedure btnGo5Click(Sender: TObject);
    procedure btnGo6Click(Sender: TObject);
    procedure btnGo7Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    procedure pub(aStr: string);
    procedure m(aStr: string);
  public
    { Public declarations }
  end;

var
  SimpleSample1Form: TSimpleSample1Form;

implementation

{$R *.dfm}




procedure TSimpleSample1Form.btnGo1Click(Sender: TObject);
var
  lDataObj: TDataObj;
begin
  // This example shows how to create a data object and make it be a frame("object").
  // Then, it adds a few slots to the frame, each with a different data type.
  // Finally, it prints a text representation of the dataObject contents
  lDataObj:=TDataObj.create;
  try
    lDataObj.AsFrame.NewSlot('FirstName').AsString := 'Sean';
    lDataObj.AsFrame.NewSlot('LastName').AsString := 'Solberg';
    lDataObj.AsFrame.NewSlot('LikesIceCream').AsBoolean := true;
    lDataObj.AsFrame.NewSlot('InchHeight').AsInt32 := 73;
    lDataObj.AsFrame.NewSlot('FeetHeight').AsDouble := 6.083333333333333;
    pub(lDataObj.PrintToString);     // Note that the output of this PrintToString call looks like JSON, but it is NOT JSON.  There is a specific Serializer class for encoding to JSON.
  finally
    lDataObj.Free;
  end;
end;

procedure TSimpleSample1Form.tnGo2Click(Sender: TObject);
var
  lDataObj: TDataObj;
  lDataFrame: TDataFrame;
begin
  // This example shows how to create a data object and make it be a frame, which is similar to example #1 above.
  // However, in this sample, once we get the dataObject as a frame, we put the frame into a local variable so we don't have to keep asking the dataObject for the frame it contains
  // Then, it adds a few slots to the frame, each with a different data type.
  // Finally, it prints a text representation of the dataObject contents
  lDataObj:=TDataObj.create;
  try
    lDataFrame := lDataObj.AsFrame;
    lDataFrame.NewSlot('FirstName').AsString := 'Sean';
    lDataFrame.NewSlot('LastName').AsString := 'Solberg';
    lDataFrame.NewSlot('LikesIceCream').AsBoolean := true;
    lDataFrame.NewSlot('InchHeight').AsInt32 := 73;
    lDataFrame.NewSlot('FeetHeight').AsDouble := 6.083333333333333;
    pub(lDataObj.PrintToString);
  finally
    lDataObj.Free;
  end;
end;

procedure TSimpleSample1Form.btnGo3Click(Sender: TObject);
var
  lDataObj: TDataObj;
begin
  // This example shows how to create a data object and make it be a frame.
  // Then, it adds a few slots to the frame using the "with" operator each with a different data type.
  // Some people frown on the with operator, but it kinda has a decent purpose here.
  // Finally, it prints a text representation of the dataObject contents
  lDataObj:=TDataObj.create;
  try
    with lDataObj.AsFrame do
    begin
      NewSlot('FirstName').AsString := 'Sean';
      NewSlot('LastName').AsString := 'Solberg';
      NewSlot('LikesIceCream').AsBoolean := true;
      NewSlot('InchHeight').AsInt32 := 73;
      NewSlot('FeetHeight').AsDouble := 6.083333333333333;
    end;
    pub(lDataObj.PrintToString);
  finally
    lDataObj.Free;
  end;
end;

procedure TSimpleSample1Form.btnGo4Click(Sender: TObject);
var
  lDataObj: TDataObj;
  lDataObj2: TDataObj;
begin
  // This example shows how to create a data object and make it be a frame.
  // Then, it adds a few slots to the frame, each with a different data type.
  // then, we will copy it to a new data object.  Essentially, create a clone of the original.
  // Finally, it will print a text representation of the copied dataObject contents
  lDataObj:=TDataObj.create;
  lDataObj2:=TDataObj.create;
  try
    with lDataObj.AsFrame do
    begin
      NewSlot('FirstName').AsString := 'Sean';
      NewSlot('LastName').AsString := 'Solberg';
      NewSlot('LikesIceCream').AsBoolean := true;
      NewSlot('InchHeight').AsInt32 := 73;
      NewSlot('FeetHeight').AsDouble := 6.083333333333333;
    end;
    lDataObj2.CopyFrom(lDataObj);
    pub(lDataObj2.PrintToString);
  finally
    lDataObj2.free;
    lDataObj.Free;
  end;
end;

procedure TSimpleSample1Form.btnGo5Click(Sender: TObject);
var
  lDataObj: TDataObj;
  lDataObj2: TDataObj;
begin
  // This example shows how to create a data object and make it be a frame.
  // Then, it adds a few slots to the frame, each with a different data type.
  // With that frame, we will then copy it into another dataObject a few times as items in an array.  This is a common pattern:  array of frames.
  // Finally, it prints a text representation of the dataObject contents
  lDataObj:=TDataObj.create;
  lDataObj2:=TDataObj.create;
  try
    with lDataObj.AsFrame do
    begin
      NewSlot('ID').AsInt64 := 12345677;
      NewSlot('FirstName').AsString := 'Sean';
      NewSlot('LastName').AsString := 'Solberg';
      NewSlot('LikesIceCream').AsBoolean := true;
      NewSlot('InchHeight').AsInt32 := 73;
      NewSlot('FeetHeight').AsDouble := 6.083333333333333;
    end;

    lDataObj2.AsArray.NewSlot.CopyFrom(lDataObj);          // make dataObj2 into an array with one new slot that then gets a clone of lDataObj

    lDataObj.AsFrame.NewSlot('ID').AsInt64 := 12345678;    // update the ID slot with a new value
    lDataObj2.AsArray.NewSlot.CopyFrom(lDataObj);          // make another clone into a new slot in the array

    lDataObj.AsFrame.NewSlot('ID').AsInt64 := 12345679;    // update the ID slot with a new value
    lDataObj2.AsArray.NewSlot.CopyFrom(lDataObj);          // make another clone into a new slot in the array

    pub(lDataObj2.PrintToString);
  finally
    lDataObj2.Free;
    lDataObj.Free;
  end;
end;

procedure TSimpleSample1Form.btnGo6Click(Sender: TObject);
var
  lDataObj: TDataObj;
  i: Integer;
  lSlot: TDataObj;

  function DigitText(aDigit: integer): string;
  begin
    case aDigit of
      0: result := 'zero';
      1: result := 'one';
      2: result := 'two';
      3: result := 'three';
      4: result := 'four';
      5: result := 'five';
      6: result := 'six';
      7: result := 'seven';
      8: result := 'eight';
      9: result := 'nine';
    else
      result := 'too high';
    end;
  end;

  procedure publish;
  var
    i: Integer;
  begin
    for i := 0 to lDataObj.AsArray.Count-1 do
    begin
      lSlot := lDataObj.AsArray.Slots[i];
      m(lSlot.DataTypeString+': '+lSlot.AsString);    // calling lSlot.AsString gets the value as a string, but doesn't convert the internal value to a string
    end;
  end;

begin
  // Not every top level object has to be a frame.  It can be an array too.
  // Make an array of values organized into triples where each triple has a number and a string and a double floating point value
  // This shows the concept that each item in an array can be a different data type
  // Finally, this prints a text representation of the dataObject contents which illustrates three things:
  //  1.  how to iterate through the items in an array
  //  2.  that you can ask a dataObject for its data type (as a string or as a dataType enumeration)
  //  3.  that you can get the value of a dataObject as a string, and if the value can be converted to a string, the string is returned without changing the dataObject's internal dataType
  lDataObj:=TDataObj.create;
  try
    for i := 0 to 5 do
    begin
      lDataObj.AsArray.NewSlot.AsInt32 := i;
      lDataObj.AsArray.NewSlot.AsString := DigitText(i);
      lDataObj.AsArray.NewSlot.AsDouble := i * 1.1;
    end;

    memo1.Lines.Clear;
    Publish;
    m('');
    Publish;    // publish a second time just to show that the dataTypes in the dataObject did not change even though in the publish procedure we were calling .AsString on each slot even if they didn't hold a string.
  finally
    lDataObj.Free;
  end;
end;

procedure TSimpleSample1Form.btnGo7Click(Sender: TObject);
var
  lDataObj: TDataObj;
begin
  // This example shows that you can create a dataObject that simply holds an atomic piece of data.
  // It also shows that you can change the data type of the dataObject by writing new data to it.
  lDataObj:=TDataObj.create;
  try
    memo1.Lines.Clear;
    lDataObj.AsString := 'Hello World';
    m(lDataObj.PrintToString);

    lDataObj.AsInt64 := 2 * 3959{radius in miles} * 5248{feet in a mile};
    m(lDataObj.PrintToString);

    lDataObj.AsString := 'Is the diameter of the earth in feet';
    m(lDataObj.PrintToString);
  finally
    lDataObj.Free;
  end;
end;

procedure TSimpleSample1Form.Button1Click(Sender: TObject);
var
  lDataObj: TDataObj;
begin
  // This example shows that you can create a dataObject using a more compact mechanism to create it.
  // The delphi "array syntax" can be used to create slots in frames and to create items in arrays.
  // If you pass a string into the array accessor [], then it will treat the data object variable as a frame.
  // If you pass an integer into the array accessor [], then it will treat the data object variable as an array.
  // of course, if the data object variable is already a different data type, then it will try to convert that
  // data to the new frame or array if it can or it will clear out the data that was there and replace it with a new frame or array.
  lDataObj:=TDataObj.create;
  try
    memo1.Lines.Clear;

    // TEST1 - Make lDataObj be a simple string
    lDataObj.AsString := 'Sean Solberg';   // Makes the lDataObj be a string.
    m(lDataObj.PrintToString);
    m('');



    // TEST2 - Use simple array notation to make a frame of frames.
    lDataObj['MyFrame1']['Slot1'].AsString := 'Hello';
//                          ^------------------------------ Makes the object returned from lDataObj['MyFrame1'] into a frame and adds new slot "Slot1" to that frame.
//               ^----------------------------------------- Makes the lDataObj be a frame which clears out the previous string value assigned above and adds slot "MyFrame1" to it
    lDataObj['MyFrame1']['Slot2'].AsString := 'World';
//                          ^------------------------------ Adds a new slot called "Slot2" to the frame returned from lDataObj['MyFrame1']
//               ^----------------------------------------- Simply returns slot "MyFrame1" from the lDataObj which is a frame that was already created above

    lDataObj['MyFrame2']['Slot1'].AsString := 'Happy';
    lDataObj['MyFrame2']['Slot2'].AsString := 'Day';

    m(lDataObj.PrintToString);
    m('');


    // TEST 3 - access a slot in a frame using an index instead of a slotname
    lDataObj['MyFrame2'][0].AsString := 'Not a Happy';
//                       ^--------------------------------- Tries to treat lDataObj['MyFrame2'] as an array by accessing the 0th item.  However, it is a frame already because of the previous line of code
//                                                          So, the slot that is accessed is the first slot (index 0) in that frame and sets it to the new string value of "Not a Happy Day"
    m(lDataObj.PrintToString);
    m('');


    // TEST 4 - create another item in the top level frame but make it an array instead.
    lDataObj['MyArray1'].AsArray.NewSlot.AsString := 'ONE';
//                         ^--------------------------------- Makes the object returned from lDataObj['MyArray1'] into an array
    lDataObj['MyArray1'].AsArray.NewSlot.AsString := 'TWO';
    lDataObj['MyArray1'].AsArray.NewSlot.AsString := 'THREE';

    lDataObj['MyArray1'][0].AsString := 'CHANGED ONE';        // change the values
    lDataObj['MyArray1'][1].AsString := 'CHANGED TWO';
    lDataObj['MyArray1'][2].AsString := 'CHANGED THREE';
    m(lDataObj.PrintToString);


    //TEST 5 - This will generate an exception because we are trying to access an item in an array that doesn't exist.
    lDataObj['MyArray1'][3].AsString := 'TRY TO ADD';
  finally
    lDataObj.Free;
  end;
end;

procedure TSimpleSample1Form.m(aStr: string);
begin
  memo1.Lines.Add(aStr);
end;

procedure TSimpleSample1Form.pub(aStr: string);
begin
  memo1.Lines.BeginUpdate;
  try
    memo1.Lines.Clear;
    memo1.Lines.Text := aStr;
  finally
    memo1.Lines.EndUpdate;
  end;
end;

end.
