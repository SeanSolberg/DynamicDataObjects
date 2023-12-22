unit TestFrm;

{ This is a test application that testes the DataObjects2.pas code library.  This test app will test the .AssignTo and .AssignFrom methods on TDataObj}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, DataObjects2;


type
  TPerson = class;
  TPeople = array of TPerson;           // dynamic array

  TArmLengths = array[0..1] of integer;  // static array
  TMyEnumeration = (cNone, cOne, cTwo, cThree);

  TMyRecord = record
    RecFieldStr: string;
    RecFieldInt: integer;
  end;
  TMyRecords = array of TMyRecord;

  TPerson = class
  private
    FFirstname: string;
    FAge: integer;
    FWeight: double;
    fLastName: string;
    fKids: TPeople;
    fArmLengths: TArmLengths;
    fMyRecords: TMyRecords;
    FMyEnumeration: TMyEnumeration;
    procedure SetFirstname(const Value: string);
    procedure SetAge(const Value: integer);
    procedure SetWeight(const Value: double);
    procedure SetMyEnumeration(const Value: TMyEnumeration);

    property Age: integer read FAge write SetAge;
  protected
    property Weight: double read FWeight write SetWeight;

  public
    property Firstname: string read FFirstname write SetFirstname;   // NOTE that this is public and not published, so by default this will not serialize to/from a DataObj unless you change the
                                                                     // aMemberVisibility parameter in the .AssignTo and .AssignFrom method calls. Default is to just assign Published properties.
    function AddKid: TPerson;
    procedure AddRecord(aRec: TMyRecord);
    property ArmLengths: TArmLengths read fArmLengths write fArmlengths;   // this won't compile if put into published.
  published
    property Lastname: string read fLastName write fLastname;
    property Kids: TPeople read fKids write fKids;      // dynamic array
    property MyRecords: TMyRecords read fMyRecords write fMyRecords;
    property MyEnumeration: TMyEnumeration read FMyEnumeration write SetMyEnumeration;
  end;


  TAssignToAssignFromTestForm = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    function CompareTwoFiles(aFilename1: string; aFilename2: string): boolean;  // returns true if same byte for byte.
    procedure Log(AStr: string; aIsCaption: boolean = false);
  public
    { Public declarations }
  end;

var
  AssignToAssignFromTestForm: TAssignToAssignFromTestForm;

implementation

{$R *.dfm}

function MyRecord(aStr: string; aInt: integer): TMyRecord;
begin
  result.RecFieldStr := aStr;
  result.RecFieldInt := aInt;
end;

{ TPerson }

function TPerson.AddKid: TPerson;
begin
  result := TPerson.Create;
  setlength(fKids, length(fKids)+1);
  fKids[high(fKids)] := result;
end;

procedure TPerson.AddRecord(aRec: TMyRecord);
begin
  setlength(fMyRecords, length(fMyRecords)+1);
  fMyRecords[high(fMyRecords)] := aRec;
end;

procedure TPerson.SetAge(const Value: integer);
begin
  FAge := Value;
end;

procedure TPerson.SetFirstname(const Value: string);
begin
  FFirstname := Value;
end;

procedure TPerson.SetMyEnumeration(const Value: TMyEnumeration);
begin
  FMyEnumeration := Value;
end;

procedure TPerson.SetWeight(const Value: double);
begin
  FWeight := Value;
end;

procedure TAssignToAssignFromTestForm.Button1Click(Sender: TObject);
var
  lDataObj: TDataobj;
  lPerson: TPerson;
  lLengths: TArmlengths;
  lPerson2: TPerson;
  lDataObj2: TDataObj;
begin
  // Create an object and populate some sample data on it so we can use it for assignto/assignfrom testing
  lPerson:=TPerson.Create;
  lPerson.Firstname := 'Sean';
  lPerson.Lastname := 'Solberg';
  lPerson.Age := 49;
  lPerson.Weight := 123.45;
  lPerson.MyEnumeration := cThree;

  with lPerson.AddKid do
  begin
    Firstname := 'Austin';
    Age := 24;
    lLengths[0] := 42;
    lLengths[1] := 43;
    ArmLengths := lLengths;
  end;

  with lPerson.AddKid do
  begin
    Firstname := 'Carter';
    Age := 21;
  end;

  lPerson.AddRecord(MyRecord('Hello',123));


  // Now let's create a few assignFrom and assignTo tests.
  lDataObj:=TDataobj.Create;
  try
    // Test1: AssignFrom with default assignment of only published properties
    Log('TEST1', TRUE);
    lDataObj.AssignFrom(lPerson);
    ForceDirectories('c:\temp');

    lDataObj.WriteToFile('c:\temp\person-Published.dataObj');

    // Test2:  AssignTo with default assignment of only published properties.  Make a second file and compare them.
    Log('TEST2', TRUE);
    lPerson2:=TPerson.Create;
    lDataObj2:=TDataObj.Create;
    try
      lDataObj.AssignTo(lPerson2);

      lDataObj2.AssignFrom(lPerson2);
      lDataObj2.WriteToFile('c:\temp\person-Published-Clone.dataObj');
    finally
      lDataObj2.free;
      lPerson2.Free;
    end;
    // compare the two files to make sure they are absolutely identical.
    if CompareTwoFiles('c:\temp\person-Published.dataObj', 'c:\temp\person-Published-Clone.dataObj') = false then
    begin
      log('Test #2 Failed');
      log('This test was expected to fail because the DataObjects RTTI assignment is not yet written to fill array properties on objects.');
    end;


    Log('TEST3', TRUE);
    lDataObj.Clear;
    lDataObj.AssignFrom(lPerson, cPublishedMembers, false, true);
    lDataObj.WriteToFile('c:\temp\person-Published-IntEnums.dataObj');

    Log('TEST4', TRUE);
    lDataObj.Clear;
    lDataObj.AssignFrom(lPerson, cPublishedMembers, true);
    lDataObj.WriteToFile('c:\temp\person-Published-NoDefault.dataObj');

    Log('TEST5', TRUE);
    lDataObj.Clear;
    lDataObj.AssignFrom(lPerson, cPublicMembers);
    lDataObj.WriteToFile('c:\temp\person-Public.dataObj');

    Log('TEST6', TRUE);
    lDataObj.Clear;
    lDataObj.AssignFrom(lPerson, cPublicMembers, true);
    lDataObj.WriteToFile('c:\temp\person-Public-NoDefault.dataObj');

    Log('TEST7', TRUE);
    lDataObj.Clear;
//    try lDataObj.AssignFrom(self, cPublicMembers); except end;    HMMMMMM. Not sure about assigning to forms yet.  I think there are problems with it creating child objects on the form.
    lDataObj.WriteToFile('c:\temp\Form.dataObj');

    Log('TEST8', TRUE);
    lDataObj.Clear;
//    try lDataObj.AssignFrom(self, cPublicMembers, true); except end;   HMMMMMM. Not sure about assigning to forms yet.  I think there are problems with it creating child objects on the form.
    lDataObj.WriteToFile('c:\temp\Form-NoDefault.dataObj');
  finally
    lDataObj.Free;
  end;
end;


function TAssignToAssignFromTestForm.CompareTwoFiles(aFilename1, aFilename2: string): boolean;
var
  lF1, lF2: TFileStream;
  lByte1, lByte2: byte;
begin
  result := true;
  lF1 := TfileStream.Create(aFilename1, fmOpenRead);
  lF2 := TfileStream.Create(aFilename2, fmOpenRead);
  try
    // slow way to do this but so what.
    while lF1.Read(lByte1, 1)=1 do
    begin
      if lF2.Read(lByte2, 1)=1 then
      begin
        if lByte1<>lByte2 then
        begin
          Log('Files '+aFilename1+' and '+aFilename2+' are not the same at '+IntToStr(lF1.Position));
          result := false;
          break;
        end;
      end
      else
      begin
        Log('Files '+aFilename1+' and '+aFilename2+' are not the same at '+IntToStr(lF1.Position)+' File 2 is smaller');
        result := false;
        break;
      end;
    end;

  finally
    lF1.Free;
    lF2.Free;
  end;
end;

procedure TAssignToAssignFromTestForm.Log(AStr: string; aIsCaption: boolean = false);
begin
  if aIsCaption then
    memo1.Lines.Add(aStr)
  else
    memo1.Lines.Add('  '+aStr);
end;

end.
