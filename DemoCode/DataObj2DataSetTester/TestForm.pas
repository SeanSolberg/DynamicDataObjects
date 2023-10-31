unit TestForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, FireDAC.Comp.DataSet, Data.DB, FireDAC.Comp.Client, Vcl.Grids, Vcl.DBGrids, Vcl.ExtCtrls, Vcl.DBCtrls, Vcl.Menus, FireDAC.Stan.StorageJSON,
  FireDAC.Stan.StorageXML, FireDAC.Stan.StorageBin, DataObjects2, DataObjects2DataSet, DataObjects2Streamers, DataObjects2JSON, DataObjects2DDO, DataObjects2CBOR,
  Vcl.StdCtrls;

type
  TForm41 = class(TForm)
    FDMemTable1: TFDMemTable;
    FDMemTable1String: TStringField;
    FDMemTable1Integer: TIntegerField;
    FDMemTable1Smallnt: TSmallintField;
    FDMemTable1Word: TWordField;
    FDMemTable1Float: TFloatField;
    FDMemTable1currency: TCurrencyField;
    FDMemTable1BCD: TBCDField;
    FDMemTable1fmtbcd: TFMTBCDField;
    FDMemTable1boolean: TBooleanField;
    FDMemTable1date: TDateField;
    FDMemTable1VarBytes: TVarBytesField;
    FDMemTable1Bytes: TBytesField;
    FDMemTable1Time: TTimeField;
    FDMemTable1Datetime: TDateTimeField;
    FDMemTable1sqltimeStamp: TSQLTimeStampField;
    FDMemTable1blob: TBlobField;
    FDMemTable1memo: TMemoField;
    FDMemTable1Graphic: TGraphicField;
    FDMemTable1AutoInc: TAutoIncField;
    FDMemTable1LargeInt: TLargeintField;
    FDMemTable1adt: TADTField;
    FDMemTable1array: TArrayField;
    FDMemTable1DataSet: TDataSetField;
    FDMemTable1Aggregate: TAggregateField;
    FDMemTable1WideString: TWideStringField;
    FDMemTable1Guid: TGuidField;
    FDMemTable1Interface: TInterfaceField;
    FDMemTable1IDispatch: TIDispatchField;
    FDMemTable1WideMemo: TWideMemoField;
    FDMemTable1longWord: TLongWordField;
    FDMemTable1ShortInt: TShortintField;
    FDMemTable1Byte: TByteField;
    FDMemTable1Extended: TExtendedField;
    FDMemTable1Single: TSingleField;
    FDMemTable1UnsignedautoInc: TUnsignedAutoIncField;
    FDMemTable1FDAutoInc: TFDAutoIncField;
    FDMemTable1FDSQLTimeInterval: TFDSQLTimeIntervalField;
    FDMemTable1FDXML: TFDXMLField;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    MainMenu1: TMainMenu;
    Application1: TMenuItem;
    Application2: TMenuItem;
    SaveDataToFile1: TMenuItem;
    FDStanStorageBinLink1: TFDStanStorageBinLink;
    FDStanStorageXMLLink1: TFDStanStorageXMLLink;
    FDStanStorageJSONLink1: TFDStanStorageJSONLink;
    SaveToDataObject1: TMenuItem;
    SaveDialog1: TSaveDialog;
    procedure SaveDataToFile1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SaveToDataObject1Click(Sender: TObject);
    procedure Application2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form41: TForm41;

implementation

{$R *.dfm}

procedure TForm41.Application2Click(Sender: TObject);
begin
  application.Terminate;
end;

procedure TForm41.FormCreate(Sender: TObject);
begin
  if fileExists('TestDataFile.binary') then
  begin
    FDmemTable1.LoadFromFile('TestDataFile.binary');
  end;
end;

procedure TForm41.SaveDataToFile1Click(Sender: TObject);
begin
  FDMemTable1.SaveToFile('TestDataFile.json', TFDStorageFormat.sfJSON);
  FDMemTable1.SaveToFile('TestDataFile.xml', TFDStorageFormat.sfXML);
  FDMemTable1.SaveToFile('TestDataFile.binary', TFDStorageFormat.sfBinary);
end;

procedure TForm41.SaveToDataObject1Click(Sender: TObject);
var
  lDataObj: TDataObj;
begin
  SaveDialog1.Filter := gStreamerRegistry.AllStreamersFileDialogFilters;

  if not SaveDialog1.Execute then exit;

  lDataObj:=TDataObj.create;
  try
    FDmemTable1.First;   // make sure we start out at the beginning of the table.
    DataSetToDataObj(FDMemTable1, lDataObj);
    lDataObj.WriteToFile(SaveDialog1.filename);
  finally
    lDataObj.Free;
  end;
end;

end.

