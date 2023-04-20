object Form41: TForm41
  Left = 0
  Top = 0
  Caption = 'Form41'
  ClientHeight = 791
  ClientWidth = 855
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Menu = MainMenu1
  OnCreate = FormCreate
  DesignSize = (
    855
    791)
  TextHeight = 15
  object DBGrid1: TDBGrid
    Left = 8
    Top = 39
    Width = 839
    Height = 744
    Anchors = [akLeft, akTop, akRight, akBottom]
    DataSource = DataSource1
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -12
    TitleFont.Name = 'Segoe UI'
    TitleFont.Style = []
  end
  object DBNavigator1: TDBNavigator
    Left = 8
    Top = 8
    Width = 830
    Height = 25
    DataSource = DataSource1
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
  end
  object FDMemTable1: TFDMemTable
    Active = True
    FieldDefs = <
      item
        Name = 'String'
        DataType = ftString
        Size = 20
      end
      item
        Name = 'Integer'
        DataType = ftInteger
      end
      item
        Name = 'Smallnt'
        DataType = ftSmallint
      end
      item
        Name = 'Word'
        DataType = ftWord
      end
      item
        Name = 'Float'
        DataType = ftFloat
      end
      item
        Name = 'currency'
        DataType = ftCurrency
        Precision = 19
      end
      item
        Name = 'BCD'
        DataType = ftBCD
        Size = 4
      end
      item
        Name = 'fmtbcd'
        DataType = ftFMTBcd
        Size = 8
      end
      item
        Name = 'boolean'
        DataType = ftBoolean
      end
      item
        Name = 'date'
        DataType = ftDate
      end
      item
        Name = 'VarBytes'
        DataType = ftVarBytes
        Size = 16
      end
      item
        Name = 'Bytes'
        Attributes = [faFixed]
        DataType = ftBytes
        Size = 16
      end
      item
        Name = 'Time'
        DataType = ftTime
      end
      item
        Name = 'Datetime'
        DataType = ftDateTime
      end
      item
        Name = 'sqltimeStamp'
        DataType = ftTimeStamp
      end
      item
        Name = 'blob'
        DataType = ftBlob
      end
      item
        Name = 'memo'
        DataType = ftMemo
      end
      item
        Name = 'Graphic'
        DataType = ftBlob
      end
      item
        Name = 'AutoInc'
        DataType = ftAutoInc
      end
      item
        Name = 'LargeInt'
        DataType = ftLargeint
      end
      item
        Name = 'adt'
        DataType = ftADT
      end
      item
        Name = 'array'
        DataType = ftArray
      end
      item
        Name = 'DataSet'
        DataType = ftDataSet
      end
      item
        Name = 'WideString'
        DataType = ftWideString
        Size = 20
      end
      item
        Name = 'Guid'
        DataType = ftGuid
        Size = 38
      end
      item
        Name = 'Interface'
        DataType = ftInterface
      end
      item
        Name = 'IDispatch'
        DataType = ftInterface
      end
      item
        Name = 'WideMemo'
        DataType = ftWideMemo
      end
      item
        Name = 'longWord'
        DataType = ftLongWord
      end
      item
        Name = 'ShortInt'
        DataType = ftShortint
      end
      item
        Name = 'Byte'
        DataType = ftByte
      end
      item
        Name = 'Extended'
        DataType = ftExtended
      end
      item
        Name = 'Single'
        DataType = ftSingle
      end
      item
        Name = 'UnsignedautoInc'
        DataType = ftAutoInc
      end
      item
        Name = 'FDAutoInc'
        Attributes = [faReadonly]
        DataType = ftAutoInc
      end
      item
        Name = 'FDSQLTimeInterval'
        DataType = ftOraInterval
      end
      item
        Name = 'FDXML'
        DataType = ftDBaseOle
      end>
    IndexDefs = <>
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    StoreDefs = True
    Left = 576
    Top = 264
    object FDMemTable1String: TStringField
      FieldName = 'String'
    end
    object FDMemTable1Integer: TIntegerField
      FieldName = 'Integer'
    end
    object FDMemTable1Smallnt: TSmallintField
      FieldName = 'Smallnt'
    end
    object FDMemTable1Word: TWordField
      FieldName = 'Word'
    end
    object FDMemTable1Float: TFloatField
      FieldName = 'Float'
    end
    object FDMemTable1currency: TCurrencyField
      FieldName = 'currency'
    end
    object FDMemTable1BCD: TBCDField
      FieldName = 'BCD'
    end
    object FDMemTable1fmtbcd: TFMTBCDField
      FieldName = 'fmtbcd'
    end
    object FDMemTable1boolean: TBooleanField
      FieldName = 'boolean'
    end
    object FDMemTable1date: TDateField
      FieldName = 'date'
    end
    object FDMemTable1VarBytes: TVarBytesField
      FieldName = 'VarBytes'
    end
    object FDMemTable1Bytes: TBytesField
      FieldName = 'Bytes'
    end
    object FDMemTable1Time: TTimeField
      FieldName = 'Time'
    end
    object FDMemTable1Datetime: TDateTimeField
      FieldName = 'Datetime'
    end
    object FDMemTable1sqltimeStamp: TSQLTimeStampField
      FieldName = 'sqltimeStamp'
    end
    object FDMemTable1blob: TBlobField
      FieldName = 'blob'
    end
    object FDMemTable1memo: TMemoField
      FieldName = 'memo'
      BlobType = ftMemo
    end
    object FDMemTable1Graphic: TGraphicField
      FieldName = 'Graphic'
      BlobType = ftGraphic
    end
    object FDMemTable1AutoInc: TAutoIncField
      FieldName = 'AutoInc'
    end
    object FDMemTable1LargeInt: TLargeintField
      FieldName = 'LargeInt'
    end
    object FDMemTable1adt: TADTField
      FieldName = 'adt'
    end
    object FDMemTable1array: TArrayField
      FieldName = 'array'
    end
    object FDMemTable1DataSet: TDataSetField
      FieldName = 'DataSet'
    end
    object FDMemTable1WideString: TWideStringField
      FieldName = 'WideString'
    end
    object FDMemTable1Guid: TGuidField
      FieldName = 'Guid'
      Size = 38
    end
    object FDMemTable1Interface: TInterfaceField
      FieldName = 'Interface'
    end
    object FDMemTable1IDispatch: TIDispatchField
      FieldName = 'IDispatch'
    end
    object FDMemTable1WideMemo: TWideMemoField
      FieldName = 'WideMemo'
      BlobType = ftWideMemo
    end
    object FDMemTable1longWord: TLongWordField
      FieldName = 'longWord'
    end
    object FDMemTable1ShortInt: TShortintField
      FieldName = 'ShortInt'
    end
    object FDMemTable1Byte: TByteField
      FieldName = 'Byte'
    end
    object FDMemTable1Extended: TExtendedField
      FieldName = 'Extended'
      Precision = 19
    end
    object FDMemTable1Single: TSingleField
      FieldName = 'Single'
    end
    object FDMemTable1UnsignedautoInc: TUnsignedAutoIncField
      FieldName = 'UnsignedautoInc'
    end
    object FDMemTable1FDAutoInc: TFDAutoIncField
      FieldName = 'FDAutoInc'
      ReadOnly = True
    end
    object FDMemTable1FDSQLTimeInterval: TFDSQLTimeIntervalField
      FieldName = 'FDSQLTimeInterval'
    end
    object FDMemTable1FDXML: TFDXMLField
      FieldName = 'FDXML'
      BlobType = ftDBaseOle
    end
    object FDMemTable1Aggregate: TAggregateField
      FieldName = 'Aggregate'
      DisplayName = ''
    end
  end
  object DataSource1: TDataSource
    DataSet = FDMemTable1
    Left = 704
    Top = 264
  end
  object MainMenu1: TMainMenu
    Left = 48
    Top = 144
    object Application1: TMenuItem
      Caption = 'Application'
      object SaveDataToFile1: TMenuItem
        Caption = 'Save Data'
        OnClick = SaveDataToFile1Click
      end
      object SaveToDataObject1: TMenuItem
        Caption = 'SaveToDataObject'
        OnClick = SaveToDataObject1Click
      end
      object Application2: TMenuItem
        Caption = 'Exit'
      end
    end
  end
  object FDStanStorageBinLink1: TFDStanStorageBinLink
    Left = 632
    Top = 600
  end
  object FDStanStorageXMLLink1: TFDStanStorageXMLLink
    Left = 632
    Top = 544
  end
  object FDStanStorageJSONLink1: TFDStanStorageJSONLink
    Left = 632
    Top = 656
  end
  object SaveDialog1: TSaveDialog
    Filter = 'All files(*.*)|*.*'
    Left = 240
    Top = 192
  end
end
