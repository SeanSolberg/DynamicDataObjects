object Form15: TForm15
  Left = 0
  Top = 0
  Anchors = [akTop, akRight]
  Caption = 'JSON Decoding/Encoding Preformance Comparisons'
  ClientHeight = 807
  ClientWidth = 1014
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  DesignSize = (
    1014
    807)
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 16
    Width = 613
    Height = 13
    Caption = 
      'This test app will run some performance tests parsing a variety ' +
      'of JSON files with a variety of different JSON libraries for del' +
      'phi.'
  end
  object Memo1: TMemo
    Left = 181
    Top = 39
    Width = 657
    Height = 760
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Button4: TButton
    Left = 844
    Top = 39
    Width = 169
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'AnsiString test'
    TabOrder = 1
    OnClick = Button4Click
  end
  object btnDelphiLoadFromJSON: TButton
    Left = 8
    Top = 101
    Width = 169
    Height = 25
    Caption = 'Delphi Load From JSON'
    TabOrder = 2
    OnClick = btnDelphiLoadFromJSONClick
  end
  object btnOldDDOLoadFromJSON: TButton
    Left = 8
    Top = 132
    Width = 169
    Height = 25
    Caption = 'Old DDO Load from JSON'
    TabOrder = 3
    OnClick = btnOldDDOLoadFromJSONClick
  end
  object btnNewDataObjLoadFromJSON: TButton
    Left = 8
    Top = 163
    Width = 169
    Height = 25
    Caption = 'New DataObj Load from JSON'
    TabOrder = 4
    OnClick = btnNewDataObjLoadFromJSONClick
  end
  object Button24: TButton
    Left = 844
    Top = 70
    Width = 169
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Escape JSON File'
    TabOrder = 5
    OnClick = Button24Click
  end
  object btnEscapeTest: TButton
    Left = 844
    Top = 101
    Width = 169
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'JSON Escaped Chars test'
    TabOrder = 6
    OnClick = btnEscapeTestClick
  end
  object Button1: TButton
    Left = 8
    Top = 194
    Width = 169
    Height = 25
    Caption = 'Grijjy Load from JSON'
    TabOrder = 7
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 8
    Top = 225
    Width = 169
    Height = 25
    Caption = 'Json-Serializer Load from JSON'
    TabOrder = 8
    OnClick = Button2Click
  end
  object btnRunAllTests: TButton
    Left = 6
    Top = 37
    Width = 169
    Height = 25
    Caption = 'Run All Tests'
    TabOrder = 9
    OnClick = btnRunAllTestsClick
  end
  object Button3: TButton
    Left = 8
    Top = 368
    Width = 167
    Height = 25
    Caption = 'Full Make Test'
    TabOrder = 10
    OnClick = Button3Click
  end
end
