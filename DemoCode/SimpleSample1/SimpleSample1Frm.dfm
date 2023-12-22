object SimpleSample1Form: TSimpleSample1Form
  Left = 0
  Top = 0
  Caption = 'Simple Sample 1'
  ClientHeight = 496
  ClientWidth = 887
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  DesignSize = (
    887
    496)
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 824
    Height = 13
    Caption = 
      'This app just shows some simple ways you can work with the DataO' +
      'bjects object model with code.  These samples don'#39't do streaming' +
      ';  see a different sample app for that. '
  end
  object Memo1: TMemo
    Left = 8
    Top = 64
    Width = 872
    Height = 425
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object btnGo1: TButton
    Left = 8
    Top = 33
    Width = 65
    Height = 25
    Caption = 'GO 1'
    TabOrder = 1
    OnClick = btnGo1Click
  end
  object tnGo2: TButton
    Left = 79
    Top = 33
    Width = 65
    Height = 25
    Caption = 'GO 2'
    TabOrder = 2
    OnClick = tnGo2Click
  end
  object btnGo3: TButton
    Left = 150
    Top = 33
    Width = 65
    Height = 25
    Caption = 'GO 3'
    TabOrder = 3
    OnClick = btnGo3Click
  end
  object btnGo4: TButton
    Left = 221
    Top = 33
    Width = 65
    Height = 25
    Caption = 'GO 4'
    TabOrder = 4
    OnClick = btnGo4Click
  end
  object btnGo5: TButton
    Left = 292
    Top = 33
    Width = 65
    Height = 25
    Caption = 'GO 5'
    TabOrder = 5
    OnClick = btnGo5Click
  end
  object btnGo6: TButton
    Left = 363
    Top = 33
    Width = 65
    Height = 25
    Caption = 'GO 6'
    TabOrder = 6
    OnClick = btnGo6Click
  end
  object btnGo7: TButton
    Left = 434
    Top = 33
    Width = 65
    Height = 25
    Caption = 'GO 7'
    TabOrder = 7
    OnClick = btnGo7Click
  end
  object Button1: TButton
    Left = 505
    Top = 33
    Width = 65
    Height = 25
    Caption = 'GO 8'
    TabOrder = 8
    OnClick = Button1Click
  end
end
