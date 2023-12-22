object Form19: TForm19
  Left = 0
  Top = 0
  Caption = 'DataObjects Performance Tester'
  ClientHeight = 1020
  ClientWidth = 826
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  DesignSize = (
    826
    1020)
  TextHeight = 13
  object Memo1: TMemo
    Left = 8
    Top = 40
    Width = 810
    Height = 977
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 0
    ExplicitWidth = 808
    ExplicitHeight = 969
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 185
    Height = 25
    Caption = 'Start Encode/Decode Tests'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button4: TButton
    Left = 199
    Top = 9
    Width = 75
    Height = 25
    Caption = 'VarInt Tests'
    TabOrder = 2
    OnClick = Button4Click
  end
  object Button2: TButton
    Left = 280
    Top = 9
    Width = 75
    Height = 25
    Caption = 'String Test'
    TabOrder = 3
    OnClick = Button2Click
  end
  object Button5: TButton
    Left = 361
    Top = 9
    Width = 137
    Height = 25
    Caption = 'String Search Test'
    TabOrder = 4
    OnClick = Button5Click
  end
  object Button3: TButton
    Left = 504
    Top = 9
    Width = 137
    Height = 25
    Caption = 'String Search Test 2'
    TabOrder = 5
    OnClick = Button3Click
  end
end
