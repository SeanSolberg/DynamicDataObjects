object Form19: TForm19
  Left = 0
  Top = 0
  Caption = 'DataObjects Performance Tester'
  ClientHeight = 444
  ClientWidth = 826
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  DesignSize = (
    826
    444)
  TextHeight = 13
  object Memo1: TMemo
    Left = 8
    Top = 40
    Width = 810
    Height = 401
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Start Tests'
    TabOrder = 1
    OnClick = Button1Click
  end
end
