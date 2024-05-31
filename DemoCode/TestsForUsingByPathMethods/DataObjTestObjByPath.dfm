object Form74: TForm74
  Left = 0
  Top = 0
  Caption = 'Test using the '#39'By Path'#39' calls in TDataObj'
  ClientHeight = 433
  ClientWidth = 622
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  DesignSize = (
    622
    433)
  TextHeight = 15
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Run Test'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 39
    Width = 606
    Height = 386
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object Button2: TButton
    Left = 461
    Top = 8
    Width = 153
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Test SparseArray Delete'
    TabOrder = 2
    OnClick = Button2Click
  end
end
