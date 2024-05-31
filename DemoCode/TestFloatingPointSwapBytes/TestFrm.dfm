object Form68: TForm68
  Left = 0
  Top = 0
  Caption = 'Test Floating Point Swap Bytes'
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
  object btnRunTest: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Run Test'
    TabOrder = 0
    OnClick = btnRunTestClick
  end
  object Memo1: TMemo
    Left = 8
    Top = 39
    Width = 606
    Height = 386
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
  object Button1: TButton
    Left = 525
    Top = 8
    Width = 89
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Test CBOR'
    TabOrder = 2
    OnClick = Button1Click
  end
end
