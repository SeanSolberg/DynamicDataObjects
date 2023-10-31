object IteratorTestForm: TIteratorTestForm
  Left = 0
  Top = 0
  Caption = 'App to test the iterators of TDataFrame and TDaraArray'
  ClientHeight = 433
  ClientWidth = 622
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object btnTest: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Run Test'
    TabOrder = 0
    OnClick = btnTestClick
  end
  object Memo1: TMemo
    Left = 8
    Top = 39
    Width = 601
    Height = 386
    TabOrder = 1
  end
end
