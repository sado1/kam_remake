object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'KMR Animation Interpolator'
  ClientHeight = 280
  ClientWidth = 753
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object btnProcessUnits: TButton
    Left = 8
    Top = 8
    Width = 105
    Height = 25
    Caption = 'Process Units'
    TabOrder = 0
    OnClick = btnProcessUnitsClick
  end
  object Memo1: TMemo
    Left = 8
    Top = 40
    Width = 737
    Height = 233
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
end
