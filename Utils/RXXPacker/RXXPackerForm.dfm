object RXXForm1: TRXXForm1
  Left = 72
  Top = 90
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'RXX Packer'
  ClientHeight = 417
  ClientWidth = 705
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 14
  object Label2: TLabel
    Left = 16
    Top = 216
    Width = 130
    Height = 13
    Caption = 'Source RX and sprites path'
  end
  object Label3: TLabel
    Left = 16
    Top = 296
    Width = 125
    Height = 13
    Caption = 'Destination RXX/RXA path'
  end
  object Label1: TLabel
    Left = 16
    Top = 256
    Width = 154
    Height = 13
    Caption = 'Source interpolated sprites path'
  end
  object btnPackRXX: TButton
    Left = 16
    Top = 376
    Width = 257
    Height = 25
    Caption = 'Pack selected'
    TabOrder = 0
    OnClick = btnPackRXXClick
  end
  object ListBox1: TListBox
    Left = 16
    Top = 16
    Width = 257
    Height = 153
    ItemHeight = 14
    MultiSelect = True
    TabOrder = 1
  end
  object btnUpdateList: TButton
    Left = 16
    Top = 344
    Width = 257
    Height = 25
    Caption = 'Update List'
    TabOrder = 2
    OnClick = btnUpdateListClick
  end
  object edSourceRxPath: TEdit
    Left = 16
    Top = 232
    Width = 257
    Height = 21
    TabOrder = 3
    OnChange = edSourceRxPathChange
  end
  object chkPackToRXA: TCheckBox
    Left = 16
    Top = 192
    Width = 73
    Height = 17
    Caption = 'Pack to RXA'
    TabOrder = 4
    OnClick = chkPackToRXAClick
  end
  object chkPackToRXX: TCheckBox
    Left = 16
    Top = 176
    Width = 73
    Height = 17
    Caption = 'Pack to RXX'
    Checked = True
    State = cbChecked
    TabOrder = 5
    OnClick = chkPackToRXXClick
  end
  object edDestinationPath: TEdit
    Left = 16
    Top = 312
    Width = 257
    Height = 21
    TabOrder = 6
    OnChange = edSourceRxPathChange
  end
  object meLog: TMemo
    Left = 280
    Top = 16
    Width = 409
    Height = 385
    TabOrder = 7
  end
  object rbRXXFormat0: TRadioButton
    Left = 224
    Top = 176
    Width = 49
    Height = 17
    Caption = 'RXX'
    TabOrder = 8
  end
  object rbRXXFormat1: TRadioButton
    Left = 224
    Top = 192
    Width = 49
    Height = 17
    Caption = 'RXX1'
    Checked = True
    TabOrder = 9
    TabStop = True
  end
  object edSourceInterpPath: TEdit
    Left = 16
    Top = 272
    Width = 257
    Height = 21
    TabOrder = 10
    OnChange = edSourceRxPathChange
  end
end
