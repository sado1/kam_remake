object RXXForm1: TRXXForm1
  Left = 72
  Top = 90
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'RXX Packer'
  ClientHeight = 401
  ClientWidth = 289
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
  object lpProgress: TLabel
    Left = 16
    Top = 176
    Width = 3
    Height = 13
  end
  object Label2: TLabel
    Left = 16
    Top = 224
    Width = 99
    Height = 13
    Caption = 'Sprites source folder'
  end
  object Label3: TLabel
    Left = 16
    Top = 272
    Width = 156
    Height = 13
    Caption = 'Packed sprites destination folder'
  end
  object btnPackRXX: TButton
    Left = 16
    Top = 360
    Width = 257
    Height = 25
    Caption = 'Pack'
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
    Top = 328
    Width = 257
    Height = 25
    Caption = 'Update List'
    TabOrder = 2
    OnClick = btnUpdateListClick
  end
  object edSpritesLoadDir: TEdit
    Left = 16
    Top = 240
    Width = 257
    Height = 21
    TabOrder = 3
    OnChange = edSpritesLoadDirChange
  end
  object chkPackToRXA: TCheckBox
    Left = 160
    Top = 192
    Width = 73
    Height = 17
    Caption = 'Pack to RXA'
    TabOrder = 4
    OnClick = chkPackToRXAClick
  end
  object chkAddRXXHeader: TCheckBox
    Left = 160
    Top = 216
    Width = 113
    Height = 17
    Caption = 'Add Version header'
    Checked = True
    State = cbChecked
    TabOrder = 5
  end
  object chkPackToRXX: TCheckBox
    Left = 160
    Top = 176
    Width = 73
    Height = 17
    Caption = 'Pack to RXX'
    Checked = True
    State = cbChecked
    TabOrder = 6
    OnClick = chkPackToRXXClick
  end
  object edSpritesSaveDir: TEdit
    Left = 16
    Top = 288
    Width = 257
    Height = 21
    TabOrder = 7
    OnChange = edSpritesLoadDirChange
  end
end
