object Form1: TForm1
  Left = 0
  Top = 0
  Action = aiOnIdle
  Caption = 'aiOnIdle'
  ClientHeight = 371
  ClientWidth = 348
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  DesignSize = (
    348
    371)
  PixelsPerInch = 96
  TextHeight = 13
  object btHookup: TButton
    Left = 177
    Top = 8
    Width = 75
    Height = 25
    Caption = 'btHookup'
    TabOrder = 0
    OnClick = btHookupClick
  end
  object btStop: TButton
    Left = 258
    Top = 8
    Width = 75
    Height = 25
    Caption = 'btStop'
    Enabled = False
    TabOrder = 1
    OnClick = btStopClick
  end
  object ListBox1: TListBox
    Left = 8
    Top = 39
    Width = 332
    Height = 331
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 2
  end
  object ckGlobal: TCheckBox
    Left = 24
    Top = 16
    Width = 97
    Height = 17
    Caption = 'ckGlobal'
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
  object ActionList1: TActionList
    Left = 32
    Top = 64
    object aiOnIdle: TAction
      Caption = 'aiOnIdle'
      OnUpdate = aiOnIdleUpdate
    end
  end
end
