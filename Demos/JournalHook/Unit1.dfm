object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 108
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 13
  object btRecord: TButton
    Left = 232
    Top = 56
    Width = 75
    Height = 25
    Caption = 'btRecord'
    TabOrder = 0
    OnClick = btRecordClick
  end
  object btPlayback: TButton
    Left = 313
    Top = 56
    Width = 75
    Height = 25
    Caption = 'btPlayback'
    TabOrder = 1
    OnClick = btPlaybackClick
  end
  object btStop: TButton
    Left = 394
    Top = 56
    Width = 75
    Height = 25
    Caption = 'btStop'
    Enabled = False
    TabOrder = 2
    OnClick = btStopClick
  end
  object ckInstantPlayback: TCheckBox
    Left = 313
    Top = 83
    Width = 97
    Height = 17
    Caption = 'Instant Playback'
    TabOrder = 3
  end
  object MainMenu1: TMainMenu
    Left = 136
    Top = 40
    object StopHook1: TMenuItem
      Action = aiStopHook
    end
  end
  object ActionList1: TActionList
    OnUpdate = OnIdle
    Left = 40
    Top = 8
    object aiStopHook: TAction
      Caption = 'Stop Hook (CTRL+BREAK)'
      OnExecute = aiStopHookExecute
      OnUpdate = aiStopHookUpdate
    end
  end
end
