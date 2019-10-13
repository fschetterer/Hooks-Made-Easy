unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Vcl.AppEvnts, System.Actions, Vcl.ActnList, Vcl.Menus;

/// <summary>
///   Uncomment to use a DLL but its not required for Journal Hooks
/// </summary>
{$DEFINE USEDLL}

type
  TForm1 = class(TForm)
    btRecord: TButton;
    btPlayback: TButton;
    btStop: TButton;
    MainMenu1: TMainMenu;
    ActionList1: TActionList;
    aiStopHook: TAction;
    StopHook1: TMenuItem;
    ckInstantPlayback: TCheckBox;
    procedure aiStopHookExecute(Sender: TObject);
    procedure aiStopHookUpdate(Sender: TObject);
    procedure btPlaybackClick(Sender: TObject);
    procedure btRecordClick(Sender: TObject);
    procedure btStopClick(Sender: TObject);
    procedure OnIdle(Action: TBasicAction; var Handled: Boolean);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{$IFDEF USEDLL}
const HooksDLL = 'Hooks.DLL';
function HookActive: Boolean; external HooksDLL;
function StartRecording: Boolean; external HooksDLL;
function StartPlayback(const InstantPlayback: Boolean): Boolean; external HooksDLL;
function UnHook: Boolean; external HooksDLL;
{$ELSE}
uses HooksMadeEasy.Journal;
{$ENDIF USEDLL}

procedure TForm1.aiStopHookExecute(Sender: TObject);
begin
  btStop.Click;
end;

procedure TForm1.aiStopHookUpdate(Sender: TObject);
begin
  aiStopHook.Enabled := btStop.Enabled;
end;

procedure TForm1.btPlaybackClick(Sender: TObject);
begin
 {$IFDEF USEDLL} StartPlayback(ckInstantPlayback.Checked) {$ELSE}
  TJournalHook.Play(ckInstantPlayback.Checked) {$ENDIF};
  caption := SysErrorMessage(GetLastError);
end;

procedure TForm1.btRecordClick(Sender: TObject);
begin
 {$IFDEF USEDLL} StartRecording {$ELSE}
  TJournalHook.Rec {$ENDIF};
  caption := SysErrorMessage(GetLastError);
end;

procedure TForm1.btStopClick(Sender: TObject);
begin
 {$IFDEF USEDLL} UnHook {$ELSE}
  TJournalHook.Stop {$ENDIF};
  caption := SysErrorMessage(GetLastError);
end;

procedure TForm1.OnIdle(Action: TBasicAction; var Handled: Boolean);
var rsl: Boolean;
begin
 {$IFDEF USEDLL} rsl:= HookActive {$ELSE}
  rsl := TJournalHook.Active {$ENDIF};
  btRecord.Enabled := not rsl;
  btPlayback.Enabled := btRecord.Enabled;
  btStop.Enabled := not btRecord.Enabled;
end;


end.
