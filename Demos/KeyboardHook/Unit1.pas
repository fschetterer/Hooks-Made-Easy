{**********************************************************************************************
  MIT License

  Copyright (c) 2019 Fred Schetterer (fschetterer@outlook.com)

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
**********************************************************************************************}

unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.Classes, System.Actions, System.SysUtils,
  Vcl.Forms, Vcl.ActnList, Vcl.StdCtrls, Vcl.Controls,
  HooksMadeEasy.Common;

{$I HooksMadeEasy.Keyboard.Shared.inc}

/// <summary>
///   Uncomment to use a DLL but its not required for Current Thread Hooks
/// </summary>
{$DEFINE USEDLL}

type
  TForm1 = class(TForm)
    btHookup: TButton;
    btStop: TButton;
    ActionList1: TActionList;
    aiOnIdle: TAction;
    ListBox1: TListBox;
    ckGlobal: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure aiOnIdleUpdate(Sender: TObject);
    procedure btHookupClick(Sender: TObject);
    procedure btStopClick(Sender: TObject);
  protected
    procedure APPHOOKMSG(var Msg: TMessage); message APP_HOOKMSG;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}


{$IFDEF USEDLL}
const HooksDLL = 'KeyboardHooks.DLL';
function HookActive: Boolean; external HooksDLL;
function Hookup(AHandle: THandle; AThreadId: Cardinal): Boolean; external HooksDLL;
function UnHook: Boolean; external HooksDLL;
{$ELSE}
uses HooksMadeEasy.Keyboard;
{$ENDIF USEDLL}

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Enable/Disable Messaging from non elevated Apps
  ChangeWindowMessageFilterEx(Handle, APP_HOOKMSG, MSGFLT_ALLOW, nil);
{$IFNDEF USEDLL} ckGlobal.Checked := False; ckGlobal.Enabled := False {$ENDIF}
end;

procedure TForm1.aiOnIdleUpdate(Sender: TObject);
var rsl: Boolean;
begin
 {$IFDEF USEDLL} rsl:= HookActive {$ELSE}
  rsl := m_HookData.Active {$ENDIF};
  btHookup.Enabled := not rsl;
  btStop.Enabled := not btHookup.Enabled;
end;

procedure TForm1.btHookupClick(Sender: TObject);
var LThreadID : Cardinal;
begin
 if ckGlobal.Checked then LThreadID := 0 else LThreadID := GetCurrentThreadID;

 {$IFDEF USEDLL} Hookup(Handle, LThreadID) {$ELSE}
  TKeyboardHook.Hookup(Handle, LThreadID) {$ENDIF};
  caption := SysErrorMessage(GetLastError);
end;

procedure TForm1.btStopClick(Sender: TObject);
begin
 {$IFDEF USEDLL} UnHook {$ELSE}
  TKeyboardHook.Stop {$ENDIF};
  caption := SysErrorMessage(GetLastError);
end;

procedure TForm1.APPHOOKMSG(var Msg: TMessage);
begin
  inherited;
  with ListBox1 do
    ItemIndex := Items.Add(Format('ALT+Numkey1, PrevState: %d, Released:%d', [Msg.wParam, Msg.LParam]));
end;


end.
