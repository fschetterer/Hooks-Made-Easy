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

{$I HooksMadeEasy.GetMsg.Shared.inc}

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
const HooksDLL = 'GetMsgHooks.DLL';
function HookActive: Boolean; external HooksDLL;
function Hookup(AHandle: THandle; AThreadId: Cardinal): Boolean; external HooksDLL;
function UnHook: Boolean; external HooksDLL;
{$ELSE}
uses HooksMadeEasy.GetMsg;
{$ENDIF USEDLL}

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Enable/Disable Messaging from non elevated Apps
  ChangeWindowMessageFilterEx(Handle, APP_HOOKMSG, MSGFLT_ALLOW, nil);
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
  TGetMsgHook.Hookup(Handle, LThreadID) {$ENDIF};
  caption := SysErrorMessage(GetLastError);
end;

procedure TForm1.btStopClick(Sender: TObject);
begin
 {$IFDEF USEDLL} UnHook {$ELSE}
  TGetMsgHook.Stop {$ENDIF};
  caption := SysErrorMessage(GetLastError);
end;

procedure TForm1.APPHOOKMSG(var Msg: TMessage);
  procedure AddToList(s: string; lParam : LPARAM);
  begin
     with ListBox1 do
      ItemIndex := Items.Add(Format('%s: %d', [s, Msg.LParam]));
  end;
begin
  inherited;
  with Msg do case wParam of
    SC_CLOSE: AddToList('SC_CLOSE', lParam);
    SC_CONTEXTHELP: AddToList('SC_CONTEXTHELP', lParam);
    SC_DEFAULT: AddToList('SC_DEFAULT', lParam);
    SC_HOTKEY: AddToList('SC_HOTKEY', lParam);
    SC_HSCROLL: AddToList('SC_HSCROLL', lParam);
    SC_KEYMENU: AddToList('SC_KEYMENU', lParam);
    SC_MAXIMIZE: AddToList('SC_MAXIMIZE', lParam);
    SC_MINIMIZE: AddToList('SC_MINIMIZE', lParam);
    SC_MONITORPOWER: AddToList('SC_MONITORPOWER', lParam);
    SC_MOUSEMENU: AddToList('SC_MOUSEMENU', lParam);
    SC_MOVE: AddToList('SC_MOVE', lParam);
    SC_NEXTWINDOW: AddToList('SC_NEXTWINDOW', lParam);
    SC_PREVWINDOW: AddToList('SC_PREVWINDOW', lParam);
    SC_RESTORE: AddToList('SC_RESTORE', lParam);
    SC_SCREENSAVE: AddToList('SC_SCREENSAVE', lParam);
    SC_SIZE: AddToList('SC_SIZE', lParam);
    SC_TASKLIST: AddToList('SC_TASKLIST', lParam);
    SC_VSCROLL: AddToList('SC_VSCROLL', lParam);
  end;
end;


end.
