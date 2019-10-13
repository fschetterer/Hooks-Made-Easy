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

/// <summary>
///   <para>
///     Hooks all messages posted via PostMessage
///   </para>
///   <para>
///     Written using only Win API and System methods
///   </para>
/// </summary>
/// <remarks>
///   For it to work as expected the App must have:
///   <list type="bullet">
///     <item>
///       Set the Manifest options "UAC Execution Level" to "requireAdministrator
///     </item>
///     <item>
///       Set "UAC Bypass UI Protection" to "Yes (/uiAccess='true')".
///     </item>
///     <item>
///       Application must be Signed
///     </item>
///     <item>
///       "UAC Bypass UI Protection" requires the App to be under %ProgramFiles%
///     </item>
///   </list>
///   In Berlin you need to use a custom manifest: "UAC.requireAdministrator.uiAccess.manifest"
/// </remarks>
unit HooksMadeEasy.GetMsg;

interface

uses
  Winapi.Windows, Winapi.Messages, HooksMadeEasy.Common;

type
  /// <summary>
  ///   Works both with and without a DLL
  /// </summary>
  PGetMsgHook = ^TGetMsgHook;
  TGetMsgHook = record
    class constructor Create;
    class destructor Destroy;
  private
    FMsgHook : HHook;
    FActive: boolean;
    FHwnd : THandle;
    class function GetMsgProc(iCode: integer; wParam: WPARAM; lParam: LPARAM): LRESULT; static; stdcall;
  public
    class function Hookup(AHandle: THandle; AThreadId: Cardinal): Boolean; static;
    class function Stop: boolean; static;
    property Active: boolean read FActive;
  end;

var
  m_MapFile : THandle;
  m_HookData : PGetMsgHook;

implementation

class constructor TGetMsgHook.Create;
const szHooks = SizeOf(TGetMsgHook);
begin
  inherited;
  m_MapFile := CreateFileMapping(INVALID_HANDLE_VALUE, // Creates a file-mapping object of the specified ASize backed by the operating-system paging
      nil, // All Access
      PAGE_READWRITE or // Gives read-write access to the committed region of page
      SEC_NOCACHE or // Noncached data so that programs can write through to the physical memory
      SEC_COMMIT, // Allocates physical storage in memory or in the paging file on disk for all pages of a section.
      0, szHooks, cGetMsgMapFile);
  m_HookData := MapViewOfFile(m_MapFile, FILE_MAP_WRITE, 0, 0, szHooks);
end;

class destructor TGetMsgHook.Destroy;
begin
  UnmapViewOfFile(m_HookData);
  CloseHandle(m_MapFile);
  inherited;
end;

class function TGetMsgHook.GetMsgProc(iCode: integer; wParam: WPARAM; lParam: LPARAM): LRESULT;
var lpMsg : PMsg absolute lParam;
begin
  Result := 0;
  // Return chained call results if Code is less than Zero
  if (iCode < 0) then Exit(CallNextHookEx(0, iCode, wParam, lParam));
  if not m_HookData.Active or (iCode <> HC_ACTION) then Exit;

  if (lpMsg.Message = WM_SYSCOMMAND) then begin
    /// <remarks WM_SYSCOMMAND>
    ///   The four low-order bits of the wParam parameter are used internally by the system.
    ///   To obtain the correct result when testing the value of wParam, an application must
    ///   combine the value 0xFFF0 with the wParam value by using the bitwise AND operator.
    /// </remarks>
    if PostMessage(m_HookData.FHwnd, APP_HOOKMSG, (lpMsg.wParam and $FFF0), lpMsg.lParam) then
      MessageBeep(0);
  end;
end;

class function TGetMsgHook.Hookup(AHandle: THandle; AThreadId: Cardinal): Boolean;
begin
  if m_HookData.FActive then Exit(False);
  m_HookData.FActive   := True;
  m_HookData.FHwnd     := AHandle;
  m_HookData.FMsgHook  := SetWindowsHookExA(WH_GETMESSAGE, GetMsgProc, hInstance, AThreadId);
  Result               := m_HookData.FMsgHook <> 0;
  m_HookData.FActive   := Result;
end;

class function TGetMsgHook.Stop: boolean;
begin
  if not m_HookData.Active then Exit(False);
  m_HookData.FActive := False;
  Result := UnhookWindowsHookEx(m_HookData.FMsgHook);
end;


end.
