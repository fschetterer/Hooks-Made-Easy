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
///     Hooks all GetMessage or PeekMessage functions when there is a keyboard message (WM_KEYUP or WM_KEYDOWN) to be processed.
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
unit HooksMadeEasy.Keyboard;

interface

uses
  Winapi.Windows, Winapi.Messages;

{$I HooksMadeEasy.Keyboard.Shared.inc}

type
  /// <summary>
  ///   Keystroke Message Flags <br />First 32 bits of LParam
  /// </summary>
  /// <remarks>
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Bits</term>
  ///       <description>Description</description>
  ///     </listheader>
  ///     <item>
  ///       <term>0-15</term>
  ///       <description>The repeat count. The value is the number of times the keystroke is repeated as a result of the user's holding down the key</description>
  ///     </item>
  ///     <item>
  ///       <term>16-23</term>
  ///       <description>The scan code. The value depends on the OEM.</description>
  ///     </item>
  ///     <item>
  ///       <term>24</term>
  ///       <description>Indicates whether the key is an extended key, such as a function key or a key on the numeric keypad. The value is 1 if the key is an extended key;
  ///         otherwise, it is 0.</description>
  ///     </item>
  ///     <item>
  ///       <term>25-28</term>
  ///       <description>Reserved.</description>
  ///     </item>
  ///     <item>
  ///       <term>29</term>
  ///       <description>The context code. The value is 1 if the ALT key is down; otherwise, it is 0.</description>
  ///     </item>
  ///     <item>
  ///       <term>30</term>
  ///       <description>The previous key state. The value is 1 if the key is down before the message is sent; it is 0 if the key is up.</description>
  ///     </item>
  ///     <item>
  ///       <term>31</term>
  ///       <description>The transition state. The value is 0 if the key is being pressed and 1 if it is being released.</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TKeyMsgRec = record
    case LPARAM of
      0: ( TimesRepeated: uint16;
           ScanCode: uint8;
           KeyFlagBits: set of (kbEXTENDED,
                                kbReserved_25, kbReserved_26, kbReserved_27, kbReserved_28,
                                kbALTDWN, kbPrevKeyState, kbTransitionState));
      1: ( lParam: LPARAM);
  end;

  /// <summary>
  ///   Works both with and without a DLL
  /// </summary>
  PKeyboardHook = ^TKeyboardHook;
  TKeyboardHook = record
    class constructor Create;
    class destructor Destroy;
  private
    FMsgHook : HHook;
    FActive: boolean;
    FHwnd : THandle;
    class function KeyboardProc(iCode: integer; VKCode: WPARAM; lParam: LPARAM): LRESULT; static; stdcall;
  public
    class function Hookup(AHandle: THandle; AThreadId: Cardinal): Boolean; static;
    class function Stop: boolean; static;
    property Active: boolean read FActive;
  end;

var
  m_MapFile : THandle;
  m_HookData : PKeyboardHook;

implementation

class constructor TKeyboardHook.Create;
const szHooks = SizeOf(TKeyboardHook);
begin
  inherited;
  m_MapFile := CreateFileMapping(INVALID_HANDLE_VALUE, // Creates a file-mapping object of the specified ASize backed by the operating-system paging
      nil, // All Access
      PAGE_READWRITE or // Gives read-write access to the committed region of page
      SEC_NOCACHE or // Noncached data so that programs can write through to the physical memory
      SEC_COMMIT, // Allocates physical storage in memory or in the paging file on disk for all pages of a section.
      0, szHooks, cKeyboardMapFile);
  m_HookData := MapViewOfFile(m_MapFile, FILE_MAP_WRITE, 0, 0, szHooks);
end;

class destructor TKeyboardHook.Destroy;
begin
  UnmapViewOfFile(m_HookData);
  CloseHandle(m_MapFile);
  inherited;
end;

class function TKeyboardHook.KeyboardProc(iCode: integer; VKCode: WPARAM; lParam: LPARAM): LRESULT;
var lpKey : TKeyMsgRec absolute lParam;
begin
  Result := 0;
  // Return chained call results if Code is less than Zero
  if (iCode < 0) then Exit(CallNextHookEx(0, iCode, VKCode, lParam));
  if not m_HookData.Active or (iCode <> HC_ACTION) then Exit;

  if (VKCode = VK_NUMPAD1) and (kbALTDWN in lpKey.KeyFlagBits)
  then begin
    if PostMessage(m_HookData.FHwnd, APP_HOOKMSG, Ord(kbPrevKeyState in lpKey.KeyFlagBits), Ord(kbTransitionState in lpKey.KeyFlagBits)) then
      MessageBeep(0);
  end;
end;

class function TKeyboardHook.Hookup(AHandle: THandle; AThreadId: Cardinal): Boolean;
begin
  if m_HookData.FActive then Exit(False);
  m_HookData.FActive   := True;
  m_HookData.FHwnd     := AHandle;
  m_HookData.FMsgHook  := SetWindowsHookExA(WH_KEYBOARD, KeyboardProc, hInstance, AThreadId);
  Result               := m_HookData.FMsgHook <> 0;
  m_HookData.FActive   := Result;
end;

class function TKeyboardHook.Stop: boolean;
begin
  if not m_HookData.Active then Exit(False);
  m_HookData.FActive := False;
  Result := UnhookWindowsHookEx(m_HookData.FMsgHook);
end;


end.
