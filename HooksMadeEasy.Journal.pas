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
///     Allows Macros recording and playback
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
/// <seealso href="https://edn.embarcadero.com/print/10323">
///   Working Demo
/// </seealso>
unit HooksMadeEasy.Journal;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Classes,
  HooksMadeEasy.Common;

type
  /// <summary>
  ///   Works both with and without a DLL
  /// </summary>
  /// <remarks>
  ///   Traps WM_CANCELJOURNAL internally
  /// </remarks>
  TJournalHook = record
    class constructor Create;
  private
   const cFileName = 'journal.dat';
   class var
    FHook, FMsgHook : HHook;
    FEventMsg: TEventMsg;
    FFile : TFileStream;
    FFileName: string;
    FDelayMS: Cardinal;
    FTicks : TTicks;
    FActive, FSysModalOn, FInstantPlayback: Boolean;
    class procedure ClearVars; static;
    class function GetMsgProc(iCode: integer; wParam: WPARAM; lParam: LPARAM): LRESULT; static; stdcall;
    class function JournalPlaybackProc(iCode: integer; wParam: wParam; lParam: lParam): LRESULT; static; stdcall;
    class function JournalRecordProc(iCode: integer; wParam: wParam; lParam: lParam): LRESULT; static; stdcall;
    class function SetHooks(const AFileMode: Word; const AHookID: Integer; const AHookProc: TFNHookProc): Boolean; static; inline;
  public
    /// <summary>
    ///   Records
    /// </summary>
    /// <returns>
    ///   If False use GetLastError
    /// </returns>
    /// <remarks>
    ///   Traps CTRL+Break to stop Recording
    /// </remarks>
    class function Rec: Boolean; static;
    /// <summary>
    ///   Playback
    /// </summary>
    /// <param name="InstantPlayback">
    ///   Sets Time delay to Zero for all events
    /// </param>
    /// <returns>
    ///   If False use GetLastError
    /// </returns>
    class function Play(const InstantPlayback: Boolean = False): Boolean; static;
    /// <summary>
    ///   Stop either hook, CTRL+ESC or CTRL+ALT+DEL are trapped with a GetMsg hook
    /// </summary>
    /// <returns>
    ///   If False use GetLastError
    /// </returns>
    class function Stop: boolean; static;
    /// <summary>
    ///   True if a Hook is Active
    /// </summary>
    class property Active: boolean read FActive;
    /// <summary>
    ///   File which stores the Journal, defaults to 'journal.dat'
    /// </summary>
    class property FileName: string read FFileName write FFileName;
  end;

implementation

class constructor TJournalHook.Create;
begin
  ClearVars;
  FFileName := cFileName;
  FActive   := False;
end;

class function TJournalHook.JournalPlaybackProc(iCode: integer; wParam: wParam; lParam: lParam): LRESULT;
var
  lpEventMsg : PEventMsg absolute lParam;
  LTicks: Cardinal;
begin
  Result := 0;
  case iCode of
    HC_GETNEXT: begin
      if FSysModalOn or not FActive then Exit;
      /// <remarks>
      ///   If code is HC_GETNEXT and the return value is greater than zero, the system sleeps
      ///   for the number of milliseconds specified by the return value. When the system continues,
      ///   it calls the hook procedure again with code set to HC_GETNEXT to retrieve the same message.
      ///   The return value from this new call to JournalPlaybackProc should be zero;
      ///   otherwise, the system will go back to sleep for the number of milliseconds specified by the return value,
      ///   call JournalPlaybackProc again, and so on. The system will appear to be not responding.
      /// </remarks>
      /// <findings: 09-Oct-2019>
      ///   That doesn't work, but resetting the sleep time (FDelayMS) seems to work..
      /// </findings>
      LTicks := FTicks.RemainingMS(FDelayMS);
      if (LTicks > 0) then Exit(LTicks);
      lpEventMsg^ := FEventMsg;  // Result Zero
    end;
    HC_SKIP: begin
      if not FActive then Exit;
      If (FFile.Position = FFile.Size) then begin
        Stop;
        Exit;
      end;
      LTicks := FEventMsg.time; // record Zeroed in ClearVars
      FFile.Read(FEventMsg, SizeOf(FEventMsg));
      // save Next Delay for calls to HC_GETNEXT, which can be more than one per message
      if (FInstantPlayback or (LTicks = 0)) then FDelayMS := 0
      else FDelayMS := (FEventMsg.time - LTicks); // wait time is diff between last and this msg
      FTicks.StartNew; // Reset the Countdown Timer
    end;
    HC_SYSMODALON: begin
      FSysModalOn := True;
      CallNextHookEx(0, iCode, wParam, lParam);     // Result Zero
    end;
    HC_SYSMODALOFF: begin
      FSysModalOn := False;
      CallNextHookEx(0, iCode, wParam, lParam);     // Result Zero
    end;
    // Return chained call results if iCode is less than Zero
    else if (iCode < 0) then Exit(CallNextHookEx(0, iCode, wParam, lParam));
  end;
end;

class function TJournalHook.JournalRecordProc(iCode: integer; wParam: wParam; lParam: lParam): LRESULT;
  {* IsCTRLBreak: Stops the Hook if True *}
  function IsCTRLBreak(AMsg : PEventMsg): Boolean; inline;
  begin
    Result := (AMsg.message = WM_KEYDOWN) and (Lo(AMsg.ParamL) = VK_CANCEL);
    Result := Result and (GetKeyState(VK_CONTROL) < 0);
    if Result then Stop;
  end;
var lpEventMsg : PEventMsg absolute lParam;
begin
  Result := 0;
  case iCode of
    HC_ACTION: begin
      if FSysModalOn or not FActive then Exit;
      if IsCTRLBreak(lpEventMsg) then Exit;
      FEventMsg := lpEventMsg^;
      FFile.Write(FEventMsg, SizeOf(FEventMsg));
      Exit;
    end;
    HC_SYSMODALON: begin
      FSysModalOn := True;
      CallNextHookEx(0, iCode, wParam, lParam); // Result Zero
    end;
    HC_SYSMODALOFF: begin
      FSysModalOn := False;
      CallNextHookEx(0, iCode, wParam, lParam); // Result Zero
    end;
    // Return chained call results if iCode is less than Zero
    else if (iCode < 0) then Exit(CallNextHookEx(0, iCode, wParam, lParam));
  end;
end;

class function TJournalHook.GetMsgProc(iCode: integer; wParam: WPARAM; lParam: LPARAM): LRESULT;
var lpMsg : PMsg absolute lParam;
begin
  Result := 0;
  // Return chained call results if Code is less than Zero
  if (iCode < 0) then Exit(CallNextHookEx(0, iCode, wParam, lParam));
  if not FActive or (iCode <> HC_ACTION) then Exit;
  if (lpMsg.Message = WM_CANCELJOURNAL) then Stop;
end;

class function TJournalHook.SetHooks(const AFileMode: Word; const AHookID: Integer; const AHookProc: TFNHookProc): Boolean;
begin
  if FActive then Exit(False);
  FActive := True;
  ClearVars;
  FMsgHook := SetWindowsHookExA(WH_GETMESSAGE, GetMsgProc, hInstance, GetCurrentThreadId);
  Result := FMsgHook <> 0;
  if not Result then begin
    FActive := False;
    Exit;
  end;

  /// <findings: 09-Oct-2019>
  ///   Changing to SetWindowsHookExA solved some weirdness
  /// </findings>
  FFile := TFileStream.Create(FFileName, AFileMode);
  try
    FHook := SetWindowsHookExA(AHookID, AHookProc, hInstance, 0);
    Result := FHook <> 0;
  finally
    if not Result then begin
      FActive := False;
      UnhookWindowsHookEx(FMsgHook);
      FreeAndNil(FFile);
    end;
  end;
end;

class function TJournalHook.Play(const InstantPlayback: Boolean = False): Boolean;
begin
  FInstantPlayback := InstantPlayback;
  Result := SetHooks(fmOpenRead, WH_JOURNALPLAYBACK, JournalPlaybackProc);
end;

class function TJournalHook.Rec: Boolean;
begin
  Result := SetHooks(fmCreate, WH_JOURNALRECORD, JournalRecordProc);
end;

class function TJournalHook.Stop: boolean;
begin
  if not FActive then Exit(False);
  FActive := False;
  Result := UnhookWindowsHookEx(FMsgHook) and UnhookWindowsHookEx(FHook);
  FreeAndNil(FFile);
end;

class procedure TJournalHook.ClearVars;
begin
  FHook       := 0;
  FMsgHook    := 0;
  FSysModalOn := False;
  FDelayMS := 0;
  FillChar(FEventMsg, SizeOf(FEventMsg), 0);
end;

end.

