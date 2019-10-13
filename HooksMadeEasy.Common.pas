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
///   Helpers and Constants
/// </summary>
unit HooksMadeEasy.Common;

interface

uses
  Winapi.Windows, Winapi.Messages, Winapi.PsApi,
  System.SysUtils, System.Classes;

type
  /// <summary>
  ///  Wrapper around GetTickCount (instead of Winapi.MMSystem.TimeGetTime)
  /// </summary>
  /// <seealso href="https://www.thedelphigeek.com/2007/10/calculating-accurate.html">
  ///    Started with: Calculating accurate 'Now'
  /// </seealso>
  TTicks = record
{$REGION 'History'}
//  13-Oct-2019 - This is a class helper in Berlin but works as a record in XE2
{$ENDREGION}
    class constructor Create;
  private
    FTicks: Uint64;
    class var FNowHigh32, FNowLastLow32 : Cardinal;
    /// <summary>
    ///   Increments FNowHigh32 for each wrap around, 49.71 days uptime
    /// </summary>
    class function Now64: Uint64; static;
  public
    function ElapsedMS: Cardinal;
    function RemainingMS(const ATimeOutMS: Cardinal): Cardinal;
    procedure StartNew;
  end;

const
  APP_HOOKMSG     = WM_APP + 101;
  cGetMsgMapFile  = '{D9158F23-ED53-470E-B029-15C9E0A06023}.{B1548FFC-07BC-4560-B953-DCCE35A482EB}';

const
  MSGFLT_ALLOW    = 1;
  MSGFLT_DISALLOW = 2;
  MSGFLT_RESET    = 0;

type
 {$MINENUMSIZE 4 DWORD}
 PCHANGEFILTERSTRUCT = ^CHANGEFILTERSTRUCT;
 CHANGEFILTERSTRUCT = record
  cbSize : DWORD;
  ExtStatus {DWORD} : (MSGFLTINFO_NONE, MSGFLTINFO_ALREADYALLOWED_FORWND, MSGFLTINFO_ALREADYDISALLOWED_FORWND, MSGFLTINFO_ALLOWED_HIGHER);
end;

/// <remarks>
///   Windows 7 and up
/// </remarks>
function ChangeWindowMessageFilterEx(hWnd : THandle; Msg : UINT; dwFlag: DWORD; lpChangeFilterStruct : PCHANGEFILTERSTRUCT): BOOL; stdcall; external user32;


implementation

{ TTicks }

class constructor TTicks.Create;
begin
  inherited;
  FNowHigh32 := 0;
  FNowLastLow32 := 0;
end;

class function TTicks.Now64: Uint64;
var Rsl : Int64Rec absolute Result;
begin
  Rsl.Lo := GetTickCount;
  if Rsl.Lo < FNowLastLow32 then Inc(FNowHigh32);
  FNowLastLow32 := Rsl.Lo;
  Rsl.Hi := FNowHigh32;
end;

function TTicks.ElapsedMS: Cardinal;
var LTicks, LValue: Uint64;
begin
  LTicks := TTicks.Now64;
  LValue := LTicks - FTicks;
  if (LValue < 0) then Result := 0
  else if (LValue > MAXDWORD) then Result := MAXDWORD
  else Result := LValue;
end;

function TTicks.RemainingMS(const ATimeOutMS: Cardinal): Cardinal;
var LValue: Cardinal;
begin
  if (ATimeOutMS = INFINITE) then Exit(INFINITE);
  if (ATimeOutMS = 0) then Exit(0);  // Can be Zero if testing a Wait State

  LValue := ElapsedMS;
  if (ATimeOutMS <= LValue) then Exit(0)
  else Result := ATimeOutMS - LValue;
end;

procedure TTicks.StartNew;
begin
  FTicks := TTicks.Now64;
end;


end.
