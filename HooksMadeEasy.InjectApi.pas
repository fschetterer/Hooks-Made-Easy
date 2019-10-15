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

unit HooksMadeEasy.InjectApi;

interface

uses
  Winapi.Windows, DDetours;

type
  TInjectApi = record
    /// <summary>
    ///   Uses DDetours to hook TerminateProcess
    /// </summary>
    class constructor Create;
    class destructor Destroy;
  private
    type
      TTerminateProcess = function(hProcess: THandle; uExitCode: UINT): BOOL; stdcall;
    class var
      FActive: Boolean;
      FTerminateProcessTrampo: TTerminateProcess;
    class function HookedTerminateProcess(hProcess: THandle; uExitCode: UINT): BOOL; static; stdcall;
  public
    class property Active: Boolean read FActive;
  end;


implementation

class constructor TInjectApi.Create;
begin
  inherited;
  if FActive then Exit;
  FActive := True;
  DisableThreadLibraryCalls(hInstance);
  @FTerminateProcessTrampo := InterceptCreate(kernel32, 'TerminateProcess', @HookedTerminateProcess);
end;

class destructor TInjectApi.Destroy;
begin
  if not FActive then Exit;
  FActive := False;
  InterceptRemove(@FTerminateProcessTrampo);
end;

class function TInjectApi.HookedTerminateProcess(hProcess: THandle; uExitCode: UINT): BOOL;
begin
  MessageBox(0, 'This Hook stops me from closing.', 'Hook.Dll', MB_OK);
  Result := True;
end;

end.
