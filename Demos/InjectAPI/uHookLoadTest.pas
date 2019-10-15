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

unit uHookLoadTest;

interface

uses
  Winapi.Windows, Winapi.Messages, Winapi.PsApi,
  System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    btLoadDllIntoRemoteProcess: TButton;
    btUNLoadDllIntoRemoteProcess: TButton;
    btTerminate: TButton;
    edPID: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure btLoadDllIntoRemoteProcessClick(Sender: TObject);
    procedure btUNLoadDllIntoRemoteProcessClick(Sender: TObject);
    procedure btTerminateClick(Sender: TObject);
  end;

  /// <summary>
  ///   Access to another Process to load unload DLLs usually Hooks
  /// </summary>
  /// <remarks>
  ///   It is not practical to write a Delphi function which can be injected into a remote process since all Delphi functions rely on the RTL and will therefore fail.
  ///   <note type="note">
  ///     The solution is to place this code inside DLLs and inject those instead.
  ///   </note>
  /// </remarks>
  TRemoteThread = record
  public
    /// <summary>
    ///   Looks for a Module with a matching Filename or Module Name loaded in hProcess
    /// </summary>
    /// <param name="AName">
    ///   If this contains a Path Terminator then the whole Filename must match else only the Module Name aka File Name part
    /// </param>
    class function FindDllModule(const hProcess: THandle; const AName: string; out hDll: HModule): Boolean; static;
    /// <summary>
    ///   Load Given DLL into Remote Thread
    /// </summary>
    /// <remarks>
    ///   <note type="note">
    ///     It is also possible to identify the bitness of the remote process and inject the appropriate DLL.
    ///   </note>
    /// </remarks>
    /// <seealso href="https://resources.infosecinstitute.com/using-createremotethread-for-dll-injection-on-windows/">
    ///   Using CreateRemoteThread for DLL Injection on Windows
    /// </seealso>
    class function LoadDll(const PID: DWORD; const FileName: string): boolean; static;
    /// <summary>
    ///   Frees given DLL in Remote Thread
    /// </summary>
    class function FreeDll(const PID: DWORD; const FileName: string): boolean; static;
    /// <summary>
    ///   Attempts to adjust any module address difference due to shims or Address Space Layout Randomization
    /// </summary>
    /// <param name="Module">
    ///   like Kernel32.dll
    /// </param>
    /// <param name="ProcName">
    ///   like LoadLibraryW
    /// </param>
    /// <seealso href="https://stackoverflow.com/a/39262250">
    ///   CreateRemoteThread fails,maybe the lpBaseAddress in the target process is invalid,but it is allocated by the system?
    /// </seealso>
    class function GetTargetProcAddress(const hProcess: THandle; const Module, ProcName: String; out ProcAddr: Pointer): Boolean; static;
  end;

var
  Form1: TForm1;

implementation


{$R *.dfm}

const HookDll = {$IFDEF CPUX64}'Hook64.dll'{$ELSE}'Hook32.dll'{$ENDIF};

procedure TForm1.FormCreate(Sender: TObject);
begin
  edPID.Text := GetCurrentProcessId.ToString;
end;

procedure TForm1.btLoadDllIntoRemoteProcessClick(Sender: TObject);
var LPid : Cardinal;
begin
  If not Cardinal.TryParse(edPID.Text, LPid) then ShowMessage('Unable to parse PID');
  if not TRemoteThread.LoadDll(LPid, ExtractFilePath(ParamStr(0)) + HookDll) then
    Caption := SysErrorMessage(GetLastError)
  else Caption := 'Loaded';
end;

procedure TForm1.btUNLoadDllIntoRemoteProcessClick(Sender: TObject);
var LPid : Cardinal;
begin
  If not Cardinal.TryParse(edPID.Text, LPid) then ShowMessage('Unable to parse PID');
  if not TRemoteThread.FreeDll(LPid, HookDll) then
    Caption := SysErrorMessage(GetLastError)
  else Caption := 'Removed';
end;

procedure TForm1.btTerminateClick(Sender: TObject);
begin
  TerminateProcess(GetCurrentProcess, 0);
end;

class function TRemoteThread.FindDllModule(const hProcess: THandle; const AName: string; out hDll: HModule): Boolean;
var
  Count, Len, BytesNeeded : DWord;
  i: Integer;
  Modules : PHModule;
  Buffer : Pointer;
  LModName : string;
  isFileName : Boolean;
begin
   Result := False;
   isFileName := AName.IndexOf(PathDelim) > -1;
   EnumProcessModules(hProcess, Buffer, 0, BytesNeeded);
   Len := BytesNeeded;
   Buffer := AllocMem(Len);
   Modules := Buffer;
   try
     if not EnumProcessModules(hProcess, Modules, Len, BytesNeeded) then Exit(False);
     Count := Len div SizeOf(HModule);
     for i := 0 to Pred(Count) do begin
        Len := MAX_PATH;
        SetLength(LModName, Len);
        Len := GetModuleFileNameEx(hProcess, Modules^, Pchar(LModName), Len);
        SetLength(LModName, Len);
        hDll := Modules^;
        case isFileName of
         True:  if SameText(LModName, AName) then Exit(True);
         False: if LModName.EndsWith(AName, True) then Exit(True);
        end;
        Inc(Modules);
     end;
   finally
     FreeMem(Buffer);
   end;
end;

class function TRemoteThread.LoadDll(const PID: DWORD; const FileName: string): boolean;
var
  Len, BytesWritten : NativeUInt;
  hProcess, hRemoteThread : THandle;
  buffer, lpLoadLibrary : Pointer;
  dwExitCode, SillyVarCard : DWORD;
const CharSize = 2;
begin
   if not FileExists(FileName) then Exit(False);

   hProcess := OpenProcess(PROCESS_ALL_ACCESS, FALSE, PID);
   if hProcess = 0 then Exit(False);
   try
     if not GetTargetProcAddress(hProcess, Kernel32, 'LoadLibraryW', lpLoadLibrary) then Exit(False);
     Len := (Length(FileName) + 1) * CharSize;
     buffer := VirtualAllocEx(hProcess, nil, Len, MEM_RESERVE or MEM_COMMIT, PAGE_READWRITE);
     if not Assigned(buffer) then Exit(False);
     try
       if not WriteProcessMemory(hProcess, buffer, PChar(FileName), Len, BytesWritten) then Exit(False);
       hRemoteThread := CreateRemoteThread(hProcess, nil, 0, lpLoadLibrary, buffer, 0, SillyVarCard);
       if hRemoteThread = 0 then Exit(False);
       try
         WaitForSingleObject(hRemoteThread, INFINITE); // Simply waits until loading of DLL has completed
         GetExitCodeThread(hRemoteThread, dwExitCode);
         Result := LongBool(dwExitCode); // Non Zero means the thread Ran, Zero the DLL was NOT loaded
         If not Result then begin
           SetLastError(ERROR_NOACCESS);
           Exit(False);
         end;
       finally
         CloseHandle(hRemoteThread);
       end;
     finally
       VirtualFreeEx(hProcess, buffer, 0, MEM_RELEASE);
     end;
   finally
     CloseHandle(hProcess);
   end;
end;

class function TRemoteThread.FreeDll(const PID: DWORD; const FileName: string): boolean;
var
  hProcess, hDll, hRemoteThread : THandle;
  dwExitCode, SillyVarCard : DWORD;
  lpFreeLibrary: Pointer;
begin
   hProcess := OpenProcess(PROCESS_ALL_ACCESS, FALSE, PID);
   if hProcess = 0 then Exit(False);
   try
     if not GetTargetProcAddress(hProcess, Kernel32, 'FreeLibrary', lpFreeLibrary) then Exit(False);
     if not FindDllModule(hProcess, FileName, hDll) then Exit(False);
     {- All Calls are sent as Pointers but FreeLibrary takes a HModule so typecast value to Ptr }
     hRemoteThread := CreateRemoteThread(hProcess, nil, 0, lpFreeLibrary, Ptr(hDll), 0, SillyVarCard);
     if hRemoteThread = 0 then Exit(False);
     try
       WaitForSingleObject(hRemoteThread, INFINITE); // Simply waits until freeing has completed
       GetExitCodeThread(hRemoteThread, dwExitCode);
       Result := LongBool(dwExitCode); // Non Zero means the thread Ran, Zero the DLL was NOT freed
       If not Result then begin
         SetLastError(ERROR_NOACCESS);
         Exit(False);
       end;
     finally
       CloseHandle(hRemoteThread);
     end;
   finally
     CloseHandle(hProcess);
   end;
end;

class function TRemoteThread.GetTargetProcAddress(const hProcess: THandle; const Module, ProcName: String; out ProcAddr: Pointer): Boolean;
var
  hLocalMod, hRemoteMod : THandle;
  ProcValue : UINT_PTR absolute ProcAddr;
  Offset : NativeInt;
begin
  Result := True;
  {- Local Address }
  hLocalMod  := GetModuleHandle(Pchar(Module));
  if hLocalMod = 0 then Exit(False);
  ProcAddr := GetProcAddress(hLocalMod, PChar(ProcName));
  if not Assigned(ProcAddr) then Exit(False);
  {- Remote Address - the module must be loaded else fail }
  if not FindDllModule(hProcess, Module, hRemoteMod) then Exit(False);
  {- Is the Module remapped }
  if (hLocalMod <> hRemoteMod) then begin
    {- We need the Offset to update our Local Address, can be negative }
    Offset := hRemoteMod - hLocalMod;
    {$WARNINGS OFF COMBINING_SIGNED_UNSIGNED}
    {- Add the Remote Offset }
    ProcValue := ProcValue + Offset;
    {$WARNINGS ON}
  end;
end;

end.
