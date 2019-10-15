/// <summary>
///   Uses RemoteThread to load unload a Hook Dll from a single process.
/// </summary>
program HookLoadTest;

uses
  Vcl.Forms,
  uHookLoadTest in 'uHookLoadTest.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
