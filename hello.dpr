program hello;

uses
  Forms,
  UnitMain in 'UnitMain.pas' {Form1},
  UnitTcpServer in 'UnitTcpServer.pas',
  UnitLogger in 'UnitLogger.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
