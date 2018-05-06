unit UnitMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient,
  Sockets, WinSock, UnitTcpServer, UnitLogger, StdCtrls;

type
  TForm1 = class(TForm)
    MemoLog: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  UnitLogger.Logger := UnitLogger.TLogger.Create(Application.ExeName + '.log.txt', LOG_MAX_LEVEL);
  Logger.Memolog := MemoLog;
  Logger.info(Format('Application: %s %d', [Application.Exename, GetCurrentProcessId]));
  Logger.info(Format('Version: %s', ['1.0.1@2018.03.08']));
  UnitTcpServer.Thread := UnitTcpServer.TcpServer.Create(True);
  UnitTcpServer.Thread.Resume;
end;

end.
