unit UnitTcpServer;

interface

uses
  Classes {$IFDEF MSWINDOWS} , Windows {$ENDIF}, WinSock, SysUtils, PerlRegEx, UnitLogger;

type
  TcpServer = class(TThread)
  private
    FdSetR:TFDSet;
    Sock:TSocket;
    procedure SetName;
    function ProcessHandler(Cmd: String; Data: String):Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean);
  end;

var
  Thread: TcpServer;
  SN: String;
  timeplus: Integer;

implementation

{ Important: Methods and properties of objects in visual components can only be
  used in a method called using Synchronize, for example,

      Synchronize(UpdateCaption);

  and UpdateCaption could look like,

    procedure TcpServer.UpdateCaption;
    begin
      Form1.Caption := 'Updated in a thread';
    end; }

{$IFDEF MSWINDOWS}
type
  TThreadNameInfo = record
    FType: LongWord;     // must be 0x1000
    FName: PChar;        // pointer to name (in user address space)
    FThreadID: LongWord; // thread ID (-1 indicates caller thread)
    FFlags: LongWord;    // reserved for future use, must be zero
  end;
{$ENDIF}

{ TcpServer }

procedure TcpServer.SetName;
{$IFDEF MSWINDOWS}
var
  ThreadNameInfo: TThreadNameInfo;
  
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  ThreadNameInfo.FType := $1000;
  ThreadNameInfo.FName := 'TcpServer';
  ThreadNameInfo.FThreadID := $FFFFFFFF;
  ThreadNameInfo.FFlags := 0;

  try
    RaiseException( $406D1388, 0, sizeof(ThreadNameInfo) div sizeof(LongWord), @ThreadNameInfo );
  except
  end;
{$ENDIF}
end;

constructor TcpServer.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  SN := '';
  timeplus := 0;
end;  

procedure TcpServer.Execute;
var
  WSData:WSAData;
  Timeout:TTimeVal;
  ReadFds:TFDSet;
  Addr:TSockAddrIn;
  Ret, Len, I:Integer;
  ClientScok:TSocket;
  Buffer:array[1..1024] of AnsiChar;
  RegExHead:TPerlRegEx;
  Cmd: String;
  Data: String;
  CmdNum: Integer;
begin
  SetName;
  { Place thread code here }
  WSAStartUp($202, WSData);
  Sock := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  Addr.sin_family := PF_INET;
  Addr.sin_port := htons(10086);
  Addr.sin_addr.S_addr := inet_addr(pchar('127.0.0.1'));
  bind(Sock, Addr, SizeOf(Addr));
  listen(Sock, 5);
  FD_ZERO(FdSetR);
  FD_SET(Sock, FdSetR);
  RegExHead := TPerlRegEx.Create;
  RegExHead.RegEx := 'AT\+B (\w+) *(.*)\r\n';
  while (not Terminated) do
  begin
    ReadFds := FdSetR;
    timeout.tv_sec := 1;
    timeout.tv_usec := 0;
    if select(0, @ReadFds, nil, nil, @timeout) > 0 then
    begin
      for I:=0 to ReadFds.fd_count-1 do
      begin
        len := sizeof(addr);
        if FD_ISSET(ReadFds.fd_array[I], ReadFds) then
        begin
          if Sock = ReadFds.fd_array[I] then
          begin
            if ReadFds.fd_count >= FD_SETSIZE then
            begin
              Continue;
            end;  
            ClientScok := accept(Sock, @addr, @len);
            Logger.info('accept');
            if ClientScok <> INVALID_SOCKET then
            begin
              FD_SET(ClientScok, FdSetR);
            end;
          end
          else
          begin
            Ret := recv(ReadFds.fd_array[I], Buffer, SizeOf(Buffer), 0);
            if Ret <= 0 then
            begin
              closesocket(ReadFds.fd_array[I]);
              FD_CLR(ReadFds.fd_array[I], FdSetR);
              Logger.info('close');
            end
            else
            begin
              RegExHead.Subject := Buffer;
              CmdNum := 0;
              while RegExHead.MatchAgain do
              begin
                Cmd := RegExHead.Groups[1];
                Data := RegExHead.Groups[2];
                Logger.info(Format('CMD: %s, DATA: %s', [Cmd, Data]));
                ProcessHandler(Cmd, Data);
                Inc(CmdNum);
              end;
              if CmdNum <= 0 then
              begin
                Logger.info(Format('%s', [PChar(@Buffer[1])]));
              end;
            end;
          end;  
        end;
      end;
    end;  
  end;
  RegExHead.Free;
  shutdown(Sock, SD_BOTH);
  closesocket(Sock);
  WSACleanup();
end;

function TcpServer.ProcessHandler(Cmd: String; Data: String):Integer;
var
  J:Integer;
  StrTmp: String;
  Buffer:array[1..1024] of AnsiChar;
begin
  Result := 0;
  if 'SN' = Cmd then
  begin
    if Length(Data) > 0 then
    begin
      SN := Data;
    end;
    for J:=0 to FdSetR.fd_count-1 do
    begin
      if FdSetR.fd_array[J] <> Sock then
      begin
        if Length(SN) > 0 then
        begin
          StrTmp := Format('AT-B %s%s', [SN, #13#10]);
          CopyMemory(@Buffer[1], PChar(StrTmp), Length(StrTmp));
          send(FdSetR.fd_array[J], Buffer, Length(StrTmp), 0);
        end;
      end;
    end;
  end
  else if 'TIME' = Cmd then
  begin
    if Length(Data) > 0 then
    begin
      timeplus := StrToInt(Data);
    end;
    for J:=0 to FdSetR.fd_count-1 do
    begin
      if FdSetR.fd_array[J] <> Sock then
      begin
        StrTmp := Format('AT-B %d%s', [timeplus, #13#10]);
        CopyMemory(@Buffer[1], PChar(StrTmp), Length(StrTmp));
        send(FdSetR.fd_array[J], Buffer, Length(StrTmp), 0);
      end;
    end;
  end;
end;

end.
