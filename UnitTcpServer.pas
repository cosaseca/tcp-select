unit UnitTcpServer;

interface

uses
  Classes {$IFDEF MSWINDOWS} , Windows {$ENDIF}, WinSock, SysUtils, PerlRegEx, UnitLogger;

type
  TcpServer = class(TThread)
  private
    procedure SetName;
  protected
    procedure Execute; override;
  end;

var
  Thread: TcpServer;
  SN: String;

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

procedure TcpServer.Execute;
var
  Sock:TSocket;
  WSData:WSAData;
  Timeout:TTimeVal;
  FdSetR:TFDSet;
  ReadFds:TFDSet;
  Addr:TSockAddrIn;
  Ret, Len, I, J:Integer;
  ClientScok:TSocket;
  Buffer:array[1..1024] of AnsiChar;
  SendBuffer:array[1..1024] of AnsiChar;
  RegExHead:TPerlRegEx;
  Cmd: String;
  Data: String;
  StrTmp: String;
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
              Logger.info('recv');
              RegExHead.Subject := Buffer;
              if RegExHead.Match then
              begin
                Cmd := RegExHead.Groups[1];
                Data := RegExHead.Groups[2];
                Logger.info(Format('CMD: %s, DATA: %s', [Cmd, Data]));
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
                        CopyMemory(@SendBuffer[1], PChar(StrTmp), Length(StrTmp));
                        send(FdSetR.fd_array[J], SendBuffer, Length(StrTmp), 0);
                      end;
                    end;
                  end;
                end;  
              end;
            end;    
          end;  
        end;
      end;
    end
    else
    begin
      OutputDebugString('timeout');
    end;  
  end;
  RegExHead.Free;
  shutdown(Sock, SD_BOTH);
  closesocket(Sock);
  WSACleanup();
end;

end.
