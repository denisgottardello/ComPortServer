unit UThTCPServer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  SERIALPORT_TO_REMOTEHOST= 0;
  SERVER_TO_REMOTEHOST= 1;
  SERVER_TO_SERIALPORT= 2;

type RComPort= record
  SocketLocal, SocketRemote, RemoteSocketTimeout, SendRecvTimeout, BufferLength: Integer;
  SerialPort: String;
  BaudRate: Integer;
  Bits: Integer;
  Parity: Char;
  Stop: Integer;
  SoftFlow, HardFlow, RTSToggle: Boolean;
  Mode: Integer;
  RemoteIp: String;
  LogIncoming, LogOutgoing: Boolean;
  LogFileName: String;
end;
type AComPort= array of RComPort;

type PComPort= ^RComPort;

type
  ThTCPServer = class(TThread)
  private
    ApplicationAsConsole: Boolean;
    Index: Integer;
    ComPort: PComPort;
    ComPortServerLog: TextFile;
    procedure OnLog(RowIn: String);
    procedure SerialPortToRemoteHost();
    procedure ServerToRemoteHost();
    procedure ServerToSerialPort();
  protected
    procedure Execute; override;
  public
    constructor Create(CComPort: PComPort; CIndex: Integer; CApplicationAsConsole: Boolean);
    destructor Destroy(); override;
  end;

implementation

uses
  DateUtils, blcksock, synsock, synaser;

constructor ThTCPServer.Create(CComPort: PComPort; CIndex: Integer; CApplicationAsConsole: Boolean);
begin
  inherited Create(false);
  Self.FreeOnTerminate:= false;
  Self.Priority:= tpNormal;
  ApplicationAsConsole:= CApplicationAsConsole;
  ComPort:= CComPort;
  Index:= CIndex;
end;

Destructor ThTCPServer.Destroy();
begin
  inherited Destroy();
end;

procedure ThTCPServer.Execute;
begin
  if not FileExists('ComPortServer'+ IntToStr(Index)+ '.log') then begin
    AssignFile(ComPortServerLog, 'ComPortServer'+ IntToStr(Index)+ '.log');
    Rewrite(ComPortServerLog);
    CloseFile(ComPortServerLog);
  end;
  case ComPort^.Mode of
    SERIALPORT_TO_REMOTEHOST: SerialPortToRemoteHost();
    SERVER_TO_REMOTEHOST: ServerToRemoteHost();
    SERVER_TO_SERIALPORT: ServerToSerialPort();
  end;
  OnLog('ThTCPServer.Execute() Terminate');
end;

procedure ThTCPServer.OnLog(RowIn: String);
begin
  AssignFile(ComPortServerLog, 'ComPortServer'+ IntToStr(Index)+ '.log');
  Append(ComPortServerLog);
  if ApplicationAsConsole then WriteLn(RowIn);
  WriteLn(ComPortServerLog, '['+ TimeToStr(Now())+ '] '+ RowIn);
  CloseFile(ComPortServerLog);
end;

procedure ThTCPServer.SerialPortToRemoteHost();
type AByteIn= array of byte;
var
  AAByteIn: AByteIn;
  TBSSerialPort: TBlockSerial;
  ByteReceived: Integer;
  TBSTCPClientSocket: TTCPBlockSocket;
  TDTLastTCPByte: TDateTime;
  FileLog: File;
begin
  repeat
    if Terminated then break;
    TBSTCPClientSocket:= TTCPBlockSocket.Create();
    try
      OnLog('ThTCPServer.SerialPortToRemoteHost() Connection to '+ ComPort^.RemoteIp+ ':'+ IntToStr(ComPort^.SocketRemote)+ ' in progress...');
      TBSTCPClientSocket.Connect(ComPort^.RemoteIp, IntToStr(ComPort^.SocketRemote));
      OnLog('ThTCPServer.SerialPortToRemoteHost() Connected');
      TDTLastTCPByte:= Now;
      if TBSTCPClientSocket.LastError= 0 then begin
        SetLength(AAByteIn, 0);
        TBSSerialPort:= TBlockSerial.Create();
        if ComPort^.LogIncoming or ComPort^.LogOutgoing then begin
          AssignFile(FileLog, ComPort^.LogFileName);
          ReWrite(FileLog, 1);
        end;
        try
          TBSSerialPort.Connect(ComPort^.SerialPort);
          {$ifdef Linux}TBSSerialPort.Config(ComPort^.BaudRate, ComPort^.Bits, ComPort^.Parity, ComPort^.Stop, ComPort^.SoftFlow, not ComPort^.HardFlow);{$endif} // Stranamente se non si configura per 2 volte la porta spesso non va.
          TBSSerialPort.Config(ComPort^.BaudRate, ComPort^.Bits, ComPort^.Parity, ComPort^.Stop, ComPort^.SoftFlow, ComPort^.HardFlow); // Lo 0 sta per Bit Stop= 1, 1 per 1.5, 2 per 2 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          TBSSerialPort.EnableRTSToggle(ComPort^.RTSToggle);
          if TBSSerialPort.LastError= 0 then begin
            while true and not Terminated do begin
              try
                if TBSSerialPort.CanReadEx(ComPort^.SendRecvTimeout) then begin
                  SetLength(AAByteIn, ComPort^.BufferLength);
                  ByteReceived:= TBSSerialPort.RecvBufferEx(@AAByteIn[0], Length(AAByteIn), ComPort^.SendRecvTimeout);
                  if ByteReceived> 0 then begin
                    TBSTCPClientSocket.SendBuffer(@AAByteIn[0], ByteReceived);
                    if ComPort^.LogIncoming then BlockWrite(FileLog, AAByteIn[0], ByteReceived);
                  end;
                end;
                if TBSTCPClientSocket.CanReadEx(ComPort^.SendRecvTimeout) then begin
                  TDTLastTCPByte:= Now;
                  SetLength(AAByteIn, ComPort^.BufferLength);
                  ByteReceived:= TBSTCPClientSocket.RecvBufferEx(@AAByteIn[0], Length(AAByteIn), ComPort^.SendRecvTimeout);
                  if ComPort^.LogOutgoing then BlockWrite(FileLog, AAByteIn[0], ByteReceived);
                  if ByteReceived> 0 then TBSSerialPort.SendBuffer(@AAByteIn[0], ByteReceived);
                  TBSSerialPort.Flush;
                end;
                if TBSTCPClientSocket.LastError= WSAECONNRESET then begin
                  OnLog('TBSTCPClientSocket.LastErrorDesc: '+ TBSTCPClientSocket.LastErrorDesc);
                  break;
                end;
                if SecondsBetween(TDTLastTCPByte, Now)>= ComPort^.RemoteSocketTimeout then begin
                  OnLog('Remote socket timeout!');
                  break;
                end;
                Except on e: Exception do begin
                  OnLog('ThTCPServer.SerialPortToRemoteHost() C '+ e.Message);
                  break;
                end;
              end;
            end;
          end else OnLog('TBSSerialPort.LastErrorDesc: '+ TBSSerialPort.LastErrorDesc);
        finally
          TBSSerialPort.Free;
        end;
        if ComPort^.LogIncoming or ComPort^.LogOutgoing then CloseFile(FileLog);
      end else OnLog('TBSTCPClientSocket.LastErrorDesc: '+ TBSTCPClientSocket.LastErrorDesc);
    finally
      TBSTCPClientSocket.Free;
      OnLog('ThTCPServer.SerialPortToRemoteHost() Disconnected from remote host');
    end;
  until false;
end;

procedure ThTCPServer.ServerToRemoteHost();
type AByteIn= array of byte;
var
  AAByteIn: AByteIn;
  ByteReceived: Integer;
  TBSTCPServer, TBSTCPServerSocket, TBSTCPClientSocket, TBSTCPServerSocketInQueued: TTCPBlockSocket;
  TDTLastTCPByte: TDateTime;
  FileLog: File;
begin
  TBSTCPServer:= TTCPBlockSocket.Create();
  try
    TBSTCPServer.CreateSocket;
    TBSTCPServer.SetLinger(true, 10);
    TBSTCPServer.EnableReuse(true);
    TBSTCPServer.Bind('0.0.0.0', IntToStr(ComPort^.SocketLocal));
    while TBSTCPServer.LastError<> 0 do begin
      if Terminated then break;
      Sleep(1000);
      TBSTCPServer.Bind('0.0.0.0', IntToStr(ComPort^.SocketLocal));
    end;
    TBSTCPServer.Listen;
    OnLog('ThTCPServer.ServerToRemoteHost() Socket '+ IntToStr(ComPort^.SocketLocal)+ ' open');
    repeat
      if Terminated then break;
      if TBSTCPServer.CanRead(1000) then begin
        OnLog('ThTCPServer.ServerToRemoteHost() Connection in progress...');
        if ComPort^.LogIncoming or ComPort^.LogOutgoing then begin
          AssignFile(FileLog, ComPort^.LogFileName);
          ReWrite(FileLog, 1);
        end;
        TBSTCPServerSocket:= TTCPBlockSocket.Create();
        try
          TBSTCPServerSocket.Socket:= TBSTCPServer.Accept;
          TDTLastTCPByte:= Now;
          OnLog('ThTCPServer.ServerToRemoteHost() Connection from '+ TBSTCPServerSocket.GetRemoteSinIP+ ':'+ IntToStr(TBSTCPServerSocket.GetRemoteSinPort)+ ', '+ TBSTCPServerSocket.ResolveIPToName(TBSTCPServerSocket.GetRemoteSinIP));
          if TBSTCPServerSocket.LastError= 0 then begin
            TBSTCPClientSocket:= TTCPBlockSocket.Create();
            try
              OnLog('ThTCPServer.ServerToRemoteHost() Connection to '+ ComPort^.RemoteIp+ ':'+ IntToStr(ComPort^.SocketRemote)+ ' in progress...');
              TBSTCPClientSocket.Connect(ComPort^.RemoteIp, IntToStr(ComPort^.SocketRemote));
              OnLog('ThTCPServer.ServerToRemoteHost() Connected');
              TDTLastTCPByte:= Now;
              if TBSTCPClientSocket.LastError= 0 then begin
                while true and not Terminated do begin
                  try
                    if TBSTCPClientSocket.CanReadEx(ComPort^.SendRecvTimeout) then begin
                      TDTLastTCPByte:= Now;
                      SetLength(AAByteIn, ComPort^.BufferLength);
                      ByteReceived:= TBSTCPClientSocket.RecvBufferEx(@AAByteIn[0], Length(AAByteIn), ComPort^.SendRecvTimeout);
                      if ComPort^.LogOutgoing then BlockWrite(FileLog, AAByteIn[0], ByteReceived);
                      if ByteReceived> 0 then TBSTCPServerSocket.SendBuffer(@AAByteIn[0], ByteReceived);
                    end;
                    if TBSTCPServerSocket.CanReadEx(ComPort^.SendRecvTimeout) then begin
                      TDTLastTCPByte:= Now;
                      SetLength(AAByteIn, ComPort^.BUFFERLENGTH);
                      ByteReceived:= TBSTCPServerSocket.RecvBufferEx(@AAByteIn[0], Length(AAByteIn), ComPort^.SendRecvTimeout);
                      if ComPort^.LogOutgoing then BlockWrite(FileLog, AAByteIn[0], ByteReceived);
                      if ByteReceived> 0 then TBSTCPClientSocket.SendBuffer(@AAByteIn[0], ByteReceived);
                    end;
                    if TBSTCPServerSocket.LastError= WSAECONNRESET then begin
                      OnLog('TBSTCPServerSocket.LastErrorDesc: '+ TBSTCPServerSocket.LastErrorDesc);
                      break;
                    end;
                    if SecondsBetween(TDTLastTCPByte, Now)>= ComPort^.RemoteSocketTimeout then begin
                      OnLog('Remote socket timeout!');
                      break;
                    end;
                    Except on e: Exception do begin
                      OnLog('ThTCPServer.ServerToRemoteHost() C '+ e.Message);
                      break;
                    end;
                  end;
                  if TBSTCPServer.CanRead(ComPort^.SendRecvTimeout) then begin
                    OnLog('Connection incoming...');
                    TBSTCPServerSocketInQueued:= TTCPBlockSocket.Create();
                    try
                      TBSTCPServerSocketInQueued.Socket:= TBSTCPServer.Accept;
                      TBSTCPServerSocketInQueued.CloseSocket;
                      OnLog('Connection refused!');
                    finally
                      TBSTCPServerSocketInQueued.Free;
                    end;
                  end;
                end;
              end else OnLog('TBSTCPClientSocket.LastErrorDesc: '+ TBSTCPClientSocket.LastErrorDesc);
            finally
              TBSTCPClientSocket.Free;
              OnLog('ThTCPServer.ServerToRemoteHost() Disconnected from remote host');
            end;
          end else OnLog('TBSTCPServerSocket.LastErrorDesc: '+ TBSTCPServerSocket.LastErrorDesc);
        finally
          TBSTCPServerSocket.Free;
          OnLog('ThTCPServer.ServerToRemoteHost() Disconnected');
        end;
        if ComPort^.LogIncoming or ComPort^.LogOutgoing then CloseFile(FileLog);
      end;
    until false;
  finally
    TBSTCPServer.Free;
  end;
  OnLog('ThTCPServer.ServerToRemoteHost() end');
end;

procedure ThTCPServer.ServerToSerialPort();
type AByteIn= array of byte;
var
  AAByteIn: AByteIn;
  TBSSerialPort: TBlockSerial;
  ByteReceived: Integer;
  TBSTCPServer, TBSTCPServerSocket, TBSTCPServerSocketInQueued: TTCPBlockSocket;
  TDTLastTCPByte: TDateTime;
  FileLog: File;
begin
  TBSTCPServer:= TTCPBlockSocket.Create();
  try
    TBSTCPServer.CreateSocket;
    TBSTCPServer.SetLinger(true, 10);
    TBSTCPServer.EnableReuse(true);
    TBSTCPServer.Bind('0.0.0.0', IntToStr(ComPort^.SocketLocal));
    while TBSTCPServer.LastError<> 0 do begin
      if Terminated then break;
      Sleep(1000);
      TBSTCPServer.Bind('0.0.0.0', IntToStr(ComPort^.SocketLocal));
    end;
    TBSTCPServer.Listen;
    OnLog('ThTCPServer.ServerToSerialPort() Socket '+ IntToStr(ComPort^.SocketLocal)+ ' open');
    repeat
      if Terminated then break;
      if TBSTCPServer.CanRead(1000) then begin
        OnLog('ThTCPServer.ServerToSerialPort() Connection in progress...');
        if ComPort^.LogIncoming or ComPort^.LogOutgoing then begin
          AssignFile(FileLog, ComPort^.LogFileName);
          ReWrite(FileLog, 1);
        end;
        TBSTCPServerSocket:= TTCPBlockSocket.Create();
        try
          TBSTCPServerSocket.Socket:= TBSTCPServer.Accept;
          TDTLastTCPByte:= Now;
          OnLog('ThTCPServer.ServerToSerialPort() Connection from '+ TBSTCPServerSocket.GetRemoteSinIP+ ':'+ IntToStr(TBSTCPServerSocket.GetRemoteSinPort)+ ', '+ TBSTCPServerSocket.ResolveIPToName(TBSTCPServerSocket.GetRemoteSinIP));
          if TBSTCPServerSocket.LastError= 0 then begin
            SetLength(AAByteIn, 0);
            TBSSerialPort:= TBlockSerial.Create();
            try
              TBSSerialPort.Connect(ComPort^.SerialPort);
              {$ifdef Linux}TBSSerialPort.Config(ComPort^.BaudRate, ComPort^.Bits, ComPort^.Parity, ComPort^.Stop, ComPort^.SoftFlow, not ComPort^.HardFlow);{$endif} // Stranamente se non si configura per 2 volte la porta spesso non va.
              TBSSerialPort.Config(ComPort^.BaudRate, ComPort^.Bits, ComPort^.Parity, ComPort^.Stop, ComPort^.SoftFlow, ComPort^.HardFlow); // Lo 0 sta per Bit Stop= 1, 1 per 1.5, 2 per 2 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
              TBSSerialPort.EnableRTSToggle(ComPort^.RTSToggle);
              if TBSSerialPort.LastError= 0 then begin
                while true and not Terminated do begin
                  try
                    if TBSSerialPort.CanReadEx(ComPort^.SENDRECVTIMEOUT) then begin
                      SetLength(AAByteIn, ComPort^.BUFFERLENGTH);
                      ByteReceived:= TBSSerialPort.RecvBufferEx(@AAByteIn[0], Length(AAByteIn), ComPort^.SendRecvTimeout);
                      if ByteReceived> 0 then begin
                        TBSTCPServerSocket.SendBuffer(@AAByteIn[0], ByteReceived);
                        if ComPort^.LogIncoming then BlockWrite(FileLog, AAByteIn[0], ByteReceived);
                      end;
                    end;
                    if TBSTCPServerSocket.CanReadEx(ComPort^.SendRecvTimeout) then begin
                      TDTLastTCPByte:= Now;
                      SetLength(AAByteIn, ComPort^.BUFFERLENGTH);
                      ByteReceived:= TBSTCPServerSocket.RecvBufferEx(@AAByteIn[0], Length(AAByteIn), ComPort^.SendRecvTimeout);
                      if ComPort^.LogOutgoing then BlockWrite(FileLog, AAByteIn[0], ByteReceived);
                      if ByteReceived> 0 then TBSSerialPort.SendBuffer(@AAByteIn[0], ByteReceived);
                      TBSSerialPort.Flush;
                    end;
                    if TBSTCPServerSocket.LastError= WSAECONNRESET then begin
                      OnLog('TBSTCPServerSocket.LastErrorDesc: '+ TBSTCPServerSocket.LastErrorDesc);
                      break;
                    end;
                    if SecondsBetween(TDTLastTCPByte, Now)>= ComPort^.RemoteSocketTimeout then begin
                      OnLog('Remote socket timeout!');
                      break;
                    end;
                    Except on e: Exception do begin
                      OnLog('ThTCPServer.ServerToSerialPort() C '+ e.Message);
                      break;
                    end;
                  end;
                  if TBSTCPServer.CanRead(ComPort^.SendRecvTimeout) then begin
                    OnLog('Connection incoming...');
                    TBSTCPServerSocketInQueued:= TTCPBlockSocket.Create();
                    try
                      TBSTCPServerSocketInQueued.Socket:= TBSTCPServer.Accept;
                      TBSTCPServerSocketInQueued.CloseSocket;
                      OnLog('Connection refused!');
                    finally
                      TBSTCPServerSocketInQueued.Free;
                    end;
                  end;
                end;
              end else OnLog('TBSSerialPort.LastErrorDesc: '+ TBSSerialPort.LastErrorDesc);
            finally
              TBSSerialPort.Free;
            end;
          end else OnLog('TBSTCPServerSocket.LastErrorDesc: '+ TBSTCPServerSocket.LastErrorDesc);
        finally
          TBSTCPServerSocket.Free;
          OnLog('ThTCPServer.ServerToSerialPort() Disconnected');
        end;
        if ComPort^.LogIncoming or ComPort^.LogOutgoing then CloseFile(FileLog);
      end;
    until false;
  finally
    TBSTCPServer.Free;
  end;
end;

end.

