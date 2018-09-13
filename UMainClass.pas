unit UMainClass;

{$mode objfpc}

interface

uses
  Classes, SysUtils, UThTCPServer, IniFiles;

type RObject= record
  TThTCPServer: ThTCPServer;
end;
type AObject= array of RObject;

type

  { TMainClass }

  TMainClass = class
  private
    AAComPort: AComPort;
    AAObject: AObject;
    procedure TerminaThread(ThreadDaTerminare: TThread);
  protected
  public
    constructor Create(ApplicationAsConsole: Boolean);
    destructor Destroy; override;
  end;

var
  MainClass: TMainClass;

implementation

constructor TMainClass.Create(ApplicationAsConsole: Boolean);
var
  TIFFileini: TIniFile;
  I: Integer;
begin
  inherited Create();
  SetLength(AAObject, 0);
  if FileExists('ComPortServer.ini') then begin
    TIFFileini:= TIniFile.Create('ComPortServer.ini');
    try
      try
        I:= 0;
        while true do begin
          if TIFFileini.SectionExists('ComPort'+ IntToStr(I)) then begin
            SetLength(AAComPort, Length(AAComPort)+ 1);
            AAComPort[High(AAComPort)].BaudRate:= TIFFileini.ReadInteger('ComPort'+ IntToStr(I), 'BaudRate', 9600);
            AAComPort[High(AAComPort)].Bits:= TIFFileini.ReadInteger('ComPort'+ IntToStr(I), 'Bits', 8);
            AAComPort[High(AAComPort)].BufferLength:= TIFFileini.ReadInteger('ComPort'+ IntToStr(I), 'BufferLength', 256);
            AAComPort[High(AAComPort)].HardFlow:= TIFFileini.ReadBool('ComPort'+ IntToStr(I), 'HardFlow', false);
            AAComPort[High(AAComPort)].LogFileName:= TIFFileini.ReadString('ComPort'+ IntToStr(I), 'LogFileName', '');
            AAComPort[High(AAComPort)].LogIncoming:= TIFFileini.ReadBool('ComPort'+ IntToStr(I), 'LogIncoming', false);
            AAComPort[High(AAComPort)].LogOutgoing:= TIFFileini.ReadBool('ComPort'+ IntToStr(I), 'LogOutgoing', false);
            AAComPort[High(AAComPort)].Mode:= TIFFileini.ReadInteger('ComPort'+ IntToStr(I), 'Mode', SERVER_TO_SERIALPORT);
            AAComPort[High(AAComPort)].Parity:= TIFFileini.ReadString('ComPort'+ IntToStr(I), 'Parity', 'n')[1];
            AAComPort[High(AAComPort)].RemoteIp:= TIFFileini.ReadString('ComPort'+ IntToStr(I), 'RemoteIp', '');
            AAComPort[High(AAComPort)].RemoteSocketTimeout:= TIFFileini.ReadInteger('ComPort'+ IntToStr(I), 'RemoteSocketTimeout', 30);
            AAComPort[High(AAComPort)].RTSToggle:= TIFFileini.ReadBool('ComPort'+ IntToStr(I), 'RTSToggle', true);
            AAComPort[High(AAComPort)].SendRecvTimeout:= TIFFileini.ReadInteger('ComPort'+ IntToStr(I), 'SendRecvTimeout', 20);
            AAComPort[High(AAComPort)].SerialPort:= TIFFileini.ReadString('ComPort'+ IntToStr(I), 'SerialPort', 'Com1');
            AAComPort[High(AAComPort)].SocketLocal:= TIFFileini.ReadInteger('ComPort'+ IntToStr(I), 'SocketLocal', 3000);
            AAComPort[High(AAComPort)].SocketRemote:= TIFFileini.ReadInteger('ComPort'+ IntToStr(I), 'SocketRemote', 3000);
            AAComPort[High(AAComPort)].SoftFlow:= TIFFileini.ReadBool('ComPort'+ IntToStr(I), 'SoftFlow', false);
            AAComPort[High(AAComPort)].Stop:= TIFFileini.ReadInteger('ComPort'+ IntToStr(I), 'Stop', 1);
            Inc(I);
          end else break;
        end;
        Except on e: Exception do begin
          WriteLn(e.Message);
        end;
      end;
    finally
      TIFFileini.Free;
    end;
  end;
  for I:= 0 to Length(AAComPort)- 1 do begin
    SetLength(AAObject, Length(AAObject)+ 1);
    AAObject[High(AAObject)].TThTCPServer:= ThTCPServer.Create(@AAComPort[I], I, ApplicationAsConsole);
  end;
end;

destructor TMainClass.Destroy;
var
  I: Integer;
begin
  for I:= 0 to Length(AAObject)- 1 do TerminaThread(AAObject[I].TThTCPServer);
  inherited Destroy;
end;

procedure TMainClass.TerminaThread(ThreadDaTerminare: TThread);
begin
  ThreadDaTerminare.Terminate;
  ThreadDaTerminare.WaitFor;
  ThreadDaTerminare.Free;
end;

end.

