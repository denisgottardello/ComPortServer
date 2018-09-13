unit TFMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Buttons, StdCtrls, Menus;

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

type

  { TMainForm }

  TMainForm = class(TForm)
    TBBDelete: TBitBtn;
    GroupBox1: TGroupBox;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    TLBComPorts: TListBox;
    TBBQuit: TBitBtn;
    TBBNewPort: TBitBtn;
    TMIInfo: TMenuItem;
    TMIQuit: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TBBDeleteClick(Sender: TObject);
    procedure TBBNewPortClick(Sender: TObject);
    procedure TBBQuitClick(Sender: TObject);
    procedure TLBComPortsClick(Sender: TObject);
    procedure TLBComPortsDblClick(Sender: TObject);
    procedure TMIInfoClick(Sender: TObject);
  private
    AAComPort: AComPort;
    procedure UpdateList();
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

implementation

uses
  TFSerialPort, IniFiles;

{ TMainForm }

procedure TMainForm.TBBNewPortClick(Sender: TObject);
var
  SerialPort: TSerialPort;
begin
  SerialPort:= TSerialPort.Create(self);
  try
    if SerialPort.ShowModal()= mrOk then begin
      SetLength(AAComPort, Length(AAComPort)+ 1);
      AAComPort[High(AAComPort)].BaudRate:= StrToInt(SerialPort.TCBVelocity.Text);
      AAComPort[High(AAComPort)].Bits:= StrToInt(SerialPort.TCBDataBits.Text);
      AAComPort[High(AAComPort)].BufferLength:= SerialPort.TSEBufferLength.Value;
      AAComPort[High(AAComPort)].HardFlow:= SerialPort.TCBHardFlow.Checked;
      AAComPort[High(AAComPort)].LogFileName:= SerialPort.TELogFileName.Text;
      AAComPort[High(AAComPort)].LogIncoming:= SerialPort.TCBLogIncoming.Checked;
      AAComPort[High(AAComPort)].LogOutgoing:= SerialPort.TCBLogOutoing.Checked;
      AAComPort[High(AAComPort)].Parity:= SerialPort.TCBParity.Text[1];
      AAComPort[High(AAComPort)].RemoteIp:= SerialPort.TERemoteIp.Text;
      AAComPort[High(AAComPort)].RemoteSocketTimeout:= SerialPort.TSERemoteSocketTimeout.Value;
      AAComPort[High(AAComPort)].RTSToggle:= SerialPort.TCBRTSToggle.Checked;
      AAComPort[High(AAComPort)].SendRecvTimeout:= SerialPort.TSESendRecvTimeout.Value;
      AAComPort[High(AAComPort)].SerialPort:= SerialPort.TCBSerialPort.Text;
      AAComPort[High(AAComPort)].SocketLocal:= SerialPort.TSESocketLocal.Value;
      AAComPort[High(AAComPort)].SocketRemote:= SerialPort.TSESocketRemote.Value;
      AAComPort[High(AAComPort)].SoftFlow:= SerialPort.TCBSoftFlow.Checked;
      AAComPort[High(AAComPort)].Stop:= StrToInt(SerialPort.TCBStopBits.Text);
      if SerialPort.TRBServerToSerialPort.Checked then AAComPort[High(AAComPort)].Mode:= SERVER_TO_SERIALPORT
      else if SerialPort.TRBServerToRemoteHost.Checked then AAComPort[High(AAComPort)].Mode:= SERVER_TO_REMOTEHOST
      else AAComPort[High(AAComPort)].Mode:= SERIALPORT_TO_REMOTEHOST;
      UpdateList();
    end;
  finally
    SerialPort.Free;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  TIFFileini: TIniFile;
  I: Integer;
begin
  SetLength(AAComPort, 0);
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
            AAComPort[High(AAComPort)].RemoteSocketTimeout:= TIFFileini.ReadInteger('ComPort'+ IntToStr(I), 'RemoteSocketTimeout', 240);
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
          ShowMessage(e.Message);
        end;
      end;
    finally
      TIFFileini.Free;
    end;
    UpdateList();
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
var
  TIFFileini: TIniFile;
  I: Integer;
begin
  if FileExists('ComPortServer.ini') then if not DeleteFile('ComPortServer.ini') then begin
    ShowMessage('Unable to delete file');
    Exit;
  end;
  TIFFileini:= TIniFile.Create('ComPortServer.ini');
  try
    try
      for I:= 0 to Length(AAComPort)- 1 do begin
        TIFFileini.WriteBool('ComPort'+ IntToStr(I), 'HardFlow', AAComPort[I].HardFlow);
        TIFFileini.WriteBool('ComPort'+ IntToStr(I), 'LogIncoming', AAComPort[I].LogIncoming);
        TIFFileini.WriteBool('ComPort'+ IntToStr(I), 'LogOutgoing', AAComPort[I].LogOutgoing);
        TIFFileini.WriteBool('ComPort'+ IntToStr(I), 'RTSToggle', AAComPort[I].RTSToggle);
        TIFFileini.WriteBool('ComPort'+ IntToStr(I), 'SoftFlow', AAComPort[I].SoftFlow);
        TIFFileini.WriteInteger('ComPort'+ IntToStr(I), 'BaudRate', AAComPort[I].BaudRate);
        TIFFileini.WriteInteger('ComPort'+ IntToStr(I), 'BufferLength', AAComPort[I].BufferLength);
        TIFFileini.WriteInteger('ComPort'+ IntToStr(I), 'Bits', AAComPort[I].Bits);
        TIFFileini.WriteInteger('ComPort'+ IntToStr(I), 'Mode', AAComPort[I].Mode);
        TIFFileini.WriteInteger('ComPort'+ IntToStr(I), 'RemoteSocketTimeout', AAComPort[I].RemoteSocketTimeout);
        TIFFileini.WriteInteger('ComPort'+ IntToStr(I), 'SendRecvTimeout', AAComPort[I].SendRecvTimeout);
        TIFFileini.WriteInteger('ComPort'+ IntToStr(I), 'SocketLocal', AAComPort[I].SocketLocal);
        TIFFileini.WriteInteger('ComPort'+ IntToStr(I), 'SocketRemote', AAComPort[I].SocketRemote);
        TIFFileini.WriteInteger('ComPort'+ IntToStr(I), 'Stop', AAComPort[I].Stop);
        TIFFileini.WriteString('ComPort'+ IntToStr(I), 'LogFileName', AAComPort[I].LogFileName);
        TIFFileini.WriteString('ComPort'+ IntToStr(I), 'Parity', AAComPort[I].Parity);
        TIFFileini.WriteString('ComPort'+ IntToStr(I), 'RemoteIp', AAComPort[I].RemoteIp);
        TIFFileini.WriteString('ComPort'+ IntToStr(I), 'SerialPort', AAComPort[I].SerialPort);
      end;
      Except on e: Exception do begin
        ShowMessage(e.Message);
      end;
    end;
  finally
    TIFFileini.Free;
  end;
end;

procedure TMainForm.TBBDeleteClick(Sender: TObject);
var
  I: Integer;
begin
  for I:= TLBComPorts.ItemIndex to Length(AAComPort)- 2 do AAComPort[I]:= AAComPort[I+ 1];
  SetLength(AAComPort, Length(AAComPort)- 1);
  UpdateList();
end;

procedure TMainForm.UpdateList();
var
  I: Integer;
begin
  TBBDelete.Enabled:= false;
  TLBComPorts.Clear;
  for I:= 0 to Length(AAComPort)- 1 do begin
    TLBComPorts.Items.Append(
                            IntToStr(AAComPort[I].SocketLocal)+ ','+
                            IntToStr(AAComPort[I].SocketRemote)+ ','+
                            IntToStr(AAComPort[I].RemoteSocketTimeout)+ ','+
                            AAComPort[I].SerialPort+ ','+
                            IntToStr(AAComPort[I].BaudRate)+ ','+
                            IntToStr(AAComPort[I].Bits)+ ','+
                            AAComPort[I].Parity+ ','+
                            IntToStr(AAComPort[I].Stop)+ ','+
                            BoolToStr(AAComPort[I].SoftFlow)+ ','+
                            BoolToStr(AAComPort[I].HardFlow)+ ','+
                            BoolToStr(AAComPort[I].RTSToggle)+ ','+
                            IntToStr(AAComPort[I].Mode)+ ','+
                            AAComPort[I].RemoteIp+ ','+
                            BoolToStr(AAComPort[I].LogIncoming)+ ','+
                            BoolToStr(AAComPort[I].LogOutgoing)+ ','+
                            AAComPort[I].LogFileName);
  end;
end;

procedure TMainForm.TBBQuitClick(Sender: TObject);
begin
  Close();
end;

procedure TMainForm.TLBComPortsClick(Sender: TObject);
begin
  TBBDelete.Enabled:= false;
  if (TLBComPorts.ItemIndex> -1) and (TLBComPorts.ItemIndex< TLBComPorts.Count) then TBBDelete.Enabled:= true;
end;

procedure TMainForm.TLBComPortsDblClick(Sender: TObject);
var
  SerialPort: TSerialPort;
begin
  if (TLBComPorts.ItemIndex> -1) and (TLBComPorts.ItemIndex< Length(AAComPort)) then begin
    SerialPort:= TSerialPort.Create(self);
    try
      SerialPort.TCBDataBits.Text:= IntToStr(AAComPort[TLBComPorts.ItemIndex].Bits);
      SerialPort.TCBHardFlow.Checked:= AAComPort[TLBComPorts.ItemIndex].HardFlow;
      SerialPort.TCBLogIncoming.Checked:= AAComPort[TLBComPorts.ItemIndex].LogIncoming;
      SerialPort.TCBLogOutoing.Checked:= AAComPort[TLBComPorts.ItemIndex].LogOutgoing;
      SerialPort.TCBParity.Text:= AAComPort[TLBComPorts.ItemIndex].Parity;
      SerialPort.TCBRTSToggle.Checked:= AAComPort[TLBComPorts.ItemIndex].RTSToggle;
      SerialPort.TCBSerialPort.Text:= AAComPort[TLBComPorts.ItemIndex].SerialPort;
      SerialPort.TCBSoftFlow.Checked:= AAComPort[TLBComPorts.ItemIndex].SoftFlow;
      SerialPort.TCBStopBits.Text:= IntToStr(AAComPort[TLBComPorts.ItemIndex].Stop);
      SerialPort.TCBVelocity.Text:= IntToStr(AAComPort[TLBComPorts.ItemIndex].BaudRate);
      SerialPort.TELogFileName.Text:= AAComPort[TLBComPorts.ItemIndex].LogFileName;
      SerialPort.TERemoteIp.Text:= AAComPort[TLBComPorts.ItemIndex].RemoteIp;
      SerialPort.TSEBufferLength.Value:= AAComPort[TLBComPorts.ItemIndex].BufferLength;
      SerialPort.TSERemoteSocketTimeout.Value:= AAComPort[TLBComPorts.ItemIndex].RemoteSocketTimeout;
      SerialPort.TSESendRecvTimeout.Value:= AAComPort[TLBComPorts.ItemIndex].SendRecvTimeout;
      SerialPort.TSESocketLocal.Value:= AAComPort[TLBComPorts.ItemIndex].SocketLocal;
      SerialPort.TSESocketRemote.Value:= AAComPort[TLBComPorts.ItemIndex].SocketRemote;
      case AAComPort[TLBComPorts.ItemIndex].Mode of
        SERIALPORT_TO_REMOTEHOST: SerialPort.TRBSerialPortToRemoteHost.Checked:= true;
        SERVER_TO_REMOTEHOST: SerialPort.TRBServerToRemoteHost.Checked:= true;
        SERVER_TO_SERIALPORT: SerialPort.TRBServerToSerialPort.Checked:= true;
      end;
      if SerialPort.ShowModal()= mrOk then begin
        AAComPort[TLBComPorts.ItemIndex].BaudRate:= StrToInt(SerialPort.TCBVelocity.Text);
        AAComPort[TLBComPorts.ItemIndex].Bits:= StrToInt(SerialPort.TCBDataBits.Text);
        AAComPort[TLBComPorts.ItemIndex].BufferLength:= SerialPort.TSEBufferLength.Value;
        AAComPort[TLBComPorts.ItemIndex].HardFlow:= SerialPort.TCBHardFlow.Checked;
        AAComPort[TLBComPorts.ItemIndex].LogFileName:= SerialPort.TELogFileName.Text;
        AAComPort[TLBComPorts.ItemIndex].LogIncoming:= SerialPort.TCBLogIncoming.Checked;
        AAComPort[TLBComPorts.ItemIndex].LogOutgoing:= SerialPort.TCBLogOutoing.Checked;
        AAComPort[TLBComPorts.ItemIndex].Parity:= SerialPort.TCBParity.Text[1];
        AAComPort[TLBComPorts.ItemIndex].RemoteIp:= SerialPort.TERemoteIp.Text;
        AAComPort[TLBComPorts.ItemIndex].RemoteSocketTimeout:= SerialPort.TSERemoteSocketTimeout.Value;
        AAComPort[TLBComPorts.ItemIndex].RTSToggle:= SerialPort.TCBRTSToggle.Checked;
        AAComPort[TLBComPorts.ItemIndex].SendRecvTimeout:= SerialPort.TSESendRecvTimeout.Value;
        AAComPort[TLBComPorts.ItemIndex].SerialPort:= SerialPort.TCBSerialPort.Text;
        AAComPort[TLBComPorts.ItemIndex].SocketLocal:= SerialPort.TSESocketLocal.Value;
        AAComPort[TLBComPorts.ItemIndex].SocketRemote:= SerialPort.TSESocketRemote.Value;
        AAComPort[TLBComPorts.ItemIndex].SoftFlow:= SerialPort.TCBSoftFlow.Checked;
        AAComPort[TLBComPorts.ItemIndex].Stop:= StrToInt(SerialPort.TCBStopBits.Text);
        if SerialPort.TRBServerToSerialPort.Checked then AAComPort[TLBComPorts.ItemIndex].Mode:= SERVER_TO_SERIALPORT
        else if SerialPort.TRBServerToRemoteHost.Checked then AAComPort[TLBComPorts.ItemIndex].Mode:= SERVER_TO_REMOTEHOST
        else AAComPort[TLBComPorts.ItemIndex].Mode:= SERIALPORT_TO_REMOTEHOST;
        UpdateList();
      end;
    finally
      SerialPort.Free;
    end;
  end;
end;

procedure TMainForm.TMIInfoClick(Sender: TObject);
begin
  ShowMessage('ComPort by Denis Gottardello, info@denisgottardello.it');
end;

initialization
  {$I TFMainForm.lrs}

end.

