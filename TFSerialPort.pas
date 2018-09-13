unit TFSerialPort;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Spin, Buttons, Menus;

type

  { TSerialPort }

  TSerialPort = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    TRBServerToRemoteHost: TRadioButton;
    TCBDataBits: TComboBox;
    TCBHardFlow: TCheckBox;
    TCBLogIncoming: TCheckBox;
    TCBLogOutoing: TCheckBox;
    TCBParity: TComboBox;
    TCBRTSToggle: TCheckBox;
    TCBSerialPort: TComboBox;
    TCBSoftFlow: TCheckBox;
    TCBStopBits: TComboBox;
    TCBVelocity: TComboBox;
    TELogFileName: TEdit;
    TERemoteIp: TEdit;
    TRBSerialPortToRemoteHost: TRadioButton;
    TRBServerToSerialPort: TRadioButton;
    TSEBufferLength: TSpinEdit;
    TSERemoteSocketTimeout: TSpinEdit;
    TSESendRecvTimeout: TSpinEdit;
    TSESocketLocal: TSpinEdit;
    TSESocketRemote: TSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure TRBServerToSerialPortChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  SerialPort: TSerialPort;

implementation

{$ifdef win32}uses
  synaser;{$endif}

{ TSerialPort }

procedure TSerialPort.FormCreate(Sender: TObject);
{$ifdef Windows}type ARicorrenze= array of Integer;{$endif}
var
  {$ifdef Windows}
    TBSSerialPort: TBlockSerial;
    AARicorrenze: ARicorrenze;
  {$endif}
  I: Integer;
begin
  {$ifdef linux}
    TCBSerialPort.Items.Append('/dev/ttyS0');
    TCBSerialPort.Items.Append('/dev/ttyS1');
    for I:= 0 to 99 do begin
      if FileExists('/dev/ttyUSB'+ IntToStr(I)) then begin
        TCBSerialPort.Items.Append('/dev/ttyUSB'+ IntToStr(I));
      end;
    end;
    for I:= 0 to 99 do begin
      if FileExists('/dev/ttyACM'+ IntToStr(I)) then begin
        TCBSerialPort.Items.Append('/dev/ttyACM'+ IntToStr(I));
      end;
    end;
  {$else}
    TBSSerialPort:= TBlockSerial.Create();
    try
      TBSSerialPort.RaiseExcept:= True;
      SetLength(AARicorrenze, 0);
      for I := 0 to Length(GetSerialPortNames) - 1 do begin
        if GetSerialPortNames[I]= Chr(44) then begin
          SetLength(AARicorrenze, Length(AAricorrenze)+ 1);
          AARicorrenze[Length(AARicorrenze)- 1]:= I;
        end;
      end;
      if Length(AARicorrenze)= 0 then begin
        TCBSerialPort.Items.Append(GetSerialPortNames);
      end else begin
        for I := 0 to Length(AARicorrenze) do begin
          if I= 0 then begin
            TCBSerialPort.Items.Append(Copy(GetSerialPortNames, 0, AARicorrenze[I]- 1));
          end else if I= Length(AARicorrenze) then begin
            TCBSerialPort.Items.Append(Copy(GetSerialPortNames, AARicorrenze[I- 1]+ 1, Length(GetSerialPortNames)- AARicorrenze[I- 1]));
          end else begin
            TCBSerialPort.Items.Append(Copy(GetSerialPortNames, AARicorrenze[I- 1]+ 1, AARicorrenze[I]- 1- AARicorrenze[I- 1]));
          end;
        end;
      end;
    finally
      TBSSerialPort.Free;
    end;
  {$endif}
end;

procedure TSerialPort.TRBServerToSerialPortChange(Sender: TObject);
begin
  TERemoteIp.Enabled:= not TRBServerToSerialPort.Checked;
end;

initialization
  {$I TFSerialPort.lrs}

end.

