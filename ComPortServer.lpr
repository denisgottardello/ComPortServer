program ComPortServer;

{$mode objfpc}{$H+}

uses
  {$ifdef Linux}
    cthreads, BaseUnix, EventLog, Process,
  {$else ifdef Windows}
    Registry, UComPortServerDaemon, UComPortServerMapper,
  {$endif}
  Classes, SysUtils, CustApp, IniFiles, KeyBoard, UMainClass, DaemonApp;

  { TComPortServer }

var
  K: TKeyEvent;
  CanClose: Boolean;
  {$ifdef Linux}
    Oact, Act: PSigActionRec;
  {$else ifdef Windows}
    Reg: TRegistry;
  {$endif}
  MainClass: TMainClass;


procedure OnDestroy();
begin
  MainClass.Free;
  WriteLn('TMainClass.OnDestroy()');
  {$ifdef Linux}
    FreeMem(Act);
    FreeMem(Oact);
  {$endif}
  WriteLn('ComPortServer ended normally');
end;

{$ifdef Linux}
  procedure DoSig(Sig: cint); cdecl;
  var
    TELLog: TEventLog;
  begin
    begin
      case Sig of
        SigUsr1: CanClose:= true;
        SigUsr2: begin
          TELLog:= TEventLog.Create(nil);
          try
            TELLog.LogType:= ltSystem;
            TELLog.Active:= true;
            TELLog.Log(etInfo, 'I think that all is ok');
          finally
            TELLog.Free;
          end;
        end;
      end;
    end;
  end;
{$endif}

procedure OnCreate(ApplicationAsConsole: Boolean);
begin
  CanClose:= false;
  FormatSettings.DateSeparator:= '/';
  FormatSettings.LongDateFormat:= 'dddd d MMMM yyyy';
  FormatSettings.TimeSeparator := ':';
  FormatSettings.ShortDateFormat:= 'dd/MM/yyyy';
  FormatSettings.ShortTimeFormat:= 'hh:nn:ss';
  {$ifdef Linux}
    New(Act);
    New(Oact);
    Act^.sa_Handler:= SigActionHandler(@DoSig);
    FillChar(Act^.Sa_Mask, SizeOf(Act^.sa_mask), #0);
    Act^.Sa_Flags:= 0;
    Act^.Sa_Restorer:= nil;
    if (fpSigAction(SigUsr1, Act, Oact)<> 0) or (fpSigAction(SigUsr2, Act, Oact)<> 0) then begin
     WriteLn('Error: ', fpgeterrno, '.');
     Halt(1);
    end;
  {$endif}
  MainClass:= TMainClass.Create(ApplicationAsConsole);
  WriteLn('TMainClass.Create()');
end;

{$ifdef Linux}
  procedure ExecuteAProcess(Process, Parameters: String);
  var
    AProcess: TProcess;
  begin
    AProcess:= TProcess.Create(nil);
    try
      try
        AProcess.Executable:= Process;
        AProcess.Parameters.Text:= Parameters;
        AProcess.Options:= AProcess.Options+ [poWaitOnExit];
        AProcess.Execute;
        Except on e: Exception do begin
          WriteLn(e.Message);
        end;
      end;
    finally
      AProcess.Free;
    end;
  end;
{$endif}

procedure DetectKeyEvent();
begin
  InitKeyBoard;
  Repeat
    if PollKeyEvent<> 0 then begin
      K:= GetKeyEvent;
      K:= TranslateKeyEvent(K);
    end;
    Sleep(100);
  Until (GetKeyEventChar(K)= 'q') or CanClose;
  DoneKeyBoard;
end;

{$R *.res}

begin
  {$ifdef Linux}
    if Application.HasOption('s', 'start') then begin
      WriteLn('Do start');
      ExecuteAProcess('/etc/init.d/ComPortServer', 'start');
    end else if Application.HasOption('t', 'stop') then begin
      WriteLn('Do stop');
      ExecuteAProcess('/etc/init.d/ComPortServer', 'stop');
    end else if Application.HasOption('r', 'restart') then begin
      WriteLn('Do restart');
      ExecuteAProcess('/etc/init.d/ComPortServer', 'restart');
    end else if Application.HasOption('u', 'status') then begin
      WriteLn('Do status');
      ExecuteAProcess('/etc/init.d/ComPortServer', 'status');
    end else if Application.HasOption('d', 'AsDaemon') then begin
      OnCreate(false);
      Repeat Sleep(500) until CanClose;
      OnDestroy();
    end else begin
      OnCreate(true);
      WriteLn('Press "q" to exit or send signal USR1 using kill -USR1 '+ IntToStr(GetProcessID));
      DetectKeyEvent();
      OnDestroy();
    end;
  {$else ifdef Windows}
    if Application.HasOption('i', 'install') or Application.HasOption('r', 'run') or Application.HasOption('u', 'uninstall') then begin
      // Application as daemon
      RegisterDaemonClass(TComPortServerDaemon);
      RegisterDaemonMapper(TComPortServerMapper);
      Application.Title:= 'ComPortServer';
      Application.Run;
      if Application.HasOption('i', 'install') then begin
        Reg:= TRegistry.Create(KEY_READ or KEY_WRITE);
        try
          Reg.RootKey:= HKEY_LOCAL_MACHINE;
          if Reg.OpenKey('\SYSTEM\CurrentControlSet\Services\' + 'ComPortServer', false) then begin
            Reg.WriteString('Description', 'ComPortServer to remotize your serial port');
            Reg.WriteString('DependOnService', 'winmgmt');
            Reg.CloseKey;
          end;
        finally
          Reg.Free;
        end;
      end;
    end else begin
      // Application as console program
      OnCreate(true);
      WriteLn('Press "q" for exit');
      DetectKeyEvent();
      OnDestroy();
    end;
  {$endif}
end.
