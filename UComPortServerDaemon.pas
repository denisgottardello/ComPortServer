unit UComPortServerDaemon;

{$mode objfpc}{$H+}
{$ifdef Windows}{$R fclel.res}{$endif}

interface

uses
  Classes, SysUtils, DaemonApp, UMainClass, EventLog;

type
  TComPortServerDaemon = class(TCustomDaemon)
  private
    MainClass: TMainClass;
  public
    function Start : Boolean; override;
    function Stop : Boolean; override;
    procedure Log(TipoEvento: TEventType; Riga: String);
  end;

var
  TTComPortServerDaemon: TComPortServerDaemon;

implementation

procedure TComPortServerDaemon.Log(TipoEvento: TEventType; Riga: String);
var
  TELLog: TEventLog;
begin
  TELLog:= TEventLog.Create(nil);
  try
    {$ifdef Windows}
    begin
      case TipoEvento of
        etInfo: TELLog.EventIDOffset:= 1001;
        etWarning: TELLog.EventIDOffset:= 1002;
        etError: TELLog.EventIDOffset:= 1003;
        etDebug: TELLog.EventIDOffset:= 1004;
      end;
    end;
    {$endif}
    TELLog.LogType:= ltSystem;
    TELLog.Active:= true;
    TELLog.Log(TipoEvento, Riga);
  finally
    TELLog.Free;
  end;
end;

function TComPortServerDaemon.Start: Boolean;
begin
  Result:= inherited Start();
  FormatSettings.DateSeparator:= '/';
  FormatSettings.LongDateFormat:= 'dddd d MMMM yyyy';
  FormatSettings.TimeSeparator := ':';
  FormatSettings.ShortDateFormat:= 'dd/MM/yyyy';
  FormatSettings.ShortTimeFormat:= 'hh:nn:ss';
  SetCurrentDir(ExtractFilePath(ParamStr(0)));
  MainClass:= TMainClass.Create(false);
  Log(etInfo, 'TComPortServerDaemon.Start()');
end;

function TComPortServerDaemon.Stop: Boolean;
begin
  MainClass.Free;
  Log(etInfo, 'TComPortServerDaemon.Stop()');
  Result:= inherited Stop();
end;

end.

