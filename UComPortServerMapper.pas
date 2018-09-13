unit UComPortServerMapper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DaemonApp;

type
  TComPortServerMapper = class(TCustomDaemonMapper)
  Constructor Create(AOwner : TComponent); override;
end;

implementation

constructor TComPortServerMapper.Create(AOwner: TComponent);
var
  D: TDaemonDef;
begin
  inherited Create(AOwner);
  D:= DaemonDefs.Add as TDaemonDef;
  D.DisplayName:= 'ComPortServer';
  D.Name:= 'ComPortServer';
  D.DaemonClassName:= 'TComPortServerDaemon';
  D.WinBindings.ServiceType:= stWin32;
end;

end.
