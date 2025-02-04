program ACBrOpenDelivery_ConsoleSrv;

{$mode delphi} {$Hint+}

{$APPTYPE CONSOLE}

uses
  Horse,
  Horse.CORS,
  SysUtils,
  SrvOD.Controller.Merchant in 'SrvOD.Controller.Merchant.pas',
  SrvOD.Middlewares in 'Middlewares\SrvOD.Middlewares.pas',
  SrvOD.Controller in 'SrvOD.Controller.pas';


begin
  {$IFDEF MSWINDOWS}
  IsConsole := False;
  {$ENDIF}

  CreateServer;
  StartServer;
end.
