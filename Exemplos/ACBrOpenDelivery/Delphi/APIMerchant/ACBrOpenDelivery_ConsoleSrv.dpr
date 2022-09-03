program ACBrOpenDelivery_ConsoleSrv;

{$APPTYPE CONSOLE}

{$R *.res}

{$R *.dres}

uses
  Horse,
  Horse.CORS,
  System.SysUtils,
  SrvOD.Controller.Merchant in 'SrvOD.Controller.Merchant.pas',
  SrvOD.Middlewares in 'Middlewares\SrvOD.Middlewares.pas',
  SrvOD.Controller in 'SrvOD.Controller.pas';

begin
  {$IFDEF MSWINDOWS}
  IsConsole := False;
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}

  CreateServer;
  StartServer;
end.
