unit SrvOD.Controller;

{$MODE Delphi}

interface

uses
  Horse,
  Horse.CORS,
  SrvOD.Middlewares,
  SrvOD.Controller.Merchant;

procedure CreateServer;
procedure StartServer;
procedure StopServer;

implementation

procedure CreateServer;
begin
  {$IFDEF MSWINDOWS}
  IsConsole := False;
  {$ENDIF}

  THorse
    .Use(CORS)
    .Use(HorseACBrJSON)
    .Use(HorseODError);

  SrvOD.Controller.Merchant.Registry;
end;

procedure ListenHorse(AHorse: THorse);
begin
  System.Writeln('Running...');
  System.Readln;
end;

procedure StartServer;
begin
  {$IFDEF HORSE_VCL}
  THorse.Listen(9030);
  {$ELSE}
  THorse.Listen(9050,
    @ListenHorse);
  {$ENDIF}
end;

procedure StopServer;
begin
  THorse.StopListen;
end;

end.
