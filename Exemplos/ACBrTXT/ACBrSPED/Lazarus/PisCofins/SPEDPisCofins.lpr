program SPEDPisCofins;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
{$IFnDEF FPC}
{$ELSE}
  Interfaces,
{$ENDIF}
  Forms,
  Frm_SPEDPisCofins in 'Frm_SPEDPisCofins.pas' {FrmSPEDPisCofins};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmSPEDPisCofins, FrmSPEDPisCofins);
  Application.Run;
end.
