program AcbrBoletoDemo;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
{$IFnDEF FPC}
{$ELSE}
  Interfaces,
{$ENDIF}
  Forms,
  uDemo, uDMForte;

{$R *.res}
begin
  Application.Initialize;
  Application.CreateForm(TdmForte, dmForte);
  Application.CreateForm(TfrmDemo, frmDemo);
  Application.Run;
end.
