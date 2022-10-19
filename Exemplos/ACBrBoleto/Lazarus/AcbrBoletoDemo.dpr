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
  uDemo    in 'uDemo.pas'    {frmDemo},
  uDMForte in 'uDMForte.pas' {dmForte: TDataModule};

{$R *.res}
begin
  Application.Initialize;
  Application.CreateForm(TdmForte, dmForte);
  Application.CreateForm(TfrmDemo, frmDemo);
  Application.Run;
end.
