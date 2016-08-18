{$I Report.inc}
program AcbrBoletoDemo;

uses
  Forms,
  uDemo in 'uDemo.pas' {frmDemo},
  {$ifdef demo_forte}
  uDMForte in 'uDMForte.pas' {dmForte: TDataModule};
  {$else}
  uDMFast in 'uDMFast.pas' {dmFast: TDataModule};
  {$endif}

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmDemo, frmDemo);
  {$ifdef demo_forte}
  Application.CreateForm(TdmForte, dmForte);
  {$else}
  Application.CreateForm(TdmFast, dmFast);
  {$endif}
  Application.Run;
end.
