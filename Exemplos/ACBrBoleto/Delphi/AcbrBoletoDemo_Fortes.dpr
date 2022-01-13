program AcbrBoletoDemo_Fortes;

{$I Report.inc}
uses
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
