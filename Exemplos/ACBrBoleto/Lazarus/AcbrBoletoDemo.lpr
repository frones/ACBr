program AcbrBoletoDemo;

{$MODE Delphi}

{$I Report.inc}
uses
  Forms, Interfaces,
  uDemo    in 'uDemo.pas'    {frmDemo},
  uDMForte in 'uDMForte.pas' {dmForte: TDataModule};

{.$R *.res}

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TdmForte, dmForte);
  Application.CreateForm(TfrmDemo, frmDemo);
  Application.Run;
end.
