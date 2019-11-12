program AcbrBoletoDemo_Fast;

{$I Report.inc}
uses
  Forms,
  uDemo in 'uDemo.pas' {frmDemo},
  uDMFast in 'uDMFast.pas' {dmFast: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TdmFast, dmFast);
  Application.CreateForm(TfrmDemo, frmDemo);
  Application.Run;
end.
