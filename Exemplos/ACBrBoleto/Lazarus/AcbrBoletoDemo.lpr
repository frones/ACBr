program AcbrBoletoDemo;

{$MODE Delphi}

uses
  Forms, Interfaces,
  uDemoBoleto in 'uDemoBoleto.pas' {frmDemoBoleto};

{$R *.res}
begin
  Application.Initialize;
  Application.CreateForm(TfrmDemoBoleto, frmDemoBoleto);
  Application.Run;
end.
