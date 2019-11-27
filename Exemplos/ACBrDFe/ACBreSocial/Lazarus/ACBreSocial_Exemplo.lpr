program ACBreSocial_Exemplo;

{$MODE Delphi}

uses
  Forms, Interfaces,
  Frm_ACBreSocial in 'Frm_ACBreSocial.pas' {frmACBreSocial},
  Frm_SelecionarCertificado in 'Frm_SelecionarCertificado.pas' {frmSelecionarCertificado},
  Frm_Status in 'Frm_Status.pas' {frmStatus};

{.$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmACBreSocial, frmACBreSocial);
  Application.CreateForm(TfrmSelecionarCertificado, frmSelecionarCertificado);
  Application.CreateForm(TfrmStatus, frmStatus);
  Application.Run;
end.
