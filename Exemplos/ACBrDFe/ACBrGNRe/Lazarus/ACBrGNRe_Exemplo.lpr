program ACBrGNRe_Exemplo;

{$MODE Delphi}

uses
  Forms, Interfaces,
  Frm_ACBrGNRe in 'Frm_ACBrGNRe.pas' {frmACBrGNRe},
  Frm_SelecionarCertificado in 'Frm_SelecionarCertificado.pas' {frmSelecionarCertificado},
  Frm_Status in 'Frm_Status.pas' {frmStatus};

{.$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmACBrGNRe, frmACBrGNRe);
  Application.CreateForm(TfrmSelecionarCertificado, frmSelecionarCertificado);
  Application.CreateForm(TfrmStatus, frmStatus);
  Application.Run;
end.
