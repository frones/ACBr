program ACBrMDFe_Exemplo;

{$MODE Delphi}

uses
  Forms, Interfaces,
  Frm_ACBrMDFe in 'Frm_ACBrMDFe.pas' {frmACBrMDFe},
  Frm_SelecionarCertificado in 'Frm_SelecionarCertificado.pas' {frmSelecionarCertificado},
  Frm_Status in 'Frm_Status.pas' {frmStatus};

{.$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmACBrMDFe, frmACBrMDFe);
  Application.CreateForm(TfrmSelecionarCertificado, frmSelecionarCertificado);
  Application.CreateForm(TfrmStatus, frmStatus);
  Application.Run;
end.
