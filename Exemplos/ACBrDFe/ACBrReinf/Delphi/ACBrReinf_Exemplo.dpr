program ACBrReinf_Exemplo;

uses
  Forms,
  Frm_ACBrReinf in 'Frm_ACBrReinf.pas' {frmACBrReinf},
  Frm_SelecionarCertificado in 'Frm_SelecionarCertificado.pas' {frmSelecionarCertificado},
  Frm_Status in 'Frm_Status.pas' {frmStatus};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmACBrReinf, frmACBrReinf);
  Application.CreateForm(TfrmSelecionarCertificado, frmSelecionarCertificado);
  Application.CreateForm(TfrmStatus, frmStatus);
  Application.Run;
end.
