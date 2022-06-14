program ACBrGTIN_Exemplo;

uses
  Forms,
  Frm_ACBrGTIN in 'Frm_ACBrGTIN.pas' {frmACBrGTIN},
  Frm_SelecionarCertificado in 'Frm_SelecionarCertificado.pas' {frmSelecionarCertificado},
  Frm_Status in 'Frm_Status.pas' {frmStatus};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmACBrGTIN, frmACBrGTIN);
  Application.CreateForm(TfrmSelecionarCertificado, frmSelecionarCertificado);
  Application.CreateForm(TfrmStatus, frmStatus);
  Application.Run;
end.
