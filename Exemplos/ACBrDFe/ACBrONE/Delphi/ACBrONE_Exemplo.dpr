program ACBrONE_Exemplo;

uses
  Forms,
  Frm_ACBrONE in 'Frm_ACBrONE.pas' {frmACBrONE},
  Frm_SelecionarCertificado in 'Frm_SelecionarCertificado.pas' {frmSelecionarCertificado},
  Frm_Status in 'Frm_Status.pas' {frmStatus};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmACBrONE, frmACBrONE);
  Application.CreateForm(TfrmSelecionarCertificado, frmSelecionarCertificado);
  Application.CreateForm(TfrmStatus, frmStatus);
  Application.Run;
end.
