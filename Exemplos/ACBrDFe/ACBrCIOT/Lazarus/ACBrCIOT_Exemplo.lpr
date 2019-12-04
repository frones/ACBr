program ACBrCIOT_Exemplo;

{$MODE Delphi}

uses
  Forms, Interfaces,
  Frm_ACBrCIOT in 'Frm_ACBrCIOT.pas' {frmACBrCIOT},
  Frm_SelecionarCertificado in 'Frm_SelecionarCertificado.pas' {frmSelecionarCertificado},
  Frm_Status in 'Frm_Status.pas' {frmStatus};

{.$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmACBrCIOT, frmACBrCIOT);
  Application.CreateForm(TfrmSelecionarCertificado, frmSelecionarCertificado);
  Application.CreateForm(TfrmStatus, frmStatus);
  Application.Run;
end.
