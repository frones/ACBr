program ACBrNF3e_Exemplo;

{$MODE Delphi}

uses
  Forms, Interfaces,
  Frm_ACBrNF3e in 'Frm_ACBrNF3e.pas' {frmACBrNF3e},
  Frm_SelecionarCertificado in 'Frm_SelecionarCertificado.pas' {frmSelecionarCertificado},
  Frm_ConfiguraSerial in 'Frm_ConfiguraSerial.pas' {frmConfiguraSerial},
  Frm_Status in 'Frm_Status.pas' {frmStatus};

{.$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmACBrNF3e, frmACBrNF3e);
  Application.CreateForm(TfrmSelecionarCertificado, frmSelecionarCertificado);
  Application.CreateForm(TfrmConfiguraSerial, frmConfiguraSerial);
  Application.CreateForm(TfrmStatus, frmStatus);
  Application.Run;
end.
