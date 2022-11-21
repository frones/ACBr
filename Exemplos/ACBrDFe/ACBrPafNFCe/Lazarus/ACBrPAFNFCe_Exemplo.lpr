program ACBrPAFNFCe_Exemplo;

{$MODE Delphi}

uses
  Forms, Interfaces,
  Frm_ACBrPAFNFCe in 'Frm_ACBrPAFNFCe.pas' {frmACBrPAFNFCe},
  Frm_SelecionarCertificado in 'Frm_SelecionarCertificado.pas' {frmSelecionarCertificado};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmACBrPAFNFCe, frmACBrPAFNFCe);
  Application.Run;
end.
