program Demo_ACBrNFSe;

uses
  Forms,
  Frm_Demo_ACBrNFSe in 'Frm_Demo_ACBrNFSe.pas' {frmDemo_ACBrNFSe},
  unit2 in 'unit2.pas' {frSelecionarCertificado};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmDemo_ACBrNFSe, frmDemo_ACBrNFSe);
  Application.CreateForm(TfrSelecionarCertificado, frSelecionarCertificado);
  Application.Run;
end.
