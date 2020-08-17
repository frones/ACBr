program POSTEFServer;

uses
  Forms,
  FormTelaPrincipal in 'FormTelaPrincipal.pas',
  FormConsultaCNPJ in 'FormConsultaCNPJ.pas' {frConsultaCNPJ},
  FormSelecionarCertificado in 'FormSelecionarCertificado.pas' {frmSelecionarCertificado};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrPOSTEFServer, frPOSTEFServer);
  Application.Run;
end.
