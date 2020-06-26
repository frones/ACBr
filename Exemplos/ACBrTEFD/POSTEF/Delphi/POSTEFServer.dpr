program POSTEFServer;

uses
  Forms,
  FormTelaPrincipal in 'FormTelaPrincipal.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrPOSTEFServer, frPOSTEFServer);
  Application.Run;
end.
