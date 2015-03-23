program Teste;

uses
  Forms,
  UfrmTelaTeste in 'UfrmTelaTeste.pas' {frmTelaTeste};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmTelaTeste, frmTelaTeste);
  Application.Run;
end.
