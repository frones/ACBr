program ExemploEsocial;

{$MODE Delphi}

uses
  Forms, Interfaces,
  uExemploEsocial in 'uExemploEsocial.pas' {FExemploEsocial},
  unit2 in 'unit2.pas' {frSelecionarCertificado},
  ufrmStatus in 'ufrmStatus.pas' {frmStatus};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFExemploEsocial, FExemploEsocial);
  Application.CreateForm(TfrSelecionarCertificado, frSelecionarCertificado);
  Application.CreateForm(TfrmStatus, frmStatus);
  Application.Run;
end.
