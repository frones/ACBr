program ExemploC6;

{$MODE Delphi}

uses
  Forms, datetimectrls, Interfaces,
  uPrincipal in 'uPrincipal.pas' {frmPrincipal},
  uResposta in 'uResposta.pas' {frmResposta};

//{$R *.res}

begin
  //ReportMemoryLeaksOnShutdown := true;
  Application.Initialize;
  Application.CreateForm(TfrmPrincipal, frmPrincipal);
  Application.Run;
end.
