program ExemploC6;

uses
  Forms,
  uPrincipal in 'uPrincipal.pas' {frmPrincipal},
  uResposta in 'uResposta.pas' {frmResposta};

{$R *.res}

begin
  //ReportMemoryLeaksOnShutdown := true;
  Application.Initialize;
  Application.CreateForm(TfrmPrincipal, frmPrincipal);
  Application.Run;
end.
