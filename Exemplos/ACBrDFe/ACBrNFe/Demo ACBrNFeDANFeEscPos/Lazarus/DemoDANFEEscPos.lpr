program DemoDANFEEscPos;

{$MODE Delphi}

uses
  Forms, Interfaces,
  principal in 'principal.pas' {frmPrincipal};

{.$R *.res}

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmPrincipal, frmPrincipal);
  Application.Run;
end.
