program SATTeste;

uses
  Forms,
  UfmPrincipal in 'UfmPrincipal.pas' {fmPrincipal},
  configuraserial in 'configuraserial.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'SAT Teste - Projeto ACBr';
  Application.CreateForm(TfmPrincipal, fmPrincipal);
  Application.Run;
end.

