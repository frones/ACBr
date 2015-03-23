program ValidadorTeste;

uses
  Forms,
  ValidadorTeste1 in 'ValidadorTeste1.pas' {frValidador};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrValidador, frValidador);
  Application.Run;
end.
