program CHQTeste;

uses
  Forms,
  CHQTeste1 in 'CHQTeste1.pas' {frCHQ};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrCHQ, frCHQ);
  Application.Run;
end.
