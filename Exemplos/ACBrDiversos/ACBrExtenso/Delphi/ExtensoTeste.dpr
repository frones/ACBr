program ExtensoTeste;

uses
  Forms,
  ExtensoTeste1 in 'ExtensoTeste1.pas' {frExtenso};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrExtenso, frExtenso);
  Application.Run;
end.
