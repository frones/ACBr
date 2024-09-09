program ACBrExtratoAPITeste;

uses
  Forms,
  Principal in 'Principal.pas' {frPrincipal};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrPrincipal, frPrincipal);
  Application.Run;
end.
