program CupomVerdeTeste;

uses
  Forms,
  Principal in 'Principal.pas' {frACBrCupomVerdeTeste};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrACBrCupomVerdeTeste, frACBrCupomVerdeTeste);
  Application.Run;
end.
