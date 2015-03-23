program Project1;

uses
  Forms,
  Unit1 in 'Unit1.pas' {FrmPrincipal};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'CatronisWilbor';
  Application.CreateForm(TFrmPrincipal, FrmPrincipal);
  Application.Run;
end.
