program Demo_SAT_FR;

uses
  Vcl.Forms,
  uPrincipal in 'uPrincipal.pas' {fPrincipal};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfPrincipal, fPrincipal);
  Application.Run;
end.
