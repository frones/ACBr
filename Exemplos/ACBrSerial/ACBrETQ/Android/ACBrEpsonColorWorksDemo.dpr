program ACBrEpsonColorWorksDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  FormPrincipal in 'FormPrincipal.pas' {FrPrincipal};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrPrincipal, FrPrincipal);
  Application.Run;
end.
