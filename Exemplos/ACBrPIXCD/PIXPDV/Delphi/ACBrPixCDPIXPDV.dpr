program ACBrPixCDPIXPDV;

uses
  Forms,
  PixCDPIXPDV in 'PixCDPIXPDV.pas' {frPixCDPIXPDV};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrPixCDPIXPDV, frPixCDPIXPDV);
  Application.Run;
end.
