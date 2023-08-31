program ACBrPixCDMatera;

uses
  Forms,
  PixCDMatera in 'PixCDMatera.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrPixCDMatera, frPixCDMatera);
  Application.Run;
end.
