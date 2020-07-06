program PosPrinterTeste;

uses
  Forms,
  Unit1 in 'unit1.pas' {FrPosPrinterTeste},
  ConfiguraSerial in 'configuraserial.pas' {frConfiguraSerial};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrPosPrinterTeste, FrPosPrinterTeste);
  Application.Run;
end.

