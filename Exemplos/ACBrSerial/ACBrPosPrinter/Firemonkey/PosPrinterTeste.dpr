program PosPrinterTeste;

uses
 FMX.Forms,
  unit1 in 'unit1.pas', {FrPosPrinterTeste}
  configuraserial in 'configuraserial.pas' {frConfiguraSerial};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrPosPrinterTeste, FrPosPrinterTeste);
  Application.Run;
end.

