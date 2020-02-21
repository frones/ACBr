program ACBrPosPrinterAndroid;

uses
  System.StartUpCopy,
  FMX.Forms,
  ACBrPosPrinterAndroidFr in 'ACBrPosPrinterAndroidFr.pas' {PosPrinterAndroidTesteForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TPosPrinterAndroidTesteForm, PosPrinterAndroidTesteForm);
  Application.Run;
end.
