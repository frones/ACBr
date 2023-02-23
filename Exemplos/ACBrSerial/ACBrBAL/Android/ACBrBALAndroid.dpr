program ACBrBALAndroid;

uses
  System.StartUpCopy,
  FMX.Forms,
  ACBrBALAndroidFr in 'ACBrBALAndroidFr.pas' {BALAndroidTesteForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TBALAndroidTesteForm, BALAndroidTesteForm);
  Application.Run;
end.
