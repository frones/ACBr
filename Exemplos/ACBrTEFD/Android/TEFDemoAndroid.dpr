program TEFDemoAndroid;

uses
  System.StartUpCopy,
  FMX.Forms,
  FormTEFDemoAndroid in 'FormTEFDemoAndroid.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrTEFDemoAndroid, FrTEFDemoAndroid);
  Application.Run;
end.
