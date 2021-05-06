program TEFDemoAndroid;

uses
  System.StartUpCopy,
  FMX.Forms,
  FormTEFDemoAndroid in 'FormTEFDemoAndroid.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
