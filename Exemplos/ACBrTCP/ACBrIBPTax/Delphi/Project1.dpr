program Project1;

uses
  Forms,
  MidasLib,
  Unit1 in 'Unit1.pas' {Form1},
  ProxyConfig in 'ProxyConfig.pas' {frProxyConfig};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
