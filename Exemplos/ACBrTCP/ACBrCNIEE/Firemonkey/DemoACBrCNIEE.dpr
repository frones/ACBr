program DemoACBrCNIEE;

uses
 FMX.Forms,
  Principal in 'Principal.pas' {frPrincipal},
  ProxyConfig in 'ProxyConfig.pas' {frProxyConfig};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrPrincipal, frPrincipal);
  Application.Run;
end.
