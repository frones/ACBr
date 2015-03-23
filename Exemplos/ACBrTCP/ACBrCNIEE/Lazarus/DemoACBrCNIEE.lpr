program DemoACBrCNIEE;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, ACBrDiversos, ACBrTCP, memdslaz, Principal, ProxyConfig
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
   Application.CreateForm(TfrPrincipal, frPrincipal) ;
  Application.Run;
end.

