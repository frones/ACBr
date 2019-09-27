program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, memdslaz, datetimectrls, UfmPrincipal, UdmVenda, UfmLiberaAcao
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TdmVenda, dmVenda);
  Application.CreateForm(TfmPrincipal, fmPrincipal);
  Application.CreateForm(TfmLiberaAcao, fmLiberaAcao);
  Application.Run;
end.

