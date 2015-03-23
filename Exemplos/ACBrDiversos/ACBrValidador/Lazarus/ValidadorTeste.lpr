program ValidadorTeste;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, ValidadorTeste1, ACBrDiversos;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrValidador, frValidador);
  Application.Run;
end.

