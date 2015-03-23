program CHQTeste;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, CHQTeste1;

begin
  Application.Initialize;
  Application.CreateForm(TfrCHQ, frCHQ);
  Application.Run;
end.

