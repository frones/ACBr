program ExtensoTeste;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, ExtensoTeste1;

begin
  Application.Initialize;
  Application.CreateForm(TfrExtenso, frExtenso);
  Application.Run;
end.

