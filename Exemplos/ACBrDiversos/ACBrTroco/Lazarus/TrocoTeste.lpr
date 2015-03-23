program TrocoTeste;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, TrocoTeste1;

begin
  Application.Initialize;
  Application.CreateForm(TFrmTroco, FrmTroco);
  Application.Run;
end.

