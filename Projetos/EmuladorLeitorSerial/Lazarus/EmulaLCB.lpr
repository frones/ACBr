program EmulaLCB;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, ACBrSerial
  { add your units here }, EmulaLCB1;

begin
  Application.Initialize;
  Application.CreateForm(TfrEmulador, frEmulador);
  Application.Run;
end.

