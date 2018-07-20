program PosPrinterTeste;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  sysutils,
  Forms, Unit1, ConfiguraSerial, ACBrEscPosHookElginDLL
  { you can add units after this };

{$R *.res}

begin
  {$IFDEF DEBUG}
   DeleteFile( 'c:\temp\heaptrclog.trc');
   SetHeapTraceOutput( 'c:\temp\heaptrclog.trc');
  {$ENDIF}
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TFrPosPrinterTeste, FrPosPrinterTeste);
  Application.Run;
end.

