program MTerTeste;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, sysutils, memdslaz, Unit1;

{$R *.res}
var
  HeapTraceFile : String ;

begin
  HeapTraceFile := ExtractFilePath(ParamStr(0))+ 'heaptrclog.trc' ;
  DeleteFile( HeapTraceFile );
  SetHeapTraceOutput( HeapTraceFile );

  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

