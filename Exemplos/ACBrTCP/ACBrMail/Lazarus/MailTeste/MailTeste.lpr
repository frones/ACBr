program MailTeste;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, sysutils, Unit1;

{$R *.res}

var
   HeapTraceFile : String ;
begin
  {$IFDEF DEBUG}
  HeapTraceFile := ExtractFilePath(ParamStr(0))+ 'heaptrclog.trc' ;
  DeleteFile( HeapTraceFile );
  SetHeapTraceOutput( HeapTraceFile );
  {$ENDIF}

  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

