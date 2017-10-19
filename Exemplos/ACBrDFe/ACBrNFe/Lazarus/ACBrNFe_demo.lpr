program ACBrNFE_Demo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, sysutils, Unit1, Unit2;

{$R *.res}

var
  HeapTraceFile : String ;
begin
  HeapTraceFile := ExtractFilePath(ParamStr(0))+ 'heaptrclog.trc' ;
  DeleteFile( HeapTraceFile );
  //SetHeapTraceOutput( HeapTraceFile );

  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

