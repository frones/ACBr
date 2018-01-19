{$I ACBr.inc}

library ACBrLibNFe;

uses
  Interfaces, sysutils, Classes, Forms,
  ACBrLibNFeClass, ACBrLibConfig, ACBrLibComum,
  ACBrLibConsts, ACBrLibNFeConfig, ACBrLibResposta, ACBrLibNFeRespostas;

{$R *.res}

{$IFDEF DEBUG}
var
   HeapTraceFile : String ;
{$ENDIF}

exports
{$I ACBrLibExport.inc}

begin
  {$IFDEF DEBUG}
   HeapTraceFile := ExtractFilePath(ParamStr(0))+ 'heaptrclog.trc' ;
   DeleteFile( HeapTraceFile );
   SetHeapTraceOutput( HeapTraceFile );
  {$ENDIF}

  pLibClass := TACBrLibNFe; // Ajusta a classe a ser criada

  MainThreadID := GetCurrentThreadId();
  Application.Initialize;
end.


