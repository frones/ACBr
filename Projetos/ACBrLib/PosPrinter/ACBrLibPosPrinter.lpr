{$I ACBr.inc}

library ACBrLibPosPrinter;

uses
  Interfaces, sysutils, Classes, Forms, ACBrLibConfig, ACBrLibComum,
  ACBrLibPosPrinterClass, ACBrLibPosPrinterConfig, ACBrLibPosPrinterDataModule;

{$R *.res}

{$IFDEF DEBUG}
var
   HeapTraceFile : String ;
{$ENDIF}

exports
  // Importadas de ACBrLibComum
  POS_Inicializar,
  POS_Finalizar,
  POS_Nome,
  POS_Versao,
  POS_UltimoRetorno,
  POS_ConfigLer,
  POS_ConfigGravar,
  POS_ConfigLerValor,
  POS_ConfigGravarValor;

begin
  {$IFDEF DEBUG}
   HeapTraceFile := ExtractFilePath(ParamStr(0))+ 'heaptrclog.trc' ;
   DeleteFile( HeapTraceFile );
   SetHeapTraceOutput( HeapTraceFile );
  {$ENDIF}

  pLibClass := TACBrLibPosPrinter; // Ajusta a classe a ser criada

  MainThreadID := GetCurrentThreadId();
end.

