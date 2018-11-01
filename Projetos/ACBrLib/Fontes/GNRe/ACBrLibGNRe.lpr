{$I ACBr.inc}

library ACBrLibGNRe;

uses
  Interfaces, sysutils, Classes, Forms, ACBrLibGNReClass, ACBrLibConfig,
  ACBrLibComum, ACBrLibConsts, ACBrLibGNReConfig, ACBrLibResposta,
  ACBrLibGNReRespostas, ACBrLibGNReConsts;

{$R *.res}

{$IFDEF DEBUG}
var
   HeapTraceFile : String ;
{$ENDIF}

exports
  // Importadas de ACBrLibComum
  GNRe_Inicializar,
  GNRe_Finalizar,
  GNRe_Nome,
  GNRe_Versao,
  GNRe_UltimoRetorno,
  GNRe_ConfigLer,
  GNRe_ConfigGravar,
  GNRe_ConfigLerValor,
  GNRe_ConfigGravarValor,

  // Servicos
  GNRe_Enviar,
  GNRe_Consultar,
  GNRe_EnviarEmail,
  GNRe_Imprimir,
  GNRe_ImprimirPDF,

  // Arquivos
  GNRe_LimparLista,
  GNRe_CarregarINI,
  GNRe_LimparListaGuiaRetorno,
  GNRe_CarregarGuiaRetorno;

begin
  {$IFDEF DEBUG}
   HeapTraceFile := ExtractFilePath(ParamStr(0))+ 'heaptrclog.trc' ;
   DeleteFile( HeapTraceFile );
   SetHeapTraceOutput( HeapTraceFile );
  {$ENDIF}

  pLibClass := TACBrLibGNRe; // Ajusta a classe a ser criada

  MainThreadID := GetCurrentThreadId();
  Application.Initialize;
end.


