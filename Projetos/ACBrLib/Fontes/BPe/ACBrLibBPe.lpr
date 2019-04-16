{$I ACBr.inc}

library ACBrLibBPe;

uses
  Interfaces, sysutils, Classes, Forms, ACBrLibBPeClass, ACBrLibConfig,
  ACBrLibComum, ACBrLibConsts, ACBrLibBPeConfig, ACBrLibResposta,
  ACBrLibBPeRespostas, ACBrLibBPeConsts;

{$R *.res}

{$IFDEF DEBUG}
var
   HeapTraceFile : String ;
{$ENDIF}

exports
  // Importadas de ACBrLibComum
  BPe_Inicializar,
  BPe_Finalizar,
  BPe_Nome,
  BPe_Versao,
  BPe_UltimoRetorno,
  BPe_ConfigLer,
  BPe_ConfigGravar,
  BPe_ConfigLerValor,
  BPe_ConfigGravarValor,

  // Servicos
  BPe_StatusServico,
  BPe_Enviar,
  BPe_Consultar,
  BPe_Cancelar,
  BPe_EnviarEvento,
  BPe_DistribuicaoDFePorUltNSU,
  BPe_DistribuicaoDFePorNSU,
  BPe_DistribuicaoDFePorChave,
  BPe_EnviarEmail,
  BPe_EnviarEmailEvento,
  BPe_Imprimir,
  BPe_ImprimirPDF,
  BPe_ImprimirEvento,
  BPe_ImprimirEventoPDF,

  // Arquivos
  BPe_CarregarXML,
  BPe_CarregarINI,
  BPe_LimparLista,
  BPe_Assinar,
  BPe_Validar,
  BPe_ValidarRegrasdeNegocios,
  BPe_VerificarAssinatura;

begin
  {$IFDEF DEBUG}
   HeapTraceFile := ExtractFilePath(ParamStr(0))+ 'heaptrclog.trc' ;
   DeleteFile( HeapTraceFile );
   SetHeapTraceOutput( HeapTraceFile );
  {$ENDIF}

  pLibClass := TACBrLibBPe; // Ajusta a classe a ser criada

  MainThreadID := GetCurrentThreadId();
  Application.Initialize;
end.


