{$I ACBr.inc}

library ACBrLibCTe;

uses
  Interfaces, sysutils, Classes, Forms, ACBrLibCTeClass, ACBrLibConfig,
  ACBrLibComum, ACBrLibConsts, ACBrLibCTeConfig, ACBrLibResposta,
  ACBrLibDistribuicaoDFe, ACBrLibCTeRespostas, ACBrLibCTeConsts;

{$R *.res}

{$IFDEF DEBUG}
var
   HeapTraceFile : String ;
{$ENDIF}

exports
  // Importadas de ACBrLibComum
  CTE_Inicializar,
  CTE_Finalizar,
  CTE_Nome,
  CTE_Versao,
  CTE_UltimoRetorno,
  CTE_ConfigLer,
  CTE_ConfigGravar,
  CTE_ConfigLerValor,
  CTE_ConfigGravarValor,

  // Servicos
  CTE_StatusServico,
  CTE_Inutilizar,
  CTE_Enviar,
  CTE_ConsultarRecibo,
  CTE_Consultar,
  CTE_Cancelar,
  CTE_EnviarEvento,
  CTE_ConsultaCadastro,
  CTE_DistribuicaoDFePorUltNSU,
  CTE_DistribuicaoDFePorNSU,
  CTE_DistribuicaoDFePorChave,
  CTE_EnviarEmail,
  CTE_EnviarEmailEvento,
  CTE_Imprimir,
  CTE_ImprimirPDF,
  CTE_ImprimirEvento,
  CTE_ImprimirEventoPDF,
  CTE_ImprimirInutilizacao,
  CTE_ImprimirInutilizacaoPDF,


  // Arquivos
  CTE_CarregarXML,
  CTE_CarregarINI,
  CTE_CarregarEventoXML,
  CTE_CarregarEventoINI,
  CTE_LimparLista,
  CTE_LimparListaEventos,
  CTE_Assinar,
  CTE_Validar,
  CTE_ValidarRegrasdeNegocios,
  CTE_VerificarAssinatura;

begin
  {$IFDEF DEBUG}
   HeapTraceFile := ExtractFilePath(ParamStr(0))+ 'heaptrclog.trc' ;
   DeleteFile( HeapTraceFile );
   SetHeapTraceOutput( HeapTraceFile );
  {$ENDIF}

  pLibClass := TACBrLibCTe; // Ajusta a classe a ser criada

  MainThreadID := GetCurrentThreadId();
  Application.Initialize;
end.


