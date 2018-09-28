{$I ACBr.inc}

library ACBrLibCTe;

uses
  Interfaces, sysutils, Classes, Forms, ACBrLibCTeClass, ACBrLibConfig,
  ACBrLibComum, ACBrLibConsts, ACBrLibCTeConfig, ACBrLibResposta,
  ACBrLibCTeRespostas, ACBrLibCTeConsts;

{$R *.res}

{$IFDEF DEBUG}
var
   HeapTraceFile : String ;
{$ENDIF}

exports
  // Importadas de ACBrLibComum
  CTe_Inicializar,
  CTe_Finalizar,
  CTe_Nome,
  CTe_Versao,
  CTe_UltimoRetorno,
  CTe_ConfigLer,
  CTe_ConfigGravar,
  CTe_ConfigLerValor,
  CTe_ConfigGravarValor,

  // Servicos
  CTe_StatusServico,
  CTe_Inutilizar,
  CTe_Enviar,
  CTe_Consultar,
  CTe_Cancelar,
  CTe_EnviarEvento,
  CTe_DistribuicaoDFePorUltNSU,
  CTe_DistribuicaoDFePorNSU,
  CTe_DistribuicaoDFePorChave,
  CTe_EnviarEmail,
  CTe_EnviarEmailEvento,
  CTe_Imprimir,
  CTe_ImprimirPDF,
  CTe_ImprimirEvento,
  CTe_ImprimirEventoPDF,
  CTe_ImprimirInutilizacao,
  CTe_ImprimirInutilizacaoPDF,


  // Arquivos
  CTe_CarregarXML,
  CTe_CarregarINI,
  CTe_LimparLista,
  CTe_Assinar,
  CTe_Validar,
  CTe_ValidarRegrasdeNegocios,
  CTe_VerificarAssinatura;

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


