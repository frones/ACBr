{$I ACBr.inc}

library ACBrLibNFe;

uses
  Interfaces, sysutils, Classes, Forms, ACBrLibNFeClass, ACBrLibConfig,
  ACBrLibComum, ACBrLibConsts, ACBrLibNFeConfig, ACBrLibResposta,
  DFeReportConfig, ACBrLibNFeRespostas;

{$R *.res}

{$IFDEF DEBUG}
var
   HeapTraceFile : String ;
{$ENDIF}

exports
  // Importadas de ACBrLibComum
  NFE_Inicializar,
  NFE_Finalizar,
  NFE_Nome,
  NFE_Versao,
  NFE_UltimoRetorno,
  NFE_ConfigLer,
  NFE_ConfigGravar,
  NFE_ConfigLerValor,
  NFE_ConfigGravarValor,

  // Servicos
  NFE_StatusServico,
  NFE_Inutilizar,
  NFE_Enviar,
  NFE_Consultar,
  NFE_Cancelar,
  NFE_EnviarEvento,
  NFE_DistribuicaoDFePorUltNSU,
  NFE_DistribuicaoDFePorNSU,
  NFE_DistribuicaoDFePorChave,
  NFE_EnviarEmail,
  NFE_EnviarEmailEvento,
  NFE_Imprimir,
  NFE_ImprimirPDF,
  NFE_ImprimirEvento,
  NFE_ImprimirEventoPDF,
  NFE_ImprimirInutilizacao,
  NFE_ImprimirInutilizacaoPDF,

  // Arquivos
  NFE_CarregarXML,
  NFE_CarregarINI,
  NFE_LimparLista,
  NFE_Assinar,
  NFE_Validar,
  NFE_ValidarRegrasdeNegocios,
  NFE_VerificarAssinatura;

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


