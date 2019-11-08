{$I ACBr.inc}

library ACBrLibMDFe;

uses
  Interfaces, sysutils, Classes, Forms, ACBrLibMDFeClass, ACBrLibConfig,
  ACBrLibComum, ACBrLibConsts, ACBrLibMDFeConfig, ACBrLibResposta,
  ACBrLibMDFeRespostas, ACBrLibMDFeConsts;

{$R *.res}

{$IFDEF DEBUG}
var
   HeapTraceFile : String ;
{$ENDIF}

exports
  // Importadas de ACBrLibComum
  MDFE_Inicializar,
  MDFE_Finalizar,
  MDFE_Nome,
  MDFE_Versao,
  MDFE_UltimoRetorno,
  MDFE_ConfigLer,
  MDFE_ConfigGravar,
  MDFE_ConfigLerValor,
  MDFE_ConfigGravarValor,

  // Servicos
  MDFE_StatusServico,
  MDFE_Enviar,
  MDFE_ConsultarRecibo,
  MDFE_Consultar,
  MDFE_Cancelar,
  MDFE_EnviarEvento,
  MDFE_DistribuicaoDFePorUltNSU,
  MDFE_DistribuicaoDFePorNSU,
  MDFE_DistribuicaoDFePorChave,
  MDFE_EnviarEmail,
  MDFE_EnviarEmailEvento,
  MDFE_Imprimir,
  MDFE_ImprimirPDF,
  MDFE_ImprimirEvento,
  MDFE_ImprimirEventoPDF,

  // Arquivos
  MDFE_CarregarXML,
  MDFE_CarregarINI,
  MDFE_ObterXml,
  MDFE_GravarXml,
  MDFE_CarregarEventoXML,
  MDFE_CarregarEventoINI,
  MDFE_LimparLista,
  MDFE_LimparListaEventos,
  MDFE_Assinar,
  MDFE_Validar,
  MDFE_ValidarRegrasdeNegocios,
  MDFE_VerificarAssinatura;

begin
  {$IFDEF DEBUG}
   HeapTraceFile := ExtractFilePath(ParamStr(0))+ 'heaptrclog.trc' ;
   DeleteFile( HeapTraceFile );
   SetHeapTraceOutput( HeapTraceFile );
  {$ENDIF}

  pLibClass := TACBrLibMDFe; // Ajusta a classe a ser criada

  MainThreadID := GetCurrentThreadId();
  Application.Initialize;
end.


