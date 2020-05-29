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
  GNRE_Inicializar,
  GNRE_Finalizar,
  GNRE_Nome,
  GNRE_Versao,
  GNRE_UltimoRetorno,
  GNRE_ConfigLer,
  GNRE_ConfigGravar,
  GNRE_ConfigLerValor,
  GNRE_ConfigGravarValor,

  // Servicos
  GNRE_Enviar,
  GNRE_Consultar,
  GNRE_EnviarEmail,
  GNRE_Imprimir,
  GNRE_ImprimirPDF,

  // Arquivos
  GNRE_LimparLista,
  GNRE_CarregarXML,
  GNRE_CarregarINI,
  GNRE_ObterXml,
  GNRE_GravarXml,
  GNRE_LimparListaGuiaRetorno,
  GNRe_CarregarGuiaRetorno,
  GNRE_Assinar,
  GNRE_Validar,
  GNRE_VerificarAssinatura,
  GNRE_ObterCertificados;

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


