{$I ACBr.inc}

library ACBrLibGNRe;

uses
  {$IFDEF MT}
   {$IFDEF UNIX}
    cthreads,
    cmem, // the c memory manager is on some systems much faster for multi-threading
   {$ENDIF}
  {$ENDIF}
  Interfaces, sysutils, Classes, Forms, ACBrLibConfig,
  {$IFDEF MT}ACBrLibGNReMT{$ELSE}ACBrLibGNReST{$ENDIF},
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
  GNRE_ConfigImportar,
  GNRE_ConfigExportar,
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
  GNRE_CarregarGuiaRetorno,
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

  MainThreadID := GetCurrentThreadId();
  Application.Initialize;
end.


