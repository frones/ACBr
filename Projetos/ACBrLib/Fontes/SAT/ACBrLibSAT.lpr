{$I ACBr.inc}

library ACBrLibSat;

uses
  Interfaces, sysutils, Classes, Forms,
  ACBrLibConfig, ACBrLibComum, DFeReportConfig,
  ACBrLibSATConfig, ACBrLibSatDataModule, ACBrLibSATClass;

{$R *.res}

{$IFDEF DEBUG}
var
   HeapTraceFile : String ;
{$ENDIF}

exports
  // Importadas de ACBrLibComum
  SAT_Inicializar,
  SAT_Finalizar,
  SAT_Nome,
  SAT_Versao,
  SAT_UltimoRetorno,
  SAT_ConfigLer,
  SAT_ConfigGravar,
  SAT_ConfigLerValor,
  SAT_ConfigGravarValor,

  SAT_InicializarSAT,
  SAT_DesInicializar,

  SAT_AssociarAssinatura,
  SAT_BloquearSAT,
  SAT_DesbloquearSAT,
  SAT_TrocarCodigoDeAtivacao,
  SAT_ConsultarSAT,
  SAT_ConsultarStatusOperacional,
  SAT_AtualizarSoftwareSAT,
  SAT_ComunicarCertificadoICPBRASIL,
  SAT_ExtrairLogs,
  SAT_TesteFimAFim,
  SAT_GerarAssinaturaSAT,

  SAT_CriarCFe,
  SAT_CriarEnviarCFe,
  SAT_EnviarCFe,
  SAT_CancelarCFe,

  SAT_ImprimirExtratoVenda,
  SAT_ImprimirExtratoResumido,
  SAT_ImprimirExtratoCancelamento,
  SAT_GerarPDFExtratoVenda,
  SAT_GerarPDFCancelamento,
  SAT_EnviarEmail;

begin
  {$IFDEF DEBUG}
   HeapTraceFile := ExtractFilePath(ParamStr(0))+ 'heaptrclog.trc' ;
   DeleteFile( HeapTraceFile );
   SetHeapTraceOutput( HeapTraceFile );
  {$ENDIF}

  pLibClass := TACBrLibSAT; // Ajusta a classe a ser criada
  MainThreadID := GetCurrentThreadId();
  Application.Initialize;
end.

