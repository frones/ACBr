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
  MDFe_Inicializar,
  MDFe_Finalizar,
  MDFe_Nome,
  MDFe_Versao,
  MDFe_UltimoRetorno,
  MDFe_ConfigLer,
  MDFe_ConfigGravar,
  MDFe_ConfigLerValor,
  MDFe_ConfigGravarValor,

  // Servicos
  MDFe_StatusServico,
  MDFe_Enviar,
  MDFe_Consultar,
  MDFe_Cancelar,
  MDFe_EnviarEvento,
  MDFe_DistribuicaoDFePorUltNSU,
  MDFe_DistribuicaoDFePorNSU,
  MDFe_DistribuicaoDFePorChave,
  MDFe_EnviarEmail,
  MDFe_EnviarEmailEvento,
  MDFe_Imprimir,
  MDFe_ImprimirPDF,
  MDFe_ImprimirEvento,
  MDFe_ImprimirEventoPDF,

  // Arquivos
  MDFe_CarregarXML,
  MDFe_CarregarINI,
  MDFe_LimparLista,
  MDFe_Assinar,
  MDFe_Validar,
  MDFe_ValidarRegrasdeNegocios,
  MDFe_VerificarAssinatura;

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


