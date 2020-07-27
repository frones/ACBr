{$I ACBr.inc}

library ACBrLibPosPrinter;

uses
  Interfaces, printer4lazarus, sysutils, Classes,
  ACBrLibConfig, ACBrLibComum,
  {$IFDEF MT}ACBrLibPosPrinterMT{$ELSE}ACBrLibPosPrinterST{$ENDIF},
  ACBrLibPosPrinterConfig, ACBrLibPosPrinterDataModule;

{$R *.res}

{$IFDEF DEBUG}
var
   HeapTraceFile : String ;
{$ENDIF}

exports
  // Importadas de ACBrLibComum
  POS_Inicializar,
  POS_Finalizar,
  POS_Inicializada,
  POS_Nome,
  POS_Versao,
  POS_UltimoRetorno,
  POS_ImportarConfig,
  POS_ConfigLer,
  POS_ConfigGravar,
  POS_ConfigLerValor,
  POS_ConfigGravarValor,

  //Ativar
  POS_Ativar,
  POS_Desativar,

  //Comandos de impressão
  POS_Imprimir,
  POS_ImprimirLinha,
  POS_ImprimirCmd,
  POS_ImprimirTags,
  POS_ImprimirImagemArquivo,
  POS_ImprimirLogo,
  POS_ImprimirCheque,
  POS_ImprimirTextoCheque,

  //Diversos
  POS_TxRx,
  POS_Zerar,
  POS_InicializarPos,
  POS_Reset,
  POS_PularLinhas,
  POS_CortarPapel,
  POS_AbrirGaveta,
  POS_LerInfoImpressora,
  POS_LerStatusImpressora,
  POS_RetornarTags,
  POS_AcharPortas,
  POS_GravarLogoArquivo,
  POS_ApagarLogo,
  POS_LeituraCheque,
  POS_LerCMC7,
  POS_EjetarCheque,
  POS_PodeLerDaPorta,
  POS_LerCaracteristicas;

begin
  {$IFDEF DEBUG}
   HeapTraceFile := ExtractFilePath(ParamStr(0))+ 'heaptrclog.trc' ;
   DeleteFile( HeapTraceFile );
   SetHeapTraceOutput( HeapTraceFile );
  {$ENDIF}

  MainThreadID := GetCurrentThreadId();
end.

