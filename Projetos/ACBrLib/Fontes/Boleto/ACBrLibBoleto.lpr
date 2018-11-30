{$I ACBr.inc}

library ACBrLibBoleto;

uses
  Interfaces, Forms,
  sysutils, Classes, ACBrLibBoletoDataModule,
  ACBrLibConfig, ACBrLibResposta,
  ACBrLibComum, ACBrLibConsts, ACBrLibBoletoClass, ACBrLibBoletoConsts, ACBrLibBoletoConfig;

//{$R *.res}

{$IFDEF DEBUG}
var
   HeapTraceFile : String ;
{$ENDIF}

exports
  Boleto_Inicializar,
  Boleto_Finalizar,
  Boleto_Nome,
  Boleto_Versao,
  Boleto_UltimoRetorno,
  Boleto_ConfigLer,
  Boleto_ConfigGravar,
  Boleto_ConfigLerValor,
  Boleto_ConfigGravarValor,

  Boleto_ConfigurarDados,
  Boleto_IncluirTitulos,
  Boleto_LimparLista,
  Boleto_TotalTitulosLista,
  Boleto_Imprimir,
  Boleto_GerarPDF,
  Boleto_GerarHTML,
  Boleto_GerarRemessa,
  Boleto_LerRetorno,
  Boleto_EnviarEmail,
  Boleto_SetDiretorioArquivo,
  Boleto_ListaBancos,
  Boleto_ListaCaractTitulo,
  Boleto_ListaOcorrencias,
  Boleto_ListaOcorrenciasEX,
  Boleto_TamNossoNumero,
  Boleto_CodigosMoraAceitos,
  Boleto_SelecionaBanco,
  Boleto_MontarNossoNumero,
  Boleto_RetornaLinhaDigitavel,
  Boleto_RetornaCodigoBarras;

begin
  {$IFDEF DEBUG}
   HeapTraceFile := ExtractFilePath(ParamStr(0))+ 'heaptrclog.trc' ;
   DeleteFile( HeapTraceFile );
   SetHeapTraceOutput( HeapTraceFile );
  {$ENDIF}

  pLibClass := TACBrLibBoleto; // Ajusta a classe a ser criada
  MainThreadID := GetCurrentThreadId();
  Application.Initialize;

end.

