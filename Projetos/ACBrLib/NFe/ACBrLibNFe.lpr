{$I ACBr.inc}

library ACBrLibNFe;

uses
  Interfaces, sysutils, Classes, Forms,
  ACBrLibNFeClass, ACBrLibConfig, ACBrLibComum,
  ACBrLibConsts, ACBrLibNFeConfig, ACBrLibResposta, ACBrLibNFeRespostas;

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
  NFE_InutilizarNFe,
  NFE_ConsultarNFe,

  // Arquivos
  NFE_CarregarXMLNFe,
  NFE_CarregarININFe,
  NFE_LimparListaNFEs,
  NFE_ValidarNFEs,
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


