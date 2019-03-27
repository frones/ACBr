unit ACBrLibNFeStaticImport;

{$IfDef FPC}
{$mode objfpc}{$H+}
{$EndIf}

{.$Define STDCALL}

interface

uses
  Classes, SysUtils;

const
 {$IfDef MSWINDOWS}
  {$IfDef CPU64}
  CACBrNFeLIBName = 'ACBrNFe64.dll';
  {$Else}
  CACBrNFeLIBName = 'ACBrNFe32.dll';
  {$EndIf}
 {$Else}
  {$IfDef CPU64}
  CACBrNFeLIBName = 'ACBrNFe64.so';
  {$Else}
  CACBrNFeLIBName = 'ACBrNFe32.so';

  {$EndIf}
 {$EndIf}

{$I ACBrLibErros.inc}

{%region Constructor/Destructor}
function NFE_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;

function NFE_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;
{%endregion}

{%region Versao/Retorno}
function NFE_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;

function NFE_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;

function NFE_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;
{%endregion}

{%region Ler/Gravar Config }
function NFE_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;

function NFE_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;

function NFE_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;

function NFE_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;
{%endregion}

{%region NFe}
function NFe_CarregarXML(const eArquivoOuXML: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;

function NFe_CarregarINI(const eArquivoOuINI: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;

function NFE_CarregarEventoXML(const eArquivoOuXML: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;

function NFE_CarregarEventoINI(const eArquivoOuINI: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;

function NFe_LimparLista: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;

function NFE_LimparListaEventos: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;

function NFe_Assinar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;

function NFe_Validar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;

function NFe_ValidarRegrasdeNegocios(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;

function NFe_VerificarAssinatura(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;
{%endregion}

{%region Servicos}
function NFe_StatusServico(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;

function NFe_Consultar(const eChaveOuNFe: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;

function NFe_Inutilizar(const ACNPJ, AJustificativa: PChar;
  Ano, Modelo, Serie, NumeroInicial, NumeroFinal: integer;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;

function NFe_Enviar(ALote: Integer; Imprimir: Boolean;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;

function NFe_Cancelar(const eChave, eJustificativa, eCNPJ: PChar; ALote: Integer;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;

function NFe_EnviarEvento(idLote: Integer;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;

function NFe_DistribuicaoDFePorUltNSU(const AcUFAutor: integer; eCNPJCPF, eultNSU: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;

function NFe_DistribuicaoDFePorNSU(const AcUFAutor: integer; eCNPJCPF, eNSU: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;

function NFe_DistribuicaoDFePorChave(const AcUFAutor: integer; eCNPJCPF, echNFe: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;

function NFe_EnviarEmail(const ePara, eChaveNFe: PChar; const AEnviaPDF: Boolean;
  const eAssunto, eCC, eAnexos, eMensagem: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;

function NFe_EnviarEmailEvento(const ePara, eChaveEvento, eChaveNFe: PChar;
  const AEnviaPDF: Boolean; const eAssunto, eCC, eAnexos, eMensagem: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;

function NFe_Imprimir: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;

function NFE_Imprimir(const cImpressora: PChar; nNumCopias: Integer; const cProtocolo,
    bMostrarPreview, cMarcaDagua, bViaConsumidor, bSimplificado: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;

function NFe_ImprimirEvento(const eChaveNFe, eChaveEvento: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;

function NFe_ImprimirEventoPDF(const eChaveNFe, eChaveEvento: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;

function NFe_ImprimirInutilizacao(const eChave: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;

function NFe_ImprimirInutilizacaoPDF(const eChave: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;
{%endregion}


implementation

end.
