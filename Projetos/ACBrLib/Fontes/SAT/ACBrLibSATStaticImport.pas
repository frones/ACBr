unit ACBrLibSATStaticImport;

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
  CACBrSATLIBName = 'ACBrSAT64.dll';
  {$Else}
  CACBrSATLIBName = 'ACBrSAT32.dll';
  {$EndIf}
 {$Else}
  {$IfDef CPU64}
  CACBrSATLIBName = 'libacbrsat64.so';
  {$Else}
  CACBrSATLIBName = 'libacbrsat32.so';

  {$EndIf}
 {$EndIf}

{$I ACBrLibErros.inc}

{%region Constructor/Destructor}
function SAT_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;
{%endregion}

{%region Versao/Retorno}
function SAT_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;
{%endregion}

{%region Ler/Gravar Config }
function SAT_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;
{%endregion}

{%region Ativar}
function SAT_InicializarSAT: longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_DesInicializar: longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;
{%endregion}

{%region Funções SAT}
function SAT_AssociarAssinatura(CNPJvalue, assinaturaCNPJs: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_BloquearSAT(const sResposta: PChar; var esTamanho: longint): longint;
        {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_DesbloquearSAT(const sResposta: PChar; var esTamanho: longint): longint;
        {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_TrocarCodigoDeAtivacao(codigoDeAtivacaoOuEmergencia: PChar;
  opcao: integer; novoCodigo: PChar; const sResposta: PChar; var esTamanho: longint):
  longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_ConsultarSAT(const sResposta: PChar; var esTamanho: longint): longint;
        {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_ConsultarStatusOperacional(const sResposta: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_ConsultarNumeroSessao(cNumeroDeSessao: integer;
  const sResposta: PChar; var esTamanho: longint): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_AtualizarSoftwareSAT(const sResposta: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_ComunicarCertificadoICPBRASIL(certificado: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_ExtrairLogs(eArquivo: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_TesteFimAFim(eArquivoXmlVenda: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;
{%endregion}

{%region CFe}
function SAT_CriarCFe(eArquivoIni: PChar; const sResposta: PChar;
  var esTamanho: longint): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_CriarEnviarCFe(eArquivoIni: PChar; const sResposta: PChar;
  var esTamanho: longint): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_EnviarCFe(eArquivoXml: PChar; const sResposta: PChar;
  var esTamanho: longint): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_CancelarCFe(eArquivoXml: PChar; const sResposta: PChar;
  var esTamanho: longint): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;
{%endregion}

{%region Impressão}
function SAT_ImprimirExtratoVenda(eArqXMLVenda, eNomeImpressora: PChar)
  : longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_ImprimirExtratoResumido(eArqXMLVenda, eNomeImpressora: PChar)
  : longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_ImprimirExtratoCancelamento(eArqXMLVenda, eArqXMLCancelamento, eNomeImpressora: PChar)
  : longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_GerarImpressaoFiscalMFe(eArqXMLVenda, eNomeImpressora: PChar; const sResposta: PChar;
  var esTamanho: longint): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_GerarPDFExtratoVenda(eArqXMLVenda, eNomeArquivo: PChar; const sResposta: PChar;
  var esTamanho: longint): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_EnviarEmail(eArqXMLVenda, sPara, sAssunto, eNomeArquivo, sMensagem,
  sCC, eAnexos: PChar): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;
{%endregion}

implementation

end.
