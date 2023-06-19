{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Antonio Carlos Junior                           }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

unit ACBrLibNFSeStaticImportMT;

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
  CACBrNFSeLIBName = 'ACBrNFSe64.dll';
  {$Else}
  CACBrNFSeLIBName = 'ACBrNFSe32.dll';
  {$EndIf}
 {$Else}
  {$IfDef CPU64}
  CACBrNFSeLIBName = 'ACBrNFSe64.so';
  {$Else}
  CACBrNFSeLIBName = 'ACBrNFSe32.so';

  {$EndIf}
 {$EndIf}

{$I ACBrLibErros.inc}

{%region Constructor/Destructor}
function NFSE_Inicializar(var libHandle: TLibHandle; const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_Finalizar(const libHandle: TLibHandle): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;
{%endregion}

{%region Versao/Retorno}
function NFSE_Nome(const libHandle: TLibHandle; const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_Versao(const libHandle: TLibHandle; const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_UltimoRetorno(const libHandle: TLibHandle; const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;
{%endregion}

{%region Ler/Gravar Config }
function NFSE_ConfigLer(const libHandle: TLibHandle; const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConfigGravar(const libHandle: TLibHandle; const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConfigLerValor(const libHandle: TLibHandle; const eSessao, eChave: PChar; sValor: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConfigGravarValor(const libHandle: TLibHandle; const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;
{%endregion}

{%region NFSe}
function NFSE_CarregarXML(const libHandle: TLibHandle; const eArquivoOuXML: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_CarregarINI(const libHandle: TLibHandle; const eArquivoOuINI: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ObterXml(const libHandle: TLibHandle; AIndex: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_GravarXml(const libHandle: TLibHandle; AIndex: longint; const eNomeArquivo, ePathArquivo: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ObterIni(const libHandle: TLibHandle; AIndex: longint; const sResposta: PChar; var esTamanho: longint):longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_GravarIni(const libHandle: TLibHandle; AIndex: longint; const eNomeArquivo, ePathArquivo: PChar):longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_LimparLista(const libHandle: TLibHandle): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ObterCertificados(const libHandle: TLibHandle; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_Emitir(const libHandle: TLibHandle; const aLote: PChar; aModoEnvio: longint;  aImprimir: Boolean; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_Cancelar(const libHandle: TLibHandle; aInfCancelamentoNFSe: PChar; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_SubstituirNFSe(const libHandle: TLibHandle; const aNumeroNFSe, aSerieNFSe, aCodigoCancelamento, aMotivoCancelamento, aNumeroLote, aCodigoVerificacao, sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_LinkNFSe(const libHandle: TLibHandle; aNumeroNFSe: PChar; const aCodigoVerificacao, aChaveAcesso, aValorServico, sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_GerarLote(const libHandle: TLibHandle; const aLote: PChar; aQtdMaximaRps, aModoEnvio: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_GerarToken(const libHandle: TLibHandle; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarSituacao(const libHandle: TLibHandle; const aProtocolo, aNumLote, sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarLoteRps(const libHandle: TLibHandle; const aProtocolo, aNumLote, sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarNFSePorRps(const libHandle: TLibHandle; const aNumeroRps, aSerie, aTipo, aCodigoVerificacao, sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarNFSePorNumero(const libHandle: TLibHandle; const aNumero: PChar; aPagina: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarNFSePorPeriodo(const libHandle: TLibHandle; aDataInicial, aDataFinal: TDateTime; aPagina: integer; aNumeroLote: PChar; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarNFSePorFaixa(const libHandle: TLibHandle; const aNumeroInicial, aNumeroFinal: PChar; aPagina: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarNFSeGenerico(const libHandle: TLibHandle; aInfConsultaNFSe: PChar; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_EnviarEmail(const libHandle: TLibHandle; const ePara, eXmlNFSe: PChar; const AEnviaPDF: boolean; const eAssunto, eCC, eAnexos, eMensagem: PChar):longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_Imprimir(const libHandle: TLibHandle; const cImpressora: PChar; nNumCopias: integer; const bGerarPDF, bMostrarPreview, cCancelada: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ImprimirPDF(const libHandle: TLibHandle): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarNFSeServicoPrestadoPorNumero(const libHandle: TLibHandle; const aNumero: PChar; aPagina: longint; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarNFSeServicoPrestadoPorPeriodo(const libHandle: TLibHandle; aDataInicial, aDataFinal: TDateTime; aPagina: longint; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarNFSeServicoPrestadoPorTomador(const libHandle: TLibHandle; const aCNPJ, aInscMun: PChar; aPagina: longint; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarNFSeServicoPrestadoPorIntermediario(const libHandle: TLibHandle; const aCNPJ, aInscMun: PChar; aPagina: longint; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarNFSeServicoTomadoPorNumero(const libHandle: TLibHandle; const aNumero: PChar; aPagina: longint; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint):longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarNFSeServicoTomadoPorPrestador(const libHandle: TLibHandle; const aCNPJ, aInscMun: PChar; aPagina: longint; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarNFSeServicoTomadoPorTomador(const libHandle: TLibHandle; const aCNPJ, aInscMun: PChar; aPagina: longint; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarNFSeServicoTomadoPorPeriodo(const libHandle: TLibHandle; aDataInicial, aDataFinal: TDateTime; aPagina: longint; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarNFSeServicoTomadoPorIntermediario(const libHandle: TLibHandle; const aCNPJ, aInscMun: PChar; aPagina: longint; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_EnviarEvento(const libHandle: TLibHandle; aInfEvento: PChar; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarDPSPorChave(const libHandle: TLibHandle; const aChaveDPS: PChar; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarNFSePorChave(const libHandle: TLibHandle; const aChaveNFSe: PChar; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarEvento(const libHandle: TLibHandle; const aChave: PChar; aTipoEvento: longint; aNumSeq: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarDFe(const libHandle: TLibHandle; aNSU: longint; sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ObterDANFSE(const libHandle: TLibHandle; const aChaveNFSe: PChar; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarParametros(const libHandle: TLibHandle; aTipoParametroMunicipio: longint; const aCodigoServico: PChar; aCompetencia: TDateTime; aNumeroBeneficio: PChar; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

{%endregion}

implementation

end.
