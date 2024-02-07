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

unit ACBrLibNFSeStaticImport;

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
function NFSE_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;
{%endregion}

{%region Versao/Retorno}
function NFSE_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;
{%endregion}

{%region Ler/Gravar Config }
function NFSE_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;
{%endregion}

{%region NFSe}
function NFSE_CarregarXML(const eArquivoOuXML: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_CarregarINI(const eArquivoOuINI: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ObterXml(AIndex: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_GravarXml(AIndex: longint; const eNomeArquivo, ePathArquivo: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ObterIni(AIndex: longint; const sResposta: PChar; var esTamanho: longint):longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_GravarIni(AIndex: longint; const eNomeArquivo, ePathArquivo: PChar):longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_LimparLista: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ObterCertificados(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_Emitir(const aLote: PChar; aModoEnvio: longint;  aImprimir: Boolean; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_Cancelar(aInfCancelamentoNFSe: PChar; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_SubstituirNFSe(const aNumeroNFSe, aSerieNFSe, aCodigoCancelamento, aMotivoCancelamento, aNumeroLote, aCodigoVerificacao, sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_LinkNFSe(aNumeroNFSe: PChar; const aCodigoVerificacao, aChaveAcesso, aValorServico, sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_GerarLote(const aLote: PChar; aQtdMaximaRps, aModoEnvio: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_GerarToken(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarSituacao(const aProtocolo, aNumLote, sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarLoteRps(const aProtocolo, aNumLote, sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarNFSePorRps(const aNumeroRps, aSerie, aTipo, aCodigoVerificacao, sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarNFSePorNumero(const aNumero: PChar; aPagina: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarNFSePorPeriodo(aDataInicial, aDataFinal: TDateTime; aPagina: longint; aNumeroLote: PChar; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarNFSePorFaixa(const aNumeroInicial, aNumeroFinal: PChar; aPagina: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarNFSeGenerico(aInfConsultaNFSe: PChar; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_EnviarEmail(const ePara, eXmlNFSe: PChar; const AEnviaPDF: boolean; const eAssunto, eCC, eAnexos, eMensagem: PChar):longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_Imprimir(const cImpressora: PChar; nNumCopias: integer; const bGerarPDF, bMostrarPreview, cCancelada: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ImprimirPDF: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_SalvarPDF(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarNFSeServicoPrestadoPorNumero(const aNumero: PChar; aPagina: longint; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarNFSeServicoPrestadoPorPeriodo(aDataInicial, aDataFinal: TDateTime; aPagina: longint; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarNFSeServicoPrestadoPorTomador(const aCNPJ, aInscMun: PChar; aPagina: longint; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarNFSeServicoPrestadoPorIntermediario(const aCNPJ, aInscMun: PChar; aPagina: longint; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarNFSeServicoTomadoPorNumero(const aNumero: PChar; aPagina: longint; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint):longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarNFSeServicoTomadoPorPrestador(const aCNPJ, aInscMun: PChar; aPagina: longint; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarNFSeServicoTomadoPorTomador(const aCNPJ, aInscMun: PChar; aPagina: longint; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarNFSeServicoTomadoPorPeriodo(aDataInicial, aDataFinal: TDateTime; aPagina: longint; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarNFSeServicoTomadoPorIntermediario(const aCNPJ, aInscMun: PChar; aPagina: longint; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_EnviarEvento(aInfEvento: PChar; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarDPSPorChave(const aChaveDPS: PChar; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarNFSePorChave(const aChaveNFSe: PChar; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarEvento(const aChave: PChar; aTipoEvento: longint; aNumSeq: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarDFe(aNSU: longint; sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ObterDANFSE(const aChaveNFSe: PChar; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarParametros(aTipoParametroMunicipio: longint; const aCodigoServico: PChar; aCompetencia: TDateTime; aNumeroBeneficio: PChar; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ObterInformacoesProvedor(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

{%endregion}

implementation

end.
