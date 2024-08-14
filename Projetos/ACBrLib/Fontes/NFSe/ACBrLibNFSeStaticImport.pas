{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
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
function NFSE_Inicializar(const eArqConfig, eChaveCrypt: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_Finalizar: Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;
{%endregion}

{%region Versao/Retorno}
function NFSE_Nome(const sNome: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_Versao(const sVersao: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_OpenSSLInfo(const sOpenSSLInfo: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_UltimoRetorno(const sMensagem: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;
{%endregion}

{%region Ler/Gravar Config }
function NFSE_ConfigLer(const eArqConfig: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConfigGravar(const eArqConfig: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConfigLerValor(const eSessao, eChave: PAnsiChar; sValor: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConfigGravarValor(const eSessao, eChave, eValor: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;
{%endregion}

{%region NFSe}
function NFSE_CarregarXML(const eArquivoOuXML: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_CarregarLoteXML(const eArquivoOuXML: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_CarregarINI(const eArquivoOuINI: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ObterXml(AIndex: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_GravarXml(AIndex: Integer; const eNomeArquivo, ePathArquivo: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ObterIni(AIndex: Integer; const sResposta: PAnsiChar; var esTamanho: Integer):Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_GravarIni(AIndex: Integer; const eNomeArquivo, ePathArquivo: PAnsiChar):Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_LimparLista: Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ObterCertificados(const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_Emitir(const aLote: PAnsiChar; aModoEnvio: Integer;  aImprimir: Boolean; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_Cancelar(aInfCancelamentoNFSe: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_SubstituirNFSe(const aNumeroNFSe, aSerieNFSe, aCodigoCancelamento, aMotivoCancelamento, aNumeroLote, aCodigoVerificacao, sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_LinkNFSe(aNumeroNFSe: PAnsiChar; const aCodigoVerificacao, aChaveAcesso, aValorServico, sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_GerarLote(const aLote: PAnsiChar; aQtdMaximaRps, aModoEnvio: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_GerarToken(const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarSituacao(const aProtocolo, aNumLote, sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarLoteRps(const aProtocolo, aNumLote, sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarNFSePorRps(const aNumeroRps, aSerie, aTipo, aCodigoVerificacao, sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarNFSePorNumero(const aNumero: PAnsiChar; aPagina: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarNFSePorPeriodo(aDataInicial, aDataFinal: TDateTime; aPagina: Integer; aNumeroLote: PAnsiChar; aTipoPeriodo: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarNFSePorFaixa(const aNumeroInicial, aNumeroFinal: PAnsiChar; aPagina: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarNFSeGenerico(aInfConsultaNFSe: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarLinkNFSe(aInfConsultaLinkNFSe: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_EnviarEmail(const ePara, eXmlNFSe: PAnsiChar; const AEnviaPDF: boolean; const eAssunto, eCC, eAnexos, eMensagem: PAnsiChar):Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_Imprimir(const cImpressora: PAnsiChar; nNumCopias: integer; const bGerarPDF, bMostrarPreview, cCancelada: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ImprimirPDF: Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_SalvarPDF(const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarNFSeServicoPrestadoPorNumero(const aNumero: PAnsiChar; aPagina: Integer; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarNFSeServicoPrestadoPorPeriodo(aDataInicial, aDataFinal: TDateTime; aPagina: Integer; aTipoPeriodo: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarNFSeServicoPrestadoPorTomador(const aCNPJ, aInscMun: PAnsiChar; aPagina: Integer; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarNFSeServicoPrestadoPorIntermediario(const aCNPJ, aInscMun: PAnsiChar; aPagina: Integer; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarNFSeServicoTomadoPorNumero(const aNumero: PAnsiChar; aPagina: Integer; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: Integer; const sResposta: PAnsiChar; var esTamanho: Integer):Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarNFSeServicoTomadoPorPrestador(const aCNPJ, aInscMun: PAnsiChar; aPagina: Integer; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarNFSeServicoTomadoPorTomador(const aCNPJ, aInscMun: PAnsiChar; aPagina: Integer; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarNFSeServicoTomadoPorPeriodo(aDataInicial, aDataFinal: TDateTime; aPagina: Integer; aTipoPeriodo: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarNFSeServicoTomadoPorIntermediario(const aCNPJ, aInscMun: PAnsiChar; aPagina: Integer; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_EnviarEvento(aInfEvento: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarDPSPorChave(const aChaveDPS: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarNFSePorChave(const aChaveNFSe: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarEvento(const aChave: PAnsiChar; aTipoEvento: Integer; aNumSeq: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarDFe(aNSU: Integer; sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ObterDANFSE(const aChaveNFSe: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ConsultarParametros(aTipoParametroMunicipio: Integer; const aCodigoServico: PAnsiChar; aCompetencia: TDateTime; aNumeroBeneficio: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

function NFSE_ObterInformacoesProvedor(const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFSeLIBName;

{%endregion}

implementation

end.
