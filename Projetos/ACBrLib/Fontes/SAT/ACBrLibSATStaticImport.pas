{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Rafael Teno Dias                                }
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
function SAT_Inicializar(const eArqConfig, eChaveCrypt: PAnsiChar): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_Finalizar: integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;
{%endregion}

{%region Versao/Retorno}
function SAT_Nome(const sNome: PAnsiChar; var esTamanho: integer): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_Versao(const sVersao: PAnsiChar; var esTamanho: integer): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_OpenSSLInfo(const sOpenSSLInfo: PAnsiChar; var esTamanho: integer): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_UltimoRetorno(const sMensagem: PAnsiChar; var esTamanho: integer): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;
{%endregion}

{%region Ler/Gravar Config }
function SAT_ConfigLer(const eArqConfig: PAnsiChar): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_ConfigGravar(const eArqConfig: PAnsiChar): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_ConfigLerValor(const eSessao, eChave: PAnsiChar; sValor: PAnsiChar; var esTamanho: integer): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_ConfigGravarValor(const eSessao, eChave, eValor: PAnsiChar): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;
{%endregion}

{%region Ativar}
function SAT_InicializarSAT: integer;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_DesInicializar: integer;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;
{%endregion}

{%region Funções SAT}
function SAT_AssociarAssinatura(CNPJvalue, assinaturaCNPJs: PAnsiChar;
  const sResposta: PAnsiChar; var esTamanho: integer): integer;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_BloquearSAT(const sResposta: PAnsiChar; var esTamanho: integer): integer;
        {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_DesbloquearSAT(const sResposta: PAnsiChar; var esTamanho: integer): integer;
        {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_TrocarCodigoDeAtivacao(codigoDeAtivacaoOuEmergencia: PAnsiChar;
  opcao: integer; novoCodigo: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: integer):
  integer; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_ConsultarSAT(const sResposta: PAnsiChar; var esTamanho: integer): integer;
        {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_ConsultarUltimaSessaoFiscal(const sResposta: PAnsiChar; var esTamanho: integer): integer;
        {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_ConsultarStatusOperacional(const sResposta: PAnsiChar;
  var esTamanho: integer): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_ConsultarNumeroSessao(cNumeroDeSessao: integer;
  const sResposta: PAnsiChar; var esTamanho: integer): integer;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_SetNumeroSessao(cNumeroDeSessao: PAnsiChar): integer;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_AtualizarSoftwareSAT(const sResposta: PAnsiChar;
  var esTamanho: integer): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_ComunicarCertificadoICPBRASIL(certificado: PAnsiChar;
  const sResposta: PAnsiChar; var esTamanho: integer): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_ExtrairLogs(eArquivo: PAnsiChar): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_TesteFimAFim(eArquivoXmlVenda: PAnsiChar): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;
{%endregion}

{%region CFe}
function SAT_CriarCFe(eArquivoIni: PAnsiChar; const sResposta: PAnsiChar;
  var esTamanho: integer): integer;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_CriarEnviarCFe(eArquivoIni: PAnsiChar; const sResposta: PAnsiChar;
  var esTamanho: integer): integer;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_ValidarCFe(eArquivoXml: PAnsiChar): integer;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_CarregarXML(eArquivo: PAnsiChar): integer;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_ObterIni(const sResposta: PAnsiChar; var esTamanho: integer): integer;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_EnviarCFe(eArquivoXml: PAnsiChar; const sResposta: PAnsiChar;
  var esTamanho: integer): integer;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_CancelarCFe(eArquivoXml: PAnsiChar; const sResposta: PAnsiChar;
  var esTamanho: integer): integer;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;
{%endregion}

{%region Impressão}
function SAT_ImprimirExtratoVenda(eArqXMLVenda, eNomeImpressora: PAnsiChar)
  : integer;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_ImprimirExtratoResumido(eArqXMLVenda, eNomeImpressora: PAnsiChar)
  : integer;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_ImprimirExtratoCancelamento(eArqXMLVenda, eArqXMLCancelamento, eNomeImpressora: PAnsiChar)
  : integer;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_SalvarPDF(const sResposta: PAnsiChar; var esTamanho: integer)
  : integer;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_GerarImpressaoFiscalMFe(eArqXMLVenda, eNomeImpressora: PAnsiChar; const sResposta: PAnsiChar;
  var esTamanho: integer): integer;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_GerarPDFExtratoVenda(eArqXMLVenda, eNomeArquivo: PAnsiChar; const sResposta: PAnsiChar;
  var esTamanho: integer): integer;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_EnviarEmail(eArqXMLVenda, sPara, sAssunto, eNomeArquivo, sMensagem,
  sCC, eAnexos: PAnsiChar): integer;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;
{%endregion}

implementation

end.
