{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
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

unit ACBrLibSATStaticImportMT;

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
function SAT_Inicializar(var libHandle: TLibHandle; const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_Finalizar(const libHandle: TLibHandle): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;
{%endregion}

{%region Versao/Retorno}
function SAT_Nome(const libHandle: TLibHandle; const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_Versao(const libHandle: TLibHandle; const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_UltimoRetorno(const libHandle: TLibHandle; const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;
{%endregion}

{%region Ler/Gravar Config }
function SAT_ConfigLer(const libHandle: TLibHandle; const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_ConfigGravar(const libHandle: TLibHandle; const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_ConfigLerValor(const libHandle: TLibHandle; const eSessao, eChave: PChar; sValor: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_ConfigGravarValor(const libHandle: TLibHandle; const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;
{%endregion}

{%region Ativar}
function SAT_InicializarSAT (const libHandle: TLibHandle): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_DesInicializar (const libHandle: TLibHandle): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;
{%endregion}

{%region Funções SAT}
function SAT_AssociarAssinatura(const libHandle: TLibHandle; CNPJvalue, assinaturaCNPJs: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_BloquearSAT(const libHandle: TLibHandle; const sResposta: PChar; var esTamanho: longint): longint;
        {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_DesbloquearSAT(const libHandle: TLibHandle; const sResposta: PChar; var esTamanho: longint): longint;
        {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_TrocarCodigoDeAtivacao(const libHandle: TLibHandle; codigoDeAtivacaoOuEmergencia: PChar;
  opcao: integer; novoCodigo: PChar; const sResposta: PChar; var esTamanho: longint):
  longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_ConsultarSAT(const libHandle: TLibHandle; const sResposta: PChar; var esTamanho: longint): longint;
        {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_ConsultarUltimaSessaoFiscal(const libHandle: TLibHandle; const sResposta: PChar; var esTamanho: longint): longint;
         {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;


function SAT_ConsultarStatusOperacional(const libHandle: TLibHandle; const sResposta: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_ConsultarNumeroSessao(const libHandle: TLibHandle; cNumeroDeSessao: integer;
  const sResposta: PChar; var esTamanho: longint): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_SetNumeroSessao(const libHandle: TLibHandle; cNumeroDeSessao: PChar): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_AtualizarSoftwareSAT(const libHandle: TLibHandle; const sResposta: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_ComunicarCertificadoICPBRASIL(const libHandle: TLibHandle; certificado: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_ExtrairLogs(const libHandle: TLibHandle; eArquivo: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_TesteFimAFim(const libHandle: TLibHandle; eArquivoXmlVenda: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;
{%endregion}

{%region CFe}
function SAT_CriarCFe(const libHandle: TLibHandle; eArquivoIni: PChar; const sResposta: PChar;
  var esTamanho: longint): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_CriarEnviarCFe(const libHandle: TLibHandle; eArquivoIni: PChar; const sResposta: PChar;
  var esTamanho: longint): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_ValidarCFe(const libHandle: TLibHandle; eArquivoXml: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_EnviarCFe(const libHandle: TLibHandle; eArquivoXml: PChar; const sResposta: PChar;
  var esTamanho: longint): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_CancelarCFe(const libHandle: TLibHandle; eArquivoXml: PChar; const sResposta: PChar;
  var esTamanho: longint): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;
{%endregion}

{%region Impressão}
function SAT_ImprimirExtratoVenda(const libHandle: TLibHandle; eArqXMLVenda, eNomeImpressora: PChar)
  : longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_ImprimirExtratoResumido(const libHandle: TLibHandle; eArqXMLVenda, eNomeImpressora: PChar)
  : longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_ImprimirExtratoCancelamento(const libHandle: TLibHandle; eArqXMLVenda, eArqXMLCancelamento, eNomeImpressora: PChar)
  : longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_SalvarPDF(const libHandle: TLibHandle; const sResposta: PChar; var esTamanho: longint)
  : longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_GerarImpressaoFiscalMFe(const libHandle: TLibHandle; eArqXMLVenda, eNomeImpressora: PChar; const sResposta: PChar;
  var esTamanho: longint): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_GerarPDFExtratoVenda(const libHandle: TLibHandle; eArqXMLVenda, eNomeArquivo: PChar; const sResposta: PChar;
  var esTamanho: longint): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;

function SAT_EnviarEmail(const libHandle: TLibHandle; eArqXMLVenda, sPara, sAssunto, eNomeArquivo, sMensagem,
  sCC, eAnexos: PChar): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrSATLIBName;
{%endregion}

implementation

end.

