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

unit ACBrLibeSocialStaticImportMT;

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
  CACBreSocialLIBName = 'ACBreSocial64.dll';
  {$Else}
  CACBreSocialLIBName = 'ACBreSocial32.dll';
  {$EndIf}
 {$Else}
  {$IfDef CPU64}
  CACBreSocialLIBName = 'ACBreSocial64.so';
  {$Else}
  CACBreSocialLIBName = 'ACBreSocial32.so';

  {$EndIf}
 {$EndIf}

{$I ACBrLibErros.inc}

{%region Constructor/Destructor}
function eSocial_Inicializar(var libHandle: TLibHandle; const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBreSocialLIBName;

function eSocial_Finalizar(const libHandle: TLibHandle): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBreSocialLIBName;
{%endregion}

{%region Versao/Retorno}
function eSocial_Nome(var libHandle: TLibHandle; const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBreSocialLIBName;

function eSocial_Versao(var libHandle: TLibHandle; const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBreSocialLIBName;

function eSocial_OpenSSLInfo(const libHandle: TLibHandle; const sOpenSSLInfo: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBreSocialLIBName;

function eSocial_UltimoRetorno(var libHandle: TLibHandle; const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBreSocialLIBName;
{%endregion}

{%region Ler/Gravar Config }
function eSocial_ConfigLer(var libHandle: TLibHandle; const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBreSocialLIBName;

function eSocial_ConfigGravar(var libHandle: TLibHandle; const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBreSocialLIBName;

function eSocial_ConfigLerValor(var libHandle: TLibHandle; const eSessao, eChave: PChar; sValor: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBreSocialLIBName;

function eSocial_ConfigGravarValor(var libHandle: TLibHandle; const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBreSocialLIBName;
{%endregion}

{%region eSocial}
function eSocial_CriarEventoeSocial(var libHandle: TLibHandle; const eArqIni: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBreSocialLIBName;

function eSocial_EnviareSocial(var libHandle: TLibHandle; const Agrupo: Integer; const sResposta: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBreSocialLIBName;

function eSocial_ConsultareSocial(var libHandle: TLibHandle; const eProtocolo, sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBreSocialLIBName;

function eSocial_CriarEnviareSocial(var libHandle: TLibHandle; const eArqIni: PChar; aGrupo:integer):longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBreSocialLIBName;

function eSocial_LimpareSocial(var libHandle: TLibHandle): Longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBreSocialLIBName;

function eSocial_CarregarXMLEventoeSocial (var libHandle: TLibHandle; const eArquivoOuXML: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBreSocialLIBName;

function eSocial_SetIDEmpregador (var libHandle: TLibHandle; const aIdEmpregador: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBreSocialLIBName;

function eSocial_SetIDTransmissor (var libHandle: TLibHandle; const aIdTransmissor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBreSocialLIBName;

function eSocial_SetTipoEmpregador (var libHandle: TLibHandle; aTipoEmpregador: integer):longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBreSocialLIBName;

function eSocial_SetVersaoDF (var libHandle: TLibHandle; const sVersao: PChar):longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBreSocialLIBName;

function eSocial_ConsultaIdentificadoresEventosEmpregador (var libHandle: TLibHandle; const aIdEmpregador: PChar; aTipoEvento: integer; aPeriodoApuracao: TDateTime; const sResposta: PChar; var esTamanho: longint):longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBreSocialLIBName;

function eSocial_ConsultaIdentificadoresEventosTabela (var libHandle: TLibHandle; const aIdEmpregador: PChar; aTipoEvento: integer; aChave: PChar; aDataInicial: TDateTime; aDataFinal: TDateTime; const sResposta: PChar; var esTamanho: longint):longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBreSocialLIBName;

function eSocial_ConsultaIdentificadoresEventosTrabalhador (var libHandle: TLibHandle; const aIdEmpregador: PChar; aCPFTrabalhador: PChar; aDataInicial:TDateTime; aDataFinal: TDateTime; const sResposta: PChar; var esTamanho: longint):longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBreSocialLIBName;

function eSocial_DownloadEventos (var libHandle: TLibHandle; const aIdEmpregador: PChar; aCPFTrabalhador: PChar; aDataInicial: TDateTime; aDataFinal: TDateTime; const sResposta: PChar; var esTamanho: longint):longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBreSocialLIBName;

function eSocial_ObterCertificados (var libHandle: TLibHandle; const sResposta: PChar; var esTamanho: longint):longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBreSocialLIBName;

function eSocial_Validar(const libHandle:longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBreSocialLIBName;

{%endregion}

implementation

end.
