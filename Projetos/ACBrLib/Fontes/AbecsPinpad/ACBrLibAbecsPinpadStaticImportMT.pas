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

unit ACBrLibAbecsPinpadStaticImportMT;

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
  CACBrAbecsPinpadLIBName = 'ACBrAbecsPinpad64.dll';
  {$Else}
  CACBrAbecsPinpadLIBName = 'ACBrAbecsPinpad32.dll';
  {$EndIf}
 {$Else}
  {$IfDef CPU64}
  CACBrAbecsPinpadLIBName = 'ACBrAbecsPinpad64.so';
  {$Else}
  CACBrAbecsPinpadLIBName = 'ACBrAbecsPinpad32.so';

  {$EndIf}
 {$EndIf}

{$I ACBrLibErros.inc}

{%region Constructor/Destructor}
function AbecsPinpad_Inicializar(var libHandle: TLibHandle; const eArqConfig, eChaveCrypt: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrAbecsPinpadLIBName;

function AbecsPinpad_Finalizar(const libHandle: TLibHandle): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrAbecsPinpadLIBName;
{%endregion}

{%region Versao/Retorno}
function AbecsPinpad_Nome(const libHandle: TLibHandle; const sNome: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrAbecsPinpadLIBName;

function AbecsPinpad_Versao(const libHandle: TLibHandle; const sVersao: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrAbecsPinpadLIBName;

function AbecsPinpad_OpenSSLInfo(const libHandle: TLibHandle; const sOpenSSLInfo: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrAbecsPinpadLIBName;

function AbecsPinpad_UltimoRetorno(const libHandle: TLibHandle; const sMensagem: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrAbecsPinpadLIBName;
{%endregion}

{%region Ler/Gravar Config }
function AbecsPinpad_ConfigImportar(const libHandle: TLibHandle; const eArqConfig: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrAbecsPinpadLIBName;

function AbecsPinpad_ConfigExportar(const libHandle: TLibHandle; const sMensagem: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrAbecsPinpadLIBName;

function AbecsPinpad_ConfigLer(const libHandle: TLibHandle; const eArqConfig: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrAbecsPinpadLIBName;

function AbecsPinpad_ConfigGravar(const libHandle: TLibHandle; const eArqConfig: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrAbecsPinpadLIBName;

function AbecsPinpad_ConfigLerValor(const libHandle: TLibHandle; const eSessao, eChave: PAnsiChar; sValor: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrAbecsPinpadLIBName;

function AbecsPinpad_ConfigGravarValor(const libHandle: TLibHandle; const eSessao, eChave, eValor: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrAbecsPinpadLIBName;
{%endregion}

{%region AbecsPinpad}
function AbecsPinpad_Ativar(const libHandle: TLibHandle): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrAbecsPinpadLIBName;

function AbecsPinpad_Desativar(const libHandle: TLibHandle): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrAbecsPinpadLIBName;

function AbecsPinpad_OPN(const libHandle: TLibHandle): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrAbecsPinpadLIBName;

function AbecsPinpad_CLO(const libHandle: TLibHandle; const sMensagem: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrAbecsPinpadLIBName;

function AbecsPinpad_CLX(const libHandle: TLibHandle; const sMensagemOuNomeImagem: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrAbecsPinpadLIBName;

function AbecsPinpad_GIX(const libHandle: TLibHandle; const PP_DATA: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrAbecsPinpadLIBName;

function AbecsPinpad_GIN(const libHandle: TLibHandle; const GIN_ACQIDX: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrAbecsPinpadLIBName;

function AbecsPinpad_PinPadCapabilities(const libHandle: TLibHandle; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrAbecsPinpadLIBName;

function AbecsPinpad_DSP(const libHandle: TLibHandle; const sMensagem: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrAbecsPinpadLIBName;

function AbecsPinpad_DEX(const libHandle: TLibHandle; const sMensagem: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrAbecsPinpadLIBName;

function AbecsPinpad_GKY(const libHandle: TLibHandle): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrAbecsPinpadLIBName;

function AbecsPinpad_RMC(const libHandle: TLibHandle; const sMensagemRMC: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrAbecsPinpadLIBName;

function AbecsPinpad_GCD(const libHandle: TLibHandle; aMSGIDX: Integer; aTimeOut: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrAbecsPinpadLIBName;

function AbecsPinpad_CEX(const libHandle: TLibHandle; VerifyKey: Boolean; VerifyMagnetic: Boolean; VerifyICCInsertion: Boolean; VerifyICCRemoval: Boolean; VerifyCTLSPresence: Boolean; aTimeOut: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrAbecsPinpadLIBName;

function AbecsPinpad_MNU(const libHandle: TLibHandle; const sMNUOPT: PAnsiChar; sDSPMSG: PAnsiChar; aTimeOut: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrAbecsPinpadLIBName;

function AbecsPinpad_LoadMedia(const libHandle: TLibHandle; const sCaminhoImagem: PAnsiChar; aTipoImagem: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrAbecsPinpadLIBName;

function AbecsPinpad_LMF(const libHandle: TLibHandle; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrAbecsPinpadLIBName;

function AbecsPinpad_DSI(const libHandle: TLibHandle; const sNomeArquivo: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrAbecsPinpadLIBName;

function AbecsPinpad_DMF(const libHandle: TLibHandle; const sNomeArquivo: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrAbecsPinpadLIBName;
{%endregion}

implementation

end.

