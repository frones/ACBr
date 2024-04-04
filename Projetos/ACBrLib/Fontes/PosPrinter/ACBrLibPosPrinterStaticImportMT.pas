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

unit ACBrLibPosPrinterStaticImportMT;

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
  CACBrPosPrinterLIBName = 'ACBrPosPrinter64.dll';
  {$Else}
  CACBrPosPrinterLIBName = 'ACBrPosPrinter32.dll';
  {$EndIf}
 {$Else}
  {$IfDef CPU64}
  CACBrPosPrinterLIBName = 'libacbrposprinter64.so';
  {$Else}
  CACBrPosPrinterLIBName = 'libacbrposprinter32.so';
  {$EndIf}
 {$EndIf}

{$I ACBrLibErros.inc}

{%region Constructor/Destructor}
function POS_Inicializar(var libHandle: TLibHandle; const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_Finalizar(const libHandle: TLibHandle): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;
{%endregion}

{%region Versao/Retorno}
function POS_Nome(const libHandle: TLibHandle; const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_Versao(const libHandle: TLibHandle; const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_OpenSSLInfo(const libHandle: TLibHandle; const sOpenSSLInfo: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_UltimoRetorno(const libHandle: TLibHandle; const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;
{%endregion}

{%region Ler/Gravar Config }
function POS_ConfigLer(const libHandle: TLibHandle; const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_ConfigGravar(const libHandle: TLibHandle; const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_ConfigLerValor(const libHandle: TLibHandle; const eSessao, eChave: PChar; sValor: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_ConfigGravarValor(const libHandle: TLibHandle; const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;
{%endregion}

{%region Ativar}
function POS_Ativar(const libHandle: TLibHandle): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_Desativar(const libHandle: TLibHandle): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;
{%endregion}

{%region Impressão}
function POS_Imprimir(const libHandle: TLibHandle; eString: PChar; PulaLinha, DecodificarTags, CodificarPagina: Boolean; Copias: Integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_ImprimirLinha(const libHandle: TLibHandle; eString: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_ImprimirCmd(const libHandle: TLibHandle; eComando: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_ImprimirTags(const libHandle: TLibHandle): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;
{%endregion}

{%region Diversos}
function POS_TxRx(const libHandle: TLibHandle; eCmd: PChar; BytesToRead: Byte; ATimeOut: Integer; WaitForTerminator: Boolean;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_Zerar(const libHandle: TLibHandle): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_InicializarPos(const libHandle: TLibHandle): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_Reset(const libHandle: TLibHandle): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_PularLinhas(const libHandle: TLibHandle; NumLinhas: Integer): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_CortarPapel(const libHandle: TLibHandle; Parcial: Boolean): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_AbrirGaveta(const libHandle: TLibHandle): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_LerInfoImpressora(const libHandle: TLibHandle; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_LerStatusImpressora(const libHandle: TLibHandle; Tentativas: Integer; var status: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_RetornarTags(const libHandle: TLibHandle; IncluiAjuda: Boolean; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;
{%endregion}


implementation

end.

