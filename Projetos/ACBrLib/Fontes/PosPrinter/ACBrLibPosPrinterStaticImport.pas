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

unit ACBrLibPosPrinterStaticImport;

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
function POS_Inicializar(const eArqConfig, eChaveCrypt: PAnsiChar): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_Finalizar: integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;
{%endregion}

{%region Versao/Retorno}
function POS_Nome(const sNome: PAnsiChar; var esTamanho: integer): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_Versao(const sVersao: PAnsiChar; var esTamanho: integer): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_OpenSSLInfo(const sOpenSSLInfo: PAnsiChar; var esTamanho: integer): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_UltimoRetorno(const sMensagem: PAnsiChar; var esTamanho: integer): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;
{%endregion}

{%region Ler/Gravar Config }
function POS_ConfigLer(const eArqConfig: PAnsiChar): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_ConfigGravar(const eArqConfig: PAnsiChar): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_ConfigLerValor(const eSessao, eChave: PAnsiChar; sValor: PAnsiChar; var esTamanho: integer): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_ConfigGravarValor(const eSessao, eChave, eValor: PAnsiChar): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;
{%endregion}

{%region Ativar}
function POS_Ativar: integer;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_Desativar: integer;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;
{%endregion}

{%region Impressão}
function POS_Imprimir(eString: PAnsiChar; PulaLinha, DecodificarTags, CodificarPagina: Boolean; Copias: Integer): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_ImprimirLinha(eString: PAnsiChar): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_ImprimirCmd(eComando: PAnsiChar): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_ImprimirTags: integer;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;
{%endregion}

{%region Diversos}
function POS_TxRx(eCmd: PAnsiChar; BytesToRead: Byte; ATimeOut: Integer; WaitForTerminator: Boolean;
  const sResposta: PAnsiChar; var esTamanho: integer): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_Zerar: integer;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_InicializarPos: integer;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_Reset: integer;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_PularLinhas(NumLinhas: Integer): integer;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_CortarPapel(Parcial: Boolean): integer;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_AbrirGaveta: integer;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_LerInfoImpressora(const sResposta: PAnsiChar; var esTamanho: integer): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_LerStatusImpressora(Tentativas: Integer; var status: integer): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;

function POS_RetornarTags(IncluiAjuda: Boolean; const sResposta: PAnsiChar; var esTamanho: integer): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPosPrinterLIBName;
{%endregion}


implementation

end.
