{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Elton Barbosa                                   }
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

unit ACBrLibNCMsStaticImportST;

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
  CACBrNCMsLIBName = 'ACBrNCMs64.dll';
  {$Else}
  CACBrNCMsLIBName = 'ACBrNCMs32.dll';
  {$EndIf}
 {$Else}
  {$IfDef CPU64}
  CACBrNCMsLIBName = 'libacbrncms64.so';
  {$Else}
  CACBrNCMsLIBName = 'libacbrncms32.so';
  {$EndIf}
 {$EndIf}

{$I ACBrLibErros.inc}

{%region Constructor/Destructor}
function NCM_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNCMsLIBName;

function NCM_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNCMsLIBName;
{%endregion}

{%region Versao/Retorno}
function NCM_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNCMsLIBName;

function NCM_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNCMsLIBName;

function NCM_OpenSSLInfo(const sOpenSSLInfo: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNCMsLIBName;

function NCM_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNCMsLIBName;
{%endregion}

{%region Ler/Gravar Config }
function NCM_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNCMsLIBName;

function NCM_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNCMsLIBName;

function NCM_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNCMsLIBName;

function NCM_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNCMsLIBName;
{%endregion}

{%region NCM}
//function NCM_BuscarPorCEP(eCEP: PChar; var Qtde: Integer;
//  const sResposta: PChar; var esTamanho: longint): longint;
//  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCEPLIBName;
//function NCM_BuscarPorLogradouro(eCidade, eTipo_Logradouro, eLogradouro, eUF,
//  eBairro: PChar; var Qtde: Integer; const sResposta: PChar;
//  var esTamanho: longint): longint;
//  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCEPLIBName;
{%endregion}

implementation

end.
