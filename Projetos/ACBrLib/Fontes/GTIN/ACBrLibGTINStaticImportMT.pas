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

unit ACBrLibGTINStaticImportMT;

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
  CACBrGTINLIBName = 'ACBrGTIN64.dll';
  {$Else}
  CACBrGTINLIBName = 'ACBrGTIN32.dll';
  {$EndIf}
 {$Else}
  {$IfDef CPU64}
  CACBrGTINLIBName = 'ACBrGTIN64.so';
  {$Else}
  CACBrGTINLIBName = 'ACBrGTIN32.so';

  {$EndIf}
 {$EndIf}

 {$I ACBrLibErros.inc}

 {%region Constructor/Destructor}
 function GTIN_Inicializar(var libHandle: TLibHandle; const eArqConfig, eChaveCrypt: PChar): longint;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGTINLIBName;

 function GTIN_Finalizar (const libHandle: TLibHandle): longint;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGTINLIBName;
 {%endregion}

 {%region Versao/Retorno}
 function GTIN_Nome(var libHandle: TLibHandle; const sNome: PChar; var esTamanho: longint): longint;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGTINLIBName;

 function GTIN_Versao(var libHandle: TLibHandle; const sVersao: PChar; var esTamanho: longint): longint;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGTINLIBName;

 function GTIN_OpenSSLInfo(const libHandle: TLibHandle; const sOpenSSLInfo: PChar; var esTamanho: longint): longint;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGTINLIBName;

 function GTIN_UltimoRetorno(var libHandle: TLibHandle; const sMensagem: PChar; var esTamanho: longint): longint;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGTINLIBName;
 {%endregion}

 {%region Ler/Gravar Config }
 function GTIN_ConfigLer(var libHandle: TLibHandle; const eArqConfig: PChar): longint;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGTINLIBName;

 function GTIN_ConfigGravar(var libHandle: TLibHandle; const eArqConfig: PChar): longint;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGTINLIBName;

 function GTIN_ConfigLerValor(var libHandle: TLibHandle; const eSessao, eChave: PChar; sValor: PChar; var esTamanho: longint): longint;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGTINLIBName;

 function GTIN_ConfigGravarValor(var libHandle: TLibHandle; const eSessao, eChave, eValor: PChar): longint;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGTINLIBName;
 {%endregion}

 {%region GTIN}
 function GTIN_Consultar(var libHandle: TLibHandle; const aGTIN, sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGTINLIBName;
 {%endregion}

implementation

end.
