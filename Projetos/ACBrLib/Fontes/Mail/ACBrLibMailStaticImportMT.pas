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

unit ACBrLibMailStaticImportMT;

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
  CACBrMailLIBName = 'ACBrMail64.dll';
  {$Else}
  CACBrMailLIBName = 'ACBrMail32.dll';
  {$EndIf}
 {$Else}
  {$IfDef CPU64}
  CACBrMailLIBName = 'ACBrMail64.so';
  {$Else}
  CACBrMailLIBName = 'ACBrMail32.so';

  {$EndIf}
 {$EndIf}

{$I ACBrLibErros.inc}

{%region Constructor/Destructor}
function MAIL_Inicializar(var libHandle: TLibHandle; const eArqConfig, eChaveCrypt: PAnsiChar): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMailLIBName;

function MAIL_Finalizar(const libHandle: TLibHandle): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMailLIBName;
{%endregion}

{%region Versao/Retorno}
function MAIL_Nome(const libHandle: TLibHandle; const sNome: PAnsiChar; var esTamanho: integer): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMailLIBName;

function MAIL_Versao(const libHandle: TLibHandle; const sVersao: PAnsiChar; var esTamanho: integer): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMailLIBName;

function MAIL_OpenSSLInfo(const libHandle: TLibHandle; const sOpenSSLInfo: PAnsiChar; var esTamanho: integer): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMailLIBName;

function MAIL_UltimoRetorno(const libHandle: TLibHandle; const sMensagem: PAnsiChar; var esTamanho: integer): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMailLIBName;
{%endregion}

{%region Ler/Gravar Config }
function MAIL_ConfigLer(const libHandle: TLibHandle; const eArqConfig: PAnsiChar): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMailLIBName;

function MAIL_ConfigGravar(const libHandle: TLibHandle; const eArqConfig: PAnsiChar): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMailLIBName;

function MAIL_ConfigLerValor(const libHandle: TLibHandle; const eSessao, eChave: PAnsiChar; sValor: PAnsiChar; var esTamanho: integer): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMailLIBName;

function MAIL_ConfigGravarValor(const libHandle: TLibHandle; const eSessao, eChave, eValor: PAnsiChar): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMailLIBName;
{%endregion}

{%region Diversos}
function MAIL_SetSubject(const libHandle: TLibHandle; const eSubject: PAnsiChar): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMailLIBName;
function MAIL_AddAddress(const libHandle: TLibHandle; const eEmail, eName: PAnsiChar): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMailLIBName;
function MAIL_AddReplyTo(const libHandle: TLibHandle; const eEmail, eName: PAnsiChar): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMailLIBName;
function MAIL_AddCC(const libHandle: TLibHandle; const eEmail, eName: PAnsiChar): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMailLIBName;
function MAIL_AddBCC(const libHandle: TLibHandle; const eEmail: PAnsiChar): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMailLIBName;
function MAIL_ClearAttachment(const libHandle: TLibHandle): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMailLIBName;
function MAIL_AddAttachment(const libHandle: TLibHandle; const eFileName, eDescription: PAnsiChar;
            const aDisposition: Integer): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMailLIBName;
function MAIL_AddBody(const libHandle: TLibHandle; const eBody: PAnsiChar): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMailLIBName;
function MAIL_AddAltBody(const libHandle: TLibHandle; const eAltBody: PAnsiChar): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMailLIBName;
function MAIL_SaveToFile(const libHandle: TLibHandle; const eFileName: PAnsiChar): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMailLIBName;
{%endregion}

{%region Envio}
function MAIL_Clear(const libHandle: TLibHandle): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMailLIBName;
function MAIL_Send(const libHandle: TLibHandle): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMailLIBName;
{%endregion}

implementation

end.

