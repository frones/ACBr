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

unit ACBrLibConsultaCNPJStaticImportMT;

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
  CACBrConsultaCNPJLIBName = 'ACBrConsultaCNPJ64.dll';
  {$Else}
  CACBrConsultaCNPJLIBName = 'ACBrConsultaCNPJ32.dll';
  {$EndIf}
 {$Else}
  {$IfDef CPU64}
  CACBrConsultaCNPJLIBName = 'ACBrConsultaCNPJ64.so';
  {$Else}
  CACBrConsultaCNPJLIBName = 'ACBrConsultaCNPJ32.so';

  {$EndIf}
 {$EndIf}

 {$I ACBrLibErros.inc}

 {%region Constructor/Destructor}
 function CNPJ_Inicializar(var libHandle: TLibHandle; const eArqConfig, eChaveCrypt: PAnsiChar): Integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrConsultaCNPJLIBName;

 function CNPJ_Finalizar (const libHandle: TLibHandle): Integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrConsultaCNPJLIBName;
 {%endregion}

 {%region Versao/Retorno}
 function CNPJ_Nome(const libHandle: TLibHandle; const sNome: PAnsiChar; var esTamanho: Integer): Integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrConsultaCNPJLIBName;

 function CNPJ_Versao(const libHandle: TLibHandle; const sVersao: PAnsiChar; var esTamanho: Integer): Integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrConsultaCNPJLIBName;

 function CNPJ_OpenSSLInfo(const libHandle: TLibHandle; const sOpenSSLInfo: PAnsiChar; var esTamanho: Integer): Integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrConsultaCNPJLIBName;

 function CNPJ_UltimoRetorno(const libHandle: TLibHandle; const sMensagem: PAnsiChar; var esTamanho: Integer): Integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrConsultaCNPJLIBName;
 {%endregion}

 {%region Ler/Gravar Config }
 function CNPJ_ConfigLer(const libHandle: TLibHandle; const eArqConfig: PAnsiChar): Integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrConsultaCNPJLIBName;

 function CNPJ_ConfigGravar(const libHandle: TLibHandle; const eArqConfig: PAnsiChar): Integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrConsultaCNPJLIBName;

 function CNPJ_ConfigLerValor(const libHandle: TLibHandle; const eSessao, eChave: PAnsiChar; sValor: PAnsiChar; var esTamanho: Integer): Integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrConsultaCNPJLIBName;

 function CNPJ_ConfigGravarValor(const libHandle: TLibHandle; const eSessao, eChave, eValor: PAnsiChar): Integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrConsultaCNPJLIBName;
 {%endregion}

 {%region ConsultaCNPJ}
 function CNPJ_ConsultarCaptcha(const libHandle: TLibHandle; ePathDownload: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrConsultaCNPJLIBName;

 function CNPJ_Consultar(const libHandle: TLibHandle; eCNPJ: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: Integer):Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrConsultaCNPJLIBName;

 {%endregion}

implementation

end.
