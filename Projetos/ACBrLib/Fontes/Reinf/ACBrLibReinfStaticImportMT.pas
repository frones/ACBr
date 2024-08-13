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

unit ACBrLibReinfStaticImportMT;

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
  CACBrReinfLIBName = 'ACBrReinf64.dll';
  {$Else}
  CACBrReinfLIBName = 'ACBrReinf32.dll';
  {$EndIf}
 {$Else}
  {$IfDef CPU64}
  CACBrReinfLIBName = 'ACBrReinf64.so';
  {$Else}
  CACBrReinfLIBName = 'ACBrReinf32.so';

  {$EndIf}
 {$EndIf}

{$I ACBrLibErros.inc}

{%region Constructor/Destructor}
function Reinf_Inicializar(var libHandle: TLibHandle; const eArqConfig, eChaveCrypt: PAnsiChar): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrReinfLIBName;

function Reinf_Finalizar(const libHandle: TLibHandle): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrReinfLIBName;
{%endregion}

{%region Versao/Retorno}
function Reinf_Nome(const libHandle: TLibHandle; const sNome: PAnsiChar; var esTamanho: integer): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrReinfLIBName;

function Reinf_Versao(const libHandle: TLibHandle; const sVersao: PAnsiChar; var esTamanho: integer): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrReinfLIBName;

function Reinf_OpenSSLInfo(const libHandle: TLibHandle; const sOpenSSLInfo: PAnsiChar; var esTamanho: integer): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrReinfLIBName;

function Reinf_UltimoRetorno(const libHandle: TLibHandle; const sMensagem: PAnsiChar; var esTamanho: integer): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrReinfLIBName;
{%endregion}

{%region Ler/Gravar Config }
function Reinf_ConfigLer(const libHandle: TLibHandle; const eArqConfig: PAnsiChar): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrReinfLIBName;

function Reinf_ConfigGravar(const libHandle: TLibHandle; const eArqConfig: PAnsiChar): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrReinfLIBName;

function Reinf_ConfigLerValor(const libHandle: TLibHandle; const eSessao, eChave: PAnsiChar; sValor: PAnsiChar; var esTamanho: integer): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrReinfLIBName;

function Reinf_ConfigGravarValor(const libHandle: TLibHandle; const eSessao, eChave, eValor: PAnsiChar): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrReinfLIBName;
{%endregion}

{%region Reinf}
function Reinf_CriarEventoReinf(const libHandle: TLibHandle; const eArqIni: PAnsiChar): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrReinfLIBName;

function Reinf_EnviarReinf(const libHandle: TLibHandle; const sResposta: PAnsiChar;
  var esTamanho: integer): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrReinfLIBName;

function Reinf_ConsultarReinf(const libHandle: TLibHandle; const eProtocolo, sResposta: PAnsiChar; var esTamanho: integer): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrReinfLIBName;

function Reinf_ConsultarReciboReinf(const libHandle: TLibHandle;
  const ePerApur: PAnsiChar; const aTipoEvento: Integer; const eNrInscEstab,
  eCnpjPrestador, eNrInscTomador, eDtApur, eCpfCnpjBenef, eCnpjFonte: PAnsiChar;
  const sResposta: PAnsiChar; var esTamanho: integer): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrReinfLIBName;

function Reinf_CriarEnviarReinf(const libHandle: TLibHandle; const eArqIni: PAnsiChar;
  const sResposta: PAnsiChar; var esTamanho: integer): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrReinfLIBName;

function Reinf_LimparReinf(const libHandle: TLibHandle): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrReinfLIBName;

function Reinf_CarregarXMLEventoReinf (const libHandle: TLibHandle; const eArquivoOuXML: PAnsiChar): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrReinfLIBName;

function Reinf_SetIDContribuinte (const libHandle: TLibHandle; const aIdContribuinte: PAnsiChar):integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrReinfLIBName;

function Reinf_SetIDTransmissor (const libHandle: TLibHandle; const aIdTransmissor: PAnsiChar): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrReinfLIBName;

function Reinf_SetTipoContribuinte (const libHandle: TLibHandle; aTipoContribuinte: integer):integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrReinfLIBName;

function Reinf_SetVersaoDF (const libHandle: TLibHandle; const sVersao: PAnsiChar):integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrReinfLIBName;

function Reinf_ObterCertificados (const libHandle: TLibHandle; const sResposta: PAnsiChar; var esTamanho: integer):integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrReinfLIBName;

function Reinf_Validar(const libHandle:TLibHandle): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrReinfLIBName;

{%endregion}

implementation

end.
