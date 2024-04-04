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

unit ACBrLibPIXCDStaticImportMT;

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
  CACBrPIXCDLIBName = 'ACBrPIXCD64.dll';
  {$Else}
  CACBrPIXCDLIBName = 'ACBrPIXCD32.dll';
  {$EndIf}
 {$Else}
  {$IfDef CPU64}
  CACBrPIXCDLIBName = 'ACBrPIXCD64.so';
  {$Else}
  CACBrPIXCDLIBName = 'ACBrPIXCD32.so';

  {$EndIf}
 {$EndIf}

 {$I ACBrLibErros.inc}

 {%region Constructor/Destructor}
 function PIXCD_Inicializar(var libHandle: TLibHandle; const eArqConfig, eChaveCrypt: PChar): longint;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_Finalizar (const libHandle: TLibHandle): longint;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;
 {%endregion}

 {%region Versao/Retorno}
 function PIXCD_Nome(const libHandle: TLibHandle; const sNome: PChar; var esTamanho: longint): longint;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_Versao(const libHandle: TLibHandle; const sVersao: PChar; var esTamanho: longint): longint;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_OpenSSLInfo(const libHandle: TLibHandle; const sOpenSSLInfo: PChar; var esTamanho: longint): longint;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_UltimoRetorno(const libHandle: TLibHandle; const sMensagem: PChar; var esTamanho: longint): longint;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;
 {%endregion}

 {%region Ler/Gravar Config }
 function PIXCD_ConfigLer(const libHandle: TLibHandle; const eArqConfig: PChar): longint;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_ConfigGravar(const libHandle: TLibHandle; const eArqConfig: PChar): longint;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_ConfigLerValor(const libHandle: TLibHandle; const eSessao, eChave: PChar; sValor: PChar; var esTamanho: longint): longint;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_ConfigGravarValor(const libHandle: TLibHandle; const eSessao, eChave, eValor: PChar): longint;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;
 {%endregion}

 {%region PIXCD}

 function PIXCD_GerarQRCodeEstatico(const libHandle: TLibHandle; AValor: Double; const AinfoAdicional: PChar; const ATxId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_ConsultarPix (const libHandle: TLibHandle; const Ae2eid: PChar; const sResposta: PChar; var esTamanho: longint): longint;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_ConsultarPixRecebidos (const libHandle: TLibHandle; ADataInicio: TDateTime; ADataFim: TDateTime; const ATxId: PChar; const ACpfCnpj: PChar; PagAtual: longint; ItensPorPagina: longint; const sResposta: PChar; var esTamanho: longint): longint;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_SolicitarDevolucaoPix(const libHandle: TLibHandle; AInfDevolucao: PChar; const Ae2eid: PChar; AidDevolucao: PChar; const sResposta: PChar; var esTamanho: longint): longint;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_ConsultarDevolucaoPix(const libHandle: TLibHandle; const Ae2eid, AidDevolucao: PChar; const sResposta: PChar; var esTamanho: longint):longint;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_CriarCobrancaImediata(const libHandle: TLibHandle; AInfCobSolicitada: PChar; const ATxId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_ConsultarCobrancaImediata(const libHandle: TLibHandle; const ATxId: PChar; ARevisao: longint; const sResposta: PChar; var esTamanho: longint): longint;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_RevisarCobrancaImediata(const libHandle: TLibHandle; AInfCobRevisada: PChar; const ATxId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_CancelarCobrancaImediata(const libHandle: TLibHandle; ATxId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_CriarCobranca(const libHandle: TLibHandle; AInfCobVSolicitada: PChar; ATxId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_ConsultarCobranca(const libHandle: TLibHandle; const ATxId: PChar; ARevisao: longint; const sResposta: PChar; var esTamanho: longint): longint;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_RevisarCobranca(const libHandle: TLibHandle; AInfCobVRevisada: PChar; const ATxId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_CancelarCobranca(const libHandle: TLibHandle; ATxId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 {%endregion}

implementation

end.
