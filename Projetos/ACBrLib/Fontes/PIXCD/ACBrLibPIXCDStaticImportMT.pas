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
 function PIXCD_Inicializar(var libHandle: TLibHandle; const eArqConfig, eChaveCrypt: PAnsiChar): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_Finalizar (const libHandle: TLibHandle): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;
 {%endregion}

 {%region Versao/Retorno}
 function PIXCD_Nome(const libHandle: TLibHandle; const sNome: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_Versao(const libHandle: TLibHandle; const sVersao: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_OpenSSLInfo(const libHandle: TLibHandle; const sOpenSSLInfo: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_UltimoRetorno(const libHandle: TLibHandle; const sMensagem: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;
 {%endregion}

 {%region Ler/Gravar Config }
 function PIXCD_ConfigLer(const libHandle: TLibHandle; const eArqConfig: PAnsiChar): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_ConfigGravar(const libHandle: TLibHandle; const eArqConfig: PAnsiChar): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_ConfigLerValor(const libHandle: TLibHandle; const eSessao, eChave: PAnsiChar; sValor: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_ConfigGravarValor(const libHandle: TLibHandle; const eSessao, eChave, eValor: PAnsiChar): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;
 {%endregion}

 {%region PIXCD}

 function PIXCD_GerarQRCodeEstatico(const libHandle: TLibHandle; AValor: Double; const AinfoAdicional: PAnsiChar; const ATxId: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_ConsultarPix (const libHandle: TLibHandle; const Ae2eid: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_ConsultarPixRecebidos (const libHandle: TLibHandle; ADataInicio: TDateTime; ADataFim: TDateTime; const ATxId: PAnsiChar; const ACpfCnpj: PAnsiChar; PagAtual: integer; ItensPorPagina: integer; const sResposta: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_SolicitarDevolucaoPix(const libHandle: TLibHandle; AInfDevolucao: PAnsiChar; const Ae2eid: PAnsiChar; AidDevolucao: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_ConsultarDevolucaoPix(const libHandle: TLibHandle; const Ae2eid, AidDevolucao: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: integer):integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_CriarCobrancaImediata(const libHandle: TLibHandle; AInfCobSolicitada: PAnsiChar; const ATxId: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_ConsultarCobrancaImediata(const libHandle: TLibHandle; const ATxId: PAnsiChar; ARevisao: integer; const sResposta: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_ConsultarCobrancasCob(const libHandle: TLibHandle; ADataInicio: TDateTime; ADataFim: TDateTime; ACpfCnpj: PAnsiChar; ALocationPresente: Boolean; AStatus: integer; PagAtual: integer; ItensPorPagina: integer; const sResposta: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_RevisarCobrancaImediata(const libHandle: TLibHandle; AInfCobRevisada: PAnsiChar; const ATxId: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_CancelarCobrancaImediata(const libHandle: TLibHandle; ATxId: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_CriarCobranca(const libHandle: TLibHandle; AInfCobVSolicitada: PAnsiChar; ATxId: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_ConsultarCobranca(const libHandle: TLibHandle; const ATxId: PAnsiChar; ARevisao: integer; const sResposta: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_ConsultarCobrancasCobV(const libHandle: TLibHandle; ADataInicio: TDateTime; ADataFim: TDateTime; ACpfCnpj: PAnsiChar; ALocationPresente: Boolean; AStatus: integer; PagAtual: integer; ItensPorPagina: integer; const sResposta: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_RevisarCobranca(const libHandle: TLibHandle; AInfCobVRevisada: PAnsiChar; const ATxId: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_CancelarCobranca(const libHandle: TLibHandle; ATxId: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 //Matera
 function PIXCD_Matera_IncluirConta(const libHandle: TLibHandle; aInfIncluirConta: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_Matera_ConsultarConta(const libHandle: TLibHandle; aAccountId: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_Matera_InativarConta(const libHandle: TLibHandle; aAccountId: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_Matera_IncluirChavePix(const libHandle: TLibHandle; aAccountId, aExternalID: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_Matera_ConsultarChavePix(const libHandle: TLibHandle; aAccountId: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_Matera_ExcluirChavePix(const libHandle: TLibHandle; aAccountId, aChavePIX: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_Matera_GerarQRCode(const libHandle: TLibHandle; aInfQRCode: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_Matera_ConsultarTransacao(const libHandle: TLibHandle; aAccountId, aTransactionID: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_Matera_ConsultarSaldoEC(const libHandle: TLibHandle; aAccountId: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_Matera_ConsultarExtratoEC(const libHandle: TLibHandle; aAccountId: PAnsiChar; aInicio: TDateTime; aFim: TDateTime; const sResposta: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_Matera_ConsultarMotivosDevolucao(const libHandle: TLibHandle; const sResposta: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_Matera_SolicitarDevolucao(const libHandle: TLibHandle; aInfSolicitarDevolucao: PAnsiChar; aAccountId, aTransactionID: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_Matera_ConsultarAliasRetirada(const libHandle: TLibHandle; aAccountId, aAlias: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_Matera_SolicitarRetirada(const libHandle: TLibHandle; aInfSolicitarRetirada: PAnsiChar; aAccountId: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 {%endregion}

implementation

end.
