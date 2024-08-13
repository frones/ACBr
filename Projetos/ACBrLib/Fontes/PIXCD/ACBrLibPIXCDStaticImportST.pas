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

unit ACBrLibPIXCDStaticImportST;

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
 function PIXCD_Inicializar(const eArqConfig, eChaveCrypt: PAnsiChar): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_Finalizar: integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;
 {%endregion}

 {%region Versao/Retorno}
 function PIXCD_Nome(const sNome: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_Versao(const sVersao: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_OpenSSLInfo(const sOpenSSLInfo: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_UltimoRetorno(const sMensagem: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;
 {%endregion}

 {%region Ler/Gravar Config }
 function PIXCD_ConfigLer(const eArqConfig: PAnsiChar): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_ConfigGravar(const eArqConfig: PAnsiChar): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_ConfigLerValor(const eSessao, eChave: PAnsiChar; sValor: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_ConfigGravarValor(const eSessao, eChave, eValor: PAnsiChar): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;
 {%endregion}

  function PIXCD_GerarQRCodeEstatico(AValor: Double; const AinfoAdicional: PAnsiChar; const ATxId: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_ConsultarPix (const Ae2eid: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_ConsultarPixRecebidos (ADataInicio: TDateTime; ADataFim: TDateTime; const ATxId: PAnsiChar; const ACpfCnpj: PAnsiChar; PagAtual: integer; ItensPorPagina: integer; const sResposta: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_SolicitarDevolucaoPix(AInfDevolucao: PAnsiChar; const Ae2eid: PAnsiChar; AidDevolucao: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_ConsultarDevolucaoPix(const Ae2eid, AidDevolucao: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: integer):integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_CriarCobrancaImediata(AChavePIX: AInfCobSolicitada: PAnsiChar; const ATxId: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_ConsultarCobrancaImediata(const ATxId: PAnsiChar; ARevisao: integer; const sResposta: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_ConsultarCobrancasCob(ADataInicio: TDateTime; ADataFim: TDateTime; ACpfCnpj: PAnsiChar; ALocationPresente: Boolean; AStatus: integer; PagAtual: integer; ItensPorPagina: integer; const sResposta: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_RevisarCobrancaImediata(AInfCobRevisada: PAnsiChar; const ATxId: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_CancelarCobrancaImediata(]ATxId: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_CriarCobranca(AInfCobVSolicitada: PAnsiChar; ATxId: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_ConsultarCobranca(const ATxId: PAnsiChar; ARevisao: integer; const sResposta: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_ConsultarCobrancasCobV(ADataInicio: TDateTime; ADataFim: TDateTime; ACpfCnpj: PAnsiChar; ALocationPresente: Boolean; AStatus: integer; PagAtual: integer; ItensPorPagina; integer; const sResposta: PAnsiChar; var esTamanho: integer): integer;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_RevisarCobranca(AInfCobVRevisada: PAnsiChar; const ATxId: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_CancelarCobranca(ATxId: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 //Matera
 function PIXCD_Matera_IncluirConta(aInfIncluirConta: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};  external CACBrPIXCDLIBName;

 function PIXCD_Matera_ConsultarConta(aAccountId: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_Matera_InativarConta(aAccountId: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_Matera_IncluirChavePix(aAccountId, aExternalID: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_Matera_ConsultarChavePix(aAccountId: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_Matera_ExcluirChavePix(aAccountId, aChavePIX: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_Matera_GerarQRCode(aInfQRCode: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_Matera_ConsultarTransacao(aAccountId, aTransactionID: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_Matera_ConsultarSaldoEC(aAccountId: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_Matera_ConsultarExtratoEC(aAccountId: PAnsiChar; aInicio: TDateTime; aFim: TDateTime; const sResposta: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_Matera_ConsultarMotivosDevolucao(const sResposta: PAnsiChar; var esTamanho: integer): integer;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_Matera_SolicitarDevolucao(aInfSolicitarDevolucao: PAnsiChar; aAccountId, aTransactionID: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: integer): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_Matera_ConsultarAliasRetirada(aAccountId, aAlias: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: integer): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 function PIXCD_Matera_SolicitarRetirada(aInfSolicitarRetirada: PAnsiChar; aAccountId: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: integer): integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrPIXCDLIBName;

 {%endregion}

implementation

end.

