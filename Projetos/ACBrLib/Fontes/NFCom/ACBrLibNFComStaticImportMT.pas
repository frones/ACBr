{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2025 Daniel Simoes de Almeida               }
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

unit ACBrLibNFComStaticImportMT;

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
  CACBrNFComLIBName = 'ACBrNFCom64.dll';
  {$Else}
  CACBrNFComLIBName = 'ACBrNFCom32.dll';
  {$EndIf}
 {$Else}
  {$IfDef CPU64}
  CACBrNFComLIBName = 'ACBrNFCom64.so';
  {$Else}
  CACBrNFComLIBName = 'ACBrNFCom32.so';

  {$EndIf}
 {$EndIf}

{$I ACBrLibErros.inc}

{%region Constructor/Destructor}
function NFCom_Inicializar(var libHandle: TLibHandle; const eArqConfig, eChaveCrypt: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_Finalizar(const libHandle: TLibHandle): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;
{%endregion}

{%region Versao/Retorno}
function NFCom_Nome(const libHandle: TLibHandle; const sNome: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_Versao(const libHandle: TLibHandle; const sVersao: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_OpenSSLInfo(const libHandle: TLibHandle; const sOpenSSLInfo: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_UltimoRetorno(const libHandle: TLibHandle; const sMensagem: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_ConfigImportar(const libHandle: TLibHandle; const eArqConfig: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_ConfigExportar(const libHandle: TLibHandle; const sMensagem: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

{%endregion}

{%region Ler/Gravar Config }
function NFCom_ConfigLer(const libHandle: TLibHandle; const eArqConfig: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_ConfigGravar(const libHandle: TLibHandle; const eArqConfig: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_ConfigLerValor(const libHandle: TLibHandle; const eSessao, eChave: PAnsiChar; sValor: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_ConfigGravarValor(const libHandle: TLibHandle; const eSessao, eChave, eValor: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;
{%endregion}

{%region ACBrNFCom}
function NFCom_CarregarXML(const libHandle: TLibHandle; const eArquivoOuXML: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_CarregarINI(const libHandle: TLibHandle; const eArquivoOuINI: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_ObterXml(const libHandle: TLibHandle; AIndex: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_GravarXml(const libHandle: TLibHandle; AIndex: Integer; const eNomeArquivo, ePathArquivo: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_ObterIni(const libHandle: TLibHandle; AIndex: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_GravarIni(const libHandle: TLibHandle; AIndex: Integer; const eNomeArquivo, ePathArquivo: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_CarregarEventoXML(const libHandle: TLibHandle; const eArquivoOuXML: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_CarregarEventoINI(const libHandle: TLibHandle; const eArquivoOuINI: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_LimparLista(const libHandle: TLibHandle): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_LimparListaEventos(const libHandle: TLibHandle): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_Assinar(const libHandle: TLibHandle): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_Validar(const libHandle: TLibHandle): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_ValidarRegrasdeNegocios(const libHandle: TLibHandle; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_VerificarAssinatura(const libHandle: TLibHandle; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_ObterCertificados(const libHandle: TLibHandle; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_GetPath(const libHandle: TLibHandle; ATipo: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_GetPathEvento(const libHandle: TLibHandle; ACodEvento: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_StatusServico(const libHandle: TLibHandle; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_Enviar(const libHandle: TLibHandle; AImprimir: Boolean; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_Consultar(const libHandle: TLibHandle; const eChaveOuNFCom: PAnsiChar; AExtrairEventos: Boolean;
  const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_Cancelar(const libHandle: TLibHandle; const eChave, eJustificativa, eCNPJCPF: PAnsiChar; ALote: Integer;
  const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_EnviarEvento(const libHandle: TLibHandle; idLote: Integer;
  const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_EnviarEmail(const libHandle: TLibHandle; const ePara, eChaveNFCom: PAnsiChar; const AEnviaPDF: Boolean;
  const eAssunto, eCC, eAnexos, eMensagem: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_EnviarEmailEvento(const libHandle: TLibHandle; const ePara, eChaveEvento, eChaveNFCom: PAnsiChar;
  const AEnviaPDF: Boolean; const eAssunto, eCC, eAnexos, eMensagem: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_Imprimir(const libHandle: TLibHandle; const cImpressora: PAnsiChar; nNumCopias: Integer; bMostrarPreview: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_ImprimirPDF(const libHandle: TLibHandle): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_SalvarPDF(const libHandle: TLibHandle; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_ImprimirEvento(const libHandle: TLibHandle; const eArquivoXmlNFCom, eArquivoXmlEvento: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_ImprimirEventoPDF(const libHandle: TLibHandle; const eArquivoXmlNFCom, eArquivoXmlEvento: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_SalvarEventoPDF(const libHandle: TLibHandle; const eArquivoXmlNFCom, eArquivoXmlEvento, sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;
{%endregion}

implementation

end.

