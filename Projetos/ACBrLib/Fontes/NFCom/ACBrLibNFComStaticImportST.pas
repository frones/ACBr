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

unit ACBrLibNFComStaticImportST;

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
function NFCom_Inicializar(const eArqConfig, eChaveCrypt: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_Finalizar: Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;
{%endregion}

{%region Versao/Retorno}
function NFCom_Nome(const sNome: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_Versao(const sVersao: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_OpenSSLInfo(const sOpenSSLInfo: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_UltimoRetorno(const sMensagem: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_ConfigImportar (const eArqConfig: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_ConfigExportar (const sMensagem: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;
{%endregion}

{%region Ler/Gravar Config }
function NFCom_ConfigLer(const eArqConfig: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_ConfigGravar(const eArqConfig: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_ConfigLerValor(const eSessao, eChave: PAnsiChar; sValor: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_ConfigGravarValor(const eSessao, eChave, eValor: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;
{%endregion}

{%region ACBrNFCom}
function NFCom_CarregarXML(const eArquivoOuXML: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_CarregarINI(const eArquivoOuINI: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_ObterXml(AIndex: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_GravarXml(AIndex: Integer; const eNomeArquivo, ePathArquivo: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_ObterIni(AIndex: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_GravarIni(AIndex: Integer; const eNomeArquivo, ePathArquivo: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_CarregarEventoXML(const eArquivoOuXML: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_CarregarEventoINI(const eArquivoOuINI: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_LimparLista: Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_LimparListaEventos: Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_Assinar: Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_Validar: Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_ValidarRegrasdeNegocios(const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_VerificarAssinatura(const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_ObterCertificados(const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_GetPath(ATipo: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_GetPathEvento(ACodEvento: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_StatusServico(const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_Enviar(AImprimir: Boolean; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_Consultar(const eChaveOuNFCom: PAnsiChar; AExtrairEventos: Boolean;
  const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_Cancelar(const eChave, eJustificativa, eCNPJCPF: PAnsiChar; ALote: Integer;
  const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_EnviarEvento(idLote: Integer;
  const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_EnviarEmail(const ePara, eChaveNFCom: PAnsiChar; const AEnviaPDF: Boolean;
  const eAssunto, eCC, eAnexos, eMensagem: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_EnviarEmailEvento(const ePara, eChaveEvento, eChaveNFCom: PAnsiChar;
  const AEnviaPDF: Boolean; const eAssunto, eCC, eAnexos, eMensagem: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_Imprimir(const cImpressora: PAnsiChar; nNumCopias: Integer; bMostrarPreview: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_ImprimirPDF: Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_SalvarPDF(const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_ImprimirEvento(const eArquivoXmlNFCom, eArquivoXmlEvento: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_ImprimirEventoPDF(const eArquivoXmlNFCom, eArquivoXmlEvento: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;

function NFCom_SalvarEventoPDF(const eArquivoXmlNFCom, eArquivoXmlEvento, sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFComLIBName;
{%endregion}
implementation

end.

