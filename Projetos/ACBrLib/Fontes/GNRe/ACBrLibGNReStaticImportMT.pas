{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }

{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }

{ Colaboradores nesse arquivo: Rafael Teno Dias                                }

{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }

{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }

{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }

{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }

{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

unit ACBrLibGNReStaticImportMT;

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
  CACBrGNReLIBName = 'ACBrGNRe64.dll';
  {$Else}
  CACBrGNReLIBName = 'ACBrGNRe32.dll';
  {$EndIf}
 {$Else}
  {$IfDef CPU64}
  CACBrGNReLIBName = 'acbrgnre64.so';
  {$Else}
  CACBrGNReLIBName = 'acbrgnre32.so';

  {$EndIf}
 {$EndIf}

{$I ACBrLibErros.inc}

{%region Constructor/Destructor}
function GNRE_Inicializar(var libHandle: TLibHandle; const eArqConfig, eChaveCrypt: PAnsiChar): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;
function GNRE_Finalizar(const libHandle: TLibHandle): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;
function GNRE_Nome(const libHandle: TLibHandle; const sNome: PAnsiChar; var esTamanho: Integer): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;
function GNRE_Versao(const libHandle: TLibHandle; const sVersao: PAnsiChar; var esTamanho: Integer): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;
function GNRE_UltimoRetorno(const libHandle: TLibHandle; const sMensagem: PAnsiChar; var esTamanho: Integer): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;
function GNRE_ConfigLer(const libHandle: TLibHandle; const eArqConfig: PAnsiChar): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;
function GNRE_ConfigGravar(const libHandle: TLibHandle; const eArqConfig: PAnsiChar): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;
function GNRE_ConfigLerValor(const libHandle: TLibHandle; const eSessao, eChave: PAnsiChar; sValor: PAnsiChar;
  var esTamanho: Integer): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;
function GNRE_ConfigGravarValor(const libHandle: TLibHandle; const eSessao, eChave, eValor: PAnsiChar): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;
{%endregion}

{%region GNRe}
function GNRE_LimparLista(const libHandle: TLibHandle): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;
function GNRE_CarregarXML(const libHandle: TLibHandle; const eArquivoOuXML: PAnsiChar): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;
function GNRE_CarregarINI(const libHandle: TLibHandle; const eArquivoOuINI: PAnsiChar): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;
function GNRE_ObterXml(const libHandle: TLibHandle; AIndex: Integer; const sResposta: PAnsiChar;
  var esTamanho: Integer): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;
function GNRE_GravarXml(const libHandle: TLibHandle; AIndex: Integer;
  const eNomeArquivo, ePathArquivo: PAnsiChar): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;
function GNRE_LimparListaGuiaRetorno(const libHandle: TLibHandle): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;
function GNRE_CarregarGuiaRetorno(const libHandle: TLibHandle; const eArquivoOuXml: PAnsiChar): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;
function GNRE_Assinar(const libHandle: TLibHandle): Integer; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
  external CACBrGNReLIBName;
function GNRE_Validar(const libHandle: TLibHandle): Integer; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
  external CACBrGNReLIBName;
function GNRE_VerificarAssinatura(const libHandle: TLibHandle; const sResposta: PAnsiChar;
  var esTamanho: Integer): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;
function GNRE_ObterCertificados(const libHandle: TLibHandle; const sResposta: PAnsiChar;
  var esTamanho: Integer): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;
{%endregion}

{%region Servicos}
function GNRE_Enviar(const libHandle: TLibHandle; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;
function GNRE_Consultar(const libHandle: TLibHandle; const eUF: PAnsiChar; const AReceita: integer;
  const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;
function GNRE_EnviarEmail(const libHandle: TLibHandle; const ePara, eArquivoOuXml: PAnsiChar;
  const AEnviaPDF: boolean; const eAssunto, eCC, eAnexos, eMensagem: PAnsiChar): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;
function GNRE_Imprimir(const libHandle: TLibHandle; const eNomeImpressora, eMostrarPreview: PAnsiChar): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;
function GNRE_ImprimirPDF(const libHandle: TLibHandle): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;
{%endregion}

implementation

end.
