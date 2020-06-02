{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }

{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }

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

unit ACBrLibGNReStaticImport;

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
function GNRE_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;
function GNRe_Finalizar: longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;
function GNRe_Nome(const sNome: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;
function GNRe_Versao(const sVersao: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;
function GNRe_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;
function GNRe_ConfigLer(const eArqConfig: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;
function GNRe_ConfigGravar(const eArqConfig: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;
function GNRe_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;
function GNRE_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;
{%endregion}

{%region GNRe}
function GNRE_LimparLista: longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;
function GNRE_CarregarXML(const eArquivoOuXML: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;
function GNRE_CarregarINI(const eArquivoOuINI: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;
function GNRE_ObterXml(AIndex: longint; const sResposta: PChar;
  var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;
function GNRE_GravarXml(AIndex: longint;
  const eNomeArquivo, ePathArquivo: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;
function GNRE_LimparListaGuiaRetorno: longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;
function GNRE_CarregarGuiaRetorno(const eArquivoOuXml: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;
function GNRE_Assinar: longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
  external CACBrGNReLIBName;
function GNRE_Validar: longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
  external CACBrGNReLIBName;
function GNRE_VerificarAssinatura(const sResposta: PChar;
  var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;
function GNRE_ObterCertificados(const sResposta: PChar;
  var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;
{%endregion}

{%region Servicos}
function GNRE_Enviar(const sResposta: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;
function GNRE_Consultar(const eUF: PChar; const AReceita: integer;
  const sResposta: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;
function GNRE_EnviarEmail(const ePara, eArquivoOuXml: PChar;
  const AEnviaPDF: boolean; const eAssunto, eCC, eAnexos, eMensagem: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;
function GNRE_Imprimir(const eNomeImpressora, eMostrarPreview: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;
function GNRE_ImprimirPDF: longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrGNReLIBName;
{%endregion}

implementation

end.
