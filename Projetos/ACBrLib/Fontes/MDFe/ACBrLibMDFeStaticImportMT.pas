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

unit ACBrLibMDFeStaticImportMT;

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
  CACBrMDFeLIBName = 'ACBrMDFe64.dll';
  {$Else}
  CACBrMDFeLIBName = 'ACBrMDFe32.dll';
  {$EndIf}
 {$Else}
  {$IfDef CPU64}
  CACBrMDFeLIBName = 'ACBrMDFe64.so';
  {$Else}
  CACBrMDFeLIBName = 'ACBrMDFe32.so';
  {$EndIf}
 {$EndIf}

{$I ACBrLibErros.inc}

{%region Declaração da funções}

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}
function MDFE_Inicializar(var libHandle: TLibHandle; const eArqConfig, eChaveCrypt: PAnsiChar): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_Finalizar(const libHandle: TLibHandle): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_Nome(const libHandle: TLibHandle; const sNome: PAnsiChar; var esTamanho: Integer): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_Versao(const libHandle: TLibHandle; const sVersao: PAnsiChar; var esTamanho: Integer): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_OpenSSLInfo(const libHandle: TLibHandle; const sOpenSSLInfo: PAnsiChar; var esTamanho: Integer): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_UltimoRetorno(const libHandle: TLibHandle; const sMensagem: PAnsiChar; var esTamanho: Integer): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_ConfigLer(const libHandle: TLibHandle; const eArqConfig: PAnsiChar): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_ConfigGravar(const libHandle: TLibHandle; const eArqConfig: PAnsiChar): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_ConfigLerValor(const libHandle: TLibHandle; const eSessao, eChave: PAnsiChar; sValor: PAnsiChar;
  var esTamanho: Integer): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_ConfigGravarValor(const libHandle: TLibHandle; const eSessao, eChave, eValor: PAnsiChar): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
{%endregion}

{%region MDFe}
function MDFE_CarregarXML(const libHandle: TLibHandle; const eArquivoOuXML: PAnsiChar): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_CarregarINI(const libHandle: TLibHandle; const eArquivoOuINI: PAnsiChar): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_ObterXml(const libHandle: TLibHandle; AIndex: Integer; const sResposta: PAnsiChar;
  var esTamanho: Integer): Integer;
      {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_GravarXml(const libHandle: TLibHandle; AIndex: Integer;
  const eNomeArquivo, ePathArquivo: PAnsiChar): Integer;
      {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_CarregarEventoXML(const libHandle: TLibHandle; const eArquivoOuXML: PAnsiChar): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_CarregarEventoINI(const libHandle: TLibHandle; const eArquivoOuINI: PAnsiChar): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_LimparLista(const libHandle: TLibHandle): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_LimparListaEventos(const libHandle: TLibHandle): Integer;
      {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_Assinar(const libHandle: TLibHandle): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_Validar(const libHandle: TLibHandle): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_ValidarRegrasdeNegocios(const libHandle: TLibHandle; const sResposta: PAnsiChar;
  var esTamanho: Integer): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_VerificarAssinatura(const libHandle: TLibHandle; const sResposta: PAnsiChar;
  var esTamanho: Integer): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
{%endregion}

{%region Servicos}
function MDFE_StatusServico(const libHandle: TLibHandle; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_Consultar(const libHandle: TLibHandle; const eChaveOuMDFe: PAnsiChar; const sResposta: PAnsiChar;
  var esTamanho: Integer): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_Enviar(const libHandle: TLibHandle; ALote: integer; Imprimir: boolean; Sincrono: boolean;
  const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_ConsultarRecibo(const libHandle: TLibHandle; ARecibo: PAnsiChar; const sResposta: PAnsiChar;
  var esTamanho: Integer): Integer;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_Cancelar(const libHandle: TLibHandle; const eChave, eJustificativa, eCNPJCPF: PAnsiChar;
  ALote: integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_EnviarEvento(const libHandle: TLibHandle; idLote: integer; const sResposta: PAnsiChar;
  var esTamanho: Integer): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_DistribuicaoDFePorUltNSU(const libHandle: TLibHandle; eCNPJCPF, eultNSU: PAnsiChar;
  const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_DistribuicaoDFePorNSU(const libHandle: TLibHandle; eCNPJCPF, eNSU: PAnsiChar;
  const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_DistribuicaoDFePorChave(const libHandle: TLibHandle; eCNPJCPF, echMDFe: PAnsiChar;
  const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_EnviarEmail(const libHandle: TLibHandle; const ePara, eArquivoXmlMDFe: PAnsiChar;
  const AEnviaPDF: boolean; const eAssunto, eCC, eAnexos, eMensagem: PAnsiChar): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_EnviarEmailEvento(const libHandle: TLibHandle; const ePara, eArquivoXmlEvento, eArquivoXmlMDFe: PAnsiChar;
  const AEnviaPDF: boolean; const eAssunto, eCC, eAnexos, eMensagem: PAnsiChar): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_Imprimir(const libHandle: TLibHandle; const cImpressora: PAnsiChar; nNumCopias: integer;
  const cProtocolo, bMostrarPreview: PAnsiChar): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_ImprimirPDF(const libHandle: TLibHandle): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_SalvarPDF(const libHandle: TLibHandle; const sResposta: PAnsiChar; var esTamanho: Integer):Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_ImprimirEvento(const libHandle: TLibHandle; const eArquivoXmlMDFe, eArquivoXmlEvento: PAnsiChar): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_ImprimirEventoPDF(const libHandle: TLibHandle;
  const eArquivoXmlMDFe, eArquivoXmlEvento: PAnsiChar): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_SalvarEventoPDF(const libHandle: TLibHandle; const eArquivoXmlMDFe, eArquivoXmlEvento, sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
{%endregion}

{%endregion}

implementation

end.

