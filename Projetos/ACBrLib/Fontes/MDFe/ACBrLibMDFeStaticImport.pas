{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
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

unit ACBrLibMDFeStaticImport;

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
function MDFE_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_Finalizar: longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_Nome(const sNome: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_Versao(const sVersao: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_ConfigLer(const eArqConfig: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_ConfigGravar(const eArqConfig: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
{%endregion}

{%region MDFe}
function MDFE_CarregarXML(const eArquivoOuXML: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_CarregarINI(const eArquivoOuINI: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_ObterXml(AIndex: longint; const sResposta: PChar;
  var esTamanho: longint): longint;
      {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_GravarXml(AIndex: longint;
  const eNomeArquivo, ePathArquivo: PChar): longint;
      {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_CarregarEventoXML(const eArquivoOuXML: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_CarregarEventoINI(const eArquivoOuINI: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_LimparLista: longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_LimparListaEventos: longint;
      {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_Assinar: longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_Validar: longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_ValidarRegrasdeNegocios(const sResposta: PChar;
  var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_VerificarAssinatura(const sResposta: PChar;
  var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
{%endregion}

{%region Servicos}
function MDFE_StatusServico(const sResposta: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_Consultar(const eChaveOuMDFe: PChar; const sResposta: PChar;
  var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_Enviar(ALote: integer; Imprimir, Sincrono: boolean;
  const sResposta: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_ConsultarRecibo(ARecibo: PChar; const sResposta: PChar;
  var esTamanho: longint): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_Cancelar(const eChave, eJustificativa, eCNPJCPF: PChar;
  ALote: integer; const sResposta: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_EnviarEvento(idLote: integer; const sResposta: PChar;
  var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_DistribuicaoDFePorUltNSU(eCNPJCPF, eultNSU: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_DistribuicaoDFePorNSU(eCNPJCPF, eNSU: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_DistribuicaoDFePorChave(eCNPJCPF, echMDFe: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_EnviarEmail(const ePara, eArquivoXmlMDFe: PChar;
  const AEnviaPDF: boolean; const eAssunto, eCC, eAnexos, eMensagem: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_EnviarEmailEvento(const ePara, eArquivoXmlEvento, eArquivoXmlMDFe: PChar;
  const AEnviaPDF: boolean; const eAssunto, eCC, eAnexos, eMensagem: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_Imprimir(const cImpressora: PChar; nNumCopias: integer;
  const cProtocolo, bMostrarPreview: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_ImprimirPDF: longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_SalvarPDF(const sResposta: PChar; var esTamanho: longint):longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_ImprimirEvento(const eArquivoXmlMDFe, eArquivoXmlEvento: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_ImprimirEventoPDF(
  const eArquivoXmlMDFe, eArquivoXmlEvento: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
function MDFE_SalvarEventoPDF(const eArquivoXmlMDFe, eArquivoXmlEvento, sResposta: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMDFeLIBName;
{%endregion}

{%endregion}

implementation

end.
