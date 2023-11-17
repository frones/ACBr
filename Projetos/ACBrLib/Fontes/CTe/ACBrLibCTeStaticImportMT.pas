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

unit ACBrLibCTeStaticImportMT;

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
  CACBrCTeLIBName = 'ACBrCTe64.dll';
  {$Else}
  CACBrCTeLIBName = 'ACBrCTe32.dll';
  {$EndIf}
 {$Else}
  {$IfDef CPU64}
  CACBrCTeLIBName = 'acbrcte64.so';
  {$Else}
  CACBrCTeLIBName = 'acbrcte32.so';

  {$EndIf}
 {$EndIf}

function CTE_Inicializar(var libHandle: TLibHandle; const eArqConfig, eChaveCrypt: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;
function CTE_Finalizar(const libHandle: TLibHandle): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_Nome(const libHandle: TLibHandle; const sNome: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_Versao(const libHandle: TLibHandle; const sVersao: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_UltimoRetorno(const libHandle: TLibHandle; const sMensagem: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_ConfigLer(const libHandle: TLibHandle; const eArqConfig: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_ConfigGravar(const libHandle: TLibHandle; const eArqConfig: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_ConfigLerValor(const libHandle: TLibHandle; const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_ConfigGravarValor(const libHandle: TLibHandle; const eSessao, eChave, eValor: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_CarregarXML(const libHandle: TLibHandle; const eArquivoOuXML: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_CarregarINI(const libHandle: TLibHandle; const eArquivoOuINI: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_ObterXml(const libHandle: TLibHandle; AIndex: longint; const sResposta: PChar;
  var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_GravarXml(const libHandle: TLibHandle; AIndex: longint;
  const eNomeArquivo, ePathArquivo: PChar): longint;
      {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_CarregarEventoXML(const libHandle: TLibHandle; const eArquivoOuXML: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_CarregarEventoINI(const libHandle: TLibHandle; const eArquivoOuINI: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_LimparLista(const libHandle: TLibHandle): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_LimparListaEventos(const libHandle: TLibHandle): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_Assinar(const libHandle: TLibHandle): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_Validar(const libHandle: TLibHandle): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_ValidarRegrasdeNegocios(const libHandle: TLibHandle; const sResposta: PChar;
  var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_VerificarAssinatura(const libHandle: TLibHandle; const sResposta: PChar;
  var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_StatusServico(const libHandle: TLibHandle; const sResposta: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_Consultar(const libHandle: TLibHandle; const eChaveOuCTe: PChar; const sResposta: PChar;
  var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_Inutilizar(const libHandle: TLibHandle; const ACNPJ, AJustificativa: PChar;
  Ano, Modelo, Serie, NumeroInicial, NumeroFinal: integer;
  const sResposta: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_Enviar(const libHandle: TLibHandle; ALote: integer; Imprimir: boolean; const sResposta: PChar;
  var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_ConsultarRecibo(const libHandle: TLibHandle; ARecibo: PChar; const sResposta: PChar;
  var esTamanho: longint): longint;
      {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_Cancelar(const libHandle: TLibHandle; const eChave, eJustificativa, eCNPJ: PChar;
  ALote: integer; const sResposta: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_EnviarEvento(const libHandle: TLibHandle; idLote: integer; const sResposta: PChar;
  var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_ConsultaCadastro(const libHandle: TLibHandle; cUF, nDocumento: PChar; nIE: boolean;
  const sResposta: PChar; var esTamanho: longint): longint;
      {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_DistribuicaoDFePorUltNSU(const libHandle: TLibHandle; const AcUFAutor: integer;
  eCNPJCPF, eultNSU: PChar; const sResposta: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_DistribuicaoDFePorNSU(const libHandle: TLibHandle; const AcUFAutor: integer;
  eCNPJCPF, eNSU: PChar; const sResposta: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_DistribuicaoDFePorChave(const libHandle: TLibHandle; const AcUFAutor: integer;
  eCNPJCPF, echCTe: PChar; const sResposta: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_EnviarEmail(const libHandle: TLibHandle; const ePara, eChaveCTe: PChar; const AEnviaPDF: boolean;
  const eAssunto, eCC, eAnexos, eMensagem: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_EnviarEmailEvento(const libHandle: TLibHandle; const ePara, eChaveEvento, eChaveCTe: PChar;
  const AEnviaPDF: boolean; const eAssunto, eCC, eAnexos, eMensagem: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_Imprimir(const libHandle: TLibHandle; const cImpressora: PChar; nNumCopias: integer;
  const cProtocolo, bMostrarPreview: PChar): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_ImprimirPDF(const libHandle: TLibHandle): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_SalvarPDF(const libHandle: TLibHandle; const sResposta: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_ImprimirEvento(const libHandle: TLibHandle; const eArquivoXmlCTe, eArquivoXmlEvento: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_ImprimirEventoPDF(const libHandle: TLibHandle;
  const eArquivoXmlCTe, eArquivoXmlEvento: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_SalvarEventoPDF(const libHandle: TLibHandle; const eArquivoXmlCTe, eArquivoXmlEvento: PChar; sResposta: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function CTE_ImprimirInutilizacao(const libHandle: TLibHandle; const eArquivoXml: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_ImprimirInutilizacaoPDF(const libHandle: TLibHandle; const eArquivoXml: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

implementation

end.

