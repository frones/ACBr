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

unit ACBrLibCTeStaticImport;

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

function CTE_Inicializar(const eArqConfig, eChaveCrypt: PAnsiChar): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;
function CTE_Finalizar: Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_Nome(const sNome: PAnsiChar; var esTamanho: Integer): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_Versao(const sVersao: PAnsiChar; var esTamanho: Integer): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function NFE_OpenSSLInfo(const sOpenSSLInfo: PAnsiChar; var esTamanho: Integer): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_UltimoRetorno(const sMensagem: PAnsiChar; var esTamanho: Integer): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_ConfigLer(const eArqConfig: PAnsiChar): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_ConfigGravar(const eArqConfig: PAnsiChar): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_ConfigLerValor(const eSessao, eChave: PAnsiChar; sValor: PAnsiChar;
  var esTamanho: Integer): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_ConfigGravarValor(const eSessao, eChave, eValor: PAnsiChar): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_CarregarXML(const eArquivoOuXML: PAnsiChar): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_CarregarINI(const eArquivoOuINI: PAnsiChar): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_ObterXml(AIndex: Integer; const sResposta: PAnsiChar;
  var esTamanho: Integer): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_GravarXml(AIndex: Integer;
  const eNomeArquivo, ePathArquivo: PAnsiChar): Integer;
      {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_CarregarEventoXML(const eArquivoOuXML: PAnsiChar): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_CarregarEventoINI(const eArquivoOuINI: PAnsiChar): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_LimparLista: Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_LimparListaEventos: Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_Assinar: Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_Validar: Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_ValidarRegrasdeNegocios(const sResposta: PAnsiChar;
  var esTamanho: Integer): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_VerificarAssinatura(const sResposta: PAnsiChar;
  var esTamanho: Integer): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_StatusServico(const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_Consultar(const eChaveOuCTe: PAnsiChar; const sResposta: PAnsiChar;
  var esTamanho: Integer): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_Inutilizar(const ACNPJ, AJustificativa: PAnsiChar;
  Ano, Modelo, Serie, NumeroInicial, NumeroFinal: integer;
  const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_Enviar(ALote: integer; Imprimir: boolean; const sResposta: PAnsiChar;
  var esTamanho: Integer): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_ConsultarRecibo(ARecibo: PAnsiChar; const sResposta: PAnsiChar;
  var esTamanho: Integer): Integer;
      {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_Cancelar(const eChave, eJustificativa, eCNPJ: PAnsiChar;
  ALote: integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_EnviarEvento(idLote: integer; const sResposta: PAnsiChar;
  var esTamanho: Integer): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_ConsultaCadastro(cUF, nDocumento: PAnsiChar; nIE: boolean;
  const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
      {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_DistribuicaoDFePorUltNSU(const AcUFAutor: integer;
  eCNPJCPF, eultNSU: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_DistribuicaoDFePorNSU(const AcUFAutor: integer;
  eCNPJCPF, eNSU: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_DistribuicaoDFePorChave(const AcUFAutor: integer;
  eCNPJCPF, echCTe: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_EnviarEmail(const ePara, eChaveCTe: PAnsiChar; const AEnviaPDF: boolean;
  const eAssunto, eCC, eAnexos, eMensagem: PAnsiChar): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_EnviarEmailEvento(const ePara, eChaveEvento, eChaveCTe: PAnsiChar;
  const AEnviaPDF: boolean; const eAssunto, eCC, eAnexos, eMensagem: PAnsiChar): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_Imprimir(const cImpressora: PAnsiChar; nNumCopias: integer;
  const cProtocolo, bMostrarPreview: PAnsiChar): Integer;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_ImprimirPDF: Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_SalvarPDF(const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_ImprimirEvento(const eArquivoXmlCTe, eArquivoXmlEvento: PAnsiChar): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_ImprimirEventoPDF(
  const eArquivoXmlCTe, eArquivoXmlEvento: PAnsiChar): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_SalvarEventoPDF(const eArquivoXmlCTe, eArquivoXmlEvento: PAnsiChar; sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_ImprimirInutilizacao(const eArquivoXml: PAnsiChar): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

function CTE_ImprimirInutilizacaoPDF(const eArquivoXml: PAnsiChar): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrCTeLIBName;

implementation

end.
