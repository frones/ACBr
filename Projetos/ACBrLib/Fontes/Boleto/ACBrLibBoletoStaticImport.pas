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

unit ACBrLibBoletoStaticImport;

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
  CACBrBoletoLIBName = 'ACBrBoleto64.dll';
  {$Else}
  CACBrBoletoLIBName = 'ACBrBoleto32.dll';
  {$EndIf}
 {$Else}
  {$IfDef CPU64}
  CACBrBoletoLIBName = 'libacbrboleto64.so';
  {$Else}
  CACBrBoletoLIBName = 'libacbrboleto32.so';
  {$EndIf}
 {$EndIf}

{$I ACBrLibErros.inc}

{%region Constructor/Destructor}
function Boleto_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;
{%endregion}

{%region Versao/Retorno}
function Boleto_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_OpenSSLInfo(const sOpenSSLInfo: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;
{%endregion}

{%region Ler/Gravar Config }
function Boleto_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;
{%endregion}

{%region Boleto}
function Boleto_ConfigurarDados(eArquivoIni: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_IncluirTitulos(eArquivoIni, eTpSaida: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_LimparLista: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_TotalTitulosLista: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_Imprimir(eNomeImpressora: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_GerarPDF: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_GerarHTML: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_GerarRemessa(eDir: PChar; eNumArquivo: longInt; eNomeArq: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_GerarRemessaStream(eDir: PChar; eNumArquivo: longInt; eNomeArq: PChar; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_LerRetorno(eDir, eNomeArq: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_LerRetornoStream(const ARetornoBase64: PChar; const sResposta : PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_ObterRetorno(eDir, eNomeArq: PChar; const sResposta: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_EnviarEmail(ePara, eAssunto, eMensagem, eCC: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_SetDiretorioArquivo(eDir, eArq: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_ListaBancos(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_ListaCaractTitulo(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_ListaOcorrencias(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_ListaOcorrenciasEX(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_TamNossoNumero(eCarteira, enossoNumero, eConvenio: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_CodigosMoraAceitos(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_SelecionaBanco(eCodBanco: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_MontarNossoNumero(eIndice: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_RetornaLinhaDigitavel(eIndice: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_RetornaCodigoBarras(eIndice: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_EnviarBoleto(eCodigoOperacao: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_ConsultarTitulosPorPeriodo(eArquivoIni: PChar; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

{%endregion}


implementation

end.
