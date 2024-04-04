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

unit ACBrLibBoletoStaticImportMT;

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
function Boleto_Inicializar(var libHandle: TLibHandle; const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_Finalizar(const libHandle: TLibHandle): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;
{%endregion}

{%region Versao/Retorno}
function Boleto_Nome(const libHandle: TLibHandle; const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_Versao(const libHandle: TLibHandle; const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_OpenSSLInfo(const libHandle: TLibHandle; const sOpenSSLInfo: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_UltimoRetorno(const libHandle: TLibHandle; const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;
{%endregion}

{%region Ler/Gravar Config }
function Boleto_ConfigLer(const libHandle: TLibHandle; const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_ConfigGravar(const libHandle: TLibHandle; const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_ConfigLerValor(const libHandle: TLibHandle; const eSessao, eChave: PChar; sValor: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_ConfigGravarValor(const libHandle: TLibHandle; const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;
{%endregion}

{%region Boleto}
function Boleto_ConfigurarDados(const libHandle: TLibHandle; eArquivoIni: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_IncluirTitulos(const libHandle: TLibHandle; eArquivoIni, eTpSaida: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_LimparLista(const libHandle: TLibHandle): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_TotalTitulosLista(const libHandle: TLibHandle; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_Imprimir(const libHandle: TLibHandle; NomeImpressora: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_GerarPDF(const libHandle: TLibHandle): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_GerarHTML(const libHandle: TLibHandle): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_GerarRemessa(const libHandle: TLibHandle; eDir: PChar; eNumArquivo: longInt; eNomeArq: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_LerRetorno(const libHandle: TLibHandle; eDir, eNomeArq: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_ObterRetorno(const libHandle: TLibHandle; eDir, eNomeArq: PChar; const sResposta: PChar;
  var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_EnviarEmail(const libHandle: TLibHandle; ePara, eAssunto, eMensagem, eCC: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_SetDiretorioArquivo(const libHandle: TLibHandle; eDir, eArq: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_ListaBancos(const libHandle: TLibHandle; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_ListaCaractTitulo(const libHandle: TLibHandle; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_ListaOcorrencias(const libHandle: TLibHandle; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_ListaOcorrenciasEX(const libHandle: TLibHandle; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_TamNossoNumero(const libHandle: TLibHandle; eCarteira, enossoNumero, eConvenio: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_CodigosMoraAceitos(const libHandle: TLibHandle; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_SelecionaBanco(const libHandle: TLibHandle; eCodBanco: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_MontarNossoNumero(const libHandle: TLibHandle; eIndice: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_RetornaLinhaDigitavel(const libHandle: TLibHandle; eIndice: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_RetornaCodigoBarras(const libHandle: TLibHandle; eIndice: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_EnviarBoleto(const libHandle: TLibHandle; eCodigoOperacao: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

function Boleto_ConsultarTitulosPorPeriodo(const libHandle: TLibHandle; eArquivoIni: PChar; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrBoletoLIBName;

{%endregion}


implementation

end.

