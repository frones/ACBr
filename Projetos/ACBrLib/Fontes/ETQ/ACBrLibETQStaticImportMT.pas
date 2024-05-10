{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Rafael Teno Dias, Daniel Morais Infocotidiano   }
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

unit ACBrLibETQStaticImportMT;

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
  CACBrETQLIBName = 'ACBrETQ64.dll';
  {$Else}
  CACBrETQLIBName = 'ACBrETQ32.dll';
  {$EndIf}
 {$Else}
  {$IfDef CPU64}
  CACBrETQLIBName = 'libacbretq64.so';
  {$Else}
  CACBrETQLIBName = 'libacbretq32.so';

  {$EndIf}
 {$EndIf}

{$I ACBrLibErros.inc}

{%region Constructor/Destructor}
function ETQ_Inicializar(const libHandle: TLibHandle; const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrETQLIBName;

function ETQ_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrETQLIBName;
{%endregion}

{%region Versao/Retorno}
function ETQ_Nome(const libHandle: TLibHandle; const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrETQLIBName;

function ETQ_Versao(const libHandle: TLibHandle; const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrETQLIBName;

function ETQ_UltimoRetorno(const libHandle: TLibHandle; const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrETQLIBName;
{%endregion}

{%region Ler/Gravar Config }
function ETQ_ConfigLer(const libHandle: TLibHandle; const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrETQLIBName;

function ETQ_ConfigGravar(const libHandle: TLibHandle; const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrETQLIBName;

function ETQ_ConfigLerValor(const libHandle: TLibHandle; const eSessao, eChave: PChar; sValor: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrETQLIBName;

function ETQ_ConfigGravarValor(const libHandle: TLibHandle; const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrETQLIBName;
{%endregion}

{%region Diversos}
function ETQ_Ativar(const libHandle: TLibHandle): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrETQLIBName;
function ETQ_Desativar(const libHandle: TLibHandle): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrETQLIBName;
function ETQ_IniciarEtiqueta(const libHandle: TLibHandle): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrETQLIBName;
function ETQ_FinalizarEtiqueta(const libHandle: TLibHandle; const ACopias, AAvancoEtq: Integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrETQLIBName;
function ETQ_CarregarImagem(const libHandle: TLibHandle; const eArquivoImagem, eNomeImagem: PChar;
      Flipped: Boolean): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrETQLIBName;
{%endregion}

{%region Impressão}
function ETQ_Imprimir(const libHandle: TLibHandle; const ACopias, AAvancoEtq: Integer): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrETQLIBName;

function ETQ_GerarStream(const libHandle: TLibHandle; const ACopias, AAvancoEtq: Integer; const sResposta: PChar; var esTamanho: longint): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrETQLIBName;

function ETQ_ImprimirTexto(const libHandle: TLibHandle; const Orientacao, Fonte, MultiplicadorH,
            MultiplicadorV, Vertical, Horizontal: Integer; const eTexto: PChar;
            const SubFonte: Integer; const ImprimirReverso: Boolean): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrETQLIBName;
function ETQ_ImprimirBarras(const libHandle: TLibHandle; const Orientacao, TipoBarras, LarguraBarraLarga,
            LarguraBarraFina, Vertical, Horizontal: Integer;
     const eTexto: PChar; const AlturaCodBarras, ExibeCodigo: Integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrETQLIBName;
function ETQ_ImprimirLinha(const libHandle: TLibHandle; const Vertical, Horizontal, Largura, Altura: Integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrETQLIBName;
function ETQ_ImprimirCaixa(const libHandle: TLibHandle; const Vertical, Horizontal, Largura, Altura,
      EspessuraVertical, EspessuraHorizontal: Integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrETQLIBName;
function ETQ_ImprimirImagem(const libHandle: TLibHandle; const MultiplicadorImagem, Vertical, Horizontal: Integer;
      const eNomeImagem: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrETQLIBName;
{%endregion}

implementation

end.
