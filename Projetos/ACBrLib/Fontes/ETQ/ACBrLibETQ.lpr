{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
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

{$I ACBr.inc}

library ACBrLibETQ;

uses
  Interfaces, sysutils, Classes,
  ACBrLibConfig, ACBrLibComum,
  {$IFDEF MT}ACBrLibETQMT{$ELSE}ACBrLibETQST{$ENDIF},
  ACBrLibETQConfig, ACBrLibETQDataModule;

{$R *.res}

{$IFDEF DEBUG}
var
   HeapTraceFile: String;
{$ENDIF}

exports
  // Importadas de ACBrLibComum
  ETQ_Inicializar,
  ETQ_Finalizar,
  ETQ_Nome,
  ETQ_Versao,
  ETQ_UltimoRetorno,
  ETQ_ConfigImportar,
  ETQ_ConfigExportar,
  ETQ_ConfigLer,
  ETQ_ConfigGravar,
  ETQ_ConfigLerValor,
  ETQ_ConfigGravarValor,

  // Diversos
  ETQ_Ativar,
  ETQ_Desativar,
  ETQ_IniciarEtiqueta,
  ETQ_FinalizarEtiqueta,
  ETQ_CarregarImagem,

  // Impressão
  ETQ_Imprimir,
  ETQ_GerarStream,
  ETQ_ImprimirTexto,
  ETQ_ImprimirTextoStr,
  ETQ_ImprimirBarras,
  ETQ_ImprimirLinha,
  ETQ_ImprimirCaixa,
  ETQ_ImprimirImagem,
  ETQ_ImprimirQRCode,
  ETQ_ComandoGravaRFIDASCII,
  ETQ_ComandoGravaRFIDHexaDecimal;

begin
  {$IFDEF DEBUG}
   HeapTraceFile := ExtractFilePath(ParamStr(0))+ 'heaptrclog.trc';
   DeleteFile( HeapTraceFile );
   SetHeapTraceOutput( HeapTraceFile );
  {$ENDIF}

  MainThreadID := GetCurrentThreadId();
end.

