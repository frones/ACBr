{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2018 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:          Rafael Teno Dias                       }
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

library ACBrLibPosPrinter;

uses
  Interfaces, printer4lazarus, sysutils, Classes,
  ACBrLibConfig, ACBrLibComum,
  {$IFDEF MT}ACBrLibPosPrinterMT{$ELSE}ACBrLibPosPrinterST{$ENDIF},
  ACBrLibPosPrinterConfig, ACBrLibPosPrinterDataModule;

{$R *.res}

{$IFDEF DEBUG}
var
   HeapTraceFile : String ;
{$ENDIF}

exports
  // Importadas de ACBrLibComum
  POS_Inicializar,
  POS_Finalizar,
  POS_Inicializada,
  POS_Nome,
  POS_Versao,
  POS_OpenSSLInfo,
  POS_UltimoRetorno,
  POS_ConfigImportar,
  POS_ConfigExportar,
  POS_ConfigLer,
  POS_ConfigGravar,
  POS_ConfigLerValor,
  POS_ConfigGravarValor,

  //Ativar
  POS_Ativar,
  POS_Desativar,

  //Comandos de impressão
  POS_Imprimir,
  POS_ImprimirLinha,
  POS_ImprimirCmd,
  POS_ImprimirTags,
  POS_ImprimirImagemArquivo,
  POS_ImprimirLogo,
  POS_ImprimirCheque,
  POS_ImprimirTextoCheque,

  //Diversos
  POS_TxRx,
  POS_Zerar,
  POS_InicializarPos,
  POS_Reset,
  POS_PularLinhas,
  POS_CortarPapel,
  POS_AbrirGaveta,
  POS_LerInfoImpressora,
  POS_LerStatusImpressora,
  POS_LerStatusImpressoraFormatado,
  POS_RetornarTags,
  POS_AcharPortas,
  POS_GravarLogoArquivo,
  POS_ApagarLogo,
  POS_LeituraCheque,
  POS_LerCMC7,
  POS_EjetarCheque,
  POS_PodeLerDaPorta,
  POS_LerCaracteristicas;

begin
  {$IFDEF DEBUG}
   HeapTraceFile := ExtractFilePath(ParamStr(0))+ 'heaptrclog.trc' ;
   DeleteFile( HeapTraceFile );
   SetHeapTraceOutput( HeapTraceFile );
  {$ENDIF}

  MainThreadID := GetCurrentThreadId();
end.

