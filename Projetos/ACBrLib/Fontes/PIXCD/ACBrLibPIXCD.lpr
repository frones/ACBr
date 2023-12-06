{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Antonio Carlos Junior                           }
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

library ACBrLibPIXCD;

uses
  Interfaces, Forms, sysutils, Classes,
  {$IFDEF MT} ACBrLibPIXCDMT{$ELSE}ACBrLibPIXCDST{$ENDIF},
  ACBrLibPIXCDDataModule, ACBrLibPIXCDConfig, ACBrLibPIXCDBase, ACBrLibConfig,
  ACBrLibResposta, ACBrLibComum, ACBrLibConsts, ACBrLibDataModule;

{$R *.res}

{$IFDEF DEBUG}
var
   HeapTraceFile: String;
{$ENDIF}

exports
  //Importadas ACBrLibComum
  PIXCD_Inicializar,
  PIXCD_Finalizar,
  PIXCD_Nome,
  PIXCD_Versao,
  PIXCD_UltimoRetorno,
  PIXCD_ConfigImportar,
  PIXCD_ConfigExportar,
  PIXCD_ConfigLer,
  PIXCD_ConfigGravar,
  PIXCD_ConfigLerValor,
  PIXCD_ConfigGravarValor,

  //EndPoint /pix
  PIXCD_ConsultarPix,
  PIXCD_ConsultarPixRecebidos,
  PIXCD_SolicitarDevolucaoPix,
  PIXCD_ConsultarDevolucaoPix,

  //EndPoint /cob
  PIXCD_CriarCobrancaImediata,
  PIXCD_ConsultarCobrancaImediata,
  PIXCD_RevisarCobrancaImediata,

  //EndPoint /cobv
  PIXCD_CriarCobranca,
  PIXCD_RevisarCobranca,
  PIXCD_ConsultarCobranca,

  //PIXCD
  PIXCD_GerarQRCodeEstatico;

begin
  {$IFDEF DEBUG}
   HeapTraceFile := ExtractFilePath(ParamStr(0))+ 'heaptrclog.trc';
   DeleteFile( HeapTraceFile );
   SetHeapTraceOutput( HeapTraceFile );
  {$ENDIF}

  MainThreadID := GetCurrentThreadId();
end.

