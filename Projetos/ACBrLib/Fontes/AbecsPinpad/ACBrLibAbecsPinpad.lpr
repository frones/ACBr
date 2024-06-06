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

library ACBrLibAbecsPinpad;

uses
  Interfaces, sysutils, Classes,
  ACBrLibConfig, ACBrLibComum,
  {$IFDEF MT}ACBrLibAbecsPinpadMT{$ELSE}ACBrLibAbecsPinpadST{$ENDIF},
  ACBrLibAbecsPinpadConfig, ACBrLibAbecsPinpadDataModule,
  ACBrLibAbecsPinpadRespostas, ACBrLibAbecsPinpadConsts;

{$R *.res}

{$IFDEF DEBUG}
var
   HeapTraceFile: String;
{$ENDIF}

exports
  //Importadas de ACBrLibComum
  AbecsPinpad_Inicializar,
  AbecsPinpad_Finalizar,
  AbecsPinpad_Nome,
  AbecsPinpad_Versao,
  AbecsPinpad_OpenSSLInfo,
  AbecsPinpad_UltimoRetorno,
  AbecsPinpad_ConfigImportar,
  AbecsPinpad_ConfigExportar,
  AbecsPinpad_ConfigLer,
  AbecsPinpad_ConfigGravar,
  AbecsPinpad_ConfigLerValor,
  AbecsPinpad_ConfigGravarValor,

  //Control Commands
  AbecsPinpad_Ativar,
  AbecsPinpad_Desativar,
  AbecsPinpad_OPN,
  AbecsPinpad_CLO,
  AbecsPinpad_CLX,
  AbecsPinpad_GIX,
  AbecsPinpad_GIN,
  AbecsPinpad_PinPadCapabilities,

  //Basic Commands
  AbecsPinpad_DSP,
  AbecsPinpad_DEX,
  AbecsPinpad_GKY,
  AbecsPinpad_RMC,
  AbecsPinpad_GCD,
  AbecsPinpad_CEX,
  AbecsPinpad_MNU,

  //Multimida Commands
  AbecsPinpad_LoadMedia,
  AbecsPinpad_LMF,
  AbecsPinpad_DSI,
  AbecsPinpad_DMF;

begin
  {$IFDEF DEBUG}
   HeapTraceFile := ExtractFilePath(ParamStr(0))+ 'heaptrclog.trc';
   DeleteFile( HeapTraceFile );
   SetHeapTraceOutput( HeapTraceFile );
  {$ENDIF}

  MainThreadID := GetCurrentThreadId();
end.

