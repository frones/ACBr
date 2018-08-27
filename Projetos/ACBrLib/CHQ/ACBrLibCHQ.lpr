{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }

{ Direitos Autorais Reservados (c) 2018 Daniel Simoes de Almeida               }

{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }

{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }

{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }

{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }

{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/gpl-license.php                           }

{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{        Rua Cel.Aureliano de Camargo, 973 - Tatuí - SP - 18270-170            }

{******************************************************************************}

{$I ACBr.inc}

library ACBrLibCHQ;

uses
  Interfaces, sysutils, Classes,
  ACBrLibConfig, ACBrLibComum,
  ACBrLibCHQClass, ACBrLibCHQConfig, ACBrLibCHQDataModule;

{$R *.res}

{$IFDEF DEBUG}
var
   HeapTraceFile: String;
{$ENDIF}

exports
  // Importadas de ACBrLibComum
  CHQ_Inicializar,
  CHQ_Finalizar,
  CHQ_Nome,
  CHQ_Versao,
  CHQ_UltimoRetorno,
  CHQ_ConfigLer,
  CHQ_ConfigGravar,
  CHQ_ConfigLerValor,
  CHQ_ConfigGravarValor,

  // Cheque
  CHQ_Ativar,
  CHQ_Desativar,
  CHQ_ImprimirCheque,
  CHQ_ImprimirLinha,
  CHQ_ImprimirVerso,
  CHQ_TravarCheque,
  CHQ_DestravarCheque,
  CHQ_SetBanco,
  CHQ_SetValor,
  CHQ_SetData,
  CHQ_SetCidade,
  CHQ_SetFavorecido,
  CHQ_SetObservacao,
  CHQ_SetBomPara;

begin
  {$IFDEF DEBUG}
   HeapTraceFile := ExtractFilePath(ParamStr(0))+ 'heaptrclog.trc';
   DeleteFile( HeapTraceFile );
   SetHeapTraceOutput( HeapTraceFile );
  {$ENDIF}

  pLibClass := TACBrLibCHQ; // Ajusta a classe a ser criada
  MainThreadID := GetCurrentThreadId();
end.

