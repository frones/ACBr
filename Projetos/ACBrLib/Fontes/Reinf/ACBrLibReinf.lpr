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

library ACBrLibReinf;

uses
  Interfaces, Forms,
  sysutils, Classes, ACBrLibReinfDataModule,
  {$IFDEF MT} ACBrLibReinfMT {$ELSE} ACBrLibReinfST{$ENDIF},
  ACBrLibConfig, ACBrLibResposta, ACBrLibComum, ACBrLibConsts,
  ACBrLibDataModule, ACBrLibReinfConfig, ACBrLibReinfBase;

{$R *.res}

{$IFDEF DEBUG}
var
   HeapTraceFile: String;
{$ENDIF}

exports
  // Importadas de ACBrLibComum
  Reinf_Inicializar,
  Reinf_Finalizar,
  Reinf_Nome,
  Reinf_Versao,
  Reinf_UltimoRetorno,
  Reinf_ConfigImportar,
  Reinf_ConfigExportar,
  Reinf_ConfigLer,
  Reinf_ConfigGravar,
  Reinf_ConfigLerValor,
  Reinf_ConfigGravarValor,

  // Reinf
  Reinf_CriarEventoReinf,
  Reinf_EnviarReinf,
  Reinf_ConsultarReinf,
  Reinf_ConsultarReciboReinf,
  Reinf_CriarEnviarReinf,
  Reinf_LimparReinf,
  Reinf_CarregarXMLEventoReinf,
  Reinf_SetIdContribuinte,
  Reinf_SetIDTransmissor,
  Reinf_SetTipoContribuinte,
  Reinf_SetVersaoDF,
  Reinf_ObterCertificados,
  Reinf_Validar;

begin
  {$IFDEF DEBUG}
   HeapTraceFile := ExtractFilePath(ParamStr(0))+ 'heaptrclog.trc';
   DeleteFile( HeapTraceFile );
   SetHeapTraceOutput( HeapTraceFile );
  {$ENDIF}

  MainThreadID := GetCurrentThreadId();
  Application.Initialize;
end.

