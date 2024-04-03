{*******************************************************************************}
{ Projeto: Componentes ACBr                                                     }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa-  }
{ mentos de Automação Comercial utilizados no Brasil                            }
{                                                                               }
{ Direitos Autorais Reservados (c) 2018 Daniel Simoes de Almeida                }
{                                                                               }
{ Colaboradores nesse arquivo: Rafael Teno Dias                                 }
{                                                                               }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr     }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr       }
{                                                                               }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la  }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela   }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério)  }
{ qualquer versão posterior.                                                    }
{                                                                               }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM    }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU       }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor }
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)               }
{                                                                               }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto }
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,   }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.           }
{ Você também pode obter uma copia da licença em:                               }
{ http://www.opensource.org/licenses/gpl-license.php                            }
{                                                                               }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br }
{        Rua Cel.Aureliano de Camargo, 963 - Tatuí - SP - 18270-170             }
{                                                                               }
{*******************************************************************************}

{$I ACBr.inc}

library ACBrLibSat;

uses
  Interfaces, sysutils, Classes, Forms,
  {$IFDEF MT}ACBrLibSATMT{$ELSE}ACBrLibSATST{$ENDIF},
  ACBrLibComum;

{$R *.res}

{$IFDEF DEBUG}
var
   HeapTraceFile : String ;
{$ENDIF}

exports
  // Importadas de ACBrLibComum
  SAT_Inicializar,
  SAT_Finalizar,
  SAT_Nome,
  SAT_Versao,
  SAT_UltimoRetorno,
  SAT_ConfigImportar,
  SAT_ConfigExportar,
  SAT_ConfigLer,
  SAT_ConfigGravar,
  SAT_ConfigLerValor,
  SAT_ConfigGravarValor,

  SAT_InicializarSAT,
  SAT_DesInicializar,

  SAT_AtivarSAT,
  SAT_AssociarAssinatura,
  SAT_BloquearSAT,
  SAT_DesbloquearSAT,
  SAT_TrocarCodigoDeAtivacao,
  SAT_ConsultarSAT,
  SAT_ConsultarUltimaSessaoFiscal,
  SAT_ConsultarStatusOperacional,
  SAT_ConsultarNumeroSessao,
  SAT_SetNumeroSessao,
  SAT_AtualizarSoftwareSAT,
  SAT_ComunicarCertificadoICPBRASIL,
  SAT_ExtrairLogs,
  SAT_TesteFimAFim,
  SAT_GerarAssinaturaSAT,

  SAT_CriarCFe,
  SAT_CriarEnviarCFe,
  SAT_ValidarCFe,
  SAT_EnviarCFe,
  SAT_CancelarCFe,

  SAT_ImprimirExtratoVenda,
  SAT_ImprimirExtratoResumido,
  SAT_ImprimirExtratoCancelamento,
  SAT_SalvarPDF,
  SAT_GerarPDFExtratoVenda,
  SAT_GerarPDFCancelamento,
  SAT_GerarImpressaoFiscalMFe,
  SAT_EnviarEmail;

begin
  {$IFDEF DEBUG}
   HeapTraceFile := ExtractFilePath(ParamStr(0))+ 'heaptrclog.trc' ;
   DeleteFile( HeapTraceFile );
   SetHeapTraceOutput( HeapTraceFile );
  {$ENDIF}

  MainThreadID := GetCurrentThreadId();
  Application.Initialize;
end.

