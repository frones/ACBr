{*******************************************************************************}
{ Projeto: ACBrLib                                                              }
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

library ACBrLibCTe;

uses
  Interfaces, sysutils, Classes, Forms, ACBrLibConfig,
  ACBrLibComum, ACBrLibConsts, ACBrLibCTeConfig, ACBrLibResposta,
  {$IFDEF MT}ACBrLibCTeMT{$ELSE}ACBrLibCTeST{$ENDIF},
  ACBrLibDistribuicaoDFe, ACBrLibCTeRespostas, ACBrLibCTeConsts;

{$R *.res}

{$IFDEF DEBUG}
var
   HeapTraceFile : String ;
{$ENDIF}

exports
  // Importadas de ACBrLibComum
  CTE_Inicializar,
  CTE_Finalizar,
  CTE_Nome,
  CTE_Versao,
  CTE_UltimoRetorno,
  CTE_ConfigImportar,
  CTE_ConfigExportar,
  CTE_ConfigLer,
  CTE_ConfigGravar,
  CTE_ConfigLerValor,
  CTE_ConfigGravarValor,

  // Servicos
  CTE_StatusServico,
  CTE_Inutilizar,
  CTE_Enviar,
  CTE_ConsultarRecibo,
  CTE_Consultar,
  CTE_Cancelar,
  CTE_EnviarEvento,
  CTE_ConsultaCadastro,
  CTE_DistribuicaoDFePorUltNSU,
  CTE_DistribuicaoDFe,
  CTE_DistribuicaoDFePorNSU,
  CTE_DistribuicaoDFePorChave,
  CTE_EnviarEmail,
  CTE_EnviarEmailEvento,
  CTE_Imprimir,
  CTE_ImprimirPDF,
  CTE_SalvarPDF,
  CTE_ImprimirEvento,
  CTE_ImprimirEventoPDF,
  CTE_SalvarEventoPDF,
  CTE_ImprimirInutilizacao,
  CTE_ImprimirInutilizacaoPDF,

  // Arquivos
  CTE_CarregarXML,
  CTE_CarregarINI,
  CTE_ObterXml,
  CTE_GravarXml,
  CTE_ObterIni,
  CTE_GravarIni,
  CTE_CarregarEventoXML,
  CTE_CarregarEventoINI,
  CTE_LimparLista,
  CTE_LimparListaEventos,
  CTE_Assinar,
  CTE_Validar,
  CTE_ValidarRegrasdeNegocios,
  CTE_VerificarAssinatura,
  CTE_GerarChave,
  CTE_ObterCertificados,
  CTE_GetPath,
  CTE_GetPathEvento;

begin
  {$IFDEF DEBUG}
   HeapTraceFile := ExtractFilePath(ParamStr(0))+ 'heaptrclog.trc' ;
   DeleteFile( HeapTraceFile );
   SetHeapTraceOutput( HeapTraceFile );
  {$ENDIF}

  MainThreadID := GetCurrentThreadId();
  Application.Initialize;
end.


