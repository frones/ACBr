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

library ACBrLibNFe;

uses
  {$IFDEF MT}
   {$IFDEF UNIX}
    cthreads,
   {$ENDIF}
  {$ENDIF}
  Interfaces, sysutils, Classes, Forms, ACBrLibConfig,
  ACBrLibComum, ACBrLibConsts, ACBrLibNFeConfig, ACBrLibResposta,
  {$IFDEF MT}ACBrLibNFeMT{$ELSE}ACBrLibNFeST{$ENDIF},
  DFeReportConfig, ACBrLibNFeRespostas;

{$R *.res}

{$IFDEF DEBUG}
var
   HeapTraceFile : String ;
{$ENDIF}

exports
  // Importadas de ACBrLibComum
  NFE_Inicializar,
  NFE_Finalizar,
  NFE_Nome,
  NFE_Versao,
  NFE_UltimoRetorno,
  NFE_ConfigImportar,
  NFE_ConfigExportar,
  NFE_ConfigLer,
  NFE_ConfigGravar,
  NFE_ConfigLerValor,
  NFE_ConfigGravarValor,

  // Servicos
  NFE_StatusServico,
  NFE_Inutilizar,
  NFE_Enviar,
  NFE_ConsultarRecibo,
  NFE_Consultar,
  NFE_Cancelar,
  NFE_EnviarEvento,
  NFE_ConsultaCadastro,
  NFE_DistribuicaoDFePorUltNSU,
  NFE_DistribuicaoDFe,
  NFE_DistribuicaoDFePorNSU,
  NFE_DistribuicaoDFePorChave,
  NFE_EnviarEmail,
  NFE_EnviarEmailEvento,
  NFE_Imprimir,
  NFE_ImprimirPDF,
  NFE_SalvarPDF,
  NFE_ImprimirEvento,
  NFE_ImprimirEventoPDF,
  NFE_SalvarEventoPDF,
  NFE_ImprimirInutilizacao,
  NFE_ImprimirInutilizacaoPDF,
  NFE_SalvarInutilizacaoPDF,

  // Arquivos
  NFE_CarregarXML,
  NFE_CarregarINI,
  NFE_ObterXml,
  NFE_GravarXml,
  NFE_ObterIni,
  NFE_GravarIni,
  NFE_CarregarEventoXML,
  NFE_CarregarEventoINI,
  NFE_LimparLista,
  NFE_LimparListaEventos,
  NFE_Assinar,
  NFE_Validar,
  NFE_ValidarRegrasdeNegocios,
  NFE_VerificarAssinatura,
  NFE_GerarChave,
  NFE_ObterCertificados,
  NFE_GetPath,
  NFE_GetPathEvento;

begin
  {$IFDEF DEBUG}
   HeapTraceFile := ExtractFilePath(ParamStr(0))+ 'heaptrclog.trc' ;
   DeleteFile( HeapTraceFile );
   SetHeapTraceOutput( HeapTraceFile );
  {$ENDIF}

  MainThreadID := GetCurrentThreadId();
  Application.Initialize;
end.
