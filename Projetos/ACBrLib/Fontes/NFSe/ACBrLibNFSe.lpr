{*******************************************************************************}
{ Projeto: ACBrLib                                                              }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa-  }
{ mentos de Automação Comercial utilizados no Brasil                            }
{                                                                               }
{ Direitos Autorais Reservados (c) 2018 Daniel Simoes de Almeida                }
{                                                                               }
{ Colaboradores nesse arquivo: Antonio Carlos Junior                            }
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

library ACBrLibNFSe;

uses
  Interfaces, sysutils, Classes, Forms,
  {$IFDEF MT}ACBrLibNFSeMT{$ELSE}ACBrLibNFSeST{$ENDIF},
  DFeReportConfig, ACBrLibNFSeRespostas, ACBrLibNFSeBase, ACBrLibNFSeDataModule;

{$R *.res}

{$IFDEF DEBUG}
var
   HeapTraceFile : String ;
{$ENDIF}

exports
  // Importadas de ACBrLibComum
  NFSE_Inicializar,
  NFSE_Finalizar,
  NFSE_Nome,
  NFSE_Versao,
  NFSE_UltimoRetorno,
  NFSE_ConfigImportar,
  NFSE_ConfigExportar,
  NFSE_ConfigLer,
  NFSE_ConfigGravar,
  NFSE_ConfigLerValor,
  NFSE_ConfigGravarValor,

  // Arquivos
  NFSE_CarregarXML,
  NFSE_CarregarINI,
  NFSE_ObterXml,
  NFSE_GravarXml,
  NFSE_ObterIni,
  NFSE_GravarIni,
  NFSE_LimparLista,
  NFSE_ObterCertificados,

  // Servicos
  NFSE_Emitir,
  NFSE_Cancelar,
  NFSE_SubstituirNFSe,
  NFSE_LinkNFSe,
  NFSE_GerarLote,
  NFSE_GerarToken,
  NFSE_ConsultarSituacao,
  NFSE_ConsultarLoteRps,
  NFSE_ConsultarNFSePorRps,
  NFSE_ConsultarNFSePorNumero,
  NFSE_ConsultarNFSePorPeriodo,
  NFSE_ConsultarNFSePorFaixa,
  NFSE_ConsultarNFSeGenerico,
  NFSE_EnviarEmail,
  NFSE_Imprimir,
  NFSE_ImprimirPDF,

  // Servico Prestado
  NFSE_ConsultarNFSeServicoPrestadoPorNumero,
  NFSE_ConsultarNFSeServicoPrestadoPorPeriodo,
  NFSE_ConsultarNFSeServicoPrestadoPorTomador,
  NFSE_ConsultarNFSeServicoPrestadoPorIntermediario,

  // Servico Tomado
  NFSE_ConsultarNFSeServicoTomadoPorNumero,
  NFSE_ConsultarNFSeServicoTomadoPorPrestador,
  NFSE_ConsultarNFSeServicoTomadoPorTomador,
  NFSE_ConsultarNFSeServicoTomadoPorPeriodo,
  NFSE_ConsultarNFSeServicoTomadoPorIntermediario,

  //Padrao Nacional
  NFSE_EnviarEvento,
  NFSE_ConsultarDPSPorChave,
  NFSE_ConsultarNFSePorChave,
  NFSE_ConsultarEvento,
  NFSE_ConsultarDFe,
  NFSE_ObterDANFSE,
  NFSE_ConsultarParametros;

begin
  {$IFDEF DEBUG}
   HeapTraceFile := ExtractFilePath(ParamStr(0))+ 'heaptrclog.trc' ;
   DeleteFile( HeapTraceFile );
   SetHeapTraceOutput( HeapTraceFile );
  {$ENDIF}

  MainThreadID := GetCurrentThreadId();
  Application.Initialize;
end.
