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

library ACBrLibMDFe;

uses
  Interfaces, sysutils, Classes, Forms, ACBrLibConfig,
  {$IFDEF MT}ACBrLibMDFeMT{$ELSE}ACBrLibMDFeST{$ENDIF},
  ACBrLibComum, ACBrLibConsts, ACBrLibMDFeConfig, ACBrLibResposta,
  ACBrLibMDFeRespostas, ACBrLibMDFeConsts;

{$R *.res}

{$IFDEF DEBUG}
var
   HeapTraceFile : String ;
{$ENDIF}

exports
  // Importadas de ACBrLibComum
  MDFE_Inicializar,
  MDFE_Finalizar,
  MDFE_Nome,
  MDFE_Versao,
  MDFE_OpenSSLInfo,
  MDFE_UltimoRetorno,
  MDFE_ConfigImportar,
  MDFE_ConfigExportar,
  MDFE_ConfigLer,
  MDFE_ConfigGravar,
  MDFE_ConfigLerValor,
  MDFE_ConfigGravarValor,

  // Servicos
  MDFE_StatusServico,
  MDFE_Enviar,
  MDFE_ConsultarRecibo,
  MDFE_Consultar,
  MDFE_Cancelar,
  MDFE_EnviarEvento,
  MDFE_EncerrarMDFe,
  MDFE_ConsultaMDFeNaoEnc,
  MDFE_DistribuicaoDFePorUltNSU,
  MDFE_DistribuicaoDFePorNSU,
  MDFE_DistribuicaoDFePorChave,
  MDFE_EnviarEmail,
  MDFE_EnviarEmailEvento,
  MDFE_Imprimir,
  MDFE_ImprimirPDF,
  MDFE_SalvarPDF,
  MDFE_ImprimirEvento,
  MDFE_ImprimirEventoPDF,
  MDFE_SalvarEventoPDF,

  // Arquivos
  MDFE_CarregarXML,
  MDFE_CarregarINI,
  MDFE_ObterXml,
  MDFE_GravarXml,
  MDFE_ObterIni,
  MDFE_GravarIni,
  MDFE_CarregarEventoXML,
  MDFE_CarregarEventoINI,
  MDFE_LimparLista,
  MDFE_LimparListaEventos,
  MDFE_Assinar,
  MDFE_Validar,
  MDFE_ValidarRegrasdeNegocios,
  MDFE_VerificarAssinatura,
  MDFE_GerarChave,
  MDFE_ObterCertificados,
  MDFE_GetPath,
  MDFE_GetPathEvento;

begin
  {$IFDEF DEBUG}
   HeapTraceFile := ExtractFilePath(ParamStr(0))+ 'heaptrclog.trc' ;
   DeleteFile( HeapTraceFile );
   SetHeapTraceOutput( HeapTraceFile );
  {$ENDIF}

  MainThreadID := GetCurrentThreadId();
  Application.Initialize;
end.


