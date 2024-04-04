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

library ACBrLibBoleto;

uses
  Interfaces, Forms,
  sysutils, Classes, ACBrLibBoletoDataModule,
  ACBrLibConfig, ACBrLibResposta,
  ACBrLibComum, ACBrLibConsts, ACBrLibDataModule,
  {$IFDEF MT}ACBrLibBoletoMT{$ELSE}ACBrLibBoletoST{$ENDIF},
  ACBrLibBoletoConsts, ACBrLibBoletoConfig, ACBrLibBoletoRespostas;

{$R *.res}

{$IFDEF DEBUG}
var
   HeapTraceFile : String ;
{$ENDIF}

exports
  Boleto_Inicializar,
  Boleto_Finalizar,
  Boleto_Nome,
  Boleto_Versao,
  Boleto_OpenSSLInfo,
  Boleto_UltimoRetorno,
  Boleto_ConfigImportar,
  Boleto_ConfigExportar,
  Boleto_ConfigLer,
  Boleto_ConfigGravar,
  Boleto_ConfigLerValor,
  Boleto_ConfigGravarValor,

  Boleto_ConfigurarDados,
  Boleto_IncluirTitulos,
  Boleto_LimparLista,
  Boleto_TotalTitulosLista,
  Boleto_Imprimir,
  Boleto_ImprimirBoleto,
  Boleto_GerarPDF,
  Boleto_SalvarPDF,
  Boleto_GerarPDFBoleto,
  Boleto_SalvarPDFBoleto,
  Boleto_GerarHTML,
  Boleto_GerarRemessa,
  Boleto_LerRetorno,
  Boleto_ObterRetorno,
  Boleto_EnviarEmail,
  Boleto_EnviarEmailBoleto,
  Boleto_SetDiretorioArquivo,
  Boleto_ListaBancos,
  Boleto_ListaCaractTitulo,
  Boleto_ListaOcorrencias,
  Boleto_ListaOcorrenciasEX,
  Boleto_TamNossoNumero,
  Boleto_CodigosMoraAceitos,
  Boleto_SelecionaBanco,
  Boleto_MontarNossoNumero,
  Boleto_RetornaLinhaDigitavel,
  Boleto_RetornaCodigoBarras,
  Boleto_EnviarBoleto,
  Boleto_ConsultarTitulosPorPeriodo;

begin
  {$IFDEF DEBUG}
   HeapTraceFile := ExtractFilePath(ParamStr(0))+ 'heaptrclog.trc' ;
   DeleteFile( HeapTraceFile );
   SetHeapTraceOutput( HeapTraceFile );
  {$ENDIF}

  MainThreadID := GetCurrentThreadId();
  Application.Initialize;
end.

