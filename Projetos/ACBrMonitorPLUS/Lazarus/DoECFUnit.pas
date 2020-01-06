{*******************************************************************************}
{ Projeto: ACBrMonitor                                                         }
{  Executavel multiplataforma que faz uso do conjunto de componentes ACBr para  }
{ criar uma interface de comunicação com equipamentos de automacao comercial.   }
{                                                                               }
{ Direitos Autorais Reservados (c) 2010 Daniel Simoes de Almeida                }
{                                                                               }
{ Colaboradores nesse arquivo:                                  }
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

{$mode objfpc}{$H+}

unit DoECFUnit;


interface

Uses Classes, TypInfo, SysUtils, CmdUnit, ACBrECF, ACBrDevice, ACBrECFClass,
  ACBrMonitorConfig, ACBrMonitorConsts, ACBrBlocoX;

type

{ TACBrObjetoECF }

TACBrObjetoECF = class(TACBrObjeto)
private
  fACBrECF: TACBrECF;
  fACBrBlocoX: TACBrBlocoX ;
  fFPG         : TACBrECFFormaPagamento ;
  fREL         : TACBrECFRelatorioGerencial;
  fICMS        : TACBrECFAliquota ;
  fCNF         : TACBrECFComprovanteNaoFiscal ;
  fFinalidade  : TACBrECFFinalizaArqMFD;
  fTipoDoc     : TACBrECFTipoDocumento;
public
  constructor Create(AConfig: TMonitorConfig; ACBrECF: TACBrECF; ACBrBlocoX: TACBrBlocoX); reintroduce;
  procedure Executar(ACmd: TACBrCmd); override;

  Function PegaAliquotas : String ;
  Function PegaRelatoriosGerenciais : String ;
  Function PegaTotaisRelatoriosGerenciais : String ;
  Function PegaTotaisAliquotas : String ;
  Function PegaFormasPagamento : String ;
  Function PegaTotaisFormasPagamento : String ;
  Function PegaComprovantesNaoFiscais : String ;
  Function PegaTotaisComprovantesNaoFiscais : String ;
  Function PegaUnidadesMedida : String ;

  Procedure StringToMemo( AString : AnsiString; Memo : TStringList );
  function AjustaNomeArquivoCmd(Params: Integer = 2) : String ;

  property ACBrECF: TACBrECF read fACBrECF;
  property ACBrBlocoX: TACBrBlocoX read fACBrBlocoX;
end;

{ TMetodoAchar }

TMetodoAchar = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoAtivar }

TMetodoAtivar = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoDesativar }

TMetodoDesativar = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoAtivo }

TMetodoAtivo = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoColunas }

TMetodoColunas = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoParamDescontoIssqn }

TMetodoParamDescontoIssqn = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoEnviaInfo }

TMetodoEnviaInfo = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoComandoEnviado }

TMetodoComandoEnviado = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoRespostaComando }

TMetodoRespostaComando = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoModeloStr }

TMetodoModeloStr = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoModelo }

TMetodoModelo = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoPorta }

TMetodoPorta = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoTimeOut }

TMetodoTimeOut = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSetTimeOut }

TMetodoSetTimeOut = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoIntervaloAposComando }

TMetodoIntervaloAposComando = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoDescricaoGrande }

TMetodoDescricaoGrande = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoGavetaSinalInvertido }

TMetodoGavetaSinalInvertido = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoIgnorarTagsFormatacao }

TMetodoIgnorarTagsFormatacao = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoControlePorta }

TMetodoControlePorta = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoOperador }

TMetodoOperador = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoMsgAguarde }

TMetodoMsgAguarde = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoMsgTrabalhando }

TMetodoMsgTrabalhando = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoMsgPoucoPapel }

TMetodoMsgPoucoPapel = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoExibeMensagem }

TMetodoExibeMensagem = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoBloqueiaMouseTeclado }

TMetodoBloqueiaMouseTeclado = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoLinhasEntreCupons }

TMetodoLinhasEntreCupons = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoPaginaDeCodigo }

TMetodoPaginaDeCodigo = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoMaxLinhasBuffer }

TMetodoMaxLinhasBuffer = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSetMaxLinhasBuffer }

TMetodoSetMaxLinhasBuffer = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoDataHora }

TMetodoDataHora = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoNumCupom }

TMetodoNumCupom = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoNumLoja }

TMetodoNumLoja = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoNumCRO }

TMetodoNumCRO = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoNumCCF }

TMetodoNumCCF = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoNumGRG }

TMetodoNumGRG = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoNumGNF }

TMetodoNumGNF = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoNumGNFC }

TMetodoNumGNFC = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoNumCDC }

TMetodoNumCDC = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoNumCFC }

TMetodoNumCFC = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoNumCCDC }

TMetodoNumCCDC = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoNumCFD }

TMetodoNumCFD = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoNumNCN }

TMetodoNumNCN = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoNumCRZ }

TMetodoNumCRZ = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoNumECF }

TMetodoNumECF = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoNumSerie }

TMetodoNumSerie = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoNumSerieMFD }

TMetodoNumSerieMFD = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoNumVersao }

TMetodoNumVersao = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoMFAdicional }

TMetodoMFAdicional = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoRFDID }

TMetodoRFDID = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoDataMovimento }

TMetodoDataMovimento = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoCNPJ }

TMetodoCNPJ = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoIE }

TMetodoIE = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoIM }

TMetodoIM = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoCliche }

TMetodoCliche = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoUsuarioAtual }

TMetodoUsuarioAtual = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoDataHoraSB }

TMetodoDataHoraSB = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoDataHoraUltimaReducaoZ }

TMetodoDataHoraUltimaReducaoZ = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoDecimaisQtd }

TMetodoDecimaisQtd = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoDecimaisPreco }

TMetodoDecimaisPreco = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSubModeloECF }

TMetodoSubModeloECF = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoPAF }

TMetodoPAF = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoNumCOOInicial }

TMetodoNumCOOInicial = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoVendaBruta }

TMetodoVendaBruta = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoGrandeTotal }

TMetodoGrandeTotal = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoTotalTroco }

TMetodoTotalTroco = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoTotalCancelamentos }

TMetodoTotalCancelamentos = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoTotalDescontos }

TMetodoTotalDescontos = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoTotalAcrescimos }

TMetodoTotalAcrescimos = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoTotalSubstituicaoTributaria }

TMetodoTotalSubstituicaoTributaria = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoTotalNaoTributado }

TMetodoTotalNaoTributado = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoTotalIsencao }

TMetodoTotalIsencao = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoTotalCancelamentosISSQN }

TMetodoTotalCancelamentosISSQN = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoTotalDescontosISSQN }

TMetodoTotalDescontosISSQN = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoTotalAcrescimosISSQN }

TMetodoTotalAcrescimosISSQN = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoTotalSubstituicaoTributariaISSQN }

TMetodoTotalSubstituicaoTributariaISSQN = class(TACBrMetodo)
public
  procedure Executar; override;
end;


{ TMetodoTotalNaoTributadoISSQN }

TMetodoTotalNaoTributadoISSQN = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoTotalIsencaoISSQN }

TMetodoTotalIsencaoISSQN = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoTotalNaoFiscal }

TMetodoTotalNaoFiscal = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoTotalCancelamentosOPNF }

TMetodoTotalCancelamentosOPNF = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoTotalDescontosOPNF }

TMetodoTotalDescontosOPNF = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoTotalAcrescimosOPNF }

TMetodoTotalAcrescimosOPNF = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoNumUltItem }

TMetodoNumUltItem  = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoDadosReducaoZ }

TMetodoDadosReducaoZ  = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoDadosUltimaReducaoZ }

TMetodoDadosUltimaReducaoZ  = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoNumReducoesZRestantes }

TMetodoNumReducoesZRestantes  = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoPegaAliquotas }

TMetodoPegaAliquotas  = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoCarregaAliquotas }

TMetodoCarregaAliquotas  = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoLerTotaisAliquota }

TMetodoLerTotaisAliquota  = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoProgramaAliquota }

TMetodoProgramaAliquota  = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoAchaICMSAliquota }

TMetodoAchaICMSAliquota  = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoFormasPagamento }

TMetodoFormasPagamento  = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoCarregaFormasPagamento }

TMetodoCarregaFormasPagamento  = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoLerTotaisFormaPagamento }

TMetodoLerTotaisFormaPagamento  = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoprogramaformapagamento }

TMetodoProgramaFormaPagamento  = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoachafpgdescricao }

TMetodoAchaFpgDescricao  = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodocomprovantesnaofiscais }

TMetodoComprovantesNaoFiscais  = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodocarregacomprovantesnaofiscais }

TMetodoCarregaComprovantesNaoFiscais  = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodolertotaiscomprovantenaofiscal }

TMetodoLerTotaisComprovanteNaoFiscal  = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoprogramacomprovantenaofiscal }

TMetodoProgramaComprovanteNaoFiscal  = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoachacnfdescricao }

TMetodoAchaCnfDescricao  = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodounidadesmedida }

TMetodoUnidadesMedida  = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodocarregaunidadesmedida }

TMetodoCarregaUnidadesMedida  = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoprogramaunidademedida }

TMetodoProgramaUnidadeMedida  = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoRelatoriosGerenciais }

TMetodoRelatoriosGerenciais  = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoCarregaRelatoriosGerenciais }

TMetodoCarregaRelatoriosGerenciais  = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoLerTotaisRelatoriosGerenciais }

TMetodoLerTotaisRelatoriosGerenciais  = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoProgramaRelatoriosGerenciais }

TMetodoProgramaRelatoriosGerenciais  = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoAchaRgDescricao }

TMetodoAchaRgDescricao  = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoTestaPodeAbrirCupom }

TMetodoTestaPodeAbrirCupom  = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoIdentificaOperador }

TMetodoIdentificaOperador  = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoIdentificaConsumidor }

TMetodoIdentificaConsumidor  = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoIdentificaPaf }

TMetodoIdentificaPaf  = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoAbreCupom }

TMetodoAbreCupom  = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoLegendaInmetroProximoItem }

TMetodoLegendaInmetroProximoItem  = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoVendeItem }

TMetodoVendeItem  = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoDescontoAcrescimoItemAnterior }

TMetodoDescontoAcrescimoItemAnterior  = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSubTotalizaCupom }

TMetodoSubTotalizaCupom  = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoEfetuaPagamento }

TMetodoEfetuaPagamento  = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoEstornaPagamento }

TMetodoEstornaPagamento  = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoFechaCupom }

TMetodoFechaCupom  = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoCancelaCupom }

TMetodoCancelaCupom  = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoCancelaItemVendido }

TMetodoCancelaItemVendido  = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoCancelaItemVendidoParcial }

TMetodoCancelaItemVendidoParcial = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoCancelaDescontoAcrescimoItem }

TMetodoCancelaDescontoAcrescimoItem = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoCancelaDescontoAcrescimoSubtotal }

TMetodoCancelaDescontoAcrescimoSubtotal = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSubTotal }

TMetodoSubTotal = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoTotalPago }

TMetodoTotalPago = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSangria }

TMetodoSangria = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSuprimento }

TMetodoSuprimento = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoCortaPapel }

TMetodoCortaPapel = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoNaoFiscalCompleto }

TMetodoNaoFiscalCompleto = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoAbreNaoFiscal }

TMetodoAbreNaoFiscal = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoRegistraItemNaoFiscal }

TMetodoRegistraItemNaoFiscal = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoCancelaItemNaoFiscal }

TMetodoCancelaItemNaoFiscal = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSubTotalizaNaoFiscal }

TMetodoSubTotalizaNaoFiscal = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoEfetuaPagamentoNaoFiscal }

TMetodoEfetuaPagamentoNaoFiscal = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoFechaNaoFiscal }

TMetodoFechaNaoFiscal = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoCancelaNaoFiscal }

TMetodoCancelaNaoFiscal = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoLeituraX }

TMetodoLeituraX = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoLeituraXSerial }

TMetodoLeituraXSerial = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoReducaoZ }

TMetodoReducaoZ = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoTipoUltimoDocumento }

TMetodoTipoUltimoDocumento = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoPoucoPapel }

TMetodoPoucoPapel = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoHorarioVerao }

TMetodoHorarioVerao = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoArredonda }

TMetodoArredonda = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoArredondaPorQtd }

TMetodoArredondaPorQtd = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoArredondaItemMFD }

TMetodoArredondaItemMFD = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSetArredondaItemMFD }

TMetodoSetArredondaItemMFD = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoMFD }

TMetodoMFD = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoTermica }

TMetodoTermica = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoIdentificaConsumidorRodape }

TMetodoIdentificaConsumidorRodape = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoEstado }

TMetodoEstado = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoAbreGaveta }

TMetodoAbreGaveta = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoGavetaAberta }

TMetodoGavetaAberta = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoImprimeCheque }

TMetodoImprimeCheque = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoCancelaImpressaoCheque }

TMetodoCancelaImpressaoCheque = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoChequePronto }

TMetodoChequePronto = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoLeituraCMC7 }

TMetodoLeituraCMC7 = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoMudaHorarioVerao }

TMetodoMudaHorarioVerao = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoMudaArredondamento }

TMetodoMudaArredondamento = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoPreparaTEF }

TMetodoPreparaTEF = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoCorrigeEstadoErro }

TMetodoCorrigeEstadoErro = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoAbreRelatorioGerencial }

TMetodoAbreRelatorioGerencial = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoRelatorioGerencial }

TMetodoRelatorioGerencial = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoPulaLinhas }

TMetodoPulaLinhas = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoLinhaRelatorioGerencial }

TMetodoLinhaRelatorioGerencial = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoAbreCupomVinculado }

TMetodoAbreCupomVinculado = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoLinhaCupomVinculado }

TMetodoLinhaCupomVinculado = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoCupomVinculado }

TMetodoCupomVinculado = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoEstornaCCD }

TMetodoEstornaCCD = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoSegundaViaVinculado }

TMetodoSegundaViaVinculado = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoReimpressaoVinculado }

TMetodoReimpressaoVinculado = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoFechaRelatorio }

TMetodoFechaRelatorio = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoLeituraMemoriaFiscal }

TMetodoLeituraMemoriaFiscal = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoLeituraMemoriaFiscalSerial }

TMetodoLeituraMemoriaFiscalSerial = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoLeituraMFDserial }

TMetodoLeituraMFDserial = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoArquivoMFD_dll }

TMetodoArquivoMFD_dll = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoEspelhoMFD_dll }

TMetodoEspelhoMFD_dll = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoPafMf_Lmfc_Impressao }

TMetodoPafMf_Lmfc_Impressao = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoPafMf_Lmfc_Espelho }

TMetodoPafMf_Lmfc_Espelho = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoPafMf_Lmfc_Cotepe1704 }

TMetodoPafMf_Lmfc_Cotepe1704 = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoPafMf_Lmfs_Impressao }

TMetodoPafMf_Lmfs_Impressao = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoPafMf_Lmfs_Espelho }

TMetodoPafMf_Lmfs_Espelho = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoPafMf_Mfd_Espelho }

TMetodoPafMf_Mfd_Espelho = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoPafMf_Mfd_Cotepe1704 }

TMetodoPafMf_Mfd_Cotepe1704 = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoPafMf_GerarCat52 }

TMetodoPafMf_GerarCat52 = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoAssinarBlocoXEstoque }

TMetodoAssinarBlocoXEstoque = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoAssinarBlocoXReducaoZ }

TMetodoAssinarBlocoXReducaoZ = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoAssinarBlocoX }

TMetodoAssinarBlocoX = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoValidarBlocoXEstoque }

TMetodoValidarBlocoXEstoque = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoValidarBlocoXReducaoZ }

TMetodoValidarBlocoXReducaoZ = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoValidarBlocoX }

TMetodoValidarBlocoX = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoEnviarBlocoXEstoque }

TMetodoEnviarBlocoXEstoque = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoEnviarBlocoXReducaoZ }

TMetodoEnviarBlocoXReducaoZ = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoEnviarBlocoX }

TMetodoEnviarBlocoX = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoConsultarBlocoX }

TMetodoConsultarBlocoX = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoEnviaComando }

TMetodoEnviaComando = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoAssinaArquivo }

TMetodoAssinaArquivo = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoConfigBarras }

TMetodoConfigBarras = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoPafMF_ArqMF_Binario }

TMetodoPafMF_ArqMF_Binario = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoPafMF_ArqMFD_Binario }

TMetodoPafMF_ArqMFD_Binario = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoTransmitirArquivo }

TMetodoTransmitirArquivo = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoConsultarProcessamentoArquivo }

TMetodoConsultarProcessamentoArquivo = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoCancelarArquivo }

TMetodoCancelarArquivo = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoConsultarHistoricoArq }

TMetodoConsultarHistoricoArq = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoConsultarPendenciasContrib }

TMetodoConsultarPendenciasContrib = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoConsultarPendenciasDevPafEcf }

TMetodoConsultarPendenciasDevPafEcf = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoDownloadArquivo }

TMetodoDownloadArquivo = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoListarArquivos }

TMetodoListarArquivos = class(TACBrMetodo)
public
  procedure Executar; override;
end;

{ TMetodoReprocessarArquivo }

TMetodoReprocessarArquivo = class(TACBrMetodo)
public
  procedure Executar; override;
end;

// //
implementation

uses StrUtils, ACBrUtil, UtilUnit, DoACBrUnit;

{ TMetodoReprocessarArquivo }

{params: 0 - cRecibo: Número do Recibo}
procedure TMetodoReprocessarArquivo.Executar;
var
  cRecibo: String;
  wXML: String;
begin
  cRecibo := fpCmd.Params(0);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrBlocoX.ReprocessarArquivo.Recibo:= cRecibo;
    ACBrBlocoX.ReprocessarArquivo.RemoverEncodingXMLAssinado:= True;
    ACBrBlocoX.ReprocessarArquivo.GerarXML(True);
    wXML:= ACBrBlocoX.ReprocessarArquivo.XMLAssinado;

    ACBrBlocoX.WebServices.ReprocessarArquivoBlocoX.UsarCData:= True;
    ACBrBlocoX.WebServices.ReprocessarArquivoBlocoX.XML:= wXML;
    ACBrBlocoX.WebServices.ReprocessarArquivoBlocoX.Executar;

    fpCmd.Resposta := ACBrBlocoX.WebServices.ReprocessarArquivoBlocoX.RetWS;

  end;
end;

{ TMetodoListarArquivos }

{params: 0 - cIE: Número da Inscrição Estadual   }
procedure TMetodoListarArquivos.Executar;
var
  cIE: String;
  wXML: String;
begin
  cIE := fpCmd.Params(0);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrBlocoX.ListarArquivos.InscricaoEstadual:= cIE;
    ACBrBlocoX.ListarArquivos.RemoverEncodingXMLAssinado:= True;
    ACBrBlocoX.ListarArquivos.GerarXML(True);
    wXML:= ACBrBlocoX.ListarArquivos.XMLAssinado;

    ACBrBlocoX.WebServices.ListarArquivosBlocoX.UsarCData:= True;
    ACBrBlocoX.WebServices.ListarArquivosBlocoX.XML:= wXML;
    ACBrBlocoX.WebServices.ListarArquivosBlocoX.Executar;

    fpCmd.Resposta := ACBrBlocoX.WebServices.ListarArquivosBlocoX.RetWS;

  end;
end;

{ TMetodoDownloadArquivo }

{params: 0 - cRecibo: Número do Recibo para processamento   }
procedure TMetodoDownloadArquivo.Executar;
var
  cRecibo: String;
  wXML: String;
begin
  cRecibo := fpCmd.Params(0);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrBlocoX.DownloadArquivo.Recibo:= cRecibo;
    ACBrBlocoX.DownloadArquivo.RemoverEncodingXMLAssinado:= True;
    ACBrBlocoX.DownloadArquivo.GerarXML(True);
    wXML:= ACBrBlocoX.DownloadArquivo.XMLAssinado;

    ACBrBlocoX.WebServices.DownloadArquivoBlocoX.UsarCData:= True;
    ACBrBlocoX.WebServices.DownloadArquivoBlocoX.XML:= wXML;
    ACBrBlocoX.WebServices.DownloadArquivoBlocoX.Executar;

    fpCmd.Resposta := ACBrBlocoX.WebServices.DownloadArquivoBlocoX.RetWS;

  end;
end;

{ TMetodoConsultarPendenciasDevPafEcf }

{params: 0 - cCNPJ: Consultar Pend. Desenvolvedor PAF-ECF   }
procedure TMetodoConsultarPendenciasDevPafEcf.Executar;
var
  cCNPJ: String;
  wXML: String;
begin
  cCNPJ := fpCmd.Params(0);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrBlocoX.ConsultarPendenciasDesenvolvedorPafEcf.CNPJ:= cCNPJ;
    ACBrBlocoX.ConsultarPendenciasDesenvolvedorPafEcf.RemoverEncodingXMLAssinado:= True;
    ACBRBlocoX.ConsultarPendenciasDesenvolvedorPafEcf.GerarXML(True);
    wXML:= ACBrBlocoX.ConsultarPendenciasDesenvolvedorPafEcf.XMLAssinado;

    ACBrBlocoX.WebServices.ConsultarPendenciasDesenvolvedorPafEcfBlocoX.UsarCData:= True;
    ACBrBlocoX.WebServices.ConsultarPendenciasDesenvolvedorPafEcfBlocoX.XML:= wXML;

    ACBrBlocoX.WebServices.ConsultarPendenciasDesenvolvedorPafEcfBlocoX.Executar;
    fpCmd.Resposta := ACBrBlocoX.WebServices.ConsultarPendenciasDesenvolvedorPafEcfBlocoX.RetWS;

  end;
end;

{ TMetodoConsultarPendenciasContrib }

{params: 0 - cIE: Numero da Inscrição Estadual     }
procedure TMetodoConsultarPendenciasContrib.Executar;
var
  cIE: String;
  wXML: String;
begin
  cIE := fpCmd.Params(0);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrBlocoX.ConsultarPendenciasContribuinte.InscricaoEstadual:= cIE;
    ACBrBlocoX.ConsultarPendenciasContribuinte.RemoverEncodingXMLAssinado:= True;
    ACBrBlocoX.ConsultarPendenciasContribuinte.GerarXML( True );
    wXML:= ACBrBlocoX.ConsultarPendenciasContribuinte.XMLAssinado;

    ACBrBlocoX.WebServices.ConsultarPendenciasContribuinteBlocoX.UsarCData:= True;
    ACBrBlocoX.WebServices.ConsultarPendenciasContribuinteBlocoX.XML:= wXML;

    ACBrBlocoX.WebServices.ConsultarPendenciasContribuinteBlocoX.Executar;
    fpCmd.Resposta := ACBrBlocoX.WebServices.ConsultarPendenciasContribuinteBlocoX.RetWS;

  end;
end;

{ TMetodoConsultarHistoricoArq }

{params: 0 - cRecibo: Numero do Recibo     }
procedure TMetodoConsultarHistoricoArq.Executar;
var
  cRecibo: String;
  wXML: String;
begin
  cRecibo := fpCmd.Params(0);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrBlocoX.ConsultarHistoricoArquivo.Recibo:= cRecibo;
    ACBrBlocoX.ConsultarHistoricoArquivo.RemoverEncodingXMLAssinado:= False;
    ACBrBlocoX.ConsultarHistoricoArquivo.GerarXML(True);
    wXML:= ACBrBlocoX.CancelarArquivo.XMLAssinado;

    ACBrBlocoX.WebServices.ConsultarHistoricoArquivoBlocoX.UsarCData:= True;
    ACBrBlocoX.WebServices.ConsultarHistoricoArquivoBlocoX.XML:= wXML;

    ACBrBlocoX.WebServices.ConsultarHistoricoArquivoBlocoX.Executar;
    fpCmd.Resposta := ACBrBlocoX.WebServices.ConsultarHistoricoArquivoBlocoX.RetWS;

  end;
end;

{ TMetodoCancelarArquivo }

{params: 0 - cRecibo: Numero do Recibo
         1 - cMotivoCanc: Motivo do Cancelamento}
procedure TMetodoCancelarArquivo.Executar;
var
  cRecibo: String;
  cMotivoCancelamento: String;
  wXML: String;
begin
  cRecibo := fpCmd.Params(0);
  cMotivoCancelamento := fpCmd.Params(1);
  wXML := '';

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrBlocoX.CancelarArquivo.Recibo := cRecibo;
    ACBrBlocoX.CancelarArquivo.Motivo := cMotivoCancelamento;
    ACBrBlocoX.CancelarArquivo.RemoverEncodingXMLAssinado:= True;
    ACBrBlocoX.CancelarArquivo.GerarXML(True);
    wXML:= ACBrBlocoX.CancelarArquivo.XMLAssinado;

    ACBrBlocoX.WebServices.CancelarArquivoBlocoX.UsarCData:= True;
    ACBrBlocoX.WebServices.CancelarArquivoBlocoX.XML:= wXML;

    ACBrBlocoX.WebServices.CancelarArquivoBlocoX.Executar;
    fpCmd.Resposta := ACBrBlocoX.WebServices.CancelarArquivoBlocoX.RetWS;

  end;

end;

{ TMetodoConsultarProcessamentoArquivo }

{params: 0 - cRecibo: Numero do Recibo}
procedure TMetodoConsultarProcessamentoArquivo.Executar;
var
  cRecibo: String;
  wXML: String;
begin
  cRecibo := fpCmd.Params(0);
  wXML := '';

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrBlocoX.ConsultarProcessamentoArquivo.Recibo:= cRecibo;
    ACBrBlocoX.ConsultarProcessamentoArquivo.RemoverEncodingXMLAssinado:= True;
    ACBrBlocoX.ConsultarProcessamentoArquivo.GerarXML(True);
    wXML := ACBrBlocoX.ConsultarProcessamentoArquivo.XMLAssinado;

    ACBrBlocoX.WebServices.ConsultarProcessamentoArquivoBlocoX.UsarCData:= True;
    ACBrBlocoX.WebServices.ConsultarProcessamentoArquivoBlocoX.XML:= wXML;

    ACBrBlocoX.WebServices.ConsultarProcessamentoArquivoBlocoX.Executar;
    fpCmd.Resposta := ACBrBlocoX.WebServices.ConsultarProcessamentoArquivoBlocoX.RetWS;

  end;

end;

{ TMetodoTransmitirArquivo }

{params: 0 - cPath: Caminho do XML ou conteúdo do arquivo XML}
procedure TMetodoTransmitirArquivo.Executar;
var
  cPath: String;
  FXMLOriginal: TStringList;
begin
  cPath  := fpCmd.Params(0);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    if FileExists(cPath) then
    begin
      FXMLOriginal := TStringList.Create;
      try
        FXMLOriginal.LoadFromFile(cPath);
        ACBrBlocoX.WebServices.TransmitirArquivoBlocoX.XML := FXMLOriginal.Text;
      finally
        FXMLOriginal.Free;
      end;
    end
    else
      ACBrBlocoX.WebServices.TransmitirArquivoBlocoX.XML := cPath;

    if ACBrBlocoX.WebServices.TransmitirArquivoBlocoX.Executar then
      fpCmd.Resposta := ACBrBlocoX.WebServices.TransmitirArquivoBlocoX.RetWS
    else
      fpCmd.Resposta := 'Erro ao enviar' + sLineBreak + ACBrBlocoX.WebServices.TransmitirArquivoBlocoX.Msg;
  end;

end;

{ TMetodoPafMF_ArqMFD_Binario }

{ Params=  cArq: Path do arquivo
}
procedure TMetodoPafMF_ArqMFD_Binario.Executar;
var
  cArq: String;
begin
  cArq:= fpCmd.Params(0);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrECF.PafMF_ArqMFD_Binario(cArq);
  end;
end;

{ TMetodoPafMF_ArqMF_Binario }

{ Params=  cArq: Path do arquivo
}
procedure TMetodoPafMF_ArqMF_Binario.Executar;
var
  cArq: String;
begin
  cArq:= fpCmd.Params(0);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrECF.PafMF_ArqMF_Binario(cArq);
  end;
end;

{ TMetodoConfigBarras }

{ Params=  nAltura: Integer;
           nLargura: Integer;
}
procedure TMetodoConfigBarras.Executar;
var
    nAltura: Integer;
    nLargura: Integer;
begin
  nAltura:= StrToIntDef( fpCmd.Params(0), 0);
  nLargura:= StrToIntDef( fpCmd.Params(1), 0);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    if (nAltura > 0) then
      ACBrECF.ConfigBarras.Altura:= nAltura;
    if (nLargura > 0) then
      ACBrECF.ConfigBarras.LarguraLinha := nLargura;
  end;
end;

{ TMetodoAssinaArquivo }

{ Params=  cArq: Path do arquivo
}
procedure TMetodoAssinaArquivo.Executar;
var
  cArq: String;
begin
  cArq := fpCmd.Params(0);

  with TACBrObjetoECF(fpObjetoDono) do
  begin;
    ACBrECF.AssinaArquivoComEAD(cArq);
  end;

end;

{ TMetodoEnviaComando }

{ Params=  cComando: Comando a enviar
           nTimeOut: Tempo de resposta
}
procedure TMetodoEnviaComando.Executar;
var
  cComando: String;
  nTimeOut: Integer;
begin
  cComando:= fpCmd.Params(0);
  nTimeOut:= StrToIntDef(fpCmd.Params(1),0);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    if (nTimeOut > 0) then
       ACBrECF.EnviaComando(cComando,nTimeOut )
    else
       ACBrECF.EnviaComando(cComando);
  end;
end;

{ TMetodoConsultarBlocoX }

{ Params=  cRecibo: numero do Recibo
}
procedure TMetodoConsultarBlocoX.Executar;
var
  cRecibo: String;
begin
  cRecibo := fpCmd.Params(0);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrBlocoX.WebServices.ConsultarBlocoX.Recibo:= cRecibo;
    ACBrBlocoX.WebServices.ConsultarBlocoX.Executar;
    fpCmd.Resposta := ACBrBlocoX.WebServices.ConsultarBlocoX.RetWS;
  end;
end;

{ TMetodoEnviarBlocoX }

{ Params=  cPath: Path do XML
}
procedure TMetodoEnviarBlocoX.Executar;
var
  cPath: String;
  FXMLOriginal: TStringList;
begin
  cPath  := fpCmd.Params(0);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    if FileExists(cPath) then
    begin
      FXMLOriginal := TStringList.Create;
      try
        FXMLOriginal.LoadFromFile(cPath);
        ACBrBlocoX.WebServices.EnviarBlocoX.XML := FXMLOriginal.Text;
      finally
        FXMLOriginal.Free;
      end;
    end
    else
      ACBrBlocoX.WebServices.EnviarBlocoX.XML := cPath;

    ACBrBlocoX.WebServices.EnviarBlocoX.Executar;
    fpCmd.Resposta := ACBrBlocoX.WebServices.EnviarBlocoX.RetWS;
  end;
end;

{ TMetodoEnviarBlocoXReducaoZ }

{ Params=  cPath: Path do XML
}
procedure TMetodoEnviarBlocoXReducaoZ.Executar;
var
  cPath: String;
  FXMLOriginal: TStringList;
begin
  cPath  := fpCmd.Params(0);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    if FileExists(cPath) then
    begin
      FXMLOriginal := TStringList.Create;
      try
        FXMLOriginal.LoadFromFile(cPath);
        ACBrBlocoX.WebServices.EnviarBlocoX.XML := FXMLOriginal.Text;
      finally
        FXMLOriginal.Free;
      end;
    end
    else
      ACBrBlocoX.WebServices.EnviarBlocoX.XML := cPath;

    ACBrBlocoX.WebServices.EnviarBlocoX.Executar;
    fpCmd.Resposta := ACBrBlocoX.WebServices.EnviarBlocoX.RetWS;
  end;

end;

{ TMetodoEnviarBlocoXEstoque }

{ Params=  cPath: Path do XML
}
procedure TMetodoEnviarBlocoXEstoque.Executar;
var
  cPath: String;
  FXMLOriginal: TStringList;
begin
  cPath  := fpCmd.Params(0);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    if FileExists(cPath) then
    begin
      FXMLOriginal := TStringList.Create;
      try
        FXMLOriginal.LoadFromFile(cPath);
        ACBrBlocoX.WebServices.EnviarBlocoX.XML := FXMLOriginal.Text;
      finally
        FXMLOriginal.Free;
      end;
    end
    else
      ACBrBlocoX.WebServices.EnviarBlocoX.XML := cPath;

    ACBrBlocoX.WebServices.EnviarBlocoX.Executar;
    fpCmd.Resposta := ACBrBlocoX.WebServices.EnviarBlocoX.RetWS;
  end;

end;

{ TMetodoValidarBlocoX }

{ Params=  cPath: Path do XML
}
procedure TMetodoValidarBlocoX.Executar;
var
  cPath: String;
  FXMLOriginal: TStringList;
begin
  cPath  := fpCmd.Params(0);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    if FileExists(cPath) then
    begin
      FXMLOriginal := TStringList.Create;
      try
        FXMLOriginal.LoadFromFile(cPath);
        ACBrBlocoX.WebServices.ValidarBlocoX.XML := FXMLOriginal.Text;
      finally
        FXMLOriginal.Free;
      end;
    end
    else
       ACBrBlocoX.WebServices.ValidarBlocoX.XML := cPath;
    ACBrBlocoX.WebServices.ValidarBlocoX.ValidarPafEcfEEcf := False;
    ACBrBlocoX.WebServices.ValidarBlocoX.Executar;
    fpCmd.Resposta := ACBrBlocoX.WebServices.ValidarBlocoX.RetWS;
  end;
end;

{ TMetodoValidarBlocoXReducaoZ }

{ Params=  cPath: Path do XML
}
procedure TMetodoValidarBlocoXReducaoZ.Executar;
var
  cPath: String;
  FXMLOriginal: TStringList;
begin
  cPath  := fpCmd.Params(0);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    if FileExists(cPath) then
    begin
      FXMLOriginal := TStringList.Create;
      try
        FXMLOriginal.LoadFromFile(cPath);
        ACBrBlocoX.WebServices.ValidarBlocoX.XML := FXMLOriginal.Text;
      finally
        FXMLOriginal.Free;
      end;
    end
    else
       ACBrBlocoX.WebServices.ValidarBlocoX.XML := cPath;

    ACBrBlocoX.WebServices.ValidarBlocoX.ValidarPafEcfEEcf := False;
    ACBrBlocoX.WebServices.ValidarBlocoX.Executar;
    fpCmd.Resposta := ACBrBlocoX.WebServices.ValidarBlocoX.RetWS;
  end;

end;

{ TMetodoValidarBlocoXEstoque }

{ Params=  cPath: Path do XML
}
procedure TMetodoValidarBlocoXEstoque.Executar;
var
  cPath: String;
  FXMLOriginal: TStringList;
begin
  cPath  := fpCmd.Params(0);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    if FileExists(cPath) then
    begin
      FXMLOriginal := TStringList.Create;
      try
        FXMLOriginal.LoadFromFile(cPath);
        ACBrBlocoX.WebServices.ValidarBlocoX.XML := FXMLOriginal.Text;
      finally
        FXMLOriginal.Free;
      end;
    end
    else
      ACBrBlocoX.WebServices.ValidarBlocoX.XML := cPath;

    ACBrBlocoX.WebServices.ValidarBlocoX.ValidarPafEcfEEcf := False;
    ACBrBlocoX.WebServices.ValidarBlocoX.Executar;
    fpCmd.Resposta := ACBrBlocoX.WebServices.ValidarBlocoX.RetWS;
  end;

end;

{ TMetodoAssinarBlocoX }

{ Params=  cPath: Path do XML
}
procedure TMetodoAssinarBlocoX.Executar;
var
  cPath: String;
  FXMLOriginal: TStringList;
begin
  cPath  := fpCmd.Params(0);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    if FileExists(cPath) then
      begin
        FXMLOriginal := TStringList.Create;
        try
          FXMLOriginal.LoadFromFile(cPath);

          if (Pos('</reducaoz>',LowerCase(FXMLOriginal.Text)) > 0) then
            FXMLOriginal.Text := ACBrBlocoX.SSL.Assinar(FXMLOriginal.Text, 'ReducaoZ', 'Mensagem')
          else
            FXMLOriginal.Text := ACBrBlocoX.SSL.Assinar(FXMLOriginal.Text, 'Estoque', 'Mensagem');
          FXMLOriginal.SaveToFile(cPath);
          fpCmd.Resposta:= 'OK: '+ cPath;
        finally
          FXMLOriginal.Free;
        end;
      end
      else
        fpCmd.Resposta := ACBrBlocoX.SSL.Assinar(cPath, 'ReducaoZ', 'Mensagem');
    end;
end;

{ TMetodoAssinarBlocoXReducaoZ }

{ Params=  cPath: Path do XML
}
procedure TMetodoAssinarBlocoXReducaoZ.Executar;
var
  cPath: String;
  FXMLOriginal: TStringList;
begin
  cPath  := fpCmd.Params(0);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    if FileExists(cPath) then
    begin
      FXMLOriginal := TStringList.Create;
      try
        FXMLOriginal.LoadFromFile(cPath);
        FXMLOriginal.Text := ACBrBlocoX.SSL.Assinar(FXMLOriginal.Text, 'ReducaoZ', 'Mensagem');
        FXMLOriginal.SaveToFile(cPath);
        fpCmd.Resposta:= 'OK: '+ cPath;
      finally
        FXMLOriginal.Free;
      end;
    end
    else
      fpCmd.Resposta := ACBrBlocoX.SSL.Assinar(cPath, 'ReducaoZ', 'Mensagem');
  end;
end;

{ TMetodoAssinarBlocoXEstoque }

{ Params=  cPath: Path do XML
}
procedure TMetodoAssinarBlocoXEstoque.Executar;
var
  cPath: String;
  FXMLOriginal: TStringList;
begin
  cPath  := fpCmd.Params(0);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    if FileExists(cPath) then
    begin
      FXMLOriginal := TStringList.Create;
      try
        FXMLOriginal.LoadFromFile(cPath);
        FXMLOriginal.Text := ACBrBlocoX.SSL.Assinar(FXMLOriginal.Text, 'Estoque', 'Mensagem');
        FXMLOriginal.SaveToFile(cPath);
        fpCmd.Resposta:= 'OK: '+ cPath;
      finally
        FXMLOriginal.Free;
      end;
    end
    else
      fpCmd.Resposta := ACBrBlocoX.SSL.Assinar(cPath, 'Estoque', 'Mensagem');
  end;
end;

{ TMetodoPafMf_GerarCat52 }

{ Params=  Ini: Parâmetro para redução Inicial
           Fim: Parâmetro para redução Final
           cDirArquivo: Diretório do Arquivo
}
procedure TMetodoPafMf_GerarCat52.Executar;
var
  Ini: String;
  Fim: String;
  cDirArquivo : String;
begin
  Ini  := fpCmd.Params(0);
  Fim  := fpCmd.Params(1);
  cDirArquivo := fpCmd.Params(2);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrECF.PafMF_GerarCAT52(
                      StringToDateTime(Ini),         { Dt.Inicial }
                      StringToDateTime(Fim),         { Dt.Final }
                      cDirArquivo );                 { Diretorio Arquivo }
  end;
end;

{ TMetodoPafMf_Mfd_Cotepe1704 }

{ Params=  Ini: Parâmetro para redução Inicial
           Fim: Parâmetro para redução Final
           cNomeArquivo: Nome do Arquivo
}
procedure TMetodoPafMf_Mfd_Cotepe1704.Executar;
var
  Ini: String;
  Fim: String;
  NomeArquivo : String;
begin
  Ini  := fpCmd.Params(0);
  Fim  := fpCmd.Params(1);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    NomeArquivo := AjustaNomeArquivoCmd();

    if pos(DateSeparator,Ini) > 0 then
       ACBrECF.PafMF_MFD_Cotepe1704(
               StringToDateTime(Ini),            { Dt.Inicial }
               StringToDateTime(Fim),            { Dt.Final }
               NomeArquivo )                     { Nome do Arquivo }
    else
       ACBrECF.PafMF_MFD_Cotepe1704(
               StrToInt(Trim(Ini)),              { CRZInicial }
               StrToInt(Trim(Fim)),              { CRZFinal }
               NomeArquivo );                    { Nome do Arquivo }

  end;
end;

{ TMetodoPafMf_Mfd_Espelho }

{ Params=  Ini: Parâmetro para redução Inicial
           Fim: Parâmetro para redução Final
           cNomeArquivo: Nome do Arquivo
}
procedure TMetodoPafMf_Mfd_Espelho.Executar;
var
  Ini: String;
  Fim: String;
  NomeArquivo : String;
begin
  Ini  := fpCmd.Params(0);
  Fim  := fpCmd.Params(1);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    NomeArquivo := AjustaNomeArquivoCmd();

    if pos(DateSeparator,Ini) > 0 then
       ACBrECF.PafMF_MFD_Espelho(
               StringToDateTime(Ini),            { Dt.Inicial }
               StringToDateTime(Fim),            { Dt.Final }
               NomeArquivo )                     { Nome do Arquivo }
    else
       ACBrECF.PafMF_MFD_Espelho(
               StrToInt(Trim(Ini)),              { CRZInicial }
               StrToInt(Trim(Fim)),              { CRZFinal }
               NomeArquivo );                    { Nome do Arquivo }

  end;
end;

{ TMetodoPafMf_Lmfs_Espelho }

{ Params=  Ini: Parâmetro para redução Inicial
           Fim: Parâmetro para redução Final
           cNomeArquivo: Nome do Arquivo
}
procedure TMetodoPafMf_Lmfs_Espelho.Executar;
var
  Ini: String;
  Fim: String;
  NomeArquivo : String;
begin
  Ini  := fpCmd.Params(0);
  Fim  := fpCmd.Params(1);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    NomeArquivo := AjustaNomeArquivoCmd();

    if pos(DateSeparator,Ini) > 0 then
       ACBrECF.PafMF_LMFS_Espelho(
               StringToDateTime(Ini),            { Dt.Inicial }
               StringToDateTime(Fim),            { Dt.Final }
               NomeArquivo )                     { Nome do Arquivo }
    else
       ACBrECF.PafMF_LMFS_Espelho(
               StrToInt(Trim(Ini)),              { CRZInicial }
               StrToInt(Trim(Fim)),              { CRZFinal }
               NomeArquivo );                    { Nome do Arquivo }

  end;
end;

{ TMetodoPafMf_Lmfs_Impressao }

{ Params=  Ini: Parâmetro para redução Inicial
           Fim: Parâmetro para redução Final
}
procedure TMetodoPafMf_Lmfs_Impressao.Executar;
var
  Ini: String;
  Fim: String;
begin
  Ini  := fpCmd.Params(0);
  Fim  := fpCmd.Params(1);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    if pos(DateSeparator,Ini) > 0 then
       ACBrECF.PafMF_LMFS_Impressao(
               StringToDateTime(Ini),            { Dt.Inicial }
               StringToDateTime(Fim))            { Dt.Final }
    else
       ACBrECF.PafMF_LMFS_Impressao(
               StrToInt(Trim(Ini)),              { CRZInicial }
               StrToInt(Trim(Fim)));             { CRZFinal }
  end;
end;

{ TMetodoPafMf_Lmfc_Cotepe1704 }

{ Params=  Ini: Parâmetro para redução Inicial
           Fim: Parâmetro para redução Final
           cNomeArquivo: Nome do Arquivo
}
procedure TMetodoPafMf_Lmfc_Cotepe1704.Executar;
var
  Ini: String;
  Fim: String;
  NomeArquivo : String;
begin
  Ini  := fpCmd.Params(0);
  Fim  := fpCmd.Params(1);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    NomeArquivo := AjustaNomeArquivoCmd();

    if pos(DateSeparator,Ini) > 0 then
       ACBrECF.PafMF_LMFC_Cotepe1704(
               StringToDateTime(Ini),            { Dt.Inicial }
               StringToDateTime(Fim),            { Dt.Final }
               NomeArquivo )                     { Nome do Arquivo }
    else
       ACBrECF.PafMF_LMFC_Cotepe1704(
               StrToInt(Trim(Ini)),              { CRZInicial }
               StrToInt(Trim(Fim)),              { CRZFinal }
               NomeArquivo );                    { Nome do Arquivo }
  end;

end;

{ TMetodoPafMf_Lmfc_Espelho }

{ Params=  Ini: Parâmetro para redução Inicial
           Fim: Parâmetro para redução Final
           cNomeArquivo: Nome do Arquivo
}
procedure TMetodoPafMf_Lmfc_Espelho.Executar;
var
  Ini: String;
  Fim: String;
  NomeArquivo : String;
begin
  Ini  := fpCmd.Params(0);
  Fim  := fpCmd.Params(1);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    NomeArquivo := AjustaNomeArquivoCmd();

    if pos(DateSeparator,Ini) > 0 then
       ACBrECF.PafMF_LMFC_Espelho(
               StringToDateTime(Ini),            { Dt.Inicial }
               StringToDateTime(Fim),            { Dt.Final }
               NomeArquivo )                     { Nome do Arquivo }
    else
       ACBrECF.PafMF_LMFC_Espelho(
               StrToInt(Trim(Ini)),              { CRZInicial }
               StrToInt(Trim(Fim)),              { CRZFinal }
               NomeArquivo );                    { Nome do Arquivo }

  end;
end;

{ TMetodoPafMf_Lmfc_Impressao }

{ Params=  Ini: Parâmetro para redução Inicial
           Fim: Parâmetro para redução Final
}
procedure TMetodoPafMf_Lmfc_Impressao.Executar;
var
  Ini: String;
  Fim: String;
begin
  Ini           := fpCmd.Params(0);
  Fim           := fpCmd.Params(1);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    if pos(DateSeparator, Ini) > 0 then
       ACBrECF.PafMF_LMFC_Impressao(
               StringToDateTime(Fim),            { Dt.Inicial }
               StringToDateTime(Fim) )           { Dt.Final }
    else
       ACBrECF.PafMF_LMFC_Impressao(
               StrToInt(Trim(Ini)),              { CRZInicial }
               StrToInt(Trim(Fim)) ) ;           { CRZFinal }
  end;
end;

{ TMetodoEspelhoMFD_dll }

{ Params=  Ini: Parâmetro para redução Inicial
           Fim: Parâmetro para redução Final
           cNomeArquivo: Nome do Arquivo
}
procedure TMetodoEspelhoMFD_dll.Executar;
var
  Ini: String;
  Fim: String;
  NomeArquivo : String ;
begin
  Ini           := fpCmd.Params(0);
  Fim           := fpCmd.Params(1);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    NomeArquivo := AjustaNomeArquivoCmd() ;

    if pos(DateSeparator,Ini) > 0 then
        ACBrECF.EspelhoMFD_DLL(
                StringToDateTime(Ini),              { Dt.Inicial }
                StringToDateTime(Fim),              { Dt.Final }
                NomeArquivo )                       { Nome do Arquivo }
    else
        ACBrECF.EspelhoMFD_DLL(
                StrToInt(Trim(Ini)),                { COOInicial }
                StrToInt(Trim(Fim)),                { COOFinal }
                NomeArquivo ) ;                     { Nome do Arquivo }

  end;
end;

{ TMetodoArquivoMFD_dll }

{ Params=  Ini: Parâmetro para redução Inicial
           Fim: Parâmetro para redução Final
           cNomeArquivo: Nome do Arquivo
           TipoDocStr    : Tipo documento;
           FinalidadeStr := Finalidade
}
procedure TMetodoArquivoMFD_dll.Executar;
var
  Ini: String;
  Fim: String;
  TipoDocStr: String;
  FinalidadeStr: String;
  NomeArquivo : String ;
begin
  Ini           := fpCmd.Params(0);
  Fim           := fpCmd.Params(1);
  TipoDocStr    := Trim(fpCmd.Params(3));
  FinalidadeStr := Trim(fpCmd.Params(4));

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    NomeArquivo := AjustaNomeArquivoCmd() ;
    fTipoDoc := docTodos;                 { Valor Padrao para Tipo Documento }
    fFinalidade := finMFD;                { Valor Padrao para Finalidade }

    if TipoDocStr <> '' then
    begin
       if StrIsNumber(TipoDocStr) then
          fTipoDoc := TACBrECFTipoDocumento(StrToIntDef(TipoDocStr, 19))
       else
          fTipoDoc := TACBrECFTipoDocumento(GetEnumValue(TypeInfo(TACBrECFTipoDocumento),TipoDocStr));   { Tipo de Documento do ArquivoMFD }
    end ;

    if FinalidadeStr <> '' then
    begin
       if StrIsNumber(FinalidadeStr) then
          fFinalidade := TACBrECFFinalizaArqMFD(StrToIntDef( FinalidadeStr, 1))
       else
          fFinalidade := TACBrECFFinalizaArqMFD(GetEnumValue(TypeInfo(TACBrECFFinalizaArqMFD),FinalidadeStr));    { Finalidade do ArquivoMFD }
    end ;

    if pos(DateSeparator,Ini) > 0 then
       ACBrECF.ArquivoMFD_DLL(
           StringToDateTime(Ini),                { Dt.Inicial }
           StringToDateTime(Fim),                { Dt.Final }
           NomeArquivo,                          { Nome do Arquivo }
           [fTipoDoc],                           { Tipo de Documento }
           fFinalidade)                          { Finalidade }
    else
       ACBrECF.ArquivoMFD_DLL(
           StrToInt(Trim(Ini)),                  { COOInicial }
           StrToInt(Trim(Fim)),                  { COOFinal }
           NomeArquivo,                          { Nome do Arquivo }
           [fTipoDoc],                           { Tipo de Documento }
           fFinalidade);                         { Finalidade }
  end;
end;

{ TMetodoLeituraMFDserial }

{ Params=  Ini: Parâmetro para redução Inicial
           Fim: Parâmetro para redução Final
           cNome: Nome do Arquivo
}
procedure TMetodoLeituraMFDserial.Executar;
var
  Ini: String;
  Fim: String;
  cNome: String;

  Linhas: TStringList;
begin
  Ini:= fpCmd.Params(0);
  Fim:= fpCmd.Params(1);
  cNome:= fpCmd.Params(2);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    if (cNome <> '') then
     begin
        if pos(DateSeparator,Ini) > 0 then
           ACBrECF.LeituraMFDSerial(
               StringToDateTime(Ini),              { Dt.Inicial }
               StringToDateTime(Fim),              { Dt.Final }
               cNome )                             { Nome do Arquivo }
        else
           ACBrECF.LeituraMFDSerial(
               StrToInt(Trim(Ini)),                { COOInicial }
               StrToInt(Trim(Fim)),                { COOFinal }
               cNome ) ;                           { Nome do Arquivo }
     end
    else
    begin
      Linhas := TStringList.Create ;
      try
         if pos(DateSeparator,Ini) > 0 then
            ACBrECF.LeituraMFDSerial(
                StringToDateTime(Ini),          { Dt.Inicial }
                StringToDateTime(Fim),          { Dt.Final }
                Linhas )                        { Retorno }
         else
            ACBrECF.LeituraMFDSerial(
                StrToInt(Trim(Ini)),            { COOInicial }
                StrToInt(Trim(Fim)),            { COOFinal }
                Linhas ) ;                      { Retorno }
               fpCmd.Resposta := Linhas.Text ;
      finally
         Linhas.Free ;
      end ;
    end ;
  end;
end;

{ TMetodoLeituraMemoriaFiscalSerial }

{ Params=  Ini: Parâmetro para redução Inicial
           Fim: Parâmetro para redução Final
           cNome: Nome do Arquivo
           bSimplificado: Boolean para leitura simplificada
}
procedure TMetodoLeituraMemoriaFiscalSerial.Executar;
var
  Ini: String;
  Fim: String;
  cNome: String;
  bSimplificado: Boolean;

  Linhas: TStringList;
begin
  Ini:= fpCmd.Params(0);
  Fim:= fpCmd.Params(1);
  cNome:= fpCmd.Params(2);
  bSimplificado:= StrToBoolDef(fpCmd.Params(3),False);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    if (cNome <> '') then
    begin
      if (pos(DateSeparator, Ini) > 0) then
          ACBrECF.LeituraMemoriaFiscalSerial(
              StringToDateTime(Ini),              { Dt.Inicial }
              StringToDateTime(Fim),              { Dt.Final }
              cNome,                              { Nome Arquivo }
              bSimplificado )                     { Simplificada }
       else
          ACBrECF.LeituraMemoriaFiscalSerial(
              StrToInt(Trim(Ini)),                { ReducaoInicial }
              StrToInt(Trim(Fim)),                { ReducaoFinal }
              cNome,                              { Nome Arquivo }
              bSimplificado ) ;                   { Simplificada }
    end
    else
    begin
      Linhas := TStringList.Create ;
      try
        if pos(DateSeparator,Ini) > 0 then
           ACBrECF.LeituraMemoriaFiscalSerial(
               StringToDateTime(Ini),                    { Dt.Inicial }
               StringToDateTime(Fim),                    { Dt.Final }
               Linhas,                                   { Retorno }
               bSimplificado )                           { Simplificada}
        else
           ACBrECF.LeituraMemoriaFiscalSerial(
               StrToInt(Trim(Ini)),                      { ReducaoInicial }
               StrToInt(Trim(Fim)),                      { ReducaoFinal }
               Linhas,                                   { Retorno }
               bSimplificado );                          { Simplificada}
          fpCmd.Resposta := Linhas.Text ;
       finally
         Linhas.Free;
       end ;
    end;

  end;

end;

{ TMetodoLeituraMemoriaFiscal }

{ Params=  Ini: Parâmetro para redução Inicial
           Fim: Parâmetro para redução Final
           bSimplificado: Boolean para leitura simplificada
}
procedure TMetodoLeituraMemoriaFiscal.Executar;
var
  Ini: String;
  Fim: String;
  bSimplificado: Boolean;
begin
  Ini:= fpCmd.Params(0);
  Fim:= fpCmd.Params(1);
  bSimplificado:= StrToBoolDef(fpCmd.Params(2),False);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    if pos(DateSeparator,Ini) > 0 then
       ACBrECF.LeituraMemoriaFiscal( StringToDateTime(Ini), {Dt.Inicial}
                             StringToDateTime(Fim),         {Dt.Final}
                             bSimplificado )                {Simplificada}
    else
       ACBrECF.LeituraMemoriaFiscal( StrToInt(Trim(Ini)),    { ReducaoInicial }
                             StrToInt(Trim(Fim)),            { ReducaoFinal }
                             bSimplificado )                 { Simplificada}
  end;
end;

{ TMetodoFechaRelatorio }

procedure TMetodoFechaRelatorio.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrECF.FechaRelatorio;
  end;
end;

{ TMetodoReimpressaoVinculado }

procedure TMetodoReimpressaoVinculado.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrECF.ReimpressaoVinculado;
  end;
end;

{ TMetodoSegundaViaVinculado }

procedure TMetodoSegundaViaVinculado.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrECF.SegundaViaVinculado;
  end;
end;

{ TMetodoEstornaCCD }

{ Params=  bTodos: Boolean para Estornar todos
}
procedure TMetodoEstornaCCD.Executar;
var
  bTodos: Boolean;
begin
  bTodos := StrToBoolDef(Trim(fpCmd.Params(0)),true);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := IntToStr(ACBrECF.EstornaCCD(bTodos)); {Estorna todos CCD}
  end;
end;

{ TMetodoCupomVinculado }

{ Params=  cCOO: Cód COO
           cCodFormaPagto: Cód Forma Pagamento
           cCodComprovanteNaoFiscal: Cód Comprovante não Fiscal
           nValor: Valor do cupom
}
procedure TMetodoCupomVinculado.Executar;
var
  Linhas: TStringList;
  cCOO : String;
  cCodFormaPagto : String;
  cCodComprovanteNaoFiscal : String;
  nValor : String;
begin
  cCOO := fpCmd.Params(0);
  cCodFormaPagto := fpCmd.Params(1);
  cCodComprovanteNaoFiscal := fpCmd.Params(2);
  nValor := Trim(fpCmd.Params(3)) ;

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    Linhas := TStringList.Create ;
    try
      StringToMemo( cCOO, Linhas ); {Linha separadas por | (pipe)}

      if EstaVazio(nValor)  then  // Não tem 4 parâmetros ?
      begin
        nValor := Trim(fpCmd.Params(2));
        cCodComprovanteNaoFiscal := '';
      end;

      if (cCodComprovanteNaoFiscal <> '') then
         ACBrECF.CupomVinculado( cCOO,                  { COO }
                         cCodFormaPagto,                { CodFormaPagto }
                         cCodComprovanteNaoFiscal,      { CodComprovanteNaoFiscal }
                         StringToFloat( nValor ),       { Valor }
                         Linhas )
      else
         ACBrECF.CupomVinculado( cCOO,                  { COO }
                         cCodFormaPagto,                { CodFormaPagto }
                         StringToFloat( nValor ),       { Valor }
                         Linhas );
    finally
       Linhas.Free ;
    end ;
  end;
end;

{ TMetodoLinhaCupomVinculado }

{ Params=  cTexto - linha cupom vinculado
}
procedure TMetodoLinhaCupomVinculado.Executar;
var
  Linha: AnsiString;
  cTexto: String;
begin
  cTexto := fpCmd.Params(0);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    Linha := StringReplace( cTexto,'|',#10,[rfReplaceAll]) ;
    ACBrECF.LinhaCupomVinculado( Linha );                    { Linha }
  end;
end;

{ TMetodoAbreCupomVinculado }

{ Params=  cCOO: Cód COO
           cCodFormaPagto: Cód Forma Pagamento
           cCodComprovanteNaoFiscal: Cód Comprovante não Fiscal
           nValor: Valor do cupom
}
procedure TMetodoAbreCupomVinculado.Executar;
var
  cCOO : String;
  cCodFormaPagto : String;
  cCodComprovanteNaoFiscal : String;
  nValor : String;
begin
  cCOO := fpCmd.Params(0);
  cCodFormaPagto := fpCmd.Params(1);
  cCodComprovanteNaoFiscal := fpCmd.Params(2);
  nValor := Trim(fpCmd.Params(3)) ;

  if EstaVazio(nValor)  then  // Não tem 4 parâmetros ?
  begin
    nValor := Trim(fpCmd.Params(2));
    cCodComprovanteNaoFiscal := '';
  end;

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    if (cCodComprovanteNaoFiscal <> '') then
       ACBrECF.AbreCupomVinculado( cCOO,                 { COO }
                           cCodFormaPagto,               { CodFormaPagto }
                           cCodComprovanteNaoFiscal,     { CodComprovanteNaoFiscal }
                           StringToFloat(nValor) )       { Valor }
    else
       ACBrECF.AbreCupomVinculado( cCOO,                 { COO }
                           cCodFormaPagto,               { CodFormaPagto }
                           StringToFloat(nValor ) )      { Valor }

  end;
end;

{ TMetodoLinhaRelatorioGerencial }

{ Params=  cLinha: Texto para impressão
}
procedure TMetodoLinhaRelatorioGerencial.Executar;
var
  cLinha: AnsiString;
begin
  cLinha := StringReplace( fpCmd.Params(0),'|',#10,[rfReplaceAll]) ;

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrECF.LinhaRelatorioGerencial( cLinha );   { Linha }
  end;
end;

{ TMetodoPulaLinhas }

{ Params=  nQtdLinhas= numero linhas pular
}
procedure TMetodoPulaLinhas.Executar;
var
  nQtdLinhas: Integer;
begin
  nQtdLinhas:= StrToIntDef( Trim(fpCmd.Params(0)),0);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrECF.PulaLinhas( nQtdLinhas );            { Num.Linhas }
  end;
end;

{ TMetodoRelatorioGerencial }

{ Params=  cTexto - Texto - linhas separadas por |
           cVias - Qtd vias
}
procedure TMetodoRelatorioGerencial.Executar;
var
  cTexto: String;
  cVias: Integer;
  Linhas : TStringList;
begin
  cTexto := fpCmd.Params(0);
  cVias  := StrToIntDef(fpCmd.Params(1),1);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    Linhas := TStringList.Create ;
    try
        StringToMemo( cTexto, Linhas ); {Linha separadas por | (pipe)}
        ACBrECF.RelatorioGerencial( Linhas,
                            cVias ); { Vias }
    finally
        Linhas.Free ;
    end ;
  end;
end;

{ TMetodoAbreRelatorioGerencial }

{ Params=  bIndice - numero do Indice
}
procedure TMetodoAbreRelatorioGerencial.Executar;
var
  bIndice: Integer;
begin
  bIndice := StrToIntDef(Trim(fpCmd.Params(0)),0);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrECF.AbreRelatorioGerencial( bIndice ); { Indice }
  end;
end;

{ TMetodoCorrigeEstadoErro }

{ Params=  bReducaoz - Boolean para habilitar ReduçãoZ
}
procedure TMetodoCorrigeEstadoErro.Executar;
var
  bReducaoz: Boolean;
begin
  bReducaoz := StrToBoolDef(Trim(fpCmd.Params(0)),True);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrECF.CorrigeEstadoErro( bReducaoz );
  end;
end;

{ TMetodoPreparaTEF }

procedure TMetodoPreparaTEF.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
     ACBrECF.PreparaTEF;
  end;
end;

{ TMetodoMudaArredondamento }

{ Params=  bMudar - Boolean para mudar arredondamento
}
procedure TMetodoMudaArredondamento.Executar;
var
  bMudar: Boolean;
begin
  bMudar := StrToBool(fpCmd.Params(0));

  with TACBrObjetoECF(fpObjetoDono) do
  begin
     ACBrECF.MudaArredondamento( bMudar );
  end;
end;

{ TMetodoMudaHorarioVerao }

{ Params=  bMudar - Boolean para mudar horário verão
}
procedure TMetodoMudaHorarioVerao.Executar;
var
  bMudar: String;
begin
  bMudar := fpCmd.Params(0);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    if (bMudar <> '') then
       ACBrECF.MudaHorarioVerao( StrToBool(Trim(bMudar)) )
    else
       ACBrECF.MudaHorarioVerao( not ACBrECF.HorarioVerao );
  end;
end;

{ TMetodoLeituraCMC7 }

procedure TMetodoLeituraCMC7.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
     fpCmd.Resposta := ACBrECF.LeituraCMC7;
  end;
end;

{ TMetodoChequePronto }

procedure TMetodoChequePronto.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
     fpCmd.Resposta := BoolToStr( ACBrECF.ChequePronto, true );
  end;
end;

{ TMetodoCancelaImpressaoCheque }

procedure TMetodoCancelaImpressaoCheque.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
     ACBrECF.CancelaImpressaoCheque;
  end;
end;

{ TMetodoImprimeCheque }

{ Params=  cBanco - Descrição do Banco
           nValor - Valor do Cheque
           cFavorecido - Nome do Favorecido
           cCidade - Cidade de Emissão
           dData - Data de Emissão
           cObservacao - campo Observações
}
procedure TMetodoImprimeCheque.Executar;
var
  cBanco : String;
  nValor : Double;
  cFavorecido : String;
  cCidade : String;
  dData : TDateTime;
  cObservacao : String;

begin
  cBanco := fpCmd.Params(0);
  nValor := StringToFloat(fpCmd.Params(1));
  cFavorecido := fpCmd.Params(2);
  cCidade := fpCmd.Params(3);
  dData := StringToDateTime(fpCmd.Params(4));
  cObservacao := fpCmd.Params(5);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
     ACBrECF.ImprimeCheque( cBanco,                       { Banco }
                    nValor,                               { Valor }
                    cFavorecido,                          { Favorecido }
                    cCidade,                              { Cidade }
                    dData,                                { Data}
                    cObservacao );                        { Observação }
  end;
end;

{ TMetodoGavetaAberta }

procedure TMetodoGavetaAberta.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
     fpCmd.Resposta := BoolToStr( ACBrECF.GavetaAberta, true );
  end;
end;

{ TMetodoAbreGaveta }

procedure TMetodoAbreGaveta.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
     ACBrECF.AbreGaveta;
  end;
end;

{ TMetodoEstado }

procedure TMetodoEstado.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
     fpCmd.Resposta := GetEnumName(TypeInfo(TACBrECFEstado),Integer(ACBrECF.Estado));
  end;
end;

{ TMetodoIdentificaConsumidorRodape }

procedure TMetodoIdentificaConsumidorRodape.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
     fpCmd.Resposta := BoolToStr( ACBrECF.IdentificaConsumidorRodape, true );
  end;
end;

{ TMetodoTermica }

procedure TMetodoTermica.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
     fpCmd.Resposta := BoolToStr( ACBrECF.Termica, true );
  end;
end;

{ TMetodoMFD }

procedure TMetodoMFD.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
     fpCmd.Resposta := BoolToStr( ACBrECF.MFD, true );
  end;
end;

{ TMetodoSetArredondaItemMFD }

{ Params=  bArredonda - Arredonda item MFD
}
procedure TMetodoSetArredondaItemMFD.Executar;
var
  bArredonda: Boolean;
begin
  bArredonda := StrToBool( Trim(fpCmd.Params(0)));

  with TACBrObjetoECF(fpObjetoDono) do
  begin
     ACBrECF.ArredondaItemMFD:= bArredonda;
  end;
end;

{ TMetodoArredondaItemMFD }

procedure TMetodoArredondaItemMFD.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
     fpCmd.Resposta := BoolToStr( ACBrECF.ArredondaItemMFD, true );
  end;
end;

{ TMetodoArredondaPorQtd }

procedure TMetodoArredondaPorQtd.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
     fpCmd.Resposta := BoolToStr( ACBrECF.ArredondaPorQtd, true );
  end;
end;

{ TMetodoArredonda }

procedure TMetodoArredonda.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
     fpCmd.Resposta := BoolToStr( ACBrECF.Arredonda, true );
  end;
end;

{ TMetodoHorarioVerao }

procedure TMetodoHorarioVerao.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
     fpCmd.Resposta := BoolToStr( ACBrECF.HorarioVerao, true );
  end;
end;

{ TMetodoPoucoPapel }

procedure TMetodoPoucoPapel.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
     fpCmd.Resposta := BoolToStr( ACBrECF.PoucoPapel, true );
  end;
end;

{ TMetodoTipoUltimoDocumento }

procedure TMetodoTipoUltimoDocumento.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
     fpCmd.Resposta := GetEnumName(TypeInfo(TACBrECFTipoDocumento),Integer(ACBrECF.TipoUltimoDocumento));
  end;
end;

{ TMetodoReducaoZ }

{ Params=  dDtHr - Data hora de Geração
}
procedure TMetodoReducaoZ.Executar;
var
  dDtHr : TDateTime;
begin
  dDtHr := StringToDateTimeDef(fpCmd.Params(0),0);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
     ACBrECF.ReducaoZ( dDtHr );
  end;
end;

{ TMetodoLeituraXSerial }

{ Params=  cNomeArq - Nome do arquivo
}
procedure TMetodoLeituraXSerial.Executar;
var
  cNomeArq : String;
  Linhas: TStringList;
begin
  cNomeArq := fpCmd.Params(0);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    if (cNomeArq <> '') then
       ACBrECF.LeituraXSerial(cNomeArq)                         { Nome Arquivo }
    else
    begin
      Linhas := TStringList.Create ;
      try
         ACBrECF.LeituraXSerial( Linhas ) ;                     { Retorno }
         fpCmd.Resposta := Linhas.Text ;
      finally
         Linhas.Free ;
      end ;
    end;
  end;
end;

{ TMetodoLeituraX }

procedure TMetodoLeituraX.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrECF.LeituraX;
  end;
end;

{ TMetodoCancelaNaoFiscal }

procedure TMetodoCancelaNaoFiscal.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrECF.CancelaNaoFiscal;
  end;
end;

{ TMetodoFechaNaoFiscal }

{ Params=  cObs - Observação
}
procedure TMetodoFechaNaoFiscal.Executar;
var
  cObs : String;
begin
  cObs := fpCmd.Params(0);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrECF.FechaNaoFiscal(cObs);
  end;
end;

{ TMetodoEfetuaPagamentoNaoFiscal }

{ Params=  cCodFormaPagamento - código forma de pagamento
           nValor - Valor pagamento
           cObs - Observação
           cImpVinculado - true para impressão vinculada
}
procedure TMetodoEfetuaPagamentoNaoFiscal.Executar;
var
  cCodFormaPagamento : String;
  nValor : Double;
  cObs : String;
  cImpVinculado : Boolean;
begin
  cCodFormaPagamento := fpCmd.Params(0);
  nValor             := StringToFloat(fpCmd.Params(1));
  cObs               := fpCmd.Params(2);
  cImpVinculado      := StrToBoolDef(fpCmd.Params(3),False);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrECF.EfetuaPagamentoNaoFiscal( cCodFormaPagamento,        { CodFormaPagamento }
                                      nValor,                    { Valor }
                                      cObs,                      { Observacao }
                                      cImpVinculado );           { Imp.Vinculado }
  end;
end;

{ TMetodoSubTotalizaNaoFiscal }

{ Params=  nValor - Valor do Acrescimo ou desconto
}
procedure TMetodoSubTotalizaNaoFiscal.Executar;
var
  nValor: Double;
begin
  nValor := StringToFloatDef(fpCmd.Params(0),0);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrECF.SubtotalizaNaoFiscal( nValor );  {Acresc/Desc}
  end;
end;

{ TMetodoCancelaItemNaoFiscal }

{ Params=  nItem - número do item
}
procedure TMetodoCancelaItemNaoFiscal.Executar;
var
  nItem : Integer;
begin
  nItem := StrToInt(fpCmd.Params(0));

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrECF.CancelaItemNaoFiscal(nItem);
  end;
end;

{ TMetodoRegistraItemNaoFiscal }

{ Params=  cCodCNF - Código CNF
           nValor - Valor
           cOBS - String
}
procedure TMetodoRegistraItemNaoFiscal.Executar;
var
  cCodCNF : String;
  nValor  : Double;
  cOBS    : String;
begin
  cCodCNF := fpCmd.Params(0);
  nValor  := StringToFloat(fpCmd.Params(1));
  cOBS := fpCmd.Params(2);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrECF.RegistraItemNaoFiscal( cCodCNF,                      { CodCNF }
                                   nValor,                       { Valor }
                                   cOBS )                        { Obs }
  end;
end;

{ TMetodoAbreNaoFiscal }

{ Params=  cCNPJCPF : String
}
procedure TMetodoAbreNaoFiscal.Executar;
var
  cCNPJCPF: String;
begin
  cCNPJCPF := fpCmd.Params(0);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrECF.AbreNaoFiscal( cCNPJCPF );
  end;
end;

{ TMetodoNaoFiscalCompleto }

{ Params=  cCodCNF - Código CNF
           nValor - Valor
           cCodFormaPagto - Código forma de Pagamento
           cOBS - String
}
procedure TMetodoNaoFiscalCompleto.Executar;
var
  cCodCNF : String;
  nValor  : Double;
  cCodFormaPagto : String;
  cOBS : String;
begin
  cCodCNF := fpCmd.Params(0);
  nValor  := StringToFloat(fpCmd.Params(1));
  cCodFormaPagto := fpCmd.Params(2);
  cOBS := fpCmd.Params(3);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrECF.NaoFiscalCompleto( cCodCNF,                          { CodCNF }
                              nValor,                            { Valor }
                              cCodFormaPagto,                    { CodFormaPagto }
                              cOBS )                             { Obs }
  end;

end;

{ TMetodoCortaPapel }

{ Params=  bCorteParcial - Corte Parcial
}
procedure TMetodoCortaPapel.Executar;
var
  bCorteParcial: Boolean;
begin
  bCorteParcial := StrToBoolDef(fpCmd.Params(0),False);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrECF.CortaPapel( bCorteParcial );
  end;
end;

{ TMetodoSuprimento }

{ Params=  nValor - Valor para a Sangria.
           cObs - Observação
           cDescricaoCNF - Descrição do Comprovante não fiscal. Parâmetro pode ser omitido e será considerado "SUPRIMENTO".
           cDescricaoFPG - Descrição da Forma de Pagamento. Parâmetro pode ser omitido e será considerado "DINHEIRO".
}
procedure TMetodoSuprimento.Executar;
var
  nValor : Double;
  cObs   : String;
  cDescricaoCNF : String;
  cDescricaoFPG : String;
begin
  nValor := StringToFloat(fpCmd.Params(0));
  cObs   := fpCmd.Params(1);
  cDescricaoCNF := fpCmd.Params(2);
  cDescricaoFPG := fpCmd.Params(3);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrECF.Suprimento( nValor,                                 { Valor }
                    cObs,                                       { Obs }
                    cDescricaoCNF,                              { DescricaoCNF }
                    cDescricaoFPG)                              { DescricaoFPG }
  end;
end;

{ TMetodoSangria }

{ Params=  nValor - Valor para a Sangria.
           cObs - Observação
           cDescricaoCNF - Descrição do Comprovante não fiscal. Parâmetro pode ser omitido e será considerado "SANGRIA".
           cDescricaoFPG - Descrição da Forma de Pagamento. Parâmetro pode ser omitido e será considerado "DINHEIRO".
}
procedure TMetodoSangria.Executar;
var
  nValor : Double;
  cObs   : String;
  cDescricaoCNF : String;
  cDescricaoFPG : String;
begin
  nValor := StringToFloat(fpCmd.Params(0));
  cObs   := fpCmd.Params(1);
  cDescricaoCNF := fpCmd.Params(2);
  cDescricaoFPG := fpCmd.Params(3);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrECF.Sangria( nValor,                                    { Valor }
                    cObs,                                       { Obs }
                    cDescricaoCNF,                              { DescricaoCNF }
                    cDescricaoFPG)                              { DescricaoFPG }
  end;
end;

{ TMetodoTotalPago }

procedure TMetodoTotalPago.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta:= FloatToStr(ACBrECF.TotalPago);
  end;
end;

{ TMetodoSubTotal }

procedure TMetodoSubTotal.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta:= FloatToStr(ACBrECF.Subtotal);
  end;
end;

{ TMetodoCancelaDescontoAcrescimoSubtotal }

{ Params=  cTipo:  D - Desconto, A - Acréscimo
}
procedure TMetodoCancelaDescontoAcrescimoSubtotal.Executar;
var
  cTipo: String;
begin
  cTipo := fpCmd.Params(0);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrECF.CancelaDescontoAcrescimoSubTotal(PadLeft(Trim(cTipo),1,'D')[1])
  end;
end;

{ TMetodoCancelaDescontoAcrescimoItem }

{ Params=  nItem - Número Item
}
procedure TMetodoCancelaDescontoAcrescimoItem.Executar;
var
  nItem: Integer;
begin
  nItem := StrToInt(fpCmd.Params(0));

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrECF.CancelaDescontoAcrescimoItem(nItem);
  end;
end;

{ TMetodoCancelaItemVendidoParcial }

{ Params=  nItem - Número Item
           nValor - Valor
}
procedure TMetodoCancelaItemVendidoParcial.Executar;
var
  nItem: Integer;
  nValor: Double;
begin
  nItem := StrToInt(fpCmd.Params(0));
  nValor:= StringToFloat(fpCmd.Params(1));

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrECF.CancelaItemVendidoParcial(nItem,
                                      nValor);
  end;
end;

{ TMetodoCancelaItemVendido }

{ Params=  NumItem
}
procedure TMetodoCancelaItemVendido.Executar;
var
  NumItem: Integer;
begin
  NumItem := StrToInt(fpCmd.Params(0));

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrECF.CancelaItemVendido(NumItem);
  end;
end;

{ TMetodoCancelaCupom }

procedure TMetodoCancelaCupom.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrECF.CancelaCupom;
  end;
end;

{ TMetodoFechaCupom }

{ Params=  cMensagemRodape - Parâmetro opcional. Informe até 8 linhas de mensagem a serem impressas no rodapé do cupom. Se esse parâmetro for omitido aqui, porém foi informado em ECF.SubtotalizaCupom , o texto informado anteriormente será utilizado.
}
procedure TMetodoFechaCupom.Executar;
var
  cMensagemRodape: String;
begin
  cMensagemRodape := fpCmd.Params(0);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrECF.FechaCupom( cMensagemRodape );
  end;
end;

{ TMetodoEstornaPagamento }

{ Params=  cCodFormaPagtoEstornar - Código da Forma de pagamento que será estornado.
           cCodFormaPagtoEfetivar - Código da Forma de pagamento que será efetivado.
           nValor - Valor do pagamento que será estornado.
           cObservacao - Parâmetro opcional. Observação ( Mensagem promocional)
}
procedure TMetodoEstornaPagamento.Executar;
var
  cCodFormaPagtoEstornar : String;
  cCodFormaPagtoEfetivar : String;
  nValor : Double;
  cObservacao : String;
begin
  cCodFormaPagtoEstornar := fpCmd.Params(0);
  cCodFormaPagtoEfetivar := fpCmd.Params(1);
  nValor                 := StringToFloat(fpCmd.Params(2));
  cObservacao            := fpCmd.Params(3);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrECF.EstornaPagamento( cCodFormaPagtoEstornar,                 { CodFormaPagamentoEstornar }
                              cCodFormaPagtoEfetivar,                 { CodFormaPagamentoEfetivar }
                              nValor,                                 { Valor }
                              cObservacao )                           { Observacao }
  end;
end;

{ TMetodoEfetuaPagamento }

{ Params=  cCodFormaPagto - Índice da Forma de pagamento cadastrada no ECF. Para conhecer todas as Formas de pagamento cadastradas e seus respectivos índices, utilize o comando ECF.FormasPagamento
           nValor - Valor pago para essa forma de pagamento.
           cObservacao - Pode ser omitido. Alguns ECFs permitem a impressão de até 2 linhas de observação para cada forma de pagamento
           bImprimeVinculado - Pode ser omitido, nesse caso assume "False". Se for informado "True" para este parâmetro, o ACBr apenas verifica se é permitido imprimir Cupom Não Fiscal Vinculado para essa forma de Pagamento.
}
procedure TMetodoEfetuaPagamento.Executar;
var
  cCodFormaPagto : String;
  nValor : Double;
  cObservacao : String;
  bImprimeVinculado : Boolean;
begin
  cCodFormaPagto    := fpCmd.Params(0);
  nValor            := StringToFloat(fpCmd.Params(1));
  cObservacao       := fpCmd.Params(2);
  bImprimeVinculado := StrToBoolDef(fpCmd.Params(3),False);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrECF.EfetuaPagamento( cCodFormaPagto,                 { CodFormaPagamento }
                             nValor,                         { Valor }
                             cObservacao,                    { Observacao }
                             bImprimeVinculado )             { Imp.Vinculado }
  end;
end;

{ TMetodoSubTotalizaCupom }

{ Params=  nDescontoAcrescimo - Parâmetro opcional. Para Descontos, informe valores negativos, para acréscimos valores positivos
           cMensagemRodape – Parâmetro opcional. Informe até 8 linhas de mensagem a serem impressas no rodapé do cupom. Usado apenas para o ECF DataRegis que não possui o método FechaCupom, nos demais ECFs, se a mensagem for informada nesse momento, ela será armazenada pelo ACBr e utilizada em FechaCupom
}
procedure TMetodoSubTotalizaCupom.Executar;
var
  nDescontoAcrescimo : Double;
  cMensagemRodape : String;
begin
  nDescontoAcrescimo := StringToFloatDef(fpCmd.Params(0),0);
  cMensagemRodape    := fpCmd.Params(1);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrECF.SubtotalizaCupom( nDescontoAcrescimo,  {Acresc/Desc}
                             cMensagemRodape )      { Msg.Rodape = ''}
  end;
end;

{ TMetodoDescontoAcrescimoItemAnterior }

{ Params=  nValorDescontoAcrescimo - Valor do Desconto/Acréscimo.
           cDescontoAcrescimo - Informa "D" para desconto e "A" para Acréscimo, se parâmetro for omitido será considerado desconto.
           cTipoDescontoAcrescimo - Informar "%" para tipo em porcentagem ou "$" para valor, se parâmetro for omitido será considerado por porcentagem.
           nNumItem - Número do item a ser atribuido o desconto ou acréscimo, se parâmetro for omitido será considerado o item anterior
}
procedure TMetodoDescontoAcrescimoItemAnterior.Executar;
var
  nValorDescontoAcrescimo : Double;
  cDescontoAcrescimo      : String;
  cTipoDescontoAcrescimo  : String;
  nNumItem                : Integer;
begin
  nValorDescontoAcrescimo := StringToFloat(fpCmd.Params(0));
  cDescontoAcrescimo      := fpCmd.Params(1);
  cTipoDescontoAcrescimo  := fpCmd.Params(2);
  nNumItem                := StrToIntDef(fpCmd.Params(3),0);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrECF.DescontoAcrescimoItemAnterior(
                 nValorDescontoAcrescimo,                   { Valor }
                 PadLeft(cDescontoAcrescimo,1,'D'),         { 'D' ou 'A' }
                 PadLeft(cTipoDescontoAcrescimo,1,'%'),     { '%' ou '$' }
                 nNumItem)                                  { NumItem = 0 }
  end;
end;

{ TMetodoVendeItem }

{ Params=  cCodigo - Texto com o código do produto, geralmente á aceito até 13 caracteres, alguns ECFs apenas aceitam numéricos no código.
           cDescricao - Texto com a descrição do Produto vendido. Procure não usar acentos, pois alguns ECFs não aceitam caracteres acentuados. Para imprimir Descrições "grandes" habilite a opção "Descrição Grande" no ACBrMonitor.
           cAliquotaICMS -  Texto com a representação da Alíquota do ICMS.
           nQtd - Quantidade de Produtos a Vender. Permite valores com até 3 casas decimais. O ACBr verifica quantas casas decimais existem no valor informado e utiliza o comando apropriado para o ECF, otimizando a impressão para Inteiros o 2 casas decimais, sempre que possível
           nValorUnitario - Preço Unitário do produto vendido. Permite valores com até 3 casas decimais. O ACBr verifica quantas casas decimais existem no valor informado e utiliza o comando apropriado para o ECF, otimizando a impressão para 2 casas decimais, sempre que possível
           nValorDescontoAcrescimo - Parâmetro opcional, Se necessário, informe a Porcentagem de Desconto a aplicar no item Vendido. Dependendo do ECF o valor e porcentagem do Desconto será impresso no Cupom.
           cUnidade - Parâmetro opcional, Se necessário, informe o Texto com a unidade de medida do Item. Exemplo: "UN", "LT", "MT", "KG", etc
           cTipoDescontoAcrescimo - Parâmetro opcional. Informar "%" para porcentagem ou "$" para valor.Se parâmetro omitido será considerado em porcentagem "%".
           cDescontoAcrescimo - Parâmetro opcional. Informar "A" para acréscimo ou "D" para desconto.Se parâmetro omitido será considerado Desconto "D"
           nCodDepartamento - Parâmetro opcional. Código de departamento.
}
procedure TMetodoVendeItem.Executar;
var
  cCodigo: String;
  cDescricao: String;
  cAliquotaICMS: String;
  nQtd: Double;
  nValorUnitario: Double;
  nValorDescontoAcrescimo: Double;
  cUnidade: String;
  cTipoDescontoAcrescimo: String;
  cDescontoAcrescimo: String;
begin
  cCodigo           := fpCmd.Params(0);
  cDescricao        := fpCmd.Params(1);
  cAliquotaICMS     := Trim(fpCmd.Params(2));
  nQtd              := StringToFloat(fpCmd.Params(3));
  nValorUnitario    := StringToFloat(fpCmd.Params(4));
  nValorDescontoAcrescimo := StringToFloatDef(fpCmd.Params(5),0);
  cUnidade          := fpCmd.Params(6);
  cTipoDescontoAcrescimo := fpCmd.Params(7);
  cDescontoAcrescimo:= fpCmd.Params(8);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrECF.VendeItem( cCodigo,
                       cDescricao,
                       cAliquotaICMS,
                       nQtd,
                       nValorUnitario,
                       nValorDescontoAcrescimo,
                       cUnidade,
                       PadLeft(cTipoDescontoAcrescimo,1,'%'),
                       PadLeft(cDescontoAcrescimo,1,'D') );
    { Aguarda 1 segundo ou até o ECF ficar Em linha novamente }
    ACBrECF.EmLinha( 1 ) ;
    { O comando acima é util para evitar erros na Impressao de Itens NAO
           concomitante, (imprimir todo o cupom), Pois o método "VendeItem" NAO
           seta "AguardaImpressao := True" para ficar mais rápido }
  end;

end;

{ TMetodoLegendaInmetroProximoItem }

procedure TMetodoLegendaInmetroProximoItem.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrECF.LegendaInmetroProximoItem;
  end;
end;

{ TMetodoAbreCupom }

{ Params=  sCNPJCPF - String
           sNome - String
           sEndereco - String
}
procedure TMetodoAbreCupom.Executar;
var
  sCNPJCPF: String;
  sNome: String;
  sEndereco: String;
begin
  sCNPJCPF := fpCmd.Params(0);
  sNome := fpCmd.Params(1);
  sEndereco := fpCmd.Params(2);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrECF.AbreCupom( sCNPJCPF,
                       sNome,
                       sEndereco );
  end;
end;

{ TMetodoIdentificaPaf }

{ Params=  slinha1 - String
           slinha2 - String
}
procedure TMetodoIdentificaPaf.Executar;
var
  slinha1: String;
  slinha2: String;
begin
  slinha1 := fpCmd.Params(0);
  slinha2 := fpCmd.Params(1);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrECF.IdentificaPAF( slinha1,
                           slinha2 );
  end;

end;

{ TMetodoIdentificaConsumidor }

{ Params=  sCNPJCPF - String
           sNome - String
           sEndereco - String
}
procedure TMetodoIdentificaConsumidor.Executar;
var
  sCNPJCPF: String;
  sNome: String;
  sEndereco: String;
begin
  sCNPJCPF := fpCmd.Params(0);
  sNome := fpCmd.Params(1);
  sEndereco := fpCmd.Params(2);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrECF.IdentificaConsumidor( sCNPJCPF,
                                  sNome,
                                  sEndereco );
  end;
end;

{ TMetodoIdentificaOperador }

{ Params=  sNome - String
}
procedure TMetodoIdentificaOperador.Executar;
var
  sNome: String;
begin
  sNome := fpCmd.Params(0);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrECF.IdentificaOperador( sNome );
  end;
end;

{ TMetodoTestaPodeAbrirCupom }

procedure TMetodoTestaPodeAbrirCupom.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrECF.TestaPodeAbrirCupom;
  end;
end;

{ TMetodoAchaRgDescricao }

{ Params=  sDescricao - String descrição
           sBusca - String para posição
}
procedure TMetodoAchaRgDescricao.Executar;
var
  sDescricao: String;
  sBusca: Boolean;
begin
  sDescricao:= fpCmd.Params(0);
  sBusca:= strToBoolDef(fpCmd.Params(1), True);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fREL := ACBrECF.AchaRGDescricao( sDescricao, sBusca ) ;
    if fREL <> nil then
             fpCmd.Resposta := PadLeft( fREL.Indice,4) +
                             PadLeft( fREL.Descricao, 30) +
                             IntToStrZero( fREL.Contador, 5 )
    else
             raise Exception.Create('Relatório Gerencial: '+Trim(sDescricao)+
                                     ' não encontrado');
  end;
end;

{ TMetodoProgramaRelatoriosGerenciais }

{ Params=  sDescricao - String descrição
           sPosicao - String para posição
}
procedure TMetodoProgramaRelatoriosGerenciais.Executar;
var
  sDescricao: String;
  sPosicao: String;
begin
  sDescricao:= fpCmd.Params(0);
  sPosicao:= fpCmd.Params(1);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrECF.ProgramaRelatoriosGerenciais( sDescricao , sPosicao ) ;
  end;
end;

{ TMetodoLerTotaisRelatoriosGerenciais }

procedure TMetodoLerTotaisRelatoriosGerenciais.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := PegaTotaisRelatoriosGerenciais;
  end;
end;

{ TMetodoCarregaRelatoriosGerenciais }

procedure TMetodoCarregaRelatoriosGerenciais.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrECF.CarregaRelatoriosGerenciais ;
    fpCmd.Resposta := PegaRelatoriosGerenciais;
  end;
end;

{ TMetodoRelatoriosGerenciais }

procedure TMetodoRelatoriosGerenciais.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := PegaRelatoriosGerenciais;
  end;
end;

{ TMetodoProgramaUnidadeMedida }

{ Params=  sDescricao - String descrição
}
procedure TMetodoProgramaUnidadeMedida.Executar;
var
  sDescricao: String;
begin
  sDescricao:= fpCmd.Params(0);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrECF.ProgramaUnidadeMedida(sDescricao);
  end;
end;

{ TMetodoCarregaUnidadesMedida }

procedure TMetodoCarregaUnidadesMedida.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrECF.CarregaUnidadesMedida ;
    fpCmd.Resposta := PegaUnidadesMedida;
  end;
end;

{ TMetodoUnidadesMedida }

procedure TMetodoUnidadesMedida.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := PegaUnidadesMedida;
  end;
end;

{ TMetodoAchaCnfDescricao }

{ Params=  sDescricao - String descrição
           bBusca - Boolean Busca Exata
}
procedure TMetodoAchaCnfDescricao.Executar;
var
  sDescricao: String;
  bBusca: Boolean;
begin
  sDescricao:= fpCmd.Params(0);
  bBusca:=  StrToBoolDef( fpCmd.Params(1), true);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fCNF := ACBrECF.AchaCNFDescricao( sDescricao,
                             bBusca ) ;
    if fCNF <> nil then
      fpCmd.Resposta := PadLeft( fCNF.Indice,4) +
                                IfThen(fCNF.PermiteVinculado,'V',' ')+
                                PadLeft( fCNF.Descricao,30) +
                                PadLeft( fCNF.FormaPagamento,4)
    else
      raise Exception.Create('Comprovante Não Fiscal: '+ sDescricao +
                                       ' não encontrado');

  end;
end;

{ TMetodoProgramaComprovanteNaoFiscal }

{ Params=  sDescricao - String descrição
           sTipo - String com Tipo
           sPosicao - String para posição
}
procedure TMetodoProgramaComprovanteNaoFiscal.Executar;
var
  sDescricao: String;
  sTipo: String;
  sPosicao: String;
begin
  sDescricao:= fpCmd.Params(0);
  sTipo:= fpCmd.Params(1);
  sPosicao:= fpCmd.Params(2);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrECF.ProgramaComprovanteNaoFiscal( sDescricao,
                                  sTipo,
                                  sPosicao );
  end;
end;

{ TMetodoLerTotaisComprovanteNaoFiscal }

procedure TMetodoLerTotaisComprovanteNaoFiscal.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := PegaTotaisComprovantesNaoFiscais
  end;
end;

{ TMetodoCarregaComprovantesNaoFiscais }

procedure TMetodoCarregaComprovantesNaoFiscais.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrECF.CarregaComprovantesNaoFiscais;
    fpCmd.Resposta := PegaComprovantesNaoFiscais;
  end;
end;

{ TMetodoComprovantesNaoFiscais }

procedure TMetodoComprovantesNaoFiscais.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := PegaComprovantesNaoFiscais;
  end;
end;

{ TMetodoAchaFpgDescricao }

{ Params=  sDescricao  - String descrição
           bBuscaExata - booleano para busca
}
procedure TMetodoAchaFpgDescricao.Executar;
var
  sDescricao: String;
  bBuscaExata: Boolean;
begin
  sDescricao:= fpCmd.Params(0);
  bBuscaExata:=  StrToBoolDef( fpCmd.Params(1), true);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fFPG := ACBrECF.AchaFPGDescricao( sDescricao,
                                      bBuscaExata );
    if fFPG <> nil then
      fpCmd.Resposta := PadLeft( fFPG.Indice,4)+
                                IfThen(fFPG.PermiteVinculado,'V',' ')+
                                PadLeft(fFPG.Descricao, 30)
    else
      raise Exception.Create('Forma de Pagamento: '+Trim(sDescricao)+
                                       ' não encontrada');

  end;
end;

{ TMetodoProgramaFormaPagamento }

{ Params=  sDescricao - String descrição
           bPermitevinculado - Boolena para vinculado
           sPosicao - String para posição
}
procedure TMetodoProgramaFormaPagamento.Executar;
var
  sDescricao: String;
  bPermitevinculado: Boolean;
  sPosicao: String;
begin
  sDescricao:= fpCmd.Params(0);
  bPermitevinculado:=  StrToBoolDef( fpCmd.Params(1), true);
  sPosicao:= fpCmd.Params(2);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrECF.ProgramaFormaPagamento( sDescricao ,
                   bPermitevinculado,
                   sPosicao ) ;
  end;
end;

{ TMetodoLerTotaisFormaPagamento }

procedure TMetodoLerTotaisFormaPagamento.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := PegaTotaisFormasPagamento
  end;
end;

{ TMetodoCarregaFormasPagamento }

procedure TMetodoCarregaFormasPagamento.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrECF.CarregaFormasPagamento;
    fpCmd.Resposta := PegaFormasPagamento;
  end;
end;

{ TMetodoFormasPagamento }

procedure TMetodoFormasPagamento.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := PegaFormasPagamento;
  end;
end;

{ TMetodoAchaICMSAliquota }

{ Params= fAliq : Double com aliq
          cTipo : Tipo: char(T, S)
}
procedure TMetodoAchaICMSAliquota.Executar;
var
  fAliq : Double;
  cTipo : String;
begin
  fAliq := StringToFloat(fpCmd.Params(0));
  cTipo := fpCmd.Params(1);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fICMS := ACBrECF.AchaICMSAliquota( fAliq,
                              PadLeft(cTipo,1)[1]) ;
    if fICMS <> nil then
      fpCmd.Resposta := PadLeft(fICMS.Indice,4) +
                              fICMS.Tipo +
                              FormatFloat('##0.00', fICMS.Aliquota )
    else
      raise Exception.Create('Aliquota: '+
                                     Trim(FloatToStr(fAliq)+' '+cTipo)+
                                     ' não encontrada');
  end;
end;

{ TMetodoProgramaAliquota }

{ Params= fAliq : Double com aliq
          cTipo : Tipo: char(T, S)
          sPosicao : String 2 caracteres
}
procedure TMetodoProgramaAliquota.Executar;
var
  fAliq : Double;
  cTipo : String;
  sPosicao : String;
begin
  fAliq := StringToFloat(fpCmd.Params(0));
  cTipo := fpCmd.Params(1);
  sPosicao := fpCmd.Params(2);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrECF.ProgramaAliquota( fAliq,
                      PadLeft(cTipo,1,'T')[1],
                      sPosicao );
  end;
end;

{ TMetodoLerTotaisAliquota }

procedure TMetodoLerTotaisAliquota.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := PegaTotaisAliquotas
  end;
end;

{ TMetodoCarregaAliquotas }

procedure TMetodoCarregaAliquotas.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrECF.CarregaAliquotas;
    fpCmd.Resposta := PegaAliquotas;
  end;
end;

{ TMetodoPegaAliquotas }

procedure TMetodoPegaAliquotas.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := PegaAliquotas;
  end;
end;

{ TMetodoNumReducoesZRestantes }

procedure TMetodoNumReducoesZRestantes.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrECF.NumReducoesZRestantes;
  end;
end;

{ TMetodoDadosUltimaReducaoZ }

procedure TMetodoDadosUltimaReducaoZ.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrECF.DadosUltimaReducaoZ;
  end;
end;

{ TMetodoDadosReducaoZ }

procedure TMetodoDadosReducaoZ.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrECF.DadosReducaoZ;
  end;
end;

{ TMetodoNumUltItem }

procedure TMetodoNumUltItem.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := IntToStr(ACBrECF.NumUltItem);
  end;
end;

{ TMetodoTotalAcrescimosOPNF }

procedure TMetodoTotalAcrescimosOPNF.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := FloatToStr(ACBrECF.TotalAcrescimosOPNF);
  end;
end;

{ TMetodoTotalDescontosOPNF }

procedure TMetodoTotalDescontosOPNF.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := FloatToStr(ACBrECF.TotalDescontosOPNF);
  end;
end;

{ TMetodoTotalCancelamentosOPNF }

procedure TMetodoTotalCancelamentosOPNF.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := FloatToStr(ACBrECF.TotalCancelamentosOPNF);
  end;
end;

{ TMetodoTotalNaoFiscal }

procedure TMetodoTotalNaoFiscal.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := FloatToStr(ACBrECF.TotalNaoFiscal);
  end;
end;

{ TMetodoTotalIsencaoISSQN }

procedure TMetodoTotalIsencaoISSQN.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := FloatToStr(ACBrECF.TotalIsencaoISSQN);
  end;
end;

{ TMetodoTotalNaoTributadoISSQN }

procedure TMetodoTotalNaoTributadoISSQN.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := FloatToStr(ACBrECF.TotalNaoTributadoISSQN);
  end;
end;


{ TMetodoTotalSubstituicaoTributariaISSQN }

procedure TMetodoTotalSubstituicaoTributariaISSQN.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := FloatToStr(ACBrECF.TotalSubstituicaoTributariaISSQN);
  end;
end;

{ TMetodoTotalAcrescimosISSQN }

procedure TMetodoTotalAcrescimosISSQN.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := FloatToStr(ACBrECF.TotalAcrescimosISSQN);
  end;
end;

{ TMetodoTotalDescontosISSQN }

procedure TMetodoTotalDescontosISSQN.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := FloatToStr(ACBrECF.TotalDescontosISSQN);
  end;
end;

{ TMetodoTotalCancelamentosISSQN }

procedure TMetodoTotalCancelamentosISSQN.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := FloatToStr(ACBrECF.TotalCancelamentosISSQN);
  end;
end;

{ TMetodoTotalIsencao }

procedure TMetodoTotalIsencao.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := FloatToStr(ACBrECF.TotalIsencao);
  end;
end;

{ TMetodoTotalNaoTributado }

procedure TMetodoTotalNaoTributado.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := FloatToStr(ACBrECF.TotalNaoTributado);
  end;
end;

{ TMetodoTotalSubstituicaoTributaria }

procedure TMetodoTotalSubstituicaoTributaria.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := FloatToStr(ACBrECF.TotalSubstituicaoTributaria);
  end;
end;

{ TMetodoTotalAcrescimos }

procedure TMetodoTotalAcrescimos.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := FloatToStr(ACBrECF.TotalAcrescimos);
  end;
end;

{ TMetodoTotalDescontos }

procedure TMetodoTotalDescontos.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := FloatToStr(ACBrECF.TotalDescontos);
  end;
end;

{ TMetodoTotalCancelamentos }

procedure TMetodoTotalCancelamentos.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := FloatToStr(ACBrECF.TotalCancelamentos);
  end;
end;

{ TMetodoTotalTroco }

procedure TMetodoTotalTroco.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := FloatToStr(ACBrECF.TotalTroco);
  end;
end;

{ TMetodoGrandeTotal }

procedure TMetodoGrandeTotal.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := FloatToStr(ACBrECF.GrandeTotal);
  end;
end;

{ TMetodoVendaBruta }

procedure TMetodoVendaBruta.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := FloatToStr(ACBrECF.VendaBruta);
  end;
end;

{ TMetodoNumCOOInicial }

procedure TMetodoNumCOOInicial.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrECF.NumCOOInicial;
  end;
end;

{ TMetodoPAF }

procedure TMetodoPAF.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrECF.PAF;
  end;
end;

{ TMetodoSubModeloECF }

procedure TMetodoSubModeloECF.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrECF.SubModeloECF;
  end;
end;

{ TMetodoDecimaisPreco }

procedure TMetodoDecimaisPreco.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := IntToStr(ACBrECF.DecimaisPreco);
  end;
end;

{ TMetodoDecimaisQtd }

procedure TMetodoDecimaisQtd.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := IntToStr(ACBrECF.DecimaisQtd);
  end;
end;

{ TMetodoDataHoraUltimaReducaoZ }

procedure TMetodoDataHoraUltimaReducaoZ.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := FormatDateTime('dd/mm/yy hh:nn:ss', ACBrECF.DataHoraUltimaReducaoZ);
  end;
end;

{ TMetodoDataHoraSB }

procedure TMetodoDataHoraSB.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := FormatDateTime('dd/mm/yy hh:nn:ss', ACBrECF.DataHoraSB);
  end;
end;

{ TMetodoUsuarioAtual }

procedure TMetodoUsuarioAtual.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrECF.UsuarioAtual;
  end;
end;

{ TMetodoCliche }

procedure TMetodoCliche.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrECF.Cliche;
  end;
end;

{ TMetodoIM }

procedure TMetodoIM.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrECF.IM;
  end;
end;

{ TMetodoIE }

procedure TMetodoIE.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrECF.IE;
  end;
end;

{ TMetodoCNPJ }

procedure TMetodoCNPJ.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrECF.CNPJ;
  end;
end;

{ TMetodoDataMovimento }

procedure TMetodoDataMovimento.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
     fpCmd.Resposta := FormatDateTime('dd/mm/yy', ACBrECF.DataMovimento);
  end;
end;

{ TMetodoRFDID }

procedure TMetodoRFDID.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrECF.RFDID;
  end;
end;

{ TMetodoMFAdicional }

procedure TMetodoMFAdicional.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrECF.MFAdicional;
  end;
end;

{ TMetodoNumVersao }

procedure TMetodoNumVersao.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrECF.NumVersao;
  end;
end;

{ TMetodoNumSerieMFD }

procedure TMetodoNumSerieMFD.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrECF.NumSerieMFD;
  end;
end;

{ TMetodoNumSerie }

procedure TMetodoNumSerie.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrECF.NumSerie;
  end;
end;

{ TMetodoNumECF }

procedure TMetodoNumECF.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrECF.NumECF;
  end;
end;

{ TMetodoNumCRZ }

procedure TMetodoNumCRZ.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrECF.NumCRZ;
  end;
end;

{ TMetodoNumNCN }

procedure TMetodoNumNCN.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrECF.NumNCN;
  end;
end;

{ TMetodoNumCFD }

procedure TMetodoNumCFD.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrECF.NumCFD;
  end;
end;

{ TMetodoNumCCDC }

procedure TMetodoNumCCDC.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrECF.NumCCDC;
  end;
end;

{ TMetodoNumCFC }

procedure TMetodoNumCFC.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrECF.NumCFC;
  end;
end;

{ TMetodoNumCDC }

procedure TMetodoNumCDC.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrECF.NumCDC;
  end;
end;

{ TMetodoNumGNFC }

procedure TMetodoNumGNFC.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrECF.NumGNFC;
  end;
end;

{ TMetodoNumGNF }

procedure TMetodoNumGNF.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrECF.NumGNF;
  end;
end;

{ TMetodoNumGRG }

procedure TMetodoNumGRG.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrECF.NumGRG;
  end;
end;

{ TMetodoNumCCF }

procedure TMetodoNumCCF.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrECF.NumCCF;
  end;
end;

{ TMetodoNumCRO }

procedure TMetodoNumCRO.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrECF.NumCRO;
  end;
end;

{ TMetodoNumLoja }

procedure TMetodoNumLoja.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrECF.NumLoja;
  end;
end;

{ TMetodoNumCupom }

procedure TMetodoNumCupom.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrECF.NumCupom ;
    if fpCmd.Resposta = '' then
      raise Exception.Create('Erro na leitura do COO');
  end;
end;

{ TMetodoDataHora }

procedure TMetodoDataHora.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := FormatDateTime('dd/mm/yy hh:nn:ss', ACBrECF.DataHora );
  end;
end;

{ TMetodoSetMaxLinhasBuffer }

{ iMaxLinhas: Integer com max de linhas em buffer
}
procedure TMetodoSetMaxLinhasBuffer.Executar;
var
   iMaxLinhas : Integer;
begin
  iMaxLinhas := StrToInt(fpCmd.Params(0));

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrECF.MaxLinhasBuffer:= iMaxLinhas;
  end;
end;

{ TMetodoMaxLinhasBuffer }

procedure TMetodoMaxLinhasBuffer.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := IntToStr(ACBrECF.MaxLinhasBuffer);
  end;
end;

{ TMetodoPaginaDeCodigo }

procedure TMetodoPaginaDeCodigo.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := IntToStr(ACBrECF.PaginaDeCodigo);
  end;
end;

{ TMetodoLinhasEntreCupons }

procedure TMetodoLinhasEntreCupons.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := IntToStr(ACBrECF.LinhasEntreCupons);
  end;
end;

{ TMetodoBloqueiaMouseTeclado }

procedure TMetodoBloqueiaMouseTeclado.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := BoolToStr(ACBrECF.BloqueiaMouseTeclado, true);
  end;
end;

{ TMetodoExibeMensagem }

procedure TMetodoExibeMensagem.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := BoolToStr(ACBrECF.ExibeMensagem, true);
  end;
end;

{ TMetodoMsgPoucoPapel }

procedure TMetodoMsgPoucoPapel.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := IntToStr(ACBrECF.MsgPoucoPapel);
  end;
end;

{ TMetodoMsgTrabalhando }

procedure TMetodoMsgTrabalhando.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrECF.MsgTrabalhando;
  end;
end;

{ TMetodoMsgAguarde }

procedure TMetodoMsgAguarde.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrECF.MsgAguarde;
  end;
end;

{ TMetodoOperador }

procedure TMetodoOperador.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrECF.Operador;
  end;
end;

{ TMetodoControlePorta }

procedure TMetodoControlePorta.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := BoolToStr( ACBrECF.ControlePorta, true );
  end;
end;

{ TMetodoIgnorarTagsFormatacao }

procedure TMetodoIgnorarTagsFormatacao.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := BoolToStr( ACBrECF.IgnorarTagsFormatacao, true );
  end;
end;


{ TMetodoGavetaSinalInvertido }

procedure TMetodoGavetaSinalInvertido.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := BoolToStr( ACBrECF.GavetaSinalInvertido, True );
  end;
end;

{ TMetodoDescricaoGrande }

procedure TMetodoDescricaoGrande.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := BoolToStr( ACBrECF.DescricaoGrande, True );
  end;
end;

{ TMetodoIntervaloAposComando }

procedure TMetodoIntervaloAposComando.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := IntToStr( ACBrECF.IntervaloAposComando );
  end;
end;

{ TMetodoSetTimeOut }

{ Params: 0 - iTimeOut
}
procedure TMetodoSetTimeOut.Executar;
var
   iTimeOut: Integer;
begin
  iTimeOut := StrToInt(fpCmd.Params(0));

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrECF.TimeOut:= iTimeOut;
  end;
end;

{ TMetodoTimeOut }

procedure TMetodoTimeOut.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := IntToStr(ACBrECF.TimeOut);
  end;
end;

{ TMetodoPorta }

procedure TMetodoPorta.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrECF.Porta;
  end;
end;

{ TMetodoModelo }

procedure TMetodoModelo.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := GetEnumName(TypeInfo(TACBrECFModelo),Integer(ACBrECF.Modelo));
  end;
end;

{ TMetodoModeloStr }

procedure TMetodoModeloStr.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrECF.ModeloStr;
  end;
end;

{ TMetodoRespostaComando }

procedure TMetodoRespostaComando.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrECF.RespostaComando;
  end;
end;

{ TMetodoComandoEnviado }

procedure TMetodoComandoEnviado.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrECF.ComandoEnviado;
  end;
end;

{ TMetodoEnviaInfo }

{ Params: 0 - sRegistrador
}
procedure TMetodoEnviaInfo.Executar;
var
   sRegistrador: String;
begin
  sRegistrador := fpCmd.Params(0);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := ACBrECF.RetornaInfoECF(sRegistrador);
  end;
end;

{ TMetodoParamDescontoIssqn }

procedure TMetodoParamDescontoIssqn.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := BoolToStr(ACBrECF.ParamDescontoISSQN, True);
  end;
end;

{ TMetodoColunas }

procedure TMetodoColunas.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := IntToStr(ACBrECF.Colunas);
  end;
end;

{ TMetodoAtivo }

procedure TMetodoAtivo.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := BoolToStr(ACBrECF.Ativo, True);
  end;
end;

{ TMetodoDesativar }

procedure TMetodoDesativar.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrECF.Desativar;
  end;
end;

{ TMetodoAtivar }

procedure TMetodoAtivar.Executar;
begin
  with TACBrObjetoECF(fpObjetoDono) do
  begin
    ACBrECF.Ativar;
  end;
end;

{ TMetodoAchar }

{ Params: 0 - bProcuraModelo
          1 - bProcuraPorta
          2 - iTimeOut
}
procedure TMetodoAchar.Executar;
var
   bProcuraModelo : boolean;
   bProcuraPorta  : boolean;
   iTimeout       : integer;
begin
  bProcuraModelo := StrToBoolDef(fpCmd.Params(0),True);
  bProcuraPorta  := StrToBoolDef(fpCmd.Params(1),True);
  iTimeout       := StrToIntDef(fpCmd.Params(2),3);

  with TACBrObjetoECF(fpObjetoDono) do
  begin
    fpCmd.Resposta := BoolToStr( ACBrECF.AcharECF(bProcuraModelo, bProcuraPorta, iTimeout) );
  end;
end;

{ TACBrObjetoECF }

constructor TACBrObjetoECF.Create(AConfig: TMonitorConfig; ACBrECF: TACBrECF; ACBrBlocoX: TACBrBlocoX);
begin
  inherited Create(AConfig);

  fACBrECF:= ACBrECF;
  fACBrBlocoX:= ACBrBlocoX;

  ListaDeMetodos.Add( CMetodoECFachar );
  ListaDeMetodos.Add( CMetodoECFativar );
  ListaDeMetodos.Add( CMetodoECFdesativar );
  ListaDeMetodos.Add( CMetodoECFativo );
  ListaDeMetodos.Add( CMetodoECFcolunas );
  ListaDeMetodos.Add( CMetodoECFparamdescontoissqn );
  ListaDeMetodos.Add( CMetodoECFenviainfo );
  ListaDeMetodos.Add( CMetodoECFretornainfoecf );
  ListaDeMetodos.Add( CMetodoECFcomandoenviado );
  ListaDeMetodos.Add( CMetodoECFrespostacomando );
  ListaDeMetodos.Add( CMetodoECFmodelostr );
  ListaDeMetodos.Add( CMetodoECFmodelo );
  ListaDeMetodos.Add( CMetodoECFporta );
  ListaDeMetodos.Add( CMetodoECFtimeout );
  ListaDeMetodos.Add( CMetodoECFsettimeout );
  ListaDeMetodos.Add( CMetodoECFintervaloaposcomando );
  ListaDeMetodos.Add( CMetodoECFdescricaogrande );
  ListaDeMetodos.Add( CMetodoECFgavetasinalinvertido );
  ListaDeMetodos.Add( CMetodoECFignorartagsformatacao );
  ListaDeMetodos.Add( CMetodoECFcontroleporta );
  ListaDeMetodos.Add( CMetodoECFoperador );
  ListaDeMetodos.Add( CMetodoECFmsgaguarde );
  ListaDeMetodos.Add( CMetodoECFmsgtrabalhando );
  ListaDeMetodos.Add( CMetodoECFmsgpoucopapel );
  ListaDeMetodos.Add( CMetodoECFexibemensagem );
  ListaDeMetodos.Add( CMetodoECFbloqueiamouseteclado );
  ListaDeMetodos.Add( CMetodoECFlinhasentrecupons );
  ListaDeMetodos.Add( CMetodoECFpaginadecodigo );
  ListaDeMetodos.Add( CMetodoECFmaxlinhasbuffer );
  ListaDeMetodos.Add( CMetodoECFsetmaxlinhasbuffer );
  ListaDeMetodos.Add( CMetodoECFdatahora );
  ListaDeMetodos.Add( CMetodoECFnumcupom );
  ListaDeMetodos.Add( CMetodoECFnumcoo );
  ListaDeMetodos.Add( CMetodoECFnumloja );
  ListaDeMetodos.Add( CMetodoECFnumcro );
  ListaDeMetodos.Add( CMetodoECFnumccf );
  ListaDeMetodos.Add( CMetodoECFnumgrg );
  ListaDeMetodos.Add( CMetodoECFnumgnf );
  ListaDeMetodos.Add( CMetodoECFnumgnfc );
  ListaDeMetodos.Add( CMetodoECFnumcdc );
  ListaDeMetodos.Add( CMetodoECFnumcfc );
  ListaDeMetodos.Add( CMetodoECFnumccdc );
  ListaDeMetodos.Add( CMetodoECFnumcfd );
  ListaDeMetodos.Add( CMetodoECFnumncn );
  ListaDeMetodos.Add( CMetodoECFnumcrz );
  ListaDeMetodos.Add( CMetodoECFnumecf );
  ListaDeMetodos.Add( CMetodoECFnumserie );
  ListaDeMetodos.Add( CMetodoECFnumseriemfd );
  ListaDeMetodos.Add( CMetodoECFnumversao );
  ListaDeMetodos.Add( CMetodoECFmfadicional );
  ListaDeMetodos.Add( CMetodoECFrfdid );
  ListaDeMetodos.Add( CMetodoECFdatamovimento );
  ListaDeMetodos.Add( CMetodoECFcnpj );
  ListaDeMetodos.Add( CMetodoECFie );
  ListaDeMetodos.Add( CMetodoECFim );
  ListaDeMetodos.Add( CMetodoECFcliche );
  ListaDeMetodos.Add( CMetodoECFusuarioatual );
  ListaDeMetodos.Add( CMetodoECFdatahorasb );
  ListaDeMetodos.Add( CMetodoECFdatahoraultimareducaoz );
  ListaDeMetodos.Add( CMetodoECFdecimaisqtd );
  ListaDeMetodos.Add( CMetodoECFdecimaispreco );
  ListaDeMetodos.Add( CMetodoECFsubmodeloecf );
  ListaDeMetodos.Add( CMetodoECFpaf );
  ListaDeMetodos.Add( CMetodoECFnumcooinicial );
  ListaDeMetodos.Add( CMetodoECFvendabruta );
  ListaDeMetodos.Add( CMetodoECFgrandetotal );
  ListaDeMetodos.Add( CMetodoECFtotaltroco );
  ListaDeMetodos.Add( CMetodoECFtotalcancelamentos );
  ListaDeMetodos.Add( CMetodoECFtotaldescontos );
  ListaDeMetodos.Add( CMetodoECFtotalacrescimos );
  ListaDeMetodos.Add( CMetodoECFtotalsubstituicaotributaria );
  ListaDeMetodos.Add( CMetodoECFtotalnaotributado );
  ListaDeMetodos.Add( CMetodoECFtotalisencao );
  ListaDeMetodos.Add( CMetodoECFtotalcancelamentosissqn );
  ListaDeMetodos.Add( CMetodoECFtotaldescontosissqn );
  ListaDeMetodos.Add( CMetodoECFtotalacrescimosissqn );
  ListaDeMetodos.Add( CMetodoECFtotalsubstituicaotributariaissqn );
  ListaDeMetodos.Add( CMetodoECFtotalnaotributadoissqn );
  ListaDeMetodos.Add( CMetodoECFtotalisencaoissqn );
  ListaDeMetodos.Add( CMetodoECFtotalnaofiscal );
  ListaDeMetodos.Add( CMetodoECFtotalcancelamentosopnf );
  ListaDeMetodos.Add( CMetodoECFtotaldescontosopnf );
  ListaDeMetodos.Add( CMetodoECFtotalacrescimosopnf );
  ListaDeMetodos.Add( CMetodoECFnumultitem );
  ListaDeMetodos.Add( CMetodoECFdadosreducaoz );
  ListaDeMetodos.Add( CMetodoECFdadosultimareducaoz );
  ListaDeMetodos.Add( CMetodoECFnumreducoeszrestantes );
  ListaDeMetodos.Add( CMetodoECFaliquotas );
  ListaDeMetodos.Add( CMetodoECFcarregaaliquotas );
  ListaDeMetodos.Add( CMetodoECFlertotaisaliquota );
  ListaDeMetodos.Add( CMetodoECFprogramaaliquota );
  ListaDeMetodos.Add( CMetodoECFachaicmsaliquota );
  ListaDeMetodos.Add( CMetodoECFformaspagamento );
  ListaDeMetodos.Add( CMetodoECFcarregaformaspagamento );
  ListaDeMetodos.Add( CMetodoECFlertotaisformapagamento );
  ListaDeMetodos.Add( CMetodoECFprogramaformapagamento );
  ListaDeMetodos.Add( CMetodoECFachafpgdescricao );
  ListaDeMetodos.Add( CMetodoECFcomprovantesnaofiscais );
  ListaDeMetodos.Add( CMetodoECFcarregacomprovantesnaofiscais );
  ListaDeMetodos.Add( CMetodoECFlertotaiscomprovantenaofiscal );
  ListaDeMetodos.Add( CMetodoECFprogramacomprovantenaofiscal );
  ListaDeMetodos.Add( CMetodoECFachacnfdescricao );
  ListaDeMetodos.Add( CMetodoECFunidadesmedida );
  ListaDeMetodos.Add( CMetodoECFcarregaunidadesmedida );
  ListaDeMetodos.Add( CMetodoECFprogramaunidademedida );
  ListaDeMetodos.Add( CMetodoECFrelatoriosgerenciais );
  ListaDeMetodos.Add( CMetodoECFcarregarelatoriosgerenciais );
  ListaDeMetodos.Add( CMetodoECFlertotaisrelatoriosgerenciais );
  ListaDeMetodos.Add( CMetodoECFprogramarelatoriosgerenciais );
  ListaDeMetodos.Add( CMetodoECFachargdescricao );
  ListaDeMetodos.Add( CMetodoECFtestapodeabrircupom );
  ListaDeMetodos.Add( CMetodoECFidentificaoperador );
  ListaDeMetodos.Add( CMetodoECFidentificaconsumidor );
  ListaDeMetodos.Add( CMetodoECFidentificapaf );
  ListaDeMetodos.Add( CMetodoECFabrecupom );
  ListaDeMetodos.Add( CMetodoECFlegendainmetroproximoitem );
  ListaDeMetodos.Add( CMetodoECFvendeitem );
  ListaDeMetodos.Add( CMetodoECFdescontoacrescimoitemanterior );
  ListaDeMetodos.Add( CMetodoECFsubtotalizacupom );
  ListaDeMetodos.Add( CMetodoECFefetuapagamento );
  ListaDeMetodos.Add( CMetodoECFestornapagamento );
  ListaDeMetodos.Add( CMetodoECFfechacupom );
  ListaDeMetodos.Add( CMetodoECFcancelacupom );
  ListaDeMetodos.Add( CMetodoECFcancelaitemvendido );
  ListaDeMetodos.Add( CMetodoECFcancelaitemvendidoparcial );
  ListaDeMetodos.Add( CMetodoECFcanceladescontoacrescimoitem );
  ListaDeMetodos.Add( CMetodoECFcanceladescontoacrescimosubtotal );
  ListaDeMetodos.Add( CMetodoECFsubtotal );
  ListaDeMetodos.Add( CMetodoECFtotalpago );
  ListaDeMetodos.Add( CMetodoECFsangria );
  ListaDeMetodos.Add( CMetodoECFsuprimento );
  ListaDeMetodos.Add( CMetodoECFcortapapel );
  ListaDeMetodos.Add( CMetodoECFnaofiscalcompleto );
  ListaDeMetodos.Add( CMetodoECFabrenaofiscal );
  ListaDeMetodos.Add( CMetodoECFregistraitemnaofiscal );
  ListaDeMetodos.Add( CMetodoECFcancelaitemnaofiscal );
  ListaDeMetodos.Add( CMetodoECFsubtotalizanaofiscal );
  ListaDeMetodos.Add( CMetodoECFefetuapagamentonaofiscal );
  ListaDeMetodos.Add( CMetodoECFfechanaofiscal );
  ListaDeMetodos.Add( CMetodoECFcancelanaofiscal );
  ListaDeMetodos.Add( CMetodoECFleiturax );
  ListaDeMetodos.Add( CMetodoECFpafmf_lx_impressao );
  ListaDeMetodos.Add( CMetodoECFleituraxserial );
  ListaDeMetodos.Add( CMetodoECFreducaoz );
  ListaDeMetodos.Add( CMetodoECFtipoultimodocumento );
  ListaDeMetodos.Add( CMetodoECFpoucopapel );
  ListaDeMetodos.Add( CMetodoECFhorarioverao );
  ListaDeMetodos.Add( CMetodoECFarredonda );
  ListaDeMetodos.Add( CMetodoECFarredondaporqtd );
  ListaDeMetodos.Add( CMetodoECFarredondaitemmfd );
  ListaDeMetodos.Add( CMetodoECFsetarredondaitemmfd );
  ListaDeMetodos.Add( CMetodoECFmfd );
  ListaDeMetodos.Add( CMetodoECFtermica );
  ListaDeMetodos.Add( CMetodoECFidentificaconsumidorrodape );
  ListaDeMetodos.Add( CMetodoECFestado );
  ListaDeMetodos.Add( CMetodoECFabregaveta );
  ListaDeMetodos.Add( CMetodoECFgavetaaberta );
  ListaDeMetodos.Add( CMetodoECFimprimecheque );
  ListaDeMetodos.Add( CMetodoECFcancelaimpressaocheque );
  ListaDeMetodos.Add( CMetodoECFchequepronto );
  ListaDeMetodos.Add( CMetodoECFleituracmc7 );
  ListaDeMetodos.Add( CMetodoECFmudahorarioverao );
  ListaDeMetodos.Add( CMetodoECFmudaarredondamento );
  ListaDeMetodos.Add( CMetodoECFpreparatef );
  ListaDeMetodos.Add( CMetodoECFcorrigeestadoerro );
  ListaDeMetodos.Add( CMetodoECFabrerelatoriogerencial );
  ListaDeMetodos.Add( CMetodoECFrelatoriogerencial );
  ListaDeMetodos.Add( CMetodoECFpulalinhas );
  ListaDeMetodos.Add( CMetodoECFlinharelatoriogerencial );
  ListaDeMetodos.Add( CMetodoECFabrecupomvinculado );
  ListaDeMetodos.Add( CMetodoECFlinhacupomvinculado );
  ListaDeMetodos.Add( CMetodoECFcupomvinculado );
  ListaDeMetodos.Add( CMetodoECFestornaccd );
  ListaDeMetodos.Add( CMetodoECFsegundaviavinculado );
  ListaDeMetodos.Add( CMetodoECFreimpressaovinculado );
  ListaDeMetodos.Add( CMetodoECFfecharelatorio );
  ListaDeMetodos.Add( CMetodoECFleituramemoriafiscal );
  ListaDeMetodos.Add( CMetodoECFleituramemoriafiscalserial );
  ListaDeMetodos.Add( CMetodoECFleituramfdserial );
  ListaDeMetodos.Add( CMetodoECFarquivomfd_dll );
  ListaDeMetodos.Add( CMetodoECFespelhomfd_dll );
  ListaDeMetodos.Add( CMetodoECFpafmf_lmfc_impressao );
  ListaDeMetodos.Add( CMetodoECFpafmf_lmfc_espelho );
  ListaDeMetodos.Add( CMetodoECFpafmf_lmfc_cotepe1704 );
  ListaDeMetodos.Add( CMetodoECFpafmf_lmfs_impressao );
  ListaDeMetodos.Add( CMetodoECFpafmf_lmfs_espelho );
  ListaDeMetodos.Add( CMetodoECFpafmf_mfd_espelho );
  ListaDeMetodos.Add( CMetodoECFpafmf_mfd_cotepe1704 );
  ListaDeMetodos.Add( CMetodoECFpafmf_gerarcat52 );
  ListaDeMetodos.Add( CMetodoECFassinarblocoxestoque );
  ListaDeMetodos.Add( CMetodoECFassinarblocoxreducaoz );
  ListaDeMetodos.Add( CMetodoECFassinarblocox );
  ListaDeMetodos.Add( CMetodoECFvalidarblocoxestoque );
  ListaDeMetodos.Add( CMetodoECFvalidarblocoxreducaoz );
  ListaDeMetodos.Add( CMetodoECFvalidarblocox );
  ListaDeMetodos.Add( CMetodoECFenviarblocoxestoque );
  ListaDeMetodos.Add( CMetodoECFenviarblocoxreducaoz );
  ListaDeMetodos.Add( CMetodoECFenviarblocox );
  ListaDeMetodos.Add( CMetodoECFconsultarblocox );
  ListaDeMetodos.Add( CMetodoECFenviacomando );
  ListaDeMetodos.Add( CMetodoECFassinaarquivo );
  ListaDeMetodos.Add( CMetodoECFconfigbarras );
  ListaDeMetodos.Add( CMetodoECFpafmf_arqmf_binario );
  ListaDeMetodos.Add( CMetodoECFpafmf_arquivomf );
  ListaDeMetodos.Add( CMetodoECFpafmf_arqmf );
  ListaDeMetodos.Add( CMetodoECFpafmf_arqmfd_binario );
  ListaDeMetodos.Add( CMetodoECFpafmf_arquivomfd );
  ListaDeMetodos.Add( CMetodoECFpafmf_arqmfd );
  ListaDeMetodos.Add( CMetodoECFTransmitirArquivo );
  ListaDeMetodos.Add( CMetodoECFConsultarProcessamentoArq );
  ListaDeMetodos.Add( CMetodoECFCancelarArquivo );
  ListaDeMetodos.Add( CMetodoECFConsultarHistoricoArq );
  ListaDeMetodos.Add( CMetodoECFConsultarPendenciasContrib );
  ListaDeMetodos.Add( CMetodoECFConsultarPendenciasDevPAFECF );
  ListaDeMetodos.Add( CMetodoECFDownloadArquivo );
  ListaDeMetodos.Add( CMetodoECFListarArquivos );
  ListaDeMetodos.Add( CMetodoECFReprocessarArquivo );

end;

procedure TACBrObjetoECF.Executar(ACmd: TACBrCmd);
var
   AMetodoClass: TACBrMetodoClass;
   CmdNum: Integer;
   Ametodo: TACBrMetodo;
begin
  inherited Executar(ACmd);

  CmdNum := ListaDeMetodos.IndexOf(LowerCase(ACmd.Metodo));
  AMetodoClass := Nil;

  case CmdNum of
    0  : AMetodoClass := TMetodoachar;
    1  : AMetodoClass := TMetodoativar;
    2  : AMetodoClass := TMetododesativar;
    3  : AMetodoClass := TMetodoativo;
    4  : AMetodoClass := TMetodocolunas;
    5  : AMetodoClass := TMetodoparamdescontoissqn;
    6  : AMetodoClass := TMetodoenviainfo;
    7  : AMetodoClass := TMetodoenviainfo; //utiliza mesmo metodo
    8  : AMetodoClass := TMetodocomandoenviado;
    9  : AMetodoClass := TMetodorespostacomando;
    10  : AMetodoClass := TMetodomodelostr;
    11  : AMetodoClass := TMetodomodelo;
    12  : AMetodoClass := TMetodoporta;
    13  : AMetodoClass := TMetodotimeout;
    14  : AMetodoClass := TMetodosettimeout;
    15  : AMetodoClass := TMetodointervaloaposcomando;
    16  : AMetodoClass := TMetododescricaogrande;
    17  : AMetodoClass := TMetodogavetasinalinvertido;
    18  : AMetodoClass := TMetodoignorartagsformatacao;
    19  : AMetodoClass := TMetodocontroleporta;
    20  : AMetodoClass := TMetodooperador;
    21  : AMetodoClass := TMetodomsgaguarde;
    22  : AMetodoClass := TMetodomsgtrabalhando;
    23  : AMetodoClass := TMetodomsgpoucopapel;
    24  : AMetodoClass := TMetodoexibemensagem;
    25  : AMetodoClass := TMetodobloqueiamouseteclado;
    26  : AMetodoClass := TMetodolinhasentrecupons;
    27  : AMetodoClass := TMetodopaginadecodigo;
    28  : AMetodoClass := TMetodomaxlinhasbuffer;
    29  : AMetodoClass := TMetodosetmaxlinhasbuffer;
    30  : AMetodoClass := TMetododatahora;
    31  : AMetodoClass := TMetodonumcupom;
    32  : AMetodoClass := TMetodonumcupom; //utiliza mesmo metodo
    33  : AMetodoClass := TMetodonumloja;
    34  : AMetodoClass := TMetodonumcro;
    35  : AMetodoClass := TMetodonumccf;
    36  : AMetodoClass := TMetodonumgrg;
    37  : AMetodoClass := TMetodonumgnf;
    38  : AMetodoClass := TMetodonumgnfc;
    39  : AMetodoClass := TMetodonumcdc;
    40  : AMetodoClass := TMetodonumcfc;
    41  : AMetodoClass := TMetodonumccdc;
    42  : AMetodoClass := TMetodonumcfd;
    43  : AMetodoClass := TMetodonumncn;
    44  : AMetodoClass := TMetodonumcrz;
    45  : AMetodoClass := TMetodonumecf;
    46  : AMetodoClass := TMetodonumserie;
    47  : AMetodoClass := TMetodonumseriemfd;
    48  : AMetodoClass := TMetodonumversao;
    49  : AMetodoClass := TMetodomfadicional;
    50  : AMetodoClass := TMetodorfdid;
    51  : AMetodoClass := TMetododatamovimento;
    52  : AMetodoClass := TMetodocnpj;
    53  : AMetodoClass := TMetodoie;
    54  : AMetodoClass := TMetodoim;
    55  : AMetodoClass := TMetodocliche;
    56  : AMetodoClass := TMetodousuarioatual;
    57  : AMetodoClass := TMetododatahorasb;
    58  : AMetodoClass := TMetododatahoraultimareducaoz;
    59  : AMetodoClass := TMetododecimaisqtd;
    60  : AMetodoClass := TMetododecimaispreco;
    61  : AMetodoClass := TMetodosubmodeloecf;
    62  : AMetodoClass := TMetodopaf;
    63  : AMetodoClass := TMetodonumcooinicial;
    64  : AMetodoClass := TMetodovendabruta;
    65  : AMetodoClass := TMetodograndetotal;
    66  : AMetodoClass := TMetodototaltroco;
    67  : AMetodoClass := TMetodototalcancelamentos;
    68  : AMetodoClass := TMetodototaldescontos;
    69  : AMetodoClass := TMetodototalacrescimos;
    70  : AMetodoClass := TMetodototalsubstituicaotributaria;
    71  : AMetodoClass := TMetodototalnaotributado;
    72  : AMetodoClass := TMetodototalisencao;
    73  : AMetodoClass := TMetodototalcancelamentosissqn;
    74  : AMetodoClass := TMetodototaldescontosissqn;
    75  : AMetodoClass := TMetodototalacrescimosissqn;
    76  : AMetodoClass := TMetodototalsubstituicaotributariaissqn;
    77  : AMetodoClass := TMetodototalnaotributadoissqn;
    78  : AMetodoClass := TMetodototalisencaoissqn;
    79  : AMetodoClass := TMetodototalnaofiscal;
    80  : AMetodoClass := TMetodototalcancelamentosopnf;
    81  : AMetodoClass := TMetodototaldescontosopnf;
    82  : AMetodoClass := TMetodototalacrescimosopnf;
    83  : AMetodoClass := TMetodonumultitem;
    84  : AMetodoClass := TMetododadosreducaoz;
    85  : AMetodoClass := TMetododadosultimareducaoz;
    86  : AMetodoClass := TMetodonumreducoeszrestantes;
    87  : AMetodoClass := TMetodoPegaaliquotas;
    88  : AMetodoClass := TMetodocarregaaliquotas;
    89  : AMetodoClass := TMetodolertotaisaliquota;
    90  : AMetodoClass := TMetodoprogramaaliquota;
    91  : AMetodoClass := TMetodoachaicmsaliquota;
    92  : AMetodoClass := TMetodoformaspagamento;
    93  : AMetodoClass := TMetodocarregaformaspagamento;
    94  : AMetodoClass := TMetodolertotaisformapagamento;
    95  : AMetodoClass := TMetodoprogramaformapagamento;
    96  : AMetodoClass := TMetodoachafpgdescricao;
    97  : AMetodoClass := TMetodocomprovantesnaofiscais;
    98  : AMetodoClass := TMetodocarregacomprovantesnaofiscais;
    99  : AMetodoClass := TMetodolertotaiscomprovantenaofiscal;
    100  : AMetodoClass := TMetodoprogramacomprovantenaofiscal;
    101  : AMetodoClass := TMetodoachacnfdescricao;
    102  : AMetodoClass := TMetodounidadesmedida;
    103  : AMetodoClass := TMetodocarregaunidadesmedida;
    104  : AMetodoClass := TMetodoprogramaunidademedida;
    105  : AMetodoClass := TMetodorelatoriosgerenciais;
    106  : AMetodoClass := TMetodocarregarelatoriosgerenciais;
    107  : AMetodoClass := TMetodolertotaisrelatoriosgerenciais;
    108  : AMetodoClass := TMetodoprogramarelatoriosgerenciais;
    109  : AMetodoClass := TMetodoachargdescricao;
    110  : AMetodoClass := TMetodotestapodeabrircupom;
    111  : AMetodoClass := TMetodoidentificaoperador;
    112  : AMetodoClass := TMetodoidentificaconsumidor;
    113  : AMetodoClass := TMetodoidentificapaf;
    114  : AMetodoClass := TMetodoabrecupom;
    115  : AMetodoClass := TMetodolegendainmetroproximoitem;
    116  : AMetodoClass := TMetodovendeitem;
    117  : AMetodoClass := TMetododescontoacrescimoitemanterior;
    118  : AMetodoClass := TMetodosubtotalizacupom;
    119  : AMetodoClass := TMetodoefetuapagamento;
    120  : AMetodoClass := TMetodoestornapagamento;
    121  : AMetodoClass := TMetodofechacupom;
    122  : AMetodoClass := TMetodocancelacupom;
    123  : AMetodoClass := TMetodocancelaitemvendido;
    124  : AMetodoClass := TMetodocancelaitemvendidoparcial;
    125  : AMetodoClass := TMetodocanceladescontoacrescimoitem;
    126  : AMetodoClass := TMetodocanceladescontoacrescimosubtotal;
    127  : AMetodoClass := TMetodosubtotal;
    128  : AMetodoClass := TMetodototalpago;
    129  : AMetodoClass := TMetodosangria;
    130  : AMetodoClass := TMetodosuprimento;
    131  : AMetodoClass := TMetodocortapapel;
    132  : AMetodoClass := TMetodonaofiscalcompleto;
    133  : AMetodoClass := TMetodoabrenaofiscal;
    134  : AMetodoClass := TMetodoregistraitemnaofiscal;
    135  : AMetodoClass := TMetodocancelaitemnaofiscal;
    136  : AMetodoClass := TMetodosubtotalizanaofiscal;
    137  : AMetodoClass := TMetodoefetuapagamentonaofiscal;
    138  : AMetodoClass := TMetodofechanaofiscal;
    139  : AMetodoClass := TMetodocancelanaofiscal;
    140  : AMetodoClass := TMetodoleiturax;
    141  : AMetodoClass := TMetodoleiturax; //Utiliza mesmo método
    142  : AMetodoClass := TMetodoleituraxserial;
    143  : AMetodoClass := TMetodoreducaoz;
    144  : AMetodoClass := TMetodotipoultimodocumento;
    145  : AMetodoClass := TMetodopoucopapel;
    146  : AMetodoClass := TMetodohorarioverao;
    147  : AMetodoClass := TMetodoarredonda;
    148  : AMetodoClass := TMetodoarredondaporqtd;
    149  : AMetodoClass := TMetodoarredondaitemmfd;
    150  : AMetodoClass := TMetodosetarredondaitemmfd;
    151  : AMetodoClass := TMetodomfd;
    152  : AMetodoClass := TMetodotermica;
    153  : AMetodoClass := TMetodoidentificaconsumidorrodape;
    154  : AMetodoClass := TMetodoestado;
    155  : AMetodoClass := TMetodoabregaveta;
    156  : AMetodoClass := TMetodogavetaaberta;
    157  : AMetodoClass := TMetodoimprimecheque;
    158  : AMetodoClass := TMetodocancelaimpressaocheque;
    159  : AMetodoClass := TMetodochequepronto;
    160  : AMetodoClass := TMetodoleituracmc7;
    161  : AMetodoClass := TMetodomudahorarioverao;
    162  : AMetodoClass := TMetodomudaarredondamento;
    163  : AMetodoClass := TMetodopreparatef;
    164  : AMetodoClass := TMetodocorrigeestadoerro;
    165  : AMetodoClass := TMetodoabrerelatoriogerencial;
    166  : AMetodoClass := TMetodorelatoriogerencial;
    167  : AMetodoClass := TMetodopulalinhas;
    168  : AMetodoClass := TMetodolinharelatoriogerencial;
    169  : AMetodoClass := TMetodoabrecupomvinculado;
    170  : AMetodoClass := TMetodolinhacupomvinculado;
    171  : AMetodoClass := TMetodocupomvinculado;
    172  : AMetodoClass := TMetodoestornaccd;
    173  : AMetodoClass := TMetodosegundaviavinculado;
    174  : AMetodoClass := TMetodoreimpressaovinculado;
    175  : AMetodoClass := TMetodofecharelatorio;
    176  : AMetodoClass := TMetodoleituramemoriafiscal;
    177  : AMetodoClass := TMetodoleituramemoriafiscalserial;
    178  : AMetodoClass := TMetodoleituramfdserial;
    179  : AMetodoClass := TMetodoarquivomfd_dll;
    180  : AMetodoClass := TMetodoespelhomfd_dll;
    181  : AMetodoClass := TMetodopafmf_lmfc_impressao;
    182  : AMetodoClass := TMetodopafmf_lmfc_espelho;
    183  : AMetodoClass := TMetodopafmf_lmfc_cotepe1704;
    184  : AMetodoClass := TMetodopafmf_lmfs_impressao;
    185  : AMetodoClass := TMetodopafmf_lmfs_espelho;
    186  : AMetodoClass := TMetodopafmf_mfd_espelho;
    187  : AMetodoClass := TMetodopafmf_mfd_cotepe1704;
    188  : AMetodoClass := TMetodopafmf_gerarcat52;
    189  : AMetodoClass := TMetodoassinarblocoxestoque;
    190  : AMetodoClass := TMetodoassinarblocoxreducaoz;
    191  : AMetodoClass := TMetodoassinarblocox;
    192  : AMetodoClass := TMetodovalidarblocoxestoque;
    193  : AMetodoClass := TMetodovalidarblocoxreducaoz;
    194  : AMetodoClass := TMetodovalidarblocox;
    195  : AMetodoClass := TMetodoenviarblocoxestoque;
    196  : AMetodoClass := TMetodoenviarblocoxreducaoz;
    197  : AMetodoClass := TMetodoenviarblocox;
    198  : AMetodoClass := TMetodoconsultarblocox;
    199  : AMetodoClass := TMetodoenviacomando;
    200  : AMetodoClass := TMetodoassinaarquivo;
    201  : AMetodoClass := TMetodoconfigbarras;
    202  : AMetodoClass := TMetodopafmf_arqmf_binario;
    203  : AMetodoClass := TMetodopafmf_arqmf_binario; //Utiliza mesmo método
    204  : AMetodoClass := TMetodopafmf_arqmf_binario; //Utiliza mesmo método
    205  : AMetodoClass := TMetodopafmf_arqmfd_binario;
    206  : AMetodoClass := TMetodopafmf_arqmfd_binario; //Utiliza mesmo método
    207  : AMetodoClass := TMetodopafmf_arqmfd_binario; //Utiliza mesmo método
    208  : AMetodoClass := TMetodoTransmitirArquivo;
    209  : AMetodoClass := TMetodoConsultarProcessamentoArquivo;
    210  : AMetodoClass := TMetodoCancelarArquivo;
    211  : AMetodoClass := TMetodoConsultarHistoricoArq;
    212  : AMetodoClass := TMetodoConsultarPendenciasContrib;
    213  : AMetodoClass := TMetodoConsultarPendenciasDevPafEcf;
    214  : AMetodoClass := TMetodoDownloadArquivo;
    215  : AMetodoClass := TMetodoListarArquivos;
    216  : AMetodoClass := TMetodoReprocessarArquivo;

    else
      DoACbr(ACmd);
  end;

  if Assigned(AMetodoClass) then
  begin
    Ametodo := AMetodoClass.Create(ACmd, Self);
    try
      Ametodo.Executar;
    finally
      Ametodo.Free;
    end;

  end;

end;

function TACBrObjetoECF.PegaAliquotas: String;
Var I : Integer ;
begin
  Result := '' ;
  with {$IFNDEF NOGUI}ACBrECF {$ELSE}dm.ACBrECF1 {$ENDIF} do
  begin
     if Aliquotas.Count < 1 then
        CarregaAliquotas ;

     for I := 0 to Aliquotas.Count -1 do
        Result := Result + PadLeft(Aliquotas[I].Indice,4) +
                           Aliquotas[I].Tipo +
                           FormatFloat('##0.00', Aliquotas[I].Aliquota ) + '|' ;

     if Result <> '' then
        Result := copy(Result,1,Length(Result)-1) ;
  end ;
end ;

function TACBrObjetoECF.PegaRelatoriosGerenciais : String ;
Var I : Integer ;
begin
  Result := '' ;
  with {$IFNDEF NOGUI}ACBrECF {$ELSE}dm.ACBrECF1 {$ENDIF} do
  begin
     if RelatoriosGerenciais.Count < 1 then
        CarregaRelatoriosGerenciais ;

     for I := 0 to RelatoriosGerenciais.Count -1 do
        Result := Result + PadLeft(RelatoriosGerenciais[I].Indice,4) +
                           PadLeft( RelatoriosGerenciais[I].Descricao, 30) +
                           IntToStrZero( RelatoriosGerenciais[I].Contador, 5 ) + '|' ;

     if Result <> '' then
        Result := copy(Result,1,Length(Result)-1) ;
  end ;
end;

function TACBrObjetoECF.PegaTotaisRelatoriosGerenciais : String ;
Var I : Integer ;
begin
  Result := '' ;
  with {$IFNDEF NOGUI}ACBrECF {$ELSE}dm.ACBrECF1 {$ENDIF} do
  begin
     LerTotaisRelatoriosGerenciais ;

     for I := 0 to RelatoriosGerenciais.Count -1 do
     begin
        Result := Result + PadLeft( RelatoriosGerenciais[I].Indice,4)  +
                           IntToStrZero( RelatoriosGerenciais[I].Contador, 5 ) + '|' ;
     end ;

     if Result <> '' then
        Result := copy(Result,1,Length(Result)-1) ;
  end ;
end;

{------------------------------------------------------------------------------}
function TACBrObjetoECF.PegaTotaisAliquotas: String;
Var I : Integer ;
begin
  Result := '' ;
  with {$IFNDEF NOGUI}ACBrECF {$ELSE}dm.ACBrECF1 {$ENDIF} do
  begin
     LerTotaisAliquota ;

     for I := 0 to Aliquotas.Count -1 do
        Result := Result + PadLeft(Aliquotas[I].Indice,4) +
                           FormatFloat('########0.00', Aliquotas[I].Total ) + '|' ;

     if Result <> '' then
        Result := copy(Result,1,Length(Result)-1) ;
  end ;
end ;

{------------------------------------------------------------------------------}
function TACBrObjetoECF.PegaFormasPagamento: String;
Var I : Integer ;
    Vinc : Char ;
begin
  Result := '' ;
  with {$IFNDEF NOGUI}ACBrECF {$ELSE}dm.ACBrECF1 {$ENDIF} do
  begin
     if FormasPagamento.Count < 1 then
        CarregaFormasPagamento ;

     for I := 0 to FormasPagamento.Count -1 do
     begin
        Vinc := ' ' ;
        if FormasPagamento[I].PermiteVinculado then
           Vinc := 'V' ;

        Result := Result + PadLeft( FormasPagamento[I].Indice,4) + Vinc +
                           PadLeft( FormasPagamento[I].Descricao, 30) + '|' ;
     end ;

     if Result <> '' then
        Result := copy(Result,1,Length(Result)-1) ;
  end ;
end ;

{------------------------------------------------------------------------------}
function TACBrObjetoECF.PegaTotaisFormasPagamento: String;
Var I : Integer ;
begin
  Result := '' ;
  with {$IFNDEF NOGUI}ACBrECF {$ELSE}dm.ACBrECF1 {$ENDIF} do
  begin
     LerTotaisFormaPagamento ;

     for I := 0 to FormasPagamento.Count -1 do
     begin
        Result := Result + PadLeft( FormasPagamento[I].Indice,4)  +
                           FormatFloat('########0.00', FormasPagamento[I].Total ) + '|' ;
     end ;

     if Result <> '' then
        Result := copy(Result,1,Length(Result)-1) ;
  end ;
end ;

{------------------------------------------------------------------------------}
function TACBrObjetoECF.PegaComprovantesNaoFiscais: String;
Var I : Integer ;
    Vinc : Char ;
begin
  Result := '' ;
  with {$IFNDEF NOGUI}ACBrECF {$ELSE}dm.ACBrECF1 {$ENDIF} do
  begin
     if ComprovantesNaoFiscais.Count < 1 then
        CarregaComprovantesNaoFiscais ;

     for I := 0 to ComprovantesNaoFiscais.Count -1 do
     begin
        Vinc := ' ' ;
        if ComprovantesNaoFiscais[I].PermiteVinculado then
           Vinc := 'V' ;

        Result := Result + PadLeft( ComprovantesNaoFiscais[I].Indice,4) + Vinc +
                           PadLeft( ComprovantesNaoFiscais[I].Descricao,30) +
                           PadLeft( ComprovantesNaoFiscais[I].FormaPagamento,4) + '|' ;
     end ;

     if Result <> '' then
        Result := copy(Result,1,Length(Result)-1) ;
  end ;
end ;

{------------------------------------------------------------------------------}
function TACBrObjetoECF.PegaTotaisComprovantesNaoFiscais: String;
Var I : Integer ;
begin
  Result := '' ;
  with {$IFNDEF NOGUI}ACBrECF {$ELSE}dm.ACBrECF1 {$ENDIF} do
  begin
     LerTotaisComprovanteNaoFiscal ;

     for I := 0 to ComprovantesNaoFiscais.Count -1 do
     begin
        Result := Result + PadLeft( ComprovantesNaoFiscais[I].Indice,4)  +
                           FormatFloat('########0.00', ComprovantesNaoFiscais[I].Total ) + '|' ;
     end ;

     if Result <> '' then
        Result := copy(Result,1,Length(Result)-1) ;
  end ;
end ;

{------------------------------------------------------------------------------}
function TACBrObjetoECF.PegaUnidadesMedida: String;
Var I : Integer ;
begin
  Result := '' ;
  with {$IFNDEF NOGUI}ACBrECF {$ELSE}dm.ACBrECF1 {$ENDIF} do
  begin
     if UnidadesMedida.Count < 1 then
        CarregaUnidadesMedida ;

     for I := 0 to UnidadesMedida.Count -1 do
        Result := Result + PadLeft( UnidadesMedida[I].Indice,4) +
                           PadLeft( UnidadesMedida[I].Descricao,4) + '|' ;

     if Result <> '' then
        Result := copy(Result,1,Length(Result)-1) ;
  end ;
end ;

procedure TACBrObjetoECF.StringToMemo(AString: AnsiString; Memo: TStringList);
begin
  AString   := StringReplace(AString,#13+#10,'|',[rfReplaceAll]) ;
  AString   := StringReplace(AString,#10,'|',[rfReplaceAll]) ;
  Memo.Text := StringReplace(AString,'|',sLineBreak,[rfReplaceAll]) ;
end;

function TACBrObjetoECF.AjustaNomeArquivoCmd(Params: Integer): String;
begin
  if (fpCmd.Params(Params) <> '') then
     Result := fpCmd.Params(Params)
  else
     Result := fpCmd.Metodo+'.txt' ;

  Result := AcertaPath( Result );
end;

end.

