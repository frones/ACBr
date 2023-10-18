{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Wemerson Souto                                  }
{                              Daniel Simoes de Almeida                        }
{                              André Ferreira de Moraes                        }
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

unit ACBrNFeDANFeRLRetrato;

interface

uses
  SysUtils, Classes,
  {$IFDEF CLX}
  QGraphics, QControls, QForms, Qt, QStdCtrls,
  {$ELSE}
  Graphics, Controls, Forms,
  {$ENDIF}
  RLReport,
  {$IFDEF BORLAND}
   {$IF CompilerVersion > 22}
    Vcl.Imaging.jpeg,
   {$ELSE}
    jpeg,
   {$IFEND}
  {$ENDIF}
  ACBrNFeDANFeRL, pcnConversao, RLBarcode, RLFilters, RLPDFFilter;

type

  { TfrlDANFeRLRetrato }

  TfrlDANFeRLRetrato = class(TfrlDANFeRL)
    rlbEmitente: TRLBand;
    rliEmitente: TRLDraw;
    RLDraw6: TRLDraw;
    rliChave: TRLDraw;
    RLDraw8: TRLDraw;
    RLDraw9: TRLDraw;
    RLDraw10: TRLDraw;
    RLDraw11: TRLDraw;
    rllDANFE: TRLLabel;
    rllDocumento1: TRLLabel;
    rllDocumento2: TRLLabel;
    rllTipoEntrada: TRLLabel;
    rllTipoSaida: TRLLabel;
    rliTipoEntrada: TRLDraw;
    rllEntradaSaida: TRLLabel;
    rllNumNF1: TRLLabel;
    rllSERIE1: TRLLabel;
    rliChave2: TRLDraw;
    rliChave3: TRLDraw;
    rlbCodigoBarras: TRLBarcode;
    rlbCabecalhoItens: TRLBand;
    rlbDadosAdicionais: TRLBand;
    rlsRectProdutos: TRLDraw;
    lblDadosDoProduto: TRLLabel;
    RLDraw50: TRLDraw;
    RLLabel23: TRLLabel;
    RLDraw51: TRLDraw;
    rlsDivProd: TRLDraw;
    rlsDivProd2: TRLDraw;
    rlsDivProd3: TRLDraw;
    rlsDivProd4: TRLDraw;
    rlsDivProd5: TRLDraw;
    rlsDivProd6: TRLDraw;
    rlsDivProd7: TRLDraw;
    rlsDivProd8: TRLDraw;
    rlsDivProd9: TRLDraw;
    rlsDivProd10: TRLDraw;
    rlsDivProd11: TRLDraw;
    rlsDivProd12: TRLDraw;
    rlsDivProd13: TRLDraw;
    RLDraw54: TRLDraw;
    rllChaveAcesso: TRLLabel;
    rllDadosVariaveis1a: TRLLabel;
    rllDadosVariaveis1b: TRLLabel;
    rllDadosVariaveis3_Descricao: TRLLabel;
    RLLabel28: TRLLabel;
    RLLabel29: TRLLabel;
    RLLabel30: TRLLabel;
    RLLabel31: TRLLabel;
    RLLabel77: TRLLabel;
    RLLabel78: TRLLabel;
    RLLabel82: TRLLabel;
    lblCST1: TRLLabel;
    lblCST2: TRLLabel;
    RLLabel84: TRLLabel;
    RLLabel85: TRLLabel;
    lblValorTotal: TRLLabel;
    lblValorUnitarioSup: TRLLabel;
    lblValorUnitarioInf: TRLLabel;
    RLLabel89: TRLLabel;
    RLLabel90: TRLLabel;
    lblQuantidade: TRLLabel;
    RLLabel92: TRLLabel;
    RLLabel93: TRLLabel;
    RLLabel94: TRLLabel;
    RLLabel95: TRLLabel;
    RLLabel96: TRLLabel;
    RLLabel97: TRLLabel;
    RLLabel98: TRLLabel;
    rlmEmitente: TRLMemo;
    rlmEndereco: TRLMemo;
    rliLogo: TRLImage;
    rllNatOperacao: TRLLabel;
    rllDadosVariaveis3: TRLLabel;
    rllInscricaoEstadual: TRLLabel;
    rllInscrEstSubst: TRLLabel;
    rllCNPJ: TRLLabel;
    rlmDadosAdicionais: TRLMemo;
    rllChave: TRLLabel;
    rllEmitente: TRLLabel;
    rlbCodigoBarrasFS: TRLBarcode;
    rllXmotivo: TRLLabel;
    rllDadosVariaveis1c: TRLLabel;
    rlbDestinatario: TRLBand;
    RLLabel18: TRLLabel;
    RLLabel32: TRLLabel;
    rllDestNome: TRLLabel;
    RLLabel35: TRLLabel;
    rllDestEndereco: TRLLabel;
    RLLabel39: TRLLabel;
    rllDestCidade: TRLLabel;
    RLLabel40: TRLLabel;
    rllDestFone: TRLLabel;
    RLLabel36: TRLLabel;
    rllDestBairro: TRLLabel;
    RLLabel41: TRLLabel;
    rllDestUF: TRLLabel;
    RLLabel33: TRLLabel;
    rllDestCNPJ: TRLLabel;
    RLLabel37: TRLLabel;
    rllDestCEP: TRLLabel;
    RLLabel42: TRLLabel;
    rllDestIE: TRLLabel;
    RLLabel34: TRLLabel;
    rllEmissao: TRLLabel;
    RLLabel38: TRLLabel;
    rllSaida: TRLLabel;
    RLLabel43: TRLLabel;
    rllHoraSaida: TRLLabel;
    RLDraw16: TRLDraw;
    RLDraw17: TRLDraw;
    RLDraw22: TRLDraw;
    RLDraw23: TRLDraw;
    RLDraw20: TRLDraw;
    RLDraw19: TRLDraw;
    RLDraw24: TRLDraw;
    RLDraw21: TRLDraw;
    RLDraw18: TRLDraw;
    RLDraw15: TRLDraw;
    rlbFatura: TRLBand;
    RLLabel19: TRLLabel;
    rllFatNum1: TRLLabel;
    rllFatNum5: TRLLabel;
    rllFatNum9: TRLLabel;
    rllFatData1: TRLLabel;
    rllFatData5: TRLLabel;
    rllFatData9: TRLLabel;
    rllFatValor1: TRLLabel;
    rllFatValor5: TRLLabel;
    rllFatValor9: TRLLabel;
    rllFatNum2: TRLLabel;
    rllFatNum6: TRLLabel;
    rllFatNum10: TRLLabel;
    rllFatData2: TRLLabel;
    rllFatData6: TRLLabel;
    rllFatData10: TRLLabel;
    rllFatValor2: TRLLabel;
    rllFatValor6: TRLLabel;
    rllFatValor10: TRLLabel;
    rllFatNum3: TRLLabel;
    rllFatNum7: TRLLabel;
    rllFatNum11: TRLLabel;
    rllFatData3: TRLLabel;
    rllFatData7: TRLLabel;
    rllFatData11: TRLLabel;
    rllFatValor3: TRLLabel;
    rllFatValor7: TRLLabel;
    rllFatValor11: TRLLabel;
    rllFatNum4: TRLLabel;
    rllFatNum8: TRLLabel;
    rllFatNum12: TRLLabel;
    rllFatData4: TRLLabel;
    rllFatData8: TRLLabel;
    rllFatData12: TRLLabel;
    rllFatValor4: TRLLabel;
    rllFatValor8: TRLLabel;
    rllFatValor12: TRLLabel;
    rliFatura1: TRLDraw;
    rliFatura2: TRLDraw;
    rliFatura3: TRLDraw;
    rliFatura: TRLDraw;
    rlbImposto: TRLBand;
    RLLabel20: TRLLabel;
    rllTituloBaseICMS: TRLLabel;
    rllBaseICMS: TRLLabel;
    RLLabel49: TRLLabel;
    rllValorFrete: TRLLabel;
    rllTituloValorICMS: TRLLabel;
    rllValorICMS: TRLLabel;
    RLLabel50: TRLLabel;
    rllValorSeguro: TRLLabel;
    RLLabel51: TRLLabel;
    rllDescontos: TRLLabel;
    rllTituloBaseICMSST: TRLLabel;
    rllBaseICMSST: TRLLabel;
    RLLabel52: TRLLabel;
    rllAcessorias: TRLLabel;
    rllTituloValorICMSST: TRLLabel;
    rllValorICMSST: TRLLabel;
    RLLabel53: TRLLabel;
    rllValorIPI: TRLLabel;
    RLLabel48: TRLLabel;
    rllTotalProdutos: TRLLabel;
    RLLabel54: TRLLabel;
    rllTotalNF: TRLLabel;
    RLDraw30: TRLDraw;
    rliDivImposto1: TRLDraw;
    RLDraw33: TRLDraw;
    RLDraw34: TRLDraw;
    RLDraw35: TRLDraw;
    rliDivImposto2: TRLDraw;
    RLDraw29: TRLDraw;
    rlbTransp: TRLBand;
    RLLabel21: TRLLabel;
    RLLabel55: TRLLabel;
    rllTransNome: TRLLabel;
    RLLabel63: TRLLabel;
    rllTransEndereco: TRLLabel;
    RLLabel67: TRLLabel;
    rllTransQTDE: TRLLabel;
    RLLabel68: TRLLabel;
    rllTransEspecie: TRLLabel;
    RLLabel69: TRLLabel;
    rllTransMarca: TRLLabel;
    RLLabel56: TRLLabel;
    RLLabel64: TRLLabel;
    rllTransCidade: TRLLabel;
    RLLabel70: TRLLabel;
    rllTransNumeracao: TRLLabel;
    rllTransModFrete: TRLLabel;
    RLLabel59: TRLLabel;
    rllTransCodigoANTT: TRLLabel;
    RLLabel60: TRLLabel;
    rllTransPlaca: TRLLabel;
    RLLabel71: TRLLabel;
    rllTransPesoBruto: TRLLabel;
    RLLabel61: TRLLabel;
    rllTransUFPlaca: TRLLabel;
    RLLabel65: TRLLabel;
    rllTransUF: TRLLabel;
    RLLabel62: TRLLabel;
    rllTransCNPJ: TRLLabel;
    RLLabel66: TRLLabel;
    rllTransIE: TRLLabel;
    RLLabel72: TRLLabel;
    rllTransPesoLiq: TRLLabel;
    RLDraw38: TRLDraw;
    RLDraw39: TRLDraw;
    rliTransp1: TRLDraw;
    rliTransp2: TRLDraw;
    RLDraw41: TRLDraw;
    rliTransp4: TRLDraw;
    RLDraw47: TRLDraw;
    RLDraw48: TRLDraw;
    RLDraw49: TRLDraw;
    rliTransp5: TRLDraw;
    rliTransp: TRLDraw;
    RLLabel25: TRLLabel;
    RLLabel26: TRLLabel;
    rlbISSQN: TRLBand;
    RLLabel24: TRLLabel;
    RLLabel73: TRLLabel;
    RLLabel74: TRLLabel;
    RLLabel75: TRLLabel;
    RLLabel76: TRLLabel;
    RLDraw56: TRLDraw;
    RLDraw57: TRLDraw;
    RLDraw58: TRLDraw;
    RLDraw52: TRLDraw;
    rliMarcaDagua1: TRLImage;
    rllPageNumber: TRLSystemInfo;
    rllLastPage: TRLSystemInfo;
    rlbAvisoContingencia: TRLBand;
    rllAvisoContingencia: TRLLabel;
    rlbContinuacaoInformacoesComplementares: TRLBand;
    RLLabel16: TRLLabel;
    rlmContinuacaoDadosAdicionais: TRLMemo;
    rllHomologacao: TRLLabel;
    LinhaDCSuperior: TRLDraw;
    LinhaDCInferior: TRLDraw;
    LinhaDCEsquerda: TRLDraw;
    LinhaDCDireita: TRLDraw;
    rllCabFatura1: TRLLabel;
    rllCabFatura2: TRLLabel;
    rllCabFatura3: TRLLabel;
    RLDraw69: TRLDraw;
    rllISSQNValorServicos: TRLLabel;
    rllISSQNBaseCalculo: TRLLabel;
    rllISSQNValorISSQN: TRLLabel;
    rllISSQNInscricao: TRLLabel;
    RLDraw70: TRLDraw;
    rliTransp3: TRLDraw;
    rlmDescricaoProduto: TRLMemo;
    rlmCodProd: TRLMemo;
    rllUsuario: TRLLabel;
    rllSistema: TRLLabel;
    rllCabFatura4: TRLLabel;
    rllCabFatura5: TRLLabel;
    rllCabFatura6: TRLLabel;
    rllCabFatura7: TRLLabel;
    rllCabFatura8: TRLLabel;
    rllCabFatura9: TRLLabel;
    rllCabFatura10: TRLLabel;
    rllCabFatura11: TRLLabel;
    rllCabFatura12: TRLLabel;
    lblPercValorDesc: TRLLabel;
    lblPercValorDesc1: TRLLabel;
    rlsDivProd14: TRLDraw;
    rllContingencia: TRLLabel;
    RLDraw4: TRLDraw;
    rllFatNum13: TRLLabel;
    rllFatData13: TRLLabel;
    rllFatValor13: TRLLabel;
    rllFatNum14: TRLLabel;
    rllFatData14: TRLLabel;
    rllFatValor14: TRLLabel;
    rllFatNum15: TRLLabel;
    rllFatData15: TRLLabel;
    rllFatValor15: TRLLabel;
    rllFatNum16: TRLLabel;
    rllFatData16: TRLLabel;
    rllFatValor16: TRLLabel;
    rllFatNum17: TRLLabel;
    rllFatData17: TRLLabel;
    rllFatValor17: TRLLabel;
    rllFatNum18: TRLLabel;
    rllFatData18: TRLLabel;
    rllFatValor18: TRLLabel;
    rllFatNum19: TRLLabel;
    rllFatData19: TRLLabel;
    rllFatValor19: TRLLabel;
    rllFatNum20: TRLLabel;
    rllFatData20: TRLLabel;
    rllFatValor20: TRLLabel;
    rllFatNum21: TRLLabel;
    rllFatData21: TRLLabel;
    rllFatValor21: TRLLabel;
    rllFatNum22: TRLLabel;
    rllFatData22: TRLLabel;
    rllFatValor22: TRLLabel;
    rllFatNum23: TRLLabel;
    rllFatData23: TRLLabel;
    rllFatValor23: TRLLabel;
    rllFatNum24: TRLLabel;
    rllFatData24: TRLLabel;
    rllFatValor24: TRLLabel;
    rllFatNum25: TRLLabel;
    rllFatData25: TRLLabel;
    rllFatValor25: TRLLabel;
    rllFatNum26: TRLLabel;
    rllFatData26: TRLLabel;
    rllFatValor26: TRLLabel;
    rllFatNum27: TRLLabel;
    rllFatData27: TRLLabel;
    rllFatValor27: TRLLabel;
    rllFatNum28: TRLLabel;
    rllFatData28: TRLLabel;
    rllFatValor28: TRLLabel;
    rllFatNum29: TRLLabel;
    rllFatData29: TRLLabel;
    rllFatValor29: TRLLabel;
    rllFatNum30: TRLLabel;
    rllFatData30: TRLLabel;
    rllFatValor30: TRLLabel;
    rllFatNum31: TRLLabel;
    rllFatData31: TRLLabel;
    rllFatValor31: TRLLabel;
    rllFatNum32: TRLLabel;
    rllFatData32: TRLLabel;
    rllFatValor32: TRLLabel;
    rllFatNum33: TRLLabel;
    rllFatData33: TRLLabel;
    rllFatValor33: TRLLabel;
    rllFatNum34: TRLLabel;
    rllFatData34: TRLLabel;
    rllFatValor34: TRLLabel;
    rllFatNum35: TRLLabel;
    rllFatData35: TRLLabel;
    rllFatValor35: TRLLabel;
    rllFatNum36: TRLLabel;
    rllFatData36: TRLLabel;
    rllFatValor36: TRLLabel;
    rllFatNum37: TRLLabel;
    rllFatData37: TRLLabel;
    rllFatValor37: TRLLabel;
    rllFatNum38: TRLLabel;
    rllFatData38: TRLLabel;
    rllFatValor38: TRLLabel;
    rllFatNum39: TRLLabel;
    rllFatData39: TRLLabel;
    rllFatValor39: TRLLabel;
    rllFatNum40: TRLLabel;
    rllFatData40: TRLLabel;
    rllFatValor40: TRLLabel;
    rllFatNum41: TRLLabel;
    rllFatData41: TRLLabel;
    rllFatValor41: TRLLabel;
    rllFatNum42: TRLLabel;
    rllFatData42: TRLLabel;
    rllFatValor42: TRLLabel;
    rllFatNum43: TRLLabel;
    rllFatData43: TRLLabel;
    rllFatValor43: TRLLabel;
    rllFatNum44: TRLLabel;
    rllFatData44: TRLLabel;
    rllFatValor44: TRLLabel;
    rllFatNum45: TRLLabel;
    rllFatData45: TRLLabel;
    rllFatValor45: TRLLabel;
    rllFatNum46: TRLLabel;
    rllFatData46: TRLLabel;
    rllFatValor46: TRLLabel;
    rllFatNum47: TRLLabel;
    rllFatData47: TRLLabel;
    rllFatValor47: TRLLabel;
    rllFatNum48: TRLLabel;
    rllFatData48: TRLLabel;
    rllFatValor48: TRLLabel;
    rllFatNum49: TRLLabel;
    rllFatData49: TRLLabel;
    rllFatValor49: TRLLabel;
    rllFatNum50: TRLLabel;
    rllFatData50: TRLLabel;
    rllFatValor50: TRLLabel;
    rllFatNum51: TRLLabel;
    rllFatData51: TRLLabel;
    rllFatValor51: TRLLabel;
    rllFatNum52: TRLLabel;
    rllFatData52: TRLLabel;
    rllFatValor52: TRLLabel;
    rllFatNum53: TRLLabel;
    rllFatData53: TRLLabel;
    rllFatValor53: TRLLabel;
    rllFatNum54: TRLLabel;
    rllFatData54: TRLLabel;
    rllFatValor54: TRLLabel;
    rllFatNum55: TRLLabel;
    rllFatData55: TRLLabel;
    rllFatValor55: TRLLabel;
    rllFatNum56: TRLLabel;
    rllFatData56: TRLLabel;
    rllFatValor56: TRLLabel;
    rllFatNum57: TRLLabel;
    rllFatData57: TRLLabel;
    rllFatValor57: TRLLabel;
    rllFatNum58: TRLLabel;
    rllFatData58: TRLLabel;
    rllFatValor58: TRLLabel;
    rllFatNum59: TRLLabel;
    rllFatData59: TRLLabel;
    rllFatValor59: TRLLabel;
    rllFatNum60: TRLLabel;
    rllFatData60: TRLLabel;
    rllFatValor60: TRLLabel;
    RLLabel12: TRLLabel;
    RLDraw3: TRLDraw;
    RLDraw5: TRLDraw;
    rliDivImposto3: TRLDraw;
    rliDivImposto4: TRLDraw;
    rliDivImposto5: TRLDraw;
    rllTituloTotalTributos: TRLLabel;
    rllTotalTributos: TRLLabel;
    rlbReciboHeaderBarra: TRLBand;
    rlbFaturaReal: TRLBand;
    RLLabel1: TRLLabel;
    RlbDadoPagamento: TRLLabel;
    RlbDadoNumero: TRLLabel;
    RlbDadoValorOriginal: TRLLabel;
    RLLabel254: TRLLabel;
    RLLabelPag: TRLLabel;
    RLLabelNUmero: TRLLabel;
    RLLabelValor: TRLLabel;
    RLLabelDupl: TRLLabel;
    RLDraw7: TRLDraw;
    RlbDadoValorDesconto: TRLLabel;
    RLLabelLIQ: TRLLabel;
    RlbDadoValorLiquido: TRLLabel;
    RLDraw12: TRLDraw;
    rlbCanceladaDenegada: TRLBand;
    RLLCanceladaDenegada: TRLLabel;
    lblValorTotalSup: TRLLabel;
    subItens: TRLSubDetail;
    rlbItens: TRLBand;
    LinhaCST: TRLDraw;
    LinhaCFOP: TRLDraw;
    LinhaUnidade: TRLDraw;
    LinhaQuantidade: TRLDraw;
    LinhaValorUnitario: TRLDraw;
    LinhaValorTotal: TRLDraw;
    LinhaBaseICMS: TRLDraw;
    LinhaValorICMS: TRLDraw;
    LinhaValorIPI: TRLDraw;
    LinhaAliqICMS: TRLDraw;
    LinhaAliqIPI: TRLDraw;
    LinhaNCM: TRLDraw;
    LinhaDescricao: TRLDraw;
    LinhaFimItens: TRLDraw;
    rlmDescricao: TRLMemo;
    txtCodigo: TRLMemo;
    txtCFOP: TRLLabel;
    txtUnidade: TRLMemo;
    txtQuantidade: TRLMemo;
    txtValorUnitario: TRLMemo;
    txtValorTotal: TRLLabel;
    txtValorDesconto: TRLLabel;
    txtBaseICMS: TRLLabel;
    txtValorICMS: TRLLabel;
    txtValorIPI: TRLLabel;
    txtAliqICMS: TRLLabel;
    txtAliqIPI: TRLLabel;
    FundoItem: TRLLabel;
    txtCST: TRLLabel;
    txtNCM: TRLLabel;
    LinhaDesconto: TRLDraw;
    LinhaCodigo: TRLDraw;
    LinhaFinal: TRLDraw;
    LinhaItem: TRLDraw;
    RLBandInfAd: TRLBand;
    RLDraw59: TRLDraw;
    RLDraw61: TRLDraw;
    RLDraw63: TRLDraw;
    RLMemoInfAd: TRLMemo;
    RLDraw2: TRLDraw;
    rlbEntrega: TRLBand;
    RLDraw37: TRLDraw;
    RLLabel9: TRLLabel;
    RLLabel11: TRLLabel;
    RLDraw27: TRLDraw;
    RLDraw31: TRLDraw;
    RLDraw25: TRLDraw;
    RLDraw32: TRLDraw;
    RLLabel4: TRLLabel;
    RLLabel13: TRLLabel;
    RLLabel14: TRLLabel;
    RLDraw26: TRLDraw;
    RLLabel22: TRLLabel;
    RLLabel27: TRLLabel;
    RLLabel44: TRLLabel;
    RLLabel45: TRLLabel;
    RLDraw40: TRLDraw;
    RLLquadroEntregaNome: TRLLabel;
    RLLquadroEntregaDocumento: TRLLabel;
    RLLquadroEntregaIE: TRLLabel;
    RLLquadroEntregaEndereco: TRLLabel;
    RLLquadroEntregaBairro: TRLLabel;
    RLLquadroEntregaCep: TRLLabel;
    RLLquadroEntregaMunicipio: TRLLabel;
    RLLquadroEntregaUF: TRLLabel;
    RLLquadroEntregaTelefone: TRLLabel;
    rlbRetirada: TRLBand;
    RLDraw13: TRLDraw;
    RLLabel3: TRLLabel;
    RLLabel5: TRLLabel;
    RLLabel6: TRLLabel;
    RLDraw14: TRLDraw;
    RLDraw28: TRLDraw;
    RLDraw36: TRLDraw;
    RLDraw42: TRLDraw;
    RLLabel7: TRLLabel;
    RLLabel8: TRLLabel;
    RLLabel15: TRLLabel;
    RLDraw43: TRLDraw;
    RLLabel17: TRLLabel;
    RLLabel46: TRLLabel;
    RLLabel47: TRLLabel;
    RLLabel57: TRLLabel;
    RLDraw44: TRLDraw;
    RLLquadroRetiradaNome: TRLLabel;
    RLLquadroRetiradaDocumento: TRLLabel;
    RLLquadroRetiradaIE: TRLLabel;
    RLLquadroRetiradaEndereco: TRLLabel;
    RLLquadroRetiradaBairro: TRLLabel;
    RLLquadroRetiradaCEP: TRLLabel;
    RLLquadroRetiradaMunicipio: TRLLabel;
    RLLquadroRetiradaUF: TRLLabel;
    RLLquadroRetiradaTelefone: TRLLabel;
    rlBarraiCanhoto: TRLDraw;
    RLBarraRecebemosDe: TRLMemo;
    RLBarraResumo: TRLMemo;
    rlBarraDataRecebimento: TRLLabel;
    rlBarraIdentificacao: TRLLabel;
    rlBarramDadosAdicionaisAuxiliar: TRLMemo;
    RLBarraBarcode: TRLBarcode;
    rllBarraNFe: TRLLabel;
    rlBarraNumNF0: TRLLabel;
    rlBarraSERIE0: TRLLabel;
    rlBarraiCanhoto1: TRLDraw;
    rlBarraiCanhoto2: TRLDraw;
    rlBarraiCanhoto3: TRLDraw;
    rlbReciboHeader: TRLBand;
    rliCanhoto: TRLDraw;
    rliCanhoto1: TRLDraw;
    rliCanhoto2: TRLDraw;
    rllRecebemosDe: TRLLabel;
    rllDataRecebimento: TRLLabel;
    rllIdentificacao: TRLLabel;
    rliCanhoto3: TRLDraw;
    rllNFe: TRLLabel;
    rllNumNF0: TRLLabel;
    rllSERIE0: TRLLabel;
    rllResumo: TRLLabel;
    rlmDadosAdicionaisAuxiliar: TRLMemo;
    rlbDivisaoRecibo: TRLBand;
    rliDivisao: TRLDraw;
    rlmDadosFisco: TRLMemo;
    rlbPagamentoReal: TRLBand;
    rliPagamentoReal: TRLDraw;
    RLLabel2: TRLLabel;
    RLLabel81: TRLLabel;
    RLDraw45: TRLDraw;
    RLLabelValor01: TRLLabel;
    RLLabelDescricao01: TRLLabel;
    RLLabelDescricao02: TRLLabel;
    RLLabelValor02: TRLLabel;
    RLLabelDescricao03: TRLLabel;
    RLLabelValor03: TRLLabel;
    RLLabelDescricao04: TRLLabel;
    RLLabelValor04: TRLLabel;
    RLPagDescricao0: TRLLabel;
    RLPagValor0: TRLLabel;
    RLPagDescricao1: TRLLabel;
    RLPagValor1: TRLLabel;
    RLPagDescricao2: TRLLabel;
    RLPagValor2: TRLLabel;
    RLPagDescricao3: TRLLabel;
    RLPagValor3: TRLLabel;
    RLPagDescricao4: TRLLabel;
    RLPagValor4: TRLLabel;
    RLPagDescricao5: TRLLabel;
    RLPagValor5: TRLLabel;
    RLPagDescricao6: TRLLabel;
    RLPagValor6: TRLLabel;
    RLPagDescricao7: TRLLabel;
    RLPagValor7: TRLLabel;
    RLPagDescricao8: TRLLabel;
    RLPagValor8: TRLLabel;
    RLPagDescricao9: TRLLabel;
    RLPagValor9: TRLLabel;
    RLPagDescricao10: TRLLabel;
    RLPagValor10: TRLLabel;
    RLPagDescricao11: TRLLabel;
    RLPagValor11: TRLLabel;
    RLPagDescricao12: TRLLabel;
    RLPagValor12: TRLLabel;
    RLPagDescricao13: TRLLabel;
    RLPagValor13: TRLLabel;
    RLPagDescricao14: TRLLabel;
    RLPagValor14: TRLLabel;
    RLPagDescricao15: TRLLabel;
    RLPagValor15: TRLLabel;
    rliPagamentoReal1: TRLDraw;
    rliPagamentoReal2: TRLDraw;
    rliPagamentoReal3: TRLDraw;

    procedure rlbContinuacaoInformacoesComplementaresBeforePrint(
      Sender: TObject; var PrintIt: Boolean);
    procedure rlbDivisaoReciboBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlbReciboHeaderBarraBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlbReciboHeaderBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure RLNFeBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlbEmitenteBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlbItensAfterPrint(Sender: TObject);
    procedure rlbDadosAdicionaisBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlbItensBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure subItensDataRecord(Sender: TObject; RecNo, CopyNo: Integer; var EOF: Boolean; var RecordAction: TRLRecordAction);
    procedure RLNFeDataRecord(Sender: TObject; RecNo, CopyNo: Integer; var EOF: Boolean; var RecordAction: TRLRecordAction);

  private
    FNumItem: Integer;
    FRecebemoDe: String;

    procedure InicializarDados;
    procedure DefinirCabecalho;
    procedure DefinirEmitente;
    procedure DefinirDestinatario;
    procedure DefinirImposto;
    procedure DefinirTransporte;
    procedure DefinirDadosAdicionais;
    procedure DefinirObservacoes;
    procedure DefinirISSQN;
    procedure AdicionarFatura;
    procedure AdicionarFaturaReal;
    procedure AplicarParametros;
    procedure DefinirEntrega;
    procedure DefinirRetirada;
    procedure DefinirCabecalhoItens;
    function ManterBandinfAdProd(const sInforAdicProduto: String): String;
    procedure posicionaCanhoto;
    function CanhotoDesabilita(Value: TRLBand): TfrlDANFeRLRetrato;
    Function ConfigurarRLBarcode:TfrlDANFeRLRetrato ;
    function ConfigurarCanhotoBarra: TfrlDANFeRLRetrato;
    function Canhoto(Value: TRLBand): TfrlDANFeRLRetrato;
    procedure ControlaExibicaoColunaDesconto(var vWidthAux: Integer; var vLeftAux: Integer);
    procedure AdicionarInformacoesPagamento;
    Procedure AtribuirDePara( ValueDe :  TRLDraw; ValuePara : array of TRLDraw  );
    procedure AjustaQuadro(iQuantlinhas: Integer; var iAltQuadro,
      iAltBand: Integer);
    procedure limparLabels(ValueInicio,ValueRepeticao: Integer;
      const ValueLbl1, ValueLbl2: String;
      const ValueLbl3: String = 'VAZIO');
    procedure AjustaFontesReport;
  end;

implementation

uses
  DateUtils, StrUtils, Math,
  ACBrNFeDANFeRLClass, ACBrDFeUtil, ACBrValidador,
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.DateTime,
  ACBrDFeDANFeReport, ACBrDFeReportFortes,
  pcnNFe, pcnConversaoNFe, ACBrNFe;

{$IfNDef FPC}
 {$R *.dfm}
{$Else}
 {$R *.lfm}
{$EndIf}

procedure TfrlDANFeRLRetrato.RLNFeBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  InicializarDados;

  RLNFe.Title := OnlyNumber(fpNFe.InfNFe.Id);

  if fpDANFe.LogoemCima then
  begin
    rliLogo.Top := 16;
    rliLogo.Left := 8;
    rliLogo.Height := 42;
    rliLogo.Width := 258;

    rlmEmitente.Top := 58;
    rlmEmitente.Left := 8;
    rlmEmitente.Height := 28;
    rlmEmitente.Width := 255;

    rlmEndereco.Top := 80;
    rlmEndereco.Left := 8;
    rlmEndereco.Height := 25;
    rlmEndereco.Width := 255;
  end;
end;

procedure TfrlDANFeRLRetrato.rlbReciboHeaderBarraBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  PrintIt := (RLNFe.PageNumber = 1);
end;

procedure TfrlDANFeRLRetrato.rlbReciboHeaderBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  PrintIt := (RLNFe.PageNumber = 1);
end;

procedure TfrlDANFeRLRetrato.rlbDivisaoReciboBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  PrintIt := (RLNFe.PageNumber = 1);
end;

procedure TfrlDANFeRLRetrato.rlbContinuacaoInformacoesComplementaresBeforePrint(
  Sender: TObject; var PrintIt: Boolean);
begin
  if (not fpDANFe.ImprimeContinuacaoDadosAdicionaisPrimeiraPagina) and (RLNFe.PageNumber = 1) then
  begin
    RLNFe.NewPageNeeded := True;
  end;
end;

procedure TfrlDANFeRLRetrato.rlbEmitenteBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  rlbCodigoBarras.BringToFront;
  if (RLNFe.PageNumber > 1) then
  begin
    rlbISSQN.Visible := False;
    rlbDadosAdicionais.Visible := False;
    if (fpQuantItens > 0) then
    begin
      rlbCabecalhoItens.Visible := True;
      lblDadosDoProduto.Caption := ACBrStr('CONTINUAÇÃO DOS DADOS DO PRODUTO / SERVIÇOS');
      rliMarcaDagua1.Top := 300;
    end
    else
      rlbCabecalhoItens.Visible := False;
  end;
end;

procedure TfrlDANFeRLRetrato.InicializarDados;
var
  h, iAlturaCanhoto, vWidthAux, vLeftAux, iAlturaMinLinha: Integer;
  CarregouLogo: Boolean;
begin
  TDFeReportFortes.AjustarMargem(RLNFe, fpDANFe);
  rlbCanceladaDenegada.Visible := False;

  iAlturaMinLinha := 12;
  if fpDANFe.EspacoEntreProdutos > iAlturaMinLinha then
    rlbItens.Height := fpDANFe.EspacoEntreProdutos
  else
    rlbItens.Height := iAlturaMinLinha;

  CarregouLogo := TDFeReportFortes.CarregarLogo(rliLogo, fpDANFe.Logo);
  if not CarregouLogo then
  begin
    rlmEndereco.Left := rlmEmitente.Left;
    rlmEndereco.Width := rlmEmitente.Width;
  end;

  if NaoEstaVazio(fpDANFe.MarcaDagua) and FileExists(fpDANFe.MarcaDagua) then
    rliMarcaDagua1.Picture.LoadFromFile(fpDANFe.MarcaDagua);

  if fpDANFe.ExibeResumoCanhoto then
  begin
    if NaoEstaVazio(fpDANFe.TextoResumoCanhoto) then
    begin
      rllResumo.Caption := fpDANFe.TextoResumoCanhoto;
    end
    else
    begin
      rllResumo.Caption := ACBrStr('EMISSÃO: ') + FormatDateBr(fpNFe.Ide.dEmi) +
        '  -  ' + 'DEST. / REM.: ' + fpNFe.Dest.xNome +
        '  -  ' + 'VALOR TOTAL: R$ ' + FormatFloatBr(fpNFe.Total.ICMSTot.vNF);
    end;
    rllResumo.Visible := True;
    iAlturaCanhoto := 25;
  end
  else
  begin
    rllResumo.Visible := False;
    iAlturaCanhoto := 15;
  end;

  rlBarraiCanhoto1.Top := iAlturaCanhoto;
  rlBarraiCanhoto2.Top := rlBarraiCanhoto1.Top;
  rlBarraiCanhoto2.Height := (rlBarraiCanhoto.Top + rlBarraiCanhoto.Height) - rlBarraiCanhoto2.Top;
  rllDataRecebimento.Top := rlBarraiCanhoto1.Top + 3;
  rllIdentificacao.Top := rlBarraiCanhoto1.Top + 3;

  rllSistema.Visible := NaoEstaVazio(fpDANFe.Sistema);
  rllSistema.Caption := fpDANFe.Sistema;

  rllUsuario.Visible := NaoEstaVazio(fpDANFe.Usuario);
  rllUsuario.Caption := ACBrStr('DATA / HORA DA IMPRESSÃO: ') + FormatDateTimeBr(Now) +
    ' - ' + fpDANFe.Usuario;

  rllHomologacao.Visible := (fpNFe.Ide.tpAmb = taHomologacao);
  rllHomologacao.Caption := ACBrStr('AMBIENTE DE HOMOLOGAÇÃO - NF-E SEM VALOR FISCAL');

  rllDadosVariaveis3_Descricao.Visible := True;
  rlbCodigoBarras.Visible := False;

  rllXmotivo.Visible := True;
  rlbCanceladaDenegada.Visible := fpDANFe.Cancelada;
  if rlbCanceladaDenegada.Visible then
  begin
    rllDadosVariaveis3_Descricao.Caption := ACBrStr('PROTOCOLO DE HOMOLOGAÇÃO DE CANCELAMENTO');
    rllXmotivo.Caption := 'NF-e CANCELADA';
    RLLCanceladaDenegada.Caption := 'NF-e CANCELADA';
  end
  else
  begin
    if (fpNFe.procNFe.cStat > 0) then
    begin
      case fpNFe.procNFe.cStat of
        100:
        begin
          rlbCodigoBarras.Visible := True;
          rllXMotivo.Visible := False;
          rllDadosVariaveis3_Descricao.Caption := ACBrStr('PROTOCOLO DE AUTORIZAÇÃO DE USO');
        end;

        101, 135, 151, 155:
        begin
          rllXmotivo.Caption := 'NF-e CANCELADA';
          rllDadosVariaveis3_Descricao.Caption := ACBrStr('PROTOCOLO DE HOMOLOGAÇÃO DE CANCELAMENTO');
          rlbCanceladaDenegada.Visible := True;
          RLLCanceladaDenegada.Caption := 'NF-e CANCELADA';
        end;

        110, 205, 301, 302:
        begin
          rllXmotivo.Caption := 'NF-e DENEGADA';
          rllDadosVariaveis3_Descricao.Caption := ACBrStr('PROTOCOLO DE DENEGAÇÃO DE USO');
          rlbCanceladaDenegada.Visible := True;
          RLLCanceladaDenegada.Caption := 'NF-e DENEGADA';
        end;

        else
        begin
          rllXmotivo.Caption := fpNFe.procNFe.xMotivo;
          rllDadosVariaveis3_Descricao.Visible := False;
          rllDadosVariaveis3.Visible := False;
        end;
      end;
    end
    else
    begin
      if (fpNFe.Ide.tpEmis in [teNormal, teSCAN]) then
      begin
        rllXmotivo.Caption := ACBrStr('NF-E NÃO ENVIADA PARA SEFAZ');
        rllDadosVariaveis3_Descricao.Visible := False;
        rllDadosVariaveis3.Visible := False;
        rllHomologacao.Visible := true;
        rllHomologacao.Caption := ACBrStr('NF-e NÃO PROTOCOLADA NA SEFAZ - SEM VALOR FISCAL')
      end;
    end;
  end;

  // Ajusta a largura da coluna "Código do Produto"
  txtCodigo.Width := fpDANFe.LarguraCodProd;
  rlmCodProd.Width := fpDANFe.LarguraCodProd;
  rlsDivProd.Left := fpDANFe.LarguraCodProd + 2;
  rlmDescricaoProduto.Left := rlsDivProd.Left + 2;
  rlmDescricaoProduto.Width := (rlsDivProd2.Left - rlsDivProd.Left) - 3;
  rlmDescricao.Left := LinhaDescricao.Left + 2;
  rlmDescricao.Width := (LinhaNCM.Left - LinhaDescricao.Left) - 24;

  // ajusta a posição do 'código do produto'
  if (rlmCodProd.Width > 90) then
  begin
    rlmCodProd.Top := 18;
    rlmCodProd.Height := 7;
  end
  else
  begin
    rlmCodProd.Top := 14;
    rlmCodProd.Height := 14;
  end;

  // Se a largura da coluna 'Código do produto' for suficiente,
  // exibe o título da coluna sem abreviações
  if (rlmCodProd.Width > 113) then
    rlmCodProd.Lines.Text := ACBrStr('CÓDIGO DO PRODUTO / SERVIÇO')
  else
    rlmCodProd.Lines.Text := ACBrStr('CÓDIGO DO PROD. / SERV.');

  // Ajusta a posição da coluna 'Descrição do produto'
  if (rlmDescricaoProduto.Width > 128) then
  begin
    rlmDescricaoProduto.Top := 18;
    rlmDescricaoProduto.Height := 7;
  end
  else
  begin
    rlmDescricaoProduto.Top := 14;
    rlmDescricaoProduto.Height := 14;
  end;

  // Se a largura da coluna 'Descrição do produto' for suficiente,
  // exibe o título da coluna sem abreviações
  if (rlmDescricaoProduto.Width > 72) then
    rlmDescricaoProduto.Lines.Text := ACBrStr('DESCRIÇÃO DO PRODUTO / SERVIÇO')
  else
    rlmDescricaoProduto.Lines.Text := 'DESCR. PROD. / SERV.';

  ControlaExibicaoColunaDesconto(vWidthAux, vLeftAux);

  // Oculta alguns itens do fpDANFe
  if fpDANFe.FormularioContinuo then
  begin
    rllRecebemosDe.Visible     := False;
    rllResumo.Visible          := False;
    rllDataRecebimento.Visible := False;
    rllIdentificacao.Visible   := False;
    rllNFe.Visible             := False;
    rliCanhoto.Visible         := False;
    rliCanhoto1.Visible        := False;
    rliCanhoto2.Visible        := False;
    rliCanhoto3.Visible        := False;
    rliDivisao.Visible         := False;
    rliTipoEntrada.Visible     := False;
    rllDANFE.Visible           := False;
    rllDocumento1.Visible      := False;
    rllDocumento2.Visible      := False;
    rllTipoEntrada.Visible     := False;
    rllTipoSaida.Visible       := False;
    rllEmitente.Visible        := False;
    rliLogo.Visible            := False;
    rlmEmitente.Visible        := False;
    rlmEndereco.Visible        := False;
    rliEmitente.Visible        := False;
    rllChaveAcesso.Visible     := False;
    rliChave.Visible           := False;
    rliChave2.Visible          := False;
    rliChave3.Visible          := False;
  end;

  // Expande a logomarca
  if fpDANFe.ExpandeLogoMarca then
  begin
    rlmEmitente.Visible := False;
    rlmEndereco.Visible := False;

    with rliLogo do
    begin
      Height := 101;
      Width := 268;
      Top := 14;
      Left := 2;

      TDFeReportFortes.AjustarLogo(rliLogo, fpDANFe.ExpandeLogoMarcaConfig);
    end;
  end;

  AjustaFontesReport;

  AplicarParametros; // Aplica os parâmetros escolhidos, após alterar o tamanho das fontes.

  DefinirDadosAdicionais;
  DefinirCabecalho;
  DefinirEmitente;
  DefinirDestinatario;
  DefinirImposto;
  DefinirISSQN;
  DefinirTransporte;
  DefinirEntrega;
  DefinirRetirada;
  AdicionarFaturaReal;
  AdicionarFatura;
  AdicionarInformacoesPagamento;
  DefinirCabecalhoItens;
  DefinirObservacoes;
  posicionaCanhoto;

  // Verifica se será exibida a 'continuação das informações complementares'
  if rlbContinuacaoInformacoesComplementares.Visible then
  begin
    h := (rlmContinuacaoDadosAdicionais.Top + rlmContinuacaoDadosAdicionais.Height) + 2;
    LinhaDCInferior.Top := h;
    h := (h - LinhaDCSuperior.Top) + 1;
    LinhaDCEsquerda.Height := h;
    LinhaDCDireita.Height := h;
  end;

  fpQuantItens := fpNFe.Det.Count;

  //Ajustar tamanho quadro DadosAdicionais
  if fpDANFe.ExpandirDadosAdicionaisAuto then
  begin
    rlbDadosAdicionais.AutoExpand := True;
    rlmDadosAdicionais.AutoSize := True;
    RLDraw50.Height := (rlmDadosAdicionais.Top + rlmDadosAdicionais.Height) - RLLabel77.Top + 10;
    RLDraw51.Height := RLDraw50.Height;
    rllSistema.Top  := RLDraw50.Top + RLDraw50.Height;
    rllUsuario.Top  := rllSistema.Top;
  end;

end;

procedure TfrlDANFeRLRetrato.DefinirCabecalho;
begin
  with fpNFe.InfNFe, fpNFe.Ide do
  begin
    rllChave.Caption := FormatarChaveAcesso(fpNFe.InfNFe.Id);
    rllChave.AutoSize := True;

    while (rliChave.Width < (rllChave.Width + 10 + (rllChave.Left - rliChave.Left))) do
      rllChave.Font.Size := rllChave.Font.Size - 1; // para nao truncar a chave vai diminuir o fonte

    rlbCodigoBarras.Visible := True;
    rlbCodigoBarras.Caption := OnlyNumber(fpNFe.InfNFe.Id);

    rllNumNF0.Caption := ACBrStr('Nº ') + FormatarNumeroDocumentoFiscal(IntToStr(nNF));
    rllNumNF1.Caption := rllNumNF0.Caption;

    rllSERIE0.Caption := ACBrStr('SÉRIE ') + PadLeft(IntToStr(Serie), 3, '0');
    rllSERIE1.Caption := rllSERIE0.Caption;

    rllNatOperacao.Caption := NatOp;
    rllEntradaSaida.Caption := tpNFToStr(tpNF);
    rllEmissao.Caption := FormatDateBr(dEmi);
    rllSaida.Caption := IfThen(DSaiEnt <> 0, FormatDateBr(dSaiEnt));

    if (versao = 2.00) then
      rllHoraSaida.Caption := ifthen(hSaiEnt = 0, '', TimeToStr(hSaiEnt))
    else
      rllHoraSaida.Caption := ifthen(TimeOf(dSaiEnt) = 0, '', TimeToStr(dSaiEnt));

    // Configuração inicial
    rllDadosVariaveis3_Descricao.Caption := ACBrStr('PROTOCOLO DE AUTORIZAÇÃO DE USO');
    rlbCodigoBarrasFS.Visible := (tpEmis in [teContingencia, teFSDA]);
    rlbAvisoContingencia.Visible := (tpEmis <> teNormal);
    rllAvisoContingencia.Caption := ACBrStr('DANFE em Contingência - Impresso em decorrência de problemas técnicos');
    rllDadosVariaveis1a.Visible := False;
    rllDadosVariaveis1b.Visible := False;
    rllDadosVariaveis1c.Visible := False;

    case fpNFe.Ide.tpEmis of
      teNormal, teSCAN, teSVCAN, teSVCRS, teSVCSP:
      begin
        rllDadosVariaveis1a.Visible := (fpNFe.procNFe.cStat > 0);
        rllDadosVariaveis1b.Visible := rllDadosVariaveis1a.Visible;
        rllDadosVariaveis1c.Visible := rllDadosVariaveis1a.Visible;

        if NaoEstaVazio(fpDANFe.Protocolo) then
          rllDadosVariaveis3.Caption := fpDANFe.Protocolo
        else
          rllDadosVariaveis3.Caption := fpNFe.procNFe.nProt + ' ' + FormatDateTimeBr(fpNFe.procNFe.dhRecbto);
      end;

      teContingencia, teFSDA:
      begin
        rllDadosVariaveis3_Descricao.Caption := 'DADOS DA NF-E';
        rlbCodigoBarrasFS.Caption := TACBrNFe(fpDANFe.ACBrNFe).GerarChaveContingencia(fpNFe);
        rllDadosVariaveis3.Caption := FormatarChaveAcesso(rlbCodigoBarrasFS.Caption);
      end;

      teDPEC:
      begin
        rllDadosVariaveis1a.Visible := True;
        rllDadosVariaveis1b.Visible := True;
        rlbAvisoContingencia.Visible := not NaoEstaVazio(fpNFe.procNFe.nProt);
        rllAvisoContingencia.Caption := ACBrStr('DANFE impresso em contingência - DPEC regularmente recebida pela Receita Federal do Brasil');
        if (not rlbAvisoContingencia.Visible) then // DPEC TRANSMITIDO
          rllDadosVariaveis3.Caption := fpNFe.procNFe.nProt + ' ' +
            IfThen(fpNFe.procNFe.dhRecbto <> 0, FormatDateTimeBr(fpNFe.procNFe.dhRecbto), '')
        else
        begin
          rllDadosVariaveis3_Descricao.Caption := ACBrStr('NÚMERO DE REGISTRO DO EPEC');
          if NaoEstaVazio(fpDANFe.Protocolo) then
            rllDadosVariaveis3.Caption := fpDANFe.Protocolo;
        end;
      end;
    end;

    if (dhCont > 0) and (xJust > '') then
      rllContingencia.Caption :=
        ACBrStr('Data / Hora da entrada em contingência: ') + FormatDateTimeBr(dhCont) +
        ACBrStr(' Motivo contingência: ') + xJust;
  end;

  if rlbCodigoBarras.Visible then
    rllChave.Holder := rlbCodigoBarras;
end;

procedure TfrlDANFeRLRetrato.DefinirEmitente;
var
  sTemp: String;
begin
  rlmEmitente.AutoSize := False;
  rlmEndereco.AutoSize := False;
  with fpNFe.Emit do
  begin
    if EstaVazio(FRecebemoDe) then
      FRecebemoDe := rllRecebemosDe.Caption;

    rllRecebemosDe.Caption := Format(FRecebemoDe, [XNome]);

    rllInscricaoEstadual.Caption := IE;
    rllInscrEstSubst.Caption := IEST;
    rllCNPJ.Caption := FormatarCNPJouCPF(CNPJCPF);
    rlmEmitente.Lines.Text := fpDANFe.ManterNomeImpresso(XNome, XFant);
    rlmEndereco.Top := rlmEmitente.Top + rlmEmitente.Height;
    rlmEndereco.Lines.Clear;
    with EnderEmit do
    begin
      sTemp := Trim(XLgr) +
        IfThen(Nro = '0', '', ', ' + Nro) + ' ' +
        IfThen(NaoEstaVazio(xCpl), Trim(XCpl), '') + ' - ' +
        Trim(XBairro);
      sTemp := sTemp + ' - CEP:' + FormatarCEP(CEP) + ' - ' + XMun + ' - ' + UF;
      rlmEndereco.Lines.add(sTemp);

      sTemp := 'TEL: ' + FormatarFone(Fone);
      rlmEndereco.Lines.add(sTemp);
    end;
  end;

  if NaoEstaVazio(fpDANFe.Site) then
    rlmEndereco.Lines.add(fpDANFe.Site);

  if NaoEstaVazio(fpDANFe.Email) then
    rlmEndereco.Lines.add(fpDANFe.Email);

  rlmEndereco.Height := rliEmitente.Height - rlmEndereco.Top - 15;
end;

procedure TfrlDANFeRLRetrato.DefinirDestinatario;
begin
  with fpNFe.Dest do
  begin
    if NaoEstaVazio(idEstrangeiro) then
      rllDestCNPJ.Caption := idEstrangeiro
    else
      rllDestCNPJ.Caption := FormatarCNPJouCPF(CNPJCPF);

    rllDestIE.Caption := IE;
    rllDestNome.Caption := XNome;
    with EnderDest do
    begin
      rllDestEndereco.Caption := XLgr +
        IfThen(Nro = '0', '', ', ' + Nro) +
        IfThen(NaoEstaVazio(xCpl), ' ' + xCpl, '');

      rllDestBairro.Caption := XBairro;
      rllDestCidade.Caption := XMun;
      rllDestUF.Caption := UF;
      rllDestCEP.Caption := FormatarCEP(CEP);
      rllDestFONE.Caption := FormatarFone(Fone);
    end;
  end;
end;

procedure TfrlDANFeRLRetrato.DefinirImposto;
var
  LarguraCampo: Integer;
begin
  with fpNFe.Total.ICMSTot do
  begin
    rllBaseICMS.Caption := FormatFloatBr(VBC);
    rllValorICMS.Caption := FormatFloatBr(VICMS);
    rllBaseICMSST.Caption := FormatFloatBr(VBCST);
    rllValorICMSST.Caption := FormatFloatBr(VST);
    rllTotalProdutos.Caption := FormatFloatBr(VProd);
    rllValorFrete.Caption := FormatFloatBr(VFrete);
    rllValorSeguro.Caption := FormatFloatBr(VSeg);
    rllDescontos.Caption := FormatFloatBr(VDesc);
    rllAcessorias.Caption := FormatFloatBr(VOutro);
    rllValorIPI.Caption := FormatFloatBr(VIPI);
    rllTotalNF.Caption := FormatFloatBr(VNF);

    //115 460 143
    // Exibe o Valor total dos tributos se vTotTrib for informado
    // e ajusta a posição dos outros campos para "abrir espaço" para ele.
    if (vTotTrib > 0) and (fpDANFe.ImprimeTributos = trbNormal)  then
    begin
      rllTotalTributos.Caption := FormatFloatBr(vTotTrib);
      rliDivImposto4.Visible := True;
      rllTituloTotalTributos.Visible := True;
      rllTotalTributos.Visible := True;

      rliDivImposto4.Left := 460;
      rllTituloTotalTributos.Left := rliDivImposto4.Left + 3;
      rllTotalTributos.Left := rliDivImposto4.Left + 7;
      rllTotalTributos.Width := (rliDivImposto5.Left - 7) - (rliDivImposto4.Left + 3);

      LarguraCampo := 115;
      rliDivImposto3.Left := rliDivImposto4.Left - LarguraCampo - 1;
      rllTituloValorICMSST.Left := rliDivImposto3.Left + 3;
      rllValorICMSST.Left := rliDivImposto3.Left + 7;
      rllValorICMSST.Width := (rliDivImposto4.Left - 7) - (rliDivImposto3.Left + 3);
    end
    else
    begin
      rliDivImposto4.Visible := False;
      rllTituloTotalTributos.Visible := False;
      rllTotalTributos.Visible := False;

      LarguraCampo := 143;
      rliDivImposto3.Left := rliDivImposto5.Left - LarguraCampo - 1;
      rllTituloValorICMSST.Left := rliDivImposto3.Left + 3;
      rllValorICMSST.Left := rliDivImposto3.Left + 7;
      rllValorICMSST.Width := (rliDivImposto5.Left - 7) - (rliDivImposto3.Left + 3);
    end;

    rliDivImposto2.Left := rliDivImposto3.Left - LarguraCampo - 1;
    rllTituloBaseICMSST.Left := rliDivImposto2.Left + 3;
    rllBaseICMSST.Left := rliDivImposto2.Left + 7;
    rllBaseICMSST.Width := (rliDivImposto3.Left - 7) - (rliDivImposto2.Left + 3);

    rliDivImposto1.Left := rliDivImposto2.Left - LarguraCampo - 1;
    rllTituloValorICMS.Left := rliDivImposto1.Left + 3;
    rllValorICMS.Left := rliDivImposto1.Left + 7;
    rllValorICMS.Width := (rliDivImposto2.Left - 7) - (rliDivImposto1.Left + 3);

    rllBaseICMS.Width := (rliDivImposto1.Left - 10);
  end;
end;

procedure TfrlDANFeRLRetrato.DefinirTransporte;
var
  i, j: Integer;
  RLLabel, RLLabelModelo: TRLLabel;
  ok: Boolean;
begin
  with fpNFe.Transp do
  begin
    rllTransModFrete.Caption := modFreteToDesStr(modFrete, DblToVersaoDF(ok, fpNFe.infNFe.Versao));
    with Transporta do
    begin
      rllTransCNPJ.Caption := FormatarCNPJouCPF(CNPJCPF);
      rllTransNome.Caption := XNome;
      rllTransIE.Caption := IE;
      rllTransEndereco.Caption := XEnder;
      rllTransCidade.Caption := XMun;
      rllTransUF.Caption := UF;
    end;
  end;

  with fpNFe.Transp.VeicTransp do
  begin
    rllTransCodigoANTT.Caption := RNTC;
    rllTransPlaca.Caption := Placa;
    rllTransUFPlaca.Caption := UF;
  end;

  if (fpNFe.Transp.Vol.Count > 0) then
  begin
    // Aproveita os labels criados em tempo de projeto (1ª linha)
    with fpNFe.Transp.Vol[0] do
    begin
      if (qVol > 0) then
        rllTransQTDE.Caption := IntToStr(QVol);

      rllTransEspecie.Caption := Esp;
      rllTransMarca.Caption := Marca;
      rllTransNumeracao.Caption := NVol;

      if (pesoL > 0) then
        rllTransPesoLiq.Caption := FormatFloatBr(PesoL, FloatMask(3));

      if (pesoB > 0) then
        rllTransPesoBruto.Caption := FormatFloatBr(PesoB, FloatMask(3));
    end;

    // Preenche os dados
    for i := 1 to (fpNFe.Transp.Vol.Count - 1) do
    begin
      RLLabelModelo := nil;
      with fpNFe.Transp.Vol[i] do
      begin
        // Cria os demais labels dinamicamente
        for j := 1 to 6 do
        begin
          RLLabel := TRLLabel.Create(Self);
          case j of
            1:
            begin // Qtde
              RLLabelModelo := rllTransQTDE;
              if (qVol > 0) then
                RLLabel.Caption := IntToStr(QVol);
            end;

            2:
            begin // Especie
              RLLabelModelo := rllTransEspecie;
              RLLabel.Caption := Esp;
            end;

            3:
            begin // Marca
              RLLabelModelo := rllTransMarca;
              RLLabel.Caption := Marca;
            end;

            4:
            begin // Numeracao
              RLLabelModelo := rllTransNumeracao;
              RLLabel.Caption := NVol;
            end;

            5:
            begin // Peso liq
              RLLabelModelo := rllTransPesoLiq;
              if (pesoL > 0) then
                RLLabel.Caption := FormatFloatBr(PesoL, FloatMask(3));
            end;

            6:
            begin // Peso bruto
              RLLabelModelo := rllTransPesoBruto;
              if (pesoB > 0) then
                RLLabel.Caption := FormatFloatBr(PesoB, FloatMask(3));
            end;
          end;

          if Assigned(RLLabelModelo) then
          begin
            RLLabel.Alignment := RLLabelModelo.Alignment;
            RLLabel.AutoSize := RLLabelModelo.AutoSize;
            RLLabel.Font := RLLabelModelo.Font;
            RLLabel.Name := RLLabelModelo.Name + IntToStr(i);
            RLLabel.Parent := RLLabelModelo.Parent;
            RLLabel.ParentFont := RLLabelModelo.ParentFont;
            RLLabel.Tag := RLLabelModelo.Tag;
            RLLabel.Height := RLLabelModelo.Height;
            RLLabel.Width := RLLabelModelo.Width;
            RLLabel.Left := RLLabelModelo.Left;
            RLLabel.Top := RLLabelModelo.Top + (i * (RLLabelModelo.Height));//iAltLinha;
          end;
        end;
      end;
    end;
  end
  else
  begin
    rllTransQTDE.Caption := '';
    rllTransEspecie.Caption := '';
    rllTransMarca.Caption := '';
    rllTransNumeracao.Caption := '';
    rllTransPesoLiq.Caption := '';
    rllTransPesoBruto.Caption := '';
  end;
end;

procedure TfrlDANFeRLRetrato.DefinirDadosAdicionais;
begin
  rlmDadosAdicionaisAuxiliar.Lines.Text :=
    fpDANFe.ManterInformacoesDadosAdicionais( fpNFe  );
end;

procedure TfrlDANFeRLRetrato.DefinirObservacoes;
var
  PosUltimoEspaco: Integer;
  sTexto: String;
  TextoContinuacao, TextoOriginal: string;
begin
  if (not fpDANFe.ExpandirDadosAdicionaisAuto) and
     (rlmDadosAdicionaisAuxiliar.Height > rlmDadosAdicionais.Height) then
  begin
    rlbContinuacaoInformacoesComplementares.Visible := True;
    TextoOriginal := rlmDadosAdicionaisAuxiliar.Lines.Text;
    sTexto := TextoOriginal;
    repeat
      PosUltimoEspaco := LastDelimiter(' '+sLineBreak, sTexto);
      sTexto          := LeftStr(TextoOriginal, PosUltimoEspaco -1 );
      //Debug
      //TextoContinuacao  := RightStr(TextoOriginal, Length(TextoOriginal)- PosUltimoEspaco);
      rlmDadosAdicionaisAuxiliar.Lines.Text := sTexto;
    until not (rlmDadosAdicionaisAuxiliar.Height > rlmDadosAdicionais.Height);

    TextoContinuacao  := RightStr(TextoOriginal, Length(TextoOriginal)- PosUltimoEspaco);
    rlmContinuacaoDadosAdicionais.Lines.Text := TextoContinuacao;
  end
  else
  begin
    rlbContinuacaoInformacoesComplementares.Visible := False;
  end;

  rlmDadosAdicionais.Lines.Text := rlmDadosAdicionaisAuxiliar.Lines.Text;

  // Área reservada ao Fisco
  rlmDadosFisco.Lines.Clear;
  rlmDadosFisco.Lines.Text := fpNFe.procNFe.xMsg;
end;

procedure TfrlDANFeRLRetrato.DefinirISSQN;
begin
  with fpNFe.Total.ISSQNtot do
  begin
    rlbISSQN.Visible := fpDANFe.ExibeDadosISSQN or
      (((vServ + vBC + vISS) > 0) and NaoEstaVazio(fpNFe.Emit.IM));

    if rlbISSQN.Visible then
    begin
      rllISSQNInscricao.Caption := fpNFe.Emit.IM;
      rllISSQNValorServicos.Caption := FormatFloatBr(vServ);
      rllISSQNBaseCalculo.Caption := FormatFloatBr(vBC);
      rllISSQNValorISSQN.Caption := FormatFloatBr(vISS);
    end;
  end;
end;


procedure TfrlDANFeRLRetrato.rlbDadosAdicionaisBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  // Posiciona a Marca D'água
  rliMarcaDagua1.Top := rlbCabecalhoItens.Top +
    ((rlbDadosAdicionais.Top - rlbCabecalhoItens.Top) div 2) -
    (rliMarcaDagua1.Height div 2);
end;

procedure TfrlDANFeRLRetrato.AdicionarFaturaReal;
begin
  rlbFaturaReal.Visible := fpDANFe.ExibeCampoFatura;

  if (fpNFe.infNFe.Versao >= 4) then
  begin
    RLLabelPag.Caption := '';
    RlbDadoPagamento.Caption := ACBrStr('DADOS DA FATURA');
    rlbFaturaReal.Visible := NaoEstaVazio(fpNFe.Cobr.Fat.nFat) and fpDANFe.ExibeCampoFatura;
  end
  else
  begin
    case fpNFe.Ide.indPag of
      ipVista:
        RlbDadoPagamento.Caption := ACBrStr('PAGAMENTO A VISTA');
      ipPrazo:
        RlbDadoPagamento.Caption := ACBrStr('PAGAMENTO A PRAZO');
      ipOutras:
      begin
        RlbDadoPagamento.Caption := 'OUTROS';
        rlbFaturaReal.Visible := NaoEstaVazio(fpNFe.Cobr.Fat.nFat) and fpDANFe.ExibeCampoFatura;
      end;
    end;
  end;

  if NaoEstaVazio(fpNFe.Cobr.Fat.nFat) then
  begin
    RLLabelNUmero.Caption := ACBrStr('NÚMERO');
    RLLabelValor.Caption := ACBrStr('VALOR ORIGINAL');
    RLLabelDupl.Caption := ACBrStr('VALOR DESCONTO');
    RLLabelLIQ.Caption := ACBrStr('VALOR LÍQUIDO');

    // Define a Coluna dos label's
    RLLabelNUmero.Left := 264;
    RLLabelValor.Left := 439;
    RLLabelDupl.Left := 541;
    RLLabelLIQ.Left := 652;
    with fpNFe.Cobr.Fat do
    begin
      RlbDadoNumero.Caption := nFat;
      RlbDadoValorOriginal.Caption := FormatFloatBr(vOrig);
      RlbDadoValorDesconto.Caption := FormatFloatBr(vDesc);
      RlbDadoValorLiquido.Caption := FormatFloatBr(vLiq);
    end;
  end
  else
  begin
    RLLabelNUmero.Caption := '';
    RLLabelValor.Caption := '';
    RLLabelDupl.Caption := '';
    RLLabelLIQ.Caption := '';
    RlbDadoNumero.Caption := '';
    RlbDadoValorOriginal.Caption := '';
    RlbDadoValorDesconto.Caption := '';
    RlbDadoValorLiquido.Caption := '';
  end;
end;


// Aplica parametros para formatar o Danfe
procedure TfrlDANFeRLRetrato.AplicarParametros;
var
  base: Integer;
  AltLinhaComun: Integer;
begin
  AltLinhaComun := fpDANFe.AltLinhaComun;

  // ******** Cabeçalho ********
  base := RLDraw6.Top;
  RLDraw6.Height := 2 * AltLinhaComun + 1;
  RLDraw8.Top := base + AltLinhaComun;
  RLDraw9.Height := AltLinhaComun + 1;

  RLDraw10.Top := base + AltLinhaComun;
  RLDraw10.Height := AltLinhaComun + 1;
  RLDraw11.Top := base + AltLinhaComun;
  RLDraw11.Height := AltLinhaComun + 1;

  RLLabel28.Top := base + 1;
  rllDadosVariaveis3_Descricao.Top := base + 1;

  RLLabel29.Top := base + AltLinhaComun + 1;
  RLLabel30.Top := base + AltLinhaComun + 1;
  RLLabel31.Top := base + AltLinhaComun + 1;

  rllNatOperacao.Top := base + AltLinhaComun - rllNatOperacao.Height;
  rllDadosVariaveis3.Top := base + AltLinhaComun - rllDadosVariaveis3.Height;

  rllInscricaoEstadual.Top := base + 2 * AltLinhaComun - rllInscricaoEstadual.Height;
  rllInscrEstSubst.Top := base + 2 * AltLinhaComun - rllInscrEstSubst.Height;
  rllCNPJ.Top := base + 2 * AltLinhaComun - rllCNPJ.Height;

  // Bands remetente
  rlbEmitente.Height := RLDraw6.Height + rliEmitente.Height + 2;// 182 + (2*AltLinhaComun - 60);

  // ******** DefinirDestinatario ********
  RLLabel18.Holder := nil;
  RLLabel18.Top := 0;
  RLDraw15.Top := RLLabel18.Height + 1;
  base := RLDraw15.Top + 1;
  RLDraw15.Height := 3 * AltLinhaComun + 2;

  RLDraw16.Top := base + AltLinhaComun;
  RLDraw17.Top := base + 2 * AltLinhaComun;
  RLDraw16.Left := RLDraw15.Left;
  RLDraw17.Left := RLDraw15.Left;

  RLDraw18.Height := 3 * AltLinhaComun + 1;
  RLDraw19.Height := AltLinhaComun + 1;

  RLDraw20.Top := base + AltLinhaComun;
  RLDraw20.Height := AltLinhaComun + 1;
  RLDraw21.Top := base + AltLinhaComun;
  RLDraw21.Height := AltLinhaComun + 1;

  RLDraw22.Top := base + 2 * AltLinhaComun;
  RLDraw22.Height := AltLinhaComun + 1;
  RLDraw23.Top := base + 2 * AltLinhaComun;
  RLDraw23.Height := AltLinhaComun + 1;
  RLDraw24.Top := base + 2 * AltLinhaComun;
  RLDraw24.Height := AltLinhaComun + 1;

  // Linha 1
  RLLabel32.Top := base + 1;
  RLLabel33.Top := base + 1;
  RLLabel34.Top := base + 1;

  rllDestNome.Top := base + AltLinhaComun - rllDestNome.Height;
  rllDestCNPJ.Top := base + AltLinhaComun - rllDestCNPJ.Height;
  rllEmissao.Top := base + AltLinhaComun - rllEmissao.Height;

  // Linha 2
  RLLabel35.Top := base + AltLinhaComun + 1;
  RLLabel36.Top := base + AltLinhaComun + 1;
  RLLabel37.Top := base + AltLinhaComun + 1;
  RLLabel38.Top := base + AltLinhaComun + 1;

  rllDestEndereco.Top := base + 2 * AltLinhaComun - rllDestEndereco.Height;
  rllDestBairro.Top := base + 2 * AltLinhaComun - rllDestBairro.Height;
  rllDestCEP.Top := base + 2 * AltLinhaComun - rllDestCEP.Height;
  rllSaida.Top := base + 2 * AltLinhaComun - rllSaida.Height;

  // Linha 3
  RLLabel39.Top := base + 2 * AltLinhaComun + 1;
  RLLabel40.Top := base + 2 * AltLinhaComun + 1;
  RLLabel41.Top := base + 2 * AltLinhaComun + 1;
  RLLabel42.Top := base + 2 * AltLinhaComun + 1;
  RLLabel43.Top := base + 2 * AltLinhaComun + 1;

  rllDestCidade.Top := base + 3 * AltLinhaComun - rllDestCidade.Height;
  rllDestFone.Top := base + 3 * AltLinhaComun - rllDestFone.Height;
  rllDestUF.Top := base + 3 * AltLinhaComun - rllDestUF.Height;
  rllDestIE.Top := base + 3 * AltLinhaComun - rllDestIE.Height;
  rllHoraSaida.Top := base + 3 * AltLinhaComun - rllHoraSaida.Height;

  // Band DefinirDestinatario
  rlbDestinatario.Height := RLDraw15.Height + RLDraw15.Top + 2; //108 + (3*AltLinhaComun - 90);

  // ******** Fatura ********
  base := RLDraw12.Top;
  RLDraw12.Height := AltLinhaComun + 1;

  RLLabelPag.Top := base + 1;
  RLLabelNUmero.Top := RLLabelPag.Top;
  RLLabelValor.Top := RLLabelPag.Top;
  RLLabelDupl.Top := RLLabelPag.Top;
  RLLabelLIQ.Top := RLLabelPag.Top;

  RLDraw7.Top := RLLabelPag.Top + RLLabelPag.Height;
  RLLabel254.Height := RLDraw7.Top - base - 2;

  RlbDadoPagamento.Top := base + AltLinhaComun - RlbDadoPagamento.Height;
  if (RlbDadoPagamento.Top < RLDraw7.Top) then
    RlbDadoPagamento.Top := RLDraw7.Top + 1;

  RlbDadoNumero.Top := RlbDadoPagamento.Top;
  RlbDadoValorOriginal.Top := RlbDadoPagamento.Top;
  RlbDadoValorDesconto.Top := RlbDadoPagamento.Top;
  RlbDadoValorLiquido.Top := RlbDadoPagamento.Top;
  // Band fatura
  rlbFaturaReal.Height := RLDraw12.Top + RLDraw12.Height + 2;

  // ******** Cálculo do DefinirImposto ********
  RLDraw29.Top := RLLabel20.Height + 1;
  base := RLDraw29.Top;
  RLDraw29.Height := 2 * AltLinhaComun + 1;

  RLDraw30.Top := base + AltLinhaComun;

  rliDivImposto1.Height := AltLinhaComun;
  rliDivImposto2.Height := AltLinhaComun;
  rliDivImposto3.Height := AltLinhaComun;
  rliDivImposto4.Height := AltLinhaComun;
  rliDivImposto5.Height := AltLinhaComun;
  rliDivImposto1.Top := base;
  rliDivImposto2.Top := base;
  rliDivImposto3.Top := base;
  rliDivImposto4.Top := base;
  rliDivImposto5.Top := base;

  RLDraw33.Height := AltLinhaComun;
  RLDraw34.Height := AltLinhaComun;
  RLDraw35.Height := AltLinhaComun;
  RLDraw3.Height := AltLinhaComun;
  RLDraw5.Height := AltLinhaComun;
  RLDraw33.Top := base + AltLinhaComun;
  RLDraw34.Top := base + AltLinhaComun;
  RLDraw35.Top := base + AltLinhaComun;
  RLDraw3.Top := base + AltLinhaComun;
  RLDraw5.Top := base + AltLinhaComun;

  // Linha 1
  rllTituloBaseICMS.Top := base + 1;
  rllTituloValorICMS.Top := base + 1;
  rllTituloBaseICMSST.Top := base + 1;
  rllTituloValorICMSST.Top := base + 1;
  rllTituloTotalTributos.Top := base + 1;
  rlLabel48.Top := base + 1;

  rllBaseICMS.Top := base + AltLinhaComun - rllBaseICMS.Height;
  rllValorICMS.Top := base + AltLinhaComun - rllValorICMS.Height;
  rllBaseICMSST.Top := base + AltLinhaComun - rllBaseICMSST.Height;
  rllValorICMSST.Top := base + AltLinhaComun - rllValorICMSST.Height;
  rllTotalTributos.Top := base + AltLinhaComun - rllTotalTributos.Height;
  rllTotalProdutos.Top := base + AltLinhaComun - rllTotalProdutos.Height;

  // Linha 2
  RLLabel49.Top := base + AltLinhaComun + 1;
  RLLabel50.Top := base + AltLinhaComun + 1;
  RLLabel51.Top := base + AltLinhaComun + 1;
  RLLabel52.Top := base + AltLinhaComun + 1;
  RLLabel53.Top := base + AltLinhaComun + 1;
  RLLabel54.Top := base + AltLinhaComun + 1;

  rllValorFrete.Top := base + 2 * AltLinhaComun - rllValorFrete.Height;
  rllValorSeguro.Top := base + 2 * AltLinhaComun - rllValorSeguro.Height;
  rllDescontos.Top := base + 2 * AltLinhaComun - rllDescontos.Height;
  rllAcessorias.Top := base + 2 * AltLinhaComun - rllAcessorias.Height;
  rllValorIPI.Top := base + 2 * AltLinhaComun - rllValorIPI.Height;
  rllTotalNF.Top := base + 2 * AltLinhaComun - rllTotalNF.Height;

  RLLabel25.Top := base + AltLinhaComun;
  RLLabel25.Height := AltLinhaComun - 1;

  // Band Calculo do DefinirImposto
  rlbImposto.Height := RLDraw29.Height + RLDraw29.Top + 2; // 79  + (2*AltLinhaComun - 60);

  // ******** Transportadora ********
  rliTransp.Top := RLLabel21.Height + 1;
  base := rliTransp.Top;
  rliTransp.Height := 3 * AltLinhaComun + 1;
  if (fpNFe.Transp.Vol.Count > 1) then//contem mais volumes na fpDANFe
    rliTransp.Height := rliTransp.Height + (rllTransQTDE.Height * (fpNFe.Transp.Vol.Count - 1));//a quantidade de volumes pode variar, entao é feito um recalculo

  RLDraw38.Top := base + AltLinhaComun;
  RLDraw39.Top := base + 2 * AltLinhaComun;
  RLDraw38.Left := rliTransp.LEFT;
  RLDraw39.Left := rliTransp.LEFT;

  RLDraw41.Top := base;
  RLDraw47.Top := base;
  RLDraw48.Top := base;
  RLDraw49.Top := base;
  rliTransp5.Top := base;

  RLDraw41.Height := AltLinhaComun;
  RLDraw47.Height := AltLinhaComun;
  RLDraw48.Height := AltLinhaComun;
  RLDraw49.Height := 2 * AltLinhaComun;
  rliTransp5.Height := rliTransp.Height;

  RLDraw70.Top := base + AltLinhaComun;
  RLDraw70.Height := AltLinhaComun;

  rliTransp1.Top := base + 2 * AltLinhaComun;
  rliTransp2.Top := base + 2 * AltLinhaComun;
  rliTransp3.Top := base + 2 * AltLinhaComun;
  rliTransp4.Top := base + 2 * AltLinhaComun;
  rliTransp1.Height := rliTransp.Height + rliTransp.top - rliTransp1.Top;//AltLinhaComun;
  rliTransp2.Height := rliTransp1.Height;
  rliTransp3.Height := rliTransp1.Height;
  rliTransp4.Height := rliTransp1.Height;

  //Linha 1
  RLLabel55.Top := base + 1;
  RLLabel56.Top := base + 1;
  RLLabel59.Top := base + 1;
  RLLabel60.Top := base + 1;
  RLLabel61.Top := base + 1;
  RLLabel62.Top := base + 1;

  rllTransNome.Top := base + AltLinhaComun - rllTransNome.Height;
  rllTransModFrete.Top := base + AltLinhaComun - rllTransModFrete.Height;
  rllTransCodigoANTT.Top := base + AltLinhaComun - rllTransCodigoANTT.Height;
  rllTransPlaca.Top := base + AltLinhaComun - rllTransPlaca.Height;
  rllTransUFPlaca.Top := base + AltLinhaComun - rllTransUFPlaca.Height;
  rllTransCNPJ.Top := base + AltLinhaComun - rllTransCNPJ.Height;

  //Linha 2
  RLLabel63.Top := base + AltLinhaComun + 1;
  RLLabel64.Top := base + AltLinhaComun + 1;
  RLLabel65.Top := base + AltLinhaComun + 1;
  RLLabel66.Top := base + AltLinhaComun + 1;

  rllTransEndereco.Top := base + 2 * AltLinhaComun - rllTransEndereco.Height;
  rllTransCidade.Top := base + 2 * AltLinhaComun - rllTransCidade.Height;
  rllTransUF.Top := base + 2 * AltLinhaComun - rllTransUF.Height;
  rllTransIE.Top := base + 2 * AltLinhaComun - rllTransIE.Height;

  //Linha 3
  RLLabel67.Top := base + 2 * AltLinhaComun + 1;
  RLLabel68.Top := base + 2 * AltLinhaComun + 1;
  RLLabel69.Top := base + 2 * AltLinhaComun + 1;
  RLLabel70.Top := base + 2 * AltLinhaComun + 1;
  RLLabel71.Top := base + 2 * AltLinhaComun + 1;
  RLLabel72.Top := base + 2 * AltLinhaComun + 1;

  rllTransQTDE.Top := base + 3 * AltLinhaComun - rllTransQTDE.Height;
  rllTransEspecie.Top := base + 3 * AltLinhaComun - rllTransEspecie.Height;
  rllTransMarca.Top := base + 3 * AltLinhaComun - rllTransMarca.Height;
  rllTransNumeracao.Top := base + 3 * AltLinhaComun - rllTransNumeracao.Height;
  rllTransPesoBruto.Top := base + 3 * AltLinhaComun - rllTransPesoBruto.Height;
  rllTransPesoLiq.Top := base + 3 * AltLinhaComun - rllTransPesoLiq.Height;

  // Band Transportadora
  rlbTransp.Height := rliTransp.Top + rliTransp.Height + 2;//110 + (3*AltLinhaComun - 90);
end;

procedure TfrlDANFeRLRetrato.DefinirEntrega;
begin
  with fpNFe.Entrega do
  begin
    rlbEntrega.Visible := NaoEstaVazio(xLgr);

    if rlbEntrega.Visible then
    begin
      RLLquadroEntregaNome.Caption := xNome;
      RLLquadroEntregaDocumento.Caption := FormatarCNPJouCPF(CNPJCPF);
      RLLquadroEntregaIE.Caption := IE;
      RLLquadroEntregaEndereco.Caption := XLgr +
                                          IfThen(Nro = '0', '', ', ' + Nro) +
                                          IfThen(EstaVazio(xCpl), '', ' - ' + xCpl);
      RLLquadroEntregaBairro.Caption := xBairro;
      RLLquadroEntregaCep.Caption := FormatarCEP(CEP);
      RLLquadroEntregaMunicipio.Caption := xMun;
      RLLquadroEntregaUF.Caption := UF;
      RLLquadroEntregaTelefone.Caption := FormatarFone(Fone);
    end;
  end;
end;

procedure TfrlDANFeRLRetrato.DefinirRetirada;
begin
  with fpNFe.Retirada do
  begin
    rlbRetirada.Visible := NaoEstaVazio(xLgr);

    if rlbRetirada.Visible then
    begin
      RLLquadroRetiradaNome.Caption := xNome;
      RLLquadroRetiradaDocumento.Caption := FormatarCNPJouCPF(CNPJCPF);
      RLLquadroRetiradaIE.Caption := IE;
      RLLquadroRetiradaEndereco.Caption := XLgr +
                                            IfThen(Nro = '0', '', ', ' + Nro) +
                                            IfThen(EstaVazio(xCpl), '', ' - ' + xCpl);
      RLLquadroRetiradaBairro.Caption := xBairro;
      RLLquadroRetiradaCEP.Caption := FormatarCEP(CEP);
      RLLquadroRetiradaMunicipio.Caption := xMun;
      RLLquadroRetiradaUF.Caption := UF;
      RLLquadroRetiradaTelefone.Caption := FormatarFone(Fone);
    end;
  end;
end;

procedure TfrlDANFeRLRetrato.subItensDataRecord(Sender: TObject; RecNo, CopyNo: Integer; var EOF: Boolean; var RecordAction: TRLRecordAction);
begin
  inherited;

  FNumItem := RecNo - 1;
  EOF := (RecNo > fpNFe.Det.Count);
  RecordAction := raUseIt;
end;

procedure TfrlDANFeRLRetrato.RLNFeDataRecord(Sender: TObject; RecNo, CopyNo: Integer; var EOF: Boolean; var RecordAction: TRLRecordAction);
begin
  inherited;

  EOF := (RecNo > 1);
  RecordAction := raUseIt;
end;

procedure TfrlDANFeRLRetrato.rlbItensBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  with fpNFe.Det.Items[FNumItem] do
  begin
    txtCodigo.Lines.Text := fpDANFe.ManterCodigo(Prod.cEAN, Prod.CProd);
    rlmDescricao.Lines.Text := fpDANFe.ManterXProd(fpNFe, FNumItem);
    RLMemoInfAd.Lines.Text := ManterBandinfAdProd(infAdProd);
    txtNCM.Caption := Prod.NCM;

    if Imposto.ISSQN.cListServ <> '' then
      txtCST.Caption := ''
    else
      txtCST.Caption := OrigToStr(Imposto.ICMS.orig) + fpDANFe.ManterCst(fpNFe.Emit.CRT, Imposto.ICMS.CSOSN, Imposto.ICMS.CST);

    txtCFOP.Caption := Prod.CFOP;

    case fpDANFe.ImprimeValor of
      iuComercial:
      begin
        txtUnidade.Lines.Text := Prod.uCom;
        txtQuantidade.Lines.Text := fpDANFe.FormatarQuantidade(Prod.qCom);
        txtValorUnitario.Lines.Text := fpDANFe.FormatarValorUnitario(Prod.vUnCom);
      end;

      iuTributavel:
      begin
        txtUnidade.Lines.Text := Prod.uTrib;
        txtQuantidade.Lines.Text := fpDANFe.FormatarQuantidade(Prod.qTrib);
        txtValorUnitario.Lines.Text := fpDANFe.FormatarValorUnitario(Prod.vUnTrib);
      end;

      iuComercialETributavel:
      begin
        if (Prod.UCom = Prod.UTrib) then
        begin
          txtUnidade.Lines.Text := Prod.uCom;
          txtQuantidade.Lines.Text := fpDANFe.FormatarQuantidade(Prod.qCom);
          txtValorUnitario.Lines.Text := fpDANFe.FormatarValorUnitario(Prod.vUnCom);
        end
        else
        begin
          txtUnidade.Lines.Text := fpDANFe.ManterUnidades(Prod.uCom, Prod.uTrib);
          txtQuantidade.Lines.Text := fpDANFe.ManterQuantidades(Prod.qCom, Prod.qTrib);
          txtValorUnitario.Lines.Text := fpDANFe.ManterValoresUnitarios(Prod.vUnCom, Prod.vUnTrib);
        end;
      end;
    end;
    {
    txtValorTotal.Caption := fpDANFe.ManterVprod(Prod.vProd, Prod.vDesc);
    txtValorDesconto.Caption := FormatFloatBr(fpDANFe.ManterVDesc(Prod.vDesc, Prod.vUnCom, Prod.qCom));
    }
    txtValorTotal.Caption := FormatFloatBr(Prod.vProd);
    txtValorDesconto.Caption := FormatFloatBr(Prod.vDesc);

    txtBaseICMS.Caption := FormatFloatBr(Imposto.ICMS.VBC);
    txtValorICMS.Caption := FormatFloatBr(Imposto.ICMS.VICMS);
    txtValorIPI.Caption := FormatFloatBr(Imposto.IPI.VIPI);
    txtAliqICMS.Caption := FormatFloatBr(Imposto.ICMS.PICMS);
    txtAliqIPI.Caption := FormatFloatBr(Imposto.IPI.PIPI);
  end;

  if fpDANFe.AlternaCoresProdutos then
  begin
    FundoItem.Height  := rlbItens.Height;
    FundoItem.Color   := fpCorDestaqueProdutos;
    FundoItem.Visible := odd(FNumItem);
  end;
end;

procedure TfrlDANFeRLRetrato.rlbItensAfterPrint(Sender: TObject);
begin
  //Código removido
end;

procedure TfrlDANFeRLRetrato.DefinirCabecalhoItens;
begin
  //   Configura Cabecalho dos Itens
  case fpNFe.Emit.CRT of
    crtRegimeNormal, crtSimplesExcessoReceita: begin
      lblCST1.Caption := 'CST';
      lblCST2.Caption := '';
    end;
    crtSimplesNacional: begin
      lblCST1.Caption := 'CSOSN';
      lblCST2.Caption := '/ CST';
    end;
  end;
  {
  if fpDANFe.ImprimeDescPorPercentual then
  begin
    lblPercValorDesc.Caption := 'PERC.(%)';
    fpDANFe.ImprimeTotalLiquido := False;
  end
  else
    lblPercValorDesc.Caption := 'VALOR';

  if fpDANFe.ImprimeTotalLiquido then
  begin
    lblValorTotal.Caption := 'DESCONTO';
    lblPercValorDesc1.Caption := ACBrStr('LÍQUIDO');
  end;
  }
  lblPercValorDesc.Caption := 'DESCONTO';
  lblPercValorDesc1.Caption := '';
//  lblValorTotal.Caption := 'VALOR';
end;

function TfrlDANFeRLRetrato.ManterBandinfAdProd(const sInforAdicProduto: String): String;
begin
  Result := Trim(sInforAdicProduto);
  Result := StringReplace(Result, ';', slineBreak, [rfReplaceAll]);

  RLBandInfAd.Visible := (Result <> '') and (fpDANFe.ExibeInforAdicProduto = infSeparadamente);
end;

procedure TfrlDANFeRLRetrato.posicionaCanhoto;
begin
  // Seleciona o Layout do Canhoto
  case fpDANFe.PosCanhotoLayout of
    prlBarra  : CanhotoDesabilita( rlbReciboHeader).
                ConfigurarCanhotoBarra.
                ConfigurarRLBarcode.
                Canhoto( rlbReciboHeaderBarra );

    prlPadrao : CanhotoDesabilita( rlbReciboHeaderBarra ).
                Canhoto( rlbReciboHeader );
  end;
end;


Function TfrlDANFeRLRetrato.CanhotoDesabilita(  Value : TRLBand ) :TfrlDANFeRLRetrato ;
begin
  result := Self;
  Value.Visible := false;
end;

Function TfrlDANFeRLRetrato.Canhoto( Value : TRLBand ) :TfrlDANFeRLRetrato ;
begin
  // Posiciona o canhoto do fpDANFe no cabeçalho ou rodapé
  result := self;
  case fpDANFe.PosCanhoto of
    prCabecalho:
      begin
        Value.BandType := btHeader;
        rlbDivisaoRecibo.BandType := btHeader;
        Value.Top := 0;
        rlbDivisaoRecibo.Top := Value.Top + Value.Height;
      end;
    prRodape:
      begin
        Value.BandType := btFooter;
        rlbDivisaoRecibo.BandType := btFooter;
        rlbDivisaoRecibo.Top := rlbDadosAdicionais.Top + rlbDadosAdicionais.Height;
        Value.Top := rlbDivisaoRecibo.Top + rlbDivisaoRecibo.Height;
      end;
  end;
end;

Function TfrlDANFeRLRetrato.ConfigurarCanhotoBarra :TfrlDANFeRLRetrato ;
begin
  result := self;
  // Define o Tamanho do Canhoto
  rlbReciboHeaderBarra.Height := 65;
  if fpDANFe.ExibeResumoCanhoto then
    rlbReciboHeaderBarra.Height := rlbReciboHeaderBarra.Height + 20;

end;

Function  TfrlDANFeRLRetrato.ConfigurarRLBarcode:TfrlDANFeRLRetrato ;
Var
  lIAlimento : Integer;
  lIAlibarra : Integer;
begin
  result := self;
  // Posiciona as informações do canhoto com chave de acesso

  rlBarraiCanhoto.Height        := rlbReciboHeaderBarra.Height;

  RLBarraRecebemosDe.Top        := rlBarraiCanhoto.Top +1;
  RLBarraRecebemosDe.Font       := rllRecebemosDe.Font;
  RLBarraRecebemosDe.Lines.Add( rllRecebemosDe.Caption );
  RLBarraRecebemosDe.Font.Size  := rllRecebemosDe.Font.Size;

  rlBarraiCanhoto1.Top          := RLBarraResumo.top + 10;
  RLBarraBarcode.Caption        := rlbCodigoBarras.Caption;
  RLBarraBarcode.Height         := rlbCodigoBarras.Height;

  if fpDANFe.ExibeResumoCanhoto then
  begin
    RLBarraResumo.Top           := RLBarraRecebemosDe.Top + 15;
    RLBarraResumo.Font          := rllResumo.Font;
    RLBarraResumo.Font.Size     := rllResumo.Font.Size;
    RLBarraResumo.Lines.Add( rllResumo.Caption );
    rlBarraiCanhoto1.Top        := RLBarraResumo.top + 20;
    lIAlimento                  := Trunc( rlBarraiCanhoto1.Top / 2 )-5;
  end
  else
    lIAlimento                  := rlBarraiCanhoto.Top +1;

  lIAlibarra                    := 530;

  rlBarraDataRecebimento.Top    := rlBarraiCanhoto1.Top + 1;
  rlBarraIdentificacao.Top      := rlBarraiCanhoto1.Top + 1;
  RLBarraBarcode.Top            := rlBarraiCanhoto1.Top + 3;

  rlBarraiCanhoto1.Width        := rlBarraiCanhoto.Width;
  rlBarraiCanhoto2.Top          := rlBarraiCanhoto1.Top;
  rlBarraiCanhoto2.Height       := (rlBarraiCanhoto.Top + rlBarraiCanhoto.Height) - rlBarraiCanhoto1.Top;
  rlBarraiCanhoto3.Height       := (rlBarraiCanhoto.Top + rlBarraiCanhoto.Height) - rlBarraiCanhoto3.Top;

  RLBarraBarcode.Left           := rlBarraiCanhoto3.Left + 3;

  rllBarraNFe.Top               := lIAlimento;
  rllBarraNFe.Left              := lIAlibarra - 80;

  rlBarraNumNF0.Top             := lIAlimento;
  rlBarraNumNF0.Left            := lIAlibarra - 40;
  rlBarraNumNF0.Alignment       := taLeftJustify;
  rlBarraNumNF0.Font            := rllBarraNFe.Font;
  rlBarraNumNF0.Font.Style      := [fsBold];
  rlBarraNumNF0.Caption         := rllNumNF0.Caption;

  rlBarraSERIE0.Top             := lIAlimento;
  rlBarraSERIE0.Left            := lIAlibarra + 70 ;
  rlBarraSERIE0.Alignment       := taLeftJustify;
  rlBarraSERIE0.Font            := rllBarraNFe.Font;
  rlBarraSERIE0.Caption         := rllSERIE0.Caption;

  rlBarraiCanhoto3.Left         := rllBarraNFe.Left - 3;

end;

procedure TfrlDANFeRLRetrato.ControlaExibicaoColunaDesconto(var vWidthAux: Integer; var vLeftAux: Integer);
begin
  // Controle para exibir coluna de desconto
  txtValorDesconto.Visible := fpDANFe.ManterColunaDesconto( fpNFe.Total.ICMSTot.vDesc );
  lblPercValorDesc.Visible := txtValorDesconto.Visible;
  lblPercValorDesc1.Visible := txtValorDesconto.Visible;
  LinhaDesconto.Visible := txtValorDesconto.Visible;
  rlsDivProd14.Visible := txtValorDesconto.Visible;
  // Ajusta a posição da coluna 'VALOR TOTAL'
  if (txtValorDesconto.Visible) then
  begin
    // Quantidade
    lblQuantidade.Width := 45;
    txtQuantidade.Width := 45;
    // Linha Valor Unitario
    rlsDivProd7.Left := 404;
    LinhaValorUnitario.Left := 404;
    // Valor Unitario
    lblValorUnitarioSup.Left := 405;
    lblValorUnitarioSup.Width := 50;
    lblValorUnitarioInf.Left := 405;
    lblValorUnitarioInf.Width := 50;
    txtValorUnitario.Left := 405;
    txtValorUnitario.Width := 50;
    // Linha Valor Total
    rlsDivProd8.Left := 455;
    LinhaValorTotal.Left := 455;
    // Valor Total
    lblValorTotalSup.Left := 455;
    lblValorTotalSup.Width := 52;
    txtValorTotal.Left := 455;
    txtValorTotal.Width := 52;
    lblValorTotal.Left := 455;
    lblValorTotal.Width := 52;
  end
  else
  begin
    // Calcula o espaço a distribuir entre os campos qtd, vlunit e vltot pela
    // diferença entre a posição inicial da qtd e a posição final do desconto, descontando 4 para separação de cada campó que restou
    vWidthAux := (txtValorDesconto.Left + txtValorDesconto.Width) - txtQuantidade.Left - 12;
    vWidthAux := Trunc(vWidthAux / 3);
    vLeftAux := txtQuantidade.Left;
    // Quantidade
    lblQuantidade.Width := vWidthAux;
    txtQuantidade.Width := vWidthAux;
    // Linha Valor Unitario
    rlsDivProd7.Left := vLeftAux + vWidthAux + 2;
    LinhaValorUnitario.Left := vLeftAux + vWidthAux + 2;
    vLeftAux := vLeftAux + vWidthAux + 4;
    // Valor Unitario
    lblValorUnitarioSup.Left := vLeftAux;
    lblValorUnitarioSup.Width := vWidthAux;
    lblValorUnitarioInf.Left := vLeftAux;
    lblValorUnitarioInf.Width := vWidthAux;
    txtValorUnitario.Left := vLeftAux;
    txtValorUnitario.Width := vWidthAux;
    // Linha Valor Total
    rlsDivProd8.Left := vLeftAux + vWidthAux + 2;
    LinhaValorTotal.Left := vLeftAux + vWidthAux + 2;
    vLeftAux := vLeftAux + vWidthAux + 4;
    // Ajuste se deu diferença na última posição quando o cálculo do vWidthAux não for divisão exata
    if ((vLeftAux + vWidthAux) <> (txtValorDesconto.Left + txtValorDesconto.Width)) then
      vWidthAux := vWidthAux + ((txtValorDesconto.Left + txtValorDesconto.Width) - (vLeftAux + vWidthAux));
    // Valor Total
    lblValorTotalSup.Left := vLeftAux;
    lblValorTotalSup.Width := vWidthAux;
    txtValorTotal.Left := vLeftAux;
    txtValorTotal.Width := vWidthAux;
    lblValorTotal.Left := vLeftAux;
    lblValorTotal.Width := vWidthAux;
  end;
  // Se a largura da coluna 'Quantidade' for suficiente,
  // exibe o título sem abreviações
  if (lblQuantidade.Width > 47) then
    lblQuantidade.Caption := ACBrStr('QUANTIDADE')
  else
    lblQuantidade.Caption := ACBrStr('QUANT.');
  // Se a largura da coluna 'Valor Unitário' for suficiente,
  // exibe o título em uma linha apenas

  if (lblValorUnitarioInf.Width > 61) then
  begin
    lblValorUnitarioSup.Visible := False;
    lblValorUnitarioInf.Caption := ACBrStr('VALOR UNITÁRIO');
    lblValorUnitarioInf.Top := 18;
  end
  else
  begin
    lblValorUnitarioSup.Visible := True;
    lblValorUnitarioInf.Caption := ACBrStr('UNITÁRIO');
    lblValorUnitarioInf.Top := 21;
  end;
  // Se a largura da coluna 'Valor Total' for suficiente,
  // exibe o título em uma linha apenas
  if (lblValorTotal.Width > 52) then
  begin
    lblValorTotalSup.Visible := False;
    lblValorTotal.Caption := ACBrStr('VALOR TOTAL');
    lblValorTotal.Top := 18;
  end
  else
  begin
    lblValorTotalSup.Visible := True;
    lblValorTotal.Caption := ACBrStr('TOTAL');
    lblValorTotal.Top := 21;
  end;
end;

procedure TfrlDANFeRLRetrato.AdicionarFatura;
  function ManterDuplicatas( ValueMaximo : Integer ): Integer;
  var
    x: Integer;
  begin
    // ValueMaximo - > Quantidade de Label's Disponivel

    Result := min(fpNFe.Cobr.Dup.Count, ValueMaximo);

    for x := 0 to (Result - 1) do
    begin
      TRLLabel(FindComponent('rllFatNum' + IntToStr(x + 1))).Caption :=
        fpNFe.Cobr.Dup[x].NDup;
      TRLLabel(FindComponent('rllFatData' + IntToStr(x + 1))).Caption :=
        FormatDateBr(fpNFe.Cobr.Dup[x].DVenc);
      TRLLabel(FindComponent('rllFatValor' + IntToStr(x + 1))).Caption :=
        FormatFloatBr(fpNFe.Cobr.Dup[x].VDup);
    end;

  end;
var
  iQuantidadedeLabel ,
  iAltQuadro,
  iAltBand: Integer;
begin

  rlbFatura.Visible := (fpNFe.Cobr.Dup.Count > 0);

  if ( rlbFatura.Visible ) then
  begin

    iQuantidadedeLabel := 60;

    limparLabels( 1,
                  iQuantidadedeLabel ,
                  'rllFatNum' ,
                  'rllFatData' ,
                  'rllFatValor');

    TRLLabel(FindComponent('rllFatNum1')).AutoSize := True;

    AjustaQuadro( ManterDuplicatas(iQuantidadedeLabel),
                  iAltQuadro,
                  iAltBand);

    rlbFatura.Height := iAltBand;
    rliFatura.Height := iAltQuadro;

    AtribuirDePara( rliFatura,
                    [ rliFatura1 ,
                      rliFatura2 ,
                      rliFatura3] );

  end;
end;

procedure TfrlDANFeRLRetrato.AdicionarInformacoesPagamento;
  function ManterInformacoesPagamento(ValueMaximo : Integer ): Integer;
  var
    x: Integer;
  begin
    // ValueMaximo - > Quantidade de Label's Disponivel

    Result := min(fpNFe.pag.Count, ValueMaximo);

    for x := 0 to (Result - 1) do
    begin
      TRLLabel(FindComponent('RLPagDescricao' + IntToStr(x))).Caption :=
        ACBrStr(
          Copy(FormaPagamentoToDescricao(fpNFe.pag.Items[x].tPag, fpNFe.pag.Items[x].xPag),1,22)
                  );
      TRLLabel(FindComponent('RLPagValor' + IntToStr(x))).Caption :=
        FormatFloatBr(fpNFe.pag.Items[x].vPag);
    end;

  end;
var
  iQuantidadedeLabel,
  iAltQuadro,
  iAltBand: Integer;
begin

  rlbPagamentoReal.Visible := ( fpDANFe.ExibeCampoDePagamento = eipQuadro )
                              and
                              ( fpNFe.pag.Count > 0);

  if ( rlbPagamentoReal.Visible ) then
  begin
    iQuantidadedeLabel := 16;

    limparLabels( 0 ,
                  ( iQuantidadedeLabel - 1 ),
                  'RLPagDescricao',
                  'RLPagValor');

    AjustaQuadro( ManterInformacoesPagamento(iQuantidadedeLabel),
                  iAltQuadro,
                  iAltBand);

    rlbPagamentoReal.Height := iAltBand;
    rliPagamentoReal.Height := iAltQuadro;

    AtribuirDePara( rliPagamentoReal,
                    [ rliPagamentoReal1 ,
                      rliPagamentoReal2 ,
                      rliPagamentoReal3 ]);
  end;
end;

procedure TfrlDANFeRLRetrato.AjustaQuadro(iQuantlinhas: Integer;
                                          var iAltQuadro: Integer;
                                          var iAltBand: Integer);
var
  iColunas: Integer;
  iAltLinha: Integer;
  iPosQuadro: Integer;
  iAltQuadro1Linha: Integer;
  iFolga: Integer;
  iLinhas: Integer;
begin
  {=============== Ajusta o tamanho do quadro ===============}
  iColunas := 4;          // Quantidade de colunas
  iAltLinha := 13;        // Altura de cada linha
  iPosQuadro := 12;       // Posição (Top) do Quadro
  iAltQuadro1Linha := 27; // Altura do quadro com 1 linha
  iFolga := 5;            // Distância entre o final da Band e o final do quadro

  // Quantidade de linhas
  if ((iQuantlinhas mod iColunas) = 0) then
    iLinhas := iQuantlinhas div iColunas
  else
    iLinhas := (iQuantlinhas div iColunas) + 1;
  if (iLinhas = 1) then
    iAltQuadro := iAltQuadro1Linha
  else
    iAltQuadro := iAltQuadro1Linha + ((iLinhas - 1) * iAltLinha);
  iAltBand := iPosQuadro + iAltQuadro + iFolga;
end;

procedure TfrlDANFeRLRetrato.limparLabels(ValueInicio,ValueRepeticao: Integer;
      const ValueLbl1, ValueLbl2: String;
      const ValueLbl3: String = 'VAZIO');
Var
  x : Integer;
Begin
  for x := ValueInicio to ValueRepeticao do
  begin
    TRLLabel(FindComponent(ValueLbl1 + IntToStr(x))).Caption := '';
    TRLLabel(FindComponent(ValueLbl2 + IntToStr(x))).Caption := '';
    if ValueLbl3 <> 'VAZIO' then
      TRLLabel(FindComponent(ValueLbl3 + IntToStr(x))).Caption := '';
  end;
End;

procedure TfrlDANFeRLRetrato.AjustaFontesReport;
var
  vWidthAux: Integer;
  vLeftAux: Integer;
  b: Integer;
  i: Integer;
  vAutoSizeAux: Boolean;
  UmaBand: TRLBand;
begin
  // Altera a fonte da Razão Social do DefinirEmitente
  rlmEmitente.Font.Size := fpDANFe.Fonte.TamanhoFonteRazaoSocial;
  //Percorre contrls do Report...
  for b := 0 to (RLNFe.ControlCount - 1) do
  begin
    if not (RLNFe.Controls[b] is TRLBand) then
    begin
      //não há configurações de componentes TRLImage por exemplo
      Continue;
    end;

    //Para cada band...
    UmaBand := TRLBand(RLNFe.Controls[b]);
    for i := 0 to (UmaBand.ControlCount - 1) do
    begin
      if (UmaBand.Controls[i].Tag = 703) then
      begin
        TRLLabel(UmaBand.Controls[i]).Font.Style := [];
        if fpDANFe.Fonte.Negrito then
          // Dados em negrito
          TRLLabel(UmaBand.Controls[i]).Font.Style := [fsBold];
        if (fpDANFe <> nil) then
          //altera o tamanho fonte demais campos
          TRLLabel(UmaBand.Controls[i]).Font.Size := fpDANFe.Fonte.TamanhoFonteDemaisCampos;
      end;

      if (UmaBand.Controls[i].Tag = 20) then
      begin
        TRLLabel(UmaBand.Controls[i]).Font.Size := fpDANFe.Fonte.TamanhoFonteInformacoesComplementares;
      end;

      // Altera a fonte dos demais campos
      case fpDANFe.Fonte.Nome of
        nfArial:
          begin
            if (UmaBand.Controls[i].Tag <> 20) then
              TRLLabel(UmaBand.Controls[i]).Font.Name := 'Arial';
            if (UmaBand.Controls[i].Tag = 3) then
              TRLLabel(UmaBand.Controls[i]).Font.Size := (TRLLabel(UmaBand.Controls[i]).Font.Size) - 1;
          end;
        nfCourierNew:
          begin
            TRLLabel(UmaBand.Controls[i]).Font.Name := 'Courier New';
            case UmaBand.Controls[i].Tag of
              0, 703, 704, 705:
                begin
                  TRLLabel(UmaBand.Controls[i]).Font.Size := (TRLLabel(UmaBand.Controls[i]).Font.Size) - 1;
                  if (TRLLabel(UmaBand.Controls[i]).Tag = 705) then
                    TRLLabel(UmaBand.Controls[i]).Top := (TRLLabel(UmaBand.Controls[i]).Top) - 1;
                end;
            end;
          end;
      else
        begin
          if (UmaBand.Controls[i].Tag <> 20) then
            TRLLabel(UmaBand.Controls[i]).Font.Name := 'Times New Roman';
          if (UmaBand.Controls[i].Tag = 3) then
            TRLLabel(UmaBand.Controls[i]).Font.Size := (TRLLabel(UmaBand.Controls[i]).Font.Size) - 1;
        end;
      end;

      //copia o left e width do componente, alterar o size do fonte, com o autosize ajusta o height, depois retorna como estava
      if (UmaBand.Controls[i].Tag = 703) then
      begin
        vWidthAux    := TRLLabel(UmaBand.Controls[i]).Width;
        vLeftAux     := TRLLabel(UmaBand.Controls[i]).Left;
        vAutoSizeAux := TRLLabel(UmaBand.Controls[i]).AutoSize;
        //        TRLLabel(UmaBand.Controls[i]).AutoSize := True;
        TRLLabel(UmaBand.Controls[i]).AutoSize := vAutoSizeAux;
        TRLLabel(UmaBand.Controls[i]).Left     := vLeftAux;
        if (TRLLabel(UmaBand.Controls[i]).Alignment = taLeftJustify) then
          vWidthAux := vWidthAux - fpAuxDiferencaPDF;
        //na hora de gerar o PDF vai ficar correto.
        TRLLabel(UmaBand.Controls[i]).Width := vWidthAux;
      end;
    end;
  end;
  if (fpDANFe.Fonte.Nome = nfCourierNew) then
  begin
    rllNumNF1.Font.Size := rllNumNF1.Font.Size - 3;
    rllNumNF1.Top := rllNumNF1.Top + 1;
    rllChave.Font.Size := rllChave.Font.Size - 1;
  end;
  if (fpDANFe.Fonte.TamanhoFonteEndereco > 0) then
    RLMEndereco.Font.Size := fpDANFe.Fonte.TamanhoFonteEndereco
  else
    RLMEndereco.Font.Size := 7;
end;

Procedure TfrlDANFeRLRetrato.AtribuirDePara( ValueDe :  TRLDraw;
                                             ValuePara : array of TRLDraw  );
var
  i : Integer;
begin
  for I := Low(ValuePara) to High(ValuePara) do
  begin
    ValuePara[i].Top := ValueDe.Top;
    ValuePara[i].Height := ValueDe.Height;
  end;
end;

end.
