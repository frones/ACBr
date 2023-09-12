{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Peterson de Cerqueira Matos                     }
{                              Wemerson Souto                                  }
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

unit ACBrNFeDANFeRLPaisagem;

interface

uses
  SysUtils, Classes,
  {$IFDEF CLX}
  QGraphics, QControls, QForms, Qt, QStdCtrls,
  {$ELSE}
  Graphics, Controls, Forms,
  {$ENDIF}
  RLReport, RLBarcode,
  {$IFDEF BORLAND}
   {$IF CompilerVersion > 22}
    Vcl.Imaging.jpeg,
   {$ELSE}
    jpeg,
   {$IFEND}
  {$ENDIF}
  ACBrNFeDANFeRL, RLFilters, RLPDFFilter;

type

  { TfrlDANFeRLPaisagem }

  TfrlDANFeRLPaisagem = class(TfrlDANFeRL)
    RLAngleLabel10: TRLAngleLabel;
    rlbCabecalhoItens: TRLBand;
    rlbDadosAdicionais: TRLBand;
    RLDraw50: TRLDraw;
    RLDraw51: TRLDraw;
    RLLabel77: TRLLabel;
    RLLabel78: TRLLabel;
    rlmDadosAdicionais: TRLMemo;
    rlbDestinatario: TRLBand;
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
    quadroDestinatarioExterno: TRLDraw;
    rlbFatura: TRLBand;
    rllFatNum1: TRLLabel;
    rllFatNum6: TRLLabel;
    rllFatNum11: TRLLabel;
    rllFatData1: TRLLabel;
    rllFatData6: TRLLabel;
    rllFatData11: TRLLabel;
    rllFatValor1: TRLLabel;
    rllFatValor6: TRLLabel;
    rllFatValor11: TRLLabel;
    rllFatNum2: TRLLabel;
    rllFatNum7: TRLLabel;
    rllFatNum12: TRLLabel;
    rllFatData2: TRLLabel;
    rllFatData7: TRLLabel;
    rllFatData12: TRLLabel;
    rllFatValor2: TRLLabel;
    rllFatValor7: TRLLabel;
    rllFatValor12: TRLLabel;
    rllFatNum3: TRLLabel;
    rllFatNum8: TRLLabel;
    rllFatNum13: TRLLabel;
    rllFatData3: TRLLabel;
    rllFatData8: TRLLabel;
    rllFatData13: TRLLabel;
    rllFatValor3: TRLLabel;
    rllFatValor8: TRLLabel;
    rllFatValor13: TRLLabel;
    rllFatNum4: TRLLabel;
    rllFatNum9: TRLLabel;
    rllFatNum14: TRLLabel;
    rllFatData4: TRLLabel;
    rllFatData9: TRLLabel;
    rllFatData14: TRLLabel;
    rllFatValor4: TRLLabel;
    rllFatValor9: TRLLabel;
    rllFatValor14: TRLLabel;
    rliFatura2: TRLDraw;
    rliFatura3: TRLDraw;
    rliFatura4: TRLDraw;
    rliFatura: TRLDraw;
    rlbImposto: TRLBand;
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
    rlbISSQN: TRLBand;
    RLLabel73: TRLLabel;
    RLLabel74: TRLLabel;
    RLLabel75: TRLLabel;
    RLLabel76: TRLLabel;
    RLDraw56: TRLDraw;
    RLDraw57: TRLDraw;
    RLDraw58: TRLDraw;
    RLDraw52: TRLDraw;
    rliMarcaDagua1: TRLImage;
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
    rllContingencia: TRLLabel;
    RLAngleLabel1: TRLAngleLabel;
    RLAngleLabel2: TRLAngleLabel;
    RLDraw13: TRLDraw;
    rliFatura1: TRLDraw;
    rllFatura: TRLAngleLabel;
    rliFatura5: TRLDraw;
    rllFatNum5: TRLLabel;
    rllFatNum10: TRLLabel;
    rllFatNum15: TRLLabel;
    rllFatData15: TRLLabel;
    rllFatData10: TRLLabel;
    rllFatData5: TRLLabel;
    rllFatValor5: TRLLabel;
    rllFatValor10: TRLLabel;
    rllFatValor15: TRLLabel;
    rllCabFatura13: TRLLabel;
    rllCabFatura14: TRLLabel;
    rllCabFatura15: TRLLabel;
    rliDivImposto0: TRLDraw;
    RLAngleLabel4: TRLAngleLabel;
    RLAngleLabel5: TRLAngleLabel;
    rliTransp6: TRLDraw;
    RLAngleLabel6: TRLAngleLabel;
    RLAngleLabel7: TRLAngleLabel;
    RLDraw3: TRLDraw;
    RLAngleLabel8: TRLAngleLabel;
    RLDraw5: TRLDraw;
    RLAngleLabel9: TRLAngleLabel;
    pnlCanhoto: TRLPanel;
    rliCanhoto: TRLDraw;
    rliCanhoto3: TRLDraw;
    rliCanhoto1: TRLDraw;
    rliCanhoto2: TRLDraw;
    rllNFe: TRLAngleLabel;
    rllNumNF0: TRLAngleLabel;
    rllSERIE0: TRLAngleLabel;
    rllIdentificacao: TRLAngleLabel;
    rllDataRecebimento: TRLAngleLabel;
    rllRecebemosDe: TRLAngleLabel;
    rllResumo: TRLAngleLabel;
    pnlDivisao: TRLPanel;
    rliDivisao: TRLDraw;
    RLLabel12: TRLLabel;
    RLDraw6: TRLDraw;
    rliDivImposto3: TRLDraw;
    rliDivImposto4: TRLDraw;
    rliDivImposto5: TRLDraw;
    RLDraw12: TRLDraw;
    rllTituloTotalTributos: TRLLabel;
    rllTotalTributos: TRLLabel;
    RLBFaturaReal: TRLBand;
    RLDraw8: TRLDraw;
    RLDraw27: TRLDraw;
    RLAngleLabel3: TRLAngleLabel;
    RLLabel5: TRLLabel;
    RLLabelLIQ: TRLLabel;
    RLLabelDupl: TRLLabel;
    RLLabelValor: TRLLabel;
    RLLabelNUmero: TRLLabel;
    RLLabelPag: TRLLabel;
    RlbDadoPagamento: TRLLabel;
    RlbDadoNumero: TRLLabel;
    RlbDadoValorOriginal: TRLLabel;
    RlbDadoValorDesconto: TRLLabel;
    RlbDadoValorLiquido: TRLLabel;
    RLDrawFaturareal: TRLDraw;
    rlbCanceladaDenegada: TRLBand;
    RLLCanceladaDenegada: TRLLabel;
    subItens: TRLSubDetail;
    rlbItens: TRLBand;
    lblDadosDoProduto: TRLLabel;
    rlmDadosAdicionaisAuxiliar: TRLMemo;
    pnlCabecalho1: TRLPanel;
    lblncm: TRLLabel;
    rlsCst: TRLDraw;
    lblCST1: TRLLabel;
    lblCST2: TRLLabel;
    rlsCfop: TRLDraw;
    lblcfop: TRLLabel;
    rlsUnd: TRLDraw;
    lblUnd: TRLLabel;
    rlsqtd: TRLDraw;
    lblqtd: TRLLabel;
    rlsVunitario: TRLDraw;
    lblValorUnit: TRLLabel;
    lblValorUnitarioInf: TRLLabel;
    rlsDivvalortotal: TRLDraw;
    lblValorTotalSup: TRLLabel;
    lblValorTotal: TRLLabel;
    rlsDivdesconto: TRLDraw;
    lblPercValorDesc: TRLLabel;
    lblPercValorDesc1: TRLLabel;
    rlsDivBaseicms: TRLDraw;
    RLLabel8: TRLLabel;
    RLLabel9: TRLLabel;
    rlsDivValorICMS: TRLDraw;
    RLLabel10: TRLLabel;
    RLLabel11: TRLLabel;
    rlsDivBaseICMSST: TRLDraw;
    RLLabel1: TRLLabel;
    RLLabel2: TRLLabel;
    rlsDivValorICMSST: TRLDraw;
    RLLabel3: TRLLabel;
    RLLabel4: TRLLabel;
    rlsAliqICMS: TRLDraw;
    rlsValorIPI: TRLDraw;
    RLLabel13: TRLLabel;
    RLLabel14: TRLLabel;
    RLLabel15: TRLLabel;
    RLLabel17: TRLLabel;
    RLLabel18: TRLLabel;
    RLDraw1: TRLDraw;
    pnlItens: TRLPanel;
    txtCodigo: TRLMemo;
    LinhaDescricao: TRLDraw;
    txtEAN: TRLLabel;
    LinhaProdEAN: TRLDraw;
    rlmDescricao: TRLMemo;
    LinhaCodigo: TRLDraw;
    RLDraw7: TRLDraw;
    pnlItens1: TRLPanel;
    RLDraw11: TRLDraw;
    RLDraw59: TRLDraw;
    RLDraw61: TRLDraw;
    txtNCM: TRLLabel;
    LinhaCST1: TRLDraw;
    txtCST: TRLLabel;
    linhaCFOP1: TRLDraw;
    txtCFOP: TRLLabel;
    LinhaUnidade: TRLDraw;
    LinhaQuantidade: TRLDraw;
    LinhaValorUnitario: TRLDraw;
    LinhaValorTotal1: TRLDraw;
    txtValorTotal: TRLLabel;
    LinhaDesconto: TRLDraw;
    txtValorDesconto: TRLLabel;
    LinhaBaseICMS1: TRLDraw;
    txtBaseICMS: TRLLabel;
    LinhaValorICMS: TRLDraw;
    txtValorICMS: TRLLabel;
    LinhaBaseICMSST: TRLDraw;
    txtBaseICMSST: TRLLabel;
    LInhaValorICMSST: TRLDraw;
    txtValorICMSST: TRLLabel;
    LinhaValorIPI: TRLDraw;
    txtValorIPI: TRLLabel;
    LinhaAliqICMS: TRLDraw;
    txtAliqICMS: TRLLabel;
    LinhaAliqIPI: TRLDraw;
    txtAliqIPI: TRLLabel;
    LinhaFinal: TRLDraw;
    rlsDivIcmsIPI: TRLDraw;
    pnlCabecalho: TRLPanel;
    rlmCodProd: TRLMemo;
    rlsProd: TRLDraw;
    rllEAN: TRLLabel;
    rlsProdEAN: TRLDraw;
    rlmDescricaoProduto: TRLMemo;
    rlbEmitente: TRLBand;
    rliEmitente: TRLDraw;
    rliNatOpe: TRLDraw;
    rliChave: TRLDraw;
    rliNatOpe1: TRLDraw;
    RLDraw9: TRLDraw;
    RLDraw10: TRLDraw;
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
    rllChaveAcesso: TRLLabel;
    rllDadosVariaveis1a: TRLLabel;
    rllDadosVariaveis1b: TRLLabel;
    rllDadosVariaveis3_Descricao: TRLLabel;
    RLLabel28: TRLLabel;
    RLLabel29: TRLLabel;
    RLLabel30: TRLLabel;
    RLLabel31: TRLLabel;
    rlmEmitente: TRLMemo;
    rlmEndereco: TRLMemo;
    rliLogo: TRLImage;
    rllNatOperacao: TRLLabel;
    rllDadosVariaveis3: TRLLabel;
    rllInscricaoEstadual: TRLLabel;
    rllInscrEstSubst: TRLLabel;
    rllCNPJ: TRLLabel;
    rllChave: TRLLabel;
    rllEmitente: TRLLabel;
    rllXmotivo: TRLLabel;
    rlbCodigoBarrasFS: TRLBarcode;
    rllPageNumber: TRLSystemInfo;
    rllLastPage: TRLSystemInfo;
    RLDraw4: TRLDraw;
    RLDraw2: TRLDraw;
    rllborda: TRLDraw;
    RlsLinhaCabecalho_Esquerda: TRLDraw;
    rllCinza: TRLLabel;
    RLDraw26: TRLDraw;
    FundoItem: TRLLabel;
    rlsMcm: TRLDraw;
    FundoItem1: TRLLabel;
    LinhaNCM: TRLDraw;
    LinhaProd15: TRLDraw;
    LinhaSepararprod: TRLDraw;
    txtUnidade: TRLMemo;
    txtQuantidade: TRLMemo;
    txtValorUnitario: TRLMemo;
    RLBandInfAd: TRLBand;
    RLDInfAdcBarra: TRLDraw;
    RLDraw25: TRLDraw;
    RLDraw31: TRLDraw;
    RLMemoInfAd: TRLMemo;
    RLDrawFinal: TRLDraw;
    rlbQuandoRetirada: TRLBand;
    QuandoRetiradaExterno: TRLDraw;
    rlbQuandoRetiradaLinha1Memo1: TRLLabel;
    QuandoRetiradaLinha: TRLDraw;
    rlbQuandoRetiradaLinha1Memo2: TRLLabel;
    rlbQuandoRetiradaLinha2Memo1: TRLLabel;
    rlbQuandoRetiradaLinha2Memo2: TRLLabel;
    rlbQuandoRetiradaLinha2Memo3: TRLLabel;
    rlbQuandoRetiradaLinha2Memo4: TRLLabel;
    RLLquadroRetiradaNome: TRLLabel;
    RLLquadroRetiradaDocumento: TRLLabel;
    RLLquadroRetiradaIE: TRLLabel;
    RLLquadroRetiradaEndereco: TRLLabel;
    RLLquadroRetiradaBairro: TRLLabel;
    RLLquadroRetiradaCEP: TRLLabel;
    RLLquadroRetiradaMunicipio: TRLLabel;
    RLLquadroRetiradaUF: TRLLabel;
    RLLquadroRetiradaTelefone: TRLLabel;
    rlbQuandoRetiradaAngleLabel1: TRLAngleLabel;
    rlbQuandoRetiradaAngleLabel2: TRLAngleLabel;
    QuandoRetiradaColuna1: TRLDraw;
    rlbQuandoRetiradaLinha2Coluna3: TRLDraw;
    rlbQuandoRetiradaLinha2Coluna2: TRLDraw;
    rlbQuandoRetiradaLinha2Coluna1: TRLDraw;
    rlbQuandoRetiradaLinha1Coluna1: TRLDraw;
    rlbQuandoEntrega: TRLBand;
    QuandoEntregaExterno: TRLDraw;
    rlbQuandoEntregaLinha1Coluna1: TRLDraw;
    rlbQuandoEntregaLinha2Coluna2: TRLDraw;
    QuandoEntregaLinha: TRLDraw;
    rlbQuandoEntregaLinha1Memo2: TRLLabel;
    rlbQuandoEntregaLinha2Memo1: TRLLabel;
    rlbQuandoEntregaLinha2Memo2: TRLLabel;
    rlbQuandoEntregaLinha2Coluna1: TRLDraw;
    rlbQuandoEntregaLinha2Memo3: TRLLabel;
    rlbQuandoEntregaLinha2Memo4: TRLLabel;
    RLLquadroEntregaNome: TRLLabel;
    RLLquadroEntregaDocumento: TRLLabel;
    RLLquadroEntregaIE: TRLLabel;
    RLLquadroEntregaEndereco: TRLLabel;
    RLLquadroEntregaBairro: TRLLabel;
    RLLquadroEntregaCep: TRLLabel;
    RLLquadroEntregaMunicipio: TRLLabel;
    RLLquadroEntregaUF: TRLLabel;
    RLLquadroEntregaTelefone: TRLLabel;
    rlbQuandoEntregaLinha1Memo1: TRLLabel;
    rlbQuandoEntregaLinha2Coluna3: TRLDraw;
    rlbQuandoEntregaAngleLabel1: TRLAngleLabel;
    rlbQuandoEntregaAngleLabel2: TRLAngleLabel;
    QuandoEntregaColuna1: TRLDraw;
    rlbQuandoRetiradaLinha1Coluna2: TRLDraw;
    rlbQuandoRetiradaLinha1Coluna3: TRLDraw;
    rlbQuandoEntregaLinha1Coluna2: TRLDraw;
    rlbQuandoEntregaLinha1Coluna3: TRLDraw;
    rlbQuandoEntregaLinha2Coluna4: TRLDraw;
    rlbQuandoRetiradaLinha2Coluna4: TRLDraw;
    rlbQuandoRetiradaLinha1Memo3: TRLLabel;
    rlbQuandoRetiradaLinha1Memo4: TRLLabel;
    rlbQuandoEntregaLinha1Memo3: TRLLabel;
    rlbQuandoEntregaLinha1Memo4: TRLLabel;
    rlbQuandoEntregaLinha2Memo5: TRLLabel;
    rlbQuandoRetiradaLinha2Memo5: TRLLabel;
    rlmDadosFisco: TRLMemo;
    rlbPagamentoReal: TRLBand;
    rliPagamentoReal: TRLDraw;
    RLLabel108: TRLLabel;
    rliPagamentoReal0: TRLDraw;
    rliPagamentoReal1: TRLDraw;
    rliPagamentoReal2: TRLDraw;
    rliPagamentoReal3: TRLDraw;
    rliPagamentoReal4: TRLDraw;
    rliPagamentoReal5: TRLDraw;
    rllPagamento: TRLAngleLabel;
    RLLabelDescricao01: TRLLabel;
    RLLabelValor01: TRLLabel;
    RLLabelDescricao02: TRLLabel;
    RLLabelValor02: TRLLabel;
    RLLabelDescricao03: TRLLabel;
    RLLabelValor03: TRLLabel;
    RLLabelDescricao04: TRLLabel;
    RLLabelValor04: TRLLabel;
    RLLabelDescricao05: TRLLabel;
    RLLabelValor05: TRLLabel;
    RLPagDescricao0: TRLLabel;
    RLPagValor0: TRLLabel;
    RLPagDescricao1: TRLLabel;
    RLPagValor1: TRLLabel;
    RLPagDescricao2: TRLLabel;
    RLPagValor2: TRLLabel;
    RLPagDescricao3: TRLLabel;
    RLPagValor3: TRLLabel;
    RLPagDescricao5: TRLLabel;
    RLPagValor5: TRLLabel;
    RLPagDescricao6: TRLLabel;
    RLPagValor6: TRLLabel;
    RLPagDescricao7: TRLLabel;
    RLPagValor7: TRLLabel;
    RLPagDescricao8: TRLLabel;
    RLPagValor8: TRLLabel;
    RLPagDescricao11: TRLLabel;
    RLPagValor13: TRLLabel;
    RLPagDescricao13: TRLLabel;
    RLPagValor12: TRLLabel;
    RLPagDescricao12: TRLLabel;
    RLPagValor11: TRLLabel;
    RLPagValor10: TRLLabel;
    RLPagDescricao10: TRLLabel;
    RLPagDescricao4: TRLLabel;
    RLPagValor4: TRLLabel;
    RLPagDescricao9: TRLLabel;
    RLPagValor9: TRLLabel;
    RLPagDescricao14: TRLLabel;
    RLPagValor14: TRLLabel;
    procedure RLNFeBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlbEmitenteBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlbItensAfterPrint(Sender: TObject);
    procedure rlbDadosAdicionaisBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlbItensBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlbCabecalhoItensBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure rlbContinuacaoInformacoesComplementaresBeforePrint(Sender: TObject;
        var PrintIt: Boolean);
    procedure subItensDataRecord(Sender: TObject; RecNo, CopyNo: Integer; var EOF: Boolean; var RecordAction: TRLRecordAction);
    procedure RLNFeDataRecord(Sender: TObject; RecNo, CopyNo: Integer; var EOF: Boolean; var RecordAction: TRLRecordAction);
    procedure RLMemoInfAdAfterPrint(Sender: TObject);
  private
    fNumItem: Integer;
    fRecebemoDe: String;
    fTamanhoBandInf: Integer;

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
    procedure DefinirCabecalhoItens;
    function ManterBandinfAdProd(const sInforAdicProduto: String): String;
    procedure AplicarParametrosEntrega;
    procedure AplicarParametrosRetirada;
    procedure DefinirEntrega;
    procedure DefinirRetirada;
    procedure ControlaExibicaoColunaDesconto(var vWidthAux: Integer; var vLeftAux: Integer);
    procedure AjustaQuadro(iQuantlinhas: Integer; var iAltQuadro,
      iAltBand: Integer);
    Procedure AtribuirDePara( ValueDe :  TRLDraw; ValuePara : array of TRLDraw  );
    procedure limparLabels(ValueInicio,ValueRepeticao: Integer;
      const ValueLbl1, ValueLbl2: String;
      const ValueLbl3: String = 'VAZIO');
    procedure AdicionarInformacoesPagamento;
    procedure AjustaFontesReport;
  end;

implementation

uses
  DateUtils, StrUtils, Math,
  ACBrNFe, pcnNFe, pcnConversao, pcnConversaoNFe,
  ACBrNFeDANFeRLClass, ACBrDFeUtil, ACBrValidador,
  ACBrDFeDANFeReport, ACBrDFeReportFortes,
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.DateTime;

{$IfNDef FPC}
  {$R *.dfm}
{$Else}
  {$R *.lfm}
{$EndIf}

procedure TfrlDANFeRLPaisagem.RLNFeBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  InicializarDados;

  RLNFe.Title := OnlyNumber(fpNFe.InfNFe.Id);
end;

procedure TfrlDANFeRLPaisagem.rlbEmitenteBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  rlbCodigoBarras.BringToFront;
  if (RLNFe.PageNumber > 1) then
  begin
    rlbISSQN.Visible := False;
    rlbDadosAdicionais.Visible := False;
    rlbCabecalhoItens.Height := 35;
    rlbCabecalhoItens.AutoExpand := False;

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

procedure TfrlDANFeRLPaisagem.InicializarDados;
var
  h, iAlturaCanhoto, vWidthAux, vLeftAux, iAlturaMinLinha: Integer;
  CarregouLogo: Boolean;
begin
  TDFeReportFortes.AjustarMargem(RLNFe, fpDANFe);
  rlbCanceladaDenegada.Visible:= False;

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

  rliCanhoto1.Left := iAlturaCanhoto;
  rliCanhoto2.Left := rliCanhoto1.Left;
  rliCanhoto2.Width := (rliCanhoto.Left + rliCanhoto.Width) - rliCanhoto2.Left;
  rllDataRecebimento.Left := rliCanhoto1.Left + 3;
  rllIdentificacao.Left := rliCanhoto1.Left + 3;

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
      end;
    end;
  end;

  // Ajusta a largura da coluna "Código do Produto"
  txtCodigo.Width := fpDANFe.LarguraCodProd;
  rlmCodProd.Width := txtCodigo.Width;
  rlsProd.Left := txtCodigo.Width + 2;
  LinhaDescricao.Left := rlsProd.Left;

  rllEAN.Visible := fpDANFe.ExibeEAN;
  txtEAN.Visible := fpDANFe.ExibeEAN;
  rlsProdEAN.Visible := fpDANFe.ExibeEAN;
  LinhaProdEAN.Visible := fpDANFe.ExibeEAN;

  if (not fpDANFe.ExibeEAN) then
  begin
    rlmDescricaoProduto.Left := rlsProd.Left + 2;
    rlmDescricaoProduto.Width := (rlsMcm.Left - rlsProd.Left) - 3;
    rlmDescricao.Left := LinhaDescricao.Left + 2;
  end
  else
  begin
    rllEAN.Left := rlsProd.Left + 2;
    txtEAN.Left := LinhaDescricao.Left + 2;
    rlsProdEAN.Left := (rllEAN.Left + rllEAN.Width) + 2;
    LinhaProdEAN.Left := (txtEAN.Left + txtEAN.Width) + 2;
    rlmDescricaoProduto.Left := (rlsProdEAN.Left) + 2;
    rlmDescricaoProduto.Width := (rlsMcm.Left - rlsProdEAN.Left) - 3;
    rlmDescricao.Left := LinhaProdEAN.Left + 2;
  end;
  rlmDescricao.SecondHolder := nil;
  rlmDescricao.Width := (LinhaNCM.Left - LinhaDescricao.Left) - 24;

  // ajusta a posição do 'código do produto'
  if (rlmCodProd.Width > 90) then
  begin
    rlmCodProd.Top := 3;
    rlmCodProd.Height := 7;
  end
  else
  begin
    rlmCodProd.Top := 3;
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
    rlmDescricaoProduto.Top := 8;
    rlmDescricaoProduto.Height := 7;
  end
  else
  begin
    rlmDescricaoProduto.Top := 3;
    rlmDescricaoProduto.Height := 14;
  end;

  // Se a largura da coluna 'Descrição do produto' for suficiente,
  // exibe o título da coluna sem abreviações
  if (rlmDescricaoProduto.Width > 72) then
    rlmDescricaoProduto.Lines.Text := ACBrStr('DESCRIÇÃO DO PRODUTO / SERVIÇO')
  else
    rlmDescricaoProduto.Lines.Text := 'DESCR. PROD. / SERV.';
  ControlaExibicaoColunaDesconto(vWidthAux, vLeftAux);

  case fpDANFe.Fonte.Nome of
    nfTimesNewRoman, nfArial:
      iAlturaMinLinha := 11;
  else
      iAlturaMinLinha := 10;
  end;

  if fpDANFe.EspacoEntreProdutos > iAlturaMinLinha + 2 then
    rlbItens.Height := fpDANFe.EspacoEntreProdutos
  else
    rlbItens.Height := iAlturaMinLinha;

  pnlItens.Height := rlbItens.Height;
  txtCodigo.Top := 0;
  txtEAN.Top := txtCodigo.Top;
  rlmDescricao.Top := txtCodigo.Top;
  txtNCM.Top := txtCodigo.Top;
  txtCST.Top := txtCodigo.Top;
  txtCFOP.Top := txtCodigo.Top;
  txtUnidade.Top := txtCodigo.Top;
  txtQuantidade.Top := txtCodigo.Top;
  txtValorUnitario.Top := txtCodigo.Top;
  txtValorTotal.Top := txtCodigo.Top;
  txtValorDesconto.Top := txtCodigo.Top;
  txtBaseICMS.Top := txtCodigo.Top;
  txtValorICMS.Top := txtCodigo.Top;
  txtBaseICMSST.Top := txtCodigo.Top;
  txtValorICMSST.Top := txtCodigo.Top;
  txtValorIPI.Top := txtCodigo.Top;
  txtAliqICMS.Top := txtCodigo.Top;
  txtAliqIPI.Top := txtCodigo.Top;

  // Posiciona o canhoto do fpDANFe no cabeçalho ou rodapé
  case fpDANFe.PosCanhoto of
    prCabecalho:
    begin
      pnlCanhoto.Align := faLeftMost;
      pnlDivisao.Align := faLeftMost;
      pnlCanhoto.Left := 26;
      pnlDivisao.Left := pnlCanhoto.Left + pnlCanhoto.Width;
    end;

    prRodape:
    begin
      pnlCanhoto.Align := faRightMost;
      pnlDivisao.Align := faRightMost;
      pnlDivisao.Left := 1024;
      pnlCanhoto.Left := pnlDivisao.Left + pnlDivisao.Width;
    end;
  end;

  // Posiciona a Marca D'água
  rliMarcaDagua1.Left := rlbItens.Left + (rlbItens.Width div 2) - (rliMarcaDagua1.Width div 2);

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
      Width := 450;

      TDFeReportFortes.AjustarLogo(rliLogo, fpDANFe.ExpandeLogoMarcaConfig);
    end;
  end;

  AjustaFontesReport;

  AplicarParametros; // Aplica os parâmetros escolhidos

  DefinirDadosAdicionais;
  DefinirCabecalho;
  DefinirEmitente;
  DefinirDestinatario;
  DefinirRetirada;
  DefinirEntrega;
  DefinirImposto;
  DefinirISSQN;
  DefinirTransporte;
  AdicionarFaturaReal;
  AdicionarFatura;
  AdicionarInformacoesPagamento;
  DefinirCabecalhoItens;
  DefinirObservacoes;

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
    RLDraw50.Height := (rlmDadosAdicionais.Top + rlmDadosAdicionais.Height) - RLLabel77.Top + 2;
    RLDraw51.Height := RLDraw50.Height;
    RLDraw5.Height := RLDraw50.Height;

    RLAngleLabel9.Caption:= 'DADOS';
    RLAngleLabel9.Alignment:= taCenter;
    RLAngleLabel9.Top:= RLDraw50.Top + 2;
    RLAngleLabel9.Height:= RLDraw50.Height -4;

    RLAngleLabel10.Caption:='ADIC.';
    RLAngleLabel10.Alignment:= taCenter;
    RLAngleLabel10.Top:= RLDraw50.Top + 2;
    RLAngleLabel10.Height:= RLDraw50.Height -4;

    rllSistema.Top  := RLDraw50.Top + RLDraw50.Height;
    rllUsuario.Top  := rllSistema.Top;
  end;

end;

procedure TfrlDANFeRLPaisagem.DefinirCabecalho;
begin
  with fpNFe.InfNFe, fpNFe.Ide do
  begin
    rllChave.Caption := FormatarChaveAcesso(fpNFe.InfNFe.Id);
    rllChave.AutoSize := True;

    while (rliChave.Width < (rllChave.Width + 10 + (rllChave.Left - rliChave.Left))) do
      rllChave.Font.Size := rllChave.Font.Size - 1;  //para nao truncar a chave vai diminuir o fonte

    rlbCodigoBarras.Visible := True;
    rlbCodigoBarras.Caption := OnlyNumber(fpNFe.InfNFe.Id);
    rllNumNF0.Caption := ACBrStr('Nº ') + FormatFloat('000,000,000', nNF);
    rllNumNF1.Caption := rllNumNF0.Caption;
    rllSERIE0.Caption := ACBrStr('SÉRIE ') + IntToStr(Serie);
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

    case fpNFe.Ide.tpEmis of
      teNormal, teSCAN, teSVCAN, teSVCRS, teSVCSP:
      begin
        rllDadosVariaveis1a.Visible := (fpNFe.procNFe.cStat > 0);
        rllDadosVariaveis1b.Visible := rllDadosVariaveis1a.Visible;
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
        rlbAvisoContingencia.Visible := EstaVazio(fpNFe.procNFe.nProt);
        rllAvisoContingencia.Caption := ACBrStr('DANFE impresso em contingência - DPEC regularmente recebida pela Receita Federal do Brasil');
        if not rlbAvisoContingencia.Visible then // DPEC TRANSMITIDO
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

procedure TfrlDANFeRLPaisagem.DefinirEmitente;
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
        IfThen(NaoEstaVazio(xCpl), Trim(XCpl), '') +
        ' - ' + Trim(XBairro);
      sTemp := sTemp + ' CEP:' + FormatarCEP(CEP) + ' - ' + XMun + ' - ' + UF;
      rlmEndereco.Lines.add(sTemp);

      sTemp := 'TEL: ' + FormatarFone(Fone);
      rlmEndereco.Lines.add(sTemp);
    end;
  end;
  if NaoEstaVazio(fpDANFe.Site) then
    rlmEndereco.Lines.Add(fpDANFe.Site);
  if NaoEstaVazio(fpDANFe.Email) then
    rlmEndereco.Lines.Add(fpDANFe.Email);

  rlmEndereco.Height := rliEmitente.Height - rlmEndereco.Top - 15;
end;

procedure TfrlDANFeRLPaisagem.DefinirDestinatario;
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

procedure TfrlDANFeRLPaisagem.DefinirImposto;
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

    // Exibe o Valor total dos tributos se vTotTrib for informado
    // e ajusta a posição dos outros campos para "abrir espaço" para ele.
    if (vTotTrib > 0) and (fpDANFe.ImprimeTributos = trbNormal) then
    begin
      rllTotalTributos.Caption := FormatFloatBr(vTotTrib);
      rliDivImposto4.Visible := True;
      rllTituloTotalTributos.Visible := True;
      rllTotalTributos.Visible := True;

      rliDivImposto4.Left := 638;
      rllTituloTotalTributos.Left := rliDivImposto4.Left + 3;
      rllTotalTributos.Left := rliDivImposto4.Left + 7;
      rllTotalTributos.Width := (rliDivImposto5.Left - 7) - (rliDivImposto4.Left + 3);

      LarguraCampo := 151;

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

      LarguraCampo := 189;

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

    rllTituloBaseICMS.Left := rliDivImposto0.Left + 3;
    rllBaseICMS.Left := rllTituloBaseICMS.Left + 4;
    rllBaseICMS.Width := (rliDivImposto1.Left - 7) - (rliDivImposto0.Left + 3);
  end;
end;

procedure TfrlDANFeRLPaisagem.DefinirTransporte;
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
      if NaoEstaVazio(Trim(CNPJCPF)) then
        rllTransCNPJ.Caption := FormatarCNPJouCPF(CNPJCPF)
      else
        rllTransCNPJ.Caption := '';

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

procedure TfrlDANFeRLPaisagem.DefinirDadosAdicionais;
begin
  rlmDadosAdicionaisAuxiliar.Lines.Text :=
    fpDANFe.ManterInformacoesDadosAdicionais( fpNFe  );
end;

procedure TfrlDANFeRLPaisagem.DefinirObservacoes;
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

procedure TfrlDANFeRLPaisagem.DefinirISSQN;
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


procedure TfrlDANFeRLPaisagem.rlbItensAfterPrint(Sender: TObject);
begin
  FundoItem.Color := fpCorDestaqueProdutos;
  FundoItem1.Color := fpCorDestaqueProdutos;
  FundoItem.Visible := not (FundoItem.Visible) and fpDANFe.AlternaCoresProdutos;
  FundoItem1.Visible := FundoItem.Visible;
end;

procedure TfrlDANFeRLPaisagem.rlbDadosAdicionaisBeforePrint(Sender: TObject; var PrintIt: Boolean);
var
  iAumento: Integer;
begin
  iAumento := pnlCanhoto.Width + pnlDivisao.Width;
  pnlCanhoto.Visible := False;
  pnlDivisao.Visible := False;
  rliChave.Width := rliChave.Width + iAumento;
  rliChave2.Width := rliChave2.Width + iAumento;
  rliChave3.Width := rliChave3.Width + iAumento;
  rliNatOpe.Width := rliNatOpe.Width + iAumento;
  rliNatOpe1.Width := rliNatOpe1.Width + iAumento;
  rllDadosVariaveis1a.Left := rllDadosVariaveis1a.Left + (iAumento div 2);
  rllDadosVariaveis1b.Left := rllDadosVariaveis1b.Left + (iAumento div 2);
  rlbCodigoBarras.Left := rlbCodigoBarras.Left + (iAumento div 2);
  rllXmotivo.Left := rllXmotivo.Left + (iAumento div 2);
  LinhaDCDireita.Left := LinhaDCDireita.Left + iAumento;
  LinhaDCSuperior.Width := LinhaDCSuperior.Width + iAumento;
  LinhaDCInferior.Width := LinhaDCInferior.Width + iAumento;

  rllborda.Width := rllborda.Width + iAumento;
  rllCinza.Width := rllCinza.Width + iAumento;
  pnlCabecalho.Width := pnlCabecalho.Width + iAumento;
  pnlCabecalho1.Left := pnlCabecalho1.Left + iAumento;
  rlmDescricaoProduto.Width := rlmDescricaoProduto.Width + iAumento;
end;

procedure TfrlDANFeRLPaisagem.FormCreate(Sender: TObject);
begin
  inherited;
  FineTuneAngleLabels := True;
  rllborda.Align := faNone;
  rllCinza.Align := faNone;
end;

procedure TfrlDANFeRLPaisagem.AdicionarFaturaReal;
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
        RlbDadoPagamento.Caption := ACBrStr('PAGAMENTO À VISTA');

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
procedure TfrlDANFeRLPaisagem.AplicarParametros;
var
  base: Integer;
  AltLinhaComun: Integer;
begin
  AltLinhaComun := fpDANFe.AltLinhaComun;

  // ******** Cabeçalho ********
  base := rliNatOpe.Top;//RLDraw6.Top;
  rliNatOpe.Height := 2 * AltLinhaComun + 1;
  rliNatOpe1.Top := base + AltLinhaComun;
  RLDraw9.Height := AltLinhaComun + 1;

  RLDraw10.Top := base + AltLinhaComun;
  RLDraw10.Height := AltLinhaComun + 1;
  RLDraw2.Top := base + AltLinhaComun;
  RLDraw2.Height := AltLinhaComun + 1;
  RLLabel28.Holder := nil;//para o compando abaixo ter efeito.
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
  rlbEmitente.Height := rliNatOpe.Height + rliEmitente.Height + 2; //168 + (2*AltLinhaComun - 60);

  // ******** DefinirDestinatario ********
  base := quadroDestinatarioExterno.Top;
  quadroDestinatarioExterno.Height := 3 * AltLinhaComun + 1;
  RLDraw13.Height := 3 * AltLinhaComun + 1;

  RLDraw16.Top := base + AltLinhaComun;
  RLDraw17.Top := base + 2 * AltLinhaComun;
  RLDraw16.Left := RLDraw13.Left;
  RLDraw17.Left := RLDraw13.Left;

  RLDraw18.Height := 3 * AltLinhaComun + 1;
  RLDraw19.Height := AltLinhaComun;

  RLDraw20.Top := base + AltLinhaComun;
  RLDraw20.Height := AltLinhaComun;
  RLDraw21.Top := base + AltLinhaComun;
  RLDraw21.Height := AltLinhaComun;

  RLDraw22.Top := base + 2 * AltLinhaComun;
  RLDraw22.Height := AltLinhaComun;
  RLDraw23.Top := base + 2 * AltLinhaComun;
  RLDraw23.Height := AltLinhaComun;
  RLDraw24.Top := base + 2 * AltLinhaComun;
  RLDraw24.Height := AltLinhaComun;

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

  rllDestCidade.Top := base + 3 * AltLinhaComun - rllDestCidade.Height; //13;
  rllDestFone.Top := base + 3 * AltLinhaComun - rllDestFone.Height;
  rllDestUF.Top := base + 3 * AltLinhaComun - rllDestUF.Height;
  rllDestIE.Top := base + 3 * AltLinhaComun - rllDestIE.Height;
  rllHoraSaida.Top := base + 3 * AltLinhaComun - rllHoraSaida.Height;

  RLAngleLabel1.Top := quadroDestinatarioExterno.Top + 1;
  RLAngleLabel1.Height := quadroDestinatarioExterno.Height - 3;
  RLAngleLabel2.Top := quadroDestinatarioExterno.Top + 1;
  RLAngleLabel2.Height := quadroDestinatarioExterno.Height - 3;
  RLAngleLabel1.AutoSize := False;//vai ajustar o tamanho da fonte se necessário.
  RLAngleLabel2.AutoSize := False;
  // Band DefinirDestinatario
  rlbDestinatario.Height := quadroDestinatarioExterno.Height + 2;// 92 + (3*AltLinhaComun - 90);

 // ******** Retirada ********
  AplicarParametrosRetirada;
  // ******** Entrega ********
  AplicarParametrosEntrega;

  // ******** Fatura ********
  base := RLDrawFaturareal.Top;
  RLDrawFaturareal.Height := AltLinhaComun + 4;
  RLDraw8.Height := RLDrawFaturareal.Height;

  RLLabelPag.Top := base + 1;
  RLLabelNUmero.Top := RLLabelPag.Top;
  RLLabelValor.Top := RLLabelPag.Top;
  RLLabelDupl.Top := RLLabelPag.Top;
  RLLabelLIQ.Top := RLLabelPag.Top;

  RLDraw27.Top := RLLabelPag.Top + RLLabelPag.Height;
  RLLabel5.top := 1;
  RLLabel5.Height := RLDraw27.Top - base - 2;

  RlbDadoPagamento.Top := base + AltLinhaComun - RlbDadoPagamento.Height;
  if RlbDadoPagamento.Top < RLDraw27.Top then
    RlbDadoPagamento.Top := RLDraw27.Top + 1;

  RlbDadoNumero.Top := RlbDadoPagamento.Top;
  RlbDadoValorOriginal.Top := RlbDadoPagamento.Top;
  RlbDadoValorDesconto.Top := RlbDadoPagamento.Top;
  RlbDadoValorLiquido.Top := RlbDadoPagamento.Top;

  RLAngleLabel3.Top := RLDrawFaturareal.Top + 1;
  RLAngleLabel3.Height := RLDrawFaturareal.Height - 3;
  RLAngleLabel3.AutoSize := False;
  // Band fatura
  RLBFaturaReal.Height := RLDrawFaturareal.Top + RLDrawFaturareal.Height + 2;

  // ******** Cálculo do DefinirImposto ********
  base := RLDraw29.Top;
  RLDraw29.Height := 2 * AltLinhaComun + 1;
  rliDivImposto0.Height := 2 * AltLinhaComun + 1;

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
  RLDraw6.Height := AltLinhaComun;
  RLDraw12.Height := AltLinhaComun;
  RLDraw33.Top := base + AltLinhaComun;
  RLDraw34.Top := base + AltLinhaComun;
  RLDraw35.Top := base + AltLinhaComun;
  RLDraw6.Top := base + AltLinhaComun;
  RLDraw12.Top := base + AltLinhaComun;

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

  RLAngleLabel4.Top := RLDraw29.Top + 1;
  RLAngleLabel4.Height := RLDraw29.Height - 3;
  RLAngleLabel5.Top := RLDraw29.Top + 1;
  RLAngleLabel5.Height := RLDraw29.Height - 3;
  RLAngleLabel4.AutoSize := False;
  RLAngleLabel5.AutoSize := False;
  // Band Calculo do DefinirImposto
  rlbImposto.Height := 62 + (2 * AltLinhaComun - 60);

  // ******** Transportadora ********
  base := rliTransp.Top;
  rliTransp.Height := 3 * AltLinhaComun + 2;
  if fpNFe.Transp.Vol.Count > 1 then//contem mais volumes na fpDANFe
    rliTransp.Height := rliTransp.Height + ((rllTransQTDE.Height) * (fpNFe.Transp.Vol.Count - 1));//a quantidade de volumes pode variar, entao é feito um recalculo

  RLDraw38.Top := base + AltLinhaComun;
  RLDraw39.Top := base + 2 * AltLinhaComun;

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
  rliTransp6.Height := rliTransp.Height;

  RLDraw70.Top := base + AltLinhaComun;
  RLDraw70.Height := AltLinhaComun;

  rliTransp1.Top := base + 2 * AltLinhaComun;
  rliTransp2.Top := base + 2 * AltLinhaComun;
  rliTransp3.Top := base + 2 * AltLinhaComun;
  rliTransp4.Top := base + 2 * AltLinhaComun;
  rliTransp1.Height := rliTransp.Height - rliTransp1.Top;//AltLinhaComun;
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

  RLAngleLabel6.Top := rliTransp.Top + 1;
  RLAngleLabel6.Height := rliTransp.Height - 3;
  RLAngleLabel7.Top := rliTransp.Top + 1;
  RLAngleLabel7.Height := rliTransp.Height - 3;
  RLAngleLabel6.AutoSize := False;
  RLAngleLabel7.AutoSize := False;
  // Band Transportadora
  rlbTransp.Height := rliTransp.Height + 2;// 110 + (3*AltLinhaComun - 90);

  // ******** Produtos ********
  ///  rlbObsItem.Height      := 12 + fEspacoEntreProdutos; // Remove espaço entre produtos com EspacoEntreProdutos = 0
end;

procedure TfrlDANFeRLPaisagem.subItensDataRecord(Sender: TObject; RecNo, CopyNo: Integer; var EOF: Boolean; var RecordAction: TRLRecordAction);
begin
  inherited;

  FNumItem := RecNo - 1;
  EOF := (RecNo > fpNFe.Det.Count);
  RecordAction := raUseIt;
end;

procedure TfrlDANFeRLPaisagem.RLNFeDataRecord(Sender: TObject; RecNo, CopyNo: Integer; var EOF: Boolean; var RecordAction: TRLRecordAction);
begin
  inherited;

  EOF := (RecNo > 1);
  RecordAction := raUseIt;
end;

procedure TfrlDANFeRLPaisagem.rlbCabecalhoItensBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  pnlCabecalho1.Left := pnlCabecalho.Width + 1;
end;

procedure TfrlDANFeRLPaisagem.rlbItensBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  // Controla os itens por página
  with fpNFe.Det.Items[fNumItem] do
  begin
    txtCodigo.Lines.Text := Prod.CProd;
    txtEAN.Caption := Prod.cEAN;
    rlmDescricao.Lines.Text := fpDANFe.ManterXProd(fpNFe, fNumItem);
    RLMemoInfAd.Lines.Text := ManterBandinfAdProd(infAdProd);
    txtNCM.Caption := Prod.NCM;
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

    if fpDANFe.ImprimeTotalLiquido then
    begin
      txtValorTotal.Caption := FormatFloatBr(fpDANFe.ManterVDesc(Prod.vDesc, Prod.vUnCom, Prod.qCom));
      txtValorDesconto.Caption := FormatFloatBr(Prod.vProd - fpDANFe.ManterVDesc(Prod.vDesc, Prod.vUnCom, Prod.qCom));
    end
    else
    begin
      txtValorTotal.Caption := FormatFloatBr(Prod.vProd);
      txtValorDesconto.Caption := FormatFloatBr(fpDANFe.ManterVDesc(Prod.vDesc, Prod.vUnCom, Prod.qCom), FloatMask);
    end;

    txtBaseICMS.Caption := FormatFloatBr(Imposto.ICMS.VBC);
    txtValorICMS.Caption := FormatFloatBr(Imposto.ICMS.VICMS);
    txtBaseICMSST.Caption := FormatFloatBr(Imposto.ICMS.vBCST);
    txtValorICMSST.Caption := FormatFloatBr(Imposto.ICMS.vICMSST);
    txtValorIPI.Caption := FormatFloatBr(Imposto.IPI.VIPI);
    txtAliqICMS.Caption := FormatFloatBr(Imposto.ICMS.PICMS);
    txtAliqIPI.Caption := FormatFloatBr(Imposto.IPI.PIPI);
  end;
end;

function TfrlDANFeRLPaisagem.ManterBandinfAdProd(const sInforAdicProduto: String): String;
begin
  Result := Trim(sInforAdicProduto);
  Result := StringReplace(Result, ';', sLineBreak, [rfReplaceAll]);

  RLBandInfAd.Visible := (Result <> '') and (fpDANFe.ExibeInforAdicProduto = infSeparadamente);
end;

procedure TfrlDANFeRLPaisagem.RLMemoInfAdAfterPrint(Sender: TObject);
begin
  inherited;

  if pnlCanhoto.Visible then
    fTamanhoBandInf := RLDInfAdcBarra.Width
  else
  begin
    RLBandInfAd.Width := fTamanhoBandInf + (pnlCanhoto.Width + pnlDivisao.Width);
    RLDrawFinal.Left := RLBandInfAd.Width - 1;
  end;
end;

procedure TfrlDANFeRLPaisagem.DefinirCabecalhoItens;
begin
  //   Configura Cabecalho dos Itens
  case fpNFe.Emit.CRT of
    crtRegimeNormal, crtSimplesExcessoReceita:
    begin
      lblCST1.Caption := 'CST';
      lblCST1.Font.Size := 5;
      lblCST2.Caption := '';
      lblCST2.Font.Size := 5;
    end;

    crtSimplesNacional:
    begin
      lblCST1.Caption := 'CSOSN';
      lblCST1.Font.Size := 4;
      lblCST2.Caption := '/ CST';
      lblCST2.Font.Size := 4;
    end;
  end;

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
end;

procedure TfrlDANFeRLPaisagem.DefinirRetirada;
begin
  with fpNFe.Retirada do
  begin
    rlbQuandoRetirada.Visible := NaoEstaVazio(xLgr);

    if rlbQuandoRetirada.Visible then
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

procedure TfrlDANFeRLPaisagem.DefinirEntrega;
begin
  with fpNFe.Entrega do
  begin
    rlbQuandoEntrega.Visible := NaoEstaVazio(xLgr);

    if rlbQuandoEntrega.Visible then
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

procedure TfrlDANFeRLPaisagem.AplicarParametrosRetirada;
begin
  // Defime os padrão de Retirada

  RLLquadroRetiradaNome.Font := rllDestNome.Font;

  RLLquadroRetiradaDocumento.Font := RLLquadroRetiradaNome.Font;
  RLLquadroRetiradaIE.Font := RLLquadroRetiradaNome.Font;
  RLLquadroRetiradaEndereco.Font := RLLquadroRetiradaNome.Font;
  RLLquadroRetiradaBairro.Font := RLLquadroRetiradaNome.Font;
  RLLquadroRetiradaCEP.Font := RLLquadroRetiradaNome.Font;
  RLLquadroRetiradaMunicipio.Font := RLLquadroRetiradaNome.Font;
  RLLquadroRetiradaUF.Font := RLLquadroRetiradaNome.Font;
  RLLquadroRetiradaTelefone.Font := RLLquadroRetiradaNome.Font;
end;

procedure TfrlDANFeRLPaisagem.AplicarParametrosEntrega;
begin
  // Defime os padrão de entrega

  RLLquadroEntregaNome.Font := rllDestNome.Font;

  RLLquadroEntregaDocumento.Font := RLLquadroEntregaNome.Font;
  RLLquadroEntregaIE.Font := RLLquadroEntregaNome.Font;
  RLLquadroEntregaEndereco.Font := RLLquadroEntregaNome.Font;
  RLLquadroEntregaBairro.Font := RLLquadroEntregaNome.Font;
  RLLquadroEntregaCep.Font := RLLquadroEntregaNome.Font;
  RLLquadroEntregaMunicipio.Font := RLLquadroEntregaNome.Font;
  RLLquadroEntregaUF.Font := RLLquadroEntregaNome.Font;
  RLLquadroEntregaTelefone.Font := RLLquadroEntregaNome.Font;
end;


procedure TfrlDANFeRLPaisagem.ControlaExibicaoColunaDesconto(var vWidthAux: Integer; var vLeftAux: Integer);
begin
  // Controle para exibir coluna de desconto
  txtValorDesconto.Visible := fpDANFe.ManterColunaDesconto( fpNFe.Total.ICMSTot.vDesc );
  lblPercValorDesc.Visible := txtValorDesconto.Visible;
  lblPercValorDesc1.Visible := txtValorDesconto.Visible;
  LinhaDesconto.Visible := txtValorDesconto.Visible;
  rlsDivdesconto.Visible := txtValorDesconto.Visible;
  // Ajusta a posição da coluna 'VALOR TOTAL'
  if (txtValorDesconto.Visible) then
  begin
    // Quantidade
    lblqtd.Width := 51;
    txtQuantidade.Width := 51;
    // Linha Valor Unitario
    rlsVunitario.Left := 180;
    LinhaValorUnitario.Left := 180;
    // Valor Unitario
    lblValorUnit.Left := 182;
    lblValorUnit.Width := 62;
    lblValorUnitarioInf.Left := 182;
    lblValorUnitarioInf.Width := 62;
    txtValorUnitario.Left := 182;
    txtValorUnitario.Width := 62;
    // Linha Valor Total
    rlsDivvalortotal.Left := 244;
    LinhaValorTotal1.Left := 244;
    // Valor Total
    lblValorTotalSup.Left := 247;
    lblValorTotalSup.Width := 43;
    txtValorTotal.Left := 247;
    txtValorTotal.Width := 43;
    lblValorTotal.Left := 247;
    lblValorTotal.Width := 43;
  end
  else
  begin
    // Calcula o espaço a distribuir entre os campos qtd, vlunit e vltot pela
    // diferença entre a posição inicial da qtd e a posição final do desconto, descontando 4 para separação de cada campó que restou
    vWidthAux := (txtValorDesconto.Left + txtValorDesconto.Width) - txtQuantidade.Left - 12;
    vWidthAux := Trunc(vWidthAux / 3);
    vLeftAux := txtQuantidade.Left;
    // Quantidade
    lblqtd.Width := vWidthAux;
    txtQuantidade.Width := vWidthAux;
    // Linha Valor Unitario
    rlsVunitario.Left := vLeftAux + vWidthAux + 2;
    LinhaValorUnitario.Left := vLeftAux + vWidthAux + 2;
    vLeftAux := vLeftAux + vWidthAux + 4;
    // Valor Unitario
    lblValorUnit.Left := vLeftAux;
    lblValorUnit.Width := vWidthAux;
    lblValorUnitarioInf.Left := vLeftAux;
    lblValorUnitarioInf.Width := vWidthAux;
    txtValorUnitario.Left := vLeftAux;
    txtValorUnitario.Width := vWidthAux;
    // Linha Valor Total
    rlsDivvalortotal.Left := vLeftAux + vWidthAux + 2;
    LinhaValorTotal1.Left := vLeftAux + vWidthAux + 2;
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
  if (lblqtd.Width > 47) then
    lblqtd.Caption := ACBrStr('QUANTIDADE')
  else
    lblqtd.Caption := ACBrStr('QUANT.');
  // Se a largura da coluna 'Valor Unitário' for suficiente,
  // exibe o título em uma linha apenas
  if (lblValorUnitarioInf.Width > 61) then
  begin
    lblValorUnit.Visible := False;
    lblValorUnitarioInf.Caption := ACBrStr('VALOR UNITÁRIO');
    lblValorUnitarioInf.Top := 8;
  end
  else
  begin
    lblValorUnit.Visible := True;
    lblValorUnitarioInf.Caption := ACBrStr('UNITÁRIO');
    lblValorUnitarioInf.Top := 10;
  end;
  // Se a largura da coluna 'Valor Total' for suficiente,
  // exibe o título em uma linha apenas
  if (lblValorTotal.Width > 52) then
  begin
    lblValorTotalSup.Visible := False;
    lblValorTotal.Caption := ACBrStr('VALOR TOTAL');
    lblValorTotal.Top := 8;
  end
  else
  begin
    lblValorTotalSup.Visible := True;
    lblValorTotal.Caption := ACBrStr('TOTAL');
    lblValorTotal.Top := 10;
  end;
end;

procedure TfrlDANFeRLPaisagem.rlbContinuacaoInformacoesComplementaresBeforePrint(
  Sender: TObject; var PrintIt: Boolean);
begin
  if (not fpDANFe.ImprimeContinuacaoDadosAdicionaisPrimeiraPagina) and (RLNFe.PageNumber = 1) then
  begin
    RLNFe.NewPageNeeded := True;
  end;
end;

procedure TfrlDANFeRLPaisagem.AdicionarFatura;
  function ManterDuplicatas( ValueMaximo : Integer ): Integer;
  var
    x: Integer;
  begin
    // ValueMaximo - > Quantidade de Label's Disponivel

    Result := min(fpNFe.Cobr.Dup.Count, ValueMaximo);

    if (Result < 6) then
      TRLLabel(FindComponent('rllFatura')).Caption := 'DUPL';

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

  if (fpNFe.Cobr.Dup.Count > 0) then
  begin
    iQuantidadedeLabel := 15;
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
                      rliFatura3 ,
                      rliFatura4 , rliFatura5]
                    );

    {=============== Centraliza o label "DUPLICATA" ===============}
    rllFatura.Top := (rlbFatura.Height - rllFatura.Height) div 2;
  end;
end;

procedure TfrlDANFeRLPaisagem.AdicionarInformacoesPagamento;
  function ManterInformacoesPagamento(ValueMaximo : Integer ): Integer;
  var
    x: Integer;
  begin
    // ValueMaximo - > Quantidade de Label's Disponivel

    Result := min(fpNFe.pag.Count, ValueMaximo);

    if (Result < 6) then
      TRLLabel(FindComponent('rllPagamento')).Caption := 'PAGT';

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
    iQuantidadedeLabel := 15;

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
                    [ rliPagamentoReal0 ,
                      rliPagamentoReal1 ,
                      rliPagamentoReal2 ,
                      rliPagamentoReal3 ,
                      rliPagamentoReal4 ]
                    );

    {=============== Centraliza o label "DUPLICATA" ===============}
    rllPagamento.Top := (rlbPagamentoReal.Height - rllPagamento.Height) div 2;
  end;
end;

procedure TfrlDANFeRLPaisagem.AjustaFontesReport;
var
  vWidthAux: Integer;
  vLeftAux: Integer;
  b: Integer;
  i: Integer;
  j: Integer;
  vAutoSizeAux: Boolean;
  UmaBand: TRLBand;
begin
  // Altera a fonte da Razão Social do DefinirEmitente
  rlmEmitente.Font.Size := fpDANFe.Fonte.TamanhoFonteRazaoSocial;
  for b := 0 to (RLNFe.ControlCount - 1) do
  begin
    //percorrendo todos os controls do report.
    UmaBand := TRLBand(RLNFe.Controls[b]);
    for i := 0 to (UmaBand.ControlCount - 1) do
    begin
      // percorrendo os subcontrols...
      //Altera o tamanho da fonte dos demais campos
      if (UmaBand.Controls[i].Tag in [3, 70]) or
         ((TRLLabel(UmaBand.Controls[i]).Parent = rlbFatura) and
          (fpDANFe <> nil) and
          (TRLLabel(UmaBand.Controls[i]).Font.Size > fpDANFe.Fonte.TamanhoFonteDemaisCampos + 1)) then
      begin
        TRLLabel(UmaBand.Controls[i]).Font.Style := [];
        if fpDANFe.Fonte.Negrito then
          // Dados em negrito
          TRLLabel(UmaBand.Controls[i]).Font.Style := [fsBold];
        if (fpDANFe <> nil) then
          TRLLabel(UmaBand.Controls[i]).Font.Size := fpDANFe.Fonte.TamanhoFonteDemaisCampos + 1;
      end;

      if (UmaBand.Controls[i].Tag = 20) then
      begin
        TRLLabel(UmaBand.Controls[i]).Font.Size := fpDANFe.Fonte.TamanhoFonteInformacoesComplementares;
      end;

      // Altera a fonde do fpDANFe
      case fpDANFe.Fonte.Nome of
        nfArial:
          begin
            for j := 0 to (TRLPanel(UmaBand.Controls[i]).ControlCount - 1) do
              TRLLabel(TRLPanel(UmaBand.Controls[i]).Controls[j]).Font.Name := 'Arial';
            if (UmaBand.Controls[i].Tag <> 20) then
              TRLLabel(UmaBand.Controls[i]).Font.Name := 'Arial';
            if (UmaBand.Controls[i].Tag = 3) then
              TRLLabel(UmaBand.Controls[i]).Font.Size := TRLLabel(UmaBand.Controls[i]).Font.Size - 1;
          end;
        nfCourierNew:
          begin
            for j := 0 to ((TRLPanel(UmaBand.Controls[i]).ControlCount) - 1) do
            begin
              TRLLabel(TRLPanel(UmaBand.Controls[i]).Controls[j]).Font.Name := 'Courier New';
              if TRLPanel(UmaBand.Controls[i]).Controls[j].Tag = 0 then
                TRLLabel(TRLPanel(UmaBand.Controls[i]).Controls[j]).Font.Size := (TRLLabel(TRLPanel(UmaBand.Controls[i]).Controls[j]).Font.Size - 1);
            end;
            TRLLabel(UmaBand.Controls[i]).Font.Name := 'Courier New';
            if (UmaBand.Controls[i].Tag = 0) or (UmaBand.Controls[i].Tag = 3) then
              TRLLabel(UmaBand.Controls[i]).Font.Size := TRLLabel(UmaBand.Controls[i]).Font.Size - 1;
            if (UmaBand.Controls[i].Tag = 40) then
              TRLLabel((UmaBand).Controls[i]).Top := TRLLabel((UmaBand).Controls[i]).Top - 1;
          end;
        nfTimesNewRoman:
          begin
            for j := 0 to (TRLPanel(UmaBand.Controls[i]).ControlCount - 1) do
              TRLLabel(TRLPanel(UmaBand.Controls[i]).Controls[j]).Font.Name := 'Times New Roman';
            if (UmaBand.Controls[i].Tag <> 20) then
              TRLLabel(UmaBand.Controls[i]).Font.Name := 'Times New Roman';
            if (UmaBand.Controls[i].Tag = 3) then
              TRLLabel(UmaBand.Controls[i]).Font.Size := TRLLabel(UmaBand.Controls[i]).Font.Size - 1;
          end;
      end;
      //copia o left e width do componente, alterar o size do fonte, com o autosize ajusta o height, depois retorna como estava
      if (UmaBand.Controls[i].Tag in [3, 70]) then
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
    rllNumNF1.Font.Size := rllNumNF1.Font.Size - 2;
    rllNumNF1.Top := rllNumNF1.Top + 1;
  end;

  if (fpDANFe.Fonte.TamanhoFonteEndereco > 0) then
    RLMEndereco.Font.Size := fpDANFe.Fonte.TamanhoFonteEndereco
  else
    RLMEndereco.Font.Size := 7;
end;


procedure TfrlDANFeRLPaisagem.AjustaQuadro(iQuantlinhas: Integer;
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
  iColunas := 5;          // Quantidade de colunas
  iAltLinha := 12;        // Altura de cada linha
  iPosQuadro := 0;        // Posição (Top) do Quadro
  iAltQuadro1Linha := 27; // Altura do quadro com 1 linha
  iFolga := 1;   // Distância entre o final da Band e o final do quadro

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

procedure TfrlDANFeRLPaisagem.limparLabels(ValueInicio,ValueRepeticao: Integer;
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

Procedure TfrlDANFeRLPaisagem.AtribuirDePara( ValueDe :  TRLDraw; ValuePara : array of TRLDraw  );
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
