{******************************************************************************}
{ Projeto: Componente ACBrCTe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Conhecimen-}
{ to de Transporte eletrônico - CTe - http://www.cte.fazenda.gov.br            }
{                                                                              }
{ Direitos Autorais Reservados (c) 2014 Mark dos Santos Gonçalves              }
{                                        Juliomar Marchetti                     }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
******************************************************************************}

{$I ACBr.inc}

unit ACBrCTeDACTeRLRetrato;

interface

{$H+}

uses
  SysUtils, Variants, Classes,
  {$IFDEF CLX}
  QGraphics, QControls, QForms, QDialogs, QExtCtrls, Qt, QStdCtrls,
  {$ELSE}
  Messages, Graphics, Controls, Forms, Dialogs, ExtCtrls, MaskUtils, StdCtrls,
  {$ENDIF}
  RLReport, RLFilters, RLPDFFilter,
  pcnConversao, RLBarcode, DB, StrUtils, RLRichText, ACBrCTeDACTeRL,

  RLMetaFile, RLFeedBack, RLParser, RLConsts, RLUtils,
  RLPrintDialog, RLTypes, RLPrinters;

type

  { TfrmDACTeRLRetrato }

  TfrmDACTeRLRetrato = class(TfrmDACTeRL)
    rlb_CTeOS_PrestacaoServico: TRLBand;
    rlb_Dados_Seguradora: TRLBand;
    rlb_03_DadosDACTe_OS: TRLBand;
    rlb_Fluxo_Carga: TRLBand;
    RLBarcode1: TRLBarcode;
    rlb_01_Recibo: TRLBand;
    rlDocOrig_tpDoc3: TRLMemo;
    RLDraw110: TRLDraw;
    RLDraw111: TRLDraw;
    RLDraw112: TRLDraw;
    RLDraw113: TRLDraw;
    RLDraw28: TRLDraw;
    RLDraw29: TRLDraw;
    RLDraw34: TRLDraw;
    RLDraw35: TRLDraw;
    RLLabel201: TRLLabel;
    RLLabel202: TRLLabel;
    RLLabel203: TRLLabel;
    RLLabel204: TRLLabel;
    RLLabel205: TRLLabel;
    RLLabel206: TRLLabel;
    RLLabel207: TRLLabel;
    RLLabel208: TRLLabel;
    RLLabel209: TRLLabel;
    RLLabel210: TRLLabel;
    RLLabel211: TRLLabel;
    RLLabel212: TRLLabel;
    RLLabel213: TRLLabel;
    RLLabel214: TRLLabel;
    RLLabel215: TRLLabel;
    RLLabel216: TRLLabel;
    RLLabel217: TRLLabel;
    RLLabel218: TRLLabel;
    RLLabel219: TRLLabel;
    RLLabel221: TRLLabel;
    RLLabel223: TRLLabel;
    RLLabel224: TRLLabel;
    rllCEPToma1: TRLLabel;
    rllCnpjToma1: TRLLabel;
    rllDestPrestacao1: TRLLabel;
    rllEnderecoToma1: TRLLabel;
    rllFoneToma1: TRLLabel;
    rllInscEstToma1: TRLLabel;
    rllMunToma1: TRLLabel;
    rllApolice: TRLMemo;
    rllOrigPrestacao1: TRLLabel;
    rllPaisToma1: TRLLabel;
    rllPercursoVeiculo: TRLLabel;
    rllRazaoToma1: TRLLabel;
    rllSiglaOrigem: TRLLabel;
    rllSiglaDestino: TRLLabel;
    rllSiglaPassagem: TRLMemo;
    rllResponsavelSeguro: TRLMemo;
    rllNomeSeguradora: TRLMemo;
    rllTituloSeguro: TRLLabel;
    rlsLinhaH03: TRLDraw;
    rlmEmitente: TRLMemo;
    rlmDadosEmitente: TRLMemo;
    rliLogo: TRLImage;
    rllNumCte: TRLLabel;
    rlLabel17: TRLLabel;
    rlLabel18: TRLLabel;
    rlLabel6: TRLLabel;
    rlLabel8: TRLLabel;
    rlLabel21: TRLLabel;
    rlLabel23: TRLLabel;
    rlLabel25: TRLLabel;
    rlLabel33: TRLLabel;
    rlLabel74: TRLLabel;
    rllChave: TRLLabel;
    rllSerie: TRLLabel;
    rllModelo: TRLLabel;
    rllEmissao: TRLLabel;
    rllModal: TRLLabel;
    rllProtocolo: TRLLabel;
    rllTipoCte: TRLLabel;
    rllDescricao: TRLLabel;
    rlLabel77: TRLLabel;
    rllTipoServico: TRLLabel;
    rlLabel28: TRLLabel;
    rllTomaServico: TRLLabel;
    rlLabel78: TRLLabel;
    rllFormaPagamento: TRLLabel;
    rllInscSuframa: TRLLabel;
    rlb_07_HeaderItens: TRLBand;
    rlLabel20: TRLLabel;
    RLDraw32: TRLDraw;
    rlLabel91: TRLLabel;
    rlLabel92: TRLLabel;
    rlLabel96: TRLLabel;
    rlLabel100: TRLLabel;
    rlLabel106: TRLLabel;
    rlLabel109: TRLLabel;
    rld_07_headerItens: TRLDraw;
    rlb_09_Obs: TRLBand;
    rlb_02_Cabecalho: TRLBand;
    rlLabel29: TRLLabel;
    rllNatOperacao: TRLLabel;
    rlLabel12: TRLLabel;
    rllOrigPrestacao: TRLLabel;
    rlLabel14: TRLLabel;
    rllDestPrestacao: TRLLabel;
    rlLabel13: TRLLabel;
    rlLabel16: TRLLabel;
    rlLabel22: TRLLabel;
    rlLabel24: TRLLabel;
    rlLabel26: TRLLabel;
    rllRazaoRemet: TRLLabel;
    rllEnderecoRemet1: TRLLabel;
    rllEnderecoRemet2: TRLLabel;
    rllMunRemet: TRLLabel;
    rllCnpjRemet: TRLLabel;
    rllPaisRemet: TRLLabel;
    rlLabel27: TRLLabel;
    rlLabel30: TRLLabel;
    rlLabel31: TRLLabel;
    rlLabel32: TRLLabel;
    rlLabel79: TRLLabel;
    rllRazaoDest: TRLLabel;
    rllEnderecoDest1: TRLLabel;
    rllEnderecoDest2: TRLLabel;
    rlLabel93: TRLLabel;
    rllInscEstRemet: TRLLabel;
    rlLabel95: TRLLabel;
    rllFoneRemet: TRLLabel;
    rllCEPRemet: TRLLabel;
    rlLabel98: TRLLabel;
    rllMunDest: TRLLabel;
    rllPaisDest: TRLLabel;
    rllCnpjDest: TRLLabel;
    rlLabel114: TRLLabel;
    rllInscEstDest: TRLLabel;
    rlLabel116: TRLLabel;
    rllFoneDest: TRLLabel;
    rllCEPDest: TRLLabel;
    rlLabel119: TRLLabel;
    rlLabel86: TRLLabel;
    rlLabel87: TRLLabel;
    rlLabel88: TRLLabel;
    rlLabel89: TRLLabel;
    rlLabel90: TRLLabel;
    rllRazaoExped: TRLLabel;
    rllEnderecoExped1: TRLLabel;
    rllEnderecoExped2: TRLLabel;
    rlLabel99: TRLLabel;
    rlLabel101: TRLLabel;
    rlLabel102: TRLLabel;
    rlLabel103: TRLLabel;
    rlLabel104: TRLLabel;
    rllRazaoReceb: TRLLabel;
    rllEnderecoReceb1: TRLLabel;
    rllEnderecoReceb2: TRLLabel;
    rllMunExped: TRLLabel;
    rllPaisExped: TRLLabel;
    rllCnpjExped: TRLLabel;
    rlLabel105: TRLLabel;
    rllInscEstExped: TRLLabel;
    rlLabel107: TRLLabel;
    rllFoneExped: TRLLabel;
    rllCEPExped: TRLLabel;
    rlLabel110: TRLLabel;
    rllMunReceb: TRLLabel;
    rllPaisReceb: TRLLabel;
    rllCnpjReceb: TRLLabel;
    rlLabel123: TRLLabel;
    rllInscEstReceb: TRLLabel;
    rlLabel125: TRLLabel;
    rllFoneReceb: TRLLabel;
    rllCEPReceb: TRLLabel;
    rlLabel128: TRLLabel;
    rlLabel80: TRLLabel;
    rlLabel81: TRLLabel;
    rlLabel94: TRLLabel;
    rllCEPToma: TRLLabel;
    rlLabel97: TRLLabel;
    rllEnderecoToma: TRLLabel;
    rlLabel82: TRLLabel;
    rllCnpjToma: TRLLabel;
    rlLabel108: TRLLabel;
    rllInscEstToma: TRLLabel;
    rlLabel111: TRLLabel;
    rllFoneToma: TRLLabel;
    rllRazaoToma: TRLLabel;
    rlLabel113: TRLLabel;
    rllPaisToma: TRLLabel;
    rllMunToma: TRLLabel;
    rlb_10_ModRodFracionado: TRLBand;
    rlb_11_ModRodLot103: TRLBand;
    rlLabel10: TRLLabel;
    RLDraw1: TRLDraw;
    rlmObs: TRLMemo;
    rllTituloLotacao: TRLLabel;
    RLDraw24: TRLDraw;
    rlLabel11: TRLLabel;
    RLDraw36: TRLDraw;
    RLDraw37: TRLDraw;
    RLDraw38: TRLDraw;
    rlLabel83: TRLLabel;
    rlLabel84: TRLLabel;
    rlLabel85: TRLLabel;
    rllRntrcEmpresa: TRLLabel;
    rllLotacao: TRLLabel;
    rllDtPrevEntrega: TRLLabel;
    rlmObsExcEmitente: TRLMemo;
    rllMsgTeste: TRLLabel;
    rlLabel7: TRLLabel;
    RLDraw27: TRLDraw;
    rlb_03_DadosDACTe: TRLBand;
    rlLabel1: TRLLabel;
    rllProdPredominante: TRLLabel;
    rlLabel4: TRLLabel;
    rllOutrasCaracCarga: TRLLabel;
    rlLabel34: TRLLabel;
    rllVlrTotalMerc: TRLLabel;
    rlLabel35: TRLLabel;
    rlLabel36: TRLLabel;
    rlLabel41: TRLLabel;
    rlLabel43: TRLLabel;
    rlLabel5: TRLLabel;
    rlLabel37: TRLLabel;
    rlLabel39: TRLLabel;
    rlLabel40: TRLLabel;
    RLDraw8: TRLDraw;
    RLDraw7: TRLDraw;
    rlb_04_DadosNotaFiscal: TRLBand;
    rlb_05_Complemento: TRLBand;
    rlLabel38: TRLLabel;
    rlLabel44: TRLLabel;
    rlmCompNome1: TRLMemo;
    rlLabel46: TRLLabel;
    rlmCompValor1: TRLMemo;
    rlLabel42: TRLLabel;
    rlmCompNome2: TRLMemo;
    rlLabel45: TRLLabel;
    rlmCompValor2: TRLMemo;
    rlLabel47: TRLLabel;
    rlmCompNome3: TRLMemo;
    rlLabel48: TRLLabel;
    rlmCompValor3: TRLMemo;
    rlLabel49: TRLLabel;
    rllVlrTotServico: TRLLabel;
    rlLabel50: TRLLabel;
    rllVlrTotReceber: TRLLabel;
    RLDraw18: TRLDraw;
    RLDraw17: TRLDraw;
    RLDraw16: TRLDraw;
    RLDraw15: TRLDraw;
    RLDraw19: TRLDraw;
    rlLabel51: TRLLabel;
    rlLabel52: TRLLabel;
    rllSitTrib: TRLLabel;
    rlLabel55: TRLLabel;
    rllBaseCalc: TRLLabel;
    rlLabel56: TRLLabel;
    rllAliqICMS: TRLLabel;
    rlLabel54: TRLLabel;
    rllVlrICMS: TRLLabel;
    rlLabel53: TRLLabel;
    rllRedBaseCalc: TRLLabel;
    rlLabel58: TRLLabel;
    rllICMS_ST: TRLLabel;
    RLDraw26: TRLDraw;
    RLDraw25: TRLDraw;
    RLDraw23: TRLDraw;
    RLDraw22: TRLDraw;
    RLDraw21: TRLDraw;
    RLDraw20: TRLDraw;
    rlLabel59: TRLLabel;
    RLDraw5: TRLDraw;
    RLDraw6: TRLDraw;
    rlLabel61: TRLLabel;
    rlLabel62: TRLLabel;
    rlLabel63: TRLLabel;
    rlLabel64: TRLLabel;
    rlsLinhaH7: TRLDraw;
    rlsLinhaV12: TRLDraw;
    rlsLinhaV13: TRLDraw;
    rlsQuadro01: TRLDraw;
    rlsQuadro02: TRLDraw;
    rlsQuadro04: TRLDraw;
    rlsQuadro05: TRLDraw;
    rlsQuadro08: TRLDraw;
    rlsQuadro09: TRLDraw;
    rlsLinhaV10: TRLDraw;
    rlsLinhaV04: TRLDraw;
    rlsLinhaH01: TRLDraw;
    rlsLinhaH02: TRLDraw;
    rlsLinhaV09: TRLDraw;
    rlsLinhaV08: TRLDraw;
    rlsLinhaV06: TRLDraw;
    rlsLinhaV05: TRLDraw;
    rlsLinhaH04: TRLDraw;
    rlsLinhaV07: TRLDraw;
    rlsLinhaV01: TRLDraw;
    rlsLinhaV11: TRLDraw;
    rlsLinhaH06: TRLDraw;
    rlsLinhaH07: TRLDraw;
    rlsLinhaH05: TRLDraw;
    rlsLinhaH08: TRLDraw;
    RLDraw55: TRLDraw;
    RLDraw9: TRLDraw;
    RLDraw56: TRLDraw;
    RLDraw58: TRLDraw;
    RLDraw59: TRLDraw;
    RLDraw60: TRLDraw;
    RLDraw61: TRLDraw;
    RLDraw62: TRLDraw;
    rlb_17_Sistema: TrlBand;
    RLDraw10: TRLDraw;
    rlLabel65: TRLLabel;
    RLDraw2: TRLDraw;
    rlLabel66: TRLLabel;
    rlLabel70: TRLLabel;
    RLDraw53: TRLDraw;
    RLDraw11: TRLDraw;
    rlLabel71: TRLLabel;
    RLDraw12: TRLDraw;
    rlLabel75: TRLLabel;
    RLDraw13: TRLDraw;
    rlLabel76: TRLLabel;
    rlLabel112: TRLLabel;
    rlLabel115: TRLLabel;
    rlLabel117: TRLLabel;
    RLDraw14: TRLDraw;
    RLDraw31: TRLDraw;
    RLDraw33: TRLDraw;
    rlLabel118: TRLLabel;
    rlLabel120: TRLLabel;
    rlLabel121: TRLLabel;
    RLDraw39: TRLDraw;
    rlLabel122: TRLLabel;
    rlLabel124: TRLLabel;
    rlLabel126: TRLLabel;
    rlLabel127: TRLLabel;
    RLDraw40: TRLDraw;
    RLDraw41: TRLDraw;
    rlLabel129: TRLLabel;
    rlLabel130: TRLLabel;
    rlLabel131: TRLLabel;
    RLDraw42: TRLDraw;
    RLDraw43: TRLDraw;
    RLDraw44: TRLDraw;
    rlb_16_DadosExcEmitente: TRLBand;
    rlLabel15: TRLLabel;
    rllblSistema: TRLLabel;
    rllNomeMotorista: TRLLabel;
    rllCPFMotorista: TRLLabel;
    rllNumRegEsp: TRLLabel;
    rllResponsavel: TRLLabel;
    rllValorTotal: TRLLabel;
    rllLacres: TRLLabel;
    rlmTipo: TRLMemo;
    rlmPlaca: TRLMemo;
    rlmUF: TRLMemo;
    rlmRNTRC: TRLMemo;
    rlmEmpresas: TRLMemo;
    rlmVigencias: TRLMemo;
    rlmNumDispositivo: TRLMemo;
    rlmCodTransacao: TRLMemo;
    RLDraw45: TRLDraw;
    rlmQtdUnidMedida1: TRLMemo;
    rlmQtdUnidMedida2: TRLMemo;
    rlmQtdUnidMedida3: TRLMemo;
    rlmQtdUnidMedida5: TRLMemo;
    rlb_06_ValorPrestacao: TRLBand;
    RLDraw46: TRLDraw;
    RLDraw48: TRLDraw;
    RLDraw49: TRLDraw;
    rlLabel3: TRLLabel;
    rlLabel132: TRLLabel;
    rlLabel133: TRLLabel;
    rlLabel134: TRLLabel;
    rlLabel135: TRLLabel;
    rlLabel136: TRLLabel;
    rlLabel137: TRLLabel;
    rlLabel138: TRLLabel;
    rlLabel139: TRLLabel;
    rlLabel140: TRLLabel;
    rllSerie2: TRLLabel;
    rllNumCTe2: TRLLabel;
    rlLabel143: TRLLabel;
    rlb_12_ModAereo: TRLBand;
    rlb_13_ModAquaviario: TRLBand;
    rlb_14_ModFerroviario: TRLBand;
    rlb_15_ModDutoviario: TRLBand;
    RLDraw47: TRLDraw;
    RLDraw54: TRLDraw;
    RLDraw63: TRLDraw;
    RLDraw64: TRLDraw;
    RLDraw65: TRLDraw;
    RLDraw66: TRLDraw;
    RLDraw67: TRLDraw;
    RLDraw68: TRLDraw;
    RLDraw69: TRLDraw;
    rlLabel141: TRLLabel;
    rlLabel142: TRLLabel;
    rlLabel144: TRLLabel;
    rlLabel145: TRLLabel;
    rlLabel146: TRLLabel;
    rlLabel147: TRLLabel;
    rlLabel148: TRLLabel;
    rlLabel149: TRLLabel;
    rlLabel150: TRLLabel;
    rlLabel153: TRLLabel;
    rllTrecho: TRLLabel;
    rllTarifaValor: TRLLabel;
    rllTarifaCodigo: TRLLabel;
    rllTarifaCL: TRLLabel;
    rllMinuta: TRLLabel;
    rllDadosRetira: TRLLabel;
    rllAWB: TRLLabel;
    rlLabel154: TRLLabel;
    rlLabel155: TRLLabel;
    rllCaracAdServico: TRLLabel;
    rllCaracAdTransporte: TRLLabel;
    RLDraw57: TRLDraw;
    RLDraw72: TRLDraw;
    rlLabel156: TRLLabel;
    rllContaCorrente: TRLLabel;
    rlLabel157: TRLLabel;
    rllLojaAgenteEmissor: TRLLabel;
    rllRetira: TRLLabel;
    RLDraw70: TRLDraw;
    RLDraw71: TRLDraw;
    RLDraw73: TRLDraw;
    rlLabel151: TRLLabel;
    RLDraw74: TRLDraw;
    rlLabel152: TRLLabel;
    rllPortoEmbarque: TRLLabel;
    rlLabel158: TRLLabel;
    rllPortoDestino: TRLLabel;
    RLDraw75: TRLDraw;
    rlLabel159: TRLLabel;
    rllIndNavioRebocador: TRLLabel;
    RLDraw76: TRLDraw;
    rlLabel160: TRLLabel;
    rllIndConteiners: TRLLabel;
    RLDraw77: TRLDraw;
    rlLabel162: TRLLabel;
    rllBCAFRMM: TRLLabel;
    rlLabel164: TRLLabel;
    rllValorAFRMM: TRLLabel;
    rlLabel166: TRLLabel;
    rllTipoNav: TRLLabel;
    rlLabel168: TRLLabel;
    rllDirecao: TRLLabel;
    RLDraw78: TRLDraw;
    RLDraw79: TRLDraw;
    RLDraw80: TRLDraw;
    rlb_01_Recibo_Aereo: TRLBand;
    rlLabel19: TRLLabel;
    RLDraw81: TRLDraw;
    RLDraw82: TRLDraw;
    rlLabel57: TRLLabel;
    rlLabel60: TRLLabel;
    rlLabel69: TRLLabel;
    rlLabel161: TRLLabel;
    rlLabel67: TRLLabel;
    rlLabel68: TRLLabel;
    rlLabel72: TRLLabel;
    rlLabel163: TRLLabel;
    RLDraw3: TRLDraw;
    rlLabel165: TRLLabel;
    lblCIOT: TRLLabel;
    rllCIOT: TRLLabel;
    rlsCIOT: TRLDraw;
    rlmObsFisco: TRLMemo;
    rlb_11_ModRodLot104: TRLBand;
    RLDraw4: TRLDraw;
    RLDraw30: TRLDraw;
    RLDraw83: TRLDraw;
    RLDraw84: TRLDraw;
    RLDraw85: TRLDraw;
    RLDraw86: TRLDraw;
    RLDraw87: TRLDraw;
    RLDraw89: TRLDraw;
    RLDraw90: TRLDraw;
    RLDraw92: TRLDraw;
    rlLabel167: TRLLabel;
    rlLabel169: TRLLabel;
    rlLabel170: TRLLabel;
    rlLabel171: TRLLabel;
    rlLabel172: TRLLabel;
    rlLabel173: TRLLabel;
    rlLabel174: TRLLabel;
    rlLabel177: TRLLabel;
    rlLabel179: TRLLabel;
    rlLabel181: TRLLabel;
    rlLabel182: TRLLabel;
    rlLabel183: TRLLabel;
    rlmUF2: TRLMemo;
    rlmTipo2: TRLMemo;
    rlmRNTRC2: TRLMemo;
    rlmPlaca2: TRLMemo;
    rlmCNPJForn: TRLMemo;
    rlmNumCompra: TRLMemo;
    rllNomeMotorista2: TRLLabel;
    rllLacres2: TRLLabel;
    rllCPFMotorista2: TRLLabel;
    rlb_18_Recibo: TRLBand;
    RLDraw91: TRLDraw;
    RLDraw93: TRLDraw;
    RLDraw94: TRLDraw;
    RLDraw95: TRLDraw;
    RLDraw96: TRLDraw;
    RLDraw97: TRLDraw;
    rlLabel175: TRLLabel;
    rlLabel176: TRLLabel;
    rlLabel180: TRLLabel;
    rlLabel184: TRLLabel;
    rlLabel185: TRLLabel;
    rlLabel186: TRLLabel;
    rlLabel187: TRLLabel;
    rlLabel188: TRLLabel;
    rlLabel189: TRLLabel;
    rlLabel190: TRLLabel;
    rlLabel191: TRLLabel;
    rllSerie3: TRLLabel;
    rllNumCTe3: TRLLabel;
    RLDraw98: TRLDraw;
    rlmCNPJPg: TRLMemo;
    RLDraw88: TRLDraw;
    rllVariavel1: TRLLabel;
    RLDraw99: TRLDraw;
    rlmQtdUnidMedida4: TRLMemo;
    rlLabel73: TRLLabel;
    RLDraw100: TRLDraw;
    rlsQuadro03: TRLDraw;
    rlsLinhaPontilhada: TRLDraw;
    rlLabel178: TRLLabel;
    rllIndBalsas: TRLLabel;
    rlmNomeSeguradora: TRLMemo;
    rlmRespSeguroMerc: TRLMemo;
    rlmNroApolice: TRLMemo;
    rlmNroAverbacao: TRLMemo;
    rlb_06_ProdutosPerigosos: TRLBand;
    rlLabel192: TRLLabel;
    RLDraw102: TRLDraw;
    rlLabel193: TRLLabel;
    rlLabel194: TRLLabel;
    rlLabel195: TRLLabel;
    rlLabel196: TRLLabel;
    rlLabel197: TRLLabel;
    RLDraw103: TRLDraw;
    RLDraw104: TRLDraw;
    RLDraw105: TRLDraw;
    RLDraw106: TRLDraw;
    rlmNumONU: TRLMemo;
    rlmNomeApropriado: TRLMemo;
    rlmClasse: TRLMemo;
    rlmGrupoEmbalagem: TRLMemo;
    rlmQtdeProduto: TRLMemo;
    RLDraw107: TRLDraw;
    rllResumoCanhotoCTe: TRLLabel;
    rllResumoCanhotoCTe2: TRLLabel;
    rlbCodigoBarras: TRLBarcode;
    RLDraw51: TRLDraw;
    RLDraw52: TRLDraw;
    RLDraw50: TRLDraw;
    RLLabel198: TRLLabel;
    RLDraw108: TRLDraw;
    RLDraw109: TRLDraw;
    rlmComplChave1: TRLMemo;
    rlmComplValor1: TRLMemo;
    rlmComplChave2: TRLMemo;
    rlmComplValor2: TRLMemo;
    rlDocOrig_tpDoc1: TRLMemo;
    rlDocOrig_tpDoc2: TRLMemo;
    RLLabel199: TRLLabel;
    RLLabel200: TRLLabel;
    rlsQuadro10: TRLDraw;
    rlsQuadro10_Fluxo: TRLDraw;
    rlsQuadro3: TRLDraw;
    RLSystemInfo1: TRLSystemInfo;
    RLSystemInfo2: TRLSystemInfo;
    rlb_06_VeiculosNovos: TRLBand;
    RLDraw228: TRLDraw;
    RLLabel222: TRLLabel;
    RLDraw229: TRLDraw;
    RLLabel229: TRLLabel;
    RLLabel231: TRLLabel;
    RLLabel242: TRLLabel;
    RLLabel243: TRLLabel;
    RLLabel244: TRLLabel;
    RLDraw324: TRLDraw;
    RLDraw335: TRLDraw;
    RLDraw310: TRLDraw;
    RLDraw411: TRLDraw;
    CHASSI: TRLMemo;
    COR: TRLMemo;
    MODELO: TRLMemo;
    VUNIT: TRLMemo;
    VFRETE: TRLMemo;
    rlpnlTributosFederais: TRLPanel;
    RLDraw101: TRLDraw;
    RLDraw114: TRLDraw;
    RLDraw115: TRLDraw;
    RLDraw116: TRLDraw;
    RLLabel220: TRLLabel;
    rlblVlrCOFINS: TRLLabel;
    RLLabel226: TRLLabel;
    rlblVlrIR: TRLLabel;
    RLLabel228: TRLLabel;
    rlblVlrINSS: TRLLabel;
    RLLabel232: TRLLabel;
    rlblVlrCSLL: TRLLabel;
    RLLabel2: TRLLabel;
    rlblVlrPIS: TRLLabel;
    procedure rlb_01_ReciboBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlb_02_CabecalhoBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlb_03_DadosDACTeBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlb_03_DadosDACTe_OSBeforePrint(Sender: TObject;
      var PrintIt: Boolean);
    procedure rlb_04_DadosNotaFiscalBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlb_05_ComplementoBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlb_06_ValorPrestacaoBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlb_07_HeaderItensBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlb_09_ObsBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlb_10_ModRodFracionadoBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlb_11_ModRodLot103BeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlb_17_SistemaBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlb_16_DadosExcEmitenteBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlb_12_ModAereoBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlb_13_ModAquaviarioBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlb_14_ModFerroviarioBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlb_15_ModDutoviarioBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlb_01_Recibo_AereoBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlb_11_ModRodLot104BeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlb_18_ReciboBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlb_06_ProdutosPerigososBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlb_06_VeiculosNovosBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlb_CTeOS_PrestacaoServicoBeforePrint(Sender: TObject;
      var PrintIt: Boolean);
    procedure rlb_Dados_SeguradoraBeforePrint(Sender: TObject;
      var PrintIt: Boolean);
    procedure rlb_Fluxo_CargaBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure RLCTeBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlb_07_HeaderItensAfterPrint(Sender: TObject);
    procedure rlb_11_ModRodLot104AfterPrint(Sender: TObject);
    procedure cabecalhoVersao30();
    procedure tomadorMod67();
    procedure dadosNotaFiscalVersao30();
    procedure modalRodoviarioVersao30();
    procedure modalRodoviarioMod67();
    procedure fluxoCargaVersao30();
    procedure prestacaoServicoMod67();
    procedure dadosSeguradoraMod67();
  private
    Linhas : Integer;
    procedure Itens;
  public
    procedure ProtocoloCTe(const sProtocolo: string);
  end;

implementation

uses
  DateUtils, ACBrUtil, ACBrDFeUtil, ACBrValidador, pcteConversaoCTe, ACBrCTe;

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}


var
  FProtocoloCTe: string;
  Versao: integer;

procedure TfrmDACTeRLRetrato.Itens;
var
  I, J, K, Item: integer;
begin
  if RLCTe.PageNumber > 0 then
    exit;

  Item := 0;
{$IFDEF PL_200}
  //Varrendo NF comum
  for I := 0 to (FCTe.infCTeNorm.infDoc.infNF.Count - 1) do
  begin
    with FCTe.infCTeNorm.infDoc.InfNF.Items[I] do
    begin
      if (Item mod 2) = 0 then
      begin
        cdsDocumentos.Append;

        cdsDocumentos.FieldByname('TIPO_1').AsString := 'NF';
        cdsDocumentos.FieldByname('CNPJCPF_1').AsString := FormatarCNPJouCPF(FCTe.Rem.CNPJCPF);
        cdsDocumentos.FieldByname('DOCUMENTO_1').AsString := serie + '-' + nDoc;
      end
      else
      begin
        cdsDocumentos.FieldByname('TIPO_2').AsString := 'NF';
        cdsDocumentos.FieldByname('CNPJCPF_2').AsString := FormatarCNPJouCPF(FCTe.Rem.CNPJCPF);
        cdsDocumentos.FieldByname('DOCUMENTO_2').AsString := serie + '-' + nDoc;

        cdsDocumentos.Post;
      end;
      inc(Item);
    end;
  end;
  //Varrendo NFe
  for I := 0 to (FCTe.infCTeNorm.infDoc.InfNFE.Count - 1) do
  begin
    with FCTe.infCTeNorm.infDoc.InfNFE.Items[I] do
    begin
      if (Item mod 2) = 0 then
      begin
        cdsDocumentos.Append;
        cdsDocumentos.FieldByname('TIPO_1').AsString := 'NF-E ' + copy(chave, 26, 9);
        cdsDocumentos.FieldByname('CNPJCPF_1').AsString := FormatarChaveAcesso(chave);
      end
      else
      begin
        cdsDocumentos.FieldByname('TIPO_2').AsString := 'NF-E ' + copy(chave, 26, 9);
        cdsDocumentos.FieldByname('CNPJCPF_2').AsString := FormatarChaveAcesso(chave);
        cdsDocumentos.Post;
      end;
      inc(Item);
    end;
  end;
  //Varrendo Outros
  for I := 0 to (FCTe.infCTeNorm.infDoc.InfOutros.Count - 1) do
  begin
    with FCTe.infCTeNorm.infDoc.InfOutros.Items[I] do
    begin
      if (Item mod 2) = 0 then
      begin
        cdsDocumentos.Append;
        // TpcteTipoDocumento = (tdDeclaracao, tdDutoviario, tdCFeSAT, tdNFCe, tdOutros);
        case tpDoc of
         tdDeclaracao: begin
                        cdsDocumentos.FieldByname('TIPO_1').AsString      := 'DECLAR';
                        cdsDocumentos.FieldByname('CNPJCPF_1').AsString   := FormatarCNPJouCPF(FCTe.Rem.CNPJCPF);
                        cdsDocumentos.FieldByname('DOCUMENTO_1').AsString := ACBrStr( 'Declaração Doc.: ' ) + nDoc;
                       end;
         tdCFeSAT    : begin
                        cdsDocumentos.FieldByname('TIPO_1').AsString      := 'CF-e SAT ';
                        cdsDocumentos.FieldByname('CNPJCPF_1').AsString   := FormatarCNPJouCPF(FCTe.Rem.CNPJCPF);
                        cdsDocumentos.FieldByname('DOCUMENTO_1').AsString := ACBrStr( 'CF-e SAT.: ' ) + nDoc;
                       end;
         tdNFCe      : begin
                        cdsDocumentos.FieldByname('TIPO_1').AsString      := 'NFC-e ' + copy(trim(descOutros), 26, 9);
                        cdsDocumentos.FieldByname('CNPJCPF_1').AsString   := FormatarChaveAcesso(trim(descOutros));
                       end;
         tdDutoviario: begin
                        cdsDocumentos.FieldByname('TIPO_1').AsString      := 'DUTO';
                        cdsDocumentos.FieldByname('CNPJCPF_1').AsString   := FormatarCNPJouCPF(FCTe.Rem.CNPJCPF);
                        cdsDocumentos.FieldByname('DOCUMENTO_1').AsString := ACBrStr( 'Dutoviário Doc.: ' ) + nDoc;
                       end;
         tdOutros    : begin
                        cdsDocumentos.FieldByname('TIPO_1').AsString      := 'Outros';
                        cdsDocumentos.FieldByname('CNPJCPF_1').AsString   := FormatarCNPJouCPF(FCTe.Rem.CNPJCPF);
                        cdsDocumentos.FieldByname('DOCUMENTO_1').AsString := copy( trim(descOutros), 1, 20 ) + ' Doc.: '+ nDoc;
                       end;

        end;
//        cdsDocumentos.FieldByname('TIPO_1').AsString := descOutros;
//        cdsDocumentos.FieldByname('CNPJCPF_1').AsString := FormatarCNPJCPF(FCTe.Rem.CNPJCPF);
      end
      else
      begin
        // TpcteTipoDocumento = (tdDeclaracao, tdDutoviario, tdCFeSAT, tdNFCe, tdOutros);
        case tpDoc of
         tdDeclaracao: begin
                        cdsDocumentos.FieldByname('TIPO_2').AsString      := 'DECLAR';
                        cdsDocumentos.FieldByname('CNPJCPF_2').AsString   := FormatarCNPJouCPF(FCTe.Rem.CNPJCPF);
                        cdsDocumentos.FieldByname('DOCUMENTO_2').AsString := ACBrStr( 'Declaração Doc.: ' ) + nDoc;
                       end;
         tdDutoviario: begin
                        cdsDocumentos.FieldByname('TIPO_2').AsString      := 'DUTO';
                        cdsDocumentos.FieldByname('CNPJCPF_2').AsString   := FormatarCNPJouCPF(FCTe.Rem.CNPJCPF);
                        cdsDocumentos.FieldByname('DOCUMENTO_2').AsString := ACBrStr( 'Dutoviário Doc.: ' ) + nDoc;
                       end;
         tdCFeSAT    : begin
                        cdsDocumentos.FieldByname('TIPO_2').AsString      := 'CF-e SAT ';
                        cdsDocumentos.FieldByname('CNPJCPF_2').AsString   := FormatarCNPJouCPF(FCTe.Rem.CNPJCPF);
                        cdsDocumentos.FieldByname('DOCUMENTO_2').AsString := ACBrStr( 'CF-e SAT.: ' ) + nDoc;
                       end;
         tdNFCe      : begin
                        cdsDocumentos.FieldByname('TIPO_2').AsString      := 'NFC-E ' + copy(trim(descOutros), 26, 9);
                        cdsDocumentos.FieldByname('CNPJCPF_2').AsString   := FormatarChaveAcesso(trim(descOutros));
                       end;
         tdOutros    : begin
                        cdsDocumentos.FieldByname('TIPO_2').AsString      := 'Outros';
                        cdsDocumentos.FieldByname('CNPJCPF_2').AsString   := FormatarCNPJouCPF(FCTe.Rem.CNPJCPF);
                        cdsDocumentos.FieldByname('DOCUMENTO_2').AsString := copy( trim(descOutros), 1, 20 ) + ' Doc.: '+ nDoc;
                       end;
        end;
//        cdsDocumentos.FieldByname('TIPO_2').AsString := descOutros;
//        cdsDocumentos.FieldByname('CNPJCPF_2').AsString := FormatarCNPJCPF(FCTe.Rem.CNPJCPF);
        cdsDocumentos.Post;
      end;
      inc(Item);
    end;
  end;
 //Varrendo Documentos de Transporte anterior
  for I := 0 to (FCTe.infCTeNorm.docAnt.emiDocAnt.Count - 1) do
  begin
    // Em Papel
    for J := 0 to (FCTe.infCTeNorm.docAnt.emiDocAnt.Items[I].idDocAnt.Count - 1) do
    begin
      for K := 0 to (FCTe.infCTeNorm.docAnt.emiDocAnt.Items[I].idDocAnt.Items[J].idDocAntPap.Count - 1) do
      begin
        with FCTe.infCTeNorm.docAnt.emiDocAnt.Items[I].idDocAnt.Items[J].idDocAntPap.Items[K] do
        begin
          if (Item mod 2) = 0 then
          begin
            cdsDocumentos.Append;

            case tpDoc of
             daCTRC: cdsDocumentos.FieldByname('TIPO_1').AsString := 'CTRC';
             daCTAC: cdsDocumentos.FieldByname('TIPO_1').AsString := 'CTAC';
             daACT:  cdsDocumentos.FieldByname('TIPO_1').AsString := 'ACT';
             daNF7:  cdsDocumentos.FieldByname('TIPO_1').AsString := 'NF M7';
             daNF27: cdsDocumentos.FieldByname('TIPO_1').AsString := 'NF M27';
             daCAN:  cdsDocumentos.FieldByname('TIPO_1').AsString := 'CAN';
             daCTMC: cdsDocumentos.FieldByname('TIPO_1').AsString := 'CTMC';
             daATRE: cdsDocumentos.FieldByname('TIPO_1').AsString := 'ATRE';
             daDTA:  cdsDocumentos.FieldByname('TIPO_1').AsString := 'DTA';
             daCAI:  cdsDocumentos.FieldByname('TIPO_1').AsString := 'CAI';
             daCCPI: cdsDocumentos.FieldByname('TIPO_1').AsString := 'CCPI';
             daCA:   cdsDocumentos.FieldByname('TIPO_1').AsString := 'CA';
             daTIF:  cdsDocumentos.FieldByname('TIPO_1').AsString := 'TIF';
             daOutros: cdsDocumentos.FieldByname('TIPO_1').AsString := 'Outros';
            end;
            cdsDocumentos.FieldByname('CNPJCPF_1').AsString := FormatarCNPJouCPF(FCTe.infCTeNorm.docAnt.emiDocAnt.Items[I].CNPJCPF);
            cdsDocumentos.FieldByname('DOCUMENTO_1').AsString := serie + '-' + IntToStr(nDoc);
          end
          else
          begin
            case tpDoc of
             daCTRC: cdsDocumentos.FieldByname('TIPO_2').AsString := 'CTRC';
             daCTAC: cdsDocumentos.FieldByname('TIPO_2').AsString := 'CTAC';
             daACT:  cdsDocumentos.FieldByname('TIPO_2').AsString := 'ACT';
             daNF7:  cdsDocumentos.FieldByname('TIPO_2').AsString := 'NF M7';
             daNF27: cdsDocumentos.FieldByname('TIPO_2').AsString := 'NF M27';
             daCAN:  cdsDocumentos.FieldByname('TIPO_2').AsString := 'CAN';
             daCTMC: cdsDocumentos.FieldByname('TIPO_2').AsString := 'CTMC';
             daATRE: cdsDocumentos.FieldByname('TIPO_2').AsString := 'ATRE';
             daDTA:  cdsDocumentos.FieldByname('TIPO_2').AsString := 'DTA';
             daCAI:  cdsDocumentos.FieldByname('TIPO_2').AsString := 'CAI';
             daCCPI: cdsDocumentos.FieldByname('TIPO_2').AsString := 'CCPI';
             daCA:   cdsDocumentos.FieldByname('TIPO_2').AsString := 'CA';
             daTIF:  cdsDocumentos.FieldByname('TIPO_2').AsString := 'TIF';
             daOutros: cdsDocumentos.FieldByname('TIPO_2').AsString := 'Outros';
            end;
            cdsDocumentos.FieldByname('CNPJCPF_2').AsString := FormatarCNPJouCPF(FCTe.infCTeNorm.docAnt.emiDocAnt.Items[I].CNPJCPF);
            cdsDocumentos.FieldByname('DOCUMENTO_2').AsString := serie + '-' + IntToStr(nDoc);

            cdsDocumentos.Post;
          end;
          inc(Item);
        end;
      end;
    end;

    // Eletrônico
    for J := 0 to (FCTe.infCTeNorm.docAnt.emiDocAnt.Items[I].idDocAnt.Count - 1) do
    begin
      for K := 0 to (FCTe.infCTeNorm.docAnt.emiDocAnt.Items[I].idDocAnt.Items[J].idDocAntEle.Count - 1) do
      begin
        with FCTe.infCTeNorm.docAnt.emiDocAnt.Items[I].idDocAnt.Items[J].idDocAntEle.Items[K] do
        begin
          if (Item mod 2) = 0 then
          begin
            cdsDocumentos.Append;

            cdsDocumentos.FieldByname('TIPO_1').AsString := 'CT-E';
            if FCTe.infCTe.versao >= 3 then
              cdsDocumentos.FieldByname('CNPJCPF_1').AsString := FormatarChaveAcesso(chCTe)
            else
              cdsDocumentos.FieldByname('CNPJCPF_1').AsString := FormatarChaveAcesso(chave);
          end
          else
          begin
            cdsDocumentos.FieldByname('TIPO_2').AsString := 'CT-E';
            if FCTe.infCTe.versao >= 3 then
              cdsDocumentos.FieldByname('CNPJCPF_2').AsString := FormatarChaveAcesso(chCTe)
            else
              cdsDocumentos.FieldByname('CNPJCPF_2').AsString := FormatarChaveAcesso(chave);
            cdsDocumentos.Post;
          end;
          inc(Item);
        end;
      end;
    end;

  end;
{$ELSE}
  //Varrendo NF comum
  for I := 0 to (FCTe.Rem.InfNF.Count - 1) do
  begin
    with FCTe.Rem.InfNF.Items[I] do
    begin
      if (Item mod 2) = 0 then
      begin
        cdsDocumentos.Append;

        cdsDocumentos.FieldByName('TIPO_1').AsString := 'NF';
        cdsDocumentos.FieldByName('CNPJCPF_1').AsString := FormatarCNPJCPF(FCTe.Rem.CNPJCPF);
        cdsDocumentos.FieldByName('DOCUMENTO_1').AsString := serie + '-' + nDoc;
      end
      else
      begin
        cdsDocumentos.FieldByName('TIPO_2').AsString := 'NF';
        cdsDocumentos.FieldByName('CNPJCPF_2').AsString := FormatarCNPJCPF(FCTe.Rem.CNPJCPF);
        cdsDocumentos.FieldByName('DOCUMENTO_2').AsString := serie + '-' + nDoc;

        cdsDocumentos.Post;
      end;
      Inc(Item);
    end;
  end;
  //Varrendo NFe
  for I := 0 to (FCTe.Rem.InfNFE.Count - 1) do
  begin
    with FCTe.Rem.InfNFE.Items[I] do
    begin
      if (Item mod 2) = 0 then
      begin
        cdsDocumentos.Append;
        cdsDocumentos.FieldByName('TIPO_1').AsString := 'NF-E ' + copy(chave, 26, 9);
        cdsDocumentos.FieldByName('CNPJCPF_1').AsString := FormatarChaveAcesso(chave, True);
      end
      else
      begin
        cdsDocumentos.FieldByName('TIPO_2').AsString := 'NF-E ' + copy(chave, 26, 9);
        cdsDocumentos.FieldByName('CNPJCPF_2').AsString := FormatarChaveAcesso(chave, True);
        cdsDocumentos.Post;
      end;
      Inc(Item);
    end;
  end;
  //Varrendo Outros
  for I := 0 to (FCTe.Rem.InfOutros.Count - 1) do
  begin
    with FCTe.Rem.InfOutros.Items[I] do
    begin
      if (Item mod 2) = 0 then
      begin
        cdsDocumentos.Append;
        // TpcteTipoDocumento = (tdDeclaracao, tdDutoviario, tdOutros);
        case tpDoc of
          tdDeclaracao:
          begin
            cdsDocumentos.FieldByName('TIPO_1').AsString := 'DECLAR';
            cdsDocumentos.FieldByName('CNPJCPF_1').AsString := FormatarCNPJCPF(FCTe.Rem.CNPJCPF);
            cdsDocumentos.FieldByName('DOCUMENTO_1').AsString := 'Declaração Doc.: ' + nDoc;
          end;
          tdDutoviario:
          begin
            cdsDocumentos.FieldByName('TIPO_1').AsString := 'DUTO';
            cdsDocumentos.FieldByName('CNPJCPF_1').AsString := FormatarCNPJCPF(FCTe.Rem.CNPJCPF);
            cdsDocumentos.FieldByName('DOCUMENTO_1').AsString := 'Dutoviário Doc.: ' + nDoc;
          end;
          tdOutros:
          begin
            cdsDocumentos.FieldByName('TIPO_1').AsString := 'Outros';
            cdsDocumentos.FieldByName('CNPJCPF_1').AsString := FormatarCNPJCPF(FCTe.Rem.CNPJCPF);
            cdsDocumentos.FieldByName('DOCUMENTO_1').AsString :=
              copy(trim(descOutros), 1, 20) + ' Doc.: ' + nDoc;
          end;
        end;
        //        cdsDocumentos.FieldByname('TIPO_1').AsString := descOutros;
        //        cdsDocumentos.FieldByname('CNPJCPF_1').AsString := FormatarCNPJCPF(FCTe.Rem.CNPJCPF);
      end
      else
      begin
        // TpcteTipoDocumento = (tdDeclaracao, tdDutoviario, tdOutros);
        case tpDoc of
          tdDeclaracao:
          begin
            cdsDocumentos.FieldByName('TIPO_2').AsString := 'DECLAR';
            cdsDocumentos.FieldByName('CNPJCPF_2').AsString := FormatarCNPJCPF(FCTe.Rem.CNPJCPF);
            cdsDocumentos.FieldByName('DOCUMENTO_2').AsString := 'Declaração Doc.: ' + nDoc;
          end;
          tdDutoviario:
          begin
            cdsDocumentos.FieldByName('TIPO_2').AsString := 'DUTO';
            cdsDocumentos.FieldByName('CNPJCPF_2').AsString := FormatarCNPJCPF(FCTe.Rem.CNPJCPF);
            cdsDocumentos.FieldByName('DOCUMENTO_2').AsString := 'Dutoviário Doc.: ' + nDoc;
          end;
          tdOutros:
          begin
            cdsDocumentos.FieldByName('TIPO_2').AsString := 'Outros';
            cdsDocumentos.FieldByName('CNPJCPF_2').AsString := FormatarCNPJCPF(FCTe.Rem.CNPJCPF);
            cdsDocumentos.FieldByName('DOCUMENTO_2').AsString :=
              copy(trim(descOutros), 1, 20) + ' Doc.: ' + nDoc;
          end;
        end;
        //        cdsDocumentos.FieldByname('TIPO_2').AsString := descOutros;
        //        cdsDocumentos.FieldByname('CNPJCPF_2').AsString := FormatarCNPJCPF(FCTe.Rem.CNPJCPF);
        cdsDocumentos.Post;
      end;
      Inc(Item);
    end;
  end;
  //Varrendo Documentos de Transporte anterior
  for I := 0 to (FCTe.infCTeNorm.emiDocAnt.Count - 1) do
  begin
    // Em Papel
    for J := 0 to (FCTe.infCTeNorm.emiDocAnt.Items[I].idDocAnt.Count - 1) do
    begin
      for K := 0 to (FCTe.infCTeNorm.emiDocAnt.Items[I].idDocAnt.Items[J].idDocAntPap.Count - 1) do
      begin
        with FCTe.infCTeNorm.emiDocAnt.Items[I].idDocAnt.Items[J].idDocAntPap.Items[K] do
        begin
          if (Item mod 2) = 0 then
          begin
            cdsDocumentos.Append;

            case tpDoc of
              daCTRC: cdsDocumentos.FieldByName('TIPO_1').AsString := 'CTRC';
              daCTAC: cdsDocumentos.FieldByName('TIPO_1').AsString := 'CTAC';
              daACT: cdsDocumentos.FieldByName('TIPO_1').AsString := 'ACT';
              daNF7: cdsDocumentos.FieldByName('TIPO_1').AsString := 'NF M7';
              daNF27: cdsDocumentos.FieldByName('TIPO_1').AsString := 'NF M27';
              daCAN: cdsDocumentos.FieldByName('TIPO_1').AsString := 'CAN';
              daCTMC: cdsDocumentos.FieldByName('TIPO_1').AsString := 'CTMC';
              daATRE: cdsDocumentos.FieldByName('TIPO_1').AsString := 'ATRE';
              daDTA: cdsDocumentos.FieldByName('TIPO_1').AsString := 'DTA';
              daCAI: cdsDocumentos.FieldByName('TIPO_1').AsString := 'CAI';
              daCCPI: cdsDocumentos.FieldByName('TIPO_1').AsString := 'CCPI';
              daCA: cdsDocumentos.FieldByName('TIPO_1').AsString := 'CA';
              daTIF: cdsDocumentos.FieldByName('TIPO_1').AsString := 'TIF';
              daOutros: cdsDocumentos.FieldByName('TIPO_1').AsString := 'Outros';
            end;
            cdsDocumentos.FieldByName('CNPJCPF_1').AsString :=
              FormatarCNPJCPF(FCTe.infCTeNorm.emiDocAnt.Items[I].CNPJCPF);
            cdsDocumentos.FieldByName('DOCUMENTO_1').AsString := serie + '-' + IntToStr(nDoc);
          end
          else
          begin
            case tpDoc of
              daCTRC: cdsDocumentos.FieldByName('TIPO_2').AsString := 'CTRC';
              daCTAC: cdsDocumentos.FieldByName('TIPO_2').AsString := 'CTAC';
              daACT: cdsDocumentos.FieldByName('TIPO_2').AsString := 'ACT';
              daNF7: cdsDocumentos.FieldByName('TIPO_2').AsString := 'NF M7';
              daNF27: cdsDocumentos.FieldByName('TIPO_2').AsString := 'NF M27';
              daCAN: cdsDocumentos.FieldByName('TIPO_2').AsString := 'CAN';
              daCTMC: cdsDocumentos.FieldByName('TIPO_2').AsString := 'CTMC';
              daATRE: cdsDocumentos.FieldByName('TIPO_2').AsString := 'ATRE';
              daDTA: cdsDocumentos.FieldByName('TIPO_2').AsString := 'DTA';
              daCAI: cdsDocumentos.FieldByName('TIPO_2').AsString := 'CAI';
              daCCPI: cdsDocumentos.FieldByName('TIPO_2').AsString := 'CCPI';
              daCA: cdsDocumentos.FieldByName('TIPO_2').AsString := 'CA';
              daTIF: cdsDocumentos.FieldByName('TIPO_2').AsString := 'TIF';
              daOutros: cdsDocumentos.FieldByName('TIPO_2').AsString := 'Outros';
            end;
            cdsDocumentos.FieldByName('CNPJCPF_2').AsString :=
              FormatarCNPJCPF(FCTe.infCTeNorm.emiDocAnt.Items[I].CNPJCPF);
            cdsDocumentos.FieldByName('DOCUMENTO_2').AsString := serie + '-' + IntToStr(nDoc);

            cdsDocumentos.Post;
          end;
          Inc(Item);
        end;
      end;
    end;

    // Eletrônico
    for J := 0 to (FCTe.infCTeNorm.emiDocAnt.Items[I].idDocAnt.Count - 1) do
    begin
      for K := 0 to (FCTe.infCTeNorm.emiDocAnt.Items[I].idDocAnt.Items[J].idDocAntEle.Count - 1) do
      begin
        with FCTe.infCTeNorm.emiDocAnt.Items[I].idDocAnt.Items[J].idDocAntEle.Items[K] do
        begin
          if (Item mod 2) = 0 then
          begin
            cdsDocumentos.Append;

            cdsDocumentos.FieldByName('TIPO_1').AsString := 'CT-E';
            cdsDocumentos.FieldByName('CNPJCPF_1').AsString := FormatarChaveAcesso(chave, True);
          end
          else
          begin
            cdsDocumentos.FieldByName('TIPO_2').AsString := 'CT-E';
            cdsDocumentos.FieldByName('CNPJCPF_2').AsString := FormatarChaveAcesso(chave, True);

            cdsDocumentos.Post;
          end;
          Inc(Item);
        end;
      end;
    end;
  end;
{$ENDIF}

  cdsDocumentos.First;
end;

procedure TfrmDACTeRLRetrato.ProtocoloCTe(const sProtocolo: string);
begin
  FProtocoloCTe := sProtocolo;
end;

procedure TfrmDACTeRLRetrato.rlb_01_ReciboBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  inherited;
  PrintIt := (RLCTe.PageNumber = 1) and (FCTe.Ide.modal <> mdAereo) and (FPosRecibo = prCabecalho);
  if (FResumoCanhoto) then
    rllResumoCanhotoCTe.Caption := getTextoResumoCanhoto
  else
    rllResumoCanhotoCTe.Caption := '';

  rllSerie2.Caption := IntToStr(FCTe.Ide.serie); // FormatFloat( '000', FCTe.Ide.serie);
  rllNumCte2.Caption := FormatFloat('000,000,000', FCTe.Ide.nCT);
  // TpcteTipoCTe = (tcNormal, tcComplemento, tcAnulacao, tcSubstituto);
  rlb_01_Recibo.Enabled := (FCTe.Ide.tpCTe = tcNormal) or (FCTe.Ide.tpCTe = tcComplemento);

  if (FCTe.ide.modelo = 67) then
    rlLabel140.Caption := 'CT-E OS';
end;

procedure TfrmDACTeRLRetrato.rlb_01_Recibo_AereoBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  inherited;
  PrintIt := (RLCTe.PageNumber = 1) and (FCTe.Ide.modal = mdAereo);

  rlb_01_Recibo_Aereo.Enabled := (FCTe.Ide.tpCTe = tcNormal) or (FCTe.Ide.tpCTe = tcComplemento);
end;

procedure TfrmDACTeRLRetrato.rlb_02_CabecalhoBeforePrint(Sender: TObject; var PrintIt: Boolean);
var
  strChaveContingencia: string;
  vStringStream: TStringStream;
begin
  inherited;

  if (FLogo <> '') then
  begin
    if FilesExists(FLogo) then
      rliLogo.Picture.LoadFromFile(FLogo)
    else
    begin
      vStringStream := TStringStream.Create(FLogo);
      try
        try
          rliLogo.Picture.Bitmap.LoadFromStream(vStringStream);
        except
        end;
      finally
        vStringStream.Free;
      end;
    end;
  end
  else
  begin
    rlmDadosEmitente.Left  := 7;
    rlmDadosEmitente.Width := 321;
    rlmDadosEmitente.Alignment := taCenter;
  end;

  if FExpandirLogoMarca then
  begin
    rlmEmitente.visible      := False;
    rlmDadosEmitente.visible := False;
    rliLogo.Stretch := True;
    rliLogo.top     := 5;
    rliLogo.Left    := 3;
    rliLogo.Height  := 115;//91;
    rliLogo.Width   := 324;//321
  end;

  rllModal.Caption   := ACBrStr( TpModalToStrText(FCTe.Ide.modal) );
  rllModelo.Caption  := IntToStr(FCTe.Ide.modelo);
  rllSerie.Caption   := IntToStr(FCTe.Ide.serie);
  rllNumCte.Caption  := FormatFloat('000,000,000', FCTe.Ide.nCT);
//  rllPageNumber.Caption   := format('%2.2d', [RLCTe.PageNumber]) + '/' + format('%2.2d', [FTotalPages]);
//  rllPageNumber.Caption   := format('%2.2d', [RLCTe.PageNumber]) + '/' + format('%2.2d', [RLCTe.rlPrinter.PageCount]);
  rllEmissao.Caption      := FormatDateTimeBr(FCTe.Ide.dhEmi);
  rlbCodigoBarras.Caption := OnlyNumber(FCTe.InfCTe.Id);
  rllChave.Caption        := FormatarChaveAcesso(OnlyNumber(FCTe.InfCTe.Id));

  if not FExpandirLogoMarca then
  begin
    rlmEmitente.Enabled := True;
    rlmDadosEmitente.Enabled := True;
    // Emitente
    with FCTe.Emit do
    begin
      rlmEmitente.Lines.Text := XNome;

      rlmDadosEmitente.Lines.Clear;
      with EnderEmit do
      begin
        rlmDadosEmitente.Lines.Add(XLgr + IfThen(Nro = '0', '', ', ' + Nro));
        if XCpl <> '' then
          rlmDadosEmitente.Lines.Add(XCpl);
        if XBairro <> '' then
          rlmDadosEmitente.Lines.Add(XBairro);
        rlmDadosEmitente.Lines.Add('CEP: ' + FormatarCEP(CEP) +
          ' - ' + XMun + ' - ' + UF);
      end;
      rlmDadosEmitente.Lines.Add('CNPJ: ' + FormatarCNPJ(CNPJ));
      rlmDadosEmitente.Lines.Add(ACBrStr('INSCRIÇÃO ESTADUAL: ') + IE);
      rlmDadosEmitente.Lines.Add('TELEFONE: ' + FormatarFone(EnderEmit.Fone));

      if Trim(FUrl) <> '' then
        rlmDadosEmitente.Lines.Add('SITE: ' + FUrl);
      if Trim(FEmail) <> '' then
        rlmDadosEmitente.Lines.Add('E-MAIL: ' + FEmail);
    end;
  end;

  rllTipoCte.Caption     := tpCTToStrText(FCTe.Ide.tpCTe);
  rllTipoServico.Caption := TpServToStrText(FCTe.Ide.tpServ);

  // CTe 3.0 **************************************************************

  if (FCTe.infCTe.versao >= 3.00) then   //Campos referente a versão 3.00
    cabecalhoVersao30()
  else                                   //Campos referente a versão 2.00
  begin
    if FCTe.Ide.Toma4.xNome = '' then
      rllTomaServico.Caption   := TpTomadorToStrText(FCTe.Ide.Toma03.Toma)
    else
      rllTomaServico.Caption   := TpTomadorToStrText(FCTe.Ide.Toma4.toma);

    rllFormaPagamento.Caption  := tpforPagToStrText(FCTe.Ide.forPag);

  end;

  // Normal **************************************************************
  if FCTe.Ide.tpEmis in [teNormal, teSCAN, teSVCSP, teSVCRS] then
  begin
    rllVariavel1.Enabled := True;
    RLBarcode1.Enabled := False;
    if FCTe.procCTe.cStat = 100 then
      rllDescricao.Caption := ACBrStr('PROTOCOLO DE AUTORIZAÇÃO DE USO');

    if FCTe.procCTe.cStat = 101 then
      rllDescricao.Caption := ACBrStr('PROTOCOLO DE HOMOLOGAÇÃO DE CANCELAMENTO');

    if FCTe.procCTe.cStat = 110 then
      rllDescricao.Caption := ACBrStr('PROTOCOLO DE DENEGAÇÃO DE USO');

    if FProtocoloCTE <> '' then
      rllProtocolo.Caption := FProtocoloCTE
    else
      rllProtocolo.Caption := FCTe.procCTe.nProt + '   ' +
        IfThen(FCTe.procCTe.dhRecbto <> 0,
        DateTimeToStr(FCTe.procCTe.dhRecbto), '');
  end;

  // Contingencia ********************************************************
  if FCTe.Ide.tpEmis in [teContingencia, teFSDA] then
  begin
    if FCTe.procCTe.cStat in [100, 101, 110] then
    begin
      rllVariavel1.Enabled := True;
      RLBarcode1.Enabled := False;
      if FCTe.procCTe.cStat = 100 then
        rllDescricao.Caption := ACBrStr('PROTOCOLO DE AUTORIZAÇÃO DE USO');

      if FCTe.procCTe.cStat = 101 then
        rllDescricao.Caption := ACBrStr('PROTOCOLO DE HOMOLOGAÇÃO DE CANCELAMENTO');

      if FCTe.procCTe.cStat = 110 then
        rllDescricao.Caption := ACBrStr('PROTOCOLO DE DENEGAÇÃO DE USO');

      if FProtocoloCTE <> '' then
        rllProtocolo.Caption := FProtocoloCTE
      else
        rllProtocolo.Caption := FCTe.procCTe.nProt + '   ' +
          IfThen(FCTe.procCTe.dhRecbto <> 0,
          DateTimeToStr(FCTe.procCTe.dhRecbto), '');
    end
    else
    begin
      rllVariavel1.Enabled := False;
      RLBarcode1.Enabled := True;

      strChaveContingencia := FACBrCTe.GerarChaveContingencia(FCTe);
      RLBarcode1.Caption := strChaveContingencia;
      rllDescricao.Caption := 'DADOS DO CT-E';
      rllProtocolo.Caption := FormatarChaveAcesso(strChaveContingencia);
    end;
  end;

  // EPEC ****************************************************************
  if FCTe.Ide.tpEmis = teDPEC then
  begin
    rllVariavel1.Enabled := False;
    RLBarcode1.Enabled := True;

    strChaveContingencia := FACBrCTe.GerarChaveContingencia(FCTe);
    RLBarcode1.Caption := strChaveContingencia;
    rllDescricao.Caption := 'DADOS DO CT-E';
    rllProtocolo.Caption := FormatarChaveAcesso(strChaveContingencia);
  end;

  rllInscSuframa.Caption := FCTe.Dest.ISUF;
end;

procedure TfrmDACTeRLRetrato.rlb_03_DadosDACTeBeforePrint(Sender: TObject; var PrintIt: Boolean);
var
  i: integer;
begin
  inherited;
  rlb_03_DadosDACTe.Enabled := not(FCTe.ide.modelo = 67);
  if not (rlb_03_DadosDACTe.Enabled) then
    rlb_03_DadosDACTe.Height := 0;

  PrintIt := RLCTe.PageNumber = 1;

  rllNatOperacao.Caption := FormatFloat('0000', FCTe.Ide.CFOP) + ' - ' + FCTe.Ide.natOp;
  rllOrigPrestacao.Caption := FCTe.Ide.xMunIni + ' - ' + FCTe.Ide.UFIni + ' - ' + FormatFloat('000', FCTe.Ide.cMunIni);
  rllDestPrestacao.Caption := FCTe.Ide.xMunFim + ' - ' + FCTe.Ide.UFFim + ' - ' + FormatFloat('000', FCTe.Ide.cMunFim);

  //DADOS REMETENTE
  rllRazaoRemet.Caption := FCTe.Rem.xNome;
  rllEnderecoRemet1.Caption := FCTe.Rem.EnderReme.xLgr + ', ' + FCTe.Rem.EnderReme.nro;
  rllEnderecoRemet2.Caption := FCTe.Rem.EnderReme.xCpl + ' - ' + FCTe.Rem.EnderReme.xBairro;
  rllCEPRemet.Caption := FormatarCEP(FCTe.Rem.EnderReme.CEP);
  rllMunRemet.Caption := FCTe.Rem.EnderReme.xMun + ' - ' + FCTe.Rem.EnderReme.UF;
  rllCnpjRemet.Caption := FormatarCNPJouCPF(FCTe.Rem.CNPJCPF);
  rllPaisRemet.Caption := FCTe.Rem.EnderReme.xPais;
  rllInscEstRemet.Caption := FCTe.Rem.IE;
  rllFoneRemet.Caption := FormatarFone(FCTe.Rem.fone);

  //DADOS DESTINATARIO
  rllRazaoDest.Caption := FCTe.Dest.xNome;
  rllEnderecoDest1.Caption := FCTe.Dest.EnderDest.xLgr + ', ' + FCTe.Dest.EnderDest.nro;
  rllEnderecoDest2.Caption := FCTe.Dest.EnderDest.xCpl + ' - ' + FCTe.Dest.EnderDest.xBairro;
  rllCEPDest.Caption := FormatarCEP(FCTe.Dest.EnderDest.CEP);
  rllMunDest.Caption := FCTe.Dest.EnderDest.xMun + ' - ' + FCTe.Dest.EnderDest.UF;
  rllCnpjDest.Caption := FormatarCNPJouCPF(FCTe.Dest.CNPJCPF);
  rllPaisDest.Caption := FCTe.Dest.EnderDest.xPais;
  rllInscEstDest.Caption := FCTe.Dest.IE;
  rllFoneDest.Caption := FormatarFone(FCTe.Dest.fone);

  //DADOS EXPEDIDOR
  if FCTe.Exped.xNome <> '' then
  begin
    rllRazaoExped.Caption := FCTe.Exped.xNome;
    rllEnderecoExped1.Caption := FCTe.Exped.EnderExped.xLgr + ', ' + FCTe.Exped.EnderExped.nro;
    rllEnderecoExped2.Caption := FCTe.Exped.EnderExped.xCpl + ' - ' + FCTe.Exped.EnderExped.xBairro;
    rllCEPExped.Caption := FormatarCEP(FCTe.Exped.EnderExped.CEP);
    rllMunExped.Caption := FCTe.Exped.EnderExped.xMun + ' - ' + FCTe.Exped.EnderExped.UF;
    rllCnpjExped.Caption := FormatarCNPJouCPF(FCTe.Exped.CNPJCPF);
    rllPaisExped.Caption := FCTe.Exped.EnderExped.xPais;
    rllInscEstExped.Caption := FCTe.Exped.IE;
    rllFoneExped.Caption := FormatarFone(FCTe.Exped.fone);
  end;

  //DADOS RECEBEDOR
  if FCTe.Receb.xNome <> '' then
  begin
    rllRazaoReceb.Caption := FCTe.Receb.xNome;
    rllEnderecoReceb1.Caption := FCTe.Receb.EnderReceb.xLgr + ', ' + FCTe.Receb.EnderReceb.nro;
    rllEnderecoReceb2.Caption := FCTe.Receb.EnderReceb.xCpl + ' - ' + FCTe.Receb.EnderReceb.xBairro;
    rllCEPReceb.Caption := FormatarCEP(FCTe.Receb.EnderReceb.CEP);
    rllMunReceb.Caption := FCTe.Receb.EnderReceb.xMun + ' - ' + FCTe.Receb.EnderReceb.UF;
    rllCnpjReceb.Caption := FormatarCNPJouCPF(FCTe.Receb.CNPJCPF);
    rllPaisReceb.Caption := FCTe.Receb.EnderReceb.xPais;
    rllInscEstReceb.Caption := FCTe.Receb.IE;
    rllFoneReceb.Caption := FormatarFone(FCTe.Receb.fone);
  end;

  if FCTe.Ide.Toma4.xNome = '' then
  begin
    case FCTe.Ide.Toma03.Toma of
      tmRemetente:
      begin
        rllRazaoToma.Caption := FCTe.Rem.xNome;
        rllEnderecoToma.Caption := FCTe.Rem.EnderReme.xLgr + ', ' + FCTe.Rem.EnderReme.nro +
          ' - ' + FCTe.Rem.EnderReme.xCpl + ' - ' + FCTe.Rem.EnderReme.xBairro;
        rllCEPToma.Caption := FormatarCEP(FCTe.Rem.EnderReme.CEP);
        rllMunToma.Caption := FCTe.Rem.EnderReme.xMun + ' - ' + FCTe.Rem.EnderReme.UF;
        rllCnpjToma.Caption := FormatarCNPJouCPF(FCTe.Rem.CNPJCPF);
        rllPaisToma.Caption := FCTe.Rem.EnderReme.xPais;
        rllInscEstToma.Caption := FCTe.Rem.IE;
        rllFoneToma.Caption := FormatarFone(FCTe.Rem.fone);
      end;
      tmExpedidor:
      begin
        rllRazaoToma.Caption := FCTe.Exped.xNome;
        rllEnderecoToma.Caption := FCTe.Exped.EnderExped.xLgr + ', ' + FCTe.Exped.EnderExped.nro +
          ' - ' + FCTe.Exped.EnderExped.xCpl + ' - ' + FCTe.Exped.EnderExped.xBairro;
        rllCEPToma.Caption := FormatarCEP(FCTe.Exped.EnderExped.CEP);
        rllMunToma.Caption := FCTe.Exped.EnderExped.xMun + ' - ' + FCTe.Exped.EnderExped.UF;
        rllCnpjToma.Caption := FormatarCNPJouCPF(FCTe.Exped.CNPJCPF);
        rllPaisToma.Caption := FCTe.Exped.EnderExped.xPais;
        rllInscEstToma.Caption := FCTe.Exped.IE;
        rllFoneToma.Caption := FormatarFone(FCTe.Exped.fone);
      end;
      tmRecebedor:
      begin
        rllRazaoToma.Caption := FCTe.Receb.xNome;
        rllEnderecoToma.Caption := FCTe.Receb.EnderReceb.xLgr + ', ' + FCTe.Receb.EnderReceb.nro +
          ' - ' + FCTe.Receb.EnderReceb.xCpl + ' - ' + FCTe.Receb.EnderReceb.xBairro;
        rllCEPToma.Caption := FormatarCEP(FCTe.Receb.EnderReceb.CEP);
        rllMunToma.Caption := FCTe.Receb.EnderReceb.xMun + ' - ' + FCTe.Receb.EnderReceb.UF;
        rllCnpjToma.Caption := FormatarCNPJouCPF(FCTe.Receb.CNPJCPF);
        rllPaisToma.Caption := FCTe.Receb.EnderReceb.xPais;
        rllInscEstToma.Caption := FCTe.Receb.IE;
        rllFoneToma.Caption := FormatarFone(FCTe.Receb.fone);
      end;
      tmDestinatario:
      begin
        rllRazaoToma.Caption := FCTe.Dest.xNome;
        rllEnderecoToma.Caption := FCTe.Dest.EnderDest.xLgr + ', ' + FCTe.Dest.EnderDest.nro +
          ' - ' + FCTe.Dest.EnderDest.xCpl + ' - ' + FCTe.Dest.EnderDest.xBairro;
        rllCEPToma.Caption := FormatarCEP(FCTe.Dest.EnderDest.CEP);
        rllMunToma.Caption := FCTe.Dest.EnderDest.xMun + ' - ' + FCTe.Dest.EnderDest.UF;
        rllCnpjToma.Caption := FormatarCNPJouCPF(FCTe.Dest.CNPJCPF);
        rllPaisToma.Caption := FCTe.Dest.EnderDest.xPais;
        rllInscEstToma.Caption := FCTe.Dest.IE;
        rllFoneToma.Caption := FormatarFone(FCTe.Dest.fone);
      end;
    end;
  end
  else
  begin
    if FCTe.Ide.Toma4.xNome <> '' then
    begin
      rllRazaoToma.Caption := FCTe.Ide.Toma4.xNome;
      rllEnderecoToma.Caption := FCTe.Ide.Toma4.EnderToma.xLgr + ', ' + FCTe.Ide.Toma4.EnderToma.nro +
        ' - ' + FCTe.Ide.Toma4.EnderToma.xCpl + ' - ' + FCTe.Ide.Toma4.EnderToma.xBairro;
      rllCEPToma.Caption := FormatarCEP(FCTe.Ide.Toma4.EnderToma.CEP);
      rllMunToma.Caption := FCTe.Ide.Toma4.EnderToma.xMun + ' - ' + FCTe.Ide.Toma4.EnderToma.UF;
      rllCnpjToma.Caption := FormatarCNPJouCPF(FCTe.Ide.Toma4.CNPJCPF);
      rllPaisToma.Caption := FCTe.Ide.Toma4.EnderToma.xPais;
      rllInscEstToma.Caption := FCTe.Ide.Toma4.IE;
      rllFoneToma.Caption := FormatarFone(FCTe.Ide.Toma4.fone);
    end;
  end;

{$IFDEF PL_200}
  rllProdPredominante.Caption := FCTe.infCTeNorm.infCarga.proPred;
  rllOutrasCaracCarga.Caption := FCTe.infCTeNorm.InfCarga.xOutCat;
{$ELSE}
  rllProdPredominante.Caption := FCTe.InfCarga.proPred;
  rllOutrasCaracCarga.Caption := FCTe.InfCarga.xOutCat;
{$ENDIF}
{$IFDEF PL_103}
  rllVlrTotalMerc.Caption := FormatFloatBr(msk15x2, FCTe.InfCarga.vMerc);
{$ENDIF}
{$IFDEF PL_104}
  rllVlrTotalMerc.Caption := FormatFloatBr(msk15x2, FCTe.InfCarga.vCarga);
{$ENDIF}
{$IFDEF PL_200}
  rllVlrTotalMerc.Caption := FormatFloatBr(msk15x2, FCTe.infCTeNorm.infCarga.vCarga);
{$ENDIF}

  rlmQtdUnidMedida1.Lines.Clear;
  rlmQtdUnidMedida2.Lines.Clear;
  rlmQtdUnidMedida3.Lines.Clear;
  rlmQtdUnidMedida4.Lines.Clear;
  rlmQtdUnidMedida5.Lines.Clear;

  rlmNomeSeguradora.Lines.Clear;
  rlmRespSeguroMerc.Lines.Clear;
  rlmNroApolice.Lines.Clear;
  rlMNroAverbacao.Lines.Clear;

  rlmCompNome1.Lines.Clear;
  rlmCompNome2.Lines.Clear;
  rlmCompNome3.Lines.Clear;
  rlmCompValor1.Lines.Clear;
  rlmCompValor2.Lines.Clear;
  rlmCompValor3.Lines.Clear;

{$IFDEF PL_200}
  for i := 0 to (FCTe.infCTeNorm.InfCarga.InfQ.Count - 1) do
   begin
    //UnidMed = (uM3,uKG, uTON, uUNIDADE, uLITROS, uMMBTU);
    case FCTe.infCTeNorm.InfCarga.InfQ.Items[i].cUnid of
          uM3: rlmQtdUnidMedida4.Lines.Add(FormatFloatBr(msk6x4,
                 FCTe.infCTeNorm.InfCarga.InfQ.Items[i].qCarga));
          uKg: begin
                if uppercase(trim(FCTe.infCTeNorm.InfCarga.InfQ.Items[i].tpMed))='PESO BRUTO'
                then rlmQtdUnidMedida1.Lines.Add(FormatFloatBr(msk6x4,
                        FCTe.infCTeNorm.InfCarga.InfQ.Items[i].qCarga))
                else
                if uppercase(trim(FCTe.infCTeNorm.InfCarga.InfQ.Items[i].tpMed))='PESO BASE DE CALCULO'
                then rlmQtdUnidMedida2.Lines.Add(FormatFloatBr(msk6x4,
                        FCTe.infCTeNorm.InfCarga.InfQ.Items[i].qCarga))
                else
                if uppercase(trim(FCTe.infCTeNorm.InfCarga.InfQ.Items[i].tpMed))='PESO BC'
                then rlmQtdUnidMedida2.Lines.Add(FormatFloatBr(msk6x4,
                        FCTe.infCTeNorm.InfCarga.InfQ.Items[i].qCarga))
                else rlmQtdUnidMedida3.Lines.Add(FormatFloatBr(msk6x4,
                        FCTe.infCTeNorm.InfCarga.InfQ.Items[i].qCarga));
               end;
         uTON: begin
                if uppercase(trim(FCTe.infCTeNorm.InfCarga.InfQ.Items[i].tpMed))='PESO BRUTO'
                then rlmQtdUnidMedida1.Lines.Add(FormatFloatBr(msk6x4,
                        FCTe.infCTeNorm.InfCarga.InfQ.Items[i].qCarga * 1000))
                else
                if uppercase(trim(FCTe.infCTeNorm.InfCarga.InfQ.Items[i].tpMed))='PESO BASE DE CALCULO'
                then rlmQtdUnidMedida2.Lines.Add(FormatFloatBr(msk6x4,
                        FCTe.infCTeNorm.InfCarga.InfQ.Items[i].qCarga * 1000))
                else
                if uppercase(trim(FCTe.infCTeNorm.InfCarga.InfQ.Items[i].tpMed))='PESO BC'
                then rlmQtdUnidMedida2.Lines.Add(FormatFloatBr(msk6x4,
                        FCTe.infCTeNorm.InfCarga.InfQ.Items[i].qCarga * 1000))
                else rlmQtdUnidMedida3.Lines.Add(FormatFloatBr(msk6x4,
                        FCTe.infCTeNorm.InfCarga.InfQ.Items[i].qCarga * 1000));
               end;
     uUNIDADE, uLITROS, uMMBTU:
       begin
               rlmQtdUnidMedida5.Lines.Add(FCTe.infCTeNorm.InfCarga.InfQ.Items[i].tpMed);
               rlmQtdUnidMedida5.Lines.Add(FormatFloatBr(msk6x4,FCTe.infCTeNorm.InfCarga.InfQ.Items[i].qCarga)+' '+
               UnidMedToDescricaoStr(FCTe.infCTeNorm.InfCarga.InfQ.Items[i].cUnid));
       end;
    end;
   end;

  if FCTe.infCTeNorm.seg.Count > 0 then
  begin
    for i := 0 to FCTe.infCTeNorm.seg.Count-1 do
     begin
      rlmNomeSeguradora.Lines.Add(FCTe.infCTeNorm.seg.Items[i].xSeg);
      rlmRespSeguroMerc.Lines.Add(TpRspSeguroToStrText(FCTe.infCTeNorm.seg.Items[i].respSeg));
      rlmNroApolice.Lines.Add(FCTe.infCTeNorm.seg.Items[i].nApol);
      rlmNroAverbacao.Lines.Add(FCTe.infCTeNorm.seg.Items[i].nAver);
     end;
  end;

  for i := 0 to (FCTe.vPrest.comp.Count - 1) do
  begin
    case i of
      0,3,6,9:
        begin
          rlmCompNome1.Lines.Add(FCTe.vPrest.comp[i].xNome);
          rlmCompValor1.Lines.Add(FormatFloatBr(msk10x2, FCTe.vPrest.comp[i].vComp));
        end;
      1,4,7,10:
        begin
          rlmCompNome2.Lines.Add(FCTe.vPrest.comp[i].xNome);
          rlmCompValor2.Lines.Add(FormatFloatBr(msk10x2, FCTe.vPrest.comp[i].vComp));
        end;
      2,5,8,11:
        begin
          rlmCompNome3.Lines.Add(FCTe.vPrest.comp[i].xNome);
          rlmCompValor3.Lines.Add(FormatFloatBr(msk10x2, FCTe.vPrest.comp[i].vComp));
        end;
    end;
  end;

  rllVlrTotServico.Caption := FormatFloatBr(msk13x2, FCTe.vPrest.vTPrest);
  rllVlrTotReceber.Caption := FormatFloatBr(msk13x2, FCTe.vPrest.vRec);

  rllSitTrib.Caption := ACBrStr(CSTICMSToStrTagPosText(FCTe.Imp.ICMS.SituTrib));
{$ELSE}
  for i := 0 to (FCTe.InfCarga.InfQ.Count - 1) do
  begin
    //UnidMed = (uM3,uKG, uTON, uUNIDADE, uLITROS, uMMBTU);
    case FCTe.InfCarga.InfQ.Items[i].cUnid of
      uM3: rlmQtdUnidMedida4.Lines.Add(FormatFloatBr(msk6x4,
          FCTe.InfCarga.InfQ.Items[i].qCarga));
      uKg:
      begin
        if uppercase(trim(FCTe.InfCarga.InfQ.Items[i].tpMed)) = 'PESO BRUTO' then
          rlmQtdUnidMedida1.Lines.Add(FormatFloatBr(msk6x4,
            FCTe.InfCarga.InfQ.Items[i].qCarga))
        else
        if uppercase(trim(FCTe.InfCarga.InfQ.Items[i].tpMed)) = 'PESO BASE DE CALCULO' then
          rlmQtdUnidMedida2.Lines.Add(FormatFloatBr(msk6x4,
            FCTe.InfCarga.InfQ.Items[i].qCarga))
        else
        if uppercase(trim(FCTe.InfCarga.InfQ.Items[i].tpMed)) = 'PESO BC' then
          rlmQtdUnidMedida2.Lines.Add(FormatFloatBr(msk6x4,
            FCTe.InfCarga.InfQ.Items[i].qCarga))
        else
          rlmQtdUnidMedida3.Lines.Add(FormatFloatBr(msk6x4,
            FCTe.InfCarga.InfQ.Items[i].qCarga));
      end;
      uTON:
      begin
        if uppercase(trim(FCTe.InfCarga.InfQ.Items[i].tpMed)) = 'PESO BRUTO' then
          rlmQtdUnidMedida1.Lines.Add(FormatFloatBr(msk6x4,
            FCTe.InfCarga.InfQ.Items[i].qCarga * 1000))
        else
        if uppercase(trim(FCTe.InfCarga.InfQ.Items[i].tpMed)) = 'PESO BASE DE CALCULO' then
          rlmQtdUnidMedida2.Lines.Add(FormatFloatBr(msk6x4,
            FCTe.InfCarga.InfQ.Items[i].qCarga * 1000))
        else
        if uppercase(trim(FCTe.InfCarga.InfQ.Items[i].tpMed)) = 'PESO BC' then
          rlmQtdUnidMedida2.Lines.Add(FormatFloatBr(msk6x4,
            FCTe.InfCarga.InfQ.Items[i].qCarga * 1000))
        else
          rlmQtdUnidMedida3.Lines.Add(FormatFloatBr(msk6x4,
            FCTe.InfCarga.InfQ.Items[i].qCarga * 1000));
      end;
      uUNIDADE, uLITROS, uMMBTU:
        begin
          rlmQtdUnidMedida5.Lines.Add(FCTe.infCTeNorm.InfCarga.InfQ.Items[i].tpMed);
          rlmQtdUnidMedida5.Lines.Add(FormatFloatBr(msk6x4,FCTe.infCTeNorm.InfCarga.InfQ.Items[i].qCarga)+' '+
          UnidMedToDescricaoStr(FCTe.infCTeNorm.InfCarga.InfQ.Items[i].cUnid));
        end;
    end;
  end;

  if FCTe.InfSeg.Count > 0 then
  begin
    for i := 0 to FCTe.infSeg.Count - 1 do
    begin
      rlmNomeSeguradora.Lines.Add(FCTe.infSeg.Items[i].xSeg);
      rlmRespSeguroMerc.Lines.Add(TpRspSeguroToStrText(FCTe.infSeg.Items[i].respSeg));
      rlmNroApolice.Lines.Add(FCTe.infSeg.Items[i].nApol);
      rlmNroAverbacao.Lines.Add(FCTe.infSeg.Items[i].nAver);
    end;
    //    rllNomeSeguradora.Caption := FCTe.InfSeg.Items[0].xSeg;
    //    rllRespSeguroMerc.Caption := TpRspSeguroToStrText(FCTe.InfSeg.Items[0].respSeg);
    //    rllNroApolice.Caption := FCTe.InfSeg.Items[0].nApol;
    //    rllNroAverbacao.Caption := FCTe.InfSeg.Items[0].nAver;
  end;

  for i := 0 to (FCTe.vPrest.comp.Count - 1) do
  begin
    case i of
      0, 3, 6, 9:
      begin
        rlmCompNome1.Lines.Add(FCTe.vPrest.comp[i].xNome);
        rlmCompValor1.Lines.Add(FormatFloatBr(msk10x2, FCTe.vPrest.comp[i].vComp));
      end;
      1, 4, 7, 10:
      begin
        rlmCompNome2.Lines.Add(FCTe.vPrest.comp[i].xNome);
        rlmCompValor2.Lines.Add(FormatFloatBr(msk10x2, FCTe.vPrest.comp[i].vComp));
      end;
      2, 5, 8, 11:
      begin
        rlmCompNome3.Lines.Add(FCTe.vPrest.comp[i].xNome);
        rlmCompValor3.Lines.Add(FormatFloatBr(msk10x2, FCTe.vPrest.comp[i].vComp));
      end;
    end;
  end;

  rllVlrTotServico.Caption := FormatFloatBr(msk13x2, FCTe.vPrest.vTPrest);
  rllVlrTotReceber.Caption := FormatFloatBr(msk13x2, FCTe.vPrest.vRec);

	rllSitTrib.Caption := CSTICMSToStrTagPosText(FCTe.Imp.ICMS.SituTrib);
{$ENDIF}

{$IFDEF PL_103}
  case FCTe.Imp.ICMS.SituTrib of
    cst00:
      begin
        rllRedBaseCalc.Caption := '';
        rllBaseCalc.Caption    := FormatFloatBr(msk9x2, FCTe.Imp.ICMS.CST00.vBC);
//        rllAliqICMS.Caption    := FormatFloatBr(mskAliq, FCTe.Imp.ICMS.CST00.pICMS);
        rllAliqICMS.Caption    := FormatFloatBr(msk4x2, FCTe.Imp.ICMS.CST00.pICMS);
        rllVlrICMS.Caption     := FormatFloatBr(msk4x2, FCTe.Imp.ICMS.CST00.vICMS);
        rllICMS_ST.Caption     := '';
      end;
    cst20:
      begin
//        rllRedBaseCalc.Caption := FormatFloatBr(mskAliq, FCTe.Imp.ICMS.CST20.pRedBC);
        rllRedBaseCalc.Caption := FormatFloatBr(msk4x2, FCTe.Imp.ICMS.CST20.pRedBC);
        rllBaseCalc.Caption    := FormatFloatBr(msk9x2, FCTe.Imp.ICMS.CST20.vBC);
//        rllAliqICMS.Caption    := FormatFloatBr(mskAliq, FCTe.Imp.ICMS.CST20.pICMS);
        rllAliqICMS.Caption    := FormatFloatBr(msk4x2, FCTe.Imp.ICMS.CST20.pICMS);
        rllVlrICMS.Caption     := FormatFloatBr(msk4x2, FCTe.Imp.ICMS.CST20.vICMS);
        rllICMS_ST.Caption     := '';
        // FormatFloatBr(msk4x2, FCTe.Imp.ICMS.CST20.vICMS);
      end;
    cst40:
      begin
        rllRedBaseCalc.Caption := '';
        rllBaseCalc.Caption    := '';
        rllAliqICMS.Caption    := '';
        rllVlrICMS.Caption     := '';
        rllICMS_ST.Caption     := '';
      end;
    cst41:
      begin
        rllRedBaseCalc.Caption := '';
        rllBaseCalc.Caption    := '';
        rllAliqICMS.Caption    := '';
        rllVlrICMS.Caption     := '';
        rllICMS_ST.Caption     := '';
      end;
    cst45:
      begin
        rllRedBaseCalc.Caption := '';
        rllBaseCalc.Caption    := '';
        rllAliqICMS.Caption    := '';
        rllVlrICMS.Caption     := '';
        rllICMS_ST.Caption     := '';
      end;
    cst51:
      begin
        rllRedBaseCalc.Caption := '';
        rllBaseCalc.Caption    := '';
        rllAliqICMS.Caption    := '';
        rllVlrICMS.Caption     := '';
        rllICMS_ST.Caption     := '';
      end;
    cst80:
      begin
        rllRedBaseCalc.Caption := '';
        rllBaseCalc.Caption    := FormatFloatBr(msk9x2, FCTe.Imp.ICMS.CST80.vBC);
//        rllAliqICMS.Caption    := FormatFloatBr(mskAliq, FCTe.Imp.ICMS.CST80.pICMS);
        rllAliqICMS.Caption    := FormatFloatBr(msk4x2, FCTe.Imp.ICMS.CST80.pICMS);
        rllVlrICMS.Caption     := FormatFloatBr(msk9x2, FCTe.Imp.ICMS.CST80.vICMS);
        rllICMS_ST.Caption     := FormatFloatBr(msk9x2, FCTe.Imp.ICMS.CST80.vCred);
      end;
    cst81:
      begin
//        rllRedBaseCalc.Caption := FormatFloatBr(mskAliq, FCTe.Imp.ICMS.CST81.pRedBC);
        rllRedBaseCalc.Caption := FormatFloatBr(msk4x2, FCTe.Imp.ICMS.CST81.pRedBC);
        rllBaseCalc.Caption    := FormatFloatBr(msk9x2, FCTe.Imp.ICMS.CST81.vBC);
//        rllAliqICMS.Caption    := FormatFloatBr(mskAliq, FCTe.Imp.ICMS.CST81.pICMS);
        rllAliqICMS.Caption    := FormatFloatBr(msk4x2, FCTe.Imp.ICMS.CST81.pICMS);
        rllVlrICMS.Caption     := FormatFloatBr(msk9x2, FCTe.Imp.ICMS.CST81.vICMS);
        rllICMS_ST.Caption     := '';
        // FormatFloatBr(msk9x2, FCTe.Imp.ICMS.CST81.vICMS);
      end;
    cst90:
      begin
//        rllRedBaseCalc.Caption := FormatFloatBr(mskAliq, FCTe.Imp.ICMS.CST90.pRedBC);
        rllRedBaseCalc.Caption := FormatFloatBr(msk4x2, FCTe.Imp.ICMS.CST90.pRedBC);
        rllBaseCalc.Caption    := FormatFloatBr(msk9x2, FCTe.Imp.ICMS.CST90.vBC);
//        rllAliqICMS.Caption    := FormatFloatBr(mskAliq, FCTe.Imp.ICMS.CST90.pICMS);
        rllAliqICMS.Caption    := FormatFloatBr(msk4x2, FCTe.Imp.ICMS.CST90.pICMS);
        rllVlrICMS.Caption     := FormatFloatBr(msk9x2, FCTe.Imp.ICMS.CST90.vICMS);
        rllICMS_ST.Caption     := FormatFloatBr(msk4x2, FCTe.Imp.ICMS.CST90.vCred);
      end;
  end;
{$ELSE}
  //{$ENDIF}
  //{$IFDEF PL_104}
  case FCTe.Imp.ICMS.SituTrib of
    cst00:
    begin
      rllRedBaseCalc.Caption := '';
      rllBaseCalc.Caption := FormatFloatBr(msk9x2, FCTe.Imp.ICMS.ICMS00.vBC);
      rllAliqICMS.Caption := FormatFloatBr(msk4x2, FCTe.Imp.ICMS.ICMS00.pICMS);
      rllVlrICMS.Caption := FormatFloatBr(msk4x2, FCTe.Imp.ICMS.ICMS00.vICMS);
      rllICMS_ST.Caption := '';
    end;
    cst20:
    begin
      //        rllRedBaseCalc.Caption := FormatFloatBr(mskAliq, FCTe.Imp.ICMS.ICMS20.pRedBC);
      rllRedBaseCalc.Caption := FormatFloatBr(msk4x2, FCTe.Imp.ICMS.ICMS20.pRedBC);
      rllBaseCalc.Caption := FormatFloatBr(msk9x2, FCTe.Imp.ICMS.ICMS20.vBC);
      rllAliqICMS.Caption := FormatFloatBr(msk4x2, FCTe.Imp.ICMS.ICMS20.pICMS);
      rllVlrICMS.Caption := FormatFloatBr(msk4x2, FCTe.Imp.ICMS.ICMS20.vICMS);
      rllICMS_ST.Caption := '';
    end;
    cst40:
    begin
      rllRedBaseCalc.Caption := '';
      rllBaseCalc.Caption := '';
      rllAliqICMS.Caption := '';
      rllVlrICMS.Caption := '';
      rllICMS_ST.Caption := '';
    end;
    cst41:
    begin
      rllRedBaseCalc.Caption := '';
      rllBaseCalc.Caption := '';
      rllAliqICMS.Caption := '';
      rllVlrICMS.Caption := '';
      rllICMS_ST.Caption := '';
    end;
    cst45:
    begin
      rllRedBaseCalc.Caption := '';
      rllBaseCalc.Caption := '';
      rllAliqICMS.Caption := '';
      rllVlrICMS.Caption := '';
      rllICMS_ST.Caption := '';
    end;
    cst51:
    begin
      rllRedBaseCalc.Caption := '';
      rllBaseCalc.Caption := '';
      rllAliqICMS.Caption := '';
      rllVlrICMS.Caption := '';
      rllICMS_ST.Caption := '';
    end;
    cst60:
    begin
      rllRedBaseCalc.Caption := '';
      rllBaseCalc.Caption := FormatFloatBr(msk9x2, FCTe.Imp.ICMS.ICMS60.vBCSTRet);
      rllAliqICMS.Caption := FormatFloatBr(msk4x2, FCTe.Imp.ICMS.ICMS60.pICMSSTRet);
      //        rllVlrICMS.Caption     := FormatFloatBr(msk9x2, FCTe.Imp.ICMS.ICMS60.vICMSSTRet);
      rllVlrICMS.Caption := '';
      //        rllICMS_ST.Caption     := FormatFloatBr(msk9x2, FCTe.Imp.ICMS.ICMS60.vCred);
      rllICMS_ST.Caption := FormatFloatBr(msk9x2, FCTe.Imp.ICMS.ICMS60.vICMSSTRet);
    end;
    cst90:
    begin
      //        rllRedBaseCalc.Caption := FormatFloatBr(mskAliq, FCTe.Imp.ICMS.ICMS90.pRedBC);
      rllRedBaseCalc.Caption := FormatFloatBr(msk4x2, FCTe.Imp.ICMS.ICMS90.pRedBC);
      rllBaseCalc.Caption := FormatFloatBr(msk9x2, FCTe.Imp.ICMS.ICMS90.vBC);
      rllAliqICMS.Caption := FormatFloatBr(msk4x2, FCTe.Imp.ICMS.ICMS90.pICMS);
      rllVlrICMS.Caption := FormatFloatBr(msk9x2, FCTe.Imp.ICMS.ICMS90.vICMS);
      //        rllICMS_ST.Caption     := FormatFloatBr(msk4x2, FCTe.Imp.ICMS.ICMS90.vCred);
      rllICMS_ST.Caption := '';
    end;
    cstICMSOutraUF:
    begin
      //        rllRedBaseCalc.Caption := FormatFloatBr(mskAliq, FCTe.Imp.ICMS.ICMSOutraUF.pRedBCOutraUF);
      rllRedBaseCalc.Caption := FormatFloatBr(msk4x2, FCTe.Imp.ICMS.ICMSOutraUF.pRedBCOutraUF);
      rllBaseCalc.Caption := FormatFloatBr(msk9x2, FCTe.Imp.ICMS.ICMSOutraUF.vBCOutraUF);
      rllAliqICMS.Caption := FormatFloatBr(msk4x2, FCTe.Imp.ICMS.ICMSOutraUF.pICMSOutraUF);
      rllVlrICMS.Caption := FormatFloatBr(msk9x2, FCTe.Imp.ICMS.ICMSOutraUF.vICMSOutraUF);
      rllICMS_ST.Caption := '';
    end;
    cstICMSSN:
    begin
      rllRedBaseCalc.Caption := '';
      rllBaseCalc.Caption := '';
      rllAliqICMS.Caption := '';
      rllVlrICMS.Caption := '';
      rllICMS_ST.Caption := '';
    end;
  end;
  if FCTe.ide.modelo = 67 then
  begin
    rlblVlrPIS.Caption := FormatFloatBr(msk13x2, FCTe.imp.infTribFed.vPIS);
    rlblVlrCOFINS.Caption := FormatFloatBr(msk13x2, FCTe.imp.infTribFed.vCOFINS);
    rlblVlrIR.Caption := FormatFloatBr(msk13x2, FCTe.imp.infTribFed.vIR);
    rlblVlrINSS.Caption := FormatFloatBr(msk13x2, FCTe.imp.infTribFed.vINSS);
    rlblVlrCSLL.Caption := FormatFloatBr(msk13x2, FCTe.imp.infTribFed.vCSLL);
  end;
{$ENDIF}
end;

procedure TfrmDACTeRLRetrato.rlb_03_DadosDACTe_OSBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  tomadorMod67();
  PrintIt := RLCTe.PageNumber = 1;
end;

procedure TfrmDACTeRLRetrato.rlb_04_DadosNotaFiscalBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  inherited;
  PrintIt := RLCTe.PageNumber = 1;

  // Imprime os dados da da Nota Fiscal se o Tipo de CTe for Normal ou Substituto
  rlb_04_DadosNotaFiscal.Enabled := ( ((FCTe.Ide.tpCTe = tcNormal) or (FCTe.Ide.tpCTe = tcSubstituto))
                                    and (FCTe.ide.modelo <> 67) );
  if not(rlb_04_DadosNotaFiscal.Enabled) then
    rlb_04_DadosNotaFiscal.Height := 0
  else
    dadosNotaFiscalVersao30();


end;

procedure TfrmDACTeRLRetrato.rlb_05_ComplementoBeforePrint(Sender: TObject; var PrintIt: Boolean);
{$IFNDEF PL_200}
var
  i: integer;
{$ENDIF}
begin
  // Imprime a lista dos CT-e Complementados se o Tipo de CTe for Complemento

  inherited;
  rlb_05_Complemento.Enabled := (FCTe.Ide.tpCTe = tcComplemento);
  if not rlb_05_Complemento.Enabled then
    rlb_05_Complemento.Height := 0;

  PrintIt := (RLCTe.PageNumber = 1);

  rlmComplChave1.Lines.Clear;
  rlmComplValor1.Lines.Clear;
  rlmComplChave2.Lines.Clear;
  rlmComplValor2.Lines.Clear;




{$IFDEF PL_200}
  rlmComplChave1.Lines.Add(FCTe.InfCTeComp.Chave);
  rlmComplValor1.Lines.Add(FormatFloatBr(msk10x2, FCTe.vPrest.vTPrest));
{$ELSE}
  for i := 0 to (FCTe.InfCTeComp.Count - 1) do
  begin
    case i of
      0..4:
      begin
        rlmComplChave1.Lines.Add(FCTe.InfCTeComp[i].Chave);
        rlmComplValor1.Lines.Add(FormatFloatBr(msk10x2, FCTe.InfCTeComp[i].vPresComp.vTPrest));
      end;
      5..9:
      begin
        rlmComplChave2.Lines.Add(FCTe.InfCTeComp[i].Chave);
        rlmComplValor2.Lines.Add(FormatFloatBr(msk10x2, FCTe.InfCTeComp[i].vPresComp.vTPrest));
      end;
    end;
  end;
{$ENDIF}
end;

procedure TfrmDACTeRLRetrato.rlb_06_ValorPrestacaoBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  inherited;
  PrintIt := RLCTe.PageNumber = 1;
end;

procedure TfrmDACTeRLRetrato.rlb_07_HeaderItensAfterPrint(Sender: TObject);
begin
  inherited;

  if (Linhas>70) and (not cdsDocumentos.eof) then
  begin
    Linhas := 0;
    rlDocOrig_tpDoc1.Height := 50;
    rlDocOrig_tpDoc2.Height := 50;
    rld_07_headerItens.Height := 81;
    RLCTe.newpage;
  end;

end;

procedure TfrmDACTeRLRetrato.rlb_07_HeaderItensBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  inherited;
  //PrintIt := RLCTe.PageNumber = 1;

  rlb_07_HeaderItens.Enabled := ( ((FCTe.Ide.tpCTe = tcNormal) or (FCTe.Ide.tpCTe = tcComplemento) or
    (FCTe.Ide.tpCTe = tcSubstituto)) and (FCTe.ide.modelo <> 67) );
  if (rlb_07_HeaderItens.Enabled) then
  begin

    rlDocOrig_tpDoc1.Lines.Clear;
    rlDocOrig_tpDoc2.Lines.Clear;

    //cdsDocumentos.First;

    if (RLCTe.PageNumber<=1) then
      cdsDocumentos.First
    else
      PrintIt := (not cdsDocumentos.eof);

    while not cdsDocumentos.Eof do
    begin
      if cdsDocumentos.FieldByName('TIPO_1').AsString <> '' then
      begin
        rlDocOrig_tpDoc1.Lines.Add(PadRight(cdsDocumentos.FieldByName('TIPO_1').AsString, 33, ' ')+
        PadRight(cdsDocumentos.FieldByName('CNPJCPF_1').AsString, 54, ' ')+
        cdsDocumentos.FieldByName('DOCUMENTO_1').AsString);
      end;
      if cdsDocumentos.FieldByName('TIPO_2').AsString <> '' then
      begin
        rlDocOrig_tpDoc2.Lines.Add(PadRight(cdsDocumentos.FieldByName('TIPO_2').AsString, 33, ' ')+
        PadRight(cdsDocumentos.FieldByName('CNPJCPF_2').AsString, 54, ' ')+
        cdsDocumentos.FieldByName('DOCUMENTO_2').AsString);
      end;
      cdsDocumentos.Next;

      if (RLCTe.PageNumber>1) then
        inc(Linhas);
      if ((cdsDocumentos.recno > 4) and (RLCTe.PageNumber=1) or (Linhas>70)) then
        break;

    end;

    rlDocOrig_tpDoc1.Height := Round( rlDocOrig_tpDoc1.Lines.Count * 12);
    rlDocOrig_tpDoc2.Height := Round(rlDocOrig_tpDoc2.Lines.Count * 12);
    rld_07_headerItens.Height := rlb_07_HeaderItens.Height - 12;

  end
  else
  begin
    rlb_07_HeaderItens.AutoSize := False;
    rlb_07_HeaderItens.Height   := 0;

  end;

end;

procedure TfrmDACTeRLRetrato.rlb_09_ObsBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  inherited;
  PrintIt := RLCTe.PageNumber = 1;

  rlmObs.Lines.BeginUpdate;
  rlmObs.Lines.Clear;

  rlmObs.Lines.Add(StringReplace(FCTe.Compl.xObs, '&lt;BR&gt;', #13#10, [rfReplaceAll, rfIgnoreCase]));

  if FCTe.Ide.tpEmis in [teContingencia, teFSDA, teDPEC] then
  begin
    if not (FCTe.procCTe.cStat in [100, 101, 110]) then
      rlmObs.Lines.Add(ACBrStr('DACTE em Contingência - Impresso em decorrência de problemas técnicos.'));
  end;

  if (FCTe.Ide.tpEmis = teDPEC) and (FEPECEnviado) then
    rlmObs.Lines.Add('EPEC regularmente recebida pela Receita Federal do Brasil');

  rlmObs.Lines.Text := StringReplace(rlmObs.Lines.Text, ';', #13, [rfReplaceAll]);
  rlmObs.Lines.EndUpdate;

  // Mensagem para modo Homologacao.
  rllMsgTeste.Visible := False;
  rllMsgTeste.Enabled := False;

  if FCTe.Ide.tpAmb = taHomologacao then
  begin
    rllMsgTeste.Caption := ACBrStr('AMBIENTE DE HOMOLOGAÇÃO - SEM VALOR FISCAL');
    rllMsgTeste.Visible := True;
    rllMsgTeste.Enabled := True;
  end
  else
  begin
    if FCTe.procCTe.cStat > 0 then
    begin
      if (FCTe.procCTe.cStat = 101) or (FCTeCancelada) then
      begin
        rllMsgTeste.Caption := 'CT-e CANCELADO';
        rllMsgTeste.Visible := True;
        rllMsgTeste.Enabled := True;
      end;

      if FCTe.procCTe.cStat = 110 then
      begin
        rllMsgTeste.Caption := 'CT-e DENEGADO';
        rllMsgTeste.Visible := True;
        rllMsgTeste.Enabled := True;
      end;

      if not FCTe.procCTe.cStat in [101, 110, 100] then
      begin
        rllMsgTeste.Caption := FCTe.procCTe.xMotivo;
        rllMsgTeste.Visible := True;
        rllMsgTeste.Enabled := True;
      end;
    end
    else
    begin
      rllMsgTeste.Caption := ACBrStr('CT-E NÃO ENVIADO PARA SEFAZ');
      rllMsgTeste.Visible := True;
      rllMsgTeste.Enabled := True;
    end;
  end;

  rllMsgTeste.Repaint;
  

  //  rllMsgTeste.Enabled := FCTe.Ide.tpAmb = taHomologacao;
  // Ajusta o tamanho do quadro conforme a OBS
  if (rlmObs.Lines.Count * 11 + 20) > rlb_09_Obs.Height then
  begin
    rlsQuadro08.Height  := rlmObs.Lines.Count * 11 + 20;
    rlmObs.Height       := rlmObs.Lines.Count * 11 + 20;
  end;
end;

procedure TfrmDACTeRLRetrato.rlb_10_ModRodFracionadoBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  inherited;
  PrintIt := RLCTe.PageNumber = 1;

  // Imprime as Informações Especificas do Modal se o Tipo de CTe for Normal
  rlb_10_ModRodFracionado.Enabled := (FCTe.Ide.tpCTe = tcNormal) and (FCTe.Ide.modal = mdRodoviario);
  if not (rlb_10_ModRodFracionado.Enabled) then
    rlb_10_ModRodFracionado.Height := 0;

  rlb_11_ModRodLot103.Enabled      := False;
  rlb_11_ModRodLot104.Enabled      := False;

{$IFDEF PL_200}
  with FCTe.infCTeNorm.rodo do
{$ELSE}
  with FCTe.Rodo do
{$ENDIF}
  begin
    rllRntrcEmpresa.Caption := RNTRC;

{$IFDEF PL_103}
    rlsCIOT.Enabled := False;
    lblCIOT.Enabled := False;
    rllCIOT.Enabled := False;
{$ELSE}
    rlsCIOT.Enabled := True;
    lblCIOT.Enabled := True;
    rllCIOT.Enabled := True;
    rllCIOT.Caption := CIOT;
{$ENDIF}
    if rlb_10_ModRodFracionado.Enabled then
    begin
      if (FCTe.infCTe.versao >= 3.00) then
        modalRodoviarioVersao30()
      else
      begin
        case Lota of
          ltNao:
          begin
            rllTituloLotacao.Caption := ACBrStr('DADOS ESPECÍFICOS DO MODAL RODOVIÁRIO - CARGA FRACIONADA');
            rllLotacao.Caption := ACBrStr('NÃO');
          end;
          ltsim:
          begin
            rllTituloLotacao.Caption := ACBrStr('DADOS ESPECÍFICOS DO MODAL RODOVIÁRIO - LOTAÇÃO');
            rllLotacao.Caption := 'SIM';
            if Versao = 103 then
              rlb_11_ModRodLot103.Enabled := True
            else
              rlb_11_ModRodLot104.Enabled := True;
          end;
        end;

        if ( dPrev > 0 ) then
          rllDtPrevEntrega.Caption := FormatDateTime('DD/MM/YYYY', dPrev);

      end;
    end;
  end;

end;

procedure TfrmDACTeRLRetrato.rlb_11_ModRodLot103BeforePrint(Sender: TObject; var PrintIt: Boolean);
var
  i: integer;
begin
  inherited;
  PrintIt := RLCTe.PageNumber = 1;

{$IFDEF PL_103}
  rllNumRegEsp.Caption := FCTe.Rodo.valePed.nroRE;
  case FCTe.Rodo.valePed.respPg of
   rpEmitente:       rllResponsavel.Caption := 'EMITENTE';
   rpRemetente:      rllResponsavel.Caption := 'REMETENTE';
   rpExpedidor:      rllResponsavel.Caption := 'EXPEDIDOR';
   rpRecebedor:      rllResponsavel.Caption := 'RECEBEDOR';
   rpDestinatario:   rllResponsavel.Caption := ACBrStr('DESTINATÁRIO');
   rpTomadorServico: rllResponsavel.Caption := ACBrStr('TOMADOR DO SERVIÇO');
  end;
  rllValorTotal.Caption := FormatFloatBr(msk13x2, FCTe.Rodo.valePed.vTValePed);
{$ENDIF}

  rlmTipo.Lines.Clear;
  rlmPlaca.Lines.Clear;
  rlmUF.Lines.Clear;
  rlmRNTRC.Lines.Clear;

  rlmEmpresas.Lines.Clear;
  rlmVigencias.Lines.Clear;
  rlmNumDispositivo.Lines.Clear;
  rlmCodTransacao.Lines.Clear;

  rllNomeMotorista.Caption := '';
  rllCPFMotorista.Caption := '';
  rllLacres.Caption := '';

{$IFDEF PL_200}
  for i:= 0 to (FCTe.infCTeNorm.Rodo.veic.Count - 1) do
  begin
   // TpcteTipoVeiculo = (tvTracao, tvReboque);
			if FCTe.infCTeNorm.Rodo.veic.Items[i].tpVeic = tvTracao then 
				rlmTipo.Lines.Add(ACBrStr('Tração'))
			else 
				rlmTipo.Lines.Add('Reboque');
   rlmPlaca.Lines.Add(FCTe.infCTeNorm.Rodo.veic.Items[i].placa);
   rlmUF.Lines.Add(FCTe.infCTeNorm.Rodo.veic.Items[i].UF);
   rlmRNTRC.Lines.Add(FCTe.infCTeNorm.Rodo.veic.Items[i].Prop.RNTRC);
  end;

  if FCTe.infCTeNorm.Rodo.moto.Count>0
   then begin
    rllNomeMotorista.Caption := FCTe.infCTeNorm.Rodo.moto.Items[0].xNome;
    rllCPFMotorista.Caption  := FormatarCPF(FCTe.infCTeNorm.Rodo.moto.Items[0].CPF);
   end;

  for i := 0 to (FCTe.infCTeNorm.Rodo.lacRodo.Count - 1) do
  begin
   rllLacres.Caption := rllLacres.Caption + FCTe.infCTeNorm.Rodo.lacRodo.Items[i].nLacre + '/';
  end;
{$ELSE}
  for i := 0 to (FCTe.Rodo.veic.Count - 1) do
  begin
    // TpcteTipoVeiculo = (tvTracao, tvReboque);
    if FCTe.Rodo.veic.Items[i].tpVeic = tvTracao then
      rlmTipo.Lines.Add(ACBrStr('Tração'))
    else
      rlmTipo.Lines.Add('Reboque');
    rlmPlaca.Lines.Add(FCTe.Rodo.veic.Items[i].placa);
    rlmUF.Lines.Add(FCTe.Rodo.veic.Items[i].UF);
    rlmRNTRC.Lines.Add(FCTe.Rodo.veic.Items[i].Prop.RNTRC);
  end;

{$IFDEF PL_103}
  for i := 0 to (FCTe.Rodo.valePed.disp.Count - 1) do
  begin
   rlmEmpresas.Lines.Add(FCTe.Rodo.valePed.disp.Items[i].xEmp);
   rlmVigencias.Lines.Add(FormatDateTime('DD/MM/YYYY', FCTe.Rodo.valePed.disp.Items[i].dVig));
   rlmNumDispositivo.Lines.Add(FCTe.Rodo.valePed.disp.Items[i].nDisp);
   rlmCodTransacao.Lines.Add(FCTe.Rodo.valePed.disp.Items[i].nCompC);
  end;
{$ENDIF}

  if FCTe.Rodo.moto.Count > 0 then
  begin
    rllNomeMotorista.Caption := FCTe.Rodo.moto.Items[0].xNome;
    rllCPFMotorista.Caption := FormatarCPF(FCTe.Rodo.moto.Items[0].CPF);
  end;

  for i := 0 to (FCTe.Rodo.Lacres.Count - 1) do
    rllLacres.Caption := rllLacres.Caption + FCTe.Rodo.Lacres.Items[i].nLacre + '/';
{$ENDIF}
end;

procedure TfrmDACTeRLRetrato.rlb_11_ModRodLot104AfterPrint(Sender: TObject);
begin
  inherited;

  if ((cdsDocumentos.recNo>4) and (rlCte.PageNumber=1)) then
    RLCte.newpage;
end;

procedure TfrmDACTeRLRetrato.cabecalhoVersao30;
begin
  if (FCTe.ide.modelo <> 67) then   //67-CTeOS
  begin
    rlLabel28.Caption               := ACBrStr('INDICADOR DO CT-E GLOBALIZADO');
    rllabel78.Caption               := ACBrStr('INFORMAÇÕES DO CT-E GLOBALIZADO');

    if FCTe.ide.indGlobalizado = tiSim then
    begin
      rllTomaServico.Caption        := 'SIM';
      if (Trim(FCTe.infCTeNorm.infGlobalizado.xObs) <> '') then
         rllFormaPagamento.Caption  := FCTe.infCTeNorm.infGlobalizado.xObs;

    end
    else
      rllTomaServico.Caption        := ACBrStr('NÃO');

  end
  else
  begin
    rlLabel28.Caption               := ACBrStr('CÓDIGO FISCAL DE OPERAÇÕES E PRESTAÇÕES - NATUREZA DA OPERAÇÃO');
    rllabel78.Caption               := '';
    rlsLinhaV01.Height              := 31;
    rllTomaServico.AutoSize         := true;
    rllTomaServico.Caption          := FormatFloat('0000', FCTe.Ide.CFOP) + ' - ' + FCTe.Ide.natOp;
    rllFormaPagamento.Caption       := '';

  end;
end;

procedure TfrmDACTeRLRetrato.tomadorMod67;
var
  I: Integer;
begin
  rlb_03_DadosDACTe_OS.Enabled := (FCTe.ide.modelo = 67);
  if (rlb_03_DadosDACTe_OS.Enabled) then
  begin

    rllOrigPrestacao1.Caption := FCTe.Ide.xMunIni + ' - ' + FCTe.Ide.UFIni + ' - ' + FormatFloat('000', FCTe.Ide.cMunIni);
    rllDestPrestacao1.Caption := FCTe.Ide.xMunFim + ' - ' + FCTe.Ide.UFFim + ' - ' + FormatFloat('000', FCTe.Ide.cMunFim);

    for I := 0 to FCTe.ide.infPercurso.Count -1 do
    begin
      if I = 0 then
        rllPercursoVeiculo.Caption := FCTe.ide.infPercurso.Items[I].UFPer
      else
        rllPercursoVeiculo.Caption := rllPercursoVeiculo.Caption +' - '+ FCTe.ide.infPercurso.Items[I].UFPer;
    end;

    with FCTe.toma do
    begin
      rllRazaoToma1.Caption         := xNome;
      rllEnderecoToma1.Caption      := EnderToma.xLgr + ', ' + EnderToma.nro +
                                    ' - ' + EnderToma.xCpl + ' - ' + EnderToma.xBairro;
      rllCEPToma1.Caption           := FormatarCEP(EnderToma.CEP);
      rllMunToma1.Caption           := EnderToma.xMun + ' - ' + EnderToma.UF;
      rllCnpjToma1.Caption          := FormatarCNPJouCPF(CNPJCPF);
      rllPaisToma1.Caption          := EnderToma.xPais;
      rllInscEstToma1.Caption       := IE;
      rllFoneToma1.Caption          := FormatarFone(fone);

    end;

  end
  else
   rlb_03_DadosDACTe_OS.Height := 0;

end;

procedure TfrmDACTeRLRetrato.dadosNotaFiscalVersao30;
begin
  //Valida a Versão para ocultar campos na tela
  with FCTe.infCTe do
  begin
    rlLabel5.Visible                    := not(versao >= 3.00);
    rlLabel37.Visible                   := not(versao >= 3.00);
    rlmNomeSeguradora.Visible           := not(versao >= 3.00);
    rlmRespSeguroMerc.Visible           := not(versao >= 3.00);
    rlLabel39.Visible                   := not(versao >= 3.00);
    rlmNroApolice.Visible               := not(versao >= 3.00);
    rllabel40.Visible                   := not(versao >= 3.00);
    rlmNroAverbacao.Visible             := not(versao >= 3.00);
    RLDraw62.Visible                    := not(versao >= 3.00);
    RLDraw61.Visible                    := not(versao >= 3.00);
    RLDraw7.Visible                     := not(versao >= 3.00);
    RLDraw8.Visible                     := not(versao >= 3.00);

  end;

  //Valida a Versão para reposicionar campos na tela
  if (FCTe.infCTe.versao >= 3.00) then
  begin
    RLDraw58.Left            := 148;
    RLDraw59.Left            := 296;
    RLDraw100.Left           := 444;
    RLDraw60.Left            := 592;

    RLLabel35.Width          := 136;
    RLLabel35.Left           := 5;
    rlmQtdUnidMedida1.Width  := 136;
    rlmQtdUnidMedida1.Left   := 5;

    RLLabel36.Width          := 136;
    RLLabel36.Left           := 154;
    rlmQtdUnidMedida2.Width  := 136;
    rlmQtdUnidMedida2.Left   := 154;

    RLLabel41.Width          := 136;
    RLLabel41.Left           := 304;
    rlmQtdUnidMedida3.Width  := 136;
    rlmQtdUnidMedida3.Left   := 304;

    RLLabel73.Width          := 136;
    RLLabel73.Left           := 449;
    rlmQtdUnidMedida4.Width  := 136;
    rlmQtdUnidMedida4.Left   := 449;

    RLLabel43.Width          := 136;
    RLLabel43.Left           := 600;
    rlmQtdUnidMedida5.Width  := 136;
    rlmQtdUnidMedida5.Left   := 600;

  end;
end;

procedure TfrmDACTeRLRetrato.modalRodoviarioVersao30;
begin
  rllTituloLotacao.Caption   := ACBrStr('DADOS ESPECÍFICOS DO MODAL RODOVIÁRIO');
  rlsCIOT.Enabled            := False;
  lblCIOT.Caption            := '';
  rllCIOT.Caption            := '';
  rllLotacao.Caption         := '';
  RLLabel83.Caption          := '';
  rlsCIOT.Visible            := False;
  RLDraw36.Visible           := False;

  if ( FCTe.compl.Entrega.comData.dProg > 0 ) then
    rllDtPrevEntrega.Caption := FormatDateTime('DD/MM/YYYY', FCTe.compl.Entrega.comData.dProg);

  if (FCTe.ide.modelo = 67) then
    modalRodoviarioMod67();

end;

procedure TfrmDACTeRLRetrato.modalRodoviarioMod67;
var
  lTAF, lREG, lCNPJ : String;
begin
  with FCTE.infCTeNorm.rodoOS.veic do
  begin
    if (Length(Prop.TAF) > 0) or (Length(Prop.NroRegEstadual) > 0) then
    begin
      //Terceiro
      lTAF  := Prop.TAF;
      lREG  := prop.NroRegEstadual;
      lCNPJ := prop.CNPJCPF;
    end
    else
    begin
      //Próprio
      lTAF  := FCTe.infCTeNorm.rodoOS.TAF;
      lREG  := FCTe.infCTeNorm.rodoOS.NroRegEstadual;
      lCNPJ := FCTe.Emit.CNPJ;
    end;
  end;

  rlsCIOT.Visible            := True;
  RLDraw36.Visible           := True;
  RLDraw37.Visible           := True;
  RLDraw38.Visible           := True;

  rlsCIOT.Left               := 148;
  RLDraw36.Left              := 296;
  RLDraw37.Left              := 444;
  RLDraw38.Left              := 592;

  RLLabel11.Visible          := True;
  rllRntrcEmpresa.Visible    := True;
  RLLabel11.Caption          := ACBrStr('TERMO AUTORIZAÇÃO DE FRETAMENTO');
  rllRntrcEmpresa.Caption    := lTAF;

  lblCIOT.Visible            := True;
  rllCIOT.Visible            := True;
  lblCIOT.Left               := 154;
  rllCIOT.Left               := 154;
  lblCIOT.Caption            := ACBrStr('Nº DE REGISTRO ESTADUAL');
  rllCIOT.Caption            := lREG;

  RLLabel83.Visible          := True;
  rllLotacao.Visible         := True;
  RLLabel83.Left             := 301;
  rllLotacao.Left            := 301;
  RLLabel83.Caption          := ACBrStr('PLACA DO VEÍCULO');
  rllLotacao.Caption         := FCTE.infCTeNorm.rodoOS.veic.placa;

  RLLabel84.Visible          := True;
  rllDtPrevEntrega.Visible   := True;
  RLLabel84.Left             := 449;
  rllDtPrevEntrega.Left      := 449;
  RLLabel84.Caption          := ACBrStr('RENAVAM DO VEÍCULO');
  rllDtPrevEntrega.Caption   := FCTE.infCTeNorm.rodoOS.veic.RENAVAM;

  RLLabel216.Visible         := True;
  RLLabel85.Visible          := True;
  RLLabel85.Width            := 70;
  RLLabel216.Left            := 597;
  RLLabel85.Left             := 597;
  RLLabel216.Caption         := ACBrStr('CNPJ/CPF');
  RLLabel85.Caption          := lCNPJ;
end;

procedure TfrmDACTeRLRetrato.fluxoCargaVersao30;
var
  i : Integer;
begin
  rllSiglaPassagem.Lines.Clear;

  rlb_Fluxo_Carga.Enabled := ((FCTe.infCTe.versao >= 3.00) and (FCTe.ide.modelo <> 67) and
                                               (FCTe.ide.modal = mdAereo)) ;
  if (rlb_Fluxo_Carga.Enabled) then
  begin
    rllSiglaOrigem.Caption       := FCTe.compl.fluxo.xOrig;
    rllSiglaDestino.Caption      := FCTe.compl.fluxo.xDest;

    for i := 0 to (FCTe.compl.fluxo.pass.Count-1) do
      rllSiglaPassagem.Lines.Add(FCTe.compl.fluxo.pass.Items[i].xPass);
  end
  else
    rlb_Fluxo_Carga.Height := 0;

end;

procedure TfrmDACTeRLRetrato.prestacaoServicoMod67;
begin
  rlb_CTeOS_PrestacaoServico.Enabled := (FCTe.ide.modelo = 67);
  if (rlb_CTeOS_PrestacaoServico.Enabled) then
  begin
    rlb_CTeOS_PrestacaoServico.AutoSize := True;
    rlDocOrig_tpDoc3.Lines.Clear;

    rlDocOrig_tpDoc3.Lines.Add('  ' +  IfThen(FCTe.infCTeNorm.infServico.qCarga = 0, '',
                               FormatFloatBr(msk6x4,FCTe.infCTeNorm.infServico.qCarga))
                               + Space(25) + FCTe.infCTeNorm.infServico.xDescServ);

  end
  else
  begin
    rlb_CTeOS_PrestacaoServico.Height := 0;
  end;
end;

procedure TfrmDACTeRLRetrato.dadosSeguradoraMod67;
var
  I: Integer;
begin
  rlb_Dados_Seguradora.Enabled := ((FCTe.ide.modelo = 67) or (FCTe.Ide.modal = mdMultimodal));
  if (rlb_Dados_Seguradora.Enabled) then
  begin
    rllNomeSeguradora.Lines.Clear;
    rllApolice.Lines.Clear;
    rllResponsavelSeguro.Lines.Clear;

    if (FCTE.infCTeNorm.seg.Count > 0) then
    begin
      for I := 0 to FCTE.infCTeNorm.seg.Count - 1 do
      begin
        case FCTE.infCTeNorm.seg.Items[I].respSeg of
          rsRemetente     : rllResponsavelSeguro.Lines.Add('Remetente');
          rsExpedidor     : rllResponsavelSeguro.Lines.Add('Expedidor');
          rsRecebedor     : rllResponsavelSeguro.Lines.Add('Recebedor');
          rsDestinatario  : rllResponsavelSeguro.Lines.Add('Destinatário');
          rsEmitenteCTe   : rllResponsavelSeguro.Lines.Add('Emitente');
          rsTomadorServico: rllResponsavelSeguro.Lines.Add('Tomador');
        end;
        rllNomeSeguradora.Lines.Add(FCTE.infCTeNorm.seg.Items[I].xSeg);
        rllApolice.Lines.Add(FCTE.infCTeNorm.seg.Items[I].nApol);

      end;
    end;
  end
  else
    rlb_Dados_Seguradora.Height := 0;
end;

procedure TfrmDACTeRLRetrato.rlb_11_ModRodLot104BeforePrint(Sender: TObject; var PrintIt: Boolean);
var
  i: integer;
begin
  inherited;
  PrintIt := RLCTe.PageNumber = 1;

  rlmTipo2.Lines.Clear;
  rlmPlaca2.Lines.Clear;
  rlmUF2.Lines.Clear;
  rlmRNTRC2.Lines.Clear;

  rlmCNPJForn.Lines.Clear;
  rlmNumCompra.Lines.Clear;
  rlmCNPJPg.Lines.Clear;

  rllNomeMotorista2.Caption := '';
  rllCPFMotorista2.Caption := '';
  rllLacres2.Caption := '';

{$IFDEF PL_200}
  for i:= 0 to (FCTe.infCTeNorm.Rodo.veic.Count - 1) do
  begin
   // TpcteTipoVeiculo = (tvTracao, tvReboque);
		if FCTe.infCTeNorm.Rodo.veic.Items[i].tpVeic = tvTracao  then 
			rlmTipo2.Lines.Add(ACBrStr('Tração'))
    else 
			rlmTipo2.Lines.Add('Reboque');
   rlmPlaca2.Lines.Add(FCTe.infCTeNorm.Rodo.veic.Items[i].placa);
   rlmUF2.Lines.Add(FCTe.infCTeNorm.Rodo.veic.Items[i].UF);
   rlmRNTRC2.Lines.Add(FCTe.infCTeNorm.Rodo.veic.Items[i].Prop.RNTRC);
  end;

  for i := 0 to (FCTe.infCTeNorm.Rodo.valePed.Count -1) do
  begin
   rlmCNPJForn.Lines.Add(FormatarCNPJ(FCTe.infCTeNorm.Rodo.valePed.Items[i].CNPJForn));
   rlmNumCompra.Lines.Add(FCTe.infCTeNorm.Rodo.valePed.Items[i].nCompra);
   rlmCNPJPg.Lines.Add(FormatarCNPJ(FCTe.infCTeNorm.Rodo.valePed.Items[i].CNPJPg));
  end;

  if FCTe.infCTeNorm.Rodo.moto.Count>0
   then begin
    rllNomeMotorista2.Caption := FCTe.infCTeNorm.Rodo.moto.Items[0].xNome;
    rllCPFMotorista2.Caption  := FormatarCPF(FCTe.infCTeNorm.Rodo.moto.Items[0].CPF);
   end;

  for i := 0 to (FCTe.infCTeNorm.Rodo.lacRodo.Count - 1) do
  begin
   rllLacres2.Caption := rllLacres2.Caption + FCTe.infCTeNorm.Rodo.lacRodo.Items[i].nLacre + '/';
  end;
{$ELSE}
  for i := 0 to (FCTe.Rodo.veic.Count - 1) do
  begin
    // TpcteTipoVeiculo = (tvTracao, tvReboque);
    if FCTe.Rodo.veic.Items[i].tpVeic = tvTracao then
      rlmTipo2.Lines.Add(ACBrStr('Tração'))
    else
      rlmTipo2.Lines.Add('Reboque');
    rlmPlaca2.Lines.Add(FCTe.Rodo.veic.Items[i].placa);
    rlmUF2.Lines.Add(FCTe.Rodo.veic.Items[i].UF);
    rlmRNTRC2.Lines.Add(FCTe.Rodo.veic.Items[i].Prop.RNTRC);
  end;

{$IFDEF PL_104}
  for i := 0 to (FCTe.Rodo.valePed.Count -1) do
  begin
   rlmCNPJForn.Lines.Add(FormatarCNPJ(FCTe.Rodo.valePed.Items[i].CNPJForn));
   rlmNumCompra.Lines.Add(FCTe.Rodo.valePed.Items[i].nCompra);
   rlmCNPJPg.Lines.Add(FormatarCNPJ(FCTe.Rodo.valePed.Items[i].CNPJPg));
  end;
{$ENDIF}

  if FCTe.Rodo.moto.Count > 0 then
  begin
    rllNomeMotorista2.Caption := FCTe.Rodo.moto.Items[0].xNome;
    rllCPFMotorista2.Caption := FormatarCPF(FCTe.Rodo.moto.Items[0].CPF);
  end;

  for i := 0 to (FCTe.Rodo.Lacres.Count - 1) do
  begin
    rllLacres2.Caption := rllLacres2.Caption + FCTe.Rodo.Lacres.Items[i].nLacre + '/';
  end;
{$ENDIF}
end;

procedure TfrmDACTeRLRetrato.rlb_12_ModAereoBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  inherited;
  PrintIt := RLCTe.PageNumber = 1;
  rlb_12_ModAereo.Enabled := (FCTe.Ide.tpCTe = tcNormal) and (FCTe.Ide.modal = mdAereo);

  rllCaracAdServico.Caption := FCTe.Compl.xCaracSer;
  rllCaracAdTransporte.Caption := FCTe.Compl.xCaracAd;

{$IFDEF PL_200}
  with FCTe.infCTeNorm.aereo do
{$ELSE}
  with FCTe.Aereo do
{$ENDIF}
  begin
    rllAWB.Caption := nOCA;
  {$IFDEF PL_103}
    rllTrecho.Caption        := tarifa.trecho;
    rllContaCorrente.Caption := cIATA; // ??? Conta Corrente ???
  {$ENDIF}
    rllTarifaCL.Caption := tarifa.CL;
    rllTarifaCodigo.Caption := tarifa.cTar;
    rllTarifaValor.Caption := FormatCurr('###,###,##0.00', tarifa.vTar);
  {$IFNDEF PL_103}
    rllContaCorrente.Caption := IdT; // ??? Conta Corrente ???
  {$ENDIF}
    rllMinuta.Caption := FormatFloat('0000000000', nMinu);

    rllLojaAgenteEmissor.Caption := xLAgEmi;
  end;

  if FCte.Ide.retira = rtSim then
    rllRetira.Caption := 'SIM'
  else
    rllRetira.Caption := ACBrStr('NÃO');
  rllDadosRetira.Caption := FCte.Ide.xdetretira;
end;

procedure TfrmDACTeRLRetrato.rlb_13_ModAquaviarioBeforePrint(Sender: TObject; var PrintIt: Boolean);
var
  i, j: integer;
begin
  inherited;
  PrintIt := RLCTe.PageNumber = 1;
  rlb_13_ModAquaviario.Enabled := (FCTe.Ide.tpCTe = tcNormal) and (FCTe.Ide.modal = mdAquaviario);

{$IFDEF PL_200}
  with FCTe.infCTeNorm.aquav do
{$ELSE}
  with FCTe.aquav do
{$ENDIF}
  begin
    rllBCAFRMM.Caption := FormatCurr('###,###,##0.00', vPrest);
    rllValorAFRMM.Caption := FormatCurr('###,###,##0.00', vAFRMM);

    rllPortoEmbarque.Caption := prtEmb;
    rllPortoDestino.Caption := prtDest;
    rllIndNavioRebocador.Caption := xNavio;

    case tpNav of
      tnInterior: rllTipoNav.Caption := 'INTERIOR';
      tnCabotagem: rllTipoNav.Caption := 'CABOTAGEM';
    end;

    case direc of
      drNorte: rllDirecao.Caption := 'NORTE';
      drLeste: rllDirecao.Caption := 'LESTE';
      drSul: rllDirecao.Caption := 'SUL';
      drOeste: rllDirecao.Caption := 'OESTE';
    end;

    // Incluido por Fabio
    rllIndBalsas.Caption := '';
  {$IFNDEF PL_103}
    for i := 0 to (balsa.Count - 1) do
    begin
      if i = 0 then
        rllIndBalsas.Caption := balsa.Items[i].xBalsa
      else
        rllIndBalsas.Caption := rllIndBalsas.Caption + '/' + balsa.Items[i].xBalsa;
    end;
  {$ENDIF}

  {$IFNDEF PL_200}
    rllIndConteiners.Caption := '';
    for i := 0 to (detCont.Count - 1) do
    begin
      for j := 0 to (detCont.Items[i].Lacre.Count - 1) do
      begin
        if i > 0 then
          rllIndConteiners.Caption := rllIndConteiners.Caption + '  ';
        if j = 0 then
          rllIndConteiners.Caption := rllIndConteiners.Caption + detCont.Items[i].nCont + '-' +
            detCont.Items[i].Lacre.Items[j].nLacre
        else
          rllIndConteiners.Caption := rllIndConteiners.Caption + '/' + detCont.Items[i].Lacre.Items[j].nLacre;
      end;
    end;
  {$ENDIF}
  end;

end;

procedure TfrmDACTeRLRetrato.rlb_14_ModFerroviarioBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  inherited;
  PrintIt := RLCTe.PageNumber = 1;
  rlb_14_ModFerroviario.Enabled := (FCTe.Ide.tpCTe = tcNormal) and (FCTe.Ide.modal = mdFerroviario);

end;

procedure TfrmDACTeRLRetrato.rlb_15_ModDutoviarioBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  inherited;
  PrintIt := RLCTe.PageNumber = 1;
  rlb_15_ModDutoviario.Enabled := (FCTe.Ide.tpCTe = tcNormal) and (FCTe.Ide.modal = mdDutoviario);

end;

procedure TfrmDACTeRLRetrato.rlb_16_DadosExcEmitenteBeforePrint(Sender: TObject; var PrintIt: Boolean);
var
  i: integer;
begin
  inherited;
  PrintIt := RLCTe.PageNumber = 1;

  rlmObsExcEmitente.Lines.BeginUpdate;
  rlmObsExcEmitente.Lines.Clear;
  rlmObsFisco.Lines.Clear;

  for i := 0 to (FCTe.Compl.ObsCont.Count - 1) do
    with FCTe.Compl.ObsCont.Items[i] do
    begin
      rlmObsExcEmitente.Lines.Add(StringReplace(xCampo, '&lt;BR&gt;', #13#10, [rfReplaceAll, rfIgnoreCase]) + ': ' +
        StringReplace(xTexto, '&lt;BR&gt;', #13#10, [rfReplaceAll, rfIgnoreCase]));
    end;

  rlmObsExcEmitente.Lines.Text := StringReplace(rlmObsExcEmitente.Lines.Text, ';', #13, [rfReplaceAll]);
  rlmObsExcEmitente.Lines.EndUpdate;

  // Incluido por Italo em 17/09/2012
  rlmObsFisco.Lines.Add(StringReplace(FCTe.Imp.infAdFisco, '&lt;BR&gt;', #13#10, [rfReplaceAll, rfIgnoreCase]));

  for i := 0 to (FCTe.Compl.ObsFisco.Count - 1) do
    with FCTe.Compl.ObsFisco.Items[i] do
    begin
      rlmObsFisco.Lines.Add(StringReplace(xCampo, '&lt;BR&gt;', #13#10, [rfReplaceAll, rfIgnoreCase]) + ': ' +
        StringReplace(xTexto, '&lt;BR&gt;', #13#10, [rfReplaceAll, rfIgnoreCase]));
    end;

  rlmObsFisco.Lines.Text := StringReplace(rlmObsFisco.Lines.Text, ';', #13, [rfReplaceAll]);
  rlmObsFisco.Lines.EndUpdate;
end;

procedure TfrmDACTeRLRetrato.rlb_17_SistemaBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  inherited;
  PrintIt := RLCTe.PageNumber = 1;

  rlLabel15.Visible := FImprimeHoraSaida;
  RLSystemInfo2.Visible := FImprimeHoraSaida;
  
  if (FSistema <> '') or (FUsuario <> '') then
    rllblSistema.Caption := FSistema + ' - ' + FUsuario
  else
    rllblSistema.Caption := '';
end;

procedure TfrmDACTeRLRetrato.rlb_18_ReciboBeforePrint(Sender: TObject; var PrintIt: Boolean);
begin
  inherited;
  PrintIt := (RLCTe.PageNumber = 1);

  if (FResumoCanhoto) then
    rllResumoCanhotoCTe2.Caption := getTextoResumoCanhoto
  else
    rllResumoCanhotoCTe2.Caption := '';

  rllSerie3.Caption := IntToStr(FCTe.Ide.serie); // FormatFloat( '000', FCTe.Ide.serie);
  rllNumCte3.Caption := FormatFloat('000,000,000', FCTe.Ide.nCT);

  // TpcteTipoCTe = (tcNormal, tcComplemento, tcAnulacao, tcSubstituto);
  if PrintIt then
  begin
    rlb_18_Recibo.Enabled := ((FCTe.Ide.tpCTe = tcNormal) or (FCTe.Ide.tpCTe = tcComplemento)) and
      (FCTe.Ide.modal <> mdAereo) and (FPosRecibo = prRodape);
    if rlb_18_Recibo.Enabled then
      rlb_18_Recibo.Height := 97
    else
      rlb_18_Recibo.Height := 0;
  end;
end;

procedure TfrmDACTeRLRetrato.rlb_06_ProdutosPerigososBeforePrint(Sender: TObject; var PrintIt: Boolean);
var
  i: integer;
begin
  inherited;

{$IFDEF PL_200}
  rlb_06_ProdutosPerigosos.Enabled := (FCTe.infCTeNorm.peri.Count > 0);
{$ELSE}
  rlb_06_ProdutosPerigosos.Enabled := (FCTe.peri.Count > 0);
{$ENDIF}

  PrintIt := (RLCTe.PageNumber = 1);
  if not rlb_06_ProdutosPerigosos.Enabled then
    rlb_06_ProdutosPerigosos.Height := 0;

  rlmNumONU.Lines.Clear;
  rlmNomeApropriado.Lines.Clear;
  rlmClasse.Lines.Clear;
  rlmGrupoEmbalagem.Lines.Clear;
  rlmQtdeProduto.Lines.Clear;

{$IFDEF PL_200}
  for i := 0 to (FCTe.infCTeNorm.peri.Count-1) do
   begin
     rlmNumONU.Lines.Add(FCTe.infCTeNorm.peri.Items[i].nONU);
     rlmNomeApropriado.Lines.Add(FCTe.infCTeNorm.peri.Items[i].xNomeAE);
     rlmClasse.Lines.Add(FCTe.infCTeNorm.peri.Items[i].xClaRisco);
     rlmGrupoEmbalagem.Lines.Add(FCTe.infCTeNorm.peri.Items[i].grEmb);
     rlmQtdeProduto.Lines.Add(FCTe.infCTeNorm.peri.Items[i].qTotProd);
   end;
{$ELSE}
  for i := 0 to (FCTe.peri.Count - 1) do
  begin
    rlmNumONU.Lines.Add(FCTe.peri.Items[i].nONU);
    rlmNomeApropriado.Lines.Add(FCTe.peri.Items[i].xNomeAE);
    rlmClasse.Lines.Add(FCTe.peri.Items[i].xClaRisco);
    rlmGrupoEmbalagem.Lines.Add(FCTe.peri.Items[i].grEmb);
    rlmQtdeProduto.Lines.Add(FCTe.peri.Items[i].qTotProd);
  end;
{$ENDIF}

end;
procedure TfrmDACTeRLRetrato.rlb_06_VeiculosNovosBeforePrint(Sender: TObject; var PrintIt: Boolean);
var
  i: integer;
begin
  inherited;

{$IFDEF PL_200}
  rlb_06_VeiculosNovos.Enabled := (FCTe.infCTeNorm.veicNovos.Count > 0);
{$ELSE}
  rlb_06_VeiculosNovos.Enabled := (FCTe.veicNovos.Count > 0);
{$ENDIF}

  PrintIt := (RLCTe.PageNumber = 1);
  if not rlb_06_VeiculosNovos.Enabled then
    rlb_06_VeiculosNovos.Height := 0;

  CHASSI.Lines.Clear;
  COR.Lines.Clear;
  MODELO.Lines.Clear;
  VUNIT.Lines.Clear;
  VFRETE.Lines.Clear;

{$IFDEF PL_200}
  for i := 0 to (FCTe.infCTeNorm.veicNovos.Count-1) do
   begin
     CHASSI.Lines.Add(FCTe.infCTeNorm.veicNovos.Items[i].chassi);
     COR.Lines.Add(FCTe.infCTeNorm.veicNovos.Items[i].cCor + ' - ' + FCTe.infCTeNorm.veicNovos.Items[i].xCor);
     MODELO.Lines.Add(FCTe.infCTeNorm.veicNovos.Items[i].cMod);
     VUNIT.Lines.Add(FloatToString(FCTe.infCTeNorm.veicNovos.Items[i].vUnit,','));
     VFRETE.Lines.Add(FloatToString(FCTe.infCTeNorm.veicNovos.Items[i].vFrete,','));
   end;
{$ELSE}
  for i := 0 to (FCTe.peri.Count - 1) do
  begin
     CHASSI.Lines.Add(FCTe.veicNovos.Items[i].chassi);
     COR.Lines.Add(FCTe.veicNovos.Items[i].cCor + ' - ' + FCTe.veicNovos.Items[i].xCor);
     MODELO.Lines.Add(FCTe.veicNovos.Items[i].cMod);
     VUNIT.Lines.Add(FloatToString(FCTe.veicNovos.Items[i].vUnit,','));
     VFRETE.Lines.Add(FloatToString(FCTe.veicNovos.Items[i].vFrete,','));
  end;
{$ENDIF}

end;

procedure TfrmDACTeRLRetrato.rlb_CTeOS_PrestacaoServicoBeforePrint(
  Sender: TObject; var PrintIt: Boolean);
begin
  prestacaoServicoMod67();

end;

procedure TfrmDACTeRLRetrato.rlb_Dados_SeguradoraBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  dadosSeguradoraMod67();

end;

procedure TfrmDACTeRLRetrato.rlb_Fluxo_CargaBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  fluxoCargaVersao30();

end;

procedure TfrmDACTeRLRetrato.RLCTeBeforePrint(Sender: TObject; var PrintIt: boolean);
begin
{$IFDEF PL_103}
  Versao := 103;
{$ENDIF}
{$IFDEF PL_104}
  Versao := 104;
{$ENDIF}
{$IFDEF PL_200}
  Versao := 200;
{$ENDIF}

  Itens;

{$IFDEF PL_200}
  if FCTe.infCTeNorm.peri.Count = 0
   then rlb_06_ProdutosPerigosos.Visible := False;
  if FCTe.infCTeNorm.veicNovos.Count = 0
   then rlb_06_VeiculosNovos.Visible := False;

  if FCTe.ide.modelo = 67 then //CteOS
  begin
    rlpnlTributosFederais.Visible := True;
    rlb_06_ValorPrestacao.Height := 144;
  end
  else
  begin
    rlpnlTributosFederais.Visible := False;
    rlb_06_ValorPrestacao.Height := 117;
  end;
{$ELSE}
  if FCTe.peri.Count = 0 then
    rlb_06_ProdutosPerigosos.Visible := False;
  if FCTe.veicNovos.Count = 0
   then rlb_06_VeiculosNovos.Visible := False;

  rlpnlTributosFederais.Visible := False;
  rlb_06_ValorPrestacao.Height := 117;
{$ENDIF}

  rlb_10_ModRodFracionado.Height := 0;
  rlb_11_ModRodLot103.Height     := 0;
  rlb_11_ModRodLot104.Height     := 0;
  rlb_12_ModAereo.Height         := 0;
  rlb_13_ModAquaviario.Height    := 0;
  rlb_14_ModFerroviario.Height   := 0;
  rlb_15_ModDutoviario.Height    := 0;

  case FCTe.Ide.modal of
    mdRodoviario:
    begin
                 {$IFDEF PL_200}
                   if FCTe.infCTeNorm.rodo.lota = ltNao
                    then begin
                     rlb_10_ModRodFracionado.Height := 44;
                    end
                    else begin
                     rlb_10_ModRodFracionado.Height := 44;
                     rlb_11_ModRodLot104.Height     := 107;
                    end;
                 {$ELSE}
      if FCTe.Rodo.Lota = ltNao then
      begin
        rlb_10_ModRodFracionado.Height := 44;
      end
      else
      begin
        rlb_10_ModRodFracionado.Height := 44;
        if Versao = 103 then
          rlb_11_ModRodLot103.Height := 108
        else
          rlb_11_ModRodLot104.Height := 107;
      end;
                 {$ENDIF}
    end;
    mdAereo:
    begin
      rlb_12_ModAereo.Height := 97;
    end;
    mdAquaviario:
    begin
      rlb_13_ModAquaviario.Height := 92;
    end;
    mdFerroviario:
    begin
      rlb_14_ModFerroviario.Height := 0;
    end;
    mdDutoviario:
    begin
      rlb_15_ModDutoviario.Height := 0;
    end;
  end;

  RLCTe.Title := 'CT-e: ' + FormatFloat('000,000,000', FCTe.Ide.nCT);

  //RLCTe.Margins.TopMargin := FMargemSuperior * 100;
  //RLCTe.Margins.BottomMargin := FMargemInferior * 100;
  //RLCTe.Margins.LeftMargin := FMargemEsquerda * 100;
  //RLCTe.Margins.RightMargin := FMargemDireita * 100;
end;

end.
