{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
{                              Mark dos Santos Gonçalves                       }
{                              Juliomar Marchetti                              }
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
  DB, StrUtils,
  RLReport, RLFilters, RLPDFFilter, RLMetaFile, RLFeedBack, RLParser,
  RLConsts, RLUtils, RLTypes, RLRichText, RLBarcode, RLPrintDialog, RLPrinters,
  pcnConversao,
  ACBrCTeDACTeRL;

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
    rllTituloCNPJ1: TRLLabel;
    rllTituloSerie1: TRLLabel;
    rllTituloSerie2: TRLLabel;
    rllTituloCNPJ2: TRLLabel;
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
    RLDraw48: TRLDraw;
    RLDraw49: TRLDraw;
    rlLabel3: TRLLabel;
    rlLabel132: TRLLabel;
    rlLabel133: TRLLabel;
    rlLabel134: TRLLabel;
    rlLabel135: TRLLabel;
    rlLabel136: TRLLabel;
    rllRecebemosDe: TRLLabel;
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
    RLDraw98: TRLDraw;
    rlmCNPJPg: TRLMemo;
    RLDraw88: TRLDraw;
    rllVariavel1: TRLLabel;
    RLDraw99: TRLDraw;
    rlmQtdUnidMedida4: TRLMemo;
    rlLabel73: TRLLabel;
    RLDraw100: TRLDraw;
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
    rlbCodigoBarras: TRLBarcode;
    RLDraw51: TRLDraw;
    RLDraw52: TRLDraw;
    RLDraw50: TRLDraw;
    rllVariavel2: TRLLabel;
    rlmComplChave1: TRLMemo;
    rlmComplValor1: TRLMemo;
    rlmComplChave2: TRLMemo;
    rlmComplValor2: TRLMemo;
    rlDocOrig_tpDoc1: TRLMemo;
    rlDocOrig_tpDoc2: TRLMemo;
    RLLabel199: TRLLabel;
    RLLabel200: TRLLabel;
    rlsQuadro3: TRLDraw;
    RLSystemInfo1: TRLSystemInfo;
    rlb_06_VeiculosNovos: TRLBand;
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
    rlb_Cte_Anulado_Substituido: TRLBand;
    RLLabel9: TRLLabel;
    RLDraw117: TRLDraw;
    RLDraw118: TRLDraw;
    rlblChaveCteSubstituido: TRLLabel;
    rlblChaveCteAnulacao: TRLLabel;
    rlChaveCteSerAnulSubst: TRLMemo;
    rlChaveCteAnulacao: TRLMemo;
    RLMemo1: TRLMemo;
    rlsLinhaV07: TRLDraw;
    imgQRCode: TRLImage;
    RLDraw10: TRLDraw;
    RLDraw46: TRLDraw;
    rlb_01_ReciboBarra: TRLBand;
    RLBarraBarcode: TRLBarcode;
    rlBarraDataRecebimento: TRLLabel;
    rlBarraiCanhoto: TRLDraw;
    rlBarraiCanhoto1: TRLDraw;
    rlBarraiCanhoto2: TRLDraw;
    rlBarraiCanhoto3: TRLDraw;
    rlBarraIdentificacao: TRLLabel;
    rlBarramDadosAdicionaisAuxiliar: TRLMemo;
    rlBarraNumero: TRLLabel;
    RLBarraRecebemosDe: TRLMemo;
    RLBarraResumo: TRLMemo;
    rlBarraSERIE: TRLLabel;
    rllBarraCTe: TRLLabel;
    rlb_DivisaoRecibo: TRLBand;
    rliDivisao: TRLDraw;

    procedure rlb_01_ReciboBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlb_02_CabecalhoBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlb_03_DadosDACTeBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlb_03_DadosDACTe_OSBeforePrint(Sender: TObject;
      var PrintIt: boolean);
    procedure rlb_04_DadosNotaFiscalBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlb_05_ComplementoBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlb_06_ValorPrestacaoBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlb_07_HeaderItensBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlb_09_ObsBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlb_10_ModRodFracionadoBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlb_11_ModRodLot103BeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlb_17_SistemaBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlb_16_DadosExcEmitenteBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlb_12_ModAereoBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlb_13_ModAquaviarioBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlb_14_ModFerroviarioBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlb_15_ModDutoviarioBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlb_01_Recibo_AereoBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlb_11_ModRodLot104BeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlb_06_ProdutosPerigososBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlb_06_VeiculosNovosBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlb_CTeOS_PrestacaoServicoBeforePrint(Sender: TObject;
      var PrintIt: boolean);
    procedure rlb_Dados_SeguradoraBeforePrint(Sender: TObject;
      var PrintIt: boolean);
    procedure rlb_Fluxo_CargaBeforePrint(Sender: TObject; var PrintIt: boolean);
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
    procedure rlb_Cte_Anulado_SubstituidoBeforePrint(Sender: TObject;
      var PrintIt: Boolean);
    procedure rlb_01_ReciboBarraBeforePrint(Sender: TObject;
      var PrintIt: Boolean);

    procedure posicionaCanhoto;
    function CanhotoDesabilita(Value: TRLBand): TfrmDACTeRLRetrato;
    function ConfigurarCanhotoBarra: TfrmDACTeRLRetrato;
    Function ConfigurarRLBarcode:TfrmDACTeRLRetrato ;
    function Canhoto(Value: TRLBand): TfrmDACTeRLRetrato;
    procedure rlb_DivisaoReciboBeforePrint(Sender: TObject;
      var PrintIt: Boolean);
    procedure rlb_06_ValorPrestacaoAfterPrint(Sender: TObject);
  private
    Linhas: integer;
    rlb_07_HeaderItensPrinted: Boolean;
    rlb_06_ValorPrestacaoPrinted: Boolean;

    procedure Itens;
    procedure DefinirAltura;
  public
    constructor Create(TheOwner: TComponent); override;

    procedure ProtocoloCTe(const sProtocolo: string);
  end;

implementation

uses
  DateUtils,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrUtil.DateTime,
  ACBrValidador, ACBrImage, ACBrDelphiZXingQRCode,
  ACBrCTe, pcteConversaoCTe,
  ACBrDFeUtil, ACBrDFeReportFortes;

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

var
  FProtocoloCTe: string;
  Versao: integer;

constructor TfrmDACTeRLRetrato.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  RLCTe.PageSetup.PaperSize := fpA4;
  RLCTe.PageSetup.PaperHeight := 297.0;
  RLCTe.PageSetup.PaperWidth := 210.0;
  rlb_07_HeaderItensPrinted := False;
  rlb_06_ValorPrestacaoPrinted := False;
end;

procedure TfrmDACTeRLRetrato.Itens;
var
  I, J, K, Item: integer;
begin
  if RLCTe.PageNumber > 0 then
    exit;

  Item := 0;

  if (fpCTe.infCTeNorm.infDoc.infNF.Count > 0) or
     (fpCTe.infCTeNorm.docAnt.emiDocAnt.Count > 0) then
  begin
    rllTituloCNPJ1.Caption := 'CNPJ/CPF EMITENTE';
    rllTituloCNPJ2.Caption := 'CNPJ/CPF EMITENTE';
    rllTituloSerie1.Caption := ACBrStr('SÉRIE/NRO. DOCUMENTO');
    rllTituloSerie2.Caption := ACBrStr('SÉRIE/NRO. DOCUMENTO');
  end;

  if (fpCTe.infCTeNorm.infDoc.infNFe.Count > 0) or
     (fpCTe.infCTeNorm.docAnt.emiDocAnt.Count > 0) then
  begin
    rllTituloCNPJ1.Caption := 'CHAVE DO DF-e';
    rllTituloCNPJ2.Caption := 'CHAVE DO DF-e';
    rllTituloSerie1.Caption := '';
    rllTituloSerie2.Caption := '';
  end;

  if fpCTe.infCTeNorm.infDoc.InfOutros.Count > 0 then
  begin
    rllTituloCNPJ1.Caption := 'CNPJ/CPF EMITENTE';
    rllTituloCNPJ2.Caption := 'CNPJ/CPF EMITENTE';
    rllTituloSerie1.Caption := '';
    rllTituloSerie2.Caption := '';
  end;

  //Varrendo NF comum
  for I := 0 to (fpCTe.infCTeNorm.infDoc.infNF.Count - 1) do
  begin
    with fpCTe.infCTeNorm.infDoc.InfNF.Items[I] do
    begin
      if (Item mod 2) = 0 then
      begin
        cdsDocumentos.Append;

        cdsDocumentos.FieldByName('TIPO_1').AsString := 'NF';
        cdsDocumentos.FieldByName('CNPJCPF_1').AsString :=
          FormatarCNPJouCPF(fpCTe.Rem.CNPJCPF);
        cdsDocumentos.FieldByName('DOCUMENTO_1').AsString := serie + '-' + nDoc;
      end
      else
      begin
        cdsDocumentos.FieldByName('TIPO_2').AsString := 'NF';
        cdsDocumentos.FieldByName('CNPJCPF_2').AsString :=
          FormatarCNPJouCPF(fpCTe.Rem.CNPJCPF);
        cdsDocumentos.FieldByName('DOCUMENTO_2').AsString := serie + '-' + nDoc;

        cdsDocumentos.Post;
      end;
      Inc(Item);
    end;
  end;

  //Varrendo NFe
  for I := 0 to (fpCTe.infCTeNorm.infDoc.InfNFE.Count - 1) do
  begin
    with fpCTe.infCTeNorm.infDoc.InfNFE.Items[I] do
    begin
      if (Item mod 2) = 0 then
      begin
        cdsDocumentos.Append;
        cdsDocumentos.FieldByName('TIPO_1').AsString := 'NF-E ' + copy(chave, 26, 9);
        cdsDocumentos.FieldByName('CNPJCPF_1').AsString := FormatarChaveAcesso(chave);
        cdsDocumentos.FieldByName('DOCUMENTO_1').AsString := '';
      end
      else
      begin
        cdsDocumentos.FieldByName('TIPO_2').AsString := 'NF-E ' + copy(chave, 26, 9);
        cdsDocumentos.FieldByName('CNPJCPF_2').AsString := FormatarChaveAcesso(chave);
        cdsDocumentos.FieldByName('DOCUMENTO_2').AsString := '';
        cdsDocumentos.Post;
      end;
      Inc(Item);
    end;
  end;

  //Varrendo Outros
  for I := 0 to (fpCTe.infCTeNorm.infDoc.InfOutros.Count - 1) do
  begin
    with fpCTe.infCTeNorm.infDoc.InfOutros.Items[I] do
    begin
      if (Item mod 2) = 0 then
      begin
        cdsDocumentos.Append;
        // TpcteTipoDocumento = (tdDeclaracao, tdDutoviario, tdCFeSAT, tdNFCe, tdOutros);
        case tpDoc of
          tdDeclaracao:
          begin
            cdsDocumentos.FieldByName('TIPO_1').AsString := 'DECLAR';
            cdsDocumentos.FieldByName('CNPJCPF_1').AsString :=
              FormatarCNPJouCPF(fpCTe.Rem.CNPJCPF);
            cdsDocumentos.FieldByName('DOCUMENTO_1').AsString :=
              ACBrStr('N. Doc.: ') + nDoc;
          end;
          tdCFeSAT:
          begin
            cdsDocumentos.FieldByName('TIPO_1').AsString := 'CF-e SAT ';
            cdsDocumentos.FieldByName('CNPJCPF_1').AsString :=
              FormatarCNPJouCPF(fpCTe.Rem.CNPJCPF);
            cdsDocumentos.FieldByName('DOCUMENTO_1').AsString :=
              ACBrStr('CF-e SAT.: ') + nDoc;
          end;
          tdNFCe:
          begin
            cdsDocumentos.FieldByName('TIPO_1').AsString :=
              'NFC-e ' + copy(trim(descOutros), 26, 9);
            cdsDocumentos.FieldByName('CNPJCPF_1').AsString :=
              FormatarChaveAcesso(trim(descOutros));
            cdsDocumentos.FieldByName('DOCUMENTO_1').AsString := '';
          end;
          tdDutoviario:
          begin
            cdsDocumentos.FieldByName('TIPO_1').AsString := 'DUTO';
            cdsDocumentos.FieldByName('CNPJCPF_1').AsString :=
              FormatarCNPJouCPF(fpCTe.Rem.CNPJCPF);
            cdsDocumentos.FieldByName('DOCUMENTO_1').AsString :=
              ACBrStr('Dutoviário Doc.: ') + nDoc;
          end;
          tdOutros:
          begin
            cdsDocumentos.FieldByName('TIPO_1').AsString := 'Outros';
            cdsDocumentos.FieldByName('CNPJCPF_1').AsString :=
              FormatarCNPJouCPF(fpCTe.Rem.CNPJCPF);
            cdsDocumentos.FieldByName('DOCUMENTO_1').AsString :=
              copy(trim(descOutros), 1, 25) + ' Doc.: ' + nDoc;
          end;

        end;
      end
      else
      begin
        case tpDoc of
          tdDeclaracao:
          begin
            cdsDocumentos.FieldByName('TIPO_2').AsString := 'DECLAR';
            cdsDocumentos.FieldByName('CNPJCPF_2').AsString :=
              FormatarCNPJouCPF(fpCTe.Rem.CNPJCPF);
            cdsDocumentos.FieldByName('DOCUMENTO_2').AsString :=
              ACBrStr('N. Doc.: ') + nDoc;
          end;
          tdDutoviario:
          begin
            cdsDocumentos.FieldByName('TIPO_2').AsString := 'DUTO';
            cdsDocumentos.FieldByName('CNPJCPF_2').AsString :=
              FormatarCNPJouCPF(fpCTe.Rem.CNPJCPF);
            cdsDocumentos.FieldByName('DOCUMENTO_2').AsString :=
              ACBrStr('Dutoviário Doc.: ') + nDoc;
          end;
          tdCFeSAT:
          begin
            cdsDocumentos.FieldByName('TIPO_2').AsString := 'CF-e SAT ';
            cdsDocumentos.FieldByName('CNPJCPF_2').AsString :=
              FormatarCNPJouCPF(fpCTe.Rem.CNPJCPF);
            cdsDocumentos.FieldByName('DOCUMENTO_2').AsString :=
              ACBrStr('CF-e SAT.: ') + nDoc;
          end;
          tdNFCe:
          begin
            cdsDocumentos.FieldByName('TIPO_2').AsString :=
              'NFC-E ' + copy(trim(descOutros), 26, 9);
            cdsDocumentos.FieldByName('CNPJCPF_2').AsString :=
              FormatarChaveAcesso(trim(descOutros));
            cdsDocumentos.FieldByName('DOCUMENTO_2').AsString :=  '';
          end;
          tdOutros:
          begin
            cdsDocumentos.FieldByName('TIPO_2').AsString := 'Outros';
            cdsDocumentos.FieldByName('CNPJCPF_2').AsString :=
              FormatarCNPJouCPF(fpCTe.Rem.CNPJCPF);
            cdsDocumentos.FieldByName('DOCUMENTO_2').AsString :=
              copy(trim(descOutros), 1, 25) + ' Doc.: ' + nDoc;
          end;
        end;
        cdsDocumentos.Post;
      end;
      Inc(Item);
    end;
  end;

  //Varrendo Documentos de Transporte anterior
  for I := 0 to (fpCTe.infCTeNorm.docAnt.emiDocAnt.Count - 1) do
  begin
    // Em Papel
    for J := 0 to (fpCTe.infCTeNorm.docAnt.emiDocAnt.Items[I].idDocAnt.Count - 1) do
    begin
      for K := 0 to (fpCTe.infCTeNorm.docAnt.emiDocAnt.Items[I].idDocAnt.Items[
          J].idDocAntPap.Count - 1) do
      begin
        with fpCTe.infCTeNorm.docAnt.emiDocAnt.Items[I].idDocAnt.Items[
            J].idDocAntPap.Items[K] do
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
              FormatarCNPJouCPF(fpCTe.infCTeNorm.docAnt.emiDocAnt.Items[I].CNPJCPF);
            cdsDocumentos.FieldByName('DOCUMENTO_1').AsString :=
              serie + '-' + nDoc;
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
              FormatarCNPJouCPF(fpCTe.infCTeNorm.docAnt.emiDocAnt.Items[I].CNPJCPF);
            cdsDocumentos.FieldByName('DOCUMENTO_2').AsString :=
              serie + '-' + nDoc;

            cdsDocumentos.Post;
          end;
          Inc(Item);
        end;
      end;
    end;

    // Eletrônico
    for J := 0 to (fpCTe.infCTeNorm.docAnt.emiDocAnt.Items[I].idDocAnt.Count - 1) do
    begin
      for K := 0 to (fpCTe.infCTeNorm.docAnt.emiDocAnt.Items[I].idDocAnt.Items[
          J].idDocAntEle.Count - 1) do
      begin
        with fpCTe.infCTeNorm.docAnt.emiDocAnt.Items[I].idDocAnt.Items[
            J].idDocAntEle.Items[K] do
        begin
          if (Item mod 2) = 0 then
          begin
            cdsDocumentos.Append;

            cdsDocumentos.FieldByName('TIPO_1').AsString := 'CT-E';

            if fpCTe.infCTe.versao >= 3 then
              cdsDocumentos.FieldByName('CNPJCPF_1').AsString :=
                FormatarChaveAcesso(chCTe)
            else
              cdsDocumentos.FieldByName('CNPJCPF_1').AsString :=
                FormatarChaveAcesso(chave);

            cdsDocumentos.FieldByName('DOCUMENTO_1').AsString := '';
          end
          else
          begin
            cdsDocumentos.FieldByName('TIPO_2').AsString := 'CT-E';
            if fpCTe.infCTe.versao >= 3 then
              cdsDocumentos.FieldByName('CNPJCPF_2').AsString :=
                FormatarChaveAcesso(chCTe)
            else
              cdsDocumentos.FieldByName('CNPJCPF_2').AsString :=
                FormatarChaveAcesso(chave);

            cdsDocumentos.FieldByName('DOCUMENTO_2').AsString := '';
            cdsDocumentos.Post;
          end;
          Inc(Item);
        end;
      end;
    end;

  end;

  cdsDocumentos.First;
end;

procedure TfrmDACTeRLRetrato.ProtocoloCTe(const sProtocolo: string);
begin
  FProtocoloCTe := sProtocolo;
end;

procedure TfrmDACTeRLRetrato.rlb_01_ReciboBarraBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  PrintIt := (RLCTe.PageNumber = 1);
end;

procedure TfrmDACTeRLRetrato.rlb_01_ReciboBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  PrintIt := (RLCTe.PageNumber = 1) and (fpCTe.Ide.modal <> mdAereo);

  if (fpDACTe.ExibeResumoCanhoto) then
    rllResumoCanhotoCTe.Caption := GetTextoResumoCanhoto
  else
    rllResumoCanhotoCTe.Caption := '';

  rllSerie2.Caption := IntToStr(fpCTe.Ide.serie); // FormatFloat( '000', fpCTe.Ide.serie);
  rllNumCte2.Caption := FormatFloat('000,000,000', fpCTe.Ide.nCT);
  // TpcteTipoCTe = (tcNormal, tcComplemento, tcAnulacao, tcSubstituto);
  rlb_01_Recibo.Enabled := (fpCTe.Ide.tpCTe = tcNormal) or
    (fpCTe.Ide.tpCTe = tcComplemento);

  if (fpCTe.ide.modelo = 67) then
    rlLabel140.Caption := 'CT-E OS';
end;

procedure TfrmDACTeRLRetrato.rlb_01_Recibo_AereoBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  PrintIt := (RLCTe.PageNumber = 1) and (fpCTe.Ide.modal = mdAereo);

  rlb_01_Recibo_Aereo.Enabled :=
    (fpCTe.Ide.tpCTe = tcNormal) or (fpCTe.Ide.tpCTe = tcComplemento);
end;

procedure TfrmDACTeRLRetrato.rlb_02_CabecalhoBeforePrint(Sender: TObject;
  var PrintIt: boolean);
var
  CarregouLogo: Boolean;
  strChaveContingencia: string;
begin
  CarregouLogo := TDFeReportFortes.CarregarLogo(rliLogo, fpDACTe.Logo);

  if not CarregouLogo then
  begin
    rlmDadosEmitente.Left := rlmEmitente.Left;
    rlmDadosEmitente.Width := rlmEmitente.Width;
  end;

  if fpDACTe.ExpandeLogoMarca then
  begin
    rlmEmitente.Visible := False;
    rlmDadosEmitente.Visible := False;
    rliLogo.top := 3;
    rliLogo.Left := 2;
    rliLogo.Height := 121;
    rliLogo.Width := 311;

    TDFeReportFortes.AjustarLogo(rliLogo, fpDACTe.ExpandeLogoMarcaConfig);
  end
  else
  begin
    rlmEmitente.Enabled := True;
    rlmDadosEmitente.Enabled := True;
    // Emitente
    with fpCTe.Emit do
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

      if Trim(fpDACTe.Site) <> '' then
        rlmDadosEmitente.Lines.Add('SITE: ' + fpDACTe.Site);
      if Trim(fpDACTe.Email) <> '' then
        rlmDadosEmitente.Lines.Add('E-MAIL: ' + fpDACTe.Email);
    end;
  end;

  rllModal.Caption := ACBrStr(TpModalToStrText(fpCTe.Ide.modal));
  rllModelo.Caption := IntToStr(fpCTe.Ide.modelo);
  rllEmissao.Caption := FormatDateTimeBr(fpCTe.Ide.dhEmi);
  rllChave.Caption := FormatarChaveAcesso(OnlyNumber(fpCTe.InfCTe.Id));

  rllTipoCte.Caption := ACBrStr(tpCTToStrText(fpCTe.Ide.tpCTe));
  rllTipoServico.Caption := ACBrStr(TpServToStrText(fpCTe.Ide.tpServ));

  // CTe 3.0 **************************************************************

  if (fpCTe.infCTe.versao >= 3.00) then   //Campos referente a versão 3.00
    cabecalhoVersao30()
  else                                   //Campos referente a versão 2.00
  begin
    if fpCTe.Ide.Toma4.xNome = '' then
      rllTomaServico.Caption := TpTomadorToStrText(fpCTe.Ide.Toma03.Toma)
    else
      rllTomaServico.Caption := TpTomadorToStrText(fpCTe.Ide.Toma4.toma);

    rllFormaPagamento.Caption := tpforPagToStrText(fpCTe.Ide.forPag);
  end;

  // Normal **************************************************************
  if fpCTe.Ide.tpEmis in [teNormal, teSCAN, teSVCSP, teSVCRS] then
  begin
    rllVariavel1.Visible := True;
    rllVariavel2.Visible := True;
    RLBarcode1.Visible := False;

    if fpCTe.procCTe.cStat = 100 then
      rllDescricao.Caption := ACBrStr('PROTOCOLO DE AUTORIZAÇÃO DE USO');

    if fpCTe.procCTe.cStat = 101 then
      rllDescricao.Caption := ACBrStr('PROTOCOLO DE HOMOLOGAÇÃO DE CANCELAMENTO');

    if fpCTe.procCTe.cStat = 110 then
      rllDescricao.Caption := ACBrStr('PROTOCOLO DE DENEGAÇÃO DE USO');

    if FProtocoloCTE <> '' then
      rllProtocolo.Caption := FProtocoloCTE
    else
      rllProtocolo.Caption := fpCTe.procCTe.nProt + '   ' +
        IfThen(fpCTe.procCTe.dhRecbto <> 0, DateTimeToStr(
        fpCTe.procCTe.dhRecbto), '');
  end;

  // Contingencia ********************************************************
  if fpCTe.Ide.tpEmis in [teContingencia, teFSDA] then
  begin
    if fpCTe.procCTe.cStat in [100, 101, 110] then
    begin
      rllVariavel1.Visible := True;
      rllVariavel2.Visible := True;
      RLBarcode1.Visible := False;

      if fpCTe.procCTe.cStat = 100 then
        rllDescricao.Caption := ACBrStr('PROTOCOLO DE AUTORIZAÇÃO DE USO');

      if fpCTe.procCTe.cStat = 101 then
        rllDescricao.Caption := ACBrStr('PROTOCOLO DE HOMOLOGAÇÃO DE CANCELAMENTO');

      if fpCTe.procCTe.cStat = 110 then
        rllDescricao.Caption := ACBrStr('PROTOCOLO DE DENEGAÇÃO DE USO');

      if FProtocoloCTE <> '' then
        rllProtocolo.Caption := FProtocoloCTE
      else
        rllProtocolo.Caption :=
          fpCTe.procCTe.nProt + '   ' + IfThen(fpCTe.procCTe.dhRecbto <>
          0, DateTimeToStr(fpCTe.procCTe.dhRecbto), '');
    end
    else
    begin
      rllVariavel1.Visible := False;
      rllVariavel2.Visible := False;
      RLBarcode1.Visible := True;

      strChaveContingencia := fpACBrCTe.GerarChaveContingencia(fpCTe);
      RLBarcode1.Caption := strChaveContingencia;
      rllDescricao.Caption := 'DADOS DO CT-E';
      rllProtocolo.Caption := FormatarChaveAcesso(strChaveContingencia);
    end;
  end;

  // EPEC ****************************************************************
  if fpCTe.Ide.tpEmis = teDPEC then
  begin
    rllVariavel1.Visible := False;
    rllVariavel2.Visible := False;
    RLBarcode1.Visible := True;

    strChaveContingencia := fpACBrCTe.GerarChaveContingencia(fpCTe);
    RLBarcode1.Caption := strChaveContingencia;
    rllDescricao.Caption := 'DADOS DO CT-E';
    rllProtocolo.Caption := FormatarChaveAcesso(strChaveContingencia);
  end;

  rllInscSuframa.Caption := fpCTe.Dest.ISUF;

  if (fpCTe.ide.modelo = 67) then
  begin
    RLLabel17.Caption := 'DACTE OS';
    RLLabel18.Caption := 'Documento Auxiliar do Conhecimento de Transporte Eletrônico para Outros Serviços';
  end;
end;

procedure TfrmDACTeRLRetrato.rlb_03_DadosDACTeBeforePrint(Sender: TObject;
  var PrintIt: boolean);
var
  i: integer;
begin
  rlb_03_DadosDACTe.Enabled := not (fpCTe.ide.modelo = 67);

  if not (rlb_03_DadosDACTe.Enabled) then
    rlb_03_DadosDACTe.Height := 0;

  PrintIt := RLCTe.PageNumber = 1;

  rllNatOperacao.Caption := FormatFloat('0000', fpCTe.Ide.CFOP) + ' - ' + fpCTe.Ide.natOp;
  rllOrigPrestacao.Caption := fpCTe.Ide.xMunIni + ' - ' + fpCTe.Ide.UFIni +
    ' - ' + FormatFloat('000', fpCTe.Ide.cMunIni);
  rllDestPrestacao.Caption := fpCTe.Ide.xMunFim + ' - ' + fpCTe.Ide.UFFim +
    ' - ' + FormatFloat('000', fpCTe.Ide.cMunFim);

  //DADOS REMETENTE
  rllRazaoRemet.Caption := fpCTe.Rem.xNome;
  rllEnderecoRemet1.Caption := fpCTe.Rem.EnderReme.xLgr + ', ' + fpCTe.Rem.EnderReme.nro;
  rllEnderecoRemet2.Caption :=
    fpCTe.Rem.EnderReme.xCpl + ' - ' + fpCTe.Rem.EnderReme.xBairro;
  rllCEPRemet.Caption := FormatarCEP(fpCTe.Rem.EnderReme.CEP);
  rllMunRemet.Caption := fpCTe.Rem.EnderReme.xMun + ' - ' + fpCTe.Rem.EnderReme.UF;
  rllCnpjRemet.Caption := FormatarCNPJouCPF(fpCTe.Rem.CNPJCPF);
  rllPaisRemet.Caption := fpCTe.Rem.EnderReme.xPais;
  rllInscEstRemet.Caption := fpCTe.Rem.IE;
  rllFoneRemet.Caption := FormatarFone(fpCTe.Rem.fone);

  //DADOS DESTINATARIO
  rllRazaoDest.Caption := fpCTe.Dest.xNome;
  rllEnderecoDest1.Caption := fpCTe.Dest.EnderDest.xLgr + ', ' + fpCTe.Dest.EnderDest.nro;
  rllEnderecoDest2.Caption := fpCTe.Dest.EnderDest.xCpl + ' - ' +
    fpCTe.Dest.EnderDest.xBairro;
  rllCEPDest.Caption := FormatarCEP(fpCTe.Dest.EnderDest.CEP);
  rllMunDest.Caption := fpCTe.Dest.EnderDest.xMun + ' - ' + fpCTe.Dest.EnderDest.UF;
  rllCnpjDest.Caption := FormatarCNPJouCPF(fpCTe.Dest.CNPJCPF);
  rllPaisDest.Caption := fpCTe.Dest.EnderDest.xPais;
  rllInscEstDest.Caption := fpCTe.Dest.IE;
  rllFoneDest.Caption := FormatarFone(fpCTe.Dest.fone);

  //DADOS EXPEDIDOR
  if fpCTe.Exped.xNome <> '' then
  begin
    rllRazaoExped.Caption := fpCTe.Exped.xNome;
    rllEnderecoExped1.Caption :=
      fpCTe.Exped.EnderExped.xLgr + ', ' + fpCTe.Exped.EnderExped.nro;
    rllEnderecoExped2.Caption :=
      fpCTe.Exped.EnderExped.xCpl + ' - ' + fpCTe.Exped.EnderExped.xBairro;
    rllCEPExped.Caption := FormatarCEP(fpCTe.Exped.EnderExped.CEP);
    rllMunExped.Caption := fpCTe.Exped.EnderExped.xMun + ' - ' + fpCTe.Exped.EnderExped.UF;
    rllCnpjExped.Caption := FormatarCNPJouCPF(fpCTe.Exped.CNPJCPF);
    rllPaisExped.Caption := fpCTe.Exped.EnderExped.xPais;
    rllInscEstExped.Caption := fpCTe.Exped.IE;
    rllFoneExped.Caption := FormatarFone(fpCTe.Exped.fone);
  end;

  //DADOS RECEBEDOR
  if fpCTe.Receb.xNome <> '' then
  begin
    rllRazaoReceb.Caption := fpCTe.Receb.xNome;
    rllEnderecoReceb1.Caption :=
      fpCTe.Receb.EnderReceb.xLgr + ', ' + fpCTe.Receb.EnderReceb.nro;
    rllEnderecoReceb2.Caption :=
      fpCTe.Receb.EnderReceb.xCpl + ' - ' + fpCTe.Receb.EnderReceb.xBairro;
    rllCEPReceb.Caption := FormatarCEP(fpCTe.Receb.EnderReceb.CEP);
    rllMunReceb.Caption := fpCTe.Receb.EnderReceb.xMun + ' - ' + fpCTe.Receb.EnderReceb.UF;
    rllCnpjReceb.Caption := FormatarCNPJouCPF(fpCTe.Receb.CNPJCPF);
    rllPaisReceb.Caption := fpCTe.Receb.EnderReceb.xPais;
    rllInscEstReceb.Caption := fpCTe.Receb.IE;
    rllFoneReceb.Caption := FormatarFone(fpCTe.Receb.fone);
  end;

  if fpCTe.Ide.Toma4.xNome = '' then
  begin
    case fpCTe.Ide.Toma03.Toma of
      tmRemetente:
      begin
        rllRazaoToma.Caption := fpCTe.Rem.xNome;
        rllEnderecoToma.Caption :=
          fpCTe.Rem.EnderReme.xLgr + ', ' + fpCTe.Rem.EnderReme.nro + ' - ' +
          fpCTe.Rem.EnderReme.xCpl + ' - ' + fpCTe.Rem.EnderReme.xBairro;
        rllCEPToma.Caption := FormatarCEP(fpCTe.Rem.EnderReme.CEP);
        rllMunToma.Caption := fpCTe.Rem.EnderReme.xMun + ' - ' + fpCTe.Rem.EnderReme.UF;
        rllCnpjToma.Caption := FormatarCNPJouCPF(fpCTe.Rem.CNPJCPF);
        rllPaisToma.Caption := fpCTe.Rem.EnderReme.xPais;
        rllInscEstToma.Caption := fpCTe.Rem.IE;
        rllFoneToma.Caption := FormatarFone(fpCTe.Rem.fone);
      end;
      tmExpedidor:
      begin
        rllRazaoToma.Caption := fpCTe.Exped.xNome;
        rllEnderecoToma.Caption :=
          fpCTe.Exped.EnderExped.xLgr + ', ' + fpCTe.Exped.EnderExped.nro +
          ' - ' + fpCTe.Exped.EnderExped.xCpl + ' - ' + fpCTe.Exped.EnderExped.xBairro;
        rllCEPToma.Caption := FormatarCEP(fpCTe.Exped.EnderExped.CEP);
        rllMunToma.Caption := fpCTe.Exped.EnderExped.xMun + ' - ' +
          fpCTe.Exped.EnderExped.UF;
        rllCnpjToma.Caption := FormatarCNPJouCPF(fpCTe.Exped.CNPJCPF);
        rllPaisToma.Caption := fpCTe.Exped.EnderExped.xPais;
        rllInscEstToma.Caption := fpCTe.Exped.IE;
        rllFoneToma.Caption := FormatarFone(fpCTe.Exped.fone);
      end;
      tmRecebedor:
      begin
        rllRazaoToma.Caption := fpCTe.Receb.xNome;
        rllEnderecoToma.Caption :=
          fpCTe.Receb.EnderReceb.xLgr + ', ' + fpCTe.Receb.EnderReceb.nro +
          ' - ' + fpCTe.Receb.EnderReceb.xCpl + ' - ' + fpCTe.Receb.EnderReceb.xBairro;
        rllCEPToma.Caption := FormatarCEP(fpCTe.Receb.EnderReceb.CEP);
        rllMunToma.Caption := fpCTe.Receb.EnderReceb.xMun + ' - ' +
          fpCTe.Receb.EnderReceb.UF;
        rllCnpjToma.Caption := FormatarCNPJouCPF(fpCTe.Receb.CNPJCPF);
        rllPaisToma.Caption := fpCTe.Receb.EnderReceb.xPais;
        rllInscEstToma.Caption := fpCTe.Receb.IE;
        rllFoneToma.Caption := FormatarFone(fpCTe.Receb.fone);
      end;
      tmDestinatario:
      begin
        rllRazaoToma.Caption := fpCTe.Dest.xNome;
        rllEnderecoToma.Caption :=
          fpCTe.Dest.EnderDest.xLgr + ', ' + fpCTe.Dest.EnderDest.nro + ' - ' +
          fpCTe.Dest.EnderDest.xCpl + ' - ' + fpCTe.Dest.EnderDest.xBairro;
        rllCEPToma.Caption := FormatarCEP(fpCTe.Dest.EnderDest.CEP);
        rllMunToma.Caption := fpCTe.Dest.EnderDest.xMun + ' - ' + fpCTe.Dest.EnderDest.UF;
        rllCnpjToma.Caption := FormatarCNPJouCPF(fpCTe.Dest.CNPJCPF);
        rllPaisToma.Caption := fpCTe.Dest.EnderDest.xPais;
        rllInscEstToma.Caption := fpCTe.Dest.IE;
        rllFoneToma.Caption := FormatarFone(fpCTe.Dest.fone);
      end;
    end;
  end
  else
  begin
    if fpCTe.Ide.Toma4.xNome <> '' then
    begin
      rllRazaoToma.Caption := fpCTe.Ide.Toma4.xNome;
      rllEnderecoToma.Caption :=
        fpCTe.Ide.Toma4.EnderToma.xLgr + ', ' + fpCTe.Ide.Toma4.EnderToma.nro +
        ' - ' + fpCTe.Ide.Toma4.EnderToma.xCpl + ' - ' + fpCTe.Ide.Toma4.EnderToma.xBairro;
      rllCEPToma.Caption := FormatarCEP(fpCTe.Ide.Toma4.EnderToma.CEP);
      rllMunToma.Caption := fpCTe.Ide.Toma4.EnderToma.xMun + ' - ' +
        fpCTe.Ide.Toma4.EnderToma.UF;
      rllCnpjToma.Caption := FormatarCNPJouCPF(fpCTe.Ide.Toma4.CNPJCPF);
      rllPaisToma.Caption := fpCTe.Ide.Toma4.EnderToma.xPais;
      rllInscEstToma.Caption := fpCTe.Ide.Toma4.IE;
      rllFoneToma.Caption := FormatarFone(fpCTe.Ide.Toma4.fone);
    end;
  end;

  rllProdPredominante.Caption := fpCTe.inFCTeNorm.infCarga.proPred;
  rllOutrasCaracCarga.Caption := fpCTe.infCTeNorm.InfCarga.xOutCat;
  rllVlrTotalMerc.Caption := FormatFloatBr(msk15x2, fpCTe.infCTeNorm.infCarga.vCarga);

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

  for i := 0 to (fpCTe.infCTeNorm.InfCarga.InfQ.Count - 1) do
  begin
    case fpCTe.infCTeNorm.InfCarga.InfQ.Items[i].cUnid of
      uM3: rlmQtdUnidMedida4.Lines.Add(FormatFloatBr(msk6x4,
          fpCTe.infCTeNorm.InfCarga.InfQ.Items[i].qCarga));
      uKg:
      begin
        if uppercase(trim(fpCTe.infCTeNorm.InfCarga.InfQ.Items[i].tpMed)) =
          'PESO BRUTO' then
          rlmQtdUnidMedida1.Lines.Add(FormatFloatBr(msk6x4,
            fpCTe.infCTeNorm.InfCarga.InfQ.Items[i].qCarga))
        else
        if uppercase(trim(fpCTe.infCTeNorm.InfCarga.InfQ.Items[i].tpMed)) =
          'PESO BASE DE CALCULO' then
          rlmQtdUnidMedida2.Lines.Add(FormatFloatBr(msk6x4,
            fpCTe.infCTeNorm.InfCarga.InfQ.Items[i].qCarga))
        else
        if uppercase(trim(fpCTe.infCTeNorm.InfCarga.InfQ.Items[i].tpMed)) =
          'PESO BC' then
          rlmQtdUnidMedida2.Lines.Add(FormatFloatBr(msk6x4,
            fpCTe.infCTeNorm.InfCarga.InfQ.Items[i].qCarga))
        else
          rlmQtdUnidMedida3.Lines.Add(FormatFloatBr(msk6x4,
            fpCTe.infCTeNorm.InfCarga.InfQ.Items[i].qCarga));
      end;
      uTON:
      begin
        if uppercase(trim(fpCTe.infCTeNorm.InfCarga.InfQ.Items[i].tpMed)) =
          'PESO BRUTO' then
          rlmQtdUnidMedida1.Lines.Add(FormatFloatBr(msk6x4,
            fpCTe.infCTeNorm.InfCarga.InfQ.Items[i].qCarga * 1000))
        else
        if uppercase(trim(fpCTe.infCTeNorm.InfCarga.InfQ.Items[i].tpMed)) =
          'PESO BASE DE CALCULO' then
          rlmQtdUnidMedida2.Lines.Add(FormatFloatBr(msk6x4,
            fpCTe.infCTeNorm.InfCarga.InfQ.Items[i].qCarga * 1000))
        else
        if uppercase(trim(fpCTe.infCTeNorm.InfCarga.InfQ.Items[i].tpMed)) =
          'PESO BC' then
          rlmQtdUnidMedida2.Lines.Add(FormatFloatBr(msk6x4,
            fpCTe.infCTeNorm.InfCarga.InfQ.Items[i].qCarga * 1000))
        else
          rlmQtdUnidMedida3.Lines.Add(FormatFloatBr(msk6x4,
            fpCTe.infCTeNorm.InfCarga.InfQ.Items[i].qCarga * 1000));
      end;
      uUNIDADE, uLITROS, uMMBTU:
      begin
        rlmQtdUnidMedida5.Lines.Add(
          fpCTe.infCTeNorm.InfCarga.InfQ.Items[i].tpMed + ': ' +
          FormatFloatBr(msk6x4, fpCTe.infCTeNorm.InfCarga.InfQ.Items[i].qCarga) + ' ' +
          UnidMedToDescricaoStr(fpCTe.infCTeNorm.InfCarga.InfQ.Items[i].cUnid));
      end;
    end;
  end;

  if fpCTe.infCTeNorm.seg.Count > 0 then
  begin
    for i := 0 to fpCTe.infCTeNorm.seg.Count - 1 do
    begin
      rlmNomeSeguradora.Lines.Add(fpCTe.infCTeNorm.seg.Items[i].xSeg);
      rlmRespSeguroMerc.Lines.Add(TpRspSeguroToStrText(
        fpCTe.infCTeNorm.seg.Items[i].respSeg));
      rlmNroApolice.Lines.Add(fpCTe.infCTeNorm.seg.Items[i].nApol);
      rlmNroAverbacao.Lines.Add(fpCTe.infCTeNorm.seg.Items[i].nAver);
    end;
  end;

  for i := 0 to (fpCTe.vPrest.comp.Count - 1) do
  begin
    case i of
      0, 3, 6, 9:
      begin
        rlmCompNome1.Lines.Add(fpCTe.vPrest.comp[i].xNome);
        rlmCompValor1.Lines.Add(FormatFloatBr(msk10x2, fpCTe.vPrest.comp[i].vComp));
      end;
      1, 4, 7, 10:
      begin
        rlmCompNome2.Lines.Add(fpCTe.vPrest.comp[i].xNome);
        rlmCompValor2.Lines.Add(FormatFloatBr(msk10x2, fpCTe.vPrest.comp[i].vComp));
      end;
      2, 5, 8, 11:
      begin
        rlmCompNome3.Lines.Add(fpCTe.vPrest.comp[i].xNome);
        rlmCompValor3.Lines.Add(FormatFloatBr(msk10x2, fpCTe.vPrest.comp[i].vComp));
      end;
    end;
  end;

  rllVlrTotServico.Caption := FormatFloatBr(msk13x2, fpCTe.vPrest.vTPrest);
  rllVlrTotReceber.Caption := FormatFloatBr(msk13x2, fpCTe.vPrest.vRec);
  rllSitTrib.Caption := ACBrStr(CSTICMSToStrTagPosText(fpCTe.Imp.ICMS.SituTrib));

  case fpCTe.Imp.ICMS.SituTrib of
    cst00:
    begin
      rllBaseCalc.Caption    := FormatFloatBr(msk9x2, fpCTe.Imp.ICMS.ICMS00.vBC);
      rllAliqICMS.Caption    := FormatFloatBr(msk4x2, fpCTe.Imp.ICMS.ICMS00.pICMS);
      rllVlrICMS.Caption     := FormatFloatBr(msk4x2, fpCTe.Imp.ICMS.ICMS00.vICMS);
      rllRedBaseCalc.Caption := '';
      rllICMS_ST.Caption     := '';
    end;

    cst20:
    begin
      rllBaseCalc.Caption    := FormatFloatBr(msk9x2, fpCTe.Imp.ICMS.ICMS20.vBC);
      rllAliqICMS.Caption    := FormatFloatBr(msk4x2, fpCTe.Imp.ICMS.ICMS20.pICMS);
      rllVlrICMS.Caption     := FormatFloatBr(msk4x2, fpCTe.Imp.ICMS.ICMS20.vICMS);
      rllRedBaseCalc.Caption := FormatFloatBr(msk4x2, fpCTe.Imp.ICMS.ICMS20.pRedBC);
      rllICMS_ST.Caption     := '';
    end;

    cst40,
    cst41,
    cst45,
    cst51,
    cstICMSSN:
    begin
      rllBaseCalc.Caption    := '';
      rllAliqICMS.Caption    := '';
      rllVlrICMS.Caption     := '';
      rllRedBaseCalc.Caption := '';
      rllICMS_ST.Caption     := '';
    end;

    cst60:
    begin
      rllBaseCalc.Caption    := FormatFloatBr(msk9x2, fpCTe.Imp.ICMS.ICMS60.vBCSTRet);
      rllAliqICMS.Caption    := FormatFloatBr(msk4x2, fpCTe.Imp.ICMS.ICMS60.pICMSSTRet);
      rllRedBaseCalc.Caption := '';

      if fpCTe.infCTe.versao >= 3 then
        rllVlrICMS.Caption := FormatFloatBr(msk9x2, fpCTe.Imp.ICMS.ICMS60.vICMSSTRet)
      else
      begin
        rllVlrICMS.Caption := '';
        rllICMS_ST.Caption := FormatFloatBr(msk9x2, fpCTe.Imp.ICMS.ICMS60.vICMSSTRet);
      end;
    end;

    cst90:
    begin
      rllBaseCalc.Caption    := FormatFloatBr(msk9x2, fpCTe.Imp.ICMS.ICMS90.vBC);
      rllAliqICMS.Caption    := FormatFloatBr(msk4x2, fpCTe.Imp.ICMS.ICMS90.pICMS);
      rllVlrICMS.Caption     := FormatFloatBr(msk9x2, fpCTe.Imp.ICMS.ICMS90.vICMS);
      rllRedBaseCalc.Caption := FormatFloatBr(msk4x2, fpCTe.Imp.ICMS.ICMS90.pRedBC);
      rllICMS_ST.Caption     := '';
    end;

    cstICMSOutraUF:
    begin
      rllBaseCalc.Caption    := FormatFloatBr(msk9x2, fpCTe.Imp.ICMS.ICMSOutraUF.vBCOutraUF);
      rllAliqICMS.Caption    := FormatFloatBr(msk4x2, fpCTe.Imp.ICMS.ICMSOutraUF.pICMSOutraUF);
      rllVlrICMS.Caption     := FormatFloatBr(msk9x2, fpCTe.Imp.ICMS.ICMSOutraUF.vICMSOutraUF);
      rllRedBaseCalc.Caption := FormatFloatBr(msk4x2, fpCTe.Imp.ICMS.ICMSOutraUF.pRedBCOutraUF);
      rllICMS_ST.Caption     := '';
    end;
  end;

  if fpCTe.ide.modelo = 67 then
  begin
    rlblVlrPIS.Caption    := FormatFloatBr(msk13x2, fpCTe.imp.infTribFed.vPIS);
    rlblVlrCOFINS.Caption := FormatFloatBr(msk13x2, fpCTe.imp.infTribFed.vCOFINS);
    rlblVlrIR.Caption     := FormatFloatBr(msk13x2, fpCTe.imp.infTribFed.vIR);
    rlblVlrINSS.Caption   := FormatFloatBr(msk13x2, fpCTe.imp.infTribFed.vINSS);
    rlblVlrCSLL.Caption   := FormatFloatBr(msk13x2, fpCTe.imp.infTribFed.vCSLL);
  end;
end;

procedure TfrmDACTeRLRetrato.rlb_03_DadosDACTe_OSBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  tomadorMod67();
  PrintIt := RLCTe.PageNumber = 1;
end;

procedure TfrmDACTeRLRetrato.rlb_04_DadosNotaFiscalBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  PrintIt := RLCTe.PageNumber = 1;

  // Imprime os dados da da Nota Fiscal se o Tipo de CTe for Normal ou Substituto
  rlb_04_DadosNotaFiscal.Enabled :=
    (((fpCTe.Ide.tpCTe = tcNormal) or (fpCTe.Ide.tpCTe = tcSubstituto))
    and (fpCTe.ide.modelo <> 67));

  if not (rlb_04_DadosNotaFiscal.Enabled) then
    rlb_04_DadosNotaFiscal.Height := 0
  else
    dadosNotaFiscalVersao30();
end;

procedure TfrmDACTeRLRetrato.rlb_05_ComplementoBeforePrint(Sender: TObject;
  var PrintIt: boolean);
var
  i: Integer;
begin
  // Imprime a lista dos CT-e Complementados se o Tipo de CTe for Complemento

  rlb_05_Complemento.Enabled := (fpCTe.Ide.tpCTe = tcComplemento);

  if not rlb_05_Complemento.Enabled then
    rlb_05_Complemento.Height := 0;

  PrintIt := (RLCTe.PageNumber = 1);

  rlmComplChave1.Lines.Clear;
  rlmComplValor1.Lines.Clear;
  rlmComplChave2.Lines.Clear;
  rlmComplValor2.Lines.Clear;

  if fpCTe.infCTe.versao <= 3 then
  begin
    rlmComplChave1.Lines.Add(fpCTe.InfCTeComp.Chave);

    rlmComplValor1.Lines.Add(FormatFloatBr(msk10x2, fpCTe.vPrest.vTPrest));
  end
  else
  begin
    for i := 0 to fpCTe.infCteComp10.Count -1 do
    begin
      if (i mod 2) = 0 then
        rlmComplChave1.Lines.Add(fpCTe.infCteComp10[i].chCTe)
      else
        rlmComplChave2.Lines.Add(fpCTe.infCteComp10[i].chCTe);
    end;
  end;
end;

procedure TfrmDACTeRLRetrato.rlb_06_ValorPrestacaoAfterPrint(Sender: TObject);
begin
  inherited;
  rlb_06_ValorPrestacaoPrinted := True;
end;

procedure TfrmDACTeRLRetrato.rlb_06_ValorPrestacaoBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  PrintIt := not rlb_06_ValorPrestacaoPrinted;

  if fpCTe.infCTe.versao >= 3 then
  begin
    RLDraw26.Visible := False;
    rlLabel58.Visible := False;
    rllICMS_ST.Visible := False;
    RLDraw25.Left := 650;
    rlLabel53.Left := 654;
    rllRedBaseCalc.Left := 654;
  end
  else
  begin
    RLDraw26.Visible := True;
    rlLabel58.Visible := True;
    rllICMS_ST.Visible := True;
    RLDraw25.Left := 586;
    rlLabel53.Left := 590;
    rllRedBaseCalc.Left := 590;
  end;
end;

procedure TfrmDACTeRLRetrato.rlb_07_HeaderItensAfterPrint(Sender: TObject);
begin
  if (Linhas > 58) and (not cdsDocumentos.EOF) then
  begin
    Linhas := 0;
    rlDocOrig_tpDoc1.Height := 50;
    rlDocOrig_tpDoc2.Height := 50;
    rld_07_headerItens.Height := 81;
    RLCTe.newpage;
  end;
  rlb_07_HeaderItensPrinted := True;
end;

procedure TfrmDACTeRLRetrato.rlb_07_HeaderItensBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  rlb_07_HeaderItens.Enabled :=
    (((fpCTe.Ide.tpCTe = tcNormal) or (fpCTe.Ide.tpCTe = tcComplemento) or
    (fpCTe.Ide.tpCTe = tcSubstituto)) and (fpCTe.ide.modelo <> 67));

  if (rlb_07_HeaderItens.Enabled) then
  begin
    rlDocOrig_tpDoc1.Lines.Clear;
    rlDocOrig_tpDoc2.Lines.Clear;

    if (RLCTe.PageNumber <= 1) then
      cdsDocumentos.First
    else
    begin
      if not rlb_07_HeaderItensPrinted then
        cdsDocumentos.First;
      PrintIt := (not cdsDocumentos.EOF);
    end;

    while not cdsDocumentos.EOF do
    begin
      if cdsDocumentos.FieldByName('TIPO_1').AsString <> '' then
      begin
        if Length(cdsDocumentos.FieldByName('CNPJCPF_1').AsString) > 18 then
          rlDocOrig_tpDoc1.Lines.Add(PadRight(cdsDocumentos.FieldByName('TIPO_1').AsString,
            33, ' ') + PadRight(cdsDocumentos.FieldByName('CNPJCPF_1').AsString, 54, ' ') +
            cdsDocumentos.FieldByName('DOCUMENTO_1').AsString)
        else
          rlDocOrig_tpDoc1.Lines.Add(PadRight(cdsDocumentos.FieldByName('TIPO_1').AsString,
            33, ' ') + PadRight(cdsDocumentos.FieldByName('CNPJCPF_1').AsString, 20, ' ') +
            cdsDocumentos.FieldByName('DOCUMENTO_1').AsString);
      end;
      if cdsDocumentos.FieldByName('TIPO_2').AsString <> '' then
      begin
        if Length(cdsDocumentos.FieldByName('CNPJCPF_2').AsString) > 18 then
          rlDocOrig_tpDoc2.Lines.Add(PadRight(cdsDocumentos.FieldByName('TIPO_2').AsString,
            33, ' ') + PadRight(cdsDocumentos.FieldByName('CNPJCPF_2').AsString, 54, ' ') +
            cdsDocumentos.FieldByName('DOCUMENTO_2').AsString)
        else
          rlDocOrig_tpDoc2.Lines.Add(PadRight(cdsDocumentos.FieldByName('TIPO_2').AsString,
            33, ' ') + PadRight(cdsDocumentos.FieldByName('CNPJCPF_2').AsString, 20, ' ') +
            cdsDocumentos.FieldByName('DOCUMENTO_2').AsString);
      end;

      cdsDocumentos.Next;

      if (RLCTe.PageNumber > 1) then
        Inc(Linhas);
      if ((cdsDocumentos.recno > 10) and (RLCTe.PageNumber = 1) or (Linhas > 58)) then
        break;
    end;

    rlDocOrig_tpDoc1.Height := Round(rlDocOrig_tpDoc1.Lines.Count * 12) + 10;
    rlDocOrig_tpDoc2.Height := Round(rlDocOrig_tpDoc2.Lines.Count * 12) + 10;
    rld_07_headerItens.Height := rlb_07_HeaderItens.Height - 12;
  end
  else
  begin
    rlb_07_HeaderItens.AutoSize := False;
    rlb_07_HeaderItens.Height := 0;
  end;
end;

procedure TfrmDACTeRLRetrato.rlb_09_ObsBeforePrint(Sender: TObject;
  var PrintIt: boolean);
var
  i: Integer;
begin
  PrintIt := RLCTe.PageNumber = 1;

  rlmObs.Lines.BeginUpdate;
  rlmObs.Lines.Clear;

  rlmObs.Lines.Add(StringReplace(fpCTe.Compl.xObs, '&lt;BR&gt;', #13#10,
    [rfReplaceAll, rfIgnoreCase]));

  if fpCTe.Ide.tpEmis in [teContingencia, teFSDA, teDPEC] then
  begin
    if not (fpCTe.procCTe.cStat in [100, 101, 110]) then
      rlmObs.Lines.Add(ACBrStr(
        'DACTE em Contingência - Impresso em decorrência de problemas técnicos.'));
  end;

  if (fpCTe.Ide.tpEmis = teDPEC) and (fpDACTe.EPECEnviado) then
    rlmObs.Lines.Add('EPEC regularmente recebida pela Receita Federal do Brasil');

  rlmObs.Lines.Text := StringReplace(rlmObs.Lines.Text, ';', #13, [rfReplaceAll]);
  rlmObs.Lines.EndUpdate;

  // Mensagem para modo Homologacao.
  rllMsgTeste.Visible := False;
  rllMsgTeste.Enabled := False;

  if fpCTe.Ide.tpAmb = taHomologacao then
  begin
    if rlmObs.Lines.Count < 5  then
    begin
       rlmObs.Lines.BeginUpdate;
       for i:=rlmObs.Lines.Count to 5 do
          rlmObs.Lines.Add('');
       rlmObs.Lines.EndUpdate;
    end;

    if fpCTe.procCTe.nprot = '' then
      rllMsgTeste.Caption := ACBrStr('CT-e NÃO ENVIADO, SEM VALOR FISCAL - HOMOLOGAÇÃO')
    else
      rllMsgTeste.Caption := ACBrStr('CT-e SEM VALOR FISCAL - AMBIENTE DE HOMOLOGAÇÃO');

    rllMsgTeste.Visible := True;
    rllMsgTeste.Enabled := True;
  end
  else
  begin
    if fpCTe.procCTe.cStat > 0 then
    begin
      if (fpCTe.procCTe.cStat = 101) or (fpDACTe.Cancelada) then
      begin
        rllMsgTeste.Caption := 'CT-e CANCELADO';
        rllMsgTeste.Visible := True;
        rllMsgTeste.Enabled := True;
      end;

      if fpCTe.procCTe.cStat = 110 then
      begin
        rllMsgTeste.Caption := 'CT-e DENEGADO';
        rllMsgTeste.Visible := True;
        rllMsgTeste.Enabled := True;
      end;

      if not fpCTe.procCTe.cStat in [101, 110, 100] then
      begin
        rllMsgTeste.Caption := fpCTe.procCTe.xMotivo;
        rllMsgTeste.Visible := True;
        rllMsgTeste.Enabled := True;
      end;
    end
    else
    begin
      if fpCTe.procCTe.nprot = '' then
        rllMsgTeste.Caption := ACBrStr('CT-e NÃO ENVIADO, SEM VALOR FISCAL');

      rllMsgTeste.Visible := True;
      rllMsgTeste.Enabled := True;
    end;
  end;

  rllMsgTeste.Repaint;

  // Ajusta o tamanho do quadro conforme a OBS
  rlb_09_Obs.Height  := rlmObs.Height + 20 + 4;

  if rllMsgTeste.Visible and (rlb_09_Obs.Height < 68) then
    rlb_09_Obs.Height  := 68;
end;

procedure TfrmDACTeRLRetrato.rlb_10_ModRodFracionadoBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  PrintIt := RLCTe.PageNumber = 1;

  // Imprime as Informações Especificas do Modal se o Tipo de CTe for Normal
  rlb_10_ModRodFracionado.Enabled :=
    (fpCTe.Ide.tpCTe = tcNormal) and (fpCTe.Ide.modal = mdRodoviario);
  if not (rlb_10_ModRodFracionado.Enabled) then
    rlb_10_ModRodFracionado.Height := 0;

  rlb_11_ModRodLot103.Enabled := False;
  rlb_11_ModRodLot104.Enabled := False;

  with fpCTe.infCTeNorm.rodo do
  begin
    rllRntrcEmpresa.Caption := RNTRC;
    rlsCIOT.Enabled := True;
    lblCIOT.Enabled := True;
    rllCIOT.Enabled := True;
    rllCIOT.Caption := CIOT;

    if rlb_10_ModRodFracionado.Enabled then
    begin
      if (fpCTe.infCTe.versao >= 3.00) then
        modalRodoviarioVersao30()
      else
      begin
        case Lota of
          ltNao:
          begin
            rllTituloLotacao.Caption :=
              ACBrStr('DADOS ESPECÍFICOS DO MODAL RODOVIÁRIO - CARGA FRACIONADA');
            rllLotacao.Caption := ACBrStr('NÃO');
          end;
          ltsim:
          begin
            rllTituloLotacao.Caption :=
              ACBrStr('DADOS ESPECÍFICOS DO MODAL RODOVIÁRIO - LOTAÇÃO');
            rllLotacao.Caption := 'SIM';
            if Versao = 103 then
              rlb_11_ModRodLot103.Enabled := True
            else
              rlb_11_ModRodLot104.Enabled := True;
          end;
        end;

        if (dPrev > 0) then
          rllDtPrevEntrega.Caption := FormatDateTime('DD/MM/YYYY', dPrev)
        else
        begin
          if (fpCTe.compl.Entrega.comData.dProg > 0) then
            rllDtPrevEntrega.Caption := FormatDateTime('DD/MM/YYYY', fpCTe.compl.Entrega.comData.dProg);
        end;
      end;
    end;
  end;
end;

procedure TfrmDACTeRLRetrato.rlb_11_ModRodLot103BeforePrint(Sender: TObject;
  var PrintIt: boolean);
var
  i: integer;
begin
  PrintIt := RLCTe.PageNumber = 1;

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

  for i := 0 to (fpCTe.infCTeNorm.Rodo.veic.Count - 1) do
  begin
    if fpCTe.infCTeNorm.Rodo.veic.Items[i].tpVeic = tvTracao then
      rlmTipo.Lines.Add(ACBrStr('Tração'))
    else
      rlmTipo.Lines.Add('Reboque');
    rlmPlaca.Lines.Add(fpCTe.infCTeNorm.Rodo.veic.Items[i].placa);
    rlmUF.Lines.Add(fpCTe.infCTeNorm.Rodo.veic.Items[i].UF);
    rlmRNTRC.Lines.Add(fpCTe.infCTeNorm.Rodo.veic.Items[i].Prop.RNTRC);
  end;

  if fpCTe.infCTeNorm.Rodo.moto.Count > 0 then
  begin
    rllNomeMotorista.Caption := fpCTe.infCTeNorm.Rodo.moto.Items[0].xNome;
    rllCPFMotorista.Caption := FormatarCPF(fpCTe.infCTeNorm.Rodo.moto.Items[0].CPF);
  end;

  for i := 0 to (fpCTe.infCTeNorm.Rodo.lacRodo.Count - 1) do
  begin
    rllLacres.Caption := rllLacres.Caption +
      fpCTe.infCTeNorm.Rodo.lacRodo.Items[i].nLacre + '/';
  end;
end;

procedure TfrmDACTeRLRetrato.rlb_11_ModRodLot104AfterPrint(Sender: TObject);
begin
  if ((cdsDocumentos.recNo > 10) and (rlCte.PageNumber = 1)) then
    RLCte.newpage;
end;

procedure TfrmDACTeRLRetrato.cabecalhoVersao30;
begin
  if (fpCTe.ide.modelo <> 67) then   //67-CTeOS
  begin
    rlLabel28.Caption := ACBrStr('INDICADOR DO CT-E GLOBALIZADO');
    rllabel78.Caption := ACBrStr('INFORMAÇÕES DO CT-E GLOBALIZADO');

    if fpCTe.ide.indGlobalizado = tiSim then
    begin
      rllTomaServico.Caption := 'SIM';
      if (Trim(fpCTe.infCTeNorm.infGlobalizado.xObs) <> '') then
        rllFormaPagamento.Caption := fpCTe.infCTeNorm.infGlobalizado.xObs;
    end
    else
      rllTomaServico.Caption := ACBrStr('NÃO');
  end
  else
  begin
    rlLabel28.Caption :=
      ACBrStr('CÓDIGO FISCAL DE OPERAÇÕES E PRESTAÇÕES - NATUREZA DA OPERAÇÃO');
    rllabel78.Caption := '';
    rlsLinhaV01.Height := 31;
    rllTomaServico.AutoSize := True;
    rllTomaServico.Caption :=
      FormatFloat('0000', fpCTe.Ide.CFOP) + ' - ' + fpCTe.Ide.natOp;
    rllFormaPagamento.Caption := '';
  end;
end;

procedure TfrmDACTeRLRetrato.tomadorMod67;
var
  I: integer;
begin
  rlb_03_DadosDACTe_OS.Enabled := (fpCTe.ide.modelo = 67);

  if (rlb_03_DadosDACTe_OS.Enabled) then
  begin
    rllOrigPrestacao1.Caption :=
      fpCTe.Ide.xMunIni + ' - ' + fpCTe.Ide.UFIni + ' - ' +
      FormatFloat('000', fpCTe.Ide.cMunIni);
    rllDestPrestacao1.Caption :=
      fpCTe.Ide.xMunFim + ' - ' + fpCTe.Ide.UFFim + ' - ' +
      FormatFloat('000', fpCTe.Ide.cMunFim);

    for I := 0 to fpCTe.ide.infPercurso.Count - 1 do
    begin
      if I = 0 then
        rllPercursoVeiculo.Caption := fpCTe.ide.infPercurso.Items[I].UFPer
      else
        rllPercursoVeiculo.Caption :=
          rllPercursoVeiculo.Caption + ' - ' + fpCTe.ide.infPercurso.Items[I].UFPer;
    end;

    with fpCTe.toma do
    begin
      rllRazaoToma1.Caption := xNome;
      rllEnderecoToma1.Caption :=
        EnderToma.xLgr + ', ' + EnderToma.nro + ' - ' +
        EnderToma.xCpl + ' - ' + EnderToma.xBairro;
      rllCEPToma1.Caption := FormatarCEP(EnderToma.CEP);
      rllMunToma1.Caption := EnderToma.xMun + ' - ' + EnderToma.UF;
      rllCnpjToma1.Caption := FormatarCNPJouCPF(CNPJCPF);
      rllPaisToma1.Caption := EnderToma.xPais;
      rllInscEstToma1.Caption := IE;
      rllFoneToma1.Caption := FormatarFone(fone);
    end;

  end
  else
    rlb_03_DadosDACTe_OS.Height := 0;
end;

procedure TfrmDACTeRLRetrato.dadosNotaFiscalVersao30;
begin
  //Valida a Versão para ocultar campos na tela
  with fpCTe.infCTe do
  begin
    rlLabel5.Visible := not (versao >= 3.00);
    rlLabel37.Visible := not (versao >= 3.00);
    rlmNomeSeguradora.Visible := not (versao >= 3.00);
    rlmRespSeguroMerc.Visible := not (versao >= 3.00);
    rlLabel39.Visible := not (versao >= 3.00);
    rlmNroApolice.Visible := not (versao >= 3.00);
    rllabel40.Visible := not (versao >= 3.00);
    rlmNroAverbacao.Visible := not (versao >= 3.00);
    RLDraw62.Visible := not (versao >= 3.00);
    RLDraw61.Visible := not (versao >= 3.00);
    RLDraw7.Visible := not (versao >= 3.00);
    RLDraw8.Visible := not (versao >= 3.00);
  end;

  //Valida a Versão para reposicionar campos na tela
  if (fpCTe.infCTe.versao >= 3.00) then
  begin
    RLDraw58.Left := 112;
    RLDraw59.Left := 224;
    RLDraw100.Left := 336;
    RLDraw60.Left := 448;

    RLLabel35.Width := 100;
    RLLabel35.Left := 5;
    rlmQtdUnidMedida1.Width := 100;
    rlmQtdUnidMedida1.Left := 5;

    RLLabel36.Width := 100;
    RLLabel36.Left := 118;
    rlmQtdUnidMedida2.Width := 100;
    rlmQtdUnidMedida2.Left := 118;

    RLLabel41.Width := 100;
    RLLabel41.Left := 232;
    rlmQtdUnidMedida3.Width := 100;
    rlmQtdUnidMedida3.Left := 232;

    RLLabel73.Width := 100;
    RLLabel73.Left := 341;
    rlmQtdUnidMedida4.Width := 100;
    rlmQtdUnidMedida4.Left := 341;

    RLLabel43.Width := 280;
    RLLabel43.Left := 456;
    rlmQtdUnidMedida5.Width := 280;
    rlmQtdUnidMedida5.Left := 456;
  end;

  DefinirAltura;
end;

procedure TfrmDACTeRLRetrato.modalRodoviarioVersao30;
begin
  rllTituloLotacao.Caption := ACBrStr('DADOS ESPECÍFICOS DO MODAL RODOVIÁRIO');
  rlsCIOT.Enabled := False;
  lblCIOT.Caption := '';
  rllCIOT.Caption := '';
  rllLotacao.Caption := '';
  RLLabel83.Caption := '';
  rlsCIOT.Visible := False;
  RLDraw36.Visible := False;

  if (fpCTe.compl.Entrega.comData.dProg > 0) then
    rllDtPrevEntrega.Caption :=
      FormatDateTime('DD/MM/YYYY', fpCTe.compl.Entrega.comData.dProg);

  if (fpCTe.ide.modelo = 67) then
    modalRodoviarioMod67();
end;

procedure TfrmDACTeRLRetrato.modalRodoviarioMod67;
var
  lTAF, lREG, lCNPJ: string;
begin
  with fpCTe.infCTeNorm.rodoOS.veic do
  begin
    if (Length(Prop.TAF) > 0) or (Length(Prop.NroRegEstadual) > 0) then
    begin
      //Terceiro
      lTAF := Prop.TAF;
      lREG := prop.NroRegEstadual;
      lCNPJ := prop.CNPJCPF;
    end
    else
    begin
      //Próprio
      lTAF := fpCTe.infCTeNorm.rodoOS.TAF;
      lREG := fpCTe.infCTeNorm.rodoOS.NroRegEstadual;
      lCNPJ := fpCTe.Emit.CNPJ;
    end;
  end;

  rlsCIOT.Visible := True;
  RLDraw36.Visible := True;
  RLDraw37.Visible := True;
  RLDraw38.Visible := True;

  rlsCIOT.Left := 148;
  RLDraw36.Left := 296;
  RLDraw37.Left := 444;
  RLDraw38.Left := 592;

  RLLabel11.Visible := True;
  rllRntrcEmpresa.Visible := True;
  RLLabel11.Caption := ACBrStr('TERMO AUTORIZAÇÃO DE FRETAMENTO');
  rllRntrcEmpresa.Caption := lTAF;

  lblCIOT.Visible := True;
  rllCIOT.Visible := True;
  lblCIOT.Left := 154;
  rllCIOT.Left := 154;
  lblCIOT.Caption := ACBrStr('Nº DE REGISTRO ESTADUAL');
  rllCIOT.Caption := lREG;

  RLLabel83.Visible := True;
  rllLotacao.Visible := True;
  RLLabel83.Left := 301;
  rllLotacao.Left := 301;
  RLLabel83.Caption := ACBrStr('PLACA DO VEÍCULO');
  rllLotacao.Caption := fpCTe.infCTeNorm.rodoOS.veic.placa;

  RLLabel84.Visible := True;
  rllDtPrevEntrega.Visible := True;
  RLLabel84.Left := 449;
  rllDtPrevEntrega.Left := 449;
  RLLabel84.Caption := ACBrStr('RENAVAM DO VEÍCULO');
  rllDtPrevEntrega.Caption := fpCTe.infCTeNorm.rodoOS.veic.RENAVAM;

  RLLabel216.Visible := True;
  RLLabel85.Visible := True;
  RLLabel85.Width := 70;
  RLLabel216.Left := 597;
  RLLabel85.Left := 597;
  RLLabel216.Caption := ACBrStr('CNPJ/CPF');
  RLLabel85.Caption := lCNPJ;
end;

procedure TfrmDACTeRLRetrato.fluxoCargaVersao30;
var
  i: integer;
begin
  rllSiglaPassagem.Lines.Clear;

  rlb_Fluxo_Carga.Enabled := ((fpCTe.infCTe.versao >= 3.00) and
    (fpCTe.ide.modelo <> 67) and (fpCTe.ide.modal = mdAereo));

  if (rlb_Fluxo_Carga.Enabled) then
  begin
    rllSiglaOrigem.Caption := fpCTe.compl.fluxo.xOrig;
    rllSiglaDestino.Caption := fpCTe.compl.fluxo.xDest;

    for i := 0 to (fpCTe.compl.fluxo.pass.Count - 1) do
      rllSiglaPassagem.Lines.Add(fpCTe.compl.fluxo.pass.Items[i].xPass);
  end
  else
    rlb_Fluxo_Carga.Height := 0;
end;

procedure TfrmDACTeRLRetrato.posicionaCanhoto;
begin
  // Seleciona o Layout do Canhoto
  case fpDACTe.PosCanhotoLayout of
    prlBarra  : CanhotoDesabilita( rlb_01_Recibo).
                ConfigurarCanhotoBarra.
                ConfigurarRLBarcode.
                Canhoto( rlb_01_ReciboBarra );

    prlPadrao : CanhotoDesabilita( rlb_01_ReciboBarra ).
                Canhoto( rlb_01_Recibo );
  end;
end;

function TfrmDACTeRLRetrato.Canhoto(Value: TRLBand): TfrmDACTeRLRetrato;
begin
  // Posiciona o canhoto do fpDACTe no cabeçalho ou rodapé
  result := self;

  case fpDACTe.PosCanhoto of
    prCabecalho:
      begin
        Value.BandType := btHeader;
        rlb_DivisaoRecibo.BandType := btHeader;
        Value.Top := 0;
        rlb_DivisaoRecibo.Top := Value.Top + Value.Height;
      end;
    prRodape:
      begin
        Value.BandType := btFooter;
        rlb_DivisaoRecibo.BandType := btFooter;
        rlb_DivisaoRecibo.Top := 0;
        Value.Top := rlb_DivisaoRecibo.Top + rlb_DivisaoRecibo.Height;
//        rlb_DivisaoRecibo.Top := rlb_16_DadosExcEmitente.Top +
//                                 rlb_16_DadosExcEmitente.Height;
      end;
  end;
end;

function TfrmDACTeRLRetrato.CanhotoDesabilita(
  Value: TRLBand): TfrmDACTeRLRetrato;
begin
  result := Self;
  Value.Visible := false;
end;

function TfrmDACTeRLRetrato.ConfigurarCanhotoBarra: TfrmDACTeRLRetrato;
begin
  result := self;
  // Define o Tamanho do Canhoto
  rlb_01_ReciboBarra.Height := 65;
  if fpDACTe.ExibeResumoCanhoto then
    rlb_01_ReciboBarra.Height := rlb_01_ReciboBarra.Height + 20;
end;

function TfrmDACTeRLRetrato.ConfigurarRLBarcode: TfrmDACTeRLRetrato;
var
  lIAlimento : Integer;
  lIAlibarra : Integer;
begin
  result := self;
  // Posiciona as informações do canhoto com chave de acesso

  rlBarraiCanhoto.Height        := rlb_01_ReciboBarra.Height;

  RLBarraRecebemosDe.Top        := rlBarraiCanhoto.Top +1;
  RLBarraRecebemosDe.Font       := rllRecebemosDe.Font;
  RLBarraRecebemosDe.Lines.Add( rllRecebemosDe.Caption );
  RLBarraRecebemosDe.Font.Size  := rllRecebemosDe.Font.Size;

  rlBarraiCanhoto1.Top          := RLBarraResumo.top + 10;
  RLBarraBarcode.Caption        := rlbCodigoBarras.Caption;
  RLBarraBarcode.Height         := rlbCodigoBarras.Height;

  if fpDACTe.ExibeResumoCanhoto then
  begin
    RLBarraResumo.Top           := RLBarraRecebemosDe.Top + 15;
//    RLBarraResumo.Font          := rllResumo.Font;
//    RLBarraResumo.Font.Size     := rllResumo.Font.Size;
//    RLBarraResumo.Lines.Add( rllResumo.Caption );
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

  rllBarraCTe.Top               := lIAlimento;
  rllBarraCTe.Left              := lIAlibarra - 80;

  rlBarraNumero.Top             := lIAlimento;
  rlBarraNumero.Left            := lIAlibarra - 40;
  rlBarraNumero.Alignment       := taLeftJustify;
  rlBarraNumero.Font            := rllBarraCTe.Font;
  rlBarraNumero.Font.Style      := [fsBold];
  rlBarraNumero.Caption         := rllNumCTe.Caption;

  rlBarraSERIE.Top             := lIAlimento;
  rlBarraSERIE.Left            := lIAlibarra + 70 ;
  rlBarraSERIE.Alignment       := taLeftJustify;
  rlBarraSERIE.Font            := rllBarraCTe.Font;
  rlBarraSERIE.Caption         := rllSerie.Caption;

  rlBarraiCanhoto3.Left         := rllBarraCTe.Left - 3;
end;

procedure TfrmDACTeRLRetrato.prestacaoServicoMod67;
begin
  rlb_CTeOS_PrestacaoServico.Enabled := (fpCTe.ide.modelo = 67);

  if (rlb_CTeOS_PrestacaoServico.Enabled) then
  begin
    rlb_CTeOS_PrestacaoServico.AutoSize := True;
    rlDocOrig_tpDoc3.Lines.Clear;

    rlDocOrig_tpDoc3.Lines.Add('  ' + IfThen(fpCTe.infCTeNorm.infServico.qCarga =
      0, '', FormatFloatBr(
      msk6x4, fpCTe.infCTeNorm.infServico.qCarga)) +
      Space(25) + fpCTe.infCTeNorm.infServico.xDescServ);

  end
  else
  begin
    rlb_CTeOS_PrestacaoServico.Height := 0;
  end;
end;

procedure TfrmDACTeRLRetrato.dadosSeguradoraMod67;
var
  I: integer;
begin
  rlb_Dados_Seguradora.Enabled :=
    ((fpCTe.ide.modelo = 67) or (fpCTe.Ide.modal = mdMultimodal));

  if (rlb_Dados_Seguradora.Enabled) then
  begin
    rllNomeSeguradora.Lines.Clear;
    rllApolice.Lines.Clear;
    rllResponsavelSeguro.Lines.Clear;

    if (fpCTe.infCTeNorm.seg.Count > 0) then
    begin
      for I := 0 to fpCTe.infCTeNorm.seg.Count - 1 do
      begin
        case fpCTe.infCTeNorm.seg.Items[I].respSeg of
          rsRemetente: rllResponsavelSeguro.Lines.Add('Remetente');
          rsExpedidor: rllResponsavelSeguro.Lines.Add('Expedidor');
          rsRecebedor: rllResponsavelSeguro.Lines.Add('Recebedor');
          rsDestinatario: rllResponsavelSeguro.Lines.Add('Destinatário');
          rsEmitenteCTe: rllResponsavelSeguro.Lines.Add('Emitente');
          rsTomadorServico: rllResponsavelSeguro.Lines.Add('Tomador');
        end;
        rllNomeSeguradora.Lines.Add(fpCTe.infCTeNorm.seg.Items[I].xSeg);
        rllApolice.Lines.Add(fpCTe.infCTeNorm.seg.Items[I].nApol);

      end;
    end;
  end
  else
    rlb_Dados_Seguradora.Height := 0;
end;

procedure TfrmDACTeRLRetrato.DefinirAltura;
begin
  RLDraw58.Height  := rlb_04_DadosNotaFiscal.Height;
  RLDraw59.Height  := rlb_04_DadosNotaFiscal.Height;
  RLDraw100.Height := rlb_04_DadosNotaFiscal.Height;
  RLDraw60.Height  := rlb_04_DadosNotaFiscal.Height;
end;

procedure TfrmDACTeRLRetrato.rlb_11_ModRodLot104BeforePrint(Sender: TObject;
  var PrintIt: boolean);
var
  i: integer;
begin
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

  for i := 0 to (fpCTe.infCTeNorm.Rodo.veic.Count - 1) do
  begin
    if fpCTe.infCTeNorm.Rodo.veic.Items[i].tpVeic = tvTracao then
      rlmTipo2.Lines.Add(ACBrStr('Tração'))
    else
      rlmTipo2.Lines.Add('Reboque');
    rlmPlaca2.Lines.Add(fpCTe.infCTeNorm.Rodo.veic.Items[i].placa);
    rlmUF2.Lines.Add(fpCTe.infCTeNorm.Rodo.veic.Items[i].UF);
    rlmRNTRC2.Lines.Add(fpCTe.infCTeNorm.Rodo.veic.Items[i].Prop.RNTRC);
  end;

  for i := 0 to (fpCTe.infCTeNorm.Rodo.valePed.Count - 1) do
  begin
    rlmCNPJForn.Lines.Add(FormatarCNPJ(fpCTe.infCTeNorm.Rodo.valePed.Items[i].CNPJForn));
    rlmNumCompra.Lines.Add(fpCTe.infCTeNorm.Rodo.valePed.Items[i].nCompra);
    rlmCNPJPg.Lines.Add(FormatarCNPJ(fpCTe.infCTeNorm.Rodo.valePed.Items[i].CNPJPg));
  end;

  if fpCTe.infCTeNorm.Rodo.moto.Count > 0 then
  begin
    rllNomeMotorista2.Caption := fpCTe.infCTeNorm.Rodo.moto.Items[0].xNome;
    rllCPFMotorista2.Caption := FormatarCPF(fpCTe.infCTeNorm.Rodo.moto.Items[0].CPF);
  end;

  for i := 0 to (fpCTe.infCTeNorm.Rodo.lacRodo.Count - 1) do
  begin
    rllLacres2.Caption := rllLacres2.Caption +
      fpCTe.infCTeNorm.Rodo.lacRodo.Items[i].nLacre + '/';
  end;
end;

procedure TfrmDACTeRLRetrato.rlb_12_ModAereoBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  PrintIt := RLCTe.PageNumber = 1;

  rlb_12_ModAereo.Enabled := (fpCTe.Ide.tpCTe = tcNormal) and
                             (fpCTe.Ide.modal = mdAereo);

  rllCaracAdServico.Caption := fpCTe.Compl.xCaracSer;
  rllCaracAdTransporte.Caption := fpCTe.Compl.xCaracAd;

  with fpCTe.infCTeNorm.aereo do
  begin
    rllAWB.Caption := nOCA;
    rllTarifaCL.Caption := tarifa.CL;
    rllTarifaCodigo.Caption := tarifa.cTar;
    rllTarifaValor.Caption := FormatCurr(',0.00', tarifa.vTar);
    rllContaCorrente.Caption := IdT; // ??? Conta Corrente ???
    rllMinuta.Caption := FormatFloat('0000000000', nMinu);
    rllLojaAgenteEmissor.Caption := xLAgEmi;
  end;

  if fpCTe.Ide.retira = rtSim then
    rllRetira.Caption := 'SIM'
  else
    rllRetira.Caption := ACBrStr('NÃO');
  rllDadosRetira.Caption := fpCTe.Ide.xdetretira;
end;

procedure TfrmDACTeRLRetrato.rlb_13_ModAquaviarioBeforePrint(Sender: TObject;
  var PrintIt: boolean);
var
  i: integer;
begin
  PrintIt := RLCTe.PageNumber = 1;

  rlb_13_ModAquaviario.Enabled := (fpCTe.Ide.tpCTe = tcNormal) and
                                  (fpCTe.Ide.modal = mdAquaviario);

  with fpCTe.infCTeNorm.aquav do
  begin
    rllBCAFRMM.Caption := FormatCurr(',0.00', vPrest);
    rllValorAFRMM.Caption := FormatCurr(',0.00', vAFRMM);

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

    rllIndBalsas.Caption := '';
    for i := 0 to (balsa.Count - 1) do
    begin
      if i = 0 then
        rllIndBalsas.Caption := balsa.Items[i].xBalsa
      else
        rllIndBalsas.Caption := rllIndBalsas.Caption + '/' + balsa.Items[i].xBalsa;
    end;
  end;
end;

procedure TfrmDACTeRLRetrato.rlb_14_ModFerroviarioBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  PrintIt := RLCTe.PageNumber = 1;

  rlb_14_ModFerroviario.Enabled := (fpCTe.Ide.tpCTe = tcNormal) and
                                   (fpCTe.Ide.modal = mdFerroviario);
end;

procedure TfrmDACTeRLRetrato.rlb_15_ModDutoviarioBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  PrintIt := RLCTe.PageNumber = 1;

  rlb_15_ModDutoviario.Enabled := (fpCTe.Ide.tpCTe = tcNormal) and
                                  (fpCTe.Ide.modal = mdDutoviario);
end;

procedure TfrmDACTeRLRetrato.rlb_16_DadosExcEmitenteBeforePrint(Sender: TObject;
  var PrintIt: boolean);
var
  i, vHeight: integer;
begin
  PrintIt := RLCTe.PageNumber = 1;

  rlmObsExcEmitente.Lines.BeginUpdate;
  rlmObsExcEmitente.Lines.Clear;
  rlmObsFisco.Lines.BeginUpdate;
  rlmObsFisco.Lines.Clear;

  for i := 0 to (fpCTe.Compl.ObsCont.Count - 1) do
    with fpCTe.Compl.ObsCont.Items[i] do
    begin
      rlmObsExcEmitente.Lines.Add(StringReplace(xCampo, '&lt;BR&gt;',
        #13#10, [rfReplaceAll, rfIgnoreCase]) + ': ' +
        StringReplace(xTexto, '&lt;BR&gt;', #13#10, [rfReplaceAll, rfIgnoreCase]));
    end;

  rlmObsExcEmitente.Lines.Text :=
    StringReplace(rlmObsExcEmitente.Lines.Text, ';', #13, [rfReplaceAll]);
  rlmObsExcEmitente.Lines.EndUpdate;

  if Length(Trim(fpCTe.Imp.infAdFisco)) > 0 then
    rlmObsFisco.Lines.Add(StringReplace(fpCTe.Imp.infAdFisco, '&lt;BR&gt;',
      #13#10, [rfReplaceAll, rfIgnoreCase]));

  for i := 0 to (fpCTe.Compl.ObsFisco.Count - 1) do
    with fpCTe.Compl.ObsFisco.Items[i] do
    begin
      rlmObsFisco.Lines.Add(StringReplace(xCampo, '&lt;BR&gt;', #13#10,
        [rfReplaceAll, rfIgnoreCase]) + ': ' + StringReplace(xTexto,
        '&lt;BR&gt;', #13#10, [rfReplaceAll, rfIgnoreCase]));
    end;

  rlmObsFisco.Lines.Text := StringReplace(rlmObsFisco.Lines.Text, ';',
    #13, [rfReplaceAll]);
  rlmObsFisco.Lines.EndUpdate;

  if (rlmObsExcEmitente.Lines.Count > 0) or (rlmObsFisco.Lines.Count > 0) then
  begin
    vHeight := rlmObsExcEmitente.Height + 20;

    if rlmObsFisco.Height > rlmObsExcEmitente.Height then
      vHeight := rlmObsFisco.Height + 20;

    RLDraw3.Height  := vHeight;
  end;
end;

procedure TfrmDACTeRLRetrato.rlb_17_SistemaBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  PrintIt := RLCTe.PageNumber = 1;

  rlLabel15.Visible := fpDACTe.ImprimirHoraSaida;

  if rlLabel15.Visible then
    rlLabel15.Caption := ACBrStr('DATA / HORA DA IMPRESSÃO: ') + FormatDateTimeBr(Now);

  if rlLabel15.Visible and (fpDACTe.Usuario <> '') then
    rlLabel15.Caption := rlLabel15.Caption + ' - ' + fpDACTe.Usuario;

  rllblSistema.Visible := NaoEstaVazio(fpDACTe.Sistema);
  rllblSistema.Caption := fpDACTe.Sistema;
end;

procedure TfrmDACTeRLRetrato.rlb_06_ProdutosPerigososBeforePrint(Sender: TObject;
  var PrintIt: boolean);
var
  i: integer;
begin
  PrintIt := (RLCTe.PageNumber = 1);

  rlb_06_ProdutosPerigosos.Enabled := (fpCTe.infCTeNorm.peri.Count > 0);

  if not rlb_06_ProdutosPerigosos.Enabled then
    rlb_06_ProdutosPerigosos.Height := 0;

  rlmNumONU.Lines.Clear;
  rlmNomeApropriado.Lines.Clear;
  rlmClasse.Lines.Clear;
  rlmGrupoEmbalagem.Lines.Clear;
  rlmQtdeProduto.Lines.Clear;

  for i := 0 to (fpCTe.infCTeNorm.peri.Count - 1) do
  begin
    rlmNumONU.Lines.Add(fpCTe.infCTeNorm.peri.Items[i].nONU);
    rlmNomeApropriado.Lines.Add(fpCTe.infCTeNorm.peri.Items[i].xNomeAE);
    rlmClasse.Lines.Add(fpCTe.infCTeNorm.peri.Items[i].xClaRisco);
    rlmGrupoEmbalagem.Lines.Add(fpCTe.infCTeNorm.peri.Items[i].grEmb);
    rlmQtdeProduto.Lines.Add(fpCTe.infCTeNorm.peri.Items[i].qTotProd);
  end;
end;

procedure TfrmDACTeRLRetrato.rlb_06_VeiculosNovosBeforePrint(Sender: TObject;
  var PrintIt: boolean);
var
  i: integer;
begin
  PrintIt := (RLCTe.PageNumber = 1);

  rlb_06_VeiculosNovos.Enabled := (fpCTe.infCTeNorm.veicNovos.Count > 0);

  if not rlb_06_VeiculosNovos.Enabled then
    rlb_06_VeiculosNovos.Height := 0;

  CHASSI.Lines.Clear;
  COR.Lines.Clear;
  MODELO.Lines.Clear;
  VUNIT.Lines.Clear;
  VFRETE.Lines.Clear;

  for i := 0 to (fpCTe.infCTeNorm.veicNovos.Count - 1) do
  begin
    CHASSI.Lines.Add(fpCTe.infCTeNorm.veicNovos.Items[i].chassi);
    COR.Lines.Add(fpCTe.infCTeNorm.veicNovos.Items[i].cCor + ' - ' +
      fpCTe.infCTeNorm.veicNovos.Items[i].xCor);
    MODELO.Lines.Add(fpCTe.infCTeNorm.veicNovos.Items[i].cMod);
    VUNIT.Lines.Add(FloatToString(fpCTe.infCTeNorm.veicNovos.Items[i].vUnit, ','));
    VFRETE.Lines.Add(FloatToString(fpCTe.infCTeNorm.veicNovos.Items[i].vFrete, ','));
  end;

  rlb_06_VeiculosNovos.Height := CHASSI.Top + CHASSI.Height + 5;

  for I := 0 to (TRLBand(Sender).ControlCount - 1) do
  begin
    if TRLBand(Sender).Controls[I] is TRLDraw then
    begin
      if TRLDraw(TRLBand(Sender).Controls[I]).DrawKind = dkLine then
        TRLDraw(TRLBand(Sender).Controls[I]).Height := rlb_06_VeiculosNovos.Height - RLDraw229.Top - 1;
    end;
  end;
end;

procedure TfrmDACTeRLRetrato.rlb_CTeOS_PrestacaoServicoBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  prestacaoServicoMod67();
end;

procedure TfrmDACTeRLRetrato.rlb_Cte_Anulado_SubstituidoBeforePrint(
  Sender: TObject; var PrintIt: Boolean);
var
  ModeloDoc: string;
begin
  rlb_Cte_Anulado_Substituido.Enabled :=((fpCTe.Ide.tpCTe = tcAnulacao) or
                                         (fpCTe.Ide.tpCTe = tcSubstituto));

  if (rlb_Cte_Anulado_Substituido.Enabled) then
  begin
    rlChaveCteSerAnulSubst.Lines.Clear;
    rlChaveCteAnulacao.Lines.Clear;
    ModeloDoc := IfThen(fpCTe.ide.modelo = 67, 'CT-E OS', 'CT-E');

    case fpCTe.Ide.tpCTe of
      tcAnulacao:
      begin
        rlblChaveCteSubstituido.Caption := PadRight('NÚMERO', 24, ' ')+'CHAVE  '+ ModeloDoc +'  ANUALADO';
        rlChaveCteSerAnulSubst.Lines.Add(PadRight(copy(fpCTe.InfCTeAnu.chCTe, 26, 9), 17, ' ')+
                                         FormatarChaveAcesso(fpCTe.InfCTeAnu.chCTe));
      end;
      tcSubstituto:
      begin
        rlblChaveCteSubstituido.Caption := PadRight('NÚMERO', 24, ' ')+'CHAVE  '+ ModeloDoc +'  SUBSTITUÍDO';
        rlChaveCteSerAnulSubst.Lines.Add(PadRight(copy(fpCTe.infCTeNorm.infCTeSub.chCte, 26, 9), 17, ' ')+
                                         FormatarChaveAcesso(fpCTe.infCTeNorm.infCTeSub.chCte));
        rlblChaveCteAnulacao.Visible := True;
        rlblChaveCteAnulacao.Caption := PadRight('NÚMERO', 24, ' ')+'CHAVE  '+ ModeloDoc +'  DE ANULAÇÃO';
        rlChaveCteAnulacao.Lines.Add(PadRight(copy(fpCTe.infCTeNorm.infCTeSub.refCteAnu, 26, 9), 17, ' ')+
                                     FormatarChaveAcesso(fpCTe.infCTeNorm.infCTeSub.refCteAnu));
      end;
    end;
  end
  else
  begin
    rlb_Cte_Anulado_Substituido.AutoSize := False;
    rlb_Cte_Anulado_Substituido.Height := 0;
  end;
end;

procedure TfrmDACTeRLRetrato.rlb_Dados_SeguradoraBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  dadosSeguradoraMod67();
end;

procedure TfrmDACTeRLRetrato.rlb_DivisaoReciboBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  PrintIt := RLCTe.PageNumber = 1;
end;

procedure TfrmDACTeRLRetrato.rlb_Fluxo_CargaBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  PrintIt := (RLCTe.PageNumber = 1);
  fluxoCargaVersao30();
end;

procedure TfrmDACTeRLRetrato.RLCTeBeforePrint(Sender: TObject; var PrintIt: boolean);
begin
  Versao := 200;
  Itens;

  if fpCTe.infCTeNorm.peri.Count = 0 then
    rlb_06_ProdutosPerigosos.Visible := False;
  if fpCTe.infCTeNorm.veicNovos.Count = 0 then
    rlb_06_VeiculosNovos.Visible := False;

  if fpCTe.ide.modelo = 67 then //CteOS
  begin
    rlpnlTributosFederais.Visible := True;
    rlb_06_ValorPrestacao.Height := 144;
  end
  else
  begin
    rlpnlTributosFederais.Visible := False;
    rlb_06_ValorPrestacao.Height := 117;
  end;

  rlb_10_ModRodFracionado.Height := 0;
  rlb_11_ModRodLot103.Height := 0;
  rlb_11_ModRodLot104.Height := 0;
  rlb_12_ModAereo.Height := 0;
  rlb_13_ModAquaviario.Height := 0;
  rlb_14_ModFerroviario.Height := 0;
  rlb_15_ModDutoviario.Height := 0;

  case fpCTe.Ide.modal of
    mdRodoviario:
    begin
      if fpCTe.infCTeNorm.rodo.lota = ltNao then
      begin
        rlb_10_ModRodFracionado.Height := 44;
      end
      else
      begin
        rlb_10_ModRodFracionado.Height := 44;
        rlb_11_ModRodLot104.Height := 107;
      end;
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

  RLCTe.Title := 'CT-e: ' + FormatFloat('000,000,000', fpCTe.Ide.nCT);

  if not EstaVazio(Trim(fpCTe.infCTeSupl.qrCodCTe)) then
    PintarQRCode(fpCTe.infCTeSupl.qrCodCTe, imgQRCode.Picture.Bitmap, qrUTF8NoBOM)
  else
  begin
    rlsLinhaV07.Height    := 26;
    rlsLinhaH03.Width     := 427;
    RLDraw99.Width        := 427;
    rlbCodigoBarras.Width := 419;
    rllVariavel1.Width    := 419;
    rllVariavel2.Width    := 419;
    imgQRCode.Visible     := False;
  end;

  rllSerie.Caption := IntToStr(fpCTe.Ide.serie);
  rllNumCte.Caption := FormatFloat('000,000,000', fpCTe.Ide.nCT);
  rlbCodigoBarras.Caption := OnlyNumber(fpCTe.InfCTe.Id);

  posicionaCanhoto;
end;

end.
