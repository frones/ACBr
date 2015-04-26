{******************************************************************************}
{ Projeto: Componente ACBrCTe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Conhecimen-}
{ to de Transporte eletrônico - CTe - http://www.cte.fazenda.gov.br            }
{                                                                              }
{ Direitos Autorais Reservados (c) 2008 Wemerson Souto                         }
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

{*******************************************************************************
|* Historico
|*
*******************************************************************************}

{$I ACBr.inc}

unit ACBrCTeDACTeQRRetrato;

// Atenção todos os comiters
// Quando enviar os fontes referentes ao DACTE favor alterar
// a data e o nome da linha abaixo.
// Última liberação:
// 27/09/2013 por Italo Jurisato Junior
// 16/09/2013 por Italo Jurisato Junior
// 06/09/2013 por Italo Jurisato Junior
// 21/08/2013 por Italo Jurisato Junior
// 20/08/2013 por André F. Moraes
// 06/08/2013 por Italo Jurisato Junior
// 28/01/2014 por Italo Jurisato Junior

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, QuickRpt, QRCtrls, XMLIntf, XMLDoc,
  JPEG, ACBrDFeQRCodeBar, pcnConversao, DB,
  DBClient, ACBrCTeDACTeQR;

type
  TfrmDACTeQRRetrato = class(TfrmDACTeQR)
    qrb_08_Itens: TQRBand;
    qrdbtTpDoc1: TQRDBText;
    cdsDocumentos: TClientDataSet;
    qrdbtCnpjEmitente1: TQRDBText;
    qrdbtDocumento1: TQRDBText;
    qrdbtDocumento2: TQRDBText;
    qrdbtCnpjEmitente2: TQRDBText;
    qrdbtTpDoc2: TQRDBText;
    cdsDocumentosTIPO_1: TStringField;
    cdsDocumentosCNPJCPF_1: TStringField;
    cdsDocumentosDOCUMENTO_1: TStringField;
    cdsDocumentosTIPO_2: TStringField;
    cdsDocumentosCNPJCPF_2: TStringField;
    cdsDocumentosDOCUMENTO_2: TStringField;
    QRShape29: TQRShape;
    qrb_01_Recibo: TQRBand;
    qrsLinhaH03: TQRShape;
    qrmEmitente: TQRMemo;
    qrmDadosEmitente: TQRMemo;
    qriLogo: TQRImage;
    qriBarCode: TQRImage;
    qrlNumCte: TQRLabel;
    QRLabel17: TQRLabel;
    QRLabel18: TQRLabel;
    QRLabel6: TQRLabel;
    QRLabel8: TQRLabel;
    QRLabel21: TQRLabel;
    QRLabel23: TQRLabel;
    QRLabel25: TQRLabel;
    QRLabel33: TQRLabel;
    QRLabel74: TQRLabel;
    qrlChave: TQRLabel;
    qrlPageNumber: TQRLabel;
    qrlSerie: TQRLabel;
    qrlModelo: TQRLabel;
    qrlEmissao: TQRLabel;
    qrlModal: TQRLabel;
    qrlProtocolo: TQRLabel;
    qrlTipoCte: TQRLabel;
    qrlDescricao: TQRLabel;
    QRLabel77: TQRLabel;
    QRLabel2: TQRLabel;
    QRLabel9: TQRLabel;
    qrlTipoServico: TQRLabel;
    QRLabel28: TQRLabel;
    qrlTomaServico: TQRLabel;
    QRLabel78: TQRLabel;
    qrlFormaPagamento: TQRLabel;
    qrlInscSuframa: TQRLabel;
    qrb_07_HeaderItens: TQRBand;
    qrlDocOrig: TQRLabel;
    QRShape32: TQRShape;
    QRLabel91: TQRLabel;
    QRLabel92: TQRLabel;
    QRLabel96: TQRLabel;
    QRLabel100: TQRLabel;
    QRLabel106: TQRLabel;
    QRLabel109: TQRLabel;
    QRShape34: TQRShape;
    QRShape28: TQRShape;
    QRShape35: TQRShape;
    qrsFimItens: TQRShape;
    qrb_09_Obs: TQRBand;
    qrb_02_Cabecalho: TQRChildBand;
    QRLabel29: TQRLabel;
    qrlNatOperacao: TQRLabel;
    QRLabel12: TQRLabel;
    qrlOrigPrestacao: TQRLabel;
    QRLabel14: TQRLabel;
    qrlDestPrestacao: TQRLabel;
    QRLabel13: TQRLabel;
    QRLabel16: TQRLabel;
    QRLabel22: TQRLabel;
    QRLabel24: TQRLabel;
    QRLabel26: TQRLabel;
    qrlRazaoRemet: TQRLabel;
    qrlEnderecoRemet1: TQRLabel;
    qrlEnderecoRemet2: TQRLabel;
    qrlMunRemet: TQRLabel;
    qrlCnpjRemet: TQRLabel;
    qrlPaisRemet: TQRLabel;
    QRLabel27: TQRLabel;
    QRLabel30: TQRLabel;
    QRLabel31: TQRLabel;
    QRLabel32: TQRLabel;
    QRLabel79: TQRLabel;
    qrlRazaoDest: TQRLabel;
    qrlEnderecoDest1: TQRLabel;
    qrlEnderecoDest2: TQRLabel;
    QRLabel93: TQRLabel;
    qrlInscEstRemet: TQRLabel;
    QRLabel95: TQRLabel;
    qrlFoneRemet: TQRLabel;
    qrlCEPRemet: TQRLabel;
    QRLabel98: TQRLabel;
    qrlMunDest: TQRLabel;
    qrlPaisDest: TQRLabel;
    qrlCnpjDest: TQRLabel;
    QRLabel114: TQRLabel;
    qrlInscEstDest: TQRLabel;
    QRLabel116: TQRLabel;
    qrlFoneDest: TQRLabel;
    qrlCEPDest: TQRLabel;
    QRLabel119: TQRLabel;
    QRLabel86: TQRLabel;
    QRLabel87: TQRLabel;
    QRLabel88: TQRLabel;
    QRLabel89: TQRLabel;
    QRLabel90: TQRLabel;
    qrlRazaoExped: TQRLabel;
    qrlEnderecoExped1: TQRLabel;
    qrlEnderecoExped2: TQRLabel;
    QRLabel99: TQRLabel;
    QRLabel101: TQRLabel;
    QRLabel102: TQRLabel;
    QRLabel103: TQRLabel;
    QRLabel104: TQRLabel;
    qrlRazaoReceb: TQRLabel;
    qrlEnderecoReceb1: TQRLabel;
    qrlEnderecoReceb2: TQRLabel;
    qrlMunExped: TQRLabel;
    qrlPaisExped: TQRLabel;
    qrlCnpjExped: TQRLabel;
    QRLabel105: TQRLabel;
    qrlInscEstExped: TQRLabel;
    QRLabel107: TQRLabel;
    qrlFoneExped: TQRLabel;
    qrlCEPExped: TQRLabel;
    QRLabel110: TQRLabel;
    qrlMunReceb: TQRLabel;
    qrlPaisReceb: TQRLabel;
    qrlCnpjReceb: TQRLabel;
    QRLabel123: TQRLabel;
    qrlInscEstReceb: TQRLabel;
    QRLabel125: TQRLabel;
    qrlFoneReceb: TQRLabel;
    qrlCEPReceb: TQRLabel;
    QRLabel128: TQRLabel;
    QRLabel80: TQRLabel;
    QRLabel81: TQRLabel;
    QRLabel94: TQRLabel;
    qrlCEPToma: TQRLabel;
    QRLabel97: TQRLabel;
    qrlEnderecoToma: TQRLabel;
    QRLabel82: TQRLabel;
    qrlCnpjToma: TQRLabel;
    QRLabel108: TQRLabel;
    qrlInscEstToma: TQRLabel;
    QRLabel111: TQRLabel;
    qrlFoneToma: TQRLabel;
    qrlRazaoToma: TQRLabel;
    QRLabel113: TQRLabel;
    qrlPaisToma: TQRLabel;
    qrlMunToma: TQRLabel;
    qrb_10_ModRodFracionado: TQRChildBand;
    qrb_11_ModRodLot103: TQRChildBand;
    QRLabel10: TQRLabel;
    QRShape1: TQRShape;
    qrmObs: TQRMemo;
    qrlTituloLotacao: TQRLabel;
    QRShape24: TQRShape;
    QRLabel11: TQRLabel;
    QRShape36: TQRShape;
    QRShape37: TQRShape;
    QRShape38: TQRShape;
    QRLabel83: TQRLabel;
    QRLabel84: TQRLabel;
    QRLabel85: TQRLabel;
    qrlRntrcEmpresa: TQRLabel;
    qrlLotacao: TQRLabel;
    qrlDtPrevEntrega: TQRLabel;
    qrmObsExcEmitente: TQRMemo;
    qrlMsgTeste: TQRLabel;
    QRLabel7: TQRLabel;
    QRShape27: TQRShape;
    qrb_03_DadosDACTe: TQRChildBand;
    QRLabel1: TQRLabel;
    qrlProdPredominante: TQRLabel;
    QRLabel4: TQRLabel;
    qrlOutrasCaracCarga: TQRLabel;
    QRLabel34: TQRLabel;
    qrlVlrTotalMerc: TQRLabel;
    QRLabel35: TQRLabel;
    QRLabel36: TQRLabel;
    QRLabel41: TQRLabel;
    QRLabel43: TQRLabel;
    QRLabel5: TQRLabel;
    QRLabel37: TQRLabel;
    QRLabel39: TQRLabel;
    QRLabel40: TQRLabel;
    QRShape8: TQRShape;
    QRShape7: TQRShape;
    qrb_04_DadosNotaFiscal: TQRChildBand;
    qrb_05_Complemento: TQRChildBand;
    QRLabel38: TQRLabel;
    QRLabel44: TQRLabel;
    qrmCompNome1: TQRMemo;
    QRLabel46: TQRLabel;
    qrmCompValor1: TQRMemo;
    QRLabel42: TQRLabel;
    qrmCompNome2: TQRMemo;
    QRLabel45: TQRLabel;
    qrmCompValor2: TQRMemo;
    QRLabel47: TQRLabel;
    qrmCompNome3: TQRMemo;
    QRLabel48: TQRLabel;
    qrmCompValor3: TQRMemo;
    QRLabel49: TQRLabel;
    qrlVlrTotServico: TQRLabel;
    QRLabel50: TQRLabel;
    qrlVlrTotReceber: TQRLabel;
    QRShape18: TQRShape;
    QRShape17: TQRShape;
    QRShape16: TQRShape;
    QRShape15: TQRShape;
    QRShape19: TQRShape;
    QRLabel51: TQRLabel;
    QRLabel52: TQRLabel;
    qrlSitTrib: TQRLabel;
    lblBC: TQRLabel;
    qrlBaseCalc: TQRLabel;
    lblAliq: TQRLabel;
    qrlAliqICMS: TQRLabel;
    lblValorICMS: TQRLabel;
    qrlVlrICMS: TQRLabel;
    lblRedBC: TQRLabel;
    qrlRedBaseCalc: TQRLabel;
    qrlICMSST: TQRLabel;
    qrlICMS_ST: TQRLabel;
    qrsICMSST: TQRShape;
    qrsRedBC: TQRShape;
    qrsValorICMS: TQRShape;
    qrsBC: TQRShape;
    QRShape21: TQRShape;
    qrsAliq: TQRShape;
    QRLabel59: TQRLabel;
    QRShape5: TQRShape;
    QRShape6: TQRShape;
    QRLabel61: TQRLabel;
    QRLabel62: TQRLabel;
    QRLabel63: TQRLabel;
    QRLabel64: TQRLabel;
    qrmComplChave1: TQRMemo;
    qrmComplValor1: TQRMemo;
    qrmComplChave2: TQRMemo;
    qrmComplValor2: TQRMemo;
    qrsQuadro01: TQRShape;
    qrsQuadro02: TQRShape;
    qrsQuadro04: TQRShape;
    qrsQuadro05: TQRShape;
    qrsQuadro07: TQRShape;
    qrsQuadro08: TQRShape;
    qrsQuadro09: TQRShape;
    qrsLinhaV10: TQRShape;
    qrsLinhaV04: TQRShape;
    qrsLinhaH01: TQRShape;
    qrsLinhaH02: TQRShape;
    qrsLinhaV09: TQRShape;
    qrsLinhaV08: TQRShape;
    qrsLinhaV06: TQRShape;
    qrsLinhaV05: TQRShape;
    qrsLinhaH04: TQRShape;
    qrsLinhaV07: TQRShape;
    qrsLinhaV01: TQRShape;
    qrsLinhaV11: TQRShape;
    qrsLinhaH06: TQRShape;
    qrsLinhaH07: TQRShape;
    qrsLinhaH05: TQRShape;
    qrsLinhaH08: TQRShape;
    QRShape55: TQRShape;
    QRShape9: TQRShape;
    QRShape56: TQRShape;
    QRShape58: TQRShape;
    QRShape59: TQRShape;
    QRShape60: TQRShape;
    QRShape61: TQRShape;
    QRShape62: TQRShape;
    qrb_17_Sistema: TQRChildBand;
    QRShape10: TQRShape;
    QRLabel65: TQRLabel;
    QRShape2: TQRShape;
    QRLabel66: TQRLabel;
    QRLabel70: TQRLabel;
    QRShape53: TQRShape;
    QRShape11: TQRShape;
    QRLabel71: TQRLabel;
    QRShape12: TQRShape;
    QRLabel75: TQRLabel;
    QRShape13: TQRShape;
    QRLabel76: TQRLabel;
    QRLabel112: TQRLabel;
    QRLabel115: TQRLabel;
    QRLabel117: TQRLabel;
    QRShape14: TQRShape;
    QRShape31: TQRShape;
    QRShape33: TQRShape;
    QRLabel118: TQRLabel;
    QRLabel120: TQRLabel;
    QRLabel121: TQRLabel;
    QRShape39: TQRShape;
    QRLabel122: TQRLabel;
    QRLabel124: TQRLabel;
    QRLabel126: TQRLabel;
    QRLabel127: TQRLabel;
    QRShape40: TQRShape;
    QRShape41: TQRShape;
    QRLabel129: TQRLabel;
    QRLabel130: TQRLabel;
    QRLabel131: TQRLabel;
    QRShape42: TQRShape;
    QRShape43: TQRShape;
    QRShape44: TQRShape;
    qrb_16_DadosExcEmitente: TQRChildBand;
    QRLabel15: TQRLabel;
    QRSysData1: TQRSysData;
    qrlblSistema: TQRLabel;
    qrlNomeMotorista: TQRLabel;
    qrlCPFMotorista: TQRLabel;
    qrlNumRegEsp: TQRLabel;
    qrlResponsavel: TQRLabel;
    qrlValorTotal: TQRLabel;
    qrlLacres: TQRLabel;
    qrmTipo: TQRMemo;
    qrmPlaca: TQRMemo;
    qrmUF: TQRMemo;
    qrmRNTRC: TQRMemo;
    qrmEmpresas: TQRMemo;
    qrmVigencias: TQRMemo;
    qrmNumDispositivo: TQRMemo;
    qrmCodTransacao: TQRMemo;
    QRShape45: TQRShape;
    qrmQtdUnidMedida1: TQRMemo;
    qrmQtdUnidMedida2: TQRMemo;
    qrmQtdUnidMedida3: TQRMemo;
    qrmQtdUnidMedida5: TQRMemo;
    qrb_06_ValorPrestacao: TQRChildBand;
    QRShape46: TQRShape;
    QRShape48: TQRShape;
    QRShape49: TQRShape;
    QRShape50: TQRShape;
    QRShape51: TQRShape;
    QRShape52: TQRShape;
    QRLabel3: TQRLabel;
    QRLabel132: TQRLabel;
    QRLabel133: TQRLabel;
    QRLabel134: TQRLabel;
    QRLabel135: TQRLabel;
    QRLabel136: TQRLabel;
    QRLabel137: TQRLabel;
    QRLabel138: TQRLabel;
    QRLabel139: TQRLabel;
    QRLabel140: TQRLabel;
    qrlSerie2: TQRLabel;
    qrlNumCTe2: TQRLabel;
    QRLabel143: TQRLabel;
    qrb_12_ModAereo: TQRChildBand;
    qrb_13_ModAquaviario: TQRChildBand;
    qrb_14_ModFerroviario: TQRChildBand;
    qrb_15_ModDutoviario: TQRChildBand;
    QRShape47: TQRShape;
    QRShape54: TQRShape;
    QRShape63: TQRShape;
    QRShape64: TQRShape;
    QRShape65: TQRShape;
    QRShape66: TQRShape;
    QRShape67: TQRShape;
    QRShape68: TQRShape;
    QRShape69: TQRShape;
    QRLabel141: TQRLabel;
    QRLabel142: TQRLabel;
    QRLabel144: TQRLabel;
    QRLabel145: TQRLabel;
    QRLabel146: TQRLabel;
    QRLabel147: TQRLabel;
    QRLabel148: TQRLabel;
    QRLabel149: TQRLabel;
    QRLabel150: TQRLabel;
    QRLabel153: TQRLabel;
    qrlTrecho: TQRLabel;
    qrlTarifaValor: TQRLabel;
    qrlTarifaCodigo: TQRLabel;
    qrlTarifaCL: TQRLabel;
    qrlMinuta: TQRLabel;
    qrlDadosRetira: TQRLabel;
    qrlAWB: TQRLabel;
    QRLabel154: TQRLabel;
    QRLabel155: TQRLabel;
    qrlCaracAdServico: TQRLabel;
    qrlCaracAdTransporte: TQRLabel;
    QRShape57: TQRShape;
    QRShape72: TQRShape;
    QRLabel156: TQRLabel;
    qrlContaCorrente: TQRLabel;
    QRLabel157: TQRLabel;
    qrlLojaAgenteEmissor: TQRLabel;
    qrlRetira: TQRLabel;
    QRShape70: TQRShape;
    QRShape71: TQRShape;
    QRShape73: TQRShape;
    QRLabel151: TQRLabel;
    QRShape74: TQRShape;
    QRLabel152: TQRLabel;
    qrlPortoEmbarque: TQRLabel;
    QRLabel158: TQRLabel;
    qrlPortoDestino: TQRLabel;
    QRShape75: TQRShape;
    QRLabel159: TQRLabel;
    qrlIndNavioRebocador: TQRLabel;
    QRShape76: TQRShape;
    QRLabel160: TQRLabel;
    qrlIndConteiners: TQRLabel;
    QRShape77: TQRShape;
    QRLabel162: TQRLabel;
    qrlBCAFRMM: TQRLabel;
    QRLabel164: TQRLabel;
    qrlValorAFRMM: TQRLabel;
    QRLabel166: TQRLabel;
    qrlTipoNav: TQRLabel;
    QRLabel168: TQRLabel;
    qrlDirecao: TQRLabel;
    QRShape78: TQRShape;
    QRShape79: TQRShape;
    QRShape80: TQRShape;
    qrb_01_Recibo_Aereo: TQRChildBand;
    QRLabel19: TQRLabel;
    QRShape81: TQRShape;
    QRShape82: TQRShape;
    QRLabel57: TQRLabel;
    QRLabel60: TQRLabel;
    QRLabel69: TQRLabel;
    QRLabel161: TQRLabel;
    QRLabel67: TQRLabel;
    QRLabel68: TQRLabel;
    QRLabel72: TQRLabel;
    QRLabel163: TQRLabel;
    QRShape3: TQRShape;
    QRLabel165: TQRLabel;
    lblCIOT: TQRLabel;
    qrlCIOT: TQRLabel;
    qrsCIOT: TQRShape;
    qrmObsFisco: TQRMemo;
    qrb_11_ModRodLot104: TQRChildBand;
    QRShape4: TQRShape;
    QRShape30: TQRShape;
    QRShape83: TQRShape;
    QRShape84: TQRShape;
    QRShape85: TQRShape;
    QRShape86: TQRShape;
    QRShape87: TQRShape;
    QRShape89: TQRShape;
    QRShape90: TQRShape;
    QRShape92: TQRShape;
    QRLabel167: TQRLabel;
    QRLabel169: TQRLabel;
    QRLabel170: TQRLabel;
    QRLabel171: TQRLabel;
    QRLabel172: TQRLabel;
    QRLabel173: TQRLabel;
    QRLabel174: TQRLabel;
    QRLabel177: TQRLabel;
    QRLabel179: TQRLabel;
    QRLabel181: TQRLabel;
    QRLabel182: TQRLabel;
    QRLabel183: TQRLabel;
    qrmUF2: TQRMemo;
    qrmTipo2: TQRMemo;
    qrmRNTRC2: TQRMemo;
    qrmPlaca2: TQRMemo;
    qrmCNPJForn: TQRMemo;
    qrmNumCompra: TQRMemo;
    qrlNomeMotorista2: TQRLabel;
    qrlLacres2: TQRLabel;
    qrlCPFMotorista2: TQRLabel;
    qrb_18_Recibo: TQRChildBand;
    QRShape91: TQRShape;
    QRShape93: TQRShape;
    QRShape94: TQRShape;
    QRShape95: TQRShape;
    QRShape96: TQRShape;
    QRShape97: TQRShape;
    QRLabel175: TQRLabel;
    QRLabel176: TQRLabel;
    QRLabel180: TQRLabel;
    QRLabel184: TQRLabel;
    QRLabel185: TQRLabel;
    QRLabel186: TQRLabel;
    QRLabel187: TQRLabel;
    QRLabel188: TQRLabel;
    QRLabel189: TQRLabel;
    QRLabel190: TQRLabel;
    QRLabel191: TQRLabel;
    qrlSerie3: TQRLabel;
    qrlNumCTe3: TQRLabel;
    QRShape98: TQRShape;
    qrmCNPJPg: TQRMemo;
    QRShape88: TQRShape;
    qrlVariavel1: TQRLabel;
    qriBarCode2: TQRImage;
    QRShape99: TQRShape;
    qrmQtdUnidMedida4: TQRMemo;
    QRLabel73: TQRLabel;
    QRShape100: TQRShape;
    qrsQuadro03: TQRShape;
    qrsLinhaPontilhada: TQRShape;
    QRLabel178: TQRLabel;
    qrlIndBalsas: TQRLabel;
    qrmNomeSeguradora: TQRMemo;
    qrmRespSeguroMerc: TQRMemo;
    qrmNroApolice: TQRMemo;
    qrmNroAverbacao: TQRMemo;
    qrb_06_ProdutosPerigosos: TQRChildBand;
    QRShape101: TQRShape;
    QRLabel192: TQRLabel;
    QRShape102: TQRShape;
    QRLabel193: TQRLabel;
    QRLabel194: TQRLabel;
    QRLabel195: TQRLabel;
    QRLabel196: TQRLabel;
    QRLabel197: TQRLabel;
    QRShape103: TQRShape;
    QRShape104: TQRShape;
    QRShape105: TQRShape;
    QRShape106: TQRShape;
    qrmNumONU: TQRMemo;
    qrmNomeApropriado: TQRMemo;
    qrmClasse: TQRMemo;
    qrmGrupoEmbalagem: TQRMemo;
    qrmQtdeProduto: TQRMemo;
    QRShape107: TQRShape;
    qrlResumoCanhotoCTe: TQRLabel;
    qrlResumoCanhotoCTe2: TQRLabel;
    procedure QRCTeBeforePrint(Sender: TCustomQuickRep; var PrintReport: Boolean);
    procedure qrb_01_ReciboBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_02_CabecalhoBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_03_DadosDACTeBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_04_DadosNotaFiscalBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_05_ComplementoBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_06_ValorPrestacaoBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_07_HeaderItensBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_08_ItensBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_09_ObsBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_10_ModRodFracionadoBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_11_ModRodLot103BeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_17_SistemaBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_16_DadosExcEmitenteBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_12_ModAereoBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_13_ModAquaviarioBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_14_ModFerroviarioBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_15_ModDutoviarioBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_01_Recibo_AereoBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_11_ModRodLot104BeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_18_ReciboBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_06_ProdutosPerigososBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
  private
    procedure Itens;
  public
    procedure ProtocoloCTe(const sProtocolo: String);
  end;

implementation

uses
  StrUtils, DateUtils, ACBrValidador, pcteConversaoCTe,
  ACBrUtil, ACBrDFeUtil, ACBrCTe;

{$R *.dfm}

var
  FProtocoloCTe : String;

const
{$IFDEF PL_103}
  Versao = 103;
{$ENDIF}
{$IFDEF PL_104}
  Versao = 104;
{$ENDIF}
{$IFDEF PL_200}
  Versao = 200;
{$ENDIF}

procedure TfrmDACTeQRRetrato.Itens;
var
  I, J, K, Item: Integer;
begin
  if QRCTe.PageNumber > 0 then
    exit;

  cdsDocumentos.CreateDataSet;
  cdsDocumentos.Open;
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

        cdsDocumentosTIPO_1.AsString      := 'NF';
        cdsDocumentosCNPJCPF_1.AsString   := FormatarCNPJouCPF(FCTe.Rem.CNPJCPF);
        cdsDocumentosDOCUMENTO_1.AsString := serie + '-' + nDoc;
      end
      else
      begin
        cdsDocumentosTIPO_2.AsString      := 'NF';
        cdsDocumentosCNPJCPF_2.AsString   := FormatarCNPJouCPF(FCTe.Rem.CNPJCPF);
        cdsDocumentosDOCUMENTO_2.AsString := serie + '-' + nDoc;

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
        cdsDocumentosTIPO_1.AsString    := 'NF-E ' + copy(chave, 26, 9);
        cdsDocumentosCNPJCPF_1.AsString := FormatarChaveAcesso(chave);
      end
      else
      begin
        cdsDocumentosTIPO_2.AsString    := 'NF-E ' + copy(chave, 26, 9);
        cdsDocumentosCNPJCPF_2.AsString := FormatarChaveAcesso(chave);
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
        case tpDoc of
         tdDeclaracao: begin
                        cdsDocumentosTIPO_1.AsString      := 'DECLAR';
                        cdsDocumentosCNPJCPF_1.AsString   := FormatarCNPJouCPF(FCTe.Rem.CNPJCPF);
                        cdsDocumentosDOCUMENTO_1.AsString := 'Declaração Doc.: ' + nDoc;
                       end;
         tdDutoviario: begin
                        cdsDocumentosTIPO_1.AsString      := 'DUTO';
                        cdsDocumentosCNPJCPF_1.AsString   := FormatarCNPJouCPF(FCTe.Rem.CNPJCPF);
                        cdsDocumentosDOCUMENTO_1.AsString := 'Dutoviário Doc.: ' + nDoc;
                       end;
         tdOutros:     begin
                        cdsDocumentosTIPO_1.AsString      := 'Outros';
                        cdsDocumentosCNPJCPF_1.AsString   := FormatarCNPJouCPF(FCTe.Rem.CNPJCPF);
                        cdsDocumentosDOCUMENTO_1.AsString := copy( trim(descOutros), 1, 20 ) + ' Doc.: '+ nDoc;
                       end;
        end;
      end
      else
      begin
        case tpDoc of
         tdDeclaracao: begin
                        cdsDocumentosTIPO_2.AsString      := 'DECLAR';
                        cdsDocumentosCNPJCPF_2.AsString   := FormatarCNPJouCPF(FCTe.Rem.CNPJCPF);
                        cdsDocumentosDOCUMENTO_2.AsString := 'Declaração Doc.: ' + nDoc;
                       end;
         tdDutoviario: begin
                        cdsDocumentosTIPO_2.AsString      := 'DUTO';
                        cdsDocumentosCNPJCPF_2.AsString   := FormatarCNPJouCPF(FCTe.Rem.CNPJCPF);
                        cdsDocumentosDOCUMENTO_2.AsString := 'Dutoviário Doc.: ' + nDoc;
                       end;
         tdOutros:     begin
                        cdsDocumentosTIPO_2.AsString      := 'Outros';
                        cdsDocumentosCNPJCPF_2.AsString   := FormatarCNPJouCPF(FCTe.Rem.CNPJCPF);
                        cdsDocumentosDOCUMENTO_2.AsString := copy( trim(descOutros), 1, 20 ) + ' Doc.: '+ nDoc;
                       end;
        end;
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
             daCTRC:   cdsDocumentosTIPO_1.AsString := 'CTRC';
             daCTAC:   cdsDocumentosTIPO_1.AsString := 'CTAC';
             daACT:    cdsDocumentosTIPO_1.AsString := 'ACT';
             daNF7:    cdsDocumentosTIPO_1.AsString := 'NF M7';
             daNF27:   cdsDocumentosTIPO_1.AsString := 'NF M27';
             daCAN:    cdsDocumentosTIPO_1.AsString := 'CAN';
             daCTMC:   cdsDocumentosTIPO_1.AsString := 'CTMC';
             daATRE:   cdsDocumentosTIPO_1.AsString := 'ATRE';
             daDTA:    cdsDocumentosTIPO_1.AsString := 'DTA';
             daCAI:    cdsDocumentosTIPO_1.AsString := 'CAI';
             daCCPI:   cdsDocumentosTIPO_1.AsString := 'CCPI';
             daCA:     cdsDocumentosTIPO_1.AsString := 'CA';
             daTIF:    cdsDocumentosTIPO_1.AsString := 'TIF';
             daOutros: cdsDocumentosTIPO_1.AsString := 'Outros';
            end;
            cdsDocumentosCNPJCPF_1.AsString   := FormatarCNPJouCPF(FCTe.infCTeNorm.docAnt.emiDocAnt.Items[I].CNPJCPF);
            cdsDocumentosDOCUMENTO_1.AsString := serie + '-' + IntToStr(nDoc);
          end
          else
          begin
            case tpDoc of
             daCTRC:   cdsDocumentosTIPO_2.AsString := 'CTRC';
             daCTAC:   cdsDocumentosTIPO_2.AsString := 'CTAC';
             daACT:    cdsDocumentosTIPO_2.AsString := 'ACT';
             daNF7:    cdsDocumentosTIPO_2.AsString := 'NF M7';
             daNF27:   cdsDocumentosTIPO_2.AsString := 'NF M27';
             daCAN:    cdsDocumentosTIPO_2.AsString := 'CAN';
             daCTMC:   cdsDocumentosTIPO_2.AsString := 'CTMC';
             daATRE:   cdsDocumentosTIPO_2.AsString := 'ATRE';
             daDTA:    cdsDocumentosTIPO_2.AsString := 'DTA';
             daCAI:    cdsDocumentosTIPO_2.AsString := 'CAI';
             daCCPI:   cdsDocumentosTIPO_2.AsString := 'CCPI';
             daCA:     cdsDocumentosTIPO_2.AsString := 'CA';
             daTIF:    cdsDocumentosTIPO_2.AsString := 'TIF';
             daOutros: cdsDocumentosTIPO_2.AsString := 'Outros';
            end;
            cdsDocumentosCNPJCPF_2.AsString   := FormatarCNPJouCPF(FCTe.infCTeNorm.docAnt.emiDocAnt.Items[I].CNPJCPF);
            cdsDocumentosDOCUMENTO_2.AsString := serie + '-' + IntToStr(nDoc);

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

            cdsDocumentosTIPO_1.AsString    := 'CT-E';
            cdsDocumentosCNPJCPF_1.AsString := FormatarChaveAcesso(chave);
          end
          else
          begin
            cdsDocumentosTIPO_2.AsString    := 'CT-E';
            cdsDocumentosCNPJCPF_2.AsString := FormatarChaveAcesso(chave);

            cdsDocumentos.Post;
          end;
          inc(Item);
        end;
      end;
    end;

  end;

  // Incluido por Italo em 27/05/2014
  // Varrendo NFe Anulada
  if FCTe.infCteAnu.chCTe <> '' then
  begin
   cdsDocumentos.Append;
   cdsDocumentosTIPO_1.AsString    := 'NF-E ' + copy(FCTe.infCteAnu.chCTe, 26, 9);
   cdsDocumentosCNPJCPF_1.AsString := FormatarChaveAcesso(FCTe.infCteAnu.chCTe);
//   cdsDocumentos.Post;
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

        cdsDocumentosTIPO_1.AsString      := 'NF';
        cdsDocumentosCNPJCPF_1.AsString   := FormatarCNPJCPF(FCTe.Rem.CNPJCPF);
        cdsDocumentosDOCUMENTO_1.AsString := serie + '-' + nDoc;
      end
      else
      begin
        cdsDocumentosTIPO_2.AsString      := 'NF';
        cdsDocumentosCNPJCPF_2.AsString   := FormatarCNPJCPF(FCTe.Rem.CNPJCPF);
        cdsDocumentosDOCUMENTO_2.AsString := serie + '-' + nDoc;

        cdsDocumentos.Post;
      end;
      inc(Item);
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
        cdsDocumentosTIPO_1.AsString    := 'NF-E ' + copy(chave, 26, 9);
        cdsDocumentosCNPJCPF_1.AsString := CTeUtil.FormatarChaveAcesso(chave, True);
      end
      else
      begin
        cdsDocumentosTIPO_2.AsString    := 'NF-E ' + copy(chave, 26, 9);
        cdsDocumentosCNPJCPF_2.AsString := CTeUtil.FormatarChaveAcesso(chave, True);
        cdsDocumentos.Post;
      end;
      inc(Item);
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
        case tpDoc of
         tdDeclaracao: begin
                        cdsDocumentosTIPO_1.AsString      := 'DECLAR';
                        cdsDocumentosCNPJCPF_1.AsString   := FormatarCNPJCPF(FCTe.Rem.CNPJCPF);
                        cdsDocumentosDOCUMENTO_1.AsString := 'Declaração Doc.: ' + nDoc;
                       end;
         tdDutoviario: begin
                        cdsDocumentosTIPO_1.AsString      := 'DUTO';
                        cdsDocumentosCNPJCPF_1.AsString   := FormatarCNPJCPF(FCTe.Rem.CNPJCPF);
                        cdsDocumentosDOCUMENTO_1.AsString := 'Dutoviário Doc.: ' + nDoc;
                       end;
         tdOutros:     begin
                        cdsDocumentosTIPO_1.AsString      := 'Outros';
                        cdsDocumentosCNPJCPF_1.AsString   := FormatarCNPJCPF(FCTe.Rem.CNPJCPF);
                        cdsDocumentosDOCUMENTO_1.AsString := copy( trim(descOutros), 1, 20 ) + ' Doc.: '+ nDoc;
                       end;
        end;
      end
      else
      begin
        case tpDoc of
         tdDeclaracao: begin
                        cdsDocumentosTIPO_2.AsString      := 'DECLAR';
                        cdsDocumentosCNPJCPF_2.AsString   := FormatarCNPJCPF(FCTe.Rem.CNPJCPF);
                        cdsDocumentosDOCUMENTO_2.AsString := 'Declaração Doc.: ' + nDoc;
                       end;
         tdDutoviario: begin
                        cdsDocumentosTIPO_2.AsString      := 'DUTO';
                        cdsDocumentosCNPJCPF_2.AsString   := FormatarCNPJCPF(FCTe.Rem.CNPJCPF);
                        cdsDocumentosDOCUMENTO_2.AsString := 'Dutoviário Doc.: ' + nDoc;
                       end;
         tdOutros:     begin
                        cdsDocumentosTIPO_2.AsString      := 'Outros';
                        cdsDocumentosCNPJCPF_2.AsString   := FormatarCNPJCPF(FCTe.Rem.CNPJCPF);
                        cdsDocumentosDOCUMENTO_2.AsString := copy( trim(descOutros), 1, 20 ) + ' Doc.: '+ nDoc;
                       end;
        end;
        cdsDocumentos.Post;
      end;
      inc(Item);
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
             daCTRC:   cdsDocumentosTIPO_1.AsString := 'CTRC';
             daCTAC:   cdsDocumentosTIPO_1.AsString := 'CTAC';
             daACT:    cdsDocumentosTIPO_1.AsString := 'ACT';
             daNF7:    cdsDocumentosTIPO_1.AsString := 'NF M7';
             daNF27:   cdsDocumentosTIPO_1.AsString := 'NF M27';
             daCAN:    cdsDocumentosTIPO_1.AsString := 'CAN';
             daCTMC:   cdsDocumentosTIPO_1.AsString := 'CTMC';
             daATRE:   cdsDocumentosTIPO_1.AsString := 'ATRE';
             daDTA:    cdsDocumentosTIPO_1.AsString := 'DTA';
             daCAI:    cdsDocumentosTIPO_1.AsString := 'CAI';
             daCCPI:   cdsDocumentosTIPO_1.AsString := 'CCPI';
             daCA:     cdsDocumentosTIPO_1.AsString := 'CA';
             daTIF:    cdsDocumentosTIPO_1.AsString := 'TIF';
             daOutros: cdsDocumentosTIPO_1.AsString := 'Outros';
            end;
            cdsDocumentosCNPJCPF_1.AsString   := FormatarCNPJCPF(FCTe.infCTeNorm.emiDocAnt.Items[I].CNPJCPF);
            cdsDocumentosDOCUMENTO_1.AsString := serie + '-' + IntToStr(nDoc);
          end
          else
          begin
            case tpDoc of
             daCTRC:   cdsDocumentosTIPO_2.AsString := 'CTRC';
             daCTAC:   cdsDocumentosTIPO_2.AsString := 'CTAC';
             daACT:    cdsDocumentosTIPO_2.AsString := 'ACT';
             daNF7:    cdsDocumentosTIPO_2.AsString := 'NF M7';
             daNF27:   cdsDocumentosTIPO_2.AsString := 'NF M27';
             daCAN:    cdsDocumentosTIPO_2.AsString := 'CAN';
             daCTMC:   cdsDocumentosTIPO_2.AsString := 'CTMC';
             daATRE:   cdsDocumentosTIPO_2.AsString := 'ATRE';
             daDTA:    cdsDocumentosTIPO_2.AsString := 'DTA';
             daCAI:    cdsDocumentosTIPO_2.AsString := 'CAI';
             daCCPI:   cdsDocumentosTIPO_2.AsString := 'CCPI';
             daCA:     cdsDocumentosTIPO_2.AsString := 'CA';
             daTIF:    cdsDocumentosTIPO_2.AsString := 'TIF';
             daOutros: cdsDocumentosTIPO_2.AsString := 'Outros';
            end;
            cdsDocumentosCNPJCPF_2.AsString   := FormatarCNPJCPF(FCTe.infCTeNorm.emiDocAnt.Items[I].CNPJCPF);
            cdsDocumentosDOCUMENTO_2.AsString := serie + '-' + IntToStr(nDoc);

            cdsDocumentos.Post;
          end;
          inc(Item);
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

            cdsDocumentosTIPO_1.AsString    := 'CT-E';
            cdsDocumentosCNPJCPF_1.AsString := CTeUtil.FormatarChaveAcesso(chave, True);
          end
          else
          begin
            cdsDocumentosTIPO_2.AsString    := 'CT-E';
            cdsDocumentosCNPJCPF_2.AsString := CTeUtil.FormatarChaveAcesso(chave, True);

            cdsDocumentos.Post;
          end;
          inc(Item);
        end;
      end;
    end;
  end;
{$ENDIF}

  cdsDocumentos.First;
end;

procedure TfrmDACTeQRRetrato.ProtocoloCTe(const sProtocolo: String);
begin
  FProtocoloCTe := sProtocolo;
end;

procedure TfrmDACTeQRRetrato.QRCTeBeforePrint(Sender: TCustomQuickRep; var PrintReport: Boolean);
begin
  inherited;

  Itens;

  qrb_01_Recibo.Enabled       := (FCTe.Ide.tpCTe = tcNormal) or (FCTe.Ide.tpCTe = tcComplemento);
  qrb_01_Recibo_Aereo.Enabled := (FCTe.Ide.tpCTe = tcNormal) or (FCTe.Ide.tpCTe = tcComplemento);
  qrb_18_Recibo.Enabled       := ((FCTe.Ide.tpCTe = tcNormal) or (FCTe.Ide.tpCTe = tcComplemento)) and
                                  (FCTe.Ide.modal <> mdAereo) and (FPosRecibo = prRodape);

  qrb_05_Complemento.Height := 0;

  if FCTe.Ide.tpCTe = tcComplemento
   then qrb_05_Complemento.Height := 87;

  qrb_06_ProdutosPerigosos.Height := 0;
{$IFDEF PL_200}
  if FCTe.infCTeNorm.peri.Count > 0
   then qrb_06_ProdutosPerigosos.Height := 82;
{$ELSE}
  if FCTe.peri.Count > 0
   then qrb_06_ProdutosPerigosos.Height := 82;
{$ENDIF}

  qrb_10_ModRodFracionado.Height := 0;
  qrb_11_ModRodLot103.Height     := 0;
  qrb_11_ModRodLot104.Height     := 0;
  qrb_12_ModAereo.Height         := 0;
  qrb_13_ModAquaviario.Height    := 0;
  qrb_14_ModFerroviario.Height   := 0;
  qrb_15_ModDutoviario.Height    := 0;

  case FCTe.Ide.modal of
   mdRodoviario: begin
                   qrb_10_ModRodFracionado.Height := 44;
                 {$IFDEF PL_200}
                   if FCTe.infCTeNorm.rodo.lota = ltSim
                 {$ELSE}
                   if FCTe.Rodo.Lota = ltSim
                 {$ENDIF}
                    then begin
                      if Versao = 103 then
                        qrb_11_ModRodLot103.Height := 108
                      else
                        qrb_11_ModRodLot104.Height := 105;
                    end;
                 end;
   mdAereo: begin
             qrb_12_ModAereo.Height := 97;
            end;
   mdAquaviario: begin
                  qrb_13_ModAquaviario.Height := 90;
                 end;
   mdFerroviario: begin
                   qrb_14_ModFerroviario.Height := 0;
                  end;
   mdDutoviario: begin
                  qrb_15_ModDutoviario.Height := 0;
                 end;
  end;

  QRCTe.ReportTitle:= 'CT-e: ' + FormatFloat('000,000,000', FCTe.Ide.nCT);

  QRCTe.Page.TopMargin    := FMargemSuperior * 100;
  QRCTe.Page.BottomMargin := FMargemInferior * 100;
  QRCTe.Page.LeftMargin   := FMargemEsquerda * 100;
  QRCTe.Page.RightMargin  := FMargemDireita  * 100;
end;

procedure TfrmDACTeQRRetrato.qrb_01_ReciboBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;
  PrintBand := (QRCTe.PageNumber = 1) and (FCTe.Ide.modal <> mdAereo) and (FPosRecibo = prCabecalho);
  //Rodrigo DSP 22/01/2014 11:22:47: impressao do canhoto - Cabecalho.
  if (FResumoCanhoto) then
    qrlResumoCanhotoCTe.Caption := getTextoResumoCanhoto
  else
    qrlResumoCanhotoCTe.Caption := '';

  qrlSerie2.Caption  := IntToStr(FCTe.Ide.serie); // FormatFloat( '000', FCTe.Ide.serie);
  qrlNumCte2.Caption := FormatFloat('000,000,000', FCTe.Ide.nCT);

  qrb_01_Recibo.Enabled := (FCTe.Ide.tpCTe = tcNormal) or (FCTe.Ide.tpCTe = tcComplemento);
end;

procedure TfrmDACTeQRRetrato.qrb_01_Recibo_AereoBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;
  PrintBand := (QRCTe.PageNumber = 1) and (FCTe.Ide.modal = mdAereo);

  qrb_01_Recibo_Aereo.Enabled := (FCTe.Ide.tpCTe = tcNormal) or (FCTe.Ide.tpCTe = tcComplemento);
end;

procedure TfrmDACTeQRRetrato.qrb_02_CabecalhoBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
var
 strChaveContingencia: String;
 vStringStream: TStringStream;
begin
  inherited;

  // Alterado por Italo em 15/01/2014
  if (FLogo <> '') then
    begin
      if FilesExists(FLogo) then
        qriLogo.Picture.LoadFromFile(FLogo)
      else
        begin
          vStringStream := TStringStream.Create(FLogo);
          try
            try
              qriLogo.Picture.Bitmap.LoadFromStream(vStringStream);
            except
            end;
          finally
            vStringStream.Free;
          end;
        end;
    end;

  if FExpandirLogoMarca then
   begin
    qriLogo.top    := 2;
    qriLogo.Left   := 2;
    qriLogo.Height := 142;
    qriLogo.Width  := 330;

    qriLogo.Stretch          := True;
    qrmEmitente.Enabled      := False;
    qrmDadosEmitente.Enabled := False;
   end;

  qrlModal.Caption      := TpModalToStrText(FCTe.Ide.modal);
  qrlModelo.Caption     := FCTe.Ide.modelo;
  qrlSerie.Caption      := IntToStr(FCTe.Ide.serie);  //FormatFloat( '000', FCTe.Ide.serie);
  qrlNumCte.Caption     := FormatFloat('000,000,000', FCTe.Ide.nCT);
  qrlPageNumber.Caption := format('%2.2d', [QRCTe.PageNumber]) + '/' + format('%2.2d', [FTotalPages]);
  qrlEmissao.Caption    := FormatDateTime('dd/mm/yyyy hh:nn', FCTe.Ide.dhEmi);

  SetBarCodeImage(Copy(FCTe.InfCTe.Id, 4, 44), qriBarCode);
  qrlChave.Caption := FormatarChaveAcesso(Copy(FCTe.InfCTe.Id, 4, 44));

  if not FExpandirLogoMarca then
   begin
    qrmEmitente.Enabled:=True;
    qrmDadosEmitente.Enabled:=True;
    // Emitente
    with FCTe.Emit do
    begin
      qrmEmitente.Lines.Text := XNome;

      qrmDadosEmitente.Lines.Clear;
      with EnderEmit do
      begin
        qrmDadosEmitente.Lines.Add(XLgr + IfThen(Nro = '0', '', ', ' + Nro));
        if XCpl <> '' then qrmDadosEmitente.Lines.Add(XCpl);
        if XBairro <> '' then qrmDadosEmitente.Lines.Add(XBairro);
        qrmDadosEmitente.Lines.Add('CEP: ' + FormatarCEP(FormatFloat( '00000000', CEP )) + ' - ' + XMun + ' - ' + UF);
      end;
      qrmDadosEmitente.Lines.Add('CNPJ: ' + FormatarCNPJ(CNPJ));
      qrmDadosEmitente.Lines.Add('INSCRIÇÃO ESTADUAL: ' + IE);
      qrmDadosEmitente.Lines.Add('TELEFONE: ' + FormatarFone(EnderEmit.Fone));

      if Trim(FUrl) <> '' then
        qrmDadosEmitente.Lines.Add('SITE: ' + FUrl);
      if Trim(FEmail) <> '' then
        qrmDadosEmitente.Lines.Add('E-MAIL: ' + FEmail);
    end;
   end;

  qrlTipoCte.Caption     := tpCTToStrText(FCTe.Ide.tpCTe);
  qrlTipoServico.Caption := TpServToStrText(FCTe.Ide.tpServ);

  if FCTe.Ide.Toma4.xNome = ''
   then qrlTomaServico.Caption := TpTomadorToStrText(FCTe.Ide.Toma03.Toma)
   else qrlTomaServico.Caption := TpTomadorToStrText(FCTe.Ide.Toma4.toma);

  qrlFormaPagamento.Caption := tpforPagToStrText(FCTe.Ide.forPag);

  // Normal **************************************************************
  if FCTe.Ide.tpEmis in [teNormal, teSCAN, teSVCSP, teSVCRS] then
   begin
    qrlVariavel1.Enabled := True;
    qriBarCode2.Enabled  := False;

    if FCTe.procCTe.cStat = 100
     then qrlDescricao.Caption := 'PROTOCOLO DE AUTORIZAÇÃO DE USO';

    if FCTe.procCTe.cStat = 101
     then qrlDescricao.Caption := 'PROTOCOLO DE HOMOLOGAÇÃO DE CANCELAMENTO';

    if FCTe.procCTe.cStat = 110
     then qrlDescricao.Caption := 'PROTOCOLO DE DENEGAÇÃO DE USO';

    if FProtocoloCTE <> ''
     then qrlProtocolo.Caption := FProtocoloCTE
     else qrlProtocolo.Caption := FCTe.procCTe.nProt + '   ' +
                                  ifThen(FCTe.procCTe.dhRecbto <> 0,
                                      DateTimeToStr(FCTe.procCTe.dhRecbto), '');
   end;

  // Contingencia ********************************************************
  if FCTe.Ide.tpEmis in [teContingencia, teFSDA] then
   begin
    if FCTe.procCTe.cStat in [100, 101, 110]
     then begin
      qrlVariavel1.Enabled := True;
      qriBarCode2.Enabled  := False;

      if FCTe.procCTe.cStat = 100
       then qrlDescricao.Caption := 'PROTOCOLO DE AUTORIZAÇÃO DE USO';

      if FCTe.procCTe.cStat = 101
       then qrlDescricao.Caption := 'PROTOCOLO DE HOMOLOGAÇÃO DE CANCELAMENTO';

      if FCTe.procCTe.cStat = 110
       then qrlDescricao.Caption := 'PROTOCOLO DE DENEGAÇÃO DE USO';

      if FProtocoloCTE <> ''
       then qrlProtocolo.Caption := FProtocoloCTE
       else qrlProtocolo.Caption := FCTe.procCTe.nProt + '   ' +
                                    ifThen(FCTe.procCTe.dhRecbto <> 0,
                                      DateTimeToStr(FCTe.procCTe.dhRecbto), '');
     end
     else begin
      qrlVariavel1.Enabled := False;
      qriBarCode2.Enabled  := True;
      strChaveContingencia := TACBrCTe(FACBrCTe).GerarChaveContingencia(FCTe);

      SetBarCodeImage(strChaveContingencia, qriBarCode2);
      qrlDescricao.Caption := 'DADOS DO CT-E';
      qrlProtocolo.Caption := FormatarChaveAcesso(strChaveContingencia);
     end;
   end;

  // EPEC ****************************************************************
  if FCTe.Ide.tpEmis = teDPEC then
   begin
      qrlVariavel1.Enabled := False;
      qriBarCode2.Enabled  := True;
      strChaveContingencia := TACBrCTe(FACBrCTe).GerarChaveContingencia(FCTe);

      SetBarCodeImage(strChaveContingencia, qriBarCode2);
      qrlDescricao.Caption := 'DADOS DO CT-E';
      qrlProtocolo.Caption := FormatarChaveAcesso(strChaveContingencia);
   end;

  qrlInscSuframa.Caption := FCTe.Dest.ISUF;
end;

procedure TfrmDACTeQRRetrato.qrb_03_DadosDACTeBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
var
 i: Integer;
begin
  inherited;
  PrintBand := QRCTe.PageNumber = 1;

  qrlNatOperacao.Caption   := FormatFloat('0000', FCTe.Ide.CFOP) + ' - ' + FCTe.Ide.natOp;
  qrlOrigPrestacao.Caption := FCTe.Ide.xMunIni + ' - ' + FCTe.Ide.UFIni + ' - ' + FormatFloat('000', FCTe.Ide.cMunIni);
  qrlDestPrestacao.Caption := FCTe.Ide.xMunFim + ' - ' + FCTe.Ide.UFFim + ' - ' + FormatFloat('000', FCTe.Ide.cMunFim);

  //DADOS REMETENTE
  qrlRazaoRemet.Caption     := FCTe.Rem.xNome;
  qrlEnderecoRemet1.Caption := FCTe.Rem.EnderReme.xLgr + ', ' + FCTe.Rem.EnderReme.nro;
  qrlEnderecoRemet2.Caption := FCTe.Rem.EnderReme.xCpl + ' - ' + FCTe.Rem.EnderReme.xBairro;
  qrlCEPRemet.Caption       := FormatarCEP(FormatFloat('00000000', FCTe.Rem.EnderReme.CEP));
  qrlMunRemet.Caption       := FCTe.Rem.EnderReme.xMun + ' - ' + FCTe.Rem.EnderReme.UF;
  qrlCnpjRemet.Caption      := FormatarCNPJouCPF(FCTe.Rem.CNPJCPF);
  qrlPaisRemet.Caption      := FCTe.Rem.EnderReme.xPais;
  qrlInscEstRemet.Caption   := FCTe.Rem.IE;
  qrlFoneRemet.Caption      := FormatarFone(FCTe.Rem.fone);

  //DADOS DESTINATARIO
  qrlRazaoDest.Caption     := FCTe.Dest.xNome;
  qrlEnderecoDest1.Caption := FCTe.Dest.EnderDest.xLgr + ', ' + FCTe.Dest.EnderDest.nro;
  qrlEnderecoDest2.Caption := FCTe.Dest.EnderDest.xCpl + ' - ' + FCTe.Dest.EnderDest.xBairro;
  qrlCEPDest.Caption       := FormatarCEP(FormatFloat('00000000', FCTe.Dest.EnderDest.CEP));
  qrlMunDest.Caption       := FCTe.Dest.EnderDest.xMun + ' - ' + FCTe.Dest.EnderDest.UF;
  qrlCnpjDest.Caption      := FormatarCNPJouCPF(FCTe.Dest.CNPJCPF);
  qrlPaisDest.Caption      := FCTe.Dest.EnderDest.xPais;
  qrlInscEstDest.Caption   := FCTe.Dest.IE;
  qrlFoneDest.Caption      := FormatarFone(FCTe.Dest.fone);

  //DADOS EXPEDIDOR
  qrlRazaoExped.Caption     := FCTe.Exped.xNome;
  qrlEnderecoExped1.Caption := FCTe.Exped.EnderExped.xLgr + ', ' + FCTe.Exped.EnderExped.nro;
  qrlEnderecoExped2.Caption := FCTe.Exped.EnderExped.xCpl + ' - ' + FCTe.Exped.EnderExped.xBairro;
  qrlCEPExped.Caption       := FormatarCEP(FormatFloat('00000000', FCTe.Exped.EnderExped.CEP));
  qrlMunExped.Caption       := FCTe.Exped.EnderExped.xMun + ' - ' + FCTe.Exped.EnderExped.UF;
  qrlCnpjExped.Caption      := FormatarCNPJouCPF(FCTe.Exped.CNPJCPF);
  qrlPaisExped.Caption      := FCTe.Exped.EnderExped.xPais;
  qrlInscEstExped.Caption   := FCTe.Exped.IE;
  qrlFoneExped.Caption      := FormatarFone(FCTe.Exped.fone);

  //DADOS RECEBEDOR
  qrlRazaoReceb.Caption     := FCTe.Receb.xNome;
  qrlEnderecoReceb1.Caption := FCTe.Receb.EnderReceb.xLgr + ', ' + FCTe.Receb.EnderReceb.nro;
  qrlEnderecoReceb2.Caption := FCTe.Receb.EnderReceb.xCpl + ' - ' + FCTe.Receb.EnderReceb.xBairro;
  qrlCEPReceb.Caption       := FormatarCEP(FormatFloat('00000000', FCTe.Receb.EnderReceb.CEP));
  qrlMunReceb.Caption       := FCTe.Receb.EnderReceb.xMun + ' - ' + FCTe.Receb.EnderReceb.UF;
  qrlCnpjReceb.Caption      := FormatarCNPJouCPF(FCTe.Receb.CNPJCPF);
  qrlPaisReceb.Caption      := FCTe.Receb.EnderReceb.xPais;
  qrlInscEstReceb.Caption   := FCTe.Receb.IE;
  qrlFoneReceb.Caption      := FormatarFone(FCTe.Receb.fone);

  if FCTe.Ide.Toma4.xNome = ''
   then begin
    case FCTe.Ide.Toma03.Toma of
    tmRemetente:
      begin
        qrlRazaoToma.Caption    := FCTe.Rem.xNome;
        qrlEnderecoToma.Caption := FCTe.Rem.EnderReme.xLgr + ', ' + FCTe.Rem.EnderReme.nro + ' - ' + FCTe.Rem.EnderReme.xCpl + ' - ' + FCTe.Rem.EnderReme.xBairro;
        qrlCEPToma.Caption      := FormatarCEP(FormatFloat('00000000', FCTe.Rem.EnderReme.CEP));
        qrlMunToma.Caption      := FCTe.Rem.EnderReme.xMun + ' - ' + FCTe.Rem.EnderReme.UF;
        qrlCnpjToma.Caption     := FormatarCNPJouCPF(FCTe.Rem.CNPJCPF);
        qrlPaisToma.Caption     := FCTe.Rem.EnderReme.xPais;
        qrlInscEstToma.Caption  := FCTe.Rem.IE;
        qrlFoneToma.Caption     := FormatarFone(FCTe.Rem.fone);
      end;
    tmExpedidor:
      begin
        qrlRazaoToma.Caption    := FCTe.Exped.xNome;
        qrlEnderecoToma.Caption := FCTe.Exped.EnderExped.xLgr + ', ' + FCTe.Exped.EnderExped.nro + ' - ' + FCTe.Exped.EnderExped.xCpl + ' - ' + FCTe.Exped.EnderExped.xBairro;
        qrlCEPToma.Caption      := FormatarCEP(FormatFloat('00000000', FCTe.Exped.EnderExped.CEP));
        qrlMunToma.Caption      := FCTe.Exped.EnderExped.xMun + ' - ' + FCTe.Exped.EnderExped.UF;
        qrlCnpjToma.Caption     := FormatarCNPJouCPF(FCTe.Exped.CNPJCPF);
        qrlPaisToma.Caption     := FCTe.Exped.EnderExped.xPais;
        qrlInscEstToma.Caption  := FCTe.Exped.IE;
        qrlFoneToma.Caption     := FormatarFone(FCTe.Exped.fone);
      end;
    tmRecebedor:
      begin
        qrlRazaoToma.Caption    := FCTe.Receb.xNome;
        qrlEnderecoToma.Caption := FCTe.Receb.EnderReceb.xLgr + ', ' + FCTe.Receb.EnderReceb.nro + ' - ' + FCTe.Receb.EnderReceb.xCpl + ' - ' + FCTe.Receb.EnderReceb.xBairro;
        qrlCEPToma.Caption      := FormatarCEP(FormatFloat('00000000', FCTe.Receb.EnderReceb.CEP));
        qrlMunToma.Caption      := FCTe.Receb.EnderReceb.xMun + ' - ' + FCTe.Receb.EnderReceb.UF;
        qrlCnpjToma.Caption     := FormatarCNPJouCPF(FCTe.Receb.CNPJCPF);
        qrlPaisToma.Caption     := FCTe.Receb.EnderReceb.xPais;
        qrlInscEstToma.Caption  := FCTe.Receb.IE;
        qrlFoneToma.Caption     := FormatarFone(FCTe.Receb.fone);
      end;
    tmDestinatario:
      begin
        qrlRazaoToma.Caption    := FCTe.Dest.xNome;
        qrlEnderecoToma.Caption := FCTe.Dest.EnderDest.xLgr + ', ' + FCTe.Dest.EnderDest.nro + ' - ' + FCTe.Dest.EnderDest.xCpl + ' - ' + FCTe.Dest.EnderDest.xBairro;
        qrlCEPToma.Caption      := FormatarCEP(FormatFloat('00000000', FCTe.Dest.EnderDest.CEP));
        qrlMunToma.Caption      := FCTe.Dest.EnderDest.xMun + ' - ' + FCTe.Dest.EnderDest.UF;
        qrlCnpjToma.Caption     := FormatarCNPJouCPF(FCTe.Dest.CNPJCPF);
        qrlPaisToma.Caption     := FCTe.Dest.EnderDest.xPais;
        qrlInscEstToma.Caption  := FCTe.Dest.IE;
        qrlFoneToma.Caption     := FormatarFone(FCTe.Dest.fone);
      end;
    end;
   end
   else begin
    qrlRazaoToma.Caption    := FCTe.Ide.Toma4.xNome;
    qrlEnderecoToma.Caption := FCTe.Ide.Toma4.EnderToma.xLgr + ', ' + FCTe.Ide.Toma4.EnderToma.nro + ' - ' + FCTe.Ide.Toma4.EnderToma.xCpl + ' - ' + FCTe.Ide.Toma4.EnderToma.xBairro;
    qrlCEPToma.Caption      := FormatarCEP(FormatFloat('00000000', FCTe.Ide.Toma4.EnderToma.CEP));
    qrlMunToma.Caption      := FCTe.Ide.Toma4.EnderToma.xMun + ' - ' + FCTe.Ide.Toma4.EnderToma.UF;
    qrlCnpjToma.Caption     := FormatarCNPJouCPF(FCTe.Ide.Toma4.CNPJCPF);
    qrlPaisToma.Caption     := FCTe.Ide.Toma4.EnderToma.xPais;
    qrlInscEstToma.Caption  := FCTe.Ide.Toma4.IE;
    qrlFoneToma.Caption     := FormatarFone(FCTe.Ide.Toma4.fone);
   end;

{$IFDEF PL_200}
  qrlProdPredominante.Caption := FCTe.infCTeNorm.infCarga.proPred;
  qrlOutrasCaracCarga.Caption := FCTe.infCTeNorm.InfCarga.xOutCat;
{$ELSE}
  qrlProdPredominante.Caption := FCTe.InfCarga.proPred;
  qrlOutrasCaracCarga.Caption := FCTe.InfCarga.xOutCat;
{$ENDIF}
{$IFDEF PL_103}
  qrlVlrTotalMerc.Caption := FormatFloat('###,###,###,##0.00', FCTe.InfCarga.vMerc);
{$ENDIF}
{$IFDEF PL_104}
  qrlVlrTotalMerc.Caption := FormatFloat('###,###,###,##0.00', FCTe.InfCarga.vCarga);
{$ENDIF}
{$IFDEF PL_200}
  qrlVlrTotalMerc.Caption := FormatFloat('###,###,###,##0.00', FCTe.infCTeNorm.infCarga.vCarga);
{$ENDIF}

  qrmQtdUnidMedida1.Lines.Clear;
  qrmQtdUnidMedida2.Lines.Clear;
  qrmQtdUnidMedida3.Lines.Clear;
  qrmQtdUnidMedida4.Lines.Clear;
  qrmQtdUnidMedida5.Lines.Clear;

  qrmNomeSeguradora.Lines.Clear;
  qrmRespSeguroMerc.Lines.Clear;
  qrmNroApolice.Lines.Clear;
  qrMNroAverbacao.Lines.Clear;

  qrmCompNome1.Lines.Clear;
  qrmCompNome2.Lines.Clear;
  qrmCompNome3.Lines.Clear;
  qrmCompValor1.Lines.Clear;
  qrmCompValor2.Lines.Clear;
  qrmCompValor3.Lines.Clear;

{$IFDEF PL_200}
  for i := 0 to (FCTe.infCTeNorm.InfCarga.InfQ.Count - 1) do
   begin
    case FCTe.infCTeNorm.InfCarga.InfQ.Items[i].cUnid of
          uM3: qrmQtdUnidMedida4.Lines.Add(FormatFloat('###,##0.000',
                 FCTe.infCTeNorm.InfCarga.InfQ.Items[i].qCarga));
          uKg: begin
                if uppercase(trim(FCTe.infCTeNorm.InfCarga.InfQ.Items[i].tpMed)) = 'PESO BRUTO'
                then qrmQtdUnidMedida1.Lines.Add(FormatFloat('###,##0.000',
                        FCTe.infCTeNorm.InfCarga.InfQ.Items[i].qCarga))
                else
                if uppercase(trim(FCTe.infCTeNorm.InfCarga.InfQ.Items[i].tpMed)) = 'PESO BASE DE CALCULO'
                then qrmQtdUnidMedida2.Lines.Add(FormatFloat('###,##0.000',
                        FCTe.infCTeNorm.InfCarga.InfQ.Items[i].qCarga))
                else
                if uppercase(trim(FCTe.infCTeNorm.InfCarga.InfQ.Items[i].tpMed)) = 'PESO BC'
                then qrmQtdUnidMedida2.Lines.Add(FormatFloat('###,##0.000',
                        FCTe.infCTeNorm.InfCarga.InfQ.Items[i].qCarga))
                else qrmQtdUnidMedida3.Lines.Add(FormatFloat('###,##0.000',
                        FCTe.infCTeNorm.InfCarga.InfQ.Items[i].qCarga));
               end;
         uTON: begin
                if uppercase(trim(FCTe.infCTeNorm.InfCarga.InfQ.Items[i].tpMed)) = 'PESO BRUTO'
                then qrmQtdUnidMedida1.Lines.Add(FormatFloat('###,##0.000',
                        FCTe.infCTeNorm.InfCarga.InfQ.Items[i].qCarga * 1000))
                else
                if uppercase(trim(FCTe.infCTeNorm.InfCarga.InfQ.Items[i].tpMed)) = 'PESO BASE DE CALCULO'
                then qrmQtdUnidMedida2.Lines.Add(FormatFloat('###,##0.000',
                        FCTe.infCTeNorm.InfCarga.InfQ.Items[i].qCarga * 1000))
                else
                if uppercase(trim(FCTe.infCTeNorm.InfCarga.InfQ.Items[i].tpMed)) = 'PESO BC'
                then qrmQtdUnidMedida2.Lines.Add(FormatFloat('###,##0.000',
                        FCTe.infCTeNorm.InfCarga.InfQ.Items[i].qCarga * 1000))
                else qrmQtdUnidMedida3.Lines.Add(FormatFloat('###,##0.000',
                        FCTe.infCTeNorm.InfCarga.InfQ.Items[i].qCarga * 1000));
               end;
     uUNIDADE: qrmQtdUnidMedida5.Lines.Add(FormatFloat('###,##0.000',
                 FCTe.infCTeNorm.InfCarga.InfQ.Items[i].qCarga) + '/' + FCTe.infCTeNorm.InfCarga.InfQ.Items[i].tpMed);
     uLITROS:  qrmQtdUnidMedida5.Lines.Add(FormatFloat('###,##0.000',
                 FCTe.infCTeNorm.InfCarga.InfQ.Items[i].qCarga) + '/' + FCTe.infCTeNorm.InfCarga.InfQ.Items[i].tpMed);
     uMMBTU:   qrmQtdUnidMedida5.Lines.Add(FormatFloat('###,##0.000',
                 FCTe.infCTeNorm.InfCarga.InfQ.Items[i].qCarga) + '/' + FCTe.infCTeNorm.InfCarga.InfQ.Items[i].tpMed);
    end;
   end;

  if FCTe.infCTeNorm.seg.Count > 0 then
  begin
    for i := 0 to FCTe.infCTeNorm.seg.Count-1 do
     begin
      qrmNomeSeguradora.Lines.Add(FCTe.infCTeNorm.seg.Items[i].xSeg);
      qrmRespSeguroMerc.Lines.Add(TpRspSeguroToStrText(FCTe.infCTeNorm.seg.Items[i].respSeg));
      qrmNroApolice.Lines.Add(FCTe.infCTeNorm.seg.Items[i].nApol);
      qrmNroAverbacao.Lines.Add(FCTe.infCTeNorm.seg.Items[i].nAver);
     end;
  end;

  for i := 0 to (FCTe.vPrest.comp.Count - 1) do
  begin
    case i of
      0,3,6,9:
        begin
          qrmCompNome1.Lines.Add(FCTe.vPrest.comp[i].xNome);
          qrmCompValor1.Lines.Add(FormatFloat('###,###,###,##0.00', FCTe.vPrest.comp[i].vComp));
        end;
      1,4,7,10:
        begin
          qrmCompNome2.Lines.Add(FCTe.vPrest.comp[i].xNome);
          qrmCompValor2.Lines.Add(FormatFloat('###,###,###,##0.00', FCTe.vPrest.comp[i].vComp));
        end;
      2,5,8,11:
        begin
          qrmCompNome3.Lines.Add(FCTe.vPrest.comp[i].xNome);
          qrmCompValor3.Lines.Add(FormatFloat('###,###,###,##0.00', FCTe.vPrest.comp[i].vComp));
        end;
    end;
  end;

  qrlVlrTotServico.Caption := FormatFloat('###,###,###,##0.00', FCTe.vPrest.vTPrest);
  qrlVlrTotReceber.Caption := FormatFloat('###,###,###,##0.00', FCTe.vPrest.vRec);

  qrlSitTrib.Caption := CSTICMSToStr(FCTe.Imp.ICMS.SituTrib) + '-' +
                        CSTICMSToStrTagPosText(FCTe.Imp.ICMS.SituTrib);
{$ELSE}
  for i := 0 to (FCTe.InfCarga.InfQ.Count - 1) do
   begin
    case FCTe.InfCarga.InfQ.Items[i].cUnid of
          uM3: qrmQtdUnidMedida4.Lines.Add(CTeUtil.FormatarValor(msk6x3,
                 FCTe.InfCarga.InfQ.Items[i].qCarga));
          uKg: begin
                if uppercase(trim(FCTe.InfCarga.InfQ.Items[i].tpMed)) = 'PESO BRUTO'
                then qrmQtdUnidMedida1.Lines.Add(CTeUtil.FormatarValor(msk6x3,
                        FCTe.InfCarga.InfQ.Items[i].qCarga))
                else
                if uppercase(trim(FCTe.InfCarga.InfQ.Items[i].tpMed)) = 'PESO BASE DE CALCULO'
                then qrmQtdUnidMedida2.Lines.Add(CTeUtil.FormatarValor(msk6x3,
                        FCTe.InfCarga.InfQ.Items[i].qCarga))
                else
                if uppercase(trim(FCTe.InfCarga.InfQ.Items[i].tpMed)) = 'PESO BC'
                then qrmQtdUnidMedida2.Lines.Add(CTeUtil.FormatarValor(msk6x3,
                        FCTe.InfCarga.InfQ.Items[i].qCarga))
                else qrmQtdUnidMedida3.Lines.Add(CTeUtil.FormatarValor(msk6x3,
                        FCTe.InfCarga.InfQ.Items[i].qCarga));
               end;
         uTON: begin
                if uppercase(trim(FCTe.InfCarga.InfQ.Items[i].tpMed)) = 'PESO BRUTO'
                then qrmQtdUnidMedida1.Lines.Add(CTeUtil.FormatarValor(msk6x3,
                        FCTe.InfCarga.InfQ.Items[i].qCarga * 1000))
                else
                if uppercase(trim(FCTe.InfCarga.InfQ.Items[i].tpMed)) = 'PESO BASE DE CALCULO'
                then qrmQtdUnidMedida2.Lines.Add(CTeUtil.FormatarValor(msk6x3,
                        FCTe.InfCarga.InfQ.Items[i].qCarga * 1000))
                else
                if uppercase(trim(FCTe.InfCarga.InfQ.Items[i].tpMed)) = 'PESO BC'
                then qrmQtdUnidMedida2.Lines.Add(CTeUtil.FormatarValor(msk6x3,
                        FCTe.InfCarga.InfQ.Items[i].qCarga * 1000))
                else qrmQtdUnidMedida3.Lines.Add(CTeUtil.FormatarValor(msk6x3,
                        FCTe.InfCarga.InfQ.Items[i].qCarga * 1000));
               end;
     uUNIDADE: qrmQtdUnidMedida5.Lines.Add(CTeUtil.FormatarValor(msk6x3,
                 FCTe.InfCarga.InfQ.Items[i].qCarga) + ' ' + FCTe.InfCarga.InfQ.Items[i].tpMed);
     uLITROS:  qrmQtdUnidMedida5.Lines.Add(CTeUtil.FormatarValor(msk6x3,
                 FCTe.InfCarga.InfQ.Items[i].qCarga) + ' ' + FCTe.InfCarga.InfQ.Items[i].tpMed);
     uMMBTU:   qrmQtdUnidMedida5.Lines.Add(CTeUtil.FormatarValor(msk6x3,
                 FCTe.InfCarga.InfQ.Items[i].qCarga) + ' ' + FCTe.InfCarga.InfQ.Items[i].tpMed);
    end;
   end;

  if FCTe.InfSeg.Count > 0 then
  begin
    for i := 0 to FCTe.infSeg.Count-1 do
     begin
      qrmNomeSeguradora.Lines.Add(FCTe.infSeg.Items[i].xSeg);
      qrmRespSeguroMerc.Lines.Add(TpRspSeguroToStrText(FCTe.infSeg.Items[i].respSeg));
      qrmNroApolice.Lines.Add(FCTe.infSeg.Items[i].nApol);
      qrmNroAverbacao.Lines.Add(FCTe.infSeg.Items[i].nAver);
     end;
  end;

  for i := 0 to (FCTe.vPrest.comp.Count - 1) do
  begin
    case i of
      0,3,6,9:
        begin
          qrmCompNome1.Lines.Add(FCTe.vPrest.comp[i].xNome);
          qrmCompValor1.Lines.Add(CTeUtil.FormatarValor(msk10x2, FCTe.vPrest.comp[i].vComp));
        end;
      1,4,7,10:
        begin
          qrmCompNome2.Lines.Add(FCTe.vPrest.comp[i].xNome);
          qrmCompValor2.Lines.Add(CTeUtil.FormatarValor(msk10x2, FCTe.vPrest.comp[i].vComp));
        end;
      2,5,8,11:
        begin
          qrmCompNome3.Lines.Add(FCTe.vPrest.comp[i].xNome);
          qrmCompValor3.Lines.Add(CTeUtil.FormatarValor(msk10x2, FCTe.vPrest.comp[i].vComp));
        end;
    end;
  end;

  qrlVlrTotServico.Caption := CTeUtil.FormatarValor(msk13x2, FCTe.vPrest.vTPrest);
  qrlVlrTotReceber.Caption := CTeUtil.FormatarValor(msk13x2, FCTe.vPrest.vRec);

  qrlSitTrib.Caption := CSTICMSToStr(FCTe.Imp.ICMS.SituTrib) + '-' +
                        CSTICMSToStrTagPosText(FCTe.Imp.ICMS.SituTrib);
{$ENDIF}

{$IFDEF PL_103}
  case FCTe.Imp.ICMS.SituTrib of
    cst00:
      begin
        qrlRedBaseCalc.Caption := '';
        qrlBaseCalc.Caption    := CTeUtil.FormatarValor(msk9x2, FCTe.Imp.ICMS.CST00.vBC);
        qrlAliqICMS.Caption    := CTeUtil.FormatarValor(msk4x2, FCTe.Imp.ICMS.CST00.pICMS);
        qrlVlrICMS.Caption     := CTeUtil.FormatarValor(msk4x2, FCTe.Imp.ICMS.CST00.vICMS);
        qrlICMS_ST.Caption     := '';
      end;
    cst20:
      begin
        qrlRedBaseCalc.Caption := CTeUtil.FormatarValor(msk4x2, FCTe.Imp.ICMS.CST20.pRedBC);
        qrlBaseCalc.Caption    := CTeUtil.FormatarValor(msk9x2, FCTe.Imp.ICMS.CST20.vBC);
        qrlAliqICMS.Caption    := CTeUtil.FormatarValor(msk4x2, FCTe.Imp.ICMS.CST20.pICMS);
        qrlVlrICMS.Caption     := CTeUtil.FormatarValor(msk4x2, FCTe.Imp.ICMS.CST20.vICMS);
        qrlICMS_ST.Caption     := '';
      end;
    cst40:
      begin
        qrlRedBaseCalc.Caption := '';
        qrlBaseCalc.Caption    := '';
        qrlAliqICMS.Caption    := '';
        qrlVlrICMS.Caption     := '';
        qrlICMS_ST.Caption     := '';
      end;
    cst41:
      begin
        qrlRedBaseCalc.Caption := '';
        qrlBaseCalc.Caption    := '';
        qrlAliqICMS.Caption    := '';
        qrlVlrICMS.Caption     := '';
        qrlICMS_ST.Caption     := '';
      end;
    cst45:
      begin
        qrlRedBaseCalc.Caption := '';
        qrlBaseCalc.Caption    := '';
        qrlAliqICMS.Caption    := '';
        qrlVlrICMS.Caption     := '';
        qrlICMS_ST.Caption     := '';
      end;
    cst51:
      begin
        qrlRedBaseCalc.Caption := '';
        qrlBaseCalc.Caption    := '';
        qrlAliqICMS.Caption    := '';
        qrlVlrICMS.Caption     := '';
        qrlICMS_ST.Caption     := '';
      end;
    cst80:
      begin
        qrlRedBaseCalc.Caption := '';
        qrlBaseCalc.Caption    := CTeUtil.FormatarValor(msk9x2, FCTe.Imp.ICMS.CST80.vBC);
        qrlAliqICMS.Caption    := CTeUtil.FormatarValor(msk4x2, FCTe.Imp.ICMS.CST80.pICMS);
        qrlVlrICMS.Caption     := CTeUtil.FormatarValor(msk9x2, FCTe.Imp.ICMS.CST80.vICMS);
        qrlICMS_ST.Caption     := CTeUtil.FormatarValor(msk9x2, FCTe.Imp.ICMS.CST80.vCred);
      end;
    cst81:
      begin
        qrlRedBaseCalc.Caption := CTeUtil.FormatarValor(msk4x2, FCTe.Imp.ICMS.CST81.pRedBC);
        qrlBaseCalc.Caption    := CTeUtil.FormatarValor(msk9x2, FCTe.Imp.ICMS.CST81.vBC);
        qrlAliqICMS.Caption    := CTeUtil.FormatarValor(msk4x2, FCTe.Imp.ICMS.CST81.pICMS);
        qrlVlrICMS.Caption     := CTeUtil.FormatarValor(msk9x2, FCTe.Imp.ICMS.CST81.vICMS);
        qrlICMS_ST.Caption     := '';
      end;
    cst90:
      begin
        qrlRedBaseCalc.Caption := CTeUtil.FormatarValor(msk4x2, FCTe.Imp.ICMS.CST90.pRedBC);
        qrlBaseCalc.Caption    := CTeUtil.FormatarValor(msk9x2, FCTe.Imp.ICMS.CST90.vBC);
        qrlAliqICMS.Caption    := CTeUtil.FormatarValor(msk4x2, FCTe.Imp.ICMS.CST90.pICMS);
        qrlVlrICMS.Caption     := CTeUtil.FormatarValor(msk9x2, FCTe.Imp.ICMS.CST90.vICMS);
        qrlICMS_ST.Caption     := CTeUtil.FormatarValor(msk4x2, FCTe.Imp.ICMS.CST90.vCred);
      end;
  end;
{$ELSE}
  case FCTe.Imp.ICMS.SituTrib of
    cst00:
      begin
        qrlRedBaseCalc.Caption := '';
        qrlBaseCalc.Caption    := FormatFloat('###,###,###,##0.00', FCTe.Imp.ICMS.ICMS00.vBC);
        qrlAliqICMS.Caption    := FormatFloat('###,###,###,##0.00', FCTe.Imp.ICMS.ICMS00.pICMS);
        qrlVlrICMS.Caption     := FormatFloat('###,###,###,##0.00', FCTe.Imp.ICMS.ICMS00.vICMS);
        qrlICMS_ST.Caption     := '';
      end;
    cst20:
      begin
        qrlRedBaseCalc.Caption := FormatFloat('###,###,###,##0.00', FCTe.Imp.ICMS.ICMS20.pRedBC);
        qrlBaseCalc.Caption    := FormatFloat('###,###,###,##0.00', FCTe.Imp.ICMS.ICMS20.vBC);
        qrlAliqICMS.Caption    := FormatFloat('###,###,###,##0.00', FCTe.Imp.ICMS.ICMS20.pICMS);
        qrlVlrICMS.Caption     := FormatFloat('###,###,###,##0.00', FCTe.Imp.ICMS.ICMS20.vICMS);
        qrlICMS_ST.Caption     := '';
      end;
    cst40:
      begin
        qrlRedBaseCalc.Caption := '';
        qrlBaseCalc.Caption    := '';
        qrlAliqICMS.Caption    := '';
        qrlVlrICMS.Caption     := '';
        qrlICMS_ST.Caption     := '';
      end;
    cst41:
      begin
        qrlRedBaseCalc.Caption := '';
        qrlBaseCalc.Caption    := '';
        qrlAliqICMS.Caption    := '';
        qrlVlrICMS.Caption     := '';
        qrlICMS_ST.Caption     := '';
      end;
    cst45:
      begin
        qrlRedBaseCalc.Caption := '';
        qrlBaseCalc.Caption    := '';
        qrlAliqICMS.Caption    := '';
        qrlVlrICMS.Caption     := '';
        qrlICMS_ST.Caption     := '';
      end;
    cst51:
      begin
        qrlRedBaseCalc.Caption := '';
        qrlBaseCalc.Caption    := '';
        qrlAliqICMS.Caption    := '';
        qrlVlrICMS.Caption     := '';
        qrlICMS_ST.Caption     := '';
      end;
    cst60:
      begin
        qrlRedBaseCalc.Caption := '';
        qrlBaseCalc.Caption    := FormatFloat('###,###,###,##0.00', FCTe.Imp.ICMS.ICMS60.vBCSTRet);
        qrlAliqICMS.Caption    := FormatFloat('###,###,###,##0.00', FCTe.Imp.ICMS.ICMS60.pICMSSTRet);
        qrlVlrICMS.Caption     := FormatFloat('###,###,###,##0.00', FCTe.Imp.ICMS.ICMS60.vICMSSTRet);
        qrlICMS_ST.Caption     := '';
      end;
    cst90:
      begin
        qrlRedBaseCalc.Caption := FormatFloat('###,###,###,##0.00', FCTe.Imp.ICMS.ICMS90.pRedBC);
        qrlBaseCalc.Caption    := FormatFloat('###,###,###,##0.00', FCTe.Imp.ICMS.ICMS90.vBC);
        qrlAliqICMS.Caption    := FormatFloat('###,###,###,##0.00', FCTe.Imp.ICMS.ICMS90.pICMS);
        qrlVlrICMS.Caption     := FormatFloat('###,###,###,##0.00', FCTe.Imp.ICMS.ICMS90.vICMS);
        qrlICMS_ST.Caption     := '';
      end;
    cstICMSOutraUF:
      begin
        qrlRedBaseCalc.Caption := FormatFloat('###,###,###,##0.00', FCTe.Imp.ICMS.ICMSOutraUF.pRedBCOutraUF);
        qrlBaseCalc.Caption    := FormatFloat('###,###,###,##0.00', FCTe.Imp.ICMS.ICMSOutraUF.vBCOutraUF);
        qrlAliqICMS.Caption    := FormatFloat('###,###,###,##0.00', FCTe.Imp.ICMS.ICMSOutraUF.pICMSOutraUF);
        qrlVlrICMS.Caption     := FormatFloat('###,###,###,##0.00', FCTe.Imp.ICMS.ICMSOutraUF.vICMSOutraUF);
        qrlICMS_ST.Caption     := '';
      end;
    cstICMSSN:
      begin
        qrlRedBaseCalc.Caption := '';
        qrlBaseCalc.Caption    := '';
        qrlAliqICMS.Caption    := '';
        qrlVlrICMS.Caption     := '';
        qrlICMS_ST.Caption     := '';
      end;
  end;
{$ENDIF}
end;

procedure TfrmDACTeQRRetrato.qrb_04_DadosNotaFiscalBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;
  PrintBand := QRCTe.PageNumber = 1;

  // Imprime os dados das Notas Fiscais se o Tipo de CTe for Normal ou Substituto
  qrb_04_DadosNotaFiscal.Enabled:=(FCTe.Ide.tpCTe = tcNormal) or (FCTe.Ide.tpCTe = tcSubstituto);
end;

procedure TfrmDACTeQRRetrato.qrb_05_ComplementoBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
{$IFNDEF PL_200}
var
 i: Integer;
{$ENDIF}
begin
  inherited;
  PrintBand := QRCTe.PageNumber = 1;

  // Imprime a lista dos CT-e Complementados se o Tipo de CTe for Complemento
  qrmComplChave1.Lines.Clear;
  qrmComplValor1.Lines.Clear;
  qrmComplChave2.Lines.Clear;
  qrmComplValor2.Lines.Clear;
  qrb_05_Complemento.Enabled:=(FCTe.Ide.tpCTe = tcComplemento);

{$IFDEF PL_200}
  qrmComplChave1.Lines.Add(FCTe.InfCTeComp.Chave);
  qrmComplValor1.Lines.Add(FormatFloat('###,###,###,##0.00', FCTe.vPrest.vTPrest));
{$ELSE}
  for i := 0 to (FCTe.InfCTeComp.Count - 1) do
  begin
    case i of
      0..4:
        begin
          qrmComplChave1.Lines.Add(FCTe.InfCTeComp[i].Chave);
          qrmComplValor1.Lines.Add(CTeUtil.FormatarValor(msk10x2, FCTe.InfCTeComp[i].vPresComp.vTPrest));
        end;
      5..9:
        begin
          qrmComplChave2.Lines.Add(FCTe.InfCTeComp[i].Chave);
          qrmComplValor2.Lines.Add(CTeUtil.FormatarValor(msk10x2, FCTe.InfCTeComp[i].vPresComp.vTPrest));
        end;
    end;
  end;
{$ENDIF}
end;

procedure TfrmDACTeQRRetrato.qrb_06_ValorPrestacaoBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;
  PrintBand := QRCTe.PageNumber = 1;

  // ICMS ST
  if FCTe.infCTe.versao < 2
   then begin
    qrsBC.Left := 346;
    lblBC.Left := 350;
    qrlBaseCalc.Left := 350;

    qrsAliq.Left := 448;
    lblAliq.Left := 454;
    qrlAliqICMS.Left := 454;

    qrsValorICMS.Left := 500;
    lblValorICMS.Left := 504;
    qrlVlrICMS.Left := 504;

    qrsRedBC.Left := 586;
    lblRedBC.Left := 590;
    qrlRedBaseCalc.Left := 590;

    qrsICMSST.Enabled := True;
    qrlICMSST.Enabled := True;
    qrlICMS_ST.Enabled := True;
   end
   else begin
    qrsBC.Left := 436;
    lblBC.Left := 440;
    qrlBaseCalc.Left := 440;

    qrsAliq.Left := 538;
    lblAliq.Left := 544;
    qrlAliqICMS.Left := 544;

    qrsValorICMS.Left := 590;
    lblValorICMS.Left := 594;
    qrlVlrICMS.Left := 594;

    qrsRedBC.Left := 676;
    lblRedBC.Left := 680;
    qrlRedBaseCalc.Left := 680;

    qrsICMSST.Enabled := False;
    qrlICMSST.Enabled := False;
    qrlICMS_ST.Enabled := False;
   end;
end;

procedure TfrmDACTeQRRetrato.qrb_07_HeaderItensBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;

  if (FCTe.Ide.tpCTe = tcAnulacao) then
    qrlDocOrig.Caption := 'DOCUMENTO ANULADO'
  else
    qrlDocOrig.Caption := 'DOCUMENTOS ORIGINÁRIOS';

  // Imprime os Documentos Originários se o Tipo de CTe for Normal
end;

procedure TfrmDACTeQRRetrato.qrb_08_ItensBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
var
  i: Integer;
begin
  inherited;

  qrb_08_Itens.Enabled := (FCTe.Ide.tpCTe = tcNormal) or
                          (FCTe.Ide.tpCTe = tcComplemento) or
                          (FCTe.Ide.tpCTe = tcSubstituto) or
                          (FCTe.Ide.tpCTe = tcAnulacao);

  for i := 1 to 2 do
    if Trim(cdsDocumentos.FieldByName('DOCUMENTO_' + IntToStr(i)).AsString) = '' then
      TQRDBText(FindComponent('qrdbtCnpjEmitente' + intToStr(i))).Width := 325
    else
      TQRDBText(FindComponent('qrdbtCnpjEmitente' + intToStr(i))).Width := 128;

end;

procedure TfrmDACTeQRRetrato.qrb_09_ObsBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;
  PrintBand := QRCTe.PageNumber = 1;

  qrmObs.Lines.BeginUpdate;
  qrmObs.Lines.Clear;

  qrmObs.Lines.Add(StringReplace(FCTe.Compl.xObs, '&lt;BR&gt;', #13#10, [rfReplaceAll,rfIgnoreCase]));

  if FCTe.Ide.tpEmis in [teContingencia, teFSDA, teDPEC]
   then begin
    if not (FCTe.procCTe.cStat in [100, 101, 110])
     then qrmObs.Lines.Add('DACTE em Contingência - Impresso em decorrência de problemas técnicos.');
   end;

  if (FCTe.Ide.tpEmis = teDPEC) and (FEPECEnviado)
   then qrmObs.Lines.Add('EPEC regularmente recebida pela Receita Federal do Brasil');

  qrmObs.Lines.Text:=StringReplace(qrmObs.Lines.Text, ';', #13, [rfReplaceAll]);
  qrmObs.Lines.EndUpdate;

  // Mensagem para modo Homologacao.
  qrlMsgTeste.Visible := False;
  qrlMsgTeste.Enabled := False;

  if FCTe.Ide.tpAmb = taHomologacao then
   begin
    qrlMsgTeste.Caption := 'AMBIENTE DE HOMOLOGAÇÃO - SEM VALOR FISCAL';
    qrlMsgTeste.Visible := True;
    qrlMsgTeste.Enabled := True;
   end else
   begin
    if FCTe.procCTe.cStat > 0 then
     begin
      if (FCTe.procCTe.cStat = 101)  or (FCTeCancelada) then
       begin
        qrlMsgTeste.Caption := 'CT-e CANCELADO';
        qrlMsgTeste.Visible := True;
        qrlMsgTeste.Enabled := True;
       end;

      if FCTe.procCTe.cStat = 110 then
       begin
        qrlMsgTeste.Caption := 'CT-e DENEGADO';
        qrlMsgTeste.Visible := True;
        qrlMsgTeste.Enabled := True;
       end;

      if not FCTe.procCTe.cStat in [101, 110, 100] then
       begin
        qrlMsgTeste.Caption := FCTe.procCTe.xMotivo;
        qrlMsgTeste.Visible := True;
        qrlMsgTeste.Enabled := True;
       end;
     end else
     begin
      qrlMsgTeste.Caption := 'CT-E NÃO ENVIADO PARA SEFAZ';
      qrlMsgTeste.Visible := True;
      qrlMsgTeste.Enabled := True;
     end;
   end;

  qrlMsgTeste.Repaint;
end;

procedure TfrmDACTeQRRetrato.qrb_10_ModRodFracionadoBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
begin
  inherited;
  PrintBand := QRCTe.PageNumber = 1;

  // Imprime as Informações Especificas do Modal se o Tipo de CTe for Normal
  qrb_10_ModRodFracionado.Enabled := (FCTe.Ide.tpCTe = tcNormal) and (FCTe.Ide.modal = mdRodoviario);
  qrb_11_ModRodLot103.Enabled := False;
  qrb_11_ModRodLot104.Enabled := False;

{$IFDEF PL_200}
  with FCTe.infCTeNorm.rodo do
{$ELSE}
  with FCTe.Rodo do
{$ENDIF}
  begin
    qrlRntrcEmpresa.Caption := RNTRC;

{$IFDEF PL_103}
    qrsCIOT.Enabled := False;
    lblCIOT.Enabled := False;
    qrlCIOT.Enabled := False;
{$ELSE}
    qrsCIOT.Enabled := True;
    lblCIOT.Enabled := True;
    qrlCIOT.Enabled := True;
    qrlCIOT.Caption := CIOT;
{$ENDIF}

    case Lota of
      ltNao: begin
              qrlTituloLotacao.Caption := 'DADOS ESPECÍFICOS DO MODAL RODOVIÁRIO - CARGA FRACIONADA';
              qrlLotacao.Caption       := 'NÃO';
             end;
      ltsim: begin
              qrlTituloLotacao.Caption := 'DADOS ESPECÍFICOS DO MODAL RODOVIÁRIO - LOTAÇÃO';
              qrlLotacao.Caption       := 'SIM';
              if Versao = 103
               then qrb_11_ModRodLot103.Enabled := True
               else qrb_11_ModRodLot104.Enabled := True;
             end;
    end;

    qrlDtPrevEntrega.Caption := FormatDateTime('DD/MM/YYYY', dPrev);
  end;
end;

procedure TfrmDACTeQRRetrato.qrb_11_ModRodLot103BeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
var
  i: Integer;
begin
  inherited;
  PrintBand := QRCTe.PageNumber = 1;

{$IFDEF PL_103}
  qrlNumRegEsp.Caption := FCTe.Rodo.valePed.nroRE;
  case FCTe.Rodo.valePed.respPg of
   rpEmitente:       qrlResponsavel.Caption := 'EMITENTE';
   rpRemetente:      qrlResponsavel.Caption := 'REMETENTE';
   rpExpedidor:      qrlResponsavel.Caption := 'EXPEDIDOR';
   rpRecebedor:      qrlResponsavel.Caption := 'RECEBEDOR';
   rpDestinatario:   qrlResponsavel.Caption := 'DESTINATÁRIO';
   rpTomadorServico: qrlResponsavel.Caption := 'TOMADOR DO SERVIÇO';
  end;
  qrlValorTotal.Caption := CTeUtil.FormatarValor(msk13x2, FCTe.Rodo.valePed.vTValePed);
{$ENDIF}

  qrmTipo.Lines.Clear;
  qrmPlaca.Lines.Clear;
  qrmUF.Lines.Clear;
  qrmRNTRC.Lines.Clear;

  qrmEmpresas.Lines.Clear;
  qrmVigencias.Lines.Clear;
  qrmNumDispositivo.Lines.Clear;
  qrmCodTransacao.Lines.Clear;

  qrlNomeMotorista.Caption := '';
  qrlCPFMotorista.Caption  := '';
  qrlLacres.Caption        := '';

{$IFDEF PL_200}
  for i := 0 to (FCTe.infCTeNorm.Rodo.veic.Count - 1) do
  begin
   if FCTe.infCTeNorm.Rodo.veic.Items[i].tpVeic = tvTracao
    then qrmTipo.Lines.Add('Tração')
    else qrmTipo.Lines.Add('Reboque');
   qrmPlaca.Lines.Add(FCTe.infCTeNorm.Rodo.veic.Items[i].placa);
   qrmUF.Lines.Add(FCTe.infCTeNorm.Rodo.veic.Items[i].UF);
   if FCTe.infCTeNorm.Rodo.veic.Items[i].Prop.RNTRC <> '' then
     qrmRNTRC.Lines.Add(FCTe.infCTeNorm.Rodo.veic.Items[i].Prop.RNTRC)
   else
     qrmRNTRC.Lines.Add(FCTe.infCTeNorm.Rodo.RNTRC);
  end;

  if FCTe.infCTeNorm.Rodo.moto.Count > 0
   then begin
    qrlNomeMotorista.Caption := FCTe.infCTeNorm.Rodo.moto.Items[0].xNome;
    qrlCPFMotorista.Caption  := FormatarCPF(FCTe.infCTeNorm.Rodo.moto.Items[0].CPF);
   end;

  for i := 0 to (FCTe.infCTeNorm.Rodo.lacRodo.Count - 1) do
  begin
   qrlLacres.Caption := qrlLacres.Caption + FCTe.infCTeNorm.Rodo.lacRodo.Items[i].nLacre + '/';
  end;
{$ELSE}
  for i := 0 to (FCTe.Rodo.veic.Count - 1) do
  begin
   if FCTe.Rodo.veic.Items[i].tpVeic = tvTracao
    then qrmTipo.Lines.Add('Tração')
    else qrmTipo.Lines.Add('Reboque');
   qrmPlaca.Lines.Add(FCTe.Rodo.veic.Items[i].placa);
   qrmUF.Lines.Add(FCTe.Rodo.veic.Items[i].UF);
   if FCTe.Rodo.veic.Items[i].Prop.RNTRC <> '' then
     qrmRNTRC.Lines.Add(FCTe.Rodo.veic.Items[i].Prop.RNTRC)
   else
     qrmRNTRC.Lines.Add(FCTe.Rodo.RNTRC);
  end;

{$IFDEF PL_103}
  for i := 0 to (FCTe.Rodo.valePed.disp.Count - 1) do
  begin
   qrmEmpresas.Lines.Add(FCTe.Rodo.valePed.disp.Items[i].xEmp);
   qrmVigencias.Lines.Add(FormatDateTime('DD/MM/YYYY', FCTe.Rodo.valePed.disp.Items[i].dVig));
   qrmNumDispositivo.Lines.Add(FCTe.Rodo.valePed.disp.Items[i].nDisp);
   qrmCodTransacao.Lines.Add(FCTe.Rodo.valePed.disp.Items[i].nCompC);
  end;
{$ENDIF}

  if FCTe.Rodo.moto.Count > 0
   then begin
    qrlNomeMotorista.Caption := FCTe.Rodo.moto.Items[0].xNome;
    qrlCPFMotorista.Caption  := FormatarCPF(FCTe.Rodo.moto.Items[0].CPF);
   end;

  for i := 0 to (FCTe.Rodo.Lacres.Count - 1) do
  begin
   qrlLacres.Caption := qrlLacres.Caption + FCTe.Rodo.Lacres.Items[i].nLacre + '/';
  end;
{$ENDIF}
end;

procedure TfrmDACTeQRRetrato.qrb_11_ModRodLot104BeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
var
  i: Integer;
begin
  inherited;

  PrintBand := QRCTe.PageNumber = 1;

  qrmTipo2.Lines.Clear;
  qrmPlaca2.Lines.Clear;
  qrmUF2.Lines.Clear;
  qrmRNTRC2.Lines.Clear;

  qrmCNPJForn.Lines.Clear;
  qrmNumCompra.Lines.Clear;
  qrmCNPJPg.Lines.Clear;

  qrlNomeMotorista2.Caption := '';
  qrlCPFMotorista2.Caption  := '';
  qrlLacres2.Caption        := '';

{$IFDEF PL_200}
  for i := 0 to (FCTe.infCTeNorm.Rodo.veic.Count - 1) do
  begin
   if FCTe.infCTeNorm.Rodo.veic.Items[i].tpVeic = tvTracao
    then qrmTipo2.Lines.Add('Tração')
    else qrmTipo2.Lines.Add('Reboque');
   qrmPlaca2.Lines.Add(FCTe.infCTeNorm.Rodo.veic.Items[i].placa);
   qrmUF2.Lines.Add(FCTe.infCTeNorm.Rodo.veic.Items[i].UF);
   if FCTe.infCTeNorm.Rodo.veic.Items[i].Prop.RNTRC <> '' then
     qrmRNTRC2.Lines.Add(FCTe.infCTeNorm.Rodo.veic.Items[i].Prop.RNTRC)
   else
     qrmRNTRC2.Lines.Add(FCTe.infCTeNorm.Rodo.RNTRC);
  end;

  for i := 0 to (FCTe.infCTeNorm.Rodo.valePed.Count -1) do
  begin
   qrmCNPJForn.Lines.Add(FormatarCNPJ(FCTe.infCTeNorm.Rodo.valePed.Items[i].CNPJForn));
   qrmNumCompra.Lines.Add(FCTe.infCTeNorm.Rodo.valePed.Items[i].nCompra);
   qrmCNPJPg.Lines.Add(FormatarCNPJ(FCTe.infCTeNorm.Rodo.valePed.Items[i].CNPJPg));
  end;

  if FCTe.infCTeNorm.Rodo.moto.Count > 0
   then begin
    qrlNomeMotorista2.Caption := FCTe.infCTeNorm.Rodo.moto.Items[0].xNome;
    qrlCPFMotorista2.Caption  := FormatarCPF(FCTe.infCTeNorm.Rodo.moto.Items[0].CPF);
   end;

  for i := 0 to (FCTe.infCTeNorm.Rodo.lacRodo.Count - 1) do
  begin
   qrlLacres2.Caption := qrlLacres2.Caption + FCTe.infCTeNorm.Rodo.lacRodo.Items[i].nLacre + '/';
  end;
{$ELSE}
  for i := 0 to (FCTe.Rodo.veic.Count - 1) do
  begin
   if FCTe.Rodo.veic.Items[i].tpVeic = tvTracao
    then qrmTipo2.Lines.Add('Tração')
    else qrmTipo2.Lines.Add('Reboque');
   qrmPlaca2.Lines.Add(FCTe.Rodo.veic.Items[i].placa);
   qrmUF2.Lines.Add(FCTe.Rodo.veic.Items[i].UF);
   if FCTe.Rodo.veic.Items[i].Prop.RNTRC <> '' then
     qrmRNTRC2.Lines.Add(FCTe.Rodo.veic.Items[i].Prop.RNTRC)
   else
     qrmRNTRC2.Lines.Add(FCTe.Rodo.RNTRC);
  end;

{$IFDEF PL_104}
  for i := 0 to (FCTe.Rodo.valePed.Count -1) do
  begin
   qrmCNPJForn.Lines.Add(FormatarCNPJ(FCTe.Rodo.valePed.Items[i].CNPJForn));
   qrmNumCompra.Lines.Add(FCTe.Rodo.valePed.Items[i].nCompra);
   qrmCNPJPg.Lines.Add(FormatarCNPJ(FCTe.Rodo.valePed.Items[i].CNPJPg));
  end;
{$ENDIF}

  if FCTe.Rodo.moto.Count > 0
   then begin
    qrlNomeMotorista2.Caption := FCTe.Rodo.moto.Items[0].xNome;
    qrlCPFMotorista2.Caption  := FormatarCPF(FCTe.Rodo.moto.Items[0].CPF);
   end;

  for i := 0 to (FCTe.Rodo.Lacres.Count - 1) do
  begin
   qrlLacres2.Caption := qrlLacres2.Caption + FCTe.Rodo.Lacres.Items[i].nLacre + '/';
  end;
{$ENDIF}
end;

procedure TfrmDACTeQRRetrato.qrb_12_ModAereoBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;
  PrintBand := QRCTe.PageNumber = 1;
  qrb_12_ModAereo.Enabled := (FCTe.Ide.tpCTe = tcNormal) and (FCTe.Ide.modal = mdAereo);

  qrlCaracAdServico.Caption    := FCTe.Compl.xCaracSer;
  qrlCaracAdTransporte.Caption := FCTe.Compl.xCaracAd;

{$IFDEF PL_200}
  with FCTe.infCTeNorm.aereo do
{$ELSE}
  with FCTe.Aereo do
{$ENDIF}
  begin
    qrlAWB.Caption           := nOCA;
  {$IFDEF PL_103}
    qrlTrecho.Caption        := tarifa.trecho;
    qrlContaCorrente.Caption := cIATA; // ??? Conta Corrente ???
  {$ENDIF}
    qrlTarifaCL.Caption      := tarifa.CL;
    qrlTarifaCodigo.Caption  := tarifa.cTar;
    qrlTarifaValor.Caption   := FormatCurr('###,###,##0.00', tarifa.vTar);
  {$IFNDEF PL_103}
    qrlContaCorrente.Caption := IdT; // ??? Conta Corrente ???
  {$ENDIF}
    qrlMinuta.Caption        := FormatFloat('0000000000', nMinu);

    qrlLojaAgenteEmissor.Caption := xLAgEmi;
  end;

  if FCte.Ide.retira = rtSim
   then qrlRetira.Caption := 'SIM'
   else qrlRetira.Caption := 'NÃO';

  qrlDadosRetira.Caption := FCte.Ide.xdetretira;
end;

procedure TfrmDACTeQRRetrato.qrb_13_ModAquaviarioBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
var
  i, j: Integer;
begin
  inherited;
  PrintBand := QRCTe.PageNumber = 1;
  qrb_13_ModAquaviario.Enabled := (FCTe.Ide.tpCTe = tcNormal) and (FCTe.Ide.modal = mdAquaviario);

{$IFDEF PL_200}
  with FCTe.infCTeNorm.aquav do
{$ELSE}
  with FCTe.aquav do
{$ENDIF}
  begin
    qrlBCAFRMM.Caption    := FormatCurr('###,###,##0.00', vPrest);
    qrlValorAFRMM.Caption := FormatCurr('###,###,##0.00', vAFRMM);

    qrlPortoEmbarque.Caption     := prtEmb;
    qrlPortoDestino.Caption      := prtDest;
    qrlIndNavioRebocador.Caption := xNavio;

    case tpNav of
     tnInterior:  qrlTipoNav.Caption := 'INTERIOR';
     tnCabotagem: qrlTipoNav.Caption := 'CABOTAGEM';
    end;

    case direc of
     drNorte: qrlDirecao.Caption := 'NORTE';
     drLeste: qrlDirecao.Caption := 'LESTE';
     drSul:   qrlDirecao.Caption := 'SUL';
     drOeste: qrlDirecao.Caption := 'OESTE';
    end;

    qrlIndBalsas.Caption := '';
  {$IFNDEF PL_103}
    for i := 0 to (balsa.Count - 1) do
     begin
      if i = 0 then
        qrlIndBalsas.Caption := balsa.Items[i].xBalsa
      else
        qrlIndBalsas.Caption := qrlIndBalsas.Caption + '/' + balsa.Items[i].xBalsa;
     end;
  {$ENDIF}

    qrlIndConteiners.Caption := '';
  {$IFNDEF PL_200}
    for i := 0 to (detCont.Count - 1) do
     begin
       for j := 0 to (detCont.Items[i].Lacre.Count - 1) do
        begin
         if i > 0 then
           qrlIndConteiners.Caption := qrlIndConteiners.Caption + '  '; 
         if j = 0 then
           qrlIndConteiners.Caption := qrlIndConteiners.Caption + detCont.Items[i].nCont + '-' + detCont.Items[i].Lacre.Items[j].nLacre
         else
           qrlIndConteiners.Caption := qrlIndConteiners.Caption + '/' + detCont.Items[i].Lacre.Items[j].nLacre;
        end;
     end;
  {$ENDIF}
  end;

end;

procedure TfrmDACTeQRRetrato.qrb_14_ModFerroviarioBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;
  PrintBand := QRCTe.PageNumber = 1;
  qrb_14_ModFerroviario.Enabled := (FCTe.Ide.tpCTe = tcNormal) and (FCTe.Ide.modal = mdFerroviario);

end;

procedure TfrmDACTeQRRetrato.qrb_15_ModDutoviarioBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;
  PrintBand := QRCTe.PageNumber = 1;
  qrb_15_ModDutoviario.Enabled := (FCTe.Ide.tpCTe = tcNormal) and (FCTe.Ide.modal = mdDutoviario);

end;

procedure TfrmDACTeQRRetrato.qrb_16_DadosExcEmitenteBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
var
 i: Integer;
begin
  inherited;
  PrintBand := QRCTe.PageNumber = 1;

  qrmObsExcEmitente.Lines.BeginUpdate;
  qrmObsExcEmitente.Lines.Clear;
  qrmObsFisco.Lines.Clear;

  if FCTe.Ide.modal <> mdAereo
   then begin
    for i := 0 to (FCTe.Compl.ObsCont.Count-1) do
     with FCTe.Compl.ObsCont.Items[i] do
      begin
       qrmObsExcEmitente.Lines.Add(StringReplace(xCampo, '&lt;BR&gt;', #13#10, [rfReplaceAll,rfIgnoreCase])+': '+
                                   StringReplace(xTexto, '&lt;BR&gt;', #13#10, [rfReplaceAll,rfIgnoreCase]));
      end;
   end;

  qrmObsExcEmitente.Lines.Text := StringReplace(qrmObsExcEmitente.Lines.Text, ';', #13, [rfReplaceAll]);
  qrmObsExcEmitente.Lines.EndUpdate;

  qrmObsFisco.Lines.Add(StringReplace(FCTe.Imp.infAdFisco , '&lt;BR&gt;', #13#10, [rfReplaceAll,rfIgnoreCase]));

  for i := 0 to (FCTe.Compl.ObsFisco.Count-1) do
   with FCTe.Compl.ObsFisco.Items[i] do
    begin
     qrmObsFisco.Lines.Add(StringReplace(xCampo, '&lt;BR&gt;', #13#10, [rfReplaceAll,rfIgnoreCase])+': '+
                           StringReplace(xTexto, '&lt;BR&gt;', #13#10, [rfReplaceAll,rfIgnoreCase]));
    end;

  qrmObsFisco.Lines.Text := StringReplace(qrmObsFisco.Lines.Text, ';', #13, [rfReplaceAll]);
  qrmObsFisco.Lines.EndUpdate;
end;

procedure TfrmDACTeQRRetrato.qrb_17_SistemaBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
begin
  inherited;
  PrintBand := QRCTe.PageNumber = 1;

  qrlblSistema.Caption := FSistema + ' - ' + FUsuario;
end;

procedure TfrmDACTeQRRetrato.qrb_18_ReciboBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;
  PrintBand := (QRCTe.PageNumber = 1);

  //Rodrigo DSP 22/01/2014 11:22:47: impressao do canhoto - Rodape.
  if (FResumoCanhoto) then
    qrlResumoCanhotoCTe2.Caption := getTextoResumoCanhoto
  else
    qrlResumoCanhotoCTe2.Caption := '';
    
  qrlSerie3.Caption  := IntToStr(FCTe.Ide.serie); // FormatFloat( '000', FCTe.Ide.serie);
  qrlNumCte3.Caption := FormatFloat('000,000,000', FCTe.Ide.nCT);

  if PrintBand
   then begin
    qrb_18_Recibo.Enabled := ((FCTe.Ide.tpCTe = tcNormal) or (FCTe.Ide.tpCTe = tcComplemento)) and
                             (FCTe.Ide.modal <> mdAereo) and (FPosRecibo = prRodape);
    if qrb_18_Recibo.Enabled
     then qrb_18_Recibo.Height  := 68
     else qrb_18_Recibo.Height  := 0;
   end;
end;

procedure TfrmDACTeQRRetrato.qrb_06_ProdutosPerigososBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
var
  i: Integer;
begin
  inherited;
  PrintBand := (QRCTe.PageNumber = 1);

  qrmNumONU.Lines.Clear;
  qrmNomeApropriado.Lines.Clear;
  qrmClasse.Lines.Clear;
  qrmGrupoEmbalagem.Lines.Clear;
  qrmQtdeProduto.Lines.Clear;

{$IFDEF PL_200}
  for i := 0 to (FCTe.infCTeNorm.peri.Count-1) do
   begin
     qrmNumONU.Lines.Add(FCTe.infCTeNorm.peri.Items[i].nONU);
     qrmNomeApropriado.Lines.Add(FCTe.infCTeNorm.peri.Items[i].xNomeAE);
     qrmClasse.Lines.Add(FCTe.infCTeNorm.peri.Items[i].xClaRisco);
     qrmGrupoEmbalagem.Lines.Add(FCTe.infCTeNorm.peri.Items[i].grEmb);
     qrmQtdeProduto.Lines.Add(FCTe.infCTeNorm.peri.Items[i].qTotProd);
   end;
{$ELSE}
  for i := 0 to (FCTe.peri.Count-1) do
   begin
     qrmNumONU.Lines.Add(FCTe.peri.Items[i].nONU);
     qrmNomeApropriado.Lines.Add(FCTe.peri.Items[i].xNomeAE);
     qrmClasse.Lines.Add(FCTe.peri.Items[i].xClaRisco);
     qrmGrupoEmbalagem.Lines.Add(FCTe.peri.Items[i].grEmb);
     qrmQtdeProduto.Lines.Add(FCTe.peri.Items[i].qTotProd);
   end;
{$ENDIF}

end;

end.

