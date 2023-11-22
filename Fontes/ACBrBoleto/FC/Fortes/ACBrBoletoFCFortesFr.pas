{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Juliana Tamizou                                 }
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

{******************************************************************************
|* Historico
|*
|* 01/04/2010: Juliana Rodrigues Prado Tamizou
|*  - Adaptação do Boleto do Projeto RLBoleto  ( http://www.produsys.com.br/ )
******************************************************************************}
{$I ACBr.inc}

unit ACBrBoletoFCFortesFr;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, RLReport, RLBarcode,
  RLPDFFilter, RLHTMLFilter, RLFilters, RLPrinters, RLUtils,
  {$IFDEF FPC}
    LResources, StdCtrls,
  {$ENDIF}
  ACBrBoleto, RLRichText , ACBrBase, ACBrBoletoConversao, ACBrValidador;

type

  { TACBrBoletoFCFortesFr }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrBoletoFCFortes = class(TACBrBoletoFCClass)
  private
    { Private declarations }
    procedure ImprimirInternal(AStream: TStream);

  public
    { Public declarations }
    Constructor Create(AOwner: TComponent); override;

    procedure Imprimir; overload; override;
    procedure Imprimir(AStream: TStream); overload; override;

  end;

  TACBrBoletoFCFortesFr = class(TForm)
     BoletoCarne: TRLReport;
     imgBarrasCarne: TRLBarcode;
    ImgLoja: TRLImage;
    lblLocalPagto: TRLMemo;
    txtEnderecoPIX: TRLLabel;
    lblValorPix: TRLLabel;
    txtCedentePix: TRLLabel;
    RLDraw225: TRLDraw;
    RLDraw226: TRLDraw;
    RLDraw227: TRLDraw;
    RLDraw228: TRLDraw;
    RLDraw231: TRLDraw;
    imgQRCodePix: TRLImage;
    txtCNPJCedentePix: TRLLabel;
    txtValorPix: TRLLabel;
    RLBandPix: TRLBand;
    RLDraw10: TRLDraw;
    RLDraw11: TRLDraw;
    RLDraw119: TRLDraw;
    RLDraw12: TRLDraw;
    RLDraw13: TRLDraw;
    RLDraw14: TRLDraw;
    RLDraw15: TRLDraw;
    RLDraw16: TRLDraw;
    RLDraw17: TRLDraw;
    RLDraw18: TRLDraw;
    RLDraw19: TRLDraw;
    RLDraw2: TRLDraw;
    RLDraw20: TRLDraw;
    RLDraw21: TRLDraw;
    RLDraw22: TRLDraw;
    rlCIP1: TRLDraw;
    rlCIP2: TRLDraw;
    rlCIP3: TRLDraw;
    rlCIP4: TRLDraw;
    RLDraw224: TRLDraw;
    RLDraw23: TRLDraw;
    RLDraw24: TRLDraw;
    RLDraw25: TRLDraw;
    RLDraw26: TRLDraw;
    RLDraw27: TRLDraw;
    RLDraw28: TRLDraw;
    RLDraw29: TRLDraw;
    RLDraw3: TRLDraw;
    RLDraw33: TRLDraw;
    RLDraw34: TRLDraw;
    RLDraw35: TRLDraw;
    RLDraw36: TRLDraw;
    RLDraw37: TRLDraw;
    RLDraw38: TRLDraw;
    RLDraw39: TRLDraw;
    RLDraw4: TRLDraw;
    RLDraw40: TRLDraw;
    RLDraw41: TRLDraw;
    RLDraw42: TRLDraw;
    RLDraw43: TRLDraw;
    RLDraw44: TRLDraw;
    RLDraw45: TRLDraw;
    RLDraw46: TRLDraw;
    RLDraw47: TRLDraw;
    RLDraw48: TRLDraw;
    RLDraw49: TRLDraw;
    RLDraw5: TRLDraw;
    RLDraw50: TRLDraw;
    RLDraw6: TRLDraw;
    RLDraw7: TRLDraw;
    RLDraw72: TRLDraw;
    RLDraw73: TRLDraw;
    RLDraw74: TRLDraw;
    RLDraw75: TRLDraw;
    RLDraw76: TRLDraw;
    RLDraw77: TRLDraw;
    RLDraw78: TRLDraw;
    RLDraw8: TRLDraw;
    RLDraw81: TRLDraw;
    RLDraw82: TRLDraw;
    RLDraw83: TRLDraw;
    RLDraw9: TRLDraw;
    lblSacador2a: TRLLabel;
    RLLabel11: TRLLabel;
    RLLabel123: TRLMemo;
    RLLabel161: TRLMemo;
    RLLabel249: TRLLabel;
    lblCIP1: TRLLabel;
    lblCIP2: TRLLabel;
    lblCIP3: TRLLabel;
    lblCIP4: TRLLabel;
    RLLabel80: TRLMemo;
    RLLabel88: TRLLabel;
    txtUsoBanco: TRLLabel;
    txtEndCedente: TRLMemo;
    txtEndCedente1: TRLMemo;
    txtEndSacadorAval3: TRLLabel;
    RLLabel14: TRLLabel;
    RLLabel145: TRLLabel;
    RLLabel146: TRLLabel;
    RLLabel147: TRLLabel;
    RLLabel148: TRLLabel;
    RLLabel149: TRLLabel;
    RLLabel150: TRLLabel;
    RLLabel151: TRLLabel;
    RLLabel152: TRLLabel;
    RLLabel153: TRLLabel;
    RLLabel154: TRLLabel;
    RLLabel155: TRLLabel;
    RLLabel156: TRLLabel;
    RLLabel157: TRLLabel;
    RLLabel158: TRLLabel;
    RLLabel159: TRLLabel;
    RLLabel160: TRLLabel;
    RLLabel162: TRLLabel;
    RLLabel163: TRLLabel;
    RLLabel164: TRLLabel;
    lblPagador3: TRLLabel;
    RLLabel166: TRLLabel;
    RLLabel167: TRLLabel;
    RLLabel168: TRLLabel;
    RLLabel169: TRLLabel;
    RLLabel171: TRLLabel;
    RLLabel18: TRLLabel;
    RLLabel2: TRLLabel;
    RLLabel20: TRLLabel;
    RLLabel21: TRLLabel;
    RLLabel23: TRLLabel;
    RLLabel25: TRLLabel;
    RLLabel27: TRLLabel;
    RLLabel28: TRLLabel;
    RLLabel29: TRLLabel;
    RLLabel3: TRLLabel;
    RLLabel30: TRLLabel;
    RLLabel31: TRLLabel;
    RLLabel32: TRLLabel;
    RLLabel33: TRLLabel;
    RLLabel35: TRLLabel;
    RLLabel37: TRLLabel;
    RLLabel4: TRLLabel;
    RLLabel50: TRLLabel;
    RLLabel51: TRLLabel;
    RLLabel52: TRLLabel;
    RLLabel54: TRLLabel;
    RLLabel55: TRLLabel;
    RLLabel56: TRLLabel;
    RLLabel57: TRLLabel;
    RLLabel6: TRLLabel;
    RLLabel67: TRLLabel;
    RLLabel68: TRLLabel;
    RLLabel69: TRLLabel;
    RLLabel7: TRLLabel;
    RLLabel70: TRLLabel;
    RLLabel71: TRLLabel;
    RLLabel72: TRLLabel;
    RLLabel73: TRLLabel;
    RLLabel74: TRLLabel;
    RLLabel75: TRLLabel;
    RLLabel76: TRLLabel;
    RLLabel77: TRLLabel;
    RLLabel78: TRLLabel;
    RLLabel79: TRLLabel;
    RLLabel8: TRLLabel;
    RLLabel81: TRLLabel;
    RLLabel82: TRLLabel;
    RLLabel83: TRLLabel;
    RLLabel84: TRLLabel;
    RLLabel85: TRLLabel;
    RLLabel86: TRLLabel;
    RLLabel87: TRLLabel;
    lblPagador2: TRLLabel;
    RLLabel89: TRLLabel;
    RLLabel9: TRLLabel;
    RLLabel90: TRLLabel;
    RLLabel91: TRLLabel;
    RLLabel92: TRLLabel;
    RLLabel96: TRLLabel;
    txtLocal: TRLMemo;
    txtLocalPagamento3: TRLMemo;
    txtNomeSacado: TRLLabel;
    txtNomeSacadoCarne: TRLMemo;
    txtNumeroDocumentoCarne: TRLLabel;
    txtOrientacoesBanco: TRLMemo;
    txtDesconto5: TRLLabel;
    txtMoraMulta4: TRLLabel;
    txtNumeroBanco: TRLLabel;
    txtTotPar: TRLLabel;
    mIntrucoes: TRLMemo;
    RLBand3: TRLBand;
    txtParcela: TRLLabel;
    txtCodCedenteCarne2: TRLLabel;
    txtCPFCarne2: TRLLabel;
    txtCIP1: TRLLabel;
    txtCIP2: TRLLabel;
    txtCIP3: TRLLabel;
    txtCIP4: TRLLabel;
    txtValorCar: TRLLabel;
    txtNossoNumCan: TRLLabel;
    txtVencCanhoto: TRLLabel;
    txtAceite: TRLLabel;
    txtCarteira: TRLLabel;
    txtCodCedenteCarne: TRLLabel;
    txtCPF: TRLLabel;
    txtDataDocto: TRLLabel;
    txtDataProces: TRLLabel;
    txtEspecieDoc: TRLLabel;
    txtLinhaDigitavelCarne: TRLLabel;
    RLDBText17: TRLDBText;
    RLDBText18: TRLDBText;
    txtNomeCedente: TRLLabel;
    txtNossoNumeroCarne: TRLLabel;
    txtNumeroDocto: TRLLabel;
    txtValorCarne: TRLLabel;
    RLDraw30: TRLDraw;
    RLDraw32: TRLDraw;
    RLDraw51: TRLDraw;
    RLDraw52: TRLDraw;
    RLDraw53: TRLDraw;
    RLDraw54: TRLDraw;
    RLDraw55: TRLDraw;
    RLDraw56: TRLDraw;
    RLDraw57: TRLDraw;
    RLDraw58: TRLDraw;
    RLDraw59: TRLDraw;
    RLDraw60: TRLDraw;
    RLDraw61: TRLDraw;
    RLDraw62: TRLDraw;
    RLDraw63: TRLDraw;
    RLDraw64: TRLDraw;
    RLDraw65: TRLDraw;
    RLDraw66: TRLDraw;
    RLDraw67: TRLDraw;
    RLDraw68: TRLDraw;
    RLDraw69: TRLDraw;
    RLDraw70: TRLDraw;
    RLDraw71: TRLDraw;
    RLHTMLFilter1: TRLHTMLFilter;
    imgBancoCarne: TRLImage;
    RLLabel1: TRLLabel;
    RLLabel10: TRLLabel;
    RLLabel101: TRLLabel;
    RLLabel103: TRLLabel;
    RLLabel105: TRLLabel;
    RLLabel107: TRLLabel;
    RLLabel109: TRLLabel;
    RLLabel111: TRLLabel;
    RLLabel113: TRLLabel;
    RLLabel115: TRLLabel;
    RLLabel117: TRLLabel;
    RLLabel119: TRLLabel;
    RLLabel121: TRLLabel;
    RLLabel124: TRLLabel;
    RLLabel126: TRLLabel;
    RLLabel128: TRLLabel;
    RLLabel13: TRLLabel;
    RLLabel130: TRLLabel;
    RLLabel132: TRLLabel;
    RLLabel135: TRLLabel;
    RLLabel138: TRLLabel;
    RLLabel15: TRLLabel;
    RLLabel16: TRLLabel;
    RLLabel19: TRLLabel;
    RLLabel36: TRLLabel;
    RLLabel38: TRLLabel;
    RLLabel39: TRLLabel;
    RLLabel40: TRLLabel;
    RLLabel41: TRLLabel;
    RLLabel42: TRLLabel;
    RLLabel43: TRLLabel;
    RLLabel44: TRLLabel;
    RLLabel45: TRLLabel;
    RLLabel46: TRLLabel;
    RLLabel48: TRLLabel;
    RLLabel49: TRLLabel;
    RLLabel5: TRLLabel;
    RLLabel94: TRLLabel;
    RLLabel95: TRLLabel;
    RLLabel97: TRLLabel;
    RLLabel99: TRLLabel;
    RLPDFFilter1: TRLPDFFilter;
    txtVencCarne2: TRLLabel;
    LayoutBoleto: TRLReport;
    RLBand4: TRLBand;
    rlBarraOrientbanco: TRLDraw;
    txtNomeCedente4: TRLLabel;
    txtCodigoCedente4: TRLLabel;
    txtNumeroDocumento4: TRLLabel;
    txtEspecie4: TRLLabel;
    txtNossoNumero4: TRLLabel;
    txtValorDocumento4: TRLLabel;
    imgBanco4: TRLImage;
    txtNumeroBanco4: TRLLabel;
    lblLocalPagto4: TRLLabel;
    txtDataVencimento4: TRLLabel;
    txtNomeSacado4: TRLLabel;
    txtDataProcessamento4: TRLLabel;
    RLBand1: TRLBand;
    imgBanco2: TRLImage;
    txtNumeroBanco2: TRLLabel;
    txtNomeCedente2: TRLLabel;
    txtDataDocumento2: TRLLabel;
    txtNumeroDocumento2: TRLLabel;
    txtEspecieDoc2: TRLLabel;
    txtAceite2: TRLLabel;
    txtDataProcessamento2: TRLLabel;
    txtUsoBanco2: TRLLabel;
    txtCarteira2: TRLLabel;
    txtEspecie2: TRLLabel;
    txtQuantidade2: TRLLabel;
    txtValorMoeda2: TRLLabel;
    txtInstrucoes2: TRLMemo;
    txtDataVencimento2: TRLLabel;
    txtCodigoCedente2: TRLLabel;
    txtNossoNumero2: TRLLabel;
    txtValorDocumento2: TRLLabel;
    txtDesconto2: TRLLabel;
    txtMoraMulta2: TRLLabel;
    txtValorCobrado2: TRLLabel;
    txtNomePagador2: TRLLabel;
    txtCpfCnpjPagador2: TRLLabel;
    txtEndSacadorAval2: TRLLabel;
    txtReferencia2: TRLLabel;
    RLBand2: TRLBand;
    imgBanco3: TRLImage;
    txtNumeroBanco3: TRLLabel;
    txtLinhaDigitavel: TRLLabel;
    txtNomeCedente3: TRLLabel;
    txtDataDocumento3: TRLLabel;
    txtNumeroDocumento3: TRLLabel;
    txtEspecieDoc3: TRLLabel;
    txtAceite3: TRLLabel;
    txtDataProcessamento3: TRLLabel;
    txtUsoBanco3: TRLLabel;
    txtCarteira3: TRLLabel;
    txtEspecie3: TRLLabel;
    txtQuantidade3: TRLLabel;
    txtValorMoeda3: TRLLabel;
    txtInstrucoes3: TRLMemo;
    txtDataVencimento3: TRLLabel;
    txtCodigoCedente3: TRLLabel;
    txtNossoNumero3: TRLLabel;
    txtValorDocumento3: TRLLabel;
    txtDesconto3: TRLLabel;
    txtMoraMulta3: TRLLabel;
    txtValorCobrado3: TRLLabel;
    txtNomePagador3: TRLLabel;
    txtNomeSacadorAval3: TRLLabel;
    txtCpfCnpjPagador3: TRLLabel;
    txtCodigoBaixa3: TRLLabel;
    txtReferencia3: TRLLabel;
    imgCodigoBarra: TRLBarcode;
    lblSacador2b: TRLLabel;
    txtNomeSacadorAval2: TRLLabel;
    lblSacador3a: TRLLabel;
    lblSacador3b: TRLLabel;
    txtEndCedenteCarne: TRLLabel;
    memoEndCedenteCarne: TRLMemo;
    txtOrientacoesBancoCarne: TRLMemo;
    BoletoReciboTopo: TRLReport;
    RLBand5: TRLBand;
    RLDraw79: TRLDraw;
    RLDraw80: TRLDraw;
    RLDraw84: TRLDraw;
    RLDraw85: TRLDraw;
    RLDraw86: TRLDraw;
    RLDraw87: TRLDraw;
    RLDraw88: TRLDraw;
    RLDraw89: TRLDraw;
    RLDraw91: TRLDraw;
    RLDraw92: TRLDraw;
    RLDraw93: TRLDraw;
    RLLabel12: TRLLabel;
    txtNomeCedenteRecTop: TRLLabel;
    RLLabel17: TRLLabel;
    txtCodigoCedenteRecTop: TRLLabel;
    RLLabel22: TRLLabel;
    RLLabel24: TRLLabel;
    txtNumeroDocumentoRecTop: TRLLabel;
    RLLabel26: TRLLabel;
    txtEspecieRecTop: TRLLabel;
    RLLabel34: TRLLabel;
    txtNossoNumeroRecTop: TRLLabel;
    RLLabel53: TRLLabel;
    txtValorDocumentoRecTop: TRLLabel;
    imgBancoRecTop: TRLImage;
    txtNumeroBancoRecTop: TRLLabel;
    RLLabel58: TRLLabel;
    RLLabel59: TRLLabel;
    RLLabel60: TRLLabel;
    txtDataVencimentoRecTop: TRLLabel;
    RLLabel61: TRLLabel;
    RLLabel62: TRLLabel;
    RLLabel63: TRLLabel;
    txtNomeSacadoRecTop: TRLLabel;
    RLLabel64: TRLLabel;
    RLLabel93: TRLLabel;
    RLLabel100: TRLLabel;
    RLLabel104: TRLLabel;
    RLLabel106: TRLLabel;
    RLLabel108: TRLLabel;
    RLLabel110: TRLLabel;
    RLLabel112: TRLLabel;
    RLLabel114: TRLLabel;
    RLLabel116: TRLLabel;
    RLDraw95: TRLDraw;
    RLLabel118: TRLLabel;
    RLDraw96: TRLDraw;
    RLDraw97: TRLDraw;
    RLDraw98: TRLDraw;
    RLDraw99: TRLDraw;
    RLDraw100: TRLDraw;
    RLDraw101: TRLDraw;
    RLDraw102: TRLDraw;
    RLDraw103: TRLDraw;
    RLDraw104: TRLDraw;
    RLDraw105: TRLDraw;
    RLDraw106: TRLDraw;
    RLDraw107: TRLDraw;
    RLDraw108: TRLDraw;
    RLDraw109: TRLDraw;
    imgBancoRecTop1: TRLImage;
    RLDraw110: TRLDraw;
    txtLinhaDigitavelRecTop: TRLLabel;
    RLDraw111: TRLDraw;
    txtNumeroBancoRecTop1: TRLLabel;
    RLLabel120: TRLLabel;
    txtLocalPagamentoRecTop1: TRLMemo;
    txtDataVencimentoRecTop1: TRLLabel;
    RLLabel122: TRLLabel;
    RLDraw112: TRLDraw;
    RLLabel125: TRLLabel;
    txtNomeCedenteRecTop1: TRLLabel;
    RLLabel127: TRLLabel;
    txtCodigoCedenteRecTop1: TRLLabel;
    RLLabel129: TRLLabel;
    txtDataDocumentoRecTop1: TRLLabel;
    RLLabel131: TRLLabel;
    txtNumeroDocumentoRecTop1: TRLLabel;
    RLLabel133: TRLLabel;
    txtEspecieDocRecTop1: TRLLabel;
    RLLabel134: TRLLabel;
    txtAceiteRecTop1: TRLLabel;
    RLLabel136: TRLLabel;
    txtDataProcessamentoRecTop1: TRLLabel;
    RLLabel137: TRLLabel;
    txtNossoNumeroRecTop1: TRLLabel;
    txtUsoBancoRecTop1: TRLLabel;
    RLLabel140: TRLLabel;
    txtCarteiraRecTop1: TRLLabel;
    RLLabel141: TRLLabel;
    txtEspecieRecTop1: TRLLabel;
    RLLabel142: TRLLabel;
    txtQuantidadeRecTop1: TRLLabel;
    RLLabel143: TRLLabel;
    txtValorMoedaRecTop1: TRLLabel;
    RLLabel144: TRLLabel;
    txtValorDocumentoRecTop1: TRLLabel;
    RLLabel170: TRLLabel;
    RLLabel172: TRLLabel;
    txtDescontoRecTop1: TRLLabel;
    RLLabel173: TRLLabel;
    txtMoraMultaRecTop1: TRLLabel;
    RLLabel174: TRLLabel;
    txtValorCobradoRecTop1: TRLLabel;
    RLLabel176: TRLLabel;
    txtNomeSacadoRecTop1: TRLLabel;
    RLLabel177: TRLLabel;
    txtCpfCnpjSacadoRecTop1: TRLLabel;
    txtEnderecoSacadoRecTop1: TRLLabel;
    RLLabel178: TRLLabel;
    txtCidadeSacadoRecTop1: TRLLabel;
    txtCodigoBaixaRecTop1: TRLLabel;
    RLLabel179: TRLLabel;
    RLLabel180: TRLLabel;
    txtReferenciaRecTop1: TRLLabel;
    txtSacadorAvalistaRecTop1: TRLLabel;
    mIntrucoesRecTop1: TRLMemo;
    imgBarrasRecTop1: TRLBarcode;
    RLDraw114: TRLDraw;
    RLDraw115: TRLDraw;
    txtOrientacoesBancoRecTop1: TRLMemo;
    RLDraw94: TRLDraw;
    RLLabel65: TRLLabel;
    txtLinhaDigitavelRecTopRecPag: TRLLabel;
    RLDraw90: TRLDraw;
    RLDraw113: TRLDraw;
    RLLabel181: TRLLabel;
    rlbndComprovanteEntrega2: TRLBand;
    RLDraw116: TRLDraw;
    RLDraw117: TRLDraw;
    RLDraw118: TRLDraw;
    RLDraw120: TRLDraw;
    RLDraw121: TRLDraw;
    RLDraw122: TRLDraw;
    RLDraw123: TRLDraw;
    RLDraw124: TRLDraw;
    RLDraw125: TRLDraw;
    RLDraw126: TRLDraw;
    RLDraw127: TRLDraw;
    RLDraw129: TRLDraw;
    RLLabel66: TRLLabel;
    RLLabel102: TRLLabel;
    txtCodigoCedente5: TRLLabel;
    RLLabel165: TRLLabel;
    RLLabel175: TRLLabel;
    txtNumeroDocumento5: TRLLabel;
    RLLabel183: TRLLabel;
    txtEspecie5: TRLLabel;
    RLLabel185: TRLLabel;
    txtNossoNumero5: TRLLabel;
    RLLabel187: TRLLabel;
    txtValorDocumento5: TRLLabel;
    imgBanco5: TRLImage;
    txtNumeroBanco5: TRLLabel;
    RLLabel191: TRLLabel;
    RLLabel194: TRLLabel;
    txtDataDocumento5: TRLLabel;
    RLLabel196: TRLLabel;
    RLLabel197: TRLLabel;
    RLLabel198: TRLLabel;
    RLLabel200: TRLLabel;
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
    RLDraw130: TRLDraw;
    RLDraw131: TRLDraw;
    RLLabel213: TRLLabel;
    txtDataProcessamento5: TRLLabel;
    RLDraw132: TRLDraw;
    RLLabel192: TRLLabel;
    txtDataVencimento5: TRLLabel;
    RLDraw133: TRLDraw;
    RLDraw134: TRLDraw;
    RLLabel188: TRLLabel;
    txtDesconto4: TRLLabel;
    RLDraw135: TRLDraw;
    RLDraw136: TRLDraw;
    RLLabel195: TRLLabel;
    txtValorCobrado4: TRLLabel;
    RLDraw137: TRLDraw;
    RLLabel215: TRLLabel;
    RLDraw138: TRLDraw;
    RLLabel217: TRLLabel;
    txtMoraMulta5: TRLLabel;
    RLDraw139: TRLDraw;
    rlmCedente5: TRLMemo;
    rlmPagador5: TRLMemo;
    LayoutFaturaDetal: TRLReport;
    RLBandReciboPagDetal: TRLBand;
    txtNomeCedenteTopDet: TRLLabel;
    RLDraw128: TRLDraw;
    RLDraw140: TRLDraw;
    imgBancoTopDet: TRLImage;
    txtEnderecoSacadoTopDet: TRLLabel;
    RLLabel182: TRLLabel;
    RLLabel184: TRLLabel;
    RLDraw141: TRLDraw;
    RLDraw142: TRLDraw;
    txtEnderecoCedenteTopDet: TRLLabel;
    txtCidadeCedenteTopDet: TRLLabel;
    RLDraw181: TRLDraw;
    RLDraw182: TRLDraw;
    txtNomeSacadoTopDet: TRLLabel;
    txtDataDocumentoTopDet: TRLLabel;
    RLLabel235: TRLLabel;
    txtCompetenciaTopDet: TRLLabel;
    RLDraw183: TRLDraw;
    RLDraw184: TRLDraw;
    txtNumeroDocumentoTopDet: TRLLabel;
    RLLabel247: TRLLabel;
    txtCodigoBaixaTopDet: TRLLabel;
    RLLabel186: TRLLabel;
    txtValorDocumentoTopDet: TRLLabel;
    RLLabel193: TRLLabel;
    txtDataVencimentoTopDet: TRLLabel;
    RLLabel239: TRLLabel;
    txtCidadeSacadoTopDet: TRLLabel;
    RLBand8: TRLBand;
    RLDraw186: TRLDraw;
    RLDraw144: TRLDraw;
    RLDraw145: TRLDraw;
    RLDraw146: TRLDraw;
    RLDraw147: TRLDraw;
    RLDraw148: TRLDraw;
    RLDraw149: TRLDraw;
    RLDraw150: TRLDraw;
    RLDraw151: TRLDraw;
    RLDraw152: TRLDraw;
    RLDraw153: TRLDraw;
    RLDraw154: TRLDraw;
    imgBancoDet: TRLImage;
    RLDraw155: TRLDraw;
    txtLinhaDigitavelDet: TRLLabel;
    RLDraw156: TRLDraw;
    txtNumeroBancoDet: TRLLabel;
    txtCodigoCedenteDet: TRLLabel;
    RLLabel234: TRLLabel;
    txtDataDocumentoDet: TRLLabel;
    RLLabel236: TRLLabel;
    txtNumeroDocumentoDet: TRLLabel;
    RLLabel238: TRLLabel;
    txtEspecieDocDet: TRLLabel;
    RLLabel240: TRLLabel;
    txtAceiteDet: TRLLabel;
    RLLabel242: TRLLabel;
    txtDataProcessamentoDet: TRLLabel;
    RLLabel244: TRLLabel;
    txtNossoNumeroDet: TRLLabel;
    RLLabel246: TRLLabel;
    txtUsoBancoDet: TRLLabel;
    RLLabel248: TRLLabel;
    txtCarteiraDet: TRLLabel;
    RLLabel250: TRLLabel;
    txtEspecieDet: TRLLabel;
    RLLabel252: TRLLabel;
    txtQuantidadeDet: TRLLabel;
    RLLabel254: TRLLabel;
    txtValorMoedaDet: TRLLabel;
    RLLabel256: TRLLabel;
    txtValorDocumentoDet: TRLLabel;
    RLLabel258: TRLLabel;
    txtInstrucoesDet: TRLMemo;
    RLLabel259: TRLLabel;
    txtDescontoDet: TRLLabel;
    RLLabel261: TRLLabel;
    txtMoraMultaDet: TRLLabel;
    RLLabel263: TRLLabel;
    txtValorCobradoDet: TRLLabel;
    RLLabel265: TRLLabel;
    txtNomeSacadoDet: TRLLabel;
    RLLabel267: TRLLabel;
    txtCpfCnpjSacadoDet: TRLLabel;
    txtEnderecoSacadoDet: TRLLabel;
    RLLabel270: TRLLabel;
    txtCidadeSacadodet: TRLLabel;
    txtCodigoBaixaDet: TRLLabel;
    RLLabel273: TRLLabel;
    RLLabel274: TRLLabel;
    txtReferenciaDet: TRLLabel;
    RLLabel276: TRLLabel;
    RLLabel277: TRLLabel;
    RLLabel278: TRLLabel;
    RLLabel279: TRLLabel;
    RLDraw157: TRLDraw;
    RLDraw158: TRLDraw;
    RLDraw159: TRLDraw;
    txtEndCedenteDet: TRLLabel;
    txtSacadorAvalistaDet: TRLLabel;
    RLDraw160: TRLDraw;
    RLLabel227: TRLLabel;
    txtLocalPagamentoDet: TRLMemo;
    RLLabel230: TRLLabel;
    txtNomeCedenteDet: TRLLabel;
    RLLabel229: TRLLabel;
    txtDataVencimentoDet: TRLLabel;
    RLLabel232: TRLLabel;
    RLDraw161: TRLDraw;
    imgCodigoBarraDet: TRLBarcode;
    RLLabel243: TRLLabel;
    txtOutrosAcrescimosDet: TRLLabel;
    RLBand6: TRLBand;
    RLDraw187: TRLDraw;
    RLDraw185: TRLDraw;
    RLDraw188: TRLDraw;
    RLDraw189: TRLDraw;
    RLDraw190: TRLDraw;
    RLDraw191: TRLDraw;
    RLDraw192: TRLDraw;
    RLDraw193: TRLDraw;
    RLDraw194: TRLDraw;
    RLDraw195: TRLDraw;
    RLLabel241: TRLLabel;
    txtNumeroDocRecPagDet: TRLLabel;
    RLLabel245: TRLLabel;
    txtEspecieDocRecPagDet: TRLLabel;
    txtDataDocRecPagDet: TRLLabel;
    RLLabel257: TRLLabel;
    RLLabel262: TRLLabel;
    RLDraw196: TRLDraw;
    RLDraw198: TRLDraw;
    RLLabel287: TRLLabel;
    RLDraw199: TRLDraw;
    txtNumeroBancoRecPagDet: TRLLabel;
    RLLabel290: TRLLabel;
    RLLabel291: TRLLabel;
    RLDraw200: TRLDraw;
    txtNossoNumeroRecPagDet: TRLLabel;
    RLLabel294: TRLLabel;
    RLDraw202: TRLDraw;
    txtValorDocumentoRecPagDet: TRLLabel;
    RLLabel296: TRLLabel;
    RLLabel297: TRLLabel;
    txtValorCobradoRecPagDet: TRLLabel;
    RLLabel300: TRLLabel;
    txtDescontoRecPagDet: TRLLabel;
    txtMoraMultaRecPagDet: TRLLabel;
    RLDraw206: TRLDraw;
    txtCodigoCedenteRecPagDet: TRLLabel;
    RLLabel304: TRLLabel;
    txtDataVencimentoRecPagDet: TRLLabel;
    RLLabel139: TRLLabel;
    txtNomeCedenteRecPagDet: TRLLabel;
    txtEndCedenteRecPagDet: TRLLabel;
    txtNomePagadorRecPagDet: TRLLabel;
    txtEndPagadorRecPagDet: TRLLabel;
    imgBancoRecPagDet: TRLImage;
    txtCarteiraRecPagDet: TRLLabel;
    RLLabel253: TRLLabel;
    RLLabel292: TRLLabel;
    RLLabel237: TRLLabel;
    txtLocalPagamentoRecPagDet: TRLMemo;
    RLLabel283: TRLLabel;
    RLBandDetalhamento: TRLBand;
    RLDraw143: TRLDraw;
    RLLabel218: TRLLabel;
    txtDetalhamento: TRLMemo;
    RLLabel189: TRLLabel;
    rlVerso: TRLBand;
    RLDraw162: TRLDraw;
    RLDraw163: TRLDraw;
    txtRemententeNome: TRLLabel;
    txtRemetenteEndereco: TRLLabel;
    txtRemetenteBairro: TRLLabel;
    txtRemetenteCidade: TRLLabel;
    txtRemetenteCep: TRLLabel;
    RLLabel199: TRLLabel;
    RLLabel214: TRLLabel;
    RLDraw164: TRLDraw;
    RLLabel216: TRLLabel;
    RLLabel219: TRLLabel;
    RLDraw165: TRLDraw;
    RLDraw166: TRLDraw;
    RLLabel220: TRLLabel;
    RLLabel221: TRLLabel;
    RLDraw167: TRLDraw;
    RLLabel222: TRLLabel;
    RLDraw168: TRLDraw;
    RLLabel223: TRLLabel;
    RLDraw169: TRLDraw;
    RLDraw170: TRLDraw;
    RLLabel224: TRLLabel;
    RLDraw171: TRLDraw;
    RLLabel225: TRLLabel;
    RLDraw172: TRLDraw;
    RLLabel226: TRLLabel;
    RLDraw173: TRLDraw;
    RLDraw174: TRLDraw;
    RLLabel228: TRLLabel;
    RLLabel231: TRLLabel;
    RLDraw175: TRLDraw;
    RLDraw176: TRLDraw;
    txtDestinatarioNome: TRLLabel;
    txtDestinatarioEndereco: TRLLabel;
    txtDestinatarioBairro: TRLLabel;
    txtDestinatarioCidade: TRLLabel;
    txtDestinatarioCEP: TRLLabel;
    imgVersoBancoTopDet2: TRLImage;
    RLDraw177: TRLDraw;
    imgVersoBancoTopDet: TRLImage;
    RLDraw178: TRLDraw;
    RLLabel233: TRLLabel;
    txtEmpNome2: TRLLabel;
    txtEmpNome: TRLLabel;
    RLDraw179: TRLDraw;
    RLDraw180: TRLDraw;
    RLLabel190: TRLLabel;
    txtLinhaDigitavelComprovanteRec: TRLLabel;
    LayoutTermica: TRLReport;
    RLAngleLabel1: TRLAngleLabel;
    RLBand7: TRLBand;
    RLDraw197: TRLDraw;
    RLDraw201: TRLDraw;
    RLDraw203: TRLDraw;
    RLDraw204: TRLDraw;
    RLDraw205: TRLDraw;
    RLDraw207: TRLDraw;
    RLDraw208: TRLDraw;
    RLDraw209: TRLDraw;
    RLDraw210: TRLDraw;
    RLDraw211: TRLDraw;
    RLDraw212: TRLDraw;
    RLDraw213: TRLDraw;
    RLDraw214: TRLDraw;
    RLDraw216: TRLDraw;
    lTerImgBanco: TRLImage;
    RLDraw217: TRLDraw;
    lTertxtLinhaDigitavel: TRLLabel;
    RLDraw218: TRLDraw;
    lTertxtNumeroBanco: TRLLabel;
    RLLabel251: TRLLabel;
    lTerlblLocalPagto: TRLMemo;
    lTertxtDataVencimento: TRLLabel;
    RLLabel260: TRLLabel;
    RLDraw219: TRLDraw;
    RLLabel264: TRLLabel;
    lTertxtNomeCedente: TRLLabel;
    RLLabel268: TRLLabel;
    lTertxtCodCedente: TRLLabel;
    RLLabel271: TRLLabel;
    lTertxtDataDocumento: TRLLabel;
    RLLabel275: TRLLabel;
    lTertxtNumeroDocumento: TRLLabel;
    RLLabel281: TRLLabel;
    lTertxtEspecieDoc: TRLLabel;
    RLLabel284: TRLLabel;
    lTertxtAceite: TRLLabel;
    RLLabel286: TRLLabel;
    lTertxtDataProcessamento: TRLLabel;
    RLLabel289: TRLLabel;
    lTertxtNossoNumero: TRLLabel;
    RLLabel295: TRLLabel;
    lTertxtUsoBanco: TRLLabel;
    RLLabel299: TRLLabel;
    lTertxtCarteira: TRLLabel;
    RLLabel302: TRLLabel;
    lTertxtEspecie: TRLLabel;
    RLLabel305: TRLLabel;
    lTertxtQuantidade: TRLLabel;
    RLLabel307: TRLLabel;
    RLLabel308: TRLLabel;
    RLLabel309: TRLLabel;
    ltertxtValorDocumento: TRLLabel;
    RLMemo3: TRLMemo;
    lTertxtInstrucoes: TRLMemo;
    RLLabel311: TRLLabel;
    RLLabel312: TRLLabel;
    RLLabel313: TRLLabel;
    RLLabel314: TRLLabel;
    RLLabel315: TRLLabel;
    RLLabel316: TRLLabel;
    RLLabel317: TRLLabel;
    lTertxtNomePagador: TRLLabel;
    RLLabel319: TRLLabel;
    lTertxtCpfCnpjPagador: TRLLabel;
    lTertxtEndPagador: TRLLabel;
    RLLabel322: TRLLabel;
    lTertxtNomeSacado: TRLLabel;
    RLLabel324: TRLLabel;
    RLLabel325: TRLLabel;
    RLLabel326: TRLLabel;
    lTertxtCodBarras: TRLBarcode;
    RLLabel327: TRLLabel;
    RLLabel328: TRLLabel;
    RLLabel329: TRLLabel;
    RLLabel330: TRLLabel;
    RLDraw220: TRLDraw;
    RLLabel331: TRLLabel;
    RLLabel332: TRLLabel;
    RLDraw221: TRLDraw;
    RLLabel333: TRLLabel;
    lTertxtEndCedente: TRLMemo;
    lTertxtOrientacoesBanco: TRLMemo;
    RLLabel255: TRLLabel;
    RLLabel269: TRLLabel;
    RLDraw229: TRLDraw;
    RLLabel272: TRLLabel;
    RLLabel280: TRLLabel;
    RLLabel282: TRLLabel;
    lTertxtQuantidade2: TRLLabel;
    RLDraw230: TRLDraw;
    RLLabel288: TRLLabel;
    RLLabel293: TRLLabel;
    RLLabel298: TRLLabel;
    RLLabel301: TRLLabel;
    RLDraw232: TRLDraw;
    RLLabel303: TRLLabel;
    RLLabel310: TRLLabel;
    RLLabel318: TRLLabel;
    RLDraw234: TRLDraw;
    lTerImgBanco2: TRLImage;
    lTertxtTotPar: TRLLabel;
    lTertxtDataVencimento2: TRLLabel;
    lTertxtCodCedente2: TRLLabel;
    ltertxtValorDocumento2: TRLLabel;
    lTertxtNossoNumero2: TRLLabel;
    lTertxtCpfCnpjPagador2: TRLLabel;
    lTertxtParcela: TRLLabel;
    lTertxtNomePagador2: TRLMemo;
    lTerMemoCedente: TRLMemo;
    RLLabel338: TRLLabel;
    RLDraw235: TRLDraw;
    lTertxtNumeroDocumento2: TRLLabel;
    RLDraw215: TRLDraw;
    RLDraw222: TRLDraw;
    RLDraw223: TRLDraw;
    txtSwHouse80mm: TRLAngleLabel;
    LayoutServicos: TRLReport;
    RLBandServicosTopo: TRLBand;
    RLDraw258: TRLDraw;
    RLDraw259: TRLDraw;
    RLDraw271: TRLDraw;
    RLDraw266: TRLDraw;
    RLDraw263: TRLDraw;
    RLDraw233: TRLDraw;
    txtNomeCedenteTopServicos: TRLLabel;
    RLDraw236: TRLDraw;
    imgLogoEmpresaServicos: TRLImage;
    RLDraw237: TRLDraw;
    RLDraw238: TRLDraw;
    txtNomeSacadoTopServicos: TRLLabel;
    RLLabel344: TRLLabel;
    imgBancoTopServicos: TRLImage;
    txtNumeroBancoTopServicos: TRLLabel;
    RLDraw239: TRLDraw;
    RLLabel428: TRLLabel;
    RLLabel285: TRLLabel;
    RLLabel436: TRLLabel;
    txtLocalPagamentoTopServicos: TRLMemo;
    RLLabel414: TRLLabel;
    txtDataVencimentoTopServicos: TRLLabel;
    RLLabel435: TRLLabel;
    txtNossoNumeroTopServicos: TRLLabel;
    RLLabel417: TRLLabel;
    txtValorDocumentoTopServicos: TRLLabel;
    RLLabel419: TRLLabel;
    txtDescontoTopServicos: TRLLabel;
    RLLabel422: TRLLabel;
    txtMultaTopServicos: TRLLabel;
    RLLabel420: TRLLabel;
    txtValorCobradoTopServicos: TRLLabel;
    txtDataDocumentoTopServicos: TRLLabel;
    RLLabel415: TRLLabel;
    RLLabel405: TRLLabel;
    txtNumeroDocumentoTopServicos: TRLLabel;
    txtEspecieDocTopServicos: TRLLabel;
    RLLabel407: TRLLabel;
    RLLabel411: TRLLabel;
    txtCarteiraTopServicos: TRLLabel;
    txtCodigoCedenteTopServicos: TRLLabel;
    RLLabel426: TRLLabel;
    RLDraw256: TRLDraw;
    RLDraw269: TRLDraw;
    RLDraw240: TRLDraw;
    RLDraw241: TRLDraw;
    RLDraw242: TRLDraw;
    RLDraw260: TRLDraw;
    RLDraw261: TRLDraw;
    RLLabel437: TRLLabel;
    txtDetalhamentoServicos: TRLMemo;
    RLLabel439: TRLLabel;
    RLDraw257: TRLDraw;
    RLLabel47: TRLLabel;
    txtDataDocumentoTopServicos2: TRLLabel;
    RLLabel266: TRLLabel;
    txtNumeroDocumentoTopServicos2: TRLLabel;
    RLLabel335: TRLLabel;
    txtLinhaDigitavelTopServicos: TRLLabel;
    RLDraw262: TRLDraw;
    RLDraw265: TRLDraw;
    RLDraw267: TRLDraw;
    RLDraw268: TRLDraw;
    RLLabel306: TRLLabel;
    txtDataProcessamentoTopServicos: TRLLabel;
    RLLabel320: TRLLabel;
    RLBandTituloServicos: TRLBand;
    imgBancoServicos: TRLImage;
    RLDraw243: TRLDraw;
    RLDraw244: TRLDraw;
    RLDraw245: TRLDraw;
    RLDraw246: TRLDraw;
    RLDraw247: TRLDraw;
    RLDraw248: TRLDraw;
    RLDraw249: TRLDraw;
    RLDraw250: TRLDraw;
    RLDraw251: TRLDraw;
    RLDraw252: TRLDraw;
    RLDraw253: TRLDraw;
    RLDraw254: TRLDraw;
    RLDraw255: TRLDraw;
    txtLinhaDigitavelServicos: TRLLabel;
    RLDraw264: TRLDraw;
    txtNumeroBancoServicos: TRLLabel;
    txtCodigoCedenteServicos: TRLLabel;
    RLLabel349: TRLLabel;
    txtDataDocumentoServicos: TRLLabel;
    RLLabel351: TRLLabel;
    txtNumeroDocumentoServicos: TRLLabel;
    RLLabel353: TRLLabel;
    txtEspecieDocServicos: TRLLabel;
    RLLabel355: TRLLabel;
    txtAceiteServicos: TRLLabel;
    RLLabel357: TRLLabel;
    txtDataProcessamentoServicos: TRLLabel;
    RLLabel359: TRLLabel;
    txtNossoNumeroServicos: TRLLabel;
    RLLabel361: TRLLabel;
    txtUsoBancoServicos: TRLLabel;
    RLLabel363: TRLLabel;
    txtCarteiraServicos: TRLLabel;
    RLLabel365: TRLLabel;
    txtEspecieServicos: TRLLabel;
    RLLabel367: TRLLabel;
    txtQuantidadeServicos: TRLLabel;
    RLLabel369: TRLLabel;
    txtValorServicos: TRLLabel;
    RLLabel371: TRLLabel;
    txtValorDocumentoServicos: TRLLabel;
    RLLabel373: TRLLabel;
    txtInstrucoesServicos: TRLMemo;
    RLLabel374: TRLLabel;
    txtValorDescontoServicos: TRLLabel;
    RLLabel376: TRLLabel;
    txtValorMoraMultaServicos: TRLLabel;
    RLLabel378: TRLLabel;
    txtValorCobradoServicos: TRLLabel;
    RLLabel380: TRLLabel;
    txtNomeSacadoServicos: TRLLabel;
    RLLabel382: TRLLabel;
    txtCPFCNPJSacadoServicos: TRLLabel;
    txtEnderecoSacadoServicos: TRLLabel;
    RLLabel385: TRLLabel;
    txtCidadeSacadoServicos: TRLLabel;
    txtCodigoBaixaServicos: TRLLabel;
    RLLabel388: TRLLabel;
    txtReferenciaServicos: TRLLabel;
    RLLabel391: TRLLabel;
    RLLabel392: TRLLabel;
    RLLabel393: TRLLabel;
    RLLabel394: TRLLabel;
    RLDraw270: TRLDraw;
    RLDraw272: TRLDraw;
    RLDraw273: TRLDraw;
    RLDraw274: TRLDraw;
    RLLabel397: TRLLabel;
    txtLocalPagamentoServicos: TRLMemo;
    RLLabel398: TRLLabel;
    txtNomeCedenteServicos: TRLLabel;
    RLLabel400: TRLLabel;
    txtDataVencimentoServicos: TRLLabel;
    RLLabel402: TRLLabel;
    RLDraw275: TRLDraw;
    imgCodigoBarraServicos: TRLBarcode;
    RLLabel403: TRLLabel;
    txtOutrosAcrescimosServicos: TRLLabel;
    LayoutCarneA5: TRLReport;
    RLBandCarneA5Topo: TRLBand;
    RLDraw276: TRLDraw;
    logoBanco1CA5: TRLImage;
    RLDraw277: TRLDraw;
    RLDraw278: TRLDraw;
    RLDraw280: TRLDraw;
    RLDraw281: TRLDraw;
    RLDraw282: TRLDraw;
    RLDraw283: TRLDraw;
    RLDraw284: TRLDraw;
    RLDraw285: TRLDraw;
    RLDraw286: TRLDraw;
    RLDraw287: TRLDraw;
    RLDraw288: TRLDraw;
    RLDraw289: TRLDraw;
    RLDraw290: TRLDraw;
    RLDraw291: TRLDraw;
    linhaDigitavel2CA5: TRLLabel;
    RLDraw292: TRLDraw;
    txtNumBanco2CA5: TRLLabel;
    RLLabel424: TRLLabel;
    txtLocalPGCA5: TRLMemo;
    RLLabel321: TRLLabel;
    RLDraw293: TRLDraw;
    RLLabel427: TRLLabel;
    txtNomeCedente2CA5: TRLLabel;
    RLLabel429: TRLLabel;
    txtAgenciaCedente2CA5: TRLLabel;
    RLLabel431: TRLLabel;
    txtDtDoc2CA5: TRLLabel;
    RLLabel433: TRLLabel;
    txtNunDoc2CA5: TRLLabel;
    RLLabel323: TRLLabel;
    txtEspecDoc2CA5: TRLLabel;
    RLLabel334: TRLLabel;
    txtAceite2CA5: TRLLabel;
    RLLabel336: TRLLabel;
    txtDtProc2CA5: TRLLabel;
    RLLabel441: TRLLabel;
    txtNossoNum2CA5: TRLLabel;
    RLLabel443: TRLLabel;
    RLLabel445: TRLLabel;
    RLLabel447: TRLLabel;
    RLLabel449: TRLLabel;
    RLLabel451: TRLLabel;
    RLLabel453: TRLLabel;
    txtValDoc2CA5: TRLLabel;
    RLMemo10: TRLMemo;
    txtLinhaDigitavel2CA5: TRLMemo;
    RLLabel455: TRLLabel;
    txtDesconto2CA5: TRLLabel;
    RLLabel457: TRLLabel;
    txtMulta2CA5: TRLLabel;
    RLLabel459: TRLLabel;
    txtValCobrado2CA5: TRLLabel;
    RLLabel461: TRLLabel;
    txtNomePagador2CA5: TRLLabel;
    txtCPF2CA5: TRLLabel;
    RLLabel466: TRLLabel;
    txtBenefeciario2CA5: TRLLabel;
    txtCodBaixa2CA5: TRLLabel;
    RLLabel469: TRLLabel;
    txtBarCodeCA5: TRLBarcode;
    RLLabel472: TRLLabel;
    RLLabel473: TRLLabel;
    RLLabel474: TRLLabel;
    RLDraw294: TRLDraw;
    txtLogradouro2CA5: TRLLabel;
    RLLabel478: TRLLabel;
    RLLabel479: TRLLabel;
    txtEnderecoCedente2CA5: TRLMemo;
    RLDraw296: TRLDraw;
    RLLabel481: TRLLabel;
    txtVenc2CA5: TRLLabel;
    RLDraw299: TRLDraw;
    RLDraw300: TRLDraw;
    RLDraw301: TRLDraw;
    RLDraw307: TRLDraw;
    RLDraw308: TRLDraw;
    RLDraw309: TRLDraw;
    RLDraw310: TRLDraw;
    RLDraw311: TRLDraw;
    txtNumBanco1CA5: TRLLabel;
    RLLabel368: TRLLabel;
    RLLabel370: TRLLabel;
    txtCedente1CA5: TRLLabel;
    RLLabel337: TRLLabel;
    txtNumDoc1CA5: TRLLabel;
    RLLabel345: TRLLabel;
    txtVenc1CA5: TRLLabel;
    RLLabel396: TRLLabel;
    RLLabel347: TRLLabel;
    txtValDoc1CA5: TRLLabel;
    txtValCobrado1CA5: TRLLabel;
    RLLabel406: TRLLabel;
    txtNomePagador1CA5: TRLLabel;
    RLLabel409: TRLLabel;
    txtCPF1CA5: TRLLabel;
    RLLabel413: TRLLabel;
    linhaDigitavel1CA5: TRLLabel;
    txtEspec2CA5: TRLLabel;
    txtValor2CA5: TRLLabel;
    txtQtd2CA5: TRLLabel;
    txtCarteira2CA5: TRLLabel;
    txtCIP2CA5: TRLLabel;
    txtUsoBanco2CA5: TRLLabel;
    txtEnderecoPagador1CA5: TRLMemo;
    RLLabel463: TRLLabel;
    RLDraw313: TRLDraw;
    RLLabel404: TRLLabel;
    RLLabel356: TRLLabel;
    txtEnderecoPagador2CA5: TRLMemo;
    txtNossNum1CA5: TRLLabel;
    RLDraw279: TRLDraw;
    RLDraw295: TRLDraw;
    RLDraw297: TRLDraw;
    txtAgenciaCedente1CA5: TRLLabel;
    RLLabel340: TRLLabel;
    txtEnderecoCedente1CA5: TRLMemo;
    logoBanco2CA5: TRLImage;
    imgLogoEmpresaBoleto: TRLImage;
    imgQRCodePixCarne: TRLImage;
    RLBand9: TRLBand;
    lblCopiaeCola: TRLLabel;
    RLDraw302: TRLDraw;
    RLDraw303: TRLDraw;
    RLDraw298: TRLDraw;
    imgQrCodePixReciboTopo: TRLImage;
    imgQrCodePixFaturaDetail: TRLImage;
    imgQrCodePixLayoutBoleto: TRLImage;
    imgQrCodePixCarneA5: TRLImage;
    imgQrCodePixServicos: TRLImage;
    mmNomeSacadorAval4: TRLMemo;
    mmEndSacadoCarne: TRLMemo;
    txtSwHouseReciboTopo: TRLLabel;
    txtSwHouseCarne: TRLLabel;
    txtSwHouseFaturaDetail: TRLLabel;
    txtSwHousePadrao: TRLLabel;
    txtEndPagador2: TRLMemo;
    txtEndPagador3: TRLMemo;
    txtEndCedenteRecTop1: TRLMemo;
    lblPaguePix: TRLLabel;
    txtSwHouseServicos: TRLAngleLabel;
    txtEnderecoSacadoTopServicos: TRLMemo;
    txtEnderecoCedenteTopServicos: TRLMemo;
    txtCA5Sw: TRLAngleLabel;
    txtCodigoBaixa2: TRLLabel;
    RLLabel98: TRLLabel;
    procedure BoletoCarneBeforePrint ( Sender: TObject; var PrintIt: boolean ) ;
    procedure BoletoCarneDataCount ( Sender: TObject; var DataCount: integer ) ;
    procedure BoletoCarneDataRecord ( Sender: TObject; RecNo: integer;
       CopyNo: integer; var Eof: boolean; var RecordAction: TRLRecordAction ) ;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LayoutBoletoBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure LayoutBoletoDataCount(Sender: TObject; var DataCount: integer);
    procedure LayoutBoletoDataRecord(Sender: TObject; RecNo: integer;
       CopyNo: integer; var Eof: boolean; var RecordAction: TRLRecordAction);
    procedure RLBand1BeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure RLBand2BeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure RLBand3BeforePrint ( Sender: TObject; var PrintIt: boolean ) ;
    procedure RLBand4BeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure RLBand5BeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure RLBandPixBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlbndComprovanteEntrega2BeforePrint(Sender: TObject;
      var PrintIt: Boolean);
    procedure LayoutFaturaDetalBeforePrint(Sender: TObject;
      var PrintIt: Boolean);
    procedure LayoutFaturaDetalDataCount(Sender: TObject;
      var DataCount: Integer);
    procedure LayoutFaturaDetalDataRecord(Sender: TObject; RecNo,
      CopyNo: Integer; var Eof: Boolean; var RecordAction: TRLRecordAction);
    procedure RLBandReciboPagDetalBeforePrint(Sender: TObject;
      var PrintIt: Boolean);
    procedure Verso;
    procedure RLBandDetalhamentoBeforePrint(Sender: TObject;
      var PrintIt: Boolean);
    procedure RLBand6BeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure RLBand8BeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure RLBand7BeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure LayoutTermicaBeforePrint(Sender: TObject;
      var PrintIt: Boolean);

    procedure LayoutTermicaDataCount(Sender: TObject;
      var DataCount: Integer);
    procedure LayoutTermicaDataRecord(Sender: TObject; RecNo,
      CopyNo: Integer; var Eof: Boolean;
      var RecordAction: TRLRecordAction);
    procedure ValidarCIP;
    procedure LayoutServicosBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure LayoutServicosDataCount(Sender: TObject; var DataCount: Integer);
    procedure LayoutServicosDataRecord(Sender: TObject; RecNo, CopyNo: Integer; var Eof: Boolean; var RecordAction: TRLRecordAction);
    procedure RLBandServicosTopoBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure RLBandTituloServicosBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure LayoutCarneA5BeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure LayoutCarneA5DataCount(Sender: TObject; var DataCount: Integer);
    procedure LayoutCarneA5DataRecord(Sender: TObject; RecNo, CopyNo: Integer; var Eof: Boolean; var RecordAction: TRLRecordAction);
     procedure memoEndCedenteCarneAfterPrint(Sender: TObject);
    procedure RLBandCarneA5TopoBeforePrint(Sender: TObject;
      var PrintIt: Boolean);


  private
     MensagemPadrao: TStringList;
     Detalhamento  : TStringList;
     fBoletoFC: TACBrBoletoFCFortes;
     fIndice: Integer;
     function GetACBrTitulo: TACBrTitulo;
     procedure printEMVPix(const AEMV : String; out ASender : TRLImage);
     procedure AjustarMargem(FReport: TRLReport; AConfig: TACBrBoletoFCClass);

    { Private declarations }
  public
    { Public declarations }
    property Indice   : Integer read fIndice ;
    property BoletoFC : TACBrBoletoFCFortes read fBoletoFC ;
    property Titulo   : TACBrTitulo read GetACBrTitulo ;
  end;

var
  ACBrBoletoFCFortesForm: TACBrBoletoFCFortesFr;

implementation

Uses
  strutils, ACBrImage, ACBrDelphiZXingQRCode, ACBrUtil.Strings,
  ACBrUtil.Base,
  ACBrUtil.FilesIO
  {$ifndef FPC}
    ,jpeg
  {$ENDIF};

{$ifdef FPC}
  {$R *.lfm}
{$else}
  {$R *.dfm}
  {$R ACBrBoletoFCFortes.dcr}
{$ENDIF}

{ TACBrBoletoFCFortes }

constructor TACBrBoletoFCFortes.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TACBrBoletoFCFortes.Imprimir;
begin
  inherited Imprimir;    // Executa verificações padroes
  ImprimirInternal(nil);
end;

procedure TACBrBoletoFCFortes.Imprimir(AStream: TStream);
begin
  inherited Imprimir(AStream);    // Executa verificações padroes
  if Filtro <> fiPDF then
    raise Exception.Create(ACBrStr('Impressão por Stream apenas para o Filtro PDF.'));
  ImprimirInternal(AStream);
end;

procedure TACBrBoletoFCFortes.ImprimirInternal(AStream: TStream);
var
  frACBrBoletoFortes : TACBRBoletoFCFortesFr;
  RLFiltro : TRLCustomSaveFilter;
  RLLayout: TRLReport;
  i: Integer;
  Bitmap : TBitmap;
  JPEG: TJPEGImage;
begin
  frACBrBoletoFortes := TACBrBoletoFCFortesFr.Create(Self);
  try
     with frACBrBoletoFortes do
     begin
        case LayOut of
           lCarne         : RLLayout := BoletoCarne;
           lReciboTopo    : RLLayout := BoletoReciboTopo;
           lFaturaDetal   : RLLayout := LayoutFaturaDetal;
           lTermica80mm   : RLLayout := LayoutTermica;
           lPrestaServicos: RLLayout := LayoutServicos;
           lCarneA5       : RLLayout := LayoutCarneA5;
        else
           RLLayout:= LayoutBoleto;
        end;

        if (NumCopias > 0) and (RLPrinter.Copies <> NumCopias) then
        begin
          RLPrinter.Copies := NumCopias;
        end;

        RLLayout.PrintDialog  := MostrarSetup;
        RLLayout.ShowProgress := MostrarProgresso;
        RLLayout.Title        := TituloRelatorio;

        RLLayout.JobTitle := 'boleto ' + ACBrBoleto.Banco.nome;

        if TituloPreview <> '' then
        begin
          RLLayout.PreviewOptions.Defaults := pdIgnoreDefaults;
          RLLayout.PreviewOptions.Caption  := TituloPreview;
        end
        else
          RLLayout.PreviewOptions.Defaults := pdUseDefaults;

        if PrinterName <> '' then
           RLPrinter.PrinterName := PrinterName;

        if Filtro = fiNenhum then
         begin
           if MostrarPreview then
           begin
              RLLayout.Title := '';
              RLLayout.PreviewModal;
           end
           else
              RLLayout.Print;
         end
        else
        begin
          if not RLLayout.Prepare then
            Exit;

           case Filtro of
             fiHTML : RLFiltro := RLHTMLFilter1;
             fiJPG:
             begin
               for i := 0 to RLLayout.Pages.PageCount - 1 do
               begin
                 Bitmap := NeedAuxBitmap;
                 Bitmap.Width := RLLayout.Pages[i].Width;
                 Bitmap.Height := RLLayout.Pages[i].Height;
                 Bitmap.PixelFormat := pf32bit;

                 Bitmap.Canvas.Brush.Color := clWhite;
                 Bitmap.Canvas.Brush.Style := bsSolid;
                 Bitmap.Canvas.FillRect(Rect(0, 0, Bitmap.Width, Bitmap.Height));

                 RLLayout.Pages[i].PaintTo(Bitmap.Canvas, Rect(0, 0, Bitmap.Width, Bitmap.Height));

                 NomeArquivo := ChangeFileExt(NomeArquivo, '');

                 JPEG := TJPEGImage.Create;
                 try
                   JPEG.CompressionQuality := 100;
                   JPEG.Assign(Bitmap);
                   JPEG.SaveToFile(NomeArquivo + FormatCurr('000', I+1) + '.jpeg');
                 finally
                   JPEG.Free;
                 end;
               end;
               exit;
             end;
           else
              RLFiltro := RLPDFFilter1;
           end;

            RLFiltro.ShowProgress := MostrarProgresso;
            RLFiltro.FileName := NomeArquivo;

            if RLFiltro = RLPDFFilter1 then
            begin
              RLPDFFilter1.DocumentInfo.Title := RLLayout.Title;
              if Assigned(AStream) then
                RLPDFFilter1.FilterPages(RLLayout.Pages, AStream)
              else
                RLPDFFilter1.FilterPages(RLLayout.Pages)
            end
            else
              RLFiltro.FilterPages(RLLayout.Pages);
        end;
     end;
  finally
     frACBrBoletoFortes.Free ;
  end;
end;

{ TACBrBoletoFCFortesFr }
procedure TACBrBoletoFCFortesFr.FormCreate(Sender: TObject);
//var
//  I : Integer ;
begin
   fIndice   := 0 ;
   fBoletoFC := TACBrBoletoFCFortes(Owner) ;  // Link para o Pai
   MensagemPadrao  := TStringList.Create;
   Detalhamento    := TStringList.Create;
   RLBand4.Visible := (fBoletoFC.LayOut = lPadraoEntrega) ;
   rlbndComprovanteEntrega2.Visible := (fBoletoFC.LayOut = lPadraoEntrega2) ;
   RLBandPix.Visible:= (fBoletoFC.LayOut = lPadraoPIX);
   ValidarCIP;
   if fBoletoFC.AlterarEscalaPadrao then
   begin
     Self.Scaled := False;
     Self.ScaleBy(fBoletoFC.NovaEscala, Screen.PixelsPerInch );
   end;

   {$IFDEF UNICODE}
   { // Fontes do ACBr estão em CP1252, convertendo textos para UTF-8 //
    For I := 0 to ComponentCount-1 do
    begin
       if Components[I] is TRLMemo then
        begin
          with TRLMemo( Components[I] ) do
             Lines.Text := AnsiToUtf8( Lines.Text )
        end
       else if Components[I] is TRLLabel then
        begin
          with TRLLabel( Components[I] ) do
             Caption := AnsiToUtf8( Caption )
        end
    end ;}
   {$ENDIF}
end;

procedure TACBrBoletoFCFortesFr.FormDestroy ( Sender: TObject ) ;
begin
   MensagemPadrao.Free;
   Detalhamento.Free;
end;

procedure TACBrBoletoFCFortesFr.BoletoCarneBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin

   fIndice := 0;
   txtSwHouseReciboTopo.Caption := ACBrStr(BoletoFC.SoftwareHouse) ;
   txtSwHouseCarne.Caption := ACBrStr(BoletoFC.SoftwareHouse) ;
   AjustarMargem(BoletoCarne,BoletoFC);

end;

procedure TACBrBoletoFCFortesFr.BoletoCarneDataCount(Sender: TObject;
  var DataCount: integer);
begin
   DataCount := fBoletoFC.ACBrBoleto.ListadeBoletos.Count ;
end;

procedure TACBrBoletoFCFortesFr.BoletoCarneDataRecord(Sender: TObject;
  RecNo: integer; CopyNo: integer; var Eof: boolean;
  var RecordAction: TRLRecordAction);
begin
   fIndice := RecNo - 1 ;

   if (fBoletoFC.IndiceImprimirIndividual >= 0) then
     Eof := (RecNo > 1)
   else
     Eof := (RecNo > fBoletoFC.ACBrBoleto.ListadeBoletos.Count) ;

   RecordAction := raUseIt ;
end;

function TACBrBoletoFCFortesFr.GetACBrTitulo: TACBrTitulo;
begin
  if (fBoletoFC.IndiceImprimirIndividual >= 0) then
    Result := fBoletoFC.ACBrBoleto.ListadeBoletos[ fBoletoFC.IndiceImprimirIndividual ]
  else
    Result := fBoletoFC.ACBrBoleto.ListadeBoletos[ fIndice ];
end;


procedure TACBrBoletoFCFortesFr.LayoutBoletoBeforePrint(Sender: TObject;
   var PrintIt: boolean);
begin
   fIndice := 0 ;
   txtSwHousePadrao.Caption := ACBrStr(BoletoFC.SoftwareHouse) ;
   AjustarMargem(LayoutBoleto,BoletoFC);
end;

procedure TACBrBoletoFCFortesFr.LayoutBoletoDataCount(Sender: TObject;
   var DataCount: integer);
begin
   DataCount := fBoletoFC.ACBrBoleto.ListadeBoletos.Count ;
end;

procedure TACBrBoletoFCFortesFr.LayoutBoletoDataRecord(Sender: TObject;
   RecNo: integer; CopyNo: integer; var Eof: boolean;
   var RecordAction: TRLRecordAction);
begin
   fIndice := RecNo - 1 ;

   if (fBoletoFC.IndiceImprimirIndividual >= 0) then
     Eof := (RecNo > 1)
   else
     Eof := (RecNo > fBoletoFC.ACBrBoleto.ListadeBoletos.Count) ;

   RecordAction := raUseIt ;
end;

procedure TACBrBoletoFCFortesFr.LayoutCarneA5BeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  fIndice := 0 ;
  txtCA5Sw.Caption := BoletoFC.SoftwareHouse ;
  AjustarMargem(layOutCarneA5,BoletoFC);

end;

procedure TACBrBoletoFCFortesFr.LayoutCarneA5DataCount(Sender: TObject;
  var DataCount: Integer);
begin
   DataCount := fBoletoFC.ACBrBoleto.ListadeBoletos.Count ;
end;

procedure TACBrBoletoFCFortesFr.LayoutCarneA5DataRecord(Sender: TObject; RecNo,
  CopyNo: Integer; var Eof: Boolean; var RecordAction: TRLRecordAction);
begin
   fIndice := RecNo - 1 ;

   if (fBoletoFC.IndiceImprimirIndividual >= 0) then
     Eof := (RecNo > 1)
   else
     Eof := (RecNo > fBoletoFC.ACBrBoleto.ListadeBoletos.Count) ;
   RecordAction := raUseIt ;
end;

procedure TACBrBoletoFCFortesFr.LayoutFaturaDetalBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  fIndice := 0 ;
  txtSwHouseFaturaDetail.Caption := ACBrStr(BoletoFC.SoftwareHouse) ;
  //txtSwHouseDet.Caption := BoletoFC.SoftwareHouse ;
  AjustarMargem(LayoutFaturaDetal,BoletoFC);
end;

procedure TACBrBoletoFCFortesFr.LayoutFaturaDetalDataCount(Sender: TObject;
  var DataCount: Integer);
begin
  DataCount := fBoletoFC.ACBrBoleto.ListadeBoletos.Count ;
end;

procedure TACBrBoletoFCFortesFr.LayoutFaturaDetalDataRecord(Sender: TObject;
  RecNo, CopyNo: Integer; var Eof: Boolean; var RecordAction: TRLRecordAction);
begin
  fIndice := RecNo - 1 ;

  if (fBoletoFC.IndiceImprimirIndividual >= 0) then
     Eof := (RecNo > 1)
  else
    Eof := (RecNo > fBoletoFC.ACBrBoleto.ListadeBoletos.Count) ;

  RecordAction := raUseIt ;
end;

procedure TACBrBoletoFCFortesFr.LayoutServicosBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
   fIndice := 0;
   txtSwHouseServicos.Caption := BoletoFC.SoftwareHouse ;
   AjustarMargem(LayoutServicos,BoletoFC);
end;

procedure TACBrBoletoFCFortesFr.LayoutServicosDataCount(Sender: TObject;
  var DataCount: Integer);
begin
  DataCount := fBoletoFC.ACBrBoleto.ListadeBoletos.Count ;
end;

procedure TACBrBoletoFCFortesFr.LayoutServicosDataRecord(Sender: TObject; RecNo,
  CopyNo: Integer; var Eof: Boolean; var RecordAction: TRLRecordAction);
begin
   fIndice := RecNo - 1 ;

   if (fBoletoFC.IndiceImprimirIndividual >= 0) then
     Eof := (RecNo > 1)
   else
     Eof := (RecNo > fBoletoFC.ACBrBoleto.ListadeBoletos.Count) ;

   RecordAction := raUseIt ;
end;

procedure TACBrBoletoFCFortesFr.LayoutTermicaBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
   fIndice := 0 ;
   txtSwHouse80mm.Caption := BoletoFC.SoftwareHouse;
   AjustarMargem(LayoutTermica,BoletoFC);
end;

procedure TACBrBoletoFCFortesFr.LayoutTermicaDataCount(Sender: TObject;
  var DataCount: Integer);
begin
   DataCount := fBoletoFC.ACBrBoleto.ListadeBoletos.Count ;
end;

procedure TACBrBoletoFCFortesFr.LayoutTermicaDataRecord(Sender: TObject;
  RecNo, CopyNo: Integer; var Eof: Boolean;
  var RecordAction: TRLRecordAction);
begin
   fIndice := RecNo - 1 ;

   if (fBoletoFC.IndiceImprimirIndividual >= 0) then
     Eof := (RecNo > 1)
   else
     Eof := (RecNo > fBoletoFC.ACBrBoleto.ListadeBoletos.Count) ;

   RecordAction := raUseIt ;
end;

procedure TACBrBoletoFCFortesFr.ValidarCIP;
var
  isCIP: Boolean;
begin
  isCIP := ( fBoletoFC.ACBrBoleto.Banco.TipoCobranca in [cobBradesco, cobBradescoSICOOB] );
  txtCIP1.Visible:= isCIP;
  lblCIP1.Visible:= isCIP;
  rlCIP1.Visible:= isCIP;
  txtCIP2.Visible:= isCIP;
  lblCIP2.Visible:= isCIP;
  rlCIP2.Visible:= isCIP;
  txtCIP3.Visible:= isCIP;
  lblCIP3.Visible:= isCIP;
  rlCIP3.Visible:= isCIP;
  txtCIP4.Visible:= isCIP;
  lblCIP4.Visible:= isCIP;
  rlCIP4.Visible:= isCIP;

end;

procedure TACBrBoletoFCFortesFr.RLBand1BeforePrint(Sender: TObject;
   var PrintIt: boolean);
Var
   NossoNum,CodCedente,TipoDoc, Carteira: String;
begin
   with fBoletoFC.ACBrBoleto do
   begin
      NossoNum    := Banco.MontarCampoNossoNumero( Titulo );
      CodCedente  := Banco.MontarCampoCodigoCedente(titulo);
      Carteira    := Banco.MontarCampoCarteira(Titulo);

      case Cedente.TipoInscricao of
         pFisica   : TipoDoc:= 'CPF: ';
         pJuridica : TipoDoc:= 'CNPJ: ';
      else
         TipoDoc := 'DOC.: ';
      end;

      MensagemPadrao.Clear;
      MensagemPadrao.Text := Titulo.Mensagem.Text;
      ACBrBoletoFC.ACBrBoleto.AdicionarMensagensPadroes(Titulo,MensagemPadrao);

      fBoletoFC.CarregaLogo( imgBanco2.Picture, Banco.Numero );
      txtNumeroBanco2.Caption         := IntToStrZero(Banco.Numero, 3)+ '-' +
                                         IfThen(Banco.Digito >= 10,'X',
                                         IntToStrZero(Banco.Digito, 1));
      lblLocalPagto.Lines.Text        := Titulo.LocalPagamento;
      txtDataVencimento2.Caption      := IfThen(Titulo.Vencimento > 0,FormatDateTime('dd/mm/yyyy', Titulo.Vencimento));
      txtNomeCedente2.Caption         := Cedente.Nome+ ' - '+TipoDoc + Cedente.CNPJCPF;
      txtCodigoCedente2.Caption       := CodCedente;
      txtDataDocumento2.Caption       := FormatDateTime('dd/mm/yyyy', Titulo.DataDocumento);
      txtNumeroDocumento2.Caption     := Titulo.NumeroDocumento;
      txtEspecieDoc2.Caption          := Titulo.EspecieDoc;
      txtAceite2.Caption              := fBoletoFC.DefineAceiteImpressao(Titulo);
      txtDataProcessamento2.Caption   := IfThen(Titulo.DataProcessamento = 0,
                                                FormatDateTime('dd/mm/yyyy',Now),
                                                FormatDateTime('dd/mm/yyyy',Titulo.DataProcessamento));
      txtNossoNumero2.Caption         := NossoNum;
      txtUsoBanco2.Caption            := Titulo.UsoBanco;
      txtCarteira2.Caption            := Carteira;
      txtEspecie2.Caption             := IfThen(trim(Titulo.EspecieMod) = '','R$',Titulo.EspecieMod);
      txtValorDocumento2.Caption      := IfThen(Titulo.ValorDocumento > 0,FormatFloatBr(Titulo.ValorDocumento, ',0.00'));
      txtCIP1.Caption                 := Banco.CIP;

      with Titulo.Sacado do
      begin
        txtNomePagador2.Caption       := NomeSacado;
        txtEndPagador2.Lines.Text     := Logradouro + ' ' + Numero + ' ' + Complemento + ' - ' +
                                         Bairro + ', ' + Cidade + ' / ' + UF + ' - ' + CEP;
        txtCpfCnpjPagador2.Caption    := FormatarCNPJouCPF(CNPJCPF);
      end;

      with Titulo.Sacado.SacadoAvalista do
      begin
        case Pessoa of
           pFisica   : TipoDoc:= 'CPF: ';
           pJuridica : TipoDoc:= 'CNPJ: ';
        else
           TipoDoc := 'DOC.: ';
        end;

        if NomeAvalista <> '' then
        begin
          txtNomeSacadorAval2.Caption   := NomeAvalista + ' - ' + TipoDoc + ' ' + FormatarCNPJouCPF(CNPJCPF);
          txtEndSacadorAval2.Caption    := Logradouro + ' ' + Numero + ' ' + Complemento + ' - ' +
                                           Bairro + ', ' + Cidade + ' / ' + UF + ' - ' + CEP;
        end
        else
        begin
          txtNomeSacadorAval2.Caption   := '';
          txtEndSacadorAval2.Caption    := '';
        end;
      end;

      txtInstrucoes2.Lines.Text       := MensagemPadrao.Text;
      txtOrientacoesBanco.Lines.Text  := Banco.OrientacoesBanco.Text;

      rlBarraOrientbanco.Visible:= txtOrientacoesBanco.Lines.Count > 0;


      with Titulo.ACBrBoleto.Cedente do
      begin
        txtEndCedente.lines.text := Logradouro + ',' + NumeroRes + ' ' +
                                    IfThen(Complemento <> '',Complemento+' ') +
                                    Bairro + ', ' + Cidade + '/' + UF + ' ' +
                                    CEP + ' Fone: ' + Telefone;
      end;
   end;
end;

procedure TACBrBoletoFCFortesFr.RLBand2BeforePrint(Sender: TObject;
   var PrintIt: boolean);
Var
  CodBarras, LinhaDigitavel : String;
begin
  with fBoletoFC.ACBrBoleto do
  begin
     CodBarras      := Banco.MontarCodigoBarras( Titulo );
     LinhaDigitavel := Banco.MontarLinhaDigitavel( CodBarras,Titulo );

     imgBanco3.Picture.Assign(imgBanco2.Picture);
     fBoletoFC.CarregaLogo( imgBanco3.Picture, Banco.Numero );
     txtNumeroBanco3.Caption         := txtNumeroBanco2.Caption;
     txtLocalPagamento3.Lines.Text   := lblLocalPagto.Lines.Text;
     txtDataVencimento3.Caption      := txtDataVencimento2.Caption;
     txtNomeCedente3.Caption         := txtNomeCedente2.Caption;
     txtEndCedente1.Lines.Text       := txtEndCedente.Lines.Text;
     txtCodigoCedente3.Caption       := txtCodigoCedente2.Caption;
     txtDataDocumento3.Caption       := txtDataDocumento2.Caption;
     txtNumeroDocumento3.Caption     := txtNumeroDocumento2.Caption;
     txtEspecie3.Caption             := txtEspecie2.Caption;
     txtAceite3.Caption              := txtAceite2.Caption;
     txtDataProcessamento3.Caption   := txtDataProcessamento2.Caption;
     txtNossoNumero3.Caption         := txtNossoNumero2.Caption;
     txtUsoBanco3.Caption            := txtUsoBanco2.Caption;
     txtCarteira3.Caption            := txtCarteira2.Caption;
     txtEspecieDoc3.Caption          := txtEspecieDoc2.Caption;
     txtValorDocumento3.Caption      := txtValorDocumento2.Caption;
     txtNomePagador3.Caption         := txtNomePagador2.Caption;
     txtEndPagador3.lines.Text       := txtEndPagador2.Lines.Text;
     txtCpfCnpjPagador3.Caption      := txtCpfCnpjPagador2.Caption;
     txtNomeSacadorAval3.Caption     := txtNomeSacadorAval2.Caption;
     txtEndSacadorAval3.Caption      := txtEndSacadorAval2.Caption;
     imgCodigoBarra.AutoSize         := False;
     imgCodigoBarra.Width            := 432;
     imgCodigoBarra.Caption          := CodBarras;
     imgCodigoBarra.Margins.LeftMargin := 5;
     txtLinhaDigitavel.Caption       := LinhaDigitavel;
     txtInstrucoes3.Lines.Text       := txtInstrucoes2.Lines.Text;
     if not RLBandPix.Visible then
        printEMVPix(Titulo.QrCode.emv, imgQrCodePixLayoutBoleto);

   end;
end;

procedure TACBrBoletoFCFortesFr.RLBand3BeforePrint(Sender: TObject;
  var PrintIt: boolean);
Var
   NossoNum,LinhaDigitavel,CodBarras,CodCedente, Carteira, TipoDoc: String;
begin
   with fBoletoFC.ACBrBoleto do
   begin
      NossoNum       := Banco.MontarCampoNossoNumero( Titulo );
      CodBarras      := Banco.MontarCodigoBarras( Titulo );
      LinhaDigitavel := Banco.MontarLinhaDigitavel( CodBarras, Titulo );
      CodCedente     := Banco.MontarCampoCodigoCedente(Titulo);
      Carteira       := Banco.MontarCampoCarteira(Titulo);

      MensagemPadrao.Clear;
      MensagemPadrao.Text := Titulo.Mensagem.Text;
      ACBrBoletoFC.ACBrBoleto.AdicionarMensagensPadroes(Titulo,MensagemPadrao);

      case Cedente.TipoInscricao of
         pFisica   : TipoDoc:= 'CPF: ';
         pJuridica : TipoDoc:= 'CNPJ: ';
      else
         TipoDoc := 'DOC.: ';
      end;

      fBoletoFC.CarregaLogo( ImgLoja.Picture, Banco.Numero );
      fBoletoFC.CarregaLogo( imgBancoCarne.Picture, Banco.Numero );
      txtNumeroBanco.Caption         := IntToStrZero(Banco.Numero, 3)+ '-' +
                                        ifthen(Banco.Digito >= 10,'X',IntToStrZero(Banco.Digito, 1));
      txtVencCanhoto.Caption          := FormatDateTime('dd/mm/yyyy',Titulo.Vencimento);
      txtVencCarne2.Caption           := txtVencCanhoto.Caption;
      txtCodCedenteCarne.Caption      := CodCedente;
      txtCodCedenteCarne2.Caption     := txtCodCedenteCarne.Caption;
      txtValorCarne.Caption           := FormatFloatBr(Titulo.ValorDocumento,',0.00');
      txtValorCar.Caption             := txtValorCarne.Caption;
      txtNossoNumeroCarne.Caption     := NossoNum;
      txtNossoNumCan.Caption          := NossoNum;
      txtNumeroDocumentoCarne.Caption := Titulo.NumeroDocumento;
      txtNomeSacado.Caption           := Titulo.Sacado.NomeSacado;
      txtNomeSacadoCarne.Lines.Text   := 'Pagador: '+txtNomeSacado.Caption;

      txtLocal.Lines.Text             := Titulo.LocalPagamento;
      txtNomeCedente.Caption          := Cedente.Nome+ ' - '+TipoDoc + Cedente.CNPJCPF;
      txtEndCedenteCarne.Caption      := Cedente.Logradouro+' '+Cedente.NumeroRes+' '+Cedente.Complemento+' '+
                                         Cedente.Bairro+' '+Cedente.Cidade+' '+Cedente.UF+' '+Cedente.CEP;

      memoEndCedenteCarne.Lines.Clear;
      memoEndCedenteCarne.Lines.Add(ACBrStr('Beneficiário: ')+Cedente.Nome +' - '+Cedente.Logradouro+' '+Cedente.NumeroRes+' '+Cedente.Complemento+' '+
                                         Cedente.Bairro+' '+Cedente.Cidade+' '+Cedente.UF+' '+Cedente.CEP+' '+TipoDoc+ Cedente.CNPJCPF);

      txtDataDocto.Caption            := FormatDateTime('dd/mm/yyyy', Titulo.DataDocumento);
      txtNumeroDocto.Caption          := Titulo.NumeroDocumento;
      txtEspecieDoc.Caption           := Titulo.EspecieDoc;
      txtAceite.Caption               := fBoletoFC.DefineAceiteImpressao(Titulo);
      txtDataProces.Caption           := IfThen(Titulo.DataProcessamento = 0,
                                                FormatDateTime('dd/mm/yyyy',Now),
                                                FormatDateTime('dd/mm/yyyy',Titulo.DataProcessamento));

      txtUsoBanco.Caption             := Titulo.UsoBanco;
      txtCIP4.Caption                 := Banco.CIP;
      txtCarteira.Caption             := Carteira;
      txtEspecie2.Caption             := IfThen(trim(Titulo.EspecieMod) = '','R$',Titulo.EspecieMod);
      txtParcela.Caption              := IntToStrZero(Titulo.Parcela,3)+' /';
      txtTotPar.Caption               := IntToStrZero(Titulo.TotalParcelas,3);

      mmEndSacadoCarne.Lines.Text     := Titulo.Sacado.Logradouro +
                                         ',' + Titulo.Sacado.Numero +
                                         ', ' + Titulo.Sacado.Complemento +
                                         ', ' + Titulo.Sacado.Bairro +
                                         ', ' + Titulo.Sacado.Cidade +
                                         '- ' + Titulo.Sacado.UF +
                                         '- ' + Titulo.Sacado.CEP;
      txtCPF.Caption                  := 'CPF/CNPJ: '+ FormatarCNPJouCPF(Titulo.Sacado.CNPJCPF);
      txtCPFCarne2.Caption            := FormatarCNPJouCPF(Titulo.Sacado.CNPJCPF);
      mIntrucoes.Lines.Text           := MensagemPadrao.Text;

      txtLinhaDigitavelCarne.Caption := LinhaDigitavel;
      imgBarrasCarne.AutoSize         := False;
      imgBarrasCarne.Width            := 432;
      imgBarrasCarne.Caption := CodBarras;
      txtOrientacoesBancoCarne.Lines.Text:=Banco.OrientacoesBanco.Text;

      printEMVPix(Titulo.QrCode.emv, imgQRCodePixCarne);

      with Titulo.Sacado.SacadoAvalista do
      begin
        case Pessoa of
           pFisica   : TipoDoc:= 'CPF: ';
           pJuridica : TipoDoc:= 'CNPJ: ';
        else
           TipoDoc := 'DOC.: ';
        end;

        if (NomeAvalista <> '') then
        begin
          mmNomeSacadorAval4.Lines.Text := NomeAvalista + ' - ' + TipoDoc + ' ' + FormatarCNPJouCPF(CNPJCPF)+ ' ' +
            Logradouro + ' ' + Numero + ' ' + Complemento + ' - ' +
            Bairro + ', ' + Cidade + ' / ' + UF + ' - ' + CEP;
        end
        else
        begin
          mmNomeSacadorAval4.Lines.Clear;
        end;
      end;
   end;
end;

procedure TACBrBoletoFCFortesFr.RLBand4BeforePrint(Sender: TObject;
  var PrintIt: Boolean);
Var
   NossoNum,CodCedente,TipoDoc : String;
begin
   with fBoletoFC.ACBrBoleto do
   begin
      NossoNum    := Banco.MontarCampoNossoNumero( Titulo );
      CodCedente  := Banco.MontarCampoCodigoCedente(titulo);

      case Cedente.TipoInscricao of
         pFisica   : TipoDoc:= 'CPF: ';
         pJuridica : TipoDoc:= 'CNPJ: ';
      else
         TipoDoc := 'DOC.: ';
      end;

      fBoletoFC.CarregaLogo( imgBanco4.Picture, Banco.Numero );
      txtNumeroBanco4.Caption         := IntToStrZero(Banco.Numero, 3)+ '-' +
                                         IfThen(Banco.Digito >= 10,'X',
                                         IntToStrZero(Banco.Digito, 1));
      lblLocalPagto4.Caption           := Titulo.LocalPagamento;
      txtDataVencimento4.Caption      := FormatDateTime('dd/mm/yyyy', Titulo.Vencimento);
      txtNomeCedente4.Caption         := Cedente.Nome+ ' - '+TipoDoc + Cedente.CNPJCPF;
      txtCodigoCedente4.Caption       := CodCedente;

      txtNumeroDocumento4.Caption     := Titulo.NumeroDocumento;
      txtDataProcessamento4.Caption   := IfThen(Titulo.DataProcessamento = 0,
                                                FormatDateTime('dd/mm/yyyy',Now),
                                                FormatDateTime('dd/mm/yyyy',Titulo.DataProcessamento));
      txtNossoNumero4.Caption         := NossoNum;
      txtEspecie4.Caption             := 'R$';
      txtValorDocumento4.Caption      := FormatFloatBr(Titulo.ValorDocumento,',0.00');
      txtNomeSacado4.Caption          := Titulo.Sacado.NomeSacado;

      if ACBrUtil.FilesIO.FilesExists(Titulo.ArquivoLogoEmp) then
         imgLogoEmpresaBoleto.Picture.LoadFromFile(Titulo.ArquivoLogoEmp);


   end;
end;

procedure TACBrBoletoFCFortesFr.RLBand5BeforePrint(Sender: TObject; var PrintIt: Boolean);
Var
   // consulta tec 04/10/2017
   EnderecoCed, NossoNum,LinhaDigitavel,CodBarras,CodCedente,TipoDoc: String;
begin
   with fBoletoFC.ACBrBoleto do
   begin
      NossoNum       := Banco.MontarCampoNossoNumero( Titulo );
      CodBarras      := Banco.MontarCodigoBarras( Titulo );
      LinhaDigitavel := Banco.MontarLinhaDigitavel( CodBarras, Titulo );
      CodCedente     := Banco.MontarCampoCodigoCedente(Titulo);

      case Cedente.TipoInscricao of
         pFisica   : TipoDoc:= 'CPF: ';
         pJuridica : TipoDoc:= 'CNPJ: ';
      else
         TipoDoc := 'DOC.: ';
      end;

      MensagemPadrao.Clear;
      MensagemPadrao.Text := Titulo.Mensagem.Text;
      ACBrBoletoFC.ACBrBoleto.AdicionarMensagensPadroes(Titulo,MensagemPadrao);

      // topo do recibo
      fBoletoFC.CarregaLogo( imgBancoRecTop.Picture, Banco.Numero );
      txtNumeroBancoRecTop.Caption    := IntToStrZero(Banco.Numero, 3)+ '-' +
                                         ifthen(Banco.Digito >= 10,'X',IntToStrZero(Banco.Digito, 1));

      txtNomeCedenteRecTop.Caption    := Cedente.Nome+ ' - '+TipoDoc + Cedente.CNPJCPF;
      txtNomeSacadoRecTop.Caption     := Titulo.Sacado.NomeSacado;

      with Titulo.ACBrBoleto.Cedente do
      begin
         EnderecoCed := Logradouro+' '+NumeroRes+' '+Complemento+' '+
                        Bairro+' '+Cidade+' '+ UF+' '+CEP;
         {
         if(length(EnderecoCed) > 73) then
             txtEndCedenteRecTop1.Font.Size := 8
         else
             txtEndCedenteRecTop1.Font.Size := 9;
         }
         txtEndCedenteRecTop1.Font.Size := 7;
         txtEndCedenteRecTop1.Lines.Text   := EnderecoCed;
      end;

      txtCodigoCedenteRecTop.Caption  := CodCedente;
      txtNossoNumeroRecTop.Caption    := NossoNum;
      txtValorDocumentoRecTop.Caption := FormatFloatBr(Titulo.ValorDocumento,'###,###,##0.00');
      txtDataVencimentoRecTop.Caption := FormatDateTime('dd/mm/yyyy',Titulo.Vencimento);

      txtNumeroDocumentoRecTop.Caption:= Titulo.NumeroDocumento;
      txtEspecieRecTop.Caption        := Titulo.EspecieDoc;

      // corpo do boleto rectop
      fBoletoFC.CarregaLogo( imgBancoRecTop1.Picture, Banco.Numero );
      txtNumeroBancoRecTop1.Caption    := IntToStrZero(Banco.Numero, 3)+ '-' +
                                         ifthen(Banco.Digito >= 10,'X',IntToStrZero(Banco.Digito, 1));
      txtDataVencimentoRecTop1.Caption := FormatDateTime('dd/mm/yyyy',Titulo.Vencimento);
      txtCodigoCedenteRecTop1.Caption  := CodCedente;
      txtValorDocumentoRecTop1.Caption := FormatFloatBr(Titulo.ValorDocumento,'###,###,##0.00');
      txtNossoNumeroRecTop1.Caption    := NossoNum;
      txtNomeSacadoRecTop1.Caption     := Titulo.Sacado.NomeSacado;

      txtLocalPagamentoRecTop1.Lines.Text:= Titulo.LocalPagamento;
      txtNomeCedenteRecTop1.Caption    := Cedente.Nome+ ' - '+TipoDoc + Cedente.CNPJCPF;
      txtSacadorAvalistaRecTop1.Caption:= Titulo.Sacado.Avalista;

      txtDataDocumentoRecTop1.Caption  := FormatDateTime('dd/mm/yyyy', Titulo.DataDocumento);
      txtNumeroDocumentoRecTop1.Caption:= Titulo.NumeroDocumento;
      txtEspecieDocRecTop1.Caption     := Titulo.EspecieDoc;
      txtAceiteRecTop1.Caption         := fBoletoFC.DefineAceiteImpressao(Titulo);
      txtDataProcessamentoRecTop1.Caption := FormatDateTime('dd/mm/yyyy',Titulo.DataProcessamento);

      txtUsoBancoRecTop1.Caption       := Titulo.UsoBanco;
      txtCIP2.Caption                  := Banco.CIP;
      txtCarteiraRecTop1.Caption       := Titulo.Carteira + IfThen( (Banco.Numero = 1) and (length(trim(Cedente.Modalidade))>0) ,'/' + IntToStr(StrToIntDef(Cedente.Modalidade,0)) );
      txtEspecieRecTop1.Caption        := IfThen(trim(Titulo.EspecieMod) = '','R$',Titulo.EspecieMod);
      txtQuantidadeRecTop1.Caption     := '';
      txtValorMoedaRecTop1.Caption     := '';

      txtEnderecoSacadoRecTop1.Caption := Titulo.Sacado.Logradouro + ' '+
                                          Titulo.Sacado.Numero + ' ' + Titulo.Sacado.Complemento +
                                          ' ' + Titulo.Sacado.Bairro;
      txtCidadeSacadoRecTop1.Caption   := Titulo.Sacado.CEP + ' '+Titulo.Sacado.Cidade +
                                          ' '+Titulo.Sacado.UF;
      txtCpfCnpjSacadoRecTop1.Caption  := FormatarCNPJouCPF( Titulo.Sacado.CNPJCPF );
      mIntrucoesRecTop1.Lines.Text     := MensagemPadrao.Text;

      txtOrientacoesBancoRecTop1.Lines.Text  := Banco.OrientacoesBanco.Text;

      txtLinhaDigitavelRecTop.Caption  := LinhaDigitavel;
      txtLinhaDigitavelRecTopRecPag.Caption  := LinhaDigitavel;
      imgBarrasRecTop1.AutoSize        := False;
      imgBarrasRecTop1.Width           := 432;
      imgBarrasRecTop1.Caption         := CodBarras;
      imgBarrasRecTop1.Margins.LeftMargin := 5;

      printEMVPix(Titulo.QrCode.emv, imgQrCodePixReciboTopo);

   end;
end;

procedure TACBrBoletoFCFortesFr.RLBandPixBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
var
   EnderecoCed: String;
begin

  with fBoletoFC.ACBrBoleto do
  begin
    EnderecoCed := Cedente.Logradouro+' '+Cedente.NumeroRes+' '+Cedente.Complemento+'  '+
                    'CEP: '+Cedente.CEP+',  '+Cedente.Bairro+', '+Cedente.Cidade+' '+ Cedente.UF;

    txtCedentePix.Caption         := UpperCase( Cedente.Nome );
    txtCNPJCedentePix.Caption     := Cedente.CNPJCPF;
    txtEnderecoPIX.Caption        := EnderecoCed;
    txtValorPix.Caption           := FormatFloatBr(Titulo.ValorDocumento,',R$ 0.00');
    lblCopiaeCola.Visible := (Titulo.QrCode.emv <> '');
    lblCopiaeCola.Caption := Titulo.QrCode.emv;
    printEMVPix(Titulo.QrCode.emv, imgQRCodePix);

  end;
end;

procedure TACBrBoletoFCFortesFr.RLBand6BeforePrint(Sender: TObject;
  var PrintIt: Boolean);
var
  NossoNum, CodCedente, EnderecoCed, TipoDoc: string;
  CodBarras, LinhaDigitavel: string;
begin
  with fBoletoFC.ACBrBoleto do
  begin
    CodBarras       := Banco.MontarCodigoBarras(Titulo);
    LinhaDigitavel  := Banco.MontarLinhaDigitavel(CodBarras, Titulo);

    NossoNum        := Banco.MontarCampoNossoNumero(Titulo);
    CodCedente      := Banco.MontarCampoCodigoCedente(titulo);

    case Cedente.TipoInscricao of
      pFisica: TipoDoc    := 'CPF: ';
      pJuridica: TipoDoc  := 'CNPJ: ';
    else
      TipoDoc             := 'DOC.: ';
    end;

    MensagemPadrao.Clear;
    MensagemPadrao.Text       := Titulo.Mensagem.Text;
    ACBrBoletoFC.ACBrBoleto.AdicionarMensagensPadroes(Titulo, MensagemPadrao);

    fBoletoFC.CarregaLogo(imgBancoRecPagDet.Picture, Banco.Numero);
    txtNumeroBancoRecPagDet.Caption       := IntToStrZero(Banco.Numero, 3) + '-' + IfThen(Banco.Digito >= 10, 'X', IntToStrZero(Banco.Digito, 1));
    txtLocalPagamentoRecPagDet.Lines.Text := Titulo.LocalPagamento;
    txtDataVencimentoRecPagDet.Caption    := IfThen(Titulo.Vencimento > 0, FormatDateTime('dd/mm/yyyy', Titulo.Vencimento));
    txtNomeCedenteRecPagDet.Caption       := Cedente.Nome + ' - ' + TipoDoc + Cedente.CNPJCPF;
    with Titulo.ACBrBoleto.Cedente do
    begin
       EnderecoCed := Logradouro+' '+NumeroRes+' '+Complemento+'  '+
                      'CEP: '+CEP+',  '+Bairro+', '+Cidade+' '+ UF;

       if(length(EnderecoCed) > 73) then
           txtEndCedenteRecTop1.Font.Size := 8
       else
           txtEndCedenteRecTop1.Font.Size := 9;

       txtEndCedenteRecPagDet.Caption  := EnderecoCed;
    end;
    txtCodigoCedenteRecPagDet.Caption  := CodCedente;
    txtDataDocRecPagDet.Caption        := FormatDateTime('dd/mm/yyyy', Titulo.DataDocumento);
    txtNumeroDocRecPagDet.Caption      := Titulo.NumeroDocumento;
    txtEspecieDocRecPagDet.Caption     := Titulo.EspecieDoc;
    txtNossoNumeroRecPagDet.Caption    := NossoNum;
    txtCarteiraRecPagDet.Caption       := Titulo.Carteira;
    txtValorDocumentoRecPagDet.Caption := IfThen(Titulo.ValorDocumento > 0, FormatFloatBr( Titulo.ValorDocumento, '###,###,##0.00'));
    //Pagador
    txtNomePagadorRecPagDet.Caption    := Titulo.Sacado.NomeSacado + '  ' +
                                          IfThen(Titulo.Sacado.Pessoa = pJuridica, 'CNPJ: ', 'CPF: ')+ FormatarCNPJouCPF(Titulo.Sacado.CNPJCPF);
    txtEndPagadorRecPagDet.Caption     := Titulo.Sacado.Logradouro + ' ' + Titulo.Sacado.Numero + Titulo.Sacado.Complemento +
                                          '  CEP: '+Titulo.Sacado.CEP + ', ' + Titulo.Sacado.Bairro + ', ' + Titulo.Sacado.Cidade + ' ' + Titulo.Sacado.UF;
  end;
end;

procedure TACBrBoletoFCFortesFr.RLBand7BeforePrint(Sender: TObject; var PrintIt: Boolean);
Var
   NossoNum,CodCedente,TipoDoc, Carteira : String;
     CodBarras, LinhaDigitavel: string;
begin
   with fBoletoFC.ACBrBoleto do
   begin
      NossoNum    := Banco.MontarCampoNossoNumero( Titulo );
      CodCedente  := Banco.MontarCampoCodigoCedente(titulo);
      Carteira    := Banco.MontarCampoCarteira(Titulo);

      CodBarras       := Banco.MontarCodigoBarras(Titulo);
      LinhaDigitavel  := Banco.MontarLinhaDigitavel(CodBarras, Titulo);

      case Cedente.TipoInscricao of
         pFisica   : TipoDoc:= 'CPF: ';
         pJuridica : TipoDoc:= 'CNPJ: ';
      else
         TipoDoc := 'DOC.: ';
      end;

      lTertxtLinhaDigitavel.Caption   := LinhaDigitavel;
      lTertxtCodBarras.AutoSize       := false;
      lTertxtCodBarras.Width          := 641;
      lTertxtCodBarras.Caption        := CodBarras;

      MensagemPadrao.Clear;
      MensagemPadrao.Text := Titulo.Mensagem.Text;
      ACBrBoletoFC.ACBrBoleto.AdicionarMensagensPadroes(Titulo,MensagemPadrao);

      fBoletoFC.CarregaLogo( lTerImgBanco.Picture, Banco.Numero );
      fBoletoFC.CarregaLogo( lTerImgBanco2.Picture, Banco.Numero );
      lTertxtNumeroBanco.Caption         := IntToStrZero(Banco.Numero, 3)+ '-' +
                                         IfThen(Banco.Digito >= 10,'X',
                                         IntToStrZero(Banco.Digito, 1));
      lTerlblLocalPagto.Lines.Text       := Titulo.LocalPagamento;
      lTertxtDataVencimento.Caption      := IfThen(Titulo.Vencimento > 0,FormatDateTime('dd/mm/yyyy', Titulo.Vencimento));
      lTertxtNomeCedente.Caption         := Cedente.Nome+ ' - '+TipoDoc + Cedente.CNPJCPF;
      lTertxtCodCedente.Caption          := CodCedente;
      lTertxtDataDocumento.Caption       := FormatDateTime('dd/mm/yyyy', Titulo.DataDocumento);
      lTertxtNumeroDocumento.Caption     := Titulo.NumeroDocumento;
      lTertxtEspecieDoc.Caption          := Titulo.EspecieDoc;
      lTertxtAceite.Caption              := fBoletoFC.DefineAceiteImpressao(Titulo);
      lTertxtDataProcessamento.Caption   := IfThen(Titulo.DataProcessamento = 0,
                                                FormatDateTime('dd/mm/yyyy',Now),
                                                FormatDateTime('dd/mm/yyyy',Titulo.DataProcessamento));
      lTertxtNossoNumero.Caption      := NossoNum;
      lTertxtUsoBanco.Caption         := Titulo.UsoBanco;
      lTertxtCarteira.Caption         := Carteira;
      lTertxtEspecie.Caption          := IfThen(trim(Titulo.EspecieMod) = '','R$',Titulo.EspecieMod);
      ltertxtValorDocumento.Caption   := IfThen(Titulo.ValorDocumento > 0,FormatFloatBr(Titulo.ValorDocumento,',0.00'));

      with Titulo.Sacado do
      begin
        lTertxtNomePagador.Caption    := NomeSacado;
        lTertxtEndPagador.Caption     := Logradouro + ' ' + Numero + ' ' + Complemento + ' - ' +
                                         Bairro + ', ' + Cidade + ' / ' + UF + ' - ' + CEP;
        lTertxtCpfCnpjPagador.Caption := FormatarCNPJouCPF(CNPJCPF);
      end;

      with Titulo.Sacado.SacadoAvalista do
      begin
        case Pessoa of
           pFisica   : TipoDoc:= 'CPF: ';
           pJuridica : TipoDoc:= 'CNPJ: ';
        else
           TipoDoc := 'DOC.: ';
        end;

        if NomeAvalista <> '' then
        begin
          lTertxtNomeSacado.Caption   := NomeAvalista + ' - ' + TipoDoc + ' ' + FormatarCNPJouCPF(CNPJCPF);
//                                         +
//                                         Logradouro + ' ' + Numero + ' ' + Complemento + ' - ' +
//                                         Bairro + ', ' + Cidade + ' / ' + UF + ' - ' + CEP;
        end
        else
        begin
          lTertxtNomeSacado.Caption   := '';
        end;
      end;

      lTertxtInstrucoes.Lines.Text       := MensagemPadrao.Text;
      lTertxtOrientacoesBanco.Lines.Text := Banco.OrientacoesBanco.Text;

      lTertxtParcela.Caption             := IntToStrZero(Titulo.Parcela,3)+' /';
      lTertxtTotPar.Caption              := IntToStrZero(Titulo.TotalParcelas,3);

      lTertxtDataVencimento2.Caption     := lTertxtDataVencimento.Caption;
      lTertxtCodCedente2.Caption         := lTertxtCodCedente.Caption;
      ltertxtValorDocumento2.Caption     := lTertxtValorDocumento.Caption;
      lTertxtNumeroDocumento2.Caption    := lTertxtNumeroDocumento.Caption;
      lTertxtNossoNumero2.Caption        := lTertxtNossoNumero.Caption;
      lTerMemoCedente.Lines.Text         := lTertxtNomeCedente.Caption + ' ' + lTertxtEndCedente.Caption;
      lTertxtNomePagador2.Caption        := lTertxtNomePagador.Caption;
      lTertxtCpfCnpjPagador2.Caption      := lTertxtCpfCnpjPagador.Caption;



      with Titulo.ACBrBoleto.Cedente do
      begin
        lTertxtEndCedente.lines.text := Logradouro+' '+NumeroRes+' '+Complemento+' '+
                                  Bairro+' '+Cidade+' '+ UF+' '+CEP;
      end;
   end;
end;

procedure TACBrBoletoFCFortesFr.RLBand8BeforePrint(Sender: TObject;
  var PrintIt: Boolean);
var
  NossoNum, CodCedente, TipoDoc: string;
  CodBarras, LinhaDigitavel: string;
begin
  with fBoletoFC.ACBrBoleto do
  begin
    CodBarras       := Banco.MontarCodigoBarras(Titulo);
    LinhaDigitavel  := Banco.MontarLinhaDigitavel(CodBarras, Titulo);

    NossoNum        := Banco.MontarCampoNossoNumero(Titulo);
    CodCedente      := Banco.MontarCampoCodigoCedente(titulo);

    case Cedente.TipoInscricao of
      pFisica: TipoDoc    := 'CPF: ';
      pJuridica: TipoDoc  := 'CNPJ: ';
    else
      TipoDoc             := 'DOC.: ';
    end;

    MensagemPadrao.Clear;
    MensagemPadrao.Text       := Titulo.Mensagem.Text;
    ACBrBoletoFC.ACBrBoleto.AdicionarMensagensPadroes(Titulo, MensagemPadrao);

    fBoletoFC.CarregaLogo(imgBancoDet.Picture, Banco.Numero);
    txtNumeroBancoDet.Caption               := IntToStrZero(Banco.Numero, 3) + '-' + IfThen(Banco.Digito >= 10, 'X', IntToStrZero(Banco.Digito, 1));
    txtLocalPagamentoDet.Lines.Text         := Titulo.LocalPagamento;
    txtDataVencimentoDet.Caption            := IfThen(Titulo.Vencimento > 0, FormatDateTime('dd/mm/yyyy', Titulo.Vencimento));
    txtNomeCedenteDet.Caption               := Cedente.Nome + ' - ' + TipoDoc + Cedente.CNPJCPF;
    txtCodigoCedenteDet.Caption             := CodCedente;
    txtDataDocumentoDet.Caption             := FormatDateTime('dd/mm/yyyy', Titulo.DataDocumento);
    txtNumeroDocumentoDet.Caption           := Titulo.NumeroDocumento;
    txtEspecieDocDEt.Caption                := Titulo.EspecieDoc;
    txtAceiteDet.Caption                    := fBoletoFC.DefineAceiteImpressao(Titulo);
    txtDataProcessamentoDet.Caption         := IfThen(Titulo.DataProcessamento = 0,FormatDateTime('dd/mm/yyyy', Now),FormatDateTime('dd/mm/yyyy', Titulo.DataProcessamento));
    txtNossoNumeroDet.Caption               := NossoNum;
    txtUsoBancoDet.Caption                  := Titulo.UsoBanco;
    txtCIP3.Caption                         := Banco.CIP;
    txtCarteiraDet.Caption                  := Titulo.Carteira;
    txtEspecieDet.Caption                   := IfThen(trim(Titulo.EspecieMod) = '', 'R$', Titulo.EspecieMod);
    txtValorDocumentoDet.Caption            := IfThen(Titulo.ValorDocumento > 0, FormatFloatBr(Titulo.ValorDocumento, '###,###,##0.00'));
    txtNomeSacadoDet.Caption                := Titulo.Sacado.NomeSacado;
    txtEnderecoSacadoDet.Caption            := Titulo.Sacado.Logradouro + ' ' + Titulo.Sacado.Numero + Titulo.Sacado.Complemento;
    txtCidadeSacadoDet.Caption              := 'CEP: '+ Titulo.Sacado.CEP + ', ' + Titulo.Sacado.Bairro + ', ' + Titulo.Sacado.Cidade + ' ' + Titulo.Sacado.UF;
    txtCpfCnpjSacadoDet.Caption             := FormatarCNPJouCPF(Titulo.Sacado.CNPJCPF);

    txtInstrucoesDet.Lines.Text             := MensagemPadrao.Text;
    //txtOrientacoesBanco.Lines.Text        := Banco.OrientacoesBanco.Text;
    txtSacadorAvalistaDet.Caption           := Titulo.Sacado.Avalista;

    //rlBarraOrientbanco.Visible            := txtOrientacoesBanco.Lines.Count > 0;


    with Titulo.ACBrBoleto.Cedente do
    begin
      txtEndCedenteDet.Caption              := Logradouro + ' ' + NumeroRes + ' ' + Complemento + ' ' +
                                                'CEP: ' + CEP +', '+ Bairro + ', ' + Cidade + ' ' + UF;
    end;
    imgCodigoBarraDet.AutoSize              := False;
    imgCodigoBarraDet.Width                 := 432;
    imgCodigoBarraDet.Caption               := CodBarras;
    txtLinhaDigitavelDet.Caption            := LinhaDigitavel;

    printEMVPix(Titulo.QrCode.emv, imgQrCodePixFaturaDetail);
  end;
end;

procedure TACBrBoletoFCFortesFr.RLBandCarneA5TopoBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
Var
   NossoNum,CodCedente,TipoDoc, Carteira, CodBarras, LinhaDigitavel: String;
begin

   with fBoletoFC.ACBrBoleto do
   begin
      NossoNum    := Banco.MontarCampoNossoNumero( Titulo );
      CodCedente  := Banco.MontarCampoCodigoCedente(titulo);
      Carteira    := Banco.MontarCampoCarteira(Titulo);

      case Cedente.TipoInscricao of
         pFisica   : TipoDoc:= 'CPF: ';
         pJuridica : TipoDoc:= 'CNPJ: ';
      else
         TipoDoc := 'DOC.: ';
      end;

      CodBarras       := Banco.MontarCodigoBarras(Titulo);
      LinhaDigitavel  := Banco.MontarLinhaDigitavel(CodBarras, Titulo);


      linhaDigitavel1CA5.Caption   := LinhaDigitavel;
      linhaDigitavel1CA5.AutoSize       := False;

      linhaDigitavel2CA5.Caption   := LinhaDigitavel;
      linhaDigitavel2CA5.AutoSize       := False;

      txtBarCodeCA5.Width          := 432;
      txtBarCodeCA5.Caption        := CodBarras;


      MensagemPadrao.Clear;
      MensagemPadrao.Text := Titulo.Mensagem.Text;
      ACBrBoletoFC.ACBrBoleto.AdicionarMensagensPadroes(Titulo,MensagemPadrao);

      fBoletoFC.CarregaLogo( logoBanco1CA5.Picture, Banco.Numero );
      fBoletoFC.CarregaLogo( logoBanco2CA5.Picture, Banco.Numero );

      txtNumBanco1CA5.Caption         := IntToStrZero(Banco.Numero, 3)+ '-' +
                                         IfThen(Banco.Digito >= 10,'X',
                                         IntToStrZero(Banco.Digito, 1));
      txtNumBanco2CA5.Caption         := txtNumBanco1CA5.Caption;

      txtLocalPGCA5.Lines.Text        := Titulo.LocalPagamento;
      txtVenc1CA5.Caption             := IfThen(Titulo.Vencimento > 0,FormatDateTime('dd/mm/yyyy', Titulo.Vencimento));
      txtVenc2CA5.Caption             := IfThen(Titulo.Vencimento > 0,FormatDateTime('dd/mm/yyyy', Titulo.Vencimento));

      txtCedente1CA5.Caption          := Cedente.Nome+ ' - '+TipoDoc + Cedente.CNPJCPF;
      txtNomeCedente2CA5.Caption      := Cedente.Nome+ ' - '+TipoDoc + Cedente.CNPJCPF;
      with Titulo.ACBrBoleto.Cedente do
      begin
        txtEnderecoCedente2CA5.lines.text := Logradouro + ',' + NumeroRes + ' ' +
                                    IfThen(Complemento <> '',Complemento+' ') +
                                    Bairro + ', ' + Cidade + '/' + UF + ' ' +
                                    CEP + ' Fone: ' + Telefone;
        txtEnderecoCedente1CA5.lines.text := Logradouro + ',' + NumeroRes + ' ' + IfThen(Complemento <> '',Complemento+' ') +
                                    Bairro + ', ' + Cidade + '/' + UF + ' ' +
                                    CEP + ' Fone: ' + Telefone;
      end;


//      txtEspecDoc1CA5.Caption := Titulo.EspecieDoc;
      txtEspecDoc2CA5.Caption := Titulo.EspecieDoc;

      txtAgenciaCedente1CA5.Caption   := CodCedente;
      txtAgenciaCedente2CA5.Caption   := CodCedente;

//      txtDtDoc1CA5.Caption            := FormatDateTime('dd/mm/yyyy', Titulo.DataDocumento);
      txtDtDoc2CA5.Caption            := FormatDateTime('dd/mm/yyyy', Titulo.DataDocumento);

      txtNumDoc1CA5.Caption           := Titulo.NumeroDocumento;
      txtNunDoc2CA5.Caption           := Titulo.NumeroDocumento;

//      txtEspec1CA5.Caption            := Titulo.EspecieDoc;
      txtEspec2CA5.Caption            := Titulo.EspecieDoc;

      txtAceite2CA5.Caption           := ifThen(Titulo.Aceite = atSim,'S','N');



      txtNossNum1CA5.Caption          := NossoNum;
      txtNossoNum2CA5.Caption         := NossoNum;

//      txtUsoBanco1CA5.Caption         := Titulo.UsoBanco;
      txtUsoBanco2CA5.Caption         := Titulo.UsoBanco;

//      txtCarteira1CA5.Caption         := Carteira;
      txtCarteira2CA5.Caption         := Carteira;

//      txtEspec1CA5.Caption            := IfThen(trim(Titulo.EspecieMod) = '','R$',Titulo.EspecieMod);
      txtEspec2CA5.Caption            := IfThen(trim(Titulo.EspecieMod) = '','R$',Titulo.EspecieMod);

      txtValDoc1CA5.Caption           := IfThen(Titulo.ValorDocumento > 0,FormatFloatBr(Titulo.ValorDocumento, ',0.00'));
      txtValDoc2CA5.Caption           := IfThen(Titulo.ValorDocumento > 0,FormatFloatBr(Titulo.ValorDocumento, ',0.00'));

      txtCIP2CA5.Caption              := Banco.CIP;

//      txtParc1CA5.Caption             := IntToStrZero(Titulo.Parcela,3)+' /';
//      txtParcTotCA5.Caption              := IntToStrZero(Titulo.TotalParcelas,3);



      with Titulo.Sacado do
      begin
        txtNomePagador1CA5.Caption    := NomeSacado;
        txtNomePagador2CA5.Caption    := NomeSacado;


        txtEnderecoPagador1CA5.lines.Text        := Logradouro + ' ' + Numero + ' ' + Complemento + ' - ' +
                                         Bairro + ', ' + Cidade + ' / ' + UF + ' - ' + CEP;
        txtEnderecoPagador2CA5.Lines.Text        := Logradouro + ' ' + Numero + ' ' + Complemento + ' - ' +
                                         Bairro + ', ' + Cidade + ' / ' + UF + ' - ' + CEP;

        txtCPF1CA5.Caption    := FormatarCNPJouCPF(CNPJCPF);
        txtCPF2CA5.Caption    := FormatarCNPJouCPF(CNPJCPF);
      end;

      with Titulo.Sacado.SacadoAvalista do
      begin
        case Pessoa of
           pFisica   : TipoDoc:= 'CPF: ';
           pJuridica : TipoDoc:= 'CNPJ: ';
        else
           TipoDoc := 'DOC.: ';
        end;

        if NomeAvalista <> '' then
        begin
          txtBenefeciario2CA5.Caption   := NomeAvalista + ' - ' + TipoDoc + ' ' + FormatarCNPJouCPF(CNPJCPF);
          txtLogradouro2CA5.Caption    := Logradouro + ' ' + Numero + ' ' + Complemento + ' - ' +
                                           Bairro + ', ' + Cidade + ' / ' + UF + ' - ' + CEP;
        end
        else
        begin
          txtBenefeciario2CA5.Caption   := '';
          txtLogradouro2CA5.Caption    := '';
        end;
      end;

      txtLinhaDigitavel2CA5.Lines.Text       := MensagemPadrao.Text;
      //txtOrientacoesBanco.Lines.Text  := Banco.OrientacoesBanco.Text;

      rlBarraOrientbanco.Visible:= txtOrientacoesBanco.Lines.Count > 0;

      printEMVPix(Titulo.QrCode.emv, imgQrCodePixCarneA5);
   end;

end;

procedure TACBrBoletoFCFortesFr.RLBandDetalhamentoBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  txtDetalhamento.Lines.Text           := '';
  txtDetalhamento.Lines.Text           := Titulo.Detalhamento.Text;
end;

procedure TACBrBoletoFCFortesFr.RLBandReciboPagDetalBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
Var
   NossoNum,CodCedente,TipoDoc : String;
begin
   with fBoletoFC.ACBrBoleto do
   begin
      NossoNum    := Banco.MontarCampoNossoNumero( Titulo );
      CodCedente  := Banco.MontarCampoCodigoCedente(titulo);

      case Cedente.TipoInscricao of
         pFisica   : TipoDoc:= 'CPF: ';
         pJuridica : TipoDoc:= 'CNPJ: ';
      else
         TipoDoc := 'DOC.: ';
      end;

      Titulo.CarregaLogoEmp( imgBancoTopDet.Picture );

      //lblLocalPagtoTopDet.Caption          := Titulo.LocalPagamento;

      txtNomeCedenteTopDet.Caption         := Cedente.Nome+ ' - '+TipoDoc + Cedente.CNPJCPF;
      txtEnderecoCedenteTopDet.Caption     := Cedente.Logradouro + ' ' + Cedente.NumeroRes + Cedente.Complemento;
      txtCidadeCedenteTopDet.Caption       := 'CEP: '+ Cedente.CEP + ', ' + Cedente.Bairro + ', ' + Cedente.Cidade + ' - ' + Cedente.UF;

      txtNomeSacadoTopDet.Caption          := Titulo.Sacado.NomeSacado;
      txtEnderecoSacadoTopDet.Caption      := Titulo.Sacado.Logradouro + ' ' + Titulo.Sacado.Numero + Titulo.Sacado.Complemento;
      txtCidadeSacadoTopDet.Caption        := 'CEP: '+ Titulo.Sacado.CEP + ', ' + Titulo.Sacado.Bairro + ', ' + Titulo.Sacado.Cidade + ' - ' + Titulo.Sacado.UF;

      txtDataDocumentoTopDet.Caption       := FormatDateTime('dd/mm/yyyy', Titulo.DataDocumento);
      txtCompetenciaTopDet.Caption         := Titulo.Competencia;
      txtValorDocumentoTopDet.Caption      := FormatFloatBr(Titulo.ValorDocumento,'R$ ###,###,##0.00');
      txtDataVencimentoTopDet.Caption      := FormatDateTime('dd/mm/yyyy', Titulo.Vencimento);
      txtNumeroDocumentoTopDet.Caption     := Titulo.NumeroDocumento;
      txtCodigoBaixaTopDet.Caption         := Banco.MontarCampoNossoNumero(Titulo);

      rlVerso.visible := Titulo.Verso;
      if Titulo.Verso then
        verso;

   end;
end;

procedure TACBrBoletoFCFortesFr.RLBandServicosTopoBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
var
  NossoNum, CodCedente, EnderecoCed, TipoDoc: string;
  CodBarras, LinhaDigitavel: string;
begin
  with fBoletoFC.ACBrBoleto do
  begin
    CodBarras       := Banco.MontarCodigoBarras(Titulo);
    LinhaDigitavel  := Banco.MontarLinhaDigitavel(CodBarras, Titulo);

    NossoNum        := Banco.MontarCampoNossoNumero(Titulo);
    CodCedente      := Banco.MontarCampoCodigoCedente(titulo);

    case Cedente.TipoInscricao of
      pFisica: TipoDoc    := 'CPF: ';
      pJuridica: TipoDoc  := 'CNPJ: ';
    else
      TipoDoc             := 'DOC.: ';
    end;

    MensagemPadrao.Clear;
    MensagemPadrao.Text    := Titulo.Mensagem.Text;
    ACBrBoletoFC.ACBrBoleto.AdicionarMensagensPadroes(Titulo, MensagemPadrao);
    fBoletoFC.CarregaLogo(imgBancoTopServicos.Picture, Banco.Numero);

    //imgLogoEmpresaServicos.Picture.LoadFromFile(ExtractFilePath(Application.ExeName) + 'logo.jpg');

      if ACBrUtil.FilesIO.FilesExists(Titulo.ArquivoLogoEmp) then
         imgLogoEmpresaServicos.Picture.LoadFromFile(Titulo.ArquivoLogoEmp);


    //Titulo.CarregaLogoEmp( imgVersoBancoTopDet2.Picture );

    //Titulo.CarregaLogoEmp( imgLogoEmpresaServicos.Picture );

    txtNumeroBancoTopServicos.Caption       := IntToStrZero(Banco.Numero, 3) + '-' + IfThen(Banco.Digito >= 10, 'X', IntToStrZero(Banco.Digito, 1));
    txtLocalPagamentoTopServicos.Lines.Text := Titulo.LocalPagamento;
    txtDataVencimentoTopServicos.Caption    := IfThen(Titulo.Vencimento > 0, FormatDateTime('dd/mm/yyyy', Titulo.Vencimento));
    txtNomeCedenteTopServicos.Caption       := Cedente.Nome + ' - ' + TipoDoc + Cedente.CNPJCPF;
    with Titulo.ACBrBoleto.Cedente do
    begin
       EnderecoCed := Logradouro+' '+NumeroRes+' '+Complemento+'  '+
                      'CEP: '+CEP+',  '+Bairro+', '+Cidade+' '+ UF;

//       if(length(EnderecoCed) > 73) then
//           txtEnderecoCedenteTopServicos.Font.Size := 8
//       else
//           txtEnderecoCedenteTopServicos.Font.Size := 9;

       txtEnderecoCedenteTopServicos.lines.Text := EnderecoCed;
    end;
    txtCodigoCedenteTopServicos.Caption   := CodCedente;
    txtDataDocumentoTopServicos.Caption   := FormatDateTime('dd/mm/yyyy', Titulo.DataDocumento);
    txtNumeroDocumentoTopServicos.Caption := Titulo.NumeroDocumento;
    txtEspecieDocTopServicos.Caption      := Titulo.EspecieDoc;
    txtNossoNumeroTopServicos.Caption     := NossoNum;
    txtCarteiraTopServicos.Caption        := Titulo.Carteira;
    txtValorDocumentoTopServicos.Caption  := IfThen(Titulo.ValorDocumento > 0, FormatFloat('###,###,##0.00', Titulo.ValorDocumento));
    txtValorCobradoServicos.Caption       := '';
    txtMultaTopServicos.Caption           := '';
    txtDescontoTopServicos.Caption        := IfThen(Titulo.ValorDesconto > 0, FormatFloat('###,###,##0.00', Titulo.ValorDesconto));
    txtValorCobradoTopServicos.Caption    := '';

    txtNomeSacadoTopServicos.Caption      := Titulo.Sacado.NomeSacado + '  ' +
                                          IfThen(Titulo.Sacado.Pessoa = pJuridica, 'CNPJ: ', 'CPF: ')+ FormatarCNPJouCPF(Titulo.Sacado.CNPJCPF);

    txtEnderecoSacadoTopServicos.lines.Text := Titulo.Sacado.Logradouro + ' ' + Titulo.Sacado.Numero + Titulo.Sacado.Complemento +
                                               '  CEP: '+Titulo.Sacado.CEP + ', ' + Titulo.Sacado.Bairro + ', ' + Titulo.Sacado.Cidade + ' ' + Titulo.Sacado.UF;

    txtDataDocumentoTopServicos2.Caption    := FormatDateTime('dd/mm/yyyy', Titulo.DataDocumento);
    txtNumeroDocumentoTopServicos2.Caption  := Titulo.NumeroDocumento;
    txtDataProcessamentoTopServicos.Caption := IfThen(Titulo.DataProcessamento = 0,FormatDateTime('dd/mm/yyyy', Now),FormatDateTime('dd/mm/yyyy', Titulo.DataProcessamento));
    txtLinhaDigitavelTopServicos.Caption    := LinhaDigitavel;
    txtDetalhamentoServicos.Lines.Text      := '';
    txtDetalhamentoServicos.Lines.Text      := Titulo.Detalhamento.Text;

  end;

end;

procedure TACBrBoletoFCFortesFr.RLBandTituloServicosBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
var
  NossoNum, CodCedente, TipoDoc: string;
  CodBarras, LinhaDigitavel: string;
begin
  with fBoletoFC.ACBrBoleto do
  begin
    CodBarras       := Banco.MontarCodigoBarras(Titulo);
    LinhaDigitavel  := Banco.MontarLinhaDigitavel(CodBarras, Titulo);

    NossoNum        := Banco.MontarCampoNossoNumero(Titulo);
    CodCedente      := Banco.MontarCampoCodigoCedente(titulo);

    case Cedente.TipoInscricao of
      pFisica: TipoDoc    := 'CPF: ';
      pJuridica: TipoDoc  := 'CNPJ: ';
    else
      TipoDoc             := 'DOC.: ';
    end;

    MensagemPadrao.Clear;
    MensagemPadrao.Text       := Titulo.Mensagem.Text;
    ACBrBoletoFC.ACBrBoleto.AdicionarMensagensPadroes(Titulo, MensagemPadrao);

    fBoletoFC.CarregaLogo(imgBancoServicos.Picture, Banco.Numero);
    txtNumeroBancoServicos.Caption               := IntToStrZero(Banco.Numero, 3) + '-' + IfThen(Banco.Digito >= 10, 'X', IntToStrZero(Banco.Digito, 1));
    txtLocalPagamentoServicos.Lines.Text         := Titulo.LocalPagamento;
    txtDataVencimentoServicos.Caption            := IfThen(Titulo.Vencimento > 0, FormatDateTime('dd/mm/yyyy', Titulo.Vencimento));
    txtNomeCedenteServicos.Caption               := Cedente.Nome + ' - ' + TipoDoc + Cedente.CNPJCPF;
    txtEnderecoSacadoServicos.Caption            := Titulo.Sacado.Logradouro + ' ' + Titulo.Sacado.Numero + Titulo.Sacado.Complemento;
    txtCodigoCedenteServicos.Caption             := CodCedente;
    txtDataDocumentoServicos.Caption             := FormatDateTime('dd/mm/yyyy', Titulo.DataDocumento);
    txtNumeroDocumentoServicos.Caption           := Titulo.NumeroDocumento;
    txtEspecieDocServicos.Caption                := Titulo.EspecieDoc;
    txtAceiteServicos.Caption                    := fBoletoFC.DefineAceiteImpressao(Titulo);
    txtDataProcessamentoServicos.Caption         := IfThen(Titulo.DataProcessamento = 0,FormatDateTime('dd/mm/yyyy', Now),FormatDateTime('dd/mm/yyyy', Titulo.DataProcessamento));
    txtNossoNumeroServicos.Caption               := NossoNum;
    txtUsoBancoServicos.Caption                  := Titulo.UsoBanco;
    txtCarteiraServicos.Caption                  := Titulo.Carteira;
    txtEspecieServicos.Caption                   := IfThen(trim(Titulo.EspecieMod) = '', 'R$', Titulo.EspecieMod);
    txtValorDocumentoServicos.Caption            := IfThen(Titulo.ValorDocumento > 0, FormatFloat('###,###,##0.00', Titulo.ValorDocumento));
    txtNomeSacadoServicos.Caption                := Titulo.Sacado.NomeSacado;
    txtEnderecoSacadoServicos.Caption            := Titulo.Sacado.Logradouro + ' ' + Titulo.Sacado.Numero + Titulo.Sacado.Complemento;
    txtCidadeSacadoServicos.Caption              := 'CEP: '+ Titulo.Sacado.CEP + ', ' + Titulo.Sacado.Bairro + ', ' + Titulo.Sacado.Cidade + ' ' + Titulo.Sacado.UF;
    txtCpfCnpjSacadoServicos.Caption             := FormatarCNPJouCPF(Titulo.Sacado.CNPJCPF);
    txtInstrucoesServicos.Lines.Text             := MensagemPadrao.Text;
    if (Titulo.Instrucao1 <> '') then
      txtInstrucoesServicos.Lines.Add(Titulo.Instrucao1);
    if (Titulo.Instrucao2 <> '') then
      txtInstrucoesServicos.Lines.Add(Titulo.Instrucao2);
    if (Titulo.Instrucao3 <> '') then
      txtInstrucoesServicos.Lines.Add(Titulo.Instrucao3);
    //txtOrientacoesBanco.Lines.Text        := Banco.OrientacoesBanco.Text;
    //txtSacadorAvalistaDet.Caption           := Titulo.Sacado.Avalista;

    //rlBarraOrientbanco.Visible            := txtOrientacoesBanco.Lines.Count > 0;

    txtQuantidadeServicos.Caption := '';
    txtValorServicos.Caption      := '';

    with Titulo.ACBrBoleto.Cedente do
    begin
      txtEndCedenteDet.Caption              := Logradouro + ' ' + NumeroRes + ' ' + Complemento + ' ' +
                                                'CEP: ' + CEP +', '+ Bairro + ', ' + Cidade + ' ' + UF;
    end;
    imgCodigoBarraServicos.AutoSize              := False;
    imgCodigoBarraServicos.Width                 := 432;
    imgCodigoBarraServicos.Caption               := CodBarras;
    txtLinhaDigitavelServicos.Caption            := LinhaDigitavel;

    printEMVPix(Titulo.QrCode.emv, imgQrCodePixServicos);

  end;
end;

procedure TACBrBoletoFCFortesFr.rlbndComprovanteEntrega2BeforePrint(
  Sender: TObject; var PrintIt: Boolean);
Var
   NossoNum,CodCedente,TipoDoc,CodBarras,LinhaDigitavel : String;
begin
   with fBoletoFC.ACBrBoleto do
   begin
      NossoNum    := Banco.MontarCampoNossoNumero( Titulo );
      CodCedente  := Banco.MontarCampoCodigoCedente(titulo);

      CodBarras      := Banco.MontarCodigoBarras( Titulo );
      LinhaDigitavel := Banco.MontarLinhaDigitavel( CodBarras, Titulo ); 
      txtLinhaDigitavelComprovanteRec.Caption  := LinhaDigitavel;

      case Cedente.TipoInscricao of
         pFisica   : TipoDoc:= 'CPF: ';
         pJuridica : TipoDoc:= 'CNPJ: ';
      else
         TipoDoc := 'DOC.: ';
      end;

      fBoletoFC.CarregaLogo( imgBanco5.Picture, Banco.Numero );
      txtNumeroBanco5.Caption         := IntToStrZero(Banco.Numero, 3)+ '-' +
                                         IfThen(Banco.Digito >= 10,'X',
                                         IntToStrZero(Banco.Digito, 1));
      txtDataVencimento5.Caption      := FormatDateTime('dd/mm/yyyy', Titulo.Vencimento);
      rlmCedente5.Lines.Clear;
      rlmCedente5.Lines.Add(Cedente.Nome+ ' - '+TipoDoc + Cedente.CNPJCPF);
      rlmCedente5.Lines.Add(Cedente.Logradouro+' '+Cedente.NumeroRes+' - '+
                            Cedente.Complemento+' '+ Cedente.Bairro+', '+
                            Cedente.Cidade+' / '+ Cedente.UF+' - '+Cedente.CEP);
      txtCodigoCedente5.Caption       := CodCedente;

      txtNumeroDocumento5.Caption     := Titulo.NumeroDocumento;
      txtDataDocumento5.Caption       := FormatDateTime('dd/mm/yyyy', Titulo.DataDocumento);
      txtDataProcessamento5.Caption   := IfThen(Titulo.DataProcessamento = 0,
                                                FormatDateTime('dd/mm/yyyy',Now),
                                                FormatDateTime('dd/mm/yyyy',Titulo.DataProcessamento));
      txtNossoNumero5.Caption         := NossoNum;
      txtEspecie5.Caption             := 'R$';
      txtValorDocumento5.Caption      := FormatFloatBr(Titulo.ValorDocumento,',0.00');
      with Titulo.Sacado do
      begin
        if Length(CNPJCPF) > 11 then
          TipoDoc:= 'CNPJ: '
        else
          TipoDoc:= 'CPF: ';
        rlmPagador5.Lines.Clear;
        rlmPagador5.Lines.Add(NomeSacado + ' - ' + TipoDoc + FormatarCNPJouCPF(CNPJCPF));
        rlmPagador5.Lines.Add(Logradouro + ' ' + Numero + ' ' + Complemento + ' - ' +
                              Bairro + ', ' + Cidade + ' / ' + UF + ' - ' + CEP)
      end;
   end;
end;

procedure TACBrBoletoFCFortesFr.Verso;
begin
  with fBoletoFC.ACBrBoleto do
  begin
    // Dados do verso - Boleto com Detalhamento.

    Titulo.CarregaLogoEmp( imgVersoBancoTopDet.Picture );
    txtEmpNome.caption                 :=  Cedente.FantasiaCedente;

    Titulo.CarregaLogoEmp( imgVersoBancoTopDet2.Picture );
    txtEmpNome2.caption                 :=  Cedente.FantasiaCedente;

    txtDestinatarioNome.Caption         := Titulo.Sacado.NomeSacado;
    txtDestinatarioEndereco.Caption     := Titulo.Sacado.Logradouro + '  ' + Titulo.Sacado.Numero +'  '+ Titulo.Sacado.Complemento;
    txtDestinatariobairro.Caption       := Titulo.Sacado.Bairro ;
    txtDestinatarioCidade.Caption       := Titulo.Sacado.Cidade + ' ' + Titulo.Sacado.UF;
    txtDestinatarioCep.Caption          := Titulo.Sacado.CEP;

    txtRemententeNome.caption           :=  Cedente.FantasiaCedente+'  '+Cedente.Telefone;
    txtRemetenteEndereco.caption        :=  Cedente.Logradouro+' '+Cedente.NumeroRes+' '+Cedente.Complemento;
    txtRemetenteBairro.caption          :=  Cedente.Bairro;
    txtRemetenteCidade.caption          :=  Cedente.Cidade+' '+ Cedente.UF;
    txtRemetenteCep.caption             :=  Cedente.CEP;

    // Dados do verso - Boleto ...
  end;
end;

procedure TACBrBoletoFCFortesFr.printEMVPix(const AEMV : String; out ASender : TRLImage);
begin

  if not EstaVazio(Trim(AEMV)) then
  begin
    ASender.Visible := True;
    PintarQRCode( AEMV, ASender.Picture.Bitmap, qrAuto );
    ASender.BringToFront;
  end
  else
    ASender.Visible := False;
end;

procedure TACBrBoletoFCFortesFr.AjustarMargem(FReport: TRLReport; AConfig: TACBrBoletoFCClass);
begin
  // AJuste das Margens
  with FReport.Margins do
  begin
    TopMargin    := AConfig.MargemSuperior;
    BottomMargin := AConfig.MargemInferior;
    LeftMargin   := AConfig.MargemEsquerda;
    RightMargin  := AConfig.MargemDireita;
  end;
end;

procedure TACBrBoletoFCFortesFr.memoEndCedenteCarneAfterPrint(Sender: TObject);
begin

end;

{$ifdef FPC}
initialization
   {$I ACBrBoletoFCFortes.lrs}
{$endif}

end.

