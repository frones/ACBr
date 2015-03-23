{******************************************************************************}
{ Projeto: Componente ACBrNFe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Nota Fiscal}
{ eletrônica - NFe - http://www.nfe.fazenda.gov.br                             }
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
| Historico
|
| 04/04/2014: Rodrigo de Oliveira
|  - Doação units para geração do Danfe via QuickReport modo Paisagem
|
*******************************************************************************}

{$I ACBr.inc}        

unit ACBrNFeDANFeQRPaisagem;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, QuickRpt, QRCtrls,  XMLIntf, XMLDoc, 
  JPEG, ACBrNFeDANFeQR, ACBrNFeQRCodeBar, pcnConversao, DB, MidasLib,
  {$IFDEF QReport_PDF}
     QRPDFFilt,
  {$ENDIF}
  DBClient, ACBrNFeDANFEClass, ACBrNFeDANFeQRClass;

type

  TfqrDANFeQRPaisagem = class(TfqrDANFeQR)
    cdsItens: TClientDataSet;
    cdsItensCODIGO: TStringField;
    cdsItensDESCRICAO: TStringField;
    cdsItensNCM: TStringField;
    cdsItensCFOP: TStringField;
    cdsItensUNIDADE: TStringField;
    cdsItensQTDE: TStringField;
    cdsItensVALOR: TStringField;
    cdsItensTOTAL: TStringField;
    cdsItensCST: TStringField;
    cdsItensBICMS: TStringField;
    cdsItensALIQICMS: TStringField;
    cdsItensVALORICMS: TStringField;
    cdsItensALIQIPI: TStringField;
    cdsItensVALORIPI: TStringField;
    qrbRecibo: TQRBand;
    qrbDadosDanfe: TQRChildBand;
    qrbEmitenteDestinatario: TQRChildBand;
    qrbHeaderItens: TQRBand;
    qrbISSQN: TQRBand;
    qrbDadosAdicionais: TQRChildBand;
    QRShape1: TQRShape;
    QRLabel10: TQRLabel;
    QRShape3: TQRShape;
    QRShape2: TQRShape;
    QRShape101: TQRShape;
    qrlRecebemosDe1: TQRLabel;
    qrlNumNF0: TQRLabel;
    qrlSERIE0: TQRLabel;
    QRLabel14: TQRLabel;
    QRLabel13: TQRLabel;
    QRLabel12: TQRLabel;
    QRLabel11: TQRLabel;
    QRShape5: TQRShape;
    QRShape6: TQRShape;
    QRShape7: TQRShape;
    QRLabel17: TQRLabel;
    QRShape8: TQRShape;
    QRShape9: TQRShape;
    QRShape11: TQRShape;
    QRLabel29: TQRLabel;
    QRLabel30: TQRLabel;
    QRLabel31: TQRLabel;
    QRLabel32: TQRLabel;
    qrlNatOperacao: TQRLabel;
    qrlInscricaoEstadual: TQRLabel;
    qrlInscrEstSubst: TQRLabel;
    qrlCNPJ: TQRLabel;
    qrlFone: TQRLabel;
    qrmEmitente: TQRMemo;
    qrmEndereco: TQRMemo;
    qriLogo: TQRImage;
    qriBarCode: TQRImage;
    QRLabel2: TQRLabel;
    qrlChave: TQRLabel;
    qrlMsgAutorizado: TQRLabel;
    qrlDescricao: TQRLabel;
    qrlProtocolo: TQRLabel;
    qrlSERIE1: TQRLabel;
    QRLabel22: TQRLabel;
    QRLabel18: TQRLabel;
    QRLabel20: TQRLabel;
    qrlNumNF1: TQRLabel;
    QRLabel24: TQRLabel;
    qrlPageNumber: TQRLabel;
    QRLabel26: TQRLabel;
    QRLabel27: TQRLabel;
    QRShape102: TQRShape;
    qrlEntradaSaida: TQRLabel;
    QRShape12: TQRShape;
    QRShape15: TQRShape;
    QRShape16: TQRShape;
    QRShape17: TQRShape;
    QRShape13: TQRShape;
    QRShape21: TQRShape;
    QRShape22: TQRShape;
    qrshpFatura: TQRShape;
    QRShape25: TQRShape;
    QRShape26: TQRShape;
    QRShape27: TQRShape;
    QRShape28: TQRShape;
    QRShape29: TQRShape;
    QRShape30: TQRShape;
    QRShape31: TQRShape;
    QRShape32: TQRShape;
    QRShape33: TQRShape;
    QRShape34: TQRShape;
    QRShape35: TQRShape;
    QRShape36: TQRShape;
    QRShape37: TQRShape;
    QRShape38: TQRShape;
    QRShape39: TQRShape;
    QRShape42: TQRShape;
    QRShape43: TQRShape;
    QRShape45: TQRShape;
    QRShape46: TQRShape;
    QRShape47: TQRShape;
    QRShape48: TQRShape;
    QRShape49: TQRShape;
    QRShape50: TQRShape;
    QRLabel34: TQRLabel;
    QRLabel35: TQRLabel;
    QRLabel36: TQRLabel;
    QRLabel37: TQRLabel;
    QRLabel38: TQRLabel;
    QRLabel39: TQRLabel;
    QRLabel40: TQRLabel;
    QRLabel41: TQRLabel;
    QRLabel42: TQRLabel;
    QRLabel43: TQRLabel;
    QRLabel44: TQRLabel;
    QRLabel46: TQRLabel;
    QRLabel47: TQRLabel;
    QRLabel48: TQRLabel;
    QRLabel49: TQRLabel;
    QRLabel50: TQRLabel;
    QRLabel51: TQRLabel;
    QRLabel52: TQRLabel;
    QRLabel53: TQRLabel;
    QRLabel54: TQRLabel;
    QRLabel55: TQRLabel;
    QRLabel56: TQRLabel;
    QRLabel57: TQRLabel;
    QRLabel58: TQRLabel;
    QRLabel59: TQRLabel;
    QRLabel60: TQRLabel;
    QRLabel61: TQRLabel;
    QRLabel62: TQRLabel;
    QRLabel63: TQRLabel;
    QRLabel64: TQRLabel;
    QRLabel65: TQRLabel;
    QRLabel66: TQRLabel;
    QRLabel67: TQRLabel;
    QRLabel68: TQRLabel;
    QRLabel69: TQRLabel;
    QRLabel70: TQRLabel;
    QRLabel71: TQRLabel;
    QRLabel72: TQRLabel;
    qrlFatData1: TQRLabel;
    qrlFatData2: TQRLabel;
    qrlFatData3: TQRLabel;
    qrlFatValor1: TQRLabel;
    qrlFatValor2: TQRLabel;
    qrlFatValor3: TQRLabel;
    qrlFatNum1: TQRLabel;
    qrlFatNum2: TQRLabel;
    qrlFatNum3: TQRLabel;
    QRShape106: TQRShape;   
    QRShape107: TQRShape;
    QRShape108: TQRShape;
    QRShape109: TQRShape;
    qrlFatNum6: TQRLabel;
    qrlFatNum5: TQRLabel;
    qrlFatNum4: TQRLabel;
    qrlFatData4: TQRLabel;
    qrlFatData5: TQRLabel;
    qrlFatData6: TQRLabel;
    qrlFatValor6: TQRLabel;
    qrlFatValor5: TQRLabel;
    qrlFatValor4: TQRLabel;
    qrlFatNum9: TQRLabel;
    qrlFatNum8: TQRLabel;
    qrlFatNum7: TQRLabel;
    qrlFatData7: TQRLabel;
    qrlFatData8: TQRLabel;
    qrlFatData9: TQRLabel;
    qrlFatValor9: TQRLabel;
    qrlFatValor8: TQRLabel;
    qrlFatValor7: TQRLabel;
    qrlFatNum12: TQRLabel;
    qrlFatNum11: TQRLabel;
    qrlFatNum10: TQRLabel;
    qrlFatData10: TQRLabel;
    qrlFatData11: TQRLabel;
    qrlFatData12: TQRLabel;
    qrlFatValor12: TQRLabel;
    qrlFatValor11: TQRLabel;
    qrlFatValor10: TQRLabel;
    qrlFatNum15: TQRLabel;
    qrlFatNum14: TQRLabel;
    qrlFatNum13: TQRLabel;
    qrlFatData13: TQRLabel;
    qrlFatData14: TQRLabel;
    qrlFatData15: TQRLabel;
    qrlFatValor15: TQRLabel;
    qrlFatValor14: TQRLabel;
    qrlFatValor13: TQRLabel;
    qrlDestNome: TQRLabel;
    qrlDestCNPJ: TQRLabel;
    qrlDestEndereco: TQRLabel;
    qrlDestBairro: TQRLabel;
    qrlDestCEP: TQRLabel;
    qrlDestCidade: TQRLabel;
    qrlDestFone: TQRLabel;
    qrlDestUF: TQRLabel;
    qrlDestIE: TQRLabel;
    qrlEmissao: TQRLabel;
    qrlSaida: TQRLabel;
    qrlBaseICMS: TQRLabel;
    qrlValorICMS: TQRLabel;
    qrlBaseICMST: TQRLabel;
    qrlValorICMST: TQRLabel;
    qrlTotalProdutos: TQRLabel;
    qrlValorFrete: TQRLabel;
    qrlValorSeguro: TQRLabel;
    qrlDescontos: TQRLabel;
    qrlAcessorias: TQRLabel;
    qrlValorIPI: TQRLabel;
    qrlTotalNF: TQRLabel;
    qrlTransNome: TQRLabel;
    qrlTransModFrete: TQRLabel;
    qrlTransCodigoANTT: TQRLabel;
    qrlTransPlaca: TQRLabel;
    qrlTransUFPlaca: TQRLabel;
    qrlTransCNPJ: TQRLabel;
    qrlTransEndereco: TQRLabel;
    qrlTransCidade: TQRLabel;
    qrlTransUF: TQRLabel;
    qrlTransIE: TQRLabel;
    qrlTransQTDE: TQRLabel;
    qrlTransEspecie: TQRLabel;
    qrlTransMarca: TQRLabel;
    qrlTransNumeracao: TQRLabel;
    qrlTransPesoBruto: TQRLabel;
    qrlTransPesoLiq: TQRLabel;
    qrlHoraSaida: TQRLabel;
    qrsRectProdutos: TQRShape;
    QRLabel142: TQRLabel;
    QRLabel143: TQRLabel;
    QRLabel144: TQRLabel;
    lblCST: TQRLabel;
    QRLabel146: TQRLabel;
    QRLabel147: TQRLabel;
    QRLabel148: TQRLabel;
    QRLabel149: TQRLabel;
    QRLabel150: TQRLabel;
    QRLabel151: TQRLabel;
    QRLabel152: TQRLabel;
    QRLabel153: TQRLabel;
    QRLabel156: TQRLabel;
    QRLabel154: TQRLabel;
    QRLabel157: TQRLabel;
    QRShape10: TQRShape;
    QRShape24: TQRShape;
    QRShape40: TQRShape;
    QRShape41: TQRShape;
    QRShape57: TQRShape;
    QRShape58: TQRShape;
    QRShape59: TQRShape;
    QRShape60: TQRShape;
    QRShape61: TQRShape;
    QRShape62: TQRShape;
    QRShape63: TQRShape;
    QRShape64: TQRShape;
    QRShape65: TQRShape;
    QRLabel3: TQRLabel;
    QRShape52: TQRShape;
    QRShape53: TQRShape;
    QRShape54: TQRShape;
    QRShape55: TQRShape;
    QRLabel137: TQRLabel;
    QRLabel138: TQRLabel;
    QRLabel139: TQRLabel;
    QRLabel140: TQRLabel;
    qrlInscMunicipal: TQRLabel;
    qrlTotalServicos: TQRLabel;
    qrlBaseISSQN: TQRLabel;
    qrlValorISSQN: TQRLabel;
    QRShape56: TQRShape;
    QRLabel100: TQRLabel;
    qrmDadosAdicionais: TQRMemo;
    rbDadosAdicionais: TQRShape;
    QRShape67: TQRShape;
    QRLabel6: TQRLabel;
    qrbItens: TQRBand;
    qrmProdutoCodigo: TQRDBText;
    qrmProdutoDescricao: TQRDBText;
    qrmProdutoNCM: TQRDBText;
    qrmProdutoCST: TQRDBText;
    qrmProdutoCFOP: TQRDBText;
    qrmProdutoUnidade: TQRDBText;
    qrmProdutoQTDE: TQRDBText;
    qrmProdutoValor: TQRDBText;
    qrmProdutoTotal: TQRDBText;
    qrmProdutoBCICMS: TQRDBText;
    qrmProdutoVALORICMS: TQRDBText;
    qrmProdutoALIQICMS: TQRDBText;
    qrmProdutoVALORIPI: TQRDBText;
    qrmProdutoALIQIPI: TQRDBText;
    QRShape4: TQRShape;
    QRShape66: TQRShape;
    qriBarCodeContingencia: TQRImage;
    qrbItensLine: TQRChildBand;
    QRShape68: TQRShape;
    QRShape69: TQRShape;
    qrlDataHoraImpressao: TQRLabel;
    qrlSistema: TQRLabel;
    qrs2: TQRShape;
    cdsItensXPROD: TStringField;
    cdsItensINFADIPROD: TStringField;
    qrs1: TQRShape;
    qrs3: TQRShape;
    qrs4: TQRShape;
    qrs5: TQRShape;
    qrs6: TQRShape;
    qrs7: TQRShape;
    qrs8: TQRShape;
    qrs9: TQRShape;
    qrs10: TQRShape;
    qrs11: TQRShape;
    qrs12: TQRShape;
    qrs13: TQRShape;
    qrs14: TQRShape;
    qrs15: TQRShape;
    cdsItensCSOSN: TStringField;
    qrlResumo: TQRLabel;
    QRShape70: TQRShape;
    QRShape71: TQRShape;
    QRShape14: TQRShape;
    qrlMsgTeste: TQRLabel;
    QRShape18: TQRShape;
    QRShape19: TQRShape;
    QRShape20: TQRShape;
    QRLabel8: TQRLabel;
    QRLabel9: TQRLabel;
    qrmProdutoBCICMSST: TQRDBText;
    qrmProdutoVALORICMSST: TQRDBText;
    cdsItensBICMSST: TStringField;
    cdsItensVALORICMSST: TStringField;
    QRShape74: TQRShape;
    cdsItensITEM: TIntegerField;
    QRShape23: TQRShape;
    QRLabel1: TQRLabel;
    QRDBText1: TQRDBText;
    QRShape44: TQRShape;
    QRShape51: TQRShape;
    QRShape72: TQRShape;
    QRShape73: TQRShape;
    QRShape75: TQRShape;
    QRLabel4: TQRLabel;
    QRLabel5: TQRLabel;
    QRDBText2: TQRDBText;
    cdsItensALIQPIS: TStringField;
    cdsItensALIQCOFINS: TStringField;
    QRDBText3: TQRDBText;
    QRShape76: TQRShape;
    QRShape77: TQRShape;
    cdsItensVALORPIS: TStringField;
    cdsItensVALORCOFINS: TStringField;
    cdsItensBPIS: TStringField;
    cdsItensBCOFINS: TStringField;
    QRShape78: TQRShape;
    QRLabel7: TQRLabel;
    QRLabel15: TQRLabel;
    QRShape79: TQRShape;
    QRLabel16: TQRLabel;
    QRLabel19: TQRLabel;
    QRShape80: TQRShape;
    QRDBText4: TQRDBText;
    QRDBText5: TQRDBText;
    QRShape81: TQRShape;
    QRShape82: TQRShape;
    QRShape83: TQRShape;
    QRLabel21: TQRLabel;
    QRLabel23: TQRLabel;
    QRDBText6: TQRDBText;
    QRDBText7: TQRDBText;
    QRShape84: TQRShape;
    procedure QRNFeBeforePrint(Sender: TCustomQuickRep;
      var PrintReport: Boolean);
    procedure qrbReciboBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrbEmitenteDestinatarioBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrbISSQNBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrbDadosAdicionaisBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrbDadosDanfeBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrbItensBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrmProdutoDescricaoPrint(sender: TObject; var Value: string);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FTotalPages : integer;
    procedure Itens;
  public
    { Public declarations }
    procedure ProtocoloNFE( const sProtocolo : String );
    procedure QRAngledLabelText_Print(sender: TObject; var Value: String);
  end;


implementation

uses
 StrUtils, DateUtils,
 ACBrUtil, ACBrDFeUtil, pcnNFe;

{$R *.dfm}

const
   _NUM_ITEMS_PAGE1      = 22;
   _NUM_ITEMS_OTHERPAGES = 46;

var
   FProtocoloNFE : String;
   nItemControle : Integer;

procedure TfqrDANFeQRPaisagem.Itens;
var
  nItem : Integer ;
  sCST, sBCICMS, sALIQICMS, sVALORICMS, sALIQIPI, sVALORIPI, vBICMSST, vVALORICMS : String ;
  sPIS_BC, sPIS_ALIQ, sPIS_VLR, sCOFINS_BC, sCOFINS_ALIQ, sCOFINS_VLR : string;
begin
  cdsItens.Close;
  cdsItens.CreateDataSet ;
  cdsItens.Open ;

  for nItem := 0 to (FNFe.Det.Count - 1) do
    begin
      with FNFe.Det.Items[nItem] do
        begin
          sPIS_BC     := FormatFloat('###,###,###,##0.00', Imposto.PIS.vBC );
          sPIS_ALIQ   := FormatFloat('###,###,###,##0.00', Imposto.PIS.pPIS );
          sPIS_VLR    := FormatFloat('###,###,###,##0.00', Imposto.PIS.vPIS );
          sCOFINS_BC  := FormatFloat('###,###,###,##0.00', Imposto.COFINS.vBC );
          sCOFINS_ALIQ:= FormatFloat('###,###,###,##0.00', Imposto.COFINS.pCOFINS );
          sCOFINS_VLR := FormatFloat('###,###,###,##0.00', Imposto.COFINS.vCOFINS );

          with Prod do
            begin
              with Imposto.ICMS do
                begin
                  sALIQIPI   := '0,00' ;
                  sVALORIPI  := '0,00' ;

                  cdsItens.Append ;
                  cdsItens.FieldByName('ITEM').AsInteger := nItem;
                  cdsItens.FieldByName('CODIGO').AsString := CProd;
                  cdsItens.FieldByName('DESCRICAO').AsString := XProd+
                                                                IfThen(Trim(cEAN)='','',' (EAN: '+cEAN+')');
                  cdsItens.FieldByName('INFADIPROD').AsString := infAdProd;
                  cdsItens.FieldByName('NCM').AsString := NCM;
                  cdsItens.FieldByName('CFOP').AsString := CFOP;

                  case FCasasDecimaisqCom of
                    0: cdsItens.FieldByName('QTDE').AsString := FormatFloat('###,###,###,##0', QCom);
                    1: cdsItens.FieldByName('QTDE').AsString := FormatFloat('###,###,###,##0.0', QCom);
                    2: cdsItens.FieldByName('QTDE').AsString := FormatFloat('###,###,###,##0.00', QCom);
                    3: cdsItens.FieldByName('QTDE').AsString := FormatFloat('###,###,###,##0.000', QCom);
                    4: cdsItens.FieldByName('QTDE').AsString := FormatFloat('###,###,###,##0.0000', QCom);
                    5: cdsItens.FieldByName('QTDE').AsString := FormatFloat('###,###,###,##0.00000', QCom);
                    6: cdsItens.FieldByName('QTDE').AsString := FormatFloat('###,###,###,##0.000000', QCom);
                    7: cdsItens.FieldByName('QTDE').AsString := FormatFloat('###,###,###,##0.0000000', QCom);
                    8: cdsItens.FieldByName('QTDE').AsString := FormatFloat('###,###,###,##0.00000000', QCom);
                    9: cdsItens.FieldByName('QTDE').AsString := FormatFloat('###,###,###,##0.000000000', QCom);
                   10: cdsItens.FieldByName('QTDE').AsString := FormatFloat('###,###,###,##0.0000000000', QCom);
                  end;

                  case FCasasDecimaisvUnCom of
                    0: cdsItens.FieldByName('VALOR').AsString := FormatFloat('###,###,###,##0', vUnCom);
                    1: cdsItens.FieldByName('VALOR').AsString := FormatFloat('###,###,###,##0.0', vUnCom);
                    2: cdsItens.FieldByName('VALOR').AsString := FormatFloat('###,###,###,##0.00', vUnCom);
                    3: cdsItens.FieldByName('VALOR').AsString := FormatFloat('###,###,###,##0.000', vUnCom);
                    4: cdsItens.FieldByName('VALOR').AsString := FormatFloat('###,###,###,##0.0000', vUnCom);
                    5: cdsItens.FieldByName('VALOR').AsString := FormatFloat('###,###,###,##0.00000', vUnCom);
                    6: cdsItens.FieldByName('VALOR').AsString := FormatFloat('###,###,###,##0.000000', vUnCom);
                    7: cdsItens.FieldByName('VALOR').AsString := FormatFloat('###,###,###,##0.0000000', vUnCom);
                    8: cdsItens.FieldByName('VALOR').AsString := FormatFloat('###,###,###,##0.00000000', vUnCom);
                    9: cdsItens.FieldByName('VALOR').AsString := FormatFloat('###,###,###,##0.000000000', vUnCom);
                   10: cdsItens.FieldByName('VALOR').AsString := FormatFloat('###,###,###,##0.0000000000', vUnCom);
                  end;

                  cdsItens.FieldByName('UNIDADE').AsString := UCom;
                  cdsItens.FieldByName('TOTAL').AsString :=
                                      FormatFloat('###,###,###,##0.00', vProd);
                  //==============================================================================
                  // Em contato com o pessoal da Receita Estadual, foi informado que Ambos os regimes
                  // trabalham de mesma forma, deferenciando-se apensa em seus códigos
                  //==============================================================================
                  if FNFe.Emit.CRT in [crtRegimeNormal, crtSimplesExcessoReceita] then
                    begin
                      if CSTICMSToStr(CST) > '' then
                        sCST := OrigToStr(orig) + CSTICMSToStr(CST)
                      else
                        sCST := '';
                      sBCICMS     := '0,00';
                      sALIQICMS   := '0,00';
                      sVALORICMS  := '0,00';
                      vBICMSST    := '0,00';
                      vVALORICMS  := '0,00';

                      if (CST = cst00) then
                        begin
                          sBCICMS    := FormatFloat('###,###,###,##0.00', VBC);
                          sALIQICMS  := FormatFloat('###,###,###,##0.00', PICMS);
                          sVALORICMS := FormatFloat('###,###,###,##0.00', VICMS);
                        end
                      else if (CST = cst10) then
                        begin
                          sBCICMS    := FormatFloat('###,###,###,##0.00', VBC);
                          sALIQICMS  := FormatFloat('###,###,###,##0.00', PICMS);
                          sVALORICMS := FormatFloat('###,###,###,##0.00', VICMS);
                          vBICMSST   := FormatFloat('###,###,###,##0.00', vBCST);
                          vVALORICMS := FormatFloat('###,###,###,##0.00', vICMSST);
                        end
                      else if (CST = cst20) then
                        begin
                          sBCICMS    := FormatFloat('###,###,###,##0.00', VBC);
                          sALIQICMS  := FormatFloat('###,###,###,##0.00', PICMS);
                          sVALORICMS := FormatFloat('###,###,###,##0.00', VICMS);
                        end
                      else if (CST = cst30) then
                        begin
                          sBCICMS    := FormatFloat('###,###,###,##0.00', VBCST);
                          sALIQICMS  := FormatFloat('###,###,###,##0.00', PICMSST);
                          sVALORICMS := FormatFloat('###,###,###,##0.00', VICMSST);
                        end
                      else if (CST = cst40) or (CST = cst41) or (CST = cst50) then
                        begin
                          // Campos vazios
                        end
                      else if (CST = cst51) then
                        begin
                          sBCICMS    := FormatFloat('###,###,###,##0.00', VBC);
                          sALIQICMS  := FormatFloat('###,###,###,##0.00', PICMS);
                          sVALORICMS := FormatFloat('###,###,###,##0.00', VICMS);
                        end
                      else if (CST = cst60) then
                        begin
                          sBCICMS    := FormatFloat('###,###,###,##0.00', VBCST);
                          sVALORICMS := FormatFloat('###,###,###,##0.00', VICMSST);
                        end
                      else if (CST = cst70) then
                        begin
                          sBCICMS    := FormatFloat('###,###,###,##0.00', VBC);
                          sALIQICMS  := FormatFloat('###,###,###,##0.00', PICMS);
                          sVALORICMS := FormatFloat('###,###,###,##0.00', VICMS);
                          vBICMSST   := FormatFloat('###,###,###,##0.00', vBCST);
                          vVALORICMS := FormatFloat('###,###,###,##0.00', vICMSST);
                        end
                      else if (CST = cst90) then
                        begin
                          sBCICMS    := FormatFloat('###,###,###,##0.00', VBC);
                          sALIQICMS  := FormatFloat('###,###,###,##0.00', PICMS);
                          vBICMSST   := FormatFloat('###,###,###,##0.00', vBCST);
                          sVALORICMS := FormatFloat('###,###,###,##0.00', VICMS);
                       end;

                      cdsItens.FieldByName('CST').AsString := sCST;
                      cdsItens.FieldByName('BICMS').AsString := sBCICMS;
                      cdsItens.FieldByName('ALIQICMS').AsString := sALIQICMS;
                      cdsItens.FieldByName('VALORICMS').AsString := sVALORICMS;
                      cdsItens.FieldByName('BICMSST').AsString := vBICMSST;
                      cdsItens.FieldByName('VALORICMSST').AsString := vVALORICMS;

                      lblCST.Caption := 'CST';
//                      lblCST.Font.Size := 5;
//                      lblCST.Top := 20;
                      qrmProdutoCST.DataField := 'CST';
                    end; //FNFe.Emit.CRT = crtRegimeNormal

                  if FNFe.Emit.CRT = crtSimplesNacional then
                    begin
                        //==============================================================================
                        // Adicionado para imprimir alíquotas
                        //==============================================================================
                        if CSOSNIcmsToStr(Imposto.ICMS.CSOSN) > '' then
                           cdsItens.FieldByName('CSOSN').AsString := OrigToStr(orig) + CSOSNIcmsToStr(Imposto.ICMS.CSOSN)
                        else
                           cdsItens.FieldByName('CSOSN').AsString := '';

                        //==============================================================================
                        // Resetando valores das qlíquotas
                        //==============================================================================
                        sBCICMS    := '0,00';
                        sALIQICMS  := '0,00';
                        sVALORICMS := '0,00';

                        case CSOSN of
                           csosn900:
                           begin
                              sBCICMS    := FormatFloat('#,##0.00', VBC);
                              sALIQICMS  := FormatFloat('#,##0.00', PICMS);
                              sVALORICMS := FormatFloat('#,##0.00', VICMS);
                           end;
                        end;

                        cdsItens.FieldByName('BICMS').AsString       := sBCICMS;
                        cdsItens.FieldByName('ALIQICMS').AsString    := sALIQICMS;
                        cdsItens.FieldByName('VALORICMS').AsString   := sVALORICMS;
                        lblCST.Caption          := 'CSOSN';
//                        lblCST.Font.Size        := 4;
//                        lblCST.Top              := 22;
                        qrmProdutoCST.DataField := 'CSOSN';
                    end; //FNFe.Emit.CRT = crtSimplesNacional
                end; // with Imposto.ICMS do

               with Imposto.IPI do
                begin
                  if (CST = ipi00) or (CST = ipi49) or
                     (CST = ipi50) or (CST = ipi99) then
                    begin
                      sALIQIPI  := FormatFloat('##0', PIPI) ;
                      sVALORIPI := FormatFloat('##0.00', VIPI) ;
                    end
                end;

              cdsItens.FieldByName('ALIQIPI').AsString := sALIQIPI;
              cdsItens.FieldByName('VALORIPI').AsString := sVALORIPI;

              cdsItens.FieldByName('ALIQPIS').AsString := sPIS_ALIQ;
              cdsItens.FieldByName('ALIQCOFINS').AsString := sCOFINS_ALIQ;
              cdsItens.FieldByName('VALORPIS').AsString := sPIS_VLR;
              cdsItens.FieldByName('VALORCOFINS').AsString := sCOFINS_VLR;
              cdsItens.FieldByName('BPIS').AsString := sPIS_BC;
              cdsItens.FieldByName('BCOFINS').AsString := sCOFINS_BC;

              cdsItens.Post ;
            end; // with Prod do
        end; //  with FNFe.Det.Items[nItem] do
    end; //  for nItem := 0 to ( FNFe.Det.Count - 1 ) do

   cdsItens.First ;
end;

procedure TfqrDANFeQRPaisagem.ProtocoloNFE( const sProtocolo : String );
begin
   FProtocoloNFE := sProtocolo;
end;

procedure TfqrDANFeQRPaisagem.QRNFeBeforePrint(Sender: TCustomQuickRep;
  var PrintReport: Boolean);
var
  nRestItens : Integer;
begin
  inherited;
   //Alteracao infoaxel 01/09/2010
   qrbISSQN.Height := 46;
   qrbDadosAdicionais.Height := 115;
   //Fim alteracao infoaxel 01/09/2010
   Itens;
   nItemControle := 0;

   FTotalPages   := 1;
   if ( FNFe.Det.Count > _NUM_ITEMS_PAGE1 ) then
   begin
      nRestItens := FNFe.Det.Count - _NUM_ITEMS_PAGE1;
      if nRestItens <= _NUM_ITEMS_OTHERPAGES then
         Inc( FTotalPages )
      else
      begin
         Inc( FTotalPages, nRestItens div _NUM_ITEMS_OTHERPAGES );
         if ( nRestItens mod _NUM_ITEMS_OTHERPAGES ) > 0 then
            Inc( FTotalPages )
      end;
   end;

   // A linha abaixo foi incluida por: Italo Jurisato Junior
   // 03/08/2010
   // Algumas impressoras não imprimem se a propriedade Report Title estive em
   // branco.
   QRNFe.ReportTitle:='NF-e: ' + FormatFloat( '000,000,000', FNFe.Ide.nNF );

   QRNFe.Page.TopMargin    := FMargemSuperior * 100;
   QRNFe.Page.BottomMargin := FMargemInferior * 100;
   QRNFe.Page.LeftMargin   := FMargemEsquerda * 100;
   QRNFe.Page.RightMargin  := FMargemDireita  * 100;
end;

procedure TfqrDANFeQRPaisagem.qrbReciboBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
begin

  if FResumoCanhoto = True then
    begin
      if FResumoCanhoto_Texto <> '' then
        qrlResumo.Caption := FResumoCanhoto_Texto
      else
        begin
          qrlResumo.Caption := 'EMISSÃO: ' +
                           FormatDateTime('DD/MM/YYYY', FNFe.Ide.dEmi) +
                           '  -  ' +
                           'DEST. / REM.: ' + FNFe.Dest.xNome + '  -  ' +
                           'VALOR TOTAL: R$ ' +
                           FormatFloat(FNFe.Total.ICMSTot.vNF,
                           '###,###,###,##0.00');
        end; // if FResumoCanhoto_Texto <> ''
    end // if FResumoCanhoto = True
  else
    qrlResumo.Caption := '';

  inherited;
   PrintBand := QRNFe.PageNumber = 1;
   qrlNumNF0.Caption := FormatFloat( '000,000,000', FNFe.Ide.nNF );
   qrlSERIE0.Caption := IntToStr( FNFe.Ide.serie );
   qrlRecebemosDe1.Caption      := StringReplace(qrlRecebemosDe1.Caption,'%s',FNFe.Emit.xNome,[rfReplaceAll]);
end;

procedure TfqrDANFeQRPaisagem.qrmProdutoDescricaoPrint(sender: TObject;
  var Value: string);
var
    intTamanhoDescricao,
    intTamanhoAdicional,
    intTamanhoLinha,
    intDivisaoDescricao, intDivisaoAdicional,
    intResto   :Integer;
begin
  inherited;
    intTamanhoDescricao:= Length(cdsItens.FieldByName( 'DESCRICAO' ).AsString);
    intDivisaoAdicional:=0;
    if Length(cdsItens.FieldByName( 'INFADIPROD' ).AsString)>0 then begin
       intTamanhoAdicional:=Length('InfAdic: '+cdsItens.FieldByName( 'INFADIPROD' ).AsString);

       intDivisaoAdicional := intTamanhoAdicional DIV 35;

       intResto:=intTamanhoAdicional - (intTamanhoAdicional DIV 35)*35;
       if intResto>0 then begin
          intDivisaoAdicional := intDivisaoAdicional + 1;
       end;
    end;

    intDivisaoDescricao := intTamanhoDescricao DIV 35;

    intResto:=intTamanhoDescricao - (intTamanhoDescricao DIV 35)*35;
    if intResto>0 then begin
       intDivisaoDescricao := intDivisaoDescricao + 1;
    end;


    intTamanhoLinha:= 15 * (intDivisaoDescricao+intDivisaoAdicional);

    qrs1.Height:= intTamanhoLinha;
    qrs2.Height:= intTamanhoLinha;
    qrs3.Height:= intTamanhoLinha;
    qrs4.Height:= intTamanhoLinha;
    qrs5.Height:= intTamanhoLinha;
    qrs6.Height:= intTamanhoLinha;
    qrs7.Height:= intTamanhoLinha;
    qrs8.Height:= intTamanhoLinha;
    qrs9.Height:= intTamanhoLinha;
    qrs10.Height:= intTamanhoLinha;
    qrs11.Height:= intTamanhoLinha;
    qrs12.Height:= intTamanhoLinha;
    qrs13.Height:= intTamanhoLinha;
    qrs14.Height:= intTamanhoLinha;
    qrs15.Height:= intTamanhoLinha;

    if intTamanhoDescricao = 0 then
    begin
        qrs1.Visible:= False;
        qrs2.Visible:= False;
        qrs3.Visible:= False;
        qrs4.Visible:= False;
        qrs5.Visible:= False;
        qrs6.Visible:= False;
        qrs7.Visible:= False;
        qrs8.Visible:= False;
        qrs9.Visible:= False;
        qrs10.Visible:= False;
        qrs11.Visible:= False;
        qrs12.Visible:= False;
        qrs13.Visible:= False;
        qrs14.Visible:= False;
        qrs15.Visible:= False;
    end;

    qrs1.Repaint;
    qrs2.Repaint;
    qrs3.Repaint;
    qrs4.Repaint;
    qrs5.Repaint;
    qrs6.Repaint;
    qrs7.Repaint;
    qrs8.Repaint;
    qrs9.Repaint;
    qrs10.Repaint;
    qrs11.Repaint;
    qrs12.Repaint;
    qrs13.Repaint;
    qrs14.Repaint;
    qrs15.Repaint;


   if cdsItensINFADIPROD.AsString<>'' then begin
      Value:=Value+#13+'InfAd: '+cdsItensINFADIPROD.AsString;
   end;
end;

procedure TfqrDANFeQRPaisagem.qrbEmitenteDestinatarioBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
var
   x, iQuantDup: integer;
   Ok: Boolean;
begin
  inherited;
   PrintBand := QRNFe.PageNumber = 1;
   iQuantDup := 0;

   // Destinatario

   with FNFe.Dest do
   begin
      if Trim( CNPJCPF ) = ''                then qrlDestCNPJ.Caption := ''
      else if Length( Trim( CNPJCPF ) ) > 11 then qrlDestCNPJ.Caption := FormatarCNPJ( CNPJCPF )
      else                                        qrlDestCNPJ.Caption := FormatarCPF( CNPJCPF );

      qrlDestIE.Caption   := IE;
      qrlDestNome.Caption := XNome;
      with EnderDest do
      begin
         qrlDestEndereco.Caption := XLgr + IfThen ( Nro = '0', '', ', ' + Nro );
         qrlDestBairro.Caption   := XBairro;
         qrlDestCidade.Caption   := XMun;
         qrlDestUF.Caption       := UF;
         qrlDestCEP.Caption      := NotaUtil.FormatarCEP( FormatFloat( '00000000', CEP ) );
         qrlDestFONE.Caption     := NotaUtil.FormatarFone( Fone );
      end;
   end;

   // Emissao, saida

   qrlEmissao.Caption   := FormatDate(DateToStr(FNFe.Ide.dEmi));
   qrlSaida.Caption     := IfThen( FNFe.Ide.DSaiEnt <> 0, FormatDate(DateToStr(FNFe.Ide.dSaiEnt)));
   // Alterado por Italo em 22/03/2011
   qrlHoraSaida.Caption := IfThen( FNFe.Ide.hSaiEnt <> 0, FormatDateTime('hh:mm:ss', FNFe.Ide.hSaiEnt));

   // Faturas

   // Zera
   for x := 1 to 15 do
   begin
      TQRLabel( FindComponent( 'qrlFatNum'   + intToStr( x ) ) ).Caption := '';
      TQRLabel( FindComponent( 'qrlFatData'  + intToStr( x ) ) ).Caption := '';
      TQRLabel( FindComponent( 'qrlFatValor' + intToStr( x ) ) ).Caption := '';
   end;

   // Incluido por Italo em 13/07/2011
   if FNFe.Cobr.Dup.Count > 0
    then Ok := (FNFe.Ide.dEmi = FNFe.Cobr.Dup[0].dVenc)
    else Ok := True;

   // Alterado por Italo em 27/05/2011
   // TpcnIndicadorPagamento = (ipVista, ipPrazo, ipOutras);
   if (FNFe.Ide.indPag = ipVista) and Ok
    then
      TQRLabel( FindComponent( 'qrlFatNum1' ) ).Caption := 'PAGAMENTO À VISTA'
    else begin
     // Adiciona
     if FNFe.Cobr.Dup.Count > 15 then
      iQuantDup := 15
     else
      iQuantDup := FNFe.Cobr.Dup.Count;

     for x := 0 to (iQuantDup - 1) do with FNFe.Cobr.Dup[ x ] do
     begin
        TQRLabel( FindComponent( 'qrlFatNum'   + intToStr ( x + 1 ) ) ).Caption := NDup;
        TQRLabel( FindComponent( 'qrlFatData'  + intToStr ( x + 1 ) ) ).Caption := FormatDate( DateToStr(DVenc) );
        TQRLabel( FindComponent( 'qrlFatValor' + intToStr ( x + 1 ) ) ).Caption := FormatFloat(VDup);
     end;
    end;

   // Impostos

   with FNFe.Total.ICMSTot do
   begin
      qrlBaseICMS.Caption      := FormatFloat( VBC );
      qrlValorICMS.Caption     := FormatFloat( VICMS );
      qrlBaseICMST.Caption     := FormatFloat( VBCST );
      qrlValorICMST.Caption    := FormatFloat( VST );
      qrlTotalProdutos.Caption := FormatFloat( VProd );
      qrlValorFrete.Caption    := FormatFloat( VFrete );
      qrlValorSeguro.Caption   := FormatFloat( VSeg );
      qrlDescontos.Caption     := FormatFloat( VDesc );
      qrlAcessorias.Caption    := FormatFloat( VOutro );
      qrlValorIPI.Caption      := FormatFloat( VIPI );
      qrlTotalNF.Caption       := FormatFloat( VNF );
   end;

   // Transporte

   with FNFe.Transp do
   begin
     case ModFrete of
      mfContaEmitente     : qrlTransModFrete.Caption := '0-Emitente';
      mfContaDestinatario : qrlTransModFrete.Caption := '1-Destinat.';
      mfContaTerceiros    : qrlTransModFrete.Caption := '2-Terceiros';
      mfSemFrete          : qrlTransModFrete.Caption := '9-Sem Frete';
     end;
      // qrlTransModFrete.Caption   := modFreteToStr( ModFrete );
      qrlTransCodigoANTT.Caption := '';
      qrlTransPlaca.Caption      := '';
      qrlTransUFPlaca.Caption    := '';

      with Transporta do
      begin
         if Trim( CNPJCPF ) = ''                then qrlTransCNPJ.Caption := '' 
         else if Length( Trim( CNPJCPF ) ) > 11 then qrlTransCNPJ.Caption := FormatarCNPJ( CNPJCPF )
         else                                        qrlTransCNPJ.Caption := FormatarCPF( CNPJCPF );

         qrlTransNome.Caption     := XNome;
         qrlTransIE.Caption       := IE;
         qrlTransEndereco.Caption := XEnder;
         qrlTransCidade.Caption   := XMun;
         qrlTransUF.Caption       := UF;
      end;
   end;

   with FNFe.Transp.VeicTransp do
   begin
      qrlTransPlaca.Caption   :=  Placa;
      qrlTransUFPlaca.Caption :=  UF;
      // Alterado por Italo em 11/10/2011
      qrlTransCodigoANTT.Caption := RNTC;
   end;

   if FNFe.Transp.Vol.Count > 0 then with FNFe.Transp.Vol[0] do
   begin
      qrlTransQTDE.Caption      := IntToStr(QVol);
      qrlTransEspecie.Caption   := Esp;
      qrlTransMarca.Caption     := Marca;
      qrlTransNumeracao.Caption := NVol;
      qrlTransPesoLiq.Caption   := FormatFloat( PesoL, '#,0.000#' );
      qrlTransPesoBruto.Caption := FormatFloat( PesoB, '#,0.000#' );
   end
   else
   begin
      qrlTransQTDE.Caption      := '';
      qrlTransEspecie.Caption   := '';
      qrlTransMarca.Caption     := '';
      qrlTransNumeracao.Caption := '';
      qrlTransPesoLiq.Caption   := '';
      qrlTransPesoBruto.Caption := '';
   end;

   // Mensagem para modo Homologacao.

    if FNFe.Ide.tpAmb = taHomologacao then
    begin
        qrlMsgTeste.Caption := 'AMBIENTE DE HOMOLOGAÇÃO - SEM VALOR FISCAL';
        qrlMsgTeste.Enabled := True;
        qrlMsgTeste.Visible := True;
    end else
    begin
        if FNFe.procNFe.cStat > 0 then
        begin
            // Alterado por Italo em 29/11/2012
            // if FNFe.procNFe.cStat = 101 then
            if FNFe.procNFe.cStat in [101, 151, 155] then
            begin
               qrlMsgTeste.Caption  := 'NF-e CANCELADA';
               qrlMsgTeste.Visible  := True;
               qrlMsgTeste.Enabled := True;
            end;
            // Alterado de 102 para 110 por Italo em 27/01/2012
            if FNFe.procNFe.cStat = 110 then
            begin
               qrlMsgTeste.Caption  := 'NF-e DENEGADA';
               qrlMsgTeste.Visible  := True;
               qrlMsgTeste.Enabled := True;
            end;

            // Alterado por Italo em 29/11/2012
            // if not FNFe.procNFe.cStat in [101, 110, 100] then
            if not FNFe.procNFe.cStat in [100, 101, 110, 151, 155] then
            begin
                qrlMsgTeste.Caption:=  FNFe.procNFe.xMotivo;
                qrlMsgTeste.Visible := True;
                qrlMsgTeste.Enabled := True;
            end;
        end else
        begin
            qrlMsgTeste.Caption  := 'NF-E NÃO ENVIADA PARA SEFAZ';
            qrlMsgTeste.Visible  := True;
            qrlMsgTeste.Enabled  := True;
        end;
    end;

    qrlMsgTeste.Repaint;
      
end;

procedure TfqrDANFeQRPaisagem.qrbISSQNBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
begin
  inherited;
   PrintBand := QRNFe.PageNumber = 1;
   if not PrintBand then
      qrbISSQN.Height := 0
   else
   begin
      if FNFE.Total.ISSQNtot.vISS > 0 then
      begin
         qrlInscMunicipal.Caption := FNFE.Emit.IM;
         qrlTotalServicos.Caption := FormatFloat( FNFE.Total.ISSQNtot.vServ );
         qrlBaseISSQN.Caption     := FormatFloat( FNFE.Total.ISSQNtot.vBC );
         qrlValorISSQN.Caption    := FormatFloat( FNFE.Total.ISSQNtot.vISS );
      end
      else
      begin
         qrbISSQN.Height          := 0;
         qrlInscMunicipal.Caption := '';
         qrlTotalServicos.Caption := '';
         qrlBaseISSQN.Caption     := '';
         qrlValorISSQN.Caption    := '';
      end;
   end;
end;

procedure TfqrDANFeQRPaisagem.qrbDadosAdicionaisBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
var
   i: Integer;
   sIndProc: String;
begin
  inherited;
    PrintBand := QRNFe.PageNumber = 1;
    if not PrintBand then
        qrbDadosAdicionais.Height := 0
    else
    with FNFe.InfAdic do
    begin
        qrmDadosAdicionais.Lines.BeginUpdate;
        qrmDadosAdicionais.Lines.Clear;

        //**********************************************************************
        // informacoes de uso livre do fisco
        for i := 0 to obsFisco.Count-1 do with obsFisco.Items[i] do
        begin
            qrmDadosAdicionais.Lines.Add( StringReplace( XCampo, '&lt;BR&gt;', #13#10, [rfReplaceAll,rfIgnoreCase] )+': '+
                                           StringReplace( XTexto, '&lt;BR&gt;', #13#10, [rfReplaceAll,rfIgnoreCase] ) );
        end;

        //**********************************************************************
        // informacoes de uso livre do contribuinte
        for i := 0 to ObsCont.Count-1 do with ObsCont.Items[i] do
        begin
            qrmDadosAdicionais.Lines.Add( StringReplace( XCampo, '&lt;BR&gt;', #13#10, [rfReplaceAll,rfIgnoreCase] )+': '+
                                           StringReplace( XTexto, '&lt;BR&gt;', #13#10, [rfReplaceAll,rfIgnoreCase] ) );
        end;

        //**********************************************************************
        // informacoes do grupo de processo referenciado
        for i := 0 to procRef.Count-1 do with procRef.Items[i] do
        begin
          case indProc of
            ipSEFAZ:           sIndProc := 'SEFAZ';
            ipJusticaFederal:  sIndProc := 'JUSTIÇA FEDERAL';
            ipJusticaEstadual: sIndProc := 'JUSTIÇA ESTADUAL';
            ipSecexRFB:        sIndProc := 'SECEX / RFB';
            ipOutros:          sIndProc := 'OUTROS';
          end;

            qrmDadosAdicionais.Lines.Add( StringReplace( 'PROCESSO OU ATO CONCESSÓRIO Nº: ' +
                                                         nProc, '&lt;BR&gt;', #13#10, [rfReplaceAll,rfIgnoreCase] )+' - '+
                                           StringReplace( 'ORIGEM: ' +
                                                          sIndProc , '&lt;BR&gt;', #13#10, [rfReplaceAll,rfIgnoreCase] ) );
        end;

        //**********************************************************************
        // informacoes complementares emitente
        if infCpl <> '' then
        begin
            qrmDadosAdicionais.Lines.Add(StringReplace( InfCpl, '&lt;BR&gt;', #13#10, [rfReplaceAll,rfIgnoreCase] ) );
        end;

        //**********************************************************************
        // informacoes complementares interesse ao fisco
        if infAdFisco <> '' then
        begin
            qrmDadosAdicionais.Lines.Add(StringReplace( 'INFORMAÇÕES ADICIONAIS DE INTERESSE DO FISCO: ' +
                                                       infAdFisco,'&lt;BR&gt;', #13#10, [rfReplaceAll,rfIgnoreCase] ));
        end;
        //**********************************************************************
        // local de retirada
        with FNFe.Retirada do
        begin
            if xLgr <> '' then
            begin
                qrmDadosAdicionais.Lines.Add(   'LOCAL DE RETIRADA: '                                                          +
                                                StringReplace( xLgr, '&lt;BR&gt;',      #13#10, [rfReplaceAll,rfIgnoreCase] )  + ', ' +
                                                StringReplace( nro, '&lt;BR&gt;',       #13#10, [rfReplaceAll,rfIgnoreCase] )  + '/' +
                                                StringReplace( xCpl, '&lt;BR&gt;',      #13#10, [rfReplaceAll,rfIgnoreCase] )  + ' ' +
                                                StringReplace( xBairro, '&lt;BR&gt;',   #13#10, [rfReplaceAll,rfIgnoreCase] )  + ' - ' +
                                                StringReplace( xMun, '&lt;BR&gt;',      #13#10, [rfReplaceAll,rfIgnoreCase] )  + ' ' +
                                                StringReplace( UF, '&lt;BR&gt;',        #13#10, [rfReplaceAll,rfIgnoreCase] ));
            end;
        end;
        //**********************************************************************
        // local de entrega
        with FNFe.Entrega do
        begin
            if xLgr <> '' then
            begin
                qrmDadosAdicionais.Lines.Add(   'LOCAL DE ENTREGA: '                                                          +
                                                StringReplace( xLgr, '&lt;BR&gt;',      #13#10, [rfReplaceAll,rfIgnoreCase] )  + ', ' +
                                                StringReplace( nro, '&lt;BR&gt;',       #13#10, [rfReplaceAll,rfIgnoreCase] )  + '/' +
                                                StringReplace( xCpl, '&lt;BR&gt;',      #13#10, [rfReplaceAll,rfIgnoreCase] )  + ' ' +
                                                StringReplace( xBairro, '&lt;BR&gt;',   #13#10, [rfReplaceAll,rfIgnoreCase] )  + ' - ' +
                                                StringReplace( xMun, '&lt;BR&gt;',      #13#10, [rfReplaceAll,rfIgnoreCase] )  + ' ' +
                                                StringReplace( UF, '&lt;BR&gt;',        #13#10, [rfReplaceAll,rfIgnoreCase] ));
            end;
        end;

        if FNFe.Ide.tpEmis in [teContingencia, teFSDA] then
            qrmDadosAdicionais.Lines.Add('DANFE em Contingência - Impresso em decorrência de problemas técnicos.');
        //**********************************************************************
      qrmDadosAdicionais.Lines.Text:=StringReplace(qrmDadosAdicionais.Lines.Text,';',#13,[rfReplaceAll]);
      qrmDadosAdicionais.Lines.EndUpdate;

        // imprime data e hora da impressao
        QrlDataHoraImpressao.Caption:= 'DATA E HORA DA IMPRESSÃO: ' + FormatDateTime('dd/mm/yyyy hh:nn',Now);

        // imprime usuario
        if FUsuario <> '' then
        begin
            QrlDataHoraImpressao.Caption:= QrlDataHoraImpressao.Caption + '   USUÁRIO: ' + FUsuario;
        end;

        // imprime sistema
        if FSistema <> '' then
        begin
            qrlSistema.Caption:= 'Desenvolvido por ' + FSistema;
        end else
        begin
            qrlSistema.Caption:= '';
        end;

    end;
end;

procedure TfqrDANFeQRPaisagem.qrbDadosDanfeBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
var
    strChaveContingencia: string;
begin
  inherited;

   //hrsoft 4/8/2010
   FTotalPages := HrTotalPages;
   //fim hrsoft 4/8/2010

   qrlPageNumber.Caption := format ( '%2.2d', [ QRNFe.PageNumber ] )
                    + '/' + format ( '%2.2d', [ FTotalPages ] );

   if QRNFe.PageNumber = 1 then
   begin
      if (FLogo <> '') and FilesExists(FLogo) then
      begin
        qriLogo.Picture.LoadFromFile(FLogo);
      end;

      // Alterado a posição por Italo em 22/06/2012
      // conforme problema detectado por Wilson
      qrlCNPJ.Caption              := FormatarCNPJ( FNFe.Emit.CNPJCPF  );
      qrlInscrEstSubst.caption     := FNFe.Emit.IEST;
      qrlInscricaoEstadual.Caption := FNFe.Emit.IE;

      // Inclido por Italo em 18/06/2012
      if FExpandirLogoMarca then
      begin
//        qriLogo.top:=9;
//        qriLogo.Left:=3;
//        qriLogo.Height:=132;
//        qriLogo.Width:=762;
//        qriLogo.Stretch:=true;
        qrmEmitente.Enabled:=False;
        qrmEndereco.Enabled:=False;
        qrlFone.Enabled:=False;
      end;

      // Incluido por Italo em 18/06/2012
      if not FExpandirLogoMarca then
       begin
        qrmEmitente.Enabled:=True;
        qrmEndereco.Enabled:=True;
        qrlFone.Enabled:=True;
        // Emitente
        with FNFe.Emit do
        begin
         qrmEmitente.Lines.Text := XNome;
         with EnderEmit do
         begin
            qrmEndereco.Lines.Clear;
//            qrmEndereco.Lines.add ( XLgr + IfThen ( Nro = '0', '', ', ' + Nro ) + ' ' + XCpl + ' ' + XBairro );
//            qrmEndereco.Lines.add ( XMun + ' - ' + UF );
//            qrmEndereco.Lines.add ( 'CEP ' + NotaUtil.FormatarCEP( FormatFloat( '00000000', CEP ) ) );
            // Alterado por Italo em 23/03/2011
            qrmEndereco.Lines.Add(XLgr + IfThen(Nro = '0', '', ', ' + Nro));
            if XCpl <> '' then qrmEndereco.Lines.Add(XCpl);
            if XBairro <> '' then qrmEndereco.Lines.Add(XBairro);
            qrmEndereco.Lines.Add(XMun + ' - ' + UF);
            qrmEndereco.Lines.Add('CEP: ' + NotaUtil.FormatarCEP(FormatFloat( '00000000', CEP )));
            // Alterado por Italo em 23/03/2011
            if Trim(FSite) <> '' then qrmEndereco.Lines.Add ('SITE: ' + FSite);
            // Alterado por Italo em 18/06/2012
            if Trim(FEmail) <> '' then qrmEndereco.Lines.Add ('E-MAIL: ' + FEmail);
            // telefone
            qrlFone.Caption:= '';
            // Alterado por Italo em 18/06/2012
            if (fone <> '') and (FFax = '') then qrlFone.Caption := 'FONE: ' + NotaUtil.FormatarFone( Fone );
            // Alterado por Italo em 18/06/2012
            if (FFax <> '') and (fone = '') then qrlFone.Caption:= 'FAX: ' + FFax;
            // Alterado por Italo em 18/06/2012
            if (FFax <> '') and (fone <> '') then qrlFone.Caption:= 'FONE: ' +
                            NotaUtil.FormatarFone( Fone ) + #13 +'FAX: ' + FFax;
         end;
       end;
      end;

      // Danfe
      qrlEntradaSaida.Caption := tpNFToStr( FNFe.Ide.tpNF );
      qrlNumNF1.Caption       := FormatFloat( '000,000,000', FNFe.Ide.nNF );
      qrlSERIE1.Caption       := IntToStr( FNFe.Ide.serie );
      qrlChave.Caption        := NotaUtil.FormatarChaveAcesso( Copy ( FNFe.InfNFe.Id, 4, 44 ) );
      qrlNatOperacao.Caption  := FNFe.Ide.natOp;
      SetBarCodeImage( Copy ( FNFe.InfNFe.Id, 4, 44 ), qriBarCode );

        // Normal **************************************************************
        if FNFe.Ide.tpEmis in [teNormal, teSCAN] then
        begin
            if FNFe.procNFe.cStat = 100 then
            begin
                qrlDescricao.Caption:= 'PROTOCOLO DE AUTORIZAÇÃO DE USO';
            end;

            // Alterado por Italo em 29/11/2012
            // if FNFe.procNFe.cStat = 101 then
            if FNFe.procNFe.cStat in [101, 151, 155] then
            begin
                qrlDescricao.Caption:= 'PROTOCOLO DE HOMOLOGAÇÃO DE CANCELAMENTO';
            end;

            // Alterado de 102 para 110 por Italo em 27/01/2012
            if FNFe.procNFe.cStat = 110 then
            begin
                qrlDescricao.Caption:= 'PROTOCOLO DE DENEGAÇÃO DE USO';
            end;

            if FProtocoloNFE <> '' then
                qrlProtocolo.Caption        := FProtocoloNFE
            else
                qrlProtocolo.Caption        :=  FNFe.procNFe.nProt + ' ' +
                                                SeSenao(FNFe.procNFe.dhRecbto <> 0, DateTimeToStr(FNFe.procNFe.dhRecbto),'');
            IF FNFe.procNFe.cStat = 0 THEN
              BEGIN
              qrlProtocolo.Caption := 'NF-E NÃO ENVIADA PARA SEFAZ';
              qrlProtocolo.Font.Color := clRed;
              END
            ELSE qrlProtocolo.Font.Color := clWindowText;
            qriBarCodeContingencia.Visible  := False;
            qrlMsgAutorizado.Enabled        := True;
        end;
        // Contingencia ********************************************************
        if FNFe.Ide.tpEmis in [teContingencia, teFSDA] then
        begin
            strChaveContingencia:= NotaUtil.GerarChaveContingencia(FNFe);
            SetBarCodeImage(strChaveContingencia,qriBarCodeContingencia);
            qriBarCodeContingencia.Visible  := True;
            qrlMsgAutorizado.Enabled        := False;
            qrlDescricao.Caption            := 'DADOS DA NF-E';
            qrlProtocolo.Caption            := NotaUtil.FormatarChaveContigencia(strChaveContingencia);
        end;
    //************************************************************************
    end;
end;

procedure TfqrDANFeQRPaisagem.qrbItensBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
begin
  inherited;
  Inc( nItemControle );
  if nItemControle = 1 then
    qrbItens.ForceNewPage := False;
    
  if QRNFe.PageNumber = 1 then
  begin
    if nItemControle = _NUM_ITEMS_PAGE1 then
    begin
      nItemControle := 0;
      qrbItens.ForceNewPage := True;
    end;
  end
  else
  begin
    if nItemControle = _NUM_ITEMS_OTHERPAGES then
    begin
      nItemControle := 0;
      qrbItens.ForceNewPage := True;
    end;
  end;


//  if QRNFe.PageNumber = 1 then
//     if QRNFe.RecordCount < _NUM_ITEMS_PAGE1 then
//        qrsFimItens.Enabled := ( nItemControle = QRNFe.RecordCount   )
//     else
//        qrsFimItens.Enabled := ( nItemControle = _NUM_ITEMS_PAGE1    )
//  else
//  begin
//     qrsFimItens.Enabled := ( nItemControle = _NUM_ITEMS_OTHERPAGES  ) or
//                            ( QRNFe.RecordNumber = QRNFe.RecordCount ) or
//                            ( cdsItens.Eof                           );
//  end;
//  if qrsFimItens.Enabled then
//     nItemControle := 0;
end;

procedure TfqrDANFeQRPaisagem.FormCreate(Sender: TObject);
const
  lTop: integer = 20;
begin
  inherited;
//  QRAngledLabel1.Top := QRAngledLabel1.Top - lTop;
//  QRAngledLabel2.Top := QRAngledLabel2.Top - lTop;
//  QRAngledLabel3.Top := QRAngledLabel3.Top - lTop;
//  QRAngledLabel4.Top := QRAngledLabel4.Top - lTop;
//  QRAngledLabel5.Top := QRAngledLabel5.Top - lTop;
//  QRAngledLabel6.Top := QRAngledLabel6.Top - lTop;
//  QRAngledLabel7.Top := QRAngledLabel7.Top - lTop;
//  QRAngledLabel8.Top := QRAngledLabel8.Top - lTop;
//  QRAngledLabel9.Top := QRAngledLabel9.Top - lTop;
end;

procedure TfqrDANFeQRPaisagem.QRAngledLabelText_Print(sender: TObject;
  var Value: String);
begin
  Value := Trim(Value);
end;

end.
