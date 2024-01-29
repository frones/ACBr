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

unit ACBrCTeDACTeRLRetratoA5;

interface

uses
  SysUtils, Variants, Classes,
  {$IFDEF CLX}
  QGraphics, QControls, QForms, QDialogs, QExtCtrls, Qt, QStdCtrls,
  {$ELSE}
  Messages, Graphics, Controls, Forms, Dialogs, ExtCtrls, MaskUtils, StdCtrls,
  {$ENDIF}
  DB, StrUtils,
  RLReport, RLFilters, RLPDFFilter, RLTypes, RLBarcode, RLRichText,
  pcnConversao,
  ACBrCTeDACTeRL;

type

  { TfrmDACTeRLRetratoA5 }

  TfrmDACTeRLRetratoA5 = class(TfrmDACTeRL)
    RLBarcode1: TRLBarcode;
    rliBarCode: TRLBarcode;
    rlb_08_Itens: TRLBand;
    rldbtTpDoc1: TRLDBText;
    rldbtCnpjEmitente1: TRLDBText;
    rldbtDocumento1: TRLDBText;
    rldbtDocumento2: TRLDBText;
    rldbtCnpjEmitente2: TRLDBText;
    rldbtTpDoc2: TRLDBText;
    RLDraw29: TRLDraw;
    rlb_01_Recibo: TRLBand;
    rlsLinhaH03: TRLDraw;
    rlmEmitente: TrlMemo;
    rlmDadosEmitente: TrlMemo;
    rliLogo: TrlImage;
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
    rllPageNumber: TRLLabel;
    rllSerie: TRLLabel;
    rllModelo: TRLLabel;
    rllEmissao: TRLLabel;
    rllModal: TRLLabel;
    rllProtocolo: TRLLabel;
    rllTipoCte: TRLLabel;
    rllDescricao: TRLLabel;
    rlLabel77: TRLLabel;
    rlLabel2: TRLLabel;
    rlLabel9: TRLLabel;
    rllTipoServico: TRLLabel;
    rlLabel28: TRLLabel;
    rllTomaServico: TRLLabel;
    rllInscSuframa: TRLLabel;
    rlb_07_HeaderItens: TRLBand;
    rlLabel91: TRLLabel;
    rlLabel92: TRLLabel;
    rlLabel96: TRLLabel;
    rlLabel100: TRLLabel;
    rlLabel106: TRLLabel;
    rlLabel109: TRLLabel;
    RLDraw34: TRLDraw;
    RLDraw28: TRLDraw;
    RLDraw35: TRLDraw;
    rlsFimItens: TRLDraw;
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
    rlmObs: TrlMemo;
    rllMsgTeste: TRLLabel;
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
    rllNomeSeguradora: TRLLabel;
    rlLabel37: TRLLabel;
    rllRespSeguroMerc: TRLLabel;
    rlLabel39: TRLLabel;
    rllNroApolice: TRLLabel;
    rlLabel40: TRLLabel;
    rllNroAverbacao: TRLLabel;
    RLDraw8: TRLDraw;
    RLDraw7: TRLDraw;
    rlb_04_DadosNotaFiscal: TRLBand;
    rlb_05_Complemento: TRLBand;
    rlLabel44: TRLLabel;
    rlmCompNome1: TrlMemo;
    rlLabel46: TRLLabel;
    rlmCompValor1: TrlMemo;
    rlLabel42: TRLLabel;
    rlmCompNome2: TrlMemo;
    rlLabel45: TRLLabel;
    rlmCompValor2: TrlMemo;
    rlLabel47: TRLLabel;
    rlmCompNome3: TrlMemo;
    rlLabel48: TRLLabel;
    rlmCompValor3: TrlMemo;
    rlLabel49: TRLLabel;
    rllVlrTotServico: TRLLabel;
    rlLabel50: TRLLabel;
    rllVlrTotReceber: TRLLabel;
    RLDraw18: TRLDraw;
    RLDraw16: TRLDraw;
    RLDraw15: TRLDraw;
    RLDraw19: TRLDraw;
    RLDraw6: TRLDraw;
    rlLabel61: TRLLabel;
    rlLabel63: TRLLabel;
    rlmComplChave1: TrlMemo;
    rlmComplChave2: TrlMemo;
    rlsQuadro01: TRLDraw;
    rlsQuadro04: TRLDraw;
    rlsQuadro05: TRLDraw;
    rlsQuadro07: TRLDraw;
    rlsQuadro08: TRLDraw;
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
    rlsLinhaH05: TRLDraw;
    RLDraw55: TRLDraw;
    RLDraw9: TRLDraw;
    RLDraw56: TRLDraw;
    RLDraw58: TRLDraw;
    RLDraw59: TRLDraw;
    RLDraw60: TRLDraw;
    RLDraw61: TRLDraw;
    RLDraw62: TRLDraw;
    rlb_17_Sistema: TRLBand;
    RLDraw10: TRLDraw;
    rlLabel65: TRLLabel;
    RLDraw2: TRLDraw;
    rlLabel66: TRLLabel;
    rlLabel70: TRLLabel;
    RLDraw53: TRLDraw;
    rlLabel15: TRLLabel;
    rllblSistema: TRLLabel;
    rlmQtdUnidMedida1: TrlMemo;
    rlmQtdUnidMedida2: TrlMemo;
    rlmQtdUnidMedida3: TrlMemo;
    rlmQtdUnidMedida5: TrlMemo;
    rlb_06_ValorPrestacao: TRLBand;
    RLDraw46: TRLDraw;
    RLDraw48: TRLDraw;
    RLDraw49: TRLDraw;
    RLDraw50: TRLDraw;
    RLDraw51: TRLDraw;
    RLDraw52: TRLDraw;
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
    rlmUF2: TrlMemo;
    rlmTipo2: TrlMemo;
    rlmRNTRC2: TrlMemo;
    rlmPlaca2: TrlMemo;
    rlmCNPJForn: TrlMemo;
    rlmNumCompra: TrlMemo;
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
    rlmCNPJPg: TrlMemo;
    RLDraw88: TRLDraw;
    RLDraw99: TRLDraw;
    rlmQtdUnidMedida4: TrlMemo;
    rlLabel73: TRLLabel;
    RLDraw100: TRLDraw;
    rlsQuadro03: TRLDraw;
    RLDraw101: TRLDraw;
    RLDraw102: TRLDraw;
    RLDraw103: TRLDraw;
    rlsQuadro02: TRLDraw;
    rlLabel52: TRLLabel;
    rllSitTrib: TRLLabel;
    RLDraw22: TRLDraw;
    rlLabel55: TRLLabel;
    rlLabel56: TRLLabel;
    rlLabel54: TRLLabel;
    rlLabel53: TRLLabel;
    rlLabel58: TRLLabel;
    RLDraw20: TRLDraw;
    RLDraw23: TRLDraw;
    RLDraw25: TRLDraw;
    RLDraw26: TRLDraw;
    rllBaseCalc: TRLLabel;
    rllAliqICMS: TRLLabel;
    rllVlrICMS: TRLLabel;
    rllRedBaseCalc: TRLLabel;
    rllICMS_ST: TRLLabel;
    rlLabel10: TRLLabel;
    rlLabel11: TRLLabel;
    lblCIOT: TRLLabel;
    rlLabel83: TRLLabel;
    rlLabel84: TRLLabel;
    rllRntrcEmpresa: TRLLabel;
    rllCIOT: TRLLabel;
    rllLotacao: TRLLabel;
    rllDtPrevEntrega: TRLLabel;
    rlsCIOT: TRLDraw;
    RLDraw36: TRLDraw;
    RLDraw37: TRLDraw;
    RLDraw38: TRLDraw;
    RLDraw24: TRLDraw;
    rlLabel85: TRLLabel;
    RLDraw1: TRLDraw;
    rlLabel20: TRLLabel;
    RLDraw5: TRLDraw;
    rlmObsFisco: TrlMemo;
    RLDraw3: TRLDraw;
    rlb_03_DadosRedespachoExpedidor: TRLBand;
    RLDraw11: TRLDraw;
    RLDraw17: TRLDraw;
    RLLabel7: TRLLabel;
    rllRazaoResdes: TRLLabel;
    rllEnderecoRedes1: TRLLabel;
    rllEnderecoRedes2: TRLLabel;
    rllMunRedes: TRLLabel;
    rllCnpjRedes: TRLLabel;
    rllPaisRedes: TRLLabel;
    RLLabel81: TRLLabel;
    RLLabel82: TRLLabel;
    RLLabel86: TRLLabel;
    RLLabel87: TRLLabel;
    RLLabel88: TRLLabel;
    RLLabel89: TRLLabel;
    rllCEPRedes: TRLLabel;
    rllInscEstRedes: TRLLabel;
    RLLabel97: TRLLabel;
    rllFoneRedes: TRLLabel;
    rllRazaoReceb: TRLLabel;
    rllEnderecoRecebe1: TRLLabel;
    rllEnderecoRecebe2: TRLLabel;
    rllMunReceb: TRLLabel;
    rllCnpjReceb: TRLLabel;
    rllPaisReceb: TRLLabel;
    RLLabel80: TRLLabel;
    RLLabel90: TRLLabel;
    RLLabel94: TRLLabel;
    RLLabel99: TRLLabel;
    RLLabel101: TRLLabel;
    RLLabel102: TRLLabel;
    rllCEPReceb: TRLLabel;
    rllInscEstReceb: TRLLabel;
    RLLabel105: TRLLabel;
    rllFoneReceb: TRLLabel;
    RLLabel108: TRLLabel;
    RLDraw12: TRLDraw;
    procedure rlb_01_ReciboBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlb_02_CabecalhoBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlb_03_DadosDACTeBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlb_04_DadosNotaFiscalBeforePrint(Sender: TObject;
      var PrintIt: boolean);
    procedure rlb_05_ComplementoBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlb_06_ValorPrestacaoBeforePrint(Sender: TObject;
      var PrintIt: boolean);
    procedure rlb_07_HeaderItensBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlb_08_ItensBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlb_09_ObsBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlb_17_SistemaBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlb_12_ModAereoBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlb_13_ModAquaviarioBeforePrint(Sender: TObject;
      var PrintIt: boolean);
    procedure rlb_14_ModFerroviarioBeforePrint(Sender: TObject;
      var PrintIt: boolean);
    procedure rlb_15_ModDutoviarioBeforePrint(Sender: TObject;
      var PrintIt: boolean);
    procedure rlb_01_Recibo_AereoBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlb_11_ModRodLot104BeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlb_18_ReciboBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure RLCTeBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlb_08_ItensAfterPrint(Sender: TObject);
    procedure rlb_03_DadosRedespachoExpedidorBeforePrint(Sender: TObject;
      var PrintIt: boolean);
  private
    Linhas: integer;
    procedure Itens;
  public
    constructor Create(TheOwner: TComponent); override;

    procedure ProtocoloCTe(const sProtocolo: string);
  end;

implementation

uses
  DateUtils, ACBrDFeUtil,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrUtil.DateTime,
  ACBrValidador, pcteConversaoCTe,
  pcteCTe, ACBrDFeReportFortes;

{$IFnDEF FPC}
  {$R *.dfm}

{$ELSE}
  {$R *.lfm}
{$ENDIF}

var
  FProtocoloCTe: string;

constructor TfrmDACTerlRetratoA5.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  RLCTe.PageSetup.PaperSize := fpA5;
  RLCTe.PageSetup.PaperHeight := 148.0;
  RLCTe.PageSetup.PaperWidth := 210.0;
end;

procedure TfrmDACTerlRetratoA5.Itens;
var
  I, J, K, Item: integer;
begin
  if rlCTe.PageNumber > 0 then
    exit;

  Item := 0;
  //Varrendo NF comum
  for I := 0 to (fpCTe.infCTeNorm.infDoc.infNF.Count - 1) do
  begin
    with fpCTe.infCTeNorm.infDoc.InfNF.Items[I] do
    begin
      if (Item mod 2) = 0 then
      begin
        cdsDocumentos.Append;

        cdsDocumentos.FieldByName('TIPO_1').AsString := 'NF';
        cdsDocumentos.FieldByName('CNPJCPF_1').AsString := FormatarCNPJouCPF(fpCTe.Rem.CNPJCPF);
        cdsDocumentos.FieldByName('DOCUMENTO_1').AsString := serie + '-' + nDoc;
      end
      else
      begin
        cdsDocumentos.FieldByName('TIPO_2').AsString := 'NF';
        cdsDocumentos.FieldByName('CNPJCPF_2').AsString := FormatarCNPJouCPF(fpCTe.Rem.CNPJCPF);
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
      end
      else
      begin
        cdsDocumentos.FieldByName('TIPO_2').AsString := 'NF-E ' + copy(chave, 26, 9);
        cdsDocumentos.FieldByName('CNPJCPF_2').AsString := FormatarChaveAcesso(chave);
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
        // TpcteTipoDocumento = (tdDeclaracao, tdDutoviario, tdOutros);
        case tpDoc of
          tdDeclaracao:
          begin
            cdsDocumentos.FieldByName('TIPO_1').AsString := 'DECLAR';
            cdsDocumentos.FieldByName('CNPJCPF_1').AsString := FormatarCNPJouCPF(fpCTe.Rem.CNPJCPF);
            cdsDocumentos.FieldByName('DOCUMENTO_1').AsString := 'Declaração Doc.: ' + nDoc;
          end;
          tdDutoviario:
          begin
            cdsDocumentos.FieldByName('TIPO_1').AsString := 'DUTO';
            cdsDocumentos.FieldByName('CNPJCPF_1').AsString := FormatarCNPJouCPF(fpCTe.Rem.CNPJCPF);
            cdsDocumentos.FieldByName('DOCUMENTO_1').AsString := 'Dutoviário Doc.: ' + nDoc;
          end;
          tdOutros:
          begin
            cdsDocumentos.FieldByName('TIPO_1').AsString := 'Outros';
            cdsDocumentos.FieldByName('CNPJCPF_1').AsString := FormatarCNPJouCPF(fpCTe.Rem.CNPJCPF);
            cdsDocumentos.FieldByName('DOCUMENTO_1').AsString := copy(trim(descOutros), 1, 20) + ' Doc.: ' + nDoc;
          end;
        end;
      end
      else
      begin
        // TpcteTipoDocumento = (tdDeclaracao, tdDutoviario, tdOutros);
        case tpDoc of
          tdDeclaracao:
          begin
            cdsDocumentos.FieldByName('TIPO_2').AsString := 'DECLAR';
            cdsDocumentos.FieldByName('CNPJCPF_2').AsString := FormatarCNPJouCPF(fpCTe.Rem.CNPJCPF);
            cdsDocumentos.FieldByName('DOCUMENTO_2').AsString := 'Declaração Doc.: ' + nDoc;
          end;
          tdDutoviario:
          begin
            cdsDocumentos.FieldByName('TIPO_2').AsString := 'DUTO';
            cdsDocumentos.FieldByName('CNPJCPF_2').AsString := FormatarCNPJouCPF(fpCTe.Rem.CNPJCPF);
            cdsDocumentos.FieldByName('DOCUMENTO_2').AsString := 'Dutoviário Doc.: ' + nDoc;
          end;
          tdOutros:
          begin
            cdsDocumentos.FieldByName('TIPO_2').AsString := 'Outros';
            cdsDocumentos.FieldByName('CNPJCPF_2').AsString := FormatarCNPJouCPF(fpCTe.Rem.CNPJCPF);
            cdsDocumentos.FieldByName('DOCUMENTO_2').AsString := copy(trim(descOutros), 1, 20) + ' Doc.: ' + nDoc;
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

            cdsDocumentos.Post;
          end;
          Inc(Item);
        end;
      end;
    end;
  end;

  cdsDocumentos.First;
end;

procedure TfrmDACTerlRetratoA5.ProtocoloCTe(const sProtocolo: string);
begin
  FProtocoloCTe := sProtocolo;
end;

procedure TfrmDACTerlRetratoA5.rlb_01_ReciboBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  inherited;
  PrintIt := (rlCTe.PageNumber = 1) and (fpCTe.Ide.modal <> mdAereo) and
    (fpDACTe.PosCanhoto = prCabecalho);

  rllSerie2.Caption := FormatFloat('000', fpCTe.Ide.serie);
  rllNumCte2.Caption := FormatFloat('000,000,000', fpCTe.Ide.nCT);
  rlb_01_Recibo.Enabled := (fpCTe.Ide.tpCTe = tcNormal);
end;

procedure TfrmDACTerlRetratoA5.rlb_01_Recibo_AereoBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  inherited;
  PrintIt := (rlCTe.PageNumber = 1) and (fpCTe.Ide.modal = mdAereo);

  rlb_01_Recibo_Aereo.Enabled := (fpCTe.Ide.tpCTe = tcNormal);
end;

procedure TfrmDACTerlRetratoA5.rlb_02_CabecalhoBeforePrint(Sender: TObject;
  var PrintIt: boolean);
var
  strChaveContingencia: string;
begin
  inherited;

  if Trim(fpDACTe.Logo) <> '' then
  begin
    rliLogo.Picture.LoadFromFile(fpDACTe.Logo);
  end;

  if fpDACTe.ExpandeLogoMarca then
  begin
    rliLogo.top := 3;
    rliLogo.Left := 2;
    rliLogo.Height := 110;
    rliLogo.Width := 329;


    TDFeReportFortes.AjustarLogo(rliLogo, fpDACTe.ExpandeLogoMarcaConfig);

    rlmEmitente.Enabled := False;
    rlmDadosEmitente.Enabled := False;
  end;

  rllModal.Caption := ACBrStr(TpModalToStrText(fpCTe.Ide.modal));
  rllModelo.Caption := IntToStr(fpCTe.Ide.modelo);
  rllSerie.Caption := FormatFloat('000', fpCTe.Ide.serie);
  rllNumCte.Caption := FormatFloat('000,000,000', fpCTe.Ide.nCT);
  rllPageNumber.Caption := format('%2.2d', [rlCTe.PageNumber]) + '/' +
    format('%2.2d', [fpTotalPages]);
  rllEmissao.Caption := FormatDateTimeBr(fpCTe.Ide.dhEmi);
  rliBarCode.Caption := OnlyNumber(fpCTe.InfCTe.Id);
  rllChave.Caption := FormatarChaveAcesso(OnlyNumber(fpCTe.InfCTe.Id));

  if not fpDACTe.ExpandeLogoMarca then
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
        rlmDadosEmitente.Lines.Add('CEP: ' + FormatarCEP(CEP) + ' - ' +
          XMun + ' - ' + UF);
      end;
      rlmDadosEmitente.Lines.Add('CNPJ: ' + FormatarCNPJ(CNPJ));
      rlmDadosEmitente.Lines.Add(ACBrStr('INSCRIÇÃO ESTADUAL: ') + IE);
      rlmDadosEmitente.Lines.Add('TELEFONE: ' + FormatarFone(EnderEmit.Fone));

      if Trim(fpDACTe.Site) <> '' then
        rlmDadosEmitente.Lines.Add(fpDACTe.Site);
    end;
  end;

  rllTipoCte.Caption := tpCTToStrText(fpCTe.Ide.tpCTe);
  rllTipoServico.Caption := TpServToStrText(fpCTe.Ide.tpServ);
  if fpCTe.Ide.Toma4.xNome = '' then
    rllTomaServico.Caption := TpTomadorToStrText(fpCTe.Ide.Toma03.Toma)
  else
    rllTomaServico.Caption := TpTomadorToStrText(fpCTe.Ide.Toma4.toma);
  //rllFormaPagamento.Caption := tpforPagToStrText(fpCTe.Ide.forPag);

  // Normal **************************************************************
  if fpCTe.Ide.tpEmis in [teNormal, teSCAN] then
  begin
    //rllVariavel1.Enabled := True;
    RLBarcode1.Enabled := False;
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
        IfThen(fpCTe.procCTe.dhRecbto <>
        0, DateTimeToStr(fpCTe.procCTe.dhRecbto), '');
  end;

  // Contingencia ********************************************************
  if fpCTe.Ide.tpEmis in [teContingencia, teFSDA] then
  begin
    if fpCTe.procCTe.cStat in [100, 101, 110] then
    begin
      //      rllVariavel1.Enabled := True;
      RLBarcode1.Enabled := False;
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
          IfThen(fpCTe.procCTe.dhRecbto <>
          0, DateTimeToStr(fpCTe.procCTe.dhRecbto), '');
    end
    else
    begin
      //      rllVariavel1.Enabled := False;
      RLBarcode1.Enabled := True;

      strChaveContingencia := fpACBrCTe.GerarChaveContingencia(fpCTe);
      RLBarcode1.Caption := strChaveContingencia;
      rllDescricao.Caption := 'DADOS DO CT-E';
      rllProtocolo.Caption := FormatarChaveAcesso(strChaveContingencia);
    end;
  end;

  // DPEC ****************************************************************
  if fpCTe.Ide.tpEmis = teDPEC then
  begin
    rllDescricao.Caption := ACBrStr('NÚMERO DE REGISTRO DPEC');
    rllProtocolo.Caption := FProtocoloCTE;
  end;

  //rllVariavel1.Enabled := True;
  RLBarcode1.Enabled := False;

  rllInscSuframa.Caption := fpCTe.Dest.ISUF;
end;

procedure TfrmDACTerlRetratoA5.rlb_03_DadosDACTeBeforePrint(Sender: TObject;
  var PrintIt: boolean);
var
  i: integer;
begin
  inherited;
  PrintIt := rlCTe.PageNumber = 1;

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

  // Imprime os dados do Redespacho ou Recebedor
  rlb_03_DadosRedespachoExpedidor.Visible :=
    (fpCTe.exped.xNome <> '') or (fpCTe.receb.xNome <> '');


  //DADOS REDESPACHO
  if (fpCTe.exped.xNome <> '') then
  begin
    rllRazaoResdes.Caption := fpCTe.exped.xNome;
    rllEnderecoRedes1.Caption :=
      fpCTe.exped.enderExped.xLgr + ', ' + fpCTe.exped.enderExped.nro;
    rllEnderecoRedes2.Caption :=
      fpCTe.exped.enderExped.xCpl + ' - ' + fpCTe.exped.enderExped.xBairro;
    rllCEPRedes.Caption := FormatarCEP(fpCTe.exped.enderExped.CEP);
    rllMunRedes.Caption := fpCTe.exped.enderExped.xMun + ' - ' + fpCTe.exped.enderExped.UF;
    rllCnpjRedes.Caption := FormatarCNPJouCPF(fpCTe.exped.CNPJCPF);
    rllPaisRedes.Caption := fpCTe.exped.enderExped.xPais;
    rllInscEstRedes.Caption := fpCTe.exped.IE;
    rllFoneRedes.Caption := FormatarFone(fpCTe.exped.fone);
  end;

  //DADOS RECEBEDOR
  if (fpCTe.receb.xNome <> '') then
  begin
    rllRazaoReceb.Caption := fpCTe.receb.xNome;
    rllEnderecoRecebe1.Caption :=
      fpCTe.receb.enderReceb.xLgr + ', ' + fpCTe.receb.enderReceb.nro;
    rllEnderecoRecebe2.Caption :=
      fpCTe.receb.enderReceb.xCpl + ' - ' + fpCTe.receb.enderReceb.xBairro;
    rllCEPReceb.Caption := FormatarCEP(fpCTe.receb.enderReceb.CEP);
    rllMunReceb.Caption := fpCTe.receb.enderReceb.xMun + ' - ' + fpCTe.receb.enderReceb.UF;
    rllCnpjReceb.Caption := FormatarCNPJouCPF(fpCTe.receb.CNPJCPF);
    rllPaisReceb.Caption := fpCTe.receb.enderReceb.xPais;
    rllInscEstReceb.Caption := fpCTe.receb.IE;
    rllFoneReceb.Caption := FormatarFone(fpCTe.receb.fone);
  end;

  rllProdPredominante.Caption := fpCTe.infCTeNorm.infCarga.proPred;
  rllOutrasCaracCarga.Caption := fpCTe.infCTeNorm.InfCarga.xOutCat;

  rlmQtdUnidMedida1.Lines.Clear;
  rlmQtdUnidMedida2.Lines.Clear;
  rlmQtdUnidMedida3.Lines.Clear;
  rlmQtdUnidMedida4.Lines.Clear;
  rlmQtdUnidMedida5.Lines.Clear;

  rllNomeSeguradora.Caption := '';
  rllRespSeguroMerc.Caption := '';
  rllNroApolice.Caption := '';
  rllNroAverbacao.Caption := '';

  rlmCompNome1.Lines.Clear;
  rlmCompNome2.Lines.Clear;
  rlmCompNome3.Lines.Clear;
  rlmCompValor1.Lines.Clear;
  rlmCompValor2.Lines.Clear;
  rlmCompValor3.Lines.Clear;

  for i := 0 to (fpCTe.infCTeNorm.InfCarga.InfQ.Count - 1) do
  begin
    //UnidMed = (uM3,uKG, uTON, uUNIDADE, uLITROS, uMMBTU);
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
        rlmQtdUnidMedida5.Lines.Add(fpCTe.infCTeNorm.InfCarga.InfQ.Items[i].tpMed);
        rlmQtdUnidMedida5.Lines.Add(
          FormatFloatBr(msk6x4, fpCTe.infCTeNorm.InfCarga.InfQ.Items[i].qCarga) + ' ' +
          UnidMedToDescricaoStr(fpCTe.infCTeNorm.InfCarga.InfQ.Items[i].cUnid));
      end;
    end;
  end;

  if fpCTe.infCTeNorm.seg.Count > 0 then
  begin
    rllNomeSeguradora.Caption := fpCTe.infCTeNorm.seg.Items[0].xSeg;
    rllRespSeguroMerc.Caption :=
      TpRspSeguroToStrText(fpCTe.infCTeNorm.seg.Items[0].respSeg);
    rllNroApolice.Caption := fpCTe.infCTeNorm.seg.Items[0].nApol;
    rllNroAverbacao.Caption := fpCTe.infCTeNorm.seg.Items[0].nAver;
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

//  rllSitTrib.Caption := ACBrStr(CSTICMSToStr(fpCTe.Imp.ICMS.SituTrib) + '-' +
//    CSTICMSToStrTagPosText(fpCTe.Imp.ICMS.SituTrib));

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
end;

procedure TfrmDACTerlRetratoA5.rlb_04_DadosNotaFiscalBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  inherited;
  PrintIt := rlCTe.PageNumber = 1;

  // Imprime os dados da da Nota Fiscal se o Tipo de CTe for Normal
  rlb_04_DadosNotaFiscal.Enabled := (fpCTe.Ide.tpCTe = tcNormal);

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

procedure TfrmDACTerlRetratoA5.rlb_05_ComplementoBeforePrint(Sender: TObject;
  var PrintIt: boolean);
var
  i: Integer;
begin
  inherited;
  PrintIt := rlCTe.PageNumber = 1;
  PrintIt := False;

  // Imprime a lista dos CT-e Complementados se o Tipo de CTe for Complemento
  rlmComplChave1.Lines.Clear;
  rlmComplChave2.Lines.Clear;
  rlb_05_Complemento.Enabled := (fpCTe.Ide.tpCTe = tcComplemento);

  if fpCTe.infCTe.versao <= 3 then
  begin
    rlmComplChave1.Lines.Add(fpCTe.InfCTeComp.Chave);
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

procedure TfrmDACTerlRetratoA5.rlb_06_ValorPrestacaoBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  inherited;
  PrintIt := rlCTe.PageNumber = 1;
end;

procedure TfrmDACTerlRetratoA5.rlb_07_HeaderItensBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  inherited;
  // Imprime os Documentos Originários se o Tipo de CTe for Normal
end;

procedure TfrmDACTerlRetratoA5.rlb_08_ItensBeforePrint(Sender: TObject;
  var PrintIt: boolean);
var
  i: integer;
begin
  inherited;

  // Imprime os Documentos Originários se o Tipo de CTe for Normal
  // TpcteTipoCTe = (tcNormal, tcComplemento, tcAnulacao, tcSubstituto);
  rlb_08_Itens.Enabled := (fpCTe.Ide.tpCTe = tcNormal);

  for i := 1 to 2 do
    if Trim(cdsDocumentos.FieldByName('DOCUMENTO_' + IntToStr(i)).AsString) = '' then
      TRLDBText(FindComponent('rldbtCnpjEmitente' + IntToStr(i))).Width := 290 //325
    else
      TRLDBText(FindComponent('rldbtCnpjEmitente' + IntToStr(i))).Width := 128;

  Inc(Linhas);

end;

procedure TfrmDACTerlRetratoA5.rlb_09_ObsBeforePrint(Sender: TObject;
  var PrintIt: boolean);
var
  i: integer;
begin
  inherited;
  PrintIt := rlCTe.PageNumber = 1;

  rlb_11_ModRodLot104.Enabled := False;

  with fpCTe.infCTeNorm.rodo do
  begin
    rllRntrcEmpresa.Caption := RNTRC;
    rlsCIOT.Enabled := True;
    lblCIOT.Enabled := True;
    rllCIOT.Enabled := True;
    rllCIOT.Caption := CIOT;

    case Lota of
      ltNao:
      begin
        rllLotacao.Caption := ACBrStr('NÃO');
      end;
      ltsim:
      begin
        rllLotacao.Caption := 'SIM';
        rlb_11_ModRodLot104.Enabled := True;
      end;
    end;

    rllDtPrevEntrega.Caption := FormatDateTime('DD/MM/YYYY', dPrev);
  end;

  rlmObs.Lines.BeginUpdate;
  rlmObs.Lines.Clear;

  rlmObs.Lines.Add(StringReplace(fpCTe.Compl.xObs, '&lt;BR&gt;',
    #13#10, [rfReplaceAll, rfIgnoreCase]));

  for i := 0 to fpCTe.Compl.ObsCont.Count - 1 do
    with fpCTe.Compl.ObsCont.Items[i] do
    begin
      rlmObs.Lines.Add(StringReplace(xCampo, '&lt;BR&gt;', #13#10,
        [rfReplaceAll, rfIgnoreCase]) + ': ' + StringReplace(
        xTexto, '&lt;BR&gt;', #13#10, [rfReplaceAll, rfIgnoreCase]));
    end;

  if fpCTe.Ide.tpEmis in [teContingencia, teFSDA] then
  begin
    if not (fpCTe.procCTe.cStat in [100, 101, 110]) then
      rlmObs.Lines.Add(ACBrStr(
        'DACTE em Contingência - Impresso em decorrência de problemas técnicos.'));
  end;

  if fpCTe.Ide.tpEmis = teDPEC then
    rlmObs.Lines.Add(ACBrStr(
      'DACTE em Contingência - DPEC regularmente recebida pela Receita Federal do Brasil'));

  rlmObs.Lines.Text := StringReplace(rlmObs.Lines.Text, ';', #13, [rfReplaceAll]);
  rlmObs.Lines.EndUpdate;

  rlmObsFisco.Lines.BeginUpdate;
  rlmObsFisco.Lines.Clear;

  for i := 0 to fpCTe.Compl.ObsFisco.Count - 1 do
    with fpCTe.Compl.ObsFisco.Items[i] do
    begin
      rlmObsFisco.Lines.Add(StringReplace(xCampo, '&lt;BR&gt;',
        #13#10, [rfReplaceAll, rfIgnoreCase]) + ': ' + StringReplace(
        xTexto, '&lt;BR&gt;', #13#10, [rfReplaceAll, rfIgnoreCase]));
    end;

  rlmObsFisco.Lines.Text := StringReplace(rlmObsFisco.Lines.Text,
    ';', #13, [rfReplaceAll]);
  rlmObsFisco.Lines.EndUpdate;

  rllMsgTeste.Visible := False;
  rllMsgTeste.Enabled := False;

  if fpCTe.Ide.tpAmb = taHomologacao then
  begin
    rllMsgTeste.Caption := ACBrStr('AMBIENTE DE HOMOLOGAÇÃO - SEM VALOR FISCAL');
    rllMsgTeste.Visible := True;
    rllMsgTeste.Enabled := True;
  end
  else
  begin
    if fpCTe.procCTe.cStat > 0 then
    begin
      if fpCTe.procCTe.cStat = 101 then
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
      rllMsgTeste.Caption := ACBrStr('CT-E NÃO ENVIADO PARA SEFAZ');
      rllMsgTeste.Visible := True;
      rllMsgTeste.Enabled := True;
    end;
  end;
  rllMsgTeste.Repaint;
end;

procedure TfrmDACTerlRetratoA5.rlb_11_ModRodLot104BeforePrint(Sender: TObject;
  var PrintIt: boolean);
var
  i: integer;
begin
  inherited;

  PrintIt := rlCTe.PageNumber = 1;

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
      rlmTipo2.Lines.Add('Tração')
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

procedure TfrmDACTerlRetratoA5.rlb_12_ModAereoBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  inherited;
  PrintIt := rlCTe.PageNumber = 1;
  rlb_12_ModAereo.Enabled := (fpCTe.Ide.tpCTe = tcNormal) and (fpCTe.Ide.modal = mdAereo);

  rllCaracAdServico.Caption := fpCTe.Compl.xCaracSer;
  rllCaracAdTransporte.Caption := fpCTe.Compl.xCaracAd;

  with fpCTe.infCTeNorm.aereo do
  begin
    rllAWB.Caption := nOCA;
    rllTarifaCL.Caption := tarifa.CL;
    rllTarifaCodigo.Caption := tarifa.cTar;
    rllTarifaValor.Caption := FormatCurr(',0.00', tarifa.vTar);
    rllMinuta.Caption := FormatFloat('0000000000', nMinu);

    rllLojaAgenteEmissor.Caption := xLAgEmi;
  end;

  if fpCTe.Ide.retira = rtSim then
    rllRetira.Caption := 'SIM'
  else
    rllRetira.Caption := ACBrStr('NÃO');
  rllDadosRetira.Caption := fpCTe.Ide.xdetretira;
end;

procedure TfrmDACTerlRetratoA5.rlb_13_ModAquaviarioBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  inherited;
  PrintIt := rlCTe.PageNumber = 1;
  rlb_13_ModAquaviario.Enabled :=
    (fpCTe.Ide.tpCTe = tcNormal) and (fpCTe.Ide.modal = mdAquaviario);

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
  end;
end;

procedure TfrmDACTerlRetratoA5.rlb_14_ModFerroviarioBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  inherited;
  PrintIt := rlCTe.PageNumber = 1;
  rlb_14_ModFerroviario.Enabled :=
    (fpCTe.Ide.tpCTe = tcNormal) and (fpCTe.Ide.modal = mdFerroviario);

end;

procedure TfrmDACTerlRetratoA5.rlb_15_ModDutoviarioBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  inherited;
  PrintIt := rlCTe.PageNumber = 1;
  rlb_15_ModDutoviario.Enabled :=
    (fpCTe.Ide.tpCTe = tcNormal) and (fpCTe.Ide.modal = mdDutoviario);

end;

procedure TfrmDACTerlRetratoA5.rlb_17_SistemaBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  inherited;
  PrintIt := rlCTe.PageNumber = 1;

  rllblSistema.Caption := fpDACTe.Sistema + ' - ' + fpDACTe.Usuario;
end;

procedure TfrmDACTerlRetratoA5.rlb_18_ReciboBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  inherited;
  PrintIt := (rlCTe.PageNumber = 1);

  rllSerie3.Caption := FormatFloat('000', fpCTe.Ide.serie);
  rllNumCte3.Caption := FormatFloat('000,000,000', fpCTe.Ide.nCT);

  // TpcteTipoCTe = (tcNormal, tcComplemento, tcAnulacao, tcSubstituto);
  if PrintIt then
  begin
    rlb_18_Recibo.Enabled := (fpCTe.Ide.tpCTe = tcNormal) and
      (fpCTe.Ide.modal <> mdAereo) and (fpDACTe.PosCanhoto = prRodape);
    if rlb_18_Recibo.Enabled then
      rlb_18_Recibo.Height := 68
    else
      rlb_18_Recibo.Height := 0;
  end;
end;

procedure TfrmDACTeRLRetratoA5.RLCTeBeforePrint(Sender: TObject; var PrintIt: boolean);
begin
  Itens;

  rlb_11_ModRodLot104.Height := 0;
  rlb_12_ModAereo.Height := 0;
  rlb_13_ModAquaviario.Height := 0;
  rlb_14_ModFerroviario.Height := 0;
  rlb_15_ModDutoviario.Height := 0;

  case fpCTe.Ide.modal of
    mdRodoviario:
    begin
      rlb_11_ModRodLot104.Height := 108;
    end;

    mdAereo:
    begin
      rlb_12_ModAereo.Height := 97;
    end;

    mdAquaviario:
    begin
      rlb_13_ModAquaviario.Height := 0;
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

  rlCTe.Title := 'CT-e: ' + FormatFloat('000,000,000', fpCTe.Ide.nCT);
end;

procedure TfrmDACTeRLRetratoA5.rlb_08_ItensAfterPrint(Sender: TObject);
begin
  inherited;
  if (Linhas > 3) and (not cdsDocumentos.EOF) then
  begin
    Linhas := 0;
    rlb_08_Itens.Height := 16;
    RLCTe.newpage;
  end;
end;

procedure TfrmDACTeRLRetratoA5.rlb_03_DadosRedespachoExpedidorBeforePrint(
  Sender: TObject; var PrintIt: boolean);
begin
  inherited;
  PrintIt := rlCTe.PageNumber = 1;
  // Imprime os dados da da Nota Fiscal se o Tipo de CTe for Normal
  rlb_03_DadosRedespachoExpedidor.Enabled :=
    (fpCTe.exped.xNome <> '') or (fpCTe.receb.xNome <> '');
end;

end.
