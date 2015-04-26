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


{******************************************************************************
|* Historico
|*
|* 16/02/2012: Italo Jurisato Junior
|* 01/04/2013: Thiago Filiano Rosa - Readaptação para A4 duas vias.
|* 18/12/2013: Italo Jurisato Junior
******************************************************************************}

{$I ACBr.inc}

unit ACBrCTeDACTeQRRetrato2Vias;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, QuickRpt, QRCtrls, XMLIntf, XMLDoc,
  JPEG, ACBrDFeQRCodeBar, pcnConversao, DB,
  {$IFDEF QReport_PDF}
     QRPDFFilt,
     // Incluido por Italo em 13/01/2011
     QRPrntr,
  {$ENDIF}
  DBClient, ACBrCTeDACTeQR;

type
  TfrmDACTeQRRetrato2Vias = class(TfrmDACTeQR)
    cdsDocumentos: TClientDataSet;
    cdsDocumentosTIPO_1: TStringField;
    cdsDocumentosCNPJCPF_1: TStringField;
    cdsDocumentosDOCUMENTO_1: TStringField;
    cdsDocumentosTIPO_2: TStringField;
    cdsDocumentosCNPJCPF_2: TStringField;
    cdsDocumentosDOCUMENTO_2: TStringField;
    QRLabel13: TQRLabel;
    QRLabel16: TQRLabel;
    QRLabel22: TQRLabel;
    QRLabel24: TQRLabel;
    qrlRazaoRemet: TQRLabel;
    qrlEnderecoRemet1: TQRLabel;
    qrlMunRemet: TQRLabel;
    qrlCnpjRemet: TQRLabel;
    QRLabel27: TQRLabel;
    QRLabel30: TQRLabel;
    QRLabel31: TQRLabel;
    QRLabel32: TQRLabel;
    qrlRazaoDest: TQRLabel;
    qrlEnderecoDest1: TQRLabel;
    QRLabel93: TQRLabel;
    qrlInscEstRemet: TQRLabel;
    QRLabel95: TQRLabel;
    qrlFoneRemet: TQRLabel;
    qrlCEPRemet: TQRLabel;
    QRLabel98: TQRLabel;
    qrlMunDest: TQRLabel;
    qrlCnpjDest: TQRLabel;
    QRLabel114: TQRLabel;
    qrlInscEstDest: TQRLabel;
    QRLabel116: TQRLabel;
    qrlFoneDest: TQRLabel;
    qrlCEPDest: TQRLabel;
    QRLabel119: TQRLabel;
    qrb_01_DadosDACTe: TQRBand;
    qrlFormaPagamento: TQRLabel;
    QRLabel78: TQRLabel;
    qrlTomaServico: TQRLabel;
    QRLabel28: TQRLabel;
    qrlRazaoExped: TQRLabel;
    qrlMunExped: TQRLabel;
    qrlInscEstExped: TQRLabel;
    qrlFoneExped: TQRLabel;
    qrlEnderecoExped1: TQRLabel;
    qrlCnpjExped: TQRLabel;
    qrlCEPExped: TQRLabel;
    QRLabel89: TQRLabel;
    QRLabel88: TQRLabel;
    QRLabel87: TQRLabel;
    QRLabel86: TQRLabel;
    QRLabel110: TQRLabel;
    QRLabel107: TQRLabel;
    QRLabel105: TQRLabel;
    qrlRazaoReceb: TQRLabel;
    qrlMunReceb: TQRLabel;
    qrlInscEstReceb: TQRLabel;
    qrlFoneReceb: TQRLabel;
    qrlEnderecoReceb1: TQRLabel;
    qrlCnpjReceb: TQRLabel;
    qrlCEPReceb: TQRLabel;
    QRLabel99: TQRLabel;
    QRLabel128: TQRLabel;
    QRLabel125: TQRLabel;
    QRLabel123: TQRLabel;
    QRLabel103: TQRLabel;
    QRLabel102: TQRLabel;
    QRLabel101: TQRLabel;
    qrlRazaoToma: TQRLabel;
    qrlInscEstToma: TQRLabel;
    qrlFoneToma: TQRLabel;
    qrlCnpjToma: TQRLabel;
    qrlCEPToma: TQRLabel;
    QRLabel97: TQRLabel;
    QRLabel94: TQRLabel;
    QRLabel82: TQRLabel;
    QRLabel81: TQRLabel;
    QRLabel80: TQRLabel;
    QRLabel111: TQRLabel;
    QRLabel108: TQRLabel;
    qrlEnderecoToma: TQRLabel;
    qrlMunToma: TQRLabel;
    QRShape14: TQRShape;
    qrmCompValor1: TQRMemo;
    qrmCompNome1: TQRMemo;
    QRLabel38: TQRLabel;
    QRLabel3: TQRLabel;
    QRShape31: TQRShape;
    qrlProdPredominante: TQRLabel;
    QRLabel1: TQRLabel;
    qrlVlrTotalMerc: TQRLabel;
    QRLabel34: TQRLabel;
    QRLabel43: TQRLabel;
    qrmQtdUnidMedida5: TQRLabel;
    QRLabel73: TQRLabel;
    qrmQtdUnidMedida4: TQRLabel;
    QRLabel35: TQRLabel;
    qrmQtdUnidMedida1: TQRLabel;
    QRLabel36: TQRLabel;
    qrmQtdUnidMedida2: TQRLabel;
    QRLabel41: TQRLabel;
    qrmQtdUnidMedida3: TQRLabel;
    QRLabel52: TQRLabel;
    qrlSitTrib: TQRLabel;
    QRLabel55: TQRLabel;
    qrlBaseCalc: TQRLabel;
    QRLabel56: TQRLabel;
    qrlAliqICMS: TQRLabel;
    QRLabel54: TQRLabel;
    qrlVlrICMS: TQRLabel;
    QRLabel7: TQRLabel;
    qrmObs1: TQRMemo;
    QRLabel10: TQRLabel;
    qrmmTipoDoc: TQRMemo;
    qrmmCNPJ: TQRMemo;
    qrmmChaveNFe: TQRMemo;
    QRLabel91: TQRLabel;
    QRLabel92: TQRLabel;
    QRLabel96: TQRLabel;
    qrlDtPrevEntrega: TQRLabel;
    QRLabel84: TQRLabel;
    QRLabel9: TQRLabel;
    QRLabel186: TQRLabel;
    QRLabel187: TQRLabel;
    qrlNumCTe3: TQRLabel;
    QRLabel190: TQRLabel;
    qrlMsgTeste: TQRLabel;
    QRLabel49: TQRLabel;
    QRLabel50: TQRLabel;
    qrlVlrTotReceber: TQRLabel;
    qrlVlrTotServico: TQRLabel;
    QRLabel188: TQRLabel;
    qrlFilial: TQRLabel;
    qriLogo: TQRImage;
    qriBarCode: TQRImage;
    qrmEmitente: TQRMemo;
    qrmDadosEmitente: TQRMemo;
    QRLabel17: TQRLabel;
    QRLabel18: TQRLabel;
    QRLabel6: TQRLabel;
    qrlModal: TQRLabel;
    QRLabel8: TQRLabel;
    qrlModelo: TQRLabel;
    QRLabel21: TQRLabel;
    qrlSerie: TQRLabel;
    QRLabel23: TQRLabel;
    qrlNumCte: TQRLabel;
    QRLabel25: TQRLabel;
    qrlPageNumber: TQRLabel;
    QRLabel33: TQRLabel;
    qrlEmissao: TQRLabel;
    qrlChave: TQRLabel;
    QRLabel2: TQRLabel;
    qrlTipoCte: TQRLabel;
    qrlDescricao: TQRLabel;
    qrlProtocolo: TQRLabel;
    QRShape4: TQRShape;
    QRShape6: TQRShape;
    QRShape7: TQRShape;
    QRShape8: TQRShape;
    QRShape9: TQRShape;
    QRShape15: TQRShape;
    QRShape2: TQRShape;
    QRShape5: TQRShape;
    QRShape12: TQRShape;
    QRShape13: TQRShape;
    QRShape18: TQRShape;
    QRShape16: TQRShape;
    QRShape19: TQRShape;
    QRShape21: TQRShape;
    QRShape22: TQRShape;
    QRShape23: TQRShape;
    QRShape24: TQRShape;
    QRShape25: TQRShape;
    QRShape26: TQRShape;
    QRShape17: TQRShape;
    QRShape20: TQRShape;
    qrlNatOperacao: TQRLabel;
    QRLabel29: TQRLabel;
    qrlOrigPrestacao: TQRLabel;
    QRLabel12: TQRLabel;
    QRShape28: TQRShape;
    qrlDestPrestacao: TQRLabel;
    QRLabel14: TQRLabel;
    QRShape29: TQRShape;
    QRLabel183: TQRLabel;
    qrlPlaca: TQRLabel;
    QRLabel11: TQRLabel;
    qrlRNTRC: TQRLabel;
    QRShape32: TQRShape;
    QRShape33: TQRShape;
    QRShape34: TQRShape;
    QRShape35: TQRShape;
    QRShape36: TQRShape;
    qrsTraco: TQRShape;
    qrlURLEmitente: TQRMemo;
    QRLabel83: TQRLabel;
    QRLabel85: TQRLabel;
    qrlCnpjEmitente: TQRLabel;
    qrlInscEstEmitente: TQRLabel;
    QRShape54: TQRShape;
    qrlVariavel1: TQRLabel;
    QRShape1: TQRShape;
    QRShape10: TQRShape;
    qrlSistema: TQRLabel;
    QRLabel5: TQRLabel;
    qriBarControle: TQRImage;
    qrmObs2: TQRMemo;
    QRLabel15: TQRLabel;
    qrlEmitidoPor: TQRLabel;
    qrb_02_Itens: TQRChildBand;
    QRShape11: TQRShape;
    qriLogoDetalhe: TQRImage;
    qriBarCode_Compl: TQRImage;
    qrmEmitente_Compl: TQRMemo;
    qrmDadosEmitente_Compl: TQRMemo;
    QRLabel37: TQRLabel;
    qrlModal_Compl: TQRLabel;
    QRLabel40: TQRLabel;
    qrlModelo_Compl: TQRLabel;
    QRLabel44: TQRLabel;
    qrlSerie_Compl: TQRLabel;
    QRLabel46: TQRLabel;
    qrlNumCte_Compl: TQRLabel;
    QRLabel48: TQRLabel;
    qrlPageNumber_Compl: TQRLabel;
    QRLabel53: TQRLabel;
    qrlEmissao_Compl: TQRLabel;
    qrlChave_Compl: TQRLabel;
    QRLabel61: TQRLabel;
    qrlProtocolo_Compl: TQRLabel;
    QRShape27: TQRShape;
    QRShape38: TQRShape;
    QRShape39: TQRShape;
    QRShape40: TQRShape;
    QRShape41: TQRShape;
    QRShape42: TQRShape;
    QRShape44: TQRShape;
    QRShape45: TQRShape;
    qrlURLEmitente_Compl: TQRMemo;
    QRLabel79: TQRLabel;
    QRLabel104: TQRLabel;
    QRShape53: TQRShape;
    QRLabel106: TQRLabel;
    QRLabel4: TQRLabel;
    qrlTipoCte_Compl: TQRLabel;
    QRShape3: TQRShape;
    QRShape30: TQRShape;
    QRShape43: TQRShape;
    qrlNatOperacao_Compl: TQRLabel;
    QRLabel20: TQRLabel;
    qrlOrigPrestacao_Compl: TQRLabel;
    QRLabel59: TQRLabel;
    QRShape46: TQRShape;
    qrlDestPrestacao_Compl: TQRLabel;
    QRLabel63: TQRLabel;
    QRShape47: TQRShape;
    QRLabel64: TQRLabel;
    qrlPlaca_Compl: TQRLabel;
    QRLabel66: TQRLabel;
    qrlRNTRC_Compl: TQRLabel;
    QRLabel68: TQRLabel;
    QRLabel69: TQRLabel;
    qrlCnpjEmitente_Compl: TQRLabel;
    qrlInscEstEmitente_Compl: TQRLabel;
    QRLabel72: TQRLabel;
    QRShape48: TQRShape;
    QRLabel74: TQRLabel;
    qrmmTipoDoc_Compl: TQRMemo;
    qrmmCNPJ_Compl: TQRMemo;
    QRLabel75: TQRLabel;
    QRLabel76: TQRLabel;
    qrmmChaveNFe_Compl: TQRMemo;
    QRLabel19: TQRLabel;
    qrlEmitidoPor_Compl: TQRLabel;
    qsTracoDetalhe: TQRShape;
    QRLabel26: TQRLabel;
    qrlVlrICMSST: TQRLabel;
    QRLabel39: TQRLabel;
    qrlVlrCREDPRES: TQRLabel;
    QRLabel172: TQRLabel;
    qrlCPFMotorista2: TQRLabel;
    qrlNomeMotorista2: TQRLabel;
    QRLabel173: TQRLabel;
    QRLabel42: TQRLabel;
    qrlPLCEquip: TQRLabel;
    qrb_03_Itens: TQRChildBand;
    QRLabel57: TQRLabel;
    QRLabel58: TQRLabel;
    QRLabel60: TQRLabel;
    qrmmTipoDoc_Compl2: TQRMemo;
    qrmmCNPJ_Compl2: TQRMemo;
    qrmmChaveNFe_Compl2: TQRMemo;
    qrmmTipoDoc_Compl3: TQRMemo;
    qrmmCNPJ_Compl3: TQRMemo;
    qrmmChaveNFe_Compl3: TQRMemo;
    QRLabel45: TQRLabel;
    QRLabel47: TQRLabel;
    QRLabel51: TQRLabel;
    QRLabel62: TQRLabel;
    QRLabel65: TQRLabel;
    QRShape37: TQRShape;
    procedure QRCTeBeforePrint(Sender: TCustomQuickRep; var PrintReport: Boolean);
    procedure qrb_01_DadosDACTeBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_02_ItensBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_01_ReciboBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_02_CabecalhoBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_04_DadosNotaFiscalBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_05_ComplementoBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_06_ValorPrestacaoBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_07_HeaderItensBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_08_ItensBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_09_ObsBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_17_SistemaBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_12_ModAereoBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_13_ModAquaviarioBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_14_ModFerroviarioBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_15_ModDutoviarioBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_01_Recibo_AereoBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_11_ModRodLot104BeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_18_ReciboBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure qrb_03_ItensBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure QRChildBand1BeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
  private
    FTotalPages: integer;
    procedure Itens;
    function AllTrim(sTexto: string): string;
  public
    procedure ProtocoloCTe(const sProtocolo: string);
  end;

implementation

uses
  StrUtils, ACBrDFeUtil, DateUtils, ACBrValidador, ACBrCTe, pcteConversaoCTe;

{$R *.dfm}

const
  // Alterado por Italo em 13/04/2012
  _NUM_ITEMS_PAGE1      = 13; //esse valor eh dobrado por ter 2 NF por linha
//  _NUM_ITEMS_PAGE1      = 26; //esse valor eh dobrado por ter 2 NF por linha
  _NUM_ITEMS_OTHERPAGES = 43;

var
  FProtocoloCTe : string;
  nItemControle : Integer;
  Fracionado    : Integer;
  Versao        : Integer;

function TfrmDACTeQRRetrato2Vias.AllTrim(sTexto: string): string;
begin
  while (Copy(sTexto, 1, 1) = ' ')
    and (Length(sTexto) > 0) do
    Delete(sTexto, 1, 1);

  while (Copy(sTexto, Length(sTexto), 1) = ' ')
    and (Length(sTexto) > 0) do
    Delete(sTexto, Length(sTexto), 1);

  AllTrim := sTexto;
end;

procedure TfrmDACTeQRRetrato2Vias.Itens;
var
  I, J, K, Item, count1 : Integer;
  sChave: string;
begin
  if QRCTe.PageNumber > 0 then
    exit;

  if not cdsDocumentos.Active then
    cdsDocumentos.CreateDataSet;

  cdsDocumentos.EmptyDataSet;

  cdsDocumentos.Open;
  Item := 0;
{$IFDEF PL_200}
  //Varrendo NF comum
  for I := 0 to (FCTe.infCTeNorm.infDoc.InfNF.Count - 1) do
  begin
    with FCTe.infCTeNorm.infDoc.InfNF.Items[I] do
    begin
      {if (Item mod 2) = 0 then
      begin}
        cdsDocumentos.Append;

        cdsDocumentosTIPO_1.AsString := 'NF';
        cdsDocumentosCNPJCPF_1.AsString := FormatarCNPJ(FCTe.Rem.CNPJCPF);
        cdsDocumentosDOCUMENTO_1.AsString := serie + '-' + nDoc;
      {end
      else
      begin
        cdsDocumentosTIPO_2.AsString := 'NF';
        cdsDocumentosCNPJCPF_2.AsString := FormatarCNPJ(FCTe.Rem.CNPJCPF);
        cdsDocumentosDOCUMENTO_2.AsString := serie + '-' + nDoc;

        cdsDocumentos.Post;
      end;}
      inc(Item);
    end;
  end;
  //Varrendo NFe
  for I := 0 to (FCTe.infCTeNorm.infDoc.InfNFE.Count - 1) do
  begin
    with FCTe.infCTeNorm.infDoc.InfNFE.Items[I] do
    begin
      {if (Item mod 2) = 0 then
      begin}
        cdsDocumentos.Append;
        cdsDocumentosTIPO_1.AsString := 'NF-E';
        sChave := FormatarChaveAcesso(chave);

        cdsDocumentosCNPJCPF_1.AsString := Copy(sChave, 1, 22);
        cdsDocumentosDOCUMENTO_1.AsString := Copy(sChave, 23, 39);
      {end
      else
      begin
        cdsDocumentosTIPO_2.AsString := 'NF-E';
        cdsDocumentosCNPJCPF_2.AsString := CTeUtil.FormatarChaveAcesso(chave, True);
        cdsDocumentos.Post;
      end;}
      inc(Item);
    end;
  end;
  //Varrendo Outros
  for I := 0 to (FCTe.infCTeNorm.infDoc.InfOutros.Count - 1) do
  begin
    with FCTe.infCTeNorm.infDoc.InfOutros.Items[I] do
    begin
      {if (Item mod 2) = 0 then
      begin}
        cdsDocumentos.Append;
        // Alterado por Italo em 18/04/2012
        // TpcteTipoDocumento = (tdDeclaracao, tdDutoviario, tdOutros);
        case tpDoc of
         tdDeclaracao: begin
                        cdsDocumentosTIPO_1.AsString      := 'DECLAR';
                        cdsDocumentosCNPJCPF_1.AsString   := FormatarCNPJ(FCTe.Rem.CNPJCPF);
                        cdsDocumentosDOCUMENTO_1.AsString := 'Declaração Doc.: ' + nDoc;
                       end;
         tdDutoviario: begin
                        cdsDocumentosTIPO_1.AsString      := 'DUTO';
                        cdsDocumentosCNPJCPF_1.AsString   := FormatarCNPJ(FCTe.Rem.CNPJCPF);
                        cdsDocumentosDOCUMENTO_1.AsString := 'Dutoviário Doc.: ' + nDoc;
                       end;
         tdOutros:     begin
                        cdsDocumentosTIPO_1.AsString      := 'Outros';
                        cdsDocumentosCNPJCPF_1.AsString   := FormatarCNPJ(FCTe.Rem.CNPJCPF);
                        cdsDocumentosDOCUMENTO_1.AsString := copy( trim(descOutros), 1, 20 ) + ' Doc.: '+ nDoc;
                       end;
        end;
//        cdsDocumentosTIPO_1.AsString := descOutros;
//        cdsDocumentosCNPJCPF_1.AsString := FormatarCNPJ(FCTe.Rem.CNPJCPF);
      {end
      else
      begin
        // Alterado por Italo em 18/04/2012
        // TpcteTipoDocumento = (tdDeclaracao, tdDutoviario, tdOutros);
        case tpDoc of
         tdDeclaracao: begin
                        cdsDocumentosTIPO_2.AsString      := 'DECLAR';
                        cdsDocumentosCNPJCPF_2.AsString   := FormatarCNPJ(FCTe.Rem.CNPJCPF);
                        cdsDocumentosDOCUMENTO_2.AsString := 'Declaração Doc.: ' + nDoc;
                       end;
         tdDutoviario: begin
                        cdsDocumentosTIPO_2.AsString      := 'DUTO';
                        cdsDocumentosCNPJCPF_2.AsString   := FormatarCNPJ(FCTe.Rem.CNPJCPF);
                        cdsDocumentosDOCUMENTO_2.AsString := 'Dutoviário Doc.: ' + nDoc;
                       end;
         tdOutros:     begin
                        cdsDocumentosTIPO_2.AsString      := 'Outros';
                        cdsDocumentosCNPJCPF_2.AsString   := FormatarCNPJ(FCTe.Rem.CNPJCPF);
                        cdsDocumentosDOCUMENTO_2.AsString := copy( trim(descOutros), 1, 20 ) + ' Doc.: '+ nDoc;
                       end;
        end;
//        cdsDocumentosTIPO_2.AsString := descOutros;
//        cdsDocumentosCNPJCPF_2.AsString := FormatarCNPJ(FCTe.Rem.CNPJCPF);
        cdsDocumentos.Post;
      end;}
      inc(Item);
    end;
  end;
 //Varrendo Documentos de Transporte anterior
 // Incluido / Alterado por Italo em 13/12/2010
  for I := 0 to (FCTe.infCTeNorm.docAnt.emiDocAnt.Count - 1) do
  begin
    // Em Papel
    // Alterado por Italo em 27/12/2010
    for J := 0 to (FCTe.infCTeNorm.docAnt.emiDocAnt.Items[I].idDocAnt.Count - 1) do
    begin
      for K := 0 to (FCTe.infCTeNorm.docAnt.emiDocAnt.Items[I].idDocAnt.Items[J].idDocAntPap.Count - 1) do
      begin
        with FCTe.infCTeNorm.docAnt.emiDocAnt.Items[I].idDocAnt.Items[J].idDocAntPap.Items[K] do
        begin
          {if (Item mod 2) = 0 then
          begin}
            cdsDocumentos.Append;

            case tpDoc of
             daCTRC: cdsDocumentosTIPO_1.AsString := 'CTRC';
             daCTAC: cdsDocumentosTIPO_1.AsString := 'CTAC';
             daACT:  cdsDocumentosTIPO_1.AsString := 'ACT';
             daNF7:  cdsDocumentosTIPO_1.AsString := 'NF M7';
             daNF27: cdsDocumentosTIPO_1.AsString := 'NF M27';
             daCAN:  cdsDocumentosTIPO_1.AsString := 'CAN';
             daCTMC: cdsDocumentosTIPO_1.AsString := 'CTMC';
             daATRE: cdsDocumentosTIPO_1.AsString := 'ATRE';
             daDTA:  cdsDocumentosTIPO_1.AsString := 'DTA';
             daCAI:  cdsDocumentosTIPO_1.AsString := 'CAI';
             daCCPI: cdsDocumentosTIPO_1.AsString := 'CCPI';
             daCA:   cdsDocumentosTIPO_1.AsString := 'CA';
             daTIF:  cdsDocumentosTIPO_1.AsString := 'TIF';
             daOutros: cdsDocumentosTIPO_1.AsString := 'Outros';
            end;
            cdsDocumentosCNPJCPF_1.AsString := FormatarCNPJ(FCTe.infCTeNorm.docAnt.emiDocAnt.Items[I].CNPJCPF);
            cdsDocumentosDOCUMENTO_1.AsString := serie + '-' + IntToStr(nDoc);
          {end
          else
          begin
            case tpDoc of
             daCTRC: cdsDocumentosTIPO_2.AsString := 'CTRC';
             daCTAC: cdsDocumentosTIPO_2.AsString := 'CTAC';
             daACT:  cdsDocumentosTIPO_2.AsString := 'ACT';
             daNF7:  cdsDocumentosTIPO_2.AsString := 'NF M7';
             daNF27: cdsDocumentosTIPO_2.AsString := 'NF M27';
             daCAN:  cdsDocumentosTIPO_2.AsString := 'CAN';
             daCTMC: cdsDocumentosTIPO_2.AsString := 'CTMC';
             daATRE: cdsDocumentosTIPO_2.AsString := 'ATRE';
             daDTA:  cdsDocumentosTIPO_2.AsString := 'DTA';
             daCAI:  cdsDocumentosTIPO_2.AsString := 'CAI';
             daCCPI: cdsDocumentosTIPO_2.AsString := 'CCPI';
             daCA:   cdsDocumentosTIPO_2.AsString := 'CA';
             daTIF:  cdsDocumentosTIPO_2.AsString := 'TIF';
             daOutros: cdsDocumentosTIPO_2.AsString := 'Outros';
            end;
            cdsDocumentosCNPJCPF_2.AsString := FormatarCNPJ(FCTe.infCTeNorm.emiDocAnt.Items[I].CNPJCPF);
            cdsDocumentosDOCUMENTO_2.AsString := serie + '-' + IntToStr(nDoc);

            cdsDocumentos.Post;
          end;}
          inc(Item);
        end;
      end;
    end;

    // Eletrônico
    // Alterado por Italo em 27/12/2010
    for J := 0 to (FCTe.infCTeNorm.docAnt.emiDocAnt.Items[I].idDocAnt.Count - 1) do
    begin
      for K := 0 to (FCTe.infCTeNorm.docAnt.emiDocAnt.Items[I].idDocAnt.Items[J].idDocAntEle.Count - 1) do
      begin
        with FCTe.infCTeNorm.docAnt.emiDocAnt.Items[I].idDocAnt.Items[J].idDocAntEle.Items[K] do
        begin
          {if (Item mod 2) = 0 then
          begin}
            cdsDocumentos.Append;

            cdsDocumentosTIPO_1.AsString := 'CT-E';
            // Alterado por Italo em 17/05/2011
            sChave := FormatarChaveAcesso(chave);
            cdsDocumentosCNPJCPF_1.AsString := Copy(sChave, 1, 22);
            cdsDocumentosDOCUMENTO_1.AsString := Copy(sChave, 23, 50);
          {end
          else
          begin
            cdsDocumentosTIPO_2.AsString := 'CT-E';
            // Alterado por Italo em 17/05/2011
            cdsDocumentosCNPJCPF_2.AsString := CTeUtil.FormatarChaveAcesso(chave, True);

            cdsDocumentos.Post;
          end;}
          inc(Item);
        end;
      end;
    end;

  end;
{$ELSE}
for I := 0 to (FCTe.Rem.InfNF.Count - 1) do
  begin
    with FCTe.Rem.InfNF.Items[I] do
    begin
      {if (Item mod 2) = 0 then
      begin}
        cdsDocumentos.Append;

        cdsDocumentosTIPO_1.AsString := 'NF';
        cdsDocumentosCNPJCPF_1.AsString := FormatarCNPJ(FCTe.Rem.CNPJCPF);
        cdsDocumentosDOCUMENTO_1.AsString := serie + '-' + nDoc;
      {end
      else
      begin
        cdsDocumentosTIPO_2.AsString := 'NF';
        cdsDocumentosCNPJCPF_2.AsString := FormatarCNPJ(FCTe.Rem.CNPJCPF);
        cdsDocumentosDOCUMENTO_2.AsString := serie + '-' + nDoc;

        cdsDocumentos.Post;
      end;}
      inc(Item);
    end;
  end;
  //Varrendo NFe
  for I := 0 to (FCTe.Rem.InfNFE.Count - 1) do
  begin
    with FCTe.Rem.InfNFE.Items[I] do
    begin
      {if (Item mod 2) = 0 then
      begin}
        cdsDocumentos.Append;
        cdsDocumentosTIPO_1.AsString := 'NF-E';
        sChave := CTeUtil.FormatarChaveAcesso(chave, True);

        cdsDocumentosCNPJCPF_1.AsString := Copy(sChave, 1, 22);
        cdsDocumentosDOCUMENTO_1.AsString := Copy(sChave, 23, 39);
      {end
      else
      begin
        cdsDocumentosTIPO_2.AsString := 'NF-E';
        cdsDocumentosCNPJCPF_2.AsString := CTeUtil.FormatarChaveAcesso(chave, True);
        cdsDocumentos.Post;
      end;}
      inc(Item);
    end;
  end;
  //Varrendo Outros
  for I := 0 to (FCTe.Rem.InfOutros.Count - 1) do
  begin
    with FCTe.Rem.InfOutros.Items[I] do
    begin
      {if (Item mod 2) = 0 then
      begin}
        cdsDocumentos.Append;
        // Alterado por Italo em 18/04/2012
        // TpcteTipoDocumento = (tdDeclaracao, tdDutoviario, tdOutros);
        case tpDoc of
         tdDeclaracao: begin
                        cdsDocumentosTIPO_1.AsString      := 'DECLAR';
                        cdsDocumentosCNPJCPF_1.AsString   := FormatarCNPJ(FCTe.Rem.CNPJCPF);
                        cdsDocumentosDOCUMENTO_1.AsString := 'Declaração Doc.: ' + nDoc;
                       end;
         tdDutoviario: begin
                        cdsDocumentosTIPO_1.AsString      := 'DUTO';
                        cdsDocumentosCNPJCPF_1.AsString   := FormatarCNPJ(FCTe.Rem.CNPJCPF);
                        cdsDocumentosDOCUMENTO_1.AsString := 'Dutoviário Doc.: ' + nDoc;
                       end;
         tdOutros:     begin
                        cdsDocumentosTIPO_1.AsString      := 'Outros';
                        cdsDocumentosCNPJCPF_1.AsString   := FormatarCNPJ(FCTe.Rem.CNPJCPF);
                        cdsDocumentosDOCUMENTO_1.AsString := copy( trim(descOutros), 1, 20 ) + ' Doc.: '+ nDoc;
                       end;
        end;
//        cdsDocumentosTIPO_1.AsString := descOutros;
//        cdsDocumentosCNPJCPF_1.AsString := FormatarCNPJ(FCTe.Rem.CNPJCPF);
      {end
      else
      begin
        // Alterado por Italo em 18/04/2012
        // TpcteTipoDocumento = (tdDeclaracao, tdDutoviario, tdOutros);
        case tpDoc of
         tdDeclaracao: begin
                        cdsDocumentosTIPO_2.AsString      := 'DECLAR';
                        cdsDocumentosCNPJCPF_2.AsString   := FormatarCNPJ(FCTe.Rem.CNPJCPF);
                        cdsDocumentosDOCUMENTO_2.AsString := 'Declaração Doc.: ' + nDoc;
                       end;
         tdDutoviario: begin
                        cdsDocumentosTIPO_2.AsString      := 'DUTO';
                        cdsDocumentosCNPJCPF_2.AsString   := FormatarCNPJ(FCTe.Rem.CNPJCPF);
                        cdsDocumentosDOCUMENTO_2.AsString := 'Dutoviário Doc.: ' + nDoc;
                       end;
         tdOutros:     begin
                        cdsDocumentosTIPO_2.AsString      := 'Outros';
                        cdsDocumentosCNPJCPF_2.AsString   := FormatarCNPJ(FCTe.Rem.CNPJCPF);
                        cdsDocumentosDOCUMENTO_2.AsString := copy( trim(descOutros), 1, 20 ) + ' Doc.: '+ nDoc;
                       end;
        end;
//        cdsDocumentosTIPO_2.AsString := descOutros;
//        cdsDocumentosCNPJCPF_2.AsString := FormatarCNPJ(FCTe.Rem.CNPJCPF);
        cdsDocumentos.Post;
      end;}
      inc(Item);
    end;
  end;
 //Varrendo Documentos de Transporte anterior
 // Incluido / Alterado por Italo em 13/12/2010
  for I := 0 to (FCTe.infCTeNorm.emiDocAnt.Count - 1) do
  begin
    // Em Papel
    // Alterado por Italo em 27/12/2010
    for J := 0 to (FCTe.infCTeNorm.emiDocAnt.Items[I].idDocAnt.Count - 1) do
    begin
      for K := 0 to (FCTe.infCTeNorm.emiDocAnt.Items[I].idDocAnt.Items[J].idDocAntPap.Count - 1) do
      begin
        with FCTe.infCTeNorm.emiDocAnt.Items[I].idDocAnt.Items[J].idDocAntPap.Items[K] do
        begin
          {if (Item mod 2) = 0 then
          begin}
            cdsDocumentos.Append;

            case tpDoc of
             daCTRC: cdsDocumentosTIPO_1.AsString := 'CTRC';
             daCTAC: cdsDocumentosTIPO_1.AsString := 'CTAC';
             daACT:  cdsDocumentosTIPO_1.AsString := 'ACT';
             daNF7:  cdsDocumentosTIPO_1.AsString := 'NF M7';
             daNF27: cdsDocumentosTIPO_1.AsString := 'NF M27';
             daCAN:  cdsDocumentosTIPO_1.AsString := 'CAN';
             daCTMC: cdsDocumentosTIPO_1.AsString := 'CTMC';
             daATRE: cdsDocumentosTIPO_1.AsString := 'ATRE';
             daDTA:  cdsDocumentosTIPO_1.AsString := 'DTA';
             daCAI:  cdsDocumentosTIPO_1.AsString := 'CAI';
             daCCPI: cdsDocumentosTIPO_1.AsString := 'CCPI';
             daCA:   cdsDocumentosTIPO_1.AsString := 'CA';
             daTIF:  cdsDocumentosTIPO_1.AsString := 'TIF';
             daOutros: cdsDocumentosTIPO_1.AsString := 'Outros';
            end;
            cdsDocumentosCNPJCPF_1.AsString := FormatarCNPJ(FCTe.infCTeNorm.emiDocAnt.Items[I].CNPJCPF);
            cdsDocumentosDOCUMENTO_1.AsString := serie + '-' + IntToStr(nDoc);
          {end
          else
          begin
            case tpDoc of
             daCTRC: cdsDocumentosTIPO_2.AsString := 'CTRC';
             daCTAC: cdsDocumentosTIPO_2.AsString := 'CTAC';
             daACT:  cdsDocumentosTIPO_2.AsString := 'ACT';
             daNF7:  cdsDocumentosTIPO_2.AsString := 'NF M7';
             daNF27: cdsDocumentosTIPO_2.AsString := 'NF M27';
             daCAN:  cdsDocumentosTIPO_2.AsString := 'CAN';
             daCTMC: cdsDocumentosTIPO_2.AsString := 'CTMC';
             daATRE: cdsDocumentosTIPO_2.AsString := 'ATRE';
             daDTA:  cdsDocumentosTIPO_2.AsString := 'DTA';
             daCAI:  cdsDocumentosTIPO_2.AsString := 'CAI';
             daCCPI: cdsDocumentosTIPO_2.AsString := 'CCPI';
             daCA:   cdsDocumentosTIPO_2.AsString := 'CA';
             daTIF:  cdsDocumentosTIPO_2.AsString := 'TIF';
             daOutros: cdsDocumentosTIPO_2.AsString := 'Outros';
            end;
            cdsDocumentosCNPJCPF_2.AsString := FormatarCNPJ(FCTe.infCTeNorm.emiDocAnt.Items[I].CNPJCPF);
            cdsDocumentosDOCUMENTO_2.AsString := serie + '-' + IntToStr(nDoc);

            cdsDocumentos.Post;
          end;}
          inc(Item);
        end;
      end;
    end;

    // Eletrônico
    // Alterado por Italo em 27/12/2010
    for J := 0 to (FCTe.infCTeNorm.emiDocAnt.Items[I].idDocAnt.Count - 1) do
    begin
      for K := 0 to (FCTe.infCTeNorm.emiDocAnt.Items[I].idDocAnt.Items[J].idDocAntEle.Count - 1) do
      begin
        with FCTe.infCTeNorm.emiDocAnt.Items[I].idDocAnt.Items[J].idDocAntEle.Items[K] do
        begin
          {if (Item mod 2) = 0 then
          begin}
            cdsDocumentos.Append;

            cdsDocumentosTIPO_1.AsString := 'CT-E';
            // Alterado por Italo em 17/05/2011
            sChave := CTeUtil.FormatarChaveAcesso(chave, True);
            cdsDocumentosCNPJCPF_1.AsString := Copy(sChave, 1, 22);
            cdsDocumentosDOCUMENTO_1.AsString := Copy(sChave, 23, 50);
          {end
          else
          begin
            cdsDocumentosTIPO_2.AsString := 'CT-E';
            // Alterado por Italo em 17/05/2011
            cdsDocumentosCNPJCPF_2.AsString := CTeUtil.FormatarChaveAcesso(chave, True);

            cdsDocumentos.Post;
          end;}
          inc(Item);
        end;
      end;
    end;

  end;
{$ENDIF}

  cdsDocumentos.First;
end;

procedure TfrmDACTeQRRetrato2Vias.ProtocoloCTe(const sProtocolo: string);
begin
  FProtocoloCTe := sProtocolo;
end;

procedure TfrmDACTeQRRetrato2Vias.qrb_02_ItensBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
begin
  inherited;

  if Trim(FLogo) <> '' then
  begin
    try                                //thiago ver aqui 29/11/2012
      qriLogoDetalhe.Picture.LoadFromFile(FLogo);
    except
    end;
  end;
  // Alterado por Italo em 17/05/2012
  if FExpandirLogoMarca then
  begin
    qriLogoDetalhe.top:=26;
    qriLogoDetalhe.Left:=10;
    qriLogoDetalhe.Height:=72;
    qriLogoDetalhe.Width:=365;
    qriLogoDetalhe.Stretch:=true;
    qrmEmitente_Compl.Enabled:=False;
    qrmDadosEmitente_Compl.Enabled:=False;
    qrlURLEmitente_Compl.Enabled:=False;
  end;

{$IFDEF PL_200}
  if FCTe.infCTeNorm.Rodo.veic.Count > 0 then
  begin
    qrlPlaca_Compl.Caption := FCTe.infCTeNorm.Rodo.veic.Items[0].placa;
    qrlRNTRC_Compl.Caption := FCTe.infCTeNorm.Rodo.veic.Items[0].Prop.RNTRC;
  end
  else
  begin
    qrlPlaca_Compl.Caption := '';
    qrlRNTRC_Compl.Caption := '';
  end;

  with FCTe.infCTeNorm.Rodo do
    qrlRNTRC_Compl.Caption := RNTRC;

{$ELSE}
  if FCTe.Rodo.veic.Count > 0 then
  begin
    qrlPlaca_Compl.Caption := FCTe.Rodo.veic.Items[0].placa;
    qrlRNTRC_Compl.Caption := FCTe.Rodo.veic.Items[0].Prop.RNTRC;
  end
  else
  begin
    qrlPlaca_Compl.Caption := '';
    qrlRNTRC_Compl.Caption := '';
  end;

  with FCTe.Rodo do
    qrlRNTRC_Compl.Caption := RNTRC;
{$ENDIF}

  qrlModal_Compl.Caption := TpModalToStrText(FCTe.Ide.modal);
  qrlModelo_Compl.Caption := FCTe.Ide.modelo;
  qrlSerie_Compl.Caption := FormatFloat( '000', FCTe.Ide.serie);
  qrlNumCte_Compl.Caption := FormatFloat( '000,000,000', FCTe.Ide.nCT );

  qrlPageNumber_Compl.Caption := format('%2.2d', [2]) + '/' + format('%2.2d', [FTotalPages]); //QRCTe.PageNumber+1
  qrlEmissao_Compl.Caption := FormatDateTime('DD/MM/YYYY HH:NN', FCTe.Ide.dhEmi);
  SetBarCodeImage(Copy(FCTe.InfCTe.Id, 4, 44), qriBarCode_Compl);
  qrlChave_Compl.Caption := FormatarChaveAcesso(Copy(FCTe.InfCTe.Id, 4, 44));

  if FProtocoloCTE <> ''
   then qrlProtocolo_Compl.Caption := FProtocoloCTE
   else qrlProtocolo_Compl.Caption := FCTe.procCTe.nProt + '   ' +
                                   ifThen(FCTe.procCTe.dhRecbto <> 0,
                                    DateTimeToStr(FCTe.procCTe.dhRecbto), '');

  qrlTipoCte_Compl.Caption := tpCTToStrText(FCTe.Ide.tpCTe);
  qrlNatOperacao_Compl.Caption := FormatFloat('0000', FCTe.Ide.CFOP) + ' - ' + FCTe.Ide.natOp;
  qrlOrigPrestacao_Compl.Caption := FCTe.Ide.xMunIni + ' - ' + FCTe.Ide.UFIni + ' - ' + FormatFloat('000', FCTe.Ide.cMunIni);
  qrlDestPrestacao_Compl.Caption := FCTe.Ide.xMunFim + ' - ' + FCTe.Ide.UFFim + ' - ' + FormatFloat('000', FCTe.Ide.cMunFim);

  qrlEmitidoPor_Compl.Caption := UpperCase(FUsuario);

  if not FExpandirLogoMarca then
  begin
    qrmEmitente_Compl.Enabled:=True;
    qrmDadosEmitente_Compl.Enabled:=True;
    // Emitente
    with FCTe.Emit do
    begin
      qrmEmitente_Compl.Lines.Text := XNome;

      qrmDadosEmitente_Compl.Lines.Clear;
      with EnderEmit do
      begin
        qrmDadosEmitente_Compl.Lines.Add(XLgr + IfThen(Nro = '0', '', ', ' + Nro));
        if XCpl<>'' then
          qrmDadosEmitente_Compl.Lines.Add(XCpl);

        if XBairro<>'' then
          qrmDadosEmitente_Compl.Lines.Add(XBairro + ' - FONE: ' + FormatarFone(EnderEmit.Fone));

        qrmDadosEmitente_Compl.Lines.Add(XMun + ' - ' + UF + ' - CEP: ' + FormatarCEP(FormatFloat( '00000000', CEP )));
      end;

      qrlCnpjEmitente_Compl.Caption := FormatarCNPJ(CNPJ);
      qrlInscEstEmitente_Compl.Caption := IE;
      qrlURLEmitente_Compl.Caption := FUrl;
    end;
  end;

  Inc(nItemControle);

  if cdsDocumentos.Active and (cdsDocumentos.RecordCount > _NUM_ITEMS_PAGE1) then
  begin
    if cdsDocumentos.Active then
    begin
      cdsDocumentos.RecNo := _NUM_ITEMS_PAGE1 +1;
      qrmmTipoDoc_Compl.Lines.Clear;
      qrmmCNPJ_Compl.Lines.Clear;
      qrmmChaveNFe_Compl.Lines.Clear;

      while not cdsDocumentos.Eof do
      begin
        qrmmTipoDoc_Compl.Lines.Add(cdsDocumentosTIPO_1.AsString);
        qrmmCNPJ_Compl.Lines.Add(Trim(cdsDocumentosCNPJCPF_1.AsString));
        qrmmChaveNFe_Compl.Lines.Add(Trim(cdsDocumentosDOCUMENTO_1.AsString));

        cdsDocumentos.Next;
      end;
    end;
  end
  else
    qrb_02_Itens.Height := 0;
end;

procedure TfrmDACTeQRRetrato2Vias.qrb_03_ItensBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
begin
  inherited;

  if cdsDocumentos.Active and (cdsDocumentos.RecNo >= (_NUM_ITEMS_OTHERPAGES + _NUM_ITEMS_OTHERPAGES)) then
  begin
    if cdsDocumentos.Active then
    begin
      cdsDocumentos.RecNo := (_NUM_ITEMS_PAGE1 + _NUM_ITEMS_OTHERPAGES) + 2;
      qrmmTipoDoc_Compl2.Lines.Clear;
      qrmmCNPJ_Compl2.Lines.Clear;
      qrmmChaveNFe_Compl2.Lines.Clear;

      while not cdsDocumentos.Eof do
      begin
        qrmmTipoDoc_Compl2.Lines.Add(cdsDocumentosTIPO_1.AsString);
        qrmmCNPJ_Compl2.Lines.Add(Trim(cdsDocumentosCNPJCPF_1.AsString));
        qrmmChaveNFe_Compl2.Lines.Add(Trim(cdsDocumentosDOCUMENTO_1.AsString));

        cdsDocumentos.Next;
      end;

      qrb_03_Itens.Height := 649;
      //QRCTe.Height := 1310;

      qrb_01_DadosDACTe.ForceNewPage := True;

      qrmmTipoDoc_Compl3.Lines.Clear;
      qrmmCNPJ_Compl3.Lines.Clear;
      qrmmChaveNFe_Compl3.Lines.Clear;
    end;
  end
  else
  begin
    if cdsDocumentos.Active and (cdsDocumentos.RecNo = (_NUM_ITEMS_OTHERPAGES * 3)) then
    begin
      if cdsDocumentos.Active then
      begin
        cdsDocumentos.RecNo := (_NUM_ITEMS_OTHERPAGES * 3);
        qrmmTipoDoc_Compl3.Lines.Clear;
        qrmmCNPJ_Compl3.Lines.Clear;
        qrmmChaveNFe_Compl3.Lines.Clear;

        while not cdsDocumentos.Eof do
        begin
          qrmmTipoDoc_Compl3.Lines.Add(cdsDocumentosTIPO_1.AsString);
          qrmmCNPJ_Compl3.Lines.Add(Trim(cdsDocumentosCNPJCPF_1.AsString));
          qrmmChaveNFe_Compl3.Lines.Add(Trim(cdsDocumentosDOCUMENTO_1.AsString));

          cdsDocumentos.Next;
        end;

        qrb_01_DadosDACTe.ForceNewPage := True;
      end;
    end
    else
    begin
      qrb_03_Itens.Height := 0;
      QRCTe.Height := 1310;
    end;
  end;
end;

procedure TfrmDACTeQRRetrato2Vias.QRChildBand1BeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
begin
  inherited;

  if cdsDocumentos.Active and (cdsDocumentos.RecNo > 60) then
  begin
    if cdsDocumentos.Active then
    begin
      cdsDocumentos.RecNo := _NUM_ITEMS_PAGE1 +1;
      qrmmTipoDoc_Compl2.Lines.Clear;
      qrmmCNPJ_Compl2.Lines.Clear;
      qrmmChaveNFe_Compl2.Lines.Clear;

      while not cdsDocumentos.Eof do
      begin
        qrmmTipoDoc_Compl2.Lines.Add(cdsDocumentosTIPO_1.AsString);
        qrmmCNPJ_Compl2.Lines.Add(Trim(cdsDocumentosCNPJCPF_1.AsString));
        qrmmChaveNFe_Compl2.Lines.Add(Trim(cdsDocumentosDOCUMENTO_1.AsString));

        cdsDocumentos.Next;
      end;
      QRCTe.Height := 1810;
      qrb_03_Itens.Height := 455;
      qrb_01_DadosDACTe.ForceNewPage := True;


    end;
  end
  else
  begin
    qrb_03_Itens.Height := 0;
    QRCTe.Height := 1322;
  end;

end;

procedure TfrmDACTeQRRetrato2Vias.QRCTeBeforePrint(Sender: TCustomQuickRep; var PrintReport: Boolean);
var
  nRestItens, nTotalItens, i : Integer;
begin
  inherited;

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
  nItemControle := 0;
  FTotalPages   := 1;
  nTotalItens   := 0;

{$IFDEF PL_200}
  // Incluido por Italo em 20/04/2011
  if (FCTe.infCTeNorm.infDoc.InfNF.Count > 0)
   then nTotalItens := FCTe.infCTeNorm.infDoc.InfNF.Count
   else begin
    if (FCTe.infCTeNorm.infDoc.InfNFE.Count > 0)
     then nTotalItens := FCTe.infCTeNorm.infDoc.InfNFE.Count
     else begin
      if (FCTe.infCTeNorm.infDoc.InfOutros.Count > 0)
       then nTotalItens := FCTe.infCTeNorm.infDoc.InfOutros.Count;
     end;
   end;
{$ELSE}
  if (FCTe.Rem.InfNF.Count > 0)
   then nTotalItens := FCTe.Rem.InfNF.Count
   else begin
    if (FCTe.Rem.InfNFE.Count > 0)
     then nTotalItens := FCTe.Rem.InfNFE.Count
     else begin
      if (FCTe.Rem.InfOutros.Count > 0)
       then nTotalItens := FCTe.Rem.InfOutros.Count;
     end;
   end;
{$ENDIF}


  // Incluido por Italo em 06/05/2011
//  qrb_11_ModRodLot104.Height     := 0;
//  qrb_12_ModAereo.Height         := 0;
//  qrb_13_ModAquaviario.Height    := 0;
//  qrb_14_ModFerroviario.Height   := 0;
//  qrb_15_ModDutoviario.Height    := 0;

//  if cdsDocumentos.Active and (cdsDocumentos.RecordCount <= _NUM_ITEMS_PAGE1) then
//    qrb_02_Itens.Height := 0
//  else
//    qrb_02_Itens.Height := 539;

{$IFDEF PL_200}
  qrlRNTRC.Caption := FCTe.infCTeNorm.Rodo.RNTRC;

  case FCTe.Ide.modal of
   mdRodoviario: begin
                  // Incluido por Italo em 26/04/2011
                  if FCTe.infCTeNorm.Rodo.Lota = ltNao
                   then begin
                    Fracionado                 := 10
                   end
                   else begin
                    Fracionado                 := 0;
//                    qrb_11_ModRodLot104.Height := 108;
                   end;
                 end;
   mdAereo: begin
  //           qrb_12_ModAereo.Height := 97;
            end;
   mdAquaviario: begin
//                  qrb_13_ModAquaviario.Height    := 0;
                 end;
   mdFerroviario: begin
  //                 qrb_14_ModFerroviario.Height   := 0;
                  end;
   mdDutoviario: begin
    //              qrb_15_ModDutoviario.Height    := 0;
                 end;
  end;
{$ELSE}
  qrlRNTRC.Caption := FCTe.Rodo.RNTRC;

case FCTe.Ide.modal of
   mdRodoviario: begin
                  // Incluido por Italo em 26/04/2011
                  if FCTe.Rodo.Lota = ltNao
                   then begin
                    Fracionado                 := 10
                   end
                   else begin
                    Fracionado                 := 0;
//                    qrb_11_ModRodLot104.Height := 108;
                   end;
                 end;
   mdAereo: begin
  //           qrb_12_ModAereo.Height := 97;
            end;
   mdAquaviario: begin
//                  qrb_13_ModAquaviario.Height    := 0;
                 end;
   mdFerroviario: begin
  //                 qrb_14_ModFerroviario.Height   := 0;
                  end;
   mdDutoviario: begin
    //              qrb_15_ModDutoviario.Height    := 0;
                 end;
  end;

{$ENDIF}





  // Alterado por Italo em 20/04/2011
  if (nTotalItens > (_NUM_ITEMS_PAGE1 + Fracionado)) then
  begin
    nRestItens := nTotalItens - (_NUM_ITEMS_PAGE1 + Fracionado);
    if nRestItens <= (_NUM_ITEMS_OTHERPAGES + Fracionado) then
      Inc(FTotalPages)
    else
    begin
      Inc(FTotalPages, nRestItens div (_NUM_ITEMS_OTHERPAGES + Fracionado));
      if (nRestItens mod (_NUM_ITEMS_OTHERPAGES + Fracionado)) > 0 then
        Inc(FTotalPages)
    end;
  end;

  QRCTe.ReportTitle:='CT-e: ' + FormatFloat( '000,000,000', FCTe.Ide.nCT );

//  QRCTe.Page.TopMargin    := 50; //FMargemSuperior * 100;
//  QRCTe.Page.BottomMargin := 50; //FMargemInferior * 100;
//  QRCTe.Page.LeftMargin   := 50; //FMargemEsquerda * 100;
//  QRCTe.Page.RightMargin  := 50; //FMargemDireita  * 100;
//

  // Incluido por Italo em 26/04/2011
//  qrbObs.Height              := 72;
//  qrbRecibo.Height           := 68;
//  qrbSistema.Height          := 16;
//  qrbDadosExcEmitente.Height := 40;
end;

procedure TfrmDACTeQRRetrato2Vias.qrb_01_ReciboBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;
  // Incluido/Alterado por Italo em 20/04/2011 / 27/04/2011
  PrintBand := (QRCTe.PageNumber = 1) and (FCTe.Ide.modal <> mdAereo) and (FPosRecibo = prCabecalho);

//  qrlSerie2.Caption  := FormatFloat( '000', FCTe.Ide.serie);
//  qrlNumCte2.Caption := FormatFloat( '000,000,000', FCTe.Ide.nCT );
  // Incluido por Italo em 27/04/2011
  // TpcteTipoCTe = (tcNormal, tcComplemento, tcAnulacao, tcSubstituto);
//  qrb_01_Recibo.Enabled := (FCTe.Ide.tpCTe = tcNormal);
end;

procedure TfrmDACTeQRRetrato2Vias.qrb_01_Recibo_AereoBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;
  PrintBand := (QRCTe.PageNumber = 1) and (FCTe.Ide.modal = mdAereo);

//  qrb_01_Recibo_Aereo.Enabled := (FCTe.Ide.tpCTe = tcNormal);
end;

procedure TfrmDACTeQRRetrato2Vias.qrb_02_CabecalhoBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
begin
//
end;

procedure TfrmDACTeQRRetrato2Vias.qrb_01_DadosDACTeBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
var
 i: Integer;
  strChaveContingencia: string;
  sObs : string;
begin
  inherited;

  qrmObs1.Lines.BeginUpdate; // Linha inserida por Italo em 31/08/2010
  qrmObs1.Lines.Clear;

  qrmObs2.Lines.BeginUpdate;
  qrmObs2.Lines.Clear;
  //thiago tratar observacao2!!!

  qrmObs1.Lines.Add(Copy(StringReplace( FCTe.Compl.xObs, '&lt;BR&gt;', #13#10, [rfReplaceAll,rfIgnoreCase] ), 1, 210));
  if Length(StringReplace( FCTe.Compl.xObs, '&lt;BR&gt;', #13#10, [rfReplaceAll,rfIgnoreCase] )) > 210 then
    qrmObs2.Lines.Add(Copy(StringReplace( FCTe.Compl.xObs, '&lt;BR&gt;', #13#10, [rfReplaceAll,rfIgnoreCase] ), 211, 600));
//  qrmObs1.Lines.Add(StringReplace( FCTe.Compl.xObs, '&lt;BR&gt;', #13#10, [rfReplaceAll,rfIgnoreCase] ) );

  qrlVlrICMSST.Caption   := '';
  qrlVlrCREDPRES.Caption := '';

  // Comentado por Italo em 20/04/2011
  //  PrintBand := QRCTe.PageNumber = 1;

  case FTotalPages of
    1: qrsTraco.Enabled := (QRCTe.PageNumber = 1);
    2:
      begin
        qrsTraco.Enabled := False;
        qsTracoDetalhe.Enabled := True
      end;
  end;

 {$IFDEF PL_200}
  if (FCTe.infCTeNorm.Rodo.Lota = ltSim) and (FCTe.infCTeNorm.Rodo.moto.Count > 0) then
  begin
    QRLabel173.Caption := 'MOTORISTA';
    QRLabel172.Caption := 'CPF';
    QRLabel42.Caption := 'EQUIP';
    qrlNomeMotorista2.Caption := FCTe.infCTeNorm.Rodo.moto.Items[0].xNome;
    qrlCPFMotorista2.Caption := FCTe.infCTeNorm.Rodo.moto.Items[0].CPF;
  end
  else
  begin
    QRLabel173.Caption := '';
    QRLabel172.Caption := '';
    QRLabel42.Caption := '';
    qrlNomeMotorista2.Caption := '';
    qrlCPFMotorista2.Caption := '';
  end;

  qrlPLCEquip.Caption := '';

  if FCTe.infCTeNorm.Rodo.veic.Count > 0 then
  begin
    qrlPlaca.Caption := FCTe.infCTeNorm.Rodo.veic.Items[0].placa;
    qrlRNTRC.Caption := FCTe.infCTeNorm.Rodo.veic.Items[0].Prop.RNTRC;

    if FCTe.infCTeNorm.Rodo.veic.Count > 1 then
      qrlPLCEquip.Caption := FCTe.infCTeNorm.Rodo.veic.Items[1].placa;
  end
  else
  begin
    qrlPlaca.Caption := '';
    qrlRNTRC.Caption := '';
  end;
{$ELSE}
  if (FCTe.Rodo.Lota = ltSim) and (FCTe.Rodo.moto.Count > 0) then
  begin
    QRLabel173.Caption := 'MOTORISTA';
    QRLabel172.Caption := 'CPF';
    QRLabel42.Caption := 'EQUIP';
    qrlNomeMotorista2.Caption := FCTe.Rodo.moto.Items[0].xNome;
    qrlCPFMotorista2.Caption := FCTe.Rodo.moto.Items[0].CPF;
  end
  else
  begin
    QRLabel173.Caption := '';
    QRLabel172.Caption := '';
    QRLabel42.Caption := '';
    qrlNomeMotorista2.Caption := '';
    qrlCPFMotorista2.Caption := '';
  end;

  qrlPLCEquip.Caption := '';

  if FCTe.Rodo.veic.Count > 0 then
  begin
    qrlPlaca.Caption := FCTe.Rodo.veic.Items[0].placa;
    qrlRNTRC.Caption := FCTe.Rodo.veic.Items[0].Prop.RNTRC;

    if FCTe.Rodo.veic.Count > 1 then
      qrlPLCEquip.Caption := FCTe.Rodo.veic.Items[1].placa;
  end
  else
  begin
    qrlPlaca.Caption := '';
    qrlRNTRC.Caption := '';
  end;
{$ENDIF}

  qrlEmitidoPor.Caption := UpperCase(FUsuario);

  if Trim(FLogo) <> '' then
  begin
    try
      qriLogo.Picture.LoadFromFile(FLogo);
    except
    end;
  end;

  // Alterado por Italo em 17/05/2012
  if FExpandirLogoMarca then
  begin
    qriLogo.top:=26;
    qriLogo.Left:=10;
    qriLogo.Height:=72;
    qriLogo.Width:=365;
    qriLogo.Stretch:=true;
    qrmEmitente.Enabled:=False;
    qrmDadosEmitente.Enabled:=False;
    qrlURLEmitente.Enabled:=False;
  end;

  qrlModal.Caption := TpModalToStrText(FCTe.Ide.modal);
  qrlModelo.Caption := FCTe.Ide.modelo;
  qrlSerie.Caption := FormatFloat( '000', FCTe.Ide.serie);
  qrlNumCte.Caption := FormatFloat( '000,000,000', FCTe.Ide.nCT );
  qrlNumCte3.Caption := FormatFloat( '#####0000', FCTe.Ide.nCT );

  //ABCDEFGHIJKLMNOPQRSTUXYWZ'
  //1234567890123456789012345
  qrlPageNumber.Caption := format('%2.2d', [1]) + '/' + format('%2.2d', [FTotalPages]);
  qrlEmissao.Caption := FormatDateTime('DD/MM/YYYY HH:NN', FCTe.Ide.dhEmi);
  SetBarCodeImage(Copy(FCTe.InfCTe.Id, 4, 44), qriBarCode);
//  SetBarCodeImage('318' + FormatFloat('0000000', FCTe.Ide.nCT), qriBarControle);
  qrlChave.Caption := FormatarChaveAcesso(Copy(FCTe.InfCTe.Id, 4, 44));

  // Incluido por Italo em 17/05/2012
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
        if XCpl<>'' then
          qrmDadosEmitente.Lines.Add(XCpl);

        if XBairro<>'' then
          qrmDadosEmitente.Lines.Add(XBairro + ' - FONE: ' + FormatarFone(EnderEmit.Fone));

        qrmDadosEmitente.Lines.Add(XMun + ' - ' + UF + ' - CEP: ' + FormatarCEP(FormatFloat( '00000000', CEP )));
      end;

      qrlCnpjEmitente.Caption := FormatarCNPJ(CNPJ);
      qrlInscEstEmitente.Caption := IE;
      //qrlURLEmitente.Caption := FUrl;
      qrlURLEmitente.Lines.Clear;
      qrlURLEmitente.Lines.Add(FUrl);
    end;
   end;

  qrlTipoCte.Caption := tpCTToStrText(FCTe.Ide.tpCTe);
  // Alterado por Italo em 30/12/2010
  if FCTe.Ide.Toma4.xNome = ''
   then qrlTomaServico.Caption := TpTomadorToStrText(FCTe.Ide.Toma03.Toma)
   else qrlTomaServico.Caption := TpTomadorToStrText(FCTe.Ide.Toma4.toma);
  qrlFormaPagamento.Caption := tpforPagToStrText(FCTe.Ide.forPag);

  // As Linhas abaixo foram inseridas por Italo em 31/08/2010
  // Normal **************************************************************
  if FCTe.Ide.tpEmis in [teNormal, teSCAN] then
   begin
    // Incluidas por Italo em 01/01/2012
    //qrlVariavel1.Enabled := True;
//    qriBarCode2.Enabled  := False;
//    if FCTe.procCTe.cStat = 100
//     then qrlDescricao.Caption := 'PROTOCOLO DE AUTORIZAÇÃO DE USO';

//    if FCTe.procCTe.cStat = 101
//     then qrlDescricao.Caption := 'PROTOCOLO DE HOMOLOGAÇÃO DE CANCELAMENTO';

    // Alterado de 102 para 110 por Italo em 27/01/2012
 //   if FCTe.procCTe.cStat = 110
//     then qrlDescricao.Caption := 'PROTOCOLO DE DENEGAÇÃO DE USO';

    if FProtocoloCTE <> ''
     then qrlProtocolo.Caption := FProtocoloCTE
     else qrlProtocolo.Caption := FCTe.procCTe.nProt + '   ' +
                                     ifThen(FCTe.procCTe.dhRecbto <> 0,
                                      DateTimeToStr(FCTe.procCTe.dhRecbto), '');
   end;

  // As Linhas abaixo foram inseridas por Italo em 28/01/2011
  // Contingencia ********************************************************

  if FCTe.Ide.tpEmis in [teContingencia, teFSDA] then
   begin
    // Incluido por Italo em 20/04/2012
    if FCTe.procCTe.cStat in [100, 101, 110]
     then begin
     // qrlVariavel1.Enabled := True;
   //   qriBarCode2.Enabled  := False;
//      if FCTe.procCTe.cStat = 100
//       then qrlDescricao.Caption := 'PROTOCOLO DE AUTORIZAÇÃO DE USO';

//      if FCTe.procCTe.cStat = 101
//       then qrlDescricao.Caption := 'PROTOCOLO DE HOMOLOGAÇÃO DE CANCELAMENTO';

//      if FCTe.procCTe.cStat = 110
//       then qrlDescricao.Caption := 'PROTOCOLO DE DENEGAÇÃO DE USO';

      if FProtocoloCTE <> ''
       then qrlProtocolo.Caption := FProtocoloCTE
       else qrlProtocolo.Caption := FCTe.procCTe.nProt + '   ' +
                                    ifThen(FCTe.procCTe.dhRecbto <> 0,
                                        DateTimeToStr(FCTe.procCTe.dhRecbto), '');
     end
     else begin
      // Incluidas por Italo em 01/01/2012
  //    qrlVariavel1.Enabled := False;
 //     qriBarCode2.Enabled  := True;

      strChaveContingencia := TACBrCTe(FACBrCTe).GerarChaveContingencia(FCTe);
    //  SetBarCodeImage(strChaveContingencia, qriBarCode2 );
//      qrlDescricao.Caption := 'DADOS DO CT-E';
      qrlProtocolo.Caption := FormatarChaveAcesso(strChaveContingencia);
     end;
   end;

  // DPEC ****************************************************************
  if FCTe.Ide.tpEmis = teDPEC then
   begin
//    qrlDescricao.Caption := 'NÚMERO DE REGISTRO DPEC';
    qrlProtocolo.Caption := FProtocoloCTE;
   end;

{$IFDEF PL_200}
  with FCTe.infCTeNorm.Rodo do
  begin
    qrlRNTRC.Caption := RNTRC;
{$ELSE}
  with FCTe.Rodo do
  begin
    qrlRNTRC.Caption := RNTRC;

{$ENDIF}


{$IFDEF PL_103}
    qrsCIOT.Enabled := False;
    lblCIOT.Enabled := False;
//    qrlCIOT.Enabled := False;
{$ENDIF}
{$IFDEF PL_104}
//    qrsCIOT.Enabled := True;
//    lblCIOT.Enabled := True;
//    qrlCIOT.Enabled := True;
//    qrlCIOT.Caption := CIOT;
{$ENDIF}

    case Lota of
      ltNao: begin
//              qrlLotacao.Caption          := 'NÃO';
             end;
      ltsim: begin
//              qrlLotacao.Caption          := 'SIM';
//              qrb_11_ModRodLot104.Enabled := True;
             end;
    end;

    qrlDtPrevEntrega.Caption := FormatDateTime('DD/MM/YYYY', dPrev);
  end;


//  qrlInscSuframa.Caption := FCTe.Dest.ISUF;


//end;

  // Incluido por Italo em 20/04/2011
//  PrintBand := QRCTe.PageNumber = 1; THIAGO 02/01/2012

  qrlNatOperacao.Caption := FormatFloat('0000', FCTe.Ide.CFOP) + ' - ' + FCTe.Ide.natOp;

//  qrlFilial.Caption := FFilial + '/';
  qrlFilial.Caption := '';

  //Rodapé
  qrlSistema.Caption := 'PROCESSADO POR: ' + FSistema;

  qrlOrigPrestacao.Caption := FCTe.Ide.xMunIni + ' - ' + FCTe.Ide.UFIni + ' - ' + FormatFloat('000', FCTe.Ide.cMunIni);
  qrlDestPrestacao.Caption := FCTe.Ide.xMunFim + ' - ' + FCTe.Ide.UFFim + ' - ' + FormatFloat('000', FCTe.Ide.cMunFim);

  //DADOS REMETENTE
  qrlRazaoRemet.Caption := FCTe.Rem.xNome;
  qrlEnderecoRemet1.Caption := FCTe.Rem.EnderReme.xLgr + ', ' + FCTe.Rem.EnderReme.nro + ' ' + FCTe.Rem.enderReme.xBairro;
//  qrlEnderecoRemet2.Caption := FCTe.Rem.EnderReme.xCpl + ' - ' + FCTe.Rem.EnderReme.xBairro;
  qrlCEPRemet.Caption := FormatarCEP(FormatFloat( '00000000', FCTe.Rem.EnderReme.CEP ));
  qrlMunRemet.Caption := FCTe.Rem.EnderReme.xMun+' - '+FCTe.Rem.EnderReme.UF;
  qrlCnpjRemet.Caption := FormatarCNPJ(FCTe.Rem.CNPJCPF);
//  qrlPaisRemet.Caption := FCTe.Rem.EnderReme.xPais;
  qrlInscEstRemet.Caption := FCTe.Rem.IE;
  qrlFoneRemet.Caption := FormatarFone(FCTe.Rem.fone);

  //DADOS DESTINATARIO
  qrlRazaoDest.Caption := FCTe.Dest.xNome;
  qrlEnderecoDest1.Caption := FCTe.Dest.EnderDest.xLgr + ', ' + FCTe.Dest.EnderDest.nro + ' ' + FCTe.Dest.enderDest.xBairro;
//  qrlEnderecoDest2.Caption := FCTe.Dest.EnderDest.xCpl + ' - ' + FCTe.Dest.EnderDest.xBairro;
  qrlCEPDest.Caption := FormatarCEP(FormatFloat( '00000000', FCTe.Dest.EnderDest.CEP));
  qrlMunDest.Caption := FCTe.Dest.EnderDest.xMun+' - '+FCTe.Dest.EnderDest.UF;
  qrlCnpjDest.Caption := FormatarCNPJ(FCTe.Dest.CNPJCPF);
//  qrlPaisDest.Caption := FCTe.Dest.EnderDest.xPais;
  qrlInscEstDest.Caption := FCTe.Dest.IE;
  qrlFoneDest.Caption := FormatarFone(FCTe.Dest.fone);


  //DADOS EXPEDIDOR
  qrlRazaoExped.Caption := FCTe.Exped.xNome;
  qrlEnderecoExped1.Caption := FCTe.Exped.EnderExped.xLgr + ', ' + FCTe.Exped.EnderExped.nro + ' ' + FCTe.Exped.enderExped.xBairro;
//  qrlEnderecoExped2.Caption := FCTe.Exped.EnderExped.xCpl + ' - ' + FCTe.Exped.EnderExped.xBairro;
  qrlCEPExped.Caption := FormatarCEP(FormatFloat( '00000000', FCTe.Exped.EnderExped.CEP));
  qrlMunExped.Caption := FCTe.Exped.EnderExped.xMun+' - '+FCTe.Exped.EnderExped.UF;
  qrlCnpjExped.Caption := FormatarCNPJ(FCTe.Exped.CNPJCPF);
//  qrlPaisExped.Caption := FCTe.Exped.EnderExped.xPais;
  qrlInscEstExped.Caption := FCTe.Exped.IE;
  qrlFoneExped.Caption := FormatarFone(FCTe.Exped.fone);

  //DADOS RECEBEDOR
  qrlRazaoReceb.Caption := FCTe.Receb.xNome;
  qrlEnderecoReceb1.Caption := FCTe.Receb.EnderReceb.xLgr + ', ' + FCTe.Receb.EnderReceb.nro + ' ' + FCTe.Receb.enderReceb.xBairro;
//  qrlEnderecoReceb2.Caption := FCTe.Receb.EnderReceb.xCpl + ' - ' + FCTe.Receb.EnderReceb.xBairro;
  qrlCEPReceb.Caption := FormatarCEP(FormatFloat( '00000000', FCTe.Receb.EnderReceb.CEP));
  qrlMunReceb.Caption := FCTe.Receb.EnderReceb.xMun+' - '+FCTe.Receb.EnderReceb.UF;
  qrlCnpjReceb.Caption := FormatarCNPJ(FCTe.Receb.CNPJCPF);
//  qrlPaisReceb.Caption := FCTe.Receb.EnderReceb.xPais;
  qrlInscEstReceb.Caption := FCTe.Receb.IE;
  qrlFoneReceb.Caption := FormatarFone(FCTe.Receb.fone);

  if FCTe.Ide.Toma4.xNome = ''
   then begin
    case FCTe.Ide.Toma03.Toma of
    tmRemetente:
      begin
        qrlRazaoToma.Caption := FCTe.Rem.xNome;
        qrlEnderecoToma.Caption := FCTe.Rem.EnderReme.xLgr + ', ' + FCTe.Rem.EnderReme.nro + ' - ' + FCTe.Rem.EnderReme.xCpl + ' - ' + FCTe.Rem.EnderReme.xBairro;
        qrlCEPToma.Caption := FormatarCEP(FormatFloat( '00000000', FCTe.Rem.EnderReme.CEP));
        qrlMunToma.Caption := FCTe.Rem.EnderReme.xMun+' - '+FCTe.Rem.EnderReme.UF;
        qrlCnpjToma.Caption := FormatarCNPJ(FCTe.Rem.CNPJCPF);
//        qrlPaisToma.Caption := FCTe.Rem.EnderReme.xPais;
        qrlInscEstToma.Caption := FCTe.Rem.IE;
        qrlFoneToma.Caption := FormatarFone(FCTe.Rem.fone);
      end;
    tmExpedidor:
      begin
        qrlRazaoToma.Caption := FCTe.Exped.xNome;
        qrlEnderecoToma.Caption := FCTe.Exped.EnderExped.xLgr + ', ' + FCTe.Exped.EnderExped.nro + ' - ' + FCTe.Exped.EnderExped.xCpl + ' - ' + FCTe.Exped.EnderExped.xBairro;
        qrlCEPToma.Caption := FormatarCEP(FormatFloat( '00000000', FCTe.Exped.EnderExped.CEP));
        qrlMunToma.Caption := FCTe.Exped.EnderExped.xMun+' - '+FCTe.Exped.EnderExped.UF;
        qrlCnpjToma.Caption := FormatarCNPJ(FCTe.Exped.CNPJCPF);
//        qrlPaisToma.Caption := FCTe.Exped.EnderExped.xPais;
        qrlInscEstToma.Caption := FCTe.Exped.IE;
        qrlFoneToma.Caption := FormatarFone(FCTe.Exped.fone);
      end;
    tmRecebedor:
      begin
        qrlRazaoToma.Caption := FCTe.Receb.xNome;
        qrlEnderecoToma.Caption := FCTe.Receb.EnderReceb.xLgr + ', ' + FCTe.Receb.EnderReceb.nro + ' - ' + FCTe.Receb.EnderReceb.xCpl + ' - ' + FCTe.Receb.EnderReceb.xBairro;
        qrlCEPToma.Caption := FormatarCEP(FormatFloat( '00000000', FCTe.Receb.EnderReceb.CEP));
        qrlMunToma.Caption := FCTe.Receb.EnderReceb.xMun+' - '+FCTe.Receb.EnderReceb.UF;
        qrlCnpjToma.Caption := FormatarCNPJ(FCTe.Receb.CNPJCPF);
        //qrlPaisToma.Caption := FCTe.Receb.EnderReceb.xPais;
        qrlInscEstToma.Caption := FCTe.Receb.IE;
        qrlFoneToma.Caption := FormatarFone(FCTe.Receb.fone);
      end;
    tmDestinatario:
      begin
        qrlRazaoToma.Caption := FCTe.Dest.xNome;
        qrlEnderecoToma.Caption := FCTe.Dest.EnderDest.xLgr + ', ' + FCTe.Dest.EnderDest.nro + ' - ' + FCTe.Dest.EnderDest.xCpl + ' - ' + FCTe.Dest.EnderDest.xBairro;
        qrlCEPToma.Caption := FormatarCEP(FormatFloat( '00000000', FCTe.Dest.EnderDest.CEP));
        qrlMunToma.Caption := FCTe.Dest.EnderDest.xMun+' - '+FCTe.Dest.EnderDest.UF;
        qrlCnpjToma.Caption := FormatarCNPJ(FCTe.Dest.CNPJCPF);
//        qrlPaisToma.Caption := FCTe.Dest.EnderDest.xPais;
        qrlInscEstToma.Caption := FCTe.Dest.IE;
        qrlFoneToma.Caption := FormatarFone(FCTe.Dest.fone);
      end;
    end;
   end
   else begin
    qrlRazaoToma.Caption := FCTe.Ide.Toma4.xNome;
    qrlEnderecoToma.Caption := FCTe.Ide.Toma4.EnderToma.xLgr + ', ' + FCTe.Ide.Toma4.EnderToma.nro + ' - ' + FCTe.Ide.Toma4.EnderToma.xCpl + ' - ' + FCTe.Ide.Toma4.EnderToma.xBairro;
    qrlCEPToma.Caption := FormatarCEP(FormatFloat( '00000000', FCTe.Ide.Toma4.EnderToma.CEP));
    qrlMunToma.Caption := FCTe.Ide.Toma4.EnderToma.xMun+' - '+FCTe.Ide.Toma4.EnderToma.UF;
    qrlCnpjToma.Caption := FormatarCNPJ(FCTe.Ide.Toma4.CNPJCPF);
//    qrlPaisToma.Caption := FCTe.Ide.Toma4.EnderToma.xPais;
    qrlInscEstToma.Caption := FCTe.Ide.Toma4.IE;
    qrlFoneToma.Caption := FormatarFone(FCTe.Ide.Toma4.fone);
   end;

{$IFDEF PL_200}
  qrlProdPredominante.Caption := FCTe.infCTeNorm.infCarga.proPred;
{$ELSE}
  qrlProdPredominante.Caption := FCTe.InfCarga.proPred;
{$ENDIF}

{$IFDEF PL_103}
  qrlVlrTotalMerc.Caption := FormatarValor(msk15x2, FCTe.InfCarga.vMerc);
{$ENDIF}
{$IFDEF PL_104}
  qrlVlrTotalMerc.Caption := CTeUtil.FormatarValor(msk15x2, FCTe.InfCarga.vCarga);
{$ENDIF}
{$IFDEF PL_200}
  qrlVlrTotalMerc.Caption := FormatFloat('###,###,###,##0.00', FCTe.infCTeNorm.infCarga.vCarga);
{$ENDIF}

  {
  for i := 1 to 4 do
    TQRLabel(FindComponent('qrlQtdUndMedida' + intToStr(i))).Caption := '';

  for i := 0 to FCTe.InfCarga.InfQ.Count - 1 do
    TQRLabel(FindComponent('qrlQtdUndMedida' + intToStr(i + 1))).Caption :=
      FormatarValor(msk6x3, FCTe.InfCarga.InfQ.Items[i].qCarga) + '/' +
      FCTe.InfCarga.InfQ.Items[i].tpMed;
  }


  qrmQtdUnidMedida1.Caption := '';
  qrmQtdUnidMedida2.Caption := '';
  qrmQtdUnidMedida3.Caption := '';
  qrmQtdUnidMedida4.Caption := '';
  qrmQtdUnidMedida5.Caption := '';

{$IFDEF PL_200}
  for i := 0 to FCTe.infCTeNorm.InfCarga.InfQ.Count - 1 do
   begin
    // Alterado por Italo em 17/05/2012
    //UnidMed = (uM3,uKG, uTON, uUNIDADE, uLITROS, uMMBTU);
    case FCTe.infCTeNorm.InfCarga.InfQ.Items[i].cUnid of
          uM3: qrmQtdUnidMedida4.Caption := FormatFloat('###,##0.000', FCTe.infCTeNorm.InfCarga.InfQ.Items[i].qCarga);
          uKg: begin
                if uppercase(trim(FCTe.infCTeNorm.InfCarga.InfQ.Items[i].tpMed))='PESO BRUTO'
                then qrmQtdUnidMedida1.Caption := FormatFloat('###,##0.000', FCTe.infCTeNorm.InfCarga.InfQ.Items[i].qCarga)
                else
                if uppercase(trim(FCTe.infCTeNorm.InfCarga.InfQ.Items[i].tpMed))='PESO BASE DE CALCULO'
                then qrmQtdUnidMedida2.Caption := FormatFloat('###,##0.000', FCTe.infCTeNorm.InfCarga.InfQ.Items[i].qCarga)
                else
                if uppercase(trim(FCTe.infCTeNorm.InfCarga.InfQ.Items[i].tpMed))='PESO BC'
                then qrmQtdUnidMedida2.Caption := FormatFloat('###,##0.000', FCTe.infCTeNorm.InfCarga.InfQ.Items[i].qCarga)
                else qrmQtdUnidMedida3.Caption := FormatFloat('###,##0.000', FCTe.infCTeNorm.InfCarga.InfQ.Items[i].qCarga);
               end;
         uTON: begin
                if uppercase(trim(FCTe.infCTeNorm.InfCarga.InfQ.Items[i].tpMed))='PESO BRUTO'
                then qrmQtdUnidMedida1.Caption := FormatFloat('###,##0.000', FCTe.infCTeNorm.InfCarga.InfQ.Items[i].qCarga)
                else
                if uppercase(trim(FCTe.infCTeNorm.InfCarga.InfQ.Items[i].tpMed))='PESO BASE DE CALCULO'
                then qrmQtdUnidMedida2.Caption := FormatFloat('###,##0.000', FCTe.infCTeNorm.InfCarga.InfQ.Items[i].qCarga)
                else
                if uppercase(trim(FCTe.infCTeNorm.InfCarga.InfQ.Items[i].tpMed))='PESO BC'
                then qrmQtdUnidMedida2.Caption := FormatFloat('###,##0.000', FCTe.infCTeNorm.InfCarga.InfQ.Items[i].qCarga)
                else qrmQtdUnidMedida3.Caption := FormatFloat('###,##0.000', FCTe.infCTeNorm.InfCarga.InfQ.Items[i].qCarga);
               end;
     uUNIDADE: qrmQtdUnidMedida5.Caption := FormatFloat('###,##0.000', FCTe.infCTeNorm.InfCarga.InfQ.Items[i].qCarga) + '/' + FCTe.infCTeNorm.InfCarga.InfQ.Items[i].tpMed;
     uLITROS:  qrmQtdUnidMedida5.Caption := FormatFloat('###,##0.000', FCTe.infCTeNorm.InfCarga.InfQ.Items[i].qCarga) + '/' + FCTe.infCTeNorm.InfCarga.InfQ.Items[i].tpMed;
     uMMBTU:   qrmQtdUnidMedida5.Caption := FormatFloat('###,##0.000', FCTe.infCTeNorm.InfCarga.InfQ.Items[i].qCarga) + '/' + FCTe.infCTeNorm.InfCarga.InfQ.Items[i].tpMed;
    end;
   end;
{$ELSE}
  for i := 0 to FCTe.InfCarga.InfQ.Count - 1 do
   begin
    // Alterado por Italo em 17/05/2012
    //UnidMed = (uM3,uKG, uTON, uUNIDADE, uLITROS, uMMBTU);
    case FCTe.InfCarga.InfQ.Items[i].cUnid of
          uM3: qrmQtdUnidMedida4.Caption := CteUtil.FormatarValor(msk6x3, FCTe.InfCarga.InfQ.Items[i].qCarga);
          uKg: begin
                if uppercase(trim(FCTe.InfCarga.InfQ.Items[i].tpMed))='PESO BRUTO'
                then qrmQtdUnidMedida1.Caption := CteUtil.FormatarValor(msk6x3, FCTe.InfCarga.InfQ.Items[i].qCarga)
                else
                if uppercase(trim(FCTe.InfCarga.InfQ.Items[i].tpMed))='PESO BASE DE CALCULO'
                then qrmQtdUnidMedida2.Caption := CteUtil.FormatarValor(msk6x3, FCTe.InfCarga.InfQ.Items[i].qCarga)
                else
                if uppercase(trim(FCTe.InfCarga.InfQ.Items[i].tpMed))='PESO BC'
                then qrmQtdUnidMedida2.Caption := CteUtil.FormatarValor(msk6x3, FCTe.InfCarga.InfQ.Items[i].qCarga)
                else qrmQtdUnidMedida3.Caption := CteUtil.FormatarValor(msk6x3, FCTe.InfCarga.InfQ.Items[i].qCarga);
               end;
         uTON: begin
                if uppercase(trim(FCTe.InfCarga.InfQ.Items[i].tpMed))='PESO BRUTO'
                then qrmQtdUnidMedida1.Caption := CteUtil.FormatarValor(msk6x3, FCTe.InfCarga.InfQ.Items[i].qCarga)
                else
                if uppercase(trim(FCTe.InfCarga.InfQ.Items[i].tpMed))='PESO BASE DE CALCULO'
                then qrmQtdUnidMedida2.Caption := CteUtil.FormatarValor(msk6x3, FCTe.InfCarga.InfQ.Items[i].qCarga)
                else
                if uppercase(trim(FCTe.InfCarga.InfQ.Items[i].tpMed))='PESO BC'
                then qrmQtdUnidMedida2.Caption := CteUtil.FormatarValor(msk6x3, FCTe.InfCarga.InfQ.Items[i].qCarga)
                else qrmQtdUnidMedida3.Caption := CteUtil.FormatarValor(msk6x3, FCTe.InfCarga.InfQ.Items[i].qCarga);
               end;
     uUNIDADE: qrmQtdUnidMedida5.Caption := CteUtil.FormatarValor(msk6x3, FCTe.InfCarga.InfQ.Items[i].qCarga) + '/' + FCTe.InfCarga.InfQ.Items[i].tpMed;
     uLITROS:  qrmQtdUnidMedida5.Caption := CteUtil.FormatarValor(msk6x3, FCTe.InfCarga.InfQ.Items[i].qCarga) + '/' + FCTe.InfCarga.InfQ.Items[i].tpMed;
     uMMBTU:   qrmQtdUnidMedida5.Caption := CteUtil.FormatarValor(msk6x3, FCTe.InfCarga.InfQ.Items[i].qCarga) + '/' + FCTe.InfCarga.InfQ.Items[i].tpMed;
    end;
   end;
{$ENDIF}


//  qrlNomeSeguradora.Caption := '';
//  qrlRespSeguroMerc.Caption := '';
//  qrlNroApolice.Caption := '';
//  qrlNroAverbacao.Caption := '';
           (*
  if FCTe.InfSeg.Count > 0 then
  begin
//    qrlNomeSeguradora.Caption := FCTe.InfSeg.Items[0].xSeg;
//    qrlRespSeguroMerc.Caption := TpRspSeguroToStrText(FCTe.InfSeg.Items[0].respSeg);
//    qrlNroApolice.Caption := FCTe.InfSeg.Items[0].nApol;
  //  qrlNroAverbacao.Caption := FCTe.InfSeg.Items[0].nAver;
  end;   *)

  qrmCompNome1.Lines.Clear;
  qrmCompValor1.Lines.Clear;

  for i := 0 to FCTe.vPrest.comp.Count - 1 do
  begin
    qrmCompNome1.Lines.Add(FCTe.vPrest.comp[i].xNome);
    qrmCompValor1.Lines.Add(FormatFloat('###,###,###,##0.00', FCTe.vPrest.comp[i].vComp));
  end;

  qrlVlrTotServico.Caption := FormatFloat('###,###,###,##0.00', FCTe.vPrest.vTPrest);
  qrlVlrTotReceber.Caption := FormatFloat('###,###,###,##0.00', FCTe.vPrest.vRec);

  qrlSitTrib.Caption := CSTICMSToStr(FCTe.Imp.ICMS.SituTrib)+'-'+
                        CSTICMSToStrTagPosText(FCTe.Imp.ICMS.SituTrib);

  qrmmTipoDoc.Lines.Clear;
  qrmmCNPJ.Lines.Clear;
  qrmmChaveNFe.Lines.Clear;

  if cdsDocumentos.Active then
  begin
    cdsDocumentos.First;
    while not cdsDocumentos.Eof do
    begin
      if cdsDocumentos.RecNo <= _NUM_ITEMS_PAGE1 then
      begin
        qrmmTipoDoc.Lines.Add(cdsDocumentosTIPO_1.AsString);
        qrmmCNPJ.Lines.Add(Trim(cdsDocumentosCNPJCPF_1.AsString));
        qrmmChaveNFe.Lines.Add(Trim(cdsDocumentosDOCUMENTO_1.AsString));
      end
      else
        break;

        cdsDocumentos.Next;
    end;

    QRCTe.Height := 1310;
    qrb_03_Itens.Height := 0;
  end;

{$IFDEF PL_103}
  case FCTe.Imp.ICMS.SituTrib of
    cst00:
      begin
        qrlRedBaseCalc.Caption := '';
        qrlBaseCalc.Caption    := CteUtil.FormatarValor(msk9x2, FCTe.Imp.ICMS.CST00.vBC);
//        qrlAliqICMS.Caption    := CteUtil.FormatarValor(mskAliq, FCTe.Imp.ICMS.CST00.pICMS);
        qrlAliqICMS.Caption    := CteUtil.FormatarValor(msk4x2, FCTe.Imp.ICMS.CST00.pICMS);
        qrlVlrICMS.Caption     := CteUtil.FormatarValor(msk4x2, FCTe.Imp.ICMS.CST00.vICMS);
        qrlICMS_ST.Caption     := '';
      end;
    cst20:
      begin
        qrlRedBaseCalc.Caption := CteUtil.FormatarValor(mskAliq, FCTe.Imp.ICMS.CST20.pRedBC);
        qrlBaseCalc.Caption    := CteUtil.FormatarValor(msk9x2, FCTe.Imp.ICMS.CST20.vBC);
//        qrlAliqICMS.Caption    := CteUtil.FormatarValor(mskAliq, FCTe.Imp.ICMS.CST20.pICMS);
        qrlAliqICMS.Caption    := CteUtil.FormatarValor(msk4x2, FCTe.Imp.ICMS.CST20.pICMS);
        qrlVlrICMS.Caption     := CteUtil.FormatarValor(msk4x2, FCTe.Imp.ICMS.CST20.vICMS);
        qrlICMS_ST.Caption     := '';
        // CteUtil.FormatarValor(msk4x2, FCTe.Imp.ICMS.CST20.vICMS);
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
        qrlBaseCalc.Caption    := CteUtil.FormatarValor(msk9x2, FCTe.Imp.ICMS.CST80.vBC);
//        qrlAliqICMS.Caption    := CteUtil.FormatarValor(mskAliq, FCTe.Imp.ICMS.CST80.pICMS);
        qrlAliqICMS.Caption    := CteUtil.FormatarValor(msk4x2, FCTe.Imp.ICMS.CST80.pICMS);
        qrlVlrICMS.Caption     := CteUtil.FormatarValor(msk9x2, FCTe.Imp.ICMS.CST80.vICMS);
        qrlICMS_ST.Caption     := CteUtil.FormatarValor(msk9x2, FCTe.Imp.ICMS.CST80.vCred);
      end;
    cst81:
      begin
        qrlRedBaseCalc.Caption := CteUtil.FormatarValor(mskAliq, FCTe.Imp.ICMS.CST81.pRedBC);
        qrlBaseCalc.Caption    := CteUtil.FormatarValor(msk9x2, FCTe.Imp.ICMS.CST81.vBC);
//        qrlAliqICMS.Caption    := CteUtil.FormatarValor(mskAliq, FCTe.Imp.ICMS.CST81.pICMS);
        qrlAliqICMS.Caption    := CteUtil.FormatarValor(msk4x2, FCTe.Imp.ICMS.CST81.pICMS);
        qrlVlrICMS.Caption     := CteUtil.FormatarValor(msk9x2, FCTe.Imp.ICMS.CST81.vICMS);
        qrlICMS_ST.Caption     := '';
        // CteUtil.FormatarValor(msk9x2, FCTe.Imp.ICMS.CST81.vICMS);
      end;
    cst90:
      begin
        qrlRedBaseCalc.Caption := CteUtil.FormatarValor(mskAliq, FCTe.Imp.ICMS.CST90.pRedBC);
        qrlBaseCalc.Caption    := CteUtil.FormatarValor(msk9x2, FCTe.Imp.ICMS.CST90.vBC);
//        qrlAliqICMS.Caption    := CteUtil.FormatarValor(mskAliq, FCTe.Imp.ICMS.CST90.pICMS);
        qrlAliqICMS.Caption    := CteUtil.FormatarValor(msk4x2, FCTe.Imp.ICMS.CST90.pICMS);
        qrlVlrICMS.Caption     := CteUtil.FormatarValor(msk9x2, FCTe.Imp.ICMS.CST90.vICMS);
        qrlICMS_ST.Caption     := CteUtil.FormatarValor(msk4x2, FCTe.Imp.ICMS.CST90.vCred);
      end;
  end;
{$ELSE}
  case FCTe.Imp.ICMS.SituTrib of
    cst00:
      begin
//        qrlRedBaseCalc.Caption := '';
        qrlBaseCalc.Caption    := FormatFloat('###,###,###,##0.00', FCTe.Imp.ICMS.ICMS00.vBC);
        qrlAliqICMS.Caption    := FormatFloat('###,###,###,##0.00', FCTe.Imp.ICMS.ICMS00.pICMS);
        qrlVlrICMS.Caption     := FormatFloat('###,###,###,##0.00', FCTe.Imp.ICMS.ICMS00.vICMS);
        qrlVlrICMSST.Caption   := '';
      end;
    cst20:
      begin
//        qrlRedBaseCalc.Caption := CTeUtil.FormatarValor(msk4x2, FCTe.Imp.ICMS.ICMS20.pRedBC);
        qrlBaseCalc.Caption    := FormatFloat('###,###,###,##0.00', FCTe.Imp.ICMS.ICMS20.vBC);
        qrlAliqICMS.Caption    := FormatFloat('###,###,###,##0.00', FCTe.Imp.ICMS.ICMS20.pICMS);
        qrlVlrICMS.Caption     := FormatFloat('###,###,###,##0.00', FCTe.Imp.ICMS.ICMS20.vICMS);
        qrlVlrICMSST.Caption   := '';
      end;
    cst40:
      begin
//        qrlRedBaseCalc.Caption := '';
        qrlBaseCalc.Caption    := '';
        qrlAliqICMS.Caption    := '';
        qrlVlrICMS.Caption     := '';
        qrlVlrICMSST.Caption   := '';
      end;
    cst41:
      begin
//        qrlRedBaseCalc.Caption := '';
        qrlBaseCalc.Caption    := '';
        qrlAliqICMS.Caption    := '';
        qrlVlrICMS.Caption     := '';
        qrlVlrICMSST.Caption   := '';
      end;
    cst45:
      begin
//        qrlRedBaseCalc.Caption := '';
        qrlBaseCalc.Caption    := '';
        qrlAliqICMS.Caption    := '';
        qrlVlrICMS.Caption     := '';
        qrlVlrICMSST.Caption   := '';
      end;
    cst51:
      begin
//        qrlRedBaseCalc.Caption := '';
        qrlBaseCalc.Caption    := '';
        qrlAliqICMS.Caption    := '';
        qrlVlrICMS.Caption     := '';
        qrlVlrICMSST.Caption   := '';
      end;
    cst60:
      begin
//        qrlRedBaseCalc.Caption := '';
        qrlBaseCalc.Caption    := FormatFloat('###,###,###,##0.00', FCTe.Imp.ICMS.ICMS60.vBCSTRet);
        qrlAliqICMS.Caption    := FormatFloat('###,###,###,##0.00', FCTe.Imp.ICMS.ICMS60.pICMSSTRet);
        qrlVlrICMS.Caption     := FormatFloat('###,###,###,##0.00', FCTe.Imp.ICMS.ICMS60.vICMSSTRet);
        qrlVlrICMSST.Caption   := '';
      end;
    cst90:
      begin
//        qrlRedBaseCalc.Caption := CTeUtil.FormatarValor(msk4x2, FCTe.Imp.ICMS.ICMS90.pRedBC);
        qrlBaseCalc.Caption    := FormatFloat('###,###,###,##0.00', FCTe.Imp.ICMS.ICMS90.vBC);
        qrlAliqICMS.Caption    := FormatFloat('###,###,###,##0.00', FCTe.Imp.ICMS.ICMS90.pICMS);
        qrlVlrICMS.Caption     := FormatFloat('###,###,###,##0.00', FCTe.Imp.ICMS.ICMS90.vICMS);
        qrlVlrICMSST.Caption   := '';
      end;
    cstICMSOutraUF:
      begin
//        qrlRedBaseCalc.Caption := CTeUtil.FormatarValor(msk4x2, FCTe.Imp.ICMS.ICMSOutraUF.pRedBCOutraUF);
        qrlBaseCalc.Caption    := FormatFloat('###,###,###,##0.00', FCTe.Imp.ICMS.ICMSOutraUF.vBCOutraUF);
        qrlAliqICMS.Caption    := FormatFloat('###,###,###,##0.00', FCTe.Imp.ICMS.ICMSOutraUF.pICMSOutraUF);
        qrlVlrICMS.Caption     := FormatFloat('###,###,###,##0.00', FCTe.Imp.ICMS.ICMSOutraUF.vICMSOutraUF);
        qrlVlrICMSST.Caption   := '';
      end;
    cstICMSSN:
      begin
//        qrlRedBaseCalc.Caption := '';
        qrlBaseCalc.Caption    := '';
        qrlAliqICMS.Caption    := '';
        qrlVlrICMS.Caption     := '';
        qrlVlrICMSST.Caption   := '';
      end;
  end;
{$ENDIF}
end;

procedure TfrmDACTeQRRetrato2Vias.qrb_04_DadosNotaFiscalBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;
  // Incluido por Italo em 20/04/2011
  PrintBand := QRCTe.PageNumber = 1;

  // Imprime os dados da da Nota Fiscal se o Tipo de CTe for Normal
  // Incluido / Alterado por Italo e Doni em 24/09/2010
//  qrb_04_DadosNotaFiscal.Enabled:=(FCTe.Ide.tpCTe = tcNormal);
end;

procedure TfrmDACTeQRRetrato2Vias.qrb_05_ComplementoBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
var
 i: Integer;
begin
  inherited;
  // Incluido por Italo em 20/04/2011
  PrintBand := QRCTe.PageNumber = 1;

  // Imprime a lista dos CT-e Complementados se o Tipo de CTe for Complemento
  // Incluido / Alterado por Italo e Doni em 24/09/2010
//  qrmComplChave1 .Lines.Clear;
//  qrmComplValor1.Lines.Clear;
//  qrmComplChave2.Lines.Clear;
//  qrmComplValor2.Lines.Clear;
//  qrb_05_Complemento.Enabled:=(FCTe.Ide.tpCTe = tcComplemento);

  (*
  for i := 0 to FCTe.InfCTeComp.Count - 1 do
  begin
    case i of
      0..4:
        begin
//          qrmComplChave1.Lines.Add(FCTe.InfCTeComp[i].Chave);
//          qrmComplValor1.Lines.Add(CteUtil.FormatarValor(msk10x2, FCTe.InfCTeComp[i].vPresComp.vTPrest));
        end;
      5..9:
        begin
//          qrmComplChave2.Lines.Add(FCTe.InfCTeComp[i].Chave);
//          qrmComplValor2.Lines.Add(CteUtil.FormatarValor(msk10x2, FCTe.InfCTeComp[i].vPresComp.vTPrest));
        end;
    end;
  end;  *)
end;

procedure TfrmDACTeQRRetrato2Vias.qrb_06_ValorPrestacaoBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;
  // Incluido por Italo em 20/04/2011
  PrintBand := QRCTe.PageNumber = 1;
end;

procedure TfrmDACTeQRRetrato2Vias.qrb_07_HeaderItensBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;
  // Imprime os Documentos Originários se o Tipo de CTe for Normal
  // Incluido / Alterado por Italo e Doni em 24/09/2010
  // qrbHeaderItens.Enabled:=(FCTe.Ide.tpCTe = tcNormal);
end;

procedure TfrmDACTeQRRetrato2Vias.qrb_08_ItensBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
var
  i : integer;
begin
  inherited;

  // Imprime os Documentos Originários se o Tipo de CTe for Normal
  // Incluido / Alterado por Italo e Doni em 24/09/2010
  // TpcteTipoCTe = (tcNormal, tcComplemento, tcAnulacao, tcSubstituto);
//  qrb_08_Itens.Enabled:=(FCTe.Ide.tpCTe = tcNormal);

{  for i := 1 to 2 do
    if Trim(cdsDocumentos.FieldByName('DOCUMENTO_' + IntToStr(i)).AsString) = '' then
      TQRDBText(FindComponent('qrdbtCnpjEmitente' + intToStr(i))).Width := 325
    else
      TQRDBText(FindComponent('qrdbtCnpjEmitente' + intToStr(i))).Width := 128;}

  Inc(nItemControle);

  if QRCTe.PageNumber = 1 then
//    if QRCTe.RecordCount < (_NUM_ITEMS_PAGE1 + Fracionado) then
//      qrsFimItens.Enabled := (nItemControle = QRCTe.RecordCount)
//    else
//      qrsFimItens.Enabled := (nItemControle = (_NUM_ITEMS_PAGE1 + Fracionado))
  else
  begin
//    qrsFimItens.Enabled := (nItemControle = (_NUM_ITEMS_OTHERPAGES + Fracionado)) or
//      (QRCTe.RecordNumber = QRCTe.RecordCount) or
//      (cdsDocumentos.Eof);
  end;

//  if qrsFimItens.Enabled then
//    nItemControle := 0;
end;

procedure TfrmDACTeQRRetrato2Vias.qrb_09_ObsBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
var
 i: integer;
begin
  inherited;
  PrintBand := QRCTe.PageNumber = 1;

//  qrb_11_ModRodLot104.Enabled := False;

{$IFDEF PL_200}
  with FCTe.infCTeNorm.Rodo do
  begin
    qrlRNTRC.Caption := RNTRC;
{$ELSE}
  with FCTe.Rodo do
  begin
    qrlRNTRC.Caption := RNTRC;
{$ENDIF}



{$IFDEF PL_103}
    qrsCIOT.Enabled := False;
    lblCIOT.Enabled := False;
//    qrlCIOT.Enabled := False;
{$ENDIF}
{$IFDEF PL_104}
//    qrsCIOT.Enabled := True;
//    lblCIOT.Enabled := True;
//    qrlCIOT.Enabled := True;
//    qrlCIOT.Caption := CIOT;
{$ENDIF}

    case Lota of
      ltNao: begin
//              qrlLotacao.Caption          := 'NÃO';
             end;
      ltsim: begin
//              qrlLotacao.Caption          := 'SIM';
//              qrb_11_ModRodLot104.Enabled := True;
             end;
    end;

    qrlDtPrevEntrega.Caption := FormatDateTime('DD/MM/YYYY', dPrev);
  end;
                                      (*
//  qrmObs.Lines.BeginUpdate; // Linha inserida por Italo em 31/08/2010
//  qrmObs.Lines.Clear;

//  qrmObs.Lines.Add(StringReplace( FCTe.Compl.xObs, '&lt;BR&gt;', #13#10, [rfReplaceAll,rfIgnoreCase] ) );

  for i := 0 to FCTe.Compl.ObsCont.Count-1 do
   with FCTe.Compl.ObsCont.Items[i] do
    begin
     qrmObs.Lines.Add( StringReplace( xCampo, '&lt;BR&gt;', #13#10, [rfReplaceAll,rfIgnoreCase] )+': '+
                       StringReplace( xTexto, '&lt;BR&gt;', #13#10, [rfReplaceAll,rfIgnoreCase] ) );
    end;

  if FCTe.Ide.tpEmis in [teContingencia, teFSDA]
   then begin
    if not (FCTe.procCTe.cStat in [100, 101, 110])
     then qrmObs.Lines.Add('DACTE em Contingência - Impresso em decorrência de problemas técnicos.');
   end;

  if FCTe.Ide.tpEmis = teDPEC
   then qrmObs.Lines.Add('DACTE em Contingência - DPEC regularmente recebida pela Receita Federal do Brasil');

  qrmObs.Lines.Text:=StringReplace(qrmObs.Lines.Text,';',#13,[rfReplaceAll]);
  qrmObs.Lines.EndUpdate;

//  qrmObsFisco.Lines.BeginUpdate;
//  qrmObsFisco.Lines.Clear;

  for i := 0 to FCTe.Compl.ObsFisco.Count-1 do
   with FCTe.Compl.ObsFisco.Items[i] do
    begin
//     qrmObsFisco.Lines.Add( StringReplace( xCampo, '&lt;BR&gt;', #13#10, [rfReplaceAll,rfIgnoreCase] )+': '+
 //                           StringReplace( xTexto, '&lt;BR&gt;', #13#10, [rfReplaceAll,rfIgnoreCase] ) );
    end;

//  qrmObsFisco.Lines.Text := StringReplace( qrmObsFisco.Lines.Text, ';', #13, [rfReplaceAll] );
//  qrmObsFisco.Lines.EndUpdate;
                                      *)
  qrlMsgTeste.Visible := False;
  qrlMsgTeste.Enabled := False;

   if FCTe.Ide.tpAmb = taHomologacao then
   begin
//    qrlMsgTeste.Caption := 'AMBIENTE DE HOMOLOGAÇÃO - SEM VALOR FISCAL';
//    qrlMsgTeste.Visible := True;
//    qrlMsgTeste.Enabled := True;
   end else
   begin
    if FCTe.procCTe.cStat > 0 then
     begin
      if FCTe.procCTe.cStat = 101 then
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

procedure TfrmDACTeQRRetrato2Vias.qrb_11_ModRodLot104BeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
var
 i: Integer;
begin
  inherited;

  // Incluido por Italo em 18/10/2011
  PrintBand := QRCTe.PageNumber = 1;

//  qrmTipo2.Lines.Clear;
//  qrmPlaca2.Lines.Clear;
//  qrmUF2.Lines.Clear;
//  qrmRNTRC2.Lines.Clear;

{$IFDEF PL_200}
  for i:= 0 to FCTe.infCTeNorm.Rodo.veic.Count - 1 do
  begin
  //   if TpPropriedadeToStr(FCTe.Rodo.veic.Items[i].tpProp) = 'P'
  //    then qrmTipo2.Lines.Add('Próprio')
  //    else qrmTipo2.Lines.Add('Terceiro');
  // qrmPlaca2.Caption := '';
   qrlPlaca.Caption := FCTe.infCTeNorm.Rodo.veic.Items[i].placa;
  //   qrmUF2.Lines.Add(FCTe.Rodo.veic.Items[i].UF);
  //qrmRNTRC2.Caption := '';
   qrlRNTRC.Caption := FCTe.infCTeNorm.Rodo.veic.Items[i].Prop.RNTRC;
  end;
{$ELSE}
  for i:= 0 to FCTe.Rodo.veic.Count - 1 do
  begin
//   if TpPropriedadeToStr(FCTe.Rodo.veic.Items[i].tpProp) = 'P'
//    then qrmTipo2.Lines.Add('Próprio')
//    else qrmTipo2.Lines.Add('Terceiro');
  // qrmPlaca2.Caption := '';
   qrlPlaca.Caption := FCTe.Rodo.veic.Items[i].placa;
//   qrmUF2.Lines.Add(FCTe.Rodo.veic.Items[i].UF);
   //qrmRNTRC2.Caption := '';
   qrlRNTRC.Caption := FCTe.Rodo.veic.Items[i].Prop.RNTRC;
  end;
{$ENDIF}

//  qrmCNPJForn.Lines.Clear;
//  qrmNumCompra.Lines.Clear;
//  qrmCNPJPg.Lines.Clear;

{$IFDEF PL_104}
  for i := 0 to FCTe.Rodo.valePed.Count -1 do
  begin
//   qrmCNPJForn.Lines.Add(CteUtil.FormatarCNPJ(FCTe.Rodo.valePed.Items[i].CNPJForn));
//   qrmNumCompra.Lines.Add(FCTe.Rodo.valePed.Items[i].nCompra);
//   qrmCNPJPg.Lines.Add(CteUtil.FormatarCNPJ(FCTe.Rodo.valePed.Items[i].CNPJPg));
  end;
{$ENDIF}

//  qrlNomeMotorista2.Caption := '';
//  qrlCPFMotorista2.Caption  := '';
//  qrlLacres2.Caption        := '';

{$IFDEF PL_200}
  if FCTe.infCTeNorm.Rodo.moto.Count>0
   then begin
//    qrlNomeMotorista2.Caption := FCTe.Rodo.moto.Items[0].xNome;
//    qrlCPFMotorista2.Caption  := CteUtil.FormatarCNPJ(FCTe.Rodo.moto.Items[0].CPF);
   end;
{$ELSE}
  if FCTe.Rodo.moto.Count>0
   then begin
//    qrlNomeMotorista2.Caption := FCTe.Rodo.moto.Items[0].xNome;
//    qrlCPFMotorista2.Caption  := CteUtil.FormatarCNPJ(FCTe.Rodo.moto.Items[0].CPF);
   end;
{$ENDIF}

            (* Rafael
  for i := 0 to FCTe.infCTeNorm.Rodo.Lacres.Count - 1 do
  begin
//   qrlLacres2.Caption := qrlLacres2.Caption + FCTe.Rodo.Lacres.Items[i].nLacre + '/';
  end;    *)
end;

procedure TfrmDACTeQRRetrato2Vias.qrb_12_ModAereoBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;
  // Incluido por Italo em 06/05/2011
  PrintBand := QRCTe.PageNumber = 1;
  {*
  ** Alterado por: Brian Collo Gonçalves e Eric Helfenstens
  ** Data: 17/03/2011
  ** Descrição: Incluida banda para os dados especificos do Modal Aéreo
  ** e carregamento de suas respectivas informações.
  *}
//  qrb_12_ModAereo.Enabled := (FCTe.Ide.tpCTe = tcNormal) and (FCTe.Ide.modal = mdAereo);

//  qrlCaracAdServico.Caption    := FCTe.Compl.xCaracSer;
//  qrlCaracAdTransporte.Caption := FCTe.Compl.xCaracAd;
   (* Rafael
  with FCTe.infCTeNorm.Aereo do
  begin
//    qrlAWB.Caption           := nOCA;
  {$IFDEF PL_103}
//    qrlTrecho.Caption        := tarifa.trecho;
//    qrlContaCorrente.Caption := cIATA; // ??? Conta Corrente ???
  {$ENDIF}
//    qrlTarifaCL.Caption      := tarifa.CL;
//    qrlTarifaCodigo.Caption  := tarifa.cTar;
//    qrlTarifaValor.Caption   := FormatCurr('###,###,##0.00', tarifa.vTar);
  {$IFDEF PL_104}
//    qrlContaCorrente.Caption := IdT; // ??? Conta Corrente ???
  {$ENDIF}
//    qrlMinuta.Caption        := FormatFloat('0000000000', nMinu);

//    qrlLojaAgenteEmissor.Caption := xLAgEmi;
  end; *)

//  if FCte.Ide.retira = rtSim
//   then qrlRetira.Caption := 'SIM'
//   else qrlRetira.Caption := 'NÃO';
//  qrlDadosRetira.Caption  := FCte.Ide.xdetretira;
end;

procedure TfrmDACTeQRRetrato2Vias.qrb_13_ModAquaviarioBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;
  // Incluido por Italo em 06/05/2011
  PrintBand := QRCTe.PageNumber = 1;
//  qrb_13_ModAquaviario.Enabled := (FCTe.Ide.tpCTe = tcNormal) and (FCTe.Ide.modal = mdAquaviario);
end;

procedure TfrmDACTeQRRetrato2Vias.qrb_14_ModFerroviarioBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
var
 i: Integer;
begin
  inherited;
  // Incluido por Italo em 06/05/2011
  PrintBand := QRCTe.PageNumber = 1;
//  qrb_14_ModFerroviario.Enabled := (FCTe.Ide.tpCTe = tcNormal) and (FCTe.Ide.modal = mdFerroviario);

//  qrlPortoEmbarque.Caption     := FCTe.Aquav.prtEmb;
//  qrlPortoDestino.Caption      := FCTe.Aquav.prtDest;
//  qrlIndNavioRebocador.Caption := FCTe.Aquav.xNavio;

//  qrlBCAFRMM.Caption    := FormatCurr('###,###,##0.00', FCTe.Aquav.vPrest);
//  qrlValorAFRMM.Caption := FormatCurr('###,###,##0.00', FCTe.Aquav.vAFRMM);

//  case FCTe.Aquav.tpNav of
//   tnInterior:  qrlTipoNav.Caption := 'INTERIOR';
//   tnCabotagem: qrlTipoNav .Caption := 'CABOTAGEM';
//  end;

//  case FCTe.Aquav.direc of
//   drNorte: qrlDirecao.Caption := 'NORTE';
//   drLeste: qrlDirecao.Caption := 'LESTE';
//   drSul:   qrlDirecao.Caption := 'SUL';
//   drOeste: qrlDirecao.Caption := 'OESTE';
//  end;

//  qrlIndConteiners.Caption := '';
(*  Rafael for i := 0 to Aquav.Lacre.Count - 1 do
   begin
//    if i = 0
//     then qrlIndConteiners.Caption := FCTe.Aquav.Lacre.Items[i].nLacre
//     else qrlIndConteiners.Caption := qrlIndConteiners.Caption + '/' + FCTe.Aquav.Lacre.Items[i].nLacre;
   end;   *)
end;

procedure TfrmDACTeQRRetrato2Vias.qrb_15_ModDutoviarioBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;
  // Incluido por Italo em 06/05/2011
  PrintBand := QRCTe.PageNumber = 1;
//  qrb_15_ModDutoviario.Enabled := (FCTe.Ide.tpCTe = tcNormal) and (FCTe.Ide.modal = mdDutoviario);
end;

procedure TfrmDACTeQRRetrato2Vias.qrb_17_SistemaBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
begin
  inherited;
  // Incluido por Italo em 20/04/2011
  PrintBand := QRCTe.PageNumber = 1;
end;

procedure TfrmDACTeQRRetrato2Vias.qrb_18_ReciboBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;
  // Incluido Novamente por Italo em 10/11/2011
  // Incluido/Alterado por Italo em 20/04/2011 / 27/04/2011 / 04/07/2011
  PrintBand := (QRCTe.PageNumber = 1);

//  qrlSerie3.Caption  := FormatFloat( '000', FCTe.Ide.serie);
  qrlNumCte3.Caption := FormatFloat( '000,000,000', FCTe.Ide.nCT );

  // Incluido/Aterado por Italo em 27/04/2011 / 04/07/2011 / 10/05/2012
  // TpcteTipoCTe = (tcNormal, tcComplemento, tcAnulacao, tcSubstituto);
  if PrintBand
   then begin
//    qrb_18_Recibo.Enabled := (FCTe.Ide.tpCTe = tcNormal) and (FCTe.Ide.modal <> mdAereo) and (FPosRecibo = prRodape);
//    if qrb_18_Recibo.Enabled
//     then qrb_18_Recibo.Height  := 68
//     else qrb_18_Recibo.Height  := 0;
   end;
end;

end.

