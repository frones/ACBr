{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2009   http://acbr.sf.net                   }
{                                                                              }
{ Colaboradores nesse arquivo: Juliana Rodrigues Prado,                        }
{                              Paulo Henrique M. Gurgel, Daniel Simoes Almeida }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 05/07/2010: Paulo Henrique Moreira Gurgel
|*  - Criação da unit para quickReport
******************************************************************************}
{$I ACBr.inc}

unit ACBrBoletoFCQuickFr;

interface

uses
  SysUtils, StdCtrls, Classes, Graphics, Controls, Forms, Dialogs, ACBrBoleto,
  QRMultiExport, QRCtrls, QuickRpt, ExtCtrls, QRExport, ACBrBarCode,
  QRWebFilt {, QRWebFilt};

const
  CACBrBoletoFCQuick_Versao = '0.0.14a' ;

type

  { TACBrBoletoFCQuickFr }
  TACBrBoletoFCQuick = class(TACBrBoletoFCClass)
  private
    { Private declarations }
  public
    { Public declarations }
    Constructor Create(AOwner: TComponent); override;

    procedure Imprimir; override;
  end;

  TACBrBoletoFCQuickFr = class(TForm)
    LayoutBoleto: TQuickRep;
    RLBandDatail: TQRBand;
    RLDraw38: TQRShape;
    RLDraw37: TQRShape;
    RLDraw34: TQRShape;
    RLDraw28: TQRShape;
    RLDraw27: TQRShape;
    RLDraw25: TQRShape;
    RLDraw24: TQRShape;
    RLDraw23: TQRShape;
    RLDraw22: TQRShape;
    RLDraw20: TQRShape;
    RLDraw19: TQRShape;
    RLDraw18: TQRShape;
    RLDraw16: TQRShape;
    imgBanco2: TQrImage;
    txtNumeroBanco2: TQrLabel;
    RLLabel67: TQrLabel;
    RLLabel68: TQrLabel;
    lblLocalPagto: TQrLabel;
    RLLabel69: TQrLabel;
    txtNomeCedente2: TQrLabel;
    RLLabel70: TQrLabel;
    txtDataDocumento2: TQrLabel;
    RLLabel71: TQrLabel;
    txtNumeroDocumento2: TQrLabel;
    RLLabel72: TQrLabel;
    txtEspecieDoc2: TQrLabel;
    RLLabel73: TQrLabel;
    txtAceite2: TQrLabel;
    RLLabel74: TQrLabel;
    txtDataProcessamento2: TQrLabel;
    txtUsoBanco2: TQrLabel;
    RLLabel75: TQrLabel;
    txtCarteira2: TQrLabel;
    RLLabel76: TQrLabel;
    RLLabel77: TQrLabel;
    txtEspecie2: TQrLabel;
    RLLabel78: TQrLabel;
    RLLabel79: TQrLabel;
    RLLabel80: TQrLabel;
    txtInstrucoes2: TQrMemo;
    RLLabel81: TQrLabel;
    txtDataVencimento2: TQrLabel;
    RLLabel82: TQrLabel;
    txtCodigoCedente2: TQrLabel;
    RLLabel83: TQrLabel;
    txtNossoNumero2: TQrLabel;
    RLLabel84: TQrLabel;
    txtValorDocumento2: TQrLabel;
    RLLabel85: TQrLabel;
    RLLabel86: TQrLabel;
    RLLabel87: TQrLabel;
    RLLabel88: TQrLabel;
    txtNomeSacado2: TQrLabel;
    txtEnderecoSacado2: TQrLabel;
    txtCidadeSacado2: TQrLabel;
    RLLabel89: TQrLabel;
    txtCpfCnpjSacado2: TQrLabel;
    RLLabel90: TQrLabel;
    RLMemo2: TQrMemo;
    RLLabel91: TQrLabel;
    RLLabel93: TQrLabel;
    RLLabel98: TQrLabel;
    RLLabel102: TQrLabel;
    txtSacadorAvalista2: TQrLabel;
    QRShape1: TQRShape;
    QRShape2: TQRShape;
    ACBrBarCode1: TACBrBarCode;
    txtValorDocumento3: TQRLabel;
    txtUsoBanco3: TQRLabel;
    txtSwHouse: TQRLabel;
    txtSacadorAvalista3: TQRLabel;
    txtNumeroDocumento3: TQRLabel;
    txtNumeroBanco3: TQRLabel;
    txtNossoNumero3: TQRLabel;
    txtNomeSacado3: TQRLabel;
    txtNomeCedente3: TQRLabel;
    txtLocalPagamento3: TQRLabel;
    txtLinhaDigitavel: TQRLabel;
    txtInstrucoes3: TQRMemo;
    txtEspecieDoc3: TQRLabel;
    txtEspecie3: TQRLabel;
    txtEnderecoSacado3: TQRLabel;
    txtDataVencimento3: TQRLabel;
    txtDataProcessamento3: TQRLabel;
    txtDataDocumento3: TQRLabel;
    txtCpfCnpjSacado3: TQRLabel;
    txtCodigoCedente3: TQRLabel;
    txtCidadeSacado3: TQRLabel;
    txtCarteira3: TQRLabel;
    txtAceite3: TQRLabel;
    RLLabel175: TQRLabel;
    RLLabel170: TQRLabel;
    RLLabel168: TQRLabel;
    RLLabel167: TQRLabel;
    RLLabel166: TQRLabel;
    RLLabel165: TQRLabel;
    RLLabel164: TQRLabel;
    RLLabel163: TQRLabel;
    RLLabel162: TQRLabel;
    RLLabel161: TQRLabel;
    RLLabel160: TQRLabel;
    RLLabel159: TQRLabel;
    RLLabel158: TQRLabel;
    RLLabel157: TQRLabel;
    RLLabel156: TQRLabel;
    RLLabel155: TQRLabel;
    RLLabel154: TQRLabel;
    RLLabel153: TQRLabel;
    RLLabel152: TQRLabel;
    RLLabel151: TQRLabel;
    RLLabel150: TQRLabel;
    RLLabel149: TQRLabel;
    RLLabel148: TQRLabel;
    RLLabel147: TQRLabel;
    RLLabel146: TQRLabel;
    RLLabel145: TQRLabel;
    QRShape9: TQRShape;
    QRShape8: TQRShape;
    QRShape7: TQRShape;
    QRShape6: TQRShape;
    QRShape5: TQRShape;
    QRShape4: TQRShape;
    QRShape3: TQRShape;
    QRShape17: TQRShape;
    QRShape16: TQRShape;
    QRShape15: TQRShape;
    QRShape14: TQRShape;
    QRShape13: TQRShape;
    QRShape12: TQRShape;
    QRShape11: TQRShape;
    QRShape10: TQRShape;
    QRLabel1: TQRLabel;
    imgBarra: TQRImage;
    imgBanco3: TQRImage;
    QRHTMLFilter1: TQRHTMLFilter;
    BoletoCarne: TQuickRep;
    rbLCDetail: TQRBand;
    QRLabel2: TQRLabel;
    txtNumeroDocumento4: TQRLabel;
    txtLinhaDigitavel4: TQRLabel;
    txtInstrucoes4: TQRMemo;
    txtAceite4: TQRLabel;
    QRLabel7: TQRLabel;
    QRLabel8: TQRLabel;
    QRLabel9: TQRLabel;
    QRLabel10: TQRLabel;
    QRShape20: TQRShape;
    QRShape21: TQRShape;
    QRShape23: TQRShape;
    QRShape24: TQRShape;
    QRShape25: TQRShape;
    QRShape26: TQRShape;
    QRShape29: TQRShape;
    QRShape30: TQRShape;
    imgBarra4: TQRImage;
    imgBanco4: TQRImage;
    txtLocalPagamento4: TQRLabel;
    QRLabel13: TQRLabel;
    QRLabel14: TQRLabel;
    txtDataVencimento4: TQRLabel;
    QRLabel16: TQRLabel;
    txtNomeCedente4: TQRLabel;
    QRLabel18: TQRLabel;
    txtDataDocumento4: TQRLabel;
    QRLabel20: TQRLabel;
    txtEspecieDoc: TQRLabel;
    QRLabel22: TQRLabel;
    txtDataProcessamento4: TQRLabel;
    QRLabel24: TQRLabel;
    txtNossoNumero4: TQRLabel;
    QRLabel26: TQRLabel;
    QRLabel27: TQRLabel;
    txtCodigoCedente4: TQRLabel;
    QRLabel29: TQRLabel;
    QRLabel30: TQRLabel;
    QRLabel31: TQRLabel;
    txtEspecieMod: TQRLabel;
    QRLabel33: TQRLabel;
    txtCarteira4: TQRLabel;
    QRLabel35: TQRLabel;
    txtUsoBanco4: TQRLabel;
    QRLabel37: TQRLabel;
    txtValorDocumento4: TQRLabel;
    txtNumeroBanco4: TQRLabel;
    QRShape31: TQRShape;
    QRLabel40: TQRLabel;
    txtCpfCnpjSacado4: TQRLabel;
    QRLabel42: TQRLabel;
    QRLabel43: TQRLabel;
    QRLabel44: TQRLabel;
    QRLabel45: TQRLabel;
    txtNomeSacado4: TQRLabel;
    txtEnderecoSacado4: TQRLabel;
    QRLabel48: TQRLabel;
    QRLabel49: TQRLabel;
    txtCidadeSacado4: TQRLabel;
    QRShape19: TQRShape;
    QRShape22: TQRShape;
    QRShape18: TQRShape;
    QRShape27: TQRShape;
    QRShape28: TQRShape;
    QRShape33: TQRShape;
    QRShape34: TQRShape;
    QRShape35: TQRShape;
    QRShape32: TQRShape;
    QRShape36: TQRShape;
    QRShape37: TQRShape;
    QRShape38: TQRShape;
    QRShape39: TQRShape;
    QRShape40: TQRShape;
    QRShape41: TQRShape;
    QRShape42: TQRShape;
    QRShape43: TQRShape;
    QRLabel5: TQRLabel;
    QRLabel6: TQRLabel;
    QRLabel11: TQRLabel;
    QRLabel12: TQRLabel;
    QRLabel15: TQRLabel;
    QRLabel17: TQRLabel;
    QRLabel19: TQRLabel;
    QRLabel21: TQRLabel;
    QRLabel23: TQRLabel;
    QRLabel25: TQRLabel;
    QRLabel28: TQRLabel;
    QRLabel32: TQRLabel;
    QRLabel3: TQRLabel;
    QRLabel4: TQRLabel;
    QRShape44: TQRShape;
    QRShape45: TQRShape;
    txtDataVencimento5: TQRLabel;
    txtParcela: TQRLabel;
    txtCodigoCedente5: TQRLabel;
    txtValorDocumento5: TQRLabel;
    txtNumeroDocumento5: TQRLabel;
    txtNossoNumero5: TQRLabel;
    Label1: TLabel;
    txtNomeSacado5: TQRLabel;
    txtEspecie5: TQRLabel;
    LayOutPadraoEntrega: TQuickRep;
    RLMemo6_1: TQRBand;
    QRShape46: TQRShape;
    QRShape47: TQRShape;
    QRShape48: TQRShape;
    QRShape49: TQRShape;
    QRShape50: TQRShape;
    QRShape51: TQRShape;
    QRShape52: TQRShape;
    QRShape53: TQRShape;
    QRShape54: TQRShape;
    QRShape55: TQRShape;
    QRShape56: TQRShape;
    QRShape57: TQRShape;
    QRShape58: TQRShape;
    QRShape59: TQRShape;
    QRShape60: TQRShape;
    imgBanco6_1: TQRImage;
    txtNumeroBanco6_1: TQRLabel;
    QRLabel36: TQRLabel;
    QRLabel38: TQRLabel;
    lblLocalPagto6_1: TQRLabel;
    QRLabel41: TQRLabel;
    txtNomeCedente6_1: TQRLabel;
    QRLabel47: TQRLabel;
    txtDataDocumento6_1: TQRLabel;
    QRLabel51: TQRLabel;
    txtNumeroDocumento6_1: TQRLabel;
    QRLabel53: TQRLabel;
    txtEspecieDoc6_1: TQRLabel;
    QRLabel55: TQRLabel;
    txtAceite6_1: TQRLabel;
    QRLabel57: TQRLabel;
    txtDataProcessamento6_1: TQRLabel;
    txtUsoBanco6_1: TQRLabel;
    QRLabel60: TQRLabel;
    txtCarteira6_1: TQRLabel;
    QRLabel62: TQRLabel;
    QRLabel63: TQRLabel;
    txtEspecie6_1: TQRLabel;
    QRLabel65: TQRLabel;
    QRLabel66: TQRLabel;
    QRLabel67: TQRLabel;
    txtInstrucoes6_1: TQRMemo;
    QRLabel68: TQRLabel;
    txtDataVencimento6_1: TQRLabel;
    QRLabel70: TQRLabel;
    txtCodigoCedente6_1: TQRLabel;
    QRLabel72: TQRLabel;
    txtNossoNumero6_1: TQRLabel;
    QRLabel74: TQRLabel;
    txtValorDocumento6_1: TQRLabel;
    QRLabel76: TQRLabel;
    QRLabel77: TQRLabel;
    QRLabel78: TQRLabel;
    QRLabel79: TQRLabel;
    txtNomeSacado6_1: TQRLabel;
    txtEnderecoSacado6_1: TQRLabel;
    txtCidadeSacado6_1: TQRLabel;
    QRLabel83: TQRLabel;
    txtCpfCnpjSacado6_1: TQRLabel;
    QRLabel85: TQRLabel;
    QRMemo2: TQRMemo;
    QRLabel86: TQRLabel;
    QRLabel87: TQRLabel;
    QRLabel88: TQRLabel;
    QRLabel89: TQRLabel;
    QRLabel90: TQRLabel;
    QRLabel91: TQRLabel;
    txtNumeroDocumento6_2: TQRLabel;
    txtLinhaDigitavel6_2: TQRLabel;
    txtInstrucoes6_2: TQRMemo;
    txtAceite6_2: TQRLabel;
    QRLabel95: TQRLabel;
    QRLabel96: TQRLabel;
    QRLabel97: TQRLabel;
    QRLabel98: TQRLabel;
    QRLabel99: TQRLabel;
    QRShape61: TQRShape;
    QRShape62: TQRShape;
    QRShape63: TQRShape;
    QRShape64: TQRShape;
    QRShape65: TQRShape;
    QRShape66: TQRShape;
    QRShape67: TQRShape;
    QRShape68: TQRShape;
    QRShape69: TQRShape;
    QRShape70: TQRShape;
    QRShape71: TQRShape;
    QRShape72: TQRShape;
    QRShape73: TQRShape;
    QRShape74: TQRShape;
    QRLabel100: TQRLabel;
    imgBarra6_2: TQRImage;
    imgBanco6_2: TQRImage;
    txtLocalPagamento6_2: TQRLabel;
    QRLabel102: TQRLabel;
    QRLabel103: TQRLabel;
    txtDataVencimento6_2: TQRLabel;
    QRLabel105: TQRLabel;
    txtNomeCedente6_2: TQRLabel;
    QRLabel107: TQRLabel;
    txtDataDocumento6_2: TQRLabel;
    QRLabel109: TQRLabel;
    txtEspecieDoc6_2: TQRLabel;
    QRLabel111: TQRLabel;
    txtDataProcessamento6_2: TQRLabel;
    QRLabel113: TQRLabel;
    txtNossoNumero6_2: TQRLabel;
    QRLabel115: TQRLabel;
    QRLabel116: TQRLabel;
    txtCodigoCedente6_2: TQRLabel;
    QRLabel118: TQRLabel;
    QRLabel119: TQRLabel;
    QRLabel120: TQRLabel;
    txtEspecie6_2: TQRLabel;
    QRLabel122: TQRLabel;
    txtCarteira6_2: TQRLabel;
    QRLabel124: TQRLabel;
    txtUsoBanco6_2: TQRLabel;
    QRLabel126: TQRLabel;
    txtValorDocumento6_2: TQRLabel;
    txtNumeroBanco6_2: TQRLabel;
    QRShape75: TQRShape;
    QRLabel129: TQRLabel;
    txtCpfCnpjSacado6_2: TQRLabel;
    QRLabel131: TQRLabel;
    QRLabel132: TQRLabel;
    QRLabel133: TQRLabel;
    QRLabel134: TQRLabel;
    txtNomeSacado6_2: TQRLabel;
    txtEnderecoSacado6_2: TQRLabel;
    QRLabel137: TQRLabel;
    QRLabel138: TQRLabel;
    txtCidadeSacado6_2: TQRLabel;
    QRShape91: TQRShape;
    QRShape92: TQRShape;
    QRShape93: TQRShape;
    QRShape94: TQRShape;
    QRShape95: TQRShape;
    QRShape96: TQRShape;
    QRShape99: TQRShape;
    QRShape100: TQRShape;
    QRShape101: TQRShape;
    QRShape104: TQRShape;
    imgBanco6_0: TQRImage;
    txtNumeroBanco6_0: TQRLabel;
    QRLabel150: TQRLabel;
    lblLocalPagto6_0: TQRLabel;
    txtNomeCedente6_0: TQRLabel;
    QRLabel157: TQRLabel;
    txtNumeroDocumento6_0: TQRLabel;
    QRLabel159: TQRLabel;
    txtEspecieDoc6_0: TQRLabel;
    QRLabel161: TQRLabel;
    txtAceite6_0: TQRLabel;
    QRLabel163: TQRLabel;
    txtDataProcessamento6_0: TQRLabel;
    QRLabel172: TQRLabel;
    QRLabel174: TQRLabel;
    txtDataVencimento6_0: TQRLabel;
    QRLabel176: TQRLabel;
    txtCodigoCedente6_0: TQRLabel;
    QRLabel178: TQRLabel;
    txtNossoNumero6_0: TQRLabel;
    QRLabel180: TQRLabel;
    txtValorDocumento6_0: TQRLabel;
    txtNomeSacado6_0: TQRLabel;
    txtEnderecoSacado6_0: TQRLabel;
    txtCidadeSacado6_0: TQRLabel;
    QRLabel189: TQRLabel;
    txtCpfCnpjSacado6_0: TQRLabel;
    QRLabel151: TQRLabel;
    QRLabel153: TQRLabel;
    QRLabel155: TQRLabel;
    txtDataDocumento6_0: TQRLabel;
    QRLabel166: TQRLabel;
    QRLabel185: TQRLabel;
    mOrientacoesBanco: TQRMemo;
    mPadraoOrientbanco: TQRMemo;
    procedure FormCreate(Sender: TObject);
    procedure LayoutBoletoNeedData(Sender: TObject; var MoreData: Boolean);
    procedure RLBandDatailBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure LayoutBoletoStartPage(Sender: TCustomQuickRep);
    procedure BoletoCarneNeedData(Sender: TObject; var MoreData: Boolean);
    procedure rbLCDetailBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure LayoutBoletoBeforePrint(Sender: TCustomQuickRep;
      var PrintReport: Boolean);
    procedure BoletoCarneBeforePrint(Sender: TCustomQuickRep;
      var PrintReport: Boolean);
    procedure LayOutPadraoEntregaBeforePrint(Sender: TCustomQuickRep;
      var PrintReport: Boolean);
    procedure LayOutPadraoEntregaNeedData(Sender: TObject;
      var MoreData: Boolean);
    procedure LayOutPadraoEntregaStartPage(Sender: TCustomQuickRep);
    procedure RLMemo6_1BeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
  private
     fBoletoFC: TACBrBoletoFCQuick;
     fIndice: Integer;
     function GetACBrTitulo: TACBrTitulo;
    { Private declarations }
  public
    { Public declarations }
    property Indice   : Integer read fIndice ;
    property BoletoFC : TACBrBoletoFCQuick read fBoletoFC ;
    property Titulo   : TACBrTitulo read GetACBrTitulo ;
  end;

procedure Register;

implementation

Uses ACBrUtil, strutils, AJBarcode;

{$R *.dfm}
{$R ACBrBoletoFCQuick.dcr}

procedure Register;
begin
  RegisterComponents('ACBrBoleto',[TACBrBoletoFCQuick]);
end;

{ TACBrBoletoFCQuick }

constructor TACBrBoletoFCQuick.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fpAbout := 'ACBRBoletoFCQuick ver: '+CACBrBoletoFCQuick_Versao;
end;

procedure TACBrBoletoFCQuick.Imprimir;
var
  frACBrBoletoQuick : TACBRBoletoFCQuickFr;
  RLLayout: TQuickRep;
  //MultiExport : TQRMultiExport ;
  //FiltroExportacao: TQRPAbstractExportFilter;
  //HTMLFilter  : TQRHTMLDocumentFilter ;
begin
  inherited Imprimir;    // Executa verificações padroes

  frACBrBoletoQuick := TACBrBoletoFCQuickFr.Create(Self);
  try
     with frACBrBoletoQuick do
     begin
        case LayOut of
           lCarne  : RLLayout:= BoletoCarne;
           lPadraoEntrega  : RLLayout:= LayoutPadraoEntrega;
        else
           RLLayout := LayoutBoleto;
        end;

        RLLayout.PrinterSettings.Copies := NumCopias ;

        case Filtro of
          fiPDF  :
            begin
//              FiltroExportacao := TQRPDFDocumentFilter.Create(NomeArquivo);
              try
//                TQRPDFDocumentFilter(FiltroExportacao).CompressionOn := true;
//                RLLayout.ExportToFilter(FiltroExportacao);
              finally
//                FiltroExportacao.Free;
              end;
//               MultiExport := TQRMultiExport.Create(self);
//               try
//                  MultiExport.Report       := RLLayout ;
//                  MultiExport.ExportFormat := qrxPDF ;
//                  MultiExport.FileName     := NomeArquivo ;
//                  MultiExport.ShowDialog   := MostrarSetup ;
//                  MultiExport.DPI          := 300 ;
//                  MultiExport.DoExport ;
//               finally
//                  MultiExport.Free ;
//               end ;
            end ;

          fiHTML :
            begin
               (*  Codigo abaixo comentado pois  não funciona...
               HTMLFilter := TQRHTMLDocumentFilter.Create( NomeArquivo );
               try
                  RLLayout.ExportToFilter( HTMLFilter );
               finally
                  HTMLFilter.Free ;
               end ;
               *)
               raise Exception.Create( ACBrStr('Exportação para HTML não implementada no QuickReport') ) ;
            end ;
        else
          begin
            if MostrarPreview then
               RLLayout.PreviewModal
            else
             begin
               if MostrarSetup then
                  RLLayout.PrinterSetup ;

               if (RLLayout.Tag = 0) then
                  RLLayout.Print;
             end ;
          end
        end ;
     end ;
  finally
     frACBrBoletoQuick.Free ;
  end;
end;


{ TACBrBoletoFCQuickFr }

procedure TACBrBoletoFCQuickFr.FormCreate(Sender: TObject);
begin
   fBoletoFC := TACBrBoletoFCQuick(Owner) ;  // Link para o Pai
   txtSwHouse.Caption := BoletoFC.SoftwareHouse ;
   ACBrBarCode1.Width := imgBarra.Width ;
   ACBrBarCode1.Height:= imgBarra.Height ;
   ACBrBarCode1.Ratio:=3;
   ACBrBarCode1.Modul:=1;
end;

function TACBrBoletoFCQuickFr.GetACBrTitulo: TACBrTitulo;
begin
   Result := fBoletoFC.ACBrBoleto.ListadeBoletos[ fIndice ] ;
end;

procedure TACBrBoletoFCQuickFr.LayoutBoletoNeedData(Sender: TObject;
  var MoreData: Boolean);
begin
   MoreData := (fIndice < fBoletoFC.ACBrBoleto.ListadeBoletos.Count) ;
end;

procedure TACBrBoletoFCQuickFr.RLBandDatailBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
Var
   NossoNum, CodCedente : String;
   CodBarras, LinhaDigitavel : String;
   BMP : TBitmap ;
begin
   with fBoletoFC.ACBrBoleto do
   begin
      NossoNum    := Banco.MontarCampoNossoNumero( Titulo );
      CodCedente  := Banco.MontarCampoCodigoCedente(titulo);

      fBoletoFC.CarregaLogo( imgBanco2.Picture, Banco.Numero );
      txtNumeroBanco2.Caption         := IntToStrZero(Banco.Numero, 3)+ '-' +
                                         IfThen(Banco.Digito >= 10,'X',IntToStrZero(Banco.Digito, 1));
      lblLocalPagto.Caption           := Titulo.LocalPagamento;
            
      txtDataVencimento2.Caption := ifthen(Titulo.Vencimento = 0, '', FormatDateTime('dd/mm/yyyy', Titulo.Vencimento));
      
      txtNomeCedente2.Caption         := Cedente.Nome;
      txtCodigoCedente2.Caption       := CodCedente;
      txtDataDocumento2.Caption       := FormatDateTime('dd/mm/yyyy', Titulo.DataDocumento);
      txtNumeroDocumento2.Caption     := Titulo.NumeroDocumento;
      txtEspecieDoc2.Caption          := Titulo.EspecieDoc;
      txtAceite2.Caption              := ifThen(Titulo.Aceite = atSim,'S','N');
      txtDataProcessamento2.Caption   := FormatDateTime('dd/mm/yyyy',Now);
      txtNossoNumero2.Caption         := NossoNum;
      txtUsoBanco2.Caption            := Titulo.UsoBanco;
      txtCarteira2.Caption            := Titulo.Carteira;
      txtEspecie2.Caption             := IfThen(Titulo.EspecieMod = '', 'R$', Titulo.EspecieMod);
      
      txtValorDocumento2.Caption := ifthen(Titulo.ValorDocumento > 0,FormatFloat('###,###,##0.00',Titulo.ValorDocumento), '');
      
      txtNomeSacado2.Caption          := Titulo.Sacado.NomeSacado;
      txtEnderecoSacado2.Caption      := Titulo.Sacado.Logradouro + ' '+
                                         Titulo.Sacado.Numero + Titulo.Sacado.Complemento;
      txtCidadeSacado2.Caption        := Titulo.Sacado.CEP + ' '+Titulo.Sacado.Bairro + ' '+Titulo.Sacado.Cidade +
                                         ' '+Titulo.Sacado.UF;
      txtCpfCnpjSacado2.Caption       := Titulo.Sacado.CNPJCPF;
      txtInstrucoes2.Lines.Text       := Titulo.Mensagem.Text;


     CodBarras      := Banco.MontarCodigoBarras( Titulo );
     LinhaDigitavel := Banco.MontarLinhaDigitavel( CodBarras, Titulo );

     imgBanco3.Picture.Assign(imgBanco2.Picture);
     txtNumeroBanco3.Caption         := txtNumeroBanco2.Caption;
     txtLocalPagamento3.Caption      := lblLocalPagto.Caption;
     txtDataVencimento3.Caption      := txtDataVencimento2.Caption;
     txtNomeCedente3.Caption         := txtNomeCedente2.Caption;
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
     txtNomeSacado3.Caption          := txtNomeSacado2.Caption;
     txtEnderecoSacado3.Caption      := txtEnderecoSacado2.Caption;
     txtCidadeSacado3.Caption        := txtCidadeSacado2.Caption;
     txtCpfCnpjSacado3.Caption       := txtCpfCnpjSacado2.Caption;
     txtLinhaDigitavel.Caption       := LinhaDigitavel;
     txtInstrucoes3.Lines.Text       := txtInstrucoes2.Lines.Text;

     mOrientacoesBanco.Lines.Text  := Banco.OrientacoesBanco.Text;
     mPadraoOrientbanco.Lines.Text := Banco.OrientacoesBanco.Text;

     ACBrBarCode1.Typ  := bcCode_2_5_interleaved ;
     ACBrBarCode1.Text := CodBarras;
     BMP := TBitmap.Create ;
     try
        BMP.Width  := imgBarra.Width ;
        BMP.Height := imgBarra.Height ;
        ACBrBarCode1.DrawBarcode(BMP.Canvas);

        imgBarra.Picture.Bitmap := BMP ;
      imgBarra4.Picture.Bitmap := BMP;
     finally
        BMP.Free ;
     end ;
   end;
end;

procedure TACBrBoletoFCQuickFr.LayoutBoletoStartPage(
  Sender: TCustomQuickRep);
begin
  Inc( fIndice ) ;
end;

procedure TACBrBoletoFCQuickFr.BoletoCarneNeedData(Sender: TObject;
  var MoreData: Boolean);
begin
  MoreData := (fIndice < fBoletoFC.ACBrBoleto.ListadeBoletos.Count);
end;

procedure TACBrBoletoFCQuickFr.rbLCDetailBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
Var
  NossoNum, CodCedente : String;
  CodBarras, LinhaDigitavel : String;
  BMP : TBitmap ;
  viPar, viPla: String;
begin
   with fBoletoFC.ACBrBoleto do
   begin
    viPar       := IntToStr(fIndice+1);
    viPla       := IntToStr(ListadeBoletos.Count);
    NossoNum       := Banco.MontarCampoNossoNumero( Titulo );
    CodCedente  := Banco.MontarCampoCodigoCedente(titulo);

    fBoletoFC.CarregaLogo( imgBanco4.Picture, Banco.Numero );

      CodBarras      := Banco.MontarCodigoBarras( Titulo );
      LinhaDigitavel := Banco.MontarLinhaDigitavel( CodBarras, Titulo );

    txtNumeroBanco4.Caption         := IntToStrZero(Banco.Numero, 3)+ '-' +
                                        ifthen(Banco.Digito >= 10,'X',IntToStrZero(Banco.Digito, 1));
    txtLocalPagamento4.Caption      := Titulo.LocalPagamento;
       
    txtDataVencimento4.Caption := ifthen(Titulo.Vencimento = 0, '', FormatDateTime('dd/mm/yyyy', Titulo.Vencimento));    

    txtNomeCedente4.Caption         := Cedente.Nome;
    txtCodigoCedente4.Caption       := CodCedente;
    txtDataDocumento4.Caption       := FormatDateTime('dd/mm/yyyy', Titulo.DataDocumento);
    txtNumeroDocumento4.Caption     := Titulo.NumeroDocumento;
    txtEspecieDoc.Caption           := Titulo.EspecieDoc;
    txtAceite4.Caption              := ifThen(Titulo.Aceite = atSim,'S','N');
    txtDataProcessamento4.Caption   := FormatDateTime('dd/mm/yyyy',Now);
    txtNossoNumero4.Caption         := NossoNum;
    txtUsoBanco4.Caption            := Titulo.UsoBanco;
    txtCarteira4.Caption            := Titulo.Carteira;
    txtEspecieMod.Caption           := IfThen(Titulo.EspecieMod = '', 'R$', Titulo.EspecieMod);
    
    txtValorDocumento4.Caption := ifthen(Titulo.ValorDocumento > 0, FormatFloat('###,###,##0.00',Titulo.ValorDocumento), '');
    
    txtNomeSacado4.Caption          := Titulo.Sacado.NomeSacado;
    txtEnderecoSacado4.Caption      := Titulo.Sacado.Logradouro + ' '+
                                       Titulo.Sacado.Numero + ' ' + Titulo.Sacado.Complemento;
    txtCidadeSacado4.Caption        := Titulo.Sacado.CEP + ' '+Titulo.Sacado.Cidade +
                                         ' '+Titulo.Sacado.UF;
    txtCpfCnpjSacado4.Caption       := Titulo.Sacado.CNPJCPF;
    txtInstrucoes4.Lines.Text       := Titulo.Mensagem.Text;
    txtLinhaDigitavel4.Caption      := LinhaDigitavel;

    txtDataVencimento5.Caption      := txtDataVencimento4.Caption;
    txtCodigoCedente5.Caption       := txtCodigoCedente4.Caption;
    txtNumeroDocumento5.Caption     := txtNumeroDocumento4.Caption;
    txtNossoNumero5.Caption         := txtNossoNumero4.Caption;
    txtEspecie5.Caption             := txtEspecieMod.Caption;
    txtValorDocumento5.Caption      := txtValorDocumento4.Caption;
    txtNomeSacado5.Caption          := txtNomeSacado4.Caption;
    txtParcela.Caption              := viPar + ' / ' + viPla;

    ACBrBarCode1.Typ  := bcCode_2_5_interleaved ;
    ACBrBarCode1.Text := CodBarras;
    BMP := TBitmap.Create ;
    try
      BMP.Width  := imgBarra.Width ;
      BMP.Height := imgBarra.Height ;
      ACBrBarCode1.DrawBarcode(BMP.Canvas);

      imgBarra4.Picture.Bitmap := BMP;
    finally
      BMP.Free ;
   end;
end;
  Inc(fIndice);
end;

procedure TACBrBoletoFCQuickFr.LayoutBoletoBeforePrint(
  Sender: TCustomQuickRep; var PrintReport: Boolean);
begin
   fIndice:= 0;
end;

procedure TACBrBoletoFCQuickFr.BoletoCarneBeforePrint(
  Sender: TCustomQuickRep; var PrintReport: Boolean);
begin
   fIndice:= 0;
end;

procedure TACBrBoletoFCQuickFr.LayOutPadraoEntregaBeforePrint(
  Sender: TCustomQuickRep; var PrintReport: Boolean);
begin
   fIndice:= 0;
end;

procedure TACBrBoletoFCQuickFr.LayOutPadraoEntregaNeedData(Sender: TObject;
  var MoreData: Boolean);
begin
   MoreData := (fIndice < fBoletoFC.ACBrBoleto.ListadeBoletos.Count) ;
end;

procedure TACBrBoletoFCQuickFr.LayOutPadraoEntregaStartPage(
  Sender: TCustomQuickRep);
begin
  Inc( fIndice ) ;
end;

procedure TACBrBoletoFCQuickFr.RLMemo6_1BeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
Var
   NossoNum, CodCedente : String;
   CodBarras, LinhaDigitavel : String;
   BMP : TBitmap ;
begin
   with fBoletoFC.ACBrBoleto do
   begin
      NossoNum    := Banco.MontarCampoNossoNumero( Titulo );
      CodCedente  := Banco.MontarCampoCodigoCedente(titulo);

      fBoletoFC.CarregaLogo( imgBanco6_0.Picture, Banco.Numero );
      txtNumeroBanco6_0.Caption         := IntToStrZero(Banco.Numero, 3)+ '-' +
                                           IfThen(Banco.Digito >= 10,'X',IntToStrZero(Banco.Digito, 1));
      lblLocalPagto6_0.Caption          := Titulo.LocalPagamento;
    
    
      txtDataVencimento6_0.Caption := ifthen(Titulo.Vencimento = 0, '', FormatDateTime('dd/mm/yyyy', Titulo.Vencimento));
    
      txtNomeCedente6_0.Caption         := Cedente.Nome;
      txtCodigoCedente6_0.Caption       := CodCedente;
      txtDataDocumento6_0.Caption       := FormatDateTime('dd/mm/yyyy', Titulo.DataDocumento);
      txtNumeroDocumento6_0.Caption     := Titulo.NumeroDocumento;
      txtEspecieDoc6_0.Caption          := Titulo.EspecieDoc;
      txtAceite6_0.Caption              := ifThen(Titulo.Aceite = atSim,'S','N');
      txtDataProcessamento6_0.Caption   := FormatDateTime('dd/mm/yyyy',Now);
      txtNossoNumero6_0.Caption         := NossoNum;
    
      txtValorDocumento6_0.Caption:= ifthen(Titulo.ValorDocumento > 0, FormatFloat('###,###,##0.00',Titulo.ValorDocumento), '');
      
      txtNomeSacado6_0.Caption          := Titulo.Sacado.NomeSacado;
      txtEnderecoSacado6_0.Caption      := Titulo.Sacado.Logradouro + ' '+
                                           Titulo.Sacado.Numero + Titulo.Sacado.Complemento;
      txtCidadeSacado6_0.Caption        := Titulo.Sacado.CEP + ' '+Titulo.Sacado.Bairro + ' '+Titulo.Sacado.Cidade +
                                           ' '+Titulo.Sacado.UF;
      txtCpfCnpjSacado6_0.Caption       := Titulo.Sacado.CNPJCPF;


      imgBanco6_1.Picture.Assign(imgBanco6_0.Picture);
      txtNumeroBanco6_1.Caption         := txtNumeroBanco6_0.Caption;
      lblLocalPagto6_1.Caption          := lblLocalPagto6_0.Caption;
      txtDataVencimento6_1.Caption      := txtDataVencimento6_0.Caption;
      txtNomeCedente6_1.Caption         := txtNomeCedente6_0.Caption;
      txtCodigoCedente6_1.Caption       := txtCodigoCedente6_0.caption;
      txtDataDocumento6_1.Caption       := txtDataDocumento6_0.Caption;
      txtNumeroDocumento6_1.Caption     := Titulo.NumeroDocumento;
      txtEspecieDoc6_1.Caption          := Titulo.EspecieDoc;
      txtAceite6_1.Caption              := txtAceite6_0.Caption;
      txtDataProcessamento6_1.Caption   := txtDataProcessamento6_0.Caption;
      txtNossoNumero6_1.Caption         := txtNossoNumero6_0.Caption;
      txtUsoBanco6_1.Caption            := Titulo.UsoBanco;
      txtCarteira6_1.Caption            := Titulo.Carteira;
      txtEspecie6_1.Caption             := IfThen(Titulo.EspecieMod = '', 'R$', Titulo.EspecieMod);
      txtValorDocumento6_1.Caption      := txtValorDocumento6_0.Caption;
      txtNomeSacado6_1.Caption          := txtNomeSacado6_0.Caption;
      txtEnderecoSacado6_1.Caption      := txtEnderecoSacado6_0.Caption;
      txtCidadeSacado6_1.Caption        := txtCidadeSacado6_0.Caption;
      txtCpfCnpjSacado6_1.Caption       := txtCpfCnpjSacado6_0.Caption;
      txtInstrucoes6_1.Lines.Text       := Titulo.Mensagem.Text;


     CodBarras      := Banco.MontarCodigoBarras( Titulo );
     LinhaDigitavel := Banco.MontarLinhaDigitavel( CodBarras, Titulo );

     imgBanco6_2.Picture.Assign(imgBanco6_1.Picture);
     txtNumeroBanco6_2.Caption         := txtNumeroBanco6_1.Caption;
     txtLocalPagamento6_2.Caption      := lblLocalPagto.Caption;
     txtDataVencimento6_2.Caption      := txtDataVencimento6_1.Caption;
     txtNomeCedente6_2.Caption         := txtNomeCedente6_1.Caption;
     txtCodigoCedente6_2.Caption       := txtCodigoCedente6_1.Caption;
     txtDataDocumento6_2.Caption       := txtDataDocumento6_1.Caption;
     txtNumeroDocumento6_2.Caption     := txtNumeroDocumento6_1.Caption;
     txtEspecie6_2.Caption             := txtEspecie6_1.Caption;
     txtAceite6_2.Caption              := txtAceite6_1.Caption;
     txtDataProcessamento6_2.Caption   := txtDataProcessamento6_1.Caption;
     txtNossoNumero6_2.Caption         := txtNossoNumero6_1.Caption;
     txtUsoBanco6_2.Caption            := txtUsoBanco6_1.Caption;
     txtCarteira6_2.Caption            := txtCarteira6_1.Caption;
     txtEspecieDoc6_2.Caption          := txtEspecieDoc6_1.Caption;
     txtValorDocumento6_2.Caption      := txtValorDocumento6_1.Caption;
     txtNomeSacado6_2.Caption          := txtNomeSacado6_1.Caption;
     txtEnderecoSacado6_2.Caption      := txtEnderecoSacado6_1.Caption;
     txtCidadeSacado6_2.Caption        := txtCidadeSacado6_1.Caption;
     txtCpfCnpjSacado6_2.Caption       := txtCpfCnpjSacado6_1.Caption;
     txtLinhaDigitavel6_2.Caption      := LinhaDigitavel;
     txtInstrucoes6_2.Lines.Text       := txtInstrucoes6_1.Lines.Text;

     ACBrBarCode1.Typ  := bcCode_2_5_interleaved ;
     ACBrBarCode1.Text := CodBarras;
     BMP := TBitmap.Create ;
     try
        BMP.Width  := imgBarra.Width ;
        BMP.Height := imgBarra.Height ;
        ACBrBarCode1.DrawBarcode(BMP.Canvas);
        imgBarra6_2.Picture.Bitmap := BMP ;
     finally
        BMP.Free ;
     end ;
   end;
end;

end.



