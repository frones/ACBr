{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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

unit ACBrGNReGuiaRLRetrato;

interface

uses
  SysUtils, 
  Variants, 
  Classes, 
  Graphics, 
  Controls, 
  Forms,
  ExtCtrls, 
  RLReport, 
  RLBarcode, 
  RLPDFFilter,
  ACBrGNReGuiaRL, 
  RLFilters, 
  pgnreGNRERetorno, 
  maskutils, StdCtrls;

type

  { TfrlGuiaRLRetrato }

  TfrlGuiaRLRetrato = class(TfrlGuiaRL)
    subItens: TRLSubDetail;
    RLBand1: TRLBand;
    RLDraw1: TRLDraw;
    RLDraw14: TRLDraw;
    RLDraw2: TRLDraw;
    RLDraw17: TRLDraw;
    RLDraw3: TRLDraw;
    RLDraw4: TRLDraw;
    RLDraw5: TRLDraw;
    RLDraw6: TRLDraw;
    RLDraw7: TRLDraw;
    RLDraw8: TRLDraw;
    RLDraw9: TRLDraw;
    RLDraw10: TRLDraw;
    RLDraw11: TRLDraw;
    RLDraw12: TRLDraw;
    RLDraw13: TRLDraw;
    RLDraw15: TRLDraw;
    RLDraw16: TRLDraw;
    RLMemo2: TRLMemo;
    RLLabel1: TRLLabel;
    RLLabel2: TRLLabel;
    RLLabel3: TRLLabel;
    RLLabel4: TRLLabel;
    RLLabel5: TRLLabel;
    RLLabel6: TRLLabel;
    RLLabel7: TRLLabel;
    RLLabel8: TRLLabel;
    RLLabel9: TRLLabel;
    RLLabel10: TRLLabel;
    RLLabel11: TRLLabel;
    RLLabel12: TRLLabel;
    RLLabel13: TRLLabel;
    RLLabel14: TRLLabel;
    RLLabel15: TRLLabel;
    RLLabel16: TRLLabel;
    RLLabel17: TRLLabel;
    RLLabel18: TRLLabel;
    RLLabel19: TRLLabel;
    RLLabel20: TRLLabel;
    RLLabel21: TRLLabel;
    RLLabel22: TRLLabel;
    RLLabel23: TRLLabel;
    RLLabel24: TRLLabel;
    RLLabel25: TRLLabel;
    RLLabel26: TRLLabel;
    RLLabel27: TRLLabel;
    RLLabel28: TRLLabel;
    RLLabel30: TRLLabel;
    RLLabel32: TRLLabel;
    RLLabel33: TRLLabel;
    RLLabel34: TRLLabel;
    RLLabel35: TRLLabel;
    RLLabel36: TRLLabel;
    RLLabel37: TRLLabel;
    RLMemo1: TRLMemo;
    RLLabel29: TRLLabel;
    RLLabel31: TRLLabel;
    RLDraw18: TRLDraw;
    RLDraw19: TRLDraw;
    RLDraw20: TRLDraw;
    RLDraw21: TRLDraw;
    RLDraw22: TRLDraw;
    RLDraw23: TRLDraw;
    RLDraw24: TRLDraw;
    RLDraw25: TRLDraw;
    RLDraw26: TRLDraw;
    RLDraw27: TRLDraw;
    RLDraw28: TRLDraw;
    RLDraw29: TRLDraw;
    RLDraw30: TRLDraw;
    RLDraw31: TRLDraw;
    RLDraw32: TRLDraw;
    RLDraw33: TRLDraw;
    RLDraw34: TRLDraw;
    RLMemo3: TRLMemo;
    RLLabel38: TRLLabel;
    RLLabel39: TRLLabel;
    RLLabel40: TRLLabel;
    RLLabel41: TRLLabel;
    RLLabel42: TRLLabel;
    RLLabel43: TRLLabel;
    RLLabel44: TRLLabel;
    RLLabel45: TRLLabel;
    RLLabel46: TRLLabel;
    RLLabel47: TRLLabel;
    RLLabel48: TRLLabel;
    RLLabel49: TRLLabel;
    RLLabel50: TRLLabel;
    RLLabel51: TRLLabel;
    RLLabel52: TRLLabel;
    RLLabel53: TRLLabel;
    RLLabel54: TRLLabel;
    RLLabel55: TRLLabel;
    RLLabel56: TRLLabel;
    RLLabel57: TRLLabel;
    RLLabel58: TRLLabel;
    RLLabel59: TRLLabel;
    RLLabel60: TRLLabel;
    RLLabel61: TRLLabel;
    RLLabel62: TRLLabel;
    RLLabel63: TRLLabel;
    RLLabel64: TRLLabel;
    RLLabel65: TRLLabel;
    RLLabel66: TRLLabel;
    RLLabel67: TRLLabel;
    RLLabel68: TRLLabel;
    RLLabel69: TRLLabel;
    RLLabel70: TRLLabel;
    RLLabel71: TRLLabel;
    RLLabel72: TRLLabel;
    RLMemo4: TRLMemo;
    RLLabel73: TRLLabel;
    RLLabel74: TRLLabel;
    RazaoSocialEmitente: TRLLabel;
    DocEmitente: TRLLabel;
    EnderecoEmitente: TRLLabel;
    UFFavorecida: TRLLabel;
    CodReceita: TRLLabel;
    MunicipioEmitente: TRLLabel;
    UFEmitente: TRLLabel;
    CEPEmitente: TRLLabel;
    TelefoneEmitente: TRLLabel;
    NumeroControle: TRLLabel;
    DataVencimento: TRLLabel;
    NumDocOrigem: TRLLabel;
    MunicipioDestinatario: TRLLabel;
    Convenio: TRLLabel;
    Produto: TRLLabel;
    DataLimitePagamento: TRLLabel;
    RepresentacaoNumerica: TRLLabel;
    RazaoSocialEmitente2: TRLLabel;
    DocEmitente2: TRLLabel;
    EnderecoEmitente2: TRLLabel;
    UFFavorecida2: TRLLabel;
    CodReceita2: TRLLabel;
    MunicipioEmitente2: TRLLabel;
    UFEmitente2: TRLLabel;
    CEPEmitente2: TRLLabel;
    TelefoneEmitente2: TRLLabel;
    NumeroControle2: TRLLabel;
    DataVencimento2: TRLLabel;
    NumDocOrigem2: TRLLabel;
    MunicipioDestinatario2: TRLLabel;
    Convenio2: TRLLabel;
    Produto2: TRLLabel;
    DataLimitePagamento2: TRLLabel;
    RepresentacaoNumerica2: TRLLabel;
    PerMesAnoRef2: TRLLabel;
    PerMesAnoRef: TRLLabel;
    CodigoBarras: TRLBarcode;
    CodigoBarras2: TRLBarcode;
    InfoComplementares: TRLMemo;
    InfoComplementares2: TRLMemo;
    RLDraw35: TRLDraw;
    RLLabel75: TRLLabel;
    RLDraw39: TRLDraw;
    RLDraw40: TRLDraw;
    RLLabel80: TRLLabel;
    UFFavorecida3: TRLLabel;
    CodReceita3: TRLLabel;
    RLLabel81: TRLLabel;
    RLLabel86: TRLLabel;
    NumeroControle3: TRLLabel;
    RLLabel87: TRLLabel;
    DataVencimento3: TRLLabel;
    RLLabel88: TRLLabel;
    NumDocOrigem3: TRLLabel;
    RLLabel98: TRLLabel;
    PerMesAnoRef3: TRLLabel;
    RLLabel99: TRLLabel;
    RLLabel100: TRLLabel;
    RLDraw38: TRLDraw;
    RLDraw44: TRLDraw;
    RLLabel101: TRLLabel;
    RLLabel102: TRLLabel;
    RLDraw45: TRLDraw;
    RLLabel110: TRLLabel;
    RLLabel103: TRLLabel;
    RLDraw46: TRLDraw;
    RLLabel111: TRLLabel;
    RLLabel104: TRLLabel;
    RLDraw47: TRLDraw;
    RLLabel105: TRLLabel;
    RLLabel106: TRLLabel;
    RLDraw48: TRLDraw;
    RLLabel107: TRLLabel;
    RLLabel108: TRLLabel;
    RLDraw49: TRLDraw;
    RLLabel109: TRLLabel;
    RLMemo5: TRLMemo;
    RLMemo6: TRLMemo;
    RLLabel76: TRLLabel;
    RLLabel77: TRLLabel;
    RLLabel79: TRLLabel;
    EnderecoEmitente3: TRLLabel;
    RLLabel78: TRLLabel;
    DocEmitente3: TRLLabel;
    RazaoSocialEmitente3: TRLLabel;
    RLLabel83: TRLLabel;
    UFEmitente3: TRLLabel;
    TelefoneEmitente3: TRLLabel;
    RLLabel85: TRLLabel;
    MunicipioEmitente3: TRLLabel;
    RLLabel82: TRLLabel;
    RLLabel84: TRLLabel;
    CEPEmitente3: TRLLabel;
    RLDraw37: TRLDraw;
    RLLabel89: TRLLabel;
    RLLabel90: TRLLabel;
    RLLabel92: TRLLabel;
    MunicipioDestinatario3: TRLLabel;
    RLLabel93: TRLLabel;
    RLLabel94: TRLLabel;
    Convenio3: TRLLabel;
    RLLabel95: TRLLabel;
    Produto3: TRLLabel;
    RLDraw50: TRLDraw;
    RLLabel91: TRLLabel;
    RLDraw36: TRLDraw;
    RLLabel96: TRLLabel;
    InfoComplementares3: TRLMemo;
    RLLabel97: TRLLabel;
    DataLimitePagamento3: TRLLabel;
    RLDraw51: TRLDraw;
    RepresentacaoNumerica3: TRLLabel;
    CodigoBarras3: TRLBarcode;
    RLDraw41: TRLDraw;
    RLDraw42: TRLDraw;
    RLDraw43: TRLDraw;
    RLDraw52: TRLDraw;
    RLDraw53: TRLDraw;
    RLDraw55: TRLDraw;
    imgQrCodePIX: TRLImage;
    imgQrCodePIX2: TRLImage;
    imgQrCodePIX3: TRLImage;
    pnlMsgPIX: TRLPanel;
    RLMemo7: TRLMemo;
    pnlMsgPIX2: TRLPanel;
    RLMemo8: TRLMemo;
    pnlMsgPIX3: TRLPanel;
    RLMemo9: TRLMemo;
      procedure RLBand1BeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure RLGNReDataRecord(Sender: TObject; RecNo, CopyNo: Integer;
      var Eof: Boolean; var RecordAction: TRLRecordAction);
    procedure subItensDataRecord(Sender: TObject; RecNo, CopyNo: Integer;
      var Eof: Boolean; var RecordAction: TRLRecordAction);
  private
    { Private declarations }
    FNumItem : integer;
    procedure printQrCodePayload(const AQrCodePayLoad: String; AInfoQrCode: TRLPanel; out AImagemQrCode: TRLImage);
  public
    { Public declarations }
  end;

implementation

uses
  StrUtils, DateUtils, ACBrImage, ACBrDelphiZXingQRCode,
  ACBrUtil.Base, ACBrUtil.Strings,
  ACBrDFeUtil;

{$ifdef FPC}
 {$R *.lfm}
{$else}
 {$R *.dfm}
{$endif}

procedure TfrlGuiaRLRetrato.printQrCodePayload(const AQrCodePayLoad: String; AInfoQrCode: TRLPanel; out AImagemQrCode: TRLImage);
begin
  if NaoEstaVazio(AQrCodePayLoad) then
  begin
    AImagemQrCode.Visible := True;
    PintarQrCode(AQrCodePayload, AImagemQrCode.Picture.Bitmap, qrAuto);
    AImagemQrCode.BringToFront;

    AInfoQrCode.Visible := False;
    AInfoQrCode.SendToBack;
  end else
  begin
    AImagemQrCode.Visible := False;
    AImagemQrCode.SendToBack;

    AInfoQrCode.Visible := True;
    AInfoQrCode.BringToFront;
  end;
end;

procedure TfrlGuiaRLRetrato.RLBand1BeforePrint(Sender: TObject; var PrintIt: Boolean);

  Function FormataPeriodoReferencia( sTexto : String) : string;
  begin
      Case  StrToIntDef(sTexto,0) of
        0: Result := 'Mensal';
        1: Result := '1a Quinzena';
        2: Result := '2a Quinzena';
        3: Result := '1o Decêndio';
        4: Result := '2o Decêndio';
        5: Result := '3o Decêndio';
      end;
    Result := ACBrStr( Result );
  end;

  Function  FormataMesAnoReferencia( sTexto : String ) : String;
  begin
    Result := sTexto;
    if sTexto <> '' then
      Result := '-' +
                Copy( sTexto , 1,Length( sTexto)-4) +
                '/' +
                Copy( sTexto , Length( sTexto)-3,Length(sTexto));
  end;

  function FormatarData(Str: string): string;
  begin
    if Pos('/', Str) = 0 then
      Result := Copy(Str, 1, 2) + '/' + Copy(Str, 3, 2) + '/' + Copy(Str, 5, 4)
    else
      Result := Str;
  end;

  function RemoverZeros(Str: string): string;
  begin
    while (Trim(Str) <> '') and (Str[1] = '0') do
      Str := Copy(Str, 2, Length(Str));

    Result := Trim(Str);
  end;

  function FormaDoc( iTipoDocEmitente : Integer; sDocEmitente : String ): String;
  begin
    if iTipoDocEmitente = 0 then
    begin
      case Length(sDocEmitente) of
        11: iTipoDocEmitente := 1;
        14: iTipoDocEmitente := 2;
      else
        iTipoDocEmitente := 3;
      end;
    end;
    case iTipoDocEmitente of
      1: result := FormatMaskText('000\.000\.000\-00;0', sDocEmitente);
      2: result := FormatMaskText('00\.000\.000\/0000\-00;0', sDocEmitente);
      3: result := RemoverZeros(sDocEmitente);
    end;
  end;
begin
  // 1ª Via
  RLLabel18.Caption := FGNRe.DocDestinatario;
  RLLabel26.Caption := IntToStrZero(FGNRe.Parcela,3);
  RLLabel28.Caption := FormatFloat('R$ ,0.00', FGNRe.ValorPrincipal);
  RLLabel30.Caption := IfThen(FGNRe.AtualizacaoMonetaria = 0 , '' ,
                              FormatFloat('R$ ,0.00', FGNRe.AtualizacaoMonetaria));
  RLLabel32.Caption := IfThen(FGNRe.Juros = 0 , '' ,
                              FormatFloat('R$ ,0.00', FGNRe.Juros));
  RLLabel34.Caption := IfThen(FGNRe.Multa = 0 , '' ,
                              FormatFloat('R$ ,0.00', FGNRe.Multa));

  RLLabel36.Caption := FormatFloat('R$ ,0.00', FGNRe.valorGNRE);
{
  RLLabel36.Caption := FormatFloat('R$ ,0.00', (FGNRe.ValorPrincICMS +
                                                FGNRe.ValorFECP +
                                                FGNRe.AtualizacaoMonetaria +
                                                FGNRe.Juros +
                                                FGNRe.Multa));
}
  RLMemo2.Visible := FGNRe.InfoCabec.Ambiente = 9;

  // 2ª Via
  RLLabel55.Caption := RLLabel18.Caption;
  RLLabel63.Caption := RLLabel26.Caption;
  RLLabel65.Caption := RLLabel28.Caption;
  RLLabel66.Caption := RLLabel30.Caption;
  RLLabel67.Caption := RLLabel32.Caption;
  RLLabel69.Caption := RLLabel34.Caption;
  RLLabel71.Caption := RLLabel36.Caption;
  RLMemo3.Visible := RLMemo2.Visible;

  // 3ª Via
  RLLabel92.Caption := RLLabel18.Caption;
  RLLabel100.Caption := RLLabel26.Caption;
  RLLabel102.Caption := RLLabel28.Caption;
  RLLabel103.Caption := RLLabel30.Caption;
  RLLabel104.Caption := RLLabel32.Caption;
  RLLabel106.Caption := RLLabel34.Caption;
  RLLabel108.Caption := RLLabel36.Caption;
  RLMemo5.Visible := RLMemo2.Visible;

  UFFavorecida.Caption := FGNRe.UFFavorecida;
  UFFavorecida2.Caption := UFFavorecida.Caption;
  UFFavorecida3.Caption := UFFavorecida.Caption;

  CodReceita.Caption := IntToStr( FGNRe.codReceita );
  CodReceita2.Caption := CodReceita.Caption;
  CodReceita3.Caption := CodReceita.Caption;

  RazaoSocialEmitente.Caption := FGNRe.RazaoSocialEmitente;
  RazaoSocialEmitente2.Caption := RazaoSocialEmitente.Caption;
  RazaoSocialEmitente3.Caption := RazaoSocialEmitente.Caption;

  DocEmitente.Caption := FormaDoc( FGNRe.TipoDocEmitente , FGNRe.DocEmitente );
  DocEmitente2.Caption := DocEmitente.Caption;
  DocEmitente3.Caption := DocEmitente.Caption;

  EnderecoEmitente.Caption := FGNRe.EnderecoEmitente;
  EnderecoEmitente2.Caption := EnderecoEmitente.Caption;
  EnderecoEmitente3.Caption := EnderecoEmitente.Caption;

  CEPEmitente.Caption := FGNRe.CEPEmitente;
  CEPEmitente2.Caption := CEPEmitente.Caption;
  CEPEmitente3.Caption := CEPEmitente.Caption;

  MunicipioEmitente.Caption := FGNRe.MunicipioEmitenteNome;
  MunicipioEmitente2.Caption := MunicipioEmitente.Caption;
  MunicipioEmitente3.Caption := MunicipioEmitente.Caption;

  UFEmitente.Caption := FGNRe.UFEmitente;
  UFEmitente2.Caption := UFEmitente.Caption;
  UFEmitente3.Caption := UFEmitente.Caption;

  TelefoneEmitente.Caption := FGNRe.TelefoneEmitente;
  TelefoneEmitente2.Caption := FGNRe.TelefoneEmitente;
  TelefoneEmitente3.Caption := FGNRe.TelefoneEmitente;

  NumeroControle.Caption := FGNRe.NumeroControle;
  NumeroControle2.Caption := FGNRe.NumeroControle;
  NumeroControle3.Caption := FGNRe.NumeroControle;

  NumDocOrigem.Caption := FGNRe.NumDocOrigem;
  NumDocOrigem2.Caption := NumDocOrigem.Caption;
  NumDocOrigem3.Caption := NumDocOrigem.Caption;

  MunicipioDestinatario.Caption := FGNRe.MunicipioDestinatarioNome;
  MunicipioDestinatario2.Caption := MunicipioDestinatario.Caption;
  MunicipioDestinatario3.Caption := MunicipioDestinatario.Caption;

  Convenio.Caption := FGNRe.Convenio;
  Convenio2.Caption := Convenio.Caption;
  Convenio3.Caption := Convenio.Caption;

  Produto.Caption := FGNRe.Produto;
  Produto2.Caption := Produto.Caption;
  Produto3.Caption := Produto.Caption;

  PerMesAnoRef.Caption := FormataPeriodoReferencia( FGNRe.PeriodoReferencia )+
                                    FormataMesAnoReferencia( FGNRe.MesAnoReferencia );
  PerMesAnoRef2.Caption := PerMesAnoRef.Caption;
  PerMesAnoRef3.Caption := PerMesAnoRef.Caption;

  DataVencimento.Caption := FormatarData(FGNRe.DataVencimento);
  DataVencimento2.Caption := DataVencimento.Caption;
  DataVencimento3.Caption := DataVencimento.Caption;

  DataLimitePagamento.Caption := FormatarData(IfThen(FGNRe.DataLimitePagamento = '00000000' ,
                                                     FGNRe.DataVencimento ,
                                                     FGNRe.DataLimitePagamento));

  DataLimitePagamento2.Caption := DataLimitePagamento.Caption;
  DataLimitePagamento3.Caption := DataLimitePagamento.Caption;

  RepresentacaoNumerica.Caption := FormatMaskText('00000000000 0 00000000000 0 00000000000 0 00000000000 0;0',
                                   FGNRe.RepresentacaoNumerica);
  RepresentacaoNumerica2.Caption := RepresentacaoNumerica.Caption;
  RepresentacaoNumerica3.Caption := RepresentacaoNumerica.Caption;

  CodigoBarras.Caption := OnlyNumber(FGNRe.CodigoBarras);
  CodigoBarras2.Caption := CodigoBarras.Caption;
  CodigoBarras3.Caption := CodigoBarras.Caption;

  printQrCodePayload(FGNRe.qrcodePayload, pnlMsgPIX, imgQrCodePIX);
  printQrCodePayload(FGNRe.qrcodePayload, pnlMsgPIX2, imgQrCodePIX2);
  printQrCodePayload(FGNRe.qrcodePayload, pnlMsgPIX3, imgQrCodePIX3);

  InfoComplementares.Lines.Text := FGNRe.InfoComplementares;
  InfoComplementares2.Lines := InfoComplementares.Lines;
  InfoComplementares3.Lines := InfoComplementares.Lines;
end;

procedure TfrlGuiaRLRetrato.RLGNReDataRecord(Sender: TObject; RecNo,
  CopyNo: Integer; var Eof: Boolean; var RecordAction: TRLRecordAction);
begin
  inherited;

  Eof := (RecNo > 1);
  RecordAction := raUseIt;
end;

procedure TfrlGuiaRLRetrato.subItensDataRecord(Sender: TObject; RecNo,
  CopyNo: Integer; var Eof: Boolean; var RecordAction: TRLRecordAction);
begin
  inherited;

  FNumItem := RecNo - 1 ;
  Eof := (RecNo > 1) ;
  RecordAction := raUseIt;
end;

end.


