{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2004 Daniel Simoes de Almeida               }
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
{ http://www.opensource.org/licenses/gpl-license.php                           }
{                                                                              }
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}


{******************************************************************************
|* Historico
|*
|* 04/04/2013:  AndrÃ© Ferreira de Moraes
|*   Inicio do desenvolvimento
******************************************************************************}
{$I ACBr.inc}
unit ACBrDANFCeFortesFr;

interface

uses Classes, SysUtils,
     {$IFDEF FPC}
       LResources,
     {$ENDIF}
     Forms, Graphics,
     ACBrNFeDANFEClass, ACBrUtil,
     pcnNFe, pcnConversao, pcnAuxiliar, ACBrDFeUtil,
     RLConsts, RLReport, RLBarcode, RLPDFFilter, RLHTMLFilter, RLPrintDialog,
     RLFilters, RLPrinters, Controls;

const
  CACBrNFeDANFCeFortes_Versao = '0.1.0' ;

type
  TACBrSATExtratoFiltro = (fiNenhum, fiPDF, fiHTML ) ;

  { TACBrNFeDANFCeFortes }

  TACBrNFeDANFCeFortes = class( TACBrNFeDANFEClass )
  private
  protected
    FpNFe: TNFe;

    procedure Imprimir(const DanfeResumido : Boolean = False; const AFiltro : TACBrSATExtratoFiltro = fiNenhum);
    procedure ImprimirCancelado(const DanfeResumido : Boolean = False; const AFiltro : TACBrSATExtratoFiltro = fiNenhum);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ImprimirDANFE(NFE : TNFe = nil); override ;
    procedure ImprimirDANFEResumido(NFE : TNFe = nil); override ;
    procedure ImprimirDANFEPDF(NFE : TNFe = nil); override;
    procedure ImprimirDANFEResumidoPDF(NFE : TNFe = nil);override;
    procedure ImprimirDANFECancelado(NFE : TNFe = nil);
  published
  end ;

  { TACBrNFeDANFCeFortesFr }

  TACBrNFeDANFCeFortesFr = class(TForm)
    lConsumidor: TRLMemo;
    lNomeFantasia: TRLMemo;
    lNomeFantasiaCanc: TRLMemo;
    lProtocolo: TRLLabel;
    lRazaoSocial: TRLMemo;
    lRazaoSocialCanc: TRLMemo;
    lTitLei12741: TRLLabel;
    lMensagemFiscal: TRLMemo;
    rlVenda: TRLReport;
    rlbRodape: TRLBand;
    imgQRCode: TRLImage;
    pGap05: TRLPanel;
    lSistema: TRLLabel;
    pGap8: TRLPanel;
    rlsbDetItem: TRLSubDetail;
    rlbDetItem: TRLBand;
    mLinhaItem: TRLMemo;
    rlbDescItem: TRLBand;
    lTitDesconto: TRLLabel;
    lTitDescValLiq: TRLLabel;
    lDesconto: TRLLabel;
    lDescValLiq: TRLLabel;
    rlbOutroItem: TRLBand;
    lTitAcrescimo: TRLLabel;
    lTitOutroValLiq: TRLLabel;
    lOutro: TRLLabel;
    lOutroValLiq: TRLLabel;
    rlbGap: TRLBand;
    rlsbPagamentos: TRLSubDetail;
    rlbPagamento: TRLBand;
    lPagamento: TRLLabel;
    lMeioPagamento: TRLLabel;
    rlbTroco: TRLBand;
    lTitTroco: TRLLabel;
    lTroco: TRLLabel;
    rlbTotal: TRLBand;
    lTitTotal: TRLLabel;
    lTotal: TRLLabel;
    lQtdItens: TRLLabel;
    lQtdTotalItensVal: TRLLabel;
    lTitFormaPagto: TRLLabel;
    lTitValorPago: TRLLabel;
    rlbsCabecalho: TRLSubDetail;
    rlbMsgDANFe: TRLBand;
    lMsgDANFCe: TRLLabel;
    lMsgDANFCe2: TRLLabel;
    rlbDadosCliche: TRLBand;
    pLogoeCliche: TRLPanel;
    lEndereco: TRLMemo;
    imgLogo: TRLImage;
    rlbLegenda: TRLBand;
    lCPF_CNPJ1: TRLLabel;
    rlbConsumidor: TRLBand;
    lEnderecoConsumidor: TRLMemo;
    rlbMensagemFiscal: TRLBand;
    lChaveDeAcesso: TRLLabel;
    lNumeroSerie: TRLLabel;
    lTitConsulteChave: TRLMemo;
    lObservacoes: TRLMemo;
    RLHTMLFilter1: TRLHTMLFilter;
    RLPDFFilter1: TRLPDFFilter;
    lEmissaoVia: TRLLabel;
    lCancelada: TRLLabel;
    rlCancelamento: TRLReport;
    rlbRodapeCanc: TRLBand;
    RLDraw9: TRLDraw;
    lConsultaQRCodeCanc: TRLLabel;
    imgQRCodeCanc: TRLImage;
    RLPanel1: TRLPanel;
    lSistemaCanc: TRLLabel;
    lProtocoloCanc: TRLLabel;
    RLPanel2: TRLPanel;
    RLSubDetail3: TRLSubDetail;
    RLBand10: TRLBand;
    RLLabel26: TRLLabel;
    RLLabel27: TRLLabel;
    RLLabel28: TRLLabel;
    RLBand11: TRLBand;
    RLPanel3: TRLPanel;
    lEmitCNPJ_IE_IM_Camc: TRLLabel;
    lEnderecoCanc: TRLMemo;
    RLDraw14: TRLDraw;
    RLImage2: TRLImage;
    rlbConsumidorCanc: TRLBand;
    RLDraw17: TRLDraw;
    lTitConsumidorCanc: TRLLabel;
    lEnderecoConsumidorCanc: TRLMemo;
    lCPF_CNPJ_ID_Canc: TRLMemo;
    rlbMensagemFiscalCanc: TRLBand;
    RLDraw18: TRLDraw;
    lMensagemFiscalCanc: TRLLabel;
    lChaveDeAcessoCanc: TRLLabel;
    lTitChaveAcessoCanc: TRLLabel;
    lNumeroSerieCanc: TRLLabel;
    lTitConsulteChaveCanc: TRLMemo;
    lEmissaoViaCanc: TRLLabel;
    RLDraw19: TRLDraw;
    lCanceladaCanc: TRLLabel;
    rlbMensagemContribuinteCanc: TRLBand;
    lMensagemContribuinteCamc: TRLLabel;
    RLDraw20: TRLDraw;
    lObservacoesCanc: TRLMemo;
    RLBand12: TRLBand;
    RLDraw15: TRLDraw;

    procedure FormDestroy(Sender: TObject);
    procedure pAsteriscoBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlbConsumidorBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlbMensagemFiscalBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlbRodapeBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlbsCabecalhoDataRecord(Sender: TObject; RecNo: integer;
      CopyNo: integer; var Eof: boolean; var RecordAction: TRLRecordAction);
    procedure rlbLegendaBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlbPagamentoBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlbGapBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlbTotalBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlbTrocoBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlVendaBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure FormCreate(Sender: TObject);
    procedure rlVendaDataRecord(Sender: TObject; RecNo: integer;
      CopyNo: integer; var Eof: boolean; var RecordAction: TRLRecordAction);
    procedure rlsbDetItemDataRecord(Sender: TObject; RecNo: integer;
      CopyNo: integer; var Eof: boolean; var RecordAction: TRLRecordAction);
    procedure rlsbPagamentosDataRecord(Sender: TObject; RecNo: integer;
      CopyNo: integer; var Eof: boolean; var RecordAction: TRLRecordAction);
    procedure lSistemaBeforePrint(Sender: TObject; var Text: string;
      var PrintIt: Boolean);
    procedure rlbDetItemBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlbDescItemBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlbOutroItemBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlCancelamentoBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlbMensagemContribuinteCancBeforePrint(Sender: TObject;
      var PrintIt: Boolean);
    procedure rlbConsumidorCancBeforePrint(Sender: TObject;
      var PrintIt: Boolean);
    procedure rlbMensagemFiscalCancBeforePrint(Sender: TObject;
      var PrintIt: Boolean);
  private
    fACBrNFeDANFCeFortes: TACBrNFeDANFCeFortes;
    fNumItem : Integer;
    fNumPagto: Integer;
    fTotalPagto : Currency;
    fNumObs  : Integer;
    fObsFisco: TStringList;
    fHeightDetItem: Integer;
    fResumido: Boolean;
    fFiltro: TACBrSATExtratoFiltro;

    procedure PintarQRCode(QRCodeData: String; APict: TPicture);
    function CompoemEnderecoCFe: String ;
    function CompoemCliche: String;
  public
    { Public declarations }
    property ACBrNFeDANFCeFortes : TACBrNFeDANFCeFortes read fACBrNFeDANFCeFortes ;
    property Resumido : Boolean read fResumido write fResumido;
    property Filtro         : TACBrSATExtratoFiltro read fFiltro write fFiltro default fiNenhum ;
  end ;

procedure Register;

implementation

uses StrUtils, math,
     ACBrDelphiZXingQRCode, ACBrNFe, ACBrValidador ;

{$ifdef FPC}
  {$R *.lfm}
{$else}
  {$R *.dfm}
{$ENDIF}

procedure Register;
begin
  RegisterComponents('ACBrNFe',[TACBrNFeDANFCeFortes]);
end;

{ TACBrNFeDANFCeFortesFr }

procedure TACBrNFeDANFCeFortesFr.FormCreate(Sender: TObject);
begin
  fNumItem  := 0 ;
  fNumPagto := 0 ;
  fTotalPagto := 0;
  fNumObs   := 0 ;
  fObsFisco := TStringList.Create ;
  fHeightDetItem := rlbDetItem.Height;

  fACBrNFeDANFCeFortes          := TACBrNFeDANFCeFortes(Owner) ;  // Link para o Pai

  //Pega as marges que for defina na classe pai.
  rlVenda.Margins.LeftMargin    := fACBrNFeDANFCeFortes.MargemEsquerda ;
  rlVenda.Margins.RightMargin   := fACBrNFeDANFCeFortes.MargemDireita ;
  rlVenda.Margins.TopMargin     := fACBrNFeDANFCeFortes.MargemSuperior ;
  rlVenda.Margins.BottomMargin  := fACBrNFeDANFCeFortes.MargemInferior ;
end;

procedure TACBrNFeDANFCeFortesFr.rlVendaDataRecord(Sender: TObject;
  RecNo: integer; CopyNo: integer; var Eof: boolean;
  var RecordAction: TRLRecordAction);
begin
  Eof := (RecNo > 1);
  RecordAction := raUseIt;
end;

procedure TACBrNFeDANFCeFortesFr.rlbMensagemFiscalBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  with ACBrNFeDANFCeFortes.FpNFe do
  begin
    PrintIt := True ;
    lMensagemFiscal.Lines.Clear;

    if Ide.tpAmb = taHomologacao then
    begin
      if Ide.tpEmis <> teNormal then
        lMensagemFiscal.Lines.Text := ACBrStr('EMITIDA EM CONTINGÊNCIA - AMB. HOMOLOG. SEM VALOR FISCAL')
      else
        lMensagemFiscal.Lines.Text := ACBrStr( 'EMITIDA EM AMBIENTE DE HOMOLOGAÇÃO - SEM VALOR FISCAL');
    end
    else
    begin
      if Ide.tpEmis <> teNormal then
        lMensagemFiscal.Lines.Text := ACBrStr('EMITIDA EM CONTINGÊNCIA')
      else
        lMensagemFiscal.Lines.Clear;
    end;

    lMensagemFiscal.Visible := (lMensagemFiscal.Lines.Text <> '');

    lNumeroSerie.Caption := ACBrStr(
      'Número ' + IntToStrZero(Ide.nNF, 9) + ' - ' +
      'Série ' + IntToStrZero(Ide.serie, 3)
    );

    lEmissaoVia.Caption := ACBrStr(
      'Emissão ' + DateTimeToStr(Ide.dEmi) + ' - ' +
      'Via ' + IfThen(fACBrNFeDANFCeFortes.ViaConsumidor, 'Consumidor', 'Estabelecimento')
    );

    lTitConsulteChave.Lines.Text := ACBrStr('Consulte pela Chave de Acesso em '+
       TACBrNFe(fACBrNFeDANFCeFortes.ACBrNFe).GetURLConsultaNFCe(Ide.cUF,Ide.tpAmb));

    lChaveDeAcesso.Caption := FormatarChaveAcesso(OnlyNumber(infNFe.ID));

    if (Ide.tpEmis = teNormal ) and (procNFe.cStat = 0) then
    begin
      lChaveDeAcesso.Caption    := ACBrStr('NFC-E NÃO ENVIADA PARA SEFAZ');
      lChaveDeAcesso.Font.Color := clRed;
    end;

    if ACBrNFeDANFCeFortes.NFeCancelada then
      lCancelada.Caption := ACBrStr('NF-e CANCELADA');
  end;
end;

procedure TACBrNFeDANFCeFortesFr.rlbRodapeBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  with ACBrNFeDANFCeFortes.FpNFe do
  begin
    if (Total.ICMSTot.vTotTrib > 0) then
      lTitLei12741.Caption := lTitLei12741.Caption +' '+ FormatFloatBr(Total.ICMSTot.vTotTrib, '#,###,##0.00')
    else
      lTitLei12741.Visible := False;
  end;
end;

procedure TACBrNFeDANFCeFortesFr.rlbMensagemFiscalCancBeforePrint(
  Sender: TObject; var PrintIt: Boolean);
begin
  with ACBrNFeDANFCeFortes.FpNFe do
  begin
    PrintIt := True ;

    if Ide.tpAmb = taHomologacao then
    begin
      lMensagemFiscalCanc.Caption := ACBrStr( 'EMITIDA EM AMBIENTE DE HOMOLOGAÇÃO - SEM VALOR FISCAL');
    end
    else
    begin
      if Ide.tpEmis <> teNormal then
        lMensagemFiscalCanc.Caption := ACBrStr('EMITIDA EM CONTINGÊNCIA')
      else
        lMensagemFiscalCanc.Caption := ACBrStr('');
    end;

    lNumeroSerieCanc.Caption := ACBrStr(
      'Número ' + IntToStrZero(Ide.nNF, 9) + ' - ' +
      'Série ' + IntToStrZero(Ide.serie, 3)
    );

    lEmissaoViaCanc.Caption := ACBrStr(
      'Emissão ' + DateTimeToStr(Ide.dEmi) + ' - ' +
      'Via ' + IfThen(fACBrNFeDANFCeFortes.ViaConsumidor, 'Consumidor', 'Estabelecimento')
    );

    lTitConsulteChaveCanc.Lines.Text := ACBrStr('Consulte pela Chave de Acesso em '+
       TACBrNFe(fACBrNFeDANFCeFortes.ACBrNFe).GetURLConsultaNFCe(Ide.cUF,Ide.tpAmb));

    lChaveDeAcessoCanc.Caption := FormatarChaveAcesso(OnlyNumber(infNFe.ID));

    if procNFe.cStat = 0 then
    begin
      lChaveDeAcessoCanc.Caption    := ACBrStr('NFC-E NÃO ENVIADA PARA SEFAZ');
      lChaveDeAcessoCanc.Font.Color := clRed;
    end;

    if ACBrNFeDANFCeFortes.NFeCancelada then
      lCanceladaCanc.Caption := ACBrStr('NF-e CANCELADA');
  end;
end;

procedure TACBrNFeDANFCeFortesFr.rlbOutroItemBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  with ACBrNFeDANFCeFortes.FpNFe.Det.Items[fNumItem] do
  begin
    PrintIt := (not Resumido) and (Prod.vOutro > 0) and (ACBrNFeDANFCeFortes.ImprimeDescAcrescItem);

    if PrintIt then
    begin
      lOutro.Caption       := FormatFloatBr(Prod.vOutro,'+#,###,##0.00');
      lOutroValLiq.Caption := FormatFloatBr(Prod.vProd+Prod.vOutro-Prod.vDesc,'#,###,##0.00');
    end;
  end;
end;

procedure TACBrNFeDANFCeFortesFr.rlsbDetItemDataRecord(Sender: TObject;
  RecNo: integer; CopyNo: integer; var Eof: boolean;
  var RecordAction: TRLRecordAction);
begin
  fNumItem := RecNo - 1 ;

  Eof := (RecNo > ACBrNFeDANFCeFortes.FpNFe.Det.Count) ;
  RecordAction := raUseIt ;
end;

procedure TACBrNFeDANFCeFortesFr.rlsbPagamentosDataRecord(Sender: TObject;
  RecNo: integer; CopyNo: integer; var Eof: boolean;
  var RecordAction: TRLRecordAction);
begin
  fNumPagto := RecNo - 1 ;

  Eof := (RecNo > ACBrNFeDANFCeFortes.FpNFe.pag.Count) ;
  RecordAction := raUseIt ;
end;

procedure TACBrNFeDANFCeFortesFr.PintarQRCode(QRCodeData: String; APict: TPicture);
var
  QRCode: TDelphiZXingQRCode;
  QRCodeBitmap: TBitmap;
  Row, Column: Integer;
begin
  QRCode       := TDelphiZXingQRCode.Create;
  QRCodeBitmap := TBitmap.Create;
  try
    QRCode.Data      := QRCodeData;
    QRCode.Encoding  := qrUTF8NoBOM;
    QRCode.QuietZone := 1;

    //QRCodeBitmap.SetSize(QRCode.Rows, QRCode.Columns);
    QRCodeBitmap.Width  := QRCode.Columns;
    QRCodeBitmap.Height := QRCode.Rows;

    for Row := 0 to QRCode.Rows - 1 do
    begin
      for Column := 0 to QRCode.Columns - 1 do
      begin
        if (QRCode.IsBlack[Row, Column]) then
          QRCodeBitmap.Canvas.Pixels[Column, Row] := clBlack
        else
          QRCodeBitmap.Canvas.Pixels[Column, Row] := clWhite;
      end;
    end;

    APict.Assign(QRCodeBitmap);
  finally
    QRCode.Free;
    QRCodeBitmap.Free;
  end;
end;

function TACBrNFeDANFCeFortesFr.CompoemEnderecoCFe: String;
var
  Endereco, CEP: String;
begin
  with ACBrNFeDANFCeFortes.FpNFe do
  begin
    // Definindo dados do Cliche //
    Endereco := Emit.EnderEmit.xLgr ;
    if (Emit.EnderEmit.nro <> '') then
      Endereco := Endereco + ', '+Emit.EnderEmit.nro;
    if (Emit.EnderEmit.xCpl <> '') then
      Endereco := Endereco + ' - '+Emit.EnderEmit.xCpl;
    if (Emit.EnderEmit.xBairro <> '') then
      Endereco := Endereco + ' - '+Emit.EnderEmit.xBairro;
    if (Emit.EnderEmit.xMun <> '') then
      Endereco := Endereco + ' - '+Emit.EnderEmit.xMun;
    if (Emit.EnderEmit.UF <> '') then
      Endereco := Endereco + ' - '+Emit.EnderEmit.UF;
    if (Emit.EnderEmit.CEP <> 0) then
    begin
      CEP := IntToStr(Emit.EnderEmit.CEP);
      Endereco := Endereco + ' - '+copy(CEP,1,5)+'-'+copy(CEP,6,3);
    end;
    if (Emit.EnderEmit.fone <> '') then
      Endereco := Endereco + ' - FONE: '+Emit.EnderEmit.fone;
  end;

  Result := Endereco;
end;

function TACBrNFeDANFCeFortesFr.CompoemCliche: String;
var
  CNPJ_IE_IM: String;
begin
  with ACBrNFeDANFCeFortes.FpNFe do
  begin
    CNPJ_IE_IM := 'CNPJ:'+Emit.CNPJCPF ;
    if (Emit.IE <> '') then
      CNPJ_IE_IM := CNPJ_IE_IM + ' IE:'+Emit.IE;
    if (Emit.IM <> '') then
      CNPJ_IE_IM := CNPJ_IE_IM + ' IM:'+Emit.IM;
  end;

  Result := CNPJ_IE_IM;
end;

procedure TACBrNFeDANFCeFortesFr.rlVendaBeforePrint(Sender: TObject;
  var PrintIt: boolean);
var
  qrcode: String;
  TotalPaginaPixel: Integer;
  LogoStream: TStringStream;
begin
  fNumItem  := 0;
  fNumPagto := 0;
  fTotalPagto := 0;
  fNumObs   := 0;
  fObsFisco.Clear;

  imgLogo.Height:=100;

  with ACBrNFeDANFCeFortes.FpNFe do
  begin
    lNomeFantasia.Lines.Text:= Emit.xFant ;
    lRazaoSocial.Lines.Text := Emit.xNome+' '+FormatarCNPJ(Emit.CNPJCPF) ;
    lEndereco.Lines.Text    := CompoemEnderecoCFe;

    if ACBrNFeDANFCeFortes.Logo <> '' then
    begin
      imgLogo.Height := ACBrNFeDANFCeFortes.TamanhoLogoHeight ;
      imgLogo.Width := ACBrNFeDANFCeFortes.TamanhoLogoWidth ;
      imgLogo.AutoSize := ACBrNFeDANFCeFortes.ExpandirLogoMarca ;

      if FileExists (ACBrNFeDANFCeFortes.Logo) then
        imgLogo.Picture.LoadFromFile(ACBrNFeDANFCeFortes.Logo)
      else
      begin
        LogoStream := TStringStream.Create(ACBrNFeDANFCeFortes.Logo);
        try
          imgLogo.Picture.Bitmap.LoadFromStream(LogoStream);
        finally
          LogoStream.Free;
        end;
      end;
    end;

    // QRCode  //
    if EstaVazio(Trim(infNFeSupl.qrCode)) then
      qrcode := TACBrNFe(ACBrNFeDANFCeFortes.ACBrNFe).GetURLQRCode( ide.cUF, ide.tpAmb,
                                     OnlyNumber(InfNFe.ID),  //correcao para pegar somente numeros, estava indo junto o NFE
                                     ifthen(Dest.idEstrangeiro <> '',Dest.idEstrangeiro, OnlyNumber(Dest.CNPJCPF)),
                                     ide.dEmi,
                                     Total.ICMSTot.vNF, Total.ICMSTot.vICMS,
                                     signature.DigestValue)
    else
      qrcode := infNFeSupl.qrCode;

    PintarQRCode( qrcode, imgQRCode.Picture );

    if not EstaVazio(Trim(procNFe.nProt)) then
      lProtocolo.Caption := ACBrStr('Protocolo de Autorização: '+procNFe.nProt+
                              ' '+ifthen(procNFe.dhRecbto<>0,DateTimeToStr(procNFe.dhRecbto),''))
    else
      lProtocolo.Caption := '';

  end;


  // Calculando o tamanho da Pagina em Pixels //
  TotalPaginaPixel := rlbsCabecalho.Height +
                      rlbRodape.Height +
                      rlbLegenda.Height +
                      rlbPagamento.Height +
                      rlbMensagemFiscal.Height +
                      rlbConsumidor.Height +
                      rlsbDetItem.Height +
                      Trunc(rlbDetItem.Height * ACBrNFeDANFCeFortes.FpNFe.Det.Count) ;
  // Pixel para Milimitros //
  rlVenda.PageSetup.PaperHeight := max( 100, 10+Trunc( TotalPaginaPixel / 3.75 ));
end;

procedure TACBrNFeDANFCeFortesFr.rlbDescItemBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  with ACBrNFeDANFCeFortes.FpNFe.Det.Items[fNumItem] do
  begin
    PrintIt := (not Resumido) and (Prod.vDesc > 0) and (ACBrNFeDANFCeFortes.ImprimeDescAcrescItem) ;

    if PrintIt then
    begin
      lDesconto.Caption   := FormatFloatBr(Prod.vDesc,'-#,###,##0.00');
      if (Prod.vOutro > 0) then
      begin
        lTitDescValLiq.Visible := False;
        lDescValLiq.Visible := False;
        rlbDescItem.Height := 12;
      end
      else
      begin
        rlbDescItem.Height := 24;
        lTitDescValLiq.Visible := True;
        lDescValLiq.Visible := True;
        lDescValLiq.Caption := FormatFloatBr(Prod.vProd+Prod.vOutro-Prod.vDesc,'#,###,##0.00');
      end;
    end;
  end;
end;

procedure TACBrNFeDANFCeFortesFr.rlbDetItemBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
var
  LinhaTotal : string;
  LinhaItem : String;
begin
  PrintIt := not Resumido;
  if not PrintIt then exit;

  mLinhaItem.Lines.Clear ;
  with ACBrNFeDANFCeFortes.FpNFe.Det.Items[fNumItem] do
  begin
    LinhaItem := IntToStrZero(Prod.nItem,3) + ' ' +
                             ACBrNFeDANFCeFortes.ManterCodigo( Prod.cEAN , Prod.cProd ) + ' ' +
                             Trim(Prod.xProd);

    if Trim(infAdProd) <> '' then
      LinhaItem := LinhaItem + '-'+ StringReplace( infAdProd, ';',#13,[rfReplaceAll]);

    mLinhaItem.Lines.Add(LinhaItem);

    //Centraliza os valores. A fonte dos itens foi mudada para Courier New, Pois esta o espaço tem o mesmo tamanho dos demais caractere.
    LinhaTotal  := PadLeft( ACBrNFeDANFCeFortes.FormatQuantidade(Prod.qCom), 12) +
                   PadCenter(Trim(Prod.uCom), 5) + ' X ' +
                   PadLeft(ACBrNFeDANFCeFortes.FormatValorUnitario(Prod.vUnCom), 12) +
                   PadLeft(FormatFloatBr(Prod.vProd, '###,###,##0.00'), 12);

    mLinhaItem.Lines.Add(LinhaTotal);
  end;
end;


procedure TACBrNFeDANFCeFortesFr.rlbPagamentoBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  with ACBrNFeDANFCeFortes.FpNFe.pag.Items[fNumPagto] do
  begin
    lMeioPagamento.Caption  := ACBrStr(FormaPagamentoToDescricao(tPag));
    lPagamento.Caption      := FormatFloatBr(vPag,'#,###,##0.00');
    fTotalPagto             := fTotalPagto + vPag;
  end;
end;

procedure TACBrNFeDANFCeFortesFr.rlbGapBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  PrintIt := not Resumido;
end;

procedure TACBrNFeDANFCeFortesFr.rlbLegendaBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  PrintIt := not Resumido;
end;

procedure TACBrNFeDANFCeFortesFr.FormDestroy(Sender: TObject);
begin
  fObsFisco.Free;
end;

procedure TACBrNFeDANFCeFortesFr.lSistemaBeforePrint(Sender: TObject;
  var Text: string; var PrintIt: Boolean);
begin
  PrintIt := True;
  if trim(fACBrNFeDANFCeFortes.Sistema) <> '' then
    Text := fACBrNFeDANFCeFortes.Sistema ;
end;

procedure TACBrNFeDANFCeFortesFr.pAsteriscoBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  PrintIt := not Resumido;
end;

procedure TACBrNFeDANFCeFortesFr.rlbConsumidorBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  with ACBrNFeDANFCeFortes.FpNFe do
  begin
    if (Dest.idEstrangeiro = '') and
       (Dest.CNPJCPF = '') then
     begin
        lConsumidor.Lines.Text := ACBrStr('CONSUMIDOR NÃO IDENTIFICADO');
     end
    else if Dest.idEstrangeiro <> '' then
     begin
       lConsumidor.Lines.Text  := 'CONSUMIDOR Id. Estrangeiro: '+Dest.idEstrangeiro+' '+Dest.xNome;
     end
    else
     begin
       if Length(trim(Dest.CNPJCPF)) > 11 then
          lConsumidor.Lines.Text  := 'CONSUMIDOR CNPJ: '+FormatarCNPJ(Dest.CNPJCPF)
       else
          lConsumidor.Lines.Text  := 'CONSUMIDOR CPF: '+FormatarCPF(Dest.CNPJCPF);

       lConsumidor.Lines.Text  := lConsumidor.Caption+' '+Dest.xNome;
     end;
     lEnderecoConsumidor.Lines.Text := Trim(Dest.EnderDest.xLgr)+' '+
                                       Trim(Dest.EnderDest.nro)+' '+
                                       Trim(Dest.EnderDest.xCpl)+' '+
                                       Trim(Dest.EnderDest.xBairro)+' '+
                                       Trim(Dest.EnderDest.xMun);
  end;
end;

procedure TACBrNFeDANFCeFortesFr.rlbConsumidorCancBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  with ACBrNFeDANFCeFortes.FpNFe do
  begin
    if (Dest.idEstrangeiro = '') and
       (Dest.CNPJCPF = '') then
     begin
        lCPF_CNPJ_ID_Canc.Lines.Text := ACBrStr('CONSUMIDOR NÃO IDENTIFICADO');
     end
    else if Dest.idEstrangeiro <> '' then
     begin
       lCPF_CNPJ_ID_Canc.Lines.Text  := 'CONSUMIDOR ID Estrangeiro: '+Dest.idEstrangeiro+' '+Dest.xNome;
     end
    else
     begin
       if Length(trim(Dest.CNPJCPF)) > 11 then
          lCPF_CNPJ_ID_Canc.Lines.Text  := 'CONSUMIDOR CNPJ: '+FormatarCNPJ(Dest.CNPJCPF)
       else
          lCPF_CNPJ_ID_Canc.Lines.Text  := 'CONSUMIDOR CPF: '+FormatarCPF(Dest.CNPJCPF);

       lCPF_CNPJ_ID_Canc.Lines.Text  := lCPF_CNPJ_ID_Canc.Caption+' '+Dest.xNome;
     end;
     lEnderecoConsumidorCanc.Lines.Text := Trim(Dest.EnderDest.xLgr)+' '+
                                           Trim(Dest.EnderDest.nro)+' '+
                                           Trim(Dest.EnderDest.xCpl)+' '+
                                           Trim(Dest.EnderDest.xBairro)+' '+
                                           Trim(Dest.EnderDest.xMun);
  end;
end;

procedure TACBrNFeDANFCeFortesFr.rlbMensagemContribuinteCancBeforePrint(
  Sender: TObject; var PrintIt: Boolean);
begin
  Printit := False;

  with ACBrNFeDANFCeFortes.FpNFe.InfAdic do
  begin
    if infCpl <> '' then
    begin
      PrintIt := True ;

      lObservacoesCanc.Lines.Add( StringReplace( infCpl, ';', #13, [rfReplaceAll] ) );
    end;
  end;
end;

procedure TACBrNFeDANFCeFortesFr.rlbsCabecalhoDataRecord(Sender: TObject;
  RecNo: integer; CopyNo: integer; var Eof: boolean;
  var RecordAction: TRLRecordAction);
begin
  Eof := (RecNo > 1);
  RecordAction := raUseIt;
end;

procedure TACBrNFeDANFCeFortesFr.rlbTotalBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  lQtdTotalItensVal.Caption := IntToStrZero(ACBrNFeDANFCeFortes.FpNFe.Det.Count,3);
  lTotal.Caption := FormatFloatBr(ACBrNFeDANFCeFortes.FpNFe.Total.ICMSTot.vNF,'#,###,##0.00');
end;

procedure TACBrNFeDANFCeFortesFr.rlbTrocoBeforePrint(Sender: TObject;
  var PrintIt: boolean);
var
  Troco : Currency ;
begin
  Troco   := fACBrNFeDANFCeFortes.vTroco;
  PrintIt := (Troco> 0);

  if PrintIt then
    lTroco.Caption := FormatFloatBr(fACBrNFeDANFCeFortes.vTroco,'#,###,##0.00');;
end;

procedure TACBrNFeDANFCeFortesFr.rlCancelamentoBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
var
  qrcode: String;
  TotalPaginaPixel: Integer;
  LogoStream: TStringStream;
begin
  fNumItem  := 0;
  fNumPagto := 0;
  fTotalPagto := 0;
  fNumObs   := 0;
  fObsFisco.Clear;

  imgLogo.Height:=100;

  with ACBrNFeDANFCeFortes.FpNFe do
  begin
    lNomeFantasiaCanc.Lines.Text:= Emit.xFant ;
    lRazaoSocialCanc.Lines.Text := Emit.xNome ;
    lEmitCNPJ_IE_IM_Camc.Caption:= CompoemCliche;
    lEnderecoCanc.Lines.Text    := CompoemEnderecoCFe;

    if ACBrNFeDANFCeFortes.Logo <> '' then
    begin
      if FileExists (ACBrNFeDANFCeFortes.Logo) then
        imgLogo.Picture.LoadFromFile(ACBrNFeDANFCeFortes.Logo)
      else
      begin
        LogoStream := TStringStream.Create(ACBrNFeDANFCeFortes.Logo);
        try
          imgLogo.Picture.Bitmap.LoadFromStream(LogoStream);
        finally
          LogoStream.Free;
        end;
      end;
    end;

    // QRCode  //
    if EstaVazio(Trim(infNFeSupl.qrCode)) then
      qrcode := TACBrNFe(ACBrNFeDANFCeFortes.ACBrNFe).GetURLQRCode( ide.cUF, ide.tpAmb,
                                     OnlyNumber(InfNFe.ID),  //correcao para pegar somente numeros, estava indo junto o NFE
                                     ifthen(Dest.idEstrangeiro <> '',Dest.idEstrangeiro, OnlyNumber(Dest.CNPJCPF)),
                                     ide.dEmi,
                                     Total.ICMSTot.vNF, Total.ICMSTot.vICMS,
                                     signature.DigestValue)
    else
      qrcode := infNFeSupl.qrCode;

    PintarQRCode( qrcode , imgQRCodeCanc.Picture );

    lProtocoloCanc.Caption := ACBrStr('Protocolo de Autorização: '+procNFe.nProt+
                           ' '+ifthen(procNFe.dhRecbto<>0,DateTimeToStr(procNFe.dhRecbto),''));

  end;


  // Calculando o tamanho da Pagina em Pixels //
  TotalPaginaPixel := rlbsCabecalho.Height +
                      rlbRodape.Height +
                      rlbLegenda.Height +
                      rlbPagamento.Height +
                      rlbMensagemFiscal.Height +
                      rlbConsumidor.Height +
                      rlsbDetItem.Height +
                      Trunc(rlbDetItem.Height * ACBrNFeDANFCeFortes.FpNFe.Det.Count) ;
  // Pixel para Milimitros //
  rlVenda.PageSetup.PaperHeight := max( 100, 10+Trunc( TotalPaginaPixel / 3.75 ));
end;

{ TACBrNFeDANFCeFortes }

constructor TACBrNFeDANFCeFortes.Create(AOwner: TComponent);
begin
  inherited create( AOwner );

end;

destructor TACBrNFeDANFCeFortes.Destroy;
begin

  inherited Destroy ;
end;

procedure TACBrNFeDANFCeFortes.ImprimirDANFE(NFE: TNFe);
begin
  if NFe = nil then
   begin
     if not Assigned(ACBrNFe) then
        raise Exception.Create('Componente ACBrNFe não atribuído');

     FpNFe := TACBrNFe(ACBrNFe).NotasFiscais.Items[0].NFe;
   end
  else
    FpNFe := NFE;

  Imprimir(False);
end;

procedure TACBrNFeDANFCeFortes.ImprimirDANFECancelado(NFE: TNFe);
begin
  if NFe = nil then
   begin
     if not Assigned(ACBrNFe) then
        raise Exception.Create('Componente ACBrNFe não atribuí­do');

     FpNFe := TACBrNFe(ACBrNFe).NotasFiscais.Items[0].NFe;
   end
  else
    FpNFe := NFE;

  ImprimirCancelado(True);
end;

procedure TACBrNFeDANFCeFortes.ImprimirDANFEResumido(NFE: TNFe);
begin
  if NFe = nil then
   begin
     if not Assigned(ACBrNFe) then
        raise Exception.Create('Componente ACBrNFe não atribuí­do');

     FpNFe := TACBrNFe(ACBrNFe).NotasFiscais.Items[0].NFe;
   end
  else
    FpNFe := NFE;

  Imprimir(True);
end;

procedure TACBrNFeDANFCeFortes.ImprimirDANFEPDF(NFE: TNFe);
begin
//  inherited ImprimirDANFEPDF(NFE);
  if NFe = nil then
   begin
     if not Assigned(ACBrNFe) then
        raise Exception.Create('Componente ACBrNFe nÃo atribuí­do');

     FpNFe := TACBrNFe(ACBrNFe).NotasFiscais.Items[0].NFe;
   end
  else
    FpNFe := NFE;
  Imprimir(False, fiPDF);
end;

procedure TACBrNFeDANFCeFortes.ImprimirDANFEResumidoPDF(NFE: TNFe);
begin
  //  inherited ImprimirDANFEPDF(NFE);
    if NFe = nil then
     begin
       if not Assigned(ACBrNFe) then
          raise Exception.Create('Componente ACBrNFe nÃo atribuí­do');

       FpNFe := TACBrNFe(ACBrNFe).NotasFiscais.Items[0].NFe;
     end
    else
      FpNFe := NFE;

    Imprimir(True, fiPDF);
end;

procedure TACBrNFeDANFCeFortes.Imprimir(const DanfeResumido: Boolean;
  const AFiltro: TACBrSATExtratoFiltro);
var
  frACBrNFeDANFCeFortesFr: TACBrNFeDANFCeFortesFr;
  RLLayout: TRLReport;
  RLFiltro: TRLCustomSaveFilter;
begin
  frACBrNFeDANFCeFortesFr := TACBrNFeDANFCeFortesFr.Create(Self);
  try
    with frACBrNFeDANFCeFortesFr do
    begin
      Filtro := AFiltro;
      RLLayout := rlVenda;
      Resumido := DanfeResumido;

      RLPrinter.Copies := NumCopias ;

      if not EstaVazio(FImpressora) then
        RLPrinter.PrinterName := FImpressora;

      RLLayout.ShowProgress := ACBrNFeDANFCeFortes.MostrarStatus;
      RLLayout.PrintDialog  := not(FMostrarPreview) and (EstaVazio(FImpressora));

      if Filtro = fiNenhum then
      begin
        if MostrarPreview then
          RLLayout.PreviewModal
        else
          RLLayout.Print;
      end
      else
      begin
        if RLLayout.Prepare then
        begin
          case Filtro of
            fiPDF  : RLFiltro := RLPDFFilter1;
            fiHTML : RLFiltro := RLHTMLFilter1;
          else
            exit ;
          end ;

          RLFiltro.ShowProgress := ACBrNFeDANFCeFortes.MostrarStatus;
          RLFiltro.FileName := PathWithDelim(ACBrNFeDANFCeFortes.PathPDF) + OnlyNumber(ACBrNFeDANFCeFortes.FpNFe.infNFe.ID) + '-nfe.pdf';
          RLFiltro.FilterPages( RLLayout.Pages );
        end;
      end;
    end;
  finally
    frACBrNFeDANFCeFortesFr.Free ;
  end;
end;

procedure TACBrNFeDANFCeFortes.ImprimirCancelado(const DanfeResumido: Boolean;
  const AFiltro: TACBrSATExtratoFiltro);
var
  frACBrNFeDANFCeFortesFr: TACBrNFeDANFCeFortesFr;
  RLLayout: TRLReport;
  RLFiltro: TRLCustomSaveFilter;
begin
  frACBrNFeDANFCeFortesFr := TACBrNFeDANFCeFortesFr.Create(Self);
  try
    with frACBrNFeDANFCeFortesFr do
    begin
      Filtro := AFiltro;
      RLLayout := rlCancelamento;
      Resumido := DanfeResumido;

      RLPrinter.Copies := NumCopias ;

      if ACBrNFeDANFCeFortes.Impressora <> '' then
        RLPrinter.PrinterName := ACBrNFeDANFCeFortes.Impressora;

      RLLayout.PrintDialog := ACBrNFeDANFCeFortes.MostrarPreview;
      RLLayout.ShowProgress:= ACBrNFeDANFCeFortes.MostrarStatus;

      if Filtro = fiNenhum then
      begin
        if MostrarPreview then
          RLLayout.PreviewModal
        else
          RLLayout.Print;
      end
      else
      begin
        if RLLayout.Prepare then
        begin
          case Filtro of
            fiPDF  : RLFiltro := RLPDFFilter1;
            fiHTML : RLFiltro := RLHTMLFilter1;
          else
            exit ;
          end ;

          RLFiltro.ShowProgress := ACBrNFeDANFCeFortes.MostrarStatus;
          RLFiltro.FileName := ACBrNFeDANFCeFortes.PathPDF + OnlyNumber(ACBrNFeDANFCeFortes.FpNFe.infNFe.ID) + '-nfe.pdf';
          RLFiltro.FilterPages( RLLayout.Pages );
        end;
      end;
    end;
  finally
    frACBrNFeDANFCeFortesFr.Free ;
  end;
end;

{$ifdef FPC}
initialization
   {$I ACBrNFeDANFCeFortes.lrs}
{$endif}

end.
