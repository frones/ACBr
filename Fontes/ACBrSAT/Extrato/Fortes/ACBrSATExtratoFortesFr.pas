{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2014 Daniel Simoes de Almeida               }
{                                                                              }
{ This file uses: DelphiZXIngQRCode Copyright 2008 ZXing authors,              }
{   port to Delphi, by Debenu Pty Ltd                                          }
{   URL: http://www.debenu.com/open-sourc1e/delphizxingqrcode                  }
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
|* 04/04/2013:  André Ferreira de Moraes
|*   Inicio do desenvolvimento
******************************************************************************}
{$I ACBr.inc}

unit ACBrSATExtratoFortesFr;

interface

uses Classes, SysUtils,
     {$IFDEF FPC}
       LResources,
     {$ENDIF}
     Forms, Graphics,
     ACBrSATExtratoClass, ACBrSATExtratoReportClass,
     pcnCFe, pcnCFeCanc, pcnConversao,
     RLConsts, RLReport, RLBarcode, RLPDFFilter, RLHTMLFilter, RLPrintDialog,
     RLFilters, RLPrinters, Controls, StrUtils;

type

  { TACBrSATExtratoFortes }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TACBrSATExtratoFortes = class( TACBrSATExtratoReportClass )
  private
  protected
    procedure Imprimir;
  public
    procedure ImprimirExtrato(ACFe: TCFe = nil); override;
    procedure ImprimirExtratoResumido(ACFe : TCFe = nil); override;
    procedure ImprimirExtratoCancelamento(ACFe : TCFe = nil; ACFeCanc: TCFeCanc = nil); override;
  end ;

  { TACBrSATExtratoFortesFr }

  TACBrSATExtratoFortesFr = class(TForm)
    bcChaveAcesso2: TRLBarcode;
    bcChaveAcessoCan1: TRLBarcode;
    bcChaveAcessoCan2: TRLBarcode;
    bcChaveAcessoCanl1: TRLBarcode;
    bcChaveAcessoCanl2: TRLBarcode;
    imgQRCodeCan: TRLImage;
    imgQRCodeCanl: TRLImage;
    lBaseCalcISSQN: TRLLabel;
    lCabItem: TRLMemo;
    lChaveAcesso: TRLMemo;
    lChaveAcessoCan: TRLMemo;
    lChaveAcessoCanl: TRLMemo;
    lCPF_CNPJ: TRLMemo;
    lCPF_CNPJCan: TRLMemo;
    lCupomFiscalEletronico: TRLMemo;
    lCupomFiscalEletronicoCan: TRLMemo;
    lDataHoraCan: TRLLabel;
    lDataHoraCanl: TRLLabel;
    lDeducISSQN: TRLLabel;
    lDesconto: TRLLabel;
    lDescValLiq: TRLLabel;
    lEmitCNPJ_IE_IM: TRLMemo;
    lEmitCNPJ_IE_IMCan: TRLMemo;
    lEndereco: TRLMemo;
    lEnderecoCan: TRLMemo;
    lFiller1: TRLLabel;
    lFiller2: TRLLabel;
    lFiller3: TRLLabel;
    lFiller4: TRLLabel;
    lFiller5: TRLLabel;
    lFiller6: TRLLabel;
    lNomeFantasia: TRLMemo;
    lNomeFantasiaCan: TRLMemo;
    lNumeroExtrato: TRLLabel;
    lNumeroExtratoCan: TRLLabel;
    lNumSATCan: TRLLabel;
    lNumSATCanl: TRLLabel;
    lOutro: TRLLabel;
    lOutroValLiq: TRLLabel;
    lRazaoSocial: TRLMemo;
    lRazaoSocialCan: TRLMemo;
    lRazaoSocialNome: TRLMemo;
    lRazaoSocialNomeCanc: TRLMemo;
    lTesteCan: TRLMemo;
    lTitCancelamento: TRLLabel;
    lTitCancelamento1: TRLMemo;
    lTitCancelamento2: TRLMemo;
    lTitLei12741: TRLMemo;
    lTitLei12743: TRLMemo;
    lTitObsContrib: TRLMemo;
    lTitSATCan: TRLLabel;
    lTeste: TRLMemo;
    lTitSATCanl: TRLLabel;
    lTitTotalCan: TRLLabel;
    lTotalCan: TRLLabel;
    lValLei12741: TRLLabel;
    lPagamento: TRLLabel;
    lMeioPagamento: TRLLabel;
    lTitTotAcrescimos: TRLLabel;
    lTitTotal: TRLLabel;
    lTitTroco: TRLLabel;
    lTitTotDescontos: TRLLabel;
    lTotAcrescimos: TRLLabel;
    lSubTotal: TRLLabel;
    lNumSAT: TRLLabel;
    lTitSAT: TRLLabel;
    bcChaveAcesso1: TRLBarcode;
    lSequencia: TRLLabel;
    lTitDeducISSQN: TRLLabel;
    lTitSubTotal: TRLLabel;
    lTitDesconto: TRLLabel;
    lTitAcrescimo: TRLLabel;
    lTitDescValLiq: TRLLabel;
    lTitOutroValLiq: TRLLabel;
    lTitBaseCalcISSQN: TRLLabel;
    lTotal: TRLLabel;
    lTroco: TRLLabel;
    lTotalItem: TRLLabel;
    lTotDescontos: TRLLabel;
    mDestEnt: TRLMemo;
    mEndEnt: TRLMemo;
    mMsgAppQRCode: TRLMemo;
    mSwHouseSite: TRLMemo;
    mMsgAppQRCodeCanc: TRLMemo;
    mObsContrib: TRLMemo;
    mLinhaItem: TRLMemo;
    mSwHouseSiteCanc: TRLMemo;
    pAsterisco: TRLPanel;
    pGap6: TRLPanel;
    pGap7: TRLPanel;
    pEspacoFinalCan: TRLPanel;
    pNumSATCan: TRLPanel;
    pNumSATCancl: TRLPanel;
    rlbDadosCupomCancelado: TRLBand;
    rlbCabecalhoCan: TRLBand;
    rlbConsumidor: TRLBand;
    rlbDadosCliche: TRLBand;
    rlbGap: TRLBand;
    rlbLegenda: TRLBand;
    rlbNumExtrato: TRLBand;
    rlbCanRodape: TRLBand;
    rlbTeste: TRLBand;
    rlbTesteCan: TRLBand;
    rlDadosEntrega: TRLBand;
    rlbPagamento: TRLBand;
    rlbObsFisco: TRLBand;
    rlbDescontos: TRLBand;
    rlbAcrescimos: TRLBand;
    rlbTotal: TRLBand;
    rlbSubTotal: TRLBand;
    rlbDescItem: TRLBand;
    rlbOutroItem: TRLBand;
    rlbDetItem: TRLBand;
    rlbDeducISSQN: TRLBand;
    rlbRodape: TRLBand;
    rlbTroco: TRLBand;
    RLDraw1: TRLDraw;
    RLDraw10: TRLDraw;
    RLDraw11: TRLDraw;
    RLDraw12: TRLDraw;
    RLDraw13: TRLDraw;
    RLDraw3: TRLDraw;
    rlbsCabecalho: TRLSubDetail;
    RLDraw4: TRLDraw;
    RLDraw5: TRLDraw;
    RLDraw8: TRLDraw;
    RLDraw9: TRLDraw;
    RLPanel1: TRLPanel;
    RLPanel2: TRLPanel;
    RLPanel3: TRLPanel;
    pTotalCanc: TRLPanel;
    pEspacoFinal: TRLPanel;
    rlVenda: TRLReport;
    rlObsContrib: TRLBand;
    RLDraw2: TRLDraw;
    RLDraw6: TRLDraw;
    RLDraw7: TRLDraw;
    RLHTMLFilter1: TRLHTMLFilter;
    lDataHora: TRLLabel;
    imgQRCode: TRLImage;
    pGap05: TRLPanel;
    mObsFisco: TRLMemo;
    lTitDadosParaEntrega: TRLLabel;
    pLei12741: TRLPanel;
    pNumSAT: TRLPanel;
    RLPDFFilter1: TRLPDFFilter;
    rlsbDetItem: TRLSubDetail;
    rlsbPagamentos: TRLSubDetail;
    rlsbObsFisco: TRLSubDetail;
    rlCancelamento: TRLReport;
    rlLogo: TRLBand;
    imgLogo: TRLImage;
    rlLogoCanc: TRLBand;
    imgLogoCanc: TRLImage;

    procedure FormDestroy(Sender: TObject);
    procedure lCPF_CNPJBeforePrint(Sender: TObject; var OutputText: string;
      var PrintIt: boolean);
    procedure lRazaoSocialNomeBeforePrint(Sender: TObject; var Text: string;
      var PrintIt: Boolean);
    procedure pAsteriscoBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure pLei12741BeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure pNumSATBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure pNumSATCanBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure pNumSATCanclBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure pTotalCancBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlbConsumidorBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlbsCabecalhoDataRecord(Sender: TObject; RecNo: integer;
      CopyNo: integer; var Eof: boolean; var RecordAction: TRLRecordAction);
    procedure rlbTesteBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlbAcrescimosBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlbLegendaBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlbDescItemBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlbDescontosBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlbDetItemBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlbDeducISSQNBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlbObsFiscoBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlbOutroItemBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlbPagamentoBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlbGapBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlbSubTotalBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlbTotalBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlbTrocoBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlCancelamentoBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlVendaBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure FormCreate(Sender: TObject);
    procedure rlVendaDataRecord(Sender: TObject; RecNo: integer;
      CopyNo: integer; var Eof: boolean; var RecordAction: TRLRecordAction);
    procedure rlDadosEntregaBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlObsContribBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlsbDetItemDataRecord(Sender: TObject; RecNo: integer;
      CopyNo: integer; var Eof: boolean; var RecordAction: TRLRecordAction);
    procedure rlsbObsFiscoDataRecord(Sender: TObject; RecNo: integer;
      CopyNo: integer; var Eof: boolean; var RecordAction: TRLRecordAction);
    procedure rlsbPagamentosDataRecord(Sender: TObject; RecNo: integer;
      CopyNo: integer; var Eof: boolean; var RecordAction: TRLRecordAction);
  private
    fACBrSATExtrato: TACBrSATExtratoFortes;
    fNumItem : Integer;
    fNumPagto: Integer;
    fNumObs  : Integer;
    fObsFisco: TStringList;
    fHeightDetItem: Integer;
    fResumido: Boolean;

    procedure PintarQRCode(QRCodeData: String; APict: TPicture);
    function CompoemEnderecoCFe: String ;
    function CompoemCliche: String;
    function CalcularCaractesWidth( Canvas : TCanvas; WidthTotal : Integer ): Integer;
  public
    { Public declarations }
    property ACBrSATExtrato : TACBrSATExtratoFortes read fACBrSATExtrato ;
    property Resumido : Boolean read fResumido write fResumido;
  end ;

procedure Register;

implementation

uses  math, RLTypes,
     ACBrDelphiZXingQRCode, ACBrValidador, ACBrDFeUtil, ACBrUtil;

{$ifdef FPC}
  {$R *.lfm}
{$else}
  {$R *.dfm}
  {$R ACBrSATExtratoFortesFr.dcr}
{$ENDIF}

procedure Register;
begin
  RegisterComponents('ACBrSAT',[TACBrSATExtratoFortes]);
end;

{ TACBrSATExtratoFortesFr }

procedure TACBrSATExtratoFortesFr.FormCreate(Sender: TObject);
begin
  fNumItem  := 0 ;
  fNumPagto := 0 ;
  fNumObs   := 0 ;
  fObsFisco := TStringList.Create ;
  fHeightDetItem := rlbDetItem.Height;
  fResumido := false;

  fACBrSATExtrato := TACBrSATExtratoFortes(Owner) ;  // Link para o Pai

  with fACBrSATExtrato do
  begin
    rlVenda.Width := LarguraBobina;
    rlVenda.Margins.LeftMargin   := Margens.Esquerda;
    rlVenda.Margins.RightMargin  := Margens.Direita;
    rlVenda.Margins.TopMargin    := Margens.Topo;
    rlVenda.Margins.BottomMargin := Margens.Fundo;

    rlCancelamento.Width := LarguraBobina;
    rlCancelamento.Margins.LeftMargin   := Margens.Esquerda;
    rlCancelamento.Margins.RightMargin  := Margens.Direita;
    rlCancelamento.Margins.TopMargin    := Margens.Topo;
    rlCancelamento.Margins.BottomMargin := Margens.Fundo;

    //Detalhes de Dimensionamento LogoTipo
    {$IfNDef NOGUI}
     rlLogo.Visible := LogoVisible and Assigned(ACBrSATExtrato.PictureLogo);
     if rlLogo.Visible then
     begin
       imgLogo.Picture.Assign( ACBrSATExtrato.PictureLogo );
       rlLogo.BandType  := btHeader;
       rlLogo.Top       := 0;
       rlLogo.Height    := LogoHeigth;
       imgLogo.Center   := LogoCenter;
       imgLogo.Width    := LogoWidth;
       imgLogo.Height   := LogoHeigth;
       imgLogo.AutoSize := LogoAutoSize;
       imgLogo.Stretch  := LogoStretch;
       rlLogo.AutoSize  := True;
     end;

     rlLogoCanc.Visible := LogoVisible and Assigned(ACBrSATExtrato.PictureLogo);
     if rlLogoCanc.Visible then
     begin
       imgLogoCanc.Picture.Assign( ACBrSATExtrato.PictureLogo );
       rlLogoCanc.BandType  := btHeader;
       rlLogoCanc.Top       := 0;
       imgLogoCanc.Center   := LogoCenter;
       imgLogoCanc.Width    := LogoWidth;
       imgLogoCanc.Height   := LogoHeigth;
       imgLogoCanc.AutoSize := LogoAutoSize;
       imgLogoCanc.Stretch  := LogoStretch;
       rlLogoCanc.AutoSize  := True;
     end;
    {$EndIf}
  end;
end;

procedure TACBrSATExtratoFortesFr.rlVendaDataRecord(Sender: TObject;
  RecNo: integer; CopyNo: integer; var Eof: boolean;
  var RecordAction: TRLRecordAction);
begin
  Eof := (RecNo > 1);
  RecordAction := raUseIt;
end;

procedure TACBrSATExtratoFortesFr.rlDadosEntregaBeforePrint(Sender: TObject;
  var PrintIt: boolean);
var
  Endereco: String;
begin
  with ACBrSATExtrato.CFe do
  begin
    mEndEnt.Lines.Clear;
    mDestEnt.Lines.Clear;

    PrintIt := Trim(Entrega.xLgr)+
               Trim(Entrega.nro)+
               Trim(Entrega.xCpl)+
               Trim(Entrega.xBairro)+
               Trim(Entrega.xMun) <> '' ;

    if PrintIt then
    begin
      Endereco := Entrega.xLgr ;
      if (Entrega.nro <> '') then
        Endereco := Endereco + ', '+Entrega.nro;
      if (Entrega.xCpl <> '') then
        Endereco := Endereco + ' - '+Entrega.xCpl;
      if (Entrega.xBairro <> '') then
        Endereco := Endereco + ' - '+Entrega.xBairro;
      if (Entrega.xMun <> '') then
        Endereco := Endereco + ' - '+Entrega.xMun;

      mEndEnt.Lines.Add( ACBrStr('Endereço: ')+Endereco );

      if (Dest.xNome <> '' ) then
        mDestEnt.Lines.Add( ACBrStr('Destinatário: ')+Dest.xNome );
    end;
  end;
end;

procedure TACBrSATExtratoFortesFr.rlObsContribBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  with ACBrSATExtrato.CFe do
  begin
    mObsContrib.Lines.Clear;

    PrintIt := (InfAdic.infCpl <> '') or
               (ACBrSATExtrato.ImprimeMsgOlhoNoImposto and (Total.vCFeLei12741 > 0));

    if PrintIt and (InfAdic.infCpl <> '') then
      mObsContrib.Lines.Text := StringReplace(InfAdic.infCpl,';',sLineBreak,[rfReplaceAll]);
  end;
end;

procedure TACBrSATExtratoFortesFr.rlsbDetItemDataRecord(Sender: TObject;
  RecNo: integer; CopyNo: integer; var Eof: boolean;
  var RecordAction: TRLRecordAction);
begin
  fNumItem := RecNo - 1 ;

  Eof := (RecNo > ACBrSATExtrato.CFe.Det.Count) ;
  RecordAction := raUseIt ;
end;

procedure TACBrSATExtratoFortesFr.rlsbObsFiscoDataRecord(Sender: TObject;
  RecNo: integer; CopyNo: integer; var Eof: boolean;
  var RecordAction: TRLRecordAction);
begin
  fNumObs := RecNo - 1 ;

  Eof := (RecNo > fObsFisco.Count) ;
  RecordAction := raUseIt ;
end;

procedure TACBrSATExtratoFortesFr.rlsbPagamentosDataRecord(Sender: TObject;
  RecNo: integer; CopyNo: integer; var Eof: boolean;
  var RecordAction: TRLRecordAction);
begin
  fNumPagto := RecNo - 1 ;

  Eof := (RecNo > ACBrSATExtrato.CFe.Pagto.Count) ;
  RecordAction := raUseIt ;
end;

procedure TACBrSATExtratoFortesFr.PintarQRCode(QRCodeData: String; APict: TPicture);
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

function TACBrSATExtratoFortesFr.CompoemEnderecoCFe: String;
var
  Endereco, CEP: String;
begin
  with ACBrSATExtrato.CFe do
  begin
    // Definindo dados do Cliche //
    Endereco := Emit.EnderEmit.xLgr ;
    if (Emit.EnderEmit.nro <> '') then
      Endereco := Endereco + ', '+Emit.EnderEmit.nro;
    if (Emit.EnderEmit.xCpl <> '') then
      Endereco := Endereco + ' - '+Emit.EnderEmit.xCpl;

    Endereco := Endereco + sLineBreak;
    if (Emit.EnderEmit.xBairro <> '') then
      Endereco := Endereco + Emit.EnderEmit.xBairro;
    if (Emit.EnderEmit.xMun <> '') then
      Endereco := Endereco + ' - '+Emit.EnderEmit.xMun;
    if (Emit.EnderEmit.CEP <> 0) then
    begin
      CEP := FormatarCEP( Emit.EnderEmit.CEP );
      Endereco := Endereco + ' - '+CEP;
    end;
  end;

  Result := Endereco;
end;

function TACBrSATExtratoFortesFr.CompoemCliche: String;
var
  CNPJ_IE_IM: String;
begin
  with ACBrSATExtrato.CFe do
  begin
    CNPJ_IE_IM := 'CNPJ:'+Emit.CNPJ ;
    if (Emit.IE <> '') then
      CNPJ_IE_IM := CNPJ_IE_IM + ' IE:'+Emit.IE;
    if (Emit.IM <> '') then
      CNPJ_IE_IM := CNPJ_IE_IM + ' IM:'+Emit.IM;
  end;

  Result := CNPJ_IE_IM;
end;

function TACBrSATExtratoFortesFr.CalcularCaractesWidth(Canvas: TCanvas;
  WidthTotal: Integer): Integer;
var
  LinhaExemplo : String;
begin
  LinhaExemplo := '*';

  while (Canvas.TextWidth(LinhaExemplo) < WidthTotal) do
    LinhaExemplo := LinhaExemplo + '*';

  Result := Length(LinhaExemplo)-2
end;

procedure TACBrSATExtratoFortesFr.rlVendaBeforePrint(Sender: TObject;
  var PrintIt: boolean);
var
  NumExtrato, qrcode: String;
  I: Integer;
begin
  fNumItem  := 0;
  fNumPagto := 0;
  fNumObs   := 0;
  fObsFisco.Clear;

  with ACBrSATExtrato.CFe do
  begin
    // Copiando as Observações do Fisco para Lista Interna //
    for I :=0 to InfAdic.obsFisco.Count - 1 do
       fObsFisco.Add( InfAdic.obsFisco.Items[I].xCampo + '-' +
                      InfAdic.obsFisco.Items[I].xTexto);

    if Emit.cRegTrib = RTSimplesNacional then
       fObsFisco.Add( Msg_ICMS_123_2006 );

    lNomeFantasia.Lines.Text:= Emit.xFant ;
    lRazaoSocial.Lines.Text := Emit.xNome ;
    lEndereco.Lines.Text    := CompoemEnderecoCFe;
    lEmitCNPJ_IE_IM.Lines.Text := CompoemCliche;

    // Numero do Extrato ou Homologação //
    if (ide.tpAmb = taHomologacao) then
      NumExtrato := '000000'
    else
      NumExtrato := Trim( IntToStr( ide.nCFe ) );

    lNumeroExtrato.Caption := StringReplace(lNumeroExtrato.Caption,'<NUMERO>',NumExtrato,[]);

    // CPF_CNPJ do Consumidor //
    rlbConsumidor.Visible := ((Trim(Dest.CNPJCPF) <> '') or ACBrSATExtrato.ImprimeCPFNaoInformado);
    if rlbConsumidor.Visible then
    begin
      lCPF_CNPJ.Lines.Text := StringReplace(lCPF_CNPJ.Caption,'<CPF_CNPJ>',
                                         IfThen( Trim(Dest.CNPJCPF)<>'',
                                                 FormatarCNPJouCPF(Dest.CNPJCPF),
                                                 ACBrStr('CONSUMIDOR NÃO IDENTIFICADO')),[]);
      lRazaoSocialNome.Visible := (Trim(ACBrSATExtrato.CFe.Dest.xNome) <> '');
      if lRazaoSocialNome.Visible then
         lRazaoSocialNome.Lines.Text := StringReplace(lRazaoSocialNome.Lines.Text,
                                            '<xNome>', Dest.xNome,[]);
    end;

    // Informações do Rodapé do Extrato //
    lNumSAT.Caption   := Trim(IntToStr( ide.nserieSAT ));
    lDataHora.Caption := FormatDateTimeBr(ide.dEmi + ide.hEmi);
    lChaveAcesso.Lines.Text := FormatarChaveAcesso(infCFe.ID);
    bcChaveAcesso1.Caption := copy(infCFe.ID, 1,22);
    bcChaveAcesso2.Caption := copy(infCFe.ID,23,22);

    // QRCode  //
    imgQRCode.Visible := ACBrSATExtrato.ImprimeQRCode;
    if ACBrSATExtrato.ImprimeQRCode then
    begin
      qrcode := ACBrSATExtrato.CalcularConteudoQRCode( infCFe.ID,
                                                       ide.dEmi+ide.hEmi,
                                                       Total.vCFe,
                                                       Trim(Dest.CNPJCPF),
                                                       ide.assinaturaQRCODE );
      PintarQRCode( qrcode, imgQRCode.Picture );
    end;

    mMsgAppQRCode.Lines.Text := ACBrSATExtrato.MsgAppQRCode;

    mSwHouseSite.Lines.Clear;
    if ACBrSATExtrato.SoftwareHouse <> '' then
      mSwHouseSite.Lines.Add(ACBrSATExtrato.SoftwareHouse);

    if ACBrSATExtrato.Site <> '' then
      mSwHouseSite.Lines.Add(ACBrSATExtrato.Site);
  end;
end;

procedure TACBrSATExtratoFortesFr.rlbDetItemBeforePrint(Sender: TObject;
  var PrintIt: boolean);
var
  LinhaItem, sCodigo, sDescricao, sVlrImpostos, mvUnCom: String;
  nTamDescricao, maxCaracter: Integer;
  {$IFNDEF FPC}
    BMP : TBitmap;
  {$ENDIF}
begin
  PrintIt := not Resumido;
  if not PrintIt then exit;

  mLinhaItem.Lines.Clear ;
  {$IFNDEF FPC}
    BMP:=TBitMap.Create;
    try
      BMP.Canvas.Font.Assign(mLinhaItem.Font);
      maxCaracter := CalcularCaractesWidth( BMP.Canvas, mLinhaItem.Width);
    finally
      BMP.Free;
    end;
  {$ELSE}
    maxCaracter := CalcularCaractesWidth(mLinhaItem.Canvas, mLinhaItem.Width);
  {$ENDIF}

  with ACBrSATExtrato.CFe.Det.Items[fNumItem] do
  begin
    lSequencia.Caption := IntToStrZero(nItem,3);
    lTotalItem.Caption := FormatFloatBr(Prod.vProd);
    mvUnCom := IfThen(Prod.EhCombustivel, ',0.000', ACBrSATExtrato.Mask_vUnCom);

    if (Length( Trim( Prod.cEAN ) ) > 0) and (ACBrSATExtrato.UsaCodigoEanImpressao) then
      sCodigo := Trim(Prod.cEAN)
    else
      sCodigo := Trim(Prod.cProd);

    if Imposto.vItem12741 > 0 then
      sVlrImpostos := ' ('+FormatFloatBr(Imposto.vItem12741)+') '
    else
      sVlrImpostos := '';

    if ACBrSATExtrato.ImprimeEmUmaLinha then
    begin
      LinhaItem := sCodigo + ' [DesProd] ' +
                   ACBrSATExtrato.FormatQuantidade(Prod.qCom, False) + ' ' +
                   Trim( Prod.uCom) + ' X ' +
                   FormatFloatBr(Prod.vUnCom, mvUnCom) +
                   sVlrImpostos;

      // acerta tamanho da descrição
      nTamDescricao := maxCaracter - Length(LinhaItem);
      sDescricao := PadRight(Copy(Trim(Prod.xProd), 1, nTamDescricao), nTamDescricao);

      LinhaItem := StringReplace(LinhaItem, '[DesProd]', sDescricao, [rfReplaceAll]);

      mLinhaItem.Lines.Add(LinhaItem);
    end
    else
    begin
      LinhaItem := sCodigo + ' ' + Trim(Prod.xProd);
      if Trim(infAdProd) <> '' then
        LinhaItem := LinhaItem + '-'+ StringReplace( infAdProd, ';', sLineBreak, [rfReplaceAll]);

      mLinhaItem.Lines.Add(LinhaItem);

      sVlrImpostos := sVlrImpostos + '|';
      //Centraliza os valores. A fonte dos itens foi mudada para Courier New, Pois esta o espaço tem o mesmo tamanho dos demais caractere.
      LinhaItem  := ACBrSATExtrato.FormatQuantidade(Prod.qCom, False) +'|'+
                    Trim(Prod.uCom) + ' X ' +
                    FormatFloatBr(Prod.vUnCom, mvUnCom) +'|'+
                    sVlrImpostos + '|';
      LinhaItem  := PadSpace(LinhaItem, maxCaracter, '|') ;
      mLinhaItem.Lines.Add(LinhaItem);
    end;
  end;
end;

procedure TACBrSATExtratoFortesFr.rlbDescItemBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  with ACBrSATExtrato.CFe.Det.Items[fNumItem] do
  begin
    PrintIt := (not Resumido) and (Prod.vDesc > 0) and ACBrSATExtrato.ImprimeDescAcrescItem ;

    if PrintIt then
    begin
      lDesconto.Caption   := FormatFloatBr(Prod.vDesc,'-,0.00');
      lDescValLiq.Caption := FormatFloatBr(Prod.vProd - Prod.vDesc);
    end;
  end;
end;

procedure TACBrSATExtratoFortesFr.rlbOutroItemBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  with ACBrSATExtrato.CFe.Det.Items[fNumItem] do
  begin
    PrintIt := (not Resumido) and (Prod.vOutro > 0) and ACBrSATExtrato.ImprimeDescAcrescItem ;

    if PrintIt then
    begin
      lOutro.Caption       := FormatFloatBr(Prod.vOutro,'+,0.00');
      lOutroValLiq.Caption := FormatFloatBr(Prod.vProd + Prod.vOutro - Prod.vDesc);
    end;
  end;

end;

procedure TACBrSATExtratoFortesFr.rlbPagamentoBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  with ACBrSATExtrato.CFe.Pagto.Items[fNumPagto] do
  begin
    lMeioPagamento.Caption := ACBrStr(CodigoMPToDescricao(cMP));
    lPagamento.Caption     := FormatFloatBr(vMP);
  end;
end;

procedure TACBrSATExtratoFortesFr.rlbGapBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  PrintIt := not Resumido;
end;

procedure TACBrSATExtratoFortesFr.rlbDeducISSQNBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  with ACBrSATExtrato.CFe.Det.Items[fNumItem] do
  begin
    PrintIt := (not Resumido) and (Imposto.ISSQN.vDeducISSQN > 0);

    if PrintIt then
    begin
      lDeducISSQN.Caption    := FormatFloatBr(Imposto.ISSQN.vDeducISSQN,'-,0.00');
      lBaseCalcISSQN.Caption := FormatFloatBr(Imposto.ISSQN.vBC);
    end;
  end;
end;

procedure TACBrSATExtratoFortesFr.rlbSubTotalBeforePrint(Sender: TObject;
  var PrintIt: boolean);
var
  Descontos, Acrescimos: Double;
begin
  with ACBrSATExtrato.CFe do
  begin
    Descontos  := (Total.ICMSTot.vDesc  + Total.DescAcrEntr.vDescSubtot);
    Acrescimos := (Total.ICMSTot.vOutro + Total.DescAcrEntr.vAcresSubtot);

    PrintIt := (not Resumido) and ((Descontos > 0) or (Acrescimos > 0));

    if PrintIt then
      lSubTotal.Caption := FormatFloatBr(Total.ICMSTot.vProd);
  end;
end;

procedure TACBrSATExtratoFortesFr.rlbDescontosBeforePrint(Sender: TObject;
  var PrintIt: boolean);
var
  Descontos: Double;
begin
  with ACBrSATExtrato.CFe do
  begin
    Descontos := (Total.ICMSTot.vDesc  + Total.DescAcrEntr.vDescSubtot);
    PrintIt   := (not Resumido) and (Descontos > 0);

    if PrintIt then
      lTotDescontos.Caption := FormatFloatBr(Descontos);
  end;
end;

procedure TACBrSATExtratoFortesFr.rlbAcrescimosBeforePrint(Sender: TObject;
  var PrintIt: boolean);
var
  Acrescimos: Double;
begin
  with ACBrSATExtrato.CFe do
  begin
    Acrescimos := (Total.ICMSTot.vOutro + Total.DescAcrEntr.vAcresSubtot);
    PrintIt    := (not Resumido) and (Acrescimos > 0);

    if PrintIt then
      lTotAcrescimos.Caption := FormatFloatBr(Acrescimos);
  end;
end;

procedure TACBrSATExtratoFortesFr.rlbLegendaBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  PrintIt := not Resumido;
end;

procedure TACBrSATExtratoFortesFr.FormDestroy(Sender: TObject);
begin
  fObsFisco.Free;
end;

procedure TACBrSATExtratoFortesFr.lCPF_CNPJBeforePrint(Sender: TObject;
  var OutputText: string; var PrintIt: boolean);
begin
  PrintIt := (ACBrSATExtrato.CFe.Dest.CNPJCPF <> '') or ACBrSATExtrato.ImprimeCPFNaoInformado ;
end;

procedure TACBrSATExtratoFortesFr.lRazaoSocialNomeBeforePrint(Sender: TObject;
  var Text: string; var PrintIt: Boolean);
begin
  PrintIt := (Trim(ACBrSATExtrato.CFe.Dest.xNome) <> '') ;
end;

procedure TACBrSATExtratoFortesFr.pAsteriscoBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  PrintIt := not Resumido;
end;

procedure TACBrSATExtratoFortesFr.pLei12741BeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  with ACBrSATExtrato.CFe do
  begin
    PrintIt := (Total.vCFeLei12741 > 0); // and (fImprimeMsgOlhoNoImposto);

    if PrintIt then
      lValLei12741.Caption := FormatFloatBr(Total.vCFeLei12741);
  end;
end;

procedure TACBrSATExtratoFortesFr.pNumSATBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  lTitSAT.Width := Trunc(pNumSAT.Width / 2);
end;

procedure TACBrSATExtratoFortesFr.pNumSATCanBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  lTitSATCan.Width := Trunc(pNumSATCan.Width / 2);
end;

procedure TACBrSATExtratoFortesFr.pNumSATCanclBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  lTitSATCanl.Width := Trunc(pNumSATCancl.Width / 2);
end;

procedure TACBrSATExtratoFortesFr.pTotalCancBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  lTitTotalCan.Width := Trunc(pTotalCanc.Width / 2);
end;

procedure TACBrSATExtratoFortesFr.rlbConsumidorBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  PrintIt := ((Trim(ACBrSATExtrato.CFe.Dest.CNPJCPF) <> '') or ACBrSATExtrato.ImprimeCPFNaoInformado);
end;

procedure TACBrSATExtratoFortesFr.rlbsCabecalhoDataRecord(Sender: TObject;
  RecNo: integer; CopyNo: integer; var Eof: boolean;
  var RecordAction: TRLRecordAction);
begin
  Eof := (RecNo > 1);
  RecordAction := raUseIt;
end;

procedure TACBrSATExtratoFortesFr.rlbTesteBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  PrintIt := (ACBrSATExtrato.CFe.ide.tpAmb = taHomologacao);
end;

procedure TACBrSATExtratoFortesFr.rlbTotalBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  lTotal.Caption := FormatFloatBr(ACBrSATExtrato.CFe.Total.vCFe);
end;

procedure TACBrSATExtratoFortesFr.rlbTrocoBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  with ACBrSATExtrato.CFe do
  begin
    PrintIt := (Pagto.vTroco > 0);

    if PrintIt then
      lTroco.Caption := FormatFloatBr(Pagto.vTroco);;
  end;
end;

procedure TACBrSATExtratoFortesFr.rlCancelamentoBeforePrint(Sender: TObject;
  var PrintIt: boolean);
var
  NumExtrato, qrcode: String;
begin
  with ACBrSATExtrato.CFe do
  begin
    lNomeFantasiaCan.Lines.Text:= Emit.xFant ;
    lRazaoSocialCan.Lines.Text := Emit.xNome ;
    lEnderecoCan.Lines.Text    := CompoemEnderecoCFe;
    lEmitCNPJ_IE_IMCan.Lines.Text := CompoemCliche;

    // Numero do Extrato ou Homologação //
    if (ide.tpAmb = taHomologacao) then
      NumExtrato := '000000'
    else
      NumExtrato := Trim( IntToStr( ACBrSATExtrato.CFeCanc.ide.nCFe ) );

    // CPF_CNPJ do Consumidor //
    lCPF_CNPJCan.Lines.Text := StringReplace(lCPF_CNPJ.Caption,'<CPF_CNPJ>',
                                       FormatarCNPJouCPF(Dest.CNPJCPF),[]);
    lRazaoSocialNomeCanc.Lines.Text := StringReplace(lRazaoSocialNomeCanc.Lines.Text,
                                       '<xNome>', Dest.xNome,[]);
    lTotalCan.Caption := FormatFloatBr(Total.vCFe);

    // Informações do Rodapé do Extrato //
    lNumSATCan.Caption   := Trim(IntToStr( ide.nserieSAT ));
    lDataHoraCan.Caption := FormatDateTimeBr(ide.dEmi + ide.hEmi);
    lChaveAcessoCan.Lines.Text := FormatarChaveAcesso(infCFe.ID);
    bcChaveAcessoCan1.Caption := copy( infCFe.ID, 1,22);
    bcChaveAcessoCan2.Caption := copy( infCFe.ID,23,22);

    // QRCode  //
    imgQRCodeCan.Visible := ACBrSATExtrato.ImprimeQRCode;
    if ACBrSATExtrato.ImprimeQRCode then
    begin
      qrcode := ACBrSATExtrato.CalcularConteudoQRCode( infCFe.ID,
                                                       ide.dEmi+ide.hEmi,
                                                       Total.vCFe,
                                                       Trim(Dest.CNPJCPF),
                                                       ide.assinaturaQRCODE );
      PintarQRCode( qrcode, imgQRCodeCan.Picture );
    end;
  end;

  with ACBrSATExtrato.CFeCanc do
  begin
    lNumeroExtratoCan.Caption := StringReplace(lNumeroExtratoCan.Caption,'<NUMERO>',NumExtrato,[]);
    // Informações do Rodapé do Extrato //
    lNumSATCanl.Caption   := Trim(IntToStr( ide.nserieSAT ));
    lDataHoraCanl.Caption := FormatDateTimeBr(ide.dEmi + ide.hEmi);
    lChaveAcessoCanl.Lines.Text := FormatarChaveAcesso(infCFe.ID);
    bcChaveAcessoCanl1.Caption := copy( infCFe.ID, 1,22);
    bcChaveAcessoCanl2.Caption := copy( infCFe.ID,23,22);

    // QRCode  //
    imgQRCodeCanl.Visible := ACBrSATExtrato.ImprimeQRCode;
    if ACBrSATExtrato.ImprimeQRCode then
    begin
      qrcode := ACBrSATExtrato.CalcularConteudoQRCode( infCFe.ID,
                                                       ide.dEmi+ide.hEmi,
                                                       Total.vCFe,
                                                       Trim(Dest.CNPJCPF),
                                                       ide.assinaturaQRCODE );
      PintarQRCode( qrcode, imgQRCodeCanl.Picture );
    end;
  end;

  mMsgAppQRCodeCanc.Lines.Text := ACBrSATExtrato.MsgAppQRCode;

  mSwHouseSiteCanc.Lines.Clear;
  if ACBrSATExtrato.SoftwareHouse <> '' then
    mSwHouseSiteCanc.Lines.Add(ACBrSATExtrato.SoftwareHouse);

  if ACBrSATExtrato.Site <> '' then
    mSwHouseSiteCanc.Lines.Add(ACBrSATExtrato.Site);
end;

procedure TACBrSATExtratoFortesFr.rlbObsFiscoBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  mObsFisco.Lines.Text := fObsFisco[ fNumObs ];
end;

{ TACBrSATExtratoFortes }

procedure TACBrSATExtratoFortes.Imprimir;
var
  frACBrSATExtratoFortesFr: TACBrSATExtratoFortesFr;
  RLLayout: TRLReport;
  RLFiltro: TRLCustomSaveFilter;
begin
  frACBrSATExtratoFortesFr := TACBrSATExtratoFortesFr.Create(Self);
  try
    with frACBrSATExtratoFortesFr do
    begin
      if LayOut = lCancelamento then
      begin
         RLLayout := rlCancelamento;
         RLLayout.Title := 'CFeCan: '+FormatarChaveAcesso(CFeCanc.infCFe.ID);
      end
      else
      begin
        RLLayout := rlVenda;
        RLLayout.Title := 'CFe: '+FormatarChaveAcesso(CFe.infCFe.ID);
        Resumido := (LayOut = lResumido);
      end;

      RLPrinter.Copies     := NumCopias ;
      RLLayout.PrintDialog := MostrarSetup;
      RLLayout.ShowProgress:= False ;

      if (Filtro = fiNenhum) and (PrinterName <> '') then
        RLPrinter.PrinterName := PrinterName;

      //Para impressoras sem guilhotina não cortar no QrCorde
      pEspacoFinal.Height := EspacoFinal;
      pEspacoFinalCan.Height  := EspacoFinal;

      // Largura e Margens do Relatório //
      RLLayout.Width := LarguraBobina;
      RLLayout.Margins.LeftMargin   := Margens.Esquerda;
      RLLayout.Margins.RightMargin  := Margens.Direita;
      RLLayout.Margins.TopMargin    := Margens.Topo;
      RLLayout.Margins.BottomMargin := Margens.Fundo;

      // Ajustando o tamanho da página //
      RLLayout.PageBreaking := pbNone;
      RLLayout.PageSetup.PaperSize   := fpCustom ;
      RLLayout.PageSetup.PaperWidth  := Round(LarguraBobina/MMAsPixels) ;

      RLLayout.UnlimitedHeight := True; // ****** ATENÇÃO ******
      // Se você recebeu um erro de compilação na linha ACIMA
      // Voce DEVE atualizar os fontes do seu Fortes Report CE
      // https://github.com/fortesinformatica/fortesreport-ce

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
            fiPDF  :
              RLFiltro := RLPDFFilter1;
            fiHTML :
              RLFiltro := RLHTMLFilter1;
          else
            exit ;
          end ;

          RLFiltro.ShowProgress := RLLayout.ShowProgress;
          RLFiltro.FileName := NomeArquivo ;
          RLFiltro.FilterPages( RLLayout.Pages );
        end;
      end;
    end;
  finally
    frACBrSATExtratoFortesFr.Free ;
  end;
end;

procedure TACBrSATExtratoFortes.ImprimirExtrato(ACFe: TCFe);
begin
  inherited;
  Imprimir;
end;

procedure TACBrSATExtratoFortes.ImprimirExtratoCancelamento(ACFe: TCFe;
  ACFeCanc: TCFeCanc);
begin
  inherited;
  Imprimir;
end;

procedure TACBrSATExtratoFortes.ImprimirExtratoResumido(ACFe: TCFe);
begin
  inherited;
  Imprimir;
end;

{$ifdef FPC}
initialization
   {$I ACBrSATExtratoFortes.lrs}
{$endif}

end.
