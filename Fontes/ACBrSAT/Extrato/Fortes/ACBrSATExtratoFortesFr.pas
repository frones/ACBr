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
    imgLogo: TRLImage;
    imgLogoCanc: TRLImage;
    imgQRCode: TRLImage;
    imgQRCodeCan: TRLImage;
    imgQRCodeCanl: TRLImage;
    lAcrescItem: TRLLabel;
    lBaseCalcISSQN: TRLLabel;
    lCabItem: TRLMemo;
    lChaveAcesso: TRLMemo;
    lChaveAcessoCan: TRLMemo;
    lChaveAcessoCanl: TRLMemo;
    lCPF_CNPJ: TRLMemo;
    lCPF_CNPJCan: TRLMemo;
    lCupomFiscalEletronicoCan: TRLMemo;
    lDataHoraLateral: TRLLabel;
    lDataHoraCan: TRLLabel;
    lDataHoraCanl: TRLLabel;
    lDeducISSQN: TRLLabel;
    lDescItem: TRLLabel;
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
    lNumeroExtrato: TRLMemo;
    lNumeroExtratoCan: TRLLabel;
    lNumSATLateral: TRLLabel;
    lNumSATCan: TRLLabel;
    lNumSATCanl: TRLLabel;
    lRatAcresSubTot: TRLLabel;
    lRatDescSubTot: TRLLabel;
    lRazaoSocial: TRLMemo;
    lRazaoSocialCan: TRLMemo;
    lRazaoSocialNome: TRLMemo;
    lRazaoSocialNomeCanc: TRLMemo;
    lTeste: TRLLabel;
    lTitLei12744Lateral: TRLMemo;
    lTitSATLateral: TRLLabel;
    lTotDescAcresItem: TRLLabel;
    lTesteCan: TRLMemo;
    lTitBaseCalcISSQN: TRLLabel;
    lTitCancelamento: TRLLabel;
    lTitCancelamento1: TRLMemo;
    lTitCancelamento2: TRLMemo;
    lTitDeducISSQN: TRLLabel;
    lTitLei12741: TRLMemo;
    lTitLei12743: TRLMemo;
    lTitObsContrib: TRLMemo;
    lTitRatAcresSubtot: TRLLabel;
    lTitRatDescSubtot: TRLLabel;
    lTitSATCan: TRLLabel;
    lTitSATCanl: TRLLabel;
    lTitTotDescAcresItem: TRLLabel;
    lTitTotalCan: TRLLabel;
    lTotalCan: TRLLabel;
    lValLei12741: TRLLabel;
    lPagamento: TRLLabel;
    lMeioPagamento: TRLLabel;
    lAcresSubTot: TRLLabel;
    lTitTotal: TRLLabel;
    lTitTroco: TRLLabel;
    lTitTotDescontos: TRLLabel;
    lTotAcrescimos: TRLLabel;
    lTotalBruto: TRLLabel;
    lNumSAT: TRLLabel;
    lTitSAT: TRLLabel;
    bcChaveAcesso1: TRLBarcode;
    lSequencia: TRLLabel;
    lTitTotalBruto: TRLLabel;
    lTitDesItem: TRLLabel;
    lTitAcrescItem: TRLLabel;
    lTotal: TRLLabel;
    lTroco: TRLLabel;
    lTotalItem: TRLLabel;
    lDescSubTot: TRLLabel;
    mConsumidorLateral: TRLMemo;
    mEndEnt: TRLMemo;
    mMsgAppQRCode: TRLMemo;
    mMsgAppQRCodeLateral: TRLMemo;
    mSwHouseSite: TRLMemo;
    mMsgAppQRCodeCanc: TRLMemo;
    mObsContrib: TRLMemo;
    mLinhaItem: TRLMemo;
    mSwHouseSiteCanc: TRLMemo;
    pAsterisco: TRLPanel;
    pGap1: TRLPanel;
    pGap10: TRLPanel;
    pGap11: TRLPanel;
    pGap9: TRLPanel;
    pGapObs: TRLPanel;
    pNumSATLateral: TRLPanel;
    pNumSATDataHoraLateral: TRLPanel;
    pGap6: TRLPanel;
    pGap7: TRLPanel;
    pEspacoFinalCan: TRLPanel;
    pGap8: TRLPanel;
    pNumSATCan: TRLPanel;
    pNumSATCancl: TRLPanel;
    rlbDadosCupomCancelado: TRLBand;
    rlbCabecalhoCan: TRLBand;
    rlbConsumidor: TRLBand;
    rlbDadosCliche: TRLBand;
    rlbGap1: TRLBand;
    rlbRatAcresSubTot: TRLBand;
    rlbGapDescAcres: TRLBand;
    rlbRatDescSubTot: TRLBand;
    rlbGapTotItens: TRLBand;
    rlbLegenda: TRLBand;
    rlbNumExtrato: TRLBand;
    rlbCanRodape: TRLBand;
    rlbSubDescAcresItem: TRLBand;
    rlbTeste: TRLBand;
    rlbTesteCan: TRLBand;
    rlDadosEntrega: TRLBand;
    rlbPagamento: TRLBand;
    rlbObsFisco: TRLBand;
    rlbDescontos: TRLBand;
    rlbAcrescimos: TRLBand;
    rlbTotal: TRLBand;
    rlbTotalBruto: TRLBand;
    rlbDescItem: TRLBand;
    rlbAcresItem: TRLBand;
    rlbDetItem: TRLBand;
    rlbDeducISSQN: TRLBand;
    rlbRodape: TRLBand;
    rlbTroco: TRLBand;
    RLDraw10: TRLDraw;
    RLDraw11: TRLDraw;
    RLDraw12: TRLDraw;
    RLDraw13: TRLDraw;
    RLDraw3: TRLDraw;
    rlbsCabecalho: TRLSubDetail;
    RLDraw4: TRLDraw;
    RLDraw5: TRLDraw;
    RLDraw6: TRLDraw;
    RLDraw8: TRLDraw;
    RLDraw9: TRLDraw;
    lTitConsumidorLateral: TRLLabel;
    RLPanel1: TRLPanel;
    pSATSerieHora: TRLPanel;
    pQRCode: TRLPanel;
    pTextoLateral: TRLPanel;
    pConsumidorLateral: TRLPanel;
    pGap: TRLPanel;
    paLogoCanc: TRLPanel;
    RLPanel3: TRLPanel;
    pTotalCanc: TRLPanel;
    pEspacoFinal: TRLPanel;
    paLogoECliche: TRLPanel;
    paLogo: TRLPanel;
    paCliche: TRLPanel;
    rlVenda: TRLReport;
    rlObsContrib: TRLBand;
    RLHTMLFilter1: TRLHTMLFilter;
    lDataHora: TRLLabel;
    pGap05: TRLPanel;
    mObsFisco: TRLMemo;
    pLei12741: TRLPanel;
    pNumSAT: TRLPanel;
    RLPDFFilter1: TRLPDFFilter;
    rlsbDetItem: TRLSubDetail;
    rlsbPagamentos: TRLSubDetail;
    rlsbObsFisco: TRLSubDetail;
    rlCancelamento: TRLReport;

    procedure FormDestroy(Sender: TObject);
    procedure pAsteriscoBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure pConsumidorLateralBeforePrint(Sender: TObject;
      var PrintIt: Boolean);
    procedure pLei12741BeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure pNumSATDataHoraLateralBeforePrint(Sender: TObject;
      var PrintIt: Boolean);
    procedure pNumSATLateralBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure pNumSATBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure pNumSATCanBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure pNumSATCanclBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure pQRCodeBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure pSATSerieHoraBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure pTotalCancBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlbConsumidorBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlbGapDescAcresBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlbRatAcresSubTotBeforePrint(Sender: TObject; var PrintIt: Boolean
      );
    procedure rlbRatDescSubTotBeforePrint(Sender: TObject; var PrintIt: Boolean
      );
    procedure rlbsCabecalhoDataRecord(Sender: TObject; RecNo: integer;
      CopyNo: integer; var Eof: boolean; var RecordAction: TRLRecordAction);
    procedure rlbSubDescAcresItemBeforePrint(Sender: TObject;
      var PrintIt: Boolean);
    procedure rlbTesteBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlbAcrescimosBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlbLegendaBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlbDescItemBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlbDescontosBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlbDetItemBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlbDeducISSQNBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlbObsFiscoBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlbAcresItemBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlbPagamentoBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlbGapTotItensBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlbTotalBrutoBeforePrint(Sender: TObject; var PrintIt: boolean);
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

    procedure PintarQRCode(const QRCodeData: String; APict: TPicture);
    function CompoemEnderecoCFe: String ;
    function CompoemCliche: String;

    function CalcularCaractesWidth( Canvas : TCanvas; WidthTotal : Integer ): Integer;
    procedure DiminuirFonteSeNecessario( ARLMemo: TRLMemo; TamanhoMinimo: Integer = 1);
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
var
  TemLogo: Boolean;
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
    TemLogo := LogoVisible;
    if TemLogo then
    begin
      TemLogo := Assigned(ACBrSATExtrato.PictureLogo) and
                 ((ACBrSATExtrato.PictureLogo.Height + ACBrSATExtrato.PictureLogo.Width) > 0);

      if (not TemLogo) and (Logo <> '') and FileExists(Logo) then
      begin
        try
          ACBrSATExtrato.PictureLogo.LoadFromFile(Logo);
          TemLogo := True;
        except
        end;
      end;
    end;

    rlVenda.Width := LarguraBobina;
    rlVenda.Margins.LeftMargin   := MargemEsquerda;
    rlVenda.Margins.RightMargin  := MargemDireita;
    rlVenda.Margins.TopMargin    := MargemSuperior;
    rlVenda.Margins.BottomMargin := MargemInferior;

    rlCancelamento.Width := LarguraBobina;
    rlCancelamento.Margins.LeftMargin   := MargemEsquerda;
    rlCancelamento.Margins.RightMargin  := MargemDireita;
    rlCancelamento.Margins.TopMargin    := MargemSuperior;
    rlCancelamento.Margins.BottomMargin := MargemInferior;

    //Detalhes de Dimensionamento LogoTipo
    {$IfNDef NOGUI}
     paLogo.Visible := TemLogo;

     if paLogo.Visible then
     begin
       imgLogo.Picture.Assign( ACBrSATExtrato.PictureLogo );

       imgLogo.AutoSize := LogoAutoSize;
       imgLogo.Stretch  := LogoStretch;
       imgLogo.Center   := LogoCenter;

       if fACBrSATExtrato.ImprimeLogoLateral then
       begin
         paCliche.Align := faClientTop;
         paLogo.Align   := faLeftTop;
         paLogo.Width   := LogoWidth;
         paLogo.Height  := LogoHeigth;
         imgLogo.Align  := faClient;
       end
       else
       begin
         paLogo.Align   := faTop;
         paLogo.Top     := 0;  // Força ir para o Topo
         paCliche.Align := faTop;
         imgLogo.Width  := LogoWidth;
         imgLogo.Height := LogoHeigth;
         imgLogo.Align  := faClientTop;
       end;
     end;

     paLogoCanc.Visible := TemLogo;

     if paLogoCanc.Visible then
     begin
       paLogoCanc.Align := faTop;
       paLogoCanc.Top   := 0;  // Força ir para o Topo
       imgLogoCanc.Picture.Assign( ACBrSATExtrato.PictureLogo );
       imgLogoCanc.AutoSize := LogoAutoSize;
       imgLogoCanc.Stretch  := LogoStretch;
       imgLogoCanc.Center   := LogoCenter;
       imgLogoCanc.Width    := LogoWidth;
       imgLogoCanc.Height   := LogoHeigth;
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

    PrintIt := (Trim(Entrega.xLgr)+
                Trim(Entrega.nro)+
                Trim(Entrega.xCpl)+
                Trim(Entrega.xBairro)+
                Trim(Entrega.xMun) <> '') ;

    if PrintIt then
    begin
      Endereco := Entrega.xLgr ;
      if (Entrega.nro <> '') then
        Endereco := Endereco + ', '+Entrega.nro;
      if (Entrega.xCpl <> '') then
        Endereco := Endereco + ' '+Entrega.xCpl;
      if (Entrega.xBairro <> '') then
        Endereco := Endereco + ' '+Entrega.xBairro;
      if (Entrega.xMun <> '') then
        Endereco := Endereco + ' '+Entrega.xMun+'-'+Entrega.UF;

      mEndEnt.Lines.Add( ACBrStr('ENDEREÇO DE ENTREGA: ')+Endereco );

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

    lTitObsContrib.Top := 0;
    pGapObs.Top := 0;
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

procedure TACBrSATExtratoFortesFr.PintarQRCode(const QRCodeData: String; APict: TPicture);
var
  QRCode: TDelphiZXingQRCode;
  QRCodeBitmap: TBitmap;
  Row, Column: Integer;
begin
  QRCode       := TDelphiZXingQRCode.Create;
  QRCodeBitmap := TBitmap.Create;
  try
    QRCode.Encoding  := qrUTF8BOM;
    QRCode.QuietZone := 1;
    QRCode.CorrectionLevel := qrclL;
    QRCode.Data      := WideString(QRCodeData);

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

procedure TACBrSATExtratoFortesFr.DiminuirFonteSeNecessario(ARLMemo: TRLMemo;
  TamanhoMinimo: Integer);
var
  ABmp: TBitmap;
begin
  ABmp := TBitmap.Create;
  try
    ABmp.Canvas.Font.Assign(ARLMemo.Font);
    TamanhoMinimo := max(1, TamanhoMinimo);

    while ABmp.Canvas.Font.Size > TamanhoMinimo do
    begin
      if ABmp.Canvas.TextWidth( ARLMemo.Lines.Text ) <= ARLMemo.ClientWidth then
        Break;

      ABmp.Canvas.Font.Size := ABmp.Canvas.Font.Size - 1;
    end;
  finally
    ARLMemo.Font.Size := ABmp.Canvas.Font.Size;
    ABmp.Free;
  end;
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

    if (Emit.cRegTrib = RTSimplesNacional) then
      fObsFisco.Add( Msg_ICMS_123_2006 );

    // Copiando as Observações do Fisco para Lista Interna //
    for I :=0 to InfAdic.obsFisco.Count - 1 do
      fObsFisco.Add( InfAdic.obsFisco.Items[I].xCampo + '-' +
                     InfAdic.obsFisco.Items[I].xTexto);

    lNomeFantasia.Lines.Text:= Emit.xFant ;
    lRazaoSocial.Lines.Text := Emit.xNome ;
    lEndereco.Lines.Text    := CompoemEnderecoCFe;
    lEmitCNPJ_IE_IM.Lines.Text := CompoemCliche;
    DiminuirFonteSeNecessario(lEmitCNPJ_IE_IM, 6);

    // Numero do Extrato ou Homologação //
    if (ide.tpAmb = taHomologacao) then
      NumExtrato := '000000'
    else
      NumExtrato := IntToStrZero(ide.nCFe, 6);

    lNumeroExtrato.Lines.Text := StringReplace(lNumeroExtrato.Lines.Text,'<NUMERO>',NumExtrato,[]);
    DiminuirFonteSeNecessario(lNumeroExtrato, 6);

    pSATSerieHora.Visible := not ACBrSATExtrato.ImprimeQRCodeLateral;
    lTitLei12743.Visible := not ACBrSATExtrato.ImprimeQRCodeLateral;
    mMsgAppQRCode.Visible := not ACBrSATExtrato.ImprimeQRCodeLateral;
    if mMsgAppQRCode.Visible then
      mMsgAppQRCode.Lines.Text := ACBrSATExtrato.MsgAppQRCode;

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

    mSwHouseSite.Lines.Clear;
    if ACBrSATExtrato.Sistema <> '' then
      mSwHouseSite.Lines.Add(ACBrSATExtrato.Sistema);

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
    mvUnCom := IfThen(Prod.EhCombustivel, ',0.000', ACBrSATExtrato.CasasDecimais.MaskvUnCom);

    if (Length( Trim( Prod.cEAN ) ) > 0) and (ACBrSATExtrato.ImprimeCodigoEan) then
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
                   ACBrSATExtrato.FormatarQuantidade(Prod.qCom, False) + ' ' +
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
      LinhaItem  := ACBrSATExtrato.FormatarQuantidade(Prod.qCom, False) +'|'+
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
    PrintIt := ACBrSATExtrato.ImprimeDescAcrescItem and (not Resumido) and (Prod.vDesc > 0);

    if PrintIt then
      lDescItem.Caption := FormatFloatBr(Prod.vDesc,'-,0.00');
  end;
end;

procedure TACBrSATExtratoFortesFr.rlbRatDescSubTotBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  with ACBrSATExtrato.CFe.Det.Items[fNumItem] do
  begin
    PrintIt := ACBrSATExtrato.ImprimeDescAcrescItem and (not Resumido) and (Prod.vRatDesc > 0);

    if PrintIt then
      lRatDescSubTot.Caption := FormatFloatBr(Prod.vRatDesc,'-,0.00');
  end;
end;

procedure TACBrSATExtratoFortesFr.rlbAcresItemBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  with ACBrSATExtrato.CFe.Det.Items[fNumItem] do
  begin
    PrintIt := ACBrSATExtrato.ImprimeDescAcrescItem and (not Resumido) and (Prod.vOutro > 0);

    if PrintIt then
      lAcrescItem.Caption := FormatFloatBr(Prod.vOutro,'+,0.00');
  end;
end;

procedure TACBrSATExtratoFortesFr.rlbRatAcresSubTotBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  with ACBrSATExtrato.CFe.Det.Items[fNumItem] do
  begin
    PrintIt := ACBrSATExtrato.ImprimeDescAcrescItem and (not Resumido) and (Prod.vRatAcr > 0);

    if PrintIt then
      lRatAcresSubTot.Caption := FormatFloatBr(Prod.vRatAcr,'+,0.00');
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

procedure TACBrSATExtratoFortesFr.rlbGapTotItensBeforePrint(Sender: TObject;
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

procedure TACBrSATExtratoFortesFr.rlbTotalBrutoBeforePrint(Sender: TObject;
  var PrintIt: boolean);
var
  TotalDescAcresItem: Currency;
begin
  with ACBrSATExtrato.CFe do
  begin
    TotalDescAcresItem := Total.ICMSTot.vOutro - Total.ICMSTot.vDesc;

    PrintIt :=  (TotalDescAcresItem <> 0) or
                (Total.DescAcrEntr.vDescSubtot <> 0) or
                (Total.DescAcrEntr.vAcresSubtot <> 0);

    if PrintIt then
      lTotalBruto.Caption := FormatFloatBr(Total.ICMSTot.vProd);
  end;
end;

procedure TACBrSATExtratoFortesFr.rlbSubDescAcresItemBeforePrint(
  Sender: TObject; var PrintIt: Boolean);
var
  TotalDescAcresItem: Currency;
  Sinal: String;
begin
  with ACBrSATExtrato.CFe do
  begin
    TotalDescAcresItem := Total.ICMSTot.vOutro - Total.ICMSTot.vDesc;

    PrintIt := (TotalDescAcresItem <> 0);
    if PrintIt then
    begin
      Sinal := IfThen(TotalDescAcresItem < 0,'-','+');
      lTotDescAcresItem.Caption := FormatFloatBr(TotalDescAcresItem, Sinal+',0.00');
    end;
  end;
end;


procedure TACBrSATExtratoFortesFr.rlbDescontosBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  with ACBrSATExtrato.CFe do
  begin
    PrintIt := (Total.DescAcrEntr.vDescSubtot > 0);

    if PrintIt then
      lDescSubTot.Caption := FormatFloatBr(Total.DescAcrEntr.vDescSubtot, '-,0.00');
  end;
end;

procedure TACBrSATExtratoFortesFr.rlbAcrescimosBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  with ACBrSATExtrato.CFe do
  begin
    PrintIt := (Total.DescAcrEntr.vAcresSubtot > 0);

    if PrintIt then
      lTotAcrescimos.Caption := FormatFloatBr(Total.DescAcrEntr.vAcresSubtot, '+,0.00');
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

procedure TACBrSATExtratoFortesFr.pAsteriscoBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  PrintIt := not Resumido;
end;

procedure TACBrSATExtratoFortesFr.pConsumidorLateralBeforePrint(
  Sender: TObject; var PrintIt: Boolean);
var
  NomeConsumidor: String;
begin
  with ACBrSATExtrato.CFe do
  begin
    if (Trim(Dest.xNome) <> '') then
      NomeConsumidor := Trim(Dest.xNome)
    else if (ACBrSATExtrato.ImprimeCPFNaoInformado and (Trim(Dest.CNPJCPF) = '')) then
      NomeConsumidor := ACBrStr('CONSUMIDOR NÃO IDENTIFICADO')
    else
      NomeConsumidor := '';

    if (Trim(Dest.CNPJCPF) <> '') then
      NomeConsumidor := FormatarCNPJouCPF(Dest.CNPJCPF) +
                        IfThen(NomeConsumidor<>'', ' - '+NomeConsumidor, '');

    PrintIt := (NomeConsumidor <> '');

    if PrintIt then
      mConsumidorLateral.Lines.Text := NomeConsumidor;
  end;
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

procedure TACBrSATExtratoFortesFr.pNumSATDataHoraLateralBeforePrint(
  Sender: TObject; var PrintIt: Boolean);
begin
  with ACBrSATExtrato.CFe do
  begin
    lNumSATLateral.Caption   := FormatFloatBr(ide.nserieSAT,'000,000,000');
    lDataHoraLateral.Caption := FormatDateTimeBr(ide.dEmi + ide.hEmi, 'DD/MM/YYYY - hh:nn:ss');
  end;
end;

procedure TACBrSATExtratoFortesFr.pNumSATLateralBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  lTitSATLateral.Width := Trunc(pNumSATLateral.Width / 2);
end;

procedure TACBrSATExtratoFortesFr.pNumSATBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  lTitSAT.Width := Trunc(pNumSAT.Width / 2)-16;
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

procedure TACBrSATExtratoFortesFr.pQRCodeBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  pTextoLateral.Visible := ACBrSATExtrato.ImprimeQRCodeLateral;
  if pTextoLateral.Visible then
  begin
    pTextoLateral.Width := Trunc(pQRCode.Width / 2);

    mMsgAppQRCodeLateral.Lines.Text := ACBrSATExtrato.MsgAppQRCode;
  end;
end;

procedure TACBrSATExtratoFortesFr.pSATSerieHoraBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  with ACBrSATExtrato.CFe do
  begin
    lNumSAT.Caption   := FormatFloatBr(ide.nserieSAT,'000,000,000');
    lDataHora.Caption := FormatDateTimeBr(ide.dEmi + ide.hEmi, 'DD/MM/YYYY - hh:nn:ss');
  end;
end;

procedure TACBrSATExtratoFortesFr.pTotalCancBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  lTitTotalCan.Width := Trunc(pTotalCanc.Width / 2);
end;

procedure TACBrSATExtratoFortesFr.rlbConsumidorBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  with ACBrSATExtrato.CFe do
  begin
    PrintIt := (not ACBrSATExtrato.ImprimeQRCodeLateral) and
               ( (Trim(Dest.CNPJCPF) <> '') or
                 ACBrSATExtrato.ImprimeCPFNaoInformado
               );

    if PrintIt then
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
  end;
end;

procedure TACBrSATExtratoFortesFr.rlbGapDescAcresBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  PrintIt := (not Resumido) and (fNumItem < (ACBrSATExtrato.CFe.Det.Count-1));
  if not PrintIt then
    Exit;

  with ACBrSATExtrato.CFe.Det.Items[fNumItem] do
  begin
    PrintIt := ACBrSATExtrato.ImprimeDescAcrescItem and
              ( (Prod.vDesc > 0) or (Prod.vOutro > 0) or
                (Prod.vRatDesc > 0) or (Prod.vRatAcr > 0) );

    PrintIt := PrintIt or (Imposto.ISSQN.vDeducISSQN > 0);
  end;
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
    DiminuirFonteSeNecessario(lEmitCNPJ_IE_IMCan, 6);

    // Numero do Extrato ou Homologação //
    if (ide.tpAmb = taHomologacao) then
      NumExtrato := '000000'
    else
      NumExtrato := Trim( IntToStr( ACBrSATExtrato.CFeCanc.ide.nCFe ) );

    // CPF_CNPJ do Consumidor //
    lCPF_CNPJCan.Visible := (Dest.CNPJCPF <> '') or ACBrSATExtrato.ImprimeCPFNaoInformado ;
    if lCPF_CNPJCan.Visible then
      lCPF_CNPJCan.Lines.Text := StringReplace(lCPF_CNPJ.Caption,'<CPF_CNPJ>',
                                         FormatarCNPJouCPF(Dest.CNPJCPF),[]);

    lRazaoSocialNomeCanc.Visible := (Trim(Dest.xNome) <> '') ;
    if lRazaoSocialNomeCanc.Visible then
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
  if ACBrSATExtrato.Sistema <> '' then
    mSwHouseSiteCanc.Lines.Add(ACBrSATExtrato.Sistema);

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
      RLLayout.PrintDialog := MostraSetup;
      RLLayout.ShowProgress:= False ;

      if (Filtro = fiNenhum) and (Impressora <> '') then
        RLPrinter.PrinterName := Impressora;

      //Para impressoras sem guilhotina não cortar no QrCorde
      pEspacoFinal.Height := EspacoFinal;
      pEspacoFinalCan.Height  := EspacoFinal;

      // Largura e Margens do Relatório //
      RLLayout.Width := LarguraBobina;
      RLLayout.Margins.LeftMargin   := MargemEsquerda;
      RLLayout.Margins.RightMargin  := MargemDireita;
      RLLayout.Margins.TopMargin    := MargemSuperior;
      RLLayout.Margins.BottomMargin := MargemInferior;

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
        if MostraPreview then
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
          RLFiltro.FileName := NomeDocumento ;
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
if (Dest.xNome <> '' ) then
  mDestEnt.Lines.Add( ACBrStr('Destinatário: ')+Dest.xNome );

