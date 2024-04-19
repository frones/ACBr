{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: André Ferreira de Moraes                        }
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
unit ACBrDANFCeFortesFr;

interface

uses
  Classes, SysUtils,
  {$IFDEF FPC}
   LResources,
  {$ENDIF}
  {$IfDef FMX}
   FMX.Forms, FMX.Graphics,
  {$Else}
   Forms, Graphics,
  {$EndIf}
  ACBrNFeDANFEClass, ACBrBase,
  pcnNFe, pcnConversao, ACBrDFeUtil,
  RLConsts, RLUtils, RLReport, RLBarcode, RLPDFFilter, RLHTMLFilter,
  RLFilters, RLPrinters, RLTypes, Controls;

const
  CACBrNFeDANFCeFortes_Versao = '0.1.0' ;

type
  TACBrNFeDANFCeFiltro = (fiNenhum, fiPDF, fiHTML ) ;

  { TACBrNFeDANFCeFortes }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrNFeDANFCeFortes = class( TACBrNFeDANFCEClass )
  private
    FFonteLinhaItem: TFont;
    FTamanhoLogoHeight: Integer;
    FTamanhoLogoWidth: Integer;

    procedure ImprimirInterno(const Cancelado: Boolean;
      const DanfeResumido : Boolean = False;
      const AFiltro : TACBrNFeDANFCeFiltro = fiNenhum;
      const AStream: TStream = nil);
  protected
    FpNFe: TNFe;

    procedure AtribuirNFe(NFE: TNFe = Nil);
    procedure Imprimir(const DanfeResumido : Boolean = False; const AFiltro : TACBrNFeDANFCeFiltro = fiNenhum;
                       const AStream: TStream = nil);
    procedure ImprimirCancelado(const DanfeResumido : Boolean = False; const AFiltro : TACBrNFeDANFCeFiltro = fiNenhum;
                                const AStream: TStream = nil);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ImprimirDANFE(NFE : TNFe = nil); override ;
    procedure ImprimirDANFEResumido(NFE : TNFe = nil); override ;
    procedure ImprimirDANFEPDF(NFE : TNFe = nil); override;
    procedure ImprimirDANFEPDF(AStream: TStream; ANFe: TNFe = nil); override;
    procedure ImprimirDANFEResumidoPDF(NFE : TNFe = nil); override;
    procedure ImprimirDANFEResumidoPDF(AStream: TStream; ANFe: TNFe = nil); override;
    procedure ImprimirDANFECancelado(NFE : TNFe = nil);override;
    procedure ImprimirEVENTO(NFE : TNFe = nil);override;
    procedure ImprimirEVENTOPDF(NFE: TNFe = nil); override;
    procedure ImprimirEVENTOPDF(AStream: TStream; NFE: TNFe = nil); override;
  published
    property TamanhoLogoHeight: Integer read FTamanhoLogoHeight write FTamanhoLogoHeight default 50;
    property TamanhoLogoWidth: Integer read FTamanhoLogoWidth write FTamanhoLogoWidth default 77;
    property FonteLinhaItem: TFont read FFonteLinhaItem write FFonteLinhaItem;
  end ;

  { TACBrNFeDANFCeFortesFr }

  TACBrNFeDANFCeFortesFr = class(TForm)
    imgQRCodeLateral: TRLImage;
    lCancelada: TRLLabel;
    lChaveDeAcesso: TRLMemo;
    lConsumidor: TRLMemo;
    lConsumidor1: TRLMemo;
    lContingencia: TRLMemo;
    lDataAutorizacao: TRLLabel;
    lDataAutorizacao1: TRLMemo;
    lDesconto: TRLLabel;
    lDescValLiq: TRLLabel;
    lEnderecoConsumidor: TRLMemo;
    lEnderecoConsumidor1: TRLMemo;
    lMensagemFiscal: TRLMemo;
    lMensagemFiscal1: TRLMemo;
    lMsgContingencia: TRLMemo;
    lMsgDANFCe: TRLLabel;
    lNomeConsumidor: TRLMemo;
    lNomeConsumidor1: TRLMemo;
    lNumeroSerie: TRLLabel;
    lNumeroSerie1: TRLMemo;
    lContingencia1: TRLMemo;
    lObservacoes: TRLMemo;
    lOutro: TRLLabel;
    lFreteItem: TRLLabel;
    lOutroValLiq: TRLLabel;
    lFreteItemValLiq: TRLLabel;
    lProtocolo: TRLLabel;
    lProtocolo1: TRLMemo;
    lQtdItens: TRLLabel;
    lQtdTotalItensVal: TRLLabel;
    lSistema: TRLLabel;
    lNomeFantasia: TRLMemo;
    lNomeFantasiaCanc: TRLMemo;
    lRazaoSocial: TRLMemo;
    lRazaoSocialCanc: TRLMemo;
    lTitAcrescimo: TRLLabel;
    lTitFreteItem: TRLLabel;
    lTitDesconto: TRLLabel;
    lTitDescValLiq: TRLLabel;
    lTitFormaPagto: TRLLabel;
    lTitLei12741: TRLMemo;
    lTitOutroValLiq: TRLLabel;
    lTitFreteItemValLiq: TRLLabel;
    lTitTotal: TRLLabel;
    lTitTotalAcrescimo: TRLLabel;
    lTitTotalFrete: TRLLabel;
    lTitTotalAPagar: TRLLabel;
    lTitTotalDesconto: TRLLabel;
    lTitValorPago: TRLLabel;
    lTotal: TRLLabel;
    lTotalAcrescimo: TRLLabel;
    lTotalFrete: TRLLabel;
    lTotalAPagar: TRLLabel;
    lTotalDesconto: TRLLabel;
    lTotalItem: TRLLabel;
    lURLConsulta: TRLMemo;
    mLinhaTotalItem: TRLMemo;
    pGap6: TRLPanel;
    pGapEspacofinalVenda: TRLPanel;
    rlbConsumidor: TRLBand;
    rlbMensagemFiscal: TRLBand;
    rlbMsgContingencia: TRLBand;
    rlbFreteItem: TRLBand;
    rlbPagamentoTitulo: TRLBand;
    rlbQRLateral: TRLBand;
    rlbRodape: TRLBand;
    rlbTotalAcrescimo: TRLBand;
    rlbTotalFrete: TRLBand;
    rlbTotalAPagar: TRLBand;
    rlbTotalDesconto: TRLBand;
    pLogo: TRLPanel;
    rlpFreteItemTit: TRLPanel;
    rlpAcresItemVal: TRLPanel;
    rlpAcresItemTit: TRLPanel;
    rlpFreteItemVal: TRLPanel;
    rlpTotTit: TRLPanel;
    rlpTotalVal: TRLPanel;
    rlpDescItemTit: TRLPanel;
    rlpDescItemVal: TRLPanel;
    rlpDadosQRCodeLateral: TRLPanel;
    rlpImgQRCodeLateral: TRLPanel;
    rlVenda: TRLReport;
    rlbQRCode: TRLBand;
    imgQRCode: TRLImage;
    rlsbDetItem: TRLSubDetail;
    rlbDetItem: TRLBand;
    mLinhaItem: TRLMemo;
    rlbDescItem: TRLBand;
    rlbOutroItem: TRLBand;
    rlbGap: TRLBand;
    rlsbPagamentos: TRLSubDetail;
    rlbPagamento: TRLBand;
    lPagamento: TRLLabel;
    lMeioPagamento: TRLLabel;
    rlbTroco: TRLBand;
    lTitTroco: TRLLabel;
    lTroco: TRLLabel;
    rlbTotal: TRLBand;
    rlbsCabecalho: TRLSubDetail;
    rlbMsgDANFe: TRLBand;
    rlbDadosCliche: TRLBand;
    pCliche: TRLPanel;
    lEndereco: TRLMemo;
    imgLogo: TRLImage;
    rlbLegenda: TRLBand;
    lLegendaItens: TRLLabel;
    rlbChaveDeAcesso: TRLBand;
    lTitConsulteChave: TRLMemo;
    RLHTMLFilter1: TRLHTMLFilter;
    RLPDFFilter1: TRLPDFFilter;
    rlCancelamento: TRLReport;
    rlbRodapeCanc: TRLBand;
    RLDraw9: TRLDraw;
    lConsultaQRCodeCanc: TRLLabel;
    imgQRCodeCanc: TRLImage;
    RLPanel1: TRLPanel;
    lSistemaCanc: TRLLabel;
    lProtocoloCanc: TRLLabel;
    pGapEspacofinalCancelamento: TRLPanel;
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
    rllFisco: TRLLabel;

    procedure FormDestroy(Sender: TObject);
    procedure pAsteriscoBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlbChaveDeAcessoBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlbFreteItemBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlbMsgContingenciaBeforePrint(Sender: TObject;
      var PrintIt: Boolean);
    procedure rlbQRLateralBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlbRodapeBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlbsCabecalhoDataRecord(Sender: TObject; RecNo: integer;
      CopyNo: integer; var Eof: boolean; var RecordAction: TRLRecordAction);
    procedure rlbLegendaBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlbPagamentoBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlbGapBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlbTotalAcrescimoBeforePrint(Sender: TObject; var PrintIt: Boolean
      );
    procedure rlbTotalAPagarBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlbTotalBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlbTotalDescontoBeforePrint(Sender: TObject; var PrintIt: Boolean
      );
    procedure rlbTotalFreteBeforePrint(Sender: TObject; var PrintIt: Boolean);
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
    fNumItem: Integer;
    fNumPagto: Integer;
    fTotalPagto: Currency;
    fNumObs: Integer;
    fObsFisco: TStringList;
    fResumido: Boolean;
    fFiltro: TACBrNFeDANFCeFiltro;

    function CompoemEnderecoCFe: String ;
    function CompoemCliche: String;
    procedure FormataTextoItemParaUmaLinha(out LinhaItem: String);
    procedure FormataTextoItemParaDuasLinhas(out LinhaItem: String;
      out LinhaTotal: String; out ValorTotal: String);
    procedure FormataTextoItemParaNormal(out LinhaItem: String;
      out LinhaTotal: String; out ValorTotal: String);
    function AjustarDescricaoAteTamanhoMaximo(UmProd: TProd;
      const LinhaOriginal: String): String;
  public
    { Public declarations }
    property ACBrNFeDANFCeFortes : TACBrNFeDANFCeFortes read fACBrNFeDANFCeFortes ;
    property Resumido : Boolean read fResumido write fResumido;
    property Filtro         : TACBrNFeDANFCeFiltro read fFiltro write fFiltro default fiNenhum ;
  end ;

implementation

uses
  StrUtils, math,
  ACBrNFe, ACBrDFeDANFeReport, ACBrDFeReportFortes,
  ACBrValidador,
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.FilesIO,
  ACBrImage, ACBrDelphiZXingQRCode, ACBrUtil.DateTime;

{$ifdef FPC}
  {$R *.lfm}
{$else}
  {$R *.dfm}
{$ENDIF}

{ TACBrNFeDANFCeFortesFr }

procedure TACBrNFeDANFCeFortesFr.FormCreate(Sender: TObject);
begin
  fNumItem := 0 ;
  fNumPagto := 0 ;
  fTotalPagto := 0;
  fNumObs := 0 ;
  fObsFisco := TStringList.Create ;

  fACBrNFeDANFCeFortes := TACBrNFeDANFCeFortes(Owner) ;  // Link para o Pai
end;

procedure TACBrNFeDANFCeFortesFr.rlVendaDataRecord(Sender: TObject;
  RecNo: integer; CopyNo: integer; var Eof: boolean;
  var RecordAction: TRLRecordAction);
begin
  Eof := (RecNo > 1);
  RecordAction := raUseIt;
end;

procedure TACBrNFeDANFCeFortesFr.rlbChaveDeAcessoBeforePrint(Sender: TObject;
  var PrintIt: boolean);
var
  Via,LNNF: String;
begin
  with ACBrNFeDANFCeFortes.FpNFe do
  begin
    PrintIt := True ;

    if EstaVazio(procNFe.nProt) then
      Via := ' Via ' + IfThen(fACBrNFeDANFCeFortes.ViaConsumidor, 'Consumidor', 'Empresa')
    else
      Via := '';

    if ACBrNFeDANFCeFortes.ImprimeQRCodeLateral then
    begin
      lContingencia1.Lines.Clear;
      lMensagemFiscal1.Lines.Clear;

      if Ide.tpAmb = taHomologacao then
      begin
        lMensagemFiscal1.Lines.Add(ACBrStr( 'EMITIDA EM AMBIENTE DE HOMOLOGAÇÃO'));
        lMensagemFiscal1.Lines.Add(ACBrStr( 'SEM VALOR FISCAL'));
      end;

      lMensagemFiscal1.Lines.Add(ACBrStr(procNFe.xMsg));

      if (Ide.tpEmis <> teNormal) and EstaVazio(procNFe.nProt) then
      begin
        lContingencia1.Lines.Add(ACBrStr('EMITIDA EM CONTINGÊNCIA'));
        lContingencia1.Lines.Add(ACBrStr('Pendente de Autorização'));
      end;

      lContingencia1.Visible   := NaoEstaVazio(Trim(lContingencia1.Lines.Text));
      lMensagemFiscal1.Visible := NaoEstaVazio(Trim(lMensagemFiscal1.Lines.Text));

      if fACBrNFeDANFCeFortes.FormatarNumeroDocumento then
        LNNF := IntToStrZero(Ide.nNF, 9)
      else
        LNNF := IntToStr(Ide.nNF);

      lNumeroSerie1.Lines.Text := ACBrStr(
        'NFC-e nº ' + LNNF + ' ' + sLineBreak +
        'Série ' + IntToStrZero(Ide.serie, 3) + ' ' + sLineBreak +
        FormatDateTimeBr(Ide.dEmi) + sLineBreak +
        Via
      );
    end
    else
    begin
      lContingencia.Lines.Clear;
      lMensagemFiscal.Lines.Clear;

      if Ide.tpAmb = taHomologacao then
      begin
        lMensagemFiscal.Lines.Add(ACBrStr( 'EMITIDA EM AMBIENTE DE HOMOLOGAÇÃO'));
        lMensagemFiscal.Lines.Add(ACBrStr( 'SEM VALOR FISCAL'));
      end;

      lMensagemFiscal.Lines.Add(ACBrStr(procNFe.xMsg));

      if (Ide.tpEmis <> teNormal) and EstaVazio(procNFe.nProt) then
      begin
        lContingencia.Lines.Add(ACBrStr('EMITIDA EM CONTINGÊNCIA'));
        lContingencia.Lines.Add(ACBrStr('Pendente de Autorização'));
      end;

      lContingencia.Visible   := NaoEstaVazio(Trim(lContingencia.Lines.Text));
      lMensagemFiscal.Visible := NaoEstaVazio(Trim(lMensagemFiscal.Lines.Text));

      if fACBrNFeDANFCeFortes.FormatarNumeroDocumento then
        LNNF := IntToStrZero(Ide.nNF, 9)
      else
        LNNF := IntToStr(Ide.nNF);

      lNumeroSerie.Caption := ACBrStr(
        'NFC-e nº ' + LNNF + ' ' +
        'Série ' + IntToStrZero(Ide.serie, 3) + ' ' +
        FormatDateTimeBr(Ide.dEmi)+ Via );
    end;

    lTitConsulteChave.Lines.Text := ACBrStr('Consulte pela Chave de Acesso em');

    if EstaVazio(infNFeSupl.urlChave) then
      lURLConsulta.Lines.Text := TACBrNFe(fACBrNFeDANFCeFortes.ACBrNFe).GetURLConsultaNFCe(Ide.cUF, Ide.tpAmb, infNFe.Versao)
    else
      lURLConsulta.Lines.Text := infNFeSupl.urlChave;

    TDFeReportFortes.DiminuirFonteSeNecessario(lURLConsulta, 5);

    lChaveDeAcesso.Lines.Text := FormatarChaveAcesso(OnlyNumber(infNFe.ID));

    if (Ide.tpEmis = teNormal ) and (procNFe.cStat = 0) then
    begin
      lChaveDeAcesso.Lines.Text := ACBrStr('NFC-E NÃO ENVIADA PARA SEFAZ');
      lChaveDeAcesso.Font.Color := clRed;
    end;

    lCancelada.Visible := ACBrNFeDANFCeFortes.Cancelada;
    if ACBrNFeDANFCeFortes.Cancelada then
      lCancelada.Caption := ACBrStr('NFC-e CANCELADA');
  end;
end;

procedure TACBrNFeDANFCeFortesFr.rlbMsgContingenciaBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  with ACBrNFeDANFCeFortes.FpNFe do
  begin
    PrintIt :=  (Ide.tpEmis <> teNormal) or (Ide.tpAmb = taHomologacao);
    lMsgContingencia.Lines.Clear;

    if Ide.tpAmb = taHomologacao then
    begin
      lMsgContingencia.Lines.Add(ACBrStr( 'EMITIDA EM AMBIENTE DE HOMOLOGAÇÃO'));
      lMsgContingencia.Lines.Add(ACBrStr( 'SEM VALOR FISCAL'));
    end;

    if (Ide.tpEmis <> teNormal) and EstaVazio(procNFe.nProt) then
    begin
      lMsgContingencia.Lines.Add(ACBrStr('EMITIDA EM CONTINGÊNCIA'));
      lMsgContingencia.Lines.Add(ACBrStr('Pendente de Autorização'));
    end;
  end;

end;

procedure TACBrNFeDANFCeFortesFr.rlbQRLateralBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
var
  HeightTotal : Integer;
begin
  //AutoSize não está funcionando corretamente para esta Banda
  HeightTotal := 0;
  HeightTotal := HeightTotal + ifthen(lConsumidor1.Visible,lConsumidor1.Height,0);
  HeightTotal := HeightTotal + ifthen(lNomeConsumidor1.Visible,lNomeConsumidor1.Height,0);
  HeightTotal := HeightTotal + ifthen(lEnderecoConsumidor1.Visible,lEnderecoConsumidor1.Height,0);
  HeightTotal := HeightTotal + ifthen(lNumeroSerie1.Visible,lNumeroSerie1.Height,0);
  HeightTotal := HeightTotal + ifthen(lProtocolo1.Visible,lProtocolo1.Height,0);
  HeightTotal := HeightTotal + ifthen(lDataAutorizacao1.Visible,lDataAutorizacao1.Height,0);
  HeightTotal := HeightTotal + ifthen(lMensagemFiscal1.Visible,lMensagemFiscal1.Height,0);
  HeightTotal := HeightTotal + ifthen(lContingencia1.Visible,lContingencia1.Height,0);

  rlbQRLateral.Height :=  max(rlpImgQRCodeLateral.Height, HeightTotal);
end;

procedure TACBrNFeDANFCeFortesFr.rlbRodapeBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
var
  i:integer;
  MsgTributos: String;
begin
  with ACBrNFeDANFCeFortes.FpNFe do
  begin
    MsgTributos := '';

    with ACBrNFeDANFCeFortes do
    begin
      if not (ImprimeTributos = trbNenhum)then
      begin
        if (ImprimeTributos = trbSeparadamente)and ((vTribFed+vTribEst+vTribMun) > 0) then
        begin
           MsgTributos := Format('Tributos Incidentes Lei Federal 12.741/12 - Total R$ %s Federal R$ %s Estadual R$ %s Municipal R$ %s',
                                 [FormatFloatBr(vTribFed + vTribEst + vTribMun),
                                  FormatFloatBr(vTribFed),
                                  FormatFloatBr(vTribEst),
                                  FormatFloatBr(vTribMun)]);
        end
        else if (Total.ICMSTot.vTotTrib > 0) then
        begin
          MsgTributos:= Format('Tributos Totais Incidentes(Lei Federal 12.741/12): R$ %s',
                               [FormatFloatBr(Total.ICMSTot.vTotTrib)]);
        end;
      end;
    end;

    if (MsgTributos = '') then
      lTitLei12741.Visible := False
    else
      lTitLei12741.Lines.Text := MsgTributos;

    if ACBrNFeDANFCeFortes.ImprimeInfContr then
    begin
      for I := 0 to InfAdic.obsCont.Count - 1 do
      begin
        lObservacoes.Lines.Add( StringReplace( InfAdic.obsCont[i].xCampo + ': ' +
                                               InfAdic.obsCont[i].xTexto, ';', #13, [rfReplaceAll] ) ) ;
      end;
    end;

    if InfAdic.infCpl <> '' then
      lObservacoes.Lines.Add( StringReplace( InfAdic.infCpl, ';', #13, [rfReplaceAll] ) );

    lObservacoes.Visible := ( lObservacoes.Lines.Count > 0 );
  end;
end;

procedure TACBrNFeDANFCeFortesFr.rlbMensagemFiscalCancBeforePrint(
  Sender: TObject; var PrintIt: Boolean);
var LNNF : string;
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


    if fACBrNFeDANFCeFortes.FormatarNumeroDocumento then
      LNNF := IntToStrZero(Ide.nNF, 9)
    else
      LNNF := IntToStr(Ide.nNF);

    lNumeroSerieCanc.Caption := ACBrStr(
      'Número ' + LNNF + ' - ' +
      'Série ' + IntToStrZero(Ide.serie, 3)
    );

    lEmissaoViaCanc.Caption := ACBrStr(
      'Emissão ' + FormatDateTimeBr(Ide.dEmi) + ' - ' +
      'Via ' + IfThen(fACBrNFeDANFCeFortes.ViaConsumidor, 'Consumidor', 'Estabelecimento')
    );

    if EstaVazio(infNFeSupl.urlChave) then
      lTitConsulteChaveCanc.Lines.Text := ACBrStr('Consulte pela Chave de Acesso em '+
         TACBrNFe(fACBrNFeDANFCeFortes.ACBrNFe).GetURLConsultaNFCe(Ide.cUF, Ide.tpAmb, infNFe.Versao))
    else
      lTitConsulteChaveCanc.Lines.Text := ACBrStr('Consulte pela Chave de Acesso em '+
         infNFeSupl.urlChave);

    TDFeReportFortes.DiminuirFonteSeNecessario(lTitConsulteChaveCanc, 5);

    lChaveDeAcessoCanc.Caption := FormatarChaveAcesso(OnlyNumber(infNFe.ID));

    if procNFe.cStat = 0 then
    begin
      lChaveDeAcessoCanc.Caption    := ACBrStr('NFC-E NÃO ENVIADA PARA SEFAZ');
      lChaveDeAcessoCanc.Font.Color := clRed;
    end;

    if ACBrNFeDANFCeFortes.Cancelada then
      lCanceladaCanc.Caption := ACBrStr('NFC-e CANCELADA');

    rllFisco.Caption := ACBrStr(procNFe.xMsg);
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

function TACBrNFeDANFCeFortesFr.CompoemEnderecoCFe: String;
var
  Endereco: String;
begin
  with ACBrNFeDANFCeFortes.FpNFe do
  begin
    // Definindo dados do Cliche //
    Endereco := Emit.EnderEmit.xLgr ;
    if NaoEstaVazio(Emit.EnderEmit.nro) then
      Endereco := Endereco + ', '+Emit.EnderEmit.nro;
    if NaoEstaVazio(Emit.EnderEmit.xCpl) then
      Endereco := Endereco + ' '+Emit.EnderEmit.xCpl;
    if NaoEstaVazio(Emit.EnderEmit.xBairro) then
      Endereco := Endereco + ' '+Emit.EnderEmit.xBairro;
    if NaoEstaVazio(Emit.EnderEmit.xMun) then
      Endereco := Endereco + ' - '+Emit.EnderEmit.xMun;
    if NaoEstaVazio(Emit.EnderEmit.UF) then
      Endereco := Endereco + ' - '+Emit.EnderEmit.UF;
    if (Emit.EnderEmit.CEP > 0) then
      Endereco := Endereco + ' '+FormatarCEP(Emit.EnderEmit.CEP);
    if NaoEstaVazio(Emit.EnderEmit.fone) then
      Endereco := Endereco + ' Fone: '+FormatarFone(Emit.EnderEmit.fone);
    if NaoEstaVazio(Emit.IE) then
      Endereco := Endereco + ' I.E.: '+FormatarIE(Emit.IE,Emit.EnderEmit.UF);
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

procedure TACBrNFeDANFCeFortesFr.FormataTextoItemParaUmaLinha(out LinhaItem: string);
var
  UmProd: TProd;
begin
  UmProd := ACBrNFeDANFCeFortes.FpNFe.Det.Items[fNumItem].Prod;
  LinhaItem := IntToStrZero(UmProd.nItem, 3) + ' ' +
               ACBrNFeDANFCeFortes.ManterCodigo(UmProd.cEAN, UmProd.cProd) + ' ' + '[DesProd] ' +
               ACBrNFeDANFCeFortes.FormatarQuantidade(UmProd.QCom, False) + ' ' + Trim(UmProd.uCom) +
               ' X ' + ACBrNFeDANFCeFortes.FormatarValorUnitario(UmProd.VUnCom) + ' ' +
               FormatFloatBr(UmProd.vProd);
  LinhaItem := AjustarDescricaoAteTamanhoMaximo(UmProd, LinhaItem);
end;

procedure TACBrNFeDANFCeFortesFr.FormataTextoItemParaDuasLinhas(out
  LinhaItem: String; out LinhaTotal: String; out ValorTotal: String);
var
  UmProd: TProd;
begin
  UmProd := ACBrNFeDANFCeFortes.FpNFe.Det.Items[fNumItem].Prod;
  LinhaItem := IntToStrZero(UmProd.nItem, 3) + ' ' +
               ACBrNFeDANFCeFortes.ManterCodigo(UmProd.cEAN, UmProd.cProd) + ' ' + '[DesProd]';

  LinhaItem := AjustarDescricaoAteTamanhoMaximo(UmProd, LinhaItem);

  LinhaTotal := '| ' + ACBrNFeDANFCeFortes.FormatarQuantidade(UmProd.qCom, False) + '|' +
                Trim(UmProd.uCom) + ' X ' + ACBrNFeDANFCeFortes.FormatarValorUnitario(UmProd.vUnCom) + '|';
  LinhaTotal := TDFeReportFortes.EspacejarTextoGrafico(LinhaTotal, mLinhaTotalItem.Width - 10, mLinhaTotalItem.Font);
  ValorTotal := FormatFloatBr(UmProd.vProd);
end;

procedure TACBrNFeDANFCeFortesFr.FormataTextoItemParaNormal(out
  LinhaItem: String; out LinhaTotal: String; out ValorTotal: String);
var
  aProd: TProd;
  infoAdProd: String;
begin
  aProd := ACBrNFeDANFCeFortes.FpNFe.Det.Items[fNumItem].Prod;
  LinhaItem := IntToStrZero(aProd.nItem, 3) + ' ' + // DEBUG {IntToStr(mLinhaItem.Width) + ','+}
               ACBrNFeDANFCeFortes.ManterCodigo(aProd.cEAN, aProd.cProd) + ' ' + Trim(aProd.xProd);

  infoAdProd := ACBrNFeDANFCeFortes.ManterinfAdProd(ACBrNFeDANFCeFortes.FpNFe, fNumItem);
  if Trim(infoAdProd) <> '' then
    LinhaItem := LinhaItem + infoAdProd;
  LinhaTotal := '| ' + ACBrNFeDANFCeFortes.FormatarQuantidade(aProd.qCom, False) + '|' +
                Trim(aProd.uCom) + ' X ' + ACBrNFeDANFCeFortes.FormatarValorUnitario(aProd.vUnCom) + '|';
  LinhaTotal := TDFeReportFortes.EspacejarTextoGrafico(LinhaTotal, mLinhaTotalItem.Width - 10, mLinhaTotalItem.Font);
  ValorTotal := FormatFloatBr(aProd.vProd);
end;

function TACBrNFeDANFCeFortesFr.AjustarDescricaoAteTamanhoMaximo(UmProd: TProd;
  const LinhaOriginal: String): String;
var
  ABMP: TBitmap;
  TamanhoDescricao: Integer;
  TamanhoLinha, TamanhoMaximo: Integer;
  DescricaoProduto, sDescricao: string;
begin
  Result := '';
  TamanhoDescricao := 9;
  TamanhoLinha     := 0;
  TamanhoMaximo    := mLinhaItem.Width - 40;
  DescricaoProduto := Trim(UmProd.xProd);

  ABMP := TBitmap.Create;
  try
    ABMP.Canvas.Font.Assign(mLinhaItem.Font);
    while TamanhoLinha < TamanhoMaximo do
    begin
      Inc(TamanhoDescricao);
      sDescricao := PadRight(DescricaoProduto, TamanhoDescricao);
      Result := StringReplace(LinhaOriginal, '[DesProd]', sDescricao, [rfReplaceAll]);
      TamanhoLinha := ABMP.Canvas.TextWidth(Result);
    end;
  finally
    ABMP.Free;
  end;
end;

procedure TACBrNFeDANFCeFortesFr.rlVendaBeforePrint(Sender: TObject;
  var PrintIt: boolean);
var
  qrcode: String;
  CarregouLogo: Boolean;
begin
  fNumItem  := 0;
  fNumPagto := 0;
  fTotalPagto := 0;
  fNumObs := 0;
  fObsFisco.Clear;

  pGapEspacofinalVenda.Height := ACBrNFeDANFCeFortes.EspacoFinal;
  rlbDetItem.Font.Assign(ACBrNFeDANFCeFortes.FonteLinhaItem);
  rlbDescItem.Font.Assign(ACBrNFeDANFCeFortes.FonteLinhaItem);
  rlbOutroItem.Font.Assign(ACBrNFeDANFCeFortes.FonteLinhaItem);

  mLinhaItem.Width := rlbDetItem.Width;
  mLinhaTotalItem.Width := (rlbDetItem.Width - lTotalItem.Width);
  mLinhaTotalItem.Visible := (not ACBrNFeDANFCeFortes.ImprimeEmUmaLinha);
  lTotalItem.Visible := (not ACBrNFeDANFCeFortes.ImprimeEmUmaLinha);

  with ACBrNFeDANFCeFortes.FpNFe do
  begin
    lNomeFantasia.Visible := ACBrNFeDANFCeFortes.ImprimeNomeFantasia;
    if lNomeFantasia.Visible then
      lNomeFantasia.Lines.Text := Emit.xFant ;

    lRazaoSocial.Lines.Text := 'CNPJ: '+FormatarCNPJ(Emit.CNPJCPF)+' '+Emit.xNome ;
    lEndereco.Lines.Text    := CompoemEnderecoCFe;

    lNomeFantasia.Alignment := taCenter;
    lRazaoSocial.Alignment  := taCenter;
    lEndereco.Alignment     := taCenter;

    CarregouLogo := TDFeReportFortes.CarregarLogo(imgLogo, ACBrNFeDANFCeFortes.Logo);
    pLogo.Visible := CarregouLogo;

    if pLogo.Visible then
    begin
      imgLogo.AutoSize := ACBrNFeDANFCeFortes.ExpandeLogoMarca;

      if ACBrNFeDANFCeFortes.ImprimeLogoLateral then
      begin
        pLogo.AutoSize   := imgLogo.AutoSize;
        pLogo.AutoExpand := imgLogo.AutoSize;

        imgLogo.Center := True;
        pCliche.Align := faClientTop;
        pLogo.Align   := faLeftTop;
        pLogo.Width   := ACBrNFeDANFCeFortes.TamanhoLogoWidth;
        pLogo.Height  := ACBrNFeDANFCeFortes.TamanhoLogoHeight;
        imgLogo.Align := faClient;
        lNomeFantasia.Alignment := taLeftJustify;
        lRazaoSocial.Alignment  := taLeftJustify;
        lEndereco.Alignment     := taLeftJustify;
      end
      else
      begin
        pLogo.Align    := faTop;
        pLogo.Top      := 0;  // Força ir para o Topo
        pCliche.Align  := faTop;
        imgLogo.Width  := ACBrNFeDANFCeFortes.TamanhoLogoWidth;
        imgLogo.Height := ACBrNFeDANFCeFortes.TamanhoLogoHeight;
        imgLogo.Align  := faClientTop;
      end;
    end;

    // QRCode  //
    if EstaVazio(Trim(infNFeSupl.qrCode)) then
      qrcode := TACBrNFe(ACBrNFeDANFCeFortes.ACBrNFe).GetURLQRCode( ide.cUF, ide.tpAmb,
                                     OnlyNumber(InfNFe.ID),  //correcao para pegar somente numeros, estava indo junto o NFE
                                     ifthen(Dest.idEstrangeiro <> '',Dest.idEstrangeiro, OnlyNumber(Dest.CNPJCPF)),
                                     ide.dEmi,
                                     Total.ICMSTot.vNF, Total.ICMSTot.vICMS,
                                     signature.DigestValue,
                                     infNFe.Versao)
    else
      qrcode := infNFeSupl.qrCode;

    if ACBrNFeDANFCeFortes.ImprimeQRCodeLateral then
    begin
      rlbConsumidor.Visible := False;
      rlbQRCode.Visible     := False;
      rlbQRLateral.Visible  := True;
      PintarQRCode(qrcode, imgQRCodeLateral.Picture.Bitmap, qrUTF8NoBOM);

      if (Dest.idEstrangeiro = '') and (Dest.CNPJCPF = '') then
      begin
        lConsumidor1.Lines.Text  := ACBrStr('CONSUMIDOR NÃO IDENTIFICADO');
        lNomeConsumidor1.Visible := False;
      end
      else
      begin
        if Dest.idEstrangeiro <> '' then
        begin
          lConsumidor1.Lines.Text  := 'CONSUMIDOR Id. Estrangeiro: '+Dest.idEstrangeiro;
        end
        else
        begin
          if Length(trim(Dest.CNPJCPF)) > 11 then
             lConsumidor1.Lines.Text  := 'CONSUMIDOR CNPJ: '+FormatarCNPJ(Dest.CNPJCPF)
          else
             lConsumidor1.Lines.Text  := 'CONSUMIDOR CPF: '+FormatarCPF(Dest.CNPJCPF);
        end;

        lNomeConsumidor1.Lines.Clear;
        lNomeConsumidor1.Visible := NaoEstaVazio(Trim(Dest.xNome));
        lNomeConsumidor1.Lines.Text := Dest.xNome;
      end;

      lEnderecoConsumidor1.Lines.Clear;
      lEnderecoConsumidor1.Lines.Text := Trim(Dest.EnderDest.xLgr)+' '+
                                         Trim(Dest.EnderDest.nro)+' '+
                                         Trim(Dest.EnderDest.xCpl)+' '+
                                         Trim(Dest.EnderDest.xBairro)+' '+
                                         Trim(Dest.EnderDest.xMun);

      lEnderecoConsumidor1.Visible := NaoEstaVazio(Trim(lEnderecoConsumidor1.Lines.Text));

      if not EstaVazio(Trim(procNFe.nProt)) then
      begin
        lProtocolo1.Visible := True;
        lProtocolo1.Lines.Text := ACBrStr('Protocolo de Autorização: '+procNFe.nProt);
        if (procNFe.dhRecbto<>0) then
        begin
          lDataAutorizacao1.Visible := True;
          lDataAutorizacao1.Lines.Text := ACBrStr('Data de Autorização '+FormatDateTimeBr(procNFe.dhRecbto));
        end
        else
          lDataAutorizacao1.Visible := False;
      end
      else
      begin
        lProtocolo1.Lines.Text := '';
        lProtocolo1.Visible := False;
        lDataAutorizacao1.Lines.Text := '';
        lDataAutorizacao1.Visible := False;
      end;
    end
    else
    begin
      rlbQRLateral.Visible  := False;
      rlbConsumidor.Visible := True;
      rlbQRCode.Visible     := True;
      PintarQRCode(qrcode, imgQRCode.Picture.Bitmap, qrUTF8NoBOM);

      if (Dest.idEstrangeiro = '') and (Dest.CNPJCPF = '') then
      begin
        lConsumidor.Lines.Text  := ACBrStr('CONSUMIDOR NÃO IDENTIFICADO');
        lNomeConsumidor.Visible := False;
      end
      else
      begin
        if Dest.idEstrangeiro <> '' then
        begin
          lConsumidor.Lines.Text  := 'CONSUMIDOR Id. Estrangeiro: '+Dest.idEstrangeiro;
        end
        else
        begin
          if Length(trim(Dest.CNPJCPF)) > 11 then
             lConsumidor.Lines.Text  := 'CONSUMIDOR CNPJ: '+FormatarCNPJ(Dest.CNPJCPF)
          else
             lConsumidor.Lines.Text  := 'CONSUMIDOR CPF: '+FormatarCPF(Dest.CNPJCPF);
        end;

        lNomeConsumidor.Lines.Clear;
        lNomeConsumidor.Visible := NaoEstaVazio(Trim(Dest.xNome));
        lNomeConsumidor.Lines.Text := Dest.xNome;
      end;

      lEnderecoConsumidor.Lines.Clear;
      lEnderecoConsumidor.Lines.Text := Trim(Dest.EnderDest.xLgr)+' '+
                                        Trim(Dest.EnderDest.nro)+' '+
                                        Trim(Dest.EnderDest.xCpl)+' '+
                                        Trim(Dest.EnderDest.xBairro)+' '+
                                        Trim(Dest.EnderDest.xMun);

      lEnderecoConsumidor.Visible := NaoEstaVazio(Trim(lEnderecoConsumidor.Lines.Text));

      if not EstaVazio(Trim(procNFe.nProt)) then
      begin
        lProtocolo.Visible := True;
        lProtocolo.Caption := ACBrStr('Protocolo de Autorização: '+procNFe.nProt);
        if (procNFe.dhRecbto<>0) then
        begin
          lDataAutorizacao.Visible := True;
          lDataAutorizacao.Caption := ACBrStr('Data de Autorização '+FormatDateTimeBr(procNFe.dhRecbto));
        end
        else
          lDataAutorizacao.Visible := False;
      end
      else
      begin
        lProtocolo.Caption := '';
        lProtocolo.Visible := False;
        lDataAutorizacao.Caption := '';
        lDataAutorizacao.Visible := False;
      end;
    end;
  end;
end;

procedure TACBrNFeDANFCeFortesFr.rlbDescItemBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
var
  vAcrescimos: Double;
  LValor : Double;
begin
  with ACBrNFeDANFCeFortes.FpNFe.Det.Items[fNumItem] do
  begin
    LValor := ACBrNFeDANFCeFortes.CalcularValorDescontoItem(ACBrNFeDANFCeFortes.FpNFe, fNumItem);
    PrintIt := (not Resumido) and
               ACBrNFeDANFCeFortes.ImprimeDescAcrescItem and
               (LValor > 0);

    if PrintIt then
    begin
      lDesconto.Caption := FormatFloatBr(LValor,'-,0.00');
      vAcrescimos       := Prod.vFrete + Prod.vSeg + Prod.vOutro;
      if (vAcrescimos > 0) then      // Imprimirá Valor líquido, na próxima Banda
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
        lDescValLiq.Caption := FormatFloatBr(ACBrNFeDANFCeFortes.CalcularValorLiquidoItem(ACBrNFeDANFCeFortes.FpNFe, fNumItem,[TVLDesconto]));
      end;
    end;
  end;
end;

procedure TACBrNFeDANFCeFortesFr.rlbFreteItemBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
var
  vOutros: Double;
begin
  with ACBrNFeDANFCeFortes.FpNFe.Det.Items[fNumItem] do
  begin
    PrintIt := (not Resumido) and
               ACBrNFeDANFCeFortes.ImprimeDescAcrescItem and
               (Prod.vFrete > 0);

    if PrintIt then
    begin
      lFreteItem.Caption := FormatFloatBr(Prod.vFrete,'+,0.00');
      vOutros            := Prod.vSeg + Prod.vOutro;
      if (vOutros > 0) then       // Imprimirá Valor líquido, na próxima Banda
      begin
        lTitFreteItemValLiq.Visible := False;
        lFreteItemValLiq.Visible := False;
        rlbFreteItem.Height := 12;
      end
      else
      begin
        rlbFreteItem.Height := 24;
        lTitFreteItemValLiq.Visible := True;
        lFreteItemValLiq.Visible := True;
        lFreteItemValLiq.Caption := FormatFloatBr(ACBrNFeDANFCeFortes.CalcularValorLiquidoItem(ACBrNFeDANFCeFortes.FpNFe, fNumItem,[TVLDesconto,TVLFrete]));
      end;
    end;
  end;
end;

procedure TACBrNFeDANFCeFortesFr.rlbOutroItemBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
var
  vOutros: Double;
begin
  with ACBrNFeDANFCeFortes.FpNFe.Det.Items[fNumItem] do
  begin
    vOutros := Prod.vSeg + Prod.vOutro;
    PrintIt := (not Resumido) and
               ACBrNFeDANFCeFortes.ImprimeDescAcrescItem and
               (vOutros > 0);

    if PrintIt then
    begin
      lOutro.Caption       := FormatFloatBr(vOutros,'+,0.00');
      lOutroValLiq.Caption := FormatFloatBr(ACBrNFeDANFCeFortes.CalcularValorLiquidoItem(ACBrNFeDANFCeFortes.FpNFe, fNumItem,[TVLDesconto,TVLFrete,TVLOutros]));
    end;
  end;
end;

procedure TACBrNFeDANFCeFortesFr.rlbDetItemBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
var
  LinhaItem, LinhaTotal, ValTotal: String;
begin
  PrintIt := not Resumido;
  if not PrintIt then exit;

  mLinhaItem.Lines.Clear;
  mLinhaTotalItem.Lines.Clear;
  lTotalItem.Caption := EmptyStr;

  if ACBrNFeDANFCeFortes.ImprimeEmUmaLinha then
  begin
    FormataTextoItemParaUmaLinha(LinhaItem);
    mLinhaItem.Lines.Text := LinhaItem;
  end
  else if ACBrNFeDANFCeFortes.ImprimeEmDuasLinhas then
  begin
    FormataTextoItemParaDuasLinhas(LinhaItem, LinhaTotal, ValTotal);
    mLinhaItem.Lines.Text      := LinhaItem;
    mLinhaTotalItem.Lines.Text := LinhaTotal;
    lTotalItem.Caption         := ValTotal;
  end
  else
  begin
    FormataTextoItemParaNormal(LinhaItem, LinhaTotal, ValTotal);
    mLinhaItem.Lines.Text      := LinhaItem;
    mLinhaTotalItem.Lines.Text := LinhaTotal;
    lTotalItem.Caption         := ValTotal;
  end;
end;

procedure TACBrNFeDANFCeFortesFr.rlbPagamentoBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  lMeioPagamento.Caption  := ACBrNFeDANFCeFortes.ManterDescricaoPagamentos(
                             ACBrNFeDANFCeFortes.FpNFe.pag.Items[fNumPagto]);
  with ACBrNFeDANFCeFortes.FpNFe.pag.Items[fNumPagto] do
  begin
    lPagamento.Caption := FormatFloatBr(vPag);
    fTotalPagto        := fTotalPagto + vPag;
  end;
end;

procedure TACBrNFeDANFCeFortesFr.rlbGapBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  PrintIt := not Resumido;
end;

procedure TACBrNFeDANFCeFortesFr.rlbTotalAPagarBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
var
  vAcrescimos: Double;
begin
  with ACBrNFeDANFCeFortes.FpNFe.Total do
  begin
    vAcrescimos := ICMSTot.vFrete + ICMSTot.vSeg + ICMSTot.vOutro;
  end;
  PrintIt:= (vAcrescimos > 0) or (ACBrNFeDANFCeFortes.FpNFe.Total.ICMSTot.vDesc > 0);

  if PrintIt then
    lTotalAPagar.Caption := FormatFloatBr(ACBrNFeDANFCeFortes.FpNFe.Total.ICMSTot.vNF);
end;

procedure TACBrNFeDANFCeFortesFr.rlbLegendaBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  PrintIt := not Resumido;
  lLegendaItens.Caption := TDFeReportFortes.EspacejarTextoGrafico(
    '#|Cód|Descrição|Qtd|Un|Vl Unit.|Vl Total', lLegendaItens.Width-10, lLegendaItens.Font);
end;

procedure TACBrNFeDANFCeFortesFr.FormDestroy(Sender: TObject);
begin
  fObsFisco.Free;
end;

procedure TACBrNFeDANFCeFortesFr.lSistemaBeforePrint(Sender: TObject;
  var Text: string; var PrintIt: Boolean);
begin
  PrintIt := True;
  if NaoEstaVazio(fACBrNFeDANFCeFortes.Sistema) then
    Text := fACBrNFeDANFCeFortes.Sistema + Space(8);
end;

procedure TACBrNFeDANFCeFortesFr.pAsteriscoBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  PrintIt := not Resumido;
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
  lTotal.Caption := FormatFloatBr(ACBrNFeDANFCeFortes.FpNFe.Total.ICMSTot.vProd+
                                  ACBrNFeDANFCeFortes.FpNFe.Total.ISSQNtot.vServ);
end;

procedure TACBrNFeDANFCeFortesFr.rlbTotalDescontoBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
  var LValorDesconto : Double;
begin
  LValorDesconto := ACBrNFeDANFCeFortes.CalcularValorDescontoTotal(ACBrNFeDANFCeFortes.FpNFe);

  PrintIt := (LValorDesconto > 0);

  if PrintIt then
    lTotalDesconto.Caption := '-' + FormatFloatBr(LValorDesconto);
end;

procedure TACBrNFeDANFCeFortesFr.rlbTotalFreteBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  PrintIt := (ACBrNFeDANFCeFortes.FpNFe.Total.ICMSTot.vFrete > 0);

  if PrintIt then
    lTotalFrete.Caption := '+' + FormatFloatBr(ACBrNFeDANFCeFortes.FpNFe.Total.ICMSTot.vFrete);
end;

procedure TACBrNFeDANFCeFortesFr.rlbTotalAcrescimoBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
var
  vOutros: Double;
begin
  with ACBrNFeDANFCeFortes.FpNFe.Total do
  begin
    vOutros := ICMSTot.vSeg + ICMSTot.vOutro;
  end;

  PrintIt := (vOutros > 0);
  if PrintIt then
    lTotalAcrescimo.Caption := '+' + FormatFloatBr(vOutros);
end;

procedure TACBrNFeDANFCeFortesFr.rlbTrocoBeforePrint(Sender: TObject;
  var PrintIt: boolean);
var
  Troco : Currency ;
begin
  Troco := ACBrNFeDANFCeFortes.FpNFe.pag.vTroco;
  if Troco = 0 then
  begin
    Troco := fACBrNFeDANFCeFortes.vTroco;
  end;
  PrintIt := (Troco> 0);

  if PrintIt then
    lTroco.Caption := FormatFloatBr(Troco);
end;

procedure TACBrNFeDANFCeFortesFr.rlCancelamentoBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
var
  qrcode: String;
begin
  fNumItem  := 0;
  fNumPagto := 0;
  fTotalPagto := 0;
  fNumObs   := 0;
  fObsFisco.Clear;

  pGapEspacofinalCancelamento.Height := ACBrNFeDANFCeFortes.EspacoFinal;

  with ACBrNFeDANFCeFortes.FpNFe do
  begin
    lNomeFantasiaCanc.Lines.Text:= Emit.xFant ;
    lRazaoSocialCanc.Lines.Text := Emit.xNome ;
    lEmitCNPJ_IE_IM_Camc.Caption:= CompoemCliche;
    lEnderecoCanc.Lines.Text    := CompoemEnderecoCFe;

    // QRCode  //
    if EstaVazio(Trim(infNFeSupl.qrCode)) then
      qrcode := TACBrNFe(ACBrNFeDANFCeFortes.ACBrNFe).GetURLQRCode( ide.cUF, ide.tpAmb,
                                     OnlyNumber(InfNFe.ID),  //correcao para pegar somente numeros, estava indo junto o NFE
                                     ifthen(Dest.idEstrangeiro <> '',Dest.idEstrangeiro, OnlyNumber(Dest.CNPJCPF)),
                                     ide.dEmi,
                                     Total.ICMSTot.vNF, Total.ICMSTot.vICMS,
                                     signature.DigestValue,
                                     infNFe.Versao)
    else
      qrcode := infNFeSupl.qrCode;

    PintarQRCode(qrcode , imgQRCodeCanc.Picture.Bitmap, qrUTF8NoBOM);

    lProtocoloCanc.Caption := ACBrStr('Protocolo de Autorização: '+procNFe.nProt+
                           ' '+ifthen(procNFe.dhRecbto<>0,FormatDateTimeBr(procNFe.dhRecbto),''));

  end;
end;

{ TACBrNFeDANFCeFortes }

constructor TACBrNFeDANFCeFortes.Create(AOwner: TComponent);
begin
  inherited create( AOwner );

  FTamanhoLogoHeight := 50;
  FTamanhoLogoWidth := 77;
  FFonteLinhaItem := TFont.Create;
  FFonteLinhaItem.Name := 'Lucida Console';
  FFonteLinhaItem.Size := 7;
end;

destructor TACBrNFeDANFCeFortes.Destroy;
begin
  FFonteLinhaItem.Free;
  inherited Destroy ;
end;

procedure TACBrNFeDANFCeFortes.ImprimirDANFE(NFE: TNFe);
begin
  AtribuirNFe(NFE);
  Imprimir(False);
end;

procedure TACBrNFeDANFCeFortes.ImprimirDANFECancelado(NFE: TNFe);
begin
  AtribuirNFe(NFE);
  ImprimirCancelado(True);
end;

procedure TACBrNFeDANFCeFortes.ImprimirEVENTO(NFE: TNFe);
begin
  ImprimirDANFECancelado(NFE);
end;

procedure TACBrNFeDANFCeFortes.ImprimirEVENTOPDF(NFE: TNFe);
begin
  AtribuirNFe(NFE);
  ImprimirCancelado(True, fiPDF);
end;

procedure TACBrNFeDANFCeFortes.ImprimirEVENTOPDF(AStream: TStream; NFE: TNFe = nil);
begin
  AtribuirNFe(NFE);
  ImprimirCancelado(True, fiPDF, AStream);
end;

procedure TACBrNFeDANFCeFortes.ImprimirDANFEResumido(NFE: TNFe);
begin
  AtribuirNFe(NFE);
  Imprimir(True);
end;

procedure TACBrNFeDANFCeFortes.ImprimirDANFEPDF(NFE: TNFe);
begin
  AtribuirNFe(NFE);
  Imprimir(False, fiPDF);
end;

procedure TACBrNFeDANFCeFortes.ImprimirDANFEPDF(AStream: TStream; ANFE: TNFe);
begin
  AtribuirNFe(ANFE);
  Imprimir(False, fiPDF, AStream);
end;

procedure TACBrNFeDANFCeFortes.ImprimirDANFEResumidoPDF(NFE: TNFe);
begin
  AtribuirNFe(NFE);
  Imprimir(True, fiPDF);
end;

procedure TACBrNFeDANFCeFortes.ImprimirDANFEResumidoPDF(AStream: TStream; ANFe: TNFe = nil);
begin
  AtribuirNFe(ANFe);
  Imprimir(True, fiPDF, AStream);
end;

procedure TACBrNFeDANFCeFortes.Imprimir(const DanfeResumido: Boolean; const AFiltro: TACBrNFeDANFCeFiltro;
  const AStream: TStream);
begin
  ImprimirInterno(False, DanfeResumido, AFiltro, AStream);
end;

procedure TACBrNFeDANFCeFortes.ImprimirCancelado(const DanfeResumido: Boolean; const AFiltro: TACBrNFeDANFCeFiltro;
  const  AStream: TStream);
begin
  ImprimirInterno(True, DanfeResumido, AFiltro, AStream);
end;

procedure TACBrNFeDANFCeFortes.ImprimirInterno(const Cancelado: Boolean;  const DanfeResumido: Boolean;
  const AFiltro: TACBrNFeDANFCeFiltro; const  AStream: TStream);
var
  frACBrNFeDANFCeFortesFr: TACBrNFeDANFCeFortesFr;
  RLLayout: TRLReport;
  RLFiltro: TRLCustomSaveFilter;
begin
  frACBrNFeDANFCeFortesFr := TACBrNFeDANFCeFortesFr.Create(Self);
  try
    with frACBrNFeDANFCeFortesFr do
    begin
      if AlterarEscalaPadrao then
      begin
        frACBrNFeDANFCeFortesFr.Scaled := False;
        frACBrNFeDANFCeFortesFr.ScaleBy(NovaEscala , Screen.PixelsPerInch);
      end;

      Filtro := AFiltro;
      if Cancelado then
        RLLayout := rlCancelamento
      else
        RLLayout := rlVenda;

      Resumido := DanfeResumido or (not Self.ImprimeItens);

      if (NumCopias > 0) and (RLPrinter.Copies <> NumCopias) then
      begin
        RLPrinter.Copies := NumCopias;
      end;

      if not EstaVazio(Impressora) then
        RLPrinter.PrinterName := Impressora;

      RLLayout.JobTitle := NomeDocumento;
      if (RLLayout.JobTitle = '') then
        RLLayout.JobTitle := OnlyNumber(FpNFe.infNFe.ID) + IfThen(Cancelado, '-cancelado', '')+'-nfe.xml';

      RLLayout.ShowProgress := MostraStatus;
      RLLayout.PrintDialog  := (not MostraPreview) and EstaVazio(Impressora);

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

      RLLayout.UnlimitedHeight := FormularioContinuo; // ****** ATENÇÃO ******
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
            fiPDF  : RLFiltro := RLPDFFilter1;
            fiHTML : RLFiltro := RLHTMLFilter1;
          else
            exit ;
          end ;

          RLFiltro.ShowProgress := ACBrNFeDANFCeFortes.MostraStatus;

          if Assigned(AStream) then
          begin
            RLPDFFilter1.FilterPages(RLLayout.Pages, AStream);
          end
          else
          begin
            RLFiltro.FileName := PathWithDelim(ACBrNFeDANFCeFortes.PathPDF) + ChangeFileExt( RLLayout.JobTitle, '.pdf');
            RLFiltro.FilterPages( RLLayout.Pages );
            ACBrNFeDANFCeFortes.FPArquivoPDF := RLFiltro.FileName;
          end;
        end;
      end;
    end;
  finally
    frACBrNFeDANFCeFortesFr.Free ;
  end;
end;

procedure TACBrNFeDANFCeFortes.AtribuirNFe(NFE: TNFe);
begin
  if NFe = nil then
  begin
    if not Assigned(ACBrNFe) then
      raise Exception.Create('Componente ACBrNFe não atribuído');

    FpNFe := TACBrNFe(ACBrNFe).NotasFiscais.Items[0].NFe;
  end
  else
    FpNFe := NFE;
end;

{$ifdef FPC}
initialization
   {$I ACBrNFeDANFCeFortes.lrs}
{$endif}

end.
