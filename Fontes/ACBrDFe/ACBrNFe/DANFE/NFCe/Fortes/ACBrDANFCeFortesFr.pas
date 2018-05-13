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
|* 04/04/2013:  André Ferreira de Moraes
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
     RLConsts, RLReport, RLBarcode, RLPDFFilter, RLHTMLFilter,
     RLFilters, RLPrinters, RLTypes, Controls;

const
  CACBrNFeDANFCeFortes_Versao = '0.1.0' ;

type
  TACBrSATExtratoFiltro = (fiNenhum, fiPDF, fiHTML ) ;

  { TACBrNFeDANFCeFortes }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}	
  TACBrNFeDANFCeFortes = class( TACBrNFeDANFEClass )
  private
    function CalcularCaractesWidth( Canvas : TCanvas; WidthTotal : Integer ): Integer;
    procedure DiminuirFonteSeNecessario( ARLMemo: TRLMemo; TamanhoMinimo: Integer = 1);

    procedure ImprimirInterno(const Cancelado: Boolean;
      const DanfeResumido : Boolean = False;
      const AFiltro : TACBrSATExtratoFiltro = fiNenhum);
  protected
    FpNFe: TNFe;

    procedure AtribuirNFe(NFE: TNFe = Nil);
    procedure Imprimir(const DanfeResumido : Boolean = False; const AFiltro : TACBrSATExtratoFiltro = fiNenhum);
    procedure ImprimirCancelado(const DanfeResumido : Boolean = False; const AFiltro : TACBrSATExtratoFiltro = fiNenhum);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ImprimirDANFE(NFE : TNFe = nil); override ;
    procedure ImprimirDANFEResumido(NFE : TNFe = nil); override ;
    procedure ImprimirDANFEPDF(NFE : TNFe = nil); override;
    procedure ImprimirDANFEResumidoPDF(NFE : TNFe = nil);override;
    procedure ImprimirDANFECancelado(NFE : TNFe = nil);override;
    procedure ImprimirEVENTO(NFE : TNFe = nil);override;
    procedure ImprimirEVENTOPDF(NFE: TNFe = nil); override;
  published
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
    lProtocolo: TRLLabel;
    lProtocolo1: TRLMemo;
    lSistema: TRLLabel;
    lNomeFantasia: TRLMemo;
    lNomeFantasiaCanc: TRLMemo;
    lRazaoSocial: TRLMemo;
    lRazaoSocialCanc: TRLMemo;
    lTitFormaPagto: TRLLabel;
    lTitLei12741: TRLMemo;
    lTitTotalAcrescimo: TRLLabel;
    lTitTotalAPagar: TRLLabel;
    lTitTotalDesconto: TRLLabel;
    lTitValorPago: TRLLabel;
    lTotalAcrescimo: TRLLabel;
    lTotalAPagar: TRLLabel;
    lTotalDesconto: TRLLabel;
    lURLConsulta: TRLMemo;
    pGap6: TRLPanel;
    pGap9: TRLPanel;
    rlbConsumidor: TRLBand;
    rlbMensagemFiscal: TRLBand;
    rlbMsgContingencia: TRLBand;
    rlbPagamentoTitulo: TRLBand;
    rlbQRLateral: TRLBand;
    rlbRodape: TRLBand;
    rlbTotalAcrescimo: TRLBand;
    rlbTotalAPagar: TRLBand;
    rlbTotalDesconto: TRLBand;
    pLogoLateral: TRLPanel;
    rlpDadosQRCodeLateral: TRLPanel;
    rlpImgQRCodeLateral: TRLPanel;
    rlVenda: TRLReport;
    rlbQRCode: TRLBand;
    imgQRCode: TRLImage;
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
    rlbsCabecalho: TRLSubDetail;
    rlbMsgDANFe: TRLBand;
    rlbDadosCliche: TRLBand;
    pLogoeCliche: TRLPanel;
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
    procedure pLogoeClicheBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure pLogoLateralBeforePrint(Sender: TObject; var PrintIt: Boolean);
    procedure rlbChaveDeAcessoBeforePrint(Sender: TObject; var PrintIt: boolean);
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

    procedure PintarQRCode(const QRCodeData: String; APict: TPicture);
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
  Via: String;
begin
  with ACBrNFeDANFCeFortes.FpNFe do
  begin
    PrintIt := True ;

    if EstaVazio(procNFe.nProt) then
      Via := ' Via ' + IfThen(fACBrNFeDANFCeFortes.ViaConsumidor, 'Consumidor', 'Empresa')
    else
      Via := '';

    if ACBrNFeDANFCeFortes.QRCodeLateral then
    begin
      lContingencia1.Lines.Clear;
      lMensagemFiscal1.Lines.Clear;

      if Ide.tpAmb = taHomologacao then
      begin
        lMensagemFiscal1.Lines.Add(ACBrStr( 'EMITIDA EM AMBIENTE DE HOMOLOGAÇÃO'));
        lMensagemFiscal1.Lines.Add(ACBrStr( 'SEM VALOR FISCAL'));
      end;

      if (Ide.tpEmis <> teNormal) and EstaVazio(procNFe.nProt) then
      begin
        lContingencia1.Lines.Add(ACBrStr('EMITIDA EM CONTINGÊNCIA'));
        lContingencia1.Lines.Add(ACBrStr('Pendente de Autorização'));
      end;

      lContingencia1.Visible   := NaoEstaVazio(Trim(lContingencia1.Lines.Text));
      lMensagemFiscal1.Visible := NaoEstaVazio(Trim(lMensagemFiscal1.Lines.Text));

      lNumeroSerie1.Lines.Text := ACBrStr(
        'NFC-e nº ' + IntToStrZero(Ide.nNF, 9) + ' ' +
        'Série ' + IntToStrZero(Ide.serie, 3) + ' ' +
        DateTimeToStr(Ide.dEmi)+Via );
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

      if (Ide.tpEmis <> teNormal) and EstaVazio(procNFe.nProt) then
      begin
        lContingencia.Lines.Add(ACBrStr('EMITIDA EM CONTINGÊNCIA'));
        lContingencia.Lines.Add(ACBrStr('Pendente de Autorização'));
      end;

      lContingencia.Visible   := NaoEstaVazio(Trim(lContingencia.Lines.Text));
      lMensagemFiscal.Visible := NaoEstaVazio(Trim(lMensagemFiscal.Lines.Text));

      lNumeroSerie.Caption := ACBrStr(
        'NFC-e nº ' + IntToStrZero(Ide.nNF, 9) + ' ' +
        'Série ' + IntToStrZero(Ide.serie, 3) + ' ' +
        DateTimeToStr(Ide.dEmi)+Via );
    end;

    lTitConsulteChave.Lines.Text := ACBrStr('Consulte pela Chave de Acesso em');

    lURLConsulta.Lines.Text := TACBrNFe(fACBrNFeDANFCeFortes.ACBrNFe).GetURLConsultaNFCe(Ide.cUF, Ide.tpAmb);
    ACBrNFeDANFCeFortes.DiminuirFonteSeNecessario(lURLConsulta, 5);

    lChaveDeAcesso.Lines.Text := FormatarChaveAcesso(OnlyNumber(infNFe.ID));

    if (Ide.tpEmis = teNormal ) and (procNFe.cStat = 0) then
    begin
      lChaveDeAcesso.Lines.Text := ACBrStr('NFC-E NÃO ENVIADA PARA SEFAZ');
      lChaveDeAcesso.Font.Color := clRed;
    end;

    lCancelada.Visible := ACBrNFeDANFCeFortes.NFeCancelada;
    if ACBrNFeDANFCeFortes.NFeCancelada then
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
      if ImprimirTributos then
      begin
        if TributosSeparadamente and ((vTribFed+vTribEst+vTribMun) > 0) then
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

    for I := 0 to InfAdic.obsCont.Count - 1 do
    begin
      lObservacoes.Lines.Add( StringReplace( InfAdic.obsCont[i].xCampo + ': ' +
                                             InfAdic.obsCont[i].xTexto, ';', #13, [rfReplaceAll] ) ) ;
    end;

    if InfAdic.infCpl <> '' then
      lObservacoes.Lines.Add( StringReplace( InfAdic.infCpl, ';', #13, [rfReplaceAll] ) );

    lObservacoes.Visible := ( lObservacoes.Lines.Count > 0 );
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
       TACBrNFe(fACBrNFeDANFCeFortes.ACBrNFe).GetURLConsultaNFCe(Ide.cUF, Ide.tpAmb));
    ACBrNFeDANFCeFortes.DiminuirFonteSeNecessario(lTitConsulteChaveCanc, 5);

    lChaveDeAcessoCanc.Caption := FormatarChaveAcesso(OnlyNumber(infNFe.ID));

    if procNFe.cStat = 0 then
    begin
      lChaveDeAcessoCanc.Caption    := ACBrStr('NFC-E NÃO ENVIADA PARA SEFAZ');
      lChaveDeAcessoCanc.Font.Color := clRed;
    end;

    if ACBrNFeDANFCeFortes.NFeCancelada then
      lCanceladaCanc.Caption := ACBrStr('NFC-e CANCELADA');
  end;
end;

procedure TACBrNFeDANFCeFortesFr.rlbOutroItemBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
var
  vAcrescimos: Double;
begin
  with ACBrNFeDANFCeFortes.FpNFe.Det.Items[fNumItem] do
  begin
    vAcrescimos := Prod.vFrete + Prod.vSeg + Prod.vOutro;
    PrintIt := (not Resumido) and (vAcrescimos > 0) and (ACBrNFeDANFCeFortes.ImprimeDescAcrescItem);

    if PrintIt then
    begin
      lOutro.Caption       := FormatFloatBr(vAcrescimos,'+,0.00');
      lOutroValLiq.Caption := FormatFloatBr(Prod.vProd+vAcrescimos-Prod.vDesc);
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

procedure TACBrNFeDANFCeFortesFr.PintarQRCode(const QRCodeData: String; APict: TPicture);
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

procedure TACBrNFeDANFCeFortesFr.rlVendaBeforePrint(Sender: TObject;
  var PrintIt: boolean);
var
  qrcode: String;
  LogoStream: TStringStream;
begin
  fNumItem  := 0;
  fNumPagto := 0;
  fTotalPagto := 0;
  fNumObs   := 0;
  fObsFisco.Clear;

  imgLogo.Height := 70;
  with ACBrNFeDANFCeFortes.FpNFe do
  begin
    lNomeFantasia.Visible := ACBrNFeDANFCeFortes.ImprimeNomeFantasia;
    if lNomeFantasia.Visible then;
      lNomeFantasia.Lines.Text:= Emit.xFant ;

    lRazaoSocial.Lines.Text := 'CNPJ: '+FormatarCNPJ(Emit.CNPJCPF)+' '+Emit.xNome ;
    lEndereco.Lines.Text    := CompoemEnderecoCFe;

    if ACBrNFeDANFCeFortes.ImprimeLogoLateral then
      imgLogo.Parent := pLogoLateral
    else
      imgLogo.Parent := pLogoeCliche;
    if ACBrNFeDANFCeFortes.Logo <> '' then
    begin
      imgLogo.Height := ACBrNFeDANFCeFortes.TamanhoLogoHeight ;
      imgLogo.Width := ACBrNFeDANFCeFortes.TamanhoLogoWidth ;
      imgLogo.AutoSize := ACBrNFeDANFCeFortes.ExpandirLogoMarca ;

      if (imgLogo.Width <= 0) or (imgLogo.Height <= 0) then
        imgLogo.AutoSize := True;

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
                                     signature.DigestValue,
                                     infNFe.Versao)
    else
      qrcode := infNFeSupl.qrCode;

    if ACBrNFeDANFCeFortes.QRCodeLateral then
    begin
      rlbConsumidor.Visible := False;
      rlbQRCode.Visible     := False;
      rlbQRLateral.Visible  := True;
      PintarQRCode( qrcode, imgQRCodeLateral.Picture );

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
          lDataAutorizacao1.Lines.Text := ACBrStr('Data de Autorização '+DateTimeToStr(procNFe.dhRecbto));
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
      PintarQRCode( qrcode, imgQRCode.Picture );

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
          lDataAutorizacao.Caption := ACBrStr('Data de Autorização '+DateTimeToStr(procNFe.dhRecbto));
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
  vAcrescimos : Double;
begin
  with ACBrNFeDANFCeFortes.FpNFe.Det.Items[fNumItem] do
  begin
    PrintIt := (not Resumido) and
               (ACBrNFeDANFCeFortes.ImprimeDescAcrescItem) and
               ((Prod.vDesc > 0)  or (Prod.vFrete + Prod.vSeg + Prod.vOutro >0));

    if PrintIt then
    begin
      lDesconto.Caption := FormatFloatBr(Prod.vDesc,'-,0.00');
      vAcrescimos       := Prod.vFrete + Prod.vSeg + Prod.vOutro;
      if (vAcrescimos > 0) then
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
        lDescValLiq.Caption := FormatFloatBr(Prod.vProd+vAcrescimos-Prod.vDesc);
      end;
    end;
  end;
end;

procedure TACBrNFeDANFCeFortesFr.rlbDetItemBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
var
  LinhaItem, LinhaTotal, sDescricao : string;
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
      maxCaracter := ACBrNFeDANFCeFortes.CalcularCaractesWidth( BMP.Canvas, mLinhaItem.Width);
    finally
      BMP.Free;
    end;
  {$ELSE}
    maxCaracter := ACBrNFeDANFCeFortes.CalcularCaractesWidth( mLinhaItem.Canvas, mLinhaItem.Width);
  {$ENDIF}
  with ACBrNFeDANFCeFortes.FpNFe.Det.Items[fNumItem] do
  begin
    if ACBrNFeDANFCeFortes.ImprimeEmUmaLinha then
    begin
      LinhaItem := IntToStrZero(Prod.nItem,3) + ' ' + ACBrNFeDANFCeFortes.ManterCodigo( Prod.cEAN , Prod.cProd ) +
                   ' ' + '[DesProd] ' + ACBrNFeDANFCeFortes.FormatQuantidade( Prod.QCom, False ) + ' ' +
                   Trim( Prod.uCom) + ' X ' +  ACBrNFeDANFCeFortes.FormatValorUnitario( Prod.VUnCom ) + ' ' +
                   FormatFloatBr( Prod.vProd );

      // acerta tamanho da descrição
      nTamDescricao := maxCaracter - Length(LinhaItem) ;
      sDescricao := PadRight(Copy(Trim(Prod.xProd), 1, nTamDescricao), nTamDescricao);

      LinhaItem := StringReplace(LinhaItem, '[DesProd]', sDescricao, [rfReplaceAll]);

      mLinhaItem.Lines.Add(LinhaItem);
    end
    else
    begin
      LinhaItem := IntToStrZero(Prod.nItem,3) + ' ' +
                               ACBrNFeDANFCeFortes.ManterCodigo( Prod.cEAN , Prod.cProd ) + ' ' +
                               Trim(Prod.xProd);

      if Trim(infAdProd) <> '' then
        LinhaItem := LinhaItem + '-'+ StringReplace( infAdProd, ';',#13,[rfReplaceAll]);

      mLinhaItem.Lines.Add(LinhaItem);

      //Centraliza os valores. A fonte dos itens foi mudada para Courier New, Pois esta o espaço tem o mesmo tamanho dos demais caractere.
      LinhaTotal  := '|'+ACBrNFeDANFCeFortes.FormatQuantidade(Prod.qCom, False) +'|'+
                     Trim(Prod.uCom) + ' X ' +
                     ACBrNFeDANFCeFortes.FormatValorUnitario(Prod.vUnCom) +'|'+
                     FormatFloatBr(Prod.vProd) ;
      LinhaTotal  := PadSpace( ACBrStr(LinhaTotal), maxCaracter-9, '|') ;

      mLinhaItem.Lines.Add(LinhaTotal);
    end;
  end;
end;


procedure TACBrNFeDANFCeFortesFr.rlbPagamentoBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  with ACBrNFeDANFCeFortes.FpNFe.pag.Items[fNumPagto] do
  begin
    lMeioPagamento.Caption  := ACBrStr(FormaPagamentoToDescricao(tPag));
    lPagamento.Caption      := FormatFloatBr(vPag);
    fTotalPagto             := fTotalPagto + vPag;
  end;
end;

procedure TACBrNFeDANFCeFortesFr.rlbGapBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  PrintIt := not Resumido;
end;

procedure TACBrNFeDANFCeFortesFr.rlbTotalAcrescimoBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
var
  vAcrescimos: Double;
begin
  with ACBrNFeDANFCeFortes.FpNFe.Total do
  begin
    vAcrescimos := ICMSTot.vFrete + ICMSTot.vSeg + ICMSTot.vOutro;
  end;
  PrintIt := vAcrescimos > 0;

  if PrintIt then
    lTotalAcrescimo.Caption := '+' + FormatFloatBr(vAcrescimos);
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
var
  maxCaracter : Integer;
  {$IFNDEF FPC}
    BMP : TBitmap;
  {$ENDIF}
begin
  PrintIt := not Resumido;

  {$IFNDEF FPC}
    BMP:=TBitMap.Create;
    try
      BMP.Canvas.Font.Assign(mLinhaItem.Font);
    maxCaracter := ACBrNFeDANFCeFortes.CalcularCaractesWidth(BMP.Canvas, lLegendaItens.Width);
    finally
      BMP.Free;
    end;
  {$ELSE}
    maxCaracter := ACBrNFeDANFCeFortes.CalcularCaractesWidth(lLegendaItens.Canvas, lLegendaItens.Width);
  {$ENDIF}

  lLegendaItens.Caption := PadSpace(ACBrStr('#|Código|Descrição|Qtde|Un|Valor unit.|Valor total'),maxCaracter, '|');
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
    Text := fACBrNFeDANFCeFortes.Sistema + Space(3);
end;

procedure TACBrNFeDANFCeFortesFr.pAsteriscoBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  PrintIt := not Resumido;
end;

procedure TACBrNFeDANFCeFortesFr.pLogoeClicheBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  pLogoLateral.Visible := ACBrNFeDANFCeFortes.ImprimeLogoLateral;
  if not ACBrNFeDANFCeFortes.ImprimeLogoLateral then
    pLogoLateral.Width := 0;

{  if ACBrNFeDANFCeFortes.ImprimeLogoLateral then
  begin
    pLogoLateral.Visible := True;
    pLogoeCliche.Align := faClientTop;
  end
  else
  begin
    pLogoLateral.Visible := False;
    pLogoeCliche.Align := faTop;
  end;
 }
end;

procedure TACBrNFeDANFCeFortesFr.pLogoLateralBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
begin
  PrintIt := ACBrNFeDANFCeFortes.ImprimeLogoLateral;

  pLogoLateral.Visible := PrintIt;

  if PrintIt then
  begin
    pLogoLateral.Height := imgLogo.Height ;
    pLogoLateral.Width  := imgLogo.Width ;
    imgLogo.Parent := pLogoLateral;
  end
  else
    pLogoLateral.Width := 0;
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
begin
  PrintIt := ACBrNFeDANFCeFortes.FpNFe.Total.ICMSTot.vDesc > 0;

  if PrintIt then
    lTotalDesconto.Caption := '-' + FormatFloatBr(ACBrNFeDANFCeFortes.FpNFe.Total.ICMSTot.vDesc);
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
  LogoStream: TStringStream;
begin
  fNumItem  := 0;
  fNumPagto := 0;
  fTotalPagto := 0;
  fNumObs   := 0;
  fObsFisco.Clear;

  imgLogo.Height := 100;

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
                                     signature.DigestValue,
                                     infNFe.Versao)
    else
      qrcode := infNFeSupl.qrCode;

    PintarQRCode( qrcode , imgQRCodeCanc.Picture );

    lProtocoloCanc.Caption := ACBrStr('Protocolo de Autorização: '+procNFe.nProt+
                           ' '+ifthen(procNFe.dhRecbto<>0,DateTimeToStr(procNFe.dhRecbto),''));

  end;
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

procedure TACBrNFeDANFCeFortes.ImprimirDANFEResumidoPDF(NFE: TNFe);
begin
  AtribuirNFe(NFE);
  Imprimir(True, fiPDF);
end;

procedure TACBrNFeDANFCeFortes.Imprimir(const DanfeResumido: Boolean;
  const AFiltro: TACBrSATExtratoFiltro);
begin
  ImprimirInterno(False, DanfeResumido, AFiltro);
end;

procedure TACBrNFeDANFCeFortes.ImprimirCancelado(const DanfeResumido: Boolean;
  const AFiltro: TACBrSATExtratoFiltro);
begin
  ImprimirInterno(True, DanfeResumido, AFiltro);
end;

procedure TACBrNFeDANFCeFortes.ImprimirInterno(const Cancelado: Boolean;
  const DanfeResumido: Boolean; const AFiltro: TACBrSATExtratoFiltro);
var
  frACBrNFeDANFCeFortesFr: TACBrNFeDANFCeFortesFr;
  RLLayout: TRLReport;
  RLFiltro: TRLCustomSaveFilter;
  NFeID: String;
begin
  frACBrNFeDANFCeFortesFr := TACBrNFeDANFCeFortesFr.Create(Self);
  try
    with frACBrNFeDANFCeFortesFr do
    begin
      Filtro := AFiltro;
      if Cancelado then
        RLLayout := rlCancelamento
      else
        RLLayout := rlVenda;

      Resumido := DanfeResumido;
      RLPrinter.Copies := NumCopias ;

      if not EstaVazio(Impressora) then
        RLPrinter.PrinterName := Impressora;

      NFeID := OnlyNumber(ACBrNFeDANFCeFortes.FpNFe.infNFe.ID);

      RLLayout.JobTitle := NomeDocumento;
      if (RLLayout.JobTitle = '') then
        RLLayout.JobTitle := NFeID + IfThen(Cancelado, '-cancelado', '')+'-nfe.xml';

      RLLayout.ShowProgress := MostrarStatus;
      RLLayout.PrintDialog  := (not MostrarPreview) and EstaVazio(Impressora);

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
          RLFiltro.FileName := PathWithDelim(ACBrNFeDANFCeFortes.PathPDF) +
                               ChangeFileExt( RLLayout.JobTitle, '.pdf');
          RLFiltro.FilterPages( RLLayout.Pages );
          ACBrNFeDANFCeFortes.FPArquivoPDF := RLFiltro.FileName;
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

function TACBrNFeDANFCeFortes.CalcularCaractesWidth(Canvas : TCanvas; WidthTotal: Integer
  ): Integer;
var
  maxCaracter : Integer;
  LinhaAjustada : String;
begin
  maxCaracter := 1;
  LinhaAjustada := '*';

  while (Canvas.TextWidth(LinhaAjustada) < WidthTotal) do
  begin
    LinhaAjustada := LinhaAjustada + '*';
    maxCaracter := maxCaracter + 1;
  end;

  Result := maxCaracter-2;
end;

procedure TACBrNFeDANFCeFortes.DiminuirFonteSeNecessario(ARLMemo: TRLMemo;
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

{$ifdef FPC}
initialization
   {$I ACBrNFeDANFCeFortes.lrs}
{$endif}

end.
