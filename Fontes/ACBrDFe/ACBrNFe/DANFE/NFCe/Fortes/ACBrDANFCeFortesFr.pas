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
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ImprimirDANFE(NFE : TNFe = nil); override ;
    procedure ImprimirDANFEResumido(NFE : TNFe = nil); override ;
    procedure ImprimirDANFEPDF(NFE : TNFe = nil); override;
    procedure ImprimirDANFEResumidoPDF(NFE : TNFe = nil);override;
  published
  end ;

  { TACBrNFeDANFCeFortesFr }

  TACBrNFeDANFCeFortesFr = class(TForm)
    lChaveDeAcesso: TRLLabel;
    lCPF_CNPJ_ID: TRLMemo;
    lEnderecoConsumidor: TRLMemo;
    lMensagemFiscal: TRLLabel;
    lNumSerieEmissao: TRLLabel;
    lTitChaveAcesso: TRLLabel;
    lTitConsulteChave: TRLMemo;
    lTitConsumidor: TRLLabel;
    lQtdItens: TRLLabel;
    lQtdTotalItensVal: TRLLabel;
    lProtocolo: TRLLabel;
    lCPF_CNPJ1: TRLLabel;
    lMsgDANFCe: TRLLabel;
    lMsgDANFCe1: TRLLabel;
    lMsgDANFCe2: TRLLabel;
    lTitLei12741: TRLLabel;
    lTitLei12742: TRLLabel;
    lTitFormaPagto: TRLLabel;
    lTitValorPago: TRLLabel;
    lPagamento: TRLLabel;
    lMeioPagamento: TRLLabel;
    lTitTotal: TRLLabel;
    lTitTroco: TRLLabel;
    lConsultaQRCode: TRLLabel;
    lDesconto: TRLLabel;
    lOutro: TRLLabel;
    lDescValLiq: TRLLabel;
    lOutroValLiq: TRLLabel;
    lSequencia: TRLLabel;
    lTitDesconto: TRLLabel;
    lTitAcrescimo: TRLLabel;
    lTitDescValLiq: TRLLabel;
    lTitOutroValLiq: TRLLabel;
    lTotal: TRLLabel;
    lTroco: TRLLabel;
    lTotalItem: TRLLabel;
    lValLei12741: TRLLabel;
    mLinhaItem: TRLMemo;
    pGap8: TRLPanel;
    rlbDadosCliche: TRLBand;
    rlbLegenda: TRLBand;
    rlbConsumidor: TRLBand;
    rlbMensagemFiscal: TRLBand;
    rlbMsgDANFe: TRLBand;
    rlbLei12741: TRLBand;
    rlbPagamento: TRLBand;
    rlbGap: TRLBand;
    rlbTotal: TRLBand;
    rlbDescItem: TRLBand;
    rlbOutroItem: TRLBand;
    rlbDetItem: TRLBand;
    rlbRodape: TRLBand;
    rlbTroco: TRLBand;
    RLDraw10: TRLDraw;
    RLDraw12: TRLDraw;
    rlbsCabecalho: TRLSubDetail;
    RLDraw4: TRLDraw;
    RLDraw5: TRLDraw;
    pLogoeCliche: TRLPanel;
    RLDraw7: TRLDraw;
    rlVenda: TRLReport;
    RLDraw2: TRLDraw;
    RLDraw6: TRLDraw;
    RLHTMLFilter1: TRLHTMLFilter;
    imgQRCode: TRLImage;
    pGap05: TRLPanel;
    RLPDFFilter1: TRLPDFFilter;
    rlsbDetItem: TRLSubDetail;
    rlsbPagamentos: TRLSubDetail;
    rlbMensagemContribuinte: TRLBand;
    lMensagemContribuinte: TRLLabel;
    RLDraw3: TRLDraw;
    lObservacoes: TRLMemo;
    lRazaoSocial: TRLLabel;
    lEmitCNPJ_IE_IM: TRLLabel;
    lEndereco: TRLMemo;
    RLDraw1: TRLDraw;
    lNomeFantasia: TRLLabel;
    imgLogo: TRLImage;

    procedure FormDestroy(Sender: TObject);
    procedure pAsteriscoBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlbConsumidorBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlbMensagemFiscalBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlbsCabecalhoDataRecord(Sender: TObject; RecNo: integer;
      CopyNo: integer; var Eof: boolean; var RecordAction: TRLRecordAction);
    procedure rlbLegendaBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlbDescItemBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlbDetItemBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlbOutroItemBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlbPagamentoBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlbGapBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlbTotalBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlbTrocoBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlVendaBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure FormCreate(Sender: TObject);
    procedure rlVendaDataRecord(Sender: TObject; RecNo: integer;
      CopyNo: integer; var Eof: boolean; var RecordAction: TRLRecordAction);
    procedure rlbLei12741BeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure rlsbDetItemDataRecord(Sender: TObject; RecNo: integer;
      CopyNo: integer; var Eof: boolean; var RecordAction: TRLRecordAction);
    procedure rlsbPagamentosDataRecord(Sender: TObject; RecNo: integer;
      CopyNo: integer; var Eof: boolean; var RecordAction: TRLRecordAction);
    procedure rlbMensagemContribuinteBeforePrint(Sender: TObject;
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

  fACBrNFeDANFCeFortes := TACBrNFeDANFCeFortes(Owner) ;  // Link para o Pai
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

    if Ide.tpAmb = taHomologacao then
     begin
       lMensagemFiscal.Caption := ACBrStr( 'EMITIDA EM AMBIENTE DE HOMOLOGAÇÃO - SEM VALOR FISCAL');
     end
    else
     begin
       if Ide.tpEmis <> teNormal then
          lMensagemFiscal.Caption := ACBrStr('EMITIDA EM CONTINGÊNCIA')
       else
          lMensagemFiscal.Caption := ACBrStr('ÁREA DE MENSAGEM FISCAL');
     end;

    lNumSerieEmissao.Caption := ACBrStr('Número '+IntToStrZero(Ide.nNF,9)+
                                ' Série '+IntToStrZero(Ide.serie,3)+
                                ' Emissão '+DateTimeToStr(Ide.dEmi) );

    lTitConsulteChave.Lines.Text := ACBrStr('Consulte pela Chave de Acesso em '+
       TACBrNFe(fACBrNFeDANFCeFortes.ACBrNFe).GetURLConsultaNFCe(Ide.cUF,Ide.tpAmb));

    lChaveDeAcesso.Caption := FormatarChaveAcesso(OnlyNumber(infNFe.ID));
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
begin
  fNumItem  := 0;
  fNumPagto := 0;
  fTotalPagto := 0;
  fNumObs   := 0;
  fObsFisco.Clear;

  with ACBrNFeDANFCeFortes.FpNFe do
  begin
    lNomeFantasia.Caption   := Emit.xFant ;
    lRazaoSocial.Caption    := Emit.xNome ;
    lEmitCNPJ_IE_IM.Caption := CompoemCliche;
    lEndereco.Lines.Text    := CompoemEnderecoCFe;
    if ACBrNFeDANFCeFortes.Logo <> '' then
       imgLogo.Picture.LoadFromFile( ACBrNFeDANFCeFortes.Logo );

    // QRCode  //

    qrcode := TACBrNFe(ACBrNFeDANFCeFortes.ACBrNFe).GetURLQRCode( ide.cUF, ide.tpAmb,
                                     OnlyNumber(InfNFe.ID),  //correcao para pegar somente numeros, estava indo junto o NFE
                                     ifthen(Dest.idEstrangeiro <> '',Dest.idEstrangeiro, Dest.CNPJCPF),
                                     ide.dEmi,
                                     Total.ICMSTot.vNF, Total.ICMSTot.vICMS,
                                     signature.DigestValue);
    PintarQRCode( qrcode, imgQRCode.Picture );

    lProtocolo.Caption := ACBrStr('Protocolo de Autorização: '+procNFe.nProt+
                           ' '+ifthen(procNFe.dhRecbto<>0,DateTimeToStr(procNFe.dhRecbto),''));

  end;


  // Calculando o tamanho da PÃ¡gina em Pixels //
  TotalPaginaPixel := rlbsCabecalho.Height +
                      rlbRodape.Height +
                      rlbLegenda.Height +
                      rlbPagamento.Height +
                      rlbLei12741.Height +
                      rlbMensagemContribuinte.Height +
                      rlbMensagemFiscal.Height +
                      rlbConsumidor.Height +
                      Trunc( rlsbDetItem.Height * ACBrNFeDANFCeFortes.FpNFe.Det.Count );
  // Pixel para Milimitros //
  rlVenda.PageSetup.PaperHeight := max( 100, 10+Trunc( TotalPaginaPixel / 3.7 ));
end;

procedure TACBrNFeDANFCeFortesFr.rlbDetItemBeforePrint(Sender: TObject;
  var PrintIt: boolean);
var
  LinhaItem, Descricao: String;
begin
  PrintIt := not Resumido;
  if not PrintIt then exit;

  with ACBrNFeDANFCeFortes.FpNFe.Det.Items[fNumItem] do
  begin
    lSequencia.Caption := IntToStrZero(Prod.nItem,3);

    Descricao := ACBrStrToAnsi( Trim(Prod.xProd) );
    LinhaItem := Trim(Prod.cProd)+' '+
                 Descricao+' '+
                 FormatFloatBr(Prod.qCom, ACBrNFeDANFCeFortes.CasasDecimais._Mask_qCom)+' '+
                 Trim(Prod.uCom)+' X '+
                 FormatFloatBr(Prod.vUnCom, ACBrNFeDANFCeFortes.CasasDecimais._Mask_vUnCom)+' ';


    if Imposto.vTotTrib > 0 then
      LinhaItem := LinhaItem + '('+FormatFloatBr(Imposto.vTotTrib,'0.00')+')* ';

    mLinhaItem.Lines.Text := LinhaItem;
    lTotalItem.Caption    := FormatFloatBr(Prod.vProd,'#,###,##0.00');
  end;
end;

procedure TACBrNFeDANFCeFortesFr.rlbDescItemBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  with ACBrNFeDANFCeFortes.FpNFe.Det.Items[fNumItem] do
  begin
    PrintIt := (not Resumido) and (Prod.vDesc > 0) ;

    if PrintIt then
    begin
      lDesconto.Caption   := FormatFloatBr(Prod.vDesc,'-#,###,##0.00');
      lDescValLiq.Caption := FormatFloatBr(Prod.vProd-Prod.vDesc,'#,###,##0.00');
    end;
  end;
end;

procedure TACBrNFeDANFCeFortesFr.rlbOutroItemBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  with ACBrNFeDANFCeFortes.FpNFe.Det.Items[fNumItem] do
  begin
    PrintIt := (not Resumido) and (Prod.vOutro > 0);

    if PrintIt then
    begin
      lOutro.Caption       := FormatFloatBr(Prod.vOutro,'+#,###,##0.00');
      lOutroValLiq.Caption := FormatFloatBr(Prod.vProd+Prod.vOutro,'#,###,##0.00');
    end;
  end;

end;

procedure TACBrNFeDANFCeFortesFr.rlbPagamentoBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  with ACBrNFeDANFCeFortes.FpNFe.pag.Items[fNumPagto] do
  begin
    lMeioPagamento.Caption := ACBrStr(FormaPagamentoToDescricao(tPag));
    lPagamento.Caption     := FormatFloatBr(vPag,'#,###,##0.00');
    fTotalPagto := fTotalPagto + vPag;
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
        lCPF_CNPJ_ID.Lines.Text := 'CONSUMIDOR NÃO IDENTIFICADO';
     end
    else if Dest.idEstrangeiro <> '' then
     begin
       lCPF_CNPJ_ID.Lines.Text  := 'CNPJ/CPF/ID Estrangeiro -'+Dest.idEstrangeiro+' '+Dest.xNome;
     end
    else
     begin
       if Length(trim(Dest.CNPJCPF)) > 11 then
          lCPF_CNPJ_ID.Lines.Text  := 'CNPJ/CPF/ID Estrangeiro -'+FormatarCNPJ(Dest.CNPJCPF)
       else
          lCPF_CNPJ_ID.Lines.Text  := 'CNPJ/CPF/ID Estrangeiro -'+FormatarCPF(Dest.CNPJCPF);

       lCPF_CNPJ_ID.Lines.Text  := lCPF_CNPJ_ID.Caption+' '+Dest.xNome;
     end;
     lEnderecoConsumidor.Lines.Text := Trim(Dest.EnderDest.xLgr)+' '+
                                       Trim(Dest.EnderDest.nro)+' '+
                                       Trim(Dest.EnderDest.xCpl)+' '+
                                       Trim(Dest.EnderDest.xBairro)+' '+
                                       Trim(Dest.EnderDest.xMun);
  end;
end;

procedure TACBrNFeDANFCeFortesFr.rlbLei12741BeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  with ACBrNFeDANFCeFortes.FpNFe do
  begin
    PrintIt := (Total.ICMSTot.vTotTrib > 0);

    if PrintIt then
      lValLei12741.Caption := FormatFloatBr(Total.ICMSTot.vTotTrib, '#,###,##0.00');
  end;
end;

procedure TACBrNFeDANFCeFortesFr.rlbMensagemContribuinteBeforePrint(
  Sender: TObject; var PrintIt: Boolean);
begin

  Printit := False;

  with ACBrNFeDANFCeFortes.FpNFe.InfAdic do
  begin
    if infCpl <> '' then
    begin
      PrintIt := True ;

      lObservacoes.Lines.Add( StringReplace( infCpl, ';', #13, [rfReplaceAll] ) );
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
  Troco : Currency;
begin
  with ACBrNFeDANFCeFortes.FpNFe do
  begin
    Troco := fTotalPagto - Total.ICMSTot.vNF;

    PrintIt := (Troco > 0);

    if PrintIt then
      lTroco.Caption := FormatFloatBr(Troco,'#,###,##0.00');;
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
  if NFe = nil then
   begin
     if not Assigned(ACBrNFe) then
        raise Exception.Create('Componente ACBrNFe nÃo atribuí­do');

     FpNFe := TACBrNFe(ACBrNFe).NotasFiscais.Items[0].NFe;
   end
  else
    FpNFe := NFE;

  Imprimir(False);
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
  {$IFDEF FPC}
   LoadPortugueseStrings;
  {$ELSE}
   // Evitando mensagem de versão do fortes //
   SetVersion( CommercialVersion, ReleaseVersion, CommentVersion );
  {$ENDIF}

  frACBrNFeDANFCeFortesFr := TACBrNFeDANFCeFortesFr.Create(Self);
  try
    with frACBrNFeDANFCeFortesFr do
    begin
      Filtro := AFiltro;
      RLLayout := rlVenda;
      Resumido := DanfeResumido;

      RLPrinter.Copies     := NumCopias ;
      
      if ACBrNFeDANFCeFortes.Impressora <> '' then
        RLPrinter.PrinterName := ACBrNFeDANFCeFortes.Impressora;

      RLLayout.PrintDialog := ACBrNFeDANFCeFortes.MostrarPreview;
      RLLayout.ShowProgress:= False ;

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

          {$IFDEF FPC}
            RLFiltro.Copies := NumCopias ;
          {$ENDIF}
          RLFiltro.ShowProgress := ACBrNFeDANFCeFortes.MostrarStatus;
          //RLFiltro.FileName := NomeArquivo ;
          RLFiltro.FileName := ACBrNFeDANFCeFortes.PathPDF +
                               OnlyNumber(ACBrNFeDANFCeFortes.FpNFe.infNFe.ID) + '-nfe.pdf';
          {$IFDEF FPC}
            RLFiltro.Pages := RLLayout.Pages ;
            RLFiltro.FirstPage := 1;
            RLFiltro.LastPage := RLLayout.Pages.PageCount;
            RLFiltro.Run;
            //RLLayout.SaveToFile(PathPDF +
                               //Copy(TACBrNFe(ACBrNFe).NotasFiscais.Items[0].NFe.infNFe.ID, 4, 44) + '.pdf');
          {$ELSE}
            RLFiltro.FilterPages( RLLayout.Pages );
          {$ENDIF}
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
