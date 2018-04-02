{******************************************************************************}
{ Projeto: Componente ACBrNFe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Nota Fiscal}
{ eletrônica - NFe - http://www.nfe.fazenda.gov.br                             }
{                                                                              }
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
| Historico
|
******************************************************************************}
{$I ACBr.inc}

unit ACBrNFeDANFeRLSimplificado;

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, RLReport, RLPDFFilter, RLBarcode, ACBrNFeDANFeRL,
  pcnConversao, DB, RLFilters;

type

  { TfrlDANFeRLSimplificado }

  TfrlDANFeRLSimplificado = class(TfrlDANFeRL)
    RLb02_Emitente: TRLBand;
    RLb03_DadosGerais: TRLBand;
    RLb04_Destinatario: TRLBand;
    RLb05c_Lin_Itens: TRLBand;
    RLiLogo: TRLImage;
    RLLabel1: TRLLabel;
    RLLabel27: TRLLabel;
    RLLabel9: TRLLabel;
    RLlChave: TRLLabel;
    RLlDescricao: TRLLabel;
    RLlEmissao: TRLLabel;
    RLlEntradaSaida: TRLLabel;
    RLlMsgTipoEmissao: TRLLabel;
    RLlProtocolo: TRLLabel;
    RLlTipoEmissao: TRLLabel;
    RLmDestinatario: TRLMemo;
    RLmEmitente: TRLMemo;
    RLShape102: TRLDraw;
    RLShape68: TRLDraw;
    rlb01_Chave: TRLBand;
    RLBarcode1: TRLBarcode;
    RLLabel17: TRLLabel;
    lblNumero: TRLLabel;
    subItens: TRLSubDetail;
    rlb05a_Cab_Itens: TRLBand;
    RLLabel2: TRLLabel;
    RLLabel4: TRLLabel;
    RLLabel5: TRLLabel;
    RLLabel6: TRLLabel;
    RLLabel7: TRLLabel;
    RLLabel8: TRLLabel;
    RLLabel3: TRLLabel;
    rlb05b_Desc_Itens: TRLBand;
    rlmProdutoCodigo: TRLLabel;
    rlmProdutoDescricao: TRLLabel;
    rlmProdutoUnidade: TRLLabel;
    rlmProdutoQTDE: TRLLabel;
    rlmProdutoValor: TRLLabel;
    rlmProdutoTotal: TRLLabel;
    rlmProdutoItem: TRLLabel;
    rlb06a_Totais: TRLBand;
    rlmPagDesc: TRLMemo;
    rlmPagValor: TRLMemo;
    rlb06b_Tributos: TRLBand;
    rllTributos: TRLLabel;
    procedure RLb02_EmitenteBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure RLb03_DadosGeraisBeforePrint(Sender: TObject; var PrintIt: boolean
      );
    procedure RLb04_DestinatarioBeforePrint(Sender: TObject;
      var PrintIt: boolean);
    procedure RLb06a_TotaisBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure RLb06b_TributosBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure RLNFeBeforePrint(Sender: TObject;
      var PrintReport: Boolean);
    procedure rlb01_ChaveBeforePrint(Sender: TObject;
      var PrintBand: Boolean);
    procedure subItensDataRecord(Sender: TObject; RecNo, CopyNo: Integer;
      var Eof: Boolean; var RecordAction: TRLRecordAction);
    procedure rlb05b_Desc_ItensBeforePrint(Sender: TObject;
      var PrintIt: Boolean);
    procedure RLNFeDataRecord(Sender: TObject; RecNo, CopyNo: Integer;
      var Eof: Boolean; var RecordAction: TRLRecordAction);
  private
    { Private declarations }
    FNumItem    : Integer;
    FTotalPages : Integer;
  public
    { Public declarations }
    procedure ProtocoloNFE( const sProtocolo : String );
  end;

implementation

uses
 StrUtils, DateUtils,
 ACBrUtil, ACBrValidador, ACBrDFeUtil,
 pcnNFe, pcnConversaoNFe, ACBrNFeDANFeRLClass;

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

const
   _NUM_ITEMS_PAGE1      = 18;
   _NUM_ITEMS_OTHERPAGES = 50;

procedure TfrlDANFeRLSimplificado.RLNFeBeforePrint(Sender: TObject;
  var PrintReport: Boolean);
var
 nRestItens: Integer;
begin
  inherited;

  FTotalPages   := 1;

  if ( FNFe.Det.Count > _NUM_ITEMS_PAGE1 ) then
  begin
    nRestItens := FNFe.Det.Count - _NUM_ITEMS_PAGE1;
    if nRestItens <= _NUM_ITEMS_OTHERPAGES then
      Inc( FTotalPages )
    else
    begin
      Inc( FTotalPages, nRestItens div _NUM_ITEMS_OTHERPAGES );
      if ( nRestItens mod _NUM_ITEMS_OTHERPAGES ) > 0 then
        Inc( FTotalPages )
    end;
  end;

  RLNFe.Title:='NF-e: ' + FormatFloat( '000,000,000', FNFe.Ide.nNF );

  with RLNFe.Margins do
  begin
    TopMargin    := FMargemSuperior * 10;
    BottomMargin := FMargemInferior * 10;
    LeftMargin   := FMargemEsquerda * 10;
    RightMargin  := FMargemDireita  * 10;
  end;
end;

procedure TfrlDANFeRLSimplificado.RLb02_EmitenteBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  inherited;

  if FExpandirLogoMarca then
  begin
    rliLogo.top         := 13;
    rliLogo.Left        := 2;
    rliLogo.Height      := 108;
    rliLogo.Width       := 284;
    rliLogo.Stretch     := True;
    rlmEmitente.Enabled := False;
  end;

  if (FLogo <> '') and FilesExists(FLogo) then
    rliLogo.Picture.LoadFromFile(FLogo);

  if not FExpandirLogoMarca then
  begin
    rlmEmitente.Enabled := True;
    rlmEmitente.Lines.Clear;
    with FNFe.Emit do
    begin
      rlmEmitente.Lines.Add(TACBrNFeDANFeRL(Owner).ManterNomeImpresso( XNome , XFant ));
      with EnderEmit do
      begin
        rlmEmitente.Lines.Add(XLgr + IfThen(Nro = '0', '', ', ' + Nro) +
                              IfThen(XCpl = '', '', ', ' + XCpl) +
                              IfThen(XBairro = '', '', ', ' + XBairro) +
                              ', ' + XMun + '/ ' + UF);
      end;
      rlmEmitente.Lines.Add('CNPJ: ' + FormatarCNPJouCPF(CNPJCPF) +
                            ' IE: '+ IE);
    end;
  end;
end;

procedure TfrlDANFeRLSimplificado.RLb03_DadosGeraisBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  // Contingencia ********************************************************
  if FNFe.Ide.tpEmis in [teContingencia, teFSDA] then
    rllTipoEmissao.Caption := 'CONTINGENCIA FS-DA';

  rllEntradaSaida.Caption := tpNFToStr( FNFe.Ide.tpNF );

  lblNumero.Caption := ACBrStr('Número: ' + FormatFloat('000,000,000', FNFe.Ide.nNF) +
                       ' - Série: '+ FormatFloat('000', FNFe.Ide.serie));

  rllEmissao.Caption := ACBrStr('Emissão: ' + FormatDateTimeBr(FNFe.Ide.dEmi));
end;

procedure TfrlDANFeRLSimplificado.RLb04_DestinatarioBeforePrint(
  Sender: TObject; var PrintIt: boolean);
begin
  inherited;

  rlmDestinatario.Lines.Clear;
  with FNFe.Dest do
  begin
    rlmDestinatario.Lines.Add(XNome);
    with EnderDest do
    begin
      rlmDestinatario.Lines.Add(XLgr + IfThen(Nro = '0', '', ', ' + Nro) +
                            IfThen(XCpl = '', '', ', ' + XCpl) +
                            IfThen(XBairro = '', '', ', ' + XBairro) +
                            ', ' + XMun + '/ ' + UF);
    end;
    rlmDestinatario.Lines.Add(ACBrStr('CPF/CNPJ: ' + FormatarCNPJouCPF(CNPJCPF) +
                              ' IE: ' + IE));
  end;
  rllMsgTipoEmissao.Visible := False;
  if FNFe.Ide.tpAmb = taHomologacao then
  begin
     rllMsgTipoEmissao.Caption := ACBrStr('HOMOLOGAÇÂO - SEM VALOR FISCAL');
     rllMsgTipoEmissao.Enabled := True;
     rllMsgTipoEmissao.Visible := True;
  end;

  if FNFe.procNFe.cStat > 0 then
  begin
     if ((FNFe.procNFe.cStat in [101, 151, 155]) or (FNFeCancelada)) then
     begin
       rllMsgTipoEmissao.Caption := 'NF-e CANCELADA';
       rllMsgTipoEmissao.Visible := True;
       rllMsgTipoEmissao.Enabled := True;
     end;
     if FNFe.procNFe.cStat = 110 then
     begin
       rllMsgTipoEmissao.Caption := 'NF-e DENEGADA';
       rllMsgTipoEmissao.Visible := True;
       rllMsgTipoEmissao.Enabled := True;
     end;
     if not FNFe.procNFe.cStat in [100, 101, 110, 151, 155] then
     begin
       rllMsgTipoEmissao.Caption := FNFe.procNFe.xMotivo;
       rllMsgTipoEmissao.Visible := True;
       rllMsgTipoEmissao.Enabled := True;
     end;
  end;

  case FNFe.Ide.tpEmis of
    teContingencia : begin
        rllMsgTipoEmissao.Caption := ACBrStr('DANFE em Contingencia - impresso em decorrencia de problemas tecnicos');
        rllMsgTipoEmissao.Visible := True;
        rllMsgTipoEmissao.Enabled := True;
      end;
    teFSDA : begin
        rllMsgTipoEmissao.Caption := ACBrStr('DANFE em Contingencia - impresso em decorrencia de problemas tecnicos');
        rllMsgTipoEmissao.Visible := True;
        rllMsgTipoEmissao.Enabled := True;
      end;
  end;

 rllMsgTipoEmissao.Repaint;
end;


procedure TfrlDANFeRLSimplificado.RLb06a_TotaisBeforePrint(Sender: TObject;
  var PrintIt: boolean);
begin
  inherited;

  rlmPagDesc.Lines.Clear;
  rlmPagValor.Lines.Clear;
  rlmPagDesc.Lines.Add('Qtde Total de Itens');
  rlmPagValor.Lines.Add(IntToStr(FNFe.Det.Count));
  rlmPagDesc.Lines.Add('Valor Total');
  rlmPagValor.Lines.Add( FormatFloat('###,###,###,##0.00', FNFE.Total.ICMSTot.vNF));
end;

procedure TfrlDANFeRLSimplificado.RLb06b_TributosBeforePrint(Sender: TObject;
  var PrintIt: boolean);
var
 Perc: Double;
begin
  inherited;
  Perc := 0;
  if FNFE.Total.ICMSTot.vNF > 0 then
    Perc := (FNFE.Total.ICMSTot.vTotTrib / FNFE.Total.ICMSTot.vNF) * 100;
  rllTributos.Caption := ACBrStr('Valor aprox. dos tributos: ' +
                         FormatFloatBr(FNFE.Total.ICMSTot.vTotTrib) +
                         '(' + FormatFloatBr(Perc) + '%)(Fonte: IBPT)');
end;

procedure TfrlDANFeRLSimplificado.ProtocoloNFE( const sProtocolo : String );
begin
  FProtocoloNFE := sProtocolo;
end;

procedure TfrlDANFeRLSimplificado.rlb01_ChaveBeforePrint(Sender: TObject;
  var PrintBand: Boolean);
begin
  inherited;

  PrintBand := RLNFe.PageNumber = 1;

  RLBarcode1.Caption := FNFe.InfNFe.Id;

  rllChave.Caption := FormatarChaveAcesso(FNFe.InfNFe.Id);

  // Normal **************************************************************
  if FNFe.Ide.tpEmis in [teNormal, teSCAN] then
  begin
    if FNFe.procNFe.cStat = 100 then
      rllDescricao.Caption := ACBrStr('Protocolo de Autorização');

    if FNFe.procNFe.cStat in [101, 151, 155] then
      rllDescricao.Caption:= ACBrStr('Protocolo de Homologação de Cancelamento');

    if FNFe.procNFe.cStat = 110 then
      rllDescricao.Caption:= ACBrStr('Protocolo de Denegação de Uso');
  end;

  if FProtocoloNFE <> '' then
    rllProtocolo.Caption := FProtocoloNFE
  else
    rllProtocolo.Caption := FNFe.procNFe.nProt + ' ' +
                              IfThen(FNFe.procNFe.dhRecbto <> 0, DateTimeToStr(FNFe.procNFe.dhRecbto), '');

end;


procedure TfrlDANFeRLSimplificado.rlb05b_Desc_ItensBeforePrint(Sender: TObject;
  var PrintIt: Boolean);
  Function ManterinfAdProd(sXProd: String; sinfAdProd :String ) : String;
  begin
    Result := sXProd;
    if sinfAdProd <> '' then
      Result := Result + #13+#13 + 'InfAd: ' + sinfAdProd;
  end;
begin
  inherited;
  with FNFe.Det.Items[FNumItem] do
  begin
    rlmProdutoItem.caption      := FormatFloat('000', FNumItem+1 );
    rlmProdutoCodigo.caption    := TACBrNFeDANFeRL(Owner).ManterCodigo(Prod.cEAN, Prod.CProd);
    rlmProdutoDescricao.caption := ManterinfAdProd(Prod.XProd, infAdProd );
    rlmProdutoQTDE.caption      := TACBrNFeDANFeRL(Owner).FormatQuantidade( Prod.qCom);
    rlmProdutoValor.caption     := TACBrNFeDANFeRL(Owner).FormatValorUnitario(  Prod.vUnCom);
    rlmProdutoUnidade.caption   := Prod.UCom;
    rlmProdutoTotal.caption     := FormatFloatBr(Prod.vProd);
  end;
end;

procedure TfrlDANFeRLSimplificado.RLNFeDataRecord(Sender: TObject; RecNo,
  CopyNo: Integer; var Eof: Boolean; var RecordAction: TRLRecordAction);
begin
  inherited;
  Eof := (RecNo > 1);
  RecordAction := raUseIt;
end;

procedure TfrlDANFeRLSimplificado.subItensDataRecord(Sender: TObject; RecNo,
  CopyNo: Integer; var Eof: Boolean; var RecordAction: TRLRecordAction);
begin
  inherited;
  FNumItem      := RecNo - 1 ;
  Eof           := (RecNo > FNFe.Det.Count) ;
  RecordAction  := raUseIt ;
end;




end.
