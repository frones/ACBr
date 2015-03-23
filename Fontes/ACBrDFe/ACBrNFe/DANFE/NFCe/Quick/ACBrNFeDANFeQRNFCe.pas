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

{*******************************************************************************
| Historico
|
|  30/09/2014 : AltC
|   - Alterado para o componente QRMemo a fim de armazenar os dados do
|     Consumidor
*******************************************************************************}

{$I ACBr.inc}

unit ACBrNFeDANFeQRNFCe;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, QuickRpt, QRCtrls,  XMLIntf, XMLDoc,
  JPEG, ACBrNFeDANFeQR, ACBrNFeQRCodeBar, pcnConversao, DB,
  {$IFDEF QReport_PDF}
     QRPDFFilt,
  {$ENDIF}
  DBClient, ACBrNFeDANFEClass, ACBrNFeDANFeQRClass,
  ACBrDelphiZXingQRCode;

type

  TfqrDANFeQRNFCe = class(TfqrDANFeQR)
    cdsItens: TClientDataSet;
    cdsItensCODIGO: TStringField;
    cdsItensDESCRICAO: TStringField;
    cdsItensNCM: TStringField;
    cdsItensCFOP: TStringField;
    cdsItensUNIDADE: TStringField;
    cdsItensQTDE: TStringField;
    cdsItensVALOR: TStringField;
    cdsItensTOTAL: TStringField;
    cdsItensCST: TStringField;
    cdsItensBICMS: TStringField;
    cdsItensALIQICMS: TStringField;
    cdsItensVALORICMS: TStringField;
    cdsItensALIQIPI: TStringField;
    cdsItensVALORIPI: TStringField;
    qrb01_Emitente: TQRBand;
    qrb02_DadosFixosDanfe: TQRChildBand;
    qrb03a_Cab_Itens: TQRBand;
    qrb04_Totais: TQRBand;
    qrb07_Consumidor: TQRChildBand;
    QRLabel17: TQRLabel;
    qrlURLSefaz: TQRLabel;
    QRLabel19: TQRLabel;
    QRLabel142: TQRLabel;
    QRLabel143: TQRLabel;
    QRLabel147: TQRLabel;
    QRLabel148: TQRLabel;
    QRLabel149: TQRLabel;
    QRLabel150: TQRLabel;
    qrb03b_Desc_Itens: TQRBand;
    qrmProdutoCodigo: TQRDBText;
    qrmProdutoDescricao: TQRDBText;
    qrmProdutoUnidade: TQRDBText;
    qrmProdutoQTDE: TQRDBText;
    qrmProdutoValor: TQRDBText;
    qrmProdutoTotal: TQRDBText;
    qrb03c_Lin_Itens: TQRChildBand;
    QRShape68: TQRShape;
    cdsItensXPROD: TStringField;
    cdsItensINFADIPROD: TStringField;
    cdsItensCSOSN: TStringField;
    qriLogo: TQRImage;
    qrmEmitente: TQRMemo;
    qrb08_QRCode: TQRChildBand;
    qrb05_Tributos: TQRChildBand;
    qrlTributos: TQRLabel;
    qrb06_Chave: TQRChildBand;
    QRLabel9: TQRLabel;
    qrlChave: TQRLabel;
    qrlTipoEmissao: TQRLabel;
    lblNumero: TQRLabel;
    qrlSiteConsulta: TQRLabel;
    QRLabel10: TQRLabel;
    qrmPagDesc: TQRMemo;
    qrmPagValor: TQRMemo;
    QRLabel1: TQRLabel;
    qrlDescricao: TQRLabel;
    qrlProtocolo: TQRLabel;
    QRLabel2: TQRLabel;
    qriQRCode: TQRImage;
    qrlEmissao: TQRLabel;
    QRLabel3: TQRLabel;
    cdsItensITEM: TStringField;
    qrmProdutoItem: TQRDBText;
    qrb05a_InfComplementar: TQRChildBand;
    qrmInfComp: TQRMemo;
    qrlDestCNPJ: TQRMemo;
    procedure QRNFeBeforePrint(Sender: TCustomQuickRep;
      var PrintReport: Boolean);
    procedure qrmProdutoDescricaoPrint(sender: TObject; var Value: string);

    procedure qrb01_EmitenteBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrb02_DadosFixosDanfeBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrb03a_Cab_ItensBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrb03b_Desc_ItensBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrb03c_Lin_ItensBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrb04_TotaisBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrb05_TributosBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrb06_ChaveBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrb07_ConsumidorBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrb08_QRCodeBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrb05a_InfComplementarBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
  private
    { Private declarations }
    FTotalPages: Integer;
    TotalItens: Integer;
    procedure Itens;
  public
    { Public declarations }
    procedure ProtocoloNFE( const sProtocolo : String );
  end;


implementation

uses
 StrUtils, DateUtils, ACBrNFe,
 ACBrUtil, ACBrDFeUtil, pcnNFe;

{$R *.dfm}

const
   _NUM_ITEMS_PAGE1      = 18;
   _NUM_ITEMS_OTHERPAGES = 50;

var
   FProtocoloNFE: String;
   nItemControle: Integer;

procedure TfqrDANFeQRNFCe.QRNFeBeforePrint(Sender: TCustomQuickRep;
  var PrintReport: Boolean);
var
 nRestItens: Integer;
begin
  inherited;

  qrb03a_Cab_Itens.Enabled := FImprimeItens;
  qrb03b_Desc_Itens.Enabled := FImprimeItens;
  qrb03c_Lin_Itens.Enabled := FImprimeItens;

  Itens;
  nItemControle := 0;
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

  QRNFe.ReportTitle:='NF-e: ' + FormatFloat( '000,000,000', FNFe.Ide.nNF );

  QRNFe.Page.TopMargin    := FMargemSuperior * 100;
  QRNFe.Page.BottomMargin := FMargemInferior * 100;
  QRNFe.Page.LeftMargin   := FMargemEsquerda * 100;
  QRNFe.Page.RightMargin  := FMargemDireita  * 100;
end;

procedure TfqrDANFeQRNFCe.qrmProdutoDescricaoPrint(sender: TObject;
  var Value: string);
var
 intTamanhoDescricao,
 intTamanhoAdicional,
 intTamanhoLinha,
 intDivisaoDescricao,
 intDivisaoAdicional,
 intResto: Integer;
begin
  inherited;

  intTamanhoDescricao := Length(cdsItens.FieldByName( 'DESCRICAO' ).AsString);
  intDivisaoAdicional := 0;
  if Length(cdsItens.FieldByName( 'INFADIPROD' ).AsString)>0
   then begin
    intTamanhoAdicional := Length('InfAdic: '+cdsItens.FieldByName( 'INFADIPROD' ).AsString);
    intDivisaoAdicional := intTamanhoAdicional DIV 35;
    intResto := intTamanhoAdicional - (intTamanhoAdicional DIV 35)*35;
    if intResto > 0
     then intDivisaoAdicional := intDivisaoAdicional + 1;
   end;

  intDivisaoDescricao := intTamanhoDescricao DIV 35;
  intResto := intTamanhoDescricao - (intTamanhoDescricao DIV 35)*35;
  if intResto>0
   then intDivisaoDescricao := intDivisaoDescricao + 1;

  //intTamanhoLinha:= 15 * (intDivisaoDescricao+intDivisaoAdicional);
  if (intTamanhoDescricao <= 35) and (cdsItens.FieldByName('INFADIPROD').AsString = '')
   then intTamanhoLinha := 12;
  if (intTamanhoDescricao <= 35) and (cdsItens.FieldByName('INFADIPROD').AsString <> '')
   then intTamanhoLinha := 22;

  if cdsItensINFADIPROD.AsString <> ''
   then Value := Value + #13 + 'InfAd: ' + cdsItensINFADIPROD.AsString;
end;

procedure TfqrDANFeQRNFCe.Itens;
var
 nItem: Integer;
 sCST, sBCICMS, sALIQICMS, sVALORICMS, sALIQIPI, sVALORIPI: String;
begin
  cdsItens.Close;
  cdsItens.CreateDataSet;
  cdsItens.Open;
  TotalItens := FNFe.Det.Count;

  for nItem := 0 to (FNFe.Det.Count - 1) do
    begin
      with FNFe.Det.Items[nItem] do
        begin
          with Prod do
            begin
              with Imposto.ICMS do
                begin
                  sALIQIPI   := '0,00';
                  sVALORIPI  := '0,00';

                  cdsItens.Append;
                  cdsItens.FieldByName('ITEM').AsString := FormatFloat('000', nItem );
                  cdsItens.FieldByName('CODIGO').AsString := CProd;
                  cdsItens.FieldByName('DESCRICAO').AsString := XProd;
                  cdsItens.FieldByName('INFADIPROD').AsString := infAdProd;
                  cdsItens.FieldByName('NCM').AsString := NCM;
                  cdsItens.FieldByName('CFOP').AsString := CFOP;

                  case FCasasDecimaisqCom of
                    0: cdsItens.FieldByName('QTDE').AsString := FormatFloat('###,###,###,##0', QCom);
                    1: cdsItens.FieldByName('QTDE').AsString := FormatFloat('###,###,###,##0.0', QCom);
                    2: cdsItens.FieldByName('QTDE').AsString := FormatFloat('###,###,###,##0.00', QCom);
                    3: cdsItens.FieldByName('QTDE').AsString := FormatFloat('###,###,###,##0.000', QCom);
                    4: cdsItens.FieldByName('QTDE').AsString := FormatFloat('###,###,###,##0.0000', QCom);
                    5: cdsItens.FieldByName('QTDE').AsString := FormatFloat('###,###,###,##0.00000', QCom);
                    6: cdsItens.FieldByName('QTDE').AsString := FormatFloat('###,###,###,##0.000000', QCom);
                    7: cdsItens.FieldByName('QTDE').AsString := FormatFloat('###,###,###,##0.0000000', QCom);
                    8: cdsItens.FieldByName('QTDE').AsString := FormatFloat('###,###,###,##0.00000000', QCom);
                    9: cdsItens.FieldByName('QTDE').AsString := FormatFloat('###,###,###,##0.000000000', QCom);
                   10: cdsItens.FieldByName('QTDE').AsString := FormatFloat('###,###,###,##0.0000000000', QCom);
                  end;

                  case FCasasDecimaisvUnCom of
                    0: cdsItens.FieldByName('VALOR').AsString := FormatFloat('###,###,###,##0', vUnCom);
                    1: cdsItens.FieldByName('VALOR').AsString := FormatFloat('###,###,###,##0.0', vUnCom);
                    2: cdsItens.FieldByName('VALOR').AsString := FormatFloat('###,###,###,##0.00', vUnCom);
                    3: cdsItens.FieldByName('VALOR').AsString := FormatFloat('###,###,###,##0.000', vUnCom);
                    4: cdsItens.FieldByName('VALOR').AsString := FormatFloat('###,###,###,##0.0000', vUnCom);
                    5: cdsItens.FieldByName('VALOR').AsString := FormatFloat('###,###,###,##0.00000', vUnCom);
                    6: cdsItens.FieldByName('VALOR').AsString := FormatFloat('###,###,###,##0.000000', vUnCom);
                    7: cdsItens.FieldByName('VALOR').AsString := FormatFloat('###,###,###,##0.0000000', vUnCom);
                    8: cdsItens.FieldByName('VALOR').AsString := FormatFloat('###,###,###,##0.00000000', vUnCom);
                    9: cdsItens.FieldByName('VALOR').AsString := FormatFloat('###,###,###,##0.000000000', vUnCom);
                   10: cdsItens.FieldByName('VALOR').AsString := FormatFloat('###,###,###,##0.0000000000', vUnCom);
                  end;

                  cdsItens.FieldByName('UNIDADE').AsString := UCom;
                  cdsItens.FieldByName('TOTAL').AsString :=
                                      FormatFloat('###,###,###,##0.00', vProd);
                  //==============================================================================
                  // Em contato com o pessoal da Receita Estadual, foi informado que Ambos os regimes
                  // trabalham de mesma forma, deferenciando-se apensa em seus códigos
                  //==============================================================================
                  if FNFe.Emit.CRT in [crtRegimeNormal, crtSimplesExcessoReceita] then
                    begin
                      if CSTICMSToStr(CST) > '' then
                        sCST := OrigToStr(orig) + CSTICMSToStr(CST)
                      else
                        sCST := '';
                      sBCICMS    := '0,00';
                      sALIQICMS  := '0,00';
                      sVALORICMS := '0,00';

                      if (CST = cst00) then
                        begin
                          sBCICMS    := FormatFloat('###,###,###,##0.00', VBC);
                          sALIQICMS  := FormatFloat('###,###,###,##0.00', PICMS);
                          sVALORICMS := FormatFloat('###,###,###,##0.00', VICMS);
                        end
                      else if (CST = cst10) then
                        begin
                          sBCICMS    := FormatFloat('###,###,###,##0.00', VBC);
                          sALIQICMS  := FormatFloat('###,###,###,##0.00', PICMS);
                          sVALORICMS := FormatFloat('###,###,###,##0.00', VICMS);
                        end
                      else if (CST = cst20) then
                        begin
                          sBCICMS    := FormatFloat('###,###,###,##0.00', VBC);
                          sALIQICMS  := FormatFloat('###,###,###,##0.00', PICMS);
                          sVALORICMS := FormatFloat('###,###,###,##0.00', VICMS);
                        end
                      else if (CST = cst30) then
                        begin
                          sBCICMS    := FormatFloat('###,###,###,##0.00', VBCST);
                          sALIQICMS  := FormatFloat('###,###,###,##0.00', PICMSST);
                          sVALORICMS := FormatFloat('###,###,###,##0.00', VICMSST);
                        end
                      else if (CST = cst40) or (CST = cst41) or (CST = cst50) then
                        begin
                          // Campos vazios
                        end
                      else if (CST = cst51) then
                        begin
                          sBCICMS    := FormatFloat('###,###,###,##0.00', VBC);
                          sALIQICMS  := FormatFloat('###,###,###,##0.00', PICMS);
                          sVALORICMS := FormatFloat('###,###,###,##0.00', VICMS);
                        end
                      else if (CST = cst60) then
                        begin
                          sBCICMS    := FormatFloat('###,###,###,##0.00', VBCST);
                          sVALORICMS := FormatFloat('###,###,###,##0.00', VICMSST);
                        end
                      else if (CST = cst70) then
                        begin
                          sBCICMS    := FormatFloat('###,###,###,##0.00', VBC);
                          sALIQICMS  := FormatFloat('###,###,###,##0.00', PICMS);
                          sVALORICMS := FormatFloat('###,###,###,##0.00', VICMS);
                        end
                      else if (CST = cst90) then
                        begin
                          sBCICMS    := FormatFloat('###,###,###,##0.00', VBC);
                          sALIQICMS  := FormatFloat('###,###,###,##0.00', PICMS);
                          sVALORICMS := FormatFloat('###,###,###,##0.00', VICMS);
                       end;

                      cdsItens.FieldByName('CST').AsString := sCST;
                      cdsItens.FieldByName('BICMS').AsString := sBCICMS;
                      cdsItens.FieldByName('ALIQICMS').AsString := sALIQICMS;
                      cdsItens.FieldByName('VALORICMS').AsString := sVALORICMS;
                    end; //FNFe.Emit.CRT = crtRegimeNormal

                  if FNFe.Emit.CRT = crtSimplesNacional then
                    begin
                        //==============================================================================
                        // Adicionado para imprimir alíquotas
                        //==============================================================================
                        if CSOSNIcmsToStr(Imposto.ICMS.CSOSN) > '' then
                           cdsItens.FieldByName('CSOSN').AsString := OrigToStr(orig) + CSOSNIcmsToStr(Imposto.ICMS.CSOSN)
                        else
                           cdsItens.FieldByName('CSOSN').AsString := '';

                        //==============================================================================
                        // Resetando valores das qlíquotas
                        //==============================================================================
                        sBCICMS    := '0,00';
                        sALIQICMS  := '0,00';
                        sVALORICMS := '0,00';

                        case CSOSN of
                           csosn900:
                           begin
                              sBCICMS    := FormatFloat('#,##0.00', VBC);
                              sALIQICMS  := FormatFloat('#,##0.00', PICMS);
                              sVALORICMS := FormatFloat('#,##0.00', VICMS);
                           end;
                        end;

                        cdsItens.FieldByName('BICMS').AsString       := sBCICMS;
                        cdsItens.FieldByName('ALIQICMS').AsString    := sALIQICMS;
                        cdsItens.FieldByName('VALORICMS').AsString   := sVALORICMS;
                    end; //FNFe.Emit.CRT = crtSimplesNacional
                end; // with Imposto.ICMS do

               with Imposto.IPI do
                begin
                  if (CST = ipi00) or (CST = ipi49) or
                     (CST = ipi50) or (CST = ipi99) then
                    begin
                      sALIQIPI  := FormatFloat('##0.00', PIPI);
                      sVALORIPI := FormatFloat('##0.00', VIPI);
                    end
                end;

              cdsItens.FieldByName('ALIQIPI').AsString := sALIQIPI;
              cdsItens.FieldByName('VALORIPI').AsString := sVALORIPI;
              cdsItens.Post;
            end; // with Prod do
        end; //  with FNFe.Det.Items[nItem] do
    end; //  for nItem := 0 to ( FNFe.Det.Count - 1 ) do

  cdsItens.First;
end;

procedure TfqrDANFeQRNFCe.ProtocoloNFE( const sProtocolo : String );
begin
  FProtocoloNFE := sProtocolo;
end;

procedure TfqrDANFeQRNFCe.qrb01_EmitenteBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
begin
  inherited;

  PrintBand := QRNFe.PageNumber = 1;

  if FExpandirLogoMarca
   then begin
    qriLogo.top         := 13;
    qriLogo.Left        := 2;
    qriLogo.Height      := 108;
    qriLogo.Width       := 284;
    qriLogo.Stretch     := True;
    qrmEmitente.Enabled := False;
   end;

  if (FLogo <> '') and FilesExists(FLogo)
   then qriLogo.Picture.LoadFromFile(FLogo);

  if not FExpandirLogoMarca
   then begin
    qrmEmitente.Enabled := True;
    qrmEmitente.Lines.Clear;
    with FNFe.Emit do
     begin
      qrmEmitente.Lines.Add(XNome);
      with EnderEmit do
       begin
        qrmEmitente.Lines.Add(XLgr + IfThen(Nro = '0', '', ', ' + Nro) +
                              IfThen(XCpl = '', '', ', ' + XCpl) +
                              IfThen(XBairro = '', '', ', ' + XBairro) +
                              ', ' + XMun + '/ ' + UF);
       end;
      qrmEmitente.Lines.Add('CNPJ: ' + FormatarCNPJ(CNPJCPF) +
                            ' IE: '+ IE);
     end;
   end;
end;

procedure TfqrDANFeQRNFCe.qrb02_DadosFixosDanfeBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
var
 strChaveContingencia: String;
begin
  inherited;

  PrintBand := QRNFe.PageNumber = 1;

  FTotalPages := HrTotalPages;
end;

procedure TfqrDANFeQRNFCe.qrb03a_Cab_ItensBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;

//  PrintBand := QRNFe.PageNumber = 1;

end;

procedure TfqrDANFeQRNFCe.qrb03b_Desc_ItensBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
begin
  inherited;

//  Inc( nItemControle );
//  if QRNFe.PageNumber = 1 then
//     if QRNFe.RecordCount < _NUM_ITEMS_PAGE1 then
//        qrsFimItens.Enabled := ( nItemControle = QRNFe.RecordCount   )
//     else
//        qrsFimItens.Enabled := ( nItemControle = _NUM_ITEMS_PAGE1    )
//  else
//  begin
//     qrsFimItens.Enabled := ( nItemControle = _NUM_ITEMS_OTHERPAGES  ) or
//                            ( QRNFe.RecordNumber = QRNFe.RecordCount ) or
//                            ( cdsItens.Eof                           );
//  end;
//  if qrsFimItens.Enabled then
//     nItemControle := 0;
end;

procedure TfqrDANFeQRNFCe.qrb03c_Lin_ItensBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;

//  qrb03c_Lin_Itens.Enabled := FImprimeItens;
end;

procedure TfqrDANFeQRNFCe.qrb04_TotaisBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
var
 i: Integer;
begin
  inherited;

//  PrintBand := QRNFe.PageNumber = 1;

  qrmPagDesc.Lines.Clear;
  qrmPagValor.Lines.Clear;

  qrmPagDesc.Lines.Add('QTD. TOTAL DE ITENS');
  qrmPagValor.Lines.Add(IntToStr(TotalItens));

  qrmPagDesc.Lines.Add('VALOR TOTAL');
//  qrmPagValor.Lines.Add(FormatFloat(FNFE.Total.ICMSTot.vNF));
  qrmPagValor.Lines.Add(FormatFloat(FNFE.Total.ICMSTot.vProd));

  if FNFE.Total.ICMSTot.vDesc > 0.0
   then begin
     qrmPagDesc.Lines.Add('TOTAL DESCONTOS');
     qrmPagValor.Lines.Add(FormatFloat(FNFE.Total.ICMSTot.vDesc));
     qrmPagDesc.Lines.Add('TOTAL LIQUIDO');
     qrmPagValor.Lines.Add(FormatFloat(FNFE.Total.ICMSTot.vNF));
   end;

  if FvTroco > 0.0
   then begin
     qrmPagDesc.Lines.Add('TROCO');
     qrmPagValor.Lines.Add(FormatFloat(FvTroco));
   end;

  qrmPagDesc.Lines.Add('FORMA DE PAGAMENTO');
  qrmPagValor.Lines.Add('VALOR PAGO');

  for i := 0 to FNFE.pag.Count -1 do
   begin
//  TpcnFormaPagamento = (fpDinheiro, fpCheque, fpCartaoCredito, fpCartaoDebito, fpCreditoLoja,
//                        fpValeAlimentacao, fpValeRefeicao, fpValePresente, fpValeCombustivel,
//                        fpOutro);
    case FNFE.pag.Items[i].tPag of
     fpDinheiro:        qrmPagDesc.Lines.Add('Dinheiro');
     fpCheque:          qrmPagDesc.Lines.Add('Cheque');
     fpCartaoCredito:   qrmPagDesc.Lines.Add('Cartão de Crédito');
     fpCartaoDebito:    qrmPagDesc.Lines.Add('Cartão de Débito');
     fpCreditoLoja:     qrmPagDesc.Lines.Add('Crédito Loja');
     fpValeAlimentacao: qrmPagDesc.Lines.Add('Vale Alimentação');
     fpValeRefeicao:    qrmPagDesc.Lines.Add('Vale Refeição');
     fpValePresente:    qrmPagDesc.Lines.Add('Vale Presente');
     fpValeCombustivel: qrmPagDesc.Lines.Add('Vale Combustível');
     fpOutro:           qrmPagDesc.Lines.Add('Outro');
    end;

    qrmPagValor.Lines.Add(FormatFloat(FNFE.pag.Items[i].vPag));
   end;
end;

procedure TfqrDANFeQRNFCe.qrb05_TributosBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
var
 Perc: Double;
begin
  inherited;

  PrintBand := (FNFE.Total.ICMSTot.vTotTrib <> 0);
  Perc := (FNFE.Total.ICMSTot.vTotTrib / FNFE.Total.ICMSTot.vNF) * 100;
  qrlTributos.Caption := 'Val Aprox. dos Tributos: ' +
                         FormatFloat(FNFE.Total.ICMSTot.vTotTrib) +
                         '(' + FormatFloat(Perc) + '%)(Fonte: IBPT)';
end;

procedure TfqrDANFeQRNFCe.qrb05a_InfComplementarBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;

  PrintBand := (FNFE.InfAdic.infCpl <> '');
  qrmInfComp.Lines.Clear;
  qrmInfComp.Lines.Add(FNFE.InfAdic.infCpl);
end;

procedure TfqrDANFeQRNFCe.qrb06_ChaveBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
begin
  inherited;
//  PrintBand := QRNFe.PageNumber = 1;

  case FNFe.Ide.tpEmis of
    teNormal,
    teSCAN:         qrlTipoEmissao.Caption := 'EMISSÃO NORMAL';

    teContingencia,
    teFSDA,
    teOffline:      qrlTipoEmissao.Caption := 'EMITIDA EM CONTINGÊNCIA';
  end;
  (*
  if FNFe.procNFe.cStat > 0 then
   begin
     if FNFe.procNFe.cStat = 100 then
       qrlTipoEmissao.Caption := 'NFC-e AUTORIZADA';
     if ((FNFe.procNFe.cStat in [101, 151, 155]) or (FNFeCancelada)) then
       qrlTipoEmissao.Caption := 'NFC-e CANCELADA';
     if FNFe.procNFe.cStat = 110 then
       qrlTipoEmissao.Caption := 'NFC-e DENEGADA';
     if not FNFe.procNFe.cStat in [100, 101, 110, 151, 155] then
       qrlTipoEmissao.Caption := FNFe.procNFe.xMotivo;
   end;
  *)
  if FNFe.Ide.tpAmb = taHomologacao then
    qrlTipoEmissao.Caption := 'HOMOLOGAÇÃO - SEM VALOR FISCAL';

  lblNumero.Caption := 'Número: ' + FormatFloat('000,000,000', FNFe.Ide.nNF) +
                       ' - Série: '+ FormatFloat('000', FNFe.Ide.serie);

  { 10/10/2014 : Edilson Alves de Oliveira
    - Descrição Via do consumidor e Via do estabelecimento como pedi o manual}
  qrlEmissao.Caption := 'Emissão: ' + FormatDateTime(DateToStr(FNFe.Ide.dEmi)) +
                       IfThen(FViaConsumidor,' - Via do consumidor', ' - Via do estabelecimento');
                       
  qrlSiteConsulta.Caption := NotaUtil.GetURLConsultaNFCe(FNFE.Ide.cUF, FNFe.Ide.tpAmb);

  qrlChave.Caption := NotaUtil.FormatarChaveAcesso(Copy(FNFe.InfNFe.Id, 4, 44));
end;

procedure TfqrDANFeQRNFCe.qrb07_ConsumidorBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
begin
  inherited;
//  PrintBand := QRNFe.PageNumber = 1;
  qrlDestCNPJ.lines.Clear;
  if FNFE.Dest.CNPJCPF <> ''
     then begin
            qrlDestCNPJ.lines.Add( 'CPF/CNPJ: ' +
                                   FormatarCNPJCPF(FNFE.Dest.CNPJCPF) +
                                   FNFE.Dest.xNome);
            qrlDestCNPJ.lines.Add( FNFE.Dest.EnderDest.xLgr + ', ' +
                                   FNFE.Dest.EnderDest.nro + ' ' +
                                   FNFE.Dest.EnderDest.xBairro + ' ' +
                                   FNFE.Dest.EnderDest.xMun);
          end
     else if FNFE.Dest.idEstrangeiro <> ''
             then qrlDestCNPJ.lines.Add( 'ID Estrangeiro: ' +
                                         FNFE.Dest.idEstrangeiro + FNFE.Dest.xNome)
             else qrlDestCNPJ.lines.Add('CONSUMIDOR NÃO IDENTIFICADO');
end;

procedure TfqrDANFeQRNFCe.qrb08_QRCodeBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
var
 QRCodeBitmap: TBitmap;
 QRCode: TDelphiZXingQRCode;
 Row, Column: Integer;
 Scale: Double;
 cDest, sURL: String;
begin
  inherited;

//  PrintBand := QRNFe.PageNumber = 1;

  QRCodeBitmap := TBitmap.Create;
  try
    QRCode := TDelphiZXingQRCode.Create;
    try
      if FNFe.Dest.idEstrangeiro <> ''
       then cDest := FNFe.Dest.idEstrangeiro
       else cDest := FNFe.Dest.CNPJCPF;

      sURL := NotaUtil.GetURLQRCode(FNFE.Ide.cUF,
                                    FNFe.Ide.tpAmb,
                                    Copy(FNFe.InfNFe.Id, 4, 44),
                                    cDest,
                                    FNFe.Ide.dEmi,
                                    FNFe.Total.ICMSTot.vNF,
                                    FNFe.Total.ICMSTot.vICMS,
                                    FNFe.signature.DigestValue,
//                                    FNFe.procNFe.digVal,
                                    TACBrNFe( FACBrNFe ).Configuracoes.Geral.IdToken,
                                    TACBrNFe( FACBrNFe ).Configuracoes.Geral.Token);
      QRCode.Data := sURL;

      // TQRCodeEncoding = (qrAuto, qrNumeric, qrAlphanumeric, qrISO88591, qrUTF8NoBOM, qrUTF8BOM);
      // 0=Auto, 1=Numeric, 2=Alphanumeric, 3=ISO-8859-1, 4=UTF-8 without BOM, 5=UTF-8 with BOM
      QRCode.Encoding     := qrAuto; //TQRCodeEncoding(0);
      QRCode.QuietZone    := 2; //4;
      QRCodeBitmap.Width  := QRCode.Rows;
      QRCodeBitmap.Height := QRCode.Columns;

      for Row := 0 to QRCode.Rows - 1 do
      begin
        for Column := 0 to QRCode.Columns - 1 do
        begin
          if (QRCode.IsBlack[Row, Column]) then
          begin
            QRCodeBitmap.Canvas.Pixels[Column, Row] := clBlack;
          end else
          begin
            QRCodeBitmap.Canvas.Pixels[Column, Row] := clWhite;
          end;
        end;
      end;

    finally
      QRCode.Free;
    end;

    qriQRCode.Canvas.Brush.Color := clWhite;
    qriQRCode.Canvas.FillRect(Rect(0, 0, qriQRCode.Width, qriQRCode.Height));
    if ((QRCodeBitmap.Width > 0) and (QRCodeBitmap.Height > 0)) then
    begin
      if (qriQRCode.Width < qriQRCode.Height) then
      begin
        Scale := qriQRCode.Width / QRCodeBitmap.Width;
      end else
      begin
        Scale := qriQRCode.Height / QRCodeBitmap.Height;
      end;

//      Scale := 1;
      qriQRCode.Canvas.StretchDraw(Rect(0, 0, Trunc(Scale * QRCodeBitmap.Width), Trunc(Scale * QRCodeBitmap.Height)), QRCodeBitmap);
    end;

  finally
    QRCodeBitmap.Free;
  end;

  if FNFe.Ide.tpEmis in [teNormal, teSCAN]
   then begin
    if FNFe.procNFe.cStat = 100
     then qrlDescricao.Caption := 'Protocolo de Autorização';

    if FNFe.procNFe.cStat in [101, 151, 155]
     then qrlDescricao.Caption:= 'Protocolo de Homologação de Cancelamento';

    if FNFe.procNFe.cStat = 110
     then qrlDescricao.Caption:= 'Protocolo de Denegação de Uso';
   end;

  if FProtocoloNFE <> ''
   then qrlProtocolo.Caption := FProtocoloNFE
   else qrlProtocolo.Caption := FNFe.procNFe.nProt + ' ' +
                                SeSenao(FNFe.procNFe.dhRecbto <> 0, DateTimeToStr(FNFe.procNFe.dhRecbto), '');
end;

end.
