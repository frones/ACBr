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
  pcnConversao;

type

  { TfrlDANFeRLSimplificado }

  TfrlDANFeRLSimplificado = class(TfrlDANFeRL)
    RLb02_Emitente: TRLBand;
    RLb03_DadosGerais: TRLBand;
    RLb04_Destinatario: TRLBand;
    RLb05a_Cab_Itens: TRLBand;
    RLb05b_Desc_Itens: TRLBand;
    RLb05c_Lin_Itens: TRLBand;
    RLb06a_Totais: TRLBand;
    RLb06b_Tributos: TRLBand;
    RLiLogo: TRLImage;
    RLLabel1: TRLLabel;
    RLLabel142: TRLLabel;
    RLLabel143: TRLLabel;
    RLLabel147: TRLLabel;
    RLLabel148: TRLLabel;
    RLLabel149: TRLLabel;
    RLLabel150: TRLLabel;
    RLLabel27: TRLLabel;
    RLLabel3: TRLLabel;
    RLLabel9: TRLLabel;
    RLlChave: TRLLabel;
    RLlDescricao: TRLLabel;
    RLlEmissao: TRLLabel;
    RLlEntradaSaida: TRLLabel;
    RLlMsgTipoEmissao: TRLLabel;
    RLlProtocolo: TRLLabel;
    RLlTipoEmissao: TRLLabel;
    RLlTributos: TRLLabel;
    RLmDestinatario: TRLMemo;
    RLmEmitente: TRLMemo;
    RLmPagDesc: TRLMemo;
    RLmPagValor: TRLMemo;
    RLmProdutoCodigo: TRLDBText;
    RLmProdutoDescricao: TRLDBText;
    RLmProdutoItem: TRLDBText;
    RLmProdutoQTDE: TRLDBText;
    RLmProdutoTotal: TRLDBText;
    RLmProdutoUnidade: TRLDBText;
    RLmProdutoValor: TRLDBText;
    RLShape102: TRLDraw;
    RLShape68: TRLDraw;
    rlb01_Chave: TRLBand;
    RLBarcode1: TRLBarcode;
    RLLabel17: TRLLabel;
    rliBarCode: TRLImage;
    lblNumero: TRLLabel;
    procedure RLb02_EmitenteBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure RLb03_DadosGeraisBeforePrint(Sender: TObject; var PrintIt: boolean
      );
    procedure RLb04_DestinatarioBeforePrint(Sender: TObject;
      var PrintIt: boolean);
    procedure RLb06a_TotaisBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure RLb06b_TributosBeforePrint(Sender: TObject; var PrintIt: boolean);
    procedure RLNFeBeforePrint(Sender: TObject;
      var PrintReport: Boolean);
    procedure rlmProdutoDescricaoPrint(sender: TObject; var Value: string);

    procedure rlb01_ChaveBeforePrint(Sender: TObject;
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
 StrUtils, DateUtils,
 ACBrUtil, ACBrValidador, ACBrDFeUtil,
 pcnNFe, pcnConversaoNFe;

{$R *.dfm}

const
   _NUM_ITEMS_PAGE1      = 18;
   _NUM_ITEMS_OTHERPAGES = 50;

procedure TfrlDANFeRLSimplificado.RLNFeBeforePrint(Sender: TObject;
  var PrintReport: Boolean);
var
 nRestItens: Integer;
begin
  inherited;

  //rlb05a_Cab_Itens.Enabled := FImprimeItens;
  //rlb05b_Desc_Itens.Enabled := FImprimeItens;
  //rlb05c_Lin_Itens.Enabled := FImprimeItens;

  Itens;
  FTotalPages := 1;

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

//  PrintBand := RLNFe.PageNumber = 1;

  if FExpandirLogoMarca
   then begin
    rliLogo.top         := 13;
    rliLogo.Left        := 2;
    rliLogo.Height      := 108;
    rliLogo.Width       := 284;
    rliLogo.Stretch     := True;
    rlmEmitente.Enabled := False;
   end;

  if (FLogo <> '') and FilesExists(FLogo)
   then rliLogo.Picture.LoadFromFile(FLogo);

  if not FExpandirLogoMarca
   then begin
    rlmEmitente.Enabled := True;
    rlmEmitente.Lines.Clear;
    with FNFe.Emit do
     begin
      rlmEmitente.Lines.Add(XNome);
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
  if FNFe.Ide.tpEmis in [teContingencia, teFSDA]
   then rllTipoEmissao.Caption := 'CONTINGENCIA FS-DA';

  rllEntradaSaida.Caption := tpNFToStr( FNFe.Ide.tpNF );

  lblNumero.Caption := 'Número: ' + FormatFloat('000,000,000', FNFe.Ide.nNF) +
                       ' - Série: '+ FormatFloat('000', FNFe.Ide.serie);

  rllEmissao.Caption := 'Emissão: ' + FormatDateTimeBr(FNFe.Ide.dEmi);
end;

procedure TfrlDANFeRLSimplificado.RLb04_DestinatarioBeforePrint(
  Sender: TObject; var PrintIt: boolean);
var
 vTpEmissao: Integer;
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
    rlmDestinatario.Lines.Add('CPF/CNPJ: ' + FormatarCNPJouCPF(CNPJCPF) +
                              ' IE: ' + IE);
   end;

  if FNFe.Ide.tpAmb = taHomologacao then
   begin
     rllMsgTipoEmissao.Caption := 'HOMOLOGAÇÂO - SEM VALOR FISCAL';
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

  if FNFe.Ide.tpEmis = teContingencia then
      vTpEmissao:=2
  else
  if FNFe.Ide.tpEmis = teFSDA then
      vTpEmissao:=5;

  case vTpEmissao of
   2: begin
       rllMsgTipoEmissao.Caption := 'DANFE em Contingencia - impresso em decorrencia de problemas tecnicos';
       rllMsgTipoEmissao.Visible := True;
       rllMsgTipoEmissao.Enabled := True;
      end;
   5: begin
       rllMsgTipoEmissao.Caption := 'DANFE em Contingencia - impresso em decorrencia de problemas tecnicos';
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

//  PrintBand := RLNFe.PageNumber = 1;

  rlmPagDesc.Lines.Clear;
  rlmPagValor.Lines.Clear;

  rlmPagDesc.Lines.Add('Qtde Total de Itens');
  rlmPagValor.Lines.Add(IntToStr(TotalItens));

  rlmPagDesc.Lines.Add('Valor Total');
  rlmPagValor.Lines.Add(FormatFloatBr(FNFE.Total.ICMSTot.vNF));
end;

procedure TfrlDANFeRLSimplificado.RLb06b_TributosBeforePrint(Sender: TObject;
  var PrintIt: boolean);
var
 Perc: Double;
begin
  inherited;
//  PrintBand := RLNFe.PageNumber = 1;

  Perc := (FNFE.Total.ICMSTot.vTotTrib / FNFE.Total.ICMSTot.vNF) * 100;
  rllTributos.Caption := 'Valor aprox. dos tributos: ' +
                         FormatFloatBr(FNFE.Total.ICMSTot.vTotTrib) +
                         '(' + FormatFloatBr(Perc) + '%)(Fonte: IBPT)';
end;

procedure TfrlDANFeRLSimplificado.rlmProdutoDescricaoPrint(sender: TObject;
  var Value: string);
var
 intTamanhoDescricao,
 intTamanhoAdicional,
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

  if cdsItens.FieldByName('INFADIPROD').AsString <> ''
   then Value := Value + #13 + 'InfAd: ' + cdsItens.FieldByName('INFADIPROD').AsString;
end;

procedure TfrlDANFeRLSimplificado.Itens;
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

procedure TfrlDANFeRLSimplificado.ProtocoloNFE( const sProtocolo : String );
begin
  FProtocoloNFE := sProtocolo;
end;

procedure TfrlDANFeRLSimplificado.rlb01_ChaveBeforePrint(Sender: TObject;
  var PrintBand: Boolean);
begin
  inherited;

  PrintBand := RLNFe.PageNumber = 1;

  RLBarcode1.Caption := Copy ( FNFe.InfNFe.Id, 4, 44 );

  rllChave.Caption := FormatarChaveAcesso(Copy(FNFe.InfNFe.Id, 4, 44));

  // Normal **************************************************************
  if FNFe.Ide.tpEmis in [teNormal, teSCAN]
   then begin
    if FNFe.procNFe.cStat = 100
     then rllDescricao.Caption := 'Protocolo de Autorização';

    if FNFe.procNFe.cStat in [101, 151, 155]
     then rllDescricao.Caption:= 'Protocolo de Homologação de Cancelamento';

    if FNFe.procNFe.cStat = 110
     then rllDescricao.Caption:= 'Protocolo de Denegação de Uso';
   end;

  if FProtocoloNFE <> ''
   then rllProtocolo.Caption := FProtocoloNFE
   else rllProtocolo.Caption := FNFe.procNFe.nProt + ' ' +
                                IfThen(FNFe.procNFe.dhRecbto <> 0, DateTimeToStr(FNFe.procNFe.dhRecbto), '');

  //FTotalPages := HrTotalPages;
end;

end.
