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
| 18/03/2010: André R. Langner
|  - Alterada fonte de Arial para Courier New.
|  - Ajuste na disposição de campos no DANFe.
|  - Impressao dos campos Fax, Email, Site referentes ao emitente.
|  - Adicionado campos qrlDataHoraImpressao, qrlSistema.
|  - Impressao de Data/Hora da impressao, Usuario, Sistema no rodapé do DANFe.
|  - Impressao de informacoes adicionais do item logo a baixo a sua descricao
|    em qrmProdutoDescricao.
|  - Impressao da Hora de Saida em qrlHoraSaida.
|  - Impressao de codigo de barras e chave em caso de impressao de formulário
|    de segurança
|  - Impressao de Local de Retirada e Local de Entrega em Informacoes Complementares.
|
| 15/12/2009: Emerson Crema
|  - Removida a rotina Detalhes e desenvolvida a Itens.
|  - Implementado ClientDataSet.
|  - Correcao no totalizador de paginas.
|
| 11/12/2009: Emerson Crema
|  - Ajuste no posicionamento das linhas e alinhamento de campos numéricos
|  - Alteracao da legenda 1-Saida, 2-Entrada, para 0-Entrada, 1- Saida.
|  - Alteracao da legenda 1-Emitente, 2-Destinatario, para
|    0-Emitente, 1-Destinatario.
|  - Incluida a mensagem para modo "homologação":
|    "Este documento não tem validade jurídica"
|    obs: precisa do componente QrAngLbl.
|  - Habilitada a banda qrsISSQN desde q FNFE.Total.ISSQNtot.vISS > 0.
|  - Preenchimento dos labels: qrlTotalServicos, qrlBaseISSQN, qrlValorISSQN.
|  - Implementada ajuste na lista de itens para + d 1 pagina.
|  - Implementado campo "Protocolo de autorizacao".
|  - Colocada a banda de "Identificacao do recebedor" no topo.
|
| 20/08/2009: Caique Rodrigues
|  - Doação units para geração do Danfe via QuickReport
|
| 16/12/2008: Wemerson Souto
|  - Doação do componente para o Projeto ACBr
|  23/11/2010: Peterson de Cerqueira Matos
|   - Formatação das casas decimais da "Quantidade" e do "Valor Unitário"
|   - Correção na exibição da coluna CST. Quando o emitente for "Simples
|     Nacional - CRT=1", será exibida a informação CSOSN ao invés do CST
|  20/05/2011: Peterson de Cerqueira Matos
|   - Ajuste de layout quadro "duplicatas"
|   - Ajuste no procedimento de exibição das duplicatas limitando-as em 15
|     para evitar Acess Violation em NF-e's com mais de 15 duplicatas
|   - Tratamento da propriedade "ExibirResumoCanhoto"
|   - Tratamento da propriedade "ExibirResumoCanhoto_Texto"
|  22/01/2013 : LUIS FERNANDO COSTA
|   - Ajustado "FSistema" para que fique uma msg livre
*******************************************************************************}

{$I ACBr.inc}

unit ACBrNFeDANFeQRSimplificado;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, QuickRpt, QRCtrls,  XMLIntf, XMLDoc,
  JPEG, ACBrNFeDANFeQR, ACBrNFeQRCodeBar, pcnConversao, DB,
  {$IFDEF QReport_PDF}
     QRPDFFilt,
  {$ENDIF}
  DBClient, ACBrNFeDANFEClass, ACBrNFeDANFeQRClass;

type

  TfqrDANFeQRSimplificado = class(TfqrDANFeQR)
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
    qrb01_Chave: TQRBand;
    qrb02_Emitente: TQRChildBand;
    qrb05a_Cab_Itens: TQRBand;
    qrb06a_Totais: TQRBand;
    QRLabel142: TQRLabel;
    QRLabel143: TQRLabel;
    QRLabel147: TQRLabel;
    QRLabel148: TQRLabel;
    QRLabel149: TQRLabel;
    QRLabel150: TQRLabel;
    qrb05b_Desc_Itens: TQRBand;
    qrmProdutoCodigo: TQRDBText;
    qrmProdutoDescricao: TQRDBText;
    qrmProdutoUnidade: TQRDBText;
    qrmProdutoQTDE: TQRDBText;
    qrmProdutoValor: TQRDBText;
    qrmProdutoTotal: TQRDBText;
    qrb05c_Lin_Itens: TQRChildBand;
    QRShape68: TQRShape;
    cdsItensXPROD: TStringField;
    cdsItensINFADIPROD: TStringField;
    cdsItensCSOSN: TStringField;
    qrb06b_Tributos: TQRChildBand;
    qrlTributos: TQRLabel;
    qrmPagDesc: TQRMemo;
    qrmPagValor: TQRMemo;
    QRLabel3: TQRLabel;
    cdsItensITEM: TStringField;
    qrmProdutoItem: TQRDBText;
    qriLogo: TQRImage;
    qrmEmitente: TQRMemo;
    QRLabel17: TQRLabel;
    qriBarCode: TQRImage;
    QRLabel9: TQRLabel;
    qrlChave: TQRLabel;
    qrlDescricao: TQRLabel;
    qrlProtocolo: TQRLabel;
    qrb03_DadosGerais: TQRChildBand;
    qrlTipoEmissao: TQRLabel;
    lblNumero: TQRLabel;
    qrlEmissao: TQRLabel;
    qrb04_Destinatario: TQRChildBand;
    QRLabel1: TQRLabel;
    qrlMsgTipoEmissao: TQRLabel;
    qrmDestinatario: TQRMemo;
    QRLabel27: TQRLabel;
    QRShape102: TQRShape;
    qrlEntradaSaida: TQRLabel;
    procedure QRNFeBeforePrint(Sender: TCustomQuickRep;
      var PrintReport: Boolean);
    procedure qrmProdutoDescricaoPrint(sender: TObject; var Value: string);

    procedure qrb01_ChaveBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrb02_EmitenteBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrb03_DadosGeraisBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrb04_DestinatarioBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrb05a_Cab_ItensBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrb05b_Desc_ItensBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrb05c_Lin_ItensBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrb06a_TotaisBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrb06b_TributosBeforePrint(Sender: TQRCustomBand;
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

procedure TfqrDANFeQRSimplificado.QRNFeBeforePrint(Sender: TCustomQuickRep;
  var PrintReport: Boolean);
var
 nRestItens: Integer;
begin
  inherited;

  qrb05a_Cab_Itens.Enabled := FImprimeItens;
  qrb05b_Desc_Itens.Enabled := FImprimeItens;
  qrb05c_Lin_Itens.Enabled := FImprimeItens;

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

procedure TfqrDANFeQRSimplificado.qrmProdutoDescricaoPrint(sender: TObject;
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

procedure TfqrDANFeQRSimplificado.Itens;
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

procedure TfqrDANFeQRSimplificado.ProtocoloNFE( const sProtocolo : String );
begin
  FProtocoloNFE := sProtocolo;
end;

procedure TfqrDANFeQRSimplificado.qrb01_ChaveBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
begin
  inherited;

  PrintBand := QRNFe.PageNumber = 1;

  SetBarCodeImage( Copy ( FNFe.InfNFe.Id, 4, 44 ), qriBarCode );

  qrlChave.Caption := NotaUtil.FormatarChaveAcesso(Copy(FNFe.InfNFe.Id, 4, 44));

  // Normal **************************************************************
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

  FTotalPages := HrTotalPages;
end;

procedure TfqrDANFeQRSimplificado.qrb02_EmitenteBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
var
 strChaveContingencia: String;
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

procedure TfqrDANFeQRSimplificado.qrb03_DadosGeraisBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;

  // Contingencia ********************************************************
  if FNFe.Ide.tpEmis in [teContingencia, teFSDA]
   then qrlTipoEmissao.Caption := 'CONTINGENCIA FS-DA';

  qrlEntradaSaida.Caption := tpNFToStr( FNFe.Ide.tpNF );

  lblNumero.Caption := 'Número: ' + FormatFloat('000,000,000', FNFe.Ide.nNF) +
                       ' - Série: '+ FormatFloat('000', FNFe.Ide.serie);

  qrlEmissao.Caption := 'Emissão: ' + FormatDateTime(DateToStr(FNFe.Ide.dEmi));
end;

procedure TfqrDANFeQRSimplificado.qrb04_DestinatarioBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
var
 vTpEmissao: Integer;
begin
  inherited;

  qrmDestinatario.Lines.Clear;
  with FNFe.Dest do
   begin
    qrmDestinatario.Lines.Add(XNome);
    with EnderDest do
     begin
      qrmDestinatario.Lines.Add(XLgr + IfThen(Nro = '0', '', ', ' + Nro) +
                            IfThen(XCpl = '', '', ', ' + XCpl) +
                            IfThen(XBairro = '', '', ', ' + XBairro) +
                            ', ' + XMun + '/ ' + UF);
     end;
    qrmDestinatario.Lines.Add('CPF/CNPJ: ' + FormatarCNPJCPF(CNPJCPF) +
                              ' IE: ' + IE);
   end;

  if FNFe.Ide.tpAmb = taHomologacao then
   begin
     qrlMsgTipoEmissao.Caption := 'HOMOLOGAÇÂO - SEM VALOR FISCAL';
     qrlMsgTipoEmissao.Enabled := True;
     qrlMsgTipoEmissao.Visible := True;
   end;

  if FNFe.procNFe.cStat > 0 then
   begin
     if ((FNFe.procNFe.cStat in [101, 151, 155]) or (FNFeCancelada)) then
     begin
       qrlMsgTipoEmissao.Caption := 'NF-e CANCELADA';
       qrlMsgTipoEmissao.Visible := True;
       qrlMsgTipoEmissao.Enabled := True;
     end;
     if FNFe.procNFe.cStat = 110 then
     begin
       qrlMsgTipoEmissao.Caption := 'NF-e DENEGADA';
       qrlMsgTipoEmissao.Visible := True;
       qrlMsgTipoEmissao.Enabled := True;
     end;
     if not FNFe.procNFe.cStat in [100, 101, 110, 151, 155] then
     begin
       qrlMsgTipoEmissao.Caption := FNFe.procNFe.xMotivo;
       qrlMsgTipoEmissao.Visible := True;
       qrlMsgTipoEmissao.Enabled := True;
     end;
   end;

  if FNFe.Ide.tpEmis = teContingencia then
      vTpEmissao:=2
  else
  if FNFe.Ide.tpEmis = teFSDA then
      vTpEmissao:=5;

  case vTpEmissao of
   2: begin
       qrlMsgTipoEmissao.Caption := 'DANFE em Contingencia - impresso em decorrencia de problemas tecnicos';
       qrlMsgTipoEmissao.Visible := True;
       qrlMsgTipoEmissao.Enabled := True;
      end;
   5: begin
       qrlMsgTipoEmissao.Caption := 'DANFE em Contingencia - impresso em decorrencia de problemas tecnicos';
       qrlMsgTipoEmissao.Visible := True;
       qrlMsgTipoEmissao.Enabled := True;
      end;
  end;

 qrlMsgTipoEmissao.Repaint;
end;

procedure TfqrDANFeQRSimplificado.qrb05a_Cab_ItensBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;

//  PrintBand := QRNFe.PageNumber = 1;

end;

procedure TfqrDANFeQRSimplificado.qrb05b_Desc_ItensBeforePrint(Sender: TQRCustomBand;
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

procedure TfqrDANFeQRSimplificado.qrb05c_Lin_ItensBeforePrint(
  Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  inherited;

//  PrintBand := QRNFe.PageNumber = 1;

end;

procedure TfqrDANFeQRSimplificado.qrb06a_TotaisBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
var
 i: Integer;
begin
  inherited;

//  PrintBand := QRNFe.PageNumber = 1;

  qrmPagDesc.Lines.Clear;
  qrmPagValor.Lines.Clear;

  qrmPagDesc.Lines.Add('Qtde Total de Itens');
  qrmPagValor.Lines.Add(IntToStr(TotalItens));

  qrmPagDesc.Lines.Add('Valor Total');
  qrmPagValor.Lines.Add(FormatFloat(FNFE.Total.ICMSTot.vNF));
end;

procedure TfqrDANFeQRSimplificado.qrb06b_TributosBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
var
 Perc: Double;
begin
  inherited;
//  PrintBand := QRNFe.PageNumber = 1;

  Perc := (FNFE.Total.ICMSTot.vTotTrib / FNFE.Total.ICMSTot.vNF) * 100;
  qrlTributos.Caption := 'Valor aprox. dos tributos: ' +
                         FormatFloat(FNFE.Total.ICMSTot.vTotTrib) +
                         '(' + FormatFloat(Perc) + '%)(Fonte: IBPT)';
end;

end.
