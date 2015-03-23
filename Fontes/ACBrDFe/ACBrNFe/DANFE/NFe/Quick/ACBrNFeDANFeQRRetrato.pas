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

unit ACBrNFeDANFeQRRetrato;

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

  TfqrDANFeQRRetrato = class(TfqrDANFeQR)
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
    qrbRecibo: TQRBand;
    qrbDadosDanfe: TQRChildBand;
    qrbEmitenteDestinatario: TQRChildBand;
    qrbHeaderItens: TQRBand;
    qrbISSQN: TQRBand;
    qrbDadosAdicionais: TQRChildBand;
    QRShape1: TQRShape;
    QRLabel10: TQRLabel;
    QRShape3: TQRShape;
    QRShape2: TQRShape;
    QRShape101: TQRShape;
    qrlRecebemosDe1: TQRLabel;
    qrlNumNF0: TQRLabel;
    qrlSERIE0: TQRLabel;
    QRLabel14: TQRLabel;
    QRLabel13: TQRLabel;
    QRLabel12: TQRLabel;
    QRLabel11: TQRLabel;
    QRShape5: TQRShape;
    QRShape6: TQRShape;
    QRShape7: TQRShape;
    QRLabel17: TQRLabel;
    QRShape8: TQRShape;
    QRShape9: TQRShape;
    QRShape11: TQRShape;
    QRLabel29: TQRLabel;
    QRLabel30: TQRLabel;
    QRLabel31: TQRLabel;
    QRLabel32: TQRLabel;
    qrlNatOperacao: TQRLabel;
    qrlInscricaoEstadual: TQRLabel;
    qrlInscrEstSubst: TQRLabel;
    qrlCNPJ: TQRLabel;
    qrlFone: TQRLabel;
    qrmEmitente: TQRMemo;
    qrmEndereco: TQRMemo;
    qriLogo: TQRImage;
    qriBarCode: TQRImage;
    QRLabel2: TQRLabel;
    qrlChave: TQRLabel;
    qrlMsgAutorizado: TQRLabel;
    qrlDescricao: TQRLabel;
    qrlProtocolo: TQRLabel;
    qrlSERIE1: TQRLabel;
    QRLabel22: TQRLabel;
    QRLabel18: TQRLabel;
    QRLabel19: TQRLabel;
    QRLabel20: TQRLabel;
    qrlNumNF1: TQRLabel;
    QRLabel24: TQRLabel;
    qrlPageNumber: TQRLabel;
    QRLabel26: TQRLabel;
    QRLabel27: TQRLabel;
    QRShape102: TQRShape;
    qrlEntradaSaida: TQRLabel;
    QRLabel1: TQRLabel;
    qrlblFatura: TQRLabel;
    QRLabel5: TQRLabel;
    QRLabel4: TQRLabel;
    QRShape12: TQRShape;
    QRShape15: TQRShape;
    QRShape16: TQRShape;
    QRShape17: TQRShape;
    QRShape18: TQRShape;
    QRShape19: TQRShape;
    QRShape20: TQRShape;
    QRShape13: TQRShape;
    QRShape14: TQRShape;
    QRShape21: TQRShape;
    QRShape22: TQRShape;
    QRShape23: TQRShape;
    qrshpFatura: TQRShape;
    QRShape25: TQRShape;
    QRShape26: TQRShape;
    QRShape27: TQRShape;
    QRShape28: TQRShape;
    QRShape29: TQRShape;
    QRShape30: TQRShape;
    QRShape31: TQRShape;
    QRShape32: TQRShape;
    QRShape33: TQRShape;
    QRShape34: TQRShape;
    QRShape35: TQRShape;
    QRShape36: TQRShape;
    QRShape37: TQRShape;
    QRShape38: TQRShape;
    QRShape39: TQRShape;
    QRShape42: TQRShape;
    QRShape43: TQRShape;
    QRShape44: TQRShape;
    QRShape45: TQRShape;
    QRShape46: TQRShape;
    QRShape47: TQRShape;
    QRShape48: TQRShape;
    QRShape49: TQRShape;
    QRShape50: TQRShape;
    QRShape51: TQRShape;
    QRLabel34: TQRLabel;
    QRLabel35: TQRLabel;
    QRLabel36: TQRLabel;
    QRLabel37: TQRLabel;
    QRLabel38: TQRLabel;
    QRLabel39: TQRLabel;
    QRLabel40: TQRLabel;
    QRLabel41: TQRLabel;
    QRLabel42: TQRLabel;
    QRLabel43: TQRLabel;
    QRLabel44: TQRLabel;
    QRLabel45: TQRLabel;
    QRLabel46: TQRLabel;
    QRLabel47: TQRLabel;
    QRLabel48: TQRLabel;
    QRLabel49: TQRLabel;
    QRLabel50: TQRLabel;
    QRLabel51: TQRLabel;
    QRLabel52: TQRLabel;
    QRLabel53: TQRLabel;
    QRLabel54: TQRLabel;
    QRLabel55: TQRLabel;
    QRLabel56: TQRLabel;
    QRLabel57: TQRLabel;
    QRLabel58: TQRLabel;
    QRLabel59: TQRLabel;
    QRLabel60: TQRLabel;
    QRLabel61: TQRLabel;
    QRLabel62: TQRLabel;
    QRLabel63: TQRLabel;
    QRLabel64: TQRLabel;
    QRLabel65: TQRLabel;
    QRLabel66: TQRLabel;
    QRLabel67: TQRLabel;
    QRLabel68: TQRLabel;
    QRLabel69: TQRLabel;
    QRLabel70: TQRLabel;
    QRLabel71: TQRLabel;
    QRLabel72: TQRLabel;
    qrlFatData1: TQRLabel;
    qrlFatData2: TQRLabel;
    qrlFatData3: TQRLabel;
    qrlFatValor1: TQRLabel;
    qrlFatValor2: TQRLabel;
    qrlFatValor3: TQRLabel;
    qrlFatNum1: TQRLabel;
    qrlFatNum2: TQRLabel;
    qrlFatNum3: TQRLabel;
    QRShape106: TQRShape;   
    QRShape107: TQRShape;
    QRShape108: TQRShape;
    QRShape109: TQRShape;
    qrlFatNum6: TQRLabel;
    qrlFatNum5: TQRLabel;
    qrlFatNum4: TQRLabel;
    qrlFatData4: TQRLabel;
    qrlFatData5: TQRLabel;
    qrlFatData6: TQRLabel;
    qrlFatValor6: TQRLabel;
    qrlFatValor5: TQRLabel;
    qrlFatValor4: TQRLabel;
    qrlFatNum9: TQRLabel;
    qrlFatNum8: TQRLabel;
    qrlFatNum7: TQRLabel;
    qrlFatData7: TQRLabel;
    qrlFatData8: TQRLabel;
    qrlFatData9: TQRLabel;
    qrlFatValor9: TQRLabel;
    qrlFatValor8: TQRLabel;
    qrlFatValor7: TQRLabel;
    qrlFatNum12: TQRLabel;
    qrlFatNum11: TQRLabel;
    qrlFatNum10: TQRLabel;
    qrlFatData10: TQRLabel;
    qrlFatData11: TQRLabel;
    qrlFatData12: TQRLabel;
    qrlFatValor12: TQRLabel;
    qrlFatValor11: TQRLabel;
    qrlFatValor10: TQRLabel;
    qrlFatNum15: TQRLabel;
    qrlFatNum14: TQRLabel;
    qrlFatNum13: TQRLabel;
    qrlFatData13: TQRLabel;
    qrlFatData14: TQRLabel;
    qrlFatData15: TQRLabel;
    qrlFatValor15: TQRLabel;
    qrlFatValor14: TQRLabel;
    qrlFatValor13: TQRLabel;
    qrlDestNome: TQRLabel;
    qrlDestCNPJ: TQRLabel;
    qrlDestEndereco: TQRLabel;
    qrlDestBairro: TQRLabel;
    qrlDestCEP: TQRLabel;
    qrlDestCidade: TQRLabel;
    qrlDestFone: TQRLabel;
    qrlDestUF: TQRLabel;
    qrlDestIE: TQRLabel;
    qrlEmissao: TQRLabel;
    qrlSaida: TQRLabel;
    qrlBaseICMS: TQRLabel;
    qrlValorICMS: TQRLabel;
    qrlBaseICMST: TQRLabel;
    qrlValorICMST: TQRLabel;
    qrlTotalProdutos: TQRLabel;
    qrlValorFrete: TQRLabel;
    qrlValorSeguro: TQRLabel;
    qrlDescontos: TQRLabel;
    qrlAcessorias: TQRLabel;
    qrlValorIPI: TQRLabel;
    qrlTotalNF: TQRLabel;
    qrlTransNome: TQRLabel;
    qrlTransModFrete: TQRLabel;
    qrlTransCodigoANTT: TQRLabel;
    qrlTransPlaca: TQRLabel;
    qrlTransUFPlaca: TQRLabel;
    qrlTransCNPJ: TQRLabel;
    qrlTransEndereco: TQRLabel;
    qrlTransCidade: TQRLabel;
    qrlTransUF: TQRLabel;
    qrlTransIE: TQRLabel;
    qrlTransQTDE: TQRLabel;
    qrlTransEspecie: TQRLabel;
    qrlTransMarca: TQRLabel;
    qrlTransNumeracao: TQRLabel;
    qrlTransPesoBruto: TQRLabel;
    qrlTransPesoLiq: TQRLabel;
    qrlHoraSaida: TQRLabel;
    QRLabel142: TQRLabel;
    QRLabel143: TQRLabel;
    QRLabel144: TQRLabel;
    lblCST: TQRLabel;
    QRLabel146: TQRLabel;
    QRLabel147: TQRLabel;
    QRLabel148: TQRLabel;
    QRLabel149: TQRLabel;
    QRLabel150: TQRLabel;
    QRLabel151: TQRLabel;
    QRLabel152: TQRLabel;
    QRLabel153: TQRLabel;
    QRLabel156: TQRLabel;
    QRLabel154: TQRLabel;
    QRLabel157: TQRLabel;
    QRShape10: TQRShape;
    QRShape24: TQRShape;
    QRShape40: TQRShape;
    QRShape41: TQRShape;
    QRShape57: TQRShape;
    QRShape58: TQRShape;
    QRShape59: TQRShape;
    QRShape60: TQRShape;
    QRShape61: TQRShape;
    QRShape62: TQRShape;
    QRShape63: TQRShape;
    QRShape64: TQRShape;
    QRShape65: TQRShape;
    QRLabel3: TQRLabel;
    QRShape52: TQRShape;
    QRShape53: TQRShape;
    QRShape54: TQRShape;
    QRShape55: TQRShape;
    QRLabel137: TQRLabel;
    QRLabel138: TQRLabel;
    QRLabel139: TQRLabel;
    QRLabel140: TQRLabel;
    qrlInscMunicipal: TQRLabel;
    qrlTotalServicos: TQRLabel;
    qrlBaseISSQN: TQRLabel;
    qrlValorISSQN: TQRLabel;
    QRShape56: TQRShape;
    QRLabel100: TQRLabel;
    QRLabel7: TQRLabel;
    qrmDadosAdicionais: TQRMemo;
    rbDadosAdicionais: TQRShape;
    QRShape67: TQRShape;
    QRLabel6: TQRLabel;
    qrbItens: TQRBand;
    qrmProdutoCodigo: TQRDBText;
    qrmProdutoDescricao: TQRDBText;
    qrmProdutoNCM: TQRDBText;
    qrmProdutoCST: TQRDBText;
    qrmProdutoCFOP: TQRDBText;
    qrmProdutoUnidade: TQRDBText;
    qrmProdutoQTDE: TQRDBText;
    qrmProdutoValor: TQRDBText;
    qrmProdutoTotal: TQRDBText;
    qrmProdutoBCICMS: TQRDBText;
    qrmProdutoVALORICMS: TQRDBText;
    qrmProdutoALIQICMS: TQRDBText;
    qrmProdutoVALORIPI: TQRDBText;
    qrmProdutoALIQIPI: TQRDBText;
    QRShape4: TQRShape;
    QRShape66: TQRShape;
    qriBarCodeContingencia: TQRImage;
    QRShape69: TQRShape;
    qrlDataHoraImpressao: TQRLabel;
    qrlSistema: TQRLabel;
    qrs2: TQRShape;
    cdsItensXPROD: TStringField;
    cdsItensINFADIPROD: TStringField;
    qrs3: TQRShape;
    qrs4: TQRShape;
    qrs5: TQRShape;
    qrs6: TQRShape;
    qrs7: TQRShape;
    qrs8: TQRShape;
    qrs9: TQRShape;
    qrs10: TQRShape;
    qrs11: TQRShape;
    qrs12: TQRShape;
    qrs13: TQRShape;
    qrs14: TQRShape;
    cdsItensCSOSN: TStringField;
    qrlResumo: TQRLabel;
    QRShape74: TQRShape;
    QRLabel15: TQRLabel;
    QRShape75: TQRShape;
    QRLabel16: TQRLabel;
    QRLabel21: TQRLabel;
    QRLabel73: TQRLabel;
    QRLabel74: TQRLabel;
    QRShape76: TQRShape;
    QRShape77: TQRShape;
    qrlDestCNPJEnt: TQRLabel;
    qrlDestEnderecoEnt: TQRLabel;
    qrlDestBairroEnt: TQRLabel;
    qrlDestCidadeEnt: TQRLabel;
    qrlMsgTeste: TQRLabel;
    qrlMsgTipoEmissao: TQRLabel;
    QRShape70: TQRShape;
    QRLabel8: TQRLabel;
    QRShape71: TQRShape;
    QRShape72: TQRShape;
    QRShape73: TQRShape;
    qrlRecebemosDe1Rodape: TQRLabel;
    qrlNumNF0Rodape: TQRLabel;
    qrlSERIE0Rodape: TQRLabel;
    QRLabel23: TQRLabel;
    QRLabel25: TQRLabel;
    QRLabel28: TQRLabel;
    QRLabel33: TQRLabel;
    qrlResumoRodape: TQRLabel;
    QRLabel141: TQRLabel;
    QRShape68: TQRShape;
    QRLabel9: TQRLabel;
    qrlValorTotTrib: TQRLabel;
    cdsItensVALORDESC: TStringField;
    QRLabel75: TQRLabel;
    qrmProdutoVALORDESC: TQRDBText;
    QRShape78: TQRShape;
    qrs15: TQRShape;
    QRShape79: TQRShape;
    qrs16: TQRShape;
    QRShape80: TQRShape;
    QRShape81: TQRShape;
    QRShape82: TQRShape;
    qrs18: TQRShape;
    qrs17: TQRShape;
    procedure QRNFeBeforePrint(Sender: TCustomQuickRep;
      var PrintReport: Boolean);
    procedure qrbReciboBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrbEmitenteDestinatarioBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrbISSQNBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrbDadosAdicionaisBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrbDadosDanfeBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrbItensBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
    procedure qrmProdutoDescricaoPrint(sender: TObject; var Value: string);
    procedure qrbHeaderItensBeforePrint(Sender: TQRCustomBand;
      var PrintBand: Boolean);
  private
    { Private declarations }
    FTotalPages : integer;
    procedure Itens;
  public
    { Public declarations }
    procedure ProtocoloNFE( const sProtocolo : String );
  end;


implementation

uses
 StrUtils, DateUtils,
 ACBrUtil, ACBrDFeUtil, pcnNFe;

{$R *.dfm}

const
   _NUM_ITEMS_PAGE1      = 18;
   _NUM_ITEMS_OTHERPAGES = 50;

var
   FProtocoloNFE : String;
   nItemControle : Integer;

procedure TfqrDANFeQRRetrato.Itens;
{var
   nItem : Integer;
   sCST, sBCICMS, sALIQICMS, sVALORICMS, sALIQIPI, sVALORIPI, sDESCRICAOPRODUTO : String;
begin

   if QRNFe.PageNumber > 0 then
      exit;

   cdsItens.CreateDataSet;
   cdsItens.Open;

   for nItem := 0 to ( FNFe.Det.Count - 1 ) do
   begin

      with FNFe.Det.Items[ nItem ] do
      begin

         with Prod do
         begin

            with Imposto.ICMS do
            begin

               sCST       := OrigToStr( orig ) + CSTICMSToStr( CST );

               sBCICMS    := '0.00';
               sALIQICMS  := '0.00';
               sVALORICMS := '0.00';

               sALIQIPI   := '0.00';
               sVALORIPI  := '0.00';

               if (CST = cst00) then
               begin
                  sBCICMS    := FormatFloat( VBC   );
                  sALIQICMS  := FormatFloat( PICMS );
                  sVALORICMS := FormatFloat( VICMS );
               end
               else if (CST = cst10) then
               begin
                  sBCICMS    := FormatFloat( VBC   );
                  sALIQICMS  := FormatFloat( PICMS );
                  sVALORICMS := FormatFloat( VICMS );
               end
               else if (CST = cst20) then
               begin
                  sBCICMS    := FormatFloat( VBC   );
                  sALIQICMS  := FormatFloat( PICMS );
                  sVALORICMS := FormatFloat( VICMS );
               end
               else if (CST = cst30) then
               begin
                  sBCICMS    := FormatFloat( VBCST   );
                  sALIQICMS  := FormatFloat( PICMSST );
                  sVALORICMS := FormatFloat( VICMSST );
               end
               else if (CST = cst40) or (CST = cst41) or (CST = cst50) then
               begin
                  // Campos vazios
               end
               else if (CST = cst51) then
               begin
                  sBCICMS    := FormatFloat( VBC   );
                  sALIQICMS  := FormatFloat( PICMS );
                  sVALORICMS := FormatFloat( VICMS );
               end
               else if (CST = cst60) then
               begin
                  sBCICMS    := FormatFloat( VBCST );
                  sVALORICMS := FormatFloat( VICMSST );
               end
               else if (CST = cst70) then
               begin
                  sBCICMS    := FormatFloat( VBC   );
                  sALIQICMS  := FormatFloat( PICMS );
                  sVALORICMS := FormatFloat( VICMS );
               end
               else if (CST = cst90) then
               begin
                  sBCICMS    := FormatFloat( VBC   );
                  sALIQICMS  := FormatFloat( PICMS );
                  sVALORICMS := FormatFloat( VICMS );
               end;
            end;

            with Imposto.IPI do
            begin
               if (CST = ipi00) or (CST = ipi49) or
                  (CST = ipi50) or (CST = ipi99) then
               begin
                  sALIQIPI  := FormatFloat( PIPI );
                  sVALORIPI := FormatFloat( VIPI );
               end
            end;

            if infAdProd <> '' then
            begin
                sDESCRICAOPRODUTO:= xProd + #13 + infAdProd;
            end else
            begin
                sDESCRICAOPRODUTO:= xProd;
            end;

            cdsItens.Append;
            cdsItens.FieldByName( 'CODIGO'    ).AsString := cProd;
            cdsItens.FieldByName( 'DESCRICAO' ).AsString := sDESCRICAOPRODUTO;
            cdsItens.FieldByName( 'NCM'       ).AsString := NCM;
            cdsItens.FieldByName( 'CFOP'      ).AsString := CFOP;
            cdsItens.FieldByName( 'UNIDADE'   ).AsString := UCom;
            cdsItens.FieldByName( 'QTDE'      ).AsString := FormatFloat( QCom );
            cdsItens.FieldByName( 'VALOR'     ).AsString := FormatFloat( VUnCom );
            cdsItens.FieldByName( 'TOTAL'     ).AsString := FormatFloat( VProd );
            cdsItens.FieldByName( 'CST'       ).AsString := sCST;
            cdsItens.FieldByName( 'BICMS'     ).AsString := sBCICMS;
            cdsItens.FieldByName( 'ALIQICMS'  ).AsString := sALIQICMS;
            cdsItens.FieldByName( 'VALORICMS' ).AsString := sVALORICMS;
            cdsItens.FieldByName( 'ALIQIPI'   ).AsString := sALIQIPI;
            cdsItens.FieldByName( 'VALORIPI'  ).AsString := sVALORIPI;

            cdsItens.FieldByName( 'XPROD'     ).AsString := xProd;
            cdsItens.FieldByName( 'INFADIPROD').AsString := infAdProd;
            cdsItens.Post;

         end;
         
      end;

   end;

   cdsItens.First;
 }
var
  nItem, I, X: Integer;
  sCST, sBCICMS, sALIQICMS, sVALORICMS, sALIQIPI, sVALORIPI, vAux: String;
  sDetalhamentoEspecifico: WideString;
  dPercDesc: Currency;
begin
  cdsItens.Close;
  cdsItens.CreateDataSet;
  cdsItens.Open;

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
          sDetalhamentoEspecifico := '';

          cdsItens.Append;
          cdsItens.FieldByName('CODIGO').AsString := CProd;
          cdsItens.FieldByName('DESCRICAO').AsString := XProd;
          cdsItens.FieldByName('INFADIPROD').AsString := infAdProd;
          cdsItens.FieldByName('NCM').AsString := NCM;
          cdsItens.FieldByName('CFOP').AsString := CFOP;

          if FImprimirDetalhamentoEspecifico then
          begin
            vAux := '';

            // *****************************************************************
            // Veículo
            // *****************************************************************
            if trim(Prod.veicProd.chassi) <> '' then
            begin
              sDetalhamentoEspecifico := #13#10;
              sDetalhamentoEspecifico := sDetalhamentoEspecifico + 'CHASSI: ' + Prod.veicProd.chassi + #13#10;
              sDetalhamentoEspecifico := sDetalhamentoEspecifico + ' COMBUSTÍVEL: ' + Prod.veicProd.CombDescricao + #13#10;
              sDetalhamentoEspecifico := sDetalhamentoEspecifico + ' COR: ' + Prod.veicProd.xCor + #13#10;
              sDetalhamentoEspecifico := sDetalhamentoEspecifico + ' FAB./MOD.: ' + IntToStr(Prod.veicProd.anoFab) + '/' + IntToStr(Prod.veicProd.anoMod) + #13#10;
              sDetalhamentoEspecifico := sDetalhamentoEspecifico + ' Nº DO MOTOR: ' + Prod.veicProd.nMotor;

              //codigo abaixo comentado foi copiado da danfe do FORTES REPORTS
              {
              if Prod.veicProd.tpOP in [toVendaConcessionaria, toFaturamentoDireto, toVendaDireta, toOutros] then
              begin
                case Prod.veicProd.tpOP of
                 toVendaConcessionaria: vAux := '1-VENDA CONCESSIONÁRIA';
                 toFaturamentoDireto:   vAux := '2-FAT. DIRETO CONS. FINAL';
                 toVendaDireta:         vAux := '3-VENDA DIRETA';
                 toOutros:              vAux := '0-OUTROS';
                end;
                sDetalhamentoEspecifico := sDetalhamentoEspecifico + 'TIPO DA OPERAÇÃO: ' + vAux + #13#10;
              end;

              if Prod.veicProd.chassi <> '' then
                sDetalhamentoEspecifico := sDetalhamentoEspecifico + 'CHASSI: ' + Prod.veicProd.chassi + #13#10;
              if Prod.veicProd.cCor <> '' then
                sDetalhamentoEspecifico := sDetalhamentoEspecifico + 'CÓDIGO DA COR: ' + Prod.veicProd.cCor + #13#10;
              if Prod.veicProd.xCor <> '' then
                sDetalhamentoEspecifico := sDetalhamentoEspecifico + 'NOME DA COR: ' + Prod.veicProd.xCor + #13#10;
              if Prod.veicProd.pot <> '' then
                sDetalhamentoEspecifico := sDetalhamentoEspecifico + 'POTÊNCIA DO MOTOR: ' + Prod.veicProd.pot + #13#10;
              if Prod.veicProd.Cilin <> '' then
                sDetalhamentoEspecifico := sDetalhamentoEspecifico + 'CILINDRADAS: ' + Prod.veicProd.Cilin + #13#10;
              if Prod.veicProd.pesoL <> '' then
                sDetalhamentoEspecifico := sDetalhamentoEspecifico + 'PESO LÍQUIDO: ' + Prod.veicProd.pesoL + #13#10;
              if Prod.veicProd.pesoB <> '' then
                sDetalhamentoEspecifico := sDetalhamentoEspecifico + 'PESO BRUTO: ' + Prod.veicProd.pesoB + #13#10;
              if Prod.veicProd.nSerie <> '' then
                sDetalhamentoEspecifico := sDetalhamentoEspecifico + 'NÚMERO DE SÉRIE: ' + Prod.veicProd.nSerie + #13#10;

              if Prod.veicProd.tpComb <> '' then
              begin
                case StrToInt(Prod.veicProd.tpComb) of
                  1: vAux := '01-ÁLCOOL';
                  2: vAux := '02-GASOLINA';
                  3: vAux := '03-DIESEL';
                  4: vAux := '04-GASOGÊNIO';
                  5: vAux := '05-GÁS METANO';
                  6: vAux := '06-ELETRICO/F INTERNA';
                  7: vAux := '07-ELETRICO/F EXTERNA';
                  8: vAux := '08-GASOLINA/GNC';
                  9: vAux := '09-ÁLCOOL/GNC';
                 10: vAux := '10-DIESEL / GNC';
                 11: vAux := '11-VIDE CAMPO OBSERVAÇÃO';
                 12: vAux := '12-ÁLCOOL/GNV';
                 13: vAux := '13-GASOLINA/GNV';
                 14: vAux := '14-DIESEL/GNV';
                 15: vAux := '15-GÁS NATURAL VEICULAR';
                 16: vAux := '16-ÁLCOOL/GASOLINA';
                 17: vAux := '17-GASOLINA/ÁLCOOL/GNV';
                 18: vAux := '18-GASOLINA/ELÉTRICO'
                 else vAux := Prod.veicProd.tpComb;
                end;
                sDetalhamentoEspecifico := sDetalhamentoEspecifico + 'COMBUSTÍVEL: ' + vAux + #13#10;
              end;

              if Prod.veicProd.nMotor <> '' then
                sDetalhamentoEspecifico := sDetalhamentoEspecifico + 'NÚMERO DO MOTOR: ' + Prod.veicProd.nMotor + #13#10;
              if Prod.veicProd.CMT <> '' then
                sDetalhamentoEspecifico := sDetalhamentoEspecifico + 'CAP. MÁX. TRAÇÃO: ' + Prod.veicProd.CMT + #13#10;
              if Prod.veicProd.dist <> '' then
                sDetalhamentoEspecifico := sDetalhamentoEspecifico + 'DISTÂNCIA ENTRE EIXOS: ' + Prod.veicProd.dist + #13#10;
              if IntToStr(Prod.veicProd.anoMod) <> '' then
                sDetalhamentoEspecifico := sDetalhamentoEspecifico + 'ANO DO MODELO: ' + IntToStr(Prod.veicProd.anoMod) + #13#10;
              if IntToStr(Prod.veicProd.anoFab) <> '' then
                sDetalhamentoEspecifico := sDetalhamentoEspecifico + 'ANO DE FABRICAÇÃO: ' + IntToStr(Prod.veicProd.anoFab) + #13#10;
              if Prod.veicProd.tpPint <> '' then
                sDetalhamentoEspecifico := sDetalhamentoEspecifico + 'TIPO DE PINTURA: ' + Prod.veicProd.tpPint + #13#10;

              if (Prod.veicProd.tpVeic IN [1..26]) then
              begin
                case Prod.veicProd.tpVeic of
                  1:  vAux := '01-BICICLETA';
                  2:  vAux := '02-CICLOMOTOR';
                  3:  vAux := '03-MOTONETA';
                  4:  vAux := '04-MOTOCICLETA';
                  5:  vAux := '05-TRICICLO';
                  6:  vAux := '06-AUTOMÓVEL';
                  7:  vAux := '07-MICROONIBUS';
                  8:  vAux := '08-ONIBUS';
                  9:  vAux := '09-BONDE';
                 10: vAux := '10-REBOQUE';
                 11: vAux := '11-SEMI-REBOQUE';
                 12: vAux := '12-CHARRETE';
                 13: vAux := '13-CAMIONETA';
                 14: vAux := '14-CAMINHÃO';
                 15: vAux := '15-CARROÇA';
                 16: vAux := '16-CARRO DE MÃO';
                 17: vAux := '17-CAMINHÃO TRATOR';
                 18: vAux := '18-TRATOR DE RODAS';
                 19: vAux := '19-TRATOR DE ESTEIRAS';
                 20: vAux := '20-TRATOR MISTO';
                 21: vAux := '21-QUADRICICLO';
                 22: vAux := '22-CHASSI/PLATAFORMA';
                 23: vAux := '23-CAMINHONETE';
                 24: vAux := '24-SIDE-CAR';
                 25: vAux := '25-UTILITÁRIO';
                 26: vAux := '26-MOTOR-CASA'
                 else vAux := IntToStr(Prod.veicProd.tpVeic);
                end;
                sDetalhamentoEspecifico := sDetalhamentoEspecifico + 'TIPO DE VEÍCULO: ' + vAux + #13#10;
              end;

              vAux := '';
              case Prod.veicProd.espVeic of
                1: vAux := '01-PASSAGEIRO';
                2: vAux := '02-CARGA';
                3: vAux := '03-MISTO';
                4: vAux := '04-CORRIDA';
                5: vAux := '05-TRAÇÃO';
                6: vAux := '06-ESPECIAL';
                7: vAux := '07-COLEÇÃO'
               else vAux := IntToStr(Prod.veicProd.espVeic);
              end;

              if vAux <> '' then
                sDetalhamentoEspecifico := sDetalhamentoEspecifico + 'ESPÉCIE DO VEÍCULO: ' + vAux + #13#10;

              vAux := '';
              if Prod.veicProd.VIN = 'R' then
                vAux := 'R-REMARCADO'
              else if Prod.veicProd.VIN = 'N' then
                vAux := 'N-NORMAL'
              else
                vAux := Prod.veicProd.VIN;

              if vAux <> '' then
                sDetalhamentoEspecifico := sDetalhamentoEspecifico + 'VIN (CHASSI): ' + vAux + #13#10;

              if Prod.veicProd.condVeic in [cvAcabado,cvInacabado,cvSemiAcabado] then
              begin
                case Prod.veicProd.condVeic of
                  cvAcabado: vAux     := '1-ACABADO';
                  cvInacabado: vAux   := '2-INACABADO';
                  cvSemiAcabado: vAux := '3-SEMI-ACABADO';
                end;
                sDetalhamentoEspecifico := sDetalhamentoEspecifico + 'CONDIÇÃO DO VEÍCULO: ' + vAux + #13#10;
              end;

              if Prod.veicProd.cMod <> '' then
                sDetalhamentoEspecifico := sDetalhamentoEspecifico + 'CÓDIGO MARCA MODELO: ' + Prod.veicProd.cMod + #13#10;

              if Prod.veicProd.cCorDENATRAN <> '' then
              begin
                case StrToInt(Prod.veicProd.cCorDENATRAN) of
                  1:  vAux := '01-AMARELO';
                  2:  vAux := '02-AZUL';
                  3:  vAux := '03-BEGE';
                  4:  vAux := '04-BRANCA';
                  5:  vAux := '05-CINZA';
                  6:  vAux := '06-DOURADA';
                  7:  vAux := '07-GRENÁ';
                  8:  vAux := '08-LARANJA';
                  9:  vAux := '09-MARROM';
                 10: vAux := '10-PRATA';
                 11: vAux := '11-PRETA';
                 12: vAux := '12-ROSA';
                 13: vAux := '13-ROXA';
                 14: vAux := '14-VERDE';
                 15: vAux := '15-VERMELHA';
                 16: vAux := '16-FANTASIA'
                 else vAux := Prod.veicProd.cCorDENATRAN;
                end;
                sDetalhamentoEspecifico := sDetalhamentoEspecifico + 'CÓDIGO COR DENATRAN: ' + vAux + #13#10;
              end;

              if Prod.veicProd.lota > 0 then
                sDetalhamentoEspecifico := sDetalhamentoEspecifico + 'CAPACIDADE MÁXIMA DE LOTAÇÃO: ' + IntToStr(Prod.veicProd.lota) + #13#10;

              if Prod.veicProd.tpRest in [0,1,2,3,4,9] then
              begin
                case Prod.veicProd.tpRest of
                  0: vAux := '0-NÃO HÁ';
                  1: vAux := '1-ALIENAÇÃO FIDUCIÁRIA';
                  2: vAux := '2-RESERVA DE DOMICÍLIO';
                  3: vAux := '3-RESERVA DE DOMÍNIO';
                  4: vAux := '4-PENHOR DE VEÍCULOS';
                  9: vAux := '9-OUTRAS'
                 else vAux := IntToStr(Prod.veicProd.tpRest);
                end;
                sDetalhamentoEspecifico := sDetalhamentoEspecifico + 'RESTRIÇÃO: ' + vAux;
              end;

              }
            end; // if Prod.veicProd.chassi > ''

            // *****************************************************************
            // Medicamento
            // *****************************************************************
            if Prod.med.Count > 0 then
            begin
              for i := 0 to Prod.med.Count - 1 do
              begin
                if Prod.med.Items[i].nLote <> '' then
                  sDetalhamentoEspecifico := sDetalhamentoEspecifico + 'NÚMERO DO LOTE: ' + Prod.med.Items[i].nLote + #13#10;
                if Prod.med.Items[i].qLote > 0 then
                  sDetalhamentoEspecifico := sDetalhamentoEspecifico + 'QUANTIDADE DO LOTE: ' + FormatFloat('###,##0.000', Prod.med.Items[i].qLote) + #13#10;
                if DateToStr(Prod.med.Items[i].dFab) <> '' then
                  sDetalhamentoEspecifico := sDetalhamentoEspecifico + 'DATA DE FABRICAÇÃO: ' + DateToStr(Prod.med.Items[i].dFab) + #13#10;
                if DateToStr(Prod.med.Items[i].dVal) <> '' then
                  sDetalhamentoEspecifico := sDetalhamentoEspecifico + 'DATA DE VALIDADE: ' + DateToStr(Prod.med.Items[i].dVal) + #13#10;
                if Prod.med.Items[i].vPMC > 0 then
                  sDetalhamentoEspecifico := sDetalhamentoEspecifico + 'PREÇO MÁX. CONSUMIDOR: R$ ' + FormatFloat('###,##0.00', Prod.med.Items[i].vPMC) + #13#10;
                if (sDetalhamentoEspecifico > '') and (sDetalhamentoEspecifico <> #13#10) then
                begin
                  if i = Prod.med.Count - 1 then
                    sDetalhamentoEspecifico := sDetalhamentoEspecifico
                  else
                    sDetalhamentoEspecifico := sDetalhamentoEspecifico + #13#10;
                end;
              end;  // for i := 0 to Prod.med.Count - 1
            end; // if Prod.med.Count > 0

            // *****************************************************************
            // Arma
            // *****************************************************************
            if Prod.arma.Count > 0 then
            begin
              for i := 0 to Prod.arma.Count - 1 do
              begin
                if Prod.arma.Items[i].tpArma in [taUsoPermitido, taUsoRestrito] then
                begin
                  case Prod.arma.Items[i].tpArma of
                   taUsoPermitido: vAux := '0-USO PERMITIDO';
                   taUsoRestrito:  vAux := '1-USO RESTRITO';
                  end;
                  sDetalhamentoEspecifico := sDetalhamentoEspecifico + 'TIPO DE ARMA: ' + vAux + #13#10;
                end;
                if Prod.arma.Items[i].nSerie <> '' then
                  sDetalhamentoEspecifico := sDetalhamentoEspecifico + 'No. SÉRIE: ARMA' + Prod.arma.Items[i].nSerie + #13#10;
                if Prod.arma.Items[i].nCano <> '' then
                  sDetalhamentoEspecifico := sDetalhamentoEspecifico + 'No. SÉRIE CANO: ' + Prod.arma.Items[i].nCano + #13#10;
                if Prod.arma.Items[i].descr <> '' then
                  sDetalhamentoEspecifico := sDetalhamentoEspecifico + 'DESCRIÇÃO ARMA: ' + Prod.arma.Items[i].descr + #13#10;
                if (sDetalhamentoEspecifico > '') and (sDetalhamentoEspecifico <> #13#10) then
                begin
                  if i = Prod.arma.Count - 1 then
                    sDetalhamentoEspecifico := sDetalhamentoEspecifico
                  else
                    sDetalhamentoEspecifico := sDetalhamentoEspecifico + #13#10;
                end;
              end;  // for i := 0 to Prod.arma.Count - 1
            end;  // if Prod.arma.Count > 0

            // *****************************************************************
            // Combustivel
            // *****************************************************************
            if Prod.comb.cProdANP > 0 then
            begin
              if Prod.comb.cProdANP > 0 then
                sDetalhamentoEspecifico := sDetalhamentoEspecifico + 'CÓD. PRODUTO ANP: ' + IntToStr(Prod.comb.cProdANP) + #13#10;
              if Prod.comb.CODIF <> '' then
                sDetalhamentoEspecifico := sDetalhamentoEspecifico + 'AUTORIZAÇÃO/CODIF: ' + Prod.comb.CODIF + #13#10;
              if Prod.comb.qTemp > 0 then
                sDetalhamentoEspecifico := sDetalhamentoEspecifico + 'QTD. FATURADA TEMP. AMBIENTE: ' + FormatFloat('###,##0.0000', Prod.comb.qTemp) + #13#10;
              if Prod.comb.UFcons > '' then
                sDetalhamentoEspecifico := sDetalhamentoEspecifico + 'UF DE CONSUMO: ' + Prod.comb.UFcons + #13#10;
              if Prod.comb.CIDE.qBCProd > 0 then
              begin
                if Prod.comb.CIDE.qBCProd > 0 then
                  sDetalhamentoEspecifico := sDetalhamentoEspecifico + 'BASE DE CÁLCULO CIDE: ' + FormatFloat('###,##0.0000', Prod.comb.CIDE.qBCProd) + #13#10;
                if Prod.comb.CIDE.vAliqProd > 0 then
                  sDetalhamentoEspecifico := sDetalhamentoEspecifico + 'ALÍQUOTA CIDE: ' + FormatFloat('###,##0.0000', Prod.comb.CIDE.vAliqProd) + #13#10;
                if Prod.comb.CIDE.vCIDE > 0 then
                  sDetalhamentoEspecifico := sDetalhamentoEspecifico + 'VALOR CIDE: ' + FormatFloat('###,##0.00', Prod.comb.CIDE.vCIDE);
              end;  // if Prod.comb.CIDE.qBCProd > 0
            end;  // if Prod.comb.cProdANP > 0

            cdsItens.FieldByName('DESCRICAO').AsString := xProd + sDetalhamentoEspecifico;

          end // if FImprimirDetalhamentoEspecifico = True
          else cdsItens.FieldByName('DESCRICAO').AsString := XProd;

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

          // Fernando pasqueto para imprimir o desconto no danfe
          if FImprimirDescPorc = True then
          begin
            if vProd > 0 then
              dPercDesc := (vDesc * 100) / vProd
            else dPercDesc := 0;
            cdsItens.FieldByName('VALORDESC').AsString := FormatFloat('###,###,###,##0.00', dPercDesc);
          end
          else cdsItens.FieldByName('VALORDESC').AsString := FormatFloat('###,###,###,##0.00', vDesc);

          cdsItens.FieldByName('UNIDADE').AsString := UCom;
          //Fernando Pasqueto para imprimir total liquido
          if (FImprimirTotalLiquido) and (vDesc > 0) then
            begin
            cdsItens.FieldByName('TOTAL').AsString := FormatFloat('###,###,###,##0.00', vProd - vDesc);
            end
          else cdsItens.FieldByName('TOTAL').AsString := FormatFloat('###,###,###,##0.00', vProd);

          //==============================================================================
          // Em contato com o pessoal da Receita Estadual, foi informado que Ambos os regimes
          // trabalham de mesma forma, deferenciando-se apenas em seus códigos
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

            case CST of
             cst00: begin
                      sBCICMS    := FormatFloat('###,###,###,##0.00', VBC);
                      sALIQICMS  := FormatFloat('###,###,###,##0.00', PICMS);
                      sVALORICMS := FormatFloat('###,###,###,##0.00', VICMS);
                    end;
             cst10: begin
                      sBCICMS    := FormatFloat('###,###,###,##0.00', VBC);
                      sALIQICMS  := FormatFloat('###,###,###,##0.00', PICMS);
                      sVALORICMS := FormatFloat('###,###,###,##0.00', VICMS);
                    end;
             cst20: begin
                      sBCICMS    := FormatFloat('###,###,###,##0.00', VBC);
                      sALIQICMS  := FormatFloat('###,###,###,##0.00', PICMS);
                      sVALORICMS := FormatFloat('###,###,###,##0.00', VICMS);
                    end;
             cst30: begin
                      sBCICMS    := FormatFloat('###,###,###,##0.00', VBCST);
                      sALIQICMS  := FormatFloat('###,###,###,##0.00', PICMSST);
                      sVALORICMS := FormatFloat('###,###,###,##0.00', VICMSST);
                    end;
             cst40,
             cst41,
             cst50: begin
                      sBCICMS    := '0,00';
                      sALIQICMS  := '0,00';
                      sVALORICMS := '0,00';
                    end;
             cst51: begin
                      sBCICMS    := FormatFloat('###,###,###,##0.00', VBC);
                      sALIQICMS  := FormatFloat('###,###,###,##0.00', PICMS);
                      sVALORICMS := FormatFloat('###,###,###,##0.00', VICMS);
                     end;
             cst60: begin
                      sBCICMS    := FormatFloat('###,###,###,##0.00', VBCST);
                      sVALORICMS := FormatFloat('###,###,###,##0.00', VICMSST);
                    end;
             cst70: begin
                      sBCICMS    := FormatFloat('###,###,###,##0.00', VBC);
                      sALIQICMS  := FormatFloat('###,###,###,##0.00', PICMS);
                      sVALORICMS := FormatFloat('###,###,###,##0.00', VICMS);
                    end;
             cst90: begin
                      sBCICMS    := FormatFloat('###,###,###,##0.00', VBC);
                      sALIQICMS  := FormatFloat('###,###,###,##0.00', PICMS);
                      sVALORICMS := FormatFloat('###,###,###,##0.00', VICMS);
                    end;
            end;

            cdsItens.FieldByName('CST').AsString       := sCST;
            cdsItens.FieldByName('BICMS').AsString     := sBCICMS;
            cdsItens.FieldByName('ALIQICMS').AsString  := sALIQICMS;
            cdsItens.FieldByName('VALORICMS').AsString := sVALORICMS;

            lblCST.Caption   := 'CST';
            lblCST.Font.Size := 5;
            //lblCST.Top := 20;

            qrmProdutoCST.DataField := 'CST';
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
              csosn900: begin
                          sBCICMS    := FormatFloat('#,##0.00', VBC);
                          sALIQICMS  := FormatFloat('#,##0.00', PICMS);
                          sVALORICMS := FormatFloat('#,##0.00', VICMS);
                        end;
            end;

            cdsItens.FieldByName('BICMS').AsString     := sBCICMS;
            cdsItens.FieldByName('ALIQICMS').AsString  := sALIQICMS;
            cdsItens.FieldByName('VALORICMS').AsString := sVALORICMS;

            lblCST.Caption   := 'CSOSN';
            lblCST.Font.Size := 4;
            //lblCST.Top       := 22;

            qrmProdutoCST.DataField := 'CSOSN';
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

        cdsItens.FieldByName('ALIQIPI').AsString  := sALIQIPI;
        cdsItens.FieldByName('VALORIPI').AsString := sVALORIPI;

        cdsItens.Post;
      end; // with Prod do
    end; //  with FNFe.Det.Items[nItem] do
  end; //  for nItem := 0 to ( FNFe.Det.Count - 1 ) do

  cdsItens.First;
end;

procedure TfqrDANFeQRRetrato.ProtocoloNFE( const sProtocolo : String );
begin
   FProtocoloNFE := sProtocolo;
end;

procedure TfqrDANFeQRRetrato.QRNFeBeforePrint(Sender: TCustomQuickRep;
  var PrintReport: Boolean);
var
  nRestItens : Integer;
begin
  inherited;
   if FLocalImpCanhoto = 0 then
    begin
      QRShape69.Enabled:=False;
      qrlDataHoraImpressao.Top := QRShape70.Top-10;
      qrlSistema.Top := QRShape70.Top-10;
    end;

   if (FLocalImpCanhoto = 1) or (FResumoCanhoto = False) then
    begin
      QRShape70.Enabled := False;
      qrlDataHoraImpressao.Top := 122;
      qrlSistema.Top := 122;
      QRLabel8.Enabled := False;
      QRShape73.Enabled := False;
      QRShape71.Enabled := False;
      QRLabel33.Enabled := False;
      QRShape72.Enabled := False;
      qrlResumoRodape.Enabled := False;
      qrlRecebemosDe1Rodape.Enabled := False;
      QRShape73.Enabled := False;
      QRLabel28.Enabled := False;
      QRLabel25.Enabled := False;
      qrlNumNF0Rodape.Enabled := False;
      QRLabel23.Enabled := False;
      qrlSERIE0Rodape.Enabled := False;
      QRShape56.Height := 108;
      qrmDadosAdicionais.Height := 135;
      rbDadosAdicionais.Height := 108;
      qrmDadosAdicionais.Height := 91;
      qrlMsgTeste.Top := 17;
    end;

    if (FResumoCanhoto = True) and (FLocalImpCanhoto = 0) then
      begin
        // Arrumar
        //qrlDataHoraImpressao.Top := QRShape70.Top;
        //qrlSistema.Top := QRShape70.Top;

        if FResumoCanhoto_Texto <> '' then
          qrlResumoRodape.Caption := FResumoCanhoto_Texto
        else
        begin
          qrlResumoRodape.Caption := 'EMISSÃO: ' + FormatDateTime('DD/MM/YYYY',
            FNFe.Ide.dEmi) + '  -  ' + 'DEST. / REM.: ' + FNFe.Dest.xNome +
            '  -  ' + 'VALOR TOTAL: R$ ' + FormatFloat(FNFe.Total.ICMSTot.vNF, '###,###,###,##0.00');
          qrlRecebemosDe1Rodape.Caption := StringReplace(qrlRecebemosDe1.Caption, '%s', FNFe.Emit.xNome, [rfReplaceAll]);
          qrlNumNF0Rodape.Caption := FormatFloat('000,000,000', FNFe.Ide.nNF);
          qrlSERIE0Rodape.Caption := IntToStr(FNFe.Ide.serie);
        end; // if FResumoCanhoto_Texto <> ''
      end // if FResumoCanhoto = True
    else
      qrlResumoRodape.Caption := '';

   //Alteracao infoaxel 01/09/2010
   qrbISSQN.Height := 46;
   qrbDadosAdicionais.Height := 135;
   //Fim alteracao infoaxel 01/09/2010
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

   // A linha abaixo foi incluida por: Italo Jurisato Junior
   // 03/08/2010
   // Algumas impressoras não imprimem se a propriedade Report Title estive em
   // branco.
   QRNFe.ReportTitle:='NF-e: ' + FormatFloat( '000,000,000', FNFe.Ide.nNF );

   QRNFe.Page.TopMargin    := FMargemSuperior * 100;
   QRNFe.Page.BottomMargin := FMargemInferior * 100;
   QRNFe.Page.LeftMargin   := FMargemEsquerda * 100;
   QRNFe.Page.RightMargin  := FMargemDireita  * 100;
end;

procedure TfqrDANFeQRRetrato.qrbReciboBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
begin

  if FLocalImpCanhoto = 1 then
  begin
    if FResumoCanhoto = True then
    begin
      qrbRecibo.Enabled := True;
      if FResumoCanhoto_Texto <> '' then
        qrlResumo.Caption := FResumoCanhoto_Texto
      else
      begin
        qrlResumo.Caption := 'EMISSÃO: ' + FormatDateTime('DD/MM/YYYY', FNFe.Ide.dEmi) +
                             '  -  ' + 'DEST. / REM.: ' + FNFe.Dest.xNome +
          '  -  ' + 'VALOR TOTAL: R$ ' + FormatFloat(FNFe.Total.ICMSTot.vNF, '###,###,###,##0.00');
      end; // if FResumoCanhoto_Texto <> ''
    end // if FResumoCanhoto = True
    else
      qrlResumo.Caption := '';
  end
  else
    qrbRecibo.Enabled := False;

  inherited;
   PrintBand := QRNFe.PageNumber = 1;
   qrlNumNF0.Caption := FormatFloat( '000,000,000', FNFe.Ide.nNF );
   qrlSERIE0.Caption := IntToStr( FNFe.Ide.serie );
   qrlRecebemosDe1.Caption      := StringReplace(qrlRecebemosDe1.Caption,'%s',FNFe.Emit.xNome,[rfReplaceAll]);
end;

procedure TfqrDANFeQRRetrato.qrmProdutoDescricaoPrint(sender: TObject;
  var Value: string);
var
	 intTamanho,
	 intLinhasDescricao,
    intLinhasAdicionais,
	 intAlturaLinha : Integer;
begin
inherited;
   // Numero de linhas ocupadas pela descrição
   intTamanho := Length(cdsItens.FieldByName( 'DESCRICAO' ).AsString);
   intLinhasDescricao := Trunc(intTamanho / 35);
   if intLinhasDescricao * 35 < intTamanho then begin
      intLinhasDescricao := intLinhasDescricao + 1;
   end;

	// Numero de Linhas ocupadas pelas Informações Adicionais do Produto
   intLinhasAdicionais := 0;
	if Length(cdsItens.FieldByName( 'INFADIPROD' ).AsString) > 0 then begin
      intTamanho := Length('InfAdic: '+cdsItens.FieldByName( 'INFADIPROD' ).AsString);
  		intLinhasAdicionais := Trunc(intTamanho / 35);
      if intLinhasAdicionais * 35 < intTamanho then begin
         intLinhasAdicionais := intLinhasAdicionais + 1;
      end;
   end;

   intAlturaLinha := (intLinhasDescricao + intLinhasAdicionais) * 12;

    qrs2.Height:= intAlturaLinha;
    qrs3.Height:= intAlturaLinha;
    qrs4.Height:= intAlturaLinha;
    qrs5.Height:= intAlturaLinha;
    qrs6.Height:= intAlturaLinha;
    qrs7.Height:= intAlturaLinha;
    qrs8.Height:= intAlturaLinha;
    qrs9.Height:= intAlturaLinha;
    qrs10.Height:= intAlturaLinha;
    qrs11.Height:= intAlturaLinha;
    qrs12.Height:= intAlturaLinha;
    qrs13.Height:= intAlturaLinha;
    qrs14.Height:= intAlturaLinha;
    qrs15.Height:= intAlturaLinha;
    qrs16.Height:= intAlturaLinha;
    qrs17.Height:= intAlturaLinha;
    qrs18.Top   := intAlturaLinha;

    qrs2.Repaint;
    qrs3.Repaint;
    qrs4.Repaint;
    qrs5.Repaint;
    qrs6.Repaint;
    qrs7.Repaint;
    qrs8.Repaint;
    qrs9.Repaint;
    qrs10.Repaint;
    qrs11.Repaint;
    qrs12.Repaint;
    qrs13.Repaint;
    qrs14.Repaint;
    qrs15.Repaint;
    qrs16.Repaint;
    qrs17.Repaint;
    qrs18.Repaint;
 

   if cdsItensINFADIPROD.AsString<>'' then begin
      Value:=Value+#13+'InfAd: '+cdsItensINFADIPROD.AsString;
   end;
end;


procedure TfqrDANFeQRRetrato.qrbEmitenteDestinatarioBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
var
   x, iQuantDup, vTpEmissao: integer;
   Ok: Boolean;
begin
  inherited;
   PrintBand := QRNFe.PageNumber = 1;
   iQuantDup := 0;

   // Destinatario

   with FNFe.Dest do
   begin
      if Trim( CNPJCPF ) = ''                then qrlDestCNPJ.Caption := ''
      else if Length( Trim( CNPJCPF ) ) > 11 then qrlDestCNPJ.Caption := FormatarCNPJ( CNPJCPF )
      else                                        qrlDestCNPJ.Caption := FormatarCPF( CNPJCPF );

      qrlDestIE.Caption   := IE;
      qrlDestNome.Caption := XNome;
      with EnderDest do
      begin
         qrlDestEndereco.Caption := XLgr + IfThen ( Nro = '0', '', ', ' + Nro );
         qrlDestBairro.Caption   := XBairro;
         qrlDestCidade.Caption   := XMun;
         qrlDestUF.Caption       := UF;
         qrlDestCEP.Caption      := NotaUtil.FormatarCEP( FormatFloat( '00000000', CEP ) );
         qrlDestFONE.Caption     := NotaUtil.FormatarFone( Fone );
      end;
   end;

   // Emissao, saida

   qrlEmissao.Caption   := FormatDate(DateToStr(FNFe.Ide.dEmi));
   qrlSaida.Caption     := IfThen( FNFe.Ide.DSaiEnt <> 0, FormatDate(DateToStr(FNFe.Ide.dSaiEnt)));
   // Alterado por Italo em 22/03/2011
   if FNFe.infNFe.Versao > 3
    then qrlHoraSaida.Caption := IfThen( FNFe.Ide.dSaiEnt <> 0, FormatDateTime('hh:mm:ss', FNFe.Ide.dSaiEnt))
    else qrlHoraSaida.Caption := IfThen( FNFe.Ide.hSaiEnt <> 0, FormatDateTime('hh:mm:ss', FNFe.Ide.hSaiEnt));

   // Faturas

   // Zera
   for x := 1 to 15 do
   begin
      TQRLabel( FindComponent( 'qrlFatNum'   + intToStr( x ) ) ).Caption := '';
      TQRLabel( FindComponent( 'qrlFatData'  + intToStr( x ) ) ).Caption := '';
      TQRLabel( FindComponent( 'qrlFatValor' + intToStr( x ) ) ).Caption := '';
   end;

   // Incluido por Italo em 13/07/2011
   if FNFe.Cobr.Dup.Count > 0 then 
		 Ok := (FormatDateTime('DD/MM/YYYY', FNFe.Ide.dEmi) = FormatDateTime('DD/MM/YYYY', FNFe.Cobr.Dup[0].dVenc))
   else 
		 Ok := True;

   // Alterado por Italo em 27/05/2011
   // TpcnIndicadorPagamento = (ipVista, ipPrazo, ipOutras);
   if (FNFe.Ide.indPag = ipVista) and Ok
    then
      TQRLabel( FindComponent( 'qrlFatNum1' ) ).Caption := 'PAGAMENTO À VISTA'
    else begin
     // Adiciona
     if FNFe.Cobr.Dup.Count > 15 then
      iQuantDup := 15
     else
      iQuantDup := FNFe.Cobr.Dup.Count;

     for x := 0 to (iQuantDup - 1) do with FNFe.Cobr.Dup[ x ] do
     begin
        TQRLabel( FindComponent( 'qrlFatNum'   + intToStr ( x + 1 ) ) ).Caption := NDup;
        TQRLabel( FindComponent( 'qrlFatData'  + intToStr ( x + 1 ) ) ).Caption := FormatDate( DateToStr(DVenc) );
        TQRLabel( FindComponent( 'qrlFatValor' + intToStr ( x + 1 ) ) ).Caption := FormatFloat(VDup);
     end;
    end;

   // Impostos

   with FNFe.Total.ICMSTot do
   begin
      qrlBaseICMS.Caption      := FormatFloat( VBC );
      qrlValorICMS.Caption     := FormatFloat( VICMS );
      qrlBaseICMST.Caption     := FormatFloat( VBCST );
      qrlValorICMST.Caption    := FormatFloat( VST );
      qrlTotalProdutos.Caption := FormatFloat( VProd );
      qrlValorFrete.Caption    := FormatFloat( VFrete );
      qrlValorSeguro.Caption   := FormatFloat( VSeg );
      qrlDescontos.Caption     := FormatFloat( VDesc );
      qrlAcessorias.Caption    := FormatFloat( VOutro );
      qrlValorIPI.Caption      := FormatFloat( VIPI );
      qrlTotalNF.Caption       := FormatFloat( VNF );
      // Incluido por Italo em 29/04/2013 conforme a NT 2013/003
      qrlValorTotTrib.Caption  := FormatFloat( vTotTrib );
   end;

   // Transporte

   with FNFe.Transp do
   begin
     case ModFrete of
      mfContaEmitente     : qrlTransModFrete.Caption := '0-Emitente';
      mfContaDestinatario : qrlTransModFrete.Caption := '1-Destinat.';
      mfContaTerceiros    : qrlTransModFrete.Caption := '2-Terceiros';
      mfSemFrete          : qrlTransModFrete.Caption := '9-Sem Frete';
     end;
      // qrlTransModFrete.Caption   := modFreteToStr( ModFrete );
      qrlTransCodigoANTT.Caption := '';
      qrlTransPlaca.Caption      := '';
      qrlTransUFPlaca.Caption    := '';

      with Transporta do
      begin
         if Trim( CNPJCPF ) = ''                then qrlTransCNPJ.Caption := '' 
         else if Length( Trim( CNPJCPF ) ) > 11 then qrlTransCNPJ.Caption := FormatarCNPJ( CNPJCPF )
         else                                        qrlTransCNPJ.Caption := FormatarCPF( CNPJCPF );

         qrlTransNome.Caption     := XNome;
         qrlTransIE.Caption       := IE;
         qrlTransEndereco.Caption := XEnder;
         qrlTransCidade.Caption   := XMun;
         qrlTransUF.Caption       := UF;
      end;
   end;

   with FNFe.Transp.VeicTransp do
   begin
      qrlTransPlaca.Caption   :=  Placa;
      qrlTransUFPlaca.Caption :=  UF;
      // Alterado por Italo em 11/10/2011
      qrlTransCodigoANTT.Caption := RNTC;
   end;

   if FNFe.Transp.Vol.Count > 0 then with FNFe.Transp.Vol[0] do
   begin
      qrlTransQTDE.Caption      := IntToStr(QVol);
      qrlTransEspecie.Caption   := Esp;
      qrlTransMarca.Caption     := Marca;
      qrlTransNumeracao.Caption := NVol;
      qrlTransPesoLiq.Caption   := FormatFloat( PesoL, '#,0.000#' );
      qrlTransPesoBruto.Caption := FormatFloat( PesoB, '#,0.000#' );
   end
   else
   begin
      qrlTransQTDE.Caption      := '';
      qrlTransEspecie.Caption   := '';
      qrlTransMarca.Caption     := '';
      qrlTransNumeracao.Caption := '';
      qrlTransPesoLiq.Caption   := '';
      qrlTransPesoBruto.Caption := '';
   end;

   // Mensagem para modo Homologacao.

   if FNFe.Ide.tpAmb = taHomologacao then
    begin
      qrlMsgTeste.Caption := 'AMBIENTE DE HOMOLOGAÇÃO - SEM VALOR FISCAL';
      qrlMsgTeste.Enabled := True;
      qrlMsgTeste.Visible := True;
    end;

   if FNFe.procNFe.cStat > 0 then
    begin
      if ((FNFe.procNFe.cStat in [101, 151, 155]) or (FNFeCancelada)) then
      begin
        qrlMsgTeste.Caption := 'NF-e CANCELADA';
        qrlMsgTeste.Visible := True;
        qrlMsgTeste.Enabled := True;
      end;
      // Alterado de 102 para 110 por Italo em 27/01/2012
      case FNFe.procNFe.cStat of
      110,205,301,302:begin
          qrlMsgTeste.Caption := 'NF-e DENEGADA';
          qrlMsgTeste.Visible := True;
          qrlMsgTeste.Enabled := True;
        end;
      end;

      // Alterado de 102 para 110 por Italo em 27/01/2012
      if not FNFe.procNFe.cStat in [100, 101, 110, 151, 155] then
      begin
        qrlMsgTeste.Caption := FNFe.procNFe.xMotivo;
        qrlMsgTeste.Visible := True;
        qrlMsgTeste.Enabled := True;
      end;
    end;


    if FNFe.Ide.tpEmis = teContingencia then
      vTpEmissao:=2
    else
    if FNFe.Ide.tpEmis in [teFSDA, teSVCAN, teSVCRS] then
      vTpEmissao:=5;

    case vTpEmissao of
      2:begin
        qrlMsgTipoEmissao.Caption := 'DANFE em Contingência - impresso em decorrência de problemas técnicos';
        qrlMsgTipoEmissao.Visible := True;
        qrlMsgTipoEmissao.Enabled := True;
      end;
      5:begin
        qrlMsgTipoEmissao.Caption := 'DANFE em Contingência - impresso em decorrência de problemas técnicos';
        qrlMsgTipoEmissao.Visible := True;
        qrlMsgTipoEmissao.Enabled := True;
      end;
    end;

    //Incluido por Luis Fernando Costa
    with FNFe.Entrega do
      begin
        if CNPJCPF <> '' then
          qrlDestCNPJEnt.Caption     := CNPJCPF
        else
          qrlDestCNPJEnt.Caption     := '';

        if xLgr <> '' then
          qrlDestEnderecoEnt.Caption := xLgr+', '+nro
        else
          qrlDestEnderecoEnt.Caption := '';

        if xBairro <> '' then
          qrlDestBairroEnt.Caption   := xBairro
        else
          qrlDestBairroEnt.Caption   := '';

        if xMun <> '' then
          qrlDestCidadeEnt.Caption   := xMun+'-'+UF
        else
          qrlDestCidadeEnt.Caption   := '';
      end;

    qrlMsgTeste.Repaint;
      
end;

procedure TfqrDANFeQRRetrato.qrbHeaderItensBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
begin
  inherited;
if FImprimirDescPorc then
  QRLabel75.Caption := '% DESCONTO'
ELSE QRLabel75.Caption := 'DESCONTO';
end;

procedure TfqrDANFeQRRetrato.qrbISSQNBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
begin
  inherited;
   PrintBand := QRNFe.PageNumber = 1;
   if not PrintBand then
      qrbISSQN.Height := 0
   else
   begin
      if FNFE.Total.ISSQNtot.vISS > 0 then
      begin
         qrlInscMunicipal.Caption := FNFE.Emit.IM;
         qrlTotalServicos.Caption := FormatFloat( FNFE.Total.ISSQNtot.vServ );
         qrlBaseISSQN.Caption     := FormatFloat( FNFE.Total.ISSQNtot.vBC );
         qrlValorISSQN.Caption    := FormatFloat( FNFE.Total.ISSQNtot.vISS );
      end
      else
      begin
         qrbISSQN.Height          := 0;
         qrlInscMunicipal.Caption := '';
         qrlTotalServicos.Caption := '';
         qrlBaseISSQN.Caption     := '';
         qrlValorISSQN.Caption    := '';
      end;
   end;
end;

procedure TfqrDANFeQRRetrato.qrbDadosAdicionaisBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
var
   i: Integer;
   sIndProc: String;
begin
  inherited;
    PrintBand := QRNFe.PageNumber = 1;
    if not PrintBand then
        qrbDadosAdicionais.Height := 0
    else
    with FNFe.InfAdic do
    begin
        qrmDadosAdicionais.Lines.BeginUpdate;
        qrmDadosAdicionais.Lines.Clear;

        //**********************************************************************
        // informacoes de uso livre do fisco
        for i := 0 to obsFisco.Count-1 do with obsFisco.Items[i] do
        begin
            qrmDadosAdicionais.Lines.Add( StringReplace( XCampo, '&lt;BR&gt;', #13#10, [rfReplaceAll,rfIgnoreCase] )+': '+
                                           StringReplace( XTexto, '&lt;BR&gt;', #13#10, [rfReplaceAll,rfIgnoreCase] ) );
        end;

        //**********************************************************************
        // informacoes de uso livre do contribuinte
        for i := 0 to ObsCont.Count-1 do with ObsCont.Items[i] do
        begin
            qrmDadosAdicionais.Lines.Add( StringReplace( XCampo, '&lt;BR&gt;', #13#10, [rfReplaceAll,rfIgnoreCase] )+': '+
                                           StringReplace( XTexto, '&lt;BR&gt;', #13#10, [rfReplaceAll,rfIgnoreCase] ) );
        end;

        //**********************************************************************
        // informacoes do grupo de processo referenciado
        for i := 0 to procRef.Count-1 do with procRef.Items[i] do
        begin
          case indProc of
            ipSEFAZ:           sIndProc := 'SEFAZ';
            ipJusticaFederal:  sIndProc := 'JUSTIÇA FEDERAL';
            ipJusticaEstadual: sIndProc := 'JUSTIÇA ESTADUAL';
            ipSecexRFB:        sIndProc := 'SECEX / RFB';
            ipOutros:          sIndProc := 'OUTROS';
          end;

            qrmDadosAdicionais.Lines.Add( StringReplace( 'PROCESSO OU ATO CONCESSÓRIO Nº: ' +
                                                         nProc, '&lt;BR&gt;', #13#10, [rfReplaceAll,rfIgnoreCase] )+' - '+
                                           StringReplace( 'ORIGEM: ' +
                                                          sIndProc , '&lt;BR&gt;', #13#10, [rfReplaceAll,rfIgnoreCase] ) );
        end;

        //**********************************************************************
        // informacoes complementares emitente
        if infCpl <> '' then
        begin
            qrmDadosAdicionais.Lines.Add(StringReplace( InfCpl, '&lt;BR&gt;', #13#10, [rfReplaceAll,rfIgnoreCase] ) );
        end;

        //**********************************************************************
        // informacoes complementares interesse ao fisco
        if infAdFisco <> '' then
        begin
            qrmDadosAdicionais.Lines.Add(StringReplace( 'INFORMAÇÕES ADICIONAIS DE INTERESSE DO FISCO: ' +
                                                       infAdFisco,'&lt;BR&gt;', #13#10, [rfReplaceAll,rfIgnoreCase] ));
        end;
        //**********************************************************************
        // local de retirada
        with FNFe.Retirada do
        begin
            if xLgr <> '' then
            begin
                qrmDadosAdicionais.Lines.Add(   'LOCAL DE RETIRADA: '                                                          +
                                                StringReplace( xLgr, '&lt;BR&gt;',      #13#10, [rfReplaceAll,rfIgnoreCase] )  + ', ' +
                                                StringReplace( nro, '&lt;BR&gt;',       #13#10, [rfReplaceAll,rfIgnoreCase] )  + '/' +
                                                StringReplace( xCpl, '&lt;BR&gt;',      #13#10, [rfReplaceAll,rfIgnoreCase] )  + ' ' +
                                                StringReplace( xBairro, '&lt;BR&gt;',   #13#10, [rfReplaceAll,rfIgnoreCase] )  + ' - ' +
                                                StringReplace( xMun, '&lt;BR&gt;',      #13#10, [rfReplaceAll,rfIgnoreCase] )  + ' ' +
                                                StringReplace( UF, '&lt;BR&gt;',        #13#10, [rfReplaceAll,rfIgnoreCase] ));
            end;
        end;
        //**********************************************************************
        // local de entrega
        with FNFe.Entrega do
        begin
            if xLgr <> '' then
            begin
                qrmDadosAdicionais.Lines.Add(   'LOCAL DE ENTREGA: '                                                          +
                                                StringReplace( xLgr, '&lt;BR&gt;',      #13#10, [rfReplaceAll,rfIgnoreCase] )  + ', ' +
                                                StringReplace( nro, '&lt;BR&gt;',       #13#10, [rfReplaceAll,rfIgnoreCase] )  + '/' +
                                                StringReplace( xCpl, '&lt;BR&gt;',      #13#10, [rfReplaceAll,rfIgnoreCase] )  + ' ' +
                                                StringReplace( xBairro, '&lt;BR&gt;',   #13#10, [rfReplaceAll,rfIgnoreCase] )  + ' - ' +
                                                StringReplace( xMun, '&lt;BR&gt;',      #13#10, [rfReplaceAll,rfIgnoreCase] )  + ' ' +
                                                StringReplace( UF, '&lt;BR&gt;',        #13#10, [rfReplaceAll,rfIgnoreCase] ));
            end;
        end;

        if FNFe.Ide.tpEmis in [teContingencia, teFSDA, teSVCAN, teSVCRS] then
            qrmDadosAdicionais.Lines.Add('DANFE em Contingência - Impresso em decorrência de problemas técnicos.');
        //**********************************************************************
      qrmDadosAdicionais.Lines.Text:=StringReplace(qrmDadosAdicionais.Lines.Text,';',#13,[rfReplaceAll]);
      qrmDadosAdicionais.Lines.EndUpdate;

        // imprime data e hora da impressao
        QrlDataHoraImpressao.Caption:= 'DATA E HORA DA IMPRESSÃO: ' + FormatDateTime('dd/mm/yyyy hh:nn',Now);

        // imprime usuario
        if FUsuario <> '' then
        begin
            QrlDataHoraImpressao.Caption:= QrlDataHoraImpressao.Caption + '   USUÁRIO: ' + FUsuario;
        end;

        // imprime sistema
        //Ajustado por Luis Fernando - para que fique uma msg livre - 22/01/2013
        if FSistema <> '' then
          qrlSistema.Caption:= FSistema
        else
          qrlSistema.Caption:= '';

    end;
end;

procedure TfqrDANFeQRRetrato.qrbDadosDanfeBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
var
    strChaveContingencia: string;
begin
  inherited;

   //hrsoft 4/8/2010
   FTotalPages := HrTotalPages;
   //fim hrsoft 4/8/2010

   qrlPageNumber.Caption := format ( '%2.2d', [ QRNFe.PageNumber ] )
                    + '/' + format ( '%2.2d', [ FTotalPages ] );

   if QRNFe.PageNumber = 1 then
   begin
      if (FLogo <> '') and FilesExists(FLogo) then
       begin
        qriLogo.Picture.LoadFromFile(FLogo);
       end;

      // Alterado a posição por Italo em 22/06/2012
      // conforme problema detectado por Wilson
      qrlCNPJ.Caption              := FormatarCNPJ( FNFe.Emit.CNPJCPF  );
      qrlInscrEstSubst.caption     := FNFe.Emit.IEST;
      qrlInscricaoEstadual.Caption := FNFe.Emit.IE;

      // Inclido por Italo em 18/06/2012
      if FExpandirLogoMarca then
       begin
        qriLogo.top:=13;
        qriLogo.Left:=2;
        qriLogo.Height:=108;
        qriLogo.Width:=284;
        qriLogo.Stretch:=true;
        qrmEmitente.Enabled:=False;
        qrmEndereco.Enabled:=False;
        qrlFone.Enabled:=False;
       end;

      // Incluido por Italo em 18/06/2012
      if not FExpandirLogoMarca then
       begin
        qrmEmitente.Enabled:=True;
        qrmEndereco.Enabled:=True;
        qrlFone.Enabled:=True;
        // Emitente
        with FNFe.Emit do
        begin
         qrmEmitente.Lines.Text := XNome;
         with EnderEmit do
         begin
            qrmEndereco.Lines.Clear;
//            qrmEndereco.Lines.add ( XLgr + IfThen ( Nro = '0', '', ', ' + Nro ) + ' ' + XCpl + ' ' + XBairro );
//            qrmEndereco.Lines.add ( XMun + ' - ' + UF );
//            qrmEndereco.Lines.add ( 'CEP ' + NotaUtil.FormatarCEP( FormatFloat( '00000000', CEP ) ) );
            // Alterado por Italo em 23/03/2011
            qrmEndereco.Lines.Add(XLgr + IfThen(Nro = '0', '', ', ' + Nro));
            if XCpl <> '' then qrmEndereco.Lines.Add(XCpl);
            if XBairro <> '' then qrmEndereco.Lines.Add(XBairro);
            qrmEndereco.Lines.Add(XMun + ' - ' + UF);
            qrmEndereco.Lines.Add('CEP: ' + NotaUtil.FormatarCEP(FormatFloat( '00000000', CEP )));
            // Alterado por Italo em 23/03/2011
            if Trim(FSite) <> '' then qrmEndereco.Lines.Add ('SITE: ' + FSite);
            // Alterado por Italo em 18/06/2012
            if Trim(FEmail) <> '' then qrmEndereco.Lines.Add ('E-MAIL: ' + FEmail);
            // telefone
            qrlFone.Caption:= '';
            // Alterado por Italo em 18/06/2012
            if (fone <> '') and (FFax = '') then qrlFone.Caption := 'FONE: ' + NotaUtil.FormatarFone( Fone );
            // Alterado por Italo em 18/06/2012
            if (FFax <> '') and (fone = '') then qrlFone.Caption:= 'FAX: ' + FFax;
            // Alterado por Italo em 18/06/2012
            if (FFax <> '') and (fone <> '') then qrlFone.Caption:= 'FONE: ' +
                            NotaUtil.FormatarFone( Fone ) + #13 +'FAX: ' + FFax;
         end;
       end;
      end;

      // Danfe
      qrlEntradaSaida.Caption := tpNFToStr( FNFe.Ide.tpNF );
      qrlNumNF1.Caption       := FormatFloat( '000,000,000', FNFe.Ide.nNF );
      qrlSERIE1.Caption       := IntToStr( FNFe.Ide.serie );
      qrlChave.Caption        := NotaUtil.FormatarChaveAcesso( Copy ( FNFe.InfNFe.Id, 4, 44 ) );
      qrlNatOperacao.Caption  := FNFe.Ide.natOp;
      SetBarCodeImage( Copy ( FNFe.InfNFe.Id, 4, 44 ), qriBarCode );

        // Normal **************************************************************
        if FNFe.Ide.tpEmis in [teNormal, teSCAN] then
        begin
            if FNFe.procNFe.cStat = 100 then
              qrlDescricao.Caption:= 'PROTOCOLO DE AUTORIZAÇÃO DE USO';

            // Alterado por Italo em 29/11/2012
            // if FNFe.procNFe.cStat = 101 then
            if FNFe.procNFe.cStat in [101, 151, 155] then
              qrlDescricao.Caption:= 'PROTOCOLO DE HOMOLOGAÇÃO DE CANCELAMENTO';

            // Alterado de 102 para 110 por Italo em 27/01/2012
            case FNFe.procNFe.cStat of
            110,205,301,302:qrlDescricao.Caption:= 'PROTOCOLO DE DENEGAÇÃO DE USO';
            end;

            if FProtocoloNFE <> '' then
                qrlProtocolo.Caption        := FProtocoloNFE
            else
                qrlProtocolo.Caption        :=  FNFe.procNFe.nProt + ' ' +
                                                SeSenao(FNFe.procNFe.dhRecbto <> 0, DateTimeToStr(FNFe.procNFe.dhRecbto),'');
            IF FNFe.procNFe.cStat = 0 THEN
              BEGIN
              qrlProtocolo.Caption := 'NF-E NÃO ENVIADA PARA SEFAZ';
              qrlProtocolo.Font.Color := clRed;
              END
            ELSE qrlProtocolo.Font.Color := clWindowText;
            qriBarCodeContingencia.Visible  := False;
            qrlMsgAutorizado.Enabled        := True;
        end;
        // Contingencia ********************************************************
        if FNFe.Ide.tpEmis in [teContingencia, teFSDA, teSVCAN, teSVCRS] then
        begin
            strChaveContingencia:= NotaUtil.GerarChaveContingencia(FNFe);
            SetBarCodeImage(strChaveContingencia,qriBarCodeContingencia);
            qriBarCodeContingencia.Visible  := True;
            qrlMsgAutorizado.Enabled        := False;
            qrlDescricao.Caption            := 'DADOS DA NF-E';
            qrlProtocolo.Caption            := NotaUtil.FormatarChaveContigencia(strChaveContingencia);
        end;
    //************************************************************************
    end;
end;

procedure TfqrDANFeQRRetrato.qrbItensBeforePrint(Sender: TQRCustomBand;
  var PrintBand: Boolean);
begin
  inherited;
  {if Length(cdsItensCODIGO.AsString) > 10 then
    qrmProdutoCodigo.Font.Size:=5
  else
    qrmProdutoCodigo.Font.Size:=6;}
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

end.
