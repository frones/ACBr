{******************************************************************************}
{ Projeto: Aplicação de demonstração ACBrPIXCD Matera                          }
{                                                                              }
{ Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{ - Elias César Vieira                                                         }
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

unit PixCDMatera;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls, Buttons, Spin, ACBrPIXCD, ACBrPIXPSPMatera, ACBrCEP,
  ACBrOpenSSLUtils, Clipbrd, Grids, ACBrImage, ACBrDelphiZXingQRCode,
  ACBrUtil.Math, ACBrSchemasMatera, IniFiles,
  {$IfDef FPC}
  DateTimePicker,
  {$EndIf}
  ACBrSocket, ACBrBase, ImgList;

const
  cURL_ACBR = 'https://projetoacbr.com.br/pix/';
  CURL_MateraPagto = 'https://flagship-payment-app.vercel.app/';
  cMoeda = 'BRL';
  cPais = 'BRA';
  CMaxConsultas = 36;

type

  TFluxoPagtoDados = record
    QRCode: String;
    Total: Double;               
    transactionID: String;
    transactionIDEstorno: String;
    StatusCobranca: TMateraTransactionStatus;
    StatusDevolucao: TMateraTransactionStatus;
    EmErro: Boolean;
    QtdConsultas: Integer;
  end;

  { TfrPixCDMatera }

  TfrPixCDMatera = class(TForm)
    ACBrCEP1: TACBrCEP;
    ACBrOpenSSLUtils1: TACBrOpenSSLUtils;
    ACBrPixCD1: TACBrPixCD;
    ACBrPSPMatera1: TACBrPSPMatera;
    btAcharArqCertificado: TSpeedButton;
    btAcharChavePrivada: TSpeedButton;
    btChavePIXConsultar: TBitBtn;
    btChavePIXCriarExternalId: TSpeedButton;
    btChavePIXExcluir: TBitBtn;
    btChavePIXIncluir: TBitBtn;
    btCobCopiaECola: TSpeedButton;
    btConsultarAliasRetirada: TBitBtn;
    btConsultarCob: TBitBtn;
    btConsultarExtratoEC: TBitBtn;
    btConsultarExtratoMediator: TBitBtn;
    btConsultarMotivosDevolucoes: TBitBtn;
    btConsultarSaldoEC: TBitBtn;
    btConsultarSaldoMediator: TBitBtn;
    btContaCriar: TBitBtn;
    btContaCriarExternalID: TSpeedButton;
    btContaCriarLimparDados: TBitBtn;
    btContaCriarPreencherDados: TBitBtn;
    btContaCriarRepresentanteFoto: TSpeedButton;
    btContaCriarRepresentanteMostrarRGFotoFrente: TSpeedButton;
    btContaCriarRepresentanteMostrarRGFotoFrente1: TSpeedButton;
    btContaCriarRepresentanteRGFotoFrente: TSpeedButton;
    btContaCriarRepresentanteRGFotoVerso: TSpeedButton;
    btContaCriarRepresentanteMostrarRGFotoVerso: TSpeedButton;
    btDevolucao: TBitBtn;
    btDevolucaoLimparDados: TBitBtn;
    btDevolucaoPreencherDados: TBitBtn;
    btFluxoCancelarVenda: TBitBtn;
    btFluxotransactionID: TSpeedButton;
    btQRCodeGerarExternalID1: TSpeedButton;
    btContaConsultar: TBitBtn;
    btContaInativar: TBitBtn;
    btCriarCobranca: TBitBtn;
    btFluxoCancelarConsulta: TBitBtn;
    btFluxoCopiaECola: TSpeedButton;
    btFluxoEstornarPagto: TBitBtn;
    btFluxoFecharVenda: TBitBtn;
    btFluxoNovaVenda: TBitBtn;
    btFluxoPagar: TBitBtn;
    btFluxoTentarNovamente: TBitBtn;
    btLerParametros: TBitBtn;
    btLogArquivo: TSpeedButton;
    btLogGerencialLimpar: TBitBtn;
    btLogOperacoesLimpar: TBitBtn;
    btProxyVerSenha: TSpeedButton;
    btQRCodeCriarLimparDados: TBitBtn;
    btQRCodeCriarPreencherDados: TBitBtn;
    btQRCodeGerarExternalID: TSpeedButton;
    btRetirada: TBitBtn;
    btretiradaConsultaLimparDados: TBitBtn;
    btRetiradaConsultaPreencherDados: TBitBtn;
    btRetiradaGerarExternalID: TSpeedButton;
    btretiradaLimparDados: TBitBtn;
    btRetiradaPreencherDados: TBitBtn;
    btSalvarParametros: TBitBtn;
    btVoltar: TBitBtn;
    cbAccountId: TComboBox;
    cbAmbiente: TComboBox;
    cbChavePIX: TComboBox;
    cbContaCriarTipoCliente: TComboBox;
    cbCriarContaTipoConta: TComboBox;
    cbRetiradaAccountTypeDestination: TComboBox;
    cbRetiradaPersonType: TComboBox;
    cbTipoMediatorFee: TComboBox;
    cbTipoMediatorFeeEstorno: TComboBox;
    cbLogNivel: TComboBox;
    cbQRCodeTipoMediatorFee: TComboBox;
    cbTipoMediatorFeeDevolucao: TComboBox;
    cbRetiradaTipoRetirada: TComboBox;
    chkQRCodeADshowToPayer: TCheckBox;
    cbQRCodeTipoCobranca: TComboBox;
    cbDevolucaoReasonCode: TComboBox;
    edArqCertificado: TEdit;
    edArqChavePrivada: TEdit;
    edChavePIXConsultar: TEdit;
    edChavePIXExcluir: TEdit;
    edChavePIXExcluirAccountId: TEdit;
    edChavePIXIncluirExternalId: TEdit;
    edChavePIXIncluirAccountId: TEdit;
    edCNPJ: TEdit;
    edCobCopiaECola: TEdit;
    edconsultaEnding: TDateTimePicker;
    edconsultaEnding1: TDateTimePicker;
    edConsultarCobTransactionID: TEdit;
    edConsultarEXIntegradorAccountID: TEdit;
    edConsultarSaldoIntegradorAccountID: TEdit;
    edConsultaStart: TDateTimePicker;
    edConsultaStart1: TDateTimePicker;
    edContaCriarBairro: TEdit;
    edContaCriarCelular: TEdit;
    edContaCriarCEP: TEdit;
    edContaCriarCidade: TEdit;
    edContaCriarComplemento: TEdit;
    edContaCriarEmail: TEdit;
    edContaCriarExternalID: TEdit;
    edContaCriarFundacao: TDateTimePicker;
    edContaCriarLogradouro: TEdit;
    edContaCriarNascimento: TDateTimePicker;
    edContaCriarNomeCliente: TEdit;
    edContaCriarNomeEmpresa: TEdit;
    edContaCriarNumero: TEdit;
    edContaCriarRepresentanteBairro: TEdit;
    edContaCriarRepresentanteCelular: TEdit;
    edContaCriarRepresentanteCEP: TEdit;
    edContaCriarRepresentanteCidade: TEdit;
    edContaCriarRepresentanteCPF: TEdit;
    edContaCriarRepresentanteEmail: TEdit;
    edContaCriarRepresentanteFoto: TEdit;
    edContaCriarRepresentanteLogradouro: TEdit;
    edContaCriarRepresentanteMae: TEdit;
    edContaCriarRepresentanteNome: TEdit;
    edContaCriarRepresentanteNumero: TEdit;
    edContaCriarRepresentanteRGFotoFrente: TEdit;
    edContaCriarRepresentanteRGFotoVerso: TEdit;
    edContaCriarRepresentanteUF: TEdit;
    edContaCriarUF: TEdit;
    edDevolucaoCobTransactionID: TEdit;
    edDevolucaoExternalID: TEdit;
    edFluxoValor: TEdit;
    edMediatorFee: TEdit;
    edMediatorFeeEstorno: TEdit;
    edDevolucaoValor: TEdit;
    edContaConsultarAccountId: TEdit;
    edContaInativarAccountId: TEdit;
    edFluxoClienteDoc: TEdit;
    edFluxoClienteNome: TEdit;
    edFluxotransactionID: TEdit;
    edFluxoCopiaECola: TEdit;
    edLogArquivo: TEdit;
    edProxyHost: TEdit;
    edProxyPorta: TSpinEdit;
    edProxySenha: TEdit;
    edProxyUsuario: TEdit;
    edPSPClientID: TEdit;
    edPSPClientSecret: TEdit;
    edPSPSecretKey: TEdit;
    edQRCodeADcontent: TEdit;
    edQRCodeADname: TEdit;
    edQRCodeCallBack: TEdit;
    edQRCodediscountsdate: TDateTimePicker;
    edQRCodediscountsmodality: TEdit;
    edQRCodediscountsvaluePerc: TEdit;
    edQRCodedueDate: TDateTimePicker;
    edQRCodeExternalID: TEdit;
    edQRCodefinesmodality: TEdit;
    edQRCodefinesvaluePerc: TEdit;
    edQRCodeinterestsmodality: TEdit;
    edQRCodeinterestsvaluePerc: TEdit;
    edMediatorFeeDevolucao: TEdit;
    edQRCodepayerCEP: TEdit;
    edQRCodepayercity: TEdit;
    edQRCodePayercpfcnpj: TEdit;
    edQRCodePayerName: TEdit;
    edQRCodepayerstreet: TEdit;
    edQRCodepayeruf: TEdit;
    edQRCoderecipientComment: TEdit;
    edQRCodereductionmodality: TEdit;
    edQRCodereductionvaluePerc: TEdit;
    edQRCodeValor: TEdit;
    edQRCodeMediatorFee: TEdit;
    edRetiradaAccountDestinationAccount: TEdit;
    edRetiradaAccountdestinationBranch: TEdit;
    edRetiradaAliasDestinatario: TEdit;
    edRetiradaConsultaAliasAccountID: TEdit;
    edRetiradaConsultaaliasAliasDestinatario: TEdit;
    edRetiradaendToEndId: TEdit;
    edRetiradaExternalID: TEdit;
    edRetiradaMediatorFee: TEdit;
    edRetiradaPSPId: TEdit;
    edRetiradaTaxID: TEdit;
    edRetiradaTEDAccountDestination: TEdit;
    edRetiradaTEDBankDestination: TEdit;
    edRetiradaTEDBranchDestination: TEdit;
    edRetiradaTEDName: TEdit;
    edRetiradaTEDTaxID: TEdit;
    edRetiradaValor: TEdit;
    edTimeout: TSpinEdit;
    gbChavePIXConsultar: TGroupBox;
    gbChavePIXExcluir: TGroupBox;
    gbChavePIXIncluir: TGroupBox;
    gbCobranca: TGroupBox;
    gbConsultaAliasRetirada: TGroupBox;
    gbConsultaExtratoEC: TGroupBox;
    gbConsultaExtratoIntegrador: TGroupBox;
    gbconsultaSaldoEC: TGroupBox;
    gbconsultaSaldoIntegrador: TGroupBox;
    gbContaCriarEndereco: TGroupBox;
    gbDadosAdicionais: TGroupBox;
    gbFluxoCliente: TGroupBox;
    gbFluxoStatus: TGroupBox;
    gbFluxoTotal: TGroupBox;
    gbLog: TGroupBox;
    gbPIXRetirada: TGroupBox;
    gbProxy: TGroupBox;
    gbPSP: TGroupBox;
    gbQRCodeDetalhesCobranca: TGroupBox;
    gbRetirada: TGroupBox;
    gbTEDRetirada: TGroupBox;
    gbDevolucao: TGroupBox;
    gbMediator: TGroupBox;
    gbAccount: TGroupBox;
    gbConsultaTxID: TGroupBox;
    gbQRCodeValuesInterests: TGroupBox;
    gbQRCodeValues: TGroupBox;
    gbQRCodeValuesReduction: TGroupBox;
    gbGerarQRCodeInfoAdicional: TGroupBox;
    gbQRCodeDiscount: TGroupBox;
    Imagem: TImage;
    ImageList1: TImageList;
    imGerarQRCodeImg: TImage;
    imErroCertificado: TImage;
    imErroChavePrivada: TImage;
    imFluxoQRCode: TImage;
    lbAccavaliable: TLabel;
    lbAvisoAberturaConta: TLabel;
    lbconsultaEnding: TLabel;
    lbconsultaEnding1: TLabel;
    lbConsultarCobAccountID2: TLabel;
    lbConsultarCobTransactionID: TLabel;
    lbConsultarCobTransactionID3: TLabel;
    lbConsultarEXIntegradorAccountID: TLabel;
    lbConsultarSaldoIntegradorAccountID: TLabel;
    lbConsultaSaldoECAvaliable: TLabel;
    lbConsultaStart: TLabel;
    lbConsultaStart1: TLabel;
    lbContaCriarBairro: TLabel;
    lbContaCriarCEP: TLabel;
    lbContaCriarCidade: TLabel;
    lbContaCriarComplemento: TLabel;
    lbContaCriarLogradouro: TLabel;
    lbContaCriarNumero: TLabel;
    lbContaCriarUF: TLabel;
    lbDisclaimerCallBack: TLabel;
    lbAmbiente: TLabel;
    lbArqCertificado: TLabel;
    lbArqChavePrivada: TLabel;
    lbChavePIXConsultar: TLabel;
    lbMediatoravaliable: TLabel;
    lbMediatoravaliableStr: TLabel;
    lbQRCodeADname: TLabel;
    lbQRCodeADcontent: TLabel;
    lbQRCodeCallBack: TLabel;
    lbQRCoderecipientComment: TLabel;
    lbChavePIXExcluir: TLabel;
    lbChavePIXExcluirAccountId: TLabel;
    lbChavePIXIncluirExternalId: TLabel;
    lbChavePIXIncluirAccountId: TLabel;
    lbPSPClientID: TLabel;
    lbPSPClientSecret: TLabel;
    lbCNPJ: TLabel;
    lbCobCopiaECola: TLabel;
    lbDevolucaoCobTransactionID: TLabel;
    lbRetiradaAccountDestinationAccount: TLabel;
    lbRetiradaAccountdestinationBranch: TLabel;
    lbRetiradaAccountTypeDestination: TLabel;
    lbRetiradaAliasDestinatario: TLabel;
    lbRetiradaendToEndId: TLabel;
    lbRetiradaPersonType: TLabel;
    lbRetiradaPSPId: TLabel;
    lbRetiradaTaxID: TLabel;
    lbDevolucaoReasonCode: TLabel;
    lbContaConsultarAccountId: TLabel;
    lbAccountId: TLabel;
    lbChavePIX: TLabel;
    lbContaCriar: TLabel;
    lbContaCriarCelular: TLabel;
    lbQRCodePayercpfcnpj: TLabel;
    lbContaCriarExternalID: TLabel;
    lbContaCriarFundacao: TLabel;
    lbQRCodedueDate: TLabel;
    lbQRCodediscountsdate: TLabel;
    lbQRCodediscountsAviso: TLabel;
    lbContaCriarNascimento: TLabel;
    lbContaCriarNomeCliente: TLabel;
    lbQRCodePayerName: TLabel;
    lbContaCriarNomeEmpresa: TLabel;
    lbContaCriarRepresentante: TLabel;
    lbContaCriarRepresentanteBairro: TLabel;
    lbContaCriarRepresentanteCelular: TLabel;
    lbContaCriarRepresentanteCEP: TLabel;
    lbQRCodepayerCEP: TLabel;
    lbContaCriarRepresentanteCidade: TLabel;
    lbQRCodepayercity: TLabel;
    lbContaCriarRepresentanteCPF: TLabel;
    lbContaCriarRepresentanteEmail: TLabel;
    lbContaCriarRepresentanteFoto: TLabel;
    lbContaCriarRepresentanteLogradouro: TLabel;
    lbQRCodepayerstreet: TLabel;
    lbContaCriarRepresentanteMae: TLabel;
    lbContaCriarRepresentanteNumero: TLabel;
    lbContaCriarRepresentanteRGFrente: TLabel;
    lbContaCriarRepresentanteRGVerso: TLabel;
    lbContaCriarRepresentanteUF: TLabel;
    lbQRCodepayeruf: TLabel;
    lbContaCriarTipoCliente: TLabel;
    lbContaCriarTipoConta: TLabel;
    lbContaInativarAccountId: TLabel;
    lbErroCertificado: TLabel;
    lbErroChavePrivada: TLabel;
    lbExpiracao: TLabel;
    lbFluxoClienteDoc: TLabel;
    lbFluxoClienteNome: TLabel;
    lbFluxoCopiaECola: TLabel;
    lbFluxoCopiaECola1: TLabel;
    lbRetiradaTEDAccountDestination: TLabel;
    lbRetiradaTEDBankDestination: TLabel;
    lbRetiradaTEDBranchDestination: TLabel;
    lbRetiradaTEDName: TLabel;
    lbRetiradaTEDTaxID: TLabel;
    lbTipoMediatorFeeDevolucao: TLabel;
    lbTipoMediatorFee: TLabel;
    lbTipoMediatorFeeEstorno: TLabel;
    lbMediatorFee: TLabel;
    lbMediatorFeeEstorno: TLabel;
    lbQRCodeMediatorFee: TLabel;
    lbQRCodeTipoMediatorFee: TLabel;
    lbMediatorFeeDevolucao: TLabel;
    lbPSPSecretKey: TLabel;
    lbSiteEfetuarPagto: TLabel;
    lbQRCodeValor: TLabel;
    lbQRCodeinterestsvaluePerc: TLabel;
    lbDevolucaoValor: TLabel;
    lbRetiradaValor: TLabel;
    lbRetiradaMediatorFee: TLabel;
    lbRetiradaTipoRetirada: TLabel;
    lbQRCodeinterestsmodality: TLabel;
    lbQRCodefinesvaluePerc: TLabel;
    lbQRCodefinesmodality: TLabel;
    lbQRCodereductionvaluePerc: TLabel;
    lbQRCodereductionmodality: TLabel;
    lbQRCodediscountsvaluePerc: TLabel;
    lbQRCodediscountsmodality: TLabel;
    lbQRCodeTipoCobranca: TLabel;
    lbLog: TLabel;
    lbLog1: TLabel;
    lbLogArquivo: TLabel;
    lbLogNivel: TLabel;
    lbProxyHost: TLabel;
    lbProxyPorta: TLabel;
    lbProxySenha: TLabel;
    lbProxyUsuario: TLabel;
    lbQRCodeExternalID: TLabel;
    lbDevolucaoExternalID: TLabel;
    lbRetiradaExternalID: TLabel;
    lbTimeout: TLabel;
    lbUrlPIX: TLabel;
    mmContaConsultarResposta: TMemo;
    mmLogGerencial: TMemo;
    mmLogOperacoes: TMemo;
    OpenDialog1: TOpenDialog;
    pnFluxoValor: TPanel;
    pnTEDRetirada: TPanel;
    pnPIXRetirada: TPanel;
    pnConsultaAliasRetirada: TPanel;
    pnContaCriarEndereco: TPanel;
    pnConsultaExtratoIntegrador: TPanel;
    pnConsultaSaldoIntegrador: TPanel;
    pnConsultaSaldoEC: TPanel;
    pnConsultaExtratoEC: TPanel;
    pnConsultaTxID: TPanel;
    pnAccExtrato: TPanel;
    pgConsultas: TPageControl;
    pnMediatorExtrato: TPanel;
    pnConsultasExtratoEC: TPanel;
    pnRodapeImagem: TPanel;
    pnImagem: TPanel;
    pnDevolucaoBotoes: TPanel;
    pnMotivosDevolucao: TPanel;
    pnFluxoCopiaECola: TPanel;
    pcContaCriarDadosAdicionais: TPageControl;
    pnContaCriar: TPanel;
    pnContaCriarCorporate: TPanel;
    pnContaCriarRodape: TPanel;
    pnFluxotransactionId: TPanel;
    pnlRetirada: TPanel;
    pnRetirada: TPanel;
    pnDevolucao: TPanel;
    pgConfig: TPageControl;
    pnConfig1: TPanel;
    pnQRCodeDetalhesCobrancaRodape: TPanel;
    pnQRCodeDiscount: TPanel;
    pnGerarQRCodeImg: TPanel;
    pnConsultas: TPanel;
    pnSiteEfetuarPagto: TPanel;
    pnQRCodeDetalhesCobranca: TPanel;
    pnQRCodeValues: TPanel;
    pnQRCodeValuesInterests: TPanel;
    pnQRCodeValuesFines: TPanel;
    pnQRCodeValuesReduction: TPanel;
    pnQRCodeInfoAdicionais: TPanel;
    pnConfig2: TPanel;
    pnQRCodeResult: TPanel;
    pnGerarQRCodeInfo: TPanel;
    pBotoesConfiguracao: TPanel;
    pConfPIX: TPanel;
    pcTestes: TPageControl;
    pgContasEChaves: TPageControl;
    pgPrincipal: TPageControl;
    pnChavePIX: TPanel;
    pnChavePIXExcluir: TPanel;
    pnChavePIXIncluir: TPanel;
    pnChavesPIXConsultar: TPanel;
    pnCobranca: TPanel;
    pnContaConsultar: TPanel;
    pnContaInativar: TPanel;
    pnFluxoBackground: TPanel;
    pnFluxoBotoes: TPanel;
    pnFluxoBotoesErroConsultar: TPanel;
    pnFluxoBotoesPrincipais: TPanel;
    pnFluxoBotoesRight: TPanel;
    pnFluxoBotoesRight1: TPanel;
    pnFluxoCliente: TPanel;
    pnFluxoDiv2: TPanel;
    pnFluxoDiv3: TPanel;
    pnFluxoDiv4: TPanel;
    pnFluxoDiv7: TPanel;
    pnFluxoPagto: TPanel;
    pnFluxoQRCode: TPanel;
    pnFluxoRodape: TPanel;
    pnFluxoStatus: TPanel;
    pnFluxoTotal: TPanel;
    pnLog: TPanel;
    pnLogs: TPanel;
    pnLogs1: TPanel;
    pnLogsRodape: TPanel;
    pnLogsRodape1: TPanel;
    pnProxy: TPanel;
    pnPSP: TPanel;
    pnPSPMatera: TPanel;
    pnRetiradaBotoes: TPanel;
    seCobrancaExpiracao: TSpinEdit;
    sgMediatorExtrato: TStringGrid;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    sgAccExtrato: TStringGrid;
    tsConsultaTxID: TTabSheet;
    tsConsultaSaldoEC: TTabSheet;
    tsConsultaMediator: TTabSheet;
    tmConsultarPagto: TTimer;
    tmConsultarEstorno: TTimer;
    tsContaCriarCorporate: TTabSheet;
    tsRetirada: TTabSheet;
    tsDevolucoes: TTabSheet;
    tsChavePIX: TTabSheet;
    tsConfig: TTabSheet;
    tsConsultas: TTabSheet;
    tsContaConsultar: TTabSheet;
    tsContaCriar: TTabSheet;
    tsContaEChaves: TTabSheet;
    tsContaInativar: TTabSheet;
    tsFluxoPagto: TTabSheet;
    tsGerarQRCodes: TTabSheet;
    tsMatera: TTabSheet;
    tsPIX: TTabSheet;
    tsTestes: TTabSheet;
    procedure btAcharArqCertificadoClick(Sender: TObject);
    procedure btAcharChavePrivadaClick(Sender: TObject);
    procedure btChavePIXConsultarClick(Sender: TObject);
    procedure btChavePIXCriarExternalIdClick(Sender: TObject);
    procedure btChavePIXExcluirClick(Sender: TObject);
    procedure btChavePIXIncluirClick(Sender: TObject);
    procedure btCobCopiaEColaClick(Sender: TObject);
    procedure btConsultarAliasRetiradaClick(Sender: TObject);
    procedure btConsultarExtratoECClick(Sender: TObject);
    procedure btConsultarExtratoMediatorClick(Sender: TObject);
    procedure btConsultarSaldoECClick(Sender: TObject);
    procedure btConsultarSaldoMediatorClick(Sender: TObject);
    procedure btConsultarCobClick(Sender: TObject);
    procedure btConsultarMotivosDevolucoesClick(Sender: TObject);
    procedure btContaConsultarClick(Sender: TObject);
    procedure btContaCriarClick(Sender: TObject);
    procedure btContaCriarLimparDadosClick(Sender: TObject);
    procedure btContaCriarPreencherDadosClick(Sender: TObject);
    procedure btContaCriarRepresentanteFotoClick(Sender: TObject);
    procedure btContaCriarRepresentanteMostrarRGFotoFrente1Click(Sender: TObject);
    procedure btContaCriarRepresentanteMostrarRGFotoFrenteClick(Sender: TObject);
    procedure btContaCriarRepresentanteMostrarRGFotoVersoClick(Sender: TObject);
    procedure btContaCriarRepresentanteRGFotoFrenteClick(Sender: TObject);
    procedure btContaCriarRepresentanteRGFotoVersoClick(Sender: TObject);
    procedure btContaInativarClick(Sender: TObject);
    procedure btCriarCobrancaClick(Sender: TObject);
    procedure btDevolucaoClick(Sender: TObject);
    procedure btDevolucaoLimparDadosClick(Sender: TObject);
    procedure btDevolucaoPreencherDadosClick(Sender: TObject);
    procedure btFluxoCancelarVendaClick(Sender: TObject);
    procedure btFluxoCopiaECola1Click(Sender: TObject);
    procedure btFluxoCopiaEColaClick(Sender: TObject);
    procedure btFluxoEstornarPagtoClick(Sender: TObject);
    procedure btFluxoNovaVendaClick(Sender: TObject);
    procedure btFluxoPagarClick(Sender: TObject);
    procedure btFluxotransactionIDClick(Sender: TObject);
    procedure btLerParametrosClick(Sender: TObject);
    procedure btLogGerencialLimparClick(Sender: TObject);
    procedure btLogOperacoesLimparClick(Sender: TObject);
    procedure btProxyVerSenhaClick(Sender: TObject);
    procedure btQRCodeCriarLimparDadosClick(Sender: TObject);
    procedure btQRCodeCriarPreencherDadosClick(Sender: TObject);
    procedure btQRCodeGerarExternalID1Click(Sender: TObject);
    procedure btQRCodeGerarExternalIDClick(Sender: TObject);
    procedure btRetiradaClick(Sender: TObject);
    procedure btretiradaConsultaLimparDadosClick(Sender: TObject);
    procedure btRetiradaConsultaPreencherDadosClick(Sender: TObject);
    procedure btRetiradaGerarExternalIDClick(Sender: TObject);
    procedure btretiradaLimparDadosClick(Sender: TObject);
    procedure btRetiradaPreencherDadosClick(Sender: TObject);
    procedure btSalvarParametrosClick(Sender: TObject);
    procedure btVoltarClick(Sender: TObject);
    procedure cbAccountIdSelect(Sender: TObject);
    procedure cbAmbienteChange(Sender: TObject);
    procedure cbContaCriarTipoClienteChange(Sender: TObject);
    procedure cbDevolucaoReasonCodeDropDown(Sender: TObject);
    procedure cbTipoMediatorFeeDevolucaoChange(Sender: TObject);
    procedure cbTipoMediatorFeeEstornoChange(Sender: TObject);
    procedure cbTipoMediatorFeeChange(Sender: TObject);
    procedure cbQRCodeTipoCobrancaChange(Sender: TObject);
    procedure cbQRCodeTipoMediatorFeeChange(Sender: TObject);
    procedure cbRetiradaTipoRetiradaChange(Sender: TObject);
    procedure edArqCertificadoExit(Sender: TObject);
    procedure edArqChavePrivadaExit(Sender: TObject);
    procedure edContaCriarCEPChange(Sender: TObject);
    procedure lbSiteEfetuarPagtoClick(Sender: TObject);
    procedure lbSiteEfetuarPagtoMouseEnter(Sender: TObject);
    procedure lbSiteEfetuarPagtoMouseLeave(Sender: TObject);
    procedure lbUrlPIXClick(Sender: TObject);
    procedure edOnlyNumbersKeyPress(Sender: TObject; var aKey: Char);
    procedure FormCreate(Sender: TObject);
    procedure btContaCriarExternalIDClick(Sender: TObject);
    procedure pgPrincipalChange(Sender: TObject);
    procedure tmConsultarEstornoTimer(Sender: TObject);
    procedure tmConsultarPagtoTimer(Sender: TObject);
    procedure tsDevolucoesShow(Sender: TObject);
    procedure tsGerarQRCodesShow(Sender: TObject);
  private
    fFluxoDados: TFluxoPagtoDados;

    function CalculaMediatorFee(aValor: Double; aMediatorFee: string; aItemIndex: integer): Double;
    function GetNomeArquivoConfig: String;
    function MediatorCaption(aItemIndex: integer): String;
    function SelecionarFoto(aFoto: string): string;
    function TratarGUID(aStr: String): String;
    function FormatarJson(aJson: String): String;
    function RemoverPathAplicacao(const AFileName: String): String;
    function AdicionarPathAplicacao(const AFileName: String): String;

    procedure LimparInterfaceFluxo;
    procedure AtualizarStatus(aStatusCobranca: TMateraTransactionStatus = mtsNone; aStatusDevolucao: TMateraTransactionStatus = mtsNone);
    procedure LerMediatorFee(wIni: TIniFile; aAccountID: String);
    procedure MostraImagem(aArquivo: String);
    procedure PreencheGradeExtrato(aGrade: TStringGrid);
    
    procedure InicializarAba;
    procedure LerConfiguracao;
    procedure GravarConfiguracao;
    procedure AplicarConfiguracao;
    procedure ValidarConfiguracaoCNPJ;
    procedure ValidarConfiguracaoConta;
    
    procedure PopularConfigContas;
    procedure PopularConfigChavesPIX(aAccountId: String);

    procedure RemoverConta(aAccountId: String);
    procedure SalvarNovaConta(aAccountId: String);
    procedure SalvarMediatorFee(aAccountID: String);
    procedure CarregarMediatorFee;
    function ExisteConta(aAccountId: String): Boolean;

    procedure SalvarChavesPIX(aAccountId: String);
    procedure RemoverChavePIX(aAccountId, aChavePIX: String);

    procedure AdicionarLinhaLog(aMsg: String);
    procedure HabilitarInterfaceFluxo(Habilitar: Boolean = True);
    procedure TratarException(Sender: TObject; E: Exception);
    procedure AvaliarInterfaceFluxo;

    procedure InicializarComponentesDefault;
    procedure LigarAlertasdeErrosDeConfiguracao;

    procedure ReiniciarFluxo;

    procedure ValidarChavePrivada;
    procedure ValidarCertificado;
  public
    property NomeArquivoConfig: String read GetNomeArquivoConfig;
  end;

var
  frPixCDMatera: TfrPixCDMatera;

implementation

uses 
  {$IfDef FPC}
   fpjson, jsonparser, jsonscanner, Jsons,
  {$EndIf}
  synacode, synautil, TypInfo, pcnConversao, ACBrPIXUtil, ACBrValidador,
  ACBrUtil.Compatibilidade,
  ACBrUtil.Strings,
  ACBrUtil.FilesIO,
  ACBrUtil.Base;

{$R *.lfm}

{ TfrPixCDMatera }

procedure TfrPixCDMatera.lbUrlPIXClick(Sender: TObject);
begin
  OpenURL(cURL_ACBR);
end;

procedure TfrPixCDMatera.edOnlyNumbersKeyPress(Sender: TObject; var aKey: Char);
begin
  if (not CharInSet(aKey, [#8,#13,'0'..'9'])) then
    aKey := #0;
end;

function TfrPixCDMatera.GetNomeArquivoConfig: String;
begin
  Result := ChangeFileExt(Application.ExeName, '.ini');
end;

function TfrPixCDMatera.TratarGUID(aStr: String): String;
begin
  Result := LowerCase(aStr);

  if (Pos('-', aStr) <= 0) then
    Result :=
      Copy(aStr, 1, 8) + '-' +
      Copy(aStr, 9, 4) + '-' +
      Copy(aStr, 13, 4) + '-' +
      Copy(aStr, 17, 4) + '-' +
      Copy(aStr, 21, Length(aStr));
end;

function TfrPixCDMatera.FormatarJson(aJson: String): String;
{$IfDef FPC}
var
  jpar: TJSONParser;
  j: TJsonObject;
{$EndIf}
begin
  Result := AJSON;
  {$IfDef FPC}
  try
    j := TJSONObject.Create();
    try
      Result := j.Decode(Result);
    finally
      j.Free;
    end;
    jpar := TJSONParser.Create(Result, [joUTF8]);
    try
      Result := jpar.Parse.FormatJSON([], 2);
    finally
      jpar.Free;
    end;
  except
    Result := AJSON;
  end;
  {$EndIf}
end;

function TfrPixCDMatera.RemoverPathAplicacao(const AFileName: String): String;
var
  s: String;
begin
  s := Trim(AFileName);
  if (Pos(ApplicationPath, s) = 1) then
    Result := ExtractFileName(s)
  else
    Result := s;
end;

function TfrPixCDMatera.AdicionarPathAplicacao(const AFileName: String): String;
var
  s: String;
begin
  s := Trim(AFileName);
  if (s = '') then
    Result := s
  else if (ExtractFilePath(AFileName) <> '') then
    Result := s
  else
    Result := ApplicationPath + s;
end;

procedure TfrPixCDMatera.InicializarAba;
begin
  if EstaVazio(edCNPJ.Text) or EstaVazio(edPSPClientID.Text) then
  begin
    pgPrincipal.ActivePage := tsConfig;
    pgConfig.ActivePage := tsMatera;
  end
  else if EstaVazio(cbAccountId.Text) or EstaVazio(cbChavePIX.Text) then
  begin
    pgPrincipal.ActivePage := tsContaEChaves;
    pgContasEChaves.ActivePage := tsContaCriar;
  end
  else
    pgPrincipal.ActivePage := tsFluxoPagto;
end;

procedure TfrPixCDMatera.FormCreate(Sender: TObject);
begin
  Application.OnException := TratarException;
  pgPrincipal.ActivePageIndex := 0;
  pcTestes.ActivePageIndex := 0;
  pgContasEChaves.ActivePageIndex := 0;
  pgConfig.ActivePageIndex := 0;
  InicializarComponentesDefault;
  LerConfiguracao;
  InicializarAba;
end;

procedure TfrPixCDMatera.btContaCriarExternalIDClick(Sender: TObject);
begin
  edContaCriarExternalID.Text := CriarTxId;
end;

procedure TfrPixCDMatera.pgPrincipalChange(Sender: TObject);
begin
  pnLogs.Visible:=not (pgPrincipal.ActivePage = tsConfig);
  Splitter1.Visible:=pnLogs.Visible;
end;

procedure TfrPixCDMatera.tmConsultarEstornoTimer(Sender: TObject);
begin
  tmConsultarEstorno.Enabled := False;
  try
    if EstaVazio(fFluxoDados.transactionIDEstorno) then
    begin
      ShowMessage('Nenhuma cobrança a ser estornada');
      Exit;
    end;

    ACBrPSPMatera1.ConsultarTransacao(ACBrPSPMatera1.AccountId, fFluxoDados.transactionIDEstorno);
    mmLogOperacoes.Lines.Add(' Resposta: ' + sLineBreak + FormatarJson(ACBrPSPMatera1.TransacoesResposta.AsJSON));

    AtualizarStatus(mtsNone, ACBrPSPMatera1.TransacoesResposta.Items[0].transactionStatus);
    fFluxoDados.QtdConsultas := fFluxoDados.QtdConsultas + 1;
  finally
    if (fFluxoDados.StatusDevolucao = mtsCreated) and (not fFluxoDados.EmErro) and
       (fFluxoDados.QtdConsultas <= CMaxConsultas) then
      tmConsultarEstorno.Enabled := True;
  end;
end;

procedure TfrPixCDMatera.tmConsultarPagtoTimer(Sender: TObject);
begin
  tmConsultarPagto.Enabled := False;
  try
    if EstaVazio(fFluxoDados.transactionID) then
    begin
      ShowMessage('Nenhuma cobrança a ser consultada');
      Exit;
    end;

    ACBrPSPMatera1.ConsultarTransacao(ACBrPSPMatera1.AccountId, fFluxoDados.transactionID);
    AtualizarStatus(ACBrPSPMatera1.TransacoesResposta.Items[0].transactionStatus, mtsNone);
    fFluxoDados.QtdConsultas := fFluxoDados.QtdConsultas + 1;
  finally
    if (fFluxoDados.StatusCobranca = mtsCreated) and
       (not fFluxoDados.EmErro) and
       (fFluxoDados.QtdConsultas <= CMaxConsultas) then
      tmConsultarPagto.Enabled := True;
  end;
end;

procedure TfrPixCDMatera.tsDevolucoesShow(Sender: TObject);
begin
  if NaoEstaVazio(edMediatorFeeEstorno.Text) then
    edMediatorFeeDevolucao.Text := edMediatorFeeEstorno.Text;
end;

procedure TfrPixCDMatera.tsGerarQRCodesShow(Sender: TObject);
begin
  if NaoEstaVazio(edMediatorFee.Text) then
    edQRCodeMediatorFee.Text := edMediatorFee.Text;
end;

procedure TfrPixCDMatera.btSalvarParametrosClick(Sender: TObject);
begin
  GravarConfiguracao;
  AplicarConfiguracao;
end;

procedure TfrPixCDMatera.btVoltarClick(Sender: TObject);
begin
  pnImagem.Visible := False;
end;

procedure TfrPixCDMatera.cbAccountIdSelect(Sender: TObject);
begin
  PopularConfigChavesPIX(Trim(cbAccountId.Text));
end;

procedure TfrPixCDMatera.cbAmbienteChange(Sender: TObject);
begin
  btContaCriarPreencherDados.Enabled := (cbAmbiente.ItemIndex <> 1);
end;

procedure TfrPixCDMatera.cbContaCriarTipoClienteChange(Sender: TObject);
begin
  gbDadosAdicionais.Visible := (cbContaCriarTipoCliente.ItemIndex = 2);
  pnContaCriarCorporate.Visible := (cbContaCriarTipoCliente.ItemIndex = 2);
end;

procedure TfrPixCDMatera.cbDevolucaoReasonCodeDropDown(Sender: TObject);
begin
  if cbDevolucaoReasonCode.Items.Count = 0 then
    btConsultarMotivosDevolucoesClick(Self);
end;

procedure TfrPixCDMatera.cbTipoMediatorFeeDevolucaoChange(Sender: TObject);
begin
  lbMediatorFeeDevolucao.Caption := MediatorCaption(cbTipoMediatorFeeDevolucao.ItemIndex);
end;

procedure TfrPixCDMatera.cbTipoMediatorFeeEstornoChange(Sender: TObject);
begin
  lbMediatorFeeEstorno.Caption := MediatorCaption(cbTipoMediatorFeeEstorno.ItemIndex);
end;

procedure TfrPixCDMatera.cbTipoMediatorFeeChange(Sender: TObject);
begin
  lbMediatorFee.Caption := MediatorCaption(cbTipoMediatorFee.ItemIndex);
end;

procedure TfrPixCDMatera.cbQRCodeTipoCobrancaChange(Sender: TObject);
begin
  gbQRCodeDetalhesCobranca.Visible := (cbQRCodeTipoCobranca.ItemIndex = 1);
end;

function TfrPixCDMatera.MediatorCaption(aItemIndex: integer): String;
begin
  if aItemIndex = 0 then
    Result := 'Mediator Fee R$'
  else
    Result := 'Mediator Fee %';
end;

procedure TfrPixCDMatera.cbQRCodeTipoMediatorFeeChange(Sender: TObject);
begin
  lbQRCodeMediatorFee.Caption := MediatorCaption(cbQRCodeTipoMediatorFee.ItemIndex);
end;

procedure TfrPixCDMatera.cbRetiradaTipoRetiradaChange(Sender: TObject);
begin
  gbPIXRetirada.Visible := (cbRetiradaTipoRetirada.ItemIndex = 0);
  gbTEDRetirada.Visible := (cbRetiradaTipoRetirada.ItemIndex = 1);
end;

procedure TfrPixCDMatera.edArqCertificadoExit(Sender: TObject);
begin
  ValidarCertificado;
end;

procedure TfrPixCDMatera.edArqChavePrivadaExit(Sender: TObject);
begin
  ValidarChavePrivada;
end;

procedure TfrPixCDMatera.edContaCriarCEPChange(Sender: TObject);
begin
  if (Length(edContaCriarCEP.Text) > 5) then
  begin
    edContaCriarCEP.Text := FormatarMascaraDinamica(OnlyNumber(edContaCriarCEP.Text), '*****-***');
    edContaCriarCEP.SelStart := Length(edContaCriarCEP.Text);
  end;
end;

procedure TfrPixCDMatera.lbSiteEfetuarPagtoClick(Sender: TObject);
begin
  btFluxoCopiaECola.Click;
  OpenURL(CURL_MateraPagto);
end;

procedure TfrPixCDMatera.lbSiteEfetuarPagtoMouseEnter(Sender: TObject);
begin
  lbSiteEfetuarPagto.Font.Color := clBlue;
end;

procedure TfrPixCDMatera.lbSiteEfetuarPagtoMouseLeave(Sender: TObject);
begin
  lbSiteEfetuarPagto.Font.Color := clHighlight;
end;

procedure TfrPixCDMatera.btAcharArqCertificadoClick(Sender: TObject);
begin
  OpenDialog1.FileName := edArqCertificado.Text;
  if OpenDialog1.Execute then
    edArqCertificado.Text := RemoverPathAplicacao(OpenDialog1.FileName);
  ValidarCertificado;
end;

procedure TfrPixCDMatera.btAcharChavePrivadaClick(Sender: TObject);
begin
  OpenDialog1.FileName := edArqChavePrivada.Text;
  if OpenDialog1.Execute then
    edArqChavePrivada.Text := RemoverPathAplicacao(OpenDialog1.FileName);
  ValidarChavePrivada;
end;

procedure TfrPixCDMatera.btChavePIXConsultarClick(Sender: TObject);
var
  wAccountId: String;
begin
  wAccountId := Trim(edChavePIXConsultar.Text);
  if EstaVazio(wAccountId) then
  begin
    MessageDlg('Preencha o Account Id', mtError, [mbOK], 0);
    edChavePIXConsultar.SetFocus;
    Exit;
  end;

  try
    mmLogGerencial.Lines.Add(' Comando: ');
    mmLogGerencial.Lines.Add('  - ACBrPSPMatera1.ChavePIXConsultar(' + wAccountId + ')' + sLineBreak);

    ACBrPSPMatera1.ChavesPIXConsultar(wAccountId);
    SalvarChavesPIX(wAccountId);
    if (cbAccountId.Text = wAccountId) then
      PopularConfigChavesPIX(wAccountId);

    mmLogGerencial.Lines.Add(' Resposta: ' + sLineBreak + FormatarJson(ACBrPSPMatera1.ChavesPIXResposta.AsJSON));
  except
    On E: Exception do
      mmLogGerencial.Lines.Add(E.Message + sLineBreak + FormatarJson(ACBrPSPMatera1.ErroResposta.AsJSON));
  end;
end;

procedure TfrPixCDMatera.btChavePIXCriarExternalIdClick(Sender: TObject);
begin
  edChavePIXIncluirExternalId.Text := TratarGUID(CriarTxId);
end;

procedure TfrPixCDMatera.btChavePIXExcluirClick(Sender: TObject);
var
  wAccountId, wChavePIX: String;
begin
  wAccountId := Trim(edChavePIXExcluirAccountId.Text);
  if EstaVazio(wAccountId) then
  begin
    MessageDlg('Preencha o Account Id', mtError, [mbOK], 0);
    edChavePIXExcluirAccountId.SetFocus;
    Exit;
  end;

  wChavePIX := Trim(edChavePIXExcluir.Text);
  if EstaVazio(wChavePIX) then
  begin
    MessageDlg('Preencha a Chave PIX', mtError, [mbOK], 0);
    edChavePIXExcluir.SetFocus;
    Exit;
  end;

  try
    mmLogGerencial.Lines.Add(' Comando: ');
    mmLogGerencial.Lines.Add('  - ACBrPSPMatera1.ChavePIXExcluir(' +
      wAccountId + ', ' + wChavePIX + ')' + sLineBreak);

    if ACBrPSPMatera1.ChavePIXExcluir(wAccountId, wChavePIX) then
    begin
      mmLogGerencial.Lines.Add('  CHAVE PIX EXCLUÍDA COM SUCESSO!');
      RemoverChavePIX(wAccountId, wChavePIX);
      if (cbAccountId.Text = wAccountId) then
        PopularConfigChavesPIX(wAccountId);
    end;
  except
    On E: Exception do
    begin
      mmLogGerencial.Lines.Add('  ERRO AO EXCLUIR CHAVE PIX!' + sLineBreak + E.Message + sLineBreak +
        FormatarJson(ACBrPSPMatera1.ErroResposta.AsJSON));
      Abort;
    end;
  end;
end;

procedure TfrPixCDMatera.btChavePIXIncluirClick(Sender: TObject);
var
  wAccountId, wChavePIX: String;
begin
  wAccountId := Trim(edChavePIXIncluirAccountId.Text);
  if EstaVazio(wAccountId) then
  begin
    MessageDlg('Preencha o Account Id', mtError, [mbOK], 0);
    edChavePIXIncluirAccountId.SetFocus;
    Exit;
  end;

  wChavePIX := Trim(edChavePIXIncluirExternalId.Text);
  if EstaVazio(wChavePIX) then
  begin
    MessageDlg('Preencha a Chave PIX', mtError, [mbOK], 0);
    edChavePIXIncluirExternalId.SetFocus;
    Exit;
  end;

  ACBrPSPMatera1.Clear;

  // Preenchendo dados da Chave PIX
  ACBrPSPMatera1.ChavePIXSolicitacao.externalIdentifier := wChavePIX;
  ACBrPSPMatera1.ChavePIXSolicitacao.alias_.type_ := malEVP;

  try
    mmLogGerencial.Lines.Add(' Comando: ');
    mmLogGerencial.Lines.Add('  - ACBrPSPMatera1.ChavePIXIncluir(' + wAccountId + ')' + sLineBreak);

    if ACBrPSPMatera1.ChavePIXIncluir(wAccountId) then
      mmLogGerencial.Lines.Add('  CHAVE PIX CADASTRADA COM SUCESSO!' + sLineBreak);

    mmLogGerencial.Lines.Add(' Resposta:' + sLineBreak +
      FormatarJson(ACBrPSPMatera1.ChavePIXResposta.AsJSON));
  except
    On E: Exception do
    begin
      mmLogGerencial.Lines.Add(E.Message);
      mmLogGerencial.Lines.Add('  ERRO AO CADASTRAR CHAVE PIX!');
      mmLogGerencial.Lines.Add(FormatarJson(ACBrPSPMatera1.ErroResposta.AsJSON));
    end;
  end;
end;

procedure TfrPixCDMatera.btCobCopiaEColaClick(Sender: TObject);
begin
  Clipboard.AsText := Trim(edCobCopiaECola.Text);
end;

procedure TfrPixCDMatera.btConsultarAliasRetiradaClick(Sender: TObject);
var
  wAccountId, wAlias: String;
begin
  wAccountId := Trim(edRetiradaConsultaAliasAccountID.Text);
  if EstaVazio(wAccountId) then
  begin
    MessageDlg('Preencha o Account Id', mtError, [mbOK], 0);
    edRetiradaConsultaAliasAccountID.SetFocus;
    Exit;
  end;

  wAlias := Trim(edRetiradaConsultaaliasAliasDestinatario.Text);
  if EstaVazio(wAlias) then
  begin
    MessageDlg('Preencha o Alias Destinatário', mtError, [mbOK], 0);
    edRetiradaConsultaaliasAliasDestinatario.SetFocus;
    Exit;
  end;

  try
    mmLogOperacoes.Lines.Add(' Comando: ');
    mmLogOperacoes.Lines.Add('  - ACBrPSPMatera1.ConsultarAliasRetirada('+
    wAccountId + ', ' + wAlias + ')' + sLineBreak);

    ACBrPSPMatera1.ConsultarAliasRetirada(wAccountId, wAlias);

    mmLogOperacoes.Lines.Add(' Resposta: ' + sLineBreak + FormatarJson(ACBrPSPMatera1.AliasRetiradaResposta.AsJSON));
  except
    On E: Exception do
      mmLogOperacoes.Lines.Add(E.Message + sLineBreak + FormatarJson(ACBrPSPMatera1.ErroResposta.AsJSON));
  end;
end;

procedure TfrPixCDMatera.PreencheGradeExtrato(aGrade: TStringGrid);
Var
  i: integer;
begin
  if ACBrPSPMatera1.ExtratoECResposta.statement.Count > 0 then
  begin
    aGrade.Clear;
    for i := 0 to ACBrPSPMatera1.ExtratoECResposta.statement.Count-1 do
    begin
      aGrade.RowCount := aGrade.RowCount + 1;
      aGrade.Cells[0,i] := FormatFloatBr(ACBrPSPMatera1.ExtratoECResposta.statement[i].amount);
      aGrade.Cells[1,i] := ACBrPSPMatera1.ExtratoECResposta.statement[i].description;
      aGrade.Cells[2,i] := FloatToStr(ACBrPSPMatera1.ExtratoECResposta.statement[i].historyCode);
      aGrade.Cells[3,i] := ACBrPSPMatera1.ExtratoECResposta.statement[i].transactionId;
      aGrade.Cells[4,i] := ACBrPSPMatera1.ExtratoECResposta.statement[i].transactionType;
      aGrade.Cells[5,i] := MaterastatementEntryTypeToString(ACBrPSPMatera1.ExtratoECResposta.statement[i].type_);
    end;
  end;
end;

procedure TfrPixCDMatera.btConsultarExtratoECClick(Sender: TObject);
begin
  ValidarConfiguracaoConta;

  try
    mmLogOperacoes.Lines.Add(' Comando: ');
    mmLogOperacoes.Lines.Add('  - ACBrPSPMatera1.ConsultarExtratoEC(' +
      ACBrPSPMatera1.AccountId + ')' + sLineBreak);

    ACBrPSPMatera1.ConsultarExtratoEC(ACBrPSPMatera1.AccountId);

    mmLogOperacoes.Lines.Add(' Resposta: ' + sLineBreak + FormatarJson(ACBrPSPMatera1.ExtratoECResposta.AsJSON));

    PreencheGradeExtrato(sgAccExtrato);

  except
    On E: Exception do
      mmLogOperacoes.Lines.Add(E.Message + sLineBreak + FormatarJson(ACBrPSPMatera1.ErroResposta.AsJSON));
  end;
end;

procedure TfrPixCDMatera.btConsultarExtratoMediatorClick(Sender: TObject);
var
  wMediatorAccountId: String;
begin
  wMediatorAccountId := Trim(edConsultarEXIntegradorAccountID.Text);
  if EstaVazio(wMediatorAccountId) then
  begin
    MessageDlg('Preencha o Mediator Account Id', mtError, [mbOK], 0);
    edConsultarEXIntegradorAccountID.SetFocus;
    Exit;
  end;

  try
    mmLogOperacoes.Lines.Add(' Comando: ');
    mmLogOperacoes.Lines.Add('  - ACBrPSPMatera1.ConsultarExtratoEC(' +
      wMediatorAccountId + ')' + sLineBreak);

    ACBrPSPMatera1.ConsultarExtratoEC(wMediatorAccountId, edConsultaStart.DateTime, edconsultaEnding.DateTime);

    mmLogOperacoes.Lines.Add(' Resposta: ' + sLineBreak + FormatarJson(ACBrPSPMatera1.ExtratoECResposta.AsJSON));

    PreencheGradeExtrato(sgMediatorExtrato);
  except
    On E: Exception do
      mmLogOperacoes.Lines.Add(E.Message + sLineBreak + FormatarJson(ACBrPSPMatera1.ErroResposta.AsJSON));
  end;
end;

procedure TfrPixCDMatera.btConsultarSaldoECClick(Sender: TObject);
var
  wSaldo: Currency;
begin
  ValidarConfiguracaoConta;

  try
    mmLogOperacoes.Lines.Add(' Comando: ');
    mmLogOperacoes.Lines.Add('  - ACBrPSPMatera1.ConsultarSaldoEC(' +
      ACBrPSPMatera1.AccountId + ')' + sLineBreak);

    ACBrPSPMatera1.ConsultarSaldoEC(ACBrPSPMatera1.AccountId);
    lbConsultaSaldoECAvaliable.Visible := True;
    lbAccavaliable.Visible := True;

    mmLogOperacoes.Lines.Add(' Resposta: ' + sLineBreak + FormatarJson(ACBrPSPMatera1.SaldoECResposta.AsJSON));
    wSaldo := ACBrPSPMatera1.SaldoECResposta.available;
    lbAccavaliable.Caption := 'R$ ' + FormatFloatBr(wSaldo);

    if (wSaldo > 0) then
      lbAccavaliable.Font.Color := clBlue
    else if (wSaldo < 0) then
      lbAccavaliable.Font.Color := clRed
    else
      lbAccavaliable.Font.Color := clBlack;
  except
    On E: Exception do
      mmLogOperacoes.Lines.Add(E.Message + sLineBreak + FormatarJson(ACBrPSPMatera1.ErroResposta.AsJSON));
  end;
end;

procedure TfrPixCDMatera.btConsultarSaldoMediatorClick(Sender: TObject);
var
  wMediatorAccountId: String;
  wSaldo: Currency;
begin
  wMediatorAccountId := Trim(edConsultarSaldoIntegradorAccountID.Text);
  if EstaVazio(wMediatorAccountId) then
  begin
    MessageDlg('Preencha o Account Id', mtError, [mbOK], 0);
    edConsultarSaldoIntegradorAccountID.SetFocus;
    Exit;
  end;

  try
    mmLogOperacoes.Lines.Add(' Comando: ');
    mmLogOperacoes.Lines.Add('  - ACBrPSPMatera1.ConsultarSaldoEC(' + wMediatorAccountId + ')' + sLineBreak);

    ACBrPSPMatera1.ConsultarSaldoEC(wMediatorAccountId);

    mmLogOperacoes.Lines.Add(' Resposta: ' + sLineBreak + FormatarJson(ACBrPSPMatera1.SaldoECResposta.AsJSON));
    wSaldo := ACBrPSPMatera1.SaldoECResposta.available;
    lbMediatoravaliable.Caption := 'R$ ' + FormatFloatBr(wSaldo);

    if (wSaldo > 0) then
      lbMediatoravaliable.Font.Color := clBlue
    else if (wSaldo < 0) then
      lbMediatoravaliable.Font.Color := clRed
    else
      lbMediatoravaliable.Font.Color := clBlack;
  except
    On E: Exception do
      mmLogOperacoes.Lines.Add(E.Message + sLineBreak + FormatarJson(ACBrPSPMatera1.ErroResposta.AsJSON));
  end;
end;

procedure TfrPixCDMatera.btConsultarCobClick(Sender: TObject);
var
  wTransactionID: String;
begin
  ValidarConfiguracaoConta;

  wTransactionID := Trim(edConsultarCobTransactionID.Text);
  if EstaVazio(wTransactionID) then
  begin
    MessageDlg('Preencha o Transaction Id', mtError, [mbOK], 0);
    edConsultarCobTransactionID.SetFocus;
    Exit;
  end;

  try
    mmLogOperacoes.Lines.Add(' Comando: ');
    mmLogOperacoes.Lines.Add('  - ACBrPSPMatera1.ConsultarTransacao(' +
      ACBrPSPMatera1.AccountId + ', ' + wTransactionID + ')' + sLineBreak);

    ACBrPSPMatera1.ConsultarTransacao(ACBrPSPMatera1.AccountId, wTransactionID);

    mmLogOperacoes.Lines.Add(' Resposta: ' + sLineBreak + FormatarJson(ACBrPSPMatera1.TransacoesResposta.AsJSON));
  except
    On E: Exception do
      mmLogOperacoes.Lines.Add(E.Message + sLineBreak + FormatarJson(ACBrPSPMatera1.ErroResposta.AsJSON));
  end;
end;

procedure TfrPixCDMatera.btConsultarMotivosDevolucoesClick(Sender: TObject);
var
  i: integer;
begin
  try
    mmLogOperacoes.Lines.Add(' Comando: ');
    mmLogOperacoes.Lines.Add('  - ACBrPSPMatera1.ConsultarMotivosDevolucoes()' + sLineBreak);

    ACBrPSPMatera1.DevolucaoConsultarMotivos;

    mmLogOperacoes.Lines.Add(' Resposta: ' + sLineBreak + FormatarJson(ACBrPSPMatera1.DevolucaoMotivos.AsJSON));

    cbDevolucaoReasonCode.Clear;
    for i := 0 to ACBrPSPMatera1.DevolucaoMotivos.returnCodes.Count-1 do
    begin
      cbDevolucaoReasonCode.Items.Add(ACBrPSPMatera1.DevolucaoMotivos.returnCodes.Items[i].code);
    end;
  except
    On E: Exception do
      mmLogOperacoes.Lines.Add(E.Message + sLineBreak + FormatarJson(ACBrPSPMatera1.ErroResposta.AsJSON));
  end;
end;

procedure TfrPixCDMatera.btContaConsultarClick(Sender: TObject);
var
  wAccountId: String;
begin
  wAccountId := Trim(edContaConsultarAccountId.Text);
  mmContaConsultarResposta.Lines.Clear;
  if EstaVazio(wAccountId) then
  begin
    MessageDlg('Preencha o Account Id', mtError, [mbOK], 0);
    edContaConsultarAccountId.SetFocus;
    Exit;
  end;

  try
    mmLogGerencial.Lines.Add(' Comando: ');
    mmLogGerencial.Lines.Add('  - ACBrPSPMatera1.ContaConsultar(' + wAccountId + ')' + sLineBreak);

    ACBrPSPMatera1.ContaConsultar(wAccountId);
  except
    On E: Exception do
    begin
      mmLogGerencial.Lines.Add(E.Message + sLineBreak +
        FormatarJson(ACBrPSPMatera1.ErroResposta.AsJSON));
      Abort;
    end;
  end;

  mmContaConsultarResposta.Lines.Add(' Status da conta: ' +
    MateraAccountStatusToString(ACBrPSPMatera1.ContaResposta.accountStatus) + sLineBreak);
  mmContaConsultarResposta.Lines.Add(' Resposta: ' + sLineBreak + sLineBreak +
    FormatarJson(ACBrPSPMatera1.ContaResposta.AsJSON));
  mmContaConsultarResposta.SelStart := 0;
end;

procedure TfrPixCDMatera.btContaCriarClick(Sender: TObject);
var
  wBase64: String;
  wFs: TFileStream;
  wFile: AnsiString;
  LContaSolicitacao : TMateraCreateAccountTransactionRequest;
  LBasicClient : TMateraBasicClient;
  LBillingAddress, LMailAddress : TMateraEndereco;
  LAdditionalDetailsCorporate : TMateraAdditionalDetailsCorporate;
  LClientRepresentative : TMateraClientRepresentative;
  LDocument : TMateraDocument;
begin
  ACBrPSPMatera1.Clear;
  ValidarConfiguracaoCNPJ;

  // Preenchendo dados da Conta
  LContaSolicitacao := ACBrPSPMatera1.ContaSolicitacao;

  LContaSolicitacao.externalIdentifier := edContaCriarExternalID.Text;
  LContaSolicitacao.clientType         := TMateraClientType(cbContaCriarTipoCliente.ItemIndex);
  LContaSolicitacao.accountType        := TMateraAccountType(cbCriarContaTipoConta.ItemIndex);

  // Preenchendo dados do cliente da conta
  LBasicClient := LContaSolicitacao.client;

  LBasicClient.name                    := edContaCriarNomeCliente.Text;
  LBasicClient.email                   := edContaCriarEmail.Text;
  LBasicClient.taxIdentifier.taxId     := edCNPJ.Text;
  LBasicClient.taxIdentifier.country   := 'BRA';
  LBasicClient.mobilePhone.country     := 'BRA';
  LBasicClient.mobilePhone.phoneNumber := edContaCriarCelular.Text;

  // Preenchendo dados do endereço da conta
  LBillingAddress := LContaSolicitacao.billingAddress;

  LBillingAddress.logradouro  := edContaCriarLogradouro.Text;
  LBillingAddress.numero      := edContaCriarNumero.Text;
  LBillingAddress.complemento := edContaCriarComplemento.Text;
  LBillingAddress.bairro      := edContaCriarBairro.Text;
  LBillingAddress.cidade      := edContaCriarCidade.Text;
  LBillingAddress.estado      := edContaCriarUF.Text;
  LBillingAddress.cep         := edContaCriarCEP.Text;
  LBillingAddress.pais        := 'BRA';


  // Conta corporativa? ...Preenche dados da empresa
  if (LContaSolicitacao.clientType = mctCorporate) then
  begin
    LAdditionalDetailsCorporate := LContaSolicitacao.additionalDetailsCorporate;

    LAdditionalDetailsCorporate.establishmentDate := edContaCriarFundacao.Date;
    LAdditionalDetailsCorporate.companyName       := edContaCriarNomeEmpresa.Text;
    LAdditionalDetailsCorporate.businessLine      := 47;
    LAdditionalDetailsCorporate.establishmentForm := '1';
    LAdditionalDetailsCorporate.financialStatistic := 0;

    // Preenchendo o representando da empresa
    LClientRepresentative := LAdditionalDetailsCorporate.representatives.New;
    LClientRepresentative.name                    := edContaCriarRepresentanteNome.Text;
    LClientRepresentative.mother                  := edContaCriarRepresentanteMae.Text;
    LClientRepresentative.birthDate               := edContaCriarNascimento.Date;
    LClientRepresentative.email                   := edContaCriarRepresentanteEmail.Text;
    LClientRepresentative.taxIdentifier.taxId     := edContaCriarRepresentanteCPF.Text;
    LClientRepresentative.taxIdentifier.country   := 'BRA';
    LClientRepresentative.mobilePhone.country     := 'BRA';
    LClientRepresentative.mobilePhone.phoneNumber := edContaCriarRepresentanteCelular.Text;

    LMailAddress := LClientRepresentative.mailAddress;
    LMailAddress.cep        := edContaCriarRepresentanteCEP.Text;
    LMailAddress.logradouro := edContaCriarRepresentanteLogradouro.Text;
    LMailAddress.numero     := edContaCriarRepresentanteNumero.Text;
    LMailAddress.bairro     := edContaCriarRepresentanteBairro.Text;
    LMailAddress.cidade     := edContaCriarRepresentanteCidade.Text;
    LMailAddress.estado     := edContaCriarRepresentanteUF.Text;
    LMailAddress.pais       := 'BRA';

    // Preenchendo a foto do representante
    if NaoEstaVazio(edContaCriarRepresentanteFoto.Text) then
    begin
      LDocument := LClientRepresentative.documents.New;
      // Lê conteúdo do arquivo e converte para Base64
      wFs := TFileStream.Create(edContaCriarRepresentanteFoto.Text, fmOpenRead or fmShareDenyWrite);
      try
        wFs.Position := 0;
        wFile := ReadStrFromStream(wFs, wFs.Size);
        wBase64 := EncodeBase64(wFile);
      finally
        wFs.Free;
      end;

      LDocument.type_ := mdtPicture;
      LDocument.content := wBase64;
    end;

    // Preenchendo foto do RG(Frente) do representante
    if NaoEstaVazio(edContaCriarRepresentanteRGFotoFrente.Text) then
    begin
      LDocument := LClientRepresentative.documents.New;
      // Lê conteúdo do arquivo e converte para Base64
      wFs := TFileStream.Create(edContaCriarRepresentanteRGFotoFrente.Text, fmOpenRead or fmShareDenyWrite);
      try
        wFs.Position := 0;
        wFile := ReadStrFromStream(wFs, wFs.Size);
        wBase64 := EncodeBase64(wFile);
      finally
        wFs.Free;
      end;

      LDocument.type_ := mdtIdentityFront;
      LDocument.content := wBase64;
    end;

    // Preenchendo os documentos do representante
    if NaoEstaVazio(edContaCriarRepresentanteRGFotoVerso.Text) then
    begin
      LDocument := LClientRepresentative.documents.New;
      // Lê conteúdo do arquivo e converte para Base64
      wFs := TFileStream.Create(edContaCriarRepresentanteRGFotoVerso.Text, fmOpenRead or fmShareDenyWrite);
      try
        wFs.Position := 0;
        wFile := ReadStrFromStream(wFs, wFs.Size);
        wBase64 := EncodeBase64(wFile);
      finally
        wFs.Free;
      end;

      LDocument.type_ := mdtIdentityBack;
      LDocument.content := wBase64;
    end;

  end;

  try
    mmLogGerencial.Lines.Add(' Comando: ');
    mmLogGerencial.Lines.Add('  - ACBrPSPMatera1.ContaIncluir' + sLineBreak);

    if ACBrPSPMatera1.ContaIncluir then
    begin
      mmLogGerencial.Lines.Add('  CONTA CRIADA COM SUCESSO!' + sLineBreak);
      SalvarNovaConta(ACBrPSPMatera1.ContaResposta.account.accountID);
      PopularConfigContas;
    end;
  except
    On E: Exception do
    begin
      mmLogGerencial.Lines.Add(E.Message);
      mmLogGerencial.Lines.Add(' ERRO:');
      mmLogGerencial.Lines.Add(FormatarJson(ACBrPSPMatera1.ErroResposta.AsJSON));
      Abort;
    end;
  end;

  mmLogGerencial.Lines.Add(' Resposta:' + sLineBreak +
    FormatarJson(ACBrPSPMatera1.ContaResposta.AsJSON));
end;

procedure TfrPixCDMatera.btContaCriarLimparDadosClick(Sender: TObject);
begin
  edContaCriarExternalID.Text := EmptyStr;
  cbCriarContaTipoConta.ItemIndex := 3;
  cbContaCriarTipoCliente.ItemIndex := 2;
  cbContaCriarTipoClienteChange(Nil);
  edContaCriarNomeCliente.Text := EmptyStr;
  edContaCriarCelular.Text := EmptyStr;
  edContaCriarEmail.Text := EmptyStr;
  edContaCriarCEP.Text := EmptyStr;
  edContaCriarLogradouro.Text := EmptyStr;
  edContaCriarNumero.Text := EmptyStr;
  edContaCriarComplemento.Text := EmptyStr;
  edContaCriarBairro.Text := EmptyStr;
  edContaCriarCidade.Text := EmptyStr;
  edContaCriarUF.Text := EmptyStr;

  edContaCriarFundacao.Date := 0;
  edContaCriarNomeEmpresa.Text := EmptyStr;
  edContaCriarNascimento.Date := 0;
  edContaCriarRepresentanteNome.Text := EmptyStr;
  edContaCriarRepresentanteMae.Text := EmptyStr;
  edContaCriarRepresentanteCPF.Text := EmptyStr;
  edContaCriarRepresentanteEmail.Text := EmptyStr;
  edContaCriarRepresentanteCelular.Text := EmptyStr;
  edContaCriarRepresentanteCEP.Text := EmptyStr;
  edContaCriarRepresentanteLogradouro.Text := EmptyStr;
  edContaCriarRepresentanteNumero.Text := EmptyStr;
  edContaCriarRepresentanteBairro.Text := EmptyStr;
  edContaCriarRepresentanteCidade.Text := EmptyStr;
  edContaCriarRepresentanteUF.Text := EmptyStr;
  edContaCriarRepresentanteFoto.Text := EmptyStr;
  edContaCriarRepresentanteRGFotoFrente.Text := EmptyStr;
  edContaCriarRepresentanteRGFotoVerso.Text := EmptyStr;
end;

procedure TfrPixCDMatera.btContaCriarPreencherDadosClick(Sender: TObject);
begin
  edContaCriarExternalID.Text := CriarTxId;
  cbContaCriarTipoCliente.ItemIndex := 2;
  cbCriarContaTipoConta.ItemIndex := 4;
  cbContaCriarTipoClienteChange(Nil);
  edContaCriarNomeCliente.Text := 'Pessoa Jurídica';
  edContaCriarCelular.Text := '12922223893';
  edContaCriarEmail.Text := 'pessoajuridica@mp.com.br';
  edContaCriarCEP.Text := '13720000';
  edContaCriarLogradouro.Text := 'Rua Sacramento';
  edContaCriarNumero.Text := '15';
  edContaCriarComplemento.Text := 'Casa';
  edContaCriarBairro.Text := 'Centro';
  edContaCriarCidade.Text := 'São Paulo';
  edContaCriarUF.Text := 'SP';
  edCNPJ.Text := EmptyStr;
  
  edContaCriarFundacao.Date := EncodeDate(1990, 5, 29);
  edContaCriarNomeEmpresa.Text := 'Nome da Empresa';
  edContaCriarNascimento.Date := EncodeDate(1990, 5, 28);
  edContaCriarRepresentanteNome.Text := 'Representante 1';
  edContaCriarRepresentanteMae.Text := 'Mãe do Representante';
  edContaCriarRepresentanteCPF.Text := '13585366864';
  edContaCriarRepresentanteEmail.Text := 'representante.pj@mp.com.br';
  edContaCriarRepresentanteCelular.Text := '12922223893';
  edContaCriarRepresentanteCEP.Text := '01309030';
  edContaCriarRepresentanteLogradouro.Text := 'Rua Fernando de Albuquerque';
  edContaCriarRepresentanteNumero.Text := '88';
  edContaCriarRepresentanteBairro.Text := 'Consolação';
  edContaCriarRepresentanteCidade.Text := 'São Paulo';
  edContaCriarRepresentanteUF.Text := 'SP';
  edContaCriarRepresentanteFoto.Text := 'foto.png';
  edContaCriarRepresentanteRGFotoFrente.Text := 'fotorgfrente.png';
  edContaCriarRepresentanteRGFotoVerso.Text := 'fotorgverso.png';
end;

function TfrPixCDMatera.SelecionarFoto(aFoto: string): string;
begin
  OpenDialog1.FileName := aFoto;
  if OpenDialog1.Execute then
    Result := RemoverPathAplicacao(OpenDialog1.FileName);
end;

procedure TfrPixCDMatera.btContaCriarRepresentanteFotoClick(Sender: TObject);
begin
  edContaCriarRepresentanteFoto.Text := SelecionarFoto(edContaCriarRepresentanteFoto.Text);
end;

procedure TfrPixCDMatera.btContaCriarRepresentanteMostrarRGFotoFrente1Click(
  Sender: TObject);
begin
  MostraImagem(edContaCriarRepresentanteFoto.Text);
end;

procedure TfrPixCDMatera.MostraImagem(aArquivo: String);
begin
  Imagem.Picture.LoadFromFile(aArquivo);
  pnImagem.BringToFront;
  pnImagem.Visible := True;
end;

procedure TfrPixCDMatera.btContaCriarRepresentanteMostrarRGFotoFrenteClick(
  Sender: TObject);
begin
  MostraImagem(edContaCriarRepresentanteRGFotoFrente.Text);
end;

procedure TfrPixCDMatera.btContaCriarRepresentanteMostrarRGFotoVersoClick(
  Sender: TObject);
begin
  MostraImagem(edContaCriarRepresentanteRGFotoVerso.Text);
end;

procedure TfrPixCDMatera.btContaCriarRepresentanteRGFotoFrenteClick(
  Sender: TObject);
begin
  edContaCriarRepresentanteRGFotoFrente.Text := SelecionarFoto(edContaCriarRepresentanteRGFotoFrente.Text);
end;

procedure TfrPixCDMatera.btContaCriarRepresentanteRGFotoVersoClick(
  Sender: TObject);
begin
  edContaCriarRepresentanteRGFotoVerso.Text := SelecionarFoto(edContaCriarRepresentanteRGFotoVerso.Text);
end;

procedure TfrPixCDMatera.btContaInativarClick(Sender: TObject);
var
  wAccountId: String;
begin
  wAccountId := Trim(edContaInativarAccountId.Text);

  if EstaVazio(wAccountId) then
  begin
    MessageDlg('Preencha o Account Id', mtError, [mbOK], 0);
    edContaInativarAccountId.SetFocus;
    Exit;
  end;

  try
    mmLogGerencial.Lines.Add(' Comando: ');
    mmLogGerencial.Lines.Add('  - ACBrPSPMatera1.ContaInativar(' + wAccountId + ')' + sLineBreak);

    if ACBrPSPMatera1.ContaInativar(wAccountId) then
    begin
      mmLogGerencial.Lines.Add('  CONTA INATIVADA COM SUCESSO!');
      RemoverConta(wAccountId);
      PopularConfigContas;
    end;
  except
    On E: Exception do
    begin
      mmLogGerencial.Lines.Add('  ERRO AO INATIVAR CONTA!' + sLineBreak + E.Message + sLineBreak +
        FormatarJson(ACBrPSPMatera1.ErroResposta.AsJSON));
      Abort;
    end;
  end;
end;

function TfrPixCDMatera.CalculaMediatorFee(aValor: Double; aMediatorFee: string; aItemIndex: integer): Double;
begin
  if aItemIndex = 0 then
    Result := StrToFloatDef(aMediatorFee, 0)
  else
    Result := aValor * (StrToFloatDef(aMediatorFee, 0) / 100);
end;

procedure TfrPixCDMatera.btCriarCobrancaClick(Sender: TObject);
var
  wOK: Boolean;
  wQRCode: String;
  wValor, wMediatorFee: Double;
  lQRCodeSolicitacao: TMateraQRCodeRequest;
  linstantPayment: TMateraInstantPayment;
  lrecipient: TMateraRecipient;
  lbillingDueDate: TMateraBillingDueDate;
  lfixedDateDiscounts: TMateraFixedDateDiscount;
  laddicionalInformation: TMateraAdditionalInformation;
begin
  ACBrPSPMatera1.Clear;
  ValidarConfiguracaoConta;

  wValor := StrToFloatDef(edQRCodeValor.Text, 1);
  wMediatorFee := CalculaMediatorFee(wValor, edQRCodeMediatorFee.Text, cbQRCodeTipoMediatorFee.ItemIndex);

  // Preenchendo dados do QRcode
  lQRCodeSolicitacao := ACBrPSPMatera1.QRCodeSolicitacao;

  lQRCodeSolicitacao.externalIdentifier := edQRCodeExternalID.Text;
  lQRCodeSolicitacao.totalAmount := RoundABNT(wValor, -2);
  lQRCodeSolicitacao.currency:= cMoeda;

  lQRCodeSolicitacao.paymentInfo.transactionType := 'InstantPayment';

  linstantPayment := lQRCodeSolicitacao.paymentInfo.instantPayment;
  linstantPayment.alias_ := cbChavePIX.Text;

  // Cobrança Normal
  if (cbQRCodeTipoCobranca.ItemIndex = 0) then
    linstantPayment.expiration := StrToInt(seCobrancaExpiracao.Text)
  else
  // Cobrança com Vencimento
  begin
    linstantPayment.expiration := 0;
    linstantPayment.dynamicQRCodeType := mqtBillingDueDate;

    lbillingDueDate := linstantPayment.billingDueDate;
    lbillingDueDate.dueDate := edQRCodedueDate.DateTime;

    lbillingDueDate.payerInformation.cpfCnpj := edQRCodePayercpfcnpj.Text;
    lbillingDueDate.payerInformation.name := edQRCodePayerName.Text;
    lbillingDueDate.payerInformation.addressing.street := edQRCodepayerstreet.Text;
    lbillingDueDate.payerInformation.addressing.city := edQRCodepayercity.Text;
    lbillingDueDate.payerInformation.addressing.uf := edQRCodepayeruf.Text;
    lbillingDueDate.payerInformation.addressing.cep := edQRCodepayerCEP.Text;


    lbillingDueDate.interests.valuePerc := StrToCurr(edQRCodeinterestsvaluePerc.Text);
    lbillingDueDate.interests.modality := StrToInt(edQRCodeinterestsmodality.Text);

    lbillingDueDate.fines.valuePerc := StrToCurr(edQRCodefinesvaluePerc.Text);
    lbillingDueDate.fines.modality := StrToInt(edQRCodefinesmodality.Text);

    lbillingDueDate.reduction.valuePerc := StrToCurr(edQRCodereductionvaluePerc.Text);
    lbillingDueDate.reduction.modality := StrToInt(edQRCodereductionmodality.Text);

    lbillingDueDate.discounts.fixedDateDiscountList.modality := StrToInt(edQRCodediscountsmodality.Text);
    lbillingDueDate.discounts.fixedDateDiscountList.fixedDateDiscounts.clear;

    lfixedDateDiscounts := lbillingDueDate.discounts.fixedDateDiscountList.fixedDateDiscounts.New;
    lfixedDateDiscounts.valuePerc := StrToCurr(edQRCodediscountsvaluePerc.Text);
    lfixedDateDiscounts.date := edQRCodediscountsdate.DateTime;

    lbillingDueDate.daysAfterDueDate := 10;
  end;

  linstantPayment.qrCodeImageGenerationSpecification.errorCorrectionLevel:='M';
  linstantPayment.qrCodeImageGenerationSpecification.imageWidth:=400;
  linstantPayment.qrCodeImageGenerationSpecification.generateImageRendering:=True;

  laddicionalInformation := linstantPayment.additionalInformation.New;
  laddicionalInformation.name:= edQRCodeADname.Text;
  laddicionalInformation.content:= edQRCodeADcontent.Text;
  laddicionalInformation.showToPlayer:= chkQRCodeADshowToPayer.Checked;

  lQRCodeSolicitacao.recipients.clear;

  lrecipient := lQRCodeSolicitacao.recipients.New;
  lrecipient.account.accountID := ACBrPSPMatera1.AccountId;
  lrecipient.amount := RoundABNT(wValor, -2);
  lrecipient.currency := cMoeda;
  lrecipient.mediatorfee := RoundABNT(wMediatorFee, -2);
  lrecipient.recipientComment := edQRCoderecipientComment.Text;

  lQRCodeSolicitacao.callbackAddress := edQRCodeCallBack.Text;

  try
    mmLogOperacoes.Lines.Add(' Comando: ');
    mmLogOperacoes.Lines.Add('  - ACBrPSPMatera1.GerarQRCode(' + ACBrPSPMatera1.AccountId + ')' + sLineBreak);

    wOK := ACBrPSPMatera1.GerarQRCode;
    pnQRCodeResult.Visible := wOK;

    if wOK then
    begin
      wQRCode := Trim(ACBrPSPMatera1.QRCodeResposta.instantPayment.textContent);
      PintarQRCode(wQRCode, imGerarQRCodeImg.Picture.Bitmap, qrUTF8BOM);
      edCobCopiaECola.Text := wQRCode;
      mmLogOperacoes.Lines.Add(' Resposta: ' + sLineBreak + FormatarJson(ACBrPSPMatera1.QRCodeResposta.AsJSON));
    end
    else
      mmLogOperacoes.Lines.Add(' ErroResposta: ' + sLineBreak + FormatarJson(ACBrPSPMatera1.ErroResposta.AsJSON));
  except
    On E: Exception do
    begin
      mmLogOperacoes.Lines.Add('  ERRO AO GERAR QRCODE!' + sLineBreak + E.Message + sLineBreak +
        FormatarJson(ACBrPSPMatera1.ErroResposta.AsJSON));
      Abort;
    end;
  end;
end;

procedure TfrPixCDMatera.btDevolucaoClick(Sender: TObject);
var
  wTransactionID, wReasonCode, wExternalID: String;
  wValor, wMediatorFee: double;
  lDevolucaoSolicitacao: TMateraDevolucaoRequest;
begin
  ValidarConfiguracaoConta;
  wTransactionID := Trim(edDevolucaoCobTransactionID.Text);
  if EstaVazio(wTransactionID) then
  begin
    MessageDlg('Preencha o Transaction Id', mtError, [mbOK], 0);
    edDevolucaoCobTransactionID.SetFocus;
    Exit;
  end;

  wReasonCode := Trim(cbDevolucaoReasonCode.Text);
  if EstaVazio(wReasonCode) then
  begin
    MessageDlg('Preencha o código da devolução', mtError, [mbOK], 0);
    cbDevolucaoReasonCode.SetFocus;
    Exit;
  end;

  wExternalID := Trim(edDevolucaoExternalID.Text);
  if EstaVazio(wExternalID) then
  begin
    MessageDlg('Preencha o External ID', mtError, [mbOK], 0);
    edDevolucaoExternalID.SetFocus;
    Exit;
  end;

  wValor := StrToFloatDef(edDevolucaoValor.Text, 1);
  if (wValor<=0) then
  begin
    MessageDlg('Preencha o valor', mtError, [mbOK], 0);
    edDevolucaoValor.SetFocus;
    Exit;
  end;

  wMediatorFee := CalculaMediatorFee(wValor, edMediatorFeeDevolucao.Text, cbTipoMediatorFeeDevolucao.ItemIndex);
  if (wMediatorFee<=0) then
  begin
    MessageDlg('Preencha o Mediator Fee', mtError, [mbOK], 0);
    edMediatorFeeDevolucao.SetFocus;
    Exit;
  end;

  try
    lDevolucaoSolicitacao := ACBrPSPMatera1.DevolucaoSolicitacao;
    lDevolucaoSolicitacao.externalIdentifier := wExternalID;
    lDevolucaoSolicitacao.amount := wValor;
    lDevolucaoSolicitacao.returnReasonCode := wReasonCode;
    lDevolucaoSolicitacao.returnReasonInformation := EmptyStr;
    lDevolucaoSolicitacao.mediatorFee := wMediatorFee;

    mmLogOperacoes.Lines.Add(' Comando: ');
    mmLogOperacoes.Lines.Add('  - ACBrPSPMatera1.Devolucao(' +
      ACBrPSPMatera1.AccountId + ', ' + wTransactionID + ')' + sLineBreak);

    ACBrPSPMatera1.DevolucaoSolicitar(ACBrPSPMatera1.AccountId, wTransactionID);

    mmLogOperacoes.Lines.Add(' Resposta: ' + sLineBreak + FormatarJson(ACBrPSPMatera1.DevolucaoResposta.AsJSON));
  except
    On E: Exception do
      mmLogOperacoes.Lines.Add(E.Message + sLineBreak + FormatarJson(ACBrPSPMatera1.ErroResposta.AsJSON));
  end;
end;

procedure TfrPixCDMatera.btDevolucaoLimparDadosClick(Sender: TObject);
begin
  edDevolucaoCobTransactionID.Text := EmptyStr;
  edDevolucaoExternalID.Text := EmptyStr;
  edDevolucaoValor.Text := EmptyStr;
  cbDevolucaoReasonCode.ItemIndex := -1;
  edMediatorFeeDevolucao.Text := EmptyStr;
  cbTipoMediatorFeeDevolucao.ItemIndex := 0;
  cbTipoMediatorFeeDevolucaoChange(nil);
end;

procedure TfrPixCDMatera.btDevolucaoPreencherDadosClick(Sender: TObject);
begin
  edDevolucaoCobTransactionID.Text := CriarTxId;
  edDevolucaoExternalID.Text := CriarTxId;
  edDevolucaoValor.Text := '1';
  cbDevolucaoReasonCode.ItemIndex := 0;
  edMediatorFeeDevolucao.Text := '0,2';
  cbTipoMediatorFeeDevolucao.ItemIndex := 0;
  cbTipoMediatorFeeDevolucaoChange(nil);
end;

procedure TfrPixCDMatera.btFluxoCancelarVendaClick(Sender: TObject);
begin
  ReiniciarFluxo;
  LimparInterfaceFluxo;
end;

procedure TfrPixCDMatera.btFluxoCopiaECola1Click(Sender: TObject);
begin
  Clipboard.AsText := Trim(edFluxoCopiaECola.Text);
end;

procedure TfrPixCDMatera.btFluxoCopiaEColaClick(Sender: TObject);
begin
  Clipboard.AsText := Trim(edFluxoCopiaECola.Text);
end;

procedure TfrPixCDMatera.btFluxoEstornarPagtoClick(Sender: TObject);
var
  wTransactionID, wReasonCode, wExternalID: String;
  wMediatorFee: Double;
begin
  if fFluxoDados.StatusCobranca = mtsApproved then
  begin
    wTransactionID := Trim(fFluxoDados.transactionID);
    wReasonCode := 'MD06'; // Verifique a rotina de Devolução para identificar os códigos existentes.
    wExternalID := CriarTxId;

    wMediatorFee := CalculaMediatorFee(fFluxoDados.Total, edMediatorFeeEstorno.Text, cbTipoMediatorFeeEstorno.ItemIndex);
    if (wMediatorFee <= 0) then
    begin
      pgPrincipal.ActivePage := tsConfig;
      pgConfig.ActivePage := tsMatera;
      edMediatorFeeEstorno.SetFocus;
      MessageDlg('Preencha o Mediator Fee', mtError, [mbOK], 0);
      Exit;
    end;

    try
      with ACBrPSPMatera1.DevolucaoSolicitacao do
      begin
        externalIdentifier := wExternalID;
        amount := fFluxoDados.Total;
        returnReasonCode := wReasonCode;
        returnReasonInformation := EmptyStr;
        mediatorFee := wMediatorFee;
      end;

      mmLogOperacoes.Lines.Add(' Comando: ');
      mmLogOperacoes.Lines.Add('  - ACBrPSPMatera1.Devolucao(' +
        ACBrPSPMatera1.AccountId + ', ' + wTransactionID + ')' + sLineBreak);

      if ACBrPSPMatera1.DevolucaoSolicitar(ACBrPSPMatera1.AccountId, wTransactionID) then
      begin
        AtualizarStatus(mtsNone, mtsCreated);
        mmLogOperacoes.Lines.Add(' Resposta: ' + sLineBreak + FormatarJson(ACBrPSPMatera1.DevolucaoResposta.AsJSON));
        fFluxoDados.transactionIDEstorno := ACBrPSPMatera1.DevolucaoResposta.transactionId;
        tmConsultarEstorno.Enabled := True;
      end;
    except
      On E: Exception do
        mmLogOperacoes.Lines.Add(E.Message + sLineBreak + FormatarJson(ACBrPSPMatera1.ErroResposta.AsJSON));
    end;
  end
  else
    ShowMessage('A venda precisa estar com status PAGAMENTO FINALIZADO para realizar o estorno.');
end;

procedure TfrPixCDMatera.btFluxoNovaVendaClick(Sender: TObject);
begin
  ReiniciarFluxo;
  LimparInterfaceFluxo;
end;

procedure TfrPixCDMatera.btFluxoPagarClick(Sender: TObject);
var
  wQRCode, wAccountId, wChavePIX , wErro: String;
  wValor, wMediatorFee: Double;
begin
  HabilitarInterfaceFluxo(False);
  try
    ACBrPSPMatera1.Clear;
    ValidarConfiguracaoConta;
    wAccountId := Trim(ACBrPSPMatera1.AccountId);
    wChavePIX := Trim(cbChavePIX.Text);

    wValor := StrToFloatDef(edFluxoValor.Text, 1);
    wMediatorFee := CalculaMediatorFee(wValor, edMediatorFee.Text, cbTipoMediatorFee.ItemIndex);

    // Preenchendo dados do QRcode
    with ACBrPSPMatera1.QRCodeSolicitacao do
    begin
      externalIdentifier := CriarTxId;
      totalAmount := RoundABNT(wValor, -2);
      currency:= cMoeda;
      paymentInfo.transactionType := 'InstantPayment';
      fFluxoDados.Total := totalAmount;
    end;

    ACBrPSPMatera1.QRCodeSolicitacao.paymentInfo.instantPayment.alias_ := wChavePIX;
    ACBrPSPMatera1.QRCodeSolicitacao.paymentInfo.instantPayment.expiration := 600;

    with ACBrPSPMatera1.QRCodeSolicitacao.paymentInfo.instantPayment.qrCodeImageGenerationSpecification do
    begin
      errorCorrectionLevel := 'M';
      imageWidth := 400;
      generateImageRendering := False;
    end;

    ACBrPSPMatera1.QRCodeSolicitacao.paymentInfo.instantPayment.additionalInformation.Clear;
    with ACBrPSPMatera1.QRCodeSolicitacao.paymentInfo.instantPayment.additionalInformation.New do
    begin
      name:= edFluxoClienteDoc.Text;
      content:= edFluxoClienteNome.Text;
      showToPlayer:= True;
    end;

    ACBrPSPMatera1.QRCodeSolicitacao.recipients.Clear;
    with ACBrPSPMatera1.QRCodeSolicitacao.recipients.New do
    begin
      account.accountID := wAccountId;
      amount := RoundABNT(wValor, -2);
      currency := cMoeda;
      mediatorfee := wMediatorFee;
      recipientComment := EmptyStr;
    end;

    try
      if ACBrPSPMatera1.GerarQRCode then
      begin
        wQRCode := Trim(ACBrPSPMatera1.QRCodeResposta.instantPayment.textContent);
        PintarQRCode(wQRCode, imFluxoQRCode.Picture.Bitmap, qrUTF8BOM);
        edFluxoCopiaECola.Text := wQRCode;
        edFluxotransactionID.Text := ACBrPSPMatera1.QRCodeResposta.transactionId;
        fFluxoDados.transactionID := ACBrPSPMatera1.QRCodeResposta.transactionId;
        AtualizarStatus(mtsCreated);

        tmConsultarPagto.Enabled := True;
      end
      else
      begin
        wErro := 'ERRO AO GERAR QRCODE:' + sLineBreak +
        ACBrPSPMatera1.ErroResposta.code + ' - ' + ACBrPSPMatera1.ErroResposta.description;
        mmLogOperacoes.Lines.Add(wErro);
        ShowMessage(wErro);
      end;
    except
      On E: Exception do
      begin
        wErro := '- ERRO AO GERAR QRCODE:' + sLineBreak + E.Message;
        mmLogOperacoes.Lines.Add(wErro);
        ShowMessage(wErro);
      end;
    end;
  finally
    HabilitarInterfaceFluxo;
  end;
end;

procedure TfrPixCDMatera.btFluxotransactionIDClick(Sender: TObject);
begin
  Clipboard.AsText := Trim(edFluxotransactionID.Text);
end;

procedure TfrPixCDMatera.btLerParametrosClick(Sender: TObject);
begin
  LerConfiguracao;
end;

procedure TfrPixCDMatera.btLogGerencialLimparClick(Sender: TObject);
begin
  mmLogGerencial.Clear;
end;

procedure TfrPixCDMatera.btLogOperacoesLimparClick(Sender: TObject);
begin
  mmLogOperacoes.Clear;
end;

procedure TfrPixCDMatera.btProxyVerSenhaClick(Sender: TObject);
begin
  {$IfDef FPC}
  if btProxyVerSenha.Down then
    edProxySenha.EchoMode := emNormal
  else
    edProxySenha.EchoMode := emPassword;
  {$Else}
  if btProxyVerSenha.Down then
    edProxySenha.PasswordChar := #0
  else
    edProxySenha.PasswordChar := '*';
  {$EndIf}
end;

procedure TfrPixCDMatera.btQRCodeCriarLimparDadosClick(Sender: TObject);
begin
  edQRCodeValor.Text := EmptyStr;
  cbQRCodeTipoMediatorFee.ItemIndex := 0;
  cbQRCodeTipoMediatorFeeChange(nil);
  edQRCodeMediatorFee.Text := EmptyStr;
  edQRCodeADname.Text := EmptyStr;
  edQRCodeADcontent.Text := EmptyStr;
  chkQRCodeADshowToPayer.Checked := False;
  edQRCodeCallBack.Text := EmptyStr;
  edQRCoderecipientComment.Text := EmptyStr;
  cbQRCodeTipoCobranca.ItemIndex := 0;
  edQRCodedueDate.DateTime := Now;
  edQRCodeinterestsvaluePerc.Text := EmptyStr;
  edQRCodeinterestsmodality.Text := EmptyStr;
  edQRCodefinesvaluePerc.Text := EmptyStr;
  edQRCodefinesmodality.Text := EmptyStr;
  edQRCodereductionvaluePerc.Text := EmptyStr;
  edQRCodereductionmodality.Text := EmptyStr;
  edQRCodediscountsvaluePerc.Text := EmptyStr;
  edQRCodediscountsmodality.Text := EmptyStr;
  edQRCodediscountsdate.DateTime := Now;
  edQRCodePayercpfcnpj.Text := EmptyStr;
  edQRCodePayerName.Text := EmptyStr;
  edQRCodepayerCEP.Text := EmptyStr;
  edQRCodepayercity.Text := EmptyStr;
  edQRCodepayeruf.Text := EmptyStr;
  edQRCodepayerstreet.Text := EmptyStr;
end;

procedure TfrPixCDMatera.btQRCodeCriarPreencherDadosClick(Sender: TObject);
begin
  edQRCodeExternalID.Text := CriarTxId;
  edQRCodeValor.Text := '5,00';
  edQRCodeMediatorFee.Text := '0,5';
  edQRCodeADname.Text := 'ID DO PEDIDO';
  edQRCodeADcontent.Text := '123456';
  chkQRCodeADshowToPayer.Checked := True;
  edQRCodeCallBack.Text := 'https://testemockqr.requestcatcher.com/';
  edQRCoderecipientComment.Text := 'Comentario 123';

  // cobrança
  edQRCodedueDate.DateTime := Now + 30;
  edQRCodeinterestsvaluePerc.Text := '1';
  edQRCodeinterestsmodality.Text := '1';
  edQRCodefinesvaluePerc.Text := '2';
  edQRCodefinesmodality.Text := '1';
  edQRCodereductionvaluePerc.Text := '1';
  edQRCodereductionmodality.Text := '1';
  edQRCodediscountsvaluePerc.Text := '1';
  edQRCodediscountsmodality.Text := '1';
  edQRCodediscountsdate.DateTime := Now + 25;
  edQRCodePayercpfcnpj.Text := '00971484074';
  edQRCodePayerName.Text := 'Nome do Pagador';
  edQRCodepayerCEP.Text := '13010210';
  edQRCodepayercity.Text := 'Mococa';
  edQRCodepayeruf.Text := 'SP';
  edQRCodepayerstreet.Text := 'Rua Sacramento';
end;

procedure TfrPixCDMatera.btQRCodeGerarExternalID1Click(Sender: TObject);
begin
  edDevolucaoExternalID.Text := CriarTxId;
end;

procedure TfrPixCDMatera.btQRCodeGerarExternalIDClick(Sender: TObject);
begin
  edQRCodeExternalID.Text := CriarTxId;
end;

procedure TfrPixCDMatera.btRetiradaClick(Sender: TObject);
var
  wExternalID, wAlias, wendToEndId, wPSPId, wtaxId, wADBranch, wADAccount,
    wName, wbankDestination, wbranchdestination, waccountDestination: String;
  wValor, wMediatorFee: Double;
  LRetiradaSolicitacao: TMateraRetiradaRequest;
  Lrecipient: TMateraInstantPaymentRecipient;
  lBankTransfer: TMateraBankTransfer;
begin
  ACBrPSPMatera1.Clear;
  ValidarConfiguracaoConta;

  wExternalID := Trim(edRetiradaExternalID.Text);
  if EstaVazio(wExternalID) then
  begin
    MessageDlg('Preencha a External ID', mtError, [mbOK], 0);
    edRetiradaExternalID.SetFocus;
    Exit;
  end;

  wValor := StrToFloatDef(edRetiradaValor.Text, 0);
  wMediatorFee := StrToFloatDef(edRetiradaMediatorFee.Text, 0);

  if cbRetiradaTipoRetirada.ItemIndex = 0 then
  begin  // PIX
    wAlias := Trim(edRetiradaAliasDestinatario.Text);
    if EstaVazio(wAlias) then
    begin
      MessageDlg('Preencha o Alias Destinatário', mtError, [mbOK], 0);
      edRetiradaAliasDestinatario.SetFocus;
      Exit;
    end;

    wendToEndId := Trim(edRetiradaendToEndId.Text);
    if EstaVazio(wendToEndId) then
    begin
      MessageDlg('Preencha o End To End ID', mtError, [mbOK], 0);
      edRetiradaendToEndId.SetFocus;
      Exit;
    end;

    wPSPId := Trim(edRetiradaPSPId.Text);
    if EstaVazio(wPSPId) then
    begin
      MessageDlg('Preencha o PSP ID', mtError, [mbOK], 0);
      edRetiradaPSPId.SetFocus;
      Exit;
    end;

    wtaxId := Trim(edRetiradaTaxID.Text);
    if EstaVazio(wtaxId) then
    begin
      MessageDlg('Preencha o Tax ID', mtError, [mbOK], 0);
      edRetiradaTaxID.SetFocus;
      Exit;
    end;

    wADBranch := Trim(edRetiradaAccountdestinationBranch.Text);
    if EstaVazio(wADBranch) then
    begin
      MessageDlg('Preencha o Account Destination Branch', mtError, [mbOK], 0);
      edRetiradaAccountdestinationBranch.SetFocus;
      Exit;
    end;

    wADAccount := Trim(edRetiradaAccountDestinationAccount.Text);
    if EstaVazio(wADAccount) then
    begin
      MessageDlg('Preencha o Account Destination Account', mtError, [mbOK], 0);
      edRetiradaAccountDestinationAccount.SetFocus;
      Exit;
    end;
  end
  else   //TED
  begin
    wbankDestination := Trim(edRetiradaTEDBankDestination.Text);
    if EstaVazio(wbankDestination) then
    begin
      MessageDlg('Preencha o Bank Destination', mtError, [mbOK], 0);
      edRetiradaTEDBankDestination.SetFocus;
      Exit;
    end;

    wName := Trim(edRetiradaTEDName.Text);
    if EstaVazio(wName) then
    begin
      MessageDlg('Preencha o Name', mtError, [mbOK], 0);
      edRetiradaTEDName.SetFocus;
      Exit;
    end;

    wtaxId := Trim(edRetiradaTEDTaxID.Text);
    if EstaVazio(wtaxId) then
    begin
      MessageDlg('Preencha o TaxID', mtError, [mbOK], 0);
      edRetiradaTEDTaxID.SetFocus;
      Exit;
    end;

    wbranchdestination := Trim(edRetiradaTEDBranchDestination.Text);
    if EstaVazio(wbranchdestination) then
    begin
      MessageDlg('Preencha o Branch Destination', mtError, [mbOK], 0);
      edRetiradaTEDBranchDestination.SetFocus;
      Exit;
    end;

    waccountDestination := Trim(edRetiradaTEDAccountDestination.Text);
    if EstaVazio(waccountDestination) then
    begin
      MessageDlg('Preencha o Account Destination', mtError, [mbOK], 0);
      edRetiradaTEDAccountDestination.SetFocus;
      Exit;
    end;

  end;

  // Preenchendo dados da Retirada
  LRetiradaSolicitacao := ACBrPSPMatera1.RetiradaSolicitacao;
  LRetiradaSolicitacao.externalIdentifier := edRetiradaExternalID.Text;
  LRetiradaSolicitacao.totalAmount := RoundABNT(wValor, 2);
  LRetiradaSolicitacao.mediatorFee := RoundABNT(wMediatorFee, 2);;
  LRetiradaSolicitacao.currency:= cMoeda;

  if cbRetiradaTipoRetirada.ItemIndex = 0 then
  begin
    LRetiradaSolicitacao.withdrawInfo.withdrawType := mwtInstantPayment;

    Lrecipient := LRetiradaSolicitacao.withdrawInfo.instantPayment.recipient;

    Lrecipient.alias_ := wAlias;
    Lrecipient.endToEndIdQuery := wendToEndId;
    Lrecipient.pspid := wPSPId;

    Lrecipient.TaxIdentifierRequest.taxId := wtaxId;
    Lrecipient.TaxIdentifierRequest.country := cPais;

    Lrecipient.accountDestination.branch := wADBranch;
    Lrecipient.accountDestination.account := wADAccount;
    Lrecipient.accountDestination.accountType := matdIP;
  end
  else
  begin
    LRetiradaSolicitacao.withdrawInfo.withdrawType := mwtBankTransfer;

    lBankTransfer := LRetiradaSolicitacao.withdrawInfo.bankTransfer;
    lBankTransfer.bankDestination := wbankDestination;
    lBankTransfer.branchDestination := wbranchDestination;
    lBankTransfer.accountDestination := waccountDestination;
    lBankTransfer.taxIdentifier.taxId := wtaxId;
    lBankTransfer.taxIdentifier.country := cPais;
    lBankTransfer.personType := cbRetiradaPersonType.Text;
    lBankTransfer.name := wName;
    lBankTransfer.accountTypeDestination := StringToMateraAccountTypeDestination(cbRetiradaAccountTypeDestination.Text);
  end;

  try
    mmLogOperacoes.Lines.Add(' Comando: ');
    mmLogOperacoes.Lines.Add('  - ACBrPSPMatera1.Retirada(' + ACBrPSPMatera1.AccountId + ')' + sLineBreak);

    if ACBrPSPMatera1.RetiradaSolicitar(ACBrPSPMatera1.AccountId) then
    begin
      mmLogOperacoes.Lines.Add('  RETIRADA REALIZADA COM SUCESSO!');
      mmLogOperacoes.Lines.Add(' Resposta: ' + sLineBreak + FormatarJson(ACBrPSPMatera1.RetiradaResposta.AsJSON));
    end
    else
      mmLogOperacoes.Lines.Add(' ErroResposta: ' + sLineBreak + FormatarJson(ACBrPSPMatera1.ErroResposta.AsJSON));
  except
    On E: Exception do
    begin
      mmLogOperacoes.Lines.Add('  ERRO AO REALIZAR RETIRADA!' + sLineBreak + E.Message + sLineBreak +
        FormatarJson(ACBrPSPMatera1.ErroResposta.AsJSON));
      Abort;
    end;
  end;
end;

procedure TfrPixCDMatera.btretiradaConsultaLimparDadosClick(Sender: TObject);
begin
  edRetiradaConsultaAliasAccountID.Text := EmptyStr;
  edRetiradaConsultaaliasAliasDestinatario.Text := EmptyStr;
end;

procedure TfrPixCDMatera.btRetiradaConsultaPreencherDadosClick(Sender: TObject);
begin
  edRetiradaConsultaAliasAccountID.Text := 'CC79ECDE-46C8-64ED-5C61-4F2255C08CC1';
  edRetiradaConsultaaliasAliasDestinatario.Text := '70288335007';
end;

procedure TfrPixCDMatera.btRetiradaGerarExternalIDClick(Sender: TObject);
begin
  edRetiradaExternalID.Text := CriarTxId;
end;

procedure TfrPixCDMatera.btretiradaLimparDadosClick(Sender: TObject);
begin
  edRetiradaExternalID.Text := EmptyStr;
  edRetiradaValor.Text := EmptyStr;
  edRetiradaMediatorFee.Text := EmptyStr;
  cbRetiradaTipoRetirada.ItemIndex := 0;
  cbRetiradaTipoRetiradaChange(nil);
  edRetiradaAliasDestinatario.Text := EmptyStr;
  edRetiradaendToEndId.Text := EmptyStr;
  edRetiradaPSPId.Text := EmptyStr;
  edRetiradaTaxID.Text := EmptyStr;
  edRetiradaAccountdestinationBranch.Text := EmptyStr;
  edRetiradaAccountDestinationAccount.Text := EmptyStr;
end;

procedure TfrPixCDMatera.btRetiradaPreencherDadosClick(Sender: TObject);
begin
  edRetiradaExternalID.Text := CriarTxId;
  edRetiradaValor.Text := '1';
  edRetiradaMediatorFee.Text := '0.5';
  cbRetiradaTipoRetirada.ItemIndex := 0;
  cbRetiradaTipoRetiradaChange(nil);
  edRetiradaAliasDestinatario.Text := '70288335007';
  edRetiradaendToEndId.Text := 'E231144472023091417061Tud04Wy3mM';
  edRetiradaPSPId.Text := '23114447';
  edRetiradaTaxID.Text := '70288335007';
  edRetiradaAccountdestinationBranch.Text := '340';
  edRetiradaAccountDestinationAccount.Text := '292400';
end;

procedure TfrPixCDMatera.LerConfiguracao;
var
  wIni: TIniFile;
begin
  AdicionarLinhaLog('- LerConfiguracao: '+ NomeArquivoConfig);
  wIni := TIniFile.Create(NomeArquivoConfig);
  try
    cbAmbiente.ItemIndex := wIni.ReadInteger('PIX','Ambiente', 0);
    edTimeout.Value := wIni.ReadInteger('PIX', 'TimeOut', ChttpTimeOutDef);

    seCobrancaExpiracao.Value := wIni.ReadInteger('Cobranca', 'Expiracao', seCobrancaExpiracao.Value);

    edProxyHost.Text := wIni.ReadString('Proxy', 'Host', '');
    edProxyPorta.Text := wIni.ReadString('Proxy', 'Porta', '');
    edProxyUsuario.Text := wIni.ReadString('Proxy', 'User', '');
    edProxySenha.Text := StrCrypt(DecodeBase64(wIni.ReadString('Proxy', 'Pass', '')), CURL_ACBR);

    edLogArquivo.Text := wIni.ReadString('Log', 'Arquivo', '');
    cbLogNivel.ItemIndex := wIni.ReadInteger('Log', 'Nivel', 1);
    
    edCNPJ.Text := wIni.ReadString('Matera', 'CNPJ', '');
    edPSPClientID.Text := wIni.ReadString('Matera', 'ClientID', '');
    edPSPClientSecret.Text := wIni.ReadString('Matera', 'ClientSecret', '');
    edPSPSecretKey.Text := wIni.ReadString('Matera', 'SecretKey', '');
    edArqCertificado.Text := wIni.ReadString('Matera', 'ArqCertificado', '');
    edArqChavePrivada.Text := wIni.ReadString('Matera', 'ArqChavePrivada', '');

    PopularConfigContas;
    cbAccountId.Text := wIni.ReadString('Matera', 'accountId', '');
    cbChavePIX.Text := wIni.ReadString('Matera', 'chavePIX', '');

    LerMediatorFee(wIni, cbAccountId.Text);
  finally
    wIni.Free;
  end;

  AplicarConfiguracao;
  LigarAlertasdeErrosDeConfiguracao;
end;

procedure TfrPixCDMatera.LerMediatorFee(wIni: TIniFile; aAccountID:String);
var
  wSection, wConta: String;
  I: Integer;
begin
  I := 1;
  wSection := 'Conta' + IntToStrZero(I, 3);
  while wIni.SectionExists(wSection) do
  begin
    wConta := wIni.ReadString(wSection, 'AccountId', EmptyStr);
    if (wConta = aAccountId) then
    begin
      edMediatorFee.Text := FloatToStr(wIni.ReadFloat(wSection, 'MediatorFeeQRCode',0));
      cbTipoMediatorFee.ItemIndex := wIni.ReadInteger(wSection, 'TipoMediatorFeeQRCode', 0);
      edMediatorFeeEstorno.Text := FloatToStr(wIni.ReadFloat(wSection, 'MediatorFeeDevolucao', 0));
      cbTipoMediatorFeeEstorno.ItemIndex := wIni.ReadInteger(wSection, 'TipoMediatorFeedevolucao', 0);
      Break;
    end;

    Inc(I);
    wSection := 'Conta' + IntToStrZero(I, 3);
  end;
end;

procedure TfrPixCDMatera.SalvarMediatorFee(aAccountID:String);
var
  wSection, wConta: String;
  wIni: TIniFile;
  I: Integer;
begin
  wIni := TIniFile.Create(NomeArquivoConfig);
  try
    I := 1;
    wSection := 'Conta' + IntToStrZero(I, 3);
    while wIni.SectionExists(wSection) do
    begin
      wConta := wIni.ReadString(wSection, 'AccountId', EmptyStr);
      if (wConta = aAccountId) then
      begin
        wIni.WriteFloat(wSection, 'MediatorFeeQRCode', StrToFloat(edMediatorFee.Text));
        wIni.WriteInteger(wSection, 'TipoMediatorFeeQRCode', cbTipoMediatorFee.ItemIndex);
        wIni.WriteFloat(wSection, 'MediatorFeeDevolucao', StrToFloat(edMediatorFeeEstorno.Text));
        wIni.WriteInteger(wSection, 'TipoMediatorFeedevolucao', cbTipoMediatorFeeEstorno.ItemIndex);
        Break;
      end;

      Inc(I);
      wSection := 'Conta' + IntToStrZero(I, 3);
    end;
  finally
    wIni.Free;
  end;
end;

procedure TfrPixCDMatera.GravarConfiguracao;
var
  wIni: TIniFile;
begin
  AdicionarLinhaLog('- LerConfiguracao: ' + NomeArquivoConfig);
  wIni := TIniFile.Create(NomeArquivoConfig);
  try
    wIni.WriteInteger('PIX','Ambiente', cbAmbiente.ItemIndex);
    wIni.WriteInteger('PIX', 'TimeOut', edTimeout.Value);

    wIni.WriteInteger('Cobranca', 'Expiracao', seCobrancaExpiracao.Value);

    wIni.WriteString('Proxy', 'Host', edProxyHost.Text);
    wIni.WriteString('Proxy', 'Porta', edProxyPorta.Text);
    wIni.WriteString('Proxy', 'User', edProxyUsuario.Text);
    wIni.WriteString('Proxy', 'Pass', EncodeBase64(StrCrypt(edProxySenha.Text, CURL_ACBR)));

    wIni.WriteString('Log', 'Arquivo', edLogArquivo.Text);
    wIni.WriteInteger('Log', 'Nivel', cbLogNivel.ItemIndex);
    
    wIni.WriteString('Matera', 'CNPJ', edCNPJ.Text);
    wIni.WriteString('Matera', 'ClientID', edPSPClientID.Text);
    wIni.WriteString('Matera', 'SecretKey', edPSPSecretKey.Text);
    wIni.WriteString('Matera', 'ArqCertificado', edArqCertificado.Text);
    wIni.WriteString('Matera', 'ArqChavePrivada', edArqChavePrivada.Text);

    wIni.WriteString('Matera', 'accountID', cbAccountId.Text);
    wIni.WriteString('Matera', 'ChavePIX', cbChavePIX.Text);

    if (not ExisteConta(cbAccountId.Text)) then
      SalvarNovaConta(cbAccountId.Text);
    SalvarMediatorFee(cbAccountId.Text);
  finally
     wIni.Free;
  end;

  LigarAlertasdeErrosDeConfiguracao;
end;

procedure TfrPixCDMatera.AplicarConfiguracao;
begin
  AdicionarLinhaLog('- AplicarConfiguracao');
  ACBrPixCD1.Ambiente := TACBrPixCDAmbiente(cbAmbiente.ItemIndex);
  ACBrPixCD1.TimeOut := edTimeout.Value;

  ACBrPixCD1.Proxy.Host := edProxyHost.Text;
  ACBrPixCD1.Proxy.Port := edProxyPorta.Text;
  ACBrPixCD1.Proxy.User := edProxyUsuario.Text;
  ACBrPixCD1.Proxy.Pass := edProxySenha.Text;

  ACBrPixCD1.ArqLOG := edLogArquivo.Text;
  ACBrPixCD1.NivelLog := cbLogNivel.ItemIndex;

  ACBrPSPMatera1.ClientID := edPSPClientID.Text;
  ACBrPSPMatera1.ClientSecret := edPSPClientSecret.Text;
  ACBrPSPMatera1.SecretKey := edPSPSecretKey.Text;
  ACBrPSPMatera1.ArquivoCertificado := edArqCertificado.Text;
  ACBrPSPMatera1.ArquivoChavePrivada := edArqChavePrivada.Text;
  ACBrPSPMatera1.AccountId := cbAccountId.Text;
end;

procedure TfrPixCDMatera.ValidarConfiguracaoCNPJ;
begin
  if EstaVazio(edCNPJ.Text) then
  begin
    pgPrincipal.ActivePage := tsContaEChaves;
    pgContasEChaves.ActivePage := tsContaCriar;

    MessageDlg('Configure o CNPJ para testes de Conta', mtError, [mbOK], 0);
    Abort;
  end;
end;

procedure TfrPixCDMatera.ValidarConfiguracaoConta;
begin
  if EstaVazio(ACBrPSPMatera1.AccountId) or EstaVazio(cbChavePIX.Text) then
  begin
    pgPrincipal.ActivePage := tsConfig;
    pgConfig.ActivePage := tsMatera;

    MessageDlg('Configure a Conta e Chave PIX para testes de Operações', mtError, [mbOK], 0);
    Abort;
  end
  else
    CarregarMediatorFee;
end;

procedure TfrPixCDMatera.CarregarMediatorFee;
var
  wSection, wConta: String;
  wIni: TIniFile;
  I: Integer;
begin
  AdicionarLinhaLog('- Carregando Mediator Fee: ' + ACBrPSPMatera1.AccountId);
  wIni := TIniFile.Create(NomeArquivoConfig);
  try
    I := 1;
    wSection := 'Conta' + IntToStrZero(I, 3);
    while wIni.SectionExists(wSection) do
    begin
      wConta := wIni.ReadString(wSection, 'AccountId', EmptyStr);
      if (wConta = ACBrPSPMatera1.AccountId) then
      begin
        edMediatorFee.Text := FloatToStr(wIni.ReadFloat(wSection, 'MediatorFeeQRCode', 0));
        cbTipoMediatorFee.ItemIndex := wIni.ReadInteger(wSection, 'TipoMediatorFeeQRCode', 0);
        edMediatorFeeEstorno.Text := FloatToStr(wIni.ReadFloat(wSection, 'MediatorFeeDevolucao', 0));
        cbTipoMediatorFeeEstorno.ItemIndex := wIni.ReadInteger(wSection, 'TipoMediatorFeeDevolucao', 0);

        Break;
      end;

      Inc(I);
      wSection := 'Conta' + IntToStrZero(I, 3);
    end;

  finally
    wIni.Free;
  end;
end;

procedure TfrPixCDMatera.PopularConfigContas;
var
  wSection, wConta: String;
  wIni: TIniFile;
  I: Integer;
begin
  cbAccountId.Items.Clear;
  cbChavePIX.Items.Clear;
  AdicionarLinhaLog('- Lendo Contas Gravadas no Ini');
  wIni := TIniFile.Create(NomeArquivoConfig);
  try
    I := 1;
    wSection := 'Conta' + IntToStrZero(I, 3);
    while wIni.SectionExists(wSection) do
    begin
      wConta := wIni.ReadString(wSection, 'AccountId', EmptyStr);
      AdicionarLinhaLog('  AccountId: ' + wConta);
      cbAccountId.Items.Add(wConta);
      Inc(I);
      wSection := 'Conta' + IntToStrZero(I, 3);
    end;
  finally
    wIni.Free;
  end;
end;

procedure TfrPixCDMatera.PopularConfigChavesPIX(aAccountId: String);
var
  wSection, wConta, wChave: String;
  wIni: TIniFile;
  I, J: Integer;
begin
  cbChavePIX.Items.Clear;
  AdicionarLinhaLog('- Lendo Chaves PIX da Conta: ' + aAccountId);
  wIni := TIniFile.Create(NomeArquivoConfig);
  try
    I := 1;
    wSection := 'Conta' + IntToStrZero(I, 3);
    for I := I to 20 do
    begin
      wSection := 'Conta' + IntToStrZero(I, 3);
      wConta := wIni.ReadString(wSection, 'AccountId', EmptyStr);
      if (wConta = aAccountId) then
      begin
        for J := 1 to 20 do
        begin
          wChave := 'Chave' + IntToStrZero(J, 3);
          if wIni.ValueExists(wSection, wChave) then
            cbChavePIX.Items.Add(wIni.ReadString(wSection, wChave, EmptyStr));
        end;
        LerMediatorFee(wIni, aAccountId);
        Break;
      end;
    end;
  finally
    wIni.Free;
  end;
end;

procedure TfrPixCDMatera.RemoverConta(aAccountId: String);
var
  wSection, wConta: String;
  wIni: TIniFile;
  I: Integer;
begin
  AdicionarLinhaLog('- Removendo conta: ' + aAccountId);
  wIni := TIniFile.Create(NomeArquivoConfig);
  try
    I := 1;
    wSection := 'Conta' + IntToStrZero(I, 3);
    while wIni.SectionExists(wSection) do
    begin
      wConta := wIni.ReadString(wSection, 'AccountId', EmptyStr);
      if (wConta = aAccountId) then
      begin
        wIni.EraseSection(wSection);
        Break;
      end;

      Inc(I);
      wSection := 'Conta' + IntToStrZero(I, 3);
    end;

//    wIni.WriteString(wSection, 'AccountId', aAccountId);
  finally
    wIni.Free;
  end;
end;

procedure TfrPixCDMatera.SalvarNovaConta(aAccountId: String);
var
  wSection: String;
  wIni: TIniFile;
  I: Integer;
begin
  AdicionarLinhaLog('- Gravando nova conta: ' + aAccountId);
  wIni := TIniFile.Create(NomeArquivoConfig);
  try
    I := 1;
    wSection := 'Conta' + IntToStrZero(I, 3);
    while wIni.SectionExists(wSection) do
    begin
      Inc(I);
      wSection := 'Conta' + IntToStrZero(I, 3);
    end;

    wIni.WriteString(wSection, 'AccountId', aAccountId);
  finally
    wIni.Free;
  end;
end;

function TfrPixCDMatera.ExisteConta(aAccountId: String): Boolean;
var
  wSection, wConta: String;
  wIni: TIniFile;
  I: Integer;
begin
  Result := False;
  wIni := TIniFile.Create(NomeArquivoConfig);
  try
    I := 1;
    wSection := 'Conta' + IntToStrZero(I, 3);
    while wIni.SectionExists(wSection) do
    begin
      wConta := wIni.ReadString(wSection, 'AccountId', EmptyStr);
      if (wConta = aAccountId) then
      begin
        Result := True;
        Exit;
      end;

      Inc(I);
      wSection := 'Conta' + IntToStrZero(I, 3);
    end;
  finally
    wIni.Free;
  end;
end;

procedure TfrPixCDMatera.RemoverChavePIX(aAccountId, aChavePIX: String);
var
  wSection, wChave, wConta: String;
  wIni: TIniFile;
  I, J: Integer;
begin
  AdicionarLinhaLog(
    '- Removendo ChavePIX. AccountId: '+ aAccountId +' - '+'Chave PIX: '+ aChavePIX);
  wIni := TIniFile.Create(NomeArquivoConfig);
  try
    I := 1;
    wSection := 'Conta' + IntToStrZero(I, 3);
    while wIni.SectionExists(wSection) do
    begin
      wConta := wIni.ReadString(wSection, 'AccountId', EmptyStr);
      if (wConta = aAccountId) then
      begin
        for J := 1 to 20 do
        begin
          wChave := 'Chave' + IntToStrZero(J, 3);
          if wIni.ValueExists(wSection, wChave) then
            wIni.DeleteKey(wSection, wChave);
        end;
        Exit;
      end;

      Inc(I);
      wSection := 'Conta' + IntToStrZero(I, 3);
    end;
  finally
    wIni.Free;
  end;
end;

procedure TfrPixCDMatera.SalvarChavesPIX(aAccountId: String);
var
  wSection, wChave, wConta: String;
  wIni: TIniFile;
  I, J: Integer;
begin
  if (ACBrPSPMatera1.ChavesPIXResposta.Count <= 0) then
    Exit;

  wIni := TIniFile.Create(NomeArquivoConfig);
  try
    I := 1;
    wSection := 'Conta' + IntToStrZero(I, 3);
    while wIni.SectionExists(wSection) do
    begin
      wConta := wIni.ReadString(wSection, 'AccountId', EmptyStr);
      if (wConta = aAccountId) then
      begin
        for J := 0 to ACBrPSPMatera1.ChavesPIXResposta.Count - 1 do
        begin
          if (ACBrPSPMatera1.ChavesPIXResposta[J].status = mastActive) then
          begin
            wChave := 'Chave' + IntToStrZero(J+1, 3);
            wIni.WriteString(wSection, wChave, ACBrPSPMatera1.ChavesPIXResposta[J].name);
          end;
        end;
        Break;
      end;

      Inc(I);
      wSection := 'Conta' + IntToStrZero(I, 3);
    end;
  finally
    wIni.Free;
  end;
end;

procedure TfrPixCDMatera.AdicionarLinhaLog(aMsg: String);
begin
  if Assigned(mmLogGerencial) then
    mmLogGerencial.Lines.Add(aMsg);
  if Assigned(mmLogOperacoes) then
    mmLogOperacoes.Lines.Add(aMsg);
end;

procedure TfrPixCDMatera.HabilitarInterfaceFluxo(Habilitar: Boolean);
begin
  pnFluxoBackground.Enabled := Habilitar;
end;

procedure TfrPixCDMatera.TratarException(Sender: TObject; E: Exception);
begin
  AdicionarLinhaLog('');
  AdicionarLinhaLog('***************' + E.ClassName + '***************');
  AdicionarLinhaLog(E.Message);
  AdicionarLinhaLog('');

  if (pgPrincipal.ActivePage <> tsTestes) then
    MessageDlg(E.Message, mtError, [mbOK], 0);
end;

procedure TfrPixCDMatera.AvaliarInterfaceFluxo;
var
  wVendendo, wSemEstorno, wAguardandoPagto: Boolean;
begin
  with fFluxoDados do
  begin
    wSemEstorno := (StatusDevolucao = mtsNone);
    wAguardandoPagto := (StatusCobranca = mtsCreated);
    wVendendo := (StatusCobranca = mtsNone) and (StatusDevolucao = mtsNone);

    gbFluxoCliente.Enabled := wVendendo;

    btFluxoPagar.Visible := wVendendo;
    btFluxoPagar.Enabled := wVendendo and (Total > 0);

    pnFluxoQRCode.Visible := wAguardandoPagto;
    pnFluxoCopiaECola.Visible := wAguardandoPagto;
    pnSiteEfetuarPagto.Visible := wAguardandoPagto and (ACBrPixCD1.Ambiente <> ambProducao);
    btFluxoEstornarPagto.Visible := (StatusCobranca = mtsApproved) and wSemEstorno;
    btFluxoNovaVenda.Visible := (StatusCobranca = mtsApproved) or (StatusDevolucao = mtsApproved);
    btFluxoCancelarVenda.Visible := wAguardandoPagto;
  end;
end;

procedure TfrPixCDMatera.InicializarComponentesDefault;
var
  i: TACBrPixCDAmbiente;
  j: TMateraClientType;
  k: TMateraAccountType;
begin
  ACBrPixCD1.PSP := ACBrPSPMatera1;
  lbErroCertificado.Caption := EmptyStr;
  lbErroChavePrivada.Caption := EmptyStr;
  pnContaCriarCorporate.Parent := gbDadosAdicionais;

  cbAmbiente.Items.Clear;
  for i := Low(TACBrPixCDAmbiente) to High(TACBrPixCDAmbiente) do
     cbAmbiente.Items.Add(GetEnumName(TypeInfo(TACBrPixCDAmbiente), Integer(i)));

  cbContaCriarTipoCliente.Items.Clear;
  for j := Low(TMateraClientType) to High(TMateraClientType) do
     cbContaCriarTipoCliente.Items.Add(GetEnumName(TypeInfo(TMateraClientType), Integer(j)));
  cbContaCriarTipoCliente.ItemIndex := 1;
  cbContaCriarTipoClienteChange(Nil);

  cbCriarContaTipoConta.Items.Clear;
  for k := Low(TMateraAccountType) to High(TMateraAccountType) do
     cbCriarContaTipoConta.Items.Add(GetEnumName(TypeInfo(TMateraAccountType), Integer(k)));

  edConsultaStart.DateTime := Now;
  edconsultaEnding.DateTime := Now;
end;

procedure TfrPixCDMatera.LigarAlertasdeErrosDeConfiguracao;
begin
  ValidarCertificado;
  ValidarChavePrivada;
end;

procedure TfrPixCDMatera.LimparInterfaceFluxo;
begin
  edFluxoClienteDoc.Text := 'Cliente';
  edFluxoClienteNome.Text := 'Nome Consumidor';
  edFluxoValor.Text := '5,00';
  edFluxotransactionID.Text := EmptyStr;
  imFluxoQRCode.Picture.Clear;
  pnFluxoCopiaECola.Visible := False;
  pnFluxoTransactionID.Visible := False;
  lbSiteEfetuarPagto.Visible := False;
end;

procedure TfrPixCDMatera.AtualizarStatus(aStatusCobranca: TMateraTransactionStatus;
  aStatusDevolucao: TMateraTransactionStatus);

  procedure AtualizarPanelPrincipal(aTexto: String; aCor: TColor);
  begin
    pnFluxoStatus.Color := aCor;
    pnFluxoStatus.Caption := aTexto;
  end;

begin
  if fFluxoDados.EmErro then
  begin
    AtualizarPanelPrincipal('ERRO AO CONSULTAR', clRed);
    AvaliarInterfaceFluxo;
    Exit;
  end;

  fFluxoDados.StatusCobranca := aStatusCobranca;
  fFluxoDados.StatusDevolucao := aStatusDevolucao;
  AvaliarInterfaceFluxo;
         
  case fFluxoDados.StatusDevolucao of
    mtsCreated: AtualizarPanelPrincipal('AGUARDANDO ESTORNO', $001ADAE3);
    mtsApproved: AtualizarPanelPrincipal('ESTORNO APROVADO', $000600EA);
  else  
    case fFluxoDados.StatusCobranca of
      mtsCreated: AtualizarPanelPrincipal('AGUARDANDO PAGAMENTO', $001ADAE3);
      mtsApproved: AtualizarPanelPrincipal('PAGAMENTO FINALIZADO', $0009E31F);
      mtsCanceled, mtsRejected: AtualizarPanelPrincipal('CANCELADO PELO PSP', $000600EA);
    else
      AtualizarPanelPrincipal('VENDENDO', clMenuHighlight);
    end;
  end;
end;

procedure TfrPixCDMatera.ReiniciarFluxo;
begin
  ACBrPSPMatera1.Clear;
  LimparInterfaceFluxo;

  fFluxoDados.Total := 0;
  fFluxoDados.EmErro := False;
  fFluxoDados.QRCode := EmptyStr;
  tmConsultarPagto.Enabled := False;
  fFluxoDados.transactionID := EmptyStr;
  fFluxoDados.StatusCobranca := mtsNone;
  fFluxoDados.StatusDevolucao := mtsNone;

  AtualizarStatus(mtsNone, mtsNone);
end;

procedure TfrPixCDMatera.ValidarChavePrivada;
var
  a, e: String;
begin
  if EstaVazio(edArqChavePrivada.Text) then
    Exit;

  a := AdicionarPathAplicacao(edArqChavePrivada.Text);
  e := 'OK';
  if (a = '') then
    e := ACBrStr('Arquivo não especificado')
  else if (not FileExists(a)) then
    e := ACBrStr('Arquivo não encontrado')
  else
  begin
    try
      ACBrOpenSSLUtils1.LoadPrivateKeyFromFile(a);
    except
      On Ex: Exception do
        e := Ex.Message;
    end;
  end;

  lbErroChavePrivada.Caption := e;
  imErroChavePrivada.Visible := (e <> 'OK');
end;

procedure TfrPixCDMatera.ValidarCertificado;
var
  a, e: String;
begin
  if EstaVazio(edArqCertificado.Text) then
    Exit;

  a := AdicionarPathAplicacao(edArqCertificado.Text);
  e := 'OK';
  if (a = '') then
    e := ACBrStr('Arquivo não especificado')
  else if (not FileExists(a)) then
    e := ACBrStr('Arquivo não encontrado')
  else
  begin
    try
      ACBrOpenSSLUtils1.LoadPEMFromFile(a);
    except
      On Ex: Exception do
        e := Ex.Message;
    end;
  end;

  lbErroCertificado.Caption := e;
  imErroCertificado.Visible := (e <> 'OK');
end;

end.

