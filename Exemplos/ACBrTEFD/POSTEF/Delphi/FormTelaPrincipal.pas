{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
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
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

unit FormTelaPrincipal;

interface

uses
  Classes, SysUtils, syncobjs, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, Spin, Buttons, Grids, Menus, ACBrPOS, ACBrPOSPGWebAPI,
  ACBrPosPrinter, ACBrMail, ACBrConsultaCNPJ, ACBrCEP, ACBrConsts, ACBrIBGE,
  ACBrSAT, synachar, ACBrNFe, ACBrNFeDANFeESCPOS, ACBrDANFCeFortesFr,
  ACBrSATClass, pcnCFe, ACBrSATExtratoESCPOS, ACBrSATExtratoFortesFr,
  ACBrPOSPGWebPrinter, ACBrTEFComum,
  pcnConversao, pcnNFe, ACBrSocket, ACBrDFe, ImgList, ACBrBase, jpeg;

const
  CACBR_URL = 'https://projetoacbr.com.br/tef/';
  CPAG_DINHEIRO = 1;
  CPAG_DEBITO = 2;
  CPAG_CREDITO = 3;

type
  TItemPedido = record
    CodItem: Integer;
    Descricao: String;
    UN: String;
    Qtd: Double;
    PrecoUnit: Currency;
  end;

  TPagtoPedido = record
    FormaPagto: Byte;
    ValorPago: Currency;
  end;

  TPedido = record
    NumPedido: Integer;
    Nome: String;
    Email: String;
    DataHora: TDateTime;
    Items: array of TItemPedido;
    ValorTotal: Currency;
    Pessoas: Integer;
    Pagamentos: array of TPagtoPedido;
    TotalPago: Currency;
  end;

  TPedidos = array of TPedido;

  TAbastecimento = record
    DataHora: TDateTime;
    Sequencia: Integer;
    Bico: Integer;
    Qtd: Double;
    FormaPagto: Byte;
    ValorPago: Currency;
  end;

  TAbastecimentos = array of TAbastecimento;

  TBico = record
    Bomba: Integer;
    Tanque: Integer;
    CodProduto: Integer;
    Descricao: String;
    CodigoANP: Integer;
    CEST: String;
    NCM: String;
    PrecoUnit: Currency;
    Encrerrante: Double;
  end;

  TBicos = array of TBico;

  EFluxoInterrompido = class(Exception);

  { TfrPOSTEFServer }

  TfrPOSTEFServer = class(TForm)
    ACBrCEP1: TACBrCEP;
    ACBrConsultaCNPJ1: TACBrConsultaCNPJ;
    ACBrIBGE1: TACBrIBGE;
    ACBrMail1: TACBrMail;
    ACBrNFe1: TACBrNFe;
    ACBrPOS1: TACBrPOS;
    ACBrSAT1: TACBrSAT;
    btConfiguracao: TBitBtn;
    btEmailTeste: TBitBtn;
    btOperacao: TBitBtn;
    btStatusServico: TBitBtn;
    btStatusOperacionalSAT: TBitBtn;
    btStatusSAT: TBitBtn;
    btVoltar2: TBitBtn;
    btIniciarParaServidor: TBitBtn;
    btLerParametros: TBitBtn;
    btCertInfo: TBitBtn;
    btLimparLog: TBitBtn;
    btVoltar: TBitBtn;
    btSalvarParametros: TBitBtn;
    cbCryptLib: TComboBox;
    cbEmailDefaultCharset: TComboBox;
    cbEmailIdeCharSet: TComboBox;
    cbHttpLib: TComboBox;
    cbIMprimirViaReduzida: TCheckBox;
    cbIniciarEmOperacao: TCheckBox;
    cbSSLLib: TComboBox;
    cbSSLType: TComboBox;
    cbSuportaDesconto: TCheckBox;
    cbSuportaSaque: TCheckBox;
    cbTipoAplicacao: TComboBox;
    cbWebServiceUF: TComboBox;
    cbSATAmbiente: TComboBox;
    cbxEmitCidade: TComboBox;
    cbxEmitUF: TComboBox;
    cbxIndRatISSQN: TComboBox;
    cbXmlSignLib: TComboBox;
    cbUtilizarSaldoTotalVoucher: TCheckBox;
    cbSATModelo: TComboBox;
    cbxRegTribISSQN: TComboBox;
    cbxTipoEmpresa: TComboBox;
    cbEmailDoctoFiscal: TCheckBox;
    ckSATUTF8: TCheckBox;
    chkEmailSSL: TCheckBox;
    chkEmailTLS: TCheckBox;
    ckEmuladorSEFAZ: TCheckBox;
    edAplicacaoNome: TEdit;
    edAplicacaoVersao: TEdit;
    edLog: TEdit;
    edMensagemBoasVindas: TEdit;
    edtSATDLL: TEdit;
    edRazaoSocial: TEdit;
    edRegistro: TEdit;
    edtCertArqPFX: TEdit;
    edtSATCodigoAtivacao: TEdit;
    edtEmailFrom: TEdit;
    edtEmailAssunto: TEdit;
    edtEmailFromName: TEdit;
    edtEmailHost: TEdit;
    edtEmailPassword: TEdit;
    edtEmailTo: TEdit;
    edtEmailUser: TEdit;
    edtCertNumSerie: TEdit;
    edtCertSenha: TEdit;
    edtCertURLPFX: TEdit;
    edtEmitBairro: TEdit;
    edtEmitCEP: TEdit;
    edtEmitCNPJ: TEdit;
    edtEmitComp: TEdit;
    edtEmitFantasia: TEdit;
    edtEmitFone: TEdit;
    edtEmitIE: TEdit;
    edtEmitIM: TEdit;
    edtEmitLogradouro: TEdit;
    edtEmitNumero: TEdit;
    edtEmitRazao: TEdit;
    edtPathSchemas: TEdit;
    edtSATSwHAssinatura: TEdit;
    edtSATSwHCNPJ: TEdit;
    edtTokenID: TEdit;
    edtProxyHost: TEdit;
    gbEmailAutenticacao: TGroupBox;
    gConfSAT: TGroupBox;
    gbSATPagCod: TGroupBox;
    gbNFCeNumeros: TGroupBox;
    imgErrSAT: TImage;
    Label39: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    Label43: TLabel;
    Label44: TLabel;
    Label45: TLabel;
    Label48: TLabel;
    Label49: TLabel;
    Label5: TLabel;
    Label52: TLabel;
    rgTransacaoPendente: TRadioGroup;
    rgDocumentoFiscal: TRadioGroup;
    gEmailMensagem: TGroupBox;
    Image1: TImage;
    Image2: TImage;
    imgErrCEP: TImage;
    imgErrCidade: TImage;
    imgErrCNPJ: TImage;
    imgErrEmail: TImage;
    imgErrFone: TImage;
    imgErrRazaoSocial: TImage;
    imgErrUF: TImage;
    imgErrWebService: TImage;
    imgErrPathSchemas: TImage;
    Label12: TLabel;
    Label13: TLabel;
    Label15: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label46: TLabel;
    Label47: TLabel;
    Label6: TLabel;
    lbl1: TLabel;
    lblDefaultCharset: TLabel;
    lblFrom: TLabel;
    lblFrom1: TLabel;
    lblFrom2: TLabel;
    lblFrom3: TLabel;
    lblFromName: TLabel;
    lblHost: TLabel;
    lblPassword: TLabel;
    lblPort: TLabel;
    lblUser: TLabel;
    gEmailConta: TGroupBox;
    mEmailMensagem: TMemo;
    gConfEmissor: TGroupBox;
    pEmitCodCidade: TPanel;
    pEmitCodUF: TPanel;
    cbTipoAmb: TComboBox;
    sbConsultaCEP: TSpeedButton;
    sbConsultaCNPJ: TSpeedButton;
    sbNomeDLL: TSpeedButton;
    sbVerSenhaCertificado: TSpeedButton;
    sbVerSenhaEmail: TSpeedButton;
    sbVerSenhaProxy: TSpeedButton;
    seEmailPort: TSpinEdit;
    seNFCeLote: TSpinEdit;
    seNFCeNumero: TSpinEdit;
    seSATNumeroCaixa: TSpinEdit;
    seSATPagCod: TSpinEdit;
    seProxyPorta: TSpinEdit;
    edtProxySenha: TEdit;
    edtProxyUser: TEdit;
    edtTokenCSC: TEdit;
    gbCertificado: TGroupBox;
    gbBibliotecas: TGroupBox;
    gbProxy: TGroupBox;
    gbToken: TGroupBox;
    gbWebService: TGroupBox;
    imgErrCertificado: TImage;
    imgErrTokenID: TImage;
    imgErrHttpLib: TImage;
    imgErrCryptLib: TImage;
    imgErrSSLLib: TImage;
    imgErrTokenCSC: TImage;
    imgErrXmlSignLib: TImage;
    Label10: TLabel;
    Label3: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Label4: TLabel;
    Label51: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lCryptLib: TLabel;
    lHttpLib: TLabel;
    lSSLLib: TLabel;
    lSSLLib1: TLabel;
    lTimeOut: TLabel;
    lXmlSign: TLabel;
    OpenDialog1: TOpenDialog;
    GroupBox1: TGroupBox;
    ImageList1: TImageList;
    Label1: TLabel;
    Label11: TLabel;
    Label14: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label7: TLabel;
    lURLTEF: TLabel;
    mLog: TMemo;
    pgConfig: TPageControl;
    pOperacao: TPanel;
    pBotoesOperacao: TPanel;
    pBotoesConfiguracao: TPanel;
    pConfiguracao: TPanel;
    pgPrincipal: TPageControl;
    pLogs: TPanel;
    pTerminais: TPanel;
    SbArqLog: TSpeedButton;
    sbtnCaminhoCert: TSpeedButton;
    sbtnGetCert: TSpeedButton;
    sbtnNumSerie: TSpeedButton;
    seMaxConexoes: TSpinEdit;
    sePortaTCP: TSpinEdit;
    seTimeOut: TSpinEdit;
    seSATVersaoEnt: TEdit;
    sgTerminais: TStringGrid;
    sbTerminarConexao: TSpeedButton;
    Splitter1: TSplitter;
    spPathSchemas: TSpeedButton;
    tiIniciar: TTimer;
    tsMenuPrincipal: TTabSheet;
    tsConfSAT: TTabSheet;
    tsConfEmissor: TTabSheet;
    tsConfEMail: TTabSheet;
    tsConfNFCe: TTabSheet;
    tsConfPosTef: TTabSheet;
    tsConfiguracao: TTabSheet;
    tsOperacao: TTabSheet;
    procedure ACBrCEP1AntesAbrirHTTP(var AURL: String);
    procedure ACBrCEP1BuscaEfetuada(Sender: TObject);
    procedure ACBrConsultaCNPJ1AntesAbrirHTTP(var AURL: String);
    procedure ACBrIBGE1AntesAbrirHTTP(var AURL: String);
    procedure ACBrIBGE1BuscaEfetuada(Sender: TObject);
    procedure ACBrIBGE1GravarCache(ConteudoCache: TStrings; var Tratado: Boolean
      );
    procedure ACBrIBGE1LerCache(ConteudoCache: TStrings; var Tratado: Boolean);
    procedure ACBrMail1AfterMailProcess(Sender: TObject);
    procedure ACBrMail1BeforeMailProcess(Sender: TObject);
    procedure ACBrMail1MailProcess(const AMail: TACBrMail;
      const aStatus: TMailStatus);
    procedure ACBrNFe1GerarLog(const ALogLine: String; var Tratado: Boolean);
    procedure ACBrNFe1StatusChange(Sender: TObject);
    procedure ACBrPOS1AposFinalizarTransacao(const TerminalId: String;
      Transacao: TACBrTEFResp; Status: TACBrPOSPGWebStatusTransacao);
    procedure ACBrPOS1AvaliarTransacaoPendente(const TerminalId: String;
      var Status: TACBrPOSPGWebStatusTransacao; const AuthSyst, VirtMerch,
      AutLocRef, AutExtRef: String);
    procedure ACBrPOS1GravarLog(const ALogLine: String; var Tratado: Boolean);
    procedure ACBrPOS1MudaEstadoTerminal(const TerminalId: String; EstadoAtual,
      EstadoAnterior: TACBrPOSPGWebEstadoTerminal);
    procedure ACBrPOS1NovaConexao(const TerminalId: String; const Model: String;
      const MAC: String; const SerNo: String);
    procedure ACBrSAT1GetcodigoDeAtivacao(var Chave: AnsiString);
    procedure ACBrSAT1GetsignAC(var Chave: AnsiString);
    procedure ACBrSAT1GravarLog(const ALogLine: String; var Tratado: Boolean);
    procedure ACBrSAT1MensagemSEFAZ(ACod: Integer; AMensagem: String);
    procedure btConfiguracaoClick(Sender: TObject);
    procedure btEmailTesteClick(Sender: TObject);
    procedure btOperacaoClick(Sender: TObject);
    procedure btStatusOperacionalSATClick(Sender: TObject);
    procedure btStatusSATClick(Sender: TObject);
    procedure btVoltar2Click(Sender: TObject);
    procedure btIniciarParaServidorClick(Sender: TObject);
    procedure btCertInfoClick(Sender: TObject);
    procedure btVoltarClick(Sender: TObject);
    procedure btLerParametrosClick(Sender: TObject);
    procedure btLimparLogClick(Sender: TObject);
    procedure btSalvarParametrosClick(Sender: TObject);
    procedure btStatusServicoClick(Sender: TObject);
    procedure cbCryptLibChange(Sender: TObject);
    procedure cbHttpLibChange(Sender: TObject);
    procedure cbSSLLibChange(Sender: TObject);
    procedure cbSSLTypeChange(Sender: TObject);
    procedure cbWebServiceUFChange(Sender: TObject);
    procedure cbxEmitCidadeChange(Sender: TObject);
    procedure cbxEmitUFChange(Sender: TObject);
    procedure cbXmlSignLibChange(Sender: TObject);
    procedure ckSATUTF8Change(Sender: TObject);
    procedure edtCertArqPFXChange(Sender: TObject);
    procedure edtCertNumSerieChange(Sender: TObject);
    procedure edtEmailFromChange(Sender: TObject);
    procedure edtEmitCEPChange(Sender: TObject);
    procedure edtEmitCEPExit(Sender: TObject);
    procedure edtEmitCNPJChange(Sender: TObject);
    procedure edtEmitFoneChange(Sender: TObject);
    procedure edtEmitRazaoChange(Sender: TObject);
    procedure edtPathSchemasChange(Sender: TObject);
    procedure edtSATDLLChange(Sender: TObject);
    procedure edtSATSwHCNPJChange(Sender: TObject);
    procedure edtTokenCSCChange(Sender: TObject);
    procedure edtTokenIDChange(Sender: TObject);
    procedure edtOnlyNumberKeyPress(Sender: TObject; var Key: char);
    procedure edtCertURLPFXChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lURLTEFClick(Sender: TObject);
    procedure rgDocumentoFiscalClick(Sender: TObject);
    procedure SbArqLogClick(Sender: TObject);
    procedure sbConsultaCEPClick(Sender: TObject);
    procedure sbConsultaCNPJClick(Sender: TObject);
    procedure sbNomeDLLClick(Sender: TObject);
    procedure sbTerminarConexaoClick(Sender: TObject);
    procedure sbtnCaminhoCertClick(Sender: TObject);
    procedure sbtnGetCertClick(Sender: TObject);
    procedure sbtnNumSerieClick(Sender: TObject);
    procedure sbVerSenhaCertificadoClick(Sender: TObject);
    procedure sbVerSenhaEmailClick(Sender: TObject);
    procedure sbVerSenhaProxyClick(Sender: TObject);
    procedure seSATPagCodChange(Sender: TObject);
    procedure spPathSchemasClick(Sender: TObject);
    procedure tiIniciarTimer(Sender: TObject);
  private
    fPedidos: TPedidos;
    fAbastecimentos: TAbastecimentos;
    fBicos: TBicos;
    fcUF: Integer;
    fcMunList: TStringList;
    fcsDocFiscal: TCriticalSection;
    fcsMailBuild: TCriticalSection;

    procedure AtualizarSSLLibsCombo;
    function GetNomeArquivoConfiguracao: String;
    procedure TratarException(Sender : TObject; E : Exception);
    procedure Minimizar(Sender: TObject);
    procedure AdicionarLinhaLog(AMensagem: String);
    function EstadoTerminal(AEstado: TACBrPOSPGWebEstadoTerminal): String;
    procedure ValidarConfigCertificado;
    procedure ValidarConfigWebService;
    procedure ValidarConfigEmail;
    procedure ValidarConfigSAT;

    procedure LigarAlertasdeErrosDeConfiguracao;
    procedure ConfigurarACBrNFe;
    procedure ConfigurarACBrPOS;
    procedure ConfigurarACBrMail;
    procedure ConfigurarACBrSAT;

    function DeveFazerEmissaoDeNFCe: Boolean;
    function DeveFazerEmissaoDeSAT: Boolean;
    function DeveFazerEmissaoDeDocumentoFiscal: Boolean;
    function DeveFazerEnvioDeEmail: Boolean;

    procedure DeduzirCredenciadoraNFCe(APag: TpagCollectionItem; ATEFResp: TACBrTEFResp);
    procedure DeduzirBandeiraNFCe(APag: TpagCollectionItem; ATEFResp: TACBrTEFResp);
    procedure DeduzirCredenciadoraSAT(APag: TMPCollectionItem; ATEFResp: TACBrTEFResp);
  protected
    procedure IrParaMenuPrincipal;
    procedure IrParaOperacaoPOS;
    procedure IrParaConfiguracao;

    procedure IniciarServidorPOS;
    procedure PararServidorPOS;

    procedure VerificarErrosConfiguracaoPOS;
    procedure VerificarErrosConfiguracaoNFCe;
    procedure VerificarErrosConfiguracaoSAT;
    procedure VerificarErrosConfiguracaoEMail;

    procedure CarregarListaDeCidades(cUF: Integer);

    procedure ExecutarFluxoPapaFila(const TerminalId: String);
    procedure ExibirMenuPedidos(const TerminalId: String);

    procedure ExecutarFluxoFechamentoMesa(const TerminalId: String);
    procedure ExibirMenuMesasEmAberto(const TerminalId: String);
    procedure PerguntarNumeroPessoas(const TerminalId: String; IndicePedido: Integer);

    procedure ExecutarFluxoFechamentoBomba(const TerminalId: String);
    procedure ExibirMenuAbastecimentos(const TerminalId: String);
    function ExibirAbastecimento(const TerminalId: String; IndiceAbastec: Integer): Boolean;
    procedure EfetuarPagamentoAbastec(const TerminalId: String; IndiceAbastec: Integer);
    procedure EfetuarPagamentoAbastecCartao(const TerminalId: String; IndiceAbastec: Integer);
    function EfetuarPagamentoAbastecDinheiro(const TerminalId: String; IndiceAbastec: Integer): Boolean;
    procedure EfetuarDocumentoFiscalAbastec(const TerminalId: String; IndiceAbastec: Integer);
    procedure EfetuarNFCeAbastec(const TerminalId: String; IndiceAbastec: Integer);
    procedure EfetuarSATAbastec(const TerminalId: String; IndiceAbastec: Integer);
    procedure GerarNFCeAbastec(const TerminalId: String; AACBrNFe: TACBrNFe; IndiceAbastec: Integer);
    procedure GerarSATAbastec(const TerminalId: String; IndiceAbastec: Integer);


    procedure ExecutarFluxoHomologacaoPayGo(const TerminalId: String);
    procedure ExibirTestesVendaHomologacao(const TerminalId: String);

    procedure ExecutarReimpressao(const TerminalId: String);
    procedure EfetuarAdministrativo(const TerminalId: String);
    procedure EfetuarCancelamento(const TerminalId: String);

    procedure EfetuarMultiplosPagamento(const TerminalId: String; IndicePedido: Integer);
    procedure EfetuarPagamento(const TerminalId: String; IndicePedido: Integer);
    procedure EfetuarPagamentoCartao(const TerminalId: String;
      IndicePedido: Integer; Valor: Double);
    function EfetuarPagamentoDinheiro(const TerminalId: String;
      IndicePedido: Integer; ValorMinimo: Double): Boolean;

    procedure IncluirPagamentoPedido(IndicePedido: Integer; TipoPagamento: Byte; ValorPago: Double);
    procedure IncluirPagamentoPedidoDinheiro(const TerminalId: String;
      IndicePedido: Integer; ValorPago: Double);

    procedure ImprimirPedido(const TerminalId: String; IndicePedido: Integer);

    procedure EfetuarDocumentoFiscal(const TerminalId: String; IndicePedido: Integer);
    function ACBrPosPrinterFactory: TACBrPosPrinter;
    function ACBrPOSPGWebPrinterFactory(const TerminalId: String; AACBrPosPrinter: TACBrPosPrinter; AACBrPOS: TACBrPOS): TACBrPOSPGWebPrinter;

    procedure EfetuarNFCe(const TerminalId: String; IndicePedido: Integer);
    procedure GerarNFCe(const TerminalId: String; AACBrNFe: TACBrNFe; IndicePedido: Integer);
    function ObterProximaNFCe: Integer;
    procedure TransmitirNFCe(AACBrNFe: TACBrNFe; const TerminalId: String);
    procedure ImprimirNFCe(AACBrNFe: TACBrNFe; const TerminalId: String);
    procedure EnviarEmailNFCe(AACBrNFe: TACBrNFe; const TerminalId, AEmail: String);

    procedure EfetuarSAT(const TerminalId: String; IndicePedido: Integer);
    procedure TravarSAT(const TerminalId: String);
    procedure GerarSAT(const TerminalId: String; IndicePedido: Integer);
    procedure TransmitirSAT(const TerminalId: String);
    procedure LiberarSAT;
    procedure ImprimirSAT(AACBrSAT: TACBrSAT; const TerminalId: String);
    procedure EnviarEmailSAT(AACBrSAT: TACBrSAT; const TerminalId, AEmail: String);

    procedure IncluirPedidosSimuladosLoja;
    procedure IncluirPedidosSimuladosRestaurante;
    procedure IncluirAbastecimentosSimulados;

  public
    property NomeArquivoConfiguracao: String read GetNomeArquivoConfiguracao;

    procedure LerConfiguracao;
    procedure GravarConfiguracao;
    procedure AplicarConfiguracao;
    procedure GravarNumeracaoNFCe(const TerminalId: String; Lote, Numero: Integer);
  end;

var
  frPOSTEFServer: TfrPOSTEFServer;

implementation

uses
  IniFiles, math, strutils, dateutils, TypInfo, FileCtrl,
  FormConsultaCNPJ, FormSelecionarCertificado,
  ACBrUtil, ACBrValidador, ACBrDFeSSL, ACBrDFeUtil, ACBrTEFPayGoComum,
  ACBrSATExtratoClass,
  pcnConversaoNFe,
  blcksock, synacode;

{$R *.dfm}

{ TfrPOSTEFServer }

procedure TfrPOSTEFServer.FormCreate(Sender: TObject);
var
  K: TpcnRegTribISSQN;
  L: TpcnindRatISSQN;
  T: TSSLLib;
  U: TSSLCryptLib;
  V: TSSLHttpLib;
  X: TSSLXmlSignLib;
  Y: TSSLType;
  E: TMimeChar;
  S: TACBrSATModelo;
  A: TpcnTipoAmbiente;
begin
  fcUF := 0;
  fcMunList := TStringList.Create;
  fcsDocFiscal := TCriticalSection.Create;
  fcsMailBuild := TCriticalSection.Create;

  tsMenuPrincipal.TabVisible := False;
  tsConfiguracao.TabVisible := False;
  tsOperacao.TabVisible := False;
  
  pgPrincipal.ActivePageIndex := 0;
  pgConfig.ActivePageIndex := 0;

  cbxRegTribISSQN.Items.Clear ;
  For K := Low(TpcnRegTribISSQN) to High(TpcnRegTribISSQN) do
     cbxRegTribISSQN.Items.Add( GetEnumName(TypeInfo(TpcnRegTribISSQN), integer(K) ));

  cbxIndRatISSQN.Items.Clear ;
  For L := Low(TpcnindRatISSQN) to High(TpcnindRatISSQN) do
     cbxIndRatISSQN.Items.Add( GetEnumName(TypeInfo(TpcnindRatISSQN), integer(L) ));

  cbSSLLib.Items.Clear;
  for T := Low(TSSLLib) to High(TSSLLib) do
    cbSSLLib.Items.Add( GetEnumName(TypeInfo(TSSLLib), integer(T) ) );
  cbSSLLib.ItemIndex := 0;

  cbCryptLib.Items.Clear;
  for U := Low(TSSLCryptLib) to High(TSSLCryptLib) do
    cbCryptLib.Items.Add( GetEnumName(TypeInfo(TSSLCryptLib), integer(U) ) );
  cbCryptLib.ItemIndex := 0;

  cbHttpLib.Items.Clear;
  for V := Low(TSSLHttpLib) to High(TSSLHttpLib) do
    cbHttpLib.Items.Add( GetEnumName(TypeInfo(TSSLHttpLib), integer(V) ) );
  cbHttpLib.ItemIndex := 0;

  cbXmlSignLib.Items.Clear;
  for X := Low(TSSLXmlSignLib) to High(TSSLXmlSignLib) do
    cbXmlSignLib.Items.Add( GetEnumName(TypeInfo(TSSLXmlSignLib), integer(X) ) );
  cbXmlSignLib.ItemIndex := 0;

  cbSSLType.Items.Clear;
  for Y := Low(TSSLType) to High(TSSLType) do
    cbSSLType.Items.Add( GetEnumName(TypeInfo(TSSLType), integer(Y) ) );
  cbSSLType.ItemIndex := 0;

  cbEmailDefaultCharset.Items.Clear;
  for E := Low(TMailCharset) to High(TMailCharset) do
    cbEmailDefaultCharset.Items.Add(GetEnumName(TypeInfo(TMailCharset), integer(E)));
  cbEmailDefaultCharset.ItemIndex := 0;
  cbEmailIdeCharSet.Items.Assign(cbEmailDefaultCharset.Items);
  cbEmailIdeCharSet.ItemIndex := 0;

  cbSATModelo.Items.Clear ;
  For S := Low(TACBrSATModelo) to High(TACBrSATModelo) do
     cbSATModelo.Items.Add( GetEnumName(TypeInfo(TACBrSATModelo), integer(S) ) ) ;

  cbSATAmbiente.Items.Clear ;
  For A := Low(TpcnTipoAmbiente) to High(TpcnTipoAmbiente) do
     cbSATAmbiente.Items.Add( GetEnumName(TypeInfo(TpcnTipoAmbiente), integer(A) ) ) ;

  LerConfiguracao;
  Application.OnException := TratarException;
  Application.OnMinimize := Minimizar;

  ImageList1.GetBitmap(0, btConfiguracao.Glyph);
  ImageList1.GetBitmap(2, btSalvarParametros.Glyph);
  ImageList1.GetBitmap(3, btVoltar.Glyph);
  ImageList1.GetBitmap(3, btVoltar2.Glyph);
  ImageList1.GetBitmap(5, sbTerminarConexao.Glyph);
  ImageList1.GetBitmap(6, btLimparLog.Glyph);
  ImageList1.GetBitmap(7, sbConsultaCEP.Glyph);
  ImageList1.GetBitmap(7, sbConsultaCNPJ.Glyph);
  ImageList1.GetBitmap(8, btIniciarParaServidor.Glyph);
  ImageList1.GetBitmap(11, btEmailTeste.Glyph);
  ImageList1.GetBitmap(13, btOperacao.Glyph);
  ImageList1.GetBitmap(15, btStatusOperacionalSAT.Glyph);
  ImageList1.GetBitmap(17, sbtnCaminhoCert.Glyph);
  ImageList1.GetBitmap(17, spPathSchemas.Glyph);
  ImageList1.GetBitmap(17, sbNomeDLL.Glyph);
  ImageList1.GetBitmap(18, sbtnGetCert.Glyph);
  ImageList1.GetBitmap(19, sbtnNumSerie.Glyph);
  ImageList1.GetBitmap(20, btCertInfo.Glyph);
  ImageList1.GetBitmap(20, btStatusServico.Glyph);
  ImageList1.GetBitmap(20, btStatusSAT.Glyph);
  ImageList1.GetBitmap(22, btLerParametros.Glyph);
  ImageList1.GetBitmap(23, sbVerSenhaCertificado.Glyph);
  ImageList1.GetBitmap(23, sbVerSenhaEmail.Glyph);
  ImageList1.GetBitmap(23, sbVerSenhaProxy.Glyph);

  with sgTerminais do
  begin
    Cells[0, 0] := 'Tertminal ID';
    Cells[1, 0] := 'Status';
  end;

  ImageList1.GetBitmap(16, imgErrCNPJ.Picture.Bitmap);
  ImageList1.GetBitmap(16, imgErrRazaoSocial.Picture.Bitmap);
  ImageList1.GetBitmap(16, imgErrCEP.Picture.Bitmap);
  ImageList1.GetBitmap(16, imgErrFone.Picture.Bitmap);
  ImageList1.GetBitmap(16, imgErrUF.Picture.Bitmap);
  ImageList1.GetBitmap(16, imgErrCidade.Picture.Bitmap);
  ImageList1.GetBitmap(16, imgErrCertificado.Picture.Bitmap);
  ImageList1.GetBitmap(16, imgErrSSLLib.Picture.Bitmap);
  ImageList1.GetBitmap(16, imgErrCryptLib.Picture.Bitmap);
  ImageList1.GetBitmap(16, imgErrHttpLib.Picture.Bitmap);
  ImageList1.GetBitmap(16, imgErrXmlSignLib.Picture.Bitmap);
  ImageList1.GetBitmap(16, imgErrWebService.Picture.Bitmap);
  ImageList1.GetBitmap(16, imgErrTokenID.Picture.Bitmap);
  ImageList1.GetBitmap(16, imgErrTokenCSC.Picture.Bitmap);
  ImageList1.GetBitmap(16, imgErrPathSchemas.Picture.Bitmap);
  ImageList1.GetBitmap(16, imgErrEmail.Picture.Bitmap);
  ImageList1.GetBitmap(16, imgErrSAT.Picture.Bitmap);

  tiIniciar.Enabled := cbIniciarEmOperacao.Checked;

  ACBrSAT1.OnGetcodigoDeAtivacao := ACBrSAT1GetcodigoDeAtivacao;
  ACBrSAT1.OnGetsignAC := ACBrSAT1GetsignAC;
end;

procedure TfrPOSTEFServer.FormDestroy(Sender: TObject);
begin
  fcMunList.Free;
  fcsDocFiscal.Free;
  fcsMailBuild.Free;
end;

procedure TfrPOSTEFServer.btLimparLogClick(Sender: TObject);
begin
  mLog.Lines.Clear;
end;

procedure TfrPOSTEFServer.btSalvarParametrosClick(Sender: TObject);
begin
  GravarConfiguracao;
end;

procedure TfrPOSTEFServer.btStatusServicoClick(Sender: TObject);
var
  SL: TStringList;
begin
  ConfigurarACBrNFe;
  ACBrNFe1.WebServices.StatusServico.Executar;
  SL := TStringList.Create;
  try
    SL.Add('versao: ' + ACBrNFe1.WebServices.StatusServico.versao);
    SL.Add('tpAmb: ' + TpAmbToStr(ACBrNFe1.WebServices.StatusServico.tpAmb));
    SL.Add('verAplic: ' + ACBrNFe1.WebServices.StatusServico.verAplic);
    SL.Add('cStat: ' + IntToStr(ACBrNFe1.WebServices.StatusServico.cStat));
    SL.Add('xMotivo: ' + ACBrNFe1.WebServices.StatusServico.xMotivo);
    SL.Add('cUF: ' + IntToStr(ACBrNFe1.WebServices.StatusServico.cUF));
    SL.Add('dhRecbto: ' + DateTimeToStr(ACBrNFe1.WebServices.StatusServico.dhRecbto));
    SL.Add('tMed: ' + IntToStr(ACBrNFe1.WebServices.StatusServico.TMed));
    SL.Add('dhRetorno: ' + DateTimeToStr(ACBrNFe1.WebServices.StatusServico.dhRetorno));
    SL.Add('xObs: ' + ACBrNFe1.WebServices.StatusServico.xObs);

    MessageDlg( SL.Text, mtInformation, [mbOK], 0);
  finally
    SL.Free;
  end;
end;

procedure TfrPOSTEFServer.cbCryptLibChange(Sender: TObject);
begin
  try
    if cbCryptLib.ItemIndex <> -1 then
      ACBrNFe1.Configuracoes.Geral.SSLCryptLib := TSSLCryptLib(cbCryptLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrPOSTEFServer.cbHttpLibChange(Sender: TObject);
begin
  try
    if cbHttpLib.ItemIndex <> -1 then
      ACBrNFe1.Configuracoes.Geral.SSLHttpLib := TSSLHttpLib(cbHttpLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrPOSTEFServer.cbSSLLibChange(Sender: TObject);
begin
  try
    if cbSSLLib.ItemIndex <> -1 then
      ACBrNFe1.Configuracoes.Geral.SSLLib := TSSLLib(cbSSLLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrPOSTEFServer.cbSSLTypeChange(Sender: TObject);
begin
  if cbSSLType.ItemIndex <> -1 then
     ACBrNFe1.SSL.SSLType := TSSLType(cbSSLType.ItemIndex);
end;

procedure TfrPOSTEFServer.cbWebServiceUFChange(Sender: TObject);
begin
  ValidarConfigWebService;
end;

procedure TfrPOSTEFServer.cbxEmitCidadeChange(Sender: TObject);
var
  Ok: Boolean;
begin
  Ok := (cbxEmitCidade.ItemIndex >= 0);
  imgErrCidade.Visible := not Ok;
  if Ok then
    pEmitCodCidade.Caption := FcMunList[cbxEmitCidade.ItemIndex];
end;

procedure TfrPOSTEFServer.cbxEmitUFChange(Sender: TObject);
var
  cUF: Integer;
  Ok: Boolean;
begin
  Ok := (cbxEmitUF.ItemIndex >= 0);
  imgErrUF.Visible := not Ok;

  if Ok then
  begin
    cUF := UFtoCUF(cbxEmitUF.Text);
    if (cUF <> FcUF) then
    begin
      pEmitCodUF.Caption := IntToStrZero(cUF, 2);
      CarregarListaDeCidades(cUF);
    end;
  end;
end;

procedure TfrPOSTEFServer.cbXmlSignLibChange(Sender: TObject);
begin
  try
    if cbXmlSignLib.ItemIndex <> -1 then
      ACBrNFe1.Configuracoes.Geral.SSLXmlSignLib := TSSLXmlSignLib(cbXmlSignLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrPOSTEFServer.ckSATUTF8Change(Sender: TObject);
begin
  ACBrSAT1.Config.EhUTF8 := ckSATUTF8.Checked;
  seSATPagCod.Value := ACBrSAT1.Config.PaginaDeCodigo;
end;

procedure TfrPOSTEFServer.edtCertURLPFXChange(Sender: TObject);
begin
if (edtCertURLPFX.Text <> '') then
  begin
    if (edtCertNumSerie.Text <> '') then
      edtCertNumSerie.Text := '';

    if (edtCertArqPFX.Text = '') then
      edtCertArqPFX.Text := 'CertA1.pfx';
  end;

  ValidarConfigCertificado;
end;

procedure TfrPOSTEFServer.edtCertArqPFXChange(Sender: TObject);
begin
  if (edtCertArqPFX.Text <> '') then
  begin
    if (edtCertNumSerie.Text <> '') then
      edtCertNumSerie.Text := '';
  end;

  ValidarConfigCertificado;
end;

procedure TfrPOSTEFServer.edtCertNumSerieChange(Sender: TObject);
begin
  if (edtCertNumSerie.Text <> '') then
  begin
    if (edtCertURLPFX.Text <> '') then
      edtCertURLPFX.Text := '';

    if (edtCertArqPFX.Text <> '') then
      edtCertArqPFX.Text := '';
  end;

  ValidarConfigCertificado;
end;

procedure TfrPOSTEFServer.edtEmailFromChange(Sender: TObject);
begin
  ValidarConfigEmail;
end;

procedure TfrPOSTEFServer.edtEmitCEPChange(Sender: TObject);
begin
  if (Length(edtEmitCEP.Text) > 5) then
  begin
    edtEmitCEP.Text := FormatarMascaraDinamica(OnlyNumber(edtEmitCEP.Text), '*****-***');
    edtEmitCEP.SelStart := Length(edtEmitCEP.Text);
  end;

  imgErrCEP.Visible := (Length(edtEmitCEP.Text) < 9);
  sbConsultaCEP.Visible := not imgErrCEP.Visible;
end;

procedure TfrPOSTEFServer.edtEmitCEPExit(Sender: TObject);
begin
  if (not imgErrCEP.Visible) and (edtEmitLogradouro.Text = '') then
    sbConsultaCEP.Click;
end;

procedure TfrPOSTEFServer.edtEmitCNPJChange(Sender: TObject);
begin
  if (Length(edtEmitCNPJ.Text) > 2) then
  begin
    edtEmitCNPJ.Text := ACBrValidador.FormatarMascaraDinamica( OnlyNumber(edtEmitCNPJ.Text), '**.***.***/****-**');
    edtEmitCNPJ.SelStart := Length(edtEmitCNPJ.Text);
  end;

  imgErrCNPJ.Visible := (Length(edtEmitCNPJ.Text) < 18) or
                        (ACBrValidador.ValidarCNPJ(edtEmitCNPJ.Text) <> '');
  sbConsultaCNPJ.Visible := not imgErrCNPJ.Visible;
end;

procedure TfrPOSTEFServer.edtEmitFoneChange(Sender: TObject);
var
  AStr, Mascara: String;
begin
  if (Length(edtEmitFone.Text) > 2) then
  begin
    AStr := OnlyNumber(edtEmitFone.Text);
    Mascara := '(**)****-****';
    case Length(AStr) of
      10:
      begin
        if (copy(AStr,1,1) = '0') and (copy(AStr,3,2) = '00') then  // 0300,0500,0800,0900
          Mascara := '****-***-****';
      end;
      11: Mascara := '(**)*****-****';
      12: Mascara := '+**(**)****-****';
    end;

    edtEmitFone.Text := ACBrValidador.FormatarMascaraDinamica(AStr, Mascara);
    edtEmitFone.SelStart := Length(edtEmitFone.Text);
  end;

  imgErrFone.Visible := (Length(OnlyNumber(edtEmitFone.Text)) < 10);
end;

procedure TfrPOSTEFServer.edtEmitRazaoChange(Sender: TObject);
begin
  imgErrRazaoSocial.Visible := (Length(edtEmitRazao.Text) < 4);
end;

procedure TfrPOSTEFServer.edtPathSchemasChange(Sender: TObject);
begin
  ValidarConfigWebService;
end;

procedure TfrPOSTEFServer.edtSATDLLChange(Sender: TObject);
begin
  ValidarConfigSAT;
end;

procedure TfrPOSTEFServer.edtSATSwHCNPJChange(Sender: TObject);
begin
  if (Length(edtSATSwHCNPJ.Text) > 2) then
  begin
    edtSATSwHCNPJ.Text := ACBrValidador.FormatarMascaraDinamica( OnlyNumber(edtSATSwHCNPJ.Text), '**.***.***/****-**');
    edtSATSwHCNPJ.SelStart := Length(edtSATSwHCNPJ.Text);
  end;

  edtSATDLLChange(Sender);
end;

procedure TfrPOSTEFServer.edtTokenCSCChange(Sender: TObject);
begin
  imgErrTokenCSC.Visible := (edtTokenCSC.Text = '');
end;

procedure TfrPOSTEFServer.edtTokenIDChange(Sender: TObject);
begin
  imgErrTokenID.Visible := (edtTokenID.Text = '');
end;

procedure TfrPOSTEFServer.edtOnlyNumberKeyPress(Sender: TObject; var Key: char);
begin
  if not CharInSet( Key, [#8,'0'..'9'] ) then
    Key := #0;
end;

procedure TfrPOSTEFServer.btVoltarClick(Sender: TObject);
begin
  GravarConfiguracao;
  IrParaMenuPrincipal;
end;

procedure TfrPOSTEFServer.btVoltar2Click(Sender: TObject);
begin
  PararServidorPOS;
  IrParaMenuPrincipal;
end;

procedure TfrPOSTEFServer.ACBrPOS1GravarLog(const ALogLine: String;
  var Tratado: Boolean);
begin
  AdicionarLinhaLog(ALogLine);
  Tratado := False;
end;

procedure TfrPOSTEFServer.ACBrCEP1AntesAbrirHTTP(var AURL: String);
begin
  AdicionarLinhaLog('ACBrCEP: Efetuando consulta em: '+AURL) ;
end;

procedure TfrPOSTEFServer.ACBrCEP1BuscaEfetuada(Sender: TObject);
begin
  AdicionarLinhaLog( Format('ACBrCEP: %d Endereço(s) encontrado(s)', [ACBrCEP1.Enderecos.Count]) );
end;

procedure TfrPOSTEFServer.ACBrConsultaCNPJ1AntesAbrirHTTP(var AURL: String);
begin
  AdicionarLinhaLog('ACBrConsultaCNPJ: Efetuando consulta em: '+AURL) ;
end;

procedure TfrPOSTEFServer.ACBrIBGE1AntesAbrirHTTP(var AURL: String);
begin
  AdicionarLinhaLog('ACBrIBGE: Efetuando consulta em: '+AURL) ;
end;

procedure TfrPOSTEFServer.ACBrIBGE1BuscaEfetuada(Sender: TObject);
begin
  AdicionarLinhaLog( Format('ACBrIBGE: %d Cidade(s) encontrada(s)', [ACBrIBGE1.Cidades.Count]) );
end;

procedure TfrPOSTEFServer.ACBrIBGE1GravarCache(ConteudoCache: TStrings;
  var Tratado: Boolean);
begin
  AdicionarLinhaLog('ACBrIBGE: Gravando Cache');
end;

procedure TfrPOSTEFServer.ACBrIBGE1LerCache(ConteudoCache: TStrings;
  var Tratado: Boolean);
begin
  AdicionarLinhaLog('ACBrIBGE: Lendo Cache');
end;

procedure TfrPOSTEFServer.ACBrMail1AfterMailProcess(Sender: TObject);
begin
  AdicionarLinhaLog('ACBrMail - Enviado: ' + TACBrMail(Sender).Subject);
end;

procedure TfrPOSTEFServer.ACBrMail1BeforeMailProcess(Sender: TObject);
begin
  AdicionarLinhaLog('ACBrMail - Inicio: ' + TACBrMail(Sender).Subject);
end;

procedure TfrPOSTEFServer.ACBrMail1MailProcess(const AMail: TACBrMail;
  const aStatus: TMailStatus);
begin
  AdicionarLinhaLog( 'ACBrMail - Status: ' + GetEnumName(TypeInfo(TMailStatus), integer(aStatus)) );
end;

procedure TfrPOSTEFServer.ACBrNFe1GerarLog(const ALogLine: String;
  var Tratado: Boolean);
begin
  AdicionarLinhaLog('ACBrNFe - '+ALogLine);
end;

procedure TfrPOSTEFServer.ACBrNFe1StatusChange(Sender: TObject);
begin
  AdicionarLinhaLog( 'ACBrNFe - Status: ' +
                     GetEnumName(TypeInfo(TStatusACBrNFe), integer(ACBrNFe1.Status)) );
end;

procedure TfrPOSTEFServer.ACBrPOS1AposFinalizarTransacao(
  const TerminalId: String; Transacao: TACBrTEFResp;
  Status: TACBrPOSPGWebStatusTransacao);
begin
  AdicionarLinhaLog( sLineBreak +
                     '-- Fim da Transação --'+ sLineBreak +
                     'Terminal: '+TerminalId + sLineBreak +
                     'Rede: '+Transacao.Rede + sLineBreak +
                     'NSU: '+Transacao.NSU + sLineBreak +
                     'Crédito: '+IfThen(Transacao.Credito, 'Sim', 'Nao') + sLineBreak +
                     'Débito: '+IfThen(Transacao.Debito, 'Sim', 'Nao') + sLineBreak +
                     'Bandeira: '+Transacao.CodigoBandeiraPadrao + sLineBreak +
                     'Status: '+IntToStr(SmallInt(Status)) + sLineBreak );
end;

procedure TfrPOSTEFServer.ACBrPOS1AvaliarTransacaoPendente(
  const TerminalId: String; var Status: TACBrPOSPGWebStatusTransacao;
  const AuthSyst, VirtMerch, AutLocRef, AutExtRef: String);
begin
  if (rgTransacaoPendente.ItemIndex = 1) then
    Status := cnfErroDiverso
  else
    Status := cnfSucesso;

  AdicionarLinhaLog( 'Transação Pendente:'+
                     ' Rede: '+AuthSyst +
                     ' NSU: '+AutExtRef +
                     ' Status: '+IntToStr(SmallInt(Status)) );
end;

procedure TfrPOSTEFServer.ACBrPOS1MudaEstadoTerminal(const TerminalId: String;
  EstadoAtual, EstadoAnterior: TACBrPOSPGWebEstadoTerminal);
var
  Linha, i: Integer;
  EstadoStr: String;
begin
  EstadoStr := EstadoTerminal(EstadoAtual);
  AdicionarLinhaLog('- MudaEstadoTerminal: '+TerminalId+', '+EstadoStr);
  Linha := -1;
  For i := 1 to sgTerminais.RowCount-1 do
  begin
    if sgTerminais.Cells[0,i] = TerminalId then
    begin
      Linha := i;
      Break;
    end;
  end;

  if (Linha < 0) then
  begin
    Linha := sgTerminais.RowCount-1;
    if (sgTerminais.Rows[Linha][0] <> '') then
    begin
      Linha := Linha + 1;
      sgTerminais.RowCount := sgTerminais.RowCount + 1
    end;
  end;

  sgTerminais.Rows[Linha][0] := TerminalId;
  sgTerminais.Rows[Linha][1] := EstadoStr;
end;

procedure TfrPOSTEFServer.ACBrPOS1NovaConexao(const TerminalId: String;
  const Model: String; const MAC: String; const SerNo: String);
begin
  try
    case cbTipoAplicacao.ItemIndex of
      1: ExecutarFluxoFechamentoMesa(TerminalId);
      2: ExecutarFluxoFechamentoBomba(TerminalId);
      3: ExecutarFluxoHomologacaoPayGo(TerminalId);
    else
      ExecutarFluxoPapaFila(TerminalId)
    end;
  except
    On E: EFluxoInterrompido do
    begin
      ACBrPOS1.ExibirMensagem(TerminalId,
         PadCenter('OPERACAO', CACBrPOSPGWebColunasDisplay)+ CR +
         PadCenter('CANCELADA', CACBrPOSPGWebColunasDisplay) );
    end;

    On E: EACBrPOSPGWeb do
      ACBrPOS1.ExibirMensagem(TerminalId, E.Message);

    On E: Exception do
      raise;
  end;
end;

procedure TfrPOSTEFServer.ACBrSAT1GetcodigoDeAtivacao(var Chave: AnsiString);
begin
  Chave := AnsiString( edtSATCodigoAtivacao.Text );
end;

procedure TfrPOSTEFServer.ACBrSAT1GetsignAC(var Chave: AnsiString);
begin
  Chave := AnsiString( edtSATSwHAssinatura.Text );
end;

procedure TfrPOSTEFServer.ACBrSAT1GravarLog(const ALogLine: String;
  var Tratado: Boolean);
begin
   AdicionarLinhaLog('ACBrSAT - '+ALogLine);
end;

procedure TfrPOSTEFServer.ACBrSAT1MensagemSEFAZ(ACod: Integer; AMensagem: String);
begin
  AdicionarLinhaLog('ACBrSAT - Mensagem do SEFAZ: ' + IntToStr(ACod)+'-'+AMensagem);
end;

procedure TfrPOSTEFServer.btConfiguracaoClick(Sender: TObject);
begin
  IrParaConfiguracao;
end;

procedure TfrPOSTEFServer.btEmailTesteClick(Sender: TObject);
begin
  ConfigurarACBrMail;

  ACBrMail1.Clear;
  ACBrMail1.IsHTML := False;
  ACBrMail1.Subject := 'Teste de Email';
  ACBrMail1.AltBody.Text := 'Teste de Email' + sLineBreak + CACBR_URL;
  ACBrMail1.AddAddress(edtEmailTo.Text);
  ACBrMail1.Send(False);

  MessageDlg( 'Email enviado com sucesso', mtInformation, [mbOK], 0);
end;

procedure TfrPOSTEFServer.btOperacaoClick(Sender: TObject);
begin
  IrParaOperacaoPOS;
end;

procedure TfrPOSTEFServer.btStatusOperacionalSATClick(Sender: TObject);
var
  SL: TStringList;
begin
  ConfigurarACBrSAT;
  ACBrSAT1.Inicializar;
  ACBrSAT1.ConsultarStatusOperacional;

  if ACBrSAT1.Resposta.codigoDeRetorno = 10000 then
  begin
    SL := TStringList.Create;
    try
      with ACBrSAT1.Status do
      begin
        SL.Add('NSERIE.........: '+NSERIE);
        SL.Add('LAN_MAC........: '+LAN_MAC);
        SL.Add('STATUS_LAN.....: '+StatusLanToStr(STATUS_LAN));
        SL.Add('NIVEL_BATERIA..: '+NivelBateriaToStr(NIVEL_BATERIA));
        SL.Add('MT_TOTAL.......: '+MT_TOTAL);
        SL.Add('MT_USADA.......: '+MT_USADA);
        SL.Add('DH_ATUAL.......: '+DateTimeToStr(DH_ATUAL));
        SL.Add('VER_SB.........: '+VER_SB);
        SL.Add('VER_LAYOUT.....: '+VER_LAYOUT);
        SL.Add('ULTIMO_CFe.....: '+ULTIMO_CFe);
        SL.Add('LISTA_INICIAL..: '+LISTA_INICIAL);
        SL.Add('LISTA_FINAL....: '+LISTA_FINAL);
        SL.Add('DH_CFe.........: '+DateTimeToStr(DH_CFe));
        SL.Add('DH_ULTIMA......: '+DateTimeToStr(DH_CFe));
        SL.Add('CERT_EMISSAO...: '+DateToStr(CERT_EMISSAO));
        SL.Add('CERT_VENCIMENTO: '+DateToStr(CERT_VENCIMENTO));
        SL.Add('ESTADO_OPERACAO: '+EstadoOperacaoToStr(ESTADO_OPERACAO));
      end;

      MessageDlg( SL.Text, mtInformation, [mbOK], 0);
    finally
      SL.Free;
    end;
  end
  else
    MessageDlg( ACBrSAT1.Resposta.RetornoStr, mtInformation, [mbOK], 0);
end;

procedure TfrPOSTEFServer.btStatusSATClick(Sender: TObject);
var
  Resp: String;
begin
  ConfigurarACBrSAT;
  ACBrSAT1.Inicializar;
  ACBrSAT1.ConsultarSAT;
  Resp := ACBrSAT1.Resposta.mensagemRetorno;

  MessageDlg( Resp, mtInformation, [mbOK], 0);
end;

procedure TfrPOSTEFServer.btIniciarParaServidorClick(Sender: TObject);
begin
  if ACBrPOS1.Inicializada then
    PararServidorPOS
  else
    IniciarServidorPOS;
end;

procedure TfrPOSTEFServer.btCertInfoClick(Sender: TObject);
var
  SL: TStringList;
begin
  ConfigurarACBrNFe;
  ACBrNFe1.SSL.CarregarCertificado;
  SL := TStringList.Create;
  try
    SL.Add('Número de Série: '+ACBrNFe1.SSL.CertNumeroSerie);
    SL.Add('Válido até: '+FormatDateBr(ACBrNFe1.SSL.CertDataVenc));
    SL.Add('Subject Name: '+ACBrNFe1.SSL.CertSubjectName);
    SL.Add('Razão Social: ' + ACBrNFe1.SSL.CertRazaoSocial);
    SL.Add('CNPJ/CPF: ' + ACBrNFe1.SSL.CertCNPJ);
    SL.Add('Emissor: ' + ACBrNFe1.SSL.CertIssuerName);
    SL.Add('Certificadora: ' + ACBrNFe1.SSL.CertCertificadora);

    MessageDlg(SL.Text, mtInformation, [mbOK], 0);
  finally
    SL.Free;
  end;
end;

procedure TfrPOSTEFServer.btLerParametrosClick(Sender: TObject);
begin
  LerConfiguracao;
end;

procedure TfrPOSTEFServer.lURLTEFClick(Sender: TObject);
begin
  OpenURL(CACBR_URL);
end;

procedure TfrPOSTEFServer.rgDocumentoFiscalClick(Sender: TObject);
begin
  tsConfEmissor.Enabled := (rgDocumentoFiscal.ItemIndex > 0);
  tsConfEMail.Enabled := DeveFazerEnvioDeEmail;
  tsConfNFCe.Enabled := (rgDocumentoFiscal.ItemIndex = 1);
  tsConfSAT.Enabled := (rgDocumentoFiscal.ItemIndex = 2);
end;

procedure TfrPOSTEFServer.SbArqLogClick(Sender: TObject);
var
  AFileLog: String;
begin
  if pos(PathDelim,edLog.Text) = 0 then
    AFileLog := ExtractFilePath( Application.ExeName ) + edLog.Text
  else
    AFileLog := edLog.Text;

  OpenURL( AFileLog );
end;

procedure TfrPOSTEFServer.sbConsultaCEPClick(Sender: TObject);
var
  EndAchado: TACBrCEPEndereco;
  cUF: Integer;
begin
  try
    ACBrCEP1.BuscarPorCEP(OnlyNumber(edtEmitCEP.Text));
    if (ACBrCEP1.Enderecos.Count > 0) then
    begin
      EndAchado := ACBrCEP1.Enderecos[0];
      edtEmitLogradouro.Text := Trim(EndAchado.Tipo_Logradouro + ' ' + EndAchado.Logradouro);
      edtEmitBairro.Text := EndAchado.Bairro;
      edtEmitCEP.Text := ACBrValidador.FormatarCEP(EndAchado.CEP);
      edtEmitComp.Text := EndAchado.Complemento;
      cUF := UFtoCUF(EndAchado.UF);
      pEmitCodUF.Caption := IntToStr(cUF);
      CarregarListaDeCidades(cUF);
      cbxEmitUF.ItemIndex := cbxEmitUF.Items.IndexOf(EndAchado.UF);
      cbxEmitCidade.ItemIndex := cbxEmitCidade.Items.IndexOf(EndAchado.Municipio);
      cbxEmitCidadeChange(nil);
      edtEmitNumero.SetFocus;
    end;
  except
    MessageDlg('Erro ao executar Consulta do CEP', mtError, [mbOK], 0);
  end;
end;

procedure TfrPOSTEFServer.sbConsultaCNPJClick(Sender: TObject);
var
  frConsultaCNPJ: TfrConsultaCNPJ;
  MR: TModalResult;
  cUF: Integer;
begin
  frConsultaCNPJ := TfrConsultaCNPJ.Create(Self);
  try
    MR := frConsultaCNPJ.ShowModal;

    if (MR = mrOK) then
    begin
      try
        if ACBrConsultaCNPJ1.Consulta(edtEmitCNPJ.Text, frConsultaCNPJ.edtCaptcha.Text) then
        begin
          //EditTipo.Text := ACBrConsultaCNPJ1.EmpresaTipo;
          edtEmitRazao.Text := ACBrConsultaCNPJ1.RazaoSocial;
          edtEmitFantasia.Text := ACBrConsultaCNPJ1.Fantasia;
          edtEmitLogradouro.Text := ACBrConsultaCNPJ1.Endereco;
          edtEmitNumero.Text := ACBrConsultaCNPJ1.Numero;
          edtEmitComp.Text := ACBrConsultaCNPJ1.Complemento;
          edtEmitCEP.Text := ACBrConsultaCNPJ1.CEP;
          edtEmitBairro.Text := ACBrConsultaCNPJ1.Bairro;
          cUF := UFtoCUF(ACBrConsultaCNPJ1.UF);
          pEmitCodUF.Caption := IntToStr(cUF);
          CarregarListaDeCidades(cUF);
          cbxEmitUF.ItemIndex := cbxEmitUF.Items.IndexOf(ACBrConsultaCNPJ1.UF);
          pEmitCodCidade.Caption := Trim(ACBrConsultaCNPJ1.IBGE_Municipio);
          cbxEmitCidade.ItemIndex := fcMunList.IndexOf(pEmitCodCidade.Caption);
          cbxEmitCidadeChange(nil);
        end;
      except
        MessageDlg('Erro ao Consultar CNPJ'+sLineBreak+'Verifique o Captcha', mtError, [mbOK], 0);
      end;
    end;
  finally
     frConsultaCNPJ.Free;
  end;
end;

procedure TfrPOSTEFServer.sbNomeDLLClick(Sender: TObject);
begin
  OpenDialog1.Filter := 'Arquivo DLL|*.dll';
  OpenDialog1.InitialDir := ExtractFilePath(edtSATDLL.Text);
  OpenDialog1.FileName := edtSATDLL.Text;
  if OpenDialog1.Execute then
    edtSATDLL.Text := OpenDialog1.FileName ;
end;

procedure TfrPOSTEFServer.sbTerminarConexaoClick(Sender: TObject);
var
  TerminalId: String;
begin
  if (sgTerminais.Row >= 0) and (sgTerminais.Row < sgTerminais.RowCount) then
    TerminalId := sgTerminais.Rows[sgTerminais.Row][0]
  else
    TerminalId := '';

  if (TerminalId <> '') then
    ACBrPOS1.TerminarConexao(TerminalId);
end;

procedure TfrPOSTEFServer.sbtnCaminhoCertClick(Sender: TObject);
var
  AFile: String;
begin
  OpenDialog1.Title := 'Selecione o Certificado';
  OpenDialog1.DefaultExt := '*.pfx';
  OpenDialog1.Filter := 'Arquivos PFX (*.pfx)|*.pfx|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ApplicationPath;

  if OpenDialog1.Execute then
  begin
    AFile := OpenDialog1.FileName;
    if (pos(ApplicationPath, AFile) > 0) then
      AFile := ExtractFileName(AFile);

    edtCertArqPFX.Text := AFile;
  end;
end;

procedure TfrPOSTEFServer.sbtnGetCertClick(Sender: TObject);
begin
  edtCertNumSerie.Text := ACBrNFe1.SSL.SelecionarCertificado;
end;

procedure TfrPOSTEFServer.sbtnNumSerieClick(Sender: TObject);
var
  I: Integer;
  AddRow: Boolean;
  frmSelecionarCertificado: TfrmSelecionarCertificado;
begin
  frmSelecionarCertificado := TfrmSelecionarCertificado.Create(Self);
  try
    ACBrNFe1.SSL.LerCertificadosStore;
    AddRow := False;

    with frmSelecionarCertificado.StringGrid1 do
    begin
      ColWidths[0] := 220;
      ColWidths[1] := 250;
      ColWidths[2] := 120;
      ColWidths[3] := 80;
      ColWidths[4] := 150;

      Cells[0, 0] := 'Num.Série';
      Cells[1, 0] := 'Razão Social';
      Cells[2, 0] := 'CNPJ';
      Cells[3, 0] := 'Validade';
      Cells[4, 0] := 'Certificadora';
    end;

    for I := 0 to ACBrNFe1.SSL.ListaCertificados.Count-1 do
    begin
      with ACBrNFe1.SSL.ListaCertificados[I] do
      begin
        if (CNPJ <> '') then
        begin
          with frmSelecionarCertificado.StringGrid1 do
          begin
            if Addrow then
              RowCount := RowCount + 1;

            Cells[0, RowCount-1] := NumeroSerie;
            Cells[1, RowCount-1] := RazaoSocial;
            Cells[2, RowCount-1] := CNPJ;
            Cells[3, RowCount-1] := FormatDateBr(DataVenc);
            Cells[4, RowCount-1] := Certificadora;

            AddRow := True;
          end;
        end;
      end;
    end;

    frmSelecionarCertificado.ShowModal;

    if frmSelecionarCertificado.ModalResult = mrOK then
      edtCertNumSerie.Text := frmSelecionarCertificado.StringGrid1.Cells[0, frmSelecionarCertificado.StringGrid1.Row];
  finally
    frmSelecionarCertificado.Free;
  end;
end;

procedure TfrPOSTEFServer.sbVerSenhaCertificadoClick(Sender: TObject);
begin
  if sbVerSenhaCertificado.Down then
    edtCertSenha.PasswordChar := #0
  else
    edtCertSenha.PasswordChar := '*';
end;

procedure TfrPOSTEFServer.sbVerSenhaEmailClick(Sender: TObject);
begin
  if sbVerSenhaEmail.Down then
    edtEmailPassword.PasswordChar := #0
  else
    edtEmailPassword.PasswordChar := '*';
end;

procedure TfrPOSTEFServer.sbVerSenhaProxyClick(Sender: TObject);
begin
  if sbVerSenhaProxy.Down then
    edtProxySenha.PasswordChar := #0
  else
    edtProxySenha.PasswordChar := '*';
end;

procedure TfrPOSTEFServer.seSATPagCodChange(Sender: TObject);
begin
  ACBrSAT1.Config.PaginaDeCodigo := seSATPagCod.Value;
  ckSATUTF8.Checked := ACBrSAT1.Config.EhUTF8;
end;

procedure TfrPOSTEFServer.spPathSchemasClick(Sender: TObject);
var
  Dir: string;
begin
  if (Trim(edtPathSchemas.Text) = '') then
     Dir := ApplicationPath
  else
     Dir := edtPathSchemas.Text;

  if SelectDirectory(Dir, [sdAllowCreate, sdPerformCreate, sdPrompt], 0) then
    edtPathSchemas.Text := Dir;
end;

procedure TfrPOSTEFServer.tiIniciarTimer(Sender: TObject);
begin
  tiIniciar.Enabled := False;
  IrParaOperacaoPOS;
end;

function TfrPOSTEFServer.GetNomeArquivoConfiguracao: String;
begin
  Result := ChangeFileExt(Application.ExeName,'.ini') ;
end;

procedure TfrPOSTEFServer.TratarException(Sender: TObject; E: Exception);
begin
  AdicionarLinhaLog('');
  AdicionarLinhaLog('***************' + E.ClassName + '***************');
  AdicionarLinhaLog(E.Message);
  AdicionarLinhaLog('');

  if pgPrincipal.ActivePage = tsConfiguracao then
    MessageDlg(E.Message, mtError, [mbOK], 0);
end;

procedure TfrPOSTEFServer.Minimizar(Sender: TObject);
begin
  mLog.Lines.Clear;
end;

procedure TfrPOSTEFServer.AdicionarLinhaLog(AMensagem: String);
begin
  if (Self.WindowState <> wsMinimized) then
    mLog.Lines.Add(AMensagem);
end;

function TfrPOSTEFServer.EstadoTerminal(AEstado: TACBrPOSPGWebEstadoTerminal
  ): String;
begin
  case AEstado of
    statConectado: Result := 'Conectado';
    statOcupado: Result := 'Ocupado';
    statDesconectado: Result := 'Desconectado';
    statEsperaConexao: Result := 'Reconectando';
    statTEF: Result := 'Em Transação';
  else
    Result := 'Desconhecido';
  end;
end;

procedure TfrPOSTEFServer.IrParaOperacaoPOS;
begin
  AdicionarLinhaLog('- IrParaOperacaoPOS');
  GravarConfiguracao;
  AplicarConfiguracao;

  case cbTipoAplicacao.ItemIndex of
    0: IncluirPedidosSimuladosLoja;
    1: IncluirPedidosSimuladosRestaurante;
    2: IncluirAbastecimentosSimulados;
  end;

  sgTerminais.RowCount := 2;
  pgPrincipal.ActivePage := tsOperacao;
  IniciarServidorPOS;
end;

procedure TfrPOSTEFServer.IrParaConfiguracao;
begin
  AdicionarLinhaLog('- IrParaConfiguracao');
  pgPrincipal.ActivePage := tsConfiguracao;
end;

procedure TfrPOSTEFServer.IniciarServidorPOS;
begin
  AdicionarLinhaLog('- IniciarServidorPOS');
  VerificarErrosConfiguracaoPOS;
  VerificarErrosConfiguracaoNFCe;
  VerificarErrosConfiguracaoSAT;
  VerificarErrosConfiguracaoEMail;

  sgTerminais.RowCount := 2;
  AplicarConfiguracao;
  if DeveFazerEmissaoDeNFCe then
    ACBrNFe1.SSL.CarregarCertificado
  else if DeveFazerEmissaoDeSAT then
    ACBrSAT1.Inicializar;

  ACBrPOS1.Inicializar;
  btIniciarParaServidor.Caption := 'Parar Servidor';
  btIniciarParaServidor.Glyph := nil;
  ImageList1.GetBitmap(9, btIniciarParaServidor.Glyph);
end;

procedure TfrPOSTEFServer.PararServidorPOS;
begin
  AdicionarLinhaLog('- PararServidorPOS');
  if DeveFazerEmissaoDeSAT then
    ACBrSAT1.DesInicializar;

  ACBrPOS1.DesInicializar;
  btIniciarParaServidor.Caption := 'Iniciar Servidor';
  btIniciarParaServidor.Glyph := nil;
  ImageList1.GetBitmap(8, btIniciarParaServidor.Glyph);
end;

procedure TfrPOSTEFServer.VerificarErrosConfiguracaoPOS;
var
  MsgErro: String;
begin
  MsgErro := '';
  if (sePortaTCP.Value < 1) then
    MsgErro := '- Porta TCP Inválida';

  if (cbTipoAplicacao.ItemIndex < 0) then
    MsgErro := MsgErro + sLineBreak + '- Tipo de Aplicação não definido';

  MsgErro := Trim(MsgErro);
  if (MsgErro <> '') then
  begin
    PararServidorPOS;
    IrParaConfiguracao;
    pgConfig.ActivePage := tsConfPosTef;
    raise Exception.Create(MsgErro);
  end;
end;

procedure TfrPOSTEFServer.VerificarErrosConfiguracaoNFCe;
var
  MsgErro: String;
begin
  if not DeveFazerEmissaoDeNFCe then
    Exit;

  MsgErro := '';
  if imgErrSSLLib.Visible or imgErrCryptLib.Visible or
     imgErrHttpLib.Visible or imgErrXmlSignLib.Visible then
    MsgErro := '- Bibliotecas não configuradas';

  if imgErrCertificado.Visible then
    MsgErro := MsgErro + sLineBreak + '- Certificado não configurado';

  if imgErrWebService.Visible or imgErrPathSchemas.Visible then
    MsgErro := MsgErro + sLineBreak + '- WebService não configurado';

  if imgErrTokenID.Visible or imgErrTokenCSC.Visible then
    MsgErro := MsgErro + sLineBreak + '- Token CSC não configurado';

  MsgErro := Trim(MsgErro);
  if (MsgErro <> '') then
  begin
    PararServidorPOS;
    IrParaConfiguracao;
    pgConfig.ActivePage := tsConfNFCe;
    raise Exception.Create(MsgErro);
  end;
end;

procedure TfrPOSTEFServer.VerificarErrosConfiguracaoSAT;
var
  MsgErro: String;
begin
  if not DeveFazerEmissaoDeSAT then
    Exit;

  MsgErro := '';
  if imgErrSAT.Visible then
    MsgErro := 'SAT não configurado';

  if (MsgErro <> '') then
  begin
    PararServidorPOS;
    IrParaConfiguracao;
    pgConfig.ActivePage := tsConfSAT;
    raise Exception.Create(MsgErro);
  end;
end;

procedure TfrPOSTEFServer.VerificarErrosConfiguracaoEMail;
var
  MsgErro: String;
begin
  if not DeveFazerEnvioDeEmail then
    Exit;

  MsgErro := '';
  if imgErrEmail.Visible then
    MsgErro := 'Email não configurado';

  if (MsgErro <> '') then
  begin
    PararServidorPOS;
    IrParaConfiguracao;
    pgConfig.ActivePage := tsConfEMail;
    raise Exception.Create(MsgErro);
  end;
end;

procedure TfrPOSTEFServer.CarregarListaDeCidades(cUF: Integer);
var
  i: Integer;
  Cidade: TACBrIBGECidade;
begin
  if (cUF = 0) or (fcUF = cUF) then
    Exit;

  fcUF := cUF;
  try
    ACBrIBGE1.BuscarPorcUF(FcUF);
    cbxEmitCidade.Items.Clear;
    cbxEmitCidade.ItemIndex := -1;
    fcMunList.Clear;
    for i := 0 to ACBrIBGE1.Cidades.Count-1 do
    begin
      Cidade := ACBrIBGE1.Cidades[i];
      cbxEmitCidade.Items.Add(Cidade.Municipio);
      fcMunList.Add(IntToStr(Cidade.CodMunicipio));
    end;

    if (cbxEmitCidade.Items.Count > 0) then
    begin
      cbxEmitCidade.ItemIndex := 0;
      cbxEmitCidadeChange(Nil);
    end;
  except
    MessageDlg('Erro ao carregar cidades', mtError, [mbOK], 0);
  end;
end;

procedure TfrPOSTEFServer.ExecutarFluxoPapaFila(const TerminalId: String);
var
  OP: SmallInt;
begin
  OP := ACBrPOS1.ExecutarMenu( TerminalId,
                               'VER PEDIDOS|'+
                               'REIMPRESSAO|'+
                               'ADMINISTRATIVO|'+
                               'CANCELAMENTO|'+
                               'S A I R',
                               PadCenter('PAPA-FILA - ACBR', CACBrPOSPGWebColunasDisplay)
                             );
  case OP of
    0: ExibirMenuPedidos(TerminalId);
    1: ExecutarReimpressao(TerminalId);
    2: EfetuarAdministrativo(TerminalId);
    3: EfetuarCancelamento(TerminalId);
  end;
end;

procedure TfrPOSTEFServer.ExibirMenuPedidos(const TerminalId: String);
var
  SL: TStringList;
  OP: SmallInt;
  i, l, IndicePedido, NumPedidos: Integer;
  PedidosListados: array of Integer;
begin
  SL := TStringList.Create;
  try
    {
      *** NOTA ***
      Aqui você deve ler os Pedidos Pendentes no Seu Banco de Dados, com algum
      Filtro, para Exibir apenas os Pedidos em aberto e relacionados a esse POS
    }
    NumPedidos := Length(fPedidos)-1;
    SetLength(PedidosListados, 0);
    for i := 0 to NumPedidos do
      if (fPedidos[i].TotalPago = 0) then  // já não foi pago ?
      begin
        l := Length(PedidosListados);
        SetLength(PedidosListados, l+1);
        PedidosListados[l] := i;
        SL.Add(LeftStr(fPedidos[i].Nome,8)+' R$ '+FormatFloatBr(fPedidos[i].ValorTotal,'###0.00') );
      end;

    SL.Add('C A N C E L A R');
    OP := ACBrPOS1.ExecutarMenu(TerminalId, SL, 'ESCOLHA O PEDIDO');

    if (OP >= 0) and (OP < SL.Count-1) then
    begin
      IndicePedido := PedidosListados[OP];
      // Imprimindo conferência do Pedido //
      ImprimirPedido(TerminalId, IndicePedido);

      // Efetuando pagamento do Pedido
      EfetuarPagamento(TerminalId, IndicePedido);

      // Gerando e transmitindo o Documento Fiscal
      EfetuarDocumentoFiscal(TerminalId, IndicePedido);
    end;
  finally
    SL.Free;
  end;
end;

procedure TfrPOSTEFServer.ImprimirPedido(const TerminalId: String;
  IndicePedido: Integer);
var
  SL: TStringList;
  ColImp, ColExp, i: Integer;
  TotalItem: Currency;
begin
  // *** NOTA *** Aqui você deve ler os Itens do Pedido Selecionado, no Seu Banco de Dados

  ACBrPOS1.ExibirMensagem(TerminalId,
      PadCenter('PEDIDO '+IntToStrZero(fPedidos[IndicePedido].NumPedido, 4), CACBrPOSPGWebColunasDisplay)+
      CR +
      PadCenter('CONFIRA', CACBrPOSPGWebColunasDisplay)+
      PadCenter('ITENS E VALOR', CACBrPOSPGWebColunasDisplay)
      );

  ColImp := CACBrPOSPGWebColunasImpressora;
  ColExp := Trunc(ColImp/2)-1;

  // Montando relatório com o Pedido
  SL := TStringList.Create;
  try
    SL.Add( '<e>'+PadCenter(edRazaoSocial.Text, ColExp) );
    SL.Add( StringOfChar('-', ColImp));
    SL.Add( PadSpace( 'Pedido: '+IntToStrZero(fPedidos[IndicePedido].NumPedido, 4)+'|'+
                      'Data: '+FormatDateTimeBr(fPedidos[IndicePedido].DataHora),
              ColImp, '|') );
    SL.Add( '<e>NOME: '+fPedidos[IndicePedido].Nome );
    SL.Add( StringOfChar('-', ColImp) );
    for i := 0 to Length(fPedidos[IndicePedido].Items)-1 do
    begin
      TotalItem := fPedidos[IndicePedido].Items[i].Qtd * fPedidos[IndicePedido].Items[i].PrecoUnit;
      SL.Add( IntToStrZero(fPedidos[IndicePedido].Items[i].CodItem, 4)   + ' '+
              PadRight(fPedidos[IndicePedido].Items[i].Descricao,35) );
      SL.Add( PadSpace( FormatFloatBr(fPedidos[IndicePedido].Items[i].Qtd, '##0.00')+ ' '+
              PadRight(fPedidos[IndicePedido].Items[i].UN,2)+ ' x ' +
                        FormatFloatBr(fPedidos[IndicePedido].Items[i].PrecoUnit, 'R$ #,##0.00') + '|' +
                        FormatFloatBr( TotalItem, 'R$ #,##0.00'),
                ColImp, '|') );
    end;
    SL.Add( StringOfChar('-', ColImp) );
    SL.Add( '<e>'+PadSpace( 'TOTAL:|'+
                            FormatFloatBr( fPedidos[IndicePedido].ValorTotal, 'R$ #,##0.00'),
                    ColExp, '|') );

    if (fPedidos[IndicePedido].Pessoas > 1) then
    begin
      SL.Add( ' ' );
      SL.Add( '  Dividir por.......: '+IntToStr(fPedidos[IndicePedido].Pessoas) );
      SL.Add( '  Valor aprox.Pessoa: '+FormatFloatBr( RoundTo( fPedidos[IndicePedido].ValorTotal /
                                                               fPedidos[IndicePedido].Pessoas, -2),
                                                      'R$ #,##0.00') );
    end;

    SL.Add( ' ' );
    SL.Add( PadCenter(CACBR_URL, ColImp, '-')  );

    ACBrPOS1.ImprimirTexto(TerminalId, SL.Text);
    ACBrPOS1.AvancarPapel(TerminalId);
  finally
    SL.Free;
  end;
end;

procedure TfrPOSTEFServer.EfetuarDocumentoFiscal(const TerminalId: String;
  IndicePedido: Integer);
begin
  if not (DeveFazerEmissaoDeNFCe or DeveFazerEmissaoDeSAT) then
    Exit;

  if DeveFazerEmissaoDeNFCe then
    EfetuarNFCe(TerminalId, IndicePedido)
  else if DeveFazerEmissaoDeSAT then
    EfetuarSAT(TerminalId, IndicePedido);

end;

function TfrPOSTEFServer.ACBrPosPrinterFactory: TACBrPosPrinter;
begin
  Result := TACBrPosPrinter.Create(nil);
  Result.ColunasFonteNormal := 40;
  Result.PaginaDeCodigo := pcNone;
end;

function TfrPOSTEFServer.ACBrPOSPGWebPrinterFactory(const TerminalId: String;
  AACBrPosPrinter: TACBrPosPrinter; AACBrPOS: TACBrPOS): TACBrPOSPGWebPrinter;
begin
  Result := TACBrPOSPGWebPrinter.Create(AACBrPosPrinter, AACBrPOS);
  Result.TerminalId := TerminalId;
  AACBrPosPrinter.ModeloExterno := Result;
end;

procedure TfrPOSTEFServer.EfetuarNFCe(const TerminalId: String;
  IndicePedido: Integer);
var
  lACBrNFe: TACBrNFe;
begin
  lACBrNFe := TACBrNFe.Create(nil);  // cria um novo componente, para essa Thread, e usa ele internamente
  try
    lACBrNFe.Configuracoes.Assign(ACBrNFe1.Configuracoes); // copia as configurações do componente Principal
    GerarNFCe(TerminalId, lACBrNFe, IndicePedido);
    TransmitirNFCe(lACBrNFe, TerminalId);
    ImprimirNFCe(lACBrNFe, TerminalId);
    EnviarEmailNFCe(lACBrNFe, TerminalId, fPedidos[IndicePedido].Email);
  finally
    lACBrNFe.Free;
  end;
end;

procedure TfrPOSTEFServer.GerarNFCe(const TerminalId: String;
  AACBrNFe: TACBrNFe; IndicePedido: Integer);
var
  BaseCalculo, BaseCalculoTotal, ValorICMS, ValorICMSTotal, TotalItem,
    ValorTotalItens: Currency;
  i: Integer;
  APag: TpagCollectionItem;
begin
  {
    *** NOTA ***
    1 - Aqui você deve consultar seu Banco de Dados, Ler o Pedido e calcular os Impostos
        corretamente
  }

  AACBrNFe.NotasFiscais.Clear;
  with AACBrNFe.NotasFiscais.Add.NFe do
  begin
    Ide.natOp     := 'VENDA';
    Ide.indPag    := ipVista;
    Ide.modelo    := 65;
    Ide.serie     := 1;
    Ide.nNF       := ObterProximaNFCe;
    Ide.cNF       := GerarCodigoDFe(Ide.nNF);
    Ide.dEmi      := now;
    Ide.dSaiEnt   := now;
    Ide.hSaiEnt   := now;
    Ide.tpNF      := tnSaida;
    Ide.tpEmis    := teNormal;  // Implementar NFCe OffLine
    Ide.tpAmb     := TpcnTipoAmbiente(cbTipoAmb.ItemIndex);
    Ide.cUF       := StrToInt(pEmitCodUF.Caption);
    Ide.cMunFG    := StrToInt(pEmitCodCidade.Caption);
    Ide.finNFe    := fnNormal;
    Ide.tpImp     := tiNFCe;
    Ide.indFinal  := cfConsumidorFinal;
    Ide.indPres   := pcPresencial;

    //if not swOnLine.IsChecked then
    //begin
    //  Ide.dhCont := date;
    //  Ide.xJust  := 'Justificativa Contingencia';
    //end;

    Emit.CNPJCPF           := OnlyNumber(edtEmitCNPJ.Text);
    Emit.IE                := OnlyNumber(edtEmitIE.Text);
    Emit.IEST              := '';
    Emit.xNome             := edtEmitRazao.Text;
    Emit.xFant             := edtEmitFantasia.Text;
    Emit.CRT := TpcnCRT(cbxTipoEmpresa.ItemIndex);

    Emit.EnderEmit.fone    := edtEmitFone.Text;
    Emit.EnderEmit.CEP     := StrToInt(OnlyNumber(edtEmitCEP.Text));
    Emit.EnderEmit.xLgr    := edtEmitLogradouro.Text;
    Emit.EnderEmit.nro     := edtEmitNumero.Text;
    Emit.EnderEmit.xCpl    := edtEmitComp.Text;
    Emit.EnderEmit.xBairro := edtEmitBairro.Text;
    Emit.EnderEmit.cMun    := StrToInt(pEmitCodCidade.Caption);
    Emit.EnderEmit.xMun    := cbxEmitCidade.Text;
    Emit.EnderEmit.UF      := cbxEmitUF.Text;
    Emit.enderEmit.cPais   := 1058;
    Emit.enderEmit.xPais   := 'BRASIL';

    BaseCalculoTotal := 0;
    ValorICMSTotal := 0;
    ValorTotalItens := 0;
    for i := 0 to Length(fPedidos[IndicePedido].Items)-1 do
    begin
      TotalItem := RoundABNT(fPedidos[IndicePedido].Items[i].Qtd * fPedidos[IndicePedido].Items[i].PrecoUnit, -2);

      //Adicionando Produtos
      with Det.New do
      begin
        Prod.nItem    := i+1;
        Prod.cProd    := IntToStr(fPedidos[IndicePedido].Items[i].CodItem);
        Prod.xProd    := fPedidos[IndicePedido].Items[i].Descricao;
        Prod.cEAN     := 'SEM GTIN';
        Prod.NCM      := '00000000';
        Prod.CFOP     := '5102';
        Prod.uCom     := fPedidos[IndicePedido].Items[i].UN;
        Prod.qCom     := fPedidos[IndicePedido].Items[i].Qtd;
        Prod.vUnCom   := fPedidos[IndicePedido].Items[i].PrecoUnit;
        Prod.vProd    := TotalItem;

        Prod.cEANTrib  := 'SEM GTIN';
        Prod.uTrib     := fPedidos[IndicePedido].Items[i].UN;
        Prod.qTrib     := fPedidos[IndicePedido].Items[i].Qtd;
        Prod.vUnTrib   := fPedidos[IndicePedido].Items[i].PrecoUnit;

        Prod.CEST := '';
  //    infAdProd      := 'Informação Adicional do Produto';

        with Imposto do
        begin
          // lei da transparencia nos impostos
          vTotTrib := 0;

          with ICMS do
          begin
            // caso o CRT seja:
            // 1=Simples Nacional
            // Os valores aceitos para CSOSN são:
            // csosn101, csosn102, csosn103, csosn201, csosn202, csosn203,
            // csosn300, csosn400, csosn500,csosn900

            // 2=Simples Nacional, excesso sublimite de receita bruta;
            // ou 3=Regime Normal.
            // Os valores aceitos para CST são:
            // cst00, cst10, cst20, cst30, cst40, cst41, cst45, cst50, cst51,
            // cst60, cst70, cst80, cst81, cst90, cstPart10, cstPart90,
            // cstRep41, cstVazio, cstICMSOutraUF, cstICMSSN, cstRep60

            // (consulte o contador do seu cliente para saber qual deve ser utilizado)
            // Pode variar de um produto para outro.

            if Emit.CRT in [crtSimplesExcessoReceita, crtRegimeNormal] then
              CST := cst00
            else
              CSOSN := csosn102;

            orig  := oeNacional;
            modBC := dbiValorOperacao;

            if Emit.CRT in [crtSimplesExcessoReceita, crtRegimeNormal] then
              BaseCalculo := TotalItem
            else
              BaseCalculo := 0;

            BaseCalculoTotal := BaseCalculoTotal + BaseCalculo;

            vBC     := BaseCalculo;
            pICMS   := 18;

            ValorICMS := vBC * pICMS;
            ValorICMSTotal := ValorICMSTotal + ValorICMS;
            ValorTotalItens := ValorTotalItens + TotalItem;

            vICMS   := ValorICMS;
            modBCST := dbisMargemValorAgregado;

            vBCFCPST := TotalItem;
          end;

          PIS.CST := pis99;
          COFINS.CST := cof99;
        end;
      end;
    end;

    Total.ICMSTot.vBC     := BaseCalculoTotal;
    Total.ICMSTot.vICMS   := ValorICMSTotal;
    Total.ICMSTot.vProd   := ValorTotalItens;
    Total.ICMSTot.vNF     := ValorTotalItens;

    Transp.modFrete := mfSemFrete; // NFC-e não pode ter FRETE

    for i := 0 to Length(fPedidos[IndicePedido].Pagamentos)-1 do
    begin
      APag := pag.New;
      with APag do
      begin
        indPag := ipVista;
        vPag := fPedidos[IndicePedido].Pagamentos[i].ValorPago;

        case fPedidos[IndicePedido].Pagamentos[i].FormaPagto of
          CPAG_DEBITO: tPag := fpCartaoDebito;
          CPAG_CREDITO: tPag := fpCartaoCredito;
        else
          tPag :=  fpDinheiro;
        end;

        if tPag in [fpCartaoCredito, fpCartaoDebito] then
        begin
          DeduzirCredenciadoraNFCe(APag, ACBrPOS1.TEFResp[TerminalId]);
          DeduzirBandeiraNFCe(APag, ACBrPOS1.TEFResp[TerminalId]);
        end;
      end;
    end;

    if fPedidos[IndicePedido].TotalPago > fPedidos[IndicePedido].ValorTotal then
      pag.vTroco := (fPedidos[IndicePedido].TotalPago - fPedidos[IndicePedido].ValorTotal);
  end;

  AACBrNFe.NotasFiscais.GerarNFe;
  AACBrNFe.NotasFiscais.Assinar;
  AACBrNFe.NotasFiscais.GravarXML;
end;

function TfrPOSTEFServer.ObterProximaNFCe: Integer;
begin
  fcsDocFiscal.Acquire;
  try
    // Incrementando o número da NFCe
    seNFCeNumero.Value := seNFCeNumero.Value + 1;
    Result := seNFCeNumero.Value;
    GravarConfiguracao;
  finally
    fcsDocFiscal.Release;
  end;
end;

procedure TfrPOSTEFServer.DeduzirCredenciadoraNFCe(APag: TpagCollectionItem;
  ATEFResp: TACBrTEFResp);
var
  CNPJ, Rede: String;
  p: Integer;
begin
  CNPJ := Trim(ATEFResp.NFCeSAT.CNPJCredenciadora);
  if (CNPJ = '') then
  begin
    Rede := LowerCase(Trim(ATEFResp.Rede));

    // Pega apenas a Perimeira Palavra
    p := pos(' ',Rede);
    if (p > 0) then
      Rede := copy(Rede,1, p-1);

    if (pos('bin', Rede) = 1) or (pos('sipag', Rede) = 1) then
      CNPJ := '02.038.232/0001-64'
    else if (pos('tribanco', Rede) = 1) then
      CNPJ := '17.351.180/0001-59'
    else if (pos('bigcard', Rede) = 1) then
      CNPJ := '04.627.085/0001-93'
    else if (pos('brasilcard', Rede) = 1) then
      CNPJ := '03.817.702/0001-50'
    else if (pos('cabal', Rede) = 1) then
      CNPJ := '03.766.873/0001-06'
    else if (pos('credpar', Rede) = 1) then
      CNPJ := '07.599.577/0001-00'
    else if (pos('ecx', Rede) = 1) then
      CNPJ := '71.225.700/0001-22'
    else if (pos('elavon', Rede) = 1) then
      CNPJ := '12.592.831/0001-89'
    else if (pos('ecofrotas', Rede) = 1) then
      CNPJ := '03.506.307/0001-57'
    else if (pos('jetpar', Rede) = 1) then
      CNPJ := '12.886.711/0001-94'
    else if (pos('usacard', Rede) = 1) then
      CNPJ := '08.011.683/0001-94'
    else if pos('rede', Rede) > 0 then
      CNPJ := '01.425.787/0001-04'
    else if (pos('repon', Rede) = 1) then
      CNPJ := '65.697.260/0001-03'
    else if (pos('senffnet', Rede) = 1) then
      CNPJ := '03.877.288/0001-75'
    else if (pos('siga', Rede) = 1) then
      CNPJ := '04.966.359/0001-79'
    else if (pos('sodexo', Rede) = 1) then
      CNPJ := '69.034.668/0001-56'
    else if (pos('stone', Rede) = 1) then
      CNPJ := '16.501.555/0001-57'
    else if (pos('tecban', Rede) = 1) then
      CNPJ := '51.427.102/0001-29'
    else if (pos('ticket', Rede) = 1) then
      CNPJ := '47.866.934/0001-74'
    else if (pos('valeshop', Rede) = 1) then
      CNPJ := '02.561.118/0001-14'
    else if (pos('visa', Rede) = 1) then
      CNPJ := '01.027.058/0001-91';
  end;

  if (CNPJ='') then
    APag.tpIntegra := tiPagNaoIntegrado
  else
  begin
    APag.tpIntegra := tiPagIntegrado;
    APag.CNPJ := OnlyNumber(CNPJ);
  end;
end;

procedure TfrPOSTEFServer.DeduzirBandeiraNFCe(APag: TpagCollectionItem;
  ATEFResp: TACBrTEFResp);
var
  p: Integer;
  Bandeira: String;
begin
  if  APag.tpIntegra = tiPagNaoIntegrado then
    Exit;

  Bandeira := LowerCase(Trim(ATEFResp.NFCeSAT.Bandeira));
  // Pega apenas a Perimeira Palavra
  p := pos(' ',Bandeira);
  if (p > 0) then
    Bandeira := copy(Bandeira,1, p-1);

  if (pos('visa',Bandeira) = 1) then
    APag.tBand := bcVisa
  else if (pos('mastercard',Bandeira) = 1) then
    APag.tBand := bcMasterCard
  else if (pos('american',Bandeira) = 1) or (pos('amex',Bandeira) = 1) then
    APag.tBand := bcAmericanExpress
  else if (pos('sorocred',Bandeira) = 1) then
    APag.tBand := bcSorocred
  else if (pos('diners',Bandeira) = 1) then
    APag.tBand := bcDinersClub
  else if (pos('elo',Bandeira) = 1) then
    APag.tBand := bcElo
  else if (pos('hipercard',Bandeira) = 1) then
    APag.tBand := bcHipercard
  else if (pos('aura',Bandeira) = 1) then
    APag.tBand := bcAura
  else if (pos('cabal',Bandeira) = 1) then
    APag.tBand := bcCabal
  else
    APag.tBand := bcOutros;
end;

procedure TfrPOSTEFServer.DeduzirCredenciadoraSAT(APag: TMPCollectionItem;
  ATEFResp: TACBrTEFResp);
var
  p, CodCred: Integer;
  Rede: String;
begin
  CodCred := StrToIntDef(ATEFResp.NFCeSAT.CodCredenciadora, 999);
  if (CodCred = 999) then
  begin
    Rede := LowerCase(Trim(ATEFResp.Rede));
    // Pega apenas a Perimeira Palavra
    p := pos(' ',Rede);
    if (p > 0) then
      Rede := copy(Rede,1, p-1);

    if (pos('sicred', Rede) = 1) then
      CodCred := 1
    else if (pos('amex', Rede) = 1) then
      CodCred := 3
    else if (pos('safra', Rede) = 1) then
      CodCred := 5
    else if (pos('topazio', Rede) = 1) then
      CodCred := 6
    else if (pos('triangulo', Rede) = 1) then
      CodCred := 7
    else if (pos('bigcard', Rede) = 1) then
      CodCred := 8
    else if (pos('bourbon', Rede) = 1) then
      CodCred := 9
    else if (pos('cabal', Rede) = 1) then
      CodCred := 10
    else if (pos('celetem', Rede) = 1) then
      CodCred := 11
    else if (pos('cielo', Rede) = 1) then
      CodCred := 12
    else if (pos('credi', Rede) = 1) then
      CodCred := 13
    else if (pos('ecx', Rede) = 1) then
      CodCred := 14
    else if (pos('embtratec', Rede) = 1) then
      CodCred := 15
    else if (pos('freedom', Rede) = 1) then
      CodCred := 17
    else if (pos('funcional', Rede) = 1) then
      CodCred := 18
    else if (pos('hipercard', Rede) = 1) then
      CodCred := 19
    else if (pos('mapa', Rede) = 1) then
      CodCred := 20
    else if (pos('novo', Rede) = 1) then
      CodCred := 21
    else if (pos('pernambucanas', Rede) = 1) then
      CodCred := 22
    else if (pos('policard', Rede) = 1) then
      CodCred := 23
    else if (pos('provar', Rede) = 1) then
      CodCred := 24
    else if (pos('redecard', Rede) = 1) then
      CodCred := 25
    else if (pos('renner', Rede) = 1) then
      CodCred := 26
    else if (pos('rp', Rede) = 1) then
      CodCred := 27
    else if (pos('santinvest', Rede) = 1) then
      CodCred := 28
    else if (pos('sodexho', Rede) = 1) then
      CodCred := 29
    else if (pos('sorocred', Rede) = 1) then
      CodCred := 30
    else if (pos('tecban', Rede) = 1) then
      CodCred := 31
    else if (pos('ticket', Rede) = 1) then
      CodCred := 32
    else if (pos('trivale', Rede) = 1) then
      CodCred := 33
    else if (pos('tricard', Rede) = 1) then
      CodCred := 34
    else
      CodCred := 999;
  end;

  APag.cAdmC := CodCred;
end;

procedure TfrPOSTEFServer.TransmitirNFCe(AACBrNFe: TACBrNFe;
  const TerminalId: String);
begin
  ACBrPOS1.ExibirMensagem( TerminalId, PadCenter( 'TRANSMITINDO', CACBrPOSPGWebColunasDisplay) +
                                       PadCenter( 'NFC-e', CACBrPOSPGWebColunasDisplay) );
  AACBrNFe.Enviar(IntToStr(seNFCeLote.Value), False, True, False);  // NãoImprimir, Sincrono
end;

procedure TfrPOSTEFServer.ExecutarFluxoFechamentoMesa(const TerminalId: String);
var
  OP: SmallInt;
begin
  OP := ACBrPOS1.ExecutarMenu( TerminalId,
                               'MESAS ABERTAS|'+
                               'REIMPRESSAO|'+
                               'ADMINISTRATIVO|'+
                               'CANCELAMENTO|'+
                               'S A I R',
                               PadCenter('RESTAURANTE - ACBR', CACBrPOSPGWebColunasDisplay)
                             );
  case OP of
    0: ExibirMenuMesasEmAberto(TerminalId);
    1: ExecutarReimpressao(TerminalId);
    2: EfetuarAdministrativo(TerminalId);
    3: EfetuarCancelamento(TerminalId);
  end;
end;

procedure TfrPOSTEFServer.ExibirMenuMesasEmAberto(const TerminalId: String);
var
  SL: TStringList;
  OP: SmallInt;
  i, l, IndicePedido, NumPedidos: Integer;
  PedidosListados: array of Integer;
begin
  SL := TStringList.Create;
  try
    {
      *** NOTA ***
      Aqui você deve ler os Pedidos Pendentes no Seu Banco de Dados, com algum
      Filtro, para Exibir apenas os Pedidos em aberto e relacionados a esse POS
    }
    NumPedidos := Length(fPedidos)-1;
    SetLength(PedidosListados, 0);
    for i := 0 to NumPedidos do
      if (fPedidos[i].TotalPago = 0) then  // já não foi pago ?
      begin
        l := Length(PedidosListados);
        SetLength(PedidosListados, l+1);
        PedidosListados[l] := i;
        SL.Add(LeftStr(fPedidos[i].Nome,8)+' R$ '+FormatFloatBr(fPedidos[i].ValorTotal,'###0.00') );
      end;

    SL.Add('C A N C E L A R');
    OP := ACBrPOS1.ExecutarMenu(TerminalId, SL, 'ESCOLHA A MESA');

    if (OP >= 0) and (OP < SL.Count-1) then
    begin
      IndicePedido := PedidosListados[OP];

      // Perguntando Número de pessoas, para divisão da conta
      PerguntarNumeroPessoas(TerminalId, IndicePedido);

      // Imprimindo conferência do Pedido
      ImprimirPedido(TerminalId, IndicePedido);

      // Efetuando pagamento do Pedido com Multiplos Pagamentos
      EfetuarMultiplosPagamento(TerminalId, IndicePedido);

      // Gerando e transmitindo o Documento Fiscal
      EfetuarDocumentoFiscal(TerminalId, IndicePedido);
    end;
  finally
    SL.Free;
  end;
end;

procedure TfrPOSTEFServer.PerguntarNumeroPessoas(const TerminalId: String;
  IndicePedido: Integer);
var
  AStr, AMsg: String;
begin
  AStr := '';
  AMsg := LeftStr(fPedidos[IndicePedido].Nome,8)+
          ' R$ '+FormatFloatBr(fPedidos[IndicePedido].ValorTotal,'###0.00')+CR+
          'NUM.PESSOAS:';

  AStr := ACBrPOS1.ObterDado(TerminalId, AMsg, '@@', 1, 2, True, False, False, 0, AStr, 3);
  if (AStr = '') then
    raise EFluxoInterrompido.Create('');

  fPedidos[IndicePedido].Pessoas := StrToIntDef(AStr, 1);
end;

procedure TfrPOSTEFServer.ExecutarFluxoFechamentoBomba(const TerminalId: String);
var
  OP: SmallInt;
begin
  OP := ACBrPOS1.ExecutarMenu( TerminalId,
                               'ABASTECIMENTOS|'+
                               'REIMPRESSAO|'+
                               'ADMINISTRATIVO|'+
                               'CANCELAMENTO|'+
                               'S A I R',
                               PadCenter('POSTO - ACBR', CACBrPOSPGWebColunasDisplay)
                             );
  case OP of
    0: ExibirMenuAbastecimentos(TerminalId);
    1: ExecutarReimpressao(TerminalId);
    2: EfetuarAdministrativo(TerminalId);
    3: EfetuarCancelamento(TerminalId);
  end;
end;

procedure TfrPOSTEFServer.ExibirMenuAbastecimentos(const TerminalId: String);
var
  SL: TStringList;
  OP: SmallInt;
  i, l, IndiceAbastec, NumAbastec: Integer;
  AbastecListados: array of Integer;
  TotalItem: Currency;
begin
  SL := TStringList.Create;
  try
    {
      *** NOTA ***
      Aqui você deve ler os Pedidos Pendentes no Seu Banco de Dados, com algum
      Filtro, para Exibir apenas os Pedidos em aberto e relacionados a esse POS
    }
    NumAbastec := Length(fAbastecimentos)-1;
    SetLength(AbastecListados, 0);
    for i := 0 to NumAbastec do
    begin
      //if (fAbastecimentos[i].FormaPagto = 0) then  // não foi pago ?
      begin
        l := Length(AbastecListados);
        SetLength(AbastecListados, l+1);
        AbastecListados[l] := i;
        TotalItem := Trunc(fAbastecimentos[i].Qtd * fBicos[fAbastecimentos[i].Bico].PrecoUnit * 100) / 100;
        SL.Add( PadRight(fBicos[fAbastecimentos[i].Bico].Descricao,7) + ' ' +
                IntToStrZero(fAbastecimentos[i].Bico+1, 2) +
                PadLeft('$'+FormatFloatBr(TotalItem), 8) );
      end;
    end;

    SL.Add('C A N C E L A R');
    OP := ACBrPOS1.ExecutarMenu(TerminalId, SL, 'ABASTECIMENTO ?');

    if (OP >= 0) and (OP < SL.Count-1) then
    begin
      IndiceAbastec := AbastecListados[OP];
      // Exibindo o Abastecimento na Tela, para confirmação //
      if not ExibirAbastecimento(TerminalId, IndiceAbastec) then
        Exit;

      // Efetuando pagamento do Abastecimento
      EfetuarPagamentoAbastec(TerminalId, IndiceAbastec);

      // Gerando e transmitindo o Documento Fiscal
      EfetuarDocumentoFiscalAbastec(TerminalId, IndiceAbastec);
    end;
  finally
    SL.Free;
  end;
end;

function TfrPOSTEFServer.ExibirAbastecimento(const TerminalId: String;
  IndiceAbastec: Integer): Boolean;
var
  Tecla: Integer;
  TotalItem: Currency;
begin
  // Exibindo o Abastecimento na Tela, para confirmação //
  with fAbastecimentos[IndiceAbastec] do
  begin
    TotalItem := Trunc(Qtd * fBicos[Bico].PrecoUnit * 100) / 100;
    ACBrPOS1.ExibirMensagem(TerminalId,
      PadCenter(FormatDateTime('HH:NN', DataHora) + ' ' +
                fBicos[Bico].Descricao,
                CACBrPOSPGWebColunasDisplay) +
      PadRight( FormatFloatBr(msk6x3, Qtd) + 'L x ' +
                FormatFloatBr(msk6x3, fBicos[Bico].PrecoUnit),
                CACBrPOSPGWebColunasDisplay) +
      'B:'+IntToStrZero(Bico+1, 2)+
      PadLeft( 'Total: '+
               FormatFloatBr(TotalItem, '0.00'),
               CACBrPOSPGWebColunasDisplay-4) +
      'C O N T I N U A R ?', 0);
  end;

  Tecla := ACBrPOS1.AguardarTecla(TerminalId, ACBrPOS1.TempoDesconexaoAutomatica);
  Result := (Tecla = PTIKEY_OK);
end;

procedure TfrPOSTEFServer.EfetuarPagamentoAbastec(const TerminalId: String;
  IndiceAbastec: Integer);
var
  OP: SmallInt;
begin
  OP := ACBrPOS1.ExecutarMenu(TerminalId, 'CARTAO|DINHEIRO|CANCELAR', 'EFETUAR O PAGAMENTO');

  case OP of
    0: EfetuarPagamentoAbastecCartao(TerminalId, IndiceAbastec);
    1:
    begin
      if not EfetuarPagamentoAbastecDinheiro(TerminalId, IndiceAbastec) then
        raise EFluxoInterrompido.Create('');
    end
  else
    raise EFluxoInterrompido.Create('');
  end;
end;

procedure TfrPOSTEFServer.EfetuarPagamentoAbastecCartao(
  const TerminalId: String; IndiceAbastec: Integer);
var
  FormaPagto: Byte;
  MsgErro: String;
  ValorTotal: Currency;
begin
  try
    ValorTotal := Trunc(fAbastecimentos[IndiceAbastec].Qtd * fBicos[fAbastecimentos[IndiceAbastec].Bico].PrecoUnit * 100) / 100;
    ACBrPOS1.ParametrosAdicionais[TerminalId].Clear;
    ACBrPOS1.ParametrosAdicionais[TerminalId].ValueInfo[PWINFO_FINTYPE] := '1'; //01: à vista
    //ACBrPOS1.ParametrosAdicionais[TerminalId].ValueInfo[PWINFO_AUTHSYST] := 'REDE';
    //ACBrPOS1.ParametrosAdicionais[TerminalId].ValueInfo[PWINFO_INSTALLMENTS] := '3';
    //ACBrPOS1.ParametrosAdicionais[TerminalId].ValueInfo[PWINFO_CARDTYPE] := '1';

    ACBrPOS1.ExecutarTransacaoPagamento(TerminalId, ValorTotal);
    if ACBrPOS1.TEFResp[TerminalId].Debito then
      FormaPagto := CPAG_DEBITO
    else
      FormaPagto := CPAG_CREDITO;

    fAbastecimentos[IndiceAbastec].FormaPagto := FormaPagto;
    fAbastecimentos[IndiceAbastec].ValorPago := ValorTotal;
  except
    on EACBrPOSPGWeb do
    begin
      MsgErro := StringToBinaryString(ACBrPOS1.DadosDaTransacao[TerminalId].ValueInfo[PWINFO_RESULTMSG]);
      if (Trim(MsgErro) = '') then
        MsgErro := 'ERRO AO PROCESSAR' + CR + 'PAGAMENTO EM CARTAO';

      ACBrPOS1.ExibirMensagem(TerminalId, MsgErro, 2);
      raise;
    end;

    on Exception do
      raise;
  end;
end;

function TfrPOSTEFServer.EfetuarPagamentoAbastecDinheiro(
  const TerminalId: String; IndiceAbastec: Integer): Boolean;
var
  ValorPago: Double;
  AStr, MsgTroco: String;
  ValorTotal, Troco: Currency;
  TempoEspera: Integer;
begin
  Result := True;
  ValorTotal := Trunc(fAbastecimentos[IndiceAbastec].Qtd * fBicos[fAbastecimentos[IndiceAbastec].Bico].PrecoUnit * 100) / 100;
  ValorPago := 0;
  while Result and (CompareValue(ValorPago, ValorTotal, 0.001) < 0) do
  begin
    AStr := IntToStr(Trunc(ValorTotal*100));
    AStr := ACBrPOS1.ObterDado(TerminalId, 'VALOR PAGO DINHEIRO', '@@@.@@@,@@', 3, 8,
                                False, False, False, 0, AStr, 3);
    ValorPago := StrToIntDef(OnlyNumber(AStr),0)/100;
    Result := (ValorPago > 0);

    if Result then
    begin
      if (CompareValue(ValorPago, ValorTotal, 0.001) < 0) then
        ACBrPOS1.ExibirMensagem(TerminalId, 'VALOR INFERIOR !' + CR +
                                            'PAGAMENTO MINIMO:' + CR +
                                            FormatFloatBr(ValorTotal), 3)
      else
      begin
        fAbastecimentos[IndiceAbastec].FormaPagto := CPAG_DINHEIRO;
        fAbastecimentos[IndiceAbastec].ValorPago := ValorPago;
        MsgTroco := '';
        TempoEspera := 0;
        if (CompareValue(ValorPago, ValorTotal, 0.001) > 0) then
        begin
          Troco := ValorPago - ValorTotal;
          MsgTroco := CR + '* CONFIRA O TROCO *' + CR +
                           'R$ '+FormatFloatBr(Troco);
          TempoEspera := 3;
        end;

        ACBrPOS1.ExibirMensagem(TerminalId,
          'PAGO.: R$ '+FormatFloatBr(ValorPago) + CR +
          '* D I N H E I R O *' +
          MsgTroco, TempoEspera);
      end;
    end;
  end;
end;

procedure TfrPOSTEFServer.EfetuarDocumentoFiscalAbastec(
  const TerminalId: String; IndiceAbastec: Integer);
begin
  if DeveFazerEmissaoDeNFCe then
    EfetuarNFCeAbastec(TerminalId, IndiceAbastec)
  else if DeveFazerEmissaoDeSAT then
    EfetuarSATAbastec(TerminalId, IndiceAbastec);
end;

procedure TfrPOSTEFServer.EfetuarSATAbastec(const TerminalId: String;
  IndiceAbastec: Integer);
var
  lACBrSAT: TACBrSAT;
begin
  TravarSAT(TerminalId);
  try
    GerarSATAbastec(TerminalId, IndiceAbastec);
    TransmitirSAT(TerminalId);
  finally
    LiberarSAT;
  end;

  lACBrSAT := TACBrSAT.Create(nil);
  try
    lACBrSAT.CFe.AsXMLString := ACBrSAT1.CFe.AsXMLString;
    ImprimirSAT( lACBrSAT, TerminalId );
  finally
    lACBrSAT.Free;
  end;
end;

procedure TfrPOSTEFServer.GerarNFCeAbastec(const TerminalId: String;
  AACBrNFe: TACBrNFe; IndiceAbastec: Integer);
var
  BaseCalculo, BaseCalculoTotal, ValorICMS, ValorICMSTotal, TotalItem, ValorTotalItens: Currency;
  i: Integer;
  APag: TpagCollectionItem;
begin
  {
    *** NOTA ***
    1 - Aqui você deve consultar seu Banco de Dados, Ler o Pedido e calcular os Impostos
        corretamente
  }

  AACBrNFe.NotasFiscais.Clear;
  with AACBrNFe.NotasFiscais.Add.NFe do
  begin
    Ide.natOp     := 'VENDA';
    Ide.indPag    := ipVista;
    Ide.modelo    := 65;
    Ide.serie     := 1;
    Ide.nNF       := ObterProximaNFCe;
    Ide.cNF       := GerarCodigoDFe(Ide.nNF);
    Ide.dEmi      := now;
    Ide.dSaiEnt   := now;
    Ide.hSaiEnt   := now;
    Ide.tpNF      := tnSaida;
    Ide.tpEmis    := teNormal;  // Implementar NFCe OffLine
    Ide.tpAmb     := TpcnTipoAmbiente(cbTipoAmb.ItemIndex);
    Ide.cUF       := StrToInt(pEmitCodUF.Caption);
    Ide.cMunFG    := StrToInt(pEmitCodCidade.Caption);
    Ide.finNFe    := fnNormal;
    Ide.tpImp     := tiNFCe;
    Ide.indFinal  := cfConsumidorFinal;
    Ide.indPres   := pcPresencial;

    //if not swOnLine.IsChecked then
    //begin
    //  Ide.dhCont := date;
    //  Ide.xJust  := 'Justificativa Contingencia';
    //end;

    Emit.CNPJCPF           := OnlyNumber(edtEmitCNPJ.Text);
    Emit.IE                := OnlyNumber(edtEmitIE.Text);
    Emit.IEST              := '';
    Emit.xNome             := edtEmitRazao.Text;
    Emit.xFant             := edtEmitFantasia.Text;
    Emit.CRT := TpcnCRT(cbxTipoEmpresa.ItemIndex);

    Emit.EnderEmit.fone    := edtEmitFone.Text;
    Emit.EnderEmit.CEP     := StrToInt(OnlyNumber(edtEmitCEP.Text));
    Emit.EnderEmit.xLgr    := edtEmitLogradouro.Text;
    Emit.EnderEmit.nro     := edtEmitNumero.Text;
    Emit.EnderEmit.xCpl    := edtEmitComp.Text;
    Emit.EnderEmit.xBairro := edtEmitBairro.Text;
    Emit.EnderEmit.cMun    := StrToInt(pEmitCodCidade.Caption);
    Emit.EnderEmit.xMun    := cbxEmitCidade.Text;
    Emit.EnderEmit.UF      := cbxEmitUF.Text;
    Emit.enderEmit.cPais   := 1058;
    Emit.enderEmit.xPais   := 'BRASIL';

    BaseCalculoTotal := 0;
    ValorICMSTotal := 0;
    ValorTotalItens := 0;
    TotalItem := Trunc(fAbastecimentos[IndiceAbastec].Qtd * fBicos[fAbastecimentos[IndiceAbastec].Bico].PrecoUnit * 100) / 100;
    //Adicionando Produtos
    with Det.New do
    begin
      Prod.nItem    := 1;
      Prod.cProd    := IntToStr(fBicos[fAbastecimentos[IndiceAbastec].Bico].CodProduto);
      Prod.xProd    := fBicos[fAbastecimentos[IndiceAbastec].Bico].Descricao;
      Prod.cEAN     := 'SEM GTIN';
      Prod.NCM      := '27101259';
      Prod.CFOP     := '5656';
      Prod.uCom     := 'LT';
      Prod.qCom     := fAbastecimentos[IndiceAbastec].Qtd;
      Prod.vUnCom   := fBicos[fAbastecimentos[IndiceAbastec].Bico].PrecoUnit;
      Prod.vProd    := TotalItem;

      Prod.cEANTrib  := 'SEM GTIN';
      Prod.uTrib     := 'LT';
      Prod.qTrib     := fAbastecimentos[IndiceAbastec].Qtd;
      Prod.vUnTrib   := fBicos[fAbastecimentos[IndiceAbastec].Bico].PrecoUnit;
      Prod.CEST      := fBicos[fAbastecimentos[IndiceAbastec].Bico].CEST;
      Prod.NCM       := fBicos[fAbastecimentos[IndiceAbastec].Bico].NCM;
//    infAdProd := 'Informação Adicional do Produto';

      with Prod.comb do
      begin
        cProdANP := fBicos[fAbastecimentos[IndiceAbastec].Bico].CodigoANP;
        descANP := fBicos[fAbastecimentos[IndiceAbastec].Bico].Descricao;
        UFcons := cbxEmitUF.Text ;
        with encerrante do
        begin
          nBico := fAbastecimentos[IndiceAbastec].Bico+1;
          nBomba := fBicos[fAbastecimentos[IndiceAbastec].Bico].Bomba;
          nTanque := fBicos[fAbastecimentos[IndiceAbastec].Bico].Tanque;
          vEncIni := fBicos[fAbastecimentos[IndiceAbastec].Bico].Encrerrante;
          vEncFin := vEncIni + fAbastecimentos[IndiceAbastec].Qtd;

          fBicos[fAbastecimentos[IndiceAbastec].Bico].Encrerrante := vEncFin;
        end;
      end;

      with Imposto do
      begin
        // lei da transparencia nos impostos
        vTotTrib := 0;

        with ICMS do
        begin
          if Emit.CRT in [crtSimplesExcessoReceita, crtRegimeNormal] then
            CST := cst60
          else
            CSOSN := csosn500;

          orig  := oeNacional;
          modBC := dbiValorOperacao;

          if Emit.CRT in [crtSimplesExcessoReceita, crtRegimeNormal] then
            BaseCalculo := TotalItem
          else
            BaseCalculo := 0;

          BaseCalculoTotal := BaseCalculoTotal + BaseCalculo;

          vBC     := BaseCalculo;
          pICMS   := 18;

          ValorICMS := vBC * pICMS;
          ValorICMSTotal := ValorICMSTotal + ValorICMS;
          ValorTotalItens := ValorTotalItens + TotalItem;

          vICMS   := ValorICMS;
          modBCST := dbisMargemValorAgregado;

          vBCFCPST := TotalItem;
        end;

        PIS.CST := pis99;
        COFINS.CST := cof99;
      end;
    end;

    Total.ICMSTot.vBC     := BaseCalculoTotal;
    Total.ICMSTot.vICMS   := ValorICMSTotal;
    Total.ICMSTot.vProd   := ValorTotalItens;
    Total.ICMSTot.vNF     := ValorTotalItens;

    Transp.modFrete := mfSemFrete; // NFC-e não pode ter FRETE

    APag := pag.New;
    with APag do
    begin
      indPag := ipVista;
      vPag := fAbastecimentos[IndiceAbastec].ValorPago;

      case fAbastecimentos[IndiceAbastec].FormaPagto of
        CPAG_DEBITO: tPag := fpCartaoDebito;
        CPAG_CREDITO: tPag := fpCartaoCredito;
      else
        tPag :=  fpDinheiro;
      end;

      if tPag in [fpCartaoCredito, fpCartaoDebito] then
      begin
        DeduzirCredenciadoraNFCe(APag, ACBrPOS1.TEFResp[TerminalId]);
        DeduzirBandeiraNFCe(APag, ACBrPOS1.TEFResp[TerminalId]);
      end;
    end;

    if (CompareValue(fAbastecimentos[IndiceAbastec].ValorPago, TotalItem, 0.001) > 0) then
      pag.vTroco := (fAbastecimentos[IndiceAbastec].ValorPago - TotalItem);
  end;

  AACBrNFe.NotasFiscais.GerarNFe;
  AACBrNFe.NotasFiscais.Assinar;
  AACBrNFe.NotasFiscais.GravarXML;
end;

procedure TfrPOSTEFServer.GerarSATAbastec(const TerminalId: String;
  IndiceAbastec: Integer);
var
  TotalItem, TotalGeral: Currency;
  i: Integer;
  APag: TMPCollectionItem;
begin
  TotalGeral := 0;

  ACBrSAT1.InicializaCFe ;

  // Montando uma Venda //
  with ACBrSAT1.CFe do
  begin
    ide.numeroCaixa := seSATNumeroCaixa.Value;
    ide.cNF := Random(999999);

    //Dest.xNome := fAbastecimentos[IndiceAbastec].Nome;

    with Det.New do
    begin
      nItem := 1;
      Prod.EhCombustivel := True;
      Prod.cProd    := IntToStr(fBicos[fAbastecimentos[IndiceAbastec].Bico].CodProduto);
      Prod.xProd    := fBicos[fAbastecimentos[IndiceAbastec].Bico].Descricao;
      Prod.CFOP     := '5656';
      Prod.uCom     := 'LT';
      Prod.qCom     := fAbastecimentos[IndiceAbastec].Qtd;
      Prod.vUnCom   := fBicos[fAbastecimentos[IndiceAbastec].Bico].PrecoUnit;
      Prod.CEST     := fBicos[fAbastecimentos[IndiceAbastec].Bico].CEST;
      Prod.NCM      := fBicos[fAbastecimentos[IndiceAbastec].Bico].NCM;

      TotalItem := RoundABNT((Prod.qCom * Prod.vUnCom) + Prod.vOutro - Prod.vDesc, -2);
      TotalGeral := TotalGeral + TotalItem;
      Imposto.vItem12741 := TotalItem * 0.12;

      with Prod.obsFiscoDet.New do
      begin
        xCampoDet := 'Cod. Produto ANP';
        xTextoDet := IntToStr(fBicos[fAbastecimentos[IndiceAbastec].Bico].CodigoANP);
      end;

      Imposto.ICMS.orig := oeNacional;
      if Emit.cRegTrib = RTSimplesNacional then
        Imposto.ICMS.CSOSN := csosn500
      else
        Imposto.ICMS.CST := cst60;

      Imposto.ICMS.pICMS := 18;
    end;

    APag := Pagto.New;
    APag.vMP := fAbastecimentos[IndiceAbastec].ValorPago;

    case fAbastecimentos[IndiceAbastec].FormaPagto of
      CPAG_DEBITO: APag.cMP := mpCartaodeDebito;
      CPAG_CREDITO: APag.cMP := mpCartaodeCredito;
    else
      APag.cMP := mpDinheiro;
    end;

    if APag.cMP in [mpCartaodeCredito, mpCartaodeDebito] then
      DeduzirCredenciadoraSAT(APag, ACBrPOS1.TEFResp[TerminalId]);
  end;

  ACBrSAT1.CFe.GerarXML( True );    // True = Gera apenas as TAGs da aplicação
end;

procedure TfrPOSTEFServer.EfetuarNFCeAbastec(const TerminalId: String;
  IndiceAbastec: Integer);
var
  lACBrNFe: TACBrNFe;
begin
  lACBrNFe := TACBrNFe.Create(nil);  // cria um novo componente, para essa Thread, e usa ele internamente
  try
    lACBrNFe.Configuracoes.Assign(ACBrNFe1.Configuracoes); // copia as configurações do componente Principal
    GerarNFCeAbastec(TerminalId, lACBrNFe, IndiceAbastec);
    TransmitirNFCe(lACBrNFe, TerminalId);
    ImprimirNFCe(lACBrNFe, TerminalId);
  finally
    lACBrNFe.Free;
  end;
end;

procedure TfrPOSTEFServer.ExecutarFluxoHomologacaoPayGo(const TerminalId: String);
var
  OP: SmallInt;
begin
  OP := 0;
  while (OP < 3) do
  begin
    OP := ACBrPOS1.ExecutarMenu( TerminalId,
                                 'VENDAS|'+
                                 'ADMINISTRATIVO|'+
                                 'CANCELAMENTO|'+
                                 'S A I R',
                                 PadCenter('TESTES HOMOLOGACAO', CACBrPOSPGWebColunasDisplay)
                               );
    case OP of
      0: ExibirTestesVendaHomologacao(TerminalId);
      1: EfetuarAdministrativo(TerminalId);
      2: EfetuarCancelamento(TerminalId);
    end;
  end;
end;

procedure TfrPOSTEFServer.ExibirTestesVendaHomologacao(const TerminalId: String);
var
  OP: SmallInt;
  ValorVenda: Currency;
  TipoCartao: Char;
  Parcelas: Integer;
begin
  ValorVenda := 0;
  TipoCartao := '1';   // credito
  Parcelas := 1;

  OP := ACBrPOS1.ExecutarMenu( TerminalId,
                               'VENDA CRED   84,00|'+
                               'VENDA CRED 1500,00|'+
                               'VENDA DEB   200,00|'+
                               'VENDA CRED    1,53|'+
                               'V O L T A R',
                               PadCenter('TESTES VENDA', CACBrPOSPGWebColunasDisplay)
                             );
  case OP of
    0:
      begin
        ValorVenda := 84;
        Parcelas := 3;
      end;
    1:
      ValorVenda := 1500;
    2:
      begin
        ValorVenda := 200;
        TipoCartao := '2';  // Debito
      end;
    3:
      ValorVenda := 1.53;
  else
    Exit;
  end;

  ACBrPOS1.ParametrosAdicionais[TerminalId].Clear;
  ACBrPOS1.ParametrosAdicionais[TerminalId].ValueInfo[PWINFO_CARDTYPE] := TipoCartao;
  if (Parcelas = 1) then
    ACBrPOS1.ParametrosAdicionais[TerminalId].ValueInfo[PWINFO_FINTYPE] := '1' //1: à vista
  else
  begin
    ACBrPOS1.ParametrosAdicionais[TerminalId].ValueInfo[PWINFO_INSTALLMENTS] := IntToStr(Parcelas);
    ACBrPOS1.ParametrosAdicionais[TerminalId].ValueInfo[PWINFO_FINTYPE] := '2'; //2: parcelado pelo emissor
  end;

  ACBrPOS1.ExecutarTransacaoPagamento(TerminalId, ValorVenda, prnAmbas);
end;

procedure TfrPOSTEFServer.ExecutarReimpressao(const TerminalId: String);
begin
  try
    ACBrPOS1.ImprimirComprovantesTEF(TerminalId, prnAmbas, False);
  except
    On E: Exception do
    begin
      ACBrPOS1.Beep(TerminalId, beepAlerta);
      ACBrPOS1.ExibirMensagem(TerminalId, E.Message, 5);
    end;
  end;
end;

procedure TfrPOSTEFServer.EfetuarAdministrativo(const TerminalId: String);
begin
  ACBrPOS1.ParametrosAdicionais[TerminalId].Clear;
  ACBrPOS1.ExecutarTransacaoTEF(TerminalId, operAdmin);
end;

procedure TfrPOSTEFServer.EfetuarCancelamento(const TerminalId: String);
begin
  ACBrPOS1.ParametrosAdicionais[TerminalId].Clear;
  ACBrPOS1.ExecutarTransacaoTEF(TerminalId, operCancelarVenda);
end;

procedure TfrPOSTEFServer.EfetuarMultiplosPagamento(const TerminalId: String;
  IndicePedido: Integer);

  function PerguntarValorAPagar(const TerminalId: String): Double;
  var
    AStr: String;
  begin
    AStr := ACBrPOS1.ObterDado(TerminalId, 'VALOR A PAGAR', '@@@.@@@,@@', 3, 8);
    Result := StrToIntDef(OnlyNumber(AStr),0)/100;
  end;

var
  SaldoRestante, ValorPorPessoa, ValorAPagar: Double;
  OPPagto, OPValor: SmallInt;
  SL: TStringList;
  TituloMenu, TipoPagamento: String;
begin
  SL := TStringList.Create;
  try
    SaldoRestante := fPedidos[IndicePedido].ValorTotal;
    while (SaldoRestante > 0) do
    begin
      SL.Clear;
      SL.Add('CARTAO');
      SL.Add('DINHEIRO');
      SL.Add('CANCELAR');

      if (fPedidos[IndicePedido].Pessoas < 2) then  // Apenas 1 pessoa
        TituloMenu := 'EFETUAR PAGAMENTO'
      else
        TituloMenu := 'Saldo '+FormatFloatBr(SaldoRestante)+
                      ' Pag '+IntToStr(Length(fPedidos[IndicePedido].Pagamentos)+1)+
                      '/'+IntToStr(fPedidos[IndicePedido].Pessoas);

      OPPagto := ACBrPOS1.ExecutarMenu(TerminalId, SL, TituloMenu);
      if (OPPagto < 0) or (OPPagto > 1) then
        raise EFluxoInterrompido.Create('');

      TipoPagamento := SL[OPPagto];
      if (fPedidos[IndicePedido].Pessoas < 2) or  // Apenas 1 pessoa
         (Length(fPedidos[IndicePedido].Pagamentos) >= fPedidos[IndicePedido].Pessoas-1) then  // Só falta 1 pessoa
        ValorAPagar := SaldoRestante

      else
      begin
        ValorPorPessoa := RoundTo(fPedidos[IndicePedido].ValorTotal / fPedidos[IndicePedido].Pessoas, -2);

        SL.Clear;
        SL.Add('PESSOAL.: '+FormatFloatBr(ValorPorPessoa));
        SL.Add('RESTANTE: '+FormatFloatBr(SaldoRestante));
        SL.Add('OUTRO VALOR');
        SL.Add('VOLTAR');

        OPValor := ACBrPOS1.ExecutarMenu(TerminalId, SL, PadCenter(TipoPagamento + ' VALOR ?', CACBrPOSPGWebColunasDisplay) );
        if (OPValor < 0) or (OPValor > 2) then
          Continue;

        case OPValor of
          0: ValorAPagar := ValorPorPessoa;
          1: ValorAPagar := SaldoRestante;
          2:
          begin
            ValorAPagar := PerguntarValorAPagar(TerminalId);
            if (ValorAPagar <= 0) then
              Continue;
          end;
        end;
      end;

      case OPPagto of
        0:
        try
          EfetuarPagamentoCartao(TerminalId, IndicePedido, ValorAPagar);
        except
        end;

        1:
        begin
          if (ValorAPagar = SaldoRestante) then
          begin
            if not EfetuarPagamentoDinheiro(TerminalId, IndicePedido, ValorAPagar) then
              Continue;
          end
          else
            IncluirPagamentoPedidoDinheiro(TerminalId, IndicePedido, ValorAPagar);

          ACBrPOS1.AguardarTecla(TerminalId, 2);
        end;
      end;

      SaldoRestante := fPedidos[IndicePedido].ValorTotal - fPedidos[IndicePedido].TotalPago;
    end;
  finally
    SL.Free;
  end;
end;

procedure TfrPOSTEFServer.EfetuarPagamento(const TerminalId: String;
  IndicePedido: Integer);
var
  OP: SmallInt;
begin
  OP := ACBrPOS1.ExecutarMenu(TerminalId, 'CARTAO OU PIX|DINHEIRO|CANCELAR', 'EFETUAR O PAGAMENTO');

  case OP of
    0: EfetuarPagamentoCartao(TerminalId, IndicePedido, fPedidos[IndicePedido].ValorTotal);
    1:
    begin
      if not EfetuarPagamentoDinheiro(TerminalId, IndicePedido, fPedidos[IndicePedido].ValorTotal) then
        raise EFluxoInterrompido.Create('');
    end
  else
    raise EFluxoInterrompido.Create('');
  end;
end;

procedure TfrPOSTEFServer.EfetuarPagamentoCartao(const TerminalId: String;
  IndicePedido: Integer; Valor: Double);
var
  FormaPagto: Byte;
  MsgErro: String;
begin
  try
    ACBrPOS1.ParametrosAdicionais[TerminalId].Clear;
    //ACBrPOS1.ParametrosAdicionais[TerminalId].ValueInfo[PWINFO_AUTHSYST] := 'REDE';
    //ACBrPOS1.ParametrosAdicionais[TerminalId].ValueInfo[PWINFO_INSTALLMENTS] := '3';
    //ACBrPOS1.ParametrosAdicionais[TerminalId].ValueInfo[PWINFO_CARDTYPE] := '1';
    ACBrPOS1.ParametrosAdicionais[TerminalId].ValueInfo[PWINFO_FINTYPE] := '01'; //01: à vista
    ACBrPOS1.ExecutarTransacaoPagamento(TerminalId, Valor);
    if ACBrPOS1.TEFResp[TerminalId].Debito then
      FormaPagto := CPAG_DEBITO
    else
      FormaPagto := CPAG_CREDITO;

    IncluirPagamentoPedido(IndicePedido, FormaPagto, Valor);
  except
    on EACBrPOSPGWeb do
    begin
      MsgErro := StringToBinaryString(ACBrPOS1.DadosDaTransacao[TerminalId].ValueInfo[PWINFO_RESULTMSG]);
      if (Trim(MsgErro) = '') then
        MsgErro := 'ERRO AO PROCESSAR' + CR + 'PAGAMENTO EM CARTAO';

      ACBrPOS1.ExibirMensagem(TerminalId, MsgErro, 2);
      raise;
    end;

    on Exception do
      raise;
  end;
end;

function TfrPOSTEFServer.EfetuarPagamentoDinheiro(const TerminalId: String;
  IndicePedido: Integer; ValorMinimo: Double): Boolean;
var
  ValorPago: Double;
  AStr: String;
begin
  Result := True;
  ValorPago := 0;
  while Result and (CompareValue(ValorPago, ValorMinimo, 0.001) < 0) do
  begin
    AStr := IntToStr(Trunc(ValorMinimo*100));
    AStr := ACBrPOS1.ObterDado(TerminalId, 'VALOR PAGO DINHEIRO', '@@@.@@@,@@', 3, 8,
                                False, False, False, 0, AStr, 3);
    ValorPago := StrToIntDef(OnlyNumber(AStr),0)/100;
    Result := (ValorPago > 0);

    if Result  then
    begin
      if (CompareValue(ValorPago, ValorMinimo, 0.001) < 0) then
        ACBrPOS1.ExibirMensagem(TerminalId, 'VALOR INFERIOR !' + CR +
                                            'PAGAMENTO MINIMO:' + CR +
                                            FormatFloatBr(ValorMinimo), 3)
      else
        IncluirPagamentoPedidoDinheiro(TerminalId, IndicePedido, ValorPago);
    end;
  end;
end;

procedure TfrPOSTEFServer.IncluirPagamentoPedido(IndicePedido: Integer;
  TipoPagamento: Byte; ValorPago: Double);
var
  i: Integer;
begin
  i := Length(fPedidos[IndicePedido].Pagamentos);
  SetLength(fPedidos[IndicePedido].Pagamentos, i+1);
  fPedidos[IndicePedido].Pagamentos[i].FormaPagto := TipoPagamento;
  fPedidos[IndicePedido].Pagamentos[i].ValorPago := ValorPago;
  fPedidos[IndicePedido].TotalPago := fPedidos[IndicePedido].TotalPago + ValorPago;
end;

procedure TfrPOSTEFServer.IncluirPagamentoPedidoDinheiro(
  const TerminalId: String; IndicePedido: Integer; ValorPago: Double);
var
  MsgTroco: String;
  Troco: Double;
  TempoEspera: Integer;
begin
  IncluirPagamentoPedido(IndicePedido, CPAG_DINHEIRO, ValorPago);
  MsgTroco := '';
  TempoEspera := 0;
  if fPedidos[IndicePedido].TotalPago > fPedidos[IndicePedido].ValorTotal then
  begin
    Troco := fPedidos[IndicePedido].TotalPago - fPedidos[IndicePedido].ValorTotal;
    TempoEspera := 3;
    MsgTroco := CR + '* CONFIRA O TROCO *' + CR +
                     'R$ '+FormatFloatBr(Troco);
  end;

  ACBrPOS1.ExibirMensagem(TerminalId,
    'PAGO.: R$ '+FormatFloatBr(ValorPago) + CR +
    '* D I N H E I R O *' +
    MsgTroco, TempoEspera);
end;

procedure TfrPOSTEFServer.ImprimirNFCe(AACBrNFe: TACBrNFe;
  const TerminalId: String);
var
  lACBrPosPrinter: TACBrPosPrinter;
  lACBrPOSPGWebPrinter: TACBrPOSPGWebPrinter;
  lACBrNFeDANFeESCPOS: TACBrNFeDANFeESCPOS;
begin
  if (AACBrNFe.NotasFiscais.Count < 1) then
    Exit;

  ACBrPOS1.ExibirMensagem( TerminalId, PadCenter( 'IMPRIMINDO', CACBrPOSPGWebColunasDisplay) +
                                       PadCenter( 'NFC-e', CACBrPOSPGWebColunasDisplay) );

  lACBrPosPrinter := ACBrPosPrinterFactory;
  try
    lACBrPOSPGWebPrinter := ACBrPOSPGWebPrinterFactory(TerminalId, lACBrPosPrinter, ACBrPOS1);
    try
      lACBrNFeDANFeESCPOS := TACBrNFeDANFeESCPOS.Create(nil);
      try
        lACBrNFeDANFeESCPOS.PosPrinter := lACBrPosPrinter;
        lACBrNFeDANFeESCPOS.SuportaCondensado := False;
        lACBrNFeDANFeESCPOS.ImprimeLogoLateral := False;
        lACBrNFeDANFeESCPOS.ImprimeQRCodeLateral := False;

        AACBrNFe.DANFE := lACBrNFeDANFeESCPOS;
        AACBrNFe.DANFE.ImprimirDANFE();
      finally
        AACBrNFe.DANFE := Nil;
        lACBrNFeDANFeESCPOS.Free;
      end;
    finally
      lACBrPosPrinter.ModeloExterno := nil;
      lACBrPOSPGWebPrinter.Free;
    end;
  finally
    lACBrPosPrinter.Free;
  end;
end;

procedure TfrPOSTEFServer.EfetuarSAT(const TerminalId: String;
  IndicePedido: Integer);
var
  lACBrSAT: TACBrSAT;
begin
  TravarSAT(TerminalId);
  try
    GerarSAT(TerminalId, IndicePedido);
    TransmitirSAT(TerminalId);
  finally
    LiberarSAT;
  end;

  lACBrSAT := TACBrSAT.Create(nil);
  try
    lACBrSAT.CFe.AsXMLString := ACBrSAT1.CFe.AsXMLString;
    ImprimirSAT( lACBrSAT, TerminalId );
    EnviarEmailSAT( lACBrSAT, TerminalId, fPedidos[IndicePedido].Email );
  finally
    lACBrSAT.Free;
  end;
end;

procedure TfrPOSTEFServer.TravarSAT(const TerminalId: String);
begin
  {
    *** NOTA ***
    A Chamada a "fcsDocFiscal.Acquire", garante que não teremos dois terminais
    POS TEF, tentanto emitir no SAT, ao mesmo Tempo...  }

  ACBrPOS1.ExibirMensagem( TerminalId, PadCenter( 'AGUARDANDO', CACBrPOSPGWebColunasDisplay) +
                                       PadCenter( 'LIBERAÇÃO DO SAT', CACBrPOSPGWebColunasDisplay) );
  fcsDocFiscal.Acquire;
  ACBrPOS1.ExibirMensagem( TerminalId, PadCenter( 'GERANDO', CACBrPOSPGWebColunasDisplay) +
                                       PadCenter( 'SAT-CFe', CACBrPOSPGWebColunasDisplay) );
end;

procedure TfrPOSTEFServer.GerarSAT(const TerminalId: String;
  IndicePedido: Integer);
var
  TotalItem, TotalGeral: Currency;
  i: Integer;
  APag: TMPCollectionItem;
begin
  TotalGeral := 0;

  ACBrSAT1.InicializaCFe ;

  // Montando uma Venda //
  with ACBrSAT1.CFe do
  begin
    ide.numeroCaixa := seSATNumeroCaixa.Value;
    ide.cNF := Random(999999);

    Dest.xNome := fPedidos[IndicePedido].Nome;

    for i := 0 to Length(fPedidos[IndicePedido].Items)-1 do
    begin
      with Det.New do
      begin
        nItem := i+1;
        Prod.cProd    := IntToStr(fPedidos[IndicePedido].Items[i].CodItem);
        Prod.xProd    := fPedidos[IndicePedido].Items[i].Descricao;
        Prod.CFOP     := '5102';
        Prod.uCom     := fPedidos[IndicePedido].Items[i].UN;
        Prod.qCom     := fPedidos[IndicePedido].Items[i].Qtd;
        Prod.vUnCom   := fPedidos[IndicePedido].Items[i].PrecoUnit;
        Prod.indRegra := irArredondamento;

        TotalItem := RoundABNT((Prod.qCom * Prod.vUnCom) + Prod.vOutro - Prod.vDesc, -2);
        TotalGeral := TotalGeral + TotalItem;
        Imposto.vItem12741 := TotalItem * 0.12;

        Imposto.ICMS.orig := oeNacional;
        if Emit.cRegTrib = RTSimplesNacional then
          Imposto.ICMS.CSOSN := csosn102
        else
          Imposto.ICMS.CST := cst00;

        Imposto.ICMS.pICMS := 18;
      end;
    end;

    for i := 0 to Length(fPedidos[IndicePedido].Pagamentos)-1 do
    begin
      APag := Pagto.New;
      APag.vMP := fPedidos[IndicePedido].Pagamentos[i].ValorPago;

      case fPedidos[IndicePedido].Pagamentos[i].FormaPagto of
        CPAG_DEBITO: APag.cMP := mpCartaodeDebito;
        CPAG_CREDITO: APag.cMP := mpCartaodeCredito;
      else
        APag.cMP := mpDinheiro;
      end;

      if APag.cMP in [mpCartaodeCredito, mpCartaodeDebito] then
        DeduzirCredenciadoraSAT(APag, ACBrPOS1.TEFResp[TerminalId]);
    end;
 end;

  ACBrSAT1.CFe.GerarXML( True );    // True = Gera apenas as TAGs da aplicação
end;

procedure TfrPOSTEFServer.TransmitirSAT(const TerminalId: String);
begin
  ACBrPOS1.ExibirMensagem( TerminalId, PadCenter( 'TRANSMITINDO', CACBrPOSPGWebColunasDisplay) +
                                       PadCenter( 'SAT CF-e', CACBrPOSPGWebColunasDisplay) );
  ACBrSAT1.EnviarDadosVenda;
  if (ACBrSAT1.Resposta.codigoDeRetorno <> 6000) then
    raise EACBrSATErro.Create(ACBrSAT1.Resposta.mensagemRetorno);
end;

procedure TfrPOSTEFServer.LiberarSAT;
begin
  fcsDocFiscal.Release;
end;

procedure TfrPOSTEFServer.ImprimirSAT(AACBrSAT: TACBrSAT;
  const TerminalId: String);
var
  lACBrPosPrinter: TACBrPosPrinter;
  lACBrPOSPGWebPrinter: TACBrPOSPGWebPrinter;
  lACBrSATExtratoESCPOS: TACBrSATExtratoESCPOS;
begin
  ACBrPOS1.ExibirMensagem( TerminalId, PadCenter( 'IMPRIMINDO', CACBrPOSPGWebColunasDisplay) +
                                       PadCenter( 'SAT CF-e', CACBrPOSPGWebColunasDisplay) );

  lACBrPosPrinter := ACBrPosPrinterFactory;
  try
    lACBrPOSPGWebPrinter := ACBrPOSPGWebPrinterFactory(TerminalId, lACBrPosPrinter, ACBrPOS1);
    try
      lACBrSATExtratoESCPOS := TACBrSATExtratoESCPOS.Create(nil);
      try
        lACBrSATExtratoESCPOS.PosPrinter := lACBrPosPrinter;
        lACBrSATExtratoESCPOS.ImprimeLogoLateral := False;
        lACBrSATExtratoESCPOS.ImprimeQRCodeLateral := False;

        AACBrSAT.Extrato := lACBrSATExtratoESCPOS;
        AACBrSAT.Extrato.ImprimirExtrato();
      finally
        AACBrSAT.Extrato := Nil;
        lACBrSATExtratoESCPOS.Free;
      end;
    finally
      lACBrPosPrinter.ModeloExterno := nil;
      lACBrPOSPGWebPrinter.Free;
    end;
  finally
    lACBrPosPrinter.Free;
  end;
end;

procedure TfrPOSTEFServer.EnviarEmailSAT(AACBrSAT: TACBrSAT; const TerminalId,
  AEmail: String);

  function SubstituirVariaveisEmail(const AText: String): String;
  begin
    Result := StringReplace(AText, '[EmitFantasia]',
                           AACBrSAT.CFe.Emit.xFant, [rfReplaceAll]);
    Result := StringReplace(Result, '[dEmissao]',
                           DateToStr(AACBrSAT.CFe.ide.dEmi), [rfReplaceAll]);
    Result := StringReplace(Result, '[ChaveDFe]',
                           AACBrSAT.CFe.infCFe.ID, [rfReplaceAll]);
  end;

var
  SL, AT: TStringList;
  ASubject: String;
  lACBrSATExtratoFortes: TACBrSATExtratoFortes;
begin
  if not DeveFazerEnvioDeEmail then
    Exit;

  if (Trim(AEmail) = '') then
    Exit;

  ACBrPOS1.ExibirMensagem( TerminalId, PadCenter( 'ENVIANDO', CACBrPOSPGWebColunasDisplay) +
                                       PadCenter( 'SAT CF-e', CACBrPOSPGWebColunasDisplay) +
                                       PadCenter( 'POR EMAIL', CACBrPOSPGWebColunasDisplay) +
                                       PadCenter( AEmail, CACBrPOSPGWebColunasDisplay) );
  SL := TStringList.Create;
  AT := TStringList.Create;
  try
    SL.Text := SubstituirVariaveisEmail(mEmailMensagem.Lines.Text);
    ASubject := SubstituirVariaveisEmail(edtEmailAssunto.Text);

    lACBrSATExtratoFortes := TACBrSATExtratoFortes.Create(nil);
    try
      lACBrSATExtratoFortes.Filtro := fiPDF;
      lACBrSATExtratoFortes.NomeDocumento := ApplicationPath + 'pdf' + PathDelim +
                                             'AD' + AACBrSAT.CFe.infCFe.ID + '.pdf';;
      AACBrSAT.Extrato := lACBrSATExtratoFortes;  // EscPos não gera PDF
      AACBrSAT.ImprimirExtrato;
      if (AACBrSAT.Extrato.ArquivoPDF <> '') and FileExists(AACBrSAT.Extrato.ArquivoPDF) then
      begin
        AT.Add(AACBrSAT.Extrato.ArquivoPDF);

        fcsMailBuild.Acquire;
        try
          AACBrSAT.MAIL := ACBrMail1;  // ACBrMail está enviando por Thread
          AACBrSAT.EnviarEmail( AEmail, ASubject,
            SL,       // Corpo do Email
            nil,      // Lista com emails copias - TStrings
            AT);      // Lista de anexos - (PDF)
        finally
          fcsMailBuild.Release;
        end;
      end;
    finally
      AACBrSAT.Extrato := nil;
      lACBrSATExtratoFortes.Free;
    end;
  finally
    SL.Free;
    AT.Free;
  end;
end;

procedure TfrPOSTEFServer.EnviarEmailNFCe(AACBrNFe: TACBrNFe; const TerminalId,
  AEmail: String);

  function SubstituirVariaveisEmail(const AText: String): String;
  begin
    Result := StringReplace(AText, '[EmitFantasia]',
                           AACBrNFe.NotasFiscais[0].NFe.Emit.xFant, [rfReplaceAll]);
    Result := StringReplace(Result, '[dEmissao]',
                           DateToStr(AACBrNFe.NotasFiscais[0].NFe.Ide.dEmi), [rfReplaceAll]);
    Result := StringReplace(Result, '[ChaveDFe]',
                           AACBrNFe.NotasFiscais[0].NFe.infNFe.ID, [rfReplaceAll]);
  end;

var
  SL: TStringList;
  ASubject: String;
  lACBrNFeDANFCeFortes: TACBrNFeDANFCeFortes;
begin
  if not DeveFazerEnvioDeEmail then
    Exit;

  if (AACBrNFe.NotasFiscais.Count < 1) then
    Exit;

  if (Trim(AEmail) = '') then
    Exit;

  ACBrPOS1.ExibirMensagem( TerminalId, PadCenter( 'ENVIANDO', CACBrPOSPGWebColunasDisplay) +
                                       PadCenter( 'NFC-e', CACBrPOSPGWebColunasDisplay) +
                                       PadCenter( 'POR EMAIL', CACBrPOSPGWebColunasDisplay) +
                                       PadCenter( AEmail, CACBrPOSPGWebColunasDisplay) );
  SL := TStringList.Create;
  try
    SL.Text := SubstituirVariaveisEmail(mEmailMensagem.Lines.Text);
    ASubject := SubstituirVariaveisEmail(edtEmailAssunto.Text);

    lACBrNFeDANFCeFortes := TACBrNFeDANFCeFortes.Create(nil);
    try
      AACBrNFe.DANFE := lACBrNFeDANFCeFortes;  // EscPos não gera PDF
      fcsMailBuild.Acquire;
      try
        AACBrNFe.MAIL := ACBrMail1;    // ACBrMail está enviando por Thread
        AACBrNFe.NotasFiscais.Items[0].EnviarEmail( AEmail, ASubject,
          SL,     // Corpo do Email
          True,   // Enviar PDF
          nil,    // Lista com emails copias - TStrings
          nil     // Lista de anexos - TStrings
        );
      finally
        fcsMailBuild.Release;
      end;
    finally
      lACBrNFeDANFCeFortes.Free;
    end;
  finally
    SL.Free;
  end;
end;

procedure TfrPOSTEFServer.IncluirPedidosSimuladosLoja;
begin
  SetLength(fPedidos, 4);
  fPedidos[0].NumPedido := 98;
  fPedidos[0].DataHora := IncMinute(Now, -40);
  fPedidos[0].Nome := 'WALTER WHITE';
  fPedidos[0].Email := 'w.white@jpwynnehs.edu';
  fPedidos[0].ValorTotal := 9.00;
  fPedidos[0].TotalPago := 0;
  fPedidos[0].Pessoas := 0;
  SetLength(fPedidos[0].Items, 1);
  fPedidos[0].Items[0].CodItem := 100;
  fPedidos[0].Items[0].Descricao := 'SACOS PLASTICOS 100G';
  fPedidos[0].Items[0].PrecoUnit := 9.00;
  fPedidos[0].Items[0].Qtd := 1;
  fPedidos[0].Items[0].UN := 'UN';
  SetLength(fPedidos[0].Pagamentos, 0);

  fPedidos[1].NumPedido := 102;
  fPedidos[1].DataHora := IncMinute(Now, -30);
  fPedidos[1].Nome := 'HOMER SIMPSON';
  fPedidos[1].Email := 'homer.simpson@fox.com';
  fPedidos[1].ValorTotal := 40;
  fPedidos[1].TotalPago := 0;
  fPedidos[1].Pessoas := 0;
  SetLength(fPedidos[1].Items, 2);
  fPedidos[1].Items[0].CodItem := 10;
  fPedidos[1].Items[0].Descricao := 'DUFF BEER 12 PACK';
  fPedidos[1].Items[0].PrecoUnit := 30;
  fPedidos[1].Items[0].Qtd := 1;
  fPedidos[1].Items[0].UN := 'UN';
  fPedidos[1].Items[1].CodItem := 21;
  fPedidos[1].Items[1].Descricao := 'KRUSTY NACHOS HOT';
  fPedidos[1].Items[1].PrecoUnit := 10;
  fPedidos[1].Items[1].Qtd := 1;
  fPedidos[1].Items[1].UN := 'PC';
  SetLength(fPedidos[1].Pagamentos, 0);

  fPedidos[2].NumPedido := 113;
  fPedidos[2].DataHora := IncMinute(Now, -20);
  fPedidos[2].Nome := 'ODORICO PARAGUAÇU';
  fPedidos[2].Email := '';
  fPedidos[2].ValorTotal := 115;
  fPedidos[2].TotalPago := 0;
  fPedidos[2].Pessoas := 0;
  SetLength(fPedidos[2].Items, 3);
  fPedidos[2].Items[0].CodItem := 200;
  fPedidos[2].Items[0].Descricao := 'CANETAS LUXO AZUL';
  fPedidos[2].Items[0].PrecoUnit := 10;
  fPedidos[2].Items[0].Qtd := 4;
  fPedidos[2].Items[0].UN := 'UN';
  fPedidos[2].Items[1].CodItem := 201;
  fPedidos[2].Items[1].Descricao := 'PAPEL A4';
  fPedidos[2].Items[1].PrecoUnit := 5;
  fPedidos[2].Items[1].Qtd := 5;
  fPedidos[2].Items[1].UN := 'UN';
  fPedidos[2].Items[2].CodItem := 300;
  fPedidos[2].Items[2].Descricao := 'LICOR DE JENIPAPO';
  fPedidos[2].Items[2].PrecoUnit := 50;
  fPedidos[2].Items[2].Qtd := 1;
  fPedidos[2].Items[2].UN := 'LT';
  SetLength(fPedidos[2].Pagamentos, 0);

  fPedidos[3].NumPedido := 212;
  fPedidos[3].DataHora := IncMinute(Now, -10);
  fPedidos[3].Nome := 'LARA CROFT';
  fPedidos[3].Email := 'lara.croft@riders.com';
  fPedidos[3].ValorTotal := 25.50;
  fPedidos[3].TotalPago := 0;
  fPedidos[3].Pessoas := 0;
  SetLength(fPedidos[3].Items, 2);
  fPedidos[3].Items[0].CodItem := 101;
  fPedidos[3].Items[0].Descricao := 'CAMISA HERING';
  fPedidos[3].Items[0].PrecoUnit := 25;
  fPedidos[3].Items[0].Qtd := 1;
  fPedidos[3].Items[0].UN := 'UN';
  fPedidos[3].Items[1].CodItem := 500;
  fPedidos[3].Items[1].Descricao := 'ELASTICOS';
  fPedidos[3].Items[1].PrecoUnit := 0.50;
  fPedidos[3].Items[1].Qtd := 1;
  fPedidos[3].Items[1].UN := 'UN';
  SetLength(fPedidos[3].Pagamentos, 0);
end;

procedure TfrPOSTEFServer.IncluirPedidosSimuladosRestaurante;
begin
  SetLength(fPedidos, 4);
  fPedidos[0].NumPedido := 98;
  fPedidos[0].DataHora := IncMinute(Now, -40);
  fPedidos[0].Nome := 'MESA 045';
  fPedidos[0].Email := '';
  fPedidos[0].ValorTotal := 25;
  fPedidos[0].TotalPago := 0;
  fPedidos[0].Pessoas := 1;
  SetLength(fPedidos[0].Items, 1);
  fPedidos[0].Items[0].CodItem := 100;
  fPedidos[0].Items[0].Descricao := 'PIZZA BROTO PEPPERONI';
  fPedidos[0].Items[0].PrecoUnit := 25;
  fPedidos[0].Items[0].Qtd := 1;
  fPedidos[0].Items[0].UN := 'UN';
  SetLength(fPedidos[0].Pagamentos, 0);

  fPedidos[1].NumPedido := 102;
  fPedidos[1].DataHora := IncMinute(Now, -30);
  fPedidos[1].Nome := 'MESA 100';
  fPedidos[1].Email := '';
  fPedidos[1].ValorTotal := 75;
  fPedidos[1].TotalPago := 0;
  fPedidos[1].Pessoas := 1;
  SetLength(fPedidos[1].Items, 2);
  fPedidos[1].Items[0].CodItem := 100;
  fPedidos[1].Items[0].Descricao := 'PIZZA BROTO PEPPERONI';
  fPedidos[1].Items[0].PrecoUnit := 25;
  fPedidos[1].Items[0].Qtd := 1;
  fPedidos[1].Items[0].UN := 'UN';
  fPedidos[1].Items[1].CodItem := 201;
  fPedidos[1].Items[1].Descricao := 'PIZZA GRANDE MARGUERITA';
  fPedidos[1].Items[1].PrecoUnit := 50;
  fPedidos[1].Items[1].Qtd := 1;
  fPedidos[1].Items[1].UN := 'PC';
  SetLength(fPedidos[1].Pagamentos, 0);

  fPedidos[2].NumPedido := 113;
  fPedidos[2].DataHora := IncMinute(Now, -20);
  fPedidos[2].Nome := 'MESA 005';
  fPedidos[2].Email := '';
  fPedidos[2].ValorTotal := 115;
  fPedidos[2].TotalPago := 0;
  fPedidos[2].Pessoas := 1;
  SetLength(fPedidos[2].Items, 3);
  fPedidos[2].Items[0].CodItem := 200;
  fPedidos[2].Items[0].Descricao := 'PIZZA GRANDE PEPPERONI';
  fPedidos[2].Items[0].PrecoUnit := 50;
  fPedidos[2].Items[0].Qtd := 1;
  fPedidos[2].Items[0].UN := 'UN';
  fPedidos[2].Items[1].CodItem := 201;
  fPedidos[2].Items[1].Descricao := 'PIZZA GRANDE MARGUERITA';
  fPedidos[2].Items[1].PrecoUnit := 50;
  fPedidos[2].Items[1].Qtd := 1;
  fPedidos[2].Items[1].UN := 'UN';
  fPedidos[2].Items[2].CodItem := 300;
  fPedidos[2].Items[2].Descricao := 'COCA PET 1,5';
  fPedidos[2].Items[2].PrecoUnit := 15;
  fPedidos[2].Items[2].Qtd := 1;
  fPedidos[2].Items[2].UN := 'LT';
  SetLength(fPedidos[2].Pagamentos, 0);

  fPedidos[3].NumPedido := 212;
  fPedidos[3].DataHora := IncMinute(Now, -10);
  fPedidos[3].Nome := 'MESA 022';
  fPedidos[3].Email := '';
  fPedidos[3].ValorTotal := 25.50;
  fPedidos[3].TotalPago := 0;
  fPedidos[3].Pessoas := 1;
  SetLength(fPedidos[3].Items, 2);
  fPedidos[3].Items[0].CodItem := 101;
  fPedidos[3].Items[0].Descricao := 'PIZZA BROTO MARGUERITA';
  fPedidos[3].Items[0].PrecoUnit := 25;
  fPedidos[3].Items[0].Qtd := 1;
  fPedidos[3].Items[0].UN := 'UN';
  fPedidos[3].Items[1].CodItem := 500;
  fPedidos[3].Items[1].Descricao := 'CHICLETE ADAMS';
  fPedidos[3].Items[1].PrecoUnit := 0.50;
  fPedidos[3].Items[1].Qtd := 1;
  fPedidos[3].Items[1].UN := 'UN';
  SetLength(fPedidos[3].Pagamentos, 0);
end;

procedure TfrPOSTEFServer.IncluirAbastecimentosSimulados;
begin
  SetLength(fBicos, 3);
  fBicos[0].CodProduto := 1;
  fBicos[0].Descricao := 'GASOLINA COMUM';
  fBicos[0].CodigoANP := 320101001;
  fBicos[0].CEST := '0600200';
  fBicos[0].NCM := '27101259';
  fBicos[0].PrecoUnit := 5.00;
  fBicos[0].Bomba := 1;
  fBicos[0].Tanque := 1;
  fBicos[0].Encrerrante := 100;

  fBicos[1].CodProduto := 2;
  fBicos[1].Descricao := 'ETANOL COMUM';
  fBicos[1].CodigoANP := 810101001;
  fBicos[1].CEST := '0600100';
  fBicos[1].NCM := '22071090';
  fBicos[1].PrecoUnit := 3.00;
  fBicos[1].Bomba := 2;
  fBicos[1].Tanque := 2;
  fBicos[1].Encrerrante := 1000;

  fBicos[2].CodProduto := 3;
  fBicos[2].Descricao := 'DIESEL BS10';
  fBicos[2].CodigoANP := 820101034;
  fBicos[2].CEST := '0600600';
  fBicos[2].NCM := '27101921';
  fBicos[2].PrecoUnit := 2.00;
  fBicos[2].Bomba := 3;
  fBicos[2].Tanque := 3;
  fBicos[2].Encrerrante := 2000;

  SetLength(fAbastecimentos, 6);
  fAbastecimentos[0].DataHora := IncMinute(Now,-80);
  fAbastecimentos[0].Sequencia := 1;
  fAbastecimentos[0].Bico := 0;
  fAbastecimentos[0].Qtd := 6;
  fAbastecimentos[0].FormaPagto := 0;
  fAbastecimentos[0].ValorPago := 0;

  fAbastecimentos[1].DataHora := IncMinute(Now,-70);
  fAbastecimentos[1].Sequencia := 2;
  fAbastecimentos[1].Bico := 1;
  fAbastecimentos[1].Qtd := 15;
  fAbastecimentos[1].FormaPagto := 0;
  fAbastecimentos[1].ValorPago := 0;

  fAbastecimentos[2].DataHora := IncMinute(Now,-50);
  fAbastecimentos[2].Sequencia := 3;
  fAbastecimentos[2].Bico := 2;
  fAbastecimentos[2].Qtd := 3;
  fAbastecimentos[2].FormaPagto := 0;
  fAbastecimentos[2].ValorPago := 0;

  fAbastecimentos[3].DataHora := IncMinute(Now,-30);
  fAbastecimentos[3].Sequencia := 4;
  fAbastecimentos[3].Bico := 0;
  fAbastecimentos[3].Qtd := 10;
  fAbastecimentos[3].FormaPagto := 0;
  fAbastecimentos[3].ValorPago := 0;

  fAbastecimentos[4].DataHora := IncMinute(Now,-10);
  fAbastecimentos[4].Sequencia := 5;
  fAbastecimentos[4].Bico := 1;
  fAbastecimentos[4].Qtd := 5;
  fAbastecimentos[4].FormaPagto := 0;
  fAbastecimentos[4].ValorPago := 0;

  fAbastecimentos[5].DataHora := IncMinute(Now,-15);
  fAbastecimentos[5].Sequencia := 6;
  fAbastecimentos[5].Bico := 2;
  fAbastecimentos[5].Qtd := 150;
  fAbastecimentos[5].FormaPagto := 0;
  fAbastecimentos[5].ValorPago := 0;
end;

procedure TfrPOSTEFServer.LerConfiguracao;
Var
  Ini : TIniFile ;
  cUF: LongInt;
  bs: String;
begin
  AdicionarLinhaLog('- LerConfiguracao');
  Ini := TIniFile.Create(NomeArquivoConfiguracao);
  try
    rgDocumentoFiscal.ItemIndex := Ini.ReadInteger('Geral','DoctoFiscal', 0);
    rgTransacaoPendente.ItemIndex := Ini.ReadInteger('Geral','TransacaoPendente', 0);
    cbEmailDoctoFiscal.Checked := Ini.ReadBool('Geral','EmailDoctoFiscal', False);
    cbIniciarEmOperacao.Checked := Ini.ReadBool('Geral','IniciarEmOperacao', False);

    sePortaTCP.Value := Ini.ReadInteger('POS', 'PortaTCP', sePortaTCP.Value);
    seMaxConexoes.Value := Ini.ReadInteger('POS', 'MaxConexoes.', seMaxConexoes.Value);
    edLog.Text := Ini.ReadString('POS', 'Log', '');
    edMensagemBoasVindas.Text := Ini.ReadString('POS', 'MensagemBoasVindas', edMensagemBoasVindas.Text);
    cbImprimirViaReduzida.Checked := Ini.ReadBool('POS', 'ImprimirViaReduzida', cbImprimirViaReduzida.Checked);
    cbSuportaDesconto.Checked := Ini.ReadBool('POS', 'SuportaDesconto', cbSuportaDesconto.Checked);
    cbSuportaSaque.Checked := Ini.ReadBool('POS', 'SuportaSaque', cbSuportaSaque.Checked);
    cbUtilizarSaldoTotalVoucher.Checked := Ini.ReadBool('POS', 'UtilizarSaldoTotalVoucher', cbUtilizarSaldoTotalVoucher.Checked);

    cbTipoAplicacao.ItemIndex := Ini.ReadInteger('Aplicacao', 'TipoDemo', cbTipoAplicacao.ItemIndex);
    edRazaoSocial.Text := Ini.ReadString('Aplicacao', 'RazaoSocial', edRazaoSocial.Text);
    edRegistro.Text := Ini.ReadString('Aplicacao', 'Registro', edRegistro.Text);
    edAplicacaoNome.Text := Ini.ReadString('Aplicacao', 'Nome', edAplicacaoNome.Text);
    edAplicacaoVersao.Text := Ini.ReadString('Aplicacao', 'Versao', edAplicacaoVersao.Text);

    edtEmitCNPJ.Text := Ini.ReadString('Emitente', 'CNPJ', '');
    edtEmitIE.Text := Ini.ReadString('Emitente', 'IE', '');
    edtEmitIM.Text := Ini.ReadString('Emitente', 'IM', '');
    edtEmitRazao.Text := Ini.ReadString('Emitente', 'RazaoSocial', '');
    edtEmitFantasia.Text := Ini.ReadString('Emitente', 'Fantasia', '');
    edtEmitFone.Text := Ini.ReadString('Emitente', 'Fone', '');
    edtEmitCEP.Text := Ini.ReadString('Emitente', 'CEP', '');
    edtEmitLogradouro.Text := Ini.ReadString('Emitente', 'Logradouro', '');
    edtEmitNumero.Text := Ini.ReadString('Emitente', 'Numero', '');
    edtEmitComp.Text := Ini.ReadString('Emitente', 'Complemento', '');
    edtEmitBairro.Text := Ini.ReadString('Emitente', 'Bairro', '');
    cUF := Ini.ReadInteger('Emitente', 'cUF', 0);
    pEmitCodUF.Caption := IntToStr(cUF);
    CarregarListaDeCidades(cUF);
    cbxEmitUF.ItemIndex := cbxEmitUF.Items.IndexOf(CUFtoUF(cUF));
    cbxEmitCidade.ItemIndex := FcMunList.IndexOf(IntToStr(Ini.ReadInteger('Emitente', 'cMun', 0)));
    cbxTipoEmpresa.ItemIndex := Ini.ReadInteger('Emitente','CRT', 0);
    cbxRegTribISSQN.ItemIndex  := Ini.ReadInteger('Emitente','RegTribISSQN',0);
    cbxIndRatISSQN.ItemIndex := Ini.ReadInteger('Emitente','IndRatISSQN',0);

    cbSSLLib.ItemIndex := Ini.ReadInteger('Certificado', 'SSLLib', 4);
    cbSSLLibChange(nil);
    cbCryptLib.ItemIndex := Ini.ReadInteger('Certificado', 'CryptLib', cbCryptLib.ItemIndex);
    cbHttpLib.ItemIndex := Ini.ReadInteger('Certificado', 'HttpLib', cbHttpLib.ItemIndex);
    cbXmlSignLib.ItemIndex := Ini.ReadInteger('Certificado', 'XmlSignLib', cbXmlSignLib.ItemIndex);
    edtCertURLPFX.Text := Ini.ReadString('Certificado', 'URL', '');
    edtCertArqPFX.Text := Ini.ReadString('Certificado', 'Caminho', '');
    edtCertSenha.Text := StrCrypt(DecodeBase64(Ini.ReadString('Certificado', 'Senha', '')), CACBR_URL);
    edtCertNumSerie.Text := Ini.ReadString('Certificado', 'NumSerie', '');

    cbSSLType.ItemIndex := Ini.ReadInteger('NFCe.WebService', 'SSLType', 5);
    cbWebServiceUF.ItemIndex := cbWebServiceUF.Items.IndexOf(Ini.ReadString('NFCe.WebService', 'UF', ''));
    cbTipoAmb.ItemIndex := Ini.ReadInteger('NFCe.WebService', 'Ambiente', 1);
    seTimeOut.Value := Ini.ReadInteger('NFCe.WebService', 'TimeOut', seTimeOut.Value);

    edtPathSchemas.Text := Ini.ReadString('NFCe', 'PathSchemas', ApplicationPath+'Schemas\NFe');
    edtTokenID.Text := Ini.ReadString('NFCe', 'TokenID', '');
    edtTokenCSC.Text := Ini.ReadString('NFCe', 'TokenCSC', '');
    seNFCeLote.Value := Ini.ReadInteger('NFCe', 'Lote', 1);
    seNFCeNumero.Value := Ini.ReadInteger('NFCe', 'Numero', 1);

    edtProxyHost.Text := Ini.ReadString('Proxy', 'Host', '');
    seProxyPorta.Text := Ini.ReadString('Proxy', 'Porta', '');
    edtProxyUser.Text := Ini.ReadString('Proxy', 'User', '');
    edtProxySenha.Text := StrCrypt(DecodeBase64(Ini.ReadString('Proxy', 'Pass', '')), CACBR_URL);

    cbSATModelo.ItemIndex := Ini.ReadInteger('SAT','Modelo', 0);
    edtSATDLL.Text := Ini.ReadString('SAT','NomeDLL', '');
    edtSATCodigoAtivacao.Text := StrCrypt(DecodeBase64(Ini.ReadString('SAT','CodigoAtivacao', '')), CACBR_URL);
    seSATNumeroCaixa.Value := Ini.ReadInteger('SAT','NumeroCaixa', 1);
    cbSATAmbiente.ItemIndex := Ini.ReadInteger('SAT','Ambiente', 1);
    seSATPagCod.Value := Ini.ReadInteger('SAT','PaginaDeCodigo', CUTF8CodPage);
    seSATVersaoEnt.Text := FormatFloat('0.00',Ini.ReadFloat('SAT','versaoDadosEnt', ACBrSAT1.Config.infCFe_versaoDadosEnt));
    ckEmuladorSEFAZ.Checked :=  Ini.ReadBool('SAT','Emulador', False);
    edtSATSwHCNPJ.Text := Ini.ReadString('SAT.SwH','CNPJ', '');
    edtSATSwHAssinatura.Text := Ini.ReadString('SAT.SwH','Assinatura', '');

    edtEmailFrom.Text := Ini.ReadString('Email', 'From', edtEmailFrom.Text);
    edtEmailFromName.Text := Ini.ReadString('Email', 'FromName', edtEmailFromName.Text);
    edtEmailHost.Text := Ini.ReadString('Email', 'Host', edtEmailHost.Text);
    seEmailPort.Text := Ini.ReadString('Email', 'Port', seEmailPort.Text);
    edtEmailUser.Text := Ini.ReadString('Email', 'User', edtEmailUser.Text);
    edtEmailPassword.Text := StrCrypt(DecodeBase64(Ini.ReadString('Email', 'Pass', edtEmailPassword.Text)), CACBR_URL);
    chkEmailTLS.Checked := Ini.ReadBool('Email', 'TLS', chkEmailTLS.Checked);
    chkEmailSSL.Checked := Ini.ReadBool('Email', 'SSL', chkEmailSSL.Checked);
    cbEmailDefaultCharset.ItemIndex := Ini.ReadInteger('Email', 'DefaultCharset', Integer(ACBrMail1.DefaultCharset));
    cbEmailIdeCharSet.ItemIndex := Ini.ReadInteger('Email', 'IdeCharset', Integer(ACBrMail1.IDECharset));
    edtEmailAssunto.Text := Ini.ReadString('Email', 'Assunto', edtEmailAssunto.Text);
    bs := BinaryStringToString(mEmailMensagem.Lines.Text);
    mEmailMensagem.Lines.Text := StringToBinaryString(Ini.ReadString('Email', 'Mensagem', bs) );
  finally
     Ini.Free ;
  end ;

  AplicarConfiguracao;
  LigarAlertasdeErrosDeConfiguracao;
end;

procedure TfrPOSTEFServer.AplicarConfiguracao;
begin
  AdicionarLinhaLog('- AplicarConfiguracao');
  ConfigurarACBrNFe;
  ConfigurarACBrPOS;
  ConfigurarACBrMail;
  ConfigurarACBrSAT;
end;

procedure TfrPOSTEFServer.GravarConfiguracao;
Var
  Ini : TIniFile ;
begin
  AdicionarLinhaLog('- GravarConfiguracao');
  Ini := TIniFile.Create(NomeArquivoConfiguracao);
  try
    Ini.WriteInteger('Geral','DoctoFiscal', rgDocumentoFiscal.ItemIndex);
    Ini.WriteInteger('Geral','TransacaoPendente', rgTransacaoPendente.ItemIndex);
    Ini.WriteBool('Geral','EmailDoctoFiscal', cbEmailDoctoFiscal.Checked);
    Ini.WriteBool('Geral','IniciarEmOperacao', cbIniciarEmOperacao.Checked);

    Ini.WriteInteger('POS', 'PortaTCP', sePortaTCP.Value);
    Ini.WriteInteger('POS', 'MaxConexoes.', seMaxConexoes.Value);
    Ini.WriteString('POS', 'Log', edLog.Text);
    Ini.WriteString('POS', 'MensagemBoasVindas', edMensagemBoasVindas.Text);
    Ini.WriteBool('POS', 'ImprimirViaReduzida', cbImprimirViaReduzida.Checked);
    Ini.WriteBool('POS', 'SuportaDesconto', cbSuportaDesconto.Checked);
    Ini.WriteBool('POS', 'SuportaSaque', cbSuportaSaque.Checked);
    Ini.WriteBool('POS', 'UtilizarSaldoTotalVoucher', cbUtilizarSaldoTotalVoucher.Checked);

    Ini.WriteInteger('Aplicacao', 'TipoDemo', cbTipoAplicacao.ItemIndex);
    Ini.WriteString('Aplicacao', 'RazaoSocial', edRazaoSocial.Text);
    Ini.WriteString('Aplicacao', 'Registro', edRegistro.Text);
    Ini.WriteString('Aplicacao', 'Nome', edAplicacaoNome.Text);
    Ini.WriteString('Aplicacao', 'Versao', edAplicacaoVersao.Text);

    Ini.WriteString('Emitente', 'CNPJ', edtEmitCNPJ.Text);
    Ini.WriteString('Emitente', 'IE', edtEmitIE.Text);
    Ini.WriteString('Emitente', 'IM', edtEmitIM.Text);
    Ini.WriteString('Emitente', 'RazaoSocial', edtEmitRazao.Text);
    Ini.WriteString('Emitente', 'Fantasia', edtEmitFantasia.Text);
    Ini.WriteString('Emitente', 'Fone', edtEmitFone.Text);
    Ini.WriteString('Emitente', 'CEP', edtEmitCEP.Text);
    Ini.WriteString('Emitente', 'Logradouro', edtEmitLogradouro.Text);
    Ini.WriteString('Emitente', 'Numero', edtEmitNumero.Text);
    Ini.WriteString('Emitente', 'Complemento', edtEmitComp.Text);
    Ini.WriteString('Emitente', 'Bairro', edtEmitBairro.Text);
    Ini.WriteInteger('Emitente', 'cUF', StrToIntDef(pEmitCodUF.Caption,0));
    Ini.WriteInteger('Emitente', 'cMun', StrToIntDef(pEmitCodCidade.Caption,0));
    Ini.WriteInteger('Emitente', 'CRT', cbxTipoEmpresa.ItemIndex);
    Ini.WriteInteger('Emitente', 'RegTribISSQN', cbxRegTribISSQN.ItemIndex);
    Ini.WriteInteger('Emitente', 'IndRatISSQN', cbxIndRatISSQN.ItemIndex);

    Ini.WriteInteger('Certificado', 'SSLLib', cbSSLLib.ItemIndex);
    Ini.WriteInteger('Certificado', 'CryptLib', cbCryptLib.ItemIndex);
    Ini.WriteInteger('Certificado', 'HttpLib', cbHttpLib.ItemIndex);
    Ini.WriteInteger('Certificado', 'XmlSignLib', cbXmlSignLib.ItemIndex);
    Ini.WriteString('Certificado', 'URL', edtCertURLPFX.Text);
    Ini.WriteString('Certificado', 'Caminho', edtCertArqPFX.Text);
    Ini.WriteString('Certificado', 'Senha', EncodeBase64(StrCrypt(edtCertSenha.Text, CACBR_URL)) );
    Ini.WriteString('Certificado', 'NumSerie', edtCertNumSerie.Text);

    Ini.WriteInteger('NFCe.WebService', 'SSLType', cbSSLType.ItemIndex);
    Ini.WriteString('NFCe.WebService', 'UF', cbWebServiceUF.Text);
    Ini.WriteInteger('NFCe.WebService', 'Ambiente', cbTipoAmb.ItemIndex);
    Ini.WriteInteger('NFCe.WebService', 'TimeOut', seTimeOut.Value);
    Ini.WriteString('NFCe', 'PathSchemas', edtPathSchemas.Text);
    Ini.WriteString('NFCe', 'TokenID', edtTokenID.Text);
    Ini.WriteString('NFCe', 'TokenCSC', edtTokenCSC.Text);
    Ini.WriteInteger('NFCe', 'Lote', seNFCeLote.Value);
    Ini.WriteInteger('NFCe', 'Numero', seNFCeNumero.Value);

    Ini.WriteString('Proxy', 'Host',  edtProxyHost.Text);
    Ini.WriteString('Proxy', 'Porta', seProxyPorta.Text);
    Ini.WriteString('Proxy', 'User',  edtProxyUser.Text);
    Ini.WriteString('Proxy', 'Pass', EncodeBase64(StrCrypt(edtProxySenha.Text, CACBR_URL)) );

    Ini.WriteInteger('SAT','Modelo',cbSATModelo.ItemIndex);
    Ini.WriteString('SAT','NomeDLL',edtSATDLL.Text);
    Ini.WriteString('SAT','CodigoAtivacao', EncodeBase64(StrCrypt(edtSATCodigoAtivacao.Text, CACBR_URL)) );
    Ini.WriteInteger('SAT','NumeroCaixa',seSATNumeroCaixa.Value);
    Ini.WriteInteger('SAT','Ambiente',cbSATAmbiente.ItemIndex);
    Ini.WriteInteger('SAT','PaginaDeCodigo',seSATPagCod.Value);
    Ini.WriteFloat('SAT','versaoDadosEnt',StrToFloatDef(seSATVersaoEnt.Text, ACBrSAT1.Config.infCFe_versaoDadosEnt) );
    Ini.WriteBool('SAT','Emulador', ckEmuladorSEFAZ.Checked);
    Ini.WriteString('SAT.SwH','CNPJ',edtSATSwHCNPJ.Text);
    Ini.WriteString('SAT.SwH','Assinatura',edtSATSwHAssinatura.Text);

    Ini.WriteString('Email', 'From', edtEmailFrom.text);
    Ini.WriteString('Email', 'FromName', edtEmailFromName.text);
    Ini.WriteString('Email', 'Host', edtEmailHost.text);
    Ini.WriteString('Email', 'Port', seEmailPort.text);
    Ini.WriteString('Email', 'User', edtEmailUser.text);
    Ini.WriteString('Email', 'Pass', EncodeBase64(StrCrypt(edtEmailPassword.text, CACBR_URL)) );
    Ini.WriteBool('Email', 'TLS', chkEmailTLS.Checked);
    Ini.WriteBool('Email', 'SSL', chkEmailSSL.Checked);
    Ini.WriteInteger('Email', 'DefaultCharset', cbEmailDefaultCharset.ItemIndex);
    Ini.WriteInteger('Email', 'IdeCharset', cbEmailIdeCharSet.ItemIndex);
    Ini.WriteString('Email', 'Assunto', edtEmailAssunto.Text);
    Ini.WriteString('Email', 'Mensagem', BinaryStringToString(mEmailMensagem.Lines.Text));
  finally
     Ini.Free ;
  end ;
end;

procedure TfrPOSTEFServer.GravarNumeracaoNFCe(const TerminalId: String; Lote,
  Numero: Integer);
begin

end;

procedure TfrPOSTEFServer.ConfigurarACBrNFe;
var
  CertPFX: string;
begin
  if not DeveFazerEmissaoDeNFCe then
    Exit;

  AdicionarLinhaLog('- ConfigurarACBrNFe');
  CertPFX := edtCertArqPFX.Text;
  if (ExtractFilePath(CertPFX) = '') then
    CertPFX := ApplicationPath + CertPFX;

  ACBrNFe1.Configuracoes.Certificados.URLPFX := edtCertURLPFX.Text;
  ACBrNFe1.Configuracoes.Certificados.ArquivoPFX := CertPFX;
  ACBrNFe1.Configuracoes.Certificados.NumeroSerie := edtCertNumSerie.Text;
  ACBrNFe1.Configuracoes.Certificados.Senha := edtCertSenha.Text;
  ACBrNFe1.SSL.URLPFX := ACBrNFe1.Configuracoes.Certificados.URLPFX;
  ACBrNFe1.SSL.ArquivoPFX := ACBrNFe1.Configuracoes.Certificados.ArquivoPFX;
  ACBrNFe1.SSL.NumeroSerie := ACBrNFe1.Configuracoes.Certificados.NumeroSerie;
  ACBrNFe1.SSL.Senha := ACBrNFe1.Configuracoes.Certificados.Senha;

  ACBrNFe1.Configuracoes.WebServices.UF := cbWebServiceUF.Text;

  ACBrNFe1.Configuracoes.WebServices.Ambiente := TpcnTipoAmbiente(cbTipoAmb.ItemIndex);
  ACBrNFe1.Configuracoes.WebServices.TimeOut := seTimeOut.Value;

  ACBrNFe1.Configuracoes.Geral.SSLLib := TSSLLib(cbSSLLib.ItemIndex);
  ACBrNFe1.Configuracoes.Geral.SSLCryptLib := TSSLCryptLib(cbCryptLib.ItemIndex);
  ACBrNFe1.Configuracoes.Geral.SSLHttpLib := TSSLHttpLib(cbHttpLib.ItemIndex);
  ACBrNFe1.Configuracoes.Geral.SSLXmlSignLib := TSSLXmlSignLib(cbXmlSignLib.ItemIndex);
  ACBrNFe1.Configuracoes.WebServices.SSLType := TSSLType(cbSSLType.ItemIndex);
  AtualizarSSLLibsCombo;

  ACBrNFe1.Configuracoes.Geral.IdCSC := edtTokenID.Text;
  ACBrNFe1.Configuracoes.Geral.CSC := edtTokenCSC.Text;
  ACBrNFe1.Configuracoes.Geral.ExibirErroSchema := True;
  ACBrNFe1.Configuracoes.Geral.FormatoAlerta := 'Campo:%DESCRICAO% - %MSG%';

  ACBrNFe1.Configuracoes.WebServices.ProxyHost := edtProxyHost.Text;
  ACBrNFe1.Configuracoes.WebServices.ProxyPort := seProxyPorta.Text;
  ACBrNFe1.Configuracoes.WebServices.ProxyUser := edtProxyUser.Text;
  ACBrNFe1.Configuracoes.WebServices.ProxyPass := edtProxySenha.Text;
  ACBrNFe1.Configuracoes.WebServices.Salvar := False;

  ACBrNFe1.Configuracoes.Arquivos.PathSchemas := edtPathSchemas.Text;
  ACBrNFe1.Configuracoes.Arquivos.PathNFe := ApplicationPath + 'xml'+PathDelim+'nfe';
  ACBrNFe1.Configuracoes.Arquivos.PathEvento := ACBrNFe1.Configuracoes.Arquivos.PathNFe;
  ACBrNFe1.Configuracoes.Arquivos.PathInu := ACBrNFe1.Configuracoes.Arquivos.PathNFe;
  ACBrNFe1.Configuracoes.Arquivos.PathSalvar := ACBrNFe1.Configuracoes.Arquivos.PathNFe + PathDelim + 'soap';
end;

procedure TfrPOSTEFServer.ConfigurarACBrPOS;
begin
  AdicionarLinhaLog('- ConfigurarACBrPOS');
  ACBrPOS1.DesInicializar;
  ACBrPOS1.PortaTCP := sePortaTCP.Value;
  ACBrPOS1.MaximoTerminaisConectados := seMaxConexoes.Value;
  ACBrPOS1.ArqLOG := edLog.Text;
  ACBrPOS1.NomeAplicacao := edAplicacaoNome.Text;
  ACBrPOS1.VersaoAplicacao := edAplicacaoVersao.Text;
  ACBrPOS1.SoftwareHouse := edRazaoSocial.Text;
  ACBrPOS1.MensagemBoasVindas := edMensagemBoasVindas.Text;
  ACBrPOS1.ImprimirViaClienteReduzida := cbImprimirViaReduzida.Checked;
  ACBrPOS1.SuportaDesconto := cbSuportaDesconto.Checked;
  ACBrPOS1.SuportaSaque := cbSuportaSaque.Checked;
  ACBrPOS1.UtilizaSaldoTotalVoucher := cbUtilizarSaldoTotalVoucher.Checked;
end;

procedure TfrPOSTEFServer.ConfigurarACBrMail;
begin
  if not DeveFazerEnvioDeEmail then
    Exit;

  ACBrMail1.From := edtEmailFrom.text;
  ACBrMail1.FromName := edtEmailFromName.text;
  ACBrMail1.Host := edtEmailHost.text;
  ACBrMail1.Username := edtEmailUser.text;
  ACBrMail1.Password := edtEmailPassword.text;
  ACBrMail1.Port := seEmailPort.text;
  ACBrMail1.SetTLS := chkEmailTLS.Checked;
  ACBrMail1.SetSSL := chkEmailSSL.Checked;  // Verifique se o seu servidor necessita SSL
  ACBrMail1.DefaultCharset := TMailCharset(cbEmailDefaultCharset.ItemIndex);
  ACBrMail1.IDECharset := TMailCharset(cbEmailIdeCharSet.ItemIndex);
end;

function TfrPOSTEFServer.DeveFazerEmissaoDeNFCe: Boolean;
begin
  Result := (rgDocumentoFiscal.ItemIndex = 1);
end;

function TfrPOSTEFServer.DeveFazerEmissaoDeSAT: Boolean;
begin
  Result := (rgDocumentoFiscal.ItemIndex = 2);
end;

function TfrPOSTEFServer.DeveFazerEmissaoDeDocumentoFiscal: Boolean;
begin
  Result := (DeveFazerEmissaoDeNFCe or DeveFazerEmissaoDeSAT);
end;

function TfrPOSTEFServer.DeveFazerEnvioDeEmail: Boolean;
begin
  Result := cbEmailDoctoFiscal.Checked and DeveFazerEmissaoDeDocumentoFiscal;
end;

procedure TfrPOSTEFServer.ConfigurarACBrSAT;
begin
  if not DeveFazerEmissaoDeSAT then
    Exit;

  with ACBrSAT1 do
  begin
    DesInicializar;
    Modelo  := TACBrSATModelo(cbSATModelo.ItemIndex);
    NomeDLL := edtSATDLL.Text;
    Config.ide_numeroCaixa := seSATNumeroCaixa.Value;
    Config.ide_tpAmb := TpcnTipoAmbiente(cbSATAmbiente.ItemIndex);
    Config.ide_CNPJ := edtSATSwHCNPJ.Text;
    if ckEmuladorSEFAZ.Checked then
    begin
      Config.emit_CNPJ := '11111111111111';
      Config.emit_IE := '111111111111';
      Config.emit_IM := '123123';
    end
    else
    begin
      Config.emit_CNPJ := OnlyNumber(edtEmitCNPJ.Text);
      Config.emit_IE := OnlyNumber(edtEmitIE.Text);
      Config.emit_IM := OnlyNumber(edtEmitIM.Text);
    end;

    Config.emit_cRegTrib := TpcnRegTrib(IfThen(cbxTipoEmpresa.ItemIndex = 2, 1, 0)) ;
    Config.emit_cRegTribISSQN := TpcnRegTribISSQN( cbxRegTribISSQN.ItemIndex ) ;
    Config.emit_indRatISSQN := TpcnindRatISSQN( cbxIndRatISSQN.ItemIndex ) ;
    Config.PaginaDeCodigo := seSATPagCod.Value;
    Config.EhUTF8 := ckSATUTF8.Checked;
    Config.infCFe_versaoDadosEnt := StrToFloatDef(seSATVersaoEnt.Text, Config.infCFe_versaoDadosEnt);

    ConfigArquivos.PastaCFeVenda := ApplicationPath + 'xml'+PathDelim+'sat';
    ConfigArquivos.PastaCFeCancelamento := ConfigArquivos.PastaCFeVenda+PathDelim+'Canc';
    ConfigArquivos.PastaEnvio := ConfigArquivos.PastaCFeVenda+PathDelim+'Env';
  end
end;

procedure TfrPOSTEFServer.IrParaMenuPrincipal;
begin
  AdicionarLinhaLog('- IrParaMenuPrincipal');
  pgPrincipal.ActivePage := tsMenuPrincipal;
end;

procedure TfrPOSTEFServer.AtualizarSSLLibsCombo;
begin
  cbSSLLib.ItemIndex := Integer(ACBrNFe1.Configuracoes.Geral.SSLLib);
  imgErrSSLLib.Visible := (cbSSLLib.ItemIndex < 1);

  cbCryptLib.ItemIndex := Integer(ACBrNFe1.Configuracoes.Geral.SSLCryptLib);
  imgErrCryptLib.Visible := (cbCryptLib.ItemIndex < 1);

  cbHttpLib.ItemIndex := Integer(ACBrNFe1.Configuracoes.Geral.SSLHttpLib);
  imgErrHttpLib.Visible := (cbHttpLib.ItemIndex < 1);

  cbXmlSignLib.ItemIndex := Integer(ACBrNFe1.Configuracoes.Geral.SSLXmlSignLib);
  imgErrXmlSignLib.Visible := (cbXmlSignLib.ItemIndex < 1);

  cbSSLType.Enabled := (ACBrNFe1.Configuracoes.Geral.SSLHttpLib in [httpWinHttp, httpOpenSSL]);

  ValidarConfigCertificado;
end;

procedure TfrPOSTEFServer.ValidarConfigCertificado;
var
 PathPfx, UrlPfx, ArqPfx, NumSerie, Senha: String;
 Ok: Boolean;
begin
  UrlPfx := edtCertURLPFX.Text;
  ArqPfx := edtCertArqPFX.Text;
  NumSerie := edtCertNumSerie.Text;
  Senha := edtCertSenha.Text;
  Ok := (cbCryptLib.ItemIndex > 0);

  if (NumSerie = '') then
  begin
    Ok := Ok and (Senha <> '');
    if Ok then
    begin
      if (UrlPfx <> '') then
        Ok := (ArqPfx <> '')   // Precisa do PFX, para Cache Local
      else
      begin
        if (ExtractFilePath(ArqPfx) = '') then
          PathPfx := ApplicationPath + ArqPfx
        else
          PathPfx := ArqPfx;

        Ok := (ArqPfx <> '') and FileExists(PathPfx);
      end;
    end;
  end;

  imgErrCertificado.Visible := not Ok;
  btCertInfo.Enabled := not imgErrCertificado.Visible;
  ValidarConfigWebService;
end;

procedure TfrPOSTEFServer.ValidarConfigWebService;
var
  Ok: Boolean;
  PathSchemas: String;
begin
  imgErrWebService.Visible := (cbWebServiceUF.ItemIndex < 0);

  PathSchemas := edtPathSchemas.Text;
  Ok := (PathSchemas <> '');
  if Ok then
    Ok := FileExists(PathWithDelim(PathSchemas) + 'nfe_v4.00.xsd');

  imgErrPathSchemas.Visible := not Ok;
  btStatusServico.Enabled := not ( imgErrCertificado.Visible or
                                   imgErrPathSchemas.Visible or
                                   imgErrWebService.Visible);
end;

procedure TfrPOSTEFServer.ValidarConfigEmail;
var
  Ok: Boolean;
begin
  Ok := (edtEmailFrom.Text <> '') and
        (edtEmailFromName.Text <> '') and
        (edtEmailHost.Text <> '') and
        (seEmailPort.Value > 0) and
        (edtEmailUser.Text <> '') and
        (edtEmailPassword.Text <> '');

  imgErrEmail.Visible := not Ok;
  btEmailTeste.Enabled := Ok and (edtEmailTo.Text <> '');
end;

procedure TfrPOSTEFServer.ValidarConfigSAT;
var
  Ok: Boolean;
begin
  Ok := (edtSATDLL.Text <> '') and
        (edtSATCodigoAtivacao.Text <> '') and
        (edtSATSwHCNPJ.Text <> '') and
        (edtSATSwHAssinatura.Text <> '') and
        (cbSATModelo.ItemIndex >= 0) and
        (cbSATAmbiente.ItemIndex >= 0);

  imgErrSAT.Visible := not Ok;
  btStatusSAT.Enabled := Ok;
  btStatusOperacionalSAT.Enabled := Ok;
end;

procedure TfrPOSTEFServer.LigarAlertasdeErrosDeConfiguracao;
begin
  rgDocumentoFiscalClick(Nil);

  edtEmitCNPJChange(Nil);
  edtEmitRazaoChange(Nil);
  edtEmitFoneChange(Nil);
  edtEmitCEPChange(Nil);
  cbxEmitCidadeChange(Nil);

  AtualizarSSLLibsCombo;
  edtCertArqPFXChange(Nil);
  edtCertNumSerieChange(Nil);

  ValidarConfigWebService;
  edtTokenCSCChange(Nil);
  edtTokenIDChange(Nil);

  ValidarConfigEmail;
end;

end.


