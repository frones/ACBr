{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
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

{$I ACBr.inc}

unit ACBrTEFPayGoAndroidAPI;

interface

uses
  Classes, SysUtils,
  System.Messaging,
  System.Character,
  System.UITypes,
  FMX.Platform,
  FMX.Platform.Android,
  Androidapi.Helpers,
  Androidapi.JNI.App,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Net,
  Androidapi.JNI.Os,
  Androidapi.JNI.Embarcadero,
  Androidapi.JNI.GraphicsContentViewText,
  ACBrTEFComum, ACBrTEFPayGoComum, ACBrBase;

resourcestring
  sErrNOTINIT = 'TACBrTEFPGWebAndroid não foi inicializada';
  sErrTRNINIT = 'Já foi iniciada uma Transação';
  sErrTRNNOTINIT = 'Não foi iniciada uma Transação';
  sErrINTENTNOTFOUND = 'Ação %s não encontrada.';
  sErrCANCELED = 'Operação TEF cancelada';

const
  CACBrTEFPGWebAndroidName = 'ACBrTEFPGWebAndroid';
  CACBrTEFPGWebAndroidVersao = '0.1.0';

  URI_Scheme = 'app';
  URI_AuthorityPayment = 'payment';
  URI_AuthorityConfirmation = 'confirmation';
  URI_AuthorityResolve = 'resolve';

  URI_PathInput = 'input';
  URI_PathOutput = 'output';
  URI_PathConfirmation = 'confirmation';
  URI_PathPendingTransaction = 'pendingTransaction';

  Key_URI = 'uri';
  Key_DadosAutomacao = 'DadosAutomacao';
  Key_Personalizacao = 'Personalizacao';
  Key_Package = 'package';
  Key_IntegracaoDireta = 'integracaoDireta';
  Key_TransacaoPendenteDados = 'TransacaoPendenteDados';
  Key_Confirmacao = 'Confirmacao';

  Intent_SERVICO = 'br.com.setis.interfaceautomacao.SERVICO';
  Intent_Payment = 'br.com.setis.payment.TRANSACTION';
  Intent_Confirmation = 'br.com.setis.confirmation.TRANSACTION';

type
  EACBrTEFPGWebAndroid = class(EACBrTEFErro);

  TACBrURI = class
  private
    fScheme: String;
    fAuthority: String;
    fParams: TACBrInformacoes;
    fPath: String;
    function GetURI: String;
    procedure SetURI(const AValue: String);
  public
    constructor Create(const AScheme, AAuthority: String; APath: String = ''); overload;
    constructor Create; overload;
    destructor Destroy; override;
    procedure Clear;

    property Scheme: String read fScheme;
    property Authority: String read fAuthority;
    property Params: TACBrInformacoes read fParams;
    property Path: String read fPath write fPath;

    property URI: String read GetURI write SetURI;
  end;

  TACBrTEFPGWebAndroidEstadoTransacao = procedure(AIntent: JIntent) of object;

  TACBrTEFPGWebAndroidAvaliarTransacaoPendente = procedure(pszReqNum: String;
    pszLocRef: String; pszExtRef: String; pszVirtMerch: String;
    pszAuthSyst: String) of object;

  TACBrTEFPGWebAndroidPersonalizacao = class
  private
    fcorFundoTela: TAlphaColor;
    fcorFonte: TAlphaColor;
    fcorTextoCaixaEdicao: TAlphaColor;
    fcorFundoCaixaEdicao: TAlphaColor;
    fcorSeparadorMenu: TAlphaColor;
    fcorTeclaLiberadaTeclado: TAlphaColor;
    fcorTeclaPressionadaTeclado: TAlphaColor;
    fcorFundoToolbar: TAlphaColor;
    fcorFonteTeclado: TAlphaColor;
    fcorFundoTeclado: TAlphaColor;
    fpathIconeToolbar: String;
    fpathFonte: String;
    ficoneToolbarBase64: String;
    ffonteBase64: String;
    function GetfonteBase64: String;
    function GeticoneToolbarBase64: String;
    function FileToBase64(AFile: String): String;
    procedure SetPathFonte(const Value: String);
    procedure SetPathIconeToolbar(const Value: String);
  public
    constructor Create;
    procedure Clear;

    function GetURI_Personalizacao: String;

    property corFundoTela: TAlphaColor read fcorFundoTela write fcorFundoTela default 0;
    property corFundoToolbar: TAlphaColor read fcorFundoToolbar write fcorFundoToolbar default 0;
    property corFundoTeclado: TAlphaColor read fcorFundoTeclado write fcorFundoTeclado default 0;
    property corFonte: TAlphaColor read fcorFonte write fcorFonte default 0;
    property corFundoCaixaEdicao: TAlphaColor read fcorFundoCaixaEdicao write fcorFundoCaixaEdicao default 0;
    property corTextoCaixaEdicao: TAlphaColor read fcorTextoCaixaEdicao write fcorTextoCaixaEdicao default 0;
    property corTeclaLiberadaTeclado: TAlphaColor read fcorTeclaLiberadaTeclado write fcorTeclaLiberadaTeclado default 0;
    property corTeclaPressionadaTeclado: TAlphaColor read fcorTeclaPressionadaTeclado write fcorTeclaPressionadaTeclado default 0;
    property corFonteTeclado: TAlphaColor read fcorFonteTeclado write fcorFonteTeclado default 0;
    property corSeparadorMenu: TAlphaColor read fcorSeparadorMenu write fcorSeparadorMenu default 0;
    property pathIconeToolbar: String read fpathIconeToolbar write SetPathIconeToolbar;
    property iconeToolbarBase64: String read GeticoneToolbarBase64;
    property pathFonte: String read fpathFonte write SetPathFonte;
    property fonteBase64: String read GetfonteBase64;
  end;

  TACBrTEFPGWebAndroidDadosAutomacao = class
  private
    fSoftwareHouse: String;
    fNomeAplicacao: String;
    fVersaoAplicacao: String;
    fSuportaSaque: Boolean;
    fSuportaDesconto: Boolean;
    fSuportaViasDiferenciadas: Boolean;
    fUtilizaSaldoTotalVoucher: Boolean;
    fImprimeViaClienteReduzida: Boolean;
    procedure SetNomeAplicacao(const AValue: String);
    procedure SetSoftwareHouse(const AValue: String);
    procedure SetVersaoAplicacao(const AValue: String);

  public
    constructor Create;
    procedure Clear;

    function CalcularCapacidadesDaAutomacao: Integer;
    function GetURI_DadosAutomacao(ParametrosAdicionais: TACBrTEFParametros): String;

    property SoftwareHouse: String read fSoftwareHouse write SetSoftwareHouse;
    property NomeAplicacao: String read fNomeAplicacao write SetNomeAplicacao ;
    property VersaoAplicacao: String read fVersaoAplicacao write SetVersaoAplicacao ;

    Property SuportaSaque: Boolean read fSuportaSaque write fSuportaSaque default False;
    Property SuportaDesconto: Boolean read fSuportaDesconto write fSuportaDesconto default False;
    property ImprimeViaClienteReduzida: Boolean read fImprimeViaClienteReduzida
      write fImprimeViaClienteReduzida default True;
    property SuportaViasDiferenciadas: Boolean read fSuportaViasDiferenciadas
      write fSuportaViasDiferenciadas default True;
    property UtilizaSaldoTotalVoucher: Boolean read fUtilizaSaldoTotalVoucher
      write fUtilizaSaldoTotalVoucher default False;
  end;

  TACBrTEFPGWebAndroid = class
  private
    fInicializada: Boolean;
    fEmTransacao: Boolean;
    fCNPJEstabelecimento: String;
    fConfirmarTransacoesPendentesNoHost: Boolean;
    fDadosTransacao: TACBrTEFParametros;
    fNomeEstabelecimento: String;
    fParametrosAdicionais: TACBrTEFParametros;
    fOnGravarLog: TACBrGravarLog;
    fOutputURI: TACBrURI;
    fDadosPendentesURI: TACBrURI;
    fOnAntesIniciarTransacao: TACBrTEFPGWebAndroidEstadoTransacao;
    fOnDepoisTerminarTransacao: TACBrTEFPGWebAndroidEstadoTransacao;
    fOnAvaliarTransacaoPendente: TACBrTEFPGWebAndroidAvaliarTransacaoPendente;
    fDadosAutomacao: TACBrTEFPGWebAndroidDadosAutomacao;
    fPersonalizacao: TACBrTEFPGWebAndroidPersonalizacao;

    procedure SetCNPJEstabelecimento(AValue: String);
    procedure SetNomeEstabelecimento(AValue: String);

    procedure SetInicializada(AValue: Boolean);

    procedure HandleActivityMessage(const Sender: TObject; const M: TMessage);
    function OnActivityResult(RequestCode, ResultCode: Integer; AIntent: JIntent): Boolean;
    function BooleanStrToByte(const AValue: String): Byte;

    function GetURI_Input(iOPER: Byte): String;
    procedure AddURIParam(AURI: TACBrURI; ParamName: string; FromInfo: Word;
      ValorPadrao: string = ''; IncluirSeVazio: Boolean = False);
    procedure IniciarIntent(AIntent: JIntent);

    function GerarTransactionId: String;
  protected
    procedure DoException( AErrorMsg: String );
    function ObterUltimoRetorno: String;

    procedure AdicionarDadosObrigatorios;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Inicializar;
    procedure DesInicializar;

    procedure GravarLog(const AString: AnsiString; Traduz: Boolean = False); overload;
    procedure GravarLog(const AString: String; Traduz: Boolean = False); overload;

    procedure IniciarTransacao(iOPER: Byte;
      ParametrosAdicionaisTransacao: TACBrTEFParametros = Nil);
    procedure ObterDadosDaTransacao;
    procedure TratarTransacaoPendente(AStatus: LongWord = 0;
      pszReqNum: String = ''; pszLocRef: String = ''; pszExtRef: String = '';
      pszVirtMerch: String = ''; pszAuthSyst: String = '');
    procedure ConfirmarTransacao(AStatus: LongWord;
      confirmTransactionIdentifier: String = '');
    procedure ResolverTransacaoPendente(AStatus: LongWord; pszReqNum: String;
      pszLocRef: String; pszExtRef: String; pszVirtMerch: String;
      pszAuthSyst: String);

    property Inicializada: Boolean read fInicializada write SetInicializada;

    property EmTransacao: Boolean read fEmTransacao;
    property DadosDaTransacao: TACBrTEFParametros read fDadosTransacao;

    Property NomeEstabelecimento: String read fNomeEstabelecimento write SetNomeEstabelecimento;
    property CNPJEstabelecimento: String read fCNPJEstabelecimento write SetCNPJEstabelecimento;

    property DadosAutomacao: TACBrTEFPGWebAndroidDadosAutomacao read fDadosAutomacao;
    property Personalizacao: TACBrTEFPGWebAndroidPersonalizacao read fPersonalizacao;

    property ParametrosAdicionais: TACBrTEFParametros read fParametrosAdicionais;

    property ConfirmarTransacoesPendentesNoHost: Boolean
      read fConfirmarTransacoesPendentesNoHost
      write fConfirmarTransacoesPendentesNoHost;

    property OnGravarLog: TACBrGravarLog read fOnGravarLog write fOnGravarLog;
    property OnAntesIniciarTransacao: TACBrTEFPGWebAndroidEstadoTransacao
      read fOnAntesIniciarTransacao write fOnAntesIniciarTransacao;
    property OnDepoisTerminarTransacao: TACBrTEFPGWebAndroidEstadoTransacao
      read fOnDepoisTerminarTransacao write fOnDepoisTerminarTransacao;

    property OnAvaliarTransacaoPendente: TACBrTEFPGWebAndroidAvaliarTransacaoPendente
      read fOnAvaliarTransacaoPendente write fOnAvaliarTransacaoPendente;
  end;

function PWOPER_ToOperation(iOPER: Byte): String;
function OperationToPWOPER_(const AOperation: String): Byte;
function PWCNF_ToTransactionStatus(Status: LongWord): String;
function TransactionStatusToPWCNF_(const AStatus: String): LongWord;
function PWINFO_AUTHSYSTToProviderName(const AUTHSYST: String): String;
function ProviderNameToPWINFO_AUTHSYST(const Provider: String): String;
function PWINFO_CARDTYPEToCardType(const CARDTYPE: String): String;
function CardTypeToPWINFO_CARDTYPE(const CardType: String): Byte;
function PWINFO_FINTYPEToFinType(const FINTYPE: String): String;
function FinancingTypeToPWINFO_FINTYPE(const financingType: String): Byte;
function PWINFO_PAYMNTTYPEToPaymentType(const PAYMNTTYPE: String): String;
function PaymentModeToPWINFO_PAYMNTTYPE(const paymentMode: String): Byte;
function PrintReceiptsToPWINFO_RCPTPRN(const printReceipts: String): Byte;
function WalletUserIdToPWINFO_WALLETUSERIDTYPE(const WalletUserId: String): Byte;

implementation
uses
  StrUtils,
  synacode, synautil,
  ACBrUtil.Strings, ACBrUtil.Math, ACBrUtil.FilesIO, ACBrValidador;


function PWOPER_ToOperation(iOPER: Byte): String;
begin
  case iOPER of
    PWOPER_SALE:
      Result := 'VENDA';
    PWOPER_ADMIN:
      Result := 'ADMINISTRATIVA';
    PWOPER_SETTLEMNT:
      Result := 'FECHAMENTO';
    PWOPER_SALEVOID, PWOPER_VOID:
      Result := 'CANCELAMENTO';
    PWOPER_PREAUTH:
      Result := 'PREAUTORIZACAO';
    PWOPER_RETBALINQ, PWOPER_CRDBALINQ:
      Result := 'CONSULTA_SALDO';
    PWOPER_CHECKINQ:
      Result := 'CONSULTA_CHEQUE';
//  PWOPER_???:
//    Result := 'GARANTIA_CHEQUE';
    PWOPER_PREAUTVOID:
      Result := 'CANCELAMENTO_PREAUTORIZACAO';
    PWOPER_CASHWDRWL:
      Result := 'SAQUE';
//  PWOPER_???:
//    Result := 'DOACAO';
    PWOPER_BILLPAYMENT:
      Result := 'PAGAMENTO_CONTA';
    PWOPER_BILLPAYMENTVOID:
      Result := 'CANCELAMENTO_PAGAMENTOCONTA';
    PWOPER_PREPAID:
      Result := 'RECARGA_CELULAR';
    PWOPER_INSTALL:
      Result := 'INSTALACAO';
    PWOPER_REPRNTNTRANSACTION, PWOPER_REPRINT:
      Result := 'REIMPRESSAO';
    PWOPER_RPTTRUNC:
      Result := 'RELATORIO_SINTETICO';
    PWOPER_RPTDETAIL:
      Result := 'RELATORIO_DETALHADO';
    PWOPER_NULL, PWOPER_COMMTEST:
      Result := 'TESTE_COMUNICACAO';
    PWOPER_RPTSUMMARY:
      Result := 'RELATORIO_RESUMIDO';
    PWOPER_SHOWPDC:
      Result := 'EXIBE_PDC';
    PWOPER_VERSION:
      Result := 'VERSAO';
    PWOPER_CONFIG:
      Result := 'CONFIGURACAO';
    PWOPER_MAINTENANCE:
      Result := 'MANUTENCAO';
  else
    Result := 'OPERACAO_DESCONHECIDA';
  end;
end;

function OperationToPWOPER_(const AOperation: String): Byte;
begin
  if (AOperation = 'VENDA') then
    Result := PWOPER_SALE
  else if (AOperation = 'ADMINISTRATIVA') then
    Result := PWOPER_ADMIN
  else if (AOperation = 'FECHAMENTO') then
    Result := PWOPER_SETTLEMNT
  else if (AOperation = 'CANCELAMENTO') then
    Result := PWOPER_VOID
  else if (AOperation = 'PREAUTORIZACAO') then
    Result := PWOPER_PREAUTH
  else if (AOperation = 'CONSULTA_SALDO') then
    Result := PWOPER_CRDBALINQ
  else if (AOperation = 'CONSULTA_CHEQUE') then
    Result := PWOPER_CHECKINQ
  else if (AOperation = 'GARANTIA_CHEQUE') then
    Result := PWOPER_ADMIN  //TODO: ???
  else if (AOperation = 'CANCELAMENTO_PREAUTORIZACAO') then
    Result := PWOPER_PREAUTVOID
  else if (AOperation = 'SAQUE') then
    Result := PWOPER_CASHWDRWL
  else if (AOperation = 'DOACAO') then
    Result := PWOPER_ADMIN  //TODO: ???
  else if (AOperation = 'PAGAMENTO_CONTA') then
    Result := PWOPER_BILLPAYMENT
  else if (AOperation = 'CANCELAMENTO_PAGAMENTOCONTA') then
    Result := PWOPER_BILLPAYMENTVOID
  else if (AOperation = 'RECARGA_CELULAR') then
    Result := PWOPER_PREPAID
  else if (AOperation = 'INSTALACAO') then
    Result := PWOPER_INSTALL
  else if (AOperation = 'REIMPRESSAO') then
    Result := PWOPER_REPRINT
  else if (AOperation = 'RELATORIO_SINTETICO') then
    Result := PWOPER_RPTTRUNC
  else if (AOperation = 'RELATORIO_DETALHADO') then
    Result := PWOPER_RPTDETAIL
  else if (AOperation = 'TESTE_COMUNICACAO') then
    Result := PWOPER_COMMTEST
  else if (AOperation = 'RELATORIO_RESUMIDO') then
    Result := PWOPER_RPTSUMMARY
  else if (AOperation = 'EXIBE_PDC') then
    Result := PWOPER_SHOWPDC
  else if (AOperation = 'VERSAO') then
    Result := PWOPER_VERSION
  else if (AOperation = 'CONFIGURACAO') then
    Result := PWOPER_CONFIG
  else if (AOperation = 'MANUTENCAO') then
    Result := PWOPER_MAINTENANCE
  else
    Result := 0;
end;

function PWCNF_ToTransactionStatus(Status: LongWord): String;
begin
  case Status of
    PWCNF_CNF_AUTO:
      Result := 'CONFIRMADO_AUTOMATICO';                          //DD
    PWCNF_CNF_MANU_AUT:
      Result := 'CONFIRMADO_MANUAL';
    PWCNF_REV_PRN_AUT:
      Result := 'DESFEITO_ERRO_IMPRESSAO_AUTOMATICO';
    PWCNF_REV_MANU_AUT:
      Result := 'DESFEITO_MANUAL';
    PWCNF_REV_DISP_AUT:
      Result := 'DESFEITO_LIBERACAO_MERCADORIA';
  else
    Result := 'STATUS_TRANSACAO_NAO_DEFINIDO';
  end;
end;

function TransactionStatusToPWCNF_(const AStatus: String): LongWord;
var
  uStatus: String;
begin
  uStatus := UpperCase(AStatus);
  if (AStatus = 'CONFIRMADO_AUTOMATICO') then                  //DD
    Result := PWCNF_CNF_AUTO
  else if (AStatus = 'CONFIRMADO_MANUAL') then
    Result := PWCNF_CNF_MANU_AUT
  else if (AStatus = 'DESFEITO_ERRO_IMPRESSAO_AUTOMATICO') then
    Result := PWCNF_REV_PRN_AUT
  else if (AStatus = 'DESFEITO_MANUAL') then
    Result := PWCNF_REV_DISP_AUT
  else if (AStatus = 'DESFEITO_LIBERACAO_MERCADORIA') then
    Result := PWCNF_REV_DISP_AUT
  else
    Result := 0;
end;

function PWINFO_AUTHSYSTToProviderName(const AUTHSYST: String): String;
var
  uAuthSyst: String;
begin
  uAuthSyst := UpperCase(AUTHSYST);
{  if uAuthSyst = 'REDE' then
    Result := 'REDECARD'
  else if uAuthSyst = 'VISANET' then
    Result := 'CIELO'
  else if uAuthSyst = 'BANESECARD' then
    Result := 'BANESE'
  else if uAuthSyst = 'TICKET' then
    Result := 'TICKETCAR'
  else if uAuthSyst = 'BIN' then
    Result := 'FIRSTDATA'
  else}
    Result := uAuthSyst;
end;

function ProviderNameToPWINFO_AUTHSYST(const Provider: String): String;
begin
  if Provider = 'REDECARD' then
    Result := 'REDE'
  else if Provider = 'CIELO' then
    Result := 'VISANET'
  else if Provider = 'BANESE' then
    Result := 'BANESECARD'
  else if Provider = 'TICKETCAR' then
    Result := 'TICKET'
  else if Provider = 'FIRSTDATA' then
    Result := 'BIN'
  else
    Result := Provider;
end;

function PWINFO_CARDTYPEToCardType(const CARDTYPE: String): String;
var
  AByte: Integer;
begin
  // 1: crédito 2: débito 4: voucher/PAT 8: private label 16: frota 128: outros
  AByte := StrToIntDef(CARDTYPE, 0);
  if TestBit(AByte,0) then
    Result := 'CARTAO_CREDITO'
  else if TestBit(AByte,1) then
    Result := 'CARTAO_DEBITO'
  else if TestBit(AByte,2) then
    Result := 'CARTAO_VOUCHER'
  else if TestBit(AByte,3) then
    Result := 'CARTAO_PRIVATELABEL'
  else if TestBit(AByte,4) then
    Result := 'CARTAO_FROTA'
  else
    Result := 'CARTAO_DESCONHECIDO';
end;

function CardTypeToPWINFO_CARDTYPE(const CardType: String): Byte;
begin
  // 1: crédito 2: débito 4: voucher/PAT 8: private label 16: frota 128: outros
  if (CardType = 'CARTAO_CREDITO') then
    Result := 1
  else if (CardType = 'CARTAO_DEBITO') then
    Result := 2
  else if (CardType = 'CARTAO_VOUCHER') then
    Result := 4
  else if (CardType = 'CARTAO_PRIVATELABEL') then
    Result := 8
  else if (CardType = 'CARTAO_FROTA') then
    Result := 16
  else
    Result := 0;
end;

function PWINFO_FINTYPEToFinType(const FINTYPE: String): String;
var
  AByte: Integer;
begin
  // 1: à vista 2: parcelado pelo emissor 4: parcelado pelo estabelecimento 8: pré-datado
  AByte := StrToIntDef(FINTYPE, 0);
  case AByte of
     1: Result := 'A_VISTA';
     2: Result := 'PARCELADO_EMISSOR';
     4: Result := 'PARCELADO_ESTABELECIMENTO';
     8: Result := 'PRE_DATADO';
    16: Result := 'CREDITO_EMISSOR';
  else
    Result := 'FINANCIAMENTO_NAO_DEFINIDO';
  end;
end;

function FinancingTypeToPWINFO_FINTYPE(const financingType: String): Byte;
begin
  if (financingType = 'A_VISTA') then
    Result := 1
  else if (financingType = 'PARCELADO_EMISSOR') then
    Result := 2
  else if (financingType = 'PARCELADO_ESTABELECIMENTO') then
    Result := 4
  else if (financingType = 'PRE_DATADO') then
    Result := 8
  else if (financingType = 'CREDITO_EMISSOR') then
    Result := 16
  else
    Result := 0;
end;


function PWINFO_PAYMNTTYPEToPaymentType(const PAYMNTTYPE: String): String;
var
  AByte: Integer;
begin
  // Modalidade de pagamento: 1: cartão 2: dinheiro 4: cheque 8: carteira virtual
  AByte := StrToIntDef(PAYMNTTYPE, 0);
  case AByte of
     1: Result := 'PAGAMENTO_CARTAO';
     2: Result := 'PAGAMENTO_DINHEIRO';
     4: Result := 'PAGAMENTO_CHEQUE';
     8: Result := 'PAGAMENTO_CARTEIRA_VIRTUAL';
  else
    Result := '';
  end;
end;

function PaymentModeToPWINFO_PAYMNTTYPE(const paymentMode: String): Byte;
begin
  // Modalidade de pagamento: 1: cartão 2: dinheiro 4: cheque 8: carteira virtual
  if (paymentMode = 'PAGAMENTO_CARTAO') then
    Result := 1
  else if (paymentMode = 'PAGAMENTO_DINHEIRO') then
    Result := 2
  else if (paymentMode = 'PAGAMENTO_CHEQUE') then
    Result := 4
  else if (paymentMode = 'PAGAMENTO_CARTEIRA_VIRTUAL') then
    Result := 8
  else
    Result := 0;
end;


function PrintReceiptsToPWINFO_RCPTPRN(const printReceipts: String): Byte;
begin
  // 0: não há comprovante
  // 1: imprimir somente a via do Cliente
  // 2: imprimir somente a via do Estabelecimento
  // 3: imprimir ambas as vias do Cliente e do Estabelecimento
  if (printReceipts = 'VIA_NENHUMA') then
    Result := 0
  else if (printReceipts = 'VIA_CLIENTE') then
    Result := 1
  else if (printReceipts = 'VIA_ESTABELECIMENTO') then
    Result := 2
  else if (printReceipts = 'VIA_CLIENTE_E_ESTABELECIMENTO') then
    Result := 3
  else
    Result := 0;
end;

function WalletUserIdToPWINFO_WALLETUSERIDTYPE(const WalletUserId: String): Byte;
begin
  // Forma de identificação do portador da carteira virtual:
  // 1: QRCode do checkout (lido pelo celular do portador) 2: CPF 128: outros
  if (WalletUserId = 'QRCODE') then
    Result := 1
  else if (WalletUserId = 'CPF') then
    Result := 2
  else
    Result := 128;
end;



{ TACBrURI }

constructor TACBrURI.Create(const AScheme, AAuthority: String; APath: String);
begin
  if AScheme.IsEmpty then
    raise Exception.Create('TACBrURI.Create, "Scheme" não informado');

  if AAuthority.IsEmpty then
    raise Exception.Create('TACBrURI.Create, "Authority" não informado');

  Create;
  fScheme := AScheme;
  fAuthority := AAuthority;
  fPath := APath;
end;

constructor TACBrURI.Create;
begin
  inherited Create;
  fParams := TACBrInformacoes.Create;

  Clear;
end;

destructor TACBrURI.Destroy;
begin
  fParams.Free;
  inherited;
end;

procedure TACBrURI.Clear;
begin
  fParams.Clear;
  fScheme := '';
  fAuthority := '';
  fPath := '';
end;

function TACBrURI.GetURI: String;
var
  AParam: TACBrInformacao;
  AValue, Params: string;
begin
  Result := fScheme + '://' +
            fAuthority;

  if not fPath.IsEmpty then
    Result := Result + '/'+ fPath;

  Result := String(EncodeURL(AnsiString(Result)));
  
  Params := '';
  if (fParams.Count > 0) then
  begin
    for AParam in fParams do
    begin
      if AParam.Tipo = tiBoolean then
        AValue := ifthen(AParam.AsBoolean, 'true', 'false')
      else
        AValue := AParam.AsString;

      Params := Params + AParam.Nome+'='+String(EncodeURLElement(AnsiString(AValue))) + '&';
    end;

    Delete(Params, Params.Length, 1);  // Remove último &

    if (not Params.IsEmpty) then
      Params := '?' + Params;
  end;

  Result := Result + Params;
end;

procedure TACBrURI.SetURI(const AValue: String);
var
  p1, p2, le, i: Integer;
  UriStr, ParamStr: String;
  SL: TStringList;
begin
  Clear;
  UriStr := AValue;

  le := Length(UriStr);
  p1 := pos(':', UriStr);  // Achando o Schema
  if (p1 < 1) then
    raise Exception.Create('TACBrURI.SetURI, no Scheme definition');

  // Pula separador do Schema
  fScheme := copy(UriStr, 1, p1-1);
  while UriStr[p1].IsInArray([':','/']) do
    Inc(p1);

  // Achando os Parâmetros
  p2 := PosEx('?', UriStr, p1+1);
  if (p2 < 1) then
    p2 := le;

  ParamStr := copy(UriStr, p2+1, le );

  // Achando a Authority e Path
  fAuthority := copy(UriStr, p1, p2-p1);
  p1 := Pos('/', fAuthority);
  if (p1 > 0) then
  begin
    fPath := copy(fAuthority, p1+1, Length(fAuthority));
    fAuthority := copy(fAuthority, 1, p1-1);
  end;

  // Achando todos os Parâmetros
  if (ParamStr <> '') then
  begin
    SL := TStringList.Create;
    try
      SL.Text := ParamStr.Replace('&', sLineBreak);
      for i := 0 to SL.Count-1 do
        Params.AddField(SL.KeyNames[i]).AsString := SL.ValueFromIndex[i];
    finally
      SL.Free;
    end;
  end;
end;

{ TACBrTEFPGWebAndroidPersonalizacao }

constructor TACBrTEFPGWebAndroidPersonalizacao.Create;
begin
  inherited;
  Clear;
end;

procedure TACBrTEFPGWebAndroidPersonalizacao.Clear;
begin
  fcorFundoTela := 0;
  fcorFonte := 0;
  fcorTextoCaixaEdicao := 0;
  fcorFundoCaixaEdicao := 0;
  fcorSeparadorMenu := 0;
  fcorTeclaLiberadaTeclado := 0;
  fcorTeclaPressionadaTeclado := 0;
  fcorFundoToolbar := 0;
  fcorFonteTeclado := 0;
  fcorFundoTeclado := 0;
  fpathIconeToolbar := '';
  fpathFonte := '';
  ficoneToolbarBase64 := '';
  fFonteBase64 := '';
end;

procedure TACBrTEFPGWebAndroidPersonalizacao.SetPathFonte(const Value: String);
begin
  fpathFonte := Value;
  fFonteBase64 := '';
end;

function TACBrTEFPGWebAndroidPersonalizacao.GetfonteBase64: String;
begin
  if fFonteBase64.IsEmpty then
    ffonteBase64 := FileToBase64(fpathFonte);

  Result := ffonteBase64;
end;

procedure TACBrTEFPGWebAndroidPersonalizacao.SetPathIconeToolbar(
  const Value: String);
begin
  fpathIconeToolbar := Value;
  ficoneToolbarBase64 := '';
end;

function TACBrTEFPGWebAndroidPersonalizacao.GeticoneToolbarBase64: String;
begin
  if ficoneToolbarBase64.IsEmpty then
    ficoneToolbarBase64 := FileToBase64(fpathIconeToolbar);

  Result := ficoneToolbarBase64;
end;

function TACBrTEFPGWebAndroidPersonalizacao.FileToBase64(AFile: String): String;
var
  FS: TFileStream;
begin
  Result := '';
  if AFile.IsEmpty or (not FileExists(AFile)) then
    Exit;
  
  FS := TFileStream.Create(AFile, fmOpenRead);
  try
    FS.Position := 0;
    Result := EncodeBase64(ReadStrFromStream(FS, FS.Size));
  finally
    FS.Free;
  end;
end;

function TACBrTEFPGWebAndroidPersonalizacao.GetURI_Personalizacao: String;
  function AlphaColorToRGBHex(AColor: TAlphaColor): String;
  var
    rgbValue: Cardinal;
  begin
    Result := '#'+
              IntToHex(TAlphaColorRec(AColor).R, 2) +
              IntToHex(TAlphaColorRec(AColor).G, 2) +
              IntToHex(TAlphaColorRec(AColor).B, 2);
  end;
var
  AURI: TACBrURI;
begin
  AURI := TACBrURI.Create(URI_Scheme, URI_AuthorityPayment, 'posCustomization');
  try
    if (corFundoTela <> 0) then
      AURI.Params.AddField('screenBackgroundColor').AsString := AlphaColorToRGBHex(corFundoTela);
    if (corFundoTeclado <> 0) then
      AURI.Params.AddField('keyboardBackgroundColor').AsString := AlphaColorToRGBHex(corFundoTeclado);
    if (corFundoToolbar <> 0) then
      AURI.Params.AddField('toolbarBackgroundColor').AsString := AlphaColorToRGBHex(corFundoToolbar);
    if (corFonte <> 0) then
     AURI.Params.AddField('fontColor').AsString := AlphaColorToRGBHex(corFonte);
    if (corFundoCaixaEdicao <> 0) then
      AURI.Params.AddField('editboxBackgroundColor').AsString := AlphaColorToRGBHex(corFundoCaixaEdicao);
    if (corTeclaLiberadaTeclado <> 0) then
      AURI.Params.AddField('releasedKeyColor').AsString := AlphaColorToRGBHex(corTeclaLiberadaTeclado);
    if (corTeclaPressionadaTeclado <> 0) then
      AURI.Params.AddField('pressedKeyColor').AsString := AlphaColorToRGBHex(corTeclaPressionadaTeclado);
    if (corFonteTeclado <> 0) then
      AURI.Params.AddField('keyboardFontColor').AsString := AlphaColorToRGBHex(corFonteTeclado);
    if (corSeparadorMenu <> 0) then
      AURI.Params.AddField('menuSeparatorColor').AsString := AlphaColorToRGBHex(corSeparadorMenu);
    if (not iconeToolbarBase64.IsEmpty) then
      AURI.Params.AddField('toolbarIcon').AsString := iconeToolbarBase64;
    if (not fonteBase64.IsEmpty) then
      AURI.Params.AddField('font').AsString := fonteBase64;

    if (corTextoCaixaEdicao <> 0) then
      AURI.Params.AddField('editColorText').AsString := AlphaColorToRGBHex(corTextoCaixaEdicao);

    Result := AURI.URI;
  finally
    AURI.Free;
  end;
end;

{ TACBrTEFPGWebAndroidDadosAutomacao }

constructor TACBrTEFPGWebAndroidDadosAutomacao.Create;
begin
  inherited;
  Clear;
end;

procedure TACBrTEFPGWebAndroidDadosAutomacao.Clear;
begin
  fSoftwareHouse := '';
  fNomeAplicacao := '';
  fVersaoAplicacao := '';
  fImprimeViaClienteReduzida := True;
  fSuportaDesconto := False;
  fSuportaSaque := False;
  fSuportaViasDiferenciadas := True;
  fUtilizaSaldoTotalVoucher := False;
end;

procedure TACBrTEFPGWebAndroidDadosAutomacao.SetNomeAplicacao(
  const AValue: String);
begin
  if fNomeAplicacao = AValue then Exit;
  fNomeAplicacao := LeftStr(Trim(AValue),128);
end;

procedure TACBrTEFPGWebAndroidDadosAutomacao.SetSoftwareHouse(
  const AValue: String);
begin
  if fSoftwareHouse = AValue then Exit;
  fSoftwareHouse := LeftStr(Trim(AValue),128);
end;

procedure TACBrTEFPGWebAndroidDadosAutomacao.SetVersaoAplicacao(
  const AValue: String);
begin
  if fVersaoAplicacao = AValue then Exit;
  fVersaoAplicacao := LeftStr(Trim(AValue),128);
end;

function TACBrTEFPGWebAndroidDadosAutomacao.CalcularCapacidadesDaAutomacao: Integer;
begin
  Result := 4;            // 4: valor fixo, sempre incluir;
  if fSuportaSaque then
    Inc(Result, 1);       // 1: funcionalidade de troco/saque;
  if fSuportaDesconto then
    Inc(Result, 2);       // 2: funcionalidade de desconto;
  if fSuportaViasDiferenciadas then
    Inc(Result, 8);       // 8: impressão das vias diferenciadas do comprovante para Cliente/Estabelecimento;
  if fImprimeViaClienteReduzida then
    Inc(Result, 16);      // 16: impressão do cupom reduzido
  if fUtilizaSaldoTotalVoucher then
    Inc(Result, 32);      // 32: utilização de saldo total do voucher para abatimento do valor da compra
end;

function TACBrTEFPGWebAndroidDadosAutomacao.GetURI_DadosAutomacao(
  ParametrosAdicionais: TACBrTEFParametros): String;
var
  AURI: TACBrURI;
  ANomeAplicacao, AVersaoAplicacao, ASoftwareHouse: string;
  ACapacidadesDaAutomacao: Integer;
  ASuportaSaque, ASuportaDesconto, ASuportaViasDiferenciadas, AImprimeViaClienteReduzida,
    AUtilizaSaldoTotalVoucher: Boolean;
begin
  ANomeAplicacao := IfEmptyThen(ParametrosAdicionais.ValueInfo[PWINFO_AUTNAME], fNomeAplicacao);
  AVersaoAplicacao := IfEmptyThen(ParametrosAdicionais.ValueInfo[PWINFO_AUTVER], fVersaoAplicacao);
  ASoftwareHouse := IfEmptyThen(ParametrosAdicionais.ValueInfo[PWINFO_AUTDEV], fSoftwareHouse);

  ACapacidadesDaAutomacao := StrToIntDef(ParametrosAdicionais.ValueInfo[PWINFO_AUTCAP], CalcularCapacidadesDaAutomacao);
  ASuportaSaque := TestBit(ACapacidadesDaAutomacao, 0);
  ASuportaDesconto := TestBit(ACapacidadesDaAutomacao, 1);
  ASuportaViasDiferenciadas := TestBit(ACapacidadesDaAutomacao, 3);
  AImprimeViaClienteReduzida := TestBit(ACapacidadesDaAutomacao, 4);
  AUtilizaSaldoTotalVoucher := TestBit(ACapacidadesDaAutomacao, 5);

  AURI := TACBrURI.Create(URI_Scheme, URI_AuthorityPayment, 'posData');
  try
    AURI.Params.AddField('posName').AsString := ANomeAplicacao;
    AURI.Params.AddField('posVersion').AsString := AVersaoAplicacao;
    AURI.Params.AddField('posDeveloper').AsString := ASoftwareHouse;
    AURI.Params.AddField('allowCashback').AsBoolean := ASuportaSaque;
    AURI.Params.AddField('allowDiscount').AsBoolean := ASuportaDesconto;
    AURI.Params.AddField('allowDifferentReceipts').AsBoolean := ASuportaViasDiferenciadas;
    AURI.Params.AddField('allowShortReceipt').AsBoolean := AImprimeViaClienteReduzida;
    AURI.Params.AddField('allowDueAmount').AsBoolean := AUtilizaSaldoTotalVoucher;

    Result := AURI.URI;
  finally
    AURI.Free;
  end;
end;

{ TACBrTEFPGWebAndroid }

constructor TACBrTEFPGWebAndroid.Create;
begin
  inherited;

  fInicializada := False;
  fConfirmarTransacoesPendentesNoHost := True;

  fCNPJEstabelecimento := '';
  fNomeEstabelecimento := '';

  fOnGravarLog := Nil;
  fOnAntesIniciarTransacao := Nil;
  fOnDepoisTerminarTransacao := Nil;
  fOnAvaliarTransacaoPendente := Nil;

  fDadosAutomacao := TACBrTEFPGWebAndroidDadosAutomacao.Create;
  fPersonalizacao := TACBrTEFPGWebAndroidPersonalizacao.Create;

  fDadosTransacao := TACBrTEFParametros.Create;
  fParametrosAdicionais := TACBrTEFParametros.Create;
  fOutputURI := TACBrURI.Create;
  fDadosPendentesURI := TACBrURI.Create;
end;

destructor TACBrTEFPGWebAndroid.Destroy;
begin
  fDadosAutomacao.Free;
  fPersonalizacao.Free;

  fDadosTransacao.Free;
  fParametrosAdicionais.Free;
  fOutputURI.Free;
  fDadosPendentesURI.Free;

  inherited;
end;

procedure TACBrTEFPGWebAndroid.SetInicializada(AValue: Boolean);
begin
  if fInicializada = AValue then
    Exit;

  GravarLog('TACBrTEFPGWebAndroid.SetInicializada( '+BoolToStr(AValue, True)+' )');

  if AValue then
    Inicializar
  else
    DesInicializar;
end;

procedure TACBrTEFPGWebAndroid.Inicializar;
begin
  GravarLog('TACBrTEFPGWebAndroid.Inicializar');
  MainActivity.registerIntentAction(StringToJString(Intent_SERVICO));
  TMessageManager.DefaultManager.SubscribeToMessage(TMessageReceivedNotification, HandleActivityMessage);
  fInicializada := True;
  fEmTransacao := False;
end;

procedure TACBrTEFPGWebAndroid.DesInicializar;
begin
  GravarLog('TACBrTEFPGWebAndroid.DesInicializar');
  TMessageManager.DefaultManager.Unsubscribe(TMessageReceivedNotification, HandleActivityMessage);
  fInicializada := False;
  fEmTransacao := False;
end;

procedure TACBrTEFPGWebAndroid.HandleActivityMessage(const Sender: TObject;
  const M: TMessage);
begin
  GravarLog('TACBrTEFPGWebAndroid.HandleActivityMessage');
  if (M is TMessageResultNotification) then
  begin
    OnActivityResult( TMessageResultNotification(M).RequestCode,
                      TMessageResultNotification(M).ResultCode,
                      TMessageResultNotification(M).Value);
  end
  else if (M is TMessageReceivedNotification) then
    OnActivityResult( 0, 0, TMessageReceivedNotification(M).Value);
end;

function TACBrTEFPGWebAndroid.OnActivityResult(RequestCode, ResultCode: Integer; AIntent: JIntent): Boolean;
var
  DataStr, KeyStr, BundleStr: string;
  ABundle: JBundle;
  Keys: JSet;
  Key: JString;
  it: JIterator;
begin
  Result := False;
  fEmTransacao := False;
  GravarLog(Format('TACBrTEFPGWebAndroid.OnActivityResult: RequestCode: %d, ResultCode: %d',
            [RequestCode, ResultCode]));

  if not Assigned(AIntent) then
  begin
    GravarLog('   no Intent');
    Exit;
  end;

  DataStr := JStringToString(AIntent.getDataString);
  GravarLog( 'Uri: '+DataStr);

  ABundle := AIntent.getExtras;
  if Assigned(ABundle) then
  begin
    GravarLog( 'Bundle:' );
    Keys := ABundle.keySet;
    if Assigned(Keys) then
    begin
      it := Keys.iterator;
      while it.hasNext do
      begin
        Key := JString(it.next());
        KeyStr := JStringToString(Key);
        BundleStr := JStringToString(ABundle.getString(Key));
        GravarLog( '   '+KeyStr+':'+BundleStr );

        if (KeyStr = Key_TransacaoPendenteDados) then
        try
          fDadosPendentesURI.URI := BundleStr;
        except
          On E: Exception do
            GravarLog('   DadosPendentesURI - '+E.ClassName+' - '+E.Message);
        end;
      end;
    end
    else
      GravarLog( '   no Bundle.keySet');
  end
  else
    GravarLog( '   no Bundle');

//if (RequestCode = Intent_ResultCode) then
//begin
//if ResultCode = TJActivity.JavaClass.RESULT_OK then
//begin
    try
      fOutputURI.URI := DataStr;
      Result := True;

      ObterDadosDaTransacao;  // Mapeia as respostas da URI e Bundles para as PWINFOs

      if Assigned(fOnDepoisTerminarTransacao) then
        fOnDepoisTerminarTransacao(AIntent);

      if (fDadosPendentesURI.Params.Count > 0) then
        TratarTransacaoPendente;
    except
      On E: Exception do
        GravarLog('   OutputURI - '+E.ClassName+' - '+E.Message);
    end;
//end
//else if ResultCode = TJActivity.JavaClass.RESULT_CANCELED then
//  DoException(ACBrStr(sErrCANCELED));
//end;
end;

procedure TACBrTEFPGWebAndroid.AddURIParam(AURI: TACBrURI; ParamName: string;
  FromInfo: Word; ValorPadrao: string = ''; IncluirSeVazio: Boolean = False);
var
  AValue: string;
begin
  AValue := IfEmptyThen(ParametrosAdicionais.ValueInfo[FromInfo], ValorPadrao);
  if IncluirSeVazio or (not AValue.IsEmpty) then
    AURI.Params.AddField(ParamName).AsString := AValue;
end;

procedure TACBrTEFPGWebAndroid.AdicionarDadosObrigatorios;
begin
  GravarLog('TACBrTEFPGWebAndroid.AdicionarDadosObrigatorios');
  ParametrosAdicionais.ValueInfo[PWINFO_AUTNAME] := fDadosAutomacao.NomeAplicacao;
  ParametrosAdicionais.ValueInfo[PWINFO_AUTVER] := fDadosAutomacao.VersaoAplicacao;
  ParametrosAdicionais.ValueInfo[PWINFO_AUTDEV] := fDadosAutomacao.SoftwareHouse;
  ParametrosAdicionais.ValueInfo[PWINFO_AUTCAP] := IntToStr(fDadosAutomacao.CalcularCapacidadesDaAutomacao);
  ParametrosAdicionais.ValueInfo[PWINFO_MERCHADDDATA4] := CACBrTEFPGWebAndroidName+' '+CACBrTEFPGWebAndroidVersao;
end;

function TACBrTEFPGWebAndroid.BooleanStrToByte(const AValue: String): Byte;
begin
  if (LowerCase(AValue) = 'true') then
    Result := 1
  else
    Result := 0;
end;

function TACBrTEFPGWebAndroid.GerarTransactionId: String;
begin
  Result := FormatDateTime('YYYYMMDDHHNNSS', Now);
end;

function TACBrTEFPGWebAndroid.GetURI_Input(iOPER: Byte): String;
var
  AURI: TACBrURI;
begin
  AURI := TACBrURI.Create(URI_Scheme, URI_AuthorityPayment, URI_PathInput);
  try
    AURI.Params.AddField('operation').AsString := PWOPER_ToOperation(iOPER);
    AddURIParam(AURI, 'transactionId', PWINFO_FISCALREF, GerarTransactionId);
    if (ParametrosAdicionais.ValueInfo[PWINFO_TOTAMNT] <> '') then
    begin
      AURI.Params.AddField('amount').AsString := ParametrosAdicionais.ValueInfo[PWINFO_TOTAMNT];
      AURI.Params.AddField('currencyCode').AsString :=
        IfEmptyThen( ParametrosAdicionais.ValueInfo[PWINFO_CURRENCY], '986');
    end;

    AddURIParam(AURI, 'boardingTax', PWINFO_BOARDINGTAX);
    AddURIParam(AURI, 'serviceTax', PWINFO_TIPAMOUNT);

    if (ParametrosAdicionais.ValueInfo[PWINFO_AUTHSYST] <> '') then
      AURI.Params.AddField('provider').AsString := PWINFO_AUTHSYSTToProviderName(ParametrosAdicionais.ValueInfo[PWINFO_AUTHSYST]);

    if (ParametrosAdicionais.ValueInfo[PWINFO_CARDTYPE] <> '') then
      AURI.Params.AddField('cardType').AsString := PWINFO_CARDTYPEToCardType(ParametrosAdicionais.ValueInfo[PWINFO_CARDTYPE]);

    if (ParametrosAdicionais.ValueInfo[PWINFO_FINTYPE] <> '') then
      AURI.Params.AddField('finType').AsString := PWINFO_FINTYPEToFinType(ParametrosAdicionais.ValueInfo[PWINFO_FINTYPE]);

    if (ParametrosAdicionais.ValueInfo[PWINFO_PAYMNTTYPE] <> '') then
      AURI.Params.AddField('paymentMode').AsString := PWINFO_PAYMNTTYPEToPaymentType(ParametrosAdicionais.ValueInfo[PWINFO_PAYMNTTYPE]);

    AddURIParam(AURI, 'installments', PWINFO_INSTALLMENTS);
    AddURIParam(AURI, 'predatedDate', PWINFO_INSTALLMDATE);
    AddURIParam(AURI, 'fiscalDocument', PWINFO_FISCALREF);
    AddURIParam(AURI, 'taxId', PWINFO_MERCHCNPJCPF);
    AddURIParam(AURI, 'billNumber', PWINFO_BARCODE);
    //AddURIParam(AURI, 'invoiceNumber', PWINFO_FISCALREF);
    AddURIParam(AURI, 'phoneNumber', PWINFO_PHONEFULLNO);
    AddURIParam(AURI, 'posId', PWINFO_POSID);
    AddURIParam(AURI, 'originalAuthorizationCode', PWINFO_TRNORIGAUTH);
    AddURIParam(AURI, 'originalAuthorizationCode', PWINFO_TRNORIGAUTHCODE);    // Dá preferencia a PWINFO_TRNORIGAUTHCODE a PWINFO_TRNORIGAUTH
    AddURIParam(AURI, 'originalTransactionNsu', PWINFO_TRNORIGNSU);
    AddURIParam(AURI, 'originalTransactionDateTime', PWINFO_TRNORIGDATE);
    AddURIParam(AURI, 'originalTransactionDateTime', PWINFO_TRNORIGDATETIME);  // Dá preferência a PWINFO_TRNORIGDATETIME a PWINFO_TRNORIGDATE
    AddURIParam(AURI, 'amount', PWINFO_TRNORIGAMNT);                           // Dá preferencia a PWINFO_TRNORIGAMNT a PWINFO_TOTAMNT
    AddURIParam(AURI, 'aditionalPosData1', PWINFO_MERCHADDDATA1);
    AddURIParam(AURI, 'aditionalPosData2', PWINFO_MERCHADDDATA2);
    AddURIParam(AURI, 'aditionalPosData3', PWINFO_MERCHADDDATA3);
    AddURIParam(AURI, 'aditionalPosData4', PWINFO_MERCHADDDATA4);

    Result := AURI.URI;
  finally
    AURI.Free;
  end;
end;

procedure TACBrTEFPGWebAndroid.DoException(AErrorMsg: String);
begin
  if (Trim(AErrorMsg) = '') then
    Exit;

  GravarLog('EACBrTEFPayGoWeb: '+AErrorMsg);
  raise EACBrTEFPGWebAndroid.Create(AErrorMsg);
end;

procedure TACBrTEFPGWebAndroid.IniciarTransacao(iOPER: Byte;
  ParametrosAdicionaisTransacao: TACBrTEFParametros);
var
  uriTransacao, uriDadosAutomacao, uriPersonalizacao: String;
  i: Integer;
  intent: JIntent;
begin
  if (not fInicializada) then
    DoException(ACBrStr(sErrNOTINIT));

  if fEmTransacao then
    DoException(ACBrStr(sErrTRNINIT));

  AdicionarDadosObrigatorios;

  // Copiando ParametrosAdicionaisTransacao para "ParametrosAdicionais"
  if Assigned(ParametrosAdicionaisTransacao) then
  begin
    For i := 0 to ParametrosAdicionaisTransacao.Count-1 do
      ParametrosAdicionais.Values[ParametrosAdicionaisTransacao.KeyNames[i]] :=
        ParametrosAdicionaisTransacao.ValueFromIndex[i];
  end;

  GravarLog('TACBrTEFPGWebAndroid.IniciarTransacao');

  // Calculando a URI de Input
  uriTransacao := GetURI_Input(iOPER);
  ParametrosAdicionais.Clear;  // Limpa para não usar nas próximas transações
  GravarLog('  URI: '+uriTransacao);

  // Criando a Intent
  intent := TJIntent.JavaClass.init( StringToJString(Intent_Payment),
                TJnet_Uri.JavaClass.parse(StringToJString(uriTransacao)));
  intent.setFlags( TJIntent.JavaClass.FLAG_ACTIVITY_NEW_TASK or
                   TJIntent.JavaClass.FLAG_ACTIVITY_CLEAR_TASK );

  // Adicionando os Bundles
  uriDadosAutomacao := fDadosAutomacao.GetURI_DadosAutomacao(fParametrosAdicionais);
  GravarLog('  '+Key_DadosAutomacao+': '+uriDadosAutomacao);
  intent.putExtra( StringToJString(Key_DadosAutomacao), StringToJString(uriDadosAutomacao));

  uriPersonalizacao := fPersonalizacao.GetURI_Personalizacao;
  GravarLog('  '+Key_Personalizacao+': '+uriPersonalizacao);
  intent.putExtra( StringToJString(Key_Personalizacao), StringToJString(uriPersonalizacao));

  GravarLog('  '+Key_Package+': '+JStringToString(TAndroidHelper.Activity.getPackageName) );
  intent.putExtra( StringToJString(Key_Package), TAndroidHelper.Activity.getPackageName );

  GravarLog('  '+Key_IntegracaoDireta+': true' );
  intent.putExtra( StringToJString(Key_IntegracaoDireta), True );

  // Disparando o Intent
  IniciarIntent(intent);
end;

procedure TACBrTEFPGWebAndroid.IniciarIntent(AIntent: JIntent);
var
  ResolveInfo: JResolveInfo;
begin
  ResolveInfo := TAndroidHelper.Activity.getPackageManager.resolveActivity(AIntent, 0);
  if (ResolveInfo = nil) then
  begin
    DoException(Format(ACBrStr(sErrINTENTNOTFOUND), [JStringToString(AIntent.getAction)] ));
    Exit;
  end;

  if Assigned(fOnAntesIniciarTransacao) then
    fOnAntesIniciarTransacao(AIntent);

  fEmTransacao := True;
  fDadosTransacao.Clear;
  fOutputURI.Clear;
  fDadosPendentesURI.Clear;

  TAndroidHelper.Activity.startActivity(AIntent)
end;

procedure TACBrTEFPGWebAndroid.ObterDadosDaTransacao;
var
  i: Integer;
  ParamKey, ParamValue: string;
  ExistsPending: Boolean;

  procedure AnalisarConfirmationTransactionId( AconfirmationTransactionId: String);
  var
    TransactionIdData: TArray<string>;

    procedure AtribuirSeDadoExistir(ADado: String; AInfo: Word);
    begin
      if ADado.Trim.IsEmpty or (ADado.ToLower = 'null') then
        Exit;

      fDadosTransacao.ValueInfo[AInfo] := ADado;
    end;
  begin
    TransactionIdData := AconfirmationTransactionId.Split(['.']);
    if (Length(TransactionIdData) = 5) then
    begin
      AtribuirSeDadoExistir( TransactionIdData[0], PWINFO_REQNUM );    // TACBrTEFResp.NumeroLoteTransacao
      AtribuirSeDadoExistir( TransactionIdData[1], PWINFO_AUTLOCREF ); // TACBrTEFResp.Finalizacao
      AtribuirSeDadoExistir( TransactionIdData[2], PWINFO_AUTEXTREF ); // TACBrTEFResp.NSU
      AtribuirSeDadoExistir( TransactionIdData[3], PWINFO_VIRTMERCH ); // TACBrTEFResp.Estabelecimento
      AtribuirSeDadoExistir( TransactionIdData[4], PWINFO_AUTHSYST );  // TACBrTEFResp.Rede
    end;
  end;

begin
  fDadosTransacao.Clear;
  ExistsPending := (fDadosPendentesURI.Params.Count > 0);

  if (fOutputURI.fScheme = URI_Scheme) and
     (fOutputURI.Authority = URI_AuthorityPayment) and
     (fOutputURI.Path = URI_PathOutput) then
  begin
    for i := 0 to fOutputURI.Params.Count-1 do
    begin
      ParamKey := fOutputURI.Params.Items[i].Nome;
      ParamValue := String(DecodeURL( AnsiString(fOutputURI.Params.Items[i].AsString) ));

      if (ParamKey = 'operation') then
        fDadosTransacao.ValueInfo[PWINFO_OPERATION] := IntToStr(OperationToPWOPER_(ParamValue))
      else if (ParamKey = 'posTransId') then
        fDadosTransacao.ValueInfo[PWINFO_FISCALREF] := ParamValue      // PWINFO_FISCALREF ?
      else if (ParamKey = 'transactionResult') then
        fDadosTransacao.ValueInfo[PWINFO_RET] := ParamValue
      else if (ParamKey = 'amount') then
        fDadosTransacao.ValueInfo[PWINFO_TOTAMNT] := ParamValue
      else if (ParamKey = 'currencyCode') then
        fDadosTransacao.ValueInfo[PWINFO_CURRENCY] := ParamValue
      else if (ParamKey = 'requiresConfirmation') then
        fDadosTransacao.ValueInfo[PWINFO_CNFREQ] := IntToStr(BooleanStrToByte(ParamValue))
      else if (ParamKey = 'confirmationTransactionId') then
      begin
        fDadosTransacao.ValueInfo[PWINFO_CONFTRANSIDENT] := ParamValue;
        AnalisarConfirmationTransactionId(ParamValue);
      end
      else if (ParamKey = 'cashbackAmount') then
        fDadosTransacao.ValueInfo[PWINFO_CASHBACKAMT] := ParamValue
      else if (ParamKey = 'discountAmount') then
        fDadosTransacao.ValueInfo[PWINFO_DISCOUNTAMT] := ParamValue
      else if (ParamKey = 'balanceVoucher') then
        fDadosTransacao.ValueInfo[PWINFO_SALDOVOUCHER] := ParamValue
      else if (ParamKey = 'dueAmount') then
        fDadosTransacao.ValueInfo[PWINFO_DUEAMNT] := ParamValue
      else if (ParamKey = 'fiscalDocument') and (ParamValue <> '') then
        fDadosTransacao.ValueInfo[PWINFO_FISCALREF] := ParamValue    // PWINFO_FISCALREF ?
      else if (ParamKey = 'transactionNsu') then
        fDadosTransacao.ValueInfo[PWINFO_AUTEXTREF] := ParamValue
      else if (ParamKey = 'terminalNsu') then
        fDadosTransacao.ValueInfo[PWINFO_AUTLOCREF] := ParamValue
      else if (ParamKey = 'authorizationCode') then
        fDadosTransacao.ValueInfo[PWINFO_AUTHCODE] := ParamValue
      else if (ParamKey = 'transactionId') and (ParamValue <> '') then
        fDadosTransacao.ValueInfo[PWINFO_FISCALREF] := ParamValue    // PWINFO_FISCALREF ?
      else if (ParamKey = 'merchantId') then
        fDadosTransacao.ValueInfo[PWINFO_VIRTMERCH] := ParamValue
      else if (ParamKey = 'posId') then
        fDadosTransacao.ValueInfo[PWINFO_POSID] := ParamValue
      else if (ParamKey = 'merchantName') then
        fDadosTransacao.ValueInfo[PWINFO_MERCHNAMEPDC] := ParamValue
      else if (ParamKey = 'transactionDateTime') then
        fDadosTransacao.ValueInfo[PWINFO_DATETIME] := ParamValue
      else if (ParamKey = 'installments') then
        fDadosTransacao.ValueInfo[PWINFO_INSTALLMENTS] := ParamValue
      else if (ParamKey = 'predatedDate') then
        fDadosTransacao.ValueInfo[PWINFO_INSTALLMDATE] := ParamValue
      else if (ParamKey = 'finType') then
        fDadosTransacao.ValueInfo[PWINFO_FINTYPE] := IntToStr(FinancingTypeToPWINFO_FINTYPE(ParamValue))
      else if (ParamKey = 'provider') then                          // Não documentado
        fDadosTransacao.ValueInfo[PWINFO_AUTHSYST] := ParamValue
      else if (ParamKey = 'providerName') then
        fDadosTransacao.ValueInfo[PWINFO_AUTHSYSTEXTENDED] := ParamValue
      else if (ParamKey = 'cardType') then
        fDadosTransacao.ValueInfo[PWINFO_CARDTYPE] := IntToStr(CardTypeToPWINFO_CARDTYPE(ParamValue))
      else if (ParamKey = 'cardEntryMode') then
        fDadosTransacao.ValueInfo[PWINFO_CARDENTMODE] := ParamValue
      else if (ParamKey = 'maskedPan') then
        fDadosTransacao.ValueInfo[PWINFO_CARDPARCPAN] := ParamValue
      else if (ParamKey = 'defaultMaskedPan') then
        fDadosTransacao.ValueInfo[PWINFO_DEFAULTCARDPARCPAN] := ParamValue
      else if (ParamKey = 'cardholderVerificationMode') then
        fDadosTransacao.ValueInfo[PWINFO_CHOLDVERIF] := ParamValue
      else if (ParamKey = 'cardName') then
        fDadosTransacao.ValueInfo[PWINFO_CARDNAME] := ParamValue
      else if (ParamKey = 'defaultCardName') then
        fDadosTransacao.ValueInfo[PWINFO_CARDNAMESTD] := ParamValue
      else if (ParamKey = 'cardholderName') then
        fDadosTransacao.ValueInfo[PWINFO_CHOLDERNAME] := ParamValue
      else if (ParamKey = 'aid') then
        fDadosTransacao.ValueInfo[PWINFO_AID] := ParamValue
      else if (ParamKey = 'resultMessage') then
        fDadosTransacao.ValueInfo[PWINFO_RESULTMSG] := UTF8ToNativeString(ParamValue)
      else if (ParamKey = 'authorizerResponse') then
        fDadosTransacao.ValueInfo[PWINFO_AUTRESPCODE] := ParamValue
      else if (ParamKey = 'printReceipts') then
        fDadosTransacao.ValueInfo[PWINFO_RCPTPRN] := IntToStr(PrintReceiptsToPWINFO_RCPTPRN(ParamValue))
      else if (ParamKey = 'fullReceipt') then
        fDadosTransacao.ValueInfo[PWINFO_RCPTFULL] := ParamValue
      else if (ParamKey = 'merchantReceipt') then
        fDadosTransacao.ValueInfo[PWINFO_RCPTMERCH] := ParamValue
      else if (ParamKey = 'cardholderReceipt') then
        fDadosTransacao.ValueInfo[PWINFO_RCPTCHOLDER] := ParamValue
      else if (ParamKey = 'shortReceipt') then
        fDadosTransacao.ValueInfo[PWINFO_RCPTCHSHORT] := ParamValue
      else if (ParamKey = 'graphicReceiptExists') then
        fDadosTransacao.ValueInfo[PWINFO_GRAPHICRCP] := IntToStr(BooleanStrToByte(ParamValue))
      else if (ParamKey = 'merchantGraphicReceipt') then
        fDadosTransacao.ValueInfo[PWINFO_MERCHGRARCP] := ParamValue
      else if (ParamKey = 'cardholderGraphicReceipt') then
        fDadosTransacao.ValueInfo[PWINFO_CHOLDERGRARCP] := ParamValue
      else if (ParamKey = 'originalTransactionAmount') then
        fDadosTransacao.ValueInfo[PWINFO_TRNORIGAMNT] := ParamValue
      else if (ParamKey = 'originalTransactionDateTime') then
        fDadosTransacao.ValueInfo[PWINFO_TRNORIGDATETIME] := ParamValue
      else if (ParamKey = 'originalTransactionNsu') then
        fDadosTransacao.ValueInfo[PWINFO_TRNORIGNSU] := ParamValue
      else if (ParamKey = 'originalAuthorizationCode') then
        fDadosTransacao.ValueInfo[PWINFO_TRNORIGAUTH] := ParamValue
      else if (ParamKey = 'originalTerminalNsu') then
        fDadosTransacao.ValueInfo[PWINFO_TRNORIGLOCREF] := ParamValue
      else if (ParamKey = 'pendingTransactionExists') then
        ExistsPending := True
      else if (ParamKey = 'authorizationMode') then
        fDadosTransacao.ValueInfo[PWINFO_ONOFF] := IfThen(LowerCase(ParamValue)='on','1','2')
      else if (ParamKey = 'paymentMode') then
        fDadosTransacao.ValueInfo[PWINFO_PAYMNTTYPE] := IntToStr(PaymentModeToPWINFO_PAYMNTTYPE(ParamValue))
      else if (ParamKey = 'walletUserId') then
        fDadosTransacao.ValueInfo[PWINFO_WALLETUSERIDTYPE] := IntToStr(WalletUserIdToPWINFO_WALLETUSERIDTYPE(ParamValue))
      else if (ParamKey = 'uniqueId') then
        fDadosTransacao.ValueInfo[PWINFO_UNIQUEID] := ParamValue;
    end;

    if ExistsPending then
    begin
      for i := 0 to fDadosPendentesURI.Params.Count-1 do
      begin
        ParamKey := fDadosPendentesURI.Params.Items[i].Nome;
        ParamValue := fDadosPendentesURI.Params.Items[i].AsString;

        if (ParamKey = 'providerName') then
          fDadosTransacao.ValueInfo[PWINFO_PNDAUTHSYST] := ProviderNameToPWINFO_AUTHSYST(ParamValue)
        else if (ParamKey = 'merchantId') then
          fDadosTransacao.ValueInfo[PWINFO_PNDVIRTMERCH] := ParamValue
        else if (ParamKey = 'localNsu') then
          fDadosTransacao.ValueInfo[PWINFO_PNDAUTLOCREF] := ParamValue
        else if (ParamKey = 'transactionNsu') then
          fDadosTransacao.ValueInfo[PWINFO_PNDREQNUM] := ParamValue
        else if (ParamKey = 'hostNsu') then
          fDadosTransacao.ValueInfo[PWINFO_PNDAUTEXTREF] := ParamValue;
      end;
    end;
  end;
end;

function TACBrTEFPGWebAndroid.ObterUltimoRetorno: String;
begin
  if (fDadosTransacao.Count < 1) then
    ObterDadosDaTransacao;

  Result := fDadosTransacao.ValueInfo[PWINFO_RESULTMSG];
end;

procedure TACBrTEFPGWebAndroid.GravarLog(const AString: AnsiString;
  Traduz: Boolean);
Var
  Tratado: Boolean;
  AStringLog: AnsiString;
begin
  if not Assigned(fOnGravarLog) then
    Exit;

  if Traduz then
    AStringLog := AnsiString(TranslateUnprintable(AString))
  else
    AStringLog := AString;

  Tratado := False;
  fOnGravarLog(String(AStringLog), Tratado);
end;

procedure TACBrTEFPGWebAndroid.GravarLog(const AString: String;
  Traduz: Boolean);
begin
  GravarLog(AnsiString(AString), Traduz);
end;

procedure TACBrTEFPGWebAndroid.SetCNPJEstabelecimento(AValue: String);
var
  ACNPJ, ErroMsg: String;
begin
  if fCNPJEstabelecimento = AValue then
    Exit;

  ACNPJ := OnlyNumber(AValue);
  if (ACNPJ <> '') then
  begin
    ErroMsg := ACBrValidador.ValidarCNPJ(ACNPJ);
    if (ErroMsg <> '') then
      DoException('SetCNPJEstabelecimento: '+ErroMsg);
  end;

  fCNPJEstabelecimento := ACNPJ;
end;

procedure TACBrTEFPGWebAndroid.SetNomeEstabelecimento(AValue: String);
begin
  if fNomeEstabelecimento = AValue then Exit;
  fNomeEstabelecimento := LeftStr(Trim(AValue),100);
end;

procedure TACBrTEFPGWebAndroid.ConfirmarTransacao(AStatus: LongWord;
  confirmTransactionIdentifier: String);
var
  AURI: TACBrURI;
  uriConfirmation: String;
  intent: JIntent;
begin
  if confirmTransactionIdentifier.IsEmpty then
    confirmTransactionIdentifier := fDadosTransacao.ValueInfo[PWINFO_CONFTRANSIDENT];

  GravarLog('TACBrTEFPGWebAndroid.ConfirmarTransacao('+
    IntToStr(AStatus)+', '+confirmTransactionIdentifier );
  if Trim(confirmTransactionIdentifier).IsEmpty then
    Exit;

  // Adicionando os Bundles
  AURI := TACBrURI.Create(URI_Scheme, URI_AuthorityConfirmation, URI_PathConfirmation);
  try
    AURI.Params.AddField('confirmationTransactionId').AsString := confirmTransactionIdentifier;
    AURI.Params.AddField('transactionStatus').AsString := PWCNF_ToTransactionStatus(AStatus);
    uriConfirmation := AURI.URI;
  finally
    AURI.Free;
  end;
  GravarLog('  uriConfirmation: '+uriConfirmation);

  // Criando a Intent
  intent := TJIntent.Create;
  intent.setAction( StringToJString(Intent_Confirmation) );
  intent.putExtra( StringToJString(Key_URI),  StringToJString(uriConfirmation) );
  intent.addFlags( TJIntent.JavaClass.FLAG_INCLUDE_STOPPED_PACKAGES );

  // Disparando o Intent
  TAndroidHelper.Activity.sendBroadcast(intent);
end;

procedure TACBrTEFPGWebAndroid.TratarTransacaoPendente(AStatus: LongWord;
  pszReqNum: String; pszLocRef: String; pszExtRef: String; pszVirtMerch: String;
  pszAuthSyst: String);
begin
  if pszAuthSyst.IsEmpty then
    pszAuthSyst := fDadosTransacao.ValueInfo[PWINFO_PNDAUTHSYST];

  if pszVirtMerch.IsEmpty then
    pszVirtMerch := fDadosTransacao.ValueInfo[PWINFO_PNDVIRTMERCH];

  if pszReqNum.IsEmpty then
    pszReqNum := fDadosTransacao.ValueInfo[PWINFO_PNDREQNUM];

  if pszLocRef.IsEmpty then
    pszLocRef := fDadosTransacao.ValueInfo[PWINFO_PNDAUTLOCREF];

  if pszExtRef.IsEmpty then
    pszExtRef := fDadosTransacao.ValueInfo[PWINFO_PNDAUTEXTREF];

  if (AStatus = 0) then
  begin
    if fConfirmarTransacoesPendentesNoHost then
      AStatus := PWCNF_CNF_MANU_AUT
    else
      AStatus := PWCNF_REV_MANU_AUT;
  end;

  if Trim(pszAuthSyst+pszVirtMerch+pszReqNum+pszLocRef+pszExtRef).IsEmpty then
    Exit;

  if Assigned(fOnAvaliarTransacaoPendente) then
    fOnAvaliarTransacaoPendente(pszReqNum, pszLocRef, pszExtRef, pszVirtMerch, pszAuthSyst)
  else
    ResolverTransacaoPendente(AStatus, pszReqNum, pszLocRef, pszExtRef, pszVirtMerch, pszAuthSyst);
end;


procedure TACBrTEFPGWebAndroid.ResolverTransacaoPendente(AStatus: LongWord;
  pszReqNum, pszLocRef, pszExtRef, pszVirtMerch, pszAuthSyst: String);
var
  AURI: TACBrURI;
  uriConfirmation, uriResolve: String;
  intent: JIntent;
begin
  GravarLog('ResolverTransacaoPendente( '+PWCNFToString(AStatus)+', '+
                                           pszReqNum+', '+
                                           pszLocRef+', '+
                                           pszExtRef+', '+
                                           pszVirtMerch+', '+
                                           pszAuthSyst+' ) ');

  if Trim(pszAuthSyst+pszVirtMerch+pszReqNum+pszLocRef+pszExtRef).IsEmpty then
    Exit;

  AURI := TACBrURI.Create(URI_Scheme, URI_AuthorityResolve, URI_PathPendingTransaction);
  try
    AddURIParam(AURI, 'transactionId', PWINFO_FISCALREF, GerarTransactionId);

    if (pszVirtMerch <> '') then
      AURI.Params.AddField('merchantId').AsString := pszVirtMerch;

    if (pszAuthSyst <> '') then
      AURI.Params.AddField('providerName').AsString := PWINFO_AUTHSYSTToProviderName(pszAuthSyst);

    if (pszExtRef <> '') then
      AURI.Params.AddField('hostNsu').AsString := pszExtRef;

    if (pszLocRef <> '') then
      AURI.Params.AddField('localNsu').AsString := pszLocRef;

    if (pszReqNum <> '') then
      AURI.Params.AddField('transactionNsu').AsString := pszReqNum;

    uriResolve := AURI.URI;
  finally
    AURI.Free;
  end;

  GravarLog('  uriResolve: '+uriResolve);

  // Criando a Intent
  intent := TJIntent.Create;
  intent.setAction( StringToJString(Intent_Confirmation) );
  intent.putExtra( StringToJString(Key_URI),  StringToJString(uriResolve) );

  intent.addFlags( TJIntent.JavaClass.FLAG_INCLUDE_STOPPED_PACKAGES );

  // Adicionando os Bundles
  AURI := TACBrURI.Create(URI_Scheme, URI_AuthorityResolve, URI_PathConfirmation);
  try
    AURI.Params.AddField('transactionStatus').AsString := PWCNF_ToTransactionStatus(AStatus);
    uriConfirmation := AURI.URI;
  finally
    AURI.Free;
  end;

  GravarLog('  uriConfirmation: '+uriConfirmation);
  intent.putExtra( StringToJString(Key_Confirmacao), StringToJString(uriConfirmation) );

  // Disparando o Intent
  TAndroidHelper.Activity.sendBroadcast(intent);
end;

end.
