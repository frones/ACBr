{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2025 Daniel Simoes de Almeida               }
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

unit ACBrTEFTPagAPI;

interface

uses
  Classes, SysUtils,
  ACBrBase;

const
  {$IFDEF MSWINDOWS}
   CTPagLib = 'libDM_SDK.dll';
  {$ELSE}
   CTPagLib = 'libDM_SDK.so';
  {$ENDIF}

resourcestring
  sErrLibJaInicializada = 'Biblioteca TPag já foi inicializada';
  sErrEventoNaoAtribuido = 'Evento %s não atribuido';
  sErrCNPJNaoInformado = 'CNPJEmpresa não informado';
  sErrNaoInicializada = 'Biblioteca TPag não inicializada';
  sErrNSUNaoInformado = 'NSU não informado';

type
  EACBrTEFTPagAPI = class(Exception);


  TPagNotificationType = (
    FREE_DSP,
    DSP_2X16,
    PROCESSING_DSP,
    INSERT_PASS_CARD,
    INSERT_CARD,
    APPROACH_INSERT_CARD,
    APPROACH_INSERT_PASS_CARD,
    APPROACH_CARD,
    PASS_CARD,
    SELECT,
    SELECTED,
    INVALID_APP,
    INVALID_PASSWORD,
    LAST_PASSWORD_ATTEMPT,
    BLOCKED_PASSWORD,
    PASSWORD_VERIFIED,
    BLOCKED_CARD_DSP,
    REMOVE_CARD,
    UPDATING_TABLES,
    REPRESENT_CARD,
    START_PIN,
    END_PIN,
    ERROR_MESSAGE,
    PIN_INSERT,
    SUCCESS_MESSAGE );

  TPagReturnCodes = (
    OK = 0,
    INTERNAL_ERROR = 1,
    INVALID_CALL = 10,
    INVALID_PARAMETER = 11,
    TIMEOUT = 12,
    CANCELED_OPERATION = 13,
    BUSY_PINPAD = 16,
    INVALID_MODEL = 17,
    EXPIRED_TABLES = 20,
    MAG_CARD_READ_ERROR = 41,
    MISSING_PIN_KEY = 42,
    MISSING_CARD = 43,
    SAM_MODULE_ERROR = 50,
    INVALID_SAM = 51,
    MISSING_SAM = 52,
    MUTE_CARD = 60,
    CARD_COMMUNICATION_ERROR = 61,
    CARD_WITH_INVALID_DATA = 62,
    BLOCKED_CARD = 63,
    CARD_WITHOUT_APPLICATION = 64,
    INVALIDATED_CARD = 67,
    PROBLEMATIC_CARD = 68,
    CARD_WITH_INVALID_DATA_2 = 69,
    CARD_WITHOUT_APPLICATION_2 = 70,
    UNUSED_APPLICATION = 71,
    FALLBACK_ERROR = 76,
    MULTIPLE_CTLSS = 80,
    CTLSS_COMMUNICATION_ERROR = 81,
    INVALIDATED_CTLSS = 82,
    PROBLEMATIC_CTLSS = 83,
    CTLSS_WITHOUT_APPLICATION = 84,
    UNSUPPORTED_CTLSS_APPLICATION = 85,
    EXTERNAL_CTLSS_DEVICE = 86,
    CTLSS_CHANGE_INTERFACE = 87,

    ABORTED_OPERATION = 1000,
    CARD_CHIP = 1001,
    PRODUCT_EMPTY = 1003,
    DIVERT_CARD_NUMBER = 1004,
    REJECTED_TRANSACTION = 1005,
    CARD_REJECTED = 1006,
    INVALID_TRANSACTION = 1007,
    PRINT_ERROR = 1008,
    EMPTY_TRANSACTION = 1009,
    CARD_NUMBER_INVALID = 1010,
    INVALID_EXP_DATE = 1011,
    NONE_PRODUCTS = 1012,
    NO_CREDIT_INSTALLMENT_SELECTED = 1013,
    LOGON_NOT_PERFORMED = 1014,
    TABLE_WRITE_ERROR = 1015,
    DEVICE_NOT_FOUND = 1016,
    DEVICE_NOT_CONNECTED = 1017,
    CONFIG_NOT_CALLED = 1018,
    TRANSACTION_NOT_FOUND = 1019,
    MAX_INSTALLMENT_AMOUNT = 1020,
    MIN_INSTALLMENT_AMOUNT = 1021,
    MAX_INSTALLMENT = 1022,
    MIN_INSTALLMENT = 1023);

  TPagMenuOptions = (
    MENU_OPTIONS_APPLICATIONS,
    MENU_OPTIONS_PRODUCTS ) ;

  TPagRequestOptions = (
    REQUEST_OPTIONS_LAST_DIGITS,
    REQUEST_OPTIONS_CVV_TYPE,
    REQUEST_OPTIONS_CVV,
    REQUEST_OPTIONS_INSTALLMENTS,
    REQUEST_OPTIONS_MONTH,
    REQUEST_OPTIONS_YEAR );

  TPagRequestData = record
    title: PAnsiChar;
    data: PAnsiChar;
    extra: PAnsiChar;
  end;
  PPagRequestData = ^TPagRequestData;

  TPagCvvType = (
    CVV_TYPE_NO_EXISTS = 1,
    CVV_TYPE_UNREADABLE = 2,
    CVV_TYPE_EXISTS = 3,
    CVV_TYPE_NOT_REQUESTED = 4 );

  TPagLastTransactionType = (
    LAST_TRANSACTION_TYPE_TRANSACTION,
    LAST_TRANSACTION_TYPE_CANCELLATION );

  TPagCreditType = (
    CREDIT_TYPE_NO_INSTALLMENT,
    CREDIT_TYPE_INSTALLMENT );

  TPagCardType = (
    CARD_TYPE_EMV,
    CARD_TYPE_CTLS,
    CARD_TYPE_MAGNETIC,
    CARD_TYPE_NONE );

  TPagTRANSACTION_TYPE = (
    TRANSACTION_TYPE_DEBIT,
    TRANSACTION_TYPE_CREDIT,
    TRANSACTION_TYPE_VOUCHER,
    TRANSACTION_TYPE_NONE );

  TPagTransactionParams = record
    amount: Int64;
    creditType: Cardinal;      // TPagCreditType
    cardType: Cardinal;        // TPagCardType
    transactionType: Cardinal; // TPagTRANSACTION_TYPE
    installment: LongInt;
    isTyped: LongInt;
  end;
  PPagTransactionParams = ^TPagTransactionParams;

  TPagReadCardType = (
    READ_CARD_TYPE_MAGNETIC,
    READ_CARD_TYPE_M1,
    READ_CARD_TYPE_M2,
    READ_CARD_TYPE_EMV_CONTACT,
    READ_CARD_TYPE_TIB,
    READ_CARD_TYPE_CONTACTLESS_STRIPE,
    READ_CARD_TYPE_CONTACTLESS_EMV,
    READ_CARD_TYPE_TYPED);
  TPagReadCardTypeSet = set of TPagReadCardType;

  TPagTransactionStatus = (
    TRANSACTION_STATUS_CONFIRMED,
    TRANSACTION_STATUS_UNDONE,
    TRANSACTION_STATUS_PENDING,
    TRANSACTION_STATUS_PENDING_CONFIRMATION,
    TRANSACTION_STATUS_UNDO,
    TRANSACTION_STATUS_PENDING_UNDO,
    TRANSACTION_STATUS_REJECTED,
    TRANSACTION_STATUS_CANCELLED);
  TPagTransactionStatusSet = set of TPagTransactionStatus;

  TPagReasonUndo = (
    REASON_UNDO_TIME_OUT,
    REASON_UNDO_DENIED_BY_CARD,
    REASON_UNDO_REMOVED_CARD );

  TPagTransactionFilter = record
    startDate: Int64;
    endDate: Int64;
    statusSize: LongInt;
    status: array[0..7] of Cardinal;        // TPagTransactionStatus
    readCardTypeSize: LongInt;
    readCardType: array[0..7] of Cardinal;  // TPagReadCardType
  end;

  TPagTransactionPartial = record
    nsuRequest: PAnsiChar;
    amount: Int64;
    typeTransaction: PAnsiChar;
    installments: LongInt;
    transactionStatus: Cardinal;      // TPagTransactionStatus
    date: Int64; //UNIX time
    nsuResponse: PAnsiChar;
    reasonUndo: Cardinal;             // TPagReasonUndo
    transactionReceipt: PAnsiChar;
    brand: PAnsiChar;
    authentication: LongInt;
    entryMode: Cardinal;              // TPagReadCardType
    merchantCode: PAnsiChar;
    nsuAcquirer: PAnsiChar;
    authAcquirer: PAnsiChar;
    printReceipt: Boolean;
    panMasked: PAnsiChar;
  end;
  PPagTransactionPartial = ^TPagTransactionPartial;

  TPagCallBackMessageProcess = procedure(code: Cardinal; process: PAnsiChar); cdecl;
  TPagCallBackMessageError = procedure(code: Cardinal; error: PAnsiChar); cdecl;
  TPagCallBackMessageSuccess = procedure(success: PAnsiChar); cdecl;
  TPagCallBackAbortProcess = function: LongInt; cdecl;
  TPagCallBackMenuProcess = function(option: Cardinal; items: PPAnsiChar; itemSize: LongInt): LongInt; cdecl;
  TPagCallBackRequestProcess = function(options: Cardinal; RequestData: PPagRequestData): LongInt; cdecl;

  TPagCallbackDmSDK = record
    messageProcess: TPagCallBackMessageProcess;
    messageError: TPagCallBackMessageError;
    messageSuccess: TPagCallBackMessageSuccess;
    AbortProcess: TPagCallBackAbortProcess;
    menuProcess: TPagCallBackMenuProcess;
    requestProcess: TPagCallBackRequestProcess;
  end;
  PPagCallbackDmSDK = ^TPagCallbackDmSDK;

  TPagPOSConfig = record
    posHasStripeReader: Boolean;
    posHasPrinter: Boolean;
    posHasDisplay: Boolean;
  end;
  PPagPOSConfig = ^TPagPOSConfig;

  TPagGravarLog = procedure(const ALogLine: String; var Tratado: Boolean) of object ;

  TPagExibeMensagem = procedure( const Mensagem: String) of object;

  TPagEstadoOperacao = (
    tpagEstFluxoAPI,
    tpagEstAguardaUsuario,
    tpagEstPinPad,
    tpagEstPinPadLerCartao,
    tpagEstPinPadDigitacao,
    tpagEstRemoveCartao,
    tpagEstLeituraQRCode );

  TPagTransacaoEmAndamento = procedure(
    EstadoOperacao: TPagEstadoOperacao; out Cancelar: Boolean) of object;

  TPagQuandoPerguntarMenu = procedure(
    const Titulo: String;
    Opcoes: TStringList;
    var ItemSelecionado: LongInt) of object;  // Retorna o Item Selecionado, iniciando com 0
                                              // -2 - Volta no Fluxo
                                              // -1 - Cancela o Fluxo
  { TPagAPI }

  TPagAPI = Class
  private
    fCarregada: Boolean;
    fIdentification: String;
    fInicializada: Boolean;
    fEmTransacao: Boolean;
    fConectada: Boolean;
    fPathLib: String;
    fOnGravarLog: TPagGravarLog;
    fOnExibeMensagem: TPagExibeMensagem;
    fDadosDaTransacao: TStringList;
    fOnTransacaoEmAndamento: TPagTransacaoEmAndamento;

  private
    xTPagConfiguration: function(
      posConfig: PPagPOSConfig;
      callbackDmSdk: PPagCallbackDmSDK): LongInt; cdecl;

    xTPagInitialization: function(identification: PAnsiChar): LongInt; cdecl;

    xTPagTransaction: function(transactionParams: TPagTransactionParams): LongInt; cdecl;

    xTPagCancellation: function(nsuResponse: PAnsiChar; cardType: Cardinal): LongInt; cdecl;

    xTPagUpdateTable: function(): LongInt; cdecl;

    xTPagListTransactionsStore: function(transactionFilter: TPagTransactionFilter;
      var outputCount: LongInt; var errorCode: LongInt ): PPagTransactionPartial; cdecl;

    xTPagLastTransactionStore: function(LastTransactionType: Cardinal; var errorCode: LongInt):
      PPagTransactionPartial; cdecl;

    xTPagFreeTransactionPartialList: procedure(listTransaction: PPagTransactionPartial;
      listSize: LongInt); cdecl;

    xTPagResetTerminal: function(): LongInt; cdecl;

    xTPagLastReceipt: function(isCustomer, isCancellation, isReprint: Boolean;
      var errorCode: LongInt): PAnsiChar; cdecl;

  private
    CallbackDmSDK: TPagCallbackDmSDK;
    fQuandoPerguntarMenu: TPagQuandoPerguntarMenu;

    function GetUltimoErro: String;
    procedure SetIdentification(AValue: String);
    procedure SetInicializada(AValue: Boolean);
    procedure SetPathLib(AValue: String);

  protected
    function GetLibFullPath: String;

    procedure LoadLibFunctions;
    procedure UnLoadLibFunctions;
    procedure ClearMethodPointers;

    procedure DoException(const AErrorMsg: String );
    procedure PrepararInicioDeTrancao;

  public
    POSConfig: TPagPOSConfig;

    constructor Create;
    destructor Destroy; override;

    property PathLib: String read fPathLib write SetPathLib;

    property Identification: String read fIdentification write SetIdentification;

    property Carregada: Boolean read fCarregada;
    property Inicializada: Boolean read fInicializada write SetInicializada;
    property Conectada: Boolean read fConectada;
    property EmTransacao: Boolean read fEmTransacao;

    property OnGravarLog: TPagGravarLog read fOnGravarLog write fOnGravarLog;

    procedure Inicializar;
    procedure DesInicializar;
    procedure Conectar;

    function Transacao(Params: TPagTransactionParams): LongInt;
    procedure AbortarTransacao;
    function Cancelamento(const nsuResponse: String; CardType: TPagCardType): LongInt;
    function AtualizarTabelas: LongInt;
    function ReiniciarTerminal: LongInt;
    function UltimoRecibo(EhConsumidor, EhCancelamento, EhReimpressao: Boolean; var errorCode: LongInt): String;
    function ObterListaTransacoes(Params: TPagTransactionFilter; var num, errorCode: LongInt): PPagTransactionPartial;
    function ObterTransacao(TransactionList: PPagTransactionPartial; index: Integer): TPagTransactionPartial;
    procedure LiberarListaTransacoes(TransactionList: PPagTransactionPartial; num: LongInt);
    procedure ObterUltimaTransacao(LastTransactionType: TPagLastTransactionType; var errorCode: LongInt);

    procedure TransacaoToStr(ATransaction: TPagTransactionPartial; sl: TStringList);

    property DadosDaTransacao: TStringList read fDadosDaTransacao;
    property UltimoErro: String read GetUltimoErro;

    property OnExibeMensagem: TPagExibeMensagem read fOnExibeMensagem
      write fOnExibeMensagem;
    property OnTransacaoEmAndamento: TPagTransacaoEmAndamento read fOnTransacaoEmAndamento
      write fOnTransacaoEmAndamento;
    property QuandoPerguntarMenu: TPagQuandoPerguntarMenu read fQuandoPerguntarMenu
      write fQuandoPerguntarMenu;

    procedure GravarLog(const AString: AnsiString; Traduz: Boolean = False);
    procedure ExibirMensagem(const AMsg: String);
    procedure PerguntarMenu(const Titulo: String; Opcoes: TStringList; var ItemSelecionado: LongInt);

    procedure TratarErroTPag(AErrorCode: LongInt); overload;
    procedure TratarErroTPag(AErrorCode: TPagReturnCodes); overload;
  end;

function GetTEFTPagAPI: TPagAPI;
function ReturnCodesToStr(ReturnCode: TPagReturnCodes): String;

procedure CallBackMessageProcess(code: Cardinal; process: PAnsiChar); cdecl;
procedure CallBackMessageError(code: Cardinal; error: PAnsiChar); cdecl;
procedure CallBackMessageSuccess(success: PAnsiChar); cdecl;
function CallBackAbortProcess: LongInt; cdecl;
function CallBackMenuProcess(option: Cardinal; items: PPAnsiChar; itemSize: LongInt): LongInt; cdecl;
function CallBackRequestProcess(options: Cardinal; RequestData: PPagRequestData): LongInt; cdecl;


var
 vTEFTPagAPI : TPagAPI;

implementation

uses
  TypInfo, StrUtils, Math,
  ACBrUtil.FilesIO,
  ACBrUtil.Strings,
  ACBrUtil.DateTime;

function GetTEFTPagAPI: TPagAPI;
begin
  if not Assigned(vTEFTPagAPI) then
    vTEFTPagAPI := TPagAPI.Create;

  Result := vTEFTPagAPI;
end;

function ReturnCodesToStr(ReturnCode: TPagReturnCodes): String;
begin
  case ReturnCode of
    OK                             : Result := 'OK';
    INTERNAL_ERROR                 : Result := 'INTERNAL_ERROR';
    INVALID_CALL                   : Result := 'INVALID_CALL';
    INVALID_PARAMETER              : Result := 'INVALID_PARAMETER';
    TIMEOUT                        : Result := 'TIMEOUT';
    CANCELED_OPERATION             : Result := 'CANCELED_OPERATION';
    BUSY_PINPAD                    : Result := 'BUSY_PINPAD';
    INVALID_MODEL                  : Result := 'INVALID_MODEL';
    EXPIRED_TABLES                 : Result := 'EXPIRED_TABLES';
    MAG_CARD_READ_ERROR            : Result := 'MAG_CARD_READ_ERROR';
    MISSING_PIN_KEY                : Result := 'MISSING_PIN_KEY';
    MISSING_CARD                   : Result := 'MISSING_CARD';
    SAM_MODULE_ERROR               : Result := 'SAM_MODULE_ERROR';
    INVALID_SAM                    : Result := 'INVALID_SAM';
    MISSING_SAM                    : Result := 'MISSING_SAM';
    MUTE_CARD                      : Result := 'MUTE_CARD';
    CARD_COMMUNICATION_ERROR       : Result := 'CARD_COMMUNICATION_ERROR';
    CARD_WITH_INVALID_DATA         : Result := 'CARD_WITH_INVALID_DATA';
    BLOCKED_CARD                   : Result := 'BLOCKED_CARD';
    CARD_WITHOUT_APPLICATION       : Result := 'CARD_WITHOUT_APPLICATION';
    INVALIDATED_CARD               : Result := 'INVALIDATED_CARD';
    PROBLEMATIC_CARD               : Result := 'PROBLEMATIC_CARD';
    CARD_WITH_INVALID_DATA_2       : Result := 'CARD_WITH_INVALID_DATA_2';
    CARD_WITHOUT_APPLICATION_2     : Result := 'CARD_WITHOUT_APPLICATION_2';
    UNUSED_APPLICATION             : Result := 'UNUSED_APPLICATION';
    FALLBACK_ERROR                 : Result := 'FALLBACK_ERROR';
    MULTIPLE_CTLSS                 : Result := 'MULTIPLE_CTLSS';
    CTLSS_COMMUNICATION_ERROR      : Result := 'CTLSS_COMMUNICATION_ERROR';
    INVALIDATED_CTLSS              : Result := 'INVALIDATED_CTLSS';
    PROBLEMATIC_CTLSS              : Result := 'PROBLEMATIC_CTLSS';
    CTLSS_WITHOUT_APPLICATION      : Result := 'CTLSS_WITHOUT_APPLICATION';
    UNSUPPORTED_CTLSS_APPLICATION  : Result := 'UNSUPPORTED_CTLSS_APPLICATION';
    EXTERNAL_CTLSS_DEVICE          : Result := 'EXTERNAL_CTLSS_DEVICE';
    CTLSS_CHANGE_INTERFACE         : Result := 'CTLSS_CHANGE_INTERFACE';
    ABORTED_OPERATION              : Result := 'ABORTED_OPERATION';
    CARD_CHIP                      : Result := 'CARD_CHIP';
    PRODUCT_EMPTY                  : Result := 'PRODUCT_EMPTY';
    DIVERT_CARD_NUMBER             : Result := 'DIVERT_CARD_NUMBER';
    REJECTED_TRANSACTION           : Result := 'REJECTED_TRANSACTION';
    CARD_REJECTED                  : Result := 'CARD_REJECTED';
    INVALID_TRANSACTION            : Result := 'INVALID_TRANSACTION';
    PRINT_ERROR                    : Result := 'PRINT_ERROR';
    EMPTY_TRANSACTION              : Result := 'EMPTY_TRANSACTION';
    CARD_NUMBER_INVALID            : Result := 'CARD_NUMBER_INVALID';
    INVALID_EXP_DATE               : Result := 'INVALID_EXP_DATE';
    NONE_PRODUCTS                  : Result := 'NONE_PRODUCTS';
    NO_CREDIT_INSTALLMENT_SELECTED : Result := 'NO_CREDIT_INSTALLMENT_SELECTED';
    LOGON_NOT_PERFORMED            : Result := 'LOGON_NOT_PERFORMED';
    TABLE_WRITE_ERROR              : Result := 'TABLE_WRITE_ERROR';
    DEVICE_NOT_FOUND               : Result := 'DEVICE_NOT_FOUND';
    DEVICE_NOT_CONNECTED           : Result := 'DEVICE_NOT_CONNECTED';
    CONFIG_NOT_CALLED              : Result := 'CONFIG_NOT_CALLED';
    TRANSACTION_NOT_FOUND          : Result := 'TRANSACTION_NOT_FOUND';
    MAX_INSTALLMENT_AMOUNT         : Result := 'MAX_INSTALLMENT_AMOUNT';
    MIN_INSTALLMENT_AMOUNT         : Result := 'MIN_INSTALLMENT_AMOUNT';
    MAX_INSTALLMENT                : Result := 'MAX_INSTALLMENT';
    MIN_INSTALLMENT                : Result := 'MIN_INSTALLMENT';
  else
    Result := 'ReturnCode: '+IntToStr(Integer(ReturnCode));
  end;
end;

procedure CallBackMessageProcess(code: Cardinal; process: PAnsiChar); cdecl;
var
  s: String;
begin
  if (process = nil) then
    s := ''
  else
    s := UTF8ToNativeString(TrimRight(String(process)));

  with GetTEFTPagAPI do
  begin
    GravarLog( Format('   Callback Process: %d - %s ', [code, s] ));
    ExibirMensagem(s);
  end;
end;

procedure CallBackMessageError(code: Cardinal; error: PAnsiChar); cdecl;
var
  s: String;
begin
  if (error = nil) then
    s := ''
  else
    s := UTF8ToNativeString(TrimRight(String(error)));

  with GetTEFTPagAPI do
  begin
    GravarLog( Format('   Callback Error: %d - %s ', [code, s] ));
    ExibirMensagem(s);
    DadosDaTransacao.Values['msgError'] := s;
  end;
end;

procedure CallBackMessageSuccess(success: PAnsiChar); cdecl;
var
  s: String;
begin
  if (success = nil) then
    s := ''
  else
    s := UTF8ToNativeString(TrimRight(String(success)));

  with GetTEFTPagAPI do
  begin
    GravarLog(  Format('   Callback Sucess: %s ', [s] ));
    ExibirMensagem(s);
    DadosDaTransacao.Values['msgSuccess'] := s;
  end;
end;

function CallBackAbortProcess: LongInt; cdecl;
var
  estado: TPagEstadoOperacao;
  Cancelar: Boolean;
begin
  Result:= 0;  // Continuar..

  with GetTEFTPagAPI do
  begin
    if Assigned(OnTransacaoEmAndamento) then
    begin
      estado := tpagEstPinPad;
      Cancelar := False;
      GravarLog('  OnTransacaoEmAndamento( '+GetEnumName(TypeInfo(TPagEstadoOperacao), integer(estado))+' )');
      OnTransacaoEmAndamento(estado, Cancelar);
      GravarLog('    Cancelar: '+BoolToStr(Cancelar, True) );
      if Cancelar then
        Result := 1;  // Abortar
    end;
  end;
end;

function CallBackMenuProcess(option: Cardinal; items: PPAnsiChar;
  itemSize: LongInt): LongInt; cdecl;
var
  i: Integer;
  sl: TStringList;
  title: String;
  iOpcaoSelecionada: LongInt;
  item: PPAnsiChar;
begin
  iOpcaoSelecionada := -1;

  with GetTEFTPagAPI do
  begin
    title := GetEnumName(TypeInfo(TPagMenuOptions), option);
    GravarLog('  CallBackMenuProcess - '+
              'option: '+title+', '+IntToStr(itemSize)+' items: '+String(items^));

    sl := TStringList.Create;
    try
      item := items;
      i := 1;
      while (item^ <> nil) do
      begin
        sl.Add(Format('%d - %s', [i, String(item^)]));
        inc(i);
        Inc(item);      // avança para o próximo ponteiro
      end;

      PerguntarMenu(title, sl, iOpcaoSelecionada);
    finally
      sl.Free;
    end;
  end;

  Result:= iOpcaoSelecionada;
end;

function CallBackRequestProcess(options: Cardinal; RequestData: PPagRequestData
  ): LongInt; cdecl;
begin
  Result:= 0;  // Continuar..

  with GetTEFTPagAPI do
  begin
    GravarLog('  CallBackRequestProcess');
    GravarLog('  '+Format('options: %d, title: %s data: %s extra: %s',
                          [options, RequestData.title, RequestData.data, RequestData.extra]));
  end;
end;

{ TPagAPI }

constructor TPagAPI.Create;
begin
  inherited;

  fOnGravarLog := Nil;
  fOnExibeMensagem := Nil;
  fOnTransacaoEmAndamento := Nil;
  fQuandoPerguntarMenu := Nil;

  fCarregada := False;
  fInicializada := False;
  fCarregada := False;
  fEmTransacao := False;
  fPathLib := '';
  fIdentification := '';
  fDadosDaTransacao := TStringList.Create;

  POSConfig.posHasDisplay := True;
  POSConfig.posHasPrinter := False;
  POSConfig.posHasStripeReader := True;

  CallbackDmSDK.messageProcess := CallBackMessageProcess;
  CallbackDmSDK.messageError := CallBackMessageError;
  CallbackDmSDK.messageSuccess := CallBackMessageSuccess;
  CallbackDmSDK.AbortProcess := CallBackAbortProcess;
  CallbackDmSDK.menuProcess := CallBackMenuProcess;
  CallbackDmSDK.requestProcess := CallBackRequestProcess;
end;

destructor TPagAPI.Destroy;
begin
  fDadosDaTransacao.Free;
  fOnGravarLog := Nil;
  fOnExibeMensagem := Nil;
  fOnTransacaoEmAndamento := Nil;
  fQuandoPerguntarMenu := Nil;
  inherited Destroy;
end;

procedure TPagAPI.Inicializar;
var
  ret: LongInt;

begin
  if fInicializada then
    Exit;

  fEmTransacao := False;
  fDadosDaTransacao.Clear;
  GravarLog('TPagAPI.Inicializar');

  if (fIdentification = '') then
    DoException(ACBrStr(sErrCNPJNaoInformado));

  if not Assigned(fOnTransacaoEmAndamento) then
    DoException(Format(ACBrStr(sErrEventoNaoAtribuido), ['OnTransacaoEmAndamento']));
  if not Assigned(fOnExibeMensagem) then
    DoException(Format(ACBrStr(sErrEventoNaoAtribuido), ['OnExibeMensagem']));
  if not Assigned(fQuandoPerguntarMenu) then
    DoException(Format(ACBrStr(sErrEventoNaoAtribuido), ['QuandoPerguntarMenu']));

  LoadLibFunctions;
  GravarLog('  call - Configuration');
  ret := xTPagConfiguration( @POSConfig, @CallbackDmSDK );
  GravarLog('   ret - '+IntToStr(ret));
  TratarErroTPag(ret);

  fInicializada := True;
end;

procedure TPagAPI.DesInicializar;
begin
  if not fInicializada then
    Exit;

  GravarLog('TPagAPI.DesInicializar');
  UnLoadLibFunctions;
  fInicializada := False;
  fConectada := False;
end;

procedure TPagAPI.Conectar;
var
  ret: LongInt;
begin
  if fConectada then
    Exit;

  fDadosDaTransacao.Clear;
  GravarLog('  call - Initialization('+fIdentification+')');
  ret := xTPagInitialization(PAnsiChar(AnsiString(fIdentification)));
  GravarLog('   ret - '+IntToStr(ret));
  TratarErroTPag(ret);
  fConectada := True;
end;

function TPagAPI.Transacao(Params: TPagTransactionParams): LongInt;
begin
  PrepararInicioDeTrancao;
  GravarLog('  call - Transaction' + sLineBreak +
            '    Params.amount: '+IntToStr(Params.amount)+ sLineBreak +
            '    Params.creditType: '+ IntToStr(Params.creditType) + sLineBreak +
            '    Params.cardType: '+IntToStr(Params.cardType) + sLineBreak +
            '    Params.transactionType: '+ IntToStr(Params.transactionType) + sLineBreak +
            '    Params.installment: '+IntToStr(Params.installment)+ sLineBreak +
            '    Params.isTyped: '+IntToStr(Params.isTyped) );

  fEmTransacao := True;
  try
    Result := xTPagTransaction(Params);
    GravarLog('   ret - '+IntToStr(Result));
    fDadosDaTransacao.Values['ret'] := IntToStr(Result);
  finally
    fEmTransacao := False;
  end;
end;

procedure TPagAPI.AbortarTransacao;
begin
  fEmTransacao := False;
end;

function TPagAPI.Cancelamento(const nsuResponse: String; CardType: TPagCardType
  ): LongInt;
var
  s: AnsiString;
  c: Cardinal;
begin
  PrepararInicioDeTrancao;
  GravarLog('  call - Cancellation( '+nsuResponse+', '+GetEnumName(TypeInfo(TPagCardType), integer(CardType))+' )' );

  s := Trim(nsuResponse);
  if (s = '') then
    DoException(ACBrStr(sErrNSUNaoInformado));

  fEmTransacao := True;
  try
    c := Cardinal(CardType);
    Result := xTPagCancellation(PAnsiChar(s), c);
    GravarLog('   ret - '+IntToStr(Result));
    fDadosDaTransacao.Values['ret'] := IntToStr(Result);
  finally
    fEmTransacao := False;
  end;
end;

function TPagAPI.AtualizarTabelas: LongInt;
begin
  PrepararInicioDeTrancao;
  GravarLog('  call - UpdateTable');
  Result := xTPagUpdateTable;
  GravarLog('   ret - '+IntToStr(Result));
end;

function TPagAPI.ReiniciarTerminal: LongInt;
begin
  PrepararInicioDeTrancao;
  GravarLog('  call - ResetTerminal');
  Result := xTPagResetTerminal;
  GravarLog('   ret - '+IntToStr(Result));
  if (Result = Integer(TPagReturnCodes(OK))) then
    fConectada := False;
end;

function TPagAPI.UltimoRecibo(EhConsumidor, EhCancelamento,
  EhReimpressao: Boolean; var errorCode: LongInt): String;
var
  p: PAnsiChar;
begin
  PrepararInicioDeTrancao;
  GravarLog('  call - LastReceipt( '+
    BoolToStr(EhConsumidor, True) + ', '+
    BoolToStr(EhCancelamento, True) + ', '+
    BoolToStr(EhReimpressao, True) + ')' );

  errorCode := -1;
  p := xTPagLastReceipt(EhConsumidor, EhCancelamento, EhReimpressao, errorCode);
  GravarLog('   ret - '+IntToStr(errorCode));
  if (errorCode = Integer(TPagReturnCodes(OK))) then
    Result := String(p)
  else
    Result := '';

  GravarLog('   LastReceipt:' + sLineBreak + Result);
end;

function TPagAPI.ObterListaTransacoes(
  Params: TPagTransactionFilter; var num, errorCode: LongInt
  ): PPagTransactionPartial;
begin
  PrepararInicioDeTrancao;
  errorCode := -1;
  num := 0;
  GravarLog('  call - ListTransactionsStore');
  Result := xTPagListTransactionsStore(Params, num, errorCode);
  GravarLog('   ret - '+IntToStr(errorCode)+', num - '+IntToStr(num));
end;

function TPagAPI.ObterTransacao(
  TransactionList: PPagTransactionPartial; index: Integer): TPagTransactionPartial;
var
  p: PPagTransactionPartial;
begin
  p := TransactionList;
  inc(p, index);
  move(p^, Result, SizeOf(TPagTransactionPartial));
end;

procedure TPagAPI.LiberarListaTransacoes(
  TransactionList: PPagTransactionPartial; num: LongInt);
begin
  GravarLog('  call - FreeTransactionPartialList( '+IntToStr(num)+' )');
  xTPagFreeTransactionPartialList(TransactionList, num);
end;

procedure TPagAPI.ObterUltimaTransacao(
  LastTransactionType: TPagLastTransactionType; var errorCode: LongInt);
var
  p: PPagTransactionPartial;
  c: Cardinal;
begin
  PrepararInicioDeTrancao;
  errorCode := -1;
  GravarLog('  call - LastTransactionStore( '+ GetEnumName(TypeInfo(TPagLastTransactionType), integer(LastTransactionType))+' )');
  c := Cardinal(LastTransactionType);
  p := xTPagLastTransactionStore(c, errorCode);
  GravarLog('   ret - '+IntToStr(errorCode));
  if (errorCode = Integer(TPagReturnCodes(OK))) and Assigned(p) then
  begin
    TransacaoToStr(p^, fDadosDaTransacao);
    LiberarListaTransacoes(p, -1);
  end;
end;

procedure TPagAPI.TransacaoToStr(
  ATransaction: TPagTransactionPartial; sl: TStringList);
var
  d: TDateTime;
  s: String;
begin
  sl.Clear;
  sl.Values['nsuRequest'] := Trim(String(ATransaction.nsuRequest));
  sl.Values['amount'] := IntToStr(ATransaction.amount);
  sl.Values['typeTransaction'] := Trim(String(ATransaction.typeTransaction));
  sl.Values['installments'] := IntToStr(ATransaction.installments);
  sl.Values['transactionStatus'] := IntToStr(ATransaction.transactionStatus);
  d := UnixMillisecondsToDateTime(ATransaction.date, False);
  s := FormatDateTime('YYYYMMDDHHNNSS', d);
  sl.Values['date'] := s;
  sl.Values['nsuResponse'] := Trim(String(ATransaction.nsuResponse));
  sl.Values['reasonUndo'] := IntToStr(ATransaction.reasonUndo);
  sl.Values['transactionReceipt'] := TrimRight(String(ATransaction.transactionReceipt));
  sl.Values['brand'] := TrimRight(String(ATransaction.brand));
  sl.Values['authentication'] := IntToStr(ATransaction.authentication);
  sl.Values['entryMode'] := IntToStr(ATransaction.entryMode);
  sl.Values['merchantCode'] := Trim(String(ATransaction.merchantCode));
  sl.Values['nsuAcquirer'] := Trim(String(ATransaction.nsuAcquirer));
  sl.Values['authAcquirer'] := Trim(String(ATransaction.authAcquirer));
  sl.Values['printReceipt'] := IfThen(ATransaction.printReceipt,'1','0');
  sl.Values['panMasked'] := Trim(String(ATransaction.panMasked));
end;

procedure TPagAPI.GravarLog(const AString: AnsiString; Traduz: Boolean);
Var
  Tratado: Boolean;
  AStringLog: AnsiString;
begin
  if not Assigned(fOnGravarLog) then
    Exit;

  if Traduz then
    AStringLog := TranslateUnprintable(AString)
  else
    AStringLog := AString;

  Tratado := False;
  fOnGravarLog(AStringLog, Tratado);
end;

procedure TPagAPI.ExibirMensagem(const AMsg: String);
begin
  if Assigned(fOnExibeMensagem) then
    fOnExibeMensagem(AMsg);
end;

procedure TPagAPI.PerguntarMenu(const Titulo: String; Opcoes: TStringList;
  var ItemSelecionado: LongInt);
begin
  GravarLog('TPagAPI.PerguntarMenu( '+Titulo+' )');
  if Assigned(fQuandoPerguntarMenu) then
    fQuandoPerguntarMenu(Titulo, Opcoes, ItemSelecionado);
end;

procedure TPagAPI.SetInicializada(AValue: Boolean);
begin
  if fInicializada = AValue then
    Exit;

  GravarLog('TPagAPI.SetInicializada( '+BoolToStr(AValue, True)+' )');

  if AValue then
    Inicializar
  else
    DesInicializar;
end;

procedure TPagAPI.SetIdentification(AValue: String);
begin
  fIdentification := OnlyNumber(AValue);
end;

function TPagAPI.GetUltimoErro: String;
begin
  Result := fDadosDaTransacao.Values['msgError'];
end;

procedure TPagAPI.SetPathLib(AValue: String);
begin
  if fPathLib = AValue then
    Exit;

  GravarLog('TPagAPI.SetPathLib( '+AValue+' )');

  if fInicializada then
    DoException(ACBrStr(sErrLibJaInicializada));

  fPathLib := PathWithDelim(ExtractFilePath(AValue));
end;

function TPagAPI.GetLibFullPath: String;
begin
  if (PathLib <> '') then
  begin
    GravarLog(ACBrStr('TPagAPI.LibFullName: Usando "PathLib" informado pela aplicação: ')+PathLib);
    Result := PathLib + CTPagLib;
  end
  else
    Result := ApplicationPath + CTPagLib;
end;

procedure TPagAPI.LoadLibFunctions;

  procedure TPagFunctionDetect(LibName, FuncName: AnsiString; var LibPointer: Pointer;
    FuncIsRequired: Boolean = True) ;
  begin
    if not Assigned( LibPointer )  then
    begin
      GravarLog('   '+FuncName);
      if not FunctionDetect(LibName, FuncName, LibPointer) then
      begin
        LibPointer := NIL ;
        if FuncIsRequired then
          DoException(Format(ACBrStr('Erro ao carregar a função: %s de: %s'),[FuncName, LibName]))
        else
          GravarLog(Format(ACBrStr('     Função não requerida: %s não encontrada em: %s'),[FuncName, LibName]));
        end ;
    end ;
  end;

var
  sLibName: string;
begin
  if fCarregada then
    Exit;

  sLibName := GetLibFullPath;
  GravarLog('TPagAPI.LoadDLLFunctions - '+sLibName);

  TPagFunctionDetect(sLibName, 'configuration', @xTPagConfiguration);
  TPagFunctionDetect(sLibName, 'initialization', @xTPagInitialization);
  TPagFunctionDetect(sLibName, 'transaction', @xTPagTransaction);
  TPagFunctionDetect(sLibName, 'cancellation', @xTPagCancellation);
  TPagFunctionDetect(sLibName, 'updateTable', @xTPagUpdateTable);
  TPagFunctionDetect(sLibName, 'listTransactionsStore', @xTPagListTransactionsStore);
  TPagFunctionDetect(sLibName, 'lastTransactionStore', @xTPagLastTransactionStore);
  TPagFunctionDetect(sLibName, 'freeTransactionPartialList', @xTPagFreeTransactionPartialList);
  TPagFunctionDetect(sLibName, 'resetTerminal', @xTPagResetTerminal);
  TPagFunctionDetect(sLibName, 'lastReceipt', @xTPagLastReceipt);

  fCarregada := True;
end;

procedure TPagAPI.UnLoadLibFunctions;
var
  sLibName: String;
begin
  if not fCarregada then
    Exit;

  GravarLog('TPagAPI.UnLoadDLLFunctions');

  sLibName := GetLibFullPath;
  UnLoadLibrary( sLibName );
  fCarregada := False;
  ClearMethodPointers;
end;

procedure TPagAPI.ClearMethodPointers;
begin
  xTPagConfiguration := Nil;
  xTPagInitialization := Nil;
  xTPagTransaction := Nil;
  xTPagCancellation := Nil;
  xTPagUpdateTable := Nil;
  xTPagListTransactionsStore := Nil;
  xTPagLastTransactionStore := Nil;
  xTPagFreeTransactionPartialList := Nil;
  xTPagResetTerminal := Nil;
  xTPagLastReceipt := Nil;
end;

procedure TPagAPI.DoException(const AErrorMsg: String);
begin
  if (Trim(AErrorMsg) = '') then
    Exit;

  GravarLog('TPagAPI: '+AErrorMsg);
  raise EACBrTEFTPagAPI.Create(AErrorMsg);
end;

procedure TPagAPI.PrepararInicioDeTrancao;
begin
  Conectar;
  fDadosDaTransacao.Clear;
end;

procedure TPagAPI.TratarErroTPag(AErrorCode: LongInt);
begin
  TratarErroTPag( TPagReturnCodes(AErrorCode) );
end;

procedure TPagAPI.TratarErroTPag(AErrorCode: TPagReturnCodes);
var
  msgErro: String;
begin
  if (AErrorCode = TPagReturnCodes(OK)) then
    Exit;

  msgErro := Trim(UltimoErro);
  if (msgErro <> '') then
    msgErro := msgErro + sLineBreak;
  msgErro := msgErro + ReturnCodesToStr(AErrorCode);

  DoException(msgErro);
end;

initialization
  vTEFTPagAPI := nil;

finalization
  if Assigned(vTEFTPagAPI) then
    FreeAndNil(vTEFTPagAPI);

end.

