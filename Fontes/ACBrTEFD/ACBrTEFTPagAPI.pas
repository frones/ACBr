{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
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

const
  CreditType_NO_INSTALLMENT = 0;
  CreditType_INSTALLMENT = 1;

  CardType_EMV = 0;
  CardType_CTLS = 1;
  CardType_NONE = 2;

  TransactionType_DEBIT = 0;
  TransactionType_CREDIT = 1;
  TransactionType_VOUCHER = 2;

  LastTransactionType_TRANSACTION = 0;
  LastTransactionType_CANCELLATION = 1;

  ReasonUndo_TIME_OUT = 0;
  ReasonUndo_DENIED_BY_CARD = 1;
  ReasonUndo_REMOVED_CARD = 2;

  Notification_FREE_DSP = 0;
  Notification_DSP_2X16 = 1;
  Notification_PROCESSING_DSP = 2;
  Notification_INSERT_PASS_CARD = 3;
  Notification_APPROACH_INSERT_PASS_CARD = 4;
  Notification_PASS_CARD = 5;
  Notification_SELECT = 6;
  Notification_SELECTED = 7;
  Notification_INVALID_APP = 8;
  Notification_INVALID_PASSWORD = 9;
  Notification_LAST_PASSWORD_ATTEMPT = 10;
  Notification_BLOCKED_PASSWORD = 11;
  Notification_PASSWORD_VERIFIED = 12;
  Notification_BLOCKED_CARD_DSP = 13;
  Notification_REMOVE_CARD = 14;
  Notification_UPDATING_TABLES = 15;
  Notification_REPRESENT_CARD = 16;
  Notification_START_PIN = 17;
  Notification_END_PIN = 18;
  Notification_ERROR_MESSAGE = 19;
  Notification_PIN_INSERT = 20;
  Notification_SUCCESS_MESSAGE = 21;

  ReturnCode_OK = 0;
  ReturnCode_INTERNAL_ERROR = 1;
  ReturnCode_INVALID_CALL = 10;
  ReturnCode_INVALID_PARAMETER = 11;
  ReturnCode_TIMEOUT = 12;
  ReturnCode_CANCELED_OPERATION = 13;
  ReturnCode_BUSY_PINPAD = 16;
  ReturnCode_INVALID_MODEL = 17;
  ReturnCode_EXPIRED_TABLES = 20;
  ReturnCode_MAG_CARD_READ_ERROR = 41;
  ReturnCode_MISSING_PIN_KEY = 42;
  ReturnCode_MISSING_CARD = 43;
  ReturnCode_SAM_MODULE_ERROR = 50;
  ReturnCode_INVALID_SAM = 51;
  ReturnCode_MISSING_SAM = 52;
  ReturnCode_MUTE_CARD = 60;
  ReturnCode_CARD_COMMUNICATION_ERROR = 61;
  ReturnCode_CARD_WITH_INVALID_DATA = 62;
  ReturnCode_BLOCKED_CARD = 63;
  ReturnCode_CARD_WITHOUT_APPLICATION = 64;
  ReturnCode_INVALIDATED_CARD = 67;
  ReturnCode_PROBLEMATIC_CARD = 68;
  ReturnCode_CARD_WITH_INVALID_DATA_2 = 69;
  ReturnCode_CARD_WITHOUT_APPLICATION_2 = 70;
  ReturnCode_UNUSED_APPLICATION = 71;
  ReturnCode_FALLBACK_ERROR = 76;
  ReturnCode_MULTIPLE_CTLSS = 80;
  ReturnCode_CTLSS_COMMUNICATION_ERROR = 81;
  ReturnCode_INVALIDATED_CTLSS = 82;
  ReturnCode_PROBLEMATIC_CTLSS = 83;
  ReturnCode_CTLSS_WITHOUT_APPLICATION = 84;
  ReturnCode_UNSUPPORTED_CTLSS_APPLICATION = 85;
  ReturnCode_EXTERNAL_CTLSS_DEVICE = 86;
  ReturnCode_CTLSS_CHANGE_INTERFACE = 87;
  ReturnCode_ABORTED_OPERATION = 1000;
  ReturnCode_CARD_CHIP = 1001;
  ReturnCode_PRODUCT_EMPTY = 1003;
  ReturnCode_DIVERT_CARD_NUMBER = 1004;
  ReturnCode_REJECTED_TRANSACTION = 1005;
  ReturnCode_CARD_REJECTED = 1006;
  ReturnCode_INVALID_TRANSACTION = 1007;
  ReturnCode_PRINT_ERROR = 1008;
  ReturnCode_EMPTY_TRANSACTION = 1009;
  ReturnCode_CARD_NUMBER_INVALID = 1010;
  ReturnCode_INVALID_EXP_DATE = 1011;
  ReturnCode_NONE_PRODUCTS = 1012;
  ReturnCode_NO_CREDIT_INSTALLMENT_SELECTED = 1013;
  ReturnCode_LOGON_NOT_PERFORMED = 1014;
  ReturnCode_TABLE_WRITE_ERROR = 1015;
  ReturnCode_DEVICE_NOT_FOUND = 1016;
  ReturnCode_DEVICE_NOT_CONNECTED = 1017;
  ReturnCode_CONFIG_NOT_CALLED = 1018;
  ReturnCode_TRANSACTION_NOT_FOUND = 1019;

type
  EACBrTEFTPagAPI = class(Exception);

  TACBrTEFTPagReadCardType = ( rct_MAGNETIC,
                               rct_M1,
                               rct_M2,
                               rct_EMV_CONTACT,
                               rct_TIB,
                               rct_CONTACTLESS_STRIPE,
                               rct_CONTACTLESS_EMV,
                               rct_TYPED);
  TACBrTEFTPagReadCardTypeSet = set of TACBrTEFTPagReadCardType;

  TACBrTEFTPagTransactionStatus = ( ts_CONFIRMED,
                                    ts_UNDONE,
                                    ts_PENDING,
                                    ts_PENDING_CONFIRMATION,
                                    ts_UNDO,
                                    ts_PENDING_UNDO,
                                    ts_REJECTED,
                                    ts_CANCELLED);
  TACBrTEFTPagTransactionStatusSet = set of TACBrTEFTPagTransactionStatus;

  TACBrTEFTPagTransactionParams = record
    amount: Int64;
    creditType: Cardinal;
    cardType: Cardinal;
    transactionType: Cardinal;
    installment: LongInt;
    isTyped: LongInt;
  end;

  TACBrTEFTPagTransactionFilter = record
    startDate: Int64;
    endDate: Int64;
    statusSize: LongInt;
    status: array[0..7] of Cardinal;
    readCardTypeSize: LongInt;
    readCardType: array[0..7] of Cardinal;
  end;

  TACBrTEFTPagTransactionPartial = record
    nsuRequest: PAnsiChar;
    amount: Int64;
    typeTransaction: PAnsiChar;
    installments: LongInt;
    transactionStatus: Cardinal;
    date: Int64; //UNIX time
    nsuResponse: PAnsiChar;
    reasonUndo: Cardinal;
    transactionReceipt: PAnsiChar;
    brand: PAnsiChar;
    authentication: LongInt;
    entryMode: Cardinal;
    merchantCode: PAnsiChar;
    nsuAcquirer: PAnsiChar;
    authAcquirer: PAnsiChar;
    printReceipt: Boolean;
    panMasked: PAnsiChar;
  end;
  PACBrTEFTPagTransactionPartial = ^TACBrTEFTPagTransactionPartial;

  TACBrTEFTPagGravarLog = procedure(const ALogLine: String; var Tratado: Boolean) of object ;

  TACBrTEFTPagExibeMensagem = procedure( const Mensagem: String) of object;

  TACBrTEFTPagEstadoOperacao = ( tpagEstFluxoAPI,
                                 tpagEstAguardaUsuario,
                                 tpagEstPinPad,
                                 tpagEstPinPadLerCartao,
                                 tpagEstPinPadDigitacao,
                                 tpagEstRemoveCartao,
                                 tpagEstLeituraQRCode );

  TACBrTEFTPagTransacaoEmAndamento = procedure(
    EstadoOperacao: TACBrTEFTPagEstadoOperacao; out Cancelar: Boolean) of object;

  TACBrTPagCallBackProcess = procedure(code: Cardinal; process: PAnsiChar); cdecl;
  TACBrTPagCallBackError = procedure(code: Cardinal; error: PAnsiChar); cdecl;
  TACBrTPagCallBackSuccess = procedure(success: PAnsiChar); cdecl;
  TACBrTPagCallAbortProcess = function: LongInt; cdecl;

  { TACBrTEFTPagAPI }

  TACBrTEFTPagAPI = Class
  private
    fCarregada: Boolean;
    fCNPJEmpresa: String;
    fInicializada: Boolean;
    fEmTransacao: Boolean;
    fConectada: Boolean;
    fPathLib: String;
    fOnGravarLog: TACBrTEFTPagGravarLog;
    fOnExibeMensagem: TACBrTEFTPagExibeMensagem;
    fDadosDaTransacao: TStringList;
    fOnTransacaoEmAndamento: TACBrTEFTPagTransacaoEmAndamento;

  private
    xTPagConfiguration: function( messageProcess: TACBrTPagCallBackProcess;
                                  messageError: TACBrTPagCallBackError;
                                  messageSuccess: TACBrTPagCallBackSuccess;
                                  abortProcess: TACBrTPagCallAbortProcess): LongInt; cdecl;

    xTPagInitialization: function(identification: PAnsiChar): LongInt; cdecl;

    xTPagTransaction: function(transactionParams: TACBrTEFTPagTransactionParams): LongInt; cdecl;

    xTPagCancellation: function(nsuResponse: PAnsiChar; cardType: Cardinal): LongInt; cdecl;

    xTPagUpdateTable: function(): LongInt; cdecl;

    xTPagListTransactionsStore: function(transactionFilter: TACBrTEFTPagTransactionFilter;
      var outputCount: LongInt; var errorCode: LongInt ): PACBrTEFTPagTransactionPartial; cdecl;

    xTPagLastTransactionStore: function(LastTransactionType: Cardinal; var errorCode: LongInt):
      PACBrTEFTPagTransactionPartial; cdecl;

    xTPagFreeTransactionPartialList: procedure(listTransaction: PACBrTEFTPagTransactionPartial;
      listSize: LongInt); cdecl;

    xTPagResetTerminal: function(): LongInt; cdecl;

    xTPagLastReceipt: function(isCustomer, isCancellation, isReprint: Boolean;
      var errorCode: LongInt): PAnsiChar; cdecl;
    procedure SetCNPJEmpresa(AValue: String);
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
    constructor Create;
    destructor Destroy; override;

    property PathLib: String read fPathLib write SetPathLib;

    property CNPJEmpresa: String read fCNPJEmpresa write SetCNPJEmpresa;

    property Carregada: Boolean read fCarregada;
    property Inicializada: Boolean read fInicializada write SetInicializada;
    property Conectada: Boolean read fConectada;
    property EmTransacao: Boolean read fEmTransacao;

    property OnGravarLog: TACBrTEFTPagGravarLog read fOnGravarLog write fOnGravarLog;

    procedure Inicializar;
    procedure DesInicializar;
    procedure Conectar;

    function Transacao(Params: TACBrTEFTPagTransactionParams): LongInt;
    procedure AbortarTransacao;
    function Cancelamento(const nsuResponse: String; CardType: Cardinal): LongInt;
    function AtualizarTabelas: LongInt;
    function ReiniciarTerminal: LongInt;
    function UltimoRecibo(EhConsumidor, EhCancelamento, EhReimpressao: Boolean; var errorCode: LongInt): String;
    function ObterListaTransacoes(Params: TACBrTEFTPagTransactionFilter; var num, errorCode: LongInt): PACBrTEFTPagTransactionPartial;
    function ObterTransacao(TransactionList: PACBrTEFTPagTransactionPartial; index: Integer): TACBrTEFTPagTransactionPartial;
    procedure LiberarListaTransacoes(TransactionList: PACBrTEFTPagTransactionPartial; num: LongInt);
    procedure ObterUltimaTransacao(LastTransactionType: Cardinal; var errorCode: LongInt);

    procedure TransacaoToStr(ATransaction: TACBrTEFTPagTransactionPartial; sl: TStringList);

    property DadosDaTransacao: TStringList read fDadosDaTransacao;

    property OnExibeMensagem: TACBrTEFTPagExibeMensagem read fOnExibeMensagem
      write fOnExibeMensagem;
    property OnTransacaoEmAndamento: TACBrTEFTPagTransacaoEmAndamento read fOnTransacaoEmAndamento
      write fOnTransacaoEmAndamento;

    procedure GravarLog(const AString: AnsiString; Traduz: Boolean = False);
    procedure ExibirMensagem(const AMsg: String);

    procedure TratarErroTPag(AErrorCode: LongInt);
  end;

function GetTEFTPagAPI: TACBrTEFTPagAPI;
function ReturnCodesToStr(ReturnCode: LongInt): String;

procedure CallBackProcess(code: Cardinal; process: PAnsiChar); cdecl;
procedure CallBackError(code: Cardinal; error: PAnsiChar); cdecl;
procedure CallBackSuccess(success: PAnsiChar); cdecl;
function CallBackAbortProcess: LongInt; cdecl;

var
 vTEFTPagAPI : TACBrTEFTPagAPI;

implementation

uses
  TypInfo, StrUtils, Math,
  ACBrUtil.FilesIO,
  ACBrUtil.Strings,
  ACBrUtil.DateTime;

function GetTEFTPagAPI: TACBrTEFTPagAPI;
begin
  if not Assigned(vTEFTPagAPI) then
    vTEFTPagAPI := TACBrTEFTPagAPI.Create;

  Result := vTEFTPagAPI;
end;

function ReturnCodesToStr(ReturnCode: LongInt): String;
begin
  case ReturnCode of
    ReturnCode_OK                             : Result := 'OK';
    ReturnCode_INTERNAL_ERROR                 : Result := 'INTERNAL_ERROR';
    ReturnCode_INVALID_CALL                   : Result := 'INVALID_CALL';
    ReturnCode_INVALID_PARAMETER              : Result := 'INVALID_PARAMETER';
    ReturnCode_TIMEOUT                        : Result := 'TIMEOUT';
    ReturnCode_CANCELED_OPERATION             : Result := 'CANCELED_OPERATION';
    ReturnCode_BUSY_PINPAD                    : Result := 'BUSY_PINPAD';
    ReturnCode_INVALID_MODEL                  : Result := 'INVALID_MODEL';
    ReturnCode_EXPIRED_TABLES                 : Result := 'EXPIRED_TABLES';
    ReturnCode_MAG_CARD_READ_ERROR            : Result := 'MAG_CARD_READ_ERROR';
    ReturnCode_MISSING_PIN_KEY                : Result := 'MISSING_PIN_KEY';
    ReturnCode_MISSING_CARD                   : Result := 'MISSING_CARD';
    ReturnCode_SAM_MODULE_ERROR               : Result := 'SAM_MODULE_ERROR';
    ReturnCode_INVALID_SAM                    : Result := 'INVALID_SAM';
    ReturnCode_MISSING_SAM                    : Result := 'MISSING_SAM';
    ReturnCode_MUTE_CARD                      : Result := 'MUTE_CARD';
    ReturnCode_CARD_COMMUNICATION_ERROR       : Result := 'CARD_COMMUNICATION_ERROR';
    ReturnCode_CARD_WITH_INVALID_DATA         : Result := 'CARD_WITH_INVALID_DATA';
    ReturnCode_BLOCKED_CARD                   : Result := 'BLOCKED_CARD';
    ReturnCode_CARD_WITHOUT_APPLICATION       : Result := 'CARD_WITHOUT_APPLICATION';
    ReturnCode_INVALIDATED_CARD               : Result := 'INVALIDATED_CARD';
    ReturnCode_PROBLEMATIC_CARD               : Result := 'PROBLEMATIC_CARD';
    ReturnCode_CARD_WITH_INVALID_DATA_2       : Result := 'CARD_WITH_INVALID_DATA_2';
    ReturnCode_CARD_WITHOUT_APPLICATION_2     : Result := 'CARD_WITHOUT_APPLICATION_2';
    ReturnCode_UNUSED_APPLICATION             : Result := 'UNUSED_APPLICATION';
    ReturnCode_FALLBACK_ERROR                 : Result := 'FALLBACK_ERROR';
    ReturnCode_MULTIPLE_CTLSS                 : Result := 'MULTIPLE_CTLSS';
    ReturnCode_CTLSS_COMMUNICATION_ERROR      : Result := 'CTLSS_COMMUNICATION_ERROR';
    ReturnCode_INVALIDATED_CTLSS              : Result := 'INVALIDATED_CTLSS';
    ReturnCode_PROBLEMATIC_CTLSS              : Result := 'PROBLEMATIC_CTLSS';
    ReturnCode_CTLSS_WITHOUT_APPLICATION      : Result := 'CTLSS_WITHOUT_APPLICATION';
    ReturnCode_UNSUPPORTED_CTLSS_APPLICATION  : Result := 'UNSUPPORTED_CTLSS_APPLICATION';
    ReturnCode_EXTERNAL_CTLSS_DEVICE          : Result := 'EXTERNAL_CTLSS_DEVICE';
    ReturnCode_CTLSS_CHANGE_INTERFACE         : Result := 'CTLSS_CHANGE_INTERFACE';
    ReturnCode_ABORTED_OPERATION              : Result := 'ABORTED_OPERATION';
    ReturnCode_CARD_CHIP                      : Result := 'CARD_CHIP';
    ReturnCode_PRODUCT_EMPTY                  : Result := 'PRODUCT_EMPTY';
    ReturnCode_DIVERT_CARD_NUMBER             : Result := 'DIVERT_CARD_NUMBER';
    ReturnCode_REJECTED_TRANSACTION           : Result := 'REJECTED_TRANSACTION';
    ReturnCode_CARD_REJECTED                  : Result := 'CARD_REJECTED';
    ReturnCode_INVALID_TRANSACTION            : Result := 'INVALID_TRANSACTION';
    ReturnCode_PRINT_ERROR                    : Result := 'PRINT_ERROR';
    ReturnCode_EMPTY_TRANSACTION              : Result := 'EMPTY_TRANSACTION';
    ReturnCode_CARD_NUMBER_INVALID            : Result := 'CARD_NUMBER_INVALID';
    ReturnCode_INVALID_EXP_DATE               : Result := 'INVALID_EXP_DATE';
    ReturnCode_NONE_PRODUCTS                  : Result := 'NONE_PRODUCTS';
    ReturnCode_NO_CREDIT_INSTALLMENT_SELECTED : Result := 'NO_CREDIT_INSTALLMENT_SELECTED';
    ReturnCode_LOGON_NOT_PERFORMED            : Result := 'LOGON_NOT_PERFORMED';
    ReturnCode_TABLE_WRITE_ERROR              : Result := 'TABLE_WRITE_ERROR';
    ReturnCode_DEVICE_NOT_FOUND               : Result := 'DEVICE_NOT_FOUND';
    ReturnCode_DEVICE_NOT_CONNECTED           : Result := 'DEVICE_NOT_CONNECTED';
    ReturnCode_CONFIG_NOT_CALLED              : Result := 'CONFIG_NOT_CALLED';
    ReturnCode_TRANSACTION_NOT_FOUND          : Result := 'TRANSACTION_NOT_FOUND';
  else
    Result := 'ReturnCode: '+IntToStr(Integer(ReturnCode));
  end;
end;

procedure CallBackProcess(code: Cardinal; process: PAnsiChar); cdecl;
var
  s: String;
begin
  if (process = nil) then
    s := ''
  else
    s := TrimRight(String(process));

  with GetTEFTPagAPI do
  begin
    GravarLog( Format('   Callback Process: %d - %s ', [code, s] ));
    ExibirMensagem(s);
  end;
end;

procedure CallBackError(code: Cardinal; error: PAnsiChar); cdecl;
var
  s: String;
begin
  if (error = nil) then
    s := ''
  else
    s := TrimRight(String(error));

  with GetTEFTPagAPI do
  begin
    GravarLog( Format('   Callback Error: %d - %s ', [code, s] ));
    ExibirMensagem(s);
    DadosDaTransacao.Values['msgError'] := s;
  end;
end;

procedure CallBackSuccess(success: PAnsiChar); cdecl;
var
  s: String;
begin
  if (success = nil) then
    s := ''
  else
    s := TrimRight(String(success));

  with GetTEFTPagAPI do
  begin
    GravarLog(  Format('   Callback Sucess: %s ', [s] ));
    ExibirMensagem(s);
    DadosDaTransacao.Values['msgSuccess'] := s;
  end;
end;

function CallBackAbortProcess: LongInt; cdecl;
var
  estado: TACBrTEFTPagEstadoOperacao;
  Cancelar: Boolean;
begin
  Result:= 0;  // Continuar..

  with GetTEFTPagAPI do
  begin
    if Assigned(OnTransacaoEmAndamento) then
    begin
      estado := tpagEstPinPad;
      Cancelar := False;
      GravarLog('  OnTransacaoEmAndamento( '+GetEnumName(TypeInfo(TACBrTEFTPagEstadoOperacao), integer(estado))+' )');
      OnTransacaoEmAndamento(estado, Cancelar);
      GravarLog('    Cancelar: '+BoolToStr(Cancelar, True) );
      if Cancelar then
        Result := 1;  // Abortar
    end;
  end;
end;

{ TACBrTEFTPagAPI }

constructor TACBrTEFTPagAPI.Create;
begin
  inherited;

  fOnGravarLog := Nil;
  fOnExibeMensagem := Nil;
  fOnTransacaoEmAndamento := Nil;

  fCarregada := False;
  fInicializada := False;
  fCarregada := False;
  fEmTransacao := False;
  fPathLib := '';
  fCNPJEmpresa := '';
  fDadosDaTransacao := TStringList.Create;
end;

destructor TACBrTEFTPagAPI.Destroy;
begin
  fDadosDaTransacao.Free;
  fOnGravarLog := Nil;
  fOnExibeMensagem := Nil;
  fOnTransacaoEmAndamento := Nil;
  inherited Destroy;
end;

procedure TACBrTEFTPagAPI.Inicializar;
var
  ret: LongInt;
begin
  if fInicializada then
    Exit;

  fEmTransacao := False;
  GravarLog('TACBrTEFTPagAPI.Inicializar');

  if (fCNPJEmpresa = '') then
    DoException(sErrCNPJNaoInformado);

  if not Assigned(fOnTransacaoEmAndamento) then
    DoException(Format(sErrEventoNaoAtribuido, ['OnTransacaoEmAndamento']));
  if not Assigned(fOnExibeMensagem) then
    DoException(Format(sErrEventoNaoAtribuido, ['OnExibeMensagem']));

  LoadLibFunctions;
  GravarLog('  call - Configuration');
  ret := xTPagConfiguration( CallBackProcess, CallBackError, CallBackSuccess, CallBackAbortProcess );
  GravarLog('   ret - '+IntToStr(ret));
  TratarErroTPag(ret);

  fInicializada := True;
end;

procedure TACBrTEFTPagAPI.DesInicializar;
begin
  if not fInicializada then
    Exit;

  GravarLog('TACBrTEFTPagAPI.DesInicializar');
  UnLoadLibFunctions;
  fInicializada := False;
  fConectada := False;
end;

procedure TACBrTEFTPagAPI.Conectar;
var
  ret: LongInt;
begin
  if fConectada then
    Exit;

  GravarLog('  call - Initialization('+fCNPJEmpresa+')');
  ret := xTPagInitialization(PAnsiChar(AnsiString(fCNPJEmpresa)));
  GravarLog('   ret - '+IntToStr(ret));
  TratarErroTPag(ret);
  fConectada := True;
end;

function TACBrTEFTPagAPI.Transacao(Params: TACBrTEFTPagTransactionParams): LongInt;
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

procedure TACBrTEFTPagAPI.AbortarTransacao;
begin
  fEmTransacao := False;
end;

function TACBrTEFTPagAPI.Cancelamento(const nsuResponse: String;
  CardType: Cardinal): LongInt;
var
  s: AnsiString;
begin
  PrepararInicioDeTrancao;
  GravarLog('  call - Cancellation( '+nsuResponse+', '+IntToStr(CardType)+' )' );

  s := Trim(nsuResponse);
  if (s = '') then
    DoException(sErrNSUNaoInformado);

  fEmTransacao := True;
  try
    Result := xTPagCancellation(PAnsiChar(s), CardType);
    GravarLog('   ret - '+IntToStr(Result));
    fDadosDaTransacao.Values['ret'] := IntToStr(Result);
  finally
    fEmTransacao := False;
  end;
end;

function TACBrTEFTPagAPI.AtualizarTabelas: LongInt;
begin
  PrepararInicioDeTrancao;
  GravarLog('  call - UpdateTable');
  Result := xTPagUpdateTable;
  GravarLog('   ret - '+IntToStr(Result));
end;

function TACBrTEFTPagAPI.ReiniciarTerminal: LongInt;
begin
  PrepararInicioDeTrancao;
  GravarLog('  call - ResetTerminal');
  Result := xTPagResetTerminal;
  GravarLog('   ret - '+IntToStr(Result));
  if (Result = ReturnCode_OK) then
    fConectada := False;
end;

function TACBrTEFTPagAPI.UltimoRecibo(EhConsumidor, EhCancelamento,
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
  if (errorCode = ReturnCode_OK) then
    Result := String(p)
  else
    Result := '';

  GravarLog('   LastReceipt:' + sLineBreak + Result);
end;

function TACBrTEFTPagAPI.ObterListaTransacoes(
  Params: TACBrTEFTPagTransactionFilter; var num, errorCode: LongInt
  ): PACBrTEFTPagTransactionPartial;
begin
  PrepararInicioDeTrancao;
  errorCode := -1;
  num := 0;
  GravarLog('  call - ListTransactionsStore');
  Result := xTPagListTransactionsStore(Params, num, errorCode);
  GravarLog('   ret - '+IntToStr(errorCode)+', num - '+IntToStr(num));
end;

function TACBrTEFTPagAPI.ObterTransacao(
  TransactionList: PACBrTEFTPagTransactionPartial; index: Integer
  ): TACBrTEFTPagTransactionPartial;
var
  p: PACBrTEFTPagTransactionPartial;
begin
  p := TransactionList;
  inc(p, index);
  move(p^, Result, SizeOf(TACBrTEFTPagTransactionPartial));
end;

procedure TACBrTEFTPagAPI.LiberarListaTransacoes(
  TransactionList: PACBrTEFTPagTransactionPartial; num: LongInt);
begin
  GravarLog('  call - FreeTransactionPartialList( '+IntToStr(num)+' )');
  xTPagFreeTransactionPartialList(TransactionList, num);
end;

procedure TACBrTEFTPagAPI.ObterUltimaTransacao(LastTransactionType: Cardinal;
  var errorCode: LongInt);
var
  p: PACBrTEFTPagTransactionPartial;
begin
  PrepararInicioDeTrancao;
  errorCode := -1;
  GravarLog('  call - LastTransactionStore( '+IntToStr(LastTransactionType)+' )');
  p := xTPagLastTransactionStore(LastTransactionType, errorCode);
  GravarLog('   ret - '+IntToStr(errorCode));
  if (errorCode = ReturnCode_OK) and Assigned(p) then
  begin
    TransacaoToStr(p^, fDadosDaTransacao);
    LiberarListaTransacoes(p, -1);
  end;
end;

procedure TACBrTEFTPagAPI.TransacaoToStr(
  ATransaction: TACBrTEFTPagTransactionPartial; sl: TStringList);
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

procedure TACBrTEFTPagAPI.GravarLog(const AString: AnsiString; Traduz: Boolean);
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

procedure TACBrTEFTPagAPI.ExibirMensagem(const AMsg: String);
begin
  if Assigned(fOnExibeMensagem) then
    fOnExibeMensagem(AMsg);
end;

procedure TACBrTEFTPagAPI.SetInicializada(AValue: Boolean);
begin
  if fInicializada = AValue then
    Exit;

  GravarLog('TACBrTEFTPagAPI.SetInicializada( '+BoolToStr(AValue, True)+' )');

  if AValue then
    Inicializar
  else
    DesInicializar;
end;

procedure TACBrTEFTPagAPI.SetCNPJEmpresa(AValue: String);
begin
  fCNPJEmpresa := OnlyNumber(AValue);
end;

procedure TACBrTEFTPagAPI.SetPathLib(AValue: String);
begin
  if fPathLib = AValue then
    Exit;

  GravarLog('TACBrTEFTPagAPI.SetPathLib( '+AValue+' )');

  if fInicializada then
    DoException(sErrLibJaInicializada);

  fPathLib := PathWithDelim(ExtractFilePath(AValue));
end;

function TACBrTEFTPagAPI.GetLibFullPath: String;
begin
  if (PathLib <> '') then
  begin
    GravarLog(ACBrStr('TACBrTEFTPagAPI.LibFullName: Usando "PathLib" informado pela aplicação: ')+PathLib);
    Result := PathLib + CTPagLib;
  end
  else
    Result := ApplicationPath + CTPagLib;
end;

procedure TACBrTEFTPagAPI.LoadLibFunctions;

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
          DoException(Format('Erro ao carregar a função: %s de: %s',[FuncName, LibName]))
        else
          GravarLog(Format('     Função não requerida: %s não encontrada em: %s',[FuncName, LibName]));
        end ;
    end ;
  end;

var
  sLibName: string;
begin
  if fCarregada then
    Exit;

  sLibName := GetLibFullPath;
  GravarLog('TACBrTEFTPagAPI.LoadDLLFunctions - '+sLibName);

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

procedure TACBrTEFTPagAPI.UnLoadLibFunctions;
var
  sLibName: String;
begin
  if not fCarregada then
    Exit;

  GravarLog('TACBrTEFTPagAPI.UnLoadDLLFunctions');

  sLibName := GetLibFullPath;
  UnLoadLibrary( sLibName );
  fCarregada := False;
  ClearMethodPointers;
end;

procedure TACBrTEFTPagAPI.ClearMethodPointers;
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

procedure TACBrTEFTPagAPI.DoException(const AErrorMsg: String);
begin
  if (Trim(AErrorMsg) = '') then
    Exit;

  GravarLog('TACBrTEFTPagAPI: '+AErrorMsg);
  raise EACBrTEFTPagAPI.Create(ACBrStr(AErrorMsg));
end;

procedure TACBrTEFTPagAPI.PrepararInicioDeTrancao;
begin
  Conectar;
  fDadosDaTransacao.Clear;
end;

procedure TACBrTEFTPagAPI.TratarErroTPag(AErrorCode: LongInt);
begin
  if (AErrorCode = ReturnCode_OK) then
    Exit;

  DoException(ReturnCodesToStr(AErrorCode));
end;

initialization
  vTEFTPagAPI := nil;

finalization
  if Assigned(vTEFTPagAPI) then
    FreeAndNil(vTEFTPagAPI);

end.

