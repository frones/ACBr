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
  Classes, SysUtils;

const
  {$IFDEF MSWINDOWS}
   CTPagLib = 'libDM_SDK.dll';
  {$ELSE}
   CTPagLib = 'libDM_SDK.so';
  {$ENDIF}

resourcestring
  sErrLibJaInicializda = 'Biblioteca TPag já foi inicializada';
  sErrEventoNaoAtribuido = 'Evento %s não atribuido';
  sErrCNPJNaoInformado = 'CNPJEmpresa não informado';

type
  EACBrTEFTPagAPI = class(Exception);

  //TACBrTEFTPagCreditType = ( CreditType_NO_INSTALLMENT,
  //                           CreditType_INSTALLMENT );

  //TACBrTEFTPagCardType = ( CardType_EMV = 1,
  //                         CardType_CTLS,
  //                         CardType_NONE);
const
  CreditType_NO_INSTALLMENT = 0;
  CreditType_INSTALLMENT = 1;

  CardType_EMV = 0;
  CardType_CTLS = 1;
  CardType_NONE = 2;

  TransactionType_DEBIT = 0;
  TransactionType_CREDIT = 1;
  TransactionType_VOUCHER = 2;

type
  TACBrTEFTPagReadCardType = ( ReadCardType_MAGNETIC,
                               ReadCardType_M1,
                               ReadCardType_M2,
                               ReadCardType_EMV_CONTACT,
                               ReadCardType_TIB,
                               ReadCardType_CONTACTLESS_STRIPE,
                               ReadCardType_CONTACTLESS_EMV,
                               ReadCardType_TYPED) ;

  TACBrTEFTPagTransactionStatus = ( TransactionStatus_CONFIRMED,
                                    TransactionStatus_UNDONE,
                                    TransactionStatus_PENDING,
                                    TransactionStatus_PENDING_CONFIRMATION,
                                    TransactionStatus_UNDO,
                                    TransactionStatus_PENDING_UNDO,
                                    TransactionStatus_REJECTED,
                                    TransactionStatus_CANCELLED );

  TACBrTEFTPagReasonUndo = ( ReasonUndo_TIME_OUT,
                             ReasonUndo_DENIED_BY_CARD,
                             ReasonUndo_REMOVED_CARD );


  TACBrTEFTPagNotificationType = ( FREE_DSP,
                                   DSP_2X16,
                                   PROCESSING_DSP,
                                   INSERT_PASS_CARD,
                                   APPROACH_INSERT_PASS_CARD,
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
                                   SUCCESS_MESSAGE);

  TACBrTEFTPagReturnCodes = (
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
    TRANSACTION_NOT_FOUND = 1019
  );


  TACBrTEFTPagTransactionParams = record
    amount: Int64;
    creditType: Cardinal; // TACBrTEFTPagCreditType;
    cardType: Cardinal; // TACBrTEFTPagCardType;
    transactionType: Cardinal; // TACBrTEFTPagTransactionType;
    installment: LongInt;
    isTyped: LongInt;
  end;

  TACBrTEFTPagTransactionFilter = record
    startDate: Int64;
    endDate: Int64;
    statusSize: LongInt;
    status: array[0..7] of TACBrTEFTPagTransactionStatus;
    readCardTypeSize: LongInt;
    readCardType: array[0..7] of TACBrTEFTPagReadCardType;
  end;

  PACBrTEFTPagTransactionPartial = ^TACBrTEFTPagTransactionPartial;
  TACBrTEFTPagTransactionPartial = record
    nsuRequest: PAnsiChar;
    amount: Int64;
    typeTransaction: PAnsiChar;
    installments: LongInt;
    transactionStatus: TACBrTEFTPagTransactionStatus;
    date: Int64; //UNIX time
    nsuResponse: PAnsiChar;
    reasonUndo: TACBrTEFTPagReasonUndo;
    transactionReceipt: PAnsiChar;
    brand: PAnsiChar;
    authentication: LongInt;
    entryMode: TACBrTEFTPagReadCardType;
    merchantCode: PAnsiChar;
    nsuAcquirer: PAnsiChar;
    authAcquirer: PAnsiChar;
    printReceipt: Boolean;
    panMasked: PAnsiChar;
  end;

  TACBrTEFTPagGravarLog = procedure(const ALogLine: String; var Tratado: Boolean) of object ;

  TACBrTEFTPagExibeMensagem = procedure( const Mensagem: String) of object;

  TACBrTPagCallBackProcess = procedure(code: TACBrTEFTPagNotificationType; process: AnsiChar);
  TACBrTPagCallBackError = procedure(code: TACBrTEFTPagReturnCodes; error: PAnsiChar);
  TACBrTPagCallBackSuccess = procedure(success: PAnsiChar);

  { TACBrTEFTPagAPI }

  TACBrTEFTPagAPI = Class
  private
    fCarregada: Boolean;
    fCNPJEmpresa: String;
    fConectado: Boolean;
    fInicializada: Boolean;
    fPathLib: String;
    fOnGravarLog: TACBrTEFTPagGravarLog;
    fOnExibeMensagem: TACBrTEFTPagExibeMensagem;

  private
    xTPagConfiguration: function( messageProcess: TACBrTPagCallBackProcess;
                                  messageError: TACBrTPagCallBackError;
                                  messageSuccess: TACBrTPagCallBackSuccess): LongInt; cdecl;

    xTPagInitialization: function(identification: PAnsiChar): LongInt; cdecl;

    xTPagTransaction: function(transactionParams: TACBrTEFTPagTransactionParams): LongInt; cdecl;

    xTPagCancellation: function(nsuResponse: PAnsiChar; cardType: SmallInt {TACBrTEFTPagCardType}): LongInt; cdecl;

    xTPagUpdateTable: function(): LongInt; cdecl;

    xTPagListTransactionsStore: function(transactionFilter: TACBrTEFTPagTransactionFilter;
      var outputCount: LongInt; var errorCode: LongInt ): PACBrTEFTPagTransactionPartial; cdecl;

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
    procedure TratarErroTPag(AErrorCode: TACBrTEFTPagReturnCodes);

  public
    constructor Create;
    destructor Destroy; override;

    property PathLib: String read fPathLib write SetPathLib;

    property CNPJEmpresa: String read fCNPJEmpresa write SetCNPJEmpresa;

    property Carregada: Boolean read fCarregada;
    property Inicializada: Boolean read fInicializada write SetInicializada;
    property Conectado: Boolean read fConectado;

    property OnGravarLog: TACBrTEFTPagGravarLog read fOnGravarLog write fOnGravarLog;

    procedure Inicializar;
    procedure DesInicializar;

    function ExecutarTransacao(Params: TACBrTEFTPagTransactionParams): LongInt;


    property OnExibeMensagem: TACBrTEFTPagExibeMensagem read fOnExibeMensagem
      write fOnExibeMensagem;
    procedure GravarLog(const AString: AnsiString; Traduz: Boolean = False);
    procedure ExibirMensagem(const AMsg: String);
  end;

function GetTEFTPagAPI: TACBrTEFTPagAPI;
function ReturnCodesToStr(ReturnCode: TACBrTEFTPagReturnCodes): String;

procedure CallBackProcess(code: TACBrTEFTPagNotificationType; process: AnsiChar);
procedure CallBackError(code: TACBrTEFTPagReturnCodes; error: PAnsiChar);
procedure CallBackSuccess(success: PAnsiChar);


var
 TEFTPagAPI : TACBrTEFTPagAPI;

implementation

uses
  TypInfo,
  ACBrUtil.FilesIO,
  ACBrUtil.Strings;

function GetTEFTPagAPI: TACBrTEFTPagAPI;
begin
  if not Assigned(TEFTPagAPI) then
    TEFTPagAPI := TACBrTEFTPagAPI.Create;

  Result := TEFTPagAPI;
end;

function ReturnCodesToStr(ReturnCode: TACBrTEFTPagReturnCodes): String;
begin
  case ReturnCode of
    OK                                  : Result := 'OK';
    INTERNAL_ERROR                      : Result := 'INTERNAL_ERROR';
    INVALID_CALL                        : Result := 'INVALID_CALL';
    INVALID_PARAMETER                   : Result := 'INVALID_PARAMETER';
    TIMEOUT                             : Result := 'TIMEOUT';
    CANCELED_OPERATION                  : Result := 'CANCELED_OPERATION';
    BUSY_PINPAD                         : Result := 'BUSY_PINPAD';
    INVALID_MODEL                       : Result := 'INVALID_MODEL';
    EXPIRED_TABLES                      : Result := 'EXPIRED_TABLES';
    MAG_CARD_READ_ERROR                 : Result := 'MAG_CARD_READ_ERROR';
    MISSING_PIN_KEY                     : Result := 'MISSING_PIN_KEY';
    MISSING_CARD                        : Result := 'MISSING_CARD';
    SAM_MODULE_ERROR                    : Result := 'SAM_MODULE_ERROR';
    INVALID_SAM                         : Result := 'INVALID_SAM';
    MISSING_SAM                         : Result := 'MISSING_SAM';
    MUTE_CARD                           : Result := 'MUTE_CARD';
    CARD_COMMUNICATION_ERROR            : Result := 'CARD_COMMUNICATION_ERROR';
    CARD_WITH_INVALID_DATA              : Result := 'CARD_WITH_INVALID_DATA';
    BLOCKED_CARD                        : Result := 'BLOCKED_CARD';
    CARD_WITHOUT_APPLICATION            : Result := 'CARD_WITHOUT_APPLICATION';
    INVALIDATED_CARD                    : Result := 'INVALIDATED_CARD';
    PROBLEMATIC_CARD                    : Result := 'PROBLEMATIC_CARD';
    CARD_WITH_INVALID_DATA_2            : Result := 'CARD_WITH_INVALID_DATA_2';
    CARD_WITHOUT_APPLICATION_2          : Result := 'CARD_WITHOUT_APPLICATION_2';
    UNUSED_APPLICATION                  : Result := 'UNUSED_APPLICATION';
    FALLBACK_ERROR                      : Result := 'FALLBACK_ERROR';
    MULTIPLE_CTLSS                      : Result := 'MULTIPLE_CTLSS';
    CTLSS_COMMUNICATION_ERROR           : Result := 'CTLSS_COMMUNICATION_ERROR';
    INVALIDATED_CTLSS                   : Result := 'INVALIDATED_CTLSS';
    PROBLEMATIC_CTLSS                   : Result := 'PROBLEMATIC_CTLSS';
    CTLSS_WITHOUT_APPLICATION           : Result := 'CTLSS_WITHOUT_APPLICATION';
    UNSUPPORTED_CTLSS_APPLICATION       : Result := 'UNSUPPORTED_CTLSS_APPLICATION';
    EXTERNAL_CTLSS_DEVICE               : Result := 'EXTERNAL_CTLSS_DEVICE';
    CTLSS_CHANGE_INTERFACE              : Result := 'CTLSS_CHANGE_INTERFACE';
    ABORTED_OPERATION                   : Result := 'ABORTED_OPERATION';
    CARD_CHIP                           : Result := 'CARD_CHIP';
    PRODUCT_EMPTY                       : Result := 'PRODUCT_EMPTY';
    DIVERT_CARD_NUMBER                  : Result := 'DIVERT_CARD_NUMBER';
    REJECTED_TRANSACTION                : Result := 'REJECTED_TRANSACTION';
    CARD_REJECTED                       : Result := 'CARD_REJECTED';
    INVALID_TRANSACTION                 : Result := 'INVALID_TRANSACTION';
    PRINT_ERROR                         : Result := 'PRINT_ERROR';
    EMPTY_TRANSACTION                   : Result := 'EMPTY_TRANSACTION';
    CARD_NUMBER_INVALID                 : Result := 'CARD_NUMBER_INVALID';
    INVALID_EXP_DATE                    : Result := 'INVALID_EXP_DATE';
    NONE_PRODUCTS                       : Result := 'NONE_PRODUCTS';
    NO_CREDIT_INSTALLMENT_SELECTED      : Result := 'NO_CREDIT_INSTALLMENT_SELECTED';
    LOGON_NOT_PERFORMED                 : Result := 'LOGON_NOT_PERFORMED';
    TABLE_WRITE_ERROR                   : Result := 'TABLE_WRITE_ERROR';
    DEVICE_NOT_FOUND                    : Result := 'DEVICE_NOT_FOUND';
    DEVICE_NOT_CONNECTED                : Result := 'DEVICE_NOT_CONNECTED';
    CONFIG_NOT_CALLED                   : Result := 'CONFIG_NOT_CALLED';
    TRANSACTION_NOT_FOUND               : Result := 'TRANSACTION_NOT_FOUND';
  else
    Result := 'ReturnCode: '+IntToStr(Integer(ReturnCode));
  end;
end;

procedure CallBackProcess(code: TACBrTEFTPagNotificationType;
  process: AnsiChar);
var
  s: String;
begin
  //if (process = nil) then
  //  s := ''
  //else
    s := TrimRight(String(process));

  with GetTEFTPagAPI do
  begin
    GravarLog( Format('   Callback Process: code: %s %s ',
      [GetEnumName(TypeInfo(TACBrTEFTPagNotificationType), integer(code)), s] ));
    //ExibirMensagem(s);  //TODO: verificar porque sempre vem vazio
  end;
end;

procedure CallBackError(code: TACBrTEFTPagReturnCodes; error: PAnsiChar);
var
  s: String;
begin
  if (error = nil) then
    s := ''
  else
    s := TrimRight(String(error));

  with GetTEFTPagAPI do
  begin
    GravarLog( Format('   Callback Error: code: %s %s ', [ReturnCodesToStr(code), s] ));
    ExibirMensagem(s);
  end;
end;

procedure CallBackSuccess(success: PAnsiChar);
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
  end;
end;


{ TACBrTEFTPagAPI }

constructor TACBrTEFTPagAPI.Create;
begin
  inherited;

  fOnGravarLog := nil;
  fOnExibeMensagem := nil;

  fCarregada := False;
  fInicializada := False;
  fConectado := False;
  fPathLib := '';
  fCNPJEmpresa := '';
end;

destructor TACBrTEFTPagAPI.Destroy;
begin
  fOnGravarLog := nil;
  fOnExibeMensagem := nil;
  inherited Destroy;
end;

procedure TACBrTEFTPagAPI.Inicializar;
var
  i: LongInt;
begin
  if fInicializada then
    Exit;

  fConectado := False;
  GravarLog('TACBrTEFTPagAPI.Inicializar');

  if (fCNPJEmpresa = '') then
    DoException(sErrCNPJNaoInformado);

  if not Assigned(fOnExibeMensagem) then
    DoException(Format(sErrEventoNaoAtribuido, ['OnExibeMensagem']));

  LoadLibFunctions;
  GravarLog('  call - configuration');
  i := xTPagConfiguration( CallBackProcess, CallBackError, CallBackSuccess );
  TratarErroTPag(TACBrTEFTPagReturnCodes(i));

  GravarLog('  call - initialization('+fCNPJEmpresa+')');
  i := xTPagInitialization(PAnsiChar(AnsiString(fCNPJEmpresa)));
  TratarErroTPag(TACBrTEFTPagReturnCodes(i));

  fInicializada := True;
end;

procedure TACBrTEFTPagAPI.DesInicializar;
begin
  if not fInicializada then
    Exit;

  GravarLog('TACBrTEFTPagAPI.DesInicializar');
  UnLoadLibFunctions;
  fInicializada := False;
end;

function TACBrTEFTPagAPI.ExecutarTransacao(Params: TACBrTEFTPagTransactionParams
  ): LongInt;
begin
  GravarLog('  call - transaction' + sLineBreak +
            '  Params.amount: '+IntToStr(Params.amount)+ sLineBreak +
            '  Params.creditType: '+ IntToStr(Params.creditType) + sLineBreak + // GetEnumName(TypeInfo(TACBrTEFTPagCreditType), integer(Params.creditType))+ sLineBreak +
            '  Params.cardType: '+IntToStr(Params.cardType) + sLineBreak +  // GetEnumName(TypeInfo(TACBrTEFTPagCardType), integer(Params.cardType))+ sLineBreak +
            '  Params.transactionType: '+ IntToStr(Params.transactionType) + sLineBreak +  //GetEnumName(TypeInfo(TACBrTEFTPagTransactionType), integer(Params.transactionType))+ sLineBreak +
            '  Params.installment: '+IntToStr(Params.installment)+ sLineBreak +
            '  Params.isTyped: '+IntToStr(Params.isTyped) );

  Result := xTPagTransaction(Params);
  TratarErroTPag( TACBrTEFTPagReturnCodes(Result));
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
    DoException(sErrLibJaInicializda);

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

procedure TACBrTEFTPagAPI.TratarErroTPag(AErrorCode: TACBrTEFTPagReturnCodes);
begin
  if (AErrorCode = TACBrTEFTPagReturnCodes(OK)) then
    Exit;

  DoException(ReturnCodesToStr(AErrorCode));
end;

initialization
  TEFTPagAPI := nil;

finalization
  if Assigned(TEFTPagAPI) then
    FreeAndNil(TEFTPagAPI);

end.



