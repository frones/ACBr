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

unit ACBrTEFDirectPinAPI;

interface

uses
  Classes, SysUtils,
  synaser,
  ACBrBase, ACBrJSON;

const
  ACK = $006;
  NAK = $015;
  SYN = $016;
  ETB = $017;

  CDPTimeOut = 90000;
  CDPTimeWait = 1000;

resourcestring
  SErrJson = 'JSon inválido para %s';
  SErrDataInvalid = 'Erro no Payload. Campo: %s não informado';
  SErrParity = 'Valores válidos para Parity: O, E, M, S, N (padrão N)';
  SErrStop = 'Valores válidos para Stop: 0 = Stop 1; 1 = Stop 1,5; 2 = Stop 2 (padrão 0)';
  SErrPortEmpty = 'Porta Serial, não definida';
  SErrSerialConnect = 'Erro %d conectando na Porta %s';
  SErrSerialConfig = 'Erro %d configurando a Porta %s';
  SErrNAK = 'DirectPin não reconheceu o comando (NAK)';
  SErrNotACK = 'DirectPin não enviou ACK';
  SErrEmptyResp = 'Sem resposta do DirectPin';
  SErrCanceldUser = 'Cancelado pelo usuário';

type

  TDPtypeTransaction = (dptNONE, dptDEBIT, dptCREDIT, dptVOUCHER, dptPIX);
  TDPcreditType = (dpcNO_INSTALLMENT, dpcINSTALLMENT);
  TDPinterestType = (dpiMERCHANT, dpiISSUER);

  EDPException = class(Exception);

  { TDPPayloadRequestTransaction }

  TDPPayloadRequestTransaction = class
  private
    Famount: Double;
    FcreditType: TDPcredittype;
    Finstallment: Integer;
    FinterestType: TDPinterestType;
    FisPreAuth: Boolean;
    FisTyped: Boolean;
    FprintReceipt: Boolean;
    FtypeTransaction: TDPtypeTransaction;
    Ftype_: String;
    function GetAsJSON: String;
    procedure SetAsJSON(const AValue: String);
  public
    constructor Create;
    procedure Clear;
    property AsJSON: String read GetAsJSON write SetAsJSON;

    property type_: String read Ftype_;
    property amount: Double read Famount write Famount;
    property typeTransaction: TDPtypeTransaction read FtypeTransaction write FtypeTransaction;
    property creditType: TDPcreditType read FcreditType write FcreditType;
    property installment: Integer read Finstallment write Finstallment;
    property isTyped: Boolean read FisTyped write FisTyped;
    property isPreAuth: Boolean read FisPreAuth write FisPreAuth;
    property interestType: TDPinterestType read FinterestType write FinterestType;
    property printReceipt: Boolean read FprintReceipt write FprintReceipt;
  end;

  { TDPPayloadResponseTransaction }

  TDPPayloadResponseTransaction = class
  private
    Famount: Double;
    FcodeResult: Integer;
    Fdate: TDateTime;
    FfinalResult: String;
    Fmessage: String;
    Fnsu: String;
    FnsuAcquirer: String;
    FpanMasked: String;
    FreceiptContent: String;
    Fresult_: Boolean;
    FtypeCard: String;
    Ftype_: String;
    function GetAsJSON: String;
    procedure SetAsJSON(AValue: String);
  public
    constructor Create;
    procedure Clear;
    property AsJSON: String read GetAsJSON write SetAsJSON;

    property type_: String read Ftype_;
    property result_: Boolean read Fresult_ write Fresult_;
    property message: String read Fmessage write Fmessage;
    property amount: Double read Famount write Famount;
    property nsu: String read Fnsu write Fnsu;
    property nsuAcquirer: String read FnsuAcquirer write FnsuAcquirer;
    property panMasked: String read FpanMasked write FpanMasked;
    property date: TDateTime read Fdate write Fdate;
    property typeCard: String read FtypeCard write FtypeCard;
    property finalResult: String read FfinalResult write FfinalResult;
    property codeResult: Integer read FcodeResult write FcodeResult;
    property receiptContent: String read FreceiptContent write FreceiptContent;
  end;

  { TDPPayLoadRequestReversal }

  TDPPayLoadRequestReversal = class
  private
    Fnsu: String;
    Ftype_: String;
    function GetAsJSON: String;
    procedure SetAsJSON(AValue: String);
  public
    constructor Create;
    procedure Clear;
    property AsJSON: String read GetAsJSON write SetAsJSON;

    property type_: String read Ftype_;
    property nsu: String read Fnsu write Fnsu;
  end;

  { TDPPayLoadResponseReversal }

  TDPPayLoadResponseReversal = class
  private
    Fmessage: String;
    FreceiptContent: String;
    Fresult_: Boolean;
    Ftype_: String;
    function GetAsJSON: String;
    procedure SetAsJSON(AValue: String);
  public
    constructor Create;
    procedure Clear;
    property AsJSON: String read GetAsJSON write SetAsJSON;

    property type_: String read Ftype_;
    property result_: Boolean read Fresult_ write Fresult_;
    property message: String read Fmessage write Fmessage;
    property receiptContent: String read FreceiptContent write FreceiptContent;
  end;

  { TDPSerialMessage }

  TDPSerialMessage = class
  private
    FPayLoad: String;

    function GetMessage: AnsiString;
    procedure SetMessage(AValue: AnsiString);
  protected

  public
    constructor Create;
    procedure Clear;

    property message: AnsiString read GetMessage write SetMessage;
    property PayLoad: String read FPayLoad write FPayLoad;
    function Checksum(const AString: AnsiString): AnsiString;
  end;

  TDPOnWriteLog = procedure(const ALogLine: String; var Tratado: Boolean) of object ;

  TDPOperation = ( dpeRespSerial );
  TDPTransactionInProgress = procedure( EstOperation: TDPOperation; out Cancel: Boolean) of object;

  { TDPAPI }

  TDPAPI = class
  private
    fBaud: Integer;
    fData: Integer;
    fHardFlow: Boolean;
    fInitialized: Boolean;
    fInTransaction: Boolean;
    fOnWriteLog: TDPOnWriteLog;
    fParity: Char;
    fPort: String;
    fprintReceipt: Boolean;
    fSerial: TBlockSerial;
    fSoftFlow: Boolean;
    fStop: Integer;
    fTimeOut: Integer;
    fTimeWait: Integer;
    fTransactionInProgress: TDPTransactionInProgress;
    procedure SetInitialized(AValue: Boolean);
    procedure SetParity(AValue: Char);
    procedure SetPort(AValue: String);
    procedure SetStop(AValue: Integer);

  protected
    procedure ConnectSerial;
    procedure DisconnectSerial;
    procedure ConfigureSerial;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Initialize;
    procedure Deinitialize;
    function SendData(const AJSon: String): Boolean;
    function ReadSerialResponse(ATimeOut: Integer = 0): AnsiString;

    function SerialPortsList: String;
    function RequestTransaction(const AJSon: String): String;
    function RequestReversal(const AJSon: String): String;
    procedure AbortTransaction;

    procedure WriteLog(const AString: AnsiString; Traduz: Boolean = False);

    property Initialized: Boolean read fInitialized write SetInitialized;
    property InTransaction: Boolean read fInTransaction;
    property Serial: TBlockSerial read fSerial;

  published
    property Port: String read fPort write SetPort;
    property Baud: Integer read fBaud write fBaud default 9600;
    property Data: Integer read fData write fData default 8;
    property Parity: Char read fParity write SetParity default 'N';
    property Stop: Integer read fStop write SetStop default 0;
    property SoftFlow: Boolean read fSoftFlow write fSoftFlow default False;
    property HardFlow: Boolean read fHardFlow write fHardFlow default False;

    property printReceipt: Boolean read fprintReceipt write fprintReceipt default False;

    property TimeOut: Integer read fTimeOut write fTimeOut default CDPTimeOut;
    property TimeWait: Integer read fTimeWait write fTimeWait default CDPTimeWait;

    property OnWriteLog: TDPOnWriteLog read fOnWriteLog write fOnWriteLog;
    property TransactionInProgress: TDPTransactionInProgress read fTransactionInProgress
      write fTransactionInProgress;
  end;

  function typeTransactionToString(AtypeTransaction: TDPtypeTransaction): String;
  function StringTotypeTransaction(const AStr: String): TDPtypeTransaction;

  function creditTypeToString(AcreditType: TDPcreditType): String;
  function StringTocreditType(const AStr: String): TDPcreditType;

  function interestTypeToString(AinterestType: TDPinterestType): String;
  function StringTointerestType(const AStr: String): TDPinterestType;

implementation

uses
  DateUtils, Math,
  synacode,
  ACBrUtil.Compatibilidade,
  ACBrUtil.DateTime,
  ACBrUtil.Strings,
  ACBrUtil.FilesIO;

function typeTransactionToString(AtypeTransaction: TDPtypeTransaction): String;
begin
  case AtypeTransaction of
    dptDEBIT: Result := 'DEBIT';
    dptCREDIT: Result := 'CREDIT';
    dptVOUCHER: Result := 'VOUCHER';
    dptPIX: Result := 'PIX';
  else
    Result := 'NONE';
  end;
end;

function StringTotypeTransaction(const AStr: String): TDPtypeTransaction;
var
  s: String;
begin
  s := trim(UpperCase(AStr));
  if (s = 'DEBIT') then
    Result := dptDEBIT
  else if (s = 'CREDIT') then
    Result := dptCREDIT
  else if (s = 'VOUCHER') then
    Result := dptVOUCHER
  else if (s = 'PIX') then
    Result := dptPIX
  else
    Result := dptNONE;
end;

function creditTypeToString(AcreditType: TDPcreditType): String;
begin
  case AcreditType of
    dpcINSTALLMENT: Result := 'INSTALLMENT';
  else
    Result := 'NO_INSTALLMENT';
  end;
end;

function StringTocreditType(const AStr: String): TDPcreditType;
var
  s: String;
begin
  s := trim(UpperCase(AStr));
  if (s = 'INSTALLMENT') then
    Result := dpcINSTALLMENT
  else
    Result := dpcNO_INSTALLMENT;
end;

function interestTypeToString(AinterestType: TDPinterestType): String;
begin
  case AinterestType of
    dpiISSUER: Result := 'ISSUER';
  else
    Result := 'MERCHANT';
  end;
end;

function StringTointerestType(const AStr: String): TDPinterestType;
var
  s: String;
begin
  s := trim(UpperCase(AStr));
  if (s = 'ISSUER') then
    Result := dpiISSUER
  else
    Result := dpiMERCHANT;
end;

{ TDPPayloadRequestTransaction }

constructor TDPPayloadRequestTransaction.Create;
begin
  inherited;
  Clear;
end;

procedure TDPPayloadRequestTransaction.Clear;
begin
  Famount := 0;
  FcreditType := dpcNO_INSTALLMENT;
  Finstallment := 0;
  FinterestType := dpiMERCHANT;
  FisPreAuth := False;
  FisTyped := False;
  FprintReceipt := False;
  FtypeTransaction := dptNONE;
  Ftype_ := 'transaction';
end;

function TDPPayloadRequestTransaction.GetAsJSON: String;
var
  js: TACBrJSONObject;
begin
  js := TACBrJSONObject.Create;
  try
    js.AddPair('type', type_);
    js.AddPair('amount', Trunc(amount*100));
    js.AddPair('typeTransaction', typeTransactionToString(typeTransaction));
    js.AddPair('creditType', creditTypeToString(creditType));
    js.AddPair('installment', installment);
    js.AddPair('isTyped', isTyped);
    js.AddPair('isPreAuth', isPreAuth);
    js.AddPair('interestType', interestTypeToString(interestType));
    js.AddPair('printReceipt', printReceipt);
    Result := js.ToJSON;
  finally
    js.Free;
  end;
end;

procedure TDPPayloadRequestTransaction.SetAsJSON(const AValue: String);
var
  js: TACBrJSONObject;
  s: String;
begin
  Clear;
  js := TACBrJSONObject.Parse(AValue);
  if not Assigned(js) then
    raise EDPException.Create(ACBrStr(Format(SErrJson, ['Payload Request Transaction'])));

  try
    s := js.AsString['type'];
    if (s <> '') then
      Ftype_ := s;

    amount := js.AsInteger['amount']/100;
    typeTransaction := StringTotypeTransaction(js.AsString['typeTransaction']);
    creditType := StringTocreditType(js.AsString['creditType']);
    installment := js.AsInteger['installment'];
    isTyped := js.AsBoolean['isTyped'];
    isPreAuth := js.AsBoolean['isPreAuth'];
    isTyped := js.AsBoolean['isTyped'];
    interestType := StringTointerestType(js.AsString['interestType']);
    printReceipt := js.AsBoolean['printReceipt'];
  finally
    js.Free;
  end;
end;

{ TDPPayloadResponseTransaction }

constructor TDPPayloadResponseTransaction.Create;
begin
  inherited;
  Clear;
end;

procedure TDPPayloadResponseTransaction.Clear;
begin
  Famount := 0;
  FcodeResult := 0;
  Fdate := 0;
  FfinalResult := '';
  Fmessage := '';
  Fnsu := '';
  FnsuAcquirer := '';
  FpanMasked := '';
  FreceiptContent := '';
  Fresult_ := False;
  FtypeCard := '';
  Ftype_ := '';
end;

function TDPPayloadResponseTransaction.GetAsJSON: String;
var
  js: TACBrJSONObject;
begin
  js := TACBrJSONObject.Create;
  try
    js.AddPair('type', type_);
    js.AddPair('result', result_);
    js.AddPair('message', message);
    js.AddPair('amount', Trunc(amount*100));
    js.AddPair('nsu', nsu);
    js.AddPair('nsuAcquirer', nsuAcquirer);
    js.AddPair('panMasked', panMasked);
    js.AddPair('date', DateTimeToUnixMilliseconds(date, False));
    js.AddPair('typeCard', typeCard);
    js.AddPair('finalResult', finalResult);
    js.AddPair('codeResult', codeResult);
    js.AddPair('receiptContent', receiptContent);
    Result := js.ToJSON;
  finally
    js.Free;
  end;
end;

procedure TDPPayloadResponseTransaction.SetAsJSON(AValue: String);
var
  js: TACBrJSONObject;
begin
  Clear;
  js := TACBrJSONObject.Parse(AValue);
  if not Assigned(js) then
    Exit;

  try
    Ftype_ := js.AsString['type'];
    result_ := js.AsBoolean['result'];
    message := js.AsString['message'];
    amount := js.AsInteger['amount']/100;
    nsu := js.AsString['nsu'];
    nsuAcquirer := js.AsString['nsuAcquirer'];
    panMasked := js.AsString['panMasked'];
    date := UnixMillisecondsToDateTime(js.AsInt64['date'], False);
    typeCard := js.AsString['typeCard'];
    finalResult := js.AsString['finalResult'];
    codeResult := js.AsInteger['codeResult'];
    receiptContent := js.AsString['receiptContent'];
  finally
    js.Free;
  end;
end;

{ TDPPayLoadRequestReversal }

constructor TDPPayLoadRequestReversal.Create;
begin
  inherited;
  Clear;
end;

procedure TDPPayLoadRequestReversal.Clear;
begin
  Fnsu := '';
  Ftype_ := 'reversal';
end;

function TDPPayLoadRequestReversal.GetAsJSON: String;
var
  js: TACBrJSONObject;
begin
  js := TACBrJSONObject.Create;
  try
    js.AddPair('type', type_);
    js.AddPair('nsu', nsu);
    Result := js.ToJSON;
  finally
    js.Free;
  end;
end;

procedure TDPPayLoadRequestReversal.SetAsJSON(AValue: String);
var
  js: TACBrJSONObject;
begin
  Clear;
  js := TACBrJSONObject.Parse(AValue);
  if not Assigned(js) then
    raise EDPException.Create(ACBrStr(Format(SErrJson, ['Payload Request Reversal'])));

  try
    Ftype_ := js.AsString['type'];
    nsu := js.AsString['nsu'];
  finally
    js.Free;
  end;
end;

{ TDPPayLoadResponseReversal }

constructor TDPPayLoadResponseReversal.Create;
begin
  inherited;
  Clear;
end;

procedure TDPPayLoadResponseReversal.Clear;
begin
  Fmessage := '';
  FreceiptContent := '';
  Fresult_ := False;
  Ftype_ := '';
end;

function TDPPayLoadResponseReversal.GetAsJSON: String;
var
  js: TACBrJSONObject;
begin
  js := TACBrJSONObject.Create;
  try
    js.AddPair('type', type_);
    js.AddPair('result', result_);
    js.AddPair('message', message);
    js.AddPair('receiptContent', receiptContent);
    Result := js.ToJSON;
  finally
    js.Free;
  end;
end;

procedure TDPPayLoadResponseReversal.SetAsJSON(AValue: String);
var
  js: TACBrJSONObject;
begin
  Clear;
  js := TACBrJSONObject.Parse(AValue);
  if not Assigned(js) then
    Exit;

  try
    Ftype_ := js.AsString['type'];
    result_ := js.AsBoolean['result'];
    message := js.AsString['message'];
    receiptContent := js.AsString['receiptContent'];
  finally
    js.Free;
  end;
end;

{ TDPSerialMessage }

constructor TDPSerialMessage.Create;
begin
  inherited;
  Clear;
end;

procedure TDPSerialMessage.Clear;
begin
  FPayLoad := '';
end;

function TDPSerialMessage.Checksum(const AString: AnsiString): AnsiString;
var
  crc: Word;
  hex: String;
begin
  crc := StringCrcCCITT(AString, 0);
  hex := IntToHex(crc, 2);
  Result := chr(StrToInt('$'+copy(hex,1,2))) + chr(StrToInt('$'+copy(hex,3,2)))
end;

function TDPSerialMessage.GetMessage: AnsiString;
var
  s: String;
begin
  s := EncodeBase64(PayLoad);
  Result := chr(SYN) + s + Checksum(s) + chr(ETB) ;
end;

procedure TDPSerialMessage.SetMessage(AValue: AnsiString);
var
  s, crc1, crc2: AnsiString;
  le: Integer;
begin
  Clear;
  le := Length(AValue);
  if (le < 5) then
    raise Exception.Create('invalid message ');

  if (ord(AValue[1]) <> SYN) then
    raise Exception.Create('the message does not start with SYN');

  if (ord(AValue[le]) <> ETB) then
    raise Exception.Create('the message does not end with ETB');

  s := copy(AValue, 2, Length(AValue)-4);
  crc1 := copy(AValue, Length(AValue)-2, 2);
  crc2 := Checksum(s);
  if (crc1 <> crc2) then
    raise Exception.Create('message with wrong CRC');

  FPayLoad := DecodeBase64(s);
end;

{ TDPAPI }

constructor TDPAPI.Create;
begin
  fSerial := TBlockSerial.Create;
  fSerial.RaiseExcept := False;

  fOnWriteLog := Nil;
  fTransactionInProgress := Nil;

  fPort := '';
  fBaud := 9600;
  fData := 8;
  fParity := 'N';
  fStop := 0;
  fHardFlow := False;
  fSoftFlow := False;
  fprintReceipt := False;
  fTimeOut := CDPTimeOut;
  fTimeWait := CDPTimeWait;
end;

destructor TDPAPI.Destroy;
begin
  fSerial.Free;
  inherited Destroy;
end;

procedure TDPAPI.SetInitialized(AValue: Boolean);
begin
  if (fInitialized = AValue) then
    Exit;

  WriteLog('TDPAPI.SetInicializada( '+BoolToStr(AValue, True)+' )');

  if AValue then
    Initialize
  else
    Deinitialize;
end;

procedure TDPAPI.Initialize;
begin
  if fInitialized then
    Exit;

  WriteLog('TDPAPI.Inicializar');

  ConnectSerial;
  try
    ConfigureSerial;
  finally
    DisconnectSerial;
  end;

  fInitialized := True;
  fInTransaction := False;
end;

procedure TDPAPI.Deinitialize;
begin
  if not fInitialized then
    Exit;

  WriteLog('TDPAPI.DesInicializar');

  DisconnectSerial;
  fInitialized := False;
  fInTransaction := False;
end;

function TDPAPI.SendData(const AJSon: String): Boolean;
var
  dpMessage: TDPSerialMessage;
  s: AnsiString;
  b: Byte;
  t: Integer;
begin
  Result := False;
  dpMessage := TDPSerialMessage.Create;
  try
    dpMessage.PayLoad := AJSon;
    s := dpMessage.message;
  finally
    dpMessage.Free;
  end;

  t := fTimeWait*2;
  fSerial.DeadlockTimeout := t;
  WriteLog('  Serial.SendString( '+s+' )', True);
  fSerial.SendString(s);
  WriteLog(Format('    Serial.LastError: %d', [fSerial.LastError]));
  if (fSerial.LastError <> sOK) then
    Exit;

  WriteLog('  fSerial.RecvByte( '+IntToStr(t)+' )');
  b := fSerial.RecvByte(t);
  if (b = NAK) then
  begin
    WriteLog('    ret: NAK');
    raise EDPException.Create(ACBrStr(SErrNAK))
  end
  else if (b <> ACK) then
  begin
    WriteLog(Format('    ret: %d', [b]));
    raise EDPException.Create(ACBrStr(SErrNotACK));
  end
  else
    WriteLog('    ret: ACK');

  Result := (fSerial.LastError = 0);
end;

function TDPAPI.ReadSerialResponse(ATimeOut: Integer): AnsiString;
var
  i, tries: Integer;
  HasData, Cancel: Boolean;
  b: Byte;
  dpMessage: TDPSerialMessage;
  s: AnsiString;
begin
  if (ATimeOut = 0) then
    ATimeOut := fTimeOut;

  WriteLog(Format('  ReadSerialResponse( %d )', [ATimeOut]));

  Result := '';
  tries := max(Trunc(ATimeOut/fTimeWait), 1);
  i := 0;
  HasData := False;
  Cancel := False;
  while fInTransaction and (not Cancel) and (not HasData) and (i < tries) do
  begin
    HasData := fSerial.CanReadEx(fTimeWait);
    inc(i);
    WriteLog(Format('    %d/%d', [i, tries]));

    if Assigned(fTransactionInProgress) then
      fTransactionInProgress( dpeRespSerial, Cancel);
  end;

  if (not fInTransaction) or Cancel then
    raise EDPException.Create(ACBrStr(SErrCanceldUser));

  s := '';
  if HasData then
  begin
    WriteLog('  Serial.RecvPacket('+IntToStr(ATimeOut)+')');
    s := fSerial.RecvPacket(ATimeOut);
    WriteLog('    ret: '+s, true);
  end;

  if (s = '') then
    raise EDPException.Create(ACBrStr(SErrEmptyResp));

  dpMessage := TDPSerialMessage.Create;
  try
    dpMessage.message := s;
    Result := dpMessage.PayLoad;
  finally
    dpMessage.Free;
  end;

  WriteLog('  Payload: '+Result);
end;

function TDPAPI.SerialPortsList: String;
begin
  WriteLog('TDPAPI.SerialPortsList');
  Result := GetSerialPortNames;
end;

procedure TDPAPI.ConnectSerial;
begin
  WriteLog(Format('  ConnectSerial( %s )', [fPort]));
  if (fPort = '') then
    raise EDPException.Create(ACBrStr(SErrPortEmpty));

  fSerial.Connect(fPort);
  WriteLog(Format('    Serial.LastError: %d', [fSerial.LastError]));
  if (fSerial.LastError <> 0) then
    raise EDPException.Create(Format(ACBrStr(SErrSerialConnect), [fSerial.LastError, fPort]));
end;

procedure TDPAPI.DisconnectSerial;
begin
  WriteLog('DisconnectSerial');
  fSerial.CloseSocket;
end;

procedure TDPAPI.ConfigureSerial;
begin
  WriteLog(Format('  ConfigureSerial: baud: %d, bits: %d, parity: %s, stop: %d, softflow: %s, hardflow: %s',
              [fBaud, fData, fParity, fStop, BoolToStr(fSoftFlow, True), BoolToStr(fHardFlow, True)]));
  fSerial.config(fBaud, fData, fParity, fStop, fSoftFlow, fHardFlow);
  WriteLog(Format('    Serial.LastError: %d', [fSerial.LastError]));
  if (fSerial.LastError <> 0) then
    raise EDPException.Create(Format(ACBrStr(SErrSerialConfig), [fSerial.LastError, fPort]));
end;

function TDPAPI.RequestTransaction(const AJSon: String): String;
var
  ReqTrans: TDPPayloadRequestTransaction;
  ResTrans: TDPPayloadResponseTransaction;
  Payload: String;
begin
  WriteLog('TDPAPI.RequestTransaction( '+AJSon+' )');
  Result := '';

  fInTransaction := True;
  ConnectSerial;
  try
    ConfigureSerial;

    ReqTrans := TDPPayloadRequestTransaction.Create;
    try
      ReqTrans.AsJSON := AJSon;
      if (ReqTrans.amount <= 0) then
        raise EDPException.Create(ACBrStr(Format(SErrDataInvalid, ['amount'])) );

      if (pos('printReceipt', AJSon) = 0) then
        ReqTrans.printReceipt := fprintReceipt;

      Payload := ReqTrans.AsJSON;
    finally
      ReqTrans.Free;
    end;

    if SendData( Payload ) then
      Payload := ReadSerialResponse();

    ResTrans := TDPPayloadResponseTransaction.Create;
    try
      ResTrans.AsJSON := Payload;
      Result := ResTrans.AsJSON;
    finally
      ResTrans.Free;
    end;
  finally
    DisconnectSerial;
    fInTransaction := False;
  end;
end;

function TDPAPI.RequestReversal(const AJSon: String): String;
var
  ReqRev: TDPPayLoadRequestReversal;
  ResRev: TDPPayLoadResponseReversal;
  Payload: String;
begin
  WriteLog('TDPAPI.RequestReversal( '+AJSon+' )');
  Result := '';

  fInTransaction := True;
  ConnectSerial;
  try
    ConfigureSerial;

    ReqRev := TDPPayLoadRequestReversal.Create;
    try
      ReqRev.AsJSON := AJSon;
      if (ReqRev.nsu = '') then
        raise EDPException.Create(ACBrStr(Format(SErrDataInvalid, ['nsu'])) );

      Payload := ReqRev.AsJSON;
    finally
      ReqRev.Free;
    end;

    if SendData( Payload ) then
      Payload := ReadSerialResponse();

    ResRev := TDPPayLoadResponseReversal.Create;
    try
      ResRev.AsJSON := Payload;
      Result := ResRev.AsJSON;
    finally
       ResRev.Free;
    end;
  finally
    DisconnectSerial;
    fInTransaction := False;
  end;
end;

procedure TDPAPI.AbortTransaction;
begin
  fInTransaction := False;
end;

procedure TDPAPI.WriteLog(const AString: AnsiString; Traduz: Boolean);
Var
  Tratado: Boolean;
  AStringLog: AnsiString;
begin
  if not Assigned(fOnWriteLog) then
    Exit;

  if Traduz then
    AStringLog := TranslateUnprintable(AString)
  else
    AStringLog := AString;

  Tratado := False;
  fOnWriteLog(AStringLog, Tratado);
end;

procedure TDPAPI.SetParity(AValue: Char);
begin
  if (fParity = AValue) then
    Exit;

  if not CharInSet(AValue, ['O', 'E', 'M', 'S', 'N']) then
    raise EDPException.Create(ACBrStr(SErrParity));

  fParity := AValue;
end;

procedure TDPAPI.SetPort(AValue: String);
begin
  if (fPort = AValue) then
    Exit;

  fPort := Trim(AValue);
end;

procedure TDPAPI.SetStop(AValue: Integer);
begin
  if (fStop = AValue) then
    Exit;

  if (AValue < 0) and (AValue > 2) then
    raise EDPException.Create(ACBrStr(SErrStop));

  fStop := AValue;
end;

end.

