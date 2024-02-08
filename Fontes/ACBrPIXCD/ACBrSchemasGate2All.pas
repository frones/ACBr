{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
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

(*
  Documentação
  https://devpaygo.readme.io/v5/docs/pix
*)

{$I ACBr.inc}

unit ACBrSchemasGate2All;

interface

uses
  Classes, SysUtils,
  ACBrPIXBase, ACBrJSON, ACBrUtil.FilesIO, ACBrUtil.Strings;

type  

  TGate2AllTransactionStatus = (
    gtsNone,
    gtsTransacaoIniciada,
    gtsAguardandoPagamento,
    gtsEmAnalise,
    gtsExpirada,
    gtsAutorizada,
    gtsConfirmada,
    gtsNegada,
    gtsCancelamentoEmAndamento,
    gtsCancelada,
    gtsPendenteDeConfirmacao,
    gtsFalhaNaComunicacao,
    gtsIntencaoCancelada
  );

  TGate2AllPixProvider = (
    gppNone,
    gppC6Bank,
    gppItau
  );

  TGate2AllPixKey = (
    gpkNone,
    gpkRandomKey,
    gpkEmail,
    gpkDocument,
    gpkPhone
  ); 

  TGate2AllIntegrationType = (
    gitNone,
    gitIntegrado,
    gitLoja,
    gitDireto
  );

  { TGate2AllPix }

  TGate2AllPix = class(TACBrPIXSchema)
  private
    fexpirationDateTime: TDateTime;
    fkey: TGate2AllPixKey;
    FpaymentAmount: Double;
    FpaymentDate: TDateTime;
    fprovider: TGate2AllPixProvider;
    FqrCode: String;
    Furl: String;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TGate2AllPix);

    property provider: TGate2AllPixProvider read fprovider write fprovider;
    property qrCode: String read FqrCode write FqrCode;
    property key: TGate2AllPixKey read fkey write fkey;
    property expirationDateTime: TDateTime read fexpirationDateTime write fexpirationDateTime;
    property url: String read Furl write Furl;
    property paymentDate: TDateTime read FpaymentDate write FpaymentDate;
    property paymentAmount: Double read FpaymentAmount write FpaymentAmount;
  end;

  { TGate2AllPayment }

  TGate2AllPayment = class(TACBrPIXSchema)
  private
    fpix: TGate2AllPix;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TGate2AllPayment);

    property pix: TGate2AllPix read fpix write fpix;
  end;

  { TGate2AllTransaction }

  TGate2AllTransaction = class(TACBrPIXSchema)
  private
    famount: Double;
    fdescription: String;
    fpayment: TGate2AllPayment;
    fpostBackUrl: String;
    freferenceId: String;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TGate2AllTransaction);

    property referenceId: String read freferenceId write freferenceId;
    property amount: Double read famount write famount;
    property description: String read fdescription write fdescription;
    property postBackUrl: String read fpostBackUrl write fpostBackUrl;
    property payment: TGate2AllPayment read fpayment write fpayment;
  end;

  {TGate2AllTransactionResponse}

  TGate2AllTransactionResponse = class(TGate2AllTransaction)
  private
    FallowsManualRetry: Boolean;
    FdtTransaction: TDateTime;
    FintegrationType: TGate2AllIntegrationType;
    Fstatus: TGate2AllTransactionStatus;
    FtransactionId: String;

  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String = ''); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TGate2AllTransactionResponse);

    property transactionId: String read FtransactionId write FtransactionId;
    property integrationType: TGate2AllIntegrationType read FintegrationType write FintegrationType;
    property status: TGate2AllTransactionStatus read Fstatus write Fstatus;
    property dtTransaction: TDateTime read FdtTransaction write FdtTransaction;
    property allowsManualRetry: Boolean read FallowsManualRetry write FallowsManualRetry;
  end;

  { TGate2AllResponseError }

  TGate2AllResponseError = class(TACBrPIXSchema)
  private
    ferror: String;
    fpath: String;
    fstatus: Integer;
    ftimestamp: TDateTime;
  protected
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String = ''); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TGate2AllResponseError);

    property timestamp: TDateTime read ftimestamp write ftimestamp;
    property status: Integer read fstatus write fstatus;
    property error: String read ferror write ferror;
    property path: String read fpath write fpath;
  end;


  function Gate2AllPixProviderToString(aPixProvider: TGate2AllPixProvider): String;
  function StringToGate2AllPixProvider(const aString: String): TGate2AllPixProvider;

  function Gate2AllPixKeyToString(aPixKey: TGate2AllPixKey): String;
  function StringToGate2AllPixKey(const aString: String): TGate2AllPixKey;

  function Gate2AllTransactionStatusToInteger(aStatus: TGate2AllTransactionStatus): Integer;
  function IntegerToGate2AllTransactionStatus(const aStatus: Integer): TGate2AllTransactionStatus;

  function Gate2AllIntegrationTypeToInteger(aType: TGate2AllIntegrationType): Integer;
  function IntegerToGate2AllIntegrationType(aType: Integer): TGate2AllIntegrationType;

implementation

uses
  synautil,
  ACBrUtil.Base, ACBrUtil.DateTime;

function Gate2AllPixProviderToString(aPixProvider: TGate2AllPixProvider): String;
begin
  case aPixProvider of
    gppC6Bank: Result := 'C6BANK';
    gppItau: Result := 'ITAU';
  else
    Result := EmptyStr;
  end;
end;

function StringToGate2AllPixProvider(const aString: String): TGate2AllPixProvider;
var
  s: String;
begin
  s := UpperCase(Trim(aString));
  if (s = 'C6BANK') then
    Result := gppC6Bank
  else if (s = 'ITAU') then
    Result := gppItau
  else
    Result := gppNone;
end;

function Gate2AllPixKeyToString(aPixKey: TGate2AllPixKey): String;
begin
  case aPixKey of
    gpkRandomKey: Result := 'RANDOM_KEY';
    gpkEmail: Result := 'EMAIL';
    gpkDocument: Result := 'DOCUMENT';
    gpkPhone: Result := 'PHONE';
  else
    Result := EmptyStr;
  end;
end;

function StringToGate2AllPixKey(const aString: String): TGate2AllPixKey;
var
  s: String;
begin
  s := UpperCase(Trim(aString));
  if (s = 'RANDOM_KEY') then
    Result := gpkRandomKey
  else if (s = 'EMAIL') then
    Result := gpkEmail
  else if (s = 'DOCUMENT') then
    Result := gpkDocument
  else if (s = 'PHONE') then
    Result := gpkPhone
  else
    Result := gpkNone;
end;

function Gate2AllTransactionStatusToInteger(aStatus: TGate2AllTransactionStatus): Integer;
begin 
  case aStatus of
    gtsTransacaoIniciada: Result := 0;
    gtsAguardandoPagamento: Result := 1;
    gtsEmAnalise: Result := 3;
    gtsExpirada: Result := 4;
    gtsAutorizada: Result := 5;
    gtsConfirmada: Result := 6;
    gtsNegada: Result := 7;
    gtsCancelamentoEmAndamento: Result := 8;
    gtsCancelada: Result := 9;
    gtsPendenteDeConfirmacao: Result := 10;
    gtsFalhaNaComunicacao: Result := 11;
    gtsIntencaoCancelada: Result := 12;
  else
    Result := -1;
  end;
end;

function IntegerToGate2AllTransactionStatus(const aStatus: Integer): TGate2AllTransactionStatus;
begin
  if (aStatus = 0) then
    Result := gtsTransacaoIniciada
  else if (aStatus = 1) then
    Result := gtsAguardandoPagamento
  else if (aStatus = 3) then
    Result := gtsEmAnalise
  else if (aStatus = 4) then
    Result := gtsExpirada
  else if (aStatus = 5) then
    Result := gtsAutorizada
  else if (aStatus = 6) then
    Result := gtsConfirmada
  else if (aStatus = 7) then
    Result := gtsNegada
  else if (aStatus = 8) then
    Result := gtsCancelamentoEmAndamento
  else if (aStatus = 9) then
    Result := gtsCancelada
  else if (aStatus = 10) then
    Result := gtsPendenteDeConfirmacao
  else if (aStatus = 11) then
    Result := gtsFalhaNaComunicacao
  else if (aStatus = 12) then
    Result := gtsIntencaoCancelada
  else
    Result := gtsNone;
end;

function Gate2AllIntegrationTypeToInteger(aType: TGate2AllIntegrationType): Integer;
begin
  case aType of
    gitIntegrado: Result := 1;
    gitLoja: Result := 2;
    gitDireto: Result := 3;
  else
    Result := -1;
  end;
end;

function IntegerToGate2AllIntegrationType(aType: Integer): TGate2AllIntegrationType;
begin
  if (aType = 1) then
    Result := gitIntegrado
  else if (aType = 2) then
    Result := gitLoja
  else if (aType = 3) then
    Result := gitDireto
  else
    Result := gitNone;
end;

{ TGate2AllResponseError }

procedure TGate2AllResponseError.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('timestamp', DateTimeToIso8601(ftimestamp))
    .AddPair('status', fstatus)
    .AddPair('error', ferror)
    .AddPair('path', fpath);
end;

procedure TGate2AllResponseError.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  s: String;
begin
  {$IFDEF FPC}s := EmptyStr;{$ENDIF}
  AJSon
    .Value('timestamp', s)
    .Value('status', fstatus)
    .Value('path', fpath);
  if NaoEstaVazio(s) then
    ftimestamp := Iso8601ToDateTime(s);

  if aJSon.IsJSONObject('error') then
    aJSon
      .AsJSONObject['error']
        .Value('message', ferror)
  else
    aJSon.Value('error', ferror);
end;

constructor TGate2AllResponseError.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  Clear;
end;

procedure TGate2AllResponseError.Clear;
begin
  ftimestamp := 0;
  fstatus := 0;
  ferror := EmptyStr;
  fpath := EmptyStr;
end;

function TGate2AllResponseError.IsEmpty: Boolean;
begin
  Result := EstaZerado(ftimestamp) and EstaZerado(fstatus) and
            EstaVazio(ferror) and EstaVazio(fpath);
end;

procedure TGate2AllResponseError.Assign(aSource: TGate2AllResponseError);
begin
  ftimestamp := aSource.timestamp;
  fstatus := aSource.status;
  ferror := aSource.error;
  fpath := aSource.path;
end;

{ TGate2AllTransactionResponse }

procedure TGate2AllTransactionResponse.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TGate2AllTransactionResponse) then
    Assign(TGate2AllTransactionResponse(aSource));
end;

procedure TGate2AllTransactionResponse.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  inherited DoWriteToJSon(aJSon);
  aJSon
    .AddPair('transactionId', FtransactionId)
    .AddPair('integrationType', Gate2AllIntegrationTypeToInteger(FintegrationType))
    .AddPair('status', Gate2AllTransactionStatusToInteger(Fstatus))
    .AddPair('dtTransaction', DateTimeToIso8601(FdtTransaction))
    .AddPair('allowsManualRetry', FallowsManualRetry);
end;

procedure TGate2AllTransactionResponse.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  s: String;
  i1, i2: Integer;
begin
  {$IFDEF FPC}
  s := EmptyStr;
  i1 := 0;
  i2 := 0;
  {$ENDIF}

  inherited DoReadFromJSon(aJSon);
  aJSon
    .Value('transactionId', FtransactionId)
    .Value('integrationType', i1)
    .Value('status', i2)
    .Value('dtTransaction', s)
    .Value('allowsManualRetry', FallowsManualRetry);

  if NaoEstaVazio(s) then
    FdtTransaction := Iso8601ToDateTime(s);

  Fstatus := IntegerToGate2AllTransactionStatus(i2);
  FintegrationType := IntegerToGate2AllIntegrationType(i1);
end;

constructor TGate2AllTransactionResponse.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  Clear;
end;

procedure TGate2AllTransactionResponse.Clear;
begin
  inherited Clear;
  Fstatus := gtsNone;
  FdtTransaction := 0;
  FintegrationType := gitNone;
  FtransactionId := EmptyStr;
  FallowsManualRetry := False;
end;

function TGate2AllTransactionResponse.IsEmpty: Boolean;
begin
  Result := inherited IsEmpty and (not FallowsManualRetry) and
              (Fstatus = gtsNone) and
              (FintegrationType = gitNone) and
              EstaZerado(FdtTransaction) and
              EstaVazio(FtransactionId);
end;

procedure TGate2AllTransactionResponse.Assign(aSource: TGate2AllTransactionResponse);
begin
  inherited Assign(aSource);
  Fstatus := aSource.status;
  FdtTransaction := aSource.dtTransaction;
  FtransactionId := aSource.transactionId;
  FintegrationType := aSource.integrationType;
  FallowsManualRetry := aSource.allowsManualRetry;
end;

{ TGate2AllTransaction }

procedure TGate2AllTransaction.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TGate2AllTransaction) then
    Assign(TGate2AllTransaction(aSource));
end;

procedure TGate2AllTransaction.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('amount', famount)
    .AddPair('description', fdescription)
    .AddPair('postBackUrl', fpostBackUrl)
    .AddPair('referenceId', freferenceId);

  fpayment.WriteToJSon(aJSon);
end;

procedure TGate2AllTransaction.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('amount', famount)
    .Value('description', fdescription)
    .Value('postBackUrl', fpostBackUrl)
    .Value('referenceId', freferenceId);

  fpayment.ReadFromJSon(aJSon);
end;

constructor TGate2AllTransaction.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  fpayment := TGate2AllPayment.Create('payment');
  Clear;
end;

destructor TGate2AllTransaction.Destroy;
begin
  fpayment.Free;
  inherited Destroy;
end;

procedure TGate2AllTransaction.Clear;
begin
  famount := 0;
  fdescription := EmptyStr;
  fpostBackUrl := EmptyStr;
  freferenceId := EmptyStr;
  fpayment.Clear;
end;

function TGate2AllTransaction.IsEmpty: Boolean;
begin
  Result := (famount = 0) and
            (fdescription = EmptyStr) and
            (fpostBackUrl = EmptyStr) and
            (freferenceId = EmptyStr) and
            fpayment.IsEmpty;
end;

procedure TGate2AllTransaction.Assign(aSource: TGate2AllTransaction);
begin
  famount := aSource.amount;
  fdescription := aSource.description;
  fpostBackUrl := aSource.postBackUrl;
  freferenceId := aSource.referenceId;
  fpayment.Assign(aSource.payment);
end;

{ TGate2AllPayment }

procedure TGate2AllPayment.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TGate2AllPayment) then
    Assign(TGate2AllPayment(aSource));
end;

procedure TGate2AllPayment.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  fpix.WriteToJSon(aJSon);
end;

procedure TGate2AllPayment.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  fpix.ReadFromJSon(aJSon);
end;

constructor TGate2AllPayment.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  fpix := TGate2AllPix.Create('pix');
  Clear;
end;

destructor TGate2AllPayment.Destroy;
begin
  fpix.Free;
  inherited Destroy;
end;

procedure TGate2AllPayment.Clear;
begin
  fpix.Clear;
end;

function TGate2AllPayment.IsEmpty: Boolean;
begin
  Result := fpix.IsEmpty;
end;

procedure TGate2AllPayment.Assign(aSource: TGate2AllPayment);
begin
  fpix.Assign(aSource.pix);
end;

{ TGate2AllPix }

procedure TGate2AllPix.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (ASource is TGate2AllPix) then
    Assign(TGate2AllPix(ASource));
end;

procedure TGate2AllPix.DoWriteToJSon(aJSon: TACBrJSONObject);
var
  ja: TACBrJSONArray;
begin
  aJSon
    .AddPair('provider', Gate2AllPixProviderToString(fprovider))
    .AddPair('qrCode', FqrCode, False)
    .AddPair('expirationDateTime', DateTimeToIso8601(fexpirationDateTime))
    .AddPair('paymentAmount', FpaymentAmount, False)
    .AddPair('url', Furl, False);

  if (FpaymentDate > 0) then
    aJSon.AddPair('paymentDate', DateTimeToIso8601(FpaymentDate));

  ja := TACBrJSONArray.Create;
  try
    ja.AddElement(Gate2AllPixKeyToString(fkey));
    AJSon.AddPair('key', ja);
  except
    ja.Free;
    raise;
  end;
end;

procedure TGate2AllPix.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  s1, s2, s3, s4: String;
begin
  {$IFDEF FPC}
  s1 := EmptyStr;
  s2 := EmptyStr;
  s3 := EmptyStr;
  s4 := EmptyStr;
  {$ENDIF}

  aJSon
    .Value('provider', s1)
    .Value('qrCode', FqrCode)
    .Value('expirationDateTime', s2)
    .Value('paymentDate', s3)
    .Value('paymentAmount', FpaymentAmount)
    .Value('url', Furl);

  fprovider := StringToGate2AllPixProvider(s1);

  if NaoEstaVazio(s2) then
    fexpirationDateTime := Iso8601ToDateTime(s2);

  if NaoEstaVazio(s3) then
    FpaymentDate := Iso8601ToDateTime(s3);

  if aJSon.IsJSONArray('key') and (aJSon.AsJSONArray['key'].Count > 0) then
    s4 := aJSon.AsJSONArray['key'].Items[0];
  fkey := StringToGate2AllPixKey(s4);
end;

constructor TGate2AllPix.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  Clear;
end;

procedure TGate2AllPix.Clear;
begin
  fprovider := gppNone;
  FqrCode := EmptyStr;
  fkey := gpkNone;
  fexpirationDateTime := 0;
  Furl := EmptyStr;
  FpaymentDate := 0;
  FpaymentAmount := 0;
end;

function TGate2AllPix.IsEmpty: Boolean;
begin
  Result := (fprovider = gppNone) and EstaVazio(FqrCode) and (fkey = gpkNone) and
            EstaZerado(fexpirationDateTime) and EstaVazio(Furl) and
            EstaZerado(FpaymentDate) and EstaZerado(FpaymentAmount);
end;

procedure TGate2AllPix.Assign(aSource: TGate2AllPix);
begin
  fprovider := aSource.provider;
  FqrCode := aSource.qrCode;
  fkey := aSource.key;
  fexpirationDateTime := aSource.expirationDateTime;
  FpaymentDate := aSource.paymentDate;
  FpaymentAmount := aSource.paymentAmount;
  Furl := aSource.url;
end;

end.

