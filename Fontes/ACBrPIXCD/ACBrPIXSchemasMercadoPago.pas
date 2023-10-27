unit ACBrPIXSchemasMercadoPago;

interface

uses
  Classes, SysUtils,
  ACBrPIXBase, ACBrJSON;

type
  TMercadoPagoPaymentStatus = (
    mpsNone,
    mpsPending,
    mpsApproved,
    mpsAuthorized,
    mpsInProcess,
    mpsInMediation,
    mpsRejected,
    mpsCancelled,
    mpsRefunded,
    mpsChargedBack
  );

  TMercadoPagoPayerEntityType = (
    metNone,
    metIndividual,
    metAssociation
  );

  TMercadoPagoPayerType = (
    mptNone,
    mptCustomer,
    mptGuest
  );

  TMercadoPagoPayerIdentificationType = (
    mitNone,
    mitCPF,
    mitCNPJ,
    mitCUIT,
    mitCUIL,
    mitDNI,
    mitCURP,
    mitRFC,
    mitCC,
    mitRUT,
    mitCI
  );

  TMercadoPagoRefundSourceType = (
    mrtNone,
    mrtAdmin,
    mrtCollector,
    mrtBPP,
    mrtMarketplace
  );

  TMercadoPagoRefundStatus = (
    mrsNone,
    mrsApproved,
    mrsInProcess,
    mrsRejected,
    mrsCancelled,
    mrsAuthorized
  );

  { TMercadoPagoErrorCause }

  TMercadoPagoErrorCause = class(TACBrPIXSchema)
  private
    Fcode: Integer;
    Fdata: string;
    Fdescription: string;
  protected
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: string = ''); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMercadoPagoErrorCause);

    property code: Integer read Fcode write Fcode;
    property description: string read Fdescription write Fdescription;
    property data: string read Fdata write Fdata;
  end;

  { TMercadoPagoErrorCauseArray }

  TMercadoPagoErrorCauseArray = class(TACBrPIXSchemaArray)
  private
    function GetItem(Index: Integer): TMercadoPagoErrorCause;
    procedure SetItem(Index: Integer; Value: TMercadoPagoErrorCause);
  protected
    function NewSchema: TACBrPIXSchema; override;
  public
    Function Add(aErrorCause: TMercadoPagoErrorCause): Integer;
    Procedure Insert(Index: Integer; aErrorCause: TMercadoPagoErrorCause);
    function New: TMercadoPagoErrorCause;
    property Items[Index: Integer]: TMercadoPagoErrorCause read GetItem write SetItem; default;
  end;

  { TMercadoPagoError }

  TMercadoPagoError = class(TACBrPIXSchema)
  private
    Fcause: TMercadoPagoErrorCauseArray;
    Ferror: string;
    Fmessage: string;
    Fstatus: Integer;
  protected
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: string = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMercadoPagoError);
                                 
    property message: string read Fmessage write Fmessage;
    property error: string read Ferror write Ferror;
    property status: Integer read Fstatus write Fstatus;
    property cause: TMercadoPagoErrorCauseArray read Fcause write Fcause;
  end;

  { TMercadoPagoPayerIdentification }

  TMercadoPagoPayerIdentification = class(TACBrPIXSchema)
    private
      FTypePayer: String;
      FNumber: String;
    protected
      procedure WriteToJSon(aJSon: TACBrJSONObject); reintroduce;
      procedure ReadFromJSon(aJSon: TACBrJSONObject);  reintroduce;
    public
      procedure Clear; override;
      function IsEmpty: Boolean; override;
      procedure Assign(aSource: TMercadoPagoPayerIdentification);

      property TypePayer: string read FTypePayer write FTypePayer;
      property Number: string read FNumber write FNumber;
  end;

  { TMercadoPagoPayer }

  TMercadoPagoPayer = class(TACBrPIXSchema)
    private
      Femail: String;
      FfirstName: String;
      FlastName: String;
      FpayerIndentification: TMercadoPagoPayerIdentification;
    protected
      procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
      procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
    public
      constructor Create(const aObjectName: string = ''); override;
      destructor Destroy; override;
      procedure Clear; override;
      function IsEmpty: Boolean; override;
      procedure Assign(aSource: TMercadoPagoPayer);

      property email: string read Femail write Femail;
      property firstName: string read FfirstName write FfirstName;
      property lastName: string read FlastName write FlastName;

      property PayerIndentification: TMercadoPagoPayerIdentification read FPayerIndentification write FPayerIndentification;
  end;

  { TMercadoPagoPaymentPayerIdentification }

  TMercadoPagoPaymentPayerIdentification = class(TACBrPIXSchema)
    private
      Ftype: TMercadoPagoPayerIdentificationType;
      Fnumber: string;
    protected
      procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
      procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
    public
      constructor Create(const ObjectName: String = ''); override;
      procedure Clear; override;
      function IsEmpty: Boolean; override;
      procedure Assign(aSource: TMercadoPagoPaymentPayerIdentification);

      property type_: TMercadoPagoPayerIdentificationType read Ftype write Ftype;
      property number: string read Fnumber write Fnumber;
  end;

  { TMercadoPagoPaymentPayer }

  TMercadoPagoPaymentPayer = class(TACBrPIXSchema)
  private
    Femail: String;
    Fentity_type: TMercadoPagoPayerEntityType;
    FfirstName: String;
    Fid: String;
    Fidentification: TMercadoPagoPaymentPayerIdentification;
    FlastName: String;
    Ftype: TMercadoPagoPayerType;
  protected
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: string = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMercadoPagoPaymentPayer);

    property entity_type: TMercadoPagoPayerEntityType read Fentity_type write Fentity_type;
    property type_: TMercadoPagoPayerType read Ftype write Ftype;
    property id: String read Fid write Fid;
    property email: String read Femail write Femail;
    property lastName: String read FlastName write FlastName;
    property firstName: String read FfirstName write FfirstName;
    property identification: TMercadoPagoPaymentPayerIdentification read Fidentification write Fidentification;
  end;

  { TMercadoPagoPaymentRequest }

  TMercadoPagoPaymentRequest = class(TACBrPIXSchema)
  private
    FtransactionAmount: Currency;
    Fdescription: String;
    FdateExpiration: String;
    FauthorizationCode: String;
    fpaymentMethodID: String;
    Fpayer: TMercadoPagoPaymentPayer;
    function GetPaymentMethodID: String;
  protected
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: string = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMercadoPagoPaymentRequest);

    property transactionAmount: Currency read FtransactionAmount write FtransactionAmount;
    property description: String read Fdescription write Fdescription;
    property paymentMethodID: String read GetPaymentMethodID;
    property dateExpiration: String read FdateExpiration write FdateExpiration;
    property authorizationCode: String read FauthorizationCode write FauthorizationCode;
    property payer: TMercadoPagoPaymentPayer read Fpayer;
  end;

  { TMercadoPagoPaymentTransactionData }

  TMercadoPagoPaymentTransactionData = class(TACBrPIXSchema)
  private
    Fqrcode: string;
    FqrcodeBase64: string;
    FticketURL: string;
  protected
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMercadoPagoPaymentTransactionData);

    property qrcode: string read Fqrcode write Fqrcode;
    property qrcodeBase64: string read FqrcodeBase64 write FqrcodeBase64;
    property ticketURL: string read FticketURL write FticketURL;
  end;

  { TMercadoPagoPointOfInteraction }

  TMercadoPagoPointOfInteraction = class(TACBrPIXSchema)
  private
    FtransactionData: TMercadoPagoPaymentTransactionData;
    Ftype: string;
  protected
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMercadoPagoPointOfInteraction);

    property type_: string read Ftype write Ftype;
    property transactionData: TMercadoPagoPaymentTransactionData read FtransactionData write FtransactionData;
  end;

  TMercadoPagoRefundArray = class;

  { TMercadoPagoPayment }

  TMercadoPagoPayment = class(TACBrPIXSchema)
  private
    FcurrencyId: string;
    FdateApproved: TDateTime;
    FdateCreated: TDateTime;
    FdateExpiration: TDateTime;
    FdateLastUpdate: TDateTime;
    Fid: string;
    Fpayer: TMercadoPagoPaymentPayer;
    FpaymentMethodID: string;
    FpointOfInteraction: TMercadoPagoPointOfInteraction;
    Frefunds: TMercadoPagoRefundArray;
    Fstatus: TMercadoPagoPaymentStatus;
    FstatusDetail: string;
    FtransactionAmount: Currency;
    FtransactionAmountRefunded: Currency;
  protected
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMercadoPagoPayment);

    property id: string read Fid write Fid;
    property dateCreated: TDateTime read FdateCreated write FdateCreated;
    property dateApproved: TDateTime read FdateApproved write FdateApproved;
    property dateLastUpdate: TDateTime read FdateLastUpdate write FdateLastUpdate;
    property dateExpiration: TDateTime read FdateExpiration write FdateExpiration;
    property status: TMercadoPagoPaymentStatus read Fstatus write Fstatus;
    property statusDetail: string read FstatusDetail write FstatusDetail;
    property currencyId: string read FcurrencyId write FcurrencyId;
    property transactionAmount: Currency read FtransactionAmount write FtransactionAmount;
    property transactionAmountRefunded: Currency read FtransactionAmountRefunded write FtransactionAmountRefunded;
    property payer: TMercadoPagoPaymentPayer read Fpayer write Fpayer;
    property paymentMethodID: string read FpaymentMethodID write FpaymentMethodID;
    property pointOfInteraction: TMercadoPagoPointOfInteraction read FpointOfInteraction write FpointOfInteraction;
    property refunds: TMercadoPagoRefundArray read Frefunds write Frefunds;
  end;

  { TMercadoPagoPaymentArray }

  TMercadoPagoPaymentArray = class(TACBrPIXSchemaArray)
  private
    function GetItem(aIndex: Integer): TMercadoPagoPayment;
    procedure SetItem(aIndex: Integer; aValue: TMercadoPagoPayment);
  protected
    function NewSchema: TACBrPIXSchema; override;
  public
    Function Add(aPayment: TMercadoPagoPayment): Integer;
    Procedure Insert(aIndex: Integer; aPayment: TMercadoPagoPayment);
    function New: TMercadoPagoPayment;
    property Items[aIndex: Integer]: TMercadoPagoPayment read GetItem write SetItem; default;
  end; 

  { TMercadoPagoConsultedPaymentsPaging }

  TMercadoPagoConsultedPaymentsPaging = class(TACBrPIXSchema)
  private
    Flimit: Integer;
    Foffset: Integer;
    Ftotal: Integer;
  protected
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMercadoPagoConsultedPaymentsPaging);

    property total: Integer read Ftotal write Ftotal;
    property limit: Integer read Flimit write Flimit;
    property offset: Integer read Foffset write Foffset;
  end;

  { TMercadoPagoConsultedPayments }

  TMercadoPagoConsultedPayments = class(TACBrPIXSchema)
  private
    Fpaging: TMercadoPagoConsultedPaymentsPaging;
    Fresults: TMercadoPagoPaymentArray;
  protected
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMercadoPagoConsultedPayments);

    property paging: TMercadoPagoConsultedPaymentsPaging read Fpaging write Fpaging;
    property results: TMercadoPagoPaymentArray read Fresults write Fresults;
  end;

  { TMercadoPagoRefundRequest }

  TMercadoPagoRefundRequest = class(TACBrPIXSchema)
  private
    Famount: Currency;
  protected
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String = ''); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMercadoPagoRefundRequest);

    property amount: Currency read Famount write Famount;
  end;


  { TMercadoPagoRefundSource }

  TMercadoPagoRefundSource = class(TACBrPIXSchema)
  private
    Fid: string;
    Fname: string;
    Ftype: TMercadoPagoRefundSourceType;
  protected
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMercadoPagoRefundSource);
                                           
    property id: string read Fid write Fid;
    property name: string read Fname write Fname;
    property type_: TMercadoPagoRefundSourceType read Ftype write Ftype;
  end;

  { TMercadoPagoRefund }

  TMercadoPagoRefund = class(TACBrPIXSchema)
  private
    FadjustmentAmount: Currency;
    Famount: Currency;
    FdataCreated: TDateTime;
    Fid: Integer;
    FpaymentId: Integer;
    Freason: string;
    FrefundMode: string;
    Fsource: TMercadoPagoRefundSource;
    Fstatus: TMercadoPagoRefundStatus;
    FuniqueSequenceNumber: string;
  protected
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMercadoPagoRefund);
    
    property id: Integer read Fid write Fid;
    property paymentId: Integer read FpaymentId write FpaymentId;
    property amount: Currency read Famount write Famount;
    property source: TMercadoPagoRefundSource read Fsource write Fsource;
    property dataCreated: TDateTime read FdataCreated write FdataCreated;
    property uniqueSequenceNumber: string read FuniqueSequenceNumber write FuniqueSequenceNumber;
    property refundMode: string read FrefundMode write FrefundMode;
    property adjustmentAmount: Currency read FadjustmentAmount write FadjustmentAmount;
    property status: TMercadoPagoRefundStatus read Fstatus write Fstatus;
    property reason: string read Freason write Freason;
  end;

  { TMercadoPagoRefundArray }

  TMercadoPagoRefundArray = class(TACBrPIXSchemaArray)
  private
    function GetItem(aIndex: Integer): TMercadoPagoRefund;
    procedure SetItem(aIndex: Integer; aValue: TMercadoPagoRefund);
  protected
    function NewSchema: TACBrPIXSchema; override;
  public
    Function Add(aRefund: TMercadoPagoRefund): Integer;
    Procedure Insert(aIndex: Integer; aRefund: TMercadoPagoRefund);
    function New: TMercadoPagoRefund;
    property Items[aIndex: Integer]: TMercadoPagoRefund read GetItem write SetItem; default;
  end;


  { TMercadoPago }

  //TMercadoPago = class(TACBrPIXSchema)
  //private
  //protected
  //  procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
  //  procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  //public
  //  constructor Create(const aObjectName: String); override;
  //  procedure Clear; override;
  //  function IsEmpty: Boolean; override;
  //  procedure Assign(aSource: TMercadoPago);
  //
  //  property a: string read Fa write Fa;
  //end;

  function PaymentStatusToString(aStatus: TMercadoPagoPaymentStatus): String;
  function StringToPaymentStatus(const aString: String): TMercadoPagoPaymentStatus;

  function PayerIdentificationTypeToString(const aType: TMercadoPagoPayerIdentificationType): String;
  function StringToPayerIdentificationType(const aValue: String): TMercadoPagoPayerIdentificationType;

  function PayerEntityTypeToString(const aValue: TMercadoPagoPayerEntityType): String;
  function StringToPayerEntityType(const aValue: String): TMercadoPagoPayerEntityType;

  function PayerTypeToString(const aValue: TMercadoPagoPayerType): String;
  function StringToPayerType(const aValue: String): TMercadoPagoPayerType;

  function RefundSourceTypeToString(const aValue: TMercadoPagoRefundSourceType): String;
  function StringToRefundSourceType(const aValue: String): TMercadoPagoRefundSourceType;

  function RefundStatusToString(const aValue: TMercadoPagoRefundStatus): String;
  function StringToRefundStatus(const aValue: String): TMercadoPagoRefundStatus;

implementation

uses
  synautil, ACBrUtil.Base, ACBrUtil.Strings;

{ TMercadoPagoRefundArray }

function TMercadoPagoRefundArray.GetItem(aIndex: Integer): TMercadoPagoRefund;
begin
  Result := TMercadoPagoRefund(inherited Items[aIndex]);
end;

procedure TMercadoPagoRefundArray.SetItem(aIndex: Integer; aValue: TMercadoPagoRefund);
begin
  inherited Items[aIndex] := aValue;
end;

function TMercadoPagoRefundArray.NewSchema: TACBrPIXSchema;
begin
  Result := New;
end;

function TMercadoPagoRefundArray.Add(aRefund: TMercadoPagoRefund): Integer;
begin
  Result := inherited Add(aRefund);
end;

procedure TMercadoPagoRefundArray.Insert(aIndex: Integer; aRefund: TMercadoPagoRefund);
begin
  inherited Insert(aIndex, aRefund);
end;

function TMercadoPagoRefundArray.New: TMercadoPagoRefund;
begin
  Result := TMercadoPagoRefund.Create;
  Add(Result);
end;

{ TMercadoPagoConsultedPayments }

procedure TMercadoPagoConsultedPayments.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  Fpaging.WriteToJSon(aJSon);
  Fresults.WriteToJSon(aJSon);
end;

procedure TMercadoPagoConsultedPayments.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  Fpaging.ReadFromJSon(aJSon);
  Fresults.ReadFromJSon(aJSon);
end;

constructor TMercadoPagoConsultedPayments.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  Fpaging := TMercadoPagoConsultedPaymentsPaging.Create('paging');
  Fresults := TMercadoPagoPaymentArray.Create('results');
end;

destructor TMercadoPagoConsultedPayments.Destroy;
begin
  Fpaging.Free;
  Fresults.Free;
  inherited Destroy;
end;

procedure TMercadoPagoConsultedPayments.Clear;
begin
  Fpaging.Clear;
  Fresults.Clear;
end;

function TMercadoPagoConsultedPayments.IsEmpty: Boolean;
begin
  Result := Fpaging.IsEmpty and Fresults.IsEmpty;
end;

procedure TMercadoPagoConsultedPayments.Assign(aSource: TMercadoPagoConsultedPayments);
begin
  Fpaging.Assign(aSource.paging);
  Fresults.Assign(aSource.results);
end;

{ TMercadoPagoConsultedPaymentsPaging }

procedure TMercadoPagoConsultedPaymentsPaging.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('total', Ftotal, False)
    .AddPair('limit', Flimit, False)
    .AddPair('offset', Foffset, False);
end;

procedure TMercadoPagoConsultedPaymentsPaging.DoReadFromJSon(
  aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('total', Ftotal)
    .Value('limit', Flimit)
    .Value('offset', Foffset);
end;

constructor TMercadoPagoConsultedPaymentsPaging.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  Clear;
end;

procedure TMercadoPagoConsultedPaymentsPaging.Clear;
begin
  Ftotal := 0;
  Flimit := 0;
  Foffset := 0;
end;

function TMercadoPagoConsultedPaymentsPaging.IsEmpty: Boolean;
begin
  Result := EstaZerado(Ftotal) and EstaZerado(Flimit) and EstaZerado(Foffset);
end;

procedure TMercadoPagoConsultedPaymentsPaging.Assign(aSource: TMercadoPagoConsultedPaymentsPaging);
begin
  Ftotal := aSource.total;
  Flimit := aSource.limit;
  Foffset := aSource.offset;
end;

{ TMercadoPagoPaymentArray }

function TMercadoPagoPaymentArray.GetItem(aIndex: Integer): TMercadoPagoPayment;
begin
  Result := TMercadoPagoPayment(inherited Items[aIndex]);
end;

procedure TMercadoPagoPaymentArray.SetItem(aIndex: Integer; aValue: TMercadoPagoPayment);
begin
  inherited Items[aIndex] := aValue;
end;

function TMercadoPagoPaymentArray.NewSchema: TACBrPIXSchema;
begin
  Result := New;
end;

function TMercadoPagoPaymentArray.Add(aPayment: TMercadoPagoPayment): Integer;
begin
  Result := inherited Add(aPayment);
end;

procedure TMercadoPagoPaymentArray.Insert(aIndex: Integer; aPayment: TMercadoPagoPayment);
begin
  inherited Insert(aIndex, aPayment);
end;

function TMercadoPagoPaymentArray.New: TMercadoPagoPayment;
begin
  Result := TMercadoPagoPayment.Create;
  Add(Result);
end;

{ TMercadoPagoErrorCauseArray }

function TMercadoPagoErrorCauseArray.GetItem(Index: Integer): TMercadoPagoErrorCause;
begin
  Result := TMercadoPagoErrorCause(inherited Items[Index]);
end;

procedure TMercadoPagoErrorCauseArray.SetItem(Index: Integer; Value: TMercadoPagoErrorCause);
begin
  inherited Items[Index] := Value;
end;

function TMercadoPagoErrorCauseArray.NewSchema: TACBrPIXSchema;
begin
  Result := New;
end;

function TMercadoPagoErrorCauseArray.Add(aErrorCause: TMercadoPagoErrorCause): Integer;
begin
  Result := inherited Add(aErrorCause);
end;

procedure TMercadoPagoErrorCauseArray.Insert(Index: Integer;aErrorCause: TMercadoPagoErrorCause);
begin
  inherited Insert(Index, aErrorCause);
end;

function TMercadoPagoErrorCauseArray.New: TMercadoPagoErrorCause;
begin
  Result := TMercadoPagoErrorCause.Create;
  Add(Result);
end;

{ TMercadoPagoRefund }

procedure TMercadoPagoRefund.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('id', Fid)
    .AddPair('payment_id', FpaymentId)
    .AddPair('amount', Famount)
    .AddPair('date_created', FdataCreated)
    .AddPair('unique_sequence_number', FuniqueSequenceNumber)
    .AddPair('refund_mode', FrefundMode)
    .AddPair('adjustment_amount', FadjustmentAmount)
    .AddPair('status', RefundStatusToString(Fstatus))
    .AddPair('reason', Freason);
  Fsource.WriteToJSon(aJSon);
end;

procedure TMercadoPagoRefund.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  s: string;
begin
  {$IFDEF FPC}s := EmptyStr;{$ENDIF}
  aJSon
    .Value('id', Fid)
    .Value('payment_id', FpaymentId)
    .Value('amount', Famount)
    .ValueISODateTime('date_created', FdataCreated)
    .Value('unique_sequence_number', FuniqueSequenceNumber)
    .Value('refund_mode', FrefundMode)
    .Value('adjustment_amount', FadjustmentAmount)
    .Value('status', s)
    .Value('reason', Freason);
  Fstatus := StringToRefundStatus(s);
  Fsource.ReadFromJSon(aJSon);
end;

constructor TMercadoPagoRefund.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  Fsource := TMercadoPagoRefundSource.Create('source');
  Clear;
end;

destructor TMercadoPagoRefund.Destroy;
begin
  Fsource.Free;
  inherited Destroy;
end;

procedure TMercadoPagoRefund.Clear;
begin
  Fid := 0;
  Famount := 0;
  FpaymentId := 0;
  FdataCreated := 0;
  FadjustmentAmount := 0;
  Freason := EmptyStr;
  FrefundMode := EmptyStr;
  FuniqueSequenceNumber := EmptyStr;
  Fstatus := mrsNone;
  Fsource.Clear;
end;

function TMercadoPagoRefund.IsEmpty: Boolean;
begin
  Result :=
    EstaZerado(fid) and
    EstaZerado(Famount) and
    EstaZerado(FpaymentId) and
    EstaZerado(FdataCreated) and
    EstaZerado(FadjustmentAmount) and
    EstaVazio(Freason) and
    EstaVazio(FrefundMode) and
    EstaVazio(FuniqueSequenceNumber) and
    (Fstatus = mrsNone) and
    Fsource.IsEmpty;
end;

procedure TMercadoPagoRefund.Assign(aSource: TMercadoPagoRefund);
begin
  Fid := aSource.id;
  Famount := aSource.amount;
  FpaymentId := aSource.paymentId;
  FdataCreated := aSource.dataCreated;
  FadjustmentAmount := aSource.adjustmentAmount;
  Freason := aSource.reason;
  FrefundMode := aSource.refundMode;
  FuniqueSequenceNumber := aSource.uniqueSequenceNumber;
  Fstatus := aSource.status;
  Fsource.Assign(aSource.source);
end;

{ TMercadoPagoRefundSource }

procedure TMercadoPagoRefundSource.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('id', Fid, False)
    .AddPair('name', Fname, False)
    .AddPair('type', RefundSourceTypeToString(Ftype));
end;

procedure TMercadoPagoRefundSource.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  s: string;
begin
  {$IFDEF FPC}s := EmptyStr;{$ENDIF}
  aJSon
    .Value('id', Fid)
    .Value('name', Fname)
    .Value('type', s);
  Ftype := StringToRefundSourceType(s);
end;

constructor TMercadoPagoRefundSource.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  Clear;
end;

procedure TMercadoPagoRefundSource.Clear;
begin
  Fid := EmptyStr;
  Fname := EmptyStr;
  Ftype := mrtNone;
end;

function TMercadoPagoRefundSource.IsEmpty: Boolean;
begin
  Result := EstaVazio(Fid) and EstaVazio(Fname) and (Ftype = mrtNone);
end;

procedure TMercadoPagoRefundSource.Assign(aSource: TMercadoPagoRefundSource);
begin
  Fid := aSource.id;
  Fname := aSource.name;
  Ftype := aSource.type_;
end;

{ TMercadoPagoRefundRequest }

procedure TMercadoPagoRefundRequest.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon.AddPair('amount', Famount, False);
end;

procedure TMercadoPagoRefundRequest.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon.Value('amount', Famount);
end;

constructor TMercadoPagoRefundRequest.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  Clear;
end;

procedure TMercadoPagoRefundRequest.Clear;
begin
  Famount := 0;
end;

function TMercadoPagoRefundRequest.IsEmpty: Boolean;
begin
  Result := EstaZerado(Famount);
end;

procedure TMercadoPagoRefundRequest.Assign(aSource: TMercadoPagoRefundRequest);
begin
  Famount := aSource.amount;
end;

{ TMercadoPagoError }

procedure TMercadoPagoError.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('message', Fmessage)
    .AddPair('error', Ferror)
    .AddPair('status', Fstatus);
  Fcause.WriteToJSon(aJSon);
end;

procedure TMercadoPagoError.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('message', Fmessage)
    .Value('error', Ferror)
    .Value('status', Fstatus);
  Fcause.ReadFromJSon(aJSon);
end;

constructor TMercadoPagoError.Create(const ObjectName: string);
begin
  inherited Create(ObjectName);
  Fcause := TMercadoPagoErrorCauseArray.Create('cause');
  Clear;
end;

destructor TMercadoPagoError.Destroy;
begin
  Fcause.Free;
  inherited Destroy;
end;

procedure TMercadoPagoError.Clear;
begin
  Fmessage := EmptyStr;
  Ferror := EmptyStr;
  Fstatus := 0;
  Fcause.Clear;
end;

function TMercadoPagoError.IsEmpty: Boolean;
begin
  Result := inherited IsEmpty;
end;

procedure TMercadoPagoError.Assign(aSource: TMercadoPagoError);
begin
  Fmessage := aSource.message;
  Ferror := aSource.error;
  Fstatus := aSource.status;
  Fcause.Assign(aSource.cause);
end;

{ TMercadoPagoErrorCause }

procedure TMercadoPagoErrorCause.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('code', Fcode)
    .AddPair('description', Fdescription)
    .AddPair('data', Fdata);
end;

procedure TMercadoPagoErrorCause.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('code', Fcode)
    .Value('description', Fdescription)
    .Value('data', Fdata);
end;

constructor TMercadoPagoErrorCause.Create(const ObjectName: string);
begin
  inherited Create(ObjectName);
  Clear;
end;

procedure TMercadoPagoErrorCause.Clear;
begin
  Fcode := 0;
  Fdata := EmptyStr;
  Fdescription := EmptyStr;
end;

function TMercadoPagoErrorCause.IsEmpty: Boolean;
begin
  Result := EstaZerado(Fcode) and EstaVazio(Fdata) and EstaVazio(Fdescription);
end;

procedure TMercadoPagoErrorCause.Assign(aSource: TMercadoPagoErrorCause);
begin
  Fcode := aSource.code;
  Fdata := aSource.data;
  Fdescription := aSource.description;
end;

{ TMercadoPagoPayment }

procedure TMercadoPagoPayment.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('id', Fid, False)
    .AddPairISODateTime('date_created', FdateCreated, False)
    .AddPairISODateTime('date_approved', FdateApproved, False)
    .AddPairISODateTime('date_last_update', FdateLastUpdate, False)
    .AddPairISODateTime('date_of_expiration', FdateExpiration, False)
    .AddPair('status', PaymentStatusToString(Fstatus), False)
    .AddPair('status_detail', FstatusDetail, False)
    .AddPair('currency_id', FcurrencyId, False)
    .AddPair('payment_method_id', FpaymentMethodID, False)
    .AddPair('transaction_amount', FtransactionAmount, False)
    .AddPair('transaction_amount_refunded', FtransactionAmountRefunded);
  Fpayer.WriteToJSon(aJSon);
  FpointOfInteraction.WriteToJSon(aJSon);
  Frefunds.WriteToJSon(aJSon);
end;

procedure TMercadoPagoPayment.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  s: string;
begin
  {$IFDEF FPC}s := EmptyStr;{$ENDIF}
  aJSon
    .Value('id', Fid)
    .ValueISODateTime('date_created', FdateCreated)
    .ValueISODateTime('date_approved', FdateApproved)
    .ValueISODateTime('date_last_update', FdateLastUpdate)
    .ValueISODateTime('date_of_expiration', FdateExpiration)
    .Value('status', s)
    .Value('status_detail', FstatusDetail)
    .Value('currency_id', FcurrencyId)
    .Value('payment_method_id', FpaymentMethodID)
    .Value('transaction_amount', FtransactionAmount)
    .Value('transaction_amount_refunded', FtransactionAmountRefunded);
  Fstatus := StringToPaymentStatus(s);
  Fpayer.ReadFromJSon(aJSon);
  FpointOfInteraction.ReadFromJSon(aJSon);
  Frefunds.ReadFromJSon(aJSon);
end;

constructor TMercadoPagoPayment.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  Fpayer := TMercadoPagoPaymentPayer.Create('payer');
  FpointOfInteraction := TMercadoPagoPointOfInteraction.Create('point_of_interaction');
  Frefunds := TMercadoPagoRefundArray.Create('refunds');
  Clear;
end;

destructor TMercadoPagoPayment.Destroy;
begin
  Fpayer.Free;
  FpointOfInteraction.Free;
  Frefunds.Free;
  inherited Destroy;
end;

procedure TMercadoPagoPayment.Clear;
begin
  Fid := EmptyStr;
  FdateCreated := 0;
  FdateApproved := 0;
  FdateExpiration := 0;
  FdateLastUpdate := 0;
  FtransactionAmount := 0;
  FtransactionAmountRefunded := 0;
  Fstatus := mpsNone;
  FpaymentMethodID := EmptyStr;
  FstatusDetail := EmptyStr;
  FcurrencyId := EmptyStr;
  Fpayer.Clear;
  Frefunds.Clear;
  FpointOfInteraction.Clear;
end;

function TMercadoPagoPayment.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(Fid) and
    EstaVazio(FstatusDetail) and
    EstaVazio(FcurrencyId) and
    EstaVazio(FpaymentMethodID) and
    EstaZerado(FdateApproved) and
    EstaZerado(FdateCreated) and
    EstaZerado(FdateExpiration) and
    EstaZerado(FdateLastUpdate) and
    EstaZerado(FtransactionAmount) and
    EstaZerado(FtransactionAmountRefunded) and
    (Fstatus = mpsNone) and
    Fpayer.IsEmpty and
    Frefunds.IsEmpty and
    FpointOfInteraction.IsEmpty;
end;

procedure TMercadoPagoPayment.Assign(aSource: TMercadoPagoPayment);
begin
  Fid := aSource.id;
  FdateCreated := aSource.dateCreated;
  FdateApproved := aSource.dateApproved;
  FdateExpiration := aSource.dateExpiration;
  FdateLastUpdate := aSource.dateLastUpdate;
  Fstatus := aSource.status;
  FstatusDetail := aSource.statusDetail;
  FcurrencyId := aSource.currencyId;
  FtransactionAmount := aSource.transactionAmount;
  FtransactionAmountRefunded := aSource.transactionAmountRefunded;
  FpaymentMethodID := aSource.paymentMethodID;
  Fpayer.Assign(aSource.payer);
  FpointOfInteraction.Assign(aSource.pointOfInteraction);
  Frefunds.Assign(aSource.refunds);
end;

{ TMercadoPagoPointOfInteraction }

procedure TMercadoPagoPointOfInteraction.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon.AddPair('type', Ftype);
  FtransactionData.WriteToJSon(aJSon);
end;

procedure TMercadoPagoPointOfInteraction.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon.Value('type', Ftype);
  FtransactionData.ReadFromJSon(aJSon);
end;

constructor TMercadoPagoPointOfInteraction.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  FtransactionData := TMercadoPagoPaymentTransactionData.Create('transaction_data');
  Clear;
end;

destructor TMercadoPagoPointOfInteraction.Destroy;
begin
  FtransactionData.Free;
  inherited Destroy;
end;

procedure TMercadoPagoPointOfInteraction.Clear;
begin
  Ftype := EmptyStr;
  FtransactionData.Clear;
end;

function TMercadoPagoPointOfInteraction.IsEmpty: Boolean;
begin
  Result := EstaVazio(Ftype) and FtransactionData.IsEmpty;
end;

procedure TMercadoPagoPointOfInteraction.Assign(aSource: TMercadoPagoPointOfInteraction);
begin
  Ftype := aSource.type_;
  FtransactionData.Assign(aSource.transactionData);
end;

{ TMercadoPagoPaymentTransactionData }

procedure TMercadoPagoPaymentTransactionData.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('qr_code', Fqrcode, False)
    .AddPair('qr_code_base64', FqrcodeBase64, False)
    .AddPair('ticket_url', FticketURL, False);
end;

procedure TMercadoPagoPaymentTransactionData.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('qr_code', Fqrcode)
    .Value('qr_code_base64', FqrcodeBase64)
    .Value('ticket_url', FticketURL);
end;

constructor TMercadoPagoPaymentTransactionData.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  Clear;
end;

procedure TMercadoPagoPaymentTransactionData.Clear;
begin
  Fqrcode := EmptyStr;
  FqrcodeBase64 := EmptyStr;
  FticketURL := EmptyStr;
end;

function TMercadoPagoPaymentTransactionData.IsEmpty: Boolean;
begin
  Result := EstaVazio(Fqrcode) and EstaVazio(FqrcodeBase64) and EstaVazio(FticketURL);
end;

procedure TMercadoPagoPaymentTransactionData.Assign(aSource: TMercadoPagoPaymentTransactionData);
begin
  Fqrcode := aSource.qrcode;
  FqrcodeBase64 := aSource.qrcodeBase64;
  FticketURL := aSource.ticketURL;
end;

{ TMercadoPagoPaymentPayer }

procedure TMercadoPagoPaymentPayer.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('email', Femail)
    .AddPair('entity_type', PayerEntityTypeToString(Fentity_type), False)
    .AddPair('type', PayerTypeToString(Ftype), False)
    .AddPair('id', Fid, False)
    .AddPair('email', Femail)
    .AddPair('first_name', FfirstName, False)
    .AddPair('last_name', FlastName, False);
  Fidentification.WriteToJSon(aJSon);
end;

procedure TMercadoPagoPaymentPayer.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  s1, s2: String;
begin
  {$IFDEF FPC}
  s1 := EmptyStr;
  s2 := EmptyStr;
  {$ENDIF}
  aJSon
    .Value('email', Femail)
    .Value('entity_type', s1)
    .Value('type', s2)
    .Value('id', Fid)
    .Value('email', Femail)
    .Value('first_name', FfirstName)
    .Value('last_name', FlastName);
  Fentity_type := StringToPayerEntityType(s1);
  Ftype := StringToPayerType(s2);
  Fidentification.ReadFromJSon(aJSon);
end;

constructor TMercadoPagoPaymentPayer.Create(const aObjectName: string);
begin
  inherited Create(aObjectName);
  Fidentification := TMercadoPagoPaymentPayerIdentification.Create('identification');
  Clear;
end;

destructor TMercadoPagoPaymentPayer.Destroy;
begin
  Fidentification.Free;
  inherited Destroy;
end;

procedure TMercadoPagoPaymentPayer.Clear;
begin
  Femail := EmptyStr;
  Fentity_type := metNone;
  FfirstName := EmptyStr;
  Fid := EmptyStr;
  FlastName := EmptyStr;
  Ftype := mptNone;
  Fidentification.Clear;
end;

function TMercadoPagoPaymentPayer.IsEmpty: Boolean;
begin
  Result := (Fentity_type = metNone) and (Ftype = mptNone) and
    EstaVazio(Femail) and
    EstaVazio(FfirstName) and
    EstaVazio(Fid) and
    EstaVazio(FlastName) and
    Fidentification.IsEmpty;
end;

procedure TMercadoPagoPaymentPayer.Assign(aSource: TMercadoPagoPaymentPayer);
begin
  Femail := aSource.email;
  Fentity_type := aSource.entity_type;
  FfirstName := aSource.firstName;
  Fid := aSource.id;
  FlastName := aSource.lastName;
  Ftype := aSource.type_;
  Fidentification.Assign(aSource.identification);
end;

{ TMercadoPagoPaymentPayerIdentification }

procedure TMercadoPagoPaymentPayerIdentification.DoWriteToJSon(
  aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('type', PayerIdentificationTypeToString(Ftype), False)
    .AddPair('number', Fnumber);
end;

procedure TMercadoPagoPaymentPayerIdentification.DoReadFromJSon(
  aJSon: TACBrJSONObject);
var
  s: String;
begin
  s := EmptyStr;
  aJSon
    .Value('type', s)
    .Value('number', Fnumber);
  Ftype := StringToPayerIdentificationType(s);
end;

constructor TMercadoPagoPaymentPayerIdentification.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

procedure TMercadoPagoPaymentPayerIdentification.Clear;
begin
  Ftype := mitNone;
  Fnumber := EmptyStr;
end;

function TMercadoPagoPaymentPayerIdentification.IsEmpty: Boolean;
begin
  Result := (Ftype = mitNone) and EstaVazio(Fnumber);
end;

procedure TMercadoPagoPaymentPayerIdentification.Assign(aSource: TMercadoPagoPaymentPayerIdentification);
begin
  Ftype := aSource.type_;
  Fnumber := aSource.number;
end;

{ TMercadoPagoPaymentRequest }

constructor TMercadoPagoPaymentRequest.Create(const aObjectName: string);
begin
  inherited Create(aObjectName);
  Fpayer := TMercadoPagoPaymentPayer.Create('payer');
  Clear;
end;

destructor TMercadoPagoPaymentRequest.Destroy;
begin
  FPayer.Free;
  inherited Destroy;
end;

function TMercadoPagoPaymentRequest.GetPaymentMethodID: String;
begin
  if NaoEstaVazio(fpaymentMethodID) then
    Result := fpaymentMethodID
  else
    Result := 'pix';
end;

procedure TMercadoPagoPaymentRequest.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('transaction_amount', ftransactionAmount, False)
    .AddPair('description', Fdescription, False)
    .AddPair('payment_method_id', paymentMethodID, False)
    .AddPair('date_of_expiration', FdateExpiration, False)
    .AddPair('authorization_code', FauthorizationCode, False);
  payer.WriteToJSon(aJSon);
end;

procedure TMercadoPagoPaymentRequest.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('transaction_amount', FtransactionAmount)
    .Value('description', Fdescription)
    .Value('payment_method_id', FpaymentMethodID)
    .Value('date_of_expiration', FdateExpiration)
    .Value('authorization_code', FauthorizationCode);
  payer.ReadFromJSon(aJSon);
end;

function TMercadoPagoPaymentRequest.IsEmpty: Boolean;
begin
  Result :=
    EstaZerado(FtransactionAmount) and
    EstaVazio(Fdescription) and
    EstaVazio(FdateExpiration) and
    EstaVazio(FauthorizationCode) and
    Fpayer.IsEmpty;
end;

procedure TMercadoPagoPaymentRequest.Clear;
begin
  FtransactionAmount := 0;
  Fdescription := EmptyStr;
  FdateExpiration := EmptyStr;
  FauthorizationCode := EmptyStr;
  fpaymentMethodID := EmptyStr;
  Fpayer.Clear;
end;

procedure TMercadoPagoPaymentRequest.Assign(aSource: TMercadoPagoPaymentRequest);
begin
  FtransactionAmount := aSource.transactionAmount;
  Fdescription := aSource.description;
  FdateExpiration := aSource.dateExpiration;
  FauthorizationCode := aSource.authorizationCode;
  fpaymentMethodID := aSource.paymentMethodID;
  Fpayer.Assign(aSource.payer);
end;

{ TMercadoPagoPayer }

procedure TMercadoPagoPayer.Assign(aSource: TMercadoPagoPayer);
begin
  Femail := aSource.Email;
  FfirstName := aSource.FirstName;
  FlastName := aSource.LastName;
  FpayerIndentification := aSource.PayerIndentification;
end;

procedure TMercadoPagoPayer.Clear;
begin
 inherited Clear;
  FEmail := EmptyStr;
  FFirstName := EmptyStr;
  FLastName := EmptyStr;
end;

constructor TMercadoPagoPayer.Create(const aObjectName: string);
begin
  inherited Create(aObjectName);
  FPayerIndentification := TMercadoPagoPayerIdentification.Create('identification');
  Clear;
end;

destructor TMercadoPagoPayer.Destroy;
begin
  FPayerIndentification.Free;
  inherited;
end;

function TMercadoPagoPayer.IsEmpty: Boolean;
begin
  Result := inherited IsEmpty and EstaVazio(FEmail) and EstaVazio(FFirstName) and EstaVazio(FLastName);
end;

procedure TMercadoPagoPayer.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('email', FEmail)
    .Value('first_name', FFirstName)
    .Value('last_name', FLastName);
end;
procedure TMercadoPagoPayer.DoWriteToJSon(aJSon: TACBrJSONObject);
var
  jo: TACBrJSONObject;
begin
  jo := TACBrJSONObject.Create;

  if NaoEstaVazio(Email) then
    jo.AddPair('email', Email);
  if NaoEstaVazio(FirstName) then
    jo.AddPair('first_name', FirstName);
  if NaoEstaVazio(LastName) then
    jo.AddPair('last_name', LastName);

  aJSon.AddPair('payer', jo);
end;

{ TMercadoPagoPayerIdentification }

procedure TMercadoPagoPayerIdentification.Assign(aSource: TMercadoPagoPayerIdentification);
begin
  FTypePayer := aSource.TypePayer;
  FNumber := aSource.Number;
end;

procedure TMercadoPagoPayerIdentification.Clear;
begin
  FTypePayer := EmptyStr;
  FNumber := EmptyStr;
end;

function TMercadoPagoPayerIdentification.IsEmpty: Boolean;
begin
  Result := EstaVazio(FTypePayer) and EstaVazio(FNumber);
end;

procedure TMercadoPagoPayerIdentification.ReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('type', FTypePayer)
    .Value('number', FNumber);
end;

procedure TMercadoPagoPayerIdentification.WriteToJSon(aJSon: TACBrJSONObject);
begin
  if NaoEstaVazio(FTypePayer) then
    aJSon.AddPair('type', FTypePayer);

  if NaoEstaVazio(FNumber) then
    aJSon.AddPair('number', FNumber);
end;

function PaymentStatusToString(aStatus: TMercadoPagoPaymentStatus): String;
begin
  case aStatus of
    mpsPending: Result := 'pending';
    mpsApproved: Result := 'approved';
    mpsAuthorized: Result := 'authorized';
    mpsInProcess: Result := 'in_process';
    mpsInMediation: Result := 'in_mediation';
    mpsRejected: Result := 'rejected';
    mpsCancelled: Result := 'cancelled';
    mpsRefunded: Result := 'refunded';
    mpsChargedBack: Result := 'charged_back';
  else
    Result := EmptyStr;
  end;
end;

function StringToPaymentStatus(const aString: String): TMercadoPagoPaymentStatus;
var
  wStatus: String;
begin
  wStatus := LowerCase(Trim(aString));
  if (wStatus = 'pending') then
    Result := mpsPending
  else if (wStatus = 'approved') then
    Result := mpsApproved
  else if (wStatus = 'authorized') then
    Result := mpsAuthorized
  else if (wStatus = 'in_process') then
    Result := mpsInProcess
  else if (wStatus = 'in_mediation') then
    Result := mpsInMediation
  else if (wStatus = 'rejected') then
    Result := mpsRejected
  else if (wStatus = 'cancelled') then
    Result := mpsCancelled
  else if (wStatus = 'refunded') then
    Result := mpsRefunded
  else if (wStatus = 'charged_back') then
    Result := mpsChargedBack
  else
    Result := mpsNone;
end;

function PayerIdentificationTypeToString(const aType: TMercadoPagoPayerIdentificationType): String;
begin
  case aType of
    mitCPF: Result := 'CPF';
    mitCNPJ: Result := 'CNPJ';
    mitCUIT: Result := 'CUIT';
    mitCUIL: Result := 'CUIL';
    mitDNI: Result := 'DNI';
    mitCURP: Result := 'CURP';
    mitRFC: Result := 'RFC';
    mitCC: Result := 'CC';
    mitRUT: Result := 'RUT';
    mitCI: Result := 'CI';
  else
    Result := EmptyStr;
  end;
end;

function StringToPayerIdentificationType(const aValue: String): TMercadoPagoPayerIdentificationType;
var
  wStatus: String;
begin
  wStatus := UpperCase(Trim(aValue));
  if (wStatus = 'CPF') then
    Result := mitCPF
  else if (wStatus = 'CNPJ') then
    Result := mitCNPJ
  else if (wStatus = 'CUIT') then
    Result := mitCUIT
  else if (wStatus = 'CUIL') then
    Result := mitCUIL
  else if (wStatus = 'DNI') then
    Result := mitDNI
  else if (wStatus = 'CURP') then
    Result := mitCURP
  else if (wStatus = 'RFC') then
    Result := mitRFC
  else if (wStatus = 'CC') then
    Result := mitCC
  else if (wStatus = 'RUT') then
    Result := mitRUT
  else if (wStatus = 'CI') then
    Result := mitCI
  else
    Result := mitNone;
end;

function PayerEntityTypeToString(const aValue: TMercadoPagoPayerEntityType): String;
begin
  case aValue of
    metIndividual: Result := 'individual';
    metAssociation: Result := 'association';
  else
    Result := EmptyStr;
  end;
end;

function StringToPayerEntityType(const aValue: String): TMercadoPagoPayerEntityType;
var
  wStatus: String;
begin
  wStatus := LowerCase(Trim(aValue));
  if (wStatus = 'individual') then
    Result := metIndividual
  else if (wStatus = 'association') then
    Result := metAssociation
  else
    Result := metNone;
end;

function PayerTypeToString(const aValue: TMercadoPagoPayerType): String;
begin
  case aValue of
    mptCustomer: Result := 'customer';
    mptGuest: Result := 'guest';
  else
    Result := EmptyStr;
  end;
end;

function StringToPayerType(const aValue: String): TMercadoPagoPayerType;
var
  wStatus: String;
begin
  wStatus := LowerCase(Trim(aValue));
  if (wStatus = 'customer') then
    Result := mptCustomer
  else if (wStatus = 'guest') then
    Result := mptGuest
  else
    Result := mptNone;
end;

function RefundSourceTypeToString(const aValue: TMercadoPagoRefundSourceType): String;
begin
  case aValue of
    mrtAdmin: Result := 'admin';
    mrtCollector: Result := 'collector';
    mrtBPP: Result := 'bpp';
    mrtMarketplace: Result := 'marketplace';
  else
    Result := EmptyStr;
  end;
end;

function StringToRefundSourceType(const aValue: String): TMercadoPagoRefundSourceType;
var
  wType: String;
begin
  wType := LowerCase(Trim(aValue));
  if (wType = 'admin') then
    Result := mrtAdmin
  else if (wType = 'collector') then
    Result := mrtCollector
  else if (wType = 'bpp') then
    Result := mrtBPP
  else if (wType = 'marketplace') then
    Result := mrtMarketplace
  else
    Result := mrtNone;
end;

function RefundStatusToString(const aValue: TMercadoPagoRefundStatus): String;
begin
  case aValue of
    mrsApproved: Result := 'approved';
    mrsInProcess: Result := 'in_process';
    mrsRejected: Result := 'rejected';
    mrsCancelled: Result := 'cancelled';
    mrsAuthorized: Result := 'authorized';
  else
    Result := EmptyStr;
  end;
end;

function StringToRefundStatus(const aValue: String): TMercadoPagoRefundStatus;
var
  wStatus: String;
begin
  wStatus := LowerCase(Trim(aValue));
  if (wStatus = 'approved') then
    Result := mrsApproved
  else if (wStatus = 'in_process') then
    Result := mrsInProcess
  else if (wStatus = 'rejected') then
    Result := mrsRejected
  else if (wStatus = 'cancelled') then
    Result := mrsCancelled
  else if (wStatus = 'authorized') then
    Result := mrsAuthorized
  else
    Result := mrsNone;
end;


end.
