{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2025 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{ - Elias César                                                                }
{ - Júlio Cavalcanti                                                           }
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
  https://doc.appless.com.br

*)

{$I ACBr.inc}

unit ACBrSchemasAppLess;

interface

uses
  Classes, SysUtils,
  ACBrBase,
  ACBrJSON,
  ACBrUtil.Base,
  ACBrPIXBase,
  ACBrPIXSchemasPix;

type  

  TAppLessOrderStatus = (
    aosNone,
    aosCreated,
    aosPayed,
    aosWaiting,
    aosCanceled,
    aosError,
    aosRefunding,
    aosRefunded
  );

  TAppLessTransactionStatus = (
    atsNone,
    atsActive,
    atsApproved,
    atsCanceled
  );

  {TACBrAppLessBankAccount}

  TACBrAppLessBankAccount = class(TACBrPIXSchema)
  private
    fbankAccount: String;
    fbankAccountDv: String;
    fbankAccountType: Integer;
    fbankBranch: String;
    fbankCode: String;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJson(aJson: TACBrJSONObject); override;
    procedure DoReadFromJson(aJson: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrAppLessBankAccount);

    property bankCode: String read fbankCode write fbankCode;
    property bankBranch: String read fbankBranch write fbankBranch;
    property bankAccount: String read fbankAccount write fbankAccount;
    property bankAccountDv: String read fbankAccountDv write fbankAccountDv;
    property bankAccountType: Integer read fbankAccountType write fbankAccountType;

  end;

  {TACBrAppLessBankAccounts}

  TACBrAppLessBankAccounts = class(TACBrPIXSchemaArray)
  private
    function GetItem(Index: Integer): TACBrAppLessBankAccount;
    procedure SetItem(Index: Integer; AValue: TACBrAppLessBankAccount);
  protected
    function NewSchema: TACBrPIXSchema; override;
  public
    function Add(aItem: TACBrAppLessBankAccount): Integer;
    procedure Insert(Index: Integer; aValue: TACBrAppLessBankAccount);
    function New: TACBrAppLessBankAccount;
    property Items[Index: Integer]: TACBrAppLessBankAccount read GetItem write SetItem; default;
  end;

  {TACBrAppLessAddres}

  TACBrAppLessAddres = class (TACBrPIXSchema)
  private
    fcity: String;
    fcomplement: String;
    fcountry: String;
    fdistrict: String;
    fnumber: String;
    fstate: String;
    fstreet: String;
    fzipCode: String;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJson(aJson: TACBrJSONObject); override;
    procedure DoReadFromJson(aJson: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrAppLessAddres);

    property street: String read fstreet write fstreet;
    property number: String read fnumber write fnumber;
    property complement: String read fcomplement write fcomplement;
    property zipCode: String read fzipCode write fzipCode;
    property district: String read fdistrict write fdistrict;
    property city: String read fcity write fcity;
    property state: String read fstate write fstate;
    property country: String read fcountry write fcountry;
  end;

  {TACBrAppLessCustumer}

  TACBrAppLessCustumer = class(TACBrPIXSchema)
  private
    fcnpj: String;
    fcpf: String;
    fname: String;
    faddress: TACBrAppLessAddres;
    function GetAddress: TACBrAppLessAddres;
   protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJson(aJson: TACBrJSONObject); override;
    procedure DoReadFromJson(aJson: TACBrJSONObject); override;
  public
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrAppLessCustumer);

    property name: String read fname write fname;
    property cpf: String read fcpf write fcpf;
    property cnpj: String read fcnpj write fcnpj;
    property address: TACBrAppLessAddres read GetAddress;
  end;

  {TACBrAppLessBeneficiary}

  TACBrAppLessBeneficiary = class(TACBrPIXSchema)
  private
    fdocument: String;
    fname: String;
    ftype_: Integer;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJson(aJson: TACBrJSONObject); override;
    procedure DoReadFromJson(aJson: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrAppLessBeneficiary);

    property type_: Integer read ftype_ write ftype_;
    property document: String read fdocument write fdocument;
    property name: String read fname write fname;
  end;

  {TACBrAppLessInterest}

  TACBrAppLessInterest = class(TACBrPIXSchema)
  private
    famount: Double;
    fdate: TDateTime;
    fid: String;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJson(aJson: TACBrJSONObject); override;
    procedure DoReadFromJson(aJson: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrAppLessInterest);

    property id: String read fid write fid;
    property date: TDateTime read fdate write fdate;
    property amount: Double read famount write famount;
  end;

  {TACBrAppLessFine}

  TACBrAppLessFine = class(TACBrPIXSchema)
  private
    famount: Double;
    fdate: TDateTime;
    fid: String;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJson(aJson: TACBrJSONObject); override;
    procedure DoReadFromJson(aJson: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrAppLessFine);

    property id: String read fid write fid;
    property date: TDateTime read fdate write fdate;
    property amount: Double read famount write famount;
  end;

  {TACBrAppLessDiscount}

  TACBrAppLessDiscount = class(TACBrPIXSchema)
  private
    famount: Double;
    fdate: TDateTime;
    fid: String;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJson(aJson: TACBrJSONObject); override;
    procedure DoReadFromJson(aJson: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrAppLessDiscount);

    property id: String read fid write fid;
    property date: TDateTime read fdate write fdate;
    property amount: Double read famount write famount;
  end;

  {TACBrAppLessBankSlip}

  TACBrAppLessBankSlip = class(TACBrPIXSchema)
  private
    fbeneficiary: TACBrAppLessBeneficiary;
    fcustumer: TACBrAppLessCustumer;
    finterest: TACBrAppLessInterest;
    ffine: TACBrAppLessFine;
    fdiscount: TACBrAppLessDiscount;
    fdueDate: TDateTime;
    finstructions: String;
    flimitDate: TDateTime;
    ftype_: Integer;
    function GetBeneficiary: TACBrAppLessBeneficiary;
    function GetCustumer: TACBrAppLessCustumer;
    function GetDiscount: TACBrAppLessDiscount;
    function GetFine: TACBrAppLessFine;
    function GetInterest: TACBrAppLessInterest;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJson(aJson: TACBrJSONObject); override;
    procedure DoReadFromJson(aJson: TACBrJSONObject); override;
  public
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrAppLessBankSlip);

    property custumer: TACBrAppLessCustumer read GetCustumer;
    property beneficiary: TACBrAppLessBeneficiary read GetBeneficiary;
    property interest: TACBrAppLessInterest read GetInterest;
    property fine: TACBrAppLessFine read GetFine;
    property discount: TACBrAppLessDiscount read GetDiscount;

    property instructions: String read finstructions write finstructions;
    property dueDate: TDateTime read fdueDate write fdueDate;
    property limitDate: TDateTime read flimitDate write flimitDate;
    property type_: Integer read ftype_ write ftype_;
  end;

  { TACBrAppLessOrderClass }

  TACBrAppLessOrderClass = class(TACBrPIXSchema)
  private
    famount: Double;
    fchannel: String;
    fcustumerCPF: String;
    fcustumerSocialName: String;
    fexpiration: Integer;
    fexternalId: String;
    finstallments: Integer;
    fisStaticPix: Boolean;
    fmerchantDocument: String;
    fmerchantName: String;
    forderDescription: String;
    ftypeOrder: String;
    fbankAccounts: TACBrAppLessBankAccounts;
    function GetBankAccounts: TACBrAppLessBankAccounts;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJson(aJson: TACBrJSONObject); override;
    procedure DoReadFromJson(aJson: TACBrJSONObject); override; 

    property amount: Double read famount write famount;
    property externalId: String read fexternalId write fexternalId;
    property custumerSocialName: String read fcustumerSocialName write fcustumerSocialName;
    property customerCPF: String read fcustumerCPF write fcustumerCPF;
    property typeOrder: String read ftypeOrder write ftypeOrder;
    property channel: String read Fchannel write fchannel;
    property installments: Integer read finstallments write finstallments;
    property orderDescription: String read forderDescription write forderDescription;
    property expiration: Integer read fexpiration write fexpiration;
    property merchantDocument: String read fmerchantDocument write fmerchantDocument;
    property merchantName: String read fmerchantName write fmerchantName;
    property isStaticPix: Boolean read fisStaticPix write fisStaticPix;

    property bankAccounts: TACBrAppLessBankAccounts read GetBankAccounts;
  public
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrAppLessOrderClass);
  end;

  TACBrAppLessOrder = class(TACBrAppLessOrderClass)
  public
    property amount;
    property externalId;
    property custumerSocialName;
    property customerCPF;
  end;

  { TACBrAppLessCalendario }

  TACBrAppLessCalendario = class(TACBrPIXSchema)
  private
    fexpiracao: Integer;
    fcriacao: TDateTime;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJson(aJson: TACBrJSONObject); override;
    procedure DoReadFromJson(aJson: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrAppLessCalendario);

    property expiracao: Integer read fexpiracao write fexpiracao;
    property criacao: TDateTime read fcriacao write fcriacao;
  end;

  { TACBrAppLessValor }

  TACBrAppLessValor = class(TACBrPIXSchema)
  private
    foriginal: String;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJson(aJson: TACBrJSONObject); override;
    procedure DoReadFromJson(aJson: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrAppLessValor);

    property original: String read foriginal write foriginal;
  end;

  { TACBrAppLessCobResponse }

  TACBrAppLessCobResponse = class(TACBrPIXSchema)
  private
    fcalendario: TACBrAppLessCalendario;
    fvalor: TACBrAppLessValor;
    fstatus: TAppLessTransactionStatus;
    furlPix: String;
    fpix: TACBrPIXArray;
    fpixCopiaECola: String;
    fpixCopiaEColaFormatted: String;
    function GetCalendario: TACBrAppLessCalendario;
    function Getpix: TACBrPIXArray;
    function GetValor: TACBrAppLessValor;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJson(aJson: TACBrJSONObject); override;
    procedure DoReadFromJson(aJson: TACBrJSONObject); override;
  public
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrAppLessCobResponse);

    property pix: TACBrPIXArray read Getpix;
    property status: TAppLessTransactionStatus read fstatus write fstatus;
    property urlPix: String read furlPix write furlPix;
    property pixCopiaECola: String read fpixCopiaECola write fpixCopiaECola;
    property pixCopiaEColaFormatted: String read fpixCopiaEColaFormatted write fpixCopiaEColaFormatted;

    property valor: TACBrAppLessValor read GetValor;
    property calendario: TACBrAppLessCalendario read GetCalendario;
  end;

  { TACBrAppLessTransactionPix }

  TACBrAppLessTransactionPix = class(TACBrPIXSchema)
  private
    ftxid: String;
    fcobResponse: TACBrAppLessCobResponse;
    fcreated: TDateTime;
    function GetCobResponse: TACBrAppLessCobResponse;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJson(aJson: TACBrJSONObject); override;
    procedure DoReadFromJson(aJson: TACBrJSONObject); override;
  public
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrAppLessTransactionPix);

    property txid: String read ftxid write ftxid;
    property cobResponse: TACBrAppLessCobResponse read GetCobResponse;
    property created: TDateTime read fcreated write fcreated;
  end;

  { TACBrAppLessTransaction }

  TACBrAppLessTransaction = class(TACBrPIXSchema)
  private
    ftransactionPix: TACBrAppLessTransactionPix;
    function GetTransactionPix: TACBrAppLessTransactionPix;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJson(aJson: TACBrJSONObject); override;
    procedure DoReadFromJson(aJson: TACBrJSONObject); override;
  public
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrAppLessTransaction);

    property transactionPix: TACBrAppLessTransactionPix read GetTransactionPix;
  end;

  { TACBrAppLessOrderResponse }

  TACBrAppLessOrderResponse = class(TACBrPIXSchema)
  private
    fid: String;
    forderId: String;
    fcustomerSocialName: String;
    famount: Double;
    famountPayed: Double;
    famountFormatted: String;
    finstallments: Integer;
    ftypeOrder: String;
    fstatus: TAppLessOrderStatus;
    fdtCreated: TDateTime;
    fdtWaiting: TDateTime;
    fmessageTransaction: String;
    fexternalId: String;
    fcustomerCNPJ: String;
    ftransaction: TACBrAppLessTransaction;
    fexpiration: Integer;
    ftxid: String;
    fcreatedAt: TDateTime;
    fcompanyNickName: String;
    fisStaticPix: Boolean;
    function GetTransaction: TACBrAppLessTransaction;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJson(aJson: TACBrJSONObject); override;
    procedure DoReadFromJson(aJson: TACBrJSONObject); override;
  public
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrAppLessOrderResponse);

    property id: String read fid write fid;
    property orderId: String read forderId write forderId;
    property customerSocialName: String read fcustomerSocialName write fcustomerSocialName;
    property amount: Double read famount write famount;
    property amountPayed: Double read famountPayed write famountPayed;
    property amountFormatted: String read famountFormatted write famountFormatted;
    property installments: Integer read finstallments write finstallments;
    property typeOrder: String read ftypeOrder write ftypeOrder;
    property status: TAppLessOrderStatus read fstatus write fstatus;
    property dtCreated: TDateTime read fdtCreated write fdtCreated;
    property dtWaiting: TDateTime read fdtWaiting write fdtWaiting;
    property messageTransaction: String read fmessageTransaction write fmessageTransaction;
    property externalId: String read fexternalId write fexternalId;
    property customerCNPJ: String read fcustomerCNPJ write fcustomerCNPJ;
    property transaction: TACBrAppLessTransaction read GetTransaction;
    property expiration: Integer read fexpiration write fexpiration;
    property txid: String read ftxid write ftxid;
    property createdAt: TDateTime read fcreatedAt write fcreatedAt;
    property companyNickName: String read fcompanyNickName write fcompanyNickName;
    property isStaticPix: Boolean read fisStaticPix write fisStaticPix;
  end;

  { TACBrAppLessOrders }

  TACBrAppLessOrders = class(TACBrPIXSchemaArray)
  private
    function GetItem(Index: Integer): TACBrAppLessOrderResponse;
    procedure SetItem(Index: Integer; Value: TACBrAppLessOrderResponse);
  protected
    function NewSchema: TACBrPIXSchema; override;
  public
    function Add(AItem: TACBrAppLessOrderResponse): Integer;
    procedure Insert(Index: Integer; Value: TACBrAppLessOrderResponse);
    function New: TACBrAppLessOrderResponse;
    property Items[Index: Integer]: TACBrAppLessOrderResponse read GetItem write SetItem; default;
  end;

  { TACBrAppLessOrdersResponse }

  TACBrAppLessOrdersResponse = class(TACBrPIXSchema)
  private
    frange: Integer;
    fskip: Integer;
    fcount: Integer;
    fresults: TACBrAppLessOrders;
    function Getresults: TACBrAppLessOrders;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJson(aJson: TACBrJSONObject); override;
    procedure DoReadFromJson(aJson: TACBrJSONObject); override;
  public
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrAppLessOrdersResponse);

    property range: Integer read frange write frange;
    property skip: Integer read fskip write fskip;
    property count: Integer read fcount write fcount;

    property results: TACBrAppLessOrders read Getresults;
  end;

  { TACBrAppLessBankSlipOrder }

  TACBrAppLessBankSlipOrder = class(TACBrAppLessOrderClass)
  public
    property amount;
    property typeOrder;
    property externalId;
    property channel;
  end;

  { TACBrAppLessBankSlipOrderRequest }

  TACBrAppLessBankSlipOrderRequest = class(TACBrPIXSchema)
  private
    forder: TACBrAppLessBankSlipOrder;
    fbankSlip: TACBrAppLessBankSlip;
    function GetOrder: TACBrAppLessBankSlipOrder;
    function GetBankSlip: TACBrAppLessBankSlip;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJson(aJson: TACBrJSONObject); override;
    procedure DoReadFromJson(aJson: TACBrJSONObject); override;
  public
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrAppLessBankSlipOrderRequest);

    property order: TACBrAppLessBankSlipOrder read GetOrder;
    property bankSlip: TACBrAppLessBankSlip read GetBankSlip;
  end; 

  function AppLessOrderStatusToString(aType: TAppLessOrderStatus): String;
  function AppLessOrderStatusToInteger(aType: TAppLessOrderStatus): Integer;
  function StringToAppLessOrderStatus(const aString: String): TAppLessOrderStatus;

  function AppLessTransactionStatusToString(aType: TAppLessTransactionStatus): String;
  function StringToAppLessTransactionStatus(const aString: String): TAppLessTransactionStatus;

implementation

uses
  ACBrUtil.DateTime;

function AppLessOrderStatusToString(aType: TAppLessOrderStatus): String;
begin
  case aType of
    aosCreated: Result := 'created';
    aosPayed: Result := 'payed';
    aosWaiting: Result := 'waiting';
    aosCanceled: Result := 'canceled';
    aosError: Result := 'error';
    aosRefunding: Result := 'refunding';
    aosRefunded: Result := 'refunded';
  else
    Result := EmptyStr;
  end;
end;

function AppLessOrderStatusToInteger(aType: TAppLessOrderStatus): Integer;
begin
  case aType of
    aosCreated: Result := 0;
    aosPayed: Result := 1;
    aosWaiting: Result := 2;
    aosCanceled: Result := 3;
    aosError: Result := 4;
    aosRefunding: Result := 5;
    aosRefunded: Result := 6;
  else
    Result := -1;
  end;
end;

function StringToAppLessOrderStatus(const aString: String): TAppLessOrderStatus;
var
  s: String;
begin
  s := LowerCase(Trim(aString));

  if (s = 'created') then
    Result := aosCreated
  else if (s = 'payed') then
    Result := aosPayed
  else if (s = 'waiting') then
    Result := aosWaiting
  else if (s = 'canceled') then
    Result := aosCanceled
  else if (s = 'error') then
    Result := aosError
  else if (s = 'refunding') then
    Result := aosRefunding
  else if (s = 'refunded') then
    Result := aosRefunded
  else
    Result := aosNone;
end;

function AppLessTransactionStatusToString(aType: TAppLessTransactionStatus): String;
begin
  case aType of
    atsActive: Result := 'ACTIVE';
    atsApproved: Result := 'APPROVED';
    atsCanceled: Result := 'CANCELED';
  else
    Result := EmptyStr;
  end;
end;

function StringToAppLessTransactionStatus(const aString: String): TAppLessTransactionStatus;
var
  s: String;
begin
  s := UpperCase(Trim(aString));

  if (s = 'ACTIVE') then
    Result := atsActive
  else if (s = 'APPROVED') then
    Result := atsApproved
  else if (s = 'CANCELED') then
    Result := atsCanceled
  else
    Result := atsNone;
end;

{ TACBrAppLessBankAccount }

procedure TACBrAppLessBankAccount.AssignSchema(aSource: TACBrPIXSchema);
begin
  if Assigned(aSource) and (aSource is TACBrAppLessBankAccount) then
    Assign(TACBrAppLessBankAccount(aSource));
end;

procedure TACBrAppLessBankAccount.DoWriteToJson(aJson: TACBrJSONObject);
begin
  if (not Assigned(aJson)) then
    Exit;

  aJson
    .AddPair('bankCode', fbankCode)
    .AddPair('bankBranch', fbankBranch)
    .AddPair('bankAccount', fbankAccount)
    .AddPair('bankAccountDv', fbankAccountDv)
    .AddPair('bankAccountType', fbankAccountType);
end;

procedure TACBrAppLessBankAccount.DoReadFromJson(aJson: TACBrJSONObject);
begin
  if (not Assigned(aJson)) then
    Exit;

  aJson
    .Value('bankCode', fbankCode)
    .Value('bankBranch', fbankBranch)
    .Value('bankAccount', fbankAccount)
    .Value('bankAccountDv', fbankAccountDv)
    .Value('bankAccountType', fbankAccountType);
end;

procedure TACBrAppLessBankAccount.Clear;
begin
  fbankCode := EmptyStr;
  fbankBranch := EmptyStr;
  fbankAccount := EmptyStr;
  fbankAccountDv := EmptyStr;
  fbankAccountType := 0;
end;

function TACBrAppLessBankAccount.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(fbankCode) and
    EstaVazio(fbankBranch) and
    EstaVazio(fbankAccount) and
    EstaVazio(fbankAccountDv) and
    EstaZerado(fbankAccountType);
end;

procedure TACBrAppLessBankAccount.Assign(Source: TACBrAppLessBankAccount);
begin
  if (not Assigned(Source)) then
    Exit;

  fbankCode := Source.bankCode;
  fbankBranch := Source.bankBranch;
  fbankAccount := Source.bankAccount;
  fbankAccountDv := Source.bankAccountDv;
  fbankAccountType := Source.bankAccountType;
end;

{ TACBrAppLessBankAccounts }

function TACBrAppLessBankAccounts.GetItem(Index: Integer): TACBrAppLessBankAccount;
begin
  Result := TACBrAppLessBankAccount(inherited Items[Index]);
end;

procedure TACBrAppLessBankAccounts.SetItem(Index: Integer; AValue: TACBrAppLessBankAccount);
begin
  inherited Items[Index] := aValue;
end;

function TACBrAppLessBankAccounts.NewSchema: TACBrPIXSchema;
begin
  Result := New;
end;

function TACBrAppLessBankAccounts.Add(aItem: TACBrAppLessBankAccount): Integer;
begin
  Result := inherited Add(aItem);
end;

procedure TACBrAppLessBankAccounts.Insert(Index: Integer; aValue: TACBrAppLessBankAccount);
begin
  inherited Insert(Index, aValue);
end;

function TACBrAppLessBankAccounts.New: TACBrAppLessBankAccount;
begin
  Result := TACBrAppLessBankAccount.Create;
  Self.Add(Result)
end;

{ TACBrAppLessAddres }

procedure TACBrAppLessAddres.AssignSchema(aSource: TACBrPIXSchema);
begin
  if Assigned(aSource) and (aSource is TACBrAppLessAddres) then
    Assign(TACBrAppLessAddres(aSource));
end;

procedure TACBrAppLessAddres.DoWriteToJson(aJson: TACBrJSONObject);
begin
  if (not Assigned(aJson)) then
    Exit;

  aJson
    .AddPair('street', fstreet)
    .AddPair('number', fnumber)
    .AddPair('complement', fcomplement)
    .AddPair('zipCode', fzipCode)
    .AddPair('district', fdistrict)
    .AddPair('city', fcity)
    .AddPair('state', fstate)
    .AddPair('country', fcountry);
end;

procedure TACBrAppLessAddres.DoReadFromJson(aJson: TACBrJSONObject);
begin
  if (not Assigned(aJson)) then
    Exit;

  aJson
    .Value('street', fstreet)
    .Value('number', fnumber)
    .Value('complement', fcomplement)
    .Value('zipCode', fzipCode)
    .Value('district', fdistrict)
    .Value('city', fcity)
    .Value('state', fstate)
    .Value('country', fcountry);
end;

procedure TACBrAppLessAddres.Clear;
begin
  fstreet := EmptyStr;
  fnumber := EmptyStr;
  fcomplement := EmptyStr;
  fzipCode := EmptyStr;
  fdistrict := EmptyStr;
  fcity := EmptyStr;
  fstate := EmptyStr;
  fcountry := EmptyStr;
end;

function TACBrAppLessAddres.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(fstreet) and
    EstaVazio(fnumber) and
    EstaVazio(fcomplement) and
    EstaVazio(fzipCode) and
    EstaVazio(fdistrict) and
    EstaVazio(fcity) and
    EstaVazio(fstate) and
    EstaVazio(fcountry);
end;

procedure TACBrAppLessAddres.Assign(Source: TACBrAppLessAddres);
begin
  if (not Assigned(Source)) then
    Exit;

  fstreet := Source.street;
  fnumber := Source.number;
  fcomplement := Source.complement;
  fzipCode := Source.zipCode;
  fdistrict := Source.district;
  fcity := Source.city;
  fstate := Source.state;
  fcountry := Source.country;
end;

{ TACBrAppLessCustumer }

function TACBrAppLessCustumer.GetAddress: TACBrAppLessAddres;
begin
  if (not Assigned(faddress)) then
    faddress := TACBrAppLessAddres.Create('address');
  Result := faddress;
end;

procedure TACBrAppLessCustumer.AssignSchema(aSource: TACBrPIXSchema);
begin
  if Assigned(aSource) and (aSource is TACBrAppLessCustumer) then
    Assign(TACBrAppLessCustumer(aSource));
end;

procedure TACBrAppLessCustumer.DoWriteToJson(aJson: TACBrJSONObject);
begin
  if (not Assigned(aJson)) then
    Exit;

  aJson
    .AddPair('cnpj', fcnpj)
    .AddPair('cpf', fcpf)
    .AddPair('name', fname);
  if Assigned(faddress) then
    faddress.WriteToJson(aJson);
end;

procedure TACBrAppLessCustumer.DoReadFromJson(aJson: TACBrJSONObject);
begin
  if (not Assigned(aJson)) then
    Exit;

  aJson
    .Value('cnpj', fcnpj)
    .Value('cpf', fcpf)
    .Value('name', fname);

  if Assigned(faddress) then
    faddress.ReadFromJson(aJson);
end;

destructor TACBrAppLessCustumer.Destroy;
begin
  if Assigned(faddress) then
    faddress.Free;

  inherited Destroy;
end;

procedure TACBrAppLessCustumer.Clear;
begin
  fcnpj := EmptyStr;
  fcpf := EmptyStr;
  fname := EmptyStr;
  if Assigned(faddress) then
    faddress.Clear;
end;

function TACBrAppLessCustumer.IsEmpty: Boolean;
begin
  Result :=
  EstaVazio(fcnpj) and
  EstaVazio(fcpf) and
  EstaVazio(fname) and
  (not Assigned(faddress) or faddress.IsEmpty);
end;

procedure TACBrAppLessCustumer.Assign(Source: TACBrAppLessCustumer);
begin
  if (not Assigned(Source)) then
    Exit;

  fcnpj:= Source.cnpj;
  fcpf := Source.cpf;
  fname := Source.name;
  address.Assign(Source.address);
end;

{ TACBrAppLessBeneficiary }

procedure TACBrAppLessBeneficiary.AssignSchema(aSource: TACBrPIXSchema);
begin
  if Assigned(aSource) and (aSource is TACBrAppLessBeneficiary) then
    Assign(TACBrAppLessBeneficiary(aSource));
end;

procedure TACBrAppLessBeneficiary.DoWriteToJson(aJson: TACBrJSONObject);
begin
  if (not Assigned(aJson)) then
    Exit;

  aJson
    .AddPair('document', fdocument)
    .AddPair('name', fname)
    .AddPair('type', ftype_);
end;

procedure TACBrAppLessBeneficiary.DoReadFromJson(aJson: TACBrJSONObject);
begin
  if (not Assigned(aJson)) then
    Exit;

  aJson
    .Value('document', fdocument)
    .Value('name', fname)
    .Value('type', ftype_);
end;

procedure TACBrAppLessBeneficiary.Clear;
begin
  fdocument := EmptyStr;
  fname := EmptyStr;
  ftype_ :=0;
end;

function TACBrAppLessBeneficiary.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(fdocument) and
    EstaVazio(fname) and
    EstaZerado(ftype_);
end;

procedure TACBrAppLessBeneficiary.Assign(Source: TACBrAppLessBeneficiary);
begin
  if (not Assigned(Source)) then
    Exit;

  fdocument := Source.document;
  fname := Source.name;
  ftype_ := Source.type_;
end;

{ TACBrAppLessInterest }

procedure TACBrAppLessInterest.AssignSchema(aSource: TACBrPIXSchema);
begin
  if Assigned(aSource) and (aSource is TACBrAppLessInterest) then
    Assign(TACBrAppLessInterest(aSource));
end;

procedure TACBrAppLessInterest.DoWriteToJson(aJson: TACBrJSONObject);
begin
  if (not Assigned(aJson)) then
    Exit;

  aJson
    .AddPair('id', fid)
    .AddPair('date', FormatDateBr(fdate, 'YYYY-MM-DD'))
    .AddPair('amount', famount);
end;

procedure TACBrAppLessInterest.DoReadFromJson(aJson: TACBrJSONObject);
var
  s: String;
begin
  {$IFDEF FPC}s := EmptyStr;{$ENDIF}
  if (not Assigned(aJson)) then
    Exit;

  aJson
    .Value('id', fid)
    .Value('date', s)
    .Value('amount', famount);
  if NaoEstaVazio(s) then
    fdate := StringToDateTime(s, 'YYYY-MM-DD');
end;

procedure TACBrAppLessInterest.Clear;
begin
  fid := EmptyStr;
  fdate := 0;
  famount := 0;
end;

function TACBrAppLessInterest.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(fid) and
    EstaZerado(fdate) and
    EstaZerado(famount);
end;

procedure TACBrAppLessInterest.Assign(Source: TACBrAppLessInterest);
begin
  if (not Assigned(Source)) then
    Exit;

  fid := Source.id;
  fdate := Source.date;
  famount := Source.amount;
end;

{ TACBrAppLessFine }

procedure TACBrAppLessFine.AssignSchema(aSource: TACBrPIXSchema);
begin
  if Assigned(aSource) and (aSource is TACBrAppLessFine) then
    Assign(TACBrAppLessFine(aSource));
end;

procedure TACBrAppLessFine.DoWriteToJson(aJson: TACBrJSONObject);
begin
  if (not Assigned(aJson)) then
    Exit;

  aJson
  .AddPair('id', fid)
  .AddPair('date', FormatDateBr(fdate, 'YYYY-MM-DD'))
  .AddPair('amount', famount);
end;

procedure TACBrAppLessFine.DoReadFromJson(aJson: TACBrJSONObject);
var
  s: String;
begin
  {$IFDEF FPC}s := EmptyStr;{$ENDIF}
   if (not Assigned(aJson)) then
    Exit;

  aJson
  .Value('id', fid)
  .Value('date', s)
  .Value('amount', famount);
  if NaoEstaVazio(s) then
    fdate := StringToDateTime(s, 'YYYY-MM-DD');
end;

procedure TACBrAppLessFine.Clear;
begin
  fid := EmptyStr;
  fdate := 0;
  famount := 0;
end;

function TACBrAppLessFine.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(fid) and
    EstaZerado(fdate) and
    EstaZerado(famount);
end;

procedure TACBrAppLessFine.Assign(Source: TACBrAppLessFine);
begin
  if (not Assigned(Source)) then
    Exit;

  fid := Source.id;
  fdate := Source.date;
  famount := Source.amount;
end;

{ TACBrAppLessDiscount }

procedure TACBrAppLessDiscount.AssignSchema(aSource: TACBrPIXSchema);
begin
  if Assigned(aSource) and (aSource is TACBrAppLessDiscount) then
    Assign(TACBrAppLessDiscount(aSource));
end;

procedure TACBrAppLessDiscount.DoWriteToJson(aJson: TACBrJSONObject);
begin
  if (not Assigned(aJson)) then
    Exit;

  aJson
  .AddPair('id', fid)
  .AddPair('date', FormatDateBr(fdate, 'YYYY-MM-DD'))
  .AddPair('amount', famount);
end;

procedure TACBrAppLessDiscount.DoReadFromJson(aJson: TACBrJSONObject);
var
  s: String;
begin 
  {$IFDEF FPC}s := EmptyStr;{$ENDIF}
  if (not Assigned(aJson)) then
    Exit;

  aJson
  .Value('id', fid)
  .Value('date', s)
  .Value('amount', famount);
  if NaoEstaVazio(s) then
    fdate := StringToDateTime(s, 'YYYY-MM-DD');
end;

procedure TACBrAppLessDiscount.Clear;
begin
  fid := EmptyStr;
  fdate := 0;
  famount := 0;
end;

function TACBrAppLessDiscount.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(fid) and
    EstaZerado(fdate) and
    EstaZerado(famount);
end;

procedure TACBrAppLessDiscount.Assign(Source: TACBrAppLessDiscount);
begin
  if (not Assigned(Source)) then
    Exit;

  fid := Source.id;
  fdate := Source.date;
  famount := Source.amount;
end;

{ TACBrAppLessBankSlip }

function TACBrAppLessBankSlip.GetBeneficiary: TACBrAppLessBeneficiary;
begin
  if (not Assigned(fbeneficiary)) then
    fbeneficiary := TACBrAppLessBeneficiary.Create('beneficiary');
  Result := fbeneficiary;
end;

function TACBrAppLessBankSlip.GetCustumer: TACBrAppLessCustumer;
begin
  if (not Assigned(fcustumer)) then
    fcustumer := TACBrAppLessCustumer.Create('customer');
  Result := fcustumer;
end;

function TACBrAppLessBankSlip.GetDiscount: TACBrAppLessDiscount;
begin
  if (not Assigned(fdiscount)) then
    fdiscount := TACBrAppLessDiscount.Create('discount');
  Result := fdiscount;
end;

function TACBrAppLessBankSlip.GetFine: TACBrAppLessFine;
begin
  if (not Assigned(ffine)) then
    ffine := TACBrAppLessFine.Create('fine');
  Result := ffine;
end;

function TACBrAppLessBankSlip.GetInterest: TACBrAppLessInterest;
begin
  if (not Assigned(finterest)) then
    finterest := TACBrAppLessInterest.Create('interest');
  Result := finterest;
end;

procedure TACBrAppLessBankSlip.AssignSchema(aSource: TACBrPIXSchema);
begin
  if Assigned(aSource) and (aSource is TACBrAppLessBankSlip) then
    Assign(TACBrAppLessBankSlip(aSource));
end;

procedure TACBrAppLessBankSlip.DoWriteToJson(aJson: TACBrJSONObject);
begin
  if (not Assigned(aJson)) then
    Exit;

  aJson
    .AddPair('instructions', finstructions)
    .AddPair('dueDate', FormatDateBr(fdueDate, 'YYYY-MM-DD'))
    .AddPair('limitDate', FormatDateBr(flimitDate, 'YYYY-MM-DD'))
    .AddPair('type', ftype_);

  if Assigned(fcustumer) then
    fcustumer.WriteToJson(aJson);

  if Assigned(fbeneficiary) then
    fbeneficiary.WriteToJson(aJson);

  if Assigned(finterest) then
    finterest.WriteToJson(aJson);

  if Assigned(ffine) then
    ffine.WriteToJson(aJson);

  if Assigned(fdiscount) then
    fdiscount.WriteToJson(aJson);
end;

procedure TACBrAppLessBankSlip.DoReadFromJson(aJson: TACBrJSONObject);
var
  s1, s2: String;
begin 
  {$IFDEF FPC}
  s1 := EmptyStr;
  s2 := EmptyStr;
  {$ENDIF}
  if (not Assigned(aJson)) then
    Exit;

  aJson
    .Value('instructions', finstructions)
    .Value('dueDate', s1)
    .Value('limitDate', s2)
    .Value('type', ftype_);

  if NaoEstaVazio(s1) then
    fdueDate := StringToDateTime(s1, 'YYYY-MM-DD');

  if NaoEstaVazio(s2) then
    flimitDate := StringToDateTime(s2, 'YYYY-MM-DD');

  if Assigned(fcustumer) then
    fcustumer.ReadFromJson(aJson);

  if Assigned(fbeneficiary) then
    fbeneficiary.ReadFromJson(aJson);

  if Assigned(finterest) then
    finterest.ReadFromJson(aJson);

  if Assigned(ffine) then
    ffine.ReadFromJson(aJson);

  if Assigned(fdiscount) then
    fdiscount.ReadFromJson(aJson);
end;

destructor TACBrAppLessBankSlip.Destroy;
begin
  if Assigned(fcustumer) then
    fcustumer.Free;

  if Assigned(fbeneficiary) then
    fbeneficiary.Free;

  if Assigned(finterest) then
    finterest.Free;

  if Assigned(ffine) then
    ffine.Free;

  if Assigned(fdiscount) then
    fdiscount.Free;

  inherited Destroy;
end;

procedure TACBrAppLessBankSlip.Clear;
begin
  finstructions := EmptyStr;
  fdueDate := 0;
  flimitDate := 0;
  ftype_:= 0;

  if Assigned(fcustumer) then
    fcustumer.Clear;

  if Assigned(fbeneficiary) then
    fbeneficiary.Clear;

  if Assigned(finterest) then
    finterest.Clear;

  if Assigned(ffine) then
    ffine.Clear;

  if Assigned(fdiscount) then
    fdiscount.Clear;
end;

function TACBrAppLessBankSlip.IsEmpty: Boolean;
begin
  Result:=
    EstaVazio(finstructions) and
    EstaZerado(fdueDate) and
    EstaZerado(flimitDate) and
    EstaZerado(ftype_) and
    (not Assigned(fcustumer) or fcustumer.IsEmpty) and
    (not Assigned(fbeneficiary) or fbeneficiary.IsEmpty) and
    (not Assigned(finterest) or finterest.IsEmpty) and
    (not Assigned(ffine) or ffine.IsEmpty) and
    (not Assigned(fdiscount) or fdiscount.IsEmpty);
end;

procedure TACBrAppLessBankSlip.Assign(Source: TACBrAppLessBankSlip);
begin
  if (not Assigned(Source)) then
    Exit;

  finstructions := Source.instructions;
  fdueDate := Source.dueDate;
  flimitDate := Source.limitDate;
  ftype_ := Source.type_;
  custumer.Assign(Source.custumer);
  beneficiary.Assign(Source.beneficiary);
  interest.Assign(Source.interest);
  fine.Assign(Source.fine);
  discount.Assign(Source.discount);
end;

{ TACBrAppLessOrderClass }

function TACBrAppLessOrderClass.GetBankAccounts: TACBrAppLessBankAccounts;
begin
  if (not Assigned(fbankAccounts)) then
    fbankAccounts := TACBrAppLessBankAccounts.Create('bankAccounts');
  Result := fbankAccounts;
end;

procedure TACBrAppLessOrderClass.AssignSchema(aSource: TACBrPIXSchema);
begin
  if Assigned(aSource) and (aSource is TACBrAppLessOrderClass) then
    Assign(TACBrAppLessOrderClass(aSource));
end;

procedure TACBrAppLessOrderClass.DoWriteToJson(aJson: TACBrJSONObject);
begin
  if (not Assigned(aJson)) then
    Exit;

  aJson
    .AddPair('amount', famount)
    .AddPair('channel', fchannel, False)
    .AddPair('custumerCPF', fcustumerCPF, False)
    .AddPair('custumerSocialName', fcustumerSocialName, False)
    .AddPair('expiration', fexpiration, False)
    .AddPair('externalId', fexternalId)
    .AddPair('installments', finstallments, False)
    .AddPair('merchantDocument', fmerchantDocument, False)
    .AddPair('merchantName', fmerchantName, False)
    .AddPair('orderDescription', forderDescription, False)
    .AddPair('typeOrder', ftypeOrder, False);

  if fisStaticPix then
    aJson.AddPair('isStaticPix', fisStaticPix);
  if Assigned(fbankAccounts) then
    fbankAccounts.WriteToJson(aJson);
end;

procedure TACBrAppLessOrderClass.DoReadFromJson(aJson: TACBrJSONObject);
begin
  if (not Assigned(aJson)) then
    Exit;

  aJson
    .Value('amount', famount)
    .Value('channel', fchannel)
    .Value('custumerCPF', fcustumerCPF)
    .Value('custumerSocialName', fcustumerSocialName)
    .Value('expiration', fexpiration)
    .Value('externalId', fexternalId)
    .Value('installments', finstallments)
    .Value('isStaticPix', fisStaticPix)
    .Value('merchantDocument', fmerchantDocument)
    .Value('merchantName', fmerchantName)
    .Value('orderDescription', forderDescription)
    .Value('typeOrder', ftypeOrder);
  bankAccounts.ReadFromJson(aJson);
end;

destructor TACBrAppLessOrderClass.Destroy;
begin
  if Assigned(fbankAccounts) then
    fbankAccounts.Free;

  inherited Destroy;
end;

procedure TACBrAppLessOrderClass.Clear;
begin
  famount := 0;
  fchannel := EmptyStr;
  fcustumerCPF := EmptyStr;
  fcustumerSocialName := EmptyStr;
  fexpiration := 0;
  fexternalId := EmptyStr;
  finstallments := 0;
  fisStaticPix := False;
  fmerchantDocument := EmptyStr;
  fmerchantName := EmptyStr;
  forderDescription := EmptyStr;
  ftypeOrder := EmptyStr;

  if Assigned(fbankAccounts) then
    fbankAccounts.Clear;
end;

function TACBrAppLessOrderClass.IsEmpty: Boolean;
begin
  Result :=
    EstaZerado(famount) and
    EstaVazio(fchannel) and
    EstaVazio(fcustumerCPF) and
    EstaVazio(fcustumerSocialName) and
    EstaZerado(fexpiration) and
    EstaVazio(fexternalId) and
    EstaZerado(finstallments) and
    (not fisStaticPix) and
    EstaVazio(fmerchantDocument) and
    EstaVazio(fmerchantName) and
    EstaVazio(forderDescription) and
    EstaVazio(ftypeOrder) and
    (not Assigned(fbankAccounts) or fbankAccounts.IsEmpty);
end;

procedure TACBrAppLessOrderClass.Assign(Source: TACBrAppLessOrderClass);
begin
  if (not Assigned(Source)) then
    Exit;

  famount := Source.amount;
  fchannel := Source.channel;
  fcustumerCPF := Source.customerCPF;
  fcustumerSocialName := Source.custumerSocialName;
  fexpiration := Source.expiration;
  fexternalId := Source.externalId;
  finstallments := Source.installments;
  fisStaticPix := Source.isStaticPix;
  fmerchantDocument := Source.merchantDocument;
  fmerchantName := Source.merchantName;
  forderDescription := Source.orderDescription;
  ftypeOrder := Source.typeOrder;
  bankAccounts.Assign(Source.bankAccounts)
end;

{ TACBrAppLessCalendario }

procedure TACBrAppLessCalendario.AssignSchema(aSource: TACBrPIXSchema);
begin
  if Assigned(aSource) and (aSource is TACBrAppLessCalendario) then
    Assign(TACBrAppLessCalendario(aSource));
end;

procedure TACBrAppLessCalendario.DoWriteToJson(aJson: TACBrJSONObject);
begin
  if not Assigned(aJson) then
    Exit;

  aJson
    .AddPair('expiracao', fexpiracao)
    .AddPair('criacao', DateTimeToIso8601(fcriacao));
end;

procedure TACBrAppLessCalendario.DoReadFromJson(aJson: TACBrJSONObject);
var
  s: String;
begin
  {$IFDEF FPC}s := EmptyStr;{$ENDIF}
  if not Assigned(aJson) then
    Exit;

  aJson
    .Value('expiracao', fexpiracao)
    .Value('criacao', s);

  if NaoEstaVazio(s) then
    fcriacao := Iso8601ToDateTime(s);
end;

procedure TACBrAppLessCalendario.Clear;
begin
  fexpiracao := 0;
  fcriacao := 0;
end;

function TACBrAppLessCalendario.IsEmpty: Boolean;
begin
  Result :=
    EstaZerado(fexpiracao) and
    EstaZerado(fcriacao);
end;

procedure TACBrAppLessCalendario.Assign(Source: TACBrAppLessCalendario);
begin
  if not Assigned(Source) then
    Exit;
  fexpiracao := Source.expiracao;
  fcriacao := Source.criacao;
end;

{ TACBrAppLessValor }

procedure TACBrAppLessValor.AssignSchema(aSource: TACBrPIXSchema);
begin
  if Assigned(aSource) and (aSource is TACBrAppLessValor) then
    Assign(TACBrAppLessValor(aSource));
end;

procedure TACBrAppLessValor.DoWriteToJson(aJson: TACBrJSONObject);
begin
  if not Assigned(aJson) then
    Exit;

  aJson.AddPair('original', foriginal);
end;

procedure TACBrAppLessValor.DoReadFromJson(aJson: TACBrJSONObject);
begin
  if not Assigned(aJson) then
    Exit;

  aJson.Value('original', foriginal);
end;

procedure TACBrAppLessValor.Clear;
begin
  foriginal := EmptyStr;
end;

function TACBrAppLessValor.IsEmpty: Boolean;
begin
  Result := EstaVazio(foriginal);
end;

procedure TACBrAppLessValor.Assign(Source: TACBrAppLessValor);
begin
  if not Assigned(Source) then
    Exit;
  foriginal := Source.original;
end;

{ TACBrAppLessCobResponse }

function TACBrAppLessCobResponse.GetCalendario: TACBrAppLessCalendario;
begin
  if not Assigned(fcalendario) then
    fcalendario := TACBrAppLessCalendario.Create('calendario');
  Result := fcalendario;
end;

function TACBrAppLessCobResponse.Getpix: TACBrPIXArray;
begin
  if not Assigned(fpix) then
    fpix := TACBrPIXArray.Create('pix');
  Result := fpix;
end;

function TACBrAppLessCobResponse.GetValor: TACBrAppLessValor;
begin
  if not Assigned(fvalor) then
    fvalor := TACBrAppLessValor.Create('valor');
  Result := fvalor;
end;

procedure TACBrAppLessCobResponse.AssignSchema(aSource: TACBrPIXSchema);
begin
  if Assigned(aSource) and (aSource is TACBrAppLessCobResponse) then
    Assign(TACBrAppLessCobResponse(aSource));
end;

procedure TACBrAppLessCobResponse.DoWriteToJson(aJson: TACBrJSONObject);
begin
  if not Assigned(aJson) then
    Exit;

  aJson
    .AddPair('status', AppLessTransactionStatusToString(fstatus))
    .AddPair('urlPix', furlPix)
    .AddPair('pixCopiaECola', fpixCopiaECola)
    .AddPair('pixCopiaEColaFormatted', fpixCopiaEColaFormatted);

  if Assigned(fcalendario) then
    fcalendario.WriteToJson(aJson);

  if Assigned(fvalor) then
    fvalor.WriteToJson(aJson);

  if Assigned(fpix) then
    fpix.WriteToJSon(aJson);
end;

procedure TACBrAppLessCobResponse.DoReadFromJson(aJson: TACBrJSONObject);
var
  s: String;
begin
  {$IFDEF FPC}s := EmptyStr;{$ENDIF}
  if not Assigned(aJson) then
    Exit;

  aJson
    .Value('status', s)
    .Value('urlPix', furlPix)
    .Value('pixCopiaECola', fpixCopiaECola)
    .Value('pixCopiaEColaFormatted', fpixCopiaEColaFormatted);

  if NaoEstaVazio(s) then
    fstatus := StringToAppLessTransactionStatus(s);
  calendario.ReadFromJson(aJson);
  valor.ReadFromJson(aJson);
  pix.ReadFromJSon(aJson);
end;

destructor TACBrAppLessCobResponse.Destroy;
begin
  if Assigned(fcalendario) then
    fcalendario.Free;
  if Assigned(fvalor) then
    fvalor.Free;
  if Assigned(fpix) then
    fpix.Free;
  inherited Destroy;
end;

procedure TACBrAppLessCobResponse.Clear;
begin
  fstatus := atsNone;
  furlPix := EmptyStr;
  fpixCopiaECola := EmptyStr;
  fpixCopiaEColaFormatted := EmptyStr;

  if Assigned(fcalendario) then
    fcalendario.Clear;
  if Assigned(fvalor) then
    fvalor.Clear;
  if Assigned(fpix) then
    fpix.Clear;
end;

function TACBrAppLessCobResponse.IsEmpty: Boolean;
begin
  Result :=
    (fstatus = atsNone) and
    EstaVazio(furlPix) and
    EstaVazio(fpixCopiaECola) and
    EstaVazio(fpixCopiaEColaFormatted) and
    ((not Assigned(fcalendario)) or fcalendario.IsEmpty) and
    ((not Assigned(fvalor)) or fvalor.IsEmpty) and
    ((not Assigned(fpix)) or fpix.IsEmpty);
end;

procedure TACBrAppLessCobResponse.Assign(Source: TACBrAppLessCobResponse);
begin
  if not Assigned(Source) then
    Exit;
  fstatus := Source.status;
  furlPix := Source.urlPix;
  fpixCopiaECola := Source.pixCopiaECola;
  fpixCopiaEColaFormatted := Source.pixCopiaEColaFormatted;
  calendario.Assign(Source.calendario);
  valor.Assign(Source.valor);
  pix.Assign(Source.pix);
end;

{ TACBrAppLessTransactionPix }

function TACBrAppLessTransactionPix.GetCobResponse: TACBrAppLessCobResponse;
begin
  if not Assigned(fcobResponse) then
    fcobResponse := TACBrAppLessCobResponse.Create('cobResponse');
  Result := fcobResponse;
end;

procedure TACBrAppLessTransactionPix.AssignSchema(aSource: TACBrPIXSchema);
begin
  if Assigned(aSource) and (aSource is TACBrAppLessTransactionPix) then
    Assign(TACBrAppLessTransactionPix(aSource));
end;

procedure TACBrAppLessTransactionPix.DoWriteToJson(aJson: TACBrJSONObject);
begin
  if not Assigned(aJson) then
    Exit;

  aJson
    .AddPair('txid', ftxid)
    .AddPair('created', DateTimeToIso8601(fcreated));

  if Assigned(fcobResponse) then
    fcobResponse.WriteToJson(aJson);
end;

procedure TACBrAppLessTransactionPix.DoReadFromJson(aJson: TACBrJSONObject);
var
  s: String;
begin
  {$IFDEF FPC}s := EmptyStr;{$ENDIF}
  if not Assigned(aJson) then
    Exit;

  aJson
    .Value('txid', ftxid)
    .Value('created', s);

  if NaoEstaVazio(s) then
    fcreated := Iso8601ToDateTime(s);
  cobResponse.ReadFromJson(aJson);
end;

destructor TACBrAppLessTransactionPix.Destroy;
begin
  if Assigned(fcobResponse) then
    fcobResponse.Free;
  inherited Destroy;
end;

procedure TACBrAppLessTransactionPix.Clear;
begin
  ftxid := EmptyStr;
  fcreated := 0;
  if Assigned(fcobResponse) then
    fcobResponse.Clear;
end;

function TACBrAppLessTransactionPix.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(ftxid) and
    EstaZerado(fcreated) and
    ((not Assigned(fcobResponse)) or fcobResponse.IsEmpty);
end;

procedure TACBrAppLessTransactionPix.Assign(Source: TACBrAppLessTransactionPix);
begin
  if not Assigned(Source) then
    Exit;
  ftxid := Source.txid;
  fcreated := Source.created;
  cobResponse.Assign(Source.cobResponse);
end;

{ TACBrAppLessTransaction }

function TACBrAppLessTransaction.GetTransactionPix: TACBrAppLessTransactionPix;
begin
  if not Assigned(ftransactionPix) then
    ftransactionPix := TACBrAppLessTransactionPix.Create('transactionPix');
  Result := ftransactionPix;
end;

procedure TACBrAppLessTransaction.AssignSchema(aSource: TACBrPIXSchema);
begin
  if Assigned(aSource) and (aSource is TACBrAppLessTransaction) then
    Assign(TACBrAppLessTransaction(aSource));
end;

procedure TACBrAppLessTransaction.DoWriteToJson(aJson: TACBrJSONObject);
begin
  if not Assigned(aJson) then
    Exit;

  if Assigned(ftransactionPix) then
    ftransactionPix.WriteToJson(aJson);
end;

procedure TACBrAppLessTransaction.DoReadFromJson(aJson: TACBrJSONObject);
begin
  if not Assigned(aJson) then
    Exit;

  transactionPix.ReadFromJson(aJson);
end;

destructor TACBrAppLessTransaction.Destroy;
begin
  if Assigned(ftransactionPix) then
    ftransactionPix.Free;
  inherited Destroy;
end;

procedure TACBrAppLessTransaction.Clear;
begin
  if Assigned(ftransactionPix) then
    ftransactionPix.Clear;
end;

function TACBrAppLessTransaction.IsEmpty: Boolean;
begin
  Result := (not Assigned(ftransactionPix)) or ftransactionPix.IsEmpty;
end;

procedure TACBrAppLessTransaction.Assign(Source: TACBrAppLessTransaction);
begin
  if not Assigned(Source) then
    Exit;
  transactionPix.Assign(Source.transactionPix);
end;

{ TACBrAppLessOrderResponse }

function TACBrAppLessOrderResponse.GetTransaction: TACBrAppLessTransaction;
begin
  if not Assigned(ftransaction) then
    ftransaction := TACBrAppLessTransaction.Create('transaction');
  Result := ftransaction;
end;

procedure TACBrAppLessOrderResponse.AssignSchema(aSource: TACBrPIXSchema);
begin
  if Assigned(aSource) and (aSource is TACBrAppLessOrderResponse) then
    Assign(TACBrAppLessOrderResponse(aSource));
end;

procedure TACBrAppLessOrderResponse.DoWriteToJson(aJson: TACBrJSONObject);
begin
  if not Assigned(aJson) then
    Exit;

  aJson
    .AddPair('id', fid)
    .AddPair('orderId', forderId)
    .AddPair('customerSocialName', fcustomerSocialName)
    .AddPair('amount', famount)
    .AddPair('amountPayed', famountPayed)
    .AddPair('amountFormatted', famountFormatted)
    .AddPair('installments', finstallments)
    .AddPair('typeOrder', ftypeOrder)
    .AddPair('status', AppLessOrderStatusToString(fstatus))
    .AddPair('dtCreated', DateTimeToIso8601(fdtCreated))
    .AddPair('dtWaiting', DateTimeToIso8601(fdtWaiting))
    .AddPair('messageTransaction', fmessageTransaction)
    .AddPair('externalId', fexternalId)
    .AddPair('customerCNPJ', fcustomerCNPJ)
    .AddPair('expiration', fexpiration)
    .AddPair('txid', ftxid)
    .AddPair('createdAt', DateTimeToIso8601(fcreatedAt))
    .AddPair('companyNickName', fcompanyNickName)
    .AddPair('isStaticPix', fisStaticPix);

  if Assigned(ftransaction) then
    ftransaction.WriteToJson(aJson);
end;

procedure TACBrAppLessOrderResponse.DoReadFromJson(aJson: TACBrJSONObject);
var
  s1, s2, s3, st: String;
begin
  {$IFDEF FPC}
  st := EmptyStr;
  s1 := EmptyStr;
  s2 := EmptyStr;
  s3 := EmptyStr;
  {$ENDIF}
  if not Assigned(aJson) then
    Exit;

  aJson
    .Value('id', fid)
    .Value('orderId', forderId)
    .Value('customerSocialName', fcustomerSocialName)
    .Value('amount', famount)
    .Value('amountPayed', famountPayed)
    .Value('amountFormatted', famountFormatted)
    .Value('installments', finstallments)
    .Value('typeOrder', ftypeOrder)
    .Value('status', st)
    .Value('dtCreated', s1)
    .Value('dtWaiting', s2)
    .Value('messageTransaction', fmessageTransaction)
    .Value('externalId', fexternalId)
    .Value('customerCNPJ', fcustomerCNPJ)
    .Value('expiration', fexpiration)
    .Value('txid', ftxid)
    .Value('createdAt', s3)
    .Value('companyNickName', fcompanyNickName)
    .Value('isStaticPix', fisStaticPix);

  transaction.ReadFromJson(aJson);
  if NaoEstaVazio(st) then
    fstatus := StringToAppLessOrderStatus(st);
  if NaoEstaVazio(s1) then
    fdtCreated := Iso8601ToDateTime(s1);
  if NaoEstaVazio(s2) then
    fdtWaiting := Iso8601ToDateTime(s2);
  if NaoEstaVazio(s3) then
    fcreatedAt := Iso8601ToDateTime(s3);
end;

destructor TACBrAppLessOrderResponse.Destroy;
begin
  if Assigned(ftransaction) then
    ftransaction.Free;
  inherited Destroy;
end;

procedure TACBrAppLessOrderResponse.Clear;
begin
  fid := EmptyStr;
  forderId := EmptyStr;
  fcustomerSocialName := EmptyStr;
  famount := 0;
  famountPayed := 0;
  famountFormatted := EmptyStr;
  finstallments := 0;
  ftypeOrder := EmptyStr;
  fstatus := aosNone;
  fdtCreated := 0;
  fdtWaiting := 0;
  fmessageTransaction := EmptyStr;
  fexternalId := EmptyStr;
  fcustomerCNPJ := EmptyStr;
  fexpiration := 0;
  ftxid := EmptyStr;
  fcreatedAt := 0;
  fcompanyNickName := EmptyStr;
  fisStaticPix := False;

  if Assigned(ftransaction) then
    ftransaction.Clear;
end;

function TACBrAppLessOrderResponse.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(fid) and
    EstaVazio(forderId) and
    EstaVazio(fcustomerSocialName) and
    EstaZerado(famount) and
    EstaZerado(famountPayed) and
    EstaVazio(famountFormatted) and
    EstaZerado(finstallments) and
    EstaVazio(ftypeOrder) and
    (fstatus = aosNone) and
    EstaZerado(fdtCreated) and
    EstaZerado(fdtWaiting) and
    EstaVazio(fmessageTransaction) and
    EstaVazio(fexternalId) and
    EstaVazio(fcustomerCNPJ) and
    EstaZerado(fexpiration) and
    EstaVazio(ftxid) and
    EstaZerado(fcreatedAt) and
    EstaVazio(fcompanyNickName) and
    (not fisStaticPix) and
    ((not Assigned(ftransaction)) or ftransaction.IsEmpty);
end;

procedure TACBrAppLessOrderResponse.Assign(Source: TACBrAppLessOrderResponse);
begin
  if not Assigned(Source) then
    Exit;

  fid := Source.id;
  forderId := Source.orderId;
  fcustomerSocialName := Source.customerSocialName;
  famount := Source.amount;
  famountPayed := Source.amountPayed;
  famountFormatted := Source.amountFormatted;
  finstallments := Source.installments;
  ftypeOrder := Source.typeOrder;
  fstatus := Source.status;
  fdtCreated := Source.dtCreated;
  fdtWaiting := Source.dtWaiting;
  fmessageTransaction := Source.messageTransaction;
  fexternalId := Source.externalId;
  fcustomerCNPJ := Source.customerCNPJ;
  fexpiration := Source.expiration;
  ftxid := Source.txid;
  fcreatedAt := Source.createdAt;
  fcompanyNickName := Source.companyNickName;
  fisStaticPix := Source.isStaticPix;
  transaction.Assign(Source.transaction);
end;

{ TACBrAppLessOrders }

function TACBrAppLessOrders.GetItem(Index: Integer): TACBrAppLessOrderResponse;
begin
  Result := TACBrAppLessOrderResponse(inherited Items[Index]);
end;

procedure TACBrAppLessOrders.SetItem(Index: Integer; Value: TACBrAppLessOrderResponse);
begin
  inherited Items[Index] := Value;
end;

function TACBrAppLessOrders.NewSchema: TACBrPIXSchema;
begin
  Result := New;
end;

function TACBrAppLessOrders.Add(AItem: TACBrAppLessOrderResponse): Integer;
begin
  Result := inherited Add(AItem);
end;

procedure TACBrAppLessOrders.Insert(Index: Integer; Value: TACBrAppLessOrderResponse);
begin
  inherited Insert(Index, Value);
end;

function TACBrAppLessOrders.New: TACBrAppLessOrderResponse;
begin
  Result := TACBrAppLessOrderResponse.Create;
  Self.Add(Result);
end;

{ TACBrAppLessOrdersResponse }

function TACBrAppLessOrdersResponse.Getresults: TACBrAppLessOrders;
begin
  if (not Assigned(fresults)) then
    fresults := TACBrAppLessOrders.Create('results');
  Result := fresults;
end;

procedure TACBrAppLessOrdersResponse.AssignSchema(aSource: TACBrPIXSchema);
begin
  if Assigned(aSource) and (aSource is TACBrAppLessOrdersResponse) then
    Assign(TACBrAppLessOrdersResponse(aSource));
end;

procedure TACBrAppLessOrdersResponse.DoWriteToJson(aJson: TACBrJSONObject);
begin
  if not Assigned(aJson) then
    Exit;

  aJson
    .AddPair('range', frange)
    .AddPair('skip', fskip)
    .AddPair('count', fcount);
  if Assigned(fresults) then
    fresults.WriteToJSon(aJson);
end;

procedure TACBrAppLessOrdersResponse.DoReadFromJson(aJson: TACBrJSONObject);
begin
  if not Assigned(aJson) then
    Exit;

  aJson
    .Value('range', frange)
    .Value('skip', fskip)
    .Value('count', fcount);
  results.ReadFromJSon(aJson);
end;

destructor TACBrAppLessOrdersResponse.Destroy;
begin
  if Assigned(fresults) then
    fresults.Free;
  inherited Destroy;
end;

procedure TACBrAppLessOrdersResponse.Clear;
begin
  frange := 0;
  fskip := 0;
  fcount := 0;
  if Assigned(fresults) then
    fresults.Clear;
end;

function TACBrAppLessOrdersResponse.IsEmpty: Boolean;
begin
  Result :=
    EstaZerado(frange) and
    EstaZerado(fskip) and
    EstaZerado(fcount) and
    ((not Assigned(fresults)) or fresults.IsEmpty);
end;

procedure TACBrAppLessOrdersResponse.Assign(aSource: TACBrAppLessOrdersResponse);
begin
  if not Assigned(aSource) then
    Exit;

  frange := aSource.range;
  fskip := aSource.skip;
  fcount := aSource.count;
  results.Assign(aSource.results);
end;

{ TACBrAppLessBankSlipOrderRequest }

function TACBrAppLessBankSlipOrderRequest.GetOrder: TACBrAppLessBankSlipOrder;
begin
  if not Assigned(forder) then
    forder := TACBrAppLessBankSlipOrder.Create('order');
  Result := forder;
end;

function TACBrAppLessBankSlipOrderRequest.GetBankSlip: TACBrAppLessBankSlip;
begin
  if not Assigned(fbankSlip) then
    fbankSlip := TACBrAppLessBankSlip.Create('bankSlip');
  Result := fbankSlip;
end;

procedure TACBrAppLessBankSlipOrderRequest.AssignSchema(aSource: TACBrPIXSchema);
begin
  if Assigned(aSource) and (aSource is TACBrAppLessBankSlipOrderRequest) then
    Assign(TACBrAppLessBankSlipOrderRequest(aSource));
end;

procedure TACBrAppLessBankSlipOrderRequest.DoWriteToJson(aJson: TACBrJSONObject);
begin
  if not Assigned(aJson) then
    Exit;

  if Assigned(forder) then
    forder.WriteToJson(aJson);

  if Assigned(fbankSlip) then
    fbankSlip.WriteToJson(aJson);
end;

procedure TACBrAppLessBankSlipOrderRequest.DoReadFromJson(aJson: TACBrJSONObject);
begin
  if not Assigned(aJson) then
    Exit;

  order.ReadFromJson(aJson);
  bankSlip.ReadFromJson(aJson);
end;

destructor TACBrAppLessBankSlipOrderRequest.Destroy;
begin
  if Assigned(forder) then
    forder.Free;

  if Assigned(fbankSlip) then
    fbankSlip.Free;

  inherited Destroy;
end;

procedure TACBrAppLessBankSlipOrderRequest.Clear;
begin
  if Assigned(forder) then
    forder.Clear;

  if Assigned(fbankSlip) then
    fbankSlip.Clear;
end;

function TACBrAppLessBankSlipOrderRequest.IsEmpty: Boolean;
begin
  Result := 
    (not Assigned(forder) or forder.IsEmpty) and
    (not Assigned(fbankSlip) or fbankSlip.IsEmpty);
end;

procedure TACBrAppLessBankSlipOrderRequest.Assign(Source: TACBrAppLessBankSlipOrderRequest);
begin
  if not Assigned(Source) then
    Exit;

  order.Assign(Source.order);
  bankSlip.Assign(Source.bankSlip);
end;

end.
