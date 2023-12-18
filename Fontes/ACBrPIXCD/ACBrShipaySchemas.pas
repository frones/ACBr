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

(*

  Documentação
  https://api-staging.shipay.com.br/docs.html

*)

{$I ACBr.inc}

unit ACBrShipaySchemas;

interface

uses
  Classes, SysUtils, ACBrPIXBase, ACBrPIXSchemasCobV, ACBrJSON;

type

  TShipayOrderStatus = (
    spsNone,
    spsPending,         // Pedido aberto e ainda não pago ou cancelado
    spsPendingV,        // Pedido aberto e ainda não pago ou cancelado (para pedidos com vencimento criados através do POST /orderv)
    spsApproved,        // Pedido aprovado na carteira digital
    spsCancelled,       // Pedido (ainda não pago) cancelado na carteira digital
    spsExpired,         // Pedido expirado após 60 minutos com status "pending"
    spsRefunded,        // Pagamento devolvido ao comprador
    spsRefundPending,   // Pagamento com devolução solicitada. Status aplicável para PIX e para a carteira digital Cielo Pay pois a ação de devolução não é síncrona nestes casos. No caso de PIX a devolução deve ser efetivada em até 90 segudos após a solicitação. No caso da Cielo Pay, a devolução ocorre sempre no dia seguinte à solicitação. Em ambos os casos, quando a devolução é efetivada, o status na Shipay é alterado para "refunded"
    spsPartial_Refunded // Pagamento Parcialmente devolvido
  );

  { TShipayWallet }

  TShipayWallet = class(TACBrPIXSchema)
  private
    fActive: Boolean;
    ffriendly_name: String;
    flogo: String;
    fminimum_payment: Currency;
    fpix_dict_key: String;
    fpix_psp: String;
    fwallet: String;
    fwallet_setting_id: String;
    fwallet_setting_name: String;
  protected
    procedure AssignSchema(ASource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TShipayWallet);

    property Active: Boolean read fActive write fActive;
    property friendly_name: String read ffriendly_name write ffriendly_name;
    property logo: String read flogo write flogo;
    property minimum_payment: Currency read fminimum_payment write fminimum_payment;
    property pix_dict_key: String read fpix_dict_key write fpix_dict_key;
    property pix_psp: String read fpix_psp write fpix_psp;
    property wallet: String read fwallet write fwallet;
    property wallet_setting_id: String read fwallet_setting_id write fwallet_setting_id;
    property wallet_setting_name: String read fwallet_setting_name write fwallet_setting_name;
  end;

  { TShipayWalletArray }

  TShipayWalletArray = class(TACBrPIXSchemaArray)
  private
    function GetItem(Index: Integer): TShipayWallet;
    procedure SetItem(Index: Integer; Value: TShipayWallet);
  protected
    function NewSchema: TACBrPIXSchema; override;
  public
    Function Add(AItem: TShipayWallet): Integer;
    Procedure Insert(Index: Integer; AItem: TShipayWallet);
    function New: TShipayWallet;
    property Items[Index: Integer]: TShipayWallet read GetItem write SetItem; default;
  end;

  { TShipayItem }

  TShipayItem = class(TACBrPIXSchema)
  private
    fean: String;
    fitem_title: String;
    fname: String;
    fquantity: Double;
    fsku: String;
    funit_price: Currency;
  protected
    procedure AssignSchema(ASource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TShipayItem);

    property ean: String read fean write fean;
    property item_title: String read fitem_title write fitem_title;
    property quantity: Double read fquantity write fquantity;
    property sku: String read fsku write fsku;
    property unit_price: Currency read funit_price write funit_price;
    property name: String read fname write fname;
  end;


  { TShipayItemArray }

  TShipayItemArray = class(TACBrPIXSchemaArray)
  private
    function GetItem(Index: Integer): TShipayItem;
    procedure SetItem(Index: Integer; Value: TShipayItem);
  protected
    function NewSchema: TACBrPIXSchema; override;
  public
    Function Add(AItem: TShipayItem): Integer;
    Procedure Insert(Index: Integer; AItem: TShipayItem);
    function New: TShipayItem;
    property Items[Index: Integer]: TShipayItem read GetItem write SetItem; default;
  end;

  { TShipayBuyer }

  TShipayBuyer = class(TACBrPIXSchema)
  private
    fcpf_cnpj: String;
    femail: String;
    fname: String;
    fphone: String;
    procedure Setcpf_cnpj(AValue: String);
    procedure Setemail(AValue: String);
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String = ''); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TShipayBuyer);

    property cpf_cnpj: String read fcpf_cnpj write Setcpf_cnpj;
    property email: String read femail write Setemail;
    property name: String read fname write fname;
    property phone: String read fphone write fphone;
  end;

  { TShipayOrderError }

  TShipayOrderError = class(TACBrPIXSchema)
  private
    fcode: Integer;
    fmessage: String;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String = ''); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TShipayOrderError);

    property code: Integer read fcode write fcode;
    property message: String read fmessage write fmessage;
  end;

  { TShipayOrder }

  TShipayOrder = class(TACBrPIXSchema)
  private
    fbuyer: TShipayBuyer;
    fcallback_url: String;
    fexpiration: Integer;
    fitems: TShipayItemArray;
    forder_ref: String;
    fpix_dict_key: String;
    ftotal: Currency;
    fwallet: String;
    procedure Setpix_dict_key(AValue: String);
    procedure Setwallet(AValue: String);
  protected
    procedure AssignSchema(ASource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TShipayOrder);

    property buyer: TShipayBuyer read fbuyer;
    property callback_url: String read fcallback_url write fcallback_url;
    property expiration: Integer read fexpiration write fexpiration;
    property items: TShipayItemArray read fitems;
    property order_ref: String read forder_ref write forder_ref;
    property pix_dict_key: String read fpix_dict_key write Setpix_dict_key;
    property total: Currency read ftotal write ftotal;
    property wallet: String read fwallet write Setwallet;
  end;

  { TShipayOrderBase }

  TShipayOrderBase = class(TACBrPIXSchema)
  private
    forder_id: String;
    fstatus: TShipayOrderStatus;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String = ''); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TShipayOrderBase);

    property order_id: String read forder_id write forder_id;
    property status: TShipayOrderStatus read fstatus write fstatus;
  end;

  { TShipayOrderCreated }

  TShipayOrderCreated = class(TShipayOrderBase)
  private
    fdeep_link: String;
    fexpiration_date: TDateTime;
    fpix_dict_key: String;
    fpix_psp: String;
    fqr_code: String;
    fqr_code_text: String;
    fwallet: String;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String = ''); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TShipayOrderCreated);

    property deep_link: String read fdeep_link write fdeep_link;
    property expiration_date: TDateTime read fexpiration_date write fexpiration_date;
    property pix_dict_key: String read fpix_dict_key write fpix_dict_key;
    property pix_psp: String read fpix_psp write fpix_psp;
    property qr_code: String read fqr_code write fqr_code;
    property qr_code_text: String read fqr_code_text write fqr_code_text;
    property wallet: String read fwallet write fwallet;
  end;

  { TShipayOrderInfo }

  TShipayOrderInfo = class(TShipayOrderBase)
  private
    fbalance: Currency;
    fcreated_at: TDateTime;
    fexpiration_date: TDateTime;
    fexternal_id: String;
    fitems: TShipayItemArray;
    fmessage: String;
    fpaid_amount: Currency;
    fpayment_date: TDateTime;
    fpix_psp: String;
    ftotal_order: Currency;
    fupdated_at: TDateTime;
    fwallet: String;
    fwallet_payment_id: String;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TShipayOrderInfo);

    property balance: Currency read fbalance write fbalance;
    property created_at: TDateTime read fcreated_at write fcreated_at;
    property expiration_date: TDateTime read fexpiration_date write fexpiration_date;
    property external_id: String read fexternal_id write fexternal_id;
    property items: TShipayItemArray read fitems;
    property paid_amount: Currency read fpaid_amount write fpaid_amount;
    property payment_date: TDateTime read fpayment_date write fpayment_date;
    property pix_psp: String read fpix_psp write fpix_psp;
    property total_order: Currency read ftotal_order write ftotal_order;
    property updated_at: TDateTime read fupdated_at write fupdated_at;
    property wallet: String read fwallet write fwallet;
    property wallet_payment_id: String read fwallet_payment_id write fwallet_payment_id;
    property message: String read fmessage write fmessage;
  end;

  { TShipayOrderData }

  TShipayOrderData = class(TShipayOrderBase)
  private
    fcustomer_id: String;
    fcustomer_name: String;
    forder_created_at: TDateTime;
    forder_expiration_date: TDateTime;
    forder_payment_date: TDateTime;
    forder_updated_at: TDateTime;
    fstore_id: String;
    fstore_name: String;
    fstore_pos_id: String;
    fstore_pos_name: String;
    ftotal_order: Currency;
    fwallet_payment_id: String;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
    procedure AssignSchema(ASource: TACBrPIXSchema); override;
  public
    constructor Create(const ObjectName: String = ''); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TShipayOrderData);

    property customer_id: String read fcustomer_id write fcustomer_id;
    property customer_name: String read fcustomer_name write fcustomer_name;
    property order_created_at: TDateTime read forder_created_at write forder_created_at;
    property order_expiration_date: TDateTime read forder_expiration_date write forder_expiration_date;
    property order_payment_date: TDateTime read forder_payment_date write forder_payment_date;
    property order_updated_at: TDateTime read forder_updated_at write forder_updated_at;
    property store_id: String read fstore_id write fstore_id;
    property store_name: String read fstore_name write fstore_name;
    property store_pos_id: String read fstore_pos_id write fstore_pos_id;
    property store_pos_name: String read fstore_pos_name write fstore_pos_name;
    property total_order: Currency read ftotal_order write ftotal_order;
    property wallet_payment_id: String read fwallet_payment_id write fwallet_payment_id;
  end;

  { TShipayOrderDataArray }

  TShipayOrderDataArray = class(TACBrPIXSchemaArray)
  private
    function GetItem(Index: Integer): TShipayOrderData;
    procedure SetItem(Index: Integer; Value: TShipayOrderData);
  protected
    function NewSchema: TACBrPIXSchema; override;
  public
    Function Add(AItem: TShipayOrderData): Integer;
    Procedure Insert(Index: Integer; AItem: TShipayOrderData);
    function New: TShipayOrderData;
    property Items[Index: Integer]: TShipayOrderData read GetItem write SetItem; default;
  end;

  { TShipayOrdersList }

  TShipayOrdersList = class(TACBrPIXSchema)
  private
    fcount: integer;
    fdata: TShipayOrderDataArray;
    foffset: integer;
    ftotal: integer;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TShipayOrdersList);

    property count: integer read fcount write fcount;
    property data: TShipayOrderDataArray read fdata;
    property offset: integer read foffset write foffset;
    property total: integer read ftotal write ftotal;
  end;

  { TShipayAmountDiscountDate }

  TShipayAmountDiscountDate = class(TACBrPIXDescontoDataFixa)
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  end;

  { TShipayAmountDiscountList }

  TShipayAmountDiscountList = class(TACBrPIXDescontosDataFixa)
  private
    function GetItem(Index: Integer): TShipayAmountDiscountDate; reintroduce;
    procedure SetItem(Index: Integer; aValue: TShipayAmountDiscountDate);  reintroduce;
  public
    function New: TShipayAmountDiscountDate;
    property Items[Index: Integer]: TShipayAmountDiscountDate read GetItem write SetItem; default;
  end;

  { TShipayAmountDetailsDiscount }

  TShipayAmountDetailsDiscount = class(TACBrPIXDesconto)
  private
    function GetDiscountList: TShipayAmountDiscountList;
  protected
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String); override;
    property descontosDataFixa: TShipayAmountDiscountList read GetDiscountList;
  end;

  { TShipayAmountDetailsValue }

  TShipayAmountDetailsValue = class(TACBrPIXModalidadeValor)
  protected
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
  end;

  { TShipayAmountDetailsInterest }

  TShipayAmountDetailsInterest = class(TACBrPIXJuros)
  protected
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  end;

  { TShipayAmountDetails }

  TShipayAmountDetails = class(TACBrPIXSchema)
  private
    fdiscount: TShipayAmountDetailsDiscount;  // Descontos aplicados à cobrança
    ffine: TShipayAmountDetailsValue;         // Multa aplicada à cobrança
    finterest: TShipayAmountDetailsInterest;  // Juros aplicados à cobrança
    frebate: TShipayAmountDetailsValue;       // Abatimento aplicado à cobrança
  protected
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TShipayAmountDetails);

    property discount: TShipayAmountDetailsDiscount read fdiscount write fdiscount;
    property fine: TShipayAmountDetailsValue read ffine write ffine;
    property interest: TShipayAmountDetailsInterest read finterest write finterest;
    property rebate: TShipayAmountDetailsValue read frebate write frebate;
  end;

  { TShipayCalendar }

  TShipayCalendar = class(TACBrPIXCalendarioCobVBase)
  protected
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    property dataDeVencimento;
    property validadeAposVencimento;
  end;

  { TShipayOrderDueDate }

  TShipayOrderDueDate = class(TShipayOrder)
  private
    famount_details: TShipayAmountDetails;
    fcalendar: TShipayCalendar;
  protected
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public                                            
    constructor Create(const aObjectName: String = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TShipayOrderDueDate);

    property amount_details: TShipayAmountDetails read famount_details write famount_details;
    property calendar: TShipayCalendar read fcalendar write fcalendar;
  end;


  function ShipayOrderStatusToString(AStatus: TShipayOrderStatus): String;
  function StringToShipayOrderStatus(const AString: String): TShipayOrderStatus;

implementation

uses
  synautil,
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.DateTime, ACBrValidador;

function ShipayOrderStatusToString(AStatus: TShipayOrderStatus): String;
begin
  case AStatus of
    spsPending: Result := 'pending';
    spsPendingV: Result := 'pendingv';
    spsApproved: Result := 'approved';
    spsCancelled: Result := 'cancelled';
    spsExpired: Result := 'expired';
    spsRefunded: Result := 'refunded';
    spsRefundPending: Result := 'refund_pending';
    spsPartial_Refunded: Result := 'partial_refunded';
  else
    Result := '';
  end;
end;

function StringToShipayOrderStatus(const AString: String): TShipayOrderStatus;
var
  s: String;
begin
  s := LowerCase(Trim(AString));
  if (s = 'pending') then
    Result := spsPending
  else if (s = 'pendingv') then
    Result := spsPendingV
  else if (s = 'approved') then
    Result := spsApproved
  else if (s = 'cancelled') then
    Result := spsCancelled
  else if (s = 'expired') then
    Result := spsExpired
  else if (s = 'refunded') then
    Result := spsRefunded
  else if (s = 'refund_pending') then
    Result := spsRefundPending
  else if (s = 'partial_refunded') then
    Result := spsPartial_Refunded
  else
    Result := spsNone;
end;

{ TShipayAmountDiscountDate }

procedure TShipayAmountDiscountDate.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('date', FormatDateTime('yyyy-mm-dd', data))
    .AddPair('value', valorPerc, False);
end;

procedure TShipayAmountDiscountDate.DoReadFromJSon(AJSon: TACBrJSONObject);
var
  d: TDateTime;
  c: Currency;
begin
  {$IfDef FPC}
  d := 0;
  c := 0;
  {$EndIf}

  AJSon
    .ValueISODate('date', d)
    .Value('value', c);
  data := d;
  valorPerc := c;
end;

{ TShipayAmountDiscountList }

function TShipayAmountDiscountList.GetItem(Index: Integer): TShipayAmountDiscountDate;
begin
  Result := TShipayAmountDiscountDate(inherited Items[Index]);
end;

procedure TShipayAmountDiscountList.SetItem(Index: Integer; aValue: TShipayAmountDiscountDate);
begin
  inherited Items[Index] := aValue;
end;

function TShipayAmountDiscountList.New: TShipayAmountDiscountDate;
begin
  Result := TShipayAmountDiscountDate.Create;
  Self.Add(Result);
end;

{ TShipayOrderDueDate }

constructor TShipayOrderDueDate.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  famount_details := TShipayAmountDetails.Create('amount_details');
  fcalendar := TShipayCalendar.Create('calendar');
end;

destructor TShipayOrderDueDate.Destroy;
begin
  amount_details.Free;
  calendar.Free;
  inherited Destroy;
end;

procedure TShipayOrderDueDate.Clear;
begin
  if Assigned(amount_details) then
    amount_details.Clear;
  if Assigned(calendar) then
    calendar.Clear;
  inherited Clear;
end;

function TShipayOrderDueDate.IsEmpty: Boolean;
begin
  Result := (inherited IsEmpty) and amount_details.IsEmpty and calendar.IsEmpty;
end;

procedure TShipayOrderDueDate.Assign(aSource: TShipayOrderDueDate);
begin
  inherited Assign(aSource);
  amount_details.Assign(aSource.amount_details);
  calendar.Assign(aSource.calendar);
end;

procedure TShipayOrderDueDate.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  inherited DoWriteToJSon(aJSon);
  amount_details.WriteToJSon(aJSon);
  calendar.WriteToJSon(aJSon);
end;

procedure TShipayOrderDueDate.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  inherited DoReadFromJSon(aJSon);
  amount_details.ReadFromJSon(aJSon);
  calendar.ReadFromJSon(aJSon);
end;

{ TShipayCalendar }

procedure TShipayCalendar.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('due_date', FormatDateTime('yyyy-mm-dd', dataDeVencimento))
    .AddPair('days_valid_after_due', validadeAposVencimento);
end;

procedure TShipayCalendar.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  s: String;
  i: Integer;
begin
  {$IfDef FPC}
  i := 0;
  s := EmptyStr;
  {$EndIf}

  aJSon
    .Value('due_date', s)
    .Value('days_valid_after_due', i);

  dataDeVencimento := Iso8601ToDateTime(s);
  validadeAposVencimento := i;
end;

{ TShipayAmountDetailsInterest }

procedure TShipayAmountDetailsInterest.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  if (modalidade = pjmNenhum) then
    Exit;

  aJSon
    .AddPair('modality', Ord(modalidade))
    .AddPair('value', valorPerc);
end;

procedure TShipayAmountDetailsInterest.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  i: Integer;
  c: Currency;
begin
  {$IfDef FPC}
  c := 0;
  i := 0;
  {$EndIf}

  aJSon
    .Value('modality', i)
    .Value('value', c);

  modalidade := TACBrPIXJurosModalidade(i);
  valorPerc := c;
end;

{ TShipayWallet }

constructor TShipayWallet.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

procedure TShipayWallet.Clear;
begin
  fActive := False;
  ffriendly_name := '';
  flogo := '';
  fminimum_payment := 0;
  fpix_dict_key := '';
  fpix_psp := '';
  fwallet := '';
  fwallet_setting_id := '';
  fwallet_setting_name := '';
end;

function TShipayWallet.IsEmpty: Boolean;
begin
  Result := (ffriendly_name = '') and
            (flogo = '') and
            (fminimum_payment = 0) and
            (fpix_dict_key = '') and
            (fpix_psp = '') and
            (fwallet = '') and
            (fwallet_setting_id = '') and
            (fwallet_setting_name = '');
end;

procedure TShipayWallet.Assign(Source: TShipayWallet);
begin
  fActive := Source.Active;
  ffriendly_name := Source.friendly_name;
  flogo := Source.logo;
  fminimum_payment := Source.minimum_payment;
  fpix_dict_key := Source.pix_dict_key;
  fpix_psp := Source.pix_psp;
  fwallet := Source.wallet;
  fwallet_setting_id := Source.wallet_setting_id;
  fwallet_setting_name := Source.wallet_setting_name;
end;

procedure TShipayWallet.AssignSchema(ASource: TACBrPIXSchema);
begin
  if (ASource is TShipayWallet) then
    Assign(TShipayWallet(ASource));
end;

procedure TShipayWallet.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('Active', fActive)
    .AddPair('friendly_name', ffriendly_name)
    .AddPair('logo', flogo)
    .AddPair('minimum_payment', fminimum_payment)
    .AddPair('pix_dict_key', fpix_dict_key)
    .AddPair('pix_psp', fpix_psp)
    .AddPair('wallet', fwallet)
    .AddPair('wallet_setting_id', fwallet_setting_id)
    .AddPair('wallet_setting_name', fwallet_setting_name)
end;

procedure TShipayWallet.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .Value('Active', fActive)
    .Value('friendly_name', ffriendly_name)
    .Value('logo', flogo)
    .Value('minimum_payment', fminimum_payment)
    .Value('pix_dict_key', fpix_dict_key)
    .Value('pix_psp', fpix_psp)
    .Value('wallet', fwallet)
    .Value('wallet_setting_id', fwallet_setting_id)
    .Value('wallet_setting_name', fwallet_setting_name);
end;

{ TShipayWalletArray }

function TShipayWalletArray.GetItem(Index: Integer): TShipayWallet;
begin
  Result := TShipayWallet(inherited Items[Index]);
end;

procedure TShipayWalletArray.SetItem(Index: Integer; Value: TShipayWallet);
begin
  inherited Items[Index] := Value;
end;

function TShipayWalletArray.NewSchema: TACBrPIXSchema;
begin
  Result := New;
end;

function TShipayWalletArray.Add(AItem: TShipayWallet): Integer;
begin
  Result := inherited Add(AItem);
end;

procedure TShipayWalletArray.Insert(Index: Integer; AItem: TShipayWallet);
begin
  inherited Insert(Index, AItem);
end;

function TShipayWalletArray.New: TShipayWallet;
begin
  Result := TShipayWallet.Create('');
  Self.Add(Result);
end;

{ TShipayItem }

constructor TShipayItem.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

procedure TShipayItem.Clear;
begin
  fean := '';
  fitem_title := '';
  fquantity := 0;
  fsku := '';
  funit_price := 0;
  fname := '';
end;

function TShipayItem.IsEmpty: Boolean;
begin
  Result := (fean = '') and
            (fitem_title = '') and
            (fquantity = 0) and
            (fsku = '') and
            (funit_price = 0) and
            (fname = '');
end;

procedure TShipayItem.Assign(Source: TShipayItem);
begin
  fean := Source.ean;
  fitem_title := Source.item_title;
  fquantity := Source.quantity;
  fsku := Source.sku;
  funit_price := Source.unit_price;
  fname := Source.name;
end;

procedure TShipayItem.AssignSchema(ASource: TACBrPIXSchema);
begin
  if (ASource is TShipayItem) then
    Assign(TShipayItem(ASource));
end;

procedure TShipayItem.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('ean', fean)
    .AddPair('item_title', fitem_title)
    .AddPair('quantity', fquantity)
    .AddPair('sku', fsku)
    .AddPair('unit_price', funit_price)
    .AddPair('name', fname, False);
end;

procedure TShipayItem.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .Value('ean', fean)
    .Value('item_title', fitem_title)
    .Value('quantity', fquantity)
    .Value('sku', fsku)
    .Value('unit_price', funit_price)
    .Value('name', fname);
end;

{ TShipayItemArray }

function TShipayItemArray.GetItem(Index: Integer): TShipayItem;
begin
  Result := TShipayItem(inherited Items[Index]);
end;

procedure TShipayItemArray.SetItem(Index: Integer; Value: TShipayItem);
begin
  inherited Items[Index] := Value;
end;

function TShipayItemArray.NewSchema: TACBrPIXSchema;
begin
  Result := New;
end;

function TShipayItemArray.Add(AItem: TShipayItem): Integer;
begin
  Result := inherited Add(AItem);
end;

procedure TShipayItemArray.Insert(Index: Integer; AItem: TShipayItem);
begin
  inherited Insert(Index, AItem);
end;

function TShipayItemArray.New: TShipayItem;
begin
  Result := TShipayItem.Create('');
  Self.Add(Result);
end;

{ TShipayBuyer }

constructor TShipayBuyer.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

procedure TShipayBuyer.Clear;
begin
  fcpf_cnpj := '';
  femail := '';
  fname := '';
  fphone := '';
end;

function TShipayBuyer.IsEmpty: Boolean;
begin
  Result := (fcpf_cnpj = '') and
            (femail = '') and
            (fname = '') and
            (fphone = '');
end;

procedure TShipayBuyer.Assign(Source: TShipayBuyer);
begin
  fcpf_cnpj := Source.cpf_cnpj;
  femail := Source.email;
  fname := Source.name;
  fphone := Source.phone;
end;

procedure TShipayBuyer.Setcpf_cnpj(AValue: String);
var
  s, e: String;
begin
  if fcpf_cnpj = AValue then
    Exit;

  s := OnlyNumber(AValue);
  if (s <> '') then
  begin
    e := ValidarCNPJouCPF(s);
    if (e <> '') then
      raise EACBrPixException.Create(ACBrStr(e));
  end;

  fcpf_cnpj := s;
end;

procedure TShipayBuyer.Setemail(AValue: String);
var
  s, e: String;
begin
  if femail = AValue then
    Exit;

  s := Trim(AValue);
  if (s <> '') then
  begin
    e := ValidarEmail(s);
    if (e <> '') then
      raise EACBrPixException.Create(ACBrStr(e));
  end;

  femail := s;
end;

procedure TShipayBuyer.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('cpf_cnpj', fcpf_cnpj, False)
    .AddPair('email', femail, False)
    .AddPair('name', fname)
    .AddPair('phone', fphone, False);
end;

procedure TShipayBuyer.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .Value('cpf_cnpj', fcpf_cnpj)
    .Value('email', femail)
    .Value('name', fname)
    .Value('phone', fphone);
end;

{ TShipayOrderError }

constructor TShipayOrderError.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

procedure TShipayOrderError.Clear;
begin
  fcode := 0;
  fmessage := EmptyStr;
end;

function TShipayOrderError.IsEmpty: Boolean;
begin
  Result := (fcode = 0) and EstaVazio(fmessage);
end;

procedure TShipayOrderError.Assign(aSource: TShipayOrderError);
begin
  fcode := aSource.code;
  fmessage := aSource.message;
end;

procedure TShipayOrderError.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('code', fcode)
    .AddPair('message', fmessage);
end;

procedure TShipayOrderError.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .Value('code', fcode)
    .Value('message', fmessage);
end;

{ TShipayOrder }

constructor TShipayOrder.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  fbuyer := TShipayBuyer.Create('buyer');
  fitems := TShipayItemArray.Create('items');
  Clear;
end;

destructor TShipayOrder.Destroy;
begin
  fbuyer.Free;
  fitems.Free;
  inherited Destroy;
end;

procedure TShipayOrder.Clear;
begin
  fbuyer.Clear;
  fitems.Clear;
  fcallback_url := '';
  fexpiration := 0;
  forder_ref := '';
  fpix_dict_key := '';
  ftotal := 0;
  fwallet := '';
end;

function TShipayOrder.IsEmpty: Boolean;
begin
  Result := fbuyer.IsEmpty and
            fitems.IsEmpty and
            (fcallback_url = '') and
            (fexpiration = 0) and
            (forder_ref = '') and
            (fpix_dict_key = '') and
            (ftotal = 0) and
            (fwallet = '');
end;

procedure TShipayOrder.Assign(Source: TShipayOrder);
begin
  fbuyer.Assign(Source.buyer);
  fitems.Assign(Source.items);
  fcallback_url := Source.callback_url;
  fexpiration := Source.expiration;
  forder_ref := Source.order_ref;
  fpix_dict_key := Source.pix_dict_key;
  ftotal := Source.total;
  fwallet := Source.wallet;
end;

procedure TShipayOrder.Setpix_dict_key(AValue: String);
begin
  if fpix_dict_key = AValue then Exit;
  fpix_dict_key := AValue;
end;

procedure TShipayOrder.Setwallet(AValue: String);
begin
  fwallet := LowerCase(Trim(AValue));
end;

procedure TShipayOrder.AssignSchema(ASource: TACBrPIXSchema);
begin
  if (ASource is TShipayOrder) then
    Assign(TShipayOrder(ASource));
end;

procedure TShipayOrder.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  fbuyer.WriteToJSon(AJSon);
  fitems.WriteToJSon(AJSon);

  AJSon
    .AddPair('callback_url', fcallback_url, False)
    .AddPair('expiration', fexpiration, False)
    .AddPair('order_ref', forder_ref)
    .AddPair('pix_dict_key', fpix_dict_key, False)
    .AddPair('total', ftotal)
    .AddPair('wallet', fwallet);
end;

procedure TShipayOrder.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  fbuyer.ReadFromJSon(AJSon);
  fitems.ReadFromJSon(AJSon);

  AJSon
    .Value('callback_url', fcallback_url)
    .Value('expiration', fexpiration)
    .Value('order_ref', forder_ref)
    .Value('pix_dict_key', fpix_dict_key)
    .Value('total', ftotal)
    .Value('wallet', fwallet);
end;

{ TShipayOrderBase }

constructor TShipayOrderBase.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

procedure TShipayOrderBase.Clear;
begin
  forder_id := '';
  fstatus := spsNone;
end;

function TShipayOrderBase.IsEmpty: Boolean;
begin
  Result := (forder_id = '') and
            (fstatus = spsNone);
end;

procedure TShipayOrderBase.Assign(Source: TShipayOrderBase);
begin
  forder_id := Source.order_id;
  fstatus := Source.status;
end;

procedure TShipayOrderBase.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('order_id', forder_id)
    .AddPair('status', ShipayOrderStatusToString(fstatus));
end;

procedure TShipayOrderBase.DoReadFromJSon(AJSon: TACBrJSONObject);
var
  s: String;
begin
  {$IfDef FPC}s := EmptyStr;{$EndIf}
  AJSon
    .Value('order_id', forder_id)
    .Value('status', s);

  fstatus := StringToShipayOrderStatus(s);
end;

{ TShipayOrderCreated }

constructor TShipayOrderCreated.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

procedure TShipayOrderCreated.Clear;
begin
  inherited Clear;
  fdeep_link := '';
  fexpiration_date := 0;
  fpix_dict_key := '';
  fpix_psp := '';
  fqr_code := '';
  fqr_code_text := '';
  fwallet := '';
end;

function TShipayOrderCreated.IsEmpty: Boolean;
begin
  Result := inherited IsEmpty and
            (fdeep_link = '') and
            (fexpiration_date = 0) and
            (fpix_dict_key = '') and
            (fpix_psp = '') and
            (fqr_code = '') and
            (fqr_code_text = '') and
            (fwallet = '');
end;

procedure TShipayOrderCreated.Assign(Source: TShipayOrderCreated);
begin
  inherited Assign(Source);
  fdeep_link := Source.deep_link;
  fexpiration_date := Source.expiration_date;
  fpix_dict_key := Source.pix_dict_key;
  fpix_psp := Source.pix_psp;
  fqr_code := Source.qr_code;
  fqr_code_text := Source.qr_code_text;
  fwallet := Source.wallet;
end;

procedure TShipayOrderCreated.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  inherited DoWriteToJSon(AJSon);

  AJSon
    .AddPair('deep_link', fdeep_link, False)
    .AddPairISODateTime('expiration_date', fexpiration_date)
    .AddPair('pix_dict_key', fpix_dict_key, False)
    .AddPair('pix_psp', fpix_psp, False)
    .AddPair('qr_code', fqr_code)
    .AddPair('qr_code_text', fqr_code_text)
    .AddPair('wallet', fwallet);
end;

procedure TShipayOrderCreated.DoReadFromJSon(AJSon: TACBrJSONObject);
var
  s: String;
begin
  inherited DoReadFromJSon(AJSon);
  
  {$IfDef FPC}s := EmptyStr;{$EndIf}

  AJSon
    .Value('deep_link', fdeep_link)
    .Value('expiration_date', s)
    .Value('pix_dict_key', fpix_dict_key)
    .Value('pix_psp', fpix_psp)
    .Value('qr_code', fqr_code)
    .Value('qr_code_text', fqr_code_text)
    .Value('wallet', fwallet);

  fexpiration_date := DecodeRfcDateTime(StringReplace(s, 'T', ' ', [rfReplaceAll]));
end;

{ TShipayOrderInfo }

constructor TShipayOrderInfo.Create(const ObjectName: String);
begin
  fitems := TShipayItemArray.Create('items');
  inherited Create(ObjectName);
end;

destructor TShipayOrderInfo.Destroy;
begin
  fitems.Free;
  inherited Destroy;
end;

procedure TShipayOrderInfo.Clear;
begin
  inherited Clear;
  fbalance := 0;
  fcreated_at := 0;
  fexpiration_date := 0;
  fexternal_id := '';
  fitems.Clear;
  fpaid_amount := 0;
  fpayment_date := 0;
  fpix_psp := '';
  ftotal_order := 0;
  fupdated_at := 0;
  fwallet := '';
  fwallet_payment_id := '';
  fmessage := '';
end;

function TShipayOrderInfo.IsEmpty: Boolean;
begin
  Result := inherited IsEmpty and
            fitems.IsEmpty and
            (fbalance = 0) and
            (fcreated_at = 0) and
            (fexpiration_date = 0) and
            (fexternal_id = '') and
            (fpaid_amount = 0) and
            (fpayment_date = 0) and
            (fpix_psp = '') and
            (ftotal_order = 0) and
            (fupdated_at = 0) and
            (fwallet = '') and
            (fwallet_payment_id = '') and
            (fmessage <> '');
end;

procedure TShipayOrderInfo.Assign(Source: TShipayOrderInfo);
begin
  inherited Assign(Source);
  fbalance := Source.balance;
  fcreated_at := Source.created_at;
  fexpiration_date := Source.expiration_date;
  fexternal_id := Source.external_id;
  fitems.Assign(Source.items);
  fpaid_amount := Source.paid_amount;
  fpayment_date := Source.payment_date;
  fpix_psp := Source.pix_psp;
  ftotal_order := Source.total_order;
  fupdated_at := Source.updated_at;
  fwallet := Source.wallet;
  fwallet_payment_id := Source.wallet_payment_id;
end;

procedure TShipayOrderInfo.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  inherited DoWriteToJSon(AJSon);
  fitems.WriteToJSon(AJSon);

  AJSon
    .AddPair('balance', (fbalance*100), False)
    .AddPair('external_id', fexternal_id, False)
    .AddPair('paid_amount', fpaid_amount, False)
    .AddPair('pix_psp', fpix_psp, False)
    .AddPair('total_order', ftotal_order, False)
    .AddPair('wallet', fwallet)
    .AddPair('wallet_payment_id', fwallet_payment_id, False)
    .AddPair('message', fmessage, False);

  if (fcreated_at <> 0) then
    AJSon.AddPair('created_at', DateTimeToIso8601(fcreated_at));
  if (fexpiration_date <> 0) then
    AJSon.AddPair('expiration_date', DateTimeToIso8601(fexpiration_date));
  if (fpayment_date <> 0) then
    AJSon.AddPair('payment_date', DateTimeToIso8601(fpayment_date));
  if (fupdated_at <> 0) then
    AJSon.AddPair('updated_at', DateTimeToIso8601(fupdated_at));
end;

procedure TShipayOrderInfo.DoReadFromJSon(AJSon: TACBrJSONObject);
var
  s: String;
  c: Currency;
begin
  inherited DoReadFromJSon(AJSon);
  fitems.ReadFromJSon(AJSon);
     
  {$IfDef FPC}
  c := 0;
  s := EmptyStr;
  {$EndIf}

  AJSon
    .Value('balance', c)
    .Value('external_id', fexternal_id)
    .Value('paid_amount', fpaid_amount)
    .Value('pix_psp', fpix_psp)
    .Value('total_order', ftotal_order)
    .Value('wallet', fwallet)
    .Value('wallet_payment_id', fwallet_payment_id)
    .Value('message', fmessage);
                                              
  fbalance := (c/100);
  AJSon.Value('created_at', s);
  if NaoEstaVazio(s) then
    fcreated_at := Iso8601ToDateTime(s);

  AJSon.Value('expiration_date', s);
  if NaoEstaVazio(s) then
    fexpiration_date := Iso8601ToDateTime(s);

  AJSon.Value('payment_date', s);
  if NaoEstaVazio(s) then
    fpayment_date := Iso8601ToDateTime(s);

  AJSon.Value('updated_at', s);
  if NaoEstaVazio(s) then
    fupdated_at := Iso8601ToDateTime(s);
end;

{ TShipayOrderData }

constructor TShipayOrderData.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
end;

procedure TShipayOrderData.Clear;
begin
  inherited Clear;
  fcustomer_id := '';
  fcustomer_name := '';
  forder_created_at := 0;
  forder_expiration_date := 0;
  forder_payment_date := 0;
  forder_updated_at := 0;
  fstore_id := '';
  fstore_name := '';
  fstore_pos_id := '';
  fstore_pos_name := '';
  ftotal_order := 0;
  fwallet_payment_id := '';
end;

function TShipayOrderData.IsEmpty: Boolean;
begin
  Result := inherited IsEmpty and
            (fcustomer_id = '') and
            (fcustomer_name = '') and
            (forder_created_at = 0) and
            (forder_expiration_date = 0) and
            (forder_payment_date = 0) and
            (forder_updated_at = 0) and
            (fstore_id = '') and
            (fstore_name = '') and
            (fstore_pos_id = '') and
            (fstore_pos_name = '') and
            (ftotal_order = 0) and
            (fwallet_payment_id = '');
end;

procedure TShipayOrderData.Assign(Source: TShipayOrderData);
begin
  inherited Assign(Source);
  fcustomer_id := Source.customer_id;
  fcustomer_name := Source.customer_name;
  forder_created_at := Source.order_created_at;
  forder_expiration_date := Source.order_expiration_date;
  forder_payment_date := Source.order_payment_date;
  forder_updated_at := Source.order_updated_at;
  fstore_id := Source.store_id;
  fstore_name := Source.store_name;
  fstore_pos_id := Source.store_pos_id;
  fstore_pos_name := Source.store_pos_name;
  ftotal_order := Source.total_order;
  fwallet_payment_id := Source.wallet_payment_id;
end;

procedure TShipayOrderData.AssignSchema(ASource: TACBrPIXSchema);
begin
   if (ASource is TShipayOrderData) then
     Assign(TShipayOrderData(ASource));
end;

procedure TShipayOrderData.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  inherited DoWriteToJSon(AJSon);

  AJSon
    .AddPair('customer_id', fcustomer_id)
    .AddPair('customer_name', fcustomer_name)
    .AddPairISODateTime('order_created_at', forder_created_at, False)
    .AddPairISODateTime('order_expiration_date', forder_expiration_date, False)
    .AddPairISODateTime('order_payment_date', forder_payment_date, False)
    .AddPairISODateTime('order_updated_at', forder_updated_at, False)
    .AddPair('store_id', fstore_id)
    .AddPair('store_name', fstore_name)
    .AddPair('store_pos_id', fstore_pos_id)
    .AddPair('store_pos_name', fstore_pos_name)
    .AddPair('total_order', ftotal_order)
    .AddPair('wallet_payment_id', fwallet_payment_id);
end;

procedure TShipayOrderData.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  inherited DoReadFromJSon(AJSon);

  AJSon
    .Value('customer_id', fcustomer_id)
    .Value('customer_name', fcustomer_name)
    .ValueISODateTime('order_created_at', forder_created_at)
    .ValueISODateTime('order_expiration_date', forder_expiration_date)
    .ValueISODateTime('order_payment_date', forder_payment_date)
    .ValueISODateTime('order_updated_at', forder_updated_at)
    .Value('store_id', fstore_id)
    .Value('store_name', fstore_name)
    .Value('store_pos_id', fstore_pos_id)
    .Value('store_pos_name', fstore_pos_name)
    .Value('total_order', ftotal_order)
    .Value('wallet_payment_id', fwallet_payment_id);
end;

{ TShipayOrderDataArray }

function TShipayOrderDataArray.GetItem(Index: Integer): TShipayOrderData;
begin
  Result := TShipayOrderData(inherited Items[Index]);
end;

procedure TShipayOrderDataArray.SetItem(Index: Integer; Value: TShipayOrderData);
begin
  inherited Items[Index] := Value;
end;

function TShipayOrderDataArray.NewSchema: TACBrPIXSchema;
begin
  Result := New;
end;

function TShipayOrderDataArray.Add(AItem: TShipayOrderData): Integer;
begin
  Result := inherited Add(AItem);
end;

procedure TShipayOrderDataArray.Insert(Index: Integer; AItem: TShipayOrderData);
begin
  inherited Insert(Index, AItem);
end;

function TShipayOrderDataArray.New: TShipayOrderData;
begin
  Result := TShipayOrderData.Create('');
  Self.Add(Result);
end;

{ TShipayOrdersList }

constructor TShipayOrdersList.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  fdata := TShipayOrderDataArray.Create('data');
end;

destructor TShipayOrdersList.Destroy;
begin
  fdata.Free;
  inherited Destroy;
end;

procedure TShipayOrdersList.Clear;
begin
  fdata.Clear;
  fcount := 0;
  foffset := 0;
  ftotal := 0;
end;

function TShipayOrdersList.IsEmpty: Boolean;
begin
  Result := fdata.IsEmpty and
            (fcount = 0) and
            (foffset = 0) and
            (ftotal = 0);
end;

procedure TShipayOrdersList.Assign(Source: TShipayOrdersList);
begin
  fdata.Assign(Source.data);
  fcount := Source.count;
  foffset := Source.offset;
  ftotal := Source.total;
end;

procedure TShipayOrdersList.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('count', fcount)
    .AddPair('offset', foffset)
    .AddPair('total', ftotal);
  fdata.WriteToJSon(AJSon);
end;

procedure TShipayOrdersList.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .Value('count', fcount)
    .Value('offset', foffset)
    .Value('total', ftotal);
  fdata.ReadFromJSon(AJSon);
end;

{ TShipayAmountDetailsValue }

procedure TShipayAmountDetailsValue.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  if (modalidade = pvmNenhum) then
    Exit;

  aJSon
    .AddPair('modality', Ord(modalidade))
    .AddPair('value', valorPerc);
end;

procedure TShipayAmountDetailsValue.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  i: Integer;
  c: Currency;
begin
  {$IfDef FPC}
  c := 0;
  i := 0;
  {$EndIf}

  aJSon
    .Value('modality', i)
    .Value('value', c);

   modalidade := TACBrPIXValoresModalidade(i);
   valorPerc := c;
end;

{ TShipayAmountDetailsDiscount }

function TShipayAmountDetailsDiscount.GetDiscountList: TShipayAmountDiscountList;
begin
  Result := TShipayAmountDiscountList(fdescontosDataFixa);
end;

procedure TShipayAmountDetailsDiscount.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  if (modalidade = pdmNenhum) then
    Exit;

  aJSon.AddPair('modality', Ord(modalidade));

  if (Ord(modalidade) >= 3) then
    aJSon.AddPair('value', valorPerc)
  else if (Ord(modalidade) <= 2) then
    descontosDataFixa.WriteToJSon(AJSon);
end;

procedure TShipayAmountDetailsDiscount.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  i: Integer;
  c: Currency;
begin
  {$IfDef FPC}
  c := 0;
  i := 0;
  {$EndIf}

  aJSon.Value('modality', i);

  modalidade := TACBrPIXDescontoModalidade(i);
  if (i >= 3) then
  begin
    aJSon.Value('value', c);
    valorPerc := c;
  end
  else if (i <= 2) then
    descontosDataFixa.ReadFromJSon(aJSon);
end;

constructor TShipayAmountDetailsDiscount.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);

  if Assigned(fdescontosDataFixa) then
    fdescontosDataFixa.Free;

  fdescontosDataFixa := TShipayAmountDiscountList.Create('fixed_date');
end;

{ TShipayAmountDetails }

constructor TShipayAmountDetails.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  fdiscount := TShipayAmountDetailsDiscount.Create('discount');
  ffine := TShipayAmountDetailsValue.Create('fine');
  finterest := TShipayAmountDetailsInterest.Create('interest');
  frebate := TShipayAmountDetailsValue.Create('rebate');
  Clear;
end;

destructor TShipayAmountDetails.Destroy;
begin
  discount.Free;
  fine.Free;
  interest.Free;
  rebate.Free;
  inherited Destroy;
end;

procedure TShipayAmountDetails.Clear;
begin
  discount.Clear;
  fine.Clear;
  interest.Clear;
  rebate.Clear;
end;

function TShipayAmountDetails.IsEmpty: Boolean;
begin
  Result := discount.IsEmpty and fine.IsEmpty and interest.IsEmpty and rebate.IsEmpty;
end;

procedure TShipayAmountDetails.Assign(aSource: TShipayAmountDetails);
begin
  discount.Assign(aSource.discount);
  fine.Assign(aSource.fine);
  interest.Assign(aSource.interest);
  rebate.Assign(aSource.rebate);
end;

procedure TShipayAmountDetails.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  discount.WriteToJSon(aJSon);
  fine.WriteToJSon(aJSon);
  interest.WriteToJSon(aJSon);
  rebate.WriteToJSon(aJSon);
end;

procedure TShipayAmountDetails.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  discount.ReadFromJSon(aJSon);
  fine.ReadFromJSon(aJSon);
  interest.ReadFromJSon(aJSon);
  rebate.ReadFromJSon(aJSon);
end;

end.

