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
  Classes, SysUtils,
  ACBrPIXBase,
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   JsonDataObjects_ACBr
  {$Else}
   Jsons
  {$EndIf};

type

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
    procedure DoWriteToJSon(AJSon: TJsonObject); override;
    procedure DoReadFromJSon(AJSon: TJsonObject); override;
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
    fquantity: Double;
    fsku: String;
    funit_price: Currency;
  protected
    procedure AssignSchema(ASource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(AJSon: TJsonObject); override;
    procedure DoReadFromJSon(AJSon: TJsonObject); override;
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
    procedure DoWriteToJSon(AJSon: TJsonObject); override;
    procedure DoReadFromJSon(AJSon: TJsonObject); override;
  public
    constructor Create(const ObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TShipayBuyer);

    property cpf_cnpj: String read fcpf_cnpj write Setcpf_cnpj;
    property email: String read femail write Setemail;
    property name: String read fname write fname;
    property phone: String read fphone write fphone;
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
  protected
    procedure AssignSchema(ASource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(AJSon: TJsonObject); override;
    procedure DoReadFromJSon(AJSon: TJsonObject); override;
  public
    constructor Create(const ObjectName: String); override;
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
    property wallet: String read fwallet write fwallet;
  end;

  TShipayOrderStatus = (
    spsNone,
    spsPending,      // Pedido aberto e ainda não pago ou cancelado
    spsPendingV,     // Pedido aberto e ainda não pago ou cancelado (para pedidos com vencimento criados através do POST /orderv)
    spsApproved,     // Pedido aprovado na carteira digital
    spsCancelled,    // Pedido (ainda não pago) cancelado na carteira digital
    spsExpired,      // Pedido expirado após 60 minutos com status "pending"
    spsRefunded,     // Pagamento devolvido ao comprador
    spsRefundPending // Pagamento com devolução solicitada. Status aplicável para PIX e para a carteira digital Cielo Pay pois a ação de devolução não é síncrona nestes casos. No caso de PIX a devolução deve ser efetivada em até 90 segudos após a solicitação. No caso da Cielo Pay, a devolução ocorre sempre no dia seguinte à solicitação. Em ambos os casos, quando a devolução é efetivada, o status na Shipay é alterado para "refunded"
  );

  { TShipayOrderBase }

  TShipayOrderBase = class(TACBrPIXSchema)
  private
    forder_id: String;
    fstatus: TShipayOrderStatus;
  protected
    procedure DoWriteToJSon(AJSon: TJsonObject); override;
    procedure DoReadFromJSon(AJSon: TJsonObject); override;
  public
    constructor Create(const ObjectName: String); override;
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
    procedure DoWriteToJSon(AJSon: TJsonObject); override;
    procedure DoReadFromJSon(AJSon: TJsonObject); override;
  public
    constructor Create(const ObjectName: String); override;
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
    fcreated_at: TDateTime;
    fexpiration_date: TDateTime;
    fexternal_id: String;
    fitems: TShipayItemArray;
    fpaid_amount: Currency;
    fpayment_date: TDateTime;
    fpix_psp: String;
    ftotal_order: Currency;
    fupdated_at: TDateTime;
    fwallet: String;
    fwallet_payment_id: String;
  protected
    procedure DoWriteToJSon(AJSon: TJsonObject); override;
    procedure DoReadFromJSon(AJSon: TJsonObject); override;
  public
    constructor Create(const ObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TShipayOrderInfo);

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
  end;


function ShipayOrderStatusToString(AStatus: TShipayOrderStatus): String;
function StringToShipayOrderStatus(const AString: String): TShipayOrderStatus;

implementation

uses
  ACBrUtil, ACBrValidador;

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
  else
    Result := spsNone;
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

procedure TShipayWallet.DoWriteToJSon(AJSon: TJsonObject);
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   AJSon.B['Active'] := fActive;
   AJSon.S['friendly_name'] := ffriendly_name;
   AJSon.S['logo'] := flogo;
   AJSon.F['minimum_payment'] := fminimum_payment;
   AJSon.S['pix_dict_key'] := fpix_dict_key;
   AJSon.S['pix_psp'] := fpix_psp;
   AJSon.S['wallet'] := fwallet;
   AJSon.S['wallet_setting_id'] := fwallet_setting_id;
   AJSon.S['wallet_setting_name'] := fwallet_setting_name;
  {$Else}
   AJSon['Active'].AsBoolean := fActive;
   AJSon['friendly_name'].AsString := ffriendly_name;
   AJSon['logo'].AsString := flogo;
   AJSon['minimum_payment'].AsNumber := fminimum_payment;
   AJSon['pix_dict_key'].AsString := fpix_dict_key;
   AJSon['pix_psp'].AsString := fpix_psp;
   AJSon['wallet'].AsString := fwallet;
   AJSon['wallet_setting_id'].AsString := fwallet_setting_id;
   AJSon['wallet_setting_name'].AsString := fwallet_setting_name;
  {$EndIf}
end;

procedure TShipayWallet.DoReadFromJSon(AJSon: TJsonObject);
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   fActive := AJSon.B['Active'];
   ffriendly_name := AJSon.S['friendly_name'];
   flogo := AJSon.S['logo'];
   fminimum_payment := AJSon.F['minimum_payment'];
   fpix_dict_key := AJSon.S['pix_dict_key'];
   fpix_psp := AJSon.S['pix_psp'];
   fwallet := AJSon.S['wallet'];
   fwallet_setting_id := AJSon.S['wallet_setting_id'];
   fwallet_setting_name := AJSon.S['wallet_setting_name'];
  {$Else}
   fActive := AJSon['Active'].AsBoolean;
   ffriendly_name := AJSon['friendly_name'].AsString;
   flogo := AJSon['logo'].AsString;
   fminimum_payment := AJSon['minimum_payment'].AsNumber;
   fpix_dict_key := AJSon['pix_dict_key'].AsString;
   fpix_psp := AJSon['pix_psp'].AsString;
   fwallet := AJSon['wallet'].AsString;
   fwallet_setting_id := AJSon['wallet_setting_id'].AsString;
   fwallet_setting_name := AJSon['wallet_setting_name'].AsString;
  {$EndIf}
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
end;

function TShipayItem.IsEmpty: Boolean;
begin
  Result := (fean = '') and
            (fitem_title = '') and
            (fquantity = 0) and
            (fsku = '') and
            (funit_price = 0);
end;

procedure TShipayItem.Assign(Source: TShipayItem);
begin
  fean := Source.ean;
  fitem_title := Source.item_title;
  fquantity := Source.quantity;
  fsku := Source.sku;
  funit_price := Source.unit_price;
end;

procedure TShipayItem.AssignSchema(ASource: TACBrPIXSchema);
begin
  if (ASource is TShipayItem) then
    Assign(TShipayItem(ASource));
end;

procedure TShipayItem.DoWriteToJSon(AJSon: TJsonObject);
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   AJSon.S['ean'] := fean;
   AJSon.S['item_title'] := fitem_title;
   AJSon.F['quantity'] := fquantity;
   AJSon.S['sku'] := fsku;
   AJSon.F['unit_price'] := funit_price;
  {$Else}
   AJSon['ean'].AsString := fean;
   AJSon['item_title'].AsString := fitem_title;
   AJSon['quantity'].AsNumber := fquantity;
   AJSon['sku'].AsString := fsku;
   AJSon['unit_price'].AsNumber := funit_price;
  {$EndIf}
end;

procedure TShipayItem.DoReadFromJSon(AJSon: TJsonObject);
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   fean := AJSon.S['ean'];
   fitem_title := AJSon.S['item_title'];
   fquantity := AJSon.F['quantity'];
   fsku := AJSon.S['sku'];
   funit_price := AJSon.F['unit_price'];
  {$Else}
   fean := AJSon['ean'].AsString;
   fitem_title := AJSon['item_title'].AsString;
   fquantity := AJSon['quantity'].AsNumber;
   fsku := AJSon['sku'].AsString;
   funit_price := AJSon['unit_price'].AsNumber;
  {$EndIf}
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

procedure TShipayBuyer.DoWriteToJSon(AJSon: TJsonObject);
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   if (fcpf_cnpj <> '') then
     AJSon.S['cpf_cnpj'] := fcpf_cnpj;
   AJSon.S['email'] := femail;
   AJSon.S['name'] := fname;
   if (fphone <> '') then
     AJSon.S['phone'] := fphone;
  {$Else}
   if (fcpf_cnpj <> '') then
     AJSon['cpf_cnpj'].AsString := fcpf_cnpj;
   AJSon['email'].AsString := femail;
   AJSon['name'].AsString := fname;
   if (fphone <> '') then
     AJSon['phone'].AsString := fphone;
  {$EndIf}
end;

procedure TShipayBuyer.DoReadFromJSon(AJSon: TJsonObject);
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   fcpf_cnpj := AJSon.S['cpf_cnpj'];
   femail := AJSon.S['email'];
   fname := AJSon.S['name'];
   fphone := AJSon.S['phone'];
  {$Else}
   fcpf_cnpj := AJSon['cpf_cnpj'].AsString;
   femail := AJSon['email'].AsString;
   fname := AJSon['name'].AsString;
   fphone := AJSon['phone'].AsString;
  {$EndIf}
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

procedure TShipayOrder.AssignSchema(ASource: TACBrPIXSchema);
begin
  if (ASource is TShipayOrder) then
    Assign(TShipayOrder(ASource));
end;

procedure TShipayOrder.DoWriteToJSon(AJSon: TJsonObject);
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   fbuyer.WriteToJSon(AJSon);
   if (fcallback_url <> '') then
     AJSon.S['callback_url'] := fcallback_url;
   if (fexpiration > 0) then
     AJSon.I['expiration'] := fexpiration;
   fitems.WriteToJSon(AJSon);
   AJSon.S['order_ref'] := forder_ref;
   if (fpix_dict_key <> '') then
     AJSon.S['pix_dict_key']:= fpix_dict_key;
   AJSon.D['total'] := ftotal;
   AJSon.S['wallet'] := fwallet;
  {$Else}
   fbuyer.WriteToJSon(AJSon);
   if (fcallback_url <> '') then
     AJSon['callback_url'].AsString := fcallback_url;
   if (fexpiration > 0) then
     AJSon['expiration'].AsInteger := fexpiration;
   fitems.WriteToJSon(AJSon);
   AJSon['order_ref'].AsString := forder_ref;
   if (fpix_dict_key <> '') then
     AJSon['pix_dict_key'].AsString := fpix_dict_key;
   AJSon['total'].AsNumber := ftotal;
   AJSon['wallet'].AsString := fwallet;
  {$EndIf}
end;

procedure TShipayOrder.DoReadFromJSon(AJSon: TJsonObject);
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   fbuyer.ReadFromJSon(AJSon);
   fcallback_url := AJSon.S['callback_url'];
   fexpiration := AJSon.I['expiration'];
   fitems.ReadFromJSon(AJSon);
   forder_ref:= AJSon.S['order_ref'];
   fpix_dict_key := AJSon.S['pix_dict_key'];
   ftotal := AJSon.D['total'];
   fwallet := AJSon.S['wallet'];
  {$Else}
   fbuyer.ReadFromJSon(AJSon);
   fcallback_url := AJSon['callback_url'].AsString;
   fexpiration := AJSon['expiration'].AsInteger;
   fitems.ReadFromJSon(AJSon);
   forder_ref:= AJSon['order_ref'].AsString;
   fpix_dict_key := AJSon['pix_dict_key'].AsString;
   ftotal := AJSon['total'].AsNumber;
   fwallet := AJSon['wallet'].AsString;
  {$EndIf}
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

procedure TShipayOrderBase.DoWriteToJSon(AJSon: TJsonObject);
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   AJSon.S['order_id'] := forder_id;
   AJSon.S['status'] := ShipayOrderStatusToString(fstatus);
  {$Else}
   AJSon['order_id'].AsString := forder_id;
   AJSon['status'].AsString := ShipayOrderStatusToString(fstatus);
  {$EndIf}
end;

procedure TShipayOrderBase.DoReadFromJSon(AJSon: TJsonObject);
begin
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   forder_id := AJSon.S['order_id'];
   fstatus := StringToShipayOrderStatus( AJSon.S['status'] );
  {$Else}
   forder_id := AJSon['order_id'].AsString;
   fstatus := StringToShipayOrderStatus( AJSon['status'].AsString );
  {$EndIf}
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

procedure TShipayOrderCreated.DoWriteToJSon(AJSon: TJsonObject);
begin
  inherited DoWriteToJSon(AJSon);
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   if (fdeep_link <> '') then
     AJSon.S['deep_link'] := fdeep_link;
   if (fexpiration_date <> 0) then
     AJSon.S['expiration_date'] := DateTimeToIso8601(fexpiration_date);
   if (fpix_dict_key <> '') then
     AJSon.S['pix_dict_key'] := fpix_dict_key;
   if (fpix_psp <> '') then
     AJSon.S['pix_psp'] := fpix_psp;
   AJSon.S['qr_code'] := fqr_code;
   AJSon.S['qr_code_text'] := fqr_code_text;
   AJSon.S['wallet'] := fwallet;
  {$Else}
   if (fdeep_link <> '') then
     AJSon['deep_link'].AsString := fdeep_link;
   if (fexpiration_date <> 0) then
     AJSon['expiration_date'].AsString := DateTimeToIso8601(fexpiration_date);
   if (fpix_dict_key <> '') then
     AJSon['pix_dict_key'].AsString := fpix_dict_key;
   if (fpix_psp <> '') then
     AJSon['pix_psp'].AsString := fpix_psp;
   AJSon['qr_code'].AsString := fqr_code;
   AJSon['qr_code_text'].AsString := fqr_code_text;
   AJSon['wallet'].AsString := fwallet;
  {$EndIf}
end;

procedure TShipayOrderCreated.DoReadFromJSon(AJSon: TJsonObject);
var
  s: String;
begin
  inherited DoReadFromJSon(AJSon);
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   fdeep_link := AJSon.S['deep_link'];
   s := AJSon.S['expiration_date'];
   if (s <> '') then
     fexpiration_date := Iso8601ToDateTime(s);
   fpix_dict_key := AJSon.S['pix_dict_key'];
   fpix_psp := AJSon.S['pix_psp'];
   fqr_code := AJSon.S['qr_code'];
   fqr_code_text := AJSon.S['qr_code_text'];
   fwallet := AJSon.S['wallet'];
  {$Else}
   fdeep_link := AJSon['deep_link'].AsString;
   s := AJSon['expiration_date'].AsString;
   if (s <> '') then
     fexpiration_date := Iso8601ToDateTime(s);
   fpix_dict_key := AJSon['pix_dict_key'].AsString;
   fpix_psp := AJSon['pix_psp'].AsString;
   fqr_code := AJSon['qr_code'].AsString;
   fqr_code_text := AJSon['qr_code_text'].AsString;
   fwallet := AJSon['wallet'].AsString;
  {$EndIf}
end;

{ TShipayOrderInfo }

constructor TShipayOrderInfo.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  fitems := TShipayItemArray.Create('items');
  Clear;
end;

destructor TShipayOrderInfo.Destroy;
begin
  fitems.Free;
  inherited Destroy;
end;

procedure TShipayOrderInfo.Clear;
begin
  inherited Clear;
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
end;

function TShipayOrderInfo.IsEmpty: Boolean;
begin
  Result := inherited IsEmpty and
            fitems.IsEmpty and
            (fcreated_at = 0) and
            (fexpiration_date = 0) and
            (fexternal_id = '') and
            (fpaid_amount = 0) and
            (fpayment_date = 0) and
            (fpix_psp = '') and
            (ftotal_order = 0) and
            (fupdated_at = 0) and
            (fwallet = '') and
            (fwallet_payment_id = '');
end;

procedure TShipayOrderInfo.Assign(Source: TShipayOrderInfo);
begin
  inherited Assign(Source);
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

procedure TShipayOrderInfo.DoWriteToJSon(AJSon: TJsonObject);
begin
  inherited DoWriteToJSon(AJSon);
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   if (fcreated_at <> 0) then
     AJSon.S['created_at'] := DateTimeToIso8601(fcreated_at);
   if (fexpiration_date <> 0) then
     AJSon.S['expiration_date'] := DateTimeToIso8601(fexpiration_date);
   if (fexternal_id <> '') then
     AJSon.S['external_id'] := fexternal_id;
   fitems.WriteToJSon(AJSon);
   if (fpaid_amount <> 0) then
     AJSon.D['paid_amount'] := fpaid_amount;
   if (fpayment_date <> 0) then
     AJSon.S['payment_date'] := DateTimeToIso8601(fpayment_date);
   if (fpix_psp <> '') then
     AJSon.S['pix_psp'] := fpix_psp;
   if (ftotal_order <> 0) then
     AJSon.D['total_order'] := ftotal_order;
   if (fupdated_at <> 0) then
     AJSon.S['updated_at'] := DateTimeToIso8601(fupdated_at);
   AJSon.S['wallet'] := fwallet;
   if (fwallet_payment_id <> '') then
     AJSon.S['wallet_payment_id'] := fwallet_payment_id;
  {$Else}
   if (fcreated_at <> 0) then
     AJSon['created_at'].AsString := DateTimeToIso8601(fcreated_at);
   if (fexpiration_date <> 0) then
     AJSon['expiration_date'].AsString := DateTimeToIso8601(fexpiration_date);
   if (fexternal_id <> '') then
     AJSon['external_id'].AsString := fexternal_id;
   fitems.WriteToJSon(AJSon);
   if (fpaid_amount <> 0) then
     AJSon['paid_amount'].AsNumber := fpaid_amount;
   if (fpayment_date <> 0) then
     AJSon['payment_date'].AsString := DateTimeToIso8601(fpayment_date);
   if (fpix_psp <> '') then
     AJSon['pix_psp'].AsString := fpix_psp;
   if (ftotal_order <> 0) then
     AJSon['total_order'].AsNumber := ftotal_order;
   if (fupdated_at <> 0) then
     AJSon['updated_at'].AsString := DateTimeToIso8601(fupdated_at);
   AJSon['wallet'].AsString := fwallet;
   if (fwallet_payment_id <> '') then
     AJSon['wallet_payment_id'].AsString := fwallet_payment_id;
  {$EndIf}
end;

procedure TShipayOrderInfo.DoReadFromJSon(AJSon: TJsonObject);
var
  s: String;
begin
  inherited DoReadFromJSon(AJSon);
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   fitems.ReadFromJSon(AJSon);
   s := AJSon.S['created_at'];
   if (s <> '') then
     fcreated_at := Iso8601ToDateTime(s);
   s := AJSon.S['expiration_date'];
   if (s <> '') then
     fexpiration_date := Iso8601ToDateTime(s);
   fexternal_id := AJSon.S['external_id'];
   fpaid_amount := AJSon.D['paid_amount'];
   s := AJSon.S['payment_date'];
   if (s <> '') then
     fpayment_date := Iso8601ToDateTime(s);
   fpix_psp := AJSon.S['pix_psp'];
   ftotal_order := AJSon.D['total_order'];
   s := AJSon.S['updated_at'];
   if (s <> '') then
     fupdated_at := Iso8601ToDateTime(s);
   fwallet := AJSon.S['wallet'];
   fwallet_payment_id := AJSon.S['wallet_payment_id'];
  {$Else}
   fitems.ReadFromJSon(AJSon);
   s := AJSon['created_at'].AsString;
   if (s <> '') then
     fcreated_at := Iso8601ToDateTime(s);
   s := AJSon['expiration_date'].AsString;
   if (s <> '') then
     fexpiration_date := Iso8601ToDateTime(s);
   fexternal_id := AJSon['external_id'].AsString;
   fpaid_amount := AJSon['paid_amount'].AsNumber;
   s := AJSon['payment_date'].AsString;
   if (s <> '') then
     fpayment_date := Iso8601ToDateTime(s);
   fpix_psp := AJSon['pix_psp'].AsString;
   ftotal_order := AJSon['total_order'].AsNumber;
   s := AJSon['updated_at'].AsString;
   if (s <> '') then
     fupdated_at := Iso8601ToDateTime(s);
   fwallet := AJSon['wallet'].AsString;
   fwallet_payment_id := AJSon['wallet_payment_id'].AsString;
  {$EndIf}
end;

end.

