unit ACBrOpenDeliverySchemaClassesTest;

{$I ACBr.inc}

interface

uses
  ACBrTests.Util,
  ACBrOpenDeliverySchema,
  ACBrOpenDeliverySchemaClasses,
  ACBrUtil.Strings,
  ACBrJSON,
  pcnConversaoOD,
  Classes,
  SysUtils;

type
  TTestAccessToken = class(TTestCase)
  private
    FJSON: String;
    FSchema: TACBrOpenDeliverySchemaAccessToken;

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure JSONToObject;
  end;

  TTestAcknowledgment = class(TTestCase)
  private
    FJSON: String;
    FJSONObject: TACBrJSONObject;
    FSchema: TACBrOpenDeliverySchemaAcknowledgment;

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure ObjectToJson;
  end;

  TTestAddress = class(TTestCase)
  private
    FJSON: String;
    FJSONObject: TACBrJSONObject;
    FSchema: TACBrOpenDeliverySchemaAddress;

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure JSONToObject;
    procedure ObjectToJson;
  end;

  TTestAvailability = class(TTestCase)
  private
    FJSON: String;
    FJSONObject: TACBrJSONObject;
    FSchema: TACBrOpenDeliverySchemaAvailability;

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure JSONToObject;
    procedure ObjectToJson;
  end;

  TTestBasicInfo = class(TTestCase)
  private
    FJSON: String;
    FJSONObject: TACBrJSONObject;
    FSchema: TACBrOpenDeliverySchemaBasicInfo;

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure JSONToObject;
    procedure ObjectToJSON;
  end;

  TTestCategory = class(TTestCase)
  private
    FJSON: String;
    FJSONObject: TACBrJSONObject;
    FSchema: TACBrOpenDeliverySchemaCategory;

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure JSONToObject;
    procedure ObjectToJSON;
  end;

  TTestContactPhone = class(TTestCase)
  private
    FJSON: String;
    FJSONObject: TACBrJSONObject;
    FSchema: TACBrOpenDeliverySchemaContactPhone;

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure JSONToObject;
    procedure ObjectToJSON;
  end;

  TTestEvent = class(TTestCase)
  private
    FJSON: String;
    FJSONObject: TACBrJSONObject;
    FSchema: TACBrOpenDeliverySchemaEvent;

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure JSONToObject;
    procedure ObjectToJSON;
  end;

  TTestGeoCoordinate = class(TTestCase)
  private
    FJSON: String;
    FJSONObject: TACBrJSONObject;
    FSchema: TACBrOpenDeliverySchemaGeoCoordinate;

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure JSONToObject;
    procedure ObjectToJSON;
  end;

  TTestGeoRadius = class(TTestCase)
  private
    FJSON: String;
    FJSONObject: TACBrJSONObject;
    FSchema: TACBrOpenDeliverySchemaGeoRadius;

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure JSONToObject;
    procedure ObjectToJSON;
  end;

  TTestHolidayHour = class(TTestCase)
  private
    FJSON: String;
    FJSONObject: TACBrJSONObject;
    FSchema: TACBrOpenDeliverySchemaHolidayHour;

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure JSONToObject;
    procedure ObjectToJSON;
  end;

  TTestHour = class(TTestCase)
  private
    FJSON: String;
    FJSONObject: TACBrJSONObject;
    FSchema: TACBrOpenDeliverySchemaHour;

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure JSONToObject;
    procedure ObjectToJSON;
  end;

  TTestImage = class(TTestCase)
  private
    FJSON: String;
    FJSONObject: TACBrJSONObject;
    FSchema: TACBrOpenDeliverySchemaImage;

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure JSONToObject;
    procedure ObjectToJSON;
  end;

  TTestItem = class(TTestCase)
  private
    FJSON: String;
    FJSONObject: TACBrJSONObject;
    FSchema: TACBrOpenDeliverySchemaItem;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure JSONToObject;
    procedure ObjectToJSON;
  end;

  TTestItemOffer = class(TTestCase)
  private
    FJSON: String;
    FJSONObject: TACBrJSONObject;
    FSchema: TACBrOpenDeliverySchemaItemOffer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure JSONToObject;
    procedure ObjectToJSON;
  end;

  TTestMenu = class(TTestCase)
  private
    FJSON: String;
    FJSONObject: TACBrJSONObject;
    FSchema: TACBrOpenDeliverySchemaMenu;

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure JSONToObject;
    procedure ObjectToJSON;
  end;

  TTestNutritionalInfo = class(TTestCase)
  private
    FJSON: String;
    FJSONObject: TACBrJSONObject;
    FSchema: TACBrOpenDeliverySchemaNutritionalInfo;

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure JSONToObject;
    procedure ObjectToJSON;
  end;

  TTestOption = class(TTestCase)
  private
    FJSON: string;
    FJSONObject: TACBrJSONObject;
    FSchema: TACBrOpenDeliverySchemaOption;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure JSONToObject;
    procedure ObjectToJSON;
  end;

  TTestOptionGroup = class(TTestCase)
  private
    FJSON: String;
    FJSONObject: TACBrJSONObject;
    FSchema: TACBrOpenDeliverySchemaOptionGroup;

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure JSONToObject;
    procedure ObjectToJSON;
  end;

  TTestOrder = class(TTestCase)
  private
    FJSON: String;
    FJSONObject: TACBrJSONObject;
    FSchema: TACBrOpenDeliverySchemaOrder;

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure JSONToObject;
    procedure ObjectToJson;
  end;

  TTestOrderAddress = class(TTestCase)
  private
    FJSON: String;
    FJSONObject: TACBrJSONObject;
    FSchema: TACBrOpenDeliverySchemaOrderAddress;

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure JSONToObject;
    procedure ObjectToJson;
  end;

  TTestOrderCustomer = class(TTestCase)
  private
    FJSON: String;
    FJSONObject: TACBrJSONObject;
    FSchema: TACBrOpenDeliverySchemaOrderCustomer;

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure JSONToObject;
    procedure ObjectToJson;
  end;

  TTestOrderDelivery = class(TTestCase)
  private
    FJSON: String;
    FJSONObject: TACBrJSONObject;
    FSchema: TACBrOpenDeliverySchemaOrderDelivery;

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure JSONToObject;
    procedure ObjectToJson;
  end;

  TTestOrderTakeout = class(TTestCase)
  private
    FJSON: String;
    FJSONObject: TACBrJSONObject;
    FSchema: TACBrOpenDeliverySchemaOrderTakeout;

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure JSONToObject;
    procedure ObjectToJSON;
  end;

  TTestPolygon = class(TTestCase)
  private
    FJSON: String;
    FJSONObject: TACBrJSONObject;
    FSchema: TACBrOpenDeliverySchemaPolygon;

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure JSONToObject;
    procedure ObjectToJSON;
  end;

  TTestPrice = class(TTestCase)
  private
    FJSON: String;
    FJSONObject: TACBrJSONObject;
    FSchema: TACBrOpenDeliverySchemaPrice;

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure JSONToObject;
    procedure ObjectToJSON;
  end;

  TTestRadius = class(TTestCase)
  private
    FJSON: String;
    FJSONObject: TACBrJSONObject;
    FSchema: TACBrOpenDeliverySchemaRadius;

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure JSONToObject;
    procedure ObjectToJSON;
  end;

  TTestService = class(TTestCase)
  private
    FJSON: String;
    FJSONObject: TACBrJSONObject;
    FSchema: TACBrOpenDeliverySchemaService;

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure JSONToObject;
    procedure ObjectToJSON;
  end;

  TTestServiceArea = class(TTestCase)
  private
    FJSON: String;
    FJSONObject: TACBrJSONObject;
    FSchema: TACBrOpenDeliverySchemaServiceArea;

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure JSONToObject;
    procedure ObjectToJSON;
  end;

  TTestServiceHour = class(TTestCase)
  private
    FJSON: String;
    FJSONObject: TACBrJSONObject;
    FSchema: TACBrOpenDeliverySchemaServiceHour;

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure JSONToObject;
    procedure ObjectToJSON;
  end;

  TTestTimePeriod = class(TTestCase)
  private
    FJSON: String;
    FJSONObject: TACBrJSONObject;
    FSchema: TACBrOpenDeliverySchemaTimePeriod;

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure JSONToObject;
    procedure ObjectToJSON;
  end;

implementation

{ TTestAddress }

procedure TTestAddress.ObjectToJson;
begin
  FSchema.AsJSON := FJSON;
  FJSONObject := TACBrJSONObject.Parse(FSchema.AsJSON);

  CheckEquals('BR', FJSONObject.AsString['country']);
  CheckEquals('BR-SP', FJSONObject.AsString['state']);
  CheckEquals('São Paulo', FJSONObject.AsString['city']);
  CheckEquals('Moema', FJSONObject.AsString['district']);
  CheckEquals('Plaza Avenue', FJSONObject.AsString['street']);
  CheckEquals('100', FJSONObject.AsString['number']);
  CheckEquals('20111-000', FJSONObject.AsString['postalCode']);
  CheckEquals('BL 02 AP 31', FJSONObject.AsString['complement']);
  CheckEquals('Yellow House', FJSONObject.AsString['reference']);
  CheckEquals('-23,54809', FloatToStr(FJSONObject.AsFloat['latitude']));
  CheckEquals('-46,63638', FloatToStr(FJSONObject.AsFloat['longitude']));
end;

procedure TTestAddress.SetUp;
begin
  inherited;
  FSchema := TACBrOpenDeliverySchemaAddress.Create;
  FJSON := ACBrStr(
    '{' +
      '"country": "BR",' +
      '"state": "BR-SP",' +
      '"city": "São Paulo",' +
      '"district": "Moema",' +
      '"street": "Plaza Avenue",' +
      '"number": "100",' +
      '"postalCode": "20111-000",' +
      '"complement": "BL 02 AP 31",' +
      '"reference": "Yellow House",' +
      '"latitude": -23.54809,' +
      '"longitude": -46.63638' +
    '}');
end;

procedure TTestAddress.TearDown;
begin
  FSchema.Free;
  FJSONObject.Free;
  inherited;
end;

procedure TTestAddress.JSONToObject;
begin
  FSchema.AsJSON := FJSON;
  CheckEquals('BR', FSchema.Country);
  CheckEquals('BR-SP', FSchema.State);
  CheckEquals('São Paulo', FSchema.City);
  CheckEquals('Moema', FSchema.District);
  CheckEquals('Plaza Avenue', FSchema.Street);
  CheckEquals('100', FSchema.Number);
  CheckEquals('20111-000', FSchema.PostalCode);
  CheckEquals('BL 02 AP 31', FSchema.Complement);
  CheckEquals('Yellow House', FSchema.Reference);
  CheckEquals('-23,54809', FloatToStr(FSchema.Latitude));
  CheckEquals('-46,63638', FloatToStr(FSchema.Longitude));
end;

{ TTestContactPhone }

procedure TTestContactPhone.JSONToObject;
begin
  FSchema.AsJSON := FJSON;
  CheckEquals('1234567', FSchema.WhatsappNumber);
  CheckEquals('7654321', FSchema.CommercialNumber);
end;

procedure TTestContactPhone.ObjectToJSON;
begin
  FSchema.AsJSON := FJSON;
  FJSONObject := TACBrJSONObject.Parse(FSchema.AsJSON);

  CheckEquals('1234567', FJSONObject.AsString['whatsappNumber']);
  CheckEquals('7654321', FJSONObject.AsString['commercialNumber']);
end;

procedure TTestContactPhone.SetUp;
begin
  inherited;
  FSchema := TACBrOpenDeliverySchemaContactPhone.Create;
  FJSON := ACBrStr(
    '{' +
      '"whatsappNumber": "1234567",' +
      '"commercialNumber": "7654321"' +
    '}');
end;

procedure TTestContactPhone.TearDown;
begin
  FSchema.Free;
  FJSONObject.Free;
  inherited;
end;

{ TTestPrice }

procedure TTestPrice.JSONToObject;
begin
  FSchema.AsJSON := FJSON;
  CheckEquals('15,5', FloatToStr(FSchema.value));
  CheckEquals('BRL', FSchema.currency);
end;

procedure TTestPrice.ObjectToJSON;
begin
  FSchema.AsJSON := FJSON;
  FJSONObject := TACBrJSONObject.Parse(FSchema.AsJSON);

  CheckEquals('15,5', FloatToStr(FJSONObject.AsFloat['value']));
  CheckEquals('BRL', FJSONObject.AsString['currency']);
end;

procedure TTestPrice.SetUp;
begin
  inherited;
  FSchema := TACBrOpenDeliverySchemaPrice.Create;
  FJSON := ACBrStr(
    '{' +
      '"value": 15.5,' +
      '"currency": "BRL"' +
    '}');
end;

procedure TTestPrice.TearDown;
begin
  FSchema.Free;
  FJSONObject.Free;
  inherited;
end;

{ TTestImage }

procedure TTestImage.JSONToObject;
begin
  FSchema.AsJSON := FJSON;
  CheckEquals('https://food-company.com/image.jpg', FSchema.URL);
  CheckEquals('96b41025', FSchema.CRC_32);
end;

procedure TTestImage.ObjectToJSON;
begin
  FSchema.AsJSON := FJSON;
  FJSONObject := TACBrJSONObject.Parse(FSchema.AsJSON);

  CheckEquals('https://food-company.com/image.jpg', FJSONObject.AsString['URL']);
  CheckEquals('96b41025', FJSONObject.AsString['CRC-32']);
end;

procedure TTestImage.SetUp;
begin
  inherited;
  FSchema := TACBrOpenDeliverySchemaImage.Create;
  FJSON := ACBrStr(
    '{' +
      '"URL": "https://food-company.com/image.jpg",' +
      '"CRC-32": "96b41025"' +
    '}');
end;

procedure TTestImage.TearDown;
begin
  inherited;
  FSchema.Free;
  FJSONObject.Free;
end;

{ TTestBasicInfo }

procedure TTestBasicInfo.JSONToObject;
begin
  FSchema.AsJSON := FJSON;
  CheckEquals('Pizza Plaza', FSchema.Name);
  CheckEquals('22815773000169', FSchema.Document);
  CheckEquals('Food Company', FSchema.CorporateName);
  CheckEquals('Food company specializing in pizzas.', FSchema.Description);
  CheckEquals('90', FloatToStr(FSchema.AverageTicket));
  CheckEquals('20', IntToStr(FSchema.AveragePreparationTime));
  CheckEquals('BRL', FSchema.MinOrderValue.currency);
  CheckEquals('40', FloatToStr(FSchema.MinOrderValue.value));
  CheckEquals('RESTAURANT', MerchantTypeToStr(FSchema.MerchantType));
  CheckEquals(3, Length(FSchema.MerchantCategories));
  CheckEquals('PIZZA', MerchantCategoriesToStr(FSchema.MerchantCategories[0]));
  CheckEquals('FAMILY_MEALS', MerchantCategoriesToStr(FSchema.MerchantCategories[1]));
  CheckEquals('PREMIUM', MerchantCategoriesToStr(FSchema.MerchantCategories[2]));
  CheckEquals('BR', FSchema.Address.Country);
  CheckEquals('BR-SP', FSchema.Address.State);
  CheckEquals('São Paulo', FSchema.Address.City);
  CheckEquals('Moema', FSchema.Address.District);
  CheckEquals('Plaza Avenue', FSchema.Address.Street);
  CheckEquals('100', FSchema.Address.Number);
  CheckEquals('20111-000', FSchema.Address.PostalCode);
  CheckEquals('BL 02 AP 31', FSchema.Address.Complement);
  CheckEquals('Yellow House', FSchema.Address.Reference);
  CheckEquals('-23,54809', FloatToStr(FSchema.Address.Latitude));
  CheckEquals('-46,63638', FloatToStr(FSchema.Address.Longitude));
  CheckEquals(2, Length(FSchema.ContactEmails));
  CheckEquals('food@company.com', FSchema.ContactEmails[0]);
  CheckEquals('food@acbr.com', FSchema.ContactEmails[1]);

  CheckEquals('11999999999', FSchema.ContactPhones.CommercialNumber);
  CheckEquals('11998888888', FSchema.ContactPhones.WhatsappNumber);

  CheckEquals('https://food-company.com/image.jpg', FSchema.LogoImage.URL);
  CheckEquals('96b41025', FSchema.LogoImage.CRC_32);

  CheckEquals('https://food-company.com/image.jpg', FSchema.BannerImage.URL);
  CheckEquals('96b41025', FSchema.BannerImage.CRC_32);

  CheckEquals('2019-08-24 14:15:22', FormatDateTime('yyyy-MM-dd hh:mm:ss', FSchema.CreatedAt));
end;

procedure TTestBasicInfo.ObjectToJSON;
begin
  FSchema.AsJSON := FJSON;
  FJSONObject := TACBrJSONObject.Parse(FSchema.AsJSON);

  CheckEquals('Pizza Plaza', FJSONObject.AsString['name']);
  CheckEquals('22815773000169', FJSONObject.AsString['document']);
  CheckEquals('Food Company', FJSONObject.AsString['corporateName']);
  CheckEquals('Food company specializing in pizzas.', FJSONObject.AsString['description']);
  CheckEquals('90', FloatToStr(FJSONObject.AsFloat['averageTicket']));
  CheckEquals('20', IntToStr(FJSONObject.AsInteger['averagePreparationTime']));
  CheckEquals('BRL', FJSONObject.AsJSONObject['minOrderValue'].AsString['currency']);
  CheckEquals('40', FloatToStr(FSchema.MinOrderValue.value));
  CheckEquals('RESTAURANT', FJSONObject.AsString['merchantType']);
  CheckEquals(3, FJSONObject.AsJSONArray['merchantCategories'].Count);
  CheckEquals('PIZZA', FJSONObject.AsJSONArray['merchantCategories'].Items[0]);
  CheckEquals('FAMILY_MEALS', FJSONObject.AsJSONArray['merchantCategories'].Items[1]);
  CheckEquals('PREMIUM', FJSONObject.AsJSONArray['merchantCategories'].Items[2]);
  CheckEquals('BR', FJSONObject.AsJSONObject['address'].AsString['country']);
  CheckEquals('BR-SP', FJSONObject.AsJSONObject['address'].AsString['state']);
  CheckEquals('São Paulo', FJSONObject.AsJSONObject['address'].AsString['city']);
  CheckEquals('Moema', FJSONObject.AsJSONObject['address'].AsString['district']);
  CheckEquals('Plaza Avenue', FJSONObject.AsJSONObject['address'].AsString['street']);
  CheckEquals('100', FJSONObject.AsJSONObject['address'].AsString['number']);
  CheckEquals('20111-000', FJSONObject.AsJSONObject['address'].AsString['postalCode']);
  CheckEquals('BL 02 AP 31', FJSONObject.AsJSONObject['address'].AsString['complement']);
  CheckEquals('Yellow House', FJSONObject.AsJSONObject['address'].AsString['reference']);
  CheckEquals('-23,54809', FloatToStr(FJSONObject.AsJSONObject['address'].AsFloat['latitude']));
  CheckEquals('-46,63638', FloatToStr(FJSONObject.AsJSONObject['address'].AsFloat['longitude']));
  CheckEquals(2, FJSONObject.AsJSONArray['contactEmails'].Count);
  CheckEquals('food@company.com', FJSONObject.AsJSONArray['contactEmails'].Items[0]);
  CheckEquals('food@acbr.com', FJSONObject.AsJSONArray['contactEmails'].Items[1]);

  CheckEquals('11999999999', FJSONObject.AsJSONObject['contactPhones'].AsString['commercialNumber']);
  CheckEquals('11998888888', FJSONObject.AsJSONObject['contactPhones'].AsString['whatsappNumber']);

  CheckEquals('https://food-company.com/image.jpg', FJSONObject.AsJSONObject['logoImage'].AsString['URL']);
  CheckEquals('96b41025', FJSONObject.AsJSONObject['logoImage'].AsString['CRC-32']);

  CheckEquals('https://food-company.com/image.jpg', FJSONObject.AsJSONObject['bannerImage'].AsString['URL']);
  CheckEquals('96b41025', FJSONObject.AsJSONObject['bannerImage'].AsString['CRC-32']);

  CheckEquals('2019-08-24 14:15:22', FormatDateTime('yyyy-MM-dd hh:mm:ss', FJSONObject.AsISODateTime['createdAt']));
end;

procedure TTestBasicInfo.SetUp;
begin
  inherited;
  FSchema := TACBrOpenDeliverySchemaBasicInfo.Create;
  FJSON := ACBrStr(
    '{' +
      '"name": "Pizza Plaza",' +
      '"document": "22815773000169",' +
      '"corporateName": "Food Company",' +
      '"description": "Food company specializing in pizzas.",' +
      '"averageTicket": 90,' +
      '"averagePreparationTime": 20,' +
      '"minOrderValue": {' +
        '"value": 40,' +
        '"currency": "BRL"' +
      '},' +
      '"merchantType": "RESTAURANT",' +
      '"merchantCategories": [' +
        '"PIZZA",' +
        '"FAMILY_MEALS",' +
        '"PREMIUM"' +
      '],' +
      '"address": {' +
        '"country": "BR",' +
        '"state": "BR-SP",' +
        '"city": "São Paulo",' +
        '"district": "Moema",' +
        '"street": "Plaza Avenue",' +
        '"number": "100",' +
        '"postalCode": "20111-000",' +
        '"complement": "BL 02 AP 31",' +
        '"reference": "Yellow House",' +
        '"latitude": -23.54809,' +
        '"longitude": -46.63638' +
      '},' +
      '"contactEmails": [' +
        '"food@company.com",' +
        '"food@acbr.com"' +
      '],' +
      '"contactPhones": {' +
        '"commercialNumber": "11999999999",' +
        '"whatsappNumber": "11998888888"' +
      '},' +
      '"logoImage": {' +
        '"URL": "https://food-company.com/image.jpg",' +
        '"CRC-32": "96b41025"' +
      '},' +
      '"bannerImage": {' +
        '"URL": "https://food-company.com/image.jpg",' +
        '"CRC-32": "96b41025"' +
      '},' +
      '"createdAt": "2019-08-24T14:15:22Z"}');
end;

procedure TTestBasicInfo.TearDown;
begin
  FSchema.Free;
  FJSONObject.Free;
  inherited;
end;

{ TTestRadius }

procedure TTestRadius.JSONToObject;
begin
  FSchema.AsJSON := FJSON;
  CheckEquals(1, FSchema.size);
  CheckEquals(5, FSchema.estimateDeliveryTime);
  CheckEquals('BRL', FSchema.price.currency);
  CheckEquals('50,5', FloatToStr(FSchema.price.value));
end;

procedure TTestRadius.ObjectToJSON;
begin
  FSchema.AsJSON := FJSON;
  FJSONObject := TACBrJSONObject.Parse(FSchema.AsJSON);

  CheckEquals(1, FJSONObject.AsInteger['size']);
  CheckEquals(5, FJSONObject.AsInteger['estimateDeliveryTime']);
  CheckEquals('BRL', FJSONObject.AsJSONObject['price'].AsString['currency']);
  CheckEquals('50,5', FloatToStr(FJSONObject.AsJSONObject['price'].AsFloat['value']));
end;

procedure TTestRadius.SetUp;
begin
  inherited;
  FSchema := TACBrOpenDeliverySchemaRadius.Create;
  FJSON := ACBrStr(
    '{' +
      '"size": 1,' +
      '"price": {' +
        '"value": 50.5,' +
        '"currency": "BRL"' +
      '},' +
      '"estimateDeliveryTime": 5' +
    '}');
end;

procedure TTestRadius.TearDown;
begin
  FSchema.Free;
  FJSONObject.Free;
  inherited;
end;

{ TTestGeoRadius }

procedure TTestGeoRadius.JSONToObject;
begin
  FSchema.AsJSON := FJSON;
  CheckEquals('-23,54809', FloatToStr(FSchema.GeoMidpointLatitude));
  CheckEquals('-46,63638', FloatToStr(FSchema.GeoMidpointLongitude));
  CheckEquals(2, FSchema.Radius.Count);
  CheckEquals(3, FSchema.Radius[0].size);
  CheckEquals('5', FloatToStr(FSchema.Radius[0].price.value));
  CheckEquals('BRL', FSchema.Radius[0].price.currency);
  CheckEquals(2, FSchema.Radius[0].estimateDeliveryTime);

  CheckEquals(4, FSchema.Radius[1].size);
  CheckEquals('6', FloatToStr(FSchema.Radius[1].price.value));
  CheckEquals('US$', FSchema.Radius[1].price.currency);
  CheckEquals(8, FSchema.Radius[1].estimateDeliveryTime);
end;

procedure TTestGeoRadius.ObjectToJSON;
begin
  FSchema.AsJSON := FJSON;
  FJSONObject := TACBrJSONObject.Parse(FSchema.AsJSON);

  CheckEquals('-23,54809', FloatToStr(FJSONObject.AsFloat['geoMidpointLatitude']));
  CheckEquals('-46,63638', FloatToStr(FJSONObject.AsFloat['geoMidpointLongitude']));
  CheckEquals(2, FJSONObject.AsJSONArray['radius'].Count);
  CheckEquals(3, FJSONObject.AsJSONArray['radius'].ItemAsJSONObject[0].AsInteger['size']);
  CheckEquals(2, FJSONObject.AsJSONArray['radius'].ItemAsJSONObject[0].AsInteger['estimateDeliveryTime']);
  CheckEquals('BRL', FJSONObject.AsJSONArray['radius'].ItemAsJSONObject[0].AsJSONObject['price'].AsString['currency']);
  CheckEquals(5, FJSONObject.AsJSONArray['radius'].ItemAsJSONObject[0].AsJSONObject['price'].AsInteger['value']);

  CheckEquals(4, FJSONObject.AsJSONArray['radius'].ItemAsJSONObject[1].AsInteger['size']);
  CheckEquals(8, FJSONObject.AsJSONArray['radius'].ItemAsJSONObject[1].AsInteger['estimateDeliveryTime']);
  CheckEquals('US$', FJSONObject.AsJSONArray['radius'].ItemAsJSONObject[1].AsJSONObject['price'].AsString['currency']);
  CheckEquals(6, FJSONObject.AsJSONArray['radius'].ItemAsJSONObject[1].AsJSONObject['price'].AsInteger['value']);
end;

procedure TTestGeoRadius.SetUp;
begin
  inherited;
  FSchema := TACBrOpenDeliverySchemaGeoRadius.Create;
  FJSON := ACBrStr(
    '{' +
      '"geoMidpointLatitude": -23.54809,' +
      '"geoMidpointLongitude": -46.63638,' +
      '"radius": [' +
        '{' +
          '"size": 3,' +
          '"price": {' +
            '"value": 5,' +
            '"currency": "BRL"' +
          '},' +
          '"estimateDeliveryTime": 2' +
        '},' +
        '{' +
          '"size": 4,' +
          '"price": {' +
            '"value": 6,' +
            '"currency": "US$"' +
          '},' +
          '"estimateDeliveryTime": 8' +
        '}' +
      ']' +
    '}');
end;

procedure TTestGeoRadius.TearDown;
begin
  FSchema.Free;
  FJSONObject.Free;
  inherited;
end;

{ TTestGeoCoordinate }

procedure TTestGeoCoordinate.JSONToObject;
begin
  FSchema.AsJSON := FJSON;
  CheckEquals('-23,54809', FloatToStr(FSchema.Latitude));
  CheckEquals('-46,63638', FloatToStr(FSchema.Longitude));
end;

procedure TTestGeoCoordinate.ObjectToJSON;
begin
  FSchema.AsJSON := FJSON;
  FJSONObject := TACBrJSONObject.Parse(FSchema.AsJSON);

  CheckEquals('-23,54809', FloatToStr(FJSONObject.AsFloat['latitude']));
  CheckEquals('-46,63638', FloatToStr(FJSONObject.AsFloat['longitude']));
end;

procedure TTestGeoCoordinate.SetUp;
begin
  inherited;
  FSchema := TACBrOpenDeliverySchemaGeoCoordinate.Create;
  FJSON := ACBrStr(
    '{' +
      '"latitude": -23.54809,' +
      '"longitude": -46.63638' +
    '}');
end;

procedure TTestGeoCoordinate.TearDown;
begin
  FSchema.Free;
  FJSONObject.Free;
  inherited;
end;

{ TTestPolygon }

procedure TTestPolygon.JSONToObject;
begin
  FSchema.AsJSON := FJSON;
  CheckEquals(3, FSchema.estimateDeliveryTime);
  CheckEquals(1, FSchema.geoCoordinates.Count);
  CheckEquals('-23,54809', FloatToStr(FSchema.geoCoordinates[0].Latitude));
  CheckEquals('-46,63638', FloatToStr(FSchema.geoCoordinates[0].Longitude));
  CheckEquals('BRL', FSchema.price.currency);
  CheckEquals('5', FloatToStr(FSchema.price.value));
end;

procedure TTestPolygon.ObjectToJSON;
begin
  FSchema.AsJSON := FJSON;
  FJSONObject := TACBrJSONObject.Parse(FSchema.AsJSON);

  CheckEquals(3, FJSONObject.AsInteger['estimateDeliveryTime']);
  CheckEquals(1, FJSONObject.AsJSONArray['geoCoordinates'].Count);
  CheckEquals('-23,54809', FloatToStr(FJSONObject.AsJSONArray['geoCoordinates'].ItemAsJSONObject[0].AsFloat['latitude']));
  CheckEquals('-46,63638', FloatToStr(FJSONObject.AsJSONArray['geoCoordinates'].ItemAsJSONObject[0].AsFloat['longitude']));
  CheckEquals('BRL', FJSONObject.AsJSONObject['price'].AsString['currency']);
  CheckEquals('5', FloatToStr(FJSONObject.AsJSONObject['price'].AsFloat['value']));
end;

procedure TTestPolygon.SetUp;
begin
  inherited;
  FSchema := TACBrOpenDeliverySchemaPolygon.Create;
  FJSON := ACBrStr(
    '{' +
      '"geoCoordinates": [{' +
        '"latitude": -23.54809,' +
        '"longitude": -46.63638' +
      '}],' +
      '"price": {' +
        '"value": 5,' +
        '"currency": "BRL"' +
      '},' +
        '"estimateDeliveryTime": 3' +
    '}');
end;

procedure TTestPolygon.TearDown;
begin
  FSchema.Free;
  FJSONObject.Free;
  inherited;
end;

{ TTestServiceArea }

procedure TTestServiceArea.JSONToObject;
begin
  FSchema.AsJSON := FJSON;
  CheckEquals('01339e6d-520b-429e-bc7c-dcfd2df42278', FSchema.id);
  CheckEquals(1, FSchema.polygon.Count);
  CheckEquals(3, FSchema.polygon[0].estimateDeliveryTime);
  CheckEquals(1, FSchema.polygon[0].geoCoordinates.Count);
  CheckEquals('-23,54809', FloatToStr(FSchema.polygon[0].geoCoordinates[0].Latitude));
  CheckEquals('-46,63638', FloatToStr(FSchema.polygon[0].geoCoordinates[0].Longitude));
  CheckEquals('BRL', FSchema.polygon[0].price.currency);
  CheckEquals('5', FloatToStr(FSchema.polygon[0].price.value));

  CheckEquals('-23,54809', FloatToStr(FSchema.geoRadius.GeoMidpointLatitude));
  CheckEquals('-46,63638', FloatToStr(FSchema.geoRadius.GeoMidpointLongitude));
  CheckEquals(1, FSchema.geoRadius.Radius.Count);
  CheckEquals(0, FSchema.geoRadius.Radius[0].size);
  CheckEquals('0', FloatToStr(FSchema.geoRadius.Radius[0].price.value));
  CheckEquals('', FSchema.geoRadius.Radius[0].price.currency);
  CheckEquals(0, FSchema.geoRadius.Radius[0].estimateDeliveryTime);
end;

procedure TTestServiceArea.ObjectToJSON;
begin
  FSchema.AsJSON := FJSON;
  FJSONObject := TACBrJSONObject.Parse(FSchema.AsJSON);

  CheckEquals('01339e6d-520b-429e-bc7c-dcfd2df42278', FJSONObject.AsString['id']);
  CheckEquals(1, FJSONObject.AsJSONArray['polygon'].Count);

  CheckEquals(3, FJSONObject.AsJSONArray['polygon'].ItemAsJSONObject[0].AsInteger['estimateDeliveryTime']);
  CheckEquals(1, FJSONObject.AsJSONArray['polygon'].ItemAsJSONObject[0].AsJSONArray['geoCoordinates'].Count);
  CheckEquals('BRL', FJSONObject.AsJSONArray['polygon'].ItemAsJSONObject[0].AsJSONObject['price'].AsString['currency']);
  CheckEquals('5', FloatToStr(FJSONObject.AsJSONArray['polygon'].ItemAsJSONObject[0].AsJSONObject['price'].AsFloat['value']));
  CheckEquals('-23,54809', FloatToStr(FJSONObject.AsJSONArray['polygon'].ItemAsJSONObject[0].AsJSONArray['geoCoordinates'].ItemAsJSONObject[0].AsFloat['latitude']));
  CheckEquals('-46,63638', FloatToStr(FJSONObject.AsJSONArray['polygon'].ItemAsJSONObject[0].AsJSONArray['geoCoordinates'].ItemAsJSONObject[0].AsFloat['longitude']));


  CheckEquals('-23,54809', FloatToStr(FJSONObject.AsJSONObject['geoRadius'].AsFloat['geoMidpointLatitude']));
  CheckEquals('-46,63638', FloatToStr(FJSONObject.AsJSONObject['geoRadius'].AsFloat['geoMidpointLongitude']));
  CheckEquals(1, FJSONObject.AsJSONObject['geoRadius'].AsJSONArray['radius'].Count);
  CheckEquals(0, FJSONObject.AsJSONObject['geoRadius'].AsJSONArray['radius'].ItemAsJSONObject[0].AsInteger['size']);
  CheckEquals('0', FloatToStr(FJSONObject.AsJSONObject['geoRadius'].AsJSONArray['radius'].ItemAsJSONObject[0].AsJSONObject['price'].AsFloat['value']));
  CheckEquals('', FJSONObject.AsJSONObject['geoRadius'].AsJSONArray['radius'].ItemAsJSONObject[0].AsJSONObject['price'].AsString['currency']);
  CheckEquals(0, FJSONObject.AsJSONObject['geoRadius'].AsJSONArray['radius'].ItemAsJSONObject[0].AsInteger['estimateDeliveryTime']);
end;

procedure TTestServiceArea.SetUp;
begin
  inherited;
  FSchema := TACBrOpenDeliverySchemaServiceArea.Create;
  FJSON := ACBrStr(
    '{' +
      '"id": "01339e6d-520b-429e-bc7c-dcfd2df42278",' +
      '"polygon": [{' +
        '"geoCoordinates": [{' +
          '"latitude": -23.54809,' +
          '"longitude": -46.63638' +
        '}],' +
        '"price": {' +
          '"value": 5,' +
          '"currency": "BRL"' +
        '},' +
        '"estimateDeliveryTime": 3' +
      '}],' +
      '"geoRadius": {' +
        '"geoMidpointLatitude": -23.54809,' +
        '"geoMidpointLongitude": -46.63638,' +
        '"radius": [{' +
          '"size": 0,' +
          '"price": {},' +
          '"estimateDeliveryTime": 0' +
        '}]' +
      '}' +
    '}');
end;

procedure TTestServiceArea.TearDown;
begin
  FSchema.Free;
  FJSONObject.Free;
  inherited;
end;

{ TTestTimePeriod }

procedure TTestTimePeriod.JSONToObject;
begin
  FSchema.AsJSON := FJSON;
  CheckEquals('10:01:02', FormatDateTime('hh:mm:ss', FSchema.startTime));
  CheckEquals('18:01:02', FormatDateTime('hh:mm:ss', FSchema.endTime));
end;

procedure TTestTimePeriod.ObjectToJSON;
begin
  FSchema.AsJSON := FJSON;
  FJSONObject := TACBrJSONObject.Parse(FSchema.AsJSON);
  CheckEquals('10:01:02', FormatDateTime('hh:mm:ss', FSchema.startTime));
  CheckEquals('18:01:02', FormatDateTime('hh:mm:ss', FSchema.endTime));
end;

procedure TTestTimePeriod.SetUp;
begin
  inherited;
  FSchema := TACBrOpenDeliverySchemaTimePeriod.Create;
  FJSON := ACBrStr(
    '{' +
      '"startTime": "10:01:02.000Z",' +
      '"endTime": "18:01:02.000Z"' +
    '}');
end;

procedure TTestTimePeriod.TearDown;
begin
  FSchema.Free;
  FJSONObject.Free;
  inherited;
end;

{ TTestHour }

procedure TTestHour.JSONToObject;
begin
  FSchema.AsJSON := FJSON;
  CheckEquals(7, Length(FSchema.dayOfWeek));
  CheckEquals('MONDAY', DayOfWeekToStr(FSchema.dayOfWeek[0]));
  CheckEquals('SUNDAY', DayOfWeekToStr(FSchema.dayOfWeek[6]));
  CheckEquals('10:01:02', FormatDateTime('hh:mm:ss', FSchema.timePeriods.startTime));
  CheckEquals('18:01:02', FormatDateTime('hh:mm:ss', FSchema.timePeriods.endTime));
end;

procedure TTestHour.ObjectToJSON;
begin
  FSchema.AsJSON := FJSON;
  FJSONObject := TACBrJSONObject.Parse(FSchema.AsJSON);
  CheckEquals(7, FJSONObject.AsJSONArray['dayOfWeek'].Count);
  CheckEquals('MONDAY', FJSONObject.AsJSONArray['dayOfWeek'].Items[0]);
  CheckEquals('SUNDAY', FJSONObject.AsJSONArray['dayOfWeek'].Items[6]);
  CheckEquals('10:01:02', FormatDateTime('hh:mm:ss', FJSONObject.AsJSONObject['timePeriods'].AsISOTime['startTime']));
  CheckEquals('18:01:02', FormatDateTime('hh:mm:ss', FJSONObject.AsJSONObject['timePeriods'].AsISOTime['endTime']));
end;

procedure TTestHour.SetUp;
begin
  inherited;
  FSchema := TACBrOpenDeliverySchemaHour.Create;
  FJSON := ACBrStr(
    '{' +
      '"dayOfWeek": [' +
        '"MONDAY",' +
        '"TUESDAY",' +
        '"WEDNESDAY",' +
        '"THURSDAY",' +
        '"FRIDAY",' +
        '"SATURDAY",' +
        '"SUNDAY"' +
      '],' +
      '"timePeriods": {' +
        '"startTime": "10:01:02.000Z",' +
        '"endTime": "18:01:02.000Z"' +
      '}' +
    '}');
end;

procedure TTestHour.TearDown;
begin
  FSchema.Free;
  FJSONObject.Free;
  inherited;
end;

{ TTestHolidayHour }

procedure TTestHolidayHour.JSONToObject;
begin
  FSchema.AsJSON := FJSON;
  CheckEquals('2021-07-04', FormatDateTime('yyyy-MM-dd', FSchema.Date));
  CheckEquals('10:00:00', FormatDateTime('hh:mm:ss', FSchema.timePeriods.startTime));
  CheckEquals('18:00:00', FormatDateTime('hh:mm:ss', FSchema.timePeriods.endTime));
end;

procedure TTestHolidayHour.ObjectToJSON;
begin
  FSchema.AsJSON := FJSON;
  FJSONObject := TACBrJSONObject.Parse(FSchema.AsJSON);
  CheckEquals('2021-07-04', FormatDateTime('yyyy-MM-dd', FJSONObject.AsISODateTime['date']));
  CheckEquals('10:00:00', FormatDateTime('hh:mm:ss', FJSONObject.AsJSONObject['timePeriods'].AsISOTime['startTime']));
  CheckEquals('18:00:00', FormatDateTime('hh:mm:ss', FJSONObject.AsJSONObject['timePeriods'].AsISOTime['endTime']));
end;

procedure TTestHolidayHour.SetUp;
begin
  inherited;
  FSchema := TACBrOpenDeliverySchemaHolidayHour.Create;
  FJSON := ACBrStr(
    '{' +
      '"date": "2021-07-04",' +
      '"timePeriods": {' +
        '"startTime": "10:00:00.000Z",' +
        '"endTime": "18:00:00.000Z"' +
      '}' +
    '}');
end;

procedure TTestHolidayHour.TearDown;
begin
  FSchema.Free;
  FJSONObject.Free;
  inherited;
end;

{ TTestServiceHour }

procedure TTestServiceHour.JSONToObject;
begin
  FSchema.AsJSON := FJSON;
  CheckEquals('fb093d8c-2ca5-40fb-afcf-472fbdae81cc', FSchema.id);
  CheckEquals(1, FSchema.weekHours.Count);
  CheckEquals(2, Length(FSchema.weekHours[0].dayOfWeek));
  CheckEquals('MONDAY', DayOfWeekToStr(FSchema.weekHours[0].dayOfWeek[0]));
  CheckEquals('SUNDAY', DayOfWeekToStr(FSchema.weekHours[0].dayOfWeek[1]));
  CheckEquals('10:00:00', FormatDateTime('hh:mm:ss', FSchema.weekHours[0].timePeriods.startTime));
  CheckEquals('18:00:00', FormatDateTime('hh:mm:ss', FSchema.weekHours[0].timePeriods.endTime));

  CheckEquals(1, FSchema.holidayHours.Count);
  CheckEquals('2021-07-04', FormatDateTime('yyyy-MM-dd', FSchema.holidayHours[0].Date));
  CheckEquals('10:00:00', FormatDateTime('hh:mm:ss', FSchema.holidayHours[0].timePeriods.startTime));
  CheckEquals('18:00:00', FormatDateTime('hh:mm:ss', FSchema.holidayHours[0].timePeriods.endTime));
end;

procedure TTestServiceHour.ObjectToJSON;
var
  LJSON: string;
begin
  FSchema.AsJSON := FJSON;
  LJSON := FSchema.AsJSON;
  FJSONObject := TACBrJSONObject.Parse(LJSON);
  CheckEquals('fb093d8c-2ca5-40fb-afcf-472fbdae81cc', FJSONObject.AsString['id']);
  CheckEquals(1, FJSONObject.AsJSONArray['weekHours'].Count);
  CheckEquals(2, FJSONObject.AsJSONArray['weekHours'].ItemAsJSONObject[0].AsJSONArray['dayOfWeek'].Count);
  CheckEquals('MONDAY', FJSONObject.AsJSONArray['weekHours'].ItemAsJSONObject[0].AsJSONArray['dayOfWeek'].Items[0]);
  CheckEquals('SUNDAY', FJSONObject.AsJSONArray['weekHours'].ItemAsJSONObject[0].AsJSONArray['dayOfWeek'].Items[1]);
  CheckEquals('10:00:00', FormatDateTime('hh:mm:ss', FJSONObject.AsJSONArray['weekHours'].ItemAsJSONObject[0].AsJSONObject['timePeriods'].AsISOTime['startTime']));
  CheckEquals('18:00:00', FormatDateTime('hh:mm:ss', FJSONObject.AsJSONArray['weekHours'].ItemAsJSONObject[0].AsJSONObject['timePeriods'].AsISOTime['endTime']));

  CheckEquals(1, FJSONObject.AsJSONArray['holidayHours'].Count);
  CheckEquals('2021-07-04', FormatDateTime('yyyy-MM-dd', FJSONObject.AsJSONArray['holidayHours'].ItemAsJSONObject[0].AsISODate['date']));
  CheckEquals('10:00:00', FormatDateTime('hh:mm:ss', FJSONObject.AsJSONArray['holidayHours'].ItemAsJSONObject[0].AsJSONObject['timePeriods'].AsISOTime['startTime']));
  CheckEquals('18:00:00', FormatDateTime('hh:mm:ss', FJSONObject.AsJSONArray['holidayHours'].ItemAsJSONObject[0].AsJSONObject['timePeriods'].AsISOTime['endTime']));
end;

procedure TTestServiceHour.SetUp;
begin
  inherited;
  FSchema := TACBrOpenDeliverySchemaServiceHour.Create;
  FJSON := ACBrStr(
    '{' +
      '"id": "fb093d8c-2ca5-40fb-afcf-472fbdae81cc",' +
      '"weekHours": [{' +
        '"dayOfWeek": ["MONDAY", "SUNDAY"],' +
        '"timePeriods": {' +
          '"startTime": "10:00:00.000Z",' +
          '"endTime": "18:00:00.000Z"' +
        '}' +
      '}],' +
      '"holidayHours": [{' +
        '"date": "2021-07-04",' +
        '"timePeriods": {' +
          '"startTime": "10:00:00.000Z",' +
          '"endTime": "18:00:00.000Z"' +
        '}' +
      '}]' +
    '}');
end;

procedure TTestServiceHour.TearDown;
begin
  FSchema.Free;
  FJSONObject.Free;
  inherited;
end;

{ TTestService }

procedure TTestService.JSONToObject;
begin
  FSchema.AsJSON := FJSON;
  CheckEquals('f078e8e2-3044-4eec-b4a8-8359810de123', FSchema.id);
  CheckEquals('AVAILABLE', StatusToStr(FSchema.status));
  CheckEquals('DELIVERY', ServiceTypeToStr(FSchema.serviceType));
  CheckEquals('f627ccdc-6789-456f-a782-148538d5035b', FSchema.menuId);

  CheckEquals('01339e6d-520b-429e-bc7c-dcfd2df42278', FSchema.serviceArea.id);
  CheckEquals(1, FSchema.serviceArea.polygon.Count);
  CheckEquals(1, FSchema.serviceArea.polygon[0].estimateDeliveryTime);
  CheckEquals(1, FSchema.serviceArea.polygon[0].geoCoordinates.Count);
  CheckEquals('-23,54809', FloatToStr(FSchema.serviceArea.polygon[0].geoCoordinates[0].Latitude));
  CheckEquals('-46,63638', FloatToStr(FSchema.serviceArea.polygon[0].geoCoordinates[0].Longitude));
  CheckEquals('BRL', FSchema.serviceArea.polygon[0].price.currency);
  CheckEquals('5', FloatToStr(FSchema.serviceArea.polygon[0].price.value));

  CheckEquals('-23,54809', FloatToStr(FSchema.serviceArea.geoRadius.GeoMidpointLatitude));
  CheckEquals('-46,63638', FloatToStr(FSchema.serviceArea.geoRadius.GeoMidpointLongitude));
  CheckEquals(1, FSchema.serviceArea.geoRadius.Radius.Count);
  CheckEquals(1, FSchema.serviceArea.geoRadius.Radius[0].size);
  CheckEquals('5', FloatToStr(FSchema.serviceArea.geoRadius.Radius[0].price.value));
  CheckEquals('BRL', FSchema.serviceArea.geoRadius.Radius[0].price.currency);
  CheckEquals(1, FSchema.serviceArea.geoRadius.Radius[0].estimateDeliveryTime);

  // Service Hours
  CheckEquals('fb093d8c-2ca5-40fb-afcf-472fbdae81cc', FSchema.serviceHours.id);
  CheckEquals(1, FSchema.serviceHours.weekHours.Count);
  CheckEquals(7, Length(FSchema.serviceHours.weekHours[0].dayOfWeek));
  CheckEquals('MONDAY', DayOfWeekToStr(FSchema.serviceHours.weekHours[0].dayOfWeek[0]));
  CheckEquals('TUESDAY', DayOfWeekToStr(FSchema.serviceHours.weekHours[0].dayOfWeek[1]));
  CheckEquals('10:00:00', FormatDateTime('hh:mm:ss', FSchema.serviceHours.weekHours[0].timePeriods.startTime));
  CheckEquals('18:00:00', FormatDateTime('hh:mm:ss', FSchema.serviceHours.weekHours[0].timePeriods.endTime));

  CheckEquals(1, FSchema.serviceHours.holidayHours.Count);
  CheckEquals('2021-04-07', FormatDateTime('yyyy-MM-dd', FSchema.serviceHours.holidayHours[0].Date));
  CheckEquals('10:00:00', FormatDateTime('hh:mm:ss', FSchema.serviceHours.holidayHours[0].timePeriods.startTime));
  CheckEquals('18:00:00', FormatDateTime('hh:mm:ss', FSchema.serviceHours.holidayHours[0].timePeriods.endTime));
end;

procedure TTestService.ObjectToJSON;
var
  LJSON: TACBrJSONObject;
begin
  FSchema.AsJSON := FJSON;
  FJSONObject := TACBrJSONObject.Parse(FSchema.AsJSON);

  CheckEquals('f078e8e2-3044-4eec-b4a8-8359810de123', FJSONObject.AsString['id']);
  CheckEquals('AVAILABLE', FJSONObject.AsString['status']);
  CheckEquals('DELIVERY', FJSONObject.AsString['serviceType']);
  CheckEquals('f627ccdc-6789-456f-a782-148538d5035b', FJSONObject.AsString['menuId']);

  // Service Area
  LJSON := FJSONObject.AsJSONObject['serviceArea'];
  CheckEquals('01339e6d-520b-429e-bc7c-dcfd2df42278', LJSON.AsString['id']);
  CheckEquals(1, LJSON.AsJSONArray['polygon'].Count);

  CheckEquals(1, LJSON.AsJSONArray['polygon'].ItemAsJSONObject[0].AsInteger['estimateDeliveryTime']);
  CheckEquals(1, LJSON.AsJSONArray['polygon'].ItemAsJSONObject[0].AsJSONArray['geoCoordinates'].Count);
  CheckEquals('BRL', LJSON.AsJSONArray['polygon'].ItemAsJSONObject[0].AsJSONObject['price'].AsString['currency']);
  CheckEquals('5', FloatToStr(LJSON.AsJSONArray['polygon'].ItemAsJSONObject[0].AsJSONObject['price'].AsFloat['value']));
  CheckEquals('-23,54809', FloatToStr(LJSON.AsJSONArray['polygon'].ItemAsJSONObject[0].AsJSONArray['geoCoordinates'].ItemAsJSONObject[0].AsFloat['latitude']));
  CheckEquals('-46,63638', FloatToStr(LJSON.AsJSONArray['polygon'].ItemAsJSONObject[0].AsJSONArray['geoCoordinates'].ItemAsJSONObject[0].AsFloat['longitude']));

  CheckEquals('-23,54809', FloatToStr(LJSON.AsJSONObject['geoRadius'].AsFloat['geoMidpointLatitude']));
  CheckEquals('-46,63638', FloatToStr(LJSON.AsJSONObject['geoRadius'].AsFloat['geoMidpointLongitude']));
  CheckEquals(1, LJSON.AsJSONObject['geoRadius'].AsJSONArray['radius'].Count);
  CheckEquals(1, LJSON.AsJSONObject['geoRadius'].AsJSONArray['radius'].ItemAsJSONObject[0].AsInteger['size']);
  CheckEquals('5', FloatToStr(LJSON.AsJSONObject['geoRadius'].AsJSONArray['radius'].ItemAsJSONObject[0].AsJSONObject['price'].AsFloat['value']));
  CheckEquals('BRL', LJSON.AsJSONObject['geoRadius'].AsJSONArray['radius'].ItemAsJSONObject[0].AsJSONObject['price'].AsString['currency']);
  CheckEquals(1, LJSON.AsJSONObject['geoRadius'].AsJSONArray['radius'].ItemAsJSONObject[0].AsInteger['estimateDeliveryTime']);

  // Service Hours
  LJSON := FJSONObject.AsJSONObject['serviceHours'];
  CheckEquals('fb093d8c-2ca5-40fb-afcf-472fbdae81cc', LJSON.AsString['id']);
  CheckEquals(1, LJSON.AsJSONArray['weekHours'].Count);
  CheckEquals(7, LJSON.AsJSONArray['weekHours'].ItemAsJSONObject[0].AsJSONArray['dayOfWeek'].Count);
  CheckEquals('MONDAY', LJSON.AsJSONArray['weekHours'].ItemAsJSONObject[0].AsJSONArray['dayOfWeek'].Items[0]);
  CheckEquals('TUESDAY', LJSON.AsJSONArray['weekHours'].ItemAsJSONObject[0].AsJSONArray['dayOfWeek'].Items[1]);
  CheckEquals('10:00:00', FormatDateTime('hh:mm:ss', LJSON.AsJSONArray['weekHours'].ItemAsJSONObject[0].AsJSONObject['timePeriods'].AsISOTime['startTime']));
  CheckEquals('18:00:00', FormatDateTime('hh:mm:ss', LJSON.AsJSONArray['weekHours'].ItemAsJSONObject[0].AsJSONObject['timePeriods'].AsISOTime['endTime']));

  CheckEquals(1, LJSON.AsJSONArray['holidayHours'].Count);
  CheckEquals('2021-04-07', FormatDateTime('yyyy-MM-dd', LJSON.AsJSONArray['holidayHours'].ItemAsJSONObject[0].AsISODate['date']));
  CheckEquals('10:00:00', FormatDateTime('hh:mm:ss', LJSON.AsJSONArray['holidayHours'].ItemAsJSONObject[0].AsJSONObject['timePeriods'].AsISOTime['startTime']));
  CheckEquals('18:00:00', FormatDateTime('hh:mm:ss', LJSON.AsJSONArray['holidayHours'].ItemAsJSONObject[0].AsJSONObject['timePeriods'].AsISOTime['endTime']));
end;

procedure TTestService.SetUp;
begin
  inherited;
  FSchema := TACBrOpenDeliverySchemaService.Create;
  FJSON := ACBrStr(
    '{' +
    '    "id": "f078e8e2-3044-4eec-b4a8-8359810de123",' +
    '    "status": "AVAILABLE",' +
    '    "serviceType": "DELIVERY",' +
    '    "menuId": "f627ccdc-6789-456f-a782-148538d5035b",' +
    '    "serviceArea": {' +
    '        "id": "01339e6d-520b-429e-bc7c-dcfd2df42278",' +
    '        "polygon": [{' +
    '                "geoCoordinates": [{' +
    '                        "latitude": -23.54809,' +
    '                        "longitude": -46.63638' +
    '                    }' +
    '                ],' +
    '                "price": {' +
    '                    "value": 5,' +
    '                    "currency": "BRL"' +
    '                },' +
    '                "estimateDeliveryTime": 1' +
    '            }' +
    '        ],' +
    '        "geoRadius": {' +
    '            "geoMidpointLatitude": -23.54809,' +
    '            "geoMidpointLongitude": -46.63638,' +
    '            "radius": [{' +
    '                    "size": 1,' +
    '                    "price": {' +
    '                        "value": 5,' +
    '                        "currency": "BRL"' +
    '                    },' +
    '                    "estimateDeliveryTime": 1' +
    '                }' +
    '            ]' +
    '        }' +
    '    },' +
    '    "serviceHours": {' +
    '        "id": "fb093d8c-2ca5-40fb-afcf-472fbdae81cc",' +
    '        "weekHours": [{' +
    '                "dayOfWeek": [' +
    '                    "MONDAY",' +
    '                    "TUESDAY",' +
    '                    "WEDNESDAY",' +
    '                    "THURSDAY",' +
    '                    "FRIDAY",' +
    '                    "SATURDAY",' +
    '                    "SUNDAY"' +
    '                ],' +
    '                "timePeriods": {' +
    '                    "startTime": "10:00:00.000Z",' +
    '                    "endTime": "18:00:00.000Z"' +
    '                }' +
    '            }' +
    '        ],' +
    '        "holidayHours": [{' +
    '                "date": "2021-04-07",' +
    '                "timePeriods": {' +
    '                    "startTime": "10:00:00.000Z",' +
    '                    "endTime": "18:00:00.000Z"' +
    '                }' +
    '            }' +
    '        ]' +
    '    }' +
    '}');
end;

procedure TTestService.TearDown;
begin
  FSchema.Free;
  FJSONObject.Free;
  inherited;
end;

{ TTestMenu }

procedure TTestMenu.JSONToObject;
begin
  FSchema.AsJSON := FJSON;
  CheckEquals('f627ccdc-6789-456f-a782-148538d5035b', FSchema.Id);
  CheckEquals('Pizzas', FSchema.Name);
  CheckEquals('Pizza menu', FSchema.Description);
  CheckEquals('123', FSchema.ExternalCode);
  CheckEquals('Lorem Ipsum is simply dummy text of the printing and typesetting industry.', FSchema.Disclaimer);
  CheckEquals('http://example.com', FSchema.DisclaimerUrl);
  CheckEquals(2, Length(FSchema.CategoryId));
  CheckEquals('92fad022-2c28-4239-a026-989f5b555cb7', FSchema.CategoryId[0]);
  CheckEquals('6bb71850-1d40-49f9-8046-b13e068c0cca', FSchema.CategoryId[1]);
end;

procedure TTestMenu.ObjectToJSON;
begin
  FSchema.AsJSON := FJSON;
  FJSONObject := TACBrJSONObject.Parse(FSchema.AsJSON);

  CheckEquals('f627ccdc-6789-456f-a782-148538d5035b', FJSONObject.AsString['id']);
  CheckEquals('Pizzas', FJSONObject.AsString['name']);
  CheckEquals('Pizza menu', FJSONObject.AsString['description']);
  CheckEquals('123', FJSONObject.AsString['externalCode']);
  CheckEquals('Lorem Ipsum is simply dummy text of the printing and typesetting industry.', FJSONObject.AsString['disclaimer']);
  CheckEquals('http://example.com', FJSONObject.AsString['disclaimerURL']);
  CheckEquals(2, FJSONObject.AsJSONArray['categoryId'].Count);
  CheckEquals('92fad022-2c28-4239-a026-989f5b555cb7', FJSONObject.AsJSONArray['categoryId'].Items[0]);
  CheckEquals('6bb71850-1d40-49f9-8046-b13e068c0cca', FJSONObject.AsJSONArray['categoryId'].Items[1]);
end;

procedure TTestMenu.SetUp;
begin
  inherited;
  FSchema := TACBrOpenDeliverySchemaMenu.Create;
  FJSON := ACBrStr(
    '{' +
    '    "id": "f627ccdc-6789-456f-a782-148538d5035b",' +
    '    "name": "Pizzas",' +
    '    "description": "Pizza menu",' +
    '    "externalCode": "123",' +
    '    "disclaimer": "Lorem Ipsum is simply dummy text of the printing and typesetting industry.",' +
    '    "disclaimerURL": "http://example.com",' +
    '    "categoryId": [' +
    '        "92fad022-2c28-4239-a026-989f5b555cb7",' +
    '        "6bb71850-1d40-49f9-8046-b13e068c0cca"' +
    '    ]' +
    '}');
end;

procedure TTestMenu.TearDown;
begin
  FSchema.Free;
  FJSONObject.Free;
  inherited;
end;

{ TTestCategory }

procedure TTestCategory.JSONToObject;
begin
  FSchema.AsJSON := FJSON;
  CheckEquals('92fad022-2c28-4239-a026-989f5b555cb7', FSchema.id);
  CheckEquals(2, FSchema.Index);
  CheckEquals('Salted Pizza', FSchema.Name);
  CheckEquals('Salted pizza flavors.', FSchema.Description);
  CheckEquals('13', FSchema.ExternalCode);
  CheckEquals('AVAILABLE', StatusToStr(FSchema.Status));

  CheckEquals('https://food-company.com/category1.png', FSchema.Image.URL);
  CheckEquals('09345UIHF98', FSchema.Image.CRC_32);
  CheckEquals(1, Length(FSchema.AvailabilityId));
  CheckEquals(1, Length(FSchema.ItemOfferId));
  CheckEquals('11d063c4-73a7-4f87-a0eb-71636cc02029', FSchema.AvailabilityId[0]);
  CheckEquals('f080cfb3-5c4a-4eb7-907d-2de3bbb5dfb9', FSchema.ItemOfferId[0]);
end;

procedure TTestCategory.ObjectToJSON;
begin
  FSchema.AsJSON := FJSON;
  FJSONObject := TACBrJSONObject.Parse(FSchema.AsJSON);

  CheckEquals('92fad022-2c28-4239-a026-989f5b555cb7', FJSONObject.AsString['id']);
  CheckEquals(2, FJSONObject.AsInteger['index']);
  CheckEquals('Salted Pizza', FJSONObject.AsString['name']);
  CheckEquals('Salted pizza flavors.', FJSONObject.AsString['description']);
  CheckEquals('13', FJSONObject.AsString['externalCode']);
  CheckEquals('AVAILABLE', FJSONObject.AsString['status']);

  CheckEquals('https://food-company.com/category1.png', FJSONObject.AsJSONObject['image'].AsString['URL']);
  CheckEquals('09345UIHF98', FJSONObject.AsJSONObject['image'].AsString['CRC-32']);

  CheckEquals(1, FJSONObject.AsJSONArray['availabilityId'].Count);
  CheckEquals(1, FJSONObject.AsJSONArray['itemOfferId'].Count);
  CheckEquals('11d063c4-73a7-4f87-a0eb-71636cc02029', FJSONObject.AsJSONArray['availabilityId'].Items[0]);
  CheckEquals('f080cfb3-5c4a-4eb7-907d-2de3bbb5dfb9', FJSONObject.AsJSONArray['itemOfferId'].Items[0]);
end;

procedure TTestCategory.SetUp;
begin
  inherited;
  FSchema := TACBrOpenDeliverySchemaCategory.Create;
  FJSON := ACBrStr(
    '{' +
    '    "id": "92fad022-2c28-4239-a026-989f5b555cb7",' +
    '    "index": 2,' +
    '    "name": "Salted Pizza",' +
    '    "description": "Salted pizza flavors.",' +
    '    "image": {' +
    '        "URL": "https://food-company.com/category1.png",' +
    '        "CRC-32": "09345UIHF98"' +
    '    },' +
    '    "externalCode": "13",' +
    '    "status": "AVAILABLE",' +
    '    "availabilityId": [' +
    '        "11d063c4-73a7-4f87-a0eb-71636cc02029"' +
    '    ],' +
    '    "itemOfferId": [' +
    '        "f080cfb3-5c4a-4eb7-907d-2de3bbb5dfb9"' +
    '    ]' +
    '}');
end;

procedure TTestCategory.TearDown;
begin
  FSchema.Free;
  FJSONObject.Free;
  inherited;
end;

{ TTestItemOffer }

procedure TTestItemOffer.JSONToObject;
begin
  FSchema.AsJSON := FJSON;

  CheckEquals('f080cfb3-5c4a-4eb7-907d-2de3bbb5dfb9', FSchema.Id);
  CheckEquals('732bd31e-77fc-47ee-88ee-a0437f97b198', FSchema.ItemId);
  CheckEquals(1, FSchema.Index);
  CheckEquals('UNAVAILABLE', StatusToStr(FSchema.status));
  CheckEquals('43', FloatToStr(FSchema.Price.value));
  CheckEquals('BRL', FSchema.Price.currency);
  CheckEquals(1, Length(FSchema.AvailabilityId));
  CheckEquals(1, Length(FSchema.OptionGroupsId));
  CheckEquals('11d063c4-73a7-4f87-a0eb-71636cc02029', FSchema.AvailabilityId[0]);
  CheckEquals('fe67e551-f42f-499a-8afb-0ed893c71fa3', FSchema.OptionGroupsId[0]);
end;

procedure TTestItemOffer.ObjectToJSON;
begin
  FSchema.AsJSON := FJSON;
  FJSONObject := TACBrJSONObject.Parse(FSchema.AsJSON);

  CheckEquals('f080cfb3-5c4a-4eb7-907d-2de3bbb5dfb9', FJSONObject.AsString['id']);
  CheckEquals('732bd31e-77fc-47ee-88ee-a0437f97b198', FJSONObject.AsString['itemId']);
  CheckEquals(1, FJSONObject.AsInteger['index']);
  CheckEquals('UNAVAILABLE', FJSONObject.AsString['status']);
  CheckEquals('43', FloatToStr(FJSONObject.AsJSONObject['price'].AsFloat['value']));
  CheckEquals('BRL', FJSONObject.AsJSONObject['price'].AsString['currency']);
  CheckEquals(1, FJSONObject.AsJSONArray['availabilityId'].Count);
  CheckEquals(1, FJSONObject.AsJSONArray['optionGroupsId'].Count);
  CheckEquals('11d063c4-73a7-4f87-a0eb-71636cc02029', FJSONObject.AsJSONArray['availabilityId'].Items[0]);
  CheckEquals('fe67e551-f42f-499a-8afb-0ed893c71fa3', FJSONObject.AsJSONArray['optionGroupsId'].Items[0]);
end;

procedure TTestItemOffer.SetUp;
begin
  inherited;
  FSchema := TACBrOpenDeliverySchemaItemOffer.Create;
  FJSON := ACBrStr(
    '{' +
    '    "id": "f080cfb3-5c4a-4eb7-907d-2de3bbb5dfb9",' +
    '    "itemId": "732bd31e-77fc-47ee-88ee-a0437f97b198",' +
    '    "index": 1,' +
    '    "status": "UNAVAILABLE",' +
    '    "price": {' +
    '        "value": 43,' +
    '        "originalValue": 43,' +
    '        "currency": "BRL"' +
    '    },' +
    '    "availabilityId": [' +
    '        "11d063c4-73a7-4f87-a0eb-71636cc02029"' +
    '    ],' +
    '    "optionGroupsId": [' +
    '        "fe67e551-f42f-499a-8afb-0ed893c71fa3"' +
    '    ]' +
    '}');
end;

procedure TTestItemOffer.TearDown;
begin
  FSchema.Free;
  FJSONObject.Free;
  inherited;
end;

{ TTestNutritionalInfo }

procedure TTestNutritionalInfo.JSONToObject;
begin
  FSchema.AsJSON := FJSON;
  CheckEquals('Contains preservatives', FSchema.Description);
  CheckEquals('2000 Cal', FSchema.Calories);
  CheckEquals(1, Length(FSchema.Allergen));
  CheckEquals(1, Length(FSchema.Additives));
  CheckEquals(1, Length(FSchema.SuitableDiet));
  CheckEquals('GLUTEN', AllergenToStr(FSchema.Allergen[0]));
  CheckEquals('teste', FSchema.Additives[0]);
  CheckEquals('GLUTEN_FREE', SuitableDietToStr(FSchema.SuitableDiet[0]));
  CheckFalse(FSchema.IsAlcoholic);
end;

procedure TTestNutritionalInfo.ObjectToJSON;
begin
  FSchema.AsJSON := FJSON;
  FJSONObject := TACBrJSONObject.Parse(FSchema.AsJSON);

  CheckEquals('Contains preservatives', FJSONObject.AsString['description']);
  CheckEquals('2000 Cal', FJSONObject.AsString['calories']);
  CheckEquals(1, FJSONObject.AsJSONArray['allergen'].Count);
  CheckEquals(1, FJSONObject.AsJSONArray['additives'].Count);
  CheckEquals(1, FJSONObject.AsJSONArray['suitableDiet'].Count);
  CheckEquals('GLUTEN', FJSONObject.AsJSONArray['allergen'].Items[0]);
  CheckEquals('teste', FJSONObject.AsJSONArray['additives'].Items[0]);
  CheckEquals('GLUTEN_FREE', FJSONObject.AsJSONArray['suitableDiet'].Items[0]);
  CheckFalse(FSchema.IsAlcoholic);
end;

procedure TTestNutritionalInfo.SetUp;
begin
  inherited;
  FSchema := TACBrOpenDeliverySchemaNutritionalInfo.Create;
  FJSON := ACBrStr(
    '{' +
      '"description": "Contains preservatives",' +
      '"calories": "2000 Cal",' +
      '"allergen": [' +
      ' "GLUTEN"' +
      '],' +
      '"additives": [' +
      ' "teste"' +
      '],' +
      '"suitableDiet": [' +
      ' "GLUTEN_FREE"' +
      ']' +
    '}');
end;

procedure TTestNutritionalInfo.TearDown;
begin
  FSchema.Free;
  FJSONObject.Free;
  inherited;
end;

{ TTestItem }

procedure TTestItem.JSONToObject;
begin
  FSchema.AsJSON := FJSON;
  CheckEquals('7e507cab-7235-4f75-a0c2-e955fb2f2048', FSchema.Id);
  CheckEquals('1/2 Mozzarella', FSchema.Name);
  CheckEquals('Delicious mozzarella pizza.', FSchema.Description);
  CheckEquals('24', FSchema.ExternalCode);
  CheckEquals('UNAVAILABLE', StatusToStr(FSchema.status));
  CheckEquals('unit', FSchema._unit);
  CheckEquals('https://www.food-place.com/images/mozzarella.png', FSchema.Image.URL);
  CheckEquals('09345UIHF98', FSchema.Image.CRC_32);
  CheckEquals('Contains preservatives', FSchema.NutritionalInfo.Description);
  CheckEquals('2500 Cal', FSchema.NutritionalInfo.Calories);
end;

procedure TTestItem.ObjectToJSON;
begin
  FSchema.AsJSON := FJSON;
  FJSONObject := TACBrJSONObject.Parse(FSchema.AsJSON);

  CheckEquals('7e507cab-7235-4f75-a0c2-e955fb2f2048', FJSONObject.AsString['id']);
  CheckEquals('1/2 Mozzarella', FJSONObject.AsString['name']);
  CheckEquals('Delicious mozzarella pizza.', FJSONObject.AsString['description']);
  CheckEquals('24', FJSONObject.AsString['externalCode']);
  CheckEquals('UNAVAILABLE', FJSONObject.AsString['status']);
  CheckEquals('unit', FJSONObject.AsString['unit']);
  CheckEquals('09345UIHF98', FJSONObject.AsJSONObject['image'].AsString['CRC-32']);
  CheckEquals('https://www.food-place.com/images/mozzarella.png', FJSONObject.AsJSONObject['image'].AsString['URL']);
  CheckEquals('Contains preservatives', FJSONObject.AsJSONObject['nutritionalInfo'].AsString['description']);
  CheckEquals('2500 Cal', FJSONObject.AsJSONObject['nutritionalInfo'].AsString['calories']);
end;

procedure TTestItem.SetUp;
begin
  inherited;
  FSchema := TACBrOpenDeliverySchemaItem.Create;
  FJSON := ACBrStr(
    '{' +
      '"id": "7e507cab-7235-4f75-a0c2-e955fb2f2048",' +
      '"name": "1/2 Mozzarella",' +
      '"description": "Delicious mozzarella pizza.",' +
      '"externalCode": "24",' +
      '"status": "UNAVAILABLE",' +
      '"image": {' +
        '"URL": "https://www.food-place.com/images/mozzarella.png",' +
        '"CRC-32": "09345UIHF98"' +
      '},' +
      '"nutritionalInfo": {' +
        '"description": "Contains preservatives",' +
        '"calories": "2500 Cal",' +
        '"allergen": [' +
          '"GLUTEN",' +
          '"LACTOSE"' +
        ']' +
      '},' +
      '"unit": "unit"' +
    '}');
end;

procedure TTestItem.TearDown;
begin
  FSchema.Free;
  FJSONObject.Free;
  inherited;
end;

{ TTestOption }

procedure TTestOption.JSONToObject;
begin
  FSchema.AsJSON := FJSON;
  CheckEquals('e5232f14-430c-4a94-8ff6-289d5a16a87b', FSchema.Id);
  CheckEquals('7e507cab-7235-4f75-a0c2-e955fb2f2048', FSchema.ItemId);
  CheckEquals(1, FSchema.Index);
  CheckEquals('UNAVAILABLE', StatusToStr(FSchema.status));
  CheckEquals('43', FloatToStr(FSchema.Price.value));
  CheckEquals('BRL', FSchema.Price.currency);
  CheckEquals(1, FSchema.MaxPermitted);
end;

procedure TTestOption.ObjectToJSON;
begin
  FSchema.AsJSON := FJSON;
  FJSONObject := TACBrJSONObject.Parse(FJSON);

  CheckEquals('e5232f14-430c-4a94-8ff6-289d5a16a87b', FJSONObject.AsString['id']);
  CheckEquals('7e507cab-7235-4f75-a0c2-e955fb2f2048', FJSONObject.AsString['itemId']);
  CheckEquals(1, FJSONObject.AsInteger['index']);
  CheckEquals('UNAVAILABLE', FJSONObject.AsString['status']);
  CheckEquals('43', FloatToStr(FJSONObject.AsJSONObject['price'].AsFloat['value']));
  CheckEquals('BRL', FJSONObject.AsJSONObject['price'].AsString['currency']);
  CheckEquals(1, FJSONObject.AsInteger['maxPermitted']);
end;

procedure TTestOption.SetUp;
begin
  inherited;
  FSchema := TACBrOpenDeliverySchemaOption.Create;
  FJSON := ACBrStr(
    '{' +
      '"id": "e5232f14-430c-4a94-8ff6-289d5a16a87b",' +
      '"itemId": "7e507cab-7235-4f75-a0c2-e955fb2f2048",' +
      '"index": 1,' +
      '"status": "UNAVAILABLE",' +
      '"price": {' +
        '"value": 43,' +
        '"originalValue": 43,' +
        '"currency": "BRL"' +
      '},' +
      '"maxPermitted": 1' +
    '}');
end;

procedure TTestOption.TearDown;
begin
  FSchema.Free;
  FJSONObject.Free;
  inherited;

end;

{ TTestOptionGroup }

procedure TTestOptionGroup.JSONToObject;
begin
  FSchema.AsJSON := FJSON;

  CheckEquals('fe67e551-f42f-499a-8afb-0ed893c71fa3', FSchema.Id);
  CheckEquals(0, FSchema.Index);
  CheckEquals('Choose your salted pizza flavor', FSchema.Name);
  CheckEquals('Choose your salted pizza flavor.', FSchema.Description);
  CheckEquals('12', FSchema.ExternalCode);
  CheckEquals('AVAILABLE', StatusToStr(FSchema.Status));
  CheckEquals(2, FSchema.MinPermitted);
  CheckEquals(2, FSchema.MaxPermitted);
  CheckEquals(2, FSchema.Options.Count);
  CheckEquals('e5232f14-430c-4a94-8ff6-289d5a16a87a', FSchema.Options[0].Id);
  CheckEquals('502ecf11-0509-48ed-b63c-6211c48fd9b9', FSchema.Options[0].ItemId);
  CheckEquals(0, FSchema.Options[0].Index);
  CheckEquals('50', FloatToStr(FSchema.Options[0].Price.value));
  CheckEquals('BRL', FSchema.Options[0].Price.currency);
  CheckEquals(1, FSchema.Options[0].MaxPermitted);

  CheckEquals('e5232f14-430c-4a94-8ff6-289d5a16a87b', FSchema.Options[1].Id);
  CheckEquals('7e507cab-7235-4f75-a0c2-e955fb2f2048', FSchema.Options[1].ItemId);
  CheckEquals(1, FSchema.Options[1].Index);
  CheckEquals('43', FloatToStr(FSchema.Options[1].Price.value));
  CheckEquals('BRL', FSchema.Options[1].Price.currency);
  CheckEquals(1, FSchema.Options[1].MaxPermitted);
end;

procedure TTestOptionGroup.ObjectToJSON;
var
  LJSONObject: TACBrJSONObject;
begin
  FSchema.AsJSON := FJSON;
  FJSONObject := TACBrJSONObject.Parse(FSchema.AsJSON);

  CheckEquals('fe67e551-f42f-499a-8afb-0ed893c71fa3', FJSONObject.AsString['id']);
  CheckEquals(0, FJSONObject.AsInteger['index']);
  CheckEquals('Choose your salted pizza flavor', FJSONObject.AsString['name']);
  CheckEquals('Choose your salted pizza flavor.', FJSONObject.AsString['description']);
  CheckEquals('12', FJSONObject.AsString['externalCode']);
  CheckEquals('AVAILABLE', FJSONObject.AsString['status']);
  CheckEquals(2, FJSONObject.AsInteger['minPermitted']);
  CheckEquals(2, FJSONObject.AsInteger['maxPermitted']);
  CheckEquals(2, FJSONObject.AsJSONArray['options'].Count);

  LJSONObject := FJSONObject.AsJSONArray['options'].ItemAsJSONObject[0];
  CheckEquals('e5232f14-430c-4a94-8ff6-289d5a16a87a', LJSONObject.AsString['id']);
  CheckEquals('502ecf11-0509-48ed-b63c-6211c48fd9b9', LJSONObject.AsString['itemId']);
  CheckEquals(0, LJSONObject.AsInteger['index']);
  CheckEquals('50', FloatToStr(LJSONObject.AsJSONObject['price'].AsFloat['value']));
  CheckEquals('BRL', LJSONObject.AsJSONObject['price'].AsString['currency']);
  CheckEquals(1, LJSONObject.AsInteger['maxPermitted']);

  LJSONObject := FJSONObject.AsJSONArray['options'].ItemAsJSONObject[1];
  CheckEquals('e5232f14-430c-4a94-8ff6-289d5a16a87b', LJSONObject.AsString['id']);
  CheckEquals('7e507cab-7235-4f75-a0c2-e955fb2f2048', LJSONObject.AsString['itemId']);
  CheckEquals(1, LJSONObject.AsInteger['index']);
  CheckEquals('43', FloatToStr(LJSONObject.AsJSONObject['price'].AsFloat['value']));
  CheckEquals('BRL', LJSONObject.AsJSONObject['price'].AsString['currency']);
  CheckEquals(1, LJSONObject.AsInteger['maxPermitted']);
end;

procedure TTestOptionGroup.SetUp;
begin
  inherited;
  FSchema := TACBrOpenDeliverySchemaOptionGroup.Create;
  FJSON := ACBrStr(
    '{' +
      '"id": "fe67e551-f42f-499a-8afb-0ed893c71fa3",' +
      '"index": 0,' +
      '"name": "Choose your salted pizza flavor",' +
      '"description": "Choose your salted pizza flavor.",' +
      '"externalCode": "12",' +
      '"status": "AVAILABLE",' +
      '"minPermitted": 2,' +
      '"maxPermitted": 2,' +
      '"options": [{' +
        '"id": "e5232f14-430c-4a94-8ff6-289d5a16a87a",' +
        '"itemId": "502ecf11-0509-48ed-b63c-6211c48fd9b9",' +
        '"index": 0,' +
        '"price": {' +
          '"value": 50,' +
          '"originalValue": 50,' +
          '"currency": "BRL"' +
        '},' +
        '"maxPermitted": 1' +
      '},' +
      '{' +
        '"id": "e5232f14-430c-4a94-8ff6-289d5a16a87b",' +
        '"itemId": "7e507cab-7235-4f75-a0c2-e955fb2f2048",' +
        '"index": 1,' +
        '"price": {' +
          '"value": 43,' +
          '"originalValue": 43,' +
          '"currency": "BRL"' +
        '},' +
        '"maxPermitted": 1' +
      '}]' +
    '}');
end;

procedure TTestOptionGroup.TearDown;
begin
  FSchema.Free;
  FJSONObject.Free;
  inherited;
end;

{ TTestAvailability }

procedure TTestAvailability.JSONToObject;
begin
  FSchema.AsJSON := FJSON;

  CheckEquals('11d063c4-73a7-4f87-a0eb-71636cc02029', FSchema.id);
  CheckEquals('2021-05-01', FormatDateTime('yyyy-MM-dd', FSchema.StartDate));
  CheckEquals('2021-05-30', FormatDateTime('yyyy-MM-dd', FSchema.EndDate));
  CheckEquals(1, FSchema.Hours.Count);
  CheckEquals(7, Length(FSchema.Hours[0].dayOfWeek));
  CheckEquals('MONDAY', DayOfWeekToStr(FSchema.Hours[0].dayOfWeek[0]));
  CheckEquals('SUNDAY', DayOfWeekToStr(FSchema.Hours[0].dayOfWeek[6]));
  CheckEquals('10:00:00', FormatDateTime('hh:mm:ss', FSchema.Hours[0].timePeriods.startTime));
  CheckEquals('18:00:00', FormatDateTime('hh:mm:ss', FSchema.Hours[0].timePeriods.endTime));
end;

procedure TTestAvailability.ObjectToJson;
begin
  FSchema.AsJSON := FJSON;
  FJSONObject := TACBrJSONObject.Parse(FSchema.AsJSON);

  CheckEquals('11d063c4-73a7-4f87-a0eb-71636cc02029', FJSONObject.AsString['id']);
  CheckEquals('2021-05-01', FormatDateTime('yyyy-MM-dd', FJSONObject.AsISODate['startDate']));
  CheckEquals('2021-05-30', FormatDateTime('yyyy-MM-dd', FJSONObject.AsISODate['endDate']));
  CheckEquals(1, FJSONObject.AsJSONArray['hours'].Count);
  CheckEquals(7, FJSONObject.AsJSONArray['hours'].ItemAsJSONObject[0].AsJSONArray['dayOfWeek'].Count);
  CheckEquals('MONDAY', FJSONObject.AsJSONArray['hours'].ItemAsJSONObject[0].AsJSONArray['dayOfWeek'].Items[0]);
  CheckEquals('SUNDAY', FJSONObject.AsJSONArray['hours'].ItemAsJSONObject[0].AsJSONArray['dayOfWeek'].Items[6]);
  CheckEquals('10:00:00', FormatDateTime('hh:mm:ss', FJSONObject.AsJSONArray['hours'].ItemAsJSONObject[0].AsJSONObject['timePeriods'].AsISOTime['startTime']));
  CheckEquals('18:00:00', FormatDateTime('hh:mm:ss', FJSONObject.AsJSONArray['hours'].ItemAsJSONObject[0].AsJSONObject['timePeriods'].AsISOTime['endTime']));

end;

procedure TTestAvailability.SetUp;
begin
  inherited;
  FSchema := TACBrOpenDeliverySchemaAvailability.Create;
  FJSON := ACBrStr(
    '{' +
    '  "id": "11d063c4-73a7-4f87-a0eb-71636cc02029",' +
    '  "startDate": "2021-05-01", ' +
    '  "endDate": "2021-05-30",' +
    '  "hours": [' +
    '    {' +
    '      "dayOfWeek": [' +
    '        "MONDAY",' +
    '        "TUESDAY",' +
    '        "WEDNESDAY",' +
    '        "THURSDAY",' +
    '        "FRIDAY",' +
    '        "SATURDAY",' +
    '        "SUNDAY"' +
    '      ],' +
    '      "timePeriods": {' +
    '        "startTime": "10:00:00.000Z",' +
    '        "endTime": "18:00:00.000Z"' +
    '      }' +
    '    }' +
    '  ]' +
    '}');
end;

procedure TTestAvailability.TearDown;
begin
  FSchema.Free;
  FJSONObject.Free;
  inherited;
end;

{ TTestEvent }

procedure TTestEvent.JSONToObject;
begin
  FSchema.AsJSON := FJSON;

  CheckEquals('1111', FSchema.EventId);
  CheckEquals(EventTypeToStr(etConfirmed), EventTypeToStr(FSchema.EventType));
  CheckEquals('2222', FSchema.OrderId);
  CheckEquals('http://example.com', FSchema.OrderURL);
  CheckEquals('0000', FSchema.SourceAppId);
  CheckEquals('2019-08-24 14:15:22', FormatDateTime('yyyy-MM-dd hh:mm:ss', FSchema.CreatedAt));
end;

procedure TTestEvent.ObjectToJSON;
begin
  FSchema.AsJSON := FJSON;
  FJSONObject := TACBrJSONObject.Parse(FSchema.AsJSON);

  CheckEquals('1111', FJSONObject.AsString['eventId']);
  CheckEquals(EventTypeToStr(etConfirmed), FJSONObject.AsString['eventType']);
  CheckEquals('2222', FJSONObject.AsString['orderId']);
  CheckEquals('http://example.com', FJSONObject.AsString['orderURL']);
  CheckEquals('0000', FJSONObject.AsString['sourceAppId']);
  CheckEquals('2019-08-24T14:15:22.000Z', FJSONObject.AsString['createdAt']);

end;

procedure TTestEvent.SetUp;
begin
  inherited;
  FSchema := TACBrOpenDeliverySchemaEvent.Create;
  FJSON := ACBrStr(
    '{' +
      '"eventId": "1111",' +
      '"eventType": "CONFIRMED",' +
      '"orderId": "2222",' +
      '"orderURL": "http://example.com",' +
      '"createdAt": "2019-08-24T14:15:22Z",' +
      '"sourceAppId": "0000"' +
    '}');
end;

procedure TTestEvent.TearDown;
begin
  FSchema.Free;
  FJSONObject.Free;
  inherited;

end;

{ TTestAccessToken }

procedure TTestAccessToken.JSONToObject;
begin
  FSchema.AsJSON := FJSON;
  CheckEquals('22222', FSchema.AccessToken);
  CheckEquals(900, FSchema.ExpiresIn);
end;

procedure TTestAccessToken.SetUp;
begin
  inherited;
  FSchema := TACBrOpenDeliverySchemaAccessToken.Create;
  FJSON := ACBrStr(
    '{' +
      '"access_token": "22222",' +
      '"token_type": "bearer",' +
      '"expires_in": 900' +
    '}');
end;

procedure TTestAccessToken.TearDown;
begin
  FSchema.Free;
  inherited;
end;

{ TTestAcknowledgment }

procedure TTestAcknowledgment.ObjectToJson;
begin
  FSchema.AsJSON := FJSON;
  FJSONObject := TACBrJSONObject.Parse(FSchema.AsJSON);

  CheckEquals('222', FJSONObject.AsString['id']);
  CheckEquals(EventTypeToStr(etDispatched), FJSONObject.AsString['eventType']);
  CheckEquals('555', FJSONObject.AsString['orderId']);
end;

procedure TTestAcknowledgment.SetUp;
begin
  inherited;
  FSchema := TACBrOpenDeliverySchemaAcknowledgment.Create;
  FJSON := ACBrStr(
    '{' +
      '"id": "222",' +
      '"orderId": "555",' +
      '"eventType": "DISPATCHED"' +
    '}');
end;

procedure TTestAcknowledgment.TearDown;
begin
  FSchema.Free;
  FJSONObject.Free;
  inherited;
end;

{ TTestOrderTakeout }

procedure TTestOrderTakeout.JSONToObject;
begin
  FSchema.AsJSON := FJSON;

  CheckEquals('PICKUP_AREA', TakeoutModeToStr(FSchema.mode));
  CheckEquals('2019-08-24 14:15:22', FormatDateTime('yyyy-MM-dd hh:mm:ss', FSchema.takeoutDateTime));
end;

procedure TTestOrderTakeout.ObjectToJSON;
begin
  FSchema.AsJSON := FJSON;
  FJSONObject := TACBrJSONObject.Parse(FSchema.AsJSON);

  CheckEquals('PICKUP_AREA', FJSONObject.AsString['mode']);
  CheckEquals('2019-08-24 14:15:22', FormatDateTime('yyyy-MM-dd hh:mm:ss', FJSONObject.AsISODateTime['takeoutDateTime']));
end;

procedure TTestOrderTakeout.SetUp;
begin
  inherited;
  FSchema := TACBrOpenDeliverySchemaOrderTakeout.Create;
  FJSON := ACBrStr(
    '{' +
      '"mode": "PICKUP_AREA",' +
      '"takeoutDateTime": "2019-08-24T14:15:22Z"' +
    '}');
end;

procedure TTestOrderTakeout.TearDown;
begin
  FSchema.Free;
  FJSONObject.Free;
  inherited;
end;

{ TTestOrderAddress }

procedure TTestOrderAddress.JSONToObject;
begin
  FSchema.AsJSON := FJSON;
  CheckEquals('BR', FSchema.country);
  CheckEquals('BR-SP', FSchema.state);
  CheckEquals('São Paulo', FSchema.city);
  CheckEquals('Moema', FSchema.district);
  CheckEquals('Plaza Avenue', FSchema.street);
  CheckEquals('100', FSchema.number);
  CheckEquals('20111-000', FSchema.postalCode);
  CheckEquals('BL 02 AP 31', FSchema.complement);
  CheckEquals('Yellow House', FSchema.reference);
  CheckEquals('-23,54823', FloatToStr(FSchema.coordinates.Latitude));
  CheckEquals('-46,63632', FloatToStr(FSchema.coordinates.Longitude));
end;

procedure TTestOrderAddress.ObjectToJson;
begin
  FSchema.AsJSON := FJSON;
  FJSONObject := TACBrJSONObject.Parse(FSchema.AsJSON);

  CheckEquals('BR', FJSONObject.AsString['country']);
  CheckEquals('BR-SP', FJSONObject.AsString['state']);
  CheckEquals('São Paulo', FJSONObject.AsString['city']);
  CheckEquals('Moema', FJSONObject.AsString['district']);
  CheckEquals('Plaza Avenue', FJSONObject.AsString['street']);
  CheckEquals('100', FJSONObject.AsString['number']);
  CheckEquals('20111-000', FJSONObject.AsString['postalCode']);
  CheckEquals('BL 02 AP 31', FJSONObject.AsString['complement']);
  CheckEquals('Yellow House', FJSONObject.AsString['reference']);
  CheckEquals('-23,54823', FloatToStr(FJSONObject.AsJSONObject['coordinates'].AsFloat['latitude']));
  CheckEquals('-46,63632', FloatToStr(FJSONObject.AsJSONObject['coordinates'].AsFloat['longitude']));
end;

procedure TTestOrderAddress.SetUp;
begin
  inherited;
  FSchema := TACBrOpenDeliverySchemaOrderAddress.Create;
  FJSON := ACBrStr(
    '{' +
      '"country": "BR",' +
      '"state": "BR-SP",' +
      '"city": "São Paulo",' +
      '"district": "Moema",' +
      '"street": "Plaza Avenue",' +
      '"number": "100",' +
      '"postalCode": "20111-000",' +
      '"complement": "BL 02 AP 31",' +
      '"reference": "Yellow House",' +
      '"coordinates": {' +
        '"latitude": -23.54823,' +
        '"longitude": -46.63632' +
      '}' +
    '}');
end;

procedure TTestOrderAddress.TearDown;
begin
  FSchema.Free;
  FJSONObject.Free;
  inherited;
end;

{ TTestOrderDelivery }

procedure TTestOrderDelivery.JSONToObject;
begin
  FSchema.AsJSON := FJSON;
  CheckEquals('MARKETPLACE', SponsorToStr(FSchema.deliveredBy));
  CheckEquals('2019-08-24 14:15:22', FormatDateTime('yyyy-MM-dd hh:mm:ss', FSchema.estimatedDeliveryDateTime));
  CheckEquals('2019-08-23 14:15:22', FormatDateTime('yyyy-MM-dd hh:mm:ss', FSchema.deliveryDateTime));
  CheckEquals('BR', FSchema.deliveryAddress.country);
  CheckEquals('BR-SP', FSchema.deliveryAddress.state);
  CheckEquals('São Paulo', FSchema.deliveryAddress.city);
  CheckEquals('Moema', FSchema.deliveryAddress.district);
  CheckEquals('Plaza Avenue', FSchema.deliveryAddress.street);
  CheckEquals('100', FSchema.deliveryAddress.number);
  CheckEquals('20111-000', FSchema.deliveryAddress.postalCode);
  CheckEquals('BL 02 AP 31', FSchema.deliveryAddress.complement);
  CheckEquals('Yellow House', FSchema.deliveryAddress.reference);
  CheckEquals('-23,54823', FloatToStr(FSchema.deliveryAddress.coordinates.Latitude));
  CheckEquals('-46,63632', FloatToStr(FSchema.deliveryAddress.coordinates.Longitude));
end;

procedure TTestOrderDelivery.ObjectToJson;
begin
  FSchema.AsJSON := FJSON;
  FJSONObject := TACBrJSONObject.Parse(FSchema.AsJSON);

  CheckEquals('MARKETPLACE', FJSONObject.AsString['deliveredBy']);
  CheckEquals('BR', FJSONObject.AsJSONObject['deliveryAddress'].AsString['country']);
  CheckEquals('BR-SP', FJSONObject.AsJSONObject['deliveryAddress'].AsString['state']);
  CheckEquals('São Paulo', FJSONObject.AsJSONObject['deliveryAddress'].AsString['city']);
  CheckEquals('Moema', FJSONObject.AsJSONObject['deliveryAddress'].AsString['district']);
  CheckEquals('Plaza Avenue', FJSONObject.AsJSONObject['deliveryAddress'].AsString['street']);
  CheckEquals('100', FJSONObject.AsJSONObject['deliveryAddress'].AsString['number']);
  CheckEquals('20111-000', FJSONObject.AsJSONObject['deliveryAddress'].AsString['postalCode']);
  CheckEquals('BL 02 AP 31', FJSONObject.AsJSONObject['deliveryAddress'].AsString['complement']);
  CheckEquals('Yellow House', FJSONObject.AsJSONObject['deliveryAddress'].AsString['reference']);
  CheckEquals('-23,54823', FloatToStr(FJSONObject.AsJSONObject['deliveryAddress'].AsJSONObject['coordinates'].AsFloat['latitude']));
  CheckEquals('-46,63632', FloatToStr(FJSONObject.AsJSONObject['deliveryAddress'].AsJSONObject['coordinates'].AsFloat['longitude']));
  CheckEquals('2019-08-24 14:15:22', FormatDateTime('yyyy-MM-dd hh:mm:ss', FJSONObject.AsISODateTime['estimatedDeliveryDateTime']));
  CheckEquals('2019-08-23 14:15:22', FormatDateTime('yyyy-MM-dd hh:mm:ss', FJSONObject.AsISODateTime['deliveryDateTime']));
end;

procedure TTestOrderDelivery.SetUp;
begin
  inherited;
  FSchema := TACBrOpenDeliverySchemaOrderDelivery.Create;
  FJSON := ACBrStr(
    '{' +
      '"deliveredBy": "MARKETPLACE",' +
      '"deliveryAddress": {' +
        '"country": "BR",' +
        '"state": "BR-SP",' +
        '"city": "São Paulo",' +
        '"district": "Moema",' +
        '"street": "Plaza Avenue",' +
        '"number": "100",' +
        '"complement": "BL 02 AP 31",' +
        '"reference": "Yellow House",' +
        '"formattedAddress": "Plaza Avenue, 100, BL 02 AP 31, Moema - São Paulo, SP - Brazil",' +
        '"postalCode": "20111-000",' +
        '"coordinates": {' +
          '"latitude": -23.54823,' +
          '"longitude": -46.63632' +
          '}' +
        '},' +
      '"estimatedDeliveryDateTime": "2019-08-24T14:15:22Z",' +
      '"deliveryDateTime": "2019-08-23T14:15:22Z"' +
    '}');
end;

procedure TTestOrderDelivery.TearDown;
begin
  FSchema.Free;
  FJSONObject.Free;
  inherited;
end;

{ TTestOrderCustomer }

procedure TTestOrderCustomer.JSONToObject;
begin
  FSchema.AsJSON := FJSON;
  CheckEquals('1234', FSchema.id);
  CheckEquals('22', FSchema.phone.extension);
  CheckEquals('9999', FSchema.phone.number);
  CheckEquals('987', FSchema.documentNumber);
  CheckEquals('Customer Test', FSchema.name);
  CheckEquals(8, FSchema.ordersCountOnMerchant);
end;

procedure TTestOrderCustomer.ObjectToJson;
begin
  FSchema.AsJSON := FJSON;
  FJSONObject := TACBrJSONObject.Parse(FSchema.AsJSON);

  CheckEquals('1234', FJSONObject.AsString['id']);
  CheckEquals('22',  FJSONObject.AsJSONObject['phone'].AsString['extension']);
  CheckEquals('9999', FJSONObject.AsJSONObject['phone'].AsString['number']);
  CheckEquals('987', FJSONObject.AsString['documentNumber']);
  CheckEquals('Customer Test', FJSONObject.AsString['name']);
  CheckEquals(8, FJSONObject.AsInteger['ordersCountOnMerchant']);
end;

procedure TTestOrderCustomer.SetUp;
begin
  inherited;
  FSchema := TACBrOpenDeliverySchemaOrderCustomer.Create;
  FJSON := ACBrStr(
    '{' +
      '"id": "1234",' +
      '"phone": {' +
        '"number": "9999",' +
        '"extension": "22"' +
      '},' +
      '"documentNumber": "987",' +
      '"name": "Customer Test",' +
      '"ordersCountOnMerchant": 8' +
    '}');
end;

procedure TTestOrderCustomer.TearDown;
begin
  FSchema.Free;
  FJSONObject.Free;
  inherited;
end;

{ TTestOrder }

procedure TTestOrder.JSONToObject;
begin
  FSchema.AsJSON := FJSON;

  CheckEquals('1111', FSchema.id);
  CheckEquals('DELIVERY', ServiceTypeToStr(FSchema._type));
  CheckEquals('01111', FSchema.displayId);
  CheckEquals('001111', FSchema.sourceAppId);
  CheckEquals('2019-08-24 14:15:22', FormatDateTime('yyyy-MM-dd hh:mm:ss', FSchema.createdAt));
  CheckEquals('2019-08-24 14:14:22', FormatDateTime('yyyy-MM-dd hh:mm:ss', FSchema.preparationStartDateTime));
  CheckEquals('SCHEDULED', OrderTimingToStr(FSchema.orderTiming));
  CheckEquals('2222222', FSchema.merchant.id);
  CheckEquals('Pizza Plaza', FSchema.merchant.name);
  CheckEquals('no onions', FSchema.extraInfo);

  // Items
  CheckEquals(1, FSchema.items.Count);
  CheckEquals(1, FSchema.items[0].options.Count);
  CheckEquals('22', FSchema.items[0].id);
  CheckEquals(0, FSchema.items[0].index);
  CheckEquals('item 1', FSchema.items[0].name);
  CheckEquals('111', FSchema.items[0].externalCode);
  CheckEquals('UNIT', FSchema.items[0]._unit);
  CheckEquals('99999', FSchema.items[0].ean);
  CheckEquals('2', FloatToStr(FSchema.items[0].quantity));
  CheckEquals('no onion', FSchema.items[0].specialInstructions);
  CheckEquals('40', FloatToStr(FSchema.items[0].unitPrice.value));
  CheckEquals('BRL', FSchema.items[0].unitPrice.currency);
  CheckEquals('40', FloatToStr(FSchema.items[0].optionsPrice.value));
  CheckEquals('BRL', FSchema.items[0].optionsPrice.currency);
  CheckEquals('120', FloatToStr(FSchema.items[0].totalPrice.value));
  CheckEquals('BRL', FSchema.items[0].totalPrice.currency);

  // Options
  CheckEquals('111', FSchema.items[0].options[0].id);
  CheckEquals(0, FSchema.items[0].options[0].index);
  CheckEquals('option 1', FSchema.items[0].options[0].name);
  CheckEquals('1111', FSchema.items[0].options[0].externalCode);
  CheckEquals('UNIT', FSchema.items[0].options[0]._unit);
  CheckEquals('22222', FSchema.items[0].options[0].ean);
  CheckEquals('1', FloatToStr(FSchema.items[0].options[0].quantity));
  CheckEquals('no onion', FSchema.items[0].options[0].specialInstructions);
  CheckEquals('40', FloatToStr(FSchema.items[0].options[0].unitPrice.value));
  CheckEquals('BRL', FSchema.items[0].options[0].unitPrice.currency);
  CheckEquals('40', FloatToStr(FSchema.items[0].options[0].totalPrice.value));
  CheckEquals('BRL', FSchema.items[0].options[0].totalPrice.currency);

  // OtherFees
  CheckEquals(1, FSchema.otherFees.Count);
  CheckEquals('ENTREGA', FSchema.otherFees[0].name);
  CheckEquals('DELIVERY_FEE', FeeTypeToStr(FSchema.otherFees[0]._type));
  CheckEquals('MARKETPLACE', FeeReceivedByToStr(FSchema.otherFees[0].receivedBy));
  CheckEquals('5555', FSchema.otherFees[0].receiverDocument);
  CheckEquals('string', FSchema.otherFees[0].observation);
  CheckEquals('BRL', FSchema.otherFees[0].price.currency);
  CheckEquals('10', FloatToStr(FSchema.otherFees[0].price.value));

  // Discounts
  CheckEquals(1, FSchema.discounts.Count);
  CheckEquals('CART', DiscountTargetToStr(FSchema.discounts[0].target));
  CheckEquals('string', FSchema.discounts[0].targetId);
  CheckEquals('BRL', FSchema.discounts[0].amount.currency);
  CheckEquals('5', FloatToStr(FSchema.discounts[0].amount.value));
  CheckEquals(1, FSchema.discounts[0].sponsorshipValues.Count);
  CheckEquals('MARKETPLACE', FSchema.discounts[0].sponsorshipValues[0].name);
  CheckEquals('BRL', FSchema.discounts[0].sponsorshipValues[0].amount.currency);
  CheckEquals('5', FloatToStr(FSchema.discounts[0].sponsorshipValues[0].amount.value));

  // Total
  CheckEquals('BRL', FSchema.total.itemsPrice.currency);
  CheckEquals('120', FloatToStr(FSchema.total.itemsPrice.value));
  CheckEquals('BRL', FSchema.total.otherFees.currency);
  CheckEquals('10', FloatToStr(FSchema.total.otherFees.value));
  CheckEquals('BRL', FSchema.total.discount.currency);
  CheckEquals('5', FloatToStr(FSchema.total.discount.value));
  CheckEquals('BRL', FSchema.total.orderAmount.currency);
  CheckEquals('125', FloatToStr(FSchema.total.orderAmount.value));

  // Payments
  CheckEquals('125', FloatToStr(FSchema.payments.prepaid));
  CheckEquals('0', FloatToStr(FSchema.payments.pending));
  CheckEquals(1, FSchema.payments.methods.Count);
  CheckEquals('125', FloatToStr(FSchema.payments.methods[0].value));
  CheckEquals('BRL', FSchema.payments.methods[0].currency);
  CheckEquals('PREPAID', PaymentTypeToStr(FSchema.payments.methods[0]._type));
  CheckEquals('DEBIT', PaymentMethodToStr(FSchema.payments.methods[0].method));
  CheckEquals('Cash', FSchema.payments.methods[0].methodInfo);
  CheckEquals('150', FloatToStr(FSchema.payments.methods[0].changeFor));
  CheckEquals('25', FloatToStr(FSchema.payments.methods[0].ChangeValue));

  // Customer
  CheckEquals('4444', FSchema.customer.id);
  CheckEquals('998855', FSchema.customer.phone.number);
  CheckEquals('21', FSchema.customer.phone.extension);
  CheckEquals('888888', FSchema.customer.documentNumber);
  CheckEquals('Customer 123', FSchema.customer.name);
  CheckEquals(3, FSchema.customer.ordersCountOnMerchant);

  // Schedule
  CheckEquals('2019-08-24 14:15:22', FormatDateTime('yyyy-MM-dd hh:mm:ss', FSchema.schedule.scheduledDateTimeStart));
  CheckEquals('2019-08-25 14:15:22', FormatDateTime('yyyy-MM-dd hh:mm:ss', FSchema.schedule.scheduledDateTimeEnd));

  // Delivery
  CheckEquals('MERCHANT', SponsorToStr(FSchema.delivery.deliveredBy));
  CheckEquals('BR', FSchema.delivery.deliveryAddress.country);
  CheckEquals('BR-SP', FSchema.delivery.deliveryAddress.state);
  CheckEquals('São Paulo', FSchema.delivery.deliveryAddress.city);
  CheckEquals('Moema', FSchema.delivery.deliveryAddress.district);
  CheckEquals('Plaza Avenue', FSchema.delivery.deliveryAddress.street);
  CheckEquals('100', FSchema.delivery.deliveryAddress.number);
  CheckEquals('BL 02 AP 31', FSchema.delivery.deliveryAddress.complement);
  CheckEquals('Yellow House', FSchema.delivery.deliveryAddress.reference);
  CheckEquals('Plaza Avenue, 100, BL 02 AP 31, Moema - São Paulo, SP - Brazil', FSchema.delivery.deliveryAddress.formattedAddress);
  CheckEquals('20111-000', FSchema.delivery.deliveryAddress.postalCode);
  CheckEquals('-23,54823', FloatToStr(FSchema.delivery.deliveryAddress.coordinates.Latitude));
  CheckEquals('-46,63632', FloatToStr(FSchema.delivery.deliveryAddress.coordinates.Longitude));
  CheckEquals('2019-08-24 14:15:22', FormatDateTime('yyyy-MM-dd hh:mm:ss', FSchema.delivery.estimatedDeliveryDateTime));
  CheckEquals('2019-08-24 14:15:22', FormatDateTime('yyyy-MM-dd hh:mm:ss', FSchema.delivery.deliveryDateTime));

  // Takeout
  CheckEquals('DEFAULT', TakeoutModeToStr(FSchema.takeout.mode));
  CheckEquals('2019-08-24 14:15:22', FormatDateTime('yyyy-MM-dd hh:mm:ss', FSchema.takeout.takeoutDateTime));

  CheckEquals('PLACE', IndoorModeToStr(FSchema.indoor.mode));
  CheckEquals('2019-08-28 14:15:22', FormatDateTime('yyyy-MM-dd hh:mm:ss', FSchema.indoor.indoorDateTime));
  CheckEquals('test place', FSchema.indoor.place);
end;

procedure TTestOrder.ObjectToJson;
var
  LJSONObject: TACBrJSONObject;
begin
  FSchema.AsJSON := FJSON;
  FJSONObject := TACBrJSONObject.Parse(FSchema.AsJSON);

  CheckEquals('1111', FJSONObject.AsString['id']);
  CheckEquals('DELIVERY', FJSONObject.AsString['type']);
  CheckEquals('01111', FJSONObject.AsString['displayId']);
  CheckEquals('001111', FJSONObject.AsString['sourceAppId']);
  CheckEquals('2019-08-24 14:15:22', FormatDateTime('yyyy-MM-dd hh:mm:ss', FJSONObject.AsISODateTime['createdAt']));
  CheckEquals('2019-08-24 14:14:22', FormatDateTime('yyyy-MM-dd hh:mm:ss', FJSONObject.AsISODateTime['preparationStartDateTime']));
  CheckEquals('SCHEDULED', FJSONObject.AsString['orderTiming']);
  CheckEquals('2222222', FJSONObject.AsJSONObject['merchant'].AsString['id']);
  CheckEquals('Pizza Plaza', FJSONObject.AsJSONObject['merchant'].AsString['name']);
  CheckEquals('no onions', FJSONObject.AsString['extraInfo']);

  // Items
  CheckEquals(1, FJSONObject.AsJSONArray['items'].Count);
  LJSONObject := FJSONObject.AsJSONArray['items'].ItemAsJSONObject[0];
  CheckEquals(1, LJSONObject.AsJSONArray['options'].Count);
  CheckEquals('22', LJSONObject.AsString['id']);
  CheckEquals(0, LJSONObject.AsInteger['index']);
  CheckEquals('item 1', LJSONObject.AsString['name']);
  CheckEquals('111', LJSONObject.AsString['externalCode']);
  CheckEquals('UNIT', LJSONObject.AsString['unit']);
  CheckEquals('99999', LJSONObject.AsString['ean']);
  CheckEquals('2', FloatToStr(LJSONObject.AsFloat['quantity']));
  CheckEquals('no onion', LJSONObject.AsString['specialInstructions']);
  CheckEquals('40', FloatToStr(LJSONObject.AsJSONObject['unitPrice'].AsFloat['value']));
  CheckEquals('BRL', LJSONObject.AsJSONObject['unitPrice'].AsString['currency']);
  CheckEquals('40', FloatToStr(LJSONObject.AsJSONObject['optionsPrice'].AsFloat['value']));
  CheckEquals('BRL', LJSONObject.AsJSONObject['optionsPrice'].AsString['currency']);
  CheckEquals('120', FloatToStr(LJSONObject.AsJSONObject['totalPrice'].AsFloat['value']));
  CheckEquals('BRL', LJSONObject.AsJSONObject['totalPrice'].AsString['currency']);

  // Options
  LJSONObject := LJSONObject.AsJSONArray['options'].ItemAsJSONObject[0];
  CheckEquals('111', LJSONObject.AsString['id']);
  CheckEquals(0, LJSONObject.AsInteger['index']);
  CheckEquals('option 1', LJSONObject.AsString['name']);
  CheckEquals('1111', LJSONObject.AsString['externalCode']);
  CheckEquals('UNIT', LJSONObject.AsString['unit']);
  CheckEquals('22222', LJSONObject.AsString['ean']);
  CheckEquals('1', FloatToStr(LJSONObject.AsFloat['quantity']));
  CheckEquals('no onion', LJSONObject.AsString['specialInstructions']);
  CheckEquals('40', FloatToStr(LJSONObject.AsJSONObject['unitPrice'].AsFloat['value']));
  CheckEquals('BRL', LJSONObject.AsJSONObject['unitPrice'].AsString['currency']);
  CheckEquals('40', FloatToStr(LJSONObject.AsJSONObject['totalPrice'].AsFloat['value']));
  CheckEquals('BRL', LJSONObject.AsJSONObject['totalPrice'].AsString['currency']);

  // OtherFees
  LJSONObject := FJSONObject.AsJSONArray['otherFees'].ItemAsJSONObject[0];
  CheckEquals(1, FJSONObject.AsJSONArray['otherFees'].Count);
  CheckEquals('ENTREGA', LJSONObject.AsString['name']);
  CheckEquals('DELIVERY_FEE', LJSONObject.AsString['type']);
  CheckEquals('MARKETPLACE', LJSONObject.AsString['receivedBy']);
  CheckEquals('5555', LJSONObject.AsString['receiverDocument']);
  CheckEquals('string', LJSONObject.AsString['observation']);
  CheckEquals('BRL', LJSONObject.AsJSONObject['price'].AsString['currency']);
  CheckEquals('10', FloatToStr(LJSONObject.AsJSONObject['price'].AsFloat['value']));

  // Discounts
  LJSONObject := FJSONObject.AsJSONArray['discounts'].ItemAsJSONObject[0];
  CheckEquals(1, FJSONObject.AsJSONArray['discounts'].Count);
  CheckEquals('CART', LJSONObject.AsString['target']);
  CheckEquals('string', LJSONObject.AsString['targetId']);
  CheckEquals('BRL', LJSONObject.AsJSONObject['amount'].AsString['currency']);
  CheckEquals('5', FloatToStr(LJSONObject.AsJSONObject['amount'].AsFloat['value']));

  CheckEquals(1, LJSONObject.AsJSONArray['sponsorshipValues'].Count);
  LJSONObject := LJSONObject.AsJSONArray['sponsorshipValues'].ItemAsJSONObject[0];
  CheckEquals('MARKETPLACE', LJSONObject.AsString['name']);
  CheckEquals('BRL', LJSONObject.AsJSONObject['amount'].AsString['currency']);
  CheckEquals('5', FloatToStr(LJSONObject.AsJSONObject['amount'].AsFloat['value']));

  // Total
  LJSONObject := FJSONObject.AsJSONObject['total'];
  CheckEquals('BRL', LJSONObject.AsJSONObject['itemsPrice'].AsString['currency']);
  CheckEquals('120', FloatToStr(LJSONObject.AsJSONObject['itemsPrice'].AsFloat['value']));
  CheckEquals('BRL', LJSONObject.AsJSONObject['otherFees'].AsString['currency']);
  CheckEquals('10', FloatToStr(LJSONObject.AsJSONObject['otherFees'].AsFloat['value']));
  CheckEquals('BRL', LJSONObject.AsJSONObject['discount'].AsString['currency']);
  CheckEquals('5', FloatToStr(LJSONObject.AsJSONObject['discount'].AsFloat['value']));
  CheckEquals('BRL', LJSONObject.AsJSONObject['orderAmount'].AsString['currency']);
  CheckEquals('125', FloatToStr(LJSONObject.AsJSONObject['orderAmount'].AsFloat['value']));

  // Payments
  LJSONObject := FJSONObject.AsJSONObject['payments'];
  CheckEquals('125', FloatToStr(LJSONObject.AsFloat['prepaid']));
  CheckEquals('0', FloatToStr(LJSONObject.AsFloat['pending']));
  CheckEquals(1, LJSONObject.AsJSONArray['methods'].Count);
  LJSONObject := LJSONObject.AsJSONArray['methods'].ItemAsJSONObject[0];
  CheckEquals('125', FloatToStr(LJSONObject.AsFloat['value']));
  CheckEquals('BRL', LJSONObject.AsString['currency']);
  CheckEquals('PREPAID', LJSONObject.AsString['type']);
  CheckEquals('DEBIT', LJSONObject.AsString['method']);
  CheckEquals('Cash', LJSONObject.AsString['methodInfo']);
  CheckEquals('150', FloatToStr(LJSONObject.AsFloat['changeFor']));

  // Customer
  LJSONObject := FJSONObject.AsJSONObject['customer'];
  CheckEquals('4444', LJSONObject.AsString['id']);
  CheckEquals('888888', LJSONObject.AsString['documentNumber']);
  CheckEquals('Customer 123', LJSONObject.AsString['name']);
  CheckEquals(3, LJSONObject.AsInteger['ordersCountOnMerchant']);
  CheckEquals('998855', LJSONObject.AsJSONObject['phone'].AsString['number']);
  CheckEquals('21', LJSONObject.AsJSONObject['phone'].AsString['extension']);

  // Delivery
  LJSONObject := FJSONObject.AsJSONObject['delivery'];
  CheckEquals('MERCHANT', LJSONObject.AsString['deliveredBy']);
  CheckEquals('2019-08-24 14:15:22', FormatDateTime('yyyy-MM-dd hh:mm:ss', LJSONObject.AsISODateTime['estimatedDeliveryDateTime']));
  CheckEquals('2019-08-24 14:15:22', FormatDateTime('yyyy-MM-dd hh:mm:ss', LJSONObject.AsISODateTime['deliveryDateTime']));
  LJSONObject := LJSONObject.AsJSONObject['deliveryAddress'];
  CheckEquals('BR', LJSONObject.AsString['country']);
  CheckEquals('BR-SP', LJSONObject.AsString['state']);
  CheckEquals('São Paulo', LJSONObject.AsString['city']);
  CheckEquals('Moema', LJSONObject.AsString['district']);
  CheckEquals('Plaza Avenue', LJSONObject.AsString['street']);
  CheckEquals('100', LJSONObject.AsString['number']);
  CheckEquals('BL 02 AP 31', LJSONObject.AsString['complement']);
  CheckEquals('Yellow House', LJSONObject.AsString['reference']);
  CheckEquals('Plaza Avenue, 100, BL 02 AP 31, Moema - São Paulo, SP - Brazil', LJSONObject.AsString['formattedAddress']);
  CheckEquals('20111-000', LJSONObject.AsString['postalCode']);
  CheckEquals('-23,54823', FloatToStr(LJSONObject.AsJSONObject['coordinates'].AsFloat['latitude']));
  CheckEquals('-46,63632', FloatToStr(LJSONObject.AsJSONObject['coordinates'].AsFloat['longitude']));

  // Takeout
  LJSONObject := FJSONObject.AsJSONObject['takeout'];
  CheckEquals('DEFAULT', LJSONObject.AsString['mode']);
  CheckEquals('2019-08-24 14:15:22', FormatDateTime('yyyy-MM-dd hh:mm:ss', LJSONObject.AsISODateTime['takeoutDateTime']));
end;

procedure TTestOrder.SetUp;
begin
  inherited;
  FSchema := TACBrOpenDeliverySchemaOrder.Create;
  FJSON := ACBrStr(
    '{' +
      '"id": "1111",' +
      '"type": "DELIVERY",' +
      '"displayId": "01111",' +
      '"sourceAppId": "001111",' +
      '"createdAt": "2019-08-24T14:15:22Z",' +
      '"orderTiming": "SCHEDULED",' +
      '"preparationStartDateTime": "2019-08-24T14:14:22Z",' +
      '"merchant": {' +
        '"id": "2222222",' +
        '"name": "Pizza Plaza"' +
      '},' +
      '"items": [' +
        '{' +
          '"id": "22",' +
          '"index": 0,' +
          '"name": "item 1",' +
          '"externalCode": "111",' +
          '"unit": "UNIT",' +
          '"ean": "99999",' +
          '"quantity": 2,' +
          '"specialInstructions": "no onion",' +
          '"unitPrice": {' +
            '"value": 40,' +
            '"currency": "BRL"' +
          '},' +
          '"optionsPrice": {' +
            '"value": 40,' +
            '"currency": "BRL"' +
          '},' +
          '"totalPrice": {' +
            '"value": 120,' +
            '"currency": "BRL"' +
          '},' +
          '"options": [' +
            '{' +
              '"index": 0,' +
              '"id": "111",' +
              '"name": "option 1",' +
              '"externalCode": "1111",' +
              '"unit": "UNIT",' +
              '"ean": "22222",' +
              '"quantity": 1,' +
              '"unitPrice": {' +
                '"value": 40,' +
                '"currency": "BRL"' +
              '},' +
              '"totalPrice": {' +
                '"value": 40,' +
                '"currency": "BRL"' +
              '},' +
              '"specialInstructions": "no onion"' +
            '}' +
          ']' +
        '}' +
      '],' +
      '"otherFees": [' +
        '{' +
          '"name": "ENTREGA",' +
          '"type": "DELIVERY_FEE",' +
          '"receivedBy": "MARKETPLACE",' +
          '"receiverDocument": "5555",' +
          '"price": {' +
            '"value": 10,' +
            '"currency": "BRL"' +
          '},' +
          '"observation": "string"' +
        '}' +
      '],' +
      '"discounts": [' +
        '{' +
          '"amount": {' +
            '"value": 5,' +
            '"currency": "BRL"' +
          '},' +
          '"target": "CART",' +
          '"targetId": "string",' +
          '"sponsorshipValues": [' +
            '{' +
              '"name": "MARKETPLACE",' +
              '"amount": {' +
                '"value": 5,' +
                '"currency": "BRL"' +
              '}' +
            '}' +
          ']' +
        '}' +
      '],' +
      '"total": {' +
        '"itemsPrice": {  ' +
          '"value": 120,' +
          '"currency": "BRL"' +
        '},' +
        '"otherFees": {' +
          '"value": 10,' +
          '"currency": "BRL"' +
        '},' +
        '"discount": {' +
          '"value": 5,' +
          '"currency": "BRL"' +
        '},' +
        '"orderAmount": {' +
          '"value": 125,' +
          '"currency": "BRL"' +
        '}' +
      '},' +
      '"payments": {' +
        '"prepaid": 125,' +
        '"pending": 0,' +
        '"methods": [' +
          '{' +
            '"value": 125,' +
            '"currency": "BRL",' +
            '"type": "PREPAID",' +
            '"method": "DEBIT",' +
            '"methodInfo": "Cash",' +
            '"changeFor": 150' +
          '}' +
        ']' +
      '},' +
      '"customer": {' +
        '"id": "4444",' +
        '"phone": {' +
          '"number": "998855",' +
          '"extension": "21"' +
        '},' +
        '"documentNumber": "888888",' +
        '"name": "Customer 123",' +
        '"ordersCountOnMerchant": 3' +
      '},' +
      '"schedule": {' +
        '"scheduledDateTimeStart": "2019-08-24T14:15:22Z",' +
        '"scheduledDateTimeEnd": "2019-08-25T14:15:22Z"' +
      '},' +
      '"delivery": {' +
        '"deliveredBy": "MERCHANT",' +
        '"deliveryAddress": {' +
          '"country": "BR",' +
          '"state": "BR-SP",' +
          '"city": "São Paulo",' +
          '"district": "Moema",' +
          '"street": "Plaza Avenue",' +
          '"number": "100",' +
          '"complement": "BL 02 AP 31",' +
          '"reference": "Yellow House",' +
          '"formattedAddress": "Plaza Avenue, 100, BL 02 AP 31, Moema - São Paulo, SP - Brazil",' +
          '"postalCode": "20111-000",' +
          '"coordinates": {' +
            '"latitude": -23.54823,' +
            '"longitude": -46.63632' +
          '}' +
        '},' +
        '"estimatedDeliveryDateTime": "2019-08-24T14:15:22Z",' +
        '"deliveryDateTime": "2019-08-24T14:15:22Z"' +
      '},' +
      '"takeout": {' +
        '"mode": "DEFAULT",' +
        '"takeoutDateTime": "2019-08-24T14:15:22Z"' +
      '},' +
      '"indoor": {' +
        '"mode": "PLACE",' +
        '"indoorDateTime": "2019-08-28T14:15:22Z",' +
        '"place": "test place"' +
      '},' +
      '"extraInfo": "no onions"' +
    '}');
end;

procedure TTestOrder.TearDown;
begin
  FSchema.Free;
  FJSONObject.Free;
  inherited;
end;

initialization
  _RegisterTest('ACBrOpenDelivery.Schema.AccessToken', TTestAccessToken);
  _RegisterTest('ACBrOpenDelivery.Schema.Acknowledgment', TTestAcknowledgment);
  _RegisterTest('ACBrOpenDelivery.Schema.Address', TTestAddress);
  _RegisterTest('ACBrOpenDelivery.Schema.Availability', TTestAvailability);
  _RegisterTest('ACBrOpenDelivery.Schema.BasicInfo', TTestBasicInfo);
  _RegisterTest('ACBrOpenDelivery.Schema.Category', TTestCategory);
  _RegisterTest('ACBrOpenDelivery.Schema.ContactPhone', TTestContactPhone);
  _RegisterTest('ACBrOpenDelivery.Schema.Event', TTestEvent);
  _RegisterTest('ACBrOpenDelivery.Schema.GeoCordinate', TTestGeoCoordinate);
  _RegisterTest('ACBrOpenDelivery.Schema.GeoRadius', TTestGeoRadius);
  _RegisterTest('ACBrOpenDelivery.Schema.HolidayHour', TTestHolidayHour);
  _RegisterTest('ACBrOpenDelivery.Schema.Hour', TTestHour);
  _RegisterTest('ACBrOpenDelivery.Schema.Image', TTestImage);
  _RegisterTest('ACBrOpenDelivery.Schema.Item', TTestItem);
  _RegisterTest('ACBrOpenDelivery.Schema.ItemOffer', TTestItemOffer);
  _RegisterTest('ACBrOpenDelivery.Schema.Menu', TTestMenu);
  _RegisterTest('ACBrOpenDelivery.Schema.NutritionalInfo', TTestNutritionalInfo);
  _RegisterTest('ACBrOpenDelivery.Schema.Option', TTestOption);
  _RegisterTest('ACBrOpenDelivery.Schema.OptionGroup', TTestOptionGroup);
  _RegisterTest('ACBrOpenDelivery.Schema.Order', TTestOrder);
  _RegisterTest('ACBrOpenDelivery.Schema.OrderAddress', TTestOrderAddress);
  _RegisterTest('ACBrOpenDelivery.Schema.OrderCustomer', TTestOrderCustomer);
  _RegisterTest('ACBrOpenDelivery.Schema.OrderDelivery', TTestOrderDelivery);
  _RegisterTest('ACBrOpenDelivery.Schema.OrderTakeout', TTestOrderTakeout);
  _RegisterTest('ACBrOpenDelivery.Schema.Polygon', TTestPolygon);
  _RegisterTest('ACBrOpenDelivery.Schema.Price', TTestPrice);
  _RegisterTest('ACBrOpenDelivery.Schema.Radius', TTestRadius);
  _RegisterTest('ACBrOpenDelivery.Schema.Service', TTestService);
  _RegisterTest('ACBrOpenDelivery.Schema.ServiceArea', TTestServiceArea);
  _RegisterTest('ACBrOpenDelivery.Schema.ServiceHour', TTestServiceHour);
  _RegisterTest('ACBrOpenDelivery.Schema.TimePeriod', TTestTimePeriod);
end.
