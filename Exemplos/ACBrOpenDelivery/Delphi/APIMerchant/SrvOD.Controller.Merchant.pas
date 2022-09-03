{$A8,B-,C+,D+,E-,F-,G+,H+,I+,J-,K-,L+,M-,N-,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$MINSTACKSIZE $00004000}
{$MAXSTACKSIZE $00100000}
{$IMAGEBASE $00400000}
{$APPTYPE GUI}
{$WARN SYMBOL_DEPRECATED ON}
{$WARN SYMBOL_LIBRARY ON}
{$WARN SYMBOL_PLATFORM ON}
{$WARN SYMBOL_EXPERIMENTAL ON}
{$WARN UNIT_LIBRARY ON}
{$WARN UNIT_PLATFORM ON}
{$WARN UNIT_DEPRECATED ON}
{$WARN UNIT_EXPERIMENTAL ON}
{$WARN HRESULT_COMPAT ON}
{$WARN HIDING_MEMBER ON}
{$WARN HIDDEN_VIRTUAL ON}
{$WARN GARBAGE ON}
{$WARN BOUNDS_ERROR ON}
{$WARN ZERO_NIL_COMPAT ON}
{$WARN STRING_CONST_TRUNCED ON}
{$WARN FOR_LOOP_VAR_VARPAR ON}
{$WARN TYPED_CONST_VARPAR ON}
{$WARN ASG_TO_TYPED_CONST ON}
{$WARN CASE_LABEL_RANGE ON}
{$WARN FOR_VARIABLE ON}
{$WARN CONSTRUCTING_ABSTRACT ON}
{$WARN COMPARISON_FALSE ON}
{$WARN COMPARISON_TRUE ON}
{$WARN COMPARING_SIGNED_UNSIGNED ON}
{$WARN COMBINING_SIGNED_UNSIGNED ON}
{$WARN UNSUPPORTED_CONSTRUCT ON}
{$WARN FILE_OPEN ON}
{$WARN FILE_OPEN_UNITSRC ON}
{$WARN BAD_GLOBAL_SYMBOL ON}
{$WARN DUPLICATE_CTOR_DTOR ON}
{$WARN INVALID_DIRECTIVE ON}
{$WARN PACKAGE_NO_LINK ON}
{$WARN PACKAGED_THREADVAR ON}
{$WARN IMPLICIT_IMPORT ON}
{$WARN HPPEMIT_IGNORED ON}
{$WARN NO_RETVAL ON}
{$WARN USE_BEFORE_DEF ON}
{$WARN FOR_LOOP_VAR_UNDEF ON}
{$WARN UNIT_NAME_MISMATCH ON}
{$WARN NO_CFG_FILE_FOUND ON}
{$WARN IMPLICIT_VARIANTS ON}
{$WARN UNICODE_TO_LOCALE ON}
{$WARN LOCALE_TO_UNICODE ON}
{$WARN IMAGEBASE_MULTIPLE ON}
{$WARN SUSPICIOUS_TYPECAST ON}
{$WARN PRIVATE_PROPACCESSOR ON}
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}
{$WARN OPTION_TRUNCATED ON}
{$WARN WIDECHAR_REDUCED ON}
{$WARN DUPLICATES_IGNORED ON}
{$WARN UNIT_INIT_SEQ ON}
{$WARN LOCAL_PINVOKE ON}
{$WARN MESSAGE_DIRECTIVE ON}
{$WARN TYPEINFO_IMPLICITLY_ADDED ON}
{$WARN RLINK_WARNING ON}
{$WARN IMPLICIT_STRING_CAST ON}
{$WARN IMPLICIT_STRING_CAST_LOSS ON}
{$WARN EXPLICIT_STRING_CAST OFF}
{$WARN EXPLICIT_STRING_CAST_LOSS OFF}
{$WARN CVT_WCHAR_TO_ACHAR ON}
{$WARN CVT_NARROWING_STRING_LOST ON}
{$WARN CVT_ACHAR_TO_WCHAR ON}
{$WARN CVT_WIDENING_STRING_LOST ON}
{$WARN NON_PORTABLE_TYPECAST ON}
{$WARN XML_WHITESPACE_NOT_ALLOWED ON}
{$WARN XML_UNKNOWN_ENTITY ON}
{$WARN XML_INVALID_NAME_START ON}
{$WARN XML_INVALID_NAME ON}
{$WARN XML_EXPECTED_CHARACTER ON}
{$WARN XML_CREF_NO_RESOLVE ON}
{$WARN XML_NO_PARM ON}
{$WARN XML_NO_MATCHING_PARM ON}
{$WARN IMMUTABLE_STRINGS OFF}
unit SrvOD.Controller.Merchant;

interface

uses
  Horse,
  ACBrJSON,
  ACBrOpenDeliverySchemaClasses,
  pcnConversaoOD,
  System.Classes,
  System.DateUtils,
  System.Types,
  System.SysUtils;

procedure Registry;
function CarregarDadosMerchant: TACBrOpenDeliverySchemaMerchant;

implementation

const
  API_KEY_ACBR = '9AD4B257-F630-4A84-BAB9-AD003935B160';

procedure LogRequest(AAPIKey: string);
var
  LFileName: string;
begin
  LFileName := ExtractFilePath(GetModuleName(HInstance)) +
    FormatDateTime('yyyyMMdd_hhmmss', Now) + AAPIKey + '.txt';
  with TStringList.Create do
  try
    Text := LFileName;
    SaveToFile(LFileName);
  finally
    Free;
  end;
end;

procedure GetInformationOfAMerchant(Req: THorseRequest; Res: THorseResponse);
var
  LJSONObject: TACBrJSONObject;
  LMerchant: TACBrOpenDeliverySchemaMerchant;
  LAPIKey: string;
begin
  // X-API-KEY é a identificação do MarketPlace para o Merchant
  // Pelo X-API-KEY que a aplicação do Merchant irá identificar
  // qual marketplace está fazendo a requisição.
  LAPIKey := Req.Headers.Field('X-API-KEY').AsString;
  LogRequest(LAPIKey);

  // Apenas como exemplo para validar a credencial de um marketplace
  // que o Merchant não esteja esperando
  if LAPIKey.ToLower <> API_KEY_ACBR.ToLower then
    raise EHorseException.New
            .Status(THTTPStatus.BadRequest)
            .Error('API-KEY inválida.');

  LMerchant := CarregarDadosMerchant;
  try
    LJSONObject := TACBrJSONObject.Parse(LMerchant.AsJSON);
    Res.Send<TACBrJSONObject>(LJSONObject);
  finally
    LMerchant.Free;
  end;
end;

procedure Registry;
begin
  THorse
    .Get('v1/merchant', GetInformationOfAMerchant);
end;

function CarregarDadosMerchant: TACBrOpenDeliverySchemaMerchant;
begin
  // Carregar os Dados do Merchant na base e setar as
  // informações como mostrado abaixo
  Result := TACBrOpenDeliverySchemaMerchant.Create;
  Result.lastUpdate := Now;
  Result.TTL := 1001;
  Result.id := '14417282000131-7cf42549-aff0-4bfd-a33b-83f4c08ff441';
  Result.status := sAvailable;
  Result.basicInfo.name := 'GET IT Restaurante e Pizzaria';
  Result.basicInfo.document := '14417282000131';
  Result.basicInfo.corporateName := 'GET IT TECHNOLOGY';
  Result.basicInfo.description := 'GET IT Restaurante e Pizzaria especializado em refeições rápidas';
  Result.basicInfo.averagePreparationTime := 30;
  Result.basicInfo.averageTicket := 25;
  Result.basicInfo.minOrderValue.value := 10;
  Result.basicInfo.merchantType := mtRestaurant;
  Result.basicInfo.merchantCategories := [mcPizza, mcBurgers];
  Result.basicInfo.createdAt := Now;

  // Endereço e contatos do Merchant
  Result.basicInfo.address.country := 'BR';
  Result.basicInfo.address.state := 'SP';
  Result.basicInfo.address.city := 'São Paulo';
  Result.basicInfo.address.district := 'Vila Carrao';
  Result.basicInfo.address.street := 'Rua Cel. Irineu de Castro';
  Result.basicInfo.address.number := '43';
  Result.basicInfo.address.postalCode := '03333-050';
  Result.basicInfo.address.complement := 'Sala 1302 13o. Andar';
  Result.basicInfo.address.reference := 'Shopping Anália Franco';
  Result.basicInfo.address.latitude := -23.54809;
  Result.basicInfo.address.longitude := -46.63638;
  Result.basicInfo.contactEmails := ['emailmerchant@gmail.com'];
  Result.basicInfo.contactPhones.commercialNumber := '999999999';
  Result.basicInfo.contactPhones.whatsappNumber := '99999999';

  // Logo do Merchant
  Result.basicInfo.logoImage.URL := 'https://portal.tdevrocks.com.br/fotos/logo.png';
  Result.basicInfo.logoImage.CRC_32 := '96b41025';
  Result.basicInfo.bannerImage.URL := 'https://portal.tdevrocks.com.br/fotos/logo.png';
  Result.basicInfo.bannerImage.CRC_32 := '96b41025';

  // Serviços
  Result.services.New;
  Result.services[0].id := '11a5608c-20cd-4c9d-be81-6e96be9b5c0a';
  Result.services[0].status := sAvailable;
  Result.services[0].serviceType := stDelivery;
  Result.services[0].menuId := 'a3e91092-02cb-4e0d-9a84-aedc9e07ca9e';
  Result.services[0].serviceArea.id := '5dabade6-0de0-49d4-ba4a-cd8a6a194758';
  Result.services[0].serviceArea.polygon.New;
  Result.services[0].serviceArea.polygon[0].price.value := 5;
  Result.services[0].serviceArea.geoRadius.geoMidpointLatitude := -23.54809;
  Result.services[0].serviceArea.geoRadius.geoMidpointLongitude := -46.63638;
  Result.services[0].serviceArea.geoRadius.radius.New;
  Result.services[0].serviceArea.geoRadius.radius[0].size := 5;
  Result.services[0].serviceArea.geoRadius.radius[0].price.value := 5;

  // Service Hours
  Result.services[0].serviceHours.id := '';
  Result.services[0].serviceHours.weekHours.New;
  Result.services[0].serviceHours.weekHours[0].dayOfWeek :=
    [dwTuesday, dwWednesday, dwThursday, dwFriday, dwSaturday, dwSunday];
  Result.services[0].serviceHours.weekHours[0].timePeriods.startTime := EncodeTime(8, 0, 0, 0);
  Result.services[0].serviceHours.weekHours[0].timePeriods.endTime := EncodeTime(23, 59, 0, 0);

  // Menus
  Result.menus.New;
  Result.menus[0].id := '595ff685-c2ae-46e7-8b0a-45e589910322';
  Result.menus[0].name := 'Pizzas';
  Result.menus[0].description := 'Pizzas';
  Result.menus[0].externalCode := '10500';
  Result.menus[0].disclaimer := 'Pizzas Salgadas e Doces';
  Result.menus[0].categoryId := ['949486c6-5f88-4426-a08e-85ba35dd0142',
    'a1fb333e-74b1-45d2-ad49-71c6e0c42d40', '27644ffa-7e39-402f-959a-08dcf41bde44'];

  Result.menus.New;
  Result.menus[1].id := '4e5ea484-5e70-4ea2-b373-031bf681fae4';
  Result.menus[1].name := 'Hamburguers';
  Result.menus[1].description := 'Hamburguers';
  Result.menus[1].externalCode := '10600';
  Result.menus[1].disclaimer := 'Hamburguers';
  Result.menus[1].categoryId := ['a1fb333e-74b1-45d2-ad49-71c6e0c42d40'];

  Result.menus.New;
  Result.menus[2].id := '46624792-a3aa-42e1-a829-69f76e87d153';
  Result.menus[2].name := 'Sucos';
  Result.menus[2].description := 'Sucos';
  Result.menus[2].externalCode := '10700';
  Result.menus[2].disclaimer := 'Sucos';
  Result.menus[2].categoryId := ['27644ffa-7e39-402f-959a-08dcf41bde44'];

  // Items
  Result.items.New;
  Result.items[0].id := '5b1ece82-9a3c-4693-af7c-d19969e50881';
  Result.items[0].name := 'Pepperoni';
  Result.items[0].description := 'Pizza de Pepperoni.';
  Result.items[0].serving := 8;
  Result.items[0]._unit := 'KG';
  Result.items[0].ean := '7896005202074';
  Result.items[0].externalCode := '5b1ece82';
  Result.items[0].image.URL := 'https://portal.tdevrocks.com.br/fotos/pizza-3.jpg';
  Result.items[0].image.CRC_32 := '5b1ece82';
  Result.items[0].nutritionalInfo.description := 'Contém conservantes';
  Result.items[0].nutritionalInfo.calories := '2000 Cal';
  Result.items[0].nutritionalInfo.isAlcoholic := False;

  Result.items.New;
  Result.items[1].id := '5cb0e8ed-620c-467d-8b43-fd2d04bdbb85';
  Result.items[1].name := 'Mussarela';
  Result.items[1].description := 'Meia pizza de mussarela';
  Result.items[1].serving := 8;
  Result.items[1]._unit := 'KG';
  Result.items[1].ean := '7896005202074';
  Result.items[1].externalCode := '5cb0e8ed';
  Result.items[1].image.URL := 'https://portal.tdevrocks.com.br/fotos/pizza-1.jpeg';
  Result.items[1].image.CRC_32 := '5b1ece82';
  Result.items[1].nutritionalInfo.description := 'Contém conservantes';
  Result.items[1].nutritionalInfo.calories := '2000 Cal';
  Result.items[1].nutritionalInfo.isAlcoholic := False;

  Result.items.New;
  Result.items[2].id := '0122764d-91b2-485d-8544-baada46cf169';
  Result.items[2].name := 'Portuguesa';
  Result.items[2].description := 'Portuguesa';
  Result.items[2].serving := 8;
  Result.items[2]._unit := 'KG';
  Result.items[2].ean := '7896005202074';
  Result.items[2].externalCode := '0122764d';
  Result.items[2].image.URL := 'https://food-company.com/image.jpg';
  Result.items[2].image.CRC_32 := '0122764d';
  Result.items[2].nutritionalInfo.description := 'Contém conservantes';
  Result.items[2].nutritionalInfo.calories := '2000 Cal';
  Result.items[2].nutritionalInfo.isAlcoholic := False;

  Result.items.New;
  Result.items[3].id := '584f4e82-37f1-49c0-8fd3-79586fab3b15';
  Result.items[3].name := 'Big Burguer';
  Result.items[3].description := 'Big Burguer';
  Result.items[3].serving := 1;
  Result.items[3]._unit := 'KG';
  Result.items[3].ean := '7896005202074';
  Result.items[3].externalCode := '584f4e82';
  Result.items[3].image.URL := 'https://portal.tdevrocks.com.br/fotos/bigburguer.jpg';
  Result.items[3].image.CRC_32 := '584f4e82';
  Result.items[3].nutritionalInfo.description := 'Contém conservantes';
  Result.items[3].nutritionalInfo.calories := '2000 Cal';
  Result.items[3].nutritionalInfo.isAlcoholic := False;

  Result.items.New;
  Result.items[4].id := '9695853c-fbf6-4e46-a098-fab148299d5b';
  Result.items[4].name := 'Mega Burguer';
  Result.items[4].description := 'Mega Burguer';
  Result.items[4].serving := 1;
  Result.items[4]._unit := 'KG';
  Result.items[4].ean := '7896005202074';
  Result.items[4].externalCode := '233467';
  Result.items[4].image.URL := 'https://portal.tdevrocks.com.br/fotos/megaburguer.jpg';
  Result.items[4].image.CRC_32 := '9695853c';
  Result.items[4].nutritionalInfo.description := '';
  Result.items[4].nutritionalInfo.calories := '2000 Cal';
  Result.items[4].nutritionalInfo.isAlcoholic := False;

  Result.items.New;
  Result.items[5].id := 'fe49c1d2-1f0f-41d7-b42b-fa40fd8d58cb';
  Result.items[5].name := 'Vege Burguer';
  Result.items[5].description := 'Hamburguer vegetariano';
  Result.items[5].serving := 1;
  Result.items[5]._unit := 'KG';
  Result.items[5].ean := '7896005202074';
  Result.items[5].externalCode := 'fe49c1d2';
  Result.items[5].image.URL := 'https://portal.tdevrocks.com.br/fotos/vegeburguer.jpg';
  Result.items[5].image.CRC_32 := 'fe49c1d2';
  Result.items[5].nutritionalInfo.description := 'Não contém conservantes nem glútem';
  Result.items[5].nutritionalInfo.calories := '1500 Cal';
  Result.items[5].nutritionalInfo.isAlcoholic := False;

  Result.items.New;
  Result.items[6].id := 'e34c43b8-fe10-4240-83fb-c8968dff42c6';
  Result.items[6].name := 'Morango com Leite';
  Result.items[6].description := 'Morango com Leite';
  Result.items[6].serving := 1;
  Result.items[6]._unit := 'LT';
  Result.items[6].ean := '7896005202074';
  Result.items[6].externalCode := 'e34c43b8';
  Result.items[6].image.URL := 'https://portal.tdevrocks.com.br/fotos/morangoleite.jpg';
  Result.items[6].image.CRC_32 := 'e34c43b8';
  Result.items[6].nutritionalInfo.description := 'Natural';
  Result.items[6].nutritionalInfo.calories := '150 Cal';
  Result.items[6].nutritionalInfo.isAlcoholic := False;

  Result.items.New;
  Result.items[7].id := '3d235a23-1eca-4069-a0f4-fd081fc95e59';
  Result.items[7].name := 'Laranja Natural';
  Result.items[7].description := 'Laranja Natural';
  Result.items[7].serving := 1;
  Result.items[7]._unit := 'LT';
  Result.items[7].ean := '7896005202074';
  Result.items[7].externalCode := '3d235a23';
  Result.items[7].image.URL := 'https://portal.tdevrocks.com.br/fotos/vegeburguer.jpg';
  Result.items[7].image.CRC_32 := '3d235a23';
  Result.items[7].nutritionalInfo.description := 'Não contém conservantes nem glútem';
  Result.items[7].nutritionalInfo.calories := '1500 Cal';
  Result.items[7].nutritionalInfo.isAlcoholic := False;

  // Categorias
  Result.categories.New;
  Result.categories[0].id := '949486c6-5f88-4426-a08e-85ba35dd0142';
  Result.categories[0].index := 0;
  Result.categories[0].name := 'Pizzas';
  Result.categories[0].description := 'Pizzas Salgadas e Doces';
  Result.categories[0].image.URL := 'https://portal.tdevrocks.com.br/fotos/pizza-1.jpeg';
  Result.categories[0].image.CRC_32 := '949486c6';
  Result.categories[0].externalCode := '1001';
  Result.categories[0].status := sAvailable;
  Result.categories[0].availabilityId := ['32a727df-3723-481d-9cd2-87824109382b'];
  Result.categories[0].itemOfferId := ['8c20cac0-0c41-4591-82c9-68924da8ad2c',
    '946ea0e2-ed0f-4515-b18a-59bce761656f', '403211aa-feda-4236-afd4-1dce581aaeab'];

  Result.categories.New;
  Result.categories[1].id := 'a1fb333e-74b1-45d2-ad49-71c6e0c42d40';
  Result.categories[1].index := 1;
  Result.categories[1].name := 'Hamburguers';
  Result.categories[1].description := 'Hamburguers';
  Result.categories[1].image.URL := 'https://portal.tdevrocks.com.br/fotos/pizza-doce.jpg';
  Result.categories[1].image.CRC_32 := 'a1fb333e';
  Result.categories[1].externalCode := '1002';
  Result.categories[1].status := sAvailable;
  Result.categories[1].availabilityId := ['32a727df-3723-481d-9cd2-87824109382b'];
  Result.categories[1].itemOfferId := ['493444b6-d634-49fd-8559-2fa65d28769e',
    'f6f50f27-f69f-477b-8fb3-2932f8c8008a', '71c721f3-22b8-4448-a410-01489de1639f'];

  Result.categories.New;
  Result.categories[2].id := '27644ffa-7e39-402f-959a-08dcf41bde44';
  Result.categories[2].index := 2;
  Result.categories[2].name := 'Sucos';
  Result.categories[2].description := 'Sucos';
  Result.categories[2].image.URL := 'https://portal.tdevrocks.com.br/fotos/prato-feito.jpg';
  Result.categories[2].image.CRC_32 := '27644ffa';
  Result.categories[2].externalCode := '1003';
  Result.categories[2].status := sAvailable;
  Result.categories[2].availabilityId := ['32a727df-3723-481d-9cd2-87824109382b'];
  Result.categories[2].itemOfferId := ['9a02143d-1292-43aa-a8b1-40c224837301',
    'bf36e7d6-cd36-4096-8e07-1af06cbc63ca'];

  // Item Offers
  Result.itemOffers.New;
  Result.itemOffers[0].id := '8c20cac0-0c41-4591-82c9-68924da8ad2c';
  Result.itemOffers[0].itemId := '5b1ece82-9a3c-4693-af7c-d19969e50881';
  Result.itemOffers[0].index := 0;
  Result.itemOffers[0].price.value := 43.5;
  Result.itemOffers[0].availabilityId := ['32a727df-3723-481d-9cd2-87824109382b'];
  Result.itemOffers[0].optionGroupsId := ['9476e5c2-94ba-43f8-ad52-e57840f42594'];

  Result.itemOffers.New;
  Result.itemOffers[1].id := '946ea0e2-ed0f-4515-b18a-59bce761656f';
  Result.itemOffers[1].itemId := '5cb0e8ed-620c-467d-8b43-fd2d04bdbb85';
  Result.itemOffers[1].index := 1;
  Result.itemOffers[1].price.value := 44.85;
  Result.itemOffers[1].availabilityId := ['32a727df-3723-481d-9cd2-87824109382b'];
  Result.itemOffers[1].optionGroupsId := ['9476e5c2-94ba-43f8-ad52-e57840f42594'];

  Result.itemOffers.New;
  Result.itemOffers[2].id := '403211aa-feda-4236-afd4-1dce581aaeab';
  Result.itemOffers[2].itemId := '0122764d-91b2-485d-8544-baada46cf169';
  Result.itemOffers[2].index := 2;
  Result.itemOffers[2].price.value := 32.6;
  Result.itemOffers[2].availabilityId := ['32a727df-3723-481d-9cd2-87824109382b'];
  Result.itemOffers[2].optionGroupsId := ['9476e5c2-94ba-43f8-ad52-e57840f42594'];

  Result.itemOffers.New;
  Result.itemOffers[3].id := '493444b6-d634-49fd-8559-2fa65d28769e';
  Result.itemOffers[3].itemId := '584f4e82-37f1-49c0-8fd3-79586fab3b15';
  Result.itemOffers[3].index := 3;
  Result.itemOffers[3].price.value := 59.7;
  Result.itemOffers[3].availabilityId := ['32a727df-3723-481d-9cd2-87824109382b'];
  Result.itemOffers[3].optionGroupsId := ['6b78d86f-76fc-4ba9-b056-4419b7c37bae'];

  Result.itemOffers.New;
  Result.itemOffers[4].id := 'f6f50f27-f69f-477b-8fb3-2932f8c8008a';
  Result.itemOffers[4].itemId := '9695853c-fbf6-4e46-a098-fab148299d5b';
  Result.itemOffers[4].index := 4;
  Result.itemOffers[4].price.value := 59.7;
  Result.itemOffers[4].availabilityId := ['32a727df-3723-481d-9cd2-87824109382b'];
  Result.itemOffers[4].optionGroupsId := ['6b78d86f-76fc-4ba9-b056-4419b7c37bae'];

  Result.itemOffers.New;
  Result.itemOffers[5].id := '71c721f3-22b8-4448-a410-01489de1639f';
  Result.itemOffers[5].itemId := 'fe49c1d2-1f0f-41d7-b42b-fa40fd8d58cb';
  Result.itemOffers[5].index := 5;
  Result.itemOffers[5].price.value := 49.3;
  Result.itemOffers[5].availabilityId := ['32a727df-3723-481d-9cd2-87824109382b'];
  Result.itemOffers[5].optionGroupsId := ['6b78d86f-76fc-4ba9-b056-4419b7c37bae'];

  Result.itemOffers.New;
  Result.itemOffers[6].id := '9a02143d-1292-43aa-a8b1-40c224837301';
  Result.itemOffers[6].itemId := 'e34c43b8-fe10-4240-83fb-c8968dff42c6';
  Result.itemOffers[6].index := 6;
  Result.itemOffers[6].price.value := 9.45;
  Result.itemOffers[6].availabilityId := ['32a727df-3723-481d-9cd2-87824109382b'];
  Result.itemOffers[6].optionGroupsId := ['8479e524-0c81-428d-bb09-46d2997fbd77'];

  Result.itemOffers.New;
  Result.itemOffers[7].id := 'bf36e7d6-cd36-4096-8e07-1af06cbc63ca';
  Result.itemOffers[7].itemId := '3d235a23-1eca-4069-a0f4-fd081fc95e59';
  Result.itemOffers[7].index := 7;
  Result.itemOffers[7].price.value := 6.9;
  Result.itemOffers[7].availabilityId := ['32a727df-3723-481d-9cd2-87824109382b'];
  Result.itemOffers[7].optionGroupsId := ['8479e524-0c81-428d-bb09-46d2997fbd77'];

  // Option Groups - Complementos
  Result.optionGroups.New;
  Result.optionGroups[0].id := '9476e5c2-94ba-43f8-ad52-e57840f42594';
  Result.optionGroups[0].index := 0;
  Result.optionGroups[0].name := 'Escolha uma pizza salgada por favor';
  Result.optionGroups[0].description := 'Escolha uma pizza salgada por favor';
  Result.optionGroups[0].externalCode := '10050';
  Result.optionGroups[0].status := sAvailable;
  Result.optionGroups[0].minPermitted := 1;
  Result.optionGroups[0].maxPermitted := 2;
  Result.optionGroups[0].options.New;
  Result.optionGroups[0].options[0].id := '9be6381d-ffdc-4620-922b-1e3e4733bcd7';
  Result.optionGroups[0].options[0].itemId := '5b1ece82-9a3c-4693-af7c-d19969e50881';
  Result.optionGroups[0].options[0].index := 0;
  Result.optionGroups[0].options[0].price.value := 43.5;
  Result.optionGroups[0].options[0].maxPermitted := 2;

  Result.optionGroups.New;
  Result.optionGroups[1].id := '6b78d86f-76fc-4ba9-b056-4419b7c37bae';
  Result.optionGroups[1].index := 1;
  Result.optionGroups[1].name := 'Escolha um Hamburguer por favor';
  Result.optionGroups[1].description := 'Escolha um Hamburguer por favor';
  Result.optionGroups[1].externalCode := '10060';
  Result.optionGroups[1].status := sAvailable;
  Result.optionGroups[1].minPermitted := 1;
  Result.optionGroups[1].maxPermitted := 2;
  Result.optionGroups[1].options.New;
  Result.optionGroups[1].options[0].id := '7b2832f4-b9c9-403d-8149-678eda0fdfae';
  Result.optionGroups[1].options[0].itemId := '584f4e82-37f1-49c0-8fd3-79586fab3b15';
  Result.optionGroups[1].options[0].index := 0;
  Result.optionGroups[1].options[0].price.value := 59.7;
  Result.optionGroups[1].options[0].maxPermitted := 2;

  Result.optionGroups.New;
  Result.optionGroups[2].id := '8479e524-0c81-428d-bb09-46d2997fbd77';
  Result.optionGroups[2].index := 2;
  Result.optionGroups[2].name := 'Escolha uma bebida por favor';
  Result.optionGroups[2].description := 'Escolha uma bebida por favor';
  Result.optionGroups[2].externalCode := '10070';
  Result.optionGroups[2].status := sAvailable;
  Result.optionGroups[2].minPermitted := 1;
  Result.optionGroups[2].maxPermitted := 2;

  // Availabilities
  Result.availabilities.New;
  Result.availabilities[0].id := '';
  Result.availabilities[0].startDate := EncodeDate(2020, 1, 1);
  Result.availabilities[0].endDate := EncodeDate(2023, 12, 31);
  Result.availabilities[0].hours.New;
  Result.availabilities[0].hours[0].dayOfWeek := [dwMonday, dwTuesday, dwWednesday,
    dwThursday, dwFriday, dwSaturday, dwSunday];
  Result.availabilities[0].hours[0].timePeriods.startTime := EncodeTime(7, 0, 0, 0);
  Result.availabilities[0].hours[0].timePeriods.endTime := EncodeTime(18, 0, 0, 0);
end;

end.
