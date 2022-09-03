{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:  Gabriel Baltazar                               }
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

unit ACBrOpenDeliverySchemaClasses;

interface

uses
  ACBrOpenDeliverySchema,
  ACBrBase,
  ACBrJSON,
  ACBrUtil.Strings,
  pcnConversaoOD,
  SysUtils;

type
  TACBrOpenDeliverySchemaAccessToken = class;
  TACBrOpenDeliverySchemaAcknowledgment = class;
  TACBrOpenDeliverySchemaAddress = class;
  TACBrOpenDeliverySchemaAvailability = class;
  TACBrOpenDeliverySchemaAvailabilityCollection = class;
  TACBrOpenDeliverySchemaBasicInfo = class;
  TACBrOpenDeliverySchemaCategory = class;
  TACBrOpenDeliverySchemaCategoryCollection = class;
  TACBrOpenDeliverySchemaContactPhone = class;
  TACBrOpenDeliverySchemaEvent = class;
  TACBrOpenDeliverySchemaGeoCoordinate = class;
  TACBrOpenDeliverySchemaGeoCoordinateCollection = class;
  TACBrOpenDeliverySchemaGeoRadius = class;
  TACBrOpenDeliverySchemaHolidayHour = class;
  TACBrOpenDeliverySchemaHolidayHourCollection = class;
  TACBrOpenDeliverySchemaHour = class;
  TACBrOpenDeliverySchemaHourCollection = class;
  TACBrOpenDeliverySchemaImage = class;
  TACBrOpenDeliverySchemaItem = class;
  TACBrOpenDeliverySchemaItemCollection = class;
  TACBrOpenDeliverySchemaItemOffer = class;
  TACBrOpenDeliverySchemaItemOfferCollection = class;
  TACBrOpenDeliverySchemaMenu = class;
  TACBrOpenDeliverySchemaMenuCollection = class;
  TACBrOpenDeliverySchemaMerchant = class;
  TACBrOpenDeliverySchemaNutritionalInfo = class;
  TACBrOpenDeliverySchemaOption = class;
  TACBrOpenDeliverySchemaOptionCollection = class;
  TACBrOpenDeliverySchemaOptionGroup = class;
  TACBrOpenDeliverySchemaOptionGroupCollection = class;
  TACBrOpenDeliverySchemaOrder = class;
  TACBrOpenDeliverySchemaOrderAddress = class;
  TACBrOpenDeliverySchemaOrderConfirmation = class;
  TACBrOpenDeliverySchemaOrderCustomer = class;
  TACBrOpenDeliverySchemaOrderDelivery = class;
  TACBrOpenDeliverySchemaOrderDiscount = class;
  TACBrOpenDeliverySchemaOrderDiscountCollection = class;
  TACBrOpenDeliverySchemaOrderDiscountSponsor = class;
  TACBrOpenDeliverySchemaOrderDiscountSponsorCollection = class;
  TACBrOpenDeliverySchemaOrderFee = class;
  TACBrOpenDeliverySchemaOrderFeeCollection = class;
  TACBrOpenDeliverySchemaOrderItem = class;
  TACBrOpenDeliverySchemaOrderItemCollection = class;
  TACBrOpenDeliverySchemaOrderItemOption = class;
  TACBrOpenDeliverySchemaOrderItemOptionCollection = class;
  TACBrOpenDeliverySchemaOrderMerchant = class;
  TACBrOpenDeliverySchemaOrderPayment = class;
  TACBrOpenDeliverySchemaOrderPaymentCollection = class;
  TACBrOpenDeliverySchemaOrderPaymentMethod = class;
  TACBrOpenDeliverySchemaOrderPaymentMethodCollection = class;
  TACBrOpenDeliverySchemaOrderTakeout = class;
  TACBrOpenDeliverySchemaOrderTotal = class;
  TACBrOpenDeliverySchemaPhone = class;
  TACBrOpenDeliverySchemaPolygon = class;
  TACBrOpenDeliverySchemaPolygonCollection = class;
  TACBrOpenDeliverySchemaPrice = class;
  TACBrOpenDeliverySchemaRadius = class;
  TACBrOpenDeliverySchemaRadiusCollection = class;
  TACBrOpenDeliverySchemaService = class;
  TACBrOpenDeliverySchemaServiceCollection = class;
  TACBrOpenDeliverySchemaServiceArea = class;
  TACBrOpenDeliverySchemaServiceHour = class;
  TACBrOpenDeliverySchemaServiceHourCollection = class;
  TACBrOpenDeliverySchemaTimePeriod = class;

  TACBrOpenDeliverySchemaAccessToken = class(TACBrOpenDeliverySchema)
  private
    FAccessToken: string;
    FExpiresIn: Integer;
    FExpiresAt: TDateTime;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    function IsValid: Boolean;

    property AccessToken: string read FAccessToken write FAccessToken;
    property ExpiresIn: Integer read FExpiresIn write FExpiresIn;
    property ExpiresAt: TDateTime read FExpiresAt write FExpiresAt;
  end;

  TACBrOpenDeliverySchemaAcknowledgment = class(TACBrOpenDeliverySchema)
  private
    FId: string;
    FOrderId: string;
    FEventType: TACBrODEventType;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;

    property Id: string read FId write FId;
    property OrderId: string read FOrderId write FOrderId;
    property EventType: TACBrODEventType read FEventType write FEventType;
  end;

  TACBrOpenDeliverySchemaAcknowledgmentCollection = class(TACBrOpenDeliverySchemaArray)
  private
    function GetItem(Index: Integer): TACBrOpenDeliverySchemaAcknowledgment;
    procedure SetItem(Index: Integer; Value: TACBrOpenDeliverySchemaAcknowledgment);
  protected
    function NewSchema: TACBrOpenDeliverySchema; override;
  public
    function New: TACBrOpenDeliverySchemaAcknowledgment;
    property Items[Index: Integer]: TACBrOpenDeliverySchemaAcknowledgment read GetItem write SetItem; default;
  end;

  TACBrOpenDeliverySchemaAddress = class(TACBrOpenDeliverySchema)
  private
    Fdistrict: string;
    Flatitude: Double;
    Fstreet: string;
    FpostalCode: string;
    Fstate: string;
    Fcomplement: string;
    Fnumber: string;
    Fcountry: string;
    Fcity: string;
    Freference: string;
    Flongitude: Double;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;

    property country: string read Fcountry write Fcountry;
    property state: string read Fstate write Fstate;
    property city: string read Fcity write Fcity;
    property district: string read Fdistrict write Fdistrict;
    property street: string read Fstreet write Fstreet;
    property number: string read Fnumber write Fnumber;
    property postalCode: string read FpostalCode write FpostalCode;
    property complement: string read Fcomplement write Fcomplement;
    property reference: string read Freference write Freference;
    property latitude: Double read Flatitude write Flatitude;
    property longitude: Double read Flongitude write Flongitude;
  end;

  TACBrOpenDeliverySchemaAvailability = class(TACBrOpenDeliverySchema)
  private
    Fid: string;
    FstartDate: TDateTime;
    FendDate: TDateTime;
    Fhours: TACBrOpenDeliverySchemaHourCollection;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const AObjectName: string = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;

    property id: string read Fid write Fid;
    property startDate: TDateTime read FstartDate write FstartDate;
    property endDate: TDateTime read FendDate write FendDate;
    property hours: TACBrOpenDeliverySchemaHourCollection read Fhours write Fhours;
  end;

  TACBrOpenDeliverySchemaAvailabilityCollection = class(TACBrOpenDeliverySchemaArray)
  private
    function GetItem(Index: Integer): TACBrOpenDeliverySchemaAvailability;
    procedure SetItem(Index: Integer; Value: TACBrOpenDeliverySchemaAvailability);
  protected
    function NewSchema: TACBrOpenDeliverySchema; override;
  public
    function New: TACBrOpenDeliverySchemaAvailability;
    property Items[Index: Integer]: TACBrOpenDeliverySchemaAvailability read GetItem write SetItem; default;
  end;

  TACBrOpenDeliverySchemaBasicInfo = class(TACBrOpenDeliverySchema)
  private
    Fname: string;
    Fdocument: string;
    FcorporateName: string;
    Fdescription: string;
    FaverageTicket: Double;
    FaveragePreparationTime: Integer;
    FminOrderValue: TACBrOpenDeliverySchemaPrice;
    FmerchantType: TACBrODMerchantType;
    FmerchantCategories: TACBrODMerchantCategoriesArray;
    Faddress: TACBrOpenDeliverySchemaAddress;
    FcontactEmails: TSplitResult;
    FcontactPhones: TACBrOpenDeliverySchemaContactPhone;
    FlogoImage: TACBrOpenDeliverySchemaImage;
    FbannerImage: TACBrOpenDeliverySchemaImage;
    FcreatedAt: TDateTime;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const AObjectName: string = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;

    property name: string read Fname write Fname;
    property document: string read Fdocument write Fdocument;
    property corporateName: string read FcorporateName write FcorporateName;
    property description: string read Fdescription write Fdescription;
    property averageTicket: Double read FaverageTicket write FaverageTicket;
    property averagePreparationTime: Integer read FaveragePreparationTime write FaveragePreparationTime;
    property minOrderValue: TACBrOpenDeliverySchemaPrice read FminOrderValue write FminOrderValue;
    property merchantType: TACBrODMerchantType read FmerchantType write FmerchantType;
    property merchantCategories: TACBrODMerchantCategoriesArray read FmerchantCategories write FmerchantCategories;
    property address: TACBrOpenDeliverySchemaAddress read Faddress write Faddress;
    property contactEmails: TSplitResult read FcontactEmails write FcontactEmails;
    property contactPhones: TACBrOpenDeliverySchemaContactPhone read FcontactPhones write FcontactPhones;
    property logoImage: TACBrOpenDeliverySchemaImage read FlogoImage write FlogoImage;
    property bannerImage: TACBrOpenDeliverySchemaImage read FbannerImage write FbannerImage;
    property createdAt: TDateTime read FcreatedAt write FcreatedAt;
  end;

  TACBrOpenDeliverySchemaCategory = class(TACBrOpenDeliverySchema)
  private
    Fid: string;
    Findex: Integer;
    Fname: string;
    Fdescription: string;
    FexternalCode: string;
    Fstatus: TACBrODStatus;
    FavailabilityId: TSplitResult;
    FitemOfferId: TSplitResult;
    Fimage: TACBrOpenDeliverySchemaImage;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const AObjectName: string = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;

    property id: string read Fid write Fid;
    property index: Integer read Findex write Findex;
    property name: string read Fname write Fname;
    property description: string read Fdescription write Fdescription;
    property externalCode: string read FexternalCode write FexternalCode;
    property status: TACBrODStatus read Fstatus write Fstatus;
    property availabilityId: TSplitResult read FavailabilityId write FavailabilityId;
    property itemOfferId: TSplitResult read FitemOfferId write FitemOfferId;
    property image: TACBrOpenDeliverySchemaImage read Fimage write Fimage;
  end;

  TACBrOpenDeliverySchemaCategoryCollection = class(TACBrOpenDeliverySchemaArray)
  private
    function GetItem(Index: Integer): TACBrOpenDeliverySchemaCategory;
    procedure SetItem(Index: Integer; Value: TACBrOpenDeliverySchemaCategory);
  protected
    function NewSchema: TACBrOpenDeliverySchema; override;
  public
    function New: TACBrOpenDeliverySchemaCategory;
    property Items[Index: Integer]: TACBrOpenDeliverySchemaCategory read GetItem write SetItem; default;
  end;

  TACBrOpenDeliverySchemaContactPhone = class(TACBrOpenDeliverySchema)
  private
    FcommercialNumber: string;
    FwhatsappNumber: string;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;

    property commercialNumber: string read FcommercialNumber write FcommercialNumber;
    property whatsappNumber: string read FwhatsappNumber write FwhatsappNumber;
  end;

  TACBrOpenDeliverySchemaError = class(TACBrOpenDeliverySchema)
  private
    FTitle: string;
    FStatus: Integer;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;

    property Status: Integer read FStatus write FStatus;
    property Title: string read FTitle write FTitle;
  end;

  TACBrOpenDeliverySchemaEvent = class(TACBrOpenDeliverySchema)
  private
    FEventType: TACBrODEventType;
    FEventId: string;
    FOrderId: string;
    FOrderURL: string;
    FCreatedAt: TDateTime;
    FSourceAppId: string;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;

    property EventId: string read FEventId write FEventId;
    property EventType: TACBrODEventType read FEventType write FEventType;
    property OrderId: string read FOrderId write FOrderId;
    property OrderURL: string read FOrderURL write FOrderURL;
    property CreatedAt: TDateTime read FCreatedAt write FCreatedAt;
    property SourceAppId: string read FSourceAppId write FSourceAppId;
  end;

  TACBrOpenDeliverySchemaEventCollection = class(TACBrOpenDeliverySchemaArray)
  private
    function GetItem(Index: Integer): TACBrOpenDeliverySchemaEvent;
    procedure SetItem(Index: Integer; Value: TACBrOpenDeliverySchemaEvent);
  protected
    function NewSchema: TACBrOpenDeliverySchema; override;
  public
    function New: TACBrOpenDeliverySchemaEvent;
    property Items[Index: Integer]: TACBrOpenDeliverySchemaEvent read GetItem write SetItem; default;
  end;

  TACBrOpenDeliverySchemaGeoCoordinate = class(TACBrOpenDeliverySchema)
  private
    Flatitude: Double;
    Flongitude: Double;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;

    property latitude: Double read Flatitude write Flatitude;
    property longitude: Double read Flongitude write Flongitude;
  end;

  TACBrOpenDeliverySchemaGeoCoordinateCollection = class(TACBrOpenDeliverySchemaArray)
  private
    function GetItem(Index: Integer): TACBrOpenDeliverySchemaGeoCoordinate;
    procedure SetItem(Index: Integer; Value: TACBrOpenDeliverySchemaGeoCoordinate);
  protected
    function NewSchema: TACBrOpenDeliverySchema; override;
  public
    function New: TACBrOpenDeliverySchemaGeoCoordinate;
    property Items[Index: Integer]: TACBrOpenDeliverySchemaGeoCoordinate read GetItem write SetItem; default;
  end;

  TACBrOpenDeliverySchemaGeoRadius = class(TACBrOpenDeliverySchema)
  private
    FgeoMidpointLatitude: Double;
    FgeoMidpointLongitude: Double;
    Fradius: TACBrOpenDeliverySchemaRadiusCollection;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const AObjectName: string = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;

    property geoMidpointLatitude: Double read FgeoMidpointLatitude write FgeoMidpointLatitude;
    property geoMidpointLongitude: Double read FgeoMidpointLongitude write FgeoMidpointLongitude;
    property radius: TACBrOpenDeliverySchemaRadiusCollection read Fradius write Fradius;
  end;

  TACBrOpenDeliverySchemaHolidayHour = class(TACBrOpenDeliverySchema)
  private
    Fdate: TDateTime;
    FtimePeriods: TACBrOpenDeliverySchemaTimePeriod;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const AObjectName: string = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;

    property date: TDateTime read Fdate write Fdate;
    property timePeriods: TACBrOpenDeliverySchemaTimePeriod read FtimePeriods write FtimePeriods;
  end;

  TACBrOpenDeliverySchemaHolidayHourCollection = class(TACBrOpenDeliverySchemaArray)
  private
    function GetItem(Index: Integer): TACBrOpenDeliverySchemaHolidayHour;
    procedure SetItem(Index: Integer; Value: TACBrOpenDeliverySchemaHolidayHour);
  protected
    function NewSchema: TACBrOpenDeliverySchema; override;
  public
    function New: TACBrOpenDeliverySchemaHolidayHour;
    property Items[Index: Integer]: TACBrOpenDeliverySchemaHolidayHour read GetItem write SetItem; default;
  end;

  TACBrOpenDeliverySchemaImage = class(TACBrOpenDeliverySchema)
  private
    FURL: string;
    FCRC_32: string;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    property URL: string read FURL write FURL;
    property CRC_32: string read FCRC_32 write FCRC_32;
  end;

  TACBrOpenDeliverySchemaItem = class(TACBrOpenDeliverySchema)
  private
    Fid: string;
    Fname: string;
    Fdescription: string;
    FexternalCode: string;
    Fimage: TACBrOpenDeliverySchemaImage;
    FnutritionalInfo: TACBrOpenDeliverySchemaNutritionalInfo;
    Fserving: Integer;
    Funit: string;
    Fean: string;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const AObjectName: string = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;

    property id: string read Fid write Fid;
    property name: string read Fname write Fname;
    property description: string read Fdescription write Fdescription;
    property externalCode: string read FexternalCode write FexternalCode;
    property image: TACBrOpenDeliverySchemaImage read Fimage write Fimage;
    property nutritionalInfo: TACBrOpenDeliverySchemaNutritionalInfo read FnutritionalInfo write FnutritionalInfo;
    property serving: Integer read Fserving write Fserving;
    property _unit: string read Funit write Funit;
    property ean: string read Fean write Fean;
  end;

  TACBrOpenDeliverySchemaItemCollection = class(TACBrOpenDeliverySchemaArray)
  private
    function GetItem(Index: Integer): TACBrOpenDeliverySchemaItem;
    procedure SetItem(Index: Integer; Value: TACBrOpenDeliverySchemaItem);
  protected
    function NewSchema: TACBrOpenDeliverySchema; override;
  public
    function New: TACBrOpenDeliverySchemaItem;
    property Items[Index: Integer]: TACBrOpenDeliverySchemaItem read GetItem write SetItem; default;
  end;

  TACBrOpenDeliverySchemaItemOffer = class(TACBrOpenDeliverySchema)
  private
    Fid: string;
    FitemId: string;
    Findex: Integer;
    Fprice: TACBrOpenDeliverySchemaPrice;
    FavailabilityId: TSplitResult;
    FoptionGroupsId: TSplitResult;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const AObjectName: string = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;

    property id: string read Fid write Fid;
    property itemId: string read FitemId write FitemId;
    property index: Integer read Findex write Findex;
    property price: TACBrOpenDeliverySchemaPrice read Fprice write Fprice;
    property availabilityId: TSplitResult read FavailabilityId write FavailabilityId;
    property optionGroupsId: TSplitResult read FoptionGroupsId write FoptionGroupsId;
  end;

  TACBrOpenDeliverySchemaItemOfferCollection = class(TACBrOpenDeliverySchemaArray)
  private
    function GetItem(Index: Integer): TACBrOpenDeliverySchemaItemOffer;
    procedure SetItem(Index: Integer; Value: TACBrOpenDeliverySchemaItemOffer);
  protected
    function NewSchema: TACBrOpenDeliverySchema; override;
  public
    function New: TACBrOpenDeliverySchemaItemOffer;
    property Items[Index: Integer]: TACBrOpenDeliverySchemaItemOffer read GetItem write SetItem; default;
  end;

  TACBrOpenDeliverySchemaMenu = class(TACBrOpenDeliverySchema)
  private
    Fid: string;
    Fname: string;
    Fdescription: string;
    FexternalCode: string;
    Fdisclaimer: string;
    FdisclaimerUrl: string;
    FcategoryId: TSplitResult;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;

    property id: string read Fid write Fid;
    property name: string read Fname write Fname;
    property description: string read Fdescription write Fdescription;
    property externalCode: string read FexternalCode write FexternalCode;
    property disclaimer: string read Fdisclaimer write Fdisclaimer;
    property disclaimerUrl: string read FdisclaimerUrl write FdisclaimerUrl;
    property categoryId: TSplitResult read FcategoryId write FcategoryId;
  end;

  TACBrOpenDeliverySchemaMenuCollection = class(TACBrOpenDeliverySchemaArray)
  private
    function GetItem(Index: Integer): TACBrOpenDeliverySchemaMenu;
    procedure SetItem(Index: Integer; Value: TACBrOpenDeliverySchemaMenu);
  protected
    function NewSchema: TACBrOpenDeliverySchema; override;
  public
    function New: TACBrOpenDeliverySchemaMenu;
    property Items[Index: Integer]: TACBrOpenDeliverySchemaMenu read GetItem write SetItem; default;
  end;

  TACBrOpenDeliverySchemaMerchant = class(TACBrOpenDeliverySchema)
  private
    FlastUpdate: TDateTime;
    Fid: string;
    FTTL: Integer;
    Fstatus: TACBrODStatus;
    FbasicInfo: TACBrOpenDeliverySchemaBasicInfo;
    Fservices: TACBrOpenDeliverySchemaServiceCollection;
    Fmenus: TACBrOpenDeliverySchemaMenuCollection;
    Fitems: TACBrOpenDeliverySchemaItemCollection;
    Fcategories: TACBrOpenDeliverySchemaCategoryCollection;
    FitemOffers: TACBrOpenDeliverySchemaItemOfferCollection;
    FoptionGroups: TACBrOpenDeliverySchemaOptionGroupCollection;
    Favailabilities: TACBrOpenDeliverySchemaAvailabilityCollection;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const AObjectName: string = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;

    property lastUpdate: TDateTime read FlastUpdate write FlastUpdate;
    property TTL: Integer read FTTL write FTTL;
    property id: string read Fid write Fid;
    property status: TACBrODStatus read Fstatus write Fstatus;
    property basicInfo: TACBrOpenDeliverySchemaBasicInfo read FbasicInfo write FbasicInfo;
    property services: TACBrOpenDeliverySchemaServiceCollection read Fservices write Fservices;
    property menus: TACBrOpenDeliverySchemaMenuCollection read Fmenus write Fmenus;
    property items: TACBrOpenDeliverySchemaItemCollection read Fitems write Fitems;
    property categories: TACBrOpenDeliverySchemaCategoryCollection read Fcategories write Fcategories;
    property itemOffers: TACBrOpenDeliverySchemaItemOfferCollection read FitemOffers write FitemOffers;
    property optionGroups: TACBrOpenDeliverySchemaOptionGroupCollection read FoptionGroups write FoptionGroups;
    property availabilities: TACBrOpenDeliverySchemaAvailabilityCollection read Favailabilities write Favailabilities;
  end;

  TACBrOpenDeliverySchemaNutritionalInfo = class(TACBrOpenDeliverySchema)
  private
    Fdescription: string;
    Fcalories: string;
    Fallergen: TACBrODAllergenArray;
    Fadditives: TSplitResult;
    FsuitableDiet: TACBrODSuitableDietArray;
    FisAlcoholic: Boolean;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;

    property description: string read Fdescription write Fdescription;
    property calories: string read Fcalories write Fcalories;
    property allergen: TACBrODAllergenArray read Fallergen write Fallergen;
    property additives: TSplitResult read Fadditives write Fadditives;
    property suitableDiet: TACBrODSuitableDietArray read FsuitableDiet write FsuitableDiet;
    property isAlcoholic: Boolean read FisAlcoholic write FisAlcoholic;
  end;

  TACBrOpenDeliverySchemaOption = class(TACBrOpenDeliverySchema)
  private
    Fid: string;
    FitemId: string;
    Findex: Integer;
    Fprice: TACBrOpenDeliverySchemaPrice;
    FmaxPermitted: Integer;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const AObjectName: string = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;

    property id: string read Fid write Fid;
    property itemId: string read FitemId write FitemId;
    property index: Integer read Findex write Findex;
    property price: TACBrOpenDeliverySchemaPrice read Fprice write Fprice;
    property maxPermitted: Integer read FmaxPermitted write FmaxPermitted;
  end;

  TACBrOpenDeliverySchemaOptionCollection = class(TACBrOpenDeliverySchemaArray)
  private
    function GetItem(Index: Integer): TACBrOpenDeliverySchemaOption;
    procedure SetItem(Index: Integer; Value: TACBrOpenDeliverySchemaOption);
  protected
    function NewSchema: TACBrOpenDeliverySchema; override;
  public
    function New: TACBrOpenDeliverySchemaOption;
    property Items[Index: Integer]: TACBrOpenDeliverySchemaOption read GetItem write SetItem; default;
  end;

  TACBrOpenDeliverySchemaOptionGroup = class(TACBrOpenDeliverySchema)
  private
    Fid: string;
    Findex: Integer;
    Fname: string;
    Fdescription: string;
    FexternalCode: string;
    Fstatus: TACBrODStatus;
    FminPermitted: Integer;
    FmaxPermitted: Integer;
    Foptions: TACBrOpenDeliverySchemaOptionCollection;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const AObjectName: string = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;

    property id: string read Fid write Fid;
    property index: Integer read Findex write Findex;
    property name: string read Fname write Fname;
    property description: string read Fdescription write Fdescription;
    property externalCode: string read FexternalCode write FexternalCode;
    property status: TACBrODStatus read Fstatus write Fstatus;
    property minPermitted: Integer read FminPermitted write FminPermitted;
    property maxPermitted: Integer read FmaxPermitted write FmaxPermitted;
    property options: TACBrOpenDeliverySchemaOptionCollection read Foptions write Foptions;
  end;

  TACBrOpenDeliverySchemaOptionGroupCollection = class(TACBrOpenDeliverySchemaArray)
  private
    function GetItem(Index: Integer): TACBrOpenDeliverySchemaOptionGroup;
    procedure SetItem(Index: Integer; Value: TACBrOpenDeliverySchemaOptionGroup);
  protected
    function NewSchema: TACBrOpenDeliverySchema; override;
  public
    function New: TACBrOpenDeliverySchemaOptionGroup;
    property Items[Index: Integer]: TACBrOpenDeliverySchemaOptionGroup read GetItem write SetItem; default;
  end;

  TACBrOpenDeliverySchemaOrder = class(TACBrOpenDeliverySchema)
  private
    Fid: string;
    Ftype: TACBrODServiceType;
    FdisplayId: string;
    FsourceAppId: string;
    FcreatedAt: TDateTime;
    ForderTiming: string;
    FpreparationStartDateTime: TDateTime;
    FextraInfo: string;
    Fmerchant: TACBrOpenDeliverySchemaOrderMerchant;
    Fitems: TACBrOpenDeliverySchemaOrderItemCollection;
    FotherFees: TACBrOpenDeliverySchemaOrderFeeCollection;
    Fdiscounts: TACBrOpenDeliverySchemaOrderDiscountCollection;
    Ftotal: TACBrOpenDeliverySchemaOrderTotal;
    Fpayments: TACBrOpenDeliverySchemaOrderPayment;
    Fcustomer: TACBrOpenDeliverySchemaOrderCustomer;
    Fdelivery: TACBrOpenDeliverySchemaOrderDelivery;
    Ftakeout: TACBrOpenDeliverySchemaOrderTakeout;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const AObjectName: string = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;

    property id: string read Fid write Fid;
    property _type: TACBrODServiceType read Ftype write Ftype;
    property displayId: string read FdisplayId write FdisplayId;
    property sourceAppId: string read FsourceAppId write FsourceAppId;
    property createdAt: TDateTime read FcreatedAt write FcreatedAt;
    property orderTiming: string read ForderTiming write ForderTiming;
    property preparationStartDateTime: TDateTime read FpreparationStartDateTime write FpreparationStartDateTime;
    property extraInfo: string read FextraInfo write FextraInfo;
    property merchant: TACBrOpenDeliverySchemaOrderMerchant read Fmerchant write Fmerchant;
    property items: TACBrOpenDeliverySchemaOrderItemCollection read Fitems write Fitems;
    property otherFees: TACBrOpenDeliverySchemaOrderFeeCollection read FotherFees write FotherFees;
    property discounts: TACBrOpenDeliverySchemaOrderDiscountCollection read Fdiscounts write Fdiscounts;
    property total: TACBrOpenDeliverySchemaOrderTotal read Ftotal write Ftotal;
    property payments: TACBrOpenDeliverySchemaOrderPayment read Fpayments write Fpayments;
    property customer: TACBrOpenDeliverySchemaOrderCustomer read Fcustomer write Fcustomer;
    property delivery: TACBrOpenDeliverySchemaOrderDelivery read Fdelivery write Fdelivery;
    property takeout: TACBrOpenDeliverySchemaOrderTakeout read Ftakeout write Ftakeout;
  end;

  TACBrOpenDeliverySchemaOrderAddress = class(TACBrOpenDeliverySchema)
  private
    Fdistrict: string;
    Fstreet: string;
    Fcoordinates: TACBrOpenDeliverySchemaGeoCoordinate;
    FpostalCode: string;
    Fstate: string;
    Fcomplement: string;
    Fnumber: string;
    Fcountry: string;
    Fcity: string;
    Freference: string;
    FformattedAddress: string;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const AObjectName: string = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;

    property country: string read Fcountry write Fcountry;
    property state: string read Fstate write Fstate;
    property city: string read Fcity write Fcity;
    property district: string read Fdistrict write Fdistrict;
    property street: string read Fstreet write Fstreet;
    property number: string read Fnumber write Fnumber;
    property postalCode: string read FpostalCode write FpostalCode;
    property complement: string read Fcomplement write Fcomplement;
    property reference: string read Freference write Freference;
    property formattedAddress: string read FformattedAddress write FformattedAddress;
    property coordinates: TACBrOpenDeliverySchemaGeoCoordinate read Fcoordinates write Fcoordinates;
  end;

  TACBrOpenDeliverySchemaOrderCancelRequest = class(TACBrOpenDeliverySchema)
  private
    Freason: string;
    Fcode: TACBrODCancelRequestCode;
    Fmode: TACBrODCancelRequestMode;
    FoutOfStockItems: TSplitResult;
    FinvalidItems: TSplitResult;
    ForderId: string;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    function AddOutOfStockItems(const AValue: string): TACBrOpenDeliverySchemaOrderCancelRequest;
    function AddInvalidItems(const AValue: string): TACBrOpenDeliverySchemaOrderCancelRequest;

    property orderId: string read ForderId write ForderId;
    property reason: string read Freason write Freason;
    property code: TACBrODCancelRequestCode read Fcode write Fcode;
    property mode: TACBrODCancelRequestMode read Fmode write Fmode;
    property outOfStockItems: TSplitResult read FoutOfStockItems write FoutOfStockItems;
    property invalidItems: TSplitResult read FinvalidItems write FinvalidItems;
  end;

  TACBrOpenDeliverySchemaOrderCancelDenyRequest = class(TACBrOpenDeliverySchema)
  private
    Freason: string;
    Fcode: TACBrODDenyCancelCode;
    ForderId: string;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;

    property orderId: string read ForderId write ForderId;
    property reason: string read Freason write Freason;
    property code: TACBrODDenyCancelCode read Fcode write Fcode;
  end;

  TACBrOpenDeliverySchemaOrderConfirmation = class(TACBrOpenDeliverySchema)
  private
    Fid: string;
    Freason: string;
    FcreatedAt: TDateTime;
    ForderExternalCode: string;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;

    property id: string read Fid write Fid;
    property reason: string read Freason write Freason;
    property createdAt: TDateTime read FcreatedAt write FcreatedAt;
    property orderExternalCode: string read ForderExternalCode write ForderExternalCode;
  end;

  TACBrOpenDeliverySchemaOrderCustomer = class(TACBrOpenDeliverySchema)
  private
    Fid: string;
    Fphone: TACBrOpenDeliverySchemaPhone;
    FdocumentNumber: string;
    Fname: string;
    FordersCountOnMerchant: Integer;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const AObjectName: string = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;

    property id: string read Fid write Fid;
    property phone: TACBrOpenDeliverySchemaPhone read Fphone write Fphone;
    property documentNumber: string read FdocumentNumber write FdocumentNumber;
    property name: string read Fname write Fname;
    property ordersCountOnMerchant: Integer read FordersCountOnMerchant write FordersCountOnMerchant;
  end;

  TACBrOpenDeliverySchemaPhone = class(TACBrOpenDeliverySchema)
  private
    Fnumber: string;
    Fextension: string;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;

    property number: string read Fnumber write Fnumber;
    property extension: string read Fextension write Fextension;
  end;

  TACBrOpenDeliverySchemaOrderDelivery = class(TACBrOpenDeliverySchema)
  private
    FdeliveredBy: TACBrODSponsor;
    FdeliveryAddress: TACBrOpenDeliverySchemaOrderAddress;
    FestimatedDeliveryDateTime: TDateTime;
    FdeliveryDateTime: TDateTime;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const AObjectName: string = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;

    property deliveredBy: TACBrODSponsor read FdeliveredBy write FdeliveredBy;
    property deliveryAddress: TACBrOpenDeliverySchemaOrderAddress read FdeliveryAddress write FdeliveryAddress;
    property estimatedDeliveryDateTime: TDateTime read FestimatedDeliveryDateTime write FestimatedDeliveryDateTime;
    property deliveryDateTime: TDateTime read FdeliveryDateTime write FdeliveryDateTime;
  end;

  TACBrOpenDeliverySchemaOrderDiscount = class(TACBrOpenDeliverySchema)
  private
    Famount: TACBrOpenDeliverySchemaPrice;
    Ftarget: TACBrODDiscountTarget;
    FsponsorshipValues: TACBrOpenDeliverySchemaOrderDiscountSponsorCollection;
    FtargetId: string;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const AObjectName: string = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;

    property amount: TACBrOpenDeliverySchemaPrice read Famount write Famount;
    property target: TACBrODDiscountTarget read Ftarget write Ftarget;
    property targetId: string read FtargetId write FtargetId;
    property sponsorshipValues: TACBrOpenDeliverySchemaOrderDiscountSponsorCollection read FsponsorshipValues write FsponsorshipValues;
  end;

  TACBrOpenDeliverySchemaOrderDiscountCollection = class(TACBrOpenDeliverySchemaArray)
  private
    function GetItem(Index: Integer): TACBrOpenDeliverySchemaOrderDiscount;
    procedure SetItem(Index: Integer; Value: TACBrOpenDeliverySchemaOrderDiscount);
  protected
    function NewSchema: TACBrOpenDeliverySchema; override;
  public
    function New: TACBrOpenDeliverySchemaOrderDiscount;
    property Items[Index: Integer]: TACBrOpenDeliverySchemaOrderDiscount read GetItem write SetItem; default;
  end;

  TACBrOpenDeliverySchemaOrderDiscountSponsor = class(TACBrOpenDeliverySchema)
  private
    Fname: string;
    Famount: TACBrOpenDeliverySchemaPrice;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const AObjectName: string = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;

    property name: string read Fname write Fname;
    property amount: TACBrOpenDeliverySchemaPrice read Famount write Famount;
  end;

  TACBrOpenDeliverySchemaOrderDiscountSponsorCollection = class(TACBrOpenDeliverySchemaArray)
  private
    function GetItem(Index: Integer): TACBrOpenDeliverySchemaOrderDiscountSponsor;
    procedure SetItem(Index: Integer; Value: TACBrOpenDeliverySchemaOrderDiscountSponsor);
  protected
    function NewSchema: TACBrOpenDeliverySchema; override;
  public
    function New: TACBrOpenDeliverySchemaOrderDiscountSponsor;
    property Items[Index: Integer]: TACBrOpenDeliverySchemaOrderDiscountSponsor read GetItem write SetItem; default;
  end;

  TACBrOpenDeliverySchemaOrderFee = class(TACBrOpenDeliverySchema)
  private
    Fname: string;
    Ftype: TACBrODFeeType;
    FreceivedBy: TACBrODFeeReceivedBy;
    FreceiverDocument: string;
    Fprice: TACBrOpenDeliverySchemaPrice;
    Fobservation: string;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const AObjectName: string = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;

    property name: string read Fname write Fname;
    property _type: TACBrODFeeType read Ftype write Ftype;
    property receivedBy: TACBrODFeeReceivedBy read FreceivedBy write FreceivedBy;
    property receiverDocument: string read FreceiverDocument write FreceiverDocument;
    property price: TACBrOpenDeliverySchemaPrice read Fprice write Fprice;
    property observation: string read Fobservation write Fobservation;
  end;

  TACBrOpenDeliverySchemaOrderFeeCollection = class(TACBrOpenDeliverySchemaArray)
  private
    function GetItem(Index: Integer): TACBrOpenDeliverySchemaOrderFee;
    procedure SetItem(Index: Integer; Value: TACBrOpenDeliverySchemaOrderFee);
  protected
    function NewSchema: TACBrOpenDeliverySchema; override;
  public
    function New: TACBrOpenDeliverySchemaOrderFee;
    property Items[Index: Integer]: TACBrOpenDeliverySchemaOrderFee read GetItem write SetItem; default;
  end;

  TACBrOpenDeliverySchemaOrderItem = class(TACBrOpenDeliverySchema)
  private
    Funit: string;
    Fname: string;
    FoptionsPrice: TACBrOpenDeliverySchemaPrice;
    FunitPrice: TACBrOpenDeliverySchemaPrice;
    FspecialInstructions: string;
    Fid: string;
    Findex: Integer;
    Fquantity: Double;
    FtotalPrice: TACBrOpenDeliverySchemaPrice;
    FexternalCode: string;
    Fean: string;
    Foptions: TACBrOpenDeliverySchemaOrderItemOptionCollection;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const AObjectName: string = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;

    property index: Integer read Findex write Findex;
    property id: string read Fid write Fid;
    property name: string read Fname write Fname;
    property externalCode: string read FexternalCode write FexternalCode;
    property _unit: string read Funit write Funit;
    property ean: string read Fean write Fean;
    property quantity: Double read Fquantity write Fquantity;
    property specialInstructions: string read FspecialInstructions write FspecialInstructions;
    property unitPrice: TACBrOpenDeliverySchemaPrice read FunitPrice write FunitPrice;
    property optionsPrice: TACBrOpenDeliverySchemaPrice read FoptionsPrice write FoptionsPrice;
    property totalPrice: TACBrOpenDeliverySchemaPrice read FtotalPrice write FtotalPrice;
    property options: TACBrOpenDeliverySchemaOrderItemOptionCollection read Foptions write Foptions;
  end;

  TACBrOpenDeliverySchemaOrderItemCollection = class(TACBrOpenDeliverySchemaArray)
  private
    function GetItem(Index: Integer): TACBrOpenDeliverySchemaOrderItem;
    procedure SetItem(Index: Integer; Value: TACBrOpenDeliverySchemaOrderItem);
  protected
    function NewSchema: TACBrOpenDeliverySchema; override;
  public
    function New: TACBrOpenDeliverySchemaOrderItem;
    property Items[Index: Integer]: TACBrOpenDeliverySchemaOrderItem read GetItem write SetItem; default;
  end;

  TACBrOpenDeliverySchemaOrderItemOption = class(TACBrOpenDeliverySchema)
  private
    Findex: Integer;
    Fid: string;
    Fname: string;
    FexternalCode: string;
    Funit: string;
    Fean: string;
    Fquantity: Double;
    FunitPrice: TACBrOpenDeliverySchemaPrice;
    FtotalPrice: TACBrOpenDeliverySchemaPrice;
    FspecialInstructions: string;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const AObjectName: string = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;

    property index: Integer read Findex write Findex;
    property id: string read Fid write Fid;
    property name: string read Fname write Fname;
    property externalCode: string read FexternalCode write FexternalCode;
    property _unit: string read Funit write Funit;
    property ean: string read Fean write Fean;
    property quantity: Double read Fquantity write Fquantity;
    property unitPrice: TACBrOpenDeliverySchemaPrice read FunitPrice write FunitPrice;
    property totalPrice: TACBrOpenDeliverySchemaPrice read FtotalPrice write FtotalPrice;
    property specialInstructions: string read FspecialInstructions write FspecialInstructions;
  end;

  TACBrOpenDeliverySchemaOrderItemOptionCollection = class(TACBrOpenDeliverySchemaArray)
  private
    function GetItem(Index: Integer): TACBrOpenDeliverySchemaOrderItemOption;
    procedure SetItem(Index: Integer; Value: TACBrOpenDeliverySchemaOrderItemOption);
  protected
    function NewSchema: TACBrOpenDeliverySchema; override;
  public
    function New: TACBrOpenDeliverySchemaOrderItemOption;
    property Items[Index: Integer]: TACBrOpenDeliverySchemaOrderItemOption read GetItem write SetItem; default;
  end;

  TACBrOpenDeliverySchemaOrderMerchant = class(TACBrOpenDeliverySchema)
  private
    Fid: string;
    Fname: string;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    property id: string read Fid write Fid;
    property name: string read Fname write Fname;
  end;

  TACBrOpenDeliverySchemaOrderPayment = class(TACBrOpenDeliverySchema)
  private
    Fprepaid: Currency;
    Fpending: Currency;
    Fmethods: TACBrOpenDeliverySchemaOrderPaymentMethodCollection;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const AObjectName: string = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    property prepaid: Currency read Fprepaid write Fprepaid;
    property pending: Currency read Fpending write Fpending;
    property methods: TACBrOpenDeliverySchemaOrderPaymentMethodCollection read Fmethods write Fmethods;
  end;

  TACBrOpenDeliverySchemaOrderPaymentCollection = class(TACBrOpenDeliverySchemaArray)
  private
    function GetItem(Index: Integer): TACBrOpenDeliverySchemaOrderPayment;
    procedure SetItem(Index: Integer; Value: TACBrOpenDeliverySchemaOrderPayment);
  protected
    function NewSchema: TACBrOpenDeliverySchema; override;
  public
    function New: TACBrOpenDeliverySchemaOrderPayment;
    property Items[Index: Integer]: TACBrOpenDeliverySchemaOrderPayment read GetItem write SetItem; default;
  end;

  TACBrOpenDeliverySchemaOrderPaymentMethod = class(TACBrOpenDeliverySchema)
  private
    Fvalue: Currency;
    Fcurrency: string;
    Fmethod: TACBrODPaymentMethod;
    Ftype: TACBrODPaymentType;
    FmethodInfo: string;
    FchangeFor: Currency;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    function ChangeValue: Currency;

    property value: Currency read Fvalue write Fvalue;
    property currency: string read Fcurrency write Fcurrency;
    property _type: TACBrODPaymentType read Ftype write Ftype;
    property method: TACBrODPaymentMethod read Fmethod write Fmethod;
    property methodInfo: string read FmethodInfo write FmethodInfo;
    property changeFor: Currency read FchangeFor write FchangeFor;
  end;

  TACBrOpenDeliverySchemaOrderPaymentMethodCollection = class(TACBrOpenDeliverySchemaArray)
  private
    function GetItem(Index: Integer): TACBrOpenDeliverySchemaOrderPaymentMethod;
    procedure SetItem(Index: Integer; Value: TACBrOpenDeliverySchemaOrderPaymentMethod);
  protected
    function NewSchema: TACBrOpenDeliverySchema; override;
  public
    function New: TACBrOpenDeliverySchemaOrderPaymentMethod;
    property Items[Index: Integer]: TACBrOpenDeliverySchemaOrderPaymentMethod read GetItem write SetItem; default;
  end;

  TACBrOpenDeliverySchemaOrderTakeout = class(TACBrOpenDeliverySchema)
  private
    Fmode: TACBrODTakeoutMode;
    FtakeoutDateTime: TDateTime;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;

    property mode: TACBrODTakeoutMode read Fmode write Fmode;
    property takeoutDateTime: TDateTime read FtakeoutDateTime write FtakeoutDateTime;
  end;

  TACBrOpenDeliverySchemaOrderTotal = class(TACBrOpenDeliverySchema)
  private
    FitemsPrice: TACBrOpenDeliverySchemaPrice;
    FotherFees: TACBrOpenDeliverySchemaPrice;
    Fdiscount: TACBrOpenDeliverySchemaPrice;
    ForderAmount: TACBrOpenDeliverySchemaPrice;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const AObjectName: string = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;

    property itemsPrice: TACBrOpenDeliverySchemaPrice read FitemsPrice write FitemsPrice;
    property otherFees: TACBrOpenDeliverySchemaPrice read FotherFees write FotherFees;
    property discount: TACBrOpenDeliverySchemaPrice read Fdiscount write Fdiscount;
    property orderAmount: TACBrOpenDeliverySchemaPrice read ForderAmount write ForderAmount;
  end;

  TACBrOpenDeliverySchemaPolygon = class(TACBrOpenDeliverySchema)
  private
    FgeoCoordinates: TACBrOpenDeliverySchemaGeoCoordinateCollection;
    Fprice: TACBrOpenDeliverySchemaPrice;
    FestimateDeliveryTime: Integer;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const AObjectName: string = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;

    property geoCoordinates: TACBrOpenDeliverySchemaGeoCoordinateCollection read FgeoCoordinates write FgeoCoordinates;
    property price: TACBrOpenDeliverySchemaPrice read Fprice write Fprice;
    property estimateDeliveryTime: Integer read FestimateDeliveryTime write FestimateDeliveryTime;
  end;

  TACBrOpenDeliverySchemaPolygonCollection = class(TACBrOpenDeliverySchemaArray)
  private
    function GetItem(Index: Integer): TACBrOpenDeliverySchemaPolygon;
    procedure SetItem(Index: Integer; Value: TACBrOpenDeliverySchemaPolygon);
  protected
    function NewSchema: TACBrOpenDeliverySchema; override;
  public
    function New: TACBrOpenDeliverySchemaPolygon;
    property Items[Index: Integer]: TACBrOpenDeliverySchemaPolygon read GetItem write SetItem; default;
  end;

  TACBrOpenDeliverySchemaPrice = class(TACBrOpenDeliverySchema)
  private
    Fvalue: Currency;
    Fcurrency: string;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const AObjectName: string = ''); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;

    property value: Currency read Fvalue write Fvalue;
    property currency: string read Fcurrency write Fcurrency;
  end;

  TACBrOpenDeliverySchemaRadius = class(TACBrOpenDeliverySchema)
  private
    Fsize: Integer;
    FestimateDeliveryTime: Integer;
    Fprice: TACBrOpenDeliverySchemaPrice;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const AObjectName: string = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;

    property size: Integer read Fsize write Fsize;
    property price: TACBrOpenDeliverySchemaPrice read Fprice write Fprice;
    property estimateDeliveryTime: Integer read FestimateDeliveryTime write FestimateDeliveryTime;
  end;

  TACBrOpenDeliverySchemaRadiusCollection = class(TACBrOpenDeliverySchemaArray)
  private
    function GetItem(Index: Integer): TACBrOpenDeliverySchemaRadius;
    procedure SetItem(Index: Integer; Value: TACBrOpenDeliverySchemaRadius);
  protected
    function NewSchema: TACBrOpenDeliverySchema; override;
  public
    function New: TACBrOpenDeliverySchemaRadius;
    property Items[Index: Integer]: TACBrOpenDeliverySchemaRadius read GetItem write SetItem; default;
  end;

  TACBrOpenDeliverySchemaService = class(TACBrOpenDeliverySchema)
  private
    Fid: string;
    Fstatus: TACBrODStatus;
    FserviceType: TACBrODServiceType;
    FmenuId: string;
    FserviceArea: TACBrOpenDeliverySchemaServiceArea;
    FserviceHours: TACBrOpenDeliverySchemaServiceHour;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const AObjectName: string = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;

    property id: string read Fid write Fid;
    property status: TACBrODStatus read Fstatus write Fstatus;
    property serviceType: TACBrODServiceType read FserviceType write FserviceType;
    property menuId: string read FmenuId write FmenuId;
    property serviceArea: TACBrOpenDeliverySchemaServiceArea read FserviceArea write FserviceArea;
    property serviceHours: TACBrOpenDeliverySchemaServiceHour read FserviceHours write FserviceHours;
  end;

  TACBrOpenDeliverySchemaServiceCollection = class(TACBrOpenDeliverySchemaArray)
  private
    function GetItem(Index: Integer): TACBrOpenDeliverySchemaService;
    procedure SetItem(Index: Integer; Value: TACBrOpenDeliverySchemaService);
  protected
    function NewSchema: TACBrOpenDeliverySchema; override;
  public
    function New: TACBrOpenDeliverySchemaService;
    property Items[Index: Integer]: TACBrOpenDeliverySchemaService read GetItem write SetItem; default;
  end;

  TACBrOpenDeliverySchemaServiceArea = class(TACBrOpenDeliverySchema)
  private
    Fid: string;
    Fpolygon: TACBrOpenDeliverySchemaPolygonCollection;
    FgeoRadius: TACBrOpenDeliverySchemaGeoRadius;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const AObjectName: string = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;

    property id: string read Fid write Fid;
    property polygon: TACBrOpenDeliverySchemaPolygonCollection read Fpolygon write Fpolygon;
    property geoRadius: TACBrOpenDeliverySchemaGeoRadius read FgeoRadius write FgeoRadius;
  end;

  TACBrOpenDeliverySchemaServiceHour = class(TACBrOpenDeliverySchema)
  private
    FweekHours: TACBrOpenDeliverySchemaHourCollection;
    FholidayHours: TACBrOpenDeliverySchemaHolidayHourCollection;
    Fid: string;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const AObjectName: string = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;

    property id: string read Fid write Fid;
    property weekHours: TACBrOpenDeliverySchemaHourCollection read FweekHours write FweekHours;
    property holidayHours: TACBrOpenDeliverySchemaHolidayHourCollection read FholidayHours write FholidayHours;
  end;

  TACBrOpenDeliverySchemaServiceHourCollection = class(TACBrOpenDeliverySchemaArray)
  private
    function GetItem(Index: Integer): TACBrOpenDeliverySchemaServiceHour;
    procedure SetItem(Index: Integer; Value: TACBrOpenDeliverySchemaServiceHour);
  protected
    function NewSchema: TACBrOpenDeliverySchema; override;
  public
    function New: TACBrOpenDeliverySchemaServiceHour;
    property Items[Index: Integer]: TACBrOpenDeliverySchemaServiceHour read GetItem write SetItem; default;
  end;

  TACBrOpenDeliverySchemaTimePeriod = class(TACBrOpenDeliverySchema)
  private
    FstartTime: TDateTime;
    FendTime: TDateTime;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;

    property startTime: TDateTime read FstartTime write FstartTime;
    property endTime: TDateTime read FendTime write FendTime;
  end;

  TACBrOpenDeliverySchemaHour = class(TACBrOpenDeliverySchema)
  private
    FdayOfWeek: TACBrODDayOfWeekArray;
    FtimePeriods: TACBrOpenDeliverySchemaTimePeriod;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const AObjectName: string = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;

    property dayOfWeek: TACBrODDayOfWeekArray read FdayOfWeek write FdayOfWeek;
    property timePeriods : TACBrOpenDeliverySchemaTimePeriod read FtimePeriods write FtimePeriods;
  end;

  TACBrOpenDeliverySchemaHourCollection = class(TACBrOpenDeliverySchemaArray)
  private
    function GetItem(Index: Integer): TACBrOpenDeliverySchemaHour;
    procedure SetItem(Index: Integer; Value: TACBrOpenDeliverySchemaHour);
  protected
    function NewSchema: TACBrOpenDeliverySchema; override;
  public
    function New: TACBrOpenDeliverySchemaHour;
    property Items[Index: Integer]: TACBrOpenDeliverySchemaHour read GetItem write SetItem; default;
  end;

implementation

{ TACBrOpenDeliverySchemaBasicInfo }

procedure TACBrOpenDeliverySchemaBasicInfo.Clear;
begin
  Fname := '';
  Fdocument := '';
  FcorporateName := '';
  Fdescription := '';
  FaverageTicket := 0;
  FaveragePreparationTime := 0;
  FcreatedAt := 0;
  SetLength(FmerchantCategories, 0);
  SetLength(FcontactEmails, 0);

  FminOrderValue.Clear;
  Faddress.Clear;
  FcontactPhones.Clear;
  FlogoImage.Clear;
  FbannerImage.Clear;
end;

constructor TACBrOpenDeliverySchemaBasicInfo.Create(const AObjectName: string);
begin
  inherited Create(AObjectName);
  FminOrderValue := TACBrOpenDeliverySchemaPrice.Create('minOrderValue');
  Faddress := TACBrOpenDeliverySchemaAddress.Create('address');
  FcontactPhones := TACBrOpenDeliverySchemaContactPhone.Create('contactPhones');
  FlogoImage := TACBrOpenDeliverySchemaImage.Create('logoImage');
  FbannerImage := TACBrOpenDeliverySchemaImage.Create('bannerImage');
end;

destructor TACBrOpenDeliverySchemaBasicInfo.Destroy;
begin
  FminOrderValue.Free;
  Faddress.Free;
  FcontactPhones.Free;
  FlogoImage.Free;
  FbannerImage.Free;
  inherited;
end;

procedure TACBrOpenDeliverySchemaBasicInfo.DoReadFromJSon(AJSon: TACBrJSONObject);
var
  LCategories: TSplitResult;
  I: Integer;
begin
  AJson
    .Value('name', Fname)
    .Value('document', Fdocument)
    .Value('corporateName', FcorporateName)
    .Value('description', Fdescription)
    .Value('averageTicket', FaverageTicket)
    .Value('averagePreparationTime', FaveragePreparationTime)
    .Value('merchantCategories', LCategories)
    .Value('contactEmails', FcontactEmails)
    .ValueISODate('createdAt', FcreatedAt);

  FmerchantType := StrToMerchantType(AJson.AsString['merchantType']);
  FminOrderValue.ReadFromJSon(AJSon);
  Faddress.ReadFromJSon(AJSon);
  FcontactPhones.ReadFromJSon(AJSon);
  FlogoImage.ReadFromJSon(AJSon);
  FbannerImage.ReadFromJSon(AJSon);

  SetLength(FmerchantCategories, Length(LCategories));
  for I := 0 to Pred(Length(LCategories)) do
    FmerchantCategories[I] := StrToMerchantCategories(LCategories[I]);
end;

procedure TACBrOpenDeliverySchemaBasicInfo.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJson
    .AddPair('name', Fname)
    .AddPair('document', Fdocument)
    .AddPair('corporateName', FcorporateName)
    .AddPair('description', Fdescription)
    .AddPair('averageTicket', FaverageTicket)
    .AddPair('averagePreparationTime', FaveragePreparationTime)
    .AddPairJSONObject('minOrderValue', FminOrderValue.AsJSON)
    .AddPair('merchantType', MerchantTypeToStr(FmerchantType))
    .AddPair('merchantCategories', MerchantCategoriesToArray(FmerchantCategories))
    .AddPairJSONObject('address', Faddress.AsJSON)
    .AddPair('contactEmails', FcontactEmails)
    .AddPairJSONObject('contactPhones', FcontactPhones.AsJSON)
    .AddPairJSONObject('logoImage', FlogoImage.AsJSON)
    .AddPairJSONObject('bannerImage', FbannerImage.AsJSON)
    .AddPairISODateTime('createdAt', FcreatedAt);

end;

function TACBrOpenDeliverySchemaBasicInfo.IsEmpty: Boolean;
begin
  Result :=
    (Fname = '') and
    (Fdocument = '') and
    (FcorporateName = '') and
    (Fdescription = '') and
    (FaverageTicket = 0) and
    (FaveragePreparationTime = 0) and
    (FcreatedAt = 0) and
    (Length(FmerchantCategories) = 0) and
    (Length(FcontactEmails) = 0) and
    (FminOrderValue.IsEmpty) and
    (Faddress.IsEmpty) and
    (FcontactPhones.IsEmpty) and
    (FlogoImage.IsEmpty) and
    (FbannerImage.IsEmpty);
end;

{ TACBrOpenDeliverySchemaAddress }

procedure TACBrOpenDeliverySchemaAddress.Clear;
begin
  Fdistrict := '';
  Fstreet := '';
  FpostalCode := '';
  Fstate := '';
  Fcomplement := '';
  Fnumber := '';
  Fcountry := '';
  Fcity := '';
  Freference := '';
  Flatitude := 0;
  Flongitude := 0;
end;

procedure TACBrOpenDeliverySchemaAddress.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  AJson
    .Value('country', Fcountry)
    .Value('state', Fstate)
    .Value('city', Fcity)
    .Value('district', Fdistrict)
    .Value('street', Fstreet)
    .Value('number', Fnumber)
    .Value('postalCode', FpostalCode)
    .Value('complement', Fcomplement)
    .Value('reference', Freference)
    .Value('latitude', Flatitude)
    .Value('longitude', Flongitude);
end;

procedure TACBrOpenDeliverySchemaAddress.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('country', Fcountry)
    .AddPair('state', Fstate)
    .AddPair('city', Fcity)
    .AddPair('district', Fdistrict)
    .AddPair('street', Fstreet)
    .AddPair('number', Fnumber)
    .AddPair('postalCode', FpostalCode)
    .AddPair('complement', Fcomplement)
    .AddPair('reference', Freference)
    .AddPair('latitude', Flatitude)
    .AddPair('longitude', Flongitude)
end;

function TACBrOpenDeliverySchemaAddress.IsEmpty: Boolean;
begin
  Result := (Fdistrict = '') and
            (Fstreet = '') and
            (FpostalCode = '') and
            (Fstate = '') and
            (Fcomplement = '') and
            (Fnumber = '') and
            (Fcountry = '') and
            (Fcity = '') and
            (Freference = '') and
            (Flatitude = 0) and
            (Flongitude = 0);
end;

{ TACBrOpenDeliverySchemaContactPhone }

procedure TACBrOpenDeliverySchemaContactPhone.Clear;
begin
  FwhatsappNumber := '';
  FcommercialNumber := '';
end;

procedure TACBrOpenDeliverySchemaContactPhone.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  AJson
    .Value('whatsappNumber', FwhatsappNumber)
    .Value('commercialNumber', FcommercialNumber);
end;

procedure TACBrOpenDeliverySchemaContactPhone.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('whatsappNumber', FwhatsappNumber)
    .AddPair('commercialNumber', FcommercialNumber);
end;

function TACBrOpenDeliverySchemaContactPhone.IsEmpty: Boolean;
begin
  Result := (FwhatsappNumber = '') and (FcommercialNumber = '');
end;

{ TACBrOpenDeliverySchemaPrice }

procedure TACBrOpenDeliverySchemaPrice.Clear;
begin
  Fvalue := 0;
  Fcurrency := '';
end;

constructor TACBrOpenDeliverySchemaPrice.Create(const AObjectName: string);
begin
  inherited Create(AObjectName);
  FCurrency := 'BRL';
end;

procedure TACBrOpenDeliverySchemaPrice.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  AJson
    .Value('value', Fvalue)
    .Value('currency', FCurrency);
end;

procedure TACBrOpenDeliverySchemaPrice.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJson
    .AddPair('value', Fvalue)
    .AddPair('currency', FCurrency);
end;

function TACBrOpenDeliverySchemaPrice.IsEmpty: Boolean;
begin
  Result := (Fvalue = 0) and (FCurrency = '');
end;

{ TACBrOpenDeliverySchemaImage }

procedure TACBrOpenDeliverySchemaImage.Clear;
begin
  FURL := '';
  FCRC_32 := '';
end;

procedure TACBrOpenDeliverySchemaImage.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  AJson
    .Value('URL', FURL)
    .Value('CRC-32', FCRC_32);
end;

procedure TACBrOpenDeliverySchemaImage.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJson
    .AddPair('URL', FURL)
    .AddPair('CRC-32', FCRC_32);
end;

function TACBrOpenDeliverySchemaImage.IsEmpty: Boolean;
begin
  Result := (FURL = '') and (FCRC_32 = '');
end;

{ TACBrOpenDeliverySchemaRadius }

procedure TACBrOpenDeliverySchemaRadius.Clear;
begin
  Fsize := 0;
  FestimateDeliveryTime := 0;
  Fprice.Clear;
end;

constructor TACBrOpenDeliverySchemaRadius.Create(const AObjectName: string);
begin
  inherited Create(AObjectName);
  Fprice := TACBrOpenDeliverySchemaPrice.Create('price');
end;

destructor TACBrOpenDeliverySchemaRadius.Destroy;
begin
  Fprice.Free;
  inherited;
end;

procedure TACBrOpenDeliverySchemaRadius.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  AJson
    .Value('size', Fsize)
    .Value('estimateDeliveryTime', FestimateDeliveryTime);

  Fprice.ReadFromJSon(AJSon);
end;

procedure TACBrOpenDeliverySchemaRadius.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJson
    .AddPair('size', Fsize)
    .AddPairJSONObject('price', Fprice.AsJSON)
    .AddPair('estimateDeliveryTime', FestimateDeliveryTime);
end;

function TACBrOpenDeliverySchemaRadius.IsEmpty: Boolean;
begin
  Result := (Fsize = 0) and (FestimateDeliveryTime = 0) and (Fprice.IsEmpty);
end;

{ TACBrOpenDeliverySchemaRadiusCollection }

function TACBrOpenDeliverySchemaRadiusCollection.GetItem(Index: Integer): TACBrOpenDeliverySchemaRadius;
begin
  Result := TACBrOpenDeliverySchemaRadius(inherited Items[Index]);
end;

function TACBrOpenDeliverySchemaRadiusCollection.New: TACBrOpenDeliverySchemaRadius;
begin
  Result := TACBrOpenDeliverySchemaRadius.Create;
  Self.Add(Result);
end;

function TACBrOpenDeliverySchemaRadiusCollection.NewSchema: TACBrOpenDeliverySchema;
begin
  Result := New;
end;

procedure TACBrOpenDeliverySchemaRadiusCollection.SetItem(Index: Integer; Value: TACBrOpenDeliverySchemaRadius);
begin
  inherited Items[Index] := Value;
end;

{ TACBrOpenDeliverySchemaGeoRadius }

procedure TACBrOpenDeliverySchemaGeoRadius.Clear;
begin
  inherited;
  FgeoMidpointLatitude := 0;
  FgeoMidpointLongitude := 0;
  Fradius.Clear;
end;

constructor TACBrOpenDeliverySchemaGeoRadius.Create(const AObjectName: string);
begin
  inherited Create(AObjectName);
  Fradius := TACBrOpenDeliverySchemaRadiusCollection.Create('radius');
end;

destructor TACBrOpenDeliverySchemaGeoRadius.Destroy;
begin
  Fradius.Free;
  inherited;
end;

procedure TACBrOpenDeliverySchemaGeoRadius.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  Fradius.Clear;
  AJSon
    .Value('geoMidpointLatitude', FgeoMidpointLatitude)
    .Value('geoMidpointLongitude', FgeoMidpointLongitude);

  Fradius.ReadFromJSon(AJSon);
end;

procedure TACBrOpenDeliverySchemaGeoRadius.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('geoMidpointLatitude', FgeoMidpointLatitude)
    .AddPair('geoMidpointLongitude', FgeoMidpointLongitude);
//    .AddPair('radius', Fradius.ToJSonArray);

  Fradius.WriteToJSon(AJson);
end;

function TACBrOpenDeliverySchemaGeoRadius.IsEmpty: Boolean;
begin
  Result := (FgeoMidpointLatitude = 0) and 
            (FgeoMidpointLongitude = 0) and
            (Fradius.Count = 0);
end;

{ TACBrOpenDeliverySchemaGeoCoordinate }

procedure TACBrOpenDeliverySchemaGeoCoordinate.Clear;
begin
  Flatitude := 0;
  Flongitude := 0;
end;

procedure TACBrOpenDeliverySchemaGeoCoordinate.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .Value('latitude', Flatitude)
    .Value('longitude', Flongitude);
end;

procedure TACBrOpenDeliverySchemaGeoCoordinate.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('latitude', Flatitude)
    .AddPair('longitude', Flongitude);
end;

function TACBrOpenDeliverySchemaGeoCoordinate.IsEmpty: Boolean;
begin
  Result := (Flatitude = 0) and (Flongitude = 0);
end;

{ TACBrOpenDeliverySchemaGeoCoordinateCollection }

function TACBrOpenDeliverySchemaGeoCoordinateCollection.GetItem(Index: Integer): TACBrOpenDeliverySchemaGeoCoordinate;
begin
  Result := TACBrOpenDeliverySchemaGeoCoordinate(inherited Items[Index]);
end;

function TACBrOpenDeliverySchemaGeoCoordinateCollection.New: TACBrOpenDeliverySchemaGeoCoordinate;
begin
  Result := TACBrOpenDeliverySchemaGeoCoordinate.Create;
  Self.Add(Result);
end;

function TACBrOpenDeliverySchemaGeoCoordinateCollection.NewSchema: TACBrOpenDeliverySchema;
begin
  Result := New;
end;

procedure TACBrOpenDeliverySchemaGeoCoordinateCollection.SetItem(Index: Integer; Value: TACBrOpenDeliverySchemaGeoCoordinate);
begin
  inherited Items[Index] := Value;
end;

{ TACBrOpenDeliverySchemaPolygon }

procedure TACBrOpenDeliverySchemaPolygon.Clear;
begin
  inherited;
  FestimateDeliveryTime := 0;
  Fprice.Clear;
  FgeoCoordinates.Clear;
end;

constructor TACBrOpenDeliverySchemaPolygon.Create(const AObjectName: string);
begin
  inherited Create(AObjectName);
  Fprice := TACBrOpenDeliverySchemaPrice.Create('price');
  FgeoCoordinates := TACBrOpenDeliverySchemaGeoCoordinateCollection.Create('geoCoordinates');
end;

destructor TACBrOpenDeliverySchemaPolygon.Destroy;
begin
  Fprice.Free;
  FgeoCoordinates.Free;
  inherited;
end;

procedure TACBrOpenDeliverySchemaPolygon.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  AJson
    .Value('estimateDeliveryTime', FestimateDeliveryTime);

  Fprice.ReadFromJSon(AJSon);
  FgeoCoordinates.ReadFromJSon(AJSon);
end;

procedure TACBrOpenDeliverySchemaPolygon.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('geoCoordinates', FgeoCoordinates.ToJSonArray)
    .AddPairJSONObject('price', Fprice.AsJSON)
    .AddPair('estimateDeliveryTime', FestimateDeliveryTime);
end;

function TACBrOpenDeliverySchemaPolygon.IsEmpty: Boolean;
begin
  Result := (FestimateDeliveryTime = 0) and
            (Fprice.IsEmpty) and
            (FgeoCoordinates.Count = 0);
end;

{ TACBrOpenDeliverySchemaPolygonCollection }

function TACBrOpenDeliverySchemaPolygonCollection.GetItem(Index: Integer): TACBrOpenDeliverySchemaPolygon;
begin
  Result := TACBrOpenDeliverySchemaPolygon(inherited Items[Index]);
end;

function TACBrOpenDeliverySchemaPolygonCollection.New: TACBrOpenDeliverySchemaPolygon;
begin
  Result := TACBrOpenDeliverySchemaPolygon.Create;
  Self.Add(Result);
end;

function TACBrOpenDeliverySchemaPolygonCollection.NewSchema: TACBrOpenDeliverySchema;
begin
  Result := New;
end;

procedure TACBrOpenDeliverySchemaPolygonCollection.SetItem(Index: Integer; Value: TACBrOpenDeliverySchemaPolygon);
begin
  inherited Items[Index] := Value;
end;

{ TACBrOpenDeliverySchemaServiceArea }

procedure TACBrOpenDeliverySchemaServiceArea.Clear;
begin
  Fid := '';
  Fpolygon.Clear;
  FgeoRadius.Clear;
end;

constructor TACBrOpenDeliverySchemaServiceArea.Create(const AObjectName: string);
begin
  inherited Create(AObjectName);
  Fpolygon := TACBrOpenDeliverySchemaPolygonCollection.Create('polygon');
  FgeoRadius := TACBrOpenDeliverySchemaGeoRadius.Create('geoRadius');
end;

destructor TACBrOpenDeliverySchemaServiceArea.Destroy;
begin
  Fpolygon.Free;
  FgeoRadius.Free;
  inherited;
end;

procedure TACBrOpenDeliverySchemaServiceArea.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  Fid := AJSon.AsString['id'];
  FgeoRadius.ReadFromJSon(AJSon);
  Fpolygon.ReadFromJSon(AJSon);
end;

procedure TACBrOpenDeliverySchemaServiceArea.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJson
    .AddPair('id', Fid)
    .AddPair('polygon', Fpolygon.ToJSonArray);
//    .AddPairJSONObject('geoRadius', FgeoRadius.AsJSON);

  FgeoRadius.WriteToJSon(AJSon);
end;

function TACBrOpenDeliverySchemaServiceArea.IsEmpty: Boolean;
begin
  Result :=
    (Fid = '') and
    (Fpolygon.Count = 0) and
    (FgeoRadius.IsEmpty);
end;

{ TACBrOpenDeliverySchemaTimePeriod }

procedure TACBrOpenDeliverySchemaTimePeriod.Clear;
begin
  FstartTime := 0;
  FendTime := 0;
end;

procedure TACBrOpenDeliverySchemaTimePeriod.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  AJson
    .ValueISOTime('startTime', FstartTime)
    .ValueISOTime('endTime', FendTime);
end;

procedure TACBrOpenDeliverySchemaTimePeriod.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJson
    .AddPair('startTime', FormatDateTime('hh:mm:ss', FstartTime))
    .AddPair('endTime', FormatDateTime('hh:mm:ss', FendTime))
end;

function TACBrOpenDeliverySchemaTimePeriod.IsEmpty: Boolean;
begin
  Result := (FstartTime = 0) and (FendTime = 0);
end;

{ TACBrOpenDeliverySchemaHour }

procedure TACBrOpenDeliverySchemaHour.Clear;
begin
  SetLength(FdayOfWeek, 0);
  FtimePeriods.Clear;
end;

constructor TACBrOpenDeliverySchemaHour.Create(const AObjectName: string);
begin
  inherited Create(AObjectName);
  FtimePeriods := TACBrOpenDeliverySchemaTimePeriod.Create('timePeriods');
end;

destructor TACBrOpenDeliverySchemaHour.Destroy;
begin
  FtimePeriods.Free;
  inherited;
end;

procedure TACBrOpenDeliverySchemaHour.DoReadFromJSon(AJSon: TACBrJSONObject);
var
  LSplitResult: TSplitResult;
  I: Integer;
begin
  FtimePeriods.ReadFromJSon(AJSon);
  AJSon.Value('dayOfWeek', LSplitResult);

  SetLength(FdayOfWeek, Length(LSplitResult));
  for I := 0 to Pred(Length(LSplitResult)) do
    FdayOfWeek[I] := StrToDayOfWeek(LSplitResult[I]);
end;

procedure TACBrOpenDeliverySchemaHour.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('dayOfWeek', DayOfWeekToArray(FdayOfWeek))
    .AddPairJSONObject('timePeriods', FtimePeriods.AsJSON);
end;

function TACBrOpenDeliverySchemaHour.IsEmpty: Boolean;
begin
  Result := (Length(FdayOfWeek) = 0) and (FtimePeriods.IsEmpty);
end;

{ TACBrOpenDeliverySchemaHolidayHour }

procedure TACBrOpenDeliverySchemaHolidayHour.Clear;
begin
  Fdate := 0;
  FtimePeriods.Clear;
end;

constructor TACBrOpenDeliverySchemaHolidayHour.Create(const AObjectName: string);
begin
  inherited Create(AObjectName);
  FtimePeriods := TACBrOpenDeliverySchemaTimePeriod.Create('timePeriods');
end;

destructor TACBrOpenDeliverySchemaHolidayHour.Destroy;
begin
  FtimePeriods.Free;
  inherited;
end;

procedure TACBrOpenDeliverySchemaHolidayHour.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  AJSon.ValueISODate('date', Fdate);
  FtimePeriods.ReadFromJSon(AJSon);
end;

procedure TACBrOpenDeliverySchemaHolidayHour.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPairISODateTime('date', Fdate)
    .AddPairJSONObject('timePeriods', FtimePeriods.AsJSON);
end;

function TACBrOpenDeliverySchemaHolidayHour.IsEmpty: Boolean;
begin
  Result := (Fdate = 0) and (FtimePeriods.IsEmpty);
end;

{ TACBrOpenDeliverySchemaHolidayHourCollection }

function TACBrOpenDeliverySchemaHolidayHourCollection.GetItem(Index: Integer): TACBrOpenDeliverySchemaHolidayHour;
begin
  Result := TACBrOpenDeliverySchemaHolidayHour(inherited Items[Index]);
end;

function TACBrOpenDeliverySchemaHolidayHourCollection.New: TACBrOpenDeliverySchemaHolidayHour;
begin
  Result := TACBrOpenDeliverySchemaHolidayHour.Create;
  Self.Add(Result);
end;

function TACBrOpenDeliverySchemaHolidayHourCollection.NewSchema: TACBrOpenDeliverySchema;
begin
  Result := New;
end;

procedure TACBrOpenDeliverySchemaHolidayHourCollection.SetItem(Index: Integer; Value: TACBrOpenDeliverySchemaHolidayHour);
begin
  inherited Items[Index] := Value;
end;

{ TACBrOpenDeliverySchemaHourCollection }

function TACBrOpenDeliverySchemaHourCollection.GetItem(Index: Integer): TACBrOpenDeliverySchemaHour;
begin
  Result := TACBrOpenDeliverySchemaHour(inherited Items[Index]);
end;

function TACBrOpenDeliverySchemaHourCollection.New: TACBrOpenDeliverySchemaHour;
begin
  Result := TACBrOpenDeliverySchemaHour.Create;
  Self.Add(Result);
end;

function TACBrOpenDeliverySchemaHourCollection.NewSchema: TACBrOpenDeliverySchema;
begin
  Result := New;
end;

procedure TACBrOpenDeliverySchemaHourCollection.SetItem(Index: Integer; Value: TACBrOpenDeliverySchemaHour);
begin
  inherited Items[Index] := Value;
end;

{ TACBrOpenDeliverySchemaServiceHour }

procedure TACBrOpenDeliverySchemaServiceHour.Clear;
begin
  Fid := '';
  FweekHours.Clear;
  FholidayHours.Clear;
end;

constructor TACBrOpenDeliverySchemaServiceHour.Create(const AObjectName: string);
begin
  inherited Create(AObjectName);
  FweekHours := TACBrOpenDeliverySchemaHourCollection.Create('weekHours');
  FholidayHours := TACBrOpenDeliverySchemaHolidayHourCollection.Create('holidayHours');
end;

destructor TACBrOpenDeliverySchemaServiceHour.Destroy;
begin
  FweekHours.Free;
  FholidayHours.Free;
  inherited;
end;

procedure TACBrOpenDeliverySchemaServiceHour.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  AJSon.Value('id', Fid);
  FweekHours.ReadFromJSon(AJSon);
  FholidayHours.ReadFromJSon(AJSon);
end;

procedure TACBrOpenDeliverySchemaServiceHour.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJson
    .AddPair('id', Fid)
    .AddPair('weekHours', FweekHours.ToJSonArray)
    .AddPair('holidayHours', FholidayHours.ToJSonArray);
end;

function TACBrOpenDeliverySchemaServiceHour.IsEmpty: Boolean;
begin
  Result := (Fid = '') and
            (FholidayHours.Count = 0) and
            (FweekHours.Count = 0);
end;

{ TACBrOpenDeliverySchemaService }

procedure TACBrOpenDeliverySchemaService.Clear;
begin
  Fid := '';
  Fstatus := sAvailable;
  FserviceType := stDelivery;
  FmenuId := '';
  FserviceArea.Clear;
  FserviceHours.Clear;
end;

constructor TACBrOpenDeliverySchemaService.Create(const AObjectName: string);
begin
  inherited Create(AObjectName);
  FserviceArea := TACBrOpenDeliverySchemaServiceArea.Create('serviceArea');
  FserviceHours := TACBrOpenDeliverySchemaServiceHour.Create('serviceHours');
end;

destructor TACBrOpenDeliverySchemaService.Destroy;
begin
  FserviceArea.Free;
  FserviceHours.Free;
  inherited;
end;

procedure TACBrOpenDeliverySchemaService.DoReadFromJSon(AJSon: TACBrJSONObject);
var
  LStrEnum: string;
begin
  AJson
    .Value('id', Fid)
    .Value('menuId', FmenuId);

  LStrEnum := AJSon.AsString['status'];
  Fstatus := StrToStatus(LStrEnum);

  LStrEnum := AJSon.AsString['serviceType'];
  FserviceType := StrToServiceType(LStrEnum);

  FserviceArea.ReadFromJSon(AJSon);
  FserviceHours.ReadFromJSon(AJSon);
end;

procedure TACBrOpenDeliverySchemaService.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('id', Fid)
    .AddPair('status', StatusToStr(Fstatus))
    .AddPair('serviceType', ServiceTypeToStr(FserviceType))
    .AddPair('menuId', FmenuId)
    .AddPairJSONObject('serviceArea', FserviceArea.AsJSON)
    .AddPairJSONObject('serviceHours', FserviceHours.AsJSON);
end;

function TACBrOpenDeliverySchemaService.IsEmpty: Boolean;
begin
  Result := (Fid = '') and
            (Fstatus = sAvailable) and
            (FserviceType = stDelivery) and
            (FmenuId = '') and
            (FserviceArea.IsEmpty) and
            (FserviceHours.IsEmpty);
end;

{ TACBrOpenDeliverySchemaServiceCollection }

function TACBrOpenDeliverySchemaServiceCollection.GetItem(Index: Integer): TACBrOpenDeliverySchemaService;
begin
  Result := TACBrOpenDeliverySchemaService(inherited Items[Index]);
end;

function TACBrOpenDeliverySchemaServiceCollection.New: TACBrOpenDeliverySchemaService;
begin
  Result := TACBrOpenDeliverySchemaService.Create;
  Self.Add(Result);
end;

function TACBrOpenDeliverySchemaServiceCollection.NewSchema: TACBrOpenDeliverySchema;
begin
  Result := New;
end;

procedure TACBrOpenDeliverySchemaServiceCollection.SetItem(Index: Integer; Value: TACBrOpenDeliverySchemaService);
begin
  inherited Items[Index] := Value;
end;

{ TACBrOpenDeliverySchemaMenu }

procedure TACBrOpenDeliverySchemaMenu.Clear;
begin
  Fid := '';
  Fname := '';
  Fdescription := '';
  FexternalCode := '';
  Fdisclaimer := '';
  FdisclaimerUrl := '';
  SetLength(FcategoryId, 0);
end;

procedure TACBrOpenDeliverySchemaMenu.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .Value('id', Fid)
    .Value('name', Fname)
    .Value('description', Fdescription)
    .Value('externalCode', FexternalCode)
    .Value('disclaimer', Fdisclaimer)
    .Value('disclaimerURL', FdisclaimerUrl)
    .Value('categoryId', FcategoryId);
end;

procedure TACBrOpenDeliverySchemaMenu.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('id', Fid)
    .AddPair('name', Fname)
    .AddPair('description', Fdescription)
    .AddPair('externalCode', FexternalCode)
    .AddPair('disclaimer', Fdisclaimer)
    .AddPair('disclaimerURL', FdisclaimerUrl)
    .AddPair('categoryId', FcategoryId);
end;

function TACBrOpenDeliverySchemaMenu.IsEmpty: Boolean;
begin
  Result := (Fid = '') and
            (Fname = '') and
            (Fdescription = '') and
            (FexternalCode = '') and
            (Fdisclaimer = '') and
            (FdisclaimerUrl = '') and
            (Length(FcategoryId) = 0);
end;

{ TACBrOpenDeliverySchemaMenuCollection }

function TACBrOpenDeliverySchemaMenuCollection.GetItem(Index: Integer): TACBrOpenDeliverySchemaMenu;
begin
  Result := TACBrOpenDeliverySchemaMenu(inherited Items[Index]);
end;

function TACBrOpenDeliverySchemaMenuCollection.New: TACBrOpenDeliverySchemaMenu;
begin
  Result := TACBrOpenDeliverySchemaMenu.Create;
  Self.Add(Result);
end;

function TACBrOpenDeliverySchemaMenuCollection.NewSchema: TACBrOpenDeliverySchema;
begin
  Result := New;
end;

procedure TACBrOpenDeliverySchemaMenuCollection.SetItem(Index: Integer; Value: TACBrOpenDeliverySchemaMenu);
begin
  inherited Items[Index] := Value;
end;

{ TACBrOpenDeliverySchemaCategory }

procedure TACBrOpenDeliverySchemaCategory.Clear;
begin
  FId := '';
  Findex := 0;
  Fname := '';
  Fdescription := '';
  FexternalCode := '';
  Fstatus := sAvailable;
  SetLength(FavailabilityId, 0);
  SetLength(FitemOfferId, 0);
  Fimage.Clear;
end;

constructor TACBrOpenDeliverySchemaCategory.Create(const AObjectName: string);
begin
  inherited Create(AObjectName);
  Fimage := TACBrOpenDeliverySchemaImage.Create('image');
end;

destructor TACBrOpenDeliverySchemaCategory.Destroy;
begin
  Fimage.Free;
  inherited;
end;

procedure TACBrOpenDeliverySchemaCategory.DoReadFromJSon(AJSon: TACBrJSONObject);
var
  LStrStatus: string;
begin
  AJSon
    .Value('id', Fid)
    .Value('index', Findex)
    .Value('name', Fname)
    .Value('description', Fdescription)
    .Value('externalCode', FexternalCode)
    .Value('status', LStrStatus)
    .Value('availabilityId', FavailabilityId)
    .Value('itemOfferId', FitemOfferId);

  Fimage.ReadFromJSon(AJSon);
  Fstatus := StrToStatus(LStrStatus);
end;

procedure TACBrOpenDeliverySchemaCategory.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJson
    .AddPair('id', Fid)
    .AddPair('index', Findex)
    .AddPair('name', Fname)
    .AddPair('description', Fdescription)
    .AddPairJSONObject('image', Fimage.AsJSON)
    .AddPair('externalCode', FexternalCode)
    .AddPair('status', StatusToStr(Fstatus))
    .AddPair('availabilityId', FavailabilityId)
    .AddPair('itemOfferId', FitemOfferId);
end;

function TACBrOpenDeliverySchemaCategory.IsEmpty: Boolean;
begin
  Result := (FId = '') and
            (Findex = 0) and
            (Fname = '') and
            (Fdescription = '') and
            (FexternalCode = '') and
            (Fstatus = sAvailable) and
            (Length(FavailabilityId) = 0) and
            (Length(FitemOfferId) = 0) and
            (Fimage.IsEmpty);
end;

{ TACBrOpenDeliverySchemaCategoryCollection }

function TACBrOpenDeliverySchemaCategoryCollection.GetItem(Index: Integer): TACBrOpenDeliverySchemaCategory;
begin
  Result := TACBrOpenDeliverySchemaCategory(inherited Items[Index]);
end;

function TACBrOpenDeliverySchemaCategoryCollection.New: TACBrOpenDeliverySchemaCategory;
begin
  Result := TACBrOpenDeliverySchemaCategory.Create;
  Self.Add(Result);
end;

function TACBrOpenDeliverySchemaCategoryCollection.NewSchema: TACBrOpenDeliverySchema;
begin
  Result := New;
end;

procedure TACBrOpenDeliverySchemaCategoryCollection.SetItem(Index: Integer; Value: TACBrOpenDeliverySchemaCategory);
begin
  inherited Items[Index] := Value;
end;

{ TACBrOpenDeliverySchemaItemOffer }

procedure TACBrOpenDeliverySchemaItemOffer.Clear;
begin
  Fid := '';
  FitemId := '';
  Findex := 0;
  SetLength(FavailabilityId, 0);
  SetLength(FoptionGroupsId, 0);
  Fprice.Clear;
end;

constructor TACBrOpenDeliverySchemaItemOffer.Create(const AObjectName: string);
begin
  inherited Create(AObjectName);
  Fprice := TACBrOpenDeliverySchemaPrice.Create('price');
end;

destructor TACBrOpenDeliverySchemaItemOffer.Destroy;
begin
  Fprice.Free;
  inherited;
end;

procedure TACBrOpenDeliverySchemaItemOffer.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .Value('id', Fid)
    .Value('itemId', FitemId)
    .Value('index', Findex)
    .Value('availabilityId', FavailabilityId)
    .Value('optionGroupsId', FoptionGroupsId);

  Fprice.ReadFromJSon(AJSon);
end;

procedure TACBrOpenDeliverySchemaItemOffer.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('id', Fid)
    .AddPair('itemId', FitemId)
    .AddPair('index', Findex)
    .AddPairJSONObject('price', Fprice.AsJSON)
    .AddPair('availabilityId', FavailabilityId)
    .AddPair('optionGroupsId', FoptionGroupsId);
end;

function TACBrOpenDeliverySchemaItemOffer.IsEmpty: Boolean;
begin
  Result := (Fid = '') and
            (FitemId = '') and
            (Findex = 0) and
            (Length(FavailabilityId) = 0) and
            (Length(FoptionGroupsId) = 0) and
            (Fprice.IsEmpty);
end;

{ TACBrOpenDeliverySchemaItemOfferCollection }

function TACBrOpenDeliverySchemaItemOfferCollection.GetItem(Index: Integer): TACBrOpenDeliverySchemaItemOffer;
begin
  Result := TACBrOpenDeliverySchemaItemOffer(inherited Items[Index]);
end;

function TACBrOpenDeliverySchemaItemOfferCollection.New: TACBrOpenDeliverySchemaItemOffer;
begin
  Result := TACBrOpenDeliverySchemaItemOffer.Create;
  Self.Add(Result);
end;

function TACBrOpenDeliverySchemaItemOfferCollection.NewSchema: TACBrOpenDeliverySchema;
begin
  Result := New;
end;

procedure TACBrOpenDeliverySchemaItemOfferCollection.SetItem(Index: Integer; Value: TACBrOpenDeliverySchemaItemOffer);
begin
  inherited Items[Index] := Value;
end;

{ TACBrOpenDeliverySchemaNutritionalInfo }

procedure TACBrOpenDeliverySchemaNutritionalInfo.Clear;
begin
  Fdescription := '';
  Fcalories := '';
  SetLength(Fallergen, 0);
  SetLength(Fadditives, 0);
  SetLength(FsuitableDiet, 0);
  FisAlcoholic := False;
end;

procedure TACBrOpenDeliverySchemaNutritionalInfo.DoReadFromJSon(AJSon: TACBrJSONObject);
var
  LStrAllergen: TSplitResult;
  LStrSuitableDiet: TSplitResult;
  I: Integer;
begin
  AJSon
    .Value('description', Fdescription)
    .Value('calories', Fcalories)
    .Value('allergen', LStrAllergen)
    .Value('additives', Fadditives)
    .Value('suitableDiet', LStrSuitableDiet)
    .Value('isAlcoholic', FisAlcoholic);


  SetLength(Fallergen, Length(LStrAllergen));
  for I := 0 to Pred(Length(LStrAllergen)) do
    Fallergen[I] := StrToAllergen(LStrAllergen[I]);

  SetLength(FsuitableDiet, Length(LStrSuitableDiet));
  for I := 0 to Pred(Length(LStrSuitableDiet)) do
    FsuitableDiet[I] := StrToSuitableDiet(LStrSuitableDiet[I]);
end;

procedure TACBrOpenDeliverySchemaNutritionalInfo.DoWriteToJSon(AJSon: TACBrJSONObject);
var
  LStrAllergen: TSplitResult;
  LStrSuitableDiet: TSplitResult;
begin
  LStrAllergen := AllergensToArray(Fallergen);
  LStrSuitableDiet := SuitableDietToArray(FsuitableDiet);

  AJSon
    .AddPair('description', Fdescription)
    .AddPair('calories', Fcalories)
    .AddPair('allergen', LStrAllergen)
    .AddPair('additives', Fadditives)
    .AddPair('suitableDiet', LStrSuitableDiet)
    .AddPair('isAlcoholic', FisAlcoholic);
end;

function TACBrOpenDeliverySchemaNutritionalInfo.IsEmpty: Boolean;
begin
  Result := (Fdescription = '') and
            (Fcalories = '') and
            (Length(Fallergen) = 0) and
            (Length(Fadditives) = 0) and
            (Length(FsuitableDiet) = 0) and
            (not (FisAlcoholic));
end;

{ TACBrOpenDeliverySchemaItem }

procedure TACBrOpenDeliverySchemaItem.Clear;
begin
  Fid := '';
  Fname := '';
  Fdescription := '';
  FexternalCode := '';
  Fserving := 0;
  Funit := '';
  Fean := '';
  Fimage.Clear;
  FnutritionalInfo.Clear;
end;

constructor TACBrOpenDeliverySchemaItem.Create(const AObjectName: string);
begin
  inherited Create(AObjectName);
  Fimage := TACBrOpenDeliverySchemaImage.Create('image');
  FnutritionalInfo := TACBrOpenDeliverySchemaNutritionalInfo.Create('nutritionalInfo');
end;

destructor TACBrOpenDeliverySchemaItem.Destroy;
begin
  Fimage.Free;
  FnutritionalInfo.Free;
  inherited;
end;

procedure TACBrOpenDeliverySchemaItem.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .Value('id', Fid)
    .Value('name', Fname)
    .Value('description', Fdescription)
    .Value('externalCode', FexternalCode)
    .Value('serving', Fserving)
    .Value('unit', Funit)
    .Value('ean', Fean);

  Fimage.ReadFromJSon(AJSon);
  FnutritionalInfo.ReadFromJSon(AJSon);
end;

procedure TACBrOpenDeliverySchemaItem.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('id', Fid)
    .AddPair('name', Fname)
    .AddPair('description', Fdescription)
    .AddPair('externalCode', FexternalCode)
    .AddPairJSONObject('image', Fimage.AsJSON)
    .AddPairJSONObject('nutritionalInfo', FnutritionalInfo.AsJSON)
    .AddPair('serving', Fserving)
    .AddPair('unit', Funit)
    .AddPair('ean', Fean);
end;

function TACBrOpenDeliverySchemaItem.IsEmpty: Boolean;
begin
  Result := (Fid = '') and
            (Fname = '') and
            (Fdescription = '') and
            (FexternalCode = '') and
            (Fserving = 0) and
            (Funit = '') and
            (Fean = '') and
            (Fimage.IsEmpty) and
            (FnutritionalInfo.IsEmpty);
end;

{ TACBrOpenDeliverySchemaItemCollection }

function TACBrOpenDeliverySchemaItemCollection.GetItem(Index: Integer): TACBrOpenDeliverySchemaItem;
begin
  Result := TACBrOpenDeliverySchemaItem(inherited Items[Index]);
end;

function TACBrOpenDeliverySchemaItemCollection.New: TACBrOpenDeliverySchemaItem;
begin
  Result := TACBrOpenDeliverySchemaItem.Create;
  Self.Add(Result);
end;

function TACBrOpenDeliverySchemaItemCollection.NewSchema: TACBrOpenDeliverySchema;
begin
  Result := New;
end;

procedure TACBrOpenDeliverySchemaItemCollection.SetItem(Index: Integer; Value: TACBrOpenDeliverySchemaItem);
begin
  inherited Items[Index] := Value;
end;

{ TACBrOpenDeliverySchemaOption }

procedure TACBrOpenDeliverySchemaOption.Clear;
begin
  Fid := '';
  FitemId := '';
  Findex := 0;
  FmaxPermitted := 0;
  Fprice.Clear;
end;

constructor TACBrOpenDeliverySchemaOption.Create(const AObjectName: string);
begin
  inherited Create(AObjectName);
  Fprice := TACBrOpenDeliverySchemaPrice.Create('price');
end;

destructor TACBrOpenDeliverySchemaOption.Destroy;
begin
  Fprice.Free;
  inherited;
end;

procedure TACBrOpenDeliverySchemaOption.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .Value('id', Fid)
    .Value('itemId', FitemId)
    .Value('index', Findex)
    .Value('maxPermitted', FmaxPermitted);

  Fprice.ReadFromJSon(AJSon);
end;

procedure TACBrOpenDeliverySchemaOption.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('id', Fid)
    .AddPair('itemId', FitemId)
    .AddPair('index', Findex)
    .AddPairJSONObject('price', Fprice.AsJSON)
    .AddPair('maxPermitted', FmaxPermitted);
end;

function TACBrOpenDeliverySchemaOption.IsEmpty: Boolean;
begin
  Result := (Fid = '') and
            (FitemId = '') and
            (Findex = 0) and
            (Fprice.IsEmpty) and
            (FmaxPermitted = 0);
end;

{ TACBrOpenDeliverySchemaOptionCollection }

function TACBrOpenDeliverySchemaOptionCollection.GetItem(Index: Integer): TACBrOpenDeliverySchemaOption;
begin
  Result := TACBrOpenDeliverySchemaOption(inherited Items[Index]);
end;

function TACBrOpenDeliverySchemaOptionCollection.New: TACBrOpenDeliverySchemaOption;
begin
  Result := TACBrOpenDeliverySchemaOption.Create;
  Self.Add(Result);
end;

function TACBrOpenDeliverySchemaOptionCollection.NewSchema: TACBrOpenDeliverySchema;
begin
  Result := New;
end;

procedure TACBrOpenDeliverySchemaOptionCollection.SetItem(Index: Integer; Value: TACBrOpenDeliverySchemaOption);
begin
  inherited Items[Index] := Value;
end;

{ TACBrOpenDeliverySchemaOptionGroup }

procedure TACBrOpenDeliverySchemaOptionGroup.Clear;
begin
  Fid := '';
  Findex := 0;
  Fname := '';
  Fdescription := '';
  FexternalCode := '';
  Fstatus := sAvailable;
  FminPermitted := 0;
  FmaxPermitted := 0;
  Foptions.Clear;
end;

constructor TACBrOpenDeliverySchemaOptionGroup.Create(const AObjectName: string);
begin
  inherited;
  Foptions := TACBrOpenDeliverySchemaOptionCollection.Create('options');
end;

destructor TACBrOpenDeliverySchemaOptionGroup.Destroy;
begin
  Foptions.Free;
  inherited;
end;

procedure TACBrOpenDeliverySchemaOptionGroup.DoReadFromJSon(AJSon: TACBrJSONObject);
var
  LStrStatus: string;
begin
  Foptions.Clear;
  AJSon
    .Value('id', Fid)
    .Value('index', Findex)
    .Value('name', Fname)
    .Value('description', Fdescription)
    .Value('externalCode', FexternalCode)
    .Value('status', LStrStatus)
    .Value('minPermitted', FminPermitted)
    .Value('maxPermitted', FmaxPermitted);

  Fstatus := StrToStatus(LStrStatus);
  Foptions.ReadFromJSon(AJSon);
end;

procedure TACBrOpenDeliverySchemaOptionGroup.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('id', Fid)
    .AddPair('index', Findex)
    .AddPair('name', Fname)
    .AddPair('description', Fdescription)
    .AddPair('externalCode', FexternalCode)
    .AddPair('status', StatusToStr(FStatus))
    .AddPair('minPermitted', FminPermitted)
    .AddPair('maxPermitted', FmaxPermitted)
    .AddPair('options', Foptions.ToJSonArray);
end;

function TACBrOpenDeliverySchemaOptionGroup.IsEmpty: Boolean;
begin
  Result := (Fid = '') and
            (Findex = 0) and
            (Fname = '') and
            (Fdescription = '') and
            (FexternalCode = '') and
            (Fstatus = sAvailable) and
            (FminPermitted = 0) and
            (FmaxPermitted = 0) and
            (Foptions.Count = 0);
end;

{ TACBrOpenDeliverySchemaOptionGroupCollection }

function TACBrOpenDeliverySchemaOptionGroupCollection.GetItem(Index: Integer): TACBrOpenDeliverySchemaOptionGroup;
begin
  Result := TACBrOpenDeliverySchemaOptionGroup(inherited Items[Index]);
end;

function TACBrOpenDeliverySchemaOptionGroupCollection.New: TACBrOpenDeliverySchemaOptionGroup;
begin
  Result := TACBrOpenDeliverySchemaOptionGroup.Create;
  Self.Add(Result);
end;

function TACBrOpenDeliverySchemaOptionGroupCollection.NewSchema: TACBrOpenDeliverySchema;
begin
  Result := New;
end;

procedure TACBrOpenDeliverySchemaOptionGroupCollection.SetItem(Index: Integer; Value: TACBrOpenDeliverySchemaOptionGroup);
begin
  inherited Items[Index] := Value;
end;

{ TACBrOpenDeliverySchemaServiceHourCollection }

function TACBrOpenDeliverySchemaServiceHourCollection.GetItem(Index: Integer): TACBrOpenDeliverySchemaServiceHour;
begin
  Result := TACBrOpenDeliverySchemaServiceHour(inherited Items[Index]);
end;

function TACBrOpenDeliverySchemaServiceHourCollection.New: TACBrOpenDeliverySchemaServiceHour;
begin
  Result := TACBrOpenDeliverySchemaServiceHour.Create;
  Self.Add(Result);
end;

function TACBrOpenDeliverySchemaServiceHourCollection.NewSchema: TACBrOpenDeliverySchema;
begin
  Result := New;
end;

procedure TACBrOpenDeliverySchemaServiceHourCollection.SetItem(Index: Integer; Value: TACBrOpenDeliverySchemaServiceHour);
begin
  inherited Items[Index] := Value;
end;

{ TACBrOpenDeliverySchemaAvailability }

procedure TACBrOpenDeliverySchemaAvailability.Clear;
begin
  Fid := '';
  FstartDate := 0;
  FendDate := 0;
  Fhours.Clear;
end;

constructor TACBrOpenDeliverySchemaAvailability.Create(const AObjectName: string);
begin
  inherited Create(AObjectName);
  Fhours := TACBrOpenDeliverySchemaHourCollection.Create('hours');
end;

destructor TACBrOpenDeliverySchemaAvailability.Destroy;
begin
  Fhours.Free;
  inherited;
end;

procedure TACBrOpenDeliverySchemaAvailability.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .Value('id', Fid)
    .ValueISODate('startDate', FstartDate)
    .ValueISODate('endDate', FendDate);

  Fhours.ReadFromJSon(AJSon);
end;

procedure TACBrOpenDeliverySchemaAvailability.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('id', Fid)
    .AddPairISODateTime('startDate', FstartDate)
    .AddPairISODateTime('endDate', FendDate)
    .AddPair('hours', Fhours.ToJSonArray);
end;

function TACBrOpenDeliverySchemaAvailability.IsEmpty: Boolean;
begin
  Result := (Fid = '') and
            (FstartDate = 0) and
            (FendDate = 0) and
            (Fhours.Count = 0);
end;

{ TACBrOpenDeliverySchemaAvailabilityCollection }

function TACBrOpenDeliverySchemaAvailabilityCollection.GetItem(Index: Integer): TACBrOpenDeliverySchemaAvailability;
begin
  Result := TACBrOpenDeliverySchemaAvailability(inherited Items[Index]);
end;

function TACBrOpenDeliverySchemaAvailabilityCollection.New: TACBrOpenDeliverySchemaAvailability;
begin
  Result := TACBrOpenDeliverySchemaAvailability.Create;
  Self.Add(Result);
end;

function TACBrOpenDeliverySchemaAvailabilityCollection.NewSchema: TACBrOpenDeliverySchema;
begin
  Result := New;
end;

procedure TACBrOpenDeliverySchemaAvailabilityCollection.SetItem(Index: Integer; Value: TACBrOpenDeliverySchemaAvailability);
begin
  inherited Items[Index];
end;

{ TACBrOpenDeliverySchemaMerchant }

procedure TACBrOpenDeliverySchemaMerchant.Clear;
begin
  FlastUpdate := 0;
  FTTL := 0;
  Fid := '';
  Fstatus := sAvailable;
  FbasicInfo.Clear;
  Fservices.Clear;
  Fmenus.Clear;
  Fitems.Clear;
  Fcategories.Clear;
  FitemOffers.Clear;
  FoptionGroups.Clear;
  Favailabilities.Clear;
end;

constructor TACBrOpenDeliverySchemaMerchant.Create(const AObjectName: string);
begin
  inherited Create(AObjectName);
  FbasicInfo := TACBrOpenDeliverySchemaBasicInfo.Create('basicInfo');
  Fservices := TACBrOpenDeliverySchemaServiceCollection.Create('services');
  Fmenus := TACBrOpenDeliverySchemaMenuCollection.Create('menus');
  Fitems := TACBrOpenDeliverySchemaItemCollection.Create('items');
  Fcategories := TACBrOpenDeliverySchemaCategoryCollection.Create('categories');
  FitemOffers := TACBrOpenDeliverySchemaItemOfferCollection.Create('itemOffers');
  FoptionGroups := TACBrOpenDeliverySchemaOptionGroupCollection.Create('optionGroups');
  Favailabilities := TACBrOpenDeliverySchemaAvailabilityCollection.Create('availabilities');
end;

destructor TACBrOpenDeliverySchemaMerchant.Destroy;
begin
  FbasicInfo.Free;
  Fservices.Free;
  Fmenus.Free;
  Fitems.Free;
  Fcategories.Free;
  FitemOffers.Free;
  FoptionGroups.Free;
  Favailabilities.Free;
  inherited;
end;

procedure TACBrOpenDeliverySchemaMerchant.DoReadFromJSon(AJSon: TACBrJSONObject);
var
  LStrStatus: string;
begin
  AJSon
    .ValueISODateTime('lastUpdate', FlastUpdate)
    .Value('TTL', FTTL)
    .Value('id', Fid)
    .Value('status', LStrStatus);

  Fstatus := StrToStatus(LStrStatus);
  FbasicInfo.ReadFromJSon(AJSon);
  Fservices.ReadFromJSon(AJSon);
  Fmenus.ReadFromJSon(AJSon);
  Fcategories.ReadFromJSon(AJSon);
  FitemOffers.ReadFromJSon(AJSon);
  FoptionGroups.ReadFromJSon(AJSon);
  Favailabilities.ReadFromJSon(AJSon);
end;

procedure TACBrOpenDeliverySchemaMerchant.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPairISODateTime('lastUpdate', FlastUpdate)
    .AddPair('TTL', FTTL)
    .AddPair('id', Fid)
    .AddPair('status', StatusToStr(Fstatus))
    .AddPairJSONObject('basicInfo', FbasicInfo.AsJSON)
    .AddPair('services', Fservices.ToJSonArray)
    .AddPair('menus', Fmenus.ToJSonArray)
    .AddPair('items', Fitems.ToJSonArray)
    .AddPair('categories', Fcategories.ToJSonArray)
    .AddPair('itemOffers', FitemOffers.ToJSonArray)
    .AddPair('optionGroups', FoptionGroups.ToJSonArray)
    .AddPair('availabilities', Favailabilities.ToJSonArray);
end;

function TACBrOpenDeliverySchemaMerchant.IsEmpty: Boolean;
begin
  Result := (FlastUpdate = 0) and
            (FTTL = 0) and
            (Fid = '') and
            (Fstatus = sAvailable) and
            (FbasicInfo.IsEmpty) and
            (Fservices.Count = 0) and
            (Fmenus.Count = 0) and
            (Fitems.Count = 0) and
            (Fcategories.Count = 0) and
            (FitemOffers.Count = 0) and
            (FoptionGroups.Count = 0) and
            (Favailabilities.Count = 0);
end;

{ TACBrOpenDeliverySchemaError }

procedure TACBrOpenDeliverySchemaError.Clear;
begin
  FStatus := 0;
  FTitle := '';
end;

procedure TACBrOpenDeliverySchemaError.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .Value('status', FStatus)
    .Value('title', FTitle);
end;

procedure TACBrOpenDeliverySchemaError.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('status', FStatus)
    .AddPair('title', FTitle);
end;

function TACBrOpenDeliverySchemaError.IsEmpty: Boolean;
begin
  Result := (Fstatus = 0) and (FTitle = '');
end;

{ TACBrOpenDeliverySchemaEvent }

procedure TACBrOpenDeliverySchemaEvent.Clear;
begin
  FEventId := '';
  FEventType := etCreated;
  FOrderId := '';
  FOrderURL := '';
  FCreatedAt := 0;
  FSourceAppId := '';
end;

procedure TACBrOpenDeliverySchemaEvent.DoReadFromJSon(AJSon: TACBrJSONObject);
var
  LStrEventType: string;
begin
  AJSon
    .Value('eventId',  FEventId)
    .Value('eventType', LStrEventType)
    .Value('orderId', FOrderId)
    .Value('orderURL', FOrderURL)
    .ValueISODateTime('createdAt', FCreatedAt)
    .Value('sourceAppId', FSourceAppId);

  FEventType := StrToEventType(LStrEventType);
end;

procedure TACBrOpenDeliverySchemaEvent.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('eventId', FEventId)
    .AddPair('eventType', EventTypeToStr(FEventType))
    .AddPair('orderId', FOrderId)
    .AddPair('orderURL', FOrderURL)
    .AddPairISODateTime('createdAt', FCreatedAt)
    .AddPair('sourceAppId', FSourceAppId);
end;

function TACBrOpenDeliverySchemaEvent.IsEmpty: Boolean;
begin
  Result := (FEventId = '') and
            (FEventType = etCreated) and
            (FOrderId = '') and
            (FOrderURL = '') and
            (FCreatedAt = 0) and
            (FSourceAppId = '');
end;

{ TACBrOpenDeliverySchemaEventCollection }

function TACBrOpenDeliverySchemaEventCollection.GetItem(Index: Integer): TACBrOpenDeliverySchemaEvent;
begin
  Result := TACBrOpenDeliverySchemaEvent(inherited Items[Index]);
end;

function TACBrOpenDeliverySchemaEventCollection.New: TACBrOpenDeliverySchemaEvent;
begin
  Result := TACBrOpenDeliverySchemaEvent.Create;
  Self.Add(Result);
end;

function TACBrOpenDeliverySchemaEventCollection.NewSchema: TACBrOpenDeliverySchema;
begin
  Result := New;
end;

procedure TACBrOpenDeliverySchemaEventCollection.SetItem(Index: Integer; Value: TACBrOpenDeliverySchemaEvent);
begin
  inherited Items[Index] := Value;
end;

{ TACBrOpenDeliverySchemaAccessToken }

procedure TACBrOpenDeliverySchemaAccessToken.Clear;
begin
  FAccessToken := '';
  FExpiresIn := 0;
end;

procedure TACBrOpenDeliverySchemaAccessToken.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .Value('access_token', FAccessToken)
    .Value('expires_in', FExpiresIn);
end;

procedure TACBrOpenDeliverySchemaAccessToken.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('access_token', FAccessToken)
    .AddPair('expires_in', FExpiresIn);

end;

function TACBrOpenDeliverySchemaAccessToken.IsEmpty: Boolean;
begin
  Result := (FAccessToken = '') and (FExpiresIn = 0);
end;

function TACBrOpenDeliverySchemaAccessToken.IsValid: Boolean;
begin
  Result := (FAccessToken <> '') and (not (FExpiresAt < Now));
end;

{ TACBrOpenDeliverySchemaAcknowledgment }

procedure TACBrOpenDeliverySchemaAcknowledgment.Clear;
begin
  FId := '';
  FOrderId := '';
  FEventType := etCreated;
end;

procedure TACBrOpenDeliverySchemaAcknowledgment.DoReadFromJSon(AJSon: TACBrJSONObject);
var
  LStr: string;
begin
  AJSon
    .Value('id', FId)
    .Value('orderId', FOrderId)
    .Value('eventType', LStr);

  FEventType := StrToEventType(LStr);
end;

procedure TACBrOpenDeliverySchemaAcknowledgment.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('id', FId)
    .AddPair('orderId', FOrderId, False)
    .AddPair('eventType', EventTypeToStr(FEventType));
end;

function TACBrOpenDeliverySchemaAcknowledgment.IsEmpty: Boolean;
begin
  Result := (FId = '') and
            (FOrderId = '') and
            (FEventType = etCreated);
end;

{ TACBrOpenDeliverySchemaAcknowledgmentCollection }

function TACBrOpenDeliverySchemaAcknowledgmentCollection.GetItem(Index: Integer): TACBrOpenDeliverySchemaAcknowledgment;
begin
  Result := TACBrOpenDeliverySchemaAcknowledgment(inherited Items[Index]);
end;

function TACBrOpenDeliverySchemaAcknowledgmentCollection.New: TACBrOpenDeliverySchemaAcknowledgment;
begin
  Result := TACBrOpenDeliverySchemaAcknowledgment.Create;
  Self.Add(Result);
end;

function TACBrOpenDeliverySchemaAcknowledgmentCollection.NewSchema: TACBrOpenDeliverySchema;
begin
  Result := New;
end;

procedure TACBrOpenDeliverySchemaAcknowledgmentCollection.SetItem(Index: Integer; Value: TACBrOpenDeliverySchemaAcknowledgment);
begin
  inherited Items[Index] := Value;
end;

{ TACBrOpenDeliverySchemaOrderTakeout }

procedure TACBrOpenDeliverySchemaOrderTakeout.Clear;
begin
  Fmode := tmDefault;
  FtakeoutDateTime := 0;
end;

procedure TACBrOpenDeliverySchemaOrderTakeout.DoReadFromJSon(AJSon: TACBrJSONObject);
var
  LStr: string;
begin
  AJSon
    .Value('mode', LStr)
    .ValueISODateTime('takeoutDateTime', FtakeoutDateTime);

  Fmode := StrToTakeoutMode(LStr);
end;

procedure TACBrOpenDeliverySchemaOrderTakeout.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('mode', TakeoutModeToStr(Fmode))
    .AddPairISODateTime('takeoutDateTime', FtakeoutDateTime);
end;

function TACBrOpenDeliverySchemaOrderTakeout.IsEmpty: Boolean;
begin
  Result := (Fmode = tmDefault) and (FtakeoutDateTime = 0);
end;

{ TACBrOpenDeliverySchemaOrderAddress }

procedure TACBrOpenDeliverySchemaOrderAddress.Clear;
begin
  Fdistrict := '';
  Fstreet := '';
  FpostalCode := '';
  Fstate := '';
  Fcomplement := '';
  Fnumber := '';
  Fcountry := '';
  Fcity := '';
  Freference := '';
  FformattedAddress := '';
  Fcoordinates.Clear;
end;

constructor TACBrOpenDeliverySchemaOrderAddress.Create(const AObjectName: string);
begin
  inherited Create(AObjectName);
  Fcoordinates := TACBrOpenDeliverySchemaGeoCoordinate.Create('coordinates');
end;

destructor TACBrOpenDeliverySchemaOrderAddress.Destroy;
begin
  Fcoordinates.Free;
  inherited;
end;

procedure TACBrOpenDeliverySchemaOrderAddress.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  AJson
    .Value('country', Fcountry)
    .Value('state', Fstate)
    .Value('city', Fcity)
    .Value('district', Fdistrict)
    .Value('street', Fstreet)
    .Value('number', Fnumber)
    .Value('postalCode', FpostalCode)
    .Value('complement', Fcomplement)
    .Value('reference', Freference)
    .Value('formattedAddress', FformattedAddress);

  Fcoordinates.ReadFromJSon(AJSon);
end;

procedure TACBrOpenDeliverySchemaOrderAddress.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('country', Fcountry)
    .AddPair('state', Fstate)
    .AddPair('city', Fcity)
    .AddPair('district', Fdistrict)
    .AddPair('street', Fstreet)
    .AddPair('number', Fnumber)
    .AddPair('postalCode', FpostalCode)
    .AddPair('complement', Fcomplement)
    .AddPair('reference', Freference)
    .AddPair('formattedAddress', FformattedAddress)
    .AddPairJSONObject('coordinates', Fcoordinates.AsJSON);
end;

function TACBrOpenDeliverySchemaOrderAddress.IsEmpty: Boolean;
begin
  Result := (Fdistrict = '') and
            (Fstreet = '') and
            (FpostalCode = '') and
            (Fstate = '') and
            (Fcomplement = '') and
            (Fnumber = '') and
            (Fcountry = '') and
            (Fcity = '') and
            (Freference = '') and
            (FformattedAddress = '') and
            (Fcoordinates.IsEmpty);
end;

{ TACBrOpenDeliverySchemaOrderDelivery }

procedure TACBrOpenDeliverySchemaOrderDelivery.Clear;
begin
  FdeliveredBy := sMerchant;
  FdeliveryAddress.Clear;
  FestimatedDeliveryDateTime := 0;
  FdeliveryDateTime := 0;
end;

constructor TACBrOpenDeliverySchemaOrderDelivery.Create(const AObjectName: string);
begin
  inherited Create(AObjectName);
  FdeliveryAddress := TACBrOpenDeliverySchemaOrderAddress.Create('deliveryAddress');
end;

destructor TACBrOpenDeliverySchemaOrderDelivery.Destroy;
begin
  FdeliveryAddress.Free;
  inherited;
end;

procedure TACBrOpenDeliverySchemaOrderDelivery.DoReadFromJSon(AJSon: TACBrJSONObject);
var
  LStr: string;
begin
  AJSon
    .Value('deliveredBy', LStr)
    .ValueISODateTime('estimatedDeliveryDateTime', FestimatedDeliveryDateTime)
    .ValueISODateTime('deliveryDateTime', FdeliveryDateTime);

  FdeliveredBy := StrToSponsor(LStr);
  FdeliveryAddress.ReadFromJSon(AJSon);
end;

procedure TACBrOpenDeliverySchemaOrderDelivery.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('deliveredBy', SponsorToStr(FdeliveredBy))
    .AddPairISODateTime('estimatedDeliveryDateTime', FestimatedDeliveryDateTime)
    .AddPairISODateTime('deliveryDateTime', FdeliveryDateTime);

  FdeliveryAddress.WriteToJSon(AJSon);
end;

function TACBrOpenDeliverySchemaOrderDelivery.IsEmpty: Boolean;
begin
  Result := (FdeliveredBy = sMerchant) and
            (FdeliveryAddress.IsEmpty) and
            (FestimatedDeliveryDateTime = 0) and
            (FdeliveryDateTime = 0);
end;

{ TACBrOpenDeliverySchemaOrderCustomer }

procedure TACBrOpenDeliverySchemaOrderCustomer.Clear;
begin
  Fid := '';
  Fphone.Clear;
  FdocumentNumber := '';
  Fname := '';
  FordersCountOnMerchant := 0;
end;

constructor TACBrOpenDeliverySchemaOrderCustomer.Create(const AObjectName: string);
begin
  inherited Create(AObjectName);
  Fphone := TACBrOpenDeliverySchemaPhone.Create('phone');
end;

destructor TACBrOpenDeliverySchemaOrderCustomer.Destroy;
begin
  Fphone.Free;
  inherited;
end;

procedure TACBrOpenDeliverySchemaOrderCustomer.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .Value('id', Fid)
    .Value('documentNumber', FdocumentNumber)
    .Value('name', Fname)
    .Value('ordersCountOnMerchant', FordersCountOnMerchant);

  Fphone.ReadFromJSon(AJSon);
end;

procedure TACBrOpenDeliverySchemaOrderCustomer.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('id', Fid)
    .AddPair('documentNumber', FdocumentNumber)
    .AddPair('name', Fname)
    .AddPair('ordersCountOnMerchant', FordersCountOnMerchant);

  Fphone.WriteToJSon(AJSon);
end;

function TACBrOpenDeliverySchemaOrderCustomer.IsEmpty: Boolean;
begin
  Result := (Fid = '') and
            (Fphone.IsEmpty) and
            (FdocumentNumber = '') and
            (Fname = '') and
            (FordersCountOnMerchant = 0);
end;

{ TACBrOpenDeliverySchemaPhone }

procedure TACBrOpenDeliverySchemaPhone.Clear;
begin
  Fnumber := '';
  Fextension := '';
end;

procedure TACBrOpenDeliverySchemaPhone.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .Value('number', Fnumber)
    .Value('extension', Fextension);
end;

procedure TACBrOpenDeliverySchemaPhone.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('number', Fnumber)
    .AddPair('extension', Fextension);
end;

function TACBrOpenDeliverySchemaPhone.IsEmpty: Boolean;
begin
  Result := (Fnumber = '') and (Fextension = '');
end;

{ TACBrOpenDeliverySchemaOrderMerchant }

procedure TACBrOpenDeliverySchemaOrderMerchant.Clear;
begin
  Fid := '';
  Fname := '';
end;

procedure TACBrOpenDeliverySchemaOrderMerchant.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .Value('id', Fid)
    .Value('name', Fname);
end;

procedure TACBrOpenDeliverySchemaOrderMerchant.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('id', Fid)
    .AddPair('name', Fname);
end;

function TACBrOpenDeliverySchemaOrderMerchant.IsEmpty: Boolean;
begin
  Result := (Fid = '') and (Fname = '');
end;

{ TACBrOpenDeliverySchemaOrderItemOption }

procedure TACBrOpenDeliverySchemaOrderItemOption.Clear;
begin
  Findex := 0;
  Fid := '';
  Fname := '';
  FexternalCode := '';
  Funit := '';
  Fean := '';
  Fquantity := 0;
  FunitPrice.Clear;
  FtotalPrice.Clear;
  FspecialInstructions := '';
end;

constructor TACBrOpenDeliverySchemaOrderItemOption.Create(const AObjectName: string);
begin
  inherited Create(AObjectName);
  FunitPrice := TACBrOpenDeliverySchemaPrice.Create('unitPrice');
  FtotalPrice := TACBrOpenDeliverySchemaPrice.Create('totalPrice');
end;

destructor TACBrOpenDeliverySchemaOrderItemOption.Destroy;
begin
  FunitPrice.Free;
  FtotalPrice.Free;
  inherited;
end;

procedure TACBrOpenDeliverySchemaOrderItemOption.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .Value('index', Findex)
    .Value('id', Fid)
    .Value('name', Fname)
    .Value('externalCode', FexternalCode)
    .Value('unit', Funit)
    .Value('ean', Fean)
    .Value('quantity', Fquantity)
    .Value('specialInstructions', FspecialInstructions);

  FunitPrice.ReadFromJSon(AJSon);
  FtotalPrice.ReadFromJSon(AJSon);
end;

procedure TACBrOpenDeliverySchemaOrderItemOption.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('index', Findex)
    .AddPair('id', Fid)
    .AddPair('name', Fname)
    .AddPair('externalCode', FexternalCode)
    .AddPair('unit', Funit)
    .AddPair('ean', Fean)
    .AddPair('quantity', Fquantity)
    .AddPair('specialInstructions', FspecialInstructions);

  FunitPrice.WriteToJSon(AJSon);
  FtotalPrice.WriteToJSon(AJSon);
end;

function TACBrOpenDeliverySchemaOrderItemOption.IsEmpty: Boolean;
begin
  Result := (Findex = 0) and
            (Fid = '') and
            (Fname = '') and
            (FexternalCode = '') and
            (Funit = '') and
            (Fean = '') and
            (Fquantity = 0) and
            (FunitPrice.IsEmpty) and
            (FtotalPrice.IsEmpty) and
            (FspecialInstructions = '');
end;

{ TACBrOpenDeliverySchemaOrderItemOptionCollection }

function TACBrOpenDeliverySchemaOrderItemOptionCollection.GetItem(Index: Integer): TACBrOpenDeliverySchemaOrderItemOption;
begin
  Result := TACBrOpenDeliverySchemaOrderItemOption(inherited Items[Index]);
end;

function TACBrOpenDeliverySchemaOrderItemOptionCollection.New: TACBrOpenDeliverySchemaOrderItemOption;
begin
  Result := TACBrOpenDeliverySchemaOrderItemOption.Create;
  Self.Add(Result);
end;

function TACBrOpenDeliverySchemaOrderItemOptionCollection.NewSchema: TACBrOpenDeliverySchema;
begin
  Result := New;
end;

procedure TACBrOpenDeliverySchemaOrderItemOptionCollection.SetItem(Index: Integer; Value: TACBrOpenDeliverySchemaOrderItemOption);
begin
  inherited Items[Index] := Value;
end;

{ TACBrOpenDeliverySchemaOrderItem }

procedure TACBrOpenDeliverySchemaOrderItem.Clear;
begin
  Findex := 0;
  Fid := '';
  Fname := '';
  FexternalCode := '';
  Funit := '';
  Fean := '';
  Fquantity := 0;
  FunitPrice.Clear;
  FtotalPrice.Clear;
  FoptionsPrice.Clear;
  Foptions.Clear;
  FspecialInstructions := '';
end;

constructor TACBrOpenDeliverySchemaOrderItem.Create(const AObjectName: string);
begin
  inherited Create(AObjectName);
  FunitPrice := TACBrOpenDeliverySchemaPrice.Create('unitPrice');
  FoptionsPrice := TACBrOpenDeliverySchemaPrice.Create('optionsPrice');
  FtotalPrice := TACBrOpenDeliverySchemaPrice.Create('totalPrice');
  Foptions := TACBrOpenDeliverySchemaOrderItemOptionCollection.Create('options');
end;

destructor TACBrOpenDeliverySchemaOrderItem.Destroy;
begin
  FunitPrice.Free;
  FoptionsPrice.Free;
  FtotalPrice.Free;
  Foptions.Free;
  inherited;
end;

procedure TACBrOpenDeliverySchemaOrderItem.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .Value('index', Findex)
    .Value('id', Fid)
    .Value('name', Fname)
    .Value('externalCode', FexternalCode)
    .Value('unit', Funit)
    .Value('ean', Fean)
    .Value('quantity', Fquantity)
    .Value('specialInstructions', FspecialInstructions);

  FunitPrice.ReadFromJSon(AJSon);
  FtotalPrice.ReadFromJSon(AJSon);
  FoptionsPrice.ReadFromJSon(AJSon);
  Foptions.ReadFromJSon(AJSon);
end;

procedure TACBrOpenDeliverySchemaOrderItem.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('index', Findex)
    .AddPair('id', Fid)
    .AddPair('name', Fname)
    .AddPair('externalCode', FexternalCode)
    .AddPair('unit', Funit)
    .AddPair('ean', Fean)
    .AddPair('quantity', Fquantity)
    .AddPair('specialInstructions', FspecialInstructions);

  FunitPrice.WriteToJSon(AJSon);
  FtotalPrice.WriteToJSon(AJSon);
  FoptionsPrice.WriteToJSon(AJSon);
  Foptions.WriteToJSon(AJSon);
end;

function TACBrOpenDeliverySchemaOrderItem.IsEmpty: Boolean;
begin
  Result := (Findex = 0) and
            (Fid = '') and
            (Fname = '') and
            (FexternalCode = '') and
            (Funit = '') and
            (Fean = '') and
            (Fquantity = 0) and
            (FunitPrice.IsEmpty) and
            (FtotalPrice.IsEmpty) and
            (FoptionsPrice.IsEmpty) and
            (Foptions.IsEmpty) and
            (FspecialInstructions = '');
end;

{ TACBrOpenDeliverySchemaOrderItemCollection }

function TACBrOpenDeliverySchemaOrderItemCollection.GetItem(Index: Integer): TACBrOpenDeliverySchemaOrderItem;
begin
  Result := TACBrOpenDeliverySchemaOrderItem(inherited Items[Index]);
end;

function TACBrOpenDeliverySchemaOrderItemCollection.New: TACBrOpenDeliverySchemaOrderItem;
begin
  Result := TACBrOpenDeliverySchemaOrderItem.Create;
  Self.Add(Result);
end;

function TACBrOpenDeliverySchemaOrderItemCollection.NewSchema: TACBrOpenDeliverySchema;
begin
  Result := New;
end;

procedure TACBrOpenDeliverySchemaOrderItemCollection.SetItem(Index: Integer; Value: TACBrOpenDeliverySchemaOrderItem);
begin
  inherited Items[Index] := Value;
end;

{ TACBrOpenDeliverySchemaOrderFee }

procedure TACBrOpenDeliverySchemaOrderFee.Clear;
begin
  Fname := '';
  Ftype := ftDeliveryFee;
  FreceivedBy := rbMarketplace;
  FreceiverDocument := '';
  Fprice.Clear;
  Fobservation := '';
end;

constructor TACBrOpenDeliverySchemaOrderFee.Create(const AObjectName: string);
begin
  inherited Create(AObjectName);
  Fprice := TACBrOpenDeliverySchemaPrice.Create('price');
end;

destructor TACBrOpenDeliverySchemaOrderFee.Destroy;
begin
  Fprice.Free;
  inherited;
end;

procedure TACBrOpenDeliverySchemaOrderFee.DoReadFromJSon(AJSon: TACBrJSONObject);
var
  LStrType: string;
  LStrReceivedBy: string;
begin
  AJSon
    .Value('name', Fname)
    .Value('type', LStrType)
    .Value('receivedBy', LStrReceivedBy)
    .Value('receiverDocument', FreceiverDocument)
    .Value('observation', Fobservation);

  Fprice.ReadFromJSon(AJSon);
  Ftype := StrToFeeType(LStrType);
  FreceivedBy := StrToFeeReceivedBy(LStrReceivedBy);
end;

procedure TACBrOpenDeliverySchemaOrderFee.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('name', Fname)
    .AddPair('type', FeeTypeToStr(Ftype))
    .AddPair('receivedBy', FeeReceivedByToStr(FreceivedBy))
    .AddPair('receiverDocument', FreceiverDocument)
    .AddPair('observation', Fobservation);

  Fprice.WriteToJSon(AJSon);
end;

function TACBrOpenDeliverySchemaOrderFee.IsEmpty: Boolean;
begin
  Result := (Fname = '') and
            (Ftype = ftDeliveryFee) and
            (FreceivedBy = rbMarketplace) and
            (FreceiverDocument = '') and
            (Fprice.IsEmpty) and
            (Fobservation = '');
end;

{ TACBrOpenDeliverySchemaOrderFeeCollection }

function TACBrOpenDeliverySchemaOrderFeeCollection.GetItem(Index: Integer): TACBrOpenDeliverySchemaOrderFee;
begin
  Result := TACBrOpenDeliverySchemaOrderFee(inherited Items[Index]);
end;

function TACBrOpenDeliverySchemaOrderFeeCollection.New: TACBrOpenDeliverySchemaOrderFee;
begin
  Result := TACBrOpenDeliverySchemaOrderFee.Create;
  Self.Add(Result);
end;

function TACBrOpenDeliverySchemaOrderFeeCollection.NewSchema: TACBrOpenDeliverySchema;
begin
  Result := New;
end;

procedure TACBrOpenDeliverySchemaOrderFeeCollection.SetItem(Index: Integer; Value: TACBrOpenDeliverySchemaOrderFee);
begin
  inherited Items[Index];
end;

{ TACBrOpenDeliverySchemaOrderDiscountSponsor }

procedure TACBrOpenDeliverySchemaOrderDiscountSponsor.Clear;
begin
  Fname := '';
  Famount.Clear;
end;

constructor TACBrOpenDeliverySchemaOrderDiscountSponsor.Create(const AObjectName: string);
begin
  inherited Create(AObjectName); 
  Famount := TACBrOpenDeliverySchemaPrice.Create('amount');
end;

destructor TACBrOpenDeliverySchemaOrderDiscountSponsor.Destroy;
begin
  Famount.Free;
  inherited;
end;

procedure TACBrOpenDeliverySchemaOrderDiscountSponsor.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  AJSon.Value('name', Fname);
  Famount.ReadFromJSon(AJSon);
end;

procedure TACBrOpenDeliverySchemaOrderDiscountSponsor.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon.AddPair('name', Fname);
  Famount.WriteToJSon(AJSon);
end;

function TACBrOpenDeliverySchemaOrderDiscountSponsor.IsEmpty: Boolean;
begin
  Result := (Fname = '') and (Famount.IsEmpty);
end;

{ TACBrOpenDeliverySchemaOrderDiscountSponsorCollection }

function TACBrOpenDeliverySchemaOrderDiscountSponsorCollection.GetItem(Index: Integer): TACBrOpenDeliverySchemaOrderDiscountSponsor;
begin
  Result := TACBrOpenDeliverySchemaOrderDiscountSponsor(inherited Items[Index]);
end;

function TACBrOpenDeliverySchemaOrderDiscountSponsorCollection.New: TACBrOpenDeliverySchemaOrderDiscountSponsor;
begin
  Result := TACBrOpenDeliverySchemaOrderDiscountSponsor.Create;
  Self.Add(Result);
end;

function TACBrOpenDeliverySchemaOrderDiscountSponsorCollection.NewSchema: TACBrOpenDeliverySchema;
begin
  Result := New;
end;

procedure TACBrOpenDeliverySchemaOrderDiscountSponsorCollection.SetItem(Index: Integer; Value: TACBrOpenDeliverySchemaOrderDiscountSponsor);
begin
  inherited Items[Index] := Value;
end;

{ TACBrOpenDeliverySchemaOrderDiscount }

procedure TACBrOpenDeliverySchemaOrderDiscount.Clear;
begin
  Famount.Clear;
  Ftarget := dtCart;
  FtargetId := '';
  FsponsorshipValues.Clear;
end;

constructor TACBrOpenDeliverySchemaOrderDiscount.Create(const AObjectName: string);
begin
  inherited Create(AObjectName);
  Famount := TACBrOpenDeliverySchemaPrice.Create('amount');
  FsponsorshipValues := TACBrOpenDeliverySchemaOrderDiscountSponsorCollection.Create('sponsorshipValues');
end;

destructor TACBrOpenDeliverySchemaOrderDiscount.Destroy;
begin
  Famount.Free;
  FsponsorshipValues.Free;  
  inherited;
end;

procedure TACBrOpenDeliverySchemaOrderDiscount.DoReadFromJSon(AJSon: TACBrJSONObject);
var
  LStr: string;
begin
  AJSon
    .Value('target', LStr)
    .Value('targetId', FtargetId);

  Ftarget := StrToDiscountTarget(LStr);
  Famount.ReadFromJSon(AJSon);
  FsponsorshipValues.ReadFromJSon(AJSon);
end;

procedure TACBrOpenDeliverySchemaOrderDiscount.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('target', DiscountTargetToStr(Ftarget))
    .AddPair('targetId', FtargetId);

  Famount.WriteToJSon(AJSon);
  FsponsorshipValues.WriteToJSon(AJSon);
end;

function TACBrOpenDeliverySchemaOrderDiscount.IsEmpty: Boolean;
begin
  Result := False;
end;

{ TACBrOpenDeliverySchemaOrderDiscountCollection }

function TACBrOpenDeliverySchemaOrderDiscountCollection.GetItem(Index: Integer): TACBrOpenDeliverySchemaOrderDiscount;
begin
  Result := TACBrOpenDeliverySchemaOrderDiscount(inherited Items[Index]);
end;

function TACBrOpenDeliverySchemaOrderDiscountCollection.New: TACBrOpenDeliverySchemaOrderDiscount;
begin
  Result := TACBrOpenDeliverySchemaOrderDiscount.Create;
  Self.Add(Result);
end;

function TACBrOpenDeliverySchemaOrderDiscountCollection.NewSchema: TACBrOpenDeliverySchema;
begin
  Result := New;
end;

procedure TACBrOpenDeliverySchemaOrderDiscountCollection.SetItem(Index: Integer; Value: TACBrOpenDeliverySchemaOrderDiscount);
begin
  inherited Items[Index] := Value;
end;

{ TACBrOpenDeliverySchemaOrderTotal }

procedure TACBrOpenDeliverySchemaOrderTotal.Clear;
begin
  FitemsPrice.Clear;
  FotherFees.Clear;
  Fdiscount.Clear;
  ForderAmount.Clear;
end;

constructor TACBrOpenDeliverySchemaOrderTotal.Create(const AObjectName: string);
begin
  inherited Create(AObjectName);
  FitemsPrice := TACBrOpenDeliverySchemaPrice.Create('itemsPrice');
  FotherFees := TACBrOpenDeliverySchemaPrice.Create('otherFees');
  Fdiscount := TACBrOpenDeliverySchemaPrice.Create('discount');
  ForderAmount := TACBrOpenDeliverySchemaPrice.Create('orderAmount');
end;

destructor TACBrOpenDeliverySchemaOrderTotal.Destroy;
begin
  FitemsPrice.Free;
  FotherFees.Free;
  Fdiscount.Free;
  ForderAmount.Free;
  inherited;
end;

procedure TACBrOpenDeliverySchemaOrderTotal.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  FitemsPrice.ReadFromJSon(AJSon);
  FotherFees.ReadFromJSon(AJSon);
  Fdiscount.ReadFromJSon(AJSon);
  ForderAmount.ReadFromJSon(AJSon);
end;

procedure TACBrOpenDeliverySchemaOrderTotal.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  FitemsPrice.WriteToJSon(AJSon);
  FotherFees.WriteToJSon(AJSon);
  Fdiscount.WriteToJSon(AJSon);
  ForderAmount.WriteToJSon(AJSon);
end;

function TACBrOpenDeliverySchemaOrderTotal.IsEmpty: Boolean;
begin
  Result := (FitemsPrice.IsEmpty) and
            (FotherFees.IsEmpty) and
            (Fdiscount.IsEmpty) and
            (ForderAmount.IsEmpty);
end;

{ TACBrOpenDeliverySchemaOrderPaymentMethod }

function TACBrOpenDeliverySchemaOrderPaymentMethod.ChangeValue: Currency;
begin
  Result := 0;
  if FchangeFor > 0 then
    Result := FchangeFor - Fvalue;
end;

procedure TACBrOpenDeliverySchemaOrderPaymentMethod.Clear;
begin
  Fvalue := 0;
  Fcurrency := '';
  Ftype := ptPrepaid;
  Fmethod := pmCredit;
  FmethodInfo := '';
  FchangeFor := 0;
end;

procedure TACBrOpenDeliverySchemaOrderPaymentMethod.DoReadFromJSon(AJSon: TACBrJSONObject);
var
  LStrType: string;
  LStrMethod: string;
begin
  AJSon
    .Value('value', Fvalue)
    .Value('currency', Fcurrency)
    .Value('type', LStrType)
    .Value('method', LStrMethod)
    .Value('methodInfo', FmethodInfo)
    .Value('changeFor', FchangeFor);

  Ftype := StrToPaymentType(LStrType);
  Fmethod := StrToPaymentMethod(LStrMethod);
end;

procedure TACBrOpenDeliverySchemaOrderPaymentMethod.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('value', Fvalue)
    .AddPair('currency', Fcurrency)
    .AddPair('type', PaymentTypeToStr(Ftype))
    .AddPair('method', PaymentMethodToStr(Fmethod))
    .AddPair('methodInfo', FmethodInfo)
    .AddPair('changeFor', FchangeFor);
end;

function TACBrOpenDeliverySchemaOrderPaymentMethod.IsEmpty: Boolean;
begin
  Result := (Fvalue = 0) and
            (Fcurrency = '') and
            (Ftype = ptPrepaid) and
            (Fmethod = pmCredit) and
            (FmethodInfo = '') and
            (FchangeFor = 0);
end;

{ TACBrOpenDeliverySchemaOrderPaymentMethodCollection }

function TACBrOpenDeliverySchemaOrderPaymentMethodCollection.GetItem(Index: Integer): TACBrOpenDeliverySchemaOrderPaymentMethod;
begin
  Result := TACBrOpenDeliverySchemaOrderPaymentMethod(inherited Items[Index]);
end;

function TACBrOpenDeliverySchemaOrderPaymentMethodCollection.New: TACBrOpenDeliverySchemaOrderPaymentMethod;
begin
  Result := TACBrOpenDeliverySchemaOrderPaymentMethod.Create;
  Self.Add(Result);
end;

function TACBrOpenDeliverySchemaOrderPaymentMethodCollection.NewSchema: TACBrOpenDeliverySchema;
begin
  Result := New;
end;

procedure TACBrOpenDeliverySchemaOrderPaymentMethodCollection.SetItem(Index: Integer; Value: TACBrOpenDeliverySchemaOrderPaymentMethod);
begin
  inherited Items[Index] := Value;
end;

{ TACBrOpenDeliverySchemaOrderPayment }

procedure TACBrOpenDeliverySchemaOrderPayment.Clear;
begin
  Fprepaid := 0;
  Fpending := 0;
  Fmethods.Clear;
end;

constructor TACBrOpenDeliverySchemaOrderPayment.Create(const AObjectName: string);
begin
  inherited Create(AObjectName);
  Fmethods := TACBrOpenDeliverySchemaOrderPaymentMethodCollection.Create('methods');
end;

destructor TACBrOpenDeliverySchemaOrderPayment.Destroy;
begin
  Fmethods.Free;
  inherited;
end;

procedure TACBrOpenDeliverySchemaOrderPayment.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .Value('prepaid', Fprepaid)
    .Value('pending', Fpending);

  Fmethods.ReadFromJSon(AJSon);
end;

procedure TACBrOpenDeliverySchemaOrderPayment.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('prepaid', Fprepaid)
    .AddPair('pending', Fpending);

  Fmethods.WriteToJSon(AJSon);
end;

function TACBrOpenDeliverySchemaOrderPayment.IsEmpty: Boolean;
begin
  Result := (Fprepaid = 0) and
            (Fpending = 0) and
            (Fmethods.IsEmpty);
end;

{ TACBrOpenDeliverySchemaOrderPaymentCollection }

function TACBrOpenDeliverySchemaOrderPaymentCollection.GetItem(Index: Integer): TACBrOpenDeliverySchemaOrderPayment;
begin
  Result := TACBrOpenDeliverySchemaOrderPayment(inherited Items[Index]);
end;

function TACBrOpenDeliverySchemaOrderPaymentCollection.New: TACBrOpenDeliverySchemaOrderPayment;
begin
  Result := TACBrOpenDeliverySchemaOrderPayment.Create;
  Self.Add(Result);
end;

function TACBrOpenDeliverySchemaOrderPaymentCollection.NewSchema: TACBrOpenDeliverySchema;
begin
  Result := New;
end;

procedure TACBrOpenDeliverySchemaOrderPaymentCollection.SetItem(Index: Integer; Value: TACBrOpenDeliverySchemaOrderPayment);
begin
  inherited Items[Index] := Value;
end;

{ TACBrOpenDeliverySchemaOrder }

procedure TACBrOpenDeliverySchemaOrder.Clear;
begin
  Fid := '';
  Ftype := stDelivery;
  FdisplayId := '';
  FsourceAppId := '';
  FcreatedAt := 0;
  ForderTiming := 'INSTANT';
  FpreparationStartDateTime := 0;
  FextraInfo := '';
  Fmerchant.Clear;
  Fitems.Clear;
  FotherFees.Clear;
  Fdiscounts.Clear;
  Ftotal.Clear;
  Fpayments.Clear;
  Fcustomer.Clear;
  Fdelivery.Clear;
  Ftakeout.Clear;
end;

constructor TACBrOpenDeliverySchemaOrder.Create(const AObjectName: string);
begin
  inherited Create(AObjectName);
  ForderTiming := 'INSTANT';
  Fmerchant := TACBrOpenDeliverySchemaOrderMerchant.Create('merchant');
  Fitems := TACBrOpenDeliverySchemaOrderItemCollection.Create('items');
  FotherFees := TACBrOpenDeliverySchemaOrderFeeCollection.Create('otherFees');
  Fdiscounts := TACBrOpenDeliverySchemaOrderDiscountCollection.Create('discounts');
  Ftotal := TACBrOpenDeliverySchemaOrderTotal.Create('total');
  Fpayments := TACBrOpenDeliverySchemaOrderPayment.Create('payments');
  Fcustomer := TACBrOpenDeliverySchemaOrderCustomer.Create('customer');
  Fdelivery := TACBrOpenDeliverySchemaOrderDelivery.Create('delivery');
  Ftakeout := TACBrOpenDeliverySchemaOrderTakeout.Create('takeout');
end;

destructor TACBrOpenDeliverySchemaOrder.Destroy;
begin
  Fmerchant.Free;
  Fitems.Free;
  FotherFees.Free;
  Fdiscounts.Free;
  Ftotal.Free;
  Fpayments.Free;
  Fcustomer.Free;
  Fdelivery.Free;
  Ftakeout.Free;
  inherited;
end;

procedure TACBrOpenDeliverySchemaOrder.DoReadFromJSon(AJSon: TACBrJSONObject);
var
  LStrType: string;
begin
  AJSon
    .Value('id', Fid)
    .Value('type', LStrType)
    .Value('displayId', FdisplayId)
    .Value('sourceAppId', FsourceAppId)
    .ValueISODateTime('createdAt', FcreatedAt)
    .Value('orderTiming', ForderTiming)
    .ValueISODateTime('preparationStartDateTime', FpreparationStartDateTime)
    .Value('extraInfo', FextraInfo);

  Ftype := StrToServiceType(LStrType);
  Fmerchant.ReadFromJSon(AJSon);
  Fitems.ReadFromJSon(AJSon);
  FotherFees.ReadFromJSon(AJSon);
  Fdiscounts.ReadFromJSon(AJSon);
  Ftotal.ReadFromJSon(AJSon);
  Fpayments.ReadFromJSon(AJSon);
  Fcustomer.ReadFromJSon(AJSon);
  Fdelivery.ReadFromJSon(AJSon);
  Ftakeout.ReadFromJSon(AJSon);
end;

procedure TACBrOpenDeliverySchemaOrder.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('id', Fid)
    .AddPair('type', ServiceTypeToStr(Ftype))
    .AddPair('displayId', FdisplayId)
    .AddPair('sourceAppId', FsourceAppId)
    .AddPairISODateTime('createdAt', FcreatedAt)
    .AddPair('orderTiming', ForderTiming)
    .AddPairISODateTime('preparationStartDateTime', FpreparationStartDateTime)
    .AddPair('extraInfo', FextraInfo);

  Fmerchant.WriteToJSon(AJSon);
  Fitems.WriteToJSon(AJSon);
  FotherFees.WriteToJSon(AJSon);
  Fdiscounts.WriteToJSon(AJSon);
  Ftotal.WriteToJSon(AJSon);
  Fpayments.WriteToJSon(AJSon);
  Fcustomer.WriteToJSon(AJSon);
  Fdelivery.WriteToJSon(AJSon);
  Ftakeout.WriteToJSon(AJSon);
end;

function TACBrOpenDeliverySchemaOrder.IsEmpty: Boolean;
begin
  Result := (Fid = '') and
            (Ftype = stDelivery) and
            (FdisplayId = '') and
            (FsourceAppId = '') and
            (FcreatedAt = 0) and
            (ForderTiming = 'INSTANT') and
            (FpreparationStartDateTime = 0) and
            (FextraInfo = '') and
            (Fmerchant.IsEmpty) and
            (Fitems.IsEmpty) and
            (FotherFees.IsEmpty) and
            (Fdiscounts.IsEmpty) and
            (Ftotal.IsEmpty) and
            (Fpayments.IsEmpty) and
            (Fcustomer.IsEmpty) and
            (Fdelivery.IsEmpty) and
            (Ftakeout.IsEmpty);
end;

{ TACBrOpenDeliverySchemaOrderConfirmation }

procedure TACBrOpenDeliverySchemaOrderConfirmation.Clear;
begin
  Fid := '';
  Freason := '';
  FcreatedAt := 0;
  ForderExternalCode := '';
end;

procedure TACBrOpenDeliverySchemaOrderConfirmation.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .Value('reason', Freason)
    .ValueISODateTime('createdAt', FcreatedAt)
    .Value('orderExternalCode', ForderExternalCode);
end;

procedure TACBrOpenDeliverySchemaOrderConfirmation.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('reason', Freason)
    .AddPairISODateTime('createdAt', FcreatedAt)
    .AddPair('orderExternalCode', ForderExternalCode);
end;

function TACBrOpenDeliverySchemaOrderConfirmation.IsEmpty: Boolean;
begin
  Result := (Fid = '') and
            (Freason = '') and
            (FcreatedAt = 0) and
            (ForderExternalCode = '');
end;

{ TACBrOpenDeliverySchemaOrderCancelRequest }

function TACBrOpenDeliverySchemaOrderCancelRequest.AddInvalidItems(const AValue: string): TACBrOpenDeliverySchemaOrderCancelRequest;
begin
  Result := Self;
  SetLength(FinvalidItems, Length(FinvalidItems) + 1);
  FinvalidItems[Length(FinvalidItems) - 1] := AValue;
end;

function TACBrOpenDeliverySchemaOrderCancelRequest.AddOutOfStockItems(const AValue: string): TACBrOpenDeliverySchemaOrderCancelRequest;
begin
  Result := Self;
  SetLength(FoutOfStockItems, Length(FoutOfStockItems) + 1);
  FoutOfStockItems[Length(FoutOfStockItems) - 1] := AValue;
end;

procedure TACBrOpenDeliverySchemaOrderCancelRequest.Clear;
begin
  ForderId := '';
  Freason := '';
  Fcode := crcSystemicIssues;
  Fmode := crmAuto;
  SetLength(FoutOfStockItems, 0);
  SetLength(FinvalidItems, 0);
end;

procedure TACBrOpenDeliverySchemaOrderCancelRequest.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .Value('reason', Freason)
    .Value('outOfStockItems', FoutOfStockItems)
    .Value('invalidItems', FinvalidItems);
end;

procedure TACBrOpenDeliverySchemaOrderCancelRequest.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('reason', Freason)
    .AddPair('code', CancelRequestCodeToStr(Fcode))
    .AddPair('mode', CancelRequestModeToStr(Fmode))
    .AddPair('outOfStockItems', FoutOfStockItems)
    .AddPair('invalidItems', FinvalidItems);
end;

function TACBrOpenDeliverySchemaOrderCancelRequest.IsEmpty: Boolean;
begin
  Result := (ForderId = '') and
            (Freason = '') and
            (Fcode = crcSystemicIssues) and
            (Fmode = crmAuto) and
            (Length(FoutOfStockItems) = 0) and
            (Length(FinvalidItems) = 0);
end;

{ TACBrOpenDeliverySchemaOrderCancelDenyRequest }

procedure TACBrOpenDeliverySchemaOrderCancelDenyRequest.Clear;
begin
  ForderId := '';
  Freason := '';
  Fcode := dccDishAlreadyDone;
end;

procedure TACBrOpenDeliverySchemaOrderCancelDenyRequest.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  AJSon.Value('reason', Freason);
end;

procedure TACBrOpenDeliverySchemaOrderCancelDenyRequest.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('reason', Freason)
    .AddPair('code', DenyCancelCodeToStr(Fcode));
end;

function TACBrOpenDeliverySchemaOrderCancelDenyRequest.IsEmpty: Boolean;
begin
  Result := (ForderId = '') and
            (Freason = '') and
            (Fcode = dccDishAlreadyDone);
end;

end.
