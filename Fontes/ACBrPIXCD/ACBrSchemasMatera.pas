{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
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
  https://doc-api.matera.com/mp_server.html

*)

{$I ACBr.inc}

unit ACBrSchemasMatera;

interface

uses
  Classes, SysUtils, ACBrPIXBase, ACBrPIXSchemasCobV, ACBrJSON,
  ACBrUtil.FilesIO;

type  

  TMateraDocumentType = (
    mdtIdentityFront,
    mdtIdentityBack,
    mdtPicture,
    mdtCNH,
    mdtDriversLicense,
    mdtProofOfResidence,
    mdtTaxID,
    mdtLifeProof,
    mdtBiometry,
    mdtUnknown
  );

  TMateraClientType = (
    mctPerson,
    mctCorporate,
    mctForeigner
  );

  TMateraAccountType = (
    matOrdinary,
    matOverdraftProtected,
    matCommon,
    matUnlimitedOrdinary
  );

  TMateraAccountStatus = (
    masRegular,
    masLocked,
    masClosed,
    masReserved,
    masCreating,
    masError
  );

  TMateraActiveStatus = (
    macActive,
    macInactive
  );

  TMateraAliasType = (
    //malTaxId,
    //malEmail,
    //malPhone,
    malEVP
  );

  TMateraAliasStatus = (
    mastIdentityValidationPending,
    mastClearingRegistrationPending,
    mastActive,
    mastExcluded,
    mastPendingDeletion,
    mastRejected,
    mastExpired,
    mastInvalidKey,
    mastPendingPortabilityConfirmation,
    mastPendingClaimConfirmation,
    mastDonatedForPortability,
    mastPortabilityRequested,
    mastUserConfirmationPendingPortability,
    mastPendingPortabilityDICT,
    mastAwaitingReturnPSPDonor,
    mastUnsuccessfulPortability,
    mastCancelPortabilityRequest,
    mastConfirmClaimingPortability,
    mastSuccessfulPortability,
    mastPendingClaimDICT,
    mastClaimAwaitingReturnPSPDonor,
    mastPendingValidationCompleteClaim,
    mastSuccessfulClaim,
    mastUnsuccessfulClaim,
    mastCancelClaimRequest,
    mastOwnershipRequested,
    mastUserKeyOwnershipValidationPending,
    mastDonorPSPKeyConfirmed,
    mastDonorPSPKeyUnconfirmed,
    mastClaimResolutionPeriodClosed,
    mastDonatedByClaim,
    mastCancelPSPDonorOwnershipClaimOrder,
    mastPendingUpdate,
    mastPendingCompleteClaimDICT,
    mastCancelDonorClaimRequest
  );

  TMateraTransactionType = (
    //mttCreditCard,
    //mttDirectDebit,
    //mttBoleto,
    //mttBoletoCobranca,
    //mttInternalTransfer,
    //mttMobilePhone,
    //mttExternal,
    mttInstantPayment
  );

  TMateraDynamicQRCodeType = (
    mqtImmediate,
    mqtBillingDueDate
  );

  TMateraMaritalStatus = (
    mmsSeparated,
    mmsSeparation_Claimed,
    mmsMarried,
    mmsSingle,
    mmsWidow,
    mmsCommonLawMarriage,
    mmsDivorced,
    mmsOther
  );

  TMateraTransactionStatus = (
    mtsCreated,
    mtsApproved,
    mtsRejected,
    mtsCanceling,
    mtsCanceled,
    mtsPartial,
    mtsExpired,
    mtsTimeOut,
    mtsOverFilled,
    mtsUnfinished,
    mtsError,
    mtsFatalError,
    mtsAuthorized,
    mtsCaptured
  );

  { TMateraTaxIdentifier }

  TMateraTaxIdentifier = class(TACBrPIXSchema)
  private
    fcountry: String;
    ftaxId: String;
    ftaxIdMasked: String;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraTaxIdentifier);

    property taxId: String read ftaxId write ftaxId;
    property country: String read fcountry write fcountry;
    property taxIdMasked: String read ftaxIdMasked write ftaxIdMasked;
  end;

  { TMateraMobilePhone }

  TMateraMobilePhone = class(TACBrPIXSchema)
  private
    fcountry: String;
    fphoneNumber: String;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraMobilePhone);

    property country: String read fcountry write fcountry;
    property phoneNumber: String read fphoneNumber write fphoneNumber;
  end;

  { TMateraMobilePhoneArray }

  TMateraMobilePhoneArray = class(TACBrPIXSchemaArray)
  private
    function GetItem(aIndex: Integer): TMateraMobilePhone;
    procedure SetItem(aIndex: Integer; aValue: TMateraMobilePhone);
  protected
    function NewSchema: TACBrPIXSchema; override;
  public
    Function Add(aItem: TMateraMobilePhone): Integer;
    Procedure Insert(aIndex: Integer; aItem: TMateraMobilePhone);
    function New: TMateraMobilePhone;
    property Items[aIndex: Integer]: TMateraMobilePhone read GetItem write SetItem; default;
  end;

  { TMateraEndereco }

  TMateraEndereco = class(TACBrPIXSchema)
  private
    fbairro: String;
    fcep: String;
    fcidade: String;
    fcomplemento: String;
    festado: String;
    flogradouro: String;
    fnumero: String;
    fpais: String;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraEndereco);

    property logradouro: String read flogradouro write flogradouro;
    property numero: String read fnumero write fnumero;
    property complemento: String read fcomplemento write fcomplemento;
    property bairro: String read fbairro write fbairro;
    property cidade: String read fcidade write fcidade;
    property estado: String read festado write festado;
    property cep: String read fcep write fcep;
    property pais: String read fpais write fpais;
  end;

  { TMateraDocument }

  TMateraDocument = class(TACBrPIXSchema)
  private
    fcontent: String;
    ftype: TMateraDocumentType;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String = ''); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraDocument);

    property content: String read fcontent write fcontent;
    property type_: TMateraDocumentType read ftype write ftype;
  end;

  { TMateraDocumentArray }

  TMateraDocumentArray = class(TACBrPIXSchemaArray)
  private
    function GetItem(aIndex: Integer): TMateraDocument;
    procedure SetItem(aIndex: Integer; aValue: TMateraDocument);
  protected
    function NewSchema: TACBrPIXSchema; override;
  public
    Function Add(aItem: TMateraDocument): Integer;
    Procedure Insert(aIndex: Integer; AItem: TMateraDocument);
    function New: TMateraDocument;
    property Items[aIndex: Integer]: TMateraDocument read GetItem write SetItem; default;
  end;

  { TMateraBasicClient }

  TMateraBasicClient = class(TACBrPIXSchema)
  private
    femail: String;
    fname: String;
    fsocialName: String;
    fmailAddress: TMateraEndereco;
    fdocuments: TMateraDocumentArray;
    fmobilePhone: TMateraMobilePhone;
    ftaxIdentifier: TMateraTaxIdentifier;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraBasicClient);
    
    property name: String read fname write fname;
    property socialName: String read fsocialName write fsocialName;
    property taxIdentifier: TMateraTaxIdentifier read ftaxIdentifier write ftaxIdentifier;
    property mobilePhone: TMateraMobilePhone read fmobilePhone write fmobilePhone;
    property email: String read femail write femail;
    property mailAddress: TMateraEndereco read fmailAddress write fmailAddress;
    property documents: TMateraDocumentArray read fdocuments write fdocuments;
  end;

  { TMateraClient }

  TMateraClient = class(TMateraBasicClient)
  private
    fbirthDate: TDateTime;
    fmother: String;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraClient);
    
    property birthDate: TDateTime read fbirthDate write fbirthDate;
    property mother: String read fmother write fmother;
  end; 

  { TMateraClientRepresentative }

  TMateraClientRepresentative = class(TMateraClient)
  private
    faccountHolderId: String;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String = ''); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraClientRepresentative);

    property accountHolderId: String read faccountHolderId write faccountHolderId;
  end;

  { TMateraClientRepresentativeArray }

  TMateraClientRepresentativeArray = class(TACBrPIXSchemaArray)
  private
    function GetItem(aIndex: Integer): TMateraClientRepresentative;
    procedure SetItem(aIndex: Integer; aValue: TMateraClientRepresentative);
  protected
    function NewSchema: TACBrPIXSchema; override;
  public
    Function Add(aItem: TMateraClientRepresentative): Integer;
    Procedure Insert(aIndex: Integer; aItem: TMateraClientRepresentative);
    function New: TMateraClientRepresentative;
    property Items[aIndex: Integer]: TMateraClientRepresentative read GetItem write SetItem; default;
  end;

  { TMateraRG }

  TMateraRG = class(TACBrPIXSchema)
  private
    fissueDate: TDateTime;
    fissuer: String;
    fnumber: String;
    fstate: String;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraRG);

    property issueDate: TDateTime read fissueDate write fissueDate;
    property number: String read fnumber write fnumber;
    property issuer: String read fissuer write fissuer;
    property state: String read fstate write fstate;
  end;

  { TMateraOtherDoc }

  TMateraOtherDoc = class(TACBrPIXSchema)
  private
    fissueDate: TDateTime;
    fissuer: String;
    fnumber: String;
    ftype: String;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraOtherDoc);

    property issueDate: TDateTime read fissueDate write fissueDate;
    property issuer: String read fissuer write fissuer;
    property number: String read fnumber write fnumber;
    property type_: String read ftype write ftype;
  end;

  { TMateraAccount }

  TMateraAccount = class(TACBrPIXSchema)
  private
    faccount: Integer;
    faccountID: String;
    fbranch: Integer;
    fmobilePhone: TMateraMobilePhone;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraAccount);

    property accountID: String read faccountID write faccountID;
    property account: Integer read faccount write faccount;
    property branch: Integer read fbranch write fbranch;
    property mobilePhone: TMateraMobilePhone read fmobilePhone write fmobilePhone;
  end;

  { TMateraAdditionalDetailsBasic }

  TMateraAdditionalDetailsBasic = class(TACBrPIXSchema)
  private
    fbirthCity: String;
    fbirthCountry: String;
    fbirthDate: TDateTime;
    fbirthState: String;
    fbusinessLine: Integer;
    fdocumentType: String;
    ffather: String;
    ffinancialStatistic: Double;
    fgender: String;
    fmaritalStatus: String;
    fmother: String;
    foccupation: Integer;
    fotherDocument: TMateraOtherDoc;
    fpartner: String;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraAdditionalDetailsBasic);

    property birthCity: String read fbirthCity write fbirthCity;
    property birthCountry: String read fbirthCountry write fbirthCountry;
    property birthDate: TDateTime read fbirthDate write fbirthDate;
    property birthState: String read fbirthState write fbirthState;
    property businessLine: Integer read fbusinessLine write fbusinessLine;
    property documentType: String read fdocumentType write fdocumentType;
    property father: String read ffather write ffather;
    property financialStatistic: Double read ffinancialStatistic write ffinancialStatistic;
    property gender: String read fgender write fgender;
    property maritalStatus: String read fmaritalStatus write fmaritalStatus;
    property mother: String read fmother write fmother;
    property occupation: Integer read foccupation write foccupation;
    property otherDocument: TMateraOtherDoc read fotherDocument write fotherDocument;
    property partner: String read fpartner write fpartner;
  end;

  { TMateraAdditionalDetailsPerson }

  TMateraAdditionalDetailsPerson = class(TMateraAdditionalDetailsBasic)
  private
    flegalResponsible: TMateraClient;
    fmonthlyIncome: Double;
    frg: TMateraRG;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraAdditionalDetailsPerson);

    property rg: TMateraRG read frg write frg;
    property monthlyIncome: Double read fmonthlyIncome write fmonthlyIncome;
    property legalResponsible: TMateraClient read flegalResponsible write flegalResponsible;
  end; 

  { TMateraAdditionalDetailsCorporate }

  TMateraAdditionalDetailsCorporate = class(TACBrPIXSchema)
  private
    fbusinessLine: Integer;
    fcompanyName: String;
    festablishmentDate: TDateTime;
    festablishmentForm: String;
    ffinancialStatistic: Double;
    fmonthlyIncome: Double;
    fstateRegistration: String;
    frepresentatives: TMateraClientRepresentativeArray;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraAdditionalDetailsCorporate);

    property companyName: String read fcompanyName write fcompanyName;
    property businessLine: Integer read fbusinessLine write fbusinessLine;
    property establishmentForm: String read festablishmentForm write festablishmentForm;
    property establishmentDate: TDateTime read festablishmentDate write festablishmentDate;
    property stateRegistration: String read fstateRegistration write fstateRegistration;
    property monthlyIncome: Double read fmonthlyIncome write fmonthlyIncome;
    property financialStatistic: Double read ffinancialStatistic write ffinancialStatistic;

    property representatives: TMateraClientRepresentativeArray read frepresentatives write frepresentatives;
  end;

  { TMateraAccountTransactionRequestBasic }

  TMateraAccountTransactionRequestBasic = class(TACBrPIXSchema)
  private
    faccountInternalTypeId: Integer;
    faccountType: TMateraAccountType;
    fexternalIdentifier: String;
    fibkPwdHash: String;
    fsharedAccount: String;

    fclientType: TMateraClientType;
    fdocuments: TMateraDocumentArray;
    fadditionalDetailsCorporate: TMateraAdditionalDetailsCorporate;
    fadditionalDetailsForeigner: TMateraAdditionalDetailsBasic;
    fadditionalDetailsPerson: TMateraAdditionalDetailsPerson;
    fbillingAddress: TMateraEndereco;
    fmobilePhone: TMateraMobilePhone;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraAccountTransactionRequestBasic);

    property externalIdentifier: String read fexternalIdentifier write fexternalIdentifier;
    property sharedAccount: String read fsharedAccount write fsharedAccount;
    property clientType: TMateraClientType read fclientType write fclientType;
    property accountType: TMateraAccountType read faccountType write faccountType;
    property ibkPwdHash: String read fibkPwdHash write fibkPwdHash;
    property accountInternalTypeId: Integer read faccountInternalTypeId write faccountInternalTypeId;

    property additionalDetailsCorporate: TMateraAdditionalDetailsCorporate read fadditionalDetailsCorporate write fadditionalDetailsCorporate;
    property additionalDetailsPerson: TMateraAdditionalDetailsPerson read fadditionalDetailsPerson write fadditionalDetailsPerson;
    property additionalDetailsForeigner: TMateraAdditionalDetailsBasic read fadditionalDetailsForeigner write fadditionalDetailsForeigner;
    property billingAddress: TMateraEndereco read fbillingAddress write fbillingAddress;
    property documents: TMateraDocumentArray read fdocuments write fdocuments;
    property mobilePhone: TMateraMobilePhone read fmobilePhone write fmobilePhone;
  end;

  { TMateraCreateAccountTransactionRequest }

  TMateraCreateAccountTransactionRequest = class(TMateraAccountTransactionRequestBasic)
  private
    fcheckingAccountBranch: Integer;
    fcheckingAccountNumber: Integer;
    fcustomData: String;
    fclient: TMateraBasicClient;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraCreateAccountTransactionRequest);

    property checkingAccountBranch: Integer read fcheckingAccountBranch write fcheckingAccountBranch;
    property checkingAccountNumber: Integer read fcheckingAccountNumber write fcheckingAccountNumber;
    property customData: String read fcustomData write fcustomData;
    property client: TMateraBasicClient read fclient write fclient;
  end;

  { TMateraAccountTransactionRequest }

  TMateraAccountTransactionRequest = class(TMateraAccountTransactionRequestBasic)
  private
    fclient: TMateraClient;
    frepresentative: TMateraClient;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraAccountTransactionRequest);

    property client: TMateraClient read fclient write fclient;
    property representative: TMateraClient read frepresentative write frepresentative;
  end;

  { TMateraRates }

  TMateraRates = class(TACBrPIXSchema)
  private
    fchargeDay: Integer;
    fchargesPeriod: String;
    feffectiveCostMonthly: Double;
    feffectiveCostYearly: Double;
    fmonthly: Double;
    fyearly: Double;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraRates);

    property chargeDay: Integer read fchargeDay write fchargeDay;
    property chargesPeriod: String read fchargesPeriod write fchargesPeriod;
    property effectiveCostMonthly: Double read feffectiveCostMonthly write feffectiveCostMonthly;
    property effectiveCostYearly: Double read feffectiveCostYearly write feffectiveCostYearly;
    property monthly: Double read fmonthly write fmonthly;
    property yearly: Double read fyearly write fyearly;
  end;

  { TMateraFinancialLimit }

  TMateraFinancialLimit = class(TACBrPIXSchema)
  private
    fcurrentMonthlyFinancialInjection: Double;
    fmaxCreditLimit: Double;
    fmonthlyFinancialInjectionLimit: Double;
    frealBalance: Double;
    frealBalanceLimit: Double;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraFinancialLimit);

    property realBalanceLimit: Double read frealBalanceLimit write frealBalanceLimit;
    property monthlyFinancialInjectionLimit: Double read fmonthlyFinancialInjectionLimit write fmonthlyFinancialInjectionLimit;
    property realBalance: Double read frealBalance write frealBalance;
    property maxCreditLimit: Double read fmaxCreditLimit write fmaxCreditLimit;
    property currentMonthlyFinancialInjection: Double read fcurrentMonthlyFinancialInjection write fcurrentMonthlyFinancialInjection;
  end;

  { TMateraAccountResponse }

  TMateraAccountResponse = class(TACBrPIXSchema)
  private
    faccountHolderId: String;
    faccountInternalTypeId: Integer;
    fdueDate: TDateTime;
    fmediatorId: String;
    frates: TMateraRates;
    fclient: TMateraClient;
    faccount: TMateraAccount;
    frepresentative: TMateraClient;  
    fclientType: TMateraClientType;
    fbillingAddress: TMateraEndereco;
    faccountStatus: TMateraAccountStatus;    
    ffinancialLimit: TMateraFinancialLimit;
    fadditionalDetailsCorporate: TMateraAdditionalDetailsCorporate;
    fadditionalDetailsForeigner: TMateraAdditionalDetailsBasic;
    fadditionalDetailsPerson: TMateraAdditionalDetailsPerson;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraAccountResponse);

    property accountHolderId: String read faccountHolderId write faccountHolderId;
    property dueDate: TDateTime read fdueDate write fdueDate;
    property mediatorId: String read fmediatorId write fmediatorId;
    property accountInternalTypeId: Integer read faccountInternalTypeId write faccountInternalTypeId;

    property rates: TMateraRates read frates write frates;
    property account: TMateraAccount read faccount write faccount;
    property client: TMateraClient read fclient write fclient;
    property representative: TMateraClient read frepresentative write frepresentative;
    property billingAddress: TMateraEndereco read fbillingAddress write fbillingAddress;
    property clientType: TMateraClientType read fclientType write fclientType;
    property additionalDetailsPerson: TMateraAdditionalDetailsPerson read fadditionalDetailsPerson write fadditionalDetailsPerson;
    property additionalDetailsCorporate: TMateraAdditionalDetailsCorporate read fadditionalDetailsCorporate write fadditionalDetailsCorporate;
    property additionalDetailsForeigner: TMateraAdditionalDetailsBasic read fadditionalDetailsForeigner write fadditionalDetailsForeigner;
    property financialLimit: TMateraFinancialLimit read ffinancialLimit write ffinancialLimit;
    property accountStatus: TMateraAccountStatus read faccountStatus write faccountStatus;
  end;

  { TMateraAccountIdentifier }

  TMateraAccountIdentifier = class(TMateraAccount)
  private
    faccountType: TMateraAccountType;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraAccountIdentifier);

    property accountType: TMateraAccountType read faccountType write faccountType;
  end;

  { TMateraAccountIdentifierArray }

  TMateraAccountIdentifierArray = class(TACBrPIXSchemaArray)
  private
    function GetItem(aIndex: Integer): TMateraAccountIdentifier;
    procedure SetItem(aIndex: Integer; aValue: TMateraAccountIdentifier);
  protected
    function NewSchema: TACBrPIXSchema; override;
  public
    Function Add(aItem: TMateraAccountIdentifier): Integer;
    Procedure Insert(aIndex: Integer; aItem: TMateraAccountIdentifier);
    function New: TMateraAccountIdentifier;
    property Items[aIndex: Integer]: TMateraAccountIdentifier read GetItem write SetItem; default;
  end;

  { TMateraAccountHolderResponse }

  TMateraAccountHolderResponse = class(TACBrPIXSchema)
  private
    faccountHolderId: String;
    fbillingAddress: TMateraEndereco;
    femail: String;
    fmailAddress: TMateraEndereco;
    fmobilePhone: TMateraMobilePhone;
    fmobilePhones: TMateraMobilePhoneArray;
    fname: String;
    frepresentativeId: String;
    frepresentatives: TMateraClientRepresentativeArray;
    fsocialName: String;
    fstatus: TMateraActiveStatus;
    ftaxIdentifier: TMateraTaxIdentifier;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraAccountHolderResponse);

    property status: TMateraActiveStatus read fstatus write fstatus;
    property accountHolderId: String read faccountHolderId write faccountHolderId;
    property taxIdentifier: TMateraTaxIdentifier read ftaxIdentifier write ftaxIdentifier;
    property mobilePhones: TMateraMobilePhoneArray read fmobilePhones write fmobilePhones;
    property mobilePhone: TMateraMobilePhone read fmobilePhone write fmobilePhone;
    property email: String read femail write femail;
    property mailAddress: TMateraEndereco read fmailAddress write fmailAddress;
    property billingAddress: TMateraEndereco read fbillingAddress write fbillingAddress;
    property name: String read fname write fname;
    property socialName: String read fsocialName write fsocialName;
    property representativeId: String read frepresentativeId write frepresentativeId;
    property representatives: TMateraClientRepresentativeArray read frepresentatives write frepresentatives;
  end;

  { TMateraAccountQueryResponse }

  TMateraAccountQueryResponse = class(TACBrPIXSchema)
  private
    faccountHolder: TMateraAccountHolderResponse;
    faccounts: TMateraAccountIdentifierArray;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraAccountQueryResponse);

    property accounts: TMateraAccountIdentifierArray read faccounts write faccounts;
    property accountHolder: TMateraAccountHolderResponse read faccountHolder write faccountHolder;
  end;

  { TMateraAliasBasic }

  TMateraAliasBasic = class(TACBrPIXSchema)
  private
    fname: String;
    ftype: TMateraAliasType;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraAliasBasic);

    property name: String read fname write fname;
    property type_: TMateraAliasType read ftype write ftype;
  end;

  { TMateraAlias }

  TMateraAlias = class(TMateraAliasBasic)
  private
    fstatus: TMateraAliasStatus;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraAlias);

    property status: TMateraAliasStatus read fstatus write fstatus;
  end;

  { TMateraAliasArray }

  TMateraAliasArray = class(TACBrPIXSchemaArray)
  private
    function GetItem(aIndex: Integer): TMateraAlias;
    procedure SetItem(aIndex: Integer; aValue: TMateraAlias);
  protected
    function NewSchema: TACBrPIXSchema; override;
  public
    Function Add(aItem: TMateraAlias): Integer;
    Procedure Insert(aIndex: Integer; aItem: TMateraAlias);
    function New: TMateraAlias;
    property Items[aIndex: Integer]: TMateraAlias read GetItem write SetItem; default;
  end;

  { TMateraAliasRequest }

  TMateraAliasRequest = class(TACBrPIXSchema)
  private
    falias: TMateraAliasBasic;
    fexternalIdentifier: String;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String = ''); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraAliasRequest);

    property alias_: TMateraAliasBasic read falias write falias;
    property externalIdentifier: String read fexternalIdentifier write fexternalIdentifier;
  end;

  { TMateraRegisterAliasResponse }

  TMateraRegisterAliasResponse = class(TACBrPIXSchema)
  private
    falias: TMateraAlias;
    fneedsIdentifyConfirmation: Boolean;
    fprotocolId: String;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraRegisterAliasResponse);

    property protocolId: String read fprotocolId write fprotocolId;
    property needsIdentifyConfirmation: Boolean read fneedsIdentifyConfirmation write fneedsIdentifyConfirmation;
    property alias_: TMateraAlias read falias write falias;
  end;

  { TMateraRecipient }

  TMateraRecipient = class(TACBrPIXSchema)
  private
    Faccount: TMateraAccount;
    Famount: currency;
    Fcurrency: string;
    Fmediatorfee: currency;
    FrecipientComment: string;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraAlias);

    property account: TMateraAccount read Faccount write Faccount;
    property amount: currency read Famount write Famount;
    property currency: string read Fcurrency write Fcurrency;
    property mediatorfee: currency read Fmediatorfee write Fmediatorfee;
    property recipientComment: string read FrecipientComment write FrecipientComment;
  end;

  { TMateraRecipientsArray }

  TMateraRecipientsArray = class(TACBrPIXSchemaArray)
  private
    function GetItem(aIndex: Integer): TMateraRecipient;
    procedure SetItem(aIndex: Integer; aValue: TMateraRecipient);
  protected
    function NewSchema: TACBrPIXSchema; override;
  public
    Function Add(aItem: TMateraRecipient): Integer;
    Procedure Insert(aIndex: Integer; aItem: TMateraRecipient);
    function New: TMateraRecipient;
    property Items[aIndex: Integer]: TMateraRecipient read GetItem write SetItem; default;
  end;

  { TMateraQRCodeSpecification }

  TMateraQRCodeSpecification = class(TACBrPIXSchema)
  private
    ferrorCorrectionLevel: String;
    fgenerateImageRendering: Boolean;
    fimageWidth: Integer;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraQRCodeSpecification);

    // Valores válidos: [L, Q, M, H, L, Q, M, H]
    property errorCorrectionLevel: String read ferrorCorrectionLevel write ferrorCorrectionLevel;

    property imageWidth: Integer read fimageWidth write fimageWidth;
    property generateImageRendering: Boolean read fgenerateImageRendering write fgenerateImageRendering;
  end;

  { TMateraAdditionalInformation }

  TMateraAdditionalInformation = class(TACBrPIXSchema)
  private
    Fcontent: String;
    Fname: String;
    FshowToPlayer: Boolean;
    procedure Setcontent(AValue: String);
    procedure Setname(AValue: String);
    procedure SetshowToPlayer(AValue: Boolean);
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraAdditionalInformation);

    property name: String read Fname write Setname;
    property content: String read Fcontent write Setcontent;
    property showToPlayer: Boolean read FshowToPlayer write SetshowToPlayer;
  end;

  { TMateraAdditionalInformationArray }

  TMateraAdditionalInformationArray = class(TACBrPIXSchemaArray)
  private
    function GetItem(aIndex: Integer): TMateraAdditionalInformation;
    procedure SetItem(aIndex: Integer; aValue: TMateraAdditionalInformation);
  protected
    function NewSchema: TACBrPIXSchema; override;
  public
    Function Add(aItem: TMateraAdditionalInformation): Integer;
    Procedure Insert(aIndex: Integer; aItem: TMateraAdditionalInformation);
    function New: TMateraAdditionalInformation;
    property Items[aIndex: Integer]: TMateraAdditionalInformation read GetItem write SetItem; default;
  end;

  { TMateraFixedDateDiscount }

  TMateraFixedDateDiscount = class(TACBrPIXSchema)
  private
    fdate: TDateTime;
    fvaluePerc: Currency;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraFixedDateDiscount);

    property valuePerc: Currency read fvaluePerc write fvaluePerc;
    property date: TDateTime read fdate write fdate;
  end;

  { TMateraFixedDateDiscountArray }

  TMateraFixedDateDiscountArray = class(TACBrPIXSchemaArray)
  private
    function GetItem(aIndex: Integer): TMateraFixedDateDiscount;
    procedure SetItem(aIndex: Integer; aValue: TMateraFixedDateDiscount);
  protected
    function NewSchema: TACBrPIXSchema; override;
  public
    Function Add(aItem: TMateraFixedDateDiscount): Integer;
    Procedure Insert(aIndex: Integer; aItem: TMateraFixedDateDiscount);
    function New: TMateraFixedDateDiscount;
    property Items[aIndex: Integer]: TMateraFixedDateDiscount read GetItem write SetItem; default;
  end;

  { TMateraFixedDateDiscountList }

  TMateraFixedDateDiscountList = class(TACBrPIXSchema)
  private
    ffixedDateDiscounts: TMateraFixedDateDiscountArray;
    fmodality: integer;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraFixedDateDiscountList);

    property modality: integer read fmodality write fmodality;
    property fixedDateDiscounts: TMateraFixedDateDiscountArray read ffixedDateDiscounts write ffixedDateDiscounts;

  end;

  { TMateraUniqueDiscount }

  TMateraUniqueDiscount = class(TACBrPIXSchema)
  private
    fmodality: integer;
    funiqueValuePercDiscount: Currency;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraUniqueDiscount);

    property uniqueValuePercDiscount: Currency read funiqueValuePercDiscount write funiqueValuePercDiscount;
    property modality: integer read fmodality write fmodality;
  end;

  { TMateraDiscountsCalculation }

  TMateraDiscountsCalculation = class(TACBrPIXSchema)
  private
    ffixedDateDiscountList: TMateraFixedDateDiscountList;
    funiqueDiscount: TMateraUniqueDiscount;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraDiscountsCalculation);

    property uniqueDiscount: TMateraUniqueDiscount read funiqueDiscount write funiqueDiscount;
    property fixedDateDiscountList: TMateraFixedDateDiscountList read ffixedDateDiscountList write ffixedDateDiscountList;
  end;

  {TMateraPayerInformationAddressing}

  TMateraPayerInformationAddressing = class(TACBrPIXSchema)
  private
    fcep: String;
    fcity: String;
    fstreet: String;
    fuf: String;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraPayerInformationAddressing);

    property street: String read fstreet write fstreet;
    property city: String read fcity write fcity;
    property uf: String read fuf write fuf;
    property cep: String read fcep write fcep;
  end;

  { TMateraQRCodeDynamicPayerInformationComplete }

  TMateraQRCodeDynamicPayerInformationComplete = class(TACBrPIXSchema)
  private
    faddressing: TMateraPayerInformationAddressing;
    fcpfCnpj: String;
    femail: String;
    fname: String;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraQRCodeDynamicPayerInformationComplete);

    property cpfCnpj: String read fcpfCnpj write fcpfCnpj;
    property name: String read fname write fname;
    property email: String read femail write femail;
    property addressing: TMateraPayerInformationAddressing read faddressing write faddressing;
  end;

  { TMateraInterestCalculation }

  TMateraInterestCalculation = class(TACBrPIXSchema)
  private
    fmodality: integer;
    fvaluePerc: currency;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraInterestCalculation);

    property valuePerc: currency read fvaluePerc write fvaluePerc;
    property modality: integer read fmodality write fmodality;
  end;

  {TMateraFinesCalculation}

  TMateraFinesCalculation = class(TACBrPIXSchema)
  private
    fmodality: integer;
    fvaluePerc: currency;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraFinesCalculation);

    property valuePerc: currency read fvaluePerc write fvaluePerc;
    property modality: integer read fmodality write fmodality;
  end;

  { TMateraReductionCalculation }

  TMateraReductionCalculation = class(TACBrPIXSchema)
  private
    fmodality: integer;
    fvaluePerc: currency;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraReductionCalculation);

    property valuePerc: currency read fvaluePerc write fvaluePerc;
    property modality: integer read fmodality write fmodality;
  end;

  { TMatera }

  {TMatera = class(TACBrPIXSchema)
  private
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMatera);

    property c: String read fc write fc;
  end;}

  { TMateraBillingDueDate }

  TMateraBillingDueDate = class(TACBrPIXSchema)
  private
    fdaysAfterDueDate: Integer;
    fdiscounts: TMateraDiscountsCalculation;
    fdueDate: TDateTime;
    ffines: TMateraFinesCalculation;
    finterests: TMateraInterestCalculation;
    fpayerInformation: TMateraQRCodeDynamicPayerInformationComplete;
    freduction: TMateraReductionCalculation;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraBillingDueDate);

    property payerInformation: TMateraQRCodeDynamicPayerInformationComplete read fpayerInformation write fpayerInformation;
    property interests: TMateraInterestCalculation read finterests write finterests;
    property fines: TMateraFinesCalculation read ffines write ffines;
    property discounts: TMateraDiscountsCalculation read fdiscounts write fdiscounts;
    property reduction: TMateraReductionCalculation read freduction write freduction;
    property dueDate: TDateTime read fdueDate write fdueDate;
    property daysAfterDueDate: Integer read fdaysAfterDueDate write fdaysAfterDueDate;
  end;

  { TMateraInstantPayment }

  TMateraInstantPayment = class(TACBrPIXSchema)
  private
    fadditionalInformation: TMateraAdditionalInformationArray;
    falias_: TMateraAlias;
    fbillingDueDate: TMateraBillingDueDate;
    fdynamicQRCodeType: TMateraDynamicQRCodeType;
    fexpiration: integer;
    fqrCodeImageGenerationSpecification: TMateraQRCodeSpecification;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraInstantPayment);

    property alias_: TMateraAlias read Falias_ write Falias_;
    property qrCodeImageGenerationSpecification: TMateraQRCodeSpecification read FqrCodeImageGenerationSpecification write FqrCodeImageGenerationSpecification;
    property expiration: integer read Fexpiration write Fexpiration;
    property additionalInformation: TMateraAdditionalInformationArray read FadditionalInformation write FadditionalInformation;
    property dynamicQRCodeType: TMateraDynamicQRCodeType read fdynamicQRCodeType write fdynamicQRCodeType;
    property billingDueDate: TMateraBillingDueDate read fbillingDueDate write fbillingDueDate;

  end;

  { TMateraPaymentInfo }

  TMateraPaymentInfo = class(TACBrPIXSchema)
  private
    FinstantPayment: TMateraInstantPayment;
    FtransactionType: String;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraPaymentInfo);

    property transactionType: String read FtransactionType write FtransactionType;
    property instantPayment: TMateraInstantPayment read FinstantPayment write FinstantPayment;
  end;

  { TMateraPayerInformation }

  TMateraPayerInformation = class(TACBrPIXSchema)
  private
    faddressing: TMateraPayerInformationAddressing;
    fcpfCnpj: String;
    femail: String;
    fname: String;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraPayerInformation);

    property cpfCnpj: String read fcpfCnpj write fcpfCnpj;
    property name: String read fname write fname;
    property email: String read femail write femail;
    property addressing: TMateraPayerInformationAddressing read faddressing write faddressing;
  end;

  { TMateraTransactionResponse } 

  TMateraTransactionResponse = class(TACBrPIXSchema)
  private
    faccountHolderId: String;
    faccountId: String;
    ftotalAmount: Currency;
    ftransactionDate: String;
    ftransactionId: String;
    ftransactionStatus: TMateraTransactionStatus;
    ftransactionType: TMateraTransactionType;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraTransactionResponse);

    property accountHolderId: String read faccountHolderId write faccountHolderId;
    property accountId: String read faccountId write faccountId;
    property transactionId: String read ftransactionId write ftransactionId;
    property transactionDate: String read ftransactionDate write ftransactionDate;
    property transactionType: TMateraTransactionType read ftransactionType write ftransactionType;
    property transactionStatus: TMateraTransactionStatus read ftransactionStatus write ftransactionStatus;
    property totalAmount: Currency read ftotalAmount write ftotalAmount;
  end; 

  { TMateraTransactionResponseArray }

  TMateraTransactionResponseArray = class(TACBrPIXSchemaArray)
  private
    function GetItem(aIndex: Integer): TMateraTransactionResponse;
    procedure SetItem(aIndex: Integer; aValue: TMateraTransactionResponse);
  protected
    function NewSchema: TACBrPIXSchema; override;
  public
    Function Add(aItem: TMateraTransactionResponse): Integer;
    Procedure Insert(aIndex: Integer; aItem: TMateraTransactionResponse);
    function New: TMateraTransactionResponse;
    property Items[aIndex: Integer]: TMateraTransactionResponse read GetItem write SetItem; default;
  end;


  { TMateraError }

  TMateraError = class(TACBrPIXSchema)
  private
    fcode: String;
    fdescription: String;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String = ''); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraError);

    property code: String read fcode write fcode;
    property description: String read fdescription write fdescription;
  end;

  { TMateraQRCodeRequest }

  TMateraQRCodeRequest = class(TACBrPIXSchema)
  private
    falias: TMateraAliasBasic;
    FcallbackAddress: string;
    fcurrency: string;
    fexternalIdentifier: String;
    FpaymentInfo: TMateraPaymentInfo;
    Frecipients: TMateraRecipientsArray;
    ftotalAmount: Currency;
    procedure SetcallbackAddress(AValue: string);
    procedure Setcurrency(AValue: string);
    procedure Setrecipients(AValue: TMateraRecipientsArray);
    procedure SettotalAmount(AValue: Currency);
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraAliasRequest);

    property totalAmount: Currency read FtotalAmount write SettotalAmount;
    property currency: string read Fcurrency write Setcurrency;
    property paymentInfo: TMateraPaymentInfo read FpaymentInfo write FpaymentInfo;
    property recipients: TMateraRecipientsArray read Frecipients write Setrecipients;
    property externalIdentifier: String read fexternalIdentifier write fexternalIdentifier;
    property callbackAddress: string read FcallbackAddress write SetcallbackAddress;
  end;

  { TMateraAuthorizationDetails }

  TMateraAuthorizationDetails = class(TACBrPIXSchema)
  private
    fnumber: String;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraAuthorizationDetails);

    property number: String read fnumber write fnumber;
  end;

  { TMaterafinancialStatement }

  TMaterafinancialStatement = class(TACBrPIXSchema)
  private
    fAuthorizationDetails: TMateraAuthorizationDetails;
    fstatus: String;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMaterafinancialStatement);

    property status: String read fstatus write fstatus;
    property AuthorizationDetails: TMateraAuthorizationDetails read fAuthorizationDetails write fAuthorizationDetails;
  end;

  { TMateraGeneratedImage }

  TMateraGeneratedImage = class(TACBrPIXSchema)
  private
    factualImageWidth: Integer;
    fimageContent: String;
    fmimeType: String;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraGeneratedImage);

    property imageContent: String read fimageContent write fimageContent;
    property mimeType: String read fmimeType write fmimeType;
    property actualImageWidth: Integer read factualImageWidth write factualImageWidth;
  end;

  { TMateraInstantPaymentQRCodeResponse }

  TMateraInstantPaymentQRCodeResponse = class(TACBrPIXSchema)
  private
    fdynamicQrCodeType: string;
    fGeneratedImage: TMateraGeneratedImage;
    fqrcodeURL: string;
    freference: string;
    ftextContent: string;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraInstantPaymentQRCodeResponse);

    property textContent: string read ftextContent write ftextContent;
    property reference: string read freference write freference;
    property qrcodeURL: string read fqrcodeURL write fqrcodeURL;
    property generateImage: TMateraGeneratedImage read fGeneratedImage write fGeneratedImage;
    property dynamicQrCodeType: string read fdynamicQrCodeType write fdynamicQrCodeType;
  end;

  { TMateracouponDetails }

  TMateracouponDetails = class(TACBrPIXSchema)
  private
    fcouponId: String;
    fdescription: String;
    fseller: String;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateracouponDetails);

    property couponId: String read fcouponId write fcouponId;
    property seller: String read fseller write fseller;
    property description: String read fdescription write fdescription;
  end;

  { TMateraQRCodeResponse }

  TMateraQRCodeResponse = class(TACBrPIXSchema)
  private
    fboletoUrl: string;
    fcreditCardToken: string;
    fdiscountAmount: Currency;
    fdueDate: TDateTime;
    fexpirationDate: TDateTime;
    fexternalIdentifier: String;
    ffinancialStatement: TMateraFinancialStatement;
    finstantPayment: TMateraInstantPaymentQRCodeResponse;
    fpaidAmount: currency;
    frecipientDescription: string;
    fsenderAccountId: string;
    ftotalAmount: currency;
    ftransactionDate: TDateTime;
    ftransactionId: String;
    ftransactionType: string;
    ftypeableLine: string;
    fcouponDetails: TMateracouponDetails;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraQRCodeResponse);

    property senderAccountId: string read fsenderAccountId write fsenderAccountId;
    property creditCardToken: string read fcreditCardToken write fcreditCardToken;
    property boletoUrl: string read fboletoUrl write fboletoUrl;
    property typeableLine: string read ftypeableLine write ftypeableLine;
    property dueDate: TDateTime read fdueDate write fdueDate;
    property discountAmount: Currency read fdiscountAmount write fdiscountAmount;
    property recipientDescription: string read frecipientDescription write frecipientDescription;
    property expirationDate: TDateTime read fexpirationDate write fexpirationDate;
    property couponDetails: TMateracouponDetails read fcouponDetails write fcouponDetails;

    property transactionId: String read ftransactionId write ftransactionId;
    property externalIdentifier: String read fexternalIdentifier write fexternalIdentifier;
    property financialStatement: TMateraFinancialStatement read ffinancialStatement write ffinancialStatement;
    property transactionDate: TDateTime read ftransactionDate write ftransactionDate;
    property transactionType: string read ftransactionType write ftransactionType;
    property totalAmount: currency read ftotalAmount write ftotalAmount;
    property paidAmount: currency read fpaidAmount write fpaidAmount;
    property instantPayment: TMateraInstantPaymentQRCodeResponse read finstantPayment write finstantPayment;

  end;

  { TMatera }

  {TMatera = class(TACBrPIXSchema)
  private
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMatera); 

    property c: String read fc write fc;
  end;}

  function MateraDocumentTypeToString(aType: TMateraDocumentType): String;
  function StringToMateraDocumentType(const aString: String): TMateraDocumentType;

  function MateraClientTypeToString(aType: TMateraClientType): String;
  function StringToMateraClientType(const aString: String): TMateraClientType;

  function MateraAccountTypeToString(aType: TMateraAccountType): String;
  function StringToMateraAccountType(const aString: String): TMateraAccountType;

  function MateraAccountStatusToString(aType: TMateraAccountStatus): String;
  function StringToMateraAccountStatus(const aString: String): TMateraAccountStatus;

  function MateraActiveStatusToString(aType: TMateraActiveStatus): String;
  function StringToMateraActiveStatus(const aString: String): TMateraActiveStatus;

  function MateraAliasTypeToString(aType: TMateraAliasType): String;
  function StringToMateraAliasType(const aString: String): TMateraAliasType;

  function MateraAliasStatusToString(aType: TMateraAliasStatus): String;
  function StringToMateraAliasStatus(const aString: String): TMateraAliasStatus;

  function MateraTransactionTypeToString(aType: TMateraTransactionType): String;
  function StringToMateraTransactionType(const aString: String): TMateraTransactionType;

  function MateraDynamicQRCodeTypeToString(aType: TMateraDynamicQRCodeType): String;
  function StringToMateraDynamicQRCodeType(const aString: String): TMateraDynamicQRCodeType;

  function MateraTransactionStatusToString(aType: TMateraTransactionStatus): String;
  function StringToMateraTransactionStatus(const aString: String): TMateraTransactionStatus;

implementation

uses
  synautil,
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.DateTime, ACBrValidador;

function MateraDocumentTypeToString(aType: TMateraDocumentType): String;
begin
  case aType of
    mdtIdentityFront: Result := 'IDENTITY_FRONT';
    mdtIdentityBack: Result := 'IDENTITY_BACK';
    mdtPicture: Result := 'PICTURE';
    mdtCNH: Result := 'CNH';
    mdtDriversLicense: Result := 'DRIVERSLICENSE';
    mdtProofOfResidence: Result := 'PROOF_OF_RESIDENCE';
    mdtTaxID: Result := 'TAX_ID';
    mdtLifeProof: Result := 'LIFE_PROOF';
    mdtBiometry: Result := 'BIOMETRY';
  else
    Result := 'UNKNOWN';
  end;
end;

function StringToMateraDocumentType(const aString: String): TMateraDocumentType;
var
  s: String;
begin
  s := UpperCase(Trim(aString));
  if (s = 'IDENTITY_FRONT') then
    Result := mdtIdentityFront
  else if (s = 'IDENTITY_BACK') then
    Result := mdtIdentityBack
  else if (s = 'PICTURE') then
    Result := mdtPicture
  else if (s = 'CNH') then
    Result := mdtCNH
  else if (s = 'DRIVERSLICENSE') then
    Result := mdtDriversLicense
  else if (s = 'PROOF_OF_RESIDENCE') then
    Result := mdtProofOfResidence
  else if (s = 'TAX_ID') then
    Result := mdtTaxID
  else if (s = 'LIFE_PROOF') then
    Result := mdtLifeProof
  else if (s = 'BIOMETRY') then
    Result := mdtBiometry
  else
    Result := mdtUnknown;
end;

function MateraClientTypeToString(aType: TMateraClientType): String;
begin
  case aType of
    mctPerson: Result := 'PERSON';
    mctCorporate: Result := 'CORPORATE';
    mctForeigner: Result := 'FOREIGNER';
  else
    Result := EmptyStr;
  end;
end;

function StringToMateraClientType(const aString: String): TMateraClientType;
var
  s: String;
begin
  s := UpperCase(Trim(aString));

  if (s = 'CORPORATE') then
    Result := mctCorporate
  else if (s = 'FOREIGNER') then
    Result := mctForeigner
  else
    Result := mctPerson;
end;

function MateraAccountTypeToString(aType: TMateraAccountType): String;
begin
  case aType of
    matOrdinary: Result := 'ORDINARY';
    matOverdraftProtected: Result := 'OVERDRAFT_PROTECTED';
    matCommon: Result := 'COMMON';
    matUnlimitedOrdinary: Result := 'UNLIMITED_ORDINARY';
  else
    Result := EmptyStr;
  end;
end;

function StringToMateraAccountType(const aString: String): TMateraAccountType;
var
  s: String;
begin
  s := UpperCase(Trim(aString));

  if (s = 'OVERDRAFT_PROTECTED') then
    Result := matOverdraftProtected
  else if (s = 'COMMON') then
    Result := matCommon
  else if (s = 'UNLIMITED_ORDINARY') then
    Result := matUnlimitedOrdinary
  else
    Result := matOrdinary;
end;

function MateraAccountStatusToString(aType: TMateraAccountStatus): String;
begin 
  case aType of
    masRegular: Result := 'REGULAR';
    masLocked: Result := 'LOCKED';
    masClosed: Result := 'CLOSED';
    masReserved: Result := 'RESERVED';
    masCreating: Result := 'CREATING';
    masError: Result := 'ERROR';
  else
    Result := EmptyStr;
  end;
end;

function StringToMateraAccountStatus(const aString: String): TMateraAccountStatus;
var
  s: String;
begin
  s := UpperCase(Trim(aString));

  if (s = 'REGULAR') then
    Result := masRegular
  else if (s = 'LOCKED') then
    Result := masLocked
  else if (s = 'CLOSED') then
    Result := masClosed
  else if (s = 'RESERVED') then
    Result := masReserved
  else if (s = 'CREATING') then
    Result := masCreating
  else
    Result := masError;
end;

function MateraActiveStatusToString(aType: TMateraActiveStatus): String;
begin
  case aType of
    macActive: Result := 'ACTIVE';
    macInactive: Result := 'INACTIVE';
  else
    Result := EmptyStr;
  end;
end;

function StringToMateraActiveStatus(const aString: String): TMateraActiveStatus;
var
  s: String;
begin
  s := UpperCase(Trim(aString));

  if (s = 'ACTIVE') then
    Result := macActive
  else
    Result := macInactive;
end;

function MateraAliasTypeToString(aType: TMateraAliasType): String;
begin
  Result := 'EVP';
  
  {case aType of
    malTaxId: Result := 'TAX_ID';
    malEmail:  Result := 'EMAIL';
    malPhone:  Result := 'PHONE';
  else
    Result := 'EVP';
  end;}
end;

function StringToMateraAliasType(const aString: String): TMateraAliasType;
//var
//  s: String;
begin
  Result := malEVP;

  {s := UpperCase(Trim(aString));
  if (s = 'TAX_ID') then
    Result := malTaxId
  else if (s = 'EMAIL') then
    Result := malEmail
  else if (s = 'PHONE') then
    Result := malPhone
  else
    Result := malEVP;}
end;

function MateraAliasStatusToString(aType: TMateraAliasStatus): String;
begin
  case aType of
    mastIdentityValidationPending: Result := 'IDENTITY_VALIDATION_PENDING';
    mastClearingRegistrationPending: Result := 'CLEARING_REGISTRATION_PENDING';
    mastActive: Result := 'ACTIVE';
    mastExcluded: Result := 'EXCLUDED';
    mastPendingDeletion: Result := 'PENDING_DELETION';
    mastRejected: Result := 'REJECTED';
    mastExpired: Result := 'EXPIRED';
    mastInvalidKey: Result := 'INVALID_KEY';
    mastPendingPortabilityConfirmation: Result := 'PENDING_PORTABILITY_CONFIRMATION';
    mastPendingClaimConfirmation: Result := 'PENDING_CLAIM_CONFIRMATION';
    mastDonatedForPortability: Result := 'DONATED_FOR_PORTABILITY';
    mastPortabilityRequested: Result := 'PORTABILITY_REQUESTED';
    mastUserConfirmationPendingPortability: Result := 'USER_CONFIRMATION_PENDING_PORTABILITY';
    mastPendingPortabilityDICT: Result := 'PENDING_PORTABILITY_DICT';
    mastAwaitingReturnPSPDonor: Result := 'AWAITING_RETURN_PSP_DONOR';
    mastUnsuccessfulPortability: Result := 'UNSUCCESSFUL_PORTABILITY';
    mastCancelPortabilityRequest: Result := 'CANCEL_PORTABILITY_REQUEST';
    mastConfirmClaimingPortability: Result := 'CONFIRM_CLAIMING_PORTABILITY';
    mastSuccessfulPortability: Result := 'SUCCESSFUL_PORTABILITY';
    mastPendingClaimDICT: Result := 'PENDING_CLAIM_DICT';
    mastClaimAwaitingReturnPSPDonor: Result := 'CLAIM_AWAITING_RETURN_PSP_DONOR';
    mastPendingValidationCompleteClaim: Result := 'PENDING_VALIDATION_COMPLETE_CLAIM';
    mastSuccessfulClaim: Result := 'SUCCESSFUL_CLAIM';
    mastUnsuccessfulClaim: Result := 'UNSUCCESSFUL_CLAIM';
    mastCancelClaimRequest: Result := 'CANCEL_CLAIM_REQUEST';
    mastOwnershipRequested: Result := 'OWNERSHIP_REQUESTED';
    mastUserKeyOwnershipValidationPending: Result := 'USER_KEY_OWNERSHIP_VALIDATION_PENDING';
    mastDonorPSPKeyConfirmed: Result := 'DONOR_PSP_KEY_CONFIRMED';
    mastDonorPSPKeyUnconfirmed: Result := 'DONOR_PSP_KEY_UNCONFIRMED';
    mastClaimResolutionPeriodClosed: Result := 'CLAIM_RESOLUTION_PERIOD_CLOSED';
    mastDonatedByClaim: Result := 'DONATED_BY_CLAIM';
    mastCancelPSPDonorOwnershipClaimOrder: Result := 'CANCEL_PSP_DONOR_OWNERSHIP_CLAIM_ORDER';
    mastPendingUpdate: Result := 'PENDING_UPDATE';
    mastPendingCompleteClaimDICT: Result := 'PENDING_COMPLETE_CLAIM_DICT';
    mastCancelDonorClaimRequest: Result := 'CANCEL_DONOR_CLAIM_REQUEST';
  else
    Result := EmptyStr;
  end;
end;

function StringToMateraAliasStatus(const aString: String): TMateraAliasStatus;
var
  s: String;
begin
  s := UpperCase(Trim(aString));

  if (s = 'IDENTITY_VALIDATION_PENDING') then
    Result := mastIdentityValidationPending
  else if (s = 'CLEARING_REGISTRATION_PENDING') then
    Result := mastClearingRegistrationPending
  else if (s = 'ACTIVE') then
    Result := mastActive
  else if (s = 'EXCLUDED') then
    Result := mastExcluded
  else if (s = 'PENDING_DELETION') then
    Result := mastPendingDeletion
  else if (s = 'REJECTED') then
    Result := mastRejected
  else if (s = 'EXPIRED') then
    Result := mastExpired
  else if (s = 'INVALID_KEY') then
    Result := mastInvalidKey
  else if (s = 'PENDING_PORTABILITY_CONFIRMATION') then
    Result := mastPendingPortabilityConfirmation
  else if (s = 'PENDING_CLAIM_CONFIRMATION') then
    Result := mastPendingClaimConfirmation
  else if (s = 'DONATED_FOR_PORTABILITY') then
    Result := mastDonatedForPortability
  else if (s = 'PORTABILITY_REQUESTED') then
    Result := mastPortabilityRequested
  else if (s = 'USER_CONFIRMATION_PENDING_PORTABILITY') then
    Result := mastUserConfirmationPendingPortability
  else if (s = 'PENDING_PORTABILITY_DICT') then
    Result := mastPendingPortabilityDICT
  else if (s = 'AWAITING_RETURN_PSP_DONOR') then
    Result := mastAwaitingReturnPSPDonor
  else if (s = 'UNSUCCESSFUL_PORTABILITY') then
    Result := mastUnsuccessfulPortability
  else if (s = 'CANCEL_PORTABILITY_REQUEST') then
    Result := mastCancelPortabilityRequest
  else if (s = 'CONFIRM_CLAIMING_PORTABILITY') then
    Result := mastConfirmClaimingPortability
  else if (s = 'SUCCESSFUL_PORTABILITY') then
    Result := mastSuccessfulPortability
  else if (s = 'PENDING_CLAIM_DICT') then
    Result := mastPendingClaimDICT
  else if (s = 'CLAIM_AWAITING_RETURN_PSP_DONOR') then
    Result := mastClaimAwaitingReturnPSPDonor
  else if (s = 'PENDING_VALIDATION_COMPLETE_CLAIM') then
    Result := mastPendingValidationCompleteClaim
  else if (s = 'SUCCESSFUL_CLAIM') then
    Result := mastSuccessfulClaim
  else if (s = 'UNSUCCESSFUL_CLAIM') then
    Result := mastUnsuccessfulClaim
  else if (s = 'CANCEL_CLAIM_REQUEST') then
    Result := mastCancelClaimRequest
  else if (s = 'OWNERSHIP_REQUESTED') then
    Result := mastOwnershipRequested
  else if (s = 'USER_KEY_OWNERSHIP_VALIDATION_PENDING') then
    Result := mastUserKeyOwnershipValidationPending
  else if (s = 'DONOR_PSP_KEY_CONFIRMED') then
    Result := mastDonorPSPKeyConfirmed
  else if (s = 'DONOR_PSP_KEY_UNCONFIRMED') then
    Result := mastDonorPSPKeyUnconfirmed
  else if (s = 'CLAIM_RESOLUTION_PERIOD_CLOSED') then
    Result := mastClaimResolutionPeriodClosed
  else if (s = 'DONATED_BY_CLAIM') then
    Result := mastDonatedByClaim
  else if (s = 'CANCEL_PSP_DONOR_OWNERSHIP_CLAIM_ORDER') then
    Result := mastCancelPSPDonorOwnershipClaimOrder
  else if (s = 'PENDING_UPDATE') then
    Result := mastPendingUpdate
  else if (s = 'PENDING_COMPLETE_CLAIM_DICT') then
    Result := mastPendingCompleteClaimDICT
  else if (s = 'CANCEL_DONOR_CLAIM_REQUEST ') then
    Result := mastCancelDonorClaimRequest
  else
    Result := mastAwaitingReturnPSPDonor;
end;

function MateraTransactionTypeToString(aType: TMateraTransactionType): String;
begin
  Result := 'InstantPayment';

  {case aType of
    mttCreditCard: Result := 'CreditCard';
    mttDirectDebit: Result := 'DirectDebit';
    mttBoleto: Result := 'Boleto';
    mttBoletoCobranca: Result := 'BoletoCobranca';
    mttInternalTransfer: Result := 'InternalTransfer';
    mttMobilePhone: Result := 'MobilePhone';
    mttExternal: Result := 'External';
  else
    Result := 'InstantPayment';
  end;}
end;

function StringToMateraTransactionType(const aString: String): TMateraTransactionType;
//var
//  s: String;
begin
  Result := mttInstantPayment;

  {s := UpperCase(Trim(aString));
  if (s = 'CreditCard') then
    Result := mttCreditCard
  else if (s = 'DirectDebit') then
    Result := mttDirectDebit
  else if (s = 'Boleto') then
    Result := mttBoleto
  else if (s = 'BoletoCobranca') then
    Result := mttBoletoCobranca
  else if (s = 'InternalTransfer') then
    Result := mttInternalTransfer
  else if (s = 'MobilePhone') then
    Result := mttMobilePhone
  else if (s = 'External') then
    Result := mttExternal
  else
    Result := mttInstantPayment;}
end;

function MateraDynamicQRCodeTypeToString(aType: TMateraDynamicQRCodeType): String;
begin
  case aType of
    mqtBillingDueDate: Result := 'BILLING_DUE_DATE';
  else
    Result := 'IMMEDIATE';
  end;
end;

function StringToMateraDynamicQRCodeType(const aString: String): TMateraDynamicQRCodeType;
var
  s: String;
begin
  s := UpperCase(Trim(aString));

  if (s = 'BILLING_DUE_DATE ') then
    Result := mqtBillingDueDate
  else
    Result := mqtImmediate;
end;

{ TMateraReductionCalculation }

procedure TMateraReductionCalculation.AssignSchema(aSource: TACBrPIXSchema);
begin
  inherited AssignSchema(aSource);
end;

procedure TMateraReductionCalculation.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('valuePerc', fvaluePerc)
    .AddPair('modality', fmodality);

end;

procedure TMateraReductionCalculation.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('valuePerc', fvaluePerc)
    .Value('modality', fmodality);

end;

constructor TMateraReductionCalculation.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
end;

procedure TMateraReductionCalculation.Clear;
begin
  fmodality := 0;
  fvaluePerc := 0;
end;

function TMateraReductionCalculation.IsEmpty: Boolean;
begin
  Result := inherited IsEmpty;
end;

procedure TMateraReductionCalculation.Assign(
  aSource: TMateraReductionCalculation);
begin

end;

{ TMateraFinesCalculation }

procedure TMateraFinesCalculation.AssignSchema(aSource: TACBrPIXSchema);
begin
  inherited AssignSchema(aSource);
end;

procedure TMateraFinesCalculation.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('valuePerc', fvaluePerc)
    .AddPair('modality', fmodality);

end;

procedure TMateraFinesCalculation.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('valuePerc', fvaluePerc)
    .Value('modality', fmodality);

end;

constructor TMateraFinesCalculation.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
end;

procedure TMateraFinesCalculation.Clear;
begin
  fmodality := 0;
  fvaluePerc := 0;
end;

function TMateraFinesCalculation.IsEmpty: Boolean;
begin
  Result := inherited IsEmpty;
end;

procedure TMateraFinesCalculation.Assign(aSource: TMateraFinesCalculation);
begin

end;

{ TMateraInterestCalculation }

procedure TMateraInterestCalculation.AssignSchema(aSource: TACBrPIXSchema);
begin
  inherited AssignSchema(aSource);
end;

procedure TMateraInterestCalculation.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('valuePerc', fvaluePerc)
    .AddPair('modality', fmodality);

end;

procedure TMateraInterestCalculation.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('valuePerc', fvaluePerc)
    .Value('modality', fmodality);

end;

constructor TMateraInterestCalculation.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
end;

procedure TMateraInterestCalculation.Clear;
begin
  fmodality := 0;
  fvaluePerc := 0;

end;

function TMateraInterestCalculation.IsEmpty: Boolean;
begin
  Result := inherited IsEmpty;
end;

procedure TMateraInterestCalculation.Assign(aSource: TMateraInterestCalculation
  );
begin

end;

{ TMateraQRCodeDynamicPayerInformationComplete }

procedure TMateraQRCodeDynamicPayerInformationComplete.AssignSchema(
  aSource: TACBrPIXSchema);
begin
  inherited AssignSchema(aSource);
end;

procedure TMateraQRCodeDynamicPayerInformationComplete.DoWriteToJSon(
  aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('cpfCnpj', fcpfCnpj)
    .AddPair('name', fname)
    .AddPair('email', femail, False);
  faddressing.WriteToJSon(aJson);

end;

procedure TMateraQRCodeDynamicPayerInformationComplete.DoReadFromJSon(
  aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('cpfCnpj', fcpfCnpj)
    .Value('name', fname)
    .Value('email', femail);
  faddressing.DoReadFromJSon(aJson);

end;

constructor TMateraQRCodeDynamicPayerInformationComplete.Create(
  const aObjectName: String);
begin
  inherited Create(aObjectName);
  faddressing := TMateraPayerInformationAddressing.Create('addressing');

end;

destructor TMateraQRCodeDynamicPayerInformationComplete.Destroy;
begin
  faddressing.Free;
end;

procedure TMateraQRCodeDynamicPayerInformationComplete.Clear;
begin
  faddressing.Clear;
  fcpfCnpj := EmptyStr;
  femail := EmptyStr;
  fname := EmptyStr;

end;

function TMateraQRCodeDynamicPayerInformationComplete.IsEmpty: Boolean;
begin
  Result := inherited IsEmpty;
end;

procedure TMateraQRCodeDynamicPayerInformationComplete.Assign(
  aSource: TMateraQRCodeDynamicPayerInformationComplete);
begin

end;

{ TMateraFixedDateDiscountArray }

function TMateraFixedDateDiscountArray.GetItem(aIndex: Integer
  ): TMateraFixedDateDiscount;
begin
  Result := TMateraFixedDateDiscount(inherited Items[aIndex]);
end;

procedure TMateraFixedDateDiscountArray.SetItem(aIndex: Integer;
  aValue: TMateraFixedDateDiscount);
begin
  inherited Items[aIndex] := aValue;
end;

function TMateraFixedDateDiscountArray.NewSchema: TACBrPIXSchema;
begin
  Result:=inherited NewSchema;
end;

function TMateraFixedDateDiscountArray.Add(aItem: TMateraFixedDateDiscount
  ): Integer;
begin
  Result := inherited Add(aItem);
end;

procedure TMateraFixedDateDiscountArray.Insert(aIndex: Integer;
  aItem: TMateraFixedDateDiscount);
begin
  inherited Insert(aIndex, aItem);
end;

function TMateraFixedDateDiscountArray.New: TMateraFixedDateDiscount;
begin
  Result := TMateraFixedDateDiscount.Create('');
  Self.Add(Result);
end;

{ TMateraFixedDateDiscount }

procedure TMateraFixedDateDiscount.AssignSchema(aSource: TACBrPIXSchema);
begin
  inherited AssignSchema(aSource);
end;

procedure TMateraFixedDateDiscount.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon.AddPair('valuePerc', fvaluePerc);

  if (fdate <> 0) then
    aJSon.AddPair('date', FormatDateTime('yyyy-mm-dd', fdate));

end;

procedure TMateraFixedDateDiscount.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('valuePerc', fvaluePerc)
    .ValueISODate('date', fdate);
end;

constructor TMateraFixedDateDiscount.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
end;

procedure TMateraFixedDateDiscount.Clear;
begin
  fvaluePerc := 0;
  fdate := 0;

end;

function TMateraFixedDateDiscount.IsEmpty: Boolean;
begin
  Result:=inherited IsEmpty;
end;

procedure TMateraFixedDateDiscount.Assign(aSource: TMateraFixedDateDiscount);
begin

end;

{ TMateraFixedDateDiscountList }

procedure TMateraFixedDateDiscountList.AssignSchema(aSource: TACBrPIXSchema);
begin
  inherited AssignSchema(aSource);
end;

procedure TMateraFixedDateDiscountList.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon.AddPair('modality', fmodality);
  ffixedDateDiscounts.WriteToJSon(aJSon);

end;

procedure TMateraFixedDateDiscountList.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon.Value('modality', fmodality);
  ffixedDateDiscounts.ReadFromJSon(aJSon);

end;

constructor TMateraFixedDateDiscountList.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  ffixedDateDiscounts := TMateraFixedDateDiscountArray.Create('fixedDateDiscounts');

end;

destructor TMateraFixedDateDiscountList.Destroy;
begin
  ffixedDateDiscounts.Free;
end;

procedure TMateraFixedDateDiscountList.Clear;
begin
  fmodality := 0;
  ffixedDateDiscounts.Clear;

end;

function TMateraFixedDateDiscountList.IsEmpty: Boolean;
begin
  Result:=inherited IsEmpty;
end;

procedure TMateraFixedDateDiscountList.Assign(
  aSource: TMateraFixedDateDiscountList);
begin

end;

{ TMateraUniqueDiscount }

procedure TMateraUniqueDiscount.AssignSchema(aSource: TACBrPIXSchema);
begin
  inherited AssignSchema(aSource);
end;

procedure TMateraUniqueDiscount.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('uniqueValuePercDiscount', funiqueValuePercDiscount, False)
    .AddPair('modality', fmodality, False);

end;

procedure TMateraUniqueDiscount.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
  .Value('uniqueValuePercDiscount', funiqueValuePercDiscount)
  .Value('modality', fmodality);

end;

constructor TMateraUniqueDiscount.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
end;

procedure TMateraUniqueDiscount.Clear;
begin
  funiqueValuePercDiscount := 0;
  fmodality := 1;
end;

function TMateraUniqueDiscount.IsEmpty: Boolean;
begin
  Result:= (funiqueValuePercDiscount = 0) and
           (fmodality = 0);
end;

procedure TMateraUniqueDiscount.Assign(aSource: TMateraUniqueDiscount);
begin

end;

function MateraTransactionStatusToString(aType: TMateraTransactionStatus): String;
begin
  case aType of
    mtsCreated: Result := 'CREATED';
    mtsApproved: Result := 'APPROVED';
    mtsRejected: Result := 'REJECTED';
    mtsCanceling: Result := 'CANCELING';
    mtsCanceled: Result := 'CANCELED';
    mtsPartial: Result := 'PARTIAL';
    mtsExpired: Result := 'EXPIRED';
    mtsTimeOut: Result := 'TIMEOUT';
    mtsOverFilled: Result := 'OVERFILLED';
    mtsUnfinished: Result := 'UNFINISHED';
    mtsError: Result := 'ERROR';
    mtsFatalError: Result := 'FATAL_ERROR';
    mtsAuthorized: Result := 'AUTHORIZED';
    mtsCaptured: Result := 'CAPTURED';
  else
    Result := EmptyStr;
  end;
end;

function StringToMateraTransactionStatus(const aString: String): TMateraTransactionStatus;
var
  s: String;
begin
  s := UpperCase(Trim(aString));

  if (s = 'CREATED') then
    Result := mtsCreated
  else if (s = 'APPROVED') then
    Result := mtsApproved
  else if (s = 'REJECTED') then
    Result := mtsRejected
  else if (s = 'CANCELING') then
    Result := mtsCanceling
  else if (s = 'CANCELED') then
    Result := mtsCanceled
  else if (s = 'PARTIAL') then
    Result := mtsPartial
  else if (s = 'EXPIRED') then
    Result := mtsExpired
  else if (s = 'TIMEOUT') then
    Result := mtsTimeOut
  else if (s = 'OVERFILLED') then
    Result := mtsOverFilled
  else if (s = 'UNFINISHED') then
    Result := mtsUnfinished
  else if (s = 'FATAL_ERROR') then
    Result := mtsFatalError
  else if (s = 'AUTHORIZED') then
    Result := mtsAuthorized
  else if (s = 'CAPTURED') then
    Result := mtsCaptured
  else
    Result := mtsError;
end;

{ TMateraTransactionResponseArray }

function TMateraTransactionResponseArray.GetItem(aIndex: Integer): TMateraTransactionResponse;
begin
  Result := TMateraTransactionResponse(inherited Items[aIndex]);
end;

procedure TMateraTransactionResponseArray.SetItem(aIndex: Integer;
  aValue: TMateraTransactionResponse);
begin
  inherited Items[aIndex] := aValue;
end;

function TMateraTransactionResponseArray.NewSchema: TACBrPIXSchema;
begin
  Result := New;
end;

function TMateraTransactionResponseArray.Add(aItem: TMateraTransactionResponse): Integer;
begin
  Result := inherited Add(aItem);
end;

procedure TMateraTransactionResponseArray.Insert(aIndex: Integer;
  aItem: TMateraTransactionResponse);
begin
  inherited Insert(aIndex, aItem);
end;

function TMateraTransactionResponseArray.New: TMateraTransactionResponse;
begin
  Result := TMateraTransactionResponse.Create('');
  Self.Add(Result);
end;

{ TMateraTransactionResponse }

procedure TMateraTransactionResponse.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraTransactionResponse) then
    Assign(TMateraTransactionResponse(aSource));
end;

procedure TMateraTransactionResponse.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('accountHolderId', faccountHolderId)
    .AddPair('accountId', faccountId)
    .AddPair('totalAmount', ftotalAmount)
    .AddPair('transactionDate', ftransactionDate)
    .AddPair('transactionId', ftransactionId)
    .AddPair('transactionStatus', MateraTransactionStatusToString(ftransactionStatus))
    .AddPair('transactionType', MateraTransactionTypeToString(ftransactionType));
end;

procedure TMateraTransactionResponse.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  s1, s2: String;
begin
  {$IFDEF FPC}
  s1 := EmptyStr;
  s2 := EmptyStr;
  {$ENDIF}
  aJSon
    .Value('accountHolderId', faccountHolderId)
    .Value('accountId', faccountId)
    .Value('totalAmount', ftotalAmount)
    .Value('transactionDate', ftransactionDate)
    .Value('transactionId', ftransactionId)
    .Value('transactionStatus', s1)
    .Value('transactionType', s2);
  ftransactionStatus := StringToMateraTransactionStatus(s1);
  ftransactionType := StringToMateraTransactionType(s2);
end;

constructor TMateraTransactionResponse.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  Clear;
end;

procedure TMateraTransactionResponse.Clear;
begin
  faccountHolderId := EmptyStr;
  faccountId := EmptyStr;
  ftotalAmount := 0;
  ftransactionDate := EmptyStr;
  ftransactionId := EmptyStr;
  ftransactionStatus := mtsError;
  ftransactionType := mttInstantPayment;
end;

function TMateraTransactionResponse.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(faccountHolderId) and
    EstaVazio(faccountId) and
    (ftotalAmount = 0) and
    EstaVazio(ftransactionDate) and
    EstaVazio(ftransactionId) and
    (ftransactionStatus = mtsError) and
    (ftransactionType = mttInstantPayment);
end;

procedure TMateraTransactionResponse.Assign(aSource: TMateraTransactionResponse);
begin
  faccountHolderId := aSource.accountHolderId;
  faccountId := aSource.accountId;
  ftotalAmount := aSource.totalAmount;
  ftransactionDate := aSource.transactionDate;
  ftransactionId := aSource.transactionId;
  ftransactionStatus := aSource.transactionStatus;
  ftransactionType := aSource.transactionType;
end;
{ TMateraDiscountsCalculation }

procedure TMateraDiscountsCalculation.AssignSchema(aSource: TACBrPIXSchema);
begin
  inherited AssignSchema(aSource);
end;

procedure TMateraDiscountsCalculation.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  ffixedDateDiscountList.WriteToJSon(aJSon);
  funiqueDiscount.WriteToJSon(aJSon);

end;

procedure TMateraDiscountsCalculation.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  ffixedDateDiscountList.ReadFromJSon(aJSon);
  funiqueDiscount.ReadFromJSon(aJSon);

end;

constructor TMateraDiscountsCalculation.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  ffixedDateDiscountList := TMateraFixedDateDiscountList.Create('fixedDateDiscountList');
  funiqueDiscount := TMateraUniqueDiscount.Create('uniqueDiscount');

end;

destructor TMateraDiscountsCalculation.Destroy;
begin
  ffixedDateDiscountList.Free;
  funiqueDiscount.Free;

end;

procedure TMateraDiscountsCalculation.Clear;
begin
  ffixedDateDiscountList.Clear;
  funiqueDiscount.Clear;
end;

function TMateraDiscountsCalculation.IsEmpty: Boolean;
begin
  Result := inherited IsEmpty;
end;

procedure TMateraDiscountsCalculation.Assign(
  aSource: TMateraDiscountsCalculation);
begin

end;

{ TMateraBillingDueDate }

procedure TMateraBillingDueDate.AssignSchema(aSource: TACBrPIXSchema);
begin
  inherited AssignSchema(aSource);
end;

procedure TMateraBillingDueDate.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  if (fdueDate <> 0) then
    aJSon.AddPair('dueDate', FormatDateTime('yyyy-mm-dd', fdueDate));

  fpayerInformation.WriteToJSon(aJSon);
  finterests.WriteToJSon(aJSon);
  ffines.WriteToJSon(aJSon);
  freduction.WriteToJSon(aJSon);
  fdiscounts.WriteToJSon(aJSon);

  aJSon.AddPair('daysAfterDueDate', fdaysAfterDueDate);

end;

procedure TMateraBillingDueDate.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  inherited DoReadFromJSon(aJSon);
end;

constructor TMateraBillingDueDate.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  fdiscounts := TMateraDiscountsCalculation.Create('discounts');
  ffines := TMateraFinesCalculation.Create('fines');
  finterests := TMateraInterestCalculation.Create('interests');
  fpayerInformation := TMateraQRCodeDynamicPayerInformationComplete.Create('payerInformation');
  freduction := TMateraReductionCalculation.Create('reduction');

end;

destructor TMateraBillingDueDate.Destroy;
begin
  fdiscounts.Free;
  ffines.Free;
  finterests.Free;
  fpayerInformation.Free;
  freduction.Free;

end;

procedure TMateraBillingDueDate.Clear;
begin
  fdaysAfterDueDate := 0;
  fdiscounts.Clear;
  fdueDate := 0;
  ffines.Clear;
  finterests.Clear;
  fpayerInformation.Clear;
  freduction.Clear;

end;

function TMateraBillingDueDate.IsEmpty: Boolean;
begin
  Result := (fdueDate <> 0);
end;

procedure TMateraBillingDueDate.Assign(aSource: TMateraBillingDueDate);
begin
  if (aSource is TMateraBillingDueDate) then
    Assign(TMateraBillingDueDate(aSource));
end;

{ TMateracouponDetails }

procedure TMateracouponDetails.AssignSchema(aSource: TACBrPIXSchema);
begin
  inherited AssignSchema(aSource);
end;

procedure TMateracouponDetails.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('couponId', fcouponId)
    .AddPair('seller', fseller)
    .AddPair('description', fdescription)

end;

procedure TMateracouponDetails.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('couponId', fcouponId)
    .Value('description', fdescription)
    .Value('seller', fseller);
end;

constructor TMateracouponDetails.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
end;

procedure TMateracouponDetails.Clear;
begin
  fcouponId:= EmptyStr;
  fdescription:= EmptyStr;
  fseller:= EmptyStr;

end;

function TMateracouponDetails.IsEmpty: Boolean;
begin
  Result:=inherited IsEmpty;
end;

procedure TMateracouponDetails.Assign(aSource: TMateracouponDetails);
begin

end;

{ TMateraAdditionalInformationArray }

function TMateraAdditionalInformationArray.GetItem(aIndex: Integer): TMateraAdditionalInformation;
begin
  Result := TMateraAdditionalInformation(inherited Items[aIndex]);
end;

procedure TMateraAdditionalInformationArray.SetItem(aIndex: Integer;
  aValue: TMateraAdditionalInformation);
begin
  inherited Items[aIndex] := aValue;
end;

function TMateraAdditionalInformationArray.NewSchema: TACBrPIXSchema;
begin
  Result := New;
end;

function TMateraAdditionalInformationArray.Add(aItem: TMateraAdditionalInformation): Integer;
begin
  Result := inherited Add(aItem);
end;

procedure TMateraAdditionalInformationArray.Insert(aIndex: Integer;
  aItem: TMateraAdditionalInformation);
begin
  inherited Insert(aIndex, aItem);
end;

function TMateraAdditionalInformationArray.New: TMateraAdditionalInformation;
begin
  Result := TMateraAdditionalInformation.Create('');
  Self.Add(Result);
end;

{ TMateraGeneratedImage }

procedure TMateraGeneratedImage.AssignSchema(aSource: TACBrPIXSchema);
begin
  inherited AssignSchema(aSource);
end;

procedure TMateraGeneratedImage.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('imageContent', fimageContent)
    .AddPair('mimeType', fmimeType)
    .AddPair('actualImageWidth', factualImageWidth);

end;

procedure TMateraGeneratedImage.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('actualImageWidth', factualImageWidth)
    .Value('imageContent', fimageContent)
    .Value('mimeType', fmimeType);

end;

constructor TMateraGeneratedImage.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
end;

procedure TMateraGeneratedImage.Clear;
begin
  factualImageWidth := 0;
  fimageContent := EmptyStr;
  fmimeType := EmptyStr;

end;

function TMateraGeneratedImage.IsEmpty: Boolean;
begin
  Result:=inherited IsEmpty;
end;

procedure TMateraGeneratedImage.Assign(aSource: TMateraGeneratedImage);
begin

end;

{ TMateraInstantPaymentQRCodeResponse }

procedure TMateraInstantPaymentQRCodeResponse.AssignSchema(
  aSource: TACBrPIXSchema);
begin
  inherited AssignSchema(aSource);
end;

procedure TMateraInstantPaymentQRCodeResponse.DoWriteToJSon(
  aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('textContent', ftextContent)
    .AddPair('reference', freference)
    .AddPair('qrcodeURL', fqrcodeURL);
  fGeneratedImage.WriteToJSon(aJSon);

  //  fdynamicQrCodeType: string;

end;

procedure TMateraInstantPaymentQRCodeResponse.DoReadFromJSon(
  aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('textContent', ftextContent)
    .Value('reference', freference)
    .Value('qrcodeURL', fqrcodeURL);
  fGeneratedImage.DoReadFromJSon(aJSon);

  //  fdynamicQrCodeType: string;

end;

constructor TMateraInstantPaymentQRCodeResponse.Create(const aObjectName: String
  );
begin
  inherited Create(aObjectName);
  fGeneratedImage := TMateraGeneratedImage.Create('GeneratedImage');
  Clear;
end;

destructor TMateraInstantPaymentQRCodeResponse.Destroy;
begin
  inherited Destroy;
  fGeneratedImage.free;
end;

procedure TMateraInstantPaymentQRCodeResponse.Clear;
begin
  fdynamicQrCodeType := EmptyStr;
  fGeneratedImage.Clear;
  fqrcodeURL := EmptyStr;
  freference := EmptyStr;
  ftextContent := EmptyStr;

end;

function TMateraInstantPaymentQRCodeResponse.IsEmpty: Boolean;
begin
  Result:=inherited IsEmpty;
end;

procedure TMateraInstantPaymentQRCodeResponse.Assign(
  aSource: TMateraInstantPaymentQRCodeResponse);
begin

end;

{ TMateraAuthorizationDetails }

procedure TMateraAuthorizationDetails.AssignSchema(aSource: TACBrPIXSchema);
begin
  inherited AssignSchema(aSource);
end;

procedure TMateraAuthorizationDetails.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon.AddPair('number', fnumber);

end;

procedure TMateraAuthorizationDetails.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon.Value('number', fnumber);
end;

constructor TMateraAuthorizationDetails.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
end;

procedure TMateraAuthorizationDetails.Clear;
begin
  fnumber := EmptyStr;
end;

function TMateraAuthorizationDetails.IsEmpty: Boolean;
begin
  Result:=inherited IsEmpty;
end;

procedure TMateraAuthorizationDetails.Assign(
  aSource: TMateraAuthorizationDetails);
begin
  fnumber := aSource.number;
end;

{ TMaterafinancialStatement }

procedure TMaterafinancialStatement.AssignSchema(aSource: TACBrPIXSchema);
begin
  inherited AssignSchema(aSource);
end;

procedure TMaterafinancialStatement.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon.AddPair('status', fstatus);
  fAuthorizationDetails.WriteToJSon(aJSon);

end;

procedure TMaterafinancialStatement.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  fAuthorizationDetails.DoReadFromJSon(aJSon);
  aJSon
    .Value('status', fstatus);
end;

constructor TMaterafinancialStatement.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  fAuthorizationDetails := TMateraAuthorizationDetails.Create('AuthorizationDetails');
  Clear;
end;

destructor TMaterafinancialStatement.Destroy;
begin
  fAuthorizationDetails.free;
  inherited Destroy;
end;

procedure TMaterafinancialStatement.Clear;
begin
  fAuthorizationDetails.Clear;
  fstatus:= EmptyStr;
end;

function TMaterafinancialStatement.IsEmpty: Boolean;
begin
  Result:=inherited IsEmpty;
end;

procedure TMaterafinancialStatement.Assign(aSource: TMaterafinancialStatement);
begin
  fstatus:=aSource.fstatus;
  fAuthorizationDetails.Assign(aSource.AuthorizationDetails);
end;

{ TMateraQRCodeResponse }

procedure TMateraQRCodeResponse.AssignSchema(aSource: TACBrPIXSchema);
begin
  inherited AssignSchema(aSource);
end;

procedure TMateraQRCodeResponse.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('transactionId',ftransactionId)
    .AddPair('externalIdentifier', fexternalIdentifier)
    .AddPair('senderAccountId', fsenderAccountId)
    .AddPair('creditCardToken', fcreditCardToken);
  ffinancialStatement.WriteToJSon(aJSon);
  aJSon
    .AddPair('boletoUrl', fboletoUrl)
    .AddPair('typeableLine', ftypeableLine)
    .AddPair('dueDate', fdueDate)
    .AddPair('transactionDate', ftransactionDate)
    .AddPair('transactionType', ftransactionType)
    .AddPair('totalAmount', ftotalAmount)
    .AddPair('paidAmount', fpaidAmount)
    .AddPair('discountAmount', fdiscountAmount)
    .AddPair('recipientDescription', frecipientDescription);
  fcouponDetails.WriteToJSon(aJSon);
  aJSon.AddPair('expirationDate', fexpirationDate);
  finstantPayment.WriteToJSon(aJSon);

end;

procedure TMateraQRCodeResponse.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('transactionId', ftransactionId)
    .Value('externalIdentifier', fexternalIdentifier)
    .Value('senderAccountId', fsenderAccountId)
    .Value('creditCardToken', fcreditCardToken);
  ffinancialStatement.ReadFromJSon(aJSon);
  aJSon
    .Value('boletoUrl', fboletoUrl)
    .Value('typeableLine', ftypeableLine);

  if (fdueDate <> 0) then
    aJSon.AddPair('dueDate', FormatDateTime('yyyy-mm-dd', fdueDate));

  if (ftransactionDate <> 0) then
    aJSon.AddPair('transactionDate', FormatDateTime('yyyy-mm-dd', ftransactionDate));

  aJSon
    .Value('transactionType', ftransactionType)
    .Value('totalAmount', ftotalAmount)
    .Value('paidAmount', fpaidAmount)
    .Value('discountAmount', fdiscountAmount)
    .Value('recipientDescription', frecipientDescription);

  fcouponDetails.ReadFromJSon(aJSon);

  if (fexpirationDate <> 0) then
    aJSon.AddPair('expirationDate', FormatDateTime('yyyy-mm-dd', fexpirationDate));

  finstantPayment.ReadFromJSon(aJSon);

end;

constructor TMateraQRCodeResponse.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  ffinancialStatement := TMateraFinancialStatement.Create('financialStatement');
  fcouponDetails := TMateracouponDetails.Create('couponDetails');
  finstantPayment := TMateraInstantPaymentQRCodeResponse.Create('instantPayment');
  Clear;
end;

destructor TMateraQRCodeResponse.Destroy;
begin
  ffinancialStatement.free;
  fcouponDetails.Free;
  finstantPayment.free;
  inherited Destroy;
end;

procedure TMateraQRCodeResponse.Clear;
begin
  fexternalIdentifier:=EmptyStr;
  ffinancialStatement.Clear;
  finstantPayment.Clear;
  fpaidAmount:=0;
  ftotalAmount:=0;
  ftransactionDate:= 0;
  ftransactionId := EmptyStr;
  ftransactionType:= EmptyStr;
end;

function TMateraQRCodeResponse.IsEmpty: Boolean;
begin
  Result:=inherited IsEmpty;
end;

procedure TMateraQRCodeResponse.Assign(aSource: TMateraQRCodeResponse);
begin

end;

{ TMateraAdditionalInformation }

procedure TMateraAdditionalInformation.Setcontent(AValue: String);
begin
  if Fcontent=AValue then Exit;
  Fcontent:=AValue;
end;

procedure TMateraAdditionalInformation.Setname(AValue: String);
begin
  if Fname=AValue then Exit;
  Fname:=AValue;
end;

procedure TMateraAdditionalInformation.SetshowToPlayer(AValue: Boolean);
begin
  if FshowToPlayer=AValue then Exit;
  FshowToPlayer:=AValue;
end;

procedure TMateraAdditionalInformation.AssignSchema(aSource: TACBrPIXSchema);
begin
  inherited AssignSchema(aSource);
end;

procedure TMateraAdditionalInformation.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('name',Fname)
    .AddPair('content',Fcontent)
    .AddPair('showToPayer',FshowToPlayer);
end;

procedure TMateraAdditionalInformation.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  inherited DoReadFromJSon(aJSon);
end;

constructor TMateraAdditionalInformation.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
end;

procedure TMateraAdditionalInformation.Clear;
begin
  Fcontent:= EmptyStr;
  Fname:= EmptyStr;
  FshowToPlayer:= False;
end;

function TMateraAdditionalInformation.IsEmpty: Boolean;
begin
  Result:=inherited IsEmpty;
end;

procedure TMateraAdditionalInformation.Assign(aSource: TMateraAdditionalInformation);
begin

end;

{ TMateraRecipientsArray }

function TMateraRecipientsArray.GetItem(aIndex: Integer): TMateraRecipient;
begin
  Result := TMateraRecipient(inherited Items[aIndex]);
end;

procedure TMateraRecipientsArray.SetItem(aIndex: Integer;
  aValue: TMateraRecipient);
begin
  inherited Items[aIndex] := aValue;
end;

function TMateraRecipientsArray.NewSchema: TACBrPIXSchema;
begin
  Result:=inherited NewSchema;
end;

function TMateraRecipientsArray.Add(aItem: TMateraRecipient): Integer;
begin
  Result := inherited Add(aItem);
end;

procedure TMateraRecipientsArray.Insert(aIndex: Integer; aItem: TMateraRecipient
  );
begin
  inherited Insert(aIndex, aItem);
end;

function TMateraRecipientsArray.New: TMateraRecipient;
begin
  Result := TMateraRecipient.Create('');
  Self.Add(Result);
end;

{ TMateraPaymentInfo }

procedure TMateraPaymentInfo.AssignSchema(aSource: TACBrPIXSchema);
begin
  inherited AssignSchema(aSource);
end;

procedure TMateraPaymentInfo.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('transactionType', FtransactionType);
  FinstantPayment.WriteToJSon(aJSon);

end;

procedure TMateraPaymentInfo.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  inherited DoReadFromJSon(aJSon);
end;

constructor TMateraPaymentInfo.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  FinstantPayment := TMateraInstantPayment.Create('instantPayment');
end;

destructor TMateraPaymentInfo.Destroy;
begin
  FinstantPayment.Free;
  inherited Destroy;
end;

procedure TMateraPaymentInfo.Clear;
begin
  FinstantPayment.clear;
  FtransactionType:= EmptyStr;
end;

function TMateraPaymentInfo.IsEmpty: Boolean;
begin
  Result:=inherited IsEmpty;
end;

procedure TMateraPaymentInfo.Assign(aSource: TMateraPaymentInfo);
begin

end;

{ TMateraInstantPayment }

procedure TMateraInstantPayment.AssignSchema(aSource: TACBrPIXSchema);
begin
  inherited AssignSchema(aSource);
end;

procedure TMateraInstantPayment.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon.AddPair('alias',Falias_.name);

  if fexpiration <> 0 then
    aJSon.AddPair('expiration',Fexpiration);

  FqrCodeImageGenerationSpecification.WriteToJSon(aJSon);

  aJSon.AddPair('dynamicQRCodeType', MateraDynamicQRCodeTypeToString(fdynamicQRCodeType));

  if (not fbillingDueDate.IsEmpty) then
    fbillingDueDate.WriteToJSon(aJSon);

  FadditionalInformation.WriteToJSon(aJSon);
end;

procedure TMateraInstantPayment.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  inherited DoReadFromJSon(aJSon);
end;

constructor TMateraInstantPayment.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  FadditionalInformation := TMateraAdditionalInformationArray.Create('additionalInformation');
  Falias_ := TMateraAlias.Create('alias');
  FqrCodeImageGenerationSpecification := TMateraQRCodeSpecification.Create('qrCodeImageGenerationSpecification');
  fbillingDueDate := TMateraBillingDueDate.Create('billingDueDate');

end;

destructor TMateraInstantPayment.Destroy;
begin
  FadditionalInformation.Free;
  Falias_.Free;
  FqrCodeImageGenerationSpecification.Free;
  fbillingDueDate.free;
  inherited Destroy;
end;

procedure TMateraInstantPayment.Clear;
begin
  FadditionalInformation.Clear;
  Falias_.Clear;
  Fexpiration:= 0;
  FqrCodeImageGenerationSpecification.Clear;
  fbillingDueDate.Clear;
end;

function TMateraInstantPayment.IsEmpty: Boolean;
begin
  Result:=inherited IsEmpty;
end;

procedure TMateraInstantPayment.Assign(aSource: TMateraInstantPayment);
begin

end;

{ TMateraRecipient }

procedure TMateraRecipient.AssignSchema(aSource: TACBrPIXSchema);
begin
  inherited AssignSchema(aSource);
end;

procedure TMateraRecipient.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  Faccount.WriteToJSon(aJSon);
  aJSon
    .AddPair('amount', Famount)
    .AddPair('currency',Fcurrency)
    .AddPair('mediatorFee', Fmediatorfee)
    .AddPair('recipientComment', FrecipientComment);
end;

procedure TMateraRecipient.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  inherited DoReadFromJSon(aJSon);
end;

constructor TMateraRecipient.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  Faccount := TMateraAccount.Create('account');
end;

destructor TMateraRecipient.Destroy;
begin
  Faccount.Free;
  inherited Destroy;
end;

procedure TMateraRecipient.Clear;
begin
  Faccount.Clear;
  Famount:= 0;
  Fcurrency:= EmptyStr;
  Fmediatorfee:= 0;
  FrecipientComment:= EmptyStr;
end;

function TMateraRecipient.IsEmpty: Boolean;
begin
  Result:=inherited IsEmpty;
end;

procedure TMateraRecipient.Assign(aSource: TMateraAlias);
begin

end;

{ TMateraQRCodeRequest }

procedure TMateraQRCodeRequest.SettotalAmount(AValue: Currency);
begin
  if FtotalAmount=AValue then Exit;
  FtotalAmount:=AValue;
end;

procedure TMateraQRCodeRequest.Setcurrency(AValue: string);
begin
  if Fcurrency=AValue then Exit;
  Fcurrency:=AValue;
end;

procedure TMateraQRCodeRequest.Setrecipients(AValue: TMateraRecipientsArray);
begin
  if Frecipients=AValue then Exit;
  Frecipients:=AValue;
end;

procedure TMateraQRCodeRequest.SetcallbackAddress(AValue: string);
begin
  if FcallbackAddress=AValue then Exit;
  FcallbackAddress:=AValue;
end;

procedure TMateraQRCodeRequest.AssignSchema(aSource: TACBrPIXSchema);
begin
  inherited AssignSchema(aSource);
end;

procedure TMateraQRCodeRequest.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('externalIdentifier', fexternalIdentifier)
    .AddPair('totalAmount',ftotalAmount)
    .AddPair('currency',fcurrency);
  FpaymentInfo.WriteToJSon(aJSon);
  Frecipients.WriteToJSon(aJSon);
  aJSon
    .AddPair('callbackAddress',FcallbackAddress);
end;

procedure TMateraQRCodeRequest.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  inherited DoReadFromJSon(aJSon);
end;

constructor TMateraQRCodeRequest.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  FpaymentInfo := TMateraPaymentInfo.Create('paymentInfo');
  Frecipients := TMateraRecipientsArray.Create('recipients');

end;

destructor TMateraQRCodeRequest.Destroy;
begin
  FpaymentInfo.Free;
  Frecipients.Free;
  inherited Destroy;
end;

procedure TMateraQRCodeRequest.Clear;
begin
  if Assigned(falias) then
    falias.clear;
  FcallbackAddress:= EmptyStr;
  fcurrency:= EmptyStr;
  fexternalIdentifier:= EmptyStr;
  if Assigned(FpaymentInfo) then
    FpaymentInfo.Clear;
  if Assigned(Frecipients) then
    Frecipients.Clear;
  ftotalAmount:= 0;
end;

function TMateraQRCodeRequest.IsEmpty: Boolean;
begin
  Result:=inherited IsEmpty;
end;

procedure TMateraQRCodeRequest.Assign(aSource: TMateraAliasRequest);
begin

end;

{ TMateraAlias }

procedure TMateraAlias.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraAlias) then
    Assign(TMateraAlias(aSource));
end;

procedure TMateraAlias.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  inherited DoWriteToJSon(aJSon);
  aJSon.AddPair('status', MateraAliasStatusToString(fstatus));
end;

procedure TMateraAlias.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  s: String;
begin
  {$IFDEF FPC}
  s := EmptyStr;
  {$ENDIF}

  inherited DoReadFromJSon(aJSon);
  aJSon.Value('status', s);
  fstatus := StringToMateraAliasStatus(s);
end;

procedure TMateraAlias.Clear;
begin
  inherited Clear;
  fstatus := mastIdentityValidationPending;
end;

function TMateraAlias.IsEmpty: Boolean;
begin
  Result := (inherited IsEmpty) and (fstatus = mastIdentityValidationPending);
end;

procedure TMateraAlias.Assign(aSource: TMateraAlias);
begin
  inherited Assign(aSource);
  fstatus := aSource.status;
end;

{ TMateraAliasArray }

function TMateraAliasArray.GetItem(aIndex: Integer): TMateraAlias;
begin
  Result := TMateraAlias(inherited Items[aIndex]);
end;

procedure TMateraAliasArray.SetItem(aIndex: Integer; aValue: TMateraAlias);
begin
  inherited Items[aIndex] := aValue;
end;

function TMateraAliasArray.NewSchema: TACBrPIXSchema;
begin
  Result := New;
end;

function TMateraAliasArray.Add(aItem: TMateraAlias): Integer;
begin
  Result := inherited Add(aItem);
end;

procedure TMateraAliasArray.Insert(aIndex: Integer; aItem: TMateraAlias);
begin
  inherited Insert(aIndex, aItem);
end;

function TMateraAliasArray.New: TMateraAlias;
begin
  Result := TMateraAlias.Create('');
  Self.Add(Result);
end;

{ TMateraPayerInformation }

procedure TMateraPayerInformation.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraPayerInformation) then
    Assign(TMateraPayerInformation(aSource));
end;

procedure TMateraPayerInformation.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('cpfCnpj', fcpfCnpj)
    .AddPair('name', fname)
    .AddPair('email', femail);
  faddressing.WriteToJSon(aJSon);
end;

procedure TMateraPayerInformation.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('cpfCnpj', fcpfCnpj)
    .Value('name', fname)
    .Value('email', femail);
  faddressing.ReadFromJSon(aJSon);
end;

constructor TMateraPayerInformation.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  faddressing := TMateraPayerInformationAddressing.Create('addressing');
  Clear;
end;

procedure TMateraPayerInformation.Clear;
begin
  fcpfCnpj := EmptyStr;
  fname := EmptyStr;
  femail := EmptyStr;
  faddressing.Clear;
end;

function TMateraPayerInformation.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(fcpfCnpj) and
    EstaVazio(fname) and
    EstaVazio(femail) and
    faddressing.IsEmpty;
end;

procedure TMateraPayerInformation.Assign(aSource: TMateraPayerInformation);
begin
  fcpfCnpj := aSource.cpfCnpj;
  fname := aSource.name;
  femail := aSource.email;
  faddressing.Assign(aSource.addressing);
end;

{ TMateraPayerInformationAddressing }

procedure TMateraPayerInformationAddressing.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraPayerInformationAddressing) then
    Assign(TMateraPayerInformationAddressing(aSource));
end;

procedure TMateraPayerInformationAddressing.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('street', fstreet)
    .AddPair('city', fcity)
    .AddPair('uf', fuf)
    .AddPair('cep', fcep);
end;

procedure TMateraPayerInformationAddressing.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('street', fstreet)
    .Value('city', fcity)
    .Value('uf', fuf)
    .Value('cep', fcep);
end;

constructor TMateraPayerInformationAddressing.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  Clear;
end;

procedure TMateraPayerInformationAddressing.Clear;
begin
  fstreet := EmptyStr;
  fcity := EmptyStr;
  fuf := EmptyStr;
  fcep := EmptyStr;
end;

function TMateraPayerInformationAddressing.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(fstreet) and
    EstaVazio(fcity) and
    EstaVazio(fuf) and
    EstaVazio(fcep);
end;

procedure TMateraPayerInformationAddressing.Assign(aSource: TMateraPayerInformationAddressing);
begin
  fstreet := aSource.street;
  fcity := aSource.city;
  fuf := aSource.uf;
  fcep := aSource.cep;
end;

{ TMateraQRCodeSpecification }

procedure TMateraQRCodeSpecification.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraQRCodeSpecification) then
    Assign(TMateraQRCodeSpecification(aSource));
end;

procedure TMateraQRCodeSpecification.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('errorCorrectionLevel', ferrorCorrectionLevel)
    .AddPair('imageWidth', fimageWidth)
    .AddPair('generateImageRendering', fgenerateImageRendering);
end;

procedure TMateraQRCodeSpecification.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('errorCorrectionLevel', ferrorCorrectionLevel)
    .Value('imageWidth', fimageWidth)
    .Value('generateImageRendering', fgenerateImageRendering);
end;

constructor TMateraQRCodeSpecification.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  Clear;
end;

procedure TMateraQRCodeSpecification.Clear;
begin
  ferrorCorrectionLevel := EmptyStr;
  fimageWidth := 0;
  fgenerateImageRendering := False;
end;

function TMateraQRCodeSpecification.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(ferrorCorrectionLevel) and
    (fimageWidth = 0) and
    (not fgenerateImageRendering);
end;

procedure TMateraQRCodeSpecification.Assign(aSource: TMateraQRCodeSpecification);
begin
  ferrorCorrectionLevel := aSource.errorCorrectionLevel;
  fimageWidth := aSource.imageWidth;
  fgenerateImageRendering := aSource.generateImageRendering;
end;

{ TMateraRegisterAliasResponse }

procedure TMateraRegisterAliasResponse.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraRegisterAliasResponse) then
    Assign(TMateraRegisterAliasResponse(aSource));
end;

procedure TMateraRegisterAliasResponse.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('protocolId', fprotocolId, False)
    .AddPair('needsIdentifyConfirmation', fneedsIdentifyConfirmation);
  falias.WriteToJSon(aJSon);
end;

procedure TMateraRegisterAliasResponse.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('protocolId', fprotocolId)
    .Value('needsIdentifyConfirmation', fneedsIdentifyConfirmation);
  falias.ReadFromJSon(aJSon);
end;

constructor TMateraRegisterAliasResponse.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  falias := TMateraAlias.Create('alias');
  Clear;
end;

procedure TMateraRegisterAliasResponse.Clear;
begin
  fprotocolId := EmptyStr;
  fneedsIdentifyConfirmation := False;
  falias.Clear;
end;

function TMateraRegisterAliasResponse.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(fprotocolId) and
    (not fneedsIdentifyConfirmation) and
    falias.IsEmpty;
end;

procedure TMateraRegisterAliasResponse.Assign(aSource: TMateraRegisterAliasResponse);
begin
  fprotocolId := aSource.protocolId;
  fneedsIdentifyConfirmation := aSource.needsIdentifyConfirmation;
  falias.Assign(aSource.alias_);
end;

{ TMateraAliasBasic }

procedure TMateraAliasBasic.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraAliasBasic) then
    Assign(TMateraAliasBasic(aSource));
end;

procedure TMateraAliasBasic.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('name', fname, False)
    .AddPair('type', MateraAliasTypeToString(ftype));
end;

procedure TMateraAliasBasic.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  s: String;
begin
  {$IfDef FPC}
  s := EmptyStr;
  {$EndIf}

  aJSon
    .Value('name', fname)
    .Value('type', s);

  ftype := StringToMateraAliasType(s);
end;

constructor TMateraAliasBasic.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  Clear;
end;

procedure TMateraAliasBasic.Clear;
begin
  fname := EmptyStr;
  ftype := malEVP;
end;

function TMateraAliasBasic.IsEmpty: Boolean;
begin
  Result := False;
end;

procedure TMateraAliasBasic.Assign(aSource: TMateraAliasBasic);
begin
  fname := aSource.name;
  ftype := aSource.type_;
end;

{ TMateraAliasRequest }

procedure TMateraAliasRequest.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraAliasRequest) then
    Assign(TMateraAliasRequest(aSource));
end;

procedure TMateraAliasRequest.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon.AddPair('externalIdentifier', fexternalIdentifier);
  falias.WriteToJSon(aJSon);
end;

procedure TMateraAliasRequest.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon.Value('externalIdentifier', fexternalIdentifier);
  falias.ReadFromJSon(aJSon);
end;

constructor TMateraAliasRequest.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  falias := TMateraAliasBasic.Create('alias');
  Clear;
end;

procedure TMateraAliasRequest.Clear;
begin
  fexternalIdentifier := EmptyStr;
  falias.Clear;
end;

function TMateraAliasRequest.IsEmpty: Boolean;
begin
  Result := EstaVazio(fexternalIdentifier);
end;

procedure TMateraAliasRequest.Assign(aSource: TMateraAliasRequest);
begin
  fexternalIdentifier := aSource.externalIdentifier;
  falias.Assign(aSource.alias_);
end;

{ TMateraAccountTransactionRequest }

procedure TMateraAccountTransactionRequest.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraAccountTransactionRequest) then
    Assign(TMateraAccountTransactionRequest(aSource));
end;

procedure TMateraAccountTransactionRequest.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  inherited DoWriteToJSon(aJSon);
  fclient.WriteToJSon(aJSon);
  frepresentative.WriteToJSon(aJSon);
end;

procedure TMateraAccountTransactionRequest.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  inherited DoReadFromJSon(aJSon);
  fclient.ReadFromJSon(aJSon);
  frepresentative.ReadFromJSon(aJSon);
end;

constructor TMateraAccountTransactionRequest.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  fclient := TMateraClient.Create('client');
  frepresentative := TMateraClient.Create('representative');
end;

destructor TMateraAccountTransactionRequest.Destroy;
begin
  fclient.Free;
  frepresentative.Free;
  inherited Destroy;
end;

procedure TMateraAccountTransactionRequest.Clear;
begin
  fclient.Clear;
  frepresentative.Clear;
  inherited Clear;
end;

function TMateraAccountTransactionRequest.IsEmpty: Boolean;
begin
  Result := inherited IsEmpty and
    fclient.IsEmpty and
    frepresentative.IsEmpty;
end;

procedure TMateraAccountTransactionRequest.Assign(
  aSource: TMateraAccountTransactionRequest);
begin
  inherited Assign(aSource);
  fclient.Assign(aSource.client);
  frepresentative.Assign(aSource.representative);
end;

{ TMateraCreateAccountTransactionRequest }

procedure TMateraCreateAccountTransactionRequest.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraCreateAccountTransactionRequest) then
    Assign(TMateraCreateAccountTransactionRequest(aSource));
end;

procedure TMateraCreateAccountTransactionRequest.DoWriteToJSon(
  aJSon: TACBrJSONObject);
begin
  inherited DoWriteToJSon(aJSon);

  aJSon
    .AddPair('checkingAccountBranch', fcheckingAccountBranch, False)
    .AddPair('checkingAccountNumber', fcheckingAccountNumber, False)
    .AddPair('customData', fcustomData, False);

  fclient.WriteToJSon(aJSon);
end;

procedure TMateraCreateAccountTransactionRequest.DoReadFromJSon(
  aJSon: TACBrJSONObject);
begin
  inherited DoReadFromJSon(aJSon);
  aJSon
    .Value('checkingAccountBranch', fcheckingAccountBranch)
    .Value('checkingAccountNumber', fcheckingAccountNumber)
    .Value('customData', fcustomData);

  fclient.ReadFromJSon(aJSon);
end;

constructor TMateraCreateAccountTransactionRequest.Create(
  const aObjectName: String);
begin
  inherited Create(aObjectName);
  fclient := TMateraBasicClient.Create('client');
end;

destructor TMateraCreateAccountTransactionRequest.Destroy;
begin
  fclient.Free;
  inherited Destroy;
end;

procedure TMateraCreateAccountTransactionRequest.Clear;
begin
  inherited Clear;
  fcheckingAccountBranch := 0;
  fcheckingAccountNumber := 0;
  faccountType := matOrdinary;
  fcustomData := EmptyStr;

  if Assigned(fclient) then
    fclient.Clear;
end;

function TMateraCreateAccountTransactionRequest.IsEmpty: Boolean;
begin
  Result := inherited IsEmpty and
    (fcheckingAccountBranch = 0) and
    (fcheckingAccountNumber = 0) and
    EstaVazio(fcustomData) and
    fclient.IsEmpty;;
end;

procedure TMateraCreateAccountTransactionRequest.Assign(aSource: TMateraCreateAccountTransactionRequest);
begin
  inherited Assign(aSource);
  fcustomData := aSource.customData;
  fcheckingAccountBranch := aSource.checkingAccountBranch;
  fcheckingAccountNumber := aSource.checkingAccountNumber;
  fclient.Assign(aSource.client);
end;

{ TMateraAccountQueryResponse }

procedure TMateraAccountQueryResponse.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraAccountQueryResponse) then
    Assign(TMateraAccountQueryResponse(aSource));
end;

procedure TMateraAccountQueryResponse.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  faccounts.WriteToJSon(aJSon);
  faccountHolder.WriteToJSon(aJSon);
end;

procedure TMateraAccountQueryResponse.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  faccounts.ReadFromJSon(aJSon);
  faccountHolder.ReadFromJSon(aJSon);
end;

constructor TMateraAccountQueryResponse.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  faccounts := TMateraAccountIdentifierArray.Create('');
  accountHolder := TMateraAccountHolderResponse.Create('accountHolder');
  Clear;
end;

procedure TMateraAccountQueryResponse.Clear;
begin
  faccounts.Clear;
  faccountHolder.Clear;
end;

function TMateraAccountQueryResponse.IsEmpty: Boolean;
begin
  Result :=
    faccounts.IsEmpty and
    faccountHolder.IsEmpty;
end;

procedure TMateraAccountQueryResponse.Assign(
  aSource: TMateraAccountQueryResponse);
begin
  faccounts.Assign(aSource.accounts);
  faccountHolder.Assign(aSource.accountHolder);
end;

{ TMateraAccountHolderResponse }

procedure TMateraAccountHolderResponse.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraAccountHolderResponse) then
    Assign(TMateraAccountHolderResponse(aSource));
end;

procedure TMateraAccountHolderResponse.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('accountHolderId', faccountHolderId)
    .AddPair('email', femail)
    .AddPair('name', fname)
    .AddPair('representativeId', frepresentativeId)
    .AddPair('socialName', fsocialName)
    .AddPair('status', MateraActiveStatusToString(fstatus));

  fbillingAddress.WriteToJSon(aJSon);
  fmailAddress.WriteToJSon(aJSon);
  fmobilePhone.WriteToJSon(aJSon);
  fmobilePhones.WriteToJSon(aJSon);
  frepresentatives.WriteToJSon(aJSon);
  ftaxIdentifier.WriteToJSon(aJSon);
end;

procedure TMateraAccountHolderResponse.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  s: String;
begin
  aJSon
    .Value('accountHolderId', faccountHolderId)
    .Value('email', femail)
    .Value('name', fname)
    .Value('representativeId', frepresentativeId)
    .Value('socialName', fsocialName)
    .Value('status', s);

  fstatus := StringToMateraActiveStatus(s);
  fbillingAddress.ReadFromJSon(aJSon);
  fmailAddress.ReadFromJSon(aJSon);
  fmobilePhone.ReadFromJSon(aJSon);
  fmobilePhones.ReadFromJSon(aJSon);
  frepresentatives.ReadFromJSon(aJSon);
  ftaxIdentifier.ReadFromJSon(aJSon);
end;

constructor TMateraAccountHolderResponse.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  fbillingAddress := TMateraEndereco.Create('billingAddress');
  fmailAddress := TMateraEndereco.Create('mailAddress');
  fmobilePhone := TMateraMobilePhone.Create('mobilePhone');
  fmobilePhones := TMateraMobilePhoneArray.Create('mobilePhones');
  frepresentatives := TMateraClientRepresentativeArray.Create('representatives');
  ftaxIdentifier := TMateraTaxIdentifier.Create('taxIdentifier');
  Clear;
end;

procedure TMateraAccountHolderResponse.Clear;
begin
  fname := EmptyStr;
  femail := EmptyStr;
  fsocialName := EmptyStr;
  faccountHolderId := EmptyStr;
  frepresentativeId := EmptyStr;
  fstatus := macInactive;

  fmailAddress.Clear;
  fmobilePhone.Clear;
  fmobilePhones.Clear;
  ftaxIdentifier.Clear;
  fbillingAddress.Clear;
  frepresentatives.Clear;
end;

function TMateraAccountHolderResponse.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(fname) and
    EstaVazio(femail) and
    EstaVazio(fsocialName) and
    EstaVazio(faccountHolderId) and
    EstaVazio(frepresentativeId) and
    (fstatus = macInactive) and
    fbillingAddress.IsEmpty and
    fmailAddress.IsEmpty and
    fmobilePhone.IsEmpty and
    fmobilePhones.IsEmpty and
    frepresentatives.IsEmpty and
    ftaxIdentifier.IsEmpty;
end;

procedure TMateraAccountHolderResponse.Assign(aSource: TMateraAccountHolderResponse);
begin
  fname := aSource.name;
  femail := aSource.email;
  fsocialName := aSource.socialName;
  faccountHolderId := aSource.accountHolderId;
  frepresentativeId := aSource.representativeId;
  fstatus := aSource.status;

  fmailAddress.Assign(aSource.mailAddress);
  fmobilePhone.Assign(aSource.mobilePhone);
  fmobilePhones.Assign(aSource.mobilePhones);
  ftaxIdentifier.Assign(aSource.taxIdentifier);
  fbillingAddress.Assign(aSource.billingAddress);
  frepresentatives.Assign(aSource.representatives);
end;

{ TMateraMobilePhoneArray }

function TMateraMobilePhoneArray.GetItem(aIndex: Integer): TMateraMobilePhone;
begin
  Result := TMateraMobilePhone(inherited Items[aIndex]);
end;

procedure TMateraMobilePhoneArray.SetItem(aIndex: Integer; aValue: TMateraMobilePhone);
begin
  inherited Items[aIndex] := aValue;
end;

function TMateraMobilePhoneArray.NewSchema: TACBrPIXSchema;
begin
  Result := New;
end;

function TMateraMobilePhoneArray.Add(aItem: TMateraMobilePhone): Integer;
begin
  Result := inherited Add(aItem);
end;

procedure TMateraMobilePhoneArray.Insert(aIndex: Integer; aItem: TMateraMobilePhone);
begin
  inherited Insert(aIndex, aItem);
end;

function TMateraMobilePhoneArray.New: TMateraMobilePhone;
begin
  Result := TMateraMobilePhone.Create('');
  Self.Add(Result);
end;

{ TMateraAccountIdentifierArray }

function TMateraAccountIdentifierArray.GetItem(aIndex: Integer): TMateraAccountIdentifier;
begin
  Result := TMateraAccountIdentifier(inherited Items[aIndex]);
end;

procedure TMateraAccountIdentifierArray.SetItem(aIndex: Integer; aValue: TMateraAccountIdentifier);
begin
  inherited Items[aIndex] := aValue;
end;

function TMateraAccountIdentifierArray.NewSchema: TACBrPIXSchema;
begin
  Result := New;
end;

function TMateraAccountIdentifierArray.Add(aItem: TMateraAccountIdentifier): Integer;
begin
  Result := inherited Add(aItem);
end;

procedure TMateraAccountIdentifierArray.Insert(aIndex: Integer; aItem: TMateraAccountIdentifier);
begin
  inherited Insert(aIndex, aItem);
end;

function TMateraAccountIdentifierArray.New: TMateraAccountIdentifier;
begin
  Result := TMateraAccountIdentifier.Create('');
  Self.Add(Result);
end;

{ TMateraAccountIdentifier }

procedure TMateraAccountIdentifier.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraAccountIdentifier) then
    Assign(TMateraAccountIdentifier(aSource));
end;

procedure TMateraAccountIdentifier.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  inherited DoWriteToJSon(aJSon);
  aJSon.AddPair('accountType', MateraAccountTypeToString(faccountType));
end;

procedure TMateraAccountIdentifier.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  s: String;
begin
  inherited DoReadFromJSon(aJSon);
  aJSon.Value('accountType', s);
  faccountType := StringToMateraAccountType(s);
end;

procedure TMateraAccountIdentifier.Clear;
begin
  inherited Clear;
  faccountType := matOrdinary;
end;

function TMateraAccountIdentifier.IsEmpty: Boolean;
begin
  Result := inherited IsEmpty and (faccountType = matOrdinary);
end;

procedure TMateraAccountIdentifier.Assign(aSource: TMateraAccountIdentifier);
begin
  inherited Assign(aSource);
  faccountType := aSource.accountType;
end;

{ TMateraError }

procedure TMateraError.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraError) then
    Assign(TMateraError(aSource));
end;

procedure TMateraError.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('code', fcode)
    .AddPair('description', fdescription);
end;

procedure TMateraError.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('code', fcode)
    .Value('description', fdescription);
end;

constructor TMateraError.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  Clear;
end;

procedure TMateraError.Clear;
begin
  fcode := EmptyStr;
  fdescription := EmptyStr;
end;

function TMateraError.IsEmpty: Boolean;
begin
  Result := EstaVazio(fcode) and EstaVazio(fdescription);
end;

procedure TMateraError.Assign(aSource: TMateraError);
begin
  fcode := aSource.code;
  fdescription := aSource.description;
end;

{ TMateraAccountResponse }

procedure TMateraAccountResponse.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraAccountResponse) then
    Assign(TMateraAccountResponse(aSource));
end;

procedure TMateraAccountResponse.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('accountHolderId', faccountHolderId)
    .AddPair('mediatorId', fmediatorId, False)
    .AddPair('accountInternalTypeId', faccountInternalTypeId, False)
    .AddPair('accountStatus', MateraAccountStatusToString(faccountStatus))
    .AddPair('clientType', MateraClientTypeToString(fclientType));

  if (fdueDate <> 0) then
    aJSon.AddPair('dueDate', FormatDateTime('yyyy-mm-dd', fdueDate));

  frates.WriteToJSon(aJSon);
  fclient.WriteToJSon(aJSon);
  faccount.WriteToJSon(aJSon);
  frepresentative.WriteToJSon(aJSon);
  fbillingAddress.WriteToJSon(aJSon);
  ffinancialLimit.WriteToJSon(aJSon);
  fadditionalDetailsCorporate.WriteToJSon(aJSon);
  fadditionalDetailsForeigner.WriteToJSon(aJSon);
  fadditionalDetailsPerson.WriteToJSon(aJSon);
end;

procedure TMateraAccountResponse.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  s1, s2, w: String;
begin
  {$IfDef FPC}
  s1 := EmptyStr;
  s2 := EmptyStr;
  {$EndIf}

  aJSon
    .Value('accountHolderId', faccountHolderId)
    .Value('mediatorId', fmediatorId)
    .Value('accountInternalTypeId', faccountInternalTypeId)
    .Value('clientType', s1)
    .Value('accountStatus', s2)
    .ValueISODate('dueDate', fdueDate);

  w := aJSon.ToJSON;

  fclientType := StringToMateraClientType(s1);
  faccountStatus := StringToMateraAccountStatus(s2);
  frates.ReadFromJSon(aJSon);
  fclient.ReadFromJSon(aJSon);
  faccount.ReadFromJSon(aJSon);
  frepresentative.ReadFromJSon(aJSon);
  fbillingAddress.ReadFromJSon(aJSon);
  ffinancialLimit.ReadFromJSon(aJSon);
  fadditionalDetailsCorporate.ReadFromJSon(aJSon);
  fadditionalDetailsForeigner.ReadFromJSon(aJSon);
  fadditionalDetailsPerson.ReadFromJSon(aJSon);
end;

constructor TMateraAccountResponse.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);

  frates := TMateraRates.Create('rates');
  fclient := TMateraClient.Create('client');
  faccount := TMateraAccount.Create('account');
  frepresentative := TMateraClient.Create('representative');
  fbillingAddress := TMateraEndereco.Create('billingAddress');
  ffinancialLimit := TMateraFinancialLimit.Create('financialLimit');
  fadditionalDetailsCorporate := TMateraAdditionalDetailsCorporate.Create('additionalDetailsCorporate');
  fadditionalDetailsForeigner := TMateraAdditionalDetailsBasic.Create('additionalDetailsForeigner');
  fadditionalDetailsPerson := TMateraAdditionalDetailsPerson.Create('additionalDetailsPerson');
  Clear;
end;

destructor TMateraAccountResponse.Destroy;
begin
  frates.Free;
  fclient.Free;
  faccount.Free;
  frepresentative.Free;
  fbillingAddress.Free;
  ffinancialLimit.Free;
  fadditionalDetailsCorporate.Free;
  fadditionalDetailsForeigner.Free;
  fadditionalDetailsPerson.Free;
  inherited Destroy;
end;

procedure TMateraAccountResponse.Clear;
begin
  fdueDate := 0;
  faccountInternalTypeId := 0;
  fmediatorId := EmptyStr;
  faccountHolderId := EmptyStr;

  frates.Clear;
  fclient.Clear;
  faccount.Clear;
  frepresentative.Clear;
  fbillingAddress.Clear;
  ffinancialLimit.Clear;
  fadditionalDetailsCorporate.Clear;
  fadditionalDetailsForeigner.Clear;
  fadditionalDetailsPerson.Clear;
end;

function TMateraAccountResponse.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(fmediatorId) and
    EstaVazio(faccountHolderId) and
    (dueDate = 0) and
    (accountInternalTypeId = 0) and
    frates.IsEmpty and
    fclient.IsEmpty and
    faccount.IsEmpty and
    frepresentative.IsEmpty and
    fbillingAddress.IsEmpty and
    ffinancialLimit.IsEmpty and
    fadditionalDetailsCorporate.IsEmpty and
    fadditionalDetailsForeigner.IsEmpty and
    fadditionalDetailsPerson.IsEmpty;
end;

procedure TMateraAccountResponse.Assign(aSource: TMateraAccountResponse);
begin
  fdueDate := aSource.dueDate;
  faccountInternalTypeId := aSource.accountInternalTypeId;
  fmediatorId := aSource.mediatorId;
  faccountHolderId := aSource.accountHolderId;

  frates.Assign(aSource.rates);
  fclient.Assign(aSource.client);
  faccount.Assign(aSource.account);
  frepresentative.Assign(aSource.representative);
  fbillingAddress.Assign(aSource.billingAddress);
  ffinancialLimit.Assign(aSource.financialLimit);
  fadditionalDetailsCorporate.Assign(aSource.additionalDetailsCorporate);
  fadditionalDetailsForeigner.Assign(aSource.additionalDetailsForeigner);
  fadditionalDetailsPerson.Assign(aSource.additionalDetailsPerson);
end;

{ TMateraFinancialLimit }

procedure TMateraFinancialLimit.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraFinancialLimit) then
    Assign(TMateraFinancialLimit(aSource));
end;

procedure TMateraFinancialLimit.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('realBalanceLimit', frealBalanceLimit)
    .AddPair('monthlyFinancialInjectionLimit', fmonthlyFinancialInjectionLimit)
    .AddPair('realBalance', frealBalance)
    .AddPair('maxCreditLimit', fmaxCreditLimit)
    .AddPair('currentMonthlyFinancialInjection', fcurrentMonthlyFinancialInjection);
end;

procedure TMateraFinancialLimit.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('realBalanceLimit', frealBalanceLimit)
    .Value('monthlyFinancialInjectionLimit', fmonthlyFinancialInjectionLimit)
    .Value('realBalance', frealBalance)
    .Value('maxCreditLimit', fmaxCreditLimit)
    .Value('currentMonthlyFinancialInjection', fcurrentMonthlyFinancialInjection);
end;

constructor TMateraFinancialLimit.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  Clear;
end;

procedure TMateraFinancialLimit.Clear;
begin
  fcurrentMonthlyFinancialInjection := 0;
  fmaxCreditLimit := 0;
  fmonthlyFinancialInjectionLimit := 0;
  frealBalance := 0;
  frealBalanceLimit := 0;
end;

function TMateraFinancialLimit.IsEmpty: Boolean;
begin
  Result :=
    (fcurrentMonthlyFinancialInjection = 0) and
    (fmaxCreditLimit = 0) and
    (fmonthlyFinancialInjectionLimit = 0) and
    (frealBalance = 0) and
    (frealBalanceLimit = 0);
end;

procedure TMateraFinancialLimit.Assign(aSource: TMateraFinancialLimit);
begin
  fcurrentMonthlyFinancialInjection := aSource.currentMonthlyFinancialInjection;
  fmaxCreditLimit := aSource.maxCreditLimit;
  fmonthlyFinancialInjectionLimit := aSource.monthlyFinancialInjectionLimit;
  frealBalance := aSource.realBalance;
  frealBalanceLimit := aSource.realBalanceLimit;
end;

{ TMateraAccount }

procedure TMateraAccount.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraAccount) then
    Assign(TMateraAccount(aSource));
end;

procedure TMateraAccount.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('accountId', faccountID)
    .AddPair('account', faccount, False)
    .AddPair('branch', fbranch, False);

  fmobilePhone.WriteToJSon(aJSon);
end;

procedure TMateraAccount.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('accountId', faccountID)
    .Value('account', faccount)
    .Value('branch', fbranch);

  fmobilePhone.ReadFromJSon(aJSon);
end;

constructor TMateraAccount.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  fmobilePhone := TMateraMobilePhone.Create('mobilePhone');
  Clear;
end;

destructor TMateraAccount.Destroy;
begin
  fmobilePhone.Free;
  inherited Destroy;
end;

procedure TMateraAccount.Clear;
begin
  faccount := 0;
  fbranch := 0;          
  faccountID := EmptyStr;
  fmobilePhone.Clear;
end;

function TMateraAccount.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(faccountID) and
    (faccount = 0) and
    (fbranch = 0) and
    fmobilePhone.IsEmpty;
end;

procedure TMateraAccount.Assign(aSource: TMateraAccount);
begin
  faccount := aSource.account;
  fbranch := aSource.branch;
  faccountID := aSource.accountID;
  fmobilePhone.Assign(aSource.mobilePhone);
end;

{ TMateraRates }

procedure TMateraRates.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraRates) then
    Assign(TMateraRates(aSource));
end;

procedure TMateraRates.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('chargeDay', fchargeDay)
    .AddPair('chargesPeriod', fchargesPeriod)
    .AddPair('effectiveCostMonthly', feffectiveCostMonthly)
    .AddPair('effectiveCostYearly', feffectiveCostYearly)
    .AddPair('monthly', fmonthly)
    .AddPair('yearly', fyearly);
end;

procedure TMateraRates.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('chargeDay', fchargeDay)
    .Value('chargesPeriod', fchargesPeriod)
    .Value('effectiveCostMonthly', feffectiveCostMonthly)
    .Value('effectiveCostYearly', feffectiveCostYearly)
    .Value('monthly', fmonthly)
    .Value('yearly', fyearly);
end;

constructor TMateraRates.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  Clear;
end;

procedure TMateraRates.Clear;
begin
  fyearly := 0;
  fmonthly := 0;
  fchargeDay := 0;
  feffectiveCostYearly := 0;
  feffectiveCostMonthly := 0;
  fchargesPeriod := EmptyStr;
end;

function TMateraRates.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(fchargesPeriod) and
    (fyearly = 0) and
    (fmonthly = 0) and
    (fchargeDay = 0) and
    (feffectiveCostYearly = 0) and
    (feffectiveCostMonthly = 0);
end;

procedure TMateraRates.Assign(aSource: TMateraRates);
begin
  fyearly := aSource.yearly;
  fmonthly := aSource.monthly;
  fchargeDay := aSource.chargeDay;
  fchargesPeriod := aSource.chargesPeriod;
  feffectiveCostYearly := aSource.effectiveCostYearly;
  feffectiveCostMonthly := aSource.effectiveCostMonthly;
end;

{ TMateraAccountTransactionRequestBasic }

procedure TMateraAccountTransactionRequestBasic.AssignSchema(
  aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraAccountTransactionRequestBasic) then
    Assign(TMateraAccountTransactionRequestBasic(aSource));
end;

procedure TMateraAccountTransactionRequestBasic.DoWriteToJSon(
  aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('accountInternalTypeId', faccountInternalTypeId, False)
    .AddPair('externalIdentifier', fexternalIdentifier)
    .AddPair('clientType', MateraClientTypeToString(fclientType))
    .AddPair('accountType', MateraAccountTypeToString(faccountType))
    .AddPair('ibkPwdHash', fibkPwdHash, False)
    .AddPair('sharedAccount', fsharedAccount, False);

  fdocuments.WriteToJSon(aJSon);
  fmobilePhone.WriteToJSon(aJSon);
  fbillingAddress.WriteToJSon(aJSon);
  fadditionalDetailsPerson.WriteToJSon(aJSon);
  fadditionalDetailsCorporate.WriteToJSon(aJSon);
  fadditionalDetailsForeigner.WriteToJSon(aJSon);
end;

procedure TMateraAccountTransactionRequestBasic.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  s: String;
begin
  aJSon
    .Value('accountInternalTypeId', faccountInternalTypeId)
    .Value('accountType', s)
    .Value('externalIdentifier', fexternalIdentifier)
    .Value('ibkPwdHash', fibkPwdHash)
    .Value('sharedAccount', fsharedAccount);

  faccountType := StringToMateraAccountType(s);
  fdocuments.ReadFromJSon(aJSon);
  fmobilePhone.ReadFromJSon(aJSon);
  fbillingAddress.ReadFromJSon(aJSon);
  fadditionalDetailsPerson.ReadFromJSon(aJSon);
  fadditionalDetailsCorporate.ReadFromJSon(aJSon);
  fadditionalDetailsForeigner.ReadFromJSon(aJSon);
end;

constructor TMateraAccountTransactionRequestBasic.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  fdocuments := TMateraDocumentArray.Create('');
  fmobilePhone := TMateraMobilePhone.Create('mobilePhone');
  fbillingAddress := TMateraEndereco.Create('billingAddress');
  fadditionalDetailsCorporate := TMateraAdditionalDetailsCorporate.Create('additionalDetailsCorporate');
  fadditionalDetailsForeigner := TMateraAdditionalDetailsBasic.Create('additionalDetailsForeigner');
  fadditionalDetailsPerson := TMateraAdditionalDetailsPerson.Create('additionalDetailsPerson');
  Clear;
end;

destructor TMateraAccountTransactionRequestBasic.Destroy;
begin
  fdocuments.Free;
  fmobilePhone.Free;
  fbillingAddress.Free;
  fadditionalDetailsPerson.Free;
  fadditionalDetailsCorporate.Free;
  fadditionalDetailsForeigner.Free;
  inherited Destroy;
end;

procedure TMateraAccountTransactionRequestBasic.Clear;
begin
  faccountInternalTypeId := 0;
  fibkPwdHash := EmptyStr;
  fsharedAccount := EmptyStr;     
  fexternalIdentifier := EmptyStr;

  fdocuments.Clear;
  fmobilePhone.Clear;
  fbillingAddress.Clear;
  fadditionalDetailsPerson.Clear;
  fadditionalDetailsCorporate.Clear;
  fadditionalDetailsForeigner.Clear;
end;

function TMateraAccountTransactionRequestBasic.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(fibkPwdHash) and
    EstaVazio(fsharedAccount) and
    EstaVazio(fexternalIdentifier) and
    (faccountType = matOrdinary) and
    (faccountInternalTypeId = 0) and
    fdocuments.IsEmpty and
    fmobilePhone.IsEmpty and
    fbillingAddress.IsEmpty and
    fadditionalDetailsPerson.IsEmpty and
    fadditionalDetailsCorporate.IsEmpty and
    fadditionalDetailsForeigner.IsEmpty;
end;

procedure TMateraAccountTransactionRequestBasic.Assign(
  aSource: TMateraAccountTransactionRequestBasic);
begin
  faccountType := aSource.accountType;
  fibkPwdHash := aSource.ibkPwdHash;
  fsharedAccount := aSource.sharedAccount;
  fexternalIdentifier := aSource.externalIdentifier;
  faccountInternalTypeId := aSource.accountInternalTypeId;

  fdocuments.Assign(aSource.documents);
  fmobilePhone.Assign(aSource.mobilePhone);
  fbillingAddress.Assign(aSource.billingAddress);
  fadditionalDetailsPerson.Assign(aSource.additionalDetailsPerson);
  fadditionalDetailsCorporate.Assign(aSource.additionalDetailsCorporate);
  fadditionalDetailsForeigner.Assign(aSource.additionalDetailsForeigner);
end;

{ TMateraClientRepresentativeArray }

function TMateraClientRepresentativeArray.GetItem(aIndex: Integer): TMateraClientRepresentative;
begin
  Result := TMateraClientRepresentative(inherited Items[aIndex]);
end;

procedure TMateraClientRepresentativeArray.SetItem(aIndex: Integer; aValue: TMateraClientRepresentative);
begin
  inherited Items[aIndex] := aValue;
end;

function TMateraClientRepresentativeArray.NewSchema: TACBrPIXSchema;
begin
  Result := New;
end;

function TMateraClientRepresentativeArray.Add(aItem: TMateraClientRepresentative): Integer;
begin
  Result := inherited Add(aItem);
end;

procedure TMateraClientRepresentativeArray.Insert(aIndex: Integer; aItem: TMateraClientRepresentative);
begin
  inherited Insert(aIndex, aItem);
end;

function TMateraClientRepresentativeArray.New: TMateraClientRepresentative;
begin
  Result := TMateraClientRepresentative.Create;
  Self.Add(Result);
end;

{ TMateraAdditionalDetailsCorporate }

procedure TMateraAdditionalDetailsCorporate.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraAdditionalDetailsCorporate) then
    Assign(TMateraAdditionalDetailsCorporate(aSource));
end;

procedure TMateraAdditionalDetailsCorporate.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('establishmentDate', FormatDateTime('yyyy-mm-dd', festablishmentDate))
    .AddPair('companyName', fcompanyName)
    .AddPair('businessLine', fbusinessLine)
    .AddPair('establishmentForm', festablishmentForm)
    .AddPair('financialStatistic', ffinancialStatistic, False)
    .AddPair('monthlyIncome', fmonthlyIncome, False)
    .AddPair('stateRegistration', fstateRegistration, False);
  frepresentatives.WriteToJSon(aJSon);
end;

procedure TMateraAdditionalDetailsCorporate.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('businessLine', fbusinessLine)
    .Value('companyName', fcompanyName)
    .ValueISODate('establishmentDate', festablishmentDate)
    .Value('establishmentForm', festablishmentForm)
    .Value('financialStatistic', ffinancialStatistic)
    .Value('monthlyIncome', fmonthlyIncome)
    .Value('stateRegistration', fstateRegistration);
  frepresentatives.ReadFromJSon(aJSon);
end;

constructor TMateraAdditionalDetailsCorporate.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  frepresentatives := TMateraClientRepresentativeArray.Create('representatives');
  Clear;
end;

destructor TMateraAdditionalDetailsCorporate.Destroy;
begin
  frepresentatives.Free;
  inherited Destroy;
end;

procedure TMateraAdditionalDetailsCorporate.Clear;
begin
  fbusinessLine := 0;
  fmonthlyIncome := 0;
  festablishmentDate := 0;       
  ffinancialStatistic := 0;
  fcompanyName := EmptyStr;
  festablishmentForm := EmptyStr;
  fstateRegistration := EmptyStr;
  frepresentatives.Clear;
end;

function TMateraAdditionalDetailsCorporate.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(fcompanyName) and
    EstaVazio(festablishmentForm) and
    EstaVazio(fstateRegistration) and
    (fbusinessLine = 0) and
    (fmonthlyIncome = 0) and
    (festablishmentDate = 0) and
    (ffinancialStatistic = 0) and
    frepresentatives.IsEmpty;
end;

procedure TMateraAdditionalDetailsCorporate.Assign(aSource: TMateraAdditionalDetailsCorporate);
begin
  fbusinessLine := aSource.businessLine;
  fmonthlyIncome := aSource.monthlyIncome;
  festablishmentDate := aSource.establishmentDate;
  ffinancialStatistic := aSource.financialStatistic;
  fcompanyName := aSource.companyName;
  festablishmentForm := aSource.establishmentForm;
  fstateRegistration := aSource.stateRegistration;
  frepresentatives.Assign(aSource.representatives);
end;

{ TMateraClientRepresentative }

procedure TMateraClientRepresentative.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraClientRepresentative) then
    Assign(TMateraClientRepresentative(aSource));
end;

procedure TMateraClientRepresentative.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  inherited DoWriteToJSon(aJSon);
  aJSon.AddPair('accountHolderId', faccountHolderId, False);
end;

procedure TMateraClientRepresentative.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  inherited DoReadFromJSon(aJSon);
  aJSon.Value('accountHolderId', faccountHolderId);
end;

constructor TMateraClientRepresentative.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  Clear;
end;

procedure TMateraClientRepresentative.Clear;
begin
  inherited Clear;
  faccountHolderId := EmptyStr;
end;

function TMateraClientRepresentative.IsEmpty: Boolean;
begin
  Result := (inherited IsEmpty) and EstaVazio(faccountHolderId);
end;

procedure TMateraClientRepresentative.Assign(
  aSource: TMateraClientRepresentative);
begin
  inherited Assign(aSource);
  faccountHolderId := aSource.accountHolderId;
end;

{ TMateraAdditionalDetailsPerson }

procedure TMateraAdditionalDetailsPerson.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraAdditionalDetailsPerson) then
    Assign(TMateraAdditionalDetailsPerson(aSource));
end;

procedure TMateraAdditionalDetailsPerson.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  inherited DoWriteToJSon(aJSon);
  aJSon.AddPair('monthlyIncome', fmonthlyIncome);

  frg.WriteToJSon(aJSon);
  flegalResponsible.WriteToJSon(aJSon);
end;

procedure TMateraAdditionalDetailsPerson.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  inherited DoReadFromJSon(aJSon);
  aJSon.Value('monthlyIncome', fmonthlyIncome);

  frg.ReadFromJSon(aJSon);
  flegalResponsible.ReadFromJSon(aJSon);
end;

constructor TMateraAdditionalDetailsPerson.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);

  frg := TMateraRG.Create('rg');
  flegalResponsible := TMateraClient.Create('legalResponsible');

  Clear;
end;

destructor TMateraAdditionalDetailsPerson.Destroy;
begin
  frg.Free;
  flegalResponsible.Free;
  inherited Destroy;
end;

procedure TMateraAdditionalDetailsPerson.Clear;
begin
  inherited Clear;
  fmonthlyIncome := 0;

  if Assigned(frg) then
    frg.Clear;

  if Assigned(flegalResponsible) then
    flegalResponsible.Clear;
end;

function TMateraAdditionalDetailsPerson.IsEmpty: Boolean;
begin
  Result :=
    (inherited IsEmpty) and
    (fmonthlyIncome = 0) and
    frg.IsEmpty and
    flegalResponsible.IsEmpty;
end;

procedure TMateraAdditionalDetailsPerson.Assign(
  aSource: TMateraAdditionalDetailsPerson);
begin
  inherited Assign(aSource);
  fmonthlyIncome := aSource.monthlyIncome;
  frg.Assign(aSource.rg);
  flegalResponsible.Assign(aSource.legalResponsible);
end;

{ TMateraAdditionalDetailsBasic }

procedure TMateraAdditionalDetailsBasic.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraAdditionalDetailsBasic) then
    Assign(TMateraAdditionalDetailsBasic(aSource));
end;

procedure TMateraAdditionalDetailsBasic.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('birthCity', fbirthCity)
    .AddPair('birthCountry', fbirthCountry)
    .AddPair('birthDate', FormatDateTime('yyyy-mm-dd', fbirthDate))
    .AddPair('birthState', fbirthState)
    .AddPair('businessLine', fbusinessLine)
    .AddPair('documentType', fdocumentType)
    .AddPair('father', ffather)
    .AddPair('financialStatistic', ffinancialStatistic)
    .AddPair('gender', fgender)
    .AddPair('maritalStatus', fmaritalStatus)
    .AddPair('mother', fmother)
    .AddPair('occupation', foccupation)
    .AddPair('partner', fpartner);
  fotherDocument.WriteToJSon(aJSon);
end;

procedure TMateraAdditionalDetailsBasic.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('birthCity', fbirthCity)
    .Value('birthCountry', fbirthCountry)
    .ValueISODate('birthDate', fbirthDate)
    .Value('birthState', fbirthState)
    .Value('businessLine', fbusinessLine)
    .Value('documentType', fdocumentType)
    .Value('father', ffather)
    .Value('financialStatistic', ffinancialStatistic)
    .Value('gender', fgender)
    .Value('maritalStatus', fmaritalStatus)
    .Value('mother', fmother)
    .Value('occupation', foccupation)
    .Value('partner', fpartner);
  fotherDocument.ReadFromJSon(aJSon);
end;

constructor TMateraAdditionalDetailsBasic.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  fotherDocument := TMateraOtherDoc.Create('otherDocument');
  Clear;
end;

destructor TMateraAdditionalDetailsBasic.Destroy;
begin
  fotherDocument.Free;
  inherited Destroy;
end;

procedure TMateraAdditionalDetailsBasic.Clear;
begin                 
  fbirthDate := 0;
  foccupation := 0;  
  fbusinessLine := 0;
  ffinancialStatistic := 0;
  fbirthCity := EmptyStr;
  fbirthCountry := EmptyStr;
  fbirthState := EmptyStr;
  fdocumentType := EmptyStr;
  ffather := EmptyStr;
  fgender := EmptyStr;
  fmaritalStatus := EmptyStr;
  fmother := EmptyStr;
  fpartner := EmptyStr;
  fotherDocument.Clear;
end;

function TMateraAdditionalDetailsBasic.IsEmpty: Boolean;
begin
  Result :=
    (fbirthDate = 0) and
    (foccupation = 0) and
    (fbusinessLine = 0) and
    (ffinancialStatistic = 0) and
    EstaVazio(fbirthCity) and
    EstaVazio(fbirthCountry) and
    EstaVazio(fbirthState) and
    EstaVazio(fdocumentType) and
    EstaVazio(ffather) and
    EstaVazio(fgender) and
    EstaVazio(fmaritalStatus) and
    EstaVazio(fmother) and
    EstaVazio(fpartner) and
    fotherDocument.IsEmpty;
end;

procedure TMateraAdditionalDetailsBasic.Assign(
  aSource: TMateraAdditionalDetailsBasic);
begin
  fbirthCity := aSource.birthCity;
  fbirthCountry := aSource.birthCountry;
  fbirthDate := aSource.birthDate;
  fbirthState := aSource.birthState;
  fbusinessLine := aSource.businessLine;
  fdocumentType := aSource.documentType;
  ffather := aSource.father;
  ffinancialStatistic := aSource.financialStatistic;
  fgender := aSource.gender;
  fmaritalStatus := aSource.maritalStatus;
  fmother := aSource.mother;
  foccupation := aSource.occupation;
  fpartner := aSource.partner;
  fotherDocument.Assign(aSource.otherDocument);
end;

{ TMateraClient }

procedure TMateraClient.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraClient) then
    Assign(TMateraClient(aSource));
end;

procedure TMateraClient.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  inherited DoWriteToJSon(aJSon);
  aJSon.AddPair('mother', fmother, False);

  if (fbirthDate > 0) then
    aJSon.AddPair('birthDate', FormatDateTime('yyyy-mm-dd', fbirthDate));
end;

procedure TMateraClient.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  inherited DoReadFromJSon(aJSon);
  aJSon
    .ValueISODate('birthDate', fbirthDate)
    .Value('mother', fmother)
end;

constructor TMateraClient.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  Clear;
end;

procedure TMateraClient.Clear;
begin
  inherited Clear;
  fbirthDate := 0;
  fmother := EmptyStr;
end;

function TMateraClient.IsEmpty: Boolean;
begin
  Result :=
    (inherited IsEmpty) and
    (fbirthDate = 0) and
    EstaVazio(fmother);
end;

procedure TMateraClient.Assign(aSource: TMateraClient);
begin
  inherited Assign(aSource);
  fbirthDate := aSource.birthDate;
  fmother := aSource.mother;
end;

{ TMateraOtherDoc }

procedure TMateraOtherDoc.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraOtherDoc) then
    Assign(TMateraOtherDoc(aSource));
end;

procedure TMateraOtherDoc.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('issueDate', FormatDateTime('yyyy-mm-dd', fissueDate))
    .AddPair('issuer', fissuer)
    .AddPair('number', fnumber)
    .AddPair('type', ftype);
end;

procedure TMateraOtherDoc.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .ValueISODate('issueDate', fissueDate)
    .Value('issuer', fissuer)
    .Value('number', fnumber)
    .Value('type', ftype);
end;

constructor TMateraOtherDoc.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  Clear;
end;

procedure TMateraOtherDoc.Clear;
begin
  fissueDate := 0;
  fissuer := EmptyStr;
  fnumber := EmptyStr;
  ftype := EmptyStr;
end;

function TMateraOtherDoc.IsEmpty: Boolean;
begin
  Result :=
    (fissueDate = 0) and
    EstaVazio(fissuer) and
    EstaVazio(fnumber) and
    EstaVazio(ftype);
end;

procedure TMateraOtherDoc.Assign(aSource: TMateraOtherDoc);
begin
  fissueDate := aSource.issueDate;
  fissuer := aSource.issuer;
  fnumber := aSource.number;
  ftype := aSource.type_;
end;

{ TMateraRG }

procedure TMateraRG.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraRG) then
    Assign(TMateraRG(aSource));
end;

procedure TMateraRG.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('issueDate', FormatDateTime('yyyy-mm-dd', fissueDate))
    .AddPair('issuer', fissuer)
    .AddPair('number', fnumber)
    .AddPair('state', fstate);
end;

procedure TMateraRG.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .ValueISODate('issueDate', fissueDate)
    .Value('issuer', fissuer)
    .Value('number', fnumber)
    .Value('state', fstate);
end;

constructor TMateraRG.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  Clear;
end;

procedure TMateraRG.Clear;
begin
  fissueDate := 0;
  fissuer := EmptyStr;
  fnumber := EmptyStr;
  fstate := EmptyStr;
end;

function TMateraRG.IsEmpty: Boolean;
begin
  Result :=
    (fissueDate = 0) and
    EstaVazio(fissuer) and
    EstaVazio(fnumber) and
    EstaVazio(fstate);
end;

procedure TMateraRG.Assign(aSource: TMateraRG);
begin
  fissueDate := aSource.issueDate;
  fissuer := aSource.issuer;
  fnumber := aSource.number;
  fstate := aSource.state;
end;

{ TMateraDocumentArray }

function TMateraDocumentArray.GetItem(aIndex: Integer): TMateraDocument;
begin
  Result := TMateraDocument(inherited Items[aIndex]);
end;

procedure TMateraDocumentArray.SetItem(aIndex: Integer; aValue: TMateraDocument);
begin
  inherited Items[aIndex] := aValue;
end;

function TMateraDocumentArray.NewSchema: TACBrPIXSchema;
begin
  Result := New;
end;

function TMateraDocumentArray.Add(aItem: TMateraDocument): Integer;
begin
  Result := inherited Add(aItem);
end;

procedure TMateraDocumentArray.Insert(aIndex: Integer; AItem: TMateraDocument);
begin
  inherited Insert(aIndex, aItem);
end;

function TMateraDocumentArray.New: TMateraDocument;
begin
  Result := TMateraDocument.Create;
  Self.Add(Result);
end;

{ TMateraDocument }

procedure TMateraDocument.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraDocument) then
    Assign(TMateraDocument(aSource));
end;

procedure TMateraDocument.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('content', fcontent)
    .AddPair('type', MateraDocumentTypeToString(type_));
end;

procedure TMateraDocument.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  s: String;
begin
  {$IfDef FPC}s := EmptyStr;{$EndIf}

  aJSon
    .Value('content', fcontent)
    .Value('type', s);

  ftype := StringToMateraDocumentType(s);
end;

constructor TMateraDocument.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  Clear;
end;

procedure TMateraDocument.Clear;
begin
  fcontent := EmptyStr;
  ftype := mdtUnknown;
end;

function TMateraDocument.IsEmpty: Boolean;
begin
  Result := EstaVazio(fcontent) and (ftype = mdtUnknown);
end;

procedure TMateraDocument.Assign(aSource: TMateraDocument);
begin
  fcontent := aSource.content;
  ftype := aSource.type_;
end;

{ TMateraEndereco }

procedure TMateraEndereco.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraEndereco) then
    Assign(TMateraEndereco(aSource));
end;

procedure TMateraEndereco.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('logradouro', flogradouro)
    .AddPair('numero', fnumero)
    .AddPair('complemento', fcomplemento, False)
    .AddPair('bairro', fbairro)
    .AddPair('cidade', fcidade)
    .AddPair('estado', festado)
    .AddPair('cep', fcep)
    .AddPair('pais', fpais);
end;

procedure TMateraEndereco.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('bairro', fbairro)
    .Value('cep', fcep)
    .Value('cidade', fcidade)
    .Value('complemento', fcomplemento)
    .Value('estado', festado)
    .Value('logradouro', flogradouro)
    .Value('numero', fnumero)
    .Value('pais', fpais);
end;

constructor TMateraEndereco.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  Clear;
end;

procedure TMateraEndereco.Clear;
begin
  fbairro := EmptyStr;
  fcep := EmptyStr;
  fcidade := EmptyStr;
  fcomplemento := EmptyStr;
  festado := EmptyStr;
  flogradouro := EmptyStr;
  fnumero := EmptyStr;
  fpais := EmptyStr;
end;

function TMateraEndereco.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(fbairro) and
    EstaVazio(fcep) and
    EstaVazio(fcidade) and
    EstaVazio(fcomplemento) and
    EstaVazio(festado) and
    EstaVazio(flogradouro) and
    EstaVazio(fnumero) and
    EstaVazio(fpais);
end;

procedure TMateraEndereco.Assign(aSource: TMateraEndereco);
begin
  fbairro := aSource.bairro;
  fcep := aSource.cep;
  fcidade := aSource.cidade;
  fcomplemento := aSource.complemento;
  festado := aSource.estado;
  flogradouro := aSource.logradouro;
  fnumero := aSource.numero;
  fpais := aSource.pais;
end;

{ TMateraTaxIdentifier }

procedure TMateraTaxIdentifier.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (ASource is TMateraTaxIdentifier) then
    Assign(TMateraTaxIdentifier(ASource));
end;

procedure TMateraTaxIdentifier.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('taxId', ftaxId)
    .AddPair('country', fcountry)
    .AddPair('taxIdMasked', ftaxIdMasked, False);
end;

procedure TMateraTaxIdentifier.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  AJSon
    .Value('country', fcountry)
    .Value('taxId', ftaxId)
    .Value('taxIdMasked', ftaxIdMasked);
end;

constructor TMateraTaxIdentifier.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  Clear;
end;

procedure TMateraTaxIdentifier.Clear;
begin
  fcountry := EmptyStr;
  ftaxId := EmptyStr;
  ftaxIdMasked := EmptyStr;
end;

function TMateraTaxIdentifier.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(fcountry) and
    EstaVazio(ftaxId) and
    EstaVazio(ftaxIdMasked);
end;

procedure TMateraTaxIdentifier.Assign(aSource: TMateraTaxIdentifier);
begin
  fcountry := aSource.country;
  ftaxId := aSource.taxId;
  ftaxIdMasked := aSource.taxIdMasked;
end;

{ TMateraMobilePhone }

procedure TMateraMobilePhone.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (ASource is TMateraMobilePhone) then
    Assign(TMateraMobilePhone(ASource));
end;

procedure TMateraMobilePhone.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('country', fcountry)
    .AddPair('phoneNumber', fphoneNumber);
end;

procedure TMateraMobilePhone.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  AJSon
    .Value('country', fcountry)
    .Value('phoneNumber', fphoneNumber);
end;

constructor TMateraMobilePhone.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  Clear;
end;

procedure TMateraMobilePhone.Clear;
begin
  fcountry := EmptyStr;
  fphoneNumber := EmptyStr;
end;

function TMateraMobilePhone.IsEmpty: Boolean;
begin
  Result := EstaVazio(fcountry) and EstaVazio(fphoneNumber);
end;

procedure TMateraMobilePhone.Assign(aSource: TMateraMobilePhone);
begin
  fcountry := aSource.country;
  fphoneNumber := aSource.phoneNumber;
end;

{ TMateraBasicClient }

procedure TMateraBasicClient.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraBasicClient) then
    Assign(TMateraBasicClient(aSource));
end;

procedure TMateraBasicClient.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('name', fname)
    .AddPair('socialName', fsocialName, False)
    .AddPair('email', femail, False);
    
  ftaxIdentifier.WriteToJSon(aJSon);
  fmobilePhone.WriteToJSon(aJSon);
  fmailAddress.WriteToJSon(aJSon);

  if (fdocuments.Count > 0) then
    fdocuments.WriteToJSon(aJSon);
end;

procedure TMateraBasicClient.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('email', femail)
    .Value('name', fname)
    .Value('socialName', fsocialName);

  fdocuments.ReadFromJSon(aJSon);
  fmailAddress.ReadFromJSon(aJSon);
  fmobilePhone.ReadFromJSon(aJSon);
  ftaxIdentifier.ReadFromJSon(aJSon);
end;

constructor TMateraBasicClient.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);

  fdocuments := TMateraDocumentArray.Create('documents');
  fmailAddress := TMateraEndereco.Create('mailAddress');
  fmobilePhone := TMateraMobilePhone.Create('mobilePhone');
  ftaxIdentifier := TMateraTaxIdentifier.Create('taxIdentifier');
  Clear;
end;

destructor TMateraBasicClient.Destroy;
begin
  fmailAddress.Free;
  fdocuments.Free;
  fmobilePhone.Free;
  ftaxIdentifier.Free;
  inherited Destroy;
end;

procedure TMateraBasicClient.Clear;
begin
  fdocuments.Clear;
  fmailAddress.Clear;
  fmobilePhone.Clear;
  ftaxIdentifier.Clear;
end;

function TMateraBasicClient.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(femail) and
    EstaVazio(fname) and
    EstaVazio(fsocialName) and
    fdocuments.IsEmpty and
    fmailAddress.IsEmpty and
    fmobilePhone.IsEmpty and
    ftaxIdentifier.IsEmpty;
end;

procedure TMateraBasicClient.Assign(aSource: TMateraBasicClient);
begin
  femail := aSource.email;
  fname := aSource.name;
  fsocialName := aSource.socialName;

  fdocuments.Assign(aSource.documents);
  fmailAddress.Assign(aSource.mailAddress);
  fmobilePhone.Assign(aSource.mobilePhone);
  ftaxIdentifier.Assign(aSource.taxIdentifier);
end;

end.

