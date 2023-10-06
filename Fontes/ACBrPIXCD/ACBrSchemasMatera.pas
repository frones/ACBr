{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{ - Elias César Vieira                                                         }
{ - Alexandre de Paula                                                         }
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
  ACBrUtil.FilesIO, ACBrUtil.Strings;

type  

  TMateraDocumentType = (
    mdtNone,
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
    mctNone,
    mctPerson,
    mctCorporate,
    mctForeigner
  );

  TMateraAccountType = (
    matNone,
    matOrdinary,
    matOverdraftProtected,
    matCommon,
    matUnlimitedOrdinary
  );

  TMateraAccountStatus = (
    mcsNone,
    mcsRegular,
    mcsLocked,
    mcsClosed,
    mcsReserved,
    mcsCreating,
    mcsError
  );

  TMateraActiveStatus = (
    macNone,
    macActive,
    macInactive,
    macBlocked
  );

  TMateraAliasType = (
    malNone,
    //malTaxId,
    //malEmail,
    //malPhone,
    malEVP
  );

  TMateraAliasStatus = (
    mastNone,
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
    mttNone,
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
    mqtNone,
    mqtImmediate,
    mqtBillingDueDate
  );

  TMateraMaritalStatus = (
    mmsNone,
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
    mtsNone,
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

  TMateraReturnType = (
    mrtNone,
    mrtTransactionValue,
    mrtCashValue,
    mrtOperationFlaw,
    mrtFraud
  );

  TMateraAccountTypeDestination = (
    matdNone,
    matdCC,
    matdPoupanca,
    matdSalario,
    matdIP
  );

  TMateraAntifraudCounter = (
    mafcNone,
    mafcSettlements,
    mafcReported_Frauds,
    mafcReported_AML_CFT,
    mafcConfirmed_Frauds,
    mafcConfirmed_AML_CFT,
    mafcRejected
  );

  TMateraWithdrawType = (
    mwtNone,
    mwtBankTransfer,
    mwtExternal,
    mwtBoleto,
    mwtReloadPrepaid,
    mwtUtilities,
    mwtInstantPayment
  );

  TMateraInitiationForm = (
    mifNone,
    mifINIC,
    mifMANU,
    mifDICT,
    mifQRES,
    mifQRDN
  );

  TMateraQrcodeType = (
    mqrtNone,
    mqrtStatic,
    mqrtDynamic
  );

  TMateraInitiationProcedure = (
    mipNone,
    mipManual,
    mipPre_Stored
  );

  TMateraInstructionPriority = (
    maipNone,
    maipHigh,
    maipNorm
  );

  TMateraTransactionPurpose = (
    mtpNone,
    mtpIPAY,
    mtpGSCB,
    mtpOTHR
  );

  TMateraInstructionType = (
    mitNone,
    mitPAGPRI,
    mitPAGFRD,
    mitPAGAGD
  );

  TMateraWithdrawAgentType = (
    mwatNone,
    mwatAGPSS,
    mwatAGTEC,
    mwatAGTOT,
    mwatAGFSS
  );

  TMateraCashValueType = (
    mcvNone,
    mcvChange,
    mcvWithdraw
  );

  TMaterastatementDeletionMode = (
    msdNone,
    msdRefund,
    msdExclusion,
    msdExclusion_Preferred
  );

  TMaterastatementEntryType = (
    msetNone,
    msetC,
    msetD,
    msetS
  );

  TMateraStatementIPCashValueType = (
    msipvtNone,
    msipvtPix_Purchase,
    msipvtPix_Change
  );

  { TMateraTaxIdentifierBasic }

  TMateraTaxIdentifierBasic = class(TACBrPIXSchema)
  private
    fcountry: String;
    ftaxId: String;
    ftaxIdMasked: String;
  protected
    property taxId: String read ftaxId write ftaxId;
    property country: String read fcountry write fcountry;
    property taxIdMasked: String read ftaxIdMasked write ftaxIdMasked;

    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraTaxIdentifierBasic);
  end;

  { TMateraTaxIdentifier }

  TMateraTaxIdentifier = class(TMateraTaxIdentifierBasic)
  public
    property taxId;
    property country;
    property taxIdMasked;
  end;

  { TMateraTaxIdentifierRequest }

  TMateraTaxIdentifierRequest = class(TMateraTaxIdentifierBasic)
  public
    property taxID;
    property country;
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
    flegalResponsible: TMateraClient;
    fmonthlyIncome: Double;
    frg: TMateraRG;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;

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

    property rg: TMateraRG read frg write frg;
    property monthlyIncome: Double read fmonthlyIncome write fmonthlyIncome;
    property legalResponsible: TMateraClient read flegalResponsible write flegalResponsible;
  public
    constructor Create(const aObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraAdditionalDetailsBasic);
  end;

  { TMateraAdditionalDetailsPerson }

  TMateraAdditionalDetailsPerson = class(TMateraAdditionalDetailsBasic)
  public
    property birthCity;
    property birthCountry;
    property birthDate;
    property birthState;
    property businessLine;
    property documentType;
    property father;
    property financialStatistic;
    property gender;
    property maritalStatus;
    property mother;
    property occupation;
    property otherDocument;
    property partner;
    property rg;
    property monthlyIncome;
    property legalResponsible;
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

    property businessLine: Integer read fbusinessLine write fbusinessLine;
    property companyName: String read fcompanyName write fcompanyName;
    property establishmentDate: TDateTime read festablishmentDate write festablishmentDate;
    property establishmentForm: String read festablishmentForm write festablishmentForm;
    property financialStatistic: Double read ffinancialStatistic write ffinancialStatistic;
    property monthlyIncome: Double read fmonthlyIncome write fmonthlyIncome;
    property stateRegistration: String read fstateRegistration write fstateRegistration;
    property representatives: TMateraClientRepresentativeArray read frepresentatives write frepresentatives;
  end;

  { TMateraAccountTransactionRequestBasic }

  TMateraAccountTransactionRequestBasic = class(TACBrPIXSchema)
  private
    faccountInternalTypeId: Integer;
    fexternalIdentifier: String;
    fibkPwdHash: String;
    fsharedAccount: String;
    faccountType: TMateraAccountType;
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

  { TMateraAccountHolderBasic }

  TMateraAccountHolderBasic = class(TACBrPIXSchema)
  private
    fcallbackSecretKey: String;
    fname: String;
    femail: String;
    fsocialName: String;
    faccountHolderId: String;
    frepresentativeId: String;
    fstatus: TMateraActiveStatus;
    fbillingAddress: TMateraEndereco;
    fmailAddress: TMateraEndereco;
    fmobilePhone: TMateraMobilePhone;
    fmobilePhones: TMateraMobilePhoneArray;
    frepresentatives: TMateraClientRepresentativeArray;
    ftaxIdentifier: TMateraTaxIdentifierBasic;
  protected
    property billingAddress: TMateraEndereco read fbillingAddress write fbillingAddress;
    property callbackSecretKey: String read fcallbackSecretKey write fcallbackSecretKey;

    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraAccountHolderBasic);

    property status: TMateraActiveStatus read fstatus write fstatus;
    property accountHolderId: String read faccountHolderId write faccountHolderId;
    property taxIdentifier: TMateraTaxIdentifierBasic read ftaxIdentifier write ftaxIdentifier;
    property mobilePhones: TMateraMobilePhoneArray read fmobilePhones write fmobilePhones;
    property mobilePhone: TMateraMobilePhone read fmobilePhone write fmobilePhone;
    property email: String read femail write femail;
    property mailAddress: TMateraEndereco read fmailAddress write fmailAddress;
    property name: String read fname write fname;
    property socialName: String read fsocialName write fsocialName;
    property representativeId: String read frepresentativeId write frepresentativeId;
    property representatives: TMateraClientRepresentativeArray read frepresentatives write frepresentatives;
  end;

  { TMateraAccountHolderResponse}
  TmateraAccountHolderResponse = class(TMateraAccountHolderBasic)
  public
    property billingAddress;
  end;

  { TMateraAccountHolder}
  TmateraAccountHolder = class(TMateraAccountHolderBasic)
  public
    property callbackSecretKey;
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
    destructor Destroy; override;
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
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraAliasRequest);

    property alias_: TMateraAliasBasic read falias write falias;
    property externalIdentifier: String read fexternalIdentifier write fexternalIdentifier;
  end;

  { TMateraRegisterAliasResponse }

  TMateraRegisterAliasResponse = class(TACBrPIXSchema)
  private
    fneedsIdentifyConfirmation: Boolean;
    fprotocolId: String; 
    falias: TMateraAlias;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    destructor Destroy; override;
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
    famount: Currency;
    fcurrency: String;
    fmediatorfee: Currency;
    frecipientComment: String;
    faccount: TMateraAccount;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraRecipient);

    property account: TMateraAccount read Faccount write Faccount;
    property amount: Currency read Famount write Famount;
    property currency: String read Fcurrency write Fcurrency;
    property mediatorfee: currency read Fmediatorfee write Fmediatorfee;
    property recipientComment: String read FrecipientComment write FrecipientComment;
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
    fcontent: String;
    fname: String;
    fshowToPlayer: Boolean;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraAdditionalInformation);

    property name: String read Fname write fname;
    property content: String read Fcontent write fcontent;
    property showToPlayer: Boolean read FshowToPlayer write fshowToPlayer;
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
    fmodality: Integer;
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

    property modality: Integer read fmodality write fmodality;
    property fixedDateDiscounts: TMateraFixedDateDiscountArray read ffixedDateDiscounts write ffixedDateDiscounts;
  end;

  { TMateraUniqueDiscount }

  TMateraUniqueDiscount = class(TACBrPIXSchema)
  private
    fmodality: Integer;
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
    property modality: Integer read fmodality write fmodality;
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

  { TMateraValueCalculation }

  TMateraValueCalculation = class(TACBrPIXSchema)
  private
    fmodality: Integer;
    fvaluePerc: Currency;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraValueCalculation);

    property valuePerc: Currency read fvaluePerc write fvaluePerc;
    property modality: Integer read fmodality write fmodality;
  end;

  { TMateraBillingDueDate }

  TMateraBillingDueDate = class(TACBrPIXSchema)
  private                                   
    fdueDate: TDateTime;
    fdaysAfterDueDate: Integer;
    ffines: TMateraValueCalculation;
    finterests: TMateraValueCalculation;
    freduction: TMateraValueCalculation;
    fdiscounts: TMateraDiscountsCalculation;
    fpayerInformation: TMateraQRCodeDynamicPayerInformationComplete;
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
    property interests: TMateraValueCalculation read finterests write finterests;
    property fines: TMateraValueCalculation read ffines write ffines;
    property discounts: TMateraDiscountsCalculation read fdiscounts write fdiscounts;
    property reduction: TMateraValueCalculation read freduction write freduction;
    property dueDate: TDateTime read fdueDate write fdueDate;
    property daysAfterDueDate: Integer read fdaysAfterDueDate write fdaysAfterDueDate;
  end;

  { TMateraInstantPayment }

  TMateraInstantPayment = class(TACBrPIXSchema)
  private
    fexpiration: Integer;
    falias: String;
    fbillingDueDate: TMateraBillingDueDate;
    fdynamicQRCodeType: TMateraDynamicQRCodeType;
    fadditionalInformation: TMateraAdditionalInformationArray;
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
    
    property expiration: Integer read Fexpiration write Fexpiration;
    property alias_: String read falias write falias;
    property dynamicQRCodeType: TMateraDynamicQRCodeType read fdynamicQRCodeType write fdynamicQRCodeType;
    property billingDueDate: TMateraBillingDueDate read fbillingDueDate write fbillingDueDate;
    property qrCodeImageGenerationSpecification: TMateraQRCodeSpecification
      read FqrCodeImageGenerationSpecification write FqrCodeImageGenerationSpecification;
    property additionalInformation: TMateraAdditionalInformationArray
      read FadditionalInformation write FadditionalInformation;
  end;

  { TMateraPaymentInfo }

  TMateraPaymentInfo = class(TACBrPIXSchema)
  private
    finstantPayment: TMateraInstantPayment;
    ftransactionType: String;
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
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraPayerInformation);

    property cpfCnpj: String read fcpfCnpj write fcpfCnpj;
    property name: String read fname write fname;
    property email: String read femail write femail;
    property addressing: TMateraPayerInformationAddressing read faddressing write faddressing;
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
    fcurrency: String;
    ftotalAmount: Currency;
    fcallbackAddress: String;
    fexternalIdentifier: String;
    fpaymentInfo: TMateraPaymentInfo;
    frecipients: TMateraRecipientsArray;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraQRCodeRequest);

    property currency: String read Fcurrency write fcurrency;
    property totalAmount: Currency read FtotalAmount write ftotalAmount;
    property externalIdentifier: String read fexternalIdentifier write fexternalIdentifier;
    property callbackAddress: String read FcallbackAddress write fcallbackAddress;
    property paymentInfo: TMateraPaymentInfo read FpaymentInfo write FpaymentInfo;
    property recipients: TMateraRecipientsArray read Frecipients write frecipients;
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

  { TMateraFinancialStatement }

  TMateraFinancialStatement = class(TACBrPIXSchema)
  private
    fAuthorizationDetails: TMateraAuthorizationDetails;
    fstatus: TMateraTransactionStatus;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraFinancialStatement);

    property status: TMateraTransactionStatus read fstatus write fstatus;
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
    fqrcodeURL: String;
    freference: String;
    ftextContent: String;
    fdynamicQrCodeType: String;
    fGeneratedImage: TMateraGeneratedImage;
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

    property reference: String read freference write freference;
    property qrcodeURL: String read fqrcodeURL write fqrcodeURL;
    property textContent: String read ftextContent write ftextContent;
    property dynamicQrCodeType: String read fdynamicQrCodeType write fdynamicQrCodeType;
    property generateImage: TMateraGeneratedImage read fGeneratedImage write fGeneratedImage;
  end;

  { TMateraCouponDetails }

  TMateraCouponDetails = class(TACBrPIXSchema)
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
    procedure Assign(aSource: TMateraCouponDetails);

    property couponId: String read fcouponId write fcouponId;
    property seller: String read fseller write fseller;
    property description: String read fdescription write fdescription;
  end;

  { TMateraQRCodeResponse }

  TMateraQRCodeResponse = class(TACBrPIXSchema)
  private
    fboletoUrl: String;
    fdueDate: TDateTime;
    fcreditCardToken: String;
    fdiscountAmount: Currency;
    fexpirationDate: TDateTime;
    fexternalIdentifier: String;
    fpaidAmount: Currency;
    frecipientDescription: String;
    fsenderAccountId: String;
    ftotalAmount: Currency;
    ftransactionDate: TDateTime;
    ftransactionId: String;
    ftransactionType: String;
    ftypeableLine: String;
    fcouponDetails: TMateraCouponDetails;
    ffinancialStatement: TMateraFinancialStatement;
    finstantPayment: TMateraInstantPaymentQRCodeResponse;
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

    property senderAccountId: String read fsenderAccountId write fsenderAccountId;
    property creditCardToken: String read fcreditCardToken write fcreditCardToken;
    property boletoUrl: String read fboletoUrl write fboletoUrl;
    property typeableLine: String read ftypeableLine write ftypeableLine;
    property dueDate: TDateTime read fdueDate write fdueDate;
    property discountAmount: Currency read fdiscountAmount write fdiscountAmount;
    property recipientDescription: String read frecipientDescription write frecipientDescription;
    property expirationDate: TDateTime read fexpirationDate write fexpirationDate;
    property transactionId: String read ftransactionId write ftransactionId;
    property externalIdentifier: String read fexternalIdentifier write fexternalIdentifier;
    property transactionDate: TDateTime read ftransactionDate write ftransactionDate;
    property transactionType: String read ftransactionType write ftransactionType;
    property totalAmount: Currency read ftotalAmount write ftotalAmount;
    property paidAmount: Currency read fpaidAmount write fpaidAmount;
    property couponDetails: TMateraCouponDetails read fcouponDetails write fcouponDetails;
    property instantPayment: TMateraInstantPaymentQRCodeResponse read finstantPayment write finstantPayment;
    property financialStatement: TMateraFinancialStatement read ffinancialStatement write ffinancialStatement;
  end;

  { TMateraReturnCode }

  TMateraReturnCode = class(TACBrPIXSchema)
  private
    fcode: String;
    fdescription: String;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraReturnCode);

    property code: String read fcode write fcode;
    property description: String read fdescription write fdescription;
  end;

  { TMateraReturnCodeArray }

  TMateraReturnCodeArray = class(TACBrPIXSchemaArray)
  private
    function GetItem(aIndex: Integer): TMateraReturnCode;
    procedure SetItem(aIndex: Integer; aValue: TMateraReturnCode);
  protected
    function NewSchema: TACBrPIXSchema; override;
  public
    Function Add(aItem: TMateraReturnCode): Integer;
    Procedure Insert(aIndex: Integer; aItem: TMateraReturnCode);
    function New: TMateraReturnCode;
    property Items[aIndex: Integer]: TMateraReturnCode read GetItem write SetItem; default;
  end;

  { TMateraReturnCodesQueryResponse }

  TMateraReturnCodesQueryResponse = class(TACBrPIXSchema)
  private
    freturnCodes: TMateraReturnCodeArray;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraReturnCodesQueryResponse);

    property returnCodes: TMateraReturnCodeArray read freturnCodes write freturnCodes;
  end;


  { TMateraDevolucaoRequest }

  TMateraDevolucaoRequest = class(TACBrPIXSchema)
  private
    fadditionalInformation: String;
    famount: currency;
    fexternalIdentifier: String;
    fmediatorFee: currency;
    fperformDebit: Boolean;
    freturnReasonCode: String;
    freturnReasonInformation: String;
    freturnType: TMaterareturnType;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraDevolucaoRequest);

    property externalIdentifier: String read fexternalIdentifier write fexternalIdentifier;
    property amount: currency read famount write famount;
    property additionalInformation: String read fadditionalInformation write fadditionalInformation;
    property returnReasonCode: String read freturnReasonCode write freturnReasonCode;
    property returnReasonInformation: String read freturnReasonInformation write freturnReasonInformation;
    property mediatorFee: currency read fmediatorFee write fmediatorFee;
    property returnType: TMaterareturnType read freturnType write freturnType;
    property performDebit: Boolean read fperformDebit write fperformDebit;
  end;

  { TMateraDevolucaoResponse }

  TMateraDevolucaoResponse = class(TACBrPIXSchema)
  private
    ftransactionId: String;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraDevolucaoResponse);

    property transactionId: String read ftransactionId write ftransactionId;
  end;

  { TMateraDestinationAccount }

  TMateraDestinationAccount = class(TACBrPIXSchema)
  private
    faccount: String;
    faccountType: TMateraAccountTypeDestination;
    fbranch: String;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraDestinationAccount);

    property branch: String read fbranch write fbranch;
    property account: String read faccount write faccount;
    property accountType: TMateraAccountTypeDestination read faccountType write faccountType;
  end;

  { TMateraCoupon }

  TMateraCoupon = class(TACBrPIXSchema)
  private
    faccountHolderId: String;
    faccountId: String;
    fcouponId: String;
    fcouponMerchantId: String;
    fdescription: String;
    fdiscount: Double;
    fdueDate: TDateTime;
    fmaxUse: Integer;
    fminExpenseValue: Double;
    fseller: String;
    fstatus: TMateraActiveStatus;
    fuseCount: Integer;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraCoupon);

    property couponId: String read fcouponId write fcouponId;
    property dueDate: TDateTime read fdueDate write fdueDate;
    property maxUse: Integer read fmaxUse write fmaxUse;
    property useCount: Integer read fuseCount write fuseCount;
    property discount: Double read fdiscount write fdiscount;
    property minExpenseValue: Double read fminExpenseValue write fminExpenseValue;
    property seller: String read fseller write fseller;
    property description: String read fdescription write fdescription;
    property status: TMateraActiveStatus read fstatus write fstatus;
    property accountId: String read faccountId write faccountId;
    property accountHolderId: String read faccountHolderId write faccountHolderId;
    property couponMerchantId: String read fcouponMerchantId write fcouponMerchantId;
  end;

  { TMateraDrawee }

  TMateraDrawee = class(TACBrPIXSchema)
  private
    fname: String;
    ftaxIdentifier: TMateraTaxIdentifierBasic;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraDrawee);

    property name: String read fname write fname;
    property taxIdentifier: TMateraTaxIdentifierBasic read ftaxIdentifier write ftaxIdentifier;
  end;

  { TMateraWithdrawProviders }

  TMateraWithdrawProviders = class(TACBrPIXSchema)
  private
    fagentModality: TMateraWithdrawAgentType;
    fserviceProvider: String;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraWithdrawProviders);

    property agentModality: TMateraWithdrawAgentType read fagentModality write fagentModality;
    property serviceProvider: String read fserviceProvider write fserviceProvider;
  end;

  { TMateraCashValue }

  TMateraCashValue = class(TACBrPIXSchema)
  private
    fallowValueChange: Boolean;
    fcashValueType: TMateraCashValueType;
    fvalue: String;
    fwithdrawProviders: TMateraWithdrawProviders;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraCashValue);

    property cashValueType: TMateraCashValueType read fcashValueType write fcashValueType;
    property value: String read fvalue write fvalue;
    property allowValueChange: Boolean read fallowValueChange write fallowValueChange;
    property withdrawProviders: TMateraWithdrawProviders read fwithdrawProviders write fwithdrawProviders;
  end;

  { TMateraAliasAccountHolder }

  TMateraAliasAccountHolder = class(TACBrPIXSchema)
  private
    fname: string;
    ftaxIdentifier: TMateraTaxIdentifierBasic;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraAliasAccountHolder);

    property taxIdentifier: TMateraTaxIdentifierBasic read ftaxIdentifier write ftaxIdentifier;
    property name: string read fname write fname;
  end;

  { TMateraCounter }

  TMateraCounter = class(TACBrPIXSchema)
  private
    fby: string;
    fd3: integer;
    fd30: integer;
    fm6: integer;
    ftype_: TMateraAntifraudCounter;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraCounter);

    property type_: TMateraAntifraudCounter read ftype_ write ftype_;
    property by: string read fby write fby;
    property d3: integer read fd3 write fd3;
    property d30: integer read fd30 write fd30;
    property m6: integer read fm6 write fm6;
  end;

  { TMateraCounterArray }

  TMateraCounterArray = class(TACBrPIXSchemaArray)
  private
    function GetItem(aIndex: Integer): TMateraCounter;
    procedure SetItem(aIndex: Integer; aValue: TMateraCounter);
  protected
    function NewSchema: TACBrPIXSchema; override;
  public
    Function Add(aItem: TMateraCounter): Integer;
    Procedure Insert(aIndex: Integer; aItem: TMateraCounter);
    function New: TMateraCounter;
    property Items[aIndex: Integer]: TMateraCounter read GetItem write SetItem; default;
  end;

  { TMateraAntiFraudClearingInfo }

  TMateraAntiFraudClearingInfo = class(TACBrPIXSchema)
  private
    fcounters: TMateraCounterArray;
    flastUpdated: TDateTime;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraAntiFraudClearingInfo);

    property lastUpdated: TDateTime read flastUpdated write flastUpdated;
    property counters: TMateraCounterArray read fcounters write fcounters;
  end;

  { TMateraPSP }

  TMateraPSP = class(TACBrPIXSchema)
  private
    fcountry: String;
    fcurrencies: TSplitResult;
    fid: String;
    fname: String;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraPSP);

    property id: String read fid write fid;
    property name: String read fname write fname;
    property country: String read fcountry write fcountry;
    property currencies: TSplitResult read fcurrencies write fcurrencies;
  end;

  { TMateraAliasResponse }

  TMateraAliasResponse = class(TACBrPIXSchema)
  private
    faccountDestination: TMateraDestinationAccount;
    faliasAccountHolder: TMateraAliasAccountHolder;
    faliasType: TMateraAliasType;
    falias_: string;
    fantiFraudClearingInfo: TMateraAntiFraudClearingInfo;
    fcreationDate: TDateTime;
    fendtoEndId: string;
    fpsp: TMateraPSP;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraAliasResponse);

    property alias_: string read falias_ write falias_;
    property aliasType: TMateraAliasType read faliasType write faliasType;
    property aliasAccountHolder: TMateraAliasAccountHolder read faliasAccountHolder write faliasAccountHolder;
    property accountDestination: TMateraDestinationAccount read faccountDestination write faccountDestination;
    property psp: TMateraPSP read fpsp write fpsp;
    property endtoEndId: string read fendtoEndId write fendtoEndId;
    property creationDate: TDateTime read fcreationDate write fcreationDate;
    property antiFraudClearingInfo: TMateraAntiFraudClearingInfo read fantiFraudClearingInfo write fantiFraudClearingInfo;
  end;

  { TMateraUtilitiesBasic }

  TMateraUtilitiesBasic = class(TACBrPIXSchema)
  private
    fdocumentNumber: String;
    fbarcode: String;
    fbeneficiaryTaxIdentifier: String;
    ftypeableLine: String;
    fdueDate: TDateTime;
    fpaidAmount: currency;
    fhistoryCode: String;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;

    property documentNumber: String read fdocumentNumber write fdocumentNumber;
    property barcode: String read fbarcode write fbarcode;
    property beneficiaryTaxIdentifier: String read fbeneficiaryTaxIdentifier write fbeneficiaryTaxIdentifier;
    property typeableLine: String read ftypeableLine write ftypeableLine;
    property dueDate: TDateTime read fdueDate write fdueDate;
    property paidAmount: currency read fpaidAmount write fpaidAmount;
    property historyCode: String read fhistoryCode write fhistoryCode;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraUtilitiesBasic);
  end;

  TMateraUtilitiesTO = class(TMateraUtilitiesBasic)
  public
    property documentNumber;
    property barcode;
    property beneficiaryTaxIdentifier;
    property typeableLine;
    property dueDate;
    property paidAmount;
    property historyCode;
  end;

  TMateraUtilitiesPayment = class(TMateraUtilitiesBasic)
  public
    property documentNumber;
    property barcode;
    property beneficiaryTaxIdentifier;
    property typeableLine;
    property dueDate;
    property paidAmount;
  end;

  { TMateraBoletoBasic }

  TMateraBoletoBasic = class(TACBrPIXSchema)
  private
    fauthenticationCode: String;
    fbankAuthentication: String;
    fbarcode: String;
    fbeneficiaryTaxIdentifier: String;
    fdiscount: Currency;
    fdocumentNumber: String;
    fdueDate: TDateTime;
    ffineAmount: Currency;
    fhistoryCode: String;
    finterestAmount: Currency;
    fpaidAmount: Currency;
    fstatus: TMateraTransactionStatus;
    ftypeableLine: String;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;

    property barcode: String read fbarcode write fbarcode;
    property interestAmount: Currency read finterestAmount write finterestAmount;
    property paidAmount: Currency read fpaidAmount write fpaidAmount;
    property fineAmount: Currency read ffineAmount write ffineAmount;
    property documentNumber: String read fdocumentNumber write fdocumentNumber;
    property historyCode: String read fhistoryCode write fhistoryCode;
    property typeableLine: String read ftypeableLine write ftypeableLine;
    property beneficiaryTaxIdentifier: String read fbeneficiaryTaxIdentifier write fbeneficiaryTaxIdentifier;
    property dueDate: TDateTime read fdueDate write fdueDate;
    property discount: Currency read fdiscount write fdiscount;
    property bankAuthentication: String read fbankAuthentication write fbankAuthentication;
    property authenticationCode: String read fauthenticationCode write fauthenticationCode;
    property status: TMateraTransactionStatus read fstatus write fstatus;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraBoletoBasic);
  end;

  { TMateraBoletoTO }

  TMateraBoletoTO = class(TMateraBoletoBasic)
  public
    property barcode;
    property interestAmount;
    property fineAmount;
    property documentNumber;
    property historyCode;
    property typeableLine;
    property beneficiaryTaxIdentifier;
    property dueDate;
    property discount;
  end;

  { TMateraBoletoPayment }

  TMateraBoletoPayment = class(TMateraBoletoBasic)
  public
    property barcode;
    property typeableLine;
    property paidAmount;
    property dueDate;
    property interestAmount;
    property fineAmount;
    property discount;
    property status;
    property bankAuthentication;
    property beneficiaryTaxIdentifier;
    property authenticationCode;
  end;

  { TMateraBankTransfer }

  TMateraBankTransfer = class(TACBrPIXSchema)
  private
    faccountDestination: String;
    faccountDigitDestination: String;
    faccountTypeDestination: TMateraAccountTypeDestination;
    fbankDestination: String;
    fbranchDestination: String;
    fhistoryCode: String;
    fname: String;
    fpersonType: String;
    fpurposeCode: String;
    ftaxIdentifier: TMateraTaxIdentifier;
    ftransferMethod: String;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraBankTransfer);

    property bankDestination: String read fbankDestination write fbankDestination;
    property branchDestination: String read fbranchDestination write fbranchDestination;
    property accountDestination: String read faccountDestination write faccountDestination;
    property taxIdentifier: TMateraTaxIdentifier read ftaxIdentifier write ftaxIdentifier;
    property personType: String read fpersonType write fpersonType;
    property name: String read fname write fname;
    property accountTypeDestination: TMateraAccountTypeDestination read faccountTypeDestination write faccountTypeDestination;
    property historyCode: String read fhistoryCode write fhistoryCode;
    property purposeCode: String read fpurposeCode write fpurposeCode;
    property transferMethod: String read ftransferMethod write ftransferMethod;
    property accountDigitDestination: String read faccountDigitDestination write faccountDigitDestination;
  end;

  { TMateraExternal }

  TMateraExternal = class(TACBrPIXSchema)
  private
    fhistoryCode: String;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraExternal);

    property historyCode: String read fhistoryCode write fhistoryCode;
  end;

  { TMateraInstantPaymentRecipient }

  TMateraInstantPaymentRecipient = class(TACBrPIXSchema)
  private
    faccountDestination: TMateraDestinationAccount;
    falias_: String;
    fendToEndIdQuery: String;
    fpspid: String;
    fTaxIdentifierRequest: TMateraTaxIdentifierRequest;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraInstantPaymentRecipient);

    property pspid: String read fpspid write fpspid;
    property TaxIdentifierRequest: TMateraTaxIdentifierRequest read fTaxIdentifierRequest write fTaxIdentifierRequest;
    property alias_: String read falias_ write falias_;
    property endToEndIdQuery: String read fendToEndIdQuery write fendToEndIdQuery;
    property accountDestination: TMateraDestinationAccount read faccountDestination write faccountDestination;

  end;

  { TMateraTransactionValuesDetails }

  TMateraTransactionValuesDetails = class(TACBrPIXSchema)
  private
    fcashValue: Currency;
    ftransactionValue: Currency;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraTransactionValuesDetails);

    property transactionValue: Currency read ftransactionValue write ftransactionValue;
    property cashValue: Currency read fcashValue write fcashValue;
  end;

  { TMateraWithdrawAgent }

  TMateraWithdrawAgent = class(TACBrPIXSchema)
  private
    fmodality: TMateraWithdrawAgentType;
    fserviceProvider: string;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraWithdrawAgent);

    property modality: TMateraWithdrawAgentType read fmodality write fmodality;
    property serviceProvider: string read fserviceProvider write fserviceProvider;
  end;

  { TMateraInstantPaymentRequest }

  TMateraInstantPaymentRequest = class(TACBrPIXSchema)
  private
    frecipient: TMateraInstantPaymentRecipient;
    freceiverReconciliationIdentifier: String;
    finitiatingInstitution: String;
    finitiationForm: TMaterainitiationForm;
    fadditionalInformation: String;
    fqrcodeType: TMateraqrcodeType;
    fhistoryCode: String;
    finitiationProcedure: TMateraInitiationProcedure;
    finstructionPriority: TMateraInstructionPriority;
    ftransactionPurpose: TMateraTransactionPurpose;
    ftransactionValuesDetails: TMateraTransactionValuesDetails;
    finstructionType: TMateraInstructionType;
    fwithdrawAgent: TMateraWithdrawAgent;
    fperformDebit: Boolean;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraInstantPaymentRequest);

    property recipient: TMateraInstantPaymentRecipient read frecipient write frecipient;
    property receiverReconciliationIdentifier: String read freceiverReconciliationIdentifier write freceiverReconciliationIdentifier;
    property initiatingInstitution: String read finitiatingInstitution write finitiatingInstitution;
    property initiationForm: TMaterainitiationForm read finitiationForm write finitiationForm;
    property additionalInformation: String read fadditionalInformation write fadditionalInformation;
    property qrcodeType: TMateraqrcodeType read fqrcodeType write fqrcodeType;
    property historyCode: String read fhistoryCode write fhistoryCode;
    property initiationProcedure: TMateraInitiationProcedure read finitiationProcedure write finitiationProcedure;
    property instructionPriority: TMateraInstructionPriority read finstructionPriority write finstructionPriority;
    property transactionPurpose: TMateraTransactionPurpose read ftransactionPurpose write ftransactionPurpose;
    property transactionValuesDetails: TMateraTransactionValuesDetails read ftransactionValuesDetails write ftransactionValuesDetails;
    property instructionType: TMateraInstructionType read finstructionType write finstructionType;
    property withdrawAgent: TMateraWithdrawAgent read fwithdrawAgent write fwithdrawAgent;
    property performDebit: Boolean read fperformDebit write fperformDebit;
  end;

  { TMateraWithdrawInfo }

  TMateraWithdrawInfo = class(TACBrPIXSchema)
  private
    fbankTransfer: TMateraBankTransfer;
    fboleto: TMateraBoletoTO;
    fexternal: TMateraExternal;
    ffutureDate: TDateTime;
    finstantPayment: TMateraInstantPaymentRequest;
    fsenderComment: String;
    futilities: TMateraUtilitiesTO;
    fwithdrawType: TMateraWithdrawType;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraWithdrawInfo);

    property withdrawType: TMateraWithdrawType read fwithdrawType write fwithdrawType;
    property bankTransfer: TMateraBankTransfer read fbankTransfer write fbankTransfer;
    property boleto: TMateraBoletoTO read fboleto write fboleto;
    property utilities: TMateraUtilitiesTO read futilities write futilities;
    property external_: TMateraExternal read fexternal write fexternal;
    property instantPayment: TMateraInstantPaymentRequest read finstantPayment write finstantPayment;
    property senderComment: String read fsenderComment write fsenderComment;
    property futureDate: TDateTime read ffutureDate write ffutureDate;
  end;

  { TMateraRetiradaRequest }

  TMateraRetiradaRequest = class(TACBrPIXSchema)
  private
    fcurrency: String;
    fexternalIdentifier: String;
    fmediatorFee: Currency;
    ftotalAmount: Currency;
    fwithdrawInfo: TMateraWithdrawInfo;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraRetiradaRequest);

    property totalAmount: Currency read ftotalAmount write ftotalAmount;
    property mediatorFee: Currency read fmediatorFee write fmediatorFee;
    property currency: String read fcurrency write fcurrency;
    property withdrawInfo: TMateraWithdrawInfo read fwithdrawInfo write fwithdrawInfo;
    property externalIdentifier: String read fexternalIdentifier write fexternalIdentifier;
  end;

  { TMateraParticipantInstantPayment }

  TMateraParticipantInstantPayment = class(TACBrPIXSchema)
  private
    faccount: TMateraDestinationAccount;
    falias: String;
    fname: String;
    fpsp: TMateraPSP;
    ftaxIdentifier: TMateraTaxIdentifierBasic;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraParticipantInstantPayment);

    property alias_: String read falias write falias;
    property name: String read fname write fname;
    property taxIdentifier: TMateraTaxIdentifierBasic read ftaxIdentifier write ftaxIdentifier;
    property account: TMateraDestinationAccount read faccount write faccount;
    property psp: TMateraPSP read fpsp write fpsp;
  end;

  { TMateraPaymentReceivedBasic }

  TMateraPaymentReceivedBasic = class(TACBrPIXSchema)
  private
    fadditionalInformation: String;
    fendToEndId: String;
    flegacyTransactionId: String;
    freceivedAmount: Currency;
    fsender: TMateraParticipantInstantPayment;
    ftransactionTimestamp: String;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;

    property sender: TMateraParticipantInstantPayment read fsender write fsender;
    property receivedAmount: Currency read freceivedAmount write freceivedAmount;
    property transactionTimestamp: String read ftransactionTimestamp write ftransactionTimestamp;
    property legacyTransactionId: String read flegacyTransactionId write flegacyTransactionId;
    property endToEndId: String read fendToEndId write fendToEndId;
    property additionalInformation: String read fadditionalInformation write fadditionalInformation;
  public
    constructor Create(const aObjectName: String = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraPaymentReceivedBasic);
  end;

  { TMateraPaymentReceived }

  TMateraPaymentReceived = class(TMateraPaymentReceivedBasic)
  public
    property sender;
    property receivedAmount;
    property transactionTimestamp;
    property legacyTransactionId;
    property endToEndId;
    property additionalInformation;
  end;

  { TMateraOriginInstantPaymentTransactionResponse }

  TMateraOriginInstantPaymentTransactionResponse = class(TMateraPaymentReceivedBasic)
  public
    property sender;
    property endToEndId;
  end;

  { TMateraPaymentReceivedArray }

  TMateraPaymentReceivedArray = class(TACBrPIXSchemaArray)
  private
    function GetItem(aIndex: Integer): TMateraPaymentReceived;
    procedure SetItem(aIndex: Integer; aValue: TMateraPaymentReceived);
  protected
    function NewSchema: TACBrPIXSchema; override;
  public
    Function Add(aItem: TMateraPaymentReceived): Integer;
    Procedure Insert(aIndex: Integer; aItem: TMateraPaymentReceived);
    function New: TMateraPaymentReceived;
    property Items[aIndex: Integer]: TMateraPaymentReceived read GetItem write SetItem; default;
  end;

  { TMateraRejectionReasonInstantPayment }

  TMateraRejectionReasonInstantPayment = class(TACBrPIXSchema)
  private
    fcode: String;
    fdescription: String;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraRejectionReasonInstantPayment);

    property code: String read fcode write fcode;
    property description: String read fdescription write fdescription;
  end;

  { TMateraInstantPaymentTransactionReturnReasonInformation }

  TMateraInstantPaymentTransactionReturnReasonInformation = class(TACBrPIXSchema)
  private
    fadditionalInformation: String;
    freasonCode: String;
    freasonDescription: String;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraInstantPaymentTransactionReturnReasonInformation);

    property reasonCode: String read freasonCode write freasonCode;
    property reasonDescription: String read freasonDescription write freasonDescription;
    property additionalInformation: String read fadditionalInformation write fadditionalInformation;
  end;

  { TMateraInstantPaymentTransactionReturnInfo }

  TMateraInstantPaymentTransactionReturnInfo = class(TACBrPIXSchema)
  private
    foriginalEndToEndId: String;
    foriginalInstantPaymentId: String;
    freturnReasonInformation: TMateraInstantPaymentTransactionReturnReasonInformation;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraInstantPaymentTransactionReturnInfo);

    property originalEndToEndId: String read foriginalEndToEndId write foriginalEndToEndId;
    property originalInstantPaymentId: String read foriginalInstantPaymentId write foriginalInstantPaymentId;
    property returnReasonInformation: TMateraInstantPaymentTransactionReturnReasonInformation
      read freturnReasonInformation write freturnReasonInformation;
  end;

  { TMateraOriginDepositTransactionResponse }

  TMateraOriginDepositTransactionResponse = class(TACBrPIXSchema)
  private
    fadditionalInformation: String;
    ftotalAmount: Double;
    ftransactionDate: String;
    ftransactionId: String;
    foriginInstantPaymentTransaction: TMateraOriginInstantPaymentTransactionResponse;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraOriginDepositTransactionResponse);

    property transactionId: String read ftransactionId write ftransactionId;
    property transactionDate: String read ftransactionDate write ftransactionDate;
    property totalAmount: Double read ftotalAmount write ftotalAmount;
    property additionalInformation: String read fadditionalInformation write fadditionalInformation;
    property originInstantPaymentTransaction: TMateraOriginInstantPaymentTransactionResponse
      read foriginInstantPaymentTransaction write foriginInstantPaymentTransaction;
  end;

  { TMateraRetiradaResponse }

  TMateraRetiradaResponse = class(TACBrPIXSchema)
  private
    ftransactionId: String;
    fexternalIdentifier: String;
    fstatus: TMateraTransactionStatus;
    freceipt: String;
    fauthenticationCode: String;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraRetiradaResponse);

    property transactionId: String read ftransactionId write ftransactionId;
    property externalIdentifier: String read fexternalIdentifier write fexternalIdentifier;
    property status: TMateraTransactionStatus read fstatus write fstatus;
    property receipt: String read freceipt write freceipt;
    property authenticationCode: String read fauthenticationCode write fauthenticationCode;
  end;

  { TMateraInstantPaymentTransactionResponse }

  TMateraInstantPaymentTransactionResponse = class(TACBrPIXSchema)
  private
    fendToEndId: String;
    fadditionalInformation: String;
    fsender: TMateraParticipantInstantPayment;
    finstantPaymentCashValue: TMateraCashValue;
    fpaymentReceived: TMateraPaymentReceivedArray;
    frecipient: TMateraParticipantInstantPayment;
    frejectionReason: TMateraRejectionReasonInstantPayment;
    freturnInfo: TMateraInstantPaymentTransactionReturnInfo;
    foriginDepositTransaction: TMateraOriginDepositTransactionResponse;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraInstantPaymentTransactionResponse);

    property endToEndId: String read fendToEndId write fendToEndId;
    property additionalInformation: String read fadditionalInformation write fadditionalInformation;

    property sender: TMateraParticipantInstantPayment read fsender write fsender;
    property instantPaymentCashValue: TMateraCashValue read finstantPaymentCashValue write finstantPaymentCashValue;
    property paymentReceived: TMateraPaymentReceivedArray read fpaymentReceived write fpaymentReceived;
    property recipient: TMateraParticipantInstantPayment read frecipient write frecipient;
    property rejectionReason: TMateraRejectionReasonInstantPayment read frejectionReason write frejectionReason;
    property returnInfo: TMateraInstantPaymentTransactionReturnInfo read freturnInfo write freturnInfo;
    property originDepositTransaction: TMateraOriginDepositTransactionResponse
      read foriginDepositTransaction write foriginDepositTransaction;
  end;

  { TMateraCancelPaymentTransactionResponse }

  TMateraCancelPaymentTransactionResponse = class(TACBrPIXSchema)
  private
    fexternalProtocolId: String;
    freason: String;
    fsourceSystem: String;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraCancelPaymentTransactionResponse);

    property reason: String read freason write freason;
    property sourceSystem: String read fsourceSystem write fsourceSystem;
    property externalProtocolId: String read fexternalProtocolId write fexternalProtocolId;
  end;

  { TMateraTransactionResponse }

  TMateraTransactionResponse = class(TACBrPIXSchema)
  private
    faccountId: String;
    faccountHolderId: String;
    fbankTransfer: TMateraBankTransfer;
    fboleto: TMateraBoletoPayment;
    fcancelPaymentTransaction: TMateraCancelPaymentTransactionResponse;
    fcancelTransactionId: String;
    fcounterPart: TMateraAccountHolderResponse;
    fcoupon: TMateraCoupon;
    fcurrentAmount: Double;
    fdiscountAmount: Double;
    fdocUrl: String;
    fdrawee: TMateraDrawee;
    femail: String;
    fendToEndId: String;
    ffirstInstallmentValue: Double;
    finstallmentQuantity: Double;
    finstantPayment: TMateraInstantPaymentTransactionResponse;
    fmobilePhone: TMateraMobilePhone;
    fotherInstallmentValues: Double;
    fpaidAmount: Double;
    frecipientDescription: String;
    fsideAccountId: String;
    ftotalAmount: Currency;
    ftransactionDate: TDateTime;
    ftransactionId: String;
    ftransactionStatus: TMateraTransactionStatus;
    ftransactionType: String;
    futilities: TMateraUtilitiesPayment;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraTransactionResponse);
                                                                                  
    property accountId: String read faccountId write faccountId;
    property accountHolderId: String read faccountHolderId write faccountHolderId;
    property transactionId: String read ftransactionId write ftransactionId;
    property transactionDate: TDateTime read ftransactionDate write ftransactionDate;
    property transactionType: String read ftransactionType write ftransactionType;
    property transactionStatus: TMateraTransactionStatus read ftransactionStatus write ftransactionStatus;
    property totalAmount: Currency read ftotalAmount write ftotalAmount;
    property email: String read femail write femail;
    property paidAmount: Double read fpaidAmount write fpaidAmount;
    property discountAmount: Double read fdiscountAmount write fdiscountAmount;
    property currentAmount: Double read fcurrentAmount write fcurrentAmount;
    property docUrl: String read fdocUrl write fdocUrl;
    property sideAccountId: String read fsideAccountId write fsideAccountId;
    property endToEndId: String read fendToEndId write fendToEndId;
    property recipientDescription: String read frecipientDescription write frecipientDescription;
    property installmentQuantity: Double read finstallmentQuantity write finstallmentQuantity;
    property firstInstallmentValue: Double read ffirstInstallmentValue write ffirstInstallmentValue;
    property otherInstallmentValues: Double read fotherInstallmentValues write fotherInstallmentValues;
    property cancelTransactionId: String read fcancelTransactionId write fcancelTransactionId;

    property counterPart: TMateraAccountHolderResponse read fcounterPart write fcounterPart;
    property mobilePhone: TMateraMobilePhone read fmobilePhone write fmobilePhone;
    property coupon: TMateraCoupon read fcoupon write fcoupon;
    property boleto: TMateraBoletoPayment read fboleto write fboleto;
    property drawee: TMateraDrawee read fdrawee write fdrawee;
    property bankTransfer: TMateraBankTransfer read fbankTransfer write fbankTransfer;
    property instantPayment: TMateraInstantPaymentTransactionResponse read finstantPayment write finstantPayment;
    property utilities: TMateraUtilitiesPayment read futilities write futilities;
    property cancelPaymentTransaction: TMateraCancelPaymentTransactionResponse
      read fcancelPaymentTransaction write fcancelPaymentTransaction;
  end;

  { TMateraTransactionResponseArray }

  TMateraTransactionResponseArray = class(TACBrPIXSchemaArray)
  private
    fhashNextPage: String;
    function GetItem(aIndex: Integer): TMateraTransactionResponse;
    procedure SetItem(aIndex: Integer; aValue: TMateraTransactionResponse);
  protected
    function NewSchema: TACBrPIXSchema; override;
  public
    function Add(aItem: TMateraTransactionResponse): Integer;
    procedure Insert(aIndex: Integer; aItem: TMateraTransactionResponse);
    function New: TMateraTransactionResponse;
    
    procedure WriteToJSon(AJSon: TACBrJSONObject); override;
    procedure ReadFromJSon(AJSon: TACBrJSONObject); override;

    property Items[aIndex: Integer]: TMateraTransactionResponse read GetItem write SetItem; default;
    property hashNextPage: String read fhashNextPage write fhashNextPage;
  end;

  { TMateraSender }

  TMateraSender = class(TACBrPIXSchema)
  private
    faccount: TMateraAccountIdentifier;
    fclient: TMateraAccountHolder;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraSender);

    property account: TMateraAccountIdentifier read faccount write faccount;
    property client: TMateraAccountHolder read fclient write fclient;
  end;

  { TMateraTransferenciaInternaRequest }

  TMateraTransferenciaInternaRequest = class(TACBrPIXSchema)
  private
    ftotalAmount: Currency;
    fcurrency: String;
    fpaymentInfo: TMateraPaymentInfo;
    fsender: TMateraSender;
    fmyAccount: TMateraAccountIdentifier;
    frecipients: TMateraRecipientsArray;
    fexternalIdentifier: String;
    fcallbackAddress: string;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraTransferenciaInternaRequest);

    property totalAmount: Currency read ftotalAmount write ftotalAmount;
    property currency: String read fcurrency write fcurrency;
    property paymentInfo: TMateraPaymentInfo read fpaymentInfo write fpaymentInfo;
    property sender: TMateraSender read fsender write fsender;
    property myAccount: TMateraAccountIdentifier read fmyAccount write fmyaccount;
    property recipients: TMateraRecipientsArray read frecipients write frecipients;
    property externalIdentifier: String read fexternalIdentifier write fexternalIdentifier;
    property callbackAddress: string read fcallbackAddress write fcallbackAddress;
  end;

  { TMateraPaymentResponse }

  TMateraPaymentResponse = class(TACBrPIXSchema)
  private
    fboletoUrl: String;
    fcouponDetails: TMateraCouponDetails;
    fcreditCardToken: String;
    fdiscountAmount: Currency;
    fdueDate: TDateTime;
    fexpirationDate: TDateTime;
    fexternalIdentifier: String;
    ffinancialStatement: TMateraFinancialStatement;
    finstantPayment: TMateraInstantPaymentQRCodeResponse;
    fpaidAmount: Currency;
    frecipientDescription: String;
    fsenderAccountId: String;
    ftotalAmount: Currency;
    ftransactionDate: TDateTime;
    ftransactionId: String;
    ftransactionType: TSplitResult;
    ftypeableLine: String;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraPaymentResponse);

    property transactionId: String read ftransactionId write ftransactionId;
    property externalIdentifier: String read fexternalIdentifier write fexternalIdentifier;
    property senderAccountId: String read fsenderAccountId write fsenderAccountId;
    property creditCardToken: String read fcreditCardToken write fcreditCardToken;
    property financialStatement: TMateraFinancialStatement read ffinancialStatement write ffinancialStatement;
    property boletoUrl: String read fboletoUrl write fboletoUrl;
    property typeableLine: String read ftypeableLine write ftypeableLine;
    property dueDate: TDateTime read fdueDate write fdueDate;
    property transactionDate: TDateTime read ftransactionDate write ftransactionDate;
    property transactionType: TSplitResult read ftransactionType write ftransactionType;
    property totalAmount: Currency read ftotalAmount write ftotalAmount;
    property paidAmount: Currency read fpaidAmount write fpaidAmount;
    property discountAmount: Currency read fdiscountAmount write fdiscountAmount;
    property recipientDescription: String read frecipientDescription write frecipientDescription;
    property couponDetails: TMateraCouponDetails read fcouponDetails write fcouponDetails;
    property expirationDate: TDateTime read fexpirationDate write fexpirationDate;
    property instantPayment: TMateraInstantPaymentQRCodeResponse read finstantPayment write finstantPayment;

  end;

  { TMateraCancelPaymentRequest }

  TMateraCancelPaymentRequest = class(TACBrPIXSchema)
  private
    freason: String;
    fstatementDeletionMode: TMaterastatementDeletionMode;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraCancelPaymentRequest);

    property statementDeletionMode: TMaterastatementDeletionMode read fstatementDeletionMode write fstatementDeletionMode;
    property reason: String read freason write freason;
  end;

  { TMateraCancelPaymentResponse }

  TMateraCancelPaymentResponse = class(TACBrPIXSchema)
  private
    ftransactionalId: String;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraCancelPaymentResponse);

    property transactionalId: String read ftransactionalId write ftransactionalId;
  end;

  { TMateraCounterpart }

  TMateraCounterpart = class(TACBrPIXSchema)
  private
    faccountDestination: String;
    fbankDestination: String;
    fbranchDestination: String;
    fclientType: TMateraClientType;
    fname: String;
    fTaxIdentifier: TMateraTaxIdentifier;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraCounterpart);

    property bankDestination: String read fbankDestination write fbankDestination;
    property branchDestination: String read fbranchDestination write fbranchDestination;
    property accountDestination: String read faccountDestination write faccountDestination;
    property clientType: TMateraClientType read fclientType write fclientType;
    property name: String read fname write fname;
    property TaxIdentifier: TMateraTaxIdentifier read fTaxIdentifier write fTaxIdentifier;
  end;

  { TMateraStatementInstantPaymentCashValueData }

  TMateraStatementInstantPaymentCashValueData = class(TACBrPIXSchema)
  private
    fcashValueType: TMateraStatementIPCashValueType;
    fvalue: currency;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraStatementInstantPaymentCashValueData);

    property cashValueType: TMateraStatementIPCashValueType read fcashValueType write fcashValueType;
    property value: currency read fvalue write fvalue;
  end;

  { TStatementInstantPaymentCashValueDataArray }

  TStatementInstantPaymentCashValueDataArray = class(TACBrPIXSchemaArray)
  private
    function GetItem(aIndex: Integer): TMateraStatementInstantPaymentCashValueData;
    procedure SetItem(aIndex: Integer; aValue: TMateraStatementInstantPaymentCashValueData);
  protected
    function NewSchema: TACBrPIXSchema; override;
  public
    Function Add(aItem: TMateraStatementInstantPaymentCashValueData): Integer;
    Procedure Insert(aIndex: Integer; aItem: TMateraStatementInstantPaymentCashValueData);
    function New: TMateraStatementInstantPaymentCashValueData;
    property Items[aIndex: Integer]: TMateraStatementInstantPaymentCashValueData read GetItem write SetItem; default;
  end;

  { TMateraStatementInstantPaymentCashValue }

  TMateraStatementInstantPaymentCashValue = class(TACBrPIXSchema)
  private
    fvalues: TStatementInstantPaymentCashValueDataArray;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraStatementInstantPaymentCashValue);

    property values: TStatementInstantPaymentCashValueDataArray read fvalues write fvalues;
  end;

  { TMaterastatementEntry }

  TMaterastatementEntry = class(TACBrPIXSchema)
  private
    fadditionalInfo: String;
    famount: Currency;
    fcomment: String;
    fcounterpart: TMateraCounterpart;
    fcreditDate: TDateTime;
    fdescription: String;
    fentryDate: TDateTime;
    fhistoryCode: Currency;
    finstantPaymentCashValue: TMateraStatementInstantPaymentCashValue;
    ftransactionId: String;
    ftransactionType: String;
    ftype_: TMaterastatementEntryType;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMaterastatementEntry);

    property entryDate: TDateTime read fentryDate write fentryDate;
    property creditDate: TDateTime read fcreditDate write fcreditDate;
    property transactionType: String read ftransactionType write ftransactionType;
    property description: String read fdescription write fdescription;
    property amount: Currency read famount write famount;
    property transactionId: String read ftransactionId write ftransactionId;
    property type_: TMaterastatementEntryType read ftype_ write ftype_;
    property comment: String read fcomment write fcomment;
    property historyCode: Currency read fhistoryCode write fhistoryCode;
    property additionalInfo: String read fadditionalInfo write fadditionalInfo;
    property counterpart: TMateraCounterpart read fcounterpart write fcounterpart;
    property instantPaymentCashValue: TMateraStatementInstantPaymentCashValue read finstantPaymentCashValue write finstantPaymentCashValue;
  end;

  { TMaterastatementEntryArray }

  TMaterastatementEntryArray = class(TACBrPIXSchemaArray)
  private
    function GetItem(aIndex: Integer): TMaterastatementEntry;
    procedure SetItem(aIndex: Integer; aValue: TMaterastatementEntry);
  protected
    function NewSchema: TACBrPIXSchema; override;
  public
    Function Add(aItem: TMaterastatementEntry): Integer;
    Procedure Insert(aIndex: Integer; aItem: TMaterastatementEntry);
    function New: TMaterastatementEntry;
    property Items[aIndex: Integer]: TMaterastatementEntry read GetItem write SetItem; default;
  end;

  { TMaterastatementResponse }

  TMaterastatementResponse = class(TACBrPIXSchema)
  private
    fstatement: TMaterastatementEntryArray;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMaterastatementResponse);

    property statement: TMaterastatementEntryArray read fstatement write fstatement;
  end;


  { TMateraBalanceResponse }

  TMateraBalanceResponse = class(TACBrPIXSchema)
  private
    faccountId: String;
    fautoInvest: Currency;
    favailable: Currency;
    favailableBalanceForTransactions: Currency;
    fblocked: Currency;
    fdate: TDateTime;
    femergencyAidBalance: Currency;
    ffuture: Currency;
    foverdraft: Currency;
    freal: Currency;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TMateraBalanceResponse);

    property accountId: String read faccountId write faccountId;
    property date: TDateTime read fdate write fdate;
    property real: Currency read freal write freal;
    property available: Currency read favailable write favailable;
    property future: Currency read ffuture write ffuture;
    property overdraft: Currency read foverdraft write foverdraft;
    property blocked: Currency read fblocked write fblocked;
    property autoInvest: Currency read fautoInvest write fautoInvest;
    property emergencyAidBalance: Currency read femergencyAidBalance write femergencyAidBalance;
    property availableBalanceForTransactions: Currency read favailableBalanceForTransactions write favailableBalanceForTransactions;
  end;

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

  function MateraDynamicQRCodeTypeToString(aType: TMateraDynamicQRCodeType): String;
  function StringToMateraDynamicQRCodeType(const aString: String): TMateraDynamicQRCodeType;

  function MateraTransactionStatusToString(aType: TMateraTransactionStatus): String;
  function StringToMateraTransactionStatus(const aString: String): TMateraTransactionStatus;

  function MateraReturnTypeToString(aType: TMateraReturnType): String;
  function StringToMateraReturnType(const aString: String): TMateraReturnType;

  function MateraAccountTypeDestinationToString(aType: TMateraAccountTypeDestination): String;
  function StringToMateraAccountTypeDestination(const aString: String): TMateraAccountTypeDestination;

  function MateraAntifraudCounterToString(aType: TMateraAntifraudCounter): String;
  function StringToMateraAntifraudCounter(const aString: String): TMateraAntifraudCounter;

  function MateraWithdrawTypeToString(aType: TMateraWithdrawType): String;
  function StringToMateraWithdrawType(const aString: String): TMateraWithdrawType;

  function MaterainitiationFormToString(aType: TMaterainitiationForm): String;
  function StringToMaterainitiationForm(const aString: String): TMaterainitiationForm;

  function MaterainitiationProcedureToString(aType: TMateraInitiationProcedure): String;
  function StringToMaterainitiationProcedure(const aString: String): TMateraInitiationProcedure;

  function MateraqrcodeTypeToString(aType: TMateraqrcodeType): String;
  function StringToMateraqrcodeType(const aString: String): TMateraqrcodeType;

  function MaterainstructionPriorityToString(aType: TMateraInstructionPriority): String;
  function StringToMaterainstructionPriority(const aString: String): TMateraInstructionPriority;

  function MateratransactionPurposeToString(aType: TMateraTransactionPurpose): String;
  function StringToMateratransactionPurpose(const aString: String): TMateraTransactionPurpose;

  function MaterainstructionTypeToString(aType: TMateraInstructionType): String;
  function StringToMaterainstructionType(const aString: String): TMateraInstructionType;

  function MateraWithdrawAgentTypeToString(aType: TMateraWithdrawAgentType): String;
  function StringToMateraWithdrawAgentType(const aString: String): TMateraWithdrawAgentType;

  function MateraCashValueTypeToString(aType: TMateraCashValueType): String;
  function StringToMateraCashValueType(const aString: String): TMateraCashValueType;

  function MaterastatementDeletionModeToString(aType: TMaterastatementDeletionMode): String;
  function StringToMaterastatementDeletionMode(const aString: String): TMaterastatementDeletionMode;

  function MaterastatementEntryTypeToString(aType: TMaterastatementEntryType): String;
  function StringToMaterastatementEntryType(const aString: String): TMaterastatementEntryType;

  function MateraStatementIPCashValueTypeToString(aType: TMateraStatementIPCashValueType): String;
  function StringToMateraStatementIPCashValueType(const aString: String): TMateraStatementIPCashValueType;

implementation

uses
  synautil,
  ACBrUtil.Base, ACBrUtil.DateTime, ACBrValidador;

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
    mdtUnknown: Result := 'UNKNOWN';
  else
    Result := EmptyStr;
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
  else if (s = 'UNKNOW') then
    Result := mdtUnknown
  else
    Result := mdtNone;
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

  if (s = 'PERSON') then
    Result := mctPerson
  else if (s = 'CORPORATE') then
    Result := mctCorporate
  else if (s = 'FOREIGNER') then
    Result := mctForeigner
  else
    Result := mctNone;
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

  if (s = 'ORDINARY') then
    Result := matOrdinary
  else if (s = 'OVERDRAFT_PROTECTED') then
    Result := matOverdraftProtected
  else if (s = 'COMMON') then
    Result := matCommon
  else if (s = 'UNLIMITED_ORDINARY') then
    Result := matUnlimitedOrdinary
  else
    Result := matNone;
end;

function MateraAccountStatusToString(aType: TMateraAccountStatus): String;
begin 
  case aType of
    mcsRegular: Result := 'REGULAR';
    mcsLocked: Result := 'LOCKED';
    mcsClosed: Result := 'CLOSED';
    mcsReserved: Result := 'RESERVED';
    mcsCreating: Result := 'CREATING';
    mcsError: Result := 'ERROR';
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
    Result := mcsRegular
  else if (s = 'LOCKED') then
    Result := mcsLocked
  else if (s = 'CLOSED') then
    Result := mcsClosed
  else if (s = 'RESERVED') then
    Result := mcsReserved
  else if (s = 'CREATING') then
    Result := mcsCreating
  else if (s = 'ERROR') then
    Result := mcsError
  else
    Result := mcsNone;
end;

function MateraActiveStatusToString(aType: TMateraActiveStatus): String;
begin
  case aType of
    macActive: Result := 'ACTIVE';
    macInactive: Result := 'INACTIVE';
    macBlocked: Result := 'BLOCKED';
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
  else if (s = 'INACTIVE') then
    Result := macInactive
  else if (s = 'BLOCKED') then
    Result := macBlocked

  else
    Result := macNone;;
end;

function MateraAliasTypeToString(aType: TMateraAliasType): String;
begin
  Result := 'EVP';
  
  case aType of
    malEVP: Result := 'EVP';
    //malTaxId: Result := 'TAX_ID';
    //malEmail:  Result := 'EMAIL';
    //malPhone:  Result := 'PHONE';
  else
    Result := EmptyStr;
  end;
end;

function StringToMateraAliasType(const aString: String): TMateraAliasType;
var
  s: String;
begin
  s := UpperCase(Trim(aString));
  if (s = 'TAX_ID') then
    Result := malEVP
  //else if (s = 'TAX_ID') then
  //  Result := malTaxId
  //else if (s = 'EMAIL') then
  //  Result := malEmail
  //else if (s = 'PHONE') then
  //  Result := malPhone
  else
    Result := malNone;
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
    Result := mastNone;
end;

function MateraDynamicQRCodeTypeToString(aType: TMateraDynamicQRCodeType): String;
begin
  case aType of
    mqtImmediate: Result := 'IMMEDIATE';
    mqtBillingDueDate: Result := 'BILLING_DUE_DATE';
  else
    Result := EmptyStr;
  end;
end;

function StringToMateraDynamicQRCodeType(const aString: String): TMateraDynamicQRCodeType;
var
  s: String;
begin
  s := UpperCase(Trim(aString));
  if (s = 'IMMEDIATE') then
    Result := mqtImmediate;
  if (s = 'BILLING_DUE_DATE') then
    Result := mqtBillingDueDate
  else
    Result := mqtNone;
end;

{ TMateraBalanceResponse }

procedure TMateraBalanceResponse.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (ASource is TMateraBalanceResponse) then
      Assign(TMateraBalanceResponse(ASource));
end;

procedure TMateraBalanceResponse.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('accountId', faccountId, False)
    .AddPair('autoInvest', fautoInvest, False)
    .AddPair('available', favailable, False)
    .AddPair('availableBalanceForTransactions', favailableBalanceForTransactions, False)
    .AddPair('blocked', fblocked, False)
    .AddPairISODate('date', fdate, False)
    .AddPair('emergencyAidBalance', femergencyAidBalance, False)
    .AddPair('future', ffuture, False)
    .AddPair('overdraft', foverdraft, False)
    .AddPair('real', freal, False);

end;

procedure TMateraBalanceResponse.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('accountId', faccountId)
    .Value('autoInvest', fautoInvest)
    .Value('available', favailable)
    .Value('availableBalanceForTransactions', favailableBalanceForTransactions)
    .Value('blocked', fblocked)
    .ValueISODate('date', fdate)
    .Value('emergencyAidBalance', femergencyAidBalance)
    .Value('future', ffuture)
    .Value('overdraft', foverdraft)
    .Value('real', freal);
end;

constructor TMateraBalanceResponse.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  Clear;
end;

procedure TMateraBalanceResponse.Clear;
begin
  faccountId := EmptyStr;
  fautoInvest := 0;
  favailable := 0;
  favailableBalanceForTransactions := 0;
  fblocked := 0;
  fdate := 0;
  femergencyAidBalance := 0;
  ffuture := 0;
  foverdraft := 0;
  freal := 0;
end;

function TMateraBalanceResponse.IsEmpty: Boolean;
begin
  Result :=  EstaVazio(faccountId) and
    EstaZerado(fautoInvest) and
    EstaZerado(favailable) and
    EstaZerado(favailableBalanceForTransactions) and
    EstaZerado(fblocked) and
    EstaZerado(fdate) and
    EstaZerado(femergencyAidBalance) and
    EstaZerado(ffuture) and
    EstaZerado(foverdraft) and
    EstaZerado(freal);
end;

procedure TMateraBalanceResponse.Assign(aSource: TMateraBalanceResponse);
begin
  faccountId := aSource.accountId;
  fautoInvest := aSource.autoInvest;
  favailable := aSource.available;
  favailableBalanceForTransactions := aSource.availableBalanceForTransactions;
  fblocked := aSource.blocked;
  fdate := aSource.date;
  femergencyAidBalance := aSource.emergencyAidBalance;
  ffuture := aSource.future;
  foverdraft := aSource.overdraft;
  freal := aSource.real;

end;

{ TStatementInstantPaymentCashValueDataArray }

function TStatementInstantPaymentCashValueDataArray.GetItem(aIndex: Integer
  ): TMateraStatementInstantPaymentCashValueData;
begin
  Result := TMateraStatementInstantPaymentCashValueData(inherited Items[aIndex]);
end;

procedure TStatementInstantPaymentCashValueDataArray.SetItem(aIndex: Integer;
  aValue: TMateraStatementInstantPaymentCashValueData);
begin
  inherited Items[aIndex] := aValue;
end;

function TStatementInstantPaymentCashValueDataArray.NewSchema: TACBrPIXSchema;
begin
  Result := New;
end;

function TStatementInstantPaymentCashValueDataArray.Add(
  aItem: TMateraStatementInstantPaymentCashValueData): Integer;
begin
  Result := inherited Add(aItem);
end;

procedure TStatementInstantPaymentCashValueDataArray.Insert(aIndex: Integer;
  aItem: TMateraStatementInstantPaymentCashValueData);
begin
  inherited Insert(aIndex, aItem);
end;

function TStatementInstantPaymentCashValueDataArray.New: TMateraStatementInstantPaymentCashValueData;
begin
  Result := TMateraStatementInstantPaymentCashValueData.Create('');
  Self.Add(Result);
end;

{ TMateraStatementInstantPaymentCashValueData }

procedure TMateraStatementInstantPaymentCashValueData.AssignSchema(
  aSource: TACBrPIXSchema);
begin
  if (ASource is TMateraStatementInstantPaymentCashValueData) then
      Assign(TMateraStatementInstantPaymentCashValueData(ASource));
end;

procedure TMateraStatementInstantPaymentCashValueData.DoWriteToJSon(
  aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('cashValueType', MateraStatementIPCashValueTypeToString(fcashValueType))
    .AddPair('value', fvalue);

end;

procedure TMateraStatementInstantPaymentCashValueData.DoReadFromJSon(
  aJSon: TACBrJSONObject);
var
  s: String;
begin
  {$IFDEF FPC}s := EmptyStr;{$ENDIF}

  aJSon
  .Value('cashValueType', s)
  .Value('value', fvalue);

  fcashValueType := StringToMateraStatementIPCashValueType(s);

end;

constructor TMateraStatementInstantPaymentCashValueData.Create(
  const aObjectName: String);
begin
  inherited Create(aObjectName);
  Clear;
end;

procedure TMateraStatementInstantPaymentCashValueData.Clear;
begin
  fcashValueType := msipvtNone;
  fvalue := 0;
end;

function TMateraStatementInstantPaymentCashValueData.IsEmpty: Boolean;
begin
  Result := (fcashValueType = msipvtNone) and
    EstaZerado(fvalue);
end;

procedure TMateraStatementInstantPaymentCashValueData.Assign(
  aSource: TMateraStatementInstantPaymentCashValueData);
begin
  fcashValueType := aSource.cashValueType;
  fvalue := aSource.value;
end;

{ TMateraStatementInstantPaymentCashValue }

procedure TMateraStatementInstantPaymentCashValue.AssignSchema(
  aSource: TACBrPIXSchema);
begin
  if (ASource is TMateraStatementInstantPaymentCashValue) then
      Assign(TMateraStatementInstantPaymentCashValue(ASource));
end;

procedure TMateraStatementInstantPaymentCashValue.DoWriteToJSon(
  aJSon: TACBrJSONObject);
begin
  fvalues.WriteToJSon(aJSon);
end;

procedure TMateraStatementInstantPaymentCashValue.DoReadFromJSon(
  aJSon: TACBrJSONObject);
begin
  fvalues.ReadFromJSon(aJSon);
end;

constructor TMateraStatementInstantPaymentCashValue.Create(
  const aObjectName: String);
begin
  inherited Create(aObjectName);
  fvalues := TStatementInstantPaymentCashValueDataArray.Create('values');
  Clear;
end;

destructor TMateraStatementInstantPaymentCashValue.Destroy;
begin
  fvalues.Free;
  inherited Destroy;
end;

procedure TMateraStatementInstantPaymentCashValue.Clear;
begin
  fvalues.Clear;
end;

function TMateraStatementInstantPaymentCashValue.IsEmpty: Boolean;
begin
  Result := fvalues.IsEmpty;
end;

procedure TMateraStatementInstantPaymentCashValue.Assign(
  aSource: TMateraStatementInstantPaymentCashValue);
begin
  fvalues.Assign(aSource.values);
end;

{ TMateraCounterpart }

procedure TMateraCounterpart.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (ASource is TMateraCounterpart) then
    Assign(TMateraCounterpart(ASource));
end;

procedure TMateraCounterpart.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('accountDestination', faccountDestination)
    .AddPair('bankDestination', fbankDestination)
    .AddPair('branchDestination', fbranchDestination)
    .AddPair('clientType', MateraClientTypeToString(fclientType))
    .AddPair('name', fname);

  fTaxIdentifier.WriteToJSon(aJSon);
end;

procedure TMateraCounterpart.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  s: String;
begin
  {$IFDEF FPC}s := EmptyStr{$ENDIF};
  aJSon
    .Value('accountDestination', faccountDestination)
    .Value('bankDestination', fbankDestination)
    .Value('branchDestination', fbranchDestination)
    .Value('clientType', s)
    .Value('name', fname);

  fclientType := StringToMateraClientType(s);
  fTaxIdentifier.ReadFromJSon(aJSon);
end;

constructor TMateraCounterpart.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  fTaxIdentifier := TMateraTaxIdentifier.Create('TaxIdentifier');
  Clear;
end;

destructor TMateraCounterpart.Destroy;
begin
  fTaxIdentifier.Free;
  inherited Destroy;
end;

procedure TMateraCounterpart.Clear;
begin
  faccountDestination := EmptyStr;
  fbankDestination := EmptyStr;
  fbranchDestination := EmptyStr;
  fclientType := mctNone;
  fname := EmptyStr;
  fTaxIdentifier.Clear;
end;

function TMateraCounterpart.IsEmpty: Boolean;
begin
  Result := (fclientType = mctNone) and
    EstaVazio(fname) and
    EstaVazio(fbankDestination) and
    EstaVazio(fbranchDestination) and
    EstaVazio(faccountDestination) and
    fTaxIdentifier.IsEmpty;
end;

procedure TMateraCounterpart.Assign(aSource: TMateraCounterpart);
begin
  faccountDestination := aSource.accountDestination;
  fbankDestination := aSource.bankDestination;
  fbranchDestination := aSource.branchDestination;
  fclientType := aSource.clientType;
  fname := aSource.name;
  fTaxIdentifier.Assign(aSource.TaxIdentifier);
end;

{ TMaterastatementEntryArray }

function TMaterastatementEntryArray.GetItem(aIndex: Integer): TMaterastatementEntry;
begin
  Result := TMaterastatementEntry(inherited Items[aIndex]);
end;

procedure TMaterastatementEntryArray.SetItem(aIndex: Integer;
  aValue: TMaterastatementEntry);
begin
  inherited Items[aIndex] := aValue;
end;

function TMaterastatementEntryArray.NewSchema: TACBrPIXSchema;
begin
  Result := New;
end;

function TMaterastatementEntryArray.Add(aItem: TMaterastatementEntry): Integer;
begin
  Result := inherited Add(aItem);
end;

procedure TMaterastatementEntryArray.Insert(aIndex: Integer;
  aItem: TMaterastatementEntry);
begin
  inherited Insert(aIndex, aItem);
end;

function TMaterastatementEntryArray.New: TMaterastatementEntry;
begin
  Result := TMaterastatementEntry.Create('');
  Self.Add(Result);
end;

{ TMaterastatementEntry }

procedure TMaterastatementEntry.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (ASource is TMaterastatementEntry) then
      Assign(TMaterastatementEntry(ASource));
end;

procedure TMaterastatementEntry.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('additionalInfo', fadditionalInfo, False)
    .AddPair('amount', famount, False)
    .AddPair('comment', fcomment, False);

  fcounterpart.WriteToJSon(aJSon);

  aJSon
    .AddPairISODate('creditDate', fcreditDate, False)
    .AddPair('description', fdescription, False)
    .AddPairISODate('entryDate', fentryDate, False)
    .AddPair('historyCode', fhistoryCode, False);

  finstantPaymentCashValue.WriteToJSon(aJSon);

  aJSon
    .AddPair('transactionId', ftransactionId, False)
    .AddPair('transactionType', ftransactionType, False)
    .AddPair('type', MaterastatementEntryTypeToString(ftype_));

end;

procedure TMaterastatementEntry.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  s: String;
begin
  {$IFDEF FPC}s := EmptyStr;{$ENDIF}

  aJSon
    .Value('additionalInfo', fadditionalInfo)
    .Value('amount', famount)
    .Value('comment', fcomment);

  fcounterpart.ReadFromJSon(aJSon);

  aJSon
    .AddPairISODate('creditDate', fcreditDate)
    .Value('description', fdescription)
    .AddPairISODate('entryDate', fentryDate)
    .Value('historyCode', fhistoryCode);

  finstantPaymentCashValue.ReadFromJSon(aJSon);

  aJSon
    .Value('transactionId', ftransactionId)
    .Value('transactionType', ftransactionType)
    .Value('type', s);

  ftype_ := StringToMaterastatementEntryType(s);

end;

constructor TMaterastatementEntry.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  fcounterpart := TMateraCounterpart.create('counterpart');
  finstantPaymentCashValue := TMateraStatementInstantPaymentCashValue.Create('instantPaymentCashValue');
  Clear;
end;

destructor TMaterastatementEntry.Destroy;
begin
  fcounterpart.Free;
  finstantPaymentCashValue.Free;
  inherited Destroy;
end;

procedure TMaterastatementEntry.Clear;
begin
  fadditionalInfo := EmptyStr;
  famount := 0;
  fcomment := EmptyStr;
  fcounterpart.Clear;
  fcreditDate := 0;
  fdescription := EmptyStr;
  fentryDate := 0;
  fhistoryCode := 0;
  finstantPaymentCashValue.Clear;
  ftransactionId := EmptyStr;
  ftransactionType := EmptyStr;
  ftype_ := msetNone;
end;

function TMaterastatementEntry.IsEmpty: Boolean;
begin
  Result := EstaVazio(fadditionalInfo) and
    EstaZerado(famount) and
    EstaVazio(fcomment) and
    (fcounterpart.IsEmpty) and
    EstaZerado(fcreditDate) and
    EstaVazio(fdescription) and
    EstaZerado(fentryDate) and
    EstaZerado(fhistoryCode) and
    (finstantPaymentCashValue.IsEmpty) and
    EstaVazio(ftransactionId) and
    EstaVazio(ftransactionType) and
    (ftype_ = msetNone);
end;

procedure TMaterastatementEntry.Assign(aSource: TMaterastatementEntry);
begin
  fadditionalInfo := aSource.additionalInfo;
  famount := aSource.amount;
  fcomment := aSource.comment;
  fcounterpart.Assign(aSource.counterpart);
  fcreditDate := aSource.creditDate;
  fdescription := aSource.description;
  fentryDate := aSource.entryDate;
  fhistoryCode := aSource.historyCode;
  finstantPaymentCashValue.Assign(aSource.instantPaymentCashValue);
  ftransactionId := aSource.transactionId;
  ftransactionType := aSource.transactionType;
  ftype_ := aSource.type_;
end;

{ TMaterastatementResponse }

procedure TMaterastatementResponse.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (ASource is TMaterastatementResponse) then
      Assign(TMaterastatementResponse(ASource));
end;

procedure TMaterastatementResponse.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  fstatement.WriteToJSon(aJSon);
end;

procedure TMaterastatementResponse.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  fstatement.ReadFromJSon(aJSon);
end;

constructor TMaterastatementResponse.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  fstatement := TMaterastatementEntryArray.Create('statement');
  Clear;
end;

destructor TMaterastatementResponse.Destroy;
begin
  fstatement.Free;
  inherited Destroy;
end;

procedure TMaterastatementResponse.Clear;
begin
  fstatement.Clear;
end;

function TMaterastatementResponse.IsEmpty: Boolean;
begin
  Result := fstatement.IsEmpty;
end;

procedure TMaterastatementResponse.Assign(aSource: TMaterastatementResponse);
begin
  fstatement.Assign(aSource.statement);
end;

{ TMateraCancelPaymentResponse }

procedure TMateraCancelPaymentResponse.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (ASource is TMateraCancelPaymentResponse) then
      Assign(TMateraCancelPaymentResponse(ASource));
end;

procedure TMateraCancelPaymentResponse.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon.AddPair('ftransactionalId', False);
end;

procedure TMateraCancelPaymentResponse.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon.Value('transactionalId', ftransactionalId);
end;

constructor TMateraCancelPaymentResponse.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  Clear;
end;

procedure TMateraCancelPaymentResponse.Clear;
begin
  ftransactionalId := EmptyStr;
end;

function TMateraCancelPaymentResponse.IsEmpty: Boolean;
begin
  Result := EstaVazio(ftransactionalId);
end;

procedure TMateraCancelPaymentResponse.Assign(
  aSource: TMateraCancelPaymentResponse);
begin
  ftransactionalId := aSource.transactionalId;
end;

{ TMateraCancelPaymentRequest }

procedure TMateraCancelPaymentRequest.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (ASource is TMateraCancelPaymentRequest) then
      Assign(TMateraCancelPaymentRequest(ASource));
end;

procedure TMateraCancelPaymentRequest.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('reason', freason, False)
    .AddPair('fstatementDeletionMode', MaterastatementDeletionModeToString(fstatementDeletionMode));

end;

procedure TMateraCancelPaymentRequest.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  s: String;
begin
  {$IFDEF FPC}s := EmptyStr;{$ENDIF}
  aJSon
    .Value('reason', freason)
    .Value('fstatementDeletionMode', s);

  fstatementDeletionMode := StringToMaterastatementDeletionMode(s);
end;

constructor TMateraCancelPaymentRequest.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  Clear;
end;

procedure TMateraCancelPaymentRequest.Clear;
begin
  freason := EmptyStr;
  fstatementDeletionMode := msdNone;
end;

function TMateraCancelPaymentRequest.IsEmpty: Boolean;
begin
  Result := EstaVazio(freason) and
    (fstatementDeletionMode = msdNone);
end;

procedure TMateraCancelPaymentRequest.Assign(
  aSource: TMateraCancelPaymentRequest);
begin
  freason := aSource.reason;
  fstatementDeletionMode := aSource.statementDeletionMode;

end;

{ TMateraPaymentResponse }

procedure TMateraPaymentResponse.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (ASource is TMateraPaymentResponse) then
      Assign(TMateraPaymentResponse(ASource));
end;

procedure TMateraPaymentResponse.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('transactionId', ftransactionId, False)
    .AddPair('externalIdentifier', fexternalIdentifier, False)
    .AddPair('senderAccountId', fsenderAccountId, False)
    .AddPair('creditCardToken', fcreditCardToken, False);
  ffinancialStatement.WriteToJSon(aJSon);
  aJSon
    .AddPair('boletoUrl', fboletoUrl, False)
    .AddPair('typeableLine', ftypeableLine, False)
    .AddPairISODateTime('dueDate', fdueDate, False)
    .AddPairISODateTime('transactionDate', ftransactionDate, False)
    .AddPair('transactionType', ftransactionType)
    .AddPair('totalAmount', ftotalAmount, False)
    .AddPair('paidAmount', fpaidAmount, False)
    .AddPair('discountAmount', fdiscountAmount, False)
    .AddPair('recipientDescription', frecipientDescription, False);
  fcouponDetails.WriteToJSon(aJSon);
  aJSon.AddPairISODateTime('expirationDate', fexpirationDate, False);
  finstantPayment.WriteToJSon(aJSon);

end;

procedure TMateraPaymentResponse.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('transactionId', ftransactionId)
    .Value('externalIdentifier', fexternalIdentifier)
    .Value('senderAccountId', fsenderAccountId)
    .Value('creditCardToken', fcreditCardToken);
  ffinancialStatement.ReadFromJSon(aJSon);
  aJSon
    .Value('boletoUrl', fboletoUrl)
    .Value('typeableLine', ftypeableLine)
    .ValueISODateTime('dueDate', fdueDate)
    .ValueISODateTime('transactionDate', ftransactionDate)
    .Value('transactionType', ftransactionType)
    .Value('totalAmount', ftotalAmount)
    .Value('paidAmount', fpaidAmount)
    .Value('discountAmount', fdiscountAmount)
    .Value('recipientDescription', frecipientDescription);
  fcouponDetails.ReadFromJSon(aJSon);
  aJSon.ValueISODateTime('expirationDate', fexpirationDate);
  finstantPayment.ReadFromJSon(aJSon);

end;

constructor TMateraPaymentResponse.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  ffinancialStatement := TMateraFinancialStatement.Create('financialStatement');
  fcouponDetails := TMateraCouponDetails.Create('couponDetails');
  finstantPayment := TMateraInstantPaymentQRCodeResponse.Create('instantPayment');
  Clear;
end;

destructor TMateraPaymentResponse.Destroy;
begin
  ffinancialStatement.Free;
  fcouponDetails.Free;
  finstantPayment.Free;
  inherited Destroy;
end;

procedure TMateraPaymentResponse.Clear;
begin
  fboletoUrl := EmptyStr;
  fcouponDetails.Clear;
  fcreditCardToken := EmptyStr;
  fdiscountAmount := 0;
  fdueDate := 0;
  fexpirationDate := 0;
  fexternalIdentifier := EmptyStr;
  ffinancialStatement.Clear;
  finstantPayment.Clear;
  fpaidAmount := 0;
  frecipientDescription := EmptyStr;
  fsenderAccountId := EmptyStr;
  ftotalAmount := 0;
  ftransactionDate := 0;
  ftransactionId := EmptyStr;
  ftransactionType := nil;
  ftypeableLine := EmptyStr;
end;

function TMateraPaymentResponse.IsEmpty: Boolean;
begin
  Result := EstaVazio(fboletoUrl) and
  (fcouponDetails.IsEmpty) and
  EstaVazio(fcreditCardToken) and
  EstaZerado(fdiscountAmount) and
  EstaZerado(fdueDate) and
  EstaZerado(fexpirationDate) and
  EstaVazio(fexternalIdentifier) and
  (ffinancialStatement.IsEmpty) and
  (finstantPayment.IsEmpty) and
  EstaZerado(fpaidAmount) and
  EstaVazio(frecipientDescription) and
  EstaVazio(fsenderAccountId) and
  EstaZerado(ftotalAmount) and
  EstaZerado(ftransactionDate) and
  EstaVazio(ftransactionId) and
  (ftransactionType = nil) and
  EstaVazio(ftypeableLine);
end;

procedure TMateraPaymentResponse.Assign(aSource: TMateraPaymentResponse);
begin
  fboletoUrl := aSource.boletoUrl;
  fcouponDetails.Assign(aSource.couponDetails);
  fcreditCardToken := aSource.creditCardToken;
  fdiscountAmount := aSource.discountAmount;
  fdueDate := aSource.dueDate;
  fexpirationDate := aSource.expirationDate;
  fexternalIdentifier := aSource.externalIdentifier;
  ffinancialStatement.Assign(aSource.financialStatement);
  finstantPayment.Assign(aSource.instantPayment);
  fpaidAmount := aSource.paidAmount;
  frecipientDescription := aSource.recipientDescription;
  fsenderAccountId := aSource.senderAccountId;
  ftotalAmount := aSource.totalAmount;
  ftransactionDate := aSource.transactionDate;
  ftransactionId := aSource.transactionId;
  ftransactionType := aSource.transactionType;
  ftypeableLine := aSource.typeableLine;
end;

{ TMateraSender }

procedure TMateraSender.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (ASource is TMateraSender) then
      Assign(TMateraSender(ASource));
end;

procedure TMateraSender.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  faccount.WriteToJSon(aJSon);
  fclient.WriteToJSon(aJSon);

end;

procedure TMateraSender.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  faccount.ReadFromJSon(aJSon);
  fclient.ReadFromJSon(aJSon);

end;

constructor TMateraSender.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  faccount := TMateraAccountIdentifier.Create('account');
  fclient := TMateraAccountHolder.Create('client');
  Clear;
end;

destructor TMateraSender.Destroy;
begin
  faccount.Free;
  fclient.Free;
  inherited Destroy;
end;

procedure TMateraSender.Clear;
begin
  faccount.Clear;
  fclient.Clear;
end;

function TMateraSender.IsEmpty: Boolean;
begin
  Result := faccount.IsEmpty and fclient.IsEmpty;
end;

procedure TMateraSender.Assign(aSource: TMateraSender);
begin
  faccount.Assign(aSource.account);
  fclient.Assign(aSource.client);
end;

{ TMateraTransferenciaInternaRequest }

procedure TMateraTransferenciaInternaRequest.AssignSchema(
  aSource: TACBrPIXSchema);
begin
  if (ASource is TMateraTransferenciaInternaRequest) then
      Assign(TMateraTransferenciaInternaRequest(ASource));
end;

procedure TMateraTransferenciaInternaRequest.DoWriteToJSon(
  aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('totalAmount', ftotalAmount, False)
    .AddPair('currency', fcurrency, False);
  fpaymentInfo.WriteToJSon(aJSon);
  fsender.WriteToJSon(aJSon);
  fmyAccount.WriteToJSon(aJSon);
  frecipients.WriteToJSon(aJSon);
  aJSon
    .AddPair('externalIdentifier', fexternalIdentifier, False)
    .AddPair('callbackAddress', fcallbackAddress, False);
end;

procedure TMateraTransferenciaInternaRequest.DoReadFromJSon(
  aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('totalAmount', ftotalAmount)
    .Value('currency', fcurrency);
  fpaymentInfo.WriteToJSon(aJSon);
  fsender.WriteToJSon(aJSon);
  fmyAccount.WriteToJSon(aJSon);
  frecipients.WriteToJSon(aJSon);
  aJSon
    .Value('externalIdentifier', fexternalIdentifier)
    .Value('callbackAddress', fcallbackAddress);

end;

constructor TMateraTransferenciaInternaRequest.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  fpaymentInfo := TMateraPaymentInfo.Create('paymentInfo');
  fsender := TMateraSender.Create('sender');
  fmyAccount := TMateraAccountIdentifier.Create('myAccount');
  frecipients := TMateraRecipientsArray.Create('recipients');
  Clear;
end;

destructor TMateraTransferenciaInternaRequest.Destroy;
begin
  fpaymentInfo.Free;
  fsender.Free;
  fmyAccount.Free;
  frecipients.Free;
  inherited Destroy;
end;

procedure TMateraTransferenciaInternaRequest.Clear;
begin
  ftotalAmount := 0;
  fcurrency := EmptyStr;
  fexternalIdentifier := EmptyStr;
  fcallbackAddress := EmptyStr;
  fpaymentInfo.Clear;
  fsender.Clear;
  fmyAccount.Clear;
  frecipients.Clear;
end;

function TMateraTransferenciaInternaRequest.IsEmpty: Boolean;
begin
  Result := EstaZerado(ftotalAmount) and
    EstaVazio(fcurrency) and
    (fpaymentInfo.IsEmpty) and
    (fsender.IsEmpty) and
    (fmyAccount.IsEmpty) and
    (frecipients.IsEmpty) and
    EstaVazio(fexternalIdentifier) and
    EstaVazio(fcallbackAddress);
end;

procedure TMateraTransferenciaInternaRequest.Assign(
  aSource: TMateraTransferenciaInternaRequest);
begin
  ftotalAmount := aSource.totalAmount;
  fcurrency := aSource.currency;
  fexternalIdentifier := aSource.externalIdentifier;
  fcallbackAddress := aSource.callbackAddress;
  fpaymentInfo.Assign(aSource.paymentInfo);
  fsender.Assign(aSource.sender);
  fmyAccount.Assign(aSource.myAccount);
  frecipients.Assign(aSource.recipients);
end;

{ TMateraRetiradaResponse }

procedure TMateraRetiradaResponse.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (ASource is TMateraRetiradaResponse) then
      Assign(TMateraRetiradaResponse(ASource));
end;

procedure TMateraRetiradaResponse.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('transactionId', ftransactionId, False)
    .AddPair('externalIdentifier', fexternalIdentifier, False)
    .AddPair('status', MateraTransactionStatusToString(fstatus), False)
    .AddPair('receipt', freceipt, False)
    .AddPair('authenticationCode', fauthenticationCode, False);

end;

procedure TMateraRetiradaResponse.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  s: String;
begin
  {$IFDEF FPC}s := EmptyStr;{$ENDIF}
  aJSon
    .Value('transactionId', ftransactionId)
    .Value('externalIdentifier', fexternalIdentifier)
    .Value('status', s)
    .Value('receipt', freceipt)
    .Value('authenticationCode', fauthenticationCode);

  fstatus := StringToMateraTransactionStatus(s);

end;

constructor TMateraRetiradaResponse.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  fstatus := mtsNone;
end;

procedure TMateraRetiradaResponse.Clear;
begin
  ftransactionId := EmptyStr;
  fexternalIdentifier := EmptyStr;
  fstatus := mtsNone;
  freceipt := EmptyStr;
  fauthenticationCode := EmptyStr;
end;

function TMateraRetiradaResponse.IsEmpty: Boolean;
begin
  Result := EstaVazio(ftransactionId) and
    EstaVazio(fexternalIdentifier) and
    (fstatus = mtsNone) and
    EstaVazio(freceipt) and
    EstaVazio(fauthenticationCode);
end;

procedure TMateraRetiradaResponse.Assign(aSource: TMateraRetiradaResponse);
begin
  ftransactionId := aSource.transactionId;
  fexternalIdentifier := aSource.externalIdentifier;
  fstatus := aSource.status;
  freceipt := aSource.receipt;
  fauthenticationCode := aSource.authenticationCode;
end;

{ TMateraCancelPaymentTransactionResponse }

procedure TMateraCancelPaymentTransactionResponse.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraCancelPaymentTransactionResponse) then
    Assign(TMateraCancelPaymentTransactionResponse(aSource));
end;

procedure TMateraCancelPaymentTransactionResponse.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('reason', freason)
    .AddPair('sourceSystem', fsourceSystem)
    .AddPair('externalProtocolId', fexternalProtocolId);
end;

procedure TMateraCancelPaymentTransactionResponse.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('reason', freason)
    .Value('sourceSystem', fsourceSystem)
    .Value('externalProtocolId', fexternalProtocolId);
end;

constructor TMateraCancelPaymentTransactionResponse.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  Clear;
end;

procedure TMateraCancelPaymentTransactionResponse.Clear;
begin
  freason := EmptyStr;
  fsourceSystem := EmptyStr;
  fexternalProtocolId := EmptyStr;
end;

function TMateraCancelPaymentTransactionResponse.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(freason) and
    EstaVazio(fsourceSystem) and
    EstaVazio(fexternalProtocolId);
end;

procedure TMateraCancelPaymentTransactionResponse.Assign(
  aSource: TMateraCancelPaymentTransactionResponse);
begin
  freason := aSource.reason;
  fsourceSystem := aSource.sourceSystem;
  fexternalProtocolId := aSource.externalProtocolId;
end;

{ TMateraOriginDepositTransactionResponse }

procedure TMateraOriginDepositTransactionResponse.AssignSchema(
  aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraOriginDepositTransactionResponse) then
    Assign(TMateraOriginDepositTransactionResponse(aSource));
end;

procedure TMateraOriginDepositTransactionResponse.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('transactionId', ftransactionId)
    .AddPair('transactionDate', ftransactionDate)
    .AddPair('totalAmount', ftotalAmount)
    .AddPair('additionalInformation', fadditionalInformation);
  foriginInstantPaymentTransaction.WriteToJSon(aJSon);
end;

procedure TMateraOriginDepositTransactionResponse.DoReadFromJSon(
  aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('transactionId', ftransactionId)
    .Value('transactionDate', ftransactionDate)
    .Value('totalAmount', ftotalAmount)
    .Value('additionalInformation', fadditionalInformation);
  foriginInstantPaymentTransaction.ReadFromJSon(aJSon);
end;

constructor TMateraOriginDepositTransactionResponse.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  foriginInstantPaymentTransaction := TMateraOriginInstantPaymentTransactionResponse.Create('originInstantPaymentTransaction');
  Clear;
end;

destructor TMateraOriginDepositTransactionResponse.Destroy;
begin
  foriginInstantPaymentTransaction.Free;
  inherited Destroy;
end;

procedure TMateraOriginDepositTransactionResponse.Clear;
begin                                 
  ftotalAmount := 0;
  fadditionalInformation := EmptyStr;
  ftransactionDate := EmptyStr;
  ftransactionId := EmptyStr;
  foriginInstantPaymentTransaction.Clear;
end;

function TMateraOriginDepositTransactionResponse.IsEmpty: Boolean;
begin
  Result := EstaZerado(ftotalAmount) and
    EstaVazio(ftransactionId) and
    EstaVazio(ftransactionDate) and
    EstaVazio(fadditionalInformation) and
    foriginInstantPaymentTransaction.IsEmpty;
end;

procedure TMateraOriginDepositTransactionResponse.Assign(aSource: TMateraOriginDepositTransactionResponse);
begin
  ftotalAmount := aSource.totalAmount;
  fadditionalInformation := aSource.additionalInformation;
  ftransactionDate := aSource.transactionDate;
  ftransactionId := aSource.transactionId;
  foriginInstantPaymentTransaction.Assign(aSource.originInstantPaymentTransaction);
end;

{ TMateraInstantPaymentTransactionReturnInfo }

procedure TMateraInstantPaymentTransactionReturnInfo.AssignSchema(
  aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraInstantPaymentTransactionReturnInfo) then
    Assign(TMateraInstantPaymentTransactionReturnInfo(aSource));
end;

procedure TMateraInstantPaymentTransactionReturnInfo.DoWriteToJSon(
  aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('originalEndToEndId', foriginalEndToEndId, False)
    .AddPair('originalInstantPaymentId', foriginalInstantPaymentId, False);
  freturnReasonInformation.WriteToJSon(aJSon);
end;

procedure TMateraInstantPaymentTransactionReturnInfo.DoReadFromJSon(
  aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('originalEndToEndId', foriginalEndToEndId)
    .Value('originalInstantPaymentId', foriginalInstantPaymentId);
  freturnReasonInformation.ReadFromJSon(aJSon);
end;

constructor TMateraInstantPaymentTransactionReturnInfo.Create(
  const aObjectName: String);
begin
  inherited Create(aObjectName);
  freturnReasonInformation := TMateraInstantPaymentTransactionReturnReasonInformation.Create('returnReasonInformation');
  Clear;
end;

destructor TMateraInstantPaymentTransactionReturnInfo.Destroy;
begin
  freturnReasonInformation.Free;
  inherited Destroy;
end;

procedure TMateraInstantPaymentTransactionReturnInfo.Clear;
begin
  foriginalEndToEndId := EmptyStr;
  foriginalInstantPaymentId := EmptyStr;
  freturnReasonInformation.Clear;
end;

function TMateraInstantPaymentTransactionReturnInfo.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(foriginalEndToEndId) and
    EstaVazio(foriginalInstantPaymentId) and
    freturnReasonInformation.IsEmpty;
end;

procedure TMateraInstantPaymentTransactionReturnInfo.Assign(aSource: TMateraInstantPaymentTransactionReturnInfo);
begin
  foriginalEndToEndId := aSource.originalEndToEndId;
  foriginalInstantPaymentId := aSource.originalInstantPaymentId;
  freturnReasonInformation.Assign(aSource.returnReasonInformation);
end;

{ TMateraInstantPaymentTransactionReturnReasonInformation }

procedure TMateraInstantPaymentTransactionReturnReasonInformation.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraInstantPaymentTransactionReturnReasonInformation) then
    Assign(TMateraInstantPaymentTransactionReturnReasonInformation(aSource));
end;

procedure TMateraInstantPaymentTransactionReturnReasonInformation.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('reasonCode', freasonCode, False)
    .AddPair('reasonDescription', freasonDescription, False)
    .AddPair('additionalInformation', fadditionalInformation, False);
end;

procedure TMateraInstantPaymentTransactionReturnReasonInformation.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('reasonCode', freasonCode)
    .Value('reasonDescription', freasonDescription)
    .Value('additionalInformation', fadditionalInformation);
end;

constructor TMateraInstantPaymentTransactionReturnReasonInformation.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  Clear;
end;

procedure TMateraInstantPaymentTransactionReturnReasonInformation.Clear;
begin
  fadditionalInformation := EmptyStr;
  freasonCode := EmptyStr;
  freasonDescription := EmptyStr;
end;

function TMateraInstantPaymentTransactionReturnReasonInformation.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(freasonCode) and
    EstaVazio(freasonDescription) and
    EstaVazio(fadditionalInformation);
end;

procedure TMateraInstantPaymentTransactionReturnReasonInformation.Assign(
  aSource: TMateraInstantPaymentTransactionReturnReasonInformation);
begin
  fadditionalInformation := aSource.additionalInformation;
  freasonCode := aSource.reasonCode;
  freasonDescription := aSource.reasonDescription;
end;

{ TMateraRejectionReasonInstantPayment }

procedure TMateraRejectionReasonInstantPayment.AssignSchema(
  aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraRejectionReasonInstantPayment) then
    Assign(TMateraRejectionReasonInstantPayment(aSource));
end;

procedure TMateraRejectionReasonInstantPayment.DoWriteToJSon(
  aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('code', fcode, False)
    .AddPair('description', fdescription, False);
end;

procedure TMateraRejectionReasonInstantPayment.DoReadFromJSon(
  aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('code', fcode)
    .Value('description', fdescription);
end;

constructor TMateraRejectionReasonInstantPayment.Create(
  const aObjectName: String);
begin
  inherited Create(aObjectName);
  Clear;
end;

procedure TMateraRejectionReasonInstantPayment.Clear;
begin
  fcode := EmptyStr;
  fdescription := EmptyStr;
end;

function TMateraRejectionReasonInstantPayment.IsEmpty: Boolean;
begin
  Result := EstaVazio(fcode) and EstaVazio(fdescription);
end;

procedure TMateraRejectionReasonInstantPayment.Assign(aSource: TMateraRejectionReasonInstantPayment);
begin
  fcode := aSource.code;
  fdescription := aSource.description;
end;

{ TMateraPaymentReceivedArray }

function TMateraPaymentReceivedArray.GetItem(aIndex: Integer): TMateraPaymentReceived;
begin
  Result := TMateraPaymentReceived(inherited Items[aIndex]);
end;

procedure TMateraPaymentReceivedArray.SetItem(aIndex: Integer; aValue: TMateraPaymentReceived);
begin
  inherited Items[aIndex] := aValue;
end;

function TMateraPaymentReceivedArray.NewSchema: TACBrPIXSchema;
begin
  Result := New;
end;

function TMateraPaymentReceivedArray.Add(aItem: TMateraPaymentReceived): Integer;
begin
  Result := inherited Add(aItem);
end;

procedure TMateraPaymentReceivedArray.Insert(aIndex: Integer; aItem: TMateraPaymentReceived);
begin
  inherited Insert(aIndex, aItem);
end;

function TMateraPaymentReceivedArray.New: TMateraPaymentReceived;
begin
  Result := TMateraPaymentReceived.Create;
  Self.Add(Result);
end;

{ TMateraPaymentReceivedBasic }

procedure TMateraPaymentReceivedBasic.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraPaymentReceivedBasic) then
    Assign(TMateraPaymentReceivedBasic(aSource));
end;

procedure TMateraPaymentReceivedBasic.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  fsender.WriteToJSon(aJSon);
  aJSon
    .AddPair('receivedAmount', freceivedAmount, False)
    .AddPair('transactionTimestamp', ftransactionTimestamp, False)
    .AddPair('legacyTransactionId', flegacyTransactionId, False)
    .AddPair('endToEndId', fendToEndId, False)
    .AddPair('additionalInformation', fadditionalInformation, False);
end;

procedure TMateraPaymentReceivedBasic.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  fsender.ReadFromJSon(aJSon);
  aJSon
    .Value('receivedAmount', freceivedAmount)
    .Value('transactionTimestamp', ftransactionTimestamp)
    .Value('legacyTransactionId', flegacyTransactionId)
    .Value('endToEndId', fendToEndId)
    .Value('additionalInformation', fadditionalInformation);
end;

constructor TMateraPaymentReceivedBasic.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  fsender := TMateraParticipantInstantPayment.Create('sender');
  Clear;
end;

destructor TMateraPaymentReceivedBasic.Destroy;
begin
  fsender.Free;
  inherited Destroy;
end;

procedure TMateraPaymentReceivedBasic.Clear;
begin
  fadditionalInformation := EmptyStr;
  fendToEndId := EmptyStr;
  flegacyTransactionId := EmptyStr;
  freceivedAmount := 0;
  ftransactionTimestamp := EmptyStr;
  fsender.Clear;
end;

function TMateraPaymentReceivedBasic.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(fadditionalInformation) and
    EstaVazio(fendToEndId) and
    EstaVazio(flegacyTransactionId) and
    EstaVazio(ftransactionTimestamp) and
    EstaZerado(freceivedAmount) and
    fsender.IsEmpty;
end;

procedure TMateraPaymentReceivedBasic.Assign(aSource: TMateraPaymentReceivedBasic);
begin
  fadditionalInformation := aSource.additionalInformation;
  fendToEndId := aSource.endToEndId;
  flegacyTransactionId := aSource.legacyTransactionId;
  freceivedAmount := aSource.receivedAmount;
  ftransactionTimestamp := aSource.transactionTimestamp;
  fsender.Assign(aSource.sender);
end;

{ TMateraWithdrawAgent }

procedure TMateraWithdrawAgent.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (ASource is TMateraWithdrawAgent) then
    Assign(TMateraWithdrawAgent(ASource));
end;

procedure TMateraWithdrawAgent.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('modality', MateraWithdrawAgentTypeToString(fmodality), False)
    .AddPair('serviceProvider', fserviceProvider, False);
end;

procedure TMateraWithdrawAgent.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  s: String;
begin
  {$IFDEF FPC}s := EmptyStr;{$ENDIF}
  aJSon
    .Value('modality', s)
    .Value('serviceProvider', fserviceProvider);

  fmodality := StringToMateraWithdrawAgentType(s);
end;

constructor TMateraWithdrawAgent.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  Clear;
end;

procedure TMateraWithdrawAgent.Clear;
begin
  fmodality := mwatNone;
  fserviceProvider := EmptyStr;
end;

function TMateraWithdrawAgent.IsEmpty: Boolean;
begin
  Result := (fmodality = mwatNone) and
    EstaVazio(fserviceProvider);
end;

procedure TMateraWithdrawAgent.Assign(aSource: TMateraWithdrawAgent);
begin
  fmodality := aSource.modality;
  fserviceProvider := aSource.serviceProvider;
end;

{ TMateraTransactionValuesDetails }

procedure TMateraTransactionValuesDetails.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (ASource is TMateraTransactionValuesDetails) then
    Assign(TMateraTransactionValuesDetails(ASource));
end;

procedure TMateraTransactionValuesDetails.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('transactionValue', ftransactionValue, False)
    .AddPair('cashValue', fcashValue, False);
end;

procedure TMateraTransactionValuesDetails.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('transactionValue', ftransactionValue)
    .Value('cashValue', fcashValue);
end;

constructor TMateraTransactionValuesDetails.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  Clear;
end;

procedure TMateraTransactionValuesDetails.Clear;
begin
  ftransactionValue := 0;
  fcashValue := 0;
end;

function TMateraTransactionValuesDetails.IsEmpty: Boolean;
begin
  Result := EstaZerado(ftransactionValue) and EstaZerado(fcashValue);
end;

procedure TMateraTransactionValuesDetails.Assign(aSource: TMateraTransactionValuesDetails);
begin
  ftransactionValue := aSource.transactionValue;
  fcashValue := aSource.cashValue;
end;

{ TMateraInstantPaymentRecipient }

procedure TMateraInstantPaymentRecipient.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (ASource is TMateraInstantPaymentRecipient) then
      Assign(TMateraInstantPaymentRecipient(ASource));
end;

procedure TMateraInstantPaymentRecipient.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon.AddPair('pspId', fpspid);
  fTaxIdentifierRequest.WriteToJSon(aJSon);
  aJSon
    .AddPair('alias', falias_, False)
    .AddPair('endToEndIdQuery', fendToEndIdQuery, False);
  faccountDestination.WriteToJSon(aJSon);
end;

procedure TMateraInstantPaymentRecipient.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon.Value('pspID', fpspid);
  fTaxIdentifierRequest.ReadFromJSon(aJSon);
  aJSon
    .Value('alias', falias_)
    .Value('endToEndIdQuery', fendToEndIdQuery);
  faccountDestination.ReadFromJSon(aJSon);
end;

constructor TMateraInstantPaymentRecipient.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  fTaxIdentifierRequest := TMateraTaxIdentifierRequest.Create('taxIdentifier');
  faccountDestination := TMateraDestinationAccount.Create('accountDestination');
end;

destructor TMateraInstantPaymentRecipient.Destroy;
begin
  fTaxIdentifierRequest.Free;
  faccountDestination.Free;
  inherited Destroy;
end;

procedure TMateraInstantPaymentRecipient.Clear;
begin
  fpspid := EmptyStr;
  TaxIdentifierRequest.Clear;
  falias_ := EmptyStr;
  fendToEndIdQuery := EmptyStr;
  accountDestination.Clear;
end;

function TMateraInstantPaymentRecipient.IsEmpty: Boolean;
begin
  Result := EstaVazio(fpspid) and
    TaxIdentifierRequest.IsEmpty and
    EstaVazio(falias_) and
    EstaVazio(fendToEndIdQuery) and
    accountDestination.IsEmpty;
end;

procedure TMateraInstantPaymentRecipient.Assign(
  aSource: TMateraInstantPaymentRecipient);
begin
  fpspid := aSource.pspid;
  TaxIdentifierRequest.Assign(aSource.TaxIdentifierRequest);
  falias_ := aSource.alias_;
  fendToEndIdQuery := aSource.endToEndIdQuery;
  accountDestination.Assign(aSource.accountDestination);
end;

{ TMateraInstantPaymentRequest }

procedure TMateraInstantPaymentRequest.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (ASource is TMateraInstantPaymentRequest) then
    Assign(TMateraInstantPaymentRequest(ASource));
end;

procedure TMateraInstantPaymentRequest.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  frecipient.WriteToJSon(aJSon);

  aJSon
    .AddPair('receiverReconciliationIdentifier', freceiverReconciliationIdentifier, False)
    .AddPair('initiatingInstitution', finitiatingInstitution, False)
    .AddPair('initiationForm', MaterainitiationFormToString(finitiationForm), False)
    .AddPair('additionalInformation', fadditionalInformation, False)
    .AddPair('qrcodeType', MateraqrcodeTypeToString(fqrcodeType), False)
    .AddPair('historyCode', fhistoryCode, False)
    .AddPair('initiationProcedure', MaterainitiationProcedureToString(finitiationProcedure), False)
    .AddPair('instructionPriority', MaterainstructionPriorityToString(finstructionPriority), False)
    .AddPair('transactionPurpose', MateratransactionPurposeToString(ftransactionPurpose), False);

  ftransactionValuesDetails.WriteToJSon(aJSon);

  aJSon.AddPair('instructionType', MaterainstructionTypeToString(finstructionType), False);

  fwithdrawAgent.WriteToJSon(aJSon);

  if not fperformDebit then
    aJSon.AddPair('performDebit', fperformDebit);

end;

procedure TMateraInstantPaymentRequest.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  s1,s2,s3,s4,s5,s6: String;
begin
  {$IFDEF FPC}
  s1 := EmptyStr;
  s2 := EmptyStr;
  s3 := EmptyStr;
  s4 := EmptyStr;
  s5 := EmptyStr;
  s6 := EmptyStr;
  {$ENDIF}

  frecipient.ReadFromJSon(aJSon);

  aJSon
    .Value('receiverReconciliationIdentifier', freceiverReconciliationIdentifier)
    .Value('initiatingInstitution', finitiatingInstitution)
    .Value('initiationForm', s1)
    .Value('additionalInformation', fadditionalInformation)
    .Value('qrcodeType', s2)
    .Value('historyCode', fhistoryCode)
    .Value('initiationProcedure', s3)
    .Value('instructionPriority', s4)
    .Value('transactionPurpose', s5);

  finitiationForm := StringToMaterainitiationForm(s1);
  fqrcodeType := StringToMateraqrcodeType(s2);
  finitiationProcedure := StringToMaterainitiationProcedure(s3);
  finstructionPriority := StringToMaterainstructionPriority(s4);
  ftransactionPurpose := StringToMateratransactionPurpose(s5);

  ftransactionValuesDetails.ReadFromJSon(aJSon);

  aJSon.Value('instructionType', s6);

  finstructionType := StringToMaterainstructionType(s6);

  fwithdrawAgent.ReadFromJSon(aJSon);

  aJSon.Value('performDebit', fperformDebit);

end;

constructor TMateraInstantPaymentRequest.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  frecipient := TMateraInstantPaymentRecipient.Create('recipient');
  ftransactionValuesDetails := TMateraTransactionValuesDetails.Create('transactionValuesDetails');
  fwithdrawAgent := TMateraWithdrawAgent.Create('withdrawAgent');
  Clear;
end;

destructor TMateraInstantPaymentRequest.Destroy;
begin
  frecipient.Free;
  ftransactionValuesDetails.Free;
  fwithdrawAgent.Free;
  inherited Destroy;
end;

procedure TMateraInstantPaymentRequest.Clear;
begin
  frecipient.Clear;
  freceiverReconciliationIdentifier := EmptyStr;
  finitiatingInstitution := EmptyStr;
  finitiationForm := mifNone;
  fadditionalInformation := EmptyStr;
  fqrcodeType := mqrtNone;
  fhistoryCode := EmptyStr;
  finitiationProcedure := mipNone;
  finstructionPriority := maipNone;
  ftransactionPurpose := mtpNone;
  ftransactionValuesDetails.clear;
  finstructionType := mitNone;
  fwithdrawAgent.Clear;
  fperformDebit := True;
end;

function TMateraInstantPaymentRequest.IsEmpty: Boolean;
begin
  Result := frecipient.IsEmpty and
    EstaVazio(freceiverReconciliationIdentifier) and
    EstaVazio(finitiatingInstitution) and
    (finitiationForm = mifNone) and
    EstaVazio(fadditionalInformation) and
    (fqrcodeType = mqrtNone) and
    EstaVazio(fhistoryCode) and
    (finitiationProcedure = mipNone) and
    (finstructionPriority = maipNone) and
    (ftransactionPurpose = mtpNone) and
    ftransactionValuesDetails.IsEmpty and
    (finstructionType = mitNone) and
    fwithdrawAgent.IsEmpty;
end;

procedure TMateraInstantPaymentRequest.Assign(
  aSource: TMateraInstantPaymentRequest);
begin
  frecipient := aSource.recipient;
  freceiverReconciliationIdentifier := aSource.receiverReconciliationIdentifier;
  finitiatingInstitution := aSource.initiatingInstitution;
  finitiationForm := aSource.initiationForm;
  fadditionalInformation := aSource.additionalInformation;
  fqrcodeType := aSource.qrcodeType;
  fhistoryCode := aSource.historyCode;
  finitiationProcedure := aSource.initiationProcedure;
  finstructionPriority := aSource.instructionPriority;
  ftransactionPurpose := aSource.transactionPurpose;
  ftransactionValuesDetails := aSource.transactionValuesDetails;
  finstructionType := aSource.instructionType;
  fwithdrawAgent := aSource.withdrawAgent;
  fperformDebit := aSource.performDebit;
end;

{ TMateraExternal }

procedure TMateraExternal.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (ASource is TMateraExternal) then
      Assign(TMateraExternal(ASource));
end;

procedure TMateraExternal.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon.AddPair('historyCode', fhistoryCode);
end;

procedure TMateraExternal.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon.Value('historyCode', fhistoryCode);
end;

constructor TMateraExternal.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
end;

procedure TMateraExternal.Clear;
begin
  fhistoryCode := EmptyStr;
end;

function TMateraExternal.IsEmpty: Boolean;
begin
  Result := EstaVazio(fhistoryCode);
end;

procedure TMateraExternal.Assign(aSource: TMateraExternal);
begin
  fhistoryCode := aSource.historyCode;
end;

{ TMateraUtilitiesBasic }

procedure TMateraUtilitiesBasic.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (ASource is TMateraUtilitiesBasic) then
    Assign(TMateraUtilitiesBasic(ASource));
end;

procedure TMateraUtilitiesBasic.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('documentNumber', fdocumentNumber)
    .AddPair('barcode', fbarcode)
    .AddPair('beneficiaryTaxIdentifier', fbeneficiaryTaxIdentifier)
    .AddPair('typeableLine', ftypeableLine)
    .AddPairISODate('dueDate', fdueDate)
    .AddPair('paidAmount', fpaidAmount)
    .AddPair('historyCode', fhistoryCode);
end;

procedure TMateraUtilitiesBasic.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('documentNumber', fdocumentNumber)
    .Value('barcode', fbarcode)
    .Value('beneficiaryTaxIdentifier', fbeneficiaryTaxIdentifier)
    .Value('typeableLine', ftypeableLine)
    .ValueISODate('dueDate', fdueDate)
    .Value('paidAmount', fpaidAmount)
    .Value('historyCode', fhistoryCode);
end;

constructor TMateraUtilitiesBasic.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  Clear;
end;

procedure TMateraUtilitiesBasic.Clear;
begin
  fdueDate := 0;
  fpaidAmount := 0;
  fbarcode := EmptyStr;
  fhistoryCode := EmptyStr;
  ftypeableLine := EmptyStr;
  fdocumentNumber := EmptyStr;
  fbeneficiaryTaxIdentifier := EmptyStr;
end;

function TMateraUtilitiesBasic.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(fbarcode) and
    EstaZerado(fdueDate) and
    EstaZerado(fpaidAmount) and
    EstaVazio(fhistoryCode) and
    EstaVazio(ftypeableLine) and
    EstaVazio(fdocumentNumber) and
    EstaVazio(fbeneficiaryTaxIdentifier);
end;

procedure TMateraUtilitiesBasic.Assign(aSource: TMateraUtilitiesBasic);
begin
  fdocumentNumber := aSource.documentNumber;
  fbarcode := aSource.barcode;
  fbeneficiaryTaxIdentifier := aSource.beneficiaryTaxIdentifier;
  ftypeableLine := aSource.typeableLine;
  fdueDate := aSource.dueDate;
  fpaidAmount := aSource.paidAmount;
  fhistoryCode := aSource.historyCode;
end;

{ TMateraBoletoBasic }

procedure TMateraBoletoBasic.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (ASource is TMateraBoletoBasic) then
    Assign(TMateraBoletoBasic(ASource));
end;

procedure TMateraBoletoBasic.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('barcode', fbarcode)
    .AddPair('interestAmount', finterestAmount)
    .AddPair('paidAmount', fpaidAmount)
    .AddPair('fineAmount', ffineAmount)
    .AddPair('documentNumber', fdocumentNumber)
    .AddPair('historyCode', fhistoryCode)
    .AddPair('typeableLine', ftypeableLine)
    .AddPair('beneficiaryTaxIdentifier', fbeneficiaryTaxIdentifier)
    .AddPairISODate('dueDate', fdueDate)
    .AddPair('discount', fdiscount)
    .AddPair('status', MateraTransactionStatusToString(fstatus))
    .AddPair('bankAuthentication', fbankAuthentication)
    .AddPair('authenticationCode', fauthenticationCode);
end;

procedure TMateraBoletoBasic.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  s: String;
begin
  {$IFDEF FPC}s := EmptyStr;{$ENDIF}
  aJSon
    .Value('barcode', fbarcode)
    .Value('interestAmount', finterestAmount)
    .Value('paidAmount', fpaidAmount)
    .Value('fineAmount', ffineAmount)
    .Value('documentNumber', fdocumentNumber)
    .Value('historyCode', fhistoryCode)
    .Value('typeableLine', ftypeableLine)
    .Value('beneficiaryTaxIdentifier', fbeneficiaryTaxIdentifier)
    .ValueISODate('dueDate', fdueDate)
    .Value('discount', fdiscount)
    .Value('status', s)
    .Value('bankAuthentication', fbankAuthentication)
    .Value('authenticationCode', fauthenticationCode);
  fstatus := StringToMateraTransactionStatus(s);
end;

constructor TMateraBoletoBasic.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  Clear;
end;

procedure TMateraBoletoBasic.Clear;
begin
  fbarcode := EmptyStr;
  fbeneficiaryTaxIdentifier := EmptyStr;
  fdiscount := 0;
  fdocumentNumber := EmptyStr;
  fdueDate := 0;
  ffineAmount := 0;
  fhistoryCode := EmptyStr;
  finterestAmount := 0;
  ftypeableLine := EmptyStr;
  fpaidAmount := 0;
  fstatus := mtsNone;
  fbankAuthentication := EmptyStr;
  fauthenticationCode := EmptyStr;
end;

function TMateraBoletoBasic.IsEmpty: Boolean;
begin
  Result := (fstatus = mtsNone) and
    EstaVazio(fbarcode) and
    EstaVazio(fbeneficiaryTaxIdentifier) and
    EstaZerado(fdiscount) and
    EstaVazio(fdocumentNumber) and
    EstaZerado(fdueDate) and
    EstaZerado(ffineAmount) and
    EstaVazio(fhistoryCode) and
    EstaZerado(finterestAmount) and
    EstaVazio(ftypeableLine) and
    EstaZerado(fpaidAmount) and
    EstaVazio(fbankAuthentication) and
    EstaVazio(fauthenticationCode);
end;

procedure TMateraBoletoBasic.Assign(aSource: TMateraBoletoBasic);
begin
  fbarcode := aSource.barcode;
  fbeneficiaryTaxIdentifier := aSource.beneficiaryTaxIdentifier;
  fdiscount := aSource.discount;
  fdocumentNumber := aSource.documentNumber;
  fdueDate := aSource.dueDate;
  ffineAmount := aSource.fineAmount;
  fhistoryCode := aSource.historyCode;
  finterestAmount := aSource.interestAmount;
  ftypeableLine := aSource.typeableLine;
  fpaidAmount := aSource.paidAmount;
  fstatus := aSource.status;
  fbankAuthentication := aSource.bankAuthentication;
  fauthenticationCode := authenticationCode;
end;

{ TMateraInstantPaymentTransactionResponse }

procedure TMateraInstantPaymentTransactionResponse.AssignSchema(
  aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraInstantPaymentTransactionResponse) then
    Assign(TMateraInstantPaymentTransactionResponse(aSource));
end;

procedure TMateraInstantPaymentTransactionResponse.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('endToEndId', fendToEndId, False)
    .AddPair('additionalInformation', fadditionalInformation, False);

  fsender.WriteToJSon(aJSon);
  frecipient.WriteToJSon(aJSon);
  freturnInfo.WriteToJSon(aJSon);
  fpaymentReceived.WriteToJSon(aJSon);
  frejectionReason.WriteToJSon(aJSon);
  finstantPaymentCashValue.WriteToJSon(aJSon);
  foriginDepositTransaction.WriteToJSon(aJSon);
end;

procedure TMateraInstantPaymentTransactionResponse.DoReadFromJSon(
  aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('endToEndId', fendToEndId)
    .Value('additionalInformation', fadditionalInformation);

  fsender.ReadFromJSon(aJSon);
  frecipient.ReadFromJSon(aJSon);
  freturnInfo.ReadFromJSon(aJSon);
  fpaymentReceived.ReadFromJSon(aJSon);
  frejectionReason.ReadFromJSon(aJSon);
  finstantPaymentCashValue.ReadFromJSon(aJSon);
  foriginDepositTransaction.ReadFromJSon(aJSon);
end;

constructor TMateraInstantPaymentTransactionResponse.Create(
  const aObjectName: String);
begin
  inherited Create(aObjectName);
  fsender := TMateraParticipantInstantPayment.Create('sender');
  finstantPaymentCashValue := TMateraCashValue.Create('instantPaymentCashValue');
  fpaymentReceived := TMateraPaymentReceivedArray.Create('paymentReceived');
  frecipient := TMateraParticipantInstantPayment.Create('recipient');
  frejectionReason := TMateraRejectionReasonInstantPayment.Create('rejectionReason');
  freturnInfo := TMateraInstantPaymentTransactionReturnInfo.Create('returnInfo');
  foriginDepositTransaction := TMateraOriginDepositTransactionResponse.Create('originDepositTransaction');
  Clear;
end;

destructor TMateraInstantPaymentTransactionResponse.Destroy;
begin
  fsender.Free;
  finstantPaymentCashValue.Free;
  fpaymentReceived.Free;
  frecipient.Free;
  frejectionReason.Free;
  freturnInfo.Free;
  foriginDepositTransaction.Free;
  inherited Destroy;
end;

procedure TMateraInstantPaymentTransactionResponse.Clear;
begin
  fendToEndId := EmptyStr;
  fadditionalInformation := EmptyStr;
  fsender.Clear;
  finstantPaymentCashValue.Clear;
  fpaymentReceived.Clear;
  frecipient.Clear;
  frejectionReason.Clear;
  freturnInfo.Clear;
  foriginDepositTransaction.Clear;
end;

function TMateraInstantPaymentTransactionResponse.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(fendToEndId) and
    EstaVazio(fadditionalInformation) and
    fsender.IsEmpty and
    finstantPaymentCashValue.IsEmpty and
    fpaymentReceived.IsEmpty and
    frecipient.IsEmpty and
    frejectionReason.IsEmpty and
    freturnInfo.IsEmpty and
    foriginDepositTransaction.IsEmpty;
end;

procedure TMateraInstantPaymentTransactionResponse.Assign(aSource: TMateraInstantPaymentTransactionResponse);
begin
  fendToEndId := aSource.endToEndId;
  fadditionalInformation := aSource.additionalInformation;
  fsender.Assign(aSource.sender);
  finstantPaymentCashValue.Assign(aSource.instantPaymentCashValue);
  fpaymentReceived.Assign(aSource.paymentReceived);
  frecipient.Assign(aSource.recipient);
  frejectionReason.Assign(aSource.rejectionReason);
  freturnInfo.Assign(aSource.returnInfo);
  foriginDepositTransaction.Assign(aSource.originDepositTransaction);
end;

{ TMateraCashValue }

procedure TMateraCashValue.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraCashValue) then
    Assign(TMateraCashValue(aSource));
end;

procedure TMateraCashValue.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('cashValueType', MateraCashValueTypeToString(fcashValueType))
    .AddPair('value', fvalue);

  if fallowValueChange then
    aJSon.AddPair('allowValueChange', fallowValueChange);

  fwithdrawProviders.WriteToJSon(aJSon);
end;

procedure TMateraCashValue.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  s: String;
begin
  aJSon
    .AddPair('cashValueType', s)
    .AddPair('value', fvalue)
    .AddPair('allowValueChange', fallowValueChange);

  fcashValueType := StringToMateraCashValueType(s);
  fwithdrawProviders.ReadFromJSon(aJSon);
end;

constructor TMateraCashValue.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  fwithdrawProviders := TMateraWithdrawProviders.Create('withdrawProviders');
end;

destructor TMateraCashValue.Destroy;
begin
  fwithdrawProviders.Free;
  inherited Destroy;
end;

procedure TMateraCashValue.Clear;
begin
  fvalue := EmptyStr;
  fallowValueChange := False;
  fcashValueType := mcvNone;
  fwithdrawProviders.Clear;
end;

function TMateraCashValue.IsEmpty: Boolean;
begin
  Result := EstaVazio(fvalue) and fwithdrawProviders.IsEmpty and
    (fcashValueType = mcvNone) and (not fallowValueChange);
end;

procedure TMateraCashValue.Assign(aSource: TMateraCashValue);
begin
  fallowValueChange := aSource.allowValueChange;
  fcashValueType := aSource.cashValueType;
  fvalue := aSource.value;
  fwithdrawProviders.Assign(aSource.withdrawProviders);
end;

{ TMateraWithdrawProviders }

procedure TMateraWithdrawProviders.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraWithdrawProviders) then
    Assign(TMateraWithdrawProviders(aSource));
end;

procedure TMateraWithdrawProviders.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('agentModality', MateraWithdrawAgentTypeToString(fagentModality))
    .AddPair('serviceProvider', fserviceProvider);
end;

procedure TMateraWithdrawProviders.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  s: String;
begin
  {$IFDEF FPC}s := EmptyStr;{$ENDIF}
  aJSon
    .Value('agentModality', s)
    .Value('serviceProvider', fserviceProvider);
  fagentModality := StringToMateraWithdrawAgentType(s);
end;

constructor TMateraWithdrawProviders.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  Clear;
end;

procedure TMateraWithdrawProviders.Clear;
begin
  fagentModality := mwatNone;
  fserviceProvider := EmptyStr;
end;

function TMateraWithdrawProviders.IsEmpty: Boolean;
begin
  Result := EstaVazio(fserviceProvider) and (fagentModality = mwatNone);
end;

procedure TMateraWithdrawProviders.Assign(aSource: TMateraWithdrawProviders);
begin
  fagentModality := aSource.agentModality;
  fserviceProvider := aSource.serviceProvider;
end;

{ TMateraParticipantInstantPayment }

procedure TMateraParticipantInstantPayment.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraParticipantInstantPayment) then
    Assign(TMateraParticipantInstantPayment(aSource));
end;

procedure TMateraParticipantInstantPayment.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('alias', falias, False)
    .AddPair('name', fname, False);
  ftaxIdentifier.WriteToJSon(aJSon);
  faccount.WriteToJSon(aJSon);
  fpsp.WriteToJSon(aJSon);
end;

procedure TMateraParticipantInstantPayment.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('alias', falias)
    .Value('name', fname);
  ftaxIdentifier.ReadFromJSon(aJSon);
  faccount.ReadFromJSon(aJSon);
  fpsp.ReadFromJSon(aJSon);
end;

constructor TMateraParticipantInstantPayment.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  ftaxIdentifier := TMateraTaxIdentifierBasic.Create('taxIdentifier');
  faccount := TMateraDestinationAccount.Create('account');
  fpsp := TMateraPSP.Create('psp');
  Clear;
end;

destructor TMateraParticipantInstantPayment.Destroy;
begin
  ftaxIdentifier.Free;
  faccount.Free;
  fpsp.Free;
  inherited Destroy;
end;

procedure TMateraParticipantInstantPayment.Clear;
begin
  falias := EmptyStr;
  fname := EmptyStr;
  ftaxIdentifier.Clear;
  faccount.Clear;
  fpsp.Clear;
end;

function TMateraParticipantInstantPayment.IsEmpty: Boolean;
begin
  Result := EstaVazio(falias) and EstaVazio(fname) and
    ftaxIdentifier.IsEmpty and faccount.IsEmpty and fpsp.IsEmpty;
end;

procedure TMateraParticipantInstantPayment.Assign(aSource: TMateraParticipantInstantPayment);
begin
  falias := aSource.alias_;
  fname := aSource.name;
  ftaxIdentifier.Assign(aSource.taxIdentifier);
  faccount.Assign(aSource.account);
  fpsp.Assign(aSource.psp);
end;

{ TMateraBankTransfer }

procedure TMateraBankTransfer.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (ASource is TMateraBankTransfer) then
      Assign(TMateraBankTransfer(ASource));
end;

procedure TMateraBankTransfer.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('bankDestination', fbankDestination)
    .AddPair('branchDestination', fbranchDestination)
    .AddPair('accountDestination', faccountDestination);
  ftaxIdentifier.WriteToJSon(aJSon);
  aJSon
    .AddPair('personType', fpersonType)
    .AddPair('name', fname)
    .AddPair('accountTypeDestination', MateraAccountTypeDestinationToString(faccountTypeDestination))
    .AddPair('historyCode', fhistoryCode)
    .AddPair('purposeCode', fpurposeCode)
    .AddPair('transferMethod', ftransferMethod)
    .AddPair('accountDigitDestination', faccountDigitDestination);

end;

procedure TMateraBankTransfer.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  s: String;
begin
  {$IfDef FPC}s := EmptyStr;{$EndIf}
  aJSon
    .Value('bankDestination', fbankDestination)
    .Value('branchDestination', fbranchDestination)
    .Value('accountDestination', faccountDestination);
  ftaxIdentifier.ReadFromJSon(aJSon);
  aJSon
    .Value('personType', fpersonType)
    .Value('name', fname)
    .Value('accountTypeDestination', s)
    .Value('historyCode', fhistoryCode)
    .Value('purposeCode', fpurposeCode)
    .Value('transferMethod', ftransferMethod)
    .Value('accountDigitDestination', faccountDigitDestination);

  faccountTypeDestination := StringToMateraAccountTypeDestination(s);
end;

constructor TMateraBankTransfer.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  ftaxIdentifier := TMateraTaxIdentifier.Create('taxIdentifier');
end;

destructor TMateraBankTransfer.Destroy;
begin
  ftaxIdentifier.Free;
  inherited Destroy;
end;

procedure TMateraBankTransfer.Clear;
begin
  faccountDestination := EmptyStr;
  faccountDigitDestination := EmptyStr;
  faccountTypeDestination := matdNone;
  fbankDestination := EmptyStr;
  fbranchDestination := EmptyStr;
  fhistoryCode := EmptyStr;
  fname := EmptyStr;
  fpersonType := EmptyStr;
  fpurposeCode := EmptyStr;
  ftaxIdentifier.Clear;
  ftransferMethod := EmptyStr;
end;

function TMateraBankTransfer.IsEmpty: Boolean;
begin
  Result := EstaVazio(faccountDestination) and
    EstaVazio(faccountDigitDestination) and
    (faccountTypeDestination = matdNone) and
    EstaVazio(fbankDestination) and
    EstaVazio(fbranchDestination) and
    EstaVazio(fhistoryCode) and
    EstaVazio(fname) and
    EstaVazio(fpersonType) and
    EstaVazio(fpurposeCode) and
    ftaxIdentifier.IsEmpty and
    EstaVazio(ftransferMethod);

end;

procedure TMateraBankTransfer.Assign(aSource: TMateraBankTransfer);
begin
  faccountDestination := aSource.accountDestination;
  faccountDigitDestination := aSource.accountDigitDestination;
  faccountTypeDestination := aSource.accountTypeDestination;
  fbankDestination := aSource.bankDestination;
  fbranchDestination := aSource.branchDestination;
  fhistoryCode := aSource.historyCode;
  fname := aSource.name;
  fpersonType := aSource.personType;
  fpurposeCode := aSource.fpurposeCode;
  ftaxIdentifier := aSource.taxIdentifier;
  ftransferMethod := aSource.transferMethod;
end;

{ TMateraWithdrawInfo }

procedure TMateraWithdrawInfo.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (ASource is TMateraWithdrawInfo) then
      Assign(TMateraWithdrawInfo(ASource));
end;

procedure TMateraWithdrawInfo.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('withdrawType', MateraWithdrawTypeToString(fwithdrawType))
    .AddPair('senderComment', fsenderComment, False)
    .AddPairISODate('futureDate', ffutureDate, False);

  fbankTransfer.WriteToJSon(aJSon);
  fboleto.WriteToJSon(aJSon);
  futilities.WriteToJSon(aJSon);
  fexternal.WriteToJSon(aJSon);
  finstantPayment.WriteToJSon(aJSon);
end;

procedure TMateraWithdrawInfo.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  s: String;
begin
  {$IfDef FPC}s := EmptyStr;{$EndIf}

  aJSon
    .Value('withdrawType', s)
    .Value('senderComment', fsenderComment)
    .ValueISODate('futureDate', ffutureDate);

  fwithdrawType := StringToMateraWithdrawType(s);
  fbankTransfer.ReadFromJSon(aJSon);
  fboleto.ReadFromJSon(aJSon);
  futilities.ReadFromJSon(aJSon);
  fexternal.ReadFromJSon(aJSon);
  finstantPayment.ReadFromJSon(aJSon);
end;

constructor TMateraWithdrawInfo.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  fbankTransfer := TMateraBankTransfer.Create('bankTransfer');
  fboleto := TMateraBoletoTO.Create('boleto');
  futilities := TMateraUtilitiesTO.Create('utilities');
  fexternal := TMateraExternal.Create('external');
  finstantPayment := TMateraInstantPaymentRequest.Create('instantPayment');
end;

destructor TMateraWithdrawInfo.Destroy;
begin
  fbankTransfer.Free;
  fboleto.Free;
  futilities.Free;
  fexternal.Free;
  finstantPayment.Free;
  inherited Destroy;
end;

procedure TMateraWithdrawInfo.Clear;
begin
  fwithdrawType := mwtNone;
  fsenderComment := EmptyStr;
  ffutureDate := 0;
  fbankTransfer.Clear;
  fboleto.Clear;
  futilities.Clear;
  fexternal.Clear;
  finstantPayment.Clear;
end;

function TMateraWithdrawInfo.IsEmpty: Boolean;
begin
  Result := EstaVazio(fsenderComment) and
    EstaZerado(ffutureDate) and
    fbankTransfer.IsEmpty and
    fboleto.IsEmpty and
    futilities.IsEmpty and
    fexternal.IsEmpty and
    finstantPayment.IsEmpty;
end;

procedure TMateraWithdrawInfo.Assign(aSource: TMateraWithdrawInfo);
begin
  fwithdrawType := aSource.withdrawType;
  fsenderComment := aSource.senderComment;
  ffutureDate := aSource.futureDate;
  fbankTransfer.Assign(aSource.bankTransfer);
  fboleto.Assign(aSource.boleto);
  futilities.Assign(aSource.utilities);
  fexternal.Assign(aSource.external_);
  finstantPayment.Assign(aSource.instantPayment);
end;

{ TMateraRetiradaRequest }

procedure TMateraRetiradaRequest.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (ASource is TMateraRetiradaRequest) then
    Assign(TMateraRetiradaRequest(ASource));
end;

procedure TMateraRetiradaRequest.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('totalAmount', ftotalAmount)
    .AddPair('mediatorFee', fmediatorFee, False)
    .AddPair('currency', fcurrency);
  fwithdrawInfo.WriteToJSon(aJSon);
  aJSon.AddPair('externalIdentifier', fexternalIdentifier);
end;

procedure TMateraRetiradaRequest.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('totalAmount', ftotalAmount)
    .Value('mediatorFee', fmediatorFee)
    .Value('currency', fcurrency);
  fwithdrawInfo.ReadFromJSon(aJSon);
  aJSon.Value('externalIdentifier', fexternalIdentifier);
end;

constructor TMateraRetiradaRequest.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  fwithdrawInfo := TMateraWithdrawInfo.Create('withdrawInfo');
  Clear;
end;

destructor TMateraRetiradaRequest.Destroy;
begin
  fwithdrawInfo.Free;
  inherited Destroy;
end;

procedure TMateraRetiradaRequest.Clear;
begin
  ftotalAmount := 0;
  fmediatorFee := 0;
  fcurrency := EmptyStr;
  fwithdrawInfo.Clear;
  fexternalIdentifier := EmptyStr;
end;

function TMateraRetiradaRequest.IsEmpty: Boolean;
begin
  Result := EstaZerado(ftotalAmount) and
    EstaZerado(fmediatorFee) and
    EstaVazio(fcurrency) and
    EstaVazio(fexternalIdentifier) and
    fwithdrawInfo.IsEmpty;
end;

procedure TMateraRetiradaRequest.Assign(aSource: TMateraRetiradaRequest);
begin
  ftotalAmount := aSource.totalAmount;
  fmediatorFee := aSource.mediatorFee;
  fcurrency := aSource.currency;
  fexternalIdentifier := aSource.externalIdentifier;
  fwithdrawInfo.Assign(aSource.withdrawInfo);
end;

{ TMateraPSP }

procedure TMateraPSP.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraPSP) then
    Assign(TMateraPSP(aSource));
end;

procedure TMateraPSP.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('id', fid)
    .AddPair('name', fname)
    .AddPair('country', fcountry, False)
    .AddPair('currencies', fcurrencies);
end;

procedure TMateraPSP.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('id', fid)
    .Value('name', fname)
    .Value('country', fcountry)
    .Value('currencies', fcurrencies);
end;

constructor TMateraPSP.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  Clear;
end;

procedure TMateraPSP.Clear;
begin
  fid := EmptyStr;
  fname := EmptyStr;
  fcountry := EmptyStr;
  fcurrencies := nil;
end;

function TMateraPSP.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(fid) and
    EstaVazio(fname) and
    EstaVazio(fcountry);
end;

procedure TMateraPSP.Assign(aSource: TMateraPSP);
begin
  fid := aSource.id;
  fname := aSource.name;
  fcountry := aSource.country;
  fcurrencies := aSource.currencies;
end;

{ TMateraCounter }

procedure TMateraCounter.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (ASource is TMateraCounter) then
      Assign(TMateraCounter(ASource));
end;

procedure TMateraCounter.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('type', MateraAntifraudCounterToString(ftype_))
    .AddPair('by', fby)
    .AddPair('d3', fd3)
    .AddPair('d30', fd30)
    .AddPair('m6', fm6);
end;

procedure TMateraCounter.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  s: String;
begin
  {$IfDef FPC}s := EmptyStr;{$EndIf}

  aJSon
    .Value('type', s)
    .Value('by', fby)
    .Value('d3', fd3)
    .Value('d30', fd30)
    .Value('m6', fm6);

  ftype_ := StringToMateraAntifraudCounter(s);

end;

constructor TMateraCounter.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
end;

procedure TMateraCounter.Clear;
begin
  fby := EmptyStr;
  fd3 := 0;
  fd30 := 0;
  fm6 := 0;
  ftype_ := mafcNone;
end;

function TMateraCounter.IsEmpty: Boolean;
begin
  Result := EstaVazio(fby) and
    EstaZerado(fd3) and
    EstaZerado(fd30) and
    EstaZerado(fm6) and
    (ftype_ = mafcNone);
end;

procedure TMateraCounter.Assign(aSource: TMateraCounter);
begin
  fby := aSource.by;
  fd3 := aSource.d3;
  fd30 := aSource.d30;
  fm6 := aSource.m6;
  ftype_ := aSource.type_;
end;

{ TMateraCounterArray }

function TMateraCounterArray.GetItem(aIndex: Integer): TMateraCounter;
begin
  Result := TMateraCounter(inherited Items[aIndex]);
end;

procedure TMateraCounterArray.SetItem(aIndex: Integer; aValue: TMateraCounter);
begin
  inherited Items[aIndex] := aValue;
end;

function TMateraCounterArray.NewSchema: TACBrPIXSchema;
begin
  Result := New;
end;

function TMateraCounterArray.Add(aItem: TMateraCounter): Integer;
begin
  Result := inherited Add(aItem);
end;

procedure TMateraCounterArray.Insert(aIndex: Integer; aItem: TMateraCounter);
begin
  inherited Insert(aIndex, aItem);
end;

function TMateraCounterArray.New: TMateraCounter;
begin
  Result := TMateraCounter.Create('');
  Self.Add(Result);
end;

{ TMateraAntiFraudClearingInfo }

procedure TMateraAntiFraudClearingInfo.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (ASource is TMateraAntiFraudClearingInfo) then
      Assign(TMateraAntiFraudClearingInfo(ASource));

end;

procedure TMateraAntiFraudClearingInfo.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon.AddPair('lastUpdated', flastUpdated);
  counters.WriteToJSon(aJSon);
end;

procedure TMateraAntiFraudClearingInfo.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon.ValueISODateTime('lastUpdated', flastUpdated);
  counters.ReadFromJSon(aJSon);
end;

constructor TMateraAntiFraudClearingInfo.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  fcounters := TMateraCounterArray.Create('counters');
end;

destructor TMateraAntiFraudClearingInfo.Destroy;
begin
  fcounters.Free;
  inherited Destroy;
end;

procedure TMateraAntiFraudClearingInfo.Clear;
begin
  fcounters.Clear;
  flastUpdated := 0;
end;

function TMateraAntiFraudClearingInfo.IsEmpty: Boolean;
begin
  Result := (flastUpdated = 0) and fcounters.IsEmpty;
end;

procedure TMateraAntiFraudClearingInfo.Assign(
  aSource: TMateraAntiFraudClearingInfo);
begin
  flastUpdated := aSource.lastUpdated;
  fcounters := aSource.counters;
end;

{ TMateraAliasAccountHolder }

procedure TMateraAliasAccountHolder.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (ASource is TMateraAliasAccountHolder) then
      Assign(TMateraAliasAccountHolder(ASource));
end;

procedure TMateraAliasAccountHolder.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon.AddPair('name', fname);
  ftaxIdentifier.WriteTOJSon(aJSon);

end;

procedure TMateraAliasAccountHolder.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon.Value('name', fname);
  ftaxIdentifier.ReadFromJSon(aJSon);
end;

constructor TMateraAliasAccountHolder.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  ftaxIdentifier := TMateraTaxIdentifierBasic.Create('taxIdentifier');
end;

destructor TMateraAliasAccountHolder.Destroy;
begin
  ftaxIdentifier.Free;
  inherited Destroy;
end;

procedure TMateraAliasAccountHolder.Clear;
begin
  fname := EmptyStr;
  ftaxIdentifier.Clear;
end;

function TMateraAliasAccountHolder.IsEmpty: Boolean;
begin
  Result := EstaVazio(fname) and (ftaxIdentifier.IsEmpty);
end;

procedure TMateraAliasAccountHolder.Assign(aSource: TMateraAliasAccountHolder);
begin
  fname := aSource.name;
  ftaxIdentifier := aSource.taxIdentifier;
end;

{ TMateraDestinationAccount }

procedure TMateraDestinationAccount.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraDestinationAccount) then
    Assign(TMateraDestinationAccount(aSource));
end;

procedure TMateraDestinationAccount.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('branch', fbranch, False)
    .AddPair('account', faccount)
    .AddPair('accountType', MateraAccountTypeDestinationToString(faccountType));
end;

procedure TMateraDestinationAccount.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  s: String;
begin
  {$IFDEF FPC}s := EmptyStr;{$ENDIF}
  aJSon
    .Value('branch', fbranch)
    .Value('account', faccount)
    .Value('accountType', s);
  faccountType := StringToMateraAccountTypeDestination(s);
end;

constructor TMateraDestinationAccount.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  Clear;
end;

procedure TMateraDestinationAccount.Clear;
begin
  fbranch := EmptyStr;
  faccount := EmptyStr;
  faccountType := matdNone;
end;

function TMateraDestinationAccount.IsEmpty: Boolean;
begin
  Result := (faccountType = matdNone) and
    EstaVazio(fbranch) and
    EstaVazio(account);
end;

procedure TMateraDestinationAccount.Assign(aSource: TMateraDestinationAccount);
begin
  fbranch := aSource.branch;
  faccount := aSource.account;
  faccountType := aSource.accountType;
end;

{ TMateraDrawee }

procedure TMateraDrawee.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraDrawee) then
    Assign(TMateraDrawee(aSource));
end;

procedure TMateraDrawee.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon.AddPair('name', fname);
  ftaxIdentifier.WriteToJSon(aJSon);
end;

procedure TMateraDrawee.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon.Value('name', fname);
  ftaxIdentifier.ReadFromJSon(aJSon);
end;

constructor TMateraDrawee.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  ftaxIdentifier := TMateraTaxIdentifierBasic.Create('taxIdentifier');
  Clear;
end;

destructor TMateraDrawee.Destroy;
begin
  ftaxIdentifier.Free;
  inherited Destroy;
end;

procedure TMateraDrawee.Clear;
begin
  fname := EmptyStr;
  ftaxIdentifier.Clear;
end;

function TMateraDrawee.IsEmpty: Boolean;
begin
  Result := EstaVazio(fname) and ftaxIdentifier.IsEmpty;
end;

procedure TMateraDrawee.Assign(aSource: TMateraDrawee);
begin
  fname := aSource.name;
  ftaxIdentifier.Assign(aSource.taxIdentifier);
end;

{ TMateraCoupon }

procedure TMateraCoupon.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraCoupon) then
    Assign(TMateraCoupon(aSource));
end;

procedure TMateraCoupon.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('accountHolderId', faccountHolderId)
    .AddPair('accountId', faccountId)
    .AddPair('couponId', fcouponId)
    .AddPair('couponMerchantId', fcouponMerchantId)
    .AddPair('description', fdescription)
    .AddPair('discount', fdiscount)
    .AddPair('maxUse', fmaxUse)
    .AddPair('minExpenseValue', fminExpenseValue)
    .AddPair('seller', fseller)
    .AddPair('status', MateraActiveStatusToString(fstatus))
    .AddPair('useCount', fuseCount);

  if (dueDate > 0) then
    aJSon.AddPair('dueDate', fdueDate);
end;

procedure TMateraCoupon.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  s: String;
begin
  {$IFDEF FPC}s := EmptyStr;{$ENDIF}
  aJSon
    .Value('accountHolderId', faccountHolderId)
    .Value('accountId', faccountId)
    .Value('couponId', fcouponId)
    .Value('couponMerchantId', fcouponMerchantId)
    .Value('description', fdescription)
    .Value('discount', fdiscount)
    .ValueISODate('dueDate', fdueDate)
    .Value('maxUse', fmaxUse)
    .Value('minExpenseValue', fminExpenseValue)
    .Value('seller', fseller)
    .Value('status', s)
    .Value('useCount', fuseCount);

  fstatus := StringToMateraActiveStatus(s);
end;

constructor TMateraCoupon.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  Clear;
end;

procedure TMateraCoupon.Clear;
begin
  fmaxUse := 0;
  fdueDate := 0;
  fdiscount := 0;
  fuseCount := 0;
  fminExpenseValue := 0;
  fstatus := macNone;
  fseller := EmptyStr;
  fcouponId := EmptyStr;
  faccountId := EmptyStr;
  fdescription := EmptyStr;
  faccountHolderId := EmptyStr;
  fcouponMerchantId := EmptyStr;
end;

function TMateraCoupon.IsEmpty: Boolean;
begin
  Result := (fstatus = macNone) and
    EstaVazio(fseller) and
    EstaVazio(fcouponId) and
    EstaVazio(accountId) and
    EstaVazio(description) and
    EstaVazio(accountHolderId) and
    EstaVazio(couponMerchantId) and
    EstaZerado(fmaxUse) and
    EstaZerado(fdueDate) and
    EstaZerado(fdiscount) and
    EstaZerado(fuseCount) and
    EstaZerado(fminExpenseValue);
end;

procedure TMateraCoupon.Assign(aSource: TMateraCoupon);
begin
  fmaxUse := aSource.maxUse;
  fstatus := aSource.status;
  fseller := aSource.seller;
  fdueDate := aSource.dueDate;
  fdiscount := aSource.discount;
  fuseCount := aSource.useCount;
  fcouponId := aSource.couponId;
  faccountId := aSource.accountId;
  fdescription := aSource.description;
  fminExpenseValue := aSource.minExpenseValue;
  faccountHolderId := aSource.accountHolderId;
  fcouponMerchantId := aSource.couponMerchantId;
end;

{ TMateraAliasResponse }

procedure TMateraAliasResponse.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (ASource is TMateraAliasResponse) then
      Assign(TMateraAliasResponse(ASource));
end;

procedure TMateraAliasResponse.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('alias', falias_)
    .AddPair('aliasType', MateraAliasTypeToString(faliasType));
  faliasAccountHolder.WriteToJson(aJson);
  faccountDestination.WriteToJson(aJson);
  fpsp.WriteToJson(aJson);
  aJSon
    .AddPair('endToEndId', fendtoEndId)
    .AddPair('creationDate', fcreationDate);
  fantiFraudClearingInfo.WriteToJson(aJson);
end;

procedure TMateraAliasResponse.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  s: String;
begin
  {$IfDef FPC}s := EmptyStr;{$EndIf}

  aJSon
    .Value('alias', falias_)
    .Value('aliasType', s);

  faliasType := StringToMateraAliasType(s);

  faliasAccountHolder.ReadFromJson(aJson);
  faccountDestination.ReadFromJson(aJson);
  fpsp.ReadFromJson(aJson);
  aJSon
    .Value('endToEndId', fendtoEndId)
    .ValueISODateTime('creationDate', fcreationDate);
  fantiFraudClearingInfo.ReadFromJson(aJson);
end;

constructor TMateraAliasResponse.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  faccountDestination := TMateraDestinationAccount.Create('accountDestination');
  faliasAccountHolder := TMateraAliasAccountHolder.Create('aliasAccountHolder');
  fantiFraudClearingInfo := TMateraAntiFraudClearingInfo.Create('antiFraudClearingInfo');
  fpsp := TMateraPSP.Create('psp');
end;

destructor TMateraAliasResponse.Destroy;
begin
  faccountDestination.Free;
  faliasAccountHolder.Free;
  fantiFraudClearingInfo.Free;
  fpsp.Free;
  inherited Destroy;
end;

procedure TMateraAliasResponse.Clear;
begin
  faccountDestination.Clear;
  faliasAccountHolder.Clear;
  fantiFraudClearingInfo.Clear;
  fpsp.Clear;
  faliasType := malNone;
  falias_ := EmptyStr;
  fcreationDate := 0;
  fendtoEndId := EmptyStr;
end;

function TMateraAliasResponse.IsEmpty: Boolean;
begin
  Result := (faccountDestination.IsEmpty) and
    (faliasAccountHolder.IsEmpty) and
    (fantiFraudClearingInfo.IsEmpty) and
    (fpsp.IsEmpty) and
    (faliasType = malNone) and
    EstaVazio(falias_) and
    EstaZerado(fcreationDate) and
    EstaVazio(fendtoEndId);
end;

procedure TMateraAliasResponse.Assign(aSource: TMateraAliasResponse);
begin
  faliasType := aSource.aliasType;
  falias_ := aSource.alias_;
  fcreationDate := aSource.creationDate;
  fendtoEndId := aSource.endtoEndId;
  fpsp.Assign(aSource.psp);
  faccountDestination.Assign(aSource.accountDestination);
  faliasAccountHolder.Assign(aSource.aliasAccountHolder);
  fantiFraudClearingInfo.Assign(aSource.antiFraudClearingInfo);
end;

{ TMateraDevolucaoResponse }

procedure TMateraDevolucaoResponse.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (ASource is TMateraDevolucaoResponse) then
      Assign(TMateraDevolucaoResponse(ASource));
end;

procedure TMateraDevolucaoResponse.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon.AddPair('transactionId', ftransactionId);
end;

procedure TMateraDevolucaoResponse.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon.Value('transactionId', ftransactionId);
end;

constructor TMateraDevolucaoResponse.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  Clear;
end;

procedure TMateraDevolucaoResponse.Clear;
begin
  ftransactionId := EmptyStr;
end;

function TMateraDevolucaoResponse.IsEmpty: Boolean;
begin
  Result := EstaVazio(ftransactionId);
end;

procedure TMateraDevolucaoResponse.Assign(aSource: TMateraDevolucaoResponse);
begin
  ftransactionId := aSource.transactionId;
end;

{ TMateraDevolucaoRequest }

procedure TMateraDevolucaoRequest.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (ASource is TMateraDevolucaoRequest) then
    Assign(TMateraDevolucaoRequest(ASource));
end;

procedure TMateraDevolucaoRequest.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('additionalInformation', fadditionalInformation, False)
    .AddPair('amount', famount)
    .AddPair('externalIdentifier', fexternalIdentifier)
    .AddPair('mediatorFee', fmediatorFee)
    .AddPair('returnReasonCode', freturnReasonCode)
    .AddPair('returnReasonInformation', freturnReasonInformation, False)
    .AddPair('returnType', MateraReturnTypeToString(freturnType), False);

  if (not fperformDebit) then
    aJSon.AddPair('performDebit', fperformDebit);
end;

procedure TMateraDevolucaoRequest.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  s: String;
begin
  {$IfDef FPC}s := EmptyStr;{$EndIf}

  aJSon
    .Value('additionalInformation', fadditionalInformation)
    .Value('amount', famount)
    .Value('externalIdentifier', fexternalIdentifier)
    .Value('mediatorFee', fmediatorFee)
    .Value('performDebit', fperformDebit)
    .Value('returnReasonCode', freturnReasonCode)
    .Value('returnReasonInformation', freturnReasonInformation)
    .Value('returnType', s);

  freturnType := StringToMateraReturnType(s);
end;

constructor TMateraDevolucaoRequest.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  Clear;
end;

procedure TMateraDevolucaoRequest.Clear;
begin
  fadditionalInformation := EmptyStr;
  famount := 0;
  fexternalIdentifier := EmptyStr;
  fmediatorFee := 0;
  fperformDebit := True;
  freturnReasonCode := EmptyStr;
  freturnReasonInformation := EmptyStr;
  freturnType := mrtNone;
end;

function TMateraDevolucaoRequest.IsEmpty: Boolean;
begin
  Result := (famount = 0) and
    (fmediatorFee = 0) and
    (freturnType = mrtNone) and
    EstaVazio(fexternalIdentifier) and
    EstaVazio(freturnReasonCode) and
    EstaVazio(fadditionalInformation) and
    EstaVazio(freturnReasonInformation);
end;

procedure TMateraDevolucaoRequest.Assign(aSource: TMateraDevolucaoRequest);
begin
  fadditionalInformation := aSource.additionalInformation;
  famount := aSource.amount;
  fexternalIdentifier := aSource.externalIdentifier;
  fmediatorFee := aSource.mediatorFee;
  fperformDebit := aSource.performDebit;
  freturnReasonCode := aSource.returnReasonCode;
  freturnReasonInformation := aSource.returnReasonInformation;
  freturnType := aSource.returnType;
end;

{ TMateraReturnCode }

procedure TMateraReturnCode.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (ASource is TMateraReturnCode) then
    Assign(TMateraReturnCode(ASource));
end;

procedure TMateraReturnCode.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('code', fcode)
    .AddPair('description', fdescription);
end;

procedure TMateraReturnCode.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('code', fcode)
    .Value('description', fdescription);
end;

constructor TMateraReturnCode.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  Clear;
end;

procedure TMateraReturnCode.Clear;
begin
  fcode := EmptyStr;
  fdescription := EmptyStr;
end;

function TMateraReturnCode.IsEmpty: Boolean;
begin
  Result := EstaVazio(fcode) and EstaVazio(fdescription);
end;

procedure TMateraReturnCode.Assign(aSource: TMateraReturnCode);
begin
  fcode := aSource.code;
  fdescription := aSource.description;
end;

{ TMateraReturnCodeArray }

function TMateraReturnCodeArray.GetItem(aIndex: Integer): TMateraReturnCode;
begin
  Result := TMateraReturnCode(inherited Items[aIndex]);
end;

procedure TMateraReturnCodeArray.SetItem(aIndex: Integer;
  aValue: TMateraReturnCode);
begin
  inherited Items[aIndex] := aValue;
end;

function TMateraReturnCodeArray.NewSchema: TACBrPIXSchema;
begin
  Result := New;
end;

function TMateraReturnCodeArray.Add(aItem: TMateraReturnCode): Integer;
begin
  Result := inherited Add(aItem);
end;

procedure TMateraReturnCodeArray.Insert(aIndex: Integer;
  aItem: TMateraReturnCode);
begin
  inherited Insert(aIndex, aItem);
end;

function TMateraReturnCodeArray.New: TMateraReturnCode;
begin
  Result := TMateraReturnCode.Create('');
  Self.Add(Result);
end;

{ TMateraReturnCodesQueryResponse }

procedure TMateraReturnCodesQueryResponse.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (ASource is TMateraReturnCodesQueryResponse) then
    Assign(TMateraReturnCodesQueryResponse(ASource));
end;

procedure TMateraReturnCodesQueryResponse.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  freturnCodes.WriteToJSon(aJSon);
end;

procedure TMateraReturnCodesQueryResponse.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  freturnCodes.ReadFromJSon(aJSon);
end;

constructor TMateraReturnCodesQueryResponse.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  freturnCodes := TMateraReturnCodeArray.Create('returnCodes');
end;

destructor TMateraReturnCodesQueryResponse.Destroy;
begin
  freturnCodes.Free;
  inherited Destroy;
end;

procedure TMateraReturnCodesQueryResponse.Clear;
begin
  freturnCodes.Clear;
end;

function TMateraReturnCodesQueryResponse.IsEmpty: Boolean;
begin
  Result := freturnCodes.IsEmpty;
end;

procedure TMateraReturnCodesQueryResponse.Assign(
  aSource: TMateraReturnCodesQueryResponse);
begin
  freturnCodes := aSource.returnCodes;
end;

{ TMateraReductionCalculation }

procedure TMateraValueCalculation.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraValueCalculation) then
    Assign(TMateraValueCalculation(aSource));
end;

procedure TMateraValueCalculation.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('valuePerc', fvaluePerc)
    .AddPair('modality', fmodality);
end;

procedure TMateraValueCalculation.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('valuePerc', fvaluePerc)
    .Value('modality', fmodality);
end;

constructor TMateraValueCalculation.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  Clear;
end;

procedure TMateraValueCalculation.Clear;
begin
  fmodality := 0;
  fvaluePerc := 0;
end;

function TMateraValueCalculation.IsEmpty: Boolean;
begin
  Result := EstaZerado(fmodality) and EstaZerado(fvaluePerc);
end;

procedure TMateraValueCalculation.Assign(aSource: TMateraValueCalculation);
begin
  fmodality := aSource.modality;
  fvaluePerc := aSource.valuePerc;
end;

{ TMateraQRCodeDynamicPayerInformationComplete }

procedure TMateraQRCodeDynamicPayerInformationComplete.AssignSchema(
  aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraQRCodeDynamicPayerInformationComplete) then
    Assign(TMateraQRCodeDynamicPayerInformationComplete(aSource));
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
  faddressing.ReadFromJSon(aJson);
end;

constructor TMateraQRCodeDynamicPayerInformationComplete.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  faddressing := TMateraPayerInformationAddressing.Create('addressing');
  Clear;
end;

destructor TMateraQRCodeDynamicPayerInformationComplete.Destroy;
begin
  faddressing.Free;
  inherited Destroy;
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
  Result := (faddressing.IsEmpty) and
    EstaVazio(fcpfCnpj) and
    EstaVazio(femail) and
    EstaVazio(fname);
end;

procedure TMateraQRCodeDynamicPayerInformationComplete.Assign(
  aSource: TMateraQRCodeDynamicPayerInformationComplete);
begin
  fname := aSource.name;
  femail := aSource.email;
  fcpfCnpj := aSource.cpfCnpj;
  faddressing.Assign(aSource.addressing);
end;

{ TMateraFixedDateDiscountArray }

function TMateraFixedDateDiscountArray.GetItem(aIndex: Integer): TMateraFixedDateDiscount;
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

function TMateraFixedDateDiscountArray.Add(aItem: TMateraFixedDateDiscount): Integer;
begin
  Result := inherited Add(aItem);
end;

procedure TMateraFixedDateDiscountArray.Insert(aIndex: Integer; aItem: TMateraFixedDateDiscount);
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
  if (aSource is TMateraFixedDateDiscount) then
    Assign(TMateraFixedDateDiscount(aSource));
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
  Clear;
end;

procedure TMateraFixedDateDiscount.Clear;
begin
  fvaluePerc := 0;
  fdate := 0;
end;

function TMateraFixedDateDiscount.IsEmpty: Boolean;
begin
  Result:= EstaZerado(fdate) and EstaZerado(fvaluePerc);
end;

procedure TMateraFixedDateDiscount.Assign(aSource: TMateraFixedDateDiscount);
begin
  fvaluePerc := aSource.valuePerc;
  fdate := aSource.date;
end;

{ TMateraFixedDateDiscountList }

procedure TMateraFixedDateDiscountList.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraFixedDateDiscountList) then
    Assign(TMateraFixedDateDiscountList(aSource));
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
  Clear;
end;

destructor TMateraFixedDateDiscountList.Destroy;
begin
  ffixedDateDiscounts.Free;
  inherited Destroy;
end;

procedure TMateraFixedDateDiscountList.Clear;
begin
  fmodality := 0;
  ffixedDateDiscounts.Clear;
end;

function TMateraFixedDateDiscountList.IsEmpty: Boolean;
begin
  Result := ffixedDateDiscounts.IsEmpty and EstaZerado(fmodality);
end;

procedure TMateraFixedDateDiscountList.Assign(aSource: TMateraFixedDateDiscountList);
begin
  fmodality := aSource.modality;
  ffixedDateDiscounts.Assign(aSource.fixedDateDiscounts);
end;

{ TMateraUniqueDiscount }

procedure TMateraUniqueDiscount.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraUniqueDiscount) then
    Assign(TMateraUniqueDiscount(aSource));
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
  Clear;
end;

procedure TMateraUniqueDiscount.Clear;
begin
  funiqueValuePercDiscount := 0;
  fmodality := 0;
end;

function TMateraUniqueDiscount.IsEmpty: Boolean;
begin
  Result:= (funiqueValuePercDiscount = 0) and (fmodality = 0);
end;

procedure TMateraUniqueDiscount.Assign(aSource: TMateraUniqueDiscount);
begin
  uniqueValuePercDiscount := aSource.uniqueValuePercDiscount;
  modality := aSource.modality;
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
  else if (s = 'ERROR') then
    Result := mtsError
  else
    Result := mtsNone;
end;

function MateraReturnTypeToString(aType: TMateraReturnType): String;
begin
  case aType of
    mrtTransactionValue: Result := 'TRANSACTIONVALUE';
    mrtCashValue: Result := 'CASHVALUE';
    mrtOperationFlaw: Result := 'OPERATIONFLAW';
    mrtFraud: Result := 'FRAUD';
  else
    Result := EmptyStr;
  end;
end;

function StringToMateraReturnType(const aString: String): TMateraReturnType;
var
  s: String;
begin
  s := UpperCase(Trim(aString));

  if (s = 'TRANSACTIONVALUE') then
    Result := mrtTransactionValue
  else if (s = 'CASHVALUE') then
    Result := mrtCashValue
  else if (s = 'OPERATIONFLAW') then
    Result := mrtOperationFlaw
  else if (s = 'FRAUD') then
    Result := mrtFraud
  else
    Result := mrtNone;
end;

function MateraAccountTypeDestinationToString(aType: TMateraAccountTypeDestination): String;
begin
  case aType of
    matdCC: Result := 'CC';
    matdPoupanca: Result := 'POUPANCA';
    matdSalario: Result := 'SALARIO';
    matdIP: Result := 'IP';
  else
    Result := EmptyStr;
  end;
end;

function StringToMateraAccountTypeDestination(const aString: String): TMateraAccountTypeDestination;
var
  s: String;
begin
  s := UpperCase(Trim(aString));

  if (s = 'CC') then
    Result := matdCC
  else if (s = 'POUPANCA') then
    Result := matdPoupanca
  else if (s = 'SALARIO') then
    Result := matdSalario
  else if (s = 'IP') then
    Result := matdIP
  else
    Result := matdNone;
end;

function MateraAntifraudCounterToString(aType: TMateraAntifraudCounter): String;
begin
  case aType of
    mafcSettlements: Result := 'SETTLEMENTS';
    mafcReported_Frauds: Result := 'REPORTED_FRAUDS';
    mafcReported_AML_CFT: Result := 'REPORTED_AML_CFT';
    mafcConfirmed_Frauds: Result := 'CONFIRMED_FRAUDS';
    mafcConfirmed_AML_CFT: Result := 'CONFIRMED_AML_CFT';
    mafcRejected: Result := 'REJECTED';
  else
    Result := EmptyStr;
  end;
end;

function StringToMateraAntifraudCounter(const aString: String
  ): TMateraAntifraudCounter;
Var
  s: String;
begin
  s := UpperCase(Trim(aString));

  if (s = 'SETTLEMENTS') then
    Result := mafcSettlements
  else if (s = 'REPORTED_FRAUDS') then
    Result := mafcReported_Frauds
  else if (s = 'REPORTED_AML_CFT') then
    Result := mafcReported_AML_CFT
  else if (s = 'CONFIRMED_FRAUDS') then
    Result := mafcConfirmed_Frauds
  else if (s = 'CONFIRMED_AML_CFT') then
    Result := mafcConfirmed_AML_CFT
  else if (s = 'REJECTED') then
    Result := mafcRejected
  else
    Result := mafcNone;

end;

function MateraWithdrawTypeToString(aType: TMateraWithdrawType): String;
begin
  case aType of
    mwtBankTransfer: Result := 'BankTransfer';
    mwtExternal: Result := 'External';
    mwtBoleto: Result := 'Boleto';
    mwtReloadPrepaid: Result := 'ReloadPrepaid';
    mwtUtilities: Result := 'Utilities';
    mwtInstantPayment: Result := 'InstantPayment';
  else
    Result := EmptyStr;
  end;

end;

function StringToMateraWithdrawType(const aString: String): TMateraWithdrawType;
Var
  s: String;
begin
  s := UpperCase(Trim(aString));

  if (s = 'BankTransfer') then
    Result := mwtBankTransfer
  else if (s = 'External') then
    Result := mwtExternal
  else if (s = 'Boleto') then
    Result := mwtBoleto
  else if (s = 'ReloadPrepaid') then
    Result := mwtReloadPrepaid
  else if (s = 'Utilities') then
    Result := mwtUtilities
  else if (s = 'InstantPayment') then
    Result := mwtInstantPayment
  else
    Result := mwtNone;

end;

function MaterainitiationFormToString(aType: TMaterainitiationForm): String;
begin
  case aType of
    mifINIC: Result := 'INIC';
    mifMANU: Result := 'MANU';
    mifDICT: Result := 'DICT';
    mifQRES: Result := 'QRES';
    mifQRDN: Result := 'QRDN';
  else
    Result := EmptyStr;
  end;
end;

function StringToMaterainitiationForm(const aString: String
  ): TMaterainitiationForm;
Var
  s: String;
begin
  s := UpperCase(Trim(aString));

  if (s = 'INIC') then
    Result := mifINIC
  else if (s = 'MANU') then
    Result := mifMANU
  else if (s = 'DICT') then
    Result := mifDICT
  else if (s = 'QRES') then
    Result := mifQRES
  else if (s = 'QRDN') then
    Result := mifQRDN
  else
    Result := mifNone;

end;

function MaterainitiationProcedureToString(aType: TMateraInitiationProcedure
  ): String;
begin
  case aType of
    mipManual: Result := 'MANUAL';
    mipPre_Stored: Result := 'PRE_STORED';
  else
    Result := EmptyStr;
  end;

end;

function StringToMaterainitiationProcedure(const aString: String
  ): TMateraInitiationProcedure;
Var
  s: String;
begin
  s := UpperCase(Trim(aString));

  if (s = 'MANUAL') then
    Result := mipManual
  else if (s = 'PRE_STORED') then
    Result := mipPre_Stored
  else
    Result := mipNone;

end;

function MateraqrcodeTypeToString(aType: TMateraqrcodeType): String;
begin
  case aType of
    mqrtStatic: Result := 'STATIC';
    mqrtDynamic: Result := 'DYNAMIC';
  else
    Result := EmptyStr;
  end;

end;

function StringToMateraqrcodeType(const aString: String): TMateraqrcodeType;
Var
  s: String;
begin
  s := UpperCase(Trim(aString));

  if (s = 'STATIC') then
    Result := mqrtStatic
  else if (s = 'DYNAMIC') then
    Result := mqrtDynamic
  else
    Result := mqrtNone;

end;

function MaterainstructionPriorityToString(aType: TMateraInstructionPriority
  ): String;
begin
  case aType of
    maipHigh: Result := 'HIGH';
    maipNorm: Result := 'NORM';
  else
    Result := EmptyStr;
  end;

end;

function StringToMaterainstructionPriority(const aString: String
  ): TMateraInstructionPriority;
Var
  s: String;
begin
  s := UpperCase(Trim(aString));

  if (s = 'HIGH') then
    Result := maipHigh
  else if (s = 'NORM') then
    Result := maipNorm
  else
    Result := maipNone;

end;

function MateratransactionPurposeToString(aType: TMateraTransactionPurpose
  ): String;
begin
  case aType of
    mtpIPAY: Result := 'IPAY';
    mtpGSCB: Result := 'GSCB';
    mtpOTHR: Result := 'OTHR';
  else
    Result := EmptyStr;
  end;
end;

function StringToMateratransactionPurpose(const aString: String
  ): TMateraTransactionPurpose;
Var
  s: String;
begin
  s := UpperCase(Trim(aString));

  if (s = 'IPAY') then
    Result := mtpIPAY
  else if (s = 'GSCB') then
    Result := mtpGSCB
  else if (s = 'OTHR') then
    Result := mtpOTHR
  else
    Result := mtpNone;

end;

function MaterainstructionTypeToString(aType: TMateraInstructionType): String;
begin
  case aType of
    mitPAGPRI: Result := 'PAGPRI';
    mitPAGFRD: Result := 'PAGFRD';
    mitPAGAGD: Result := 'PAGAGD';
  else
    Result := EmptyStr;
  end;
end;

function StringToMaterainstructionType(const aString: String): TMateraInstructionType;
Var
  s: String;
begin
  s := UpperCase(Trim(aString));

  if (s = 'PAGPRI') then
    Result := mitPAGPRI
  else if (s = 'PAGFRD') then
    Result := mitPAGFRD
  else if (s = 'PAGAGD') then
    Result := mitPAGAGD
  else
    Result := mitNone;
end;

function MateraWithdrawAgentTypeToString(aType: TMateraWithdrawAgentType): String;
begin
  case aType of
    mwatAGPSS: Result := 'AGPSS';
    mwatAGTEC: Result := 'AGTEC';
    mwatAGTOT: Result := 'AGTOT';
    mwatAGFSS: Result := 'AGFSS';
  else
    Result := EmptyStr;
  end;
end;

function StringToMateraWithdrawAgentType(const aString: String): TMateraWithdrawAgentType;
var
  s: String;
begin
  s := UpperCase(Trim(aString));

  if (s = 'AGPSS') then
    Result := mwatAGPSS
  else if (s = 'AGTEC') then
    Result := mwatAGTEC
  else if (s = 'AGTOT') then
    Result := mwatAGTOT
  else if (s = 'AGFSS') then
    Result := mwatAGFSS
  else
    Result := mwatNone;
end;

function MateraCashValueTypeToString(aType: TMateraCashValueType): String;
begin
  case aType of
    mcvChange: Result := 'CHANGE';
    mcvWithdraw: Result := 'WITHDRAW';
  else
    Result := EmptyStr;
  end;
end;

function StringToMateraCashValueType(const aString: String): TMateraCashValueType;
var
  s: String;
begin
  s := UpperCase(Trim(aString));

  if (s = 'CHANGE') then
    Result := mcvChange
  else if (s = 'WITHDRAW') then
    Result := mcvWithdraw
  else
    Result := mcvNone;
end;

function MaterastatementDeletionModeToString(aType: TMaterastatementDeletionMode): String;
begin
  case aType of
    msdRefund: Result := 'REFUND';
    msdExclusion: Result := 'EXCLUSION';
    msdExclusion_Preferred: Result := 'EXCLUSION_PREFERRED';
  else
    Result := EmptyStr;
  end;

end;

function StringToMaterastatementDeletionMode(const aString: String): TMaterastatementDeletionMode;
var
  s: String;
begin
  s := UpperCase(Trim(aString));

  if (s = 'REFUND') then
    Result := msdRefund
  else if (s = 'EXCLUSION') then
    Result := msdExclusion
  else if (s = 'EXCLUSION_PREFERRED') then
    Result := msdExclusion_Preferred
  else
    Result := msdNone;
end;

function MaterastatementEntryTypeToString(aType: TMaterastatementEntryType
  ): String;
begin
  case aType of
    msetC: Result := 'C';
    msetD: Result := 'D';
    msetS: Result := 'S';
  else
    Result := EmptyStr;
  end;

end;

function StringToMaterastatementEntryType(const aString: String
  ): TMaterastatementEntryType;
var
  s: String;
begin
  s := UpperCase(Trim(aString));

  if (s = 'C') then
    Result := msetC
  else if (s = 'D') then
    Result := msetD
  else if (s = 'S') then
    Result := msetS
  else
    Result := msetNone;

end;

function MateraStatementIPCashValueTypeToString(
  aType: TMateraStatementIPCashValueType): String;
begin
  case aType of
    msipvtPix_Purchase: Result := 'PIX_PURCHASE';
    msipvtPix_Change: Result := 'PIX_CHANGE';
  else
    Result := EmptyStr;
  end;

end;

function StringToMateraStatementIPCashValueType(const aString: String
  ): TMateraStatementIPCashValueType;
var
  s: String;
begin
  s := UpperCase(Trim(aString));

  if (s = 'PIX_PURCHASE') then
    Result := msipvtPix_Purchase
  else if (s = 'PIX_CHANGE') then
    Result := msipvtPix_Change
  else
    Result := msipvtNone;

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

procedure TMateraTransactionResponseArray.WriteToJSon(AJSon: TACBrJSONObject);
begin
  inherited WriteToJSon(AJSon);
  AJSon.AddPair('hashNextPage', fhashNextPage, False);
end;

procedure TMateraTransactionResponseArray.ReadFromJSon(AJSon: TACBrJSONObject);
begin
  inherited ReadFromJSon(AJSon);
  AJSon.Value('hashNextPage', fhashNextPage);
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
    .AddPair('accountHolderId', faccountHolderId, False)
    .AddPair('accountId', faccountId, False)
    .AddPair('cancelTransactionId', fcancelTransactionId, False)
    .AddPair('currentAmount', fcurrentAmount, False)
    .AddPair('discountAmount', fdiscountAmount, False)
    .AddPair('docUrl', fdocUrl, False)
    .AddPair('otherInstallmentValues', fotherInstallmentValues, False)
    .AddPair('paidAmount', fpaidAmount, False)
    .AddPair('recipientDescription', frecipientDescription, False)
    .AddPair('sideAccountId', fsideAccountId, False)
    .AddPair('email', femail, False)
    .AddPair('endToEndId', fendToEndId, False)
    .AddPair('firstInstallmentValue', ffirstInstallmentValue, False)
    .AddPair('installmentQuantity', finstallmentQuantity, False)
    .AddPair('totalAmount', ftotalAmount, False)
    .AddPair('transactionDate', DateTimeToIso8601(ftransactionDate), False)
    .AddPair('transactionId', ftransactionId, False)
    .AddPair('transactionStatus', MateraTransactionStatusToString(ftransactionStatus), False)
    .AddPair('transactionType', ftransactionType, False);

  fbankTransfer.WriteToJSon(aJSon);
  fcancelPaymentTransaction.WriteToJSon(aJSon);
  fcounterPart.WriteToJSon(aJSon);
  fcoupon.WriteToJSon(aJSon);
  fboleto.WriteToJSon(aJSon);
  fdrawee.WriteToJSon(aJSon);
  finstantPayment.WriteToJSon(aJSon);
  fmobilePhone.WriteToJSon(aJSon);
  futilities.WriteToJSon(aJSon);
end;

procedure TMateraTransactionResponse.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  s1: String;
begin
  {$IFDEF FPC}s1 := EmptyStr;{$ENDIF}
  aJSon
    .Value('accountHolderId', faccountHolderId)
    .Value('accountId', faccountId)
    .Value('cancelTransactionId', fcancelTransactionId)
    .Value('currentAmount', fcurrentAmount)
    .Value('discountAmount', fdiscountAmount)
    .Value('docUrl', fdocUrl)
    .Value('otherInstallmentValues', fotherInstallmentValues)
    .Value('paidAmount', fpaidAmount)
    .Value('recipientDescription', frecipientDescription)
    .Value('sideAccountId', fsideAccountId)
    .Value('email', femail)
    .Value('endToEndId', fendToEndId)
    .Value('firstInstallmentValue', ffirstInstallmentValue)
    .Value('installmentQuantity', finstallmentQuantity)
    .Value('totalAmount', ftotalAmount)
    .ValueISODateTime('transactionDate', ftransactionDate)
    .Value('transactionId', ftransactionId)
    .Value('transactionType', ftransactionType)
    .Value('transactionStatus', s1);

  ftransactionStatus := StringToMateraTransactionStatus(s1);
  fbankTransfer.ReadFromJSon(aJSon);
  fcancelPaymentTransaction.ReadFromJSon(aJSon);
  fcounterPart.ReadFromJSon(aJSon);
  fcoupon.ReadFromJSon(aJSon);
  fboleto.ReadFromJSon(aJSon);
  fdrawee.ReadFromJSon(aJSon);
  finstantPayment.ReadFromJSon(aJSon);
  fmobilePhone.ReadFromJSon(aJSon);
  futilities.ReadFromJSon(aJSon);
end;

constructor TMateraTransactionResponse.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  fbankTransfer := TMateraBankTransfer.Create('bankTransfer');
  fcancelPaymentTransaction := TMateraCancelPaymentTransactionResponse.Create('cancelPaymentTransaction');
  fcounterPart := TMateraAccountHolderResponse.Create('counterPart');
  fcoupon := TMateraCoupon.Create('coupon');
  fboleto := TMateraBoletoPayment.Create('boleto');
  fdrawee := TMateraDrawee.Create('drawee');
  finstantPayment := TMateraInstantPaymentTransactionResponse.Create('instantPayment');
  fmobilePhone := TMateraMobilePhone.Create('mobilePhone');
  futilities := TMateraUtilitiesPayment.Create('utilities');
  Clear;
end;

destructor TMateraTransactionResponse.Destroy;
begin
  fbankTransfer.Free;
  fcancelPaymentTransaction.Free;
  fcounterPart.Free;
  fcoupon.Free;
  fboleto.Free;
  fdrawee.Free;
  finstantPayment.Free;
  fmobilePhone.Free;
  futilities.Free;
  inherited Destroy;
end;

procedure TMateraTransactionResponse.Clear;
begin
  ftotalAmount := 0;
  fpaidAmount := 0;
  fcurrentAmount := 0;
  fdiscountAmount := 0;
  ftransactionDate := 0;
  finstallmentQuantity := 0;
  ffirstInstallmentValue := 0;
  fotherInstallmentValues := 0;
  femail := EmptyStr;
  fdocUrl := EmptyStr;
  faccountId := EmptyStr;
  fendToEndId := EmptyStr;
  fsideAccountId := EmptyStr;
  ftransactionId := EmptyStr;
  ftransactionType := EmptyStr;
  faccountHolderId := EmptyStr;
  ftransactionStatus := mtsNone;
  fcancelTransactionId := EmptyStr;
  frecipientDescription := EmptyStr;

  fbankTransfer.Clear;
  fcancelPaymentTransaction.Clear;
  fcounterPart.Clear;
  fcoupon.Clear;
  fboleto.Clear;
  fdrawee.Clear;
  finstantPayment.Clear;
  fmobilePhone.Clear;
  futilities.Clear;
end;

function TMateraTransactionResponse.IsEmpty: Boolean;
begin
  Result :=
    (ftransactionStatus = mtsNone) and
    EstaZerado(ftotalAmount) and
    EstaZerado(fpaidAmount) and
    EstaZerado(fcurrentAmount) and
    EstaZerado(fdiscountAmount) and
    EstaZerado(ftransactionDate) and
    EstaZerado(finstallmentQuantity) and
    EstaZerado(ffirstInstallmentValue) and
    EstaZerado(fotherInstallmentValues) and
    EstaVazio(femail) and
    EstaVazio(fdocUrl) and
    EstaVazio(fendToEndId) and
    EstaVazio(fsideAccountId) and
    EstaVazio(fcancelTransactionId) and
    EstaVazio(frecipientDescription) and
    EstaVazio(faccountHolderId) and
    EstaVazio(faccountId) and
    EstaVazio(ftransactionType) and
    EstaVazio(ftransactionId) and
    fbankTransfer.IsEmpty and
    fcancelPaymentTransaction.IsEmpty and
    fcounterPart.IsEmpty and
    fcoupon.IsEmpty and
    fboleto.IsEmpty and
    fdrawee.IsEmpty and
    finstantPayment.IsEmpty and
    fmobilePhone.IsEmpty and
    futilities.IsEmpty;
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

  fpaidAmount := aSource.paidAmount;
  fcurrentAmount := aSource.currentAmount;
  fdiscountAmount := aSource.discountAmount;
  finstallmentQuantity := aSource.installmentQuantity;
  ffirstInstallmentValue := aSource.firstInstallmentValue;
  fotherInstallmentValues := aSource.otherInstallmentValues;
  femail := aSource.email;
  fdocUrl := aSource.docUrl;
  fendToEndId := aSource.endToEndId;
  fsideAccountId := aSource.sideAccountId;
  fcancelTransactionId := aSource.cancelTransactionId;
  frecipientDescription := aSource.recipientDescription;

  fbankTransfer.Assign(aSource.bankTransfer);
  fcancelPaymentTransaction.Assign(aSource.cancelPaymentTransaction);
  fcounterPart.Assign(aSource.counterPart);
  fcoupon.Assign(aSource.coupon);
  fboleto.Assign(aSource.boleto);
  fdrawee.Assign(aSource.drawee);
  finstantPayment.Assign(aSource.instantPayment);
  fmobilePhone.Assign(aSource.mobilePhone);
  futilities.Assign(aSource.utilities);
end;

{ TMateraDiscountsCalculation }

procedure TMateraDiscountsCalculation.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraDiscountsCalculation) then
    Assign(TMateraDiscountsCalculation(aSource));
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
  Clear;
end;

destructor TMateraDiscountsCalculation.Destroy;
begin
  ffixedDateDiscountList.Free;
  funiqueDiscount.Free;
  inherited Destroy;
end;

procedure TMateraDiscountsCalculation.Clear;
begin
  ffixedDateDiscountList.Clear;
  funiqueDiscount.Clear;
end;

function TMateraDiscountsCalculation.IsEmpty: Boolean;
begin
  Result := ffixedDateDiscountList.IsEmpty and funiqueDiscount.IsEmpty;
end;

procedure TMateraDiscountsCalculation.Assign(aSource: TMateraDiscountsCalculation);
begin
  fixedDateDiscountList.Assign(aSource.fixedDateDiscountList);
  uniqueDiscount.Assign(aSource.uniqueDiscount);
end;

{ TMateraBillingDueDate }

procedure TMateraBillingDueDate.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraBillingDueDate) then
    Assign(TMateraBillingDueDate(aSource));
end;

procedure TMateraBillingDueDate.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  if (fdueDate <> 0) then
    aJSon.AddPair('dueDate', FormatDateTime('yyyy-mm-dd', fdueDate));
  aJSon.AddPair('daysAfterDueDate', fdaysAfterDueDate);

  ffines.WriteToJSon(aJSon);
  freduction.WriteToJSon(aJSon);
  fdiscounts.WriteToJSon(aJSon);
  finterests.WriteToJSon(aJSon);
  fpayerInformation.WriteToJSon(aJSon);
end;

procedure TMateraBillingDueDate.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .ValueISODate('dueDate', fdueDate)
    .Value('daysAfterDueDate', fdaysAfterDueDate);

  ffines.ReadFromJSon(aJSon);
  freduction.ReadFromJSon(aJSon);
  fdiscounts.ReadFromJSon(aJSon);
  finterests.ReadFromJSon(aJSon);
  fpayerInformation.ReadFromJSon(aJSon);
end;

constructor TMateraBillingDueDate.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  ffines := TMateraValueCalculation.Create('fines');
  finterests := TMateraValueCalculation.Create('interests');
  freduction := TMateraValueCalculation.Create('reduction');
  fdiscounts := TMateraDiscountsCalculation.Create('discounts');
  fpayerInformation := TMateraQRCodeDynamicPayerInformationComplete.Create('payerInformation');
  Clear;
end;

destructor TMateraBillingDueDate.Destroy;
begin             
  ffines.Free;
  fdiscounts.Free;
  finterests.Free;
  freduction.Free;
  fpayerInformation.Free;
  inherited Destroy;
end;

procedure TMateraBillingDueDate.Clear;
begin
  fdueDate := 0;
  fdaysAfterDueDate := 0;
  ffines.Clear;
  finterests.Clear;
  fdiscounts.Clear;
  freduction.Clear;
  fpayerInformation.Clear;
end;

function TMateraBillingDueDate.IsEmpty: Boolean;
begin
  Result:=
    (fdaysAfterDueDate = 0) and
    (fdueDate = 0) and
    (ffines.IsEmpty) and
    (freduction.IsEmpty) and
    (finterests.IsEmpty) and   
    (fdiscounts.IsEmpty) and
    (fpayerInformation.IsEmpty);
end;

procedure TMateraBillingDueDate.Assign(aSource: TMateraBillingDueDate);
begin
  fdaysAfterDueDate := aSource.daysAfterDueDate;
  fdueDate := aSource.dueDate;
  ffines.Assign(aSource.fines);
  finterests.Assign(aSource.interests);
  fdiscounts.Assign(aSource.discounts);
  freduction.Assign(aSource.reduction);
  fpayerInformation.Assign(aSource.payerInformation);
end;

{ TMateraCouponDetails }

procedure TMateraCouponDetails.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraCouponDetails) then
    Assign(TMateraCouponDetails(aSource));
end;

procedure TMateraCouponDetails.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('couponId', fcouponId)
    .AddPair('seller', fseller)
    .AddPair('description', fdescription)
end;

procedure TMateraCouponDetails.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('couponId', fcouponId)
    .Value('description', fdescription)
    .Value('seller', fseller);
end;

constructor TMateraCouponDetails.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  Clear;
end;

procedure TMateraCouponDetails.Clear;
begin
  fcouponId := EmptyStr;
  fdescription := EmptyStr;
  fseller := EmptyStr;
end;

function TMateraCouponDetails.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(fcouponId) and
    EstaVazio(fdescription) and
    EstaVazio(fseller);
end;

procedure TMateraCouponDetails.Assign(aSource: TMateraCouponDetails);
begin
  fcouponId := aSource.couponId;
  fdescription := aSource.description;
  fseller := aSource.seller;
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
  if (aSource is TMateraGeneratedImage) then
    Assign(TMateraGeneratedImage(aSource));
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
  Clear;
end;

procedure TMateraGeneratedImage.Clear;
begin
  factualImageWidth := 0;
  fimageContent := EmptyStr;
  fmimeType := EmptyStr;
end;

function TMateraGeneratedImage.IsEmpty: Boolean;
begin
  Result :=
    EstaZerado(factualImageWidth) and
    EstaVazio(fimageContent) and
    EstaVazio(fmimeType);
end;

procedure TMateraGeneratedImage.Assign(aSource: TMateraGeneratedImage);
begin
  factualImageWidth := aSource.actualImageWidth;
  fimageContent := aSource.imageContent;
  fmimeType := aSource.mimeType;
end;

{ TMateraInstantPaymentQRCodeResponse }

procedure TMateraInstantPaymentQRCodeResponse.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraInstantPaymentQRCodeResponse) then
    Assign(TMateraInstantPaymentQRCodeResponse(aSource));
end;

procedure TMateraInstantPaymentQRCodeResponse.DoWriteToJSon(
  aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('textContent', ftextContent)
    .AddPair('reference', freference)
    .AddPair('qrcodeURL', fqrcodeURL)
    .AddPair('dynamicQrCodeType', fdynamicQrCodeType);
  fGeneratedImage.WriteToJSon(aJSon);
end;

procedure TMateraInstantPaymentQRCodeResponse.DoReadFromJSon(
  aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('textContent', ftextContent)
    .Value('reference', freference)
    .Value('qrcodeURL', fqrcodeURL)
    .Value('dynamicQrCodeType', fdynamicQrCodeType);
  fGeneratedImage.ReadFromJSon(aJSon);
end;

constructor TMateraInstantPaymentQRCodeResponse.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  fGeneratedImage := TMateraGeneratedImage.Create('GeneratedImage');
  Clear;
end;

destructor TMateraInstantPaymentQRCodeResponse.Destroy;
begin
  fGeneratedImage.Free;
  inherited Destroy;
end;

procedure TMateraInstantPaymentQRCodeResponse.Clear;
begin
  fqrcodeURL := EmptyStr;
  freference := EmptyStr;
  ftextContent := EmptyStr;
  fdynamicQrCodeType := EmptyStr;
  fGeneratedImage.Clear;
end;

function TMateraInstantPaymentQRCodeResponse.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(fdynamicQrCodeType) and
    EstaVazio(fqrcodeURL) and
    EstaVazio(freference) and
    EstaVazio(ftextContent) and
    (fGeneratedImage.IsEmpty);
end;

procedure TMateraInstantPaymentQRCodeResponse.Assign(
  aSource: TMateraInstantPaymentQRCodeResponse);
begin
  fqrcodeURL := aSource.qrcodeURL;
  freference := aSource.reference;
  ftextContent := aSource.textContent;
  fdynamicQrCodeType := aSource.dynamicQrCodeType;
  fGeneratedImage.Assign(aSource.generateImage);
end;

{ TMateraAuthorizationDetails }

procedure TMateraAuthorizationDetails.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraAuthorizationDetails) then
    Assign(TMateraAuthorizationDetails(aSource));
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
  Clear;
end;

procedure TMateraAuthorizationDetails.Clear;
begin
  fnumber := EmptyStr;
end;

function TMateraAuthorizationDetails.IsEmpty: Boolean;
begin
  Result := EstaVazio(fnumber);
end;

procedure TMateraAuthorizationDetails.Assign(aSource: TMateraAuthorizationDetails);
begin
  fnumber := aSource.number;
end;

{ TMateraFinancialStatement }

procedure TMateraFinancialStatement.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraFinancialStatement) then
    Assign(TMateraFinancialStatement(aSource));
end;

procedure TMateraFinancialStatement.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon.AddPair('status', MateraTransactionStatusToString(fstatus));
  fAuthorizationDetails.WriteToJSon(aJSon);
end;

procedure TMateraFinancialStatement.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  s: String;
begin
  {$IFDEF FPC}s := EmptyStr{$ENDIF};
  aJSon.Value('status', s);
  fstatus := StringToMateraTransactionStatus(s);
  fAuthorizationDetails.ReadFromJSon(aJSon);
end;

constructor TMateraFinancialStatement.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  fAuthorizationDetails := TMateraAuthorizationDetails.Create('AuthorizationDetails');
  Clear;
end;

destructor TMateraFinancialStatement.Destroy;
begin
  fAuthorizationDetails.Free;
  inherited Destroy;
end;

procedure TMateraFinancialStatement.Clear;
begin
  fAuthorizationDetails.Clear;
  fstatus := mtsNone;
end;

function TMateraFinancialStatement.IsEmpty: Boolean;
begin
  Result := (fAuthorizationDetails.IsEmpty) and (fstatus = mtsNone);
end;

procedure TMateraFinancialStatement.Assign(aSource: TMateraFinancialStatement);
begin
  fstatus := aSource.status;
  fAuthorizationDetails.Assign(aSource.AuthorizationDetails);
end;

{ TMateraQRCodeResponse }

procedure TMateraQRCodeResponse.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraQRCodeResponse) then
    Assign(TMateraQRCodeResponse(aSource));
end;

procedure TMateraQRCodeResponse.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('transactionId', ftransactionId)
    .AddPair('externalIdentifier', fexternalIdentifier)
    .AddPair('senderAccountId', fsenderAccountId)
    .AddPair('creditCardToken', fcreditCardToken)
    .AddPair('boletoUrl', fboletoUrl)
    .AddPair('typeableLine', ftypeableLine)
    .AddPair('transactionType', ftransactionType)
    .AddPair('totalAmount', ftotalAmount)
    .AddPair('paidAmount', fpaidAmount)
    .AddPair('discountAmount', fdiscountAmount)
    .AddPair('recipientDescription', frecipientDescription);
       
  if (fdueDate <> 0) then
    aJSon.AddPair('dueDate', FormatDateTime('yyyy-mm-dd', fdueDate));
  if (fexpirationDate <> 0) then
    aJSon.AddPair('expirationDate', FormatDateTime('yyyy-mm-dd', fexpirationDate));
  if (ftransactionDate <> 0) then
    aJSon.AddPair('transactionDate', FormatDateTime('yyyy-mm-dd', ftransactionDate));

  ffinancialStatement.WriteToJSon(aJSon);
  fcouponDetails.WriteToJSon(aJSon);
  finstantPayment.WriteToJSon(aJSon);
end;

procedure TMateraQRCodeResponse.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('transactionId', ftransactionId)
    .Value('externalIdentifier', fexternalIdentifier)
    .Value('senderAccountId', fsenderAccountId)
    .Value('creditCardToken', fcreditCardToken)
    .Value('boletoUrl', fboletoUrl)
    .Value('typeableLine', ftypeableLine)
    .Value('transactionType', ftransactionType)
    .Value('totalAmount', ftotalAmount)
    .Value('paidAmount', fpaidAmount)
    .Value('discountAmount', fdiscountAmount)
    .Value('recipientDescription', frecipientDescription)
    .ValueISODate('dueDate', fdueDate)
    .ValueISODate('transactionDate', ftransactionDate)
    .ValueISODate('expirationDate', fexpirationDate);

  fcouponDetails.ReadFromJSon(aJSon);
  finstantPayment.ReadFromJSon(aJSon);
  ffinancialStatement.ReadFromJSon(aJSon);
end;

constructor TMateraQRCodeResponse.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  ffinancialStatement := TMateraFinancialStatement.Create('financialStatement');
  fcouponDetails := TMateraCouponDetails.Create('couponDetails');
  finstantPayment := TMateraInstantPaymentQRCodeResponse.Create('instantPayment');
  Clear;
end;

destructor TMateraQRCodeResponse.Destroy;
begin
  ffinancialStatement.Free;
  fcouponDetails.Free;
  finstantPayment.Free;
  inherited Destroy;
end;

procedure TMateraQRCodeResponse.Clear;
begin
  fdueDate := 0;
  fpaidAmount := 0;
  ftotalAmount := 0;
  fdiscountAmount := 0;
  fexpirationDate := 0;
  ftransactionDate := 0;
  fboletoUrl := EmptyStr;
  ftypeableLine := EmptyStr;
  ftransactionId := EmptyStr;
  ftransactionType := EmptyStr;
  fcreditCardToken := EmptyStr;
  fsenderAccountId := EmptyStr;
  fexternalIdentifier := EmptyStr;
  frecipientDescription := EmptyStr;
  fcouponDetails.Clear;
  finstantPayment.Clear;
  ffinancialStatement.Clear;
end;

function TMateraQRCodeResponse.IsEmpty: Boolean;
begin
  Result :=
    EstaZerado(fdueDate) and
    EstaZerado(fpaidAmount) and
    EstaZerado(ftotalAmount) and
    EstaZerado(fdiscountAmount) and
    EstaZerado(fexpirationDate) and
    EstaZerado(ftransactionDate) and
    EstaVazio(fboletoUrl) and
    EstaVazio(ftypeableLine) and
    EstaVazio(ftransactionId) and
    EstaVazio(ftransactionType) and
    EstaVazio(fcreditCardToken) and
    EstaVazio(fsenderAccountId) and
    EstaVazio(fexternalIdentifier) and
    EstaVazio(frecipientDescription) and
    fcouponDetails.IsEmpty and
    finstantPayment.IsEmpty and
    ffinancialStatement.IsEmpty;
end;

procedure TMateraQRCodeResponse.Assign(aSource: TMateraQRCodeResponse);
begin
  fdueDate := aSource.dueDate;
  fpaidAmount := aSource.paidAmount;
  ftotalAmount := aSource.totalAmount;
  fdiscountAmount := aSource.discountAmount;
  fexpirationDate := aSource.expirationDate;
  ftransactionDate := aSource.transactionDate;
  fboletoUrl := aSource.boletoUrl;
  ftypeableLine := aSource.typeableLine;
  ftransactionId := aSource.transactionId;
  ftransactionType := aSource.transactionType;
  fcreditCardToken := aSource.creditCardToken;
  fsenderAccountId := aSource.senderAccountId;
  fexternalIdentifier := aSource.externalIdentifier;
  frecipientDescription := aSource.recipientDescription;
  fcouponDetails.Assign(aSource.couponDetails);
  finstantPayment.Assign(aSource.instantPayment);
  ffinancialStatement.Assign(aSource.financialStatement);
end;

{ TMateraAdditionalInformation }

procedure TMateraAdditionalInformation.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraAdditionalInformation) then
    Assign(TMateraAdditionalInformation(aSource));
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
  aJSon
    .Value('name',Fname)
    .Value('content',Fcontent)
    .Value('showToPayer',FshowToPlayer);
end;

constructor TMateraAdditionalInformation.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  Clear;
end;

procedure TMateraAdditionalInformation.Clear;
begin
  fname := EmptyStr;
  fcontent := EmptyStr;
  fshowToPlayer := False;
end;

function TMateraAdditionalInformation.IsEmpty: Boolean;
begin
  Result := EstaVazio(fcontent) and EstaVazio(fname);
end;

procedure TMateraAdditionalInformation.Assign(aSource: TMateraAdditionalInformation);
begin
  fname := aSource.name;
  fcontent := aSource.content;
  fshowToPlayer := aSource.showToPlayer;
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

procedure TMateraRecipientsArray.Insert(aIndex: Integer; aItem: TMateraRecipient);
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
  if (aSource is TMateraPaymentInfo) then
    Assign(TMateraPaymentInfo(aSource));
end;

procedure TMateraPaymentInfo.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon.AddPair('transactionType', ftransactionType);
  finstantPayment.WriteToJSon(aJSon);
end;

procedure TMateraPaymentInfo.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon.Value('transactionType', ftransactionType);
  finstantPayment.ReadFromJSon(aJSon);
end;

constructor TMateraPaymentInfo.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  finstantPayment := TMateraInstantPayment.Create('instantPayment');
  Clear;
end;

destructor TMateraPaymentInfo.Destroy;
begin
  finstantPayment.Free;
  inherited Destroy;
end;

procedure TMateraPaymentInfo.Clear;
begin
  finstantPayment.Clear;
  ftransactionType := EmptyStr;
end;

function TMateraPaymentInfo.IsEmpty: Boolean;
begin
  Result := (finstantPayment.IsEmpty) and EstaVazio(ftransactionType);
end;

procedure TMateraPaymentInfo.Assign(aSource: TMateraPaymentInfo);
begin
  finstantPayment.Assign(aSource.instantPayment);
  ftransactionType := aSource.transactionType;
end;

{ TMateraInstantPayment }

procedure TMateraInstantPayment.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraInstantPayment) then
    Assign(TMateraInstantPayment(aSource));
end;

procedure TMateraInstantPayment.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('dynamicQRCodeType', MateraDynamicQRCodeTypeToString(fdynamicQRCodeType), False)
    .AddPair('expiration', fexpiration, False)
    .AddPair('alias', falias);

  //falias_.WriteToJSon(aJSon);
  fbillingDueDate.WriteToJSon(aJSon);
  FadditionalInformation.WriteToJSon(aJSon);
  FqrCodeImageGenerationSpecification.WriteToJSon(aJSon);
end;

procedure TMateraInstantPayment.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  s: String;
begin
  {$IFDEF FPC}s := EmptyStr;{$ENDIF}
  aJSon
    .Value('expiration', fexpiration)
    .Value('dynamicQRCodeType', s)
    .Value('alias', falias);

  fdynamicQRCodeType := StringToMateraDynamicQRCodeType(s);
  //falias_.ReadFromJSon(aJSon);
  fbillingDueDate.ReadFromJSon(aJSon);
  FadditionalInformation.ReadFromJSon(aJSon);
  FqrCodeImageGenerationSpecification.ReadFromJSon(aJSon);
end;

constructor TMateraInstantPayment.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  //falias := TMateraAlias.Create('alias');
  fbillingDueDate := TMateraBillingDueDate.Create('billingDueDate');
  FadditionalInformation := TMateraAdditionalInformationArray.Create('additionalInformation');
  FqrCodeImageGenerationSpecification := TMateraQRCodeSpecification.Create('qrCodeImageGenerationSpecification');
  Clear;
end;

destructor TMateraInstantPayment.Destroy;
begin
  //falias.Free;
  fbillingDueDate.free;
  FadditionalInformation.Free;
  FqrCodeImageGenerationSpecification.Free;
  inherited Destroy;
end;

procedure TMateraInstantPayment.Clear;
begin
  fexpiration := 0;
  falias := EmptyStr;
  fdynamicQRCodeType := mqtNone;

  //falias.Clear;
  fbillingDueDate.Clear;
  fadditionalInformation.Clear;
  fqrCodeImageGenerationSpecification.Clear;
end;

function TMateraInstantPayment.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(falias) and
    EstaZerado(fexpiration) and
    (fdynamicQRCodeType = mqtNone) and
    //(falias_.IsEmpty) and
    (fbillingDueDate.IsEmpty) and
    (fadditionalInformation.IsEmpty) and
    (fqrCodeImageGenerationSpecification.IsEmpty);
end;

procedure TMateraInstantPayment.Assign(aSource: TMateraInstantPayment);
begin
  falias := aSource.alias_;
  fexpiration := aSource.expiration;
  fdynamicQRCodeType := aSource.dynamicQRCodeType;

  //falias_.Assign(aSource.alias_);
  fbillingDueDate.Assign(aSource.billingDueDate);
  fadditionalInformation.Assign(aSource.additionalInformation);
  fqrCodeImageGenerationSpecification.Assign(aSource.qrCodeImageGenerationSpecification);
end;

{ TMateraRecipient }

procedure TMateraRecipient.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraRecipient) then
    Assign(TMateraRecipient(aSource));
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
  Faccount.ReadFromJSon(aJSon);
  aJSon
    .Value('amount', Famount)
    .Value('currency',Fcurrency)
    .Value('mediatorFee', Fmediatorfee)
    .Value('recipientComment', FrecipientComment);
end;

constructor TMateraRecipient.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  Faccount := TMateraAccount.Create('account');
  Clear;
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
  Result:= 
    (Faccount.IsEmpty) and
    EstaZerado(Famount) and
    EstaZerado(Fmediatorfee) and
    EstaVazio(Fcurrency) and
    EstaVazio(FrecipientComment);
end;

procedure TMateraRecipient.Assign(aSource: TMateraRecipient);
begin
  Famount := aSource.amount;
  Fcurrency := aSource.currency;
  Fmediatorfee := aSource.mediatorfee;
  FrecipientComment := aSource.recipientComment;
  Faccount.Assign(aSource.account);
end;

{ TMateraQRCodeRequest }

procedure TMateraQRCodeRequest.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraQRCodeRequest) then
    Assign(TMateraQRCodeRequest(aSource));
end;

procedure TMateraQRCodeRequest.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('currency',fcurrency)
    .AddPair('totalAmount',ftotalAmount)
    .AddPair('callbackAddress',FcallbackAddress)
    .AddPair('externalIdentifier', fexternalIdentifier);

  fpaymentInfo.WriteToJSon(aJSon);
  frecipients.WriteToJSon(aJSon);
end;

procedure TMateraQRCodeRequest.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('currency',fcurrency)
    .Value('totalAmount',ftotalAmount)
    .Value('callbackAddress',FcallbackAddress)
    .Value('externalIdentifier', fexternalIdentifier);

  fpaymentInfo.ReadFromJSon(aJSon);
  frecipients.ReadFromJSon(aJSon);
end;

constructor TMateraQRCodeRequest.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  FpaymentInfo := TMateraPaymentInfo.Create('paymentInfo');
  Frecipients := TMateraRecipientsArray.Create('recipients');
  Clear;
end;

destructor TMateraQRCodeRequest.Destroy;
begin
  FpaymentInfo.Free;
  Frecipients.Free;
  inherited Destroy;
end;

procedure TMateraQRCodeRequest.Clear;
begin
  ftotalAmount:= 0;
  fcurrency:= EmptyStr;
  FcallbackAddress:= EmptyStr;
  fexternalIdentifier:= EmptyStr;

  FpaymentInfo.Clear;
  Frecipients.Clear;
end;

function TMateraQRCodeRequest.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(fcurrency) and
    EstaZerado(ftotalAmount) and
    EstaVazio(fcallbackAddress) and
    EstaVazio(fexternalIdentifier) and
    (fpaymentInfo.IsEmpty) and
    (frecipients.IsEmpty);
end;

procedure TMateraQRCodeRequest.Assign(aSource: TMateraQRCodeRequest);
begin
  fcurrency:= aSource.currency;
  ftotalAmount:= aSource.totalAmount;
  FcallbackAddress:= aSource.callbackAddress;
  fexternalIdentifier:= aSource.externalIdentifier;

  Frecipients.Clear;
  FpaymentInfo.Clear;
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
  {$IFDEF FPC}s := EmptyStr;{$ENDIF}

  inherited DoReadFromJSon(aJSon);
  aJSon.Value('status', s);
  fstatus := StringToMateraAliasStatus(s);
end;

procedure TMateraAlias.Clear;
begin
  inherited Clear;
  fstatus := mastNone;
end;

function TMateraAlias.IsEmpty: Boolean;
begin
  Result := (inherited IsEmpty) and (fstatus = mastNone);
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

destructor TMateraPayerInformation.Destroy;
begin
  faddressing.Free;
  inherited Destroy;
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
    EstaZerado(fimageWidth) and
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

destructor TMateraRegisterAliasResponse.Destroy;
begin
  falias.Free;
  inherited Destroy;
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
  {$IfDef FPC}s := EmptyStr;{$EndIf}

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
  ftype := malNone;
end;

function TMateraAliasBasic.IsEmpty: Boolean;
begin
  Result := EstaVazio(fname) and (ftype = malNone);
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

destructor TMateraAliasRequest.Destroy;
begin
  falias.Free;
  inherited Destroy;
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

constructor TMateraCreateAccountTransactionRequest.Create(const aObjectName: String);
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
  fcustomData := EmptyStr;
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

destructor TMateraAccountQueryResponse.Destroy;
begin
  faccounts.Free;
  accountHolder.Free;
  inherited Destroy;
end;

procedure TMateraAccountQueryResponse.Clear;
begin
  faccounts.Clear;
  faccountHolder.Clear;
end;

function TMateraAccountQueryResponse.IsEmpty: Boolean;
begin
  Result := faccounts.IsEmpty and faccountHolder.IsEmpty;
end;

procedure TMateraAccountQueryResponse.Assign(aSource: TMateraAccountQueryResponse);
begin
  faccounts.Assign(aSource.accounts);
  faccountHolder.Assign(aSource.accountHolder);
end;

{ TMateraAccountHolderBasic }

procedure TMateraAccountHolderBasic.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TMateraAccountHolderBasic) then
    Assign(TMateraAccountHolderBasic(aSource));
end;

procedure TMateraAccountHolderBasic.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('accountHolderId', faccountHolderId)
    .AddPair('email', femail)
    .AddPair('name', fname)
    .AddPair('representativeId', frepresentativeId)
    .AddPair('socialName', fsocialName)
    .AddPair('status', MateraActiveStatusToString(fstatus))
    .AddPair('callbackSecretKey', fcallbackSecretKey, False);

  fbillingAddress.WriteToJSon(aJSon);
  fmailAddress.WriteToJSon(aJSon);
  fmobilePhone.WriteToJSon(aJSon);
  fmobilePhones.WriteToJSon(aJSon);
  frepresentatives.WriteToJSon(aJSon);
  ftaxIdentifier.WriteToJSon(aJSon);
end;

procedure TMateraAccountHolderBasic.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  s: String;
begin 
  {$IfDef FPC}s := EmptyStr;{$EndIf}

  aJSon
    .Value('accountHolderId', faccountHolderId)
    .Value('email', femail)
    .Value('name', fname)
    .Value('representativeId', frepresentativeId)
    .Value('socialName', fsocialName)
    .Value('status', s)
    .Value('callbackSecretKey', fcallbackSecretKey);

  fstatus := StringToMateraActiveStatus(s);
  fbillingAddress.ReadFromJSon(aJSon);
  fmailAddress.ReadFromJSon(aJSon);
  fmobilePhone.ReadFromJSon(aJSon);
  fmobilePhones.ReadFromJSon(aJSon);
  frepresentatives.ReadFromJSon(aJSon);
  ftaxIdentifier.ReadFromJSon(aJSon);
end;

constructor TMateraAccountHolderBasic.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  fbillingAddress := TMateraEndereco.Create('billingAddress');
  fmailAddress := TMateraEndereco.Create('mailAddress');
  fmobilePhone := TMateraMobilePhone.Create('mobilePhone');
  fmobilePhones := TMateraMobilePhoneArray.Create('mobilePhones');
  frepresentatives := TMateraClientRepresentativeArray.Create('representatives');
  ftaxIdentifier := TMateraTaxIdentifierBasic.Create('taxIdentifier');
  Clear;
end;

destructor TMateraAccountHolderBasic.Destroy;
begin
  fbillingAddress.Free;
  fmailAddress.Free;
  fmobilePhone.Free;
  fmobilePhones.Free;
  frepresentatives.Free;
  ftaxIdentifier.Free;

  inherited Destroy;
end;

procedure TMateraAccountHolderBasic.Clear;
begin
  fname := EmptyStr;
  femail := EmptyStr;
  fsocialName := EmptyStr;
  faccountHolderId := EmptyStr;
  frepresentativeId := EmptyStr;
  fstatus := macNone;
  fcallbackSecretKey := EmptyStr;

  fmailAddress.Clear;
  fmobilePhone.Clear;
  fmobilePhones.Clear;
  ftaxIdentifier.Clear;
  fbillingAddress.Clear;
  frepresentatives.Clear;
end;

function TMateraAccountHolderBasic.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(fname) and
    EstaVazio(femail) and
    EstaVazio(fsocialName) and
    EstaVazio(faccountHolderId) and
    EstaVazio(frepresentativeId) and
    EstaVazio(fcallbackSecretKey) and
    (fstatus = macNone) and
    fbillingAddress.IsEmpty and
    fmailAddress.IsEmpty and
    fmobilePhone.IsEmpty and
    fmobilePhones.IsEmpty and
    frepresentatives.IsEmpty and
    ftaxIdentifier.IsEmpty;
end;

procedure TMateraAccountHolderBasic.Assign(aSource: TMateraAccountHolderBasic);
begin
  fname := aSource.name;
  femail := aSource.email;
  fsocialName := aSource.socialName;
  faccountHolderId := aSource.accountHolderId;
  frepresentativeId := aSource.representativeId;
  fstatus := aSource.status;
  fcallbackSecretKey := aSource.callbackSecretKey;

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
  {$IFDEF FPC}s := EmptyStr;{$ENDIF}

  inherited DoReadFromJSon(aJSon);
  aJSon.Value('accountType', s);
  faccountType := StringToMateraAccountType(s);
end;

procedure TMateraAccountIdentifier.Clear;
begin
  inherited Clear;
  faccountType := matNone;
end;

function TMateraAccountIdentifier.IsEmpty: Boolean;
begin
  Result := inherited IsEmpty and (faccountType = matNone);
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
  s1, s2: String;
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
  fclientType := mctNone;
  faccountStatus := mcsNone;

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
    (faccountStatus = mcsNone) and
    (fclientType = mctNone) and
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
  faccountStatus := aSource.accountStatus;
  fclientType := aSource.clientType;

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
  s1, s2: String;
begin
  {$IFDEF FPC}
  s1 := EmptyStr;
  s2 := EmptyStr;
  {$ENDIF}

  aJSon
    .Value('accountInternalTypeId', faccountInternalTypeId)
    .Value('accountType', s1)
    .Value('clientType', s2)
    .Value('externalIdentifier', fexternalIdentifier)
    .Value('ibkPwdHash', fibkPwdHash)
    .Value('sharedAccount', fsharedAccount);

  faccountType := StringToMateraAccountType(s1);
  fclientType := StringToMateraClientType(s2);
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
  faccountType := matNone;
  fclientType := mctNone;

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
    (faccountInternalTypeId = 0) and
    EstaVazio(fexternalIdentifier) and
    (faccountType = matNone) and
    (fclientType = mctNone) and
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
  fibkPwdHash := aSource.ibkPwdHash;
  fsharedAccount := aSource.sharedAccount;
  fexternalIdentifier := aSource.externalIdentifier;
  faccountInternalTypeId := aSource.accountInternalTypeId;
  fclientType := aSource.clientType;
  faccountType := aSource.accountType;

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
    .AddPair('companyName', fcompanyName)
    .AddPair('businessLine', fbusinessLine)
    .AddPair('establishmentForm', festablishmentForm)
    .AddPair('financialStatistic', ffinancialStatistic, False)
    .AddPair('monthlyIncome', fmonthlyIncome, False)
    .AddPair('stateRegistration', fstateRegistration, False)
    .AddPair('establishmentDate', FormatDateTime('yyyy-mm-dd', festablishmentDate), False);

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

procedure TMateraClientRepresentative.Assign(aSource: TMateraClientRepresentative);
begin
  inherited Assign(aSource);
  faccountHolderId := aSource.accountHolderId;
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
    .AddPair('birthState', fbirthState)
    .AddPair('businessLine', fbusinessLine)
    .AddPair('documentType', fdocumentType)
    .AddPair('father', ffather)
    .AddPair('financialStatistic', ffinancialStatistic)
    .AddPair('gender', fgender)
    .AddPair('maritalStatus', fmaritalStatus)
    .AddPair('mother', fmother)
    .AddPair('occupation', foccupation)
    .AddPair('partner', fpartner)
    .AddPair('birthDate', FormatDateTime('yyyy-mm-dd', fbirthDate), False)
    .AddPair('monthlyIncome', fmonthlyIncome, False);

  frg.WriteToJSon(aJSon);
  flegalResponsible.WriteToJSon(aJSon);
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
    .Value('partner', fpartner)
    .Value('monthlyIncome', fmonthlyIncome);

  frg.ReadFromJSon(aJSon);
  flegalResponsible.ReadFromJSon(aJSon);
  fotherDocument.ReadFromJSon(aJSon);
end;

constructor TMateraAdditionalDetailsBasic.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  frg := TMateraRG.Create('rg');
  fotherDocument := TMateraOtherDoc.Create('otherDocument');
  flegalResponsible := TMateraClient.Create('legalResponsible');
  Clear;
end;

destructor TMateraAdditionalDetailsBasic.Destroy;
begin
  frg.Free;
  fotherDocument.Free;
  flegalResponsible.Free;
  inherited Destroy;
end;

procedure TMateraAdditionalDetailsBasic.Clear;
begin                 
  fbirthDate := 0;
  foccupation := 0;  
  fbusinessLine := 0;
  ffinancialStatistic := 0;
  fmonthlyIncome := 0;
  fbirthCity := EmptyStr;
  fbirthCountry := EmptyStr;
  fbirthState := EmptyStr;
  fdocumentType := EmptyStr;
  ffather := EmptyStr;
  fgender := EmptyStr;
  fmaritalStatus := EmptyStr;
  fmother := EmptyStr;
  fpartner := EmptyStr;
  frg.Clear;
  fotherDocument.Clear;
  flegalResponsible.Clear;
end;

function TMateraAdditionalDetailsBasic.IsEmpty: Boolean;
begin
  Result :=
    (fbirthDate = 0) and
    (foccupation = 0) and
    (fbusinessLine = 0) and
    (ffinancialStatistic = 0) and
    (fmonthlyIncome = 0) and
    EstaVazio(fbirthCity) and
    EstaVazio(fbirthCountry) and
    EstaVazio(fbirthState) and
    EstaVazio(fdocumentType) and
    EstaVazio(ffather) and
    EstaVazio(fgender) and
    EstaVazio(fmaritalStatus) and
    EstaVazio(fmother) and
    EstaVazio(fpartner) and
    frg.IsEmpty and
    fotherDocument.IsEmpty and
    flegalResponsible.IsEmpty;
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
  fmonthlyIncome := aSource.monthlyIncome;
  frg.Assign(aSource.rg);
  fotherDocument.Assign(aSource.otherDocument);
  flegalResponsible.Assign(aSource.legalResponsible);
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
    .AddPair('issuer', fissuer)
    .AddPair('number', fnumber)
    .AddPair('type', ftype);

  if (issueDate > 0) then
    aJSon.AddPair('issueDate', FormatDateTime('yyyy-mm-dd', fissueDate));
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
    .AddPair('issuer', fissuer)
    .AddPair('number', fnumber)
    .AddPair('state', fstate);

  if (issueDate > 0) then
    aJSon.AddPair('issueDate', FormatDateTime('yyyy-mm-dd', fissueDate));
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
  ftype := mdtNone;
end;

function TMateraDocument.IsEmpty: Boolean;
begin
  Result := EstaVazio(fcontent) and (ftype = mdtNone);
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

{ TMateraTaxIdentifierBasic }

procedure TMateraTaxIdentifierBasic.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (ASource is TMateraTaxIdentifierBasic) then
    Assign(TMateraTaxIdentifierBasic(ASource));
end;

procedure TMateraTaxIdentifierBasic.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('taxId', ftaxId)
    .AddPair('country', fcountry)
    .AddPair('taxIdMasked', ftaxIdMasked, False);
end;

procedure TMateraTaxIdentifierBasic.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  AJSon
    .Value('country', fcountry)
    .Value('taxId', ftaxId)
    .Value('taxIdMasked', ftaxIdMasked);
end;

constructor TMateraTaxIdentifierBasic.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  Clear;
end;

procedure TMateraTaxIdentifierBasic.Clear;
begin
  fcountry := EmptyStr;
  ftaxId := EmptyStr;
  ftaxIdMasked := EmptyStr;
end;

function TMateraTaxIdentifierBasic.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(fcountry) and
    EstaVazio(ftaxId) and
    EstaVazio(ftaxIdMasked);
end;

procedure TMateraTaxIdentifierBasic.Assign(aSource: TMateraTaxIdentifierBasic);
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

  fdocuments.WriteToJSon(aJSon);
  ftaxIdentifier.WriteToJSon(aJSon);
  fmobilePhone.WriteToJSon(aJSon);
  fmailAddress.WriteToJSon(aJSon);
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

