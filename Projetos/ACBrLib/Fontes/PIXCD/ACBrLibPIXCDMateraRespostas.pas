{ criar uma interface de comunicação com equipamentos de automacao comercial.   }
{                                                                               }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida                }
{                                                                               }
{ Colaboradores nesse arquivo: Antonio Carlos Junior                            }
{                                                                               }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr     }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr       }
{                                                                               }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la  }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela   }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério)  }
{ qualquer versão posterior.                                                    }
{                                                                               }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM    }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU       }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor }
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)               }
{                                                                               }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto }
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,   }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.           }
{ Você também pode obter uma copia da licença em:                               }
{ http://www.opensource.org/licenses/gpl-license.php                            }
{                                                                               }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br }
{        Rua Cel.Aureliano de Camargo, 963 - Tatuí - SP - 18270-170             }
{                                                                               }
{*******************************************************************************}

{$I ACBr.inc}

unit ACBrLibPIXCDMateraRespostas;

interface

uses
  Classes, SysUtils, ACBrBase, ACBrLibPIXCDConsts, ACBrLibResposta, ACBrPIXCD, ACBrPIXBase, ACBrSchemasMatera, ACBrUtil.Strings, ACBrPIXPSPMatera;

type
  { TLibPIXCDMateraGeneratedImage }
  TLibPIXCDMateraGeneratedImage = class(TACBrLibRespostaBase)
    private
      factualImageWidth: Integer;
      fimageContent: String;
      fmimeType: String;

    public
      procedure Clear;
      procedure Processar(const MateraGeneratedImage: TMateraGeneratedImage);

    published
      property actualImageWidth: Integer read factualImageWidth write factualImageWidth;
      property imageContent: String read fimageContent write fimageContent;
      property mimeType: String read fmimeType write fmimeType;
  end;

  { TLibPIXCDMateraInstantPaymentQRCodeResponse }
  TLibPIXCDMateraInstantPaymentQRCodeResponse = class(TACBrLibRespostaBase)
    private
      fqrcodeURL: String;
      freference: String;
      ftextContent: String;
      fdynamicQrCodeType: String;
      fGeneratedImage: TLibPIXCDMateraGeneratedImage;

    public
      constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
      destructor Destroy; override;

      procedure Clear;
      procedure Processar(const MateraInstantPaymentQRCodeResponse: TMateraInstantPaymentQRCodeResponse);

    published
      property qrcodeURL: String read fqrcodeURL write fqrcodeURL;
      property reference: String read freference write freference;
      property textContent: String read ftextContent write ftextContent;
      property dynamicQrCodeType: String read fdynamicQrCodeType write fdynamicQrCodeType;
      property GeneratedImage: TLibPIXCDMateraGeneratedImage read fGeneratedImage write fGeneratedImage;
  end;

  { TLibPIXCDMateraAuthorizationDetails }
  TLibPIXCDMateraAuthorizationDetails = class(TACBrLibRespostaBase)
    private
      fnumber: String;

    public
      procedure Clear;
      procedure Processar(const MateraAuthorizationDetails: TMateraAuthorizationDetails);

    published
      property number: String read fnumber write fnumber;
  end;

  { TLibPIXCDMateraFinancialStatement }
  TLibPIXCDMateraFinancialStatement = class(TACBrLibRespostaBase)
    private
      fAuthorizationDetails: TLibPIXCDMateraAuthorizationDetails;
      fstatus: TMateraTransactionStatus;

    public
      constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
      destructor Destroy; override;

      procedure Clear;
      procedure Processar(const MateraFinancialStatement: TMateraFinancialStatement);

    published
      property AuthorizationDetails: TLibPIXCDMateraAuthorizationDetails read fAuthorizationDetails write fAuthorizationDetails;
      property status: TMateraTransactionStatus read fstatus write fstatus;
  end;

  { TLibPIXCDMateraCouponDetails }
  TLibPIXCDMateraCouponDetails = class(TACBrLibRespostaBase)
    private
      fcouponId: String;
      fdescription: String;
      fseller: String;

    public
      procedure Clear;
      procedure Processar(const MateraCouponDetails: TMateraCouponDetails);

    published
      property couponId: String read fcouponId write fcouponId;
      property description: String read fdescription write fdescription;
      property seller: String read fseller write fseller;
  end;

  { TLibPIXCDQRCodeResposta }
  TLibPIXCDQRCodeResposta = class(TACBrLibRespostaBase)
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
      fcouponDetails: TLibPIXCDMateraCouponDetails;
      ffinancialStatement: TLibPIXCDMateraFinancialStatement;
      finstantPayment: TLibPIXCDMateraInstantPaymentQRCodeResponse;

    public
      constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
      destructor Destroy; override;

      procedure Clear;
      procedure Processar(const QRCodeResposta: TMateraQRCodeResponse);

    published
      property boletoUrl: String read fboletoUrl write fboletoUrl;
      property dueDate: TDateTime read fdueDate write fdueDate;
      property creditCardToken: String read fcreditCardToken write fcreditCardToken;
      property discountAmount: Currency read fdiscountAmount write fdiscountAmount;
      property expirationDate: TDateTime read fexpirationDate write fexpirationDate;
      property externalIdentifier: String read fexternalIdentifier write fexternalIdentifier;
      property paidAmount: Currency read fpaidAmount write fpaidAmount;
      property recipientDescription: String read frecipientDescription write frecipientDescription;
      property senderAccountId: String read fsenderAccountId write fsenderAccountId;
      property totalAmount: Currency read ftotalAmount write ftotalAmount;
      property transactionDate: TDateTime read ftransactionDate write ftransactionDate;
      property transactionId: String read ftransactionId write ftransactionId;
      property transactionType: String read ftransactionType write ftransactionType;
      property typeableLine: String read ftypeableLine write ftypeableLine;
      property couponDetails: TLibPIXCDMateraCouponDetails read fcouponDetails write fcouponDetails;
      property financialStatement: TLibPIXCDMateraFinancialStatement read ffinancialStatement write ffinancialStatement;
      property instantPayment: TLibPIXCDMateraInstantPaymentQRCodeResponse read finstantPayment write finstantPayment;
  end;

  { TLibPIXCDSolicitarRetiradaResposta }
  TLibPIXCDSolicitarRetiradaResposta = class(TACBrLibRespostaBase)
    private
      ftransactionId: String;
      fexternalIdentifier: String;
      fstatus: TMateraTransactionStatus;
      freceipt: String;
      fauthenticationCode: String;

    public
      constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
      destructor Destroy; override;

      procedure Clear;
      procedure Processar(const SolicitarRetiradaResposta: TMateraRetiradaResponse);

    published
      property transactionId: String read ftransactionId write ftransactionId;
      property externalIdentifier: String read fexternalIdentifier write fexternalIdentifier;
      property status: TMateraTransactionStatus read fstatus write fstatus;
      property receipt: String read freceipt write freceipt;
      property authenticationCode: String read fauthenticationCode write fauthenticationCode;
  end;

  { TLibPIXCDSolicitarDevolucaoResposta }
  TLibPIXCDSolicitarDevolucaoResposta = class(TACBrLibRespostaBase)
    private
      ftransactionId: String;

    public
      constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
      destructor Destroy; override;

      procedure Clear;
      procedure Processar(const SolicitarDevolucaoResposta: TMateraDevolucaoResponse);

    published
      property transactionId: String read ftransactionId write ftransactionId;
  end;

  { TLibPIXCDMateraValues }
  TLibPIXCDMateraValues = class(TACBrLibRespostaBase)
    private
      fd90: integer;
      fm12: integer;
      fm60: integer;

    public
      procedure Clear;
      procedure Processar(const MateraValues: TMateraValues);

    published
      property d90: integer read fd90 write fd90;
      property m12: integer read fm12 write fm12;
      property m60: integer read fm60 write fm60;
  end;

  { TLibPIXCDMateraspi }
  TLibPIXCDMateraspi = class (TACBrLibRespostaBase)
    private
      fsettlements: TLibPIXCDMateraValues;
      fwatermark: string;

    public
      constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
      destructor Destroy; override;

      procedure Clear;
      procedure Processar(const Materaspi: TMateraspi);

    published
      property settlements: TLibPIXCDMateraValues read fsettlements write fsettlements;
      property watermark: string read fwatermark write fwatermark;
  end;

  { TLibPIXCDMaterainfractionReports }
  TLibPIXCDMaterainfractionReports = class (TACBrLibRespostaBase)
    private
      fopenReports: integer;
      fopenReportsDistinctReporters: integer;
      frejectedReports: TLibPIXCDMateraValues;
      fwatermark: string;

    public
      constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
      destructor Destroy; override;

      procedure Clear;
      procedure Processar(const MaterainfractionReports: TMaterainfractionReports);

    published
      property openReports: integer read fopenReports write fopenReports;
      property openReportsDistinctReporters: integer read fopenReportsDistinctReporters write fopenReportsDistinctReporters;
      property rejectedReports: TLibPIXCDMateraValues read frejectedReports write frejectedReports;
      property watermark: string read fwatermark write fwatermark;
  end;

  { TLibPIXCDMateraFraudMarkers }
  TLibPIXCDMateraFraudMarkers = class (TACBrLibRespostaBase)
    private
      fapplicationFrauds: TLibPIXCDMateraValues;
      fdistinctFraudReporters: TLibPIXCDMateraValues;
      fmuleAccounts: TLibPIXCDMateraValues;
      fotherFrauds: TLibPIXCDMateraValues;
      fscammerAccounts: TLibPIXCDMateraValues;
      ftotalFrauds: TLibPIXCDMateraValues;
      ftotalFraudTransactionAmount: TLibPIXCDMateraValues;
      fwatermark: string;

    public
      constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
      destructor Destroy; override;

      procedure Clear;
      procedure Processar(const MateraFraudMarkers: TMateraFraudMarkers);

    published
      property applicationFrauds: TLibPIXCDMateraValues read fapplicationFrauds write fapplicationFrauds;
      property distinctFraudReporters: TLibPIXCDMateraValues read fdistinctFraudReporters write fdistinctFraudReporters;
      property muleAccounts: TLibPIXCDMateraValues read fmuleAccounts write fmuleAccounts;
      property otherFrauds: TLibPIXCDMateraValues read fotherFrauds write fotherFrauds;
      property scammerAccounts: TLibPIXCDMateraValues read fscammerAccounts write fscammerAccounts;
      property totalFrauds: TLibPIXCDMateraValues read ftotalFrauds write ftotalFrauds;
      property totalFraudTransactionAmount: TLibPIXCDMateraValues read ftotalFraudTransactionAmount write ftotalFraudTransactionAmount;
      property watermark: string read fwatermark write fwatermark;
  end;

  { TLibPIXCDMateraentries }
  TLibPIXCDMateraentries = class(TACBrLibRespostaBase)
    private
      fregisteredAccounts: integer;
      fwatermark: string;

    public
      procedure Clear;
      procedure Processar(const Materaentries: TMateraentries);

    published
      property registeredAccounts: integer read fregisteredAccounts write fregisteredAccounts;
      property watermark: string read fwatermark write fwatermark;
  end;

  { TLibPIXCDMaterapersonStatistics }
  TLibPIXCDMaterapersonStatistics = class(TACBrLibRespostaBase)
    private
      fentries: TLibPIXCDMateraentries;
      ffraudMarkers: TLibPIXCDMateraFraudMarkers;
      finfractionReports: TLibPIXCDMaterainfractionReports;
      fspi: TLibPIXCDMateraspi;

    public
      constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
      destructor Destroy; override;

      procedure Clear;
      procedure Processar(const MaterapersonStatistics: TMaterapersonStatistics);

    published
      property entries: TLibPIXCDMateraentries read fentries write fentries;
      property fraudMarkers: TLibPIXCDMateraFraudMarkers read ffraudMarkers write ffraudMarkers;
      property infractionReports: TLibPIXCDMaterainfractionReports read finfractionReports write finfractionReports;
      property spi: TLibPIXCDMateraspi read fspi write fspi;
  end;

  { TLibPIXCDMateraaliasStatistics }
  TLibPIXCDMateraaliasStatistics = class(TACBrLibRespostaBase)
    private
      fentries: TLibPIXCDMateraentries;
      ffraudMarkers: TLibPIXCDMateraFraudMarkers;
      finfractionReports: TLibPIXCDMaterainfractionReports;
      fspi: TLibPIXCDMateraspi;

    public
      constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
      destructor Destroy; override;

      procedure Clear;
      procedure Processar(const MateraaliasStatistics: TMateraaliasStatistics);

    published
      property entries: TLibPIXCDMateraentries read fentries write fentries;
      property fraudMarkers: TLibPIXCDMateraFraudMarkers read ffraudMarkers write ffraudMarkers;
      property infractionReports: TLibPIXCDMaterainfractionReports read finfractionReports write finfractionReports;
      property spi: TLibPIXCDMateraspi read fspi write fspi;
  end;

  { TLibPIXCDMateraaliasHolderAddress }
  TLibPIXCDMateraaliasHolderAddress = class(TACBrLibRespostaBase)
    private
      fcity: String;
      fstreet: String;
      fuf: String;
      fzipCode: String;

    public
      procedure Clear;
      procedure Processar(const MateraaliasHolderAddress: TMateraaliasHolderAddress);

    published
      property city: String read fcity write fcity;
      property street: String read fstreet write fstreet;
      property uf: String read fuf write fuf;
      property zipCode: String read fzipCode write fzipCode;
  end;

  { TLibPIXCDMateraPSP }
  TLibPIXCDMateraPSP = class(TACBrLibRespostaBase)
    private
      fcountry: String;
      fcurrencies: String;
      fid: String;
      fname: String;

    public
      procedure Clear;
      procedure Processar(const MateraPSP: TMateraPSP);

    published
      property country_MateraPSP: String read fcountry write fcountry;
      property currencies: String read fcurrencies write fcurrencies;
      property id: String read fid write fid;
      property name: String read fname write fname;
  end;

  { TLibPIXCDMateraCounter }
  TLibPIXCDMateraCounter = class(TACBrLibRespostaBase)
    private
      fby: string;
      fd3: integer;
      fd30: integer;
      fm6: integer;
      ftype_: TMateraAntifraudCounter;

    public
      constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
      destructor Destroy; override;

      procedure Clear;
      procedure Processar(const MateraCounter: TMateraCounter);

    published
      property by: string read fby write fby;
      property d3: integer read fd3 write fd3;
      property d30: integer read fd30 write fd30;
      property m6: integer read fm6 write fm6;
      property type_: TMateraAntifraudCounter read ftype_ write ftype_;
  end;

  { TLibPIXCDMateraAntiFraudClearingInfo }
  TLibPIXCDMateraAntiFraudClearingInfo = class(TACBrLibRespostaBase)
    private
      fcounters: TACBrObjectList;
      flastUpdated: TDateTime;

    public
      constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
      destructor Destroy; override;

      procedure Clear;
      procedure Processar(const MateraAntiFraudClearingInfo: TMateraAntiFraudClearingInfo);

    published
      property counters: TACBrObjectList read fcounters write fcounters;
      property lastUpdated: TDateTime read flastUpdated write flastUpdated;
  end;

  { TLibPIXCDMateraDestinationAccount }
  TLibPIXCDMateraDestinationAccount = class(TACBrLibRespostaBase)
    private
      faccount: String;
      faccountType: TMateraAccountTypeDestination;
      fbranch: String;

    public
      procedure Clear;
      procedure Processar(const MateraDestinationAccount: TMateraDestinationAccount);

    published
      property account: String read faccount write faccount;
      property accountType: TMateraAccountTypeDestination read faccountType write faccountType;
      property branch: String read fbranch write fbranch;
  end;

  { TLibPIXCDMateraTaxIdentifier }
  TLibPIXCDMateraTaxIdentifier = class(TACBrLibRespostaBase)
    private
      fcountry: String;
      ftaxId: String;
      ftaxIdMasked: String;

    public
      procedure Clear;
      procedure Processar(const MateraTaxIdentifier: TMateraTaxIdentifier);

    published
      property country_TaxIdentifier: String read fcountry write fcountry;
      property taxId: String read ftaxId write ftaxId;
      property taxIdMasked: String read ftaxIdMasked write ftaxIdMasked;
  end;

  { TLibPIXCDMateraAliasAccountHolder }
  TLibPIXCDMateraAliasAccountHolder = class(TACBrLibRespostaBase)
    private
      fname: string;
      ftaxIdentifier: TLibPIXCDMateraTaxIdentifier;

    public
      constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
      destructor Destroy; override;

      procedure Clear;
      procedure Processar(const MateraAliasAccountHolder: TMateraAliasAccountHolder);

    published
      property name: string read fname write fname;
      property taxIdentifier: TLibPIXCDMateraTaxIdentifier read ftaxIdentifier write ftaxIdentifier;
  end;

  { TLibPIXCDAliasRetiradaResposta }
  TLibPIXCDAliasRetiradaResposta = class(TACBrLibRespostaBase)
    private
      faccountDestination: TLibPIXCDMateraDestinationAccount;
      faliasAccountHolder: TLibPIXCDMateraAliasAccountHolder;
      faliasType: TMateraAliasType;
      falias_: string;
      fantiFraudClearingInfo: TLibPIXCDMateraAntiFraudClearingInfo;
      fcreationDate: TDateTime;
      fendtoEndId: string;
      fpsp: TLibPIXCDMateraPSP;
      //MateraAliasResponseV2
      faliasHolderAddress: TLibPIXCDMateraaliasHolderAddress;
      faliasStatistics: TLibPIXCDMateraaliasStatistics;
      fpersonStatistics: TLibPIXCDMaterapersonStatistics;
      fstatus: TMateraAliasStatus;

    public
      constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
      destructor Destroy; override;

      procedure Clear;
      procedure Processar(const AliasRetiradaResposta: TMateraAliasResponse);
      procedure ProcessarAliasResponseV2(const MateraAliasResponseV2: TMateraAliasResponseV2);

    published
      property accountDestination: TLibPIXCDMateraDestinationAccount read faccountDestination write faccountDestination;
      property aliasAccountHolder: TLibPIXCDMateraAliasAccountHolder read faliasAccountHolder write faliasAccountHolder;
      property aliasType: TMateraAliasType read faliasType write faliasType;
      property alias_: string read falias_ write falias_;
      property antiFraudClearingInfo: TLibPIXCDMateraAntiFraudClearingInfo read fantiFraudClearingInfo write fantiFraudClearingInfo;
      property creationDate: TDateTime read fcreationDate write fcreationDate;
      property endtoEndId: string read fendtoEndId write fendtoEndId;
      property psp: TLibPIXCDMateraPSP read fpsp write fpsp;
      //MateraAliasResponseV2
      property aliasHolderAddress: TLibPIXCDMateraaliasHolderAddress read faliasHolderAddress write faliasHolderAddress;
      property aliasStatistics: TLibPIXCDMateraaliasStatistics read faliasStatistics write faliasStatistics;
      property personStatistics: TLibPIXCDMaterapersonStatistics read fpersonStatistics write fpersonStatistics;
      property status: TMateraAliasStatus read fstatus write fstatus;
  end;

  { TLibPIXCDMateraReturnCode }
  TLibPIXCDMateraReturnCode = class(TACBrLibRespostaBase)
    private
      fcode: String;
      fdescription: String;

    public
      procedure Clear;
      procedure Processar(const MateraReturnCode: TMateraReturnCode);

    published
      property code: String read fcode write fcode;
      property description: String read fdescription write fdescription;
  end;

  { TLibPIXCDMotivosDevolucaoResposta }
  TLibPIXCDMotivosDevolucaoResposta = class(TACBrLibRespostaBase)
    private
      freturnCodes: TACBrObjectList;

    public
      constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
      destructor Destroy; override;

      procedure Clear;
      procedure Processar(const MotivosDevolucaoResposta: TMateraReturnCodeArray);

    published
      property returnCodes: TACBrObjectList read freturnCodes write freturnCodes;
  end;

  { TLibPIXCDMateraStatementInstantPaymentCashValueData }
  TLibPIXCDMateraStatementInstantPaymentCashValueData = class(TACBrLibRespostaBase)
    private
      fcashValueType: TMateraStatementIPCashValueType;
      fvalue: currency;

    public
      constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
      destructor Destroy; override;

      procedure Clear;
      procedure Processar(const MateraStatementInstantPaymentCashValueData: TMateraStatementInstantPaymentCashValueData);

    published
      property cashValueType: TMateraStatementIPCashValueType read fcashValueType write fcashValueType;
      property value: currency read fvalue write fvalue;
  end;

  { TLibPIXCDMateraStatementInstantPaymentCashValue }
  TLibPIXCDMateraStatementInstantPaymentCashValue = class(TACBrLibRespostaBase)
    private
      fvalues: TACBrObjectList;

    public
      constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
      destructor Destroy; override;

      procedure Clear;
      procedure Processar(const StatementInstantPaymentCashValueDataArray: TStatementInstantPaymentCashValueDataArray);

    published
      property values: TACBrObjectList read fvalues write fvalues;
  end;

  { TLibPIXCDMateraCounterpart }
  TLibPIXCDMateraCounterpart = class(TACBrLibRespostaBase)
    private
      faccountDestination: String;
      fbankDestination: String;
      fbranchDestination: String;
      fclientType: TMateraClientType;
      fname: String;
      fTaxIdentifier: TLibPIXCDMateraTaxIdentifier;

    public
      constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
      destructor Destroy; override;

      procedure Clear;
      procedure Processar(const MateraCounterpart: TMateraCounterpart);

    published
      property accountDestination: String read faccountDestination write faccountDestination;
      property bankDestination: String read fbankDestination write fbankDestination;
      property branchDestination: String read fbranchDestination write fbranchDestination;
      property clientType: TMateraClientType read fclientType write fclientType;
      property name: String read fname write fname;
      property TaxIdentifier: TLibPIXCDMateraTaxIdentifier read fTaxIdentifier write fTaxIdentifier;
  end;

  { TLibPIXCDMaterastatementEntry }
  TLibPIXCDMaterastatementEntry = class(TACBrLibRespostaBase)
    private
      fadditionalInfo: String;
      famount: Currency;
      fcomment: String;
      fcounterpart: TLibPIXCDMateraCounterpart;
      fcreditDate: TDateTime;
      fdescription: String;
      fentryDate: TDateTime;
      fhistoryCode: Currency;
      finstantPaymentCashValue: TLibPIXCDMateraStatementInstantPaymentCashValue;
      ftransactionId: String;
      ftransactionType: String;
      ftype_: TMaterastatementEntryType;

    public
      constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
      destructor Destroy; override;

      procedure Clear;
      procedure Processar(const MaterastatementEntry: TMaterastatementEntry);

    published
      property additionalInfo: String read fadditionalInfo write fadditionalInfo;
      property amount: Currency read famount write famount;
      property comment: String read fcomment write fcomment;
      property counterpart: TLibPIXCDMateraCounterpart read fcounterpart write fcounterpart;
      property creditDate: TDateTime read fcreditDate write fcreditDate;
      property description: String read fdescription write fdescription;
      property entryDate: TDateTime read fentryDate write fentryDate;
      property historyCode: Currency read fhistoryCode write fhistoryCode;
      property instantPaymentCashValue: TLibPIXCDMateraStatementInstantPaymentCashValue read finstantPaymentCashValue write finstantPaymentCashValue;
      property transactionId: String read ftransactionId write ftransactionId;
      property transactionType: String read ftransactionType write ftransactionType;
      property type_: TMaterastatementEntryType read ftype_ write ftype_;
  end;

  { TLibPIXCDExtratoECResposta }
  TLibPIXCDExtratoECResposta = class(TACBrLibRespostaBase)
    private
      fstatement: TACBrObjectList;

    public
      constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
      destructor Destroy; override;

      procedure Clear;
      procedure Processar(const ExtratoECResposta: TMaterastatementResponse);

    published
      property statement: TACBrObjectList read fstatement write fstatement;
  end;

  { TLibPIXCDSaldoECResposta }
  TLibPIXCDSaldoECResposta = class(TACBrLibRespostaBase)
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

    public
      constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
      destructor Destroy; override;

      procedure Clear;
      procedure Processar(const SaldoECResposta: TMateraBalanceResponse);

    published
      property accountId: String read faccountId write faccountId;
      property autoInvest: Currency read fautoInvest write fautoInvest;
      property available: Currency read favailable write favailable;
      property availableBalanceForTransactions: Currency read favailableBalanceForTransactions write favailableBalanceForTransactions;
      property blocked: Currency read fblocked write fblocked;
      property date: TDateTime read fdate write fdate;
      property emergencyAidBalance: Currency read femergencyAidBalance write femergencyAidBalance;
      property future: Currency read ffuture write ffuture;
      property overdraft: Currency read foverdraft write foverdraft;
      property real: Currency read freal write freal;
  end;

  { TLibPIXCDMateraParticipantInstantPayment }
  TLibPIXCDMateraParticipantInstantPayment = class(TACBrLibRespostaBase)
    private
      faccount: TLibPIXCDMateraDestinationAccount;
      falias: String;
      fname: String;
      fpsp: TLibPIXCDMateraPSP;
      ftaxIdentifier: TLibPIXCDMateraTaxIdentifier;

    public
      constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
      destructor Destroy; override;

      procedure Clear;
      procedure Processar(const MateraParticipantInstantPayment: TMateraParticipantInstantPayment);

    published
      property account: TLibPIXCDMateraDestinationAccount read faccount write faccount;
      property alias_: String read falias write falias;
      property name: String read fname write fname;
      property psp: TLibPIXCDMateraPSP read fpsp write fpsp;
      property taxIdentifier: TLibPIXCDMateraTaxIdentifier read ftaxIdentifier write ftaxIdentifier;
  end;

  { TLibPIXCDMateraPaymentReceived }
  TLibPIXCDMateraPaymentReceived = class(TACBrLibRespostaBase)
    private
      fadditionalInformation: String;
      fendToEndId: String;
      flegacyTransactionId: String;
      freceivedAmount: Currency;
      fsender: TLibPIXCDMateraParticipantInstantPayment;
      ftransactionTimestamp: String;

    public
      constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
      destructor Destroy; override;

      procedure Clear;
      procedure Processar(const MateraPaymentReceived: TMateraPaymentReceived);

    published
      property additionalInformation: String read fadditionalInformation write fadditionalInformation;
      property endToEndId: String read fendToEndId write fendToEndId;
      property legacyTransactionId: String read flegacyTransactionId write flegacyTransactionId;
      property receivedAmount: Currency read freceivedAmount write freceivedAmount;
      property sender: TLibPIXCDMateraParticipantInstantPayment read fsender write fsender;
      property transactionTimestamp: String read ftransactionTimestamp write ftransactionTimestamp;
  end;

  { TLibPIXCDMateraOriginInstantPaymentTransactionResponse }
  TLibPIXCDMateraOriginInstantPaymentTransactionResponse = class(TACBrLibRespostaBase)
    private
      fendToEndId: String;
      fsender: TLibPIXCDMateraParticipantInstantPayment;

    public
      constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
      destructor Destroy; override;

      procedure Clear;
      procedure Processar(const MateraOriginInstantPaymentTransactionResponse: TMateraOriginInstantPaymentTransactionResponse);

    published
      property endToEndId: String read fendToEndId write fendToEndId;
      property sender: TLibPIXCDMateraParticipantInstantPayment read fsender write fsender;
  end;

  { TLibPIXCDMateraOriginDepositTransactionResponse }
  TLibPIXCDMateraOriginDepositTransactionResponse = class(TACBrLibRespostaBase)
    private
      fadditionalInformation: String;
      ftotalAmount: Double;
      ftransactionDate: String;
      ftransactionId: String;
      foriginInstantPaymentTransaction: TLibPIXCDMateraOriginInstantPaymentTransactionResponse;

    public
      constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
      destructor Destroy; override;

      procedure Clear;
      procedure Processar(const MateraOriginDepositTransactionResponse: TMateraOriginDepositTransactionResponse);

    published
      property additionalInformation: String read fadditionalInformation write fadditionalInformation;
      property totalAmount: Double read ftotalAmount write ftotalAmount;
      property transactionDate: String read ftransactionDate write ftransactionDate;
      property transactionId: String read ftransactionId write ftransactionId;
      property originInstantPaymentTransaction: TLibPIXCDMateraOriginInstantPaymentTransactionResponse read foriginInstantPaymentTransaction write foriginInstantPaymentTransaction;
  end;

  { TLibPIXCDMateraInstantPaymentTransactionReturnReasonInformation }
  TLibPIXCDMateraInstantPaymentTransactionReturnReasonInformation = class(TACBrLibRespostaBase)
    private
      fadditionalInformation: String;
      freasonCode: String;
      freasonDescription: String;

    public
      procedure Clear;
      procedure Processar(const MateraInstantPaymentTransactionReturnReasonInformation: TMateraInstantPaymentTransactionReturnReasonInformation);

    published
      property additionalInformation: String read fadditionalInformation write fadditionalInformation;
      property reasonCode: String read freasonCode write freasonCode;
      property reasonDescription: String read freasonDescription write freasonDescription;
  end;

  { TLibPIXCDMateraInstantPaymentTransactionReturnInfo }
  TLibPIXCDMateraInstantPaymentTransactionReturnInfo = class(TACBrLibRespostaBase)
    private
      foriginalEndToEndId: String;
      foriginalInstantPaymentId: String;
      freturnReasonInformation: TLibPIXCDMateraInstantPaymentTransactionReturnReasonInformation;

    public
      constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
      destructor Destroy; override;

      procedure Clear;
      procedure Processar(const MateraInstantPaymentTransactionReturnInfo: TMateraInstantPaymentTransactionReturnInfo);

    published
      property originalEndToEndId: String read foriginalEndToEndId write foriginalEndToEndId;
      property originalInstantPaymentId: String read foriginalInstantPaymentId write foriginalInstantPaymentId;
      property returnReasonInformation: TLibPIXCDMateraInstantPaymentTransactionReturnReasonInformation read freturnReasonInformation write freturnReasonInformation;
  end;

  { TLibPIXCDMateraRejectionReasonInstantPayment }
  TLibPIXCDMateraRejectionReasonInstantPayment = class(TACBrLibRespostaBase)
    private
      fcode: String;
      fdescription: String;

    public
      procedure Clear;
      procedure Processar(const MateraRejectionReasonInstantPayment: TMateraRejectionReasonInstantPayment);

    published
      property code: String read fcode write fcode;
      property description: String read fcode write fcode;
  end;

  { TLibPIXCDMateraWithdrawProviders }
  TLibPIXCDMateraWithdrawProviders = class(TACBrLibRespostaBase)
    private
      fagentModality: TMateraWithdrawAgentType;
      fserviceProvider: String;

    public
      procedure Clear;
      procedure Processar(const MateraWithdrawProviders: TMateraWithdrawProviders);

    published
      property agentModality: TMateraWithdrawAgentType read fagentModality write fagentModality;
      property serviceProvider: String read fserviceProvider write fserviceProvider;
  end;

  { TLibPIXCDMateraCashValue }
  TLibPIXCDMateraCashValue = class(TACBrLibRespostaBase)
    private
      fallowValueChange: Boolean;
      fcashValueType: TMateraCashValueType;
      fvalue: String;
      fwithdrawProviders: TLibPIXCDMateraWithdrawProviders;

    public
      constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
      destructor Destroy; override;

      procedure Clear;
      procedure Processar(const MateraCashValue: TMateraCashValue);

    published
      property allowValueChange: Boolean read fallowValueChange write fallowValueChange;
      property cashValueType: TMateraCashValueType read fcashValueType write fcashValueType;
      property value: String read fvalue write fvalue;
      property withdrawProviders: TLibPIXCDMateraWithdrawProviders read fwithdrawProviders write fwithdrawProviders;
  end;

  { TLibPIXCDMateraInstantPaymentTransactionResponse }
  TLibPIXCDMateraInstantPaymentTransactionResponse = class(TACBrLibRespostaBase)
    private
      fendToEndId: String;
      fadditionalInformation: String;
      fsender: TLibPIXCDMateraParticipantInstantPayment;
      finstantPaymentCashValue: TLibPIXCDMateraCashValue;
      fpaymentReceived: TACBrObjectList;
      frecipient: TLibPIXCDMateraParticipantInstantPayment;
      frejectionReason: TLibPIXCDMateraRejectionReasonInstantPayment;
      freturnInfo: TLibPIXCDMateraInstantPaymentTransactionReturnInfo;
      foriginDepositTransaction: TLibPIXCDMateraOriginDepositTransactionResponse;

    public
      constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
      destructor Destroy; override;

      procedure Clear;
      procedure Processar(const MateraInstantPaymentTransactionResponse: TMateraInstantPaymentTransactionResponse);

    published
      property endToEndId: String read fendToEndId write fendToEndId;
      property additionalInformation: String read fadditionalInformation write fadditionalInformation;
      property sender: TLibPIXCDMateraParticipantInstantPayment read fsender write fsender;
      property instantPaymentCashValue: TLibPIXCDMateraCashValue read finstantPaymentCashValue write finstantPaymentCashValue;
      property paymentReceived: TACBrObjectList read fpaymentReceived write fpaymentReceived;
      property recipient: TLibPIXCDMateraParticipantInstantPayment read frecipient write frecipient;
      property rejectionReason: TLibPIXCDMateraRejectionReasonInstantPayment read frejectionReason write frejectionReason;
      property returnInfo: TLibPIXCDMateraInstantPaymentTransactionReturnInfo read freturnInfo write freturnInfo;
      property originDepositTransaction: TLibPIXCDMateraOriginDepositTransactionResponse read foriginDepositTransaction write foriginDepositTransaction;
  end;

  { TLibPIXCDMateraUtilitiesPayment }
  TLibPIXCDMateraUtilitiesPayment = class(TACBrLibRespostaBase)
    private
      fdocumentNumber: String;
      fbarcode: String;
      fbeneficiaryTaxIdentifier: String;
      ftypeableLine: String;
      fdueDate: TDateTime;
      fpaidAmount: Currency;

    public
      procedure Clear;
      procedure Processar(const MateraUtilitiesPayment: TMateraUtilitiesPayment);

    published
      property documentNumber: String read fdocumentNumber write fdocumentNumber;
      property barcode: String read fbarcode write fbarcode;
      property beneficiaryTaxIdentifier: String read fbeneficiaryTaxIdentifier write fbeneficiaryTaxIdentifier;
      property typeableLine: String read ftypeableLine write ftypeableLine;
      property dueDate: TDateTime read fdueDate write fdueDate;
      property paidAmount: Currency read fpaidAmount write fpaidAmount;
  end;

  { TLibPIXCDMateraMobilePhone }
  TLibPIXCDMateraMobilePhone = class(TACBrLibRespostaBase)
    private
      fcountry: String;
      fphoneNumber: String;

    public
      procedure Clear;
      procedure Processar(const MateraMobilePhone: TMateraMobilePhone);

    published
      property country: String read fcountry write fcountry;
      property phoneNumber: String read fphoneNumber write fphoneNumber;
  end;

  { TLibPIXCDMateraDrawee }
  TLibPIXCDMateraDrawee = class(TACBrLibRespostaBase)
    private
      fname: String;
      ftaxIdentifier: TLibPIXCDMateraTaxIdentifier;

    public
      constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
      destructor Destroy; override;

      procedure Clear;
      procedure Processar(const MateraDrawee: TMateraDrawee);

    published
      property name: String read fname write fname;
      property taxIdentifier: TLibPIXCDMateraTaxIdentifier read ftaxIdentifier write ftaxIdentifier;
  end;

  { TLibPIXCDMateraCoupon }
  TLibPIXCDMateraCoupon = class(TACBrLibRespostaBase)
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

    public
      constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
      destructor Destroy; override;

      procedure Clear;
      procedure Processar(const MateraCoupon: TMateraCoupon);

    published
      property accountHolderId: String read faccountHolderId write faccountHolderId;
      property accountId: String read faccountId write faccountId;
      property couponId: String read fcouponId write fcouponId;
      property couponMerchantId: String read fcouponMerchantId write fcouponMerchantId;
      property description: String read fdescription write fdescription;
      property discount: Double read fdiscount write fdiscount;
      property dueDate: TDateTime read fdueDate write fdueDate;
      property maxUse: Integer read fmaxUse write fmaxUse;
      property minExpenseValue: Double read fminExpenseValue write fminExpenseValue;
      property seller: String read fseller write fseller;
      property status: TMateraActiveStatus read fstatus write fstatus;
      property useCount: Integer read fuseCount write fuseCount;
  end;

  { TLibPIXCDMateraEndereco }
  TLibPIXCDMateraEndereco = class(TACBrLibRespostaBase)
    private
      fbairro: String;
      fcep: String;
      fcidade: String;
      fcomplemento: String;
      festado: String;
      flogradouro: String;
      fnumero: String;
      fpais: String;

    public
      procedure Clear;
      procedure Processar(const MateraEndereco: TMateraEndereco);

    published
      property bairro: String read fbairro write fbairro;
      property cep: String read fcep write fcep;
      property cidade: String read fcidade write fcidade;
      property complemento: String read fcomplemento write fcomplemento;
      property estado: String read festado write festado;
      property logradouro: String read flogradouro write flogradouro;
      property numero: String read fnumero write fnumero;
      property pais: String read fpais write fpais;
  end;

  { TLibPIXCDMateraAccountHolderResponse }
  TLibPIXCDMateraAccountHolderResponse = class(TACBrLibRespostaBase)
    private
      fbillingAddress: TLibPIXCDMateraEndereco;

    public
      constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
      destructor Destroy; override;

      procedure Clear;
      procedure Processar(const MateraAccountHolderResponse: TMateraAccountHolderResponse);

    published
      property billingAddress: TLibPIXCDMateraEndereco read fbillingAddress write fbillingAddress;
  end;

  { TLibPIXCDMateraCancelPaymentTransactionResponse }
  TLibPIXCDMateraCancelPaymentTransactionResponse = class(TACBrLibRespostaBase)
    private
      fexternalProtocolId: String;
      freason: String;
      fsourceSystem: String;

    public
      constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
      destructor Destroy; override;

      procedure Clear;
      procedure Processar(const MateraCancelPaymentTransactionResponse: TMateraCancelPaymentTransactionResponse);

    published
      property externalProtocolId: String read fexternalProtocolId write fexternalProtocolId;
      property reason: String read freason write freason;
      property sourceSystem: String read fsourceSystem write fsourceSystem;
  end;

  { TLibPIXCDMateraBoletoPayment }
  TLibPIXCDMateraBoletoPayment = class(TACBrLibRespostaBase)
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

    public
      constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
      destructor Destroy; override;

      procedure Clear;
      procedure Processar(const MateraBoletoPayment: TMateraBoletoPayment);

    published
      property authenticationCode: String read fauthenticationCode write fauthenticationCode;
      property bankAuthentication: String read fbankAuthentication write fbankAuthentication;
      property barcode: String read fbarcode write fbarcode;
      property beneficiaryTaxIdentifier: String read fbeneficiaryTaxIdentifier write fbeneficiaryTaxIdentifier;
      property discount: Currency read fdiscount write fdiscount;
      property documentNumber: String read fdocumentNumber write fdocumentNumber;
      property dueDate: TDateTime read fdueDate write fdueDate;
      property fineAmount: Currency read ffineAmount write ffineAmount;
      property historyCode: String read fhistoryCode write fhistoryCode;
      property interestAmount: Currency read finterestAmount write finterestAmount;
      property paidAmount: Currency read fpaidAmount write fpaidAmount;
      property status: TMateraTransactionStatus read fstatus write fstatus;
      property typeableLine: String read ftypeableLine write ftypeableLine;
  end;

  { TLibPIXCDMateraBankTransfer }
  TLibPIXCDMateraBankTransfer = class(TACBrLibRespostaBase)
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
      ftaxIdentifier: TLibPIXCDMateraTaxIdentifier;
      ftransferMethod: String;

    public
      constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
      destructor Destroy; override;

      procedure Clear;
      procedure Processar(const MateraBankTransfer: TMateraBankTransfer);

    published
      property accountDestination: String read faccountDestination write faccountDestination;
      property accountDigitDestination: String read faccountDigitDestination write faccountDigitDestination;
      property accountTypeDestination: TMateraAccountTypeDestination read faccountTypeDestination write faccountTypeDestination;
      property bankDestination: String read fbankDestination write fbankDestination;
      property branchDestination: String read fbranchDestination write fbranchDestination;
      property historyCode: String read fhistoryCode write fhistoryCode;
      property name: String read fname write fname;
      property personType: String read fpersonType write fpersonType;
      property purposeCode: String read fpurposeCode write fpurposeCode;
      property taxIdentifier: TLibPIXCDMateraTaxIdentifier read ftaxIdentifier write ftaxIdentifier;
      property transferMethod: String read ftransferMethod write ftransferMethod;
  end;

  { TLibPIXCDTransactionResponse }
  TLibPIXCDTransactionResponse = class(TACBrLibRespostaBase)
    private
      faccountId: String;
      faccountHolderId: String;
      fbankTransfer: TLibPIXCDMateraBankTransfer;
      fboleto: TLibPIXCDMateraBoletoPayment;
      fcancelPaymentTransaction: TLibPIXCDMateraCancelPaymentTransactionResponse;
      fcancelTransactionId: String;
      fcounterPart: TLibPIXCDMateraAccountHolderResponse;
      fcoupon: TLibPIXCDMateraCoupon;
      fcurrentAmount: Double;
      fdiscountAmount: Double;
      fdocUrl: String;
      fdrawee: TLibPIXCDMateraDrawee;
      femail: String;
      fendToEndId: String;
      ffirstInstallmentValue: Double;
      finstallmentQuantity: Double;
      finstantPayment: TLibPIXCDMateraInstantPaymentTransactionResponse;
      fmobilePhone: TLibPIXCDMateraMobilePhone;
      fotherInstallmentValues: Double;
      fpaidAmount: Double;
      frecipientDescription: String;
      fsideAccountId: String;
      ftotalAmount: Currency;
      ftransactionDate: TDateTime;
      ftransactionId: String;
      ftransactionStatus: TMateraTransactionStatus;
      ftransactionType: String;
      futilities: TLibPIXCDMateraUtilitiesPayment;

    public
      constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
      destructor Destroy; override;

      procedure Clear;
      procedure Processar(const TransactionResponse: TMateraTransactionResponse);

    published
      property accountId: String read faccountId write faccountId;
      property accountHolderId: String read faccountHolderId write faccountHolderId;
      property bankTransfer: TLibPIXCDMateraBankTransfer read fbankTransfer write fbankTransfer;
      property boleto: TLibPIXCDMateraBoletoPayment read fboleto write fboleto;
      property cancelPaymentTransaction: TLibPIXCDMateraCancelPaymentTransactionResponse read fcancelPaymentTransaction write fcancelPaymentTransaction;
      property cancelTransactionId: String read fcancelTransactionId write fcancelTransactionId;
      property counterPart: TLibPIXCDMateraAccountHolderResponse read fcounterPart write fcounterPart;
      property coupon: TLibPIXCDMateraCoupon read fcoupon write fcoupon;
      property currentAmount: Double read fcurrentAmount write fcurrentAmount;
      property discountAmount: Double read fdiscountAmount write fdiscountAmount;
      property docUrl: String read fdocUrl write fdocUrl;
      property drawee: TLibPIXCDMateraDrawee read fdrawee write fdrawee;
      property email: String read femail write femail;
      property endToEndId: String read fendToEndId write fendToEndId;
      property firstInstallmentValue: Double read ffirstInstallmentValue write ffirstInstallmentValue;
      property installmentQuantity: Double read finstallmentQuantity write finstallmentQuantity;
      property instantPayment: TLibPIXCDMateraInstantPaymentTransactionResponse read finstantPayment write finstantPayment;
      property mobilePhone: TLibPIXCDMateraMobilePhone read fmobilePhone write fmobilePhone;
      property otherInstallmentValues: Double read fotherInstallmentValues write fotherInstallmentValues;
      property paidAmount: Double read fpaidAmount write fpaidAmount;
      property recipientDescription: String read frecipientDescription write frecipientDescription;
      property sideAccountId: String read fsideAccountId write fsideAccountId;
      property totalAmount: Currency read ftotalAmount write ftotalAmount;
      property transactionDate: TDateTime read ftransactionDate write ftransactionDate;
      property transactionId: String read ftransactionId write ftransactionId;
      property transactionStatus: TMateraTransactionStatus read ftransactionStatus write ftransactionStatus;
      property transactionType: String read ftransactionType write ftransactionType;
      property utilities: TLibPIXCDMateraUtilitiesPayment read futilities write futilities;
  end;

  { TLibPIXCDConsultarTransacaoResposta }
  TLibPIXCDConsultarTransacaoResposta = class(TACBrLibRespostaBase)
    private
      fhashNextPage: String;
      fItems: TACBrObjectList;

    public
      constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
      destructor Destroy; override;

      procedure Clear;
      procedure Processar(const TransacoesResposta: TMateraTransactionResponseArray);

    published
      property hashNextPage: String read fhashNextPage write fhashNextPage;
      property Items: TACBrObjectList read fItems write fItems;
  end;

  { TLibPIXCDMateraAlias }
  TLibPIXCDMateraAlias = class(TACBrLibRespostaBase)
    private
      fAlias_name: String;
      fAlias_type: TMateraAliasType;
      fAlias_status: TMateraAliasStatus;

    public
      procedure Clear;
      procedure Processar(const MateraAlias: TMateraAlias);

    published
      property Alias_name: String read fAlias_name write fAlias_name;
      property Alias_type: TMateraAliasType read fAlias_type write fAlias_type;
      property Alias_status: TMateraAliasStatus read fAlias_status write fAlias_status;
  end;

  { TLibPIXCDExcluirChavePIXResposta }
  TLibPIXCDExcluirChavePIXResposta = class(TACBrLibRespostaBase)
    private
      fResultCode: Integer;
      fResultString: String;

    public
      constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
      destructor Destroy; override;

      procedure Clear;
      procedure Processar(const APSP: TACBrPSPMatera);

    published
      property ResultCode: Integer read fResultCode write fResultCode;
      property ResultString: String read fResultString write fResultString;
  end;

  { TLibPIXCDChavePIXResposta }
  TLibPIXCDChavePIXResposta = class (TACBrLibRespostaBase)
    private
      fneedsIdentifyConfirmation: Boolean;
      fprotocolId: String;
      faliases: TACBrObjectList;

    public
      constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
      destructor Destroy; override;

      procedure Clear;
      procedure Processar(const ChavesPIXResposta: TMateraAliasArray); overload;
      procedure Processar(const ChavePIXResposta: TMateraRegisterAliasResponse); overload;

    published
      property needsIdentifyConfirmation: Boolean read fneedsIdentifyConfirmation write fneedsIdentifyConfirmation;
      property protocolId: String read fprotocolId write fprotocolId;
      property aliases: TACBrObjectList read faliases write faliases;
  end;

  { TLibPIXCDProblemaRespostaMatera }
  TLibPIXCDProblemaRespostaMatera = class(TACBrLibRespostaBase)
    private
      fCode: String;
      fDescription: String;
      fField: String;

    public
      constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
      destructor Destroy; override;

      procedure Clear;
      procedure Processar(const Matera: TMateraError);

    published
      property Code: String read fCode write fCode;
      property Description: String read fDescription write fDescription;
      property Field: String read fField write fField;
  end;

  { TLibPIXCDMateraAccount }
  TLibPIXCDMateraAccount = class(TACBrLibRespostaBase)
    private
      faccount: Integer;
      faccountID: String;
      fbranch: Integer;
      fmobilePhone: TLibPIXCDMateraMobilePhone;

    public
      constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
      destructor Destroy; override;

      procedure Clear;
      procedure Processar(const MateraAccount: TMateraAccount);

    published
      property account: Integer read faccount write faccount;
      property accountID: String read faccountID write faccountID;
      property branch: Integer read fbranch write fbranch;
      property mobilePhone: TLibPIXCDMateraMobilePhone read fmobilePhone write fmobilePhone;
  end;

  {TLibPIXCDInativarContaReposta}
  TLibPIXCDInativarContaReposta = class(TACBrLibRespostaBase)
    private
      fResultCode: Integer;
      fResultString: String;

    public
      constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
      destructor Destroy; override;

      procedure Clear;
      procedure Processar(const APSP: TACBrPSPMatera);

    published
      property ResultCode: Integer read fResultCode write fResultCode;
      property ResultString: String read fResultString write fResultString;
  end;

  { TLibPIXCDMateraAdditionalDetailsCorporate }
  TLibPIXCDMateraAdditionalDetailsCorporate = class(TACBrLibRespostaBase)
    private
      fbusinessLine: Integer;
      fcompanyName: String;
      festablishmentDate: TDateTime;
      festablishmentForm: String;
      ffinancialStatistic: Double;
      fmonthlyIncome: Double;
      frepresentatives: TACBrObjectList;

    public
      constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
      destructor Destroy; override;

      procedure Clear;
      procedure Processar(const MateraAdditionalDetailsCorporate: TMateraAdditionalDetailsCorporate);

    published
      property businessLine: Integer read fbusinessLine write fbusinessLine;
      property companyName: String read fcompanyName write fcompanyName;
      property establishmentDate: TDateTime read festablishmentDate write festablishmentDate;
      property establishmentForm: String read festablishmentForm write festablishmentForm;
      property financialStatistic: Double read ffinancialStatistic write ffinancialStatistic;
      property monthlyIncome: Double read fmonthlyIncome write fmonthlyIncome;
      property representatives: TACBrObjectList read frepresentatives write frepresentatives;
  end;

  { TLibPIXCDMateraDocument }
  TLibPIXCDMateraDocument = class(TACBrLibRespostaBase)
    private
      fcontent: String;
      ftype_: TMateraDocumentType;

    public
      procedure Clear;
      procedure Processar(const MateraDocument: TMateraDocument);

    published
      property content: String read fcontent write fcontent;
      property type_: TMateraDocumentType read ftype_ write ftype_;
  end;

  { TLibPIXCDMateraBasicClient }
  TLibPIXCDMateraBasicClient = class(TACBrLibRespostaBase)
    private
      femail: String;
      fname: String;
      fsocialName: String;
      fbirthDate: TDateTime;
      fmother: String;
      fmailAddress: TLibPIXCDMateraEndereco;
      fmobilePhone: TLibPIXCDMateraMobilePhone;
      ftaxIdentifier: TLibPIXCDMateraTaxIdentifier;
      fdocuments: TACBrObjectList;

    public
      constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
      destructor Destroy; override;

      procedure Clear;
      procedure Processar(const MateraClient: TMateraClient);

    published
      property email: String read femail write femail;
      property name: String read fname write fname;
      property socialName: String read fsocialName write fsocialName;
      property birthDate: TDateTime read fbirthDate write fbirthDate;
      property mother: String read fmother write fmother;
      property mailAddress: TLibPIXCDMateraEndereco read fmailAddress write fmailAddress;
      property mobilePhone: TLibPIXCDMateraMobilePhone read fmobilePhone write fmobilePhone;
      property taxIdentifier: TLibPIXCDMateraTaxIdentifier read ftaxIdentifier write ftaxIdentifier;
      property documents: TACBrObjectList read fdocuments write fdocuments;
  end;

  { TLibPIXCDMateraClientRepresentative }
  TLibPIXCDMateraClientRepresentative = class(TACBrLibRespostaBase)
    private
      faccountHolderId: String;
      //BasicClient
      femail: String;
      fname: String;
      fsocialName: String;
      //Documents
      fcontent: TACBrObjectList;
      //MailAddress
      fbairro: String;
      fcep: String;
      fcidade: String;
      fcomplemento: String;
      festado: String;
      flogradouro: String;
      fnumero: String;
      fpais: String;
      //MobilePhone
      fcountry_mobilePhone: String;
      fphoneNumber: String;
      //TaxIdentifier
      fcountry_taxIdentifier: String;
      ftaxId: String;
      ftaxIdMasked: String;

    public
      constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
      destructor Destroy; override;

      procedure Clear;
      procedure Processar(const MateraClientRepresentative: TMateraClientRepresentative);

    published
      property accountHolderId: String read faccountHolderId write faccountHolderId;
      //BasicClient
      property email: String read femail write femail;
      property name: String read fname write fname;
      property socialName: String read fsocialName write fsocialName;
      //Documents
      property content: TACBrObjectList read fcontent write fcontent;
      //MailAddress
      property bairro: String read fbairro write fbairro;
      property cep: String read fcep write fcep;
      property cidade: String read fcidade write fcidade;
      property complemento: String read fcomplemento write fcomplemento;
      property estado: String read festado write festado;
      property logradouro: String read flogradouro write flogradouro;
      property numero: String read fnumero write fnumero;
      property pais: String read fpais write fpais;
      //MobilePhone
      property country_mobilePhone: String read fcountry_mobilePhone write fcountry_mobilePhone;
      property phoneNumber: String read fphoneNumber write fphoneNumber;
      //TaxIdentifier
      property country_taxIdentifier: String read fcountry_taxIdentifier write fcountry_taxIdentifier;
      property taxId: String read ftaxId write ftaxId;
      property taxIdMasked: String read ftaxIdMasked write ftaxIdMasked;
  end;

  { TLibPIXCDConsultarContaReposta }
  TLibPIXCDConsultarContaReposta = class(TACBrLibRespostaBase)
    private
      faccountHolderId: String;
      faccountInternalTypeId: Integer;
      fmediatorId: String;
      faccount: TLibPIXCDMateraAccount;
      fclientType: TMateraClientType;
      fbillingAddress: TLibPIXCDMateraEndereco;
      faccountStatus: TMateraAccountStatus;
      fadditionalDetailsCorporate: TLibPIXCDMateraAdditionalDetailsCorporate;
      fclient: TLibPIXCDMateraBasicClient;

    public
      constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
      destructor Destroy; override;

      procedure Clear;
      procedure Processar(const ConsultarContaResposta: TMateraAccountResponse);

    published
      property accountHolderId: String read faccountHolderId write faccountHolderId;
      property accountInternalTypeId: Integer read faccountInternalTypeId write faccountInternalTypeId;
      property mediatorId: String read fmediatorId write fmediatorId;
      property account: TLibPIXCDMateraAccount read faccount write faccount;
      property clientType: TMateraClientType read fclientType write fclientType;
      property billingAddress: TLibPIXCDMateraEndereco read fbillingAddress write fbillingAddress;
      property accountStatus: TMateraAccountStatus read faccountStatus write faccountStatus;
      property additionalDetailsCorporate: TLibPIXCDMateraAdditionalDetailsCorporate read fadditionalDetailsCorporate write fadditionalDetailsCorporate;
      property client: TLibPIXCDMateraBasicClient read fclient write fclient;
  end;

  { TLibPIXCDIncluirContaResposta }
  TLibPIXCDIncluirContaResposta = class(TACBrLibRespostaBase)
    private
      faccountHolderId: String;
      faccountStatus: TMateraAccountStatus;
      faccount: TLibPIXCDMateraAccount;

    public
      constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
      destructor Destroy; override;

      procedure Clear;
      procedure Processar(const IncluirContaResposta: TMateraAccountResponse);

    published
      property accountHolderId: String read faccountHolderId write faccountHolderId;
      property accountStatus: TMateraAccountStatus read faccountStatus write faccountStatus;
      property account: TLibPIXCDMateraAccount read faccount write faccount;
  end;

implementation

{ TLibPIXCDProblemaRespostaMatera }
constructor TLibPIXCDProblemaRespostaMatera.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespProblemaMatera, ATipo, AFormato);
  Clear;
end;

destructor TLibPIXCDProblemaRespostaMatera.Destroy;
begin
  inherited Destroy;
end;

procedure TLibPIXCDProblemaRespostaMatera.Clear;
begin
  fCode := EmptyStr;
  fDescription := EmptyStr;
  fField := EmptyStr;
end;

procedure TLibPIXCDProblemaRespostaMatera.Processar(const Matera: TMateraError);
begin
  Code := Matera.code;
  Description := Matera.description;
  Field := Matera.field;
end;

{ TLibPIXCDInativarContaReposta }
constructor TLibPIXCDInativarContaReposta.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespInativarContaMatera, ATipo, AFormato);
end;

destructor TLibPIXCDInativarContaReposta.Destroy;
begin
  inherited Destroy;
end;

procedure TLibPIXCDInativarContaReposta.Clear;
begin
  fResultCode := 0;
  fResultString := EmptyStr;
end;

procedure TLibPIXCDInativarContaReposta.Processar(const APSP: TACBrPSPMatera);
begin
  ResultCode := APSP.Http.ResultCode;
  ResultString := APSP.Http.ResultString;
end;

{ TLibPIXCDConsultarContaReposta }
constructor TLibPIXCDConsultarContaReposta.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespConsultarContaMatera, ATipo, AFormato);

  faccount := TLibPIXCDMateraAccount.Create(CSessapRespMateraAccount, ATipo, AFormato);
  fbillingAddress := TLibPIXCDMateraEndereco.Create(CSessaoRespBillingAddressMatera, ATipo, AFormato);
  fadditionalDetailsCorporate := TLibPIXCDMateraAdditionalDetailsCorporate.Create(CSessaoRespAdditionalDetailsCorporateMatera, ATipo, AFormato);
  fclient := TLibPIXCDMateraBasicClient.Create(CSessaoRespMateraClient, ATipo, AFormato);
  Clear;
end;

destructor TLibPIXCDConsultarContaReposta.Destroy;
begin
  faccount.Free;
  fbillingAddress.Free;
  fadditionalDetailsCorporate.Free;
  fclient.Free;
  inherited Destroy;
end;

procedure TLibPIXCDConsultarContaReposta.Clear;
begin
  faccountHolderId := EmptyStr;
  faccountInternalTypeId := 0;
  fmediatorId := EmptyStr;
  faccount.Clear;
  fclientType := mctNone;
  fbillingAddress.Clear;
  faccountStatus := mcsNone;
  fadditionalDetailsCorporate.Clear;
  fclient.Clear;
end;

procedure TLibPIXCDConsultarContaReposta.Processar(const ConsultarContaResposta: TMateraAccountResponse);
begin
  accountHolderId := ConsultarContaResposta.accountHolderId;
  accountInternalTypeId := ConsultarContaResposta.accountInternalTypeId;
  mediatorId := ConsultarContaResposta.mediatorId;
  account.Processar(ConsultarContaResposta.account);
  clientType := ConsultarContaResposta.clientType;
  billingAddress.Processar(ConsultarContaResposta.billingAddress);
  accountStatus := ConsultarContaResposta.accountStatus;
  additionalDetailsCorporate.Processar(ConsultarContaResposta.additionalDetailsCorporate);
  client.Processar(ConsultarContaResposta.client);
end;

{ TLibPIXCDIncluirContaResposta }
constructor TLibPIXCDIncluirContaResposta.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespIncluirContaMatera, ATipo, AFormato);
  faccount := TLibPIXCDMateraAccount.Create(CSessaoRespAccountMatera, ATipo, AFormato);
  Clear;
end;

destructor TLibPIXCDIncluirContaResposta.Destroy;
begin
  inherited Destroy;
end;

procedure TLibPIXCDIncluirContaResposta.Clear;
begin
  faccountHolderId := EmptyStr;
  faccountStatus := mcsNone;
  faccount.Clear;
end;

procedure TLibPIXCDIncluirContaResposta.Processar(const IncluirContaResposta: TMateraAccountResponse);
begin
  accountHolderId := IncluirContaResposta.accountHolderId;
  accountStatus := IncluirContaResposta.accountStatus;
  account.Processar(IncluirContaResposta.account);
end;

{ TLibPIXCDMateraAccount }
constructor TLibPIXCDMateraAccount.Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessapRespMateraAccount, ATipo, AFormato);
  fmobilePhone := TLibPIXCDMateraMobilePhone.Create(CSessapRespMateraAccount, ATipo, AFormato);
  Clear;
end;

destructor TLibPIXCDMateraAccount.Destroy;
begin
  fmobilePhone.Free;
  inherited Destroy;
end;

procedure TLibPIXCDMateraAccount.Clear;
begin
  faccount := 0;
  faccountID := EmptyStr;
  fbranch := 0;
  fmobilePhone.Clear;
end;

procedure TLibPIXCDMateraAccount.Processar(const MateraAccount: TMateraAccount);
begin
  account := MateraAccount.account;
  accountID := MateraAccount.accountID;
  branch := MateraAccount.branch;
  mobilePhone.Processar(MateraAccount.mobilePhone);
end;

{ TLibPIXCDMateraTaxIdentifier }
procedure TLibPIXCDMateraTaxIdentifier.Clear;
begin
  fcountry := EmptyStr;
  ftaxId := EmptyStr;
  ftaxIdMasked := EmptyStr;
end;

procedure TLibPIXCDMateraTaxIdentifier.Processar(const MateraTaxIdentifier: TMateraTaxIdentifier);
begin
  country_TaxIdentifier := MateraTaxIdentifier.country;
  taxId := MateraTaxIdentifier.taxId;
  taxIdMasked := MateraTaxIdentifier.taxIdMasked;
end;

{ TLibPIXCDMateraMobilePhone }
procedure TLibPIXCDMateraMobilePhone.Clear;
begin
  fcountry := EmptyStr;
  fphoneNumber := EmptyStr;
end;

procedure TLibPIXCDMateraMobilePhone.Processar(const MateraMobilePhone: TMateraMobilePhone);
begin
  country := MateraMobilePhone.country;
  phoneNumber := MateraMobilePhone.phoneNumber;
end;

{ TLibPIXCDMateraEndereco }
procedure TLibPIXCDMateraEndereco.Clear;
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

procedure TLibPIXCDMateraEndereco.Processar(const MateraEndereco: TMateraEndereco);
begin
  bairro := MateraEndereco.bairro;
  cep := MateraEndereco.cep;
  cidade := MateraEndereco.cidade;
  complemento := MateraEndereco.complemento;
  estado := MateraEndereco.estado;
  logradouro := MateraEndereco.logradouro;
  numero := MateraEndereco.numero;
  pais := MateraEndereco.pais;
end;

{ TLibPIXCDMateraClientRepresentative }
constructor TLibPIXCDMateraClientRepresentative.Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
  fcontent := TACBrObjectList.Create;
  Clear;
end;

destructor TLibPIXCDMateraClientRepresentative.Destroy;
begin
  fcontent.Free;
  inherited Destroy;
end;

procedure TLibPIXCDMateraClientRepresentative.Clear;
begin
  faccountHolderId := EmptyStr;
  //BasicClient
  femail := EmptyStr;
  fname := EmptyStr;
  fsocialName := EmptyStr;
  //Documents
  fcontent.Clear;
  //MailAddress
  fbairro := EmptyStr;
  fcep := EmptyStr;
  fcidade := EmptyStr;
  fcomplemento := EmptyStr;
  festado := EmptyStr;
  flogradouro := EmptyStr;
  fnumero := EmptyStr;
  fpais := EmptyStr;
  //MobilePhone
  fcountry_mobilePhone := EmptyStr;
  fphoneNumber := EmptyStr;
  //TaxIdentifier
  fcountry_taxIdentifier := EmptyStr;
  ftaxId := EmptyStr;
  ftaxIdMasked := EmptyStr;
end;

procedure TLibPIXCDMateraClientRepresentative.Processar(const MateraClientRepresentative: TMateraClientRepresentative);
var
  i: Integer;
  contentsInfo: TLibPIXCDMateraDocument;
begin
  accountHolderId := MateraClientRepresentative.accountHolderId;
  //BasicClient
  email := MateraClientRepresentative.email;
  name := MateraClientRepresentative.name;
  socialName := MateraClientRepresentative.socialName;
  //Documents
  for i := 0 to MateraClientRepresentative.documents.Count - 1 do
  begin
    contentsInfo := TLibPIXCDMateraDocument.Create(CSessaoRespDocumentsMatera + IntToStr(i+1), Tipo, Formato);
    contentsInfo.Processar(MateraClientRepresentative.documents.Items[i]);
    fcontent.Add(contentsInfo);
  end;
  //MailAddress
  bairro := MateraClientRepresentative.mailAddress.bairro;
  cep := MateraClientRepresentative.mailAddress.cep;
  cidade := MateraClientRepresentative.mailAddress.cidade;
  complemento := MateraClientRepresentative.mailAddress.complemento;
  estado := MateraClientRepresentative.mailAddress.estado;
  logradouro := MateraClientRepresentative.mailAddress.logradouro;
  numero := MateraClientRepresentative.mailAddress.numero;
  pais := MateraClientRepresentative.mailAddress.pais;
  //MobilePhone
  country_mobilePhone := MateraClientRepresentative.mobilePhone.country;
  phoneNumber := MateraClientRepresentative.mobilePhone.phoneNumber;
  //TaxIdentifier
  country_taxIdentifier := MateraClientRepresentative.taxIdentifier.country;
  taxId := MateraClientRepresentative.taxIdentifier.taxId;
  taxIdMasked := MateraClientRepresentative.taxIdentifier.taxIdMasked;
end;

{ TLibPIXCDMateraAdditionalDetailsCorporate }
constructor TLibPIXCDMateraAdditionalDetailsCorporate.Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
  frepresentatives := TACBrObjectList.Create;
  Clear;
end;

destructor TLibPIXCDMateraAdditionalDetailsCorporate.Destroy;
begin
  frepresentatives.Free;
  inherited Destroy;
end;

procedure TLibPIXCDMateraAdditionalDetailsCorporate.Clear;
begin
  fbusinessLine := 0;
  fcompanyName := EmptyStr;
  festablishmentDate := 0;
  festablishmentForm := EmptyStr;
  ffinancialStatistic := 0;
  fmonthlyIncome := 0;
  frepresentatives.Clear;
end;

procedure TLibPIXCDMateraAdditionalDetailsCorporate.Processar(const MateraAdditionalDetailsCorporate: TMateraAdditionalDetailsCorporate);
var
  i: Integer;
  representativesInfo: TLibPIXCDMateraClientRepresentative;
begin
  businessLine := MateraAdditionalDetailsCorporate.businessLine;
  companyName := MateraAdditionalDetailsCorporate.companyName;
  establishmentDate := MateraAdditionalDetailsCorporate.establishmentDate;
  establishmentForm := MateraAdditionalDetailsCorporate.establishmentForm;
  financialStatistic := MateraAdditionalDetailsCorporate.financialStatistic;
  monthlyIncome := MateraAdditionalDetailsCorporate.monthlyIncome;

  for i := 0 to MateraAdditionalDetailsCorporate.representatives.Count - 1 do
  begin
    representativesInfo := TLibPIXCDMateraClientRepresentative.Create(CSessaoRespRepresentativeMatera + IntToStr(i+1), Tipo, Formato);
    representativesInfo.Processar(MateraAdditionalDetailsCorporate.representatives.Items[i]);
    frepresentatives.Add(representativesInfo);
  end;
end;

{ TLibPIXCDMateraDocument }
procedure TLibPIXCDMateraDocument.Clear;
begin
  fcontent := EmptyStr;
  ftype_ := mdtNone;
end;

procedure TLibPIXCDMateraDocument.Processar(const MateraDocument: TMateraDocument);
begin
  content := MateraDocument.content;
  type_ := MateraDocument.type_;
end;

{ TLibPIXCDMateraBasicClient }
constructor TLibPIXCDMateraBasicClient.Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespMateraClient, ATipo, AFormato);
  fmailAddress := TLibPIXCDMateraEndereco.Create(CSessaoRespMateraClientMailAddress, ATipo, AFormato);
  fmobilePhone := TLibPIXCDMateraMobilePhone.Create(CSessaoRespMateraClientMobilePhone, ATipo, AFormato);
  ftaxIdentifier := TLibPIXCDMateraTaxIdentifier.Create(CSessaoRespMateraClientTaxIdentifier, ATipo, AFormato);
  fdocuments := TACBrObjectList.Create;
end;

destructor TLibPIXCDMateraBasicClient.Destroy;
begin
  fmailAddress.Free;
  fmobilePhone.Free;
  ftaxIdentifier.Free;
  fdocuments.Free;
  inherited Destroy;
end;

procedure TLibPIXCDMateraBasicClient.Clear;
begin
  femail := EmptyStr;
  fname := EmptyStr;
  fsocialName := EmptyStr;
  fbirthDate := 0;
  fmother := EmptyStr;
  fmailAddress.Clear;
  fmobilePhone.Clear;
  ftaxIdentifier.Clear;
  fdocuments.Clear;
end;

procedure TLibPIXCDMateraBasicClient.Processar(const MateraClient: TMateraClient);
var
  i: Integer;
  documentsInfo: TLibPIXCDMateraDocument;
begin
  email := MateraClient.email;
  name := MateraClient.name;
  socialName := MateraClient.socialName;
  birthDate := MateraClient.birthDate;
  mother := MateraClient.mother;
  mailAddress.Processar(MateraClient.mailAddress);
  mobilePhone.Processar(MateraClient.mobilePhone);
  taxIdentifier.Processar(MateraClient.taxIdentifier);

  for i := 0 to MateraClient.documents.Count - 1 do
  begin
    documentsInfo := TLibPIXCDMateraDocument.Create(CSessaoRespDocumentsMatera + IntToStr(i+1), Tipo, Formato);
    documentsInfo.Processar(MateraClient.documents.Items[i]);
    fdocuments.Add(documentsInfo);
  end;
end;

{ TLibPIXCDExcluirChavePIXResposta }
constructor TLibPIXCDExcluirChavePIXResposta.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespExcluirChavePIX, ATipo, AFormato);
  Clear;
end;

destructor TLibPIXCDExcluirChavePIXResposta.Destroy;
begin
  inherited Destroy;
end;

procedure TLibPIXCDExcluirChavePIXResposta.Clear;
begin
  fResultCode := 0;
  fResultString := EmptyStr;
end;

procedure TLibPIXCDExcluirChavePIXResposta.Processar(const APSP: TACBrPSPMatera);
begin
  ResultCode := APSP.Http.ResultCode;
  ResultString := APSP.Http.ResultString;
end;

{ TLibPIXCDChavePIXResposta }
constructor TLibPIXCDChavePIXResposta.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespChavePIX, ATipo, AFormato);
  faliases := TACBrObjectList.Create;
  Clear;
end;

destructor TLibPIXCDChavePIXResposta.Destroy;
begin
  faliases.Free;
  inherited Destroy;
end;

procedure TLibPIXCDChavePIXResposta.Clear;
begin
  fneedsIdentifyConfirmation := False;
  fprotocolId := EmptyStr;
  faliases.Clear;
end;

procedure TLibPIXCDChavePIXResposta.Processar(const ChavesPIXResposta: TMateraAliasArray);
var
  item: TLibPIXCDMateraAlias;
  i: Integer;
begin
  for i := 0 to ChavesPIXResposta.Count - 1 do
  begin
    item := TLibPIXCDMateraAlias.Create(CSessaoRespAliasMatera + IntToStr(i), Tipo, Formato);
    item.Processar(ChavesPIXResposta[i]);
    aliases.Add(item);
  end;
end;

procedure TLibPIXCDChavePIXResposta.Processar(const ChavePIXResposta: TMateraRegisterAliasResponse);
var
  item: TLibPIXCDMateraAlias;
begin
  needsIdentifyConfirmation := ChavePIXResposta.needsIdentifyConfirmation;
  protocolId := ChavePIXResposta.protocolId;

  item := TLibPIXCDMateraAlias.Create(CSessaoRespAliasMatera, Tipo, Formato);
  item.Processar(ChavePIXResposta.alias_);
  aliases.Add(item);
end;

{ TLibPIXCDMateraAlias }
procedure TLibPIXCDMateraAlias.Clear;
begin
  fAlias_name := EmptyStr;
  fAlias_type := malNone;
  fAlias_status := mastNone;
end;

procedure TLibPIXCDMateraAlias.Processar(const MateraAlias: TMateraAlias);
begin
  Alias_name := MateraAlias.name;
  Alias_type := MateraAlias.type_;
  Alias_status := MateraAlias.status;
end;

{ TLibPIXCDConsultarTransacaoResposta }
constructor TLibPIXCDConsultarTransacaoResposta.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespConsultarTransacao, ATipo, AFormato);
  fItems := TACBrObjectList.Create;
  Clear;
end;

destructor TLibPIXCDConsultarTransacaoResposta.Destroy;
begin
  fItems.Free;
  inherited Destroy;
end;

procedure TLibPIXCDConsultarTransacaoResposta.Clear;
begin
  fhashNextPage := EmptyStr;
  fItems.Clear;
end;

procedure TLibPIXCDConsultarTransacaoResposta.Processar(const TransacoesResposta: TMateraTransactionResponseArray);
var
  item: TLibPIXCDTransactionResponse;
  i: Integer;
begin
  hashNextPage := TransacoesResposta.hashNextPage;

  for i := 0 to TransacoesResposta.Count - 1 do
  begin
    item := TLibPIXCDTransactionResponse.Create(CSessaoRespTransactionResponse + IntToStr(i), Tipo, Formato);
    item.Processar(TransacoesResposta[i]);
    Items.Add(item);
  end;
end;

{ TLibPIXCDTransactionResponse }
constructor TLibPIXCDTransactionResponse.Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespTransactionResponse, ATipo, AFormato);
  fbankTransfer := TLibPIXCDMateraBankTransfer.Create(CSessaoRespBankTransfer, ATipo, AFormato);
  fboleto := TLibPIXCDMateraBoletoPayment.Create(CSessaoRespBoletoPayment, ATipo, AFormato);
  fcancelPaymentTransaction := TLibPIXCDMateraCancelPaymentTransactionResponse.Create(CSessaoRespCancelPaymentTransactionResponse, ATipo, AFormato);
  fcounterPart := TLibPIXCDMateraAccountHolderResponse.Create(CSessaoRespAccountHolderResponse, ATipo, AFormato);
  fcoupon := TLibPIXCDMateraCoupon.Create(CSessaoRespMateraCoupon, ATipo, AFormato);
  fdrawee := TLibPIXCDMateraDrawee.Create(CSessaoRespMateraDrawee, ATipo, AFormato);
  finstantPayment := TLibPIXCDMateraInstantPaymentTransactionResponse.Create(CSessaoRespMateraInstantPaymentTransactionResponse, ATipo, AFormato);
  fmobilePhone := TLibPIXCDMateraMobilePhone.Create(CSessaoRespTransactionResponseMobilePhone, ATipo, AFormato);
  futilities := TLibPIXCDMateraUtilitiesPayment.Create(CSessaoRespTransactionResponseUtilities, ATipo, AFormato);
end;

destructor TLibPIXCDTransactionResponse.Destroy;
begin
  fbankTransfer.Free;
  fboleto.Free;
  fcancelPaymentTransaction.Free;
  fcounterPart.Free;
  fcoupon.Free;
  fdrawee.Free;
  finstantPayment.Free;
  fmobilePhone.Free;
  futilities.Free;
  inherited Destroy;
end;

procedure TLibPIXCDTransactionResponse.Clear;
begin
  faccountId := EmptyStr;
  faccountHolderId := EmptyStr;
  fbankTransfer.Clear;
  fboleto.Clear;
  fcancelPaymentTransaction.Clear;
  fcancelTransactionId := EmptyStr;
  fcounterPart.Clear;
  fcoupon.Clear;
  fcurrentAmount := 0;
  fdiscountAmount := 0;
  fdocUrl := EmptyStr;
  fdrawee.Clear;
  femail := EmptyStr;
  fendToEndId := EmptyStr;
  ffirstInstallmentValue := 0;
  finstallmentQuantity := 0;
  finstantPayment.Clear;
  fmobilePhone.Clear;
  fotherInstallmentValues := 0;
  fpaidAmount := 0;
  frecipientDescription := EmptyStr;
  fsideAccountId := EmptyStr;
  ftotalAmount := 0;
  ftransactionDate := 0;
  ftransactionId := EmptyStr;
  ftransactionStatus := mtsNone;
  ftransactionType := EmptyStr;
  futilities.Clear;
end;

procedure TLibPIXCDTransactionResponse.Processar(const TransactionResponse: TMateraTransactionResponse);
begin
  accountId := TransactionResponse.accountId;
  accountHolderId := TransactionResponse.accountHolderId;
  bankTransfer.Processar(TransactionResponse.bankTransfer);
  boleto.Processar(TransactionResponse.boleto);
  cancelPaymentTransaction.Processar(TransactionResponse.cancelPaymentTransaction);
  cancelTransactionId := TransactionResponse.cancelTransactionId;
  counterPart.Processar(TransactionResponse.counterPart);
  coupon.Processar(TransactionResponse.coupon);
  currentAmount := TransactionResponse.currentAmount;
  discountAmount := TransactionResponse.discountAmount;
  docUrl := TransactionResponse.docUrl;
  drawee.Processar(TransactionResponse.drawee);
  email := TransactionResponse.email;
  endToEndId := TransactionResponse.endToEndId;
  firstInstallmentValue := TransactionResponse.firstInstallmentValue;
  installmentQuantity := TransactionResponse.installmentQuantity;
  instantPayment.Processar(TransactionResponse.instantPayment);
  mobilePhone.Processar(TransactionResponse.mobilePhone);
  otherInstallmentValues := TransactionResponse.otherInstallmentValues;
  paidAmount := TransactionResponse.paidAmount;
  recipientDescription := TransactionResponse.recipientDescription;
  sideAccountId := TransactionResponse.sideAccountId;
  totalAmount := TransactionResponse.totalAmount;
  transactionDate := TransactionResponse.transactionDate;
  transactionId := TransactionResponse.transactionId;
  transactionStatus := TransactionResponse.transactionStatus;
  transactionType := TransactionResponse.transactionType;
  utilities.Processar(TransactionResponse.utilities);
end;

{ TLibPIXCDMateraBankTransfer }
constructor TLibPIXCDMateraBankTransfer.Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespBankTransfer, ATipo, AFormato);
  ftaxIdentifier := TLibPIXCDMateraTaxIdentifier.Create(CSessaoRespMateraParticipantInstantPaymentTaxIdentifier, ATipo, AFormato);
end;

destructor TLibPIXCDMateraBankTransfer.Destroy;
begin
  ftaxIdentifier.Free;
  inherited Destroy;
end;

procedure TLibPIXCDMateraBankTransfer.Clear;
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

procedure TLibPIXCDMateraBankTransfer.Processar(const MateraBankTransfer: TMateraBankTransfer);
begin
  accountDestination := MateraBankTransfer.accountDestination;
  accountDigitDestination := MateraBankTransfer.accountDigitDestination;
  accountTypeDestination := MateraBankTransfer.accountTypeDestination;
  bankDestination := MateraBankTransfer.bankDestination;
  branchDestination := MateraBankTransfer.branchDestination;
  historyCode := MateraBankTransfer.historyCode;
  name := MateraBankTransfer.name;
  personType := MateraBankTransfer.personType;
  purposeCode := MateraBankTransfer.purposeCode;
  taxIdentifier.Processar(MateraBankTransfer.taxIdentifier);
  transferMethod := MateraBankTransfer.transferMethod;
end;

{ TLibPIXCDMateraBoletoPayment }
constructor TLibPIXCDMateraBoletoPayment.Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespBoletoPayment, ATipo, AFormato);
end;

destructor TLibPIXCDMateraBoletoPayment.Destroy;
begin
  inherited Destroy;
end;

procedure TLibPIXCDMateraBoletoPayment.Clear;
begin
  fauthenticationCode := EmptyStr;
  fbankAuthentication := EmptyStr;
  fbarcode := EmptyStr;
  fbeneficiaryTaxIdentifier := EmptyStr;
  fdiscount := 0;
  fdocumentNumber := EmptyStr;
  fdueDate := 0;
  ffineAmount := 0;
  fhistoryCode := EmptyStr;
  finterestAmount := 0;
  fpaidAmount := 0;
  fstatus := mtsNone;
  ftypeableLine := EmptyStr;
end;

procedure TLibPIXCDMateraBoletoPayment.Processar(const MateraBoletoPayment: TMateraBoletoPayment);
begin
  barcode := MateraBoletoPayment.barcode;
  typeableLine := MateraBoletoPayment.typeableLine;
  paidAmount := MateraBoletoPayment.paidAmount;
  dueDate := MateraBoletoPayment.dueDate;
  interestAmount := MateraBoletoPayment.interestAmount;
  fineAmount := MateraBoletoPayment.fineAmount;
  discount := MateraBoletoPayment.discount;
  status := MateraBoletoPayment.status;
  bankAuthentication := MateraBoletoPayment.bankAuthentication;
  beneficiaryTaxIdentifier := MateraBoletoPayment.beneficiaryTaxIdentifier;
  authenticationCode := MateraBoletoPayment.authenticationCode;
end;

{ TLibPIXCDMateraCancelPaymentTransactionResponse }
constructor TLibPIXCDMateraCancelPaymentTransactionResponse.Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespCancelPaymentTransactionResponse, ATipo, AFormato);
end;

destructor TLibPIXCDMateraCancelPaymentTransactionResponse.Destroy;
begin
  inherited Destroy;
end;

procedure TLibPIXCDMateraCancelPaymentTransactionResponse.Clear;
begin
  fexternalProtocolId := EmptyStr;
  freason := EmptyStr;
  fsourceSystem := EmptyStr;
end;

procedure TLibPIXCDMateraCancelPaymentTransactionResponse.Processar(const MateraCancelPaymentTransactionResponse: TMateraCancelPaymentTransactionResponse);
begin
  externalProtocolId := MateraCancelPaymentTransactionResponse.externalProtocolId;
  reason := MateraCancelPaymentTransactionResponse.reason;
  sourceSystem := MateraCancelPaymentTransactionResponse.sourceSystem;
end;

{ TLibPIXCDMateraAccountHolderResponse }
constructor TLibPIXCDMateraAccountHolderResponse.Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespAccountHolderResponse, ATipo, AFormato);
  fbillingAddress := TLibPIXCDMateraEndereco.Create(CSessaoRespAccountHolderResponseBillingAddress, ATipo, AFormato);
end;

destructor TLibPIXCDMateraAccountHolderResponse.Destroy;
begin
  fbillingAddress.Free;
  inherited Destroy;
end;

procedure TLibPIXCDMateraAccountHolderResponse.Clear;
begin
  fbillingAddress.Clear;
end;

procedure TLibPIXCDMateraAccountHolderResponse.Processar(const MateraAccountHolderResponse: TMateraAccountHolderResponse);
begin
  billingAddress.Processar(MateraAccountHolderResponse.billingAddress);
end;

{ TLibPIXCDMateraCoupon }
constructor TLibPIXCDMateraCoupon.Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespMateraCoupon, ATipo, AFormato);
end;

destructor TLibPIXCDMateraCoupon.Destroy;
begin
  inherited Destroy;
end;

procedure TLibPIXCDMateraCoupon.Clear;
begin
  faccountHolderId := EmptyStr;
  faccountId := EmptyStr;
  fcouponId := EmptyStr;
  fcouponMerchantId := EmptyStr;
  fdescription := EmptyStr;
  fdiscount := 0;
  fdueDate := 0;
  fmaxUse := 0;
  fminExpenseValue := 0;
  fseller := EmptyStr;
  fstatus := macNone;
  fuseCount := 0;
end;

procedure TLibPIXCDMateraCoupon.Processar(const MateraCoupon: TMateraCoupon);
begin
  accountHolderId := MateraCoupon.accountHolderId;
  accountId := MateraCoupon.accountId;
  couponId := MateraCoupon.couponId;
  couponMerchantId := MateraCoupon.couponMerchantId;
  description := MateraCoupon.description;
  discount := MateraCoupon.discount;
  dueDate := MateraCoupon.dueDate;
  maxUse := MateraCoupon.maxUse;
  minExpenseValue := MateraCoupon.minExpenseValue;
  seller := MateraCoupon.seller;
  status := MateraCoupon.status;
  useCount := MateraCoupon.useCount;
end;

{ TLibPIXCDMateraDrawee }
constructor TLibPIXCDMateraDrawee.Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespMateraDrawee, ATipo, AFormato);
  ftaxIdentifier := TLibPIXCDMateraTaxIdentifier.Create(CSessaoRespMateraParticipantInstantPaymentTaxIdentifier, ATipo, AFormato);
end;

destructor TLibPIXCDMateraDrawee.Destroy;
begin
  ftaxIdentifier.Clear;
  inherited Destroy;
end;

procedure TLibPIXCDMateraDrawee.Clear;
begin
  fname := EmptyStr;
  ftaxIdentifier.Clear;
end;

procedure TLibPIXCDMateraDrawee.Processar(const MateraDrawee: TMateraDrawee);
begin
  name := MateraDrawee.name;
  taxIdentifier.Processar(MateraDrawee.taxIdentifier);
end;

{ TLibPIXCDMateraUtilitiesPayment }
procedure TLibPIXCDMateraUtilitiesPayment.Clear;
begin
  fdocumentNumber := EmptyStr;
  fbarcode := EmptyStr;
  fbeneficiaryTaxIdentifier := EmptyStr;
  ftypeableLine := EmptyStr;
  fdueDate := 0;
  fpaidAmount := 0;
end;

procedure TLibPIXCDMateraUtilitiesPayment.Processar(const MateraUtilitiesPayment: TMateraUtilitiesPayment);
begin
  documentNumber := MateraUtilitiesPayment.documentNumber;
  barcode := MateraUtilitiesPayment.barcode;
  beneficiaryTaxIdentifier := MateraUtilitiesPayment.beneficiaryTaxIdentifier;
  typeableLine := MateraUtilitiesPayment.typeableLine;
  dueDate := MateraUtilitiesPayment.dueDate;
  paidAmount := MateraUtilitiesPayment.paidAmount;
end;

{ TLibPIXCDMateraInstantPaymentTransactionResponse }
constructor TLibPIXCDMateraInstantPaymentTransactionResponse.Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespMateraInstantPaymentTransactionResponse, ATipo, AFormato);
  fsender := TLibPIXCDMateraParticipantInstantPayment.Create(CSessaoRespMateraPaymentReceivedSender, ATipo, AFormato);
  finstantPaymentCashValue := TLibPIXCDMateraCashValue.Create(CSessaoRespMateraInstantPaymentTransactionResponseInstantPaymentCashValue, ATipo, AFormato);
  fpaymentReceived := TACBrObjectList.Create;
  frecipient := TLibPIXCDMateraParticipantInstantPayment.Create(CSessaoRespMateraInstantPaymentTransactionResponseRecipient, ATipo, AFormato);
  frejectionReason := TLibPIXCDMateraRejectionReasonInstantPayment.Create(CSessaoRespMateraInstantPaymentTransactionResponseRejectionReason, ATipo, AFormato);
  freturnInfo := TLibPIXCDMateraInstantPaymentTransactionReturnInfo.Create(CSessaoRespMateraInstantPaymentTransactionResponseReturnInfo, ATipo, AFormato);
  foriginDepositTransaction := TLibPIXCDMateraOriginDepositTransactionResponse.Create(CSessaoRespMateraInstantPaymentTransactionResponseOriginDepositTransaction, ATipo, AFormato);
end;

destructor TLibPIXCDMateraInstantPaymentTransactionResponse.Destroy;
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

procedure TLibPIXCDMateraInstantPaymentTransactionResponse.Clear;
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

procedure TLibPIXCDMateraInstantPaymentTransactionResponse.Processar(const MateraInstantPaymentTransactionResponse: TMateraInstantPaymentTransactionResponse);
var
  i: integer;
  item: TLibPIXCDMateraPaymentReceived;
begin
  endToEndId := EmptyStr;
  additionalInformation := EmptyStr;
  sender.Processar(MateraInstantPaymentTransactionResponse.sender);
  instantPaymentCashValue.Processar(MateraInstantPaymentTransactionResponse.instantPaymentCashValue);

  for i := 0 to MateraInstantPaymentTransactionResponse.paymentReceived.Count - 1 do
  begin
    item := TLibPIXCDMateraPaymentReceived.Create(CSessaoRespMateraPaymentReceived + IntToStr(i), Tipo, Formato);
    item.Processar(MateraInstantPaymentTransactionResponse.paymentReceived[i]);
    paymentReceived.Add(item);
  end;

  recipient.Processar(MateraInstantPaymentTransactionResponse.recipient);
  rejectionReason.Processar(MateraInstantPaymentTransactionResponse.rejectionReason);
  returnInfo.Processar(MateraInstantPaymentTransactionResponse.returnInfo);
  originDepositTransaction.Processar(MateraInstantPaymentTransactionResponse.originDepositTransaction);
end;

{ TLibPIXCDMateraParticipantInstantPayment }
constructor TLibPIXCDMateraParticipantInstantPayment.Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
  faccount := TLibPIXCDMateraDestinationAccount.Create(CSessaoRespMateraParticipantInstantPaymentAccount, ATipo, AFormato);
  fpsp := TLibPIXCDMateraPSP.Create(CSessaoRespMateraParticipantInstantPaymentPsp, ATipo, AFormato);
  ftaxIdentifier := TLibPIXCDMateraTaxIdentifier.Create(CSessaoRespMateraParticipantInstantPaymentTaxIdentifier, ATipo, AFormato);
end;

destructor TLibPIXCDMateraParticipantInstantPayment.Destroy;
begin
  faccount.Free;
  fpsp.Free;
  ftaxIdentifier.Free;
  inherited Destroy;
end;

procedure TLibPIXCDMateraParticipantInstantPayment.Clear;
begin
  faccount.Clear;
  falias := EmptyStr;
  fname := EmptyStr;
  fpsp.Clear;
  ftaxIdentifier.Clear;
end;

procedure TLibPIXCDMateraParticipantInstantPayment.Processar(const MateraParticipantInstantPayment: TMateraParticipantInstantPayment);
begin
  account.Processar(MateraParticipantInstantPayment.account);
  alias_ := MateraParticipantInstantPayment.alias_;
  name := MateraParticipantInstantPayment.name;
  psp.Processar(MateraParticipantInstantPayment.psp);
  taxIdentifier.Processar(MateraParticipantInstantPayment.taxIdentifier);
end;

{ TLibPIXCDMateraDestinationAccount }
procedure TLibPIXCDMateraDestinationAccount.Clear;
begin
  faccount := EmptyStr;
  faccountType := matdNone;
  fbranch := EmptyStr;
end;

procedure TLibPIXCDMateraDestinationAccount.Processar(const MateraDestinationAccount: TMateraDestinationAccount);
begin
  account := MateraDestinationAccount.account;
  accountType := MateraDestinationAccount.accountType;
  branch := MateraDestinationAccount.branch;
end;

{ TLibPIXCDMateraPSP }
procedure TLibPIXCDMateraPSP.Clear;
begin
  fcountry := EmptyStr;
  fcurrencies := EmptyStr;
  fid := EmptyStr;
  fname := EmptyStr;
end;

procedure TLibPIXCDMateraPSP.Processar(const MateraPSP: TMateraPSP);
begin
  country_MateraPSP := MateraPSP.country;
  currencies := SplitToString(MateraPSP.currencies);
  id := MateraPSP.id;
  name := MateraPSP.name;
end;

{ TLibPIXCDMateraCashValue }
constructor TLibPIXCDMateraCashValue.Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespMateraCashValue, ATipo, AFormato);
  fwithdrawProviders := TLibPIXCDMateraWithdrawProviders.Create(CSessaoRespMateraCashValueWithdrawProviders, ATipo, AFormato);
end;

destructor TLibPIXCDMateraCashValue.Destroy;
begin
  fwithdrawProviders.Free;
  inherited Destroy;
end;

procedure TLibPIXCDMateraCashValue.Clear;
begin
  fallowValueChange := False;
  fcashValueType := mcvNone;
  fvalue := EmptyStr;
  fwithdrawProviders.Clear;
end;

procedure TLibPIXCDMateraCashValue.Processar(const MateraCashValue: TMateraCashValue);
begin
  allowValueChange := MateraCashValue.allowValueChange;
  cashValueType := MateraCashValue.cashValueType;
  value := MateraCashValue.value;
  withdrawProviders.Processar(MateraCashValue.withdrawProviders);
end;

{ TLibPIXCDMateraWithdrawProviders }
procedure TLibPIXCDMateraWithdrawProviders.Clear;
begin
  fagentModality := mwatNone;
  fserviceProvider := EmptyStr;
end;

procedure TLibPIXCDMateraWithdrawProviders.Processar(const MateraWithdrawProviders: TMateraWithdrawProviders);
begin
  agentModality := MateraWithdrawProviders.agentModality;
  serviceProvider := MateraWithdrawProviders.serviceProvider;
end;

{ TLibPIXCDMateraRejectionReasonInstantPayment }
procedure TLibPIXCDMateraRejectionReasonInstantPayment.Clear;
begin
  fcode := EmptyStr;
  fdescription := EmptyStr;
end;

procedure TLibPIXCDMateraRejectionReasonInstantPayment.Processar(const MateraRejectionReasonInstantPayment: TMateraRejectionReasonInstantPayment);
begin
  code := MateraRejectionReasonInstantPayment.code;
  description := MateraRejectionReasonInstantPayment.description;
end;

{ TLibPIXCDMateraInstantPaymentTransactionReturnInfo }
constructor TLibPIXCDMateraInstantPaymentTransactionReturnInfo.Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespMateraInstantPaymentTransactionReturnInfo, ATipo, AFormato);
  freturnReasonInformation := TLibPIXCDMateraInstantPaymentTransactionReturnReasonInformation.Create(CSessaoRespMateraInstantPaymentTransactionReturnInfoReturnReasonInformation, ATipo, AFormato);
end;

destructor TLibPIXCDMateraInstantPaymentTransactionReturnInfo.Destroy;
begin
  freturnReasonInformation.Free;
  inherited Destroy;
end;

procedure TLibPIXCDMateraInstantPaymentTransactionReturnInfo.Clear;
begin
  foriginalEndToEndId := EmptyStr;
  foriginalInstantPaymentId := EmptyStr;
  freturnReasonInformation.Clear;
end;

procedure TLibPIXCDMateraInstantPaymentTransactionReturnInfo.Processar(const MateraInstantPaymentTransactionReturnInfo: TMateraInstantPaymentTransactionReturnInfo);
begin
  originalEndToEndId := MateraInstantPaymentTransactionReturnInfo.originalEndToEndId;
  originalInstantPaymentId := MateraInstantPaymentTransactionReturnInfo.originalInstantPaymentId;
  returnReasonInformation.Processar(MateraInstantPaymentTransactionReturnInfo.returnReasonInformation);
end;

{ TLibPIXCDMateraInstantPaymentTransactionReturnReasonInformation }
procedure TLibPIXCDMateraInstantPaymentTransactionReturnReasonInformation.Clear;
begin
  fadditionalInformation := EmptyStr;
  freasonCode := EmptyStr;
  freasonDescription := EmptyStr;
end;

procedure TLibPIXCDMateraInstantPaymentTransactionReturnReasonInformation.Processar(const MateraInstantPaymentTransactionReturnReasonInformation: TMateraInstantPaymentTransactionReturnReasonInformation);
begin
  additionalInformation := MateraInstantPaymentTransactionReturnReasonInformation.additionalInformation;
  reasonCode := MateraInstantPaymentTransactionReturnReasonInformation.reasonCode;
  reasonDescription := MateraInstantPaymentTransactionReturnReasonInformation.reasonDescription;
end;

{ TLibPIXCDMateraOriginDepositTransactionResponse }
constructor TLibPIXCDMateraOriginDepositTransactionResponse.Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespMateraOriginDepositTransactionResponse, ATipo, AFormato);
  foriginInstantPaymentTransaction := TLibPIXCDMateraOriginInstantPaymentTransactionResponse.Create(CSessaoRespMateraOriginInstantPaymentTransactionResponse, ATipo, AFormato);
end;

destructor TLibPIXCDMateraOriginDepositTransactionResponse.Destroy;
begin
  foriginInstantPaymentTransaction.Free;
  inherited Destroy;
end;

procedure TLibPIXCDMateraOriginDepositTransactionResponse.Clear;
begin
  fadditionalInformation := EmptyStr;
  ftotalAmount := 0;
  ftransactionDate := EmptyStr;
  ftransactionId := EmptyStr;
  foriginInstantPaymentTransaction.Clear;
end;

procedure TLibPIXCDMateraOriginDepositTransactionResponse.Processar(const MateraOriginDepositTransactionResponse: TMateraOriginDepositTransactionResponse);
begin
  additionalInformation := MateraOriginDepositTransactionResponse.additionalInformation;
  totalAmount := MateraOriginDepositTransactionResponse.totalAmount;
  transactionDate := MateraOriginDepositTransactionResponse.transactionDate;
  transactionId := MateraOriginDepositTransactionResponse.transactionId;
  originInstantPaymentTransaction.Processar(MateraOriginDepositTransactionResponse.originInstantPaymentTransaction);
end;

{ TLibPIXCDMateraOriginInstantPaymentTransactionResponse }
constructor TLibPIXCDMateraOriginInstantPaymentTransactionResponse.Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespMateraOriginInstantPaymentTransactionResponse, ATipo, AFormato);
  fsender := TLibPIXCDMateraParticipantInstantPayment.Create(CSessaoRespMateraPaymentReceivedSender, ATipo, AFormato);
end;

destructor TLibPIXCDMateraOriginInstantPaymentTransactionResponse.Destroy;
begin
  fsender.Free;
  inherited Destroy;
end;

procedure TLibPIXCDMateraOriginInstantPaymentTransactionResponse.Clear;
begin
  fendToEndId := EmptyStr;
  fsender.Clear;
end;

procedure TLibPIXCDMateraOriginInstantPaymentTransactionResponse.Processar(const MateraOriginInstantPaymentTransactionResponse: TMateraOriginInstantPaymentTransactionResponse);
begin
  endToEndId := MateraOriginInstantPaymentTransactionResponse.endToEndId;
  sender.Processar(MateraOriginInstantPaymentTransactionResponse.sender);
end;

{ TLibPIXCDMateraPaymentReceived }
constructor TLibPIXCDMateraPaymentReceived.Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
  fsender := TLibPIXCDMateraParticipantInstantPayment.Create(CSessaoRespMateraPaymentReceivedSender, ATipo, AFormato);
end;

destructor TLibPIXCDMateraPaymentReceived.Destroy;
begin
  inherited Destroy;
  fsender.Free;
end;

procedure TLibPIXCDMateraPaymentReceived.Clear;
begin
  fadditionalInformation := EmptyStr;
  fendToEndId := EmptyStr;
  flegacyTransactionId := EmptyStr;
  freceivedAmount := 0;
  fsender.Clear;
  ftransactionTimestamp := EmptyStr;
end;

procedure TLibPIXCDMateraPaymentReceived.Processar(const MateraPaymentReceived: TMateraPaymentReceived);
begin
  additionalInformation := MateraPaymentReceived.additionalInformation;
  endToEndId := MateraPaymentReceived.endToEndId;
  legacyTransactionId := MateraPaymentReceived.legacyTransactionId;
  receivedAmount := MateraPaymentReceived.receivedAmount;
  sender.Processar(MateraPaymentReceived.sender);
  transactionTimestamp := MateraPaymentReceived.transactionTimestamp;
end;

{ TLibPIXCDSaldoECResposta }
constructor TLibPIXCDSaldoECResposta.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespSaldoECResposta, ATipo, AFormato);
end;

destructor TLibPIXCDSaldoECResposta.Destroy;
begin
  inherited Destroy;
end;

procedure TLibPIXCDSaldoECResposta.Clear;
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

procedure TLibPIXCDSaldoECResposta.Processar(const SaldoECResposta: TMateraBalanceResponse);
begin
  accountId := SaldoECResposta.accountId;
  autoInvest := SaldoECResposta.autoInvest;
  available := SaldoECResposta.available;
  availableBalanceForTransactions := SaldoECResposta.availableBalanceForTransactions;
  blocked := SaldoECResposta.blocked;
  date := SaldoECResposta.date;
  emergencyAidBalance := SaldoECResposta.emergencyAidBalance;
  future := SaldoECResposta.future;
  overdraft := SaldoECResposta.overdraft;
  real := SaldoECResposta.real;
end;

{ TLibPIXCDExtratoECResposta }
constructor TLibPIXCDExtratoECResposta.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespExtratoECResposta, ATipo, AFormato);
  fstatement := TACBrObjectList.Create;
end;

destructor TLibPIXCDExtratoECResposta.Destroy;
begin
  fstatement.Free;
  inherited Destroy;
end;

procedure TLibPIXCDExtratoECResposta.Clear;
begin
  fstatement.Clear;
end;

procedure TLibPIXCDExtratoECResposta.Processar(const ExtratoECResposta: TMaterastatementResponse);
var
  item: TLibPIXCDMaterastatementEntry;
  i: Integer;
begin
  for i := 0 to ExtratoECResposta.statement.Count - 1 do
  begin
    item := TLibPIXCDMaterastatementEntry.Create(CSessaoRespMaterastatementEntry + IntToStr(i), Tipo, Formato);
    item.Processar(ExtratoECResposta.statement[i]);
    statement.Add(item);
  end;
end;

{ TLibPIXCDMaterastatementEntry }
constructor TLibPIXCDMaterastatementEntry.Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespMaterastatementEntry, ATipo, AFormato);
  fcounterpart := TLibPIXCDMateraCounterpart.Create(CSessaoRespMaterastatementEntry, ATipo, AFormato);
  finstantPaymentCashValue := TLibPIXCDMateraStatementInstantPaymentCashValue.Create(CSessaoRespMaterastatementEntry, ATipo, AFormato);
end;

destructor TLibPIXCDMaterastatementEntry.Destroy;
begin
  fcounterpart.Free;
  finstantPaymentCashValue.Free;
  inherited Destroy;
end;

procedure TLibPIXCDMaterastatementEntry.Clear;
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

procedure TLibPIXCDMaterastatementEntry.Processar(const MaterastatementEntry: TMaterastatementEntry);
begin
  additionalInfo := MaterastatementEntry.additionalInfo;
  amount := MaterastatementEntry.amount;
  comment := MaterastatementEntry.comment;
  counterpart.Processar(MaterastatementEntry.counterpart);
  creditDate := MaterastatementEntry.creditDate;
  description := MaterastatementEntry.description;
  entryDate := MaterastatementEntry.entryDate;
  historyCode := MaterastatementEntry.historyCode;
  instantPaymentCashValue.Processar(MaterastatementEntry.instantPaymentCashValue.values);
  transactionId := MaterastatementEntry.transactionId;
  transactionType := MaterastatementEntry.transactionType;
  type_ := MaterastatementEntry.type_;
end;

{ TLibPIXCDMateraCounterpart }
constructor TLibPIXCDMateraCounterpart.Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespMateraCounterpart, ATipo, AFormato);
  fTaxIdentifier := TLibPIXCDMateraTaxIdentifier.Create(CSessaoRespMateraCounterpart, ATipo, AFormato);
end;

destructor TLibPIXCDMateraCounterpart.Destroy;
begin
  fTaxIdentifier.Free;
  inherited Destroy;
end;

procedure TLibPIXCDMateraCounterpart.Clear;
begin
  faccountDestination := EmptyStr;
  fbankDestination := EmptyStr;
  fbranchDestination := EmptyStr;
  fclientType := mctNone;
  fname := EmptyStr;
  fTaxIdentifier.Clear;
end;

procedure TLibPIXCDMateraCounterpart.Processar(const MateraCounterpart: TMateraCounterpart);
begin
  accountDestination := MateraCounterpart.accountDestination;
  bankDestination := MateraCounterpart.bankDestination;
  branchDestination := MateraCounterpart.branchDestination;
  clientType := MateraCounterpart.clientType;
  name := MateraCounterpart.name;
  TaxIdentifier.Processar(MateraCounterpart.TaxIdentifier);
end;

{ TLibPIXCDMateraStatementInstantPaymentCashValue }
constructor TLibPIXCDMateraStatementInstantPaymentCashValue.Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespMateraStatementInstantPaymentCashValue, ATipo, AFormato);
  fvalues := TACBrObjectList.Create;
end;

destructor TLibPIXCDMateraStatementInstantPaymentCashValue.Destroy;
begin
  fvalues.Free;
  inherited Destroy;
end;

procedure TLibPIXCDMateraStatementInstantPaymentCashValue.Clear;
begin
  fvalues.Clear;
end;

procedure TLibPIXCDMateraStatementInstantPaymentCashValue.Processar(const StatementInstantPaymentCashValueDataArray: TStatementInstantPaymentCashValueDataArray);
var
  item: TLibPIXCDMateraStatementInstantPaymentCashValueData;
  i: Integer;
begin
  for i := 0 to StatementInstantPaymentCashValueDataArray.Count - 1 do
  begin
    item := TLibPIXCDMateraStatementInstantPaymentCashValueData.Create(CSessaoRespMateraStatementInstantPaymentCashValue + IntToStr(i), Tipo, Formato);
    item.Processar(StatementInstantPaymentCashValueDataArray[i]);
    values.Add(item);
  end;
end;

{ TLibPIXCDMateraStatementInstantPaymentCashValueData }
constructor TLibPIXCDMateraStatementInstantPaymentCashValueData.Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespMateraStatementInstantPaymentCashValueData, ATipo, AFormato);
end;

destructor TLibPIXCDMateraStatementInstantPaymentCashValueData.Destroy;
begin
  inherited Destroy;
end;

procedure TLibPIXCDMateraStatementInstantPaymentCashValueData.Clear;
begin
  fcashValueType := msipvtNone;
  fvalue := 0;
end;

procedure TLibPIXCDMateraStatementInstantPaymentCashValueData.Processar(const MateraStatementInstantPaymentCashValueData: TMateraStatementInstantPaymentCashValueData);
begin
  cashValueType := MateraStatementInstantPaymentCashValueData.cashValueType;
  value := MateraStatementInstantPaymentCashValueData.value;
end;

{ TLibPIXCDMotivosDevolucaoResposta }
constructor TLibPIXCDMotivosDevolucaoResposta.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespMotivosDevolucaoResposta, ATipo, AFormato);
  freturnCodes := TACBrObjectList.Create;
end;

destructor TLibPIXCDMotivosDevolucaoResposta.Destroy;
begin
  freturnCodes.Free;
  inherited Destroy;
end;

procedure TLibPIXCDMotivosDevolucaoResposta.Clear;
begin
  freturnCodes.Clear;
end;

procedure TLibPIXCDMotivosDevolucaoResposta.Processar(const MotivosDevolucaoResposta: TMateraReturnCodeArray);
var
  item: TLibPIXCDMateraReturnCode;
  i: Integer;
begin
  for i := 0 to MotivosDevolucaoResposta.Count - 1 do
  begin
    item := TLibPIXCDMateraReturnCode.Create(CSessaoRespMateraReturnCode + IntToStr(i), Tipo, Formato);
    item.Processar(MotivosDevolucaoResposta[i]);
    returnCodes.Add(item);
  end;
end;

{ TLibPIXCDMateraReturnCode }
procedure TLibPIXCDMateraReturnCode.Clear;
begin
  fcode := EmptyStr;
  fdescription := EmptyStr;
end;

procedure TLibPIXCDMateraReturnCode.Processar(const MateraReturnCode: TMateraReturnCode);
begin
  code := MateraReturnCode.code;
  description := MateraReturnCode.description;
end;

{ TLibPIXCDAliasRetiradaResposta }
constructor TLibPIXCDAliasRetiradaResposta.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespAliasRetiradaResposta, ATipo, AFormato);
  faccountDestination := TLibPIXCDMateraDestinationAccount.Create(CSessaoRespAliasRetiradaRespostaAccountDestination, ATipo, AFormato);
  faliasAccountHolder := TLibPIXCDMateraAliasAccountHolder.Create(CSessaoRespAliasRetiradaRespostaAccountHolder, ATipo, AFormato);
  fantiFraudClearingInfo := TLibPIXCDMateraAntiFraudClearingInfo.Create(CSessaoRespAliasRetiradaRespostaAntiFraudClearingInfo, ATipo, AFormato);
  fpsp := TLibPIXCDMateraPSP.Create(CSessaoRespAliasRetiradaRespostaPsp, ATipo, AFormato);
  //MateraAliasResponseV2
  faliasHolderAddress := TLibPIXCDMateraaliasHolderAddress.Create(CSessaoRespAliasResponseV2AliasHolderAddress, ATipo, AFormato);
  faliasStatistics := TLibPIXCDMateraaliasStatistics.Create(CSessaoRespAliasResponseV2AliasStatistics, ATipo, AFormato);
  fpersonStatistics := TLibPIXCDMaterapersonStatistics.Create(CSessaoRespAliasResponseV2PersonStatistics, ATipo, AFormato);
end;

destructor TLibPIXCDAliasRetiradaResposta.Destroy;
begin
  faccountDestination.Free;
  faliasAccountHolder.Free;
  fantiFraudClearingInfo.Free;
  fpsp.Free;
  //MateraAliasResponseV2
  faliasHolderAddress.Free;
  faliasStatistics.Free;
  fpersonStatistics.Free;
  inherited Destroy;
end;

procedure TLibPIXCDAliasRetiradaResposta.Clear;
begin
  faccountDestination.Clear;
  faliasAccountHolder.Clear;
  faliasType := malNone;
  falias_ := EmptyStr;
  fantiFraudClearingInfo.Clear;
  fcreationDate := 0;
  fendtoEndId := EmptyStr;
  fpsp.Clear;
  //MateraAliasResponseV2
  faliasHolderAddress.Clear;
  faliasStatistics.Clear;
  fpersonStatistics.Clear;
  fstatus := mastNone;
end;

procedure TLibPIXCDAliasRetiradaResposta.Processar(const AliasRetiradaResposta: TMateraAliasResponse);
begin
  accountDestination.Processar(AliasRetiradaResposta.accountDestination);
  aliasAccountHolder.Processar(AliasRetiradaResposta.aliasAccountHolder);
  aliasType := AliasRetiradaResposta.aliasType;
  alias_ := AliasRetiradaResposta.alias_;
  antiFraudClearingInfo.Processar(AliasRetiradaResposta.antiFraudClearingInfo);
  creationDate := AliasRetiradaResposta.creationDate;
  endtoEndId := AliasRetiradaResposta.endtoEndId;
  psp.Processar(AliasRetiradaResposta.psp);
end;

procedure TLibPIXCDAliasRetiradaResposta.ProcessarAliasResponseV2(const MateraAliasResponseV2: TMateraAliasResponseV2);
begin
  aliasHolderAddress.Processar(MateraAliasResponseV2.aliasHolderAddress);
  aliasStatistics.Processar(MateraAliasResponseV2.aliasStatistics);
  personStatistics.Processar(MateraAliasResponseV2.personStatistics);
  status := MateraAliasResponseV2.status;
end;

{ TLibPIXCDMateraAliasAccountHolder }
constructor TLibPIXCDMateraAliasAccountHolder.Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespAliasAccountHolder, ATipo, AFormato);
  ftaxIdentifier := TLibPIXCDMateraTaxIdentifier.Create(CSessaoRespAliasAccountHolderTaxIdentifier, ATipo, AFormato);
end;

destructor TLibPIXCDMateraAliasAccountHolder.Destroy;
begin
  inherited Destroy;
end;

procedure TLibPIXCDMateraAliasAccountHolder.Clear;
begin
  fname := EmptyStr;
end;

procedure TLibPIXCDMateraAliasAccountHolder.Processar(const MateraAliasAccountHolder: TMateraAliasAccountHolder);
begin
  name := MateraAliasAccountHolder.name;
  taxIdentifier.Processar(MateraAliasAccountHolder.taxIdentifier);
end;

{ TLibPIXCDMateraAntiFraudClearingInfo }
constructor TLibPIXCDMateraAntiFraudClearingInfo.Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespAntiFraudClearingInfo, ATipo, AFormato);
  fcounters := TACBrObjectList.Create;
end;

destructor TLibPIXCDMateraAntiFraudClearingInfo.Destroy;
begin
  fcounters.Free;
  inherited Destroy;
end;

procedure TLibPIXCDMateraAntiFraudClearingInfo.Clear;
begin
  fcounters.Clear;
  flastUpdated := 0;
end;

procedure TLibPIXCDMateraAntiFraudClearingInfo.Processar(const MateraAntiFraudClearingInfo: TMateraAntiFraudClearingInfo);
var
  item: TLibPIXCDMateraCounter;
  i: integer;
begin
  for i := 0 to MateraAntiFraudClearingInfo.counters.Count - 1 do
  begin
    item := TLibPIXCDMateraCounter.Create(CSessaoRespMateraCounter + IntToStr(i), Tipo, Formato);
    item.Processar(MateraAntiFraudClearingInfo.counters[i]);
    counters.Add(item);
  end;
end;

{ TLibPIXCDMateraCounter }
constructor TLibPIXCDMateraCounter.Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
    inherited Create(CSessaoRespMateraCounter, ATipo, AFormato);
end;

destructor TLibPIXCDMateraCounter.Destroy;
begin
  inherited Destroy;
end;

procedure TLibPIXCDMateraCounter.Clear;
begin
  fby := EmptyStr;
  fd3 := 0;
  fd30 := 0;
  fm6 := 0;
  ftype_ := mafcNone;
end;

procedure TLibPIXCDMateraCounter.Processar(const MateraCounter: TMateraCounter);
begin
  by := MateraCounter.by;
  d3 := MateraCounter.d3;
  d30 := MateraCounter.d30;
  m6 := MateraCounter.m6;
  type_ := MateraCounter.type_;
end;

{ TLibPIXCDMateraaliasHolderAddress }
procedure TLibPIXCDMateraaliasHolderAddress.Clear;
begin
  fcity := EmptyStr;
  fstreet := EmptyStr;
  fuf := EmptyStr;
  fzipCode := EmptyStr;
end;

procedure TLibPIXCDMateraaliasHolderAddress.Processar(const MateraaliasHolderAddress: TMateraaliasHolderAddress);
begin
  city := MateraaliasHolderAddress.city;
  street := MateraaliasHolderAddress.street;
  uf := MateraaliasHolderAddress.uf;
  zipCode := MateraaliasHolderAddress.zipCode;
end;

{ TLibPIXCDMateraaliasStatistics }
constructor TLibPIXCDMateraaliasStatistics.Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespMateraaliasStatistics, ATipo, AFormato);
  fentries := TLibPIXCDMateraentries.Create(CSessaoRespMateraaliasStatisticsEntries, ATipo, AFormato);
  ffraudMarkers := TLibPIXCDMateraFraudMarkers.Create(CSessaoRespMateraaliasStatisticsFraudMarkers, ATipo, AFormato);
  finfractionReports := TLibPIXCDMaterainfractionReports.Create(CSessaoRespMateraaliasStatisticsInfractionReports, ATipo, AFormato);
  fspi := TLibPIXCDMateraspi.Create(CSessaoRespMateraaliasStatisticsSpi, ATipo, AFormato);
end;

destructor TLibPIXCDMateraaliasStatistics.Destroy;
begin
  fentries.Free;
  ffraudMarkers.Free;
  finfractionReports.Free;
  fspi.Free;
  inherited Destroy;
end;

procedure TLibPIXCDMateraaliasStatistics.Clear;
begin
  fentries.Clear;
  ffraudMarkers.Clear;
  finfractionReports.Clear;
  fspi.Clear;
end;

procedure TLibPIXCDMateraaliasStatistics.Processar(const MateraaliasStatistics: TMateraaliasStatistics);
begin
  entries.Processar(MateraaliasStatistics.entries);
  fraudMarkers.Processar(MateraaliasStatistics.fraudMarkers);
  infractionReports.Processar(MateraaliasStatistics.infractionReports);
  spi.Processar(MateraaliasStatistics.spi);
end;

{ TLibPIXCDMateraentries }
procedure TLibPIXCDMateraentries.Clear;
begin
  fregisteredAccounts := 0;
  fwatermark := EmptyStr;
end;

procedure TLibPIXCDMateraentries.Processar(const Materaentries: TMateraentries);
begin
  registeredAccounts := Materaentries.registeredAccounts;
  watermark := Materaentries.watermark;
end;

{ TLibPIXCDMateraFraudMarkers }
constructor TLibPIXCDMateraFraudMarkers.Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespMateraFraudMarkers, ATipo, AFormato);
  fapplicationFrauds := TLibPIXCDMateraValues.Create(CSessaoRespMateraFraudMarkersApplicationFrauds, ATipo, AFormato);
  fdistinctFraudReporters := TLibPIXCDMateraValues.Create(CSessaoRespMateraFraudMarkersDistinctFraudReporters, ATipo, AFormato);
  fmuleAccounts := TLibPIXCDMateraValues.Create(CSessaoRespMateraFraudMarkersMuleAccounts, ATipo, AFormato);
  fotherFrauds := TLibPIXCDMateraValues.Create(CSessaoRespMateraFraudMarkersOtherFrauds, ATipo, AFormato);
  fscammerAccounts := TLibPIXCDMateraValues.Create(CSessaoRespMateraFraudMarkersScammerAccounts, ATipo, AFormato);
  ftotalFrauds := TLibPIXCDMateraValues.Create(CSessaoRespMateraFraudMarkersTotalFrauds, ATipo, AFormato);
  ftotalFraudTransactionAmount := TLibPIXCDMateraValues.Create(CSessaoRespMateraFraudMarkersTotalFraudTransactionAmount, ATipo, AFormato);
end;

destructor TLibPIXCDMateraFraudMarkers.Destroy;
begin
  fapplicationFrauds.Free;
  fdistinctFraudReporters.Free;
  fmuleAccounts.Free;
  fotherFrauds.Free;
  fscammerAccounts.Free;
  ftotalFrauds.Free;
  ftotalFraudTransactionAmount.Free;
  inherited Destroy;
end;

procedure TLibPIXCDMateraFraudMarkers.Clear;
begin
  fapplicationFrauds.Clear;
  fdistinctFraudReporters.Clear;
  fmuleAccounts.Clear;
  fotherFrauds.Clear;
  fscammerAccounts.Clear;
  ftotalFrauds.Clear;
  ftotalFraudTransactionAmount.Clear;
  fwatermark := EmptyStr;
end;

procedure TLibPIXCDMateraFraudMarkers.Processar(const MateraFraudMarkers: TMateraFraudMarkers);
begin
  applicationFrauds.Processar(MateraFraudMarkers.applicationFrauds);
  distinctFraudReporters.Processar(MateraFraudMarkers.distinctFraudReporters);
  muleAccounts.Processar(MateraFraudMarkers.muleAccounts);
  otherFrauds.Processar(MateraFraudMarkers.otherFrauds);
  scammerAccounts.Processar(MateraFraudMarkers.scammerAccounts);
  totalFrauds.Processar(MateraFraudMarkers.totalFrauds);
  totalFraudTransactionAmount.Processar(MateraFraudMarkers.totalFraudTransactionAmount);
  watermark := MateraFraudMarkers.watermark;
end;

{ TLibPIXCDMateraValues }
procedure TLibPIXCDMateraValues.Clear;
begin
  fd90 := 0;
  fm12 := 0;
  fm60 := 0;
end;

procedure TLibPIXCDMateraValues.Processar(const MateraValues: TMateraValues);
begin
  d90 := MateraValues.d90;
  m12 := MateraValues.m12;
  m60 := MateraValues.m60;
end;

{ TLibPIXCDMaterainfractionReports }
constructor TLibPIXCDMaterainfractionReports.Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespMaterainfractionReports, ATipo, AFormato);
  frejectedReports := TLibPIXCDMateraValues.Create(CSessaoRespMaterainfractionReportsRejectedReports, ATipo, AFormato);
end;

destructor TLibPIXCDMaterainfractionReports.Destroy;
begin
  frejectedReports.Free;
  inherited Destroy;
end;

procedure TLibPIXCDMaterainfractionReports.Clear;
begin
  fopenReports := 0;
  fopenReportsDistinctReporters := 0;
  frejectedReports.Clear;
  fwatermark := EmptyStr;
end;

procedure TLibPIXCDMaterainfractionReports.Processar(const MaterainfractionReports: TMaterainfractionReports);
begin
  openReports := MaterainfractionReports.openReports;
  openReportsDistinctReporters := openReportsDistinctReporters;
  rejectedReports.Processar(MaterainfractionReports.rejectedReports);
  watermark := MaterainfractionReports.watermark;
end;

{ TLibPIXCDMateraspi }
constructor TLibPIXCDMateraspi.Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespMateraspi, ATipo, AFormato);
  fsettlements := TLibPIXCDMateraValues.Create(CSessaoRespMateraspiSettlements, ATipo, AFormato);
end;

destructor TLibPIXCDMateraspi.Destroy;
begin
  fsettlements.Free;
  inherited Destroy;
end;

procedure TLibPIXCDMateraspi.Clear;
begin
  fsettlements.Clear;
  fwatermark := EmptyStr;
end;

procedure TLibPIXCDMateraspi.Processar(const Materaspi: TMateraspi);
begin
  settlements.Processar(Materaspi.settlements);
  watermark := Materaspi.watermark;
end;

{ TLibPIXCDMaterapersonStatistics }
constructor TLibPIXCDMaterapersonStatistics.Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespMaterapersonStatistics, ATipo, AFormato);
  fentries := TLibPIXCDMateraentries.Create(CSessaoRespMaterapersonStatisticsEntries, ATipo, AFormato);
  ffraudMarkers := TLibPIXCDMateraFraudMarkers.Create(CSessaoRespMaterapersonStatisticsFraudMarkers, ATipo, AFormato);
  finfractionReports := TLibPIXCDMaterainfractionReports.Create(CSessaoRespMaterapersonStatisticsInfractionReports, ATipo, AFormato);
  fspi := TLibPIXCDMateraspi.Create(CSessaoRespMaterapersonStatistics, ATipo, AFormato);
end;

destructor TLibPIXCDMaterapersonStatistics.Destroy;
begin
  fentries.Free;
  ffraudMarkers.Free;
  finfractionReports.Free;
  fspi.Free;
  inherited Destroy;
end;

procedure TLibPIXCDMaterapersonStatistics.Clear;
begin
  fentries.Clear;
  ffraudMarkers.Clear;
  finfractionReports.Clear;
  fspi.Clear;
end;

procedure TLibPIXCDMaterapersonStatistics.Processar(const MaterapersonStatistics: TMaterapersonStatistics);
begin
  entries.Processar(MaterapersonStatistics.entries);
  fraudMarkers.Processar(MaterapersonStatistics.fraudMarkers);
  infractionReports.Processar(MaterapersonStatistics.infractionReports);
  spi.Processar(MaterapersonStatistics.spi);
end;

{ TLibPIXCDSolicitarDevolucaoResposta }
constructor TLibPIXCDSolicitarDevolucaoResposta.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespSolicitarDevolucaoResposta, ATipo, AFormato);
end;

destructor TLibPIXCDSolicitarDevolucaoResposta.Destroy;
begin
  inherited Destroy;
end;

procedure TLibPIXCDSolicitarDevolucaoResposta.Clear;
begin
  ftransactionId := EmptyStr;
end;

procedure TLibPIXCDSolicitarDevolucaoResposta.Processar(const SolicitarDevolucaoResposta: TMateraDevolucaoResponse);
begin
  transactionId := SolicitarDevolucaoResposta.transactionId;
end;

{ TLibPIXCDSolicitarRetiradaResposta }
constructor TLibPIXCDSolicitarRetiradaResposta.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespSolicitarRetiradaResposta, ATipo, AFormato);
end;

destructor TLibPIXCDSolicitarRetiradaResposta.Destroy;
begin
  inherited Destroy;
end;

procedure TLibPIXCDSolicitarRetiradaResposta.Clear;
begin
  ftransactionId := EmptyStr;
  fexternalIdentifier := EmptyStr;
  fstatus := mtsNone;
  freceipt := EmptyStr;
  fauthenticationCode := EmptyStr;
end;

procedure TLibPIXCDSolicitarRetiradaResposta.Processar(const SolicitarRetiradaResposta: TMateraRetiradaResponse);
begin
  transactionId := SolicitarRetiradaResposta.transactionId;
  externalIdentifier := SolicitarRetiradaResposta.externalIdentifier;
  status := SolicitarRetiradaResposta.status;
  receipt := SolicitarRetiradaResposta.receipt;
  authenticationCode := SolicitarRetiradaResposta.authenticationCode;
end;

{ TLibPIXCDQRCodeResposta }
constructor TLibPIXCDQRCodeResposta.Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespQRCodeResposta, ATipo, AFormato);
  fcouponDetails := TLibPIXCDMateraCouponDetails.Create(CSessaoRespQRCodeResposta, ATipo, AFormato);
  ffinancialStatement := TLibPIXCDMateraFinancialStatement.Create(CSessaoRespQRCodeResposta, ATipo, AFormato);
  finstantPayment := TLibPIXCDMateraInstantPaymentQRCodeResponse.Create(CSessaoRespQRCodeResposta, ATipo, AFormato);
end;

destructor TLibPIXCDQRCodeResposta.Destroy;
begin
  fcouponDetails.Free;
  ffinancialStatement.Free;
  finstantPayment.Free;
  inherited Destroy;
end;

procedure TLibPIXCDQRCodeResposta.Clear;
begin
  fboletoUrl := EmptyStr;
  fdueDate := 0;
  fcreditCardToken := EmptyStr;
  fdiscountAmount := 0;
  fexpirationDate := 0;
  fexternalIdentifier := EmptyStr;
  fpaidAmount := 0;
  frecipientDescription := EmptyStr;
  fsenderAccountId := EmptyStr;
  ftotalAmount := 0;
  ftransactionDate := 0;
  ftransactionId := EmptyStr;
  ftransactionType := EmptyStr;
  ftypeableLine := EmptyStr;
  fcouponDetails.Clear;
  ffinancialStatement.Clear;
  finstantPayment.Clear;
end;

procedure TLibPIXCDQRCodeResposta.Processar(const QRCodeResposta: TMateraQRCodeResponse);
begin
  boletoUrl := QRCodeResposta.boletoUrl;
  dueDate := QRCodeResposta.dueDate;
  creditCardToken := QRCodeResposta.creditCardToken;
  discountAmount := QRCodeResposta.discountAmount;
  expirationDate := QRCodeResposta.expirationDate;
  externalIdentifier := QRCodeResposta.externalIdentifier;
  paidAmount := QRCodeResposta.paidAmount;
  recipientDescription := QRCodeResposta.recipientDescription;
  senderAccountId := QRCodeResposta.senderAccountId;
  totalAmount := QRCodeResposta.totalAmount;
  transactionDate := QRCodeResposta.transactionDate;
  transactionId := QRCodeResposta.transactionId;
  transactionType := QRCodeResposta.transactionType;
  typeableLine := QRCodeResposta.typeableLine;
  couponDetails.Processar(QRCodeResposta.couponDetails);
  financialStatement.Processar(QRCodeResposta.financialStatement);
  instantPayment.Processar(QRCodeResposta.instantPayment);
end;

{ TLibPIXCDMateraCouponDetails }
procedure TLibPIXCDMateraCouponDetails.Clear;
begin
  fcouponId := EmptyStr;
  fdescription := EmptyStr;
  fseller := EmptyStr;
end;

procedure TLibPIXCDMateraCouponDetails.Processar(const MateraCouponDetails: TMateraCouponDetails);
begin
  couponId := MateraCouponDetails.couponId;
  description := MateraCouponDetails.description;
  seller := MateraCouponDetails.seller;
end;

{ TLibPIXCDMateraFinancialStatement }
constructor TLibPIXCDMateraFinancialStatement.Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespMateraFinancialStatement, ATipo, AFormato);
  fAuthorizationDetails := TLibPIXCDMateraAuthorizationDetails.Create(CSessaoRespMateraFinancialStatement, ATipo, AFormato);
end;

destructor TLibPIXCDMateraFinancialStatement.Destroy;
begin
  fAuthorizationDetails.Free;
  inherited Destroy;
end;

procedure TLibPIXCDMateraFinancialStatement.Clear;
begin
  fAuthorizationDetails.Clear;
  fstatus := mtsNone;
end;

procedure TLibPIXCDMateraFinancialStatement.Processar(const MateraFinancialStatement: TMateraFinancialStatement);
begin
  AuthorizationDetails.Processar(MateraFinancialStatement.AuthorizationDetails);
  status := MateraFinancialStatement.status;
end;

{ TLibPIXCDMateraAuthorizationDetails }
procedure TLibPIXCDMateraAuthorizationDetails.Clear;
begin
  fnumber := EmptyStr;
end;

procedure TLibPIXCDMateraAuthorizationDetails.Processar(const MateraAuthorizationDetails: TMateraAuthorizationDetails);
begin
  number := MateraAuthorizationDetails.number;
end;

{ TLibPIXCDMateraInstantPaymentQRCodeResponse }
constructor TLibPIXCDMateraInstantPaymentQRCodeResponse.Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRespMateraInstantPaymentQRCodeResponse, ATipo, AFormato);
  fGeneratedImage := TLibPIXCDMateraGeneratedImage.Create(CSessaoRespMateraInstantPaymentQRCodeResponse, ATipo, AFormato);
end;

destructor TLibPIXCDMateraInstantPaymentQRCodeResponse.Destroy;
begin
  fGeneratedImage.Free;
  inherited Destroy;
end;

procedure TLibPIXCDMateraInstantPaymentQRCodeResponse.Clear;
begin
  fqrcodeURL := EmptyStr;
  freference := EmptyStr;
  ftextContent := EmptyStr;
  fdynamicQrCodeType := EmptyStr;
  fGeneratedImage.Clear;
end;

procedure TLibPIXCDMateraInstantPaymentQRCodeResponse.Processar(const MateraInstantPaymentQRCodeResponse: TMateraInstantPaymentQRCodeResponse);
begin
  qrcodeURL := MateraInstantPaymentQRCodeResponse.qrcodeURL;
  reference := MateraInstantPaymentQRCodeResponse.reference;
  textContent := MateraInstantPaymentQRCodeResponse.textContent;
  dynamicQrCodeType := MateraInstantPaymentQRCodeResponse.dynamicQrCodeType;
  GeneratedImage.Processar(MateraInstantPaymentQRCodeResponse.generateImage);
end;

{ TLibPIXCDMateraGeneratedImage }
procedure TLibPIXCDMateraGeneratedImage.Clear;
begin
  factualImageWidth := 0;
  fimageContent := EmptyStr;
  fmimeType := EmptyStr;
end;

procedure TLibPIXCDMateraGeneratedImage.Processar(const MateraGeneratedImage: TMateraGeneratedImage);
begin
  actualImageWidth := MateraGeneratedImage.actualImageWidth;
  imageContent := MateraGeneratedImage.imageContent;
  mimeType := MateraGeneratedImage.mimeType;
end;

end.

