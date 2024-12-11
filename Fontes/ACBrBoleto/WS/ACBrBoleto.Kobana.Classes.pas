{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{ Colaboradores nesse arquivo: Samuel "Muka" David                             }
{                Victor H. Gonzales - Pandaaa                                  }
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
//INCLUIDO EM 11/12/2024

{$I ACBr.inc}
unit ACBrBoleto.Kobana.Classes;

interface

uses
  SysUtils,
  Classes,
  ACBRJson,
  ACBrBoleto,
  ACBrUtil.Strings,
  ACBrUtil.DateTime;

type
  TFormat = class
  public
    FDefault: string;
    FPng: string;
    FPdf: string;
    FBoleto_hibrido: string;
    FBoleto_pix: string;
    FBarcode: string;
    FEnvelope: string;
    FLetter: string;
    FLine: string;
    FRecibo: string;
  end;

  TRemittance = class
  public
    FId: Integer;
    FUid: string;
    FOur_code: string;
    FOccurrence: string;
    FRemittance_id: Integer;
    FBank_billet_id: Integer;
    FBank_billet_account_id: Integer;
    FProcessed_at: TDateTime;
    FOccurrence_detail: string;
    FCreated_at: TDateTime;
  end;

  TStringArray = Array of string;
  TFormatArray = Array of TFormat;
  TRemittanceArray = Array of TRemittance;

  TBankBillet = class(TPersistent)
  private
    FBank_billet_account_id: Integer;
    FAmount: Currency;
    FExpire_at: TDateTime;
    FCustomer_person_name: string;
    FCustomer_cnpj_cpf: string;
    FCustomer_state: string;
    FCustomer_city_name: string;
    FCustomer_zipcode: string;
    FCustomer_address: string;
    FCustomer_address_complement: string;
    FCustomer_address_number: string;
    FCustomer_email: string;
    FCustomer_email_cc: string;
    FCustomer_neighborhood: string;
    FInterest_type: Integer;
    FInterest_days_type: Integer;
    FFine_type: Integer;
    FDiscount_type: Integer;
    FCharge_type: Integer;
    FDispatch_type: Integer;
    FDocument_type: string;
    FAcceptance: string;
    FOur_number: string;
    FPrevent_registration: Boolean;
    FIgnore_email: Boolean;
    FIgnore_sms: Boolean;
    FIgnore_whatsapp: Boolean;
    FPix_txid: string;
    FPrevent_pix: Boolean;
    FInstructions_mode: Integer;
    FInterest_Percentage: Double;
    FInstructions: string;
    FDocument_date: TDateTime;
    FDocument_number: string;
  published
    property bank_billet_account_id: Integer read FBank_billet_account_id write FBank_billet_account_id;
    property amount: Currency read FAmount write FAmount;
    property expire_at: TDateTime read FExpire_at write FExpire_at;
    property customer_person_name: string read FCustomer_person_name write FCustomer_person_name;
    property customer_cnpj_cpf: string read FCustomer_cnpj_cpf write FCustomer_cnpj_cpf;
    property customer_state: string read FCustomer_state write FCustomer_state;
    property customer_city_name: string read FCustomer_city_name write FCustomer_city_name;
    property customer_zipcode: string read FCustomer_zipcode write FCustomer_zipcode;
    property customer_address: string read FCustomer_address write FCustomer_address;
    property customer_address_complement: string read FCustomer_address_complement write FCustomer_address_complement;
    property customer_address_number: string read FCustomer_address_number write FCustomer_address_number;
    property customer_email: string read FCustomer_email write FCustomer_email;
    property customer_email_cc: string read FCustomer_email_cc write FCustomer_email_cc;
    property customer_neighborhood: string read FCustomer_neighborhood write FCustomer_neighborhood;
    property interest_type: Integer read FInterest_type write FInterest_type;
    property interest_days_type: Integer read FInterest_days_type write FInterest_days_type;
    property fine_type: Integer read FFine_type write FFine_type;
    property discount_type: Integer read FDiscount_type write FDiscount_type;
    property charge_type: Integer read FCharge_type write FCharge_type;
    property dispatch_type: Integer read FDispatch_type write FDispatch_type;
    property document_type: string read FDocument_type write FDocument_type;
    property acceptance: string read FAcceptance write FAcceptance;
    property our_number: string read FOur_number write FOur_number;
    property prevent_registration: Boolean read FPrevent_registration write FPrevent_registration;
    property ignore_email: Boolean read FIgnore_email write FIgnore_email;
    property ignore_sms: Boolean read FIgnore_sms write FIgnore_sms;
    property ignore_whatsapp: Boolean read FIgnore_whatsapp write FIgnore_whatsapp;
    property pix_txid: string read fPix_txid write fPix_txid;
    property prevent_pix: Boolean read FPrevent_pix write FPrevent_pix;
    property instructions_mode: Integer read FInstructions_mode write FInstructions_mode;
    property interest_Percentage: Double read FInterest_Percentage write FInterest_Percentage;
    property instructions: string read FInstructions write FInstructions;
    property document_date: TDateTime read FDocument_date write FDocument_date;
    property document_number: string read FDocument_number write FDocument_number;
    function ToJsonString: string;
  end;

  TParamsBankBillet = class
  private
    FBank_billet_account_id: string;
    fmeta: string;
    fstatus: string;
    FOur_number: string;
    fprocessed_our_number_raw: string;
    fcnpj_cpf: string;
    fcreated_from: string;
    fcreated_to: string;
    fexpire_from: string;
    fexpire_to: string;
    fpaid_from: string;
    fpaid_to: string;
    fregistered_to: string;
    fregistered_from: string;
    fpage: Integer;
    fper_page: Integer;
  public
    property Bank_billet_account_id: string read FBank_billet_account_id write FBank_billet_account_id;
    property Meta: string read fmeta write fmeta;
    property Status: string read fstatus write fstatus;
    property Our_number: string read FOur_number write FOur_number;
    property Processed_our_number_raw: string read fprocessed_our_number_raw write fprocessed_our_number_raw;
    property Cnpj_cpf: string read fcnpj_cpf write fcnpj_cpf;
    property Created_from: string read fcreated_from write fcreated_from;
    property Created_to: string read fcreated_to write fcreated_to;
    property Expire_from: string read fexpire_from write fexpire_from;
    property Expire_to: string read fexpire_to write fexpire_to;
    property Paid_from: string read fpaid_from write fpaid_from;
    property Paid_to: string read fpaid_to write fpaid_to;
    property Registered_to: string read fregistered_to write fregistered_to;
    property Registered_from: string read fregistered_from write fregistered_from;
    property Page: Integer read fpage write fpage;
    property Per_page: Integer read fper_page write fper_page;
  end;

  TBankBilletRetJSON = class
  private
    FId: Integer;
    FUid: string;
    FExpire_at: TDateTime;
    FPaid_at: TDateTime;
    FDescription: string;
    fstatus: string;
    FRegistration_status: string;
    FCustomer_person_name: string;
    FCustomer_cnpj_cpf: string;
    FCustomer_address: string;
    FCustomer_state: string;
    FCustomer_neighborhood: string;
    FCustomer_zipcode: string;
    FCustomer_address_number: string;
    FCustomer_address_complement: string;
    FCustomer_phone_number: string;
    FCustomer_email: string;
    FCustomer_email_cc: string;
    FCustomer_ignore_email: Boolean;
    FCustomer_ignore_sms: Boolean;
    FCustomer_mobile_local_code: Integer;
    FCustomer_mobile_number: Integer;
    FCustomer_nickname: string;
    FCustomer_notes: string;
    FCreated_via_api: Boolean;
    FCustomer_city_name: string;
    FPaid_amount: Currency;
    FAmount: Currency;
    FUrl: string;
    FFormat: TFormatArray;
    fmeta: string;
    FFine_for_delay: Currency;
    FFine_type: Integer;
    FFine_percentage: Currency;
    FFine_value: Currency;
    FDays_for_fine: Integer;
    FLate_payment_interest: Currency;
    FInterest_type: Integer;
    FInterest_daily_value: Currency;
    FInterest_daily_percentage: Currency;
    FInterest_monthly_percentage: Currency;
    FDays_for_interest: Integer;
    FDiscount_type: Integer;
    FDiscount_limit_date: TDateTime;
    FDiscount_value: Currency;
    FDiscount_percentage: Currency;
    FDays_for_revoke: Integer;
    FNotes: string;
    FPayment_count: Integer;
    FBank_billet_account_id: Integer;
    FBeneficiary_name: string;
    FBeneficiary_cnpj_cpf: string;
    FBeneficiary_address: string;
    FBeneficiary_assignor_code: string;
    FGuarantor_name: string;
    FGuarantor_cnpj_cpf: string;
    FPayment_place: string;
    FInstructions: string;
    FDocument_date: TDateTime;
    FDocument_type: string;
    FDocument_number: string;
    FAcceptance: string;
    FProcessed_our_number: string;
    fprocessed_our_number_raw: string;
    FBank_contract_slug: string;
    FAgency_number: string;
    FAgency_digit: string;
    FAccount_number: string;
    FAccount_digit: string;
    FExtra1: string;
    FExtra1_digit: string;
    FExtra2: string;
    FExtra2_digit: string;
    FLine: string;
    FOur_number: string;
    FCustomer_subscription_id: Integer;
    FInstallment_total: Integer;
    FInstallment_number: Integer;
    FInstallment_id: Integer;
    FCarne_url: string;
    FBank_billet_layout_id: Integer;
    FCreated_at: TDateTime;
    FUpdated_at: TDateTime;
    FTags: TStringArray;
    FTag_list: string;
    FDocument_type_label: string;
    FAddons: string;
    FIgnore_email: Boolean;
    FIgnore_sms: Boolean;
    FIgnore_whatsapp: Boolean;
    FSue_code: string;
    FRevoke_code: string;
    FFirst_instruction: string;
    FSecond_instruction: string;
    FGuarantor_address_number: string;
    FGuarantor_neighborhood: string;
    FGuarantor_phone_number: string;
    FGuarantor_city_name: string;
    FGuarantor_state: string;
    FGuarantor_zipcode: string;
    FGuarantor_address: string;
    FGuarantor_address_complement: string;
    FBarcode: string;
    FRegistered_at: TDateTime;
    FPrevent_registration: Boolean;
    FCustomer_id: Integer;
    FControl_number: Integer;
    FDivergent_payment_type: Integer;
    FDivergent_payment_value_type: Integer;
    FDivergent_payment_maximum_value: Currency;
    FDivergent_payment_minimum_value: Currency;
    FDivergent_payment_maximum_percentage: Currency;
    FDivergent_payment_minimum_percentage: Currency;
    FDivergent_payment_limit: Currency;
    FDays_for_discount: Integer;
    FDays_for_second_discount: Integer;
    FSecond_discount_percentage: Currency;
    FSecond_discount_value: Currency;
    FDays_for_third_discount: Integer;
    FThird_discount_percentage: Currency;
    FThird_discount_value: Currency;
    FDays_for_sue: Integer;
    FDays_for_negativation: Integer;
    FInterest_percentage: Currency;
    FInterest_value: Currency;
    FInterest_days_type: Integer;
    FCustomer_contact_person: string;
    FCustom_attachment_name: string;
    FSplit_payment: Boolean;
    FDispatch_type: Integer;
    FCharge_type: Integer;
    FCustom_data: string;
    FIssued_at: TDateTime;
    FShorten_url: string;
    FPix_enabled: Boolean;
    FPix_qrcode: string;
    FPassword_protected_mode: Integer;
    FRevoked_at: TDateTime;
    FRecipient_account: string;
    FPix_txid: string;
    FCancel_type: Integer;
    FCancellation_reason: string;
    FPrevent_pix: Boolean;
    FReduction_amount: Currency;
    FInstructions_mode: Integer;
    FImport_id: Integer;
    FReduction_type: Integer;
    FReduction_percentage: Currency;
    FUrl_hash: string;
    FBank_billet_discharges: TStringArray;
    FBank_billet_remittances: TRemittanceArray;
    FBank_billet_payments: TStringArray;
    FBank_billet_registrations: TStringArray;
    FBank_billet_split_accounts: TStringArray;
    FPix: TStringArray;
  public
    constructor Create;
    destructor Destroy; override;
    property id: Integer read FId write FId;
    property uid: string read FUid write FUid;
    property expire_at: TDateTime read FExpire_at write FExpire_at;
    property paid_at: TDateTime read FPaid_at write FPaid_at;
    property description: string read FDescription write FDescription;
    property status: string read fstatus write fstatus;
    property registration_status: string read FRegistration_status write FRegistration_status;
    property customer_person_name: string read FCustomer_person_name write FCustomer_person_name;
    property customer_cnpj_cpf: string read FCustomer_cnpj_cpf write FCustomer_cnpj_cpf;
    property customer_address: string read FCustomer_address write FCustomer_address;
    property customer_state: string read FCustomer_state write FCustomer_state;
    property customer_neighborhood: string read FCustomer_neighborhood write FCustomer_neighborhood;
    property customer_zipcode: string read FCustomer_zipcode write FCustomer_zipcode;
    property customer_address_number: string read FCustomer_address_number write FCustomer_address_number;
    property customer_address_complement: string read FCustomer_address_complement write FCustomer_address_complement;
    property customer_phone_number: string read FCustomer_phone_number write FCustomer_phone_number;
    property customer_email: string read FCustomer_email write FCustomer_email;
    property customer_email_cc: string read FCustomer_email_cc write FCustomer_email_cc;
    property customer_ignore_email: Boolean read FCustomer_ignore_email write FCustomer_ignore_email;
    property customer_ignore_sms: Boolean read FCustomer_ignore_sms write FCustomer_ignore_sms;
    property customer_mobile_local_code: Integer read FCustomer_mobile_local_code write FCustomer_mobile_local_code;
    property customer_mobile_number: Integer read FCustomer_mobile_number write FCustomer_mobile_number;
    property customer_nickname: string read FCustomer_nickname write FCustomer_nickname;
    property customer_notes: string read FCustomer_notes write FCustomer_notes;
    property created_via_api: Boolean read FCreated_via_api write FCreated_via_api;
    property customer_city_name: string read FCustomer_city_name write FCustomer_city_name;
    property paid_amount: Currency read FPaid_amount write FPaid_amount;
    property amount: Currency read FAmount write FAmount;
    property url: string read FUrl write FUrl;
    property formats: TFormatArray read FFormat write FFormat;
    property meta: string read fmeta write fmeta;
    property fine_for_delay: Currency read FFine_for_delay write FFine_for_delay;
    property fine_type: Integer read FFine_type write FFine_type;
    property fine_percentage: Currency read FFine_percentage write FFine_percentage;
    property fine_value: Currency read FFine_value write FFine_value;
    property days_for_fine: Integer read FDays_for_fine write FDays_for_fine;
    property late_payment_interest: Currency read FLate_payment_interest write FLate_payment_interest;
    property interest_type: Integer read FInterest_type write FInterest_type;
    property interest_daily_value: Currency read FInterest_daily_value write FInterest_daily_value;
    property interest_daily_percentage: Currency read FInterest_daily_percentage write FInterest_daily_percentage;
    property interest_monthly_percentage: Currency read FInterest_monthly_percentage write FInterest_monthly_percentage;
    property days_for_interest: Integer read FDays_for_interest write FDays_for_interest;
    property discount_type: Integer read FDiscount_type write FDiscount_type;
    property discount_limit_date: TDateTime read FDiscount_limit_date write FDiscount_limit_date;
    property discount_value: Currency read FDiscount_value write FDiscount_value;
    property discount_percentage: Currency read FDiscount_percentage write FDiscount_percentage;
    property days_for_revoke: Integer read FDays_for_revoke write FDays_for_revoke;
    property notes: string read FNotes write FNotes;
    property payment_count: Integer read FPayment_count write FPayment_count;
    property bank_billet_account_id: Integer read FBank_billet_account_id write FBank_billet_account_id;
    property beneficiary_name: string read FBeneficiary_name write FBeneficiary_name;
    property beneficiary_cnpj_cpf: string read FBeneficiary_cnpj_cpf write FBeneficiary_cnpj_cpf;
    property beneficiary_address: string read FBeneficiary_address write FBeneficiary_address;
    property beneficiary_assignor_code: string read FBeneficiary_assignor_code write FBeneficiary_assignor_code;
    property guarantor_name: string read FGuarantor_name write FGuarantor_name;
    property guarantor_cnpj_cpf: string read FGuarantor_cnpj_cpf write FGuarantor_cnpj_cpf;
    property payment_place: string read FPayment_place write FPayment_place;
    property instructions: string read FInstructions write FInstructions;
    property document_date: TDateTime read FDocument_date write FDocument_date;
    property document_type: string read FDocument_type write FDocument_type;
    property document_number: string read FDocument_number write FDocument_number;
    property acceptance: string read FAcceptance write FAcceptance;
    property processed_our_number: string read FProcessed_our_number write FProcessed_our_number;
    property processed_our_number_raw: string read fprocessed_our_number_raw write fprocessed_our_number_raw;
    property bank_contract_slug: string read FBank_contract_slug write FBank_contract_slug;
    property agency_number: string read FAgency_number write FAgency_number;
    property agency_digit: string read FAgency_digit write FAgency_digit;
    property account_number: string read FAccount_number write FAccount_number;
    property account_digit: string read FAccount_digit write FAccount_digit;
    property extra1: string read FExtra1 write FExtra1;
    property extra1_digit: string read FExtra1_digit write FExtra1_digit;
    property extra2: string read FExtra2 write FExtra2;
    property extra2_digit: string read FExtra2_digit write FExtra2_digit;
    property line: string read FLine write FLine;
    property our_number: string read FOur_number write FOur_number;
    property customer_subscription_id: Integer read FCustomer_subscription_id write FCustomer_subscription_id;
    property installment_total: Integer read FInstallment_total write FInstallment_total;
    property installment_number: Integer read FInstallment_number write FInstallment_number;
    property installment_id: Integer read FInstallment_id write FInstallment_id;
    property carne_url: string read FCarne_url write FCarne_url;
    property bank_billet_layout_id: Integer read FBank_billet_layout_id write FBank_billet_layout_id;
    property created_at: TDateTime read FCreated_at write FCreated_at;
    property updated_at: TDateTime read FUpdated_at write FUpdated_at;
    property tags: TStringArray read FTags write FTags;
    property tag_list: string read FTag_list write FTag_list;
    property document_type_label: string read FDocument_type_label write FDocument_type_label;
    property addons: string read FAddons write FAddons;
    property ignore_email: Boolean read FIgnore_email write FIgnore_email;
    property ignore_sms: Boolean read FIgnore_sms write FIgnore_sms;
    property ignore_whatsapp: Boolean read FIgnore_whatsapp write FIgnore_whatsapp;
    property sue_code: string read FSue_code write FSue_code;
    property revoke_code: string read FRevoke_code write FRevoke_code;
    property first_instruction: string read FFirst_instruction write FFirst_instruction;
    property second_instruction: string read FSecond_instruction write FSecond_instruction;
    property guarantor_address_number: string read FGuarantor_address_number write FGuarantor_address_number;
    property guarantor_neighborhood: string read FGuarantor_neighborhood write FGuarantor_neighborhood;
    property guarantor_phone_number: string read FGuarantor_phone_number write FGuarantor_phone_number;
    property guarantor_city_name: string read FGuarantor_city_name write FGuarantor_city_name;
    property guarantor_state: string read FGuarantor_state write FGuarantor_state;
    property guarantor_zipcode: string read FGuarantor_zipcode write FGuarantor_zipcode;
    property guarantor_address: string read FGuarantor_address write FGuarantor_address;
    property guarantor_address_complement: string read FGuarantor_address_complement write FGuarantor_address_complement;
    property barcode: string read FBarcode write FBarcode;
    property registered_at: TDateTime read FRegistered_at write FRegistered_at;
    property prevent_registration: Boolean read FPrevent_registration write FPrevent_registration;
    property customer_id: Integer read FCustomer_id write FCustomer_id;
    property control_number: Integer read FControl_number write FControl_number;
    property divergent_payment_type: Integer read FDivergent_payment_type write FDivergent_payment_type;
    property divergent_payment_value_type: Integer read FDivergent_payment_value_type write FDivergent_payment_value_type;
    property divergent_payment_maximum_value: Currency read FDivergent_payment_maximum_value write FDivergent_payment_maximum_value;
    property divergent_payment_minimum_value: Currency read FDivergent_payment_minimum_value write FDivergent_payment_minimum_value;
    property divergent_payment_maximum_percentage: Currency read FDivergent_payment_maximum_percentage write FDivergent_payment_maximum_percentage;
    property divergent_payment_minimum_percentage: Currency read FDivergent_payment_minimum_percentage write FDivergent_payment_minimum_percentage;
    property divergent_payment_limit: Currency read FDivergent_payment_limit write FDivergent_payment_limit;
    property days_for_discount: Integer read FDays_for_discount write FDays_for_discount;
    property days_for_second_discount: Integer read FDays_for_second_discount write FDays_for_second_discount;
    property second_discount_percentage: Currency read FSecond_discount_percentage write FSecond_discount_percentage;
    property second_discount_value: Currency read FSecond_discount_value write FSecond_discount_value;
    property days_for_third_discount: Integer read FDays_for_third_discount write FDays_for_third_discount;
    property third_discount_percentage: Currency read FThird_discount_percentage write FThird_discount_percentage;
    property third_discount_value: Currency read FThird_discount_value write FThird_discount_value;
    property days_for_sue: Integer read FDays_for_sue write FDays_for_sue;
    property days_for_negativation: Integer read FDays_for_negativation write FDays_for_negativation;
    property interest_percentage: Currency read FInterest_percentage write FInterest_percentage;
    property interest_value: Currency read FInterest_value write FInterest_value;
    property interest_days_type: Integer read FInterest_days_type write FInterest_days_type;
    property customer_contact_person: string read FCustomer_contact_person write FCustomer_contact_person;
    property custom_attachment_name: string read FCustom_attachment_name write FCustom_attachment_name;
    property split_payment: Boolean read FSplit_payment write FSplit_payment;
    property dispatch_type: Integer read FDispatch_type write FDispatch_type;
    property charge_type: Integer read FCharge_type write FCharge_type;
    property custom_data: string read FCustom_data write FCustom_data;
    property issued_at: TDateTime read FIssued_at write FIssued_at;
    property shorten_url: string read FShorten_url write FShorten_url;
    property pix_enabled: Boolean read FPix_enabled write FPix_enabled;
    property pix_qrcode: string read FPix_qrcode write FPix_qrcode;
    property password_protected_mode: Integer read FPassword_protected_mode write FPassword_protected_mode;
    property revoked_at: TDateTime read FRevoked_at write FRevoked_at;
    property recipient_account: string read FRecipient_account write FRecipient_account;
    property pix_txid: string read FPix_txid write FPix_txid;
    property cancel_type: Integer read FCancel_type write FCancel_type;
    property cancellation_reason: string read FCancellation_reason write FCancellation_reason;
    property prevent_pix: Boolean read FPrevent_pix write FPrevent_pix;
    property reduction_amount: Currency read FReduction_amount write FReduction_amount;
    property instructions_mode: Integer read FInstructions_mode write FInstructions_mode;
    property import_id: Integer read FImport_id write FImport_id;
    property reduction_type: Integer read FReduction_type write FReduction_type;
    property reduction_percentage: Currency read FReduction_percentage write FReduction_percentage;
    property url_hash: string read FUrl_hash write FUrl_hash;
    property bank_billet_discharges: TStringArray read FBank_billet_discharges write FBank_billet_discharges;
    property bank_billet_remittances: TRemittanceArray read FBank_billet_remittances write FBank_billet_remittances;
    property bank_billet_payments: TStringArray read FBank_billet_payments write FBank_billet_payments;
    property bank_billet_registrations: TStringArray read FBank_billet_registrations write FBank_billet_registrations;
    property bank_billet_split_accounts: TStringArray read FBank_billet_split_accounts write FBank_billet_split_accounts;
    property pix: TStringArray read FPix write FPix;
  end;

  TBankBilletRetJSONArray = Array of TBankBilletRetJSON;

  TBankBilletRetJSONList = class(TPersistent)
  private
    FList: TBankBilletRetJSONArray;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddItem(pBankBilletRetJSON: TBankBilletRetJSON);
    property List: TBankBilletRetJSONArray read FList;
    class function FromJsonString(const AJsonString: string): TBankBilletRetJSONList;
  end;

  TRetornoCarteira = class
  private
    FUIDConta: string;
    FIDCarteira: Integer;
    FCodBanco: Integer;
    FURL: string;

    function GetUIDConta: string;
    procedure SetUIDConta(const pValor: string);
    function GetIDCarteira: Integer;
    procedure SetIDCarteira(const pValor: Integer);
    function GetCodBanco: Integer;
    procedure SetCodBanco(const pValor: Integer);
    procedure RetornarDadosDaCarteira(pBoleto: TACBrBoleto);
  public
    constructor Create(pBoleto: TACBrBoleto; const pURL: string);
    destructor Destroy; override;

    property UIDConta: string read GetUIDConta write SetUIDConta;
    property IDCarteira: Integer read GetIDCarteira write SetIDCarteira;
    property CodBanco: Integer read GetCodBanco write SetCodBanco;
    property URL: string read FURL write FURL;
  end;

  TConsultaBoleto = class
  private
    FEncontrouBoleto: Boolean;
    FURL: string;
    procedure RetornarDadosDaConsulta(const pBoleto: TACBrBoleto; const pTitulo: TACBrTitulo);
    function GetEncontrouBoleto: Boolean;
    procedure SetEncontrouBoleto(const pValue: Boolean);
  public
    constructor Create(pBoleto: TACBrBoleto; pTitulo: TACBrTitulo; const pURL: string);
    property EncontrouBoleto: Boolean read GetEncontrouBoleto write SetEncontrouBoleto;
    property URL: string read FURL write FURL;
  end;

  TSerializadorParametrosEnvioERetornoBankBilletJSON = class
  public
    function Serializar(const pBoleto: TACBrBoleto): TACBrJSONObject;
    procedure Desserializar(const pRetorno: string; out pRetornoCarteira: TRetornoCarteira);
    procedure DesserializarConsulta(const pConsulta: string; out pConsultaBoleto: TConsultaBoleto);

  end;

implementation

uses
  httpsend,
  ACBrUtil.Base;

{ TBankBilletRetJSON }

constructor TBankBilletRetJSON.Create;
begin
  inherited Create;
end;

destructor TBankBilletRetJSON.Destroy;
begin
  inherited Destroy;
end;

{ TBankBilletRetJSONList }

procedure TBankBilletRetJSONList.AddItem(pBankBilletRetJSON: TBankBilletRetJSON);
var
  LListCount: Integer;
begin
  LListCount := length(FList);
  SetLength(FList, LListCount + 1);
  FList[LListCount] := pBankBilletRetJSON;
end;

constructor TBankBilletRetJSONList.Create;
begin
  inherited Create;
end;

destructor TBankBilletRetJSONList.Destroy;
var
  lI: Integer;
begin
  for lI := 0 to pred(length(FList)) do
  begin
    FList[lI].Free;
  end;

  inherited;
end;

class function TBankBilletRetJSONList.FromJsonString(const AJsonString: string): TBankBilletRetJSONList;
var
  LACBrJSONObject: TACBrJSONObject;
  LACBrJSONArray: TACBrJSONArray;
  Li: Integer;
  LBankBilletRetJSON: TBankBilletRetJSON;
begin
  Result := TBankBilletRetJSONList.Create;
  try
    LACBrJSONArray := TACBrJSONArray.Parse(AJsonString);

    for Li := 0 to LACBrJSONArray.Count - 1 do
    begin
      LACBrJSONObject := LACBrJSONArray.ItemAsJSONObject[Li];
      LBankBilletRetJSON := TBankBilletRetJSON.Create;

      LBankBilletRetJSON.Id := LACBrJSONObject.AsInteger['id'];
      LBankBilletRetJSON.Uid := LACBrJSONObject.AsString['uid'];
      LBankBilletRetJSON.Expire_at := LACBrJSONObject.AsISODate['expire_at'];
      LBankBilletRetJSON.Paid_at := LACBrJSONObject.AsISODate['paid_at'];
      LBankBilletRetJSON.Description := LACBrJSONObject.AsString['description'];
      LBankBilletRetJSON.Status := LACBrJSONObject.AsString['status'];
      LBankBilletRetJSON.Registration_status := LACBrJSONObject.AsString['registration_status'];
      LBankBilletRetJSON.Customer_person_name := LACBrJSONObject.AsString['customer_person_name'];
      LBankBilletRetJSON.Customer_cnpj_cpf := LACBrJSONObject.AsString['customer_cnpj_cpf'];
      LBankBilletRetJSON.Customer_address := LACBrJSONObject.AsString['customer_address'];
      LBankBilletRetJSON.Customer_state := LACBrJSONObject.AsString['customer_state'];
      LBankBilletRetJSON.Customer_neighborhood := LACBrJSONObject.AsString['customer_neighborhood'];
      LBankBilletRetJSON.Customer_zipcode := LACBrJSONObject.AsString['customer_zipcode'];
      LBankBilletRetJSON.Customer_address_number := LACBrJSONObject.AsString['customer_address_number'];
      LBankBilletRetJSON.Customer_address_complement := LACBrJSONObject.AsString['customer_address_complement'];
      LBankBilletRetJSON.Customer_phone_number := LACBrJSONObject.AsString['customer_phone_number'];
      LBankBilletRetJSON.Customer_email := LACBrJSONObject.AsString['customer_email'];
      LBankBilletRetJSON.Customer_email_cc := LACBrJSONObject.AsString['customer_email_cc'];
      LBankBilletRetJSON.Customer_ignore_email := LACBrJSONObject.AsBoolean['customer_ignore_email'];
      LBankBilletRetJSON.Customer_ignore_sms := LACBrJSONObject.AsBoolean['customer_ignore_sms'];
      LBankBilletRetJSON.Customer_mobile_local_code := LACBrJSONObject.AsInteger['customer_mobile_local_code'];
      LBankBilletRetJSON.Customer_mobile_number := LACBrJSONObject.AsInteger['customer_mobile_number'];
      LBankBilletRetJSON.Customer_nickname := LACBrJSONObject.AsString['customer_nickname'];
      LBankBilletRetJSON.Customer_notes := LACBrJSONObject.AsString['customer_notes'];
      LBankBilletRetJSON.Created_via_api := LACBrJSONObject.AsBoolean['created_via_api'];
      LBankBilletRetJSON.Customer_city_name := LACBrJSONObject.AsString['customer_city_name'];
      LBankBilletRetJSON.Paid_amount := LACBrJSONObject.AsCurrency['paid_amount'];
      LBankBilletRetJSON.Amount := LACBrJSONObject.AsCurrency['amount'];
      LBankBilletRetJSON.Url := LACBrJSONObject.AsString['url'];
      LBankBilletRetJSON.Meta := LACBrJSONObject.AsString['meta'];
      LBankBilletRetJSON.Fine_for_delay := LACBrJSONObject.AsCurrency['fine_for_delay'];
      LBankBilletRetJSON.Fine_type := LACBrJSONObject.AsInteger['fine_type'];
      LBankBilletRetJSON.Fine_percentage := LACBrJSONObject.AsCurrency['fine_percentage'];
      LBankBilletRetJSON.Fine_value := LACBrJSONObject.AsCurrency['fine_value'];
      LBankBilletRetJSON.Days_for_fine := LACBrJSONObject.AsInteger['days_for_fine'];
      LBankBilletRetJSON.Late_payment_interest := LACBrJSONObject.AsCurrency['late_payment_interest'];
      LBankBilletRetJSON.Interest_type := LACBrJSONObject.AsInteger['interest_type'];
      LBankBilletRetJSON.Interest_daily_value := LACBrJSONObject.AsCurrency['interest_daily_value'];
      LBankBilletRetJSON.Interest_daily_percentage := LACBrJSONObject.AsCurrency['interest_daily_percentage'];
      LBankBilletRetJSON.Interest_monthly_percentage := LACBrJSONObject.AsCurrency['interest_monthly_percentage'];
      LBankBilletRetJSON.Days_for_interest := LACBrJSONObject.AsInteger['days_for_interest'];
      LBankBilletRetJSON.Discount_type := LACBrJSONObject.AsInteger['discount_type'];
      LBankBilletRetJSON.Discount_limit_date := LACBrJSONObject.AsISODate['discount_limit_date'];
      LBankBilletRetJSON.Discount_value := LACBrJSONObject.AsCurrency['discount_value'];
      LBankBilletRetJSON.Discount_percentage := LACBrJSONObject.AsCurrency['discount_percentage'];
      LBankBilletRetJSON.Days_for_revoke := LACBrJSONObject.AsInteger['days_for_revoke'];
      LBankBilletRetJSON.Notes := LACBrJSONObject.AsString['notes'];
      LBankBilletRetJSON.Payment_count := LACBrJSONObject.AsInteger['payment_count'];
      LBankBilletRetJSON.Bank_billet_account_id := LACBrJSONObject.AsInteger['bank_billet_account_id'];
      LBankBilletRetJSON.Beneficiary_name := LACBrJSONObject.AsString['beneficiary_name'];
      LBankBilletRetJSON.Beneficiary_cnpj_cpf := LACBrJSONObject.AsString['beneficiary_cnpj_cpf'];
      LBankBilletRetJSON.Beneficiary_address := LACBrJSONObject.AsString['beneficiary_address'];
      LBankBilletRetJSON.Beneficiary_assignor_code := LACBrJSONObject.AsString['beneficiary_assignor_code'];
      LBankBilletRetJSON.Guarantor_name := LACBrJSONObject.AsString['guarantor_name'];
      LBankBilletRetJSON.Guarantor_cnpj_cpf := LACBrJSONObject.AsString['guarantor_cnpj_cpf'];
      LBankBilletRetJSON.Payment_place := LACBrJSONObject.AsString['payment_place'];
      LBankBilletRetJSON.Instructions := LACBrJSONObject.AsString['instructions'];
      LBankBilletRetJSON.Document_date := LACBrJSONObject.AsISODate['document_date'];
      LBankBilletRetJSON.Document_type := LACBrJSONObject.AsString['document_type'];
      LBankBilletRetJSON.Document_number := LACBrJSONObject.AsString['document_number'];
      LBankBilletRetJSON.Acceptance := LACBrJSONObject.AsString['acceptance'];
      LBankBilletRetJSON.Processed_our_number := LACBrJSONObject.AsString['processed_our_number'];
      LBankBilletRetJSON.Processed_our_number_raw := LACBrJSONObject.AsString['processed_our_number_raw'];
      LBankBilletRetJSON.Bank_contract_slug := LACBrJSONObject.AsString['bank_contract_slug'];
      LBankBilletRetJSON.Agency_number := LACBrJSONObject.AsString['agency_number'];
      LBankBilletRetJSON.Agency_digit := LACBrJSONObject.AsString['agency_digit'];
      LBankBilletRetJSON.Account_number := LACBrJSONObject.AsString['account_number'];
      LBankBilletRetJSON.Account_digit := LACBrJSONObject.AsString['account_digit'];

      Result.AddItem(LBankBilletRetJSON);
    end;
  finally
    LACBrJSONArray.Free;
  end;
end;

{ TSerializadorParametrosEnvioERetornoBankBilletJSON }

procedure TSerializadorParametrosEnvioERetornoBankBilletJSON.Desserializar(const pRetorno: string; out pRetornoCarteira: TRetornoCarteira);
var
  lJSONRetorno: TACBrJSONObject;
  lData: TACBrJSONObject;
begin
  lJSONRetorno := TACBrJSONObject.Parse(pRetorno) as TACBrJSONObject;
  try
    lData := lJSONRetorno.AsJSONObject['data'];
    pRetornoCarteira.IDCarteira := lData.AsInteger['bank_billet_account_id'];
    pRetornoCarteira.UIDConta := lData.AsString['account_uid'];
  finally
    lJSONRetorno.Free;
  end;
end;

function TSerializadorParametrosEnvioERetornoBankBilletJSON.Serializar(const pBoleto: TACBrBoleto): TACBrJSONObject;
var
  lJSONItem: TACBrJSONObject;
  lChavePix: string;
  lJSONPix: TACBrJSONObject;
begin
  Result := TACBrJSONObject.Create;
  lJSONItem := TACBrJSONObject.Create;

  lJSONItem.AddPair('Banco', pBoleto.Banco.BancoClass.Numero);
  lJSONItem.AddPair('Nome', pBoleto.Cedente.Nome);
  lJSONItem.AddPair('FantasiaCedente', pBoleto.Cedente.FantasiaCedente);
  lJSONItem.AddPair('CodigoCedente', pBoleto.Cedente.CodigoCedente);
  lJSONItem.AddPair('CodigoTransmissao', pBoleto.Cedente.CodigoTransmissao);
  lJSONItem.AddPair('Agencia', pBoleto.Cedente.Agencia);
  lJSONItem.AddPair('AgenciaDigito', pBoleto.Cedente.AgenciaDigito);
  lJSONItem.AddPair('Conta', pBoleto.Cedente.Conta);
  lJSONItem.AddPair('ContaDigito', pBoleto.Cedente.ContaDigito);
  lJSONItem.AddPair('Modalidade', pBoleto.Cedente.Modalidade);
  lJSONItem.AddPair('Convenio', pBoleto.Cedente.Convenio);
  lJSONItem.AddPair('TipoDocumento', Integer(pBoleto.Cedente.TipoDocumento));
  lJSONItem.AddPair('TipoCarteira', Integer(pBoleto.Cedente.TipoCarteira));
  lJSONItem.AddPair('ResponEmissao', Integer(pBoleto.Cedente.ResponEmissao));
  lJSONItem.AddPair('CaracTitulo', Integer(pBoleto.Cedente.CaracTitulo));
  lJSONItem.AddPair('CNPJCPF', pBoleto.Cedente.CNPJCPF);
  lJSONItem.AddPair('TipoInscricao', Integer(pBoleto.Cedente.TipoInscricao));
  lJSONItem.AddPair('Logradouro', pBoleto.Cedente.Logradouro);
  lJSONItem.AddPair('NumeroRes', pBoleto.Cedente.NumeroRes);
  lJSONItem.AddPair('Complemento', pBoleto.Cedente.Complemento);
  lJSONItem.AddPair('Bairro', pBoleto.Cedente.Bairro);
  lJSONItem.AddPair('Cidade', pBoleto.Cedente.Cidade);
  lJSONItem.AddPair('UF', pBoleto.Cedente.UF);
  lJSONItem.AddPair('CEP', pBoleto.Cedente.CEP);
  lJSONItem.AddPair('Telefone', pBoleto.Cedente.Telefone);
  lJSONItem.AddPair('DigitoVerificadorAgenciaConta', pBoleto.Cedente.DigitoVerificadorAgenciaConta);
  lJSONItem.AddPair('IdentDistribuicao', Ord(pBoleto.Cedente.IdentDistribuicao));
  lJSONItem.AddPair('Operacao', pBoleto.Cedente.Operacao);

  lChavePix := pBoleto.Cedente.Pix.Chave;
  if NaoEstaVazio(lChavePix) then
  begin
    lJSONPix := TACBrJSONObject.Create;
    lJSONPix.AddPair('TipoChavePIX', Integer(pBoleto.Cedente.Pix.TipoChavePIX));
    lJSONPix.AddPair('Chave', pBoleto.Cedente.Pix.Chave);
    lJSONItem.AddPair('PIX', lJSONPix);
  end;

  Result.AddPair('payload', lJSONItem);
end;


procedure TSerializadorParametrosEnvioERetornoBankBilletJSON.DesserializarConsulta(const pConsulta: string; out pConsultaBoleto: TConsultaBoleto);
var
  lJSONRetorno: TACBrJSONArray;
  lJSONItem: TACBrJSONObject;
  lNossoNumero: string;
begin
  lJSONRetorno := TACBrJSONArray.Parse(pConsulta) as TACBrJSONArray;
  try
    if lJSONRetorno.Count > 0 then
    begin
      lJSONItem := lJSONRetorno.ItemAsJSONObject[0] as TACBrJSONObject;
      lNossoNumero := lJSONItem.AsString['processed_our_number_raw'];
      pConsultaBoleto.FEncontrouBoleto := True;
    end else
      pConsultaBoleto.FEncontrouBoleto := False;
  finally
    lJSONRetorno.Free;
  end;
end;

{ TRetornoCarteira }

constructor TRetornoCarteira.Create(pBoleto: TACBrBoleto; const pURL: string);
begin
  FURL := pURL;
  RetornarDadosDaCarteira(pBoleto);
end;

destructor TRetornoCarteira.Destroy;
begin
  inherited;
end;

procedure TRetornoCarteira.RetornarDadosDaCarteira(pBoleto: TACBrBoleto);
var
  lSerializador: TSerializadorParametrosEnvioERetornoBankBilletJSON;
  lJSONBody: TACBrJSONObject;
  lHTTPSend: THTTPSend;
  lStream: TStringStream;
  lStringStream: TStringStream;
  lUrl,lJson: string;
begin
  lSerializador := TSerializadorParametrosEnvioERetornoBankBilletJSON.Create;
  try
    lJSONBody := lSerializador.Serializar(pBoleto);
    lHTTPSend := THTTPSend.Create;
    try
      lStream := TStringStream.Create('');
      try
        lHTTPSend.OutputStream := lStream;

        lHTTPSend.Headers.Add('authorization: Bearer ' + pBoleto.Cedente.CedenteWS.ClientSecret);
        lHTTPSend.MimeType := 'application/json';

        lJson := lJSONBody.ToJSON;
        lStringStream := TStringStream.Create(UTF8ToNativeString(lJSONBody.ToJSON));
        try
          lHTTPSend.Document.LoadFromStream(lStringStream);
        finally
          lStringStream.Free;
        end;

        lUrl := FURL + '/v2/acbr/accounts/find_or_create';
        lHTTPSend.HTTPMethod('POST', lUrl);

        lStream.Position := 0;

        lSerializador.Desserializar(lStream.DataString, Self);
      finally
        lStream.Free;
      end;
    finally
      lHTTPSend.Free
    end;
  finally
    lSerializador.Free;
  end;
end;

function TRetornoCarteira.GetCodBanco: Integer;
begin
  Result := FCodBanco;
end;

function TRetornoCarteira.GetIDCarteira: Integer;
begin
  Result := FIDCarteira;
end;

function TRetornoCarteira.GetUIDConta: string;
begin
  Result := FUIDConta;
end;

procedure TRetornoCarteira.SetCodBanco(const pValor: Integer);
begin
  FCodBanco := pValor;
end;

procedure TRetornoCarteira.SetIDCarteira(const pValor: Integer);
begin
  FIDCarteira := pValor;
end;

procedure TRetornoCarteira.SetUIDConta(const pValor: string);
begin
  FUIDConta := pValor;
end;

{ TBankBillet }

function TBankBillet.ToJsonString: string;
var
  LJSON: TACBrJSONObject;
begin
  LJSON := TACBrJSONObject.Create;

  try
    LJSON.AddPair('bank_billet_account_id',bank_billet_account_id);
    LJSON.AddPair('amount', amount);
    LJSON.AddPair('expire_at',  FormatDateBr(expire_at, 'YYYY-MM-DD'));
    LJSON.AddPair('customer_person_name',customer_person_name);
    LJSON.AddPair('customer_cnpj_cpf',customer_cnpj_cpf);
    LJSON.AddPair('customer_state',customer_state);
    LJSON.AddPair('customer_city_name',customer_city_name);
    LJSON.AddPair('customer_zipcode',customer_zipcode);
    LJSON.AddPair('customer_address',customer_address);
    LJSON.AddPair('customer_address_complement',customer_address_complement);
    LJSON.AddPair('customer_address_number',customer_address_number);
    LJSON.AddPair('customer_email',customer_email);
    LJSON.AddPair('customer_email_cc',customer_email_cc);
    LJSON.AddPair('customer_neighborhood',customer_neighborhood);
    LJSON.AddPair('interest_type',interest_type);
    LJSON.AddPair('interest_days_type',interest_days_type);
    LJSON.AddPair('fine_type',fine_type);
    LJSON.AddPair('discount_type',discount_type);
    LJSON.AddPair('charge_type',charge_type);
    LJSON.AddPair('dispatch_type',dispatch_type);
    LJSON.AddPair('document_type',document_type);
    LJSON.AddPair('acceptance',acceptance);
    LJSON.AddPair('our_number',our_number);
    LJSON.AddPair('prevent_registration',prevent_registration);
    LJSON.AddPair('ignore_email',ignore_email);
    LJSON.AddPair('ignore_sms',ignore_sms);
    LJSON.AddPair('ignore_whatsapp',ignore_whatsapp);
    LJSON.AddPair('pix_txid',pix_txid);
    LJSON.AddPair('prevent_pix',prevent_pix);
    LJSON.AddPair('instructions_mode',instructions_mode);
    LJSON.AddPair('interest_Percentage',interest_Percentage);
    LJSON.AddPair('instructions',instructions);
    LJSON.AddPair('document_date', FormatDateBr(document_date, 'YYYY-MM-DD'));
    LJSON.AddPair('document_number',document_number);

    Result := LJSON.ToJSON;
  finally
    LJSON.Free;
  end;

end;


{ TConsultaBoleto }

constructor TConsultaBoleto.Create(pBoleto: TACBrBoleto; pTitulo: TACBrTitulo; const pURL: string);
begin
  FURL := pURL;
  RetornarDadosDaConsulta(pBoleto, pTitulo);
end;

function TConsultaBoleto.GetEncontrouBoleto: Boolean;
begin
  Result := FEncontrouBoleto;
end;

procedure TConsultaBoleto.RetornarDadosDaConsulta(const pBoleto: TACBrBoleto; const pTitulo: TACBrTitulo);
var
  lNossoNumeroParam: string;
  lSerealizador: TSerializadorParametrosEnvioERetornoBankBilletJSON;
  lHTTPSend: THTTPSend;
  lStream: TStringStream;
  lUrl: string;
begin
  lNossoNumeroParam := pTitulo.NossoNumero;
  lNossoNumeroParam := pBoleto.Banco.MontarCampoNossoNumero(pTitulo);
  lNossoNumeroParam := OnlyAlphaNum(Trim(lNossoNumeroParam));

  lSerealizador := TSerializadorParametrosEnvioERetornoBankBilletJSON.Create;
  try
    lHTTPSend := THTTPSend.Create;
    try
      lStream := TStringStream.Create('');
      try
        lHTTPSend.OutputStream := lStream;

        lHTTPSend.Headers.Add('authorization: Bearer ' + pBoleto.Cedente.CedenteWS.ClientSecret);
        lHTTPSend.MimeType := 'application/json';

        lUrl :=
          FURL + '/v1/bank_billets?bank_billet_account_id=' + pBoleto.Cedente.CedenteWS.KeyUser +
          '&' + 'processed_our_number_raw=' + lNossoNumeroParam;

        lHTTPSend.HTTPMethod('GET', lUrl);

        lStream.Position := 0;

        lSerealizador.DesserializarConsulta(lStream.DataString, Self);
      finally
        lStream.Free;
      end;
    finally
      lHTTPSend.Free
    end;
  finally
    lSerealizador.Free;
  end;
end;

procedure TConsultaBoleto.SetEncontrouBoleto(const pValue: Boolean);
begin
  FEncontrouBoleto := pValue;
end;

end.
