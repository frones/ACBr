{*****************************************************************}
{                                                                 }
{            CodeGear Delphi Runtime Library                      }
{            NCrypt.pas interface unit                            }
{                                                                 }
{            Coypright (c) 2007,2008 Rudy Velthuis                }
{            Converted 13 Oct 2007 Rudy Velthuis                  }
{            Last modified 07 Mar 2008 Rudy Velthuis              }
{                                                                 }
{            Cryptographic API Prototypes and Definitions         }
{                                                                 }
{*****************************************************************}

{*****************************************************************}
{                                                                 }
{ The contents of this file are subject to the Mozilla Public     }
{ License Version 1.1 (the "License"). You may not use this file  }
{ except in compliance with the License. You may obtain a copy of }
{ the License at http://www.mozilla.org/MPL/MPL-1.1.html          }
{                                                                 }
{ Software distributed under the License is distributed on an     }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or  }
{ implied. See the License for the specific language governing    }
{ rights and limitations under the License.                       }
{                                                                 }
{ The original code is: ncrypt.h, released 1992-1999.             }
{                                                                 }
{ The initial developer of the original translation is            }
{ Rudy Velthuis (articles@rvelthuis.de).                          }
{                                                                 }
{ Portions created by Rudy Velthuis are                           }
{ Copyright (C) 2007,2008 Rudy Velthuis.                          }
{                                                                 }
{ Portions created by Microsoft Corporation are                   }
{ Copyright (C) 1992-1999 Microsoft Corporation.                  }
{                                                                 }
{ All Rights Reserved.                                            }
{                                                                 }
{*****************************************************************}

{ 19/04/2017
  - Refactoring to allow Dynamic Loading from DLL, to avoid problems
    on Windows XP (by DSA)
}

{$I ACBr.inc}

unit ACBr_NCrypt;

{$ALIGN 8}

interface

uses
  Windows, Types, ACBr_BCrypt
  {$IfDef FPC}
   ,dynlibs
  {$EndIf};

{$HPPEMIT '#include <ncrypt.h>'}

type
  {$EXTERNALSYM SECURITY_STATUS}
  SECURITY_STATUS = Longint;

  {$IfNDef FPC}
    LPVOID = Pointer;
    SizeUInt = {$IFDEF COMPILER16_UP} NativeUInt {$ELSE} Longword {$ENDIF};
    {$IfNDef COMPILER12_UP}
     ULONG_PTR = SizeUInt;
    {$EndIf}
  {$EndIf}

//
// Microsoft built-in providers.
//

const
  {$EXTERNALSYM MS_KEY_STORAGE_PROVIDER}
  MS_KEY_STORAGE_PROVIDER            = 'Microsoft Software Key Storage Provider';
  {$EXTERNALSYM MS_SMART_CARD_KEY_STORAGE_PROVIDER}
  MS_SMART_CARD_KEY_STORAGE_PROVIDER = 'Microsoft Smart Card Key Storage Provider';

//
// Common algorithm identifiers.
//

  {$EXTERNALSYM NCRYPT_RSA_ALGORITHM}
  NCRYPT_RSA_ALGORITHM            = BCRYPT_RSA_ALGORITHM;
  {$EXTERNALSYM NCRYPT_RSA_SIGN_ALGORITHM}
  NCRYPT_RSA_SIGN_ALGORITHM       = BCRYPT_RSA_SIGN_ALGORITHM;
  {$EXTERNALSYM NCRYPT_DH_ALGORITHM}
  NCRYPT_DH_ALGORITHM             = BCRYPT_DH_ALGORITHM;
  {$EXTERNALSYM NCRYPT_DSA_ALGORITHM}
  NCRYPT_DSA_ALGORITHM            = BCRYPT_DSA_ALGORITHM;
  {$EXTERNALSYM NCRYPT_MD2_ALGORITHM}
  NCRYPT_MD2_ALGORITHM            = BCRYPT_MD2_ALGORITHM;
  {$EXTERNALSYM NCRYPT_MD4_ALGORITHM}
  NCRYPT_MD4_ALGORITHM            = BCRYPT_MD4_ALGORITHM;
  {$EXTERNALSYM NCRYPT_MD5_ALGORITHM}
  NCRYPT_MD5_ALGORITHM            = BCRYPT_MD5_ALGORITHM;
  {$EXTERNALSYM NCRYPT_SHA1_ALGORITHM}
  NCRYPT_SHA1_ALGORITHM           = BCRYPT_SHA1_ALGORITHM;
  {$EXTERNALSYM NCRYPT_SHA256_ALGORITHM}
  NCRYPT_SHA256_ALGORITHM         = BCRYPT_SHA256_ALGORITHM;
  {$EXTERNALSYM NCRYPT_SHA384_ALGORITHM}
  NCRYPT_SHA384_ALGORITHM         = BCRYPT_SHA384_ALGORITHM;
  {$EXTERNALSYM NCRYPT_SHA512_ALGORITHM}
  NCRYPT_SHA512_ALGORITHM         = BCRYPT_SHA512_ALGORITHM;
  {$EXTERNALSYM NCRYPT_ECDSA_P256_ALGORITHM}
  NCRYPT_ECDSA_P256_ALGORITHM     = BCRYPT_ECDSA_P256_ALGORITHM;
  {$EXTERNALSYM NCRYPT_ECDSA_P384_ALGORITHM}
  NCRYPT_ECDSA_P384_ALGORITHM     = BCRYPT_ECDSA_P384_ALGORITHM;
  {$EXTERNALSYM NCRYPT_ECDSA_P521_ALGORITHM}
  NCRYPT_ECDSA_P521_ALGORITHM     = BCRYPT_ECDSA_P521_ALGORITHM;
  {$EXTERNALSYM NCRYPT_ECDH_P256_ALGORITHM}
  NCRYPT_ECDH_P256_ALGORITHM      = BCRYPT_ECDH_P256_ALGORITHM;
  {$EXTERNALSYM NCRYPT_ECDH_P384_ALGORITHM}
  NCRYPT_ECDH_P384_ALGORITHM      = BCRYPT_ECDH_P384_ALGORITHM;
  {$EXTERNALSYM NCRYPT_ECDH_P521_ALGORITHM}
  NCRYPT_ECDH_P521_ALGORITHM      = BCRYPT_ECDH_P521_ALGORITHM;

  {$EXTERNALSYM NCRYPT_KEY_STORAGE_ALGORITHM}
  NCRYPT_KEY_STORAGE_ALGORITHM    = 'KEY_STORAGE';

//
// Interfaces
//

  {$EXTERNALSYM NCRYPT_HASH_INTERFACE}
  NCRYPT_HASH_INTERFACE                   = BCRYPT_HASH_INTERFACE;
  {$EXTERNALSYM NCRYPT_ASYMMETRIC_ENCRYPTION_INTERFACE}
  NCRYPT_ASYMMETRIC_ENCRYPTION_INTERFACE  = BCRYPT_ASYMMETRIC_ENCRYPTION_INTERFACE;

  {$EXTERNALSYM NCRYPT_SECRET_AGREEMENT_INTERFACE}
  NCRYPT_SECRET_AGREEMENT_INTERFACE       = BCRYPT_SECRET_AGREEMENT_INTERFACE;

  {$EXTERNALSYM NCRYPT_SIGNATURE_INTERFACE}
  NCRYPT_SIGNATURE_INTERFACE              = BCRYPT_SIGNATURE_INTERFACE;

  {$EXTERNALSYM NCRYPT_KEY_STORAGE_INTERFACE}
  NCRYPT_KEY_STORAGE_INTERFACE            = $00010001;
  {$EXTERNALSYM NCRYPT_SCHANNEL_INTERFACE}
  NCRYPT_SCHANNEL_INTERFACE               = $00010002;

//
// algorithm groups.
//

  {$EXTERNALSYM NCRYPT_RSA_ALGORITHM_GROUP}
  NCRYPT_RSA_ALGORITHM_GROUP      = NCRYPT_RSA_ALGORITHM;
  {$EXTERNALSYM NCRYPT_DH_ALGORITHM_GROUP}
  NCRYPT_DH_ALGORITHM_GROUP       = NCRYPT_DH_ALGORITHM;
  {$EXTERNALSYM NCRYPT_DSA_ALGORITHM_GROUP}
  NCRYPT_DSA_ALGORITHM_GROUP      = NCRYPT_DSA_ALGORITHM;
  {$EXTERNALSYM NCRYPT_ECDSA_ALGORITHM_GROUP}
  NCRYPT_ECDSA_ALGORITHM_GROUP    = 'ECDSA';
  {$EXTERNALSYM NCRYPT_ECDH_ALGORITHM_GROUP}
  NCRYPT_ECDH_ALGORITHM_GROUP     = 'ECDH';

//
// NCrypt generic memory descriptors
//

  {$EXTERNALSYM NCRYPTBUFFER_VERSION}
  NCRYPTBUFFER_VERSION                = 0;

  {$EXTERNALSYM NCRYPTBUFFER_EMPTY}
  NCRYPTBUFFER_EMPTY                  = 0;
  {$EXTERNALSYM NCRYPTBUFFER_DATA}
  NCRYPTBUFFER_DATA                   = 1;
  {$EXTERNALSYM NCRYPTBUFFER_SSL_CLIENT_RANDOM}
  NCRYPTBUFFER_SSL_CLIENT_RANDOM      = 20;
  {$EXTERNALSYM NCRYPTBUFFER_SSL_SERVER_RANDOM}
  NCRYPTBUFFER_SSL_SERVER_RANDOM      = 21;
  {$EXTERNALSYM NCRYPTBUFFER_SSL_HIGHEST_VERSION}
  NCRYPTBUFFER_SSL_HIGHEST_VERSION    = 22;
  {$EXTERNALSYM NCRYPTBUFFER_SSL_CLEAR_KEY}
  NCRYPTBUFFER_SSL_CLEAR_KEY          = 23;
  {$EXTERNALSYM NCRYPTBUFFER_SSL_KEY_ARG_DATA}
  NCRYPTBUFFER_SSL_KEY_ARG_DATA       = 24;

  {$EXTERNALSYM NCRYPTBUFFER_PKCS_OID}
  NCRYPTBUFFER_PKCS_OID               = 40;
  {$EXTERNALSYM NCRYPTBUFFER_PKCS_ALG_OID}
  NCRYPTBUFFER_PKCS_ALG_OID           = 41;
  {$EXTERNALSYM NCRYPTBUFFER_PKCS_ALG_PARAM}
  NCRYPTBUFFER_PKCS_ALG_PARAM         = 42;
  {$EXTERNALSYM NCRYPTBUFFER_PKCS_ALG_ID}
  NCRYPTBUFFER_PKCS_ALG_ID            = 43;
  {$EXTERNALSYM NCRYPTBUFFER_PKCS_ATTRS}
  NCRYPTBUFFER_PKCS_ATTRS             = 44;
  {$EXTERNALSYM NCRYPTBUFFER_PKCS_KEY_NAME}
  NCRYPTBUFFER_PKCS_KEY_NAME          = 45;
  {$EXTERNALSYM NCRYPTBUFFER_PKCS_SECRET}
  NCRYPTBUFFER_PKCS_SECRET            = 46;

  {$EXTERNALSYM NCRYPTBUFFER_CERT_BLOB}
  NCRYPTBUFFER_CERT_BLOB              = 47;

// NCRYPT shares the same BCRYPT definitions
type
  {$EXTERNALSYM NCryptBuffer}
  NCryptBuffer = BCryptBuffer;
  TNCryptBuffer = BCryptBuffer;
  {$EXTERNALSYM PNCryptBuffer}
  PNCryptBuffer = PBCryptBuffer;
  {$EXTERNALSYM NCryptBufferDesc}
  NCryptBufferDesc = BCryptBufferDesc;
  TNCryptBufferDesc = BCryptBufferDesc;
  {$EXTERNALSYM PNCryptBufferDesc}
  PNCryptBufferDesc = PBCryptBufferDesc;

//
// NCrypt handles
//

  {$EXTERNALSYM NCRYPT_HANDLE}
  NCRYPT_HANDLE = ULONG_PTR;
  {$EXTERNALSYM NCRYPT_PROV_HANDLE}
  NCRYPT_PROV_HANDLE = ULONG_PTR;
  {$EXTERNALSYM NCRYPT_KEY_HANDLE}
  NCRYPT_KEY_HANDLE = ULONG_PTR;
  {$EXTERNALSYM NCRYPT_HASH_HANDLE}
  NCRYPT_HASH_HANDLE = ULONG_PTR;
  {$EXTERNALSYM NCRYPT_SECRET_HANDLE}
  NCRYPT_SECRET_HANDLE = ULONG_PTR;

//
// NCrypt API Flags
//

const
  {$EXTERNALSYM NCRYPT_NO_PADDING_FLAG}
  NCRYPT_NO_PADDING_FLAG      = BCRYPT_PAD_NONE;
  {$EXTERNALSYM NCRYPT_PAD_PKCS1_FLAG}
  NCRYPT_PAD_PKCS1_FLAG       = BCRYPT_PAD_PKCS1;  // NCryptEncrypt/Decrypt NCryptSignHash/VerifySignature
  {$EXTERNALSYM NCRYPT_PAD_OAEP_FLAG}
  NCRYPT_PAD_OAEP_FLAG        = BCRYPT_PAD_OAEP;   // BCryptEncrypt/Decrypt
  {$EXTERNALSYM NCRYPT_PAD_PSS_FLAG}
  NCRYPT_PAD_PSS_FLAG         = BCRYPT_PAD_PSS;    // BCryptSignHash/VerifySignature
  {$EXTERNALSYM NCRYPT_NO_KEY_VALIDATION}
  NCRYPT_NO_KEY_VALIDATION    = BCRYPT_NO_KEY_VALIDATION;
  {$EXTERNALSYM NCRYPT_MACHINE_KEY_FLAG}
  NCRYPT_MACHINE_KEY_FLAG                 = $00000020;  // same as CAPI CRYPT_MACHINE_KEYSET
  {$EXTERNALSYM NCRYPT_SILENT_FLAG}
  NCRYPT_SILENT_FLAG                      = $00000040;  // same as CAPI CRYPT_SILENT
  {$EXTERNALSYM NCRYPT_OVERWRITE_KEY_FLAG}
  NCRYPT_OVERWRITE_KEY_FLAG               = $00000080;
  {$EXTERNALSYM NCRYPT_WRITE_KEY_TO_LEGACY_STORE_FLAG}
  NCRYPT_WRITE_KEY_TO_LEGACY_STORE_FLAG   = $00000200;
  {$EXTERNALSYM NCRYPT_DO_NOT_FINALIZE_FLAG}
  NCRYPT_DO_NOT_FINALIZE_FLAG             = $00000400;
  {$EXTERNALSYM NCRYPT_PERSIST_ONLY_FLAG}
  NCRYPT_PERSIST_ONLY_FLAG                = $40000000;
  {$EXTERNALSYM NCRYPT_PERSIST_FLAG}
  NCRYPT_PERSIST_FLAG                     = $80000000;
  {$EXTERNALSYM NCRYPT_REGISTER_NOTIFY_FLAG}
  NCRYPT_REGISTER_NOTIFY_FLAG             = $00000001;
  {$EXTERNALSYM NCRYPT_UNREGISTER_NOTIFY_FLAG}
  NCRYPT_UNREGISTER_NOTIFY_FLAG           = $00000002;

//
// Functions used to manage persisted keys.
//

function NCryptOpenStorageProvider(out phProvider: NCRYPT_PROV_HANDLE;
  pszProviderName: LPCWSTR; dwFlags: DWORD): SECURITY_STATUS;

const
// AlgOperations flags for use with NCryptEnumAlgorithms()
  {$EXTERNALSYM NCRYPT_CIPHER_OPERATION}
  NCRYPT_CIPHER_OPERATION                 = BCRYPT_CIPHER_OPERATION;
  {$EXTERNALSYM NCRYPT_HASH_OPERATION}
  NCRYPT_HASH_OPERATION                   = BCRYPT_HASH_OPERATION;
  {$EXTERNALSYM NCRYPT_ASYMMETRIC_ENCRYPTION_OPERATION}
  NCRYPT_ASYMMETRIC_ENCRYPTION_OPERATION  = BCRYPT_ASYMMETRIC_ENCRYPTION_OPERATION;
  {$EXTERNALSYM NCRYPT_SECRET_AGREEMENT_OPERATION}
  NCRYPT_SECRET_AGREEMENT_OPERATION       = BCRYPT_SECRET_AGREEMENT_OPERATION;
  {$EXTERNALSYM NCRYPT_SIGNATURE_OPERATION}
  NCRYPT_SIGNATURE_OPERATION              = BCRYPT_SIGNATURE_OPERATION;
  {$EXTERNALSYM NCRYPT_RNG_OPERATION}
  NCRYPT_RNG_OPERATION                    = BCRYPT_RNG_OPERATION;

// USE EXTREME CAUTION: editing comments that contain "certenrolls_*" tokens
// could break building CertEnroll idl files:
// certenrolls_begin -- NCryptAlgorithmName
type
  PPNCryptAlgorithmName = ^PNCryptAlgorithmName;
  PNCryptAlgorithmName = ^TNCryptAlgorithmName;
  {$EXTERNALSYM _NCryptAlgorithmName}
  _NCryptAlgorithmName = record
    pszName: LPWSTR;
    dwClass: DWORD;            // the CNG interface that supports this algorithm
    dwAlgOperations: DWORD;    // the types of operations supported by this algorithm
    dwFlags: DWORD;
  end;
  {$EXTERNALSYM NCryptAlgorithmName}
  NCryptAlgorithmName = _NCryptAlgorithmName;
  TNCryptAlgorithmName = _NCryptAlgorithmName;
// certenrolls_end

function NCryptEnumAlgorithms(hProvider: NCRYPT_PROV_HANDLE;
  dwAlgOperations: DWORD; out pdwAlgCount: DWORD;
  ppAlgList: PPNCryptAlgorithmName; dwFlags: DWORD): SECURITY_STATUS;

function NCryptIsAlgSupported(hProvider: NCRYPT_PROV_HANDLE; pszAlgId: LPCWSTR;
  dwFlags: DWORD): SECURITY_STATUS;

type
  PNCryptKeyName = ^TNCryptKeyName;
  {$EXTERNALSYM NCryptKeyName}
  NCryptKeyName = record
    pszName: LPWSTR;
    pszAlgid: LPWSTR;
    dwLegacyKeySpec: DWORD;
    dwFlags: DWORD;
  end;
  TNCryptKeyName = NCryptKeyName;

function NCryptEnumKeys(hProvider: NCRYPT_PROV_HANDLE; pszScope: LPCWSTR;
  out ppKeyName: PNCryptKeyName; var ppEnumState: Pointer;
  dwFlags: DWORD): SECURITY_STATUS;

type
  PNCryptProviderName = ^TNCryptProviderName;
  {$EXTERNALSYM NCryptProviderName}
  NCryptProviderName = record
    pszName: LPWSTR;
    pszComment: LPWSTR;
  end;
  TNCryptProviderName = NCryptProviderName;

function NCryptEnumStorageProviders(out pdwProviderCount: DWORD;
  out ppProviderList: PNCryptProviderName;
  dwFlags: DWORD): SECURITY_STATUS;

function NCryptFreeBuffer(pvInput: Pointer): SECURITY_STATUS;

function NCryptOpenKey(hProvider: NCRYPT_PROV_HANDLE;
  out phKey: NCRYPT_KEY_HANDLE; pszKeyName: LPCWSTR;
  dwLegacyKeySpec, dwFlags: DWORD): SECURITY_STATUS;

function NCryptCreatePersistedKey(hProvider: NCRYPT_PROV_HANDLE;
  out phKey: NCRYPT_KEY_HANDLE; pszAlgId, pszKeyName: LPCWSTR;
  dwLegacyKeySpec, dwFlags: DWORD): SECURITY_STATUS;

const
// Standard property names.
  {$EXTERNALSYM NCRYPT_NAME_PROPERTY}
  NCRYPT_NAME_PROPERTY                    = 'Name';
  {$EXTERNALSYM NCRYPT_UNIQUE_NAME_PROPERTY}
  NCRYPT_UNIQUE_NAME_PROPERTY             = 'Unique Name';
  {$EXTERNALSYM NCRYPT_ALGORITHM_PROPERTY}
  NCRYPT_ALGORITHM_PROPERTY               = 'Algorithm Name';
  {$EXTERNALSYM NCRYPT_LENGTH_PROPERTY}
  NCRYPT_LENGTH_PROPERTY                  = 'Length';
  {$EXTERNALSYM NCRYPT_LENGTHS_PROPERTY}
  NCRYPT_LENGTHS_PROPERTY                 = 'Lengths';
  {$EXTERNALSYM NCRYPT_BLOCK_LENGTH_PROPERTY}
  NCRYPT_BLOCK_LENGTH_PROPERTY            = 'Block Length';
  {$EXTERNALSYM NCRYPT_UI_POLICY_PROPERTY}
  NCRYPT_UI_POLICY_PROPERTY               = 'UI Policy';
  {$EXTERNALSYM NCRYPT_EXPORT_POLICY_PROPERTY}
  NCRYPT_EXPORT_POLICY_PROPERTY           = 'Export Policy';
  {$EXTERNALSYM NCRYPT_WINDOW_HANDLE_PROPERTY}
  NCRYPT_WINDOW_HANDLE_PROPERTY           = 'HWND Handle';
  {$EXTERNALSYM NCRYPT_USE_CONTEXT_PROPERTY}
  NCRYPT_USE_CONTEXT_PROPERTY             = 'Use Context';
  {$EXTERNALSYM NCRYPT_IMPL_TYPE_PROPERTY}
  NCRYPT_IMPL_TYPE_PROPERTY               = 'Impl Type';
  {$EXTERNALSYM NCRYPT_KEY_USAGE_PROPERTY}
  NCRYPT_KEY_USAGE_PROPERTY               = 'Key Usage';
  {$EXTERNALSYM NCRYPT_KEY_TYPE_PROPERTY}
  NCRYPT_KEY_TYPE_PROPERTY                = 'Key Type';
  {$EXTERNALSYM NCRYPT_VERSION_PROPERTY}
  NCRYPT_VERSION_PROPERTY                 = 'Version';
  {$EXTERNALSYM NCRYPT_SECURITY_DESCR_SUPPORT_PROPERTY}
  NCRYPT_SECURITY_DESCR_SUPPORT_PROPERTY  = 'Security Descr Support';
  {$EXTERNALSYM NCRYPT_SECURITY_DESCR_PROPERTY}
  NCRYPT_SECURITY_DESCR_PROPERTY          = 'Security Descr';
  {$EXTERNALSYM NCRYPT_USE_COUNT_ENABLED_PROPERTY}
  NCRYPT_USE_COUNT_ENABLED_PROPERTY       = 'Enabled Use Count';
  {$EXTERNALSYM NCRYPT_USE_COUNT_PROPERTY}
  NCRYPT_USE_COUNT_PROPERTY               = 'Use Count';
  {$EXTERNALSYM NCRYPT_LAST_MODIFIED_PROPERTY}
  NCRYPT_LAST_MODIFIED_PROPERTY           = 'Modified';
  {$EXTERNALSYM NCRYPT_MAX_NAME_LENGTH_PROPERTY}
  NCRYPT_MAX_NAME_LENGTH_PROPERTY         = 'Max Name Length';
  {$EXTERNALSYM NCRYPT_ALGORITHM_GROUP_PROPERTY}
  NCRYPT_ALGORITHM_GROUP_PROPERTY         = 'Algorithm Group';
  {$EXTERNALSYM NCRYPT_DH_PARAMETERS_PROPERTY}
  NCRYPT_DH_PARAMETERS_PROPERTY           = BCRYPT_DH_PARAMETERS;
  {$EXTERNALSYM NCRYPT_PROVIDER_HANDLE_PROPERTY}
  NCRYPT_PROVIDER_HANDLE_PROPERTY         = 'Provider Handle';
  {$EXTERNALSYM NCRYPT_PIN_PROPERTY}
  NCRYPT_PIN_PROPERTY                     = 'SmartCardPin';
  {$EXTERNALSYM NCRYPT_READER_PROPERTY}
  NCRYPT_READER_PROPERTY                  = 'SmartCardReader';
  {$EXTERNALSYM NCRYPT_SMARTCARD_GUID_PROPERTY}
  NCRYPT_SMARTCARD_GUID_PROPERTY          = 'SmartCardGuid';
  {$EXTERNALSYM NCRYPT_CERTIFICATE_PROPERTY}
  NCRYPT_CERTIFICATE_PROPERTY             = 'SmartCardKeyCertificate';
  {$EXTERNALSYM NCRYPT_PIN_PROMPT_PROPERTY}
  NCRYPT_PIN_PROMPT_PROPERTY              = 'SmartCardPinPrompt';
  {$EXTERNALSYM NCRYPT_USER_CERTSTORE_PROPERTY}
  NCRYPT_USER_CERTSTORE_PROPERTY          = 'SmartCardUserCertStore';
  {$EXTERNALSYM NCRYPT_ROOT_CERTSTORE_PROPERTY}
  NCRYPT_ROOT_CERTSTORE_PROPERTY          = 'SmartcardRootCertStore';

// Maximum length of property name (in characters)
  {$EXTERNALSYM NCRYPT_MAX_PROPERTY_NAME}
  NCRYPT_MAX_PROPERTY_NAME        = 64;

// Maximum length of property data (in bytes)
  {$EXTERNALSYM NCRYPT_MAX_PROPERTY_DATA}
  NCRYPT_MAX_PROPERTY_DATA        = $100000;

// NCRYPT_EXPORT_POLICY_PROPERTY property flags.
  {$EXTERNALSYM NCRYPT_ALLOW_EXPORT_FLAG}
  NCRYPT_ALLOW_EXPORT_FLAG                = $00000001;
  {$EXTERNALSYM NCRYPT_ALLOW_PLAINTEXT_EXPORT_FLAG}
  NCRYPT_ALLOW_PLAINTEXT_EXPORT_FLAG      = $00000002;
  {$EXTERNALSYM NCRYPT_ALLOW_ARCHIVING_FLAG}
  NCRYPT_ALLOW_ARCHIVING_FLAG             = $00000004;
  {$EXTERNALSYM NCRYPT_ALLOW_PLAINTEXT_ARCHIVING_FLAG}
  NCRYPT_ALLOW_PLAINTEXT_ARCHIVING_FLAG   = $00000008;

// NCRYPT_IMPL_TYPE_PROPERTY property flags.
  {$EXTERNALSYM NCRYPT_IMPL_HARDWARE_FLAG}
  NCRYPT_IMPL_HARDWARE_FLAG               = $00000001;
  {$EXTERNALSYM NCRYPT_IMPL_SOFTWARE_FLAG}
  NCRYPT_IMPL_SOFTWARE_FLAG               = $00000002;
  {$EXTERNALSYM NCRYPT_IMPL_REMOVABLE_FLAG}
  NCRYPT_IMPL_REMOVABLE_FLAG              = $00000008;
  {$EXTERNALSYM NCRYPT_IMPL_HARDWARE_RNG_FLAG}
  NCRYPT_IMPL_HARDWARE_RNG_FLAG           = $00000010;

// NCRYPT_KEY_USAGE_PROPERTY property flags.
  {$EXTERNALSYM NCRYPT_ALLOW_DECRYPT_FLAG}
  NCRYPT_ALLOW_DECRYPT_FLAG               = $00000001;
  {$EXTERNALSYM NCRYPT_ALLOW_SIGNING_FLAG}
  NCRYPT_ALLOW_SIGNING_FLAG               = $00000002;
  {$EXTERNALSYM NCRYPT_ALLOW_KEY_AGREEMENT_FLAG}
  NCRYPT_ALLOW_KEY_AGREEMENT_FLAG         = $00000004;
  {$EXTERNALSYM NCRYPT_ALLOW_ALL_USAGES}
  NCRYPT_ALLOW_ALL_USAGES                 = $00ffffff;

// NCRYPT_UI_POLICY_PROPERTY property flags and structure
  {$EXTERNALSYM NCRYPT_UI_PROTECT_KEY_FLAG}
  NCRYPT_UI_PROTECT_KEY_FLAG              = $00000001;
  {$EXTERNALSYM NCRYPT_UI_FORCE_HIGH_PROTECTION_FLAG}
  NCRYPT_UI_FORCE_HIGH_PROTECTION_FLAG    = $00000002;

type
  PNCryptUIPolicyBlob = ^TNCryptUIPolicyBlob;
  {$EXTERNALSYM __NCRYPT_UI_POLICY_BLOB}
  __NCRYPT_UI_POLICY_BLOB = record
    dwVersion: DWORD;
    dwFlags: DWORD;
    cbCreationTitle: DWORD;
    cbFriendlyName: DWORD;
    cbDescription: DWORD;
    // creation title string
    // friendly name string
    // description string
  end;
  {$EXTERNALSYM NCRYPT_UI_POLICY_BLOB}
  NCRYPT_UI_POLICY_BLOB = __NCRYPT_UI_POLICY_BLOB;
  TNCryptUIPolicyBlob = __NCRYPT_UI_POLICY_BLOB;

  PNCryptUIPolicy = ^TNCryptUIPolicy;
  {$EXTERNALSYM __NCRYPT_UI_POLICY}
  __NCRYPT_UI_POLICY = record
    dwVersion: DWORD;
    dwFlags: DWORD;
    pszCreationTitle: LPCWSTR;
    pszFriendlyName: LPCWSTR;
    pszDescription: LPCWSTR;
  end;
  {$EXTERNALSYM NCRYPT_UI_POLICY}
  NCRYPT_UI_POLICY = __NCRYPT_UI_POLICY;
  TNCryptUIPolicy = __NCRYPT_UI_POLICY;


// NCRYPT_LENGTHS_PROPERTY property structure.
  PNCryptSupportedLengths = ^TNCryptSupportedLengths;
  {$EXTERNALSYM __NCRYPT_SUPPORTED_LENGTHS}
  __NCRYPT_SUPPORTED_LENGTHS = record
    dwMinLength: DWORD;
    dwMaxLength: DWORD;
    dwIncrement: DWORD;
    dwDefaultLength: DWORD;
  end;
  {$EXTERNALSYM NCRYPT_SUPPORTED_LENGTHS}
  NCRYPT_SUPPORTED_LENGTHS = __NCRYPT_SUPPORTED_LENGTHS;
  TNCryptSupportedLengths = __NCRYPT_SUPPORTED_LENGTHS;

function NCryptGetProperty(hObject: NCRYPT_HANDLE; pszProperty: LPCWSTR;
  pbOutput: PBYTE; cbOutput: DWORD; out pcbResult: DWORD;
  dwFlags: DWORD): SECURITY_STATUS;

function NCryptSetProperty(hObject: NCRYPT_HANDLE; pszProperty: LPCWSTR;
  pbInput: PBYTE; cbInput, dwFlags: DWORD): SECURITY_STATUS;

function NCryptFinalizeKey(hKey: NCRYPT_KEY_HANDLE;
  dwFlags: DWORD): SECURITY_STATUS;

function NCryptEncrypt(hKey: NCRYPT_KEY_HANDLE; pbInput: PBYTE;
  cbInput: DWORD; pPaddingInfo: Pointer; pbOutput: PBYTE;
  cbOutput: DWORD; out pcbResult: DWORD;
  dwFlags: DWORD): SECURITY_STATUS;

function NCryptDecrypt(hKey: NCRYPT_KEY_HANDLE; pbInput: PBYTE;
  cbInput: DWORD; pPaddingInfo: Pointer; pbOutput: PBYTE;
  cbOutput: DWORD; out pcbResult: DWORD;
  dwFlags: DWORD): SECURITY_STATUS;

const
  {$EXTERNALSYM NCRYPT_PKCS7_ENVELOPE_BLOB}
  NCRYPT_PKCS7_ENVELOPE_BLOB      = 'PKCS7_ENVELOPE';
  {$EXTERNALSYM NCRYPT_PKCS8_PRIVATE_KEY_BLOB}
  NCRYPT_PKCS8_PRIVATE_KEY_BLOB   = 'PKCS8_PRIVATEKEY';
  {$EXTERNALSYM NCRYPT_OPAQUETRANSPORT_BLOB}
  NCRYPT_OPAQUETRANSPORT_BLOB     = 'OpaqueTransport';

function NCryptImportKey(hProvider: NCRYPT_PROV_HANDLE;
  hImportKey: NCRYPT_KEY_HANDLE; pszBlobType: LPCWSTR;
  pParameterList: PNCryptBufferDesc; out phKey: NCRYPT_KEY_HANDLE;
  pbData: PBYTE; cbData, dwFlags: DWORD): SECURITY_STATUS;

function NCryptExportKey(hKey, hExportKey: NCRYPT_KEY_HANDLE;
  pszBlobType: LPCWSTR; pParameterList: PNCryptBufferDesc;
  pbOutput: PBYTE; cbOutput: DWORD; out pcbResult: DWORD;
  dwFlags: DWORD): SECURITY_STATUS;

function NCryptSignHash(hKey: NCRYPT_KEY_HANDLE; pPaddingInfo: Pointer;
  pbHashValue: PBYTE; cbHashValue: DWORD; pbSignature: PBYTE;
  cbSignature: DWORD; out pcbResult: DWORD;
  dwFlags: DWORD): SECURITY_STATUS;

function NCryptVerifySignature(hKey: NCRYPT_KEY_HANDLE; pPaddingInfo: Pointer;
  pbHashValue: PBYTE; cbHashValue: DWORD; pbSignature: PBYTE;
  cbSignature, dwFlags: DWORD): SECURITY_STATUS;

function NCryptDeleteKey(hKey: NCRYPT_KEY_HANDLE;
  dwFlags: DWORD): SECURITY_STATUS;

function NCryptFreeObject(hObject: NCRYPT_HANDLE): SECURITY_STATUS;

function NCryptIsKeyHandle(hKey: NCRYPT_KEY_HANDLE): BOOL;

function NCryptTranslateHandle(out phProvider: NCRYPT_PROV_HANDLE;
  out phKey: NCRYPT_KEY_HANDLE; hLegacyProv: ULONG_PTR {HCRYPTPROV};
  hLegacyKey: ULONG_PTR {HCRYPTKEY};
  dwLegacyKeySpec, dwFlags: DWORD): SECURITY_STATUS;

function NCryptNotifyChangeKey(hProvider: NCRYPT_PROV_HANDLE;
  var phEvent: THANDLE; dwFlags: DWORD): SECURITY_STATUS;

function NCryptSecretAgreement(hPrivKey, hPubKey: NCRYPT_KEY_HANDLE;
  out phAgreedSecret: NCRYPT_SECRET_HANDLE;
  dwFlags: DWORD): SECURITY_STATUS;

function NCryptDeriveKey(hSharedSecret: NCRYPT_SECRET_HANDLE;
  pwszKDF: LPCWSTR; pParameterList: PNCryptBufferDesc;
  pbDerivedKey: PBYTE; cbDerivedKey: DWORD; out pcbResult: DWORD;
  dwFlags: ULONG): SECURITY_STATUS;

const
  {$EXTERNALSYM NCRYPT_KEY_STORAGE_INTERFACE_VERSION}
  NCRYPT_KEY_STORAGE_INTERFACE_VERSION: TBCryptInterfaceVersion =
    (MajorVersion: 1; MinorVersion: 0);

function CheckNCryptIsLoaded: Boolean;
procedure LoadNCrypt;
procedure UnLoadNCrypt;

implementation

const
  ncryptdll = 'ncrypt.dll';

type
  {$IFNDEF FPC}
    TLibHandle = THandle;
  {$ENDIF}

  TNCryptOpenStorageProvider = function(out phProvider: NCRYPT_PROV_HANDLE;
     pszProviderName: LPCWSTR; dwFlags: DWORD): SECURITY_STATUS; stdcall;
  TNCryptEnumAlgorithms = function(hProvider: NCRYPT_PROV_HANDLE;
     dwAlgOperations: DWORD; out pdwAlgCount: DWORD;
     ppAlgList: PPNCryptAlgorithmName; dwFlags: DWORD): SECURITY_STATUS; stdcall;
  TNCryptIsAlgSupported = function(hProvider: NCRYPT_PROV_HANDLE;
     pszAlgId: LPCWSTR; dwFlags: DWORD): SECURITY_STATUS; stdcall;
  TNCryptEnumKeys = function(hProvider: NCRYPT_PROV_HANDLE; pszScope: LPCWSTR;
     out ppKeyName: PNCryptKeyName; var ppEnumState: Pointer; dwFlags: DWORD):
     SECURITY_STATUS; stdcall;
  TNCryptEnumStorageProviders = function(out pdwProviderCount: DWORD;
     out ppProviderList: PNCryptProviderName; dwFlags: DWORD): SECURITY_STATUS;
     stdcall;
  TNCryptFreeBuffer = function(pvInput: Pointer): SECURITY_STATUS; stdcall;
  TNCryptOpenKey = function(hProvider: NCRYPT_PROV_HANDLE;
     out phKey: NCRYPT_KEY_HANDLE; pszKeyName: LPCWSTR;
     dwLegacyKeySpec, dwFlags: DWORD): SECURITY_STATUS; stdcall;
  TNCryptCreatePersistedKey = function(hProvider: NCRYPT_PROV_HANDLE;
     out phKey: NCRYPT_KEY_HANDLE; pszAlgId, pszKeyName: LPCWSTR;
     dwLegacyKeySpec, dwFlags: DWORD): SECURITY_STATUS; stdcall;
  TNCryptGetProperty = function(hObject: NCRYPT_HANDLE; pszProperty: LPCWSTR;
     pbOutput: PBYTE; cbOutput: DWORD; out pcbResult: DWORD; dwFlags: DWORD):
     SECURITY_STATUS; stdcall;
  TNCryptSetProperty = function(hObject: NCRYPT_HANDLE; pszProperty: LPCWSTR;
     pbInput: PBYTE; cbInput, dwFlags: DWORD): SECURITY_STATUS; stdcall;
  TNCryptFinalizeKey = function(hKey: NCRYPT_KEY_HANDLE; dwFlags: DWORD):
     SECURITY_STATUS; stdcall;
  TNCryptEncrypt = function(hKey: NCRYPT_KEY_HANDLE; pbInput: PBYTE;
     cbInput: DWORD; pPaddingInfo: Pointer; pbOutput: PBYTE; cbOutput: DWORD;
     out pcbResult: DWORD; dwFlags: DWORD): SECURITY_STATUS; stdcall;
  TNCryptDecrypt = function (hKey: NCRYPT_KEY_HANDLE; pbInput: PBYTE;
     cbInput: DWORD; pPaddingInfo: Pointer; pbOutput: PBYTE; cbOutput: DWORD;
     out pcbResult: DWORD; dwFlags: DWORD): SECURITY_STATUS; stdcall;
  TNCryptImportKey = function(hProvider: NCRYPT_PROV_HANDLE;
     hImportKey: NCRYPT_KEY_HANDLE; pszBlobType: LPCWSTR;
     pParameterList: PNCryptBufferDesc; out phKey: NCRYPT_KEY_HANDLE;
     pbData: PBYTE; cbData, dwFlags: DWORD): SECURITY_STATUS; stdcall;
  TNCryptExportKey = function(hKey, hExportKey: NCRYPT_KEY_HANDLE;
     pszBlobType: LPCWSTR; pParameterList: PNCryptBufferDesc; pbOutput: PBYTE;
     cbOutput: DWORD; out pcbResult: DWORD; dwFlags: DWORD): SECURITY_STATUS; stdcall;
  TNCryptSignHash = function(hKey: NCRYPT_KEY_HANDLE; pPaddingInfo: Pointer;
     pbHashValue: PBYTE; cbHashValue: DWORD; pbSignature: PBYTE;
     cbSignature: DWORD; out pcbResult: DWORD; dwFlags: DWORD): SECURITY_STATUS; stdcall;
  TNCryptVerifySignature = function(hKey: NCRYPT_KEY_HANDLE; pPaddingInfo: Pointer;
    pbHashValue: PBYTE; cbHashValue: DWORD; pbSignature: PBYTE;
    cbSignature, dwFlags: DWORD): SECURITY_STATUS; stdcall;
  TNCryptDeleteKey = function (hKey: NCRYPT_KEY_HANDLE;
    dwFlags: DWORD): SECURITY_STATUS; stdcall;
  TNCryptFreeObject = function(hObject: NCRYPT_HANDLE): SECURITY_STATUS; stdcall;
  TNCryptIsKeyHandle = function(hKey: NCRYPT_KEY_HANDLE): BOOL; stdcall;
  TNCryptTranslateHandle = function(out phProvider: NCRYPT_PROV_HANDLE;
    out phKey: NCRYPT_KEY_HANDLE; hLegacyProv: ULONG_PTR {HCRYPTPROV};
    hLegacyKey: ULONG_PTR {HCRYPTKEY}; dwLegacyKeySpec, dwFlags: DWORD):
     SECURITY_STATUS; stdcall;
  TNCryptNotifyChangeKey = function(hProvider: NCRYPT_PROV_HANDLE;
    var phEvent: THANDLE; dwFlags: DWORD): SECURITY_STATUS; stdcall;
  TNCryptSecretAgreement = function (hPrivKey, hPubKey: NCRYPT_KEY_HANDLE;
    out phAgreedSecret: NCRYPT_SECRET_HANDLE; dwFlags: DWORD): SECURITY_STATUS; stdcall;
  TNCryptDeriveKey = function(hSharedSecret: NCRYPT_SECRET_HANDLE;
    pwszKDF: LPCWSTR; pParameterList: PNCryptBufferDesc;
    pbDerivedKey: PBYTE; cbDerivedKey: DWORD; out pcbResult: DWORD;
    dwFlags: ULONG): SECURITY_STATUS; stdcall;

var
  NCryptLoaded: Boolean;
  NCryptHandle: TLibHandle;

  _NCryptOpenStorageProvider: TNCryptOpenStorageProvider;
  _NCryptEnumAlgorithms: TNCryptEnumAlgorithms;
  _NCryptIsAlgSupported: TNCryptIsAlgSupported;
  _NCryptEnumKeys: TNCryptEnumKeys;
  _NCryptEnumStorageProviders: TNCryptEnumStorageProviders;
  _NCryptFreeBuffer: TNCryptFreeBuffer;
  _NCryptOpenKey: TNCryptOpenKey;
  _NCryptCreatePersistedKey: TNCryptCreatePersistedKey;
  _NCryptGetProperty: TNCryptGetProperty;
  _NCryptSetProperty: TNCryptSetProperty;
  _NCryptFinalizeKey: TNCryptFinalizeKey;
  _NCryptEncrypt: TNCryptEncrypt;
  _NCryptDecrypt: TNCryptDecrypt;
  _NCryptImportKey: TNCryptImportKey;
  _NCryptExportKey: TNCryptExportKey;
  _NCryptSignHash: TNCryptSignHash;
  _NCryptVerifySignature: TNCryptVerifySignature;
  _NCryptDeleteKey: TNCryptDeleteKey;
  _NCryptFreeObject: TNCryptFreeObject;
  _NCryptIsKeyHandle: TNCryptIsKeyHandle;
  _NCryptTranslateHandle: TNCryptTranslateHandle;
  _NCryptNotifyChangeKey: TNCryptNotifyChangeKey;
  _NCryptSecretAgreement: TNCryptSecretAgreement;
  _NCryptDeriveKey: TNCryptDeriveKey;


{$IFDEF REMOVE_CAST_WARN}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}

function NCryptOpenStorageProvider(out phProvider: NCRYPT_PROV_HANDLE;
  pszProviderName: LPCWSTR; dwFlags: DWORD): SECURITY_STATUS;
begin
  if CheckNCryptIsLoaded and Assigned(_NCryptOpenStorageProvider) then
    Result := _NCryptOpenStorageProvider(phProvider, pszProviderName, dwFlags)
  else
    Result := ERROR_INVALID_FUNCTION;
end;

function NCryptEnumAlgorithms(hProvider: NCRYPT_PROV_HANDLE;
  dwAlgOperations: DWORD; out pdwAlgCount: DWORD;
  ppAlgList: PPNCryptAlgorithmName; dwFlags: DWORD): SECURITY_STATUS;
begin
  if CheckNCryptIsLoaded and Assigned(_NCryptEnumAlgorithms) then
    Result := _NCryptEnumAlgorithms(hProvider, dwAlgOperations, pdwAlgCount, ppAlgList, dwFlags)
  else
    Result := ERROR_INVALID_FUNCTION;
end;

function NCryptIsAlgSupported(hProvider: NCRYPT_PROV_HANDLE; pszAlgId: LPCWSTR;
  dwFlags: DWORD): SECURITY_STATUS;
begin
  if CheckNCryptIsLoaded and Assigned(_NCryptIsAlgSupported) then
    Result := _NCryptIsAlgSupported(hProvider, pszAlgId, dwFlags)
  else
    Result := ERROR_INVALID_FUNCTION;
end;

function NCryptEnumKeys(hProvider: NCRYPT_PROV_HANDLE; pszScope: LPCWSTR; out
  ppKeyName: PNCryptKeyName; var ppEnumState: Pointer; dwFlags: DWORD
  ): SECURITY_STATUS;
begin
  if CheckNCryptIsLoaded and Assigned(_NCryptEnumKeys) then
    Result := _NCryptEnumKeys(hProvider, pszScope, ppKeyName, ppEnumState, dwFlags)
  else
    Result := ERROR_INVALID_FUNCTION;
end;

function NCryptEnumStorageProviders(out pdwProviderCount: DWORD; out
  ppProviderList: PNCryptProviderName; dwFlags: DWORD): SECURITY_STATUS;
begin
  if CheckNCryptIsLoaded and Assigned(_NCryptEnumStorageProviders) then
    Result := _NCryptEnumStorageProviders(pdwProviderCount, ppProviderList, dwFlags)
  else
    Result := ERROR_INVALID_FUNCTION;
end;

function NCryptFreeBuffer(pvInput: Pointer): SECURITY_STATUS;
begin
  if CheckNCryptIsLoaded and Assigned(_NCryptFreeBuffer) then
    Result := _NCryptFreeBuffer(pvInput)
  else
    Result := ERROR_INVALID_FUNCTION;
end;

function NCryptOpenKey(hProvider: NCRYPT_PROV_HANDLE; out
  phKey: NCRYPT_KEY_HANDLE; pszKeyName: LPCWSTR; dwLegacyKeySpec, dwFlags: DWORD
  ): SECURITY_STATUS;
begin
  if CheckNCryptIsLoaded and Assigned(_NCryptOpenKey) then
    Result := _NCryptOpenKey(hProvider, phKey, pszKeyName, dwLegacyKeySpec, dwFlags)
  else
    Result := ERROR_INVALID_FUNCTION;
end;

function NCryptCreatePersistedKey(hProvider: NCRYPT_PROV_HANDLE; out
  phKey: NCRYPT_KEY_HANDLE; pszAlgId, pszKeyName: LPCWSTR; dwLegacyKeySpec,
  dwFlags: DWORD): SECURITY_STATUS;
begin
  if CheckNCryptIsLoaded and Assigned(_NCryptCreatePersistedKey) then
    Result := _NCryptCreatePersistedKey(hProvider, phKey, pszAlgId, pszKeyName, dwLegacyKeySpec, dwFlags)
  else
    Result := ERROR_INVALID_FUNCTION;
end;

function NCryptGetProperty(hObject: NCRYPT_HANDLE; pszProperty: LPCWSTR;
  pbOutput: PBYTE; cbOutput: DWORD; out pcbResult: DWORD; dwFlags: DWORD
  ): SECURITY_STATUS;
begin
  if CheckNCryptIsLoaded and Assigned(_NCryptGetProperty) then
    Result := _NCryptGetProperty(hObject, pszProperty, pbOutput, cbOutput, pcbResult, dwFlags)
  else
    Result := ERROR_INVALID_FUNCTION;
end;

function NCryptSetProperty(hObject: NCRYPT_HANDLE; pszProperty: LPCWSTR;
  pbInput: PBYTE; cbInput, dwFlags: DWORD): SECURITY_STATUS;
begin
  if CheckNCryptIsLoaded and Assigned(_NCryptSetProperty) then
    Result := _NCryptSetProperty(hObject, pszProperty, pbInput, cbInput, dwFlags)
  else
    Result := ERROR_INVALID_FUNCTION;
end;

function NCryptFinalizeKey(hKey: NCRYPT_KEY_HANDLE; dwFlags: DWORD
  ): SECURITY_STATUS;
begin
  if CheckNCryptIsLoaded and Assigned(_NCryptFinalizeKey) then
    Result := _NCryptFinalizeKey(hKey, dwFlags)
  else
    Result := ERROR_INVALID_FUNCTION;
end;

function NCryptEncrypt(hKey: NCRYPT_KEY_HANDLE; pbInput: PBYTE; cbInput: DWORD;
  pPaddingInfo: Pointer; pbOutput: PBYTE; cbOutput: DWORD; out
  pcbResult: DWORD; dwFlags: DWORD): SECURITY_STATUS;
begin
  if CheckNCryptIsLoaded and Assigned(_NCryptEncrypt) then
    Result := _NCryptEncrypt(hKey, pbInput, cbInput, pPaddingInfo, pbOutput, cbOutput, pcbResult, dwFlags)
  else
    Result := ERROR_INVALID_FUNCTION;
end;

function NCryptDecrypt(hKey: NCRYPT_KEY_HANDLE; pbInput: PBYTE; cbInput: DWORD;
  pPaddingInfo: Pointer; pbOutput: PBYTE; cbOutput: DWORD; out
  pcbResult: DWORD; dwFlags: DWORD): SECURITY_STATUS;
begin
  if CheckNCryptIsLoaded and Assigned(_NCryptDecrypt) then
    Result := _NCryptDecrypt(hKey, pbInput, cbInput, pPaddingInfo, pbOutput, cbOutput, pcbResult, dwFlags)
  else
    Result := ERROR_INVALID_FUNCTION;
end;

function NCryptImportKey(hProvider: NCRYPT_PROV_HANDLE;
  hImportKey: NCRYPT_KEY_HANDLE; pszBlobType: LPCWSTR;
  pParameterList: PNCryptBufferDesc; out phKey: NCRYPT_KEY_HANDLE;
  pbData: PBYTE; cbData, dwFlags: DWORD): SECURITY_STATUS;
begin
  if CheckNCryptIsLoaded and Assigned(_NCryptImportKey) then
    Result := _NCryptImportKey(hProvider, hImportKey, pszBlobType, pParameterList, phKey, pbData, cbData, dwFlags)
  else
    Result := ERROR_INVALID_FUNCTION;
end;

function NCryptExportKey(hKey, hExportKey: NCRYPT_KEY_HANDLE;
  pszBlobType: LPCWSTR; pParameterList: PNCryptBufferDesc; pbOutput: PBYTE;
  cbOutput: DWORD; out pcbResult: DWORD; dwFlags: DWORD): SECURITY_STATUS;
begin
  if CheckNCryptIsLoaded and Assigned(_NCryptExportKey) then
    Result := _NCryptExportKey(hKey, hExportKey, pszBlobType, pParameterList, pbOutput, cbOutput, pcbResult, dwFlags)
  else
    Result := ERROR_INVALID_FUNCTION;
end;

function NCryptSignHash(hKey: NCRYPT_KEY_HANDLE; pPaddingInfo: Pointer;
  pbHashValue: PBYTE; cbHashValue: DWORD; pbSignature: PBYTE;
  cbSignature: DWORD; out pcbResult: DWORD; dwFlags: DWORD): SECURITY_STATUS;
begin
  if CheckNCryptIsLoaded and Assigned(_NCryptSignHash) then
    Result := _NCryptSignHash(hKey, pPaddingInfo, pbHashValue, cbHashValue, pbSignature, cbSignature, pcbResult, dwFlags)
  else
    Result := ERROR_INVALID_FUNCTION;
end;

function NCryptVerifySignature(hKey: NCRYPT_KEY_HANDLE; pPaddingInfo: Pointer;
  pbHashValue: PBYTE; cbHashValue: DWORD; pbSignature: PBYTE; cbSignature,
  dwFlags: DWORD): SECURITY_STATUS;
begin
  if CheckNCryptIsLoaded and Assigned(_NCryptVerifySignature) then
    Result := _NCryptVerifySignature(hKey, pPaddingInfo, pbHashValue, cbHashValue, pbSignature, cbSignature, dwFlags)
  else
    Result := ERROR_INVALID_FUNCTION;
end;

function NCryptDeleteKey(hKey: NCRYPT_KEY_HANDLE; dwFlags: DWORD
  ): SECURITY_STATUS;
begin
  if CheckNCryptIsLoaded and Assigned(_NCryptDeleteKey) then
    Result := _NCryptDeleteKey(hKey, dwFlags)
  else
    Result := ERROR_INVALID_FUNCTION;
end;

function NCryptFreeObject(hObject: NCRYPT_HANDLE): SECURITY_STATUS;
begin
  if CheckNCryptIsLoaded and Assigned(_NCryptFreeObject) then
    Result := _NCryptFreeObject(hObject)
  else
    Result := ERROR_INVALID_FUNCTION;
end;

function NCryptIsKeyHandle(hKey: NCRYPT_KEY_HANDLE): BOOL;
begin
  if CheckNCryptIsLoaded and Assigned(_NCryptIsKeyHandle) then
    Result := _NCryptIsKeyHandle(hKey)
  else
    Result := False;
end;

function NCryptTranslateHandle(out phProvider: NCRYPT_PROV_HANDLE; out
  phKey: NCRYPT_KEY_HANDLE; hLegacyProv: ULONG_PTR; hLegacyKey: ULONG_PTR;
  dwLegacyKeySpec, dwFlags: DWORD): SECURITY_STATUS;
begin
  if CheckNCryptIsLoaded and Assigned(_NCryptTranslateHandle) then
    Result := _NCryptTranslateHandle(phProvider, phKey, hLegacyProv, hLegacyKey, dwLegacyKeySpec, dwFlags)
  else
    Result := ERROR_INVALID_FUNCTION;
end;

function NCryptNotifyChangeKey(hProvider: NCRYPT_PROV_HANDLE;
  var phEvent: THANDLE; dwFlags: DWORD): SECURITY_STATUS;
begin
  if CheckNCryptIsLoaded and Assigned(_NCryptNotifyChangeKey) then
    Result := _NCryptNotifyChangeKey(hProvider, phEvent, dwFlags)
  else
    Result := ERROR_INVALID_FUNCTION;
end;

function NCryptSecretAgreement(hPrivKey, hPubKey: NCRYPT_KEY_HANDLE; out
  phAgreedSecret: NCRYPT_SECRET_HANDLE; dwFlags: DWORD): SECURITY_STATUS;
begin
  if CheckNCryptIsLoaded and Assigned(_NCryptSecretAgreement) then
    Result := _NCryptSecretAgreement(hPrivKey, hPubKey, phAgreedSecret, dwFlags)
  else
    Result := ERROR_INVALID_FUNCTION;
end;

function NCryptDeriveKey(hSharedSecret: NCRYPT_SECRET_HANDLE; pwszKDF: LPCWSTR;
  pParameterList: PNCryptBufferDesc; pbDerivedKey: PBYTE; cbDerivedKey: DWORD;
  out pcbResult: DWORD; dwFlags: ULONG): SECURITY_STATUS;
begin
  if CheckNCryptIsLoaded and Assigned(_NCryptDeriveKey) then
    Result := _NCryptDeriveKey(hSharedSecret, pwszKDF, pParameterList, pbDerivedKey, cbDerivedKey, pcbResult, dwFlags)
  else
    Result := ERROR_INVALID_FUNCTION;
end;

function CheckNCryptIsLoaded: Boolean;
begin
  LoadNCrypt;
  Result := NCryptLoaded;
end;

procedure LoadNCrypt;
begin
  if NCryptLoaded then
    Exit;

  NCryptHandle := LoadLibrary(ncryptdll);
  NCryptLoaded := (NCryptHandle <> 0);

  if NCryptLoaded then
  begin
    _NCryptOpenStorageProvider := GetProcAddress(NCryptHandle, 'NCryptOpenStorageProvider');
    _NCryptEnumAlgorithms := GetProcAddress(NCryptHandle, 'NCryptEnumAlgorithms');
    _NCryptIsAlgSupported := GetProcAddress(NCryptHandle, 'NCryptIsAlgSupported');
    _NCryptEnumKeys := GetProcAddress(NCryptHandle, 'NCryptEnumKeys');
    _NCryptEnumStorageProviders := GetProcAddress(NCryptHandle, 'NCryptEnumStorageProviders');
    _NCryptFreeBuffer := GetProcAddress(NCryptHandle, 'NCryptFreeBuffer');
    _NCryptOpenKey := GetProcAddress(NCryptHandle, 'NCryptOpenKey');
    _NCryptCreatePersistedKey := GetProcAddress(NCryptHandle, 'NCryptCreatePersistedKey');
    _NCryptGetProperty := GetProcAddress(NCryptHandle, 'NCryptGetProperty');
    _NCryptSetProperty := GetProcAddress(NCryptHandle, 'NCryptSetProperty');
    _NCryptFinalizeKey := GetProcAddress(NCryptHandle, 'NCryptFinalizeKey');
    _NCryptEncrypt := GetProcAddress(NCryptHandle, 'NCryptEncrypt');
    _NCryptDecrypt := GetProcAddress(NCryptHandle, 'NCryptDecrypt');
    _NCryptImportKey := GetProcAddress(NCryptHandle, 'NCryptImportKey');
    _NCryptExportKey := GetProcAddress(NCryptHandle, 'NCryptExportKey');
    _NCryptSignHash := GetProcAddress(NCryptHandle, 'NCryptSignHash');
    _NCryptVerifySignature := GetProcAddress(NCryptHandle, 'NCryptVerifySignature');
    _NCryptDeleteKey := GetProcAddress(NCryptHandle, 'NCryptDeleteKey');
    _NCryptFreeObject := GetProcAddress(NCryptHandle, 'NCryptFreeObject');
    _NCryptIsKeyHandle := GetProcAddress(NCryptHandle, 'NCryptIsKeyHandle');
    _NCryptTranslateHandle := GetProcAddress(NCryptHandle, 'NCryptTranslateHandle');
    _NCryptNotifyChangeKey := GetProcAddress(NCryptHandle, 'NCryptNotifyChangeKey');
    _NCryptSecretAgreement := GetProcAddress(NCryptHandle, 'NCryptSecretAgreement');
    _NCryptDeriveKey := GetProcAddress(NCryptHandle, 'NCryptDeriveKey');
  end

end;

procedure UnLoadNCrypt;
begin
  if NCryptHandle <> 0 then
  begin
    FreeLibrary(NCryptHandle);
    NCryptHandle := 0;
  end;

  _NCryptOpenStorageProvider := Nil;
  _NCryptEnumAlgorithms := Nil;
  _NCryptIsAlgSupported := Nil;
  _NCryptEnumKeys := Nil;
  _NCryptEnumStorageProviders := Nil;
  _NCryptFreeBuffer := Nil;
  _NCryptOpenKey := Nil;
  _NCryptCreatePersistedKey := Nil;
  _NCryptGetProperty := Nil;
  _NCryptSetProperty := Nil;
  _NCryptFinalizeKey := Nil;
  _NCryptEncrypt := Nil;
  _NCryptDecrypt := Nil;
  _NCryptImportKey := Nil;
  _NCryptExportKey := Nil;
  _NCryptSignHash := Nil;
  _NCryptVerifySignature := Nil;
  _NCryptDeleteKey := Nil;
  _NCryptFreeObject := Nil;
  _NCryptIsKeyHandle := Nil;
  _NCryptTranslateHandle := Nil;
  _NCryptNotifyChangeKey := Nil;
  _NCryptSecretAgreement := Nil;
  _NCryptDeriveKey := Nil;

  NCryptLoaded := False;
end;

initialization
  NCryptHandle := 0;
  UnLoadNCrypt;

finalization
  UnLoadNCrypt;

end.

