{*****************************************************************}
{                                                                 }
{            CodeGear Delphi Runtime Library                      }
{            BCrypt.pas interface unit                            }
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
{ The original code is: bcrypt.h, released 1992-1999.             }
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

{$I ACBr.inc}

unit ACBr_BCrypt;

// {$WEAKPACKAGEUNIT}
// http://docs.embarcadero.com/products/rad_studio/delphiAndcpp2009/HelpUpdate2/EN/html/devcommon/cm_weakpackage_body_xml.html
{$ALIGN 8}

interface

uses
  Windows, Types, ACBr_NTStatus;

//
// DeriveKey KDF Types
//
const
  {$EXTERNALSYM BCRYPT_KDF_HASH}
  BCRYPT_KDF_HASH     = 'HASH';
  {$EXTERNALSYM BCRYPT_KDF_HMAC}
  BCRYPT_KDF_HMAC     = 'HMAC';
  {$EXTERNALSYM BCRYPT_KDF_TLS_PRF}
  BCRYPT_KDF_TLS_PRF  = 'TLS_PRF';

//
// DeriveKey KDF BufferTypes
//
// For BCRYPT_KDF_HASH and BCRYPT_KDF_HMAC operations, there may be an arbitrary
// number of KDF_SECRET_PREPEND and KDF_SECRET_APPEND buffertypes in the
// parameter list.  The BufferTypes are processed in order of appearence 
// within the parameter list.
//
  {$EXTERNALSYM KDF_HASH_ALGORITHM}
  KDF_HASH_ALGORITHM  = $0;
  {$EXTERNALSYM KDF_SECRET_PREPEND}
  KDF_SECRET_PREPEND  = $1;
  {$EXTERNALSYM KDF_SECRET_APPEND}
  KDF_SECRET_APPEND   = $2;
  {$EXTERNALSYM KDF_HMAC_KEY}
  KDF_HMAC_KEY        = $3;
  {$EXTERNALSYM KDF_TLS_PRF_LABEL}
  KDF_TLS_PRF_LABEL   = $4;
  {$EXTERNALSYM KDF_TLS_PRF_SEED}
  KDF_TLS_PRF_SEED    = $5;
  {$EXTERNALSYM KDF_SECRET_HANDLE}
  KDF_SECRET_HANDLE   = $6;

//
// DeriveKey Flags:
//
// KDF_USE_SECRET_AS_HMAC_KEY_FLAG causes the secret agreement to serve also
// as the HMAC key.  If this flag is used, the KDF_HMAC_KEY parameter should
// NOT be specified.
//
  {$EXTERNALSYM KDF_USE_SECRET_AS_HMAC_KEY_FLAG}
  KDF_USE_SECRET_AS_HMAC_KEY_FLAG = $1;

//
// BCrypt structs
//

type
  PBCryptKeyLengthsStruct = ^TBCryptKeyLengthsStruct;
  {$EXTERNALSYM __BCRYPT_KEY_LENGTHS_STRUCT}
  __BCRYPT_KEY_LENGTHS_STRUCT = record
    dwMinLength: ULONG;
    dwMaxLength: ULONG;
    dwIncrement: ULONG;
  end;
  {$EXTERNALSYM BCRYPT_KEY_LENGTHS_STRUCT}
  BCRYPT_KEY_LENGTHS_STRUCT = __BCRYPT_KEY_LENGTHS_STRUCT;
  TBCryptKeyLengthsStruct = __BCRYPT_KEY_LENGTHS_STRUCT;

  PBCryptOID = ^TBCryptOID;
  {$EXTERNALSYM _BCRYPT_OID}
  _BCRYPT_OID = packed record
    cbOID: ULONG;
    pbOID: PUCHAR;
  end;
  {$EXTERNALSYM BCRYPT_OID}
  BCRYPT_OID = _BCRYPT_OID;
  TBCryptOID = _BCRYPT_OID;

  PBCryptOIDList = ^TBCryptOIDList;
  {$EXTERNALSYM _BCRYPT_OID_LIST}
  _BCRYPT_OID_LIST = packed record
    dwOIDCount: ULONG;
    pOIDs: PBCryptOID;
  end;
  {$EXTERNALSYM BCRYPT_OID_LIST}
  BCRYPT_OID_LIST = _BCRYPT_OID_LIST;
  TBCryptOIDList = _BCRYPT_OID_LIST;

  PBCryptPKCS1PaddingInfo = ^TBCryptPKCS1PaddingInfo;
  {$EXTERNALSYM _BCRYPT_PKCS1_PADDING_INFO}
  _BCRYPT_PKCS1_PADDING_INFO = record
    pszAlgId: LPCWSTR;
  end;
  {$EXTERNALSYM BCRYPT_PKCS1_PADDING_INFO}
  BCRYPT_PKCS1_PADDING_INFO = _BCRYPT_PKCS1_PADDING_INFO;
  TBCryptPKCS1PaddingInfo = _BCRYPT_PKCS1_PADDING_INFO;

  PBCryptPSSPaddingInfo = ^TBCryptPSSPaddingInfo;
  {$EXTERNALSYM _BCRYPT_PSS_PADDING_INFO}
  _BCRYPT_PSS_PADDING_INFO = record
    pszAlgId: LPCWSTR;
    cbSalt: ULONG;
  end;
  {$EXTERNALSYM BCRYPT_PSS_PADDING_INFO}
  BCRYPT_PSS_PADDING_INFO = _BCRYPT_PSS_PADDING_INFO;
  TBCryptPSSPaddingInfo = _BCRYPT_PSS_PADDING_INFO;

  PBCryptOAEPPaddingInfo = ^TBCryptOAEPPaddingInfo;
  {$EXTERNALSYM _BCRYPT_OAEP_PADDING_INFO}
  _BCRYPT_OAEP_PADDING_INFO = record
    pszAlgId: LPCWSTR;
    pbLabel: PUCHAR;
    cbLabel: ULONG;
  end;
  {$EXTERNALSYM BCRYPT_OAEP_PADDING_INFO}
  BCRYPT_OAEP_PADDING_INFO = _BCRYPT_OAEP_PADDING_INFO;
  TBCryptOAEPPaddingInfo = _BCRYPT_OAEP_PADDING_INFO;

//
// BCrypt String Properties
//

const
// BCrypt(Import/Export)Key BLOB types
  {$EXTERNALSYM BCRYPT_OPAQUE_KEY_BLOB}
  BCRYPT_OPAQUE_KEY_BLOB      = 'OpaqueKeyBlob';
  {$EXTERNALSYM BCRYPT_KEY_DATA_BLOB}
  BCRYPT_KEY_DATA_BLOB        = 'KeyDataBlob';

// BCryptGetProperty strings
  {$EXTERNALSYM BCRYPT_OBJECT_LENGTH}
  BCRYPT_OBJECT_LENGTH        = 'ObjectLength';
  {$EXTERNALSYM BCRYPT_ALGORITHM_NAME}
  BCRYPT_ALGORITHM_NAME       = 'AlgorithmName';
  {$EXTERNALSYM BCRYPT_PROVIDER_HANDLE}
  BCRYPT_PROVIDER_HANDLE      = 'ProviderHandle';
  {$EXTERNALSYM BCRYPT_CHAINING_MODE}
  BCRYPT_CHAINING_MODE        = 'ChainingMode';
  {$EXTERNALSYM BCRYPT_BLOCK_LENGTH}
  BCRYPT_BLOCK_LENGTH         = 'BlockLength';
  {$EXTERNALSYM BCRYPT_KEY_LENGTH}
  BCRYPT_KEY_LENGTH           = 'KeyLength';
  {$EXTERNALSYM BCRYPT_KEY_OBJECT_LENGTH}
  BCRYPT_KEY_OBJECT_LENGTH    = 'KeyObjectLength';
  {$EXTERNALSYM BCRYPT_KEY_STRENGTH}
  BCRYPT_KEY_STRENGTH         = 'KeyStrength';
  {$EXTERNALSYM BCRYPT_KEY_LENGTHS}
  BCRYPT_KEY_LENGTHS          = 'KeyLengths';
  {$EXTERNALSYM BCRYPT_BLOCK_SIZE_LIST}
  BCRYPT_BLOCK_SIZE_LIST      = 'BlockSizeList';
  {$EXTERNALSYM BCRYPT_EFFECTIVE_KEY_LENGTH}
  BCRYPT_EFFECTIVE_KEY_LENGTH = 'EffectiveKeyLength';
  {$EXTERNALSYM BCRYPT_HASH_LENGTH}
  BCRYPT_HASH_LENGTH          = 'HashDigestLength';
  {$EXTERNALSYM BCRYPT_HASH_OID_LIST}
  BCRYPT_HASH_OID_LIST        = 'HashOIDList';
  {$EXTERNALSYM BCRYPT_PADDING_SCHEMES}
  BCRYPT_PADDING_SCHEMES      = 'PaddingSchemes';
  {$EXTERNALSYM BCRYPT_SIGNATURE_LENGTH}
  BCRYPT_SIGNATURE_LENGTH     = 'SignatureLength';
  {$EXTERNALSYM BCRYPT_HASH_BLOCK_LENGTH}
  BCRYPT_HASH_BLOCK_LENGTH    = 'HashBlockLength';

// BCryptSetProperty strings
  {$EXTERNALSYM BCRYPT_INITIALIZATION_VECTOR}
  BCRYPT_INITIALIZATION_VECTOR    = 'IV';


// Property Strings
  {$EXTERNALSYM BCRYPT_CHAIN_MODE_NA}
  BCRYPT_CHAIN_MODE_NA        = 'ChainingModeN/A';
  {$EXTERNALSYM BCRYPT_CHAIN_MODE_CBC}
  BCRYPT_CHAIN_MODE_CBC       = 'ChainingModeCBC';
  {$EXTERNALSYM BCRYPT_CHAIN_MODE_ECB}
  BCRYPT_CHAIN_MODE_ECB       = 'ChainingModeECB';
  {$EXTERNALSYM BCRYPT_CHAIN_MODE_CFB}
  BCRYPT_CHAIN_MODE_CFB       = 'ChainingModeCFB';

// Supported RSA Padding Types
  {$EXTERNALSYM BCRYPT_SUPPORTED_PAD_ROUTER}
  BCRYPT_SUPPORTED_PAD_ROUTER     = $00000001;
  {$EXTERNALSYM BCRYPT_SUPPORTED_PAD_PKCS1_ENC}
  BCRYPT_SUPPORTED_PAD_PKCS1_ENC  = $00000002;
  {$EXTERNALSYM BCRYPT_SUPPORTED_PAD_PKCS1_SIG}
  BCRYPT_SUPPORTED_PAD_PKCS1_SIG  = $00000004;
  {$EXTERNALSYM BCRYPT_SUPPORTED_PAD_OAEP}
  BCRYPT_SUPPORTED_PAD_OAEP       = $00000008;
  {$EXTERNALSYM BCRYPT_SUPPORTED_PAD_PSS}
  BCRYPT_SUPPORTED_PAD_PSS        = $00000010;

//
//      BCrypt Flags
//

  {$EXTERNALSYM BCRYPT_PROV_DISPATCH}
  BCRYPT_PROV_DISPATCH        = $00000001;  // BCryptOpenAlgorithmProvider

  {$EXTERNALSYM BCRYPT_BLOCK_PADDING}
  BCRYPT_BLOCK_PADDING        = $00000001;  // BCryptEncrypt/Decrypt

// RSA padding schemes
  {$EXTERNALSYM BCRYPT_PAD_NONE}
  BCRYPT_PAD_NONE             = $00000001;
  {$EXTERNALSYM BCRYPT_PAD_PKCS1}
  BCRYPT_PAD_PKCS1            = $00000002;  // BCryptEncrypt/Decrypt BCryptSignHash/VerifySignature
  {$EXTERNALSYM BCRYPT_PAD_OAEP}
  BCRYPT_PAD_OAEP             = $00000004;  // BCryptEncrypt/Decrypt
  {$EXTERNALSYM BCRYPT_PAD_PSS}
  BCRYPT_PAD_PSS              = $00000008;  // BCryptSignHash/VerifySignature

  {$EXTERNALSYM BCRYPTBUFFER_VERSION}
  BCRYPTBUFFER_VERSION        = 0;

type
  {$EXTERNALSYM PBCryptBuffer}
  PBCryptBuffer = ^TBCryptBuffer;
  {$EXTERNALSYM _BCryptBuffer}
  _BCryptBuffer = record
    cbBuffer: ULONG;             // Length of buffer, in bytes
    BufferType: ULONG;           // Buffer type
    pvBuffer: Pointer;           // Pointer to buffer
  end;
  {$EXTERNALSYM BCryptBuffer}
  BCryptBuffer = _BCryptBuffer;
  TBCryptBuffer = _BCryptBuffer;

  {$EXTERNALSYM PBCryptBufferDesc}
  PBCryptBufferDesc = ^TBCryptBufferDesc;
  {$EXTERNALSYM _BCryptBufferDesc}
  _BCryptBufferDesc = record
    ulVersion: ULONG;            // Version number
    cBuffers: ULONG;             // Number of buffers
    pBuffers: PBCryptBuffer;       // Pointer to array of buffers
  end;
  {$EXTERNALSYM BCryptBufferDesc}
  BCryptBufferDesc = _BCryptBufferDesc;
  TBCryptBufferDesc = _BCryptBufferDesc;


//
// Primitive handles
//

  {$EXTERNALSYM BCRYPT_HANDLE}
  BCRYPT_HANDLE = THandle;
  {$EXTERNALSYM BCRYPT_ALG_HANDLE}
  BCRYPT_ALG_HANDLE = THandle;
  {$EXTERNALSYM BCRYPT_KEY_HANDLE}
  BCRYPT_KEY_HANDLE = THandle;
  {$EXTERNALSYM BCRYPT_HASH_HANDLE}
  BCRYPT_HASH_HANDLE = THandle;
  {$EXTERNALSYM BCRYPT_SECRET_HANDLE}
  BCRYPT_SECRET_HANDLE = THandle;


//
// Structures used to represent key blobs.
//

const
  {$EXTERNALSYM BCRYPT_PUBLIC_KEY_BLOB}
  BCRYPT_PUBLIC_KEY_BLOB       = 'PUBLICBLOB';
  {$EXTERNALSYM BCRYPT_PRIVATE_KEY_BLOB}
  BCRYPT_PRIVATE_KEY_BLOB      = 'PRIVATEBLOB';

type
  PBCryptKeyBlob = ^TBCryptKeyBlob;
  {$EXTERNALSYM _BCRYPT_KEY_BLOB}
  _BCRYPT_KEY_BLOB = record
    Magic: ULONG;
  end;
  {$EXTERNALSYM BCRYPT_KEY_BLOB}
  BCRYPT_KEY_BLOB = _BCRYPT_KEY_BLOB;
  TBCryptKeyBlob = _BCRYPT_KEY_BLOB;

// The BCRYPT_RSAPUBLIC_BLOB and BCRYPT_RSAPRIVATE_BLOB blob types are used
// to transport plaintext RSA keys. These blob types will be supported by
// all RSA primitive providers.
// The BCRYPT_RSAPRIVATE_BLOB includes the following values:
// Public Exponent
// Modulus
// Prime1
// Prime2

const
  {$EXTERNALSYM BCRYPT_RSAPUBLIC_BLOB}
  BCRYPT_RSAPUBLIC_BLOB       = 'RSAPUBLICBLOB';
  {$EXTERNALSYM BCRYPT_RSAPRIVATE_BLOB}
  BCRYPT_RSAPRIVATE_BLOB      = 'RSAPRIVATEBLOB';
  {$EXTERNALSYM LEGACY_RSAPUBLIC_BLOB}
  LEGACY_RSAPUBLIC_BLOB       = 'CAPIPUBLICBLOB';
  {$EXTERNALSYM LEGACY_RSAPRIVATE_BLOB}
  LEGACY_RSAPRIVATE_BLOB      = 'CAPIPRIVATEBLOB';

  {$EXTERNALSYM BCRYPT_RSAPUBLIC_MAGIC}
  BCRYPT_RSAPUBLIC_MAGIC = $31415352;  // RSA1
  {$EXTERNALSYM BCRYPT_RSAPRIVATE_MAGIC}
  BCRYPT_RSAPRIVATE_MAGIC = $32415352;  // RSA2

type
  PBCryptRSAKeyBlob = ^TBCryptRSAKeyBlob;
  {$EXTERNALSYM _BCRYPT_RSAKEY_BLOB}
  _BCRYPT_RSAKEY_BLOB = record
    Magic: ULONG;
    BitLength: ULONG;
    cbPublicExp: ULONG;
    cbModulus: ULONG;
    cbPrime1: ULONG;
    cbPrime2: ULONG;
  end;
  {$EXTERNALSYM BCRYPT_RSAKEY_BLOB}
  BCRYPT_RSAKEY_BLOB = _BCRYPT_RSAKEY_BLOB;
  TBCryptRSAKeyBlob = _BCRYPT_RSAKEY_BLOB;

const

// The BCRYPT_RSAFULLPRIVATE_BLOB blob type is used to transport
// plaintext private RSA keys.  It includes the following values:
// Public Exponent
// Modulus
// Prime1
// Prime2
// Private Exponent mod (Prime1 - 1)
// Private Exponent mod (Prime2 - 1)
// Inverse of Prime2 mod Prime1
// PrivateExponent
  {$EXTERNALSYM BCRYPT_RSAFULLPRIVATE_BLOB}
  BCRYPT_RSAFULLPRIVATE_BLOB      = 'RSAFULLPRIVATEBLOB';

  {$EXTERNALSYM BCRYPT_RSAFULLPRIVATE_MAGIC}
  BCRYPT_RSAFULLPRIVATE_MAGIC = $33415352;  // RSA3

// The BCRYPT_ECCPUBLIC_BLOB and BCRYPT_ECCPRIVATE_BLOB blob types are used
// to transport plaintext ECC keys. These blob types will be supported by
// all ECC primitive providers.
  {$EXTERNALSYM BCRYPT_ECCPUBLIC_BLOB}
  BCRYPT_ECCPUBLIC_BLOB           = 'ECCPUBLICBLOB';
  {$EXTERNALSYM BCRYPT_ECCPRIVATE_BLOB}
  BCRYPT_ECCPRIVATE_BLOB          = 'ECCPRIVATEBLOB';

  {$EXTERNALSYM BCRYPT_ECDH_PUBLIC_P256_MAGIC}
  BCRYPT_ECDH_PUBLIC_P256_MAGIC   = $314B4345;  // ECK1
  {$EXTERNALSYM BCRYPT_ECDH_PRIVATE_P256_MAGIC}
  BCRYPT_ECDH_PRIVATE_P256_MAGIC  = $324B4345;  // ECK2
  {$EXTERNALSYM BCRYPT_ECDH_PUBLIC_P384_MAGIC}
  BCRYPT_ECDH_PUBLIC_P384_MAGIC   = $334B4345;  // ECK3
  {$EXTERNALSYM BCRYPT_ECDH_PRIVATE_P384_MAGIC}
  BCRYPT_ECDH_PRIVATE_P384_MAGIC  = $344B4345;  // ECK4
  {$EXTERNALSYM BCRYPT_ECDH_PUBLIC_P521_MAGIC}
  BCRYPT_ECDH_PUBLIC_P521_MAGIC   = $354B4345;  // ECK5
  {$EXTERNALSYM BCRYPT_ECDH_PRIVATE_P521_MAGIC}
  BCRYPT_ECDH_PRIVATE_P521_MAGIC  = $364B4345;  // ECK6

  {$EXTERNALSYM BCRYPT_ECDSA_PUBLIC_P256_MAGIC}
  BCRYPT_ECDSA_PUBLIC_P256_MAGIC  = $31534345;  // ECS1
  {$EXTERNALSYM BCRYPT_ECDSA_PRIVATE_P256_MAGIC}
  BCRYPT_ECDSA_PRIVATE_P256_MAGIC = $32534345;  // ECS2
  {$EXTERNALSYM BCRYPT_ECDSA_PUBLIC_P384_MAGIC}
  BCRYPT_ECDSA_PUBLIC_P384_MAGIC  = $33534345;  // ECS3
  {$EXTERNALSYM BCRYPT_ECDSA_PRIVATE_P384_MAGIC}
  BCRYPT_ECDSA_PRIVATE_P384_MAGIC = $34534345;  // ECS4
  {$EXTERNALSYM BCRYPT_ECDSA_PUBLIC_P521_MAGIC}
  BCRYPT_ECDSA_PUBLIC_P521_MAGIC  = $35534345;  // ECS5
  {$EXTERNALSYM BCRYPT_ECDSA_PRIVATE_P521_MAGIC}
  BCRYPT_ECDSA_PRIVATE_P521_MAGIC = $36534345;  // ECS6

type  
  PBCryptECCKeyBlob = ^TBCryptECCKeyBlob;
  {$EXTERNALSYM _BCRYPT_ECCKEY_BLOB}
  _BCRYPT_ECCKEY_BLOB = record
    dwMagic: ULONG;
    cbKey: ULONG;
  end;
  {$EXTERNALSYM BCRYPT_ECCKEY_BLOB}
  BCRYPT_ECCKEY_BLOB = _BCRYPT_ECCKEY_BLOB;
  TBCryptECCKeyBlob = _BCRYPT_ECCKEY_BLOB;

const
// The BCRYPT_DH_PUBLIC_BLOB and BCRYPT_DH_PRIVATE_BLOB blob types are used
// to transport plaintext DH keys. These blob types will be supported by
// all DH primitive providers.
  {$EXTERNALSYM BCRYPT_DH_PUBLIC_BLOB}
  BCRYPT_DH_PUBLIC_BLOB           = 'DHPUBLICBLOB';
  {$EXTERNALSYM BCRYPT_DH_PRIVATE_BLOB}
  BCRYPT_DH_PRIVATE_BLOB          = 'DHPRIVATEBLOB';
  {$EXTERNALSYM LEGACY_DH_PUBLIC_BLOB}
  LEGACY_DH_PUBLIC_BLOB           = 'CAPIDHPUBLICBLOB';
  {$EXTERNALSYM LEGACY_DH_PRIVATE_BLOB}
  LEGACY_DH_PRIVATE_BLOB          = 'CAPIDHPRIVATEBLOB';

  {$EXTERNALSYM BCRYPT_DH_PUBLIC_MAGIC}
  BCRYPT_DH_PUBLIC_MAGIC          = $42504844;  // DHPB
  {$EXTERNALSYM BCRYPT_DH_PRIVATE_MAGIC}
  BCRYPT_DH_PRIVATE_MAGIC         = $56504844;  // DHPV

type
  PBCryptDHKeyBlob = ^TBCryptDHKeyBlob;
  {$EXTERNALSYM _BCRYPT_DH_KEY_BLOB}
  _BCRYPT_DH_KEY_BLOB = record
    dwMagic: ULONG;
    cbKey: ULONG;
  end;
  {$EXTERNALSYM BCRYPT_DH_KEY_BLOB}
  BCRYPT_DH_KEY_BLOB = _BCRYPT_DH_KEY_BLOB;
  TBCryptDHKeyBlob = _BCRYPT_DH_KEY_BLOB;

const
// Property Strings for DH
  {$EXTERNALSYM BCRYPT_DH_PARAMETERS}
  BCRYPT_DH_PARAMETERS            = 'DHParameters';

  {$EXTERNALSYM BCRYPT_DH_PARAMETERS_MAGIC}
  BCRYPT_DH_PARAMETERS_MAGIC      = $4d504844;  // DHPM

type
  PBCryptDHParameterHeader = ^TBCryptDHParameterHeader;
  {$EXTERNALSYM _BCRYPT_DH_PARAMETER_HEADER}
  _BCRYPT_DH_PARAMETER_HEADER = record
    cbLength: ULONG;
    dwMagic: ULONG;
    cbKeyLength: ULONG;
  end;
  {$EXTERNALSYM BCRYPT_DH_PARAMETER_HEADER}
  BCRYPT_DH_PARAMETER_HEADER = _BCRYPT_DH_PARAMETER_HEADER;
  TBCryptDHParameterHeader = _BCRYPT_DH_PARAMETER_HEADER;

const
// The BCRYPT_DSA_PUBLIC_BLOB and BCRYPT_DSA_PRIVATE_BLOB blob types are used
// to transport plaintext DSA keys. These blob types will be supported by
// all DSA primitive providers.
  {$EXTERNALSYM BCRYPT_DSA_PUBLIC_BLOB}
  BCRYPT_DSA_PUBLIC_BLOB          = 'DSAPUBLICBLOB';
  {$EXTERNALSYM BCRYPT_DSA_PRIVATE_BLOB}
  BCRYPT_DSA_PRIVATE_BLOB         = 'DSAPRIVATEBLOB';
  {$EXTERNALSYM LEGACY_DSA_PUBLIC_BLOB}
  LEGACY_DSA_PUBLIC_BLOB          = 'CAPIDSAPUBLICBLOB';
  {$EXTERNALSYM LEGACY_DSA_PRIVATE_BLOB}
  LEGACY_DSA_PRIVATE_BLOB         = 'CAPIDSAPRIVATEBLOB';
  {$EXTERNALSYM LEGACY_DSA_V2_PRIVATE_BLOB}
  LEGACY_DSA_V2_PRIVATE_BLOB      = 'V2CAPIDSAPRIVATEBLOB';

  {$EXTERNALSYM BCRYPT_DSA_PUBLIC_MAGIC}
  BCRYPT_DSA_PUBLIC_MAGIC         = $42505344;  // DSPB
  {$EXTERNALSYM BCRYPT_DSA_PRIVATE_MAGIC}
  BCRYPT_DSA_PRIVATE_MAGIC        = $56505344;  // DSPV

type
  PBCryptDSAKeyBlob = ^TBCryptDSAKeyBlob;
  {$EXTERNALSYM _BCRYPT_DSA_KEY_BLOB}
  _BCRYPT_DSA_KEY_BLOB = record
    dwMagic: ULONG;
    cbKey: ULONG;
    Count: array[0..3] of UCHAR;
    Seed: array[0..19] of UCHAR;
    q: array[0..19] of UCHAR;
  end;
  {$EXTERNALSYM BCRYPT_DSA_KEY_BLOB}
  BCRYPT_DSA_KEY_BLOB = _BCRYPT_DSA_KEY_BLOB;
  TBCryptDSAKeyBlob = _BCRYPT_DSA_KEY_BLOB;

  PBCryptKeyDataBlobHeader = ^TBCryptKeyDataBlobHeader;
  {$EXTERNALSYM _BCRYPT_KEY_DATA_BLOB_HEADER}
  _BCRYPT_KEY_DATA_BLOB_HEADER = record
    dwMagic: ULONG;
    dwVersion: ULONG;
    cbKeyData: ULONG;
  end;
  {$EXTERNALSYM BCRYPT_KEY_DATA_BLOB_HEADER}
  BCRYPT_KEY_DATA_BLOB_HEADER = _BCRYPT_KEY_DATA_BLOB_HEADER;
  TBCryptKeyDataBlobHeader = _BCRYPT_KEY_DATA_BLOB_HEADER;

const
  {$EXTERNALSYM BCRYPT_KEY_DATA_BLOB_MAGIC}
  BCRYPT_KEY_DATA_BLOB_MAGIC      = $4d42444b; //Key Data Blob Magic (KDBM)

  {$EXTERNALSYM BCRYPT_KEY_DATA_BLOB_VERSION1}
  BCRYPT_KEY_DATA_BLOB_VERSION1   = $1;

// Property Strings for DSA
  {$EXTERNALSYM BCRYPT_DSA_PARAMETERS}
  BCRYPT_DSA_PARAMETERS           = 'DSAParameters';

  {$EXTERNALSYM BCRYPT_DSA_PARAMETERS_MAGIC}
  BCRYPT_DSA_PARAMETERS_MAGIC     = $4d505344;  // DSPM

type
  PBCryptDSAParameterHeader = ^TBCryptDSAParameterHeader;
  {$EXTERNALSYM _BCRYPT_DSA_PARAMETER_HEADER}
  _BCRYPT_DSA_PARAMETER_HEADER = record
    cbLength: ULONG;
    dwMagic: ULONG;
    cbKeyLength: ULONG;
    Count: array[0..3] of UCHAR;
    Seed: array[0..19] of UCHAR;
    q: array[0..19] of UCHAR;
  end;
  {$EXTERNALSYM BCRYPT_DSA_PARAMETER_HEADER}
  BCRYPT_DSA_PARAMETER_HEADER = _BCRYPT_DSA_PARAMETER_HEADER;
  TBCryptDSAParameterHeader = _BCRYPT_DSA_PARAMETER_HEADER;

const
//
// Microsoft built-in providers.
//

  {$EXTERNALSYM MS_PRIMITIVE_PROVIDER}
  MS_PRIMITIVE_PROVIDER                   = 'Microsoft Primitive Provider';

//
// Common algorithm identifiers.
//

  {$EXTERNALSYM BCRYPT_RSA_ALGORITHM}
  BCRYPT_RSA_ALGORITHM                    = 'RSA';
  {$EXTERNALSYM BCRYPT_RSA_SIGN_ALGORITHM}
  BCRYPT_RSA_SIGN_ALGORITHM               = 'RSA_SIGN';
  {$EXTERNALSYM BCRYPT_DH_ALGORITHM}
  BCRYPT_DH_ALGORITHM                     = 'DH';
  {$EXTERNALSYM BCRYPT_DSA_ALGORITHM}
  BCRYPT_DSA_ALGORITHM                    = 'DSA';
  {$EXTERNALSYM BCRYPT_RC2_ALGORITHM}
  BCRYPT_RC2_ALGORITHM                    = 'RC2';
  {$EXTERNALSYM BCRYPT_RC4_ALGORITHM}
  BCRYPT_RC4_ALGORITHM                    = 'RC4';
  {$EXTERNALSYM BCRYPT_AES_ALGORITHM}
  BCRYPT_AES_ALGORITHM                    = 'AES';
  {$EXTERNALSYM BCRYPT_DES_ALGORITHM}
  BCRYPT_DES_ALGORITHM                    = 'DES';
  {$EXTERNALSYM BCRYPT_DESX_ALGORITHM}
  BCRYPT_DESX_ALGORITHM                   = 'DESX';
  {$EXTERNALSYM BCRYPT_3DES_ALGORITHM}
  BCRYPT_3DES_ALGORITHM                   = '3DES';
  {$EXTERNALSYM BCRYPT_3DES_112_ALGORITHM}
  BCRYPT_3DES_112_ALGORITHM               = '3DES_112';
  {$EXTERNALSYM BCRYPT_MD2_ALGORITHM}
  BCRYPT_MD2_ALGORITHM                    = 'MD2';
  {$EXTERNALSYM BCRYPT_MD4_ALGORITHM}
  BCRYPT_MD4_ALGORITHM                    = 'MD4';
  {$EXTERNALSYM BCRYPT_MD5_ALGORITHM}
  BCRYPT_MD5_ALGORITHM                    = 'MD5';
  {$EXTERNALSYM BCRYPT_SHA1_ALGORITHM}
  BCRYPT_SHA1_ALGORITHM                   = 'SHA1';
  {$EXTERNALSYM BCRYPT_SHA256_ALGORITHM}
  BCRYPT_SHA256_ALGORITHM                 = 'SHA256';
  {$EXTERNALSYM BCRYPT_SHA384_ALGORITHM}
  BCRYPT_SHA384_ALGORITHM                 = 'SHA384';
  {$EXTERNALSYM BCRYPT_SHA512_ALGORITHM}
  BCRYPT_SHA512_ALGORITHM                 = 'SHA512';
  {$EXTERNALSYM BCRYPT_ECDSA_P256_ALGORITHM}
  BCRYPT_ECDSA_P256_ALGORITHM             = 'ECDSA_P256';
  {$EXTERNALSYM BCRYPT_ECDSA_P384_ALGORITHM}
  BCRYPT_ECDSA_P384_ALGORITHM             = 'ECDSA_P384';
  {$EXTERNALSYM BCRYPT_ECDSA_P521_ALGORITHM}
  BCRYPT_ECDSA_P521_ALGORITHM             = 'ECDSA_P521';
  {$EXTERNALSYM BCRYPT_ECDH_P256_ALGORITHM}
  BCRYPT_ECDH_P256_ALGORITHM              = 'ECDH_P256';
  {$EXTERNALSYM BCRYPT_ECDH_P384_ALGORITHM}
  BCRYPT_ECDH_P384_ALGORITHM              = 'ECDH_P384';
  {$EXTERNALSYM BCRYPT_ECDH_P521_ALGORITHM}
  BCRYPT_ECDH_P521_ALGORITHM              = 'ECDH_P521';
  {$EXTERNALSYM BCRYPT_RNG_ALGORITHM}
  BCRYPT_RNG_ALGORITHM                    = 'RNG';
  {$EXTERNALSYM BCRYPT_RNG_FIPS186_DSA_ALGORITHM}
  BCRYPT_RNG_FIPS186_DSA_ALGORITHM        = 'FIPS186DSARNG';

//
// Interfaces
//

  {$EXTERNALSYM BCRYPT_CIPHER_INTERFACE}
  BCRYPT_CIPHER_INTERFACE                 = $00000001;
  {$EXTERNALSYM BCRYPT_HASH_INTERFACE}
  BCRYPT_HASH_INTERFACE                   = $00000002;
  {$EXTERNALSYM BCRYPT_ASYMMETRIC_ENCRYPTION_INTERFACE}
  BCRYPT_ASYMMETRIC_ENCRYPTION_INTERFACE  = $00000003;
  {$EXTERNALSYM BCRYPT_SECRET_AGREEMENT_INTERFACE}
  BCRYPT_SECRET_AGREEMENT_INTERFACE       = $00000004;
  {$EXTERNALSYM BCRYPT_SIGNATURE_INTERFACE}
  BCRYPT_SIGNATURE_INTERFACE              = $00000005;
  {$EXTERNALSYM BCRYPT_RNG_INTERFACE}
  BCRYPT_RNG_INTERFACE                    = $00000006;

//
// Primitive algorithm provider functions.
//

  {$EXTERNALSYM BCRYPT_ALG_HANDLE_HMAC_FLAG}
  BCRYPT_ALG_HANDLE_HMAC_FLAG = $00000008;

{$EXTERNALSYM BCryptOpenAlgorithmProvider}
function BCryptOpenAlgorithmProvider(out phAlgorithm: BCRYPT_ALG_HANDLE;
  pszAlgId, pszImplementation: LPCWSTR; dwFlags: ULONG): TNTStatus; stdcall;

type
  {$EXTERNALSYM BCryptOpenAlgorithmProviderFn}
  BCryptOpenAlgorithmProviderFn = function(out phAlgorithm: BCRYPT_ALG_HANDLE;
    pszAlgId: LPCWSTR; dwFlags: ULONG): TNTStatus stdcall;

const
// AlgOperations flags for use with BCryptEnumAlgorithms()
  {$EXTERNALSYM BCRYPT_CIPHER_OPERATION}
  BCRYPT_CIPHER_OPERATION                 = $00000001;
  {$EXTERNALSYM BCRYPT_HASH_OPERATION}
  BCRYPT_HASH_OPERATION                   = $00000002;
  {$EXTERNALSYM BCRYPT_ASYMMETRIC_ENCRYPTION_OPERATION}
  BCRYPT_ASYMMETRIC_ENCRYPTION_OPERATION  = $00000004;
  {$EXTERNALSYM BCRYPT_SECRET_AGREEMENT_OPERATION}
  BCRYPT_SECRET_AGREEMENT_OPERATION       = $00000008;
  {$EXTERNALSYM BCRYPT_SIGNATURE_OPERATION}
  BCRYPT_SIGNATURE_OPERATION              = $00000010;
  {$EXTERNALSYM BCRYPT_RNG_OPERATION}
  BCRYPT_RNG_OPERATION                    = $00000020;

// USE EXTREME CAUTION: editing comments that contain "certenrolls_*" tokens
// could break building CertEnroll idl files:
// certenrolls_begin -- BCRYPT_ALGORITHM_IDENTIFIER
type
  PBCryptAlgorithmIdentifier = ^TBCryptAlgorithmIdentifier;
  {$EXTERNALSYM _BCRYPT_ALGORITHM_IDENTIFIER}
  _BCRYPT_ALGORITHM_IDENTIFIER = record
    pszName: LPWSTR;
    dwClass: ULONG;
    dwFlags: ULONG;
  end;
  {$EXTERNALSYM BCRYPT_ALGORITHM_IDENTIFIER}
  BCRYPT_ALGORITHM_IDENTIFIER = _BCRYPT_ALGORITHM_IDENTIFIER;
  TBCryptAlgorithmIdentifier = _BCRYPT_ALGORITHM_IDENTIFIER;
// certenrolls_end

{$EXTERNALSYM BCryptEnumAlgorithms}
function BCryptEnumAlgorithms(dwAlgOperations: ULONG; out pAlgCount: ULONG;
  out ppAlgList: PBCryptAlgorithmIdentifier; dwFlags: ULONG): TNTStatus; stdcall;

type
  PBCryptProviderName = ^TBCryptProviderName;
  {$EXTERNALSYM _BCRYPT_PROVIDER_NAME}
  _BCRYPT_PROVIDER_NAME = record
    pszProviderName: LPWSTR;
  end;
  {$EXTERNALSYM BCRYPT_PROVIDER_NAME}
  BCRYPT_PROVIDER_NAME = _BCRYPT_PROVIDER_NAME;
  TBCryptProviderName = _BCRYPT_PROVIDER_NAME;

{$EXTERNALSYM BCryptEnumProviders}
function BCryptEnumProviders(pszAlgId: LPCWSTR; out pImplCount: ULONG;
  out ppImplList: PBCryptProviderName; dwFlags: ULONG): TNTStatus; stdcall;

const
  // Flags for use with BCryptGetProperty and BCryptSetProperty
  {$EXTERNALSYM BCRYPT_PUBLIC_KEY_FLAG}
  BCRYPT_PUBLIC_KEY_FLAG                  = $00000001;
  {$EXTERNALSYM BCRYPT_PRIVATE_KEY_FLAG}
  BCRYPT_PRIVATE_KEY_FLAG                 = $00000002;


{$EXTERNALSYM BCryptGetProperty}
function BCryptGetProperty(hObject: BCRYPT_HANDLE; pszProperty: LPCWSTR;
  pbOutput: PUCHAR; cbOutput: ULONG; out pcbResult: ULONG;
  dwFlags: ULONG): TNTStatus; stdcall;

{$EXTERNALSYM BCryptSetProperty}
function BCryptSetProperty(hObject: BCRYPT_HANDLE; pszProperty: LPCWSTR;
  pbInput: PUCHAR; cbInput: ULONG; dwFlags: ULONG): TNTStatus; stdcall;

{$EXTERNALSYM BCryptCloseAlgorithmProvider}
function BCryptCloseAlgorithmProvider(hAlgorithm: BCRYPT_ALG_HANDLE;
  dwFlags: ULONG): TNTStatus; stdcall;

{$EXTERNALSYM BCryptFreeBuffer}
procedure BCryptFreeBuffer(pvBuffer: Pointer); stdcall;

//
// Primitive encryption functions.
//

{$EXTERNALSYM BCryptGenerateSymmetricKey}
function BCryptGenerateSymmetricKey(hAlgorithm: BCRYPT_ALG_HANDLE;
  out phKey: BCRYPT_KEY_HANDLE; pbKeyObject: PUCHAR; cbKeyObject: ULONG;
  pbSecret: PUCHAR; cbSecret, dwFlags: ULONG): TNTStatus; stdcall;

{$EXTERNALSYM BCryptGenerateKeyPair}
function BCryptGenerateKeyPair(hAlgorithm: BCRYPT_ALG_HANDLE;
  out phKey: BCRYPT_KEY_HANDLE; dwLength, dwFlags: ULONG): TNTStatus; stdcall;

{$EXTERNALSYM BCryptEncrypt}
function BCryptEncrypt(hKey: BCRYPT_KEY_HANDLE; pbInput: PUCHAR;
  cbInput: ULONG; pPaddingInfo: Pointer; pbIV: PUCHAR; cbIV: ULONG;
  pbOutput: PUCHAR; cbOutput: ULONG; out pcbResult: ULONG;
  dwFlags: ULONG): TNTStatus; stdcall;

{$EXTERNALSYM BCryptDecrypt}
function BCryptDecrypt(hKey: BCRYPT_KEY_HANDLE; pbInput: PUCHAR;
  cbInput: ULONG; pPaddingInfo: Pointer; pbIV: PUCHAR; cbIV: ULONG;
  pbOutput: PUCHAR; cbOutput: ULONG; out pcbResult: ULONG;
  dwFlags: ULONG): TNTStatus; stdcall;

{$EXTERNALSYM BCryptExportKey}
function BCryptExportKey(hKey, hExportKey: BCRYPT_KEY_HANDLE;
  pszBlobType: LPCWSTR; pbOutput: PUCHAR; cbOutput: ULONG;
  out pcbResult: ULONG; dwFlags: ULONG): TNTStatus; stdcall;

{$EXTERNALSYM BCryptImportKey}
function BCryptImportKey(hAlgorithm: BCRYPT_ALG_HANDLE;
  hImportKey: BCRYPT_KEY_HANDLE; pszBlobType: LPCWSTR;
  out phKey: BCRYPT_KEY_HANDLE; pbKeyObject: PUCHAR; cbKeyObject: ULONG;
  pbInput: PUCHAR; cbInput, dwFlags: ULONG): TNTStatus; stdcall;

const
  {$EXTERNALSYM BCRYPT_NO_KEY_VALIDATION}
  BCRYPT_NO_KEY_VALIDATION = $00000008;

{$EXTERNALSYM BCryptImportKeyPair}
function BCryptImportKeyPair(hAlgorithm: BCRYPT_ALG_HANDLE;
  hImportKey: BCRYPT_KEY_HANDLE; pszBlobType: LPCWSTR;
  out phKey: BCRYPT_KEY_HANDLE; pbInput: PUCHAR; cbInput: ULONG;
  dwFlags: ULONG): TNTStatus; stdcall;

{$EXTERNALSYM BCryptDuplicateKey}
function BCryptDuplicateKey(hKey: BCRYPT_KEY_HANDLE;
  out phNewKey: BCRYPT_KEY_HANDLE; pbKeyObject: PUCHAR;
  cbKeyObject, dwFlags: ULONG): TNTStatus; stdcall;

{$EXTERNALSYM BCryptFinalizeKeyPair}
function BCryptFinalizeKeyPair(hKey: BCRYPT_KEY_HANDLE;
  dwFlags: ULONG): TNTStatus; stdcall;

{$EXTERNALSYM BCryptDestroyKey}
function BCryptDestroyKey(hKey: BCRYPT_KEY_HANDLE): TNTStatus; stdcall;

{$EXTERNALSYM BCryptDestroySecret}
function BCryptDestroySecret(hSecret: BCRYPT_SECRET_HANDLE): TNTStatus; stdcall;

{$EXTERNALSYM BCryptSignHash}
function BCryptSignHash(hKey: BCRYPT_KEY_HANDLE; pPaddingInfo: Pointer;
  pbInput: PUCHAR; cbInput: ULONG; pbOutput: PUCHAR; cbOutput: ULONG;
  out pcbResult: ULONG; dwFlags: ULONG): TNTStatus; stdcall;

{$EXTERNALSYM BCryptVerifySignature}
function BCryptVerifySignature(hKey: BCRYPT_KEY_HANDLE; pPaddingInfo: Pointer;
  pbHash: PUCHAR; cbHash: ULONG; pbSignature: PUCHAR;
  cbSignature, dwFlags: ULONG): TNTStatus; stdcall;

{$EXTERNALSYM BCryptSecretAgreement}
function BCryptSecretAgreement(hPrivKey, hPubKey: BCRYPT_KEY_HANDLE;
  out phAgreedSecret: BCRYPT_SECRET_HANDLE; dwFlags: ULONG): TNTStatus; stdcall;

{$EXTERNALSYM BCryptDeriveKey}
function BCryptDeriveKey(hSharedSecret: BCRYPT_SECRET_HANDLE; pwszKDF: LPCWSTR;
  pParameterList: PBCryptBufferDesc; pbDerivedKey: PUCHAR; cbDerivedKey: ULONG;
  out pcbResult: ULONG; dwFlags: ULONG): TNTStatus; stdcall;

//
// Primitive hashing functions.
//

{$EXTERNALSYM BCryptCreateHash}
function BCryptCreateHash(hAlgorithm: BCRYPT_ALG_HANDLE;
  out phHash: BCRYPT_HASH_HANDLE; pbHashObject: PUCHAR; cbHashObject: ULONG;
  pbSecret: PUCHAR { optional }; cbSecret: ULONG { optional };
  dwFlags: ULONG): TNTStatus; stdcall;

{$EXTERNALSYM BCryptHashData}
function BCryptHashData(hHash: BCRYPT_HASH_HANDLE; pbInput: PUCHAR;
  cbInput, dwFlags: ULONG): TNTStatus; stdcall;

{$EXTERNALSYM BCryptFinishHash}
function BCryptFinishHash(hHash: BCRYPT_HASH_HANDLE; pbOutput: PUCHAR;
  cbOutput, dwFlags: ULONG): TNTStatus; stdcall;

{$EXTERNALSYM BCryptDuplicateHash}
function BCryptDuplicateHash(hHash: BCRYPT_HASH_HANDLE;
  out phNewHash: BCRYPT_HASH_HANDLE; pbHashObject: PUCHAR;
  cbHashObject, dwFlags: ULONG): TNTStatus; stdcall;

{$EXTERNALSYM BCryptDestroyHash}
function BCryptDestroyHash(hHash: BCRYPT_HASH_HANDLE): TNTStatus; stdcall;

//
// Primitive random number generation.
//

const
// Flags to BCryptGenRandom
  BCRYPT_RNG_USE_ENTROPY_IN_BUFFER        = $00000001;

{$EXTERNALSYM BCryptGenRandom}
function BCryptGenRandom(hAlgorithm: BCRYPT_ALG_HANDLE; pbBuffer: PUCHAR;
  cbBuffer, dwFlags: ULONG): TNTStatus; stdcall;

//
// Interface version control...
//
type
  PBCryptInterfaceVersion = ^TBCryptInterfaceVersion;
  {$EXTERNALSYM _BCRYPT_INTERFACE_VERSION}
  _BCRYPT_INTERFACE_VERSION = record
    MajorVersion: Word;
    MinorVersion: Word;
  end;
  {$EXTERNALSYM BCRYPT_INTERFACE_VERSION}
  BCRYPT_INTERFACE_VERSION = _BCRYPT_INTERFACE_VERSION;
  TBCryptInterfaceVersion = _BCRYPT_INTERFACE_VERSION;

{$EXTERNALSYM BCRYPT_MAKE_INTERFACE_VERSION}
function BCRYPT_MAKE_INTERFACE_VERSION(
  major, minor: Word): TBCryptInterfaceVersion;

{$EXTERNALSYM BCRYPT_IS_INTERFACE_VERSION_COMPATIBLE}
function BCRYPT_IS_INTERFACE_VERSION_COMPATIBLE(
  const loader, provider: TBCryptInterfaceVersion): Boolean;

//
// Primitive provider interfaces.
//

const
  {$EXTERNALSYM BCRYPT_CIPHER_INTERFACE_VERSION_1}
  BCRYPT_CIPHER_INTERFACE_VERSION_1: TBCryptInterfaceVersion =
    (MajorVersion: 1; MinorVersion: 0);

  {$EXTERNALSYM BCRYPT_HASH_INTERFACE_VERSION_1}
  BCRYPT_HASH_INTERFACE_VERSION_1: TBCryptInterfaceVersion =
    (MajorVersion: 1; MinorVersion: 0);

  {$EXTERNALSYM BCRYPT_ASYMMETRIC_ENCRYPTION_INTERFACE_VERSION_1}
  BCRYPT_ASYMMETRIC_ENCRYPTION_INTERFACE_VERSION_1: TBCryptInterfaceVersion =
    (MajorVersion: 1; MinorVersion: 0);

  {$EXTERNALSYM BCRYPT_SECRET_AGREEMENT_INTERFACE_VERSION_1}
  BCRYPT_SECRET_AGREEMENT_INTERFACE_VERSION_1: TBCryptInterfaceVersion =
    (MajorVersion: 1; MinorVersion: 0);

  {$EXTERNALSYM BCRYPT_SIGNATURE_INTERFACE_VERSION_1}
  BCRYPT_SIGNATURE_INTERFACE_VERSION_1: TBCryptInterfaceVersion =
    (MajorVersion: 1; MinorVersion: 0);

  {$EXTERNALSYM BCRYPT_RNG_INTERFACE_VERSION_1}
  BCRYPT_RNG_INTERFACE_VERSION_1: TBCryptInterfaceVersion =
    (MajorVersion: 1; MinorVersion: 0);

//////////////////////////////////////////////////////////////////////////////
// CryptoConfig Definitions //////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////

const
// Interface registration flags
  {$EXTERNALSYM CRYPT_MIN_DEPENDENCIES}
  CRYPT_MIN_DEPENDENCIES      = $00000001;
  {$EXTERNALSYM CRYPT_PROCESS_ISOLATE}
  CRYPT_PROCESS_ISOLATE       = $00010000;  // User-mode only

// Processor modes supported by a provider
//
// = Valid for BCryptQueryProviderRegistration and BCryptResolveProviders; :
//
  {$EXTERNALSYM CRYPT_UM}
  CRYPT_UM                    = $00000001;     // User mode only
  {$EXTERNALSYM CRYPT_KM}
  CRYPT_KM                    = $00000002;     // Kernel mode only
  {$EXTERNALSYM CRYPT_MM}
  CRYPT_MM                    = $00000003;     // Multi-mode: Must support BOTH UM and KM
//
// = Valid only for BCryptQueryProviderRegistration; :
//
  {$EXTERNALSYM CRYPT_ANY}
  CRYPT_ANY                   = $00000004;     // Wildcard: Either UM, or KM, or both

// Write behavior flags
  {$EXTERNALSYM CRYPT_OVERWRITE}
  CRYPT_OVERWRITE             = $00000001;

// Configuration tables
  {$EXTERNALSYM CRYPT_LOCAL}
  CRYPT_LOCAL                 = $00000001;
  {$EXTERNALSYM CRYPT_DOMAIN}
  CRYPT_DOMAIN                = $00000002;

// Context configuration flags
  {$EXTERNALSYM CRYPT_EXCLUSIVE}
  CRYPT_EXCLUSIVE             = $00000001;
  {$EXTERNALSYM CRYPT_OVERRIDE}
  CRYPT_OVERRIDE              = $00010000;  // Enterprise table only

// Resolution and enumeration flags
  {$EXTERNALSYM CRYPT_ALL_FUNCTIONS}
  CRYPT_ALL_FUNCTIONS         = $00000001;
  {$EXTERNALSYM CRYPT_ALL_PROVIDERS}
  CRYPT_ALL_PROVIDERS         = $00000002;

// Priority list positions
  {$EXTERNALSYM CRYPT_PRIORITY_TOP}
  CRYPT_PRIORITY_TOP          = $00000000;
  {$EXTERNALSYM CRYPT_PRIORITY_BOTTOM}
  CRYPT_PRIORITY_BOTTOM       = $FFFFFFFF;

// Default system-wide context
  {$EXTERNALSYM CRYPT_DEFAULT_CONTEXT}
  CRYPT_DEFAULT_CONTEXT       = 'Default';

//////////////////////////////////////////////////////////////////////////////
// CryptoConfig Structures ///////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////

//
// Provider Registration Structures
//

type
  PCryptInterfaceReg = ^TCryptInterfaceReg;
  {$EXTERNALSYM _CRYPT_INTERFACE_REG}
  _CRYPT_INTERFACE_REG = record
    dwInterface: ULONG;
    dwFlags: ULONG;

    cFunctions: ULONG;
    rgpszFunctions: ^PWideChar;
  end;
  {$EXTERNALSYM CRYPT_INTERFACE_REG}
  CRYPT_INTERFACE_REG = _CRYPT_INTERFACE_REG;
  TCryptInterfaceReg = _CRYPT_INTERFACE_REG;

  PCryptImageReg = ^TCryptImageReg;
  {$EXTERNALSYM _CRYPT_IMAGE_REG}
  _CRYPT_IMAGE_REG = record
    pszImage: PWideChar;

    cInterfaces: ULONG;
    rgpInterfaces: ^PCryptInterfaceReg;
  end;
  {$EXTERNALSYM CRYPT_IMAGE_REG}
  CRYPT_IMAGE_REG = _CRYPT_IMAGE_REG;
  TCryptImageReg = _CRYPT_IMAGE_REG;

  PCryptProviderReg = ^TCryptProviderReg;
  {$EXTERNALSYM _CRYPT_PROVIDER_REG}
  _CRYPT_PROVIDER_REG = record
    cAliases: ULONG;
    rgpszAliases: ^PWideChar;

    pUM: PCryptImageReg;
    pKM: PCryptImageReg;
  end;
  {$EXTERNALSYM CRYPT_PROVIDER_REG}
  CRYPT_PROVIDER_REG = _CRYPT_PROVIDER_REG;
  TCryptProviderReg = _CRYPT_PROVIDER_REG;

  PCryptProviders = ^TCryptProviders;
  {$EXTERNALSYM _CRYPT_PROVIDERS}
  _CRYPT_PROVIDERS = record
    cProviders: ULONG;
    rgpszProviders: ^PWideChar;
  end;
  {$EXTERNALSYM CRYPT_PROVIDERS}
  CRYPT_PROVIDERS = _CRYPT_PROVIDERS;
  TCryptProviders = _CRYPT_PROVIDERS;

//
// Context Configuration Structures
//

  PCryptContextConfig = ^TCryptContextConfig;
  {$EXTERNALSYM _CRYPT_CONTEXT_CONFIG}
  _CRYPT_CONTEXT_CONFIG = record
    dwFlags: ULONG;
    dwReserved: ULONG;
  end;
  {$EXTERNALSYM CRYPT_CONTEXT_CONFIG}
  CRYPT_CONTEXT_CONFIG = _CRYPT_CONTEXT_CONFIG;
  TCryptContextConfig = _CRYPT_CONTEXT_CONFIG;

  PCryptContextFunctionConfig = ^TCryptContextFunctionConfig;
  {$EXTERNALSYM _CRYPT_CONTEXT_FUNCTION_CONFIG}
  _CRYPT_CONTEXT_FUNCTION_CONFIG = record
    dwFlags: ULONG;
    dwReserved: ULONG;
  end;
  {$EXTERNALSYM CRYPT_CONTEXT_FUNCTION_CONFIG}
  CRYPT_CONTEXT_FUNCTION_CONFIG = _CRYPT_CONTEXT_FUNCTION_CONFIG;
  TCryptContextFunctionConfig = _CRYPT_CONTEXT_FUNCTION_CONFIG;

  PCryptContexts = ^TCryptContexts;
  {$EXTERNALSYM _CRYPT_CONTEXTS}
  _CRYPT_CONTEXTS = record
    cContexts: ULONG;
    rgpszContexts: ^PWideChar;
  end;
  {$EXTERNALSYM CRYPT_CONTEXTS}
  CRYPT_CONTEXTS = _CRYPT_CONTEXTS;
  TCryptContexts = _CRYPT_CONTEXTS;

  PCryptContextFunctions = ^TCryptContextFunctions;
  {$EXTERNALSYM _CRYPT_CONTEXT_FUNCTIONS}
  _CRYPT_CONTEXT_FUNCTIONS = record
    cFunctions: ULONG;
    rgpszFunctions: ^PWideChar;
  end;
  {$EXTERNALSYM CRYPT_CONTEXT_FUNCTIONS}
  CRYPT_CONTEXT_FUNCTIONS = _CRYPT_CONTEXT_FUNCTIONS;
  TCryptContextFunctions = _CRYPT_CONTEXT_FUNCTIONS;

  PCryptContextFunctionProviders = ^TCryptContextFunctionProviders;
  {$EXTERNALSYM _CRYPT_CONTEXT_FUNCTION_PROVIDERS}
  _CRYPT_CONTEXT_FUNCTION_PROVIDERS = record
    cProviders: ULONG;
    rgpszProviders: ^PWideChar;
  end;
  {$EXTERNALSYM CRYPT_CONTEXT_FUNCTION_PROVIDERS}
  CRYPT_CONTEXT_FUNCTION_PROVIDERS = _CRYPT_CONTEXT_FUNCTION_PROVIDERS;
  TCryptContextFunctionProviders = _CRYPT_CONTEXT_FUNCTION_PROVIDERS;

//
// Provider Resolution Structures
//

  PCryptPropertyRef = ^TCryptPropertyRef;
  {$EXTERNALSYM _CRYPT_PROPERTY_REF}
  _CRYPT_PROPERTY_REF = record
    pszProperty: PWideChar;

    cbValue: ULONG;
    pbValue: PUCHAR;
  end;
  {$EXTERNALSYM CRYPT_PROPERTY_REF}
  CRYPT_PROPERTY_REF = _CRYPT_PROPERTY_REF;
  TCryptPropertyRef = _CRYPT_PROPERTY_REF;

  PCryptImageRef = ^TCryptImageRef;
  {$EXTERNALSYM _CRYPT_IMAGE_REF}
  _CRYPT_IMAGE_REF = record
    pszImage: PWideChar;
    dwFlags: ULONG;
  end;
  {$EXTERNALSYM CRYPT_IMAGE_REF}
  CRYPT_IMAGE_REF = _CRYPT_IMAGE_REF;
  TCryptImageRef = _CRYPT_IMAGE_REF;

  PCryptProviderRef = ^TCryptProviderRef;
  {$EXTERNALSYM _CRYPT_PROVIDER_REF}
  _CRYPT_PROVIDER_REF = record
    dwInterface: ULONG;
    pszFunction: PWideChar;
    pszProvider: PWideChar;

    cProperties: ULONG;
    rgpProperties: ^PCryptPropertyRef;

    pUM: PCryptImageRef;
    pKM: PCryptImageRef;
  end;
  {$EXTERNALSYM CRYPT_PROVIDER_REF}
  CRYPT_PROVIDER_REF = _CRYPT_PROVIDER_REF;
  TCryptProviderRef = _CRYPT_PROVIDER_REF;

  PPCryptProviderRefs = ^PCryptProviderRefs;
  PCryptProviderRefs = ^TCryptProviderRefs;
  {$EXTERNALSYM _CRYPT_PROVIDER_REFS}
  _CRYPT_PROVIDER_REFS = record
    cProviders: ULONG;
    rgpProviders: ^PCryptProviderRef;
  end;
  {$EXTERNALSYM CRYPT_PROVIDER_REFS}
  CRYPT_PROVIDER_REFS = _CRYPT_PROVIDER_REFS;
  TCryptProviderRefs = _CRYPT_PROVIDER_REFS;

//////////////////////////////////////////////////////////////////////////////
// CryptoConfig Functions ////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////

var
  {$EXTERNALSYM BCryptQueryProviderRegistration}
  BCryptQueryProviderRegistration: function(pszProvider: LPCWSTR;
    dwMode, dwInterface: ULONG; pcbBuffer: PULONG;
    out ppBuffer: PCryptProviderReg): TNTStatus stdcall;

  {$EXTERNALSYM BCryptEnumRegisteredProviders}
  BCryptEnumRegisteredProviders: function(var pcbBuffer: ULONG;
    out ppBuffer: PCryptProviders): TNTStatus stdcall;

//
// Context Configuration Functions
//

  {$EXTERNALSYM BCryptCreateContext}
  BCryptCreateContext: function(dwTable: ULONG; pszContext: LPCWSTR;
    pConfig: PCryptContextConfig): TNTStatus stdcall; // Optional

  {$EXTERNALSYM BCryptDeleteContext}
  BCryptDeleteContext: function(dwTable: ULONG;
    pszContext: LPCWSTR): TNTStatus stdcall;

  {$EXTERNALSYM BCryptEnumContexts}
  BCryptEnumContexts: function(dwTable: ULONG; var pcbBuffer: ULONG;
    out ppBuffer: PCryptContexts): TNTStatus stdcall;

  {$EXTERNALSYM BCryptConfigureContext}
  BCryptConfigureContext: function(dwTable: ULONG; pszContext: LPCWSTR;
    pConfig: PCryptContextConfig): TNTStatus stdcall;

  {$EXTERNALSYM BCryptQueryContextConfiguration}
  BCryptQueryContextConfiguration: function(dwTable: ULONG; pszContext: LPCWSTR;
    var pcbBuffer: ULONG; out ppBuffer: PCryptContextConfig): TNTStatus stdcall;

  {$EXTERNALSYM BCryptAddContextFunction}
  BCryptAddContextFunction: function(dwTable: ULONG; pszContext: LPCWSTR;
    dwInterface: ULONG; pszFunction: LPCWSTR;
    dwPosition: ULONG): TNTStatus stdcall;

  {$EXTERNALSYM BCryptRemoveContextFunction}
  BCryptRemoveContextFunction: function(dwTable: ULONG; pszContext: LPCWSTR;
    dwInterface: ULONG; pszFunction: LPCWSTR): TNTStatus stdcall;

  {$EXTERNALSYM BCryptEnumContextFunctions}
  BCryptEnumContextFunctions: function(dwTable: ULONG; pszContext: LPCWSTR;
    dwInterface: ULONG; var pcbBuffer: ULONG;
    out ppBuffer: PCryptContextFunctions): TNTStatus stdcall;

  {$EXTERNALSYM BCryptConfigureContextFunction}
  BCryptConfigureContextFunction: function(dwTable: ULONG; pszContext: LPCWSTR;
    dwInterface: ULONG; pszFunction: LPCWSTR;
    pConfig: PCryptContextFunctionConfig): TNTStatus stdcall;

  {$EXTERNALSYM BCryptQueryContextFunctionConfiguration}
  BCryptQueryContextFunctionConfiguration: function(dwTable: ULONG;
    pszContext: LPCWSTR; dwInterface: ULONG; pszFunction: LPCWSTR;
    var pcbBuffer: ULONG;
    out ppBuffer: PCryptContextFunctionConfig): TNTStatus stdcall;

  {$EXTERNALSYM BCryptEnumContextFunctionProviders}
  BCryptEnumContextFunctionProviders: function(dwTable: ULONG;
    pszContext: LPCWSTR; dwInterface: ULONG; pszFunction: LPCWSTR;
    var pcbBuffer: ULONG;
    out ppBuffer: PCryptContextFunctionProviders): TNTStatus stdcall;

  {$EXTERNALSYM BCryptSetContextFunctionProperty}
  BCryptSetContextFunctionProperty: function(dwTable: ULONG;
    pszContext: LPCWSTR; dwInterface: ULONG; pszFunction, pszProperty: LPCWSTR;
    cbValue: ULONG; pbValue: PUCHAR): TNTStatus stdcall;

  {$EXTERNALSYM BCryptQueryContextFunctionProperty}
  BCryptQueryContextFunctionProperty: function(dwTable: ULONG;
    pszContext: LPCWSTR; dwInterface: ULONG; pszFunction, pszProperty: LPCWSTR;
    var pcbValue: ULONG; var ppbValue: PUCHAR): TNTStatus stdcall;

//
// Configuration Change Notification Functions
//

{$EXTERNALSYM BCryptRegisterConfigChangeNotify}
function BCryptRegisterConfigChangeNotify(
  out phEvent: THANDLE): TNTStatus; stdcall;

{$EXTERNALSYM BCryptUnregisterConfigChangeNotify}
function BCryptUnregisterConfigChangeNotify(
  hEvent: THANDLE): TNTStatus; stdcall;

//
// Provider Resolution Functions
//

{$EXTERNALSYM BCryptResolveProviders}
function BCryptResolveProviders(pszContext: LPCWSTR; dwInterface: ULONG;
  pszFunction, pszProvider: LPCWSTR; dwMode, dwFlags: ULONG;
  var pcbBuffer: ULONG; ppBuffer: PPCryptProviderRefs): TNTStatus; stdcall;

//
// Miscellaneous queries about the crypto environment
//

{$EXTERNALSYM BCryptGetFipsAlgorithmMode}
function BCryptGetFipsAlgorithmMode(
  out pfEnabled: ByteBool): TNTStatus; stdcall;

implementation

const
  bcryptdll = 'bcrypt.dll';

function BCryptOpenAlgorithmProvider; external bcryptdll name 'BCryptOpenAlgorithmProvider';
function BCryptEnumAlgorithms; external bcryptdll name 'BCryptEnumAlgorithms';
function BCryptEnumProviders; external bcryptdll name 'BCryptEnumProviders';
function BCryptGetProperty; external bcryptdll name 'BCryptGetProperty';
function BCryptSetProperty; external bcryptdll name 'BCryptSetProperty';
function BCryptCloseAlgorithmProvider; external bcryptdll name 'BCryptCloseAlgorithmProvider';
procedure BCryptFreeBuffer; external bcryptdll name 'BCryptFreeBuffer'; 
function BCryptGenerateSymmetricKey; external bcryptdll name 'BCryptGenerateSymmetricKey';
function BCryptGenerateKeyPair; external bcryptdll name 'BCryptGenerateKeyPair';
function BCryptEncrypt; external bcryptdll name 'BCryptEncrypt';
function BCryptDecrypt; external bcryptdll name 'BCryptDecrypt';
function BCryptExportKey; external bcryptdll name 'BCryptExportKey';
function BCryptImportKey; external bcryptdll name 'BCryptImportKey';
function BCryptImportKeyPair; external bcryptdll name 'BCryptImportKeyPair';
function BCryptDuplicateKey; external bcryptdll name 'BCryptDuplicateKey';
function BCryptFinalizeKeyPair; external bcryptdll name 'BCryptFinalizeKeyPair';
function BCryptDestroyKey; external bcryptdll name 'BCryptDestroyKey';
function BCryptDestroySecret; external bcryptdll name 'BCryptDestroySecret';
function BCryptSignHash; external bcryptdll name 'BCryptSignHash';
function BCryptVerifySignature; external bcryptdll name 'BCryptVerifySignature';
function BCryptSecretAgreement; external bcryptdll name 'BCryptSecretAgreement';
function BCryptDeriveKey; external bcryptdll name 'BCryptDeriveKey';
function BCryptCreateHash; external bcryptdll name 'BCryptCreateHash';
function BCryptHashData; external bcryptdll name 'BCryptHashData';
function BCryptFinishHash; external bcryptdll name 'BCryptFinishHash';
function BCryptDuplicateHash; external bcryptdll name 'BCryptDuplicateHash';
function BCryptDestroyHash; external bcryptdll name 'BCryptDestroyHash';
function BCryptGenRandom; external bcryptdll name 'BCryptGenRandom';
function BCryptRegisterConfigChangeNotify; external bcryptdll name 'BCryptRegisterConfigChangeNotify';
function BCryptUnregisterConfigChangeNotify; external bcryptdll name 'BCryptUnregisterConfigChangeNotify';
function BCryptResolveProviders; external bcryptdll name 'BCryptResolveProviders';
function BCryptGetFipsAlgorithmMode; external bcryptdll name 'BCryptGetFipsAlgorithmMode';

// Macro translations

function BCRYPT_MAKE_INTERFACE_VERSION(
  major, minor: Word): TBCryptInterfaceVersion;
begin
  Result.MajorVersion := major;
  Result.MinorVersion := minor;
end;

function BCRYPT_IS_INTERFACE_VERSION_COMPATIBLE(
  const loader, provider: TBCryptInterfaceVersion): Boolean;
begin
  Result := loader.MajorVersion <= provider.MajorVersion;
end;

// Dynamically loaded functions

function _BCryptQueryProviderRegistration(pszProvider: LPCWSTR;
  dwMode, dwInterface: ULONG; var pcbBuffer: ULONG;
  out ppBuffer: PCryptProviderReg): TNTStatus; stdcall;
begin
  Result := STATUS_NOT_IMPLEMENTED;
end;

function _BCryptEnumRegisteredProviders(var pcbBuffer: ULONG;
  out ppBuffer: PCryptProviders): TNTStatus; stdcall;
begin
  Result := STATUS_NOT_IMPLEMENTED;
end;

function _BCryptCreateContext(dwTable: ULONG; pszContext: LPCWSTR;
  pConfig: PCryptContextConfig): TNTStatus stdcall;
begin
  Result := STATUS_NOT_IMPLEMENTED;
end;

function _BCryptDeleteContext(dwTable: ULONG;
  pszContext: LPCWSTR): TNTStatus; stdcall;
begin
  Result := STATUS_NOT_IMPLEMENTED;
end;

function _BCryptEnumContexts(dwTable: ULONG; var pcbBuffer: ULONG;
  out ppBuffer: PCryptContexts): TNTStatus; stdcall;
begin
  Result := STATUS_NOT_IMPLEMENTED;
end;

function _BCryptConfigureContext(dwTable: ULONG; pszContext: LPCWSTR;
  pConfig: PCryptContextConfig): TNTStatus; stdcall;
begin
  Result := STATUS_NOT_IMPLEMENTED;
end;

function _BCryptQueryContextConfiguration(dwTable: ULONG; pszContext: LPCWSTR;
  var pcbBuffer: ULONG; out ppBuffer: PCryptContextConfig): TNTStatus; stdcall;
begin
  Result := STATUS_NOT_IMPLEMENTED;
end;

function _BCryptAddContextFunction(dwTable: ULONG; pszContext: LPCWSTR;
  dwInterface: ULONG; pszFunction: LPCWSTR;
  dwPosition: ULONG): TNTStatus; stdcall;
begin
  Result := STATUS_NOT_IMPLEMENTED;
end;

function _BCryptRemoveContextFunction(dwTable: ULONG; pszContext: LPCWSTR;
  dwInterface: ULONG; pszFunction: LPCWSTR): TNTStatus; stdcall;
begin
  Result := STATUS_NOT_IMPLEMENTED;
end;

function _BCryptEnumContextFunctions(dwTable: ULONG; pszContext: LPCWSTR;
  dwInterface: ULONG; var pcbBuffer: ULONG;
  out ppBuffer: PCryptContextFunctions): TNTStatus; stdcall;
begin
  Result := STATUS_NOT_IMPLEMENTED;
end;

function _BCryptConfigureContextFunction(dwTable: ULONG; pszContext: LPCWSTR;
  dwInterface: ULONG; pszFunction: LPCWSTR;
  pConfig: PCryptContextFunctionConfig): TNTStatus; stdcall;
begin
  Result := STATUS_NOT_IMPLEMENTED;
end;

function _BCryptQueryContextFunctionConfiguration(dwTable: ULONG;
  pszContext: LPCWSTR; dwInterface: ULONG; pszFunction: LPCWSTR;
  var pcbBuffer: ULONG;
  out ppBuffer: PCryptContextFunctionConfig): TNTStatus; stdcall;
begin
  Result := STATUS_NOT_IMPLEMENTED;
end;

function _BCryptEnumContextFunctionProviders(dwTable: ULONG;
  pszContext: LPCWSTR; dwInterface: ULONG; pszFunction: LPCWSTR;
  var pcbBuffer: ULONG;
  out ppBuffer: PCryptContextFunctionProviders): TNTStatus; stdcall;
begin
  Result := STATUS_NOT_IMPLEMENTED;
end;

function _BCryptSetContextFunctionProperty(dwTable: ULONG;
  pszContext: LPCWSTR; dwInterface: ULONG; pszFunction, pszProperty: LPCWSTR;
  cbValue: ULONG; pbValue: PUCHAR): TNTStatus; stdcall;
begin
  Result := STATUS_NOT_IMPLEMENTED;
end;

function _BCryptQueryContextFunctionProperty(dwTable: ULONG;
  pszContext: LPCWSTR; dwInterface: ULONG; pszFunction, pszProperty: LPCWSTR;
  var pcbValue: ULONG; var ppbValue: PUCHAR): TNTStatus stdcall;
begin
  Result := STATUS_NOT_IMPLEMENTED;
end;

// Helper function

function SafeGetProcAddress(H: HMODULE; ProcName: PChar;
  Default: Pointer): TFarProc;
begin
  if H = 0 then
    Result := Default
  else
  begin
    Result := GetProcAddress(H, ProcName);
    if not Assigned(Result) then
    begin
      Result := Default;
{$IFDEF SFGPA_DEBUG}
      Writeln(bcryptdll, ': ', ProcName, ' replaced');
{$ENDIF}
    end;
  end;
end;

var
  Module: HMODULE;

initialization
{$IFDEF SFGPA_DEBUG}
  Writeln;
  Writeln('BCrypt');
  Writeln;
{$ENDIF}
  Module := LoadLibrary(bcryptdll);
  BCryptQueryProviderRegistration := SafeGetProcAddress(Module, 'BCryptQueryProviderRegistration', @_BCryptQueryProviderRegistration);
  BCryptEnumRegisteredProviders := SafeGetProcAddress(Module, 'BCryptEnumRegisteredProviders', @_BCryptEnumRegisteredProviders);
  BCryptCreateContext := SafeGetProcAddress(Module, 'BCryptCreateContext', @_BCryptCreateContext);
  BCryptDeleteContext := SafeGetProcAddress(Module, 'BCryptDeleteContext', @_BCryptDeleteContext);
  BCryptEnumContexts := SafeGetProcAddress(Module, 'BCryptEnumContexts', @_BCryptEnumContexts);
  BCryptConfigureContext := SafeGetProcAddress(Module, 'BCryptConfigureContext', @_BCryptConfigureContext);
  BCryptQueryContextConfiguration := SafeGetprocAddress(Module, 'BCryptQueryContextConfiguration', @_BCryptQueryContextConfiguration);
  BCryptAddContextFunction := SafeGetProcAddress(Module, 'BCryptAddContextFunction', @_BCryptAddContextFunction);
  BCryptRemoveContextFunction := SafeGetProcAddress(Module, 'BCryptRemoveContextFunction', @_BCryptRemoveContextFunction);
  BCryptEnumContextFunctions := SafeGetProcAddress(Module, 'BCryptEnumContextFunctions', @_BCryptEnumContextFunctions);
  BCryptConfigureContextFunction := SafeGetProcAddress(Module, 'BCryptConfigureContextFunction', @_BCryptConfigureContextFunction);
  BCryptQueryContextFunctionConfiguration := SafeGetProcAddress(Module, 'BCryptQueryContextFunctionConfiguration', @_BCryptQueryContextFunctionConfiguration);
  BCryptEnumContextFunctionProviders := SafeGetProcAddress(Module, 'BCryptEnumContextFunctionProviders', @_BCryptEnumContextFunctionProviders);
  BCryptSetContextFunctionProperty := SafeGetProcAddress(Module, 'BCryptSetContextFunctionProperty', @_BCryptSetContextFunctionProperty);
  BCryptQueryContextFunctionProperty := SafeGetProcAddress(Module, 'BCryptQueryContextFunctionProperty', @_BCryptQueryContextFunctionProperty);

finalization
  if Module <> 0 then
    FreeLibrary(Module);

end.

