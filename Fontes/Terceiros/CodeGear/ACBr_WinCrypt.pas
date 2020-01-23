{*****************************************************************}
{                                                                 }
{            CodeGear Delphi Runtime Library                      }
{            WinCrypt.pas interface unit                          }
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
{ The original code is: wincrypt.h, released 1992-1999.           }
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
{ Contributor(s):                                                 }
{                                                                 }
{ Notes: Unicode is off, in this version.                         }
{                                                                 }
{ Modification history:                                           }
{                                                                 }
{ Known Issues:                                                   }
{                                                                 }
{   Some structs contain anonymous unions in the middle of the    }
{   struct. To keep them anonymous, I apply the trick as layed    }
{   out in my conversion article:                                 }
{                                                                 }
{     http://rvelthuis.de/articles/articles-convert.html#unions   }
{                                                                 }
{   This means I start the variant part where the union appears,  }
{   and add the rest of the fields, that are supposed to be after }
{   the union to the largest field of the union. The diagram in   }
{   my article explains why this is safe.                         }
{                                                                 }
{   In structs with several such unions, I gave the unions names  }
{   like u1, u2, like the API headers also do, if the C compiler  }
{   does not allow anonymous unions.                              }
{                                                                 }
{*****************************************************************}

{$I ACBr.inc}

{ $DEFINE UNICODE} // Unicode off

unit ACBr_WinCrypt;

// {$WEAKPACKAGEUNIT}
// http://docs.embarcadero.com/products/rad_studio/delphiAndcpp2009/HelpUpdate2/EN/html/devcommon/cm_weakpackage_body_xml.html

interface

uses
  Types, Windows, Messages, ACBr_BCrypt, ACBr_NCrypt;

const
  Crypt32 = 'crypt32.dll';
  CryptNet = 'cryptnet.dll';
  Advapi32 = 'advapi32.dll';
  cryptuiapi = 'cryptui.dll';

{$HPPEMIT '#include <wincrypt.h>'}

//
// Algorithm IDs and Flags
//

// ALG_ID crackers
{$EXTERNALSYM GET_ALG_CLASS}
function GET_ALG_CLASS(x: DWORD): DWORD; {$IfDef HAS_INLINE}inline;{$EndIf}
{$EXTERNALSYM GET_ALG_TYPE}
function GET_ALG_TYPE(x: DWORD): DWORD; {$IfDef HAS_INLINE}inline;{$EndIf}
{$EXTERNALSYM GET_ALG_SID}
function GET_ALG_SID(x: DWORD): DWORD; {$IfDef HAS_INLINE}inline;{$EndIf}

const
// Algorithm classes
// certenrolld_begin -- ALG_CLASS_*
  {$EXTERNALSYM ALG_CLASS_ANY}
  ALG_CLASS_ANY                   = 0;
  {$EXTERNALSYM ALG_CLASS_SIGNATURE}
  ALG_CLASS_SIGNATURE             = 1 shl 13;
  {$EXTERNALSYM ALG_CLASS_MSG_ENCRYPT}
  ALG_CLASS_MSG_ENCRYPT           = 2 shl 13;
  {$EXTERNALSYM ALG_CLASS_DATA_ENCRYPT}
  ALG_CLASS_DATA_ENCRYPT          = 3 shl 13;
  {$EXTERNALSYM ALG_CLASS_HASH}
  ALG_CLASS_HASH                  = 4 shl 13;
  {$EXTERNALSYM ALG_CLASS_KEY_EXCHANGE}
  ALG_CLASS_KEY_EXCHANGE          = 5 shl 13;
  {$EXTERNALSYM ALG_CLASS_ALL}
  ALG_CLASS_ALL                   = 7 shl 13;
// certenrolld_end

// Algorithm types
  {$EXTERNALSYM ALG_TYPE_ANY}
  ALG_TYPE_ANY                    = 0;
  {$EXTERNALSYM ALG_TYPE_DSS}
  ALG_TYPE_DSS                    = 1 shl 9;
  {$EXTERNALSYM ALG_TYPE_RSA}
  ALG_TYPE_RSA                    = 2 shl 9;
  {$EXTERNALSYM ALG_TYPE_BLOCK}
  ALG_TYPE_BLOCK                  = 3 shl 9;
  {$EXTERNALSYM ALG_TYPE_STREAM}
  ALG_TYPE_STREAM                 = 4 shl 9;
  {$EXTERNALSYM ALG_TYPE_DH}
  ALG_TYPE_DH                     = 5 shl 9;
  {$EXTERNALSYM ALG_TYPE_SECURECHANNEL}
  ALG_TYPE_SECURECHANNEL          = 6 shl 9;

// Generic sub-ids
  {$EXTERNALSYM ALG_SID_ANY}
  ALG_SID_ANY                     = 0;

// Some RSA sub-ids
  {$EXTERNALSYM ALG_SID_RSA_ANY}
  ALG_SID_RSA_ANY                 = 0;
  {$EXTERNALSYM ALG_SID_RSA_PKCS}
  ALG_SID_RSA_PKCS                = 1;
  {$EXTERNALSYM ALG_SID_RSA_MSATWORK}
  ALG_SID_RSA_MSATWORK            = 2;
  {$EXTERNALSYM ALG_SID_RSA_ENTRUST}
  ALG_SID_RSA_ENTRUST             = 3;
  {$EXTERNALSYM ALG_SID_RSA_PGP}
  ALG_SID_RSA_PGP                 = 4;

// Some DSS sub-ids
//
  {$EXTERNALSYM ALG_SID_DSS_ANY}
  ALG_SID_DSS_ANY                 = 0;
  {$EXTERNALSYM ALG_SID_DSS_PKCS}
  ALG_SID_DSS_PKCS                = 1;
  {$EXTERNALSYM ALG_SID_DSS_DMS}
  ALG_SID_DSS_DMS                 = 2;
  {$EXTERNALSYM ALG_SID_ECDSA}
  ALG_SID_ECDSA                   = 3;

// Block cipher sub ids
// DES sub_ids
  {$EXTERNALSYM ALG_SID_DES}
  ALG_SID_DES                     = 1;
  {$EXTERNALSYM ALG_SID_3DES}
  ALG_SID_3DES                    = 3;
  {$EXTERNALSYM ALG_SID_DESX}
  ALG_SID_DESX                    = 4;
  {$EXTERNALSYM ALG_SID_IDEA}
  ALG_SID_IDEA                    = 5;
  {$EXTERNALSYM ALG_SID_CAST}
  ALG_SID_CAST                    = 6;
  {$EXTERNALSYM ALG_SID_SAFERSK64}
  ALG_SID_SAFERSK64               = 7;
  {$EXTERNALSYM ALG_SID_SAFERSK128}
  ALG_SID_SAFERSK128              = 8;
  {$EXTERNALSYM ALG_SID_3DES_112}
  ALG_SID_3DES_112                = 9;
  {$EXTERNALSYM ALG_SID_CYLINK_MEK}
  ALG_SID_CYLINK_MEK              = 12;
  {$EXTERNALSYM ALG_SID_RC5}
  ALG_SID_RC5                     = 13;
  {$EXTERNALSYM ALG_SID_AES_128}
  ALG_SID_AES_128                 = 14;
  {$EXTERNALSYM ALG_SID_AES_192}
  ALG_SID_AES_192                 = 15;
  {$EXTERNALSYM ALG_SID_AES_256}
  ALG_SID_AES_256                 = 16;
  {$EXTERNALSYM ALG_SID_AES}
  ALG_SID_AES                     = 17;

// Fortezza sub-ids
  {$EXTERNALSYM ALG_SID_SKIPJACK}
  ALG_SID_SKIPJACK                = 10;
  {$EXTERNALSYM ALG_SID_TEK}
  ALG_SID_TEK                     = 11;

// KP_MODE
  {$EXTERNALSYM CRYPT_MODE_CBCI}
  CRYPT_MODE_CBCI                 = 6;       // ANSI CBC Interleaved
  {$EXTERNALSYM CRYPT_MODE_CFBP}
  CRYPT_MODE_CFBP                 = 7;       // ANSI CFB Pipelined
  {$EXTERNALSYM CRYPT_MODE_OFBP}
  CRYPT_MODE_OFBP                 = 8;       // ANSI OFB Pipelined
  {$EXTERNALSYM CRYPT_MODE_CBCOFM}
  CRYPT_MODE_CBCOFM               = 9;       // ANSI CBC + OF Masking
  {$EXTERNALSYM CRYPT_MODE_CBCOFMI}
  CRYPT_MODE_CBCOFMI              = 10;      // ANSI CBC + OFM Interleaved

// RC2 sub-ids
  {$EXTERNALSYM ALG_SID_RC2}
  ALG_SID_RC2                     = 2;

// Stream cipher sub-ids
  {$EXTERNALSYM ALG_SID_RC4}
  ALG_SID_RC4                     = 1;
  {$EXTERNALSYM ALG_SID_SEAL}
  ALG_SID_SEAL                    = 2;

// Diffie-Hellman sub-ids
  {$EXTERNALSYM ALG_SID_DH_SANDF}
  ALG_SID_DH_SANDF                = 1;
  {$EXTERNALSYM ALG_SID_DH_EPHEM}
  ALG_SID_DH_EPHEM                = 2;
  {$EXTERNALSYM ALG_SID_AGREED_KEY_ANY}
  ALG_SID_AGREED_KEY_ANY          = 3;
  {$EXTERNALSYM ALG_SID_KEA}
  ALG_SID_KEA                     = 4;
  {$EXTERNALSYM ALG_SID_ECDH}
  ALG_SID_ECDH                    = 5;

// Hash sub ids
  {$EXTERNALSYM ALG_SID_MD2}
  ALG_SID_MD2                     = 1;
  {$EXTERNALSYM ALG_SID_MD4}
  ALG_SID_MD4                     = 2;
  {$EXTERNALSYM ALG_SID_MD5}
  ALG_SID_MD5                     = 3;
  {$EXTERNALSYM ALG_SID_SHA}
  ALG_SID_SHA                     = 4;
  {$EXTERNALSYM ALG_SID_SHA1}
  ALG_SID_SHA1                    = 4;
  {$EXTERNALSYM ALG_SID_MAC}
  ALG_SID_MAC                     = 5;
  {$EXTERNALSYM ALG_SID_RIPEMD}
  ALG_SID_RIPEMD                  = 6;
  {$EXTERNALSYM ALG_SID_RIPEMD160}
  ALG_SID_RIPEMD160               = 7;
  {$EXTERNALSYM ALG_SID_SSL3SHAMD5}
  ALG_SID_SSL3SHAMD5              = 8;
  {$EXTERNALSYM ALG_SID_HMAC}
  ALG_SID_HMAC                    = 9;
  {$EXTERNALSYM ALG_SID_TLS1PRF}
  ALG_SID_TLS1PRF                 = 10;
  {$EXTERNALSYM ALG_SID_HASH_REPLACE_OWF}
  ALG_SID_HASH_REPLACE_OWF        = 11;
  {$EXTERNALSYM ALG_SID_SHA_256}
  ALG_SID_SHA_256                 = 12;
  {$EXTERNALSYM ALG_SID_SHA_384}
  ALG_SID_SHA_384                 = 13;
  {$EXTERNALSYM ALG_SID_SHA_512}
  ALG_SID_SHA_512                 = 14;

// secure channel sub ids
  {$EXTERNALSYM ALG_SID_SSL3_MASTER}
  ALG_SID_SSL3_MASTER             = 1;
  {$EXTERNALSYM ALG_SID_SCHANNEL_MASTER_HASH}
  ALG_SID_SCHANNEL_MASTER_HASH    = 2;
  {$EXTERNALSYM ALG_SID_SCHANNEL_MAC_KEY}
  ALG_SID_SCHANNEL_MAC_KEY        = 3;
  {$EXTERNALSYM ALG_SID_PCT1_MASTER}
  ALG_SID_PCT1_MASTER             = 4;
  {$EXTERNALSYM ALG_SID_SSL2_MASTER}
  ALG_SID_SSL2_MASTER             = 5;
  {$EXTERNALSYM ALG_SID_TLS1_MASTER}
  ALG_SID_TLS1_MASTER             = 6;
  {$EXTERNALSYM ALG_SID_SCHANNEL_ENC_KEY}
  ALG_SID_SCHANNEL_ENC_KEY        = 7;

// misc ECC sub ids
  {$EXTERNALSYM ALG_SID_ECMQV}
  ALG_SID_ECMQV                   = 1;

// Our silly example sub-id
  {$EXTERNALSYM ALG_SID_EXAMPLE}
  ALG_SID_EXAMPLE                 = 80;

// certenrolls_begin -- PROV_ENUMALGS_EX
type
  {$EXTERNALSYM ALG_ID}
  ALG_ID = Cardinal;
// certenrolls_end

const
// algorithm identifier definitions
  {$EXTERNALSYM CALG_MD2}
  CALG_MD2                = ALG_CLASS_HASH or ALG_TYPE_ANY or ALG_SID_MD2;
  {$EXTERNALSYM CALG_MD4}
  CALG_MD4                = ALG_CLASS_HASH or ALG_TYPE_ANY or ALG_SID_MD4;
  {$EXTERNALSYM CALG_MD5}
  CALG_MD5                = ALG_CLASS_HASH or ALG_TYPE_ANY or ALG_SID_MD5;
  {$EXTERNALSYM CALG_SHA}
  CALG_SHA                = ALG_CLASS_HASH or ALG_TYPE_ANY or ALG_SID_SHA;
  {$EXTERNALSYM CALG_SHA1}
  CALG_SHA1               = ALG_CLASS_HASH or ALG_TYPE_ANY or ALG_SID_SHA1;
  {$EXTERNALSYM CALG_MAC}
  CALG_MAC                = ALG_CLASS_HASH or ALG_TYPE_ANY or ALG_SID_MAC;
  {$EXTERNALSYM CALG_RSA_SIGN}
  CALG_RSA_SIGN           = ALG_CLASS_SIGNATURE or ALG_TYPE_RSA or ALG_SID_RSA_ANY;
  {$EXTERNALSYM CALG_DSS_SIGN}
  CALG_DSS_SIGN           = ALG_CLASS_SIGNATURE or ALG_TYPE_DSS or ALG_SID_DSS_ANY;
  {$EXTERNALSYM CALG_NO_SIGN}
  CALG_NO_SIGN            = ALG_CLASS_SIGNATURE or ALG_TYPE_ANY or ALG_SID_ANY;
  {$EXTERNALSYM CALG_RSA_KEYX}
  CALG_RSA_KEYX           = ALG_CLASS_KEY_EXCHANGE or ALG_TYPE_RSA or ALG_SID_RSA_ANY;
  {$EXTERNALSYM CALG_DES}
  CALG_DES                = ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_BLOCK or ALG_SID_DES;
  {$EXTERNALSYM CALG_3DES_112}
  CALG_3DES_112           = ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_BLOCK or ALG_SID_3DES_112;
  {$EXTERNALSYM CALG_3DES}
  CALG_3DES               = ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_BLOCK or ALG_SID_3DES;
  {$EXTERNALSYM CALG_DESX}
  CALG_DESX               = ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_BLOCK or ALG_SID_DESX;
  {$EXTERNALSYM CALG_RC2}
  CALG_RC2                = ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_BLOCK or ALG_SID_RC2;
  {$EXTERNALSYM CALG_RC4}
  CALG_RC4                = ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_STREAM or ALG_SID_RC4;
  {$EXTERNALSYM CALG_SEAL}
  CALG_SEAL               = ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_STREAM or ALG_SID_SEAL;
  {$EXTERNALSYM CALG_DH_SF}
  CALG_DH_SF              = ALG_CLASS_KEY_EXCHANGE or ALG_TYPE_DH or ALG_SID_DH_SANDF;
  {$EXTERNALSYM CALG_DH_EPHEM}
  CALG_DH_EPHEM           = ALG_CLASS_KEY_EXCHANGE or ALG_TYPE_DH or ALG_SID_DH_EPHEM;
  {$EXTERNALSYM CALG_AGREEDKEY_ANY}
  CALG_AGREEDKEY_ANY      = ALG_CLASS_KEY_EXCHANGE or ALG_TYPE_DH or ALG_SID_AGREED_KEY_ANY;
  {$EXTERNALSYM CALG_KEA_KEYX}
  CALG_KEA_KEYX           = ALG_CLASS_KEY_EXCHANGE or ALG_TYPE_DH or ALG_SID_KEA;
  {$EXTERNALSYM CALG_HUGHES_MD5}
  CALG_HUGHES_MD5         = ALG_CLASS_KEY_EXCHANGE or ALG_TYPE_ANY or ALG_SID_MD5;
  {$EXTERNALSYM CALG_SKIPJACK}
  CALG_SKIPJACK           = ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_BLOCK or ALG_SID_SKIPJACK;
  {$EXTERNALSYM CALG_TEK}
  CALG_TEK                = ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_BLOCK or ALG_SID_TEK;
  {$EXTERNALSYM CALG_CYLINK_MEK}
  CALG_CYLINK_MEK         = ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_BLOCK or ALG_SID_CYLINK_MEK;
  {$EXTERNALSYM CALG_SSL3_SHAMD5}
  CALG_SSL3_SHAMD5        = ALG_CLASS_HASH or ALG_TYPE_ANY or ALG_SID_SSL3SHAMD5;
  {$EXTERNALSYM CALG_SSL3_MASTER}
  CALG_SSL3_MASTER        = ALG_CLASS_MSG_ENCRYPT or ALG_TYPE_SECURECHANNEL or ALG_SID_SSL3_MASTER;
  {$EXTERNALSYM CALG_SCHANNEL_MASTER_HASH}
  CALG_SCHANNEL_MASTER_HASH   = ALG_CLASS_MSG_ENCRYPT or ALG_TYPE_SECURECHANNEL or ALG_SID_SCHANNEL_MASTER_HASH;
  {$EXTERNALSYM CALG_SCHANNEL_MAC_KEY}
  CALG_SCHANNEL_MAC_KEY   = ALG_CLASS_MSG_ENCRYPT or ALG_TYPE_SECURECHANNEL or ALG_SID_SCHANNEL_MAC_KEY;
  {$EXTERNALSYM CALG_SCHANNEL_ENC_KEY}
  CALG_SCHANNEL_ENC_KEY   = ALG_CLASS_MSG_ENCRYPT or ALG_TYPE_SECURECHANNEL or ALG_SID_SCHANNEL_ENC_KEY;
  {$EXTERNALSYM CALG_PCT1_MASTER}
  CALG_PCT1_MASTER        = ALG_CLASS_MSG_ENCRYPT or ALG_TYPE_SECURECHANNEL or ALG_SID_PCT1_MASTER;
  {$EXTERNALSYM CALG_SSL2_MASTER}
  CALG_SSL2_MASTER        = ALG_CLASS_MSG_ENCRYPT or ALG_TYPE_SECURECHANNEL or ALG_SID_SSL2_MASTER;
  {$EXTERNALSYM CALG_TLS1_MASTER}
  CALG_TLS1_MASTER        = ALG_CLASS_MSG_ENCRYPT or ALG_TYPE_SECURECHANNEL or ALG_SID_TLS1_MASTER;
  {$EXTERNALSYM CALG_RC5}
  CALG_RC5                = ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_BLOCK or ALG_SID_RC5;
  {$EXTERNALSYM CALG_HMAC}
  CALG_HMAC               = ALG_CLASS_HASH or ALG_TYPE_ANY or ALG_SID_HMAC;
  {$EXTERNALSYM CALG_TLS1PRF}
  CALG_TLS1PRF            = ALG_CLASS_HASH or ALG_TYPE_ANY or ALG_SID_TLS1PRF;
  {$EXTERNALSYM CALG_HASH_REPLACE_OWF}
  CALG_HASH_REPLACE_OWF   = ALG_CLASS_HASH or ALG_TYPE_ANY or ALG_SID_HASH_REPLACE_OWF;
  {$EXTERNALSYM CALG_AES_128}
  CALG_AES_128            = ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_BLOCK or ALG_SID_AES_128;
  {$EXTERNALSYM CALG_AES_192}
  CALG_AES_192            = ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_BLOCK or ALG_SID_AES_192;
  {$EXTERNALSYM CALG_AES_256}
  CALG_AES_256            = ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_BLOCK or ALG_SID_AES_256;
  {$EXTERNALSYM CALG_AES}
  CALG_AES                = ALG_CLASS_DATA_ENCRYPT or ALG_TYPE_BLOCK or ALG_SID_AES;
  {$EXTERNALSYM CALG_SHA_256}
  CALG_SHA_256            = ALG_CLASS_HASH or ALG_TYPE_ANY or ALG_SID_SHA_256;
  {$EXTERNALSYM CALG_SHA_384}
  CALG_SHA_384            = ALG_CLASS_HASH or ALG_TYPE_ANY or ALG_SID_SHA_384;
  {$EXTERNALSYM CALG_SHA_512}
  CALG_SHA_512            = ALG_CLASS_HASH or ALG_TYPE_ANY or ALG_SID_SHA_512;
  {$EXTERNALSYM CALG_ECDH}
  CALG_ECDH               = ALG_CLASS_KEY_EXCHANGE or ALG_TYPE_DH or ALG_SID_ECDH;
  {$EXTERNALSYM CALG_ECMQV}
  CALG_ECMQV              = ALG_CLASS_KEY_EXCHANGE or ALG_TYPE_ANY or ALG_SID_ECMQV;
  {$EXTERNALSYM CALG_ECDSA}
  CALG_ECDSA              = ALG_CLASS_SIGNATURE or ALG_TYPE_DSS or ALG_SID_ECDSA;

  // resource number for signatures in the CSP
  SIGNATURE_RESOURCE_NUMBER = $29A;

type
  {$EXTERNALSYM PVTableProcStruc}
  PVTableProcStruc = ^TVTableProvStruc;
  {$EXTERNALSYM _VTableProvStruc}
  _VTableProvStruc = record
    Version: DWORD;
    FuncVerifyImage: FARPROC;
    FuncReturnhWnd: FARPROC;
    dwProvType: DWORD;
    pbContextInfo: PBYTE;
    cbContextInfo: DWORD;
    pszProvName: LPSTR;
  end;
  {$EXTERNALSYM VTableProvStruc}
  VTableProvStruc = _VTableProvStruc;
  TVTableProvStruc = _VTableProvStruc;

// Used for certenroll.idl:
// certenrolls_begin -- HCRYPT*
type
  {$EXTERNALSYM HCRYPTPROV}
  HCRYPTPROV = ULONG_PTR;
  {$EXTERNALSYM HCRYPTKEY}
  HCRYPTKEY = ULONG_PTR;
  {$EXTERNALSYM HCRYPTHASH}
  HCRYPTHASH = ULONG_PTR;
// certenrolls_end

const
  // dwFlags definitions for CryptAcquireContext
  {$EXTERNALSYM CRYPT_VERIFYCONTEXT}
  CRYPT_VERIFYCONTEXT     = $F0000000;
  {$EXTERNALSYM CRYPT_NEWKEYSET}
  CRYPT_NEWKEYSET         = $00000008;
  {$EXTERNALSYM CRYPT_DELETEKEYSET}
  CRYPT_DELETEKEYSET      = $00000010;
  {$EXTERNALSYM CRYPT_MACHINE_KEYSET}
  CRYPT_MACHINE_KEYSET    = $00000020;
  {$EXTERNALSYM CRYPT_SILENT}
  CRYPT_SILENT            = $00000040;
  {$EXTERNALSYM CRYPT_DEFAULT_CONTAINER_OPTIONAL}
  CRYPT_DEFAULT_CONTAINER_OPTIONAL = $00000080;

  // dwFlag definitions for CryptGenKey
  {$EXTERNALSYM CRYPT_EXPORTABLE}
  CRYPT_EXPORTABLE        = $00000001;
  {$EXTERNALSYM CRYPT_USER_PROTECTED}
  CRYPT_USER_PROTECTED    = $00000002;
  {$EXTERNALSYM CRYPT_CREATE_SALT}
  CRYPT_CREATE_SALT       = $00000004;
  {$EXTERNALSYM CRYPT_UPDATE_KEY}
  CRYPT_UPDATE_KEY        = $00000008;
  {$EXTERNALSYM CRYPT_NO_SALT}
  CRYPT_NO_SALT           = $00000010;
  {$EXTERNALSYM CRYPT_PREGEN}
  CRYPT_PREGEN            = $00000040;
  {$EXTERNALSYM CRYPT_RECIPIENT}
  CRYPT_RECIPIENT         = $00000010;
  {$EXTERNALSYM CRYPT_INITIATOR}
  CRYPT_INITIATOR         = $00000040;
  {$EXTERNALSYM CRYPT_ONLINE}
  CRYPT_ONLINE            = $00000080;
  {$EXTERNALSYM CRYPT_SF}
  CRYPT_SF                = $00000100;
  {$EXTERNALSYM CRYPT_CREATE_IV}
  CRYPT_CREATE_IV         = $00000200;
  {$EXTERNALSYM CRYPT_KEK}
  CRYPT_KEK               = $00000400;
  {$EXTERNALSYM CRYPT_DATA_KEY}
  CRYPT_DATA_KEY          = $00000800;
  {$EXTERNALSYM CRYPT_VOLATILE}
  CRYPT_VOLATILE          = $00001000;
  {$EXTERNALSYM CRYPT_SGCKEY}
  CRYPT_SGCKEY            = $00002000;
  {$EXTERNALSYM CRYPT_ARCHIVABLE}
  CRYPT_ARCHIVABLE        = $00004000;
  {$EXTERNALSYM CRYPT_FORCE_KEY_PROTECTION_HIGH}
  CRYPT_FORCE_KEY_PROTECTION_HIGH = $00008000;

  {$EXTERNALSYM RSA1024BIT_KEY}
  RSA1024BIT_KEY          = $04000000;

  // dwFlags definitions for CryptDeriveKey
  {$EXTERNALSYM CRYPT_SERVER}
  CRYPT_SERVER            = $00000400;

  {$EXTERNALSYM KEY_LENGTH_MASK}
  KEY_LENGTH_MASK         = $FFFF0000;

  // dwFlag definitions for CryptExportKey
  {$EXTERNALSYM CRYPT_Y_ONLY}
  CRYPT_Y_ONLY            = $00000001;
  {$EXTERNALSYM CRYPT_SSL2_FALLBACK}
  CRYPT_SSL2_FALLBACK     = $00000002;
  {$EXTERNALSYM CRYPT_DESTROYKEY}
  CRYPT_DESTROYKEY        = $00000004;
  {$EXTERNALSYM CRYPT_OAEP}
  CRYPT_OAEP              = $00000040;  // used with RSA encryptions/decryptions
                                        // CryptExportKey, CryptImportKey,
                                        // CryptEncrypt and CryptDecrypt

  {$EXTERNALSYM CRYPT_BLOB_VER3}
  CRYPT_BLOB_VER3         = $00000080;  // export version 3 of a blob type
  {$EXTERNALSYM CRYPT_IPSEC_HMAC_KEY}
  CRYPT_IPSEC_HMAC_KEY    = $00000100;  // CryptImportKey only

  // dwFlags definitions for CryptDecrypt
  //  See also CRYPT_OAEP, above.
  //  Note, the following flag is not supported for CryptEncrypt
  {$EXTERNALSYM CRYPT_DECRYPT_RSA_NO_PADDING_CHECK}
  CRYPT_DECRYPT_RSA_NO_PADDING_CHECK      = $00000020;

  // dwFlags definitions for CryptCreateHash
  {$EXTERNALSYM CRYPT_SECRETDIGEST}
  CRYPT_SECRETDIGEST      = $00000001;

  // dwFlags definitions for CryptHashData
  {$EXTERNALSYM CRYPT_OWF_REPL_LM_HASH}
  CRYPT_OWF_REPL_LM_HASH  = $00000001;  // this is only for the OWF replacement CSP

  // dwFlags definitions for CryptHashSessionKey
  {$EXTERNALSYM CRYPT_LITTLE_ENDIAN}
  CRYPT_LITTLE_ENDIAN     = $00000001;

  // dwFlags definitions for CryptSignHash and CryptVerifySignature
  {$EXTERNALSYM CRYPT_NOHASHOID}
  CRYPT_NOHASHOID         = $00000001;
  {$EXTERNALSYM CRYPT_TYPE2_FORMAT}
  CRYPT_TYPE2_FORMAT      = $00000002;
  {$EXTERNALSYM CRYPT_X931_FORMAT}
  CRYPT_X931_FORMAT       = $00000004;

  // dwFlag definitions for CryptSetProviderEx and CryptGetDefaultProvider
  {$EXTERNALSYM CRYPT_MACHINE_DEFAULT}
  CRYPT_MACHINE_DEFAULT   = $00000001;
  {$EXTERNALSYM CRYPT_USER_DEFAULT}
  CRYPT_USER_DEFAULT      = $00000002;
  {$EXTERNALSYM CRYPT_DELETE_DEFAULT}
  CRYPT_DELETE_DEFAULT    = $00000004;

  // exported key blob definitions
  // certenrolld_begin -- *BLOB
  {$EXTERNALSYM SIMPLEBLOB}
  SIMPLEBLOB              = $1;
  {$EXTERNALSYM PUBLICKEYBLOB}
  PUBLICKEYBLOB           = $6;
  {$EXTERNALSYM PRIVATEKEYBLOB}
  PRIVATEKEYBLOB          = $7;
  {$EXTERNALSYM PLAINTEXTKEYBLOB}
  PLAINTEXTKEYBLOB        = $8;
  {$EXTERNALSYM OPAQUEKEYBLOB}
  OPAQUEKEYBLOB           = $9;
  {$EXTERNALSYM PUBLICKEYBLOBEX}
  PUBLICKEYBLOBEX         = $A;
  {$EXTERNALSYM SYMMETRICWRAPKEYBLOB}
  SYMMETRICWRAPKEYBLOB    = $B;
  {$EXTERNALSYM KEYSTATEBLOB}
  KEYSTATEBLOB            = $C;
// certenrolld_end

// certenrolld_begin -- AT_*
  {$EXTERNALSYM AT_KEYEXCHANGE}
  AT_KEYEXCHANGE         =  1;
  {$EXTERNALSYM AT_SIGNATURE}
  AT_SIGNATURE           =  2;
// certenrolld_end

  {$EXTERNALSYM CRYPT_USERDATA}
  CRYPT_USERDATA         =  1;

  // dwParam
  {$EXTERNALSYM KP_IV}
  KP_IV                   = 1;       // Initialization vector
  {$EXTERNALSYM KP_SALT}
  KP_SALT                 = 2;       // Salt value
  {$EXTERNALSYM KP_PADDING}
  KP_PADDING              = 3;       // Padding values
  {$EXTERNALSYM KP_MODE}
  KP_MODE                 = 4;       // Mode of the cipher
  {$EXTERNALSYM KP_MODE_BITS}
  KP_MODE_BITS            = 5;       // Number of bits to feedback
  {$EXTERNALSYM KP_PERMISSIONS}
  KP_PERMISSIONS          = 6;       // Key permissions DWORD
  {$EXTERNALSYM KP_ALGID}
  KP_ALGID                = 7;       // Key algorithm
  {$EXTERNALSYM KP_BLOCKLEN}
  KP_BLOCKLEN             = 8;       // Block size of the cipher
  {$EXTERNALSYM KP_KEYLEN}
  KP_KEYLEN               = 9;       // Length of key in bits
  {$EXTERNALSYM KP_SALT_EX}
  KP_SALT_EX              = 10;      // Length of salt in bytes
  {$EXTERNALSYM KP_P}
  KP_P                    = 11;      // DSS/Diffie-Hellman P value
  {$EXTERNALSYM KP_G}
  KP_G                    = 12;      // DSS/Diffie-Hellman G value
  {$EXTERNALSYM KP_Q}
  KP_Q                    = 13;      // DSS Q value
  {$EXTERNALSYM KP_X}
  KP_X                    = 14;      // Diffie-Hellman X value
  {$EXTERNALSYM KP_Y}
  KP_Y                    = 15;      // Y value
  {$EXTERNALSYM KP_RA}
  KP_RA                   = 16;      // Fortezza RA value
  {$EXTERNALSYM KP_RB}
  KP_RB                   = 17;      // Fortezza RB value
  {$EXTERNALSYM KP_INFO}
  KP_INFO                 = 18;      // for putting information into an RSA envelope
  {$EXTERNALSYM KP_EFFECTIVE_KEYLEN}
  KP_EFFECTIVE_KEYLEN     = 19;      // setting and getting RC2 effective key length
  {$EXTERNALSYM KP_SCHANNEL_ALG}
  KP_SCHANNEL_ALG         = 20;      // for setting the Secure Channel algorithms
  {$EXTERNALSYM KP_CLIENT_RANDOM}
  KP_CLIENT_RANDOM        = 21;      // for setting the Secure Channel client random data
  {$EXTERNALSYM KP_SERVER_RANDOM}
  KP_SERVER_RANDOM        = 22;      // for setting the Secure Channel server random data
  {$EXTERNALSYM KP_RP}
  KP_RP                   = 23;
  {$EXTERNALSYM KP_PRECOMP_MD5}
  KP_PRECOMP_MD5          = 24;
  {$EXTERNALSYM KP_PRECOMP_SHA}
  KP_PRECOMP_SHA          = 25;
  {$EXTERNALSYM KP_CERTIFICATE}
  KP_CERTIFICATE          = 26;      // for setting Secure Channel certificate data (PCT1)
  {$EXTERNALSYM KP_CLEAR_KEY}
  KP_CLEAR_KEY            = 27;      // for setting Secure Channel clear key data (PCT1)
  {$EXTERNALSYM KP_PUB_EX_LEN}
  KP_PUB_EX_LEN           = 28;
  {$EXTERNALSYM KP_PUB_EX_VAL}
  KP_PUB_EX_VAL           = 29;
  {$EXTERNALSYM KP_KEYVAL}
  KP_KEYVAL               = 30;
  {$EXTERNALSYM KP_ADMIN_PIN}
  KP_ADMIN_PIN            = 31;
  {$EXTERNALSYM KP_KEYEXCHANGE_PIN}
  KP_KEYEXCHANGE_PIN      = 32;
  {$EXTERNALSYM KP_SIGNATURE_PIN}
  KP_SIGNATURE_PIN        = 33;
  {$EXTERNALSYM KP_PREHASH}
  KP_PREHASH              = 34;
  {$EXTERNALSYM KP_ROUNDS}
  KP_ROUNDS               = 35;
  {$EXTERNALSYM KP_OAEP_PARAMS}
  KP_OAEP_PARAMS          = 36;      // for setting OAEP params on RSA keys
  {$EXTERNALSYM KP_CMS_KEY_INFO}
  KP_CMS_KEY_INFO         = 37;
  {$EXTERNALSYM KP_CMS_DH_KEY_INFO}
  KP_CMS_DH_KEY_INFO      = 38;
  {$EXTERNALSYM KP_PUB_PARAMS}
  KP_PUB_PARAMS           = 39;      // for setting public parameters
  {$EXTERNALSYM KP_VERIFY_PARAMS}
  KP_VERIFY_PARAMS        = 40;      // for verifying DSA and DH parameters
  {$EXTERNALSYM KP_HIGHEST_VERSION}
  KP_HIGHEST_VERSION      = 41;      // for TLS protocol version setting
  {$EXTERNALSYM KP_GET_USE_COUNT}
  KP_GET_USE_COUNT        = 42;      // for use with PP_CRYPT_COUNT_KEY_USE contexts

  // KP_PADDING
  {$EXTERNALSYM PKCS5_PADDING}
  PKCS5_PADDING           = 1;       // PKCS 5 (sec 6.2) padding method
  {$EXTERNALSYM RANDOM_PADDING}
  RANDOM_PADDING          = 2;
  {$EXTERNALSYM ZERO_PADDING}
  ZERO_PADDING            = 3;

  // KP_MODE
  {$EXTERNALSYM CRYPT_MODE_CBC}
  CRYPT_MODE_CBC          = 1;       // Cipher block chaining
  {$EXTERNALSYM CRYPT_MODE_ECB}
  CRYPT_MODE_ECB          = 2;       // Electronic code book
  {$EXTERNALSYM CRYPT_MODE_OFB}
  CRYPT_MODE_OFB          = 3;       // Output feedback mode
  {$EXTERNALSYM CRYPT_MODE_CFB}
  CRYPT_MODE_CFB          = 4;       // Cipher feedback mode
  {$EXTERNALSYM CRYPT_MODE_CTS}
  CRYPT_MODE_CTS          = 5;       // Ciphertext stealing mode

  // KP_PERMISSIONS
  {$EXTERNALSYM CRYPT_ENCRYPT}
  CRYPT_ENCRYPT           = $0001;  // Allow encryption
  {$EXTERNALSYM CRYPT_DECRYPT}
  CRYPT_DECRYPT           = $0002;  // Allow decryption
  {$EXTERNALSYM CRYPT_EXPORT}
  CRYPT_EXPORT            = $0004;  // Allow key to be exported
  {$EXTERNALSYM CRYPT_READ}
  CRYPT_READ              = $0008;  // Allow parameters to be read
  {$EXTERNALSYM CRYPT_WRITE}
  CRYPT_WRITE             = $0010;  // Allow parameters to be set
  {$EXTERNALSYM CRYPT_MAC}
  CRYPT_MAC               = $0020;  // Allow MACs to be used with key
  {$EXTERNALSYM CRYPT_EXPORT_KEY}
  CRYPT_EXPORT_KEY        = $0040;  // Allow key to be used for exporting keys
  {$EXTERNALSYM CRYPT_IMPORT_KEY}
  CRYPT_IMPORT_KEY        = $0080;  // Allow key to be used for importing keys
  {$EXTERNALSYM CRYPT_ARCHIVE}
  CRYPT_ARCHIVE           = $0100;  // Allow key to be exported at creation only

  {$EXTERNALSYM HP_ALGID}
  HP_ALGID                = $0001;  // Hash algorithm
  {$EXTERNALSYM HP_HASHVAL}
  HP_HASHVAL              = $0002;  // Hash value
  {$EXTERNALSYM HP_HASHSIZE}
  HP_HASHSIZE             = $0004;  // Hash value size
  {$EXTERNALSYM HP_HMAC_INFO}
  HP_HMAC_INFO            = $0005;  // information for creating an HMAC
  {$EXTERNALSYM HP_TLS1PRF_LABEL}
  HP_TLS1PRF_LABEL        = $0006;  // label for TLS1 PRF
  {$EXTERNALSYM HP_TLS1PRF_SEED}
  HP_TLS1PRF_SEED         = $0007;  // seed for TLS1 PRF

  {$EXTERNALSYM CRYPT_FAILED}
  CRYPT_FAILED           =  False;
  {$EXTERNALSYM CRYPT_SUCCEED}
  CRYPT_SUCCEED          =  True;

{$EXTERNALSYM RCRYPT_SUCCEEDED}
function RCRYPT_SUCCEEDED(rt: BOOL): Boolean;
{$EXTERNALSYM RCRYPT_FAILED}
function RCRYPT_FAILED(rt: BOOL): Boolean;

const
  //
  // CryptGetProvParam
  //
  {$EXTERNALSYM PP_ENUMALGS}
  PP_ENUMALGS            = 1;
  {$EXTERNALSYM PP_ENUMCONTAINERS}
  PP_ENUMCONTAINERS      = 2;
  {$EXTERNALSYM PP_IMPTYPE}
  PP_IMPTYPE             = 3;
  {$EXTERNALSYM PP_NAME}
  PP_NAME                = 4;
  {$EXTERNALSYM PP_VERSION}
  PP_VERSION             = 5;
  {$EXTERNALSYM PP_CONTAINER}
  PP_CONTAINER           = 6;
  {$EXTERNALSYM PP_CHANGE_PASSWORD}
  PP_CHANGE_PASSWORD     = 7;
  {$EXTERNALSYM PP_KEYSET_SEC_DESCR}
  PP_KEYSET_SEC_DESCR    = 8;      // get/set security descriptor of keyset
  {$EXTERNALSYM PP_CERTCHAIN}
  PP_CERTCHAIN           = 9;      // for retrieving certificates from tokens
  {$EXTERNALSYM PP_KEY_TYPE_SUBTYPE}
  PP_KEY_TYPE_SUBTYPE    = 10;
  {$EXTERNALSYM PP_PROVTYPE}
  PP_PROVTYPE            = 16;
  {$EXTERNALSYM PP_KEYSTORAGE}
  PP_KEYSTORAGE          = 17;
  {$EXTERNALSYM PP_APPLI_CERT}
  PP_APPLI_CERT          = 18;
  {$EXTERNALSYM PP_SYM_KEYSIZE}
  PP_SYM_KEYSIZE         = 19;
  {$EXTERNALSYM PP_SESSION_KEYSIZE}
  PP_SESSION_KEYSIZE     = 20;
  {$EXTERNALSYM PP_UI_PROMPT}
  PP_UI_PROMPT           = 21;
  {$EXTERNALSYM PP_ENUMALGS_EX}
  PP_ENUMALGS_EX         = 22;
  {$EXTERNALSYM PP_ENUMMANDROOTS}
  PP_ENUMMANDROOTS       = 25;
  {$EXTERNALSYM PP_ENUMELECTROOTS}
  PP_ENUMELECTROOTS      = 26;
  {$EXTERNALSYM PP_KEYSET_TYPE}
  PP_KEYSET_TYPE         = 27;
  {$EXTERNALSYM PP_ADMIN_PIN}
  PP_ADMIN_PIN           = 31;
  {$EXTERNALSYM PP_KEYEXCHANGE_PIN}
  PP_KEYEXCHANGE_PIN     = 32;
  {$EXTERNALSYM PP_SIGNATURE_PIN}
  PP_SIGNATURE_PIN       = 33;
  {$EXTERNALSYM PP_SIG_KEYSIZE_INC}
  PP_SIG_KEYSIZE_INC     = 34;
  {$EXTERNALSYM PP_KEYX_KEYSIZE_INC}
  PP_KEYX_KEYSIZE_INC    = 35;
  {$EXTERNALSYM PP_UNIQUE_CONTAINER}
  PP_UNIQUE_CONTAINER    = 36;
  {$EXTERNALSYM PP_SGC_INFO}
  PP_SGC_INFO            = 37;
  {$EXTERNALSYM PP_USE_HARDWARE_RNG}
  PP_USE_HARDWARE_RNG    = 38;
  {$EXTERNALSYM PP_KEYSPEC}
  PP_KEYSPEC             = 39;
  {$EXTERNALSYM PP_ENUMEX_SIGNING_PROT}
  PP_ENUMEX_SIGNING_PROT = 40;
  {$EXTERNALSYM PP_CRYPT_COUNT_KEY_USE}
  PP_CRYPT_COUNT_KEY_USE = 41;
  {$EXTERNALSYM PP_USER_CERTSTORE}
  PP_USER_CERTSTORE      = 42;
  {$EXTERNALSYM PP_SMARTCARD_READER}
  PP_SMARTCARD_READER    = 43;
  {$EXTERNALSYM PP_SMARTCARD_GUID}
  PP_SMARTCARD_GUID      = 45;
  {$EXTERNALSYM PP_ROOT_CERTSTORE}
  PP_ROOT_CERTSTORE      = 46;

  {$EXTERNALSYM CRYPT_FIRST}
  CRYPT_FIRST            = 1;
  {$EXTERNALSYM CRYPT_NEXT}
  CRYPT_NEXT             = 2;
  {$EXTERNALSYM CRYPT_SGC_ENUM}
  CRYPT_SGC_ENUM         = 4;

  {$EXTERNALSYM CRYPT_IMPL_HARDWARE}
  CRYPT_IMPL_HARDWARE    = 1;
  {$EXTERNALSYM CRYPT_IMPL_SOFTWARE}
  CRYPT_IMPL_SOFTWARE    = 2;
  {$EXTERNALSYM CRYPT_IMPL_MIXED}
  CRYPT_IMPL_MIXED       = 3;
  {$EXTERNALSYM CRYPT_IMPL_UNKNOWN}
  CRYPT_IMPL_UNKNOWN     = 4;
  {$EXTERNALSYM CRYPT_IMPL_REMOVABLE}
  CRYPT_IMPL_REMOVABLE   = 8;

// key storage flags
  {$EXTERNALSYM CRYPT_SEC_DESCR}
  CRYPT_SEC_DESCR         = 00000001;
  {$EXTERNALSYM CRYPT_PSTORE}
  CRYPT_PSTORE            = 00000002;
  {$EXTERNALSYM CRYPT_UI_PROMPT}
  CRYPT_UI_PROMPT         = 00000004;

// protocol flags
  {$EXTERNALSYM CRYPT_FLAG_PCT1}
  CRYPT_FLAG_PCT1         = 0001;
  {$EXTERNALSYM CRYPT_FLAG_SSL2}
  CRYPT_FLAG_SSL2         = 0002;
  {$EXTERNALSYM CRYPT_FLAG_SSL3}
  CRYPT_FLAG_SSL3         = 0004;
  {$EXTERNALSYM CRYPT_FLAG_TLS1}
  CRYPT_FLAG_TLS1         = 0008;
  {$EXTERNALSYM CRYPT_FLAG_IPSEC}
  CRYPT_FLAG_IPSEC        = 0010;
  {$EXTERNALSYM CRYPT_FLAG_SIGNING}
  CRYPT_FLAG_SIGNING      = 0020;

// SGC flags
  {$EXTERNALSYM CRYPT_SGC}
  CRYPT_SGC               = 0001;
  {$EXTERNALSYM CRYPT_FASTSGC}
  CRYPT_FASTSGC           = 0002;

//
// CryptSetProvParam
//
  {$EXTERNALSYM PP_CLIENT_HWND}
  PP_CLIENT_HWND         = 1;
  {$EXTERNALSYM PP_CONTEXT_INFO}
  PP_CONTEXT_INFO        = 11;
  {$EXTERNALSYM PP_KEYEXCHANGE_KEYSIZE}
  PP_KEYEXCHANGE_KEYSIZE = 12;
  {$EXTERNALSYM PP_SIGNATURE_KEYSIZE}
  PP_SIGNATURE_KEYSIZE   = 13;
  {$EXTERNALSYM PP_KEYEXCHANGE_ALG}
  PP_KEYEXCHANGE_ALG     = 14;
  {$EXTERNALSYM PP_SIGNATURE_ALG}
  PP_SIGNATURE_ALG       = 15;
  {$EXTERNALSYM PP_DELETEKEY}
  PP_DELETEKEY           = 24;
  {$EXTERNALSYM PP_PIN_PROMPT_STRING}
  PP_PIN_PROMPT_STRING   = 44;

// certenrolld_begin -- PROV_RSA_*
  {$EXTERNALSYM PROV_RSA_FULL}
  PROV_RSA_FULL          = 1;
  {$EXTERNALSYM PROV_RSA_SIG}
  PROV_RSA_SIG           = 2;
  {$EXTERNALSYM PROV_DSS}
  PROV_DSS               = 3;
  {$EXTERNALSYM PROV_FORTEZZA}
  PROV_FORTEZZA          = 4;
  {$EXTERNALSYM PROV_MS_EXCHANGE}
  PROV_MS_EXCHANGE       = 5;
  {$EXTERNALSYM PROV_SSL}
  PROV_SSL               = 6;
  {$EXTERNALSYM PROV_RSA_SCHANNEL}
  PROV_RSA_SCHANNEL      = 12;
  {$EXTERNALSYM PROV_DSS_DH}
  PROV_DSS_DH            = 13;
  {$EXTERNALSYM PROV_EC_ECDSA_SIG}
  PROV_EC_ECDSA_SIG      = 14;
  {$EXTERNALSYM PROV_EC_ECNRA_SIG}
  PROV_EC_ECNRA_SIG      = 15;
  {$EXTERNALSYM PROV_EC_ECDSA_FULL}
  PROV_EC_ECDSA_FULL     = 16;
  {$EXTERNALSYM PROV_EC_ECNRA_FULL}
  PROV_EC_ECNRA_FULL     = 17;
  {$EXTERNALSYM PROV_DH_SCHANNEL}
  PROV_DH_SCHANNEL       = 18;
  {$EXTERNALSYM PROV_SPYRUS_LYNKS}
  PROV_SPYRUS_LYNKS      = 20;
  {$EXTERNALSYM PROV_RNG}
  PROV_RNG               = 21;
  {$EXTERNALSYM PROV_INTEL_SEC}
  PROV_INTEL_SEC         = 22;
  {$EXTERNALSYM PROV_REPLACE_OWF}
  PROV_REPLACE_OWF       = 23;
  {$EXTERNALSYM PROV_RSA_AES}
  PROV_RSA_AES           = 24;
// certenrolld_end

//
// STT defined Providers
//
  {$EXTERNALSYM PROV_STT_MER}
  PROV_STT_MER           = 7;
  {$EXTERNALSYM PROV_STT_ACQ}
  PROV_STT_ACQ           = 8;
  {$EXTERNALSYM PROV_STT_BRND}
  PROV_STT_BRND          = 9;
  {$EXTERNALSYM PROV_STT_ROOT}
  PROV_STT_ROOT          = 10;
  {$EXTERNALSYM PROV_STT_ISS}
  PROV_STT_ISS           = 11;

//
// Provider friendly names
//
  {$EXTERNALSYM MS_DEF_PROV_A}
  MS_DEF_PROV_A          = 'Microsoft Base Cryptographic Provider v1.0';
  {$EXTERNALSYM MS_DEF_PROV_W}
  MS_DEF_PROV_W          = 'Microsoft Base Cryptographic Provider v1.0';
  {$EXTERNALSYM MS_DEF_PROV}
{$IFDEF UNICODE}
  MS_DEF_PROV            = MS_DEF_PROV_W;
{$ELSE}
  MS_DEF_PROV            = MS_DEF_PROV_A;
{$ENDIF}

  {$EXTERNALSYM MS_ENHANCED_PROV_A}
  MS_ENHANCED_PROV_A     = 'Microsoft Enhanced Cryptographic Provider v1.0';
  {$EXTERNALSYM MS_ENHANCED_PROV_W}
  MS_ENHANCED_PROV_W     = 'Microsoft Enhanced Cryptographic Provider v1.0';
  {$EXTERNALSYM MS_ENHANCED_PROV}
{$IFDEF UNICODE}
  MS_ENHANCED_PROV       = MS_ENHANCED_PROV_W;
{$ELSE}
  MS_ENHANCED_PROV       = MS_ENHANCED_PROV_A;
{$ENDIF}

  {$EXTERNALSYM MS_STRONG_PROV_A}
  MS_STRONG_PROV_A       = 'Microsoft Strong Cryptographic Provider';
  {$EXTERNALSYM MS_STRONG_PROV_W}
  MS_STRONG_PROV_W       = 'Microsoft Strong Cryptographic Provider';
  {$EXTERNALSYM MS_STRONG_PROV}
{$IFDEF UNICODE}
  MS_STRONG_PROV         = MS_STRONG_PROV_W;
{$ELSE}
  MS_STRONG_PROV         = MS_STRONG_PROV_A;
{$ENDIF}

  {$EXTERNALSYM MS_DEF_RSA_SIG_PROV_A}
  MS_DEF_RSA_SIG_PROV_A  = 'Microsoft RSA Signature Cryptographic Provider';
  {$EXTERNALSYM MS_DEF_RSA_SIG_PROV_W}
  MS_DEF_RSA_SIG_PROV_W  = 'Microsoft RSA Signature Cryptographic Provider';
  {$EXTERNALSYM MS_DEF_RSA_SIG_PROV}
{$IFDEF UNICODE}
  MS_DEF_RSA_SIG_PROV    = MS_DEF_RSA_SIG_PROV_W;
{$ELSE}
  MS_DEF_RSA_SIG_PROV    = MS_DEF_RSA_SIG_PROV_A;
{$ENDIF}

  {$EXTERNALSYM MS_DEF_RSA_SCHANNEL_PROV_A}
  MS_DEF_RSA_SCHANNEL_PROV_A = 'Microsoft RSA SChannel Cryptographic Provider';
  {$EXTERNALSYM MS_DEF_RSA_SCHANNEL_PROV_W}
  MS_DEF_RSA_SCHANNEL_PROV_W = 'Microsoft RSA SChannel Cryptographic Provider';
  {$EXTERNALSYM MS_DEF_RSA_SCHANNEL_PROV}
{$IFDEF UNICODE}
  MS_DEF_RSA_SCHANNEL_PROV   = MS_DEF_RSA_SCHANNEL_PROV_W;
{$ELSE}
  MS_DEF_RSA_SCHANNEL_PROV   = MS_DEF_RSA_SCHANNEL_PROV_A;
{$ENDIF}

  {$EXTERNALSYM MS_DEF_DSS_PROV_A}
  MS_DEF_DSS_PROV_A      = 'Microsoft Base DSS Cryptographic Provider';
  {$EXTERNALSYM MS_DEF_DSS_PROV_W}
  MS_DEF_DSS_PROV_W      = 'Microsoft Base DSS Cryptographic Provider';
  {$EXTERNALSYM MS_DEF_DSS_PROV}
{$IFDEF UNICODE}
  MS_DEF_DSS_PROV        = MS_DEF_DSS_PROV_W;
{$ELSE}
  MS_DEF_DSS_PROV        = MS_DEF_DSS_PROV_A;
{$ENDIF}

  {$EXTERNALSYM MS_DEF_DSS_DH_PROV_A}
  MS_DEF_DSS_DH_PROV_A   = 'Microsoft Base DSS and Diffie-Hellman Cryptographic Provider';
  {$EXTERNALSYM MS_DEF_DSS_DH_PROV_W}
  MS_DEF_DSS_DH_PROV_W   = 'Microsoft Base DSS and Diffie-Hellman Cryptographic Provider';
  {$EXTERNALSYM MS_DEF_DSS_DH_PROV}
{$IFDEF UNICODE}
  MS_DEF_DSS_DH_PROV     = MS_DEF_DSS_DH_PROV_W;
{$ELSE}
  MS_DEF_DSS_DH_PROV     = MS_DEF_DSS_DH_PROV_A;
{$ENDIF}

  {$EXTERNALSYM MS_ENH_DSS_DH_PROV_A}
  MS_ENH_DSS_DH_PROV_A   = 'Microsoft Enhanced DSS and Diffie-Hellman Cryptographic Provider';
  {$EXTERNALSYM MS_ENH_DSS_DH_PROV_W}
  MS_ENH_DSS_DH_PROV_W   = 'Microsoft Enhanced DSS and Diffie-Hellman Cryptographic Provider';
  {$EXTERNALSYM MS_ENH_DSS_DH_PROV}
{$IFDEF UNICODE}
  MS_ENH_DSS_DH_PROV     = MS_ENH_DSS_DH_PROV_W;
{$ELSE}
  MS_ENH_DSS_DH_PROV     = MS_ENH_DSS_DH_PROV_A;
{$ENDIF}

  {$EXTERNALSYM MS_DEF_DH_SCHANNEL_PROV_A}
  MS_DEF_DH_SCHANNEL_PROV_A = 'Microsoft DH SChannel Cryptographic Provider';
  {$EXTERNALSYM MS_DEF_DH_SCHANNEL_PROV_W}
  MS_DEF_DH_SCHANNEL_PROV_W = 'Microsoft DH SChannel Cryptographic Provider';
  {$EXTERNALSYM MS_DEF_DH_SCHANNEL_PROV}
{$IFDEF UNICODE}
  MS_DEF_DH_SCHANNEL_PROV = MS_DEF_DH_SCHANNEL_PROV_W;
{$ELSE}
  MS_DEF_DH_SCHANNEL_PROV = MS_DEF_DH_SCHANNEL_PROV_A;
{$ENDIF}

  {$EXTERNALSYM MS_SCARD_PROV_A}
  MS_SCARD_PROV_A        = 'Microsoft Base Smart Card Crypto Provider';
  {$EXTERNALSYM MS_SCARD_PROV_W}
  MS_SCARD_PROV_W        = 'Microsoft Base Smart Card Crypto Provider';
  {$EXTERNALSYM MS_SCARD_PROV}
{$IFDEF UNICODE}
  MS_SCARD_PROV          = MS_SCARD_PROV_W;
{$ELSE}
  MS_SCARD_PROV          = MS_SCARD_PROV_A;
{$ENDIF}

  {$EXTERNALSYM MS_ENH_RSA_AES_PROV_A}
  MS_ENH_RSA_AES_PROV_A  = 'Microsoft Enhanced RSA and AES Cryptographic Provider';
  {$EXTERNALSYM MS_ENH_RSA_AES_PROV_W}
  MS_ENH_RSA_AES_PROV_W  = 'Microsoft Enhanced RSA and AES Cryptographic Provider';
  {$EXTERNALSYM MS_ENH_RSA_AES_PROV}
{$IFDEF UNICODE}
  MS_ENH_RSA_AES_PROV    = MS_ENH_RSA_AES_PROV_W;
{$ELSE}
  MS_ENH_RSA_AES_PROV    = MS_ENH_RSA_AES_PROV_A;
{$ENDIF}

  {$EXTERNALSYM MAXUIDLEN}
  MAXUIDLEN              = 64;

// Exponentiation Offload Reg Location
  {$EXTERNALSYM EXPO_OFFLOAD_REG_VALUE}
  EXPO_OFFLOAD_REG_VALUE = 'ExpoOffload';
  {$EXTERNALSYM EXPO_OFFLOAD_FUNC_NAME}
  EXPO_OFFLOAD_FUNC_NAME = 'OffloadModExpo';

//
// Registry key in which the following private key-related
// values are created.
//
  {$EXTERNALSYM szKEY_CRYPTOAPI_PRIVATE_KEY_OPTIONS}
  szKEY_CRYPTOAPI_PRIVATE_KEY_OPTIONS = 'Software\Policies\Microsoft\Cryptography';

//
// Registry value for controlling Data Protection API (DPAPI) UI settings.
//
  {$EXTERNALSYM szFORCE_KEY_PROTECTION}
  szFORCE_KEY_PROTECTION              = 'ForceKeyProtection';

  {$EXTERNALSYM dwFORCE_KEY_PROTECTION_DISABLED}
  dwFORCE_KEY_PROTECTION_DISABLED     = $0;
  {$EXTERNALSYM dwFORCE_KEY_PROTECTION_USER_SELECT}
  dwFORCE_KEY_PROTECTION_USER_SELECT  = $1;
  {$EXTERNALSYM dwFORCE_KEY_PROTECTION_HIGH}
  dwFORCE_KEY_PROTECTION_HIGH         = $2;

//
// Registry values for enabling and controlling the caching (and timeout)
// of private keys.  This feature is intended for UI-protected private
// keys.
//
// Note that in Windows 2000 and later, private keys, once read from storage,
// are cached in the associated HCRYPTPROV structure for subsequent use.
//
// In Server 2003 and XP SP1, new key caching behavior is available.  Keys
// that have been read from storage and cached may now be considered 'stale'
// if a period of time has elapsed since the key was last used.  This forces
// the key to be re-read from storage (which will make the DPAPI UI appear
// again).
//
// Optional Key Timeouts:
//
// In Windows Server 2003, XP SP1, and later, new key caching behavior is
// available.  Keys that have been read from storage and cached per-context
// may now be considered 'stale' if a period of time has elapsed since the
// key was last used.  This forces the key to be re-read from storage (which
// will make the Data Protection API dialog appear again if the key is
// UI-protected).
//
// To enable the new behavior, create the registry DWORD value
// szKEY_CACHE_ENABLED and set it to 1.  The registry DWORD value
// szKEY_CACHE_SECONDS must also be created and set to the number of seconds
// that a cached private key may still be considered usable.
//
  {$EXTERNALSYM szKEY_CACHE_ENABLED}
  szKEY_CACHE_ENABLED                 = 'CachePrivateKeys';
  {$EXTERNALSYM szKEY_CACHE_SECONDS}
  szKEY_CACHE_SECONDS                 = 'PrivateKeyLifetimeSeconds';

//
// In platforms later than (and not including) Windows Server 2003, private
// keys are always cached for a period of time per-process, even when
// not being used in any context.
//
// The differences between the process-wide caching settings described below
// and the Optional Key Timeouts described above are subtle.
//
//  - The Optional Key Timeout policy is applied only when an attempt is made
//    to use a specific private key with an open context handle (HCRYPTPROV).
//    If szKEY_CACHE_SECONDS have elapsed since the key was last used, the
//    private key will be re-read from storage.
//
//  - The Cache Purge Interval policy, below, is applied whenever any
//    non-ephemeral private key is used or read from storage.  If
//    szPRIV_KEY_CACHE_PURGE_INTERVAL_SECONDS have elapsed since the last
//    purge occurred, all cached keys that have not been referenced since the
//    last purge will be removed from the cache.
//
//    If a private key that is purged from the cache is currently
//    referenced in an open context, then the key will be re-read from storage
//    the next time an attempt is made to use it (via any context).
//
// The following two registry DWORD values control this behavior.
//

//
// Registry value for controlling the maximum number of persisted
// (non-ephemeral) private keys that can be cached per-process.  If the cache
// fills up, keys will be replaced on a least-recently-used basis.  If the
// maximum number of cached keys is set to zero, no keys will be globally
// cached.
//
  {$EXTERNALSYM szPRIV_KEY_CACHE_MAX_ITEMS}
  szPRIV_KEY_CACHE_MAX_ITEMS              = 'PrivKeyCacheMaxItems';
  {$EXTERNALSYM cPRIV_KEY_CACHE_MAX_ITEMS_DEFAULT}
  cPRIV_KEY_CACHE_MAX_ITEMS_DEFAULT       = 20;

//
// Registry value for controlling the interval at which the private key
// cache is proactively purged of outdated keys.
//
  {$EXTERNALSYM szPRIV_KEY_CACHE_PURGE_INTERVAL_SECONDS}
  szPRIV_KEY_CACHE_PURGE_INTERVAL_SECONDS = 'PrivKeyCachePurgeIntervalSeconds';
  {$EXTERNALSYM cPRIV_KEY_CACHE_PURGE_INTERVAL_SECONDS_DEFAULT}
  cPRIV_KEY_CACHE_PURGE_INTERVAL_SECONDS_DEFAULT = 86400; // 1 day

  {$EXTERNALSYM CUR_BLOB_VERSION}
  CUR_BLOB_VERSION        = 2;

type
// structure for use with CryptSetKeyParam for CMS keys
// DO NOT USE THIS STRUCTURE!!!!!
  PCMSKeyInfo = ^TCMSKeyInfo;
  {$EXTERNALSYM _CMS_KEY_INFO}
  _CMS_KEY_INFO = record
    dwVersion: DWORD;           // sizeof(CMS_KEY_INFO)
    Algid: ALG_ID;              // algorithmm id for the key to be converted
    pbOID: PBYTE;               // pointer to OID to hash in with Z
    cbOID: DWORD;               // length of OID to hash in with Z
  end;
  {$EXTERNALSYM CMS_KEY_INFO}
  CMS_KEY_INFO = _CMS_KEY_INFO;
  {$EXTERNALSYM PCMS_KEY_INFO}
  PCMS_KEY_INFO = ^_CMS_KEY_INFO;
  TCMSKeyInfo = _CMS_KEY_INFO;

// structure for use with CryptSetHashParam with CALG_HMAC
  PHMACInfo = ^THMACInfo;
  {$EXTERNALSYM _HMAC_Info}
  _HMAC_Info = record
    HashAlgid: ALG_ID;
    pbInnerString: PBYTE;
    cbInnerString: DWORD;
    pbOuterString: PBYTE;
    cbOuterString: DWORD;
  end;
  {$EXTERNALSYM HMAC_INFO}
  HMAC_INFO = _HMAC_INFO;
  {$EXTERNALSYM PHMAC_INFO}
  PHMAC_INFO = ^_HMAC_INFO;
  THMACInfo = _HMAC_INFO;

// structure for use with CryptSetKeyParam with KP_SCHANNEL_ALG
  PSChannelALG = ^TSChannelALG;
  {$EXTERNALSYM _SCHANNEL_ALG}
  _SCHANNEL_ALG  = record
    dwUse: DWORD;
    Algid: ALG_ID;
    cBits: DWORD;
    dwFlags: DWORD;
    dwReserved: DWORD;
  end;
  {$EXTERNALSYM SCHANNEL_ALG}
  SCHANNEL_ALG = _SCHANNEL_ALG;
  {$EXTERNALSYM PSCHANNEL_ALG}
  PSCHANNEL_ALG = ^_SCHANNEL_ALG;
  TSChannelALG = _SCHANNEL_ALG;

// uses of algortihms for SCHANNEL_ALG structure
const
  {$EXTERNALSYM SCHANNEL_MAC_KEY}
  SCHANNEL_MAC_KEY    = $00000000;
  {$EXTERNALSYM SCHANNEL_ENC_KEY}
  SCHANNEL_ENC_KEY    = $00000001;

// uses of dwFlags SCHANNEL_ALG structure
  {$EXTERNALSYM INTERNATIONAL_USAGE}
  INTERNATIONAL_USAGE = $00000001;

type
  PProvEnumALGs = ^TProvEnumAlgs;
  {$EXTERNALSYM _PROV_ENUMALGS}
  _PROV_ENUMALGS = record
    aiAlgid: ALG_ID;
    dwBitLen: DWORD;
    dwNameLen: DWORD;
    szName: array[0..19] of CHAR;
  end;
  {$EXTERNALSYM PROV_ENUMALGS}
  PROV_ENUMALGS = _PROV_ENUMALGS;
  {$EXTERNALSYM PPROV_ENUMALGS}
  PPROV_ENUMALGS = ^_PROV_ENUMALGS;
  TProvEnumALGs = _PROV_ENUMALGS;

// certenrolls_begin -- PROV_ENUMALGS_EX
  PProvEnumALGsEx = ^TProvEnumALGsEx;
  {$EXTERNALSYM _PROV_ENUMALGS_EX}
  _PROV_ENUMALGS_EX = record
    aiAlgid: ALG_ID;
    dwDefaultLen: DWORD;
    dwMinLen: DWORD;
    dwMaxLen: DWORD;
    dwProtocols: DWORD;
    dwNameLen: DWORD;
    szName: array[0..19] of CHAR;
    dwLongNameLen: DWORD;
    szLongName: array[0..39] of CHAR;
  end;
  {$EXTERNALSYM PROV_ENUMALGS_EX}
  PROV_ENUMALGS_EX = _PROV_ENUMALGS_EX;
  {$EXTERNALSYM PPROV_ENUMALGS_EX}
  PPROV_ENUMALGS_EX = ^_PROV_ENUMALGS_EX;
  TProvEnumALGsEx = _PROV_ENUMALGS_EX;
// certenrolls_end

  PPublicKeyStruc = ^TPublicKeyStruc;
  PBlobHeader = ^TBlobHeader;
  {$EXTERNALSYM _PUBLICKEYSTRUC}
  _PUBLICKEYSTRUC = record
    bType: BYTE;
    bVersion: BYTE;
    reserved: WORD;
    aiKeyAlg: ALG_ID;
  end;
  {$EXTERNALSYM BLOBHEADER}
  BLOBHEADER = _PUBLICKEYSTRUC;
  {$EXTERNALSYM PUBLICKEYSTRUC}
  PUBLICKEYSTRUC = _PUBLICKEYSTRUC;
  TBlobHeader = _PUBLICKEYSTRUC;
  TPublicKeyStruc = _PUBLICKEYSTRUC;


  PRSAPubKey = ^TRSAPubKey;
  {$EXTERNALSYM _RSAPUBKEY}
  _RSAPUBKEY = record
    magic: DWORD;                  // Has to be RSA1
    bitlen: DWORD;                 // # of bits in modulus
    pubexp: DWORD;                 // public exponent
  end;                             // Modulus data follows
  {$EXTERNALSYM RSAPUBKEY}
  RSAPUBKEY = _RSAPUBKEY;
  TRSAPubKey = _RSAPUBKEY;

  PDHPubKey = ^TDHPubKey;
  PDSSPubKey = ^TDSSPubKey;
  PKEAPubKey = ^TKEAPubKey;
  PTEKPubKey = ^TTEKPubKey;
  PPubKey = ^TPubKey;
  {$EXTERNALSYM _PUBKEY}
  _PUBKEY = record
    magic: DWORD;
    bitlen: DWORD;                 // # of bits in modulus
  end;
  {$EXTERNALSYM DHPUBKEY}
  DHPUBKEY = _PUBKEY;
  {$EXTERNALSYM DSSPUBKEY}
  DSSPUBKEY = _PUBKEY;
  {$EXTERNALSYM KEAPUBKEY}
  KEAPUBKEY = _PUBKEY;
  {$EXTERNALSYM TEKPUBKEY}
  TEKPUBKEY = _PUBKEY;
  TPubKey = _PUBKEY;
  TDHPubKey = _PUBKEY;
  TDSSPubKey = _PUBKEY;
  TKEAPubKey = _PUBKEY;
  TTEKPubKey = _PUBKEY;

  PDSSSeed = ^TDSSSeed;
  {$EXTERNALSYM _DSSSEED}
  _DSSSEED = record
    counter: DWORD;
    seed: array[0..19] of BYTE;
  end;
  {$EXTERNALSYM DSSSEED}
  DSSSEED = _DSSSEED;
  TDSSSeed = _DSSSEED;

  PPubKeyVer3 = ^TPubKeyVer3;
  PDHPubKeyVer3 = ^TDHPubKeyVer3;
  PDSSPubKeyVer3 = ^TDSSPubKeyVer3;
  {$EXTERNALSYM _PUBKEYVER3}
  _PUBKEYVER3 = record
    magic: DWORD;
    bitlenP: DWORD;                // # of bits in prime modulus
    bitlenQ: DWORD;                // # of bits in prime q, 0 if not available
    bitlenJ: DWORD;                // # of bits in (p-1)/q, 0 if not available
    DSSSeed: DSSSEED;
  end;
  {$EXTERNALSYM DHPUBKEY_VER3}
  DHPUBKEY_VER3 = _PUBKEYVER3;
  {$EXTERNALSYM PDHPUBKEY_VER3}
  PDHPUBKEY_VER3 = ^_PUBKEYVER3;
  {$EXTERNALSYM DSSPUBKEY_VER3}
  DSSPUBKEY_VER3 = _PUBKEYVER3;
  {$EXTERNALSYM PDSSPUBKEY_VER3}
  PDSSPUBKEY_VER3 = ^_PUBKEYVER3;
  TPubKeyVer3 = _PUBKEYVER3;
  TDHPubKeyVer3 = _PUBKEYVER3;
  TDSSPubKeyVer3 = _PUBKEYVER3;

  PPrivKeyVer3 = ^TPrivKeyVer3;
  PDHPrivKeyVer3 = ^TDHPrivKeyVer3;
  PDSSPrivKeyVer3 = ^TDSSPrivKeyVer3;
  {$EXTERNALSYM _PRIVKEYVER3}
  _PRIVKEYVER3 = record
    magic: DWORD;
    bitlenP: DWORD;                // # of bits in prime modulus
    bitlenQ: DWORD;                // # of bits in prime q, 0 if not available
    bitlenJ: DWORD;                // # of bits in (p-1)/q, 0 if not available
    bitlenX: DWORD;                // # of bits in X
    DSSSeed: DSSSEED;
  end;
  {$EXTERNALSYM DHPRIVKEY_VER3}
  DHPRIVKEY_VER3 = _PRIVKEYVER3;
  {$EXTERNALSYM PDHPRIVKEY_VER3}
  PDHPRIVKEY_VER3 = ^_PRIVKEYVER3;
  {$EXTERNALSYM DSSPRIVKEY_VER3}
  DSSPRIVKEY_VER3 = _PRIVKEYVER3;
  {$EXTERNALSYM PDSSPRIVKEY_VER3}
  PDSSPRIVKEY_VER3 = ^_PRIVKEYVER3;
  TPrivKeyVer3 = _PRIVKEYVER3;
  TDHPrivKeyVer3 = _PRIVKEYVER3;
  TDSSPrivKeyVer3 = _PRIVKEYVER3;

  PKeyTypeSubtype = ^TKeyTypeSubType;
  {$EXTERNALSYM _KEY_TYPE_SUBTYPE}
  _KEY_TYPE_SUBTYPE = record
    dwKeySpec: DWORD;
    _Type: TGUID;
    Subtype: TGUID;
  end;
  {$EXTERNALSYM KEY_TYPE_SUBTYPE}
  KEY_TYPE_SUBTYPE = _KEY_TYPE_SUBTYPE;
  {$EXTERNALSYM PKEY_TYPE_SUBTYPE}
  PKEY_TYPE_SUBTYPE = ^_KEY_TYPE_SUBTYPE;
  TKeyTypeSubtype = _KEY_TYPE_SUBTYPE;

  PCertFortezzaDataProp = ^TCertFortezzaDataProp;
  {$EXTERNALSYM _CERT_FORTEZZA_DATA_PROP}
  _CERT_FORTEZZA_DATA_PROP = record
    SerialNumber: array[0..7] of Byte;
    CertIndex: Integer;
    CertLabel: array[0..35] of Byte;
  end;
  {$EXTERNALSYM CERT_FORTEZZA_DATA_PROP}
  CERT_FORTEZZA_DATA_PROP = _CERT_FORTEZZA_DATA_PROP;
  {$EXTERNALSYM PCERT_FORTEZZA_DATA_PROP}
  PCERT_FORTEZZA_DATA_PROP = ^_CERT_FORTEZZA_DATA_PROP;
  TCertFortezzaDataProp = _CERT_FORTEZZA_DATA_PROP;

  PCryptRC4KeyState = ^TCryptRC4KeyState;
  {$EXTERNALSYM _CRYPT_RC4_KEY_STATE}
  _CRYPT_RC4_KEY_STATE = record
    Key: array[0..15] of Byte;
    SBox: array[0..255] of Byte;
    i: Byte;
    j: Byte;
  end;
  {$EXTERNALSYM CRYPT_RC4_KEY_STATE}
  CRYPT_RC4_KEY_STATE = _CRYPT_RC4_KEY_STATE;
  {$EXTERNALSYM PCRYPT_RC4_KEY_STATE}
  PCRYPT_RC4_KEY_STATE = ^_CRYPT_RC4_KEY_STATE;
  TCryptRC4KeyState = _CRYPT_RC4_KEY_STATE;

  PCryptDESKeyState = ^TCryptDESKeyState;
  {$EXTERNALSYM _CRYPT_DES_KEY_STATE}
  _CRYPT_DES_KEY_STATE = record
    Key: array[0..7] of Byte;
    IV: array[0..7] of Byte;
    Feedback: array[0..7] of Byte;
  end;
  {$EXTERNALSYM CRYPT_DES_KEY_STATE}
  CRYPT_DES_KEY_STATE = _CRYPT_DES_KEY_STATE;
  {$EXTERNALSYM PCRYPT_DES_KEY_STATE}
  PCRYPT_DES_KEY_STATE = ^_CRYPT_DES_KEY_STATE;
  TCryptDESKeyState = _CRYPT_DES_KEY_STATE;

  PCrypt3DESKeyState = ^TCrypt3DESKeyState;
  {$EXTERNALSYM _CRYPT_3DES_KEY_STATE}
  _CRYPT_3DES_KEY_STATE = record
    Key: array[0..23] of Byte;
    IV: array[0..7] of Byte;
    Feedback: array[0..7] of Byte;
  end;
  {$EXTERNALSYM CRYPT_3DES_KEY_STATE}
  CRYPT_3DES_KEY_STATE = _CRYPT_3DES_KEY_STATE;
  {$EXTERNALSYM PCRYPT_3DES_KEY_STATE}
  PCRYPT_3DES_KEY_STATE = ^_CRYPT_3DES_KEY_STATE;
  TCrypt3DESKeyState = _CRYPT_3DES_KEY_STATE;

  {$NODEFINE TCryptAESKeyValue}
  TCryptAESKeyValue = array[0..15] of Byte;


  PCryptAES128KeyState = ^TCryptAES128KeyState;
  {$EXTERNALSYM _CRYPT_AES_128_KEY_STATE}
  _CRYPT_AES_128_KEY_STATE = record
    Key: array[0..15] of Byte;
    IV: TCryptAESKeyValue;
    EncryptionState: array[0..10] of TCryptAESKeyValue;      // 10 rounds + 1
    DecryptionState: array[0..10] of TCryptAESKeyValue;
    Feedback: TCryptAESKeyValue;
  end;
  {$EXTERNALSYM CRYPT_AES_128_KEY_STATE}
  CRYPT_AES_128_KEY_STATE = _CRYPT_AES_128_KEY_STATE;
  {$EXTERNALSYM PCRYPT_AES_128_KEY_STATE}
  PCRYPT_AES_128_KEY_STATE = ^_CRYPT_AES_128_KEY_STATE;
  TCryptAES128KeyState = _CRYPT_AES_128_KEY_STATE;

  PCryptAES256KeyState = ^TCryptAES256KeyState;
  {$EXTERNALSYM _CRYPT_AES_256_KEY_STATE}
  _CRYPT_AES_256_KEY_STATE = record
    Key: array[0..31] of Byte;
    IV: TCryptAESKeyValue;
    EncryptionState: array[0..14] of TCryptAESKeyValue;      // 14 rounds + 1
    DecryptionState: array[0..14] of TCryptAESKeyValue;
    Feedback: TCryptAESKeyValue;
  end;
  {$EXTERNALSYM CRYPT_AES_256_KEY_STATE}
  CRYPT_AES_256_KEY_STATE = _CRYPT_AES_256_KEY_STATE;
  {$EXTERNALSYM PCRYPT_AES_256_KEY_STATE}
  PCRYPT_AES_256_KEY_STATE = ^_CRYPT_AES_256_KEY_STATE;
  TCryptAES256KeyState = _CRYPT_AES_256_KEY_STATE;

//+-------------------------------------------------------------------------
//  CRYPTOAPI BLOB definitions
//--------------------------------------------------------------------------
// certenrolls_begin -- *_BLOB
  PCryptoAPIBlob = ^TCryptoAPIBlob;
  {$EXTERNALSYM _CRYPTOAPI_BLOB}
  _CRYPTOAPI_BLOB = record
    cbData: DWORD;
    pbData: PBYTE;
  end;
  {$EXTERNALSYM CRYPT_INTEGER_BLOB}
  CRYPT_INTEGER_BLOB = _CRYPTOAPI_BLOB;
  {$EXTERNALSYM PCRYPT_INTEGER_BLOB}
  PCRYPT_INTEGER_BLOB = ^_CRYPTOAPI_BLOB;
  {$EXTERNALSYM CRYPT_UINT_BLOB}
  CRYPT_UINT_BLOB = _CRYPTOAPI_BLOB;
  {$EXTERNALSYM PCRYPT_UINT_BLOB}
  PCRYPT_UINT_BLOB = ^_CRYPTOAPI_BLOB;
  {$EXTERNALSYM CRYPT_OBJID_BLOB}
  CRYPT_OBJID_BLOB = _CRYPTOAPI_BLOB;
  {$EXTERNALSYM PCRYPT_OBJID_BLOB}
  PCRYPT_OBJID_BLOB = ^_CRYPTOAPI_BLOB;
  {$EXTERNALSYM CERT_NAME_BLOB}
  CERT_NAME_BLOB = _CRYPTOAPI_BLOB;
  {$EXTERNALSYM PCERT_NAME_BLOB}
  PCERT_NAME_BLOB = ^_CRYPTOAPI_BLOB;
  {$EXTERNALSYM CERT_RDN_VALUE_BLOB}
  CERT_RDN_VALUE_BLOB = _CRYPTOAPI_BLOB;
  {$EXTERNALSYM PCERT_RDN_VALUE_BLOB}
  PCERT_RDN_VALUE_BLOB = ^_CRYPTOAPI_BLOB;
  {$EXTERNALSYM CERT_BLOB}
  CERT_BLOB = _CRYPTOAPI_BLOB;
  {$EXTERNALSYM PCERT_BLOB}
  PCERT_BLOB = ^_CRYPTOAPI_BLOB;
  {$EXTERNALSYM CRL_BLOB}
  CRL_BLOB = _CRYPTOAPI_BLOB;
  {$EXTERNALSYM PCRL_BLOB}
  PCRL_BLOB = ^_CRYPTOAPI_BLOB;
  {$EXTERNALSYM DATA_BLOB}
  DATA_BLOB = _CRYPTOAPI_BLOB;
  {$EXTERNALSYM PDATA_BLOB}
  PDATA_BLOB = ^_CRYPTOAPI_BLOB;
  {$EXTERNALSYM CRYPT_DATA_BLOB}
  CRYPT_DATA_BLOB = _CRYPTOAPI_BLOB;
  {$EXTERNALSYM PCRYPT_DATA_BLOB}
  PCRYPT_DATA_BLOB = ^_CRYPTOAPI_BLOB;
  {$EXTERNALSYM CRYPT_HASH_BLOB}
  CRYPT_HASH_BLOB = _CRYPTOAPI_BLOB;
  {$EXTERNALSYM PCRYPT_HASH_BLOB}
  PCRYPT_HASH_BLOB = ^_CRYPTOAPI_BLOB;
  {$EXTERNALSYM CRYPT_DIGEST_BLOB}
  CRYPT_DIGEST_BLOB = _CRYPTOAPI_BLOB;
  {$EXTERNALSYM PCRYPT_DIGEST_BLOB}
  PCRYPT_DIGEST_BLOB = ^_CRYPTOAPI_BLOB;
  {$EXTERNALSYM CRYPT_DER_BLOB}
  CRYPT_DER_BLOB = _CRYPTOAPI_BLOB;
  {$EXTERNALSYM PCRYPT_DER_BLOB}
  PCRYPT_DER_BLOB = ^_CRYPTOAPI_BLOB;
  {$EXTERNALSYM CRYPT_ATTR_BLOB}
  CRYPT_ATTR_BLOB = _CRYPTOAPI_BLOB;
  {$EXTERNALSYM PCRYPT_ATTR_BLOB}
  PCRYPT_ATTR_BLOB = ^_CRYPTOAPI_BLOB;
  TCryptoAPIBlob = _CRYPTOAPI_BLOB;
  PCryptIntegerBlob = ^TCryptIntegerBlob;
  TCryptIntegerBlob = _CRYPTOAPI_BLOB;
  PCryptUIntBlob = ^TCryptUIntBlob;
  TCryptUIntBlob = _CRYPTOAPI_BLOB;
  PCryptObjIDBlob = ^TCryptObjIDBlob;
  TCryptObjIDBlob = _CRYPTOAPI_BLOB;
  PCertNameBlob = ^TCertNameBlob;
  TCertNameBlob = _CRYPTOAPI_BLOB;
  PCertRdnValueBlob = ^TCertRdnValueBlob;
  TCertRdnValueBlob = _CRYPTOAPI_BLOB;
  PCertBlob = ^TCertBlob;
  TCertBlob = _CRYPTOAPI_BLOB;
  PCRLBlob = ^TCRLBlob;
  TCRLBlob = _CRYPTOAPI_BLOB;
  PDataBlob = ^TDataBlob;
  TDataBlob = _CRYPTOAPI_BLOB;
  PCryptDataBlob = ^TDataBlob;
  TCryptDataBlob = _CRYPTOAPI_BLOB;
  PCryptHashBlob = ^TDataBlob;
  TCryptHashBlob = _CRYPTOAPI_BLOB;
  PCryptDigestBlob = ^TCryptDigestBlob;
  TCryptDigestBlob = _CRYPTOAPI_BLOB;
  PCryptDERBlob = ^TCryptDERBlob;
  TCryptDERBlob = _CRYPTOAPI_BLOB;
  PCryptAttrBlob = ^TCryptAttrBlob;
  TCryptAttrBlob = _CRYPTOAPI_BLOB;
// certenrolls_end

// structure for use with CryptSetKeyParam for CMS keys
  PCMSDHKeyInfo = ^TCMSDHKeyInfo;
  {$EXTERNALSYM _CMS_DH_KEY_INFO}
  _CMS_DH_KEY_INFO = record
    dwVersion: DWORD;           // sizeof(CMS_DH_KEY_INFO)
    Algid: ALG_ID;              // algorithmm id for the key to be converted
    pszContentEncObjId: LPSTR;  // pointer to OID to hash in with Z
    PubInfo: CRYPT_DATA_BLOB;   // OPTIONAL - public information
    pReserved: Pointer;         // reserved - should be NULL
  end;
  {$EXTERNALSYM CMS_DH_KEY_INFO}
  CMS_DH_KEY_INFO = _CMS_DH_KEY_INFO;
  {$EXTERNALSYM PCMS_DH_KEY_INFO}
  PCMS_DH_KEY_INFO = ^_CMS_DH_KEY_INFO;
  TCMSDHKeyInfo = _CMS_DH_KEY_INFO;

{$EXTERNALSYM CryptAcquireContextA}
function CryptAcquireContextA(var phProv: HCRYPTPROV; szContainer: LPCSTR;
  szProvider: LPCSTR; dwProvType: DWORD; dwFlags: DWORD): BOOL; stdcall;
{$EXTERNALSYM CryptAcquireContextW}
function CryptAcquireContextW(var phProv: HCRYPTPROV; szContainer: LPCWSTR;
  szProvider: LPCWSTR; dwProvType: DWORD; dwFlags: DWORD): BOOL; stdcall;
{$EXTERNALSYM CryptAcquireContext}
{$ifdef UNICODE}
  function CryptAcquireContext(var phProv: HCRYPTPROV; szContainer: LPCSTR;
    szProvider: LPCSTR; dwProvType: DWORD; dwFlags: DWORD): BOOL; stdcall;
{$else}
  function CryptAcquireContext(var phProv: HCRYPTPROV; szContainer: LPCWSTR;
    szProvider: LPCWSTR; dwProvType: DWORD; dwFlags: DWORD): BOOL; stdcall;
{$endif}

{$EXTERNALSYM CryptReleaseContext}
function CryptReleaseContext(hProv: HCRYPTPROV; dwFlags: DWORD): BOOL; stdcall;

{$EXTERNALSYM CryptGenKey}
function CryptGenKey(hProv: HCRYPTPROV; Algid: ALG_ID; dwFlags: DWORD;
  var phKey: HCRYPTKEY): BOOL; stdcall;

{$EXTERNALSYM CryptDeriveKey}
function CryptDeriveKey(hProv: HCRYPTPROV; Algid: ALG_ID;
  hBaseData: HCRYPTHASH; dwFlags: DWORD; var phKey: HCRYPTKEY): BOOL; stdcall;

{$EXTERNALSYM CryptDestroyKey}
function CryptDestroyKey(hKey: HCRYPTKEY): BOOL; stdcall;

{$EXTERNALSYM CryptSetKeyParam}
function CryptSetKeyParam(hKey: HCRYPTKEY; dwParam: DWORD; pbData: PBYTE;
  dwFlags: DWORD): BOOL; stdcall;

{$EXTERNALSYM CryptGetKeyParam}
function CryptGetKeyParam(hKey: HCRYPTKEY; dwParam: DWORD; pbData: PBYTE;
  var dwDataLen: DWORD; dwFlags: DWORD): BOOL; stdcall;

{$EXTERNALSYM CryptSetHashParam}
function CryptSetHashParam(hHash: HCRYPTHASH; dwParam: DWORD; pbData: PBYTE;
  dwFlags: DWORD): BOOL; stdcall;

{$EXTERNALSYM CryptGetHashParam}
function CryptGetHashParam(hHash: HCRYPTHASH; dwParam: DWORD; pbData: PBYTE;
  var dwDataLen: DWORD; dwFlags: DWORD): BOOL; stdcall;

{$EXTERNALSYM CryptSetProvParam}
function CryptSetProvParam(hProv: HCRYPTPROV; dwParam: DWORD; pbData: PBYTE;
  dwFlags: DWORD): BOOL; stdcall;

{$EXTERNALSYM CryptGetProvParam}
function CryptGetProvParam(hProv: HCRYPTPROV; dwParam: DWORD; pbData: PBYTE;
  var pdwDataLen: DWORD; dwFlags: DWORD): BOOL; stdcall;

{$EXTERNALSYM CryptGenRandom}
function CryptGenRandom(hProv: HCRYPTPROV; dwLen: DWORD;
  pbBuffer: PBYTE): BOOL; stdcall;

{$EXTERNALSYM CryptGetUserKey}
function CryptGetUserKey(hProv: HCRYPTPROV; dwKeySpec: DWORD;
  out phUserKey: HCRYPTKEY): BOOL; stdcall;

{$EXTERNALSYM CryptExportKey}
function CryptExportKey(hKey: HCRYPTKEY; hExpKey: HCRYPTKEY; dwBlobType: DWORD;
  dwFlags: DWORD; pbData: PBYTE; var dwDataLen: DWORD): BOOL; stdcall;

{$EXTERNALSYM CryptImportKey}
function CryptImportKey(hProv: HCRYPTPROV; pbData: PBYTE; dwDataLen: DWORD;
  hPubKey: HCRYPTKEY; dwFlags: DWORD; out phKey: HCRYPTKEY): BOOL; stdcall;

{$EXTERNALSYM CryptEncrypt}
function CryptEncrypt(hKey: HCRYPTKEY; hHash: HCRYPTHASH; Final: BOOL;
  dwFlags: DWORD; pbData: PBYTE; out pdwDataLen: DWORD;
  dwBufLen: DWORD): BOOL; stdcall;

{$EXTERNALSYM CryptDecrypt}
function CryptDecrypt(hKey: HCRYPTKEY; hHash: HCRYPTHASH; Final: BOOL;
  dwFlags: DWORD; pbData: PBYTE; var dwDataLen: DWORD): BOOL; stdcall;

{$EXTERNALSYM CryptCreateHash}
function CryptCreateHash(hProv: HCRYPTPROV; Algid: ALG_ID; hKey: HCRYPTKEY;
  dwFlags: DWORD; out phHash: HCRYPTHASH): BOOL; stdcall;

{$EXTERNALSYM CryptHashData}
function CryptHashData(hHash: HCRYPTHASH; pbData: PBYTE;
  dwDataLen, dwFlags: DWORD): BOOL; stdcall;

{$EXTERNALSYM CryptHashSessionKey}
function CryptHashSessionKey(hHash: HCRYPTHASH; hKey: HCRYPTKEY;
  dwFlags: DWORD): BOOL; stdcall;

{$EXTERNALSYM CryptDestroyHash}
function CryptDestroyHash(hHash: HCRYPTHASH): BOOL; stdcall;


//******************************************************************************

{$EXTERNALSYM CryptSignHashA}
function CryptSignHashA(hHash: HCRYPTHASH; dwKeySpec: DWORD;
  szDescription: LPCSTR; dwFlags: DWORD; pbSignature: PBYTE;
  var pdwSigLen: DWORD): BOOL; stdcall;
{$EXTERNALSYM CryptSignHashW}
function CryptSignHashW(hHash: HCRYPTHASH; dwKeySpec: DWORD;
  szDescription: LPCWSTR; dwFlags: DWORD; pbSignature: PBYTE;
  var pdwSigLen: DWORD): BOOL; stdcall;
{$EXTERNALSYM CryptSignHash}
{$IFDEF UNICODE}
function CryptSignHash(hHash: HCRYPTHASH; dwKeySpec: DWORD;
  szDescription: LPCSTR; dwFlags: DWORD; pbSignature: PBYTE;
  var pdwSigLen: DWORD): BOOL; stdcall;
{$ELSE}
function CryptSignHash(hHash: HCRYPTHASH; dwKeySpec: DWORD;
  szDescription: LPCWSTR; dwFlags: DWORD; pbSignature: PBYTE;
  var pdwSigLen: DWORD): BOOL; stdcall;
{$ENDIF}

{$EXTERNALSYM CryptVerifySignatureA}
function CryptVerifySignatureA(hHash: HCRYPTHASH; pbSignature: PBYTE;
  dwSigLen: DWORD; hPubKey: HCRYPTKEY; szDescription: LPCSTR;
  dwFlags: DWORD): BOOL; stdcall;
{$EXTERNALSYM CryptVerifySignatureW}
function CryptVerifySignatureW(hHash: HCRYPTHASH; pbSignature: PBYTE;
  dwSigLen: DWORD; hPubKey: HCRYPTKEY; szDescription: LPCWSTR;
  dwFlags: DWORD): BOOL; stdcall;
{$EXTERNALSYM CryptVerifySignature}
{$IFDEF UNICODE}
function CryptVerifySignature(hHash: HCRYPTHASH; pbSignature: PBYTE;
  dwSigLen: DWORD; hPubKey: HCRYPTKEY; szDescription: LPCSTR;
  dwFlags: DWORD): BOOL; stdcall;
{$ELSE}
function CryptVerifySignature(hHash: HCRYPTHASH; pbSignature: PBYTE;
  dwSigLen: DWORD; hPubKey: HCRYPTKEY; szDescription: LPCWSTR;
  dwFlags: DWORD): BOOL; stdcall;
{$ENDIF}

{$EXTERNALSYM CryptSetProviderA}
function CryptSetProviderA(pszProvName: LPCSTR;
  dwProvType: DWORD): BOOL; stdcall;
{$EXTERNALSYM CryptSetProviderW}
function CryptSetProviderW(pszProvName: LPCWSTR;
  dwProvType: DWORD): BOOL; stdcall;
{$EXTERNALSYM CryptSetProvider}
{$IFDEF UNICODE}
function CryptSetProvider(pszProvName: LPCSTR;
  dwProvType: DWORD): BOOL; stdcall;
{$ELSE}
function CryptSetProvider(pszProvName: LPCWSTR;
  dwProvType: DWORD): BOOL; stdcall;
{$ENDIF}

{$EXTERNALSYM CryptSetProviderExA}
function CryptSetProviderExA(pszProvName: LPCSTR; dwProvType: DWORD;
  pdwReserved: PDWORD; dwFlags: DWORD): BOOL; stdcall;
{$EXTERNALSYM CryptSetProviderExW}
function CryptSetProviderExW(pszProvName: LPCWSTR; dwProvType: DWORD;
  pdwReserved: PDWORD; dwFlags: DWORD): BOOL; stdcall;
{$EXTERNALSYM CryptSetProviderEx}
{$IFDEF UNICODE}
function CryptSetProviderEx(pszProvName: LPCSTR; dwProvType: DWORD;
  pdwReserved: PDWORD; dwFlags: DWORD): BOOL; stdcall;
{$ELSE}
function CryptSetProviderEx(pszProvName: LPCWSTR; dwProvType: DWORD;
  pdwReserved: PDWORD; dwFlags: DWORD): BOOL; stdcall;
{$ENDIF}

{$EXTERNALSYM CryptGetDefaultProviderA}
function CryptGetDefaultProviderA(dwProvType: DWORD; pdwReserved: PDWORD;
  dwFlags: DWORD; pszProvName: LPSTR; out pcbProvName: DWORD): BOOL; stdcall;
{$EXTERNALSYM CryptGetDefaultProviderW}
function CryptGetDefaultProviderW(dwProvType: DWORD; pdwReserved: PDWORD;
  dwFlags: DWORD; pszProvName: LPWSTR; out pcbProvName: DWORD): BOOL; stdcall;
{$EXTERNALSYM CryptGetDefaultProvider}
{$IFDEF UNICODE}
function CryptGetDefaultProvider(dwProvType: DWORD; pdwReserved: PDWORD;
  dwFlags: DWORD; pszProvName: LPSTR; out pcbProvName: DWORD): BOOL; stdcall;
{$ELSE}
function CryptGetDefaultProvider(dwProvType: DWORD; pdwReserved: PDWORD;
  dwFlags: DWORD; pszProvName: LPWSTR; out pcbProvName: DWORD): BOOL; stdcall;
{$ENDIF}

{$EXTERNALSYM CryptEnumProviderTypesA}
function CryptEnumProviderTypesA(dwIndex: DWORD; pdwReserved: PDWORD;
  dwFlags: DWORD; out pdwProvType: DWORD; szTypeName: LPSTR;
  out pcbTypeName: DWORD): BOOL; stdcall;
{$EXTERNALSYM CryptEnumProviderTypesW}
function CryptEnumProviderTypesW(dwIndex: DWORD; pdwReserved: PDWORD;
  dwFlags: DWORD; out pdwProvType: DWORD; szTypeName: LPWSTR;
  out pcbTypeName: DWORD): BOOL; stdcall;
{$EXTERNALSYM CryptEnumProviderTypes}
{$IFDEF UNICODE}
function CryptEnumProviderTypes(dwIndex: DWORD; pdwReserved: PDWORD;
  dwFlags: DWORD; out pdwProvType: DWORD; szTypeName: LPSTR;
  out pcbTypeName: DWORD): BOOL; stdcall;
{$ELSE}
function CryptEnumProviderTypes(dwIndex: DWORD; pdwReserved: PDWORD;
  dwFlags: DWORD; out pdwProvType: DWORD; szTypeName: LPWSTR;
  out pcbTypeName: DWORD): BOOL; stdcall;
{$ENDIF}

{$EXTERNALSYM CryptEnumProvidersA}
function CryptEnumProvidersA(dwIndex: DWORD; pdwReserved: PDWORD;
  dwFlags: DWORD; out pdwProvType: DWORD; szProvName: LPSTR;
  var pcbProvName: DWORD): BOOL; stdcall;
{$EXTERNALSYM CryptEnumProvidersW}
function CryptEnumProvidersW(dwIndex: DWORD; pdwReserved: PDWORD;
  dwFlags: DWORD; out pdwProvType: DWORD; szProvName: LPWSTR;
  var pcbProvName: DWORD): BOOL; stdcall;
{$EXTERNALSYM CryptEnumProviders}
{$IFDEF UNICODE}
function CryptEnumProviders(dwIndex: DWORD; pdwReserved: PDWORD;
  dwFlags: DWORD; out pdwProvType: DWORD; szProvName: LPSTR;
  var pcbProvName: DWORD): BOOL; stdcall;
{$ELSE}
function CryptEnumProviders(dwIndex: DWORD; pdwReserved: PDWORD;
  dwFlags: DWORD; out pdwProvType: DWORD; szProvName: LPWSTR;
  var pcbProvName: DWORD): BOOL; stdcall;
{$ENDIF}


{$EXTERNALSYM CryptContextAddRef}
function CryptContextAddRef(hProv: HCRYPTPROV; pdwReserved: PDWORD;
  dwFlags: DWORD): BOOL; stdcall;

{$EXTERNALSYM CryptDuplicateKey}
function CryptDuplicateKey(hKey: HCRYPTKEY; pdwReserved: PDWORD;
  dwFlags: DWORD; out phKey: HCRYPTKEY): BOOL; stdcall;

{$EXTERNALSYM CryptDuplicateHash}
function CryptDuplicateHash(hHash: HCRYPTHASH; pdwReserved: PDWORD;
  dwFlags: DWORD; out phHash: HCRYPTHASH): BOOL; stdcall;

//
// This function is provided in Microsoft Windows 2000 as a means of
// installing the 128-bit encryption provider. This function is unavailable
// in Microsoft Windows XP, because Windows XP ships with the 128-bit
// encryption provider.
//
var
  {$EXTERNALSYM GetEncSChannel}
  GetEncSChannel: function(var pData: PByte; var dwDecSize: DWORD): BOOL cdecl;

type
  PCryptKeyLimits = ^TCryptKeyLimits;
  {$EXTERNALSYM _CRYPT_KEY_LIMITS_V01}
  _CRYPT_KEY_LIMITS_V01 = record
    dwVersion: DWORD;
    algId: ALG_ID;
    dwMinKeyLength: DWORD;
    dwMaxKeyLength: DWORD;
    dwRequiredFlags: DWORD;
    dwDisallowedFlags: DWORD;
  end;
  {$EXTERNALSYM CRYPT_KEY_LIMITS}
  CRYPT_KEY_LIMITS = _CRYPT_KEY_LIMITS_V01;
  {$EXTERNALSYM PCRYPT_KEY_LIMITS}
  PCRYPT_KEY_LIMITS = ^_CRYPT_KEY_LIMITS_V01;
  TCryptKeyLimits = _CRYPT_KEY_LIMITS_V01;

// Request Flag definitions
const
  {$EXTERNALSYM CRYPTLIMIT_USING_PCT}
  CRYPTLIMIT_USING_PCT = $0001;
  {$EXTERNALSYM CRYPTLIMIT_USING_SGC}
  CRYPTLIMIT_USING_SGC = $0002;

var
  {$EXTERNALSYM CryptGetLocalKeyLimits}
  CryptGetLocalKeyLimits: function(algId: ALG_ID; dwFlags: DWORD;
    out pLimits: CRYPT_KEY_LIMITS; var cbLimitLength: DWORD): BOOL; stdcall;

// In Longhorn, the following APIs were updated to support the new
// CNG (Cryptography Next Generation) BCrypt* and NCrypt* APIs in addition
// to the above CAPI1 APIs.

// Include the definitions for the CNG APIs

// #include <bcrypt.h>
// #include <ncrypt.h>

// This type is used when the API can take either the CAPI1 HCRYPTPROV or
// the CNG NCRYPT_KEY_HANDLE. Where appropriate, the HCRYPTPROV will be
// converted to a NCRYPT_KEY_HANDLE via the CNG NCryptTranslateHandle().
type
  {$EXTERNALSYM HCRYPTPROV_OR_NCRYPT_KEY_HANDLE}
  HCRYPTPROV_OR_NCRYPT_KEY_HANDLE = ULONG_PTR;

// This type is used where the HCRYPTPROV parameter is no longer used.
// The caller should always pass in NULL.
  {$EXTERNALSYM HCRYPTPROV_LEGACY}
  HCRYPTPROV_LEGACY = ULONG_PTR;

//+-------------------------------------------------------------------------
//  In a CRYPT_BIT_BLOB the last byte may contain 0-7 unused bits. Therefore, the
//  overall bit length is cbData * 8 - cUnusedBits.
//--------------------------------------------------------------------------
// certenrolls_begin -- CERT_CONTEXT
  PCryptBitBlob = ^TCryptBitBlob;
  {$EXTERNALSYM _CRYPT_BIT_BLOB}
  _CRYPT_BIT_BLOB = record
    cbData: DWORD;
    pbData: PBYTE;
    cUnusedBits: DWORD;
  end;
  {$EXTERNALSYM CRYPT_BIT_BLOB}
  CRYPT_BIT_BLOB = _CRYPT_BIT_BLOB;
  {$EXTERNALSYM PCRYPT_BIT_BLOB}
  PCRYPT_BIT_BLOB = ^_CRYPT_BIT_BLOB;
  TCryptBitBlob = _CRYPT_BIT_BLOB;

//+-------------------------------------------------------------------------
//  Type used for any algorithm
//
//  Where the Parameters CRYPT_OBJID_BLOB is in its encoded representation. For most
//  algorithm types, the Parameters CRYPT_OBJID_BLOB is NULL (Parameters.cbData = 0).
//--------------------------------------------------------------------------
  PCryptAlgorithmIdentifier = ^TCryptAlgorithmIdentifier;
  {$EXTERNALSYM _CRYPT_ALGORITHM_IDENTIFIER}
  _CRYPT_ALGORITHM_IDENTIFIER = record
    pszObjId: LPSTR;
    Parameters: CRYPT_OBJID_BLOB;
  end;
  {$EXTERNALSYM CRYPT_ALGORITHM_IDENTIFIER}
  CRYPT_ALGORITHM_IDENTIFIER = _CRYPT_ALGORITHM_IDENTIFIER;
  {$EXTERNALSYM PCRYPT_ALGORITHM_IDENTIFIER}
  PCRYPT_ALGORITHM_IDENTIFIER = ^_CRYPT_ALGORITHM_IDENTIFIER;
  TCryptAlgorithmIdentifier = _CRYPT_ALGORITHM_IDENTIFIER;
// certenrolls_end

// Following are the definitions of various algorithm object identifiers
// RSA
const
  {$EXTERNALSYM szOID_RSA}
  szOID_RSA               = '1.2.840.113549';
  {$EXTERNALSYM szOID_PKCS}
  szOID_PKCS              = '1.2.840.113549.1';
  {$EXTERNALSYM szOID_RSA_HASH}
  szOID_RSA_HASH          = '1.2.840.113549.2';
  {$EXTERNALSYM szOID_RSA_ENCRYPT}
  szOID_RSA_ENCRYPT       = '1.2.840.113549.3';

  {$EXTERNALSYM szOID_PKCS_1}
  szOID_PKCS_1            = '1.2.840.113549.1.1';
  {$EXTERNALSYM szOID_PKCS_2}
  szOID_PKCS_2            = '1.2.840.113549.1.2';
  {$EXTERNALSYM szOID_PKCS_3}
  szOID_PKCS_3            = '1.2.840.113549.1.3';
  {$EXTERNALSYM szOID_PKCS_4}
  szOID_PKCS_4            = '1.2.840.113549.1.4';
  {$EXTERNALSYM szOID_PKCS_5}
  szOID_PKCS_5            = '1.2.840.113549.1.5';
  {$EXTERNALSYM szOID_PKCS_6}
  szOID_PKCS_6            = '1.2.840.113549.1.6';
  {$EXTERNALSYM szOID_PKCS_7}
  szOID_PKCS_7            = '1.2.840.113549.1.7';
  {$EXTERNALSYM szOID_PKCS_8}
  szOID_PKCS_8            = '1.2.840.113549.1.8';
  {$EXTERNALSYM szOID_PKCS_9}
  szOID_PKCS_9            = '1.2.840.113549.1.9';
  {$EXTERNALSYM szOID_PKCS_10}
  szOID_PKCS_10           = '1.2.840.113549.1.10';
  {$EXTERNALSYM szOID_PKCS_12}
  szOID_PKCS_12           = '1.2.840.113549.1.12';

  {$EXTERNALSYM szOID_RSA_RSA}
  szOID_RSA_RSA           = '1.2.840.113549.1.1.1';
  {$EXTERNALSYM szOID_RSA_MD2RSA}
  szOID_RSA_MD2RSA        = '1.2.840.113549.1.1.2';
  {$EXTERNALSYM szOID_RSA_MD4RSA}
  szOID_RSA_MD4RSA        = '1.2.840.113549.1.1.3';
  {$EXTERNALSYM szOID_RSA_MD5RSA}
  szOID_RSA_MD5RSA        = '1.2.840.113549.1.1.4';
  {$EXTERNALSYM szOID_RSA_SHA1RSA}
  szOID_RSA_SHA1RSA       = '1.2.840.113549.1.1.5';
  {$EXTERNALSYM szOID_RSA_SETOAEP_RSA}
  szOID_RSA_SETOAEP_RSA   = '1.2.840.113549.1.1.6';

  {$EXTERNALSYM szOID_RSAES_OAEP}
  szOID_RSAES_OAEP        = '1.2.840.113549.1.1.7';
  {$EXTERNALSYM szOID_RSA_MGF1}
  szOID_RSA_MGF1          = '1.2.840.113549.1.1.8';
  {$EXTERNALSYM szOID_RSA_PSPECIFIED}
  szOID_RSA_PSPECIFIED    = '1.2.840.113549.1.1.9';
  {$EXTERNALSYM szOID_RSA_SSA_PSS}
  szOID_RSA_SSA_PSS       = '1.2.840.113549.1.1.10';
  {$EXTERNALSYM szOID_RSA_SHA256RSA}
  szOID_RSA_SHA256RSA     = '1.2.840.113549.1.1.11';
  {$EXTERNALSYM szOID_RSA_SHA384RSA}
  szOID_RSA_SHA384RSA     = '1.2.840.113549.1.1.12';
  {$EXTERNALSYM szOID_RSA_SHA512RSA}
  szOID_RSA_SHA512RSA     = '1.2.840.113549.1.1.13';

  {$EXTERNALSYM szOID_RSA_DH}
  szOID_RSA_DH            = '1.2.840.113549.1.3.1';

  {$EXTERNALSYM szOID_RSA_data}
  szOID_RSA_data          = '1.2.840.113549.1.7.1';
  {$EXTERNALSYM szOID_RSA_signedData}
  szOID_RSA_signedData    = '1.2.840.113549.1.7.2';
  {$EXTERNALSYM szOID_RSA_envelopedData}
  szOID_RSA_envelopedData = '1.2.840.113549.1.7.3';
  {$EXTERNALSYM szOID_RSA_signEnvData}
  szOID_RSA_signEnvData   = '1.2.840.113549.1.7.4';
  {$EXTERNALSYM szOID_RSA_digestedData}
  szOID_RSA_digestedData  = '1.2.840.113549.1.7.5';
  {$EXTERNALSYM szOID_RSA_hashedData}
  szOID_RSA_hashedData    = '1.2.840.113549.1.7.5';
  {$EXTERNALSYM szOID_RSA_encryptedData}
  szOID_RSA_encryptedData = '1.2.840.113549.1.7.6';

  {$EXTERNALSYM szOID_RSA_emailAddr}
  szOID_RSA_emailAddr     = '1.2.840.113549.1.9.1';
  {$EXTERNALSYM szOID_RSA_unstructName}
  szOID_RSA_unstructName  = '1.2.840.113549.1.9.2';
  {$EXTERNALSYM szOID_RSA_contentType}
  szOID_RSA_contentType   = '1.2.840.113549.1.9.3';
  {$EXTERNALSYM szOID_RSA_messageDigest}
  szOID_RSA_messageDigest = '1.2.840.113549.1.9.4';
  {$EXTERNALSYM szOID_RSA_signingTime}
  szOID_RSA_signingTime   = '1.2.840.113549.1.9.5';
  {$EXTERNALSYM szOID_RSA_counterSign}
  szOID_RSA_counterSign   = '1.2.840.113549.1.9.6';
  {$EXTERNALSYM szOID_RSA_challengePwd}
  szOID_RSA_challengePwd  = '1.2.840.113549.1.9.7';
  {$EXTERNALSYM szOID_RSA_unstructAddr}
  szOID_RSA_unstructAddr  = '1.2.840.113549.1.9.8';
  {$EXTERNALSYM szOID_RSA_extCertAttrs}
  szOID_RSA_extCertAttrs  = '1.2.840.113549.1.9.9';
  {$EXTERNALSYM szOID_RSA_certExtensions}
  szOID_RSA_certExtensions = '1.2.840.113549.1.9.14';
  {$EXTERNALSYM szOID_RSA_SMIMECapabilities}
  szOID_RSA_SMIMECapabilities = '1.2.840.113549.1.9.15';
  {$EXTERNALSYM szOID_RSA_preferSignedData}
  szOID_RSA_preferSignedData = '1.2.840.113549.1.9.15.1';

  {$EXTERNALSYM szOID_RSA_SMIMEalg}
  szOID_RSA_SMIMEalg              = '1.2.840.113549.1.9.16.3';
  {$EXTERNALSYM szOID_RSA_SMIMEalgESDH}
  szOID_RSA_SMIMEalgESDH          = '1.2.840.113549.1.9.16.3.5';
  {$EXTERNALSYM szOID_RSA_SMIMEalgCMS3DESwrap}
  szOID_RSA_SMIMEalgCMS3DESwrap   = '1.2.840.113549.1.9.16.3.6';
  {$EXTERNALSYM szOID_RSA_SMIMEalgCMSRC2wrap}
  szOID_RSA_SMIMEalgCMSRC2wrap    = '1.2.840.113549.1.9.16.3.7';

  {$EXTERNALSYM szOID_RSA_MD2}
  szOID_RSA_MD2           = '1.2.840.113549.2.2';
  {$EXTERNALSYM szOID_RSA_MD4}
  szOID_RSA_MD4           = '1.2.840.113549.2.4';
  {$EXTERNALSYM szOID_RSA_MD5}
  szOID_RSA_MD5           = '1.2.840.113549.2.5';

  {$EXTERNALSYM szOID_RSA_RC2CBC}
  szOID_RSA_RC2CBC        = '1.2.840.113549.3.2';
  {$EXTERNALSYM szOID_RSA_RC4}
  szOID_RSA_RC4           = '1.2.840.113549.3.4';
  {$EXTERNALSYM szOID_RSA_DES_EDE3_CBC}
  szOID_RSA_DES_EDE3_CBC  = '1.2.840.113549.3.7';
  {$EXTERNALSYM szOID_RSA_RC5_CBCPad}
  szOID_RSA_RC5_CBCPad    = '1.2.840.113549.3.9';

  {$EXTERNALSYM szOID_ANSI_X942}
  szOID_ANSI_X942         = '1.2.840.10046';
  {$EXTERNALSYM szOID_ANSI_X942_DH}
  szOID_ANSI_X942_DH      = '1.2.840.10046.2.1';

  {$EXTERNALSYM szOID_X957}
  szOID_X957              = '1.2.840.10040';
  {$EXTERNALSYM szOID_X957_DSA}
  szOID_X957_DSA          = '1.2.840.10040.4.1';
  {$EXTERNALSYM szOID_X957_SHA1DSA}
  szOID_X957_SHA1DSA      = '1.2.840.10040.4.3';

// iso(1) member-body(2) us(840) 10045 keyType(2) unrestricted(1)
  {$EXTERNALSYM szOID_ECC_PUBLIC_KEY}
  szOID_ECC_PUBLIC_KEY    = '1.2.840.10045.2.1';

// iso(1) member-body(2) us(840) 10045 curves(3) prime(1) 7
  {$EXTERNALSYM szOID_ECC_CURVE_P256}
  szOID_ECC_CURVE_P256    = '1.2.840.10045.3.1.7';

// iso(1) identified-organization(3) certicom(132) curve(0) 34
  {$EXTERNALSYM szOID_ECC_CURVE_P384}
  szOID_ECC_CURVE_P384    = '1.3.132.0.34';

// iso(1) identified-organization(3) certicom(132) curve(0) 35
  {$EXTERNALSYM szOID_ECC_CURVE_P521}
  szOID_ECC_CURVE_P521    = '1.3.132.0.35';

// iso(1) member-body(2) us(840) 10045 signatures(4) sha1(1)
  {$EXTERNALSYM szOID_ECDSA_SHA1}
  szOID_ECDSA_SHA1        = '1.2.840.10045.4.1';

// iso(1) member-body(2) us(840) 10045 signatures(4) specified(3)
  {$EXTERNALSYM szOID_ECDSA_SPECIFIED}
  szOID_ECDSA_SPECIFIED   = '1.2.840.10045.4.3';

// iso(1) member-body(2) us(840) 10045 signatures(4) specified(3) 2
  {$EXTERNALSYM szOID_ECDSA_SHA256}
  szOID_ECDSA_SHA256      = '1.2.840.10045.4.3.2';

// iso(1) member-body(2) us(840) 10045 signatures(4) specified(3) 3
  {$EXTERNALSYM szOID_ECDSA_SHA384}
  szOID_ECDSA_SHA384      = '1.2.840.10045.4.3.3';

// iso(1) member-body(2) us(840) 10045 signatures(4) specified(3) 4
  {$EXTERNALSYM szOID_ECDSA_SHA512}
  szOID_ECDSA_SHA512      = '1.2.840.10045.4.3.4';


// NIST AES CBC Algorithms
// joint-iso-itu-t(2) country(16) us(840) organization(1) gov(101) csor(3) nistAlgorithms(4)  aesAlgs(1) }

  {$EXTERNALSYM szOID_NIST_AES128_CBC}
  szOID_NIST_AES128_CBC        = '2.16.840.1.101.3.4.1.2';
  {$EXTERNALSYM szOID_NIST_AES192_CBC}
  szOID_NIST_AES192_CBC        = '2.16.840.1.101.3.4.1.22';
  {$EXTERNALSYM szOID_NIST_AES256_CBC}
  szOID_NIST_AES256_CBC        = '2.16.840.1.101.3.4.1.42';

// For the above Algorithms, the AlgorithmIdentifier parameters must be
// present and the parameters field MUST contain an AES-IV:
//
//  AES-IV ::= OCTET STRING (SIZE(16))

// NIST AES WRAP Algorithms
  {$EXTERNALSYM szOID_NIST_AES128_WRAP}
  szOID_NIST_AES128_WRAP       = '2.16.840.1.101.3.4.1.5';
  {$EXTERNALSYM szOID_NIST_AES192_WRAP}
  szOID_NIST_AES192_WRAP       = '2.16.840.1.101.3.4.1.25';
  {$EXTERNALSYM szOID_NIST_AES256_WRAP}
  szOID_NIST_AES256_WRAP       = '2.16.840.1.101.3.4.1.45';

//      x9-63-scheme OBJECT IDENTIFIER ::= { iso(1)
//         identified-organization(3) tc68(133) country(16) x9(840)
//         x9-63(63) schemes(0) }

// ECDH single pass ephemeral-static KeyAgreement KeyEncryptionAlgorithm
  {$EXTERNALSYM szOID_DH_SINGLE_PASS_STDDH_SHA1_KDF}
  szOID_DH_SINGLE_PASS_STDDH_SHA1_KDF = '1.3.133.16.840.63.0.2';

// For the above KeyEncryptionAlgorithm the following wrap algorithms are
// supported:
//  szOID_RSA_SMIMEalgCMS3DESwrap
//  szOID_RSA_SMIMEalgCMSRC2wrap
//  szOID_NIST_AES128_WRAP
//  szOID_NIST_AES192_WRAP
//  szOID_NIST_AES256_WRAP

// ITU-T UsefulDefinitions
  {$EXTERNALSYM szOID_DS}
  szOID_DS                = '2.5';
  {$EXTERNALSYM szOID_DSALG}
  szOID_DSALG             = '2.5.8';
  {$EXTERNALSYM szOID_DSALG_CRPT}
  szOID_DSALG_CRPT        = '2.5.8.1';
  {$EXTERNALSYM szOID_DSALG_HASH}
  szOID_DSALG_HASH        = '2.5.8.2';
  {$EXTERNALSYM szOID_DSALG_SIGN}
  szOID_DSALG_SIGN        = '2.5.8.3';
  {$EXTERNALSYM szOID_DSALG_RSA}
  szOID_DSALG_RSA         = '2.5.8.1.1';
// NIST OSE Implementors' Workshop (OIW)
// http://nemo.ncsl.nist.gov/oiw/agreements/stable/OSI/12s_9506.w51
// http://nemo.ncsl.nist.gov/oiw/agreements/working/OSI/12w_9503.w51
  {$EXTERNALSYM szOID_OIW}
  szOID_OIW               = '1.3.14';
// NIST OSE Implementors' Workshop (OIW) Security SIG algorithm identifiers
  {$EXTERNALSYM szOID_OIWSEC}
  szOID_OIWSEC            = '1.3.14.3.2';
  {$EXTERNALSYM szOID_OIWSEC_md4RSA}
  szOID_OIWSEC_md4RSA     = '1.3.14.3.2.2';
  {$EXTERNALSYM szOID_OIWSEC_md5RSA}
  szOID_OIWSEC_md5RSA     = '1.3.14.3.2.3';
  {$EXTERNALSYM szOID_OIWSEC_md4RSA2}
  szOID_OIWSEC_md4RSA2    = '1.3.14.3.2.4';
  {$EXTERNALSYM szOID_OIWSEC_desECB}
  szOID_OIWSEC_desECB     = '1.3.14.3.2.6';
  {$EXTERNALSYM szOID_OIWSEC_desCBC}
  szOID_OIWSEC_desCBC     = '1.3.14.3.2.7';
  {$EXTERNALSYM szOID_OIWSEC_desOFB}
  szOID_OIWSEC_desOFB     = '1.3.14.3.2.8';
  {$EXTERNALSYM szOID_OIWSEC_desCFB}
  szOID_OIWSEC_desCFB     = '1.3.14.3.2.9';
  {$EXTERNALSYM szOID_OIWSEC_desMAC}
  szOID_OIWSEC_desMAC     = '1.3.14.3.2.10';
  {$EXTERNALSYM szOID_OIWSEC_rsaSign}
  szOID_OIWSEC_rsaSign    = '1.3.14.3.2.11';
  {$EXTERNALSYM szOID_OIWSEC_dsa}
  szOID_OIWSEC_dsa        = '1.3.14.3.2.12';
  {$EXTERNALSYM szOID_OIWSEC_shaDSA}
  szOID_OIWSEC_shaDSA     = '1.3.14.3.2.13';
  {$EXTERNALSYM szOID_OIWSEC_mdc2RSA}
  szOID_OIWSEC_mdc2RSA    = '1.3.14.3.2.14';
  {$EXTERNALSYM szOID_OIWSEC_shaRSA}
  szOID_OIWSEC_shaRSA     = '1.3.14.3.2.15';
  {$EXTERNALSYM szOID_OIWSEC_dhCommMod}
  szOID_OIWSEC_dhCommMod  = '1.3.14.3.2.16';
  {$EXTERNALSYM szOID_OIWSEC_desEDE}
  szOID_OIWSEC_desEDE     = '1.3.14.3.2.17';
  {$EXTERNALSYM szOID_OIWSEC_sha}
  szOID_OIWSEC_sha        = '1.3.14.3.2.18';
  {$EXTERNALSYM szOID_OIWSEC_mdc2}
  szOID_OIWSEC_mdc2       = '1.3.14.3.2.19';
  {$EXTERNALSYM szOID_OIWSEC_dsaComm}
  szOID_OIWSEC_dsaComm    = '1.3.14.3.2.20';
  {$EXTERNALSYM szOID_OIWSEC_dsaCommSHA}
  szOID_OIWSEC_dsaCommSHA = '1.3.14.3.2.21';
  {$EXTERNALSYM szOID_OIWSEC_rsaXchg}
  szOID_OIWSEC_rsaXchg    = '1.3.14.3.2.22';
  {$EXTERNALSYM szOID_OIWSEC_keyHashSeal}
  szOID_OIWSEC_keyHashSeal = '1.3.14.3.2.23';
  {$EXTERNALSYM szOID_OIWSEC_md2RSASign}
  szOID_OIWSEC_md2RSASign = '1.3.14.3.2.24';
  {$EXTERNALSYM szOID_OIWSEC_md5RSASign}
  szOID_OIWSEC_md5RSASign = '1.3.14.3.2.25';
  {$EXTERNALSYM szOID_OIWSEC_sha1}
  szOID_OIWSEC_sha1       = '1.3.14.3.2.26';
  {$EXTERNALSYM szOID_OIWSEC_dsaSHA1}
  szOID_OIWSEC_dsaSHA1    = '1.3.14.3.2.27';
  {$EXTERNALSYM szOID_OIWSEC_dsaCommSHA1}
  szOID_OIWSEC_dsaCommSHA1 = '1.3.14.3.2.28';
  {$EXTERNALSYM szOID_OIWSEC_sha1RSASign}
  szOID_OIWSEC_sha1RSASign = '1.3.14.3.2.29';
// NIST OSE Implementors' Workshop (OIW) Directory SIG algorithm identifiers
  {$EXTERNALSYM szOID_OIWDIR}
  szOID_OIWDIR            = '1.3.14.7.2';
  {$EXTERNALSYM szOID_OIWDIR_CRPT}
  szOID_OIWDIR_CRPT       = '1.3.14.7.2.1';
  {$EXTERNALSYM szOID_OIWDIR_HASH}
  szOID_OIWDIR_HASH       = '1.3.14.7.2.2';
  {$EXTERNALSYM szOID_OIWDIR_SIGN}
  szOID_OIWDIR_SIGN       = '1.3.14.7.2.3';
  {$EXTERNALSYM szOID_OIWDIR_md2}
  szOID_OIWDIR_md2        = '1.3.14.7.2.2.1';
  {$EXTERNALSYM szOID_OIWDIR_md2RSA}
  szOID_OIWDIR_md2RSA     = '1.3.14.7.2.3.1';

// INFOSEC Algorithms
// joint-iso-ccitt(2) country(16) us(840) organization(1) us-government(101) dod(2) id-infosec(1)
  {$EXTERNALSYM szOID_INFOSEC}
  szOID_INFOSEC                       = '2.16.840.1.101.2.1';
  {$EXTERNALSYM szOID_INFOSEC_sdnsSignature}
  szOID_INFOSEC_sdnsSignature         = '2.16.840.1.101.2.1.1.1';
  {$EXTERNALSYM szOID_INFOSEC_mosaicSignature}
  szOID_INFOSEC_mosaicSignature       = '2.16.840.1.101.2.1.1.2';
  {$EXTERNALSYM szOID_INFOSEC_sdnsConfidentiality}
  szOID_INFOSEC_sdnsConfidentiality   = '2.16.840.1.101.2.1.1.3';
  {$EXTERNALSYM szOID_INFOSEC_mosaicConfidentiality}
  szOID_INFOSEC_mosaicConfidentiality = '2.16.840.1.101.2.1.1.4';
  {$EXTERNALSYM szOID_INFOSEC_sdnsIntegrity}
  szOID_INFOSEC_sdnsIntegrity         = '2.16.840.1.101.2.1.1.5';
  {$EXTERNALSYM szOID_INFOSEC_mosaicIntegrity}
  szOID_INFOSEC_mosaicIntegrity       = '2.16.840.1.101.2.1.1.6';
  {$EXTERNALSYM szOID_INFOSEC_sdnsTokenProtection}
  szOID_INFOSEC_sdnsTokenProtection   = '2.16.840.1.101.2.1.1.7';
  {$EXTERNALSYM szOID_INFOSEC_mosaicTokenProtection}
  szOID_INFOSEC_mosaicTokenProtection = '2.16.840.1.101.2.1.1.8';
  {$EXTERNALSYM szOID_INFOSEC_sdnsKeyManagement}
  szOID_INFOSEC_sdnsKeyManagement     = '2.16.840.1.101.2.1.1.9';
  {$EXTERNALSYM szOID_INFOSEC_mosaicKeyManagement}
  szOID_INFOSEC_mosaicKeyManagement   = '2.16.840.1.101.2.1.1.10';
  {$EXTERNALSYM szOID_INFOSEC_sdnsKMandSig}
  szOID_INFOSEC_sdnsKMandSig          = '2.16.840.1.101.2.1.1.11';
  {$EXTERNALSYM szOID_INFOSEC_mosaicKMandSig}
  szOID_INFOSEC_mosaicKMandSig        = '2.16.840.1.101.2.1.1.12';
  {$EXTERNALSYM szOID_INFOSEC_SuiteASignature}
  szOID_INFOSEC_SuiteASignature       = '2.16.840.1.101.2.1.1.13';
  {$EXTERNALSYM szOID_INFOSEC_SuiteAConfidentiality}
  szOID_INFOSEC_SuiteAConfidentiality = '2.16.840.1.101.2.1.1.14';
  {$EXTERNALSYM szOID_INFOSEC_SuiteAIntegrity}
  szOID_INFOSEC_SuiteAIntegrity       = '2.16.840.1.101.2.1.1.15';
  {$EXTERNALSYM szOID_INFOSEC_SuiteATokenProtection}
  szOID_INFOSEC_SuiteATokenProtection = '2.16.840.1.101.2.1.1.16';
  {$EXTERNALSYM szOID_INFOSEC_SuiteAKeyManagement}
  szOID_INFOSEC_SuiteAKeyManagement   = '2.16.840.1.101.2.1.1.17';
  {$EXTERNALSYM szOID_INFOSEC_SuiteAKMandSig}
  szOID_INFOSEC_SuiteAKMandSig        = '2.16.840.1.101.2.1.1.18';
  {$EXTERNALSYM szOID_INFOSEC_mosaicUpdatedSig}
  szOID_INFOSEC_mosaicUpdatedSig      = '2.16.840.1.101.2.1.1.19';
  {$EXTERNALSYM szOID_INFOSEC_mosaicKMandUpdSig}
  szOID_INFOSEC_mosaicKMandUpdSig     = '2.16.840.1.101.2.1.1.20';
  {$EXTERNALSYM szOID_INFOSEC_mosaicUpdatedInteg}
  szOID_INFOSEC_mosaicUpdatedInteg    = '2.16.840.1.101.2.1.1.21';

// NIST Hash Algorithms
// joint-iso-itu-t(2) country(16) us(840) organization(1) gov(101) csor(3) nistalgorithm(4) hashalgs(2)

  {$EXTERNALSYM szOID_NIST_sha256}
  szOID_NIST_sha256                   = '2.16.840.1.101.3.4.2.1';
  {$EXTERNALSYM szOID_NIST_sha384}
  szOID_NIST_sha384                   = '2.16.840.1.101.3.4.2.2';
  {$EXTERNALSYM szOID_NIST_sha512}
  szOID_NIST_sha512                   = '2.16.840.1.101.3.4.2.3';

type  
  PCryptObjIDTable = ^TCryptObjIDTable;
  {$EXTERNALSYM _CRYPT_OBJID_TABLE}
  _CRYPT_OBJID_TABLE = record
    dwAlgId: DWORD;
    pszObjId: LPCSTR;
  end;
  {$EXTERNALSYM CRYPT_OBJID_TABLE}
  CRYPT_OBJID_TABLE = _CRYPT_OBJID_TABLE;
  {$EXTERNALSYM PCRYPT_OBJID_TABLE}
  PCRYPT_OBJID_TABLE = ^_CRYPT_OBJID_TABLE;
  TCryptObjIDTable = _CRYPT_OBJID_TABLE;


//+-------------------------------------------------------------------------
//  PKCS #1 HashInfo (DigestInfo)
//--------------------------------------------------------------------------
  PCryptHashInfo = ^TCryptHashInfo;
  {$EXTERNALSYM _CRYPT_HASH_INFO}
  _CRYPT_HASH_INFO = record
    HashAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    Hash: CRYPT_HASH_BLOB;
  end;
  {$EXTERNALSYM CRYPT_HASH_INFO}
  CRYPT_HASH_INFO = _CRYPT_HASH_INFO;
  {$EXTERNALSYM PCRYPT_HASH_INFO}
  PCRYPT_HASH_INFO = ^_CRYPT_HASH_INFO;
  TCryptHashInfo = _CRYPT_HASH_INFO;

//+-------------------------------------------------------------------------
//  Type used for an extension to an encoded content
//
//  Where the Value's CRYPT_OBJID_BLOB is in its encoded representation.
//--------------------------------------------------------------------------
// certenrolls_begin -- CERT_CONTEXT
  PCertExtension = ^TCertExtension;
  {$EXTERNALSYM _CERT_EXTENSION}
  _CERT_EXTENSION = record
    pszObjId: LPSTR;
    fCritical: BOOL;
    Value: CRYPT_OBJID_BLOB;
  end;
  {$EXTERNALSYM CERT_EXTENSION}
  CERT_EXTENSION = _CERT_EXTENSION;
  {$EXTERNALSYM PCERT_EXTENSION}
  PCERT_EXTENSION = ^_CERT_EXTENSION;
  TCertExtension = _CERT_EXTENSION;
// certenrolls_end

//+-------------------------------------------------------------------------
//  AttributeTypeValue
//
//  Where the Value's CRYPT_OBJID_BLOB is in its encoded representation.
//--------------------------------------------------------------------------
// certenrolls_begin -- CRYPT_ATTRIBUTE_TYPE_VALUE
  PCryptAttributeTypeValue = ^TCryptAttributeTypeValue;
  {$EXTERNALSYM _CRYPT_ATTRIBUTE_TYPE_VALUE}
  _CRYPT_ATTRIBUTE_TYPE_VALUE = record
    pszObjId: LPSTR;
    Value: CRYPT_OBJID_BLOB;
  end;
  {$EXTERNALSYM CRYPT_ATTRIBUTE_TYPE_VALUE}
  CRYPT_ATTRIBUTE_TYPE_VALUE = _CRYPT_ATTRIBUTE_TYPE_VALUE;
  {$EXTERNALSYM PCRYPT_ATTRIBUTE_TYPE_VALUE}
  PCRYPT_ATTRIBUTE_TYPE_VALUE = ^_CRYPT_ATTRIBUTE_TYPE_VALUE;
  TCryptAttributeTypeValue = _CRYPT_ATTRIBUTE_TYPE_VALUE;
// certenrolls_end

//+-------------------------------------------------------------------------
//  Attributes
//
//  Where the Value's PATTR_BLOBs are in their encoded representation.
//--------------------------------------------------------------------------
// certenrolls_begin -- CRYPT_ATTRIBUTE
  PCryptAttribute = ^TCryptAttribute;
  {$EXTERNALSYM _CRYPT_ATTRIBUTE}
  _CRYPT_ATTRIBUTE = record
    pszObjId: LPSTR;
    cValue: DWORD;
    rgValue: PCryptAttrBlob;
  end;
  {$EXTERNALSYM CRYPT_ATTRIBUTE}
  CRYPT_ATTRIBUTE = _CRYPT_ATTRIBUTE;
  {$EXTERNALSYM PCRYPT_ATTRIBUTE}
  PCRYPT_ATTRIBUTE = ^_CRYPT_ATTRIBUTE;
  TCryptAttribute = _CRYPT_ATTRIBUTE;

  PCryptAttributes = ^TCryptAttributes;
  {$EXTERNALSYM _CRYPT_ATTRIBUTES}
  _CRYPT_ATTRIBUTES = record
    cAttr: DWORD;
    rgAttr: PCryptAttribute;
  end;
  {$EXTERNALSYM CRYPT_ATTRIBUTES}
  CRYPT_ATTRIBUTES = _CRYPT_ATTRIBUTES;
  {$EXTERNALSYM PCRYPT_ATTRIBUTES}
  PCRYPT_ATTRIBUTES = ^_CRYPT_ATTRIBUTES;
  TCryptAttributes = _CRYPT_ATTRIBUTES;
// certenrolls_end

//+-------------------------------------------------------------------------
//  Attributes making up a Relative Distinguished Name (CERT_RDN)
//
//  The interpretation of the Value depends on the dwValueType.
//  See below for a list of the types.
//--------------------------------------------------------------------------
  PCertRDNAttr = ^TCertRDNAttr;
  {$EXTERNALSYM _CERT_RDN_ATTR}
  _CERT_RDN_ATTR = record
    pszObjId: LPSTR;
    dwValueType: DWORD;
    Value: CERT_RDN_VALUE_BLOB;
  end;
  {$EXTERNALSYM CERT_RDN_ATTR}
  CERT_RDN_ATTR = _CERT_RDN_ATTR;
  {$EXTERNALSYM PCERT_RDN_ATTR}
  PCERT_RDN_ATTR = ^_CERT_RDN_ATTR;
  TCertRDNAttr = _CERT_RDN_ATTR;

//+-------------------------------------------------------------------------
//  CERT_RDN attribute Object Identifiers
//--------------------------------------------------------------------------
// Labeling attribute types:
const
  {$EXTERNALSYM szOID_COMMON_NAME}
  szOID_COMMON_NAME                   = '2.5.4.3';  // case-ignore string
  {$EXTERNALSYM szOID_SUR_NAME}
  szOID_SUR_NAME                      = '2.5.4.4';  // case-ignore string
  {$EXTERNALSYM szOID_DEVICE_SERIAL_NUMBER}
  szOID_DEVICE_SERIAL_NUMBER          = '2.5.4.5';  // printable string

// Geographic attribute types:
  {$EXTERNALSYM szOID_COUNTRY_NAME}
  szOID_COUNTRY_NAME                  = '2.5.4.6';  // printable 2char string
  {$EXTERNALSYM szOID_LOCALITY_NAME}
  szOID_LOCALITY_NAME                 = '2.5.4.7';  // case-ignore string
  {$EXTERNALSYM szOID_STATE_OR_PROVINCE_NAME}
  szOID_STATE_OR_PROVINCE_NAME        = '2.5.4.8';  // case-ignore string
  {$EXTERNALSYM szOID_STREET_ADDRESS}
  szOID_STREET_ADDRESS                = '2.5.4.9';  // case-ignore string

// Organizational attribute types:
  {$EXTERNALSYM szOID_ORGANIZATION_NAME}
  szOID_ORGANIZATION_NAME             = '2.5.4.10'; // case-ignore string
  {$EXTERNALSYM szOID_ORGANIZATIONAL_UNIT_NAME}
  szOID_ORGANIZATIONAL_UNIT_NAME      = '2.5.4.11'; // case-ignore string
  {$EXTERNALSYM szOID_TITLE}
  szOID_TITLE                         = '2.5.4.12'; // case-ignore string

// Explanatory attribute types:
  {$EXTERNALSYM szOID_DESCRIPTION}
  szOID_DESCRIPTION                   = '2.5.4.13'; // case-ignore string
  {$EXTERNALSYM szOID_SEARCH_GUIDE}
  szOID_SEARCH_GUIDE                  = '2.5.4.14';
  {$EXTERNALSYM szOID_BUSINESS_CATEGORY}
  szOID_BUSINESS_CATEGORY             = '2.5.4.15'; // case-ignore string

// Postal addressing attribute types:
  {$EXTERNALSYM szOID_POSTAL_ADDRESS}
  szOID_POSTAL_ADDRESS                = '2.5.4.16';
  {$EXTERNALSYM szOID_POSTAL_CODE}
  szOID_POSTAL_CODE                   = '2.5.4.17'; // case-ignore string
  {$EXTERNALSYM szOID_POST_OFFICE_BOX}
  szOID_POST_OFFICE_BOX               = '2.5.4.18'; // case-ignore string
  {$EXTERNALSYM szOID_PHYSICAL_DELIVERY_OFFICE_NAME}
  szOID_PHYSICAL_DELIVERY_OFFICE_NAME = '2.5.4.19'; // case-ignore string

// Telecommunications addressing attribute types:
  {$EXTERNALSYM szOID_TELEPHONE_NUMBER}
  szOID_TELEPHONE_NUMBER              = '2.5.4.20'; // telephone number
  {$EXTERNALSYM szOID_TELEX_NUMBER}
  szOID_TELEX_NUMBER                  = '2.5.4.21';
  {$EXTERNALSYM szOID_TELETEXT_TERMINAL_IDENTIFIER}
  szOID_TELETEXT_TERMINAL_IDENTIFIER  = '2.5.4.22';
  {$EXTERNALSYM szOID_FACSIMILE_TELEPHONE_NUMBER}
  szOID_FACSIMILE_TELEPHONE_NUMBER    = '2.5.4.23';
  {$EXTERNALSYM szOID_X21_ADDRESS}
  szOID_X21_ADDRESS                   = '2.5.4.24'; // numeric string
  {$EXTERNALSYM szOID_INTERNATIONAL_ISDN_NUMBER}
  szOID_INTERNATIONAL_ISDN_NUMBER     = '2.5.4.25'; // numeric string
  {$EXTERNALSYM szOID_REGISTERED_ADDRESS}
  szOID_REGISTERED_ADDRESS            = '2.5.4.26';
  {$EXTERNALSYM szOID_DESTINATION_INDICATOR}
  szOID_DESTINATION_INDICATOR         = '2.5.4.27'; // printable string

// Preference attribute types:
  {$EXTERNALSYM szOID_PREFERRED_DELIVERY_METHOD}
  szOID_PREFERRED_DELIVERY_METHOD     = '2.5.4.28';

// OSI application attribute types:
  {$EXTERNALSYM szOID_PRESENTATION_ADDRESS}
  szOID_PRESENTATION_ADDRESS          = '2.5.4.29';
  {$EXTERNALSYM szOID_SUPPORTED_APPLICATION_CONTEXT}
  szOID_SUPPORTED_APPLICATION_CONTEXT = '2.5.4.30';

// Relational application attribute types:
  {$EXTERNALSYM szOID_MEMBER}
  szOID_MEMBER                        = '2.5.4.31';
  {$EXTERNALSYM szOID_OWNER}
  szOID_OWNER                         = '2.5.4.32';
  {$EXTERNALSYM szOID_ROLE_OCCUPANT}
  szOID_ROLE_OCCUPANT                 = '2.5.4.33';
  {$EXTERNALSYM szOID_SEE_ALSO}
  szOID_SEE_ALSO                      = '2.5.4.34';

// Security attribute types:
  {$EXTERNALSYM szOID_USER_PASSWORD}
  szOID_USER_PASSWORD                 = '2.5.4.35';
  {$EXTERNALSYM szOID_USER_CERTIFICATE}
  szOID_USER_CERTIFICATE              = '2.5.4.36';
  {$EXTERNALSYM szOID_CA_CERTIFICATE}
  szOID_CA_CERTIFICATE                = '2.5.4.37';
  {$EXTERNALSYM szOID_AUTHORITY_REVOCATION_LIST}
  szOID_AUTHORITY_REVOCATION_LIST     = '2.5.4.38';
  {$EXTERNALSYM szOID_CERTIFICATE_REVOCATION_LIST}
  szOID_CERTIFICATE_REVOCATION_LIST   = '2.5.4.39';
  {$EXTERNALSYM szOID_CROSS_CERTIFICATE_PAIR}
  szOID_CROSS_CERTIFICATE_PAIR        = '2.5.4.40';

// Undocumented attribute types???
//  szOID_???                         = '2.5.4.41';
  {$EXTERNALSYM szOID_GIVEN_NAME}
  szOID_GIVEN_NAME                    = '2.5.4.42'; // case-ignore string
  {$EXTERNALSYM szOID_INITIALS}
  szOID_INITIALS                      = '2.5.4.43'; // case-ignore string

// The DN Qualifier attribute type specifies disambiguating information to add
// to the relative distinguished name of an entry. It is intended to be used
// for entries held in multiple DSAs which would otherwise have the same name,
// and that its value be the same in a given DSA for all entries to which
// the information has been added.
  {$EXTERNALSYM szOID_DN_QUALIFIER}
  szOID_DN_QUALIFIER                  = '2.5.4.46';

// Pilot user attribute types:
  {$EXTERNALSYM szOID_DOMAIN_COMPONENT}
  szOID_DOMAIN_COMPONENT  = '0.9.2342.19200300.100.1.25'; // IA5, UTF8 string

// used for PKCS 12 attributes
  {$EXTERNALSYM szOID_PKCS_12_FRIENDLY_NAME_ATTR}
  szOID_PKCS_12_FRIENDLY_NAME_ATTR     = '1.2.840.113549.1.9.20';
  {$EXTERNALSYM szOID_PKCS_12_LOCAL_KEY_ID}
  szOID_PKCS_12_LOCAL_KEY_ID           = '1.2.840.113549.1.9.21';
  {$EXTERNALSYM szOID_PKCS_12_KEY_PROVIDER_NAME_ATTR}
  szOID_PKCS_12_KEY_PROVIDER_NAME_ATTR = '1.3.6.1.4.1.311.17.1';
  {$EXTERNALSYM szOID_LOCAL_MACHINE_KEYSET}
  szOID_LOCAL_MACHINE_KEYSET           = '1.3.6.1.4.1.311.17.2';
  {$EXTERNALSYM szOID_PKCS_12_EXTENDED_ATTRIBUTES}
  szOID_PKCS_12_EXTENDED_ATTRIBUTES    = '1.3.6.1.4.1.311.17.3';

//+-------------------------------------------------------------------------
//  Microsoft CERT_RDN attribute Object Identifiers
//--------------------------------------------------------------------------
// Special RDN containing the KEY_ID. Its value type is CERT_RDN_OCTET_STRING.
  {$EXTERNALSYM szOID_KEYID_RDN}
  szOID_KEYID_RDN                     = '1.3.6.1.4.1.311.10.7.1';

//+-------------------------------------------------------------------------
//  CERT_RDN Attribute Value Types
//
//  For RDN_ENCODED_BLOB, the Value's CERT_RDN_VALUE_BLOB is in its encoded
//  representation. Otherwise, its an array of bytes.
//
//  For all CERT_RDN types, Value.cbData is always the number of bytes, not
//  necessarily the number of elements in the string. For instance,
//  RDN_UNIVERSAL_STRING is an array of ints (cbData == intCnt * 4) and
//  RDN_BMP_STRING is an array of unsigned shorts (cbData == ushortCnt * 2).
//
//  A RDN_UTF8_STRING is an array of UNICODE characters (cbData == charCnt *2).
//  These UNICODE characters are encoded as UTF8 8 bit characters.
//
//  For CertDecodeName, two 0 bytes are always appended to the end of the
//  string (ensures a CHAR or WCHAR string is null terminated).
//  These added 0 bytes are't included in the BLOB.cbData.
//--------------------------------------------------------------------------
  {$EXTERNALSYM CERT_RDN_ANY_TYPE}
  CERT_RDN_ANY_TYPE = 0;
  {$EXTERNALSYM CERT_RDN_ENCODED_BLOB}
  CERT_RDN_ENCODED_BLOB = 1;
  {$EXTERNALSYM CERT_RDN_OCTET_STRING}
  CERT_RDN_OCTET_STRING = 2;
  {$EXTERNALSYM CERT_RDN_NUMERIC_STRING}
  CERT_RDN_NUMERIC_STRING = 3;
  {$EXTERNALSYM CERT_RDN_PRINTABLE_STRING}
  CERT_RDN_PRINTABLE_STRING = 4;
  {$EXTERNALSYM CERT_RDN_TELETEX_STRING}
  CERT_RDN_TELETEX_STRING = 5;
  {$EXTERNALSYM CERT_RDN_T61_STRING}
  CERT_RDN_T61_STRING = 5;
  {$EXTERNALSYM CERT_RDN_VIDEOTEX_STRING}
  CERT_RDN_VIDEOTEX_STRING = 6;
  {$EXTERNALSYM CERT_RDN_IA5_STRING}
  CERT_RDN_IA5_STRING = 7;
  {$EXTERNALSYM CERT_RDN_GRAPHIC_STRING}
  CERT_RDN_GRAPHIC_STRING = 8;
  {$EXTERNALSYM CERT_RDN_VISIBLE_STRING}
  CERT_RDN_VISIBLE_STRING = 9;
  {$EXTERNALSYM CERT_RDN_ISO646_STRING}
  CERT_RDN_ISO646_STRING = 9;
  {$EXTERNALSYM CERT_RDN_GENERAL_STRING}
  CERT_RDN_GENERAL_STRING = 10;
  {$EXTERNALSYM CERT_RDN_UNIVERSAL_STRING}
  CERT_RDN_UNIVERSAL_STRING = 11;
  {$EXTERNALSYM CERT_RDN_INT4_STRING}
  CERT_RDN_INT4_STRING = 11;
  {$EXTERNALSYM CERT_RDN_BMP_STRING}
  CERT_RDN_BMP_STRING = 12;
  {$EXTERNALSYM CERT_RDN_UNICODE_STRING}
  CERT_RDN_UNICODE_STRING = 12;
  {$EXTERNALSYM CERT_RDN_UTF8_STRING}
  CERT_RDN_UTF8_STRING = 13;

  {$EXTERNALSYM CERT_RDN_TYPE_MASK}
  CERT_RDN_TYPE_MASK = $000000FF;
  {$EXTERNALSYM CERT_RDN_FLAGS_MASK}
  CERT_RDN_FLAGS_MASK = $FF000000;

//+-------------------------------------------------------------------------
//  Flags that can be or'ed with the above Value Type when encoding/decoding
//--------------------------------------------------------------------------
// For encoding: when set, CERT_RDN_T61_STRING is selected instead of
// CERT_RDN_UNICODE_STRING if all the unicode characters are <= $FF
  {$EXTERNALSYM CERT_RDN_ENABLE_T61_UNICODE_FLAG}
  CERT_RDN_ENABLE_T61_UNICODE_FLAG = $80000000;

// For encoding: when set, CERT_RDN_UTF8_STRING is selected instead of
// CERT_RDN_UNICODE_STRING.
  {$EXTERNALSYM CERT_RDN_ENABLE_UTF8_UNICODE_FLAG}
  CERT_RDN_ENABLE_UTF8_UNICODE_FLAG = $20000000;

// For encoding: when set, CERT_RDN_UTF8_STRING is selected instead of
// CERT_RDN_PRINTABLE_STRING for DirectoryString types. Also,
// enables CERT_RDN_ENABLE_UTF8_UNICODE_FLAG.
  {$EXTERNALSYM CERT_RDN_FORCE_UTF8_UNICODE_FLAG}
  CERT_RDN_FORCE_UTF8_UNICODE_FLAG = $10000000;

// For encoding: when set, the characters aren't checked to see if they
// are valid for the Value Type.
  {$EXTERNALSYM CERT_RDN_DISABLE_CHECK_TYPE_FLAG}
  CERT_RDN_DISABLE_CHECK_TYPE_FLAG = $40000000;

// For decoding: by default, CERT_RDN_T61_STRING values are initially decoded
// as UTF8. If the UTF8 decoding fails, then, decoded as 8 bit characters.
// Setting this flag skips the initial attempt to decode as UTF8.
  {$EXTERNALSYM CERT_RDN_DISABLE_IE4_UTF8_FLAG}
  CERT_RDN_DISABLE_IE4_UTF8_FLAG = $01000000;

// Macro to check that the dwValueType is a character string and not an
// encoded blob or octet string
{$EXTERNALSYM IS_CERT_RDN_CHAR_STRING}
function IS_CERT_RDN_CHAR_STRING(X: DWORD): Boolean; {$IfDef HAS_INLINE}inline;{$EndIf}

type
//+-------------------------------------------------------------------------
//  A CERT_RDN consists of an array of the above attributes
//--------------------------------------------------------------------------
  PCertRDN = ^TCertRDN;
  {$EXTERNALSYM _CERT_RDN}
  _CERT_RDN = record
    cRDNAttr: DWORD;
    rgRDNAttr: ^CERT_RDN_ATTR;
  end;
  {$EXTERNALSYM CERT_RDN}
  CERT_RDN = _CERT_RDN;
  {$EXTERNALSYM PCERT_RDN}
  PCERT_RDN = ^_CERT_RDN;
  TCertRDN = _CERT_RDN;

//+-------------------------------------------------------------------------
//  Information stored in a subject's or issuer's name. The information
//  is represented as an array of the above RDNs.
//--------------------------------------------------------------------------
  PCertNameInfo = ^TCertNameInfo;
  {$EXTERNALSYM _CERT_NAME_INFO}
  _CERT_NAME_INFO = record
    cRDN: DWORD;
    rgRDN: ^CERT_RDN;
  end;
  {$EXTERNALSYM CERT_NAME_INFO}
  CERT_NAME_INFO = _CERT_NAME_INFO;
  {$EXTERNALSYM PCERT_NAME_INFO}
  PCERT_NAME_INFO = ^_CERT_NAME_INFO;
  TCertNameInfo = _CERT_NAME_INFO;

//+-------------------------------------------------------------------------
//  Name attribute value without the Object Identifier
//
//  The interpretation of the Value depends on the dwValueType.
//  See above for a list of the types.
//--------------------------------------------------------------------------
  PCertNameValue = ^TCertNameValue;
  {$EXTERNALSYM _CERT_NAME_VALUE}
  _CERT_NAME_VALUE = record
    dwValueType: DWORD;
    Value: CERT_RDN_VALUE_BLOB;
  end;
  {$EXTERNALSYM CERT_NAME_VALUE}
  CERT_NAME_VALUE = _CERT_NAME_VALUE;
  {$EXTERNALSYM PCERT_NAME_VALUE}
  PCERT_NAME_VALUE = ^_CERT_NAME_VALUE;
  TCertNameValue = _CERT_NAME_VALUE;

//+-------------------------------------------------------------------------
//  Public Key Info
//
//  The PublicKey is the encoded representation of the information as it is
//  stored in the bit string
//--------------------------------------------------------------------------
// certenrolls_begin -- CERT_CONTEXT
  PCertPublicKeyInfo = ^TCertPublicKeyInfo;
  {$EXTERNALSYM _CERT_PUBLIC_KEY_INFO}
  _CERT_PUBLIC_KEY_INFO = record
    Algorithm: CRYPT_ALGORITHM_IDENTIFIER;
    PublicKey: CRYPT_BIT_BLOB;
  end;
  {$EXTERNALSYM CERT_PUBLIC_KEY_INFO}
  CERT_PUBLIC_KEY_INFO = _CERT_PUBLIC_KEY_INFO;
  {$EXTERNALSYM PCERT_PUBLIC_KEY_INFO}
  PCERT_PUBLIC_KEY_INFO = ^_CERT_PUBLIC_KEY_INFO;
  TCertPublicKeyInfo = _CERT_PUBLIC_KEY_INFO;
// certenrolls_end

const
{$EXTERNALSYM CERT_RSA_PUBLIC_KEY_OBJID}
  CERT_RSA_PUBLIC_KEY_OBJID            = szOID_RSA_RSA;
{$EXTERNALSYM CERT_DEFAULT_OID_PUBLIC_KEY_SIGN}
  CERT_DEFAULT_OID_PUBLIC_KEY_SIGN     = szOID_RSA_RSA;
{$EXTERNALSYM CERT_DEFAULT_OID_PUBLIC_KEY_XCHG}
  CERT_DEFAULT_OID_PUBLIC_KEY_XCHG     = szOID_RSA_RSA;

type
//+-------------------------------------------------------------------------
//  structure that contains all the information in a PKCS#8 PrivateKeyInfo
//--------------------------------------------------------------------------
  PCryptPrivateKeyInfo = ^TCryptPrivateKeyInfo;
  {$EXTERNALSYM _CRYPT_PRIVATE_KEY_INFO}
  _CRYPT_PRIVATE_KEY_INFO = record
    Version: DWORD;
    Algorithm: CRYPT_ALGORITHM_IDENTIFIER;
    PrivateKey: CRYPT_DER_BLOB;
    pAttributes: PCryptAttributes;
  end;
  {$EXTERNALSYM CRYPT_PRIVATE_KEY_INFO}
  CRYPT_PRIVATE_KEY_INFO = _CRYPT_PRIVATE_KEY_INFO;
  {$EXTERNALSYM PCRYPT_PRIVATE_KEY_INFO}
  PCRYPT_PRIVATE_KEY_INFO = ^_CRYPT_PRIVATE_KEY_INFO;
  TCryptPrivateKeyInfo = _CRYPT_PRIVATE_KEY_INFO;

//+-------------------------------------------------------------------------
//  structure that contains all the information in a PKCS#8
//  EncryptedPrivateKeyInfo
//--------------------------------------------------------------------------
  PCryptEncryptedPrivateKeyInfo = ^TCryptEncryptedPrivateKeyInfo;
  {$EXTERNALSYM _CRYPT_ENCRYPTED_PRIVATE_KEY_INFO}
  _CRYPT_ENCRYPTED_PRIVATE_KEY_INFO = record
    EncryptionAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    EncryptedPrivateKey: CRYPT_DATA_BLOB;
  end;
  {$EXTERNALSYM CRYPT_ENCRYPTED_PRIVATE_KEY_INFO}
  CRYPT_ENCRYPTED_PRIVATE_KEY_INFO = _CRYPT_ENCRYPTED_PRIVATE_KEY_INFO;
  {$EXTERNALSYM PCRYPT_ENCRYPTED_PRIVATE_KEY_INFO}
  PCRYPT_ENCRYPTED_PRIVATE_KEY_INFO = ^_CRYPT_ENCRYPTED_PRIVATE_KEY_INFO;
  TCryptEncryptedPrivateKeyInfo = _CRYPT_ENCRYPTED_PRIVATE_KEY_INFO;

//+-------------------------------------------------------------------------
// this callback is given when an EncryptedProvateKeyInfo structure is
// encountered during ImportPKCS8.  the caller is then expected to decrypt
// the private key and hand back the decrypted contents.
//
// the parameters are:
// Algorithm - the algorithm used to encrypt the PrivateKeyInfo
// EncryptedPrivateKey - the encrypted private key blob
// pClearTextKey - a buffer to receive the clear text
// cbClearTextKey - the number of bytes of the pClearTextKey buffer
//                  note the if this is zero then this should be
//                  filled in with the size required to decrypt the
//                  key into, and pClearTextKey should be ignored
// pVoidDecryptFunc - this is the pVoid that was passed into the call
//                    and is preserved and passed back as context
//+-------------------------------------------------------------------------

  {$EXTERNALSYM PCRYPT_DECRYPT_PRIVATE_KEY_FUNC}
  PCRYPT_DECRYPT_PRIVATE_KEY_FUNC = function(
    Algorithm: CRYPT_ALGORITHM_IDENTIFIER;
    EncryptedPrivateKey: CRYPT_DATA_BLOB; pbClearTextKey: PBYTE;
    out pcbClearTextKey: DWORD; pVoidDecryptFunc: Pointer): BOOL stdcall;
  TCryptDecryptPrivateKeyFunc = PCRYPT_DECRYPT_PRIVATE_KEY_FUNC;

//+-------------------------------------------------------------------------
// this callback is given when creating a PKCS8 EncryptedPrivateKeyInfo.
// The caller is then expected to encrypt the private key and hand back
// the encrypted contents.
//
// the parameters are:
// Algorithm - the algorithm used to encrypt the PrivateKeyInfo
// pClearTextPrivateKey - the cleartext private key to be encrypted
// pbEncryptedKey - the output encrypted private key blob
// cbEncryptedKey - the number of bytes of the pbEncryptedKey buffer
//                  note the if this is zero then this should be
//                  filled in with the size required to encrypt the
//                  key into, and pbEncryptedKey should be ignored
// pVoidEncryptFunc - this is the pVoid that was passed into the call
//                    and is preserved and passed back as context
//+-------------------------------------------------------------------------
  {$EXTERNALSYM PCRYPT_ENCRYPT_PRIVATE_KEY_FUNC}
  PCRYPT_ENCRYPT_PRIVATE_KEY_FUNC = function(
    pAlgorithm: PCryptAlgorithmIdentifier;
    pClearTextPrivateKey: PCryptDataBlob;
    pbEncryptedKey: PBYTE; out pcbEncryptedKey: DWORD;
    pVoidEncryptFunc: Pointer): BOOL stdcall;
  TCryptEncryptPrivateKeyFunc = PCRYPT_ENCRYPT_PRIVATE_KEY_FUNC;

//+-------------------------------------------------------------------------
// this callback is given from the context of a ImportPKCS8 calls.  the caller
// is then expected to hand back an HCRYPTPROV to receive the key being imported
//
// the parameters are:
// pPrivateKeyInfo - pointer to a CRYPT_PRIVATE_KEY_INFO structure which
//                   describes the key being imported
// EncryptedPrivateKey - the encrypted private key blob
// phCryptProv - a pointer to a HCRRYPTPROV to be filled in
// pVoidResolveFunc - this is the pVoidResolveFunc passed in by the caller in the
//                    CRYPT_PRIVATE_KEY_BLOB_AND_PARAMS struct
//+-------------------------------------------------------------------------
  {$EXTERNALSYM PCRYPT_RESOLVE_HCRYPTPROV_FUNC}
  PCRYPT_RESOLVE_HCRYPTPROV_FUNC = function(
    pPrivateKeyInfo: PCryptPrivateKeyInfo; var phCryptProv: HCRYPTPROV;
    pVoidResolveFunc: Pointer): BOOL stdcall;
  TCryptResolveHCryptProvFunc = PCRYPT_RESOLVE_HCRYPTPROV_FUNC;

//+-------------------------------------------------------------------------
// this struct contains a PKCS8 private key and two pointers to callback
// functions, with a corresponding pVoids.  the first callback is used to give
// the caller the opportunity to specify where the key is imported to.  the callback
// passes the caller the algoroithm OID and key size to use in making the decision.
// the other callback is used to decrypt the private key if the PKCS8 contains an
// EncryptedPrivateKeyInfo.  both pVoids are preserved and passed back to the caller
// in the respective callback
//+-------------------------------------------------------------------------
  PCryptPKCS8ImportParams = ^TCryptPKCS8ImportParams;
  PCryptPrivateKeyBlobandParams = ^TCryptPrivateKeyBlobandParams;
  {$EXTERNALSYM _CRYPT_PKCS8_IMPORT_PARAMS}
  _CRYPT_PKCS8_IMPORT_PARAMS = record
    PrivateKey: CRYPT_DIGEST_BLOB;             // PKCS8 blob
    pResolvehCryptProvFunc: TCryptResolveHCryptProvFunc; // optional
    pVoidResolveFunc: Pointer;       // optional
    pDecryptPrivateKeyFunc: TCryptDecryptPrivateKeyFunc;
    pVoidDecryptFunc: Pointer;
  end;
  {$EXTERNALSYM CRYPT_PKCS8_IMPORT_PARAMS}
  CRYPT_PKCS8_IMPORT_PARAMS = _CRYPT_PKCS8_IMPORT_PARAMS;
  {$EXTERNALSYM PCRYPT_PKCS8_IMPORT_PARAMS}
  PCRYPT_PKCS8_IMPORT_PARAMS = ^_CRYPT_PKCS8_IMPORT_PARAMS;
  {$EXTERNALSYM CRYPT_PRIVATE_KEY_BLOB_AND_PARAMS}
  CRYPT_PRIVATE_KEY_BLOB_AND_PARAMS = _CRYPT_PKCS8_IMPORT_PARAMS;
  {$EXTERNALSYM PCRYPT_PRIVATE_KEY_BLOB_AND_PARAMS}
  PCRYPT_PRIVATE_KEY_BLOB_AND_PARAMS = ^_CRYPT_PKCS8_IMPORT_PARAMS;
  TCryptPKCS8ImportParams = _CRYPT_PKCS8_IMPORT_PARAMS;
  TCryptPrivateKeyBlobandParams = _CRYPT_PKCS8_IMPORT_PARAMS;


//+-------------------------------------------------------------------------
// this struct contains information identifying a private key and a pointer
// to a callback function, with a corresponding pVoid. The callback is used
// to encrypt the private key. If the pEncryptPrivateKeyFunc is NULL, the
// key will not be encrypted and an EncryptedPrivateKeyInfo will not be generated.
// The pVoid is preserved and passed back to the caller in the respective callback
//+-------------------------------------------------------------------------
  PCryptPKCS8ExportParams = ^TCryptPKCS8ExportParams;
  {$EXTERNALSYM _CRYPT_PKCS8_EXPORT_PARAMS}
  _CRYPT_PKCS8_EXPORT_PARAMS = record
    hCryptProv: HCRYPTPROV;
    dwKeySpec: DWORD;
    pszPrivateKeyObjId: LPSTR;

    pEncryptPrivateKeyFunc: TCryptEncryptPrivateKeyFunc;
    pVoidEncryptFunc: Pointer;
  end;
  {$EXTERNALSYM CRYPT_PKCS8_EXPORT_PARAMS}
  CRYPT_PKCS8_EXPORT_PARAMS = _CRYPT_PKCS8_EXPORT_PARAMS;
  {$EXTERNALSYM PCRYPT_PKCS8_EXPORT_PARAMS}
  PCRYPT_PKCS8_EXPORT_PARAMS = ^_CRYPT_PKCS8_EXPORT_PARAMS;
  TCryptPKCS8ExportParams = _CRYPT_PKCS8_EXPORT_PARAMS;

//+-------------------------------------------------------------------------
//  Information stored in a certificate
//
//  The Issuer, Subject, Algorithm, PublicKey and Extension BLOBs are the
//  encoded representation of the information.
//--------------------------------------------------------------------------
// certenrolls_begin -- CERT_CONTEXT
  PCertInfo = ^TCertInfo;
  {$EXTERNALSYM _CERT_INFO}
  _CERT_INFO = record
    dwVersion: DWORD;
    SerialNumber: CRYPT_INTEGER_BLOB;
    SignatureAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    Issuer: CERT_NAME_BLOB;
    NotBefore: FILETIME;
    NotAfter: FILETIME;
    Subject: CERT_NAME_BLOB;
    SubjectPublicKeyInfo: CERT_PUBLIC_KEY_INFO;
    IssuerUniqueId: CRYPT_BIT_BLOB;
    SubjectUniqueId: CRYPT_BIT_BLOB;
    cExtension: DWORD;
    rgExtension: PCertExtension;
  end;
  {$EXTERNALSYM CERT_INFO}
  CERT_INFO = _CERT_INFO;
  {$EXTERNALSYM PCERT_INFO}
  PCERT_INFO = ^_CERT_INFO;
  TCertInfo = _CERT_INFO;
// certenrolls_end

const
//+-------------------------------------------------------------------------
//  Certificate versions
//--------------------------------------------------------------------------
  {$EXTERNALSYM CERT_V1}
  CERT_V1 = 0;
  {$EXTERNALSYM CERT_V2}
  CERT_V2 = 1;
  {$EXTERNALSYM CERT_V3}
  CERT_V3 = 2;

//+-------------------------------------------------------------------------
//  Certificate Information Flags
//--------------------------------------------------------------------------
  {$EXTERNALSYM CERT_INFO_VERSION_FLAG}
  CERT_INFO_VERSION_FLAG = 1;
  {$EXTERNALSYM CERT_INFO_SERIAL_NUMBER_FLAG}
  CERT_INFO_SERIAL_NUMBER_FLAG = 2;
  {$EXTERNALSYM CERT_INFO_SIGNATURE_ALGORITHM_FLAG}
  CERT_INFO_SIGNATURE_ALGORITHM_FLAG = 3;
  {$EXTERNALSYM CERT_INFO_ISSUER_FLAG}
  CERT_INFO_ISSUER_FLAG = 4;
  {$EXTERNALSYM CERT_INFO_NOT_BEFORE_FLAG}
  CERT_INFO_NOT_BEFORE_FLAG = 5;
  {$EXTERNALSYM CERT_INFO_NOT_AFTER_FLAG}
  CERT_INFO_NOT_AFTER_FLAG = 6;
  {$EXTERNALSYM CERT_INFO_SUBJECT_FLAG}
  CERT_INFO_SUBJECT_FLAG = 7;
  {$EXTERNALSYM CERT_INFO_SUBJECT_PUBLIC_KEY_INFO_FLAG}
  CERT_INFO_SUBJECT_PUBLIC_KEY_INFO_FLAG = 8;
  {$EXTERNALSYM CERT_INFO_ISSUER_UNIQUE_ID_FLAG}
  CERT_INFO_ISSUER_UNIQUE_ID_FLAG = 9;
  {$EXTERNALSYM CERT_INFO_SUBJECT_UNIQUE_ID_FLAG}
  CERT_INFO_SUBJECT_UNIQUE_ID_FLAG = 10;
  {$EXTERNALSYM CERT_INFO_EXTENSION_FLAG}
  CERT_INFO_EXTENSION_FLAG = 11;

type
//+-------------------------------------------------------------------------
//  An entry in a CRL
//
//  The Extension BLOBs are the encoded representation of the information.
//--------------------------------------------------------------------------
  PCRLEntry = ^TCRLEntry;
  {$EXTERNALSYM _CRL_ENTRY}
  _CRL_ENTRY = record
    SerialNumber: CRYPT_INTEGER_BLOB;
    RevocationDate: FILETIME;
    cExtension: DWORD;
    rgExtension: PCertExtension;
  end;
  {$EXTERNALSYM CRL_ENTRY}
  CRL_ENTRY = _CRL_ENTRY;
  {$EXTERNALSYM PCRL_ENTRY}
  PCRL_ENTRY = ^_CRL_ENTRY;
  TCRLEntry = _CRL_ENTRY;

//+-------------------------------------------------------------------------
//  Information stored in a CRL
//
//  The Issuer, Algorithm and Extension BLOBs are the encoded
//  representation of the information.
//--------------------------------------------------------------------------
  PCRLInfo = ^TCRLInfo;
  {$EXTERNALSYM _CRL_INFO}
  _CRL_INFO = record
    dwVersion: DWORD;
    SignatureAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    Issuer: CERT_NAME_BLOB;
    ThisUpdate: FILETIME;
    NextUpdate: FILETIME;
    cCRLEntry: DWORD;
    rgCRLEntry: PCRLEntry;
    cExtension: DWORD;
    rgExtension: PCertExtension;
  end;
  {$EXTERNALSYM CRL_INFO}
  CRL_INFO = _CRL_INFO;
  {$EXTERNALSYM PCRL_INFO}
  PCRL_INFO = ^_CRL_INFO;
  {$NODEFINE PPCRL_INFO}
  PPCRL_INFO = ^PCRL_INFO;
  TCRLInfo = _CRL_INFO;

const
//+-------------------------------------------------------------------------
//  CRL versions
//--------------------------------------------------------------------------
  {$EXTERNALSYM CRL_V1}
  CRL_V1 = 0;
  {$EXTERNALSYM CRL_V2}
  CRL_V2 = 1;

type
//+-------------------------------------------------------------------------
//  Information stored in a certificate request
//
//  The Subject, Algorithm, PublicKey and Attribute BLOBs are the encoded
//  representation of the information.
//--------------------------------------------------------------------------
  PCertRequestInfo = ^TCertRequestInfo;
  {$EXTERNALSYM _CERT_REQUEST_INFO}
  _CERT_REQUEST_INFO = record
    dwVersion: DWORD;
    Subject: CERT_NAME_BLOB;
    SubjectPublicKeyInfo: CERT_PUBLIC_KEY_INFO;
    cAttribute: DWORD;
    rgAttribute: PCryptAttribute;
  end;
  {$EXTERNALSYM CERT_REQUEST_INFO}
  CERT_REQUEST_INFO = _CERT_REQUEST_INFO;
  {$EXTERNALSYM PCERT_REQUEST_INFO}
  PCERT_REQUEST_INFO = ^_CERT_REQUEST_INFO;
  TCertRequestInfo = _CERT_REQUEST_INFO;

const
//+-------------------------------------------------------------------------
//  Certificate Request versions
//--------------------------------------------------------------------------
  {$EXTERNALSYM CERT_REQUEST_V1}
  CERT_REQUEST_V1 = 0;

type
//+-------------------------------------------------------------------------
//  Information stored in Netscape's Keygen request
//--------------------------------------------------------------------------
  PCertKeyGenRequestInfo = ^TCertKeyGenRequestInfo;
  {$EXTERNALSYM _CERT_KEYGEN_REQUEST_INFO}
  _CERT_KEYGEN_REQUEST_INFO = record
    dwVersion: DWORD;
    SubjectPublicKeyInfo: CERT_PUBLIC_KEY_INFO;
    pwszChallengeString: LPWSTR;        // encoded as IA5
  end;
  {$EXTERNALSYM CERT_KEYGEN_REQUEST_INFO}
  CERT_KEYGEN_REQUEST_INFO = _CERT_KEYGEN_REQUEST_INFO;
  {$EXTERNALSYM PCERT_KEYGEN_REQUEST_INFO}
  PCERT_KEYGEN_REQUEST_INFO = ^_CERT_KEYGEN_REQUEST_INFO;
  TCertKeyGenRequestInfo = _CERT_KEYGEN_REQUEST_INFO;

const
  {$EXTERNALSYM CERT_KEYGEN_REQUEST_V1}
  CERT_KEYGEN_REQUEST_V1 = 0;

type
//+-------------------------------------------------------------------------
//  Certificate, CRL, Certificate Request or Keygen Request Signed Content
//
//  The 'to be signed' encoded content plus its signature. The ToBeSigned
//  is the encoded CERT_INFO, CRL_INFO, CERT_REQUEST_INFO or
//  CERT_KEYGEN_REQUEST_INFO.
//--------------------------------------------------------------------------
  PCertSignedContextInfo = ^TCertSignedContextInfo;
  {$EXTERNALSYM _CERT_SIGNED_CONTENT_INFO}
  _CERT_SIGNED_CONTENT_INFO = record
    ToBeSigned: CRYPT_DER_BLOB;
    SignatureAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    Signature: CRYPT_BIT_BLOB;
  end;
  {$EXTERNALSYM CERT_SIGNED_CONTENT_INFO}
  CERT_SIGNED_CONTENT_INFO = _CERT_SIGNED_CONTENT_INFO;
  {$EXTERNALSYM PCERT_SIGNED_CONTENT_INFO}
  PCERT_SIGNED_CONTENT_INFO = ^_CERT_SIGNED_CONTENT_INFO;
  TCertSignedContextInfo = _CERT_SIGNED_CONTENT_INFO;

//+-------------------------------------------------------------------------
//  Certificate Trust List (CTL)
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CTL Usage. Also used for EnhancedKeyUsage extension.
//--------------------------------------------------------------------------
  PCTLUsage = ^TCTLUsage;
  PCertEnhKeyUsage = ^TCertEnhKeyUsage;
  {$EXTERNALSYM _CTL_USAGE}
  _CTL_USAGE = record
    cUsageIdentifier: DWORD;
    rgpszUsageIdentifier: PLPSTR;      // array of pszObjId
  end;
  {$EXTERNALSYM CTL_USAGE}
  CTL_USAGE = _CTL_USAGE;
  {$EXTERNALSYM PCTL_USAGE}
  PCTL_USAGE = ^_CTL_USAGE;
  {$EXTERNALSYM CERT_ENHKEY_USAGE}
  CERT_ENHKEY_USAGE = _CTL_USAGE;
  {$EXTERNALSYM PCERT_ENHKEY_USAGE}
  PCERT_ENHKEY_USAGE = ^_CTL_USAGE;
  TCTLUsage = _CTL_USAGE;
  TCertEnhKeyUsage = _CTL_USAGE;

//+-------------------------------------------------------------------------
//  An entry in a CTL
//--------------------------------------------------------------------------
  PCTLEntry = ^TCTLEntry;
  {$EXTERNALSYM _CTL_ENTRY}
  _CTL_ENTRY = record
    SubjectIdentifier: CRYPT_DATA_BLOB;          // For example, its hash
    cAttribute: DWORD;
    rgAttribute: PCryptAttribute;                // OPTIONAL
  end;
  {$EXTERNALSYM CTL_ENTRY}
  CTL_ENTRY = _CTL_ENTRY;
  {$EXTERNALSYM PCTL_ENTRY}
  PCTL_ENTRY = ^_CTL_ENTRY;
  TCTLEntry = _CTL_ENTRY;

//+-------------------------------------------------------------------------
//  Information stored in a CTL
//--------------------------------------------------------------------------
  PCTLInfo = ^TCTLInfo;
  {$EXTERNALSYM _CTL_INFO}
  _CTL_INFO = record
    dwVersion: DWORD;
    SubjectUsage: CTL_USAGE;
    ListIdentifier: CRYPT_DATA_BLOB;     // OPTIONAL
    SequenceNumber: CRYPT_INTEGER_BLOB;     // OPTIONAL
    ThisUpdate: FILETIME;
    NextUpdate: FILETIME;         // OPTIONAL
    SubjectAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    cCTLEntry: DWORD;
    rgCTLEntry: PCTLEntry;         // OPTIONAL
    cExtension: DWORD;
    rgExtension: PCertExtension;        // OPTIONAL
  end;
  {$EXTERNALSYM CTL_INFO}
  CTL_INFO = _CTL_INFO;
  {$EXTERNALSYM PCTL_INFO}
  PCTL_INFO = ^_CTL_INFO;
  TCTLInfo = _CTL_INFO;

const
//+-------------------------------------------------------------------------
//  CTL versions
//--------------------------------------------------------------------------
  {$EXTERNALSYM CTL_V1}
  CTL_V1 = 0;

type
//+-------------------------------------------------------------------------
//  TimeStamp Request
//
//  The pszTimeStamp is the OID for the Time type requested
//  The pszContentType is the Content Type OID for the content, usually DATA
//  The Content is a un-decoded blob
//--------------------------------------------------------------------------
  PCryptTimeStampRequestInfo = ^TCryptTimeStampRequestInfo;
  {$EXTERNALSYM _CRYPT_TIME_STAMP_REQUEST_INFO}
  _CRYPT_TIME_STAMP_REQUEST_INFO = record
    pszTimeStampAlgorithm: LPSTR;   // pszObjId
    pszContentType: LPSTR;          // pszObjId
    Content: CRYPT_OBJID_BLOB;
    cAttribute: DWORD;
    rgAttribute: PCryptAttribute;
  end;
  {$EXTERNALSYM CRYPT_TIME_STAMP_REQUEST_INFO}
  CRYPT_TIME_STAMP_REQUEST_INFO = _CRYPT_TIME_STAMP_REQUEST_INFO;
  {$EXTERNALSYM PCRYPT_TIME_STAMP_REQUEST_INFO}
  PCRYPT_TIME_STAMP_REQUEST_INFO = ^_CRYPT_TIME_STAMP_REQUEST_INFO;
  TCryptTimeStampRequestInfo = _CRYPT_TIME_STAMP_REQUEST_INFO;

//+-------------------------------------------------------------------------
//  Name Value Attribute
//--------------------------------------------------------------------------
  PCryptEnrollmentNameValuePair = ^TCryptEnrollmentNameValuePair;
  {$EXTERNALSYM _CRYPT_ENROLLMENT_NAME_VALUE_PAIR}
  _CRYPT_ENROLLMENT_NAME_VALUE_PAIR = record
    pwszName: LPWSTR;
    pwszValue: LPWSTR;
  end;
  {$EXTERNALSYM CRYPT_ENROLLMENT_NAME_VALUE_PAIR}
  CRYPT_ENROLLMENT_NAME_VALUE_PAIR = _CRYPT_ENROLLMENT_NAME_VALUE_PAIR;
  {$EXTERNALSYM PCRYPT_ENROLLMENT_NAME_VALUE_PAIR}
  PCRYPT_ENROLLMENT_NAME_VALUE_PAIR = ^_CRYPT_ENROLLMENT_NAME_VALUE_PAIR;
  TCryptEnrollmentNameValuePair = _CRYPT_ENROLLMENT_NAME_VALUE_PAIR;

//+-------------------------------------------------------------------------
//  CSP Provider
//--------------------------------------------------------------------------
  PCryptCSPProvider = ^TCryptCSPProvider;
  {$EXTERNALSYM _CRYPT_CSP_PROVIDER}
  _CRYPT_CSP_PROVIDER = record
    dwKeySpec: DWORD;
    pwszProviderName: LPWSTR;
    Signature: CRYPT_BIT_BLOB;
  end;
  {$EXTERNALSYM CRYPT_CSP_PROVIDER}
  CRYPT_CSP_PROVIDER = _CRYPT_CSP_PROVIDER;
  {$EXTERNALSYM PCRYPT_CSP_PROVIDER}
  PCRYPT_CSP_PROVIDER = ^_CRYPT_CSP_PROVIDER;
  TCryptCSPProvider = _CRYPT_CSP_PROVIDER;

//+-------------------------------------------------------------------------
//  Certificate and Message encoding types
//
//  The encoding type is a DWORD containing both the certificate and message
//  encoding types. The certificate encoding type is stored in the LOWORD.
//  The message encoding type is stored in the HIWORD. Some functions or
//  structure fields require only one of the encoding types. The following
//  naming convention is used to indicate which encoding type(s) are
//  required:
//      dwEncodingType              (both encoding types are required)
//      dwMsgAndCertEncodingType    (both encoding types are required)
//      dwMsgEncodingType           (only msg encoding type is required)
//      dwCertEncodingType          (only cert encoding type is required)
//
//  Its always acceptable to specify both.
//--------------------------------------------------------------------------
const
  {$EXTERNALSYM CERT_ENCODING_TYPE_MASK}
  CERT_ENCODING_TYPE_MASK = $0000FFFF;
  {$EXTERNALSYM CMSG_ENCODING_TYPE_MASK}
  CMSG_ENCODING_TYPE_MASK = $FFFF0000;

{$EXTERNALSYM GET_CERT_ENCODING_TYPE}
function GET_CERT_ENCODING_TYPE(X: DWORD): DWORD; {$IfDef HAS_INLINE}inline;{$EndIf}

{$EXTERNALSYM GET_CMSG_ENCODING_TYPE}
function GET_CMSG_ENCODING_TYPE(X: DWORD): DWORD; {$IfDef HAS_INLINE}inline;{$EndIf}

const
  {$EXTERNALSYM CRYPT_ASN_ENCODING}
  CRYPT_ASN_ENCODING = $00000001;
  {$EXTERNALSYM CRYPT_NDR_ENCODING}
  CRYPT_NDR_ENCODING = $00000002;
  {$EXTERNALSYM X509_ASN_ENCODING}
  X509_ASN_ENCODING = $00000001;
  {$EXTERNALSYM X509_NDR_ENCODING}
  X509_NDR_ENCODING = $00000002;
  {$EXTERNALSYM PKCS_7_ASN_ENCODING}
  PKCS_7_ASN_ENCODING = $00010000;
  {$EXTERNALSYM PKCS_7_NDR_ENCODING}
  PKCS_7_NDR_ENCODING = $00020000;


//+-------------------------------------------------------------------------
//  format the specified data structure according to the certificate
//  encoding type.
//
//  The default behavior of CryptFormatObject is to return single line
//  display of the encoded data, that is, each subfield will be concatenated with
//  a ', ' on one line.  If user prefers to display the data in multiple line,
//  set the flag CRYPT_FORMAT_STR_MULTI_LINE, that is, each subfield will be displayed
//  on a seperate line.
//
//  If there is no formatting routine installed or registered
//  for the lpszStructType, the hex dump of the encoded BLOB will be returned.
//  User can set the flag CRYPT_FORMAT_STR_NO_HEX to disable the hex dump.
//--------------------------------------------------------------------------
function CryptFormatObject(dwCertEncodingType, dwFormatType,
  dwFormatStrType: DWORD; pFormatStruct: Pointer; lpszStructType: LPCSTR;
  pbEncoded: PBYTE; cbEncoded: DWORD; pbFormat: Pointer;
  out pcbFormat: DWORD): BOOL; stdcall;

const
//-------------------------------------------------------------------------
// constants for dwFormatStrType of function CryptFormatObject
//-------------------------------------------------------------------------
  {$EXTERNALSYM CRYPT_FORMAT_STR_MULTI_LINE}
  CRYPT_FORMAT_STR_MULTI_LINE         = $0001;
  {$EXTERNALSYM CRYPT_FORMAT_STR_NO_HEX}
  CRYPT_FORMAT_STR_NO_HEX             = $0010;

//-------------------------------------------------------------------------
// constants for dwFormatType of function CryptFormatObject
// when format X509_NAME or X509_UNICODE_NAME
//-------------------------------------------------------------------------
// Just get the simple string
  {$EXTERNALSYM CRYPT_FORMAT_SIMPLE}
  CRYPT_FORMAT_SIMPLE                 = $0001;

//Put an attribute name infront of the attribute
//such as 'O=Microsoft,DN=xiaohs'
  {$EXTERNALSYM CRYPT_FORMAT_X509}
  CRYPT_FORMAT_X509                   = $0002;

//Put an OID infront of the simple string, such as
//'2.5.4.22=Microsoft,2.5.4.3=xiaohs'
  {$EXTERNALSYM CRYPT_FORMAT_OID}
  CRYPT_FORMAT_OID                    = $0004;

//Put a ';' between each RDN.  The default is ','
  {$EXTERNALSYM CRYPT_FORMAT_RDN_SEMICOLON}
  CRYPT_FORMAT_RDN_SEMICOLON          = $0100;

//Put a '\n' between each RDN.
  {$EXTERNALSYM CRYPT_FORMAT_RDN_CRLF}
  CRYPT_FORMAT_RDN_CRLF               = $0200;

//Unquote the DN value, which is quoated by default va the following
//rules: if the DN contains leading or trailing
//white space or one of the following characters: ',', '+', '=',
//''', '\n',  '<', '>', '#' or ';'. The quoting character is '.
//If the DN Value contains a ' it is double quoted ('').
  {$EXTERNALSYM CRYPT_FORMAT_RDN_UNQUOTE}
  CRYPT_FORMAT_RDN_UNQUOTE            = $0400;

//reverse the order of the RDNs before converting to the string
  {$EXTERNALSYM CRYPT_FORMAT_RDN_REVERSE}
  CRYPT_FORMAT_RDN_REVERSE            = $0800;

//-------------------------------------------------------------------------
//  contants dwFormatType of function CryptFormatObject when format a DN.:
//
//  The following three values are defined in the section above:
//  CRYPT_FORMAT_SIMPLE:    Just a simple string
//                          such as  'Microsoft+xiaohs+NT'
//  CRYPT_FORMAT_X509       Put an attribute name infront of the attribute
//                          such as 'O=Microsoft+xiaohs+NT'
//
//  CRYPT_FORMAT_OID        Put an OID infront of the simple string,
//                          such as '2.5.4.22=Microsoft+xiaohs+NT'
//
//  Additional values are defined as following:
//----------------------------------------------------------------------------
//Put a ',' between each value.  Default is '+'
  {$EXTERNALSYM CRYPT_FORMAT_COMMA}
  CRYPT_FORMAT_COMMA                  = $1000;

//Put a ';' between each value
  {$EXTERNALSYM CRYPT_FORMAT_SEMICOLON}
  CRYPT_FORMAT_SEMICOLON              = CRYPT_FORMAT_RDN_SEMICOLON;

//Put a '\n' between each value
  {$EXTERNALSYM CRYPT_FORMAT_CRLF}
  CRYPT_FORMAT_CRLF                   = CRYPT_FORMAT_RDN_CRLF;

type
//+-------------------------------------------------------------------------
//  Encode / decode the specified data structure according to the certificate
//  encoding type.
//
//  See below for a list of the predefined data structures.
//--------------------------------------------------------------------------

  {$EXTERNALSYM PFN_CRYPT_ALLOC}
  PFN_CRYPT_ALLOC = function(cbSize: Cardinal): Pointer stdcall;
  TFnCryptAlloc = PFN_CRYPT_ALLOC;

  {$EXTERNALSYM PFN_CRYPT_FREE}
  PFN_CRYPT_FREE = procedure(pv: Pointer) stdcall;
  TFnCryptFree = PFN_CRYPT_FREE;

  PCryptEncodePara = ^TCryptEncodePara;
  {$EXTERNALSYM _CRYPT_ENCODE_PARA}
  _CRYPT_ENCODE_PARA = record
    cbSize: DWORD;
    pfnAlloc: PFN_CRYPT_ALLOC;           // OPTIONAL
    pfnFree: PFN_CRYPT_FREE;            // OPTIONAL
  end;
  {$EXTERNALSYM CRYPT_ENCODE_PARA}
  CRYPT_ENCODE_PARA = _CRYPT_ENCODE_PARA;
  {$EXTERNALSYM PCRYPT_ENCODE_PARA}
  PCRYPT_ENCODE_PARA = ^_CRYPT_ENCODE_PARA;
  TCryptEncodePara = _CRYPT_ENCODE_PARA;

{$EXTERNALSYM CryptEncodeObjectEx}
function CryptEncodeObjectEx(dwCertEncodingType: DWORD; lpszStructType: LPCSTR;
  pvStructInfo: Pointer; dwFlags: DWORD; pEncodePara: PCryptEncodePara;
  pvEncoded: Pointer; var pcbEncoded: DWORD): BOOL; stdcall;

{$EXTERNALSYM CryptEncodeObject}
function CryptEncodeObject(dwCertEncodingType: DWORD; lpszStructType: LPCSTR;
  pvStructInfo: Pointer; pbEncoded: PBYTE;
  out pcbEncoded: DWORD): BOOL; stdcall;

const
// By default the signature bytes are reversed. The following flag can
// be set to inhibit the byte reversal.
//
// This flag is applicable to
//      X509_CERT_TO_BE_SIGNED
  {$EXTERNALSYM CRYPT_ENCODE_NO_SIGNATURE_BYTE_REVERSAL_FLAG}
  CRYPT_ENCODE_NO_SIGNATURE_BYTE_REVERSAL_FLAG = $8;

//  When the following flag is set the called encode function allocates
//  memory for the encoded bytes. A pointer to the allocated bytes
//  is returned in pvEncoded. If pEncodePara or pEncodePara->pfnAlloc is
//  NULL, then, LocalAlloc is called for the allocation and LocalFree must
//  be called to do the free. Otherwise, pEncodePara->pfnAlloc is called
//  for the allocation.
//
//  *pcbEncoded is ignored on input and updated with the length of the
//  allocated, encoded bytes.
//
//  If pfnAlloc is set, then, pfnFree should also be set.
  {$EXTERNALSYM CRYPT_ENCODE_ALLOC_FLAG}
  CRYPT_ENCODE_ALLOC_FLAG = $8000;

//  The following flag is applicable when encoding X509_UNICODE_NAME.
//  When set, CERT_RDN_T61_STRING is selected instead of
//  CERT_RDN_UNICODE_STRING if all the unicode characters are <= $FF
  {$EXTERNALSYM CRYPT_UNICODE_NAME_ENCODE_ENABLE_T61_UNICODE_FLAG}
  CRYPT_UNICODE_NAME_ENCODE_ENABLE_T61_UNICODE_FLAG   = CERT_RDN_ENABLE_T61_UNICODE_FLAG;

//  The following flag is applicable when encoding X509_UNICODE_NAME.
//  When set, CERT_RDN_UTF8_STRING is selected instead of
//  CERT_RDN_UNICODE_STRING.
  {$EXTERNALSYM CRYPT_UNICODE_NAME_ENCODE_ENABLE_UTF8_UNICODE_FLAG}
  CRYPT_UNICODE_NAME_ENCODE_ENABLE_UTF8_UNICODE_FLAG  = CERT_RDN_ENABLE_UTF8_UNICODE_FLAG;

//  The following flag is applicable when encoding X509_UNICODE_NAME.
//  When set, CERT_RDN_UTF8_STRING is selected instead of
//  CERT_RDN_PRINTABLE_STRING for DirectoryString types. Also,
//  enables CRYPT_UNICODE_NAME_ENCODE_ENABLE_UTF8_UNICODE_FLAG.
  {$EXTERNALSYM CRYPT_UNICODE_NAME_ENCODE_FORCE_UTF8_UNICODE_FLAG}
  CRYPT_UNICODE_NAME_ENCODE_FORCE_UTF8_UNICODE_FLAG   = CERT_RDN_FORCE_UTF8_UNICODE_FLAG;

//  The following flag is applicable when encoding X509_UNICODE_NAME,
//  X509_UNICODE_NAME_VALUE or X509_UNICODE_ANY_STRING.
//  When set, the characters aren't checked to see if they
//  are valid for the specified Value Type.
  {$EXTERNALSYM CRYPT_UNICODE_NAME_ENCODE_DISABLE_CHECK_TYPE_FLAG}
  CRYPT_UNICODE_NAME_ENCODE_DISABLE_CHECK_TYPE_FLAG   = CERT_RDN_DISABLE_CHECK_TYPE_FLAG;

//  The following flag is applicable when encoding the PKCS_SORTED_CTL. This
//  flag should be set if the identifier for the TrustedSubjects is a hash,
//  such as, MD5 or SHA1.
  {$EXTERNALSYM CRYPT_SORTED_CTL_ENCODE_HASHED_SUBJECT_IDENTIFIER_FLAG}
  CRYPT_SORTED_CTL_ENCODE_HASHED_SUBJECT_IDENTIFIER_FLAG = $10000;

type  
  PCryptDecodePara = ^TCryptDecodePara;
  {$EXTERNALSYM _CRYPT_DECODE_PARA}
  _CRYPT_DECODE_PARA = record
    cbSize: DWORD;
    pfnAlloc: PFN_CRYPT_ALLOC;           // OPTIONAL
    pfnFree: PFN_CRYPT_FREE;            // OPTIONAL
  end;
  {$EXTERNALSYM CRYPT_DECODE_PARA}
  CRYPT_DECODE_PARA = _CRYPT_DECODE_PARA;
  TCryptDecodePara = _CRYPT_DECODE_PARA;

{$EXTERNALSYM CryptDecodeObjectEx}
function CryptDecodeObjectEx(dwCertEncodingType: DWORD; lpszStructType: LPCSTR;
  pbEncoded: PBYTE; cbEncoded, dwFlags: DWORD; pDecodePara: PCryptDecodePara;
  pvStructInfo: Pointer; out pcbStructInfo: DWORD): BOOL; stdcall;

{$EXTERNALSYM CryptDecodeObject}
function CryptDecodeObject(dwCertEncodingType: DWORD; lpszStructType: LPCSTR;
  pbEncoded: PBYTE; cbEncoded, dwFlags: DWORD; pvStructInfo: Pointer;
  out pcbStructInfo: DWORD): BOOL; stdcall;

const
// When the following flag is set the nocopy optimization is enabled.
// This optimization where appropriate, updates the pvStructInfo fields
// to point to content residing within pbEncoded instead of making a copy
// of and appending to pvStructInfo.
//
// Note, when set, pbEncoded can't be freed until pvStructInfo is freed.
  {$EXTERNALSYM CRYPT_DECODE_NOCOPY_FLAG}
  CRYPT_DECODE_NOCOPY_FLAG                 = $1;

// For CryptDecodeObject(), by default the pbEncoded is the 'to be signed'
// plus its signature. Set the following flag, if pbEncoded points to only
// the 'to be signed'.
//
// This flag is applicable to
//      X509_CERT_TO_BE_SIGNED
//      X509_CERT_CRL_TO_BE_SIGNED
//      X509_CERT_REQUEST_TO_BE_SIGNED
//      X509_KEYGEN_REQUEST_TO_BE_SIGNED
  {$EXTERNALSYM CRYPT_DECODE_TO_BE_SIGNED_FLAG}
  CRYPT_DECODE_TO_BE_SIGNED_FLAG          = $2;

// When the following flag is set, the OID strings are allocated in
// crypt32.dll and shared instead of being copied into the returned
// data structure. This flag may be set if crypt32.dll isn't unloaded
// before the caller is unloaded.
  {$EXTERNALSYM CRYPT_DECODE_SHARE_OID_STRING_FLAG}
  CRYPT_DECODE_SHARE_OID_STRING_FLAG      = $4;

// By default the signature bytes are reversed. The following flag can
// be set to inhibit the byte reversal.
//
// This flag is applicable to
//      X509_CERT_TO_BE_SIGNED
  {$EXTERNALSYM CRYPT_DECODE_NO_SIGNATURE_BYTE_REVERSAL_FLAG}
  CRYPT_DECODE_NO_SIGNATURE_BYTE_REVERSAL_FLAG = $8;

// When the following flag is set the called decode function allocates
// memory for the decoded structure. A pointer to the allocated structure
// is returned in pvStructInfo. If pDecodePara or pDecodePara->pfnAlloc is
// NULL, then, LocalAlloc is called for the allocation and LocalFree must
// be called to do the free. Otherwise, pDecodePara->pfnAlloc is called
// for the allocation.
//
// *pcbStructInfo is ignored on input and updated with the length of the
// allocated, decoded structure.
//
// This flag may also be set in the CryptDecodeObject API. Since
// CryptDecodeObject doesn't take a pDecodePara, LocalAlloc is always
// called for the allocation which must be freed by calling LocalFree.
  {$EXTERNALSYM CRYPT_DECODE_ALLOC_FLAG}
  CRYPT_DECODE_ALLOC_FLAG                 = $8000;

// The following flag is applicable when decoding X509_UNICODE_NAME,
// X509_UNICODE_NAME_VALUE or X509_UNICODE_ANY_STRING.
// By default, CERT_RDN_T61_STRING values are initially decoded
// as UTF8. If the UTF8 decoding fails, then, decoded as 8 bit characters.
// Setting this flag skips the initial attempt to decode as UTF8.
  {$EXTERNALSYM CRYPT_UNICODE_NAME_DECODE_DISABLE_IE4_UTF8_FLAG}
  CRYPT_UNICODE_NAME_DECODE_DISABLE_IE4_UTF8_FLAG     = CERT_RDN_DISABLE_IE4_UTF8_FLAG;

//+-------------------------------------------------------------------------
//  Predefined X509 certificate data structures that can be encoded / decoded.
//--------------------------------------------------------------------------
  {$EXTERNALSYM CRYPT_ENCODE_DECODE_NONE}
  CRYPT_ENCODE_DECODE_NONE            = 0;
  {$EXTERNALSYM X509_CERT}
  X509_CERT                           = LPCSTR(1);
  {$EXTERNALSYM X509_CERT_TO_BE_SIGNED}
  X509_CERT_TO_BE_SIGNED              = LPCSTR(2);
  {$EXTERNALSYM X509_CERT_CRL_TO_BE_SIGNED}
  X509_CERT_CRL_TO_BE_SIGNED          = LPCSTR(3);
  {$EXTERNALSYM X509_CERT_REQUEST_TO_BE_SIGNED}
  X509_CERT_REQUEST_TO_BE_SIGNED      = LPCSTR(4);
  {$EXTERNALSYM X509_EXTENSIONS}
  X509_EXTENSIONS                     = LPCSTR(5);
  {$EXTERNALSYM X509_NAME_VALUE}
  X509_NAME_VALUE                     = LPCSTR(6);
  {$EXTERNALSYM X509_NAME}
  X509_NAME                           = LPCSTR(7);
  {$EXTERNALSYM X509_PUBLIC_KEY_INFO}
  X509_PUBLIC_KEY_INFO                = LPCSTR(8);

//+-------------------------------------------------------------------------
//  Predefined X509 certificate extension data structures that can be
//  encoded / decoded.
//--------------------------------------------------------------------------
  {$EXTERNALSYM X509_AUTHORITY_KEY_ID}
  X509_AUTHORITY_KEY_ID               = LPCSTR(9);
  {$EXTERNALSYM X509_KEY_ATTRIBUTES}
  X509_KEY_ATTRIBUTES                 = LPCSTR(10);
  {$EXTERNALSYM X509_KEY_USAGE_RESTRICTION}
  X509_KEY_USAGE_RESTRICTION          = LPCSTR(11);
  {$EXTERNALSYM X509_ALTERNATE_NAME}
  X509_ALTERNATE_NAME                 = LPCSTR(12);
  {$EXTERNALSYM X509_BASIC_CONSTRAINTS}
  X509_BASIC_CONSTRAINTS              = LPCSTR(13);
  {$EXTERNALSYM X509_KEY_USAGE}
  X509_KEY_USAGE                      = LPCSTR(14);
  {$EXTERNALSYM X509_BASIC_CONSTRAINTS2}
  X509_BASIC_CONSTRAINTS2             = LPCSTR(15);
  {$EXTERNALSYM X509_CERT_POLICIES}
  X509_CERT_POLICIES                  = LPCSTR(16);

//+-------------------------------------------------------------------------
//  Additional predefined data structures that can be encoded / decoded.
//--------------------------------------------------------------------------
  {$EXTERNALSYM PKCS_UTC_TIME}
  PKCS_UTC_TIME                       = LPCSTR(17);
  {$EXTERNALSYM PKCS_TIME_REQUEST}
  PKCS_TIME_REQUEST                   = LPCSTR(18);
  {$EXTERNALSYM RSA_CSP_PUBLICKEYBLOB}
  RSA_CSP_PUBLICKEYBLOB               = LPCSTR(19);
  {$EXTERNALSYM X509_UNICODE_NAME}
  X509_UNICODE_NAME                   = LPCSTR(20);

  {$EXTERNALSYM X509_KEYGEN_REQUEST_TO_BE_SIGNED}
  X509_KEYGEN_REQUEST_TO_BE_SIGNED    = LPCSTR(21);
  {$EXTERNALSYM PKCS_ATTRIBUTE}
  PKCS_ATTRIBUTE                      = LPCSTR(22);
  {$EXTERNALSYM PKCS_CONTENT_INFO_SEQUENCE_OF_ANY}
  PKCS_CONTENT_INFO_SEQUENCE_OF_ANY   = LPCSTR(23);

//+-------------------------------------------------------------------------
//  Predefined primitive data structures that can be encoded / decoded.
//--------------------------------------------------------------------------
  {$EXTERNALSYM X509_UNICODE_NAME_VALUE}
  X509_UNICODE_NAME_VALUE             = LPCSTR(24);
  {$EXTERNALSYM X509_ANY_STRING}
  X509_ANY_STRING                     = X509_NAME_VALUE;
  {$EXTERNALSYM X509_UNICODE_ANY_STRING}
  X509_UNICODE_ANY_STRING             = X509_UNICODE_NAME_VALUE;
  {$EXTERNALSYM X509_OCTET_STRING}
  X509_OCTET_STRING                   = LPCSTR(25);
  {$EXTERNALSYM X509_BITS}
  X509_BITS                           = LPCSTR(26);
  {$EXTERNALSYM X509_INTEGER}
  X509_INTEGER                        = LPCSTR(27);
  {$EXTERNALSYM X509_MULTI_BYTE_INTEGER}
  X509_MULTI_BYTE_INTEGER             = LPCSTR(28);
  {$EXTERNALSYM X509_ENUMERATED}
  X509_ENUMERATED                     = LPCSTR(29);
  {$EXTERNALSYM X509_CHOICE_OF_TIME}
  X509_CHOICE_OF_TIME                 = LPCSTR(30);

//+-------------------------------------------------------------------------
//  More predefined X509 certificate extension data structures that can be
//  encoded / decoded.
//--------------------------------------------------------------------------
  {$EXTERNALSYM X509_AUTHORITY_KEY_ID2}
  X509_AUTHORITY_KEY_ID2              = LPCSTR(31);
  {$EXTERNALSYM X509_AUTHORITY_INFO_ACCESS}
  X509_AUTHORITY_INFO_ACCESS          = LPCSTR(32);
  {$EXTERNALSYM X509_SUBJECT_INFO_ACCESS}
  X509_SUBJECT_INFO_ACCESS            = X509_AUTHORITY_INFO_ACCESS;
  {$EXTERNALSYM X509_CRL_REASON_CODE}
  X509_CRL_REASON_CODE                = X509_ENUMERATED;
  {$EXTERNALSYM PKCS_CONTENT_INFO}
  PKCS_CONTENT_INFO                   = LPCSTR(33);
  {$EXTERNALSYM X509_SEQUENCE_OF_ANY}
  X509_SEQUENCE_OF_ANY                = LPCSTR(34);
  {$EXTERNALSYM X509_CRL_DIST_POINTS}
  X509_CRL_DIST_POINTS                = LPCSTR(35);
  {$EXTERNALSYM X509_ENHANCED_KEY_USAGE}
  X509_ENHANCED_KEY_USAGE             = LPCSTR(36);
  {$EXTERNALSYM PKCS_CTL}
  PKCS_CTL                            = LPCSTR(37);

  {$EXTERNALSYM X509_MULTI_BYTE_UINT}
  X509_MULTI_BYTE_UINT                = LPCSTR(38);
  {$EXTERNALSYM X509_DSS_PUBLICKEY}
  X509_DSS_PUBLICKEY                  = X509_MULTI_BYTE_UINT;
  {$EXTERNALSYM X509_DSS_PARAMETERS}
  X509_DSS_PARAMETERS                 = LPCSTR(39);
  {$EXTERNALSYM X509_DSS_SIGNATURE}
  X509_DSS_SIGNATURE                  = LPCSTR(40);
  {$EXTERNALSYM PKCS_RC2_CBC_PARAMETERS}
  PKCS_RC2_CBC_PARAMETERS             = LPCSTR(41);
  {$EXTERNALSYM PKCS_SMIME_CAPABILITIES}
  PKCS_SMIME_CAPABILITIES             = LPCSTR(42);

// Qualified Certificate Statements Extension uses the same encode/decode
// function as PKCS_SMIME_CAPABILITIES. Its data structures are identical
// except for the names of the fields.
  {$EXTERNALSYM X509_QC_STATEMENTS_EXT}
  X509_QC_STATEMENTS_EXT              = LPCSTR(42);

//+-------------------------------------------------------------------------
//  data structures for private keys
//--------------------------------------------------------------------------
  {$EXTERNALSYM PKCS_RSA_PRIVATE_KEY}
  PKCS_RSA_PRIVATE_KEY                = LPCSTR(43);
  {$EXTERNALSYM PKCS_PRIVATE_KEY_INFO}
  PKCS_PRIVATE_KEY_INFO               = LPCSTR(44);
  {$EXTERNALSYM PKCS_ENCRYPTED_PRIVATE_KEY_INFO}
  PKCS_ENCRYPTED_PRIVATE_KEY_INFO     = LPCSTR(45);

//+-------------------------------------------------------------------------
//  certificate policy qualifier
//--------------------------------------------------------------------------
  {$EXTERNALSYM X509_PKIX_POLICY_QUALIFIER_USERNOTICE}
  X509_PKIX_POLICY_QUALIFIER_USERNOTICE = LPCSTR(46);

//+-------------------------------------------------------------------------
//  Diffie-Hellman Key Exchange
//--------------------------------------------------------------------------
  {$EXTERNALSYM X509_DH_PUBLICKEY}
  X509_DH_PUBLICKEY                   = X509_MULTI_BYTE_UINT;
  {$EXTERNALSYM X509_DH_PARAMETERS}
  X509_DH_PARAMETERS                  = LPCSTR(47);
  {$EXTERNALSYM PKCS_ATTRIBUTES}
  PKCS_ATTRIBUTES                     = LPCSTR(48);
  {$EXTERNALSYM PKCS_SORTED_CTL}
  PKCS_SORTED_CTL                     = LPCSTR(49);

//+-------------------------------------------------------------------------
//  ECC Signature
//--------------------------------------------------------------------------
// Uses the same encode/decode function as X509_DH_PARAMETERS. Its data
// structure is identical except for the names of the fields.
  {$EXTERNALSYM X509_ECC_SIGNATURE}
  X509_ECC_SIGNATURE                  = LPCSTR(47);

//+-------------------------------------------------------------------------
//  X942 Diffie-Hellman
//--------------------------------------------------------------------------
  {$EXTERNALSYM X942_DH_PARAMETERS}
  X942_DH_PARAMETERS                  = LPCSTR(50);

//+-------------------------------------------------------------------------
//  The following is the same as X509_BITS, except before encoding,
//  the bit length is decremented to exclude trailing zero bits.
//--------------------------------------------------------------------------
  {$EXTERNALSYM X509_BITS_WITHOUT_TRAILING_ZEROES}
  X509_BITS_WITHOUT_TRAILING_ZEROES   = LPCSTR(51);

//+-------------------------------------------------------------------------
//  X942 Diffie-Hellman Other Info
//--------------------------------------------------------------------------
  {$EXTERNALSYM X942_OTHER_INFO}
  X942_OTHER_INFO                     = LPCSTR(52);

  {$EXTERNALSYM X509_CERT_PAIR}
  X509_CERT_PAIR                      = LPCSTR(53);
  {$EXTERNALSYM X509_ISSUING_DIST_POINT}
  X509_ISSUING_DIST_POINT             = LPCSTR(54);
  {$EXTERNALSYM X509_NAME_CONSTRAINTS}
  X509_NAME_CONSTRAINTS               = LPCSTR(55);
  {$EXTERNALSYM X509_POLICY_MAPPINGS}
  X509_POLICY_MAPPINGS                = LPCSTR(56);
  {$EXTERNALSYM X509_POLICY_CONSTRAINTS}
  X509_POLICY_CONSTRAINTS             = LPCSTR(57);
  {$EXTERNALSYM X509_CROSS_CERT_DIST_POINTS}
  X509_CROSS_CERT_DIST_POINTS         = LPCSTR(58);

//+-------------------------------------------------------------------------
//  Certificate Management Messages over CMS (CMC) Data Structures
//--------------------------------------------------------------------------
  {$EXTERNALSYM CMC_DATA}
  CMC_DATA                            = LPCSTR(59);
  {$EXTERNALSYM CMC_RESPONSE}
  CMC_RESPONSE                        = LPCSTR(60);
  {$EXTERNALSYM CMC_STATUS}
  CMC_STATUS                          = LPCSTR(61);
  {$EXTERNALSYM CMC_ADD_EXTENSIONS}
  CMC_ADD_EXTENSIONS                  = LPCSTR(62);
  {$EXTERNALSYM CMC_ADD_ATTRIBUTES}
  CMC_ADD_ATTRIBUTES                  = LPCSTR(63);

//+-------------------------------------------------------------------------
//  Certificate Template
//--------------------------------------------------------------------------
  {$EXTERNALSYM X509_CERTIFICATE_TEMPLATE}
  X509_CERTIFICATE_TEMPLATE           = LPCSTR(64);

//+-------------------------------------------------------------------------
//  Online Certificate Status Protocol (OCSP) Data Structures
//--------------------------------------------------------------------------
  {$EXTERNALSYM OCSP_SIGNED_REQUEST}
  OCSP_SIGNED_REQUEST                 = LPCSTR(65);
  {$EXTERNALSYM OCSP_REQUEST}
  OCSP_REQUEST                        = LPCSTR(66);
  {$EXTERNALSYM OCSP_RESPONSE}
  OCSP_RESPONSE                       = LPCSTR(67);
  {$EXTERNALSYM OCSP_BASIC_SIGNED_RESPONSE}
  OCSP_BASIC_SIGNED_RESPONSE          = LPCSTR(68);
  {$EXTERNALSYM OCSP_BASIC_RESPONSE}
  OCSP_BASIC_RESPONSE                 = LPCSTR(69);

//+-------------------------------------------------------------------------
//  Logotype and Biometric Extensions
//--------------------------------------------------------------------------
  {$EXTERNALSYM X509_LOGOTYPE_EXT}
  X509_LOGOTYPE_EXT                   = LPCSTR(70);
  {$EXTERNALSYM X509_BIOMETRIC_EXT}
  X509_BIOMETRIC_EXT                  = LPCSTR(71);

  {$EXTERNALSYM CNG_RSA_PUBLIC_KEY_BLOB}
  CNG_RSA_PUBLIC_KEY_BLOB             = LPCSTR(72);
  {$EXTERNALSYM X509_OBJECT_IDENTIFIER}
  X509_OBJECT_IDENTIFIER              = LPCSTR(73);
  {$EXTERNALSYM X509_ALGORITHM_IDENTIFIER}
  X509_ALGORITHM_IDENTIFIER           = LPCSTR(74);
  {$EXTERNALSYM PKCS_RSA_SSA_PSS_PARAMETERS}
  PKCS_RSA_SSA_PSS_PARAMETERS         = LPCSTR(75);
  {$EXTERNALSYM PKCS_RSAES_OAEP_PARAMETERS}
  PKCS_RSAES_OAEP_PARAMETERS          = LPCSTR(76);

  {$EXTERNALSYM ECC_CMS_SHARED_INFO}
  ECC_CMS_SHARED_INFO                 = LPCSTR(77);

//+-------------------------------------------------------------------------
//  Predefined PKCS #7 data structures that can be encoded / decoded.
//--------------------------------------------------------------------------
  {$EXTERNALSYM PKCS7_SIGNER_INFO}
  PKCS7_SIGNER_INFO                   = LPCSTR(500);

//+-------------------------------------------------------------------------
//  Predefined PKCS #7 data structures that can be encoded / decoded.
//--------------------------------------------------------------------------
  {$EXTERNALSYM CMS_SIGNER_INFO}
  CMS_SIGNER_INFO                     = LPCSTR(501);

//+-------------------------------------------------------------------------
//  Predefined Software Publishing Credential (SPC)  data structures that
//  can be encoded / decoded.
//
//  Predefined values: 2000 .. 2999
//
//  See spc.h for value and data structure definitions.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  Extension Object Identifiers
//--------------------------------------------------------------------------
  {$EXTERNALSYM szOID_AUTHORITY_KEY_IDENTIFIER}
  szOID_AUTHORITY_KEY_IDENTIFIER  = '2.5.29.1';
  {$EXTERNALSYM szOID_KEY_ATTRIBUTES}
  szOID_KEY_ATTRIBUTES            = '2.5.29.2';
  {$EXTERNALSYM szOID_CERT_POLICIES_95}
  szOID_CERT_POLICIES_95          = '2.5.29.3';
  {$EXTERNALSYM szOID_KEY_USAGE_RESTRICTION}
  szOID_KEY_USAGE_RESTRICTION     = '2.5.29.4';
  {$EXTERNALSYM szOID_SUBJECT_ALT_NAME}
  szOID_SUBJECT_ALT_NAME          = '2.5.29.7';
  {$EXTERNALSYM szOID_ISSUER_ALT_NAME}
  szOID_ISSUER_ALT_NAME           = '2.5.29.8';
  {$EXTERNALSYM szOID_BASIC_CONSTRAINTS}
  szOID_BASIC_CONSTRAINTS         = '2.5.29.10';
  {$EXTERNALSYM szOID_KEY_USAGE}
  szOID_KEY_USAGE                 = '2.5.29.15';
  {$EXTERNALSYM szOID_PRIVATEKEY_USAGE_PERIOD}
  szOID_PRIVATEKEY_USAGE_PERIOD   = '2.5.29.16';
  {$EXTERNALSYM szOID_BASIC_CONSTRAINTS2}
  szOID_BASIC_CONSTRAINTS2        = '2.5.29.19';

  {$EXTERNALSYM szOID_CERT_POLICIES}
  szOID_CERT_POLICIES             = '2.5.29.32';
  {$EXTERNALSYM szOID_ANY_CERT_POLICY}
  szOID_ANY_CERT_POLICY           = '2.5.29.32.0';
  {$EXTERNALSYM szOID_INHIBIT_ANY_POLICY}
  szOID_INHIBIT_ANY_POLICY        = '2.5.29.54';

  {$EXTERNALSYM szOID_AUTHORITY_KEY_IDENTIFIER2}
  szOID_AUTHORITY_KEY_IDENTIFIER2 = '2.5.29.35';
  {$EXTERNALSYM szOID_SUBJECT_KEY_IDENTIFIER}
  szOID_SUBJECT_KEY_IDENTIFIER    = '2.5.29.14';
  {$EXTERNALSYM szOID_SUBJECT_ALT_NAME2}
  szOID_SUBJECT_ALT_NAME2         = '2.5.29.17';
  {$EXTERNALSYM szOID_ISSUER_ALT_NAME2}
  szOID_ISSUER_ALT_NAME2          = '2.5.29.18';
  {$EXTERNALSYM szOID_CRL_REASON_CODE}
  szOID_CRL_REASON_CODE           = '2.5.29.21';
  {$EXTERNALSYM szOID_REASON_CODE_HOLD}
  szOID_REASON_CODE_HOLD          = '2.5.29.23';
  {$EXTERNALSYM szOID_CRL_DIST_POINTS}
  szOID_CRL_DIST_POINTS           = '2.5.29.31';
  {$EXTERNALSYM szOID_ENHANCED_KEY_USAGE}
  szOID_ENHANCED_KEY_USAGE        = '2.5.29.37';

  {$EXTERNALSYM szOID_ANY_ENHANCED_KEY_USAGE}
  szOID_ANY_ENHANCED_KEY_USAGE    = '2.5.29.37.0';

// szOID_CRL_NUMBER -- Base CRLs only.  Monotonically increasing sequence
// number for each CRL issued by a CA.
  {$EXTERNALSYM szOID_CRL_NUMBER}
  szOID_CRL_NUMBER                = '2.5.29.20';
// szOID_DELTA_CRL_INDICATOR -- Delta CRLs only.  Marked critical.
// Contains the minimum base CRL Number that can be used with a delta CRL.
  {$EXTERNALSYM szOID_DELTA_CRL_INDICATOR}
  szOID_DELTA_CRL_INDICATOR       = '2.5.29.27';
  {$EXTERNALSYM szOID_ISSUING_DIST_POINT}
  szOID_ISSUING_DIST_POINT        = '2.5.29.28';
// szOID_FRESHEST_CRL -- Base CRLs only.  Formatted identically to a CDP
// extension that holds URLs to fetch the delta CRL.
  {$EXTERNALSYM szOID_FRESHEST_CRL}
  szOID_FRESHEST_CRL              = '2.5.29.46';
  {$EXTERNALSYM szOID_NAME_CONSTRAINTS}
  szOID_NAME_CONSTRAINTS          = '2.5.29.30';

// Note on 1/1/2000 szOID_POLICY_MAPPINGS was changed from '2.5.29.5'
  {$EXTERNALSYM szOID_POLICY_MAPPINGS}
  szOID_POLICY_MAPPINGS           = '2.5.29.33';
  {$EXTERNALSYM szOID_LEGACY_POLICY_MAPPINGS}
  szOID_LEGACY_POLICY_MAPPINGS    = '2.5.29.5';
  {$EXTERNALSYM szOID_POLICY_CONSTRAINTS}
  szOID_POLICY_CONSTRAINTS        = '2.5.29.36';

// Microsoft PKCS10 Attributes
  {$EXTERNALSYM szOID_RENEWAL_CERTIFICATE}
  szOID_RENEWAL_CERTIFICATE           = '1.3.6.1.4.1.311.13.1';
  {$EXTERNALSYM szOID_ENROLLMENT_NAME_VALUE_PAIR}
  szOID_ENROLLMENT_NAME_VALUE_PAIR    = '1.3.6.1.4.1.311.13.2.1';
  {$EXTERNALSYM szOID_ENROLLMENT_CSP_PROVIDER}
  szOID_ENROLLMENT_CSP_PROVIDER       = '1.3.6.1.4.1.311.13.2.2';
  {$EXTERNALSYM szOID_OS_VERSION}
  szOID_OS_VERSION                    = '1.3.6.1.4.1.311.13.2.3';

//
// Extension contain certificate type
  {$EXTERNALSYM szOID_ENROLLMENT_AGENT}
  szOID_ENROLLMENT_AGENT              = '1.3.6.1.4.1.311.20.2.1';

// Internet Public Key Infrastructure (PKIX)
  {$EXTERNALSYM szOID_PKIX}
  szOID_PKIX                          = '1.3.6.1.5.5.7';
  {$EXTERNALSYM szOID_PKIX_PE}
  szOID_PKIX_PE                       = '1.3.6.1.5.5.7.1';
  {$EXTERNALSYM szOID_AUTHORITY_INFO_ACCESS}
  szOID_AUTHORITY_INFO_ACCESS         = '1.3.6.1.5.5.7.1.1';
  {$EXTERNALSYM szOID_SUBJECT_INFO_ACCESS}
  szOID_SUBJECT_INFO_ACCESS           = '1.3.6.1.5.5.7.1.11';
  {$EXTERNALSYM szOID_BIOMETRIC_EXT}
  szOID_BIOMETRIC_EXT                 = '1.3.6.1.5.5.7.1.2';
  {$EXTERNALSYM szOID_QC_STATEMENTS_EXT}
  szOID_QC_STATEMENTS_EXT             = '1.3.6.1.5.5.7.1.3';
  {$EXTERNALSYM szOID_LOGOTYPE_EXT}
  szOID_LOGOTYPE_EXT                  = '1.3.6.1.5.5.7.1.12';

// Microsoft extensions or attributes
  {$EXTERNALSYM szOID_CERT_EXTENSIONS}
  szOID_CERT_EXTENSIONS               = '1.3.6.1.4.1.311.2.1.14';
  {$EXTERNALSYM szOID_NEXT_UPDATE_LOCATION}
  szOID_NEXT_UPDATE_LOCATION          = '1.3.6.1.4.1.311.10.2';
  {$EXTERNALSYM szOID_REMOVE_CERTIFICATE}
  szOID_REMOVE_CERTIFICATE            = '1.3.6.1.4.1.311.10.8.1';
  {$EXTERNALSYM szOID_CROSS_CERT_DIST_POINTS}
  szOID_CROSS_CERT_DIST_POINTS        = '1.3.6.1.4.1.311.10.9.1';

//  Microsoft PKCS #7 ContentType Object Identifiers
  {$EXTERNALSYM szOID_CTL}
  szOID_CTL                           = '1.3.6.1.4.1.311.10.1';

//  Microsoft Sorted CTL Extension Object Identifier
  {$EXTERNALSYM szOID_SORTED_CTL}
  szOID_SORTED_CTL                    = '1.3.6.1.4.1.311.10.1.1';

// serialized serial numbers for PRS
  {$EXTERNALSYM szOID_SERIALIZED}
  szOID_SERIALIZED                    = '1.3.6.1.4.1.311.10.3.3.1';

// UPN principal name in SubjectAltName
  {$EXTERNALSYM szOID_NT_PRINCIPAL_NAME}
  szOID_NT_PRINCIPAL_NAME             = '1.3.6.1.4.1.311.20.2.3';

// Windows product update unauthenticated attribute
  {$EXTERNALSYM szOID_PRODUCT_UPDATE}
  szOID_PRODUCT_UPDATE                = '1.3.6.1.4.1.311.31.1';

// CryptUI
  {$EXTERNALSYM szOID_ANY_APPLICATION_POLICY}
  szOID_ANY_APPLICATION_POLICY        = '1.3.6.1.4.1.311.10.12.1';

//+-------------------------------------------------------------------------
//  Object Identifiers for use with Auto Enrollment
//--------------------------------------------------------------------------
  {$EXTERNALSYM szOID_AUTO_ENROLL_CTL_USAGE}
  szOID_AUTO_ENROLL_CTL_USAGE             = '1.3.6.1.4.1.311.20.1';

// Extension contain certificate type
// AKA Certificate template extension (v1)
  {$EXTERNALSYM szOID_ENROLL_CERTTYPE_EXTENSION}
  szOID_ENROLL_CERTTYPE_EXTENSION         = '1.3.6.1.4.1.311.20.2';

  {$EXTERNALSYM szOID_CERT_MANIFOLD}
  szOID_CERT_MANIFOLD                     = '1.3.6.1.4.1.311.20.3';

//+-------------------------------------------------------------------------
//  Object Identifiers for use with the MS Certificate Server
//--------------------------------------------------------------------------
  {$EXTERNALSYM szOID_CERTSRV_CA_VERSION}
  szOID_CERTSRV_CA_VERSION                = '1.3.6.1.4.1.311.21.1';

// szOID_CERTSRV_PREVIOUS_CERT_HASH -- Contains the sha1 hash of the previous
// version of the CA certificate.
  {$EXTERNALSYM szOID_CERTSRV_PREVIOUS_CERT_HASH}
  szOID_CERTSRV_PREVIOUS_CERT_HASH        = '1.3.6.1.4.1.311.21.2';

// szOID_CRL_VIRTUAL_BASE -- Delta CRLs only.  Contains the base CRL Number
// of the corresponding base CRL.
  {$EXTERNALSYM szOID_CRL_VIRTUAL_BASE}
  szOID_CRL_VIRTUAL_BASE                  = '1.3.6.1.4.1.311.21.3';

// szOID_CRL_NEXT_PUBLISH -- Contains the time when the next CRL is expected
// to be published.  This may be sooner than the CRL's NextUpdate field.
  {$EXTERNALSYM szOID_CRL_NEXT_PUBLISH}
  szOID_CRL_NEXT_PUBLISH                  = '1.3.6.1.4.1.311.21.4';

// Enhanced Key Usage for CA encryption certificate
  {$EXTERNALSYM szOID_KP_CA_EXCHANGE}
  szOID_KP_CA_EXCHANGE                    = '1.3.6.1.4.1.311.21.5';

// Enhanced Key Usage for key recovery agent certificate
  {$EXTERNALSYM szOID_KP_KEY_RECOVERY_AGENT}
  szOID_KP_KEY_RECOVERY_AGENT             = '1.3.6.1.4.1.311.21.6';

// Certificate template extension (v2)
  {$EXTERNALSYM szOID_CERTIFICATE_TEMPLATE}
  szOID_CERTIFICATE_TEMPLATE              = '1.3.6.1.4.1.311.21.7';

// The root oid for all enterprise specific oids
  {$EXTERNALSYM szOID_ENTERPRISE_OID_ROOT}
  szOID_ENTERPRISE_OID_ROOT               = '1.3.6.1.4.1.311.21.8';

// Dummy signing Subject RDN
  {$EXTERNALSYM szOID_RDN_DUMMY_SIGNER}
  szOID_RDN_DUMMY_SIGNER                  = '1.3.6.1.4.1.311.21.9';

// Application Policies extension -- same encoding as szOID_CERT_POLICIES
  {$EXTERNALSYM szOID_APPLICATION_CERT_POLICIES}
  szOID_APPLICATION_CERT_POLICIES         = '1.3.6.1.4.1.311.21.10';

// Application Policy Mappings -- same encoding as szOID_POLICY_MAPPINGS
  {$EXTERNALSYM szOID_APPLICATION_POLICY_MAPPINGS}
  szOID_APPLICATION_POLICY_MAPPINGS       = '1.3.6.1.4.1.311.21.11';

// Application Policy Constraints -- same encoding as szOID_POLICY_CONSTRAINTS
  {$EXTERNALSYM szOID_APPLICATION_POLICY_CONSTRAINTS}
  szOID_APPLICATION_POLICY_CONSTRAINTS    = '1.3.6.1.4.1.311.21.12';

  {$EXTERNALSYM szOID_ARCHIVED_KEY_ATTR}
  szOID_ARCHIVED_KEY_ATTR                 = '1.3.6.1.4.1.311.21.13';
  {$EXTERNALSYM szOID_CRL_SELF_CDP}
  szOID_CRL_SELF_CDP                      = '1.3.6.1.4.1.311.21.14';


// Requires all certificates below the root to have a non-empty intersecting
// issuance certificate policy usage.
  {$EXTERNALSYM szOID_REQUIRE_CERT_CHAIN_POLICY}
  szOID_REQUIRE_CERT_CHAIN_POLICY         = '1.3.6.1.4.1.311.21.15';
  {$EXTERNALSYM szOID_ARCHIVED_KEY_CERT_HASH}
  szOID_ARCHIVED_KEY_CERT_HASH            = '1.3.6.1.4.1.311.21.16';
  {$EXTERNALSYM szOID_ISSUED_CERT_HASH}
  szOID_ISSUED_CERT_HASH                  = '1.3.6.1.4.1.311.21.17';

// Enhanced key usage for DS email replication
  {$EXTERNALSYM szOID_DS_EMAIL_REPLICATION}
  szOID_DS_EMAIL_REPLICATION              = '1.3.6.1.4.1.311.21.19';

  {$EXTERNALSYM szOID_REQUEST_CLIENT_INFO}
  szOID_REQUEST_CLIENT_INFO               = '1.3.6.1.4.1.311.21.20';
  {$EXTERNALSYM szOID_ENCRYPTED_KEY_HASH}
  szOID_ENCRYPTED_KEY_HASH                = '1.3.6.1.4.1.311.21.21';
  {$EXTERNALSYM szOID_CERTSRV_CROSSCA_VERSION}
  szOID_CERTSRV_CROSSCA_VERSION           = '1.3.6.1.4.1.311.21.22';

//+-------------------------------------------------------------------------
//  Object Identifiers for use with the MS Directory Service
//--------------------------------------------------------------------------
  {$EXTERNALSYM szOID_NTDS_REPLICATION}
  szOID_NTDS_REPLICATION                  = '1.3.6.1.4.1.311.25.1';

//+-------------------------------------------------------------------------
//  Extension Object Identifiers (currently not implemented)
//--------------------------------------------------------------------------
  {$EXTERNALSYM szOID_SUBJECT_DIR_ATTRS}
  szOID_SUBJECT_DIR_ATTRS                 = '2.5.29.9';

//+-------------------------------------------------------------------------
//  Enhanced Key Usage (Purpose) Object Identifiers
//--------------------------------------------------------------------------
  {$EXTERNALSYM szOID_PKIX_KP}
  szOID_PKIX_KP                           = '1.3.6.1.5.5.7.3';

// Consistent key usage bits: DIGITAL_SIGNATURE, KEY_ENCIPHERMENT
// or KEY_AGREEMENT
  {$EXTERNALSYM szOID_PKIX_KP_SERVER_AUTH}
  szOID_PKIX_KP_SERVER_AUTH               = '1.3.6.1.5.5.7.3.1';

// Consistent key usage bits: DIGITAL_SIGNATURE
  {$EXTERNALSYM szOID_PKIX_KP_CLIENT_AUTH}
  szOID_PKIX_KP_CLIENT_AUTH               = '1.3.6.1.5.5.7.3.2';

// Consistent key usage bits: DIGITAL_SIGNATURE
  {$EXTERNALSYM szOID_PKIX_KP_CODE_SIGNING}
  szOID_PKIX_KP_CODE_SIGNING              = '1.3.6.1.5.5.7.3.3';

// Consistent key usage bits: DIGITAL_SIGNATURE, NON_REPUDIATION and/or
// (KEY_ENCIPHERMENT or KEY_AGREEMENT)
  {$EXTERNALSYM szOID_PKIX_KP_EMAIL_PROTECTION}
  szOID_PKIX_KP_EMAIL_PROTECTION          = '1.3.6.1.5.5.7.3.4';

// Consistent key usage bits: DIGITAL_SIGNATURE and/or
// (KEY_ENCIPHERMENT or KEY_AGREEMENT)
  {$EXTERNALSYM szOID_PKIX_KP_IPSEC_END_SYSTEM}
  szOID_PKIX_KP_IPSEC_END_SYSTEM          = '1.3.6.1.5.5.7.3.5';

// Consistent key usage bits: DIGITAL_SIGNATURE and/or
// (KEY_ENCIPHERMENT or KEY_AGREEMENT)
  {$EXTERNALSYM szOID_PKIX_KP_IPSEC_TUNNEL}
  szOID_PKIX_KP_IPSEC_TUNNEL              = '1.3.6.1.5.5.7.3.6';

// Consistent key usage bits: DIGITAL_SIGNATURE and/or
// (KEY_ENCIPHERMENT or KEY_AGREEMENT)
  {$EXTERNALSYM szOID_PKIX_KP_IPSEC_USER}
  szOID_PKIX_KP_IPSEC_USER                = '1.3.6.1.5.5.7.3.7';

// Consistent key usage bits: DIGITAL_SIGNATURE or NON_REPUDIATION
  {$EXTERNALSYM szOID_PKIX_KP_TIMESTAMP_SIGNING}
  szOID_PKIX_KP_TIMESTAMP_SIGNING         = '1.3.6.1.5.5.7.3.8';

// OCSP response signer
  {$EXTERNALSYM szOID_PKIX_KP_OCSP_SIGNING}
  szOID_PKIX_KP_OCSP_SIGNING              = '1.3.6.1.5.5.7.3.9';

// Following extension is present to indicate no revocation checking
// for the OCSP signer certificate
  {$EXTERNALSYM szOID_PKIX_OCSP_NOCHECK}
  szOID_PKIX_OCSP_NOCHECK                 = '1.3.6.1.5.5.7.48.1.5';

// OCSP Nonce
  {$EXTERNALSYM szOID_PKIX_OCSP_NONCE}
  szOID_PKIX_OCSP_NONCE                   = '1.3.6.1.5.5.7.48.1.2';

// IKE (Internet Key Exchange) Intermediate KP for an IPsec end entity.
// Defined in draft-ietf-ipsec-pki-req-04.txt, December 14, 1999.
  {$EXTERNALSYM szOID_IPSEC_KP_IKE_INTERMEDIATE}
  szOID_IPSEC_KP_IKE_INTERMEDIATE         = '1.3.6.1.5.5.8.2.2';

// iso (1) org (3) dod (6) internet (1) security (5) kerberosv5 (2) pkinit (3) 5
  {$EXTERNALSYM szOID_PKINIT_KP_KDC}
  szOID_PKINIT_KP_KDC                     = '1.3.6.1.5.2.3.5';

//+-------------------------------------------------------------------------
//  Microsoft Enhanced Key Usage (Purpose) Object Identifiers
//+-------------------------------------------------------------------------

//  Signer of CTLs
  {$EXTERNALSYM szOID_KP_CTL_USAGE_SIGNING}
  szOID_KP_CTL_USAGE_SIGNING              = '1.3.6.1.4.1.311.10.3.1';

//  Signer of TimeStamps
  {$EXTERNALSYM szOID_KP_TIME_STAMP_SIGNING}
  szOID_KP_TIME_STAMP_SIGNING             = '1.3.6.1.4.1.311.10.3.2';

  {$EXTERNALSYM szOID_SERVER_GATED_CRYPTO}
  szOID_SERVER_GATED_CRYPTO               = '1.3.6.1.4.1.311.10.3.3';

  {$EXTERNALSYM szOID_SGC_NETSCAPE}
  szOID_SGC_NETSCAPE                      = '2.16.840.1.113730.4.1';

  {$EXTERNALSYM szOID_KP_EFS}
  szOID_KP_EFS                            = '1.3.6.1.4.1.311.10.3.4';
  {$EXTERNALSYM szOID_EFS_RECOVERY}
  szOID_EFS_RECOVERY                      = '1.3.6.1.4.1.311.10.3.4.1';

// Can use Windows Hardware Compatible (WHQL)
  {$EXTERNALSYM szOID_WHQL_CRYPTO}
  szOID_WHQL_CRYPTO                       = '1.3.6.1.4.1.311.10.3.5';

// Signed by the NT5 build lab
  {$EXTERNALSYM szOID_NT5_CRYPTO}
  szOID_NT5_CRYPTO                        = '1.3.6.1.4.1.311.10.3.6';

// Signed by and OEM of WHQL
  {$EXTERNALSYM szOID_OEM_WHQL_CRYPTO}
  szOID_OEM_WHQL_CRYPTO                   = '1.3.6.1.4.1.311.10.3.7';

// Signed by the Embedded NT
  {$EXTERNALSYM szOID_EMBEDDED_NT_CRYPTO}
  szOID_EMBEDDED_NT_CRYPTO                = '1.3.6.1.4.1.311.10.3.8';

// Signer of a CTL containing trusted roots
  {$EXTERNALSYM szOID_ROOT_LIST_SIGNER}
  szOID_ROOT_LIST_SIGNER                  = '1.3.6.1.4.1.311.10.3.9';

// Can sign cross-cert and subordinate CA requests with qualified
// subordination (name constraints, policy mapping, etc.)
  {$EXTERNALSYM szOID_KP_QUALIFIED_SUBORDINATION}
  szOID_KP_QUALIFIED_SUBORDINATION        = '1.3.6.1.4.1.311.10.3.10';

// Can be used to encrypt/recover escrowed keys
  {$EXTERNALSYM szOID_KP_KEY_RECOVERY}
  szOID_KP_KEY_RECOVERY                   = '1.3.6.1.4.1.311.10.3.11';

// Signer of documents
  {$EXTERNALSYM szOID_KP_DOCUMENT_SIGNING}
  szOID_KP_DOCUMENT_SIGNING               = '1.3.6.1.4.1.311.10.3.12';

// The default WinVerifyTrust Authenticode policy is to treat all time stamped
// signatures as being valid forever. This OID limits the valid lifetime of the
// signature to the lifetime of the certificate. This allows timestamped
// signatures to expire. Normally this OID will be used in conjunction with
// szOID_PKIX_KP_CODE_SIGNING to indicate new time stamp semantics should be
// used. Support for this OID was added in WXP.
  {$EXTERNALSYM szOID_KP_LIFETIME_SIGNING}
  szOID_KP_LIFETIME_SIGNING               = '1.3.6.1.4.1.311.10.3.13';

  {$EXTERNALSYM szOID_KP_MOBILE_DEVICE_SOFTWARE}
  szOID_KP_MOBILE_DEVICE_SOFTWARE         = '1.3.6.1.4.1.311.10.3.14';

  {$EXTERNALSYM szOID_KP_SMART_DISPLAY}
  szOID_KP_SMART_DISPLAY                  = '1.3.6.1.4.1.311.10.3.15';

  {$EXTERNALSYM szOID_KP_CSP_SIGNATURE}
  szOID_KP_CSP_SIGNATURE                  = '1.3.6.1.4.1.311.10.3.16';

  {$EXTERNALSYM szOID_DRM}
  szOID_DRM                               = '1.3.6.1.4.1.311.10.5.1';

// Microsoft DRM EKU
  {$EXTERNALSYM szOID_DRM_INDIVIDUALIZATION}
  szOID_DRM_INDIVIDUALIZATION             = '1.3.6.1.4.1.311.10.5.2';

  {$EXTERNALSYM szOID_LICENSES}
  szOID_LICENSES                          = '1.3.6.1.4.1.311.10.6.1';

  {$EXTERNALSYM szOID_LICENSE_SERVER}
  szOID_LICENSE_SERVER                    = '1.3.6.1.4.1.311.10.6.2';

  {$EXTERNALSYM szOID_KP_SMARTCARD_LOGON}
  szOID_KP_SMARTCARD_LOGON                = '1.3.6.1.4.1.311.20.2.2';

  {$EXTERNALSYM szOID_KP_KERNEL_MODE_CODE_SIGNING}
  szOID_KP_KERNEL_MODE_CODE_SIGNING       = '1.3.6.1.4.1.311.61.1.1';

//+-------------------------------------------------------------------------
//  Microsoft Attribute Object Identifiers
//+-------------------------------------------------------------------------
  {$EXTERNALSYM szOID_YESNO_TRUST_ATTR}
  szOID_YESNO_TRUST_ATTR                  = '1.3.6.1.4.1.311.10.4.1';

//+-------------------------------------------------------------------------
//  Qualifiers that may be part of the szOID_CERT_POLICIES and
//  szOID_CERT_POLICIES95 extensions
//+-------------------------------------------------------------------------
  {$EXTERNALSYM szOID_PKIX_POLICY_QUALIFIER_CPS}
  szOID_PKIX_POLICY_QUALIFIER_CPS         = '1.3.6.1.5.5.7.2.1';
  {$EXTERNALSYM szOID_PKIX_POLICY_QUALIFIER_USERNOTICE}
  szOID_PKIX_POLICY_QUALIFIER_USERNOTICE  = '1.3.6.1.5.5.7.2.2';

  {$EXTERNALSYM szOID_ROOT_PROGRAM_FLAGS}
  szOID_ROOT_PROGRAM_FLAGS                = '1.3.6.1.4.1.311.60.1.1';

// OID for old qualifer
  {$EXTERNALSYM szOID_CERT_POLICIES_95_QUALIFIER1}
  szOID_CERT_POLICIES_95_QUALIFIER1       = '2.16.840.1.113733.1.7.1.1';

//+-------------------------------------------------------------------------
//  X509_CERT
//
//  The 'to be signed' encoded content plus its signature. The ToBeSigned
//  content is the CryptEncodeObject() output for one of the following:
//  X509_CERT_TO_BE_SIGNED, X509_CERT_CRL_TO_BE_SIGNED or
//  X509_CERT_REQUEST_TO_BE_SIGNED.
//
//  pvStructInfo points to CERT_SIGNED_CONTENT_INFO.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  X509_CERT_TO_BE_SIGNED
//
//  pvStructInfo points to CERT_INFO.
//
//  For CryptDecodeObject(), the pbEncoded is the 'to be signed' plus its
//  signature (output of a X509_CERT CryptEncodeObject()).
//
//  For CryptEncodeObject(), the pbEncoded is just the 'to be signed'.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  X509_CERT_CRL_TO_BE_SIGNED
//
//  pvStructInfo points to CRL_INFO.
//
//  For CryptDecodeObject(), the pbEncoded is the 'to be signed' plus its
//  signature (output of a X509_CERT CryptEncodeObject()).
//
//  For CryptEncodeObject(), the pbEncoded is just the 'to be signed'.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  X509_CERT_REQUEST_TO_BE_SIGNED
//
//  pvStructInfo points to CERT_REQUEST_INFO.
//
//  For CryptDecodeObject(), the pbEncoded is the 'to be signed' plus its
//  signature (output of a X509_CERT CryptEncodeObject()).
//
//  For CryptEncodeObject(), the pbEncoded is just the 'to be signed'.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  X509_EXTENSIONS
//  szOID_CERT_EXTENSIONS
//
//  pvStructInfo points to following CERT_EXTENSIONS.
//--------------------------------------------------------------------------
// certenrolls_begin -- CERT_CONTEXTS
type
  PCertExtensions = ^TCertExtensions;
  {$EXTERNALSYM _CERT_EXTENSIONS}
  _CERT_EXTENSIONS = record
    cExtension: DWORD;
    rgExtension: PCertExtension;
  end;
  {$EXTERNALSYM CERT_EXTENSIONS}
  CERT_EXTENSIONS = _CERT_EXTENSIONS;
  {$EXTERNALSYM PCERT_EXTENSIONS}
  PCERT_EXTENSIONS = ^_CERT_EXTENSIONS;
  TCertExtensions = _CERT_EXTENSIONS;
// certenrolls_end

//+-------------------------------------------------------------------------
//  X509_NAME_VALUE
//  X509_ANY_STRING
//
//  pvStructInfo points to CERT_NAME_VALUE.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  X509_UNICODE_NAME_VALUE
//  X509_UNICODE_ANY_STRING
//
//  pvStructInfo points to CERT_NAME_VALUE.
//
//  The name values are unicode strings.
//
//  For CryptEncodeObject:
//    Value.pbData points to the unicode string.
//    If Value.cbData = 0, then, the unicode string is NULL terminated.
//    Otherwise, Value.cbData is the unicode string byte count. The byte count
//    is twice the character count.
//
//    If the unicode string contains an invalid character for the specified
//    dwValueType, then, *pcbEncoded is updated with the unicode character
//    index of the first invalid character. LastError is set to:
//    CRYPT_E_INVALID_NUMERIC_STRING, CRYPT_E_INVALID_PRINTABLE_STRING or
//    CRYPT_E_INVALID_IA5_STRING.
//
//    To disable the above check, either set CERT_RDN_DISABLE_CHECK_TYPE_FLAG
//    in dwValueType or set CRYPT_UNICODE_NAME_ENCODE_DISABLE_CHECK_TYPE_FLAG
//    in dwFlags passed to CryptEncodeObjectEx.
//
//    The unicode string is converted before being encoded according to
//    the specified dwValueType. If dwValueType is set to 0, LastError
//    is set to E_INVALIDARG.
//
//    If the dwValueType isn't one of the character strings (its a
//    CERT_RDN_ENCODED_BLOB or CERT_RDN_OCTET_STRING), then, CryptEncodeObject
//    will return FALSE with LastError set to CRYPT_E_NOT_CHAR_STRING.
//
//  For CryptDecodeObject:
//    Value.pbData points to a NULL terminated unicode string. Value.cbData
//    contains the byte count of the unicode string excluding the NULL
//    terminator. dwValueType contains the type used in the encoded object.
//    Its not forced to CERT_RDN_UNICODE_STRING. The encoded value is
//    converted to the unicode string according to the dwValueType.
//
//    If the encoded object isn't one of the character string types, then,
//    CryptDecodeObject will return FALSE with LastError set to
//    CRYPT_E_NOT_CHAR_STRING. For a non character string, decode using
//    X509_NAME_VALUE or X509_ANY_STRING.
//
//    By default, CERT_RDN_T61_STRING values are initially decoded
//    as UTF8. If the UTF8 decoding fails, then, decoded as 8 bit characters.
//    Set CRYPT_UNICODE_NAME_DECODE_DISABLE_IE4_UTF8_FLAG in dwFlags
//    passed to either CryptDecodeObject or CryptDecodeObjectEx to
//    skip the initial attempt to decode as UTF8.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  X509_NAME
//
//  pvStructInfo points to CERT_NAME_INFO.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  X509_UNICODE_NAME
//
//  pvStructInfo points to CERT_NAME_INFO.
//
//  The RDN attribute values are unicode strings except for the dwValueTypes of
//  CERT_RDN_ENCODED_BLOB or CERT_RDN_OCTET_STRING. These dwValueTypes are
//  the same as for a X509_NAME. Their values aren't converted to/from unicode.
//
//  For CryptEncodeObject:
//    Value.pbData points to the unicode string.
//    If Value.cbData = 0, then, the unicode string is NULL terminated.
//    Otherwise, Value.cbData is the unicode string byte count. The byte count
//    is twice the character count.
//
//    If dwValueType = 0 (CERT_RDN_ANY_TYPE), the pszObjId is used to find
//    an acceptable dwValueType. If the unicode string contains an
//    invalid character for the found or specified dwValueType, then,
//    *pcbEncoded is updated with the error location of the invalid character.
//    See below for details. LastError is set to:
//    CRYPT_E_INVALID_NUMERIC_STRING, CRYPT_E_INVALID_PRINTABLE_STRING or
//    CRYPT_E_INVALID_IA5_STRING.
//
//    To disable the above check, either set CERT_RDN_DISABLE_CHECK_TYPE_FLAG
//    in dwValueType or set CRYPT_UNICODE_NAME_ENCODE_DISABLE_CHECK_TYPE_FLAG
//    in dwFlags passed to CryptEncodeObjectEx.
//
//    Set CERT_RDN_UNICODE_STRING in dwValueType or set
//    CRYPT_UNICODE_NAME_ENCODE_ENABLE_T61_UNICODE_FLAG in dwFlags passed
//    to CryptEncodeObjectEx to select CERT_RDN_T61_STRING instead of
//    CERT_RDN_UNICODE_STRING if all the unicode characters are <= $FF.
//
//    Set CERT_RDN_ENABLE_UTF8_UNICODE_STRING in dwValueType or set
//    CRYPT_UNICODE_NAME_ENCODE_ENABLE_UTF8_UNICODE_FLAG in dwFlags passed
//    to CryptEncodeObjectEx to select CERT_RDN_UTF8_STRING instead of
//    CERT_RDN_UNICODE_STRING.
//
//    The unicode string is converted before being encoded according to
//    the specified or ObjId matching dwValueType.
//
//  For CryptDecodeObject:
//    Value.pbData points to a NULL terminated unicode string. Value.cbData
//    contains the byte count of the unicode string excluding the NULL
//    terminator. dwValueType contains the type used in the encoded object.
//    Its not forced to CERT_RDN_UNICODE_STRING. The encoded value is
//    converted to the unicode string according to the dwValueType.
//
//    If the dwValueType of the encoded value isn't a character string
//    type, then, it isn't converted to UNICODE. Use the
//    IS_CERT_RDN_CHAR_STRING() macro on the dwValueType to check
//    that Value.pbData points to a converted unicode string.
//
//    By default, CERT_RDN_T61_STRING values are initially decoded
//    as UTF8. If the UTF8 decoding fails, then, decoded as 8 bit characters.
//    Set CRYPT_UNICODE_NAME_DECODE_DISABLE_IE4_UTF8_FLAG in dwFlags
//    passed to either CryptDecodeObject or CryptDecodeObjectEx to
//    skip the initial attempt to decode as UTF8.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  Unicode Name Value Error Location Definitions
//
//  Error location is returned in *pcbEncoded by
//  CryptEncodeObject(X509_UNICODE_NAME)
//
//  Error location consists of:
//    RDN_INDEX     - 10 bits shl 22
//    ATTR_INDEX    - 6 bits shl 16
//    VALUE_INDEX   - 16 bits (unicode character index)
//--------------------------------------------------------------------------
const
  {$EXTERNALSYM CERT_UNICODE_RDN_ERR_INDEX_MASK}
  CERT_UNICODE_RDN_ERR_INDEX_MASK = $3FF;
  {$EXTERNALSYM CERT_UNICODE_RDN_ERR_INDEX_SHIFT}
  CERT_UNICODE_RDN_ERR_INDEX_SHIFT = 22;
  {$EXTERNALSYM CERT_UNICODE_ATTR_ERR_INDEX_MASK}
  CERT_UNICODE_ATTR_ERR_INDEX_MASK = $003F;
  {$EXTERNALSYM CERT_UNICODE_ATTR_ERR_INDEX_SHIFT}
  CERT_UNICODE_ATTR_ERR_INDEX_SHIFT = 16;
  {$EXTERNALSYM CERT_UNICODE_VALUE_ERR_INDEX_MASK}
  CERT_UNICODE_VALUE_ERR_INDEX_MASK = $0000FFFF;
  {$EXTERNALSYM CERT_UNICODE_VALUE_ERR_INDEX_SHIFT}
  CERT_UNICODE_VALUE_ERR_INDEX_SHIFT = 0;

{$EXTERNALSYM GET_CERT_UNICODE_RDN_ERR_INDEX}
function GET_CERT_UNICODE_RDN_ERR_INDEX(X: DWORD): DWORD; {$IfDef HAS_INLINE}inline;{$EndIf}

{$EXTERNALSYM GET_CERT_UNICODE_ATTR_ERR_INDEX}
function GET_CERT_UNICODE_ATTR_ERR_INDEX(X: DWORD): DWORD; {$IfDef HAS_INLINE}inline;{$EndIf}

{$EXTERNALSYM GET_CERT_UNICODE_VALUE_ERR_INDEX}
function GET_CERT_UNICODE_VALUE_ERR_INDEX(X: DWORD): DWORD; {$IfDef HAS_INLINE}inline;{$EndIf}

//+-------------------------------------------------------------------------
//  X509_PUBLIC_KEY_INFO
//
//  pvStructInfo points to CERT_PUBLIC_KEY_INFO.
//--------------------------------------------------------------------------


//+-------------------------------------------------------------------------
//  X509_AUTHORITY_KEY_ID
//  szOID_AUTHORITY_KEY_IDENTIFIER
//
//  pvStructInfo points to following CERT_AUTHORITY_KEY_ID_INFO.
//--------------------------------------------------------------------------
type
  PCertAuthorityKeyIDInfo = ^TCertAuthorityKeyIDInfo;
  {$EXTERNALSYM _CERT_AUTHORITY_KEY_ID_INFO}
  _CERT_AUTHORITY_KEY_ID_INFO = record
    KeyId: CRYPT_DATA_BLOB;
    CertIssuer: CERT_NAME_BLOB;
    CertSerialNumber: CRYPT_INTEGER_BLOB;
  end;
  {$EXTERNALSYM CERT_AUTHORITY_KEY_ID_INFO}
  CERT_AUTHORITY_KEY_ID_INFO = _CERT_AUTHORITY_KEY_ID_INFO;
  {$EXTERNALSYM PCERT_AUTHORITY_KEY_ID_INFO}
  PCERT_AUTHORITY_KEY_ID_INFO = ^_CERT_AUTHORITY_KEY_ID_INFO;
  TCertAuthorityKeyIDInfo = _CERT_AUTHORITY_KEY_ID_INFO;

//+-------------------------------------------------------------------------
//  X509_KEY_ATTRIBUTES
//  szOID_KEY_ATTRIBUTES
//
//  pvStructInfo points to following CERT_KEY_ATTRIBUTES_INFO.
//--------------------------------------------------------------------------
  PCertPrivateKeyValidity = ^TCertPrivateKeyValidity;
  {$EXTERNALSYM _CERT_PRIVATE_KEY_VALIDITY}
  _CERT_PRIVATE_KEY_VALIDITY = record
    NotBefore: FILETIME;
    NotAfter: FILETIME;
  end;
  {$EXTERNALSYM CERT_PRIVATE_KEY_VALIDITY}
  CERT_PRIVATE_KEY_VALIDITY = _CERT_PRIVATE_KEY_VALIDITY;
  {$EXTERNALSYM PCERT_PRIVATE_KEY_VALIDITY}
  PCERT_PRIVATE_KEY_VALIDITY = ^_CERT_PRIVATE_KEY_VALIDITY;
  TCertPrivateKeyValidity = _CERT_PRIVATE_KEY_VALIDITY;

  PCertKeyAttributesInfo = ^TCertKeyAttributesInfo;
  {$EXTERNALSYM _CERT_KEY_ATTRIBUTES_INFO}
  _CERT_KEY_ATTRIBUTES_INFO = record
    KeyId: CRYPT_DATA_BLOB;
    IntendedKeyUsage: CRYPT_BIT_BLOB;
    pPrivateKeyUsagePeriod: PCertPrivateKeyValidity;     // OPTIONAL
  end;
  {$EXTERNALSYM CERT_KEY_ATTRIBUTES_INFO}
  CERT_KEY_ATTRIBUTES_INFO = _CERT_KEY_ATTRIBUTES_INFO;
  {$EXTERNALSYM PCERT_KEY_ATTRIBUTES_INFO}
  PCERT_KEY_ATTRIBUTES_INFO = ^_CERT_KEY_ATTRIBUTES_INFO;
  TCertKeyAttributesInfo = _CERT_KEY_ATTRIBUTES_INFO;

// certenrolld_begin -- CERT_*_KEY_USAGE
const
// Byte[0]
  {$EXTERNALSYM CERT_DIGITAL_SIGNATURE_KEY_USAGE}
  CERT_DIGITAL_SIGNATURE_KEY_USAGE = $80;
  {$EXTERNALSYM CERT_NON_REPUDIATION_KEY_USAGE}
  CERT_NON_REPUDIATION_KEY_USAGE = $40;
  {$EXTERNALSYM CERT_KEY_ENCIPHERMENT_KEY_USAGE}
  CERT_KEY_ENCIPHERMENT_KEY_USAGE = $20;
  {$EXTERNALSYM CERT_DATA_ENCIPHERMENT_KEY_USAGE}
  CERT_DATA_ENCIPHERMENT_KEY_USAGE = $10;
  {$EXTERNALSYM CERT_KEY_AGREEMENT_KEY_USAGE}
  CERT_KEY_AGREEMENT_KEY_USAGE = $08;
  {$EXTERNALSYM CERT_KEY_CERT_SIGN_KEY_USAGE}
  CERT_KEY_CERT_SIGN_KEY_USAGE = $04;
  {$EXTERNALSYM CERT_OFFLINE_CRL_SIGN_KEY_USAGE}
  CERT_OFFLINE_CRL_SIGN_KEY_USAGE = $02;
  {$EXTERNALSYM CERT_CRL_SIGN_KEY_USAGE}
  CERT_CRL_SIGN_KEY_USAGE = $02;
  {$EXTERNALSYM CERT_ENCIPHER_ONLY_KEY_USAGE}
  CERT_ENCIPHER_ONLY_KEY_USAGE = $01;
// Byte[1]
  {$EXTERNALSYM CERT_DECIPHER_ONLY_KEY_USAGE}
  CERT_DECIPHER_ONLY_KEY_USAGE = $80;
// certenrolld_end

//+-------------------------------------------------------------------------
//  X509_KEY_USAGE_RESTRICTION
//  szOID_KEY_USAGE_RESTRICTION
//
//  pvStructInfo points to following CERT_KEY_USAGE_RESTRICTION_INFO.
//--------------------------------------------------------------------------
type
  PCertPolicyID = ^TCertPolicyID;
  {$EXTERNALSYM _CERT_POLICY_ID}
  _CERT_POLICY_ID = record
    cCertPolicyElementId: DWORD;
    rgpszCertPolicyElementId: PLPSTR;  // pszObjId
  end;
  {$EXTERNALSYM CERT_POLICY_ID}
  CERT_POLICY_ID = _CERT_POLICY_ID;
  {$EXTERNALSYM PCERT_POLICY_ID}
  PCERT_POLICY_ID = ^_CERT_POLICY_ID;
  TCertPolicyID = _CERT_POLICY_ID;

  PCertKeyUsageRestrictionInfo = ^TCertKeyUsageRestrictionInfo;
  {$EXTERNALSYM _CERT_KEY_USAGE_RESTRICTION_INFO}
  _CERT_KEY_USAGE_RESTRICTION_INFO = record
    cCertPolicyId: DWORD;
    rgCertPolicyId: PCertPolicyID;
    RestrictedKeyUsage: CRYPT_BIT_BLOB;
  end;
  {$EXTERNALSYM CERT_KEY_USAGE_RESTRICTION_INFO}
  CERT_KEY_USAGE_RESTRICTION_INFO = _CERT_KEY_USAGE_RESTRICTION_INFO;
  {$EXTERNALSYM PCERT_KEY_USAGE_RESTRICTION_INFO}
  PCERT_KEY_USAGE_RESTRICTION_INFO = ^_CERT_KEY_USAGE_RESTRICTION_INFO;
  TCertKeyUsageRestrictionInfo = _CERT_KEY_USAGE_RESTRICTION_INFO;

// See CERT_KEY_ATTRIBUTES_INFO for definition of the RestrictedKeyUsage bits

//+-------------------------------------------------------------------------
//  X509_ALTERNATE_NAME
//  szOID_SUBJECT_ALT_NAME
//  szOID_ISSUER_ALT_NAME
//  szOID_SUBJECT_ALT_NAME2
//  szOID_ISSUER_ALT_NAME2
//
//  pvStructInfo points to following CERT_ALT_NAME_INFO.
//--------------------------------------------------------------------------

// certenrolls_begin -- CERT_ALT_NAME_INFO
  PCertOtherName = ^TCertOtherName;
  {$EXTERNALSYM _CERT_OTHER_NAME}
  _CERT_OTHER_NAME = record
    pszObjId: LPSTR;
    Value: CRYPT_OBJID_BLOB;
  end;
  {$EXTERNALSYM CERT_OTHER_NAME}
  CERT_OTHER_NAME = _CERT_OTHER_NAME;
  {$EXTERNALSYM PCERT_OTHER_NAME}
  PCERT_OTHER_NAME = ^_CERT_OTHER_NAME;
  TCertOtherName = _CERT_OTHER_NAME;

  PCertAltNameEntry = ^TCertAltNameEntry;
  {$EXTERNALSYM _CERT_ALT_NAME_ENTRY}
  _CERT_ALT_NAME_ENTRY = record
    case dwAltNameChoice: DWORD of
      1: (pOtherName: PCertOtherName);    // 1
      2: (pwszRfc822Name: LPWSTR);        // 2  (encoded IA5)
      3: (pwszDNSName: LPWSTR);           // 3  (encoded IA5)
      // Not implemented          x400Address;        // 4
      5: (DirectoryName: CERT_NAME_BLOB); // 5
      // Not implemented          pEdiPartyName;      // 6
      7: (pwszURL: LPWSTR);               // 7  (encoded IA5)
      8: (IPAddress: CRYPT_DATA_BLOB);    // 8  (Octet String)
      9: (pszRegisteredID: LPSTR);        // 9  (Object Identifer)
  end;
  {$EXTERNALSYM CERT_ALT_NAME_ENTRY}
  CERT_ALT_NAME_ENTRY = _CERT_ALT_NAME_ENTRY;
  {$EXTERNALSYM PCERT_ALT_NAME_ENTRY}
  PCERT_ALT_NAME_ENTRY = ^_CERT_ALT_NAME_ENTRY;
  TCertAltNameEntry = _CERT_ALT_NAME_ENTRY;
// certenrolls_end

// certenrolld_begin -- CERT_ALT_NAME_*
const
  {$EXTERNALSYM CERT_ALT_NAME_OTHER_NAME}
  CERT_ALT_NAME_OTHER_NAME = 1;
  {$EXTERNALSYM CERT_ALT_NAME_RFC822_NAME}
  CERT_ALT_NAME_RFC822_NAME = 2;
  {$EXTERNALSYM CERT_ALT_NAME_DNS_NAME}
  CERT_ALT_NAME_DNS_NAME = 3;
  {$EXTERNALSYM CERT_ALT_NAME_X400_ADDRESS}
  CERT_ALT_NAME_X400_ADDRESS = 4;
  {$EXTERNALSYM CERT_ALT_NAME_DIRECTORY_NAME}
  CERT_ALT_NAME_DIRECTORY_NAME = 5;
  {$EXTERNALSYM CERT_ALT_NAME_EDI_PARTY_NAME}
  CERT_ALT_NAME_EDI_PARTY_NAME = 6;
  {$EXTERNALSYM CERT_ALT_NAME_URL}
  CERT_ALT_NAME_URL = 7;
  {$EXTERNALSYM CERT_ALT_NAME_IP_ADDRESS}
  CERT_ALT_NAME_IP_ADDRESS = 8;
  {$EXTERNALSYM CERT_ALT_NAME_REGISTERED_ID}
  CERT_ALT_NAME_REGISTERED_ID = 9;
// certenrolld_end

// certenrolls_begin -- CERT_ALT_NAME_INFO
type
  PCertAltNameInfo = ^TCertAltNameInfo;
  {$EXTERNALSYM _CERT_ALT_NAME_INFO}
  _CERT_ALT_NAME_INFO = record
    cAltEntry: DWORD;
    rgAltEntry: PCertAltNameEntry;
  end;
  {$EXTERNALSYM CERT_ALT_NAME_INFO}
  CERT_ALT_NAME_INFO = _CERT_ALT_NAME_INFO;
  {$EXTERNALSYM PCERT_ALT_NAME_INFO}
  PCERT_ALT_NAME_INFO = ^_CERT_ALT_NAME_INFO;
  TCertAltNameInfo = _CERT_ALT_NAME_INFO;
// certenrolls_end

//+-------------------------------------------------------------------------
//  Alternate name IA5 Error Location Definitions for
//  CRYPT_E_INVALID_IA5_STRING.
//
//  Error location is returned in *pcbEncoded by
//  CryptEncodeObject(X509_ALTERNATE_NAME)
//
//  Error location consists of:
//    ENTRY_INDEX   - 8 bits shl 16
//    VALUE_INDEX   - 16 bits (unicode character index)
//--------------------------------------------------------------------------
const
  {$EXTERNALSYM CERT_ALT_NAME_ENTRY_ERR_INDEX_MASK}
  CERT_ALT_NAME_ENTRY_ERR_INDEX_MASK = $FF;
  {$EXTERNALSYM CERT_ALT_NAME_ENTRY_ERR_INDEX_SHIFT}
  CERT_ALT_NAME_ENTRY_ERR_INDEX_SHIFT = 16;
  {$EXTERNALSYM CERT_ALT_NAME_VALUE_ERR_INDEX_MASK}
  CERT_ALT_NAME_VALUE_ERR_INDEX_MASK = $0000FFFF;
  {$EXTERNALSYM CERT_ALT_NAME_VALUE_ERR_INDEX_SHIFT}
  CERT_ALT_NAME_VALUE_ERR_INDEX_SHIFT = 0;

{$EXTERNALSYM GET_CERT_ALT_NAME_ENTRY_ERR_INDEX}
function GET_CERT_ALT_NAME_ENTRY_ERR_INDEX(X: DWORD): DWORD; {$IfDef HAS_INLINE}inline;{$EndIf}

{$EXTERNALSYM GET_CERT_ALT_NAME_VALUE_ERR_INDEX}
function GET_CERT_ALT_NAME_VALUE_ERR_INDEX(X: DWORD): DWORD; {$IfDef HAS_INLINE}inline;{$EndIf}

//+-------------------------------------------------------------------------
//  X509_BASIC_CONSTRAINTS
//  szOID_BASIC_CONSTRAINTS
//
//  pvStructInfo points to following CERT_BASIC_CONSTRAINTS_INFO.
//--------------------------------------------------------------------------
type
  PCertBasicConstraintsInfo = ^TCertBasicConstraintsInfo;
  {$EXTERNALSYM _CERT_BASIC_CONSTRAINTS_INFO}
  _CERT_BASIC_CONSTRAINTS_INFO = record
    SubjectType: CRYPT_BIT_BLOB;
    fPathLenConstraint: BOOL;
    dwPathLenConstraint: DWORD;
    cSubtreesConstraint: DWORD;
    rgSubtreesConstraint: PCertNameBlob;
  end;
  {$EXTERNALSYM CERT_BASIC_CONSTRAINTS_INFO}
  CERT_BASIC_CONSTRAINTS_INFO = _CERT_BASIC_CONSTRAINTS_INFO;
  {$EXTERNALSYM PCERT_BASIC_CONSTRAINTS_INFO}
  PCERT_BASIC_CONSTRAINTS_INFO = ^_CERT_BASIC_CONSTRAINTS_INFO;
  TCertBasicConstraintsInfo = _CERT_BASIC_CONSTRAINTS_INFO;

const
  {$EXTERNALSYM CERT_CA_SUBJECT_FLAG}
  CERT_CA_SUBJECT_FLAG = $80;
  {$EXTERNALSYM CERT_END_ENTITY_SUBJECT_FLAG}
  CERT_END_ENTITY_SUBJECT_FLAG = $40;

//+-------------------------------------------------------------------------
//  X509_BASIC_CONSTRAINTS2
//  szOID_BASIC_CONSTRAINTS2
//
//  pvStructInfo points to following CERT_BASIC_CONSTRAINTS2_INFO.
//--------------------------------------------------------------------------
type
  PCertBasicConstraints2Info = ^TCertBasicConstraints2Info;
  {$EXTERNALSYM _CERT_BASIC_CONSTRAINTS2_INFO}
  _CERT_BASIC_CONSTRAINTS2_INFO = record
    fCA: BOOL;
    fPathLenConstraint: BOOL;
    dwPathLenConstraint: DWORD;
  end;
  {$EXTERNALSYM CERT_BASIC_CONSTRAINTS2_INFO}
  CERT_BASIC_CONSTRAINTS2_INFO = _CERT_BASIC_CONSTRAINTS2_INFO;
  {$EXTERNALSYM PCERT_BASIC_CONSTRAINTS2_INFO}
  PCERT_BASIC_CONSTRAINTS2_INFO = ^_CERT_BASIC_CONSTRAINTS2_INFO;
  TCertBasicConstraints2Info = _CERT_BASIC_CONSTRAINTS2_INFO;

//+-------------------------------------------------------------------------
//  X509_KEY_USAGE
//  szOID_KEY_USAGE
//
//  pvStructInfo points to a CRYPT_BIT_BLOB. Has same bit definitions as
//  CERT_KEY_ATTRIBUTES_INFO's IntendedKeyUsage.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  X509_CERT_POLICIES
//  szOID_CERT_POLICIES
//  szOID_CERT_POLICIES_95   NOTE--Only allowed for decoding!!!
//
//  pvStructInfo points to following CERT_POLICIES_INFO.
//
//  NOTE: when decoding using szOID_CERT_POLICIES_95 the pszPolicyIdentifier
//        may contain an empty string
//--------------------------------------------------------------------------
// certenrolls_begin -- CERT_POLICY_QUALIFIER_INFO
  PCertPolicyQualifierInfo = ^TCertPolicyQualifierInfo;
  {$EXTERNALSYM _CERT_POLICY_QUALIFIER_INFO}
  _CERT_POLICY_QUALIFIER_INFO = record
    pszPolicyQualifierId: LPSTR;   // pszObjId
    Qualifier: CRYPT_OBJID_BLOB;              // optional
  end;
  {$EXTERNALSYM CERT_POLICY_QUALIFIER_INFO}
  CERT_POLICY_QUALIFIER_INFO = _CERT_POLICY_QUALIFIER_INFO;
  {$EXTERNALSYM PCERT_POLICY_QUALIFIER_INFO}
  PCERT_POLICY_QUALIFIER_INFO = ^_CERT_POLICY_QUALIFIER_INFO;
  TCertPolicyQualifierInfo = _CERT_POLICY_QUALIFIER_INFO;

  PCertPolicyInfo = ^TCertPolicyInfo;
  {$EXTERNALSYM _CERT_POLICY_INFO}
  _CERT_POLICY_INFO = record
    pszPolicyIdentifier: LPSTR;    // pszObjId
    cPolicyQualifier: DWORD;       // optional
    rgPolicyQualifier: ^CERT_POLICY_QUALIFIER_INFO;
  end;
  {$EXTERNALSYM CERT_POLICY_INFO}
  CERT_POLICY_INFO = _CERT_POLICY_INFO;
  {$EXTERNALSYM PCERT_POLICY_INFO}
  PCERT_POLICY_INFO = ^_CERT_POLICY_INFO;
  TCertPolicyInfo = _CERT_POLICY_INFO;

  PCertPoliciesInfo = ^TCertPoliciesInfo;
  {$EXTERNALSYM _CERT_POLICIES_INFO}
  _CERT_POLICIES_INFO = record
    cPolicyInfo: DWORD;
    rgPolicyInfo: ^CERT_POLICY_INFO;
  end;
  {$EXTERNALSYM CERT_POLICIES_INFO}
  CERT_POLICIES_INFO = _CERT_POLICIES_INFO;
  {$EXTERNALSYM PCERT_POLICIES_INFO}
  PCERT_POLICIES_INFO = ^_CERT_POLICIES_INFO;
  TCertPoliciesInfo = _CERT_POLICIES_INFO;
// certenrolls_end

//+-------------------------------------------------------------------------
//  X509_PKIX_POLICY_QUALIFIER_USERNOTICE
//  szOID_PKIX_POLICY_QUALIFIER_USERNOTICE
//
//  pvStructInfo points to following CERT_POLICY_QUALIFIER_USER_NOTICE.
//
//--------------------------------------------------------------------------
  PCertPolicyQualifierNoticeReference = ^TCertPolicyQualifierNoticeReference;
  {$EXTERNALSYM _CERT_POLICY_QUALIFIER_NOTICE_REFERENCE}
  _CERT_POLICY_QUALIFIER_NOTICE_REFERENCE = record
    pszOrganization: LPSTR;
    cNoticeNumbers: DWORD;
    rgNoticeNumbers: ^Integer;
  end;
  {$EXTERNALSYM CERT_POLICY_QUALIFIER_NOTICE_REFERENCE}
  CERT_POLICY_QUALIFIER_NOTICE_REFERENCE = _CERT_POLICY_QUALIFIER_NOTICE_REFERENCE;
  {$EXTERNALSYM PCERT_POLICY_QUALIFIER_NOTICE_REFERENCE}
  PCERT_POLICY_QUALIFIER_NOTICE_REFERENCE = ^_CERT_POLICY_QUALIFIER_NOTICE_REFERENCE;
  TCertPolicyQualifierNoticeReference = _CERT_POLICY_QUALIFIER_NOTICE_REFERENCE;

  PCertPolicyQualifierUserNotice = ^TCertPolicyQualifierUserNotice;
  {$EXTERNALSYM _CERT_POLICY_QUALIFIER_USER_NOTICE}
  _CERT_POLICY_QUALIFIER_USER_NOTICE = record
    pNoticeReference: ^CERT_POLICY_QUALIFIER_NOTICE_REFERENCE;  // optional
    pszDisplayText: LPWSTR;     // optional
  end;
  {$EXTERNALSYM CERT_POLICY_QUALIFIER_USER_NOTICE}
  CERT_POLICY_QUALIFIER_USER_NOTICE = _CERT_POLICY_QUALIFIER_USER_NOTICE;
  {$EXTERNALSYM PCERT_POLICY_QUALIFIER_USER_NOTICE}
  PCERT_POLICY_QUALIFIER_USER_NOTICE = ^_CERT_POLICY_QUALIFIER_USER_NOTICE;
  TCertPolicyQualifierUserNotice = _CERT_POLICY_QUALIFIER_USER_NOTICE;

//+-------------------------------------------------------------------------
//  szOID_CERT_POLICIES_95_QUALIFIER1 - Decode Only!!!!
//
//  pvStructInfo points to following CERT_POLICY95_QUALIFIER1.
//
//--------------------------------------------------------------------------
  PCPSURLs = ^TCPSURLs;
  {$EXTERNALSYM _CPS_URLS}
  _CPS_URLS = record
    pszURL: LPWSTR;
    pAlgorithm: ^CRYPT_ALGORITHM_IDENTIFIER; // optional
    pDigest: ^CRYPT_DATA_BLOB;    // optional
  end;
  {$EXTERNALSYM CPS_URLS}
  CPS_URLS = _CPS_URLS;
  {$EXTERNALSYM PCPS_URLS}
  PCPS_URLS = ^_CPS_URLS;
  TCPSURLs = _CPS_URLS;

  PCertPolicy95Qualifier1 = ^TCertPolicy95Qualifier1;
  {$EXTERNALSYM _CERT_POLICY95_QUALIFIER1}
  _CERT_POLICY95_QUALIFIER1 = record
    pszPracticesReference: LPWSTR;      // optional
    pszNoticeIdentifier: LPSTR;        // optional
    pszNSINoticeIdentifier: LPSTR;     // optional
    cCPSURLs: DWORD;
    rgCPSURLs: ^CPS_URLS;                 // optional
  end;
  {$EXTERNALSYM CERT_POLICY95_QUALIFIER1}
  CERT_POLICY95_QUALIFIER1 = _CERT_POLICY95_QUALIFIER1;
  {$EXTERNALSYM PCERT_POLICY95_QUALIFIER1}
  PCERT_POLICY95_QUALIFIER1 = ^_CERT_POLICY95_QUALIFIER1;
  TCertPolicy95Qualifier1 = _CERT_POLICY95_QUALIFIER1;

//+-------------------------------------------------------------------------
//  szOID_INHIBIT_ANY_POLICY data structure
//
//  pvStructInfo points to an int.
//--------------------------------------------------------------------------


//+-------------------------------------------------------------------------
//  X509_POLICY_MAPPINGS
//  szOID_POLICY_MAPPINGS
//  szOID_LEGACY_POLICY_MAPPINGS
//
//  pvStructInfo points to following CERT_POLICY_MAPPINGS_INFO.
//--------------------------------------------------------------------------
  PCertPolicyMapping = ^TCertPolicyMapping;
  {$EXTERNALSYM _CERT_POLICY_MAPPING}
  _CERT_POLICY_MAPPING = record
    pszIssuerDomainPolicy: LPSTR;      // pszObjId
    pszSubjectDomainPolicy: LPSTR;     // pszObjId
  end;
  {$EXTERNALSYM CERT_POLICY_MAPPING}
  CERT_POLICY_MAPPING = _CERT_POLICY_MAPPING;
  TCertPolicyMapping = _CERT_POLICY_MAPPING;

  PCertPolicyMappingsInfo = ^TCertPolicyMappingsInfo;
  {$EXTERNALSYM _CERT_POLICY_MAPPINGS_INFO}
  _CERT_POLICY_MAPPINGS_INFO = record
    cPolicyMapping: DWORD;
    rgPolicyMapping: PCertPolicyMapping;
  end;
  {$EXTERNALSYM CERT_POLICY_MAPPINGS_INFO}
  CERT_POLICY_MAPPINGS_INFO = _CERT_POLICY_MAPPINGS_INFO;
  {$EXTERNALSYM PCERT_POLICY_MAPPINGS_INFO}
  PCERT_POLICY_MAPPINGS_INFO = ^_CERT_POLICY_MAPPINGS_INFO;
  TCertPolicyMappingsInfo = _CERT_POLICY_MAPPINGS_INFO;

//+-------------------------------------------------------------------------
//  X509_POLICY_CONSTRAINTS
//  szOID_POLICY_CONSTRAINTS
//
//  pvStructInfo points to following CERT_POLICY_CONSTRAINTS_INFO.
//--------------------------------------------------------------------------
  PCertPolicyConstraintsInfo = ^TCertPolicyConstraintsInfo;
  {$EXTERNALSYM _CERT_POLICY_CONSTRAINTS_INFO}
  _CERT_POLICY_CONSTRAINTS_INFO = record
    fRequireExplicitPolicy: BOOL;
    dwRequireExplicitPolicySkipCerts: DWORD;

    fInhibitPolicyMapping: BOOL;
    dwInhibitPolicyMappingSkipCerts: DWORD;
  end;
  {$EXTERNALSYM CERT_POLICY_CONSTRAINTS_INFO}
  CERT_POLICY_CONSTRAINTS_INFO = _CERT_POLICY_CONSTRAINTS_INFO;
  {$EXTERNALSYM PCERT_POLICY_CONSTRAINTS_INFO}
  PCERT_POLICY_CONSTRAINTS_INFO = ^_CERT_POLICY_CONSTRAINTS_INFO;
  TCertPolicyConstraintsInfo = _CERT_POLICY_CONSTRAINTS_INFO;

//+-------------------------------------------------------------------------
//  RSA_CSP_PUBLICKEYBLOB
//
//  pvStructInfo points to a PUBLICKEYSTRUC immediately followed by a
//  RSAPUBKEY and the modulus bytes.
//
//  CryptExportKey outputs the above StructInfo for a dwBlobType of
//  PUBLICKEYBLOB. CryptImportKey expects the above StructInfo when
//  importing a public key.
//
//  For dwCertEncodingType = X509_ASN_ENCODING, the RSA_CSP_PUBLICKEYBLOB is
//  encoded as a PKCS #1 RSAPublicKey consisting of a SEQUENCE of a
//  modulus INTEGER and a publicExponent INTEGER. The modulus is encoded
//  as being a unsigned integer. When decoded, if the modulus was encoded
//  as unsigned integer with a leading 0 byte, the 0 byte is removed before
//  converting to the CSP modulus bytes.
//
//  For decode, the aiKeyAlg field of PUBLICKEYSTRUC is always set to
//  CALG_RSA_KEYX.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CNG_RSA_PUBLIC_KEY_BLOB
//
//  pvStructInfo points to a BCRYPT_RSAKEY_BLOB immediately followed by the
//  exponent and the modulus bytes. Both the exponent and modulus are
//  big endian. The private key fields consisting of cbPrime1 and cbPrime2
//  are set to zero.
//
//  For dwCertEncodingType = X509_ASN_ENCODING, the CNG_RSA_PUBLIC_KEY_BLOB is
//  encoded as a PKCS #1 RSAPublicKey consisting of a SEQUENCE of a
//  modulus HUGEINTEGER and a publicExponent HUGEINTEGER.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  X509_KEYGEN_REQUEST_TO_BE_SIGNED
//
//  pvStructInfo points to CERT_KEYGEN_REQUEST_INFO.
//
//  For CryptDecodeObject(), the pbEncoded is the 'to be signed' plus its
//  signature (output of a X509_CERT CryptEncodeObject()).
//
//  For CryptEncodeObject(), the pbEncoded is just the 'to be signed'.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  PKCS_ATTRIBUTE data structure
//
//  pvStructInfo points to a CRYPT_ATTRIBUTE.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  PKCS_ATTRIBUTES data structure
//
//  pvStructInfo points to a CRYPT_ATTRIBUTES.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  PKCS_CONTENT_INFO_SEQUENCE_OF_ANY data structure
//
//  pvStructInfo points to following CRYPT_CONTENT_INFO_SEQUENCE_OF_ANY.
//
//  For X509_ASN_ENCODING: encoded as a PKCS#7 ContentInfo structure wrapping
//  a sequence of ANY. The value of the contentType field is pszObjId,
//  while the content field is the following structure:
//      SequenceOfAny ::= SEQUENCE OF ANY
//
//  The CRYPT_DER_BLOBs point to the already encoded ANY content.
//--------------------------------------------------------------------------
  PCryptContentInfoSequenceOfAny = ^TCryptContentInfoSequenceOfAny;
  {$EXTERNALSYM _CRYPT_CONTENT_INFO_SEQUENCE_OF_ANY}
  _CRYPT_CONTENT_INFO_SEQUENCE_OF_ANY = record
    pszObjId: LPSTR;
    cValue: DWORD;
    rgValue: PCryptDERBlob;
  end;
  {$EXTERNALSYM CRYPT_CONTENT_INFO_SEQUENCE_OF_ANY}
  CRYPT_CONTENT_INFO_SEQUENCE_OF_ANY = _CRYPT_CONTENT_INFO_SEQUENCE_OF_ANY;
  {$EXTERNALSYM PCRYPT_CONTENT_INFO_SEQUENCE_OF_ANY}
  PCRYPT_CONTENT_INFO_SEQUENCE_OF_ANY = ^_CRYPT_CONTENT_INFO_SEQUENCE_OF_ANY;
  TCryptContentInfoSequenceOfAny = _CRYPT_CONTENT_INFO_SEQUENCE_OF_ANY;

//+-------------------------------------------------------------------------
//  PKCS_CONTENT_INFO data structure
//
//  pvStructInfo points to following CRYPT_CONTENT_INFO.
//
//  For X509_ASN_ENCODING: encoded as a PKCS#7 ContentInfo structure.
//  The CRYPT_DER_BLOB points to the already encoded ANY content.
//--------------------------------------------------------------------------
  PCryptContentInfo = ^TCryptContentInfo;
  {$EXTERNALSYM _CRYPT_CONTENT_INFO}
  _CRYPT_CONTENT_INFO = record
    pszObjId: LPSTR;
    Content: CRYPT_DER_BLOB;
  end;
  {$EXTERNALSYM CRYPT_CONTENT_INFO}
  CRYPT_CONTENT_INFO = _CRYPT_CONTENT_INFO;
  {$EXTERNALSYM PCRYPT_CONTENT_INFO}
  PCRYPT_CONTENT_INFO = ^_CRYPT_CONTENT_INFO;
  TCryptContentInfo = _CRYPT_CONTENT_INFO;


//+-------------------------------------------------------------------------
//  X509_OCTET_STRING data structure
//
//  pvStructInfo points to a CRYPT_DATA_BLOB.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  X509_BITS data structure
//
//  pvStructInfo points to a CRYPT_BIT_BLOB.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  X509_BITS_WITHOUT_TRAILING_ZEROES data structure
//
//  pvStructInfo points to a CRYPT_BIT_BLOB.
//
//  The same as X509_BITS, except before encoding, the bit length is
//  decremented to exclude trailing zero bits.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  X509_INTEGER data structure
//
//  pvStructInfo points to an int.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  X509_MULTI_BYTE_INTEGER data structure
//
//  pvStructInfo points to a CRYPT_INTEGER_BLOB.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  X509_ENUMERATED data structure
//
//  pvStructInfo points to an int containing the enumerated value
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  X509_CHOICE_OF_TIME data structure
//
//  pvStructInfo points to a FILETIME.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  X509_SEQUENCE_OF_ANY data structure
//
//  pvStructInfo points to following CRYPT_SEQUENCE_OF_ANY.
//
//  The CRYPT_DER_BLOBs point to the already encoded ANY content.
//--------------------------------------------------------------------------
  PCryptSequenceOfAny = ^TCryptSequenceOfAny;
  {$EXTERNALSYM _CRYPT_SEQUENCE_OF_ANY}
  _CRYPT_SEQUENCE_OF_ANY = record
    cValue: DWORD;
    rgValue: PCryptDerBlob;
  end;
  {$EXTERNALSYM CRYPT_SEQUENCE_OF_ANY}
  CRYPT_SEQUENCE_OF_ANY = _CRYPT_SEQUENCE_OF_ANY;
  {$EXTERNALSYM PCRYPT_SEQUENCE_OF_ANY}
  PCRYPT_SEQUENCE_OF_ANY = ^_CRYPT_SEQUENCE_OF_ANY;
  TCryptSequenceOfAny = _CRYPT_SEQUENCE_OF_ANY;

//+-------------------------------------------------------------------------
//  X509_AUTHORITY_KEY_ID2
//  szOID_AUTHORITY_KEY_IDENTIFIER2
//
//  pvStructInfo points to following CERT_AUTHORITY_KEY_ID2_INFO.
//
//  For CRYPT_E_INVALID_IA5_STRING, the error location is returned in
//  *pcbEncoded by CryptEncodeObject(X509_AUTHORITY_KEY_ID2)
//
//  See X509_ALTERNATE_NAME for error location defines.
//--------------------------------------------------------------------------
  PCertAuthorityKeyID2Info = ^TCertAuthorityKeyID2Info;
  {$EXTERNALSYM _CERT_AUTHORITY_KEY_ID2_INFO}
  _CERT_AUTHORITY_KEY_ID2_INFO = record
    KeyId: CRYPT_DATA_BLOB;
    AuthorityCertIssuer: CERT_ALT_NAME_INFO;    // Optional, set cAltEntry
                                                // to 0 to omit.
    AuthorityCertSerialNumber: CRYPT_INTEGER_BLOB;
  end;
  {$EXTERNALSYM CERT_AUTHORITY_KEY_ID2_INFO}
  CERT_AUTHORITY_KEY_ID2_INFO = _CERT_AUTHORITY_KEY_ID2_INFO;
  {$EXTERNALSYM PCERT_AUTHORITY_KEY_ID2_INFO}
  PCERT_AUTHORITY_KEY_ID2_INFO = ^_CERT_AUTHORITY_KEY_ID2_INFO;
  TCertAuthorityKeyID2Info = _CERT_AUTHORITY_KEY_ID2_INFO;

//+-------------------------------------------------------------------------
//  szOID_SUBJECT_KEY_IDENTIFIER
//
//  pvStructInfo points to a CRYPT_DATA_BLOB.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  X509_AUTHORITY_INFO_ACCESS
//  szOID_AUTHORITY_INFO_ACCESS
//
//  X509_SUBJECT_INFO_ACCESS
//  szOID_SUBJECT_INFO_ACCESS
//
//  pvStructInfo points to following CERT_AUTHORITY_INFO_ACCESS.
//
//  For CRYPT_E_INVALID_IA5_STRING, the error location is returned in
//  *pcbEncoded by CryptEncodeObject(X509_AUTHORITY_INFO_ACCESS)
//
//  Error location consists of:
//    ENTRY_INDEX   - 8 bits shl 16
//    VALUE_INDEX   - 16 bits (unicode character index)
//
//  See X509_ALTERNATE_NAME for ENTRY_INDEX and VALUE_INDEX error location
//  defines.
//
//  Note, the szOID_SUBJECT_INFO_ACCESS extension has the same ASN.1
//  encoding as the szOID_AUTHORITY_INFO_ACCESS extension.
//--------------------------------------------------------------------------
  PCertAccessDescription = ^TCertAccessDescription;
  {$EXTERNALSYM _CERT_ACCESS_DESCRIPTION}
  _CERT_ACCESS_DESCRIPTION = record
    pszAccessMethod: LPSTR;        // pszObjId
    AccessLocation: CERT_ALT_NAME_ENTRY;
  end;
  {$EXTERNALSYM CERT_ACCESS_DESCRIPTION}
  CERT_ACCESS_DESCRIPTION = _CERT_ACCESS_DESCRIPTION;
  {$EXTERNALSYM PCERT_ACCESS_DESCRIPTION}
  PCERT_ACCESS_DESCRIPTION = ^_CERT_ACCESS_DESCRIPTION;
  TCertAccessDescription = _CERT_ACCESS_DESCRIPTION;

  PCertAuthorityInfoAccess = ^TCertAuthorityInfoAccess;
  PCertSubjectInfoAccess = ^TCertSubjectInfoAccess;
  {$EXTERNALSYM _CERT_AUTHORITY_INFO_ACCESS}
  _CERT_AUTHORITY_INFO_ACCESS = record
    cAccDescr: DWORD;
    rgAccDescr: PCertAccessDescription;
  end;
  {$EXTERNALSYM CERT_AUTHORITY_INFO_ACCESS}
  CERT_AUTHORITY_INFO_ACCESS = _CERT_AUTHORITY_INFO_ACCESS;
  {$EXTERNALSYM PCERT_AUTHORITY_INFO_ACCESS}
  PCERT_AUTHORITY_INFO_ACCESS = ^_CERT_AUTHORITY_INFO_ACCESS;
  {$EXTERNALSYM CERT_SUBJECT_INFO_ACCESS}
  CERT_SUBJECT_INFO_ACCESS = _CERT_AUTHORITY_INFO_ACCESS;
  {$EXTERNALSYM PCERT_SUBJECT_INFO_ACCESS}
  PCERT_SUBJECT_INFO_ACCESS = ^_CERT_AUTHORITY_INFO_ACCESS;
  TCertAuthorityInfoAccess = _CERT_AUTHORITY_INFO_ACCESS;
  TCertSubjectInfoAccess = _CERT_AUTHORITY_INFO_ACCESS;

const
//+-------------------------------------------------------------------------
//  PKIX Access Description: Access Method Object Identifiers
//--------------------------------------------------------------------------
  {$EXTERNALSYM szOID_PKIX_ACC_DESCR}
  szOID_PKIX_ACC_DESCR            = '1.3.6.1.5.5.7.48';

// For szOID_AUTHORITY_INFO_ACCESS
  {$EXTERNALSYM szOID_PKIX_OCSP}
  szOID_PKIX_OCSP                 = '1.3.6.1.5.5.7.48.1';
  {$EXTERNALSYM szOID_PKIX_CA_ISSUERS}
  szOID_PKIX_CA_ISSUERS           = '1.3.6.1.5.5.7.48.2';

// For szOID_SUBJECT_INFO_ACCESS
  {$EXTERNALSYM szOID_PKIX_TIME_STAMPING}
  szOID_PKIX_TIME_STAMPING        = '1.3.6.1.5.5.7.48.3';
  {$EXTERNALSYM szOID_PKIX_CA_REPOSITORY}
  szOID_PKIX_CA_REPOSITORY        = '1.3.6.1.5.5.7.48.5';

//+-------------------------------------------------------------------------
//  X509_CRL_REASON_CODE
//  szOID_CRL_REASON_CODE
//
//  pvStructInfo points to an int which can be set to one of the following
//  enumerated values:
//--------------------------------------------------------------------------
  {$EXTERNALSYM CRL_REASON_UNSPECIFIED}
  CRL_REASON_UNSPECIFIED = 0;
  {$EXTERNALSYM CRL_REASON_KEY_COMPROMISE}
  CRL_REASON_KEY_COMPROMISE = 1;
  {$EXTERNALSYM CRL_REASON_CA_COMPROMISE}
  CRL_REASON_CA_COMPROMISE = 2;
  {$EXTERNALSYM CRL_REASON_AFFILIATION_CHANGED}
  CRL_REASON_AFFILIATION_CHANGED = 3;
  {$EXTERNALSYM CRL_REASON_SUPERSEDED}
  CRL_REASON_SUPERSEDED = 4;
  {$EXTERNALSYM CRL_REASON_CESSATION_OF_OPERATION}
  CRL_REASON_CESSATION_OF_OPERATION = 5;
  {$EXTERNALSYM CRL_REASON_CERTIFICATE_HOLD}
  CRL_REASON_CERTIFICATE_HOLD = 6;
  {$EXTERNALSYM CRL_REASON_REMOVE_FROM_CRL}
  CRL_REASON_REMOVE_FROM_CRL = 8;

type
//+-------------------------------------------------------------------------
//  X509_CRL_DIST_POINTS
//  szOID_CRL_DIST_POINTS
//
//  pvStructInfo points to following CRL_DIST_POINTS_INFO.
//
//  For CRYPT_E_INVALID_IA5_STRING, the error location is returned in
//  *pcbEncoded by CryptEncodeObject(X509_CRL_DIST_POINTS)
//
//  Error location consists of:
//    CRL_ISSUER_BIT    - 1 bit  shl 31 (0 for FullName, 1 for CRLIssuer)
//    POINT_INDEX       - 7 bits shl 24
//    ENTRY_INDEX       - 8 bits shl 16
//    VALUE_INDEX       - 16 bits (unicode character index)
//
//  See X509_ALTERNATE_NAME for ENTRY_INDEX and VALUE_INDEX error location
//  defines.
//--------------------------------------------------------------------------
  PCRLDistPointName = ^TCRLDistPointName;
  {$EXTERNALSYM _CRL_DIST_POINT_NAME}
  _CRL_DIST_POINT_NAME = record
    case dwDistPointNameChoice: DWORD of
      1: (FullName: CERT_ALT_NAME_INFO);       // 1
      // Not implemented      IssuerRDN;       // 2
  end;
  {$EXTERNALSYM CRL_DIST_POINT_NAME}
  CRL_DIST_POINT_NAME = _CRL_DIST_POINT_NAME;
  {$EXTERNALSYM PCRL_DIST_POINT_NAME}
  PCRL_DIST_POINT_NAME = ^_CRL_DIST_POINT_NAME;
  TCRLDistPointName = _CRL_DIST_POINT_NAME;

const
  {$EXTERNALSYM CRL_DIST_POINT_NO_NAME}
  CRL_DIST_POINT_NO_NAME = 0;
  {$EXTERNALSYM CRL_DIST_POINT_FULL_NAME}
  CRL_DIST_POINT_FULL_NAME = 1;
  {$EXTERNALSYM CRL_DIST_POINT_ISSUER_RDN_NAME}
  CRL_DIST_POINT_ISSUER_RDN_NAME = 2;

type
  PCRLDistPoint = ^TCRLDistPoint;
  {$EXTERNALSYM _CRL_DIST_POINT}
  _CRL_DIST_POINT = record
    DistPointName: CRL_DIST_POINT_NAME;      // OPTIONAL
    ReasonFlags: CRYPT_BIT_BLOB;             // OPTIONAL
    CRLIssuer: CERT_ALT_NAME_INFO;           // OPTIONAL
  end;
  {$EXTERNALSYM CRL_DIST_POINT}
  CRL_DIST_POINT = _CRL_DIST_POINT;
  {$EXTERNALSYM PCRL_DIST_POINT}
  PCRL_DIST_POINT = ^_CRL_DIST_POINT;
  TCRLDistPoint = _CRL_DIST_POINT;

const
  {$EXTERNALSYM CRL_REASON_UNUSED_FLAG}
  CRL_REASON_UNUSED_FLAG = $80;
  {$EXTERNALSYM CRL_REASON_KEY_COMPROMISE_FLAG}
  CRL_REASON_KEY_COMPROMISE_FLAG = $40;
  {$EXTERNALSYM CRL_REASON_CA_COMPROMISE_FLAG}
  CRL_REASON_CA_COMPROMISE_FLAG = $20;
  {$EXTERNALSYM CRL_REASON_AFFILIATION_CHANGED_FLAG}
  CRL_REASON_AFFILIATION_CHANGED_FLAG = $10;
  {$EXTERNALSYM CRL_REASON_SUPERSEDED_FLAG}
  CRL_REASON_SUPERSEDED_FLAG = $08;
  {$EXTERNALSYM CRL_REASON_CESSATION_OF_OPERATION_FLAG}
  CRL_REASON_CESSATION_OF_OPERATION_FLAG = $04;
  {$EXTERNALSYM CRL_REASON_CERTIFICATE_HOLD_FLAG}
  CRL_REASON_CERTIFICATE_HOLD_FLAG = $02;

type
  PCRLDistPointsInfo = ^TCRLDistPointsInfo;
  {$EXTERNALSYM _CRL_DIST_POINTS_INFO}
  _CRL_DIST_POINTS_INFO = record
    cDistPoint: DWORD;
    rgDistPoint: PCRLDistPoint;
  end;
  {$EXTERNALSYM CRL_DIST_POINTS_INFO}
  CRL_DIST_POINTS_INFO = _CRL_DIST_POINTS_INFO;
  {$EXTERNALSYM PCRL_DIST_POINTS_INFO}
  PCRL_DIST_POINTS_INFO = ^_CRL_DIST_POINTS_INFO;
  TCRLDistPointsInfo = _CRL_DIST_POINTS_INFO;

const
  {$EXTERNALSYM CRL_DIST_POINT_ERR_INDEX_MASK}
  CRL_DIST_POINT_ERR_INDEX_MASK = $7F;
  {$EXTERNALSYM CRL_DIST_POINT_ERR_INDEX_SHIFT}
  CRL_DIST_POINT_ERR_INDEX_SHIFT = 24;

{$EXTERNALSYM GET_CRL_DIST_POINT_ERR_INDEX}
function GET_CRL_DIST_POINT_ERR_INDEX(X: DWORD): DWORD; {$IfDef HAS_INLINE}inline;{$EndIf}

const
  {$EXTERNALSYM CRL_DIST_POINT_ERR_CRL_ISSUER_BIT}
  CRL_DIST_POINT_ERR_CRL_ISSUER_BIT = $80000000;

{$EXTERNALSYM IS_CRL_DIST_POINT_ERR_CRL_ISSUER}
function IS_CRL_DIST_POINT_ERR_CRL_ISSUER(X: DWORD): Boolean; {$IfDef HAS_INLINE}inline;{$EndIf}

//+-------------------------------------------------------------------------
//  X509_CROSS_CERT_DIST_POINTS
//  szOID_CROSS_CERT_DIST_POINTS
//
//  pvStructInfo points to following CROSS_CERT_DIST_POINTS_INFO.
//
//  For CRYPT_E_INVALID_IA5_STRING, the error location is returned in
//  *pcbEncoded by CryptEncodeObject(X509_CRL_DIST_POINTS)
//
//  Error location consists of:
//    POINT_INDEX       - 8 bits shl 24
//    ENTRY_INDEX       - 8 bits shl 16
//    VALUE_INDEX       - 16 bits (unicode character index)
//
//  See X509_ALTERNATE_NAME for ENTRY_INDEX and VALUE_INDEX error location
//  defines.
//--------------------------------------------------------------------------
type
  PCrossCertDistPointsInfo = ^TCrossCertDistPointsInfo;
  {$EXTERNALSYM _CROSS_CERT_DIST_POINTS_INFO}
  _CROSS_CERT_DIST_POINTS_INFO = record
    // Seconds between syncs. 0 implies use client default.
    dwSyncDeltaTime: DWORD;

    cDistPoint: DWORD;
    rgDistPoint: PCertAltNameInfo;
  end;
  {$EXTERNALSYM CROSS_CERT_DIST_POINTS_INFO}
  CROSS_CERT_DIST_POINTS_INFO = _CROSS_CERT_DIST_POINTS_INFO;
  {$EXTERNALSYM PCROSS_CERT_DIST_POINTS_INFO}
  PCROSS_CERT_DIST_POINTS_INFO = ^_CROSS_CERT_DIST_POINTS_INFO;
  TCrossCertDistPointsInfo = _CROSS_CERT_DIST_POINTS_INFO;

const
  {$EXTERNALSYM CROSS_CERT_DIST_POINT_ERR_INDEX_MASK}
  CROSS_CERT_DIST_POINT_ERR_INDEX_MASK = $FF;
  {$EXTERNALSYM CROSS_CERT_DIST_POINT_ERR_INDEX_SHIFT}
  CROSS_CERT_DIST_POINT_ERR_INDEX_SHIFT = 24;

{$EXTERNALSYM GET_CROSS_CERT_DIST_POINT_ERR_INDEX}
function GET_CROSS_CERT_DIST_POINT_ERR_INDEX(X: DWORD): DWORD; {$IfDef HAS_INLINE}inline;{$EndIf}

//+-------------------------------------------------------------------------
//  X509_ENHANCED_KEY_USAGE
//  szOID_ENHANCED_KEY_USAGE
//
//  pvStructInfo points to a CERT_ENHKEY_USAGE, CTL_USAGE.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  X509_CERT_PAIR
//
//  pvStructInfo points to the following CERT_PAIR.
//--------------------------------------------------------------------------
type
  PCertPair = ^TCertPair;
  {$EXTERNALSYM _CERT_PAIR}
  _CERT_PAIR = record
    Forward: CERT_BLOB;        // OPTIONAL, if Forward.cbData == 0, omitted
    Reverse: CERT_BLOB;        // OPTIONAL, if Reverse.cbData == 0, omitted
  end;
  {$EXTERNALSYM CERT_PAIR}
  CERT_PAIR = _CERT_PAIR;
  {$EXTERNALSYM PCERT_PAIR}
  PCERT_PAIR = ^_CERT_PAIR;
  TCertPair = _CERT_PAIR;

//+-------------------------------------------------------------------------
//  szOID_CRL_NUMBER
//
//  pvStructInfo points to an int.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  szOID_DELTA_CRL_INDICATOR
//
//  pvStructInfo points to an int.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  szOID_ISSUING_DIST_POINT
//  X509_ISSUING_DIST_POINT
//
//  pvStructInfo points to the following CRL_ISSUING_DIST_POINT.
//
//  For CRYPT_E_INVALID_IA5_STRING, the error location is returned in
//  *pcbEncoded by CryptEncodeObject(X509_ISSUING_DIST_POINT)
//
//  Error location consists of:
//    ENTRY_INDEX       - 8 bits shl 16
//    VALUE_INDEX       - 16 bits (unicode character index)
//
//  See X509_ALTERNATE_NAME for ENTRY_INDEX and VALUE_INDEX error location
//  defines.
//--------------------------------------------------------------------------
  PCRLIssuingDistPoint = ^TCRLIssuingDistPoint;
  {$EXTERNALSYM _CRL_ISSUING_DIST_POINT}
  _CRL_ISSUING_DIST_POINT = record
    DistPointName: CRL_DIST_POINT_NAME;              // OPTIONAL
    fOnlyContainsUserCerts: BOOL;
    fOnlyContainsCACerts: BOOL;
    OnlySomeReasonFlags: CRYPT_BIT_BLOB;        // OPTIONAL
    fIndirectCRL: BOOL;
  end;
  {$EXTERNALSYM CRL_ISSUING_DIST_POINT}
  CRL_ISSUING_DIST_POINT = _CRL_ISSUING_DIST_POINT;
  {$EXTERNALSYM PCRL_ISSUING_DIST_POINT}
  PCRL_ISSUING_DIST_POINT = ^_CRL_ISSUING_DIST_POINT;
  TCRLIssuingDistPoint = _CRL_ISSUING_DIST_POINT;

//+-------------------------------------------------------------------------
//  szOID_FRESHEST_CRL
//
//  pvStructInfo points to CRL_DIST_POINTS_INFO.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  szOID_NAME_CONSTRAINTS
//  X509_NAME_CONSTRAINTS
//
//  pvStructInfo points to the following CERT_NAME_CONSTRAINTS_INFO
//
//  For CRYPT_E_INVALID_IA5_STRING, the error location is returned in
//  *pcbEncoded by CryptEncodeObject(X509_NAME_CONSTRAINTS)
//
//  Error location consists of:
//    EXCLUDED_SUBTREE_BIT  - 1 bit  shl 31 (0 for permitted, 1 for excluded)
//    ENTRY_INDEX           - 8 bits shl 16
//    VALUE_INDEX           - 16 bits (unicode character index)
//
//  See X509_ALTERNATE_NAME for ENTRY_INDEX and VALUE_INDEX error location
//  defines.
//--------------------------------------------------------------------------
  PCertGeneralSubtree = ^TCertGeneralSubtree;
  {$EXTERNALSYM _CERT_GENERAL_SUBTREE}
  _CERT_GENERAL_SUBTREE = record
    Base: CERT_ALT_NAME_ENTRY;
    dwMinimum: DWORD;
    fMaximum: BOOL;
    dwMaximum: DWORD;
  end;
  {$EXTERNALSYM CERT_GENERAL_SUBTREE}
  CERT_GENERAL_SUBTREE = _CERT_GENERAL_SUBTREE;
  {$EXTERNALSYM PCERT_GENERAL_SUBTREE}
  PCERT_GENERAL_SUBTREE = ^_CERT_GENERAL_SUBTREE;
  TCertGeneralSubtree = _CERT_GENERAL_SUBTREE;

  PCertNameConstraintsInfo = ^TCertNameConstraintsInfo;
  {$EXTERNALSYM _CERT_NAME_CONSTRAINTS_INFO}
  _CERT_NAME_CONSTRAINTS_INFO = record
    cPermittedSubtree: DWORD;
    rgPermittedSubtree: PCertGeneralSubtree;
    cExcludedSubtree: DWORD;
    rgExcludedSubtree: PCertGeneralSubtree;
  end;
  {$EXTERNALSYM CERT_NAME_CONSTRAINTS_INFO}
  CERT_NAME_CONSTRAINTS_INFO = _CERT_NAME_CONSTRAINTS_INFO;
  {$EXTERNALSYM PCERT_NAME_CONSTRAINTS_INFO}
  PCERT_NAME_CONSTRAINTS_INFO = ^_CERT_NAME_CONSTRAINTS_INFO;
  TCertNameConstraintsInfo = _CERT_NAME_CONSTRAINTS_INFO;

const
  {$EXTERNALSYM CERT_EXCLUDED_SUBTREE_BIT}
  CERT_EXCLUDED_SUBTREE_BIT = $80000000;

{$EXTERNALSYM IS_CERT_EXCLUDED_SUBTREE}
function IS_CERT_EXCLUDED_SUBTREE(X: DWORD): Boolean; {$IfDef HAS_INLINE}inline;{$EndIf}

//+-------------------------------------------------------------------------
//  szOID_NEXT_UPDATE_LOCATION
//
//  pvStructInfo points to a CERT_ALT_NAME_INFO.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  szOID_REMOVE_CERTIFICATE
//
//  pvStructInfo points to an int which can be set to one of the following
//   0 - Add certificate
//   1 - Remove certificate
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  PKCS_CTL
//  szOID_CTL
//
//  pvStructInfo points to a CTL_INFO.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  PKCS_SORTED_CTL
//
//  pvStructInfo points to a CTL_INFO.
//
//  Same as for PKCS_CTL, except, the CTL entries are sorted. The following
//  extension containing the sort information is inserted as the first
//  extension in the encoded CTL.
//
//  Only supported for Encoding. CRYPT_ENCODE_ALLOC_FLAG flag must be
//  set.
//--------------------------------------------------------------------------


//+-------------------------------------------------------------------------
// Sorted CTL TrustedSubjects extension
//
//  Array of little endian DWORDs:
//   [0] - Flags
//   [1] - Count of HashBucket entry offsets
//   [2] - Maximum HashBucket entry collision count
//   [3 ..] (Count + 1) HashBucket entry offsets
//
//  When this extension is present in the CTL,
//  the ASN.1 encoded sequence of TrustedSubjects are HashBucket ordered.
//
//  The entry offsets point to the start of the first encoded TrustedSubject
//  sequence for the HashBucket. The encoded TrustedSubjects for a HashBucket
//  continue until the encoded offset of the next HashBucket. A HashBucket has
//  no entries if HashBucket[N] == HashBucket[N + 1].
//
//  The HashBucket offsets are from the start of the ASN.1 encoded CTL_INFO.
//--------------------------------------------------------------------------
const
  {$EXTERNALSYM SORTED_CTL_EXT_FLAGS_OFFSET}
  SORTED_CTL_EXT_FLAGS_OFFSET         = 0 * 4;
  {$EXTERNALSYM SORTED_CTL_EXT_COUNT_OFFSET}
  SORTED_CTL_EXT_COUNT_OFFSET         = 1 * 4;
  {$EXTERNALSYM SORTED_CTL_EXT_MAX_COLLISION_OFFSET}
  SORTED_CTL_EXT_MAX_COLLISION_OFFSET = 2 * 4;
  {$EXTERNALSYM SORTED_CTL_EXT_HASH_BUCKET_OFFSET}
  SORTED_CTL_EXT_HASH_BUCKET_OFFSET   = 3 * 4;

// If the SubjectIdentifiers are a MD5 or SHA1 hash, the following flag is
// set. When set, the first 4 bytes of the SubjectIdentifier are used as
// the dwhash. Otherwise, the SubjectIdentifier bytes are hashed into dwHash.
// In either case the HashBucket index = dwHash % cHashBucket.
  {$EXTERNALSYM SORTED_CTL_EXT_HASHED_SUBJECT_IDENTIFIER_FLAG}
  SORTED_CTL_EXT_HASHED_SUBJECT_IDENTIFIER_FLAG = $1;

//+-------------------------------------------------------------------------
//  X509_MULTI_BYTE_UINT
//
//  pvStructInfo points to a CRYPT_UINT_BLOB. Before encoding, inserts a
//  leading $00. After decoding, removes a leading $00.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  X509_DSS_PUBLICKEY
//
//  pvStructInfo points to a CRYPT_UINT_BLOB.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  X509_DSS_PARAMETERS
//
//  pvStructInfo points to following CERT_DSS_PARAMETERS data structure.
//--------------------------------------------------------------------------
type
  PCertDSSParameters = ^TCertDSSParameters;
  {$EXTERNALSYM _CERT_DSS_PARAMETERS}
  _CERT_DSS_PARAMETERS = record
    p: CRYPT_UINT_BLOB;
    q: CRYPT_UINT_BLOB;
    g: CRYPT_UINT_BLOB;
  end;
  {$EXTERNALSYM CERT_DSS_PARAMETERS}
  CERT_DSS_PARAMETERS = _CERT_DSS_PARAMETERS;
  {$EXTERNALSYM PCERT_DSS_PARAMETERS}
  PCERT_DSS_PARAMETERS = ^_CERT_DSS_PARAMETERS;
  TCertDSSParameters = _CERT_DSS_PARAMETERS;

//+-------------------------------------------------------------------------
//  X509_DSS_SIGNATURE
//
//  pvStructInfo is a BYTE rgbSignature[CERT_DSS_SIGNATURE_LEN]. The
//  bytes are ordered as output by the DSS CSP's CryptSignHash().
//--------------------------------------------------------------------------
const
  {$EXTERNALSYM CERT_DSS_R_LEN}
  CERT_DSS_R_LEN = 20;
  {$EXTERNALSYM CERT_DSS_S_LEN}
  CERT_DSS_S_LEN = 20;

const
  {$EXTERNALSYM CERT_DSS_SIGNATURE_LEN}
  CERT_DSS_SIGNATURE_LEN  = CERT_DSS_R_LEN + CERT_DSS_S_LEN;

// Sequence of 2 unsigned integers (the extra +1 is for a potential leading
// $00 to make the integer unsigned)
  {$EXTERNALSYM CERT_MAX_ASN_ENCODED_DSS_SIGNATURE_LEN}
  CERT_MAX_ASN_ENCODED_DSS_SIGNATURE_LEN  = 2 + 2 * (2 + 20 + 1);

//+-------------------------------------------------------------------------
//  X509_DH_PUBLICKEY
//
//  pvStructInfo points to a CRYPT_UINT_BLOB.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  X509_DH_PARAMETERS
//
//  pvStructInfo points to following CERT_DH_PARAMETERS data structure.
//--------------------------------------------------------------------------
type
  PCertDHParameters = ^TCertDHParameters;
  {$EXTERNALSYM _CERT_DH_PARAMETERS}
  _CERT_DH_PARAMETERS = record
    p: CRYPT_UINT_BLOB;
    g: CRYPT_UINT_BLOB;
  end;
  {$EXTERNALSYM CERT_DH_PARAMETERS}
  CERT_DH_PARAMETERS = _CERT_DH_PARAMETERS;
  {$EXTERNALSYM PCERT_DH_PARAMETERS}
  PCERT_DH_PARAMETERS = ^_CERT_DH_PARAMETERS;
  TCertDHParameters = _CERT_DH_PARAMETERS;

//+-------------------------------------------------------------------------
//  X509_ECC_SIGNATURE
//
//  pvStructInfo points to following CERT_ECC_SIGNATURE data structure.
//
//  Note, identical to the above except for the names of the fields. Same
//  underlying encode/decode functions are used.
//--------------------------------------------------------------------------
  PCertECCSignature = ^TCertECCSignature;
  {$EXTERNALSYM _CERT_ECC_SIGNATURE}
  _CERT_ECC_SIGNATURE = record
    r: CRYPT_UINT_BLOB;
    s: CRYPT_UINT_BLOB;
  end;
  {$EXTERNALSYM CERT_ECC_SIGNATURE}
  CERT_ECC_SIGNATURE = _CERT_ECC_SIGNATURE;
  {$EXTERNALSYM PCERT_ECC_SIGNATURE}
  PCERT_ECC_SIGNATURE = ^_CERT_ECC_SIGNATURE;
  TCertECCSignature = _CERT_ECC_SIGNATURE;

//+-------------------------------------------------------------------------
//  X942_DH_PARAMETERS
//
//  pvStructInfo points to following CERT_X942_DH_PARAMETERS data structure.
//
//  If q.cbData == 0, then, the following fields are zero'ed.
//--------------------------------------------------------------------------
  PCertX942DHValidationParams = ^TCertX942DHValidationParams;
  {$EXTERNALSYM _CERT_X942_DH_VALIDATION_PARAMS}
  _CERT_X942_DH_VALIDATION_PARAMS = record
    seed: CRYPT_BIT_BLOB;
    pgenCounter: DWORD;
  end;
  {$EXTERNALSYM CERT_X942_DH_VALIDATION_PARAMS}
  CERT_X942_DH_VALIDATION_PARAMS = _CERT_X942_DH_VALIDATION_PARAMS;
  {$EXTERNALSYM PCERT_X942_DH_VALIDATION_PARAMS}
  PCERT_X942_DH_VALIDATION_PARAMS = ^_CERT_X942_DH_VALIDATION_PARAMS;
  TCertX942DHValidationParams = _CERT_X942_DH_VALIDATION_PARAMS;

  PCertX942DHParameters = ^TCertX942DHParameters;
  {$EXTERNALSYM _CERT_X942_DH_PARAMETERS}
  _CERT_X942_DH_PARAMETERS = record
    p: CRYPT_UINT_BLOB;          // odd prime, p = jq + 1
    g: CRYPT_UINT_BLOB;          // generator, g
    q: CRYPT_UINT_BLOB;          // factor of p - 1, OPTIONAL
    j: CRYPT_UINT_BLOB;          // subgroup factor, OPTIONAL
    pValidationParams: PCertX942DHValidationParams;  // OPTIONAL
  end;
  {$EXTERNALSYM CERT_X942_DH_PARAMETERS}
  CERT_X942_DH_PARAMETERS = _CERT_X942_DH_PARAMETERS;
  {$EXTERNALSYM PCERT_X942_DH_PARAMETERS}
  PCERT_X942_DH_PARAMETERS = ^_CERT_X942_DH_PARAMETERS;
  TCertX942DHParameters = _CERT_X942_DH_PARAMETERS;

//+-------------------------------------------------------------------------
//  X942_OTHER_INFO
//
//  pvStructInfo points to following CRYPT_X942_OTHER_INFO data structure.
//
//  rgbCounter and rgbKeyLength are in Little Endian order.
//--------------------------------------------------------------------------
const
  {$EXTERNALSYM CRYPT_X942_COUNTER_BYTE_LENGTH}
  CRYPT_X942_COUNTER_BYTE_LENGTH = 4;
  {$EXTERNALSYM CRYPT_X942_KEY_LENGTH_BYTE_LENGTH}
  CRYPT_X942_KEY_LENGTH_BYTE_LENGTH = 4;
  {$EXTERNALSYM CRYPT_X942_PUB_INFO_BYTE_LENGTH}
  CRYPT_X942_PUB_INFO_BYTE_LENGTH = 512 div 8;

type
  PCryptX942OtherInfo = ^TCryptX942OtherInfo;
  {$EXTERNALSYM _CRYPT_X942_OTHER_INFO}
  _CRYPT_X942_OTHER_INFO = record
    pszContentEncryptionObjId: LPSTR;
    rgbCounter: array[0..CRYPT_X942_COUNTER_BYTE_LENGTH - 1] of Byte;
    rgbKeyLength: array[0..CRYPT_X942_KEY_LENGTH_BYTE_LENGTH - 1] of Byte;
    PubInfo: CRYPT_DATA_BLOB;    // OPTIONAL
  end;
  {$EXTERNALSYM CRYPT_X942_OTHER_INFO}
  CRYPT_X942_OTHER_INFO = _CRYPT_X942_OTHER_INFO;
  {$EXTERNALSYM PCRYPT_X942_OTHER_INFO}
  PCRYPT_X942_OTHER_INFO = ^_CRYPT_X942_OTHER_INFO;
  TCryptX942OtherInfo = _CRYPT_X942_OTHER_INFO;


//+-------------------------------------------------------------------------
//  ECC_CMS_SHARED_INFO
//
//  pvStructInfo points to following ECC_CMS_SHARED_INFO data structure.
//
//  rgbSuppPubInfo is in Little Endian order.
//--------------------------------------------------------------------------
const
  {$EXTERNALSYM CRYPT_ECC_CMS_SHARED_INFO_SUPPPUBINFO_BYTE_LENGTH}
  CRYPT_ECC_CMS_SHARED_INFO_SUPPPUBINFO_BYTE_LENGTH = 4;

type
  PCryptECCCMSSharedInfo = ^TCryptECCCMSSharedInfo;
  {$EXTERNALSYM _CRYPT_ECC_CMS_SHARED_INFO}
  _CRYPT_ECC_CMS_SHARED_INFO = record
    Algorithm: CRYPT_ALGORITHM_IDENTIFIER;
    EntityUInfo: CRYPT_DATA_BLOB;    // OPTIONAL
    rgbSuppPubInfo: array[0..CRYPT_ECC_CMS_SHARED_INFO_SUPPPUBINFO_BYTE_LENGTH - 1] of Byte;
  end;
  {$EXTERNALSYM CRYPT_ECC_CMS_SHARED_INFO}
  CRYPT_ECC_CMS_SHARED_INFO = _CRYPT_ECC_CMS_SHARED_INFO;
  {$EXTERNALSYM PCRYPT_ECC_CMS_SHARED_INFO}
  PCRYPT_ECC_CMS_SHARED_INFO = ^_CRYPT_ECC_CMS_SHARED_INFO;
  TCryptECCCMSSharedInfo = _CRYPT_ECC_CMS_SHARED_INFO;

//+-------------------------------------------------------------------------
//  PKCS_RC2_CBC_PARAMETERS
//  szOID_RSA_RC2CBC
//
//  pvStructInfo points to following CRYPT_RC2_CBC_PARAMETERS data structure.
//--------------------------------------------------------------------------
  PCryptRC2CBCParameters = ^TCryptRC2CBCParameters;
  {$EXTERNALSYM _CRYPT_RC2_CBC_PARAMETERS}
  _CRYPT_RC2_CBC_PARAMETERS = record
    dwVersion: DWORD;
    fIV: BOOL;            // set if has following IV
    rgbIV: array[0..7] of Byte;
  end;
  {$EXTERNALSYM CRYPT_RC2_CBC_PARAMETERS}
  CRYPT_RC2_CBC_PARAMETERS = _CRYPT_RC2_CBC_PARAMETERS;
  {$EXTERNALSYM PCRYPT_RC2_CBC_PARAMETERS}
  PCRYPT_RC2_CBC_PARAMETERS = ^_CRYPT_RC2_CBC_PARAMETERS;
  TCryptRC2CBCParameters = _CRYPT_RC2_CBC_PARAMETERS;

const
  {$EXTERNALSYM CRYPT_RC2_40BIT_VERSION}
  CRYPT_RC2_40BIT_VERSION = 160;
  {$EXTERNALSYM CRYPT_RC2_56BIT_VERSION}
  CRYPT_RC2_56BIT_VERSION = 52;
  {$EXTERNALSYM CRYPT_RC2_64BIT_VERSION}
  CRYPT_RC2_64BIT_VERSION = 120;
  {$EXTERNALSYM CRYPT_RC2_128BIT_VERSION}
  CRYPT_RC2_128BIT_VERSION = 58;

//+-------------------------------------------------------------------------
//  PKCS_SMIME_CAPABILITIES
//  szOID_RSA_SMIMECapabilities
//
//  pvStructInfo points to following CRYPT_SMIME_CAPABILITIES data structure.
//
//  Note, for CryptEncodeObject(X509_ASN_ENCODING), Parameters.cbData == 0
//  causes the encoded parameters to be omitted and not encoded as a NULL
//  (05 00) as is done when encoding a CRYPT_ALGORITHM_IDENTIFIER. This
//  is per the SMIME specification for encoding capabilities.
//--------------------------------------------------------------------------
// certenrolls_begin -- CRYPT_SMIME_CAPABILITY
type
  PCryptSMIMECapability = ^TCryptSMIMECapability;
  {$EXTERNALSYM _CRYPT_SMIME_CAPABILITY}
  _CRYPT_SMIME_CAPABILITY = record
    pszObjId: LPSTR;
    Parameters: CRYPT_OBJID_BLOB;
  end;
  {$EXTERNALSYM CRYPT_SMIME_CAPABILITY}
  CRYPT_SMIME_CAPABILITY = _CRYPT_SMIME_CAPABILITY;
  {$EXTERNALSYM PCRYPT_SMIME_CAPABILITY}
  PCRYPT_SMIME_CAPABILITY = ^_CRYPT_SMIME_CAPABILITY;
  TCryptSMIMECapability = _CRYPT_SMIME_CAPABILITY;

  PCryptSMIMECapabilities = ^TCryptSMIMECapabilities;
  {$EXTERNALSYM _CRYPT_SMIME_CAPABILITIES}
  _CRYPT_SMIME_CAPABILITIES = record
    cCapability: DWORD;
    rgCapability: PCryptSMIMECapability;
  end;
  {$EXTERNALSYM CRYPT_SMIME_CAPABILITIES}
  CRYPT_SMIME_CAPABILITIES = _CRYPT_SMIME_CAPABILITIES;
  {$EXTERNALSYM PCRYPT_SMIME_CAPABILITIES}
  PCRYPT_SMIME_CAPABILITIES = ^_CRYPT_SMIME_CAPABILITIES;
  TCryptSMIMECapabilities = _CRYPT_SMIME_CAPABILITIES;
// certenrolls_end

//+-------------------------------------------------------------------------
//  Qualified Certificate Statements Extension Data Structures
//
//  X509_QC_STATEMENTS_EXT
//  szOID_QC_STATEMENTS_EXT
//
//  pvStructInfo points to following CERT_QC_STATEMENTS_EXT_INFO
//  data structure.
//
//  Note, identical to the above except for the names of the fields. Same
//  underlying encode/decode functions are used.
//--------------------------------------------------------------------------
  PCertQCStatement = ^TCertQCStatement;
  {$EXTERNALSYM _CERT_QC_STATEMENT}
  _CERT_QC_STATEMENT = record
    pszStatementId: LPSTR;     // pszObjId
    StatementInfo: CRYPT_OBJID_BLOB;      // OPTIONAL
  end;
  {$EXTERNALSYM CERT_QC_STATEMENT}
  CERT_QC_STATEMENT = _CERT_QC_STATEMENT;
  {$EXTERNALSYM PCERT_QC_STATEMENT}
  PCERT_QC_STATEMENT = ^_CERT_QC_STATEMENT;
  TCertQCStatement = _CERT_QC_STATEMENT;

  PCertQCStatementExtInfo = ^TCertQCStatementExtInfo;
  {$EXTERNALSYM _CERT_QC_STATEMENTS_EXT_INFO}
  _CERT_QC_STATEMENTS_EXT_INFO = record
    cStatement: DWORD;
    rgStatement: PCertQCStatement;
  end;
  {$EXTERNALSYM CERT_QC_STATEMENTS_EXT_INFO}
  CERT_QC_STATEMENTS_EXT_INFO = _CERT_QC_STATEMENTS_EXT_INFO;
  {$EXTERNALSYM PCERT_QC_STATEMENTS_EXT_INFO}
  PCERT_QC_STATEMENTS_EXT_INFO = ^_CERT_QC_STATEMENTS_EXT_INFO;
  TCertQCStatementExtInfo = _CERT_QC_STATEMENTS_EXT_INFO;


// QC Statement Ids

const
// European Union
  {$EXTERNALSYM szOID_QC_EU_COMPLIANCE}
  szOID_QC_EU_COMPLIANCE          = '0.4.0.1862.1.1';
// Secure Signature Creation Device
  {$EXTERNALSYM szOID_QC_SSCD}
  szOID_QC_SSCD                   = '0.4.0.1862.1.4';

//+-------------------------------------------------------------------------
//  X509_OBJECT_IDENTIFIER
//  szOID_ECC_PUBLIC_KEY
//
//  pvStructInfo points to a LPSTR of the dot representation.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  X509_ALGORITHM_IDENTIFIER
//  szOID_ECDSA_SPECIFIED
//
//  pvStructInfo points to a CRYPT_ALGORITHM_IDENTIFIER.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  PKCS_RSA_SSA_PSS_PARAMETERS
//  szOID_RSA_SSA_PSS
//
//  pvStructInfo points to the following CRYPT_RSA_SSA_PSS_PARAMETERS
//  data structure.
//
//  For encoding uses the following defaults if the corresponding field
//  is set to NULL or 0:
//      HashAlgorithm.pszObjId : szOID_OIWSEC_sha1
//      MaskGenAlgorithm.pszObjId : szOID_RSA_MGF1
//      MaskGenAlgorithm.HashAlgorithm.pszObjId : HashAlgorithm.pszObjId
//      dwSaltLength: cbHash
//      dwTrailerField : PKCS_RSA_SSA_PSS_TRAILER_FIELD_BC
//
//  Normally for encoding, only the HashAlgorithm.pszObjId field will
//  need to be set.
//
//  For decoding, all of fields are explicitly set.
//--------------------------------------------------------------------------
type
  PCryptMaskGenAlgorithm = ^TCryptMaskGenAlgorithm;
  {$EXTERNALSYM _CRYPT_MASK_GEN_ALGORITHM}
  _CRYPT_MASK_GEN_ALGORITHM = record
    pszObjId: LPSTR;
    HashAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
  end;
  {$EXTERNALSYM CRYPT_MASK_GEN_ALGORITHM}
  CRYPT_MASK_GEN_ALGORITHM = _CRYPT_MASK_GEN_ALGORITHM;
  {$EXTERNALSYM PCRYPT_MASK_GEN_ALGORITHM}
  PCRYPT_MASK_GEN_ALGORITHM = ^_CRYPT_MASK_GEN_ALGORITHM;
  TCryptMaskGenAlgorithm = _CRYPT_MASK_GEN_ALGORITHM;

  PCryptRSASSAPSSParameters = ^TCryptRSASSAPSSParameters;
  {$EXTERNALSYM _CRYPT_RSA_SSA_PSS_PARAMETERS}
  _CRYPT_RSA_SSA_PSS_PARAMETERS = record
    HashAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    MaskGenAlgorithm: CRYPT_MASK_GEN_ALGORITHM;
    dwSaltLength: DWORD;
    dwTrailerField: DWORD;
  end;
  {$EXTERNALSYM CRYPT_RSA_SSA_PSS_PARAMETERS}
  CRYPT_RSA_SSA_PSS_PARAMETERS = _CRYPT_RSA_SSA_PSS_PARAMETERS;
  {$EXTERNALSYM PCRYPT_RSA_SSA_PSS_PARAMETERS}
  PCRYPT_RSA_SSA_PSS_PARAMETERS = ^_CRYPT_RSA_SSA_PSS_PARAMETERS;
  TCryptRSASSAPSSParameters = _CRYPT_RSA_SSA_PSS_PARAMETERS;

const
  {$EXTERNALSYM PKCS_RSA_SSA_PSS_TRAILER_FIELD_BC}
  PKCS_RSA_SSA_PSS_TRAILER_FIELD_BC = 1;

//+-------------------------------------------------------------------------
//  PKCS_RSAES_OAEP_PARAMETERS
//  szOID_RSAES_OAEP
//
//  pvStructInfo points to the following CRYPT_RSAES_OAEP_PARAMETERS
//  data structure.
//
//  For encoding uses the following defaults if the corresponding field
//  is set to NULL or 0:
//      HashAlgorithm.pszObjId : szOID_OIWSEC_sha1
//      MaskGenAlgorithm.pszObjId : szOID_RSA_MGF1
//      MaskGenAlgorithm.HashAlgorithm.pszObjId : HashAlgorithm.pszObjId
//      PSourceAlgorithm.pszObjId : szOID_RSA_PSPECIFIED
//      PSourceAlgorithm.EncodingParameters.cbData : 0
//      PSourceAlgorithm.EncodingParameters.pbData : NULL
//
//  Normally for encoding, only the HashAlgorithm.pszObjId field will
//  need to be set.
//
//  For decoding, all of fields are explicitly set.
//--------------------------------------------------------------------------
type
  PCryptPSourceAlgorithm = ^TCryptPSourceAlgorithm;
  {$EXTERNALSYM _CRYPT_PSOURCE_ALGORITHM}
  _CRYPT_PSOURCE_ALGORITHM = record
    pszObjId: LPSTR;
    EncodingParameters: CRYPT_DATA_BLOB;
  end;
  {$EXTERNALSYM CRYPT_PSOURCE_ALGORITHM}
  CRYPT_PSOURCE_ALGORITHM = _CRYPT_PSOURCE_ALGORITHM;
  {$EXTERNALSYM PCRYPT_PSOURCE_ALGORITHM}
  PCRYPT_PSOURCE_ALGORITHM = ^_CRYPT_PSOURCE_ALGORITHM;
  TCryptPSourceAlgorithm = _CRYPT_PSOURCE_ALGORITHM;

  PCryptRSAESOAEPParameters = ^TCryptRSAESOAEPParameters;
  {$EXTERNALSYM _CRYPT_RSAES_OAEP_PARAMETERS}
  _CRYPT_RSAES_OAEP_PARAMETERS = record
    HashAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    MaskGenAlgorithm: CRYPT_MASK_GEN_ALGORITHM;
    PSourceAlgorithm: CRYPT_PSOURCE_ALGORITHM;
  end;
  {$EXTERNALSYM CRYPT_RSAES_OAEP_PARAMETERS}
  CRYPT_RSAES_OAEP_PARAMETERS = _CRYPT_RSAES_OAEP_PARAMETERS;
  {$EXTERNALSYM PCRYPT_RSAES_OAEP_PARAMETERS}
  PCRYPT_RSAES_OAEP_PARAMETERS = ^_CRYPT_RSAES_OAEP_PARAMETERS;
  TCryptRSAESOAEPParameters = _CRYPT_RSAES_OAEP_PARAMETERS;

//+-------------------------------------------------------------------------
//  PKCS7_SIGNER_INFO
//
//  pvStructInfo points to CMSG_SIGNER_INFO.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMS_SIGNER_INFO
//
//  pvStructInfo points to CMSG_CMS_SIGNER_INFO.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  Verisign Certificate Extension Object Identifiers
//--------------------------------------------------------------------------

const
// Octet String containing Boolean
  {$EXTERNALSYM szOID_VERISIGN_PRIVATE_6_9}
  szOID_VERISIGN_PRIVATE_6_9       = '2.16.840.1.113733.1.6.9';

// Octet String containing IA5 string: lower case 32 char hex string
  {$EXTERNALSYM szOID_VERISIGN_ONSITE_JURISDICTION_HASH}
  szOID_VERISIGN_ONSITE_JURISDICTION_HASH = '2.16.840.1.113733.1.6.11';

// Octet String containing Bit string
  {$EXTERNALSYM szOID_VERISIGN_BITSTRING_6_13}
  szOID_VERISIGN_BITSTRING_6_13    = '2.16.840.1.113733.1.6.13';

// EKU
  {$EXTERNALSYM szOID_VERISIGN_ISS_STRONG_CRYPTO}
  szOID_VERISIGN_ISS_STRONG_CRYPTO = '2.16.840.1.113733.1.8.1';

//+-------------------------------------------------------------------------
//  Netscape Certificate Extension Object Identifiers
//--------------------------------------------------------------------------
  {$EXTERNALSYM szOID_NETSCAPE}
  szOID_NETSCAPE                  = '2.16.840.1.113730';
  {$EXTERNALSYM szOID_NETSCAPE_CERT_EXTENSION}
  szOID_NETSCAPE_CERT_EXTENSION   = '2.16.840.1.113730.1';
  {$EXTERNALSYM szOID_NETSCAPE_CERT_TYPE}
  szOID_NETSCAPE_CERT_TYPE        = '2.16.840.1.113730.1.1';
  {$EXTERNALSYM szOID_NETSCAPE_BASE_URL}
  szOID_NETSCAPE_BASE_URL         = '2.16.840.1.113730.1.2';
  {$EXTERNALSYM szOID_NETSCAPE_REVOCATION_URL}
  szOID_NETSCAPE_REVOCATION_URL   = '2.16.840.1.113730.1.3';
  {$EXTERNALSYM szOID_NETSCAPE_CA_REVOCATION_URL}
  szOID_NETSCAPE_CA_REVOCATION_URL = '2.16.840.1.113730.1.4';
  {$EXTERNALSYM szOID_NETSCAPE_CERT_RENEWAL_URL}
  szOID_NETSCAPE_CERT_RENEWAL_URL = '2.16.840.1.113730.1.7';
  {$EXTERNALSYM szOID_NETSCAPE_CA_POLICY_URL}
  szOID_NETSCAPE_CA_POLICY_URL    = '2.16.840.1.113730.1.8';
  {$EXTERNALSYM szOID_NETSCAPE_SSL_SERVER_NAME}
  szOID_NETSCAPE_SSL_SERVER_NAME  = '2.16.840.1.113730.1.12';
  {$EXTERNALSYM szOID_NETSCAPE_COMMENT}
  szOID_NETSCAPE_COMMENT          = '2.16.840.1.113730.1.13';

//+-------------------------------------------------------------------------
//  Netscape Certificate Data Type Object Identifiers
//--------------------------------------------------------------------------
  {$EXTERNALSYM szOID_NETSCAPE_DATA_TYPE}
  szOID_NETSCAPE_DATA_TYPE        = '2.16.840.1.113730.2';
  {$EXTERNALSYM szOID_NETSCAPE_CERT_SEQUENCE}
  szOID_NETSCAPE_CERT_SEQUENCE    = '2.16.840.1.113730.2.5';

//+-------------------------------------------------------------------------
//  szOID_NETSCAPE_CERT_TYPE extension
//
//  Its value is a bit string. CryptDecodeObject/CryptEncodeObject using
//  X509_BITS or X509_BITS_WITHOUT_TRAILING_ZEROES.
//
//  The following bits are defined:
//--------------------------------------------------------------------------
  {$EXTERNALSYM NETSCAPE_SSL_CLIENT_AUTH_CERT_TYPE}
  NETSCAPE_SSL_CLIENT_AUTH_CERT_TYPE = $80;
  {$EXTERNALSYM NETSCAPE_SSL_SERVER_AUTH_CERT_TYPE}
  NETSCAPE_SSL_SERVER_AUTH_CERT_TYPE = $40;
  {$EXTERNALSYM NETSCAPE_SMIME_CERT_TYPE}
  NETSCAPE_SMIME_CERT_TYPE = $20;
  {$EXTERNALSYM NETSCAPE_SIGN_CERT_TYPE}
  NETSCAPE_SIGN_CERT_TYPE = $10;
  {$EXTERNALSYM NETSCAPE_SSL_CA_CERT_TYPE}
  NETSCAPE_SSL_CA_CERT_TYPE = $04;
  {$EXTERNALSYM NETSCAPE_SMIME_CA_CERT_TYPE}
  NETSCAPE_SMIME_CA_CERT_TYPE = $02;
  {$EXTERNALSYM NETSCAPE_SIGN_CA_CERT_TYPE}
  NETSCAPE_SIGN_CA_CERT_TYPE = $01;

//+-------------------------------------------------------------------------
//  szOID_NETSCAPE_BASE_URL extension
//
//  Its value is an IA5_STRING. CryptDecodeObject/CryptEncodeObject using
//  X509_ANY_STRING or X509_UNICODE_ANY_STRING, where,
//  dwValueType = CERT_RDN_IA5_STRING.
//
//  When present this string is added to the beginning of all relative URLs
//  in the certificate.  This extension can be considered an optimization
//  to reduce the size of the URL extensions.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  szOID_NETSCAPE_REVOCATION_URL extension
//
//  Its value is an IA5_STRING. CryptDecodeObject/CryptEncodeObject using
//  X509_ANY_STRING or X509_UNICODE_ANY_STRING, where,
//  dwValueType = CERT_RDN_IA5_STRING.
//
//  It is a relative or absolute URL that can be used to check the
//  revocation status of a certificate. The revocation check will be
//  performed as an HTTP GET method using a url that is the concatenation of
//  revocation-url and certificate-serial-number.
//  Where the certificate-serial-number is encoded as a string of
//  ascii hexadecimal digits. For example, if the netscape-base-url is
//  https://www.certs-r-us.com/, the netscape-revocation-url is
//  cgi-bin/check-rev.cgi?, and the certificate serial number is 173420,
//  the resulting URL would be:
//  https://www.certs-r-us.com/cgi-bin/check-rev.cgi?02a56c
//
//  The server should return a document with a Content-Type of
//  application/x-netscape-revocation.  The document should contain
//  a single ascii digit, '1' if the certificate is not curently valid,
//  and '0' if it is curently valid.
//
//  Note: for all of the URLs that include the certificate serial number,
//  the serial number will be encoded as a string which consists of an even
//  number of hexadecimal digits.  If the number of significant digits is odd,
//  the string will have a single leading zero to ensure an even number of
//  digits is generated.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  szOID_NETSCAPE_CA_REVOCATION_URL extension
//
//  Its value is an IA5_STRING. CryptDecodeObject/CryptEncodeObject using
//  X509_ANY_STRING or X509_UNICODE_ANY_STRING, where,
//  dwValueType = CERT_RDN_IA5_STRING.
//
//  It is a relative or absolute URL that can be used to check the
//  revocation status of any certificates that are signed by the CA that
//  this certificate belongs to. This extension is only valid in CA
//  certificates.  The use of this extension is the same as the above
//  szOID_NETSCAPE_REVOCATION_URL extension.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  szOID_NETSCAPE_CERT_RENEWAL_URL extension
//
//  Its value is an IA5_STRING. CryptDecodeObject/CryptEncodeObject using
//  X509_ANY_STRING or X509_UNICODE_ANY_STRING, where,
//  dwValueType = CERT_RDN_IA5_STRING.
//
//  It is a relative or absolute URL that points to a certificate renewal
//  form. The renewal form will be accessed with an HTTP GET method using a
//  url that is the concatenation of renewal-url and
//  certificate-serial-number. Where the certificate-serial-number is
//  encoded as a string of ascii hexadecimal digits. For example, if the
//  netscape-base-url is https://www.certs-r-us.com/, the
//  netscape-cert-renewal-url is cgi-bin/check-renew.cgi?, and the
//  certificate serial number is 173420, the resulting URL would be:
//  https://www.certs-r-us.com/cgi-bin/check-renew.cgi?02a56c
//  The document returned should be an HTML form that will allow the user
//  to request a renewal of their certificate.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  szOID_NETSCAPE_CA_POLICY_URL extension
//
//  Its value is an IA5_STRING. CryptDecodeObject/CryptEncodeObject using
//  X509_ANY_STRING or X509_UNICODE_ANY_STRING, where,
//  dwValueType = CERT_RDN_IA5_STRING.
//
//  It is a relative or absolute URL that points to a web page that
//  describes the policies under which the certificate was issued.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  szOID_NETSCAPE_SSL_SERVER_NAME extension
//
//  Its value is an IA5_STRING. CryptDecodeObject/CryptEncodeObject using
//  X509_ANY_STRING or X509_UNICODE_ANY_STRING, where,
//  dwValueType = CERT_RDN_IA5_STRING.
//
//  It is a 'shell expression' that can be used to match the hostname of the
//  SSL server that is using this certificate.  It is recommended that if
//  the server's hostname does not match this pattern the user be notified
//  and given the option to terminate the SSL connection.  If this extension
//  is not present then the CommonName in the certificate subject's
//  distinguished name is used for the same purpose.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  szOID_NETSCAPE_COMMENT extension
//
//  Its value is an IA5_STRING. CryptDecodeObject/CryptEncodeObject using
//  X509_ANY_STRING or X509_UNICODE_ANY_STRING, where,
//  dwValueType = CERT_RDN_IA5_STRING.
//
//  It is a comment that may be displayed to the user when the certificate
//  is viewed.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  szOID_NETSCAPE_CERT_SEQUENCE
//
//  Its value is a PKCS#7 ContentInfo structure wrapping a sequence of
//  certificates. The value of the contentType field is
//  szOID_NETSCAPE_CERT_SEQUENCE, while the content field is the following
//  structure:
//      CertificateSequence ::= SEQUENCE OF Certificate.
//
//  CryptDecodeObject/CryptEncodeObject using
//  PKCS_CONTENT_INFO_SEQUENCE_OF_ANY, where,
//  pszObjId = szOID_NETSCAPE_CERT_SEQUENCE and the CRYPT_DER_BLOBs point
//  to encoded X509 certificates.
//--------------------------------------------------------------------------

//+=========================================================================
//  Certificate Management Messages over CMS (CMC) Data Structures
//==========================================================================

// Content Type (request)
  {$EXTERNALSYM szOID_CT_PKI_DATA}
  szOID_CT_PKI_DATA               = '1.3.6.1.5.5.7.12.2';

// Content Type (response)
  {$EXTERNALSYM szOID_CT_PKI_RESPONSE}
  szOID_CT_PKI_RESPONSE           = '1.3.6.1.5.5.7.12.3';

// Signature value that only contains the hash octets. The parameters for
// this algorithm must be present and must be encoded as NULL.
  {$EXTERNALSYM szOID_PKIX_NO_SIGNATURE}
  szOID_PKIX_NO_SIGNATURE         = '1.3.6.1.5.5.7.6.2';

  {$EXTERNALSYM szOID_CMC}
  szOID_CMC                       = '1.3.6.1.5.5.7.7';
  {$EXTERNALSYM szOID_CMC_STATUS_INFO}
  szOID_CMC_STATUS_INFO           = '1.3.6.1.5.5.7.7.1';
  {$EXTERNALSYM szOID_CMC_IDENTIFICATION}
  szOID_CMC_IDENTIFICATION        = '1.3.6.1.5.5.7.7.2';
  {$EXTERNALSYM szOID_CMC_IDENTITY_PROOF}
  szOID_CMC_IDENTITY_PROOF        = '1.3.6.1.5.5.7.7.3';
  {$EXTERNALSYM szOID_CMC_DATA_RETURN}
  szOID_CMC_DATA_RETURN           = '1.3.6.1.5.5.7.7.4';

// Transaction Id (integer)
  {$EXTERNALSYM szOID_CMC_TRANSACTION_ID}
  szOID_CMC_TRANSACTION_ID        = '1.3.6.1.5.5.7.7.5';

// Sender Nonce (octet string)
  {$EXTERNALSYM szOID_CMC_SENDER_NONCE}
  szOID_CMC_SENDER_NONCE          = '1.3.6.1.5.5.7.7.6';

// Recipient Nonce (octet string)
  {$EXTERNALSYM szOID_CMC_RECIPIENT_NONCE}
  szOID_CMC_RECIPIENT_NONCE       = '1.3.6.1.5.5.7.7.7';

  {$EXTERNALSYM szOID_CMC_ADD_EXTENSIONS}
  szOID_CMC_ADD_EXTENSIONS        = '1.3.6.1.5.5.7.7.8';
  {$EXTERNALSYM szOID_CMC_ENCRYPTED_POP}
  szOID_CMC_ENCRYPTED_POP         = '1.3.6.1.5.5.7.7.9';
  {$EXTERNALSYM szOID_CMC_DECRYPTED_POP}
  szOID_CMC_DECRYPTED_POP         = '1.3.6.1.5.5.7.7.10';
  {$EXTERNALSYM szOID_CMC_LRA_POP_WITNESS}
  szOID_CMC_LRA_POP_WITNESS       = '1.3.6.1.5.5.7.7.11';

// Issuer Name + Serial
  {$EXTERNALSYM szOID_CMC_GET_CERT}
  szOID_CMC_GET_CERT              = '1.3.6.1.5.5.7.7.15';

// Issuer Name [+ CRL Name] + Time [+ Reasons]
  {$EXTERNALSYM szOID_CMC_GET_CRL}
  szOID_CMC_GET_CRL               = '1.3.6.1.5.5.7.7.16';

// Issuer Name + Serial [+ Reason] [+ Effective Time] [+ Secret] [+ Comment]
  {$EXTERNALSYM szOID_CMC_REVOKE_REQUEST}
  szOID_CMC_REVOKE_REQUEST        = '1.3.6.1.5.5.7.7.17';

// (octet string) URL-style parameter list (IA5?)
  {$EXTERNALSYM szOID_CMC_REG_INFO}
  szOID_CMC_REG_INFO              = '1.3.6.1.5.5.7.7.18';

  {$EXTERNALSYM szOID_CMC_RESPONSE_INFO}
  szOID_CMC_RESPONSE_INFO         = '1.3.6.1.5.5.7.7.19';

// (octet string)
  {$EXTERNALSYM szOID_CMC_QUERY_PENDING}
  szOID_CMC_QUERY_PENDING         = '1.3.6.1.5.5.7.7.21';
  {$EXTERNALSYM szOID_CMC_ID_POP_LINK_RANDOM}
  szOID_CMC_ID_POP_LINK_RANDOM    = '1.3.6.1.5.5.7.7.22';
  {$EXTERNALSYM szOID_CMC_ID_POP_LINK_WITNESS}
  szOID_CMC_ID_POP_LINK_WITNESS   = '1.3.6.1.5.5.7.7.23';

// optional Name + Integer
  {$EXTERNALSYM szOID_CMC_ID_CONFIRM_CERT_ACCEPTANCE}
  szOID_CMC_ID_CONFIRM_CERT_ACCEPTANCE = '1.3.6.1.5.5.7.7.24';

  {$EXTERNALSYM szOID_CMC_ADD_ATTRIBUTES}
  szOID_CMC_ADD_ATTRIBUTES        = '1.3.6.1.4.1.311.10.10.1';

//+-------------------------------------------------------------------------
//  CMC_DATA
//  CMC_RESPONSE
//
//  Certificate Management Messages over CMS (CMC) PKIData and Response
//  messages.
//
//  For CMC_DATA, pvStructInfo points to a CMC_DATA_INFO.
//  CMC_DATA_INFO contains optional arrays of tagged attributes, requests,
//  content info and/or arbitrary other messages.
//
//  For CMC_RESPONSE, pvStructInfo points to a CMC_RESPONSE_INFO.
//  CMC_RESPONSE_INFO is the same as CMC_DATA_INFO without the tagged
//  requests.
//--------------------------------------------------------------------------
type
  PCMCTaggedAttribute = ^TCMCTaggedAttribute;
  {$EXTERNALSYM _CMC_TAGGED_ATTRIBUTE}
  _CMC_TAGGED_ATTRIBUTE = record
    dwBodyPartID: DWORD;
    Attribute: CRYPT_ATTRIBUTE;
  end;
  {$EXTERNALSYM CMC_TAGGED_ATTRIBUTE}
  CMC_TAGGED_ATTRIBUTE = _CMC_TAGGED_ATTRIBUTE;
  {$EXTERNALSYM PCMC_TAGGED_ATTRIBUTE}
  PCMC_TAGGED_ATTRIBUTE = ^_CMC_TAGGED_ATTRIBUTE;
  TCMCTaggedAttribute = _CMC_TAGGED_ATTRIBUTE;

  PCMCTaggedCertRequest = ^TCMCTaggedCertRequest;
  {$EXTERNALSYM _CMC_TAGGED_CERT_REQUEST}
  _CMC_TAGGED_CERT_REQUEST = record
    dwBodyPartID: DWORD;
    SignedCertRequest: CRYPT_DER_BLOB;
  end;
  {$EXTERNALSYM CMC_TAGGED_CERT_REQUEST}
  CMC_TAGGED_CERT_REQUEST = _CMC_TAGGED_CERT_REQUEST;
  {$EXTERNALSYM PCMC_TAGGED_CERT_REQUEST}
  PCMC_TAGGED_CERT_REQUEST = ^_CMC_TAGGED_CERT_REQUEST;
  TCMCTaggedCertRequest = _CMC_TAGGED_CERT_REQUEST;

  PCMCTaggedRequest = ^TCMCTaggedRequest;
  {$EXTERNALSYM _CMC_TAGGED_REQUEST}
  _CMC_TAGGED_REQUEST = record
    case dwTaggedRequestChoice: DWORD of
      // CMC_TAGGED_CERT_REQUEST_CHOICE
      1: (pTaggedCertRequest: PCMCTaggedCertRequest);
  end;
  {$EXTERNALSYM CMC_TAGGED_REQUEST}
  CMC_TAGGED_REQUEST = _CMC_TAGGED_REQUEST;
  {$EXTERNALSYM PCMC_TAGGED_REQUEST}
  PCMC_TAGGED_REQUEST = ^_CMC_TAGGED_REQUEST;
  TCMCTaggedRequest = _CMC_TAGGED_REQUEST;

const
  {$EXTERNALSYM CMC_TAGGED_CERT_REQUEST_CHOICE}
  CMC_TAGGED_CERT_REQUEST_CHOICE = 1;

type
  PCMCTaggedContentInfo = ^TCMCTaggedContentInfo;
  {$EXTERNALSYM _CMC_TAGGED_CONTENT_INFO}
  _CMC_TAGGED_CONTENT_INFO = record
    dwBodyPartID: DWORD;
    EncodedContentInfo: CRYPT_DER_BLOB;
  end;
  {$EXTERNALSYM CMC_TAGGED_CONTENT_INFO}
  CMC_TAGGED_CONTENT_INFO = _CMC_TAGGED_CONTENT_INFO;
  {$EXTERNALSYM PCMC_TAGGED_CONTENT_INFO}
  PCMC_TAGGED_CONTENT_INFO = ^_CMC_TAGGED_CONTENT_INFO;
  TCMCTaggedContentInfo = _CMC_TAGGED_CONTENT_INFO;

  PCMCTaggedOtherMsg = ^TCMCTaggedOtherMsg;
  {$EXTERNALSYM _CMC_TAGGED_OTHER_MSG}
  _CMC_TAGGED_OTHER_MSG = record
    dwBodyPartID: DWORD;
    pszObjId: LPSTR;
    Value: CRYPT_OBJID_BLOB;
  end;
  {$EXTERNALSYM CMC_TAGGED_OTHER_MSG}
  CMC_TAGGED_OTHER_MSG = _CMC_TAGGED_OTHER_MSG;
  {$EXTERNALSYM PCMC_TAGGED_OTHER_MSG}
  PCMC_TAGGED_OTHER_MSG = ^_CMC_TAGGED_OTHER_MSG;
  TCMCTaggedOtherMsg = _CMC_TAGGED_OTHER_MSG;

// All the tagged arrays are optional
  PCMCDataInfo = ^TCMCDataInfo;
  {$EXTERNALSYM _CMC_DATA_INFO}
  _CMC_DATA_INFO = record
    cTaggedAttribute: DWORD;
    rgTaggedAttribute: PCMCTaggedAttribute;
    cTaggedRequest: DWORD;
    rgTaggedRequest: PCMCTaggedRequest;
    cTaggedContentInfo: DWORD;
    rgTaggedContentInfo: PCMCTaggedContentInfo;
    cTaggedOtherMsg: DWORD;
    rgTaggedOtherMsg: PCMCTaggedOtherMsg;
  end;
  {$EXTERNALSYM CMC_DATA_INFO}
  CMC_DATA_INFO = _CMC_DATA_INFO;
  {$EXTERNALSYM PCMC_DATA_INFO}
  PCMC_DATA_INFO = ^_CMC_DATA_INFO;
  TCMCDataInfo = _CMC_DATA_INFO;

// All the tagged arrays are optional
  PCMCResponseInfo = ^TCMCResponseInfo;
  {$EXTERNALSYM _CMC_RESPONSE_INFO}
  _CMC_RESPONSE_INFO = record
    cTaggedAttribute: DWORD;
    rgTaggedAttribute: PCMCTaggedAttribute;
    cTaggedContentInfo: DWORD;
    rgTaggedContentInfo: PCMCTaggedContentInfo;
    cTaggedOtherMsg: DWORD;
    rgTaggedOtherMsg: PCMCTaggedOtherMsg;
  end;
  {$EXTERNALSYM CMC_RESPONSE_INFO}
  CMC_RESPONSE_INFO = _CMC_RESPONSE_INFO;
  {$EXTERNALSYM PCMC_RESPONSE_INFO}
  PCMC_RESPONSE_INFO = ^_CMC_RESPONSE_INFO;
  TCMCResponseInfo = _CMC_RESPONSE_INFO;

//+-------------------------------------------------------------------------
//  CMC_STATUS
//
//  Certificate Management Messages over CMS (CMC) Status.
//
//  pvStructInfo points to a CMC_STATUS_INFO.
//--------------------------------------------------------------------------
  PCMCPendInfo = ^TCMCPendInfo;
  {$EXTERNALSYM _CMC_PEND_INFO}
  _CMC_PEND_INFO = record
    PendToken: CRYPT_DATA_BLOB;
    PendTime: FILETIME;
  end;
  {$EXTERNALSYM CMC_PEND_INFO}
  CMC_PEND_INFO = _CMC_PEND_INFO;
  {$EXTERNALSYM PCMC_PEND_INFO}
  PCMC_PEND_INFO = ^_CMC_PEND_INFO;
  TCMCPendInfo = _CMC_PEND_INFO;

const
  {$EXTERNALSYM CMC_OTHER_INFO_NO_CHOICE}
  CMC_OTHER_INFO_NO_CHOICE = 0;
  {$EXTERNALSYM CMC_OTHER_INFO_FAIL_CHOICE}
  CMC_OTHER_INFO_FAIL_CHOICE = 1;
  {$EXTERNALSYM CMC_OTHER_INFO_PEND_CHOICE}
  CMC_OTHER_INFO_PEND_CHOICE = 2;

type
  PCMCStatusInfo = ^TCMCStatusInfo;
  {$EXTERNALSYM _CMC_STATUS_INFO}
  _CMC_STATUS_INFO = record
    dwStatus: DWORD;
    cBodyList: DWORD;
    rgdwBodyList: ^DWORD;
    pwszStatusString: LPWSTR;   // OPTIONAL
    case dwOtherInfoChoice: DWORD of
      CMC_OTHER_INFO_NO_CHOICE: ({none});
      CMC_OTHER_INFO_FAIL_CHOICE: (dwFailInfo: DWORD);
      CMC_OTHER_INFO_PEND_CHOICE: (pPendInfo: PCMCPendInfo);
  end;
  {$EXTERNALSYM CMC_STATUS_INFO}
  CMC_STATUS_INFO = _CMC_STATUS_INFO;
  {$EXTERNALSYM PCMC_STATUS_INFO}
  PCMC_STATUS_INFO = ^_CMC_STATUS_INFO;
  TCMCStatusInfo = _CMC_STATUS_INFO;

//
// dwStatus values
//

// Request was granted
const
  {$EXTERNALSYM CMC_STATUS_SUCCESS}
  CMC_STATUS_SUCCESS = 0;

// Request failed, more information elsewhere in the message
  {$EXTERNALSYM CMC_STATUS_FAILED}
  CMC_STATUS_FAILED = 2;

// The request body part has not yet been processed. Requester is responsible
// to poll back. May only be returned for certificate request operations.
  {$EXTERNALSYM CMC_STATUS_PENDING}
  CMC_STATUS_PENDING = 3;

// The requested operation is not supported
  {$EXTERNALSYM CMC_STATUS_NO_SUPPORT}
  CMC_STATUS_NO_SUPPORT = 4;

// Confirmation using the idConfirmCertAcceptance control is required
// before use of certificate
  {$EXTERNALSYM CMC_STATUS_CONFIRM_REQUIRED}
  CMC_STATUS_CONFIRM_REQUIRED = 5;

//
// dwFailInfo values
//

// Unrecognized or unsupported algorithm
  {$EXTERNALSYM CMC_FAIL_BAD_ALG}
  CMC_FAIL_BAD_ALG = 0;

// Integrity check failed
  {$EXTERNALSYM CMC_FAIL_BAD_MESSAGE_CHECK}
  CMC_FAIL_BAD_MESSAGE_CHECK = 1;

// Transaction not permitted or supported
  {$EXTERNALSYM CMC_FAIL_BAD_REQUEST}
  CMC_FAIL_BAD_REQUEST = 2;

// Message time field was not sufficiently close to the system time
  {$EXTERNALSYM CMC_FAIL_BAD_TIME}
  CMC_FAIL_BAD_TIME = 3;

// No certificate could be identified matching the provided criteria
  {$EXTERNALSYM CMC_FAIL_BAD_CERT_ID}
  CMC_FAIL_BAD_CERT_ID = 4;

// A requested X.509 extension is not supported by the recipient CA.
  {$EXTERNALSYM CMC_FAIL_UNSUPORTED_EXT}
  CMC_FAIL_UNSUPORTED_EXT = 5;

// Private key material must be supplied
  {$EXTERNALSYM CMC_FAIL_MUST_ARCHIVE_KEYS}
  CMC_FAIL_MUST_ARCHIVE_KEYS = 6;

// Identification Attribute failed to verify
  {$EXTERNALSYM CMC_FAIL_BAD_IDENTITY}
  CMC_FAIL_BAD_IDENTITY = 7;

// Server requires a POP proof before issuing certificate
  {$EXTERNALSYM CMC_FAIL_POP_REQUIRED}
  CMC_FAIL_POP_REQUIRED = 8;

// POP processing failed
  {$EXTERNALSYM CMC_FAIL_POP_FAILED}
  CMC_FAIL_POP_FAILED = 9;

// Server policy does not allow key re-use
  {$EXTERNALSYM CMC_FAIL_NO_KEY_REUSE}
  CMC_FAIL_NO_KEY_REUSE = 10;

  {$EXTERNALSYM CMC_FAIL_INTERNAL_CA_ERROR}
  CMC_FAIL_INTERNAL_CA_ERROR = 11;

  {$EXTERNALSYM CMC_FAIL_TRY_LATER}
  CMC_FAIL_TRY_LATER = 12;

//+-------------------------------------------------------------------------
//  CMC_ADD_EXTENSIONS
//
//  Certificate Management Messages over CMS (CMC) Add Extensions control
//  attribute.
//
//  pvStructInfo points to a CMC_ADD_EXTENSIONS_INFO.
//--------------------------------------------------------------------------
type
  PCMCAddExtensionsInfo = ^TCMCAddExtensionsInfo;
  {$EXTERNALSYM _CMC_ADD_EXTENSIONS_INFO}
  _CMC_ADD_EXTENSIONS_INFO = record
    dwCmcDataReference: DWORD;
    cCertReference: DWORD;
    rgdwCertReference: PDWORD;
    cExtension: DWORD;
    rgExtension: PCertExtension;
  end;
  {$EXTERNALSYM CMC_ADD_EXTENSIONS_INFO}
  CMC_ADD_EXTENSIONS_INFO = _CMC_ADD_EXTENSIONS_INFO;
  {$EXTERNALSYM PCMC_ADD_EXTENSIONS_INFO}
  PCMC_ADD_EXTENSIONS_INFO = ^_CMC_ADD_EXTENSIONS_INFO;
  TCMCAddExtensionsInfo = _CMC_ADD_EXTENSIONS_INFO;

//+-------------------------------------------------------------------------
//  CMC_ADD_ATTRIBUTES
//
//  Certificate Management Messages over CMS (CMC) Add Attributes control
//  attribute.
//
//  pvStructInfo points to a CMC_ADD_ATTRIBUTES_INFO.
//--------------------------------------------------------------------------
  PCMCAddAttributesInfo = ^TCMCAddAttributesInfo;
  {$EXTERNALSYM _CMC_ADD_ATTRIBUTES_INFO}
  _CMC_ADD_ATTRIBUTES_INFO = record
    dwCmcDataReference: DWORD;
    cCertReference: DWORD;
    rgdwCertReference: PDWORD;
    cAttribute: DWORD;
    rgAttribute: PCryptAttribute;
  end;
  {$EXTERNALSYM CMC_ADD_ATTRIBUTES_INFO}
  CMC_ADD_ATTRIBUTES_INFO = _CMC_ADD_ATTRIBUTES_INFO;
  {$EXTERNALSYM PCMC_ADD_ATTRIBUTES_INFO}
  PCMC_ADD_ATTRIBUTES_INFO = ^_CMC_ADD_ATTRIBUTES_INFO;
  TCMCAddAttributesInfo = _CMC_ADD_ATTRIBUTES_INFO;

//+-------------------------------------------------------------------------
//  X509_CERTIFICATE_TEMPLATE
//  szOID_CERTIFICATE_TEMPLATE
//
//  pvStructInfo points to following CERT_TEMPLATE_EXT data structure.
//
//--------------------------------------------------------------------------
  PCertTemplateExt = ^TCertTemplateExt;
  {$EXTERNALSYM _CERT_TEMPLATE_EXT}
  _CERT_TEMPLATE_EXT = record
    pszObjId: LPSTR;
    dwMajorVersion: DWORD;
    fMinorVersion: BOOL;      // TRUE for a minor version
    dwMinorVersion: DWORD;
  end;
  {$EXTERNALSYM CERT_TEMPLATE_EXT}
  CERT_TEMPLATE_EXT = _CERT_TEMPLATE_EXT;
  {$EXTERNALSYM PCERT_TEMPLATE_EXT}
  PCERT_TEMPLATE_EXT = ^_CERT_TEMPLATE_EXT;
  TCertTemplateExt = _CERT_TEMPLATE_EXT;

//+=========================================================================
//  Logotype Extension Data Structures
//
//  X509_LOGOTYPE_EXT
//  szOID_LOGOTYPE_EXT
//
//  pvStructInfo points to a CERT_LOGOTYPE_EXT_INFO.
//==========================================================================
  PCertHashedURL = ^TCertHashedURL;
  {$EXTERNALSYM _CERT_HASHED_URL}
  _CERT_HASHED_URL = record
    HashAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    Hash: CRYPT_HASH_BLOB;
    pwszUrl: LPWSTR;    // Encoded as IA5, Optional for
                        // biometric data
  end;
  {$EXTERNALSYM CERT_HASHED_URL}
  CERT_HASHED_URL = _CERT_HASHED_URL;
  {$EXTERNALSYM PCERT_HASHED_URL}
  PCERT_HASHED_URL = ^_CERT_HASHED_URL;
  TCertHashedURL = _CERT_HASHED_URL;

  PCertLogoTypeDetails = ^TCertLogoTypeDetails;
  {$EXTERNALSYM _CERT_LOGOTYPE_DETAILS}
  _CERT_LOGOTYPE_DETAILS = record
    pwszMimeType: LPWSTR;   // Encoded as IA5
    cHashedUrl: DWORD;
    rgHashedUrl: PCertHashedURL;
  end;
  {$EXTERNALSYM CERT_LOGOTYPE_DETAILS}
  CERT_LOGOTYPE_DETAILS = _CERT_LOGOTYPE_DETAILS;
  {$EXTERNALSYM PCERT_LOGOTYPE_DETAILS}
  PCERT_LOGOTYPE_DETAILS = ^_CERT_LOGOTYPE_DETAILS;
  TCertLogoTypeDetails = _CERT_LOGOTYPE_DETAILS;

  PCertLogoTypeReference = ^TCertLogoTypeReference;
  {$EXTERNALSYM _CERT_LOGOTYPE_REFERENCE}
  _CERT_LOGOTYPE_REFERENCE = record
    cHashedUrl: DWORD;
    rgHashedUrl: PCertHashedURL;
  end;
  {$EXTERNALSYM CERT_LOGOTYPE_REFERENCE}
  CERT_LOGOTYPE_REFERENCE = _CERT_LOGOTYPE_REFERENCE;
  {$EXTERNALSYM PCERT_LOGOTYPE_REFERENCE}
  PCERT_LOGOTYPE_REFERENCE = ^_CERT_LOGOTYPE_REFERENCE;
  TCertLogoTypeReference = _CERT_LOGOTYPE_REFERENCE;

const
  {$EXTERNALSYM CERT_LOGOTYPE_GRAY_SCALE_IMAGE_INFO_CHOICE}
  CERT_LOGOTYPE_GRAY_SCALE_IMAGE_INFO_CHOICE = 1;
  {$EXTERNALSYM CERT_LOGOTYPE_COLOR_IMAGE_INFO_CHOICE}
  CERT_LOGOTYPE_COLOR_IMAGE_INFO_CHOICE = 2;

  {$EXTERNALSYM CERT_LOGOTYPE_NO_IMAGE_RESOLUTION_CHOICE}
  CERT_LOGOTYPE_NO_IMAGE_RESOLUTION_CHOICE = 0;
  {$EXTERNALSYM CERT_LOGOTYPE_BITS_IMAGE_RESOLUTION_CHOICE}
  CERT_LOGOTYPE_BITS_IMAGE_RESOLUTION_CHOICE = 1;
  {$EXTERNALSYM CERT_LOGOTYPE_TABLE_SIZE_IMAGE_RESOLUTION_CHOICE}
  CERT_LOGOTYPE_TABLE_SIZE_IMAGE_RESOLUTION_CHOICE = 2;

type  
  PCertLogoTypeImageInfo = ^TCertLogoTypeImageInfo;
  {$EXTERNALSYM _CERT_LOGOTYPE_IMAGE_INFO}
  _CERT_LOGOTYPE_IMAGE_INFO = record
    // CERT_LOGOTYPE_GRAY_SCALE_IMAGE_INFO_CHOICE or
    // CERT_LOGOTYPE_COLOR_IMAGE_INFO_CHOICE
    dwLogotypeImageInfoChoice: DWORD;

    dwFileSize: DWORD;          // In octets
    dwXSize: DWORD;             // Horizontal size in pixels
    dwYSize: DWORD;             // Vertical size in pixels

    case dwLogotypeImageResolutionChoice: DWORD of
      CERT_LOGOTYPE_NO_IMAGE_RESOLUTION_CHOICE:                   // size: 0
        (); // No resolution value

      CERT_LOGOTYPE_BITS_IMAGE_RESOLUTION_CHOICE:
        (dwNumBits: DWORD);     // Resolution in bits             // size: 4

      CERT_LOGOTYPE_TABLE_SIZE_IMAGE_RESOLUTION_CHOICE:
        (dwTableSize: DWORD;    // Number of color or grey tones  // size: 4

// In the original struct, the following field is a field outside the union,
// but to avoid introducing an extra variant record, and therefore an extra
// identifier to be used before dwNumBits or dwTableSize, it was put in the
// last variant part.

         pwszLanguage: LPWSTR); // Optional. Encoded as IA5.
                                // RFC 3066 Language Tag
  end;
  {$EXTERNALSYM CERT_LOGOTYPE_IMAGE_INFO}
  CERT_LOGOTYPE_IMAGE_INFO = _CERT_LOGOTYPE_IMAGE_INFO;
  {$EXTERNALSYM PCERT_LOGOTYPE_IMAGE_INFO}
  PCERT_LOGOTYPE_IMAGE_INFO = ^_CERT_LOGOTYPE_IMAGE_INFO;
  TCertLogoTypeImageInfo = _CERT_LOGOTYPE_IMAGE_INFO;

  PCertLogoTypeImage = ^TCertLogoTypeImage;
  {$EXTERNALSYM _CERT_LOGOTYPE_IMAGE}
  _CERT_LOGOTYPE_IMAGE = record
    LogotypeDetails: CERT_LOGOTYPE_DETAILS;

    pLogotypeImageInfo: PCertLogoTypeImageInfo; // Optional
  end;
  {$EXTERNALSYM CERT_LOGOTYPE_IMAGE}
  CERT_LOGOTYPE_IMAGE = _CERT_LOGOTYPE_IMAGE;
  {$EXTERNALSYM PCERT_LOGOTYPE_IMAGE}
  PCERT_LOGOTYPE_IMAGE = ^_CERT_LOGOTYPE_IMAGE;
  TCertLogoTypeImage = _CERT_LOGOTYPE_IMAGE;

  PCertLogoTypeAudioInfo = ^TCertLogoTypeAudioInfo;
  {$EXTERNALSYM _CERT_LOGOTYPE_AUDIO_INFO}
  _CERT_LOGOTYPE_AUDIO_INFO = record
    dwFileSize: DWORD;     // In octets
    dwPlayTime: DWORD;     // In milliseconds
    dwChannels: DWORD;     // 1=mono, 2=stereo, 4=quad
    dwSampleRate: DWORD;   // Optional. 0 => not present.
                           // Samples per second
    pwszLanguage: LPWSTR;  // Optional. Encoded as IA5.
                           // RFC 3066 Language Tag
  end;
  {$EXTERNALSYM CERT_LOGOTYPE_AUDIO_INFO}
  CERT_LOGOTYPE_AUDIO_INFO = _CERT_LOGOTYPE_AUDIO_INFO;
  TCertLogoTypeAudioInfo = _CERT_LOGOTYPE_AUDIO_INFO;

  PCertLogoTypeAudio = ^TCertLogoTypeAudio;
  {$EXTERNALSYM _CERT_LOGOTYPE_AUDIO}
  _CERT_LOGOTYPE_AUDIO = record
    LogotypeDetails: CERT_LOGOTYPE_DETAILS;

    pLogotypeAudioInfo: PCertLogoTypeAudioInfo; // Optional
  end;
  {$EXTERNALSYM CERT_LOGOTYPE_AUDIO}
  CERT_LOGOTYPE_AUDIO = _CERT_LOGOTYPE_AUDIO;
  {$EXTERNALSYM PCERT_LOGOTYPE_AUDIO}
  PCERT_LOGOTYPE_AUDIO = ^_CERT_LOGOTYPE_AUDIO;
  TCertLogoTypeAudio = _CERT_LOGOTYPE_AUDIO;

  PCertLogoTypeData = ^TCertLogoTypeData;
  {$EXTERNALSYM _CERT_LOGOTYPE_DATA}
  _CERT_LOGOTYPE_DATA = record
    cLogotypeImage: DWORD;
    rgLogotypeImage: PCertLogoTypeImage;

    cLogotypeAudio: DWORD;
    rgLogotypeAudio: PCertLogoTypeAudio;
  end;
  {$EXTERNALSYM CERT_LOGOTYPE_DATA}
  CERT_LOGOTYPE_DATA = _CERT_LOGOTYPE_DATA;
  {$EXTERNALSYM PCERT_LOGOTYPE_DATA}
  PCERT_LOGOTYPE_DATA = ^_CERT_LOGOTYPE_DATA;
  TCertLogoTypeData = _CERT_LOGOTYPE_DATA;

const
  {$EXTERNALSYM CERT_LOGOTYPE_DIRECT_INFO_CHOICE}
  CERT_LOGOTYPE_DIRECT_INFO_CHOICE = 1;
  {$EXTERNALSYM CERT_LOGOTYPE_INDIRECT_INFO_CHOICE}
  CERT_LOGOTYPE_INDIRECT_INFO_CHOICE = 2;

type
  PCertLogoTypeInfo = ^TCertLogoTypeInfo;
  {$EXTERNALSYM _CERT_LOGOTYPE_INFO}
  _CERT_LOGOTYPE_INFO = record
    case dwLogotypeInfoChoice: DWORD of
      CERT_LOGOTYPE_DIRECT_INFO_CHOICE:
        (pLogotypeDirectInfo: PCertLogoTypeData);
      CERT_LOGOTYPE_INDIRECT_INFO_CHOICE:
        (pLogotypeIndirectInfo: PCertLogoTypeReference);
  end;
  {$EXTERNALSYM CERT_LOGOTYPE_INFO}
  CERT_LOGOTYPE_INFO = _CERT_LOGOTYPE_INFO;
  {$EXTERNALSYM PCERT_LOGOTYPE_INFO}
  PCERT_LOGOTYPE_INFO = ^CERT_LOGOTYPE_INFO;
  TCertLogoTypeInfo = _CERT_LOGOTYPE_INFO;

  PCertOtherLogoTypeInfo = ^TCertOtherLogoTypeInfo;
  {$EXTERNALSYM _CERT_OTHER_LOGOTYPE_INFO}
  _CERT_OTHER_LOGOTYPE_INFO = record
    pszObjId: LPSTR;
    LogotypeInfo: CERT_LOGOTYPE_INFO;
  end;
  {$EXTERNALSYM CERT_OTHER_LOGOTYPE_INFO}
  CERT_OTHER_LOGOTYPE_INFO = _CERT_OTHER_LOGOTYPE_INFO;
  {$EXTERNALSYM PCERT_OTHER_LOGOTYPE_INFO}
  PCERT_OTHER_LOGOTYPE_INFO = ^_CERT_OTHER_LOGOTYPE_INFO;
  TCertOtherLogoTypeInfo = _CERT_OTHER_LOGOTYPE_INFO;

const
  {$EXTERNALSYM szOID_LOYALTY_OTHER_LOGOTYPE}
  szOID_LOYALTY_OTHER_LOGOTYPE                = '1.3.6.1.5.5.7.20.1';
  {$EXTERNALSYM szOID_BACKGROUND_OTHER_LOGOTYPE}
  szOID_BACKGROUND_OTHER_LOGOTYPE             = '1.3.6.1.5.5.7.20.2';

type
  PCertLogoTypeExtInfo = ^TCertLogoTypeExtInfo;
  {$EXTERNALSYM _CERT_LOGOTYPE_EXT_INFO}
  _CERT_LOGOTYPE_EXT_INFO = record
    cCommunityLogo: DWORD;
    rgCommunityLogo: PCERT_LOGOTYPE_INFO;
    pIssuerLogo: PCERT_LOGOTYPE_INFO;        // Optional
    pSubjectLogo: PCERT_LOGOTYPE_INFO;       // Optional
    cOtherLogo: DWORD;
    rgOtherLogo: PCERT_OTHER_LOGOTYPE_INFO;
  end;
  {$EXTERNALSYM CERT_LOGOTYPE_EXT_INFO}
  CERT_LOGOTYPE_EXT_INFO = _CERT_LOGOTYPE_EXT_INFO;
  {$EXTERNALSYM PCERT_LOGOTYPE_EXT_INFO}
  PCERT_LOGOTYPE_EXT_INFO = ^_CERT_LOGOTYPE_EXT_INFO;
  TCertLogoTypeExtInfo = _CERT_LOGOTYPE_EXT_INFO;

//+=========================================================================
//  Biometric Extension Data Structures
//
//  X509_BIOMETRIC_EXT
//  szOID_BIOMETRIC_EXT
//
//  pvStructInfo points to following CERT_BIOMETRIC_EXT_INFO data structure.
//==========================================================================

const
  {$EXTERNALSYM CERT_BIOMETRIC_PREDEFINED_DATA_CHOICE}
  CERT_BIOMETRIC_PREDEFINED_DATA_CHOICE = 1;
  {$EXTERNALSYM CERT_BIOMETRIC_OID_DATA_CHOICE}
  CERT_BIOMETRIC_OID_DATA_CHOICE = 2;

  {$EXTERNALSYM CERT_BIOMETRIC_PICTURE_TYPE}
  CERT_BIOMETRIC_PICTURE_TYPE = 0;
  {$EXTERNALSYM CERT_BIOMETRIC_SIGNATURE_TYPE}
  CERT_BIOMETRIC_SIGNATURE_TYPE = 1;

type
  PCertBiometricData = ^TCertBiometricData;
  {$EXTERNALSYM _CERT_BIOMETRIC_DATA}
  _CERT_BIOMETRIC_DATA = record
    case dwTypeOfBiometricDataChoice: DWORD of
      CERT_BIOMETRIC_PREDEFINED_DATA_CHOICE:
        (dwPredefined: DWORD);
      CERT_BIOMETRIC_OID_DATA_CHOICE:
        (pszObjId: LPSTR;
         HashedUrl: CERT_HASHED_URL);      // pwszUrl is Optional.
  end;
  {$EXTERNALSYM CERT_BIOMETRIC_DATA}
  CERT_BIOMETRIC_DATA = _CERT_BIOMETRIC_DATA;
  {$EXTERNALSYM PCERT_BIOMETRIC_DATA}
  PCERT_BIOMETRIC_DATA = ^_CERT_BIOMETRIC_DATA;
  TCertBiometricData = _CERT_BIOMETRIC_DATA;

  PCertBiometricExtInfo = ^TCertBiometricExtInfo;
  {$EXTERNALSYM _CERT_BIOMETRIC_EXT_INFO}
  _CERT_BIOMETRIC_EXT_INFO = record
    cBiometricData: DWORD;
    rgBiometricData: PCERT_BIOMETRIC_DATA;
  end;
  {$EXTERNALSYM CERT_BIOMETRIC_EXT_INFO}
  CERT_BIOMETRIC_EXT_INFO = _CERT_BIOMETRIC_EXT_INFO;
  {$EXTERNALSYM PCERT_BIOMETRIC_EXT_INFO}
  PCERT_BIOMETRIC_EXT_INFO = ^_CERT_BIOMETRIC_EXT_INFO;
  TCertBiometricExtInfo = _CERT_BIOMETRIC_EXT_INFO;

//+=========================================================================
//  Online Certificate Status Protocol (OCSP) Data Structures
//==========================================================================

//+-------------------------------------------------------------------------
//  OCSP_SIGNED_REQUEST
//
//  OCSP signed request.
//
//  Note, in most instances, pOptionalSignatureInfo will be NULL indicating
//  no signature is present.
//--------------------------------------------------------------------------

  POCSPSignatureInfo = ^TOCSPSignatureInfo;
  {$EXTERNALSYM _OCSP_SIGNATURE_INFO}
  _OCSP_SIGNATURE_INFO = record
    SignatureAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    Signature: CRYPT_BIT_BLOB;
    cCertEncoded: DWORD;
    rgCertEncoded: PCERT_BLOB;
  end;
  {$EXTERNALSYM OCSP_SIGNATURE_INFO}
  OCSP_SIGNATURE_INFO = _OCSP_SIGNATURE_INFO;
  {$EXTERNALSYM POCSP_SIGNATURE_INFO}
  POCSP_SIGNATURE_INFO = ^_OCSP_SIGNATURE_INFO;
  TOCSPSignatureInfo = _OCSP_SIGNATURE_INFO;

  POCSPSignedRequestInfo = ^TOCSPSignedRequestInfo;
  {$EXTERNALSYM _OCSP_SIGNED_REQUEST_INFO}
  _OCSP_SIGNED_REQUEST_INFO = record
    ToBeSigned: CRYPT_DER_BLOB;             // Encoded OCSP_REQUEST
    pOptionalSignatureInfo: POCSP_SIGNATURE_INFO; // NULL, no signature
  end;
  {$EXTERNALSYM OCSP_SIGNED_REQUEST_INFO}
  OCSP_SIGNED_REQUEST_INFO = _OCSP_SIGNED_REQUEST_INFO;
  {$EXTERNALSYM POCSP_SIGNED_REQUEST_INFO}
  POCSP_SIGNED_REQUEST_INFO = ^_OCSP_SIGNED_REQUEST_INFO;
  TOCSPSignedRequestInfo = _OCSP_SIGNED_REQUEST_INFO;

//+-------------------------------------------------------------------------
//  OCSP_REQUEST
//
//  ToBeSigned OCSP request.
//--------------------------------------------------------------------------

  POCSPCertID = ^TOCSPCertID;
  {$EXTERNALSYM _OCSP_CERT_ID}
  _OCSP_CERT_ID = record
    HashAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;  // Normally SHA1
    IssuerNameHash: CRYPT_HASH_BLOB; // Hash of encoded name
    IssuerKeyHash: CRYPT_HASH_BLOB;  // Hash of PublicKey bits
    SerialNumber: CRYPT_INTEGER_BLOB;
  end;
  {$EXTERNALSYM OCSP_CERT_ID}
  OCSP_CERT_ID = _OCSP_CERT_ID;
  {$EXTERNALSYM POCSP_CERT_ID}
  POCSP_CERT_ID = ^_OCSP_CERT_ID;
  TOCSPCertID = _OCSP_CERT_ID;

  POCSPRequestEntry = ^TOCSPRequestEntry;
  {$EXTERNALSYM _OCSP_REQUEST_ENTRY}
  _OCSP_REQUEST_ENTRY = record
    CertId: OCSP_CERT_ID;
    cExtension: DWORD;
    rgExtension: PCERT_EXTENSION;
  end;
  {$EXTERNALSYM OCSP_REQUEST_ENTRY}
  OCSP_REQUEST_ENTRY = _OCSP_REQUEST_ENTRY;
  {$EXTERNALSYM POCSP_REQUEST_ENTRY}
  POCSP_REQUEST_ENTRY = ^_OCSP_REQUEST_ENTRY;
  TOCSPRequestEntry = _OCSP_REQUEST_ENTRY;

  POCSPRequestInfo = ^TOCSPRequestInfo;
  {$EXTERNALSYM _OCSP_REQUEST_INFO}
  _OCSP_REQUEST_INFO = record
    dwVersion: DWORD;
    pRequestorName: PCERT_ALT_NAME_ENTRY;     // OPTIONAL
    cRequestEntry: DWORD;
    rgRequestEntry: POCSP_REQUEST_ENTRY;
    cExtension: DWORD;
    rgExtension: PCERT_EXTENSION;
  end;
  {$EXTERNALSYM OCSP_REQUEST_INFO}
  OCSP_REQUEST_INFO = _OCSP_REQUEST_INFO;
  {$EXTERNALSYM POCSP_REQUEST_INFO}
  POCSP_REQUEST_INFO = ^_OCSP_REQUEST_INFO;
  TOCSPRequestInfo = _OCSP_REQUEST_INFO;

const
  {$EXTERNALSYM OCSP_REQUEST_V1}
  OCSP_REQUEST_V1 = 0;

//+-------------------------------------------------------------------------
//  OCSP_RESPONSE
//
//  OCSP outer, unsigned response wrapper.
//--------------------------------------------------------------------------
type
  POCSPResponseInfo = ^TOCSPResponseInfo;
  {$EXTERNALSYM _OCSP_RESPONSE_INFO}
  _OCSP_RESPONSE_INFO = record
    dwStatus: DWORD;
    pszObjId: LPSTR;   // OPTIONAL, may be NULL
    Value: CRYPT_OBJID_BLOB;      // OPTIONAL
  end;
  {$EXTERNALSYM OCSP_RESPONSE_INFO}
  OCSP_RESPONSE_INFO = _OCSP_RESPONSE_INFO;
  {$EXTERNALSYM POCSP_RESPONSE_INFO}
  POCSP_RESPONSE_INFO = ^_OCSP_RESPONSE_INFO;
  TOCSPResponseInfo = _OCSP_RESPONSE_INFO;

const
  {$EXTERNALSYM OCSP_SUCCESSFUL_RESPONSE}
  OCSP_SUCCESSFUL_RESPONSE = 0;
  {$EXTERNALSYM OCSP_MALFORMED_REQUEST_RESPONSE}
  OCSP_MALFORMED_REQUEST_RESPONSE = 1;
  {$EXTERNALSYM OCSP_INTERNAL_ERROR_RESPONSE}
  OCSP_INTERNAL_ERROR_RESPONSE = 2;
  {$EXTERNALSYM OCSP_TRY_LATER_RESPONSE}
  OCSP_TRY_LATER_RESPONSE = 3;
// 4 is not used
  {$EXTERNALSYM OCSP_SIG_REQUIRED_RESPONSE}
  OCSP_SIG_REQUIRED_RESPONSE = 5;
  {$EXTERNALSYM OCSP_UNAUTHORIZED_RESPONSE}
  OCSP_UNAUTHORIZED_RESPONSE = 6;

  {$EXTERNALSYM szOID_PKIX_OCSP_BASIC_SIGNED_RESPONSE}
  szOID_PKIX_OCSP_BASIC_SIGNED_RESPONSE   = '1.3.6.1.5.5.7.48.1.1';

//+-------------------------------------------------------------------------
//  OCSP_BASIC_SIGNED_RESPONSE
//  szOID_PKIX_OCSP_BASIC_SIGNED_RESPONSE
//
//  OCSP basic signed response.
//--------------------------------------------------------------------------
type
  POCSPBasicSignedResponseInfo = ^TOCSPBasicSignedResponseInfo;
  {$EXTERNALSYM _OCSP_BASIC_SIGNED_RESPONSE_INFO}
  _OCSP_BASIC_SIGNED_RESPONSE_INFO = record
    ToBeSigned: CRYPT_DER_BLOB;     // Encoded OCSP_BASIC_RESPONSE
    SignatureInfo: OCSP_SIGNATURE_INFO;
  end;
  {$EXTERNALSYM OCSP_BASIC_SIGNED_RESPONSE_INFO}
  OCSP_BASIC_SIGNED_RESPONSE_INFO = _OCSP_BASIC_SIGNED_RESPONSE_INFO;
  {$EXTERNALSYM POCSP_BASIC_SIGNED_RESPONSE_INFO}
  POCSP_BASIC_SIGNED_RESPONSE_INFO = ^_OCSP_BASIC_SIGNED_RESPONSE_INFO;
  TOCSPBasicSignedResponseInfo = _OCSP_BASIC_SIGNED_RESPONSE_INFO;

//+-------------------------------------------------------------------------
//  OCSP_BASIC_RESPONSE
//
//  ToBeSigned OCSP basic response.
//--------------------------------------------------------------------------

  POCSPBasicRevokedInfo = ^TOCSPBasicRevokedInfo;
  {$EXTERNALSYM _OCSP_BASIC_REVOKED_INFO}
  _OCSP_BASIC_REVOKED_INFO = record
    RevocationDate: FILETIME;

    // See X509_CRL_REASON_CODE for list of reason codes
    dwCrlReasonCode: DWORD;
  end;
  {$EXTERNALSYM OCSP_BASIC_REVOKED_INFO}
  OCSP_BASIC_REVOKED_INFO = _OCSP_BASIC_REVOKED_INFO;
  {$EXTERNALSYM POCSP_BASIC_REVOKED_INFO}
  POCSP_BASIC_REVOKED_INFO = ^_OCSP_BASIC_REVOKED_INFO;
  TOCSPBasicRevokedInfo = _OCSP_BASIC_REVOKED_INFO;

  POCSPBasicResponseEntry = ^TOCSPBasicResponseEntry;
  {$EXTERNALSYM _OCSP_BASIC_RESPONSE_ENTRY}
  _OCSP_BASIC_RESPONSE_ENTRY = record
    CertId: OCSP_CERT_ID;
    dwCertStatus: DWORD;
    // union {
        // OCSP_BASIC_GOOD_CERT_STATUS
        // OCSP_BASIC_UNKNOWN_CERT_STATUS
        //  No additional information

        // OCSP_BASIC_REVOKED_CERT_STATUS
        pRevokedInfo: POCSP_BASIC_REVOKED_INFO;

    // };
    ThisUpdate: FILETIME;
    NextUpdate: FILETIME; // Optional, zero filetime implies
                          // never expires
    cExtension: DWORD;
    rgExtension: PCERT_EXTENSION;
  end;
  {$EXTERNALSYM OCSP_BASIC_RESPONSE_ENTRY}
  OCSP_BASIC_RESPONSE_ENTRY = _OCSP_BASIC_RESPONSE_ENTRY;
  {$EXTERNALSYM POCSP_BASIC_RESPONSE_ENTRY}
  POCSP_BASIC_RESPONSE_ENTRY = ^_OCSP_BASIC_RESPONSE_ENTRY;
  TOCSPBasicResponseEntry = _OCSP_BASIC_RESPONSE_ENTRY;

const
  {$EXTERNALSYM OCSP_BASIC_GOOD_CERT_STATUS}
  OCSP_BASIC_GOOD_CERT_STATUS = 0;
  {$EXTERNALSYM OCSP_BASIC_REVOKED_CERT_STATUS}
  OCSP_BASIC_REVOKED_CERT_STATUS = 1;
  {$EXTERNALSYM OCSP_BASIC_UNKNOWN_CERT_STATUS}
  OCSP_BASIC_UNKNOWN_CERT_STATUS = 2;

  {$EXTERNALSYM OCSP_BASIC_RESPONSE_V1}
  OCSP_BASIC_RESPONSE_V1 = 0;

  {$EXTERNALSYM OCSP_BASIC_BY_NAME_RESPONDER_ID}
  OCSP_BASIC_BY_NAME_RESPONDER_ID = 1;
  {$EXTERNALSYM OCSP_BASIC_BY_KEY_RESPONDER_ID}
  OCSP_BASIC_BY_KEY_RESPONDER_ID = 2;

type  
  POCSPBasicResponseInfo = ^TOCSPBasicResponseInfo;
  {$EXTERNALSYM _OCSP_BASIC_RESPONSE_INFO}
  _OCSP_BASIC_RESPONSE_INFO = record
    dwVersion: DWORD;
    case dwResponderIdChoice: DWORD of
    // union {
      OCSP_BASIC_BY_NAME_RESPONDER_ID:
        (ByNameResponderId: CERT_NAME_BLOB); // size: 8
      OCSP_BASIC_BY_KEY_RESPONDER_ID:
        (ByKeyResponderId: CRYPT_HASH_BLOB;  // size: 8
    // }
    ProducedAt: FILETIME;
    cResponseEntry: DWORD;
    rgResponseEntry: POCSP_BASIC_RESPONSE_ENTRY;
    cExtension: DWORD;
    rgExtension: PCERT_EXTENSION
    );
  end;
  {$EXTERNALSYM OCSP_BASIC_RESPONSE_INFO}
  OCSP_BASIC_RESPONSE_INFO = _OCSP_BASIC_RESPONSE_INFO;
  {$EXTERNALSYM POCSP_BASIC_RESPONSE_INFO}
  POCSP_BASIC_RESPONSE_INFO = _OCSP_BASIC_RESPONSE_INFO;
  TOCSPBasicResponseInfo = _OCSP_BASIC_RESPONSE_INFO;

//+=========================================================================
//  Object IDentifier (OID) Installable Functions:  Data Structures and APIs
//==========================================================================

type
  {$EXTERNALSYM HCRYPTOIDFUNCSET}
  HCRYPTOIDFUNCSET  = Pointer; // TODO: THandle?
  {$EXTERNALSYM HCRYPTOIDFUNCADDR}
  HCRYPTOIDFUNCADDR = Pointer; // TODO: THandle?

const
// Predefined OID Function Names
  {$EXTERNALSYM CRYPT_OID_ENCODE_OBJECT_FUNC}
  CRYPT_OID_ENCODE_OBJECT_FUNC        = 'CryptDllEncodeObject';
  {$EXTERNALSYM CRYPT_OID_DECODE_OBJECT_FUNC}
  CRYPT_OID_DECODE_OBJECT_FUNC        = 'CryptDllDecodeObject';
  {$EXTERNALSYM CRYPT_OID_ENCODE_OBJECT_EX_FUNC}
  CRYPT_OID_ENCODE_OBJECT_EX_FUNC     = 'CryptDllEncodeObjectEx';
  {$EXTERNALSYM CRYPT_OID_DECODE_OBJECT_EX_FUNC}
  CRYPT_OID_DECODE_OBJECT_EX_FUNC     = 'CryptDllDecodeObjectEx';
  {$EXTERNALSYM CRYPT_OID_CREATE_COM_OBJECT_FUNC}
  CRYPT_OID_CREATE_COM_OBJECT_FUNC    = 'CryptDllCreateCOMObject';
  {$EXTERNALSYM CRYPT_OID_VERIFY_REVOCATION_FUNC}
  CRYPT_OID_VERIFY_REVOCATION_FUNC    = 'CertDllVerifyRevocation';
  {$EXTERNALSYM CRYPT_OID_VERIFY_CTL_USAGE_FUNC}
  CRYPT_OID_VERIFY_CTL_USAGE_FUNC     = 'CertDllVerifyCTLUsage';
  {$EXTERNALSYM CRYPT_OID_FORMAT_OBJECT_FUNC}
  CRYPT_OID_FORMAT_OBJECT_FUNC        = 'CryptDllFormatObject';
  {$EXTERNALSYM CRYPT_OID_FIND_OID_INFO_FUNC}
  CRYPT_OID_FIND_OID_INFO_FUNC        = 'CryptDllFindOIDInfo';
  {$EXTERNALSYM CRYPT_OID_FIND_LOCALIZED_NAME_FUNC}
  CRYPT_OID_FIND_LOCALIZED_NAME_FUNC  = 'CryptDllFindLocalizedName';

// CryptDllEncodeObject has same function signature as CryptEncodeObject.

// CryptDllDecodeObject has same function signature as CryptDecodeObject.

// CryptDllEncodeObjectEx has same function signature as CryptEncodeObjectEx.
// The Ex version MUST support the CRYPT_ENCODE_ALLOC_FLAG option.
//
// If an Ex function isn't installed or registered, then, attempts to find
// a non-EX version. If the ALLOC flag is set, then, CryptEncodeObjectEx,
// does the allocation and calls the non-EX version twice.

// CryptDllDecodeObjectEx has same function signature as CryptDecodeObjectEx.
// The Ex version MUST support the CRYPT_DECODE_ALLOC_FLAG option.
//
// If an Ex function isn't installed or registered, then, attempts to find
// a non-EX version. If the ALLOC flag is set, then, CryptDecodeObjectEx,
// does the allocation and calls the non-EX version twice.

// CryptDllCreateCOMObject has the following signature:
//      BOOL WINAPI CryptDllCreateCOMObject(
//           DWORD dwEncodingType,
//           LPCSTR pszOID,
//           PCRYPT_DATA_BLOB pEncodedContent,
//           DWORD dwFlags,
//           REFIID riid,
//          __deref_out void **ppvObj);

// CertDllVerifyRevocation has the same signature as CertVerifyRevocation
//  (See CertVerifyRevocation for details on when called)

// CertDllVerifyCTLUsage has the same signature as CertVerifyCTLUsage

// CryptDllFindOIDInfo currently is only used to store values used by
// CryptFindOIDInfo. See CryptFindOIDInfo() for more details.

// CryptDllFindLocalizedName is only used to store localized string
// values used by CryptFindLocalizedName. See CryptFindLocalizedName() for
// more details.

//  Example of a complete OID Function Registry Name:
//    HKEY_LOCAL_MACHINE\Software\Microsoft\Cryptography\OID
//      Encoding Type 1\CryptDllEncodeObject\1.2.3
//
//  The key's 'Dll' value contains the name of the Dll.
//  The key's 'FuncName' value overrides the default function name
  {$EXTERNALSYM CRYPT_OID_REGPATH}
  CRYPT_OID_REGPATH                    = 'Software\Microsoft\Cryptography\OID';
  {$EXTERNALSYM CRYPT_OID_REG_ENCODING_TYPE_PREFIX}
  CRYPT_OID_REG_ENCODING_TYPE_PREFIX   = 'EncodingType ';
  {$EXTERNALSYM CRYPT_OID_REG_DLL_VALUE_NAME}
  CRYPT_OID_REG_DLL_VALUE_NAME         = 'Dll';
  {$EXTERNALSYM CRYPT_OID_REG_FUNC_NAME_VALUE_NAME}
  CRYPT_OID_REG_FUNC_NAME_VALUE_NAME   = 'FuncName';
  {$EXTERNALSYM CRYPT_OID_REG_FUNC_NAME_VALUE_NAME_A}
  CRYPT_OID_REG_FUNC_NAME_VALUE_NAME_A = 'FuncName';

// CRYPT_INSTALL_OID_FUNC_BEFORE_FLAG can be set in the key's 'CryptFlags'
// value to register the functions before the installed functions.
//
// CryptSetOIDFunctionValue must be called to set this value. 'CryptFlags'
// must be set using a dwValueType of REG_DWORD.
  {$EXTERNALSYM CRYPT_OID_REG_FLAGS_VALUE_NAME}
  CRYPT_OID_REG_FLAGS_VALUE_NAME       = 'CryptFlags';

// OID used for Default OID functions
  {$EXTERNALSYM CRYPT_DEFAULT_OID}
  CRYPT_DEFAULT_OID                    = 'DEFAULT';

type
  PCryptOIDFuncEntry = ^TCryptOIDFuncEntry;
  {$EXTERNALSYM _CRYPT_OID_FUNC_ENTRY}
  _CRYPT_OID_FUNC_ENTRY = record
    pszOID: LPCSTR;
    pvFuncAddr: Pointer;
  end;
  {$EXTERNALSYM CRYPT_OID_FUNC_ENTRY}
  CRYPT_OID_FUNC_ENTRY = _CRYPT_OID_FUNC_ENTRY;
  {$EXTERNALSYM PCRYPT_OID_FUNC_ENTRY}
  PCRYPT_OID_FUNC_ENTRY = ^_CRYPT_OID_FUNC_ENTRY;
  TCryptOIDFuncEntry = _CRYPT_OID_FUNC_ENTRY;

const
  {$EXTERNALSYM CRYPT_INSTALL_OID_FUNC_BEFORE_FLAG}
  CRYPT_INSTALL_OID_FUNC_BEFORE_FLAG = 1;

//+-------------------------------------------------------------------------
//  Install a set of callable OID function addresses.
//
//  By default the functions are installed at end of the list.
//  Set CRYPT_INSTALL_OID_FUNC_BEFORE_FLAG to install at beginning of list.
//
//  hModule should be updated with the hModule passed to DllMain to prevent
//  the Dll containing the function addresses from being unloaded by
//  CryptGetOIDFuncAddress/CryptFreeOIDFunctionAddress. This would be the
//  case when the Dll has also regsvr32'ed OID functions via
//  CryptRegisterOIDFunction.
//
//  DEFAULT functions are installed by setting rgFuncEntry[].pszOID =
//  CRYPT_DEFAULT_OID.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptInstallOIDFunctionAddress}
function CryptInstallOIDFunctionAddress(hModule: HMODULE; { hModule passed to DllMain }
  dwEncodingType: DWORD; pszFuncName: LPCSTR; cFuncEntry: DWORD;
  const rgFuncEntry: PCRYPT_OID_FUNC_ENTRY; dwFlags: DWORD):BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Initialize and return handle to the OID function set identified by its
//  function name.
//
//  If the set already exists, a handle to the existing set is returned.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptInitOIDFunctionSet}
function CryptInitOIDFunctionSet(pszFuncName: LPCSTR;
  dwFlags: DWORD): HCRYPTOIDFUNCSET; stdcall;

//+-------------------------------------------------------------------------
//  Search the list of installed functions for an encoding type and OID match.
//  If not found, search the registry.
//
//  For success, returns TRUE with *ppvFuncAddr updated with the function's
//  address and *phFuncAddr updated with the function address's handle.
//  The function's handle is AddRef'ed. CryptFreeOIDFunctionAddress needs to
//  be called to release it.
//
//  For a registry match, the Dll containing the function is loaded.
//
//  By default, both the registered and installed function lists are searched.
//  Set CRYPT_GET_INSTALLED_OID_FUNC_FLAG to only search the installed list
//  of functions. This flag would be set by a registered function to get
//  the address of a pre-installed function it was replacing. For example,
//  the registered function might handle a new special case and call the
//  pre-installed function to handle the remaining cases.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptGetOIDFunctionAddress}
function CryptGetOIDFunctionAddress(hFuncSet: HCRYPTOIDFUNCSET;
  dwEncodingType: DWORD; pszOID: LPCSTR; dwFlags: DWORD;
  out ppvFuncAddr: Pointer; out phFuncAddr: HCRYPTOIDFUNCADDR): BOOL; stdcall;

const
  {$EXTERNALSYM CRYPT_GET_INSTALLED_OID_FUNC_FLAG}
  CRYPT_GET_INSTALLED_OID_FUNC_FLAG = $1;

//+-------------------------------------------------------------------------
//  Get the list of registered default Dll entries for the specified
//  function set and encoding type.
//
//  The returned list consists of none, one or more null terminated Dll file
//  names. The list is terminated with an empty ('\0') Dll file name.
//  For example: 'first.dll' '\0' L'second.dll' '\0' '\0'
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptGetDefaultOIDDllList}
function CryptGetDefaultOIDDllList(hFuncSet: HCRYPTOIDFUNCSET;
  dwEncodingType: DWORD; pwszDllList: PWideChar;
  out pcchDllList: DWORD): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Either: get the first or next installed DEFAULT function OR
//  load the Dll containing the DEFAULT function.
//
//  If pwszDll is NULL, search the list of installed DEFAULT functions.
//  *phFuncAddr must be set to NULL to get the first installed function.
//  Successive installed functions are returned by setting *phFuncAddr
//  to the hFuncAddr returned by the previous call.
//
//  If pwszDll is NULL, the input *phFuncAddr
//  is always CryptFreeOIDFunctionAddress'ed by this function, even for
//  an error.
//
//  If pwszDll isn't NULL, then, attempts to load the Dll and the DEFAULT
//  function. *phFuncAddr is ignored upon entry and isn't
//  CryptFreeOIDFunctionAddress'ed.
//
//  For success, returns TRUE with *ppvFuncAddr updated with the function's
//  address and *phFuncAddr updated with the function address's handle.
//  The function's handle is AddRef'ed. CryptFreeOIDFunctionAddress needs to
//  be called to release it or CryptGetDefaultOIDFunctionAddress can also
//  be called for a NULL pwszDll.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptGetDefaultOIDFunctionAddress}
function CryptGetDefaultOIDFunctionAddress(hFuncSet: HCRYPTOIDFUNCSET;
  dwEncodingType: DWORD; pwszDll: LPCWSTR; dwFlags: DWORD;
  out ppvFuncAddr: Pointer; out phFuncAddr: HCRYPTOIDFUNCADDR): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Releases the handle AddRef'ed and returned by CryptGetOIDFunctionAddress
//  or CryptGetDefaultOIDFunctionAddress.
//
//  If a Dll was loaded for the function its unloaded. However, before doing
//  the unload, the DllCanUnloadNow function exported by the loaded Dll is
//  called. It should return S_FALSE to inhibit the unload or S_TRUE to enable
//  the unload. If the Dll doesn't export DllCanUnloadNow, the Dll is unloaded.
//
//  DllCanUnloadNow has the following signature:
//      STDAPI  DllCanUnloadNow(void);
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptFreeOIDFunctionAddress}
function CryptFreeOIDFunctionAddress(hFuncAddr: HCRYPTOIDFUNCADDR;
  dwFlags: DWORD): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Register the Dll containing the function to be called for the specified
//  encoding type, function name and OID.
//
//  pwszDll may contain environment-variable strings
//  which are ExpandEnvironmentStrings()'ed before loading the Dll.
//
//  In addition to registering the DLL, you may override the
//  name of the function to be called. For example,
//      pszFuncName = 'CryptDllEncodeObject',
//      pszOverrideFuncName = 'MyEncodeXyz'.
//  This allows a Dll to export multiple OID functions for the same
//  function name without needing to interpose its own OID dispatcher function.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptRegisterOIDFunction}
function CryptRegisterOIDFunction(dwEncodingType: DWORD;
  pszFuncName, pszOID: LPCSTR; pwszDll: LPCWSTR;
  pszOverrideFuncName: LPCSTR): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Unregister the Dll containing the function to be called for the specified
//  encoding type, function name and OID.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptUnregisterOIDFunction}
function CryptUnregisterOIDFunction(dwEncodingType: DWORD;
  pszFuncName, pszOID: LPCSTR): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Register the Dll containing the default function to be called for the
//  specified encoding type and function name.
//
//  Unlike CryptRegisterOIDFunction, you can't override the function name
//  needing to be exported by the Dll.
//
//  The Dll is inserted before the entry specified by dwIndex.
//    dwIndex == 0, inserts at the beginning.
//    dwIndex == CRYPT_REGISTER_LAST_INDEX, appends at the end.
//
//  pwszDll may contain environment-variable strings
//  which are ExpandEnvironmentStrings()'ed before loading the Dll.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptRegisterDefaultOIDFunction}
function CryptRegisterDefaultOIDFunction(dwEncodingType: DWORD;
  pszFuncName: LPCSTR; dwIndex: DWORD; pwszDll: LPCWSTR): BOOL; stdcall;

const
  {$EXTERNALSYM CRYPT_REGISTER_FIRST_INDEX}
  CRYPT_REGISTER_FIRST_INDEX = 0;
  {$EXTERNALSYM CRYPT_REGISTER_LAST_INDEX}
  CRYPT_REGISTER_LAST_INDEX = $FFFFFFFF;

//+-------------------------------------------------------------------------
//  Unregister the Dll containing the default function to be called for
//  the specified encoding type and function name.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptUnregisterDefaultOIDFunction}
function CryptUnregisterDefaultOIDFunction(dwEncodingType: DWORD;
  pszFuncName: LPCSTR; pwszDll: LPCWSTR): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Set the value for the specified encoding type, function name, OID and
//  value name.
//
//  See RegSetValueEx for the possible value types.
//
//  String types are UNICODE.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptSetOIDFunctionValue}
function CryptSetOIDFunctionValue(dwEncodingType: DWORD;
  pszFuncName, pszOID: LPCSTR; pwszValueName: LPCWSTR; dwValueType: DWORD;
  pbValueData: PBYTE; cbValueData: DWORD): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Get the value for the specified encoding type, function name, OID and
//  value name.
//
//  See RegEnumValue for the possible value types.
//
//  String types are UNICODE.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptGetOIDFunctionValue}
function CryptGetOIDFunctionValue(dwEncodingType: DWORD;
  pszFuncName: LPCSTR; pszOID: LPCSTR; pwszValueName: LPCWSTR;
  out pdwValueType: DWORD; pbValueData: PBYTE;
  var pcbValueData: DWORD): BOOL; stdcall;

type
  {$NODEFINE PPBYTE}
  PPBYTE = ^PByte;
  {$EXTERNALSYM PFN_CRYPT_ENUM_OID_FUNC}
  PFN_CRYPT_ENUM_OID_FUNC = function(dwEncodingType: DWORD;
    pszFuncName: LPCSTR; pszOID: LPCSTR; cValue: DWORD;
    rgdwValueType: PDWORD; // ^array[0..cValue - 1] of DWORD
    rgpwszValueName: PPWideChar; // array of PWideChar
    rgpbValueData: PPBYTE; // array of PByte
    rgcbValueData: PDWORD; // array of DWORD
    pvArg: Pointer): BOOL stdcall;
  TFnCryptEnumOIDFunc = PFN_CRYPT_ENUM_OID_FUNC;

//+-------------------------------------------------------------------------
//  Enumerate the OID functions identified by their encoding type,
//  function name and OID.
//
//  pfnEnumOIDFunc is called for each registry key matching the input
//  parameters. Setting dwEncodingType to CRYPT_MATCH_ANY_ENCODING_TYPE matches
//  any. Setting pszFuncName or pszOID to NULL matches any.
//
//  Set pszOID == CRYPT_DEFAULT_OID to restrict the enumeration to only the
//  DEFAULT functions
//
//  String types are UNICODE.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptEnumOIDFunction}
function CryptEnumOIDFunction(dwEncodingType: DWORD;
  pszFuncName, pszOID: LPCSTR; dwFlags: DWORD; pvArg: Pointer;
  pfnEnumOIDFunc: PFN_CRYPT_ENUM_OID_FUNC): BOOL; stdcall;

const
  {$EXTERNALSYM CRYPT_MATCH_ANY_ENCODING_TYPE}
  CRYPT_MATCH_ANY_ENCODING_TYPE = $FFFFFFFF;

//+=========================================================================
//  Object IDentifier (OID) Information:  Data Structures and APIs
//==========================================================================

//+-------------------------------------------------------------------------
//  Special ALG_ID's used in CRYPT_OID_INFO
//--------------------------------------------------------------------------
// Algorithm is only implemented in CNG.
  {$EXTERNALSYM CALG_OID_INFO_CNG_ONLY}
  CALG_OID_INFO_CNG_ONLY = $FFFFFFFF;

// Algorithm is defined in the encoded parameters. Only supported
// using CNG.
  {$EXTERNALSYM CALG_OID_INFO_PARAMETERS}
  CALG_OID_INFO_PARAMETERS = $FFFFFFFE;

// Macro to check for a special ALG_ID used in CRYPT_OID_INFO
{$EXTERNALSYM IS_SPECIAL_OID_INFO_ALGID}
function IS_SPECIAL_OID_INFO_ALGID(Algid: ALG_ID): Boolean; {$IfDef HAS_INLINE}inline;{$EndIf}

//+-------------------------------------------------------------------------
// Special CNG Algorithms used in CRYPT_OID_INFO
//--------------------------------------------------------------------------
const
  {$EXTERNALSYM CRYPT_OID_INFO_HASH_PARAMETERS_ALGORITHM}
  CRYPT_OID_INFO_HASH_PARAMETERS_ALGORITHM = 'CryptOIDInfoHashParameters';
  {$EXTERNALSYM CRYPT_OID_INFO_ECC_PARAMETERS_ALGORITHM}
  CRYPT_OID_INFO_ECC_PARAMETERS_ALGORITHM  = 'CryptOIDInfoECCParameters';
  {$EXTERNALSYM CRYPT_OID_INFO_MGF1_PARAMETERS_ALGORITHM}
  CRYPT_OID_INFO_MGF1_PARAMETERS_ALGORITHM = 'CryptOIDInfoMgf1Parameters';
  {$EXTERNALSYM CRYPT_OID_INFO_NO_SIGN_ALGORITHM}
  CRYPT_OID_INFO_NO_SIGN_ALGORITHM         = 'CryptOIDInfoNoSign';
  {$EXTERNALSYM CRYPT_OID_INFO_OAEP_PARAMETERS_ALGORITHM}
  CRYPT_OID_INFO_OAEP_PARAMETERS_ALGORITHM = 'CryptOIDInfoOAEPParameters';
  {$EXTERNALSYM CRYPT_OID_INFO_ECC_WRAP_PARAMETERS_ALGORITHM}
  CRYPT_OID_INFO_ECC_WRAP_PARAMETERS_ALGORITHM = 'CryptOIDInfoECCWrapParameters';

//+-------------------------------------------------------------------------
//  OID Information
//--------------------------------------------------------------------------
type
  PCryptOIDInfo = ^TCryptOIDInfo;
  {$EXTERNALSYM _CRYPT_OID_INFO}
  _CRYPT_OID_INFO = record
    cbSize: DWORD;
    pszOID: LPCSTR;
    pwszName: LPCWSTR;
    case dwGroupId: DWORD of
      0: (dwValue: DWORD);               // size: 4
      1: (Algid: ALG_ID);                // size: 4
      2: (dwLength: DWORD;               // size: 4
    ExtraInfo: CRYPT_DATA_BLOB;

    // Note, if you #define CRYPT_OID_INFO_HAS_EXTRA_FIELDS, then, you
    // must zero all unused fields in this data structure.
    // More fields could be added in a future release.

    // The following 2 fields are set to an empty string, '', if not defined.

    // This is the Algid string passed to the BCrypt* and NCrypt* APIs
    // defined in bcrypt.h and ncrypt.h.
    //
    // Its only applicable to the following groups:
    //  CRYPT_HASH_ALG_OID_GROUP_ID
    //  CRYPT_ENCRYPT_ALG_OID_GROUP_ID
    //  CRYPT_PUBKEY_ALG_OID_GROUP_ID
    //  CRYPT_SIGN_ALG_OID_GROUP_ID
    pwszCNGAlgid: LPCWSTR;

    // Following is only applicable to the following groups:
    //  CRYPT_SIGN_ALG_OID_GROUP_ID
    //      The public key pwszCNGAlgid. For ECC,
    //      CRYPT_OID_INFO_ECC_PARAMETERS_ALGORITHM.
    //  CRYPT_PUBKEY_ALG_OID_GROUP_ID
    //      For the ECC algorithms, CRYPT_OID_INFO_ECC_PARAMETERS_ALGORITHM.
    pwszCNGExtraAlgid: LPCWSTR;
  );
  end;
  {$EXTERNALSYM CRYPT_OID_INFO}
  CRYPT_OID_INFO = _CRYPT_OID_INFO;
  {$EXTERNALSYM PCRYPT_OID_INFO}
  PCRYPT_OID_INFO = ^_CRYPT_OID_INFO;
  {$EXTERNALSYM CCRYPT_OID_INFO}
  CCRYPT_OID_INFO = CRYPT_OID_INFO;
  {$EXTERNALSYM PCCRYPT_OID_INFO}
  PCCRYPT_OID_INFO = ^CRYPT_OID_INFO;
  TCryptOIDInfo = _CRYPT_OID_INFO;

// certenrolld_begin -- CRYPT_*_OID_GROUP_ID
//+-------------------------------------------------------------------------
//  OID Group IDs
//--------------------------------------------------------------------------
const
  {$EXTERNALSYM CRYPT_HASH_ALG_OID_GROUP_ID}
  CRYPT_HASH_ALG_OID_GROUP_ID = 1;
  {$EXTERNALSYM CRYPT_ENCRYPT_ALG_OID_GROUP_ID}
  CRYPT_ENCRYPT_ALG_OID_GROUP_ID = 2;
  {$EXTERNALSYM CRYPT_PUBKEY_ALG_OID_GROUP_ID}
  CRYPT_PUBKEY_ALG_OID_GROUP_ID = 3;
  {$EXTERNALSYM CRYPT_SIGN_ALG_OID_GROUP_ID}
  CRYPT_SIGN_ALG_OID_GROUP_ID = 4;
  {$EXTERNALSYM CRYPT_RDN_ATTR_OID_GROUP_ID}
  CRYPT_RDN_ATTR_OID_GROUP_ID = 5;
  {$EXTERNALSYM CRYPT_EXT_OR_ATTR_OID_GROUP_ID}
  CRYPT_EXT_OR_ATTR_OID_GROUP_ID = 6;
  {$EXTERNALSYM CRYPT_ENHKEY_USAGE_OID_GROUP_ID}
  CRYPT_ENHKEY_USAGE_OID_GROUP_ID = 7;
  {$EXTERNALSYM CRYPT_POLICY_OID_GROUP_ID}
  CRYPT_POLICY_OID_GROUP_ID = 8;
  {$EXTERNALSYM CRYPT_TEMPLATE_OID_GROUP_ID}
  CRYPT_TEMPLATE_OID_GROUP_ID = 9;
  {$EXTERNALSYM CRYPT_LAST_OID_GROUP_ID}
  CRYPT_LAST_OID_GROUP_ID = 9;

  {$EXTERNALSYM CRYPT_FIRST_ALG_OID_GROUP_ID}
  CRYPT_FIRST_ALG_OID_GROUP_ID            = CRYPT_HASH_ALG_OID_GROUP_ID;
  {$EXTERNALSYM CRYPT_LAST_ALG_OID_GROUP_ID}
  CRYPT_LAST_ALG_OID_GROUP_ID             = CRYPT_SIGN_ALG_OID_GROUP_ID;
// certenrolld_end


// The CRYPT_*_ALG_OID_GROUP_ID's have an Algid. The CRYPT_RDN_ATTR_OID_GROUP_ID
// has a dwLength. The CRYPT_EXT_OR_ATTR_OID_GROUP_ID,
// CRYPT_ENHKEY_USAGE_OID_GROUP_ID, CRYPT_POLICY_OID_GROUP_ID or
// CRYPT_TEMPLATE_OID_GROUP_ID don't have a dwValue.
//

// CRYPT_ENCRYPT_ALG_OID_GROUP_ID has the following optional ExtraInfo
// for AES algorithms:
//  DWORD[0] - dwBitLength

// CRYPT_PUBKEY_ALG_OID_GROUP_ID has the following optional ExtraInfo:
//  DWORD[0] - Flags. CRYPT_OID_INHIBIT_SIGNATURE_FORMAT_FLAG can be set to
//             inhibit the reformatting of the signature before
//             CryptVerifySignature is called or after CryptSignHash
//             is called. CRYPT_OID_USE_PUBKEY_PARA_FOR_PKCS7_FLAG can
//             be set to include the public key algorithm's parameters
//             in the PKCS7's digestEncryptionAlgorithm's parameters.
//             CRYPT_OID_NO_NULL_ALGORITHM_PARA_FLAG can be set to omit
//             NULL parameters when encoding.
//
// For the ECC named curve public keys
//  DWORD[1] - BCRYPT_ECCKEY_BLOB dwMagic field value
//  DWORD[2] - dwBitLength. Where BCRYPT_ECCKEY_BLOB's
//             cbKey = dwBitLength / 8 + ((dwBitLength % 8) ? 1 : 0)
//

  {$EXTERNALSYM CRYPT_OID_INHIBIT_SIGNATURE_FORMAT_FLAG}
  CRYPT_OID_INHIBIT_SIGNATURE_FORMAT_FLAG = $00000001;
  {$EXTERNALSYM CRYPT_OID_USE_PUBKEY_PARA_FOR_PKCS7_FLAG}
  CRYPT_OID_USE_PUBKEY_PARA_FOR_PKCS7_FLAG = $00000002;
  {$EXTERNALSYM CRYPT_OID_NO_NULL_ALGORITHM_PARA_FLAG}
  CRYPT_OID_NO_NULL_ALGORITHM_PARA_FLAG = $00000004;

  {$EXTERNALSYM CRYPT_OID_PUBKEY_SIGN_ONLY_FLAG}
  CRYPT_OID_PUBKEY_SIGN_ONLY_FLAG = $80000000;
  {$EXTERNALSYM CRYPT_OID_PUBKEY_ENCRYPT_ONLY_FLAG}
  CRYPT_OID_PUBKEY_ENCRYPT_ONLY_FLAG = $40000000;

// CRYPT_SIGN_ALG_OID_GROUP_ID has the following optional ExtraInfo:
//  DWORD[0] - Public Key Algid.
//  DWORD[1] - Flags. Same as above for CRYPT_PUBKEY_ALG_OID_GROUP_ID.
//  DWORD[2] - Optional CryptAcquireContext(CRYPT_VERIFYCONTEXT)'s dwProvType.
//             If omitted or 0, uses Public Key Algid to select
//             appropriate dwProvType for signature verification.

// CRYPT_RDN_ATTR_OID_GROUP_ID has the following optional ExtraInfo:
//  Array of DWORDs:
//   [0 ..] - Null terminated list of acceptable RDN attribute
//            value types. An empty list implies CERT_RDN_PRINTABLE_STRING,
//            CERT_RDN_UNICODE_STRING, 0.

//+-------------------------------------------------------------------------
//  Find OID information. Returns NULL if unable to find any information
//  for the specified key and group. Note, returns a pointer to a constant
//  data structure. The returned pointer MUST NOT be freed.
//
//  dwKeyType's:
//    CRYPT_OID_INFO_OID_KEY, pvKey points to a szOID
//    CRYPT_OID_INFO_NAME_KEY, pvKey points to a wszName
//    CRYPT_OID_INFO_ALGID_KEY, pvKey points to an ALG_ID
//    CRYPT_OID_INFO_SIGN_KEY, pvKey points to an array of two ALG_ID's:
//      ALG_ID[0] - Hash Algid
//      ALG_ID[1] - PubKey Algid
//    CRYPT_OID_INFO_CNG_ALGID_KEY, pvKey points to a wszCNGAlgid
//    CRYPT_OID_INFO_CNG_SIGN_KEY, pvKey is an array of two
//     pwszCNGAlgid's:
//      Algid[0] - Hash pwszCNGAlgid
//      Algid[1] - PubKey pwszCNGAlgid
//
//  For CRYPT_OID_INFO_NAME_KEY, CRYPT_OID_INFO_CNG_ALGID_KEY and
//  CRYPT_OID_INFO_CNG_SIGN_KEY the string comparison is case insensitive.
//
//  Setting dwGroupId to 0, searches all groups according to the dwKeyType.
//  Otherwise, only the dwGroupId is searched.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptFindOIDInfo}
function CryptFindOIDInfo(dwKeyType: DWORD; pvKey: Pointer;
  dwGroupId: DWORD): PCCRYPT_OID_INFO; stdcall;

const
  {$EXTERNALSYM CRYPT_OID_INFO_OID_KEY}
  CRYPT_OID_INFO_OID_KEY = 1;
  {$EXTERNALSYM CRYPT_OID_INFO_NAME_KEY}
  CRYPT_OID_INFO_NAME_KEY = 2;
  {$EXTERNALSYM CRYPT_OID_INFO_ALGID_KEY}
  CRYPT_OID_INFO_ALGID_KEY = 3;
  {$EXTERNALSYM CRYPT_OID_INFO_SIGN_KEY}
  CRYPT_OID_INFO_SIGN_KEY = 4;
  {$EXTERNALSYM CRYPT_OID_INFO_CNG_ALGID_KEY}
  CRYPT_OID_INFO_CNG_ALGID_KEY = 5;
  {$EXTERNALSYM CRYPT_OID_INFO_CNG_SIGN_KEY}
  CRYPT_OID_INFO_CNG_SIGN_KEY = 6;

// Set the following in the above dwKeyType parameter to restrict public keys
// valid for signing or encrypting
// certenrolld_begin -- CRYPT_*_KEY_FLAG
  {$EXTERNALSYM CRYPT_OID_INFO_OID_KEY_FLAGS_MASK}
  CRYPT_OID_INFO_OID_KEY_FLAGS_MASK = $FFFF0000;
  {$EXTERNALSYM CRYPT_OID_INFO_PUBKEY_SIGN_KEY_FLAG}
  CRYPT_OID_INFO_PUBKEY_SIGN_KEY_FLAG = $80000000;
  {$EXTERNALSYM CRYPT_OID_INFO_PUBKEY_ENCRYPT_KEY_FLAG}
  CRYPT_OID_INFO_PUBKEY_ENCRYPT_KEY_FLAG = $40000000;

// The following flag can be set in above dwGroupId parameter to disable
// searching the directory server
  {$EXTERNALSYM CRYPT_OID_DISABLE_SEARCH_DS_FLAG}
  CRYPT_OID_DISABLE_SEARCH_DS_FLAG = $80000000;

// certenrolld_end -- CRYPT_*_KEY_FLAG

// The bit length shifted left 16 bits can be OR'ed into the above
// dwGroupId parameter. Only applicable to the CRYPT_ENCRYPT_ALG_OID_GROUP_ID.
// Also, only applicable to encryption algorithms having a dwBitLen ExtraInfo.
// Currently, only the AES encryption algorithms have this.
//
// For example, to find the OIDInfo for BCRYPT_AES_ALGORITHM, bit length 192,
// CryptFindOIDInfo would be called as follows:
//  PCCRYPT_OID_INFO pOIDInfo =
//      CryptFindOIDInfo(
//          CRYPT_OID_INFO_CNG_ALGID_KEY,
//          (void *) BCRYPT_AES_ALGORITHM,
//          CRYPT_ENCRYPT_ALG_OID_GROUP_ID |
//              (192 shl CRYPT_OID_INFO_OID_GROUP_BIT_LEN_SHIFT)
//          );

  {$EXTERNALSYM CRYPT_OID_INFO_OID_GROUP_BIT_LEN_MASK}
  CRYPT_OID_INFO_OID_GROUP_BIT_LEN_MASK = $0FFF0000;
  {$EXTERNALSYM CRYPT_OID_INFO_OID_GROUP_BIT_LEN_SHIFT}
  CRYPT_OID_INFO_OID_GROUP_BIT_LEN_SHIFT = 16;

//+-------------------------------------------------------------------------
//  Register OID information. The OID information specified in the
//  CCRYPT_OID_INFO structure is persisted to the registry.
//
//  crypt32.dll contains information for the commonly known OIDs. This function
//  allows applications to augment crypt32.dll's OID information. During
//  CryptFindOIDInfo's first call, the registered OID information is installed.
//
//  By default the registered OID information is installed after crypt32.dll's
//  OID entries. Set CRYPT_INSTALL_OID_INFO_BEFORE_FLAG to install before.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptRegisterOIDInfo}
function CryptRegisterOIDInfo(pInfo: PCCRYPT_OID_INFO;
  dwFlags: DWORD): BOOL; stdcall;

const
  {$EXTERNALSYM CRYPT_INSTALL_OID_INFO_BEFORE_FLAG}
  CRYPT_INSTALL_OID_INFO_BEFORE_FLAG = 1;

//+-------------------------------------------------------------------------
//  Unregister OID information. Only the pszOID and dwGroupId fields are
//  used to identify the OID information to be unregistered.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptUnregisterOIDInfo}
function CryptUnregisterOIDInfo(pInfo: PCCRYPT_OID_INFO): BOOL; stdcall;

// If the callback returns FALSE, stops the enumeration.
type
  {$EXTERNALSYM PFN_CRYPT_ENUM_OID_INFO}
  PFN_CRYPT_ENUM_OID_INFO = function(pInfo: PCCRYPT_OID_INFO;
    pvArg: Pointer): BOOL stdcall;
  TFnCryptEnumOIDInfo = PFN_CRYPT_ENUM_OID_INFO;

//+-------------------------------------------------------------------------
//  Enumerate the OID information.
//
//  pfnEnumOIDInfo is called for each OID information entry.
//
//  Setting dwGroupId to 0 matches all groups. Otherwise, only enumerates
//  entries in the specified group.
//
//  dwFlags currently isn't used and must be set to 0.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptEnumOIDInfo}
function CryptEnumOIDInfo(dwGroupId, dwFlags: DWORD; pvArg: Pointer;
  pfnEnumOIDInfo: PFN_CRYPT_ENUM_OID_INFO): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Find the localized name for the specified name. For example, find the
//  localized name for the 'Root' system store name. A case insensitive
//  string comparison is done.
//
//  Returns NULL if unable to find the the specified name.
//
//  Localized names for the predefined system stores ('Root', 'My') and
//  predefined physical stores ('.Default', '.LocalMachine') are pre-installed
//  as resource strings in crypt32.dll. CryptSetOIDFunctionValue can be called
//  as follows to register additional localized strings:
//      dwEncodingType = CRYPT_LOCALIZED_NAME_ENCODING_TYPE
//      pszFuncName = CRYPT_OID_FIND_LOCALIZED_NAME_FUNC
//      pszOID = CRYPT_LOCALIZED_NAME_OID
//      pwszValueName = Name to be localized, for example, 'ApplicationStore'
//      dwValueType = REG_SZ
//      pbValueData = pointer to the UNICODE localized string
//      cbValueData = (wcslen(UNICODE localized string) + 1) * sizeof(WCHAR)
//
//  To unregister, set pbValueData to NULL and cbValueData to 0.
//
//  The registered names are searched before the pre-installed names.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptFindLocalizedName}
function CryptFindLocalizedName(pwszCryptName: LPCWSTR): LPCWSTR; stdcall;

const
  {$EXTERNALSYM CRYPT_LOCALIZED_NAME_ENCODING_TYPE}
  CRYPT_LOCALIZED_NAME_ENCODING_TYPE = 0;
  {$EXTERNALSYM CRYPT_LOCALIZED_NAME_OID}
  CRYPT_LOCALIZED_NAME_OID            = 'LocalizedNames';

//+=========================================================================
//  Low Level Cryptographic Message Data Structures and APIs
//==========================================================================
type
  {$EXTERNALSYM HCRYPTMSG}
  HCRYPTMSG = Pointer; // TODO: THandle?

const
  {$EXTERNALSYM szOID_PKCS_7_DATA}
  szOID_PKCS_7_DATA               = '1.2.840.113549.1.7.1';
  {$EXTERNALSYM szOID_PKCS_7_SIGNED}
  szOID_PKCS_7_SIGNED             = '1.2.840.113549.1.7.2';
  {$EXTERNALSYM szOID_PKCS_7_ENVELOPED}
  szOID_PKCS_7_ENVELOPED          = '1.2.840.113549.1.7.3';
  {$EXTERNALSYM szOID_PKCS_7_SIGNEDANDENVELOPED}
  szOID_PKCS_7_SIGNEDANDENVELOPED = '1.2.840.113549.1.7.4';
  {$EXTERNALSYM szOID_PKCS_7_DIGESTED}
  szOID_PKCS_7_DIGESTED           = '1.2.840.113549.1.7.5';
  {$EXTERNALSYM szOID_PKCS_7_ENCRYPTED}
  szOID_PKCS_7_ENCRYPTED          = '1.2.840.113549.1.7.6';

  {$EXTERNALSYM szOID_PKCS_9_CONTENT_TYPE}
  szOID_PKCS_9_CONTENT_TYPE       = '1.2.840.113549.1.9.3';
  {$EXTERNALSYM szOID_PKCS_9_MESSAGE_DIGEST}
  szOID_PKCS_9_MESSAGE_DIGEST     = '1.2.840.113549.1.9.4';

//+-------------------------------------------------------------------------
//  Message types
//--------------------------------------------------------------------------
  {$EXTERNALSYM CMSG_DATA}
  CMSG_DATA = 1;
  {$EXTERNALSYM CMSG_SIGNED}
  CMSG_SIGNED = 2;
  {$EXTERNALSYM CMSG_ENVELOPED}
  CMSG_ENVELOPED = 3;
  {$EXTERNALSYM CMSG_SIGNED_AND_ENVELOPED}
  CMSG_SIGNED_AND_ENVELOPED = 4;
  {$EXTERNALSYM CMSG_HASHED}
  CMSG_HASHED = 5;
  {$EXTERNALSYM CMSG_ENCRYPTED}
  CMSG_ENCRYPTED = 6;

//+-------------------------------------------------------------------------
//  Message Type Bit Flags
//--------------------------------------------------------------------------
  {$EXTERNALSYM CMSG_ALL_FLAGS}
  CMSG_ALL_FLAGS                   = (not Cardinal(0));
  {$EXTERNALSYM CMSG_DATA_FLAG}
  CMSG_DATA_FLAG                   = (1 shl CMSG_DATA);
  {$EXTERNALSYM CMSG_SIGNED_FLAG}
  CMSG_SIGNED_FLAG                 = (1 shl CMSG_SIGNED);
  {$EXTERNALSYM CMSG_ENVELOPED_FLAG}
  CMSG_ENVELOPED_FLAG              = (1 shl CMSG_ENVELOPED);
  {$EXTERNALSYM CMSG_SIGNED_AND_ENVELOPED_FLAG}
  CMSG_SIGNED_AND_ENVELOPED_FLAG   = (1 shl CMSG_SIGNED_AND_ENVELOPED);
  {$EXTERNALSYM CMSG_HASHED_FLAG}
  CMSG_HASHED_FLAG                 = (1 shl CMSG_HASHED);
  {$EXTERNALSYM CMSG_ENCRYPTED_FLAG}
  CMSG_ENCRYPTED_FLAG              = (1 shl CMSG_ENCRYPTED);

//+-------------------------------------------------------------------------
//  Certificate Issuer and SerialNumber
//--------------------------------------------------------------------------
type
  PCertIssuerSerialNumber = ^TCertIssuerSerialNumber;
  {$EXTERNALSYM _CERT_ISSUER_SERIAL_NUMBER}
  _CERT_ISSUER_SERIAL_NUMBER = record
    Issuer: CERT_NAME_BLOB;
    SerialNumber: CRYPT_INTEGER_BLOB;
  end;
  {$EXTERNALSYM CERT_ISSUER_SERIAL_NUMBER}
  CERT_ISSUER_SERIAL_NUMBER = _CERT_ISSUER_SERIAL_NUMBER;
  {$EXTERNALSYM PCERT_ISSUER_SERIAL_NUMBER}
  PCERT_ISSUER_SERIAL_NUMBER = ^_CERT_ISSUER_SERIAL_NUMBER;
  TCertIssuerSerialNumber = _CERT_ISSUER_SERIAL_NUMBER;

//+-------------------------------------------------------------------------
//  Certificate Identifier
//--------------------------------------------------------------------------
const
  {$EXTERNALSYM CERT_ID_ISSUER_SERIAL_NUMBER}
  CERT_ID_ISSUER_SERIAL_NUMBER = 1;
  {$EXTERNALSYM CERT_ID_KEY_IDENTIFIER}
  CERT_ID_KEY_IDENTIFIER = 2;
  {$EXTERNALSYM CERT_ID_SHA1_HASH}
  CERT_ID_SHA1_HASH = 3;

type
  PCertID = ^TCertID;
  {$EXTERNALSYM _CERT_ID}
  _CERT_ID = record
    case dwIdChoice: DWORD of
      CERT_ID_ISSUER_SERIAL_NUMBER:
        (IssuerSerialNumber: CERT_ISSUER_SERIAL_NUMBER);
      CERT_ID_KEY_IDENTIFIER:
        (KeyId: CRYPT_HASH_BLOB);
      CERT_ID_SHA1_HASH:
        (HashId: CRYPT_HASH_BLOB);
  end;
  {$EXTERNALSYM CERT_ID}
  CERT_ID = _CERT_ID;
  {$EXTERNALSYM PCERT_ID}
  PCERT_ID = ^_CERT_ID;
  TCertID = _CERT_ID;

//+-------------------------------------------------------------------------
//  The message encode information (pvMsgEncodeInfo) is message type dependent
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_DATA: pvMsgEncodeInfo = NULL
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_SIGNED
//
//  The pCertInfo in the CMSG_SIGNER_ENCODE_INFO provides the Issuer, SerialNumber
//  and PublicKeyInfo.Algorithm. The PublicKeyInfo.Algorithm implicitly
//  specifies the HashEncryptionAlgorithm to be used.
//
//  If the SignerId is present with a nonzero dwIdChoice its used instead
//  of the Issuer and SerialNumber in pCertInfo.
//
//  CMS supports the KEY_IDENTIFIER and ISSUER_SERIAL_NUMBER CERT_IDs. PKCS #7
//  version 1.5 only supports the ISSUER_SERIAL_NUMBER CERT_ID choice.
//
//  If HashEncryptionAlgorithm is present and not NULL its used instead of
//  the PublicKeyInfo.Algorithm.
//
//  Note, for RSA, the hash encryption algorithm is normally the same as
//  the public key algorithm. For DSA, the hash encryption algorithm is
//  normally a DSS signature algorithm.
//
//  pvHashEncryptionAuxInfo currently isn't used and must be set to NULL if
//  present in the data structure.
//
//  The hCryptProv and dwKeySpec specify the private key to use. If dwKeySpec
//  == 0, then, defaults to AT_SIGNATURE.
//
//  If the HashEncryptionAlgorithm is set to szOID_PKIX_NO_SIGNATURE, then,
//  the signature value only contains the hash octets. hCryptProv must still
//  be specified. However, since a private key isn't used the hCryptProv can be
//  acquired using CRYPT_VERIFYCONTEXT.
//
//  If CMSG_CRYPT_RELEASE_CONTEXT_FLAG is set in the dwFlags
//  passed to CryptMsgOpenToEncode(), the signer hCryptProv's are released.
//
//  For CNG, this applies to the hNCryptKey.
//
//  pvHashAuxInfo currently isn't used and must be set to NULL.
//
//  CMS signed messages allow the inclusion of Attribute Certs.
//--------------------------------------------------------------------------

  PCMsgSignerEncodeInfo = ^TCMsgSignerEncodeInfo;
  {$EXTERNALSYM _CMSG_SIGNER_ENCODE_INFO}
  _CMSG_SIGNER_ENCODE_INFO = record
    cbSize: DWORD;
    pCertInfo: PCERT_INFO;

    // NCryptIsKeyHandle() is called to determine the union choice.
    case Byte of
      0: (hNCryptKey: NCRYPT_KEY_HANDLE);  // size: 4
      1: (hCryptProv: HCRYPTPROV;          // size: 4

    // not applicable for hNCryptKey choice
    dwKeySpec: DWORD;

    HashAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    pvHashAuxInfo: Pointer;
    cAuthAttr: DWORD;
    rgAuthAttr: PCRYPT_ATTRIBUTE;
    cUnauthAttr: DWORD;
    rgUnauthAttr: PCRYPT_ATTRIBUTE;

// #ifdef CMSG_SIGNER_ENCODE_INFO_HAS_CMS_FIELDS
    SignerId: CERT_ID;

    // This is also referred to as the SignatureAlgorithm
    HashEncryptionAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    pvHashEncryptionAuxInfo: Pointer;
    );
  end;
// #endif
  {$EXTERNALSYM CMSG_SIGNER_ENCODE_INFO}
  CMSG_SIGNER_ENCODE_INFO = _CMSG_SIGNER_ENCODE_INFO;
  {$EXTERNALSYM PCMSG_SIGNER_ENCODE_INFO}
  PCMSG_SIGNER_ENCODE_INFO = ^_CMSG_SIGNER_ENCODE_INFO;
  TCMsgSignerEncodeInfo = _CMSG_SIGNER_ENCODE_INFO;

  PCMsgSignedEncodeInfo = ^TCMsgSignedEncodeInfo;
  {$EXTERNALSYM _CMSG_SIGNED_ENCODE_INFO}
  _CMSG_SIGNED_ENCODE_INFO = record
    cbSize: DWORD;
    cSigners: DWORD;
    rgSigners: PCMSG_SIGNER_ENCODE_INFO;
    cCertEncoded: DWORD;
    rgCertEncoded: PCERT_BLOB;
    cCrlEncoded: DWORD;
    rgCrlEncoded: PCRL_BLOB;

// #ifdef CMSG_SIGNED_ENCODE_INFO_HAS_CMS_FIELDS
    cAttrCertEncoded: DWORD;
    rgAttrCertEncoded: PCERT_BLOB;
// #endif
  end;
  {$EXTERNALSYM CMSG_SIGNED_ENCODE_INFO}
  CMSG_SIGNED_ENCODE_INFO = _CMSG_SIGNED_ENCODE_INFO;
  {$EXTERNALSYM PCMSG_SIGNED_ENCODE_INFO}
  PCMSG_SIGNED_ENCODE_INFO = ^_CMSG_SIGNED_ENCODE_INFO;
  TCMsgSignedEncodeInfo = _CMSG_SIGNED_ENCODE_INFO;

//+-------------------------------------------------------------------------
//  CMSG_ENVELOPED
//
//  The PCERT_INFO for the rgRecipients provides the Issuer, SerialNumber
//  and PublicKeyInfo. The PublicKeyInfo.Algorithm implicitly
//  specifies the KeyEncryptionAlgorithm to be used.
//
//  The PublicKeyInfo.PublicKey in PCERT_INFO is used to encrypt the content
//  encryption key for the recipient.
//
//  hCryptProv is used to do the content encryption, recipient key encryption
//  and export. The hCryptProv's private keys aren't used. If hCryptProv
//  is NULL, a default hCryptProv is chosen according to the
//  ContentEncryptionAlgorithm and the first recipient KeyEncryptionAlgorithm.
//
//  If CMSG_CRYPT_RELEASE_CONTEXT_FLAG is set in the dwFlags
//  passed to CryptMsgOpenToEncode(), the envelope's hCryptProv is released.
//
//  Note: CAPI currently doesn't support more than one KeyEncryptionAlgorithm
//  per provider. This will need to be fixed.
//
//  Currently, pvEncryptionAuxInfo is only defined for RC2 or RC4 encryption
//  algorithms. Otherwise, its not used and must be set to NULL.
//  See CMSG_RC2_AUX_INFO for the RC2 encryption algorithms.
//  See CMSG_RC4_AUX_INFO for the RC4 encryption algorithms.
//
//  To enable SP3 compatible encryption, pvEncryptionAuxInfo should point to
//  a CMSG_SP3_COMPATIBLE_AUX_INFO data structure.
//
//  To enable the CMS envelope enhancements, rgpRecipients must be set to
//  NULL, and rgCmsRecipients updated to point to an array of
//  CMSG_RECIPIENT_ENCODE_INFO's.
//
//  Also, CMS envelope enhancements support the inclusion of a bag of
//  Certs, CRLs, Attribute Certs and/or Unprotected Attributes.
//
//  AES ContentEncryption and ECC KeyAgreement recipients are only supported
//  via CNG. DH KeyAgreement or mail list recipients are only supported via
//  CAPI1. SP3 compatible encryption and RC4 are only supported via CAPI1.
//
//  For an RSA recipient identified via PCERT_INFO, for AES ContentEncryption,
//  szOID_RSAES_OAEP will be implicitly used for the KeyEncryptionAlgorithm.
//--------------------------------------------------------------------------
const
  {$EXTERNALSYM CMSG_KEY_AGREE_EPHEMERAL_KEY_CHOICE}
  CMSG_KEY_AGREE_EPHEMERAL_KEY_CHOICE = 1;
  {$EXTERNALSYM CMSG_KEY_AGREE_STATIC_KEY_CHOICE}
  CMSG_KEY_AGREE_STATIC_KEY_CHOICE = 2;

  {$EXTERNALSYM CMSG_MAIL_LIST_HANDLE_KEY_CHOICE}
  CMSG_MAIL_LIST_HANDLE_KEY_CHOICE = 1;

  {$EXTERNALSYM CMSG_KEY_TRANS_RECIPIENT}
  CMSG_KEY_TRANS_RECIPIENT = 1;
  {$EXTERNALSYM CMSG_KEY_AGREE_RECIPIENT}
  CMSG_KEY_AGREE_RECIPIENT = 2;
  {$EXTERNALSYM CMSG_MAIL_LIST_RECIPIENT}
  CMSG_MAIL_LIST_RECIPIENT = 3;

type
  {$EXTERNALSYM PCMSG_RECIPIENT_ENCODE_INFO}
  PCMSG_RECIPIENT_ENCODE_INFO = ^_CMSG_RECIPIENT_ENCODE_INFO;

  PCMsgEnvelopedEncodeInfo = ^TCMsgEnvelopedEncodeInfo;
  {$EXTERNALSYM _CMSG_ENVELOPED_ENCODE_INFO}
  _CMSG_ENVELOPED_ENCODE_INFO = record
    cbSize: DWORD;
    hCryptProv: HCRYPTPROV_LEGACY;
    ContentEncryptionAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    pvEncryptionAuxInfo: Pointer;
    cRecipients: DWORD;

    // The following array may only be used for transport recipients identified
    // by their IssuereAndSerialNumber. If rgpRecipients != NULL, then,
    // the rgCmsRecipients must be NULL.
    rgpRecipients: ^PCERT_INFO;

// #ifdef CMSG_ENVELOPED_ENCODE_INFO_HAS_CMS_FIELDS
    // If rgCmsRecipients != NULL, then, the above rgpRecipients must be
    // NULL.
    rgCmsRecipients: PCMSG_RECIPIENT_ENCODE_INFO;
    cCertEncoded: DWORD;
    rgCertEncoded: PCERT_BLOB;
    cCrlEncoded: DWORD;
    rgCrlEncoded: PCRL_BLOB;
    cAttrCertEncoded: DWORD;
    rgAttrCertEncoded: PCERT_BLOB;
    cUnprotectedAttr: DWORD;
    rgUnprotectedAttr: PCRYPT_ATTRIBUTE;
// #endif
  end;
  {$EXTERNALSYM CMSG_ENVELOPED_ENCODE_INFO}
  CMSG_ENVELOPED_ENCODE_INFO = _CMSG_ENVELOPED_ENCODE_INFO;
  {$EXTERNALSYM PCMSG_ENVELOPED_ENCODE_INFO}
  PCMSG_ENVELOPED_ENCODE_INFO = ^_CMSG_ENVELOPED_ENCODE_INFO;
  TCMsgEnvelopedEncodeInfo = _CMSG_ENVELOPED_ENCODE_INFO;


//+-------------------------------------------------------------------------
//  Key Transport Recipient Encode Info
//
//  hCryptProv is used to do the recipient key encryption
//  and export. The hCryptProv's private keys aren't used.
//
//  If hCryptProv is NULL, then, the hCryptProv specified in
//  CMSG_ENVELOPED_ENCODE_INFO is used.
//
//  Note, even if CMSG_CRYPT_RELEASE_CONTEXT_FLAG is set in the dwFlags
//  passed to CryptMsgOpenToEncode(), this hCryptProv isn't released.
//
//  CMS supports the KEY_IDENTIFIER and ISSUER_SERIAL_NUMBER CERT_IDs. PKCS #7
//  version 1.5 only supports the ISSUER_SERIAL_NUMBER CERT_ID choice.
//
//  For RSA AES, KeyEncryptionAlgorithm.pszObjId should be set to
//  szOID_RSAES_OAEP. KeyEncryptionAlgorithm.Parameters should be set
//  to the encoded PKCS_RSAES_OAEP_PARAMETERS. If
//  KeyEncryptionAlgorithm.Parameters.cbData == 0, then, the default
//  parameters are used and encoded.
//--------------------------------------------------------------------------
  PCMsgKeyTransRecipientEncodeInfo = ^TCMsgKeyTransRecipientEncodeInfo;
  {$EXTERNALSYM _CMSG_KEY_TRANS_RECIPIENT_ENCODE_INFO}
  _CMSG_KEY_TRANS_RECIPIENT_ENCODE_INFO = record
    cbSize: DWORD;
    KeyEncryptionAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    pvKeyEncryptionAuxInfo: Pointer;
    hCryptProv: HCRYPTPROV_LEGACY;
    RecipientPublicKey: CRYPT_BIT_BLOB;
    RecipientId: CERT_ID;
  end;
  {$EXTERNALSYM CMSG_KEY_TRANS_RECIPIENT_ENCODE_INFO}
  CMSG_KEY_TRANS_RECIPIENT_ENCODE_INFO = _CMSG_KEY_TRANS_RECIPIENT_ENCODE_INFO;
  {$EXTERNALSYM PCMSG_KEY_TRANS_RECIPIENT_ENCODE_INFO}
  PCMSG_KEY_TRANS_RECIPIENT_ENCODE_INFO = ^_CMSG_KEY_TRANS_RECIPIENT_ENCODE_INFO;
  TCMsgKeyTransRecipientEncodeInfo = _CMSG_KEY_TRANS_RECIPIENT_ENCODE_INFO;


//+-------------------------------------------------------------------------
//  Key Agreement Recipient Encode Info
//
//  If hCryptProv is NULL, then, the hCryptProv specified in
//  CMSG_ENVELOPED_ENCODE_INFO is used.
//
//  For the CMSG_KEY_AGREE_STATIC_KEY_CHOICE, both the hCryptProv and
//  dwKeySpec must be specified to select the sender's private key.
//
//  Note, even if CMSG_CRYPT_RELEASE_CONTEXT_FLAG is set in the dwFlags
//  passed to CryptMsgOpenToEncode(), this hCryptProv isn't released.
//
//  CMS supports the KEY_IDENTIFIER and ISSUER_SERIAL_NUMBER CERT_IDs.
//
//  There is 1 key choice, ephemeral originator. The originator's ephemeral
//  key is generated using the public key algorithm parameters shared
//  amongst all the recipients.
//
//  There are 2 key choices: ephemeral originator or static sender. The
//  originator's ephemeral key is generated using the public key algorithm
//  parameters shared amongst all the recipients. For the static sender its
//  private key is used. The hCryptProv and dwKeySpec specify the private key.
//  The pSenderId identifies the certificate containing the sender's public key.
//
//  Currently, pvKeyEncryptionAuxInfo isn't used and must be set to NULL.
//
//  If KeyEncryptionAlgorithm.Parameters.cbData == 0, then, its Parameters
//  are updated with the encoded KeyWrapAlgorithm.
//
//  Currently, pvKeyWrapAuxInfo is only defined for algorithms with
//  RC2. Otherwise, its not used and must be set to NULL.
//  When set for RC2 algorithms, points to a CMSG_RC2_AUX_INFO containing
//  the RC2 effective key length.
//
//  Note, key agreement recipients are not supported in PKCS #7 version 1.5.
//
//  For the ECC szOID_DH_SINGLE_PASS_STDDH_SHA1_KDF KeyEncryptionAlgorithm
//  the CMSG_KEY_AGREE_EPHEMERAL_KEY_CHOICE must be specified.
//--------------------------------------------------------------------------
  PCMsgRecipientEncryptedKeyEncodeInfo = ^TCMsgRecipientEncryptedKeyEncodeInfo;
  {$EXTERNALSYM _CMSG_RECIPIENT_ENCRYPTED_KEY_ENCODE_INFO}
  _CMSG_RECIPIENT_ENCRYPTED_KEY_ENCODE_INFO = record
    cbSize: DWORD;
    RecipientPublicKey: CRYPT_BIT_BLOB;
    RecipientId: CERT_ID;

    // Following fields are optional and only applicable to KEY_IDENTIFIER
    // CERT_IDs.
    Date: FILETIME;
    pOtherAttr: PCRYPT_ATTRIBUTE_TYPE_VALUE;
  end;
  {$EXTERNALSYM CMSG_RECIPIENT_ENCRYPTED_KEY_ENCODE_INFO}
  CMSG_RECIPIENT_ENCRYPTED_KEY_ENCODE_INFO = _CMSG_RECIPIENT_ENCRYPTED_KEY_ENCODE_INFO;
  {$EXTERNALSYM PCMSG_RECIPIENT_ENCRYPTED_KEY_ENCODE_INFO}
  PCMSG_RECIPIENT_ENCRYPTED_KEY_ENCODE_INFO = ^_CMSG_RECIPIENT_ENCRYPTED_KEY_ENCODE_INFO;
  TCMsgRecipientEncryptedKeyEncodeInfo = _CMSG_RECIPIENT_ENCRYPTED_KEY_ENCODE_INFO;

  PCMsgKeyAgreeRecipientEncodeInfo = ^TCMsgKeyAgreeRecipientEncodeInfo;
  {$EXTERNALSYM _CMSG_KEY_AGREE_RECIPIENT_ENCODE_INFO}
  _CMSG_KEY_AGREE_RECIPIENT_ENCODE_INFO = record
    cbSize: DWORD;
    KeyEncryptionAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    pvKeyEncryptionAuxInfo: Pointer;
    KeyWrapAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    pvKeyWrapAuxInfo: Pointer;

    // The following hCryptProv and dwKeySpec must be specified for the
    // CMSG_KEY_AGREE_STATIC_KEY_CHOICE.
    //
    // For CMSG_KEY_AGREE_EPHEMERAL_KEY_CHOICE, dwKeySpec isn't applicable
    // and hCryptProv is optional.

    hCryptProv: HCRYPTPROV_LEGACY;
    dwKeySpec: DWORD;

    dwKeyChoice: DWORD;
    case Byte of
      CMSG_KEY_AGREE_EPHEMERAL_KEY_CHOICE:
        //
        // The ephemeral public key algorithm and parameters.
        (pEphemeralAlgorithm: PCRYPT_ALGORITHM_IDENTIFIER);    // size: 4

      CMSG_KEY_AGREE_STATIC_KEY_CHOICE:
        //
        // The CertId of the sender's certificate
        (pSenderId: PCERT_ID;                                  // size: 4
    UserKeyingMaterial: CRYPT_DATA_BLOB;     // OPTIONAL

    cRecipientEncryptedKeys: DWORD;
    rgpRecipientEncryptedKeys: ^PCMSG_RECIPIENT_ENCRYPTED_KEY_ENCODE_INFO
    );
  end;
  {$EXTERNALSYM CMSG_KEY_AGREE_RECIPIENT_ENCODE_INFO}
  CMSG_KEY_AGREE_RECIPIENT_ENCODE_INFO = _CMSG_KEY_AGREE_RECIPIENT_ENCODE_INFO;
  {$EXTERNALSYM PCMSG_KEY_AGREE_RECIPIENT_ENCODE_INFO}
  PCMSG_KEY_AGREE_RECIPIENT_ENCODE_INFO = ^_CMSG_KEY_AGREE_RECIPIENT_ENCODE_INFO;
  TCMsgKeyAgreeRecipientEncodeInfo = _CMSG_KEY_AGREE_RECIPIENT_ENCODE_INFO;

//+-------------------------------------------------------------------------
//  Mail List Recipient Encode Info
//
//  There is 1 choice for the KeyEncryptionKey: an already created CSP key
//  handle. For the key handle choice, hCryptProv must be nonzero. This key
//  handle isn't destroyed.
//
//  Note, even if CMSG_CRYPT_RELEASE_CONTEXT_FLAG is set in the dwFlags
//  passed to CryptMsgOpenToEncode(), this hCryptProv isn't released.
//
//  Currently, pvKeyEncryptionAuxInfo is only defined for RC2 key wrap
//  algorithms. Otherwise, its not used and must be set to NULL.
//  When set for RC2 algorithms, points to a CMSG_RC2_AUX_INFO containing
//  the RC2 effective key length.
//
//  Note, mail list recipients are not supported in PKCS #7 version 1.5.
//
//  Mail list recipients aren't supported using CNG.
//--------------------------------------------------------------------------
  PCMsgMailListRecipientEncodeInfo = ^TCMsgMailListRecipientEncodeInfo;
  {$EXTERNALSYM _CMSG_MAIL_LIST_RECIPIENT_ENCODE_INFO}
  _CMSG_MAIL_LIST_RECIPIENT_ENCODE_INFO = record
    cbSize: DWORD;
    KeyEncryptionAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    pvKeyEncryptionAuxInfo: Pointer;
    hCryptProv: HCRYPTPROV;
    case dwKeyChoice: DWORD of
      CMSG_MAIL_LIST_HANDLE_KEY_CHOICE:
        (hKeyEncryptionKey: HCRYPTKEY);                   // size: 4
      0:
        // Reserve space for a potential pointer choice
        (pvKeyEncryptionKey: Pointer;                     // size: 4
    KeyId: CRYPT_DATA_BLOB;

    // Following fields are optional.
    Date: FILETIME;
    pOtherAttr: PCRYPT_ATTRIBUTE_TYPE_VALUE
    );
  end;
  {$EXTERNALSYM CMSG_MAIL_LIST_RECIPIENT_ENCODE_INFO}
  CMSG_MAIL_LIST_RECIPIENT_ENCODE_INFO = _CMSG_MAIL_LIST_RECIPIENT_ENCODE_INFO;
  {$EXTERNALSYM PCMSG_MAIL_LIST_RECIPIENT_ENCODE_INFO}
  PCMSG_MAIL_LIST_RECIPIENT_ENCODE_INFO = ^_CMSG_MAIL_LIST_RECIPIENT_ENCODE_INFO;
  TCMsgMailListRecipientEncodeInfo = _CMSG_MAIL_LIST_RECIPIENT_ENCODE_INFO;

//+-------------------------------------------------------------------------
//  Recipient Encode Info
//
//  Note, only key transport recipients are supported in PKCS #7 version 1.5.
//--------------------------------------------------------------------------
  PCMsgRecipientEncodeInfo = ^TCMsgRecipientEncodeInfo;
  {$EXTERNALSYM _CMSG_RECIPIENT_ENCODE_INFO}
  _CMSG_RECIPIENT_ENCODE_INFO = record
    case dwRecipientChoice: DWORD of
      CMSG_KEY_TRANS_RECIPIENT:
        (pKeyTrans: PCMSG_KEY_TRANS_RECIPIENT_ENCODE_INFO);
      CMSG_KEY_AGREE_RECIPIENT:
        (pKeyAgree: PCMSG_KEY_AGREE_RECIPIENT_ENCODE_INFO);
      CMSG_MAIL_LIST_RECIPIENT:
        (pMailList: PCMSG_MAIL_LIST_RECIPIENT_ENCODE_INFO);
  end;
  {$EXTERNALSYM CMSG_RECIPIENT_ENCODE_INFO}
  CMSG_RECIPIENT_ENCODE_INFO = _CMSG_RECIPIENT_ENCODE_INFO;
  // PCMSG_RECIPIENT_ENCODE_INFO forward defined at start of this type section
  TCMsgRecipientEncodeInfo = _CMSG_RECIPIENT_ENCODE_INFO;

//+-------------------------------------------------------------------------
//  CMSG_RC2_AUX_INFO
//
//  AuxInfo for RC2 encryption algorithms. The pvEncryptionAuxInfo field
//  in CMSG_ENCRYPTED_ENCODE_INFO should be updated to point to this
//  structure. If not specified, defaults to 40 bit.
//
//  Note, this AuxInfo is only used when, the ContentEncryptionAlgorithm's
//  Parameter.cbData is zero. Otherwise, the Parameters is decoded to
//  get the bit length.
//
//  If CMSG_SP3_COMPATIBLE_ENCRYPT_FLAG is set in dwBitLen, then, SP3
//  compatible encryption is done and the bit length is ignored.
//--------------------------------------------------------------------------
  PCMsgRC2AuxInfo = ^TCMsgRC2AuxInfo;
  {$EXTERNALSYM _CMSG_RC2_AUX_INFO}
  _CMSG_RC2_AUX_INFO = record
    cbSize: DWORD;
    dwBitLen: DWORD;
  end;
  {$EXTERNALSYM CMSG_RC2_AUX_INFO}
  CMSG_RC2_AUX_INFO = _CMSG_RC2_AUX_INFO;
  {$EXTERNALSYM PCMSG_RC2_AUX_INFO}
  PCMSG_RC2_AUX_INFO = ^_CMSG_RC2_AUX_INFO;
  TCMsgRC2AuxInfo = _CMSG_RC2_AUX_INFO;

//+-------------------------------------------------------------------------
//  CMSG_SP3_COMPATIBLE_AUX_INFO
//
//  AuxInfo for enabling SP3 compatible encryption.
//
//  The CMSG_SP3_COMPATIBLE_ENCRYPT_FLAG is set in dwFlags to enable SP3
//  compatible encryption. When set, uses zero salt instead of no salt,
//  the encryption algorithm parameters are NULL instead of containing the
//  encoded RC2 parameters or encoded IV octet string and the encrypted
//  symmetric key is encoded little endian instead of big endian.
//
//  SP3 compatible encryption isn't supported using CNG.
//--------------------------------------------------------------------------
  PCMsgSP3CompatibleAuxInfo = ^TCMsgSP3CompatibleAuxInfo;
  {$EXTERNALSYM _CMSG_SP3_COMPATIBLE_AUX_INFO}
  _CMSG_SP3_COMPATIBLE_AUX_INFO = record
    cbSize: DWORD;
    dwFlags: DWORD;
  end;
  {$EXTERNALSYM CMSG_SP3_COMPATIBLE_AUX_INFO}
  CMSG_SP3_COMPATIBLE_AUX_INFO = _CMSG_SP3_COMPATIBLE_AUX_INFO;
  {$EXTERNALSYM PCMSG_SP3_COMPATIBLE_AUX_INFO}
  PCMSG_SP3_COMPATIBLE_AUX_INFO = ^_CMSG_SP3_COMPATIBLE_AUX_INFO;
  TCMsgSP3CompatibleAuxInfo = _CMSG_SP3_COMPATIBLE_AUX_INFO;

const
  {$EXTERNALSYM CMSG_SP3_COMPATIBLE_ENCRYPT_FLAG}
  CMSG_SP3_COMPATIBLE_ENCRYPT_FLAG = $80000000;

//+-------------------------------------------------------------------------
//  CMSG_RC4_AUX_INFO
//
//  AuxInfo for RC4 encryption algorithms. The pvEncryptionAuxInfo field
//  in CMSG_ENCRYPTED_ENCODE_INFO should be updated to point to this
//  structure. If not specified, uses the CSP's default bit length with no
//  salt. Note, the base CSP has a 40 bit default and the enhanced CSP has
//  a 128 bit default.
//
//  If CMSG_RC4_NO_SALT_FLAG is set in dwBitLen, then, no salt is generated.
//  Otherwise, (128 - dwBitLen)/8 bytes of salt are generated and encoded
//  as an OCTET STRING in the algorithm parameters field.
//
//  RC4 isn't supported using CNG.
//--------------------------------------------------------------------------
type
  PCMsgRC4AuxInfo = ^TCMsgRC4AuxInfo;
  {$EXTERNALSYM _CMSG_RC4_AUX_INFO}
  _CMSG_RC4_AUX_INFO = record
    cbSize: DWORD;
    dwBitLen: DWORD;
  end;
  {$EXTERNALSYM CMSG_RC4_AUX_INFO}
  CMSG_RC4_AUX_INFO = _CMSG_RC4_AUX_INFO;
  {$EXTERNALSYM PCMSG_RC4_AUX_INFO}
  PCMSG_RC4_AUX_INFO = ^_CMSG_RC4_AUX_INFO;
  TCMsgRC4AuxInfo = _CMSG_RC4_AUX_INFO;

const
  {$EXTERNALSYM CMSG_RC4_NO_SALT_FLAG}
  CMSG_RC4_NO_SALT_FLAG = $40000000;

//+-------------------------------------------------------------------------
//  CMSG_SIGNED_AND_ENVELOPED
//
//  For PKCS #7, a signed and enveloped message doesn't have the
//  signer's authenticated or unauthenticated attributes. Otherwise, a
//  combination of the CMSG_SIGNED_ENCODE_INFO and CMSG_ENVELOPED_ENCODE_INFO.
//--------------------------------------------------------------------------
type
  PCMsgSignedAndEnvelopedEncodeInfo = ^TCMsgSignedAndEnvelopedEncodeInfo;
  {$EXTERNALSYM _CMSG_SIGNED_AND_ENVELOPED_ENCODE_INFO}
  _CMSG_SIGNED_AND_ENVELOPED_ENCODE_INFO = record
    cbSize: DWORD;
    SignedInfo: CMSG_SIGNED_ENCODE_INFO;
    EnvelopedInfo: CMSG_ENVELOPED_ENCODE_INFO;
  end;
  {$EXTERNALSYM CMSG_SIGNED_AND_ENVELOPED_ENCODE_INFO}
  CMSG_SIGNED_AND_ENVELOPED_ENCODE_INFO = _CMSG_SIGNED_AND_ENVELOPED_ENCODE_INFO;
  {$EXTERNALSYM PCMSG_SIGNED_AND_ENVELOPED_ENCODE_INFO}
  PCMSG_SIGNED_AND_ENVELOPED_ENCODE_INFO = ^_CMSG_SIGNED_AND_ENVELOPED_ENCODE_INFO;
  TCMsgSignedAndEnvelopedEncodeInfo = _CMSG_SIGNED_AND_ENVELOPED_ENCODE_INFO;

//+-------------------------------------------------------------------------
//  CMSG_HASHED
//
//  hCryptProv is used to do the hash. Doesn't need to use a private key.
//
//  If CMSG_CRYPT_RELEASE_CONTEXT_FLAG is set in the dwFlags
//  passed to CryptMsgOpenToEncode(), the hCryptProv is released.
//
//  IN LH, the hCryptProv isn't used. However, its still released if the
//  above flag is set.
//
//  If fDetachedHash is set, then, the encoded message doesn't contain
//  any content (its treated as NULL Data)
//
//  pvHashAuxInfo currently isn't used and must be set to NULL.
//--------------------------------------------------------------------------
  PCMsgHashedEncodeInfo = ^TCMsgHashedEncodeInfo;
  {$EXTERNALSYM _CMSG_HASHED_ENCODE_INFO}
  _CMSG_HASHED_ENCODE_INFO = record
    cbSize: DWORD;
    hCryptProv: HCRYPTPROV_LEGACY;
    HashAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    pvHashAuxInfo: Pointer;
  end;
  {$EXTERNALSYM CMSG_HASHED_ENCODE_INFO}
  CMSG_HASHED_ENCODE_INFO = _CMSG_HASHED_ENCODE_INFO;
  {$EXTERNALSYM PCMSG_HASHED_ENCODE_INFO}
  PCMSG_HASHED_ENCODE_INFO = ^_CMSG_HASHED_ENCODE_INFO;
  TCMsgHashedEncodeInfo = _CMSG_HASHED_ENCODE_INFO;

//+-------------------------------------------------------------------------
//  CMSG_ENCRYPTED
//
//  The key used to encrypt the message is identified outside of the message
//  content (for example, password).
//
//  The content input to CryptMsgUpdate has already been encrypted.
//
//  pvEncryptionAuxInfo currently isn't used and must be set to NULL.
//--------------------------------------------------------------------------
  PCMsgEncryptedEncodeInfo = ^TCMsgEncryptedEncodeInfo;
  {$EXTERNALSYM _CMSG_ENCRYPTED_ENCODE_INFO}
  _CMSG_ENCRYPTED_ENCODE_INFO = record
    cbSize: DWORD;
    ContentEncryptionAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    pvEncryptionAuxInfo: Pointer;
  end;
  {$EXTERNALSYM CMSG_ENCRYPTED_ENCODE_INFO}
  CMSG_ENCRYPTED_ENCODE_INFO = _CMSG_ENCRYPTED_ENCODE_INFO;
  {$EXTERNALSYM PCMSG_ENCRYPTED_ENCODE_INFO}
  PCMSG_ENCRYPTED_ENCODE_INFO = ^_CMSG_ENCRYPTED_ENCODE_INFO;
  TCMsgEncryptedEncodeInfo = _CMSG_ENCRYPTED_ENCODE_INFO;

//+-------------------------------------------------------------------------
//  This parameter allows messages to be of variable length with streamed
//  output.
//
//  By default, messages are of a definite length and
//  CryptMsgGetParam(CMSG_CONTENT_PARAM) is
//  called to get the cryptographically processed content. Until closed,
//  the handle keeps a copy of the processed content.
//
//  With streamed output, the processed content can be freed as its streamed.
//
//  If the length of the content to be updated is known at the time of the
//  open, then, ContentLength should be set to that length. Otherwise, it
//  should be set to CMSG_INDEFINITE_LENGTH.
//--------------------------------------------------------------------------
  {$EXTERNALSYM PFN_CMSG_STREAM_OUTPUT}
  PFN_CMSG_STREAM_OUTPUT = function(const pvArg: Pointer; pbData: PBYTE;
    cbData: DWORD; fFinal: BOOL): BOOL stdcall;
  TFnCMSGStreamOutput = PFN_CMSG_STREAM_OUTPUT;

const
  {$EXTERNALSYM CMSG_INDEFINITE_LENGTH}
  CMSG_INDEFINITE_LENGTH       = $FFFFFFFF;

type
  PCMsgStreamInfo = ^TCMsgStreamInfo;
  {$EXTERNALSYM _CMSG_STREAM_INFO}
  _CMSG_STREAM_INFO = record
    cbContent: DWORD;
    pfnStreamOutput: PFN_CMSG_STREAM_OUTPUT;
    pvArg: Pointer;
  end;
  {$EXTERNALSYM CMSG_STREAM_INFO}
  CMSG_STREAM_INFO = _CMSG_STREAM_INFO;
  {$EXTERNALSYM PCMSG_STREAM_INFO}
  PCMSG_STREAM_INFO = ^_CMSG_STREAM_INFO;
  TCMsgStreamInfo = _CMSG_STREAM_INFO;

//+-------------------------------------------------------------------------
//  Open dwFlags
//--------------------------------------------------------------------------
const
  {$EXTERNALSYM CMSG_BARE_CONTENT_FLAG}
  CMSG_BARE_CONTENT_FLAG = $00000001;
  {$EXTERNALSYM CMSG_LENGTH_ONLY_FLAG}
  CMSG_LENGTH_ONLY_FLAG = $00000002;
  {$EXTERNALSYM CMSG_DETACHED_FLAG}
  CMSG_DETACHED_FLAG = $00000004;
  {$EXTERNALSYM CMSG_AUTHENTICATED_ATTRIBUTES_FLAG}
  CMSG_AUTHENTICATED_ATTRIBUTES_FLAG = $00000008;
  {$EXTERNALSYM CMSG_CONTENTS_OCTETS_FLAG}
  CMSG_CONTENTS_OCTETS_FLAG = $00000010;
  {$EXTERNALSYM CMSG_MAX_LENGTH_FLAG}
  CMSG_MAX_LENGTH_FLAG = $00000020;

// When set, nonData type inner content is encapsulated within an
// OCTET STRING. Applicable to both Signed and Enveloped messages.
  {$EXTERNALSYM CMSG_CMS_ENCAPSULATED_CONTENT_FLAG}
  CMSG_CMS_ENCAPSULATED_CONTENT_FLAG = $00000040;

// If set, then, the hCryptProv passed to CryptMsgOpenToEncode or
// CryptMsgOpenToDecode is released on the final CryptMsgClose.
// Not released if CryptMsgOpenToEncode or CryptMsgOpenToDecode fails.
//
// Also applies to hNCryptKey where applicable.
//
// Note, the envelope recipient hCryptProv's aren't released.
  {$EXTERNALSYM CMSG_CRYPT_RELEASE_CONTEXT_FLAG}
  CMSG_CRYPT_RELEASE_CONTEXT_FLAG = $00008000;

//+-------------------------------------------------------------------------
//  Open a cryptographic message for encoding
//
//  If CMSG_BARE_CONTENT_FLAG is specified for a streamed message,
//  the streamed output will not have an outer ContentInfo wrapper. This
//  makes it suitable to be streamed into an enclosing message.
//
//  The pStreamInfo parameter needs to be set to stream the encoded message
//  output.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptMsgOpenToEncode}
function CryptMsgOpenToEncode(dwMsgEncodingType, dwFlags, dwMsgType: DWORD;
  pvMsgEncodeInfo: Pointer; pszInnerContentObjID: LPSTR;
  pStreamInfo: PCMSG_STREAM_INFO): HCRYPTMSG; stdcall;

//+-------------------------------------------------------------------------
//  Calculate the length of an encoded cryptographic message.
//
//  Calculates the length of the encoded message given the
//  message type, encoding parameters and total length of
//  the data to be updated. Note, this might not be the exact length. However,
//  it will always be greater than or equal to the actual length.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptMsgCalculateEncodedLength}
function CryptMsgCalculateEncodedLength(dwMsgEncodingType, dwFlags,
  dwMsgType: DWORD; pvMsgEncodeInfo: Pointer; pszInnerContentObjID: LPSTR;
  cbData: DWORD): DWORD; stdcall;

//+-------------------------------------------------------------------------
//  Open a cryptographic message for decoding
//
//  hCryptProv specifies the crypto provider to use for hashing and/or
//  decrypting the message. If hCryptProv is NULL, a default crypt provider
//  is used.
//
//  Currently pRecipientInfo isn't used and should be set to NULL.
//
//  The pStreamInfo parameter needs to be set to stream the decoded content
//  output.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptMsgOpenToDecode}
function CryptMsgOpenToDecode(dwMsgEncodingType, dwFlags, dwMsgType: DWORD;
  hCryptProv: HCRYPTPROV_LEGACY; pRecipientInfo: PCERT_INFO;
  pStreamInfo: PCMSG_STREAM_INFO): HCRYPTMSG; stdcall;

//+-------------------------------------------------------------------------
//  Duplicate a cryptographic message handle
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptMsgDuplicate}
function CryptMsgDuplicate(hCryptMsg: HCRYPTMSG): HCRYPTMSG; stdcall;

//+-------------------------------------------------------------------------
//  Close a cryptographic message handle
//
//  LastError is preserved unless FALSE is returned.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptMsgClose}
function CryptMsgClose(hCryptMsg: HCRYPTMSG): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Update the content of a cryptographic message. Depending on how the
//  message was opened, the content is either encoded or decoded.
//
//  This function is repetitively called to append to the message content.
//  fFinal is set to identify the last update. On fFinal, the encode/decode
//  is completed. The encoded/decoded content and the decoded parameters
//  are valid until the open and all duplicated handles are closed.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptMsgUpdate}
function CryptMsgUpdate(hCryptMsg: HCRYPTMSG; pbData: PBYTE; cbData: DWORD;
  fFinal: BOOL): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Get a parameter after encoding/decoding a cryptographic message. Called
//  after the final CryptMsgUpdate. Only the CMSG_CONTENT_PARAM and
//  CMSG_COMPUTED_HASH_PARAM are valid for an encoded message.
//
//  For an encoded HASHED message, the CMSG_COMPUTED_HASH_PARAM can be got
//  before any CryptMsgUpdates to get its length.
//
//  The pvData type definition depends on the dwParamType value.
//
//  Elements pointed to by fields in the pvData structure follow the
//  structure. Therefore, *pcbData may exceed the size of the structure.
//
//  Upon input, if *pcbData == 0, then, *pcbData is updated with the length
//  of the data and the pvData parameter is ignored.
//
//  Upon return, *pcbData is updated with the length of the data.
//
//  The OBJID BLOBs returned in the pvData structures point to
//  their still encoded representation. The appropriate functions
//  must be called to decode the information.
//
//  See below for a list of the parameters to get.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptMsgGetParam}
function CryptMsgGetParam(hCryptMsg: HCRYPTMSG; dwParamType, dwIndex: DWORD;
  pvData: Pointer; out pcbData: DWORD): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Get parameter types and their corresponding data structure definitions.
//--------------------------------------------------------------------------
const
  {$EXTERNALSYM CMSG_TYPE_PARAM}
  CMSG_TYPE_PARAM = 1;
  {$EXTERNALSYM CMSG_CONTENT_PARAM}
  CMSG_CONTENT_PARAM = 2;
  {$EXTERNALSYM CMSG_BARE_CONTENT_PARAM}
  CMSG_BARE_CONTENT_PARAM = 3;
  {$EXTERNALSYM CMSG_INNER_CONTENT_TYPE_PARAM}
  CMSG_INNER_CONTENT_TYPE_PARAM = 4;
  {$EXTERNALSYM CMSG_SIGNER_COUNT_PARAM}
  CMSG_SIGNER_COUNT_PARAM = 5;
  {$EXTERNALSYM CMSG_SIGNER_INFO_PARAM}
  CMSG_SIGNER_INFO_PARAM = 6;
  {$EXTERNALSYM CMSG_SIGNER_CERT_INFO_PARAM}
  CMSG_SIGNER_CERT_INFO_PARAM = 7;
  {$EXTERNALSYM CMSG_SIGNER_HASH_ALGORITHM_PARAM}
  CMSG_SIGNER_HASH_ALGORITHM_PARAM = 8;
  {$EXTERNALSYM CMSG_SIGNER_AUTH_ATTR_PARAM}
  CMSG_SIGNER_AUTH_ATTR_PARAM = 9;
  {$EXTERNALSYM CMSG_SIGNER_UNAUTH_ATTR_PARAM}
  CMSG_SIGNER_UNAUTH_ATTR_PARAM = 10;
  {$EXTERNALSYM CMSG_CERT_COUNT_PARAM}
  CMSG_CERT_COUNT_PARAM = 11;
  {$EXTERNALSYM CMSG_CERT_PARAM}
  CMSG_CERT_PARAM = 12;
  {$EXTERNALSYM CMSG_CRL_COUNT_PARAM}
  CMSG_CRL_COUNT_PARAM = 13;
  {$EXTERNALSYM CMSG_CRL_PARAM}
  CMSG_CRL_PARAM = 14;
  {$EXTERNALSYM CMSG_ENVELOPE_ALGORITHM_PARAM}
  CMSG_ENVELOPE_ALGORITHM_PARAM = 15;
  {$EXTERNALSYM CMSG_RECIPIENT_COUNT_PARAM}
  CMSG_RECIPIENT_COUNT_PARAM = 17;
  {$EXTERNALSYM CMSG_RECIPIENT_INDEX_PARAM}
  CMSG_RECIPIENT_INDEX_PARAM = 18;
  {$EXTERNALSYM CMSG_RECIPIENT_INFO_PARAM}
  CMSG_RECIPIENT_INFO_PARAM = 19;
  {$EXTERNALSYM CMSG_HASH_ALGORITHM_PARAM}
  CMSG_HASH_ALGORITHM_PARAM = 20;
  {$EXTERNALSYM CMSG_HASH_DATA_PARAM}
  CMSG_HASH_DATA_PARAM = 21;
  {$EXTERNALSYM CMSG_COMPUTED_HASH_PARAM}
  CMSG_COMPUTED_HASH_PARAM = 22;
  {$EXTERNALSYM CMSG_ENCRYPT_PARAM}
  CMSG_ENCRYPT_PARAM = 26;
  {$EXTERNALSYM CMSG_ENCRYPTED_DIGEST}
  CMSG_ENCRYPTED_DIGEST = 27;
  {$EXTERNALSYM CMSG_ENCODED_SIGNER}
  CMSG_ENCODED_SIGNER = 28;
  {$EXTERNALSYM CMSG_ENCODED_MESSAGE}
  CMSG_ENCODED_MESSAGE = 29;
  {$EXTERNALSYM CMSG_VERSION_PARAM}
  CMSG_VERSION_PARAM = 30;
  {$EXTERNALSYM CMSG_ATTR_CERT_COUNT_PARAM}
  CMSG_ATTR_CERT_COUNT_PARAM = 31;
  {$EXTERNALSYM CMSG_ATTR_CERT_PARAM}
  CMSG_ATTR_CERT_PARAM = 32;
  {$EXTERNALSYM CMSG_CMS_RECIPIENT_COUNT_PARAM}
  CMSG_CMS_RECIPIENT_COUNT_PARAM = 33;
  {$EXTERNALSYM CMSG_CMS_RECIPIENT_INDEX_PARAM}
  CMSG_CMS_RECIPIENT_INDEX_PARAM = 34;
  {$EXTERNALSYM CMSG_CMS_RECIPIENT_ENCRYPTED_KEY_INDEX_PARAM}
  CMSG_CMS_RECIPIENT_ENCRYPTED_KEY_INDEX_PARAM = 35;
  {$EXTERNALSYM CMSG_CMS_RECIPIENT_INFO_PARAM}
  CMSG_CMS_RECIPIENT_INFO_PARAM = 36;
  {$EXTERNALSYM CMSG_UNPROTECTED_ATTR_PARAM}
  CMSG_UNPROTECTED_ATTR_PARAM = 37;
  {$EXTERNALSYM CMSG_SIGNER_CERT_ID_PARAM}
  CMSG_SIGNER_CERT_ID_PARAM = 38;
  {$EXTERNALSYM CMSG_CMS_SIGNER_INFO_PARAM}
  CMSG_CMS_SIGNER_INFO_PARAM = 39;

//+-------------------------------------------------------------------------
//  CMSG_TYPE_PARAM
//
//  The type of the decoded message.
//
//  pvData points to a DWORD
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_CONTENT_PARAM
//
//  The encoded content of a cryptographic message. Depending on how the
//  message was opened, the content is either the whole PKCS#7
//  message (opened to encode) or the inner content (opened to decode).
//  In the decode case, the decrypted content is returned, if enveloped.
//  If not enveloped, and if the inner content is of type DATA, the returned
//  data is the contents octets of the inner content.
//
//  pvData points to the buffer receiving the content bytes
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_BARE_CONTENT_PARAM
//
//  The encoded content of an encoded cryptographic message, without the
//  outer layer of ContentInfo. That is, only the encoding of the
//  ContentInfo.content field is returned.
//
//  pvData points to the buffer receiving the content bytes
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_INNER_CONTENT_TYPE_PARAM
//
//  The type of the inner content of a decoded cryptographic message,
//  in the form of a NULL-terminated object identifier string
//  (eg. '1.2.840.113549.1.7.1').
//
//  pvData points to the buffer receiving the object identifier string
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_SIGNER_COUNT_PARAM
//
//  Count of signers in a SIGNED or SIGNED_AND_ENVELOPED message
//
//  pvData points to a DWORD
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_SIGNER_CERT_INFO_PARAM
//
//  To get all the signers, repetitively call CryptMsgGetParam, with
//  dwIndex set to 0 .. SignerCount - 1.
//
//  pvData points to a CERT_INFO struct.
//
//  Only the following fields have been updated in the CERT_INFO struct:
//  Issuer and SerialNumber.
//
//  Note, if the KEYID choice was selected for a CMS SignerId, then, the
//  SerialNumber is 0 and the Issuer is encoded containing a single RDN with a
//  single Attribute whose OID is szOID_KEYID_RDN, value type is
//  CERT_RDN_OCTET_STRING and value is the KEYID. When the
//  CertGetSubjectCertificateFromStore and
//  CertFindCertificateInStore(CERT_FIND_SUBJECT_CERT) APIs see this
//  special KEYID Issuer and SerialNumber, they do a KEYID match.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_SIGNER_INFO_PARAM
//
//  To get all the signers, repetitively call CryptMsgGetParam, with
//  dwIndex set to 0 .. SignerCount - 1.
//
//  pvData points to a CMSG_SIGNER_INFO struct.
//
//  Note, if the KEYID choice was selected for a CMS SignerId, then, the
//  SerialNumber is 0 and the Issuer is encoded containing a single RDN with a
//  single Attribute whose OID is szOID_KEYID_RDN, value type is
//  CERT_RDN_OCTET_STRING and value is the KEYID. When the
//  CertGetSubjectCertificateFromStore and
//  CertFindCertificateInStore(CERT_FIND_SUBJECT_CERT) APIs see this
//  special KEYID Issuer and SerialNumber, they do a KEYID match.
//--------------------------------------------------------------------------
type
  PCMsgSignerInfo = ^TCMsgSignerInfo;
  {$EXTERNALSYM _CMSG_SIGNER_INFO}
  _CMSG_SIGNER_INFO = record
    dwVersion: DWORD;
    Issuer: CERT_NAME_BLOB;
    SerialNumber: CRYPT_INTEGER_BLOB;
    HashAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;

    // This is also referred to as the SignatureAlgorithm
    HashEncryptionAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;

    EncryptedHash: CRYPT_DATA_BLOB;
    AuthAttrs: CRYPT_ATTRIBUTES;
    UnauthAttrs: CRYPT_ATTRIBUTES;
  end;
  {$EXTERNALSYM CMSG_SIGNER_INFO}
  CMSG_SIGNER_INFO = _CMSG_SIGNER_INFO;
  {$EXTERNALSYM PCMSG_SIGNER_INFO}
  PCMSG_SIGNER_INFO = ^_CMSG_SIGNER_INFO;
  TCMsgSignerInfo = _CMSG_SIGNER_INFO;

//+-------------------------------------------------------------------------
//  CMSG_SIGNER_CERT_ID_PARAM
//
//  To get all the signers, repetitively call CryptMsgGetParam, with
//  dwIndex set to 0 .. SignerCount - 1.
//
//  pvData points to a CERT_ID struct.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_CMS_SIGNER_INFO_PARAM
//
//  Same as CMSG_SIGNER_INFO_PARAM, except, contains SignerId instead of
//  Issuer and SerialNumber.
//
//  To get all the signers, repetitively call CryptMsgGetParam, with
//  dwIndex set to 0 .. SignerCount - 1.
//
//  pvData points to a CMSG_CMS_SIGNER_INFO struct.
//--------------------------------------------------------------------------
  PCMsgCMSSignerInfo = ^TCMsgCMSSignerInfo;
  {$EXTERNALSYM _CMSG_CMS_SIGNER_INFO}
  _CMSG_CMS_SIGNER_INFO = record
    dwVersion: DWORD;
    SignerId: CERT_ID;
    HashAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;

    // This is also referred to as the SignatureAlgorithm
    HashEncryptionAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;

    EncryptedHash: CRYPT_DATA_BLOB;
    AuthAttrs: CRYPT_ATTRIBUTES;
    UnauthAttrs: CRYPT_ATTRIBUTES;
  end;
  {$EXTERNALSYM CMSG_CMS_SIGNER_INFO}
  CMSG_CMS_SIGNER_INFO = _CMSG_CMS_SIGNER_INFO;
  {$EXTERNALSYM PCMSG_CMS_SIGNER_INFO}
  PCMSG_CMS_SIGNER_INFO = ^_CMSG_CMS_SIGNER_INFO;
  TCMsgCMSSignerInfo = _CMSG_CMS_SIGNER_INFO;

//+-------------------------------------------------------------------------
//  CMSG_SIGNER_HASH_ALGORITHM_PARAM
//
//  This parameter specifies the HashAlgorithm that was used for the signer.
//
//  Set dwIndex to iterate through all the signers.
//
//  pvData points to an CRYPT_ALGORITHM_IDENTIFIER struct.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_SIGNER_AUTH_ATTR_PARAM
//
//  The authenticated attributes for the signer.
//
//  Set dwIndex to iterate through all the signers.
//
//  pvData points to a CMSG_ATTR struct.
//--------------------------------------------------------------------------

  {$EXTERNALSYM CMSG_ATTR}
  CMSG_ATTR = CRYPT_ATTRIBUTES;
  {$EXTERNALSYM PCMSG_ATTR}
  PCMSG_ATTR = PCRYPT_ATTRIBUTES;

//+-------------------------------------------------------------------------
//  CMSG_SIGNER_UNAUTH_ATTR_PARAM
//
//  The unauthenticated attributes for the signer.
//
//  Set dwIndex to iterate through all the signers.
//
//  pvData points to a CMSG_ATTR struct.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_CERT_COUNT_PARAM
//
//  Count of certificates in a SIGNED or SIGNED_AND_ENVELOPED message.
//
//  CMS, also supports certificates in an ENVELOPED message.
//
//  pvData points to a DWORD
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_CERT_PARAM
//
//  To get all the certificates, repetitively call CryptMsgGetParam, with
//  dwIndex set to 0 .. CertCount - 1.
//
//  pvData points to an array of the certificate's encoded bytes.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_CRL_COUNT_PARAM
//
//  Count of CRLs in a SIGNED or SIGNED_AND_ENVELOPED message.
//
//  CMS, also supports CRLs in an ENVELOPED message.
//
//  pvData points to a DWORD
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_CRL_PARAM
//
//  To get all the CRLs, repetitively call CryptMsgGetParam, with
//  dwIndex set to 0 .. CrlCount - 1.
//
//  pvData points to an array of the CRL's encoded bytes.
//--------------------------------------------------------------------------


//+-------------------------------------------------------------------------
//  CMSG_ENVELOPE_ALGORITHM_PARAM
//
//  The ContentEncryptionAlgorithm that was used in
//  an ENVELOPED or SIGNED_AND_ENVELOPED message.
//
//  For streaming you must be able to successfully get this parameter before
//  doing a CryptMsgControl decrypt.
//
//  pvData points to an CRYPT_ALGORITHM_IDENTIFIER struct.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_RECIPIENT_COUNT_PARAM
//
//  Count of recipients in an ENVELOPED or SIGNED_AND_ENVELOPED message.
//
//  Count of key transport recepients.
//
//  The CMSG_CMS_RECIPIENT_COUNT_PARAM has the total count of
//  recipients (it also includes key agree and mail list recipients).
//
//  pvData points to a DWORD
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_RECIPIENT_INDEX_PARAM
//
//  Index of the recipient used to decrypt an ENVELOPED or SIGNED_AND_ENVELOPED
//  message.
//
//  Index of a key transport recipient. If a non key transport
//  recipient was used to decrypt, fails with LastError set to
//  CRYPT_E_INVALID_INDEX.
//
//  pvData points to a DWORD
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_RECIPIENT_INFO_PARAM
//
//  To get all the recipients, repetitively call CryptMsgGetParam, with
//  dwIndex set to 0 .. RecipientCount - 1.
//
//  Only returns the key transport recepients.
//
//  The CMSG_CMS_RECIPIENT_INFO_PARAM returns all recipients.
//
//  pvData points to a CERT_INFO struct.
//
//  Only the following fields have been updated in the CERT_INFO struct:
//  Issuer, SerialNumber and PublicKeyAlgorithm. The PublicKeyAlgorithm
//  specifies the KeyEncryptionAlgorithm that was used.
//
//  Note, if the KEYID choice was selected for a key transport recipient, then,
//  the SerialNumber is 0 and the Issuer is encoded containing a single RDN
//  with a single Attribute whose OID is szOID_KEYID_RDN, value type is
//  CERT_RDN_OCTET_STRING and value is the KEYID. When the
//  CertGetSubjectCertificateFromStore and
//  CertFindCertificateInStore(CERT_FIND_SUBJECT_CERT) APIs see this
//  special KEYID Issuer and SerialNumber, they do a KEYID match.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_HASH_ALGORITHM_PARAM
//
//  The HashAlgorithm in a HASHED message.
//
//  pvData points to an CRYPT_ALGORITHM_IDENTIFIER struct.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_HASH_DATA_PARAM
//
//  The hash in a HASHED message.
//
//  pvData points to an array of bytes.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_COMPUTED_HASH_PARAM
//
//  The computed hash for a HASHED message.
//  This may be called for either an encoded or decoded message.
//
//  Also, the computed hash for one of the signer's in a SIGNED message.
//  It may be called for either an encoded or decoded message after the
//  final update.  Set dwIndex to iterate through all the signers.
//
//  pvData points to an array of bytes.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_ENCRYPT_PARAM
//
//  The ContentEncryptionAlgorithm that was used in an ENCRYPTED message.
//
//  pvData points to an CRYPT_ALGORITHM_IDENTIFIER struct.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_ENCODED_MESSAGE
//
//  The full encoded message. This is useful in the case of a decoded
//  message which has been modified (eg. a signed-data or
//  signed-and-enveloped-data message which has been countersigned).
//
//  pvData points to an array of the message's encoded bytes.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_VERSION_PARAM
//
//  The version of the decoded message.
//
//  pvData points to a DWORD
//--------------------------------------------------------------------------
const
  {$EXTERNALSYM CMSG_SIGNED_DATA_V1}
  CMSG_SIGNED_DATA_V1                     = 1;
  {$EXTERNALSYM CMSG_SIGNED_DATA_V3}
  CMSG_SIGNED_DATA_V3                     = 3;
  {$EXTERNALSYM CMSG_SIGNED_DATA_PKCS_1_5_VERSION}
  CMSG_SIGNED_DATA_PKCS_1_5_VERSION       = CMSG_SIGNED_DATA_V1;
  {$EXTERNALSYM CMSG_SIGNED_DATA_CMS_VERSION}
  CMSG_SIGNED_DATA_CMS_VERSION            = CMSG_SIGNED_DATA_V3;

  {$EXTERNALSYM CMSG_SIGNER_INFO_V1}
  CMSG_SIGNER_INFO_V1                     = 1;
  {$EXTERNALSYM CMSG_SIGNER_INFO_V3}
  CMSG_SIGNER_INFO_V3                     = 3;
  {$EXTERNALSYM CMSG_SIGNER_INFO_PKCS_1_5_VERSION}
  CMSG_SIGNER_INFO_PKCS_1_5_VERSION       = CMSG_SIGNER_INFO_V1;
  {$EXTERNALSYM CMSG_SIGNER_INFO_CMS_VERSION}
  CMSG_SIGNER_INFO_CMS_VERSION            = CMSG_SIGNER_INFO_V3;

  {$EXTERNALSYM CMSG_HASHED_DATA_V0}
  CMSG_HASHED_DATA_V0                     = 0;
  {$EXTERNALSYM CMSG_HASHED_DATA_V2}
  CMSG_HASHED_DATA_V2                     = 2;
  {$EXTERNALSYM CMSG_HASHED_DATA_PKCS_1_5_VERSION}
  CMSG_HASHED_DATA_PKCS_1_5_VERSION       = CMSG_HASHED_DATA_V0;
  {$EXTERNALSYM CMSG_HASHED_DATA_CMS_VERSION}
  CMSG_HASHED_DATA_CMS_VERSION            = CMSG_HASHED_DATA_V2;

  {$EXTERNALSYM CMSG_ENVELOPED_DATA_V0}
  CMSG_ENVELOPED_DATA_V0                  = 0;
  {$EXTERNALSYM CMSG_ENVELOPED_DATA_V2}
  CMSG_ENVELOPED_DATA_V2                  = 2;
  {$EXTERNALSYM CMSG_ENVELOPED_DATA_PKCS_1_5_VERSION}
  CMSG_ENVELOPED_DATA_PKCS_1_5_VERSION    = CMSG_ENVELOPED_DATA_V0;
  {$EXTERNALSYM CMSG_ENVELOPED_DATA_CMS_VERSION}
  CMSG_ENVELOPED_DATA_CMS_VERSION         = CMSG_ENVELOPED_DATA_V2;

//+-------------------------------------------------------------------------
//  CMSG_ATTR_CERT_COUNT_PARAM
//
//  Count of attribute certificates in a SIGNED or ENVELOPED message.
//
//  pvData points to a DWORD
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_ATTR_CERT_PARAM
//
//  To get all the attribute certificates, repetitively call CryptMsgGetParam,
//  with dwIndex set to 0 .. AttrCertCount - 1.
//
//  pvData points to an array of the attribute certificate's encoded bytes.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_CMS_RECIPIENT_COUNT_PARAM
//
//  Count of all CMS recipients in an ENVELOPED message.
//
//  pvData points to a DWORD
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_CMS_RECIPIENT_INDEX_PARAM
//
//  Index of the CMS recipient used to decrypt an ENVELOPED message.
//
//  pvData points to a DWORD
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_CMS_RECIPIENT_ENCRYPTED_KEY_INDEX_PARAM
//
//  For a CMS key agreement recipient, the index of the encrypted key
//  used to decrypt an ENVELOPED message.
//
//  pvData points to a DWORD
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_CMS_RECIPIENT_INFO_PARAM
//
//  To get all the CMS recipients, repetitively call CryptMsgGetParam, with
//  dwIndex set to 0 .. CmsRecipientCount - 1.
//
//  pvData points to a CMSG_CMS_RECIPIENT_INFO struct.
//--------------------------------------------------------------------------
type
  PCMsgKeyTransRecipientInfo = ^TCMsgKeyTransRecipientInfo;
  {$EXTERNALSYM _CMSG_KEY_TRANS_RECIPIENT_INFO}
  _CMSG_KEY_TRANS_RECIPIENT_INFO = record
    dwVersion: DWORD;

    // Currently, only ISSUER_SERIAL_NUMBER or KEYID choices
    RecipientId: CERT_ID;

    KeyEncryptionAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    EncryptedKey: CRYPT_DATA_BLOB;
  end;
  {$EXTERNALSYM CMSG_KEY_TRANS_RECIPIENT_INFO}
  CMSG_KEY_TRANS_RECIPIENT_INFO = _CMSG_KEY_TRANS_RECIPIENT_INFO;
  {$EXTERNALSYM PCMSG_KEY_TRANS_RECIPIENT_INFO}
  PCMSG_KEY_TRANS_RECIPIENT_INFO = ^_CMSG_KEY_TRANS_RECIPIENT_INFO;
  TCMsgKeyTransRecipientInfo = _CMSG_KEY_TRANS_RECIPIENT_INFO;

  PCMsgRecipientEncryptedKeyInfo = ^TCMsgRecipientEncryptedKeyInfo;
  {$EXTERNALSYM _CMSG_RECIPIENT_ENCRYPTED_KEY_INFO}
  _CMSG_RECIPIENT_ENCRYPTED_KEY_INFO = record
    // Currently, only ISSUER_SERIAL_NUMBER or KEYID choices
    RecipientId: CERT_ID;

    EncryptedKey: CRYPT_DATA_BLOB;

    // The following optional fields are only applicable to KEYID choice
    Date: FILETIME;
    pOtherAttr: PCRYPT_ATTRIBUTE_TYPE_VALUE;
  end;
  {$EXTERNALSYM CMSG_RECIPIENT_ENCRYPTED_KEY_INFO}
  CMSG_RECIPIENT_ENCRYPTED_KEY_INFO = _CMSG_RECIPIENT_ENCRYPTED_KEY_INFO;
  {$EXTERNALSYM PCMSG_RECIPIENT_ENCRYPTED_KEY_INFO}
  PCMSG_RECIPIENT_ENCRYPTED_KEY_INFO = ^_CMSG_RECIPIENT_ENCRYPTED_KEY_INFO;
  TCMsgRecipientEncryptedKeyInfo = _CMSG_RECIPIENT_ENCRYPTED_KEY_INFO;

const
  {$EXTERNALSYM CMSG_KEY_AGREE_ORIGINATOR_CERT}
  CMSG_KEY_AGREE_ORIGINATOR_CERT = 1;
  {$EXTERNALSYM CMSG_KEY_AGREE_ORIGINATOR_PUBLIC_KEY}
  CMSG_KEY_AGREE_ORIGINATOR_PUBLIC_KEY = 2;

type
  PCMsgKeyAgreeRecipientInfo = ^TCMsgKeyAgreeRecipientInfo;
  {$EXTERNALSYM _CMSG_KEY_AGREE_RECIPIENT_INFO}
  _CMSG_KEY_AGREE_RECIPIENT_INFO = record
    dwVersion: DWORD;
    case dwOriginatorChoice: DWORD of
      CMSG_KEY_AGREE_ORIGINATOR_CERT:
        (OriginatorCertId: CERT_ID);                    // size: 20
      CMSG_KEY_AGREE_ORIGINATOR_PUBLIC_KEY:
        (OriginatorPublicKeyInfo: CERT_PUBLIC_KEY_INFO; // size: 24
    UserKeyingMaterial: CRYPT_DATA_BLOB;
    KeyEncryptionAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;

    cRecipientEncryptedKeys: DWORD;
    rgpRecipientEncryptedKeys: ^PCMSG_RECIPIENT_ENCRYPTED_KEY_INFO;
    );
  end;
  {$EXTERNALSYM CMSG_KEY_AGREE_RECIPIENT_INFO}
  CMSG_KEY_AGREE_RECIPIENT_INFO = _CMSG_KEY_AGREE_RECIPIENT_INFO;
  {$EXTERNALSYM PCMSG_KEY_AGREE_RECIPIENT_INFO}
  PCMSG_KEY_AGREE_RECIPIENT_INFO = ^_CMSG_KEY_AGREE_RECIPIENT_INFO;
  TCMsgKeyAgreeRecipientInfo = _CMSG_KEY_AGREE_RECIPIENT_INFO;

  PCMsgMailIstRecipientInfo = ^TCMsgMailIstRecipientInfo;
  {$EXTERNALSYM _CMSG_MAIL_LIST_RECIPIENT_INFO}
  _CMSG_MAIL_LIST_RECIPIENT_INFO = record
    dwVersion: DWORD;
    KeyId: CRYPT_DATA_BLOB;
    KeyEncryptionAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    EncryptedKey: CRYPT_DATA_BLOB;

    // The following fields are optional
    Date: FILETIME;
    pOtherAttr: PCRYPT_ATTRIBUTE_TYPE_VALUE;
  end;
  {$EXTERNALSYM CMSG_MAIL_LIST_RECIPIENT_INFO}
  CMSG_MAIL_LIST_RECIPIENT_INFO = _CMSG_MAIL_LIST_RECIPIENT_INFO;
  {$EXTERNALSYM PCMSG_MAIL_LIST_RECIPIENT_INFO}
  PCMSG_MAIL_LIST_RECIPIENT_INFO = ^_CMSG_MAIL_LIST_RECIPIENT_INFO;
  TCMsgMailIstRecipientInfo = _CMSG_MAIL_LIST_RECIPIENT_INFO;

  PCMsgCMSRecipientInfo = ^TCMsgCMSRecipientInfo;
  {$EXTERNALSYM _CMSG_CMS_RECIPIENT_INFO}
  _CMSG_CMS_RECIPIENT_INFO = record
    case dwRecipientChoice: DWORD of
      CMSG_KEY_TRANS_RECIPIENT:
        (pKeyTrans: PCMSG_KEY_TRANS_RECIPIENT_INFO);
      CMSG_KEY_AGREE_RECIPIENT:
        (pKeyAgree: PCMSG_KEY_AGREE_RECIPIENT_INFO);
      CMSG_MAIL_LIST_RECIPIENT:
        (pMailList: PCMSG_MAIL_LIST_RECIPIENT_INFO);
  end;
  {$EXTERNALSYM CMSG_CMS_RECIPIENT_INFO}
  CMSG_CMS_RECIPIENT_INFO = _CMSG_CMS_RECIPIENT_INFO;
  {$EXTERNALSYM PCMSG_CMS_RECIPIENT_INFO}
  PCMSG_CMS_RECIPIENT_INFO = ^_CMSG_CMS_RECIPIENT_INFO;
  TCMsgCMSRecipientInfo = _CMSG_CMS_RECIPIENT_INFO;

const
// dwVersion numbers for the KeyTrans, KeyAgree and MailList recipients
  {$EXTERNALSYM CMSG_ENVELOPED_RECIPIENT_V0}
  CMSG_ENVELOPED_RECIPIENT_V0             = 0;
  {$EXTERNALSYM CMSG_ENVELOPED_RECIPIENT_V2}
  CMSG_ENVELOPED_RECIPIENT_V2             = 2;
  {$EXTERNALSYM CMSG_ENVELOPED_RECIPIENT_V3}
  CMSG_ENVELOPED_RECIPIENT_V3             = 3;
  {$EXTERNALSYM CMSG_ENVELOPED_RECIPIENT_V4}
  CMSG_ENVELOPED_RECIPIENT_V4             = 4;
  {$EXTERNALSYM CMSG_KEY_TRANS_PKCS_1_5_VERSION}
  CMSG_KEY_TRANS_PKCS_1_5_VERSION         = CMSG_ENVELOPED_RECIPIENT_V0;
  {$EXTERNALSYM CMSG_KEY_TRANS_CMS_VERSION}
  CMSG_KEY_TRANS_CMS_VERSION              = CMSG_ENVELOPED_RECIPIENT_V2;
  {$EXTERNALSYM CMSG_KEY_AGREE_VERSION}
  CMSG_KEY_AGREE_VERSION                  = CMSG_ENVELOPED_RECIPIENT_V3;
  {$EXTERNALSYM CMSG_MAIL_LIST_VERSION}
  CMSG_MAIL_LIST_VERSION                  = CMSG_ENVELOPED_RECIPIENT_V4;

//+-------------------------------------------------------------------------
//  CMSG_UNPROTECTED_ATTR_PARAM
//
//  The unprotected attributes in the envelped message.
//
//  pvData points to a CMSG_ATTR struct.
//--------------------------------------------------------------------------


//+-------------------------------------------------------------------------
//  Perform a special 'control' function after the final CryptMsgUpdate of a
//  encoded/decoded cryptographic message.
//
//  The dwCtrlType parameter specifies the type of operation to be performed.
//
//  The pvCtrlPara definition depends on the dwCtrlType value.
//
//  See below for a list of the control operations and their pvCtrlPara
//  type definition.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptMsgControl}
function CryptMsgControl(hCryptMsg: HCRYPTMSG; dwFlags, dwCtrlType: DWORD;
  pvCtrlPara: Pointer): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Message control types
//--------------------------------------------------------------------------
const
  {$EXTERNALSYM CMSG_CTRL_VERIFY_SIGNATURE}
  CMSG_CTRL_VERIFY_SIGNATURE = 1;
  {$EXTERNALSYM CMSG_CTRL_DECRYPT}
  CMSG_CTRL_DECRYPT = 2;
  {$EXTERNALSYM CMSG_CTRL_VERIFY_HASH}
  CMSG_CTRL_VERIFY_HASH = 5;
  {$EXTERNALSYM CMSG_CTRL_ADD_SIGNER}
  CMSG_CTRL_ADD_SIGNER = 6;
  {$EXTERNALSYM CMSG_CTRL_DEL_SIGNER}
  CMSG_CTRL_DEL_SIGNER = 7;
  {$EXTERNALSYM CMSG_CTRL_ADD_SIGNER_UNAUTH_ATTR}
  CMSG_CTRL_ADD_SIGNER_UNAUTH_ATTR = 8;
  {$EXTERNALSYM CMSG_CTRL_DEL_SIGNER_UNAUTH_ATTR}
  CMSG_CTRL_DEL_SIGNER_UNAUTH_ATTR = 9;
  {$EXTERNALSYM CMSG_CTRL_ADD_CERT}
  CMSG_CTRL_ADD_CERT = 10;
  {$EXTERNALSYM CMSG_CTRL_DEL_CERT}
  CMSG_CTRL_DEL_CERT = 11;
  {$EXTERNALSYM CMSG_CTRL_ADD_CRL}
  CMSG_CTRL_ADD_CRL = 12;
  {$EXTERNALSYM CMSG_CTRL_DEL_CRL}
  CMSG_CTRL_DEL_CRL = 13;
  {$EXTERNALSYM CMSG_CTRL_ADD_ATTR_CERT}
  CMSG_CTRL_ADD_ATTR_CERT = 14;
  {$EXTERNALSYM CMSG_CTRL_DEL_ATTR_CERT}
  CMSG_CTRL_DEL_ATTR_CERT = 15;
  {$EXTERNALSYM CMSG_CTRL_KEY_TRANS_DECRYPT}
  CMSG_CTRL_KEY_TRANS_DECRYPT = 16;
  {$EXTERNALSYM CMSG_CTRL_KEY_AGREE_DECRYPT}
  CMSG_CTRL_KEY_AGREE_DECRYPT = 17;
  {$EXTERNALSYM CMSG_CTRL_MAIL_LIST_DECRYPT}
  CMSG_CTRL_MAIL_LIST_DECRYPT = 18;
  {$EXTERNALSYM CMSG_CTRL_VERIFY_SIGNATURE_EX}
  CMSG_CTRL_VERIFY_SIGNATURE_EX = 19;
  {$EXTERNALSYM CMSG_CTRL_ADD_CMS_SIGNER_INFO}
  CMSG_CTRL_ADD_CMS_SIGNER_INFO = 20;

//+-------------------------------------------------------------------------
//  CMSG_CTRL_VERIFY_SIGNATURE
//
//  Verify the signature of a SIGNED or SIGNED_AND_ENVELOPED
//  message after it has been decoded.
//
//  For a SIGNED_AND_ENVELOPED message, called after
//  CryptMsgControl(CMSG_CTRL_DECRYPT), if CryptMsgOpenToDecode was called
//  with a NULL pRecipientInfo.
//
//  pvCtrlPara points to a CERT_INFO struct.
//
//  The CERT_INFO contains the Issuer and SerialNumber identifying
//  the Signer of the message. The CERT_INFO also contains the
//  PublicKeyInfo
//  used to verify the signature. The cryptographic provider specified
//  in CryptMsgOpenToDecode is used.
//
//  Note, if the message contains CMS signers identified by KEYID, then,
//  the CERT_INFO's Issuer and SerialNumber is ignored and only the public
//  key is used to find a signer whose signature verifies.
//
//  The following CMSG_CTRL_VERIFY_SIGNATURE_EX should be used instead.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_CTRL_VERIFY_SIGNATURE_EX
//
//  Verify the signature of a SIGNED message after it has been decoded.
//
//  pvCtrlPara points to the following CMSG_CTRL_VERIFY_SIGNATURE_EX_PARA.
//
//  If hCryptProv is NULL, uses the cryptographic provider specified in
//  CryptMsgOpenToDecode. If CryptMsgOpenToDecode's hCryptProv is also NULL,
//  gets default provider according to the signer's public key OID.
//
//  dwSignerIndex is the index of the signer to use to verify the signature.
//
//  The signer can be a pointer to a CERT_PUBLIC_KEY_INFO, certificate
//  context or a chain context.
//
//  If the signer's HashEncryptionAlgorithm is szOID_PKIX_NO_SIGNATURE, then,
//  the signature is expected to contain the hash octets. Only dwSignerType
//  of CMSG_VERIFY_SIGNER_NULL may be specified to verify this no signature
//  case.
//--------------------------------------------------------------------------
type
  PCMsgCtrlVerifySignatureExPara = ^TCMsgCtrlVerifySignatureExPara;
  {$EXTERNALSYM _CMSG_CTRL_VERIFY_SIGNATURE_EX_PARA}
  _CMSG_CTRL_VERIFY_SIGNATURE_EX_PARA = record
    cbSize: DWORD;
    hCryptProv: HCRYPTPROV_LEGACY;
    dwSignerIndex: DWORD;
    dwSignerType: DWORD;
    pvSigner: Pointer;
  end;
  {$EXTERNALSYM CMSG_CTRL_VERIFY_SIGNATURE_EX_PARA}
  CMSG_CTRL_VERIFY_SIGNATURE_EX_PARA = _CMSG_CTRL_VERIFY_SIGNATURE_EX_PARA;
  {$EXTERNALSYM PCMSG_CTRL_VERIFY_SIGNATURE_EX_PARA}
  PCMSG_CTRL_VERIFY_SIGNATURE_EX_PARA = ^_CMSG_CTRL_VERIFY_SIGNATURE_EX_PARA;
  TCMsgCtrlVerifySignatureExPara = _CMSG_CTRL_VERIFY_SIGNATURE_EX_PARA;

// Signer Types
const
  {$EXTERNALSYM CMSG_VERIFY_SIGNER_PUBKEY}
  CMSG_VERIFY_SIGNER_PUBKEY = 1;
    // pvSigner :: PCERT_PUBLIC_KEY_INFO
  {$EXTERNALSYM CMSG_VERIFY_SIGNER_CERT}
  CMSG_VERIFY_SIGNER_CERT = 2;
    // pvSigner :: PCCERT_CONTEXT
  {$EXTERNALSYM CMSG_VERIFY_SIGNER_CHAIN}
  CMSG_VERIFY_SIGNER_CHAIN = 3;
    // pvSigner :: PCCERT_CHAIN_CONTEXT
  {$EXTERNALSYM CMSG_VERIFY_SIGNER_NULL}
  CMSG_VERIFY_SIGNER_NULL = 4;
    // pvSigner :: NULL


//+-------------------------------------------------------------------------
//  CMSG_CTRL_DECRYPT
//
//  Decrypt an ENVELOPED or SIGNED_AND_ENVELOPED message after it has been
//  decoded.
//
//  This decrypt is only applicable to key transport recipients.
//
//  hCryptProv and dwKeySpec specify the private key to use. For dwKeySpec ==
//  0, defaults to AT_KEYEXCHANGE.
//
//  hNCryptKey can be set to decrypt using a CNG private key.
//
//  If CMSG_CRYPT_RELEASE_CONTEXT_FLAG is set in the dwFlags passed
//  to CryptMsgControl, then, the hCryptProv is released on the final
//  CryptMsgClose. Not released if CryptMsgControl fails. Also applies
//  to freeing the hNCryptKey.
//
//  dwRecipientIndex is the index of the recipient in the message associated
//  with the hCryptProv's or hNCryptKey's private key.
//
//  The dwRecipientIndex is the index of a key transport recipient.
//
//  Note, the message can only be decrypted once.
//--------------------------------------------------------------------------
type
  PCMsgCtrlDecryptPara = ^TCMsgCtrlDecryptPara;
  {$EXTERNALSYM _CMSG_CTRL_DECRYPT_PARA}
  _CMSG_CTRL_DECRYPT_PARA = record
    cbSize: DWORD;

    // NCryptIsKeyHandle() is called to determine the union choice.
    case Byte of
      0: (hCryptProv: HCRYPTPROV);        // size: 4
      1: (hNCryptKey: NCRYPT_KEY_HANDLE;  // size: 4

    // not applicable for hNCryptKey choice
    dwKeySpec: DWORD;

    dwRecipientIndex: DWORD;
    );
  end;
  {$EXTERNALSYM CMSG_CTRL_DECRYPT_PARA}
  CMSG_CTRL_DECRYPT_PARA = _CMSG_CTRL_DECRYPT_PARA;
  {$EXTERNALSYM PCMSG_CTRL_DECRYPT_PARA}
  PCMSG_CTRL_DECRYPT_PARA = ^_CMSG_CTRL_DECRYPT_PARA;
  TCMsgCtrlDecryptPara = _CMSG_CTRL_DECRYPT_PARA;


//+-------------------------------------------------------------------------
//  CMSG_CTRL_KEY_TRANS_DECRYPT
//
//  Decrypt an ENVELOPED message after it has been decoded for a key
//  transport recipient.
//
//  hCryptProv and dwKeySpec specify the private key to use. For dwKeySpec ==
//  0, defaults to AT_KEYEXCHANGE.
//
//  hNCryptKey can be set to decrypt using a CNG private key.
//
//  If CMSG_CRYPT_RELEASE_CONTEXT_FLAG is set in the dwFlags passed
//  to CryptMsgControl, then, the hCryptProv is released on the final
//  CryptMsgClose. Not released if CryptMsgControl fails. Also applies
//  to freeing the hNCryptKey.
//
//  pKeyTrans points to the CMSG_KEY_TRANS_RECIPIENT_INFO obtained via
//  CryptMsgGetParam(CMSG_CMS_RECIPIENT_INFO_PARAM)
//
//  dwRecipientIndex is the index of the recipient in the message associated
//  with the hCryptProv's or hNCryptKey's private key.
//
//  Note, the message can only be decrypted once.
//--------------------------------------------------------------------------
type
  PCMsgCtrlKeyTransDecryptPara = ^TCMsgCtrlKeyTransDecryptPara;
  {$EXTERNALSYM _CMSG_CTRL_KEY_TRANS_DECRYPT_PARA}
  _CMSG_CTRL_KEY_TRANS_DECRYPT_PARA = record
    cbSize: DWORD;
    // NCryptIsKeyHandle() is called to determine the union choice.
    case Byte of
      0: (hCryptProv: HCRYPTPROV);
      1: (hNCryptKey: NCRYPT_KEY_HANDLE;

    // not applicable for hNCryptKey choice
    dwKeySpec: DWORD;

    pKeyTrans: PCMSG_KEY_TRANS_RECIPIENT_INFO;
    dwRecipientIndex: DWORD);
  end;
  {$EXTERNALSYM CMSG_CTRL_KEY_TRANS_DECRYPT_PARA}
  CMSG_CTRL_KEY_TRANS_DECRYPT_PARA = _CMSG_CTRL_KEY_TRANS_DECRYPT_PARA;
  {$EXTERNALSYM PCMSG_CTRL_KEY_TRANS_DECRYPT_PARA}
  PCMSG_CTRL_KEY_TRANS_DECRYPT_PARA = ^_CMSG_CTRL_KEY_TRANS_DECRYPT_PARA;
  TCMsgCtrlKeyTransDecryptPara = _CMSG_CTRL_KEY_TRANS_DECRYPT_PARA;

//+-------------------------------------------------------------------------
//  CMSG_CTRL_KEY_AGREE_DECRYPT
//
//  Decrypt an ENVELOPED message after it has been decoded for a key
//  agreement recipient.
//
//  hCryptProv and dwKeySpec specify the private key to use. For dwKeySpec ==
//  0, defaults to AT_KEYEXCHANGE.
//
//  hNCryptKey can be set to decrypt using a CNG private key.
//
//  If CMSG_CRYPT_RELEASE_CONTEXT_FLAG is set in the dwFlags passed
//  to CryptMsgControl, then, the hCryptProv is released on the final
//  CryptMsgClose. Not released if CryptMsgControl fails. Also applies
//  to freeing the hNCryptKey.
//
//  pKeyAgree points to the CMSG_KEY_AGREE_RECIPIENT_INFO obtained via
//  CryptMsgGetParam(CMSG_CMS_RECIPIENT_INFO_PARAM) for dwRecipientIndex.
//
//  dwRecipientIndex, dwRecipientEncryptedKeyIndex are the indices of the
//  recipient's encrypted key in the message associated with the hCryptProv's
//  or hNCryptKey's private key.
//
//  OriginatorPublicKey is the originator's public key obtained from either
//  the originator's certificate or the CMSG_KEY_AGREE_RECIPIENT_INFO obtained
//  via the CMSG_CMS_RECIPIENT_INFO_PARAM.
//
//  Note, the message can only be decrypted once.
//--------------------------------------------------------------------------
  PCMsgCtrlKeyAgreeDecryptPara = ^TCMsgCtrlKeyAgreeDecryptPara;
  {$EXTERNALSYM _CMSG_CTRL_KEY_AGREE_DECRYPT_PARA}
  _CMSG_CTRL_KEY_AGREE_DECRYPT_PARA = record
    cbSize: DWORD;

    // NCryptIsKeyHandle() is called to determine the union choice.
    case Byte of
      0: ( hCryptProv: HCRYPTPROV);
      1: (hNCryptKey: NCRYPT_KEY_HANDLE;

    // not applicable for hNCryptKey choice
    dwKeySpec: DWORD;

    pKeyAgree: PCMSG_KEY_AGREE_RECIPIENT_INFO;
    dwRecipientIndex: DWORD;
    dwRecipientEncryptedKeyIndex: DWORD;
    OriginatorPublicKey: CRYPT_BIT_BLOB);
  end;
  {$EXTERNALSYM CMSG_CTRL_KEY_AGREE_DECRYPT_PARA}
  CMSG_CTRL_KEY_AGREE_DECRYPT_PARA = _CMSG_CTRL_KEY_AGREE_DECRYPT_PARA;
  {$EXTERNALSYM PCMSG_CTRL_KEY_AGREE_DECRYPT_PARA}
  PCMSG_CTRL_KEY_AGREE_DECRYPT_PARA = ^_CMSG_CTRL_KEY_AGREE_DECRYPT_PARA;
  TCMsgCtrlKeyAgreeDecryptPara = _CMSG_CTRL_KEY_AGREE_DECRYPT_PARA;


//+-------------------------------------------------------------------------
//  CMSG_CTRL_MAIL_LIST_DECRYPT
//
//  Decrypt an ENVELOPED message after it has been decoded for a mail
//  list recipient.
//
//  pMailList points to the CMSG_MAIL_LIST_RECIPIENT_INFO obtained via
//  CryptMsgGetParam(CMSG_CMS_RECIPIENT_INFO_PARAM) for dwRecipientIndex.
//
//  There is 1 choice for the KeyEncryptionKey: an already created CSP key
//  handle. For the key handle choice, hCryptProv must be nonzero. This key
//  handle isn't destroyed.
//
//  If CMSG_CRYPT_RELEASE_CONTEXT_FLAG is set in the dwFlags passed
//  to CryptMsgControl, then, the hCryptProv is released on the final
//  CryptMsgClose. Not released if CryptMsgControl fails.
//
//  For RC2 wrap, the effective key length is obtained from the
//  KeyEncryptionAlgorithm parameters and set on the hKeyEncryptionKey before
//  decrypting.
//
//  Note, the message can only be decrypted once.
//
//  Mail list recipients aren't supported using CNG.
//--------------------------------------------------------------------------
  PCMsgCtrlMailListDecryptPara = ^TCMsgCtrlMailListDecryptPara;
  {$EXTERNALSYM _CMSG_CTRL_MAIL_LIST_DECRYPT_PARA}
  _CMSG_CTRL_MAIL_LIST_DECRYPT_PARA = record
    cbSize: DWORD;
    hCryptProv: HCRYPTPROV;
    pMailList: PCMSG_MAIL_LIST_RECIPIENT_INFO;
    dwRecipientIndex: DWORD;
    case dwKeyChoice: DWORD of
      CMSG_MAIL_LIST_HANDLE_KEY_CHOICE:
        (hKeyEncryptionKey: HCRYPTKEY);
      MaxInt:  // Reserve space for a potential pointer choice
        (pvKeyEncryptionKey: Pointer);
   end;
  {$EXTERNALSYM CMSG_CTRL_MAIL_LIST_DECRYPT_PARA}
  CMSG_CTRL_MAIL_LIST_DECRYPT_PARA = _CMSG_CTRL_MAIL_LIST_DECRYPT_PARA;
  {$EXTERNALSYM PCMSG_CTRL_MAIL_LIST_DECRYPT_PARA}
  PCMSG_CTRL_MAIL_LIST_DECRYPT_PARA = ^_CMSG_CTRL_MAIL_LIST_DECRYPT_PARA;
  TCMsgCtrlMailListDecryptPara = _CMSG_CTRL_MAIL_LIST_DECRYPT_PARA;

//+-------------------------------------------------------------------------
//  CMSG_CTRL_VERIFY_HASH
//
//  Verify the hash of a HASHED message after it has been decoded.
//
//  Only the hCryptMsg parameter is used, to specify the message whose
//  hash is being verified.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_CTRL_ADD_SIGNER
//
//  Add a signer to a signed-data message.
//
//  pvCtrlPara points to a CMSG_SIGNER_ENCODE_INFO.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_CTRL_ADD_CMS_SIGNER_INFO
//
//  Add a signer to a signed-data message.
//
//  Differs from the above, CMSG_CTRL_ADD_SIGNER, wherein, the signer info
//  already contains the signature.
//
//  pvCtrlPara points to a CMSG_CMS_SIGNER_INFO.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_CTRL_DEL_SIGNER
//
//  Remove a signer from a signed-data or signed-and-enveloped-data message.
//
//  pvCtrlPara points to a DWORD containing the 0-based index of the
//  signer to be removed.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_CTRL_ADD_SIGNER_UNAUTH_ATTR
//
//  Add an unauthenticated attribute to the SignerInfo of a signed-data or
//  signed-and-enveloped-data message.
//
//  The unauthenticated attribute is input in the form of an encoded blob.
//--------------------------------------------------------------------------

  PCmsgCtrlAddSignerUnauthAttrPara = ^TCmsgCtrlAddSignerUnauthAttrPara; 
  {$EXTERNALSYM _CMSG_CTRL_ADD_SIGNER_UNAUTH_ATTR_PARA} 
  _CMSG_CTRL_ADD_SIGNER_UNAUTH_ATTR_PARA = record 
    cbSize: DWORD; 
    dwSignerIndex: DWORD; 
    blob: CRYPT_DATA_BLOB; 
  end; 
  {$EXTERNALSYM CMSG_CTRL_ADD_SIGNER_UNAUTH_ATTR_PARA} 
  CMSG_CTRL_ADD_SIGNER_UNAUTH_ATTR_PARA = _CMSG_CTRL_ADD_SIGNER_UNAUTH_ATTR_PARA; 
  {$EXTERNALSYM PCMSG_CTRL_ADD_SIGNER_UNAUTH_ATTR_PARA} 
  PCMSG_CTRL_ADD_SIGNER_UNAUTH_ATTR_PARA = ^_CMSG_CTRL_ADD_SIGNER_UNAUTH_ATTR_PARA; 
  TCmsgCtrlAddSignerUnauthAttrPara = _CMSG_CTRL_ADD_SIGNER_UNAUTH_ATTR_PARA; 

//+-------------------------------------------------------------------------
//  CMSG_CTRL_DEL_SIGNER_UNAUTH_ATTR
//
//  Delete an unauthenticated attribute from the SignerInfo of a signed-data
//  or signed-and-enveloped-data message.
//
//  The unauthenticated attribute to be removed is specified by
//  a 0-based index.
//--------------------------------------------------------------------------

  PCmsgCtrlDelSignerUnauthAttrPara = ^TCmsgCtrlDelSignerUnauthAttrPara;
  {$EXTERNALSYM _CMSG_CTRL_DEL_SIGNER_UNAUTH_ATTR_PARA} 
  _CMSG_CTRL_DEL_SIGNER_UNAUTH_ATTR_PARA = record 
    cbSize: DWORD; 
    dwSignerIndex: DWORD; 
    dwUnauthAttrIndex: DWORD; 
  end; 
  {$EXTERNALSYM CMSG_CTRL_DEL_SIGNER_UNAUTH_ATTR_PARA} 
  CMSG_CTRL_DEL_SIGNER_UNAUTH_ATTR_PARA = _CMSG_CTRL_DEL_SIGNER_UNAUTH_ATTR_PARA; 
  {$EXTERNALSYM PCMSG_CTRL_DEL_SIGNER_UNAUTH_ATTR_PARA} 
  PCMSG_CTRL_DEL_SIGNER_UNAUTH_ATTR_PARA = ^_CMSG_CTRL_DEL_SIGNER_UNAUTH_ATTR_PARA; 
  TCmsgCtrlDelSignerUnauthAttrPara = _CMSG_CTRL_DEL_SIGNER_UNAUTH_ATTR_PARA; 

//+-------------------------------------------------------------------------
//  CMSG_CTRL_ADD_CERT
//
//  Add a certificate to a signed-data or signed-and-enveloped-data message.
//
//  pvCtrlPara points to a CRYPT_DATA_BLOB containing the certificate's
//  encoded bytes.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_CTRL_DEL_CERT
//
//  Delete a certificate from a signed-data or signed-and-enveloped-data
//  message.
//
//  pvCtrlPara points to a DWORD containing the 0-based index of the
//  certificate to be removed.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_CTRL_ADD_CRL
//
//  Add a CRL to a signed-data or signed-and-enveloped-data message.
//
//  pvCtrlPara points to a CRYPT_DATA_BLOB containing the CRL's
//  encoded bytes.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_CTRL_DEL_CRL
//
//  Delete a CRL from a signed-data or signed-and-enveloped-data message.
//
//  pvCtrlPara points to a DWORD containing the 0-based index of the CRL
//  to be removed.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_CTRL_ADD_ATTR_CERT
//
//  Add an attribute certificate to a signed-data message.
//
//  pvCtrlPara points to a CRYPT_DATA_BLOB containing the attribute
//  certificate's encoded bytes.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CMSG_CTRL_DEL_ATTR_CERT
//
//  Delete an attribute certificate from a signed-data message.
//
//  pvCtrlPara points to a DWORD containing the 0-based index of the
//  attribute certificate to be removed.
//--------------------------------------------------------------------------


//+-------------------------------------------------------------------------
//  Verify a countersignature, at the SignerInfo level.
//  ie. verify that pbSignerInfoCountersignature contains the encrypted
//  hash of the encryptedDigest field of pbSignerInfo.
//
//  hCryptProv is used to hash the encryptedDigest field of pbSignerInfo.
//  The only fields referenced from pciCountersigner are SerialNumber, Issuer,
//  and SubjectPublicKeyInfo.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptMsgVerifyCountersignatureEncoded}
function CryptMsgVerifyCountersignatureEncoded(hCryptProv: HCRYPTPROV_LEGACY;
  dwEncodingType: DWORD; pbSignerInfo: PBYTE; cbSignerInfo: DWORD;
  pbSignerInfoCountersignature: PBYTE; cbSignerInfoCountersignature: DWORD;
  pciCountersigner: PCERT_INFO): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Verify a countersignature, at the SignerInfo level.
//  ie. verify that pbSignerInfoCountersignature contains the encrypted
//  hash of the encryptedDigest field of pbSignerInfo.
//
//  hCryptProv is used to hash the encryptedDigest field of pbSignerInfo.
//
//  The signer can be a CERT_PUBLIC_KEY_INFO, certificate context or a
//  chain context.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptMsgVerifyCountersignatureEncodedEx}
function CryptMsgVerifyCountersignatureEncodedEx(hCryptProv: HCRYPTPROV_LEGACY;
  dwEncodingType: DWORD; pbSignerInfo: PBYTE; cbSignerInfo: DWORD;
  pbSignerInfoCountersignature: PBYTE; cbSignerInfoCountersignature,
  dwSignerType: DWORD; pvSigner: Pointer; dwFlags: DWORD;
  pvReserved: Pointer): BOOL; stdcall;

// See CMSG_CTRL_VERIFY_SIGNATURE_EX_PARA for dwSignerType definitions

//+-------------------------------------------------------------------------
//  Countersign an already-existing signature in a message
//
//  dwIndex is a zero-based index of the SignerInfo to be countersigned.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptMsgCountersign}
function CryptMsgCountersign(hCryptMsg: HCRYPTMSG;
  dwIndex, cCountersigners: DWORD;
  rgCountersigners: PCMSG_SIGNER_ENCODE_INFO): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Countersign an already-existing signature (encoded SignerInfo).
//  Output an encoded SignerInfo blob, suitable for use as a countersignature
//  attribute in the unauthenticated attributes of a signed-data or
//  signed-and-enveloped-data message.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptMsgCountersignEncoded}
function CryptMsgCountersignEncoded(dwEncodingType: DWORD; pbSignerInfo: PBYTE;
  cbSignerInfo, cCountersigners: DWORD;
  rgCountersigners: PCMSG_SIGNER_ENCODE_INFO; pbCountersignature: PBYTE;
  out pcbCountersignature: DWORD): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  CryptMsg OID installable functions
//--------------------------------------------------------------------------

type
  {$EXTERNALSYM PFN_CMSG_ALLOC}
  PFN_CMSG_ALLOC = function(cb: Cardinal): Pointer stdcall;
  TFnCMsgAlloc = PFN_CMSG_ALLOC;

  {$EXTERNALSYM PFN_CMSG_FREE}
  PFN_CMSG_FREE = procedure(pv: Pointer) stdcall;
  TFnCMsgFree = PFN_CMSG_FREE;

// Note, the following 3 installable functions are obsolete and have been
// replaced with GenContentEncryptKey, ExportKeyTrans, ExportKeyAgree,
// ExportMailList, ImportKeyTrans, ImportKeyAgree and ImportMailList
// installable functions.

// If *phCryptProv is NULL upon entry, then, if supported, the installable
// function should acquire a default provider and return. Note, its up
// to the installable function to release at process detach.
//
// If paiEncrypt->Parameters.cbData is 0, then, the callback may optionally
// return default encoded parameters in *ppbEncryptParameters and
// *pcbEncryptParameters. pfnAlloc must be called for the allocation.
const
  {$EXTERNALSYM CMSG_OID_GEN_ENCRYPT_KEY_FUNC}
  CMSG_OID_GEN_ENCRYPT_KEY_FUNC   = 'CryptMsgDllGenEncryptKey';

type                                                         
  {$EXTERNALSYM PFN_CMSG_GEN_ENCRYPT_KEY}
  PFN_CMSG_GEN_ENCRYPT_KEY = function(var phCryptProv: HCRYPTPROV;
    var paiEncrypt: CRYPT_ALGORITHM_IDENTIFIER; pvEncryptAuxInfo: Pointer;
    var pPublicKeyInfo: CERT_PUBLIC_KEY_INFO; pfnAlloc: PFN_CMSG_ALLOC;
    var phEncryptKey: HCRYPTKEY; ppbEncryptParameters: PPBYTE;
    var pcbEncryptParameters: DWORD): BOOL stdcall;
  TFnCMsgGenEncryptKey = PFN_CMSG_GEN_ENCRYPT_KEY;

const
  {$EXTERNALSYM CMSG_OID_EXPORT_ENCRYPT_KEY_FUNC}
  CMSG_OID_EXPORT_ENCRYPT_KEY_FUNC   = 'CryptMsgDllExportEncryptKey';

type
  {$EXTERNALSYM PFN_CMSG_EXPORT_ENCRYPT_KEY}
  PFN_CMSG_EXPORT_ENCRYPT_KEY = function(hCryptProv: HCRYPTPROV;
    hEncryptKey: HCRYPTKEY; var pPublicKeyInfo: CERT_PUBLIC_KEY_INFO;
    pbData: PBYTE; out pcbData: DWORD): BOOL stdcall;
  TFnCMsgExportEncryptKey = PFN_CMSG_EXPORT_ENCRYPT_KEY;

const
  {$EXTERNALSYM CMSG_OID_IMPORT_ENCRYPT_KEY_FUNC}
  CMSG_OID_IMPORT_ENCRYPT_KEY_FUNC   = 'CryptMsgDllImportEncryptKey';

type
  {$EXTERNALSYM PFN_CMSG_IMPORT_ENCRYPT_KEY}
  PFN_CMSG_IMPORT_ENCRYPT_KEY = function(hCryptProv: HCRYPTPROV;
    dwKeySpec: DWORD; var paiEncrypt: CRYPT_ALGORITHM_IDENTIFIER;
    var paiPubKey: CRYPT_ALGORITHM_IDENTIFIER; pbEncodedKey: PBYTE;
    cbEncodedKey: DWORD; var phEncryptKey: HCRYPTKEY): BOOL stdcall;
  TFnCMsgImportEncryptKey = PFN_CMSG_IMPORT_ENCRYPT_KEY;

// To get the default installable function for GenContentEncryptKey,
// ExportKeyTrans, ExportKeyAgree, ExportMailList, ImportKeyTrans,
// ImportKeyAgree or ImportMailList call CryptGetOIDFunctionAddress()
// with the pszOID argument set to the following constant. dwEncodingType
// should be set to CRYPT_ASN_ENCODING or X509_ASN_ENCODING.
const
  {$EXTERNALSYM CMSG_DEFAULT_INSTALLABLE_FUNC_OID}
  CMSG_DEFAULT_INSTALLABLE_FUNC_OID   = LPCSTR(1);

//+-------------------------------------------------------------------------
//  Content Encrypt Info
//
//  The following data structure contains the information shared between
//  the GenContentEncryptKey and the ExportKeyTrans, ExportKeyAgree and
//  ExportMailList installable functions.
//
//  For a ContentEncryptionAlgorithm.pszObjId having a 'Special' algid, only
//  supported via CNG, for example, AES, then, fCNG will be set.
//  fCNG will also be set to TRUE for any ECC agreement or OAEP RSA transport
//  recipients.
//
//  When, fCNG is TRUE, the hCNGContentEncryptKey choice is selected and
//  pbCNGContentEncryptKeyObject and pbContentEncryptKey will be pfnAlloc'ed.
//--------------------------------------------------------------------------
type
  PCmsgContentEncryptInfo = ^TCmsgContentEncryptInfo; 
  {$EXTERNALSYM _CMSG_CONTENT_ENCRYPT_INFO} 
  _CMSG_CONTENT_ENCRYPT_INFO = record 
    cbSize: DWORD; 
    hCryptProv: HCRYPTPROV_LEGACY; 
    ContentEncryptionAlgorithm: CRYPT_ALGORITHM_IDENTIFIER; 
    pvEncryptionAuxInfo: Pointer; 
    cRecipients: DWORD; 
    rgCmsRecipients: PCMSG_RECIPIENT_ENCODE_INFO;
    pfnAlloc: PFN_CMSG_ALLOC; 
    pfnFree: PFN_CMSG_FREE; 
    dwEncryptFlags: DWORD; 
    case Boolean of
      // fCNG == FALSE
      False: (hContentEncryptKey: HCRYPTKEY);           // size: 4
      // fCNG == TRUE
      True: (hCNGContentEncryptKey: BCRYPT_KEY_HANDLE;  // size: 4
    dwFlags: DWORD;
    fCNG: BOOL;

    // When fCNG == TRUE, pfnAlloc'ed
    pbCNGContentEncryptKeyObject: PByte;
    pbContentEncryptKey: PByte;
    cbContentEncryptKey: DWORD
    );
  end;
  {$EXTERNALSYM CMSG_CONTENT_ENCRYPT_INFO} 
  CMSG_CONTENT_ENCRYPT_INFO = _CMSG_CONTENT_ENCRYPT_INFO; 
  {$EXTERNALSYM PCMSG_CONTENT_ENCRYPT_INFO} 
  PCMSG_CONTENT_ENCRYPT_INFO = ^_CMSG_CONTENT_ENCRYPT_INFO; 
  TCmsgContentEncryptInfo = _CMSG_CONTENT_ENCRYPT_INFO; 

const
  {$EXTERNALSYM CMSG_CONTENT_ENCRYPT_PAD_ENCODED_LEN_FLAG}
  CMSG_CONTENT_ENCRYPT_PAD_ENCODED_LEN_FLAG = $00000001;

  {$EXTERNALSYM CMSG_CONTENT_ENCRYPT_FREE_PARA_FLAG}
  CMSG_CONTENT_ENCRYPT_FREE_PARA_FLAG = $00000001;
  {$EXTERNALSYM CMSG_CONTENT_ENCRYPT_FREE_OBJID_FLAG}
  CMSG_CONTENT_ENCRYPT_FREE_OBJID_FLAG = $00000002;
  {$EXTERNALSYM CMSG_CONTENT_ENCRYPT_RELEASE_CONTEXT_FLAG}
  CMSG_CONTENT_ENCRYPT_RELEASE_CONTEXT_FLAG = $00008000;

//+-------------------------------------------------------------------------
// Upon input, ContentEncryptInfo has been initialized from the
// EnvelopedEncodeInfo.
//
// Note, if rgpRecipients instead of rgCmsRecipients are set in the
// EnvelopedEncodeInfo, then, the rgpRecipients have been converted
// to rgCmsRecipients in the ContentEncryptInfo.
//
// For fCNG == FALSE, the following fields may be changed in ContentEncryptInfo:
//      hContentEncryptKey
//      hCryptProv
//      ContentEncryptionAlgorithm.pszObjId
//      ContentEncryptionAlgorithm.Parameters
//      dwFlags
//
// For fCNG == TRUE, the following fields may be changed in ContentEncryptInfo:
//      hCNGContentEncryptKey
//      pbCNGContentEncryptKeyObject
//      pbContentEncryptKey
//      cbContentEncryptKey
//      ContentEncryptionAlgorithm.pszObjId
//      ContentEncryptionAlgorithm.Parameters
//      dwFlags
//
// All other fields in the ContentEncryptInfo are READONLY.
//
// If CMSG_CONTENT_ENCRYPT_PAD_ENCODED_LEN_FLAG is set upon entry
// in dwEncryptFlags, then, any potentially variable length encoded
// output should be padded with zeroes to always obtain the
// same maximum encoded length. This is necessary for
// CryptMsgCalculateEncodedLength() or CryptMsgOpenToEncode() with
// definite length streaming.
//
// For fCNG == FALSE:
//      The hContentEncryptKey must be updated.
//
//      If hCryptProv is NULL upon input, then, it must be updated.
//      If a HCRYPTPROV is acquired that must be released, then, the
//      CMSG_CONTENT_ENCRYPT_RELEASE_CONTEXT_FLAG must be set in dwFlags.
// Otherwise, for fCNG == TRUE:
//      The hCNGContentEncryptKey and cbContentEncryptKey must be updated and
//      pbCNGContentEncryptKeyObject and pbContentEncryptKey pfnAlloc'ed.
//      This key will be freed and destroyed when hCryptMsg is closed.
//
// If ContentEncryptionAlgorithm.pszObjId is changed, then, the
// CMSG_CONTENT_ENCRYPT_FREE_OBJID_FLAG must be set in dwFlags.
// If ContentEncryptionAlgorithm.Parameters is updated, then, the
// CMSG_CONTENT_ENCRYPT_FREE_PARA_FLAG must be set in dwFlags. pfnAlloc and
// pfnFree must be used for doing the allocation.
//
// ContentEncryptionAlgorithm.pszObjId is used to get the OIDFunctionAddress.
//--------------------------------------------------------------------------

// The following CAPI1 installable function is called when fCNG == FALSE.
  {$EXTERNALSYM CMSG_OID_GEN_CONTENT_ENCRYPT_KEY_FUNC}
  CMSG_OID_GEN_CONTENT_ENCRYPT_KEY_FUNC       =
    'CryptMsgDllGenContentEncryptKey';
  {$EXTERNALSYM CMSG_OID_CAPI1_GEN_CONTENT_ENCRYPT_KEY_FUNC}
  CMSG_OID_CAPI1_GEN_CONTENT_ENCRYPT_KEY_FUNC =
    CMSG_OID_GEN_CONTENT_ENCRYPT_KEY_FUNC;

type
  {$EXTERNALSYM PFN_CMSG_GEN_CONTENT_ENCRYPT_KEY}
  PFN_CMSG_GEN_CONTENT_ENCRYPT_KEY = function(
    out pContentEncryptInfo: CMSG_CONTENT_ENCRYPT_INFO;
    dwFlags: DWORD; pvReserved: Pointer): BOOL stdcall;
  TFnCMsgGenContentEncryptKey = PFN_CMSG_GEN_CONTENT_ENCRYPT_KEY;

// The following installable function is called when fCNG == TRUE. It has the
// same API signature as for the above
// CMSG_OID_CAPI1_GEN_CONTENT_ENCRYPT_KEY_FUNC.
const
  {$EXTERNALSYM CMSG_OID_CNG_GEN_CONTENT_ENCRYPT_KEY_FUNC}
  CMSG_OID_CNG_GEN_CONTENT_ENCRYPT_KEY_FUNC     =
    'CryptMsgDllCNGGenContentEncryptKey';

//+-------------------------------------------------------------------------
//  Key Transport Encrypt Info
//
//  The following data structure contains the information updated by the
//  ExportKeyTrans installable function.
//--------------------------------------------------------------------------
type
  PCmsgKeyTransEncryptInfo = ^TCmsgKeyTransEncryptInfo; 
  {$EXTERNALSYM _CMSG_KEY_TRANS_ENCRYPT_INFO} 
  _CMSG_KEY_TRANS_ENCRYPT_INFO = record 
    cbSize: DWORD; 
    dwRecipientIndex: DWORD; 
    KeyEncryptionAlgorithm: CRYPT_ALGORITHM_IDENTIFIER; 
    EncryptedKey: CRYPT_DATA_BLOB; 
    dwFlags: DWORD; 
  end; 
  {$EXTERNALSYM CMSG_KEY_TRANS_ENCRYPT_INFO} 
  CMSG_KEY_TRANS_ENCRYPT_INFO = _CMSG_KEY_TRANS_ENCRYPT_INFO; 
  {$EXTERNALSYM PCMSG_KEY_TRANS_ENCRYPT_INFO} 
  PCMSG_KEY_TRANS_ENCRYPT_INFO = ^_CMSG_KEY_TRANS_ENCRYPT_INFO; 
  TCmsgKeyTransEncryptInfo = _CMSG_KEY_TRANS_ENCRYPT_INFO; 

const
  {$EXTERNALSYM CMSG_KEY_TRANS_ENCRYPT_FREE_PARA_FLAG}
  CMSG_KEY_TRANS_ENCRYPT_FREE_PARA_FLAG = $00000001;
  {$EXTERNALSYM CMSG_KEY_TRANS_ENCRYPT_FREE_OBJID_FLAG}
  CMSG_KEY_TRANS_ENCRYPT_FREE_OBJID_FLAG = $00000002;


//+-------------------------------------------------------------------------
// Upon input, KeyTransEncryptInfo has been initialized from the
// KeyTransEncodeInfo.
//
// The following fields may be changed in KeyTransEncryptInfo:
//      EncryptedKey
//      KeyEncryptionAlgorithm.pszObjId
//      KeyEncryptionAlgorithm.Parameters
//      dwFlags
//
// All other fields in the KeyTransEncryptInfo are READONLY.
//
// The EncryptedKey must be updated. The pfnAlloc and pfnFree specified in
// ContentEncryptInfo must be used for doing the allocation.
//
// If the KeyEncryptionAlgorithm.pszObjId is changed, then, the
// CMSG_KEY_TRANS_ENCRYPT_FREE_OBJID_FLAG  must be set in dwFlags.
// If the KeyEncryptionAlgorithm.Parameters is updated, then, the
// CMSG_KEY_TRANS_ENCRYPT_FREE_PARA_FLAG must be set in dwFlags.
// The pfnAlloc and pfnFree specified in ContentEncryptInfo must be used
// for doing the allocation.
//
// KeyEncryptionAlgorithm.pszObjId is used to get the OIDFunctionAddress.
//--------------------------------------------------------------------------

// The following CAPI1 installable function is called when
// pContentEncryptInfo->fCNG == FALSE.
  {$EXTERNALSYM CMSG_OID_EXPORT_KEY_TRANS_FUNC}
  CMSG_OID_EXPORT_KEY_TRANS_FUNC  = 'CryptMsgDllExportKeyTrans';
  {$EXTERNALSYM CMSG_OID_CAPI1_EXPORT_KEY_TRANS_FUNC}
  CMSG_OID_CAPI1_EXPORT_KEY_TRANS_FUNC = CMSG_OID_EXPORT_KEY_TRANS_FUNC;

type
  {$EXTERNALSYM PFN_CMSG_EXPORT_KEY_TRANS}
  PFN_CMSG_EXPORT_KEY_TRANS = function(
    var pContentEncryptInfo: CMSG_CONTENT_ENCRYPT_INFO;
    var pKeyTransEncodeInfo: CMSG_KEY_TRANS_RECIPIENT_ENCODE_INFO;
    out pKeyTransEncryptInfo: CMSG_KEY_TRANS_ENCRYPT_INFO;
    dwFlags: DWORD; pvReserved: Pointer): BOOL stdcall;
  TFnCMsgExportKeyTrans = PFN_CMSG_EXPORT_KEY_TRANS;

// The following CNG installable function is called when
// pContentEncryptInfo->fCNG == TRUE. It has the same API signature as for
// the above CMSG_OID_CAPI1_EXPORT_KEY_TRANS_FUNC.
const
  {$EXTERNALSYM CMSG_OID_CNG_EXPORT_KEY_TRANS_FUNC}
  CMSG_OID_CNG_EXPORT_KEY_TRANS_FUNC  = 'CryptMsgDllCNGExportKeyTrans';

//+-------------------------------------------------------------------------
//  Key Agree Key Encrypt Info
//
//  The following data structure contains the information updated by the
//  ExportKeyAgree installable function for each encrypted key agree
//  recipient.
//--------------------------------------------------------------------------
type
  PCmsgKeyAgreeKeyEncryptInfo = ^TCmsgKeyAgreeKeyEncryptInfo;
  {$EXTERNALSYM _CMSG_KEY_AGREE_KEY_ENCRYPT_INFO} 
  _CMSG_KEY_AGREE_KEY_ENCRYPT_INFO = record 
    cbSize: DWORD; 
    EncryptedKey: CRYPT_DATA_BLOB; 
  end; 
  {$EXTERNALSYM CMSG_KEY_AGREE_KEY_ENCRYPT_INFO} 
  CMSG_KEY_AGREE_KEY_ENCRYPT_INFO = _CMSG_KEY_AGREE_KEY_ENCRYPT_INFO; 
  {$EXTERNALSYM PCMSG_KEY_AGREE_KEY_ENCRYPT_INFO} 
  PCMSG_KEY_AGREE_KEY_ENCRYPT_INFO = ^_CMSG_KEY_AGREE_KEY_ENCRYPT_INFO; 
  TCmsgKeyAgreeKeyEncryptInfo = _CMSG_KEY_AGREE_KEY_ENCRYPT_INFO; 

//+-------------------------------------------------------------------------
//  Key Agree Encrypt Info
//
//  The following data structure contains the information applicable to
//  all recipients. Its updated by the ExportKeyAgree installable function.
//--------------------------------------------------------------------------
  PCmsgKeyAgreeEncryptInfo = ^TCmsgKeyAgreeEncryptInfo;
  {$EXTERNALSYM _CMSG_KEY_AGREE_ENCRYPT_INFO}
  _CMSG_KEY_AGREE_ENCRYPT_INFO = record
    cbSize: DWORD;
    dwRecipientIndex: DWORD;
    KeyEncryptionAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    UserKeyingMaterial: CRYPT_DATA_BLOB;
    dwOriginatorChoice: DWORD;
    case Byte of
      CMSG_KEY_AGREE_ORIGINATOR_CERT:
        (OriginatorCertId: CERT_ID); // size: 20
      CMSG_KEY_AGREE_ORIGINATOR_PUBLIC_KEY:
        (OriginatorPublicKeyInfo: CERT_PUBLIC_KEY_INFO; // size: 24
    cKeyAgreeKeyEncryptInfo: DWORD;
    rgpKeyAgreeKeyEncryptInfo: ^PCMSG_KEY_AGREE_KEY_ENCRYPT_INFO;
    dwFlags: DWORD
    );
  end;
  {$EXTERNALSYM CMSG_KEY_AGREE_ENCRYPT_INFO}
  CMSG_KEY_AGREE_ENCRYPT_INFO = _CMSG_KEY_AGREE_ENCRYPT_INFO;
  {$EXTERNALSYM PCMSG_KEY_AGREE_ENCRYPT_INFO}
  PCMSG_KEY_AGREE_ENCRYPT_INFO = ^_CMSG_KEY_AGREE_ENCRYPT_INFO;
  TCmsgKeyAgreeEncryptInfo = _CMSG_KEY_AGREE_ENCRYPT_INFO;

const
  {$EXTERNALSYM CMSG_KEY_AGREE_ENCRYPT_FREE_PARA_FLAG}
  CMSG_KEY_AGREE_ENCRYPT_FREE_PARA_FLAG = $00000001;
  {$EXTERNALSYM CMSG_KEY_AGREE_ENCRYPT_FREE_MATERIAL_FLAG}
  CMSG_KEY_AGREE_ENCRYPT_FREE_MATERIAL_FLAG = $00000002;
  {$EXTERNALSYM CMSG_KEY_AGREE_ENCRYPT_FREE_PUBKEY_ALG_FLAG}
  CMSG_KEY_AGREE_ENCRYPT_FREE_PUBKEY_ALG_FLAG = $00000004;
  {$EXTERNALSYM CMSG_KEY_AGREE_ENCRYPT_FREE_PUBKEY_PARA_FLAG}
  CMSG_KEY_AGREE_ENCRYPT_FREE_PUBKEY_PARA_FLAG = $00000008;
  {$EXTERNALSYM CMSG_KEY_AGREE_ENCRYPT_FREE_PUBKEY_BITS_FLAG}
  CMSG_KEY_AGREE_ENCRYPT_FREE_PUBKEY_BITS_FLAG = $00000010;
  {$EXTERNALSYM CMSG_KEY_AGREE_ENCRYPT_FREE_OBJID_FLAG}
  CMSG_KEY_AGREE_ENCRYPT_FREE_OBJID_FLAG = $00000020;

//+-------------------------------------------------------------------------
// Upon input, KeyAgreeEncryptInfo has been initialized from the
// KeyAgreeEncodeInfo.
//
// The following fields may be changed in KeyAgreeEncryptInfo:
//      KeyEncryptionAlgorithm.pszObjId
//      KeyEncryptionAlgorithm.Parameters
//      UserKeyingMaterial
//      dwOriginatorChoice
//      OriginatorCertId
//      OriginatorPublicKeyInfo
//      dwFlags
//
// All other fields in the KeyAgreeEncryptInfo are READONLY.
//
// If the KeyEncryptionAlgorithm.pszObjId is changed, then, the
// CMSG_KEY_AGREE_ENCRYPT_FREE_OBJID_FLAG  must be set in dwFlags.
// If the KeyEncryptionAlgorithm.Parameters is updated, then, the
// CMSG_KEY_AGREE_ENCRYPT_FREE_PARA_FLAG must be set in dwFlags.
// The pfnAlloc and pfnFree specified in ContentEncryptInfo must be used
// for doing the allocation.
//
// If the UserKeyingMaterial is updated, then, the
// CMSG_KEY_AGREE_ENCRYPT_FREE_MATERIAL_FLAG must be set in dwFlags.
// pfnAlloc and pfnFree must be used for doing the allocation.
//
// The dwOriginatorChoice must be updated to either
// CMSG_KEY_AGREE_ORIGINATOR_CERT or CMSG_KEY_AGREE_ORIGINATOR_PUBLIC_KEY.
//
// If the OriginatorPublicKeyInfo is updated, then, the appropriate
// CMSG_KEY_AGREE_ENCRYPT_FREE_PUBKEY_*_FLAG must be set in dwFlags and
// pfnAlloc and pfnFree must be used for doing the allocation.
//
// If CMSG_CONTENT_ENCRYPT_PAD_ENCODED_LEN_FLAG is set upon entry
// in pContentEncryptInfo->dwEncryptFlags, then, the OriginatorPublicKeyInfo's
// Ephemeral PublicKey should be padded with zeroes to always obtain the
// same maximum encoded length. Note, the length of the generated ephemeral Y
// public key can vary depending on the number of leading zero bits.
//
// Upon input, the array of *rgpKeyAgreeKeyEncryptInfo has been initialized.
// The EncryptedKey must be updated for each recipient key.
// The pfnAlloc and pfnFree specified in
// ContentEncryptInfo must be used for doing the allocation.
//
// KeyEncryptionAlgorithm.pszObjId is used to get the OIDFunctionAddress.
//--------------------------------------------------------------------------

// The following CAPI1 installable function is called when
// pContentEncryptInfo->fCNG == FALSE.
  {$EXTERNALSYM CMSG_OID_EXPORT_KEY_AGREE_FUNC}
  CMSG_OID_EXPORT_KEY_AGREE_FUNC  = 'CryptMsgDllExportKeyAgree';
  {$EXTERNALSYM CMSG_OID_CAPI1_EXPORT_KEY_AGREE_FUNC}
  CMSG_OID_CAPI1_EXPORT_KEY_AGREE_FUNC = CMSG_OID_EXPORT_KEY_AGREE_FUNC;

type
  {$EXTERNALSYM PFN_CMSG_EXPORT_KEY_AGREE}
  PFN_CMSG_EXPORT_KEY_AGREE = function(
    var pContentEncryptInfo: CMSG_CONTENT_ENCRYPT_INFO;
    var pKeyAgreeEncodeInfo: CMSG_KEY_AGREE_RECIPIENT_ENCODE_INFO;
    out pKeyAgreeEncryptInfo: CMSG_KEY_AGREE_ENCRYPT_INFO;
    dwFlags: DWORD; pvReserved: Pointer): BOOL stdcall;
  TFnCMsgExportKeyAgree = PFN_CMSG_EXPORT_KEY_AGREE;

// The following CNG installable function is called when
// pContentEncryptInfo->fCNG == TRUE. It has the same API signature as for
// the above CMSG_OID_CAPI1_EXPORT_KEY_AGREE_FUNC.
const
  {$EXTERNALSYM CMSG_OID_CNG_EXPORT_KEY_AGREE_FUNC}
  CMSG_OID_CNG_EXPORT_KEY_AGREE_FUNC  = 'CryptMsgDllCNGExportKeyAgree';

//+-------------------------------------------------------------------------
//  Mail List Encrypt Info
//
//  The following data structure contains the information updated by the
//  ExportMailList installable function.
//--------------------------------------------------------------------------
type
  PCmsgMailListEncryptInfo = ^TCmsgMailListEncryptInfo; 
  {$EXTERNALSYM _CMSG_MAIL_LIST_ENCRYPT_INFO} 
  _CMSG_MAIL_LIST_ENCRYPT_INFO = record 
    cbSize: DWORD; 
    dwRecipientIndex: DWORD; 
    KeyEncryptionAlgorithm: CRYPT_ALGORITHM_IDENTIFIER; 
    EncryptedKey: CRYPT_DATA_BLOB; 
    dwFlags: DWORD; 
  end; 
  {$EXTERNALSYM CMSG_MAIL_LIST_ENCRYPT_INFO} 
  CMSG_MAIL_LIST_ENCRYPT_INFO = _CMSG_MAIL_LIST_ENCRYPT_INFO; 
  {$EXTERNALSYM PCMSG_MAIL_LIST_ENCRYPT_INFO} 
  PCMSG_MAIL_LIST_ENCRYPT_INFO = ^_CMSG_MAIL_LIST_ENCRYPT_INFO; 
  TCmsgMailListEncryptInfo = _CMSG_MAIL_LIST_ENCRYPT_INFO; 

const
  {$EXTERNALSYM CMSG_MAIL_LIST_ENCRYPT_FREE_PARA_FLAG}
  CMSG_MAIL_LIST_ENCRYPT_FREE_PARA_FLAG = $00000001;
  {$EXTERNALSYM CMSG_MAIL_LIST_ENCRYPT_FREE_OBJID_FLAG}
  CMSG_MAIL_LIST_ENCRYPT_FREE_OBJID_FLAG = $00000002;


//+-------------------------------------------------------------------------
// Upon input, MailListEncryptInfo has been initialized from the
// MailListEncodeInfo.
//
// The following fields may be changed in MailListEncryptInfo:
//      EncryptedKey
//      KeyEncryptionAlgorithm.pszObjId
//      KeyEncryptionAlgorithm.Parameters
//      dwFlags
//
// All other fields in the MailListEncryptInfo are READONLY.
//
// The EncryptedKey must be updated. The pfnAlloc and pfnFree specified in
// ContentEncryptInfo must be used for doing the allocation.
//
// If the KeyEncryptionAlgorithm.pszObjId is changed, then, the
// CMSG_MAIL_LIST_ENCRYPT_FREE_OBJID_FLAG must be set in dwFlags.
// If the KeyEncryptionAlgorithm.Parameters is updated, then, the
// CMSG_MAIL_LIST_ENCRYPT_FREE_PARA_FLAG must be set in dwFlags.
// The pfnAlloc and pfnFree specified in ContentEncryptInfo must be used
// for doing the allocation.
//
// KeyEncryptionAlgorithm.pszObjId is used to get the OIDFunctionAddress.
//
// Note, only has a CAPI1 installable function. No CNG installable function.
//--------------------------------------------------------------------------
// The following CAPI1 installable function is called when
// pContentEncryptInfo->fCNG == FALSE.
  {$EXTERNALSYM CMSG_OID_EXPORT_MAIL_LIST_FUNC}
  CMSG_OID_EXPORT_MAIL_LIST_FUNC  = 'CryptMsgDllExportMailList';
  {$EXTERNALSYM CMSG_OID_CAPI1_EXPORT_MAIL_LIST_FUNC}
  CMSG_OID_CAPI1_EXPORT_MAIL_LIST_FUNC = CMSG_OID_EXPORT_MAIL_LIST_FUNC;

type
  {$EXTERNALSYM PFN_CMSG_EXPORT_MAIL_LIST}
  PFN_CMSG_EXPORT_MAIL_LIST = function(
    var pContentEncryptInfo: CMSG_CONTENT_ENCRYPT_INFO;
    var pMailListEncodeInfo: CMSG_MAIL_LIST_RECIPIENT_ENCODE_INFO;
    out pMailListEncryptInfo: CMSG_MAIL_LIST_ENCRYPT_INFO;
    dwFlags: DWORD; pvReserved: Pointer): BOOL stdcall;
  TFnCMsgMailList = PFN_CMSG_EXPORT_MAIL_LIST;

//+-------------------------------------------------------------------------
// CAPI1 OID Installable functions for importing an encoded and encrypted
// content encryption key.
//
// There's a different installable function for each CMS Recipient choice:
//  ImportKeyTrans
//  ImportKeyAgree
//  ImportMailList
//
// Iterates through the following OIDs to get the OID installable function:
//   KeyEncryptionOID!ContentEncryptionOID
//   KeyEncryptionOID
//   ContentEncryptionOID
//
// If the OID installable function doesn't support the specified
// KeyEncryption and ContentEncryption OIDs, then, return FALSE with
// LastError set to E_NOTIMPL.
//--------------------------------------------------------------------------
const
  {$EXTERNALSYM CMSG_OID_IMPORT_KEY_TRANS_FUNC}
  CMSG_OID_IMPORT_KEY_TRANS_FUNC   = 'CryptMsgDllImportKeyTrans';
  {$EXTERNALSYM CMSG_OID_CAPI1_IMPORT_KEY_TRANS_FUNC}
  CMSG_OID_CAPI1_IMPORT_KEY_TRANS_FUNC = CMSG_OID_IMPORT_KEY_TRANS_FUNC;

type
  {$EXTERNALSYM PFN_CMSG_IMPORT_KEY_TRANS}
  PFN_CMSG_IMPORT_KEY_TRANS = function(
    var pContentEncryptionAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    var pKeyTransDecryptPara: CMSG_CTRL_KEY_TRANS_DECRYPT_PARA;
    dwFlags: DWORD; pvReserved: Pointer;
    var phContentEncryptKey: HCRYPTKEY): BOOL stdcall;
  TFnCMsgImportKeyTrans = PFN_CMSG_IMPORT_KEY_TRANS;

const
  {$EXTERNALSYM CMSG_OID_IMPORT_KEY_AGREE_FUNC}
  CMSG_OID_IMPORT_KEY_AGREE_FUNC   = 'CryptMsgDllImportKeyAgree';
  {$EXTERNALSYM CMSG_OID_CAPI1_IMPORT_KEY_AGREE_FUNC}
  CMSG_OID_CAPI1_IMPORT_KEY_AGREE_FUNC = CMSG_OID_IMPORT_KEY_AGREE_FUNC;

type
  {$EXTERNALSYM PFN_CMSG_IMPORT_KEY_AGREE}
  PFN_CMSG_IMPORT_KEY_AGREE = function(
    var pContentEncryptionAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    var pKeyAgreeDecryptPara: CMSG_CTRL_KEY_AGREE_DECRYPT_PARA;
    dwFlags: DWORD; pvReserved: Pointer;
    var phContentEncryptKey: HCRYPTKEY): BOOL stdcall;
  TFnCMsgImportKeyAgree = PFN_CMSG_IMPORT_KEY_AGREE;

const
  {$EXTERNALSYM CMSG_OID_IMPORT_MAIL_LIST_FUNC}
  CMSG_OID_IMPORT_MAIL_LIST_FUNC   = 'CryptMsgDllImportMailList';
  {$EXTERNALSYM CMSG_OID_CAPI1_IMPORT_MAIL_LIST_FUNC}
  CMSG_OID_CAPI1_IMPORT_MAIL_LIST_FUNC  = CMSG_OID_IMPORT_MAIL_LIST_FUNC;

type
  {$EXTERNALSYM PFN_CMSG_IMPORT_MAIL_LIST}
  PFN_CMSG_IMPORT_MAIL_LIST = function(
    var pContentEncryptionAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    var pMailListDecryptPara: CMSG_CTRL_MAIL_LIST_DECRYPT_PARA;
    dwFlags: DWORD; pvReserved: Pointer;
    var phContentEncryptKey: HCRYPTKEY): BOOL stdcall;
  TFnCMsgImportMailList = PFN_CMSG_IMPORT_MAIL_LIST;

//+-------------------------------------------------------------------------
//  CNG Content Decrypt Info
//
//  The following data structure contains the information shared between
//  CNGImportKeyTrans, CNGImportKeyAgree and CNGImportContentEncryptKey
//  installable functions.
//
//  pbContentEncryptKey and pbCNGContentEncryptKeyObject are allocated
//  and freed via pfnAlloc and pfnFree.
//--------------------------------------------------------------------------
  PCmsgCngContentDecryptInfo = ^TCmsgCngContentDecryptInfo;
  {$EXTERNALSYM _CMSG_CNG_CONTENT_DECRYPT_INFO}
  _CMSG_CNG_CONTENT_DECRYPT_INFO = record
    cbSize: DWORD;
    ContentEncryptionAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    pfnAlloc: PFN_CMSG_ALLOC;
    pfnFree: PFN_CMSG_FREE;
    // This key must be used over the one in the DecryptPara. An
    // HCRYPTPROV in the DecryptPara may have been converted to a
    // NCRYPT_KEY_HANDLE.
    hNCryptKey: NCRYPT_KEY_HANDLE;
    pbContentEncryptKey: PByte;
    cbContentEncryptKey: DWORD;
    hCNGContentEncryptKey: BCRYPT_KEY_HANDLE;
    pbCNGContentEncryptKeyObject: PByte;
  end;
  {$EXTERNALSYM CMSG_CNG_CONTENT_DECRYPT_INFO}
  CMSG_CNG_CONTENT_DECRYPT_INFO = _CMSG_CNG_CONTENT_DECRYPT_INFO;
  {$EXTERNALSYM PCMSG_CNG_CONTENT_DECRYPT_INFO}
  PCMSG_CNG_CONTENT_DECRYPT_INFO = ^_CMSG_CNG_CONTENT_DECRYPT_INFO;
  TCmsgCngContentDecryptInfo = _CMSG_CNG_CONTENT_DECRYPT_INFO;

//+-------------------------------------------------------------------------
// CNG OID Installable function for importing and decrypting a key transport
// recipient encrypted content encryption key.
//
// Upon input, CNGContentDecryptInfo has been initialized.
//
// The following fields must be updated using hNCryptKey to decrypt
// pKeyTransDecryptPara->pKeyTrans->EncryptedKey.
//      pbContentEncryptKey (pfnAlloc'ed)
//      cbContentEncryptKey
//
// All other fields in the CNGContentEncryptInfo are READONLY.
//
// pKeyTransDecryptPara->pKeyTrans->KeyEncryptionAlgorithm.pszObjId is used
// to get the OIDFunctionAddress.
//--------------------------------------------------------------------------
const
  {$EXTERNALSYM CMSG_OID_CNG_IMPORT_KEY_TRANS_FUNC}
  CMSG_OID_CNG_IMPORT_KEY_TRANS_FUNC  = 'CryptMsgDllCNGImportKeyTrans';

type
  {$EXTERNALSYM PFN_CMSG_CNG_IMPORT_KEY_TRANS}
  PFN_CMSG_CNG_IMPORT_KEY_TRANS = function(
    out pCNGContentDecryptInfo: CMSG_CNG_CONTENT_DECRYPT_INFO;
    var pKeyTransDecryptPara: CMSG_CTRL_KEY_TRANS_DECRYPT_PARA;
    dwFlags: DWORD; pvReserved: Pointer): BOOL stdcall;
  TFnCMsgCNGImportKeyTrans = PFN_CMSG_CNG_IMPORT_KEY_TRANS;

//+-------------------------------------------------------------------------
// CNG OID Installable function for importing and decrypting a key agreement
// recipient encrypted content encryption key.
//
// Upon input, CNGContentDecryptInfo has been initialized.
//
// The following fields must be updated using hNCryptKey to decrypt
// pKeyAgreeDecryptPara->pKeyAgree->rgpRecipientEncryptedKeys[
//  pKeyAgreeDecryptPara->dwRecipientEncryptedKeyIndex]->EncryptedKey.
//      pbContentEncryptKey (pfnAlloc'ed)
//      cbContentEncryptKey
//
// All other fields in the CNGContentEncryptInfo are READONLY.
//
// pKeyAgreeDecryptPara->pKeyAgree->KeyEncryptionAlgorithm.pszObjId is used
// to get the OIDFunctionAddress.
//--------------------------------------------------------------------------
const
  {$EXTERNALSYM CMSG_OID_CNG_IMPORT_KEY_AGREE_FUNC}
  CMSG_OID_CNG_IMPORT_KEY_AGREE_FUNC   = 'CryptMsgDllCNGImportKeyAgree';

type
  {$EXTERNALSYM PFN_CMSG_CNG_IMPORT_KEY_AGREE}
  PFN_CMSG_CNG_IMPORT_KEY_AGREE = function(
    out pCNGContentDecryptInfo: CMSG_CNG_CONTENT_DECRYPT_INFO;
    var pKeyAgreeDecryptPara: CMSG_CTRL_KEY_AGREE_DECRYPT_PARA;
    dwFlags: DWORD; pvReserved: Pointer): BOOL stdcall;
  TFnCMsgCNGImportKeyAgree = PFN_CMSG_CNG_IMPORT_KEY_AGREE;

//+-------------------------------------------------------------------------
// CNG OID Installable function for importing an already decrypted
// content encryption key.
//
// Upon input, CNGContentDecryptInfo has been initialized.
//
// The following fields must be updated using pbContentEncryptKey and
// cbContentEncryptKey:
//      hCNGContentEncryptKey
//      pbCNGContentEncryptKeyObject (pfnAlloc'ed)
//
// The hCNGContentEncryptKey will be destroyed when hCryptMsg is closed.
//
// All other fields in the CNGContentEncryptInfo are READONLY.
//
// ContentEncryptionAlgorithm.pszObjId is used to get the OIDFunctionAddress.
//--------------------------------------------------------------------------
const
  {$EXTERNALSYM CMSG_OID_CNG_IMPORT_CONTENT_ENCRYPT_KEY_FUNC}
  CMSG_OID_CNG_IMPORT_CONTENT_ENCRYPT_KEY_FUNC  = 'CryptMsgDllCNGImportContentEncryptKey';

type
  {$EXTERNALSYM PFN_CMSG_CNG_IMPORT_CONTENT_ENCRYPT_KEY}
  PFN_CMSG_CNG_IMPORT_CONTENT_ENCRYPT_KEY = function(
    out pCNGContentDecryptInfo: CMSG_CNG_CONTENT_DECRYPT_INFO;
    dwFlags: DWORD; pvReserved: Pointer): BOOL stdcall;
  TFnCMsgCNGImportContentEncryptKey = PFN_CMSG_CNG_IMPORT_CONTENT_ENCRYPT_KEY;

//+=========================================================================
//  Certificate Store Data Structures and APIs
//==========================================================================

//+-------------------------------------------------------------------------
//              In its most basic implementation, a cert store is simply a
//              collection of certificates and/or CRLs. This is the case when
//              a cert store is opened with all of its certificates and CRLs
//              coming from a PKCS #7 encoded cryptographic message.
//
//              Nonetheless, all cert stores have the following properties:
//               - A public key may have more than one certificate in the store.
//                 For example, a private/public key used for signing may have a
//                 certificate issued for VISA and another issued for
//                 Mastercard. Also, when a certificate is renewed there might
//                 be more than one certificate with the same subject and
//                 issuer.
//               - However, each certificate in the store is uniquely
//                 identified by its Issuer and SerialNumber.
//               - There's an issuer of subject certificate relationship. A
//                 certificate's issuer is found by doing a match of
//                 pSubjectCert->Issuer with pIssuerCert->Subject.
//                 The relationship is verified by using
//                 the issuer's public key to verify the subject certificate's
//                 signature. Note, there might be X.509 v3 extensions
//                 to assist in finding the issuer certificate.
//               - Since issuer certificates might be renewed, a subject
//                 certificate might have more than one issuer certificate.
//               - There's an issuer of CRL relationship. An
//                 issuer's CRL is found by doing a match of
//                 pIssuerCert->Subject with pCrl->Issuer.
//                 The relationship is verified by using
//                 the issuer's public key to verify the CRL's
//                 signature. Note, there might be X.509 v3 extensions
//                 to assist in finding the CRL.
//               - Since some issuers might support the X.509 v3 delta CRL
//                 extensions, an issuer might have more than one CRL.
//               - The store shouldn't have any redundant certificates or
//                 CRLs. There shouldn't be two certificates with the same
//                 Issuer and SerialNumber. There shouldn't be two CRLs with
//                 the same Issuer, ThisUpdate and NextUpdate.
//               - The store has NO policy or trust information. No
//                 certificates are tagged as being 'root'. Its up to
//                 the application to maintain a list of CertIds (Issuer +
//                 SerialNumber) for certificates it trusts.
//               - The store might contain bad certificates and/or CRLs.
//                 The issuer's signature of a subject certificate or CRL may
//                 not verify. Certificates or CRLs may not satisfy their
//                 time validity requirements. Certificates may be
//                 revoked.
//
//              In addition to the certificates and CRLs, properties can be
//              stored. There are two predefined property IDs for a user
//              certificate: CERT_KEY_PROV_HANDLE_PROP_ID and
//              CERT_KEY_PROV_INFO_PROP_ID. The CERT_KEY_PROV_HANDLE_PROP_ID
//              is a HCRYPTPROV handle to the private key assoicated
//              with the certificate. The CERT_KEY_PROV_INFO_PROP_ID contains
//              information to be used to call
//              CryptAcquireContext and CryptSetProvParam to get a handle
//              to the private key associated with the certificate.
//
//              There exists two more predefined property IDs for certificates
//              and CRLs, CERT_SHA1_HASH_PROP_ID and CERT_MD5_HASH_PROP_ID.
//              If these properties don't already exist, then, a hash of the
//              content is computed. (CERT_HASH_PROP_ID maps to the default
//              hash algorithm, currently, CERT_SHA1_HASH_PROP_ID).
//
//              There are additional APIs for creating certificate and CRL
//      contexts not in a store (CertCreateCertificateContext and
//      CertCreateCRLContext).
//
//--------------------------------------------------------------------------
  {$EXTERNALSYM HCERTSTORE}
  HCERTSTORE = Pointer;
  {$NODEFINE PHCERTSTORE}
  PHCERTSTORE = ^HCERTSTORE;

//+-------------------------------------------------------------------------
//  Certificate context.
//
//  A certificate context contains both the encoded and decoded representation
//  of a certificate. A certificate context returned by a cert store function
//  must be freed by calling the CertFreeCertificateContext function. The
//  CertDuplicateCertificateContext function can be called to make a duplicate
//  copy (which also must be freed by calling CertFreeCertificateContext).
//--------------------------------------------------------------------------
// certenrolls_begin -- CERT_CONTEXT
  PCertContext = ^TCertContext;
  {$EXTERNALSYM _CERT_CONTEXT}
  _CERT_CONTEXT = record
    dwCertEncodingType: DWORD;
    pbCertEncoded: PByte;
    cbCertEncoded: DWORD;
    pCertInfo: PCERT_INFO;
    hCertStore: HCERTSTORE;
  end;
  {$EXTERNALSYM CERT_CONTEXT}
  CERT_CONTEXT = _CERT_CONTEXT;
  {$EXTERNALSYM PCERT_CONTEXT}
  PCERT_CONTEXT = ^_CERT_CONTEXT;
  TCertContext = _CERT_CONTEXT;
  {$EXTERNALSYM PCCERT_CONTEXT}
  PCCERT_CONTEXT = ^CERT_CONTEXT;
  {$NODEFINE PPCCERT_CONTEXT}
  PPCCERT_CONTEXT = ^PCCERT_CONTEXT;
// certenrolls_end

//+-------------------------------------------------------------------------
//  CRL context.
//
//  A CRL context contains both the encoded and decoded representation
//  of a CRL. A CRL context returned by a cert store function
//  must be freed by calling the CertFreeCRLContext function. The
//  CertDuplicateCRLContext function can be called to make a duplicate
//  copy (which also must be freed by calling CertFreeCRLContext).
//--------------------------------------------------------------------------
  PCrlContext = ^TCrlContext;
  {$EXTERNALSYM _CRL_CONTEXT}
  _CRL_CONTEXT = record
    dwCertEncodingType: DWORD;
    pbCrlEncoded: PByte;
    cbCrlEncoded: DWORD;
    pCrlInfo: PCRL_INFO;
    hCertStore: HCERTSTORE;
  end;
  {$EXTERNALSYM CRL_CONTEXT}
  CRL_CONTEXT = _CRL_CONTEXT;
  {$EXTERNALSYM PCRL_CONTEXT}
  PCRL_CONTEXT = ^_CRL_CONTEXT;
  TCrlContext = _CRL_CONTEXT;
  {$EXTERNALSYM PCCRL_CONTEXT}
  PCCRL_CONTEXT = ^CRL_CONTEXT; 

//+-------------------------------------------------------------------------
//  Certificate Trust List (CTL) context.
//
//  A CTL context contains both the encoded and decoded representation
//  of a CTL. Also contains an opened HCRYPTMSG handle to the decoded
//  cryptographic signed message containing the CTL_INFO as its inner content.
//  pbCtlContent is the encoded inner content of the signed message.
//
//  The CryptMsg APIs can be used to extract additional signer information.
//--------------------------------------------------------------------------
  PCtlContext = ^TCtlContext;
  {$EXTERNALSYM _CTL_CONTEXT}
  _CTL_CONTEXT = record
    dwMsgAndCertEncodingType: DWORD;
    pbCtlEncoded: PByte;
    cbCtlEncoded: DWORD;
    pCtlInfo: PCTL_INFO;
    hCertStore: HCERTSTORE;
    hCryptMsg: HCRYPTMSG;
    pbCtlContent: PByte;
    cbCtlContent: DWORD;
  end;
  {$EXTERNALSYM CTL_CONTEXT}
  CTL_CONTEXT = _CTL_CONTEXT;
  {$EXTERNALSYM PCTL_CONTEXT}
  PCTL_CONTEXT = ^_CTL_CONTEXT;
  TCtlContext = _CTL_CONTEXT;
  {$EXTERNALSYM PCCTL_CONTEXT}
  PCCTL_CONTEXT = ^CTL_CONTEXT; 

// certenrolld_begin -- CERT_*_PROP_ID
//+-------------------------------------------------------------------------
//  Certificate, CRL and CTL property IDs
//
//  See CertSetCertificateContextProperty or CertGetCertificateContextProperty
//  for usage information.
//--------------------------------------------------------------------------
const
  {$EXTERNALSYM CERT_KEY_PROV_HANDLE_PROP_ID}
  CERT_KEY_PROV_HANDLE_PROP_ID                  = 1;
  {$EXTERNALSYM CERT_KEY_PROV_INFO_PROP_ID}
  CERT_KEY_PROV_INFO_PROP_ID                    = 2;   // CRYPT_KEY_PROV_INFO
  {$EXTERNALSYM CERT_SHA1_HASH_PROP_ID}
  CERT_SHA1_HASH_PROP_ID                        = 3;
  {$EXTERNALSYM CERT_MD5_HASH_PROP_ID}
  CERT_MD5_HASH_PROP_ID                         = 4;
  {$EXTERNALSYM CERT_HASH_PROP_ID}
  CERT_HASH_PROP_ID                             = CERT_SHA1_HASH_PROP_ID;
  {$EXTERNALSYM CERT_KEY_CONTEXT_PROP_ID}
  CERT_KEY_CONTEXT_PROP_ID                      = 5;
  {$EXTERNALSYM CERT_KEY_SPEC_PROP_ID}
  CERT_KEY_SPEC_PROP_ID                         = 6;
  {$EXTERNALSYM CERT_IE30_RESERVED_PROP_ID}
  CERT_IE30_RESERVED_PROP_ID                    = 7;
  {$EXTERNALSYM CERT_PUBKEY_HASH_RESERVED_PROP_ID}
  CERT_PUBKEY_HASH_RESERVED_PROP_ID             = 8;
  {$EXTERNALSYM CERT_ENHKEY_USAGE_PROP_ID}
  CERT_ENHKEY_USAGE_PROP_ID                     = 9;
  {$EXTERNALSYM CERT_CTL_USAGE_PROP_ID}
  CERT_CTL_USAGE_PROP_ID                        = CERT_ENHKEY_USAGE_PROP_ID;
  {$EXTERNALSYM CERT_NEXT_UPDATE_LOCATION_PROP_ID}
  CERT_NEXT_UPDATE_LOCATION_PROP_ID             = 10;
  {$EXTERNALSYM CERT_FRIENDLY_NAME_PROP_ID}
  CERT_FRIENDLY_NAME_PROP_ID                    = 11;  // string
  {$EXTERNALSYM CERT_PVK_FILE_PROP_ID}
  CERT_PVK_FILE_PROP_ID                         = 12;
  {$EXTERNALSYM CERT_DESCRIPTION_PROP_ID}
  CERT_DESCRIPTION_PROP_ID                      = 13;  // string
  {$EXTERNALSYM CERT_ACCESS_STATE_PROP_ID}
  CERT_ACCESS_STATE_PROP_ID                     = 14;
  {$EXTERNALSYM CERT_SIGNATURE_HASH_PROP_ID}
  CERT_SIGNATURE_HASH_PROP_ID                   = 15;
  {$EXTERNALSYM CERT_SMART_CARD_DATA_PROP_ID}
  CERT_SMART_CARD_DATA_PROP_ID                  = 16;
  {$EXTERNALSYM CERT_EFS_PROP_ID}
  CERT_EFS_PROP_ID                              = 17;
  {$EXTERNALSYM CERT_FORTEZZA_DATA_PROP_ID}
  CERT_FORTEZZA_DATA_PROP_ID                    = 18;
  {$EXTERNALSYM CERT_ARCHIVED_PROP_ID}
  CERT_ARCHIVED_PROP_ID                         = 19;
  {$EXTERNALSYM CERT_KEY_IDENTIFIER_PROP_ID}
  CERT_KEY_IDENTIFIER_PROP_ID                   = 20;
  {$EXTERNALSYM CERT_AUTO_ENROLL_PROP_ID}
  CERT_AUTO_ENROLL_PROP_ID                      = 21;  // string:machine DNS name
  {$EXTERNALSYM CERT_PUBKEY_ALG_PARA_PROP_ID}
  CERT_PUBKEY_ALG_PARA_PROP_ID                  = 22;
  {$EXTERNALSYM CERT_CROSS_CERT_DIST_POINTS_PROP_ID}
  CERT_CROSS_CERT_DIST_POINTS_PROP_ID           = 23;
  {$EXTERNALSYM CERT_ISSUER_PUBLIC_KEY_MD5_HASH_PROP_ID}
  CERT_ISSUER_PUBLIC_KEY_MD5_HASH_PROP_ID       = 24;
  {$EXTERNALSYM CERT_SUBJECT_PUBLIC_KEY_MD5_HASH_PROP_ID}
  CERT_SUBJECT_PUBLIC_KEY_MD5_HASH_PROP_ID      = 25;
  {$EXTERNALSYM CERT_ENROLLMENT_PROP_ID}
  CERT_ENROLLMENT_PROP_ID                       = 26;  // RequestId+CADNS+CACN+Friendly Name
  {$EXTERNALSYM CERT_DATE_STAMP_PROP_ID}
  CERT_DATE_STAMP_PROP_ID                       = 27;
  {$EXTERNALSYM CERT_ISSUER_SERIAL_NUMBER_MD5_HASH_PROP_ID}
  CERT_ISSUER_SERIAL_NUMBER_MD5_HASH_PROP_ID    = 28;
  {$EXTERNALSYM CERT_SUBJECT_NAME_MD5_HASH_PROP_ID}
  CERT_SUBJECT_NAME_MD5_HASH_PROP_ID            = 29;
  {$EXTERNALSYM CERT_EXTENDED_ERROR_INFO_PROP_ID}
  CERT_EXTENDED_ERROR_INFO_PROP_ID              = 30;  // string

// Note, 32 - 35 are reserved for the CERT, CRL, CTL and KeyId file element IDs.
//       36 - 63 are reserved for future element IDs.

  {$EXTERNALSYM CERT_RENEWAL_PROP_ID}
  CERT_RENEWAL_PROP_ID                          = 64;
  {$EXTERNALSYM CERT_ARCHIVED_KEY_HASH_PROP_ID}
  CERT_ARCHIVED_KEY_HASH_PROP_ID                = 65;  // Encrypted key hash
  {$EXTERNALSYM CERT_AUTO_ENROLL_RETRY_PROP_ID}
  CERT_AUTO_ENROLL_RETRY_PROP_ID                = 66;  // AE_RETRY_INFO:cb+cRetry+FILETIME
  {$EXTERNALSYM CERT_AIA_URL_RETRIEVED_PROP_ID}
  CERT_AIA_URL_RETRIEVED_PROP_ID                = 67;
  {$EXTERNALSYM CERT_AUTHORITY_INFO_ACCESS_PROP_ID}
  CERT_AUTHORITY_INFO_ACCESS_PROP_ID            = 68;
  {$EXTERNALSYM CERT_BACKED_UP_PROP_ID}
  CERT_BACKED_UP_PROP_ID                        = 69;  // VARIANT_BOOL+FILETIME
  {$EXTERNALSYM CERT_OCSP_RESPONSE_PROP_ID}
  CERT_OCSP_RESPONSE_PROP_ID                    = 70;
  {$EXTERNALSYM CERT_REQUEST_ORIGINATOR_PROP_ID}
  CERT_REQUEST_ORIGINATOR_PROP_ID               = 71;  // string:machine DNS name
  {$EXTERNALSYM CERT_SOURCE_LOCATION_PROP_ID}
  CERT_SOURCE_LOCATION_PROP_ID                  = 72;  // string
  {$EXTERNALSYM CERT_SOURCE_URL_PROP_ID}
  CERT_SOURCE_URL_PROP_ID                       = 73;  // string
  {$EXTERNALSYM CERT_NEW_KEY_PROP_ID}
  CERT_NEW_KEY_PROP_ID                          = 74;
  {$EXTERNALSYM CERT_OCSP_CACHE_PREFIX_PROP_ID}
  CERT_OCSP_CACHE_PREFIX_PROP_ID                = 75;  // string
  {$EXTERNALSYM CERT_SMART_CARD_ROOT_INFO_PROP_ID}
  CERT_SMART_CARD_ROOT_INFO_PROP_ID             = 76;  // CRYPT_SMART_CARD_ROOT_INFO
  {$EXTERNALSYM CERT_NO_AUTO_EXPIRE_CHECK_PROP_ID}
  CERT_NO_AUTO_EXPIRE_CHECK_PROP_ID             = 77;
  {$EXTERNALSYM CERT_NCRYPT_KEY_HANDLE_PROP_ID}
  CERT_NCRYPT_KEY_HANDLE_PROP_ID                = 78;
  {$EXTERNALSYM CERT_HCRYPTPROV_OR_NCRYPT_KEY_HANDLE_PROP_ID}
  CERT_HCRYPTPROV_OR_NCRYPT_KEY_HANDLE_PROP_ID  = 79;

  {$EXTERNALSYM CERT_SUBJECT_INFO_ACCESS_PROP_ID}
  CERT_SUBJECT_INFO_ACCESS_PROP_ID              = 80;
  {$EXTERNALSYM CERT_CA_OCSP_AUTHORITY_INFO_ACCESS_PROP_ID}
  CERT_CA_OCSP_AUTHORITY_INFO_ACCESS_PROP_ID    = 81;
  {$EXTERNALSYM CERT_CA_DISABLE_CRL_PROP_ID}
  CERT_CA_DISABLE_CRL_PROP_ID                   = 82;
  {$EXTERNALSYM CERT_ROOT_PROGRAM_CERT_POLICIES_PROP_ID}
  CERT_ROOT_PROGRAM_CERT_POLICIES_PROP_ID       = 83;
  {$EXTERNALSYM CERT_ROOT_PROGRAM_NAME_CONSTRAINTS_PROP_ID}
  CERT_ROOT_PROGRAM_NAME_CONSTRAINTS_PROP_ID    = 84;
  {$EXTERNALSYM CERT_FIRST_RESERVED_PROP_ID}
  CERT_FIRST_RESERVED_PROP_ID                   = 85;

  {$EXTERNALSYM CERT_LAST_RESERVED_PROP_ID}
  CERT_LAST_RESERVED_PROP_ID                    = $00007FFF;
  {$EXTERNALSYM CERT_FIRST_USER_PROP_ID}
  CERT_FIRST_USER_PROP_ID                       = $00008000;
  {$EXTERNALSYM CERT_LAST_USER_PROP_ID}
  CERT_LAST_USER_PROP_ID                        = $0000FFFF;
// certenrolld_end

{$EXTERNALSYM IS_CERT_HASH_PROP_ID}
function IS_CERT_HASH_PROP_ID(X: DWORD): Boolean;

{$EXTERNALSYM IS_PUBKEY_HASH_PROP_ID}
function IS_PUBKEY_HASH_PROP_ID(X: DWORD): Boolean;

{$EXTERNALSYM IS_CHAIN_HASH_PROP_ID}
function IS_CHAIN_HASH_PROP_ID(X: DWORD): Boolean;

//+-------------------------------------------------------------------------
//  Property OIDs
//--------------------------------------------------------------------------
// The OID component following the prefix contains the PROP_ID (decimal)
const
  {$EXTERNALSYM szOID_CERT_PROP_ID_PREFIX}
  szOID_CERT_PROP_ID_PREFIX           = '1.3.6.1.4.1.311.10.11.';

//#define _szPROP_ID(PropId)       #PropId
//
//// Ansi OID string from Property Id:
//#define szOID_CERT_PROP_ID(PropId) szOID_CERT_PROP_ID_PREFIX _szPROP_ID(PropId)
//
//// Unicode OID string from Property Id:
//#define __CRYPT32WTEXT(quote)           L##quote
//#define _CRYPT32WTEXT(quote)            __CRYPT32WTEXT(quote)
//#define wszOID_CERT_PROP_ID(PropId) \
//        _CRYPT32WTEXT(szOID_CERT_PROP_ID_PREFIX) _CRYPT32WTEXT(_szPROP_ID(PropId))

// Use szOID_CERT_PROP_ID(CERT_KEY_IDENTIFIER_PROP_ID) instead:
  {$EXTERNALSYM szOID_CERT_KEY_IDENTIFIER_PROP_ID}
  szOID_CERT_KEY_IDENTIFIER_PROP_ID   = '1.3.6.1.4.1.311.10.11.20';

// Use szOID_CERT_PROP_ID(CERT_ISSUER_SERIAL_NUMBER_MD5_HASH_PROP_ID) instead:
  {$EXTERNALSYM szOID_CERT_ISSUER_SERIAL_NUMBER_MD5_HASH_PROP_ID}
  szOID_CERT_ISSUER_SERIAL_NUMBER_MD5_HASH_PROP_ID = '1.3.6.1.4.1.311.10.11.28';

// Use szOID_CERT_PROP_ID(CERT_SUBJECT_NAME_MD5_HASH_PROP_ID) instead:
  {$EXTERNALSYM szOID_CERT_SUBJECT_NAME_MD5_HASH_PROP_ID}
  szOID_CERT_SUBJECT_NAME_MD5_HASH_PROP_ID = '1.3.6.1.4.1.311.10.11.29';

// Use szOID_CERT_PROP_ID(CERT_MD5_HASH_PROP_ID) instead:
  {$EXTERNALSYM szOID_CERT_MD5_HASH_PROP_ID}
  szOID_CERT_MD5_HASH_PROP_ID         = '1.3.6.1.4.1.311.10.11.4';

//+-------------------------------------------------------------------------
//  Access State flags returned by CERT_ACCESS_STATE_PROP_ID. Note,
//  CERT_ACCESS_PROP_ID is read only.
//--------------------------------------------------------------------------

// Set if context property writes are persisted. For instance, not set for
// memory store contexts. Set for registry based stores opened as read or write.
// Not set for registry based stores opened as read only.
  {$EXTERNALSYM CERT_ACCESS_STATE_WRITE_PERSIST_FLAG}
  CERT_ACCESS_STATE_WRITE_PERSIST_FLAG = $1;

// Set if context resides in a SYSTEM or SYSTEM_REGISTRY store.
  {$EXTERNALSYM CERT_ACCESS_STATE_SYSTEM_STORE_FLAG}
  CERT_ACCESS_STATE_SYSTEM_STORE_FLAG = $2;

// Set if context resides in a LocalMachine SYSTEM or SYSTEM_REGISTRY store.
  {$EXTERNALSYM CERT_ACCESS_STATE_LM_SYSTEM_STORE_FLAG}
  CERT_ACCESS_STATE_LM_SYSTEM_STORE_FLAG = $4;

// Set if context resides in a GroupPolicy SYSTEM or SYSTEM_REGISTRY store.
  {$EXTERNALSYM CERT_ACCESS_STATE_GP_SYSTEM_STORE_FLAG}
  CERT_ACCESS_STATE_GP_SYSTEM_STORE_FLAG = $8;

//+-------------------------------------------------------------------------
//  Cryptographic Key Provider Information
//
//  CRYPT_KEY_PROV_INFO defines the CERT_KEY_PROV_INFO_PROP_ID's pvData.
//
//  The CRYPT_KEY_PROV_INFO fields are passed to CryptAcquireContext
//  to get a HCRYPTPROV handle. The optional CRYPT_KEY_PROV_PARAM fields are
//  passed to CryptSetProvParam to further initialize the provider.
//
//  The dwKeySpec field identifies the private key to use from the container
//  For example, AT_KEYEXCHANGE or AT_SIGNATURE.
//--------------------------------------------------------------------------
type
  PCryptKeyProvParam = ^TCryptKeyProvParam;
  {$EXTERNALSYM _CRYPT_KEY_PROV_PARAM}
  _CRYPT_KEY_PROV_PARAM = record
    dwParam: DWORD;
    pbData: PByte;
    cbData: DWORD;
    dwFlags: DWORD;
  end;
  {$EXTERNALSYM CRYPT_KEY_PROV_PARAM}
  CRYPT_KEY_PROV_PARAM = _CRYPT_KEY_PROV_PARAM;
  {$EXTERNALSYM PCRYPT_KEY_PROV_PARAM}
  PCRYPT_KEY_PROV_PARAM = ^_CRYPT_KEY_PROV_PARAM;
  TCryptKeyProvParam = _CRYPT_KEY_PROV_PARAM;

  PCryptKeyProvInfo = ^TCryptKeyProvInfo;
  {$EXTERNALSYM _CRYPT_KEY_PROV_INFO} 
  _CRYPT_KEY_PROV_INFO = record 
    pwszContainerName: LPWSTR; 
    pwszProvName: LPWSTR; 
    dwProvType: DWORD; 
    dwFlags: DWORD; 
    cProvParam: DWORD; 
    rgProvParam: PCRYPT_KEY_PROV_PARAM; 
    dwKeySpec: DWORD; 
  end; 
  {$EXTERNALSYM CRYPT_KEY_PROV_INFO} 
  CRYPT_KEY_PROV_INFO = _CRYPT_KEY_PROV_INFO; 
  {$EXTERNALSYM PCRYPT_KEY_PROV_INFO} 
  PCRYPT_KEY_PROV_INFO = ^_CRYPT_KEY_PROV_INFO; 
  TCryptKeyProvInfo = _CRYPT_KEY_PROV_INFO; 

//+-------------------------------------------------------------------------
//  The following flag should be set in the above dwFlags to enable
//  a CertSetCertificateContextProperty(CERT_KEY_CONTEXT_PROP_ID) after a
//  CryptAcquireContext is done in the Sign or Decrypt Message functions.
//
//  The following define must not collide with any of the
//  CryptAcquireContext dwFlag defines.
//--------------------------------------------------------------------------
const
  {$EXTERNALSYM CERT_SET_KEY_PROV_HANDLE_PROP_ID}
  CERT_SET_KEY_PROV_HANDLE_PROP_ID = $00000001;
  {$EXTERNALSYM CERT_SET_KEY_CONTEXT_PROP_ID}
  CERT_SET_KEY_CONTEXT_PROP_ID = $00000001;

// Special dwKeySpec indicating a CNG NCRYPT_KEY_HANDLE instead of a CAPI1
// HCRYPTPROV
  {$EXTERNALSYM CERT_NCRYPT_KEY_SPEC}
  CERT_NCRYPT_KEY_SPEC = $FFFFFFFF;

//+-------------------------------------------------------------------------
//  Certificate Key Context
//
//  CERT_KEY_CONTEXT defines the CERT_KEY_CONTEXT_PROP_ID's pvData.
//
//  dwKeySpec is set to the special CERT_NCRYPT_KEY_SPEC to select the
//  hNCryptKey choice.
//--------------------------------------------------------------------------

type
  PCertKeyContext = ^TCertKeyContext;
  {$EXTERNALSYM _CERT_KEY_CONTEXT}
  _CERT_KEY_CONTEXT = record
    cbSize: DWORD;           // sizeof(CERT_KEY_CONTEXT)
    case Byte of
      0: (hCryptProv: HCRYPTPROV;          // size: 4
          dwKeySpec: DWORD);
      1: (hNCryptKey: NCRYPT_KEY_HANDLE);  // size: 4
  end;
  {$EXTERNALSYM CERT_KEY_CONTEXT}
  CERT_KEY_CONTEXT = _CERT_KEY_CONTEXT;
  TCertKeyContext = _CERT_KEY_CONTEXT;

//+-------------------------------------------------------------------------
//  Cryptographic Smart Card Root Information
//
//  CRYPT_SMART_CARD_ROOT_INFO defines the
//  CERT_SMART_CARD_ROOT_INFO_PROP_ID's pvData.
//--------------------------------------------------------------------------
  PRootInfoLuid = ^TRootInfoLuid;
  {$EXTERNALSYM _ROOT_INFO_LUID}
  _ROOT_INFO_LUID = record
    LowPart: DWORD;
    HighPart: Longint;
  end;
  {$EXTERNALSYM ROOT_INFO_LUID}
  ROOT_INFO_LUID = _ROOT_INFO_LUID;
  {$EXTERNALSYM PROOT_INFO_LUID}
  PROOT_INFO_LUID = ^_ROOT_INFO_LUID;
  TRootInfoLuid = _ROOT_INFO_LUID;

  PCryptSmartCardRootInfo = ^TCryptSmartCardRootInfo;
  {$EXTERNALSYM _CRYPT_SMART_CARD_ROOT_INFO} 
  _CRYPT_SMART_CARD_ROOT_INFO = record 
    rgbCardID: array[0..15] of Byte; 
    luid: ROOT_INFO_LUID; 
  end; 
  {$EXTERNALSYM CRYPT_SMART_CARD_ROOT_INFO} 
  CRYPT_SMART_CARD_ROOT_INFO = _CRYPT_SMART_CARD_ROOT_INFO; 
  {$EXTERNALSYM PCRYPT_SMART_CARD_ROOT_INFO} 
  PCRYPT_SMART_CARD_ROOT_INFO = ^_CRYPT_SMART_CARD_ROOT_INFO; 
  TCryptSmartCardRootInfo = _CRYPT_SMART_CARD_ROOT_INFO; 

//+-------------------------------------------------------------------------
//  Certificate Store Provider Types
//--------------------------------------------------------------------------
const
  {$EXTERNALSYM CERT_STORE_PROV_MSG}
  CERT_STORE_PROV_MSG                 = LPCSTR(1);
  {$EXTERNALSYM CERT_STORE_PROV_MEMORY}
  CERT_STORE_PROV_MEMORY              = LPCSTR(2);
  {$EXTERNALSYM CERT_STORE_PROV_FILE}
  CERT_STORE_PROV_FILE                = LPCSTR(3);
  {$EXTERNALSYM CERT_STORE_PROV_REG}
  CERT_STORE_PROV_REG                 = LPCSTR(4);

  {$EXTERNALSYM CERT_STORE_PROV_PKCS7}
  CERT_STORE_PROV_PKCS7               = LPCSTR(5);
  {$EXTERNALSYM CERT_STORE_PROV_SERIALIZED}
  CERT_STORE_PROV_SERIALIZED          = LPCSTR(6);
  {$EXTERNALSYM CERT_STORE_PROV_FILENAME_A}
  CERT_STORE_PROV_FILENAME_A          = LPCSTR(7);
  {$EXTERNALSYM CERT_STORE_PROV_FILENAME_W}
  CERT_STORE_PROV_FILENAME_W          = LPCSTR(8);
  {$EXTERNALSYM CERT_STORE_PROV_FILENAME}
  CERT_STORE_PROV_FILENAME            = CERT_STORE_PROV_FILENAME_W;
  {$EXTERNALSYM CERT_STORE_PROV_SYSTEM_A}
  CERT_STORE_PROV_SYSTEM_A            = LPCSTR(9);
  {$EXTERNALSYM CERT_STORE_PROV_SYSTEM_W}
  CERT_STORE_PROV_SYSTEM_W            = LPCSTR(10);
  {$EXTERNALSYM CERT_STORE_PROV_SYSTEM}
  CERT_STORE_PROV_SYSTEM              = CERT_STORE_PROV_SYSTEM_W;

  {$EXTERNALSYM CERT_STORE_PROV_COLLECTION}
  CERT_STORE_PROV_COLLECTION          = LPCSTR(11);
  {$EXTERNALSYM CERT_STORE_PROV_SYSTEM_REGISTRY_A}
  CERT_STORE_PROV_SYSTEM_REGISTRY_A   = LPCSTR(12);
  {$EXTERNALSYM CERT_STORE_PROV_SYSTEM_REGISTRY_W}
  CERT_STORE_PROV_SYSTEM_REGISTRY_W   = LPCSTR(13);
  {$EXTERNALSYM CERT_STORE_PROV_SYSTEM_REGISTRY}
  CERT_STORE_PROV_SYSTEM_REGISTRY     = CERT_STORE_PROV_SYSTEM_REGISTRY_W;
  {$EXTERNALSYM CERT_STORE_PROV_PHYSICAL_W}
  CERT_STORE_PROV_PHYSICAL_W          = LPCSTR(14);
  {$EXTERNALSYM CERT_STORE_PROV_PHYSICAL}
  CERT_STORE_PROV_PHYSICAL            = CERT_STORE_PROV_PHYSICAL_W;

// SmartCard Store Provider isn't supported
  {$EXTERNALSYM CERT_STORE_PROV_SMART_CARD_W}
  CERT_STORE_PROV_SMART_CARD_W        = LPCSTR(15);
  {$EXTERNALSYM CERT_STORE_PROV_SMART_CARD}
  CERT_STORE_PROV_SMART_CARD          = CERT_STORE_PROV_SMART_CARD_W;

  {$EXTERNALSYM CERT_STORE_PROV_LDAP_W}
  CERT_STORE_PROV_LDAP_W              = LPCSTR(16);
  {$EXTERNALSYM CERT_STORE_PROV_LDAP}
  CERT_STORE_PROV_LDAP                = CERT_STORE_PROV_LDAP_W;
  {$EXTERNALSYM CERT_STORE_PROV_PKCS12}
  CERT_STORE_PROV_PKCS12              = LPCSTR(17);

  {$EXTERNALSYM sz_CERT_STORE_PROV_MEMORY}
  sz_CERT_STORE_PROV_MEMORY           = 'Memory';
  {$EXTERNALSYM sz_CERT_STORE_PROV_FILENAME_W}
  sz_CERT_STORE_PROV_FILENAME_W       = 'File';
  {$EXTERNALSYM sz_CERT_STORE_PROV_FILENAME}
  sz_CERT_STORE_PROV_FILENAME         = sz_CERT_STORE_PROV_FILENAME_W;
  {$EXTERNALSYM sz_CERT_STORE_PROV_SYSTEM_W}
  sz_CERT_STORE_PROV_SYSTEM_W         = 'System';
  {$EXTERNALSYM sz_CERT_STORE_PROV_SYSTEM}
  sz_CERT_STORE_PROV_SYSTEM           = sz_CERT_STORE_PROV_SYSTEM_W;
  {$EXTERNALSYM sz_CERT_STORE_PROV_PKCS7}
  sz_CERT_STORE_PROV_PKCS7            = 'PKCS7';
  {$EXTERNALSYM sz_CERT_STORE_PROV_PKCS12}
  sz_CERT_STORE_PROV_PKCS12           = 'PKCS12';
  {$EXTERNALSYM sz_CERT_STORE_PROV_SERIALIZED}
  sz_CERT_STORE_PROV_SERIALIZED       = 'Serialized';

  {$EXTERNALSYM sz_CERT_STORE_PROV_COLLECTION}
  sz_CERT_STORE_PROV_COLLECTION       = 'Collection';
  {$EXTERNALSYM sz_CERT_STORE_PROV_SYSTEM_REGISTRY_W}
  sz_CERT_STORE_PROV_SYSTEM_REGISTRY_W = 'SystemRegistry';
  {$EXTERNALSYM sz_CERT_STORE_PROV_SYSTEM_REGISTRY}
  sz_CERT_STORE_PROV_SYSTEM_REGISTRY  = sz_CERT_STORE_PROV_SYSTEM_REGISTRY_W;
  {$EXTERNALSYM sz_CERT_STORE_PROV_PHYSICAL_W}
  sz_CERT_STORE_PROV_PHYSICAL_W       = 'Physical';
  {$EXTERNALSYM sz_CERT_STORE_PROV_PHYSICAL}
  sz_CERT_STORE_PROV_PHYSICAL         = sz_CERT_STORE_PROV_PHYSICAL_W;

// SmartCard Store Provider isn't supported
  {$EXTERNALSYM sz_CERT_STORE_PROV_SMART_CARD_W}
  sz_CERT_STORE_PROV_SMART_CARD_W     = 'SmartCard';
  {$EXTERNALSYM sz_CERT_STORE_PROV_SMART_CARD}
  sz_CERT_STORE_PROV_SMART_CARD       = sz_CERT_STORE_PROV_SMART_CARD_W;

  {$EXTERNALSYM sz_CERT_STORE_PROV_LDAP_W}
  sz_CERT_STORE_PROV_LDAP_W           = 'Ldap';
  {$EXTERNALSYM sz_CERT_STORE_PROV_LDAP}
  sz_CERT_STORE_PROV_LDAP             = sz_CERT_STORE_PROV_LDAP_W;

//+-------------------------------------------------------------------------
//  Certificate Store verify/results flags
//--------------------------------------------------------------------------
  {$EXTERNALSYM CERT_STORE_SIGNATURE_FLAG}
  CERT_STORE_SIGNATURE_FLAG = $00000001;
  {$EXTERNALSYM CERT_STORE_TIME_VALIDITY_FLAG}
  CERT_STORE_TIME_VALIDITY_FLAG = $00000002;
  {$EXTERNALSYM CERT_STORE_REVOCATION_FLAG}
  CERT_STORE_REVOCATION_FLAG = $00000004;
  {$EXTERNALSYM CERT_STORE_NO_CRL_FLAG}
  CERT_STORE_NO_CRL_FLAG = $00010000;
  {$EXTERNALSYM CERT_STORE_NO_ISSUER_FLAG}
  CERT_STORE_NO_ISSUER_FLAG = $00020000;

  {$EXTERNALSYM CERT_STORE_BASE_CRL_FLAG}
  CERT_STORE_BASE_CRL_FLAG = $00000100;
  {$EXTERNALSYM CERT_STORE_DELTA_CRL_FLAG}
  CERT_STORE_DELTA_CRL_FLAG = $00000200;

//+-------------------------------------------------------------------------
//  Certificate Store open/property flags
//--------------------------------------------------------------------------
  {$EXTERNALSYM CERT_STORE_NO_CRYPT_RELEASE_FLAG}
  CERT_STORE_NO_CRYPT_RELEASE_FLAG = $00000001;
  {$EXTERNALSYM CERT_STORE_SET_LOCALIZED_NAME_FLAG}
  CERT_STORE_SET_LOCALIZED_NAME_FLAG = $00000002;
  {$EXTERNALSYM CERT_STORE_DEFER_CLOSE_UNTIL_LAST_FREE_FLAG}
  CERT_STORE_DEFER_CLOSE_UNTIL_LAST_FREE_FLAG = $00000004;
  {$EXTERNALSYM CERT_STORE_DELETE_FLAG}
  CERT_STORE_DELETE_FLAG = $00000010;
  {$EXTERNALSYM CERT_STORE_UNSAFE_PHYSICAL_FLAG}
  CERT_STORE_UNSAFE_PHYSICAL_FLAG = $00000020;
  {$EXTERNALSYM CERT_STORE_SHARE_STORE_FLAG}
  CERT_STORE_SHARE_STORE_FLAG = $00000040;
  {$EXTERNALSYM CERT_STORE_SHARE_CONTEXT_FLAG}
  CERT_STORE_SHARE_CONTEXT_FLAG = $00000080;
  {$EXTERNALSYM CERT_STORE_MANIFOLD_FLAG}
  CERT_STORE_MANIFOLD_FLAG = $00000100;
  {$EXTERNALSYM CERT_STORE_ENUM_ARCHIVED_FLAG}
  CERT_STORE_ENUM_ARCHIVED_FLAG = $00000200;
  {$EXTERNALSYM CERT_STORE_UPDATE_KEYID_FLAG}
  CERT_STORE_UPDATE_KEYID_FLAG = $00000400;
  {$EXTERNALSYM CERT_STORE_BACKUP_RESTORE_FLAG}
  CERT_STORE_BACKUP_RESTORE_FLAG = $00000800;
  {$EXTERNALSYM CERT_STORE_READONLY_FLAG}
  CERT_STORE_READONLY_FLAG = $00008000;
  {$EXTERNALSYM CERT_STORE_OPEN_EXISTING_FLAG}
  CERT_STORE_OPEN_EXISTING_FLAG = $00004000;
  {$EXTERNALSYM CERT_STORE_CREATE_NEW_FLAG}
  CERT_STORE_CREATE_NEW_FLAG = $00002000;
  {$EXTERNALSYM CERT_STORE_MAXIMUM_ALLOWED_FLAG}
  CERT_STORE_MAXIMUM_ALLOWED_FLAG = $00001000;

//+-------------------------------------------------------------------------
//  Certificate Store Provider flags are in the HiWord ($FFFF0000)
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  Certificate System Store Flag Values
//--------------------------------------------------------------------------
// Includes flags and location
  {$EXTERNALSYM CERT_SYSTEM_STORE_MASK}
  CERT_SYSTEM_STORE_MASK = $FFFF0000;

// Set if pvPara points to a CERT_SYSTEM_STORE_RELOCATE_PARA structure
  {$EXTERNALSYM CERT_SYSTEM_STORE_RELOCATE_FLAG}
  CERT_SYSTEM_STORE_RELOCATE_FLAG = $80000000;

type
  PCertSystemStoreRelocatePara = ^TCertSystemStoreRelocatePara;
  {$EXTERNALSYM _CERT_SYSTEM_STORE_RELOCATE_PARA} 
  _CERT_SYSTEM_STORE_RELOCATE_PARA = record 
    u1: record case Byte of
      0: (hKeyBase: HKEY);              // size: 4
      1: (pvBase: Pointer);             // size: 4
    end; 
    u2: record case Byte of
      0: (pvSystemStore: Pointer);      // size: 4
      1: (pszSystemStore: LPCSTR);      // size: 4
      2: (pwszSystemStore: LPCWSTR);    // size: 4 
    end; 
  end; 
  {$EXTERNALSYM CERT_SYSTEM_STORE_RELOCATE_PARA} 
  CERT_SYSTEM_STORE_RELOCATE_PARA = _CERT_SYSTEM_STORE_RELOCATE_PARA; 
  {$EXTERNALSYM PCERT_SYSTEM_STORE_RELOCATE_PARA} 
  PCERT_SYSTEM_STORE_RELOCATE_PARA = ^_CERT_SYSTEM_STORE_RELOCATE_PARA; 
  TCertSystemStoreRelocatePara = _CERT_SYSTEM_STORE_RELOCATE_PARA; 

// By default, when the CurrentUser 'Root' store is opened, any SystemRegistry
// roots not also on the protected root list are deleted from the cache before
// CertOpenStore() returns. Set the following flag to return all the roots
// in the SystemRegistry without checking the protected root list.
const
  {$EXTERNALSYM CERT_SYSTEM_STORE_UNPROTECTED_FLAG}
  CERT_SYSTEM_STORE_UNPROTECTED_FLAG              = $40000000;

// Location of the system store:
  {$EXTERNALSYM CERT_SYSTEM_STORE_LOCATION_MASK}
  CERT_SYSTEM_STORE_LOCATION_MASK                 = $00FF0000;
  {$EXTERNALSYM CERT_SYSTEM_STORE_LOCATION_SHIFT}
  CERT_SYSTEM_STORE_LOCATION_SHIFT                = 16;

//  Registry: HKEY_CURRENT_USER or HKEY_LOCAL_MACHINE
  {$EXTERNALSYM CERT_SYSTEM_STORE_CURRENT_USER_ID}
  CERT_SYSTEM_STORE_CURRENT_USER_ID               = 1;
  {$EXTERNALSYM CERT_SYSTEM_STORE_LOCAL_MACHINE_ID}
  CERT_SYSTEM_STORE_LOCAL_MACHINE_ID              = 2;
//  Registry: HKEY_LOCAL_MACHINE\Software\Microsoft\Cryptography\Services
  {$EXTERNALSYM CERT_SYSTEM_STORE_CURRENT_SERVICE_ID}
  CERT_SYSTEM_STORE_CURRENT_SERVICE_ID            = 4;
  {$EXTERNALSYM CERT_SYSTEM_STORE_SERVICES_ID}
  CERT_SYSTEM_STORE_SERVICES_ID                   = 5;
//  Registry: HKEY_USERS
  {$EXTERNALSYM CERT_SYSTEM_STORE_USERS_ID}
  CERT_SYSTEM_STORE_USERS_ID                      = 6;

//  Registry: HKEY_CURRENT_USER\Software\Policies\Microsoft\SystemCertificates
  {$EXTERNALSYM CERT_SYSTEM_STORE_CURRENT_USER_GROUP_POLICY_ID}
  CERT_SYSTEM_STORE_CURRENT_USER_GROUP_POLICY_ID  = 7;
//  Registry: HKEY_LOCAL_MACHINE\Software\Policies\Microsoft\SystemCertificates
  {$EXTERNALSYM CERT_SYSTEM_STORE_LOCAL_MACHINE_GROUP_POLICY_ID}
  CERT_SYSTEM_STORE_LOCAL_MACHINE_GROUP_POLICY_ID = 8;

//  Registry: HKEY_LOCAL_MACHINE\Software\Microsoft\EnterpriseCertificates
  {$EXTERNALSYM CERT_SYSTEM_STORE_LOCAL_MACHINE_ENTERPRISE_ID}
  CERT_SYSTEM_STORE_LOCAL_MACHINE_ENTERPRISE_ID   = 9;

  {$EXTERNALSYM CERT_SYSTEM_STORE_CURRENT_USER}
  CERT_SYSTEM_STORE_CURRENT_USER                  =
    CERT_SYSTEM_STORE_CURRENT_USER_ID shl CERT_SYSTEM_STORE_LOCATION_SHIFT;
  {$EXTERNALSYM CERT_SYSTEM_STORE_LOCAL_MACHINE}
  CERT_SYSTEM_STORE_LOCAL_MACHINE                 =
    CERT_SYSTEM_STORE_LOCAL_MACHINE_ID shl CERT_SYSTEM_STORE_LOCATION_SHIFT;
  {$EXTERNALSYM CERT_SYSTEM_STORE_CURRENT_SERVICE}
  CERT_SYSTEM_STORE_CURRENT_SERVICE               =
    CERT_SYSTEM_STORE_CURRENT_SERVICE_ID shl CERT_SYSTEM_STORE_LOCATION_SHIFT;
  {$EXTERNALSYM CERT_SYSTEM_STORE_SERVICES}
  CERT_SYSTEM_STORE_SERVICES                      =
    CERT_SYSTEM_STORE_SERVICES_ID shl CERT_SYSTEM_STORE_LOCATION_SHIFT;
  {$EXTERNALSYM CERT_SYSTEM_STORE_USERS}
  CERT_SYSTEM_STORE_USERS                         =
    CERT_SYSTEM_STORE_USERS_ID shl CERT_SYSTEM_STORE_LOCATION_SHIFT;

  {$EXTERNALSYM CERT_SYSTEM_STORE_CURRENT_USER_GROUP_POLICY}
  CERT_SYSTEM_STORE_CURRENT_USER_GROUP_POLICY     =
    CERT_SYSTEM_STORE_CURRENT_USER_GROUP_POLICY_ID shl
      CERT_SYSTEM_STORE_LOCATION_SHIFT;
  {$EXTERNALSYM CERT_SYSTEM_STORE_LOCAL_MACHINE_GROUP_POLICY}
  CERT_SYSTEM_STORE_LOCAL_MACHINE_GROUP_POLICY    =
    CERT_SYSTEM_STORE_LOCAL_MACHINE_GROUP_POLICY_ID shl
      CERT_SYSTEM_STORE_LOCATION_SHIFT;

  {$EXTERNALSYM CERT_SYSTEM_STORE_LOCAL_MACHINE_ENTERPRISE}
  CERT_SYSTEM_STORE_LOCAL_MACHINE_ENTERPRISE      =
    CERT_SYSTEM_STORE_LOCAL_MACHINE_ENTERPRISE_ID shl
        CERT_SYSTEM_STORE_LOCATION_SHIFT;


//+-------------------------------------------------------------------------
//  Group Policy Store Defines
//--------------------------------------------------------------------------
// Registry path to the Group Policy system stores
  {$EXTERNALSYM CERT_GROUP_POLICY_SYSTEM_STORE_REGPATH}
  CERT_GROUP_POLICY_SYSTEM_STORE_REGPATH          =
    'Software\Policies\Microsoft\SystemCertificates';

//+-------------------------------------------------------------------------
//  EFS Defines
//--------------------------------------------------------------------------
// Registry path to the EFS EFSBlob SubKey - Value type is REG_BINARY
  {$EXTERNALSYM CERT_EFSBLOB_REGPATH}
  CERT_EFSBLOB_REGPATH                            =
    CERT_GROUP_POLICY_SYSTEM_STORE_REGPATH + '\EFS';
  {$EXTERNALSYM CERT_EFSBLOB_VALUE_NAME}
  CERT_EFSBLOB_VALUE_NAME = 'EFSBlob';

//+-------------------------------------------------------------------------
//  Protected Root Defines
//--------------------------------------------------------------------------
// Registry path to the Protected Roots Flags SubKey
  {$EXTERNALSYM CERT_PROT_ROOT_FLAGS_REGPATH}
  CERT_PROT_ROOT_FLAGS_REGPATH                    =
    CERT_GROUP_POLICY_SYSTEM_STORE_REGPATH + '\Root\ProtectedRoots';

// The following is a REG_DWORD. The bit definitions follow.
  {$EXTERNALSYM CERT_PROT_ROOT_FLAGS_VALUE_NAME}
  CERT_PROT_ROOT_FLAGS_VALUE_NAME = 'Flags';

// Set the following flag to inhibit the opening of the CurrentUser's
// .Default physical store when opening the CurrentUser's 'Root' system store.
// The .Default physical store open's the CurrentUser SystemRegistry 'Root'
// store.
  {$EXTERNALSYM CERT_PROT_ROOT_DISABLE_CURRENT_USER_FLAG}
  CERT_PROT_ROOT_DISABLE_CURRENT_USER_FLAG        = $1;

// Set the following flag to inhibit the adding of roots from the
// CurrentUser SystemRegistry 'Root' store to the protected root list
// when the 'Root' store is initially protected.
  {$EXTERNALSYM CERT_PROT_ROOT_INHIBIT_ADD_AT_INIT_FLAG}
  CERT_PROT_ROOT_INHIBIT_ADD_AT_INIT_FLAG         = $2;

// Set the following flag to inhibit the purging of protected roots from the
// CurrentUser SystemRegistry 'Root' store that are
// also in the LocalMachine SystemRegistry 'Root' store. Note, when not
// disabled, the purging is done silently without UI.
  {$EXTERNALSYM CERT_PROT_ROOT_INHIBIT_PURGE_LM_FLAG}
  CERT_PROT_ROOT_INHIBIT_PURGE_LM_FLAG            = $4;

// Set the following flag to inhibit the opening of the LocalMachine's
// .AuthRoot physical store when opening the LocalMachine's 'Root' system store.
// The .AuthRoot physical store open's the LocalMachine SystemRegistry
// 'AuthRoot' store. The 'AuthRoot' store contains the pre-installed
// SSL ServerAuth and the ActiveX Authenticode 'root' certificates.
  {$EXTERNALSYM CERT_PROT_ROOT_DISABLE_LM_AUTH_FLAG}
  CERT_PROT_ROOT_DISABLE_LM_AUTH_FLAG             = $8;

// The semantics for the following legacy definition has been changed to be
// the same as for the CERT_PROT_ROOT_DISABLE_LM_AUTH_FLAG.
  {$EXTERNALSYM CERT_PROT_ROOT_ONLY_LM_GPT_FLAG}
  CERT_PROT_ROOT_ONLY_LM_GPT_FLAG                 = $8;

// Set the following flag to disable the requiring of the issuing CA
// certificate being in the 'NTAuth' system registry store found in the
// CERT_SYSTEM_STORE_LOCAL_MACHINE_ENTERPRISE store location.
//
// When set, CertVerifyCertificateChainPolicy(CERT_CHAIN_POLICY_NT_AUTH)
// will check that the chain has a valid name constraint for all name
// spaces, including UPN if the issuing CA isn't in the 'NTAuth' store.
  {$EXTERNALSYM CERT_PROT_ROOT_DISABLE_NT_AUTH_REQUIRED_FLAG}
  CERT_PROT_ROOT_DISABLE_NT_AUTH_REQUIRED_FLAG    = $10;

// Set the following flag to disable checking for not defined name
// constraints.
//
// When set, CertGetCertificateChain won't check for or set the following
// dwErrorStatus: CERT_TRUST_HAS_NOT_DEFINED_NAME_CONSTRAINT.
//
// In LH, checking for not defined name constraints is always disabled.
  {$EXTERNALSYM CERT_PROT_ROOT_DISABLE_NOT_DEFINED_NAME_CONSTRAINT_FLAG}
  CERT_PROT_ROOT_DISABLE_NOT_DEFINED_NAME_CONSTRAINT_FLAG = $20;

// Set the following flag to disallow the users to trust peer-trust
  {$EXTERNALSYM CERT_PROT_ROOT_DISABLE_PEER_TRUST}
  CERT_PROT_ROOT_DISABLE_PEER_TRUST               = $10000;

// The following is a REG_MULTI_SZ containing the list of user allowed
// Enhanced Key Usages for peer trust.
  {$EXTERNALSYM CERT_PROT_ROOT_PEER_USAGES_VALUE_NAME}
  CERT_PROT_ROOT_PEER_USAGES_VALUE_NAME           = 'PeerUsages';
  {$EXTERNALSYM CERT_PROT_ROOT_PEER_USAGES_VALUE_NAME_A}
  CERT_PROT_ROOT_PEER_USAGES_VALUE_NAME_A         = 'PeerUsages';

// If the above REG_MULTI_SZ isn't defined or is empty, defaults to
// the following multi-string value
  {$EXTERNALSYM CERT_PROT_ROOT_PEER_USAGES_DEFAULT_A}
  CERT_PROT_ROOT_PEER_USAGES_DEFAULT_A            =
    szOID_PKIX_KP_CLIENT_AUTH + #0 +
    szOID_PKIX_KP_EMAIL_PROTECTION + #0 +
    szOID_KP_EFS + #0;

//+-------------------------------------------------------------------------
//  Trusted Publisher Definitions
//--------------------------------------------------------------------------
// Registry path to the trusted publisher 'Safer' group policy subkey
  {$EXTERNALSYM CERT_TRUST_PUB_SAFER_GROUP_POLICY_REGPATH}
  CERT_TRUST_PUB_SAFER_GROUP_POLICY_REGPATH       =
    CERT_GROUP_POLICY_SYSTEM_STORE_REGPATH + '\TrustedPublisher\Safer';

// Registry path to the Local Machine system stores
  {$EXTERNALSYM CERT_LOCAL_MACHINE_SYSTEM_STORE_REGPATH}
  CERT_LOCAL_MACHINE_SYSTEM_STORE_REGPATH         =
    'Software\\Microsoft\\SystemCertificates';

// Registry path to the trusted publisher 'Safer' local machine subkey
  {$EXTERNALSYM CERT_TRUST_PUB_SAFER_LOCAL_MACHINE_REGPATH}
  CERT_TRUST_PUB_SAFER_LOCAL_MACHINE_REGPATH      =
    CERT_LOCAL_MACHINE_SYSTEM_STORE_REGPATH + '\TrustedPublisher\Safer';

// 'Safer' subkey value names. All values are DWORDs.
  {$EXTERNALSYM CERT_TRUST_PUB_AUTHENTICODE_FLAGS_VALUE_NAME}
  CERT_TRUST_PUB_AUTHENTICODE_FLAGS_VALUE_NAME    = 'AuthenticodeFlags';

// AuthenticodeFlags definitions

// Definition of who is allowed to trust publishers
//
// Setting allowed trust to MACHINE_ADMIN or ENTERPRISE_ADMIN disables UI,
// only trusts publishers in the 'TrustedPublisher' system store and
// inhibits the opening of the CurrentUser's .Default physical store when
// opening the CurrentUsers's 'TrustedPublisher' system store.
//
// The .Default physical store open's the CurrentUser SystemRegistry
// 'TrustedPublisher' store.
//
// Setting allowed trust to ENTERPRISE_ADMIN only opens the
// LocalMachine's .GroupPolicy and .Enterprise physical stores when opening
// the CurrentUser's 'TrustedPublisher' system store or when opening the
// LocalMachine's 'TrustedPublisher' system store.

  {$EXTERNALSYM CERT_TRUST_PUB_ALLOW_TRUST_MASK}
  CERT_TRUST_PUB_ALLOW_TRUST_MASK             = $00000003;
  {$EXTERNALSYM CERT_TRUST_PUB_ALLOW_END_USER_TRUST}
  CERT_TRUST_PUB_ALLOW_END_USER_TRUST         = $00000000;
  {$EXTERNALSYM CERT_TRUST_PUB_ALLOW_MACHINE_ADMIN_TRUST}
  CERT_TRUST_PUB_ALLOW_MACHINE_ADMIN_TRUST    = $00000001;
  {$EXTERNALSYM CERT_TRUST_PUB_ALLOW_ENTERPRISE_ADMIN_TRUST}
  CERT_TRUST_PUB_ALLOW_ENTERPRISE_ADMIN_TRUST = $00000002;

// Set the following flag to enable revocation checking of the publisher
// chain.
  {$EXTERNALSYM CERT_TRUST_PUB_CHECK_PUBLISHER_REV_FLAG}
  CERT_TRUST_PUB_CHECK_PUBLISHER_REV_FLAG     = $00000100;

// Set the following flag to enable revocation checking of the time stamp
// chain.
  {$EXTERNALSYM CERT_TRUST_PUB_CHECK_TIMESTAMP_REV_FLAG}
  CERT_TRUST_PUB_CHECK_TIMESTAMP_REV_FLAG     = $00000200;

//+-------------------------------------------------------------------------
//  OCM Subcomponents Definitions
//
//  Reading of the following registry key has been deprecated on Longhorn.
//--------------------------------------------------------------------------

// Registry path to the OCM Subcomponents local machine subkey
  {$EXTERNALSYM CERT_OCM_SUBCOMPONENTS_LOCAL_MACHINE_REGPATH}
  CERT_OCM_SUBCOMPONENTS_LOCAL_MACHINE_REGPATH        =
    'SOFTWARE\Microsoft\Windows\CurrentVersion\Setup\OC Manager\Subcomponents';

// REG_DWORD, 1 is installed, 0 is NOT installed
  {$EXTERNALSYM CERT_OCM_SUBCOMPONENTS_ROOT_AUTO_UPDATE_VALUE_NAME}
  CERT_OCM_SUBCOMPONENTS_ROOT_AUTO_UPDATE_VALUE_NAME  = 'RootAutoUpdate';

//+-------------------------------------------------------------------------
//  DisableRootAutoUpdate Defines
//--------------------------------------------------------------------------
// Registry path to the DisableRootAutoUpdate SubKey
  {$EXTERNALSYM CERT_DISABLE_ROOT_AUTO_UPDATE_REGPATH}
  CERT_DISABLE_ROOT_AUTO_UPDATE_REGPATH               =
    CERT_GROUP_POLICY_SYSTEM_STORE_REGPATH + '\AuthRoot';

// REG_DWORD Value Name, 1 - disables, 0 - enables
  CERT_DISABLE_ROOT_AUTO_UPDATE_VALUE_NAME            = 'DisableRootAutoUpdate';

//+-------------------------------------------------------------------------
//  AuthRoot Auto Update Definitions
//--------------------------------------------------------------------------

// Registry path to the AuthRoot 'Auto Update' local machine subkey
  {$EXTERNALSYM CERT_AUTH_ROOT_AUTO_UPDATE_LOCAL_MACHINE_REGPATH}
  CERT_AUTH_ROOT_AUTO_UPDATE_LOCAL_MACHINE_REGPATH    =
    CERT_LOCAL_MACHINE_SYSTEM_STORE_REGPATH + '\AuthRoot\AutoUpdate';

// AuthRoot Auto Update subkey value names.

// REG_SZ, URL to the directory containing the AuthRoots, CTL and Seq files
  {$EXTERNALSYM CERT_AUTH_ROOT_AUTO_UPDATE_ROOT_DIR_URL_VALUE_NAME}
  CERT_AUTH_ROOT_AUTO_UPDATE_ROOT_DIR_URL_VALUE_NAME      = 'RootDirUrl';

// REG_DWORD, seconds between syncs. 0 implies use default.
  {$EXTERNALSYM CERT_AUTH_ROOT_AUTO_UPDATE_SYNC_DELTA_TIME_VALUE_NAME}
  CERT_AUTH_ROOT_AUTO_UPDATE_SYNC_DELTA_TIME_VALUE_NAME   = 'SyncDeltaTime';

// REG_DWORD, misc flags
  {$EXTERNALSYM CERT_AUTH_ROOT_AUTO_UPDATE_FLAGS_VALUE_NAME}
  CERT_AUTH_ROOT_AUTO_UPDATE_FLAGS_VALUE_NAME             = 'Flags';

  {$EXTERNALSYM CERT_AUTH_ROOT_AUTO_UPDATE_DISABLE_UNTRUSTED_ROOT_LOGGING_FLAG}
  CERT_AUTH_ROOT_AUTO_UPDATE_DISABLE_UNTRUSTED_ROOT_LOGGING_FLAG = $1;
  {$EXTERNALSYM CERT_AUTH_ROOT_AUTO_UPDATE_DISABLE_PARTIAL_CHAIN_LOGGING_FLAG}
  CERT_AUTH_ROOT_AUTO_UPDATE_DISABLE_PARTIAL_CHAIN_LOGGING_FLAG  = $2;

// AuthRoot Auto Update filenames

// CTL containing the list of certs in the AuthRoot store
  {$EXTERNALSYM CERT_AUTH_ROOT_CTL_FILENAME}
  CERT_AUTH_ROOT_CTL_FILENAME                             = 'authroot.stl';
  {$EXTERNALSYM CERT_AUTH_ROOT_CTL_FILENAME_A}
  CERT_AUTH_ROOT_CTL_FILENAME_A                           = 'authroot.stl';

// Cab containing the above CTL
  {$EXTERNALSYM CERT_AUTH_ROOT_CAB_FILENAME}
  CERT_AUTH_ROOT_CAB_FILENAME                             = 'authrootstl.cab';

// SequenceNumber (Formatted as big endian ascii hex)
  {$EXTERNALSYM CERT_AUTH_ROOT_SEQ_FILENAME}
  CERT_AUTH_ROOT_SEQ_FILENAME                             = 'authrootseq.txt';

// Root certs extension
  {$EXTERNALSYM CERT_AUTH_ROOT_CERT_EXT}
  CERT_AUTH_ROOT_CERT_EXT                                 = '.crt';

//+-------------------------------------------------------------------------
//  Certificate Registry Store Flag Values (CERT_STORE_REG)
//--------------------------------------------------------------------------

// Set this flag if the HKEY passed in pvPara points to a remote computer
// registry key.
  {$EXTERNALSYM CERT_REGISTRY_STORE_REMOTE_FLAG}
  CERT_REGISTRY_STORE_REMOTE_FLAG      = $10000;

// Set this flag if the contexts are to be persisted as a single serialized
// store in the registry. Mainly used for stores downloaded from the GPT.
// Such as the CurrentUserGroupPolicy or LocalMachineGroupPolicy stores.
  {$EXTERNALSYM CERT_REGISTRY_STORE_SERIALIZED_FLAG}
  CERT_REGISTRY_STORE_SERIALIZED_FLAG  = $20000;

// The following flags are for internal use. When set, the
// pvPara parameter passed to CertOpenStore is a pointer to the following
// data structure and not the HKEY. The above CERT_REGISTRY_STORE_REMOTE_FLAG
// is also set if hKeyBase was obtained via RegConnectRegistry().
  {$EXTERNALSYM CERT_REGISTRY_STORE_CLIENT_GPT_FLAG}
  CERT_REGISTRY_STORE_CLIENT_GPT_FLAG  = $80000000;
  {$EXTERNALSYM CERT_REGISTRY_STORE_LM_GPT_FLAG}
  CERT_REGISTRY_STORE_LM_GPT_FLAG      = $01000000;

type
  PCertRegistryStoreClientGptPara = ^TCertRegistryStoreClientGptPara; 
  {$EXTERNALSYM _CERT_REGISTRY_STORE_CLIENT_GPT_PARA} 
  _CERT_REGISTRY_STORE_CLIENT_GPT_PARA = record 
    hKeyBase: HKEY; 
    pwszRegPath: LPWSTR; 
  end; 
  {$EXTERNALSYM CERT_REGISTRY_STORE_CLIENT_GPT_PARA} 
  CERT_REGISTRY_STORE_CLIENT_GPT_PARA = _CERT_REGISTRY_STORE_CLIENT_GPT_PARA; 
  {$EXTERNALSYM PCERT_REGISTRY_STORE_CLIENT_GPT_PARA} 
  PCERT_REGISTRY_STORE_CLIENT_GPT_PARA = ^_CERT_REGISTRY_STORE_CLIENT_GPT_PARA; 
  TCertRegistryStoreClientGptPara = _CERT_REGISTRY_STORE_CLIENT_GPT_PARA; 

// The following flag is for internal use. When set, the contexts are
// persisted into roaming files instead of the registry. Such as, the
// CurrentUser 'My' store. When this flag is set, the following data structure
// is passed to CertOpenStore instead of HKEY.
const
  {$EXTERNALSYM CERT_REGISTRY_STORE_ROAMING_FLAG}
  CERT_REGISTRY_STORE_ROAMING_FLAG = $40000;

// hKey may be NULL or non-NULL. When non-NULL, existing contexts are
// moved from the registry to roaming files.
type
  PCertRegistryStoreRoamingPara = ^TCertRegistryStoreRoamingPara; 
  {$EXTERNALSYM _CERT_REGISTRY_STORE_ROAMING_PARA} 
  _CERT_REGISTRY_STORE_ROAMING_PARA = record 
    hKey: HKEY; 
    pwszStoreDirectory: LPWSTR; 
  end; 
  {$EXTERNALSYM CERT_REGISTRY_STORE_ROAMING_PARA} 
  CERT_REGISTRY_STORE_ROAMING_PARA = _CERT_REGISTRY_STORE_ROAMING_PARA; 
  {$EXTERNALSYM PCERT_REGISTRY_STORE_ROAMING_PARA} 
  PCERT_REGISTRY_STORE_ROAMING_PARA = ^_CERT_REGISTRY_STORE_ROAMING_PARA; 
  TCertRegistryStoreRoamingPara = _CERT_REGISTRY_STORE_ROAMING_PARA; 

const
// The following flag is for internal use. When set, the 'My' DWORD value
// at HKLM\Software\Microsoft\Cryptography\IEDirtyFlags is set to $1
// whenever a certificate is added to the registry store.
//
// Legacy definition, no longer supported after 01-May-02 (Server 2003)
  {$EXTERNALSYM CERT_REGISTRY_STORE_MY_IE_DIRTY_FLAG}
  CERT_REGISTRY_STORE_MY_IE_DIRTY_FLAG = $80000;

// Registry path to the subkey containing the 'My' DWORD value to be set
//
// Legacy definition, no longer supported after 01-May-02 (Server 2003)
  {$EXTERNALSYM CERT_IE_DIRTY_FLAGS_REGPATH}
  CERT_IE_DIRTY_FLAGS_REGPATH          =
    'Software\Microsoft\Cryptography\IEDirtyFlags';

//+-------------------------------------------------------------------------
//  Certificate File Store Flag Values for the providers:
//      CERT_STORE_PROV_FILE
//      CERT_STORE_PROV_FILENAME
//      CERT_STORE_PROV_FILENAME_A
//      CERT_STORE_PROV_FILENAME_W
//      sz_CERT_STORE_PROV_FILENAME_W
//--------------------------------------------------------------------------

// Set this flag if any store changes are to be committed to the file.
// The changes are committed at CertCloseStore or by calling
// CertControlStore(CERT_STORE_CTRL_COMMIT).
//
// The open fails with E_INVALIDARG if both CERT_FILE_STORE_COMMIT_ENABLE_FLAG
// and CERT_STORE_READONLY_FLAG are set in dwFlags.
//
// For the FILENAME providers:  if the file contains an X509 encoded
// certificate, the open fails with ERROR_ACCESS_DENIED.
//
// For the FILENAME providers: if CERT_STORE_CREATE_NEW_FLAG is set, the
// CreateFile uses CREATE_NEW. If CERT_STORE_OPEN_EXISTING is set, uses
// OPEN_EXISTING. Otherwise, defaults to OPEN_ALWAYS.
//
// For the FILENAME providers:  the file is committed as either a PKCS7 or
// serialized store depending on the type read at open. However, if the
// file is empty then, if the filename has either a '.p7c' or '.spc'
// extension its committed as a PKCS7. Otherwise, its committed as a
// serialized store.
//
// For CERT_STORE_PROV_FILE, the file handle is duplicated. Its always
// committed as a serialized store.
//
  {$EXTERNALSYM CERT_FILE_STORE_COMMIT_ENABLE_FLAG}
  CERT_FILE_STORE_COMMIT_ENABLE_FLAG    = $10000;


//+-------------------------------------------------------------------------
//  Certificate LDAP Store Flag Values for the providers:
//      CERT_STORE_PROV_LDAP
//      CERT_STORE_PROV_LDAP_W
//      sz_CERT_STORE_PROV_LDAP_W
//      sz_CERT_STORE_PROV_LDAP
//--------------------------------------------------------------------------

// Set this flag to digitally sign all of the ldap traffic to and from a
// Windows 2000 LDAP server using the Kerberos authentication protocol.
// This feature provides integrity required by some applications.
//
  {$EXTERNALSYM CERT_LDAP_STORE_SIGN_FLAG}
  CERT_LDAP_STORE_SIGN_FLAG             = $10000;

// Performs an A-Record only DNS lookup on the supplied host string.
// This prevents bogus DNS queries from being generated when resolving host
// names. Use this flag whenever passing a hostname as opposed to a
// domain name for the hostname parameter.
//
// See LDAP_OPT_AREC_EXCLUSIVE defined in winldap.h for more details.
  {$EXTERNALSYM CERT_LDAP_STORE_AREC_EXCLUSIVE_FLAG}
  CERT_LDAP_STORE_AREC_EXCLUSIVE_FLAG   = $20000;

// Set this flag if the LDAP session handle has already been opened. When
// set, pvPara points to the following CERT_LDAP_STORE_OPENED_PARA structure.
  {$EXTERNALSYM CERT_LDAP_STORE_OPENED_FLAG}
  CERT_LDAP_STORE_OPENED_FLAG           = $40000;

type
  PCertLdapStoreOpenedPara = ^TCertLdapStoreOpenedPara; 
  {$EXTERNALSYM _CERT_LDAP_STORE_OPENED_PARA} 
  _CERT_LDAP_STORE_OPENED_PARA = record 
    pvLdapSessionHandle: Pointer; // The (LDAP *) handle returned by ldap_init
    pwszLdapUrl: LPCWSTR; 
  end; 
  {$EXTERNALSYM CERT_LDAP_STORE_OPENED_PARA} 
  CERT_LDAP_STORE_OPENED_PARA = _CERT_LDAP_STORE_OPENED_PARA; 
  {$EXTERNALSYM PCERT_LDAP_STORE_OPENED_PARA} 
  PCERT_LDAP_STORE_OPENED_PARA = ^_CERT_LDAP_STORE_OPENED_PARA; 
  TCertLdapStoreOpenedPara = _CERT_LDAP_STORE_OPENED_PARA; 

// Set this flag if the above CERT_LDAP_STORE_OPENED_FLAG is set and
// you want an ldap_unbind() of the above pvLdapSessionHandle when the
// store is closed. Note, if CertOpenStore() fails, then, ldap_unbind()
// isn't called.
const
  {$EXTERNALSYM CERT_LDAP_STORE_UNBIND_FLAG}
  CERT_LDAP_STORE_UNBIND_FLAG           = $80000;

//+-------------------------------------------------------------------------
//  Open the cert store using the specified store provider.
//
//  If CERT_STORE_DELETE_FLAG is set, then, the store is deleted. NULL is
//  returned for both success and failure. However, GetLastError() returns 0
//  for success and nonzero for failure.
//
//  If CERT_STORE_SET_LOCALIZED_NAME_FLAG is set, then, if supported, the
//  provider sets the store's CERT_STORE_LOCALIZED_NAME_PROP_ID property.
//  The store's localized name can be retrieved by calling
//  CertSetStoreProperty(dwPropID = CERT_STORE_LOCALIZED_NAME_PROP_ID).
//  This flag is supported by the following providers (and their sz_
//  equivalent):
//      CERT_STORE_PROV_FILENAME_A
//      CERT_STORE_PROV_FILENAME_W
//      CERT_STORE_PROV_SYSTEM_A
//      CERT_STORE_PROV_SYSTEM_W
//      CERT_STORE_PROV_SYSTEM_REGISTRY_A
//      CERT_STORE_PROV_SYSTEM_REGISTRY_W
//      CERT_STORE_PROV_PHYSICAL_W
//
//  If CERT_STORE_DEFER_CLOSE_UNTIL_LAST_FREE_FLAG is set, then, the
//  closing of the store's provider is deferred until all certificate,
//  CRL and CTL contexts obtained from the store are freed. Also,
//  if a non NULL HCRYPTPROV was passed, then, it will continue to be used.
//  By default, the store's provider is closed on the final CertCloseStore.
//  If this flag isn't set, then, any property changes made to previously
//  duplicated contexts after the final CertCloseStore will not be persisted.
//  By setting this flag, property changes made
//  after the CertCloseStore will be persisted. Note, setting this flag
//  causes extra overhead in doing context duplicates and frees.
//  If CertCloseStore is called with CERT_CLOSE_STORE_FORCE_FLAG, then,
//  the CERT_STORE_DEFER_CLOSE_UNTIL_LAST_FREE_FLAG flag is ignored.
//
//  CERT_STORE_MANIFOLD_FLAG can be set to check for certificates having the
//  manifold extension and archive the 'older' certificates with the same
//  manifold extension value. A certificate is archived by setting the
//  CERT_ARCHIVED_PROP_ID.
//
//  By default, contexts having the CERT_ARCHIVED_PROP_ID, are skipped
//  during enumeration. CERT_STORE_ENUM_ARCHIVED_FLAG can be set to include
//  archived contexts when enumerating. Note, contexts having the
//  CERT_ARCHIVED_PROP_ID are still found for explicit finds, such as,
//  finding a context with a specific hash or finding a certificate having
//  a specific issuer and serial number.
//
//  CERT_STORE_UPDATE_KEYID_FLAG can be set to also update the Key Identifier's
//  CERT_KEY_PROV_INFO_PROP_ID property whenever a certificate's
//  CERT_KEY_IDENTIFIER_PROP_ID or CERT_KEY_PROV_INFO_PROP_ID property is set
//  and the other property already exists. If the Key Identifier's
//  CERT_KEY_PROV_INFO_PROP_ID already exists, it isn't updated. Any
//  errors encountered are silently ignored.
//
//  By default, this flag is implicitly set for the 'My\.Default' CurrentUser
//  and LocalMachine physical stores.
//
//  CERT_STORE_READONLY_FLAG can be set to open the store as read only.
//  Otherwise, the store is opened as read/write.
//
//  CERT_STORE_OPEN_EXISTING_FLAG can be set to only open an existing
//  store. CERT_STORE_CREATE_NEW_FLAG can be set to create a new store and
//  fail if the store already exists. Otherwise, the default is to open
//  an existing store or create a new store if it doesn't already exist.
//
//  hCryptProv specifies the crypto provider to use to create the hash
//  properties or verify the signature of a subject certificate or CRL.
//  The store doesn't need to use a private
//  key. If the CERT_STORE_NO_CRYPT_RELEASE_FLAG isn't set, hCryptProv is
//  CryptReleaseContext'ed on the final CertCloseStore.
//
//  Note, if the open fails, hCryptProv is released if it would have been
//  released when the store was closed.
//
//  If hCryptProv is zero, then, the default provider and container for the
//  PROV_RSA_FULL provider type is CryptAcquireContext'ed with
//  CRYPT_VERIFYCONTEXT access. The CryptAcquireContext is deferred until
//  the first create hash or verify signature. In addition, once acquired,
//  the default provider isn't released until process exit when crypt32.dll
//  is unloaded. The acquired default provider is shared across all stores
//  and threads.
//
//  After initializing the store's data structures and optionally acquiring a
//  default crypt provider, CertOpenStore calls CryptGetOIDFunctionAddress to
//  get the address of the CRYPT_OID_OPEN_STORE_PROV_FUNC specified by
//  lpszStoreProvider. Since a store can contain certificates with different
//  encoding types, CryptGetOIDFunctionAddress is called with dwEncodingType
//  set to 0 and not the dwEncodingType passed to CertOpenStore.
//  PFN_CERT_DLL_OPEN_STORE_FUNC specifies the signature of the provider's
//  open function. This provider open function is called to load the
//  store's certificates and CRLs. Optionally, the provider may return an
//  array of functions called before a certificate or CRL is added or deleted
//  or has a property that is set.
//
//  Use of the dwEncodingType parameter is provider dependent. The type
//  definition for pvPara also depends on the provider.
//
//  Store providers are installed or registered via
//  CryptInstallOIDFunctionAddress or CryptRegisterOIDFunction, where,
//  dwEncodingType is 0 and pszFuncName is CRYPT_OID_OPEN_STORE_PROV_FUNC.
//
//  Here's a list of the predefined provider types (implemented in crypt32.dll):
//
//  CERT_STORE_PROV_MSG:
//      Gets the certificates and CRLs from the specified cryptographic message.
//      dwEncodingType contains the message and certificate encoding types.
//      The message's handle is passed in pvPara. Given,
//          HCRYPTMSG hCryptMsg; pvPara = (const void *) hCryptMsg;
//
//  CERT_STORE_PROV_MEMORY
//  sz_CERT_STORE_PROV_MEMORY:
//      Opens a store without any initial certificates or CRLs. pvPara
//      isn't used.
//
//  CERT_STORE_PROV_FILE:
//      Reads the certificates and CRLs from the specified file. The file's
//      handle is passed in pvPara. Given,
//          HANDLE hFile; pvPara = (const void *) hFile;
//
//      For a successful open, the file pointer is advanced past
//      the certificates and CRLs and their properties read from the file.
//      Note, only expects a serialized store and not a file containing
//      either a PKCS #7 signed message or a single encoded certificate.
//
//      The hFile isn't closed.
//
//  CERT_STORE_PROV_REG:
//      Reads the certificates and CRLs from the registry. The registry's
//      key handle is passed in pvPara. Given,
//          HKEY hKey; pvPara = (const void *) hKey;
//
//      The input hKey isn't closed by the provider. Before returning, the
//      provider opens it own copy of the hKey.
//
//      If CERT_STORE_READONLY_FLAG is set, then, the registry subkeys are
//      RegOpenKey'ed with KEY_READ_ACCESS. Otherwise, the registry subkeys
//      are RegCreateKey'ed with KEY_ALL_ACCESS.
//
//      This provider returns the array of functions for reading, writing,
//      deleting and property setting certificates and CRLs.
//      Any changes to the opened store are immediately pushed through to
//      the registry. However, if CERT_STORE_READONLY_FLAG is set, then,
//      writing, deleting or property setting results in a
//      SetLastError(E_ACCESSDENIED).
//
//      Note, all the certificates and CRLs are read from the registry
//      when the store is opened. The opened store serves as a write through
//      cache.
//
//      If CERT_REGISTRY_STORE_SERIALIZED_FLAG is set, then, the
//      contexts are persisted as a single serialized store subkey in the
//      registry.
//
//  CERT_STORE_PROV_PKCS7:
//  sz_CERT_STORE_PROV_PKCS7:
//      Gets the certificates and CRLs from the encoded PKCS #7 signed message.
//      dwEncodingType specifies the message and certificate encoding types.
//      The pointer to the encoded message's blob is passed in pvPara. Given,
//          CRYPT_DATA_BLOB EncodedMsg; pvPara = (const void *) &EncodedMsg;
//
//      Note, also supports the IE3.0 special version of a
//      PKCS #7 signed message referred to as a 'SPC' formatted message.
//
//  CERT_STORE_PROV_SERIALIZED:
//  sz_CERT_STORE_PROV_SERIALIZED:
//      Gets the certificates and CRLs from memory containing a serialized
//      store.  The pointer to the serialized memory blob is passed in pvPara.
//      Given,
//          CRYPT_DATA_BLOB Serialized; pvPara = (const void *) &Serialized;
//
//  CERT_STORE_PROV_FILENAME_A:
//  CERT_STORE_PROV_FILENAME_W:
//  CERT_STORE_PROV_FILENAME:
//  sz_CERT_STORE_PROV_FILENAME_W:
//  sz_CERT_STORE_PROV_FILENAME:
//      Opens the file and first attempts to read as a serialized store. Then,
//      as a PKCS #7 signed message. Finally, as a single encoded certificate.
//      The filename is passed in pvPara. The filename is UNICODE for the
//      '_W' provider and ASCII for the '_A' provider. For '_W': given,
//          LPCWSTR pwszFilename; pvPara = (const void *) pwszFilename;
//      For '_A': given,
//          LPCSTR pszFilename; pvPara = (const void *) pszFilename;
//
//      Note, the default (without '_A' or '_W') is unicode.
//
//      Note, also supports the reading of the IE3.0 special version of a
//      PKCS #7 signed message file referred to as a 'SPC' formatted file.
//
//  CERT_STORE_PROV_SYSTEM_A:
//  CERT_STORE_PROV_SYSTEM_W:
//  CERT_STORE_PROV_SYSTEM:
//  sz_CERT_STORE_PROV_SYSTEM_W:
//  sz_CERT_STORE_PROV_SYSTEM:
//      Opens the specified logical 'System' store. The upper word of the
//      dwFlags parameter is used to specify the location of the system store.
//
//      A 'System' store is a collection consisting of one or more 'Physical'
//      stores. A 'Physical' store is registered via the
//      CertRegisterPhysicalStore API. Each of the registered physical stores
//      is CertStoreOpen'ed and added to the collection via
//      CertAddStoreToCollection.
//
//      The CERT_SYSTEM_STORE_CURRENT_USER, CERT_SYSTEM_STORE_LOCAL_MACHINE,
//      CERT_SYSTEM_STORE_CURRENT_SERVICE, CERT_SYSTEM_STORE_SERVICES,
//      CERT_SYSTEM_STORE_USERS, CERT_SYSTEM_STORE_CURRENT_USER_GROUP_POLICY,
//      CERT_SYSTEM_STORE_LOCAL_MACHINE_GROUP_POLICY and
//      CERT_SYSTEM_STORE_LOCAL_MACHINE_ENTERPRSE
//      system stores by default have a 'SystemRegistry' store that is
//      opened and added to the collection.
//
//      The system store name is passed in pvPara. The name is UNICODE for the
//      '_W' provider and ASCII for the '_A' provider. For '_W': given,
//          LPCWSTR pwszSystemName; pvPara = (const void *) pwszSystemName;
//      For '_A': given,
//          LPCSTR pszSystemName; pvPara = (const void *) pszSystemName;
//
//      Note, the default (without '_A' or '_W') is UNICODE.
//
//      The system store name can't contain any backslashes.
//
//      If CERT_SYSTEM_STORE_RELOCATE_FLAG is set in dwFlags, pvPara
//      points to a CERT_SYSTEM_STORE_RELOCATE_PARA data structure instead
//      of pointing to a null terminated UNICODE or ASCII string.
//      Sibling physical stores are also opened as relocated using
//      pvPara's hKeyBase.
//
//      The CERT_SYSTEM_STORE_SERVICES or CERT_SYSTEM_STORE_USERS system
//      store name must be prefixed with the ServiceName or UserName.
//      For example, 'ServiceName\Trust'.
//
//      Stores on remote computers can be accessed for the
//      CERT_SYSTEM_STORE_LOCAL_MACHINE, CERT_SYSTEM_STORE_SERVICES,
//      CERT_SYSTEM_STORE_USERS, CERT_SYSTEM_STORE_LOCAL_MACHINE_GROUP_POLICY
//      or CERT_SYSTEM_STORE_LOCAL_MACHINE_ENTERPRISE
//      locations by prepending the computer name. For example, a remote
//      local machine store is accessed via '\\ComputerName\Trust' or
//      'ComputerName\Trust'. A remote service store is accessed via
//      '\\ComputerName\ServiceName\Trust'. The leading '\\' backslashes are
//      optional in the ComputerName.
//
//      If CERT_STORE_READONLY_FLAG is set, then, the registry is
//      RegOpenKey'ed with KEY_READ_ACCESS. Otherwise, the registry is
//      RegCreateKey'ed with KEY_ALL_ACCESS.
//
//      The 'root' store is treated differently from the other system
//      stores. Before a certificate is added to or deleted from the 'root'
//      store, a pop up message box is displayed. The certificate's subject,
//      issuer, serial number, time validity, sha1 and md5 thumbprints are
//      displayed. The user is given the option to do the add or delete.
//      If they don't allow the operation, LastError is set to E_ACCESSDENIED.
//
//  CERT_STORE_PROV_SYSTEM_REGISTRY_A
//  CERT_STORE_PROV_SYSTEM_REGISTRY_W
//  CERT_STORE_PROV_SYSTEM_REGISTRY
//  sz_CERT_STORE_PROV_SYSTEM_REGISTRY_W
//  sz_CERT_STORE_PROV_SYSTEM_REGISTRY
//      Opens the 'System' store's default 'Physical' store residing in the
//      registry. The upper word of the dwFlags
//      parameter is used to specify the location of the system store.
//
//      After opening the registry key associated with the system name,
//      the CERT_STORE_PROV_REG provider is called to complete the open.
//
//      The system store name is passed in pvPara. The name is UNICODE for the
//      '_W' provider and ASCII for the '_A' provider. For '_W': given,
//          LPCWSTR pwszSystemName; pvPara = (const void *) pwszSystemName;
//      For '_A': given,
//          LPCSTR pszSystemName; pvPara = (const void *) pszSystemName;
//
//      Note, the default (without '_A' or '_W') is UNICODE.
//
//      If CERT_SYSTEM_STORE_RELOCATE_FLAG is set in dwFlags, pvPara
//      points to a CERT_SYSTEM_STORE_RELOCATE_PARA data structure instead
//      of pointing to a null terminated UNICODE or ASCII string.
//
//      See above for details on prepending a ServiceName and/or ComputerName
//      to the store name.
//
//      If CERT_STORE_READONLY_FLAG is set, then, the registry is
//      RegOpenKey'ed with KEY_READ_ACCESS. Otherwise, the registry is
//      RegCreateKey'ed with KEY_ALL_ACCESS.
//
//      The 'root' store is treated differently from the other system
//      stores. Before a certificate is added to or deleted from the 'root'
//      store, a pop up message box is displayed. The certificate's subject,
//      issuer, serial number, time validity, sha1 and md5 thumbprints are
//      displayed. The user is given the option to do the add or delete.
//      If they don't allow the operation, LastError is set to E_ACCESSDENIED.
//
//  CERT_STORE_PROV_PHYSICAL_W
//  CERT_STORE_PROV_PHYSICAL
//  sz_CERT_STORE_PROV_PHYSICAL_W
//  sz_CERT_STORE_PROV_PHYSICAL
//      Opens the specified 'Physical' store in the 'System' store.
//
//      Both the system store and physical names are passed in pvPara. The
//      names are separated with an intervening '\'. For example,
//      'Root\.Default'. The string is UNICODE.
//
//      The system and physical store names can't contain any backslashes.
//
//      If CERT_SYSTEM_STORE_RELOCATE_FLAG is set in dwFlags, pvPara
//      points to a CERT_SYSTEM_STORE_RELOCATE_PARA data structure instead
//      of pointing to a null terminated UNICODE string.
//      The specified physical store is opened as relocated using pvPara's
//      hKeyBase.
//
//      For CERT_SYSTEM_STORE_SERVICES or CERT_SYSTEM_STORE_USERS,
//      the system and physical store names
//      must be prefixed with the ServiceName or UserName. For example,
//      'ServiceName\Root\.Default'.
//
//      Physical stores on remote computers can be accessed for the
//      CERT_SYSTEM_STORE_LOCAL_MACHINE, CERT_SYSTEM_STORE_SERVICES,
//      CERT_SYSTEM_STORE_USERS, CERT_SYSTEM_STORE_LOCAL_MACHINE_GROUP_POLICY
//      or CERT_SYSTEM_STORE_LOCAL_MACHINE_ENTERPRISE
//      locations by prepending the computer name. For example, a remote
//      local machine store is accessed via '\\ComputerName\Root\.Default'
//      or 'ComputerName\Root\.Default'. A remote service store is
//      accessed via '\\ComputerName\ServiceName\Root\.Default'. The
//      leading '\\' backslashes are optional in the ComputerName.
//
//  CERT_STORE_PROV_COLLECTION
//  sz_CERT_STORE_PROV_COLLECTION
//      Opens a store that is a collection of other stores. Stores are
//      added or removed to/from the collection via the CertAddStoreToCollection
//      and CertRemoveStoreFromCollection APIs.
//
//  CERT_STORE_PROV_SMART_CARD_W
//  CERT_STORE_PROV_SMART_CARD
//  sz_CERT_STORE_PROV_SMART_CARD_W
//  sz_CERT_STORE_PROV_SMART_CARD
//      Opens a store instantiated over a particular smart card storage.  pvPara
//      identifies where on the card the store is located and is of the
//      following format:
//
//                Card Name\Provider Name\Provider Type[\Container Name]
//
//      Container Name is optional and if NOT specified the Card Name is used
//      as the Container Name.  Future versions of the provider will support
//      instantiating the store over the entire card in which case just
//      Card Name ( or id ) will be sufficient.
//
//  Here's a list of the predefined provider types (implemented in
//  cryptnet.dll):
//
//  CERT_STORE_PROV_LDAP_W
//  CERT_STORE_PROV_LDAP
//  sz_CERT_STORE_PROV_LDAP_W
//  sz_CERT_STORE_PROV_LDAP
//      Opens a store over the results of the query specified by and LDAP
//      URL which is passed in via pvPara.  In order to do writes to the
//      store the URL must specify a BASE query, no filter and a single
//      attribute.
//
//--------------------------------------------------------------------------
{$EXTERNALSYM CertOpenStore}
function CertOpenStore(lpszStoreProvider: LPCSTR; dwEncodingType: DWORD;
  hCryptProv: HCRYPTPROV_LEGACY; dwFlags: DWORD;
  pvPara: Pointer): HCERTSTORE; stdcall;

//+-------------------------------------------------------------------------
//  OID Installable Certificate Store Provider Data Structures
//--------------------------------------------------------------------------

// Handle returned by the store provider when opened.
type
  {$EXTERNALSYM HCERTSTOREPROV}
  HCERTSTOREPROV = Pointer; 

// Store Provider OID function's pszFuncName.
const
  {$EXTERNALSYM CRYPT_OID_OPEN_STORE_PROV_FUNC}
  CRYPT_OID_OPEN_STORE_PROV_FUNC   = 'CertDllOpenStoreProv';

// Note, the Store Provider OID function's dwEncodingType is always 0.

// The following information is returned by the provider when opened. Its
// zeroed with cbSize set before the provider is called. If the provider
// doesn't need to be called again after the open it doesn't need to
// make any updates to the CERT_STORE_PROV_INFO.
type
  PCertStoreProvInfo = ^TCertStoreProvInfo;
  {$EXTERNALSYM _CERT_STORE_PROV_INFO}
  _CERT_STORE_PROV_INFO = record
    cbSize: DWORD;
    cStoreProvFunc: DWORD;
    rgpvStoreProvFunc: PPointer;
    hStoreProv: HCERTSTOREPROV;
    dwStoreProvFlags: DWORD;
    hStoreProvFuncAddr2: HCRYPTOIDFUNCADDR;
  end;
  {$EXTERNALSYM CERT_STORE_PROV_INFO}
  CERT_STORE_PROV_INFO = _CERT_STORE_PROV_INFO;
  {$EXTERNALSYM PCERT_STORE_PROV_INFO}
  PCERT_STORE_PROV_INFO = ^_CERT_STORE_PROV_INFO;
  TCertStoreProvInfo = _CERT_STORE_PROV_INFO;

// Definition of the store provider's open function.
//
// *pStoreProvInfo has been zeroed before the call.
//
// Note, pStoreProvInfo->cStoreProvFunc should be set last.  Once set,
// all subsequent store calls, such as CertAddSerializedElementToStore will
// call the appropriate provider callback function.
type
  PFN_CERT_DLL_OPEN_STORE_PROV_FUNC = function(lpszStoreProvider: LPCSTR;
    dwEncodingType: DWORD; hCryptProv: HCRYPTPROV_LEGACY;
    dwFlags: DWORD; pvPara: Pointer; hCertStore: HCERTSTORE;
    out pStoreProvInfo: CERT_STORE_PROV_INFO): BOOL stdcall;
  TFnCertDLLOpenStoreProvFunc = PFN_CERT_DLL_OPEN_STORE_PROV_FUNC;

const
// The open callback sets the following flag, if it maintains its
// contexts externally and not in the cached store.
  {$EXTERNALSYM CERT_STORE_PROV_EXTERNAL_FLAG}
  CERT_STORE_PROV_EXTERNAL_FLAG           = $1;

// The open callback sets the following flag for a successful delete.
// When set, the close callback isn't called.
  {$EXTERNALSYM CERT_STORE_PROV_DELETED_FLAG}
  CERT_STORE_PROV_DELETED_FLAG            = $2;

// The open callback sets the following flag if it doesn't persist store
// changes.
  {$EXTERNALSYM CERT_STORE_PROV_NO_PERSIST_FLAG}
  CERT_STORE_PROV_NO_PERSIST_FLAG         = $4;

// The open callback sets the following flag if the contexts are persisted
// to a system store.
  {$EXTERNALSYM CERT_STORE_PROV_SYSTEM_STORE_FLAG}
  CERT_STORE_PROV_SYSTEM_STORE_FLAG       = $8;

// The open callback sets the following flag if the contexts are persisted
// to a LocalMachine system store.
  {$EXTERNALSYM CERT_STORE_PROV_LM_SYSTEM_STORE_FLAG}
  CERT_STORE_PROV_LM_SYSTEM_STORE_FLAG    = $10;

// The open callback sets the following flag if the contexts are persisted
// to a GroupPolicy system store.
  {$EXTERNALSYM CERT_STORE_PROV_GP_SYSTEM_STORE_FLAG}
  CERT_STORE_PROV_GP_SYSTEM_STORE_FLAG    = $20;

// Indices into the store provider's array of callback functions.
//
// The provider can implement any subset of the following functions. It
// sets pStoreProvInfo->cStoreProvFunc to the last index + 1 and any
// preceding not implemented functions to NULL.
  {$EXTERNALSYM CERT_STORE_PROV_CLOSE_FUNC}
  CERT_STORE_PROV_CLOSE_FUNC              = 0;
  {$EXTERNALSYM CERT_STORE_PROV_READ_CERT_FUNC}
  CERT_STORE_PROV_READ_CERT_FUNC          = 1;
  {$EXTERNALSYM CERT_STORE_PROV_WRITE_CERT_FUNC}
  CERT_STORE_PROV_WRITE_CERT_FUNC         = 2;
  {$EXTERNALSYM CERT_STORE_PROV_DELETE_CERT_FUNC}
  CERT_STORE_PROV_DELETE_CERT_FUNC        = 3;
  {$EXTERNALSYM CERT_STORE_PROV_SET_CERT_PROPERTY_FUNC}
  CERT_STORE_PROV_SET_CERT_PROPERTY_FUNC  = 4;
  {$EXTERNALSYM CERT_STORE_PROV_READ_CRL_FUNC}
  CERT_STORE_PROV_READ_CRL_FUNC           = 5;
  {$EXTERNALSYM CERT_STORE_PROV_WRITE_CRL_FUNC}
  CERT_STORE_PROV_WRITE_CRL_FUNC          = 6;
  {$EXTERNALSYM CERT_STORE_PROV_DELETE_CRL_FUNC}
  CERT_STORE_PROV_DELETE_CRL_FUNC         = 7;
  {$EXTERNALSYM CERT_STORE_PROV_SET_CRL_PROPERTY_FUNC}
  CERT_STORE_PROV_SET_CRL_PROPERTY_FUNC   = 8;
  {$EXTERNALSYM CERT_STORE_PROV_READ_CTL_FUNC}
  CERT_STORE_PROV_READ_CTL_FUNC           = 9;
  {$EXTERNALSYM CERT_STORE_PROV_WRITE_CTL_FUNC}
  CERT_STORE_PROV_WRITE_CTL_FUNC          = 10;
  {$EXTERNALSYM CERT_STORE_PROV_DELETE_CTL_FUNC}
  CERT_STORE_PROV_DELETE_CTL_FUNC         = 11;
  {$EXTERNALSYM CERT_STORE_PROV_SET_CTL_PROPERTY_FUNC}
  CERT_STORE_PROV_SET_CTL_PROPERTY_FUNC   = 12;
  {$EXTERNALSYM CERT_STORE_PROV_CONTROL_FUNC}
  CERT_STORE_PROV_CONTROL_FUNC            = 13;
  {$EXTERNALSYM CERT_STORE_PROV_FIND_CERT_FUNC}
  CERT_STORE_PROV_FIND_CERT_FUNC          = 14;
  {$EXTERNALSYM CERT_STORE_PROV_FREE_FIND_CERT_FUNC}
  CERT_STORE_PROV_FREE_FIND_CERT_FUNC     = 15;
  {$EXTERNALSYM CERT_STORE_PROV_GET_CERT_PROPERTY_FUNC}
  CERT_STORE_PROV_GET_CERT_PROPERTY_FUNC  = 16;
  {$EXTERNALSYM CERT_STORE_PROV_FIND_CRL_FUNC}
  CERT_STORE_PROV_FIND_CRL_FUNC           = 17;
  {$EXTERNALSYM CERT_STORE_PROV_FREE_FIND_CRL_FUNC}
  CERT_STORE_PROV_FREE_FIND_CRL_FUNC      = 18;
  {$EXTERNALSYM CERT_STORE_PROV_GET_CRL_PROPERTY_FUNC}
  CERT_STORE_PROV_GET_CRL_PROPERTY_FUNC   = 19;
  {$EXTERNALSYM CERT_STORE_PROV_FIND_CTL_FUNC}
  CERT_STORE_PROV_FIND_CTL_FUNC           = 20;
  {$EXTERNALSYM CERT_STORE_PROV_FREE_FIND_CTL_FUNC}
  CERT_STORE_PROV_FREE_FIND_CTL_FUNC      = 21;
  {$EXTERNALSYM CERT_STORE_PROV_GET_CTL_PROPERTY_FUNC}
  CERT_STORE_PROV_GET_CTL_PROPERTY_FUNC   = 22;

// Called by CertCloseStore when the store's reference count is
// decremented to 0.
type
  {$EXTERNALSYM PFN_CERT_STORE_PROV_CLOSE}
  PFN_CERT_STORE_PROV_CLOSE = procedure(hStoreProv: HCERTSTOREPROV;
    dwFlags: DWORD) stdcall;

// Currently not called directly by the store APIs. However, may be exported
// to support other providers based on it.
//
// Reads the provider's copy of the certificate context. If it exists,
// creates a new certificate context.
  {$EXTERNALSYM PFN_CERT_STORE_PROV_READ_CERT}
  PFN_CERT_STORE_PROV_READ_CERT = function(hStoreProv: HCERTSTOREPROV;
    var pStoreCertContext: CERT_CONTEXT; dwFlags: DWORD;
    out ppProvCertContext: PCCERT_CONTEXT): BOOL stdcall;
  TFnCertStoreProvReadCert = PFN_CERT_STORE_PROV_READ_CERT;

const
  {$EXTERNALSYM CERT_STORE_PROV_WRITE_ADD_FLAG}
  CERT_STORE_PROV_WRITE_ADD_FLAG = $1;

type
// Called by CertAddEncodedCertificateToStore,
// CertAddCertificateContextToStore or CertAddSerializedElementToStore before
// adding to the store. The CERT_STORE_PROV_WRITE_ADD_FLAG is set. In
// addition to the encoded certificate, the added pCertContext might also
// have properties.
//
// Returns TRUE if its OK to update the the store.
  {$EXTERNALSYM PFN_CERT_STORE_PROV_WRITE_CERT}
  PFN_CERT_STORE_PROV_WRITE_CERT = function(hStoreProv: HCERTSTOREPROV;
    var pCertContext: CERT_CONTEXT; dwFlags: DWORD): BOOL stdcall;
  TFnCertStoreProvWriteCert = PFN_CERT_STORE_PROV_WRITE_CERT;

// Called by CertDeleteCertificateFromStore before deleting from the
// store.
//
// Returns TRUE if its OK to delete from the store.
  {$EXTERNALSYM PFN_CERT_STORE_PROV_DELETE_CERT}
  PFN_CERT_STORE_PROV_DELETE_CERT = function(hStoreProv: HCERTSTOREPROV;
    var pCertContext: CERT_CONTEXT; dwFlags: DWORD): BOOL stdcall;
  TFnCertStoreProvDeleteCert = PFN_CERT_STORE_PROV_DELETE_CERT;

// Called by CertSetCertificateContextProperty before setting the
// certificate's property. Also called by CertGetCertificateContextProperty,
// when getting a hash property that needs to be created and then persisted
// via the set.
//
// Upon input, the property hasn't been set for the pCertContext parameter.
//
// Returns TRUE if its OK to set the property.
  {$EXTERNALSYM PFN_CERT_STORE_PROV_SET_CERT_PROPERTY}
  PFN_CERT_STORE_PROV_SET_CERT_PROPERTY = function(hStoreProv: HCERTSTOREPROV;
    var pCertContext: CERT_CONTEXT; dwPropId, dwFlags: DWORD;
    pvData: Pointer): BOOL stdcall;
  TFnCertzStoreProvSetCertProperty = PFN_CERT_STORE_PROV_SET_CERT_PROPERTY;

// Currently not called directly by the store APIs. However, may be exported
// to support other providers based on it.
//
// Reads the provider's copy of the CRL context. If it exists,
// creates a new CRL context.
  {$EXTERNALSYM PFN_CERT_STORE_PROV_READ_CRL}
  PFN_CERT_STORE_PROV_READ_CRL = function(hStoreProv: HCERTSTOREPROV;
    pStoreCrlContext: PCCRL_CONTEXT; dwFlags: DWORD;
    out ppProvCrlContext: PCCRL_CONTEXT): BOOL stdcall;
  TFnCertStoreProvReadCRL = PFN_CERT_STORE_PROV_READ_CRL;

// Called by CertAddEncodedCRLToStore,
// CertAddCRLContextToStore or CertAddSerializedElementToStore before
// adding to the store. The CERT_STORE_PROV_WRITE_ADD_FLAG is set. In
// addition to the encoded CRL, the added pCertContext might also
// have properties.
//
// Returns TRUE if its OK to update the the store.
  {$EXTERNALSYM PFN_CERT_STORE_PROV_WRITE_CRL}
  PFN_CERT_STORE_PROV_WRITE_CRL = function(hStoreProv: HCERTSTOREPROV;
    pCrlContext: PCCRL_CONTEXT; dwFlags: DWORD): BOOL stdcall;
  TFnCertStoreProvWriteCRL = PFN_CERT_STORE_PROV_WRITE_CRL;

// Called by CertDeleteCRLFromStore before deleting from the store.
//
// Returns TRUE if its OK to delete from the store.
  {$EXTERNALSYM PFN_CERT_STORE_PROV_DELETE_CRL}
  PFN_CERT_STORE_PROV_DELETE_CRL = function(hStoreProv: HCERTSTOREPROV;
    pCrlContext: PCCRL_CONTEXT; dwFlags: DWORD): BOOL stdcall;
  TFnCertStoreProvDeleteCRL = PFN_CERT_STORE_PROV_DELETE_CRL;

// Called by CertSetCRLContextProperty before setting the
// CRL's property. Also called by CertGetCRLContextProperty,
// when getting a hash property that needs to be created and then persisted
// via the set.
//
// Upon input, the property hasn't been set for the pCrlContext parameter.
//
// Returns TRUE if its OK to set the property.
  {$EXTERNALSYM PFN_CERT_STORE_PROV_SET_CRL_PROPERTY}
  PFN_CERT_STORE_PROV_SET_CRL_PROPERTY = function(hStoreProv: HCERTSTOREPROV;
    pCrlContext: PCCRL_CONTEXT; dwPropId, dwFlags: DWORD;
    pvData: Pointer): BOOL stdcall;
  TFnCertStoreProvSetCRLProperty = PFN_CERT_STORE_PROV_SET_CRL_PROPERTY;

// Currently not called directly by the store APIs. However, may be exported
// to support other providers based on it.
//
// Reads the provider's copy of the CTL context. If it exists,
// creates a new CTL context.
  {$EXTERNALSYM PFN_CERT_STORE_PROV_READ_CTL}
  PFN_CERT_STORE_PROV_READ_CTL = function(hStoreProv: HCERTSTOREPROV;
    pStoreCtlContext: PCCTL_CONTEXT; dwFlags: DWORD;
    out ppProvCtlContext: PCCTL_CONTEXT): BOOL stdcall;
  TFnCertStoreProvReadCtl = PFN_CERT_STORE_PROV_READ_CTL;

// Called by CertAddEncodedCTLToStore,
// CertAddCTLContextToStore or CertAddSerializedElementToStore before
// adding to the store. The CERT_STORE_PROV_WRITE_ADD_FLAG is set. In
// addition to the encoded CTL, the added pCertContext might also
// have properties.
//
// Returns TRUE if its OK to update the the store.
 {$EXTERNALSYM PFN_CERT_STORE_PROV_WRITE_CTL}
  PFN_CERT_STORE_PROV_WRITE_CTL = function(hStoreProv: HCERTSTOREPROV;
    pCtlContext: PCCTL_CONTEXT; dwFlags: DWORD): BOOL stdcall;
  TFnCertStoreProvWriteCtl = PFN_CERT_STORE_PROV_WRITE_CTL;

// Called by CertDeleteCTLFromStore before deleting from the store.
//
// Returns TRUE if its OK to delete from the store.
  {$EXTERNALSYM PFN_CERT_STORE_PROV_DELETE_CTL}
  PFN_CERT_STORE_PROV_DELETE_CTL = function(hStoreProv: HCERTSTOREPROV;
    pCtlContext: PCCTL_CONTEXT; dwFlags: DWORD): BOOL stdcall;
  TFnCertStoreProvDeleteCtl = PFN_CERT_STORE_PROV_DELETE_CTL;

// Called by CertSetCTLContextProperty before setting the
// CTL's property. Also called by CertGetCTLContextProperty,
// when getting a hash property that needs to be created and then persisted
// via the set.
//
// Upon input, the property hasn't been set for the pCtlContext parameter.
//
// Returns TRUE if its OK to set the property.
  {$EXTERNALSYM PFN_CERT_STORE_PROV_SET_CTL_PROPERTY}
  PFN_CERT_STORE_PROV_SET_CTL_PROPERTY = function(hStoreProv: HCERTSTOREPROV;
    pCtlContext: PCCTL_CONTEXT; dwPropId, dwFlags: DWORD;
    pvData: Pointer): BOOL stdcall;
  TFnCertStoreProvSetCtlProperty = PFN_CERT_STORE_PROV_SET_CTL_PROPERTY;

  {$EXTERNALSYM PFN_CERT_STORE_PROV_CONTROL}
  PFN_CERT_STORE_PROV_CONTROL = function(hStoreProv: HCERTSTOREPROV;
    dwFlags: DWORD; dwCtrlType: DWORD; pvCtrlPara: Pointer): BOOL stdcall;
  TFnCertStoreProvControl = PFN_CERT_STORE_PROV_CONTROL;

type
  PCertStoreProvFindInfo = ^TCertStoreProvFindInfo; 
  {$EXTERNALSYM _CERT_STORE_PROV_FIND_INFO} 
  _CERT_STORE_PROV_FIND_INFO = record 
    cbSize: DWORD; 
    dwMsgAndCertEncodingType: DWORD; 
    dwFindFlags: DWORD; 
    dwFindType: DWORD; 
    pvFindPara: Pointer; 
  end; 
  {$EXTERNALSYM CERT_STORE_PROV_FIND_INFO} 
  CERT_STORE_PROV_FIND_INFO = _CERT_STORE_PROV_FIND_INFO; 
  {$EXTERNALSYM PCERT_STORE_PROV_FIND_INFO} 
  PCERT_STORE_PROV_FIND_INFO = ^_CERT_STORE_PROV_FIND_INFO; 
  TCertStoreProvFindInfo = _CERT_STORE_PROV_FIND_INFO;
  {$EXTERNALSYM CCERT_STORE_PROV_FIND_INFO}
  CCERT_STORE_PROV_FIND_INFO = CERT_STORE_PROV_FIND_INFO; 
  {$EXTERNALSYM PCCERT_STORE_PROV_FIND_INFO} 
  PCCERT_STORE_PROV_FIND_INFO = ^CERT_STORE_PROV_FIND_INFO; 

  {$EXTERNALSYM PFN_CERT_STORE_PROV_FIND_CERT}
  PFN_CERT_STORE_PROV_FIND_CERT = function(hStoreProv: HCERTSTOREPROV;
    var pFindInfo: CERT_STORE_PROV_FIND_INFO;
    var pPrevCertContext: CERT_CONTEXT; dwFlags: DWORD;
    var ppvStoreProvFindInfo: Pointer;
    out ppProvCertContext: PCCERT_CONTEXT): BOOL stdcall;
  TFnCertStoreProvFindCert = PFN_CERT_STORE_PROV_FIND_CERT;

  {$EXTERNALSYM PFN_CERT_STORE_PROV_FREE_FIND_CERT}
  PFN_CERT_STORE_PROV_FREE_FIND_CERT = function(hStoreProv: HCERTSTOREPROV;
    var pCertContext: CERT_CONTEXT; pvStoreProvFindInfo: Pointer;
    dwFlags: DWORD): BOOL stdcall;
  TFnCertStoreProvFreeFindCert = PFN_CERT_STORE_PROV_FREE_FIND_CERT;

  {$EXTERNALSYM PFN_CERT_STORE_PROV_GET_CERT_PROPERTY}
  PFN_CERT_STORE_PROV_GET_CERT_PROPERTY = function(hStoreProv: HCERTSTOREPROV;
    var pCertContext: CERT_CONTEXT; dwPropId, dwFlags: DWORD;
    pvData: Pointer; out pcbData: DWORD): BOOL stdcall;
  TFnCertStoreProvGetCertProperty = PFN_CERT_STORE_PROV_GET_CERT_PROPERTY;

  {$EXTERNALSYM PFN_CERT_STORE_PROV_FIND_CRL}
  PFN_CERT_STORE_PROV_FIND_CRL = function(hStoreProv: HCERTSTOREPROV;
    var pFindInfo: CERT_STORE_PROV_FIND_INFO; var pPrevCrlContext: CRL_CONTEXT;
    dwFlags: DWORD; var ppvStoreProvFindInfo: Pointer;
    out ppProvCrlContext: PCCRL_CONTEXT): BOOL stdcall;
  TFnCertStoreProvFindCRL = PFN_CERT_STORE_PROV_FIND_CRL;

  {$EXTERNALSYM PFN_CERT_STORE_PROV_FREE_FIND_CRL}
  PFN_CERT_STORE_PROV_FREE_FIND_CRL = function(hStoreProv: HCERTSTOREPROV;
    var pCrlContext: CRL_CONTEXT; pvStoreProvFindInfo: Pointer;
    dwFlags: DWORD): BOOL stdcall;
  TFnCertStoreProvFreeFindCRL = PFN_CERT_STORE_PROV_FREE_FIND_CRL;

  {$EXTERNALSYM PFN_CERT_STORE_PROV_GET_CRL_PROPERTY}
  PFN_CERT_STORE_PROV_GET_CRL_PROPERTY = function(hStoreProv: HCERTSTOREPROV;
    var pCrlContext: CRL_CONTEXT; dwPropId, dwFlags: DWORD;
    pvData: Pointer; out pcbData: DWORD): BOOL stdcall;
  TFnCertStoreProvGetCRLProperty = PFN_CERT_STORE_PROV_GET_CRL_PROPERTY;

  {$EXTERNALSYM PFN_CERT_STORE_PROV_FIND_CTL}
  PFN_CERT_STORE_PROV_FIND_CTL = function(hStoreProv: HCERTSTOREPROV;
    var pFindInfo: CERT_STORE_PROV_FIND_INFO; var pPrevCtlContext: CTL_CONTEXT;
    dwFlags: DWORD; var ppvStoreProvFindInfo: Pointer;
    out ppProvCtlContext: PCCTL_CONTEXT): BOOL stdcall;
  TFnCertStoreProvFindCtl = PFN_CERT_STORE_PROV_FIND_CTL;

  {$EXTERNALSYM PFN_CERT_STORE_PROV_FREE_FIND_CTL}
  PFN_CERT_STORE_PROV_FREE_FIND_CTL = function(hStoreProv: HCERTSTOREPROV;
    var pCtlContext: CTL_CONTEXT; pvStoreProvFindInfo: Pointer;
    dwFlags: DWORD): BOOL stdcall;
  TFnCertStoreProvFreeFindCtl = PFN_CERT_STORE_PROV_FREE_FIND_CTL;

  {$EXTERNALSYM PFN_CERT_STORE_PROV_GET_CTL_PROPERTY}
  PFN_CERT_STORE_PROV_GET_CTL_PROPERTY = function(hStoreProv: HCERTSTOREPROV;
    var pCtlContext: CTL_CONTEXT; dwPropId: DWORD; dwFlags: DWORD;
    pvData: Pointer; out pcbData: DWORD): BOOL stdcall;
  TFnCertStoreProvGetCtlProperty = PFN_CERT_STORE_PROV_GET_CTL_PROPERTY;

//+-------------------------------------------------------------------------
//  Duplicate a cert store handle
//--------------------------------------------------------------------------
{$EXTERNALSYM CertDuplicateStore}
function CertDuplicateStore(hCertStore: HCERTSTORE): HCERTSTORE; stdcall;

const
  {$EXTERNALSYM CERT_STORE_SAVE_AS_STORE}
  CERT_STORE_SAVE_AS_STORE        = 1;
  {$EXTERNALSYM CERT_STORE_SAVE_AS_PKCS7}
  CERT_STORE_SAVE_AS_PKCS7        = 2;
  {$EXTERNALSYM CERT_STORE_SAVE_AS_PKCS12}
  CERT_STORE_SAVE_AS_PKCS12       = 3;

  {$EXTERNALSYM CERT_STORE_SAVE_TO_FILE}
  CERT_STORE_SAVE_TO_FILE         = 1;
  {$EXTERNALSYM CERT_STORE_SAVE_TO_MEMORY}
  CERT_STORE_SAVE_TO_MEMORY       = 2;
  {$EXTERNALSYM CERT_STORE_SAVE_TO_FILENAME_A}
  CERT_STORE_SAVE_TO_FILENAME_A   = 3;
  {$EXTERNALSYM CERT_STORE_SAVE_TO_FILENAME_W}
  CERT_STORE_SAVE_TO_FILENAME_W   = 4;
  {$EXTERNALSYM CERT_STORE_SAVE_TO_FILENAME}
  CERT_STORE_SAVE_TO_FILENAME     = CERT_STORE_SAVE_TO_FILENAME_W;

//+-------------------------------------------------------------------------
//  Save the cert store. Extended version with lots of options.
//
//  According to the dwSaveAs parameter, the store can be saved as a
//  serialized store (CERT_STORE_SAVE_AS_STORE) containing properties in
//  addition to encoded certificates, CRLs and CTLs or the store can be saved
//  as a PKCS #7 signed message (CERT_STORE_SAVE_AS_PKCS7) which doesn't
//  include the properties or CTLs.
//
//  Note, the CERT_KEY_CONTEXT_PROP_ID property (and its
//  CERT_KEY_PROV_HANDLE_PROP_ID or CERT_KEY_SPEC_PROP_ID) isn't saved into
//  a serialized store.
//
//  For CERT_STORE_SAVE_AS_PKCS7, the dwEncodingType specifies the message
//  encoding type. The dwEncodingType parameter isn't used for
//  CERT_STORE_SAVE_AS_STORE.
//
//  The dwFlags parameter currently isn't used and should be set to 0.
//
//  The dwSaveTo and pvSaveToPara parameters specify where to save the
//  store as follows:
//    CERT_STORE_SAVE_TO_FILE:
//      Saves to the specified file. The file's handle is passed in
//      pvSaveToPara. Given,
//          HANDLE hFile; pvSaveToPara = (void *) hFile;
//
//      For a successful save, the file pointer is positioned after the
//      last write.
//
//    CERT_STORE_SAVE_TO_MEMORY:
//      Saves to the specified memory blob. The pointer to
//      the memory blob is passed in pvSaveToPara. Given,
//          CRYPT_DATA_BLOB SaveBlob; pvSaveToPara = (void *) &SaveBlob;
//      Upon entry, the SaveBlob's pbData and cbData need to be initialized.
//      Upon return, cbData is updated with the actual length.
//      For a length only calculation, pbData should be set to NULL. If
//      pbData is non-NULL and cbData isn't large enough, FALSE is returned
//      with a last error of ERRROR_MORE_DATA.
//
//    CERT_STORE_SAVE_TO_FILENAME_A:
//    CERT_STORE_SAVE_TO_FILENAME_W:
//    CERT_STORE_SAVE_TO_FILENAME:
//      Opens the file and saves to it. The filename is passed in pvSaveToPara.
//      The filename is UNICODE for the '_W' option and ASCII for the '_A'
//      option. For '_W': given,
//          LPCWSTR pwszFilename; pvSaveToPara = (void *) pwszFilename;
//      For '_A': given,
//          LPCSTR pszFilename; pvSaveToPara = (void *) pszFilename;
//
//      Note, the default (without '_A' or '_W') is UNICODE.
//
//--------------------------------------------------------------------------
{$EXTERNALSYM CertSaveStore}
function CertSaveStore(hCertStore: HCERTSTORE;
  dwEncodingType, dwSaveAs, dwSaveTo: DWORD; pvSaveToPara: Pointer;
  dwFlags: DWORD): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Certificate Store close flags
//--------------------------------------------------------------------------
const
  {$EXTERNALSYM CERT_CLOSE_STORE_FORCE_FLAG}
  CERT_CLOSE_STORE_FORCE_FLAG = $00000001;
  {$EXTERNALSYM CERT_CLOSE_STORE_CHECK_FLAG}
  CERT_CLOSE_STORE_CHECK_FLAG = $00000002;

//+-------------------------------------------------------------------------
//  Close a cert store handle.
//
//  There needs to be a corresponding close for each open and duplicate.
//
//  Even on the final close, the cert store isn't freed until all of its
//  certificate and CRL contexts have also been freed.
//
//  On the final close, the hCryptProv passed to CertStoreOpen is
//  CryptReleaseContext'ed.
//
//  To force the closure of the store with all of its memory freed, set the
//  CERT_STORE_CLOSE_FORCE_FLAG. This flag should be set when the caller does
//  its own reference counting and wants everything to vanish.
//
//  To check if all the store's certificates and CRLs have been freed and that
//  this is the last CertCloseStore, set the CERT_CLOSE_STORE_CHECK_FLAG. If
//  set and certs, CRLs or stores still need to be freed/closed, FALSE is
//  returned with LastError set to CRYPT_E_PENDING_CLOSE. Note, for FALSE,
//  the store is still closed. This is a diagnostic flag.
//
//  LastError is preserved unless CERT_CLOSE_STORE_CHECK_FLAG is set and FALSE
//  is returned.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertCloseStore}
function CertCloseStore(hCertStore: HCERTSTORE; dwFlags: DWORD): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Get the subject certificate context uniquely identified by its Issuer and
//  SerialNumber from the store.
//
//  If the certificate isn't found, NULL is returned. Otherwise, a pointer to
//  a read only CERT_CONTEXT is returned. CERT_CONTEXT must be freed by calling
//  CertFreeCertificateContext. CertDuplicateCertificateContext can be called to make a
//  duplicate.
//
//  The returned certificate might not be valid. Normally, it would be
//  verified when getting its issuer certificate (CertGetIssuerCertificateFromStore).
//--------------------------------------------------------------------------
{$EXTERNALSYM CertGetSubjectCertificateFromStore}
function CertGetSubjectCertificateFromStore(hCertStore: HCERTSTORE;
  dwCertEncodingType: DWORD;
  var pCertId: CERT_INFO           // Only the Issuer and SerialNumber
                                   // fields are used
  ): PCCERT_CONTEXT; stdcall;

//+-------------------------------------------------------------------------
//  Enumerate the certificate contexts in the store.
//
//  If a certificate isn't found, NULL is returned.
//  Otherwise, a pointer to a read only CERT_CONTEXT is returned. CERT_CONTEXT
//  must be freed by calling CertFreeCertificateContext or is freed when passed as the
//  pPrevCertContext on a subsequent call. CertDuplicateCertificateContext
//  can be called to make a duplicate.
//
//  pPrevCertContext MUST BE NULL to enumerate the first
//  certificate in the store. Successive certificates are enumerated by setting
//  pPrevCertContext to the CERT_CONTEXT returned by a previous call.
//
//  NOTE: a NON-NULL pPrevCertContext is always CertFreeCertificateContext'ed by
//  this function, even for an error.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertEnumCertificatesInStore}
function CertEnumCertificatesInStore(hCertStore: HCERTSTORE;
  var pPrevCertContext: CERT_CONTEXT): PCCERT_CONTEXT; stdcall;

//+-------------------------------------------------------------------------
//  Find the first or next certificate context in the store.
//
//  The certificate is found according to the dwFindType and its pvFindPara.
//  See below for a list of the find types and its parameters.
//
//  Currently dwFindFlags is only used for CERT_FIND_SUBJECT_ATTR,
//  CERT_FIND_ISSUER_ATTR or CERT_FIND_CTL_USAGE. Otherwise, must be set to 0.
//
//  Usage of dwCertEncodingType depends on the dwFindType.
//
//  If the first or next certificate isn't found, NULL is returned.
//  Otherwise, a pointer to a read only CERT_CONTEXT is returned. CERT_CONTEXT
//  must be freed by calling CertFreeCertificateContext or is freed when passed as the
//  pPrevCertContext on a subsequent call. CertDuplicateCertificateContext
//  can be called to make a duplicate.
//
//  pPrevCertContext MUST BE NULL on the first
//  call to find the certificate. To find the next certificate, the
//  pPrevCertContext is set to the CERT_CONTEXT returned by a previous call.
//
//  NOTE: a NON-NULL pPrevCertContext is always CertFreeCertificateContext'ed by
//  this function, even for an error.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertFindCertificateInStore}
function CertFindCertificateInStore(hCertStore: HCERTSTORE;
  dwCertEncodingType, dwFindFlags, dwFindType: DWORD;
  pvFindPara: Pointer;
  var pPrevCertContext: CERT_CONTEXT): PCCERT_CONTEXT; stdcall;

//+-------------------------------------------------------------------------
// Certificate comparison functions
//--------------------------------------------------------------------------
const
  {$EXTERNALSYM CERT_COMPARE_MASK}
  CERT_COMPARE_MASK                   = $FFFF;
  {$EXTERNALSYM CERT_COMPARE_SHIFT}
  CERT_COMPARE_SHIFT                  = 16;
  {$EXTERNALSYM CERT_COMPARE_ANY}
  CERT_COMPARE_ANY                    = 0;
  {$EXTERNALSYM CERT_COMPARE_SHA1_HASH}
  CERT_COMPARE_SHA1_HASH              = 1;
  {$EXTERNALSYM CERT_COMPARE_NAME}
  CERT_COMPARE_NAME                   = 2;
  {$EXTERNALSYM CERT_COMPARE_ATTR}
  CERT_COMPARE_ATTR                   = 3;
  {$EXTERNALSYM CERT_COMPARE_MD5_HASH}
  CERT_COMPARE_MD5_HASH               = 4;
  {$EXTERNALSYM CERT_COMPARE_PROPERTY}
  CERT_COMPARE_PROPERTY               = 5;
  {$EXTERNALSYM CERT_COMPARE_PUBLIC_KEY}
  CERT_COMPARE_PUBLIC_KEY             = 6;
  {$EXTERNALSYM CERT_COMPARE_HASH}
  CERT_COMPARE_HASH                   = CERT_COMPARE_SHA1_HASH;
  {$EXTERNALSYM CERT_COMPARE_NAME_STR_A}
  CERT_COMPARE_NAME_STR_A             = 7;
  {$EXTERNALSYM CERT_COMPARE_NAME_STR_W}
  CERT_COMPARE_NAME_STR_W             = 8;
  {$EXTERNALSYM CERT_COMPARE_KEY_SPEC}
  CERT_COMPARE_KEY_SPEC               = 9;
  {$EXTERNALSYM CERT_COMPARE_ENHKEY_USAGE}
  CERT_COMPARE_ENHKEY_USAGE           = 10;
  {$EXTERNALSYM CERT_COMPARE_CTL_USAGE}
  CERT_COMPARE_CTL_USAGE              = CERT_COMPARE_ENHKEY_USAGE;
  {$EXTERNALSYM CERT_COMPARE_SUBJECT_CERT}
  CERT_COMPARE_SUBJECT_CERT           = 11;
  {$EXTERNALSYM CERT_COMPARE_ISSUER_OF}
  CERT_COMPARE_ISSUER_OF              = 12;
  {$EXTERNALSYM CERT_COMPARE_EXISTING}
  CERT_COMPARE_EXISTING               = 13;
  {$EXTERNALSYM CERT_COMPARE_SIGNATURE_HASH}
  CERT_COMPARE_SIGNATURE_HASH         = 14;
  {$EXTERNALSYM CERT_COMPARE_KEY_IDENTIFIER}
  CERT_COMPARE_KEY_IDENTIFIER         = 15;
  {$EXTERNALSYM CERT_COMPARE_CERT_ID}
  CERT_COMPARE_CERT_ID                = 16;
  {$EXTERNALSYM CERT_COMPARE_CROSS_CERT_DIST_POINTS}
  CERT_COMPARE_CROSS_CERT_DIST_POINTS = 17;

  {$EXTERNALSYM CERT_COMPARE_PUBKEY_MD5_HASH}
  CERT_COMPARE_PUBKEY_MD5_HASH        = 18;

  {$EXTERNALSYM CERT_COMPARE_SUBJECT_INFO_ACCESS}
  CERT_COMPARE_SUBJECT_INFO_ACCESS    = 19;

//+-------------------------------------------------------------------------
//  dwFindType
//
//  The dwFindType definition consists of two components:
//   - comparison function
//   - certificate information flag
//--------------------------------------------------------------------------
  {$EXTERNALSYM CERT_FIND_ANY}
  CERT_FIND_ANY                    =
    CERT_COMPARE_ANY shl CERT_COMPARE_SHIFT;
  {$EXTERNALSYM CERT_FIND_SHA1_HASH}
  CERT_FIND_SHA1_HASH              =
    CERT_COMPARE_SHA1_HASH shl CERT_COMPARE_SHIFT;
  {$EXTERNALSYM CERT_FIND_MD5_HASH}
  CERT_FIND_MD5_HASH               =
    CERT_COMPARE_MD5_HASH shl CERT_COMPARE_SHIFT;
  {$EXTERNALSYM CERT_FIND_SIGNATURE_HASH}
  CERT_FIND_SIGNATURE_HASH         =
    CERT_COMPARE_SIGNATURE_HASH shl CERT_COMPARE_SHIFT;
  {$EXTERNALSYM CERT_FIND_KEY_IDENTIFIER}
  CERT_FIND_KEY_IDENTIFIER         =
    CERT_COMPARE_KEY_IDENTIFIER shl CERT_COMPARE_SHIFT;
  {$EXTERNALSYM CERT_FIND_HASH}
  CERT_FIND_HASH                   = CERT_FIND_SHA1_HASH;
  {$EXTERNALSYM CERT_FIND_PROPERTY}
  CERT_FIND_PROPERTY               =
    CERT_COMPARE_PROPERTY shl CERT_COMPARE_SHIFT;
  {$EXTERNALSYM CERT_FIND_PUBLIC_KEY}
  CERT_FIND_PUBLIC_KEY             =
    CERT_COMPARE_PUBLIC_KEY shl CERT_COMPARE_SHIFT;
  {$EXTERNALSYM CERT_FIND_SUBJECT_NAME}
  CERT_FIND_SUBJECT_NAME           =
    CERT_COMPARE_NAME shl CERT_COMPARE_SHIFT or CERT_INFO_SUBJECT_FLAG;
  {$EXTERNALSYM CERT_FIND_SUBJECT_ATTR}
  CERT_FIND_SUBJECT_ATTR           =
    CERT_COMPARE_ATTR shl CERT_COMPARE_SHIFT or CERT_INFO_SUBJECT_FLAG;
  {$EXTERNALSYM CERT_FIND_ISSUER_NAME}
  CERT_FIND_ISSUER_NAME            =
    CERT_COMPARE_NAME shl CERT_COMPARE_SHIFT or CERT_INFO_ISSUER_FLAG;
  {$EXTERNALSYM CERT_FIND_ISSUER_ATTR}
  CERT_FIND_ISSUER_ATTR            =
    CERT_COMPARE_ATTR shl CERT_COMPARE_SHIFT or CERT_INFO_ISSUER_FLAG;
  {$EXTERNALSYM CERT_FIND_SUBJECT_STR_A}
  CERT_FIND_SUBJECT_STR_A          =
    CERT_COMPARE_NAME_STR_A shl CERT_COMPARE_SHIFT or CERT_INFO_SUBJECT_FLAG;
  {$EXTERNALSYM CERT_FIND_SUBJECT_STR_W}
  CERT_FIND_SUBJECT_STR_W          =
    CERT_COMPARE_NAME_STR_W shl CERT_COMPARE_SHIFT or CERT_INFO_SUBJECT_FLAG;
  {$EXTERNALSYM CERT_FIND_SUBJECT_STR}
  CERT_FIND_SUBJECT_STR            = CERT_FIND_SUBJECT_STR_W;
  {$EXTERNALSYM CERT_FIND_ISSUER_STR_A}
  CERT_FIND_ISSUER_STR_A           =
    (CERT_COMPARE_NAME_STR_A shl CERT_COMPARE_SHIFT) or CERT_INFO_ISSUER_FLAG;
  {$EXTERNALSYM CERT_FIND_ISSUER_STR_W}
  CERT_FIND_ISSUER_STR_W           =
    (CERT_COMPARE_NAME_STR_W shl CERT_COMPARE_SHIFT) or CERT_INFO_ISSUER_FLAG;
  {$EXTERNALSYM CERT_FIND_ISSUER_STR}
  CERT_FIND_ISSUER_STR             = CERT_FIND_ISSUER_STR_W;
  {$EXTERNALSYM CERT_FIND_KEY_SPEC}
  CERT_FIND_KEY_SPEC               =
    CERT_COMPARE_KEY_SPEC shl CERT_COMPARE_SHIFT;
  {$EXTERNALSYM CERT_FIND_ENHKEY_USAGE}
  CERT_FIND_ENHKEY_USAGE           =
    CERT_COMPARE_ENHKEY_USAGE shl CERT_COMPARE_SHIFT;
  {$EXTERNALSYM CERT_FIND_CTL_USAGE}
  CERT_FIND_CTL_USAGE              = CERT_FIND_ENHKEY_USAGE;

  {$EXTERNALSYM CERT_FIND_SUBJECT_CERT}
  CERT_FIND_SUBJECT_CERT           =
    CERT_COMPARE_SUBJECT_CERT shl CERT_COMPARE_SHIFT;
  {$EXTERNALSYM CERT_FIND_ISSUER_OF}
  CERT_FIND_ISSUER_OF              =
    CERT_COMPARE_ISSUER_OF shl CERT_COMPARE_SHIFT;
  {$EXTERNALSYM CERT_FIND_EXISTING}
  CERT_FIND_EXISTING               =
    CERT_COMPARE_EXISTING shl CERT_COMPARE_SHIFT;
  {$EXTERNALSYM CERT_FIND_CERT_ID}
  CERT_FIND_CERT_ID                =
    CERT_COMPARE_CERT_ID shl CERT_COMPARE_SHIFT;
  {$EXTERNALSYM CERT_FIND_CROSS_CERT_DIST_POINTS}
  CERT_FIND_CROSS_CERT_DIST_POINTS =
    CERT_COMPARE_CROSS_CERT_DIST_POINTS shl CERT_COMPARE_SHIFT;

  {$EXTERNALSYM CERT_FIND_PUBKEY_MD5_HASH}
  CERT_FIND_PUBKEY_MD5_HASH
                    = CERT_COMPARE_PUBKEY_MD5_HASH shl CERT_COMPARE_SHIFT;

  {$EXTERNALSYM CERT_FIND_SUBJECT_INFO_ACCESS}
  CERT_FIND_SUBJECT_INFO_ACCESS
                    = CERT_COMPARE_SUBJECT_INFO_ACCESS shl CERT_COMPARE_SHIFT;

//+-------------------------------------------------------------------------
//  CERT_FIND_ANY
//
//  Find any certificate.
//
//  pvFindPara isn't used.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CERT_FIND_HASH
//
//  Find a certificate with the specified hash.
//
//  pvFindPara points to a CRYPT_HASH_BLOB.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CERT_FIND_KEY_IDENTIFIER
//
//  Find a certificate with the specified KeyIdentifier. Gets the
//  CERT_KEY_IDENTIFIER_PROP_ID property and compares with the input
//  CRYPT_HASH_BLOB.
//
//  pvFindPara points to a CRYPT_HASH_BLOB.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CERT_FIND_PROPERTY
//
//  Find a certificate having the specified property.
//
//  pvFindPara points to a DWORD containing the PROP_ID
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CERT_FIND_PUBLIC_KEY
//
//  Find a certificate matching the specified public key.
//
//  pvFindPara points to a CERT_PUBLIC_KEY_INFO containing the public key
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CERT_FIND_SUBJECT_NAME
//  CERT_FIND_ISSUER_NAME
//
//  Find a certificate with the specified subject/issuer name. Does an exact
//  match of the entire name.
//
//  Restricts search to certificates matching the dwCertEncodingType.
//
//  pvFindPara points to a CERT_NAME_BLOB.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CERT_FIND_SUBJECT_ATTR
//  CERT_FIND_ISSUER_ATTR
//
//  Find a certificate with the specified subject/issuer attributes.
//
//  Compares the attributes in the subject/issuer name with the
//  Relative Distinguished Name's (CERT_RDN) array of attributes specified in
//  pvFindPara. The comparison iterates through the CERT_RDN attributes and looks
//  for an attribute match in any of the subject/issuer's RDNs.
//
//  The CERT_RDN_ATTR fields can have the following special values:
//    pszObjId == NULL              - ignore the attribute object identifier
//    dwValueType == RDN_ANY_TYPE   - ignore the value type
//    Value.pbData == NULL          - match any value
//
//  CERT_CASE_INSENSITIVE_IS_RDN_ATTRS_FLAG should be set in dwFindFlags to do
//  a case insensitive match. Otherwise, defaults to an exact, case sensitive
//  match.
//
//  CERT_UNICODE_IS_RDN_ATTRS_FLAG should be set in dwFindFlags if the RDN was
//  initialized with unicode strings as for
//  CryptEncodeObject(X509_UNICODE_NAME).
//
//  Restricts search to certificates matching the dwCertEncodingType.
//
//  pvFindPara points to a CERT_RDN (defined in wincert.h).
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CERT_FIND_SUBJECT_STR_A
//  CERT_FIND_SUBJECT_STR_W | CERT_FIND_SUBJECT_STR
//  CERT_FIND_ISSUER_STR_A
//  CERT_FIND_ISSUER_STR_W  | CERT_FIND_ISSUER_STR
//
//  Find a certificate containing the specified subject/issuer name string.
//
//  First, the certificate's subject/issuer is converted to a name string
//  via CertNameToStrA/CertNameToStrW(CERT_SIMPLE_NAME_STR). Then, a
//  case insensitive substring within string match is performed.
//
//  Restricts search to certificates matching the dwCertEncodingType.
//
//  For *_STR_A, pvFindPara points to a null terminated character string.
//  For *_STR_W, pvFindPara points to a null terminated wide character string.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CERT_FIND_KEY_SPEC
//
//  Find a certificate having a CERT_KEY_SPEC_PROP_ID property matching
//  the specified KeySpec.
//
//  pvFindPara points to a DWORD containing the KeySpec.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CERT_FIND_ENHKEY_USAGE
//
//  Find a certificate having the szOID_ENHANCED_KEY_USAGE extension or
//  the CERT_ENHKEY_USAGE_PROP_ID and matching the specified pszUsageIdentifers.
//
//  pvFindPara points to a CERT_ENHKEY_USAGE data structure. If pvFindPara
//  is NULL or CERT_ENHKEY_USAGE's cUsageIdentifier is 0, then, matches any
//  certificate having enhanced key usage.
//
//  If the CERT_FIND_VALID_ENHKEY_USAGE_FLAG is set, then, only does a match
//  for certificates that are valid for the specified usages. By default,
//  the ceriticate must be valid for all usages. CERT_FIND_OR_ENHKEY_USAGE_FLAG
//  can be set, if the certificate only needs to be valid for one of the
//  specified usages. Note, CertGetValidUsages() is called to get the
//  certificate's list of valid usages. Only the CERT_FIND_OR_ENHKEY_USAGE_FLAG
//  is applicable when this flag is set.
//
//  The CERT_FIND_OPTIONAL_ENHKEY_USAGE_FLAG can be set in dwFindFlags to
//  also match a certificate without either the extension or property.
//
//  If CERT_FIND_NO_ENHKEY_USAGE_FLAG is set in dwFindFlags, finds
//  certificates without the key usage extension or property. Setting this
//  flag takes precedence over pvFindPara being NULL.
//
//  If the CERT_FIND_EXT_ONLY_ENHKEY_USAGE_FLAG is set, then, only does a match
//  using the extension. If pvFindPara is NULL or cUsageIdentifier is set to
//  0, finds certificates having the extension. If
//  CERT_FIND_OPTIONAL_ENHKEY_USAGE_FLAG is set, also matches a certificate
//  without the extension. If CERT_FIND_NO_ENHKEY_USAGE_FLAG is set, finds
//  certificates without the extension.
//
//  If the CERT_FIND_PROP_ONLY_ENHKEY_USAGE_FLAG is set, then, only does a match
//  using the property. If pvFindPara is NULL or cUsageIdentifier is set to
//  0, finds certificates having the property. If
//  CERT_FIND_OPTIONAL_ENHKEY_USAGE_FLAG is set, also matches a certificate
//  without the property. If CERT_FIND_NO_ENHKEY_USAGE_FLAG is set, finds
//  certificates without the property.
//
//  If CERT_FIND_OR_ENHKEY_USAGE_FLAG is set, does an 'OR' match of any of
//  the specified pszUsageIdentifiers. If not set, then, does an 'AND' match
//  of all of the specified pszUsageIdentifiers.
//--------------------------------------------------------------------------

  {$EXTERNALSYM CERT_FIND_OPTIONAL_ENHKEY_USAGE_FLAG}
  CERT_FIND_OPTIONAL_ENHKEY_USAGE_FLAG  = $1;
  {$EXTERNALSYM CERT_FIND_EXT_ONLY_ENHKEY_USAGE_FLAG}
  CERT_FIND_EXT_ONLY_ENHKEY_USAGE_FLAG  = $2;
  {$EXTERNALSYM CERT_FIND_PROP_ONLY_ENHKEY_USAGE_FLAG}
  CERT_FIND_PROP_ONLY_ENHKEY_USAGE_FLAG = $4;
  {$EXTERNALSYM CERT_FIND_NO_ENHKEY_USAGE_FLAG}
  CERT_FIND_NO_ENHKEY_USAGE_FLAG        = $8;
  {$EXTERNALSYM CERT_FIND_OR_ENHKEY_USAGE_FLAG}
  CERT_FIND_OR_ENHKEY_USAGE_FLAG        = $10;
  {$EXTERNALSYM CERT_FIND_VALID_ENHKEY_USAGE_FLAG}
  CERT_FIND_VALID_ENHKEY_USAGE_FLAG     = $20;

  {$EXTERNALSYM CERT_FIND_OPTIONAL_CTL_USAGE_FLAG}
  CERT_FIND_OPTIONAL_CTL_USAGE_FLAG     = CERT_FIND_OPTIONAL_ENHKEY_USAGE_FLAG;

  {$EXTERNALSYM CERT_FIND_EXT_ONLY_CTL_USAGE_FLAG}
  CERT_FIND_EXT_ONLY_CTL_USAGE_FLAG     = CERT_FIND_EXT_ONLY_ENHKEY_USAGE_FLAG;

  {$EXTERNALSYM CERT_FIND_PROP_ONLY_CTL_USAGE_FLAG}
  CERT_FIND_PROP_ONLY_CTL_USAGE_FLAG    = CERT_FIND_PROP_ONLY_ENHKEY_USAGE_FLAG;

  {$EXTERNALSYM CERT_FIND_NO_CTL_USAGE_FLAG}
  CERT_FIND_NO_CTL_USAGE_FLAG           = CERT_FIND_NO_ENHKEY_USAGE_FLAG;
  {$EXTERNALSYM CERT_FIND_OR_CTL_USAGE_FLAG}
  CERT_FIND_OR_CTL_USAGE_FLAG           = CERT_FIND_OR_ENHKEY_USAGE_FLAG;
  {$EXTERNALSYM CERT_FIND_VALID_CTL_USAGE_FLAG}
  CERT_FIND_VALID_CTL_USAGE_FLAG        = CERT_FIND_VALID_ENHKEY_USAGE_FLAG;

//+-------------------------------------------------------------------------
//  CERT_FIND_CERT_ID
//
//  Find a certificate with the specified CERT_ID.
//
//  pvFindPara points to a CERT_ID.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CERT_FIND_CROSS_CERT_DIST_POINTS
//
//  Find a certificate having either a cross certificate distribution
//  point extension or property.
//
//  pvFindPara isn't used.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CERT_FIND_SUBJECT_INFO_ACCESS
//
//  Find a certificate having either a SubjectInfoAccess extension or
//  property.
//
//  pvFindPara isn't used.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  Get the certificate context from the store for the first or next issuer
//  of the specified subject certificate. Perform the enabled
//  verification checks on the subject. (Note, the checks are on the subject
//  using the returned issuer certificate.)
//
//  If the first or next issuer certificate isn't found, NULL is returned.
//  Otherwise, a pointer to a read only CERT_CONTEXT is returned. CERT_CONTEXT
//  must be freed by calling CertFreeCertificateContext or is freed when passed as the
//  pPrevIssuerContext on a subsequent call. CertDuplicateCertificateContext
//  can be called to make a duplicate.
//
//  For a self signed subject certificate, NULL is returned with LastError set
//  to CERT_STORE_SELF_SIGNED. The enabled verification checks are still done.
//
//  The pSubjectContext may have been obtained from this store, another store
//  or created by the caller application. When created by the caller, the
//  CertCreateCertificateContext function must have been called.
//
//  An issuer may have multiple certificates. This may occur when the validity
//  period is about to change. pPrevIssuerContext MUST BE NULL on the first
//  call to get the issuer. To get the next certificate for the issuer, the
//  pPrevIssuerContext is set to the CERT_CONTEXT returned by a previous call.
//
//  NOTE: a NON-NULL pPrevIssuerContext is always CertFreeCertificateContext'ed by
//  this function, even for an error.
//
//  The following flags can be set in *pdwFlags to enable verification checks
//  on the subject certificate context:
//      CERT_STORE_SIGNATURE_FLAG     - use the public key in the returned
//                                      issuer certificate to verify the
//                                      signature on the subject certificate.
//                                      Note, if pSubjectContext->hCertStore ==
//                                      hCertStore, the store provider might
//                                      be able to eliminate a redo of
//                                      the signature verify.
//      CERT_STORE_TIME_VALIDITY_FLAG - get the current time and verify that
//                                      its within the subject certificate's
//                                      validity period
//      CERT_STORE_REVOCATION_FLAG    - check if the subject certificate is on
//                                      the issuer's revocation list
//
//  If an enabled verification check fails, then, its flag is set upon return.
//  If CERT_STORE_REVOCATION_FLAG was enabled and the issuer doesn't have a
//  CRL in the store, then, CERT_STORE_NO_CRL_FLAG is set in addition to
//  the CERT_STORE_REVOCATION_FLAG.
//
//  If CERT_STORE_SIGNATURE_FLAG or CERT_STORE_REVOCATION_FLAG is set, then,
//  CERT_STORE_NO_ISSUER_FLAG is set if it doesn't have an issuer certificate
//  in the store.
//
//  For a verification check failure, a pointer to the issuer's CERT_CONTEXT
//  is still returned and SetLastError isn't updated.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertGetIssuerCertificateFromStore}
function CertGetIssuerCertificateFromStore(hCertStore: HCERTSTORE;
  pSubjectContext, pPrevIssuerContext: PCCERT_CONTEXT;
  out pdwFlags: DWORD): PCCERT_CONTEXT; stdcall;

//+-------------------------------------------------------------------------
//  Perform the enabled verification checks on the subject certificate
//  using the issuer. Same checks and flags definitions as for the above
//  CertGetIssuerCertificateFromStore.
//
//  If you are only checking CERT_STORE_TIME_VALIDITY_FLAG, then, the
//  issuer can be NULL.
//
//  For a verification check failure, SUCCESS is still returned.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertVerifySubjectCertificateContext}
function CertVerifySubjectCertificateContext(
  var pSubject, pIssuer: CERT_CONTEXT; out pdwFlags: DWORD): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Duplicate a certificate context
//--------------------------------------------------------------------------
{$EXTERNALSYM CertDuplicateCertificateContext}
function CertDuplicateCertificateContext(
  pCertContext: PCCERT_CONTEXT): PCCERT_CONTEXT; stdcall;

//+-------------------------------------------------------------------------
//  Create a certificate context from the encoded certificate. The created
//  context isn't put in a store.
//
//  Makes a copy of the encoded certificate in the created context.
//
//  If unable to decode and create the certificate context, NULL is returned.
//  Otherwise, a pointer to a read only CERT_CONTEXT is returned.
//  CERT_CONTEXT must be freed by calling CertFreeCertificateContext.
//  CertDuplicateCertificateContext can be called to make a duplicate.
//
//  CertSetCertificateContextProperty and CertGetCertificateContextProperty can be called
//  to store properties for the certificate.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertCreateCertificateContext}
function CertCreateCertificateContext(dwCertEncodingType: DWORD;
  pbCertEncoded: PBYTE; cbCertEncoded: DWORD): PCCERT_CONTEXT; stdcall;

//+-------------------------------------------------------------------------
//  Free a certificate context
//
//  There needs to be a corresponding free for each context obtained by a
//  get, find, duplicate or create.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertFreeCertificateContext}
function CertFreeCertificateContext(pCertContext: PCCERT_CONTEXT): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Set the property for the specified certificate context.
//
//  The type definition for pvData depends on the dwPropId value. There are
//  five predefined types:
//      CERT_KEY_PROV_HANDLE_PROP_ID - a HCRYPTPROV for the certificate's
//      private key is passed in pvData. Updates the hCryptProv field
//      of the CERT_KEY_CONTEXT_PROP_ID. If the CERT_KEY_CONTEXT_PROP_ID
//      doesn't exist, its created with all the other fields zeroed out. If
//      CERT_STORE_NO_CRYPT_RELEASE_FLAG isn't set, HCRYPTPROV is implicitly
//      released when either the property is set to NULL or on the final
//      free of the CertContext.
//
//      CERT_NCRYPT_KEY_HANDLE_PROP_ID - a NCRYPT_KEY_HANDLE for the
//      certificate's private key is passed in pvData. The dwKeySpec is
//      set to CERT_NCRYPT_KEY_SPEC.
//
//      CERT_HCRYPTPROV_OR_NCRYPT_KEY_HANDLE_PROP_ID - a
//      HCRYPTPROV_OR_NCRYPT_KEY_HANDLE for the certificates's private
//      key is passed in pvData.  NCryptIsKeyHandle()
//      is called to determine if this is a CNG NCRYPT_KEY_HANDLE.
//      For a NCRYPT_KEY_HANDLE does a CERT_NCRYPT_KEY_HANDLE_PROP_ID set.
//      Otherwise, does a CERT_KEY_PROV_HANDLE_PROP_ID set.
//
//      CERT_KEY_PROV_INFO_PROP_ID - a PCRYPT_KEY_PROV_INFO for the certificate's
//      private key is passed in pvData.
//
//      CERT_SHA1_HASH_PROP_ID       -
//      CERT_MD5_HASH_PROP_ID        -
//      CERT_SIGNATURE_HASH_PROP_ID  - normally, a hash property is implicitly
//      set by doing a CertGetCertificateContextProperty. pvData points to a
//      CRYPT_HASH_BLOB.
//
//      CERT_KEY_CONTEXT_PROP_ID - a PCERT_KEY_CONTEXT for the certificate's
//      private key is passed in pvData. The CERT_KEY_CONTEXT contains both the
//      hCryptProv and dwKeySpec for the private key. A dwKeySpec of
//      CERT_NCRYPT_KEY_SPEC selects the hNCryptKey choice.
//      See the CERT_KEY_PROV_HANDLE_PROP_ID for more information about
//      the hCryptProv field and dwFlags settings. Note, more fields may
//      be added for this property. The cbSize field value will be adjusted
//      accordingly.
//
//      CERT_KEY_SPEC_PROP_ID - the dwKeySpec for the private key. pvData
//      points to a DWORD containing the KeySpec
//
//      CERT_ENHKEY_USAGE_PROP_ID - enhanced key usage definition for the
//      certificate. pvData points to a CRYPT_DATA_BLOB containing an
//      ASN.1 encoded CERT_ENHKEY_USAGE (encoded via
//      CryptEncodeObject(X509_ENHANCED_KEY_USAGE).
//
//      CERT_NEXT_UPDATE_LOCATION_PROP_ID - location of the next update.
//      Currently only applicable to CTLs. pvData points to a CRYPT_DATA_BLOB
//      containing an ASN.1 encoded CERT_ALT_NAME_INFO (encoded via
//      CryptEncodeObject(X509_ALTERNATE_NAME)).
//
//      CERT_FRIENDLY_NAME_PROP_ID - friendly name for the cert, CRL or CTL.
//      pvData points to a CRYPT_DATA_BLOB. pbData is a pointer to a NULL
//      terminated unicode, wide character string.
//      cbData = (wcslen((LPWSTR) pbData) + 1) * sizeof(WCHAR).
//
//      CERT_DESCRIPTION_PROP_ID - description for the cert, CRL or CTL.
//      pvData points to a CRYPT_DATA_BLOB. pbData is a pointer to a NULL
//      terminated unicode, wide character string.
//      cbData = (wcslen((LPWSTR) pbData) + 1) * sizeof(WCHAR).
//
//      CERT_ARCHIVED_PROP_ID - when this property is set, the certificate
//      is skipped during enumeration. Note, certificates having this property
//      are still found for explicit finds, such as, finding a certificate
//      with a specific hash or finding a certificate having a specific issuer
//      and serial number. pvData points to a CRYPT_DATA_BLOB. This blob
//      can be NULL (pbData = NULL, cbData = 0).
//
//      CERT_PUBKEY_ALG_PARA_PROP_ID - for public keys supporting
//      algorithm parameter inheritance. pvData points to a CRYPT_OBJID_BLOB
//      containing the ASN.1 encoded PublicKey Algorithm Parameters. For
//      DSS this would be the parameters encoded via
//      CryptEncodeObject(X509_DSS_PARAMETERS). This property may be set
//      by CryptVerifyCertificateSignatureEx().
//
//      CERT_CROSS_CERT_DIST_POINTS_PROP_ID - location of the cross certs.
//      Currently only applicable to certs. pvData points to a CRYPT_DATA_BLOB
//      containing an ASN.1 encoded CROSS_CERT_DIST_POINTS_INFO (encoded via
//      CryptEncodeObject(X509_CROSS_CERT_DIST_POINTS)).
//
//      CERT_ENROLLMENT_PROP_ID - enrollment information of the pending request.
//      It contains RequestID, CADNSName, CAName, and FriendlyName.
//      The data format is defined as, the first 4 bytes - pending request ID,
//      next 4 bytes - CADNSName size in characters including null-terminator
//      followed by CADNSName string with null-terminator,
//      next 4 bytes - CAName size in characters including null-terminator
//      followed by CAName string with null-terminator,
//      next 4 bytes - FriendlyName size in characters including null-terminator
//      followed by FriendlyName string with null-terminator.
//
//      CERT_DATE_STAMP_PROP_ID - contains the time when added to the store
//      by an admin tool. pvData points to a CRYPT_DATA_BLOB containing
//      the FILETIME.
//
//      CERT_RENEWAL_PROP_ID - contains the hash of renewed certificate
//
//      CERT_OCSP_RESPONSE_PROP_ID - contains the encoded OCSP response.
//      CryptDecodeObject/CryptEncodeObject using
//      lpszStructType = OCSP_RESPONSE.
//      pvData points to a CRYPT_DATA_BLOB containing the encoded OCSP response.
//      If this property is present, CertVerifyRevocation() will first attempt
//      to use before doing an URL retrieval.
//
//      CERT_SOURCE_LOCATION_PROP_ID - contains source location of the CRL or
//      OCSP. pvData points to a CRYPT_DATA_BLOB. pbData is a pointer to a NULL
//      terminated unicode, wide character string. Where,
//      cbData = (wcslen((LPWSTR) pbData) + 1) * sizeof(WCHAR).
//
//      CERT_SOURCE_URL_PROP_ID - contains URL for the CRL or OCSP. pvData
//      is the same as for CERT_SOURCE_LOCATION_PROP_ID.
//
//  For all the other PROP_IDs: an encoded PCRYPT_DATA_BLOB is passed in pvData.
//
//  If the property already exists, then, the old value is deleted and silently
//  replaced. Setting, pvData to NULL, deletes the property.
//
//  CERT_SET_PROPERTY_IGNORE_PERSIST_ERROR_FLAG can be set to ignore any
//  provider write errors and always update the cached context's property.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertSetCertificateContextProperty}
function CertSetCertificateContextProperty(pCertContext: PCCERT_CONTEXT;
  dwPropId, dwFlags: DWORD; pvData: Pointer): BOOL; stdcall;

const
// Set this flag to ignore any store provider write errors and always update
// the cached context's property
  {$EXTERNALSYM CERT_SET_PROPERTY_IGNORE_PERSIST_ERROR_FLAG}
  CERT_SET_PROPERTY_IGNORE_PERSIST_ERROR_FLAG   = $80000000;

// Set this flag to inhibit the persisting of this property
  {$EXTERNALSYM CERT_SET_PROPERTY_INHIBIT_PERSIST_FLAG}
  CERT_SET_PROPERTY_INHIBIT_PERSIST_FLAG        = $40000000;

//+-------------------------------------------------------------------------
//  Get the property for the specified certificate context.
//
//  For CERT_KEY_PROV_HANDLE_PROP_ID, pvData points to a HCRYPTPROV.
//  The CERT_NCRYPT_KEY_SPEC NCRYPT_KEY_HANDLE choice isn't returned.
//
//  For CERT_NCRYPT_KEY_HANDLE_PROP_ID, pvData points to a NCRYPT_KEY_HANDLE.
//  Only returned for the CERT_NCRYPT_KEY_SPEC choice.
//
//  For CERT_HCRYPTPROV_OR_NCRYPT_KEY_HANDLE_PROP_ID, pvData points to a
//  HCRYPTPROV_OR_NCRYPT_KEY_HANDLE. Returns either the HCRYPTPROV or
//  NCRYPT_KEY_HANDLE choice.
//
//  For CERT_KEY_PROV_INFO_PROP_ID, pvData points to a CRYPT_KEY_PROV_INFO structure.
//  Elements pointed to by fields in the pvData structure follow the
//  structure. Therefore, *pcbData may exceed the size of the structure.
//
//  For CERT_KEY_CONTEXT_PROP_ID, pvData points to a CERT_KEY_CONTEXT structure.
//
//  For CERT_KEY_SPEC_PROP_ID, pvData points to a DWORD containing the KeySpec.
//  If the CERT_KEY_CONTEXT_PROP_ID exists, the KeySpec is obtained from there.
//  Otherwise, if the CERT_KEY_PROV_INFO_PROP_ID exists, its the source
//  of the KeySpec. CERT_NCRYPT_KEY_SPEC is returned if the
//  CERT_NCRYPT_KEY_HANDLE_PROP_ID has been set.
//
//  For CERT_SHA1_HASH_PROP_ID or CERT_MD5_HASH_PROP_ID, if the hash
//  doesn't already exist, then, its computed via CryptHashCertificate()
//  and then set. pvData points to the computed hash. Normally, the length
//  is 20 bytes for SHA and 16 for MD5.
//
//  For CERT_SIGNATURE_HASH_PROP_ID, if the hash
//  doesn't already exist, then, its computed via CryptHashToBeSigned()
//  and then set. pvData points to the computed hash. Normally, the length
//  is 20 bytes for SHA and 16 for MD5.
//
//  For CERT_ACCESS_STATE_PROP_ID, pvData points to a DWORD containing the
//  access state flags. The appropriate CERT_ACCESS_STATE_*_FLAG's are set
//  in the returned DWORD. See the CERT_ACCESS_STATE_*_FLAG definitions
//  above. Note, this property is read only. It can't be set.
//
//  For CERT_KEY_IDENTIFIER_PROP_ID, if property doesn't already exist,
//  first searches for the szOID_SUBJECT_KEY_IDENTIFIER extension. Next,
//  does SHA1 hash of the certficate's SubjectPublicKeyInfo. pvData
//  points to the key identifier bytes. Normally, the length is 20 bytes.
//
//  For CERT_PUBKEY_ALG_PARA_PROP_ID, pvPara points to the ASN.1 encoded
//  PublicKey Algorithm Parameters. This property will only be set
//  for public keys supporting algorithm parameter inheritance and when the
//  parameters have been omitted from the encoded and signed certificate.
//
//  For CERT_DATE_STAMP_PROP_ID, pvPara points to a FILETIME updated by
//  an admin tool to indicate when the certificate was added to the store.
//
//  For CERT_OCSP_RESPONSE_PROP_ID, pvPara points to an encoded OCSP response.
//
//  For CERT_SOURCE_LOCATION_PROP_ID and CERT_SOURCE_URL_PROP_ID,
//  pvPara points to a NULL terminated unicode, wide character string.
//
//  For all other PROP_IDs, pvData points to an encoded array of bytes.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertGetCertificateContextProperty}
function CertGetCertificateContextProperty(pCertContext: PCCERT_CONTEXT;
  dwPropId: DWORD; pvData: Pointer; out pcbData: DWORD): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Enumerate the properties for the specified certificate context.
//
//  To get the first property, set dwPropId to 0. The ID of the first
//  property is returned. To get the next property, set dwPropId to the
//  ID returned by the last call. To enumerate all the properties continue
//  until 0 is returned.
//
//  CertGetCertificateContextProperty is called to get the property's data.
//
//  Note, since, the CERT_KEY_PROV_HANDLE_PROP_ID and CERT_KEY_SPEC_PROP_ID
//  properties are stored as fields in the CERT_KEY_CONTEXT_PROP_ID
//  property, they aren't enumerated individually.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertEnumCertificateContextProperties}
function CertEnumCertificateContextProperties(pCertContext: PCCERT_CONTEXT;
  dwPropId: DWORD): DWORD; stdcall;

//+-------------------------------------------------------------------------
//  Creates a CTL entry whose attributes are the certificate context's
//  properties.
//
//  The SubjectIdentifier in the CTL entry is the SHA1 hash of the certificate.
//
//  The certificate properties are added as attributes. The property attribute
//  OID is the decimal PROP_ID preceded by szOID_CERT_PROP_ID_PREFIX. Each
//  property value is copied as a single attribute value.
//
//  Any additional attributes to be included in the CTL entry can be passed
//  in via the cOptAttr and rgOptAttr parameters.
//
//  CTL_ENTRY_FROM_PROP_CHAIN_FLAG can be set in dwFlags, to force the
//  inclusion of the chain building hash properties as attributes.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertCreateCTLEntryFromCertificateContextProperties}
function CertCreateCTLEntryFromCertificateContextProperties(
  pCertContext: PCCERT_CONTEXT; cOptAttr: DWORD;
  var rgOptAttr: CRYPT_ATTRIBUTE; dwFlags: DWORD; pvReserved: Pointer;
  var pCtlEntry: CTL_ENTRY; out pcbCtlEntry: DWORD): BOOL; stdcall;

const
// Set this flag to get and include the chain building hash properties
// as attributes in the CTL entry
  {$EXTERNALSYM CTL_ENTRY_FROM_PROP_CHAIN_FLAG}
  CTL_ENTRY_FROM_PROP_CHAIN_FLAG = $1;

//+-------------------------------------------------------------------------
//  Sets properties on the certificate context using the attributes in
//  the CTL entry.
//
//  The property attribute OID is the decimal PROP_ID preceded by
//  szOID_CERT_PROP_ID_PREFIX. Only attributes containing such an OID are
//  copied.
//
//  CERT_SET_PROPERTY_IGNORE_PERSIST_ERROR_FLAG may be set in dwFlags.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertSetCertificateContextPropertiesFromCTLEntry}
function CertSetCertificateContextPropertiesFromCTLEntry(
  pCertContext: PCCERT_CONTEXT; var pCtlEntry: CTL_ENTRY;
  dwFlags: DWORD): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Get the first or next CRL context from the store for the specified
//  issuer certificate. Perform the enabled verification checks on the CRL.
//
//  If the first or next CRL isn't found, NULL is returned.
//  Otherwise, a pointer to a read only CRL_CONTEXT is returned. CRL_CONTEXT
//  must be freed by calling CertFreeCRLContext. However, the free must be
//  pPrevCrlContext on a subsequent call. CertDuplicateCRLContext
//  can be called to make a duplicate.
//
//  The pIssuerContext may have been obtained from this store, another store
//  or created by the caller application. When created by the caller, the
//  CertCreateCertificateContext function must have been called.
//
//  If pIssuerContext == NULL, finds all the CRLs in the store.
//
//  An issuer may have multiple CRLs. For example, it generates delta CRLs
//  using a X.509 v3 extension. pPrevCrlContext MUST BE NULL on the first
//  call to get the CRL. To get the next CRL for the issuer, the
//  pPrevCrlContext is set to the CRL_CONTEXT returned by a previous call.
//
//  NOTE: a NON-NULL pPrevCrlContext is always CertFreeCRLContext'ed by
//  this function, even for an error.
//
//  The following flags can be set in *pdwFlags to enable verification checks
//  on the returned CRL:
//      CERT_STORE_SIGNATURE_FLAG     - use the public key in the
//                                      issuer's certificate to verify the
//                                      signature on the returned CRL.
//                                      Note, if pIssuerContext->hCertStore ==
//                                      hCertStore, the store provider might
//                                      be able to eliminate a redo of
//                                      the signature verify.
//      CERT_STORE_TIME_VALIDITY_FLAG - get the current time and verify that
//                                      its within the CRL's ThisUpdate and
//                                      NextUpdate validity period.
//      CERT_STORE_BASE_CRL_FLAG      - get base CRL.
//      CERT_STORE_DELTA_CRL_FLAG     - get delta CRL.
//
//  If only one of CERT_STORE_BASE_CRL_FLAG or CERT_STORE_DELTA_CRL_FLAG is
//  set, then, only returns either a base or delta CRL. In any case, the
//  appropriate base or delta flag will be cleared upon returned. If both
//  flags are set, then, only one of flags will be cleared.
//
//  If an enabled verification check fails, then, its flag is set upon return.
//
//  If pIssuerContext == NULL, then, an enabled CERT_STORE_SIGNATURE_FLAG
//  always fails and the CERT_STORE_NO_ISSUER_FLAG is also set.
//
//  For a verification check failure, a pointer to the first or next
//  CRL_CONTEXT is still returned and SetLastError isn't updated.
//--------------------------------------------------------------------------
function CertGetCRLFromStore(hCertStore: HCERTSTORE;
  pIssuerContext: PCCERT_CONTEXT; pPrevCrlContext: PCCRL_CONTEXT;
  out pdwFlags: DWORD): PCCRL_CONTEXT; stdcall;

//+-------------------------------------------------------------------------
//  Enumerate the CRL contexts in the store.
//
//  If a CRL isn't found, NULL is returned.
//  Otherwise, a pointer to a read only CRL_CONTEXT is returned. CRL_CONTEXT
//  must be freed by calling CertFreeCRLContext or is freed when passed as the
//  pPrevCrlContext on a subsequent call. CertDuplicateCRLContext
//  can be called to make a duplicate.
//
//  pPrevCrlContext MUST BE NULL to enumerate the first
//  CRL in the store. Successive CRLs are enumerated by setting
//  pPrevCrlContext to the CRL_CONTEXT returned by a previous call.
//
//  NOTE: a NON-NULL pPrevCrlContext is always CertFreeCRLContext'ed by
//  this function, even for an error.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertEnumCRLsInStore}
function CertEnumCRLsInStore(hCertStore: HCERTSTORE;
  pPrevCrlContext: PCCRL_CONTEXT): PCCRL_CONTEXT; stdcall;

//+-------------------------------------------------------------------------
//  Find the first or next CRL context in the store.
//
//  The CRL is found according to the dwFindType and its pvFindPara.
//  See below for a list of the find types and its parameters.
//
//  Currently dwFindFlags isn't used and must be set to 0.
//
//  Usage of dwCertEncodingType depends on the dwFindType.
//
//  If the first or next CRL isn't found, NULL is returned.
//  Otherwise, a pointer to a read only CRL_CONTEXT is returned. CRL_CONTEXT
//  must be freed by calling CertFreeCRLContext or is freed when passed as the
//  pPrevCrlContext on a subsequent call. CertDuplicateCRLContext
//  can be called to make a duplicate.
//
//  pPrevCrlContext MUST BE NULL on the first
//  call to find the CRL. To find the next CRL, the
//  pPrevCrlContext is set to the CRL_CONTEXT returned by a previous call.
//
//  NOTE: a NON-NULL pPrevCrlContext is always CertFreeCRLContext'ed by
//  this function, even for an error.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertFindCRLInStore}
function CertFindCRLInStore(hCertStore: HCERTSTORE;
  dwCertEncodingType, dwFindFlags, dwFindType: DWORD; pvFindPara: Pointer;
  pPrevCrlContext: PCCRL_CONTEXT): PCCRL_CONTEXT; stdcall;

const
  {$EXTERNALSYM CRL_FIND_ANY}
  CRL_FIND_ANY          = 0;
  {$EXTERNALSYM CRL_FIND_ISSUED_BY}
  CRL_FIND_ISSUED_BY    = 1;
  {$EXTERNALSYM CRL_FIND_EXISTING}
  CRL_FIND_EXISTING     = 2;
  {$EXTERNALSYM CRL_FIND_ISSUED_FOR}
  CRL_FIND_ISSUED_FOR   = 3;

//+-------------------------------------------------------------------------
//  CRL_FIND_ANY
//
//  Find any CRL.
//
//  pvFindPara isn't used.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CRL_FIND_ISSUED_BY
//
//  Find CRL matching the specified issuer.
//
//  pvFindPara is the PCCERT_CONTEXT of the CRL issuer. May be NULL to
//  match any issuer.
//
//  By default, only does issuer name matching. The following flags can be
//  set in dwFindFlags to do additional filtering.
//
//  If CRL_FIND_ISSUED_BY_AKI_FLAG is set in dwFindFlags, then, checks if the
//  CRL has an Authority Key Identifier (AKI) extension. If the CRL has an
//  AKI, then, only returns a CRL whose AKI matches the issuer.
//
//  Note, the AKI extension has the following OID:
//  szOID_AUTHORITY_KEY_IDENTIFIER2 and its corresponding data structure.
//
//  If CRL_FIND_ISSUED_BY_SIGNATURE_FLAG is set in dwFindFlags, then,
//  uses the public key in the issuer's certificate to verify the
//  signature on the CRL. Only returns a CRL having a valid signature.
//
//  If CRL_FIND_ISSUED_BY_DELTA_FLAG is set in dwFindFlags, then, only
//  returns a delta CRL.
//
//  If CRL_FIND_ISSUED_BY_BASE_FLAG is set in dwFindFlags, then, only
//  returns a base CRL.
//--------------------------------------------------------------------------
  {$EXTERNALSYM CRL_FIND_ISSUED_BY_AKI_FLAG}
  CRL_FIND_ISSUED_BY_AKI_FLAG           = $1;
  {$EXTERNALSYM CRL_FIND_ISSUED_BY_SIGNATURE_FLAG}
  CRL_FIND_ISSUED_BY_SIGNATURE_FLAG     = $2;
  {$EXTERNALSYM CRL_FIND_ISSUED_BY_DELTA_FLAG}
  CRL_FIND_ISSUED_BY_DELTA_FLAG         = $4;
  {$EXTERNALSYM CRL_FIND_ISSUED_BY_BASE_FLAG}
  CRL_FIND_ISSUED_BY_BASE_FLAG          = $8;

//+-------------------------------------------------------------------------
//  CRL_FIND_EXISTING
//
//  Find existing CRL in the store.
//
//  pvFindPara is the PCCRL_CONTEXT of the CRL to check if it already
//  exists in the store.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CRL_FIND_ISSUED_FOR
//
//  Find CRL for the specified subject and its issuer.
//
//  pvFindPara points to the following CRL_FIND_ISSUED_FOR_PARA which contains
//  both the subject and issuer certificates. Not optional.
//
//  The subject's issuer name is used to match the CRL's issuer name. Otherwise,
//  the issuer's certificate is used the same as in the above
//  CRL_FIND_ISSUED_BY.
//
//  Note, when cross certificates are used, the subject name in the issuer's
//  certificate may not match the issuer name in the subject certificate and
//  its corresponding CRL.
//
//  All of the above CRL_FIND_ISSUED_BY_*_FLAGS apply to this find type.
//--------------------------------------------------------------------------
type
  PCrlFindIssuedForPara = ^TCrlFindIssuedForPara; 
  {$EXTERNALSYM _CRL_FIND_ISSUED_FOR_PARA} 
  _CRL_FIND_ISSUED_FOR_PARA = record 
    pSubjectCert: PCCERT_CONTEXT; 
    pIssuerCert: PCCERT_CONTEXT; 
  end; 
  {$EXTERNALSYM CRL_FIND_ISSUED_FOR_PARA} 
  CRL_FIND_ISSUED_FOR_PARA = _CRL_FIND_ISSUED_FOR_PARA; 
  {$EXTERNALSYM PCRL_FIND_ISSUED_FOR_PARA} 
  PCRL_FIND_ISSUED_FOR_PARA = ^_CRL_FIND_ISSUED_FOR_PARA; 
  TCrlFindIssuedForPara = _CRL_FIND_ISSUED_FOR_PARA; 

//+-------------------------------------------------------------------------
//  Duplicate a CRL context
//--------------------------------------------------------------------------
{$EXTERNALSYM CertDuplicateCRLContext}
function CertDuplicateCRLContext(
  pCrlContext: PCCRL_CONTEXT): PCCRL_CONTEXT; stdcall;

//+-------------------------------------------------------------------------
//  Create a CRL context from the encoded CRL. The created
//  context isn't put in a store.
//
//  Makes a copy of the encoded CRL in the created context.
//
//  If unable to decode and create the CRL context, NULL is returned.
//  Otherwise, a pointer to a read only CRL_CONTEXT is returned.
//  CRL_CONTEXT must be freed by calling CertFreeCRLContext.
//  CertDuplicateCRLContext can be called to make a duplicate.
//
//  CertSetCRLContextProperty and CertGetCRLContextProperty can be called
//  to store properties for the CRL.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertCreateCRLContext}
function CertCreateCRLContext(dwCertEncodingType: DWORD;
  pbCrlEncoded: PBYTE; cbCrlEncoded: DWORD): PCCRL_CONTEXT; stdcall;

//+-------------------------------------------------------------------------
//  Free a CRL context
//
//  There needs to be a corresponding free for each context obtained by a
//  get, duplicate or create.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertFreeCRLContext}
function CertFreeCRLContext(pCrlContext: PCCRL_CONTEXT): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Set the property for the specified CRL context.
//
//  Same Property Ids and semantics as CertSetCertificateContextProperty.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertSetCRLContextProperty}
function CertSetCRLContextProperty(pCrlContext: PCCRL_CONTEXT;
  dwPropId, dwFlags: DWORD; pvData: Pointer): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Get the property for the specified CRL context.
//
//  Same Property Ids and semantics as CertGetCertificateContextProperty.
//
//  CERT_SHA1_HASH_PROP_ID, CERT_MD5_HASH_PROP_ID or
//  CERT_SIGNATURE_HASH_PROP_ID is the predefined property of most interest.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertGetCRLContextProperty}
function CertGetCRLContextProperty(pCrlContext: PCCRL_CONTEXT;
  dwPropId: DWORD; pvData: Pointer; out pcbData: DWORD): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Enumerate the properties for the specified CRL context.
//
//  To get the first property, set dwPropId to 0. The ID of the first
//  property is returned. To get the next property, set dwPropId to the
//  ID returned by the last call. To enumerate all the properties continue
//  until 0 is returned.
//
//  CertGetCRLContextProperty is called to get the property's data.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertEnumCRLContextProperties}
function CertEnumCRLContextProperties(pCrlContext: PCCRL_CONTEXT;
  dwPropId: DWORD): DWORD; stdcall;

//+-------------------------------------------------------------------------
//  Search the CRL's list of entries for the specified certificate.
//
//  TRUE is returned if we were able to search the list. Otherwise, FALSE is
//  returned,
//
//  For success, if the certificate was found in the list, *ppCrlEntry is
//  updated with a pointer to the entry. Otherwise, *ppCrlEntry is set to NULL.
//  The returned entry isn't allocated and must not be freed.
//
//  dwFlags and pvReserved currently aren't used and must be set to 0 or NULL.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertFindCertificateInCRL}
function CertFindCertificateInCRL(pCert: PCCERT_CONTEXT;
  pCrlContext: PCCRL_CONTEXT; dwFlags: DWORD; pvReserved: Pointer;
  var ppCrlEntry: PCRL_ENTRY): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Is the specified CRL valid for the certificate.
//
//  Returns TRUE if the CRL's list of entries would contain the certificate
//  if it was revoked. Note, doesn't check that the certificate is in the
//  list of entries.
//
//  If the CRL has an Issuing Distribution Point (IDP) extension, checks
//  that it's valid for the subject certificate.
//
//  dwFlags and pvReserved currently aren't used and must be set to 0 and NULL.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertIsValidCRLForCertificate}
function CertIsValidCRLForCertificate(pCert: PCCERT_CONTEXT;
  pCrl: PCCRL_CONTEXT; dwFlags: DWORD; pvReserved: Pointer): BOOL; stdcall;

//+-------------------------------------------------------------------------
// Add certificate/CRL, encoded, context or element disposition values.
//--------------------------------------------------------------------------
const
  {$EXTERNALSYM CERT_STORE_ADD_NEW}
  CERT_STORE_ADD_NEW                                 = 1;
  {$EXTERNALSYM CERT_STORE_ADD_USE_EXISTING}
  CERT_STORE_ADD_USE_EXISTING                        = 2;
  {$EXTERNALSYM CERT_STORE_ADD_REPLACE_EXISTING}
  CERT_STORE_ADD_REPLACE_EXISTING                    = 3;
  {$EXTERNALSYM CERT_STORE_ADD_ALWAYS}
  CERT_STORE_ADD_ALWAYS                              = 4;
  {$EXTERNALSYM CERT_STORE_ADD_REPLACE_EXISTING_INHERIT_PROPERTIES}
  CERT_STORE_ADD_REPLACE_EXISTING_INHERIT_PROPERTIES = 5;
  {$EXTERNALSYM CERT_STORE_ADD_NEWER}
  CERT_STORE_ADD_NEWER                               = 6;
  {$EXTERNALSYM CERT_STORE_ADD_NEWER_INHERIT_PROPERTIES}
  CERT_STORE_ADD_NEWER_INHERIT_PROPERTIES            = 7;


//+-------------------------------------------------------------------------
//  Add the encoded certificate to the store according to the specified
//  disposition action.
//
//  Makes a copy of the encoded certificate before adding to the store.
//
//  dwAddDispostion specifies the action to take if the certificate
//  already exists in the store. This parameter must be one of the following
//  values:
//    CERT_STORE_ADD_NEW
//      Fails if the certificate already exists in the store. LastError
//      is set to CRYPT_E_EXISTS.
//    CERT_STORE_ADD_USE_EXISTING
//      If the certifcate already exists, then, its used and if ppCertContext
//      is non-NULL, the existing context is duplicated.
//    CERT_STORE_ADD_REPLACE_EXISTING
//      If the certificate already exists, then, the existing certificate
//      context is deleted before creating and adding the new context.
//    CERT_STORE_ADD_ALWAYS
//      No check is made to see if the certificate already exists. A
//      new certificate context is always created. This may lead to
//      duplicates in the store.
//    CERT_STORE_ADD_REPLACE_EXISTING_INHERIT_PROPERTIES
//      If the certificate already exists, then, its used.
//    CERT_STORE_ADD_NEWER
//      Fails if the certificate already exists in the store AND the NotBefore
//      time of the existing certificate is equal to or greater than the
//      NotBefore time of the new certificate being added. LastError
//      is set to CRYPT_E_EXISTS.
//
//      If an older certificate is replaced, same as
//      CERT_STORE_ADD_REPLACE_EXISTING.
//
//      For CRLs or CTLs compares the ThisUpdate times.
//
//    CERT_STORE_ADD_NEWER_INHERIT_PROPERTIES
//      Same as CERT_STORE_ADD_NEWER. However, if an older certificate is
//      replaced, same as CERT_STORE_ADD_REPLACE_EXISTING_INHERIT_PROPERTIES.
//
//  CertGetSubjectCertificateFromStore is called to determine if the
//  certificate already exists in the store.
//
//  ppCertContext can be NULL, indicating the caller isn't interested
//  in getting the CERT_CONTEXT of the added or existing certificate.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertAddEncodedCertificateToStore}
function CertAddEncodedCertificateToStore(hCertStore: HCERTSTORE;
  dwCertEncodingType: DWORD; pbCertEncoded: PBYTE;
  cbCertEncoded, dwAddDisposition: DWORD;
  out ppCertContext: PCCERT_CONTEXT): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Add the certificate context to the store according to the specified
//  disposition action.
//
//  In addition to the encoded certificate, the context's properties are
//  also copied.  Note, the CERT_KEY_CONTEXT_PROP_ID property (and its
//  CERT_KEY_PROV_HANDLE_PROP_ID or CERT_KEY_SPEC_PROP_ID) isn't copied.
//
//  Makes a copy of the certificate context before adding to the store.
//
//  dwAddDispostion specifies the action to take if the certificate
//  already exists in the store. This parameter must be one of the following
//  values:
//    CERT_STORE_ADD_NEW
//      Fails if the certificate already exists in the store. LastError
//      is set to CRYPT_E_EXISTS.
//    CERT_STORE_ADD_USE_EXISTING
//      If the certifcate already exists, then, its used and if ppStoreContext
//      is non-NULL, the existing context is duplicated. Iterates
//      through pCertContext's properties and only copies the properties
//      that don't already exist. The SHA1 and MD5 hash properties aren't
//      copied.
//    CERT_STORE_ADD_REPLACE_EXISTING
//      If the certificate already exists, then, the existing certificate
//      context is deleted before creating and adding a new context.
//      Properties are copied before doing the add.
//    CERT_STORE_ADD_ALWAYS
//      No check is made to see if the certificate already exists. A
//      new certificate context is always created and added. This may lead to
//      duplicates in the store. Properties are
//      copied before doing the add.
//    CERT_STORE_ADD_REPLACE_EXISTING_INHERIT_PROPERTIES
//      If the certificate already exists, then, the existing certificate
//      context is used. Properties from the added context are copied and
//      replace existing properties. However, any existing properties not
//      in the added context remain and aren't deleted.
//    CERT_STORE_ADD_NEWER
//      Fails if the certificate already exists in the store AND the NotBefore
//      time of the existing context is equal to or greater than the
//      NotBefore time of the new context being added. LastError
//      is set to CRYPT_E_EXISTS.
//
//      If an older context is replaced, same as
//      CERT_STORE_ADD_REPLACE_EXISTING.
//
//      For CRLs or CTLs compares the ThisUpdate times.
//
//    CERT_STORE_ADD_NEWER_INHERIT_PROPERTIES
//      Same as CERT_STORE_ADD_NEWER. However, if an older context is
//      replaced, same as CERT_STORE_ADD_REPLACE_EXISTING_INHERIT_PROPERTIES.
//
//  CertGetSubjectCertificateFromStore is called to determine if the
//  certificate already exists in the store.
//
//  ppStoreContext can be NULL, indicating the caller isn't interested
//  in getting the CERT_CONTEXT of the added or existing certificate.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertAddCertificateContextToStore}
function CertAddCertificateContextToStore(hCertStore: HCERTSTORE;
  pCertContext: PCCERT_CONTEXT; dwAddDisposition: DWORD;
  out ppStoreContext: PCCERT_CONTEXT): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Certificate Store Context Types
//--------------------------------------------------------------------------
const
  {$EXTERNALSYM CERT_STORE_CERTIFICATE_CONTEXT}
  CERT_STORE_CERTIFICATE_CONTEXT = 1;
  {$EXTERNALSYM CERT_STORE_CRL_CONTEXT}
  CERT_STORE_CRL_CONTEXT         = 2;
  {$EXTERNALSYM CERT_STORE_CTL_CONTEXT}
  CERT_STORE_CTL_CONTEXT         = 3;

//+-------------------------------------------------------------------------
//  Certificate Store Context Bit Flags
//--------------------------------------------------------------------------
  {$EXTERNALSYM CERT_STORE_ALL_CONTEXT_FLAG}
  CERT_STORE_ALL_CONTEXT_FLAG         = not Longword(0);
  {$EXTERNALSYM CERT_STORE_CERTIFICATE_CONTEXT_FLAG}
  CERT_STORE_CERTIFICATE_CONTEXT_FLAG = 1 shl CERT_STORE_CERTIFICATE_CONTEXT;
  {$EXTERNALSYM CERT_STORE_CRL_CONTEXT_FLAG}
  CERT_STORE_CRL_CONTEXT_FLAG         = 1 shl CERT_STORE_CRL_CONTEXT;
  {$EXTERNALSYM CERT_STORE_CTL_CONTEXT_FLAG}
  CERT_STORE_CTL_CONTEXT_FLAG         = 1 shl CERT_STORE_CTL_CONTEXT;

//+-------------------------------------------------------------------------
//  Add the serialized certificate or CRL element to the store.
//
//  The serialized element contains the encoded certificate, CRL or CTL and
//  its properties, such as, CERT_KEY_PROV_INFO_PROP_ID.
//
//  If hCertStore is NULL, creates a certificate, CRL or CTL context not
//  residing in any store.
//
//  dwAddDispostion specifies the action to take if the certificate or CRL
//  already exists in the store. See CertAddCertificateContextToStore for a
//  list of and actions taken.
//
//  dwFlags currently isn't used and should be set to 0.
//
//  dwContextTypeFlags specifies the set of allowable contexts. For example, to
//  add either a certificate or CRL, set dwContextTypeFlags to:
//      CERT_STORE_CERTIFICATE_CONTEXT_FLAG | CERT_STORE_CRL_CONTEXT_FLAG
//
//  *pdwContextType is updated with the type of the context returned in
//  *ppvContxt. pdwContextType or ppvContext can be NULL, indicating the
//  caller isn't interested in getting the output. If *ppvContext is
//  returned it must be freed by calling CertFreeCertificateContext or
//  CertFreeCRLContext.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertAddSerializedElementToStore}
function CertAddSerializedElementToStore(hCertStore: HCERTSTORE;
  pbElement: PBYTE;
  cbElement, dwAddDisposition, dwFlags, dwContextTypeFlags: DWORD;
  var pdwContextType: DWORD; out ppvContext: Pointer): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Delete the specified certificate from the store.
//
//  All subsequent gets or finds for the certificate will fail. However,
//  memory allocated for the certificate isn't freed until all of its contexts
//  have also been freed.
//
//  The pCertContext is obtained from a get, enum, find or duplicate.
//
//  Some store provider implementations might also delete the issuer's CRLs
//  if this is the last certificate for the issuer in the store.
//
//  NOTE: the pCertContext is always CertFreeCertificateContext'ed by
//  this function, even for an error.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertDeleteCertificateFromStore}
function CertDeleteCertificateFromStore(
  pCertContext: PCCERT_CONTEXT): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Add the encoded CRL to the store according to the specified
//  disposition option.
//
//  Makes a copy of the encoded CRL before adding to the store.
//
//  dwAddDispostion specifies the action to take if the CRL
//  already exists in the store. See CertAddEncodedCertificateToStore for a
//  list of and actions taken.
//
//  Compares the CRL's Issuer to determine if the CRL already exists in the
//  store.
//
//  ppCrlContext can be NULL, indicating the caller isn't interested
//  in getting the CRL_CONTEXT of the added or existing CRL.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertAddEncodedCRLToStore}
function CertAddEncodedCRLToStore(hCertStore: HCERTSTORE;
  dwCertEncodingType: DWORD; pbCrlEncoded: PBYTE;
  cbCrlEncoded, dwAddDisposition: DWORD;
  out ppCrlContext: PCCRL_CONTEXT): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Add the CRL context to the store according to the specified
//  disposition option.
//
//  In addition to the encoded CRL, the context's properties are
//  also copied.  Note, the CERT_KEY_CONTEXT_PROP_ID property (and its
//  CERT_KEY_PROV_HANDLE_PROP_ID or CERT_KEY_SPEC_PROP_ID) isn't copied.
//
//  Makes a copy of the encoded CRL before adding to the store.
//
//  dwAddDispostion specifies the action to take if the CRL
//  already exists in the store. See CertAddCertificateContextToStore for a
//  list of and actions taken.
//
//  Compares the CRL's Issuer, ThisUpdate and NextUpdate to determine
//  if the CRL already exists in the store.
//
//  ppStoreContext can be NULL, indicating the caller isn't interested
//  in getting the CRL_CONTEXT of the added or existing CRL.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertAddCRLContextToStore}
function CertAddCRLContextToStore(hCertStore: HCERTSTORE;
  pCrlContext: PCCRL_CONTEXT; dwAddDisposition: DWORD;
  out ppStoreContext: PCCRL_CONTEXT): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Delete the specified CRL from the store.
//
//  All subsequent gets for the CRL will fail. However,
//  memory allocated for the CRL isn't freed until all of its contexts
//  have also been freed.
//
//  The pCrlContext is obtained from a get or duplicate.
//
//  NOTE: the pCrlContext is always CertFreeCRLContext'ed by
//  this function, even for an error.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertDeleteCRLFromStore}
function CertDeleteCRLFromStore(pCrlContext: PCCRL_CONTEXT): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Serialize the certificate context's encoded certificate and its
//  properties.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertSerializeCertificateStoreElement}
function CertSerializeCertificateStoreElement(pCertContext: PCCERT_CONTEXT;
  dwFlags: DWORD; pbElement: PBYTE; out pcbElement: DWORD): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Serialize the CRL context's encoded CRL and its properties.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertSerializeCRLStoreElement}
function CertSerializeCRLStoreElement(pCrlContext: PCCRL_CONTEXT;
  dwFlags: DWORD; pbElement: PBYTE; out pcbElement: DWORD): BOOL; stdcall;

//+=========================================================================
//  Certificate Trust List (CTL) Store Data Structures and APIs
//==========================================================================

//+-------------------------------------------------------------------------
//  Duplicate a CTL context
//--------------------------------------------------------------------------
{$EXTERNALSYM CertDuplicateCTLContext}
function CertDuplicateCTLContext(
  pCtlContext: PCCTL_CONTEXT): PCCTL_CONTEXT; stdcall;

//+-------------------------------------------------------------------------
//  Create a CTL context from the encoded CTL. The created
//  context isn't put in a store.
//
//  Makes a copy of the encoded CTL in the created context.
//
//  If unable to decode and create the CTL context, NULL is returned.
//  Otherwise, a pointer to a read only CTL_CONTEXT is returned.
//  CTL_CONTEXT must be freed by calling CertFreeCTLContext.
//  CertDuplicateCTLContext can be called to make a duplicate.
//
//  CertSetCTLContextProperty and CertGetCTLContextProperty can be called
//  to store properties for the CTL.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertCreateCTLContext}
function CertCreateCTLContext(dwMsgAndCertEncodingType: DWORD;
  pbCtlEncoded: PBYTE; cbCtlEncoded: DWORD): PCCTL_CONTEXT; stdcall;

//+-------------------------------------------------------------------------
//  Free a CTL context
//
//  There needs to be a corresponding free for each context obtained by a
//  get, duplicate or create.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertFreeCTLContext}
function CertFreeCTLContext(pCtlContext: PCCTL_CONTEXT): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Set the property for the specified CTL context.
//
//  Same Property Ids and semantics as CertSetCertificateContextProperty.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertSetCTLContextProperty}
function CertSetCTLContextProperty(pCtlContext: PCCTL_CONTEXT;
  dwPropId, dwFlags: DWORD; pvData: Pointer): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Get the property for the specified CTL context.
//
//  Same Property Ids and semantics as CertGetCertificateContextProperty.
//
//  CERT_SHA1_HASH_PROP_ID or CERT_NEXT_UPDATE_LOCATION_PROP_ID are the
//  predefined properties of most interest.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertGetCTLContextProperty}
function CertGetCTLContextProperty(pCtlContext: PCCTL_CONTEXT;
  dwPropId: DWORD; pvData: Pointer; out pcbData: DWORD): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Enumerate the properties for the specified CTL context.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertEnumCTLContextProperties}
function CertEnumCTLContextProperties(pCtlContext: PCCTL_CONTEXT;
  dwPropId: DWORD): DWORD; stdcall;

//+-------------------------------------------------------------------------
//  Enumerate the CTL contexts in the store.
//
//  If a CTL isn't found, NULL is returned.
//  Otherwise, a pointer to a read only CTL_CONTEXT is returned. CTL_CONTEXT
//  must be freed by calling CertFreeCTLContext or is freed when passed as the
//  pPrevCtlContext on a subsequent call. CertDuplicateCTLContext
//  can be called to make a duplicate.
//
//  pPrevCtlContext MUST BE NULL to enumerate the first
//  CTL in the store. Successive CTLs are enumerated by setting
//  pPrevCtlContext to the CTL_CONTEXT returned by a previous call.
//
//  NOTE: a NON-NULL pPrevCtlContext is always CertFreeCTLContext'ed by
//  this function, even for an error.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertEnumCTLsInStore}
function CertEnumCTLsInStore(hCertStore: HCERTSTORE;
  pPrevCtlContext: PCCTL_CONTEXT): PCCTL_CONTEXT; stdcall;

//+-------------------------------------------------------------------------
//  Attempt to find the specified subject in the CTL.
//
//  For CTL_CERT_SUBJECT_TYPE, pvSubject points to a CERT_CONTEXT. The CTL's
//  SubjectAlgorithm is examined to determine the representation of the
//  subject's identity. Initially, only SHA1 or MD5 hash will be supported.
//  The appropriate hash property is obtained from the CERT_CONTEXT.
//
//  For CTL_ANY_SUBJECT_TYPE, pvSubject points to the CTL_ANY_SUBJECT_INFO
//  structure which contains the SubjectAlgorithm to be matched in the CTL
//  and the SubjectIdentifer to be matched in one of the CTL entries.
//
//  The certificate's hash or the CTL_ANY_SUBJECT_INFO's SubjectIdentifier
//  is used as the key in searching the subject entries. A binary
//  memory comparison is done between the key and the entry's SubjectIdentifer.
//
//  dwEncodingType isn't used for either of the above SubjectTypes.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertFindSubjectInCTL}
function CertFindSubjectInCTL(dwEncodingType, dwSubjectType: DWORD;
  pvSubject: Pointer; pCtlContext: PCCTL_CONTEXT;
  dwFlags: DWORD): PCTL_ENTRY; stdcall;

// Subject Types:
//  CTL_ANY_SUBJECT_TYPE, pvSubject points to following CTL_ANY_SUBJECT_INFO.
//  CTL_CERT_SUBJECT_TYPE, pvSubject points to CERT_CONTEXT.
const
  {$EXTERNALSYM CTL_ANY_SUBJECT_TYPE}
  CTL_ANY_SUBJECT_TYPE  = 1;
  {$EXTERNALSYM CTL_CERT_SUBJECT_TYPE}
  CTL_CERT_SUBJECT_TYPE = 2;

type
  PCtlAnySubjectInfo = ^TCtlAnySubjectInfo; 
  {$EXTERNALSYM _CTL_ANY_SUBJECT_INFO} 
  _CTL_ANY_SUBJECT_INFO = record 
    SubjectAlgorithm: CRYPT_ALGORITHM_IDENTIFIER; 
    SubjectIdentifier: CRYPT_DATA_BLOB; 
  end; 
  {$EXTERNALSYM CTL_ANY_SUBJECT_INFO} 
  CTL_ANY_SUBJECT_INFO = _CTL_ANY_SUBJECT_INFO; 
  {$EXTERNALSYM PCTL_ANY_SUBJECT_INFO} 
  PCTL_ANY_SUBJECT_INFO = ^_CTL_ANY_SUBJECT_INFO; 
  TCtlAnySubjectInfo = _CTL_ANY_SUBJECT_INFO; 

//+-------------------------------------------------------------------------
//  Find the first or next CTL context in the store.
//
//  The CTL is found according to the dwFindType and its pvFindPara.
//  See below for a list of the find types and its parameters.
//
//  Currently dwFindFlags isn't used and must be set to 0.
//
//  Usage of dwMsgAndCertEncodingType depends on the dwFindType.
//
//  If the first or next CTL isn't found, NULL is returned.
//  Otherwise, a pointer to a read only CTL_CONTEXT is returned. CTL_CONTEXT
//  must be freed by calling CertFreeCTLContext or is freed when passed as the
//  pPrevCtlContext on a subsequent call. CertDuplicateCTLContext
//  can be called to make a duplicate.
//
//  pPrevCtlContext MUST BE NULL on the first
//  call to find the CTL. To find the next CTL, the
//  pPrevCtlContext is set to the CTL_CONTEXT returned by a previous call.
//
//  NOTE: a NON-NULL pPrevCtlContext is always CertFreeCTLContext'ed by
//  this function, even for an error.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertFindCTLInStore}
function CertFindCTLInStore(hCertStore: HCERTSTORE;
  dwMsgAndCertEncodingType, dwFindFlags, dwFindType: DWORD;
  pvFindPara: Pointer; pPrevCtlContext: PCCTL_CONTEXT): PCCTL_CONTEXT; stdcall;

const
  {$EXTERNALSYM CTL_FIND_ANY}
  CTL_FIND_ANY          = 0;
  {$EXTERNALSYM CTL_FIND_SHA1_HASH}
  CTL_FIND_SHA1_HASH    = 1;
  {$EXTERNALSYM CTL_FIND_MD5_HASH}
  CTL_FIND_MD5_HASH     = 2;
  {$EXTERNALSYM CTL_FIND_USAGE}
  CTL_FIND_USAGE        = 3;
  {$EXTERNALSYM CTL_FIND_SUBJECT}
  CTL_FIND_SUBJECT      = 4;
  {$EXTERNALSYM CTL_FIND_EXISTING}
  CTL_FIND_EXISTING     = 5;

type
  PCtlFindUsagePara = ^TCtlFindUsagePara;
  {$EXTERNALSYM _CTL_FIND_USAGE_PARA} 
  _CTL_FIND_USAGE_PARA = record 
    cbSize: DWORD; 
    SubjectUsage: CTL_USAGE;              // optional 
    ListIdentifier: CRYPT_DATA_BLOB;      // optional 
    pSigner: PCERT_INFO;                  // optional 
  end; 
  {$EXTERNALSYM CTL_FIND_USAGE_PARA} 
  CTL_FIND_USAGE_PARA = _CTL_FIND_USAGE_PARA; 
  {$EXTERNALSYM PCTL_FIND_USAGE_PARA} 
  PCTL_FIND_USAGE_PARA = ^_CTL_FIND_USAGE_PARA; 
  TCtlFindUsagePara = _CTL_FIND_USAGE_PARA;

const
  {$EXTERNALSYM CTL_FIND_NO_LIST_ID_CBDATA}
  CTL_FIND_NO_LIST_ID_CBDATA    = $FFFFFFFF;
  {$EXTERNALSYM CTL_FIND_NO_SIGNER_PTR}
  CTL_FIND_NO_SIGNER_PTR        = PCERT_INFO(-1);

  {$EXTERNALSYM CTL_FIND_SAME_USAGE_FLAG}
  CTL_FIND_SAME_USAGE_FLAG      = $1;

type
  PCtlFindSubjectPara = ^TCtlFindSubjectPara;
  {$EXTERNALSYM _CTL_FIND_SUBJECT_PARA} 
  _CTL_FIND_SUBJECT_PARA = record 
    cbSize: DWORD; 
    pUsagePara: PCTL_FIND_USAGE_PARA;     // optional 
    dwSubjectType: DWORD; 
    pvSubject: Pointer; 
  end; 
  {$EXTERNALSYM CTL_FIND_SUBJECT_PARA} 
  CTL_FIND_SUBJECT_PARA = _CTL_FIND_SUBJECT_PARA; 
  {$EXTERNALSYM PCTL_FIND_SUBJECT_PARA} 
  PCTL_FIND_SUBJECT_PARA = ^_CTL_FIND_SUBJECT_PARA; 
  TCtlFindSubjectPara = _CTL_FIND_SUBJECT_PARA; 

//+-------------------------------------------------------------------------
//  CTL_FIND_ANY
//
//  Find any CTL.
//
//  pvFindPara isn't used.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CTL_FIND_SHA1_HASH
//  CTL_FIND_MD5_HASH
//
//  Find a CTL with the specified hash.
//
//  pvFindPara points to a CRYPT_HASH_BLOB.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CTL_FIND_USAGE
//
//  Find a CTL having the specified usage identifiers, list identifier or
//  signer. The CertEncodingType of the signer is obtained from the
//  dwMsgAndCertEncodingType parameter.
//
//  pvFindPara points to a CTL_FIND_USAGE_PARA data structure. The
//  SubjectUsage.cUsageIdentifer can be 0 to match any usage. The
//  ListIdentifier.cbData can be 0 to match any list identifier. To only match
//  CTLs without a ListIdentifier, cbData must be set to
//  CTL_FIND_NO_LIST_ID_CBDATA. pSigner can be NULL to match any signer. Only
//  the Issuer and SerialNumber fields of the pSigner's PCERT_INFO are used.
//  To only match CTLs without a signer, pSigner must be set to
//  CTL_FIND_NO_SIGNER_PTR.
//
//  The CTL_FIND_SAME_USAGE_FLAG can be set in dwFindFlags to
//  only match CTLs with the same usage identifiers. CTLs having additional
//  usage identifiers aren't matched. For example, if only '1.2.3' is specified
//  in CTL_FIND_USAGE_PARA, then, for a match, the CTL must only contain
//  '1.2.3' and not any additional usage identifers.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CTL_FIND_SUBJECT
//
//  Find a CTL having the specified subject. CertFindSubjectInCTL can be
//  called to get a pointer to the subject's entry in the CTL.  pUsagePara can
//  optionally be set to enable the above CTL_FIND_USAGE matching.
//
//  pvFindPara points to a CTL_FIND_SUBJECT_PARA data structure.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  Add the encoded CTL to the store according to the specified
//  disposition option.
//
//  Makes a copy of the encoded CTL before adding to the store.
//
//  dwAddDispostion specifies the action to take if the CTL
//  already exists in the store. See CertAddEncodedCertificateToStore for a
//  list of and actions taken.
//
//  Compares the CTL's SubjectUsage, ListIdentifier and any of its signers
//  to determine if the CTL already exists in the store.
//
//  ppCtlContext can be NULL, indicating the caller isn't interested
//  in getting the CTL_CONTEXT of the added or existing CTL.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertAddEncodedCTLToStore}
function CertAddEncodedCTLToStore(hCertStore: HCERTSTORE;
  dwMsgAndCertEncodingType: DWORD; pbCtlEncoded: PBYTE;
  cbCtlEncoded, dwAddDisposition: DWORD;
  out ppCtlContext: PCCTL_CONTEXT): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Add the CTL context to the store according to the specified
//  disposition option.
//
//  In addition to the encoded CTL, the context's properties are
//  also copied.  Note, the CERT_KEY_CONTEXT_PROP_ID property (and its
//  CERT_KEY_PROV_HANDLE_PROP_ID or CERT_KEY_SPEC_PROP_ID) isn't copied.
//
//  Makes a copy of the encoded CTL before adding to the store.
//
//  dwAddDispostion specifies the action to take if the CTL
//  already exists in the store. See CertAddCertificateContextToStore for a
//  list of and actions taken.
//
//  Compares the CTL's SubjectUsage, ListIdentifier and any of its signers
//  to determine if the CTL already exists in the store.
//
//  ppStoreContext can be NULL, indicating the caller isn't interested
//  in getting the CTL_CONTEXT of the added or existing CTL.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertAddCTLContextToStore}
function CertAddCTLContextToStore(hCertStore: HCERTSTORE;
  pCtlContext: PCCTL_CONTEXT; dwAddDisposition: DWORD;
  out ppStoreContext: PCCTL_CONTEXT): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Serialize the CTL context's encoded CTL and its properties.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertSerializeCTLStoreElement}
function CertSerializeCTLStoreElement(pCtlContext: PCCTL_CONTEXT;
  dwFlags: DWORD; pbElement: PBYTE; out pcbElement: DWORD): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Delete the specified CTL from the store.
//
//  All subsequent gets for the CTL will fail. However,
//  memory allocated for the CTL isn't freed until all of its contexts
//  have also been freed.
//
//  The pCtlContext is obtained from a get or duplicate.
//
//  NOTE: the pCtlContext is always CertFreeCTLContext'ed by
//  this function, even for an error.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertDeleteCTLFromStore}
function CertDeleteCTLFromStore(pCtlContext: PCCTL_CONTEXT): BOOL; stdcall;

{$EXTERNALSYM CertAddCertificateLinkToStore}
function CertAddCertificateLinkToStore(hCertStore: HCERTSTORE;
  pCertContext: PCCERT_CONTEXT; dwAddDisposition: DWORD;
  out ppStoreContext: PCCERT_CONTEXT): BOOL; stdcall;

{$EXTERNALSYM CertAddCRLLinkToStore}
function CertAddCRLLinkToStore(hCertStore: HCERTSTORE;
  pCrlContext: PCCRL_CONTEXT; dwAddDisposition: DWORD;
  out ppStoreContext: PCCRL_CONTEXT): BOOL; stdcall;

{$EXTERNALSYM CertAddCTLLinkToStore}
function CertAddCTLLinkToStore(hCertStore: HCERTSTORE;
  pCtlContext: PCCTL_CONTEXT; dwAddDisposition: DWORD;
  out ppStoreContext: PCCTL_CONTEXT): BOOL; stdcall;

{$EXTERNALSYM CertAddStoreToCollection}
function CertAddStoreToCollection(hCollectionStore, hSiblingStore: HCERTSTORE;
  dwUpdateFlags, dwPriority: DWORD): BOOL; stdcall;

{$EXTERNALSYM CertRemoveStoreFromCollection}
procedure CertRemoveStoreFromCollection(
  hCollectionStore, hSiblingStore: HCERTSTORE); stdcall;

{$EXTERNALSYM CertControlStore}
function CertControlStore(hCertStore: HCERTSTORE; dwFlags, dwCtrlType: DWORD;
  pvCtrlPara: Pointer): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Certificate Store control types
//--------------------------------------------------------------------------
const
  {$EXTERNALSYM CERT_STORE_CTRL_RESYNC}
  CERT_STORE_CTRL_RESYNC                        = 1;
  {$EXTERNALSYM CERT_STORE_CTRL_NOTIFY_CHANGE}
  CERT_STORE_CTRL_NOTIFY_CHANGE                 = 2;
  {$EXTERNALSYM CERT_STORE_CTRL_COMMIT}
  CERT_STORE_CTRL_COMMIT                        = 3;
  {$EXTERNALSYM CERT_STORE_CTRL_AUTO_RESYNC}
  CERT_STORE_CTRL_AUTO_RESYNC                   = 4;
  {$EXTERNALSYM CERT_STORE_CTRL_CANCEL_NOTIFY}
  CERT_STORE_CTRL_CANCEL_NOTIFY                 = 5;

  {$EXTERNALSYM CERT_STORE_CTRL_INHIBIT_DUPLICATE_HANDLE_FLAG}
  CERT_STORE_CTRL_INHIBIT_DUPLICATE_HANDLE_FLAG = $1;

//+-------------------------------------------------------------------------
//  CERT_STORE_CTRL_RESYNC
//
//  Re-synchronize the store.
//
//  The pvCtrlPara points to the event HANDLE to be signaled on
//  the next store change. Normally, this would be the same
//  event HANDLE passed to CERT_STORE_CTRL_NOTIFY_CHANGE during initialization.
//
//  If pvCtrlPara is NULL, no events are re-armed.
//
//  By default the event HANDLE is DuplicateHandle'd.
//  CERT_STORE_CTRL_INHIBIT_DUPLICATE_HANDLE_FLAG can be set in dwFlags
//  to inhibit a DupicateHandle of the event HANDLE. If this flag
//  is set, then, CertControlStore(CERT_STORE_CTRL_CANCEL_NOTIFY) must be
//  called for this event HANDLE before closing the hCertStore.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CERT_STORE_CTRL_NOTIFY_CHANGE
//
//  Signal the event when the underlying store is changed.
//
//  pvCtrlPara points to the event HANDLE to be signaled.
//
//  pvCtrlPara can be NULL to inform the store of a subsequent
//  CERT_STORE_CTRL_RESYNC and allow it to optimize by only doing a resync
//  if the store has changed. For the registry based stores, an internal
//  notify change event is created and registered to be signaled.
//
//  Recommend calling CERT_STORE_CTRL_NOTIFY_CHANGE once for each event to
//  be passed to CERT_STORE_CTRL_RESYNC. This should only happen after
//  the event has been created. Not after each time the event is signaled.
//
//  By default the event HANDLE is DuplicateHandle'd.
//  CERT_STORE_CTRL_INHIBIT_DUPLICATE_HANDLE_FLAG can be set in dwFlags
//  to inhibit a DupicateHandle of the event HANDLE. If this flag
//  is set, then, CertControlStore(CERT_STORE_CTRL_CANCEL_NOTIFY) must be
//  called for this event HANDLE before closing the hCertStore.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CERT_STORE_CTRL_CANCEL_NOTIFY
//
//  Cancel notification signaling of the event HANDLE passed in a previous
//  CERT_STORE_CTRL_NOTIFY_CHANGE or CERT_STORE_CTRL_RESYNC.
//
//  pvCtrlPara points to the event HANDLE to be canceled.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CERT_STORE_CTRL_AUTO_RESYNC
//
//  At the start of every enumeration or find store API call, check if the
//  underlying store has changed. If it has changed, re-synchronize.
//
//  This check is only done in the enumeration or find APIs when the
//  pPrevContext is NULL.
//
//  The pvCtrlPara isn't used and must be set to NULL.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CERT_STORE_CTRL_COMMIT
//
//  If any changes have been to the cached store, they are committed to
//  persisted storage. If no changes have been made since the store was
//  opened or the last commit, this call is ignored. May also be ignored by
//  store providers that persist changes immediately.
//
//  CERT_STORE_CTRL_COMMIT_FORCE_FLAG can be set to force the store
//  to be committed even if it hasn't been touched.
//
//  CERT_STORE_CTRL_COMMIT_CLEAR_FLAG can be set to inhibit a commit on
//  store close.
//--------------------------------------------------------------------------

  {$EXTERNALSYM CERT_STORE_CTRL_COMMIT_FORCE_FLAG}
  CERT_STORE_CTRL_COMMIT_FORCE_FLAG = $1;
  {$EXTERNALSYM CERT_STORE_CTRL_COMMIT_CLEAR_FLAG}
  CERT_STORE_CTRL_COMMIT_CLEAR_FLAG = $2;

//+=========================================================================
//  Cert Store Property Defines and APIs
//==========================================================================

//+-------------------------------------------------------------------------
//  Store property IDs. This is a property applicable to the entire store.
//  Its not a property on an individual certificate, CRL or CTL context.
//
//  Currently, no store properties are persisted. (This differs from
//  most context properties which are persisted.)
//
//  See CertSetStoreProperty or CertGetStoreProperty for usage information.
//
//  Note, the range for predefined store properties should be outside
//  the range of predefined context properties. We will start at 4096.
//--------------------------------------------------------------------------
// certenrolld_begin -- CERT_*_PROP_ID
  {$EXTERNALSYM CERT_STORE_LOCALIZED_NAME_PROP_ID}
  CERT_STORE_LOCALIZED_NAME_PROP_ID = $1000;
// certenrolld_end

//+-------------------------------------------------------------------------
//  Set a store property.
//
//  The type definition for pvData depends on the dwPropId value.
//      CERT_STORE_LOCALIZED_NAME_PROP_ID - localized name of the store.
//      pvData points to a CRYPT_DATA_BLOB. pbData is a pointer to a NULL
//      terminated unicode, wide character string.
//      cbData = (wcslen((LPWSTR) pbData) + 1) * sizeof(WCHAR).
//
//  For all the other PROP_IDs: an encoded PCRYPT_DATA_BLOB is passed in pvData.
//
//  If the property already exists, then, the old value is deleted and silently
//  replaced. Setting, pvData to NULL, deletes the property.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertSetStoreProperty}
function CertSetStoreProperty(hCertStore: HCERTSTORE; dwPropId, dwFlags: DWORD;
  pvData: Pointer): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Get a store property.
//
//  The type definition for pvData depends on the dwPropId value.
//      CERT_STORE_LOCALIZED_NAME_PROP_ID - localized name of the store.
//      pvData points to a NULL terminated unicode, wide character string.
//      cbData = (wcslen((LPWSTR) pvData) + 1) * sizeof(WCHAR).
//
//  For all other PROP_IDs, pvData points to an array of bytes.
//
//  If the property doesn't exist, returns FALSE and sets LastError to
//  CRYPT_E_NOT_FOUND.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertGetStoreProperty}
function CertGetStoreProperty(hCertStore: HCERTSTORE;
  dwPropId: DWORD; pvData: Pointer; out pcbData: DWORD): BOOL; stdcall;

//+-------------------------------------------------------------------------
// If the callback returns FALSE, stops the sort. CertCreateContext
// will return FALSE and set last error to ERROR_CANCELLED if the sort
// was stopped.
//
// Where:
//  cbTotalEncoded  - total byte count of the encoded entries.
//  cbRemainEncoded - remaining byte count of the encoded entries.
//  cEntry          - running count of sorted entries
//  pvSort          - value passed in pCreatePara
//--------------------------------------------------------------------------
type
  {$EXTERNALSYM PFN_CERT_CREATE_CONTEXT_SORT_FUNC}
  PFN_CERT_CREATE_CONTEXT_SORT_FUNC = function(
    cbTotalEncoded, cbRemainEncoded, cEntry: DWORD;
    pvSort: Pointer): BOOL stdcall;
  TFnCertCreateContextSortFunc = PFN_CERT_CREATE_CONTEXT_SORT_FUNC;

  PCertCreateContextPara = ^TCertCreateContextPara;
  {$EXTERNALSYM _CERT_CREATE_CONTEXT_PARA}
  _CERT_CREATE_CONTEXT_PARA = record
    cbSize: DWORD;
    pfnFree: PFN_CRYPT_FREE;                     // OPTIONAL
    pvFree: Pointer;                             // OPTIONAL
    
    // Only applicable to CERT_STORE_CTL_CONTEXT when
    // CERT_CREATE_CONTEXT_SORTED_FLAG is set in dwFlags.
    pfnSort: PFN_CERT_CREATE_CONTEXT_SORT_FUNC;  // OPTIONAL
    pvSort: Pointer;                             // OPTIONAL
  end;
  {$EXTERNALSYM CERT_CREATE_CONTEXT_PARA}
  CERT_CREATE_CONTEXT_PARA = _CERT_CREATE_CONTEXT_PARA;
  {$EXTERNALSYM PCERT_CREATE_CONTEXT_PARA}
  PCERT_CREATE_CONTEXT_PARA = ^_CERT_CREATE_CONTEXT_PARA;
  TCertCreateContextPara = _CERT_CREATE_CONTEXT_PARA;

//+-------------------------------------------------------------------------
//  Creates the specified context from the encoded bytes. The created
//  context isn't put in a store.
//
//  dwContextType values:
//      CERT_STORE_CERTIFICATE_CONTEXT
//      CERT_STORE_CRL_CONTEXT
//      CERT_STORE_CTL_CONTEXT
//
//  If CERT_CREATE_CONTEXT_NOCOPY_FLAG is set, the created context points
//  directly to the pbEncoded instead of an allocated copy. See flag
//  definition for more details.
//
//  If CERT_CREATE_CONTEXT_SORTED_FLAG is set, the context is created
//  with sorted entries. This flag may only be set for CERT_STORE_CTL_CONTEXT.
//  Setting this flag implicitly sets CERT_CREATE_CONTEXT_NO_HCRYPTMSG_FLAG and
//  CERT_CREATE_CONTEXT_NO_ENTRY_FLAG. See flag definition for
//  more details.
//
//  If CERT_CREATE_CONTEXT_NO_HCRYPTMSG_FLAG is set, the context is created
//  without creating a HCRYPTMSG handle for the context. This flag may only be
//  set for CERT_STORE_CTL_CONTEXT.  See flag definition for more details.
//
//  If CERT_CREATE_CONTEXT_NO_ENTRY_FLAG is set, the context is created
//  without decoding the entries. This flag may only be set for
//  CERT_STORE_CTL_CONTEXT.  See flag definition for more details.
//
//  If unable to decode and create the context, NULL is returned.
//  Otherwise, a pointer to a read only CERT_CONTEXT, CRL_CONTEXT or
//  CTL_CONTEXT is returned. The context must be freed by the appropriate
//  free context API. The context can be duplicated by calling the
//  appropriate duplicate context API.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertCreateContext}
function CertCreateContext(dwContextType, dwEncodingType: DWORD;
  pbEncoded: PBYTE; cbEncoded, dwFlags: DWORD;
  var pCreatePara: CERT_CREATE_CONTEXT_PARA): Pointer; stdcall;

const
// When the following flag is set, the created context points directly to the
// pbEncoded instead of an allocated copy. If pCreatePara and
// pCreatePara->pfnFree are non-NULL, then, pfnFree is called to free
// the pbEncoded when the context is last freed. Otherwise, no attempt is
// made to free the pbEncoded. If pCreatePara->pvFree is non-NULL, then its
// passed to pfnFree instead of pbEncoded.
//
// Note, if CertCreateContext fails, pfnFree is still called.
  {$EXTERNALSYM CERT_CREATE_CONTEXT_NOCOPY_FLAG}
  CERT_CREATE_CONTEXT_NOCOPY_FLAG               = $1;

// When the following flag is set, a context with sorted entries is created.
// Currently only applicable to a CTL context.
//
// For CTLs: the cCTLEntry in the returned CTL_INFO is always
// 0. CertFindSubjectInSortedCTL and CertEnumSubjectInSortedCTL must be called
// to find or enumerate the CTL entries.
//
// The Sorted CTL TrustedSubjects extension isn't returned in the created
// context's CTL_INFO.
//
// pfnSort and pvSort can be set in the pCreatePara parameter to be called for
// each sorted entry. pfnSort can return FALSE to stop the sorting.
  {$EXTERNALSYM CERT_CREATE_CONTEXT_SORTED_FLAG}
  CERT_CREATE_CONTEXT_SORTED_FLAG               = $2;

// By default when a CTL context is created, a HCRYPTMSG handle to its
// SignedData message is created. This flag can be set to improve performance
// by not creating the HCRYPTMSG handle.
//
// This flag is only applicable to a CTL context.
  {$EXTERNALSYM CERT_CREATE_CONTEXT_NO_HCRYPTMSG_FLAG}
  CERT_CREATE_CONTEXT_NO_HCRYPTMSG_FLAG         = $4;

// By default when a CTL context is created, its entries are decoded.
// This flag can be set to improve performance by not decoding the
// entries.
//
// This flag is only applicable to a CTL context.
  {$EXTERNALSYM CERT_CREATE_CONTEXT_NO_ENTRY_FLAG}
  CERT_CREATE_CONTEXT_NO_ENTRY_FLAG             = $8;

//+=========================================================================
//  Certificate System Store Data Structures and APIs
//==========================================================================

//+-------------------------------------------------------------------------
//  System Store Information
//
//  Currently, no system store information is persisted.
//--------------------------------------------------------------------------
type
  PCertSystemStoreInfo = ^TCertSystemStoreInfo;
  {$EXTERNALSYM _CERT_SYSTEM_STORE_INFO}
  _CERT_SYSTEM_STORE_INFO = record
    cbSize: DWORD;
  end;
  {$EXTERNALSYM CERT_SYSTEM_STORE_INFO}
  CERT_SYSTEM_STORE_INFO = _CERT_SYSTEM_STORE_INFO;
  {$EXTERNALSYM PCERT_SYSTEM_STORE_INFO}
  PCERT_SYSTEM_STORE_INFO = ^_CERT_SYSTEM_STORE_INFO;
  TCertSystemStoreInfo = _CERT_SYSTEM_STORE_INFO;

//+-------------------------------------------------------------------------
//  Physical Store Information
//
//  The Open fields are passed directly to CertOpenStore() to open
//  the physical store.
//
//  By default all system stores located in the registry have an
//  implicit SystemRegistry physical store that is opened. To disable the
//  opening of this store, the SystemRegistry
//  physical store corresponding to the System store must be registered with
//  CERT_PHYSICAL_STORE_OPEN_DISABLE_FLAG set in dwFlags. Alternatively,
//  a physical store with the name of '.Default' may be registered.
//
//  Depending on the store location and store name, additional predefined
//  physical stores may be opened. For example, system stores in
//  CURRENT_USER have the predefined physical store, .LocalMachine.
//  To disable the opening of these predefined physical stores, the
//  corresponding physical store must be registered with
//  CERT_PHYSICAL_STORE_OPEN_DISABLE_FLAG set in dwFlags.
//
//  The CERT_PHYSICAL_STORE_ADD_ENABLE_FLAG must be set in dwFlags
//  to enable the adding of a context to the store.
//
//  When a system store is opened via the SERVICES or USERS store location,
//  the ServiceName\ is prepended to the OpenParameters
//  for CERT_SYSTEM_STORE_CURRENT_USER or CERT_SYSTEM_STORE_CURRENT_SERVICE
//  physical stores and the dwOpenFlags store location is changed to
//  CERT_SYSTEM_STORE_USERS or CERT_SYSTEM_STORE_SERVICES.
//
//  By default the SYSTEM, SYSTEM_REGISTRY and PHYSICAL provider
//  stores are also opened remotely when the outer system store is opened.
//  The CERT_PHYSICAL_STORE_REMOTE_OPEN_DISABLE_FLAG may be set in dwFlags
//  to disable remote opens.
//
//  When opened remotely, the \\ComputerName is implicitly prepended to the
//  OpenParameters for the SYSTEM, SYSTEM_REGISTRY and PHYSICAL provider types.
//  To also prepend the \\ComputerName to other provider types, set the
//  CERT_PHYSICAL_STORE_INSERT_COMPUTER_NAME_ENABLE_FLAG in dwFlags.
//
//  When the system store is opened, its physical stores are ordered
//  according to the dwPriority. A larger dwPriority indicates higher priority.
//--------------------------------------------------------------------------
  PCertPhysicalStoreInfo = ^TCertPhysicalStoreInfo;
  {$EXTERNALSYM _CERT_PHYSICAL_STORE_INFO}
  _CERT_PHYSICAL_STORE_INFO = record
    cbSize: DWORD;
    pszOpenStoreProvider: LPSTR;          // REG_SZ
    dwOpenEncodingType: DWORD;            // REG_DWORD
    dwOpenFlags: DWORD;                   // REG_DWORD
    OpenParameters: CRYPT_DATA_BLOB;      // REG_BINARY
    dwFlags: DWORD;                       // REG_DWORD
    dwPriority: DWORD;                    // REG_DWORD
  end;
  {$EXTERNALSYM CERT_PHYSICAL_STORE_INFO}
  CERT_PHYSICAL_STORE_INFO = _CERT_PHYSICAL_STORE_INFO;
  {$EXTERNALSYM PCERT_PHYSICAL_STORE_INFO}
  PCERT_PHYSICAL_STORE_INFO = ^_CERT_PHYSICAL_STORE_INFO;
  TCertPhysicalStoreInfo = _CERT_PHYSICAL_STORE_INFO;

//+-------------------------------------------------------------------------
//  Physical Store Information dwFlags
//--------------------------------------------------------------------------
const
  {$EXTERNALSYM CERT_PHYSICAL_STORE_ADD_ENABLE_FLAG}
  CERT_PHYSICAL_STORE_ADD_ENABLE_FLAG                   = $1;
  {$EXTERNALSYM CERT_PHYSICAL_STORE_OPEN_DISABLE_FLAG}
  CERT_PHYSICAL_STORE_OPEN_DISABLE_FLAG                 = $2;
  {$EXTERNALSYM CERT_PHYSICAL_STORE_REMOTE_OPEN_DISABLE_FLAG}
  CERT_PHYSICAL_STORE_REMOTE_OPEN_DISABLE_FLAG          = $4;
  {$EXTERNALSYM CERT_PHYSICAL_STORE_INSERT_COMPUTER_NAME_ENABLE_FLAG}
  CERT_PHYSICAL_STORE_INSERT_COMPUTER_NAME_ENABLE_FLAG  = $8;

//+-------------------------------------------------------------------------
//  Register a system store.
//
//  The upper word of the dwFlags parameter is used to specify the location of
//  the system store.
//
//  If CERT_SYSTEM_STORE_RELOCATE_FLAG is set in dwFlags, pvSystemStore
//  points to a CERT_SYSTEM_STORE_RELOCATE_PARA data structure. Otherwise,
//  pvSystemStore points to a null terminated UNICODE string.
//
//  The CERT_SYSTEM_STORE_SERVICES or CERT_SYSTEM_STORE_USERS system store
//  name must be prefixed with the ServiceName or UserName. For example,
//  'ServiceName\Trust'.
//
//  Stores on remote computers can be registered for the
//  CERT_SYSTEM_STORE_LOCAL_MACHINE, CERT_SYSTEM_STORE_SERVICES,
//  CERT_SYSTEM_STORE_USERS, CERT_SYSTEM_STORE_LOCAL_MACHINE_GROUP_POLICY
//  or CERT_SYSTEM_STORE_LOCAL_MACHINE_ENTERPRISE
//  locations by prepending the computer name. For example, a remote
//  local machine store is registered via '\\ComputerName\Trust' or
//  'ComputerName\Trust'. A remote service store is registered via
//  '\\ComputerName\ServiceName\Trust'. The leading '\\' backslashes are
//  optional in the ComputerName.
//
//  Set CERT_STORE_CREATE_NEW_FLAG to cause a failure if the system store
//  already exists in the store location.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertRegisterSystemStore}
function CertRegisterSystemStore(pvSystemStore: Pointer;
  dwFlags: DWORD; var pStoreInfo: CERT_SYSTEM_STORE_INFO;
  pvReserved: Pointer): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Register a physical store for the specified system store.
//
//  The upper word of the dwFlags parameter is used to specify the location of
//  the system store.
//
//  If CERT_SYSTEM_STORE_RELOCATE_FLAG is set in dwFlags, pvSystemStore
//  points to a CERT_SYSTEM_STORE_RELOCATE_PARA data structure. Otherwise,
//  pvSystemStore points to a null terminated UNICODE string.
//
//  See CertRegisterSystemStore for details on prepending a ServiceName
//  and/or ComputerName to the system store name.
//
//  Set CERT_STORE_CREATE_NEW_FLAG to cause a failure if the physical store
//  already exists in the system store.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertRegisterPhysicalStore}
function CertRegisterPhysicalStore(pvSystemStore: Pointer; dwFlags: DWORD;
  pwszStoreName: LPCWSTR; var pStoreInfo: CERT_PHYSICAL_STORE_INFO;
  pvReserved: Pointer): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Unregister the specified system store.
//
//  The upper word of the dwFlags parameter is used to specify the location of
//  the system store.
//
//  If CERT_SYSTEM_STORE_RELOCATE_FLAG is set in dwFlags, pvSystemStore
//  points to a CERT_SYSTEM_STORE_RELOCATE_PARA data structure. Otherwise,
//  pvSystemStore points to a null terminated UNICODE string.
//
//  See CertRegisterSystemStore for details on prepending a ServiceName
//  and/or ComputerName to the system store name.
//
//  CERT_STORE_DELETE_FLAG can optionally be set in dwFlags.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertUnregisterSystemStore}
function CertUnregisterSystemStore(pvSystemStore: Pointer;
  dwFlags: DWORD): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Unregister the physical store from the specified system store.
//
//  The upper word of the dwFlags parameter is used to specify the location of
//  the system store.
//
//  If CERT_SYSTEM_STORE_RELOCATE_FLAG is set in dwFlags, pvSystemStore
//  points to a CERT_SYSTEM_STORE_RELOCATE_PARA data structure. Otherwise,
//  pvSystemStore points to a null terminated UNICODE string.
//
//  See CertRegisterSystemStore for details on prepending a ServiceName
//  and/or ComputerName to the system store name.
//
//  CERT_STORE_DELETE_FLAG can optionally be set in dwFlags.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertUnregisterPhysicalStore}
function CertUnregisterPhysicalStore(pvSystemStore: Pointer;
  dwFlags: DWORD; pwszStoreName: LPCWSTR): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Enum callbacks
//
//  The CERT_SYSTEM_STORE_LOCATION_MASK bits in the dwFlags parameter
//  specifies the location of the system store
//
//  If CERT_SYSTEM_STORE_RELOCATE_FLAG is set in dwFlags, pvSystemStore
//  points to a CERT_SYSTEM_STORE_RELOCATE_PARA data structure. Otherwise,
//  pvSystemStore points to a null terminated UNICODE string.
//
//  The callback returns FALSE and sets LAST_ERROR to stop the enumeration.
//  The LAST_ERROR is returned to the caller of the enumeration.
//
//  The pvSystemStore passed to the callback has leading ComputerName and/or
//  ServiceName prefixes where appropriate.
//--------------------------------------------------------------------------
type
  {$EXTERNALSYM PFN_CERT_ENUM_SYSTEM_STORE_LOCATION}
  PFN_CERT_ENUM_SYSTEM_STORE_LOCATION = function(pwszStoreLocation: LPCWSTR;
    dwFlags: DWORD; pvReserved, pvArg: Pointer): BOOL stdcall;
  TFnCertEnumSystemStoreLocation = PFN_CERT_ENUM_SYSTEM_STORE_LOCATION;

  {$EXTERNALSYM PFN_CERT_ENUM_SYSTEM_STORE}
  PFN_CERT_ENUM_SYSTEM_STORE = function(pvSystemStore: Pointer;
    dwFlags: DWORD; var pStoreInfo: CERT_SYSTEM_STORE_INFO;
    pvReserved, pvArg: Pointer): BOOL stdcall;
  TFnCertEnumSystemStore = PFN_CERT_ENUM_SYSTEM_STORE;

  {$EXTERNALSYM PFN_CERT_ENUM_PHYSICAL_STORE}
  PFN_CERT_ENUM_PHYSICAL_STORE = function(pvSystemStore: Pointer;
    dwFlags: DWORD; pwszStoreName: LPCWSTR;
    var pStoreInfo: CERT_PHYSICAL_STORE_INFO;
    pvReserved, pvArg: Pointer): BOOL stdcall;
  TFnCertEnumPhysicalStore = PFN_CERT_ENUM_PHYSICAL_STORE;

const
// In the PFN_CERT_ENUM_PHYSICAL_STORE callback the following flag is
// set if the physical store wasn't registered and is an implicitly created
// predefined physical store.
  {$EXTERNALSYM CERT_PHYSICAL_STORE_PREDEFINED_ENUM_FLAG}
  CERT_PHYSICAL_STORE_PREDEFINED_ENUM_FLAG      = $1;

// Names of implicitly created predefined physical stores
  {$EXTERNALSYM CERT_PHYSICAL_STORE_DEFAULT_NAME}
  CERT_PHYSICAL_STORE_DEFAULT_NAME              = '.Default';
  {$EXTERNALSYM CERT_PHYSICAL_STORE_GROUP_POLICY_NAME}
  CERT_PHYSICAL_STORE_GROUP_POLICY_NAME         = '.GroupPolicy';
  {$EXTERNALSYM CERT_PHYSICAL_STORE_LOCAL_MACHINE_NAME}
  CERT_PHYSICAL_STORE_LOCAL_MACHINE_NAME        = '.LocalMachine';
  {$EXTERNALSYM CERT_PHYSICAL_STORE_DS_USER_CERTIFICATE_NAME}
  CERT_PHYSICAL_STORE_DS_USER_CERTIFICATE_NAME  = '.UserCertificate';
  {$EXTERNALSYM CERT_PHYSICAL_STORE_LOCAL_MACHINE_GROUP_POLICY_NAME}
  CERT_PHYSICAL_STORE_LOCAL_MACHINE_GROUP_POLICY_NAME =
    '.LocalMachineGroupPolicy';
  {$EXTERNALSYM CERT_PHYSICAL_STORE_ENTERPRISE_NAME}
  CERT_PHYSICAL_STORE_ENTERPRISE_NAME           = '.Enterprise';
  {$EXTERNALSYM CERT_PHYSICAL_STORE_AUTH_ROOT_NAME}
  CERT_PHYSICAL_STORE_AUTH_ROOT_NAME            = '.AuthRoot';
  {$EXTERNALSYM CERT_PHYSICAL_STORE_SMART_CARD_NAME}
  CERT_PHYSICAL_STORE_SMART_CARD_NAME           = '.SmartCard';

//+-------------------------------------------------------------------------
//  Enumerate the system store locations.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertEnumSystemStoreLocation}
function CertEnumSystemStoreLocation(dwFlags: DWORD; pvArg: Pointer;
  pfnEnum: PFN_CERT_ENUM_SYSTEM_STORE_LOCATION): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Enumerate the system stores.
//
//  The upper word of the dwFlags parameter is used to specify the location of
//  the system store.
//
//  If CERT_SYSTEM_STORE_RELOCATE_FLAG is set in dwFlags,
//  pvSystemStoreLocationPara points to a CERT_SYSTEM_STORE_RELOCATE_PARA
//  data structure. Otherwise, pvSystemStoreLocationPara points to a null
//  terminated UNICODE string.
//
//  For CERT_SYSTEM_STORE_LOCAL_MACHINE,
//  CERT_SYSTEM_STORE_LOCAL_MACHINE_GROUP_POLICY or
//  CERT_SYSTEM_STORE_LOCAL_MACHINE_ENTERPRISE, pvSystemStoreLocationPara can
//  optionally be set to a unicode computer name for enumerating local machine
//  stores on a remote computer. For example, '\\ComputerName' or
//  'ComputerName'.  The leading '\\' backslashes are optional in the
//  ComputerName.
//
//  For CERT_SYSTEM_STORE_SERVICES or CERT_SYSTEM_STORE_USERS,
//  if pvSystemStoreLocationPara is NULL, then,
//  enumerates both the service/user names and the stores for each service/user
//  name. Otherwise, pvSystemStoreLocationPara is a unicode string specifying a
//  remote computer name and/or service/user name. For example:
//      'ServiceName'
//      '\\ComputerName' or 'ComputerName\'
//      'ComputerName\ServiceName'
//  Note, if only the ComputerName is specified, then, it must have either
//  the leading '\\' backslashes or a trailing backslash. Otherwise, its
//  interpretted as the ServiceName or UserName.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertEnumSystemStore}
function CertEnumSystemStore(dwFlags: DWORD;
  pvSystemStoreLocationPara, pvArg: Pointer;
  pfnEnum: PFN_CERT_ENUM_SYSTEM_STORE): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Enumerate the physical stores for the specified system store.
//
//  The upper word of the dwFlags parameter is used to specify the location of
//  the system store.
//
//  If CERT_SYSTEM_STORE_RELOCATE_FLAG is set in dwFlags, pvSystemStore
//  points to a CERT_SYSTEM_STORE_RELOCATE_PARA data structure. Otherwise,
//  pvSystemStore points to a null terminated UNICODE string.
//
//  See CertRegisterSystemStore for details on prepending a ServiceName
//  and/or ComputerName to the system store name.
//
//  If the system store location only supports system stores and doesn't
//  support physical stores, LastError is set to ERROR_CALL_NOT_IMPLEMENTED.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertEnumPhysicalStore}
function CertEnumPhysicalStore(pvSystemStore: Pointer; dwFlags: DWORD;
  pvArg: Pointer; pfnEnum: PFN_CERT_ENUM_PHYSICAL_STORE): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Certificate System Store Installable Functions
//
//  The CERT_SYSTEM_STORE_LOCATION_MASK bits in the dwFlags parameter passed
//  to the CertOpenStore(for 'System', 'SystemRegistry' or 'Physical'
//  Provider), CertRegisterSystemStore,
//  CertUnregisterSystemStore, CertEnumSystemStore, CertRegisterPhysicalStore,
//  CertUnregisterPhysicalStore and CertEnumPhysicalStore APIs is used as the
//  constant pszOID value passed to the OID installable functions.
//  Therefore, the pszOID is restricted to a constant <= (LPCSTR) $0FFF.
//
//  The EncodingType is 0.
//--------------------------------------------------------------------------

// Installable System Store Provider OID pszFuncNames.
const
  {$EXTERNALSYM CRYPT_OID_OPEN_SYSTEM_STORE_PROV_FUNC}
  CRYPT_OID_OPEN_SYSTEM_STORE_PROV_FUNC    = 'CertDllOpenSystemStoreProv';
  {$EXTERNALSYM CRYPT_OID_REGISTER_SYSTEM_STORE_FUNC}
  CRYPT_OID_REGISTER_SYSTEM_STORE_FUNC     = 'CertDllRegisterSystemStore';
  {$EXTERNALSYM CRYPT_OID_UNREGISTER_SYSTEM_STORE_FUNC}
  CRYPT_OID_UNREGISTER_SYSTEM_STORE_FUNC   = 'CertDllUnregisterSystemStore';
  {$EXTERNALSYM CRYPT_OID_ENUM_SYSTEM_STORE_FUNC}
  CRYPT_OID_ENUM_SYSTEM_STORE_FUNC         = 'CertDllEnumSystemStore';
  {$EXTERNALSYM CRYPT_OID_REGISTER_PHYSICAL_STORE_FUNC}
  CRYPT_OID_REGISTER_PHYSICAL_STORE_FUNC   = 'CertDllRegisterPhysicalStore';
  {$EXTERNALSYM CRYPT_OID_UNREGISTER_PHYSICAL_STORE_FUNC}
  CRYPT_OID_UNREGISTER_PHYSICAL_STORE_FUNC = 'CertDllUnregisterPhysicalStore';
  {$EXTERNALSYM CRYPT_OID_ENUM_PHYSICAL_STORE_FUNC}
  CRYPT_OID_ENUM_PHYSICAL_STORE_FUNC       = 'CertDllEnumPhysicalStore';

// CertDllOpenSystemStoreProv has the same function signature as the
// installable 'CertDllOpenStoreProv' function. See CertOpenStore for
// more details.

// CertDllRegisterSystemStore has the same function signature as
// CertRegisterSystemStore.
//
// The 'SystemStoreLocation' REG_SZ value must also be set for registered
// CertDllEnumSystemStore OID functions.
  {$EXTERNALSYM CRYPT_OID_SYSTEM_STORE_LOCATION_VALUE_NAME}
  CRYPT_OID_SYSTEM_STORE_LOCATION_VALUE_NAME  = 'SystemStoreLocation';

// The remaining Register, Enum and Unregister OID installable functions
// have the same signature as their Cert Store API counterpart.


//+=========================================================================
//  Enhanced Key Usage Helper Functions
//==========================================================================

//+-------------------------------------------------------------------------
//  Get the enhanced key usage extension or property from the certificate
//  and decode.
//
//  If the CERT_FIND_EXT_ONLY_ENHKEY_USAGE_FLAG is set, then, only get the
//  extension.
//
//  If the CERT_FIND_PROP_ONLY_ENHKEY_USAGE_FLAG is set, then, only get the
//  property.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertGetEnhancedKeyUsage}
function CertGetEnhancedKeyUsage(pCertContext: PCCERT_CONTEXT;
  dwFlags: DWORD; var pUsage: CERT_ENHKEY_USAGE;
  out pcbUsage: DWORD): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Set the enhanced key usage property for the certificate.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertSetEnhancedKeyUsage}
function CertSetEnhancedKeyUsage(pCertContext: PCCERT_CONTEXT;
  var pUsage: CERT_ENHKEY_USAGE): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Add the usage identifier to the certificate's enhanced key usage property.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertAddEnhancedKeyUsageIdentifier}
function CertAddEnhancedKeyUsageIdentifier(pCertContext: PCCERT_CONTEXT;
  pszUsageIdentifier: LPCSTR): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Remove the usage identifier from the certificate's enhanced key usage
//  property.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertRemoveEnhancedKeyUsageIdentifier}
function CertRemoveEnhancedKeyUsageIdentifier(pCertContext: PCCERT_CONTEXT;
  pszUsageIdentifier: LPCSTR): BOOL; stdcall;

//+---------------------------------------------------------------------------
//
//  Takes an array of certs and returns an array of usages
//  which consists of the intersection of the valid usages for each cert.
//  If each cert is good for all possible usages then the cNumOIDs is set to -1.
//
//----------------------------------------------------------------------------
{$EXTERNALSYM CertGetValidUsages}
function CertGetValidUsages(cCerts: DWORD; rghCerts: PPCCERT_CONTEXT;
  var cNumOIDs: Integer; rghOIDs: PLPSTR; out pcbOIDs: DWORD): BOOL; stdcall;

//+=========================================================================
//  Cryptographic Message helper functions for verifying and signing a
//  CTL.
//==========================================================================

//+-------------------------------------------------------------------------
//  Get and verify the signer of a cryptographic message.
//
//  To verify a CTL, the hCryptMsg is obtained from the CTL_CONTEXT's
//  hCryptMsg field.
//
//  If CMSG_TRUSTED_SIGNER_FLAG is set, then, treat the Signer stores as being
//  trusted and only search them to find the certificate corresponding to the
//  signer's issuer and serial number.  Otherwise, the SignerStores are
//  optionally provided to supplement the message's store of certificates.
//  If a signer certificate is found, its public key is used to verify
//  the message signature. The CMSG_SIGNER_ONLY_FLAG can be set to
//  return the signer without doing the signature verify.
//
//  If CMSG_USE_SIGNER_INDEX_FLAG is set, then, only get the signer specified
//  by *pdwSignerIndex. Otherwise, iterate through all the signers
//  until a signer verifies or no more signers.
//
//  For a verified signature, *ppSigner is updated with certificate context
//  of the signer and *pdwSignerIndex is updated with the index of the signer.
//  ppSigner and/or pdwSignerIndex can be NULL, indicating the caller isn't
//  interested in getting the CertContext and/or index of the signer.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptMsgGetAndVerifySigner}
function CryptMsgGetAndVerifySigner(hCryptMsg: HCRYPTMSG;
  cSignerStore: DWORD; rghSignerStore: PHCERTSTORE;
  dwFlags: DWORD; out ppSigner: PCCERT_CONTEXT;
  var pdwSignerIndex: DWORD): BOOL; stdcall;

const
  {$EXTERNALSYM CMSG_TRUSTED_SIGNER_FLAG}
  CMSG_TRUSTED_SIGNER_FLAG      = $1;
  {$EXTERNALSYM CMSG_SIGNER_ONLY_FLAG}
  CMSG_SIGNER_ONLY_FLAG         = $2;
  {$EXTERNALSYM CMSG_USE_SIGNER_INDEX_FLAG}
  CMSG_USE_SIGNER_INDEX_FLAG    = $4;

//+-------------------------------------------------------------------------
//  Sign an encoded CTL.
//
//  The pbCtlContent can be obtained via a CTL_CONTEXT's pbCtlContent
//  field or via a CryptEncodeObject(PKCS_CTL or PKCS_SORTED_CTL).
//
//  CMSG_CMS_ENCAPSULATED_CTL_FLAG can be set to encode a CMS compatible
//  V3 SignedData message.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptMsgSignCTL}
function CryptMsgSignCTL(dwMsgEncodingType: DWORD; pbCtlContent: PBYTE;
  cbCtlContent: DWORD; var pSignInfo: CMSG_SIGNED_ENCODE_INFO;
  dwFlags: DWORD; pbEncoded: PBYTE; out pcbEncoded: DWORD): BOOL; stdcall;

const
// When set, CTL inner content is encapsulated within an OCTET STRING
  {$EXTERNALSYM CMSG_CMS_ENCAPSULATED_CTL_FLAG}
  CMSG_CMS_ENCAPSULATED_CTL_FLAG = $00008000;

//+-------------------------------------------------------------------------
//  Encode the CTL and create a signed message containing the encoded CTL.
//
//  Set CMSG_ENCODE_SORTED_CTL_FLAG if the CTL entries are to be sorted
//  before encoding. This flag should be set, if the
//  CertFindSubjectInSortedCTL or CertEnumSubjectInSortedCTL APIs will
//  be called. If the identifier for the CTL entries is a hash, such as,
//  MD5 or SHA1, then, CMSG_ENCODE_HASHED_SUBJECT_IDENTIFIER_FLAG should
//  also be set.
//
//  CMSG_CMS_ENCAPSULATED_CTL_FLAG can be set to encode a CMS compatible
//  V3 SignedData message.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptMsgEncodeAndSignCTL}
function CryptMsgEncodeAndSignCTL(dwMsgEncodingType: DWORD;
  var pCtlInfo: CTL_INFO; var pSignInfo: CMSG_SIGNED_ENCODE_INFO;
  dwFlags: DWORD; pbEncoded: PBYTE; out pcbEncoded: DWORD): BOOL; stdcall;

const
//  The following flag is set if the CTL is to be encoded with sorted
//  trusted subjects and the szOID_SORTED_CTL extension is inserted containing
//  sorted offsets to the encoded subjects.
  {$EXTERNALSYM CMSG_ENCODE_SORTED_CTL_FLAG}
  CMSG_ENCODE_SORTED_CTL_FLAG                = $1;

//  If the above sorted flag is set, then, the following flag should also
//  be set if the identifier for the TrustedSubjects is a hash,
//  such as, MD5 or SHA1.
  {$EXTERNALSYM CMSG_ENCODE_HASHED_SUBJECT_IDENTIFIER_FLAG}
  CMSG_ENCODE_HASHED_SUBJECT_IDENTIFIER_FLAG = $2;

//+-------------------------------------------------------------------------
//  Returns TRUE if the SubjectIdentifier exists in the CTL. Optionally
//  returns a pointer to and byte count of the Subject's encoded attributes.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertFindSubjectInSortedCTL}
function CertFindSubjectInSortedCTL(var pSubjectIdentifier: CRYPT_DATA_BLOB;
  pCtlContext: PCCTL_CONTEXT; dwFlags: DWORD; pvReserved: Pointer;
  out pEncodedAttributes: CRYPT_DER_BLOB): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Enumerates through the sequence of TrustedSubjects in a CTL context
//  created with CERT_CREATE_CONTEXT_SORTED_FLAG set.
//
//  To start the enumeration, *ppvNextSubject must be NULL. Upon return,
//  *ppvNextSubject is updated to point to the next TrustedSubject in
//  the encoded sequence.
//
//  Returns FALSE for no more subjects or invalid arguments.
//
//  Note, the returned DER_BLOBs point directly into the encoded
//  bytes (not allocated, and must not be freed).
//--------------------------------------------------------------------------
{$EXTERNALSYM CertEnumSubjectInSortedCTL}
function CertEnumSubjectInSortedCTL(pCtlContext: PCCTL_CONTEXT;
  var ppvNextSubject: Pointer;
  out pSubjectIdentifier, pEncodedAttributes: CRYPT_DER_BLOB): BOOL; stdcall;

//+=========================================================================
//  Certificate Verify CTL Usage Data Structures and APIs
//==========================================================================
type
  PCtlVerifyUsagePara = ^TCtlVerifyUsagePara;
  {$EXTERNALSYM _CTL_VERIFY_USAGE_PARA}
  _CTL_VERIFY_USAGE_PARA = record
    cbSize: DWORD;
    ListIdentifier: CRYPT_DATA_BLOB;      // OPTIONAL
    cCtlStore: DWORD;
    rghCtlStore: ^HCERTSTORE;             // OPTIONAL
    cSignerStore: DWORD;
    rghSignerStore: ^HCERTSTORE;          // OPTIONAL
  end;
  {$EXTERNALSYM CTL_VERIFY_USAGE_PARA}
  CTL_VERIFY_USAGE_PARA = _CTL_VERIFY_USAGE_PARA;
  {$EXTERNALSYM PCTL_VERIFY_USAGE_PARA}
  PCTL_VERIFY_USAGE_PARA = ^_CTL_VERIFY_USAGE_PARA;
  TCtlVerifyUsagePara = _CTL_VERIFY_USAGE_PARA;

  PCtlVerifyUsageStatus = ^TCtlVerifyUsageStatus;
  {$EXTERNALSYM _CTL_VERIFY_USAGE_STATUS}
  _CTL_VERIFY_USAGE_STATUS = record
    cbSize: DWORD;
    dwError: DWORD;
    dwFlags: DWORD;
    ppCtl: ^PCCTL_CONTEXT;                // IN OUT OPTIONAL
    dwCtlEntryIndex: DWORD;
    ppSigner: ^PCCERT_CONTEXT;            // IN OUT OPTIONAL
    dwSignerIndex: DWORD;
  end;
  {$EXTERNALSYM CTL_VERIFY_USAGE_STATUS}
  CTL_VERIFY_USAGE_STATUS = _CTL_VERIFY_USAGE_STATUS;
  {$EXTERNALSYM PCTL_VERIFY_USAGE_STATUS}
  PCTL_VERIFY_USAGE_STATUS = ^_CTL_VERIFY_USAGE_STATUS;
  TCtlVerifyUsageStatus = _CTL_VERIFY_USAGE_STATUS;

const
  {$EXTERNALSYM CERT_VERIFY_INHIBIT_CTL_UPDATE_FLAG}
  CERT_VERIFY_INHIBIT_CTL_UPDATE_FLAG   = $1;
  {$EXTERNALSYM CERT_VERIFY_TRUSTED_SIGNERS_FLAG}
  CERT_VERIFY_TRUSTED_SIGNERS_FLAG      = $2;
  {$EXTERNALSYM CERT_VERIFY_NO_TIME_CHECK_FLAG}
  CERT_VERIFY_NO_TIME_CHECK_FLAG        = $4;
  {$EXTERNALSYM CERT_VERIFY_ALLOW_MORE_USAGE_FLAG}
  CERT_VERIFY_ALLOW_MORE_USAGE_FLAG     = $8;

  {$EXTERNALSYM CERT_VERIFY_UPDATED_CTL_FLAG}
  CERT_VERIFY_UPDATED_CTL_FLAG          = $1;

//+-------------------------------------------------------------------------
//  Verify that a subject is trusted for the specified usage by finding a
//  signed and time valid CTL with the usage identifiers and containing the
//  the subject. A subject can be identified by either its certificate context
//  or any identifier such as its SHA1 hash.
//
//  See CertFindSubjectInCTL for definition of dwSubjectType and pvSubject
//  parameters.
//
//  Via pVerifyUsagePara, the caller can specify the stores to be searched
//  to find the CTL. The caller can also specify the stores containing
//  acceptable CTL signers. By setting the ListIdentifier, the caller
//  can also restrict to a particular signer CTL list.
//
//  Via pVerifyUsageStatus, the CTL containing the subject, the subject's
//  index into the CTL's array of entries, and the signer of the CTL
//  are returned. If the caller is not interested, ppCtl and ppSigner can be set
//  to NULL. Returned contexts must be freed via the store's free context APIs.
//
//  If the CERT_VERIFY_INHIBIT_CTL_UPDATE_FLAG isn't set, then, a time
//  invalid CTL in one of the CtlStores may be replaced. When replaced, the
//  CERT_VERIFY_UPDATED_CTL_FLAG is set in pVerifyUsageStatus->dwFlags.
//
//  If the CERT_VERIFY_TRUSTED_SIGNERS_FLAG is set, then, only the
//  SignerStores specified in pVerifyUsageStatus are searched to find
//  the signer. Otherwise, the SignerStores provide additional sources
//  to find the signer's certificate.
//
//  If CERT_VERIFY_NO_TIME_CHECK_FLAG is set, then, the CTLs aren't checked
//  for time validity.
//
//  If CERT_VERIFY_ALLOW_MORE_USAGE_FLAG is set, then, the CTL may contain
//  additional usage identifiers than specified by pSubjectUsage. Otherwise,
//  the found CTL will contain the same usage identifers and no more.
//
//  CertVerifyCTLUsage will be implemented as a dispatcher to OID installable
//  functions. First, it will try to find an OID function matching the first
//  usage object identifier in the pUsage sequence. Next, it will dispatch
//  to the default CertDllVerifyCTLUsage functions.
//
//  If the subject is trusted for the specified usage, then, TRUE is
//  returned. Otherwise, FALSE is returned with dwError set to one of the
//  following:
//      CRYPT_E_NO_VERIFY_USAGE_DLL
//      CRYPT_E_NO_VERIFY_USAGE_CHECK
//      CRYPT_E_VERIFY_USAGE_OFFLINE
//      CRYPT_E_NOT_IN_CTL
//      CRYPT_E_NO_TRUSTED_SIGNER
//--------------------------------------------------------------------------
{$EXTERNALSYM CertVerifyCTLUsage}
function CertVerifyCTLUsage(dwEncodingType, dwSubjectType: DWORD;
  pvSubject: Pointer; var pSubjectUsage: CTL_USAGE; dwFlags: DWORD;
  var pVerifyUsagePara: CTL_VERIFY_USAGE_PARA;
  out pVerifyUsageStatus: CTL_VERIFY_USAGE_STATUS): BOOL; stdcall;

//+=========================================================================
//  Certificate Revocation Data Structures and APIs
//==========================================================================

//+-------------------------------------------------------------------------
//  This data structure is updated by a CRL revocation type handler
//  with the base and possibly the delta CRL used.
//--------------------------------------------------------------------------
type
  PCertRevocationCrlInfo = ^TCertRevocationCrlInfo;
  {$EXTERNALSYM _CERT_REVOCATION_CRL_INFO}
  _CERT_REVOCATION_CRL_INFO = record
    cbSize: DWORD;
    pBaseCrlContext: PCCRL_CONTEXT;
    pDeltaCrlContext: PCCRL_CONTEXT;
    
    // When revoked, points to entry in either of the above CRL contexts.
    // Don't free.
    pCrlEntry: PCRL_ENTRY;
    fDeltaCrlEntry: BOOL;                 // TRUE if in pDeltaCrlContext
  end;
  {$EXTERNALSYM CERT_REVOCATION_CRL_INFO}
  CERT_REVOCATION_CRL_INFO = _CERT_REVOCATION_CRL_INFO;
  {$EXTERNALSYM PCERT_REVOCATION_CRL_INFO}
  PCERT_REVOCATION_CRL_INFO = ^_CERT_REVOCATION_CRL_INFO;
  TCertRevocationCrlInfo = _CERT_REVOCATION_CRL_INFO;

//+-------------------------------------------------------------------------
//  The following data structure may be passed to CertVerifyRevocation to
//  assist in finding the issuer of the context to be verified.
//
//  When pIssuerCert is specified, pIssuerCert is the issuer of
//  rgpvContext[cContext - 1].
//
//  When cCertStore and rgCertStore are specified, these stores may contain
//  an issuer certificate.
//
//  When hCrlStore is specified then a handler which uses CRLs can search this
//  store for them
//
//  When pftTimeToUse is specified then the handler (if possible) must determine
//  revocation status relative to the time given otherwise the answer may be
//  independent of time or relative to current time
//--------------------------------------------------------------------------
  PCertRevocationPara = ^TCertRevocationPara;
  {$EXTERNALSYM _CERT_REVOCATION_PARA} 
  _CERT_REVOCATION_PARA = record 
    cbSize: DWORD; 
    pIssuerCert: PCCERT_CONTEXT; 
    cCertStore: DWORD; 
    rgCertStore: ^HCERTSTORE; 
    hCrlStore: HCERTSTORE; 
    pftTimeToUse: PFileTime;

    // Note, if you #define CERT_REVOCATION_PARA_HAS_EXTRA_FIELDS, then, you
    // must zero all unused fields in this data structure.
    // More fields could be added in a future release.
    // 0 uses revocation handler's default timeout.
    dwUrlRetrievalTimeout: DWORD;         // milliseconds

    // When set, checks and attempts to retrieve a CRL where
    // ThisUpdate >= (CurrentTime - dwFreshnessTime). Otherwise, defaults
    // to using the CRL's NextUpdate.
    fCheckFreshnessTime: BOOL;
    dwFreshnessTime: DWORD;               // seconds

    // If NULL, revocation handler gets the current time
    pftCurrentTime: PFileTime; 
    // If nonNULL, a CRL revocation type handler updates with the base and
    // possibly the delta CRL used. Note, *pCrlInfo must be initialized
    // by the caller. Any nonNULL CRL contexts are freed. Any updated
    // CRL contexts must be freed by the caller.
    //
    // The CRL info is only applicable to the last context checked. If
    // interested in this information, then, CertVerifyRevocation should be
    // called with cContext = 1.
    pCrlInfo: PCERT_REVOCATION_CRL_INFO;

    // If nonNULL, any cached information before this time is considered
    // time invalid and forces a wire retrieval. 
    pftCacheResync: PFileTime; 
  end; 
  {$EXTERNALSYM CERT_REVOCATION_PARA} 
  CERT_REVOCATION_PARA = _CERT_REVOCATION_PARA;
  {$EXTERNALSYM PCERT_REVOCATION_PARA} 
  PCERT_REVOCATION_PARA = ^_CERT_REVOCATION_PARA; 
  TCertRevocationPara = _CERT_REVOCATION_PARA; 

//+-------------------------------------------------------------------------
//  The following data structure is returned by CertVerifyRevocation to
//  specify the status of the revoked or unchecked context. Review the
//  following CertVerifyRevocation comments for details.
//
//  Upon input to CertVerifyRevocation, cbSize must be set to a size
//  >= (offsetof(CERT_REVOCATION_STATUS, dwReason) + sizeof(DWORD) ).
//  Otherwise, CertVerifyRevocation returns FALSE and sets LastError to
//  E_INVALIDARG.
//
//  Upon input to the installed or registered CRYPT_OID_VERIFY_REVOCATION_FUNC
//  functions, the dwIndex, dwError and dwReason have been zero'ed.
//  If present, fHasFreshnessTime and dwFreshnessTime have been zero'ed.
//--------------------------------------------------------------------------
  PCertRevocationStatus = ^TCertRevocationStatus;
  {$EXTERNALSYM _CERT_REVOCATION_STATUS}
  _CERT_REVOCATION_STATUS = record
    cbSize: DWORD;
    dwIndex: DWORD;
    dwError: DWORD;
    dwReason: DWORD;

    // Depending on cbSize, the following fields may optionally be returned.

    // The Freshness time is only applicable to the last context checked. If
    // interested in this information, then, CertVerifyRevocation should be
    // called with cContext = 1.
    //
    // fHasFreshnessTime is only set if we are able to retrieve revocation
    // information. For a CRL its CurrentTime - ThisUpdate.
    fHasFreshnessTime: BOOL;
    dwFreshnessTime: DWORD;               // seconds
  end;
  {$EXTERNALSYM CERT_REVOCATION_STATUS} 
  CERT_REVOCATION_STATUS = _CERT_REVOCATION_STATUS; 
  {$EXTERNALSYM PCERT_REVOCATION_STATUS} 
  PCERT_REVOCATION_STATUS = ^_CERT_REVOCATION_STATUS;
  TCertRevocationStatus = _CERT_REVOCATION_STATUS; 

//+-------------------------------------------------------------------------
//  Verifies the array of contexts for revocation. The dwRevType parameter
//  indicates the type of the context data structure passed in rgpvContext.
//  Currently only the revocation of certificates is defined.
//
//  If the CERT_VERIFY_REV_CHAIN_FLAG flag is set, then, CertVerifyRevocation
//  is verifying a chain of certs where, rgpvContext[i + 1] is the issuer
//  of rgpvContext[i]. Otherwise, CertVerifyRevocation makes no assumptions
//  about the order of the contexts.
//
//  To assist in finding the issuer, the pRevPara may optionally be set. See
//  the CERT_REVOCATION_PARA data structure for details.
//
//  The contexts must contain enough information to allow the
//  installable or registered revocation DLLs to find the revocation server. For
//  certificates, this information would normally be conveyed in an
//  extension such as the IETF's AuthorityInfoAccess extension.
//
//  CertVerifyRevocation returns TRUE if all of the contexts were successfully
//  checked and none were revoked. Otherwise, returns FALSE and updates the
//  returned pRevStatus data structure as follows:
//    dwIndex
//      Index of the first context that was revoked or unable to
//      be checked for revocation
//    dwError
//      Error status. LastError is also set to this error status.
//      dwError can be set to one of the following error codes defined
//      in winerror.h:
//        ERROR_SUCCESS - good context
//        CRYPT_E_REVOKED - context was revoked. dwReason contains the
//           reason for revocation
//        CRYPT_E_REVOCATION_OFFLINE - unable to connect to the
//           revocation server
//        CRYPT_E_NOT_IN_REVOCATION_DATABASE - the context to be checked
//           was not found in the revocation server's database.
//        CRYPT_E_NO_REVOCATION_CHECK - the called revocation function
//           wasn't able to do a revocation check on the context
//        CRYPT_E_NO_REVOCATION_DLL - no installed or registered Dll was
//           found to verify revocation
//    dwReason
//      The dwReason is currently only set for CRYPT_E_REVOKED and contains
//      the reason why the context was revoked. May be one of the following
//      CRL reasons defined by the CRL Reason Code extension ('2.5.29.21')
//          CRL_REASON_UNSPECIFIED              0
//          CRL_REASON_KEY_COMPROMISE           1
//          CRL_REASON_CA_COMPROMISE            2
//          CRL_REASON_AFFILIATION_CHANGED      3
//          CRL_REASON_SUPERSEDED               4
//          CRL_REASON_CESSATION_OF_OPERATION   5
//          CRL_REASON_CERTIFICATE_HOLD         6
//
//  For each entry in rgpvContext, CertVerifyRevocation iterates
//  through the CRYPT_OID_VERIFY_REVOCATION_FUNC
//  function set's list of installed DEFAULT functions.
//  CryptGetDefaultOIDFunctionAddress is called with pwszDll = NULL. If no
//  installed functions are found capable of doing the revocation verification,
//  CryptVerifyRevocation iterates through CRYPT_OID_VERIFY_REVOCATION_FUNC's
//  list of registered DEFAULT Dlls. CryptGetDefaultOIDDllList is called to
//  get the list. CryptGetDefaultOIDFunctionAddress is called to load the Dll.
//
//  The called functions have the same signature as CertVerifyRevocation. A
//  called function returns TRUE if it was able to successfully check all of
//  the contexts and none were revoked. Otherwise, the called function returns
//  FALSE and updates pRevStatus. dwIndex is set to the index of
//  the first context that was found to be revoked or unable to be checked.
//  dwError and LastError are updated. For CRYPT_E_REVOKED, dwReason
//  is updated. Upon input to the called function, dwIndex, dwError and
//  dwReason have been zero'ed. cbSize has been checked to be >=
//  sizeof(CERT_REVOCATION_STATUS).
//
//  If the called function returns FALSE, and dwError isn't set to
//  CRYPT_E_REVOKED, then, CertVerifyRevocation either continues on to the
//  next DLL in the list for a returned dwIndex of 0 or for a returned
//  dwIndex > 0, restarts the process of finding a verify function by
//  advancing the start of the context array to the returned dwIndex and
//  decrementing the count of remaining contexts.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertVerifyRevocation}
function CertVerifyRevocation(dwEncodingType, dwRevType, cContext: DWORD;
  rgpvContext: PPointer; dwFlags: DWORD; var pRevPara: CERT_REVOCATION_PARA;
  out pRevStatus: CERT_REVOCATION_STATUS): BOOL; stdcall;

const
//+-------------------------------------------------------------------------
//  Revocation types
//--------------------------------------------------------------------------
  {$EXTERNALSYM CERT_CONTEXT_REVOCATION_TYPE}
  CERT_CONTEXT_REVOCATION_TYPE                  = 1;

//+-------------------------------------------------------------------------
//  When the following flag is set, rgpvContext[] consists of a chain
//  of certificates, where rgpvContext[i + 1] is the issuer of rgpvContext[i].
//--------------------------------------------------------------------------
  {$EXTERNALSYM CERT_VERIFY_REV_CHAIN_FLAG}
  CERT_VERIFY_REV_CHAIN_FLAG                    = $00000001;

//+-------------------------------------------------------------------------
// CERT_VERIFY_CACHE_ONLY_BASED_REVOCATION prevents the revocation handler from
// accessing any network based resources for revocation checking
//--------------------------------------------------------------------------
  {$EXTERNALSYM CERT_VERIFY_CACHE_ONLY_BASED_REVOCATION}
  CERT_VERIFY_CACHE_ONLY_BASED_REVOCATION       = $00000002;

//+-------------------------------------------------------------------------
//  By default, the dwUrlRetrievalTimeout in pRevPara is the timeout used
//  for each URL wire retrieval. When the following flag is set,
//  dwUrlRetrievalTimeout is the accumulative timeout across all URL wire
//  retrievals.
//--------------------------------------------------------------------------
  {$EXTERNALSYM CERT_VERIFY_REV_ACCUMULATIVE_TIMEOUT_FLAG}
  CERT_VERIFY_REV_ACCUMULATIVE_TIMEOUT_FLAG     = $00000004;

//+-------------------------------------------------------------------------
//  When the following flag is set, only OCSP responses are used for
//  doing revocation checking. If the certificate doesn't have any
//  OCSP AIA URLs, dwError is set to CRYPT_E_NOT_IN_REVOCATION_DATABASE.
//--------------------------------------------------------------------------
  {$EXTERNALSYM CERT_VERIFY_REV_SERVER_OCSP_FLAG}
  CERT_VERIFY_REV_SERVER_OCSP_FLAG              = $00000008;

//+-------------------------------------------------------------------------
//  CERT_CONTEXT_REVOCATION_TYPE
//
//  pvContext points to a const CERT_CONTEXT.
//--------------------------------------------------------------------------

//+=========================================================================
//  Certificate Helper APIs
//==========================================================================

//+-------------------------------------------------------------------------
//  Compare two multiple byte integer blobs to see if they are identical.
//
//  Before doing the comparison, leading zero bytes are removed from a
//  positive number and leading $FF bytes are removed from a negative
//  number.
//
//  The multiple byte integers are treated as Little Endian. pbData[0] is the
//  least significant byte and pbData[cbData - 1] is the most significant
//  byte.
//
//  Returns TRUE if the integer blobs are identical after removing leading
//  0 or $FF bytes.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertCompareIntegerBlob}
function CertCompareIntegerBlob(
  var pInt1, pInt2: CRYPT_INTEGER_BLOB): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Compare two certificates to see if they are identical.
//
//  Since a certificate is uniquely identified by its Issuer and SerialNumber,
//  these are the only fields needing to be compared.
//
//  Returns TRUE if the certificates are identical.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertCompareCertificate}
function CertCompareCertificate(dwCertEncodingType: DWORD;
  var pCertId1, pCertId2: CERT_INFO): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Compare two certificate names to see if they are identical.
//
//  Returns TRUE if the names are identical.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertCompareCertificateName}
function CertCompareCertificateName(dwCertEncodingType: DWORD;
  var pCertName1, pCertName2: CERT_NAME_BLOB): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Compare the attributes in the certificate name with the specified
//  Relative Distinguished Name's (CERT_RDN) array of attributes.
//  The comparison iterates through the CERT_RDN attributes and looks for an
//  attribute match in any of the certificate name's RDNs.
//  Returns TRUE if all the attributes are found and match.
//
//  The CERT_RDN_ATTR fields can have the following special values:
//    pszObjId == NULL              - ignore the attribute object identifier
//    dwValueType == RDN_ANY_TYPE   - ignore the value type
//
//  CERT_CASE_INSENSITIVE_IS_RDN_ATTRS_FLAG should be set to do
//  a case insensitive match. Otherwise, defaults to an exact, case sensitive
//  match.
//
//  CERT_UNICODE_IS_RDN_ATTRS_FLAG should be set if the pRDN was initialized
//  with unicode strings as for CryptEncodeObject(X509_UNICODE_NAME).
//--------------------------------------------------------------------------
{$EXTERNALSYM CertIsRDNAttrsInCertificateName}
function CertIsRDNAttrsInCertificateName(dwCertEncodingType, dwFlags: DWORD;
  var pCertName: CERT_NAME_BLOB; var pRDN: CERT_RDN): BOOL; stdcall;

const
  {$EXTERNALSYM CERT_UNICODE_IS_RDN_ATTRS_FLAG}
  CERT_UNICODE_IS_RDN_ATTRS_FLAG                = $1;
  {$EXTERNALSYM CERT_CASE_INSENSITIVE_IS_RDN_ATTRS_FLAG}
  CERT_CASE_INSENSITIVE_IS_RDN_ATTRS_FLAG       = $2;

//+-------------------------------------------------------------------------
//  Compare two public keys to see if they are identical.
//
//  Returns TRUE if the keys are identical.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertComparePublicKeyInfo}
function CertComparePublicKeyInfo(dwCertEncodingType: DWORD;
  var pPublicKey1, pPublicKey2: CERT_PUBLIC_KEY_INFO): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Get the public/private key's bit length.
//
//  Returns 0 if unable to determine the key's length.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertGetPublicKeyLength}
function CertGetPublicKeyLength(dwCertEncodingType: DWORD;
  var pPublicKey: CERT_PUBLIC_KEY_INFO): DWORD; stdcall;

//+-------------------------------------------------------------------------
//  Verify the signature of a subject certificate or a CRL using the
//  public key info
//
//  Returns TRUE for a valid signature.
//
//  hCryptProv specifies the crypto provider to use to verify the signature.
//  It doesn't need to use a private key.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptVerifyCertificateSignature}
function CryptVerifyCertificateSignature(hCryptProv: HCRYPTPROV_LEGACY;
  dwCertEncodingType: DWORD; pbEncoded: PBYTE; cbEncoded: DWORD;
  var pPublicKey: CERT_PUBLIC_KEY_INFO): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Verify the signature of a subject certificate, CRL, certificate request
//  or keygen request using the issuer's public key.
//
//  Returns TRUE for a valid signature.
//
//  The subject can be an encoded blob or a context for a certificate or CRL.
//  For a subject certificate context, if the certificate is missing
//  inheritable PublicKey Algorithm Parameters, the context's
//  CERT_PUBKEY_ALG_PARA_PROP_ID is updated with the issuer's public key
//  algorithm parameters for a valid signature.
//
//  The issuer can be a pointer to a CERT_PUBLIC_KEY_INFO, certificate
//  context or a chain context.
//
//  hCryptProv specifies the crypto provider to use to verify the signature.
//  Its private key isn't used. If hCryptProv is NULL, a default
//  provider is picked according to the PublicKey Algorithm OID.
//
//  If the signature algorithm is a hashing algorithm, then, the
//  signature is expected to contain the hash octets. Only dwIssuerType
//  of CRYPT_VERIFY_CERT_SIGN_ISSUER_NULL may be specified
//  to verify this no signature case. If any other dwIssuerType is
//  specified, the verify will fail with LastError set to E_INVALIDARG.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptVerifyCertificateSignatureEx}
function CryptVerifyCertificateSignatureEx(hCryptProv: HCRYPTPROV_LEGACY;
  dwCertEncodingType, dwSubjectType: DWORD; pvSubject: Pointer;
  dwIssuerType: DWORD; pvIssuer: Pointer; dwFlags: DWORD;
  pvReserved: Pointer): BOOL; stdcall;

const
// Subject Types
  {$EXTERNALSYM CRYPT_VERIFY_CERT_SIGN_SUBJECT_BLOB}
  CRYPT_VERIFY_CERT_SIGN_SUBJECT_BLOB                       = 1;
    // pvSubject :: PCRYPT_DATA_BLOB
  {$EXTERNALSYM CRYPT_VERIFY_CERT_SIGN_SUBJECT_CERT}
  CRYPT_VERIFY_CERT_SIGN_SUBJECT_CERT                       = 2;
    // pvSubject :: PCCERT_CONTEXT
  {$EXTERNALSYM CRYPT_VERIFY_CERT_SIGN_SUBJECT_CRL}
  CRYPT_VERIFY_CERT_SIGN_SUBJECT_CRL                        = 3;
    // pvSubject :: PCCRL_CONTEXT
  {$EXTERNALSYM CRYPT_VERIFY_CERT_SIGN_SUBJECT_OCSP_BASIC_SIGNED_RESPONSE}
  CRYPT_VERIFY_CERT_SIGN_SUBJECT_OCSP_BASIC_SIGNED_RESPONSE = 4;
    // pvSubject :: POCSP_BASIC_SIGNED_RESPONSE_INFO

// Issuer Types
  {$EXTERNALSYM CRYPT_VERIFY_CERT_SIGN_ISSUER_PUBKEY}
  CRYPT_VERIFY_CERT_SIGN_ISSUER_PUBKEY                      = 1;
    // pvIssuer :: PCERT_PUBLIC_KEY_INFO
  {$EXTERNALSYM CRYPT_VERIFY_CERT_SIGN_ISSUER_CERT}
  CRYPT_VERIFY_CERT_SIGN_ISSUER_CERT                        = 2;
    // pvIssuer :: PCCERT_CONTEXT
  {$EXTERNALSYM CRYPT_VERIFY_CERT_SIGN_ISSUER_CHAIN}
  CRYPT_VERIFY_CERT_SIGN_ISSUER_CHAIN                       = 3;
    // pvIssuer :: PCCERT_CHAIN_CONTEXT
  {$EXTERNALSYM CRYPT_VERIFY_CERT_SIGN_ISSUER_NULL}
  CRYPT_VERIFY_CERT_SIGN_ISSUER_NULL                        = 4;
    // pvIssuer :: NULL

//+-------------------------------------------------------------------------
//  Compute the hash of the 'to be signed' information in the encoded
//  signed content (CERT_SIGNED_CONTENT_INFO).
//
//  hCryptProv specifies the crypto provider to use to compute the hash.
//  It doesn't need to use a private key.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptHashToBeSigned}
function CryptHashToBeSigned(hCryptProv: HCRYPTPROV_LEGACY;
  dwCertEncodingType: DWORD; pbEncoded: PBYTE; cbEncoded: DWORD;
  pbComputedHash: PBYTE; out pcbComputedHash: DWORD): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Hash the encoded content.
//
//  hCryptProv specifies the crypto provider to use to compute the hash.
//  It doesn't need to use a private key.
//
//  Algid specifies the CAPI hash algorithm to use. If Algid is 0, then, the
//  default hash algorithm (currently SHA1) is used.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptHashCertificate}
function CryptHashCertificate(hCryptProv: HCRYPTPROV_LEGACY; Algid: ALG_ID;
  dwFlags: DWORD; pbEncoded: PBYTE; cbEncoded: DWORD; pbComputedHash: PBYTE;
  out pcbComputedHash: DWORD): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Hash the encoded content using the CNG hash algorithm provider.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptHashCertificate2}
function CryptHashCertificate2(pwszCNGHashAlgid: LPCWSTR; dwFlags: DWORD;
  pvReserved: Pointer; pbEncoded: PBYTE; cbEncoded: DWORD;
  pbComputedHash: PBYTE; out pcbComputedHash: DWORD): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Sign the 'to be signed' information in the encoded signed content.
//
//  hCryptProvOrNCryptKey specifies the crypto provider to use to do the
//  signature.  It uses the specified private key.
//
//  If the SignatureAlgorithm is a hash algorithm, then, the signature
//  contains the hash octets. A private key isn't used to encrypt the hash.
//  dwKeySpec isn't used and hCryptProvOrNCryptKey can be NULL where an
//  appropriate default provider will be used for hashing.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptSignCertificate}
function CryptSignCertificate(
  hCryptProvOrNCryptKey: HCRYPTPROV_OR_NCRYPT_KEY_HANDLE;
  dwKeySpec: DWORD;       // not applicable for NCRYPT_KEY_HANDLE
  dwCertEncodingType: DWORD; pbEncodedToBeSigned: PBYTE;
  cbEncodedToBeSigned: DWORD; pSignatureAlgorithm: PCRYPT_ALGORITHM_IDENTIFIER;
  pvHashAuxInfo: Pointer; pbSignature: PBYTE;
  out pcbSignature: DWORD): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Encode the 'to be signed' information. Sign the encoded 'to be signed'.
//  Encode the 'to be signed' and the signature.
//
//  hCryptProv specifies the crypto provider to use to do the signature.
//  It uses the specified private key.
//
//  If the SignatureAlgorithm is a hash algorithm, then, the signature
//  contains the hash octets. A private key isn't used to encrypt the hash.
//  dwKeySpec isn't used and hCryptProv can be NULL where an appropriate
//  default provider will be used for hashing.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptSignAndEncodeCertificate}
function CryptSignAndEncodeCertificate(
  hCryptProvOrNCryptKey: HCRYPTPROV_OR_NCRYPT_KEY_HANDLE;
  dwKeySpec: DWORD;       // not applicable for NCRYPT_KEY_HANDLE
  dwCertEncodingType: DWORD;
  lpszStructType: LPCSTR;       // 'to be signed'
  pvStructInfo: Pointer;
  pSignatureAlgorithm: PCRYPT_ALGORITHM_IDENTIFIER;
  pvHashAuxInfo: Pointer;
  pbEncoded: PBYTE;
  out pcbEncoded: DWORD): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Certificate and CryptMsg encoded signature OID installable functions
//--------------------------------------------------------------------------

// The dwCertEncodingType and pSignatureAlgorithm->pszObjId are used
// to call the signature OID installable functions.
//
// If the OID installable function doesn't support the signature,
// it should return FALSE with LastError set to ERROR_NOT_SUPPORTED.

// Called if the signature has encoded parameters. Returns the CNG
// hash algorithm identifier string. Optionally returns the decoded
// signature parameters passed to either the SignAndEncodeHash or
// VerifyEncodedSignature OID installable function.
//
// Returned allocated parameters are freed via LocalFree().
const
  {$EXTERNALSYM CRYPT_OID_EXTRACT_ENCODED_SIGNATURE_PARAMETERS_FUNC}
  CRYPT_OID_EXTRACT_ENCODED_SIGNATURE_PARAMETERS_FUNC =
    'CryptDllExtractEncodedSignatureParameters';

type
  {$EXTERNALSYM PFN_CRYPT_EXTRACT_ENCODED_SIGNATURE_PARAMETERS_FUNC}
  PFN_CRYPT_EXTRACT_ENCODED_SIGNATURE_PARAMETERS_FUNC = function(
    dwCertEncodingType: DWORD;
    pSignatureAlgorithm: PCRYPT_ALGORITHM_IDENTIFIER;
    var ppvDecodedSignPara: Pointer;    // LocalFree()
    out ppwszCNGHashAlgid: LPWSTR       // LocalFree()
  ): BOOL stdcall;
  TFnCryptExtractEncodedSignatureParametersFunc =
    PFN_CRYPT_EXTRACT_ENCODED_SIGNATURE_PARAMETERS_FUNC;


// Called to sign the computed hash and encode it.
const
  {$EXTERNALSYM CRYPT_OID_SIGN_AND_ENCODE_HASH_FUNC}
  CRYPT_OID_SIGN_AND_ENCODE_HASH_FUNC =
    'CryptDllSignAndEncodeHash';

type
  {$EXTERNALSYM PFN_CRYPT_SIGN_AND_ENCODE_HASH_FUNC}
  PFN_CRYPT_SIGN_AND_ENCODE_HASH_FUNC = function(hKey: NCRYPT_KEY_HANDLE;
    dwCertEncodingType: DWORD; pSignatureAlgorithm: PCRYPT_ALGORITHM_IDENTIFIER;
    pvDecodedSignPara: Pointer;
    pwszCNGPubKeyAlgid: LPCWSTR;    // obtained from signature OID
    pwszCNGHashAlgid: LPCWSTR; pbComputedHash: PBYTE; cbComputedHash: DWORD;
    pbSignature: PBYTE; out pcbSignature: DWORD): BOOL stdcall;
  TFnCryptSignAndEncodeHashFunc = PFN_CRYPT_SIGN_AND_ENCODE_HASH_FUNC;

// Called to decode and decrypt the encoded signature and compare it with the
// computed hash.
const
  {$EXTERNALSYM CRYPT_OID_VERIFY_ENCODED_SIGNATURE_FUNC}
  CRYPT_OID_VERIFY_ENCODED_SIGNATURE_FUNC =
    'CryptDllVerifyEncodedSignature';

type
  {$EXTERNALSYM PFN_CRYPT_VERIFY_ENCODED_SIGNATURE_FUNC}
  PFN_CRYPT_VERIFY_ENCODED_SIGNATURE_FUNC = function(dwCertEncodingType: DWORD;
    pPubKeyInfo: PCERT_PUBLIC_KEY_INFO;
    pSignatureAlgorithm: PCRYPT_ALGORITHM_IDENTIFIER;
    pvDecodedSignPara: Pointer;
    pwszCNGPubKeyAlgid: LPCWSTR;    // obtained from signature OID
    pwszCNGHashAlgid: LPCWSTR; pbComputedHash: PBYTE; cbComputedHash: DWORD;
    pbSignature: PBYTE; cbSignature: DWORD): BOOL stdcall;
  TFnCryptVerifyEncodedSignatureFunc = PFN_CRYPT_VERIFY_ENCODED_SIGNATURE_FUNC;

//+-------------------------------------------------------------------------
//  Verify the time validity of a certificate.
//
//  Returns -1 if before NotBefore, +1 if after NotAfter and otherwise 0 for
//  a valid certificate
//
//  If pTimeToVerify is NULL, uses the current time.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertVerifyTimeValidity}
function CertVerifyTimeValidity(var pTimeToVerify: TFileTime;
  pCertInfo: PCERT_INFO): Longint; stdcall;

//+-------------------------------------------------------------------------
//  Verify the time validity of a CRL.
//
//  Returns -1 if before ThisUpdate, +1 if after NextUpdate and otherwise 0 for
//  a valid CRL
//
//  If pTimeToVerify is NULL, uses the current time.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertVerifyCRLTimeValidity}
function CertVerifyCRLTimeValidity(var pTimeToVerify: TFileTime;
  pCrlInfo: PCRL_INFO): Longint; stdcall;

//+-------------------------------------------------------------------------
//  Verify that the subject's time validity nests within the issuer's time
//  validity.
//
//  Returns TRUE if it nests. Otherwise, returns FALSE.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertVerifyValidityNesting}
function CertVerifyValidityNesting(
  pSubjectInfo, pIssuerInfo: PCERT_INFO): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Verify that the subject certificate isn't on its issuer CRL.
//
//  Returns true if the certificate isn't on the CRL.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertVerifyCRLRevocation}
function CertVerifyCRLRevocation(dwCertEncodingType: DWORD;
  pCertId: PCERT_INFO;          // Only the Issuer and SerialNumber
                                // fields are used
  cCrlInfo: DWORD; rgpCrlInfo: PPCRL_INFO): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Convert the CAPI AlgId to the ASN.1 Object Identifier string
//
//  Returns NULL if there isn't an ObjId corresponding to the AlgId.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertAlgIdToOID}
function CertAlgIdToOID(dwAlgId: DWORD): LPCSTR; stdcall;

//+-------------------------------------------------------------------------
//  Convert the ASN.1 Object Identifier string to the CAPI AlgId.
//
//  Returns 0 if there isn't an AlgId corresponding to the ObjId.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertOIDToAlgId}
function CertOIDToAlgId(pszObjId: LPCSTR): DWORD; stdcall;

//+-------------------------------------------------------------------------
//  Find an extension identified by its Object Identifier.
//
//  If found, returns pointer to the extension. Otherwise, returns NULL.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertFindExtension}
function CertFindExtension(pszObjId: LPCSTR; cExtensions: DWORD;
  rgExtensions: PCERT_EXTENSION): PCERT_EXTENSION; stdcall;

//+-------------------------------------------------------------------------
//  Find the first attribute identified by its Object Identifier.
//
//  If found, returns pointer to the attribute. Otherwise, returns NULL.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertFindAttribute}
function CertFindAttribute(pszObjId: LPCSTR; cAttr: DWORD;
  rgAttr: PCRYPT_ATTRIBUTE): PCRYPT_ATTRIBUTE; stdcall;

//+-------------------------------------------------------------------------
//  Find the first CERT_RDN attribute identified by its Object Identifier in
//  the name's list of Relative Distinguished Names.
//
//  If found, returns pointer to the attribute. Otherwise, returns NULL.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertFindRDNAttr}
function CertFindRDNAttr(pszObjId: LPCSTR;
  pName: PCERT_NAME_INFO): PCERT_RDN_ATTR; stdcall;

//+-------------------------------------------------------------------------
//  Get the intended key usage bytes from the certificate.
//
//  If the certificate doesn't have any intended key usage bytes, returns FALSE
//  and *pbKeyUsage is zeroed. Otherwise, returns TRUE and up through
//  cbKeyUsage bytes are copied into *pbKeyUsage. Any remaining uncopied
//  bytes are zeroed.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertGetIntendedKeyUsage}
function CertGetIntendedKeyUsage(dwCertEncodingType: DWORD;
  pCertInfo: PCERT_INFO; pbKeyUsage: PBYTE; cbKeyUsage: DWORD): BOOL; stdcall;

type
  {$EXTERNALSYM HCRYPTDEFAULTCONTEXT}
  HCRYPTDEFAULTCONTEXT = Pointer; 

//+-------------------------------------------------------------------------
//  Install a previously CryptAcquiredContext'ed HCRYPTPROV to be used as
//  a default context.
//
//  dwDefaultType and pvDefaultPara specify where the default context is used.
//  For example, install the HCRYPTPROV to be used to verify certificate's
//  having szOID_OIWSEC_md5RSA signatures.
//
//  By default, the installed HCRYPTPROV is only applicable to the current
//  thread. Set CRYPT_DEFAULT_CONTEXT_PROCESS_FLAG to allow the HCRYPTPROV
//  to be used by all threads in the current process.
//
//  For a successful install, TRUE is returned and *phDefaultContext is
//  updated with the HANDLE to be passed to CryptUninstallDefaultContext.
//
//  The installed HCRYPTPROVs are stack ordered (the last installed
//  HCRYPTPROV is checked first). All thread installed HCRYPTPROVs are
//  checked before any process HCRYPTPROVs.
//
//  The installed HCRYPTPROV remains available for default usage until
//  CryptUninstallDefaultContext is called or the thread or process exits.
//
//  If CRYPT_DEFAULT_CONTEXT_AUTO_RELEASE_FLAG is set, then, the HCRYPTPROV
//  is CryptReleaseContext'ed at thread or process exit. However,
//  not CryptReleaseContext'ed if CryptUninstallDefaultContext is
//  called.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptInstallDefaultContext}
function CryptInstallDefaultContext(hCryptProv: HCRYPTPROV;
  dwDefaultType: DWORD; pvDefaultPara: Pointer; dwFlags: DWORD;
  pvReserved: Pointer;
  var phDefaultContext: HCRYPTDEFAULTCONTEXT): BOOL; stdcall;

const
// dwFlags
  {$EXTERNALSYM CRYPT_DEFAULT_CONTEXT_AUTO_RELEASE_FLAG}
  CRYPT_DEFAULT_CONTEXT_AUTO_RELEASE_FLAG       = $00000001;
  {$EXTERNALSYM CRYPT_DEFAULT_CONTEXT_PROCESS_FLAG}
  CRYPT_DEFAULT_CONTEXT_PROCESS_FLAG            = $00000002;

// List of dwDefaultType's
  {$EXTERNALSYM CRYPT_DEFAULT_CONTEXT_CERT_SIGN_OID}
  CRYPT_DEFAULT_CONTEXT_CERT_SIGN_OID           = 1;
  {$EXTERNALSYM CRYPT_DEFAULT_CONTEXT_MULTI_CERT_SIGN_OID}
  CRYPT_DEFAULT_CONTEXT_MULTI_CERT_SIGN_OID     = 2;

//+-------------------------------------------------------------------------
//  CRYPT_DEFAULT_CONTEXT_CERT_SIGN_OID
//
//  Install a default HCRYPTPROV used to verify a certificate
//  signature. pvDefaultPara points to the szOID of the certificate
//  signature algorithm, for example, szOID_OIWSEC_md5RSA. If
//  pvDefaultPara is NULL, then, the HCRYPTPROV is used to verify all
//  certificate signatures. Note, pvDefaultPara can't be NULL when
//  CRYPT_DEFAULT_CONTEXT_PROCESS_FLAG is set.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CRYPT_DEFAULT_CONTEXT_MULTI_CERT_SIGN_OID
//
//  Same as CRYPT_DEFAULT_CONTEXT_CERT_SIGN_OID. However, the default
//  HCRYPTPROV is to be used for multiple signature szOIDs. pvDefaultPara
//  points to a CRYPT_DEFAULT_CONTEXT_MULTI_OID_PARA structure containing
//  an array of szOID pointers.
//--------------------------------------------------------------------------
type
  PCryptDefaultContextMultiOidPara = ^TCryptDefaultContextMultiOidPara;
  {$EXTERNALSYM _CRYPT_DEFAULT_CONTEXT_MULTI_OID_PARA} 
  _CRYPT_DEFAULT_CONTEXT_MULTI_OID_PARA = record 
    cOID: DWORD; 
    rgpszOID: ^LPSTR; 
  end; 
  {$EXTERNALSYM CRYPT_DEFAULT_CONTEXT_MULTI_OID_PARA} 
  CRYPT_DEFAULT_CONTEXT_MULTI_OID_PARA = _CRYPT_DEFAULT_CONTEXT_MULTI_OID_PARA; 
  {$EXTERNALSYM PCRYPT_DEFAULT_CONTEXT_MULTI_OID_PARA} 
  PCRYPT_DEFAULT_CONTEXT_MULTI_OID_PARA = ^_CRYPT_DEFAULT_CONTEXT_MULTI_OID_PARA; 
  TCryptDefaultContextMultiOidPara = _CRYPT_DEFAULT_CONTEXT_MULTI_OID_PARA; 

//+-------------------------------------------------------------------------
//  Uninstall a default context previously installed by
//  CryptInstallDefaultContext.
//
//  For a default context installed with CRYPT_DEFAULT_CONTEXT_PROCESS_FLAG
//  set, if any other threads are currently using this context,
//  this function will block until they finish.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptUninstallDefaultContext}
function CryptUninstallDefaultContext(hDefaultContext: HCRYPTDEFAULTCONTEXT;
  dwFlags: DWORD; pvReserved: Pointer): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Export the public key info associated with the provider's corresponding
//  private key.
//
//  Calls CryptExportPublicKeyInfoEx with pszPublicKeyObjId = NULL,
//  dwFlags = 0 and pvAuxInfo = NULL.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptExportPublicKeyInfo}
function CryptExportPublicKeyInfo(
  hCryptProvOrNCryptKey: HCRYPTPROV_OR_NCRYPT_KEY_HANDLE;
  dwKeySpec: DWORD;       // not applicable for NCRYPT_KEY_HANDLE
  dwCertEncodingType: DWORD; pInfo: PCERT_PUBLIC_KEY_INFO;
  out pcbInfo: DWORD): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Export the public key info associated with the provider's corresponding
//  private key.
//
//  Uses the dwCertEncodingType and pszPublicKeyObjId to call the
//  installable CRYPT_OID_EXPORT_PUBLIC_KEY_INFO_FUNC. The called function
//  has the same signature as CryptExportPublicKeyInfoEx.
//
//  If unable to find an installable OID function for the pszPublicKeyObjId,
//  attempts to export as a RSA Public Key (szOID_RSA_RSA).
//
//  The dwFlags and pvAuxInfo aren't used for szOID_RSA_RSA.
//
//  dwFlags can be set with the following 2 flags passed directly to
//  CryptFindOIDInfo:
//      CRYPT_OID_INFO_PUBKEY_SIGN_KEY_FLAG
//      CRYPT_OID_INFO_PUBKEY_ENCRYPT_KEY_FLAG
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptExportPublicKeyInfoEx}
function CryptExportPublicKeyInfoEx(
  hCryptProvOrNCryptKey: HCRYPTPROV_OR_NCRYPT_KEY_HANDLE;
  dwKeySpec: DWORD;       // not applicable for NCRYPT_KEY_HANDLE
  dwCertEncodingType: DWORD; pszPublicKeyObjId: LPSTR;
  dwFlags: DWORD; pvAuxInfo: Pointer; pInfo: PCERT_PUBLIC_KEY_INFO;
  out pcbInfo: DWORD): BOOL; stdcall;

// Legacy define used for exporting CAPI1 HCRYPTPROV public keys.
const
  {$EXTERNALSYM CRYPT_OID_EXPORT_PUBLIC_KEY_INFO_FUNC}
  CRYPT_OID_EXPORT_PUBLIC_KEY_INFO_FUNC     = 'CryptDllExportPublicKeyInfoEx';

//+-------------------------------------------------------------------------
//  Export CNG PublicKeyInfo OID installable function. Note, not called
//  for a HCRYPTPROV choice.
//--------------------------------------------------------------------------
  {$EXTERNALSYM CRYPT_OID_EXPORT_PUBLIC_KEY_INFO_EX2_FUNC}
  CRYPT_OID_EXPORT_PUBLIC_KEY_INFO_EX2_FUNC =
    'CryptDllExportPublicKeyInfoEx2';

type
  {$EXTERNALSYM PFN_CRYPT_EXPORT_PUBLIC_KEY_INFO_EX2_FUNC}
  PFN_CRYPT_EXPORT_PUBLIC_KEY_INFO_EX2_FUNC = function(
    hNCryptKey: NCRYPT_KEY_HANDLE; dwCertEncodingType: DWORD;
    pszPublicKeyObjId: LPSTR; dwFlags: DWORD; pvAuxInfo: Pointer;
    pInfo: PCERT_PUBLIC_KEY_INFO; out pcbInfo: DWORD): BOOL stdcall;
  TFnCryptExportPublicKeyInfoEx2Func =
    PFN_CRYPT_EXPORT_PUBLIC_KEY_INFO_EX2_FUNC;

//+-------------------------------------------------------------------------
//  Convert and import the public key info into the provider and return a
//  handle to the public key.
//
//  Calls CryptImportPublicKeyInfoEx with aiKeyAlg = 0, dwFlags = 0 and
//  pvAuxInfo = NULL.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptImportPublicKeyInfo}
function CryptImportPublicKeyInfo(hCryptProv: HCRYPTPROV;
  dwCertEncodingType: DWORD; pInfo: PCERT_PUBLIC_KEY_INFO;
  var phKey: HCRYPTKEY): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Convert and import the public key info into the provider and return a
//  handle to the public key.
//
//  Uses the dwCertEncodingType and pInfo->Algorithm.pszObjId to call the
//  installable CRYPT_OID_IMPORT_PUBLIC_KEY_INFO_FUNC. The called function
//  has the same signature as CryptImportPublicKeyInfoEx.
//
//  If unable to find an installable OID function for the pszObjId,
//  attempts to import as a RSA Public Key (szOID_RSA_RSA).
//
//  For szOID_RSA_RSA: aiKeyAlg may be set to CALG_RSA_SIGN or CALG_RSA_KEYX.
//  Defaults to CALG_RSA_KEYX. The dwFlags and pvAuxInfo aren't used.
//--------------------------------------------------------------------------
const
  {$EXTERNALSYM CRYPT_OID_IMPORT_PUBLIC_KEY_INFO_FUNC}
  CRYPT_OID_IMPORT_PUBLIC_KEY_INFO_FUNC   = 'CryptDllImportPublicKeyInfoEx';

{$EXTERNALSYM CryptImportPublicKeyInfoEx}
function CryptImportPublicKeyInfoEx(hCryptProv: HCRYPTPROV;
  dwCertEncodingType: DWORD; pInfo: PCERT_PUBLIC_KEY_INFO; aiKeyAlg: ALG_ID;
  dwFlags: DWORD; pvAuxInfo: Pointer; var phKey: HCRYPTKEY): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Convert and import the public key info into the CNG asymmetric or
//  signature algorithm provider and return a BCRYPT_KEY_HANDLE to it.
//
//  Uses the dwCertEncodingType and pInfo->Algorithm.pszObjId to call the
//  installable CRYPT_OID_IMPORT_PUBLIC_KEY_INFO_EX2_FUNC. The called function
//  has the same signature as CryptImportPublicKeyInfoEx2.
//
//  dwFlags can be set with the following 2 flags passed directly to
//  CryptFindOIDInfo:
//      CRYPT_OID_INFO_PUBKEY_SIGN_KEY_FLAG
//      CRYPT_OID_INFO_PUBKEY_ENCRYPT_KEY_FLAG
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptImportPublicKeyInfoEx2}
function CryptImportPublicKeyInfoEx2(dwCertEncodingType: DWORD;
  pInfo: PCERT_PUBLIC_KEY_INFO; dwFlags: DWORD; pvAuxInfo: Pointer;
  var phKey: BCRYPT_KEY_HANDLE): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Import CNG PublicKeyInfo OID installable function
//--------------------------------------------------------------------------
const
  {$EXTERNALSYM CRYPT_OID_IMPORT_PUBLIC_KEY_INFO_EX2_FUNC}
  CRYPT_OID_IMPORT_PUBLIC_KEY_INFO_EX2_FUNC =
    'CryptDllImportPublicKeyInfoEx2';

type
  {$EXTERNALSYM PFN_IMPORT_PUBLIC_KEY_INFO_EX2_FUNC}
  PFN_IMPORT_PUBLIC_KEY_INFO_EX2_FUNC = function(
    dwCertEncodingType: DWORD; pInfo: PCERT_PUBLIC_KEY_INFO; dwFlags: DWORD;
    pvAuxInfo: Pointer; var phKey: BCRYPT_KEY_HANDLE): BOOL stdcall;
  TFnImportPublicKeyInfoEx2Func = PFN_IMPORT_PUBLIC_KEY_INFO_EX2_FUNC;

//+-------------------------------------------------------------------------
//  Acquire a HCRYPTPROV and dwKeySpec or NCRYPT_KEY_HANDLE for the
//  specified certificate context. Uses the certificate's
//  CERT_KEY_PROV_INFO_PROP_ID property.
//  The returned HCRYPTPROV or NCRYPT_KEY_HANDLE handle may optionally be
//  cached using the certificate's CERT_KEY_CONTEXT_PROP_ID property.
//
//  If CRYPT_ACQUIRE_CACHE_FLAG is set, then, if an already acquired and
//  cached HCRYPTPROV or NCRYPT_KEY_HANDLE exists for the certificate, its
//  returned. Otherwise, a HCRYPTPROV or NCRYPT_KEY_HANDLE is acquired and
//  then cached via the certificate's CERT_KEY_CONTEXT_PROP_ID.
//
//  The CRYPT_ACQUIRE_USE_PROV_INFO_FLAG can be set to use the dwFlags field of
//  the certificate's CERT_KEY_PROV_INFO_PROP_ID property's CRYPT_KEY_PROV_INFO
//  data structure to determine if the returned HCRYPTPROV or
//  NCRYPT_KEY_HANDLE should be cached.
//  Caching is enabled if the CERT_SET_KEY_CONTEXT_PROP_ID flag was
//  set.
//
//  If CRYPT_ACQUIRE_COMPARE_KEY_FLAG is set, then,
//  the public key in the certificate is compared with the public
//  key returned by the cryptographic provider. If the keys don't match, the
//  acquire fails and LastError is set to NTE_BAD_PUBLIC_KEY. Note, if
//  a cached HCRYPTPROV or NCRYPT_KEY_HANDLE is returned, the comparison isn't
//  done. We assume the comparison was done on the initial acquire.
//
//  The CRYPT_ACQUIRE_NO_HEALING flags prohibits this function from
//  attempting to recreate the CERT_KEY_PROV_INFO_PROP_ID in the certificate
//  context if it fails to retrieve this property.
//
//  The CRYPT_ACQUIRE_SILENT_FLAG can be set to suppress any UI by the CSP.
//  See CryptAcquireContext's CRYPT_SILENT flag for more details.
//
//  The following flags can be set to optionally open and return a CNG
//  NCRYPT_KEY_HANDLE instead of a HCRYPTPROV. *pdwKeySpec is set to
//  CERT_NCRYPT_KEY_SPEC when a NCRYPT_KEY_HANDLE is returned.
//      CRYPT_ACQUIRE_ALLOW_NCRYPT_KEY_FLAG - if the CryptAcquireContext
//      fails, then, an NCryptOpenKey is attempted.
//
//      CRYPT_ACQUIRE_PREFER_NCRYPT_KEY_FLAG - the NCryptOpenKey is
//      first attempted and its handle returned for success.
//
//      CRYPT_ACQUIRE_ONLY_NCRYPT_KEY_FLAG - only the NCryptOpenKey is
//      attempted.
//
//  *pfCallerFreeProvOrNCryptKey is returned set to FALSE for:
//    - Acquire or public key comparison fails.
//    - CRYPT_ACQUIRE_CACHE_FLAG is set.
//    - CRYPT_ACQUIRE_USE_PROV_INFO_FLAG is set AND
//      CERT_SET_KEY_CONTEXT_PROP_ID flag is set in the dwFlags field of the
//      certificate's CERT_KEY_PROV_INFO_PROP_ID property's
//      CRYPT_KEY_PROV_INFO data structure.
//  When *pfCallerFreeProvOrNCryptKey is FALSE, the caller must not release. The
//  returned HCRYPTPROV or NCRYPT_KEY_HANDLE will be released on the last
//  free of the certificate context.
//
//  Otherwise, *pfCallerFreeProvOrNCryptKey is TRUE and a returned
//  HCRYPTPROV must be released by the caller by calling CryptReleaseContext.
//  A returned NCRYPT_KEY_HANDLE is freed by calling NCryptFreeObject.
//  *pdwKeySpec MUST be checked when CRYPT_ACQUIRE_ALLOW_NCRYPT_KEY_FLAG
//  or CRYPT_ACQUIRE_PREFER_NCRYPT_KEY_FLAG is set.
//
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptAcquireCertificatePrivateKey}
function CryptAcquireCertificatePrivateKey(pCert: PCCERT_CONTEXT;
  dwFlags: DWORD; pvReserved: Pointer;
  var phCryptProvOrNCryptKey: HCRYPTPROV_OR_NCRYPT_KEY_HANDLE;
  var pdwKeySpec: DWORD; var pfCallerFreeProvOrNCryptKey: BOOL): BOOL; stdcall;

const
  {$EXTERNALSYM CRYPT_ACQUIRE_CACHE_FLAG}
  CRYPT_ACQUIRE_CACHE_FLAG              = $00000001;
  {$EXTERNALSYM CRYPT_ACQUIRE_USE_PROV_INFO_FLAG}
  CRYPT_ACQUIRE_USE_PROV_INFO_FLAG      = $00000002;
  {$EXTERNALSYM CRYPT_ACQUIRE_COMPARE_KEY_FLAG}
  CRYPT_ACQUIRE_COMPARE_KEY_FLAG        = $00000004;
  {$EXTERNALSYM CRYPT_ACQUIRE_NO_HEALING}
  CRYPT_ACQUIRE_NO_HEALING              = $00000008;

  {$EXTERNALSYM CRYPT_ACQUIRE_SILENT_FLAG}
  CRYPT_ACQUIRE_SILENT_FLAG             = $00000040;

  {$EXTERNALSYM CRYPT_ACQUIRE_NCRYPT_KEY_FLAGS_MASK}
  CRYPT_ACQUIRE_NCRYPT_KEY_FLAGS_MASK   = $00070000;
  {$EXTERNALSYM CRYPT_ACQUIRE_ALLOW_NCRYPT_KEY_FLAG}
  CRYPT_ACQUIRE_ALLOW_NCRYPT_KEY_FLAG   = $00010000;
  {$EXTERNALSYM CRYPT_ACQUIRE_PREFER_NCRYPT_KEY_FLAG}
  CRYPT_ACQUIRE_PREFER_NCRYPT_KEY_FLAG  = $00020000;
  {$EXTERNALSYM CRYPT_ACQUIRE_ONLY_NCRYPT_KEY_FLAG}
  CRYPT_ACQUIRE_ONLY_NCRYPT_KEY_FLAG    = $00040000;

//+-------------------------------------------------------------------------
//  Enumerates the cryptographic providers and their containers to find the
//  private key corresponding to the certificate's public key. For a match,
//  the certificate's CERT_KEY_PROV_INFO_PROP_ID property is updated.
//
//  If the CERT_KEY_PROV_INFO_PROP_ID is already set, then, its checked to
//  see if it matches the provider's public key. For a match, the above
//  enumeration is skipped.
//
//  By default both the user and machine key containers are searched.
//  The CRYPT_FIND_USER_KEYSET_FLAG or CRYPT_FIND_MACHINE_KEYSET_FLAG
//  can be set in dwFlags to restrict the search to either of the containers.
//
//  The CRYPT_FIND_SILENT_KEYSET_FLAG can be set to suppress any UI by the CSP.
//  See CryptAcquireContext's CRYPT_SILENT flag for more details.
//
//  If a container isn't found, returns FALSE with LastError set to
//  NTE_NO_KEY.
//
//  The above CRYPT_ACQUIRE_NCRYPT_KEY_FLAGS can also be set. The default
//  is CRYPT_ACQUIRE_ALLOW_NCRYPT_KEY_FLAG.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptFindCertificateKeyProvInfo}
function CryptFindCertificateKeyProvInfo(pCert: PCCERT_CONTEXT; dwFlags: DWORD;
  pvReserved: Pointer): BOOL; stdcall;

const
  {$EXTERNALSYM CRYPT_FIND_USER_KEYSET_FLAG}
  CRYPT_FIND_USER_KEYSET_FLAG           = $00000001;
  {$EXTERNALSYM CRYPT_FIND_MACHINE_KEYSET_FLAG}
  CRYPT_FIND_MACHINE_KEYSET_FLAG        = $00000002;
  {$EXTERNALSYM CRYPT_FIND_SILENT_KEYSET_FLAG}
  CRYPT_FIND_SILENT_KEYSET_FLAG         = $00000040;

//+-------------------------------------------------------------------------
//  This is the prototype for the installable function which is called to
//  actually import a key into a CSP.  an installable of this type is called
//  from CryptImportPKCS8.  the algorithm OID of the private key is used
//  to look up the proper installable function to call.
//
//  hCryptProv - the provider to import the key to
//  pPrivateKeyInfo - describes the key to be imported
//  dwFlags - The available flags are:
//              CRYPT_EXPORTABLE
//              this flag is used when importing private keys, for a full
//              explanation please see the documentation for CryptImportKey.
//  pvAuxInfo - reserved for future, must be NULL
//--------------------------------------------------------------------------
type
  {$EXTERNALSYM PFN_IMPORT_PRIV_KEY_FUNC}
  PFN_IMPORT_PRIV_KEY_FUNC = function(
    hCryptProv: HCRYPTPROV; var pPrivateKeyInfo: CRYPT_PRIVATE_KEY_INFO;
    dwFlags: DWORD; pvAuxInfo: Pointer): BOOL stdcall;
  TFnImportPrivKeyFunc = PFN_IMPORT_PRIV_KEY_FUNC;

const
  {$EXTERNALSYM CRYPT_OID_IMPORT_PRIVATE_KEY_INFO_FUNC}
  CRYPT_OID_IMPORT_PRIVATE_KEY_INFO_FUNC   = 'CryptDllImportPrivateKeyInfoEx';

//+-------------------------------------------------------------------------
// Convert (from PKCS8 format) and import the private key into a provider
// and return a handle to the provider as well as the KeySpec used to import to.
//
// This function will call the PRESOLVE_HCRYPTPROV_FUNC in the
// privateKeyAndParams to obtain a handle of provider to import the key to.
// if the PRESOLVE_HCRYPTPROV_FUNC is NULL then the default provider will be used.
//
// privateKeyAndParams - private key blob and corresponding parameters
// dwFlags - The available flags are:
//              CRYPT_EXPORTABLE
//              this flag is used when importing private keys, for a full
//              explanation please see the documentation for CryptImportKey.
// phCryptProv - filled in with the handle of the provider the key was
//               imported to, the caller is responsible for freeing it
// pvAuxInfo - This parameter is reserved for future use and should be set
//             to NULL in the interim.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptImportPKCS8}
function CryptImportPKCS8(sPrivateKeyAndParams: CRYPT_PKCS8_IMPORT_PARAMS;
  dwFlags: DWORD; var phCryptProv: HCRYPTPROV;
  pvAuxInfo: Pointer): BOOL; stdcall;

//+-------------------------------------------------------------------------
// this is the prototype for installable functions for exporting the private key
//--------------------------------------------------------------------------
type
  PFN_EXPORT_PRIV_KEY_FUNC = function(hCryptProv: HCRYPTPROV; dwKeySpec: DWORD;
    pszPrivateKeyObjId: LPSTR; dwFlags: DWORD; pvAuxInfo: Pointer;
    out pPrivateKeyInfo: CRYPT_PRIVATE_KEY_INFO;
    out pcbPrivateKeyInfo: DWORD): BOOL stdcall;

const
  {$EXTERNALSYM CRYPT_OID_EXPORT_PRIVATE_KEY_INFO_FUNC}
  CRYPT_OID_EXPORT_PRIVATE_KEY_INFO_FUNC   = 'CryptDllExportPrivateKeyInfoEx';

  {$EXTERNALSYM CRYPT_DELETE_KEYSET}
  CRYPT_DELETE_KEYSET                      = $0001;
//+-------------------------------------------------------------------------
//  CryptExportPKCS8 -- superseded by CryptExportPKCS8Ex
//
//  Export the private key in PKCS8 format
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptExportPKCS8}
function CryptExportPKCS8(hCryptProv: HCRYPTPROV; dwKeySpec: DWORD;
  pszPrivateKeyObjId: LPSTR; dwFlags: DWORD; pvAuxInfo: Pointer;
  out pbPrivateKeyBlob: BYTE; out pcbPrivateKeyBlob: DWORD): BOOL; stdcall;

//+-------------------------------------------------------------------------
// CryptExportPKCS8Ex
//
//  Export the private key in PKCS8 format
//
//
//  Uses the pszPrivateKeyObjId to call the
//  installable CRYPT_OID_EXPORT_PRIVATE_KEY_INFO_FUNC. The called function
//  has the signature defined by PFN_EXPORT_PRIV_KEY_FUNC.
//
//  If unable to find an installable OID function for the pszPrivateKeyObjId,
//  attempts to export as a RSA Private Key (szOID_RSA_RSA).
//
// psExportParams - specifies information about the key to export
// dwFlags - The flag values. None currently supported
// pvAuxInfo - This parameter is reserved for future use and should be set to
//                         NULL in the interim.
// pbPrivateKeyBlob - A pointer to the private key blob.  It will be encoded
//                                        as a PKCS8 PrivateKeyInfo.
// pcbPrivateKeyBlob - A pointer to a DWORD that contains the size, in bytes,
//                                         of the private key blob being exported.
//+-------------------------------------------------------------------------
{$EXTERNALSYM CryptExportPKCS8Ex}
var
  CryptExportPKCS8Ex: function(psExportParams: PCRYPT_PKCS8_EXPORT_PARAMS;
    dwFlags: DWORD; pvAuxInfo: Pointer; out pbPrivateKeyBlob: BYTE;
    var pcbPrivateKeyBlob: PDWORD): BOOL stdcall;

//+-------------------------------------------------------------------------
//  Compute the hash of the encoded public key info.
//
//  The public key info is encoded and then hashed.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptHashPublicKeyInfo}
function CryptHashPublicKeyInfo(hCryptProv: HCRYPTPROV_LEGACY; Algid: ALG_ID;
  dwFlags, dwCertEncodingType: DWORD; var pInfo: CERT_PUBLIC_KEY_INFO;
   pbComputedHash: PBYTE; out pcbComputedHash: DWORD): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Convert a Name Value to a null terminated char string
//
//  Returns the number of characters converted including the terminating null
//  character. If psz is NULL or csz is 0, returns the required size of the
//  destination string (including the terminating null char).
//
//  If psz != NULL && csz != 0, returned psz is always NULL terminated.
//
//  Note: csz includes the NULL char.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertRDNValueToStrA}
function CertRDNValueToStrA(dwValueType: DWORD; var pValue: CERT_RDN_VALUE_BLOB;
  psz: LPSTR; csz: DWORD): DWORD; stdcall;

//+-------------------------------------------------------------------------
//  Convert a Name Value to a null terminated char string
//
//  Returns the number of characters converted including the terminating null
//  character. If psz is NULL or csz is 0, returns the required size of the
//  destination string (including the terminating null char).
//
//  If psz != NULL && csz != 0, returned psz is always NULL terminated.
//
//  Note: csz includes the NULL char.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertRDNValueToStrW}
function CertRDNValueToStrW(dwValueType: DWORD; var pValue: CERT_RDN_VALUE_BLOB;
  psz: LPWSTR; csz: DWORD): DWORD; stdcall;

{$EXTERNALSYM CertRDNValueToStr}
{$IFDEF UNICODE}
function CertRDNValueToStr(dwValueType: DWORD; var pValue: CERT_RDN_VALUE_BLOB;
  psz: LPWSTR; csz: DWORD): DWORD; stdcall;
{$ELSE}
function CertRDNValueToStr(dwValueType: DWORD; var pValue: CERT_RDN_VALUE_BLOB;
  psz: LPSTR; csz: DWORD): DWORD; stdcall;
{$ENDIF} // UNICODE

//+-------------------------------------------------------------------------
//  Convert the certificate name blob to a null terminated char string.
//
//  Follows the string representation of distinguished names specified in
//  RFC 1779. (Note, added double quoting '' for embedded quotes, quote
//  empty strings and don't quote strings containing consecutive spaces).
//  RDN values of type CERT_RDN_ENCODED_BLOB or CERT_RDN_OCTET_STRING are
//  formatted in hexadecimal (e.g. #0A56CF).
//
//  The name string is formatted according to the dwStrType:
//    CERT_SIMPLE_NAME_STR
//      The object identifiers are discarded. CERT_RDN entries are separated
//      by ', '. Multiple attributes per CERT_RDN are separated by ' + '.
//      For example:
//          Microsoft, Joe Cool + Programmer
//    CERT_OID_NAME_STR
//      The object identifiers are included with a '=' separator from their
//      attribute value. CERT_RDN entries are separated by ', '.
//      Multiple attributes per CERT_RDN are separated by ' + '. For example:
//          2.5.4.11=Microsoft, 2.5.4.3=Joe Cool + 2.5.4.12=Programmer
//    CERT_X500_NAME_STR
//      The object identifiers are converted to their X500 key name. Otherwise,
//      same as CERT_OID_NAME_STR. If the object identifier doesn't have
//      a corresponding X500 key name, then, the object identifier is used with
//      a 'OID.' prefix. For example:
//          OU=Microsoft, CN=Joe Cool + T=Programmer, OID.1.2.3.4.5.6=Unknown
//    CERT_XML_NAME_STR
//      The object identifiers are converted the same as the above
//      CERT_X500_NAME_STR. However, formatted as sequence of XML elements.
//      Here's an example:
//          <CN>cart.barnesandnoble.com</CN>
//          <OU>Terms of use at www.verisign.com/rpa (c)00</OU>
//          <OU rDNAttribute='true'>IT Operations</OU>
//          <O>Barnesandnoble.com</O>
//          <L>New York</L>
//          <S>New York</S>
//          <C>US</C>
//          <RDN oid='1.2.3.4' type='string'>name</RDN>
//          <RDN rDNAttribute='true' oid='1.2.1.3' type='encoded'>0500</RDN>
//          <RDN oid='1.2.1.4' type='encoded'>020135</RDN>
//          <RDN oid='1.2.2.5.3' type='octet'>01FF7F</RDN>
//      Where:
//          Any XML markup characters are escaped:
//             '&'   - '&amp;'
//             '<'   - '&lt;'
//             '>'   - '&gt;'
//             '\''  - '&apos;'
//             '\"'  - '&quot;'
//          Will escape characters > $7F via chararacter references,
//          '&#xXXXX;'
//
//          CERT_NAME_STR_REVERSE_FLAG and CERT_NAME_STR_CRLF_FLAG can be set.
//          The following quoting, semicolon and plus semantics aren't
//          applicable. The '+' is replaced with rDNAttribute='true'.
//
//
//  We quote the RDN value if it contains leading or trailing whitespace
//  or one of the following characters: ',', '+', '=', ''', '\n',  '<', '>',
//  '#' or ';'. The quoting character is '. If the the RDN Value contains
//  a ' it is double quoted (''). For example:
//      OU='  Microsoft', CN='Joe ''Cool''' + T='Programmer, Manager'
//
//  CERT_NAME_STR_SEMICOLON_FLAG can be or'ed into dwStrType to replace
//  the ', ' separator with a '; ' separator.
//
//  CERT_NAME_STR_CRLF_FLAG can be or'ed into dwStrType to replace
//  the ', ' separator with a '\r\n' separator.
//
//  CERT_NAME_STR_NO_PLUS_FLAG can be or'ed into dwStrType to replace the
//  ' + ' separator with a single space, ' '.
//
//  CERT_NAME_STR_NO_QUOTING_FLAG can be or'ed into dwStrType to inhibit
//  the above quoting.
//
//  CERT_NAME_STR_REVERSE_FLAG can be or'ed into dwStrType to reverse the
//  order of the RDNs before converting to the string.
//
//  By default, CERT_RDN_T61_STRING encoded values are initially decoded
//  as UTF8. If the UTF8 decoding fails, then, decoded as 8 bit characters.
//  CERT_NAME_STR_DISABLE_IE4_UTF8_FLAG can be or'ed into dwStrType to
//  skip the initial attempt to decode as UTF8.
//
//  Returns the number of characters converted including the terminating null
//  character. If psz is NULL or csz is 0, returns the required size of the
//  destination string (including the terminating null char).
//
//  If psz != NULL && csz != 0, returned psz is always NULL terminated.
//
//  Note: csz includes the NULL char.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//--------------------------------------------------------------------------
{$EXTERNALSYM CertNameToStrA}
function CertNameToStrA(dwCertEncodingType: DWORD; pName: PCERT_NAME_BLOB;
  dwStrType: DWORD; psz: LPSTR; csz: DWORD): DWORD; stdcall;
//+-------------------------------------------------------------------------
//--------------------------------------------------------------------------
{$EXTERNALSYM CertNameToStrW}
function CertNameToStrW(dwCertEncodingType: DWORD; pName: PCERT_NAME_BLOB;
  dwStrType: DWORD; psz: LPWSTR; csz: DWORD): DWORD; stdcall;
{$EXTERNALSYM CertNameToStr}
{$IFDEF UNICODE}
function CertNameToStr(dwCertEncodingType: DWORD; pName: PCERT_NAME_BLOB;
  dwStrType: DWORD; psz: LPWSTR; csz: DWORD): DWORD; stdcall;
{$ELSE}
function CertNameToStr(dwCertEncodingType: DWORD; pName: PCERT_NAME_BLOB;
  dwStrType: DWORD; psz: LPSTR; csz: DWORD): DWORD; stdcall;
{$ENDIF} // !UNICODE

// certenrolld_begin -- CERT_NAME_STR_*_FLAG
//+-------------------------------------------------------------------------
//  Certificate name string types
//--------------------------------------------------------------------------
const
  {$EXTERNALSYM CERT_SIMPLE_NAME_STR}
  CERT_SIMPLE_NAME_STR  = 1;
  {$EXTERNALSYM CERT_OID_NAME_STR}
  CERT_OID_NAME_STR     = 2;
  {$EXTERNALSYM CERT_X500_NAME_STR}
  CERT_X500_NAME_STR    = 3;
  {$EXTERNALSYM CERT_XML_NAME_STR}
  CERT_XML_NAME_STR     = 4;

//+-------------------------------------------------------------------------
//  Certificate name string type flags OR'ed with the above types
//--------------------------------------------------------------------------
  {$EXTERNALSYM CERT_NAME_STR_SEMICOLON_FLAG}
  CERT_NAME_STR_SEMICOLON_FLAG            = $40000000;
  {$EXTERNALSYM CERT_NAME_STR_NO_PLUS_FLAG}
  CERT_NAME_STR_NO_PLUS_FLAG              = $20000000;
  {$EXTERNALSYM CERT_NAME_STR_NO_QUOTING_FLAG}
  CERT_NAME_STR_NO_QUOTING_FLAG           = $10000000;
  {$EXTERNALSYM CERT_NAME_STR_CRLF_FLAG}
  CERT_NAME_STR_CRLF_FLAG                 = $08000000;
  {$EXTERNALSYM CERT_NAME_STR_COMMA_FLAG}
  CERT_NAME_STR_COMMA_FLAG                = $04000000;
  {$EXTERNALSYM CERT_NAME_STR_REVERSE_FLAG}
  CERT_NAME_STR_REVERSE_FLAG              = $02000000;
  {$EXTERNALSYM CERT_NAME_STR_FORWARD_FLAG}
  CERT_NAME_STR_FORWARD_FLAG              = $01000000;

  {$EXTERNALSYM CERT_NAME_STR_DISABLE_IE4_UTF8_FLAG}
  CERT_NAME_STR_DISABLE_IE4_UTF8_FLAG     = $00010000;
  {$EXTERNALSYM CERT_NAME_STR_ENABLE_T61_UNICODE_FLAG}
  CERT_NAME_STR_ENABLE_T61_UNICODE_FLAG   = $00020000;
  {$EXTERNALSYM CERT_NAME_STR_ENABLE_UTF8_UNICODE_FLAG}
  CERT_NAME_STR_ENABLE_UTF8_UNICODE_FLAG  = $00040000;
  {$EXTERNALSYM CERT_NAME_STR_FORCE_UTF8_DIR_STR_FLAG}
  CERT_NAME_STR_FORCE_UTF8_DIR_STR_FLAG   = $00080000;
  {$EXTERNALSYM CERT_NAME_STR_DISABLE_UTF8_DIR_STR_FLAG}
  CERT_NAME_STR_DISABLE_UTF8_DIR_STR_FLAG = $00100000;
// certenrolld_end


//+-------------------------------------------------------------------------
//  Convert the null terminated X500 string to an encoded certificate name.
//
//  The input string is expected to be formatted the same as the output
//  from the above CertNameToStr API.
//
//  The CERT_SIMPLE_NAME_STR type and CERT_XML_NAME_STR aren't supported.
//  Otherwise, when dwStrType
//  is set to 0, CERT_OID_NAME_STR or CERT_X500_NAME_STR, allow either a
//  case insensitive X500 key (CN=), case insensitive 'OID.' prefixed
//  object identifier (OID.1.2.3.4.5.6=) or an object identifier (1.2.3.4=).
//
//  If no flags are OR'ed into dwStrType, then, allow ',' or ';' as RDN
//  separators and '+' as the multiple RDN value separator. Quoting is
//  supported. A quote may be included in a quoted value by double quoting,
//  for example (CN='Joe ''Cool'''). A value starting with a '#' is treated
//  as ascii hex and converted to a CERT_RDN_OCTET_STRING. Embedded whitespace
//  is skipped (1.2.3 = # AB CD 01  is the same as 1.2.3=#ABCD01).
//
//  Whitespace surrounding the keys, object identifers and values is removed.
//
//  CERT_NAME_STR_COMMA_FLAG can be or'ed into dwStrType to only allow the
//  ',' as the RDN separator.
//
//  CERT_NAME_STR_SEMICOLON_FLAG can be or'ed into dwStrType to only allow the
//  ';' as the RDN separator.
//
//  CERT_NAME_STR_CRLF_FLAG can be or'ed into dwStrType to only allow
//  '\r' or '\n' as the RDN separator.
//
//  CERT_NAME_STR_NO_PLUS_FLAG can be or'ed into dwStrType to ignore '+'
//  as a separator and not allow multiple values per RDN.
//
//  CERT_NAME_STR_NO_QUOTING_FLAG can be or'ed into dwStrType to inhibit
//  quoting.
//
//  CERT_NAME_STR_REVERSE_FLAG can be or'ed into dwStrType to reverse the
//  order of the RDNs after converting from the string and before encoding.
//
//  CERT_NAME_STR_FORWARD_FLAG can be or'ed into dwStrType to defeat setting
//  CERT_NAME_STR_REVERSE_FLAG, if reverse order becomes the default.
//
//  CERT_NAME_STR_ENABLE_T61_UNICODE_FLAG can be or'ed into dwStrType to
//  to select the CERT_RDN_T61_STRING encoded value type instead of
//  CERT_RDN_UNICODE_STRING if all the UNICODE characters are <= $FF.
//
//  CERT_NAME_STR_ENABLE_UTF8_UNICODE_FLAG can be or'ed into dwStrType to
//  to select the CERT_RDN_UTF8_STRING encoded value type instead of
//  CERT_RDN_UNICODE_STRING.
//
//  CERT_NAME_STR_FORCE_UTF8_DIR_STR_FLAG can be or'ed into dwStrType
//  to force the CERT_RDN_UTF8_STRING encoded value type instead of
//  allowing CERT_RDN_PRINTABLE_STRING for DirectoryString types.
//  Applies to the X500 Keys below which allow 'Printable, Unicode'.
//  Also, enables CERT_NAME_STR_ENABLE_UTF8_UNICODE_FLAG.
//
//  CERT_NAME_STR_DISABLE_UTF8_DIR_STR_FLAG can be or'ed into dwStrType to
//  defeat setting CERT_NAME_STR_FORCE_UTF8_DIR_STR_FLAG, if forcing UTF-8
//  becomes the default.
//
//  Support the following X500 Keys:
//
//  Key         Object Identifier               RDN Value Type(s)
//  ---         -----------------               -----------------
//  CN          szOID_COMMON_NAME               Printable, Unicode
//  L           szOID_LOCALITY_NAME             Printable, Unicode
//  O           szOID_ORGANIZATION_NAME         Printable, Unicode
//  OU          szOID_ORGANIZATIONAL_UNIT_NAME  Printable, Unicode
//  E           szOID_RSA_emailAddr             Only IA5
//  Email       szOID_RSA_emailAddr             Only IA5
//  C           szOID_COUNTRY_NAME              Only Printable
//  S           szOID_STATE_OR_PROVINCE_NAME    Printable, Unicode
//  ST          szOID_STATE_OR_PROVINCE_NAME    Printable, Unicode
//  STREET      szOID_STREET_ADDRESS            Printable, Unicode
//  T           szOID_TITLE                     Printable, Unicode
//  Title       szOID_TITLE                     Printable, Unicode
//  G           szOID_GIVEN_NAME                Printable, Unicode
//  GN          szOID_GIVEN_NAME                Printable, Unicode
//  GivenName   szOID_GIVEN_NAME                Printable, Unicode
//  I           szOID_INITIALS                  Printable, Unicode
//  Initials    szOID_INITIALS                  Printable, Unicode
//  SN          szOID_SUR_NAME                  Printable, Unicode
//  DC          szOID_DOMAIN_COMPONENT          IA5, UTF8
//  SERIALNUMBER szOID_DEVICE_SERIAL_NUMBER     Only Printable
//
//  Note, T61 is selected instead of Unicode if
//  CERT_NAME_STR_ENABLE_T61_UNICODE_FLAG is set and all the unicode
//  characters are <= $FF.
//
//  Note, UTF8 is selected instead of Unicode if
//  CERT_NAME_STR_ENABLE_UTF8_UNICODE_FLAG is set.
//
//  Returns TRUE if successfully parsed the input string and encoded
//  the name.
//
//  If the input string is detected to be invalid, *ppszError is updated
//  to point to the beginning of the invalid character sequence. Otherwise,
//  *ppszError is set to NULL. *ppszError is updated with a non-NULL pointer
//  for the following errors:
//      CRYPT_E_INVALID_X500_STRING
//      CRYPT_E_INVALID_NUMERIC_STRING
//      CRYPT_E_INVALID_PRINTABLE_STRING
//      CRYPT_E_INVALID_IA5_STRING
//
//  ppszError can be set to NULL if not interested in getting a pointer
//  to the invalid character sequence.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//--------------------------------------------------------------------------
{$EXTERNALSYM CertStrToNameA}
function CertStrToNameA(dwCertEncodingType: DWORD; pszX500: LPCSTR;
  dwStrType: DWORD; pvReserved: Pointer; pbEncoded: PBYTE;
  out pcbEncoded: DWORD; out ppszError: LPCSTR): BOOL; stdcall;
//+-------------------------------------------------------------------------
//--------------------------------------------------------------------------
{$EXTERNALSYM CertStrToNameW}
function CertStrToNameW(dwCertEncodingType: DWORD; pszX500: LPCWSTR;
  dwStrType: DWORD; pvReserved: Pointer; pbEncoded: PBYTE;
  out pcbEncoded: DWORD; out ppszError: LPCWSTR): BOOL; stdcall;
{$EXTERNALSYM CertStrToName}
{$IFDEF UNICODE}
function CertStrToName(dwCertEncodingType: DWORD; pszX500: LPCWSTR;
  dwStrType: DWORD; pvReserved: Pointer; pbEncoded: PBYTE;
  out pcbEncoded: DWORD; out ppszError: LPCWSTR): BOOL; stdcall;
{$ELSE}
function CertStrToName(dwCertEncodingType: DWORD; pszX500: LPCSTR;
  dwStrType: DWORD; pvReserved: Pointer; pbEncoded: PBYTE;
  out pcbEncoded: DWORD; out ppszError: LPCSTR): BOOL; stdcall;
{$ENDIF} // !UNICODE

//+-------------------------------------------------------------------------
//  Get the subject or issuer name from the certificate and
//  according to the specified format type, convert to a null terminated
//  character string.
//
//  CERT_NAME_ISSUER_FLAG can be set to get the issuer's name. Otherwise,
//  gets the subject's name.
//
//  By default, CERT_RDN_T61_STRING encoded values are initially decoded
//  as UTF8. If the UTF8 decoding fails, then, decoded as 8 bit characters.
//  CERT_NAME_DISABLE_IE4_UTF8_FLAG can be set in dwFlags to
//  skip the initial attempt to decode as UTF8.
//
//  The name string is formatted according to the dwType:
//    CERT_NAME_EMAIL_TYPE
//      If the certificate has a Subject Alternative Name extension (for
//      issuer, Issuer Alternative Name), searches for first rfc822Name choice.
//      If the rfc822Name choice isn't found in the extension, searches the
//      Subject Name field for the Email OID, '1.2.840.113549.1.9.1'.
//      If the rfc822Name or Email OID is found, returns the string. Otherwise,
//      returns an empty string (returned character count is 1).
//    CERT_NAME_DNS_TYPE
//      If the certificate has a Subject Alternative Name extension (for
//      issuer, Issuer Alternative Name), searches for first DNSName choice.
//      If the DNSName choice isn't found in the extension, searches the
//      Subject Name field for the CN OID, '2.5.4.3'.
//      If the DNSName or CN OID is found, returns the string. Otherwise,
//      returns an empty string.
//    CERT_NAME_URL_TYPE
//      If the certificate has a Subject Alternative Name extension (for
//      issuer, Issuer Alternative Name), searches for first URL choice.
//      If the URL choice is found, returns the string. Otherwise,
//      returns an empty string.
//    CERT_NAME_UPN_TYPE
//      If the certificate has a Subject Alternative Name extension,
//      searches the OtherName choices looking for a
//      pszObjId == szOID_NT_PRINCIPAL_NAME, '1.3.6.1.4.1.311.20.2.3'.
//      If the UPN OID is found, the blob is decoded as a
//      X509_UNICODE_ANY_STRING and the decoded string is returned.
//      Otherwise, returns an empty string.
//    CERT_NAME_RDN_TYPE
//      Converts the Subject Name blob by calling CertNameToStr. pvTypePara
//      points to a DWORD containing the dwStrType passed to CertNameToStr.
//      If the Subject Name field is empty and the certificate has a
//      Subject Alternative Name extension, searches for and converts
//      the first directoryName choice.
//    CERT_NAME_ATTR_TYPE
//      pvTypePara points to the Object Identifier specifying the name attribute
//      to be returned. For example, to get the CN,
//      pvTypePara = szOID_COMMON_NAME ('2.5.4.3'). Searches, the Subject Name
//      field for the attribute.
//      If the Subject Name field is empty and the certificate has a
//      Subject Alternative Name extension, checks for
//      the first directoryName choice and searches it.
//
//      Note, searches the RDNs in reverse order.
//
//    CERT_NAME_SIMPLE_DISPLAY_TYPE
//      Iterates through the following list of name attributes and searches
//      the Subject Name and then the Subject Alternative Name extension
//      for the first occurrence of:
//          szOID_COMMON_NAME ('2.5.4.3')
//          szOID_ORGANIZATIONAL_UNIT_NAME ('2.5.4.11')
//          szOID_ORGANIZATION_NAME ('2.5.4.10')
//          szOID_RSA_emailAddr ('1.2.840.113549.1.9.1')
//
//      If none of the above attributes is found, then, searches the
//      Subject Alternative Name extension for a rfc822Name choice.
//
//      If still no match, then, returns the first attribute.
//
//      Note, like CERT_NAME_ATTR_TYPE, searches the RDNs in reverse order.
//
//    CERT_NAME_FRIENDLY_DISPLAY_TYPE
//      First checks if the certificate has a CERT_FRIENDLY_NAME_PROP_ID
//      property. If it does, then, this property is returned. Otherwise,
//      returns the above CERT_NAME_SIMPLE_DISPLAY_TYPE.
//
//  Returns the number of characters converted including the terminating null
//  character. If pwszNameString is NULL or cchNameString is 0, returns the
//  required size of the destination string (including the terminating null
//  char). If the specified name type isn't found. returns an empty string
//  with a returned character count of 1.
//
//  If pwszNameString != NULL && cwszNameString != 0, returned pwszNameString
//  is always NULL terminated.
//
//  Note: cchNameString includes the NULL char.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//--------------------------------------------------------------------------
{$EXTERNALSYM CertGetNameStringA}
function CertGetNameStringA(pCertContext: PCCERT_CONTEXT;
  dwType, dwFlags: DWORD; pvTypePara: Pointer; pszNameString: LPSTR;
  cchNameString: DWORD): DWORD; stdcall;
//+-------------------------------------------------------------------------
//--------------------------------------------------------------------------
{$EXTERNALSYM CertGetNameStringW}
function CertGetNameStringW(pCertContext: PCCERT_CONTEXT;
  dwType, dwFlags: DWORD; pvTypePara: Pointer; pszNameString: LPWSTR;
  cchNameString: DWORD): DWORD; stdcall;
{$EXTERNALSYM CertGetNameString}
{$IFDEF UNICODE}
function CertGetNameString(pCertContext: PCCERT_CONTEXT;
  dwType, dwFlags: DWORD; pvTypePara: Pointer; pszNameString: LPWSTR;
  cchNameString: DWORD): DWORD; stdcall;
{$ELSE}
function CertGetNameString(pCertContext: PCCERT_CONTEXT;
  dwType, dwFlags: DWORD; pvTypePara: Pointer; pszNameString: LPSTR;
  cchNameString: DWORD): DWORD; stdcall;
{$ENDIF} // !UNICODE

//+-------------------------------------------------------------------------
//  Certificate name types
//--------------------------------------------------------------------------
const
  {$EXTERNALSYM CERT_NAME_EMAIL_TYPE}
  CERT_NAME_EMAIL_TYPE            = 1;
  {$EXTERNALSYM CERT_NAME_RDN_TYPE}
  CERT_NAME_RDN_TYPE              = 2;
  {$EXTERNALSYM CERT_NAME_ATTR_TYPE}
  CERT_NAME_ATTR_TYPE             = 3;
  {$EXTERNALSYM CERT_NAME_SIMPLE_DISPLAY_TYPE}
  CERT_NAME_SIMPLE_DISPLAY_TYPE   = 4;
  {$EXTERNALSYM CERT_NAME_FRIENDLY_DISPLAY_TYPE}
  CERT_NAME_FRIENDLY_DISPLAY_TYPE = 5;
  {$EXTERNALSYM CERT_NAME_DNS_TYPE}
  CERT_NAME_DNS_TYPE              = 6;
  {$EXTERNALSYM CERT_NAME_URL_TYPE}
  CERT_NAME_URL_TYPE              = 7;
  {$EXTERNALSYM CERT_NAME_UPN_TYPE}
  CERT_NAME_UPN_TYPE              = 8;

//+-------------------------------------------------------------------------
//  Certificate name flags
//--------------------------------------------------------------------------
  {$EXTERNALSYM CERT_NAME_ISSUER_FLAG}
  CERT_NAME_ISSUER_FLAG           = $1;
  {$EXTERNALSYM CERT_NAME_DISABLE_IE4_UTF8_FLAG}
  CERT_NAME_DISABLE_IE4_UTF8_FLAG = $00010000;

//+=========================================================================
//  Simplified Cryptographic Message Data Structures and APIs
//==========================================================================

//+-------------------------------------------------------------------------
//              Conventions for the *pb and *pcb output parameters:
//
//              Upon entry to the function:
//                  if pcb is OPTIONAL && pcb == NULL, then,
//                      No output is returned
//                  else if pb == NULL && pcb != NULL, then,
//                      Length only determination. No length error is
//                      returned.
//                  otherwise where (pb != NULL && pcb != NULL && *pcb != 0)
//                      Output is returned. If *pcb isn't big enough a
//                      length error is returned. In all cases *pcb is updated
//                      with the actual length needed/returned.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  Type definitions of the parameters used for doing the cryptographic
//  operations.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  Callback to get and verify the signer's certificate.
//
//  Passed the CertId of the signer (its Issuer and SerialNumber) and a
//  handle to its cryptographic signed message's cert store.
//
//  For CRYPT_E_NO_SIGNER, called with pSignerId == NULL.
//
//  For a valid signer certificate, returns a pointer to a read only
//  CERT_CONTEXT. The returned CERT_CONTEXT is either obtained from a
//  cert store or was created via CertCreateCertificateContext. For either case,
//  its freed via CertFreeCertificateContext.
//
//  If a valid certificate isn't found, this callback returns NULL with
//  LastError set via SetLastError().
//
//  The NULL implementation tries to get the Signer certificate from the
//  message cert store. It doesn't verify the certificate.
//
//  Note, if the KEYID choice was selected for a CMS SignerId, then, the
//  SerialNumber is 0 and the Issuer is encoded containing a single RDN with a
//  single Attribute whose OID is szOID_KEYID_RDN, value type is
//  CERT_RDN_OCTET_STRING and value is the KEYID. When the
//  CertGetSubjectCertificateFromStore and
//  CertFindCertificateInStore(CERT_FIND_SUBJECT_CERT) APIs see this
//  special KEYID Issuer and SerialNumber, they do a KEYID match.
//--------------------------------------------------------------------------
type
  {$EXTERNALSYM PFN_CRYPT_GET_SIGNER_CERTIFICATE}
  PFN_CRYPT_GET_SIGNER_CERTIFICATE = function(
    pvGetArg: Pointer;
    dwCertEncodingType: DWORD;
    var pSignerId: CERT_INFO;   // Only the Issuer and SerialNumber
                                // fields have been updated
    hMsgCertStore: HCERTSTORE
  ): PCCERT_CONTEXT stdcall;
  TFnCryptGetSignerCertificate = PFN_CRYPT_GET_SIGNER_CERTIFICATE;

//+-------------------------------------------------------------------------
//  The CRYPT_SIGN_MESSAGE_PARA are used for signing messages using the
//  specified signing certificate context.
//
//  Either the CERT_KEY_PROV_HANDLE_PROP_ID or CERT_KEY_PROV_INFO_PROP_ID must
//  be set for each rgpSigningCert[]. Either one specifies the private
//  signature key to use.
//
//  If any certificates and/or CRLs are to be included in the signed message,
//  then, the MsgCert and MsgCrl parameters need to be updated. If the
//  rgpSigningCerts are to be included, then, they must also be in the
//  rgpMsgCert array.
//
//  cbSize must be set to the sizeof(CRYPT_SIGN_MESSAGE_PARA) or else
//  LastError will be updated with E_INVALIDARG.
//
//  pvHashAuxInfo currently isn't used and must be set to NULL.
//
//  dwFlags normally is set to 0. However, if the encoded output
//  is to be a CMSG_SIGNED inner content of an outer cryptographic message,
//  such as a CMSG_ENVELOPED, then, the CRYPT_MESSAGE_BARE_CONTENT_OUT_FLAG
//  should be set. If not set, then it would be encoded as an inner content
//  type of CMSG_DATA.
//
//  dwInnerContentType is normally set to 0. It needs to be set if the
//  ToBeSigned input is the encoded output of another cryptographic
//  message, such as, an CMSG_ENVELOPED. When set, it's one of the cryptographic
//  message types, for example, CMSG_ENVELOPED.
//
//  If the inner content of a nested cryptographic message is data (CMSG_DATA
//  the default), then, neither dwFlags or dwInnerContentType need to be set.
//
//  For CMS messages, CRYPT_MESSAGE_ENCAPSULATED_CONTENT_OUT_FLAG may be
//  set to encapsulate nonData inner content within an OCTET STRING.
//
//  For CMS messages, CRYPT_MESSAGE_KEYID_SIGNER_FLAG may be set to identify
//  signers by their Key Identifier and not their Issuer and Serial Number.
//
//  The CRYPT_MESSAGE_SILENT_KEYSET_FLAG can be set to suppress any UI by the
//  CSP. See CryptAcquireContext's CRYPT_SILENT flag for more details.
//
//  If HashEncryptionAlgorithm is present and not NULL its used instead of
//  the SigningCert's PublicKeyInfo.Algorithm.
//
//  Note, for RSA, the hash encryption algorithm is normally the same as
//  the public key algorithm. For DSA, the hash encryption algorithm is
//  normally a DSS signature algorithm.
//
//  pvHashEncryptionAuxInfo currently isn't used and must be set to NULL if
//  present in the data structure.
//--------------------------------------------------------------------------
type
  PCryptSignMessagePara = ^TCryptSignMessagePara;
  {$EXTERNALSYM _CRYPT_SIGN_MESSAGE_PARA} 
  _CRYPT_SIGN_MESSAGE_PARA = record 
    cbSize: DWORD; 
    dwMsgEncodingType: DWORD; 
    pSigningCert: PCCERT_CONTEXT; 
    HashAlgorithm: CRYPT_ALGORITHM_IDENTIFIER; 
    pvHashAuxInfo: Pointer; 
    cMsgCert: DWORD; 
    rgpMsgCert: ^PCCERT_CONTEXT; 
    cMsgCrl: DWORD; 
    rgpMsgCrl: ^PCCRL_CONTEXT; 
    cAuthAttr: DWORD; 
    rgAuthAttr: PCRYPT_ATTRIBUTE; 
    cUnauthAttr: DWORD; 
    rgUnauthAttr: PCRYPT_ATTRIBUTE; 
    dwFlags: DWORD; 
    dwInnerContentType: DWORD; 
    //#ifdef CRYPT_SIGN_MESSAGE_PARA_HAS_CMS_FIELDS 
    // This is also referred to as the SignatureAlgorithm 
    HashEncryptionAlgorithm: CRYPT_ALGORITHM_IDENTIFIER; 
    pvHashEncryptionAuxInfo: Pointer; 
    //#endif 
  end; 
  {$EXTERNALSYM CRYPT_SIGN_MESSAGE_PARA} 
  CRYPT_SIGN_MESSAGE_PARA = _CRYPT_SIGN_MESSAGE_PARA; 
  {$EXTERNALSYM PCRYPT_SIGN_MESSAGE_PARA} 
  PCRYPT_SIGN_MESSAGE_PARA = ^_CRYPT_SIGN_MESSAGE_PARA; 
  TCryptSignMessagePara = _CRYPT_SIGN_MESSAGE_PARA; 

const
  {$EXTERNALSYM CRYPT_MESSAGE_BARE_CONTENT_OUT_FLAG}
  CRYPT_MESSAGE_BARE_CONTENT_OUT_FLAG           = $00000001;

// When set, nonData type inner content is encapsulated within an
// OCTET STRING
  {$EXTERNALSYM CRYPT_MESSAGE_ENCAPSULATED_CONTENT_OUT_FLAG}
  CRYPT_MESSAGE_ENCAPSULATED_CONTENT_OUT_FLAG   = $00000002;

// When set, signers are identified by their Key Identifier and not
// their Issuer and Serial Number.
  {$EXTERNALSYM CRYPT_MESSAGE_KEYID_SIGNER_FLAG}
  CRYPT_MESSAGE_KEYID_SIGNER_FLAG               = $00000004;

// When set, suppresses any UI by the CSP.
// See CryptAcquireContext's CRYPT_SILENT flag for more details.
  {$EXTERNALSYM CRYPT_MESSAGE_SILENT_KEYSET_FLAG}
  CRYPT_MESSAGE_SILENT_KEYSET_FLAG              = $00000040;

//+-------------------------------------------------------------------------
//  The CRYPT_VERIFY_MESSAGE_PARA are used to verify signed messages.
//
//  hCryptProv is used to do hashing and signature verification.
//
//  The dwCertEncodingType specifies the encoding type of the certificates
//  and/or CRLs in the message.
//
//  pfnGetSignerCertificate is called to get and verify the message signer's
//  certificate.
//
//  cbSize must be set to the sizeof(CRYPT_VERIFY_MESSAGE_PARA) or else
//  LastError will be updated with E_INVALIDARG.
//--------------------------------------------------------------------------
type
  PCryptVerifyMessagePara = ^TCryptVerifyMessagePara;
  {$EXTERNALSYM _CRYPT_VERIFY_MESSAGE_PARA}
  _CRYPT_VERIFY_MESSAGE_PARA = record
    cbSize: DWORD;
    dwMsgAndCertEncodingType: DWORD;
    hCryptProv: HCRYPTPROV_LEGACY;
    pfnGetSignerCertificate: PFN_CRYPT_GET_SIGNER_CERTIFICATE;
    pvGetArg: Pointer;
  end;
  {$EXTERNALSYM CRYPT_VERIFY_MESSAGE_PARA}
  CRYPT_VERIFY_MESSAGE_PARA = _CRYPT_VERIFY_MESSAGE_PARA;
  {$EXTERNALSYM PCRYPT_VERIFY_MESSAGE_PARA}
  PCRYPT_VERIFY_MESSAGE_PARA = ^_CRYPT_VERIFY_MESSAGE_PARA;
  TCryptVerifyMessagePara = _CRYPT_VERIFY_MESSAGE_PARA;

//+-------------------------------------------------------------------------
//  The CRYPT_ENCRYPT_MESSAGE_PARA are used for encrypting messages.
//
//  hCryptProv is used to do content encryption, recipient key
//  encryption, and recipient key export. Its private key
//  isn't used.
//
//  Currently, pvEncryptionAuxInfo is only defined for RC2 or RC4 encryption
//  algorithms. Otherwise, its not used and must be set to NULL.
//  See CMSG_RC2_AUX_INFO for the RC2 encryption algorithms.
//  See CMSG_RC4_AUX_INFO for the RC4 encryption algorithms.
//
//  To enable SP3 compatible encryption, pvEncryptionAuxInfo should point to
//  a CMSG_SP3_COMPATIBLE_AUX_INFO data structure.
//
//  cbSize must be set to the sizeof(CRYPT_ENCRYPT_MESSAGE_PARA) or else
//  LastError will be updated with E_INVALIDARG.
//
//  dwFlags normally is set to 0. However, if the encoded output
//  is to be a CMSG_ENVELOPED inner content of an outer cryptographic message,
//  such as a CMSG_SIGNED, then, the CRYPT_MESSAGE_BARE_CONTENT_OUT_FLAG
//  should be set. If not set, then it would be encoded as an inner content
//  type of CMSG_DATA.
//
//  dwInnerContentType is normally set to 0. It needs to be set if the
//  ToBeEncrypted input is the encoded output of another cryptographic
//  message, such as, an CMSG_SIGNED. When set, it's one of the cryptographic
//  message types, for example, CMSG_SIGNED.
//
//  If the inner content of a nested cryptographic message is data (CMSG_DATA
//  the default), then, neither dwFlags or dwInnerContentType need to be set.
//
//  For CMS messages, CRYPT_MESSAGE_ENCAPSULATED_CONTENT_OUT_FLAG may be
//  set to encapsulate nonData inner content within an OCTET STRING before
//  encrypting.
//
//  For CMS messages, CRYPT_MESSAGE_KEYID_RECIPIENT_FLAG may be set to identify
//  recipients by their Key Identifier and not their Issuer and Serial Number.
//--------------------------------------------------------------------------
  PCryptEncryptMessagePara = ^TCryptEncryptMessagePara;
  {$EXTERNALSYM _CRYPT_ENCRYPT_MESSAGE_PARA}
  _CRYPT_ENCRYPT_MESSAGE_PARA = record
    cbSize: DWORD;
    dwMsgEncodingType: DWORD;
    hCryptProv: HCRYPTPROV_LEGACY;
    ContentEncryptionAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    pvEncryptionAuxInfo: Pointer;
    dwFlags: DWORD;
    dwInnerContentType: DWORD;
  end;
  {$EXTERNALSYM CRYPT_ENCRYPT_MESSAGE_PARA}
  CRYPT_ENCRYPT_MESSAGE_PARA = _CRYPT_ENCRYPT_MESSAGE_PARA;
  {$EXTERNALSYM PCRYPT_ENCRYPT_MESSAGE_PARA}
  PCRYPT_ENCRYPT_MESSAGE_PARA = ^_CRYPT_ENCRYPT_MESSAGE_PARA;
  TCryptEncryptMessagePara = _CRYPT_ENCRYPT_MESSAGE_PARA;

const
// When set, recipients are identified by their Key Identifier and not
// their Issuer and Serial Number.
  {$EXTERNALSYM CRYPT_MESSAGE_KEYID_RECIPIENT_FLAG}
  CRYPT_MESSAGE_KEYID_RECIPIENT_FLAG = $4;

//+-------------------------------------------------------------------------
//  The CRYPT_DECRYPT_MESSAGE_PARA are used for decrypting messages.
//
//  The CertContext to use for decrypting a message is obtained from one
//  of the specified cert stores. An encrypted message can have one or
//  more recipients. The recipients are identified by their CertId (Issuer
//  and SerialNumber). The cert stores are searched to find the CertContext
//  corresponding to the CertId.
//
//  For CMS, the recipients may also be identified by their KeyId.
//  CMS also allows Key Agreement (Diffie Hellman) in addition to
//  Key Transport (RSA) recipients.
//
//  Only CertContexts in the store with either
//  the CERT_KEY_PROV_HANDLE_PROP_ID or CERT_KEY_PROV_INFO_PROP_ID set
//  can be used. Either property specifies the private exchange key to use.
//
//  cbSize must be set to the sizeof(CRYPT_DECRYPT_MESSAGE_PARA) or else
//  LastError will be updated with E_INVALIDARG.
//--------------------------------------------------------------------------
type
  PCryptDecryptMessagePara = ^TCryptDecryptMessagePara;
  {$EXTERNALSYM _CRYPT_DECRYPT_MESSAGE_PARA} 
  _CRYPT_DECRYPT_MESSAGE_PARA = record 
    cbSize: DWORD; 
    dwMsgAndCertEncodingType: DWORD; 
    cCertStore: DWORD; 
    rghCertStore: ^HCERTSTORE; 
    //#ifdef CRYPT_DECRYPT_MESSAGE_PARA_HAS_EXTRA_FIELDS 
    // The above defined, CRYPT_MESSAGE_SILENT_KEYSET_FLAG, can be set to 
    // suppress UI by the CSP.  See CryptAcquireContext's CRYPT_SILENT 
    // flag for more details. 
    dwFlags: DWORD; 
    //#endif 
  end; 
  {$EXTERNALSYM CRYPT_DECRYPT_MESSAGE_PARA} 
  CRYPT_DECRYPT_MESSAGE_PARA = _CRYPT_DECRYPT_MESSAGE_PARA; 
  {$EXTERNALSYM PCRYPT_DECRYPT_MESSAGE_PARA} 
  PCRYPT_DECRYPT_MESSAGE_PARA = ^_CRYPT_DECRYPT_MESSAGE_PARA; 
  TCryptDecryptMessagePara = _CRYPT_DECRYPT_MESSAGE_PARA; 

//+-------------------------------------------------------------------------
//  The CRYPT_HASH_MESSAGE_PARA are used for hashing or unhashing
//  messages.
//
//  hCryptProv is used to compute the hash.
//
//  pvHashAuxInfo currently isn't used and must be set to NULL.
//
//  cbSize must be set to the sizeof(CRYPT_HASH_MESSAGE_PARA) or else
//  LastError will be updated with E_INVALIDARG.
//--------------------------------------------------------------------------
  PCryptHashMessagePara = ^TCryptHashMessagePara;
  {$EXTERNALSYM _CRYPT_HASH_MESSAGE_PARA} 
  _CRYPT_HASH_MESSAGE_PARA = record 
    cbSize: DWORD; 
    dwMsgEncodingType: DWORD; 
    hCryptProv: HCRYPTPROV_LEGACY; 
    HashAlgorithm: CRYPT_ALGORITHM_IDENTIFIER; 
    pvHashAuxInfo: Pointer; 
  end; 
  {$EXTERNALSYM CRYPT_HASH_MESSAGE_PARA} 
  CRYPT_HASH_MESSAGE_PARA = _CRYPT_HASH_MESSAGE_PARA; 
  {$EXTERNALSYM PCRYPT_HASH_MESSAGE_PARA} 
  PCRYPT_HASH_MESSAGE_PARA = ^_CRYPT_HASH_MESSAGE_PARA; 
  TCryptHashMessagePara = _CRYPT_HASH_MESSAGE_PARA; 

//+-------------------------------------------------------------------------
//  The CRYPT_KEY_SIGN_MESSAGE_PARA are used for signing messages until a
//  certificate has been created for the signature key.
//
//  pvHashAuxInfo currently isn't used and must be set to NULL.
//
//  If PubKeyAlgorithm isn't set, defaults to szOID_RSA_RSA.
//
//  cbSize must be set to the sizeof(CRYPT_KEY_SIGN_MESSAGE_PARA) or else
//  LastError will be updated with E_INVALIDARG.
//--------------------------------------------------------------------------
  PCryptKeySignMessagePara = ^TCryptKeySignMessagePara;
  {$EXTERNALSYM _CRYPT_KEY_SIGN_MESSAGE_PARA} 
  _CRYPT_KEY_SIGN_MESSAGE_PARA = record 
    cbSize: DWORD; 
    dwMsgAndCertEncodingType: DWORD;
    case Boolean of
      // NCryptIsKeyHandle() is called to determine the union choice.
      False: (hNCryptKey: NCRYPT_KEY_HANDLE);    // size: 4
      True:  (hCryptProv: HCRYPTPROV;           // size: 4

    // not applicable for hNCryptKey choice
    dwKeySpec: DWORD;

    HashAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
    pvHashAuxInfo: Pointer;
    // This is also referred to as the SignatureAlgorithm
    PubKeyAlgorithm: CRYPT_ALGORITHM_IDENTIFIER
    );
  end;
  {$EXTERNALSYM CRYPT_KEY_SIGN_MESSAGE_PARA}
  CRYPT_KEY_SIGN_MESSAGE_PARA = _CRYPT_KEY_SIGN_MESSAGE_PARA;
  {$EXTERNALSYM PCRYPT_KEY_SIGN_MESSAGE_PARA}
  PCRYPT_KEY_SIGN_MESSAGE_PARA = ^_CRYPT_KEY_SIGN_MESSAGE_PARA;
  TCryptKeySignMessagePara = _CRYPT_KEY_SIGN_MESSAGE_PARA;

//+-------------------------------------------------------------------------
//  The CRYPT_KEY_VERIFY_MESSAGE_PARA are used to verify signed messages without
//  a certificate for the signer.
//
//  Normally used until a certificate has been created for the key.
//
//  hCryptProv is used to do hashing and signature verification.
//
//  cbSize must be set to the sizeof(CRYPT_KEY_VERIFY_MESSAGE_PARA) or else
//  LastError will be updated with E_INVALIDARG.
//--------------------------------------------------------------------------
  PCryptKeyVerifyMessagePara = ^TCryptKeyVerifyMessagePara;
  {$EXTERNALSYM _CRYPT_KEY_VERIFY_MESSAGE_PARA} 
  _CRYPT_KEY_VERIFY_MESSAGE_PARA = record 
    cbSize: DWORD; 
    dwMsgEncodingType: DWORD; 
    hCryptProv: HCRYPTPROV_LEGACY; 
  end; 
  {$EXTERNALSYM CRYPT_KEY_VERIFY_MESSAGE_PARA} 
  CRYPT_KEY_VERIFY_MESSAGE_PARA = _CRYPT_KEY_VERIFY_MESSAGE_PARA; 
  {$EXTERNALSYM PCRYPT_KEY_VERIFY_MESSAGE_PARA} 
  PCRYPT_KEY_VERIFY_MESSAGE_PARA = ^_CRYPT_KEY_VERIFY_MESSAGE_PARA; 
  TCryptKeyVerifyMessagePara = _CRYPT_KEY_VERIFY_MESSAGE_PARA; 

//+-------------------------------------------------------------------------
//  Sign the message.
//
//  If fDetachedSignature is TRUE, the 'to be signed' content isn't included
//  in the encoded signed blob.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptSignMessage}
function CryptSignMessage(var pSignPara: CRYPT_SIGN_MESSAGE_PARA;
  fDetachedSignature: BOOL; cToBeSigned: DWORD; rgpbToBeSigned: PPBYTE;
  rgcbToBeSigned: PDWORD; pbSignedBlob: PBYTE;
  out pcbSignedBlob: DWORD): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Verify a signed message.
//
//  If pbDecoded == NULL, then, *pcbDecoded is implicitly set to 0 on input.
//  For *pcbDecoded == 0 && ppSignerCert == NULL on input, the signer isn't
//  verified.
//
//  A message might have more than one signer. Set dwSignerIndex to iterate
//  through all the signers. dwSignerIndex == 0 selects the first signer.
//
//  pVerifyPara's pfnGetSignerCertificate is called to get the signer's
//  certificate.
//
//  For a verified signer and message, *ppSignerCert is updated
//  with the CertContext of the signer. It must be freed by calling
//  CertFreeCertificateContext. Otherwise, *ppSignerCert is set to NULL.
//
//  ppSignerCert can be NULL, indicating the caller isn't interested
//  in getting the CertContext of the signer.
//
//  pcbDecoded can be NULL, indicating the caller isn't interested in getting
//  the decoded content. Furthermore, if the message doesn't contain any
//  content or signers, then, pcbDecoded must be set to NULL, to allow the
//  pVerifyPara->pfnGetCertificate to be called. Normally, this would be
//  the case when the signed message contains only certficates and CRLs.
//  If pcbDecoded is NULL and the message doesn't have the indicated signer,
//  pfnGetCertificate is called with pSignerId set to NULL.
//
//  If the message doesn't contain any signers || dwSignerIndex > message's
//  SignerCount, then, an error is returned with LastError set to
//  CRYPT_E_NO_SIGNER. Also, for CRYPT_E_NO_SIGNER, pfnGetSignerCertificate
//  is still called with pSignerId set to NULL.
//
//  Note, an alternative way to get the certificates and CRLs from a
//  signed message is to call CryptGetMessageCertificates.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptVerifyMessageSignature}
function CryptVerifyMessageSignature(var pVerifyPara: CRYPT_VERIFY_MESSAGE_PARA;
  dwSignerIndex: DWORD; pbSignedBlob: PBYTE; cbSignedBlob: DWORD;
  pbDecoded: PBYTE; out pcbDecoded: DWORD;
  ppSignerCert: PPCCERT_CONTEXT): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Returns the count of signers in the signed message. For no signers, returns
//  0. For an error returns -1 with LastError updated accordingly.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptGetMessageSignerCount}
function CryptGetMessageSignerCount(dwMsgEncodingType: DWORD;
  pbSignedBlob: PBYTE; cbSignedBlob: DWORD): Longint; stdcall;

//+-------------------------------------------------------------------------
//  Returns the cert store containing the message's certs and CRLs.
//  For an error, returns NULL with LastError updated.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptGetMessageCertificates}
function CryptGetMessageCertificates(
  dwMsgAndCertEncodingType: DWORD;
  hCryptProv: HCRYPTPROV_LEGACY;    // passed to CertOpenStore
  dwFlags: DWORD;                   // passed to CertOpenStore
  pbSignedBlob: PBYTE;
  cbSignedBlob: DWORD): HCERTSTORE; stdcall;

//+-------------------------------------------------------------------------
//  Verify a signed message containing detached signature(s).
//  The 'to be signed' content is passed in separately. No
//  decoded output. Otherwise, identical to CryptVerifyMessageSignature.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptVerifyDetachedMessageSignature}
function CryptVerifyDetachedMessageSignature(
  var pVerifyPara: CRYPT_VERIFY_MESSAGE_PARA; dwSignerIndex: DWORD;
  pbDetachedSignBlob: PBYTE; cbDetachedSignBlob, cToBeSigned: DWORD;
  rgpbToBeSigned: PPBYTE; rgcbToBeSigned: PDWORD;
  ppSignerCert: PPCCERT_CONTEXT): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Encrypts the message for the recipient(s).
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptEncryptMessage}
function CryptEncryptMessage(var pEncryptPara: CRYPT_ENCRYPT_MESSAGE_PARA;
  cRecipientCert: DWORD; rgpRecipientCert: PPCCERT_CONTEXT;
  pbToBeEncrypted: PBYTE; cbToBeEncrypted: DWORD; pbEncryptedBlob: PBYTE;
  out pcbEncryptedBlob: DWORD): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Decrypts the message.
//
//  If pbDecrypted == NULL, then, *pcbDecrypted is implicitly set to 0 on input.
//  For *pcbDecrypted == 0 && ppXchgCert == NULL on input, the message isn't
//  decrypted.
//
//  For a successfully decrypted message, *ppXchgCert is updated
//  with the CertContext used to decrypt. It must be freed by calling
//  CertStoreFreeCert. Otherwise, *ppXchgCert is set to NULL.
//
//  ppXchgCert can be NULL, indicating the caller isn't interested
//  in getting the CertContext used to decrypt.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptDecryptMessage}
function CryptDecryptMessage(var pDecryptPara: CRYPT_DECRYPT_MESSAGE_PARA;
  pbEncryptedBlob: PBYTE; cbEncryptedBlob: DWORD; pbDecrypted: PBYTE;
  var pcbDecrypted: DWORD; ppXchgCert: PPCCERT_CONTEXT): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Sign the message and encrypt for the recipient(s). Does a CryptSignMessage
//  followed with a CryptEncryptMessage.
//
//  Note: this isn't the CMSG_SIGNED_AND_ENVELOPED. Its a CMSG_SIGNED
//  inside of an CMSG_ENVELOPED.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptSignAndEncryptMessage}
function CryptSignAndEncryptMessage(var pSignPara: CRYPT_SIGN_MESSAGE_PARA;
  var pEncryptPara: CRYPT_ENCRYPT_MESSAGE_PARA; cRecipientCert: DWORD;
  rgpRecipientCert: PPCCERT_CONTEXT; pbToBeSignedAndEncrypted: PBYTE;
  cbToBeSignedAndEncrypted: DWORD; pbSignedAndEncryptedBlob: PBYTE;
  out pcbSignedAndEncryptedBlob: DWORD): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Decrypts the message and verifies the signer. Does a CryptDecryptMessage
//  followed with a CryptVerifyMessageSignature.
//
//  If pbDecrypted == NULL, then, *pcbDecrypted is implicitly set to 0 on input.
//  For *pcbDecrypted == 0 && ppSignerCert == NULL on input, the signer isn't
//  verified.
//
//  A message might have more than one signer. Set dwSignerIndex to iterate
//  through all the signers. dwSignerIndex == 0 selects the first signer.
//
//  The pVerifyPara's VerifySignerPolicy is called to verify the signer's
//  certificate.
//
//  For a successfully decrypted and verified message, *ppXchgCert and
//  *ppSignerCert are updated. They must be freed by calling
//  CertStoreFreeCert. Otherwise, they are set to NULL.
//
//  ppXchgCert and/or ppSignerCert can be NULL, indicating the
//  caller isn't interested in getting the CertContext.
//
//  Note: this isn't the CMSG_SIGNED_AND_ENVELOPED. Its a CMSG_SIGNED
//  inside of an CMSG_ENVELOPED.
//
//  The message always needs to be decrypted to allow access to the
//  signed message. Therefore, if ppXchgCert != NULL, its always updated.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptDecryptAndVerifyMessageSignature}
function CryptDecryptAndVerifyMessageSignature(
  var pDecryptPara: CRYPT_DECRYPT_MESSAGE_PARA;
  var pVerifyPara: CRYPT_VERIFY_MESSAGE_PARA;
  dwSignerIndex: DWORD; pbEncryptedBlob: PBYTE;
  cbEncryptedBlob: DWORD; pbDecrypted: PBYTE; var pcbDecrypted: DWORD;
  var ppXchgCert, ppSignerCert: PCCERT_CONTEXT): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Decodes a cryptographic message which may be one of the following types:
//    CMSG_DATA
//    CMSG_SIGNED
//    CMSG_ENVELOPED
//    CMSG_SIGNED_AND_ENVELOPED
//    CMSG_HASHED
//
//  dwMsgTypeFlags specifies the set of allowable messages. For example, to
//  decode either SIGNED or ENVELOPED messages, set dwMsgTypeFlags to:
//      CMSG_SIGNED_FLAG | CMSG_ENVELOPED_FLAG.
//
//  dwProvInnerContentType is only applicable when processing nested
//  crytographic messages. When processing an outer crytographic message
//  it must be set to 0. When decoding a nested cryptographic message
//  its the dwInnerContentType returned by a previous CryptDecodeMessage
//  of the outer message. The InnerContentType can be any of the CMSG types,
//  for example, CMSG_DATA, CMSG_SIGNED, ...
//
//  The optional *pdwMsgType is updated with the type of message.
//
//  The optional *pdwInnerContentType is updated with the type of the inner
//  message. Unless there is cryptographic message nesting, CMSG_DATA
//  is returned.
//
//  For CMSG_DATA: returns decoded content.
//  For CMSG_SIGNED: same as CryptVerifyMessageSignature.
//  For CMSG_ENVELOPED: same as CryptDecryptMessage.
//  For CMSG_SIGNED_AND_ENVELOPED: same as CryptDecryptMessage plus
//      CryptVerifyMessageSignature.
//  For CMSG_HASHED: verifies the hash and returns decoded content.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptDecodeMessage}
function CryptDecodeMessage(dwMsgTypeFlags: DWORD;
  var pDecryptPara: CRYPT_DECRYPT_MESSAGE_PARA;
  var pVerifyPara: CRYPT_VERIFY_MESSAGE_PARA; dwSignerIndex: DWORD;
  pbEncodedBlob: PBYTE; cbEncodedBlob, dwPrevInnerContentType: DWORD;
  var pdwMsgType, pdwInnerContentType: DWORD; pbDecoded: PBYTE;
  var pcbDecoded: DWORD;
  var ppXchgCert, ppSignerCert: PCCERT_CONTEXT): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Hash the message.
//
//  If fDetachedHash is TRUE, only the ComputedHash is encoded in the
//  pbHashedBlob. Otherwise, both the ToBeHashed and ComputedHash
//  are encoded.
//
//  pcbHashedBlob or pcbComputedHash can be NULL, indicating the caller
//  isn't interested in getting the output.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptHashMessage}
function CryptHashMessage(var pHashPara: CRYPT_HASH_MESSAGE_PARA;
  fDetachedHash: BOOL; cToBeHashed: DWORD; rgpbToBeHashed: PPBYTE;
  rgcbToBeHashed: PDWORD; pbHashedBlob: PBYTE; var pcbHashedBlob: DWORD;
  pbComputedHash: PBYTE; var pcbComputedHash: DWORD): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Verify a hashed message.
//
//  pcbToBeHashed or pcbComputedHash can be NULL,
//  indicating the caller isn't interested in getting the output.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptVerifyMessageHash}
function CryptVerifyMessageHash(var pHashPara: CRYPT_HASH_MESSAGE_PARA;
  pbHashedBlob: PBYTE; cbHashedBlob: DWORD; pbToBeHashed: PBYTE;
  var pcbToBeHashed: DWORD; pbComputedHash: PBYTE;
  var pcbComputedHash: DWORD): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Verify a hashed message containing a detached hash.
//  The 'to be hashed' content is passed in separately. No
//  decoded output. Otherwise, identical to CryptVerifyMessageHash.
//
//  pcbComputedHash can be NULL, indicating the caller isn't interested
//  in getting the output.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptVerifyDetachedMessageHash}
function CryptVerifyDetachedMessageHash(var pHashPara: CRYPT_HASH_MESSAGE_PARA;
  pbDetachedHashBlob: PBYTE; cbDetachedHashBlob, cToBeHashed: DWORD;
  rgpbToBeHashed: PPBYTE; rgcbToBeHashed: PDWORD; pbComputedHash: PBYTE;
  var pcbComputedHash: DWORD): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Sign the message using the provider's private key specified in the
//  parameters. A dummy SignerId is created and stored in the message.
//
//  Normally used until a certificate has been created for the key.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptSignMessageWithKey}
function CryptSignMessageWithKey(var pSignPara: CRYPT_KEY_SIGN_MESSAGE_PARA;
  pbToBeSigned: PBYTE; cbToBeSigned: DWORD; pbSignedBlob: PBYTE;
  out pcbSignedBlob: DWORD): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Verify a signed message using the specified public key info.
//
//  Normally called by a CA until it has created a certificate for the
//  key.
//
//  pPublicKeyInfo contains the public key to use to verify the signed
//  message. If NULL, the signature isn't verified (for instance, the decoded
//  content may contain the PublicKeyInfo).
//
//  pcbDecoded can be NULL, indicating the caller isn't interested
//  in getting the decoded content.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptVerifyMessageSignatureWithKey}
function CryptVerifyMessageSignatureWithKey(
  var pVerifyPara: CRYPT_KEY_VERIFY_MESSAGE_PARA;
  var pPublicKeyInfo: CERT_PUBLIC_KEY_INFO; pbSignedBlob: PBYTE;
  cbSignedBlob: DWORD; pbDecoded: PBYTE;
  var pcbDecoded: DWORD): BOOL; stdcall;

//+=========================================================================
//  System Certificate Store Data Structures and APIs
//==========================================================================

//+-------------------------------------------------------------------------
//  Get a system certificate store based on a subsystem protocol.
//
//  Current examples of subsystems protocols are:
//      'MY'    Cert Store hold certs with associated Private Keys
//      'CA'    Certifying Authority certs
//      'ROOT'  Root Certs
//      'SPC'   Software publisher certs
//
//
//  If hProv is NULL the default provider '1' is opened for you.
//  When the store is closed the provider is release. Otherwise
//  if hProv is not NULL, no provider is created or released.
//
//  The returned Cert Store can be searched for an appropriate Cert
//  using the Cert Store API's (see certstor.h)
//
//  When done, the cert store should be closed using CertStoreClose
//--------------------------------------------------------------------------
{$EXTERNALSYM CertOpenSystemStoreA}
function CertOpenSystemStoreA(hProv: HCRYPTPROV_LEGACY;
  szSubsystemProtocol: LPCSTR): HCERTSTORE; stdcall;
{$EXTERNALSYM CertOpenSystemStoreW}
function CertOpenSystemStoreW(hProv: HCRYPTPROV_LEGACY;
  szSubsystemProtocol: LPCWSTR): HCERTSTORE; stdcall;
{$EXTERNALSYM CertOpenSystemStore}
{$IFDEF UNICODE}
function CertOpenSystemStore(hProv: HCRYPTPROV_LEGACY;
  szSubsystemProtocol: LPCWSTR): HCERTSTORE; stdcall;
{$ELSE}
function CertOpenSystemStore(hProv: HCRYPTPROV_LEGACY;
  szSubsystemProtocol: LPCSTR): HCERTSTORE; stdcall;
{$ENDIF} // !UNICODE

{$EXTERNALSYM CertAddEncodedCertificateToSystemStoreA}
function CertAddEncodedCertificateToSystemStoreA(szCertStoreName: LPCSTR;
  pbCertEncoded: PBYTE; cbCertEncoded: DWORD): BOOL; stdcall;
{$EXTERNALSYM CertAddEncodedCertificateToSystemStoreW}
function CertAddEncodedCertificateToSystemStoreW(szCertStoreName: LPCWSTR;
  pbCertEncoded: PBYTE; cbCertEncoded: DWORD): BOOL; stdcall;
{$EXTERNALSYM CertAddEncodedCertificateToSystemStore}
{$IFDEF UNICODE}
function CertAddEncodedCertificateToSystemStore(szCertStoreName: LPCWSTR;
  pbCertEncoded: PBYTE; cbCertEncoded: DWORD): BOOL; stdcall;
{$ELSE}
function CertAddEncodedCertificateToSystemStore(szCertStoreName: LPCSTR;
  pbCertEncoded: PBYTE; cbCertEncoded: DWORD): BOOL; stdcall;
{$ENDIF} // !UNICODE

//+-------------------------------------------------------------------------
//  Find all certificate chains tying the given issuer name to any certificate
//  that the current user has a private key for.
//
//  If no certificate chain is found, FALSE is returned with LastError set
//  to CRYPT_E_NOT_FOUND and the counts zeroed.
//
//  IE 3.0 ASSUMPTION:
//   The client certificates are in the 'My' system store. The issuer
//   cerificates may be in the 'Root', 'CA' or 'My' system stores.
//--------------------------------------------------------------------------
type
  PCertChain = ^TCertChain;
  {$EXTERNALSYM _CERT_CHAIN} 
  _CERT_CHAIN = record 
    cCerts: DWORD;                        // number of certs in chain 
    certs: PCERT_BLOB;                    // pointer to array of cert chain blobs 
                                          // representing the certs 
    keyLocatorInfo: CRYPT_KEY_PROV_INFO;  // key locator for cert 
  end; 
  {$EXTERNALSYM CERT_CHAIN} 
  CERT_CHAIN = _CERT_CHAIN; 
  {$EXTERNALSYM PCERT_CHAIN} 
  PCERT_CHAIN = ^_CERT_CHAIN;
  TCertChain = _CERT_CHAIN; 

// This is not exported by crypt32, it is exported by softpub
var
  FindCertsByIssuer: function(pCertChains: PCERT_CHAIN;
    out pcbCertChains: DWORD;
    out pcCertChains: DWORD;    // count of certificates chains returned
    pbEncodedIssuerName: PBYTE; // DER encoded issuer name
    cbEncodedIssuerName: DWORD; // count in bytes of encoded issuer name
    pwszPurpose: LPCWSTR;       // 'ClientAuth' or 'CodeSigning'
    dwKeySpec: DWORD            // only return signers supporting this keyspec
  ): HRESULT stdcall;

//-------------------------------------------------------------------------
//
//  CryptQueryObject takes a CERT_BLOB or a file name and returns the
//  information about the content in the blob or in the file.
//
//  Parameters:
//  INPUT   dwObjectType:
//                       Indicate the type of the object.  Should be one of the
//                       following:
//                          CERT_QUERY_OBJECT_FILE
//                          CERT_QUERY_OBJECT_BLOB
//
//  INPUT   pvObject:
//                        If dwObjectType == CERT_QUERY_OBJECT_FILE, it is a
//                        LPWSTR, that is, the pointer to a wchar file name
//                        if dwObjectType == CERT_QUERY_OBJECT_BLOB, it is a
//                        PCERT_BLOB, that is, a pointer to a CERT_BLOB
//
//  INPUT   dwExpectedContentTypeFlags:
//                        Indicate the expected contenet type.
//                        Can be one of the following:
//                              CERT_QUERY_CONTENT_FLAG_ALL  (the content can be any type)
//                              CERT_QUERY_CONTENT_FLAG_CERT
//                              CERT_QUERY_CONTENT_FLAG_CTL
//                              CERT_QUERY_CONTENT_FLAG_CRL
//                              CERT_QUERY_CONTENT_FLAG_SERIALIZED_STORE
//                              CERT_QUERY_CONTENT_FLAG_SERIALIZED_CERT
//                              CERT_QUERY_CONTENT_FLAG_SERIALIZED_CTL
//                              CERT_QUERY_CONTENT_FLAG_SERIALIZED_CRL
//                              CERT_QUERY_CONTENT_FLAG_PKCS7_SIGNED
//                              CERT_QUERY_CONTENT_FLAG_PKCS7_UNSIGNED
//                              CERT_QUERY_CONTENT_FLAG_PKCS7_SIGNED_EMBED
//                              CERT_QUERY_CONTENT_FLAG_PKCS10
//                              CERT_QUERY_CONTENT_FLAG_PFX
//                              CERT_QUERY_CONTENT_FLAG_CERT_PAIR
//                              CERT_QUERY_CONTENT_FLAG_PFX_AND_LOAD
//
//  INPUT   dwExpectedFormatTypeFlags:
//                        Indicate the expected format type.
//                        Can be one of the following:
//                              CERT_QUERY_FORMAT_FLAG_ALL (the content can be any format)
//                              CERT_QUERY_FORMAT_FLAG_BINARY
//                              CERT_QUERY_FORMAT_FLAG_BASE64_ENCODED
//                              CERT_QUERY_FORMAT_FLAG_ASN_ASCII_HEX_ENCODED
//
//
//  INPUT   dwFlags
//                        Reserved flag.  Should always set to 0
//
//  OUTPUT  pdwMsgAndCertEncodingType
//                        Optional output.  If NULL != pdwMsgAndCertEncodingType,
//                        it contains the encoding type of the content as any
//                        combination of the following:
//                              X509_ASN_ENCODING
//                              PKCS_7_ASN_ENCODING
//
//  OUTPUT  pdwContentType
//                        Optional output.  If NULL!=pdwContentType, it contains
//                        the content type as one of the the following:
//                              CERT_QUERY_CONTENT_CERT
//                              CERT_QUERY_CONTENT_CTL
//                              CERT_QUERY_CONTENT_CRL
//                              CERT_QUERY_CONTENT_SERIALIZED_STORE
//                              CERT_QUERY_CONTENT_SERIALIZED_CERT
//                              CERT_QUERY_CONTENT_SERIALIZED_CTL
//                              CERT_QUERY_CONTENT_SERIALIZED_CRL
//                              CERT_QUERY_CONTENT_PKCS7_SIGNED
//                              CERT_QUERY_CONTENT_PKCS7_UNSIGNED
//                              CERT_QUERY_CONTENT_PKCS7_SIGNED_EMBED
//                              CERT_QUERY_CONTENT_PKCS10
//                              CERT_QUERY_CONTENT_PFX
//                              CERT_QUERY_CONTENT_CERT_PAIR
//                              CERT_QUERY_CONTENT_PFX_AND_LOAD
//
//  OUTPUT  pdwFormatType
//                        Optional output.  If NULL !=pdwFormatType, it
//                        contains the format type of the content as one of the
//                        following:
//                              CERT_QUERY_FORMAT_BINARY
//                              CERT_QUERY_FORMAT_BASE64_ENCODED
//                              CERT_QUERY_FORMAT_ASN_ASCII_HEX_ENCODED
//
//
//  OUTPUT  phCertStore
//                        Optional output.  If NULL !=phStore,
//                        it contains a cert store that includes all of certificates,
//                        CRL, and CTL in the object if the object content type is
//                        one of the following:
//                              CERT_QUERY_CONTENT_CERT
//                              CERT_QUERY_CONTENT_CTL
//                              CERT_QUERY_CONTENT_CRL
//                              CERT_QUERY_CONTENT_SERIALIZED_STORE
//                              CERT_QUERY_CONTENT_SERIALIZED_CERT
//                              CERT_QUERY_CONTENT_SERIALIZED_CTL
//                              CERT_QUERY_CONTENT_SERIALIZED_CRL
//                              CERT_QUERY_CONTENT_PKCS7_SIGNED
//                              CERT_QUERY_CONTENT_PKCS7_SIGNED_EMBED
//                              CERT_QUERY_CONTENT_CERT_PAIR
//
//                       Caller should free *phCertStore via CertCloseStore.
//
//
//  OUTPUT  phMsg        Optional output.  If NULL != phMsg,
//                        it contains a handle to a opened message if
//                        the content type is one of the following:
//                              CERT_QUERY_CONTENT_PKCS7_SIGNED
//                              CERT_QUERY_CONTENT_PKCS7_UNSIGNED
//                              CERT_QUERY_CONTENT_PKCS7_SIGNED_EMBED
//
//                       Caller should free *phMsg via CryptMsgClose.
//
//  OUTPUT pContext     Optional output.  If NULL != pContext,
//                      it contains either a PCCERT_CONTEXT or PCCRL_CONTEXT,
//                      or PCCTL_CONTEXT based on the content type.
//
//                      If the content type is CERT_QUERY_CONTENT_CERT or
//                      CERT_QUERY_CONTENT_SERIALIZED_CERT, it is a PCCERT_CONTEXT;
//                      Caller should free the pContext via CertFreeCertificateContext.
//
//                      If the content type is CERT_QUERY_CONTENT_CRL or
//                      CERT_QUERY_CONTENT_SERIALIZED_CRL, it is a PCCRL_CONTEXT;
//                      Caller should free the pContext via CertFreeCRLContext.
//
//                      If the content type is CERT_QUERY_CONTENT_CTL or
//                      CERT_QUERY_CONTENT_SERIALIZED_CTL, it is a PCCTL_CONTEXT;
//                      Caller should free the pContext via CertFreeCTLContext.
//
//  If the *pbObject is of type CERT_QUERY_CONTENT_PKCS10 or CERT_QUERY_CONTENT_PFX, CryptQueryObject
//  will not return anything in *phCertstore, *phMsg, or *ppvContext.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptQueryObject}
function CryptQueryObject(dwObjectType: DWORD; pvObject: Pointer;
  dwExpectedContentTypeFlags, dwExpectedFormatTypeFlags, dwFlags: DWORD;
  var pdwMsgAndCertEncodingType, pdwContentType, pdwFormatType: DWORD;
  var phCertStore: HCERTSTORE; var phMsg: HCRYPTMSG;
  var ppvContext: Pointer): BOOL; stdcall;

const
  //-------------------------------------------------------------------------
  //dwObjectType for CryptQueryObject
  //-------------------------------------------------------------------------
  {$EXTERNALSYM CERT_QUERY_OBJECT_FILE}
  CERT_QUERY_OBJECT_FILE = $00000001;
  {$EXTERNALSYM CERT_QUERY_OBJECT_BLOB}
  CERT_QUERY_OBJECT_BLOB = $00000002;

  //-------------------------------------------------------------------------
  //dwContentType for CryptQueryObject
  //-------------------------------------------------------------------------
  //encoded single certificate
  {$EXTERNALSYM CERT_QUERY_CONTENT_CERT}
  CERT_QUERY_CONTENT_CERT                 = 1;
  //encoded single CTL
  {$EXTERNALSYM CERT_QUERY_CONTENT_CTL}
  CERT_QUERY_CONTENT_CTL                  = 2;
  //encoded single CRL
  {$EXTERNALSYM CERT_QUERY_CONTENT_CRL}
  CERT_QUERY_CONTENT_CRL                  = 3;
  //serialized store
  {$EXTERNALSYM CERT_QUERY_CONTENT_SERIALIZED_STORE}
  CERT_QUERY_CONTENT_SERIALIZED_STORE     = 4;
  //serialized single certificate
  {$EXTERNALSYM CERT_QUERY_CONTENT_SERIALIZED_CERT}
  CERT_QUERY_CONTENT_SERIALIZED_CERT      = 5;
  //serialized single CTL
  {$EXTERNALSYM CERT_QUERY_CONTENT_SERIALIZED_CTL}
  CERT_QUERY_CONTENT_SERIALIZED_CTL       = 6;
  //serialized single CRL
  {$EXTERNALSYM CERT_QUERY_CONTENT_SERIALIZED_CRL}
  CERT_QUERY_CONTENT_SERIALIZED_CRL       = 7;
  //a PKCS#7 signed message
  {$EXTERNALSYM CERT_QUERY_CONTENT_PKCS7_SIGNED}
  CERT_QUERY_CONTENT_PKCS7_SIGNED         = 8;
  //a PKCS#7 message, such as enveloped message.  But it is not a signed message,
  {$EXTERNALSYM CERT_QUERY_CONTENT_PKCS7_UNSIGNED}
  CERT_QUERY_CONTENT_PKCS7_UNSIGNED       = 9;
  //a PKCS7 signed message embedded in a file
  {$EXTERNALSYM CERT_QUERY_CONTENT_PKCS7_SIGNED_EMBED}
  CERT_QUERY_CONTENT_PKCS7_SIGNED_EMBED   = 10;
  //an encoded PKCS#10
  {$EXTERNALSYM CERT_QUERY_CONTENT_PKCS10}
  CERT_QUERY_CONTENT_PKCS10               = 11;
  //an encoded PFX BLOB
  {$EXTERNALSYM CERT_QUERY_CONTENT_PFX}
  CERT_QUERY_CONTENT_PFX                  = 12;
  //an encoded CertificatePair (contains forward and/or reverse cross certs)
  {$EXTERNALSYM CERT_QUERY_CONTENT_CERT_PAIR}
  CERT_QUERY_CONTENT_CERT_PAIR            = 13;
  //an encoded PFX BLOB, which was loaded to phCertStore
  {$EXTERNALSYM CERT_QUERY_CONTENT_PFX_AND_LOAD}
  CERT_QUERY_CONTENT_PFX_AND_LOAD         = 14;

  //-------------------------------------------------------------------------
  //dwExpectedConentTypeFlags for CryptQueryObject
  //-------------------------------------------------------------------------

  //encoded single certificate
  {$EXTERNALSYM CERT_QUERY_CONTENT_FLAG_CERT}
  CERT_QUERY_CONTENT_FLAG_CERT             = (1 shl CERT_QUERY_CONTENT_CERT);

  //encoded single CTL
  {$EXTERNALSYM CERT_QUERY_CONTENT_FLAG_CTL}
  CERT_QUERY_CONTENT_FLAG_CTL              = (1 shl CERT_QUERY_CONTENT_CTL);

  //encoded single CRL
  {$EXTERNALSYM CERT_QUERY_CONTENT_FLAG_CRL}
  CERT_QUERY_CONTENT_FLAG_CRL              = (1 shl CERT_QUERY_CONTENT_CRL);

  //serialized store
  {$EXTERNALSYM CERT_QUERY_CONTENT_FLAG_SERIALIZED_STORE}
  CERT_QUERY_CONTENT_FLAG_SERIALIZED_STORE =
    (1 shl CERT_QUERY_CONTENT_SERIALIZED_STORE);

  //serialized single certificate
  {$EXTERNALSYM CERT_QUERY_CONTENT_FLAG_SERIALIZED_CERT}
  CERT_QUERY_CONTENT_FLAG_SERIALIZED_CERT  =
    (1 shl CERT_QUERY_CONTENT_SERIALIZED_CERT);

  //serialized single CTL
  {$EXTERNALSYM CERT_QUERY_CONTENT_FLAG_SERIALIZED_CTL}
  CERT_QUERY_CONTENT_FLAG_SERIALIZED_CTL   =
    (1 shl CERT_QUERY_CONTENT_SERIALIZED_CTL);

  //serialized single CRL
  {$EXTERNALSYM CERT_QUERY_CONTENT_FLAG_SERIALIZED_CRL}
  CERT_QUERY_CONTENT_FLAG_SERIALIZED_CRL   =
    (1 shl CERT_QUERY_CONTENT_SERIALIZED_CRL);

  //an encoded PKCS#7 signed message
  {$EXTERNALSYM CERT_QUERY_CONTENT_FLAG_PKCS7_SIGNED}
  CERT_QUERY_CONTENT_FLAG_PKCS7_SIGNED     =
    (1 shl CERT_QUERY_CONTENT_PKCS7_SIGNED);

  //an encoded PKCS#7 message.  But it is not a signed message
  {$EXTERNALSYM CERT_QUERY_CONTENT_FLAG_PKCS7_UNSIGNED}
  CERT_QUERY_CONTENT_FLAG_PKCS7_UNSIGNED   =
    (1 shl CERT_QUERY_CONTENT_PKCS7_UNSIGNED);

  //the content includes an embedded PKCS7 signed message
  {$EXTERNALSYM CERT_QUERY_CONTENT_FLAG_PKCS7_SIGNED_EMBED}
  CERT_QUERY_CONTENT_FLAG_PKCS7_SIGNED_EMBED =
    (1 shl CERT_QUERY_CONTENT_PKCS7_SIGNED_EMBED);

  //an encoded PKCS#10
  {$EXTERNALSYM CERT_QUERY_CONTENT_FLAG_PKCS10}
  CERT_QUERY_CONTENT_FLAG_PKCS10           = (1 shl CERT_QUERY_CONTENT_PKCS10);

  //an encoded PFX BLOB
  {$EXTERNALSYM CERT_QUERY_CONTENT_FLAG_PFX}
  CERT_QUERY_CONTENT_FLAG_PFX              = (1 shl CERT_QUERY_CONTENT_PFX);

  //an encoded CertificatePair (contains forward and/or reverse cross certs)
  {$EXTERNALSYM CERT_QUERY_CONTENT_FLAG_CERT_PAIR}
  CERT_QUERY_CONTENT_FLAG_CERT_PAIR        =
    (1 shl CERT_QUERY_CONTENT_CERT_PAIR);

  //an encoded PFX BLOB, and we do want to load it (not included in
  //CERT_QUERY_CONTENT_FLAG_ALL)
  {$EXTERNALSYM CERT_QUERY_CONTENT_FLAG_PFX_AND_LOAD}
  CERT_QUERY_CONTENT_FLAG_PFX_AND_LOAD     =
    (1 shl CERT_QUERY_CONTENT_PFX_AND_LOAD);

  //content can be any type
  {$EXTERNALSYM CERT_QUERY_CONTENT_FLAG_ALL}
  CERT_QUERY_CONTENT_FLAG_ALL = (CERT_QUERY_CONTENT_FLAG_CERT or
                                 CERT_QUERY_CONTENT_FLAG_CTL or
                                 CERT_QUERY_CONTENT_FLAG_CRL or
                                 CERT_QUERY_CONTENT_FLAG_SERIALIZED_STORE or
                                 CERT_QUERY_CONTENT_FLAG_SERIALIZED_CERT or
                                 CERT_QUERY_CONTENT_FLAG_SERIALIZED_CTL or
                                 CERT_QUERY_CONTENT_FLAG_SERIALIZED_CRL or
                                 CERT_QUERY_CONTENT_FLAG_PKCS7_SIGNED or
                                 CERT_QUERY_CONTENT_FLAG_PKCS7_UNSIGNED or
                                 CERT_QUERY_CONTENT_FLAG_PKCS7_SIGNED_EMBED or
                                 CERT_QUERY_CONTENT_FLAG_PKCS10 or
                                 CERT_QUERY_CONTENT_FLAG_PFX or
                                 CERT_QUERY_CONTENT_FLAG_CERT_PAIR);


  //-------------------------------------------------------------------------
  //dwFormatType for CryptQueryObject
  //-------------------------------------------------------------------------
  //the content is in binary format
  {$EXTERNALSYM CERT_QUERY_FORMAT_BINARY}
  CERT_QUERY_FORMAT_BINARY                 = 1;

  //the content is base64 encoded
  {$EXTERNALSYM CERT_QUERY_FORMAT_BASE64_ENCODED}
  CERT_QUERY_FORMAT_BASE64_ENCODED         = 2;

  //the content is ascii hex encoded with '{ASN}' prefix
  {$EXTERNALSYM CERT_QUERY_FORMAT_ASN_ASCII_HEX_ENCODED}
  CERT_QUERY_FORMAT_ASN_ASCII_HEX_ENCODED  = 3;

  //-------------------------------------------------------------------------
  //dwExpectedFormatTypeFlags for CryptQueryObject
  //-------------------------------------------------------------------------
  //the content is in binary format
  {$EXTERNALSYM CERT_QUERY_FORMAT_FLAG_BINARY}
  CERT_QUERY_FORMAT_FLAG_BINARY            = (1 shl CERT_QUERY_FORMAT_BINARY);

  //the content is base64 encoded
  {$EXTERNALSYM CERT_QUERY_FORMAT_FLAG_BASE64_ENCODED}
  CERT_QUERY_FORMAT_FLAG_BASE64_ENCODED    =
    (1 shl CERT_QUERY_FORMAT_BASE64_ENCODED);

  //the content is ascii hex encoded with '{ASN}' prefix
  {$EXTERNALSYM CERT_QUERY_FORMAT_FLAG_ASN_ASCII_HEX_ENCODED}
  CERT_QUERY_FORMAT_FLAG_ASN_ASCII_HEX_ENCODED =
    (1 shl CERT_QUERY_FORMAT_ASN_ASCII_HEX_ENCODED);

  //the content can be of any format
  {$EXTERNALSYM CERT_QUERY_FORMAT_FLAG_ALL}
  CERT_QUERY_FORMAT_FLAG_ALL               =
    (CERT_QUERY_FORMAT_FLAG_BINARY or
     CERT_QUERY_FORMAT_FLAG_BASE64_ENCODED or
     CERT_QUERY_FORMAT_FLAG_ASN_ASCII_HEX_ENCODED);

//
// Crypt32 Memory Management Routines.  All Crypt32 API which return allocated
// buffers will do so via CryptMemAlloc, CryptMemRealloc.  Clients can free
// those buffers using CryptMemFree.  Also included is CryptMemSize
//

{$EXTERNALSYM CryptMemAlloc}
function CryptMemAlloc(cbSize: ULONG): Pointer; stdcall;

{$EXTERNALSYM CryptMemRealloc}
function CryptMemRealloc(pv: Pointer; cbSize: ULONG): Pointer; stdcall;

{$EXTERNALSYM CryptMemFree}
procedure CryptMemFree(pv: Pointer) stdcall;

//
// Crypt32 Asynchronous Parameter Management Routines.  All Crypt32 API which
// expose asynchronous mode operation use a Crypt32 Async Handle to pass
// around information about the operation e.g. callback routines.  The
// following API are used for manipulation of the async handle
//

// Following functions were never used. If called, will fail with LastError
// set to ERROR_CALL_NOT_IMPLEMENTED.

type
  {$EXTERNALSYM HCRYPTASYNC}
  HCRYPTASYNC = THandle;
  {$EXTERNALSYM PHCRYPTASYNC}
  PHCRYPTASYNC = ^HCRYPTASYNC;

  {$EXTERNALSYM PFN_CRYPT_ASYNC_PARAM_FREE_FUNC}
  PFN_CRYPT_ASYNC_PARAM_FREE_FUNC = procedure(pszParamOid: LPSTR;
    pvParam: Pointer) stdcall;
  TFnCryptAsyncParamFreeFunc = PFN_CRYPT_ASYNC_PARAM_FREE_FUNC;

{$EXTERNALSYM CryptCreateAsyncHandle}
function CryptCreateAsyncHandle(dwFlags: DWORD;
  var phAsync: HCRYPTASYNC): BOOL; stdcall;

{$EXTERNALSYM CryptSetAsyncParam}
function CryptSetAsyncParam(hAsync: HCRYPTASYNC; pszParamOid: LPSTR;
  pvParam: Pointer; pfnFree: PFN_CRYPT_ASYNC_PARAM_FREE_FUNC): BOOL; stdcall;

{$EXTERNALSYM CryptGetAsyncParam}
function CryptGetAsyncParam(hAsync: HCRYPTASYNC; pszParamOid: LPSTR;
  var ppvParam: Pointer;
  var ppfnFree: PFN_CRYPT_ASYNC_PARAM_FREE_FUNC): BOOL; stdcall;

{$EXTERNALSYM CryptCloseAsyncHandle}
function CryptCloseAsyncHandle(hAsync: HCRYPTASYNC): BOOL; stdcall;

//
// Crypt32 Remote Object Retrieval Routines.  This API allows retrieval of
// remote PKI objects where the location is given by an URL.  The remote
// object retrieval manager exposes two provider models.  One is the 'Scheme
// Provider' model which allows for installable protocol providers as defined
// by the URL scheme e.g. ldap, http, ftp.  The scheme provider entry point is
// the same as the CryptRetrieveObjectByUrl however the *ppvObject returned
// is ALWAYS a counted array of encoded bits (one per object retrieved).  The
// second provider model is the 'Context Provider' model which allows for
// installable creators of CAPI2 context handles (objects) based on the
// retrieved encoded bits.  These are dispatched based on the object OID given
// in the call to CryptRetrieveObjectByUrl.
//
type
  PCryptBlobArray = ^TCryptBlobArray;
  {$EXTERNALSYM _CRYPT_BLOB_ARRAY}
  _CRYPT_BLOB_ARRAY = record
    cBlob: DWORD;
    rgBlob: PCRYPT_DATA_BLOB;
  end;
  {$EXTERNALSYM CRYPT_BLOB_ARRAY}
  CRYPT_BLOB_ARRAY = _CRYPT_BLOB_ARRAY;
  {$EXTERNALSYM PCRYPT_BLOB_ARRAY}
  PCRYPT_BLOB_ARRAY = ^_CRYPT_BLOB_ARRAY;
  TCryptBlobArray = _CRYPT_BLOB_ARRAY;

  PCryptCredentials = ^TCryptCredentials;
  {$EXTERNALSYM _CRYPT_CREDENTIALS} 
  _CRYPT_CREDENTIALS = record 
    cbSize: DWORD; 
    pszCredentialsOid: LPCSTR; 
    pvCredentials: Pointer; 
  end; 
  {$EXTERNALSYM CRYPT_CREDENTIALS} 
  CRYPT_CREDENTIALS = _CRYPT_CREDENTIALS; 
  {$EXTERNALSYM PCRYPT_CREDENTIALS} 
  PCRYPT_CREDENTIALS = ^_CRYPT_CREDENTIALS; 
  TCryptCredentials = _CRYPT_CREDENTIALS; 

const
  {$EXTERNALSYM CREDENTIAL_OID_PASSWORD_CREDENTIALS_A}
  CREDENTIAL_OID_PASSWORD_CREDENTIALS_A = LPCSTR(1);
  {$EXTERNALSYM CREDENTIAL_OID_PASSWORD_CREDENTIALS_W}
  CREDENTIAL_OID_PASSWORD_CREDENTIALS_W = LPCSTR(2);

  {$EXTERNALSYM CREDENTIAL_OID_PASSWORD_CREDENTIALS}
  {$IFDEF UNICODE}
  CREDENTIAL_OID_PASSWORD_CREDENTIALS = CREDENTIAL_OID_PASSWORD_CREDENTIALS_W;
  {$ELSE}
  CREDENTIAL_OID_PASSWORD_CREDENTIALS = CREDENTIAL_OID_PASSWORD_CREDENTIALS_A;
  {$ENDIF} //UNICODE

type
  PCryptPasswordCredentialsA = ^TCryptPasswordCredentialsA;
  {$EXTERNALSYM _CRYPT_PASSWORD_CREDENTIALSA} 
  _CRYPT_PASSWORD_CREDENTIALSA = record 
    cbSize: DWORD; 
    pszUsername: LPSTR; 
    pszPassword: LPSTR; 
  end; 
  {$EXTERNALSYM CRYPT_PASSWORD_CREDENTIALSA} 
  CRYPT_PASSWORD_CREDENTIALSA = _CRYPT_PASSWORD_CREDENTIALSA; 
  {$EXTERNALSYM PCRYPT_PASSWORD_CREDENTIALSA} 
  PCRYPT_PASSWORD_CREDENTIALSA = ^_CRYPT_PASSWORD_CREDENTIALSA; 
  TCryptPasswordCredentialsA = _CRYPT_PASSWORD_CREDENTIALSA;

  PCryptPasswordCredentialsW = ^TCryptPasswordCredentialsW;
  {$EXTERNALSYM _CRYPT_PASSWORD_CREDENTIALSW}
  _CRYPT_PASSWORD_CREDENTIALSW = record
    cbSize: DWORD;
    pszUsername: LPWSTR;
    pszPassword: LPWSTR;
  end;
  {$EXTERNALSYM CRYPT_PASSWORD_CREDENTIALSW}
  CRYPT_PASSWORD_CREDENTIALSW = _CRYPT_PASSWORD_CREDENTIALSW;
  {$EXTERNALSYM PCRYPT_PASSWORD_CREDENTIALSW}
  PCRYPT_PASSWORD_CREDENTIALSW = ^_CRYPT_PASSWORD_CREDENTIALSW;
  TCryptPasswordCredentialsW = _CRYPT_PASSWORD_CREDENTIALSW;

  PCryptPasswordCredentials = ^TCryptPasswordCredentials;
  {$EXTERNALSYM CRYPT_PASSWORD_CREDENTIALS}
  {$EXTERNALSYM PCRYPT_PASSWORD_CREDENTIALS}
{$IFDEF UNICODE}
  CRYPT_PASSWORD_CREDENTIALS = CRYPT_PASSWORD_CREDENTIALSW;
  PCRYPT_PASSWORD_CREDENTIALS = PCRYPT_PASSWORD_CREDENTIALSW;
  TCryptPasswordCredentials = CRYPT_PASSWORD_CREDENTIALSW;
{$ELSE}
  CRYPT_PASSWORD_CREDENTIALS = CRYPT_PASSWORD_CREDENTIALSA;
  PCRYPT_PASSWORD_CREDENTIALS = PCRYPT_PASSWORD_CREDENTIALSA;
  TCryptPasswordCredentials = CRYPT_PASSWORD_CREDENTIALSA;
{$ENDIF} // UNICODE

//
// Scheme Provider Signatures
//

const
// The following is obsolete and has been replaced with the following
// definition
  {$EXTERNALSYM SCHEME_OID_RETRIEVE_ENCODED_OBJECT_FUNC}
  SCHEME_OID_RETRIEVE_ENCODED_OBJECT_FUNC = 'SchemeDllRetrieveEncodedObject';

// 2-8-02 Server 2003 changed to use UNICODE Url strings instead of multibyte
  {$EXTERNALSYM SCHEME_OID_RETRIEVE_ENCODED_OBJECTW_FUNC}
  SCHEME_OID_RETRIEVE_ENCODED_OBJECTW_FUNC = 'SchemeDllRetrieveEncodedObjectW';

type
  {$EXTERNALSYM PFN_FREE_ENCODED_OBJECT_FUNC}
  PFN_FREE_ENCODED_OBJECT_FUNC = procedure(pszObjectOid: LPCSTR;
    out pObject: CRYPT_BLOB_ARRAY; pvFreeContext: Pointer);
  TFnFreeEncodedObjectFunc = PFN_FREE_ENCODED_OBJECT_FUNC;

//
// SchemeDllRetrieveEncodedObject was replaced in Server 2003 with
// the following. (Changed to use UNICODE Url Strings.)
//

//
// SchemeDllRetrieveEncodedObjectW has the following signature:
//
// __success(return == TRUE)
// BOOL WINAPI SchemeDllRetrieveEncodedObjectW (
//                    LPCWSTR pwszUrl,
//                    LPCSTR pszObjectOid,
//                    DWORD dwRetrievalFlags,
//                    DWORD dwTimeout,                // milliseconds
//                    PCRYPT_BLOB_ARRAY pObject,
//                   __deref_out __callback PFN_FREE_ENCODED_OBJECT_FUNC* ppfnFreeObject,
//                   __deref_ LPVOID* ppvFreeContext,
//                    HCRYPTASYNC hAsyncRetrieve,
//                    PCRYPT_CREDENTIALS pCredentials,
//                    PCRYPT_RETRIEVE_AUX_INFO pAuxInfo
//                   )
//

//
// Context Provider Signatures
//
const
  {$EXTERNALSYM CONTEXT_OID_CREATE_OBJECT_CONTEXT_FUNC}
  CONTEXT_OID_CREATE_OBJECT_CONTEXT_FUNC = 'ContextDllCreateObjectContext';

  {$EXTERNALSYM CONTEXT_OID_CERTIFICATE}
  CONTEXT_OID_CERTIFICATE = LPCSTR(1);
  {$EXTERNALSYM CONTEXT_OID_CRL}
  CONTEXT_OID_CRL         = LPCSTR(2);
  {$EXTERNALSYM CONTEXT_OID_CTL}
  CONTEXT_OID_CTL         = LPCSTR(3);
  {$EXTERNALSYM CONTEXT_OID_PKCS7}
  CONTEXT_OID_PKCS7       = LPCSTR(4);
  {$EXTERNALSYM CONTEXT_OID_CAPI2_ANY}
  CONTEXT_OID_CAPI2_ANY   = LPCSTR(5);
  {$EXTERNALSYM CONTEXT_OID_OCSP_RESP}
  CONTEXT_OID_OCSP_RESP   = LPCSTR(6);

  //
  // ContextDllCreateObjectContext has the following signature:
  //
  // __success(return == TRUE)
  // BOOL WINAPI ContextDllCreateObjectContext (
  //                     LPCSTR pszObjectOid,
  //                     DWORD dwRetrievalFlags,
  //                     PCRYPT_BLOB_ARRAY pObject,
  //                    __deref_out LPVOID* ppvContext
  //                    )
  //

  //
  // Remote Object Retrieval API
  //

  //
  // Retrieval flags
  //

  {$EXTERNALSYM CRYPT_RETRIEVE_MULTIPLE_OBJECTS}
  CRYPT_RETRIEVE_MULTIPLE_OBJECTS       = $00000001;
  {$EXTERNALSYM CRYPT_CACHE_ONLY_RETRIEVAL}
  CRYPT_CACHE_ONLY_RETRIEVAL            = $00000002;
  {$EXTERNALSYM CRYPT_WIRE_ONLY_RETRIEVAL}
  CRYPT_WIRE_ONLY_RETRIEVAL             = $00000004;
  {$EXTERNALSYM CRYPT_DONT_CACHE_RESULT}
  CRYPT_DONT_CACHE_RESULT               = $00000008;
  {$EXTERNALSYM CRYPT_ASYNC_RETRIEVAL}
  CRYPT_ASYNC_RETRIEVAL                 = $00000010;
  {$EXTERNALSYM CRYPT_STICKY_CACHE_RETRIEVAL}
  CRYPT_STICKY_CACHE_RETRIEVAL          = $00001000;
  {$EXTERNALSYM CRYPT_LDAP_SCOPE_BASE_ONLY_RETRIEVAL}
  CRYPT_LDAP_SCOPE_BASE_ONLY_RETRIEVAL  = $00002000;
  {$EXTERNALSYM CRYPT_OFFLINE_CHECK_RETRIEVAL}
  CRYPT_OFFLINE_CHECK_RETRIEVAL         = $00004000;

  // When the following flag is set, the following 2 NULL terminated ascii
  // strings are inserted at the beginning of each returned blob:
  //  '%d\0%s\0', dwEntryIndex, pszAttribute
  //
  //  The first dwEntryIndex is 0, '0\0'.
  //
  // When set, pszObjectOid must be NULL, so that a PCRYPT_BLOB_ARRAY is returned.
  {$EXTERNALSYM CRYPT_LDAP_INSERT_ENTRY_ATTRIBUTE}
  CRYPT_LDAP_INSERT_ENTRY_ATTRIBUTE     = $00008000;

  // Set this flag to digitally sign all of the ldap traffic to and from a
  // Windows 2000 LDAP server using the Kerberos authentication protocol.
  // This feature provides integrity required by some applications.
  {$EXTERNALSYM CRYPT_LDAP_SIGN_RETRIEVAL}
  CRYPT_LDAP_SIGN_RETRIEVAL             = $00010000;

  // Set this flag to inhibit automatic authentication handling. See the
  // wininet flag, INTERNET_FLAG_NO_AUTH, for more details.
  {$EXTERNALSYM CRYPT_NO_AUTH_RETRIEVAL}
  CRYPT_NO_AUTH_RETRIEVAL               = $00020000;

  // Performs an A-Record only DNS lookup on the supplied host string.
  // This prevents bogus DNS queries from being generated when resolving host
  // names. Use this flag whenever passing a hostname as opposed to a
  // domain name for the hostname parameter.
  //
  // See LDAP_OPT_AREC_EXCLUSIVE defined in winldap.h for more details.
  {$EXTERNALSYM CRYPT_LDAP_AREC_EXCLUSIVE_RETRIEVAL}
  CRYPT_LDAP_AREC_EXCLUSIVE_RETRIEVAL   = $00040000;

  // Apply AIA URL restrictions, such as, validate retrieved content before
  // writing to cache.
  {$EXTERNALSYM CRYPT_AIA_RETRIEVAL}
  CRYPT_AIA_RETRIEVAL                   = $00080000;

  // For HTTP: use POST instead of the default GET
  //
  // The POST additional binary data and header strings are appended to
  // the host name and path URL as follows:
  //  + '/'<Optional url escaped and base64 encoded additional data>
  //  + '?'<Optional additional headers>
  //
  // Here's an example of an OCSP POST URL:
  //  http://ocsp.openvalidation.org/MEIwQDA%2BMDwwOjAJBgUrDgMCGgUABBQdKNE
  //      wjytjKBQADcgM61jfflNpyQQUv1NDgnjQnsOA5RtnygUA37lIg6UCA
  //      QI%3D?Content-Type: application/ocsp-request
  //
  //
  // When this flag is set, CryptRetrieveObjectByUrl, searches for the
  // last '/' and '?' POST marker characters in the URL string.
  // These are removed from the URL before it is passed to the WinHttp
  // APIs. The '?' string is passed as the AdditionHeaders to
  // WinHttpSendRequest. The '/' string is url unescaped (%xx converted
  // to appropriate character) and base64 decoded into binary. This
  // decoded binary is passed as the additional data to WinHttpSendRequest.
  {$EXTERNALSYM CRYPT_HTTP_POST_RETRIEVAL}
  CRYPT_HTTP_POST_RETRIEVAL             = $00100000;

  // When this flag is set we won't attempt to bypass any potential proxy caches.
  // If a proxy cache wasn't explicitly bypassed, fProxyCacheRetrieval will be
  // set in pAuxInfo. Only applicable to http URL retrievals.
  {$EXTERNALSYM CRYPT_PROXY_CACHE_RETRIEVAL}
  CRYPT_PROXY_CACHE_RETRIEVAL           = $00200000;

  // When this flag is set, for a conditional retrieval returning not modified,
  // TRUE is returned and *ppvObject is set to NULL. For a nonNULL pAuxInfo,
  // dwHttpStatusCode is set to winhttp.h's HTTP_STATUS_NOT_MODIFIED. Otherwise,
  // *ppvObject is updated for a successful retrieval. Only applicable to
  // http URL retrievals.
  {$EXTERNALSYM CRYPT_NOT_MODIFIED_RETRIEVAL}
  CRYPT_NOT_MODIFIED_RETRIEVAL          = $00400000;

  //
  // Data verification retrieval flags
  //
  // CRYPT_VERIFY_CONTEXT_SIGNATURE is used to get signature verification
  // on the context created.  In this case pszObjectOid must be non-NULL and
  // pvVerify points to the signer certificate context
  //
  // CRYPT_VERIFY_DATA_HASH is used to get verification of the blob data
  // retrieved by the protocol.  The pvVerify points to an URL_DATA_HASH
  // structure (TBD)
  //

  {$EXTERNALSYM CRYPT_VERIFY_CONTEXT_SIGNATURE}
  CRYPT_VERIFY_CONTEXT_SIGNATURE        = $00000020;
  {$EXTERNALSYM CRYPT_VERIFY_DATA_HASH}
  CRYPT_VERIFY_DATA_HASH                = $00000040;

  //
  // Time Valid Object flags
  //

  {$EXTERNALSYM CRYPT_KEEP_TIME_VALID}
  CRYPT_KEEP_TIME_VALID                 = $00000080;
  {$EXTERNALSYM CRYPT_DONT_VERIFY_SIGNATURE}
  CRYPT_DONT_VERIFY_SIGNATURE           = $00000100;
  {$EXTERNALSYM CRYPT_DONT_CHECK_TIME_VALIDITY}
  CRYPT_DONT_CHECK_TIME_VALIDITY        = $00000200;

  // The default checks if ftNextUpdate >= ftValidFor. Set this flag to
  // check if ftThisUpdate >= ftValidFor.
  {$EXTERNALSYM CRYPT_CHECK_FRESHNESS_TIME_VALIDITY}
  CRYPT_CHECK_FRESHNESS_TIME_VALIDITY   = $00000400;

  {$EXTERNALSYM CRYPT_ACCUMULATIVE_TIMEOUT}
  CRYPT_ACCUMULATIVE_TIMEOUT            = $00000800;

  // Set this flag to only use OCSP AIA URLs.
  {$EXTERNALSYM CRYPT_OCSP_ONLY_RETRIEVAL}
  CRYPT_OCSP_ONLY_RETRIEVAL             = $01000000;

//
// Cryptnet URL Cache Pre-Fetch Info
//
type
  PCryptnetUrlCachePreFetchInfo = ^TCryptnetUrlCachePreFetchInfo; 
  {$EXTERNALSYM _CRYPTNET_URL_CACHE_PRE_FETCH_INFO} 
  _CRYPTNET_URL_CACHE_PRE_FETCH_INFO = record
    cbSize: DWORD;
    dwObjectType: DWORD;

    // Possible errors:
    //  S_OK                - Pending
    //  ERROR_MEDIA_OFFLINE - CRL pre-fetch disabled due to OCSP offline.
    //  ERROR_FILE_OFFLINE  - Unchanged pre-fetch content
    //  ERROR_INVALID_DATA  - Invalid pre-fetch content
    //  Other errors        - Unable to retrieve pre-fetch content
    dwError: DWORD; 
    dwReserved: DWORD; 
    ThisUpdateTime: FILETIME; 
    NextUpdateTime: FILETIME; 
    PublishTime: FILETIME;                // May be zero 
  end; 
  {$EXTERNALSYM CRYPTNET_URL_CACHE_PRE_FETCH_INFO} 
  CRYPTNET_URL_CACHE_PRE_FETCH_INFO = _CRYPTNET_URL_CACHE_PRE_FETCH_INFO; 
  {$EXTERNALSYM PCRYPTNET_URL_CACHE_PRE_FETCH_INFO} 
  PCRYPTNET_URL_CACHE_PRE_FETCH_INFO = ^_CRYPTNET_URL_CACHE_PRE_FETCH_INFO; 
  TCryptnetUrlCachePreFetchInfo = _CRYPTNET_URL_CACHE_PRE_FETCH_INFO;

const
  // Pre-fetch ObjectTypes
  {$EXTERNALSYM CRYPTNET_URL_CACHE_PRE_FETCH_NONE}
  CRYPTNET_URL_CACHE_PRE_FETCH_NONE = 0;
  {$EXTERNALSYM CRYPTNET_URL_CACHE_PRE_FETCH_BLOB}
  CRYPTNET_URL_CACHE_PRE_FETCH_BLOB = 1;
  {$EXTERNALSYM CRYPTNET_URL_CACHE_PRE_FETCH_CRL}
  CRYPTNET_URL_CACHE_PRE_FETCH_CRL = 2;
  {$EXTERNALSYM CRYPTNET_URL_CACHE_PRE_FETCH_OCSP}
  CRYPTNET_URL_CACHE_PRE_FETCH_OCSP = 3;
  {$EXTERNALSYM CRYPTNET_URL_CACHE_PRE_FETCH_AUTOROOT_CAB}
  CRYPTNET_URL_CACHE_PRE_FETCH_AUTOROOT_CAB = 5;


//
// Cryptnet URL Cache Flush Info
//
type
  PCryptnetUrlCacheFlushInfo = ^TCryptnetUrlCacheFlushInfo; 
  {$EXTERNALSYM _CRYPTNET_URL_CACHE_FLUSH_INFO} 
  _CRYPTNET_URL_CACHE_FLUSH_INFO = record 
    cbSize: DWORD; 
    // If pre-fetching is enabled, following is ignored 
    // 
    // 0          - use default flush exempt seconds (2 weeks) 
    // $FFFFFFFF - disable flushing 
    dwExemptSeconds: DWORD;

    // Time the object expires. The above dwExemptSeconds is added to 
    // to determine the flush time. The LastSyncTime is used if 
    // after this time. 
    ExpireTime: FILETIME; 
  end; 
  {$EXTERNALSYM CRYPTNET_URL_CACHE_FLUSH_INFO} 
  CRYPTNET_URL_CACHE_FLUSH_INFO = _CRYPTNET_URL_CACHE_FLUSH_INFO; 
  {$EXTERNALSYM PCRYPTNET_URL_CACHE_FLUSH_INFO} 
  PCRYPTNET_URL_CACHE_FLUSH_INFO = ^_CRYPTNET_URL_CACHE_FLUSH_INFO; 
  TCryptnetUrlCacheFlushInfo = _CRYPTNET_URL_CACHE_FLUSH_INFO;

const
  {$EXTERNALSYM CRYPTNET_URL_CACHE_DEFAULT_FLUSH}
  CRYPTNET_URL_CACHE_DEFAULT_FLUSH = 0;
  {$EXTERNALSYM CRYPTNET_URL_CACHE_DISABLE_FLUSH}
  CRYPTNET_URL_CACHE_DISABLE_FLUSH = $FFFFFFFF;

//
// Cryptnet URL Cache Response Info
//
type
  PCryptnetUrlCacheResponseInfo = ^TCryptnetUrlCacheResponseInfo;
  {$EXTERNALSYM _CRYPTNET_URL_CACHE_RESPONSE_INFO}
  _CRYPTNET_URL_CACHE_RESPONSE_INFO = record
    cbSize: DWORD;
    wResponseType: WORD;
    wResponseFlags: WORD;
    // The following are zero if not present
    LastModifiedTime: FILETIME;
    dwMaxAge: DWORD;
    pwszETag: LPCWSTR;
    dwProxyId: DWORD;
  end;
  {$EXTERNALSYM CRYPTNET_URL_CACHE_RESPONSE_INFO}
  CRYPTNET_URL_CACHE_RESPONSE_INFO = _CRYPTNET_URL_CACHE_RESPONSE_INFO;
  {$EXTERNALSYM PCRYPTNET_URL_CACHE_RESPONSE_INFO}
  PCRYPTNET_URL_CACHE_RESPONSE_INFO = ^_CRYPTNET_URL_CACHE_RESPONSE_INFO;
  TCryptnetUrlCacheResponseInfo = _CRYPTNET_URL_CACHE_RESPONSE_INFO;

const
  // ResponseTypes
  {$EXTERNALSYM CRYPTNET_URL_CACHE_RESPONSE_NONE}
  CRYPTNET_URL_CACHE_RESPONSE_NONE = 0;
  {$EXTERNALSYM CRYPTNET_URL_CACHE_RESPONSE_HTTP}
  CRYPTNET_URL_CACHE_RESPONSE_HTTP = 1;

  // ResponseFlags
  {$EXTERNALSYM CRYPTNET_URL_CACHE_RESPONSE_VALIDATED}
  CRYPTNET_URL_CACHE_RESPONSE_VALIDATED = $8000;

//
// CryptRetrieveObjectByUrl Auxilliary Info
//
//
// All unused fields in this data structure must be zeroed. More fields
// could be added in a future release.
//
type
  PCryptRetrieveAuxInfo = ^TCryptRetrieveAuxInfo;
  {$EXTERNALSYM _CRYPT_RETRIEVE_AUX_INFO} 
  _CRYPT_RETRIEVE_AUX_INFO = record 
    cbSize: DWORD; 
    pLastSyncTime: ^FILETIME; 
    // 0 => implies no limit 
    dwMaxUrlRetrievalByteCount: DWORD; 
    // To get any PreFetchInfo, set the following pointer to a 
    // CRYPTNET_URL_CACHE_PRE_FETCH_INFO structure with its cbSize set 
    // upon input. For no PreFetchInfo, except for cbSize, the data 
    // structure is zeroed upon return. 
    pPreFetchInfo: PCRYPTNET_URL_CACHE_PRE_FETCH_INFO; 
    // To get any FlushInfo, set the following pointer to a 
    // CRYPTNET_URL_CACHE_FLUSH_INFO structure with its cbSize set 
    // upon input. For no FlushInfo, except for cbSize, the data structure 
    // is zeroed upon return. 
    pFlushInfo: PCRYPTNET_URL_CACHE_FLUSH_INFO; 
    // To get any ResponseInfo, set the following pointer to the address 
    // of a PCRYPTNET_URL_CACHE_RESPONSE_INFO pointer updated with 
    // the allocated structure. For no ResponseInfo, *ppResponseInfo is set 
    // to NULL. Otherwise, *ppResponseInfo must be free via CryptMemFree(). 
    ppResponseInfo: ^PCRYPTNET_URL_CACHE_RESPONSE_INFO; 
    // If nonNULL, the specified prefix string is prepended to the 
    // cached filename. 
    pwszCacheFileNamePrefix: LPWSTR; 
    // If nonNULL, any cached information before this time is considered 
    // time invalid. For CRYPT_CACHE_ONLY_RETRIEVAL, if there is a 
    // cached entry before this time, LastError is set to ERROR_INVALID_TIME. 
    // Also used to set max-age for http retrievals. 
    pftCacheResync: PFileTime; 
    // The following flag is set upon return if CRYPT_PROXY_CACHE_RETRIEVAL 
    // was set in dwRetrievalFlags and the proxy cache wasn't explicitly 
    // bypassed for the retrieval. This flag won't be explicitly cleared. 
    // This flag will only be set for http URL retrievals. 
    fProxyCacheRetrieval: BOOL; 
    // This value is only updated upon return for a nonSuccessful status code 
    // returned in a HTTP response header. This value won't be explicitly 
    // cleared. This value will only be updated for http or https URL 
    // retrievals. 
    // 
    // If CRYPT_NOT_MODIFIED_RETRIEVAL was set in dwFlags, set to winhttp.h's 
    // HTTP_STATUS_NOT_MODIFIED if the retrieval returned not modified. In 
    // this case TRUE is returned with *ppvObject set to NULL. 
    dwHttpStatusCode: DWORD; 
  end; 
  {$EXTERNALSYM CRYPT_RETRIEVE_AUX_INFO} 
  CRYPT_RETRIEVE_AUX_INFO = _CRYPT_RETRIEVE_AUX_INFO; 
  {$EXTERNALSYM PCRYPT_RETRIEVE_AUX_INFO} 
  PCRYPT_RETRIEVE_AUX_INFO = ^_CRYPT_RETRIEVE_AUX_INFO; 
  TCryptRetrieveAuxInfo = _CRYPT_RETRIEVE_AUX_INFO; 

{$EXTERNALSYM CryptRetrieveObjectByUrlA}
function CryptRetrieveObjectByUrlA(pszUrl, pszObjectOid: LPCSTR;
  dwRetrievalFlags, dwTimeout: DWORD; out ppvObject: Pointer;
  hAsyncRetrieve: HCRYPTASYNC; var pCredentials: CRYPT_CREDENTIALS;
  pvVerify: Pointer; var pAuxInfo: CRYPT_RETRIEVE_AUX_INFO): BOOL; stdcall;
{$EXTERNALSYM CryptRetrieveObjectByUrlW}
function CryptRetrieveObjectByUrlW(pszUrl: LPCWSTR; pszObjectOid: LPCSTR;
  dwRetrievalFlags, dwTimeout: DWORD; out ppvObject: Pointer;
  hAsyncRetrieve: HCRYPTASYNC; var pCredentials: CRYPT_CREDENTIALS;
  pvVerify: Pointer; var pAuxInfo: CRYPT_RETRIEVE_AUX_INFO): BOOL; stdcall;
{$IFDEF UNICODE}
function CryptRetrieveObjectByUrl(pszUrl: LPCWSTR; pszObjectOid: LPCSTR;
  dwRetrievalFlags, dwTimeout: DWORD; out ppvObject: Pointer;
  hAsyncRetrieve: HCRYPTASYNC; var pCredentials: CRYPT_CREDENTIALS;
  pvVerify: Pointer; var pAuxInfo: CRYPT_RETRIEVE_AUX_INFO): BOOL; stdcall;
{$ELSE}
function CryptRetrieveObjectByUrl(pszUrl, pszObjectOid: LPCSTR;
  dwRetrievalFlags, dwTimeout: DWORD; out ppvObject: Pointer;
  hAsyncRetrieve: HCRYPTASYNC; var pCredentials: CRYPT_CREDENTIALS;
  pvVerify: Pointer; var pAuxInfo: CRYPT_RETRIEVE_AUX_INFO): BOOL; stdcall;
{$ENDIF} // !UNICODE

//
// Call back function to cancel object retrieval
//
// The function can be installed on a per thread basis.
// If CryptInstallCancelRetrieval is called for multiple times, only the most recent
// installation will be kept.
//
// This is only effective for http, https, gopher, and ftp protocol.
// It is ignored by the rest of the protocols.
type
  {$EXTERNALSYM PFN_CRYPT_CANCEL_RETRIEVAL}
  PFN_CRYPT_CANCEL_RETRIEVAL = function(dwFlags: DWORD;
    pvArg: Pointer): BOOL stdcall;
  TFnCryptCancelRetrieval = PFN_CRYPT_CANCEL_RETRIEVAL;

//
// PFN_CRYPT_CANCEL_RETRIEVAL
//
// This function should return FALSE when the object retrieval should be continued
// and return TRUE when the object retrieval should be cancelled.
//

{$EXTERNALSYM CryptInstallCancelRetrieval}
function CryptInstallCancelRetrieval(pfnCancel: PFN_CRYPT_CANCEL_RETRIEVAL;
  pvArg: Pointer; dwFlags: DWORD; pvReserved: Pointer): BOOL; stdcall;

{$EXTERNALSYM CryptUninstallCancelRetrieval}
function CryptUninstallCancelRetrieval(dwFlags: DWORD;
  pvReserved: Pointer): BOOL; stdcall;

{$EXTERNALSYM CryptCancelAsyncRetrieval}
function CryptCancelAsyncRetrieval(hAsyncRetrieval: HCRYPTASYNC): BOOL; stdcall;

//
// Remote Object Async Retrieval parameters
//

//
// A client that wants to be notified of asynchronous object retrieval
// completion sets this parameter on the async handle
//
const
  {$EXTERNALSYM CRYPT_PARAM_ASYNC_RETRIEVAL_COMPLETION}
  CRYPT_PARAM_ASYNC_RETRIEVAL_COMPLETION = LPCSTR(1);

type
  {$EXTERNALSYM PFN_CRYPT_ASYNC_RETRIEVAL_COMPLETION_FUNC}
  PFN_CRYPT_ASYNC_RETRIEVAL_COMPLETION_FUNC = procedure(
    pvCompletion: Pointer; dwCompletionCode: DWORD; pszUrl: LPCSTR;
    pszObjectOid: LPSTR; pvObject: Pointer) stdcall;
  TFnCryptAsyncRetrievalCompletionFunc =
    PFN_CRYPT_ASYNC_RETRIEVAL_COMPLETION_FUNC;

  PCryptAsyncRetrievalCompletion = ^TCryptAsyncRetrievalCompletion;
  {$EXTERNALSYM _CRYPT_ASYNC_RETRIEVAL_COMPLETION} 
  _CRYPT_ASYNC_RETRIEVAL_COMPLETION = record 
    pfnCompletion: PFN_CRYPT_ASYNC_RETRIEVAL_COMPLETION_FUNC; { __callback }
    pvCompletion: Pointer; 
  end; 
  {$EXTERNALSYM CRYPT_ASYNC_RETRIEVAL_COMPLETION} 
  CRYPT_ASYNC_RETRIEVAL_COMPLETION = _CRYPT_ASYNC_RETRIEVAL_COMPLETION; 
  {$EXTERNALSYM PCRYPT_ASYNC_RETRIEVAL_COMPLETION} 
  PCRYPT_ASYNC_RETRIEVAL_COMPLETION = ^_CRYPT_ASYNC_RETRIEVAL_COMPLETION; 
  TCryptAsyncRetrievalCompletion = _CRYPT_ASYNC_RETRIEVAL_COMPLETION; 

//
// This function is set on the async handle by a scheme provider that
// supports asynchronous retrieval
//
const
  {$EXTERNALSYM CRYPT_PARAM_CANCEL_ASYNC_RETRIEVAL}
  CRYPT_PARAM_CANCEL_ASYNC_RETRIEVAL = LPCSTR(2);

type
  {$EXTERNALSYM PFN_CANCEL_ASYNC_RETRIEVAL_FUNC}
  PFN_CANCEL_ASYNC_RETRIEVAL_FUNC = function(
    hAsyncRetrieve: HCRYPTASYNC): BOOL stdcall;
  TFnCancelAsyncRetrievalFunc = PFN_CANCEL_ASYNC_RETRIEVAL_FUNC;

//
// Get the locator for a CAPI object
//
const
  {$EXTERNALSYM CRYPT_GET_URL_FROM_PROPERTY}
  CRYPT_GET_URL_FROM_PROPERTY           = $00000001;
  {$EXTERNALSYM CRYPT_GET_URL_FROM_EXTENSION}
  CRYPT_GET_URL_FROM_EXTENSION          = $00000002;
  {$EXTERNALSYM CRYPT_GET_URL_FROM_UNAUTH_ATTRIBUTE}
  CRYPT_GET_URL_FROM_UNAUTH_ATTRIBUTE   = $00000004;
  {$EXTERNALSYM CRYPT_GET_URL_FROM_AUTH_ATTRIBUTE}
  CRYPT_GET_URL_FROM_AUTH_ATTRIBUTE     = $00000008;

type
  PCryptUrlArray = ^TCryptUrlArray; 
  {$EXTERNALSYM _CRYPT_URL_ARRAY} 
  _CRYPT_URL_ARRAY = record 
    cUrl: DWORD; 
    rgwszUrl: ^LPWSTR; 
  end; 
  {$EXTERNALSYM CRYPT_URL_ARRAY} 
  CRYPT_URL_ARRAY = _CRYPT_URL_ARRAY; 
  {$EXTERNALSYM PCRYPT_URL_ARRAY} 
  PCRYPT_URL_ARRAY = ^_CRYPT_URL_ARRAY; 
  TCryptUrlArray = _CRYPT_URL_ARRAY; 

  PCryptUrlInfo = ^TCryptUrlInfo;
  {$EXTERNALSYM _CRYPT_URL_INFO}
  _CRYPT_URL_INFO = record
    cbSize: DWORD;
    // Seconds between syncs
    dwSyncDeltaTime: DWORD;
    // Returned URLs may be grouped. For instance, groups of cross cert
    // distribution points. Each distribution point may have multiple
    // URLs, (LDAP and HTTP scheme).
    cGroup: DWORD;
    rgcGroupEntry: ^DWORD;
  end;
  {$EXTERNALSYM CRYPT_URL_INFO}
  CRYPT_URL_INFO = _CRYPT_URL_INFO;
  {$EXTERNALSYM PCRYPT_URL_INFO}
  PCRYPT_URL_INFO = ^_CRYPT_URL_INFO;
  TCryptUrlInfo = _CRYPT_URL_INFO;

function CryptGetObjectUrl(pszUrlOid: LPCSTR; pvPara: Pointer; dwFlags: DWORD;
  var pUrlArray: CRYPT_URL_ARRAY; out pcbUrlArray: DWORD;
  var pUrlInfo: CRYPT_URL_INFO; var pcbUrlInfo: DWORD;
  pvReserved: Pointer): BOOL; stdcall;

const
  {$EXTERNALSYM URL_OID_GET_OBJECT_URL_FUNC}
  URL_OID_GET_OBJECT_URL_FUNC = 'UrlDllGetObjectUrl';

//
// UrlDllGetObjectUrl has the same signature as CryptGetObjectUrl
//

//
// URL_OID_CERTIFICATE_ISSUER
//
// pvPara == PCCERT_CONTEXT, certificate whose issuer's URL is being requested
//
// This will be retrieved from the authority info access extension or property
// on the certificate
//
// URL_OID_CERTIFICATE_CRL_DIST_POINT
//
// pvPara == PCCERT_CONTEXT, certificate whose CRL distribution point is being
// requested
//
// This will be retrieved from the CRL distribution point extension or property
// on the certificate
//
// URL_OID_CTL_ISSUER
//
// pvPara == PCCTL_CONTEXT, Signer Index, CTL whose issuer's URL (identified
// by the signer index) is being requested
//
// This will be retrieved from an authority info access attribute method encoded
// in each signer info in the PKCS7 (CTL)
//
// URL_OID_CTL_NEXT_UPDATE
//
// pvPara == PCCTL_CONTEXT, Signer Index, CTL whose next update URL is being
// requested and an optional signer index in case we need to check signer
// info attributes
//
// This will be retrieved from an authority info access CTL extension, property,
// or signer info attribute method
//
// URL_OID_CRL_ISSUER
//
// pvPara == PCCRL_CONTEXT, CRL whose issuer's URL is being requested
//
// This will be retrieved from a property on the CRL which has been inherited
// from the subject cert (either from the subject cert issuer or the subject
// cert distribution point extension).  It will be encoded as an authority
// info access extension method.
//
// URL_OID_CERTIFICATE_FRESHEST_CRL
//
// pvPara == PCCERT_CONTEXT, certificate whose freshest CRL distribution point
// is being requested
//
// This will be retrieved from the freshest CRL extension or property
// on the certificate
//
// URL_OID_CRL_FRESHEST_CRL
//
// pvPara == PCCERT_CRL_CONTEXT_PAIR, certificate's base CRL whose
// freshest CRL distribution point is being requested
//
// This will be retrieved from the freshest CRL extension or property
// on the CRL
//
// URL_OID_CROSS_CERT_DIST_POINT
//
// pvPara == PCCERT_CONTEXT, certificate whose cross certificate distribution
// point is being requested
//
// This will be retrieved from the cross certificate distribution point
// extension or property on the certificate
//
// URL_OID_CERTIFICATE_OCSP
//
// pvPara == PCCERT_CONTEXT, certificate whose OCSP URL is being requested
//
// This will be retrieved from the authority info access extension or property
// on the certificate
//
// URL_OID_CERTIFICATE_OCSP_AND_CRL_DIST_POINT
//
// pvPara == PCCERT_CONTEXT, certificate whose OCSP URL and
// CRL distribution point are being requested
//
// This will be retrieved from the authority info access and
// CRL distribution point extension or property on the certificate.
// If any OCSP URLs are present, they will be first with each URL prefixed
// with 'ocsp:'. The 'ocsp:' prefix should be removed before using.
//
// URL_OID_CERTIFICATE_CRL_DIST_POINT_AND_OCSP
//
// Same as URL_OID_CERTIFICATE_OCSP_AND_CRL_DIST_POINT, except,
// the CRL URLs will be first
//
// URL_OID_CROSS_CERT_SUBJECT_INFO_ACCESS
//
// pvPara == PCCERT_CONTEXT, certificate whose cross certificates
// are being requested
//
// This will be retrieved from the Authority Info Access
// extension or property on the certificate. Only access methods
// matching szOID_PKIX_CA_REPOSITORY will be returned.
const
  {$EXTERNALSYM URL_OID_CERTIFICATE_ISSUER}
  URL_OID_CERTIFICATE_ISSUER                  = LPCSTR(1);
  {$EXTERNALSYM URL_OID_CERTIFICATE_CRL_DIST_POINT}
  URL_OID_CERTIFICATE_CRL_DIST_POINT          = LPCSTR(2);
  {$EXTERNALSYM URL_OID_CTL_ISSUER}
  URL_OID_CTL_ISSUER                          = LPCSTR(3);
  {$EXTERNALSYM URL_OID_CTL_NEXT_UPDATE}
  URL_OID_CTL_NEXT_UPDATE                     = LPCSTR(4);
  {$EXTERNALSYM URL_OID_CRL_ISSUER}
  URL_OID_CRL_ISSUER                          = LPCSTR(5);
  {$EXTERNALSYM URL_OID_CERTIFICATE_FRESHEST_CRL}
  URL_OID_CERTIFICATE_FRESHEST_CRL            = LPCSTR(6);
  {$EXTERNALSYM URL_OID_CRL_FRESHEST_CRL}
  URL_OID_CRL_FRESHEST_CRL                    = LPCSTR(7);
  {$EXTERNALSYM URL_OID_CROSS_CERT_DIST_POINT}
  URL_OID_CROSS_CERT_DIST_POINT               = LPCSTR(8);
  {$EXTERNALSYM URL_OID_CERTIFICATE_OCSP}
  URL_OID_CERTIFICATE_OCSP                    = LPCSTR(9);
  {$EXTERNALSYM URL_OID_CERTIFICATE_OCSP_AND_CRL_DIST_POINT}
  URL_OID_CERTIFICATE_OCSP_AND_CRL_DIST_POINT = LPCSTR(10);
  {$EXTERNALSYM URL_OID_CERTIFICATE_CRL_DIST_POINT_AND_OCSP}
  URL_OID_CERTIFICATE_CRL_DIST_POINT_AND_OCSP = LPCSTR(11);
  {$EXTERNALSYM URL_OID_CROSS_CERT_SUBJECT_INFO_ACCESS}
  URL_OID_CROSS_CERT_SUBJECT_INFO_ACCESS      = LPCSTR(12);

type
  PCertCrlContextPair = ^TCertCrlContextPair;
  {$EXTERNALSYM _CERT_CRL_CONTEXT_PAIR}
  _CERT_CRL_CONTEXT_PAIR = record
    pCertContext: PCCERT_CONTEXT;
    pCrlContext: PCCRL_CONTEXT;
  end;
  {$EXTERNALSYM CERT_CRL_CONTEXT_PAIR}
  CERT_CRL_CONTEXT_PAIR = _CERT_CRL_CONTEXT_PAIR;
  {$EXTERNALSYM PCERT_CRL_CONTEXT_PAIR}
  PCERT_CRL_CONTEXT_PAIR = ^_CERT_CRL_CONTEXT_PAIR;
  TCertCrlContextPair = _CERT_CRL_CONTEXT_PAIR;
  {$EXTERNALSYM PCCERT_CRL_CONTEXT_PAIR}
  PCCERT_CRL_CONTEXT_PAIR = ^CERT_CRL_CONTEXT_PAIR; 

//
// Get a time valid CAPI2 object
//

//+-------------------------------------------------------------------------
//  The following optional Extra Info may be passed to
//  CryptGetTimeValidObject().
//
//  All unused fields in this data structure must be zeroed. More fields
//  could be added in a future release.
//--------------------------------------------------------------------------
  PCryptGetTimeValidObjectExtraInfo = ^TCryptGetTimeValidObjectExtraInfo;
  {$EXTERNALSYM _CRYPT_GET_TIME_VALID_OBJECT_EXTRA_INFO}
  _CRYPT_GET_TIME_VALID_OBJECT_EXTRA_INFO = record
    cbSize: DWORD;
    // If > 0, check that the CRL's number is >=
    iDeltaCrlIndicator: Integer;
    // If nonNULL, any cached information before this time is considered
    // time invalid and forces a wire retrieval.
    pftCacheResync: PFileTime;
    // If nonNull, returns the cache's LastSyncTime
    pLastSyncTime: PFileTime;
    // If nonNull, returns the internal MaxAge expiration time
    // for the object. If the object doesn't have a MaxAge expiration, set
    // to zero.
    pMaxAgeTime: PFileTime;
  end;
  {$EXTERNALSYM CRYPT_GET_TIME_VALID_OBJECT_EXTRA_INFO}
  CRYPT_GET_TIME_VALID_OBJECT_EXTRA_INFO = _CRYPT_GET_TIME_VALID_OBJECT_EXTRA_INFO;
  {$EXTERNALSYM PCRYPT_GET_TIME_VALID_OBJECT_EXTRA_INFO}
  PCRYPT_GET_TIME_VALID_OBJECT_EXTRA_INFO = ^_CRYPT_GET_TIME_VALID_OBJECT_EXTRA_INFO;
  TCryptGetTimeValidObjectExtraInfo = _CRYPT_GET_TIME_VALID_OBJECT_EXTRA_INFO;

{$EXTERNALSYM CryptGetTimeValidObject}
function CryptGetTimeValidObject(pszTimeValidOid: LPCSTR; pvPara: Pointer;
  pIssuer: PCCERT_CONTEXT; var pftValidFor: TFileTime;
  dwFlags: DWORD; dwTimeout: DWORD; var ppvObject: Pointer;
  var pCredentials: CRYPT_CREDENTIALS;
  var pExtraInfo: CRYPT_GET_TIME_VALID_OBJECT_EXTRA_INFO): BOOL; stdcall;

const
  {$EXTERNALSYM TIME_VALID_OID_GET_OBJECT_FUNC}
  TIME_VALID_OID_GET_OBJECT_FUNC = 'TimeValidDllGetObject';

//
// TimeValidDllGetObject has the same signature as CryptGetTimeValidObject
//

//
// TIME_VALID_OID_GET_CTL
//
// pvPara == PCCTL_CONTEXT, the current CTL
//
// TIME_VALID_OID_GET_CRL
//
// pvPara == PCCRL_CONTEXT, the current CRL
//
// TIME_VALID_OID_GET_CRL_FROM_CERT
//
// pvPara == PCCERT_CONTEXT, the subject cert
//
// TIME_VALID_OID_GET_FRESHEST_CRL_FROM_CERT
//
// pvPara == PCCERT_CONTEXT, the subject cert
//
// TIME_VALID_OID_GET_FRESHEST_CRL_FROM_CRL
//
// pvPara == PCCERT_CRL_CONTEXT_PAIR, the subject cert and its base CRL
//
const
  {$EXTERNALSYM TIME_VALID_OID_GET_CTL}
  TIME_VALID_OID_GET_CTL                        = LPCSTR(1);
  {$EXTERNALSYM TIME_VALID_OID_GET_CRL}
  TIME_VALID_OID_GET_CRL                        = LPCSTR(2);
  {$EXTERNALSYM TIME_VALID_OID_GET_CRL_FROM_CERT}
  TIME_VALID_OID_GET_CRL_FROM_CERT              = LPCSTR(3);

  {$EXTERNALSYM TIME_VALID_OID_GET_FRESHEST_CRL_FROM_CERT}
  TIME_VALID_OID_GET_FRESHEST_CRL_FROM_CERT     = LPCSTR(4);
  {$EXTERNALSYM TIME_VALID_OID_GET_FRESHEST_CRL_FROM_CRL}
  TIME_VALID_OID_GET_FRESHEST_CRL_FROM_CRL      = LPCSTR(5);

{$EXTERNALSYM CryptFlushTimeValidObject}
function CryptFlushTimeValidObject(pszFlushTimeValidOid: LPCSTR;
  pvPara: Pointer; pIssuer: PCCERT_CONTEXT; dwFlags: DWORD;
  pvReserved: Pointer): BOOL; stdcall;

const
  {$EXTERNALSYM TIME_VALID_OID_FLUSH_OBJECT_FUNC}
  TIME_VALID_OID_FLUSH_OBJECT_FUNC = 'TimeValidDllFlushObject';

//
// TimeValidDllFlushObject has the same signature as CryptFlushTimeValidObject
//

//
// TIME_VALID_OID_FLUSH_CTL
//
// pvPara == PCCTL_CONTEXT, the CTL to flush
//
// TIME_VALID_OID_FLUSH_CRL
//
// pvPara == PCCRL_CONTEXT, the CRL to flush
//
// TIME_VALID_OID_FLUSH_CRL_FROM_CERT
//
// pvPara == PCCERT_CONTEXT, the subject cert's CRL to flush
//
// TIME_VALID_OID_FLUSH_FRESHEST_CRL_FROM_CERT
//
// pvPara == PCCERT_CONTEXT, the subject cert's freshest CRL to flush
//
// TIME_VALID_OID_FLUSH_FRESHEST_CRL_FROM_CRL
//
// pvPara == PCCERT_CRL_CONTEXT_PAIR, the subject cert and its base CRL's
// freshest CRL to flush
//
  {$EXTERNALSYM TIME_VALID_OID_FLUSH_CTL}
  TIME_VALID_OID_FLUSH_CTL                      = LPCSTR(1);
  {$EXTERNALSYM TIME_VALID_OID_FLUSH_CRL}
  TIME_VALID_OID_FLUSH_CRL                      = LPCSTR(2);
  {$EXTERNALSYM TIME_VALID_OID_FLUSH_CRL_FROM_CERT}
  TIME_VALID_OID_FLUSH_CRL_FROM_CERT            = LPCSTR(3);

  {$EXTERNALSYM TIME_VALID_OID_FLUSH_FRESHEST_CRL_FROM_CERT}
  TIME_VALID_OID_FLUSH_FRESHEST_CRL_FROM_CERT   = LPCSTR(4);
  {$EXTERNALSYM TIME_VALID_OID_FLUSH_FRESHEST_CRL_FROM_CRL}
  TIME_VALID_OID_FLUSH_FRESHEST_CRL_FROM_CRL    = LPCSTR(5);

//-------------------------------------------------------------------------
// Data Protection APIs
//-------------------------------------------------------------------------

//
// Data protection APIs enable applications to easily secure data.
//
// The base provider provides protection based on the users' logon
// credentials. The data secured with these APIs follow the same
// roaming characteristics as HKCU -- if HKCU roams, the data
// protected by the base provider may roam as well. This makes
// the API ideal for the munging of data stored in the registry.
//

//
// Prompt struct -- what to tell users about the access
//
type
  PCryptprotectPromptstruct = ^TCryptprotectPromptstruct; 
  {$EXTERNALSYM _CRYPTPROTECT_PROMPTSTRUCT} 
  _CRYPTPROTECT_PROMPTSTRUCT = record 
    cbSize: DWORD; 
    dwPromptFlags: DWORD; 
    hwndApp: HWND; 
    szPrompt: LPCWSTR; 
  end; 
  {$EXTERNALSYM CRYPTPROTECT_PROMPTSTRUCT} 
  CRYPTPROTECT_PROMPTSTRUCT = _CRYPTPROTECT_PROMPTSTRUCT; 
  {$EXTERNALSYM PCRYPTPROTECT_PROMPTSTRUCT} 
  PCRYPTPROTECT_PROMPTSTRUCT = ^_CRYPTPROTECT_PROMPTSTRUCT; 
  TCryptprotectPromptstruct = _CRYPTPROTECT_PROMPTSTRUCT; 

//
// base provider action
//
const
  {$EXTERNALSYM CRYPTPROTECT_DEFAULT_PROVIDER}
  CRYPTPROTECT_DEFAULT_PROVIDER: TGUID =
    (D1: $DF9D8CD0; D2: $1501; D3: $11D1;
     D4: ($8C, $7A, $00, $C0, $4F, $C2, $97, $EB));

  //
  // CryptProtect PromptStruct dwPromtFlags
  //
  //
  // prompt on unprotect
  {$EXTERNALSYM CRYPTPROTECT_PROMPT_ON_UNPROTECT}
  CRYPTPROTECT_PROMPT_ON_UNPROTECT      = $1;      // 1 shl 0
  //
  // prompt on protect
  {$EXTERNALSYM CRYPTPROTECT_PROMPT_ON_PROTECT}
  CRYPTPROTECT_PROMPT_ON_PROTECT        = $2;      // 1 shl 1
  {$EXTERNALSYM CRYPTPROTECT_PROMPT_RESERVED}
  CRYPTPROTECT_PROMPT_RESERVED          = $04;     // reserved, do not use.

  //
  // default to strong variant UI protection (user supplied password currently).
  {$EXTERNALSYM CRYPTPROTECT_PROMPT_STRONG}
  CRYPTPROTECT_PROMPT_STRONG            = $08;     // 1shl3

  //
  // require strong variant UI protection (user supplied password currently).
  {$EXTERNALSYM CRYPTPROTECT_PROMPT_REQUIRE_STRONG}
  CRYPTPROTECT_PROMPT_REQUIRE_STRONG    = $10; // 1shl4

  //
  // CryptProtectData and CryptUnprotectData dwFlags
  //
  // for remote-access situations where ui is not an option
  // if UI was specified on protect or unprotect operation, the call
  // will fail and GetLastError() will indicate ERROR_PASSWORD_RESTRICTION
  {$EXTERNALSYM CRYPTPROTECT_UI_FORBIDDEN}
  CRYPTPROTECT_UI_FORBIDDEN             = $1;

  //
  // per machine protected data -- any user on machine where CryptProtectData
  // took place may CryptUnprotectData
  {$EXTERNALSYM CRYPTPROTECT_LOCAL_MACHINE}
  CRYPTPROTECT_LOCAL_MACHINE            = $4;

  //
  // force credential synchronize during CryptProtectData()
  // Synchronize is only operation that occurs during this operation
  {$EXTERNALSYM CRYPTPROTECT_CRED_SYNC}
  CRYPTPROTECT_CRED_SYNC                = $8;

  //
  // Generate an Audit on protect and unprotect operations
  //
  {$EXTERNALSYM CRYPTPROTECT_AUDIT}
  CRYPTPROTECT_AUDIT                    = $10;

  //
  // Protect data with a non-recoverable key
  //
  {$EXTERNALSYM CRYPTPROTECT_NO_RECOVERY}
  CRYPTPROTECT_NO_RECOVERY              = $20;

  //
  // Verify the protection of a protected blob
  //
  {$EXTERNALSYM CRYPTPROTECT_VERIFY_PROTECTION}
  CRYPTPROTECT_VERIFY_PROTECTION        = $40;

  //
  // Regenerate the local machine protection
  //
  {$EXTERNALSYM CRYPTPROTECT_CRED_REGENERATE}
  CRYPTPROTECT_CRED_REGENERATE          = $80;

  // flags reserved for system use
  {$EXTERNALSYM CRYPTPROTECT_FIRST_RESERVED_FLAGVAL}
  CRYPTPROTECT_FIRST_RESERVED_FLAGVAL   = $0FFFFFFF;
  {$EXTERNALSYM CRYPTPROTECT_LAST_RESERVED_FLAGVAL}
  CRYPTPROTECT_LAST_RESERVED_FLAGVAL    = $FFFFFFFF;

  //
  // flags specific to base provider
  //

{$EXTERNALSYM CryptProtectData}
function CryptProtectData(var pDataIn: DATA_BLOB; szDataDescr: LPCWSTR;
  pOptionalEntropy: PDATA_BLOB; pvReserved: Pointer;
  pPromptStruct: PCRYPTPROTECT_PROMPTSTRUCT; dwFlags: DWORD;
  out pDataOut: DATA_BLOB): BOOL; stdcall;

{$EXTERNALSYM CryptUnprotectData}
function CryptUnprotectData(var pDataIn: DATA_BLOB;
  out ppszDataDescr: LPWSTR; pOptionalEntropy: PDATA_BLOB;
  pvReserved: Pointer; pPromptStruct: PCRYPTPROTECT_PROMPTSTRUCT;
  dwFlags: DWORD; out pDataOut: DATA_BLOB): BOOL; stdcall;

{$EXTERNALSYM CryptUpdateProtectedState}
function CryptUpdateProtectedState(pOldSid: PSID; pwszOldPassword: LPCWSTR;
  dwFlags: DWORD; out pdwSuccessCount, pdwFailureCount: DWORD): BOOL; stdcall;

const
  //
  // The buffer length passed into CryptProtectMemory and CryptUnprotectMemory
  // must be a multiple of this length (or zero).
  //
  {$EXTERNALSYM CRYPTPROTECTMEMORY_BLOCK_SIZE}
  CRYPTPROTECTMEMORY_BLOCK_SIZE    = 16;

  //
  // CryptProtectMemory/CryptUnprotectMemory dwFlags
  //

  //
  // Encrypt/Decrypt within current process context.
  //
  {$EXTERNALSYM CRYPTPROTECTMEMORY_SAME_PROCESS}
  CRYPTPROTECTMEMORY_SAME_PROCESS  = $00;

  //
  // Encrypt/Decrypt across process boundaries.
  // eg: encrypted buffer passed across LPC to another process which calls CryptUnprotectMemory.
  //
  {$EXTERNALSYM CRYPTPROTECTMEMORY_CROSS_PROCESS}
  CRYPTPROTECTMEMORY_CROSS_PROCESS = $01;

  //
  // Encrypt/Decrypt across callers with same LogonId.
  // eg: encrypted buffer passed across LPC to another process which calls CryptUnprotectMemory whilst impersonating.
  //
  {$EXTERNALSYM CRYPTPROTECTMEMORY_SAME_LOGON}
  CRYPTPROTECTMEMORY_SAME_LOGON    = $02;

{$EXTERNALSYM CryptProtectMemory}
function CryptProtectMemory(
  pDataIn: Pointer;             // in out data to encrypt
  cbDataIn: DWORD;              // multiple of CRYPTPROTECTMEMORY_BLOCK_SIZE
  dwFlags: DWORD): BOOL; stdcall;

{$EXTERNALSYM CryptUnprotectMemory}
function CryptUnprotectMemory(
  pDataIn: Pointer;             // in out data to decrypt
  cbDataIn: DWORD;              // multiple of CRYPTPROTECTMEMORY_BLOCK_SIZE
  dwFlags: DWORD): BOOL; stdcall;

//+=========================================================================
//  Helper functions to build certificates
//==========================================================================

//+-------------------------------------------------------------------------
//
// Builds a self-signed certificate and returns a PCCERT_CONTEXT representing
// the certificate. A hProv may be specified to build the cert context.
//
// pSubjectIssuerBlob is the DN for the certifcate. If an alternate subject
// name is desired it must be specified as an extension in the pExtensions
// parameter. pSubjectIssuerBlob can NOT be NULL, so minimually an empty DN
// must be specified.
//
// By default:
// pKeyProvInfo - The CSP is queried for the KeyProvInfo parameters. Only the Provider,
// Provider Type and Container is queried. Many CSPs don't support these
// queries and will cause a failure. In such cases the pKeyProvInfo
// must be specified (RSA BASE works fine).
//
// pSignatureAlgorithm - will default to SHA1RSA
// pStartTime will default to the current time
// pEndTime will default to 1 year
// pEntensions will be empty.
//
// The returned PCCERT_CONTEXT will reference the private keys by setting the
// CERT_KEY_PROV_INFO_PROP_ID. However, if this property is not desired specify the
// CERT_CREATE_SELFSIGN_NO_KEY_INFO in dwFlags.
//
// If the cert being built is only a dummy placeholder cert for speed it may not
// need to be signed. Signing of the cert is skipped if CERT_CREATE_SELFSIGN_NO_SIGN
// is specified in dwFlags.
//
//--------------------------------------------------------------------------
{$EXTERNALSYM CertCreateSelfSignCertificate}
function CertCreateSelfSignCertificate(
  hCryptProvOrNCryptKey: HCRYPTPROV_OR_NCRYPT_KEY_HANDLE;
  var pSubjectIssuerBlob: CERT_NAME_BLOB; dwFlags: DWORD;
  var pKeyProvInfo: CRYPT_KEY_PROV_INFO;
  var pSignatureAlgorithm: CRYPT_ALGORITHM_IDENTIFIER;
  pStartTime, pEndTime: PSYSTEMTIME;
  pExtensions: PCERT_EXTENSIONS): PCCERT_CONTEXT; stdcall;

const
  {$EXTERNALSYM CERT_CREATE_SELFSIGN_NO_SIGN}
  CERT_CREATE_SELFSIGN_NO_SIGN          = 1;
  {$EXTERNALSYM CERT_CREATE_SELFSIGN_NO_KEY_INFO}
  CERT_CREATE_SELFSIGN_NO_KEY_INFO      = 2;

//+=========================================================================
//  Key Identifier Property Data Structures and APIs
//==========================================================================

//+-------------------------------------------------------------------------
//  Get the property for the specified Key Identifier.
//
//  The Key Identifier is the SHA1 hash of the encoded CERT_PUBLIC_KEY_INFO.
//  The Key Identifier for a certificate can be obtained by getting the
//  certificate's CERT_KEY_IDENTIFIER_PROP_ID. The
//  CryptCreateKeyIdentifierFromCSP API can be called to create the Key
//  Identifier from a CSP Public Key Blob.
//
//  A Key Identifier can have the same properties as a certificate context.
//  CERT_KEY_PROV_INFO_PROP_ID is the property of most interest.
//  For CERT_KEY_PROV_INFO_PROP_ID, pvData points to a CRYPT_KEY_PROV_INFO
//  structure. Elements pointed to by fields in the pvData structure follow the
//  structure. Therefore, *pcbData will exceed the size of the structure.
//
//  If CRYPT_KEYID_ALLOC_FLAG is set, then, *pvData is updated with a
//  pointer to allocated memory. LocalFree() must be called to free the
//  allocated memory.
//
//  By default, searches the CurrentUser's list of Key Identifiers.
//  CRYPT_KEYID_MACHINE_FLAG can be set to search the LocalMachine's list
//  of Key Identifiers. When CRYPT_KEYID_MACHINE_FLAG is set, pwszComputerName
//  can also be set to specify the name of a remote computer to be searched
//  instead of the local machine.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptGetKeyIdentifierProperty}
function CryptGetKeyIdentifierProperty(var pKeyIdentifier: CRYPT_HASH_BLOB;
  dwPropId, dwFlags: DWORD; pwszComputerName: LPCWSTR;
  pvReserved, pvData: Pointer; out pcbData: DWORD): BOOL; stdcall;

const
  // When the following flag is set, searches the LocalMachine instead of the
  // CurrentUser. This flag is applicable to all the KeyIdentifierProperty APIs.
  {$EXTERNALSYM CRYPT_KEYID_MACHINE_FLAG}
  CRYPT_KEYID_MACHINE_FLAG = $00000020;

  // When the following flag is set, *pvData is updated with a pointer to
  // allocated memory. LocalFree() must be called to free the allocated memory.
  {$EXTERNALSYM CRYPT_KEYID_ALLOC_FLAG}
  CRYPT_KEYID_ALLOC_FLAG = $00008000;

//+-------------------------------------------------------------------------
//  Set the property for the specified Key Identifier.
//
//  For CERT_KEY_PROV_INFO_PROP_ID pvData points to the
//  CRYPT_KEY_PROV_INFO data structure. For all other properties, pvData
//  points to a CRYPT_DATA_BLOB.
//
//  Setting pvData == NULL, deletes the property.
//
//  Set CRYPT_KEYID_MACHINE_FLAG to set the property for a LocalMachine
//  Key Identifier. Set pwszComputerName, to select a remote computer.
//
//  If CRYPT_KEYID_DELETE_FLAG is set, the Key Identifier and all its
//  properties is deleted.
//
//  If CRYPT_KEYID_SET_NEW_FLAG is set, the set fails if the property already
//  exists. For an existing property, FALSE is returned with LastError set to
//  CRYPT_E_EXISTS.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptSetKeyIdentifierProperty}
function CryptSetKeyIdentifierProperty(var pKeyIdentifier: CRYPT_HASH_BLOB;
  dwPropId, dwFlags: DWORD; pwszComputerName: LPCWSTR; pvReserved: Pointer;
  pvData: Pointer): BOOL; stdcall;

const
  // When the following flag is set, the Key Identifier and all its properties
  // are deleted.
  {$EXTERNALSYM CRYPT_KEYID_DELETE_FLAG}
  CRYPT_KEYID_DELETE_FLAG        = $00000010;

  // When the following flag is set, the set fails if the property already
  // exists.
  {$EXTERNALSYM CRYPT_KEYID_SET_NEW_FLAG}
  CRYPT_KEYID_SET_NEW_FLAG       = $00002000;

//+-------------------------------------------------------------------------
//  For CERT_KEY_PROV_INFO_PROP_ID, rgppvData[] points to a
//  CRYPT_KEY_PROV_INFO.
//
//  Return FALSE to stop the enumeration.
//--------------------------------------------------------------------------
type
  {$EXTERNALSYM PFN_CRYPT_ENUM_KEYID_PROP}
  PFN_CRYPT_ENUM_KEYID_PROP = function(var pKeyIdentifier: CRYPT_HASH_BLOB;
    dwFlags: DWORD; pvReserved, pvArg: Pointer; cProp: DWORD;
    rgdwPropId: PDWORD; rgpvData: PPointer; var rgcbData: DWORD): BOOL stdcall;
  TFnCryptEnumKeyIDProp = PFN_CRYPT_ENUM_KEYID_PROP;

//+-------------------------------------------------------------------------
//  Enumerate the Key Identifiers.
//
//  If pKeyIdentifier is NULL, enumerates all Key Identifers. Otherwise,
//  calls the callback for the specified KeyIdentifier. If dwPropId is
//  0, calls the callback with all the properties. Otherwise, only calls
//  the callback with the specified property (cProp = 1).
//  Furthermore, when dwPropId is specified, skips KeyIdentifiers not
//  having the property.
//
//  Set CRYPT_KEYID_MACHINE_FLAG to enumerate the LocalMachine
//  Key Identifiers. Set pwszComputerName, to enumerate Key Identifiers on
//  a remote computer.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptEnumKeyIdentifierProperties}
function CryptEnumKeyIdentifierProperties(var pKeyIdentifier: CRYPT_HASH_BLOB;
  dwPropId, dwFlags: DWORD; pwszComputerName: LPCWSTR;
  pvReserved, pvArg: Pointer; pfnEnum: PFN_CRYPT_ENUM_KEYID_PROP): BOOL; stdcall;

//+-------------------------------------------------------------------------
//  Create a KeyIdentifier from the CSP Public Key Blob.
//
//  Converts the CSP PUBLICKEYSTRUC into a X.509 CERT_PUBLIC_KEY_INFO and
//  encodes. The encoded CERT_PUBLIC_KEY_INFO is SHA1 hashed to obtain
//  the Key Identifier.
//
//  By default, the pPubKeyStruc->aiKeyAlg is used to find the appropriate
//  public key Object Identifier. pszPubKeyOID can be set to override
//  the default OID obtained from the aiKeyAlg.
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptCreateKeyIdentifierFromCSP}
function CryptCreateKeyIdentifierFromCSP(dwCertEncodingType: DWORD;
  pszPubKeyOID: LPCSTR; var pPubKeyStruc: PUBLICKEYSTRUC;
  cbPubKeyStruc, dwFlags: DWORD; pvReserved: Pointer; pbHash: PBYTE;
  out pcbHash: DWORD): BOOL; stdcall;

//+=========================================================================
//  Certificate Chaining Infrastructure
//==========================================================================
const
  {$EXTERNALSYM CERT_CHAIN_CONFIG_REGPATH}
  CERT_CHAIN_CONFIG_REGPATH =
    'Software\Microsoft\Cryptography\OID\EncodingType 0\' +
    'CertDllCreateCertificateChainEngine\Config';

  // The following is a REG_BINARY. It contains the cache resync FILETIME.
  // Any cached information before this time is considered time invalid
  // and forces a wire retrieval. By default this is disabled.
  {$EXTERNALSYM CERT_CHAIN_CACHE_RESYNC_FILETIME_VALUE_NAME}
  CERT_CHAIN_CACHE_RESYNC_FILETIME_VALUE_NAME = 'ChainCacheResyncFiletime';

  // The following are REG_DWORD's. These configuration parameters are used
  // to disable different chain building semantics enabled by default. Set
  // the appropriate registry value to nonzero to disable.
  {$EXTERNALSYM CERT_CHAIN_DISABLE_MANDATORY_BASIC_CONSTRAINTS_VALUE_NAME}
  CERT_CHAIN_DISABLE_MANDATORY_BASIC_CONSTRAINTS_VALUE_NAME =
    'DisableMandatoryBasicConstraints';

  // By default the BasicConstraints extension must be present with CA enabled
  // for non-Root intermediate CA certificates.
  {$EXTERNALSYM CERT_CHAIN_DISABLE_CA_NAME_CONSTRAINTS_VALUE_NAME}
  CERT_CHAIN_DISABLE_CA_NAME_CONSTRAINTS_VALUE_NAME =
    'DisableCANameConstraints';

  // By default the NameConstraints extension is applied to the intermediate
  // CA certificates in addition to the end entity certificate.
  {$EXTERNALSYM CERT_CHAIN_DISABLE_UNSUPPORTED_CRITICAL_EXTENSIONS_VALUE_NAME}
  CERT_CHAIN_DISABLE_UNSUPPORTED_CRITICAL_EXTENSIONS_VALUE_NAME =
    'DisableUnsupportedCriticalExtensions';

  // By default any unsupported extension marked critical sets the following
  // dwErrorStatus bit: CERT_TRUST_HAS_NOT_SUPPORTED_CRITICAL_EXT.

  // The following are REG_DWORD's. These configuration parameters are used
  // to restrict Authority Info Access (AIA) URL retrieval.
  {$EXTERNALSYM CERT_CHAIN_MAX_AIA_URL_COUNT_IN_CERT_VALUE_NAME}
  CERT_CHAIN_MAX_AIA_URL_COUNT_IN_CERT_VALUE_NAME          =
    'MaxAIAUrlCountInCert';
  {$EXTERNALSYM CERT_CHAIN_MAX_AIA_URL_COUNT_IN_CERT_DEFAULT}
  CERT_CHAIN_MAX_AIA_URL_COUNT_IN_CERT_DEFAULT             = 5;

  {$EXTERNALSYM CERT_CHAIN_MAX_AIA_URL_RETRIEVAL_COUNT_PER_CHAIN_VALUE_NAME}
  CERT_CHAIN_MAX_AIA_URL_RETRIEVAL_COUNT_PER_CHAIN_VALUE_NAME =
    'MaxAIAUrlRetrievalCountPerChain';
  {$EXTERNALSYM CERT_CHAIN_MAX_AIA_URL_RETRIEVAL_COUNT_PER_CHAIN_DEFAULT}
  CERT_CHAIN_MAX_AIA_URL_RETRIEVAL_COUNT_PER_CHAIN_DEFAULT = 10;

  {$EXTERNALSYM CERT_CHAIN_MAX_AIA_URL_RETRIEVAL_BYTE_COUNT_VALUE_NAME}
  CERT_CHAIN_MAX_AIA_URL_RETRIEVAL_BYTE_COUNT_VALUE_NAME   =
    'MaxAIAUrlRetrievalByteCount';
  {$EXTERNALSYM CERT_CHAIN_MAX_AIA_URL_RETRIEVAL_BYTE_COUNT_DEFAULT}
  CERT_CHAIN_MAX_AIA_URL_RETRIEVAL_BYTE_COUNT_DEFAULT      = 100000;

  {$EXTERNALSYM CERT_CHAIN_MAX_AIA_URL_RETRIEVAL_CERT_COUNT_VALUE_NAME}
  CERT_CHAIN_MAX_AIA_URL_RETRIEVAL_CERT_COUNT_VALUE_NAME   =
    'MaxAIAUrlRetrievalCertCount';
  {$EXTERNALSYM CERT_CHAIN_MAX_AIA_URL_RETRIEVAL_CERT_COUNT_DEFAULT}
  CERT_CHAIN_MAX_AIA_URL_RETRIEVAL_CERT_COUNT_DEFAULT      = 10;

  // The following is a REG_DWORD. If the OCSP response NextUpdate is zero,
  // this value is added to the ThisUpdate to get a nonzero NextUpdate.
  {$EXTERNALSYM CERT_CHAIN_OCSP_VALIDITY_SECONDS_VALUE_NAME}
  CERT_CHAIN_OCSP_VALIDITY_SECONDS_VALUE_NAME = 'OcspValiditySeconds';
  // 12 hours
  {$EXTERNALSYM CERT_CHAIN_OCSP_VALIDITY_SECONDS_DEFAULT}
  CERT_CHAIN_OCSP_VALIDITY_SECONDS_DEFAULT    = 12 * 60 * 60;

  // The following are REG_DWORD's. These configuration parameters are
  // used by the following APIs to get a non-blocking, time valid OCSP
  // response for a server certificate chain:
  //   CertOpenServerOcspResponse
  //   CertAddRefServerOcspResponse
  //   CertCloseServerOcspResponse
  //   CertGetServerOcspResponseContext
  //   CertAddRefServerOcspResponseContext
  //   CertFreeServerOcspResponseContext

  // This is the minimum validity of the server OCSP response to be
  // returned by CertGetServerOcspResponseContext(). Since this OCSP
  // response will be returned to the client, it must be sufficiently long
  // so that the client will treat it as being time valid.
  {$EXTERNALSYM CERT_SRV_OCSP_RESP_MIN_VALIDITY_SECONDS_VALUE_NAME}
  CERT_SRV_OCSP_RESP_MIN_VALIDITY_SECONDS_VALUE_NAME =
    'SrvOcspRespMinValiditySeconds';
  // 10 minutes
  {$EXTERNALSYM CERT_SRV_OCSP_RESP_MIN_VALIDITY_SECONDS_DEFAULT}
  CERT_SRV_OCSP_RESP_MIN_VALIDITY_SECONDS_DEFAULT    = 10 * 60;

  // This is the maximum number of milliseconds for each server OCSP response
  // pre-fetch wire URL retrieval.
  {$EXTERNALSYM CERT_SRV_OCSP_RESP_URL_RETRIEVAL_TIMEOUT_MILLISECONDS_VALUE_NAME}
  CERT_SRV_OCSP_RESP_URL_RETRIEVAL_TIMEOUT_MILLISECONDS_VALUE_NAME =
    'SrvOcspRespUrlRetrievalTimeoutMilliseconds';
  // 15 seconds
  {$EXTERNALSYM CERT_SRV_OCSP_RESP_URL_RETRIEVAL_TIMEOUT_MILLISECONDS_DEFAULT}
  CERT_SRV_OCSP_RESP_URL_RETRIEVAL_TIMEOUT_MILLISECONDS_DEFAULT = (15 * 1000);

  // This is the maximum number of seconds to do a server OCSP response
  // pre-fetch retrieval before the OCSP response's NextUpdate. The
  // server OCSP response pre-fetch thread will wait until CurrentTime >=
  // NextUpdate - MaxBeforeNextUpdateSeconds before doing the next retrieval.
  {$EXTERNALSYM CERT_SRV_OCSP_RESP_MAX_BEFORE_NEXT_UPDATE_SECONDS_VALUE_NAME}
  CERT_SRV_OCSP_RESP_MAX_BEFORE_NEXT_UPDATE_SECONDS_VALUE_NAME =
    'SrvOcspRespMaxBeforeNextUpdateSeconds';
  // 4 hours
  {$EXTERNALSYM CERT_SRV_OCSP_RESP_MAX_BEFORE_NEXT_UPDATE_SECONDS_DEFAULT}
  CERT_SRV_OCSP_RESP_MAX_BEFORE_NEXT_UPDATE_SECONDS_DEFAULT = (4 * 60 * 60);

  // This is the minimum number of seconds to do a server OCSP response
  // pre-fetch retrieval before the OCSP response's NextUpdate.
  // If CurrentTime >= NextUpdate - MinBeforeNextUpdateSeconds, will wait until
  // after NextUpdate + MinAfterNextUpdateSeconds.
  {$EXTERNALSYM CERT_SRV_OCSP_RESP_MIN_BEFORE_NEXT_UPDATE_SECONDS_VALUE_NAME}
  CERT_SRV_OCSP_RESP_MIN_BEFORE_NEXT_UPDATE_SECONDS_VALUE_NAME =
    'SrvOcspRespMinBeforeNextUpdateSeconds';
  // 2 minutes
  {$EXTERNALSYM CERT_SRV_OCSP_RESP_MIN_BEFORE_NEXT_UPDATE_SECONDS_DEFAULT}
  CERT_SRV_OCSP_RESP_MIN_BEFORE_NEXT_UPDATE_SECONDS_DEFAULT = (2 * 60);

  // This is the minimum number of seconds to do a server OCSP response
  // pre-fetch retrieval after the OCSP response's NextUpdate when
  // (NextUpdate - MinBeforeNextUpdateSeconds) < CurrentTime < NextUpdate.
  {$EXTERNALSYM CERT_SRV_OCSP_RESP_MIN_AFTER_NEXT_UPDATE_SECONDS_VALUE_NAME}
  CERT_SRV_OCSP_RESP_MIN_AFTER_NEXT_UPDATE_SECONDS_VALUE_NAME=
    'SrvOcspRespMinAfterNextUpdateSeconds';
  // 1 minute
  {$EXTERNALSYM CERT_SRV_OCSP_RESP_MIN_AFTER_NEXT_UPDATE_SECONDS_DEFAULT}
  CERT_SRV_OCSP_RESP_MIN_AFTER_NEXT_UPDATE_SECONDS_DEFAULT = (1 * 60);

  // The following are REG_DWORD's. These configuration parameters are used
  // in the ordering of the revocation retrieval URLs.

  // When the number of cached OCSP URLs associated with the same CDP extension
  // equal or exceed this number, the OCSP AIA URLs aren't used.
  {$EXTERNALSYM CRYPTNET_MAX_CACHED_OCSP_PER_CRL_COUNT_VALUE_NAME}
  CRYPTNET_MAX_CACHED_OCSP_PER_CRL_COUNT_VALUE_NAME =
    'CryptnetMaxCachedOcspPerCrlCount';
  {$EXTERNALSYM CRYPTNET_MAX_CACHED_OCSP_PER_CRL_COUNT_DEFAULT}
  CRYPTNET_MAX_CACHED_OCSP_PER_CRL_COUNT_DEFAULT = 500;

  // The above registry value can be set to this value, to disable OCSP
  // when a CDP extension is present. Note, a registry value of 0, uses the
  // above default value.
  {$EXTERNALSYM CRYPTNET_OCSP_AFTER_CRL_DISABLE}
  CRYPTNET_OCSP_AFTER_CRL_DISABLE = $FFFFFFFF;

  // The following are REG_DWORD's. These configuration parameters are
  // used by the Cryptnet Url Cache Service (CUCS).

  // The following parameter is used as the default flush exempt seconds
  {$EXTERNALSYM CRYPTNET_URL_CACHE_DEFAULT_FLUSH_EXEMPT_SECONDS_VALUE_NAME}
  CRYPTNET_URL_CACHE_DEFAULT_FLUSH_EXEMPT_SECONDS_VALUE_NAME =
    'CryptnetDefaultFlushExemptSeconds';

  // 4 Weeks : 28 days * 24 hours * 60 minutes * 60 seconds
  {$EXTERNALSYM CRYPTNET_URL_CACHE_DEFAULT_FLUSH_EXEMPT_SECONDS_DEFAULT}
  CRYPTNET_URL_CACHE_DEFAULT_FLUSH_EXEMPT_SECONDS_DEFAULT = (28 * 24 * 60 * 60);

  // Following 2 parameters are used to set the lower and upper limit
  // on the max-age retrievals done before the Publish and NextUpdate times.
  {$EXTERNALSYM CRYPTNET_PRE_FETCH_MIN_MAX_AGE_SECONDS_VALUE_NAME}
  CRYPTNET_PRE_FETCH_MIN_MAX_AGE_SECONDS_VALUE_NAME =
    'CryptnetPreFetchMinMaxAgeSeconds';
  // 1 hour
  {$EXTERNALSYM CRYPTNET_PRE_FETCH_MIN_MAX_AGE_SECONDS_DEFAULT}
  CRYPTNET_PRE_FETCH_MIN_MAX_AGE_SECONDS_DEFAULT =
    (1 * 60 * 60);

  {$EXTERNALSYM CRYPTNET_PRE_FETCH_MAX_MAX_AGE_SECONDS_VALUE_NAME}
  CRYPTNET_PRE_FETCH_MAX_MAX_AGE_SECONDS_VALUE_NAME =
    'CryptnetPreFetchMaxMaxAgeSeconds';
  // 2 Weeks : 14 days * 24 hours * 60 minutes * 60 seconds
  {$EXTERNALSYM CRYPTNET_PRE_FETCH_MAX_MAX_AGE_SECONDS_DEFAULT}
  CRYPTNET_PRE_FETCH_MAX_MAX_AGE_SECONDS_DEFAULT =
    (14 * 24 * 60 * 60);

  // Following 3 parameters are used to calculate the PreFetch start before
  // the NextUpdate
  //
  // Where PreFetchStartTime = PublishTime +
  //                              PublishPeriod / AfterPublishPreFetchDivisor
  //       PreFetchEndTime = NextUpdate -
  //                              PublishPeriod / BeforeNextUpdatePreFetchDivisor
  //
  //       PreFetchPeriod = PreFetchEndTime - PreFetchStartTime
  //
  //       if (PreFetchPeriod < MinBeforeNextUpdatePreFetchPeriodSeconds)
  //          - No PreFetch is done before NextUpdate
  //       else
  //          - PreFetch starts are randomized over this period

  // The start of the PreFetch period is delayed after the start of the
  // Publish period by dividing the PublishPeriod (NextUpdate - PublishTime)
  // by this integer divisor.
  {$EXTERNALSYM CRYPTNET_PRE_FETCH_AFTER_PUBLISH_PRE_FETCH_DIVISOR_VALUE_NAME}
  CRYPTNET_PRE_FETCH_AFTER_PUBLISH_PRE_FETCH_DIVISOR_VALUE_NAME =
    'CryptnetPreFetchAfterPublishPreFetchDivisor';
  // 10, where 12 hours / 10 = 72 minutes or 1.2 hours / 10 = 7.2 minutes
  {$EXTERNALSYM CRYPTNET_PRE_FETCH_AFTER_PUBLISH_PRE_FETCH_DIVISOR_DEFAULT}
  CRYPTNET_PRE_FETCH_AFTER_PUBLISH_PRE_FETCH_DIVISOR_DEFAULT = 10;

  // The finish of the PreFetch period occurs before NextUpdate
  // by dividing the PublishPeriod (NextUpdate - PublishTime)
  // by this integer divisor.
  {$EXTERNALSYM CRYPTNET_PRE_FETCH_BEFORE_NEXT_UPDATE_PRE_FETCH_DIVISOR_VALUE_NAME}
  CRYPTNET_PRE_FETCH_BEFORE_NEXT_UPDATE_PRE_FETCH_DIVISOR_VALUE_NAME =
    'CryptnetPreFetchBeforeNextUpdatePreFetchDivisor';
  // 20, where 12 hours / 20 = 36 minutes or 1.2 hours / 10 = 3.6 minutes
  {$EXTERNALSYM CRYPTNET_PRE_FETCH_BEFORE_NEXT_UPDATE_PRE_FETCH_DIVISOR_DEFAULT}
  CRYPTNET_PRE_FETCH_BEFORE_NEXT_UPDATE_PRE_FETCH_DIVISOR_DEFAULT = 20;

  // The PreFetch period must exceed this minimum duration in seconds
  // to do a PreFetch before NextUpdate
  {$EXTERNALSYM CRYPTNET_PRE_FETCH_MIN_BEFORE_NEXT_UPDATE_PRE_FETCH_PERIOD_SECONDS_VALUE_NAME}
  CRYPTNET_PRE_FETCH_MIN_BEFORE_NEXT_UPDATE_PRE_FETCH_PERIOD_SECONDS_VALUE_NAME =
    'CryptnetPreFetchMinBeforeNextUpdatePreFetchSeconds';
  // 1 hour
  //
  // For the default OCSP period of 12 hours using above defaults,
  // PreFetchPeriod = 72 minutes - 7.2 minutes - 3.6 mintes = 61.2 minutes
  {$EXTERNALSYM CRYPTNET_PRE_FETCH_MIN_BEFORE_NEXT_UPDATE_PRE_FETCH_PERIOD_SECONDS_DEFAULT}
  CRYPTNET_PRE_FETCH_MIN_BEFORE_NEXT_UPDATE_PRE_FETCH_PERIOD_SECONDS_DEFAULT =
    (1 * 60 * 60);

  // Following 4 parameters are used to calculate the PreFetch start after
  // the NextUpdate
  //
  // ValidityPeriod = NextUpdate - ThisUpdate
  //
  // PreFetchPeriod = ValidityPeriod / AfterNextUpdatePreFetchDivisor
  //
  // Where PreFetchPeriod is decreased to MaxAfterNextUpdatePreFetchPeriodSeconds
  // or increased to MinAfterNextUpdatePreFetchPeriodSeconds;
  //
  // PreFetchStartTime = NextUpdate
  // PreFetchEndTime = PreFetchStartTime + PreFetchPeriod
  //
  // PreFetch starts are randomized over the above PreFetchPeriod
  //
  // If CurrentTime > RandomPreFetchStartTime, then, the
  // AfterCurrentTimePreFetchPeriodSeconds is randomized and added to
  // CurrentTime for the RandomPreFetchStartTime

  // The PreFetch period after NextUpdate is initially calculated by
  // dividing the ValidityPeriod (NextUpdate - ThisUpdate) by this integer
  // divisor.
  {$EXTERNALSYM CRYPTNET_PRE_FETCH_VALIDITY_PERIOD_AFTER_NEXT_UPDATE_PRE_FETCH_DIVISOR_VALUE_NAME}
  CRYPTNET_PRE_FETCH_VALIDITY_PERIOD_AFTER_NEXT_UPDATE_PRE_FETCH_DIVISOR_VALUE_NAME =
    'CryptnetPreFetchValidityPeriodAfterNextUpdatePreFetchDivisor';
  // 10, where 1 week / 10 = 16.8 hours
  {$EXTERNALSYM CRYPTNET_PRE_FETCH_VALIDITY_PERIOD_AFTER_NEXT_UPDATE_PRE_FETCH_DIVISOR_DEFAULT}
  CRYPTNET_PRE_FETCH_VALIDITY_PERIOD_AFTER_NEXT_UPDATE_PRE_FETCH_DIVISOR_DEFAULT =
    10;

  // If necessary, the above PreFetch period will be decreased
  // to this maximum duration in seconds.
  {$EXTERNALSYM CRYPTNET_PRE_FETCH_MAX_AFTER_NEXT_UPDATE_PRE_FETCH_PERIOD_SECONDS_VALUE_NAME}
  CRYPTNET_PRE_FETCH_MAX_AFTER_NEXT_UPDATE_PRE_FETCH_PERIOD_SECONDS_VALUE_NAME =
    'CryptnetPreFetchMaxAfterNextUpdatePreFetchPeriodSeconds';
  // 4 hours
  {$EXTERNALSYM CRYPTNET_PRE_FETCH_MAX_AFTER_NEXT_UPDATE_PRE_FETCH_PERIOD_SECONDS_DEFAULT}
  CRYPTNET_PRE_FETCH_MAX_AFTER_NEXT_UPDATE_PRE_FETCH_PERIOD_SECONDS_DEFAULT =
    (4 * 60 * 60);

  // If necessary, the above PreFetch period will be increased
  // to this minimum duration in seconds.
  {$EXTERNALSYM CRYPTNET_PRE_FETCH_MIN_AFTER_NEXT_UPDATE_PRE_FETCH_PERIOD_SECONDS_VALUE_NAME}
  CRYPTNET_PRE_FETCH_MIN_AFTER_NEXT_UPDATE_PRE_FETCH_PERIOD_SECONDS_VALUE_NAME =
    'CryptnetPreFetchMinAfterNextUpdatePreFetchPeriodSeconds';
  // 30 minutes
  {$EXTERNALSYM CRYPTNET_PRE_FETCH_MIN_AFTER_NEXT_UPDATE_PRE_FETCH_PERIOD_SECONDS_DEFAULT}
  CRYPTNET_PRE_FETCH_MIN_AFTER_NEXT_UPDATE_PRE_FETCH_PERIOD_SECONDS_DEFAULT =
    (30 * 60);

  // If the CurrentTime is after the above randomized start time, the following
  // parameter will be randomized and added to the CurrentTime.
  {$EXTERNALSYM CRYPTNET_PRE_FETCH_AFTER_CURRENT_TIME_PRE_FETCH_PERIOD_SECONDS_VALUE_NAME}
  CRYPTNET_PRE_FETCH_AFTER_CURRENT_TIME_PRE_FETCH_PERIOD_SECONDS_VALUE_NAME =
    'CryptnetPreFetchAfterCurrentTimePreFetchPeriodSeconds';
  // 30 minutes
  {$EXTERNALSYM CRYPTNET_PRE_FETCH_AFTER_CURRENT_TIME_PRE_FETCH_PERIOD_SECONDS_DEFAULT}
  CRYPTNET_PRE_FETCH_AFTER_CURRENT_TIME_PRE_FETCH_PERIOD_SECONDS_DEFAULT =
    (30 * 60);

  // Following parameter specifies the minimum time period between sending
  // trigger URL cache PreFetch LRPC messages to cryptsvc after doing online
  // revocation enabled chain builds.
  {$EXTERNALSYM CRYPTNET_PRE_FETCH_TRIGGER_PERIOD_SECONDS_VALUE_NAME}
  CRYPTNET_PRE_FETCH_TRIGGER_PERIOD_SECONDS_VALUE_NAME =
    'CryptnetPreFetchTriggerPeriodSeconds';
  // 10 minutes
  CRYPTNET_PRE_FETCH_TRIGGER_PERIOD_SECONDS_DEFAULT =
    (10 * 60);

  // The above registry value can be set to this value, to disable the
  // sending of trigger URL cache PreFetch LRPC messages. Note, a registry
  // value of 0, uses the above default value.
  {$EXTERNALSYM CRYPTNET_PRE_FETCH_TRIGGER_DISABLE}
  CRYPTNET_PRE_FETCH_TRIGGER_DISABLE =
    $FFFFFFFF;

  // Following parameter specifies the delay time to wait to scan the
  // URL cache directory after receiving a trigger LRPC message request.
  {$EXTERNALSYM CRYPTNET_PRE_FETCH_SCAN_AFTER_TRIGGER_DELAY_SECONDS_VALUE_NAME}
  CRYPTNET_PRE_FETCH_SCAN_AFTER_TRIGGER_DELAY_SECONDS_VALUE_NAME =
    'CryptnetPreFetchScanAfterTriggerDelaySeconds';
  // 30 seconds
  {$EXTERNALSYM CRYPTNET_PRE_FETCH_SCAN_AFTER_TRIGGER_DELAY_SECONDS_DEFAULT}
  CRYPTNET_PRE_FETCH_SCAN_AFTER_TRIGGER_DELAY_SECONDS_DEFAULT = 30;

  // Following parameter specifies the maximum amount of time to wait for any
  // PreFetch retrieval to complete
  {$EXTERNALSYM CRYPTNET_PRE_FETCH_RETRIEVAL_TIMEOUT_SECONDS_VALUE_NAME}
  CRYPTNET_PRE_FETCH_RETRIEVAL_TIMEOUT_SECONDS_VALUE_NAME =
    'CryptnetPreFetchRetrievalTimeoutSeconds';
  // 5 minutes
  {$EXTERNALSYM CRYPTNET_PRE_FETCH_RETRIEVAL_TIMEOUT_SECONDS_DEFAULT}
  CRYPTNET_PRE_FETCH_RETRIEVAL_TIMEOUT_SECONDS_DEFAULT = (5 * 60);

  //+-------------------------------------------------------------------------
  // The following configuration parameters are store in HKLM group policy
  //--------------------------------------------------------------------------

  {$EXTERNALSYM CERT_GROUP_POLICY_CHAIN_CONFIG_REGPATH}
  CERT_GROUP_POLICY_CHAIN_CONFIG_REGPATH =
    CERT_GROUP_POLICY_SYSTEM_STORE_REGPATH + '\ChainEngine\Config';

  // In Longhorn, the following have been moved from the above HKLM
  // configuration parameters:

  // The following are REG_DWORDs. These configuration parameters are used
  // to override the default URL timeouts in chain building

  // This is the default URL timeout in milliseconds
  {$EXTERNALSYM CERT_CHAIN_URL_RETRIEVAL_TIMEOUT_MILLISECONDS_VALUE_NAME}
  CERT_CHAIN_URL_RETRIEVAL_TIMEOUT_MILLISECONDS_VALUE_NAME    =
    'ChainUrlRetrievalTimeoutMilliseconds';
  // 15 seconds
  {$EXTERNALSYM CERT_CHAIN_URL_RETRIEVAL_TIMEOUT_MILLISECONDS_DEFAULT}
  CERT_CHAIN_URL_RETRIEVAL_TIMEOUT_MILLISECONDS_DEFAULT       =
    (15 * 1000);

  // This is the default revocation accumulative URL timeout in milliseconds
  // The first revocation URL retrieval uses half of this timeout
  {$EXTERNALSYM CERT_CHAIN_REV_ACCUMULATIVE_URL_RETRIEVAL_TIMEOUT_MILLISECONDS_VALUE_NAME}
  CERT_CHAIN_REV_ACCUMULATIVE_URL_RETRIEVAL_TIMEOUT_MILLISECONDS_VALUE_NAME =
    'ChainRevAccumulativeUrlRetrievalTimeoutMilliseconds';
  // 20 seconds
  {$EXTERNALSYM CERT_CHAIN_REV_ACCUMULATIVE_URL_RETRIEVAL_TIMEOUT_MILLISECONDS_DEFAULT}
  CERT_CHAIN_REV_ACCUMULATIVE_URL_RETRIEVAL_TIMEOUT_MILLISECONDS_DEFAULT =
    (20 * 1000);

  // Note, will allow the machine setting to be used if this value isn't
  // defined.

  // By default AIA OCSP URLs are before CDP CRL URLs. When the number of cached
  // OCSP URLs associated with the same CDP extension equal or exceed this
  // number, the CRL URLs are placed before the OCSP URLs.
  {$EXTERNALSYM CRYPTNET_CACHED_OCSP_SWITCH_TO_CRL_COUNT_VALUE_NAME}
  CRYPTNET_CACHED_OCSP_SWITCH_TO_CRL_COUNT_VALUE_NAME =
    'CryptnetCachedOcspSwitchToCrlCount';
  {$EXTERNALSYM CRYPTNET_CACHED_OCSP_SWITCH_TO_CRL_COUNT_DEFAULT}
  CRYPTNET_CACHED_OCSP_SWITCH_TO_CRL_COUNT_DEFAULT = 50;
  // The above registry value can be set to this value, to always place
  // the CRL URLs before the OCSP URLs. Note, a registry value of 0, uses the
  // above default value.
  {$EXTERNALSYM CRYPTNET_CRL_BEFORE_OCSP_ENABLE}
  CRYPTNET_CRL_BEFORE_OCSP_ENABLE = $FFFFFFFF;

  // Support for the following was removed in Longhorn. Changed to use
  // the following OPTIONS flags in HKLM Group Policy
  {$EXTERNALSYM CERT_CHAIN_DISABLE_AIA_URL_RETRIEVAL_VALUE_NAME}
  CERT_CHAIN_DISABLE_AIA_URL_RETRIEVAL_VALUE_NAME             =
    'DisableAIAUrlRetrieval';
  // By default AIA Url Retrieval is enabled. Set this registry value to nonzero
  // to disable

  // This is the name of the REG_DWORD for chain engine Options
  {$EXTERNALSYM CERT_CHAIN_OPTIONS_VALUE_NAME}
  CERT_CHAIN_OPTIONS_VALUE_NAME  = 'Options';

  // Disable AIA URL retrieval when this bit is set in the Options
  {$EXTERNALSYM CERT_CHAIN_OPTION_DISABLE_AIA_URL_RETRIEVAL}
  CERT_CHAIN_OPTION_DISABLE_AIA_URL_RETRIEVAL = $2;

  {$EXTERNALSYM CERT_CHAIN_CROSS_CERT_DOWNLOAD_INTERVAL_HOURS_VALUE_NAME}
  CERT_CHAIN_CROSS_CERT_DOWNLOAD_INTERVAL_HOURS_VALUE_NAME =
    'CrossCertDownloadIntervalHours';

  // 7 days
  {$EXTERNALSYM CERT_CHAIN_CROSS_CERT_DOWNLOAD_INTERVAL_HOURS_DEFAULT}
  CERT_CHAIN_CROSS_CERT_DOWNLOAD_INTERVAL_HOURS_DEFAULT = 24 * 7;

  // When not defined or zero, the CRL validity isn't extended
  {$EXTERNALSYM CERT_CHAIN_CRL_VALIDITY_EXT_PERIOD_HOURS_VALUE_NAME}
  CERT_CHAIN_CRL_VALIDITY_EXT_PERIOD_HOURS_VALUE_NAME =
    'CRLValidityExtensionPeriod';

  // 12 hour
  {$EXTERNALSYM CERT_CHAIN_CRL_VALIDITY_EXT_PERIOD_HOURS_DEFAULT}
  CERT_CHAIN_CRL_VALIDITY_EXT_PERIOD_HOURS_DEFAULT = 12;

//
// The chain engine defines the store namespace and cache partitioning for
// the Certificate Chaining infrastructure.  A default chain engine
// is defined for the process which uses all default system stores e.g.
// Root, CA, Trust, for chain building and caching.  If an application
// wishes to define its own store namespace or have its own partitioned
// cache then it can create its own chain engine.  It is advisable to create
// a chain engine at application startup and use it throughout the lifetime
// of the application in order to get optimal caching behavior
//
type
  {$EXTERNALSYM HCERTCHAINENGINE}
  HCERTCHAINENGINE = THandle;

const
  {$EXTERNALSYM HCCE_CURRENT_USER}
  HCCE_CURRENT_USER  = HCERTCHAINENGINE(0);
  {$EXTERNALSYM HCCE_LOCAL_MACHINE}
  HCCE_LOCAL_MACHINE = HCERTCHAINENGINE($1);

//
// Create a certificate chain engine.
//

//
// Configuration parameters for the certificate chain engine
//
//      hRestrictedRoot - restrict the root store (must be a subset of 'Root')
//
//      hRestrictedTrust - restrict the store for CTLs
//
//      hRestrictedOther - restrict the store for certs and CRLs
//
//      cAdditionalStore, rghAdditionalStore - additional stores
//
//      NOTE: The algorithm used to define the stores for the engine is as
//            follows:
//
//            hRoot = hRestrictedRoot or System Store 'Root'
//
//            hTrust = hRestrictedTrust or hWorld (defined later)
//
//            hOther = hRestrictedOther or (hRestrictedTrust == NULL) ? hWorld :
//                     hRestrictedTrust + hWorld
//
//            hWorld = hRoot + 'CA' + 'My' + 'Trust' + rghAdditionalStore
//
//      dwFlags  - flags
//
//          CERT_CHAIN_CACHE_END_CERT - information will be cached on
//                                      the end cert as well as the other
//                                      certs in the chain
//
//          CERT_CHAIN_THREAD_STORE_SYNC - use separate thread for store syncs
//                                         and related cache updates
//
//          CERT_CHAIN_CACHE_ONLY_URL_RETRIEVAL - don't hit the wire to get
//                                                URL based objects
//
//      dwUrlRetrievalTimeout - timeout for wire based URL object retrievals
//                              (milliseconds)
//

  {$EXTERNALSYM CERT_CHAIN_CACHE_END_CERT}
  CERT_CHAIN_CACHE_END_CERT             = $00000001;
  {$EXTERNALSYM CERT_CHAIN_THREAD_STORE_SYNC}
  CERT_CHAIN_THREAD_STORE_SYNC          = $00000002;
  {$EXTERNALSYM CERT_CHAIN_CACHE_ONLY_URL_RETRIEVAL}
  CERT_CHAIN_CACHE_ONLY_URL_RETRIEVAL   = $00000004;
  {$EXTERNALSYM CERT_CHAIN_USE_LOCAL_MACHINE_STORE}
  CERT_CHAIN_USE_LOCAL_MACHINE_STORE    = $00000008;
  {$EXTERNALSYM CERT_CHAIN_ENABLE_CACHE_AUTO_UPDATE}
  CERT_CHAIN_ENABLE_CACHE_AUTO_UPDATE   = $00000010;
  {$EXTERNALSYM CERT_CHAIN_ENABLE_SHARE_STORE}
  CERT_CHAIN_ENABLE_SHARE_STORE         = $00000020;

type  
  PCertChainEngineConfig = ^TCertChainEngineConfig;
  {$EXTERNALSYM _CERT_CHAIN_ENGINE_CONFIG} 
  _CERT_CHAIN_ENGINE_CONFIG = record 
    cbSize: DWORD; 
    hRestrictedRoot: HCERTSTORE; 
    hRestrictedTrust: HCERTSTORE; 
    hRestrictedOther: HCERTSTORE; 
    cAdditionalStore: DWORD; 
    rghAdditionalStore: ^HCERTSTORE; 
    dwFlags: DWORD; 
    dwUrlRetrievalTimeout: DWORD;         // milliseconds 
    MaximumCachedCertificates: DWORD; 
    CycleDetectionModulus: DWORD; 
  end; 
  {$EXTERNALSYM CERT_CHAIN_ENGINE_CONFIG} 
  CERT_CHAIN_ENGINE_CONFIG = _CERT_CHAIN_ENGINE_CONFIG; 
  {$EXTERNALSYM PCERT_CHAIN_ENGINE_CONFIG} 
  PCERT_CHAIN_ENGINE_CONFIG = ^_CERT_CHAIN_ENGINE_CONFIG; 
  TCertChainEngineConfig = _CERT_CHAIN_ENGINE_CONFIG; 

{$EXTERNALSYM CertCreateCertificateChainEngine}
function CertCreateCertificateChainEngine(var pConfig: CERT_CHAIN_ENGINE_CONFIG;
  out phChainEngine: HCERTCHAINENGINE): BOOL; stdcall;

//
// Free a certificate trust engine
//

{$EXTERNALSYM CertFreeCertificateChainEngine}
procedure CertFreeCertificateChainEngine(
  hChainEngine: HCERTCHAINENGINE); stdcall;

//
// Resync the certificate chain engine.  This resync's the stores backing
// the engine and updates the engine caches.
//

{$EXTERNALSYM CertResyncCertificateChainEngine}
function CertResyncCertificateChainEngine(
  hChainEngine: HCERTCHAINENGINE): BOOL; stdcall;

//
// When an application requests a certificate chain, the data structure
// returned is in the form of a CERT_CHAIN_CONTEXT.  This contains
// an array of CERT_SIMPLE_CHAIN where each simple chain goes from
// an end cert to a self signed cert and the chain context connects simple
// chains via trust lists.  Each simple chain contains the chain of
// certificates, summary trust information about the chain and trust information
// about each certificate element in the chain.
//

//
// Trust status bits
//
type
  PCertTrustStatus = ^TCertTrustStatus;
  {$EXTERNALSYM _CERT_TRUST_STATUS} 
  _CERT_TRUST_STATUS = record 
    dwErrorStatus: DWORD; 
    dwInfoStatus: DWORD; 
  end; 
  {$EXTERNALSYM CERT_TRUST_STATUS} 
  CERT_TRUST_STATUS = _CERT_TRUST_STATUS; 
  {$EXTERNALSYM PCERT_TRUST_STATUS} 
  PCERT_TRUST_STATUS = ^_CERT_TRUST_STATUS; 
  TCertTrustStatus = _CERT_TRUST_STATUS; 

//
// The following are error status bits
//

// These can be applied to certificates and chains
const
  {$EXTERNALSYM CERT_TRUST_NO_ERROR}
  CERT_TRUST_NO_ERROR                           = $00000000;
  {$EXTERNALSYM CERT_TRUST_IS_NOT_TIME_VALID}
  CERT_TRUST_IS_NOT_TIME_VALID                  = $00000001;
  {$EXTERNALSYM CERT_TRUST_IS_NOT_TIME_NESTED}
  CERT_TRUST_IS_NOT_TIME_NESTED                 = $00000002;
  {$EXTERNALSYM CERT_TRUST_IS_REVOKED}
  CERT_TRUST_IS_REVOKED                         = $00000004;
  {$EXTERNALSYM CERT_TRUST_IS_NOT_SIGNATURE_VALID}
  CERT_TRUST_IS_NOT_SIGNATURE_VALID             = $00000008;
  {$EXTERNALSYM CERT_TRUST_IS_NOT_VALID_FOR_USAGE}
  CERT_TRUST_IS_NOT_VALID_FOR_USAGE             = $00000010;
  {$EXTERNALSYM CERT_TRUST_IS_UNTRUSTED_ROOT}
  CERT_TRUST_IS_UNTRUSTED_ROOT                  = $00000020;
  {$EXTERNALSYM CERT_TRUST_REVOCATION_STATUS_UNKNOWN}
  CERT_TRUST_REVOCATION_STATUS_UNKNOWN          = $00000040;
  {$EXTERNALSYM CERT_TRUST_IS_CYCLIC}
  CERT_TRUST_IS_CYCLIC                          = $00000080;

  {$EXTERNALSYM CERT_TRUST_INVALID_EXTENSION}
  CERT_TRUST_INVALID_EXTENSION                  = $00000100;
  {$EXTERNALSYM CERT_TRUST_INVALID_POLICY_CONSTRAINTS}
  CERT_TRUST_INVALID_POLICY_CONSTRAINTS         = $00000200;
  {$EXTERNALSYM CERT_TRUST_INVALID_BASIC_CONSTRAINTS}
  CERT_TRUST_INVALID_BASIC_CONSTRAINTS          = $00000400;
  {$EXTERNALSYM CERT_TRUST_INVALID_NAME_CONSTRAINTS}
  CERT_TRUST_INVALID_NAME_CONSTRAINTS           = $00000800;
  {$EXTERNALSYM CERT_TRUST_HAS_NOT_SUPPORTED_NAME_CONSTRAINT}
  CERT_TRUST_HAS_NOT_SUPPORTED_NAME_CONSTRAINT  = $00001000;

// In LH, this error will never be set.
  {$EXTERNALSYM CERT_TRUST_HAS_NOT_DEFINED_NAME_CONSTRAINT}
  CERT_TRUST_HAS_NOT_DEFINED_NAME_CONSTRAINT    = $00002000;

  {$EXTERNALSYM CERT_TRUST_HAS_NOT_PERMITTED_NAME_CONSTRAINT}
  CERT_TRUST_HAS_NOT_PERMITTED_NAME_CONSTRAINT  = $00004000;
  {$EXTERNALSYM CERT_TRUST_HAS_EXCLUDED_NAME_CONSTRAINT}
  CERT_TRUST_HAS_EXCLUDED_NAME_CONSTRAINT       = $00008000;

  {$EXTERNALSYM CERT_TRUST_IS_OFFLINE_REVOCATION}
  CERT_TRUST_IS_OFFLINE_REVOCATION              = $01000000;
  {$EXTERNALSYM CERT_TRUST_NO_ISSUANCE_CHAIN_POLICY}
  CERT_TRUST_NO_ISSUANCE_CHAIN_POLICY           = $02000000;
  {$EXTERNALSYM CERT_TRUST_IS_EXPLICIT_DISTRUST}
  CERT_TRUST_IS_EXPLICIT_DISTRUST               = $04000000;
  {$EXTERNALSYM CERT_TRUST_HAS_NOT_SUPPORTED_CRITICAL_EXT}
  CERT_TRUST_HAS_NOT_SUPPORTED_CRITICAL_EXT     = $08000000;


// These can be applied to chains only

  {$EXTERNALSYM CERT_TRUST_IS_PARTIAL_CHAIN}
  CERT_TRUST_IS_PARTIAL_CHAIN           = $00010000;
  {$EXTERNALSYM CERT_TRUST_CTL_IS_NOT_TIME_VALID}
  CERT_TRUST_CTL_IS_NOT_TIME_VALID      = $00020000;
  {$EXTERNALSYM CERT_TRUST_CTL_IS_NOT_SIGNATURE_VALID}
  CERT_TRUST_CTL_IS_NOT_SIGNATURE_VALID = $00040000;
  {$EXTERNALSYM CERT_TRUST_CTL_IS_NOT_VALID_FOR_USAGE}
  CERT_TRUST_CTL_IS_NOT_VALID_FOR_USAGE = $00080000;

//
// The following are info status bits
//

// These can be applied to certificates only

  {$EXTERNALSYM CERT_TRUST_HAS_EXACT_MATCH_ISSUER}
  CERT_TRUST_HAS_EXACT_MATCH_ISSUER     = $00000001;
  {$EXTERNALSYM CERT_TRUST_HAS_KEY_MATCH_ISSUER}
  CERT_TRUST_HAS_KEY_MATCH_ISSUER       = $00000002;
  {$EXTERNALSYM CERT_TRUST_HAS_NAME_MATCH_ISSUER}
  CERT_TRUST_HAS_NAME_MATCH_ISSUER      = $00000004;
  {$EXTERNALSYM CERT_TRUST_IS_SELF_SIGNED}
  CERT_TRUST_IS_SELF_SIGNED             = $00000008;

// These can be applied to certificates and chains

  {$EXTERNALSYM CERT_TRUST_HAS_PREFERRED_ISSUER}
  CERT_TRUST_HAS_PREFERRED_ISSUER       = $00000100;
  {$EXTERNALSYM CERT_TRUST_HAS_ISSUANCE_CHAIN_POLICY}
  CERT_TRUST_HAS_ISSUANCE_CHAIN_POLICY  = $00000200;
  {$EXTERNALSYM CERT_TRUST_HAS_VALID_NAME_CONSTRAINTS}
  CERT_TRUST_HAS_VALID_NAME_CONSTRAINTS = $00000400;
  {$EXTERNALSYM CERT_TRUST_IS_PEER_TRUSTED}
  CERT_TRUST_IS_PEER_TRUSTED            = $00000800;
  {$EXTERNALSYM CERT_TRUST_HAS_CRL_VALIDITY_EXTENDED}
  CERT_TRUST_HAS_CRL_VALIDITY_EXTENDED  = $00001000;

// These can be applied to chains only

  {$EXTERNALSYM CERT_TRUST_IS_COMPLEX_CHAIN}
  CERT_TRUST_IS_COMPLEX_CHAIN           = $00010000;

//
// Each certificate context in a simple chain has a corresponding chain element
// in the simple chain context
//
// dwErrorStatus has CERT_TRUST_IS_REVOKED, pRevocationInfo set
// dwErrorStatus has CERT_TRUST_REVOCATION_STATUS_UNKNOWN, pRevocationInfo set

//
//         Note that the post processing revocation supported in the first
//         version only sets cbSize and dwRevocationResult.  Everything else
//         is NULL
//

//
// Revocation Information
//
type
  PCertRevocationInfo = ^TCertRevocationInfo;
  {$EXTERNALSYM _CERT_REVOCATION_INFO} 
  _CERT_REVOCATION_INFO = record 
    cbSize: DWORD; 
    dwRevocationResult: DWORD; 
    pszRevocationOid: LPCSTR; 
    pvOidSpecificInfo: Pointer; 
                                          // fHasFreshnessTime is only set if we are able to retrieve revocation 
                                          // information. For a CRL its CurrentTime - ThisUpdate. 
    fHasFreshnessTime: BOOL; 
    dwFreshnessTime: DWORD;               // seconds 
                                          // NonNULL for CRL base revocation checking 
    pCrlInfo: PCERT_REVOCATION_CRL_INFO; 
  end; 
  {$EXTERNALSYM CERT_REVOCATION_INFO} 
  CERT_REVOCATION_INFO = _CERT_REVOCATION_INFO; 
  {$EXTERNALSYM PCERT_REVOCATION_INFO} 
  PCERT_REVOCATION_INFO = ^_CERT_REVOCATION_INFO; 
  TCertRevocationInfo = _CERT_REVOCATION_INFO; 

//
// Trust List Information
//
  PCertTrustListInfo = ^TCertTrustListInfo;
  {$EXTERNALSYM _CERT_TRUST_LIST_INFO}
  _CERT_TRUST_LIST_INFO = record
    cbSize: DWORD;
    pCtlEntry: PCTL_ENTRY;
    pCtlContext: PCCTL_CONTEXT;
  end;
  {$EXTERNALSYM CERT_TRUST_LIST_INFO}
  CERT_TRUST_LIST_INFO = _CERT_TRUST_LIST_INFO;
  {$EXTERNALSYM PCERT_TRUST_LIST_INFO}
  PCERT_TRUST_LIST_INFO = ^_CERT_TRUST_LIST_INFO;
  TCertTrustListInfo = _CERT_TRUST_LIST_INFO;

//
// Chain Element
//
  PCertChainElement = ^TCertChainElement;
  {$EXTERNALSYM _CERT_CHAIN_ELEMENT}
  _CERT_CHAIN_ELEMENT = record
    cbSize: DWORD;
    pCertContext: PCCERT_CONTEXT;
    TrustStatus: CERT_TRUST_STATUS;
    pRevocationInfo: PCERT_REVOCATION_INFO;
    pIssuanceUsage: PCERT_ENHKEY_USAGE;     // If NULL, any
    pApplicationUsage: PCERT_ENHKEY_USAGE;  // If NULL, any
    pwszExtendedErrorInfo: LPCWSTR;         // If NULL, none
  end;
  {$EXTERNALSYM CERT_CHAIN_ELEMENT}
  CERT_CHAIN_ELEMENT = _CERT_CHAIN_ELEMENT;
  {$EXTERNALSYM PCERT_CHAIN_ELEMENT}
  PCERT_CHAIN_ELEMENT = ^_CERT_CHAIN_ELEMENT;
  TCertChainElement = _CERT_CHAIN_ELEMENT;

//
// The simple chain is an array of chain elements and a summary trust status
// for the chain
//
// rgpElements[0] is the end certificate chain element
//
// rgpElements[cElement-1] is the self-signed 'root' certificate chain element
//

  PCertSimpleChain = ^TCertSimpleChain;
  {$EXTERNALSYM _CERT_SIMPLE_CHAIN}
  _CERT_SIMPLE_CHAIN = record
    cbSize: DWORD;
    TrustStatus: CERT_TRUST_STATUS;
    cElement: DWORD;
    rgpElement: ^PCERT_CHAIN_ELEMENT;
    pTrustListInfo: PCERT_TRUST_LIST_INFO;
                                          // fHasRevocationFreshnessTime is only set if we are able to retrieve
                                          // revocation information for all elements checked for revocation.
                                          // For a CRL its CurrentTime - ThisUpdate.
                                          //
                                          // dwRevocationFreshnessTime is the largest time across all elements
                                          // checked.
    fHasRevocationFreshnessTime: BOOL;
    dwRevocationFreshnessTime: DWORD;     // seconds
  end;
  {$EXTERNALSYM CERT_SIMPLE_CHAIN}
  CERT_SIMPLE_CHAIN = _CERT_SIMPLE_CHAIN;
  {$EXTERNALSYM PCERT_SIMPLE_CHAIN}
  PCERT_SIMPLE_CHAIN = ^_CERT_SIMPLE_CHAIN;
  TCertSimpleChain = _CERT_SIMPLE_CHAIN;

//
// And the chain context contains an array of simple chains and summary trust
// status for all the connected simple chains
//
// rgpChains[0] is the end certificate simple chain
//
// rgpChains[cChain-1] is the final (possibly trust list signer) chain which
// ends in a certificate which is contained in the root store
//

  {$EXTERNALSYM PCCERT_CHAIN_CONTEXT}
  PCCERT_CHAIN_CONTEXT = ^CERT_CHAIN_CONTEXT;
  PCertChainContext = ^TCertChainContext;
  {$EXTERNALSYM _CERT_CHAIN_CONTEXT}
  _CERT_CHAIN_CONTEXT = record
    cbSize: DWORD;
    TrustStatus: CERT_TRUST_STATUS;
    cChain: DWORD;
    rgpChain: ^PCERT_SIMPLE_CHAIN;

    // Following is returned when CERT_CHAIN_RETURN_LOWER_QUALITY_CONTEXTS
    // is set in dwFlags
    cLowerQualityChainContext: DWORD;
    rgpLowerQualityChainContext: ^PCCERT_CHAIN_CONTEXT;

    // fHasRevocationFreshnessTime is only set if we are able to retrieve
    // revocation information for all elements checked for revocation.
    // For a CRL its CurrentTime - ThisUpdate.
    //
    // dwRevocationFreshnessTime is the largest time across all elements
    // checked.
    fHasRevocationFreshnessTime: BOOL;
    dwRevocationFreshnessTime: DWORD;     // seconds

    // Flags passed when created via CertGetCertificateChain
    dwCreateFlags: DWORD;

    // Following is updated with unique Id when the chain context is logged.
    ChainId: TGUID;
  end;
  {$EXTERNALSYM CERT_CHAIN_CONTEXT}
  CERT_CHAIN_CONTEXT = _CERT_CHAIN_CONTEXT;
  {$EXTERNALSYM PCERT_CHAIN_CONTEXT}
  PCERT_CHAIN_CONTEXT = ^_CERT_CHAIN_CONTEXT;
  TCertChainContext = _CERT_CHAIN_CONTEXT;

//
// When building a chain, the there are various parameters used for finding
// issuing certificates and trust lists.  They are identified in the
// following structure
//

const
// Default usage match type is AND with value zero
  {$EXTERNALSYM USAGE_MATCH_TYPE_AND}
  USAGE_MATCH_TYPE_AND          = $00000000;
  {$EXTERNALSYM USAGE_MATCH_TYPE_OR}
  USAGE_MATCH_TYPE_OR           = $00000001;

type
  PCertUsageMatch = ^TCertUsageMatch; 
  {$EXTERNALSYM _CERT_USAGE_MATCH} 
  _CERT_USAGE_MATCH = record 
    dwType: DWORD; 
    Usage: CERT_ENHKEY_USAGE; 
  end; 
  {$EXTERNALSYM CERT_USAGE_MATCH} 
  CERT_USAGE_MATCH = _CERT_USAGE_MATCH; 
  {$EXTERNALSYM PCERT_USAGE_MATCH} 
  PCERT_USAGE_MATCH = ^_CERT_USAGE_MATCH; 
  TCertUsageMatch = _CERT_USAGE_MATCH; 

  PCtlUsageMatch = ^TCtlUsageMatch;
  {$EXTERNALSYM _CTL_USAGE_MATCH} 
  _CTL_USAGE_MATCH = record 
    dwType: DWORD; 
    Usage: CTL_USAGE; 
  end; 
  {$EXTERNALSYM CTL_USAGE_MATCH} 
  CTL_USAGE_MATCH = _CTL_USAGE_MATCH; 
  {$EXTERNALSYM PCTL_USAGE_MATCH} 
  PCTL_USAGE_MATCH = ^_CTL_USAGE_MATCH; 
  TCtlUsageMatch = _CTL_USAGE_MATCH; 

  PCertChainPara = ^TCertChainPara;
  {$EXTERNALSYM _CERT_CHAIN_PARA} 
  _CERT_CHAIN_PARA = record 
    cbSize: DWORD; 
    RequestedUsage: CERT_USAGE_MATCH;

    //#ifdef CERT_CHAIN_PARA_HAS_EXTRA_FIELDS
    // Note, if you #define CERT_CHAIN_PARA_HAS_EXTRA_FIELDS, then, you
    // must zero all unused fields in this data structure.
    // More fields could be added in a future release.
    RequestedIssuancePolicy: CERT_USAGE_MATCH;
    dwUrlRetrievalTimeout: DWORD;         // milliseconds
    fCheckRevocationFreshnessTime: BOOL;
    dwRevocationFreshnessTime: DWORD;     // seconds

    // If nonNULL, any cached information before this time is considered
    // time invalid and forces a wire retrieval. When set overrides
    // the registry configuration CacheResync time.
    pftCacheResync: PFileTime; 
    //#endif 
  end; 
  {$EXTERNALSYM CERT_CHAIN_PARA} 
  CERT_CHAIN_PARA = _CERT_CHAIN_PARA; 
  {$EXTERNALSYM PCERT_CHAIN_PARA} 
  PCERT_CHAIN_PARA = ^_CERT_CHAIN_PARA;
  TCertChainPara = _CERT_CHAIN_PARA; 

//
// The following API is used for retrieving certificate chains
//
// Parameters:
//
//      hChainEngine     - the chain engine (namespace and cache) to use, NULL
//                         mean use the default chain engine
//
//      pCertContext     - the context we are retrieving the chain for, it
//                         will be the zero index element in the chain
//
//      pTime            - the point in time that we want the chain validated
//                         for.  Note that the time does not affect trust list,
//                         revocation, or root store checking.  NULL means use
//                         the current system time
//
//      hAdditionalStore - additional store to use when looking up objects
//
//      pChainPara       - parameters for chain building
//
//      dwFlags          - flags such as should revocation checking be done
//                         on the chain?
//
//      pvReserved       - reserved parameter, must be NULL
//
//      ppChainContext   - chain context returned
//

const
  // CERT_CHAIN_CACHE_END_CERT can be used here as well
  // Revocation flags are in the high nibble
  {$EXTERNALSYM CERT_CHAIN_REVOCATION_CHECK_END_CERT}
  CERT_CHAIN_REVOCATION_CHECK_END_CERT          = $10000000;
  {$EXTERNALSYM CERT_CHAIN_REVOCATION_CHECK_CHAIN}
  CERT_CHAIN_REVOCATION_CHECK_CHAIN             = $20000000;
  {$EXTERNALSYM CERT_CHAIN_REVOCATION_CHECK_CHAIN_EXCLUDE_ROOT}
  CERT_CHAIN_REVOCATION_CHECK_CHAIN_EXCLUDE_ROOT = $40000000;
  {$EXTERNALSYM CERT_CHAIN_REVOCATION_CHECK_CACHE_ONLY}
  CERT_CHAIN_REVOCATION_CHECK_CACHE_ONLY        = $80000000;

  // By default, the dwUrlRetrievalTimeout in pChainPara is the timeout used
  // for each revocation URL wire retrieval. When the following flag is set,
  // dwUrlRetrievalTimeout is the accumulative timeout across all
  // revocation URL wire retrievals.
  {$EXTERNALSYM CERT_CHAIN_REVOCATION_ACCUMULATIVE_TIMEOUT}
  CERT_CHAIN_REVOCATION_ACCUMULATIVE_TIMEOUT    = $08000000;

  // First pass determines highest quality based upon:
  //  - Chain signature valid (higest quality bit of this set)
  //  - Complete chain
  //  - Trusted root          (lowestest quality bit of this set)
  // By default, second pass only considers paths >= highest first pass quality
  {$EXTERNALSYM CERT_CHAIN_DISABLE_PASS1_QUALITY_FILTERING}
  CERT_CHAIN_DISABLE_PASS1_QUALITY_FILTERING    = $00000040;

  {$EXTERNALSYM CERT_CHAIN_RETURN_LOWER_QUALITY_CONTEXTS}
  CERT_CHAIN_RETURN_LOWER_QUALITY_CONTEXTS      = $00000080;

  {$EXTERNALSYM CERT_CHAIN_DISABLE_AUTH_ROOT_AUTO_UPDATE}
  CERT_CHAIN_DISABLE_AUTH_ROOT_AUTO_UPDATE      = $00000100;

  // When this flag is set, pTime will be used as the timestamp time.
  // pTime will be used to determine if the end certificate was valid at this
  // time. Revocation checking will be relative to pTime.
  // In addition, current time will also be used
  // to determine if the certificate is still time valid. All remaining
  // CA and root certificates will be checked using current time and not pTime.
  //
  // This flag was added 4/5/01 in WXP.
  {$EXTERNALSYM CERT_CHAIN_TIMESTAMP_TIME}
  CERT_CHAIN_TIMESTAMP_TIME = $00000200;

  // When this flag is set, 'My' certificates having a private key or end
  // entity certificates in the 'TrustedPeople' store are trusted without
  // doing any chain building. Neither the CERT_TRUST_IS_PARTIAL_CHAIN or
  // CERT_TRUST_IS_UNTRUSTED_ROOT dwErrorStatus bits will be set for
  // such certificates.
  //
  // This flag was added 6/9/03 in LH.
  {$EXTERNALSYM CERT_CHAIN_ENABLE_PEER_TRUST}
  CERT_CHAIN_ENABLE_PEER_TRUST = $00000400;

  // When this flag is set, 'My' certificates aren't considered for
  // PEER_TRUST.
  //
  // This flag was added 11/12/04 in LH.
  //
  // On 8-05-05 changed to never consider 'My' certificates for PEER_TRUST.
  {$EXTERNALSYM CERT_CHAIN_DISABLE_MY_PEER_TRUST}
  CERT_CHAIN_DISABLE_MY_PEER_TRUST = $00000800;

{$EXTERNALSYM CertGetCertificateChain}
function CertGetCertificateChain(hChainEngine: HCERTCHAINENGINE;
  pCertContext: PCCERT_CONTEXT; var pTime: TFileTime;
  hAdditionalStore: HCERTSTORE; var pChainPara: CERT_CHAIN_PARA;
  dwFlags: DWORD; pvReserved: Pointer;
  out ppChainContext: PCCERT_CHAIN_CONTEXT): BOOL; stdcall;

//
// Free a certificate chain
//

{$EXTERNALSYM CertFreeCertificateChain}
procedure CertFreeCertificateChain(pChainContext: PCCERT_CHAIN_CONTEXT); stdcall;

//
// Duplicate (add a reference to) a certificate chain
//

{$EXTERNALSYM CertDuplicateCertificateChain}
function CertDuplicateCertificateChain(
  pChainContext: PCCERT_CHAIN_CONTEXT): PCCERT_CHAIN_CONTEXT; stdcall;

const
  //
  // Specific Revocation Type OID and structure definitions
  //

  //
  // CRL Revocation OID
  //
  {$EXTERNALSYM REVOCATION_OID_CRL_REVOCATION}
  REVOCATION_OID_CRL_REVOCATION = LPCSTR(1);

//
// For the CRL revocation OID the pvRevocationPara is NULL
//

//
// CRL Revocation Info
//

type
  PCrlRevocationInfo = ^TCrlRevocationInfo; 
  {$EXTERNALSYM _CRL_REVOCATION_INFO} 
  _CRL_REVOCATION_INFO = record 
    pCrlEntry: PCRL_ENTRY; 
    pCrlContext: PCCRL_CONTEXT; 
    pCrlIssuerChain: PCCERT_CHAIN_CONTEXT; 
  end; 
  {$EXTERNALSYM CRL_REVOCATION_INFO} 
  CRL_REVOCATION_INFO = _CRL_REVOCATION_INFO; 
  {$EXTERNALSYM PCRL_REVOCATION_INFO} 
  PCRL_REVOCATION_INFO = ^_CRL_REVOCATION_INFO; 
  TCrlRevocationInfo = _CRL_REVOCATION_INFO; 

//+-------------------------------------------------------------------------
//  Find the first or next certificate chain context in the store.
//
//  The chain context is found according to the dwFindFlags, dwFindType and
//  its pvFindPara. See below for a list of the find types and its parameters.
//
//  If the first or next chain context isn't found, NULL is returned.
//  Otherwise, a pointer to a read only CERT_CHAIN_CONTEXT is returned.
//  CERT_CHAIN_CONTEXT must be freed by calling CertFreeCertificateChain
//  or is freed when passed as the
//  pPrevChainContext on a subsequent call. CertDuplicateCertificateChain
//  can be called to make a duplicate.
//
//  pPrevChainContext MUST BE NULL on the first
//  call to find the chain context. To find the next chain context, the
//  pPrevChainContext is set to the CERT_CHAIN_CONTEXT returned by a previous
//  call.
//
//  NOTE: a NON-NULL pPrevChainContext is always CertFreeCertificateChain'ed by
//  this function, even for an error.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertFindChainInStore}
function CertFindChainInStore(hCertStore: HCERTSTORE;
  dwCertEncodingType, dwFindFlags, dwFindType: DWORD; pvFindPara: Pointer;
  pPrevChainContext: PCCERT_CHAIN_CONTEXT): PCCERT_CHAIN_CONTEXT; stdcall;

const
  {$EXTERNALSYM CERT_CHAIN_FIND_BY_ISSUER}
  CERT_CHAIN_FIND_BY_ISSUER = 1;

//+-------------------------------------------------------------------------
//  CERT_CHAIN_FIND_BY_ISSUER
//
//  Find a certificate chain having a private key for the end certificate and
//  matching one of the given issuer names. A matching dwKeySpec and
//  enhanced key usage can also be specified. Additionally a callback can
//  be provided for even more caller provided filtering before building the
//  chain.
//
//  By default, only the issuers in the first simple chain are compared
//  for a name match. CERT_CHAIN_FIND_BY_ISSUER_COMPLEX_CHAIN_FLAG can
//  be set in dwFindFlags to match issuers in all the simple chains.
//
//  CERT_CHAIN_FIND_BY_ISSUER_NO_KEY_FLAG can be set in dwFindFlags to
//  not check if the end certificate has a private key.
//
//  CERT_CHAIN_FIND_BY_ISSUER_COMPARE_KEY_FLAG can be set in dwFindFlags
//  to compare the public key in the end certificate with the crypto
//  provider's public key. The dwAcquirePrivateKeyFlags can be set
//  in CERT_CHAIN_FIND_BY_ISSUER_PARA to enable caching of the private key's
//  HKEY returned by the CSP.
//
//  If dwCertEncodingType == 0, defaults to X509_ASN_ENCODING for the
//  array of encoded issuer names.
//
//  By default, the hCertStore passed to CertFindChainInStore, is passed
//  as an additional store to CertGetCertificateChain.
//  CERT_CHAIN_FIND_BY_ISSUER_CACHE_ONLY_FLAG can be set in dwFindFlags
//  to improve performance by only searching the cached system stores
//  (root, my, ca, trust) to find the issuer certificates. If you are doing
//  a find in the 'my' system store, than, this flag should be set to
//  improve performance.
//
//  Setting CERT_CHAIN_FIND_BY_ISSUER_LOCAL_MACHINE_FLAG in dwFindFlags
//  restricts CertGetCertificateChain to search the Local Machine
//  cached system stores instead of the Current User's.
//
//  Setting CERT_CHAIN_FIND_BY_ISSUER_CACHE_ONLY_URL_FLAG in dwFindFlags
//  restricts CertGetCertificateChain to only search the URL cache
//  and not hit the wire.
//--------------------------------------------------------------------------

// Returns FALSE to skip this certificate. Otherwise, returns TRUE to
// build a chain for this certificate.
type
  PFN_CERT_CHAIN_FIND_BY_ISSUER_CALLBACK = function(pCert: PCCERT_CONTEXT;
    pvFindArg: Pointer): BOOL; stdcall;
  TFnCertChainFindByIssuerCallback = PFN_CERT_CHAIN_FIND_BY_ISSUER_CALLBACK;

  PCertChainFindIssuerPara = ^TCertChainFindIssuerPara; 
  {$EXTERNALSYM _CERT_CHAIN_FIND_BY_ISSUER_PARA} 
  _CERT_CHAIN_FIND_BY_ISSUER_PARA = record 
    cbSize: DWORD; 
    // If pszUsageIdentifier == NULL, matches any usage. 
    pszUsageIdentifier: LPCSTR; 
    // If dwKeySpec == 0, matches any KeySpec 
    dwKeySpec: DWORD; 
    // When CERT_CHAIN_FIND_BY_ISSUER_COMPARE_KEY_FLAG is set in dwFindFlags, 
    // CryptAcquireCertificatePrivateKey is called to do the public key 
    // comparison. The following flags can be set to enable caching 
    // of the acquired private key or suppress CSP UI. See the API for more 
    // details on these flags. 
    dwAcquirePrivateKeyFlags: DWORD; 
    // Pointer to an array of X509, ASN.1 encoded issuer name blobs. If 
    // cIssuer == 0, matches any issuer 
    cIssuer: DWORD; 
    rgIssuer: ^CERT_NAME_BLOB; 
    // If NULL or Callback returns TRUE, builds the chain for the end 
    // certificate having a private key with the specified KeySpec and 
    // enhanced key usage. 
    pfnFindCallback: PFN_CERT_CHAIN_FIND_BY_ISSUER_CALLBACK; 
    pvFindArg: Pointer; 
    //#ifdef CERT_CHAIN_FIND_BY_ISSUER_PARA_HAS_EXTRA_FIELDS 
    // Note, if you #define CERT_CHAIN_FIND_BY_ISSUER_PARA_HAS_EXTRA_FIELDS, 
    // then, you must zero all unused fields in this data structure. 
    // More fields could be added in a future release. 
    // If the following pointers are nonNull, returns the index of the 
    // matching issuer certificate, which is at: 
    // pChainContext-> 
    //      rgpChain[*pdwIssuerChainIndex]->rgpElement[*pdwIssuerElementIndex]. 
    // 
    // The issuer name blob is compared against the Issuer field in the 
    // certificate. The *pdwIssuerElementIndex is set to the index of this 
    // subject certificate + 1. Therefore, its possible for a partial chain or 
    // a self signed certificate matching the name blob, where 
    // *pdwIssuerElementIndex points past the last certificate in the chain. 
    // 
    // Note, not updated if the above cIssuer == 0. 
    pdwIssuerChainIndex: ^DWORD; 
    pdwIssuerElementIndex: ^DWORD; 
    //#endif 
  end; 
  {$EXTERNALSYM CERT_CHAIN_FIND_ISSUER_PARA} 
  CERT_CHAIN_FIND_ISSUER_PARA = _CERT_CHAIN_FIND_BY_ISSUER_PARA; 
  {$EXTERNALSYM PCERT_CHAIN_FIND_ISSUER_PARA} 
  PCERT_CHAIN_FIND_ISSUER_PARA = ^_CERT_CHAIN_FIND_BY_ISSUER_PARA; 
  {$EXTERNALSYM CERT_CHAIN_FIND_BY_ISSUER_PARA} 
  CERT_CHAIN_FIND_BY_ISSUER_PARA = _CERT_CHAIN_FIND_BY_ISSUER_PARA; 
  {$EXTERNALSYM PCERT_CHAIN_FIND_BY_ISSUER_PARA} 
  PCERT_CHAIN_FIND_BY_ISSUER_PARA = ^_CERT_CHAIN_FIND_BY_ISSUER_PARA; 
  TCertChainFindIssuerPara = _CERT_CHAIN_FIND_BY_ISSUER_PARA; 

const
  // The following dwFindFlags can be set for CERT_CHAIN_FIND_BY_ISSUER

  // If set, compares the public key in the end certificate with the crypto
  // provider's public key. This comparison is the last check made on the
  // build chain.
  {$EXTERNALSYM CERT_CHAIN_FIND_BY_ISSUER_COMPARE_KEY_FLAG}
  CERT_CHAIN_FIND_BY_ISSUER_COMPARE_KEY_FLAG = $0001;

  // If not set, only checks the first simple chain for an issuer name match.
  // When set, also checks second and subsequent simple chains.
  {$EXTERNALSYM CERT_CHAIN_FIND_BY_ISSUER_COMPLEX_CHAIN_FLAG}
  CERT_CHAIN_FIND_BY_ISSUER_COMPLEX_CHAIN_FLAG = $0002;

  // If set, CertGetCertificateChain only searches the URL cache and
  // doesn't hit the wire.
  {$EXTERNALSYM CERT_CHAIN_FIND_BY_ISSUER_CACHE_ONLY_URL_FLAG}
  CERT_CHAIN_FIND_BY_ISSUER_CACHE_ONLY_URL_FLAG = $0004;

  // If set, CertGetCertificateChain only opens the Local Machine
  // certificate stores instead of the Current User's.
  {$EXTERNALSYM CERT_CHAIN_FIND_BY_ISSUER_LOCAL_MACHINE_FLAG}
  CERT_CHAIN_FIND_BY_ISSUER_LOCAL_MACHINE_FLAG = $0008;

  // If set, no check is made to see if the end certificate has a private
  // key associated with it.
  {$EXTERNALSYM CERT_CHAIN_FIND_BY_ISSUER_NO_KEY_FLAG}
  CERT_CHAIN_FIND_BY_ISSUER_NO_KEY_FLAG = $4000;

  // By default, the hCertStore passed to CertFindChainInStore, is passed
  // as the additional store to CertGetCertificateChain. This flag can be
  // set to improve performance by only searching the cached system stores
  // (root, my, ca, trust) to find the issuer certificates. If not set, then,
  // the hCertStore is always searched in addition to the cached system
  // stores.
  {$EXTERNALSYM CERT_CHAIN_FIND_BY_ISSUER_CACHE_ONLY_FLAG}
  CERT_CHAIN_FIND_BY_ISSUER_CACHE_ONLY_FLAG = $8000;

//+=========================================================================
//  Certificate Chain Policy Data Structures and APIs
//==========================================================================
type
  PCertChainPolicyPara = ^TCertChainPolicyPara;
  {$EXTERNALSYM _CERT_CHAIN_POLICY_PARA} 
  _CERT_CHAIN_POLICY_PARA = record 
    cbSize: DWORD; 
    dwFlags: DWORD; 
    pvExtraPolicyPara: Pointer;           // pszPolicyOID specific 
  end; 
  {$EXTERNALSYM CERT_CHAIN_POLICY_PARA} 
  CERT_CHAIN_POLICY_PARA = _CERT_CHAIN_POLICY_PARA; 
  {$EXTERNALSYM PCERT_CHAIN_POLICY_PARA} 
  PCERT_CHAIN_POLICY_PARA = ^_CERT_CHAIN_POLICY_PARA; 
  TCertChainPolicyPara = _CERT_CHAIN_POLICY_PARA; 

  // If both lChainIndex and lElementIndex are set to -1, the dwError applies
  // to the whole chain context. If only lElementIndex is set to -1, the
  // dwError applies to the lChainIndex'ed chain. Otherwise, the dwError applies
  // to the certificate element at
  // pChainContext->rgpChain[lChainIndex]->rgpElement[lElementIndex].

  PCertChainPolicyStatus = ^TCertChainPolicyStatus;
  {$EXTERNALSYM _CERT_CHAIN_POLICY_STATUS} 
  _CERT_CHAIN_POLICY_STATUS = record 
    cbSize: DWORD; 
    dwError: DWORD; 
    lChainIndex: Longint; 
    lElementIndex: Longint; 
    pvExtraPolicyStatus: Pointer;         // pszPolicyOID specific 
  end; 
  {$EXTERNALSYM CERT_CHAIN_POLICY_STATUS} 
  CERT_CHAIN_POLICY_STATUS = _CERT_CHAIN_POLICY_STATUS; 
  {$EXTERNALSYM PCERT_CHAIN_POLICY_STATUS} 
  PCERT_CHAIN_POLICY_STATUS = ^_CERT_CHAIN_POLICY_STATUS; 
  TCertChainPolicyStatus = _CERT_CHAIN_POLICY_STATUS; 

const
  // Common chain policy flags
  {$EXTERNALSYM CERT_CHAIN_POLICY_IGNORE_NOT_TIME_VALID_FLAG}
  CERT_CHAIN_POLICY_IGNORE_NOT_TIME_VALID_FLAG             = $00000001;
  {$EXTERNALSYM CERT_CHAIN_POLICY_IGNORE_CTL_NOT_TIME_VALID_FLAG}
  CERT_CHAIN_POLICY_IGNORE_CTL_NOT_TIME_VALID_FLAG         = $00000002;
  {$EXTERNALSYM CERT_CHAIN_POLICY_IGNORE_NOT_TIME_NESTED_FLAG}
  CERT_CHAIN_POLICY_IGNORE_NOT_TIME_NESTED_FLAG            = $00000004;
  {$EXTERNALSYM CERT_CHAIN_POLICY_IGNORE_INVALID_BASIC_CONSTRAINTS_FLAG}
  CERT_CHAIN_POLICY_IGNORE_INVALID_BASIC_CONSTRAINTS_FLAG  = $00000008;

  {$EXTERNALSYM CERT_CHAIN_POLICY_IGNORE_ALL_NOT_TIME_VALID_FLAGS}
  CERT_CHAIN_POLICY_IGNORE_ALL_NOT_TIME_VALID_FLAGS        =
    CERT_CHAIN_POLICY_IGNORE_NOT_TIME_VALID_FLAG or
    CERT_CHAIN_POLICY_IGNORE_CTL_NOT_TIME_VALID_FLAG or
    CERT_CHAIN_POLICY_IGNORE_NOT_TIME_NESTED_FLAG;

  {$EXTERNALSYM CERT_CHAIN_POLICY_ALLOW_UNKNOWN_CA_FLAG}
  CERT_CHAIN_POLICY_ALLOW_UNKNOWN_CA_FLAG                  = $00000010;
  {$EXTERNALSYM CERT_CHAIN_POLICY_IGNORE_WRONG_USAGE_FLAG}
  CERT_CHAIN_POLICY_IGNORE_WRONG_USAGE_FLAG                = $00000020;
  {$EXTERNALSYM CERT_CHAIN_POLICY_IGNORE_INVALID_NAME_FLAG}
  CERT_CHAIN_POLICY_IGNORE_INVALID_NAME_FLAG               = $00000040;
  {$EXTERNALSYM CERT_CHAIN_POLICY_IGNORE_INVALID_POLICY_FLAG}
  CERT_CHAIN_POLICY_IGNORE_INVALID_POLICY_FLAG             = $00000080;

  {$EXTERNALSYM CERT_CHAIN_POLICY_IGNORE_END_REV_UNKNOWN_FLAG}
  CERT_CHAIN_POLICY_IGNORE_END_REV_UNKNOWN_FLAG            = $00000100;
  {$EXTERNALSYM CERT_CHAIN_POLICY_IGNORE_CTL_SIGNER_REV_UNKNOWN_FLAG}
  CERT_CHAIN_POLICY_IGNORE_CTL_SIGNER_REV_UNKNOWN_FLAG      = $00000200;
  {$EXTERNALSYM CERT_CHAIN_POLICY_IGNORE_CA_REV_UNKNOWN_FLAG}
  CERT_CHAIN_POLICY_IGNORE_CA_REV_UNKNOWN_FLAG             = $00000400;
  {$EXTERNALSYM CERT_CHAIN_POLICY_IGNORE_ROOT_REV_UNKNOWN_FLAG}
  CERT_CHAIN_POLICY_IGNORE_ROOT_REV_UNKNOWN_FLAG           = $00000800;

  {$EXTERNALSYM CERT_CHAIN_POLICY_IGNORE_ALL_REV_UNKNOWN_FLAGS}
  CERT_CHAIN_POLICY_IGNORE_ALL_REV_UNKNOWN_FLAGS           =
    CERT_CHAIN_POLICY_IGNORE_END_REV_UNKNOWN_FLAG or
    CERT_CHAIN_POLICY_IGNORE_CTL_SIGNER_REV_UNKNOWN_FLAG or
    CERT_CHAIN_POLICY_IGNORE_CA_REV_UNKNOWN_FLAG or
    CERT_CHAIN_POLICY_IGNORE_ROOT_REV_UNKNOWN_FLAG;

  {$EXTERNALSYM CERT_CHAIN_POLICY_ALLOW_TESTROOT_FLAG}
  CERT_CHAIN_POLICY_ALLOW_TESTROOT_FLAG                    = $00008000;
  {$EXTERNALSYM CERT_CHAIN_POLICY_TRUST_TESTROOT_FLAG}
  CERT_CHAIN_POLICY_TRUST_TESTROOT_FLAG                    = $00004000;

  {$EXTERNALSYM CERT_CHAIN_POLICY_IGNORE_NOT_SUPPORTED_CRITICAL_EXT_FLAG}
  CERT_CHAIN_POLICY_IGNORE_NOT_SUPPORTED_CRITICAL_EXT_FLAG = $00002000;
  {$EXTERNALSYM CERT_CHAIN_POLICY_IGNORE_PEER_TRUST_FLAG}
  CERT_CHAIN_POLICY_IGNORE_PEER_TRUST_FLAG                 = $00001000;

//+-------------------------------------------------------------------------
//  Verify that the certificate chain satisfies the specified policy
//  requirements. If we were able to verify the chain policy, TRUE is returned
//  and the dwError field of the pPolicyStatus is updated. A dwError of 0
//  (ERROR_SUCCESS, S_OK) indicates the chain satisfies the specified policy.
//
//  If dwError applies to the entire chain context, both lChainIndex and
//  lElementIndex are set to -1. If dwError applies to a simple chain,
//  lElementIndex is set to -1 and lChainIndex is set to the index of the
//  first offending chain having the error. If dwError applies to a
//  certificate element, lChainIndex and lElementIndex are updated to
//  index the first offending certificate having the error, where, the
//  the certificate element is at:
//      pChainContext->rgpChain[lChainIndex]->rgpElement[lElementIndex].
//
//  The dwFlags in pPolicyPara can be set to change the default policy checking
//  behaviour. In addition, policy specific parameters can be passed in
//  the pvExtraPolicyPara field of pPolicyPara.
//
//  In addition to returning dwError, in pPolicyStatus, policy OID specific
//  extra status may be returned via pvExtraPolicyStatus.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertVerifyCertificateChainPolicy}
function CertVerifyCertificateChainPolicy(pszPolicyOID: LPCSTR;
  pChainContext: PCCERT_CHAIN_CONTEXT; var pPolicyPara: CERT_CHAIN_POLICY_PARA;
  out pPolicyStatus: CERT_CHAIN_POLICY_STATUS): BOOL; stdcall;

const
// Predefined OID Function Names
  {$EXTERNALSYM CRYPT_OID_VERIFY_CERTIFICATE_CHAIN_POLICY_FUNC}
  CRYPT_OID_VERIFY_CERTIFICATE_CHAIN_POLICY_FUNC  =
    'CertDllVerifyCertificateChainPolicy';

// CertDllVerifyCertificateChainPolicy has same function signature as
// CertVerifyCertificateChainPolicy.

//+-------------------------------------------------------------------------
//  Predefined verify chain policies
//--------------------------------------------------------------------------
  {$EXTERNALSYM CERT_CHAIN_POLICY_BASE}
  CERT_CHAIN_POLICY_BASE              = LPCSTR(1);
  {$EXTERNALSYM CERT_CHAIN_POLICY_AUTHENTICODE}
  CERT_CHAIN_POLICY_AUTHENTICODE      = LPCSTR(2);
  {$EXTERNALSYM CERT_CHAIN_POLICY_AUTHENTICODE_TS}
  CERT_CHAIN_POLICY_AUTHENTICODE_TS   = LPCSTR(3);
  {$EXTERNALSYM CERT_CHAIN_POLICY_SSL}
  CERT_CHAIN_POLICY_SSL               = LPCSTR(4);
  {$EXTERNALSYM CERT_CHAIN_POLICY_BASIC_CONSTRAINTS}
  CERT_CHAIN_POLICY_BASIC_CONSTRAINTS = LPCSTR(5);
  {$EXTERNALSYM CERT_CHAIN_POLICY_NT_AUTH}
  CERT_CHAIN_POLICY_NT_AUTH           = LPCSTR(6);
  {$EXTERNALSYM CERT_CHAIN_POLICY_MICROSOFT_ROOT}
  CERT_CHAIN_POLICY_MICROSOFT_ROOT    = LPCSTR(7);

//+-------------------------------------------------------------------------
//  CERT_CHAIN_POLICY_BASE
//
//  Implements the base chain policy verification checks. dwFlags can
//  be set in pPolicyPara to alter the default policy checking behaviour.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CERT_CHAIN_POLICY_AUTHENTICODE
//
//  Implements the Authenticode chain policy verification checks.
//
//  pvExtraPolicyPara may optionally be set to point to the following
//  AUTHENTICODE_EXTRA_CERT_CHAIN_POLICY_PARA.
//
//  pvExtraPolicyStatus may optionally be set to point to the following
//  AUTHENTICODE_EXTRA_CERT_CHAIN_POLICY_STATUS.
//--------------------------------------------------------------------------

// dwRegPolicySettings are defined in wintrust.h
type
  PAuthenticodeExtraCertChainPolicyPara = ^TAuthenticodeExtraCertChainPolicyPara; 
  {$EXTERNALSYM _AUTHENTICODE_EXTRA_CERT_CHAIN_POLICY_PARA} 
  _AUTHENTICODE_EXTRA_CERT_CHAIN_POLICY_PARA = record 
    cbSize: DWORD; 
    dwRegPolicySettings: DWORD; 
    pSignerInfo: PCMSG_SIGNER_INFO;       // optional 
  end; 
  {$EXTERNALSYM AUTHENTICODE_EXTRA_CERT_CHAIN_POLICY_PARA} 
  AUTHENTICODE_EXTRA_CERT_CHAIN_POLICY_PARA = _AUTHENTICODE_EXTRA_CERT_CHAIN_POLICY_PARA; 
  {$EXTERNALSYM PAUTHENTICODE_EXTRA_CERT_CHAIN_POLICY_PARA} 
  PAUTHENTICODE_EXTRA_CERT_CHAIN_POLICY_PARA = ^_AUTHENTICODE_EXTRA_CERT_CHAIN_POLICY_PARA; 
  TAuthenticodeExtraCertChainPolicyPara = _AUTHENTICODE_EXTRA_CERT_CHAIN_POLICY_PARA; 

  PAuthenticodeExtraCertChainPolicyStatus = ^TAuthenticodeExtraCertChainPolicyStatus;
  {$EXTERNALSYM _AUTHENTICODE_EXTRA_CERT_CHAIN_POLICY_STATUS} 
  _AUTHENTICODE_EXTRA_CERT_CHAIN_POLICY_STATUS = record 
    cbSize: DWORD; 
    fCommercial: BOOL;                    // obtained from signer statement 
  end; 
  {$EXTERNALSYM AUTHENTICODE_EXTRA_CERT_CHAIN_POLICY_STATUS} 
  AUTHENTICODE_EXTRA_CERT_CHAIN_POLICY_STATUS = _AUTHENTICODE_EXTRA_CERT_CHAIN_POLICY_STATUS; 
  {$EXTERNALSYM PAUTHENTICODE_EXTRA_CERT_CHAIN_POLICY_STATUS} 
  PAUTHENTICODE_EXTRA_CERT_CHAIN_POLICY_STATUS = ^_AUTHENTICODE_EXTRA_CERT_CHAIN_POLICY_STATUS; 
  TAuthenticodeExtraCertChainPolicyStatus = _AUTHENTICODE_EXTRA_CERT_CHAIN_POLICY_STATUS; 

//+-------------------------------------------------------------------------
//  CERT_CHAIN_POLICY_AUTHENTICODE_TS
//
//  Implements the Authenticode Time Stamp chain policy verification checks.
//
//  pvExtraPolicyPara may optionally be set to point to the following
//  AUTHENTICODE_TS_EXTRA_CERT_CHAIN_POLICY_PARA.
//
//  pvExtraPolicyStatus isn't used and must be set to NULL.
//--------------------------------------------------------------------------

// dwRegPolicySettings are defined in wintrust.h
  PAuthenticodeTsExtraCertChainPolicyPara = ^TAuthenticodeTsExtraCertChainPolicyPara;
  {$EXTERNALSYM _AUTHENTICODE_TS_EXTRA_CERT_CHAIN_POLICY_PARA} 
  _AUTHENTICODE_TS_EXTRA_CERT_CHAIN_POLICY_PARA = record 
    cbSize: DWORD; 
    dwRegPolicySettings: DWORD; 
    fCommercial: BOOL; 
  end; 
  {$EXTERNALSYM AUTHENTICODE_TS_EXTRA_CERT_CHAIN_POLICY_PARA} 
  AUTHENTICODE_TS_EXTRA_CERT_CHAIN_POLICY_PARA = _AUTHENTICODE_TS_EXTRA_CERT_CHAIN_POLICY_PARA; 
  {$EXTERNALSYM PAUTHENTICODE_TS_EXTRA_CERT_CHAIN_POLICY_PARA} 
  PAUTHENTICODE_TS_EXTRA_CERT_CHAIN_POLICY_PARA = ^_AUTHENTICODE_TS_EXTRA_CERT_CHAIN_POLICY_PARA; 
  TAuthenticodeTsExtraCertChainPolicyPara = _AUTHENTICODE_TS_EXTRA_CERT_CHAIN_POLICY_PARA; 


//+-------------------------------------------------------------------------
//  CERT_CHAIN_POLICY_SSL
//
//  Implements the SSL client/server chain policy verification checks.
//
//  pvExtraPolicyPara may optionally be set to point to the following
//  SSL_EXTRA_CERT_CHAIN_POLICY_PARA data structure
//--------------------------------------------------------------------------
const
  {$EXTERNALSYM AUTHTYPE_CLIENT}
  AUTHTYPE_CLIENT         = 1;
  {$EXTERNALSYM AUTHTYPE_SERVER}
  AUTHTYPE_SERVER         = 2;

// fdwChecks flags are defined in wininet.h
type
  PHTTPSPolicyCallbackData = ^THTTPSPolicyCallbackData; 
  {$EXTERNALSYM _HTTPSPolicyCallbackData} 
  _HTTPSPolicyCallbackData = record 
    case Byte of
      0: (cbStruct: DWORD);     // sizeof(HTTPSPolicyCallbackData);
      1: (cbSize: DWORD;        // sizeof(HTTPSPolicyCallbackData);
    dwAuthType: DWORD;
    fdwChecks: DWORD;
    pwszServerName: PWideChar   // used to check against CN=xxxx
    );
  end; 
  {$EXTERNALSYM HTTPSPolicyCallbackData} 
  HTTPSPolicyCallbackData = _HTTPSPolicyCallbackData; 
  {$EXTERNALSYM PHTTPSPolicyCallbackData} 
  {$EXTERNALSYM SSL_EXTRA_CERT_CHAIN_POLICY_PARA}
  SSL_EXTRA_CERT_CHAIN_POLICY_PARA = _HTTPSPolicyCallbackData; 
  {$EXTERNALSYM PSSL_EXTRA_CERT_CHAIN_POLICY_PARA} 
  PSSL_EXTRA_CERT_CHAIN_POLICY_PARA = ^_HTTPSPolicyCallbackData; 
  THTTPSPolicyCallbackData = _HTTPSPolicyCallbackData; 

//+-------------------------------------------------------------------------
//  CERT_CHAIN_POLICY_BASIC_CONSTRAINTS
//
//  Implements the basic constraints chain policy.
//
//  Iterates through all the certificates in the chain checking for either
//  a szOID_BASIC_CONSTRAINTS or a szOID_BASIC_CONSTRAINTS2 extension. If
//  neither extension is present, the certificate is assumed to have
//  valid policy. Otherwise, for the first certificate element, checks if
//  it matches the expected CA_FLAG or END_ENTITY_FLAG specified in
//  pPolicyPara->dwFlags. If neither or both flags are set, then, the first
//  element can be either a CA or END_ENTITY. All other elements must be
//  a CA. If the PathLenConstraint is present in the extension, its
//  checked.
//
//  The first elements in the remaining simple chains (ie, the certificate
//  used to sign the CTL) are checked to be an END_ENTITY.
//
//  If this verification fails, dwError will be set to
//  TRUST_E_BASIC_CONSTRAINTS.
//--------------------------------------------------------------------------
const
  {$EXTERNALSYM BASIC_CONSTRAINTS_CERT_CHAIN_POLICY_CA_FLAG}
  BASIC_CONSTRAINTS_CERT_CHAIN_POLICY_CA_FLAG           = $80000000;
  {$EXTERNALSYM BASIC_CONSTRAINTS_CERT_CHAIN_POLICY_END_ENTITY_FLAG}
  BASIC_CONSTRAINTS_CERT_CHAIN_POLICY_END_ENTITY_FLAG   = $40000000;

//+-------------------------------------------------------------------------
//  CERT_CHAIN_POLICY_NT_AUTH
//
//  Implements the NT Authentication chain policy.
//
//  The NT Authentication chain policy consists of 3 distinct chain
//  verifications in the following order:
//      [1] CERT_CHAIN_POLICY_BASE - Implements the base chain policy
//          verification checks. The LOWORD of dwFlags can be set in
//          pPolicyPara to alter the default policy checking behaviour. See
//          CERT_CHAIN_POLICY_BASE for more details.
//
//      [2] CERT_CHAIN_POLICY_BASIC_CONSTRAINTS - Implements the basic
//          constraints chain policy. The HIWORD of dwFlags can be set
//          to specify if the first element must be either a CA or END_ENTITY.
//          See CERT_CHAIN_POLICY_BASIC_CONSTRAINTS for more details.
//
//      [3] Checks if the second element in the chain, the CA that issued
//          the end certificate, is a trusted CA for NT
//          Authentication. A CA is considered to be trusted if it exists in
//          the 'NTAuth' system registry store found in the
//          CERT_SYSTEM_STORE_LOCAL_MACHINE_ENTERPRISE store location.
//          If this verification fails, whereby the CA isn't trusted,
//          dwError is set to CERT_E_UNTRUSTEDCA.
//
//          If CERT_PROT_ROOT_DISABLE_NT_AUTH_REQUIRED_FLAG is set
//          in the 'Flags' value of the HKLM policy 'ProtectedRoots' subkey
//          defined by CERT_PROT_ROOT_FLAGS_REGPATH, then,
//          if the above check fails, checks if the chain
//          has CERT_TRUST_HAS_VALID_NAME_CONSTRAINTS set in dwInfoStatus. This
//          will only be set if there was a valid name constraint for all
//          name spaces including UPN. If the chain doesn't have this info
//          status set, dwError is set to CERT_E_UNTRUSTEDCA.
//--------------------------------------------------------------------------

//+-------------------------------------------------------------------------
//  CERT_CHAIN_POLICY_MICROSOFT_ROOT
//
//  Checks if the last element of the first simple chain contains a
//  Microsoft root public key. If it doesn't contain a Microsoft root
//  public key, dwError is set to CERT_E_UNTRUSTEDROOT.
//
//  pPolicyPara is optional. However,
//  MICROSOFT_ROOT_CERT_CHAIN_POLICY_ENABLE_TEST_ROOT_FLAG can be set in
//  the dwFlags in pPolicyPara to also check for the Microsoft Test Roots.
//
//  pvExtraPolicyPara and pvExtraPolicyStatus aren't used and must be set
//  to NULL.
//--------------------------------------------------------------------------
  {$EXTERNALSYM MICROSOFT_ROOT_CERT_CHAIN_POLICY_ENABLE_TEST_ROOT_FLAG}
  MICROSOFT_ROOT_CERT_CHAIN_POLICY_ENABLE_TEST_ROOT_FLAG = $00010000;

//+-------------------------------------------------------------------------
// convert formatted string to binary
// If cchString is 0, then pszString is NULL terminated and
// cchString is obtained via strlen() + 1.
// dwFlags defines string format
// if pbBinary is NULL, *pcbBinary returns the size of required memory
// *pdwSkip returns the character count of skipped strings, optional
// *pdwFlags returns the actual format used in the conversion, optional
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptStringToBinaryA}
function CryptStringToBinaryA(pszString: LPCSTR; cchString, dwFlags: DWORD;
  pbBinary: PBYTE; out pcbBinary: DWORD;
  var pdwSkip, pdwFlags: DWORD): BOOL; stdcall;
//+-------------------------------------------------------------------------
// convert formatted string to binary
// If cchString is 0, then pszString is NULL terminated and
// cchString is obtained via strlen() + 1.
// dwFlags defines string format
// if pbBinary is NULL, *pcbBinary returns the size of required memory
// *pdwSkip returns the character count of skipped strings, optional
// *pdwFlags returns the actual format used in the conversion, optional
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptStringToBinaryW}
function CryptStringToBinaryW(pszString: LPCWSTR; cchString, dwFlags: DWORD;
  pbBinary: PBYTE; out pcbBinary: DWORD;
  var pdwSkip, pdwFlags: DWORD): BOOL; stdcall;
{$EXTERNALSYM CryptStringToBinary}
{$IFDEF UNICODE}
function CryptStringToBinary(pszString: LPCWSTR; cchString, dwFlags: DWORD;
  pbBinary: PBYTE; out pcbBinary: DWORD;
  var pdwSkip, pdwFlags: DWORD): BOOL; stdcall;
{$ELSE}
function CryptStringToBinary(pszString: LPCSTR; cchString, dwFlags: DWORD;
  pbBinary: PBYTE; out pcbBinary: DWORD;
  var pdwSkip, pdwFlags: DWORD): BOOL; stdcall;
{$ENDIF} // !UNICODE

//+-------------------------------------------------------------------------
// convert binary to formatted string
// dwFlags defines string format
// if pszString is NULL, *pcchString returns size in characters
// including null-terminator
//--------------------------------------------------------------------------
{$EXTERNALSYM CryptBinaryToStringA}
function CryptBinaryToStringA(pbBinary: PBYTE; cbBinary, dwFlags: DWORD;
  pszString: LPSTR; out pcchString: DWORD): BOOL; stdcall;
//+-------------------------------------------------------------------------
// convert binary to formatted string
// dwFlags defines string format
// if pszString is NULL, *pcchString returns size in characters
// including null-terminator
//--------------------------------------------------------------------------
function CryptBinaryToStringW(pbBinary: PBYTE; cbBinary, dwFlags: DWORD;
  pszString: LPWSTR; out pcchString: DWORD): BOOL; stdcall;
{$EXTERNALSYM CryptBinaryToString}
{$IFDEF UNICODE}
function CryptBinaryToString(pbBinary: PBYTE; cbBinary, dwFlags: DWORD;
  pszString: LPWSTR; out pcchString: DWORD): BOOL; stdcall;
{$ELSE}
function CryptBinaryToString(pbBinary: PBYTE; cbBinary, dwFlags: DWORD;
  pszString: LPSTR; out pcchString: DWORD): BOOL; stdcall;
{$ENDIF} // !UNICODE

const
// dwFlags has the following defines
// certenrolld_begin -- CRYPT_STRING_*
  {$EXTERNALSYM CRYPT_STRING_BASE64HEADER}
  CRYPT_STRING_BASE64HEADER             = $00000000;
  {$EXTERNALSYM CRYPT_STRING_BASE64}
  CRYPT_STRING_BASE64                   = $00000001;
  {$EXTERNALSYM CRYPT_STRING_BINARY}
  CRYPT_STRING_BINARY                   = $00000002;
  {$EXTERNALSYM CRYPT_STRING_BASE64REQUESTHEADER}
  CRYPT_STRING_BASE64REQUESTHEADER      = $00000003;
  {$EXTERNALSYM CRYPT_STRING_HEX}
  CRYPT_STRING_HEX                      = $00000004;
  {$EXTERNALSYM CRYPT_STRING_HEXASCII}
  CRYPT_STRING_HEXASCII                 = $00000005;
  {$EXTERNALSYM CRYPT_STRING_BASE64_ANY}
  CRYPT_STRING_BASE64_ANY               = $00000006;
  {$EXTERNALSYM CRYPT_STRING_ANY}
  CRYPT_STRING_ANY                      = $00000007;
  {$EXTERNALSYM CRYPT_STRING_HEX_ANY}
  CRYPT_STRING_HEX_ANY                  = $00000008;
  {$EXTERNALSYM CRYPT_STRING_BASE64X509CRLHEADER}
  CRYPT_STRING_BASE64X509CRLHEADER      = $00000009;
  {$EXTERNALSYM CRYPT_STRING_HEXADDR}
  CRYPT_STRING_HEXADDR                  = $0000000A;
  {$EXTERNALSYM CRYPT_STRING_HEXASCIIADDR}
  CRYPT_STRING_HEXASCIIADDR             = $0000000B;
  {$EXTERNALSYM CRYPT_STRING_HEXRAW}
  CRYPT_STRING_HEXRAW                   = $0000000C;

  {$EXTERNALSYM CRYPT_STRING_NOCRLF}
  CRYPT_STRING_NOCRLF                   = $40000000;
  {$EXTERNALSYM CRYPT_STRING_NOCR}
  CRYPT_STRING_NOCR                     = $80000000;
// certenrolld_end

// CryptBinaryToString uses the following flags
// CRYPT_STRING_BASE64HEADER - base64 format with certificate begin
//                             and end headers
// CRYPT_STRING_BASE64 - only base64 without headers
// CRYPT_STRING_BINARY - pure binary copy
// CRYPT_STRING_BASE64REQUESTHEADER - base64 format with request begin
//                                    and end headers
// CRYPT_STRING_BASE64X509CRLHEADER - base64 format with x509 crl begin
//                                    and end headers
// CRYPT_STRING_HEX - only hex format
// CRYPT_STRING_HEXASCII - hex format with ascii char display
// CRYPT_STRING_HEXADDR - hex format with address display
// CRYPT_STRING_HEXASCIIADDR - hex format with ascii char and address display
//
// CryptBinaryToString accepts CRYPT_STRING_NOCR or'd into one of the above.
// When set, line breaks contain only LF, instead of CR-LF pairs.

// CryptStringToBinary uses the following flags
// CRYPT_STRING_BASE64_ANY tries the following, in order:
//    CRYPT_STRING_BASE64HEADER
//    CRYPT_STRING_BASE64
// CRYPT_STRING_ANY tries the following, in order:
//    CRYPT_STRING_BASE64_ANY
//    CRYPT_STRING_BINARY -- should always succeed
// CRYPT_STRING_HEX_ANY tries the following, in order:
//    CRYPT_STRING_HEXADDR
//    CRYPT_STRING_HEXASCIIADDR
//    CRYPT_STRING_HEXASCII
//    CRYPT_STRING_HEX


//+=========================================================================
//  PFX (PKCS #12) function definitions and types
//==========================================================================

//+-------------------------------------------------------------------------
//  PKCS#12 OIDs
//--------------------------------------------------------------------------

  {$EXTERNALSYM szOID_PKCS_12_PbeIds}
  szOID_PKCS_12_PbeIds                        = '1.2.840.113549.1.12.1';
  {$EXTERNALSYM szOID_PKCS_12_pbeWithSHA1And128BitRC4}
  szOID_PKCS_12_pbeWithSHA1And128BitRC4       = '1.2.840.113549.1.12.1.1';
  {$EXTERNALSYM szOID_PKCS_12_pbeWithSHA1And40BitRC4}
  szOID_PKCS_12_pbeWithSHA1And40BitRC4        = '1.2.840.113549.1.12.1.2';
  {$EXTERNALSYM szOID_PKCS_12_pbeWithSHA1And3KeyTripleDES}
  szOID_PKCS_12_pbeWithSHA1And3KeyTripleDES   = '1.2.840.113549.1.12.1.3';
  {$EXTERNALSYM szOID_PKCS_12_pbeWithSHA1And2KeyTripleDES}
  szOID_PKCS_12_pbeWithSHA1And2KeyTripleDES   = '1.2.840.113549.1.12.1.4';
  {$EXTERNALSYM szOID_PKCS_12_pbeWithSHA1And128BitRC2}
  szOID_PKCS_12_pbeWithSHA1And128BitRC2       = '1.2.840.113549.1.12.1.5';
  {$EXTERNALSYM szOID_PKCS_12_pbeWithSHA1And40BitRC2}
  szOID_PKCS_12_pbeWithSHA1And40BitRC2        = '1.2.840.113549.1.12.1.6';


//+-------------------------------------------------------------------------
//  PBE parameters as defined in PKCS#12 as pkcs-12PbeParams.
//
//  NOTE that the salt bytes will immediately follow this structure.
//  we avoid using pointers in this structure for easy of passing
//  it into NCryptExportKey() as a NCryptBuffer (may be sent via RPC
//  to the key isolation process).
//--------------------------------------------------------------------------
type
  PCryptPkcs12PbeParams = ^TCryptPkcs12PbeParams; 
  {$EXTERNALSYM _CRYPT_PKCS12_PBE_PARAMS} 
  _CRYPT_PKCS12_PBE_PARAMS = record 
    iIterations: Integer; 
     { iteration count              } 
    cbSalt: ULONG; 
    { byte size of the salt        } 
  end; 
  {$EXTERNALSYM CRYPT_PKCS12_PBE_PARAMS} 
  CRYPT_PKCS12_PBE_PARAMS = _CRYPT_PKCS12_PBE_PARAMS; 
  TCryptPkcs12PbeParams = _CRYPT_PKCS12_PBE_PARAMS; 

//+-------------------------------------------------------------------------
//      PFXImportCertStore
//
//  Import the PFX blob and return a store containing certificates
//
//  If the password parameter is incorrect or any other problems decoding
//  the PFX blob are encountered, the function will return NULL and the
//      error code can be found from GetLastError().
//
//  The dwFlags parameter may be set to the following:
//  CRYPT_EXPORTABLE - specify that any imported keys should be marked as
//                     exportable (see documentation on CryptImportKey)
//  CRYPT_USER_PROTECTED - (see documentation on CryptImportKey)
//  CRYPT_MACHINE_KEYSET - used to force the private key to be stored in the
//                        the local machine and not the current user.
//  CRYPT_USER_KEYSET - used to force the private key to be stored in the
//                      the current user and not the local machine, even if
//                      the pfx blob specifies that it should go into local
//                      machine.
//  PKCS12_INCLUDE_EXTENDED_PROPERTIES - used to import all extended
//                     properties that were saved with CertExportCertStore()
//                     using the same flag.
//--------------------------------------------------------------------------
{$EXTERNALSYM PFXImportCertStore}
function PFXImportCertStore(var pPFX: CRYPT_DATA_BLOB;
  szPassword: LPCWSTR; dwFlags: DWORD): HCERTSTORE; stdcall;

const
// dwFlags definitions for PFXImportCertStore
//  CRYPT_EXPORTABLE = $00000001;  // CryptImportKey dwFlags
//  CRYPT_USER_PROTECTED = $00000002;  // CryptImportKey dwFlags
//  CRYPT_MACHINE_KEYSET = $00000020;  // CryptAcquireContext dwFlags
//  PKCS12_INCLUDE_EXTENDED_PROPERTIES = $10;
  {$EXTERNALSYM CRYPT_USER_KEYSET}
  CRYPT_USER_KEYSET             = $00001000;
  {$EXTERNALSYM PKCS12_PREFER_CNG_KSP}
  PKCS12_PREFER_CNG_KSP         = $00000100;  // prefer using CNG KSP
  {$EXTERNALSYM PKCS12_ALWAYS_CNG_KSP}
  PKCS12_ALWAYS_CNG_KSP         = $00000200;  // always use CNG KSP
  {$EXTERNALSYM PKCS12_ALLOW_OVERWRITE_KEY}
  PKCS12_ALLOW_OVERWRITE_KEY    = $00004000;  // allow overwrite existing key
  {$EXTERNALSYM PKCS12_NO_PERSIST_KEY}
  PKCS12_NO_PERSIST_KEY         = $00008000;  // key will not be persisted
  {$EXTERNALSYM PKCS12_IMPORT_RESERVED_MASK}
  PKCS12_IMPORT_RESERVED_MASK   = $FFFF0000;


//+-------------------------------------------------------------------------
//      PFXIsPFXBlob
//
//  This function will try to decode the outer layer of the blob as a pfx
//  blob, and if that works it will return TRUE, it will return FALSE otherwise
//
//--------------------------------------------------------------------------
{$EXTERNALSYM PFXIsPFXBlob}
function PFXIsPFXBlob(var pPFX: CRYPT_DATA_BLOB): BOOL; stdcall;

//+-------------------------------------------------------------------------
//      PFXVerifyPassword
//
//  This function will attempt to decode the outer layer of the blob as a pfx
//  blob and decrypt with the given password. No data from the blob will be
//  imported.
//
//  Return value is TRUE if password appears correct, FALSE otherwise.
//
//--------------------------------------------------------------------------
{$EXTERNALSYM PFXVerifyPassword}
function PFXVerifyPassword(var pPFX: CRYPT_DATA_BLOB; szPassword: LPCWSTR;
  dwFlags: DWORD): BOOL; stdcall;

//+-------------------------------------------------------------------------
//      PFXExportCertStoreEx
//
//  Export the certificates and private keys referenced in the passed-in store
//
//  This API encodes the blob under a stronger algorithm. The resulting
//  PKCS12 blobs are incompatible with the earlier PFXExportCertStore API.
//
//  The value passed in the password parameter will be used to encrypt and
//  verify the integrity of the PFX packet. If any problems encoding the store
//  are encountered, the function will return FALSE and the error code can
//  be found from GetLastError().
//
//  The dwFlags parameter may be set to any combination of
//      EXPORT_PRIVATE_KEYS
//      REPORT_NO_PRIVATE_KEY
//      REPORT_NOT_ABLE_TO_EXPORT_PRIVATE_KEY
//      PKCS12_INCLUDE_EXTENDED_PROPERTIES
//
//  The encoded PFX blob is returned in *pPFX. If pPFX->pbData is NULL upon
//  input, this is a length only calculation, whereby, pPFX->cbData is updated
//  with the number of bytes required for the encoded blob. Otherwise,
//  the memory pointed to by pPFX->pbData is updated with the encoded bytes
//  and pPFX->cbData is updated with the encoded byte length.
//--------------------------------------------------------------------------
{$EXTERNALSYM PFXExportCertStoreEx}
function PFXExportCertStoreEx(hStore: HCERTSTORE;
  out pPFX: CRYPT_DATA_BLOB; szPassword: LPCWSTR; pvReserved: Pointer;
  dwFlags: DWORD): BOOL; stdcall;

const
// dwFlags definitions for PFXExportCertStoreEx
  {$EXTERNALSYM REPORT_NO_PRIVATE_KEY}
  REPORT_NO_PRIVATE_KEY                 = $0001;
  {$EXTERNALSYM REPORT_NOT_ABLE_TO_EXPORT_PRIVATE_KEY}
  REPORT_NOT_ABLE_TO_EXPORT_PRIVATE_KEY = $0002;
  {$EXTERNALSYM EXPORT_PRIVATE_KEYS}
  EXPORT_PRIVATE_KEYS                   = $0004;
  {$EXTERNALSYM PKCS12_INCLUDE_EXTENDED_PROPERTIES}
  PKCS12_INCLUDE_EXTENDED_PROPERTIES    = $0010;
  {$EXTERNALSYM PKCS12_EXPORT_RESERVED_MASK}
  PKCS12_EXPORT_RESERVED_MASK           = $FFFF0000;

//+-------------------------------------------------------------------------
//      PFXExportCertStore
//
//  Export the certificates and private keys referenced in the passed-in store
//
//  This is an old API kept for compatibility with IE4 clients. New applications
//  should call the above PfxExportCertStoreEx for enhanced security.
//--------------------------------------------------------------------------
{$EXTERNALSYM PFXExportCertStore}
function PFXExportCertStore(hStore: HCERTSTORE; out pPFX: CRYPT_DATA_BLOB;
  szPassword: LPCWSTR; dwFlags: DWORD): BOOL; stdcall;


//+=========================================================================
//  APIs to get a non-blocking, time valid OCSP response for
//  a server certificate chain.
//
//  Normally, this OCSP response will be included along with the server
//  certificate in a message returned to the client. As a result only the
//  server should need to contact the OCSP responser for its certificate.
//==========================================================================

//+-------------------------------------------------------------------------
//  Server OCSP response handle.
//--------------------------------------------------------------------------
type
  {$EXTERNALSYM HCERT_SERVER_OCSP_RESPONSE}
  HCERT_SERVER_OCSP_RESPONSE = Pointer;

//+-------------------------------------------------------------------------
//  Open a handle to an OCSP response associated with a server certificate
//  chain. If the end certificate doesn't have an OCSP AIA URL, NULL is
//  returned with LastError set to CRYPT_E_NOT_IN_REVOCATION_DATABASE. NULL
//  will also be returned if unable to allocate memory or create system
//  objects.
//
//  This API will try to retrieve an initial OCSP response before returning.
//  This API will block during the retrieval. If unable to successfully
//  retrieve the first OCSP response, a non-NULL handle will still be returned
//  if not one of the error cases mentioned above.
//
//  A background thread is created that will pre-fetch time valid
//  OCSP responses.
//
//  The input chain context will be AddRef'ed and not freed until
//  the returned handle is closed.
//
//  CertCloseServerOcspResponse() must be called to close the returned
//  handle.
//
//  dwFlags and pvReserved aren't currently used and must be set to 0
//  and NULL.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertOpenServerOcspResponse}
function CertOpenServerOcspResponse(pChainContext: PCCERT_CHAIN_CONTEXT;
  dwFlags: DWORD; pvReserved: Pointer): HCERT_SERVER_OCSP_RESPONSE; stdcall;

//+-------------------------------------------------------------------------
//  AddRef a HCERT_SERVER_OCSP_RESPONSE returned by
//  CertOpenServerOcspResponse(). Each Open and AddRef requires a
//  corresponding CertCloseServerOcspResponse().
//--------------------------------------------------------------------------
{$EXTERNALSYM CertAddRefServerOcspResponse}
procedure CertAddRefServerOcspResponse(
  hServerOcspResponse: HCERT_SERVER_OCSP_RESPONSE); stdcall;

//+-------------------------------------------------------------------------
//  Close the handle returned by CertOpenServerOcspResponse() or AddRef'ed
//  by CertAddRefServerOcspResponse().
//
//  dwFlags isn't currently used and must be set to 0.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertCloseServerOcspResponse}
procedure CertCloseServerOcspResponse(
  hServerOcspResponse: HCERT_SERVER_OCSP_RESPONSE; dwFlags: DWORD); stdcall;

//+-------------------------------------------------------------------------
//  Server OCSP response context.
//--------------------------------------------------------------------------

type
  PCertServerOcspResponseContext = ^TCertServerOcspResponseContext; 
  {$EXTERNALSYM _CERT_SERVER_OCSP_RESPONSE_CONTEXT} 
  _CERT_SERVER_OCSP_RESPONSE_CONTEXT = record 
    cbSize: DWORD; 
    pbEncodedOcspResponse: PByte; 
    cbEncodedOcspResponse: DWORD; 
  end; 
  {$EXTERNALSYM CERT_SERVER_OCSP_RESPONSE_CONTEXT} 
  CERT_SERVER_OCSP_RESPONSE_CONTEXT = _CERT_SERVER_OCSP_RESPONSE_CONTEXT; 
  {$EXTERNALSYM PCERT_SERVER_OCSP_RESPONSE_CONTEXT} 
  PCERT_SERVER_OCSP_RESPONSE_CONTEXT = ^_CERT_SERVER_OCSP_RESPONSE_CONTEXT; 
  TCertServerOcspResponseContext = _CERT_SERVER_OCSP_RESPONSE_CONTEXT; 
  {$EXTERNALSYM PCCERT_SERVER_OCSP_RESPONSE_CONTEXT}
  PCCERT_SERVER_OCSP_RESPONSE_CONTEXT = ^CERT_SERVER_OCSP_RESPONSE_CONTEXT; 

//+-------------------------------------------------------------------------
//  Get a time valid OCSP response context for the handle created for
//  the server certificate chain.
//
//  This API won't block to retrieve the OCSP response. It will return
//  the current pre-fetched OCSP response. If a time valid OCSP response
//  isn't available, NULL will be returned with LAST_ERROR set to
//  CRYPT_E_REVOCATION_OFFLINE.
//
//  CertFreeServerOcspResponseContext() must be called to free the
//  returned OCSP response context.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertGetServerOcspResponseContext}
function CertGetServerOcspResponseContext(
  hServerOcspResponse: HCERT_SERVER_OCSP_RESPONSE; dwFlags: DWORD;
  pvReserved: Pointer): PCCERT_SERVER_OCSP_RESPONSE_CONTEXT; stdcall;

//+-------------------------------------------------------------------------
//  AddRef a PCCERT_SERVER_OCSP_RESPONSE_CONTEXT returned by
//  CertGetServerOcspResponseContext(). Each Get and AddRef requires a
//  corresponding CertFreeServerOcspResponseContext().
//--------------------------------------------------------------------------
{$EXTERNALSYM CertAddRefServerOcspResponseContext}
procedure CertAddRefServerOcspResponseContext(
  pServerOcspResponseContext: PCCERT_SERVER_OCSP_RESPONSE_CONTEXT); stdcall;

//+-------------------------------------------------------------------------
//  Free the OCSP response context returned by
//  CertGetServerOcspResponseContext().
//--------------------------------------------------------------------------
{$EXTERNALSYM CertFreeServerOcspResponseContext}
procedure CertFreeServerOcspResponseContext(
  pServerOcspResponseContext: PCCERT_SERVER_OCSP_RESPONSE_CONTEXT); stdcall;

//+-------------------------------------------------------------------------
//  Helper function to do URL retrieval of logo or biometric information
//  specified in either the szOID_LOGOTYPE_EXT or szOID_BIOMETRIC_EXT
//  certificate extension.
//
//  Only the first hashed URL matching lpszLogoOrBiometricType is used
//  to do the URL retrieval. Only direct logotypes are supported.
//  The bytes at the first URL are retrieved via
//  CryptRetrieveObjectByUrlW and hashed. The computed hash is compared
//  against the hash in the certificate.  For success, ppbData, pcbData
//  and optionally ppwszMimeType are updated with
//  CryptMemAlloc'ed memory which must be freed by calling CryptMemFree().
//  For failure, *ppbData, *pcbData and optionally *ppwszMimeType are
//  zero'ed.
//
//  For failure, the following errors may be set in LastError:
//      E_INVALIDARG - invalid lpszLogoOrBiometricType, not one of the
//          acceptable predefined types.
//      CRYPT_E_NOT_FOUND - certificate doesn't have the
//          szOID_LOGOTYPE_EXT or szOID_BIOMETRIC_EXT extension or a matching
//          lpszLogoOrBiometricType wasn't found with a non-empty
//          hashed URL.
//      ERROR_NOT_SUPPORTED - matched the unsupported indirect logotype
//      NTE_BAD_ALGID - unknown hash algorithm OID
//      ERROR_INVALID_DATA - no bytes were retrieved at the specified URL
//          in the certificate extension
//      CRYPT_E_HASH_VALUE - the computed hash doesn't match the hash
//          in the certificate
//  CertRetrieveLogoOrBiometricInfo calls the following functions which
//  will set LastError for failure:
//      CryptDecodeObjectEx(szOID_LOGOTYPE_EXT or szOID_BIOMETRIC_EXT)
//      CryptRetrieveObjectByUrlW
//      CryptHashCertificate
//      CryptMemAlloc
//
//  lpszLogoOrBiometricType is one of the predefined logotype or biometric
//  types, an other logotype OID or a biometric OID.
//
//  dwRetrievalFlags - see CryptRetrieveObjectByUrlW
//  dwTimeout - see CryptRetrieveObjectByUrlW
//
//  dwFlags - reserved, must be set to 0
//  pvReserved - reserved, must be set to NULL
//
//  *ppwszMimeType is always NULL for the biometric types. For success,
//  the caller must always check if non-NULL before dereferencing.
//--------------------------------------------------------------------------
{$EXTERNALSYM CertRetrieveLogoOrBiometricInfo}
function CertRetrieveLogoOrBiometricInfo(pCertContext: PCCERT_CONTEXT;
  lpszLogoOrBiometricType: LPCSTR; dwRetrievalFlags, dwTimeout, dwFlags: DWORD;
  pvReserved: Pointer; var ppbData: PBYTE;
  var pcbData: DWORD; var ppwszMimeType: LPWSTR): BOOL; stdcall;

// Predefined Logotypes
const
  {$EXTERNALSYM CERT_RETRIEVE_ISSUER_LOGO}
  CERT_RETRIEVE_ISSUER_LOGO                       = LPCSTR(1);
  {$EXTERNALSYM CERT_RETRIEVE_SUBJECT_LOGO}
  CERT_RETRIEVE_SUBJECT_LOGO                      = LPCSTR(2);
  {$EXTERNALSYM CERT_RETRIEVE_COMMUNITY_LOGO}
  CERT_RETRIEVE_COMMUNITY_LOGO                    = LPCSTR(3);

// Predefined Biometric types
  {$EXTERNALSYM CERT_RETRIEVE_BIOMETRIC_PREDEFINED_BASE_TYPE}
  CERT_RETRIEVE_BIOMETRIC_PREDEFINED_BASE_TYPE    = LPCSTR(1000);

  {$EXTERNALSYM CERT_RETRIEVE_BIOMETRIC_PICTURE_TYPE}
  CERT_RETRIEVE_BIOMETRIC_PICTURE_TYPE            =
    (CERT_RETRIEVE_BIOMETRIC_PREDEFINED_BASE_TYPE + CERT_BIOMETRIC_PICTURE_TYPE);
  {$EXTERNALSYM CERT_RETRIEVE_BIOMETRIC_SIGNATURE_TYPE}
  CERT_RETRIEVE_BIOMETRIC_SIGNATURE_TYPE          =
    (CERT_RETRIEVE_BIOMETRIC_PREDEFINED_BASE_TYPE + CERT_BIOMETRIC_SIGNATURE_TYPE);

//+-------------------------------------------------------------------------
//  Dialog to select a certificate from the specified store.
//
//  Returns the selected certificate context. If no certificate was
//  selected, NULL is returned.
//
//  pwszTitle is either NULL or the title to be used for the dialog.
//  If NULL, the default title is used.  The default title is
//  "Select Certificate".
//
//  pwszDisplayString is either NULL or the text statement in the selection
//  dialog.  If NULL, the default phrase
//  "Select a certificate you wish to use" is used in the dialog.
//
//  dwDontUseColumn can be set to exclude columns from the selection
//  dialog. See the CRYPTDLG_SELECTCERT_*_COLUMN definitions below.
//
//  dwFlags currently isn't used and should be set to 0.
//--------------------------------------------------------------------------

{$EXTERNALSYM CryptUIDlgSelectCertificateFromStore}
function CryptUIDlgSelectCertificateFromStore(hCertStore: HCERTSTORE; hwnd: HWND; pwszTitle, pwszDisplayString: LPCWSTR;
  dwDontUseColumn, dwFlags: DWORD; pvReserved: LPVOID): PCCERT_CONTEXT; stdcall;

// flags for dwDontUseColumn

const
  CRYPTUI_SELECT_ISSUEDTO_COLUMN       = $000000001;
  {$EXTERNALSYM CRYPTUI_SELECT_ISSUEDTO_COLUMN}
  CRYPTUI_SELECT_ISSUEDBY_COLUMN       = $000000002;
  {$EXTERNALSYM CRYPTUI_SELECT_ISSUEDBY_COLUMN}
  CRYPTUI_SELECT_INTENDEDUSE_COLUMN    = $000000004;
  {$EXTERNALSYM CRYPTUI_SELECT_INTENDEDUSE_COLUMN}
  CRYPTUI_SELECT_FRIENDLYNAME_COLUMN   = $000000008;
  {$EXTERNALSYM CRYPTUI_SELECT_FRIENDLYNAME_COLUMN}
  CRYPTUI_SELECT_LOCATION_COLUMN       = $000000010;
  {$EXTERNALSYM CRYPTUI_SELECT_LOCATION_COLUMN}
  CRYPTUI_SELECT_EXPIRATION_COLUMN     = $000000020;
  {$EXTERNALSYM CRYPTUI_SELECT_EXPIRATION_COLUMN}

implementation

uses
  ShellAPI;

{ Macro functions }

function GET_ALG_CLASS(x: DWORD): DWORD;
begin
  Result := x and (7 shl 13);
end;

function GET_ALG_TYPE(x: DWORD): DWORD;
begin
  Result := x and (15 shl 9);
end;

function GET_ALG_SID(x: DWORD): DWORD;
begin
  Result := x and 511;
end;

function RCRYPT_SUCCEEDED(rt: BOOL): Boolean;
begin
  Result := rt;
end;

function RCRYPT_FAILED(rt: BOOL): Boolean;
begin
  Result := not rt;
end;

function IS_CERT_RDN_CHAR_STRING(X: DWORD): Boolean;
begin
  Result := (X and CERT_RDN_TYPE_MASK) >= CERT_RDN_NUMERIC_STRING;
end;

function GET_CERT_ENCODING_TYPE(X: DWORD): DWORD;
begin
  Result := X and CERT_ENCODING_TYPE_MASK;
end;

function GET_CMSG_ENCODING_TYPE(X: DWORD): DWORD;
begin
  Result := X and CMSG_ENCODING_TYPE_MASK;
end;

function GET_CERT_UNICODE_RDN_ERR_INDEX(X: DWORD): DWORD;
begin
  Result := (X shr CERT_UNICODE_RDN_ERR_INDEX_SHIFT) and CERT_UNICODE_RDN_ERR_INDEX_MASK;
end;

function GET_CERT_UNICODE_ATTR_ERR_INDEX(X: DWORD): DWORD;
begin
  Result := (X shr CERT_UNICODE_ATTR_ERR_INDEX_SHIFT) and CERT_UNICODE_ATTR_ERR_INDEX_MASK;
end;

function GET_CERT_UNICODE_VALUE_ERR_INDEX(X: DWORD): DWORD;
begin
  Result := X and CERT_UNICODE_VALUE_ERR_INDEX_MASK;
end;

function GET_CERT_ALT_NAME_ENTRY_ERR_INDEX(X: DWORD): DWORD;
begin
  Result := (X shr CERT_ALT_NAME_ENTRY_ERR_INDEX_SHIFT) and CERT_ALT_NAME_ENTRY_ERR_INDEX_MASK;
end;

function GET_CERT_ALT_NAME_VALUE_ERR_INDEX(X: DWORD): DWORD;
begin
  Result := X and CERT_ALT_NAME_VALUE_ERR_INDEX_MASK;
end;

function GET_CRL_DIST_POINT_ERR_INDEX(X: DWORD): DWORD;
begin
  Result := (X shr CRL_DIST_POINT_ERR_INDEX_SHIFT) and CRL_DIST_POINT_ERR_INDEX_MASK;
end;

function IS_CRL_DIST_POINT_ERR_CRL_ISSUER(X: DWORD): Boolean;
begin
  Result := 0 <> (X and CRL_DIST_POINT_ERR_CRL_ISSUER_BIT);
end;

function GET_CROSS_CERT_DIST_POINT_ERR_INDEX(X: DWORD): DWORD;
begin
  Result := (X shr CROSS_CERT_DIST_POINT_ERR_INDEX_SHIFT) and  CROSS_CERT_DIST_POINT_ERR_INDEX_MASK;
end;

function IS_CERT_EXCLUDED_SUBTREE(X: DWORD): Boolean;
begin
  Result := 0 <> (X and CERT_EXCLUDED_SUBTREE_BIT);
end;

function IS_SPECIAL_OID_INFO_ALGID(Algid: ALG_ID): Boolean;
begin
  Result := (Algid >= CALG_OID_INFO_PARAMETERS);
end;

function IS_CERT_HASH_PROP_ID(X: DWORD): Boolean;
begin
  Result := (CERT_SHA1_HASH_PROP_ID = X) or
            (CERT_MD5_HASH_PROP_ID = X) or
            (CERT_SIGNATURE_HASH_PROP_ID = X);
end;

function IS_PUBKEY_HASH_PROP_ID(X: DWORD): Boolean;
begin
  Result := (CERT_ISSUER_PUBLIC_KEY_MD5_HASH_PROP_ID = X) or
            (CERT_SUBJECT_PUBLIC_KEY_MD5_HASH_PROP_ID = X);
end;

function IS_CHAIN_HASH_PROP_ID(X: DWORD): Boolean;
begin
  Result := (CERT_ISSUER_PUBLIC_KEY_MD5_HASH_PROP_ID = X) or
            (CERT_SUBJECT_PUBLIC_KEY_MD5_HASH_PROP_ID = X) or
            (CERT_ISSUER_SERIAL_NUMBER_MD5_HASH_PROP_ID = X) or
            (CERT_SUBJECT_NAME_MD5_HASH_PROP_ID = X);
end;

// Imports

function CryptAcquireContextA; external Advapi32 name 'CryptAcquireContextA';
function CryptAcquireContextW; external Advapi32 name 'CryptAcquireContextW';
{$IFDEF UNICODE}
function CryptAcquireContext; external Advapi32 name 'CryptAcquireContextW';
{$ELSE}
function CryptAcquireContext; external Advapi32 name 'CryptAcquireContextA';
{$ENDIF}
function CryptReleaseContext; external Advapi32 name 'CryptReleaseContext';
function CryptGenKey; external Advapi32 name 'CryptGenKey';
function CryptDeriveKey; external Advapi32 name 'CryptDeriveKey';
function CryptDestroyKey; external Advapi32 name 'CryptDestroyKey';
function CryptSetKeyParam; external Advapi32 name 'CryptSetKeyParam';
function CryptGetKeyParam; external Advapi32 name 'CryptGetKeyParam';
function CryptSetHashParam; external Advapi32 name 'CryptSetHashParam';
function CryptGetHashParam; external Advapi32 name 'CryptGetHashParam';
function CryptSetProvParam; external Advapi32 name 'CryptSetProvParam';
function CryptGetProvParam; external Advapi32 name 'CryptGetProvParam';
function CryptGenRandom; external Advapi32 name 'CryptGenRandom';
function CryptGetUserKey; external Advapi32 name 'CryptGetUserKey';
function CryptExportKey; external Advapi32 name 'CryptExportKey';
function CryptImportKey; external Advapi32 name 'CryptImportKey';
function CryptEncrypt; external Advapi32 name 'CryptEncrypt';
function CryptDecrypt; external Advapi32 name 'CryptDecrypt';
function CryptCreateHash; external Advapi32 name 'CryptCreateHash';
function CryptHashData; external Advapi32 name 'CryptHashData';
function CryptHashSessionKey; external Advapi32 name 'CryptHashSessionKey';
function CryptDestroyHash; external Advapi32 name 'CryptDestroyHash';
function CryptSignHashA; external Advapi32 name 'CryptSignHashA';
function CryptSignHashW; external Advapi32 name 'CryptSignHashW';
{$IFDEF UNICODE}
function CryptSignHash; external Advapi32 name 'CryptSignHashW';
{$ELSE}
function CryptSignHash; external Advapi32 name 'CryptSignHashA';
{$ENDIF}
function CryptVerifySignatureA; external Advapi32 name 'CryptVerifySignatureA';
function CryptVerifySignatureW; external Advapi32 name 'CryptVerifySignatureW';
{$IFDEF UNICODE}
function CryptVerifySignature; external Advapi32 name 'CryptVerifySignatureW';
{$ELSE}
function CryptVerifySignature; external Advapi32 name 'CryptVerifySignatureA';
{$ENDIF}
function CryptSetProviderA; external Advapi32 name 'CryptSetProviderA';
function CryptSetProviderW; external Advapi32 name 'CryptSetProviderW';
{$IFDEF UNICODE}
function CryptSetProvider; external Advapi32 name 'CryptSetProviderW';
{$ELSE}
function CryptSetProvider; external Advapi32 name 'CryptSetProviderA';
{$ENDIF}
function CryptSetProviderExA; external Advapi32 name 'CryptSetProviderExA';
function CryptSetProviderExW; external Advapi32 name 'CryptSetProviderExW';
{$IFDEF UNICODE}
function CryptSetProviderEx; external Advapi32 name 'CryptSetProviderExW';
{$ELSE}
function CryptSetProviderEx; external Advapi32 name 'CryptSetProviderExA';
{$ENDIF}
function CryptGetDefaultProviderA; external Advapi32 name 'CryptGetDefaultProviderA';
function CryptGetDefaultProviderW; external Advapi32 name 'CryptGetDefaultProviderW';
{$IFDEF UNICODE}
function CryptGetDefaultProvider; external Advapi32 name 'CryptGetDefaultProviderW';
{$ELSE}
function CryptGetDefaultProvider; external Advapi32 name 'CryptGetDefaultProviderA';
{$ENDIF}
function CryptEnumProviderTypesA; external Advapi32 name 'CryptEnumProviderTypesA';
function CryptEnumProviderTypesW; external Advapi32 name 'CryptEnumProviderTypesW';
{$IFDEF UNICODE}
function CryptEnumProviderTypes; external Advapi32 name 'CryptEnumProviderTypesW';
{$ELSE}
function CryptEnumProviderTypes; external Advapi32 name 'CryptEnumProviderTypesA';
{$ENDIF}
function CryptEnumProvidersA; external Advapi32 name 'CryptEnumProvidersA';
function CryptEnumProvidersW; external Advapi32 name 'CryptEnumProvidersW';
{$IFDEF UNICODE}
function CryptEnumProviders; external Advapi32 name 'CryptEnumProvidersW';
{$ELSE}
function CryptEnumProviders; external Advapi32 name 'CryptEnumProvidersA';
{$ENDIF}
function CryptContextAddRef; external Advapi32 name 'CryptContextAddRef';
function CryptDuplicateKey; external Advapi32 name 'CryptDuplicateKey';
function CryptDuplicateHash; external Advapi32 name 'CryptDuplicateHash';
function CryptFormatObject; external Crypt32 name 'CryptFormatObject';
function CryptEncodeObjectEx; external Crypt32 name 'CryptEncodeObjectEx';
function CryptEncodeObject; external Crypt32 name 'CryptEncodeObject';
function CryptDecodeObjectEx; external Crypt32 name 'CryptDecodeObjectEx';
function CryptDecodeObject; external Crypt32 name 'CryptDecodeObject';
function CryptInstallOIDFunctionAddress; external Crypt32 name 'CryptInstallOIDFunctionAddress';
function CryptInitOIDFunctionSet; external Crypt32 name 'CryptInitOIDFunctionSet';
function CryptGetOIDFunctionAddress; external Crypt32 name 'CryptGetOIDFunctionAddress';
function CryptGetDefaultOIDDllList; external Crypt32 name 'CryptGetDefaultOIDDllList';
function CryptGetDefaultOIDFunctionAddress; external Crypt32 name 'CryptGetDefaultOIDFunctionAddress';
function CryptFreeOIDFunctionAddress; external Crypt32 name 'CryptFreeOIDFunctionAddress';
function CryptRegisterOIDFunction; external Crypt32 name 'CryptRegisterOIDFunction';
function CryptUnregisterOIDFunction; external Crypt32 name 'CryptUnregisterOIDFunction';
function CryptRegisterDefaultOIDFunction; external Crypt32 name 'CryptRegisterDefaultOIDFunction';
function CryptUnregisterDefaultOIDFunction; external Crypt32 name 'CryptUnregisterDefaultOIDFunction';
function CryptSetOIDFunctionValue; external Crypt32 name 'CryptSetOIDFunctionValue';
function CryptGetOIDFunctionValue; external Crypt32 name 'CryptGetOIDFunctionValue';
function CryptEnumOIDFunction; external Crypt32 name 'CryptEnumOIDFunction';
function CryptFindOIDInfo; external Crypt32 name 'CryptFindOIDInfo';
function CryptRegisterOIDInfo; external Crypt32 name 'CryptRegisterOIDInfo';
function CryptUnregisterOIDInfo; external Crypt32 name 'CryptUnregisterOIDInfo';
function CryptEnumOIDInfo; external Crypt32 name 'CryptEnumOIDInfo';
function CryptFindLocalizedName; external Crypt32 name 'CryptFindLocalizedName';
function CryptMsgOpenToEncode; external Crypt32 name 'CryptMsgOpenToEncode';
function CryptMsgCalculateEncodedLength; external Crypt32 name 'CryptMsgCalculateEncodedLength';
function CryptMsgOpenToDecode; external Crypt32 name 'CryptMsgOpenToDecode';
function CryptMsgDuplicate; external Crypt32 name 'CryptMsgDuplicate';
function CryptMsgClose; external Crypt32 name 'CryptMsgClose';
function CryptMsgUpdate; external Crypt32 name 'CryptMsgUpdate';
function CryptMsgGetParam; external Crypt32 name 'CryptMsgGetParam';
function CryptMsgControl; external Crypt32 name 'CryptMsgControl';
function CryptMsgVerifyCountersignatureEncoded; external Crypt32 name 'CryptMsgVerifyCountersignatureEncoded';
function CryptMsgVerifyCountersignatureEncodedEx; external Crypt32 name 'CryptMsgVerifyCountersignatureEncodedEx';
function CryptMsgCountersign; external Crypt32 name 'CryptMsgCountersign';
function CryptMsgCountersignEncoded; external Crypt32 name 'CryptMsgCountersignEncoded';
function CertOpenStore; external Crypt32 name 'CertOpenStore';
function CertDuplicateStore; external Crypt32 name 'CertDuplicateStore';
function CertSaveStore; external Crypt32 name 'CertSaveStore';
function CertCloseStore; external Crypt32 name 'CertCloseStore';
function CertGetSubjectCertificateFromStore; external Crypt32 name 'CertGetSubjectCertificateFromStore';
function CertEnumCertificatesInStore; external Crypt32 name 'CertEnumCertificatesInStore';
function CertFindCertificateInStore; external Crypt32 name 'CertFindCertificateInStore';
function CertGetIssuerCertificateFromStore; external Crypt32 name 'CertGetIssuerCertificateFromStore';
function CertVerifySubjectCertificateContext; external Crypt32 name 'CertVerifySubjectCertificateContext';
function CertDuplicateCertificateContext; external Crypt32 name 'CertDuplicateCertificateContext';
function CertCreateCertificateContext; external Crypt32 name 'CertCreateCertificateContext';
function CertFreeCertificateContext; external Crypt32 name 'CertFreeCertificateContext';
function CertSetCertificateContextProperty; external Crypt32 name 'CertSetCertificateContextProperty';
function CertGetCertificateContextProperty; external Crypt32 name 'CertGetCertificateContextProperty';
function CertEnumCertificateContextProperties; external Crypt32 name 'CertEnumCertificateContextProperties';
function CertCreateCTLEntryFromCertificateContextProperties; external Crypt32 name 'CertCreateCTLEntryFromCertificateContextProperties';
function CertSetCertificateContextPropertiesFromCTLEntry; external Crypt32 name 'CertSetCertificateContextPropertiesFromCTLEntry';
function CertGetCRLFromStore; external Crypt32 name 'CertGetCRLFromStore';
function CertEnumCRLsInStore; external Crypt32 name 'CertEnumCRLsInStore';
function CertFindCRLInStore; external Crypt32 name 'CertFindCRLInStore';
function CertDuplicateCRLContext; external Crypt32 name 'CertDuplicateCRLContext';
function CertCreateCRLContext; external Crypt32 name 'CertCreateCRLContext';
function CertFreeCRLContext; external Crypt32 name 'CertFreeCRLContext';
function CertSetCRLContextProperty; external Crypt32 name 'CertSetCRLContextProperty';
function CertGetCRLContextProperty; external Crypt32 name 'CertGetCRLContextProperty';
function CertEnumCRLContextProperties; external Crypt32 name 'CertEnumCRLContextProperties';
function CertFindCertificateInCRL; external Crypt32 name 'CertFindCertificateInCRL';
function CertIsValidCRLForCertificate; external Crypt32 name 'CertIsValidCRLForCertificate';
function CertAddEncodedCertificateToStore; external Crypt32 name 'CertAddEncodedCertificateToStore';
function CertAddCertificateContextToStore; external Crypt32 name 'CertAddCertificateContextToStore';
function CertAddSerializedElementToStore; external Crypt32 name 'CertAddSerializedElementToStore';
function CertDeleteCertificateFromStore; external Crypt32 name 'CertDeleteCertificateFromStore';
function CertAddEncodedCRLToStore; external Crypt32 name 'CertAddEncodedCRLToStore';
function CertAddCRLContextToStore; external Crypt32 name 'CertAddCRLContextToStore';
function CertDeleteCRLFromStore; external Crypt32 name 'CertDeleteCRLFromStore';
function CertSerializeCertificateStoreElement; external Crypt32 name 'CertSerializeCertificateStoreElement';
function CertSerializeCRLStoreElement; external Crypt32 name 'CertSerializeCRLStoreElement';
function CertDuplicateCTLContext; external Crypt32 name 'CertDuplicateCTLContext';
function CertCreateCTLContext; external Crypt32 name 'CertCreateCTLContext';
function CertFreeCTLContext; external Crypt32 name 'CertFreeCTLContext';
function CertSetCTLContextProperty; external Crypt32 name 'CertSetCTLContextProperty';
function CertGetCTLContextProperty; external Crypt32 name 'CertGetCTLContextProperty';
function CertEnumCTLContextProperties; external Crypt32 name 'CertEnumCTLContextProperties';
function CertEnumCTLsInStore; external Crypt32 name 'CertEnumCTLsInStore';
function CertFindSubjectInCTL; external Crypt32 name 'CertFindSubjectInCTL';
function CertFindCTLInStore; external Crypt32 name 'CertFindCTLInStore';
function CertAddEncodedCTLToStore; external Crypt32 name 'CertAddEncodedCTLToStore';
function CertAddCTLContextToStore; external Crypt32 name 'CertAddCTLContextToStore';
function CertSerializeCTLStoreElement; external Crypt32 name 'CertSerializeCTLStoreElement';
function CertDeleteCTLFromStore; external Crypt32 name 'CertDeleteCTLFromStore';
function CertAddCertificateLinkToStore; external Crypt32 name 'CertAddCertificateLinkToStore';
function CertAddCRLLinkToStore; external Crypt32 name 'CertAddCRLLinkToStore';
function CertAddCTLLinkToStore; external Crypt32 name 'CertAddCTLLinkToStore';
function CertAddStoreToCollection; external Crypt32 name 'CertAddStoreToCollection';
procedure CertRemoveStoreFromCollection; external Crypt32 name 'CertRemoveStoreFromCollection';
function CertControlStore; external Crypt32 name 'CertControlStore';
function CertSetStoreProperty; external Crypt32 name 'CertSetStoreProperty';
function CertGetStoreProperty; external Crypt32 name 'CertGetStoreProperty';
function CertCreateContext; external Crypt32 name 'CertCreateContext';
function CertRegisterSystemStore; external Crypt32 name 'CertRegisterSystemStore';
function CertRegisterPhysicalStore; external Crypt32 name 'CertRegisterPhysicalStore';
function CertUnregisterSystemStore; external Crypt32 name 'CertUnregisterSystemStore';
function CertUnregisterPhysicalStore; external Crypt32 name 'CertUnregisterPhysicalStore';
function CertEnumSystemStoreLocation; external Crypt32 name 'CertEnumSystemStoreLocation';
function CertEnumSystemStore; external Crypt32 name 'CertEnumSystemStore';
function CertEnumPhysicalStore; external Crypt32 name 'CertEnumPhysicalStore';
function CertGetEnhancedKeyUsage; external Crypt32 name 'CertGetEnhancedKeyUsage';
function CertSetEnhancedKeyUsage; external Crypt32 name 'CertSetEnhancedKeyUsage';
function CertAddEnhancedKeyUsageIdentifier; external Crypt32 name 'CertAddEnhancedKeyUsageIdentifier';
function CertRemoveEnhancedKeyUsageIdentifier; external Crypt32 name 'CertRemoveEnhancedKeyUsageIdentifier';
function CertGetValidUsages; external Crypt32 name 'CertGetValidUsages';
function CryptMsgGetAndVerifySigner; external Crypt32 name 'CryptMsgGetAndVerifySigner';
function CryptMsgSignCTL; external Crypt32 name 'CryptMsgSignCTL';
function CryptMsgEncodeAndSignCTL; external Crypt32 name 'CryptMsgEncodeAndSignCTL';
function CertFindSubjectInSortedCTL; external Crypt32 name 'CertFindSubjectInSortedCTL';
function CertEnumSubjectInSortedCTL; external Crypt32 name 'CertEnumSubjectInSortedCTL';
function CertVerifyCTLUsage; external Crypt32 name 'CertVerifyCTLUsage';
function CertVerifyRevocation; external Crypt32 name 'CertVerifyRevocation';
function CertCompareIntegerBlob; external Crypt32 name 'CertCompareIntegerBlob';
function CertCompareCertificate; external Crypt32 name 'CertCompareCertificate';
function CertCompareCertificateName; external Crypt32 name 'CertCompareCertificateName';
function CertIsRDNAttrsInCertificateName; external Crypt32 name 'CertIsRDNAttrsInCertificateName';
function CertComparePublicKeyInfo; external Crypt32 name 'CertComparePublicKeyInfo';
function CertGetPublicKeyLength; external Crypt32 name 'CertGetPublicKeyLength';
function CryptVerifyCertificateSignature; external Crypt32 name 'CryptVerifyCertificateSignature';
function CryptVerifyCertificateSignatureEx; external Crypt32 name 'CryptVerifyCertificateSignatureEx';
function CryptHashToBeSigned; external Crypt32 name 'CryptHashToBeSigned';
function CryptHashCertificate; external Crypt32 name 'CryptHashCertificate';
function CryptHashCertificate2; external Crypt32 name 'CryptHashCertificate2';
function CryptSignCertificate; external Crypt32 name 'CryptSignCertificate';
function CryptSignAndEncodeCertificate; external Crypt32 name 'CryptSignAndEncodeCertificate';
function CertVerifyTimeValidity; external Crypt32 name 'CertVerifyTimeValidity';
function CertVerifyCRLTimeValidity; external Crypt32 name 'CertVerifyCRLTimeValidity';
function CertVerifyValidityNesting; external Crypt32 name 'CertVerifyValidityNesting';
function CertVerifyCRLRevocation; external Crypt32 name 'CertVerifyCRLRevocation';
function CertAlgIdToOID; external Crypt32 name 'CertAlgIdToOID';
function CertOIDToAlgId; external Crypt32 name 'CertOIDToAlgId';
function CertFindExtension; external Crypt32 name 'CertFindExtension';
function CertFindAttribute; external Crypt32 name 'CertFindAttribute';
function CertFindRDNAttr; external Crypt32 name 'CertFindRDNAttr';
function CertGetIntendedKeyUsage; external Crypt32 name 'CertGetIntendedKeyUsage';
function CryptInstallDefaultContext; external Crypt32 name 'CryptInstallDefaultContext';
function CryptUninstallDefaultContext; external Crypt32 name 'CryptUninstallDefaultContext';
function CryptExportPublicKeyInfo; external Crypt32 name 'CryptExportPublicKeyInfo';
function CryptExportPublicKeyInfoEx; external Crypt32 name 'CryptExportPublicKeyInfoEx';
function CryptImportPublicKeyInfo; external Crypt32 name 'CryptImportPublicKeyInfo';
function CryptImportPublicKeyInfoEx; external Crypt32 name 'CryptImportPublicKeyInfoEx';
function CryptImportPublicKeyInfoEx2; external Crypt32 name 'CryptImportPublicKeyInfoEx2';
function CryptAcquireCertificatePrivateKey; external Crypt32 name 'CryptAcquireCertificatePrivateKey';
function CryptFindCertificateKeyProvInfo; external Crypt32 name 'CryptFindCertificateKeyProvInfo';
function CryptImportPKCS8; external Crypt32 name 'CryptImportPKCS8';
function CryptExportPKCS8; external Crypt32 name 'CryptExportPKCS8';
function CryptHashPublicKeyInfo; external Crypt32 name 'CryptHashPublicKeyInfo';
function CertRDNValueToStrA; external Crypt32 name 'CertRDNValueToStrA';
function CertRDNValueToStrW; external Crypt32 name 'CertRDNValueToStrW';
{$IFDEF UNICODE}
function CertRDNValueToStr; external Crypt32 name 'CertRDNValueToStrW';
{$ELSE}
function CertRDNValueToStr; external Crypt32 name 'CertRDNValueToStrA';
{$ENDIF}
function CertNameToStrA; external Crypt32 name 'CertNameToStrA';
function CertNameToStrW; external Crypt32 name 'CertNameToStrW';
{$IFDEF UNICODE}
function CertNameToStr; external Crypt32 name 'CertNameToStrW';
{$ELSE}
function CertNameToStr; external Crypt32 name 'CertNameToStrA';
{$ENDIF}
function CertStrToNameA; external Crypt32 name 'CertStrToNameA';
function CertStrToNameW; external Crypt32 name 'CertStrToNameW';
{$IFDEF UNICODE}
function CertStrToName; external Crypt32 name 'CertStrToNameW';
{$ELSE}
function CertStrToName; external Crypt32 name 'CertStrToNameA';
{$ENDIF}
function CertGetNameStringA; external Crypt32 name 'CertGetNameStringA';
function CertGetNameStringW; external Crypt32 name 'CertGetNameStringW';
{$IFDEF UNICODE}
function CertGetNameString; external Crypt32 name 'CertGetNameStringW"';
{$ELSE}
function CertGetNameString; external Crypt32 name 'CertGetNameStringA';
{$ENDIF}
function CryptSignMessage; external Crypt32 name 'CryptSignMessage';
function CryptVerifyMessageSignature; external Crypt32 name 'CryptVerifyMessageSignature';
function CryptGetMessageSignerCount; external Crypt32 name 'CryptGetMessageSignerCount';
function CryptGetMessageCertificates; external Crypt32 name 'CryptGetMessageCertificates';
function CryptVerifyDetachedMessageSignature; external Crypt32 name 'CryptVerifyDetachedMessageSignature';
function CryptEncryptMessage; external Crypt32 name 'CryptEncryptMessage';
function CryptDecryptMessage; external Crypt32 name 'CryptDecryptMessage';
function CryptSignAndEncryptMessage; external Crypt32 name 'CryptSignAndEncryptMessage';
function CryptDecryptAndVerifyMessageSignature; external Crypt32 name 'CryptDecryptAndVerifyMessageSignature';
function CryptDecodeMessage; external Crypt32 name 'CryptDecodeMessage';
function CryptHashMessage; external Crypt32 name 'CryptHashMessage';
function CryptVerifyMessageHash; external Crypt32 name 'CryptVerifyMessageHash';
function CryptVerifyDetachedMessageHash; external Crypt32 name 'CryptVerifyDetachedMessageHash';
function CryptSignMessageWithKey; external Crypt32 name 'CryptSignMessageWithKey';
function CryptVerifyMessageSignatureWithKey; external Crypt32 name 'CryptVerifyMessageSignatureWithKey';
function CertOpenSystemStoreA; external Crypt32 name 'CertOpenSystemStoreA';
function CertOpenSystemStoreW; external Crypt32 name 'CertOpenSystemStoreW';
{$IFDEF UNICODE}
function CertOpenSystemStore; external Crypt32 name 'CertOpenSystemStoreW';
{$ELSE}
function CertOpenSystemStore; external Crypt32 name 'CertOpenSystemStoreA';
{$ENDIF}
function CertAddEncodedCertificateToSystemStoreA; external Crypt32 name 'CertAddEncodedCertificateToSystemStoreA';
function CertAddEncodedCertificateToSystemStoreW; external Crypt32 name 'CertAddEncodedCertificateToSystemStoreW';
{$IFDEF UNICODE}
function CertAddEncodedCertificateToSystemStore; external Crypt32 name 'CertAddEncodedCertificateToSystemStoreW';
{$ELSE}
function CertAddEncodedCertificateToSystemStore; external Crypt32 name 'CertAddEncodedCertificateToSystemStoreA';
{$ENDIF}
function CryptQueryObject; external Crypt32 name 'CryptQueryObject';
function CryptMemAlloc; external Crypt32 name 'CryptMemAlloc';
function CryptMemRealloc; external Crypt32 name 'CryptMemRealloc';
procedure CryptMemFree; external Crypt32 name 'CryptMemFree';
function CryptCreateAsyncHandle; external Crypt32 name 'CryptCreateAsyncHandle';
function CryptSetAsyncParam; external Crypt32 name 'CryptSetAsyncParam';
function CryptGetAsyncParam; external Crypt32 name 'CryptGetAsyncParam';
function CryptCloseAsyncHandle; external Crypt32 name 'CryptCloseAsyncHandle';
function CryptRetrieveObjectByUrlA; external CryptNet name 'CryptRetrieveObjectByUrlA';
function CryptRetrieveObjectByUrlW; external CryptNet name 'CryptRetrieveObjectByUrlW';
{$IFDEF UNICODE}
function CryptRetrieveObjectByUrl; external CryptNet name 'CryptRetrieveObjectByUrlW';
{$ELSE}
function CryptRetrieveObjectByUrl; external CryptNet name 'CryptRetrieveObjectByUrlA';
{$ENDIF}
function CryptInstallCancelRetrieval; external CryptNet name 'CryptInstallCancelRetrieval';
function CryptUninstallCancelRetrieval; external CryptNet name 'CryptUninstallCancelRetrieval';
function CryptCancelAsyncRetrieval; external CryptNet name 'CryptCancelAsyncRetrieval';
function CryptGetObjectUrl; external CryptNet name 'CryptGetObjectUrl';
function CryptGetTimeValidObject; external CryptNet name 'CryptGetTimeValidObject';
function CryptFlushTimeValidObject; external CryptNet name 'CryptFlushTimeValidObject';
function CryptProtectData; external Crypt32 name 'CryptProtectData';
function CryptUnprotectData; external Crypt32 name 'CryptUnprotectData';
function CryptUpdateProtectedState; external Crypt32 name 'CryptUpdateProtectedState';
function CryptProtectMemory; external Crypt32 name 'CryptProtectMemory';
function CryptUnprotectMemory; external Crypt32 name 'CryptUnprotectMemory';
function CertCreateSelfSignCertificate; external Crypt32 name 'CertCreateSelfSignCertificate';
function CryptGetKeyIdentifierProperty; external Crypt32 name 'CryptGetKeyIdentifierProperty';
function CryptSetKeyIdentifierProperty; external Crypt32 name 'CryptSetKeyIdentifierProperty';
function CryptEnumKeyIdentifierProperties; external Crypt32 name 'CryptEnumKeyIdentifierProperties';
function CryptCreateKeyIdentifierFromCSP; external Crypt32 name 'CryptCreateKeyIdentifierFromCSP';
function CertCreateCertificateChainEngine; external Crypt32 name 'CertCreateCertificateChainEngine';
procedure CertFreeCertificateChainEngine; external Crypt32 name 'CertFreeCertificateChainEngine';
function CertResyncCertificateChainEngine; external Crypt32 name 'CertResyncCertificateChainEngine';
function CertGetCertificateChain; external Crypt32 name 'CertGetCertificateChain';
procedure CertFreeCertificateChain; external Crypt32 name 'CertFreeCertificateChain';
function CertDuplicateCertificateChain; external Crypt32 name 'CertDuplicateCertificateChain';
function CertFindChainInStore; external Crypt32 name 'CertFindChainInStore';
function CertVerifyCertificateChainPolicy; external Crypt32 name 'CertVerifyCertificateChainPolicy';
function CryptStringToBinaryA; external Crypt32 name 'CryptStringToBinaryA';
function CryptStringToBinaryW; external Crypt32 name 'CryptStringToBinaryW';
{$IFDEF UNICODE}
function CryptStringToBinary; external Crypt32 name 'CryptStringToBinaryW';
{$ELSE}
function CryptStringToBinary; external Crypt32 name 'CryptStringToBinaryA';
{$ENDIF}
function CryptBinaryToStringA; external Crypt32 name 'CryptBinaryToStringA';
function CryptBinaryToStringW; external Crypt32 name 'CryptBinaryToStringW';
{$IFDEF UNICODE}
function CryptBinaryToString; external Crypt32 name 'CryptBinaryToStringW';
{$ELSE}
function CryptBinaryToString; external Crypt32 name 'CryptBinaryToStringA';
{$ENDIF}
function PFXImportCertStore; external Crypt32 name 'PFXImportCertStore';
function PFXIsPFXBlob; external Crypt32 name 'PFXIsPFXBlob';
function PFXVerifyPassword; external Crypt32 name 'PFXVerifyPassword';
function PFXExportCertStoreEx; external Crypt32 name 'PFXExportCertStoreEx';
function PFXExportCertStore; external Crypt32 name 'PFXExportCertStore';
function CertOpenServerOcspResponse; external Crypt32 name 'CertOpenServerOcspResponse';
procedure CertAddRefServerOcspResponse; external Crypt32 name 'CertAddRefServerOcspResponse';
procedure CertCloseServerOcspResponse; external Crypt32 name 'CertCloseServerOcspResponse';
function CertGetServerOcspResponseContext; external Crypt32 name 'CertGetServerOcspResponseContext';
procedure CertAddRefServerOcspResponseContext; external Crypt32 name 'CertAddRefServerOcspResponseContext';
procedure CertFreeServerOcspResponseContext; external Crypt32 name 'CertFreeServerOcspResponseContext';
function CertRetrieveLogoOrBiometricInfo; external Crypt32 name 'CertRetrieveLogoOrBiometricInfo';
function CryptUIDlgSelectCertificateFromStore; external cryptuiapi name 'CryptUIDlgSelectCertificateFromStore';

// Dynamically loaded functions

function _GetEncSChannel(var pData: PByte; var dwDecSize: DWORD): BOOL; cdecl;
begin
  Result := False;
end;

function _CryptGetLocalKeyLimits(algId: ALG_ID; dwFlags: DWORD;
  out pLimits: CRYPT_KEY_LIMITS; var cbLimitLength: DWORD): BOOL; stdcall;
begin
  Result := False;
end;

function _FindCertsByIssuer(pCertChains: PCERT_CHAIN;
  out pcbCertChains, pcCertChains: DWORD; pbEncodedIssuerName: PBYTE;
  cbEncodedIssuerName: DWORD; pwszPurpose: LPCWSTR;
  dwKeySpec: DWORD): HRESULT; stdcall;
begin
  Result := E_NOTIMPL;
end;

function _CryptExportPKCS8Ex(psExportParams: PCRYPT_PKCS8_EXPORT_PARAMS;
  dwFlags: DWORD; pvAuxInfo: Pointer; out pbPrivateKeyBlob: BYTE;
  var pcbPrivateKeyBlob: PDWORD): BOOL; stdcall;
begin
  Result := False;
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
      Writeln(Crypt32, ': ', ProcName, ' replaced');
{$ENDIF}
    end;
  end;
end;

var
  WinCryptModule: HMODULE;
  SoftpubModule: HMODULE;

initialization
{$IFDEF SFGPA_DEBUG}
  Writeln;
  Writeln('WinCrypt');
  Writeln;
{$ENDIF}
  WinCryptModule := LoadLibrary(Crypt32);
  GetEncSChannel := SafeGetProcAddress(WinCryptModule, 'GetEncSChannel', @_GetEncSChannel);
  CryptGetLocalKeyLimits := SafeGetProcAddress(WinCryptModule, 'CryptGetLocalKeyLimits', @_CryptGetLocalKeyLimits);
  CryptExportPKCS8Ex := SafeGetprocAddress(WinCryptModule, 'CryptExportPKCS8Ex', @_CryptExportPKCS8Ex);

{$IFDEF SFGPA_DEBUG}
  Writeln;
  Writeln('Softpub');
  Writeln;
{$ENDIF}
  SoftpubModule := LoadLibrary('softpub.dll');
  FindCertsByIssuer := SafeGetProcAddress(SoftpubModule, 'FindCertsByIssuer', @_FindCertsByIssuer)

finalization
  if WinCryptModule <> 0 then
    FreeLibrary(WinCryptModule);
  if SoftpubModule <> 0 then
    FreeLibrary(SoftpubModule);

end.


