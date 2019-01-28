(******************************************************************************
 Delphi import unit for OpenSSL libeay, version 0.7m, 2010-11-05

 For OpenSSL libeay32.dll version 0.9.6b, DLL compiled by GnuWin32.
 Tested with Borland Delphi 6, 7 Personal

 Copyright (C) 2002-2010, Marco Ferrante.
 2002-2006, CSITA - Università di Genova (IT).
     http://www.csita.unige.it/.
 2007-2009, DISI - Università di Genova (IT).
     http://www.disi.unige.it/.
 2010, CSITA - Università di Genova (IT).
     http://www.csita.unige.it/.
 Thanks to:
   - Michal Hlavac (Slovakia)
   - Risto Tamme (Estonia)
   - Simon Sun (probably USA)
   
   - Luis Carrasco, Bambu Code (Mexico) 
 for contributes and fix

 A small part of this work is inspired on MySSL, interface to OpenSSL for
 Delphi written by Jan Tomasek.

 This product is related to cryptographic software written by Eric
 Young (eay@cryptsoft.com). This product is related to software written
 by Tim Hudson (tjh@cryptsoft.com)

 == Changelog =======================================================
 
 Version 0.7n, 2010-12-27
 - typo corrected
 
 Version 0.7m, 2010-11-05
 - added support for PCKS#8 functions (contributed by Luis Carrasco - Bambu Code, Mexico),
 - redefinition of PChar as PCharacter to handle PChar and PAnsiChar types
 - basic AES support
 
 Version 0.7h, 2009-02-25
 - added X509_sign(), 
 
 Version 0.7g, 2007-02-20
 - Bugfix: PKCS12_parse function uses a by-reference parameter
 - Bugfix: BIO_get_mem_data(). Thanks to Andrei
 - Removed redundant declarations

 Version 0.7f, 2007-02-20
 - Bugfix: PEM_read_* function uses a by-reference parameter

 Version 0.7e, 2007-02-11
 - Bugfix
 - Replace BN_mod import with a wrapper to BN_div, see man BN_mul(3)

 Version 0.7d, 2006-12-15
 - Typos
 - Removed EVP_MD_size and EVP_MD_CTX_size: these functions are not defined in
   DLL and handle their parameter in a non-opaque way.

 Version 0.7c, 2006-11-14
 - Add BIGNUM functions
 - Defined RSA record
 - Add missing EVP_VerifyFinal

 Version 0.7b, 2006-11-05
 - Between 0.9.6h and 0.9.7, OpenSSL split OpenSSL_add_all_algorithms
   in two new functions. Some versions of libeay32.dll use old name,
   some use new one. See http://www.openssl.org/news/changelog.html
   In this unit, OpenSSL_add_all_algorithms is now a wrapper that
   dynamically loads appropriate function from DLL.

 Version 0.7a, 2006-09-14
 - Bug fixes
 - Defined wrapper for OpenSSL memory management function

 == License =========================================================
 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:

 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.

 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in
    the documentation and/or other materials provided with the
    distribution.

 3. All advertising materials mentioning features or use of this
    software must display the following acknowledgment:
    "This product includes software developed by CSITA - University
    of Genoa (Italy) (http://www.unige.it/)"

 4. Redistributions of any form whatsoever must retain the following
    acknowledgment:
    "This product includes software developed by the University
    of Genoa (Italy) (http://www.unige.it/) and its contributors"

 THIS SOFTWARE IS PROVIDED BY THE OpenSSL PROJECT ``AS IS'' AND ANY
 EXPRESSED OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE OpenSSL PROJECT OR
 ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 OF THE POSSIBILITY OF SUCH DAMAGE.
 ====================================================================

 ******************************************************************************)
{$I ACBr.inc}

unit libeay32;

interface

const
  _SSLEAY_VERSION = 0;
  _SSLEAY_CFLAGS = 2;
  _SSLEAY_BUILT_ON = 3;
  _SSLEAY_PLATFORM = 4;

  // PADDING constants
  RSA_PKCS1_PADDING = 1;
  RSA_SSLV23_PADDING = 2;
  RSA_NO_PADDING = 3;
  RSA_PKCS1_OAEP_PADDING = 4;

  // ASN1 type constants
  NID_undef = 0;
  NID_rsaEncryption = 6;
  NID_pkcs7_signed = 22;
  NID_pkcs7_signedAndEnveloped = 24;
  NID_basic_constraints = 87;
  NID_subject_alt_name = 85;

  V_ASN1_INTEGER = $02;
  V_ASN1_ENUMERATED = 10;
  V_ASN1_NEG_INTEGER = $102;
  V_ASN1_UTCTIME = 23;
  V_ASN1_GENERALIZEDTIME = 24;
  V_ASN1_CONTEXT_SPECIFIC = $80;

  B_ASN1_NUMERICSTRING	= $0001;
  B_ASN1_PRINTABLESTRING = $0002;
  B_ASN1_T61STRING = $0004;
  B_ASN1_TELETEXSTRING = $0008;
  B_ASN1_VIDEOTEXSTRING	= $0008;
  B_ASN1_IA5STRING = $0010;
  B_ASN1_GRAPHICSTRING = $0020;
  B_ASN1_ISO64STRING = $0040;
  B_ASN1_VISIBLESTRING = $0040;
  B_ASN1_GENERALSTRING = $0080;
  B_ASN1_UNIVERSALSTRING = $0100;
  B_ASN1_OCTET_STRING = $0200;
  B_ASN1_BIT_STRING = $0400;
  B_ASN1_BMPSTRING = $0800;
  B_ASN1_UNKNOWN = $1000;
  B_ASN1_UTF8STRING = $2000;

  MBSTRING_FLAG	= $1000;
  MBSTRING_ASC = MBSTRING_FLAG or 1;
  MBSTRING_BMP = MBSTRING_FLAG or 2;
  MBSTRING_UNIV = MBSTRING_FLAG or 3;
  MBSTRING_UTF8 = MBSTRING_FLAG or 4;

  // These are the 'types' of BIOs
  BIO_TYPE_NONE = $0000;
  BIO_TYPE_MEM = $0001 or $0400;
  BIO_TYPE_FILE = $0002 or $0400;

  BIO_TYPE_FD = $0004 or $0400 or $0100;
  BIO_TYPE_SOCKET = $0005 or $0400 or $0100;
  BIO_TYPE_NULL = $0006 or $0400;
  BIO_TYPE_SSL = $0007 or $0200;
  BIO_TYPE_MD = $0008 or $0200;  // passive filter
  BIO_TYPE_BUFFER = $0009 or $0200;  // filter
  BIO_TYPE_CIPHER = $00010 or $0200;  // filter
  BIO_TYPE_BASE64 = $00011 or $0200;  // filter
  BIO_TYPE_CONNECT = $00012 or $0400 or $0100;  // socket - connect
  BIO_TYPE_ACCEPT = $00013 or $0400 or $0100;  // socket for accept
  BIO_TYPE_PROXY_CLIENT = $00014 or $0200;  // client proxy BIO
  BIO_TYPE_PROXY_SERVER = $00015 or $0200;  // server proxy BIO
  BIO_TYPE_NBIO_TEST = $00016 or $0200;  // server proxy BIO
  BIO_TYPE_NULL_FILTER = $00017 or $0200;
  BIO_TYPE_BER = $00018 or $0200;  // BER -> bin filter
  BIO_TYPE_BIO = $00019 or $0400;  // (half a; BIO pair
  BIO_TYPE_LINEBUFFER = $00020 or $0200;  // filter

  BIO_TYPE_DESCRIPTOR = $0100;  // socket, fd, connect or accept
  BIO_TYPE_FILTER= $0200;
  BIO_TYPE_SOURCE_SINK = $0400;

  // BIO ops constants
  // BIO_FILENAME_READ|BIO_CLOSE to open or close on free.
  // BIO_set_fp(in,stdin,BIO_NOCLOSE);
  BIO_NOCLOSE = $00;
  BIO_CLOSE = $01;
  BIO_FP_READ = $02;
  BIO_FP_WRITE = $04;
  BIO_FP_APPEND = $08;
  BIO_FP_TEXT = $10;

  BIO_C_SET_FILENAME = 108;
  BIO_CTRL_RESET = 1;  // opt - rewind/zero etc
  BIO_CTRL_EOF = 2;  // opt - are we at the eof
  BIO_CTRL_INFO = 3;  // opt - extra tit-bits
  BIO_CTRL_SET = 4;  // man - set the 'IO' type
  BIO_CTRL_GET = 5;  // man - get the 'IO' type
  BIO_CTRL_PUSH = 6;  // opt - internal, used to signify change
  BIO_CTRL_POP = 7;  // opt - internal, used to signify change
  BIO_CTRL_GET_CLOSE = 8;  // man - set the 'close' on free
  BIO_CTRL_SET_CLOSE = 9;  // man - set the 'close' on free
  BIO_CTRL_PENDING = 10;  // opt - is their more data buffered
  BIO_CTRL_FLUSH = 11;  // opt - 'flush' buffered output
  BIO_CTRL_DUP = 12;  // man - extra stuff for 'duped' BIO
  BIO_CTRL_WPENDING = 13;  // opt - number of bytes still to write

  BIO_C_GET_MD_CTX = 120;

  BN_CTX_NUM = 16;
  BN_CTX_NUM_POS = 12;

  // RSA key exponent
  RSA_3: longint = $3;
  RSA_F4: longint = $10001;

  FORMAT_UNDEF = 0;
  FORMAT_ASN1 = 1;
  FORMAT_TEXT = 2;
  FORMAT_PEM = 3;
  FORMAT_NETSCAPE = 4;
  FORMAT_PKCS12 = 5;
  FORMAT_SMIME = 6;
  FORMAT_X509 = 509; // Not defined in original libeay

  PKCS7_TEXT = $001;
  PKCS7_NOCERTS = $002;
  PKCS7_NOSIGS = $004;
  PKCS7_NOCHAIN = $008;
  PKCS7_NOINTERN = $010;
  PKCS7_NOVERIFY = $020;
  PKCS7_DETACHED = $040;
  PKCS7_BINARY = $080;
  PKCS7_NOATTR = $100;
  PKCS7_NOSMIMECAP = $200;

  X509_V_ERR_DEPTH_ZERO_SELF_SIGNED_CERT = 18;

  SHA_DIGEST_LENGTH = 20;

  EVP_MAX_MD_SIZE = 16+20;  // The SSLv3 md5+sha1 type
  EVP_PKEY_RSA = NID_rsaEncryption;

  EXFLAG_KUSAGE = $02;
  EXFLAG_BCONS = $1;
  EXFLAG_CA = $10;
  EXFLAG_SS = $20;
  EXFLAG_V1 = $40;
  KU_KEY_CERT_SIGN = $0004;
  V1_ROOT = EXFLAG_V1 or EXFLAG_SS;

  GEN_OTHERNAME = 0 or V_ASN1_CONTEXT_SPECIFIC;
  GEN_EMAIL = 1 or V_ASN1_CONTEXT_SPECIFIC;
  GEN_DNS = 2 or V_ASN1_CONTEXT_SPECIFIC;
  GEN_X400 = 3 or V_ASN1_CONTEXT_SPECIFIC;
  GEN_DIRNAME = 4 or V_ASN1_CONTEXT_SPECIFIC;
  GEN_EDIPARTY = 5 or V_ASN1_CONTEXT_SPECIFIC;
  GEN_URI = 6 or V_ASN1_CONTEXT_SPECIFIC;
  GEN_IPADD = 7 or V_ASN1_CONTEXT_SPECIFIC;
  GEN_RID = 8 or V_ASN1_CONTEXT_SPECIFIC;

  XN_FLAG_SEP_MASK       = 983040;  //(0xf << 16)
  XN_FLAG_SEP_CPLUS_SPC  = 131072;  //(2 << 16) ,+ spaced: more readable
  ASN1_STRFLGS_UTF8_CONVERT = 16;

type
// Check the correct "Char" type to use according to the Delphi Version
{$IFDEF FPC}
 PCharacter = PAnsiChar;
{$ELSE}
 {$IF CompilerVersion >= 20}
  PCharacter = PAnsiChar;
 {$ELSEIF True}
  PCharacter = PChar;
 {$IFEND}
{$ENDIF}

  pSTACK = pointer;

  // ASN1 types
  pASN1_OBJECT = pointer;
  pASN1_STRING = ^ASN1_STRING;
  ASN1_STRING = record
	length: integer;
	asn1_type: integer;
	data: pointer;
	flags: longint;
	end;
  pASN1_IA5STRING = pASN1_STRING;
  pASN1_INTEGER = pASN1_STRING;
  pASN1_ENUMERATED = pASN1_STRING;
  pASN1_TIME = pASN1_STRING;
  pASN1_OCTET_STRING = pASN1_STRING;

  pBN_ULONG = ^BN_ULONG;
  BN_ULONG = array of byte; // system dependent, consider it as a opaque pointer
  pBIGNUM = ^BIGNUM;
  BIGNUM = record
	d: pBN_ULONG;	// Pointer to an array of 'BN_BITS2' bit chunks.
	top: integer;	// Index of last used d +1.
                        // The next are internal book keeping for bn_expand.
	dmax: integer;	// Size of the d array.
	neg: integer;	// one if the number is negative
	flags: integer;
        end;

  pBN_CTX = ^BN_CTX;
  BN_CTX = record
	tos: integer;
	bn: array [0..BN_CTX_NUM-1] of BIGNUM;
	flags: integer;
	depth: integer;
	pos: array [0..BN_CTX_NUM_POS-1] of integer;
	too_many: integer;
        end;

  pBN_BLINDING = ^BN_BLINDING;
  BN_BLINDING = record
	init: integer;
	A: pBIGNUM;
	Ai: pBIGNUM;
	_mod: pBIGNUM;  // just a reference (original name: mod)
	end;

  // Used for montgomery multiplication
  pBN_MONT_CTX = ^BN_MONT_CTX;
  BN_MONT_CTX = record
	ri: integer;    // number of bits in R
	RR: BIGNUM;     // used to convert to montgomery form
	N: BIGNUM;      // The modulus
	Ni: BIGNUM;     // R*(1/R mod N) - N*Ni = 1
	                // (Ni is only stored for bignum algorithm)
	n0: BN_ULONG;   // least significant word of Ni
	flags: integer;
	end;

  // Used for reciprocal division/mod functions
  // It cannot be shared between threads
  pBN_RECP_CTX = ^BN_RECP_CTX;
  BN_RECP_CTX = record
	N: BIGNUM;	// the divisor
	Nr: BIGNUM;	// the reciprocal
	num_bits: integer;
	shift: integer;
	flags: integer;
	end;

  pX509_STORE_CTX = pointer;

  // Password ask callback for I/O function prototipe
  // It must fill buffer with password and return password length
  TPWCallbackFunction = function(buffer: PCharacter; length: integer;
      verify: integer; data: pointer): integer; cdecl;
  // Progress callback function prototipe
  TProgressCallbackFunction = procedure(status: integer; progress: integer;
      data: pointer);
  // Certificate verification callback
  TCertificateVerifyFunction = function(ok: integer;
      ctx: pX509_STORE_CTX): integer; cdecl;

  pBIO = pointer;
  pBIO_METHOD = pointer;
  pBUF_MEM = pointer;

  des_cblock = array [0..7] of byte;
  MD2_CTX = record
    num: integer;
    data: array [0..15] of byte;
    cksm: array [0..15] of cardinal;
    state: array [0..15] of cardinal;
    end;
  MD4_CTX = record
    A, B, C, D: cardinal;
    Nl, Nh: cardinal;
    data: array [0..15] of cardinal;
    num: integer;
    end;
  MD5_CTX = record
    A, B, C, D: cardinal;
    Nl, Nh: cardinal;
    data: array [0..15] of cardinal;
    num: integer;
    end;
  RIPEMD160_CTX = record
    A, B, C, D, E: cardinal;
    Nl, Nh: cardinal;
    data: array [0..15] of cardinal;
    num: integer;
    end;
  SHA_CTX = record
    h0, h1, h2, h3, h4: cardinal;
    Nl, Nh: cardinal;
    data: array [0..16] of cardinal;
    num: integer;
    end;
  MDC2_CTX = record
    num: integer;
    data: array [0..7] of byte;
    h, hh: des_cblock;
    pad_type: integer; // either 1 or 2, default 1
    end;

  CRYPTO_EX_DATA = record
    sk: pointer;
    dummy: integer;
    end;

  pAES_KEY = pointer;
  
  pRSA = ^RSA;
  pRSA_METHOD = pointer;
  RSA = record
	// The first parameter is used to pickup errors where
	// this is passed instead of aEVP_PKEY, it is set to 0
	pad: integer;
	version: integer;
	meth: pRSA_METHOD;
	n: pBIGNUM;
	e: pBIGNUM;
	d: pBIGNUM;
	p: pBIGNUM;
	q: pBIGNUM;
	dmp1: pBIGNUM;
	dmq1: pBIGNUM;
	iqmp: pBIGNUM;
	// be careful using this if the RSA structure is shared
	ex_data: CRYPTO_EX_DATA;
	references: integer;
	flags: integer;
	// Used to cache montgomery values
	_method_mod_n: pBN_MONT_CTX;
	_method_mod_p: pBN_MONT_CTX;
	_method_mod_q: pBN_MONT_CTX;
        // all BIGNUM values are actually in the following data, if it is not
	// NULL
	bignum_data: ^byte;
	blinding: ^BN_BLINDING;
	end;

  pDSA = ^DSA;
  DSA = record
	// This first variable is used to pick up errors where
	// a DSA is passed instead of of a EVP_PKEY
	pad: integer;
	version: integer;
	write_params: integer;
	p: pointer;
	q: pointer;	// = 20
	g: pointer;
	pub_key: pointer;  // y public key
	priv_key: pointer; // x private key
	kinv: pointer;	// Signing pre-calc
	r: pointer;	// Signing pre-calc
	flags: integer;
	// Normally used to cache montgomery values
	method_mont_p: PCharacter;
	references: integer;
	ex_data: record
      sk: pointer;
      dummy: integer;
      end;
	meth: pointer;
    end;

  pDH = pointer;

  pEC_KEY = pointer;

  pEVP_CIPHER = pointer;

  pEVP_MD = ^EVP_MD;
  EVP_MD = record
    _type: integer;
    pkey_type: integer;
    md_size: integer;
    init: pointer;
    update: pointer;
    final: pointer;
    sign: pointer;
    verify: pointer;
    required_pkey_type: array [0..4] of integer;
    block_size: integer;
    ctx_size: integer;
    end;

  // Superfluo? No, in EVP_MD ci sono le dimensioni del risultato
  pEVP_MD_CTX = ^EVP_MD_CTX;
  EVP_MD_CTX = record
    digest: pEVP_MD;
    case integer of
      0: (base: array [0..3] of byte);
      1: (md2: MD2_CTX);
      8: (md4: MD4_CTX);
      2: (md5: MD5_CTX);
      16: (ripemd160: RIPEMD160_CTX);
      4: (sha: SHA_CTX);
      32: (mdc2: MDC2_CTX);
    end;

  pX509_NAME_ENTRY = ^X509_NAME_ENTRY;
  X509_NAME_ENTRY = record
    obj: pASN1_OBJECT;
    value: pASN1_STRING;
	_set: integer;
	size: integer; // temp variable
    end;

  pX509_NAME = ^X509_NAME;
  pDN = ^X509_NAME;
  X509_NAME = record
    entries: pointer;
    modified: integer;
    bytes: pointer;
    hash: cardinal;
    end;

  pX509_VAL = ^X509_VAL;
  X509_VAL = record
	notBefore: pASN1_TIME;
    notAfter: pASN1_TIME;
	end;

  pX509_CINF = ^X509_CINF;
  X509_CINF = record
    version: pointer;
    serialNumber: pointer;
    signature: pointer;
    issuer: pointer;
    validity: pX509_VAL;
    subject: pointer;
    key: pointer;
    issuerUID: pointer;
    subjectUID: pointer;
    extensions: pointer;
    end;

  pX509 = ^X509;
  X509 = record
    cert_info: pX509_CINF;
    sig_alg: pointer;  // ^X509_ALGOR
    signature: pointer;  // ^ASN1_BIT_STRING
    valid: integer;
    references: integer;
    name: PCharacter;
    ex_data: CRYPTO_EX_DATA;
    ex_pathlen: integer;
    ex_flags: integer;
    ex_kusage: integer;
    ex_xkusage: integer;
    ex_nscert: integer;
    skid: pASN1_OCTET_STRING;
    akid: pointer;  // ?
    sha1_hash: array [0..SHA_DIGEST_LENGTH-1] of char;
    aux: pointer;  // ^X509_CERT_AUX
    end;
  pSTACK_OFX509 = pointer;
  pX509_STORE = ^X509_STORE;
  pX509_LOOKUP = pointer;
  pSTACK_OF509LOOKUP = pointer;
  pX509_LOOKUP_METHOD = pointer;
  X509_STORE = record
    cache: integer;
    objs: pSTACK_OFX509;
    get_cert_methods: pSTACK_OF509LOOKUP;
    verify: pointer;  // function called to verify a certificate
    verify_cb: TCertificateVerifyFunction;
    ex_data: pointer;
    references: integer;
    depth: integer;
    end;

  pX509V3_CTX = pointer;
  
  pX509_REQ = ^X509_REQ;
  pX509_REQ_INFO = ^X509_REQ_INFO;
  X509_REQ_INFO = record
	asn1: pointer;
	length: integer;
	version: pointer;
	subject: pX509_NAME;
	pubkey: pointer;
	attributes: pointer;
	req_kludge: integer;
	end;
  X509_REQ = record
	req_info: pX509_REQ_INFO;
	sig_alg: pointer;
	signature: pointer;
	references: integer;
	end;

  pX509_EXTENSION = ^X509_EXTENSION;
  X509_EXTENSION = record
    obj: pASN1_OBJECT;
	critical: Smallint;
	netscape_hack: Smallint;
	value: pASN1_OCTET_STRING;
	method: pointer;	// struct v3_ext_method *: V3 method to use
	ext_val: pointer;	// extension value
	end;
  pSTACK_OFX509_EXTENSION = pointer;

  pX509_CRL = pointer;
  
  pX509_SIG = ^X509_SIG;
  X509_SIG = record
     algor: Pointer; // X509_ALGOR *algor;
     digest: pASN1_OCTET_STRING;
  end;
  
  pBASIC_CONSTRAINTS = ^BASIC_CONSTRAINTS;
  BASIC_CONSTRAINTS = record
    ca: integer;
    pathlen: pASN1_INTEGER;
    end;
  pOTHERNAME = ^OTHERNAME;
  OTHERNAME = record
    type_id: pASN1_OBJECT;  //There is a bug in x509v3/x509v3.h ?
    value: pointer;  //pASN1_TYPE;
    end;
  pGENERAL_NAME = ^GENERAL_NAME ;
  pGENERAL_NAMEDATA = record
    case integer of
      GEN_EMAIL: (ia5: pASN1_IA5STRING);  // also DNS and URI
      GEN_IPADD: (ip: pASN1_OCTET_STRING);
      GEN_DIRNAME: (dirn: pX509_NAME);
      GEN_RID: (rid: pASN1_OBJECT);
      GEN_OTHERNAME: (otherName: pOTHERNAME);
      GEN_X400: (other: pointer);  // also EDI
      end;
  GENERAL_NAME = record
    nametype: integer;
    d: pGENERAL_NAMEDATA;
    end;

  pEVP_PKEY = ^EVP_PKEY;
  EVP_PKEY_PKEY = record
    case integer of
      0: (ptr: PCharacter);
      1: (rsa: pRSA);  // ^rsa_st
      2: (dsa: pDSA);  // ^dsa_st
      3: (dh: pDH);  // ^dh_st
    end;
  EVP_PKEY = record
    ktype: integer;
    save_type: integer;
    references: integer;
    pkey: EVP_PKEY_PKEY;
    save_parameters: integer;
    attributes: pSTACK_OFX509;
    end;

  pPKCS7_SIGNER_INFO = pointer;
  pSTACK_OFPKCS7_SIGNER_INFO = pointer;
  pPKCS7_signed = ^PKCS7_signed;
  PKCS7_signed = record
    version: pASN1_INTEGER;
    md_algs: pointer;  // ^STACK_OF(X509_ALGOR)
    cert: pointer;  // ^STACK_OF(X509)
    crl: pointer;  // ^STACK_OF(X509_CRL)
    signer_info: pSTACK_OFPKCS7_SIGNER_INFO;
    contents: pointer;  // ^struct pkcs7_st
    end;

  pPKCS7_signedandenveloped = ^PKCS7_signedandenveloped;
  PKCS7_signedandenveloped = record
    version: pASN1_INTEGER;
    md_algs: pointer;  // ^STACK_OF(X509_ALGOR)
    cert: pointer;  // ^STACK_OF(X509)
    crl: pointer;  // ^STACK_OF(X509_CRL)
    signer_info: pSTACK_OFPKCS7_SIGNER_INFO;
    enc_data: pointer;  // ^PKCS7_ENC_CONTENT
    recipientinfo: pointer;  // ^STACK_OF(PKCS7_RECIP_INFO)
    end;

  pPKCS7 = ^PKCS7;
  PKCS7 = record
    asn1: PCharacter;
    length: integer;
    state: integer;
    detached: integer;
    asn1_type: pointer; // ^ASN1_OBJECT
    case integer of
      0: (ptr: pASN1_OCTET_STRING);
      1: (data: pointer);  // ^PKCS7_SIGNED
      2: (sign: pPKCS7_signed);  // ^PKCS7_SIGNED
      3: (enveloped: pointer);  // ^PKCS7_ENVELOPE
      4: (signed_and_enveloped: pPKCS7_signedandenveloped);
      5: (digest: pointer);  // ^PKCS7_DIGEST
      6: (encrypted: pointer);  // ^PKCS7_ENCRYPT
      7: (other: pointer);  // ^ASN1_TYPE
    end;

  pPKCS8_Priv_Key_Info = ^PKCS8_Priv_Key_Info;
  PKCS8_Priv_Key_Info = record
    broken: Integer; // Flag for various broken formats */
    version: pASN1_INTEGER;
    pkeyalg: Pointer; // X509_ALGOR *pkeyalg;
    pkey: Pointer; // ASN1_TYPE *pkey; /* Should be OCTET STRING but some are broken */
    attributes: Pointer; // STACK_OF(X509_ATTRIBUTE) *attributes;
    end;
    
  pPKCS12 = ^PKCS12;
  PKCS12 = record
    version: pointer;
    mac: pointer;
    authsafes: pPKCS7;
    end;

function SSLeay: cardinal;
function SSLeay_version(t: integer): PCharacter; cdecl;
procedure OpenSSL_add_all_algorithms;
procedure OpenSSL_add_all_ciphers; cdecl;
procedure OpenSSL_add_all_digests; cdecl;
procedure EVP_cleanup(); cdecl;

function ERR_get_error: cardinal; cdecl;
function ERR_peek_error: cardinal; cdecl;
function ERR_peek_last_error: cardinal; cdecl;
function ERR_error_string(e: cardinal; buf: PCharacter): PCharacter; cdecl;
procedure ERR_clear_error;
procedure ERR_load_crypto_strings;
procedure ERR_free_strings;

// Low level debugable memory management function
function CRYPTO_malloc(length: longint; const f: PCharacter; line: integer): pointer; cdecl;
function CRYPTO_realloc(str: PCharacter; length: longint; const f: PCharacter; line: integer): pointer; cdecl;
function CRYPTO_remalloc(a: pointer; length: longint; const f: PCharacter; line: integer): pointer; cdecl;
procedure CRYPTO_free(str: pointer); cdecl;
// High level memory management function
function OPENSSL_malloc(length: longint): pointer;
function OPENSSL_realloc(address: PCharacter; length: longint): pointer;
function OPENSSL_remalloc(var address: pointer; length: longint): pointer;
procedure OPENSSL_free(address: pointer); cdecl;

// Big number function
function BN_new(): pBIGNUM; cdecl;
procedure BN_init(bn: pBIGNUM); cdecl;
procedure BN_clear(bn: pBIGNUM); cdecl;
procedure BN_free(bn: pBIGNUM); cdecl;
procedure BN_clear_free(bn: pBIGNUM); cdecl;
procedure BN_set_params(mul, high, low, mont: integer); cdecl;
function BN_get_params(which: integer): integer; cdecl;

function BN_options: PCharacter; cdecl;

function BN_CTX_new: pBN_CTX; cdecl;
procedure BN_CTX_init(ctx: pBN_CTX); cdecl;
procedure BN_CTX_start(ctx: pBN_CTX); cdecl;
function BN_CTX_get(ctx: pBN_CTX): pBIGNUM; cdecl;
procedure BN_CTX_end(ctx: pBN_CTX); cdecl;
procedure BN_CTX_free(ctx: pBN_CTX); cdecl;

function BN_MONT_CTX_new: pBN_MONT_CTX; cdecl;
procedure BN_MONT_CTX_init(m_ctx: pBN_MONT_CTX); cdecl;
function BN_MONT_CTX_set(m_ctx: pBN_MONT_CTX;
    const modulus: pBIGNUM; ctx: pBN_CTX): integer; cdecl;
function BN_MONT_CTX_copy(_to: pBN_MONT_CTX; from: pBN_MONT_CTX): pBN_MONT_CTX; cdecl;
procedure BN_MONT_CTX_free(m_ctx: pBN_MONT_CTX); cdecl;
function BN_mod_mul_montgomery(r, a, b: pBIGNUM; m_ctx: pBN_MONT_CTX; ctx: pBN_CTX): integer; cdecl;
function BN_from_montgomery(r, a: pBIGNUM; m_ctx: pBN_MONT_CTX; ctx: pBN_CTX): integer; cdecl;
function BN_to_montgomery(r, a: pBIGNUM; m_ctx: pBN_MONT_CTX; ctx: pBN_CTX): integer;

procedure BN_RECP_CTX_init(recp: pBN_RECP_CTX); cdecl;
function BN_RECP_CTX_set(recp: pBN_RECP_CTX; const rdiv: pBIGNUM; ctx: pBN_CTX): integer; cdecl;
function BN_RECP_CTX_new: pBN_RECP_CTX; cdecl;
procedure BN_RECP_CTX_free(recp: pBN_RECP_CTX); cdecl;
function BN_div_recp(dv, rem, a: pBIGNUM; recp: pBN_RECP_CTX; ctx: pBN_CTX): integer; cdecl;
function BN_mod_mul_reciprocal(r, a, b: pBIGNUM; recp: pBN_RECP_CTX; ctx: pBN_CTX): integer; cdecl;

function BN_BLINDING_new(a: pBIGNUM; Ai: pBIGNUM; _mod: pBIGNUM): pBN_BLINDING; cdecl;
function BN_BLINDING_update(b: pBN_BLINDING; ctx: pBN_CTX): pBN_BLINDING; cdecl;
procedure BN_BLINDING_free(b: pBN_BLINDING); cdecl;
function BN_BLINDING_convert(n: pBIGNUM; r: pBN_BLINDING; ctx: pBN_CTX): integer; cdecl;
function BN_BLINDING_invert(n: pBIGNUM; b: pBN_BLINDING; ctx: pBN_CTX): integer; cdecl;

function BN_copy(_to: pBIGNUM; const from: pBIGNUM): pBIGNUM; cdecl;
function BN_dup(const from: pBIGNUM): pBIGNUM; cdecl;

// Helper: convert standard Delphi integer in big-endian integer
function int2bin(n: integer): integer;

function BN_bn2bin(const n: pBIGNUM; _to: pointer): integer; cdecl;
function BN_bin2bn(const _from: pointer; len: integer; ret: pBIGNUM): pBIGNUM; cdecl;

function BN_bn2hex(const n: pBIGNUM): PCharacter; cdecl;
function BN_bn2dec(const n: pBIGNUM): PCharacter; cdecl;
function BN_hex2bn(var n: pBIGNUM; const str: PCharacter): integer; cdecl;
function BN_dec2bn(var n: pBIGNUM; const str: PCharacter): integer; cdecl;
function BN_bn2mpi(const a: pBIGNUM; _to: pointer): integer; cdecl;
function BN_mpi2bn(s: pointer; len: integer; ret: pBIGNUM): pBIGNUM; cdecl;
function BN_print(fp: pBIO; const a: pointer): integer; cdecl;
//function BN_print_fp(FILE *fp, const BIGNUM *a): integer; cdecl;

function BN_zero(n: pBIGNUM): integer;
function BN_one(n: pBIGNUM): integer;
function BN_value_one(): pBIGNUM; cdecl;
function BN_set_word(n: pBIGNUM; w: cardinal): integer; cdecl;
function BN_get_word(n: pBIGNUM): cardinal; cdecl;

function BN_cmp(a: pBIGNUM; b: pBIGNUM): integer; cdecl;
function BN_ucmp(a: pBIGNUM; b: pBIGNUM): integer; cdecl;
//function BN_is_zero(a: pBIGNUM): boolean;
//function BN_is_one(a: pBIGNUM): boolean;
//function BN_is_word(a: pBIGNUM; w: BN_ULONG): boolean;
//function BN_is_odd(a: pBIGNUM): boolean;

function BN_num_bytes(const a: pBIGNUM): integer;
function BN_num_bits(const a: pBIGNUM): integer; cdecl;
function BN_num_bits_word(w: BN_ULONG): integer; cdecl;

function BN_add(r: pBIGNUM; const a, b: pBIGNUM): integer; cdecl;
function BN_sub(r: pBIGNUM; const a, b: pBIGNUM): integer; cdecl;
function BN_uadd(r: pBIGNUM; const a, b: pBIGNUM): integer; cdecl;
function BN_usub(r: pBIGNUM; const a, b: pBIGNUM): integer; cdecl;
function BN_mul(r: pBIGNUM; a: pBIGNUM; b: pBIGNUM; ctx: pBN_CTX): integer; cdecl;
function BN_sqr(r: pBIGNUM; a: pBIGNUM; ctx: pBN_CTX): integer; cdecl;
function BN_div(dv: pBIGNUM; rem: pBIGNUM; const a, d: pBIGNUM; ctx: pBN_CTX): integer; cdecl;
// BN_mod redefined as BN_div in some DLL version
function BN_mod(rem: pBIGNUM; const a, m: pBIGNUM; ctx: pBN_CTX): integer;
function BN_exp(r: pBIGNUM; a: pBIGNUM; p: pBIGNUM; ctx: pBN_CTX): integer; cdecl;
function BN_mod_exp(r, a: pBIGNUM; const p, m: pBIGNUM; ctx: pBN_CTX): integer; cdecl;
function BN_gcd(r: pBIGNUM; a: pBIGNUM; b: pBIGNUM; ctx: pBN_CTX): integer; cdecl;
// BN_nnmod requires OpenSSL >= 0.9.7
function BN_nnmod(rem: pBIGNUM; const a: pBIGNUM; const m: pBIGNUM; ctx: pBN_CTX): integer; cdecl;
// BN_mod_add requires OpenSSL >= 0.9.7
function BN_mod_add(r: pBIGNUM; a: pBIGNUM; b: pBIGNUM; const m: pBIGNUM;
    ctx: pBN_CTX): integer; cdecl;
// BN_mod_sub requires OpenSSL >= 0.9.7
function BN_mod_sub(r: pBIGNUM; a: pBIGNUM; b: pBIGNUM; const m: pBIGNUM;
    ctx: pBN_CTX): integer; cdecl;
// BN_mod_mul requires OpenSSL >= 0.9.7
function BN_mod_mul(ret, a, b: pBIGNUM; const m: pBIGNUM; ctx: pBN_CTX): integer; cdecl;
// BN_mod_sqr requires OpenSSL >= 0.9.7
function BN_mod_sqr(r: pBIGNUM; a: pBIGNUM; const m: pBIGNUM; ctx: pBN_CTX): integer; cdecl;
function BN_reciprocal(r, m: pBIGNUM; ctx: pBN_CTX): integer; cdecl;

function BN_mod_exp2_mont(r, a1, p1, a2, p2, m: pBIGNUM;
    ctx: pBN_CTX; m_ctx: pBN_MONT_CTX): integer; cdecl;
function BN_mod_exp_mont(r, a: pBIGNUM; const p, m: pBIGNUM;
    ctx: pBN_CTX; m_ctx: pBN_MONT_CTX): integer; cdecl;
function BN_mod_exp_mont_word(r: pBIGNUM; a: BN_ULONG; const p, m: pBIGNUM;
    ctx: pBN_CTX; m_ctx: pBN_MONT_CTX): integer; cdecl;
function BN_mod_exp_simple(r, a, p, m: pBIGNUM; ctx: pBN_CTX): integer; cdecl;
function BN_mod_exp_recp(r: pBIGNUM; const a, p, m: pBIGNUM; ctx: pBN_CTX): integer; cdecl;
function BN_mod_inverse(ret, a: pBIGNUM; const n: pBIGNUM; ctx: pBN_CTX): pBIGNUM; cdecl;

function BN_add_word(a: pBIGNUM; w: BN_ULONG): integer; cdecl;  // Adds w to a ("a+=w").
function BN_sub_word(a: pBIGNUM; w: BN_ULONG): integer; cdecl;  // Subtracts w from a ("a-=w").
function BN_mul_word(a: pBIGNUM; w: BN_ULONG): integer; cdecl;  // Multiplies a and w ("a*=b").
function BN_div_word(a: pBIGNUM; w: BN_ULONG): BN_ULONG; cdecl;  // Divides a by w ("a/=w") and returns the remainder.
function BN_mod_word(const a: pBIGNUM; w: BN_ULONG): BN_ULONG; cdecl;  // Returns the remainder of a divided by w ("a%m").
function bn_mul_words(rp, ap: pBN_ULONG; num: integer; w: BN_ULONG): BN_ULONG; cdecl;
function bn_mul_add_words(rp, ap: pBN_ULONG; num: integer; w: BN_ULONG): BN_ULONG; cdecl;
procedure bn_sqr_words(rp, ap: pBN_ULONG; num: integer); cdecl;
function bn_div_words(h, l, d: BN_ULONG): BN_ULONG; cdecl;
function bn_add_words(rp, ap, bp: pBN_ULONG; num: integer): BN_ULONG; cdecl;
function bn_sub_words(rp, ap, bp: pBN_ULONG; num: integer): BN_ULONG; cdecl;
function bn_expand2(a: pBIGNUM; n: integer): pBIGNUM; cdecl;

function BN_set_bit(a: pBIGNUM; n: integer): integer; cdecl;
function BN_clear_bit(a: pBIGNUM; n: integer): integer; cdecl;
function BN_is_bit_set(const a: pBIGNUM; n: integer): integer; cdecl;
function BN_mask_bits(a: pBIGNUM; n: integer): integer; cdecl;
function BN_lshift(r: pBIGNUM; const a: pBIGNUM; n: integer): integer; cdecl;
function BN_lshift1(r: pBIGNUM; a: pBIGNUM): integer; cdecl;
function BN_rshift(r: pBIGNUM; const a: pBIGNUM; n: integer): integer; cdecl;
function BN_rshift1(r: pBIGNUM; a: pBIGNUM): integer; cdecl;

function BN_generate_prime(ret: pBIGNUM; num, safe: integer; add, rem: pBIGNUM;
    progress: TProgressCallbackFunction; cb_arg: pointer): pBIGNUM; cdecl;
function BN_is_prime(const a: pBIGNUM; checks: integer;
    progress: TProgressCallbackFunction; ctx: pBN_CTX; cb_arg: pointer): integer; cdecl;
function BN_is_prime_fasttest(const a: pBIGNUM; checks: integer;
    progress: TProgressCallbackFunction; ctx: pBN_CTX; cb_arg: pointer;
    do_trial_division: integer): integer; cdecl;

function BN_rand(rnd: pBIGNUM; bits, top, bottom: integer): integer; cdecl;
function BN_pseudo_rand(rnd: pBIGNUM; bits, top, bottom: integer): integer; cdecl;
function BN_rand_range(rnd, range: pBIGNUM): integer; cdecl;
// BN_pseudo_rand_range requires OpenSSL >= 0.9.6c
function BN_pseudo_rand_range(rnd, range: pBIGNUM): integer; cdecl;
function BN_bntest_rand(rnd: pBIGNUM; bits, top, bottom: integer): integer; cdecl;

function BN_to_ASN1_INTEGER(bn: pBIGNUM; ai: pASN1_INTEGER): pASN1_INTEGER; cdecl;
function BN_to_ASN1_ENUMERATED(bn: pBIGNUM; ai: pASN1_ENUMERATED): pASN1_ENUMERATED; cdecl;

// ASN.1 functions
function ASN1_IA5STRING_new: pASN1_IA5STRING; cdecl;
procedure ASN1_INTEGER_free(x: pASN1_IA5STRING); cdecl;
function ASN1_INTEGER_get(a: pointer): longint; cdecl;
procedure ASN1_STRING_set_default_mask(mask: cardinal); cdecl;
function ASN1_STRING_get_default_mask: cardinal; cdecl;
function ASN1_TIME_print(fp: pBIO; a: pASN1_TIME): integer; cdecl;

// OBJ functions
function OBJ_obj2nid(asn1_object: pointer): integer; cdecl;
function OBJ_txt2nid(s: PCharacter): integer; cdecl;
function OBJ_txt2obj(s: PCharacter; no_name: integer): integer; cdecl;

// safestack functions
function sk_new_null: pointer; cdecl;
procedure sk_free(st: pointer); cdecl;
function sk_push(st: pointer; val: pointer): integer; cdecl;
function sk_num(st: pointer): integer; cdecl;
function sk_value(st: pointer; i: integer): pointer; cdecl;

// BIO functions
function BIO_new(_type: pBIO_METHOD): pBIO; cdecl;
function BIO_new_file(const filename: PCharacter; const mode: PCharacter): pBIO; cdecl;
function BIO_set(a: pBIO; _type: pBIO_METHOD): integer; cdecl;
function BIO_free(a: pBIO): integer; cdecl;
procedure BIO_vfree(a: pBIO); cdecl;
procedure BIO_free_all(a: pBIO); cdecl;
function BIO_push(b: pBIO; append: pBIO): pBIO; cdecl;
function BIO_pop(b: pBIO): pBIO; cdecl;
function BIO_ctrl(bp: pBIO; cmd: Integer; larg: Longint;
    parg: Pointer): Longint; cdecl;

function BIO_read(b: pBIO; buf: pointer; len: integer): integer; cdecl;
function BIO_gets(b: pBIO; buf: PCharacter; size: integer): integer; cdecl;
function BIO_write(b: pBIO; const buf: pointer; len: integer): integer; cdecl;
function BIO_puts(b: pBIO; const buf: PCharacter): integer; cdecl;
function BIO_flush(b: pBIO): integer;

function BIO_reset(bp: pBIO): integer;
function BIO_eof(bp: pBIO): integer;
function BIO_set_close(bp: pBIO; c: integer): integer;
function BIO_get_close(bp: pBIO): integer;
function BIO_pending(bp: pBIO): integer;
function BIO_wpending(bp: pBIO): integer;
function BIO_read_filename(bp: pBIO; filename: PCharacter): integer;
function BIO_write_filename(bp: pBIO; filename: PCharacter): integer;
function BIO_append_filename(bp: pBIO; filename: PCharacter): integer;
function BIO_rw_filename(bp: pBIO; filename: PCharacter): integer;

function BIO_s_mem: pBIO_METHOD; cdecl;
function BIO_f_base64: pBIO_METHOD; cdecl;
procedure BIO_set_mem_eof_return(b: pBIO; v: integer); cdecl;
function BIO_get_mem_data(b: pBIO; var pp: PCharacter): integer; cdecl;  // long ??
procedure BIO_set_mem_buf(b: pBIO; bm: pBUF_MEM; c: integer); cdecl;
procedure BIO_get_mem_ptr(b: pBIO; var pp: pBUF_MEM); cdecl;
function BIO_new_mem_buf(buf: pointer; len: integer): pBIO; cdecl;
function BIO_s_file: pBIO_METHOD; cdecl;

function BIO_get_md_ctx(bp: pBIO; mdcp: Pointer): Longint;

// Internal to DER and DER to internal conversion functions
function i2d_ASN1_TIME(a: pASN1_TIME; pp: PCharacter): integer; cdecl;
function d2i_ASN1_TIME(var a: pASN1_TIME; pp: PCharacter; length: longint): pASN1_TIME; cdecl;
function d2i_X509_REQ_bio(bp: pBIO; req: pX509_REQ): pX509_REQ; cdecl;
function i2d_X509_REQ_bio(bp: pBIO; req: pX509_REQ): integer; cdecl;
function d2i_X509_bio(bp: pBIO; x509: pX509): pX509; cdecl;
function i2d_X509_bio(bp: pBIO; x509: pX509): integer; cdecl;
function d2i_PrivateKey_bio(bp: pBIO; var a: pEVP_PKEY): pEVP_PKEY; cdecl;
function i2d_PrivateKey_bio(bp: pBIO; pkey: pEVP_PKEY): integer; cdecl;
function d2i_PUBKEY_bio(bp: pBIO; var a: pEVP_PKEY): pEVP_PKEY; cdecl;
function i2d_PUBKEY_bio(bp: pBIO; pkey: pEVP_PKEY): integer; cdecl;
function d2i_PKCS12_bio(bp: pBIO; pkcs12: pPKCS12): pPKCS12; cdecl;
function i2d_PKCS12_bio(bp: pBIO; pkcs12: pPKCS12): integer; cdecl;
function d2i_PKCS7(var a: pPKCS7; pp: pointer; length: longint): pPKCS7; cdecl;
function d2i_PKCS7_bio(bp: pBIO; p7: pPKCS7): pPKCS7; cdecl;
function i2d_PKCS7_bio(bp: pBIO; p7: pPKCS7): integer; cdecl;
function d2i_PKCS8_bio(bp: pBIO; p8: pX509_SIG): pX509_SIG; cdecl;
function d2i_PKCS8_PRIV_KEY_INFO(var a: pPKCS8_Priv_Key_Info;
    pp: PCharacter; Length: LongInt): pPKCS8_Priv_Key_Info; cdecl;
function d2i_DSAPrivateKey_bio(bp: pBIO; dsa: pDSA): pDSA; cdecl;
function i2d_DSAPrivateKey_bio(bp: pBIO; dsa: pDSA): integer; cdecl;
function d2i_RSAPrivateKey_bio(bp: pBIO; rsa: pRSA): pRSA; cdecl;
function i2d_RSAPrivateKey_bio(bp: pBIO; rsa: pRSA): integer; cdecl;

// Internal to ASN.1 and ASN.1 to internal conversion functions
function i2a_ASN1_INTEGER(bp: pBIO; a: pASN1_INTEGER): integer; cdecl;
function a2i_ASN1_INTEGER(bp: pBIO; bs: pASN1_INTEGER; buf: PCharacter;
    size: integer): integer; cdecl;

// Hash functions
function EVP_md_null: pEVP_MD; cdecl;
function EVP_md2: pEVP_MD; cdecl;
function EVP_md5: pEVP_MD; cdecl;
function EVP_sha: pEVP_MD; cdecl;
function EVP_sha1: pEVP_MD; cdecl;
function EVP_dss: pEVP_MD; cdecl;
function EVP_dss1: pEVP_MD; cdecl;
function EVP_mdc2: pEVP_MD; cdecl;
function EVP_ripemd160: pEVP_MD; cdecl;
function EVP_get_digestbyname(const name: PCharacter): pEVP_MD; cdecl;

procedure EVP_DigestInit(ctx: pEVP_MD_CTX; const _type: pEVP_MD); cdecl;
procedure EVP_DigestUpdate(ctx: pEVP_MD_CTX; const d: Pointer; cnt: cardinal); cdecl;
procedure EVP_DigestFinal(ctx: pEVP_MD_CTX; md: PCharacter; var s: cardinal); cdecl;
procedure EVP_SignInit(ctx: pEVP_MD_CTX; const _type: pEVP_MD);
procedure EVP_SignUpdate(ctx: pEVP_MD_CTX; const d: Pointer; cnt: cardinal);
function EVP_SignFinal(ctx: pEVP_MD_CTX; sig: pointer; var s: cardinal;
    key: pEVP_PKEY): integer; cdecl;
procedure EVP_VerifyInit(ctx: pEVP_MD_CTX; const _type: pEVP_MD);
procedure EVP_VerifyUpdate(ctx: pEVP_MD_CTX; const d: Pointer; cnt: cardinal);
function EVP_VerifyFinal(ctx: pEVP_MD_CTX; sigbuf: pointer;
    siglen: cardinal; pkey: pEVP_PKEY): integer;  cdecl;
function EVP_PKEY_assign(pkey: pEVP_PKEY; key_type: integer;
    key: PCharacter): integer; cdecl;
//function EVP_MD_size(e: pEVP_MD): integer;
//function EVP_MD_CTX_size(e: pEVP_MD_CTX): integer;
function EVP_MD_CTX_copy(_out: pEVP_MD_CTX; _in: pEVP_MD_CTX): integer; cdecl;

// Crypt functions
function EVP_enc_null: pEVP_CIPHER; cdecl;
function EVP_des_ecb: pEVP_CIPHER; cdecl;
function EVP_des_ede: pEVP_CIPHER; cdecl;
function EVP_des_ede3: pEVP_CIPHER; cdecl;
function EVP_des_cfb: pEVP_CIPHER; cdecl;
function EVP_des_ede_cfb: pEVP_CIPHER; cdecl;
function EVP_des_ede3_cfb: pEVP_CIPHER; cdecl;
function EVP_des_ofb: pEVP_CIPHER; cdecl;
function EVP_des_ede_ofb: pEVP_CIPHER; cdecl;
function EVP_des_ede3_ofb: pEVP_CIPHER; cdecl;
function EVP_des_cbc: pEVP_CIPHER; cdecl;
function EVP_des_ede_cbc: pEVP_CIPHER; cdecl;
function EVP_des_ede3_cbc: pEVP_CIPHER; cdecl;
function EVP_desx_cbc: pEVP_CIPHER; cdecl;
function EVP_idea_cbc: pEVP_CIPHER; cdecl;
function EVP_idea_cfb: pEVP_CIPHER; cdecl;
function EVP_idea_ecb: pEVP_CIPHER; cdecl;
function EVP_idea_ofb: pEVP_CIPHER; cdecl;
function EVP_get_cipherbyname(name: PCharacter): pEVP_CIPHER; cdecl;

// EVP Key functions
function EVP_PKEY_new: pEVP_PKEY; cdecl;
procedure EVP_PKEY_free(key: pEVP_PKEY); cdecl;
function EVP_PKEY_type(keytype: integer): integer; cdecl;
function EVP_PKEY_assign_RSA(key: pEVP_PKEY; rsa: pRSA): integer; cdecl;
function EVP_PKEY_assign_DSA(key: pEVP_PKEY; dsa: pDSA): integer; cdecl;
function EVP_PKEY_assign_DH(key: pEVP_PKEY; dh: pDH): integer; cdecl;
function EVP_PKEY_assign_EC_KEY(key: pEVP_PKEY; ec: pEC_KEY): integer; cdecl;
function EVP_PKEY_set1_RSA(key: pEVP_PKEY; rsa: pRSA): integer; cdecl;
function EVP_PKEY_set1_DSA(key: pEVP_PKEY; dsa: pDSA): integer; cdecl;
function EVP_PKEY_set1_DH(key: pEVP_PKEY; dh: pDH): integer; cdecl;
function EVP_PKEY_set1_EC_KEY(key: pEVP_PKEY; ec: pEC_KEY): integer; cdecl;
function EVP_PKEY_size(key: pEVP_PKEY): integer; cdecl;
function EVP_PKEY_get1_RSA(key: pEVP_PKEY): pRSA; cdecl;
function EVP_PKEY_get1_DSA(key: pEVP_PKEY): pDSA; cdecl;
function EVP_PKEY_get1_DH(key: pEVP_PKEY): pDH; cdecl;
function EVP_PKEY_get1_EC_KEY(key: pEVP_PKEY): pEC_KEY; cdecl;

// Password prompt for callback function
procedure EVP_set_pw_prompt(prompt: PCharacter);
function EVP_get_pw_prompt: PCharacter;
// Default callback password function: replace if you want
function EVP_read_pw_string(buf: PCharacter; len: integer;
    const prompt: PCharacter; verify: integer): integer;

// pseudo-random number generator (PRNG) functions
procedure RAND_seed(const buf: pointer; num: integer); cdecl;
procedure RAND_add(const buf: pointer; num: integer; entropy: double); cdecl;
function RAND_status: integer; cdecl;
//function RAND_event(UINT iMsg, WPARAM wParam, LPARAM lParam): integer; cdecl;
procedure RAND_screen; cdecl;
function RAND_file_name(buf: PCharacter; size_t: cardinal): PCharacter; cdecl;
function RAND_load_file(const filename: PCharacter; max_bytes: longint): integer; cdecl;
function RAND_write_file(const filename: PCharacter): integer; cdecl;

// RSA function
function RSA_new: pRSA; cdecl;
procedure RSA_free(r: pRSA); cdecl;
function RSA_new_method(method: pRSA_METHOD): pRSA; cdecl;
function RSA_size(pkey: pRSA): integer; cdecl;
function RSA_generate_key(bits: integer; exp: Cardinal;
    progress: TProgressCallbackFunction; cb_arg: pointer):pRSA; cdecl;
function RSA_check_key(arg0: pRSA): integer; cdecl;
function RSA_public_encrypt(flen: integer; from: PCharacter; _to: PCharacter; rsa: pRSA; padding: integer): integer; cdecl;
function RSA_private_encrypt(flen: integer; from: PCharacter; _to: PCharacter; rsa: pRSA; padding: integer): integer; cdecl;
function RSA_public_decrypt(flen: integer; from: PCharacter; _to: PCharacter; rsa: pRSA; padding: integer): integer; cdecl;
function RSA_private_decrypt(flen: integer; from: PCharacter; _to: PCharacter; rsa: pRSA; padding: integer): integer; cdecl;
function RSA_flags(r: pRSA): integer; cdecl;
procedure RSA_set_default_method(meth: pRSA_METHOD); cdecl;
function RSA_get_default_method: pRSA_METHOD; cdecl;
function RSA_get_method(rsa: pRSA): pRSA_METHOD; cdecl;
function RSA_set_method(rsa: pRSA; meth: pRSA_METHOD): pRSA_METHOD; cdecl;
function RSA_memory_lock(r: pRSA):integer; cdecl;
function RSA_PKCS1_SSLeay: pRSA_METHOD; cdecl;
procedure ERR_load_RSA_strings; cdecl;

function DSA_new: pDSA; cdecl;
procedure DSA_free(r: pDSA); cdecl;
function DSA_generate_parameters(bits: integer; seed: pointer; seed_len: integer;
    var counter_ret: integer; var h_ret: cardinal;
    progress: TProgressCallbackFunction; cb_arg: Pointer): pDSA; cdecl;
function DSA_generate_key(a: pDSA): integer; cdecl;

// X.509 names (DN)
function X509_NAME_oneline(a: pX509_NAME; buf: PCharacter; size: integer): PCharacter; cdecl;
function X509_NAME_print_ex(bp: pBIO; x: pX509_NAME; indent: integer; flags: Cardinal): Integer; cdecl;
function X509_NAME_new: pX509_NAME; cdecl;
procedure X509_NAME_free(x:pX509_NAME) cdecl;
function X509_NAME_add_entry_by_txt(name: pX509_NAME; field: PCharacter;
    asn1_type: integer;	bytes: pointer; len, loc, pos: integer): integer; cdecl;
function X509_NAME_get_entry(name: pX509_NAME; loc: integer): pX509_NAME_ENTRY; cdecl;
function X509_NAME_get_text_by_NID(name: pX509_NAME; nid: integer; buf: PCharacter;
    len: integer): integer; cdecl;

// X.509 function
function X509_new: pX509; cdecl;
procedure X509_free(a: pX509); cdecl;
function X509_print(bp: pBIO; x: pX509): integer; cdecl;
function X509_set_version(x: pX509; version: longint): integer; cdecl;
function X509_get_version(x: pX509): integer;
function X509_get_serialNumber(x: pX509): pASN1_INTEGER; cdecl;
function X509_load_cert_file(ctx: pX509_LOOKUP; const filename: PCharacter;
    _type: integer): integer; cdecl;
function X509_get_issuer_name(a: pX509): pX509_NAME; cdecl;
function X509_get_subject_name(a: pX509): pX509_NAME; cdecl;
function X509_get_notBefore(a: pX509): pASN1_TIME;
function X509_get_notAfter(a: pX509): pASN1_TIME;
function X509_get1_email(x: pX509): pSTACK; cdecl;
function X509_get_pubkey(a: pX509): pEVP_PKEY; cdecl;
function X509_check_private_key(x509: pX509; pkey: pEVP_PKEY): integer; cdecl;
function X509_check_purpose(x: pX509; id: integer; ca: integer): integer; cdecl;
function X509_issuer_and_serial_cmp(a: pX509; b: pX509): integer; cdecl;
function X509_issuer_and_serial_hash(a: pX509): cardinal; cdecl;
function X509_gmtime_adj(s: pASN1_TIME; adj: longint): pASN1_TIME; cdecl;
function X509_verify_cert(ctx: pX509_STORE_CTX): integer; cdecl;
function X509_verify_cert_error_string(n: longint): PCharacter; cdecl;
procedure X509_email_free(sk: pSTACK); cdecl;
function X509_get_ext(x: pX509; loc: integer): pX509_EXTENSION; cdecl;
function X509_get_ext_by_NID(x: pX509; nid, lastpos: integer): integer; cdecl;
function X509_get_ext_d2i(x: pX509; nid: integer; var crit,
    idx: integer): pointer; cdecl;
function X509V3_EXT_d2i(ext: pX509_EXTENSION): pointer; cdecl;
function X509V3_EXT_i2d(ext_nid: integer; crit: integer; ext_struc: pointer):
    pX509_EXTENSION; cdecl;
function X509V3_EXT_conf_nid(conf: pointer; ctx: pointer;
    ext_nid: integer; value: PCharacter): pX509_EXTENSION; cdecl;

function X509_sign(x: pX509; pkey: pEVP_PKEY; const md: pEVP_MD): integer; cdecl;
function X509_digest(x: pX509; const _type: pEVP_MD; md: pointer; var mdlen: cardinal): integer; cdecl;
function X509_set_issuer_name(x: pX509; name: pX509_NAME): integer; cdecl;
function X509_set_subject_name(x: pX509; name: pX509_NAME): integer; cdecl;
procedure X509V3_set_ctx(ctx: pX509V3_CTX; issuer: pX509; subject: pX509;
    req: pX509_REQ; crl: pX509_CRL; flags: integer);
procedure X509_SIG_free(a: pX509_SIG); cdecl;

function X509_PUBKEY_get(key: pointer): pEVP_PKEY; cdecl;

function X509_REQ_new: pX509_REQ; cdecl;
procedure X509_REQ_free(req: pX509_REQ); cdecl;
function X509_REQ_set_version(req: pX509_REQ; version: longint): integer; cdecl;
function X509_REQ_get_version(req: pX509_REQ): integer;
function X509_REQ_set_subject_name(req: pX509_REQ; name: pX509_NAME): integer; cdecl;
function X509_REQ_get_subject_name(req: pX509_REQ): pX509_NAME;
function X509_REQ_add1_attr_by_txt(req: pX509_REQ; attrname: PCharacter;
    asn1_type: integer; bytes: pointer; len: integer): integer; cdecl;
function X509_REQ_add_extensions(req: pX509_REQ;
    exts: pSTACK_OFX509_EXTENSION): integer; cdecl;
function X509_REQ_set_pubkey(req: pX509_REQ; pkey: pEVP_PKEY): integer; cdecl;
function X509_REQ_get_pubkey(req: pX509_REQ): pEVP_PKEY; cdecl;
function X509_REQ_sign(req: pX509_REQ; pkey: pEVP_PKEY; const md: pEVP_MD): integer; cdecl;

// X.509 collections
function X509_STORE_new: pX509_STORE; cdecl;
procedure X509_STORE_free(v: pX509_STORE); cdecl;
function X509_STORE_add_cert(ctx: pX509_STORE; x: pX509): integer; cdecl;
function X509_STORE_add_lookup(v: pX509_STORE; m: pX509_LOOKUP_METHOD):
    pX509_LOOKUP; cdecl;
function X509_STORE_CTX_new: pX509_STORE_CTX; cdecl;
procedure X509_STORE_CTX_free(ctx: pX509_STORE); cdecl;
procedure X509_STORE_CTX_init(ctx: pX509_STORE_CTX; store: pX509_STORE;
    x509: pX509; chain: pSTACK_OFX509); cdecl;
function X509_STORE_CTX_get_current_cert(ctx: pX509_STORE_CTX): pX509; cdecl;
function X509_STORE_CTX_get_error(ctx: pX509_STORE_CTX): integer; cdecl;
function X509_STORE_CTX_get_error_depth(ctx: pX509_STORE_CTX): integer; cdecl;

function X509_LOOKUP_new(method: pX509_LOOKUP_METHOD): pX509_LOOKUP; cdecl;
function X509_LOOKUP_init(ctx: pX509_LOOKUP): integer; cdecl;
procedure X509_LOOKUP_free(ctx: pX509_LOOKUP); cdecl;
function X509_LOOKUP_ctrl(ctx: pX509_LOOKUP; cmd: integer; const argc: PCharacter;
	argl: longint; ret: pointer): integer; cdecl;
function X509_LOOKUP_file: pX509_LOOKUP_METHOD; cdecl;

// PEM functions
function PEM_read_bio_RSAPrivateKey(bp: pBIO; var x: pRSA;
    cb: TPWCallbackFunction; u: pointer): pRSA; cdecl;
function PEM_write_bio_RSAPrivateKey(bp: pBIO; x: pRSA; const enc: pEVP_CIPHER;
    kstr: PCharacter; klen: integer; cb: TPWCallbackFunction;
    u: pointer): integer; cdecl;
function PEM_read_bio_RSAPublicKey(bp: pBIO; var x: pRSA;
    cb: TPWCallbackFunction; u: pointer): pRSA; cdecl;
function PEM_write_bio_RSAPublicKey(bp: pBIO; x: pRSA): integer; cdecl;

function PEM_read_bio_DSAPrivateKey(bp: pBIO; var dsa: pDSA;
    cb: TPWCallbackFunction; data: pointer): pDSA; cdecl;
function PEM_write_bio_DSAPrivateKey(bp: pBIO; dsa: pDSA; const enc: pEVP_CIPHER;
    kstr: PCharacter; klen: integer; cb: TPWCallbackFunction;
    data: pointer): integer; cdecl;

function PEM_read_bio_PUBKEY(bp: pBIO; var x: pEVP_PKEY;
    cb: TPWCallbackFunction; u: pointer): pEVP_PKEY; cdecl;
function PEM_write_bio_PUBKEY(bp: pBIO; x: pEVP_PKEY): integer; cdecl;

function PEM_read_bio_X509(bp: pBIO; var x: pX509; cb: TPWCallbackFunction;
    u: pointer): pX509; cdecl;
function PEM_write_bio_X509(bp: pBIO; x: pX509): integer; cdecl;
function PEM_read_bio_X509_AUX(bp: pBIO; var x: pX509; cb: TPWCallbackFunction;
    u: pointer): pX509; cdecl;
function PEM_write_bio_X509_AUX(bp: pBIO; x: pX509): integer; cdecl;
function PEM_read_bio_X509_REQ(bp: pBIO; var x: pX509_REQ; cb: TPWCallbackFunction;
    u: pointer): pX509_REQ; cdecl;
function PEM_write_bio_X509_REQ(bp: pBIO; x: pX509_REQ): integer; cdecl;
function PEM_read_bio_X509_CRL(bp: pBIO; var x: pX509_CRL; cb: TPWCallbackFunction;
    u: pointer): pX509_CRL; cdecl;
function PEM_write_bio_X509_CRL(bp: pBIO; x: pX509_CRL): integer; cdecl;

function PEM_read_bio_PrivateKey(bp: pBIO; var x: pEVP_PKEY;
    cb: TPWCallbackFunction; u: pointer): pEVP_PKEY; cdecl;
function PEM_write_bio_PrivateKey(bp: pBIO; x: pEVP_PKEY;
    const enc: pEVP_CIPHER; kstr: PCharacter; klen: Integer; cb: TPWCallbackFunction;
    u: pointer): integer; cdecl;
function PEM_write_bio_PKCS7(bp: pBIO; x: pPKCS7): integer; cdecl;

// PKCS#5 functions
function PKCS5_PBKDF2_HMAC_SHA1(pass: PCharacter; passlen: integer;
    salt: pointer; saltlen: integer; iter: integer;
    keylen: integer; u: pointer): integer; cdecl;

// PKCS#7 functions
function PKCS7_sign(signcert: pX509; pkey: pEVP_PKEY; certs: pointer;
    data: pBIO; flags: integer): pPKCS7; cdecl;
function PKCS7_get_signer_info(p7: pPKCS7): pSTACK_OFPKCS7_SIGNER_INFO; cdecl;
function PKCS7_verify(p7: pPKCS7; certs: pointer; store: pSTACK_OFX509;
    indata: pBIO; _out: pBIO; flags: integer): integer; cdecl;
function PKCS7_get0_signers(p7: pPKCS7; certs: pSTACK_OFX509;
    flags: integer): pSTACK_OFX509; cdecl;
function PKCS7_signatureVerify(bio: pBIO; p7: pPKCS7; si: pPKCS7_SIGNER_INFO;
    x509: pX509): integer; cdecl;
function PKCS7_encrypt(certs: pSTACK_OFX509; _in: pBIO;
    cipher: pEVP_CIPHER; flags: integer): pPKCS7; cdecl;
function PKCS7_decrypt(p7: pPKCS7; pkey: pEVP_PKEY; cert: pX509;
    data: pBIO; flags: integer): integer; cdecl;
procedure PKCS7_free(p7: pPKCS7); cdecl;
function PKCS7_ctrl(p7: pPKCS7; cmd: integer; larg: longint;
    parg: PCharacter): longint; cdecl;
function PKCS7_get_detached(p7: pPKCS7): pointer;
function PKCS7_dataInit(p7: pPKCS7; bio: pBIO): pBIO; cdecl;
// PKCS#7 DER/PEM to internal conversion function
{
    d2i_PKCS7_DIGEST                        @737
    d2i_PKCS7_ENCRYPT                       @738
    d2i_PKCS7_ENC_CONTENT                   @739
    d2i_PKCS7_ENVELOPE                      @740
	 d2i_PKCS7_ISSUER_AND_SERIAL             @741
    d2i_PKCS7_RECIP_INFO                    @742
    d2i_PKCS7_SIGNED                        @743
    d2i_PKCS7_SIGNER_INFO                   @744
    d2i_PKCS7_SIGN_ENVELOPE                 @745 }

function EVP_PKCS82PKEY(p8 : pPKCS8_Priv_Key_Info) : pEVP_PKEY; cdecl;
function PKCS8_decrypt(p8: pX509_SIG; Pass: PCharacter; PassLen: integer): pPKCS8_Priv_Key_Info; cdecl;
procedure PKCS8_PRIV_KEY_INFO_free(var a: pPKCS8_Priv_Key_Info); cdecl;

function PKCS12_new: pPKCS12; cdecl;
procedure PKCS12_free(a: pPKCS12); cdecl;
function PKCS12_parse(p12: pPKCS12; pass: PCharacter; var pkey: pEVP_PKEY;
    var cert: pX509; var ca: pSTACK_OFX509): integer; cdecl;

function AES_set_decrypt_key(userKey: PCharacter; bits: integer; key: pAES_KEY): integer; cdecl;
procedure AES_cbc_encrypt(buffer: PCharacter; u: PCharacter; length: longint;
    key: pAES_KEY; ivec: pointer; enc: integer); cdecl;

function PEM_read_bio_PKCS7(bp: pBIO; data: pointer;
    cb: TPWCallbackFunction; u: pointer): pPKCS7; cdecl;

// SMIME function
function SMIME_write_PKCS7(bp: pBIO; p7: pPKCS7; data: pBIO;
    flags: integer): integer; cdecl;
function SMIME_read_PKCS7(bp: pBIO; var bcont: pBIO): pPKCS7; cdecl;

implementation

uses
  {$IFDEF FPC} 
   dynlibs, 
  {$ELSE} 
   {$IFDEF MSWINDOWS}
    Windows, 
   {$ENDIF}
  {$ENDIF}
  SysUtils;

const
  {$IFDEF MSWINDOWS}
	LIBEAY_DLL_NAME = 'libeay32.dll';  
  {$ELSE}
	LIBEAY_DLL_NAME = 'libcrypto';
  {$ENDIF}

type
  TOpenSSL_InitFunction = procedure; cdecl;

function SSLeay: cardinal; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function SSLeay_version; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
// OpenSSL_add_all_algorithms prototype changed between OpenSSL version 0.9.6 and 0.9.7
// See: http://www.openssl.org/news/changelog.html
procedure OpenSSL_add_all_algorithms_old; external LIBEAY_DLL_NAME name 'OpenSSL_add_all_algorithms' {$IFDEF USE_DELAYED}delayed{$ENDIF};
procedure OpenSSL_add_all_algorithms_noconf; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

procedure OpenSSL_add_all_algorithms;
var
  hLibeayDLL: THandle;
  Add_all_algorithms_procedure: TOpenSSL_InitFunction;
  LibPointer : Pointer ;
begin
{$IFDEF FPC}
 hLibeayDLL := dynlibs.LoadLibrary(LIBEAY_DLL_NAME) ;
{$ELSE}
 hLibeayDLL := GetModuleHandle(LIBEAY_DLL_NAME);
{$ENDIF}
if hLibeayDLL = 0 then
  raise Exception.Create('libeay32.dll not loaded');
// Try to load new version
LibPointer := GetProcAddress(hLibeayDLL, 'OPENSSL_add_all_algorithms_noconf');
// Fallback to old version
if LibPointer = nil then
  LibPointer := GetProcAddress(hLibeayDLL, 'OpenSSL_add_all_algorithms');
if LibPointer <> nil then
  begin
  Add_all_algorithms_procedure := TOpenSSL_InitFunction(LibPointer) ;
  Add_all_algorithms_procedure;
  end
else
  raise Exception.Create('OpenSSL_add_all_algorithms procedure not defined in libeay32.dll');
end;

procedure OpenSSL_add_all_ciphers; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
procedure OpenSSL_add_all_digests; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
procedure EVP_cleanup; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

function ERR_get_error: cardinal; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function ERR_peek_error: cardinal; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function ERR_peek_last_error: cardinal; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function ERR_error_string; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
procedure ERR_clear_error; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
procedure ERR_load_crypto_strings; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
procedure ERR_free_strings; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

function CRYPTO_malloc; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function CRYPTO_realloc; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function CRYPTO_remalloc; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
procedure CRYPTO_free; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

// Not in DLL
function OPENSSL_malloc(length: longint): pointer;
begin
OPENSSL_malloc := CRYPTO_malloc(length, nil, 0);
end;
function OPENSSL_realloc(address: PCharacter; length: longint): pointer;
begin
OPENSSL_realloc := CRYPTO_realloc(address, length, nil, 0);
end;
function OPENSSL_remalloc(var address: pointer; length: longint): pointer;
begin
OPENSSL_remalloc := CRYPTO_remalloc(address, length, nil, 0);
end;
procedure OPENSSL_free(address: pointer);
begin
CRYPTO_free(address);
end;

function BN_new; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
procedure BN_init; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
procedure BN_clear; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
procedure BN_free; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
procedure BN_clear_free; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

procedure BN_set_params; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_get_params; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

function BN_options; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

function BN_CTX_new; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
procedure BN_CTX_init; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
procedure BN_CTX_start; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_CTX_get; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
procedure BN_CTX_end; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
procedure BN_CTX_free; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

function BN_MONT_CTX_new; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
procedure BN_MONT_CTX_init; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_MONT_CTX_set; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_MONT_CTX_copy; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
procedure BN_MONT_CTX_free; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_mod_mul_montgomery; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_from_montgomery; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_to_montgomery(r, a: pBIGNUM; m_ctx: pBN_MONT_CTX; ctx: pBN_CTX): integer;
begin
result := BN_mod_mul_montgomery(r, a, @(m_ctx^.RR), m_ctx, ctx);
end;

procedure BN_RECP_CTX_init; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_RECP_CTX_set; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_RECP_CTX_new; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
procedure BN_RECP_CTX_free; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_div_recp; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_mod_mul_reciprocal; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

function BN_BLINDING_new; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_BLINDING_update; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
procedure BN_BLINDING_free; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_BLINDING_convert; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_BLINDING_invert; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

function BN_copy; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_dup; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

function int2bin(n: integer): integer;
begin
   result := ((cardinal(n) shr 24) and $000000FF) or
        ((cardinal(n) shr 8) and $0000FF00) or
        ((cardinal(n) shl 8) and $00FF0000) or
        ((cardinal(n) shl 24) and $FF000000);
end;

function BN_bn2bin; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_bin2bn; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

function BN_bn2hex; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_bn2dec; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_hex2bn; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_dec2bn; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_bn2mpi; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_mpi2bn; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_print; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

function BN_zero(n: pBIGNUM): integer;
begin
result := BN_set_word(n, 0)
end;

function BN_one(n: pBIGNUM): integer;
begin
result := BN_set_word(n, 1)
end;

function BN_value_one; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_set_word; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_get_word; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

function BN_cmp; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_ucmp; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

//function BN_is_zero(a: pBIGNUM): boolean;
//begin
//result := (a^.top = 0) or BN_is_word(a, 0);
//end;

//function BN_is_one(a: pBIGNUM): boolean;
//begin
// define BN_is_one(a) (BN_is_word((a),1))
//result := BN_is_word(a, 1);
//end;

//function BN_is_word(a: pBIGNUM; w: BN_ULONG): boolean;
//begin
//define BN_is_word(a,w) (((a)->top == 1) && ((a)->d[0] == (BN_ULONG)(w)))
//result := (a^.top = 1) and (a^.d[0] = w);
//end;

//function BN_is_odd(a: pBIGNUM): boolean;
//begin
//result := (a^.top > 0) and ((a^.d[0] and $01) = $01);
//end;

function BN_num_bytes(const a: pBIGNUM): integer;
begin
result := (BN_num_bits(a) + 7) div 8;
end;

function BN_num_bits; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_num_bits_word; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

function BN_add; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_sub; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_uadd; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_usub; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_mul; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_sqr; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_div; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

function BN_mod(rem: pBIGNUM; const a, m: pBIGNUM; ctx: pBN_CTX): integer;
begin
result := BN_div(nil, rem, a, m, ctx);
end;

function BN_exp; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_mod_exp; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_gcd; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_nnmod; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_mod_add; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_mod_sub; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_mod_mul; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_mod_sqr; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_reciprocal; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

function BN_mod_exp2_mont; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_mod_exp_mont; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_mod_exp_mont_word; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_mod_exp_simple; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_mod_exp_recp; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_mod_inverse; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

function BN_add_word; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_sub_word; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_mul_word; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_div_word; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_mod_word; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function bn_mul_words; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function bn_mul_add_words; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
procedure bn_sqr_words; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function bn_div_words; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function bn_add_words; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function bn_sub_words; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function bn_expand2; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

function BN_set_bit; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_clear_bit; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_is_bit_set; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_mask_bits; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_lshift; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_lshift1; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_rshift; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_rshift1; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

function BN_generate_prime; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_is_prime; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_is_prime_fasttest; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

function BN_rand; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_pseudo_rand; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_rand_range; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_pseudo_rand_range; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_bntest_rand; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

function BN_to_ASN1_INTEGER; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BN_to_ASN1_ENUMERATED; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

function ASN1_IA5STRING_new; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
procedure ASN1_INTEGER_free; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function ASN1_INTEGER_get; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
procedure ASN1_STRING_set_default_mask; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function ASN1_STRING_get_default_mask; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function ASN1_TIME_print; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

function OBJ_obj2nid; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function OBJ_txt2nid; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function OBJ_txt2obj; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

function sk_new_null; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
procedure sk_free; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function sk_push; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function sk_num; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function sk_value; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

function BIO_new; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BIO_new_file; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BIO_set; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BIO_free; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
procedure BIO_vfree; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
procedure BIO_free_all; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BIO_push; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BIO_pop; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BIO_ctrl; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

function BIO_read; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BIO_gets; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BIO_write; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BIO_puts; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

function BIO_flush(b: pBIO): integer;
begin
result := BIO_ctrl(b, BIO_CTRL_FLUSH, 0, nil);
end;

function BIO_s_mem; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BIO_f_base64; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
procedure BIO_set_mem_eof_return; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

function BIO_get_mem_data(b: pBIO; var pp: PCharacter): integer; cdecl;
begin
result := BIO_ctrl(b, BIO_CTRL_INFO, 0, @pp);
end;

procedure BIO_set_mem_buf; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
procedure BIO_get_mem_ptr; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BIO_new_mem_buf; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function BIO_s_file; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

// Not in DLL
function BIO_get_md_ctx(bp: pBIO; mdcp: Pointer): Longint;
begin
result := BIO_ctrl(bp, BIO_C_GET_MD_CTX, 0, mdcp);
end;

function BIO_reset(bp: pBIO): integer;
begin
result := BIO_ctrl(bp, BIO_CTRL_RESET, 0, nil);
end;

function BIO_eof(bp: pBIO): integer;
begin
result := BIO_ctrl(bp, BIO_CTRL_EOF, 0, nil);
end;

function BIO_set_close(bp: pBIO; c: integer): integer;
begin
result := BIO_ctrl(bp, BIO_CTRL_SET_CLOSE, c, nil);
end;

function BIO_get_close(bp: pBIO): integer;
begin
result := BIO_ctrl(bp, BIO_CTRL_GET_CLOSE, 0, nil);
end;

function BIO_pending(bp: pBIO): integer;
begin
result := BIO_ctrl(bp, BIO_CTRL_PENDING, 0, nil);
end;

function BIO_wpending(bp: pBIO): integer;
begin
result := BIO_ctrl(bp, BIO_CTRL_WPENDING, 0, nil);
end;

function BIO_read_filename(bp: pBIO; filename: PCharacter): integer;
begin
result := BIO_ctrl(bp, BIO_C_SET_FILENAME, BIO_CLOSE or BIO_FP_READ, filename);
end;

function BIO_write_filename(bp: pBIO; filename: PCharacter): integer;
begin
result := BIO_ctrl(bp, BIO_C_SET_FILENAME, BIO_CLOSE or BIO_FP_WRITE, filename);
end;

function BIO_append_filename(bp: pBIO; filename: PCharacter): integer;
begin
result := BIO_ctrl(bp, BIO_C_SET_FILENAME, BIO_CLOSE or BIO_FP_APPEND, filename);
end;

function BIO_rw_filename(bp: pBIO; filename: PCharacter): integer;
begin
result := BIO_ctrl(bp, BIO_C_SET_FILENAME, BIO_CLOSE or BIO_FP_READ or BIO_FP_WRITE, filename);
end;

function i2d_ASN1_TIME; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function d2i_ASN1_TIME; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function d2i_X509_REQ_bio; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function i2d_X509_REQ_bio; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function d2i_X509_bio; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function i2d_X509_bio; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function d2i_PrivateKey_bio; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function i2d_PrivateKey_bio; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function d2i_PUBKEY_bio; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function i2d_PUBKEY_bio; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function d2i_PKCS12_bio; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function i2d_PKCS12_bio; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function d2i_PKCS7; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function d2i_PKCS7_bio; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function i2d_PKCS7_bio; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function d2i_PKCS8_bio; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function d2i_PKCS8_PRIV_KEY_INFO; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function d2i_DSAPrivateKey_bio; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function i2d_DSAPrivateKey_bio; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function d2i_RSAPrivateKey_bio; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function i2d_RSAPrivateKey_bio; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

function i2a_ASN1_INTEGER; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function a2i_ASN1_INTEGER; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

function  EVP_md_null; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function  EVP_md2; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function  EVP_md5; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function  EVP_sha; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function  EVP_sha1; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function  EVP_dss; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function  EVP_dss1; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function  EVP_mdc2; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function  EVP_ripemd160; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function  EVP_get_digestbyname; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

function EVP_PKEY_new; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
procedure EVP_PKEY_free; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function EVP_PKEY_type; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function EVP_PKEY_assign_RSA; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function EVP_PKEY_assign_DSA; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function EVP_PKEY_assign_DH; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function EVP_PKEY_assign_EC_KEY; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function EVP_PKEY_set1_RSA; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function EVP_PKEY_set1_DSA; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function EVP_PKEY_set1_DH; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function EVP_PKEY_set1_EC_KEY; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function EVP_PKEY_size; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function EVP_PKEY_get1_RSA; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function EVP_PKEY_get1_DSA; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function EVP_PKEY_get1_DH; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function EVP_PKEY_get1_EC_KEY; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

procedure EVP_DigestInit; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
procedure EVP_DigestUpdate; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
procedure EVP_DigestFinal; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

procedure EVP_SignInit(ctx: pEVP_MD_CTX; const _type: pEVP_MD);
  begin
  EVP_DigestInit(ctx, _type);
  end;

procedure EVP_SignUpdate(ctx: pEVP_MD_CTX; const d: Pointer; cnt: cardinal);
  begin
  EVP_DigestUpdate(ctx, d, cnt);
  end;

function EVP_SignFinal; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

procedure EVP_VerifyInit(ctx: pEVP_MD_CTX; const _type: pEVP_MD);
  begin
  EVP_DigestInit(ctx, _type);
  end;

procedure EVP_VerifyUpdate(ctx: pEVP_MD_CTX; const d: Pointer; cnt: cardinal);
  begin
  EVP_DigestUpdate(ctx, d, cnt);
  end;

function EVP_VerifyFinal; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function EVP_PKEY_assign; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

// Not in DLL
function EVP_MD_size(e: pEVP_MD): integer;
  begin
  result := e^.md_size;
  end;

// Not in DLL
function EVP_MD_CTX_size(e: pEVP_MD_CTX): integer;
  begin
  result := EVP_MD_size(e^.digest);
  end;

function EVP_MD_CTX_copy; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

function EVP_enc_null; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function EVP_des_ecb; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function EVP_des_ede; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function EVP_des_ede3; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function EVP_des_cfb; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function EVP_des_ede_cfb; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function EVP_des_ede3_cfb; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function EVP_des_ofb; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function EVP_des_ede_ofb; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function EVP_des_ede3_ofb; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function EVP_des_cbc; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function EVP_des_ede_cbc; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function EVP_des_ede3_cbc; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function EVP_desx_cbc; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function EVP_idea_cbc; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function EVP_idea_cfb; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function EVP_idea_ecb; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function EVP_idea_ofb; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function EVP_get_cipherbyname; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

procedure EVP_set_pw_prompt; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function EVP_get_pw_prompt; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function EVP_read_pw_string; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

procedure RAND_seed; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
procedure RAND_add; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function RAND_status; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
//function RAND_event(UINT iMsg, WPARAM wParam, LPARAM lParam): integer; cdecl; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
procedure RAND_screen; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function RAND_file_name; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function RAND_load_file; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function RAND_write_file; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

function  RSA_new; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
procedure RSA_free; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function  RSA_new_method; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function  RSA_size; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function  RSA_generate_key; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function  RSA_check_key; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function  RSA_public_encrypt; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function  RSA_private_encrypt; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function  RSA_public_decrypt; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function  RSA_private_decrypt; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function  RSA_flags; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
procedure RSA_set_default_method; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function  RSA_get_default_method; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function  RSA_get_method; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function  RSA_set_method; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function  RSA_memory_lock; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function  RSA_PKCS1_SSLeay; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
procedure ERR_load_RSA_strings; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

function DSA_new; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
procedure DSA_free; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function DSA_generate_parameters; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function DSA_generate_key; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

function X509_NAME_oneline; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function X509_NAME_print_ex; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function X509_NAME_new; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
procedure X509_NAME_free; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function X509_NAME_add_entry_by_txt; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function X509_NAME_get_entry; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function X509_NAME_get_text_by_NID; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

function X509_new; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
procedure X509_free; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function X509_print; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function X509_set_version; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

function X509_get_version(x: pX509): integer;
begin
result := ASN1_INTEGER_get(x.cert_info.version);
end;

function X509_get_serialNumber; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function X509_load_cert_file; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function X509_get_issuer_name; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function X509_get_subject_name; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

function X509_get_notBefore(a: pX509): pASN1_TIME;
  begin
  result := a.cert_info.validity.notBefore;
  end;
function X509_get_notAfter(a: pX509): pASN1_TIME;
  begin
  result := a.cert_info.validity.notAfter;
  end;

function X509_get1_email; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function X509_get_pubkey; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function X509_check_private_key; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function X509_check_purpose; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function X509_issuer_and_serial_cmp; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function X509_issuer_and_serial_hash; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function X509_gmtime_adj; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function X509_verify_cert; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function X509_verify_cert_error_string; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
procedure X509_email_free; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function X509_get_ext; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function X509_get_ext_by_NID; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function X509_get_ext_d2i; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function X509V3_EXT_d2i; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function X509V3_EXT_i2d; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function X509V3_EXT_conf_nid; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

function X509_sign; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function X509_digest; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function X509_set_issuer_name; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function X509_set_subject_name; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
procedure X509V3_set_ctx; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
procedure X509_SIG_free; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

function X509_PUBKEY_get; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

function X509_REQ_new; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
procedure X509_REQ_free; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function X509_REQ_set_version; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

function X509_REQ_get_version(req: pX509_REQ): integer;
begin
result := ASN1_INTEGER_get(req.req_info.version);
end;

function X509_REQ_set_subject_name; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

function X509_REQ_get_subject_name(req: pX509_REQ): pX509_NAME;
begin
result := req.req_info.subject;
end;

function X509_REQ_add1_attr_by_txt; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function X509_REQ_add_extensions; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function X509_REQ_set_pubkey; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function X509_REQ_get_pubkey; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function X509_REQ_sign; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

function X509_STORE_new; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
procedure X509_STORE_free; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function X509_STORE_add_cert; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function X509_STORE_add_lookup; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function X509_STORE_CTX_new; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
procedure X509_STORE_CTX_free; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
procedure X509_STORE_CTX_init; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

function X509_STORE_CTX_get_current_cert; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function X509_STORE_CTX_get_error; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function X509_STORE_CTX_get_error_depth; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

function X509_LOOKUP_new; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function X509_LOOKUP_init; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
procedure X509_LOOKUP_free; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

function X509_LOOKUP_ctrl; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function X509_LOOKUP_file; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

function PEM_read_bio_RSAPrivateKey; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function PEM_write_bio_RSAPrivateKey; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function PEM_read_bio_RSAPublicKey; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function PEM_write_bio_RSAPublicKey; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

function PEM_read_bio_DSAPrivateKey; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function PEM_write_bio_DSAPrivateKey; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

function PEM_read_bio_PUBKEY; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function PEM_write_bio_PUBKEY; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

function PEM_read_bio_X509; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function PEM_write_bio_X509; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function PEM_read_bio_X509_AUX; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function PEM_write_bio_X509_AUX; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function PEM_read_bio_X509_REQ; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function PEM_write_bio_X509_REQ; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function PEM_read_bio_X509_CRL; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function PEM_write_bio_X509_CRL; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function PEM_read_bio_PrivateKey; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function PEM_write_bio_PrivateKey; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function PEM_read_bio_PKCS7; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function PEM_write_bio_PKCS7; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

function PKCS5_PBKDF2_HMAC_SHA1; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
    
function PKCS7_sign; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function PKCS7_get_signer_info; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};  // STACK_OF(PKCS7_SIGNER_INFO)
function PKCS7_verify; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function PKCS7_get0_signers; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function PKCS7_signatureVerify; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function PKCS7_encrypt; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function PKCS7_decrypt; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
procedure PKCS7_free; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function PKCS7_ctrl; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

function PKCS7_get_detached(p7: pPKCS7): pointer;
begin
result := pointer(PKCS7_ctrl(p7, 2, 0, nil));
end;

function PKCS7_dataInit; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

function EVP_PKCS82PKEY; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
procedure PKCS8_PRIV_KEY_INFO_free; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function PKCS8_decrypt; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

function PKCS12_new; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
procedure PKCS12_free; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function PKCS12_parse;  external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

function AES_set_decrypt_key; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
procedure AES_cbc_encrypt; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

function SMIME_write_PKCS7; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};
function SMIME_read_PKCS7; external LIBEAY_DLL_NAME {$IFDEF USE_DELAYED}delayed{$ENDIF};

end.

