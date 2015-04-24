unit CAPICOM_TLB;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// $Rev: 8291 $
// File generated on 27/04/2010 15:35:18 from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\WINDOWS\system32\capicom.dll (1)
// LIBID: {BD26B198-EE42-4725-9B23-AFA912434229}
// LCID: 0
// Helpfile: 
// HelpString: CAPICOM v2.1 Type Library
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINDOWS\system32\stdole2.tlb)
// Errors:
//   Error creating palette bitmap of (TSettings) : Server C:\WINDOWS\system32\capicom.dll contains no icons
//   Error creating palette bitmap of (TCertificate) : Server C:\WINDOWS\system32\capicom.dll contains no icons
//   Error creating palette bitmap of (TCertificates) : Server C:\WINDOWS\system32\capicom.dll contains no icons
//   Error creating palette bitmap of (TChain) : Server C:\WINDOWS\system32\capicom.dll contains no icons
//   Error creating palette bitmap of (TStore) : Server C:\WINDOWS\system32\capicom.dll contains no icons
//   Error creating palette bitmap of (TAttribute) : Server C:\WINDOWS\system32\capicom.dll contains no icons
//   Error creating palette bitmap of (TSigner) : Server C:\WINDOWS\system32\capicom.dll contains no icons
//   Error creating palette bitmap of (TSignedData) : Server C:\WINDOWS\system32\capicom.dll contains no icons
//   Error creating palette bitmap of (TEnvelopedData) : Server C:\WINDOWS\system32\capicom.dll contains no icons
//   Error creating palette bitmap of (TEncryptedData) : Server C:\WINDOWS\system32\capicom.dll contains no icons
//   Error creating palette bitmap of (TOID) : Server C:\WINDOWS\system32\capicom.dll contains no icons
//   Error creating palette bitmap of (TExtendedProperty) : Server C:\WINDOWS\system32\capicom.dll contains no icons
//   Error creating palette bitmap of (TPrivateKey) : Server C:\WINDOWS\system32\capicom.dll contains no icons
//   Error creating palette bitmap of (TSignedCode) : Server C:\WINDOWS\system32\capicom.dll contains no icons
//   Error creating palette bitmap of (THashedData) : Server C:\WINDOWS\system32\capicom.dll contains no icons
//   Error creating palette bitmap of (TUtilities) : Server C:\WINDOWS\system32\capicom.dll contains no icons
// ************************************************************************ //
// *************************************************************************//
// NOTE:                                                                      
// Items guarded by $IFDEF_LIVE_SERVER_AT_DESIGN_TIME are used by properties  
// which return objects that may need to be explicitly created via a function 
// call prior to any access via the property. These items have been disabled  
// in order to prevent accidental use from within the object inspector. You   
// may enable them by defining LIVE_SERVER_AT_DESIGN_TIME or by selectively   
// removing them from the $IFDEF blocks. However, such items must still be    
// programmatically created via a method of the appropriate CoClass before    
// they can be used.                                                          
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
interface

uses Windows, ActiveX, Classes, Graphics, OleServer, StdVCL, Variants;
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  CAPICOMMajorVersion = 2;
  CAPICOMMinorVersion = 1;

  LIBID_CAPICOM: TGUID = '{BD26B198-EE42-4725-9B23-AFA912434229}';

  IID_ISettings: TGUID = '{A24104F5-46D0-4C0F-926D-665565908E91}';
  CLASS_Settings: TGUID = '{A996E48C-D3DC-4244-89F7-AFA33EC60679}';
  IID_IEKU: TGUID = '{976B7E6D-1002-4051-BFD4-824A74BD74E2}';
  CLASS_EKU: TGUID = '{8535F9A1-738A-40D0-8FB1-10CC8F74E7D3}';
  IID_IEKUs: TGUID = '{47C87CEC-8C4B-4E3C-8D22-34280274EFD1}';
  CLASS_EKUs: TGUID = '{F1800663-5BFC-4D1A-8D44-56CE02DDA34F}';
  IID_IKeyUsage: TGUID = '{41DD35A8-9FF9-45A6-9A7C-F65B2F085D1F}';
  CLASS_KeyUsage: TGUID = '{9226C95C-38BE-4CC4-B3A2-A867F5199C13}';
  IID_IExtendedKeyUsage: TGUID = '{7289D408-987D-45D1-8DEE-CF9E91C2E90E}';
  CLASS_ExtendedKeyUsage: TGUID = '{42C18607-1B4B-4126-8F1B-76E2DC7F631A}';
  IID_IBasicConstraints: TGUID = '{4E298C47-ABA6-459E-851B-993D6C626EAD}';
  CLASS_BasicConstraints: TGUID = '{C05AAC6E-3A58-45A9-A203-56952E961E48}';
  IID_ICertificateStatus: TGUID = '{AB769053-6D38-49D4-86EF-5FA85ED3AF27}';
  IID_ICertificateStatus2: TGUID = '{BF95660E-F743-4EAC-9DE5-960787A4606C}';
  IID_ICertificateStatus3: TGUID = '{A4EAB890-0786-406B-9B31-2746F31F8D87}';
  CLASS_CertificateStatus: TGUID = '{0EF24D18-BD9B-47D4-9458-E05B489FB7BA}';
  IID_IOIDs: TGUID = '{DA55E8FC-8E27-451B-AEA8-1470D80FAD42}';
  IID_IOID: TGUID = '{208E5E9B-58B1-4086-970F-161B582A846F}';
  IID_ICertificates: TGUID = '{68646716-BDA0-4046-AB82-4444BC93B84A}';
  IID_ICertificate: TGUID = '{0BBA0B86-766C-4755-A443-243FF2BD8D29}';
  IID_ICertificate2: TGUID = '{6FE450DC-AD32-48D4-A366-01EE7E0B1374}';
  IID_ICertContext: TGUID = '{9E7D3477-4F63-423E-8A45-E13B2BB851A2}';
  IID_ITemplate: TGUID = '{5F10FFCE-C922-476F-AA76-DF99D5BDFA2C}';
  IID_IPublicKey: TGUID = '{72BF9ADA-6817-4C31-B43E-25F7C7B091F4}';
  IID_IEncodedData: TGUID = '{D3D460F2-E7F3-4AF3-8EC6-8EB68C61C567}';
  IID_IPrivateKey: TGUID = '{659DEDC3-6C85-42DB-8527-EFCB21742862}';
  IID_IExtensions: TGUID = '{BC530D61-E692-4225-9E7A-07B90B45856A}';
  IID_IExtendedProperties: TGUID = '{3B096E87-6218-4A3B-A880-F6CB951E7805}';
  IID_IExtendedProperty: TGUID = '{ECB8A5C8-562C-4989-B49D-FA37D40F8FC4}';
  CLASS_Certificate: TGUID = '{9171C115-7DD9-46BA-B1E5-0ED50AFFC1B8}';
  IID_ICertificates2: TGUID = '{7B57C04B-1786-4B30-A7B6-36235CD58A14}';
  IID_ICCertificates: TGUID = '{EBDC6DC2-684D-4425-BBB7-CB4D15A088A7}';
  CLASS_Certificates: TGUID = '{3605B612-C3CF-4AB4-A426-2D853391DB2E}';
  IID_IChain: TGUID = '{77F6F881-5D3A-4F2F-AEF0-E4A2F9AA689D}';
  IID_IChain2: TGUID = '{CA65D842-2110-4073-AEE3-D0AA5F56C421}';
  IID_IChainContext: TGUID = '{B27FFB30-432E-4585-A3FD-72530108CBFD}';
  CLASS_Chain: TGUID = '{550C8FFB-4DC0-4756-828C-862E6D0AE74F}';
  IID_IStore: TGUID = '{E860EF75-1B63-4254-AF47-960DAA3DD337}';
  IID_IStore2: TGUID = '{4DA6ABC4-BDCD-4317-B650-262075B93A9C}';
  IID_IStore3: TGUID = '{F701F8EC-31C7-48FB-B621-5DE417C3A607}';
  IID_ICertStore: TGUID = '{BB3ECB9C-A83A-445C-BDB5-EFBEF691B731}';
  CLASS_Store: TGUID = '{91D221C4-0CD4-461C-A728-01D509321556}';
  IID_IAttribute: TGUID = '{B17A8D78-B5A6-45F7-BA21-01AB94B08415}';
  CLASS_Attribute: TGUID = '{54BA1E8F-818D-407F-949D-BAE1692C5C18}';
  IID_IAttributes: TGUID = '{6ADC653E-D5B9-422A-991A-A2B0119CEDAC}';
  CLASS_Attributes: TGUID = '{933013A9-64C8-4485-ACEF-4908C3692A33}';
  IID_ISigner: TGUID = '{51017B88-1913-49AD-82BE-6BB7C417DCF2}';
  IID_ISigner2: TGUID = '{625B1F55-C720-41D6-9ECF-BA59F9B85F17}';
  IID_ICSigner: TGUID = '{8F83F792-014C-4E22-BD57-5C381E622F34}';
  CLASS_Signer: TGUID = '{60A9863A-11FD-4080-850E-A8E184FC3A3C}';
  IID_ISigners: TGUID = '{5A0780F8-9E6B-4BB0-BF54-87CD9627A8B4}';
  CLASS_Signers: TGUID = '{1314C1D8-D3A8-4F8A-BED0-811FD7A8A633}';
  IID_ISignedData: TGUID = '{AE9C454B-FC65-4C10-B130-CD9B45BA948B}';
  CLASS_SignedData: TGUID = '{94AFFFCC-6C05-4814-B123-A941105AA77F}';
  IID_IAlgorithm: TGUID = '{BF3D04A9-B0DA-4153-B45E-6CCFA5AC715B}';
  CLASS_Algorithm: TGUID = '{A1EEF42F-5026-4A32-BC5C-2E552B70FD96}';
  IID_IRecipients: TGUID = '{A694C896-FC38-4C34-AE61-3B1A95984C14}';
  CLASS_Recipients: TGUID = '{96A1B8B0-8F9A-436A-84DE-E23CD6818DA5}';
  IID_IEnvelopedData: TGUID = '{F6CB6A20-CC18-4424-AE57-6F2AA3DC2059}';
  CLASS_EnvelopedData: TGUID = '{F3A12E08-EDE9-4160-8B51-334D982A9AD0}';
  IID_IEncryptedData: TGUID = '{C4778A66-972F-42E4-87C5-5CC16F7931CA}';
  CLASS_EncryptedData: TGUID = '{A440BD76-CFE1-4D46-AB1F-15F238437A3D}';
  CLASS_OID: TGUID = '{7BF3AC5C-CC84-429A-ACA5-74D916AD6B8C}';
  CLASS_OIDs: TGUID = '{FD661131-D716-4D15-A187-AEAAB161C8AD}';
  IID_INoticeNumbers: TGUID = '{EE2C051D-33A1-4157-86B4-9280E29782F2}';
  CLASS_NoticeNumbers: TGUID = '{A6FDF22A-8E00-464B-B15D-1A891D88B6ED}';
  IID_IQualifier: TGUID = '{3604C9DD-A22E-4A15-A469-8181C0C113DE}';
  CLASS_Qualifier: TGUID = '{E5F29B74-0902-4654-8A9A-21C5201DFA61}';
  IID_IQualifiers: TGUID = '{6B5A8AB6-597D-4398-AC63-1036EF546348}';
  CLASS_Qualifiers: TGUID = '{6C8006C0-F649-4783-B4A6-617DD0B270C7}';
  IID_IPolicyInformation: TGUID = '{8973710C-8411-4951-9E65-D45FD524FFDF}';
  CLASS_PolicyInformation: TGUID = '{0AAF88F4-1C22-4F65-A0E3-289D97DCE994}';
  IID_ICertificatePolicies: TGUID = '{CC7A72A7-C83A-4049-85F4-4292DE9DBFD3}';
  CLASS_CertificatePolicies: TGUID = '{988583C2-00C7-4D22-9241-E810E35EED1B}';
  CLASS_EncodedData: TGUID = '{7083C0AA-E7B9-48A4-8EFB-D6A109EBEC13}';
  IID_IExtension: TGUID = '{ED4E4ED4-FDD8-476E-AED9-5239E7948257}';
  CLASS_Extension: TGUID = '{D2359E2C-82D6-458F-BB6F-41559155E693}';
  CLASS_Extensions: TGUID = '{7C92E131-C1DC-4CA1-B02C-F513A08B41ED}';
  CLASS_ExtendedProperty: TGUID = '{9E7EA907-5810-4FCA-B817-CD0BBA8496FC}';
  CLASS_ExtendedProperties: TGUID = '{90E7143D-1A07-438D-8F85-3DBB0B73D314}';
  CLASS_Template: TGUID = '{61F0D2BD-373E-4F3C-962E-59B7C42C1B22}';
  CLASS_PublicKey: TGUID = '{301FC658-4055-4D76-9703-AA38E6D7236A}';
  IID_ICPrivateKey: TGUID = '{50F241B7-A8F2-4E0A-B982-4BD7DF0CCF3C}';
  CLASS_PrivateKey: TGUID = '{03ACC284-B757-4B8F-9951-86E600D2CD06}';
  IID_ISignedCode: TGUID = '{84FBCB95-5600-404C-9187-AC25B4CD6E94}';
  CLASS_SignedCode: TGUID = '{8C3E4934-9FA4-4693-9253-A29A05F99186}';
  IID_IHashedData: TGUID = '{9F7F23E8-06F4-42E8-B965-5CBD044BF27F}';
  CLASS_HashedData: TGUID = '{CE32ABF6-475D-41F6-BF82-D27F03E3D38B}';
  IID_IUtilities: TGUID = '{EB166CF6-2AE6-44DA-BD96-0C1635D183FE}';
  CLASS_Utilities: TGUID = '{22A85CE1-F011-4231-B9E4-7E7A0438F71B}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                    
// *********************************************************************//
// Constants for enum CAPICOM_ERROR_CODE
type
  CAPICOM_ERROR_CODE = TOleEnum;
const
  CAPICOM_E_ENCODE_INVALID_TYPE = $80880100;
  CAPICOM_E_EKU_INVALID_OID = $80880200;
  CAPICOM_E_EKU_OID_NOT_INITIALIZED = $80880201;
  CAPICOM_E_CERTIFICATE_NOT_INITIALIZED = $80880210;
  CAPICOM_E_CERTIFICATE_NO_PRIVATE_KEY = $80880211;
  CAPICOM_E_CHAIN_NOT_BUILT = $80880220;
  CAPICOM_E_STORE_NOT_OPENED = $80880230;
  CAPICOM_E_STORE_EMPTY = $80880231;
  CAPICOM_E_STORE_INVALID_OPEN_MODE = $80880232;
  CAPICOM_E_STORE_INVALID_SAVE_AS_TYPE = $80880233;
  CAPICOM_E_ATTRIBUTE_NAME_NOT_INITIALIZED = $80880240;
  CAPICOM_E_ATTRIBUTE_VALUE_NOT_INITIALIZED = $80880241;
  CAPICOM_E_ATTRIBUTE_INVALID_NAME = $80880242;
  CAPICOM_E_ATTRIBUTE_INVALID_VALUE = $80880243;
  CAPICOM_E_SIGNER_NOT_INITIALIZED = $80880250;
  CAPICOM_E_SIGNER_NOT_FOUND = $80880251;
  CAPICOM_E_SIGNER_NO_CHAIN = $80880252;
  CAPICOM_E_SIGNER_INVALID_USAGE = $80880253;
  CAPICOM_E_SIGN_NOT_INITIALIZED = $80880260;
  CAPICOM_E_SIGN_INVALID_TYPE = $80880261;
  CAPICOM_E_SIGN_NOT_SIGNED = $80880262;
  CAPICOM_E_INVALID_ALGORITHM = $80880270;
  CAPICOM_E_INVALID_KEY_LENGTH = $80880271;
  CAPICOM_E_ENVELOP_NOT_INITIALIZED = $80880280;
  CAPICOM_E_ENVELOP_INVALID_TYPE = $80880281;
  CAPICOM_E_ENVELOP_NO_RECIPIENT = $80880282;
  CAPICOM_E_ENVELOP_RECIPIENT_NOT_FOUND = $80880283;
  CAPICOM_E_ENCRYPT_NOT_INITIALIZED = $80880290;
  CAPICOM_E_ENCRYPT_INVALID_TYPE = $80880291;
  CAPICOM_E_ENCRYPT_NO_SECRET = $80880292;
  CAPICOM_E_NOT_SUPPORTED = $80880900;
  CAPICOM_E_UI_DISABLED = $80880901;
  CAPICOM_E_CANCELLED = $80880902;
  CAPICOM_E_NOT_ALLOWED = $80880903;
  CAPICOM_E_OUT_OF_RESOURCE = $80880904;
  CAPICOM_E_INTERNAL = $80880911;
  CAPICOM_E_UNKNOWN = $80880999;
  CAPICOM_E_PRIVATE_KEY_NOT_INITIALIZED = $80880300;
  CAPICOM_E_PRIVATE_KEY_NOT_EXPORTABLE = $80880301;
  CAPICOM_E_ENCODE_NOT_INITIALIZED = $80880320;
  CAPICOM_E_EXTENSION_NOT_INITIALIZED = $80880330;
  CAPICOM_E_PROPERTY_NOT_INITIALIZED = $80880340;
  CAPICOM_E_FIND_INVALID_TYPE = $80880350;
  CAPICOM_E_FIND_INVALID_PREDEFINED_POLICY = $80880351;
  CAPICOM_E_CODE_NOT_INITIALIZED = $80880360;
  CAPICOM_E_CODE_NOT_SIGNED = $80880361;
  CAPICOM_E_CODE_DESCRIPTION_NOT_INITIALIZED = $80880362;
  CAPICOM_E_CODE_DESCRIPTION_URL_NOT_INITIALIZED = $80880363;
  CAPICOM_E_CODE_INVALID_TIMESTAMP_URL = $80880364;
  CAPICOM_E_HASH_NO_DATA = $80880370;
  CAPICOM_E_INVALID_CONVERT_TYPE = $80880380;

// Constants for enum CAPICOM_ENCODING_TYPE
type
  CAPICOM_ENCODING_TYPE = TOleEnum;
const
  CAPICOM_ENCODE_BASE64 = $00000000;
  CAPICOM_ENCODE_BINARY = $00000001;
  CAPICOM_ENCODE_ANY = $FFFFFFFF;

// Constants for enum CAPICOM_EKU
type
  CAPICOM_EKU = TOleEnum;
const
  CAPICOM_EKU_OTHER = $00000000;
  CAPICOM_EKU_SERVER_AUTH = $00000001;
  CAPICOM_EKU_CLIENT_AUTH = $00000002;
  CAPICOM_EKU_CODE_SIGNING = $00000003;
  CAPICOM_EKU_EMAIL_PROTECTION = $00000004;
  CAPICOM_EKU_SMARTCARD_LOGON = $00000005;
  CAPICOM_EKU_ENCRYPTING_FILE_SYSTEM = $00000006;

// Constants for enum CAPICOM_CHECK_FLAG
type
  CAPICOM_CHECK_FLAG = TOleEnum;
const
  CAPICOM_CHECK_NONE = $00000000;
  CAPICOM_CHECK_TRUSTED_ROOT = $00000001;
  CAPICOM_CHECK_TIME_VALIDITY = $00000002;
  CAPICOM_CHECK_SIGNATURE_VALIDITY = $00000004;
  CAPICOM_CHECK_ONLINE_REVOCATION_STATUS = $00000008;
  CAPICOM_CHECK_OFFLINE_REVOCATION_STATUS = $00000010;
  CAPICOM_CHECK_COMPLETE_CHAIN = $00000020;
  CAPICOM_CHECK_NAME_CONSTRAINTS = $00000040;
  CAPICOM_CHECK_BASIC_CONSTRAINTS = $00000080;
  CAPICOM_CHECK_NESTED_VALIDITY_PERIOD = $00000100;
  CAPICOM_CHECK_ONLINE_ALL = $000001EF;
  CAPICOM_CHECK_OFFLINE_ALL = $000001F7;

// Constants for enum CAPICOM_CERT_INFO_TYPE
type
  CAPICOM_CERT_INFO_TYPE = TOleEnum;
const
  CAPICOM_CERT_INFO_SUBJECT_SIMPLE_NAME = $00000000;
  CAPICOM_CERT_INFO_ISSUER_SIMPLE_NAME = $00000001;
  CAPICOM_CERT_INFO_SUBJECT_EMAIL_NAME = $00000002;
  CAPICOM_CERT_INFO_ISSUER_EMAIL_NAME = $00000003;
  CAPICOM_CERT_INFO_SUBJECT_UPN = $00000004;
  CAPICOM_CERT_INFO_ISSUER_UPN = $00000005;
  CAPICOM_CERT_INFO_SUBJECT_DNS_NAME = $00000006;
  CAPICOM_CERT_INFO_ISSUER_DNS_NAME = $00000007;

// Constants for enum CAPICOM_STORE_LOCATION
type
  CAPICOM_STORE_LOCATION = TOleEnum;
const
  CAPICOM_MEMORY_STORE = $00000000;
  CAPICOM_LOCAL_MACHINE_STORE = $00000001;
  CAPICOM_CURRENT_USER_STORE = $00000002;
  CAPICOM_ACTIVE_DIRECTORY_USER_STORE = $00000003;
  CAPICOM_SMART_CARD_USER_STORE = $00000004;

// Constants for enum CAPICOM_STORE_OPEN_MODE
type
  CAPICOM_STORE_OPEN_MODE = TOleEnum;
const
  CAPICOM_STORE_OPEN_READ_ONLY = $00000000;
  CAPICOM_STORE_OPEN_READ_WRITE = $00000001;
  CAPICOM_STORE_OPEN_MAXIMUM_ALLOWED = $00000002;
  CAPICOM_STORE_OPEN_EXISTING_ONLY = $00000080;
  CAPICOM_STORE_OPEN_INCLUDE_ARCHIVED = $00000100;

// Constants for enum CAPICOM_ACTIVE_DIRECTORY_SEARCH_LOCATION
type
  CAPICOM_ACTIVE_DIRECTORY_SEARCH_LOCATION = TOleEnum;
const
  CAPICOM_SEARCH_ANY = $00000000;
  CAPICOM_SEARCH_GLOBAL_CATALOG = $00000001;
  CAPICOM_SEARCH_DEFAULT_DOMAIN = $00000002;

// Constants for enum CAPICOM_STORE_SAVE_AS_TYPE
type
  CAPICOM_STORE_SAVE_AS_TYPE = TOleEnum;
const
  CAPICOM_STORE_SAVE_AS_SERIALIZED = $00000000;
  CAPICOM_STORE_SAVE_AS_PKCS7 = $00000001;

// Constants for enum CAPICOM_ATTRIBUTE
type
  CAPICOM_ATTRIBUTE = TOleEnum;
const
  CAPICOM_AUTHENTICATED_ATTRIBUTE_SIGNING_TIME = $00000000;
  CAPICOM_AUTHENTICATED_ATTRIBUTE_DOCUMENT_NAME = $00000001;
  CAPICOM_AUTHENTICATED_ATTRIBUTE_DOCUMENT_DESCRIPTION = $00000002;

// Constants for enum CAPICOM_SIGNED_DATA_VERIFY_FLAG
type
  CAPICOM_SIGNED_DATA_VERIFY_FLAG = TOleEnum;
const
  CAPICOM_VERIFY_SIGNATURE_ONLY = $00000000;
  CAPICOM_VERIFY_SIGNATURE_AND_CERTIFICATE = $00000001;

// Constants for enum CAPICOM_ENCRYPTION_ALGORITHM
type
  CAPICOM_ENCRYPTION_ALGORITHM = TOleEnum;
const
  CAPICOM_ENCRYPTION_ALGORITHM_RC2 = $00000000;
  CAPICOM_ENCRYPTION_ALGORITHM_RC4 = $00000001;
  CAPICOM_ENCRYPTION_ALGORITHM_DES = $00000002;
  CAPICOM_ENCRYPTION_ALGORITHM_3DES = $00000003;
  CAPICOM_ENCRYPTION_ALGORITHM_AES = $00000004;

// Constants for enum CAPICOM_ENCRYPTION_KEY_LENGTH
type
  CAPICOM_ENCRYPTION_KEY_LENGTH = TOleEnum;
const
  CAPICOM_ENCRYPTION_KEY_LENGTH_MAXIMUM = $00000000;
  CAPICOM_ENCRYPTION_KEY_LENGTH_40_BITS = $00000001;
  CAPICOM_ENCRYPTION_KEY_LENGTH_56_BITS = $00000002;
  CAPICOM_ENCRYPTION_KEY_LENGTH_128_BITS = $00000003;
  CAPICOM_ENCRYPTION_KEY_LENGTH_192_BITS = $00000004;
  CAPICOM_ENCRYPTION_KEY_LENGTH_256_BITS = $00000005;

// Constants for enum CAPICOM_SECRET_TYPE
type
  CAPICOM_SECRET_TYPE = TOleEnum;
const
  CAPICOM_SECRET_PASSWORD = $00000000;

// Constants for enum CAPICOM_KEY_ALGORITHM
type
  CAPICOM_KEY_ALGORITHM = TOleEnum;
const
  CAPICOM_KEY_ALGORITHM_OTHER = $00000000;
  CAPICOM_KEY_ALGORITHM_RSA = $00000001;
  CAPICOM_KEY_ALGORITHM_DSS = $00000002;

// Constants for enum CAPICOM_OID
type
  CAPICOM_OID = TOleEnum;
const
  CAPICOM_OID_OTHER = $00000000;
  CAPICOM_OID_AUTHORITY_KEY_IDENTIFIER_EXTENSION = $00000001;
  CAPICOM_OID_KEY_ATTRIBUTES_EXTENSION = $00000002;
  CAPICOM_OID_CERT_POLICIES_95_EXTENSION = $00000003;
  CAPICOM_OID_KEY_USAGE_RESTRICTION_EXTENSION = $00000004;
  CAPICOM_OID_LEGACY_POLICY_MAPPINGS_EXTENSION = $00000005;
  CAPICOM_OID_SUBJECT_ALT_NAME_EXTENSION = $00000006;
  CAPICOM_OID_ISSUER_ALT_NAME_EXTENSION = $00000007;
  CAPICOM_OID_BASIC_CONSTRAINTS_EXTENSION = $00000008;
  CAPICOM_OID_SUBJECT_KEY_IDENTIFIER_EXTENSION = $00000009;
  CAPICOM_OID_KEY_USAGE_EXTENSION = $0000000A;
  CAPICOM_OID_PRIVATEKEY_USAGE_PERIOD_EXTENSION = $0000000B;
  CAPICOM_OID_SUBJECT_ALT_NAME2_EXTENSION = $0000000C;
  CAPICOM_OID_ISSUER_ALT_NAME2_EXTENSION = $0000000D;
  CAPICOM_OID_BASIC_CONSTRAINTS2_EXTENSION = $0000000E;
  CAPICOM_OID_NAME_CONSTRAINTS_EXTENSION = $0000000F;
  CAPICOM_OID_CRL_DIST_POINTS_EXTENSION = $00000010;
  CAPICOM_OID_CERT_POLICIES_EXTENSION = $00000011;
  CAPICOM_OID_POLICY_MAPPINGS_EXTENSION = $00000012;
  CAPICOM_OID_AUTHORITY_KEY_IDENTIFIER2_EXTENSION = $00000013;
  CAPICOM_OID_POLICY_CONSTRAINTS_EXTENSION = $00000014;
  CAPICOM_OID_ENHANCED_KEY_USAGE_EXTENSION = $00000015;
  CAPICOM_OID_CERTIFICATE_TEMPLATE_EXTENSION = $00000016;
  CAPICOM_OID_APPLICATION_CERT_POLICIES_EXTENSION = $00000017;
  CAPICOM_OID_APPLICATION_POLICY_MAPPINGS_EXTENSION = $00000018;
  CAPICOM_OID_APPLICATION_POLICY_CONSTRAINTS_EXTENSION = $00000019;
  CAPICOM_OID_AUTHORITY_INFO_ACCESS_EXTENSION = $0000001A;
  CAPICOM_OID_SERVER_AUTH_EKU = $00000064;
  CAPICOM_OID_CLIENT_AUTH_EKU = $00000065;
  CAPICOM_OID_CODE_SIGNING_EKU = $00000066;
  CAPICOM_OID_EMAIL_PROTECTION_EKU = $00000067;
  CAPICOM_OID_IPSEC_END_SYSTEM_EKU = $00000068;
  CAPICOM_OID_IPSEC_TUNNEL_EKU = $00000069;
  CAPICOM_OID_IPSEC_USER_EKU = $0000006A;
  CAPICOM_OID_TIME_STAMPING_EKU = $0000006B;
  CAPICOM_OID_CTL_USAGE_SIGNING_EKU = $0000006C;
  CAPICOM_OID_TIME_STAMP_SIGNING_EKU = $0000006D;
  CAPICOM_OID_SERVER_GATED_CRYPTO_EKU = $0000006E;
  CAPICOM_OID_ENCRYPTING_FILE_SYSTEM_EKU = $0000006F;
  CAPICOM_OID_EFS_RECOVERY_EKU = $00000070;
  CAPICOM_OID_WHQL_CRYPTO_EKU = $00000071;
  CAPICOM_OID_NT5_CRYPTO_EKU = $00000072;
  CAPICOM_OID_OEM_WHQL_CRYPTO_EKU = $00000073;
  CAPICOM_OID_EMBEDED_NT_CRYPTO_EKU = $00000074;
  CAPICOM_OID_ROOT_LIST_SIGNER_EKU = $00000075;
  CAPICOM_OID_QUALIFIED_SUBORDINATION_EKU = $00000076;
  CAPICOM_OID_KEY_RECOVERY_EKU = $00000077;
  CAPICOM_OID_DIGITAL_RIGHTS_EKU = $00000078;
  CAPICOM_OID_LICENSES_EKU = $00000079;
  CAPICOM_OID_LICENSE_SERVER_EKU = $0000007A;
  CAPICOM_OID_SMART_CARD_LOGON_EKU = $0000007B;
  CAPICOM_OID_PKIX_POLICY_QUALIFIER_CPS = $0000007C;
  CAPICOM_OID_PKIX_POLICY_QUALIFIER_USERNOTICE = $0000007D;

// Constants for enum CAPICOM_PROPID
type
  CAPICOM_PROPID = TOleEnum;
const
  CAPICOM_PROPID_UNKNOWN = $00000000;
  CAPICOM_PROPID_KEY_PROV_HANDLE = $00000001;
  CAPICOM_PROPID_KEY_PROV_INFO = $00000002;
  CAPICOM_PROPID_SHA1_HASH = $00000003;
  CAPICOM_PROPID_HASH_PROP = $00000003;
  CAPICOM_PROPID_MD5_HASH = $00000004;
  CAPICOM_PROPID_KEY_CONTEXT = $00000005;
  CAPICOM_PROPID_KEY_SPEC = $00000006;
  CAPICOM_PROPID_IE30_RESERVED = $00000007;
  CAPICOM_PROPID_PUBKEY_HASH_RESERVED = $00000008;
  CAPICOM_PROPID_ENHKEY_USAGE = $00000009;
  CAPICOM_PROPID_CTL_USAGE = $00000009;
  CAPICOM_PROPID_NEXT_UPDATE_LOCATION = $0000000A;
  CAPICOM_PROPID_FRIENDLY_NAME = $0000000B;
  CAPICOM_PROPID_PVK_FILE = $0000000C;
  CAPICOM_PROPID_DESCRIPTION = $0000000D;
  CAPICOM_PROPID_ACCESS_STATE = $0000000E;
  CAPICOM_PROPID_SIGNATURE_HASH = $0000000F;
  CAPICOM_PROPID_SMART_CARD_DATA = $00000010;
  CAPICOM_PROPID_EFS = $00000011;
  CAPICOM_PROPID_FORTEZZA_DATA = $00000012;
  CAPICOM_PROPID_ARCHIVED = $00000013;
  CAPICOM_PROPID_KEY_IDENTIFIER = $00000014;
  CAPICOM_PROPID_AUTO_ENROLL = $00000015;
  CAPICOM_PROPID_PUBKEY_ALG_PARA = $00000016;
  CAPICOM_PROPID_CROSS_CERT_DIST_POINTS = $00000017;
  CAPICOM_PROPID_ISSUER_PUBLIC_KEY_MD5_HASH = $00000018;
  CAPICOM_PROPID_SUBJECT_PUBLIC_KEY_MD5_HASH = $00000019;
  CAPICOM_PROPID_ENROLLMENT = $0000001A;
  CAPICOM_PROPID_DATE_STAMP = $0000001B;
  CAPICOM_PROPID_ISSUER_SERIAL_NUMBER_MD5_HASH = $0000001C;
  CAPICOM_PROPID_SUBJECT_NAME_MD5_HASH = $0000001D;
  CAPICOM_PROPID_EXTENDED_ERROR_INFO = $0000001E;
  CAPICOM_PROPID_RENEWAL = $00000040;
  CAPICOM_PROPID_ARCHIVED_KEY_HASH = $00000041;
  CAPICOM_PROPID_FIRST_RESERVED = $00000042;
  CAPICOM_PROPID_LAST_RESERVED = $00007FFF;
  CAPICOM_PROPID_FIRST_USER = $00008000;
  CAPICOM_PROPID_LAST_USER = $0000FFFF;

// Constants for enum CAPICOM_PROV_TYPE
type
  CAPICOM_PROV_TYPE = TOleEnum;
const
  CAPICOM_PROV_RSA_FULL = $00000001;
  CAPICOM_PROV_RSA_SIG = $00000002;
  CAPICOM_PROV_DSS = $00000003;
  CAPICOM_PROV_FORTEZZA = $00000004;
  CAPICOM_PROV_MS_EXCHANGE = $00000005;
  CAPICOM_PROV_SSL = $00000006;
  CAPICOM_PROV_RSA_SCHANNEL = $0000000C;
  CAPICOM_PROV_DSS_DH = $0000000D;
  CAPICOM_PROV_EC_ECDSA_SIG = $0000000E;
  CAPICOM_PROV_EC_ECNRA_SIG = $0000000F;
  CAPICOM_PROV_EC_ECDSA_FULL = $00000010;
  CAPICOM_PROV_EC_ECNRA_FULL = $00000011;
  CAPICOM_PROV_DH_SCHANNEL = $00000012;
  CAPICOM_PROV_SPYRUS_LYNKS = $00000014;
  CAPICOM_PROV_RNG = $00000015;
  CAPICOM_PROV_INTEL_SEC = $00000016;
  CAPICOM_PROV_REPLACE_OWF = $00000017;
  CAPICOM_PROV_RSA_AES = $00000018;

// Constants for enum CAPICOM_CERTIFICATE_SAVE_AS_TYPE
type
  CAPICOM_CERTIFICATE_SAVE_AS_TYPE = TOleEnum;
const
  CAPICOM_CERTIFICATE_SAVE_AS_PFX = $00000000;
  CAPICOM_CERTIFICATE_SAVE_AS_CER = $00000001;

// Constants for enum CAPICOM_CERTIFICATES_SAVE_AS_TYPE
type
  CAPICOM_CERTIFICATES_SAVE_AS_TYPE = TOleEnum;
const
  CAPICOM_CERTIFICATES_SAVE_AS_SERIALIZED = $00000000;
  CAPICOM_CERTIFICATES_SAVE_AS_PKCS7 = $00000001;
  CAPICOM_CERTIFICATES_SAVE_AS_PFX = $00000002;

// Constants for enum CAPICOM_CERTIFICATE_INCLUDE_OPTION
type
  CAPICOM_CERTIFICATE_INCLUDE_OPTION = TOleEnum;
const
  CAPICOM_CERTIFICATE_INCLUDE_CHAIN_EXCEPT_ROOT = $00000000;
  CAPICOM_CERTIFICATE_INCLUDE_WHOLE_CHAIN = $00000001;
  CAPICOM_CERTIFICATE_INCLUDE_END_ENTITY_ONLY = $00000002;

// Constants for enum CAPICOM_KEY_SPEC
type
  CAPICOM_KEY_SPEC = TOleEnum;
const
  CAPICOM_KEY_SPEC_KEYEXCHANGE = $00000001;
  CAPICOM_KEY_SPEC_SIGNATURE = $00000002;

// Constants for enum CAPICOM_KEY_LOCATION
type
  CAPICOM_KEY_LOCATION = TOleEnum;
const
  CAPICOM_CURRENT_USER_KEY = $00000000;
  CAPICOM_LOCAL_MACHINE_KEY = $00000001;

// Constants for enum CAPICOM_KEY_STORAGE_FLAG
type
  CAPICOM_KEY_STORAGE_FLAG = TOleEnum;
const
  CAPICOM_KEY_STORAGE_DEFAULT = $00000000;
  CAPICOM_KEY_STORAGE_EXPORTABLE = $00000001;
  CAPICOM_KEY_STORAGE_USER_PROTECTED = $00000002;

// Constants for enum CAPICOM_EXPORT_FLAG
type
  CAPICOM_EXPORT_FLAG = TOleEnum;
const
  CAPICOM_EXPORT_DEFAULT = $00000000;
  CAPICOM_EXPORT_IGNORE_PRIVATE_KEY_NOT_EXPORTABLE_ERROR = $00000001;

// Constants for enum CAPICOM_KEY_USAGE
type
  CAPICOM_KEY_USAGE = TOleEnum;
const
  CAPICOM_DIGITAL_SIGNATURE_KEY_USAGE = $00000080;
  CAPICOM_NON_REPUDIATION_KEY_USAGE = $00000040;
  CAPICOM_KEY_ENCIPHERMENT_KEY_USAGE = $00000020;
  CAPICOM_DATA_ENCIPHERMENT_KEY_USAGE = $00000010;
  CAPICOM_KEY_AGREEMENT_KEY_USAGE = $00000008;
  CAPICOM_KEY_CERT_SIGN_KEY_USAGE = $00000004;
  CAPICOM_OFFLINE_CRL_SIGN_KEY_USAGE = $00000002;
  CAPICOM_CRL_SIGN_KEY_USAGE = $00000002;
  CAPICOM_ENCIPHER_ONLY_KEY_USAGE = $00000001;
  CAPICOM_DECIPHER_ONLY_KEY_USAGE = $00008000;

// Constants for enum CAPICOM_CERTIFICATE_FIND_TYPE
type
  CAPICOM_CERTIFICATE_FIND_TYPE = TOleEnum;
const
  CAPICOM_CERTIFICATE_FIND_SHA1_HASH = $00000000;
  CAPICOM_CERTIFICATE_FIND_SUBJECT_NAME = $00000001;
  CAPICOM_CERTIFICATE_FIND_ISSUER_NAME = $00000002;
  CAPICOM_CERTIFICATE_FIND_ROOT_NAME = $00000003;
  CAPICOM_CERTIFICATE_FIND_TEMPLATE_NAME = $00000004;
  CAPICOM_CERTIFICATE_FIND_EXTENSION = $00000005;
  CAPICOM_CERTIFICATE_FIND_EXTENDED_PROPERTY = $00000006;
  CAPICOM_CERTIFICATE_FIND_APPLICATION_POLICY = $00000007;
  CAPICOM_CERTIFICATE_FIND_CERTIFICATE_POLICY = $00000008;
  CAPICOM_CERTIFICATE_FIND_TIME_VALID = $00000009;
  CAPICOM_CERTIFICATE_FIND_TIME_NOT_YET_VALID = $0000000A;
  CAPICOM_CERTIFICATE_FIND_TIME_EXPIRED = $0000000B;
  CAPICOM_CERTIFICATE_FIND_KEY_USAGE = $0000000C;

// Constants for enum CAPICOM_HASH_ALGORITHM
type
  CAPICOM_HASH_ALGORITHM = TOleEnum;
const
  CAPICOM_HASH_ALGORITHM_SHA1 = $00000000;
  CAPICOM_HASH_ALGORITHM_MD2 = $00000001;
  CAPICOM_HASH_ALGORITHM_MD4 = $00000002;
  CAPICOM_HASH_ALGORITHM_MD5 = $00000003;
  CAPICOM_HASH_ALGORITHM_SHA_256 = $00000004;
  CAPICOM_HASH_ALGORITHM_SHA_384 = $00000005;
  CAPICOM_HASH_ALGORITHM_SHA_512 = $00000006;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  ISettings = interface;
  ISettingsDisp = dispinterface;
  IEKU = interface;
  IEKUDisp = dispinterface;
  IEKUs = interface;
  IEKUsDisp = dispinterface;
  IKeyUsage = interface;
  IKeyUsageDisp = dispinterface;
  IExtendedKeyUsage = interface;
  IExtendedKeyUsageDisp = dispinterface;
  IBasicConstraints = interface;
  IBasicConstraintsDisp = dispinterface;
  ICertificateStatus = interface;
  ICertificateStatusDisp = dispinterface;
  ICertificateStatus2 = interface;
  ICertificateStatus2Disp = dispinterface;
  ICertificateStatus3 = interface;
  ICertificateStatus3Disp = dispinterface;
  IOIDs = interface;
  IOIDsDisp = dispinterface;
  IOID = interface;
  IOIDDisp = dispinterface;
  ICertificates = interface;
  ICertificatesDisp = dispinterface;
  ICertificate = interface;
  ICertificateDisp = dispinterface;
  ICertificate2 = interface;
  ICertificate2Disp = dispinterface;
  ICertContext = interface;
  ITemplate = interface;
  ITemplateDisp = dispinterface;
  IPublicKey = interface;
  IPublicKeyDisp = dispinterface;
  IEncodedData = interface;
  IEncodedDataDisp = dispinterface;
  IPrivateKey = interface;
  IPrivateKeyDisp = dispinterface;
  IExtensions = interface;
  IExtensionsDisp = dispinterface;
  IExtendedProperties = interface;
  IExtendedPropertiesDisp = dispinterface;
  IExtendedProperty = interface;
  IExtendedPropertyDisp = dispinterface;
  ICertificates2 = interface;
  ICertificates2Disp = dispinterface;
  ICCertificates = interface;
  IChain = interface;
  IChainDisp = dispinterface;
  IChain2 = interface;
  IChain2Disp = dispinterface;
  IChainContext = interface;
  IStore = interface;
  IStoreDisp = dispinterface;
  IStore2 = interface;
  IStore2Disp = dispinterface;
  IStore3 = interface;
  IStore3Disp = dispinterface;
  ICertStore = interface;
  IAttribute = interface;
  IAttributeDisp = dispinterface;
  IAttributes = interface;
  IAttributesDisp = dispinterface;
  ISigner = interface;
  ISignerDisp = dispinterface;
  ISigner2 = interface;
  ISigner2Disp = dispinterface;
  ICSigner = interface;
  ISigners = interface;
  ISignersDisp = dispinterface;
  ISignedData = interface;
  ISignedDataDisp = dispinterface;
  IAlgorithm = interface;
  IAlgorithmDisp = dispinterface;
  IRecipients = interface;
  IRecipientsDisp = dispinterface;
  IEnvelopedData = interface;
  IEnvelopedDataDisp = dispinterface;
  IEncryptedData = interface;
  IEncryptedDataDisp = dispinterface;
  INoticeNumbers = interface;
  INoticeNumbersDisp = dispinterface;
  IQualifier = interface;
  IQualifierDisp = dispinterface;
  IQualifiers = interface;
  IQualifiersDisp = dispinterface;
  IPolicyInformation = interface;
  IPolicyInformationDisp = dispinterface;
  ICertificatePolicies = interface;
  ICertificatePoliciesDisp = dispinterface;
  IExtension = interface;
  IExtensionDisp = dispinterface;
  ICPrivateKey = interface;
  ISignedCode = interface;
  ISignedCodeDisp = dispinterface;
  IHashedData = interface;
  IHashedDataDisp = dispinterface;
  IUtilities = interface;
  IUtilitiesDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  Settings = ISettings;
  EKU = IEKU;
  EKUs = IEKUs;
  KeyUsage = IKeyUsage;
  ExtendedKeyUsage = IExtendedKeyUsage;
  BasicConstraints = IBasicConstraints;
  CertificateStatus = ICertificateStatus3;
  Certificate = ICertificate2;
  Certificates = ICertificates2;
  Chain = IChain2;
  Store = IStore3;
  Attribute = IAttribute;
  Attributes = IAttributes;
  Signer = ISigner2;
  Signers = ISigners;
  SignedData = ISignedData;
  Algorithm = IAlgorithm;
  Recipients = IRecipients;
  EnvelopedData = IEnvelopedData;
  EncryptedData = IEncryptedData;
  OID = IOID;
  OIDs = IOIDs;
  NoticeNumbers = INoticeNumbers;
  Qualifier = IQualifier;
  Qualifiers = IQualifiers;
  PolicyInformation = IPolicyInformation;
  CertificatePolicies = ICertificatePolicies;
  EncodedData = IEncodedData;
  Extension = IExtension;
  Extensions = IExtensions;
  ExtendedProperty = IExtendedProperty;
  ExtendedProperties = IExtendedProperties;
  Template = ITemplate;
  PublicKey = IPublicKey;
  PrivateKey = IPrivateKey;
  SignedCode = ISignedCode;
  HashedData = IHashedData;
  Utilities = IUtilities;


// *********************************************************************//
// Declaration of structures, unions and aliases.                         
// *********************************************************************//
  PUserType1 = ^_CRYPT_KEY_PROV_INFO; {*}
  PUserType2 = ^_CERT_KEY_CONTEXT; {*}

  _CRYPT_KEY_PROV_PARAM = packed record
    dwParam: LongWord;
    pbData: ^Byte;
    cbData: LongWord;
    dwFlags: LongWord;
  end;

  _CRYPT_KEY_PROV_INFO = packed record
    pwszContainerName: PWideChar;
    pwszProvName: PWideChar;
    dwProvType: LongWord;
    dwFlags: LongWord;
    cProvParam: LongWord;
    rgProvParam: ^_CRYPT_KEY_PROV_PARAM;
    dwKeySpec: LongWord;
  end;

  __MIDL___MIDL_itf_capicom_0001_0064_0024 = record
    case Integer of
      0: (hCryptProv: LongWord);
      1: (hNCryptKey: LongWord);
  end;

  _CERT_KEY_CONTEXT = packed record
    cbSize: LongWord;
    __MIDL____MIDL_itf_capicom_0001_00640106: __MIDL___MIDL_itf_capicom_0001_0064_0024;
    dwKeySpec: LongWord;
  end;


// *********************************************************************//
// Interface: ISettings
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A24104F5-46D0-4C0F-926D-665565908E91}
// *********************************************************************//
  ISettings = interface(IDispatch)
    ['{A24104F5-46D0-4C0F-926D-665565908E91}']
    function Get_EnablePromptForCertificateUI: WordBool; safecall;
    procedure Set_EnablePromptForCertificateUI(pVal: WordBool); safecall;
    function Get_ActiveDirectorySearchLocation: CAPICOM_ACTIVE_DIRECTORY_SEARCH_LOCATION; safecall;
    procedure Set_ActiveDirectorySearchLocation(pVal: CAPICOM_ACTIVE_DIRECTORY_SEARCH_LOCATION); safecall;
    property EnablePromptForCertificateUI: WordBool read Get_EnablePromptForCertificateUI write Set_EnablePromptForCertificateUI;
    property ActiveDirectorySearchLocation: CAPICOM_ACTIVE_DIRECTORY_SEARCH_LOCATION read Get_ActiveDirectorySearchLocation write Set_ActiveDirectorySearchLocation;
  end;

// *********************************************************************//
// DispIntf:  ISettingsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A24104F5-46D0-4C0F-926D-665565908E91}
// *********************************************************************//
  ISettingsDisp = dispinterface
    ['{A24104F5-46D0-4C0F-926D-665565908E91}']
    property EnablePromptForCertificateUI: WordBool dispid 1;
    property ActiveDirectorySearchLocation: CAPICOM_ACTIVE_DIRECTORY_SEARCH_LOCATION dispid 2;
  end;

// *********************************************************************//
// Interface: IEKU
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {976B7E6D-1002-4051-BFD4-824A74BD74E2}
// *********************************************************************//
  IEKU = interface(IDispatch)
    ['{976B7E6D-1002-4051-BFD4-824A74BD74E2}']
    function Get_Name: CAPICOM_EKU; safecall;
    procedure Set_Name(pVal: CAPICOM_EKU); safecall;
    function Get_OID: WideString; safecall;
    procedure Set_OID(const pVal: WideString); safecall;
    property Name: CAPICOM_EKU read Get_Name write Set_Name;
    property OID: WideString read Get_OID write Set_OID;
  end;

// *********************************************************************//
// DispIntf:  IEKUDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {976B7E6D-1002-4051-BFD4-824A74BD74E2}
// *********************************************************************//
  IEKUDisp = dispinterface
    ['{976B7E6D-1002-4051-BFD4-824A74BD74E2}']
    property Name: CAPICOM_EKU dispid 0;
    property OID: WideString dispid 1;
  end;

// *********************************************************************//
// Interface: IEKUs
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {47C87CEC-8C4B-4E3C-8D22-34280274EFD1}
// *********************************************************************//
  IEKUs = interface(IDispatch)
    ['{47C87CEC-8C4B-4E3C-8D22-34280274EFD1}']
    function Get_Item(Index: Integer): OleVariant; safecall;
    function Get_Count: Integer; safecall;
    function Get__NewEnum: IUnknown; safecall;
    property Item[Index: Integer]: OleVariant read Get_Item; default;
    property Count: Integer read Get_Count;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  IEKUsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {47C87CEC-8C4B-4E3C-8D22-34280274EFD1}
// *********************************************************************//
  IEKUsDisp = dispinterface
    ['{47C87CEC-8C4B-4E3C-8D22-34280274EFD1}']
    property Item[Index: Integer]: OleVariant readonly dispid 0; default;
    property Count: Integer readonly dispid 1;
    property _NewEnum: IUnknown readonly dispid -4;
  end;

// *********************************************************************//
// Interface: IKeyUsage
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {41DD35A8-9FF9-45A6-9A7C-F65B2F085D1F}
// *********************************************************************//
  IKeyUsage = interface(IDispatch)
    ['{41DD35A8-9FF9-45A6-9A7C-F65B2F085D1F}']
    function Get_IsPresent: WordBool; safecall;
    function Get_IsCritical: WordBool; safecall;
    function Get_IsDigitalSignatureEnabled: WordBool; safecall;
    function Get_IsNonRepudiationEnabled: WordBool; safecall;
    function Get_IsKeyEnciphermentEnabled: WordBool; safecall;
    function Get_IsDataEnciphermentEnabled: WordBool; safecall;
    function Get_IsKeyAgreementEnabled: WordBool; safecall;
    function Get_IsKeyCertSignEnabled: WordBool; safecall;
    function Get_IsCRLSignEnabled: WordBool; safecall;
    function Get_IsEncipherOnlyEnabled: WordBool; safecall;
    function Get_IsDecipherOnlyEnabled: WordBool; safecall;
    property IsPresent: WordBool read Get_IsPresent;
    property IsCritical: WordBool read Get_IsCritical;
    property IsDigitalSignatureEnabled: WordBool read Get_IsDigitalSignatureEnabled;
    property IsNonRepudiationEnabled: WordBool read Get_IsNonRepudiationEnabled;
    property IsKeyEnciphermentEnabled: WordBool read Get_IsKeyEnciphermentEnabled;
    property IsDataEnciphermentEnabled: WordBool read Get_IsDataEnciphermentEnabled;
    property IsKeyAgreementEnabled: WordBool read Get_IsKeyAgreementEnabled;
    property IsKeyCertSignEnabled: WordBool read Get_IsKeyCertSignEnabled;
    property IsCRLSignEnabled: WordBool read Get_IsCRLSignEnabled;
    property IsEncipherOnlyEnabled: WordBool read Get_IsEncipherOnlyEnabled;
    property IsDecipherOnlyEnabled: WordBool read Get_IsDecipherOnlyEnabled;
  end;

// *********************************************************************//
// DispIntf:  IKeyUsageDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {41DD35A8-9FF9-45A6-9A7C-F65B2F085D1F}
// *********************************************************************//
  IKeyUsageDisp = dispinterface
    ['{41DD35A8-9FF9-45A6-9A7C-F65B2F085D1F}']
    property IsPresent: WordBool readonly dispid 1;
    property IsCritical: WordBool readonly dispid 2;
    property IsDigitalSignatureEnabled: WordBool readonly dispid 3;
    property IsNonRepudiationEnabled: WordBool readonly dispid 4;
    property IsKeyEnciphermentEnabled: WordBool readonly dispid 5;
    property IsDataEnciphermentEnabled: WordBool readonly dispid 6;
    property IsKeyAgreementEnabled: WordBool readonly dispid 7;
    property IsKeyCertSignEnabled: WordBool readonly dispid 8;
    property IsCRLSignEnabled: WordBool readonly dispid 9;
    property IsEncipherOnlyEnabled: WordBool readonly dispid 10;
    property IsDecipherOnlyEnabled: WordBool readonly dispid 11;
  end;

// *********************************************************************//
// Interface: IExtendedKeyUsage
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {7289D408-987D-45D1-8DEE-CF9E91C2E90E}
// *********************************************************************//
  IExtendedKeyUsage = interface(IDispatch)
    ['{7289D408-987D-45D1-8DEE-CF9E91C2E90E}']
    function Get_IsPresent: WordBool; safecall;
    function Get_IsCritical: WordBool; safecall;
    function Get_EKUs: IEKUs; safecall;
    property IsPresent: WordBool read Get_IsPresent;
    property IsCritical: WordBool read Get_IsCritical;
    property EKUs: IEKUs read Get_EKUs;
  end;

// *********************************************************************//
// DispIntf:  IExtendedKeyUsageDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {7289D408-987D-45D1-8DEE-CF9E91C2E90E}
// *********************************************************************//
  IExtendedKeyUsageDisp = dispinterface
    ['{7289D408-987D-45D1-8DEE-CF9E91C2E90E}']
    property IsPresent: WordBool readonly dispid 1;
    property IsCritical: WordBool readonly dispid 2;
    property EKUs: IEKUs readonly dispid 3;
  end;

// *********************************************************************//
// Interface: IBasicConstraints
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4E298C47-ABA6-459E-851B-993D6C626EAD}
// *********************************************************************//
  IBasicConstraints = interface(IDispatch)
    ['{4E298C47-ABA6-459E-851B-993D6C626EAD}']
    function Get_IsPresent: WordBool; safecall;
    function Get_IsCritical: WordBool; safecall;
    function Get_IsCertificateAuthority: WordBool; safecall;
    function Get_IsPathLenConstraintPresent: WordBool; safecall;
    function Get_PathLenConstraint: Integer; safecall;
    property IsPresent: WordBool read Get_IsPresent;
    property IsCritical: WordBool read Get_IsCritical;
    property IsCertificateAuthority: WordBool read Get_IsCertificateAuthority;
    property IsPathLenConstraintPresent: WordBool read Get_IsPathLenConstraintPresent;
    property PathLenConstraint: Integer read Get_PathLenConstraint;
  end;

// *********************************************************************//
// DispIntf:  IBasicConstraintsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4E298C47-ABA6-459E-851B-993D6C626EAD}
// *********************************************************************//
  IBasicConstraintsDisp = dispinterface
    ['{4E298C47-ABA6-459E-851B-993D6C626EAD}']
    property IsPresent: WordBool readonly dispid 1;
    property IsCritical: WordBool readonly dispid 2;
    property IsCertificateAuthority: WordBool readonly dispid 3;
    property IsPathLenConstraintPresent: WordBool readonly dispid 4;
    property PathLenConstraint: Integer readonly dispid 5;
  end;

// *********************************************************************//
// Interface: ICertificateStatus
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {AB769053-6D38-49D4-86EF-5FA85ED3AF27}
// *********************************************************************//
  ICertificateStatus = interface(IDispatch)
    ['{AB769053-6D38-49D4-86EF-5FA85ED3AF27}']
    function Get_Result: WordBool; safecall;
    function Get_CheckFlag: CAPICOM_CHECK_FLAG; safecall;
    procedure Set_CheckFlag(pVal: CAPICOM_CHECK_FLAG); safecall;
    function EKU: IEKU; safecall;
    property Result: WordBool read Get_Result;
    property CheckFlag: CAPICOM_CHECK_FLAG read Get_CheckFlag write Set_CheckFlag;
  end;

// *********************************************************************//
// DispIntf:  ICertificateStatusDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {AB769053-6D38-49D4-86EF-5FA85ED3AF27}
// *********************************************************************//
  ICertificateStatusDisp = dispinterface
    ['{AB769053-6D38-49D4-86EF-5FA85ED3AF27}']
    property Result: WordBool readonly dispid 0;
    property CheckFlag: CAPICOM_CHECK_FLAG dispid 1;
    function EKU: IEKU; dispid 2;
  end;

// *********************************************************************//
// Interface: ICertificateStatus2
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {BF95660E-F743-4EAC-9DE5-960787A4606C}
// *********************************************************************//
  ICertificateStatus2 = interface(ICertificateStatus)
    ['{BF95660E-F743-4EAC-9DE5-960787A4606C}']
    function Get_VerificationTime: TDateTime; safecall;
    procedure Set_VerificationTime(pVal: TDateTime); safecall;
    function Get_UrlRetrievalTimeout: Integer; safecall;
    procedure Set_UrlRetrievalTimeout(pVal: Integer); safecall;
    function CertificatePolicies: IOIDs; safecall;
    function ApplicationPolicies: IOIDs; safecall;
    property VerificationTime: TDateTime read Get_VerificationTime write Set_VerificationTime;
    property UrlRetrievalTimeout: Integer read Get_UrlRetrievalTimeout write Set_UrlRetrievalTimeout;
  end;

// *********************************************************************//
// DispIntf:  ICertificateStatus2Disp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {BF95660E-F743-4EAC-9DE5-960787A4606C}
// *********************************************************************//
  ICertificateStatus2Disp = dispinterface
    ['{BF95660E-F743-4EAC-9DE5-960787A4606C}']
    property VerificationTime: TDateTime dispid 3;
    property UrlRetrievalTimeout: Integer dispid 4;
    function CertificatePolicies: IOIDs; dispid 5;
    function ApplicationPolicies: IOIDs; dispid 6;
    property Result: WordBool readonly dispid 0;
    property CheckFlag: CAPICOM_CHECK_FLAG dispid 1;
    function EKU: IEKU; dispid 2;
  end;

// *********************************************************************//
// Interface: ICertificateStatus3
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A4EAB890-0786-406B-9B31-2746F31F8D87}
// *********************************************************************//
  ICertificateStatus3 = interface(ICertificateStatus2)
    ['{A4EAB890-0786-406B-9B31-2746F31F8D87}']
    function Get_ValidationCertificates: ICertificates; safecall;
    property ValidationCertificates: ICertificates read Get_ValidationCertificates;
  end;

// *********************************************************************//
// DispIntf:  ICertificateStatus3Disp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A4EAB890-0786-406B-9B31-2746F31F8D87}
// *********************************************************************//
  ICertificateStatus3Disp = dispinterface
    ['{A4EAB890-0786-406B-9B31-2746F31F8D87}']
    property ValidationCertificates: ICertificates readonly dispid 7;
    property VerificationTime: TDateTime dispid 3;
    property UrlRetrievalTimeout: Integer dispid 4;
    function CertificatePolicies: IOIDs; dispid 5;
    function ApplicationPolicies: IOIDs; dispid 6;
    property Result: WordBool readonly dispid 0;
    property CheckFlag: CAPICOM_CHECK_FLAG dispid 1;
    function EKU: IEKU; dispid 2;
  end;

// *********************************************************************//
// Interface: IOIDs
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {DA55E8FC-8E27-451B-AEA8-1470D80FAD42}
// *********************************************************************//
  IOIDs = interface(IDispatch)
    ['{DA55E8FC-8E27-451B-AEA8-1470D80FAD42}']
    function Get_Item(Index: OleVariant): OleVariant; safecall;
    function Get_Count: Integer; safecall;
    function Get__NewEnum: IUnknown; safecall;
    procedure Add(const pVal: IOID); safecall;
    procedure Remove(Index: OleVariant); safecall;
    procedure Clear; safecall;
    property Item[Index: OleVariant]: OleVariant read Get_Item; default;
    property Count: Integer read Get_Count;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  IOIDsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {DA55E8FC-8E27-451B-AEA8-1470D80FAD42}
// *********************************************************************//
  IOIDsDisp = dispinterface
    ['{DA55E8FC-8E27-451B-AEA8-1470D80FAD42}']
    property Item[Index: OleVariant]: OleVariant readonly dispid 0; default;
    property Count: Integer readonly dispid 1;
    property _NewEnum: IUnknown readonly dispid -4;
    procedure Add(const pVal: IOID); dispid 2;
    procedure Remove(Index: OleVariant); dispid 3;
    procedure Clear; dispid 4;
  end;

// *********************************************************************//
// Interface: IOID
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {208E5E9B-58B1-4086-970F-161B582A846F}
// *********************************************************************//
  IOID = interface(IDispatch)
    ['{208E5E9B-58B1-4086-970F-161B582A846F}']
    function Get_Name: CAPICOM_OID; safecall;
    procedure Set_Name(pVal: CAPICOM_OID); safecall;
    function Get_FriendlyName: WideString; safecall;
    procedure Set_FriendlyName(const pVal: WideString); safecall;
    function Get_Value: WideString; safecall;
    procedure Set_Value(const pVal: WideString); safecall;
    property Name: CAPICOM_OID read Get_Name write Set_Name;
    property FriendlyName: WideString read Get_FriendlyName write Set_FriendlyName;
    property Value: WideString read Get_Value write Set_Value;
  end;

// *********************************************************************//
// DispIntf:  IOIDDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {208E5E9B-58B1-4086-970F-161B582A846F}
// *********************************************************************//
  IOIDDisp = dispinterface
    ['{208E5E9B-58B1-4086-970F-161B582A846F}']
    property Name: CAPICOM_OID dispid 0;
    property FriendlyName: WideString dispid 1;
    property Value: WideString dispid 2;
  end;

// *********************************************************************//
// Interface: ICertificates
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {68646716-BDA0-4046-AB82-4444BC93B84A}
// *********************************************************************//
  ICertificates = interface(IDispatch)
    ['{68646716-BDA0-4046-AB82-4444BC93B84A}']
    function Get_Item(Index: Integer): OleVariant; safecall;
    function Get_Count: Integer; safecall;
    function Get__NewEnum: IUnknown; safecall;
    property Item[Index: Integer]: OleVariant read Get_Item; default;
    property Count: Integer read Get_Count;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  ICertificatesDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {68646716-BDA0-4046-AB82-4444BC93B84A}
// *********************************************************************//
  ICertificatesDisp = dispinterface
    ['{68646716-BDA0-4046-AB82-4444BC93B84A}']
    property Item[Index: Integer]: OleVariant readonly dispid 0; default;
    property Count: Integer readonly dispid 1;
    property _NewEnum: IUnknown readonly dispid -4;
  end;

// *********************************************************************//
// Interface: ICertificate
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {0BBA0B86-766C-4755-A443-243FF2BD8D29}
// *********************************************************************//
  ICertificate = interface(IDispatch)
    ['{0BBA0B86-766C-4755-A443-243FF2BD8D29}']
    function Get_Version: Integer; safecall;
    function Get_SerialNumber: WideString; safecall;
    function Get_SubjectName: WideString; safecall;
    function Get_IssuerName: WideString; safecall;
    function Get_ValidFromDate: TDateTime; safecall;
    function Get_ValidToDate: TDateTime; safecall;
    function Get_Thumbprint: WideString; safecall;
    function HasPrivateKey: WordBool; safecall;
    function GetInfo(InfoType: CAPICOM_CERT_INFO_TYPE): WideString; safecall;
    function IsValid: ICertificateStatus; safecall;
    function KeyUsage: IKeyUsage; safecall;
    function ExtendedKeyUsage: IExtendedKeyUsage; safecall;
    function BasicConstraints: IBasicConstraints; safecall;
    function Export(EncodingType: CAPICOM_ENCODING_TYPE): WideString; safecall;
    procedure Import(const EncodedCertificate: WideString); safecall;
    procedure Display; safecall;
    property Version: Integer read Get_Version;
    property SerialNumber: WideString read Get_SerialNumber;
    property SubjectName: WideString read Get_SubjectName;
    property IssuerName: WideString read Get_IssuerName;
    property ValidFromDate: TDateTime read Get_ValidFromDate;
    property ValidToDate: TDateTime read Get_ValidToDate;
    property Thumbprint: WideString read Get_Thumbprint;
  end;

// *********************************************************************//
// DispIntf:  ICertificateDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {0BBA0B86-766C-4755-A443-243FF2BD8D29}
// *********************************************************************//
  ICertificateDisp = dispinterface
    ['{0BBA0B86-766C-4755-A443-243FF2BD8D29}']
    property Version: Integer readonly dispid 1;
    property SerialNumber: WideString readonly dispid 2;
    property SubjectName: WideString readonly dispid 3;
    property IssuerName: WideString readonly dispid 4;
    property ValidFromDate: TDateTime readonly dispid 5;
    property ValidToDate: TDateTime readonly dispid 6;
    property Thumbprint: WideString readonly dispid 7;
    function HasPrivateKey: WordBool; dispid 10;
    function GetInfo(InfoType: CAPICOM_CERT_INFO_TYPE): WideString; dispid 11;
    function IsValid: ICertificateStatus; dispid 12;
    function KeyUsage: IKeyUsage; dispid 13;
    function ExtendedKeyUsage: IExtendedKeyUsage; dispid 14;
    function BasicConstraints: IBasicConstraints; dispid 15;
    function Export(EncodingType: CAPICOM_ENCODING_TYPE): WideString; dispid 16;
    procedure Import(const EncodedCertificate: WideString); dispid 17;
    procedure Display; dispid 18;
  end;

// *********************************************************************//
// Interface: ICertificate2
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {6FE450DC-AD32-48D4-A366-01EE7E0B1374}
// *********************************************************************//
  ICertificate2 = interface(ICertificate)
    ['{6FE450DC-AD32-48D4-A366-01EE7E0B1374}']
    function Get_Archived: WordBool; safecall;
    procedure Set_Archived(pVal: WordBool); safecall;
    function Template: ITemplate; safecall;
    function PublicKey: IPublicKey; safecall;
    function Get_PrivateKey: IPrivateKey; safecall;
    procedure Set_PrivateKey(const pVal: IPrivateKey); safecall;
    function Extensions: IExtensions; safecall;
    function ExtendedProperties: IExtendedProperties; safecall;
    procedure Load(const FileName: WideString; const Password: WideString; 
                   KeyStorageFlag: CAPICOM_KEY_STORAGE_FLAG; KeyLocation: CAPICOM_KEY_LOCATION); safecall;
    procedure Save(const FileName: WideString; const Password: WideString; 
                   SaveAs: CAPICOM_CERTIFICATE_SAVE_AS_TYPE; 
                   IncludeOption: CAPICOM_CERTIFICATE_INCLUDE_OPTION); safecall;
    property Archived: WordBool read Get_Archived write Set_Archived;
    property PrivateKey: IPrivateKey read Get_PrivateKey write Set_PrivateKey;
  end;

// *********************************************************************//
// DispIntf:  ICertificate2Disp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {6FE450DC-AD32-48D4-A366-01EE7E0B1374}
// *********************************************************************//
  ICertificate2Disp = dispinterface
    ['{6FE450DC-AD32-48D4-A366-01EE7E0B1374}']
    property Archived: WordBool dispid 19;
    function Template: ITemplate; dispid 20;
    function PublicKey: IPublicKey; dispid 21;
    property PrivateKey: IPrivateKey dispid 22;
    function Extensions: IExtensions; dispid 23;
    function ExtendedProperties: IExtendedProperties; dispid 24;
    procedure Load(const FileName: WideString; const Password: WideString; 
                   KeyStorageFlag: CAPICOM_KEY_STORAGE_FLAG; KeyLocation: CAPICOM_KEY_LOCATION); dispid 25;
    procedure Save(const FileName: WideString; const Password: WideString; 
                   SaveAs: CAPICOM_CERTIFICATE_SAVE_AS_TYPE; 
                   IncludeOption: CAPICOM_CERTIFICATE_INCLUDE_OPTION); dispid 26;
    property Version: Integer readonly dispid 1;
    property SerialNumber: WideString readonly dispid 2;
    property SubjectName: WideString readonly dispid 3;
    property IssuerName: WideString readonly dispid 4;
    property ValidFromDate: TDateTime readonly dispid 5;
    property ValidToDate: TDateTime readonly dispid 6;
    property Thumbprint: WideString readonly dispid 7;
    function HasPrivateKey: WordBool; dispid 10;
    function GetInfo(InfoType: CAPICOM_CERT_INFO_TYPE): WideString; dispid 11;
    function IsValid: ICertificateStatus; dispid 12;
    function KeyUsage: IKeyUsage; dispid 13;
    function ExtendedKeyUsage: IExtendedKeyUsage; dispid 14;
    function BasicConstraints: IBasicConstraints; dispid 15;
    function Export(EncodingType: CAPICOM_ENCODING_TYPE): WideString; dispid 16;
    procedure Import(const EncodedCertificate: WideString); dispid 17;
    procedure Display; dispid 18;
  end;

// *********************************************************************//
// Interface: ICertContext
// Flags:     (0)
// GUID:      {9E7D3477-4F63-423E-8A45-E13B2BB851A2}
// *********************************************************************//
  ICertContext = interface(IUnknown)
    ['{9E7D3477-4F63-423E-8A45-E13B2BB851A2}']
    function Get_CertContext(out ppCertContext: Integer): HResult; stdcall;
    function Set_CertContext(ppCertContext: Integer): HResult; stdcall;
    function FreeContext(pCertContext: Integer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ITemplate
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {5F10FFCE-C922-476F-AA76-DF99D5BDFA2C}
// *********************************************************************//
  ITemplate = interface(IDispatch)
    ['{5F10FFCE-C922-476F-AA76-DF99D5BDFA2C}']
    function Get_IsPresent: WordBool; safecall;
    function Get_IsCritical: WordBool; safecall;
    function Get_Name: WideString; safecall;
    function Get_OID: IOID; safecall;
    function Get_MajorVersion: Integer; safecall;
    function Get_MinorVersion: Integer; safecall;
    property IsPresent: WordBool read Get_IsPresent;
    property IsCritical: WordBool read Get_IsCritical;
    property Name: WideString read Get_Name;
    property OID: IOID read Get_OID;
    property MajorVersion: Integer read Get_MajorVersion;
    property MinorVersion: Integer read Get_MinorVersion;
  end;

// *********************************************************************//
// DispIntf:  ITemplateDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {5F10FFCE-C922-476F-AA76-DF99D5BDFA2C}
// *********************************************************************//
  ITemplateDisp = dispinterface
    ['{5F10FFCE-C922-476F-AA76-DF99D5BDFA2C}']
    property IsPresent: WordBool readonly dispid 1;
    property IsCritical: WordBool readonly dispid 2;
    property Name: WideString readonly dispid 3;
    property OID: IOID readonly dispid 4;
    property MajorVersion: Integer readonly dispid 5;
    property MinorVersion: Integer readonly dispid 6;
  end;

// *********************************************************************//
// Interface: IPublicKey
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {72BF9ADA-6817-4C31-B43E-25F7C7B091F4}
// *********************************************************************//
  IPublicKey = interface(IDispatch)
    ['{72BF9ADA-6817-4C31-B43E-25F7C7B091F4}']
    function Get_Algorithm: IOID; safecall;
    function Get_Length: Integer; safecall;
    function Get_EncodedKey: IEncodedData; safecall;
    function Get_EncodedParameters: IEncodedData; safecall;
    property Algorithm: IOID read Get_Algorithm;
    property Length: Integer read Get_Length;
    property EncodedKey: IEncodedData read Get_EncodedKey;
    property EncodedParameters: IEncodedData read Get_EncodedParameters;
  end;

// *********************************************************************//
// DispIntf:  IPublicKeyDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {72BF9ADA-6817-4C31-B43E-25F7C7B091F4}
// *********************************************************************//
  IPublicKeyDisp = dispinterface
    ['{72BF9ADA-6817-4C31-B43E-25F7C7B091F4}']
    property Algorithm: IOID readonly dispid 0;
    property Length: Integer readonly dispid 1;
    property EncodedKey: IEncodedData readonly dispid 2;
    property EncodedParameters: IEncodedData readonly dispid 3;
  end;

// *********************************************************************//
// Interface: IEncodedData
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D3D460F2-E7F3-4AF3-8EC6-8EB68C61C567}
// *********************************************************************//
  IEncodedData = interface(IDispatch)
    ['{D3D460F2-E7F3-4AF3-8EC6-8EB68C61C567}']
    function Get_Value(EncodingType: CAPICOM_ENCODING_TYPE): WideString; safecall;
    function Format(bMultiLines: WordBool): WideString; safecall;
    function Decoder: IDispatch; safecall;
    property Value[EncodingType: CAPICOM_ENCODING_TYPE]: WideString read Get_Value; default;
  end;

// *********************************************************************//
// DispIntf:  IEncodedDataDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D3D460F2-E7F3-4AF3-8EC6-8EB68C61C567}
// *********************************************************************//
  IEncodedDataDisp = dispinterface
    ['{D3D460F2-E7F3-4AF3-8EC6-8EB68C61C567}']
    property Value[EncodingType: CAPICOM_ENCODING_TYPE]: WideString readonly dispid 0; default;
    function Format(bMultiLines: WordBool): WideString; dispid 1;
    function Decoder: IDispatch; dispid 2;
  end;

// *********************************************************************//
// Interface: IPrivateKey
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {659DEDC3-6C85-42DB-8527-EFCB21742862}
// *********************************************************************//
  IPrivateKey = interface(IDispatch)
    ['{659DEDC3-6C85-42DB-8527-EFCB21742862}']
    function Get_ContainerName: WideString; safecall;
    function Get_UniqueContainerName: WideString; safecall;
    function Get_ProviderName: WideString; safecall;
    function Get_ProviderType: CAPICOM_PROV_TYPE; safecall;
    function Get_KeySpec: CAPICOM_KEY_SPEC; safecall;
    function IsAccessible: WordBool; safecall;
    function IsProtected: WordBool; safecall;
    function IsExportable: WordBool; safecall;
    function IsRemovable: WordBool; safecall;
    function IsMachineKeyset: WordBool; safecall;
    function IsHardwareDevice: WordBool; safecall;
    procedure Open(const ContainerName: WideString; const ProviderName: WideString; 
                   ProviderType: CAPICOM_PROV_TYPE; KeySpec: CAPICOM_KEY_SPEC; 
                   StoreLocation: CAPICOM_STORE_LOCATION; bCheckExistence: WordBool); safecall;
    procedure Delete; safecall;
    property ContainerName: WideString read Get_ContainerName;
    property UniqueContainerName: WideString read Get_UniqueContainerName;
    property ProviderName: WideString read Get_ProviderName;
    property ProviderType: CAPICOM_PROV_TYPE read Get_ProviderType;
    property KeySpec: CAPICOM_KEY_SPEC read Get_KeySpec;
  end;

// *********************************************************************//
// DispIntf:  IPrivateKeyDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {659DEDC3-6C85-42DB-8527-EFCB21742862}
// *********************************************************************//
  IPrivateKeyDisp = dispinterface
    ['{659DEDC3-6C85-42DB-8527-EFCB21742862}']
    property ContainerName: WideString readonly dispid 0;
    property UniqueContainerName: WideString readonly dispid 1;
    property ProviderName: WideString readonly dispid 2;
    property ProviderType: CAPICOM_PROV_TYPE readonly dispid 3;
    property KeySpec: CAPICOM_KEY_SPEC readonly dispid 4;
    function IsAccessible: WordBool; dispid 5;
    function IsProtected: WordBool; dispid 6;
    function IsExportable: WordBool; dispid 7;
    function IsRemovable: WordBool; dispid 8;
    function IsMachineKeyset: WordBool; dispid 9;
    function IsHardwareDevice: WordBool; dispid 10;
    procedure Open(const ContainerName: WideString; const ProviderName: WideString; 
                   ProviderType: CAPICOM_PROV_TYPE; KeySpec: CAPICOM_KEY_SPEC; 
                   StoreLocation: CAPICOM_STORE_LOCATION; bCheckExistence: WordBool); dispid 11;
    procedure Delete; dispid 12;
  end;

// *********************************************************************//
// Interface: IExtensions
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {BC530D61-E692-4225-9E7A-07B90B45856A}
// *********************************************************************//
  IExtensions = interface(IDispatch)
    ['{BC530D61-E692-4225-9E7A-07B90B45856A}']
    function Get_Item(Index: OleVariant): OleVariant; safecall;
    function Get_Count: Integer; safecall;
    function Get__NewEnum: IUnknown; safecall;
    property Item[Index: OleVariant]: OleVariant read Get_Item; default;
    property Count: Integer read Get_Count;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  IExtensionsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {BC530D61-E692-4225-9E7A-07B90B45856A}
// *********************************************************************//
  IExtensionsDisp = dispinterface
    ['{BC530D61-E692-4225-9E7A-07B90B45856A}']
    property Item[Index: OleVariant]: OleVariant readonly dispid 0; default;
    property Count: Integer readonly dispid 1;
    property _NewEnum: IUnknown readonly dispid -4;
  end;

// *********************************************************************//
// Interface: IExtendedProperties
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3B096E87-6218-4A3B-A880-F6CB951E7805}
// *********************************************************************//
  IExtendedProperties = interface(IDispatch)
    ['{3B096E87-6218-4A3B-A880-F6CB951E7805}']
    function Get_Item(Index: Integer): OleVariant; safecall;
    function Get_Count: Integer; safecall;
    function Get__NewEnum: IUnknown; safecall;
    procedure Add(const pVal: IExtendedProperty); safecall;
    procedure Remove(PropID: CAPICOM_PROPID); safecall;
    property Item[Index: Integer]: OleVariant read Get_Item; default;
    property Count: Integer read Get_Count;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  IExtendedPropertiesDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3B096E87-6218-4A3B-A880-F6CB951E7805}
// *********************************************************************//
  IExtendedPropertiesDisp = dispinterface
    ['{3B096E87-6218-4A3B-A880-F6CB951E7805}']
    property Item[Index: Integer]: OleVariant readonly dispid 0; default;
    property Count: Integer readonly dispid 1;
    property _NewEnum: IUnknown readonly dispid -4;
    procedure Add(const pVal: IExtendedProperty); dispid 2;
    procedure Remove(PropID: CAPICOM_PROPID); dispid 3;
  end;

// *********************************************************************//
// Interface: IExtendedProperty
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {ECB8A5C8-562C-4989-B49D-FA37D40F8FC4}
// *********************************************************************//
  IExtendedProperty = interface(IDispatch)
    ['{ECB8A5C8-562C-4989-B49D-FA37D40F8FC4}']
    function Get_PropID: CAPICOM_PROPID; safecall;
    procedure Set_PropID(pVal: CAPICOM_PROPID); safecall;
    function Get_Value(EncodingType: CAPICOM_ENCODING_TYPE): WideString; safecall;
    procedure Set_Value(EncodingType: CAPICOM_ENCODING_TYPE; const pVal: WideString); safecall;
    property PropID: CAPICOM_PROPID read Get_PropID write Set_PropID;
    property Value[EncodingType: CAPICOM_ENCODING_TYPE]: WideString read Get_Value write Set_Value;
  end;

// *********************************************************************//
// DispIntf:  IExtendedPropertyDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {ECB8A5C8-562C-4989-B49D-FA37D40F8FC4}
// *********************************************************************//
  IExtendedPropertyDisp = dispinterface
    ['{ECB8A5C8-562C-4989-B49D-FA37D40F8FC4}']
    property PropID: CAPICOM_PROPID dispid 0;
    property Value[EncodingType: CAPICOM_ENCODING_TYPE]: WideString dispid 1;
  end;

// *********************************************************************//
// Interface: ICertificates2
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {7B57C04B-1786-4B30-A7B6-36235CD58A14}
// *********************************************************************//
  ICertificates2 = interface(ICertificates)
    ['{7B57C04B-1786-4B30-A7B6-36235CD58A14}']
    function Find(FindType: CAPICOM_CERTIFICATE_FIND_TYPE; varCriteria: OleVariant; 
                  bFindValidOnly: WordBool): ICertificates2; safecall;
    function Select(const Title: WideString; const DisplayString: WideString; bMultiSelect: WordBool): ICertificates2; safecall;
    procedure Add(const pVal: ICertificate2); safecall;
    procedure Remove(Index: OleVariant); safecall;
    procedure Clear; safecall;
    procedure Save(const FileName: WideString; const Password: WideString; 
                   SaveAs: CAPICOM_CERTIFICATES_SAVE_AS_TYPE; ExportFlag: CAPICOM_EXPORT_FLAG); safecall;
  end;

// *********************************************************************//
// DispIntf:  ICertificates2Disp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {7B57C04B-1786-4B30-A7B6-36235CD58A14}
// *********************************************************************//
  ICertificates2Disp = dispinterface
    ['{7B57C04B-1786-4B30-A7B6-36235CD58A14}']
    function Find(FindType: CAPICOM_CERTIFICATE_FIND_TYPE; varCriteria: OleVariant; 
                  bFindValidOnly: WordBool): ICertificates2; dispid 2;
    function Select(const Title: WideString; const DisplayString: WideString; bMultiSelect: WordBool): ICertificates2; dispid 3;
    procedure Add(const pVal: ICertificate2); dispid 4;
    procedure Remove(Index: OleVariant); dispid 5;
    procedure Clear; dispid 6;
    procedure Save(const FileName: WideString; const Password: WideString; 
                   SaveAs: CAPICOM_CERTIFICATES_SAVE_AS_TYPE; ExportFlag: CAPICOM_EXPORT_FLAG); dispid 7;
    property Item[Index: Integer]: OleVariant readonly dispid 0; default;
    property Count: Integer readonly dispid 1;
    property _NewEnum: IUnknown readonly dispid -4;
  end;

// *********************************************************************//
// Interface: ICCertificates
// Flags:     (512) Restricted
// GUID:      {EBDC6DC2-684D-4425-BBB7-CB4D15A088A7}
// *********************************************************************//
  ICCertificates = interface(IUnknown)
    ['{EBDC6DC2-684D-4425-BBB7-CB4D15A088A7}']
    function _ExportToStore(var hCertStore: Pointer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IChain
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {77F6F881-5D3A-4F2F-AEF0-E4A2F9AA689D}
// *********************************************************************//
  IChain = interface(IDispatch)
    ['{77F6F881-5D3A-4F2F-AEF0-E4A2F9AA689D}']
    function Get_Certificates: ICertificates; safecall;
    function Get_Status(Index: Integer): Integer; safecall;
    function Build(const pICertificate: ICertificate): WordBool; safecall;
    property Certificates: ICertificates read Get_Certificates;
    property Status[Index: Integer]: Integer read Get_Status;
  end;

// *********************************************************************//
// DispIntf:  IChainDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {77F6F881-5D3A-4F2F-AEF0-E4A2F9AA689D}
// *********************************************************************//
  IChainDisp = dispinterface
    ['{77F6F881-5D3A-4F2F-AEF0-E4A2F9AA689D}']
    property Certificates: ICertificates readonly dispid 0;
    property Status[Index: Integer]: Integer readonly dispid 1;
    function Build(const pICertificate: ICertificate): WordBool; dispid 2;
  end;

// *********************************************************************//
// Interface: IChain2
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {CA65D842-2110-4073-AEE3-D0AA5F56C421}
// *********************************************************************//
  IChain2 = interface(IChain)
    ['{CA65D842-2110-4073-AEE3-D0AA5F56C421}']
    function CertificatePolicies: IOIDs; safecall;
    function ApplicationPolicies: IOIDs; safecall;
    function ExtendedErrorInfo(Index: Integer): WideString; safecall;
  end;

// *********************************************************************//
// DispIntf:  IChain2Disp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {CA65D842-2110-4073-AEE3-D0AA5F56C421}
// *********************************************************************//
  IChain2Disp = dispinterface
    ['{CA65D842-2110-4073-AEE3-D0AA5F56C421}']
    function CertificatePolicies: IOIDs; dispid 3;
    function ApplicationPolicies: IOIDs; dispid 4;
    function ExtendedErrorInfo(Index: Integer): WideString; dispid 5;
    property Certificates: ICertificates readonly dispid 0;
    property Status[Index: Integer]: Integer readonly dispid 1;
    function Build(const pICertificate: ICertificate): WordBool; dispid 2;
  end;

// *********************************************************************//
// Interface: IChainContext
// Flags:     (0)
// GUID:      {B27FFB30-432E-4585-A3FD-72530108CBFD}
// *********************************************************************//
  IChainContext = interface(IUnknown)
    ['{B27FFB30-432E-4585-A3FD-72530108CBFD}']
    function Get_ChainContext(out pChainContext: Integer): HResult; stdcall;
    function Set_ChainContext(pChainContext: Integer): HResult; stdcall;
    function FreeContext(pChainContext: Integer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IStore
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {E860EF75-1B63-4254-AF47-960DAA3DD337}
// *********************************************************************//
  IStore = interface(IDispatch)
    ['{E860EF75-1B63-4254-AF47-960DAA3DD337}']
    function Get_Certificates: ICertificates; safecall;
    procedure Open(StoreLocation: CAPICOM_STORE_LOCATION; const StoreName: WideString; 
                   OpenMode: CAPICOM_STORE_OPEN_MODE); safecall;
    procedure Add(const pVal: ICertificate); safecall;
    procedure Remove(const pVal: ICertificate); safecall;
    function Export(SaveAs: CAPICOM_STORE_SAVE_AS_TYPE; EncodingType: CAPICOM_ENCODING_TYPE): WideString; safecall;
    procedure Import(const EncodedStore: WideString); safecall;
    property Certificates: ICertificates read Get_Certificates;
  end;

// *********************************************************************//
// DispIntf:  IStoreDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {E860EF75-1B63-4254-AF47-960DAA3DD337}
// *********************************************************************//
  IStoreDisp = dispinterface
    ['{E860EF75-1B63-4254-AF47-960DAA3DD337}']
    property Certificates: ICertificates readonly dispid 0;
    procedure Open(StoreLocation: CAPICOM_STORE_LOCATION; const StoreName: WideString; 
                   OpenMode: CAPICOM_STORE_OPEN_MODE); dispid 1;
    procedure Add(const pVal: ICertificate); dispid 2;
    procedure Remove(const pVal: ICertificate); dispid 3;
    function Export(SaveAs: CAPICOM_STORE_SAVE_AS_TYPE; EncodingType: CAPICOM_ENCODING_TYPE): WideString; dispid 4;
    procedure Import(const EncodedStore: WideString); dispid 5;
  end;

// *********************************************************************//
// Interface: IStore2
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4DA6ABC4-BDCD-4317-B650-262075B93A9C}
// *********************************************************************//
  IStore2 = interface(IStore)
    ['{4DA6ABC4-BDCD-4317-B650-262075B93A9C}']
    procedure Load(const FileName: WideString; const Password: WideString; 
                   KeyStorageFlag: CAPICOM_KEY_STORAGE_FLAG); safecall;
  end;

// *********************************************************************//
// DispIntf:  IStore2Disp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4DA6ABC4-BDCD-4317-B650-262075B93A9C}
// *********************************************************************//
  IStore2Disp = dispinterface
    ['{4DA6ABC4-BDCD-4317-B650-262075B93A9C}']
    procedure Load(const FileName: WideString; const Password: WideString; 
                   KeyStorageFlag: CAPICOM_KEY_STORAGE_FLAG); dispid 6;
    property Certificates: ICertificates readonly dispid 0;
    procedure Open(StoreLocation: CAPICOM_STORE_LOCATION; const StoreName: WideString; 
                   OpenMode: CAPICOM_STORE_OPEN_MODE); dispid 1;
    procedure Add(const pVal: ICertificate); dispid 2;
    procedure Remove(const pVal: ICertificate); dispid 3;
    function Export(SaveAs: CAPICOM_STORE_SAVE_AS_TYPE; EncodingType: CAPICOM_ENCODING_TYPE): WideString; dispid 4;
    procedure Import(const EncodedStore: WideString); dispid 5;
  end;

// *********************************************************************//
// Interface: IStore3
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F701F8EC-31C7-48FB-B621-5DE417C3A607}
// *********************************************************************//
  IStore3 = interface(IStore2)
    ['{F701F8EC-31C7-48FB-B621-5DE417C3A607}']
    function Get_Name: WideString; safecall;
    function Get_Location: CAPICOM_STORE_LOCATION; safecall;
    function Delete: WordBool; safecall;
    procedure Close; safecall;
    property Name: WideString read Get_Name;
    property Location: CAPICOM_STORE_LOCATION read Get_Location;
  end;

// *********************************************************************//
// DispIntf:  IStore3Disp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F701F8EC-31C7-48FB-B621-5DE417C3A607}
// *********************************************************************//
  IStore3Disp = dispinterface
    ['{F701F8EC-31C7-48FB-B621-5DE417C3A607}']
    property Name: WideString readonly dispid 7;
    property Location: CAPICOM_STORE_LOCATION readonly dispid 8;
    function Delete: WordBool; dispid 9;
    procedure Close; dispid 10;
    procedure Load(const FileName: WideString; const Password: WideString; 
                   KeyStorageFlag: CAPICOM_KEY_STORAGE_FLAG); dispid 6;
    property Certificates: ICertificates readonly dispid 0;
    procedure Open(StoreLocation: CAPICOM_STORE_LOCATION; const StoreName: WideString; 
                   OpenMode: CAPICOM_STORE_OPEN_MODE); dispid 1;
    procedure Add(const pVal: ICertificate); dispid 2;
    procedure Remove(const pVal: ICertificate); dispid 3;
    function Export(SaveAs: CAPICOM_STORE_SAVE_AS_TYPE; EncodingType: CAPICOM_ENCODING_TYPE): WideString; dispid 4;
    procedure Import(const EncodedStore: WideString); dispid 5;
  end;

// *********************************************************************//
// Interface: ICertStore
// Flags:     (0)
// GUID:      {BB3ECB9C-A83A-445C-BDB5-EFBEF691B731}
// *********************************************************************//
  ICertStore = interface(IUnknown)
    ['{BB3ECB9C-A83A-445C-BDB5-EFBEF691B731}']
    function Get_StoreHandle(out phCertStore: Integer): HResult; stdcall;
    function Set_StoreHandle(phCertStore: Integer): HResult; stdcall;
    function Get_StoreLocation(out pStoreLocation: CAPICOM_STORE_LOCATION): HResult; stdcall;
    function Set_StoreLocation(pStoreLocation: CAPICOM_STORE_LOCATION): HResult; stdcall;
    function CloseHandle(hCertStore: Integer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IAttribute
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B17A8D78-B5A6-45F7-BA21-01AB94B08415}
// *********************************************************************//
  IAttribute = interface(IDispatch)
    ['{B17A8D78-B5A6-45F7-BA21-01AB94B08415}']
    function Get_Name: CAPICOM_ATTRIBUTE; safecall;
    procedure Set_Name(pVal: CAPICOM_ATTRIBUTE); safecall;
    function Get_Value: OleVariant; safecall;
    procedure Set_Value(pVal: OleVariant); safecall;
    property Name: CAPICOM_ATTRIBUTE read Get_Name write Set_Name;
    property Value: OleVariant read Get_Value write Set_Value;
  end;

// *********************************************************************//
// DispIntf:  IAttributeDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B17A8D78-B5A6-45F7-BA21-01AB94B08415}
// *********************************************************************//
  IAttributeDisp = dispinterface
    ['{B17A8D78-B5A6-45F7-BA21-01AB94B08415}']
    property Name: CAPICOM_ATTRIBUTE dispid 0;
    property Value: OleVariant dispid 1;
  end;

// *********************************************************************//
// Interface: IAttributes
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {6ADC653E-D5B9-422A-991A-A2B0119CEDAC}
// *********************************************************************//
  IAttributes = interface(IDispatch)
    ['{6ADC653E-D5B9-422A-991A-A2B0119CEDAC}']
    function Get_Item(Index: Integer): OleVariant; safecall;
    function Get_Count: Integer; safecall;
    function Get__NewEnum: IUnknown; safecall;
    procedure Add(const pVal: IAttribute); safecall;
    procedure Remove(Index: Integer); safecall;
    procedure Clear; safecall;
    property Item[Index: Integer]: OleVariant read Get_Item; default;
    property Count: Integer read Get_Count;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  IAttributesDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {6ADC653E-D5B9-422A-991A-A2B0119CEDAC}
// *********************************************************************//
  IAttributesDisp = dispinterface
    ['{6ADC653E-D5B9-422A-991A-A2B0119CEDAC}']
    property Item[Index: Integer]: OleVariant readonly dispid 0; default;
    property Count: Integer readonly dispid 1;
    property _NewEnum: IUnknown readonly dispid -4;
    procedure Add(const pVal: IAttribute); dispid 2;
    procedure Remove(Index: Integer); dispid 3;
    procedure Clear; dispid 4;
  end;

// *********************************************************************//
// Interface: ISigner
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {51017B88-1913-49AD-82BE-6BB7C417DCF2}
// *********************************************************************//
  ISigner = interface(IDispatch)
    ['{51017B88-1913-49AD-82BE-6BB7C417DCF2}']
    function Get_Certificate: ICertificate; safecall;
    procedure Set_Certificate(const pVal: ICertificate); safecall;
    function Get_AuthenticatedAttributes: IAttributes; safecall;
    property Certificate: ICertificate read Get_Certificate write Set_Certificate;
    property AuthenticatedAttributes: IAttributes read Get_AuthenticatedAttributes;
  end;

// *********************************************************************//
// DispIntf:  ISignerDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {51017B88-1913-49AD-82BE-6BB7C417DCF2}
// *********************************************************************//
  ISignerDisp = dispinterface
    ['{51017B88-1913-49AD-82BE-6BB7C417DCF2}']
    property Certificate: ICertificate dispid 0;
    property AuthenticatedAttributes: IAttributes readonly dispid 1;
  end;

// *********************************************************************//
// Interface: ISigner2
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {625B1F55-C720-41D6-9ECF-BA59F9B85F17}
// *********************************************************************//
  ISigner2 = interface(ISigner)
    ['{625B1F55-C720-41D6-9ECF-BA59F9B85F17}']
    function Get_Chain: IChain; safecall;
    function Get_Options: CAPICOM_CERTIFICATE_INCLUDE_OPTION; safecall;
    procedure Set_Options(pVal: CAPICOM_CERTIFICATE_INCLUDE_OPTION); safecall;
    procedure Load(const FileName: WideString; const Password: WideString); safecall;
    property Chain: IChain read Get_Chain;
    property Options: CAPICOM_CERTIFICATE_INCLUDE_OPTION read Get_Options write Set_Options;
  end;

// *********************************************************************//
// DispIntf:  ISigner2Disp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {625B1F55-C720-41D6-9ECF-BA59F9B85F17}
// *********************************************************************//
  ISigner2Disp = dispinterface
    ['{625B1F55-C720-41D6-9ECF-BA59F9B85F17}']
    property Chain: IChain readonly dispid 2;
    property Options: CAPICOM_CERTIFICATE_INCLUDE_OPTION dispid 3;
    procedure Load(const FileName: WideString; const Password: WideString); dispid 4;
    property Certificate: ICertificate dispid 0;
    property AuthenticatedAttributes: IAttributes readonly dispid 1;
  end;

// *********************************************************************//
// Interface: ICSigner
// Flags:     (512) Restricted
// GUID:      {8F83F792-014C-4E22-BD57-5C381E622F34}
// *********************************************************************//
  ICSigner = interface(IUnknown)
    ['{8F83F792-014C-4E22-BD57-5C381E622F34}']
    function Get_AdditionalStore(out phAdditionalStore: Integer): HResult; stdcall;
    function Set_AdditionalStore(phAdditionalStore: Integer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISigners
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {5A0780F8-9E6B-4BB0-BF54-87CD9627A8B4}
// *********************************************************************//
  ISigners = interface(IDispatch)
    ['{5A0780F8-9E6B-4BB0-BF54-87CD9627A8B4}']
    function Get_Item(Index: Integer): OleVariant; safecall;
    function Get_Count: Integer; safecall;
    function Get__NewEnum: IUnknown; safecall;
    property Item[Index: Integer]: OleVariant read Get_Item; default;
    property Count: Integer read Get_Count;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  ISignersDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {5A0780F8-9E6B-4BB0-BF54-87CD9627A8B4}
// *********************************************************************//
  ISignersDisp = dispinterface
    ['{5A0780F8-9E6B-4BB0-BF54-87CD9627A8B4}']
    property Item[Index: Integer]: OleVariant readonly dispid 0; default;
    property Count: Integer readonly dispid 1;
    property _NewEnum: IUnknown readonly dispid -4;
  end;

// *********************************************************************//
// Interface: ISignedData
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {AE9C454B-FC65-4C10-B130-CD9B45BA948B}
// *********************************************************************//
  ISignedData = interface(IDispatch)
    ['{AE9C454B-FC65-4C10-B130-CD9B45BA948B}']
    procedure Set_Content(const pVal: WideString); safecall;
    function Get_Content: WideString; safecall;
    function Get_Signers: ISigners; safecall;
    function Get_Certificates: ICertificates; safecall;
    function Sign(const pSigner: ISigner; bDetached: WordBool; EncodingType: CAPICOM_ENCODING_TYPE): WideString; safecall;
    function CoSign(const pSigner: ISigner; EncodingType: CAPICOM_ENCODING_TYPE): WideString; safecall;
    procedure Verify(const SignedMessage: WideString; bDetached: WordBool; 
                     VerifyFlag: CAPICOM_SIGNED_DATA_VERIFY_FLAG); safecall;
    property Content: WideString read Get_Content write Set_Content;
    property Signers: ISigners read Get_Signers;
    property Certificates: ICertificates read Get_Certificates;
  end;

// *********************************************************************//
// DispIntf:  ISignedDataDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {AE9C454B-FC65-4C10-B130-CD9B45BA948B}
// *********************************************************************//
  ISignedDataDisp = dispinterface
    ['{AE9C454B-FC65-4C10-B130-CD9B45BA948B}']
    property Content: WideString dispid 0;
    property Signers: ISigners readonly dispid 1;
    property Certificates: ICertificates readonly dispid 2;
    function Sign(const pSigner: ISigner; bDetached: WordBool; EncodingType: CAPICOM_ENCODING_TYPE): WideString; dispid 3;
    function CoSign(const pSigner: ISigner; EncodingType: CAPICOM_ENCODING_TYPE): WideString; dispid 4;
    procedure Verify(const SignedMessage: WideString; bDetached: WordBool; 
                     VerifyFlag: CAPICOM_SIGNED_DATA_VERIFY_FLAG); dispid 5;
  end;

// *********************************************************************//
// Interface: IAlgorithm
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {BF3D04A9-B0DA-4153-B45E-6CCFA5AC715B}
// *********************************************************************//
  IAlgorithm = interface(IDispatch)
    ['{BF3D04A9-B0DA-4153-B45E-6CCFA5AC715B}']
    function Get_Name: CAPICOM_ENCRYPTION_ALGORITHM; safecall;
    procedure Set_Name(pVal: CAPICOM_ENCRYPTION_ALGORITHM); safecall;
    function Get_KeyLength: CAPICOM_ENCRYPTION_KEY_LENGTH; safecall;
    procedure Set_KeyLength(pVal: CAPICOM_ENCRYPTION_KEY_LENGTH); safecall;
    property Name: CAPICOM_ENCRYPTION_ALGORITHM read Get_Name write Set_Name;
    property KeyLength: CAPICOM_ENCRYPTION_KEY_LENGTH read Get_KeyLength write Set_KeyLength;
  end;

// *********************************************************************//
// DispIntf:  IAlgorithmDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {BF3D04A9-B0DA-4153-B45E-6CCFA5AC715B}
// *********************************************************************//
  IAlgorithmDisp = dispinterface
    ['{BF3D04A9-B0DA-4153-B45E-6CCFA5AC715B}']
    property Name: CAPICOM_ENCRYPTION_ALGORITHM dispid 0;
    property KeyLength: CAPICOM_ENCRYPTION_KEY_LENGTH dispid 1;
  end;

// *********************************************************************//
// Interface: IRecipients
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A694C896-FC38-4C34-AE61-3B1A95984C14}
// *********************************************************************//
  IRecipients = interface(IDispatch)
    ['{A694C896-FC38-4C34-AE61-3B1A95984C14}']
    function Get_Item(Index: Integer): OleVariant; safecall;
    function Get_Count: Integer; safecall;
    function Get__NewEnum: IUnknown; safecall;
    procedure Add(const pVal: ICertificate); safecall;
    procedure Remove(Index: Integer); safecall;
    procedure Clear; safecall;
    property Item[Index: Integer]: OleVariant read Get_Item; default;
    property Count: Integer read Get_Count;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  IRecipientsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A694C896-FC38-4C34-AE61-3B1A95984C14}
// *********************************************************************//
  IRecipientsDisp = dispinterface
    ['{A694C896-FC38-4C34-AE61-3B1A95984C14}']
    property Item[Index: Integer]: OleVariant readonly dispid 0; default;
    property Count: Integer readonly dispid 1;
    property _NewEnum: IUnknown readonly dispid -4;
    procedure Add(const pVal: ICertificate); dispid 2;
    procedure Remove(Index: Integer); dispid 3;
    procedure Clear; dispid 4;
  end;

// *********************************************************************//
// Interface: IEnvelopedData
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F6CB6A20-CC18-4424-AE57-6F2AA3DC2059}
// *********************************************************************//
  IEnvelopedData = interface(IDispatch)
    ['{F6CB6A20-CC18-4424-AE57-6F2AA3DC2059}']
    procedure Set_Content(const pVal: WideString); safecall;
    function Get_Content: WideString; safecall;
    function Get_Algorithm: IAlgorithm; safecall;
    function Get_Recipients: IRecipients; safecall;
    function Encrypt(EncodingType: CAPICOM_ENCODING_TYPE): WideString; safecall;
    procedure Decrypt(const EnvelopedMessage: WideString); safecall;
    property Content: WideString read Get_Content write Set_Content;
    property Algorithm: IAlgorithm read Get_Algorithm;
    property Recipients: IRecipients read Get_Recipients;
  end;

// *********************************************************************//
// DispIntf:  IEnvelopedDataDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F6CB6A20-CC18-4424-AE57-6F2AA3DC2059}
// *********************************************************************//
  IEnvelopedDataDisp = dispinterface
    ['{F6CB6A20-CC18-4424-AE57-6F2AA3DC2059}']
    property Content: WideString dispid 0;
    property Algorithm: IAlgorithm readonly dispid 1;
    property Recipients: IRecipients readonly dispid 2;
    function Encrypt(EncodingType: CAPICOM_ENCODING_TYPE): WideString; dispid 3;
    procedure Decrypt(const EnvelopedMessage: WideString); dispid 4;
  end;

// *********************************************************************//
// Interface: IEncryptedData
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C4778A66-972F-42E4-87C5-5CC16F7931CA}
// *********************************************************************//
  IEncryptedData = interface(IDispatch)
    ['{C4778A66-972F-42E4-87C5-5CC16F7931CA}']
    procedure Set_Content(const pVal: WideString); safecall;
    function Get_Content: WideString; safecall;
    function Get_Algorithm: IAlgorithm; safecall;
    procedure SetSecret(const newVal: WideString; SecretType: CAPICOM_SECRET_TYPE); safecall;
    function Encrypt(EncodingType: CAPICOM_ENCODING_TYPE): WideString; safecall;
    procedure Decrypt(const EncryptedMessage: WideString); safecall;
    property Content: WideString read Get_Content write Set_Content;
    property Algorithm: IAlgorithm read Get_Algorithm;
  end;

// *********************************************************************//
// DispIntf:  IEncryptedDataDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C4778A66-972F-42E4-87C5-5CC16F7931CA}
// *********************************************************************//
  IEncryptedDataDisp = dispinterface
    ['{C4778A66-972F-42E4-87C5-5CC16F7931CA}']
    property Content: WideString dispid 0;
    property Algorithm: IAlgorithm readonly dispid 1;
    procedure SetSecret(const newVal: WideString; SecretType: CAPICOM_SECRET_TYPE); dispid 2;
    function Encrypt(EncodingType: CAPICOM_ENCODING_TYPE): WideString; dispid 3;
    procedure Decrypt(const EncryptedMessage: WideString); dispid 4;
  end;

// *********************************************************************//
// Interface: INoticeNumbers
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {EE2C051D-33A1-4157-86B4-9280E29782F2}
// *********************************************************************//
  INoticeNumbers = interface(IDispatch)
    ['{EE2C051D-33A1-4157-86B4-9280E29782F2}']
    function Get_Item(Index: Integer): OleVariant; safecall;
    function Get_Count: Integer; safecall;
    function Get__NewEnum: IUnknown; safecall;
    property Item[Index: Integer]: OleVariant read Get_Item; default;
    property Count: Integer read Get_Count;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  INoticeNumbersDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {EE2C051D-33A1-4157-86B4-9280E29782F2}
// *********************************************************************//
  INoticeNumbersDisp = dispinterface
    ['{EE2C051D-33A1-4157-86B4-9280E29782F2}']
    property Item[Index: Integer]: OleVariant readonly dispid 0; default;
    property Count: Integer readonly dispid 1;
    property _NewEnum: IUnknown readonly dispid -4;
  end;

// *********************************************************************//
// Interface: IQualifier
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3604C9DD-A22E-4A15-A469-8181C0C113DE}
// *********************************************************************//
  IQualifier = interface(IDispatch)
    ['{3604C9DD-A22E-4A15-A469-8181C0C113DE}']
    function Get_OID: IOID; safecall;
    function Get_CPSPointer: WideString; safecall;
    function Get_OrganizationName: WideString; safecall;
    function Get_NoticeNumbers: INoticeNumbers; safecall;
    function Get_ExplicitText: WideString; safecall;
    property OID: IOID read Get_OID;
    property CPSPointer: WideString read Get_CPSPointer;
    property OrganizationName: WideString read Get_OrganizationName;
    property NoticeNumbers: INoticeNumbers read Get_NoticeNumbers;
    property ExplicitText: WideString read Get_ExplicitText;
  end;

// *********************************************************************//
// DispIntf:  IQualifierDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3604C9DD-A22E-4A15-A469-8181C0C113DE}
// *********************************************************************//
  IQualifierDisp = dispinterface
    ['{3604C9DD-A22E-4A15-A469-8181C0C113DE}']
    property OID: IOID readonly dispid 0;
    property CPSPointer: WideString readonly dispid 1;
    property OrganizationName: WideString readonly dispid 2;
    property NoticeNumbers: INoticeNumbers readonly dispid 3;
    property ExplicitText: WideString readonly dispid 4;
  end;

// *********************************************************************//
// Interface: IQualifiers
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {6B5A8AB6-597D-4398-AC63-1036EF546348}
// *********************************************************************//
  IQualifiers = interface(IDispatch)
    ['{6B5A8AB6-597D-4398-AC63-1036EF546348}']
    function Get_Item(Index: Integer): OleVariant; safecall;
    function Get_Count: Integer; safecall;
    function Get__NewEnum: IUnknown; safecall;
    property Item[Index: Integer]: OleVariant read Get_Item; default;
    property Count: Integer read Get_Count;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  IQualifiersDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {6B5A8AB6-597D-4398-AC63-1036EF546348}
// *********************************************************************//
  IQualifiersDisp = dispinterface
    ['{6B5A8AB6-597D-4398-AC63-1036EF546348}']
    property Item[Index: Integer]: OleVariant readonly dispid 0; default;
    property Count: Integer readonly dispid 1;
    property _NewEnum: IUnknown readonly dispid -4;
  end;

// *********************************************************************//
// Interface: IPolicyInformation
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8973710C-8411-4951-9E65-D45FD524FFDF}
// *********************************************************************//
  IPolicyInformation = interface(IDispatch)
    ['{8973710C-8411-4951-9E65-D45FD524FFDF}']
    function Get_OID: IOID; safecall;
    function Get_Qualifiers: IQualifiers; safecall;
    property OID: IOID read Get_OID;
    property Qualifiers: IQualifiers read Get_Qualifiers;
  end;

// *********************************************************************//
// DispIntf:  IPolicyInformationDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8973710C-8411-4951-9E65-D45FD524FFDF}
// *********************************************************************//
  IPolicyInformationDisp = dispinterface
    ['{8973710C-8411-4951-9E65-D45FD524FFDF}']
    property OID: IOID readonly dispid 0;
    property Qualifiers: IQualifiers readonly dispid 1;
  end;

// *********************************************************************//
// Interface: ICertificatePolicies
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {CC7A72A7-C83A-4049-85F4-4292DE9DBFD3}
// *********************************************************************//
  ICertificatePolicies = interface(IDispatch)
    ['{CC7A72A7-C83A-4049-85F4-4292DE9DBFD3}']
    function Get_Item(Index: Integer): OleVariant; safecall;
    function Get_Count: Integer; safecall;
    function Get__NewEnum: IUnknown; safecall;
    property Item[Index: Integer]: OleVariant read Get_Item; default;
    property Count: Integer read Get_Count;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  ICertificatePoliciesDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {CC7A72A7-C83A-4049-85F4-4292DE9DBFD3}
// *********************************************************************//
  ICertificatePoliciesDisp = dispinterface
    ['{CC7A72A7-C83A-4049-85F4-4292DE9DBFD3}']
    property Item[Index: Integer]: OleVariant readonly dispid 0; default;
    property Count: Integer readonly dispid 1;
    property _NewEnum: IUnknown readonly dispid -4;
  end;

// *********************************************************************//
// Interface: IExtension
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {ED4E4ED4-FDD8-476E-AED9-5239E7948257}
// *********************************************************************//
  IExtension = interface(IDispatch)
    ['{ED4E4ED4-FDD8-476E-AED9-5239E7948257}']
    function Get_OID: IOID; safecall;
    function Get_IsCritical: WordBool; safecall;
    function Get_EncodedData: IEncodedData; safecall;
    property OID: IOID read Get_OID;
    property IsCritical: WordBool read Get_IsCritical;
    property EncodedData: IEncodedData read Get_EncodedData;
  end;

// *********************************************************************//
// DispIntf:  IExtensionDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {ED4E4ED4-FDD8-476E-AED9-5239E7948257}
// *********************************************************************//
  IExtensionDisp = dispinterface
    ['{ED4E4ED4-FDD8-476E-AED9-5239E7948257}']
    property OID: IOID readonly dispid 0;
    property IsCritical: WordBool readonly dispid 1;
    property EncodedData: IEncodedData readonly dispid 2;
  end;

// *********************************************************************//
// Interface: ICPrivateKey
// Flags:     (512) Restricted
// GUID:      {50F241B7-A8F2-4E0A-B982-4BD7DF0CCF3C}
// *********************************************************************//
  ICPrivateKey = interface(IUnknown)
    ['{50F241B7-A8F2-4E0A-B982-4BD7DF0CCF3C}']
    function _GetKeyProvInfo(out pKeyProvInfo: PUserType1): HResult; stdcall;
    function _GetKeyContext(out pKeyContext: PUserType2): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISignedCode
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {84FBCB95-5600-404C-9187-AC25B4CD6E94}
// *********************************************************************//
  ISignedCode = interface(IDispatch)
    ['{84FBCB95-5600-404C-9187-AC25B4CD6E94}']
    function Get_FileName: WideString; safecall;
    procedure Set_FileName(const pVal: WideString); safecall;
    function Get_Description: WideString; safecall;
    procedure Set_Description(const pVal: WideString); safecall;
    function Get_DescriptionURL: WideString; safecall;
    procedure Set_DescriptionURL(const pVal: WideString); safecall;
    function Get_Signer: ISigner2; safecall;
    function Get_TimeStamper: ISigner2; safecall;
    function Get_Certificates: ICertificates2; safecall;
    procedure Sign(const pISigner2: ISigner2); safecall;
    procedure Timestamp(const URL: WideString); safecall;
    procedure Verify(bUIAllowed: WordBool); safecall;
    property FileName: WideString read Get_FileName write Set_FileName;
    property Description: WideString read Get_Description write Set_Description;
    property DescriptionURL: WideString read Get_DescriptionURL write Set_DescriptionURL;
    property Signer: ISigner2 read Get_Signer;
    property TimeStamper: ISigner2 read Get_TimeStamper;
    property Certificates: ICertificates2 read Get_Certificates;
  end;

// *********************************************************************//
// DispIntf:  ISignedCodeDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {84FBCB95-5600-404C-9187-AC25B4CD6E94}
// *********************************************************************//
  ISignedCodeDisp = dispinterface
    ['{84FBCB95-5600-404C-9187-AC25B4CD6E94}']
    property FileName: WideString dispid 0;
    property Description: WideString dispid 1;
    property DescriptionURL: WideString dispid 2;
    property Signer: ISigner2 readonly dispid 3;
    property TimeStamper: ISigner2 readonly dispid 4;
    property Certificates: ICertificates2 readonly dispid 5;
    procedure Sign(const pISigner2: ISigner2); dispid 6;
    procedure Timestamp(const URL: WideString); dispid 7;
    procedure Verify(bUIAllowed: WordBool); dispid 8;
  end;

// *********************************************************************//
// Interface: IHashedData
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {9F7F23E8-06F4-42E8-B965-5CBD044BF27F}
// *********************************************************************//
  IHashedData = interface(IDispatch)
    ['{9F7F23E8-06F4-42E8-B965-5CBD044BF27F}']
    function Get_Value: WideString; safecall;
    function Get_Algorithm: CAPICOM_HASH_ALGORITHM; safecall;
    procedure Set_Algorithm(pVal: CAPICOM_HASH_ALGORITHM); safecall;
    procedure Hash(const newVal: WideString); safecall;
    property Value: WideString read Get_Value;
    property Algorithm: CAPICOM_HASH_ALGORITHM read Get_Algorithm write Set_Algorithm;
  end;

// *********************************************************************//
// DispIntf:  IHashedDataDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {9F7F23E8-06F4-42E8-B965-5CBD044BF27F}
// *********************************************************************//
  IHashedDataDisp = dispinterface
    ['{9F7F23E8-06F4-42E8-B965-5CBD044BF27F}']
    property Value: WideString readonly dispid 0;
    property Algorithm: CAPICOM_HASH_ALGORITHM dispid 1;
    procedure Hash(const newVal: WideString); dispid 2;
  end;

// *********************************************************************//
// Interface: IUtilities
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {EB166CF6-2AE6-44DA-BD96-0C1635D183FE}
// *********************************************************************//
  IUtilities = interface(IDispatch)
    ['{EB166CF6-2AE6-44DA-BD96-0C1635D183FE}']
    function GetRandom(Length: Integer; EncodingType: CAPICOM_ENCODING_TYPE): WideString; safecall;
    function Base64Encode(const SrcString: WideString): WideString; safecall;
    function Base64Decode(const EncodedString: WideString): WideString; safecall;
    function BinaryToHex(const BinaryString: WideString): WideString; safecall;
    function HexToBinary(const HexString: WideString): WideString; safecall;
    function BinaryStringToByteArray(const BinaryString: WideString): OleVariant; safecall;
    function ByteArrayToBinaryString(varByteArray: OleVariant): WideString; safecall;
    function LocalTimeToUTCTime(LocalTime: TDateTime): TDateTime; safecall;
    function UTCTimeToLocalTime(UTCTime: TDateTime): TDateTime; safecall;
  end;

// *********************************************************************//
// DispIntf:  IUtilitiesDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {EB166CF6-2AE6-44DA-BD96-0C1635D183FE}
// *********************************************************************//
  IUtilitiesDisp = dispinterface
    ['{EB166CF6-2AE6-44DA-BD96-0C1635D183FE}']
    function GetRandom(Length: Integer; EncodingType: CAPICOM_ENCODING_TYPE): WideString; dispid 1;
    function Base64Encode(const SrcString: WideString): WideString; dispid 2;
    function Base64Decode(const EncodedString: WideString): WideString; dispid 3;
    function BinaryToHex(const BinaryString: WideString): WideString; dispid 4;
    function HexToBinary(const HexString: WideString): WideString; dispid 5;
    function BinaryStringToByteArray(const BinaryString: WideString): OleVariant; dispid 6;
    function ByteArrayToBinaryString(varByteArray: OleVariant): WideString; dispid 7;
    function LocalTimeToUTCTime(LocalTime: TDateTime): TDateTime; dispid 8;
    function UTCTimeToLocalTime(UTCTime: TDateTime): TDateTime; dispid 9;
  end;

// *********************************************************************//
// The Class CoSettings provides a Create and CreateRemote method to          
// create instances of the default interface ISettings exposed by              
// the CoClass Settings. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSettings = class
    class function Create: ISettings;
    class function CreateRemote(const MachineName: string): ISettings;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TSettings
// Help String      : Settings Class
// Default Interface: ISettings
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TSettingsProperties= class;
{$ENDIF}
  TSettings = class(TOleServer)
  private
    FIntf: ISettings;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TSettingsProperties;
    function GetServerProperties: TSettingsProperties;
{$ENDIF}
    function GetDefaultInterface: ISettings;
  protected
    procedure InitServerData; override;
    function Get_EnablePromptForCertificateUI: WordBool;
    procedure Set_EnablePromptForCertificateUI(pVal: WordBool);
    function Get_ActiveDirectorySearchLocation: CAPICOM_ACTIVE_DIRECTORY_SEARCH_LOCATION;
    procedure Set_ActiveDirectorySearchLocation(pVal: CAPICOM_ACTIVE_DIRECTORY_SEARCH_LOCATION);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: ISettings);
    procedure Disconnect; override;
    property DefaultInterface: ISettings read GetDefaultInterface;
    property EnablePromptForCertificateUI: WordBool read Get_EnablePromptForCertificateUI write Set_EnablePromptForCertificateUI;
    property ActiveDirectorySearchLocation: CAPICOM_ACTIVE_DIRECTORY_SEARCH_LOCATION read Get_ActiveDirectorySearchLocation write Set_ActiveDirectorySearchLocation;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TSettingsProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TSettings
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TSettingsProperties = class(TPersistent)
  private
    FServer:    TSettings;
    function    GetDefaultInterface: ISettings;
    constructor Create(AServer: TSettings);
  protected
    function Get_EnablePromptForCertificateUI: WordBool;
    procedure Set_EnablePromptForCertificateUI(pVal: WordBool);
    function Get_ActiveDirectorySearchLocation: CAPICOM_ACTIVE_DIRECTORY_SEARCH_LOCATION;
    procedure Set_ActiveDirectorySearchLocation(pVal: CAPICOM_ACTIVE_DIRECTORY_SEARCH_LOCATION);
  public
    property DefaultInterface: ISettings read GetDefaultInterface;
  published
    property EnablePromptForCertificateUI: WordBool read Get_EnablePromptForCertificateUI write Set_EnablePromptForCertificateUI;
    property ActiveDirectorySearchLocation: CAPICOM_ACTIVE_DIRECTORY_SEARCH_LOCATION read Get_ActiveDirectorySearchLocation write Set_ActiveDirectorySearchLocation;
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoEKU provides a Create and CreateRemote method to          
// create instances of the default interface IEKU exposed by              
// the CoClass EKU. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoEKU = class
    class function Create: IEKU;
    class function CreateRemote(const MachineName: string): IEKU;
  end;

// *********************************************************************//
// The Class CoEKUs provides a Create and CreateRemote method to          
// create instances of the default interface IEKUs exposed by              
// the CoClass EKUs. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoEKUs = class
    class function Create: IEKUs;
    class function CreateRemote(const MachineName: string): IEKUs;
  end;

// *********************************************************************//
// The Class CoKeyUsage provides a Create and CreateRemote method to          
// create instances of the default interface IKeyUsage exposed by              
// the CoClass KeyUsage. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoKeyUsage = class
    class function Create: IKeyUsage;
    class function CreateRemote(const MachineName: string): IKeyUsage;
  end;

// *********************************************************************//
// The Class CoExtendedKeyUsage provides a Create and CreateRemote method to          
// create instances of the default interface IExtendedKeyUsage exposed by              
// the CoClass ExtendedKeyUsage. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoExtendedKeyUsage = class
    class function Create: IExtendedKeyUsage;
    class function CreateRemote(const MachineName: string): IExtendedKeyUsage;
  end;

// *********************************************************************//
// The Class CoBasicConstraints provides a Create and CreateRemote method to          
// create instances of the default interface IBasicConstraints exposed by              
// the CoClass BasicConstraints. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoBasicConstraints = class
    class function Create: IBasicConstraints;
    class function CreateRemote(const MachineName: string): IBasicConstraints;
  end;

// *********************************************************************//
// The Class CoCertificateStatus provides a Create and CreateRemote method to          
// create instances of the default interface ICertificateStatus3 exposed by              
// the CoClass CertificateStatus. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCertificateStatus = class
    class function Create: ICertificateStatus3;
    class function CreateRemote(const MachineName: string): ICertificateStatus3;
  end;

// *********************************************************************//
// The Class CoCertificate provides a Create and CreateRemote method to          
// create instances of the default interface ICertificate2 exposed by              
// the CoClass Certificate. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCertificate = class
    class function Create: ICertificate2;
    class function CreateRemote(const MachineName: string): ICertificate2;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TCertificate
// Help String      : Certificate Class
// Default Interface: ICertificate2
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TCertificateProperties= class;
{$ENDIF}
  TCertificate = class(TOleServer)
  private
    FIntf: ICertificate2;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TCertificateProperties;
    function GetServerProperties: TCertificateProperties;
{$ENDIF}
    function GetDefaultInterface: ICertificate2;
  protected
    procedure InitServerData; override;
    function Get_Version: Integer;
    function Get_SerialNumber: WideString;
    function Get_SubjectName: WideString;
    function Get_IssuerName: WideString;
    function Get_ValidFromDate: TDateTime;
    function Get_ValidToDate: TDateTime;
    function Get_Thumbprint: WideString;
    function Get_Archived: WordBool;
    procedure Set_Archived(pVal: WordBool);
    function Get_PrivateKey: IPrivateKey;
    procedure Set_PrivateKey(const pVal: IPrivateKey);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: ICertificate2);
    procedure Disconnect; override;
    function HasPrivateKey: WordBool;
    function GetInfo(InfoType: CAPICOM_CERT_INFO_TYPE): WideString;
    function IsValid: ICertificateStatus;
    function KeyUsage: IKeyUsage;
    function ExtendedKeyUsage: IExtendedKeyUsage;
    function BasicConstraints: IBasicConstraints;
    function Export(EncodingType: CAPICOM_ENCODING_TYPE): WideString;
    procedure Import(const EncodedCertificate: WideString);
    procedure Display;
    function Template: ITemplate;
    function PublicKey: IPublicKey;
    function Extensions: IExtensions;
    function ExtendedProperties: IExtendedProperties;
    procedure Load(const FileName: WideString; const Password: WideString; 
                   KeyStorageFlag: CAPICOM_KEY_STORAGE_FLAG; KeyLocation: CAPICOM_KEY_LOCATION);
    procedure Save(const FileName: WideString; const Password: WideString; 
                   SaveAs: CAPICOM_CERTIFICATE_SAVE_AS_TYPE; 
                   IncludeOption: CAPICOM_CERTIFICATE_INCLUDE_OPTION);
    property DefaultInterface: ICertificate2 read GetDefaultInterface;
    property Version: Integer read Get_Version;
    property SerialNumber: WideString read Get_SerialNumber;
    property SubjectName: WideString read Get_SubjectName;
    property IssuerName: WideString read Get_IssuerName;
    property ValidFromDate: TDateTime read Get_ValidFromDate;
    property ValidToDate: TDateTime read Get_ValidToDate;
    property Thumbprint: WideString read Get_Thumbprint;
    property Archived: WordBool read Get_Archived write Set_Archived;
    property PrivateKey: IPrivateKey read Get_PrivateKey write Set_PrivateKey;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TCertificateProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TCertificate
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TCertificateProperties = class(TPersistent)
  private
    FServer:    TCertificate;
    function    GetDefaultInterface: ICertificate2;
    constructor Create(AServer: TCertificate);
  protected
    function Get_Version: Integer;
    function Get_SerialNumber: WideString;
    function Get_SubjectName: WideString;
    function Get_IssuerName: WideString;
    function Get_ValidFromDate: TDateTime;
    function Get_ValidToDate: TDateTime;
    function Get_Thumbprint: WideString;
    function Get_Archived: WordBool;
    procedure Set_Archived(pVal: WordBool);
    function Get_PrivateKey: IPrivateKey;
    procedure Set_PrivateKey(const pVal: IPrivateKey);
  public
    property DefaultInterface: ICertificate2 read GetDefaultInterface;
  published
    property Archived: WordBool read Get_Archived write Set_Archived;
    property PrivateKey: IPrivateKey read Get_PrivateKey write Set_PrivateKey;
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoCertificates provides a Create and CreateRemote method to          
// create instances of the default interface ICertificates2 exposed by              
// the CoClass Certificates. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCertificates = class
    class function Create: ICertificates2;
    class function CreateRemote(const MachineName: string): ICertificates2;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TCertificates
// Help String      : Certificates Class
// Default Interface: ICertificates2
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TCertificatesProperties= class;
{$ENDIF}
  TCertificates = class(TOleServer)
  private
    FIntf: ICertificates2;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TCertificatesProperties;
    function GetServerProperties: TCertificatesProperties;
{$ENDIF}
    function GetDefaultInterface: ICertificates2;
  protected
    procedure InitServerData; override;
    function Get_Item(Index: Integer): OleVariant;
    function Get_Count: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: ICertificates2);
    procedure Disconnect; override;
    function Find(FindType: CAPICOM_CERTIFICATE_FIND_TYPE; varCriteria: OleVariant; 
                  bFindValidOnly: WordBool): ICertificates2;
    function Select(const Title: WideString; const DisplayString: WideString; bMultiSelect: WordBool): ICertificates2;
    procedure Add(const pVal: ICertificate2);
    procedure Remove(Index: OleVariant);
    procedure Clear;
    procedure Save(const FileName: WideString; const Password: WideString; 
                   SaveAs: CAPICOM_CERTIFICATES_SAVE_AS_TYPE; ExportFlag: CAPICOM_EXPORT_FLAG);
    property DefaultInterface: ICertificates2 read GetDefaultInterface;
    property Item[Index: Integer]: OleVariant read Get_Item; default;
    property Count: Integer read Get_Count;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TCertificatesProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TCertificates
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TCertificatesProperties = class(TPersistent)
  private
    FServer:    TCertificates;
    function    GetDefaultInterface: ICertificates2;
    constructor Create(AServer: TCertificates);
  protected
    function Get_Item(Index: Integer): OleVariant;
    function Get_Count: Integer;
  public
    property DefaultInterface: ICertificates2 read GetDefaultInterface;
  published
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoChain provides a Create and CreateRemote method to          
// create instances of the default interface IChain2 exposed by              
// the CoClass Chain. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoChain = class
    class function Create: IChain2;
    class function CreateRemote(const MachineName: string): IChain2;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TChain
// Help String      : Chain Class
// Default Interface: IChain2
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TChainProperties= class;
{$ENDIF}
  TChain = class(TOleServer)
  private
    FIntf: IChain2;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TChainProperties;
    function GetServerProperties: TChainProperties;
{$ENDIF}
    function GetDefaultInterface: IChain2;
  protected
    procedure InitServerData; override;
    function Get_Certificates: ICertificates;
    function Get_Status(Index: Integer): Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IChain2);
    procedure Disconnect; override;
    function Build(const pICertificate: ICertificate): WordBool;
    function CertificatePolicies: IOIDs;
    function ApplicationPolicies: IOIDs;
    function ExtendedErrorInfo(Index: Integer): WideString;
    property DefaultInterface: IChain2 read GetDefaultInterface;
    property Certificates: ICertificates read Get_Certificates;
    property Status[Index: Integer]: Integer read Get_Status;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TChainProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TChain
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TChainProperties = class(TPersistent)
  private
    FServer:    TChain;
    function    GetDefaultInterface: IChain2;
    constructor Create(AServer: TChain);
  protected
    function Get_Certificates: ICertificates;
    function Get_Status(Index: Integer): Integer;
  public
    property DefaultInterface: IChain2 read GetDefaultInterface;
  published
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoStore provides a Create and CreateRemote method to          
// create instances of the default interface IStore3 exposed by              
// the CoClass Store. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoStore = class
    class function Create: IStore3;
    class function CreateRemote(const MachineName: string): IStore3;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TStore
// Help String      : Store Class
// Default Interface: IStore3
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TStoreProperties= class;
{$ENDIF}
  TStore = class(TOleServer)
  private
    FIntf: IStore3;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TStoreProperties;
    function GetServerProperties: TStoreProperties;
{$ENDIF}
    function GetDefaultInterface: IStore3;
  protected
    procedure InitServerData; override;
    function Get_Certificates: ICertificates;
    function Get_Name: WideString;
    function Get_Location: CAPICOM_STORE_LOCATION;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IStore3);
    procedure Disconnect; override;
    procedure Open(StoreLocation: CAPICOM_STORE_LOCATION; const StoreName: WideString; 
                   OpenMode: CAPICOM_STORE_OPEN_MODE);
    procedure Add(const pVal: ICertificate);
    procedure Remove(const pVal: ICertificate);
    function Export(SaveAs: CAPICOM_STORE_SAVE_AS_TYPE; EncodingType: CAPICOM_ENCODING_TYPE): WideString;
    procedure Import(const EncodedStore: WideString);
    procedure Load(const FileName: WideString; const Password: WideString; 
                   KeyStorageFlag: CAPICOM_KEY_STORAGE_FLAG);
    function Delete: WordBool;
    procedure Close;
    property DefaultInterface: IStore3 read GetDefaultInterface;
    property Certificates: ICertificates read Get_Certificates;
    property Name: WideString read Get_Name;
    property Location: CAPICOM_STORE_LOCATION read Get_Location;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TStoreProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TStore
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TStoreProperties = class(TPersistent)
  private
    FServer:    TStore;
    function    GetDefaultInterface: IStore3;
    constructor Create(AServer: TStore);
  protected
    function Get_Certificates: ICertificates;
    function Get_Name: WideString;
    function Get_Location: CAPICOM_STORE_LOCATION;
  public
    property DefaultInterface: IStore3 read GetDefaultInterface;
  published
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoAttribute provides a Create and CreateRemote method to          
// create instances of the default interface IAttribute exposed by              
// the CoClass Attribute. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoAttribute = class
    class function Create: IAttribute;
    class function CreateRemote(const MachineName: string): IAttribute;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TAttribute
// Help String      : Attribute Class
// Default Interface: IAttribute
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TAttributeProperties= class;
{$ENDIF}
  TAttribute = class(TOleServer)
  private
    FIntf: IAttribute;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TAttributeProperties;
    function GetServerProperties: TAttributeProperties;
{$ENDIF}
    function GetDefaultInterface: IAttribute;
  protected
    procedure InitServerData; override;
    function Get_Name: CAPICOM_ATTRIBUTE;
    procedure Set_Name(pVal: CAPICOM_ATTRIBUTE);
    function Get_Value: OleVariant;
    procedure Set_Value(pVal: OleVariant);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IAttribute);
    procedure Disconnect; override;
    property DefaultInterface: IAttribute read GetDefaultInterface;
    property Value: OleVariant read Get_Value write Set_Value;
    property Name: CAPICOM_ATTRIBUTE read Get_Name write Set_Name;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TAttributeProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TAttribute
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TAttributeProperties = class(TPersistent)
  private
    FServer:    TAttribute;
    function    GetDefaultInterface: IAttribute;
    constructor Create(AServer: TAttribute);
  protected
    function Get_Name: CAPICOM_ATTRIBUTE;
    procedure Set_Name(pVal: CAPICOM_ATTRIBUTE);
    function Get_Value: OleVariant;
    procedure Set_Value(pVal: OleVariant);
  public
    property DefaultInterface: IAttribute read GetDefaultInterface;
  published
    property Name: CAPICOM_ATTRIBUTE read Get_Name write Set_Name;
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoAttributes provides a Create and CreateRemote method to          
// create instances of the default interface IAttributes exposed by              
// the CoClass Attributes. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoAttributes = class
    class function Create: IAttributes;
    class function CreateRemote(const MachineName: string): IAttributes;
  end;

// *********************************************************************//
// The Class CoSigner provides a Create and CreateRemote method to          
// create instances of the default interface ISigner2 exposed by              
// the CoClass Signer. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSigner = class
    class function Create: ISigner2;
    class function CreateRemote(const MachineName: string): ISigner2;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TSigner
// Help String      : Signer Class
// Default Interface: ISigner2
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TSignerProperties= class;
{$ENDIF}
  TSigner = class(TOleServer)
  private
    FIntf: ISigner2;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TSignerProperties;
    function GetServerProperties: TSignerProperties;
{$ENDIF}
    function GetDefaultInterface: ISigner2;
  protected
    procedure InitServerData; override;
    function Get_Certificate: ICertificate;
    procedure Set_Certificate(const pVal: ICertificate);
    function Get_AuthenticatedAttributes: IAttributes;
    function Get_Chain: IChain;
    function Get_Options: CAPICOM_CERTIFICATE_INCLUDE_OPTION;
    procedure Set_Options(pVal: CAPICOM_CERTIFICATE_INCLUDE_OPTION);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: ISigner2);
    procedure Disconnect; override;
    procedure Load(const FileName: WideString; const Password: WideString);
    property DefaultInterface: ISigner2 read GetDefaultInterface;
    property AuthenticatedAttributes: IAttributes read Get_AuthenticatedAttributes;
    property Chain: IChain read Get_Chain;
    property Certificate: ICertificate read Get_Certificate write Set_Certificate;
    property Options: CAPICOM_CERTIFICATE_INCLUDE_OPTION read Get_Options write Set_Options;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TSignerProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TSigner
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TSignerProperties = class(TPersistent)
  private
    FServer:    TSigner;
    function    GetDefaultInterface: ISigner2;
    constructor Create(AServer: TSigner);
  protected
    function Get_Certificate: ICertificate;
    procedure Set_Certificate(const pVal: ICertificate);
    function Get_AuthenticatedAttributes: IAttributes;
    function Get_Chain: IChain;
    function Get_Options: CAPICOM_CERTIFICATE_INCLUDE_OPTION;
    procedure Set_Options(pVal: CAPICOM_CERTIFICATE_INCLUDE_OPTION);
  public
    property DefaultInterface: ISigner2 read GetDefaultInterface;
  published
    property Certificate: ICertificate read Get_Certificate write Set_Certificate;
    property Options: CAPICOM_CERTIFICATE_INCLUDE_OPTION read Get_Options write Set_Options;
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoSigners provides a Create and CreateRemote method to          
// create instances of the default interface ISigners exposed by              
// the CoClass Signers. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSigners = class
    class function Create: ISigners;
    class function CreateRemote(const MachineName: string): ISigners;
  end;

// *********************************************************************//
// The Class CoSignedData provides a Create and CreateRemote method to          
// create instances of the default interface ISignedData exposed by              
// the CoClass SignedData. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSignedData = class
    class function Create: ISignedData;
    class function CreateRemote(const MachineName: string): ISignedData;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TSignedData
// Help String      : SignedData Class
// Default Interface: ISignedData
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TSignedDataProperties= class;
{$ENDIF}
  TSignedData = class(TOleServer)
  private
    FIntf: ISignedData;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TSignedDataProperties;
    function GetServerProperties: TSignedDataProperties;
{$ENDIF}
    function GetDefaultInterface: ISignedData;
  protected
    procedure InitServerData; override;
    procedure Set_Content(const pVal: WideString);
    function Get_Content: WideString;
    function Get_Signers: ISigners;
    function Get_Certificates: ICertificates;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: ISignedData);
    procedure Disconnect; override;
    function Sign(const pSigner: ISigner; bDetached: WordBool; EncodingType: CAPICOM_ENCODING_TYPE): WideString;
    function CoSign(const pSigner: ISigner; EncodingType: CAPICOM_ENCODING_TYPE): WideString;
    procedure Verify(const SignedMessage: WideString; bDetached: WordBool; 
                     VerifyFlag: CAPICOM_SIGNED_DATA_VERIFY_FLAG);
    property DefaultInterface: ISignedData read GetDefaultInterface;
    property Signers: ISigners read Get_Signers;
    property Certificates: ICertificates read Get_Certificates;
    property Content: WideString read Get_Content write Set_Content;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TSignedDataProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TSignedData
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TSignedDataProperties = class(TPersistent)
  private
    FServer:    TSignedData;
    function    GetDefaultInterface: ISignedData;
    constructor Create(AServer: TSignedData);
  protected
    procedure Set_Content(const pVal: WideString);
    function Get_Content: WideString;
    function Get_Signers: ISigners;
    function Get_Certificates: ICertificates;
  public
    property DefaultInterface: ISignedData read GetDefaultInterface;
  published
    property Content: WideString read Get_Content write Set_Content;
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoAlgorithm provides a Create and CreateRemote method to          
// create instances of the default interface IAlgorithm exposed by              
// the CoClass Algorithm. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoAlgorithm = class
    class function Create: IAlgorithm;
    class function CreateRemote(const MachineName: string): IAlgorithm;
  end;

// *********************************************************************//
// The Class CoRecipients provides a Create and CreateRemote method to          
// create instances of the default interface IRecipients exposed by              
// the CoClass Recipients. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoRecipients = class
    class function Create: IRecipients;
    class function CreateRemote(const MachineName: string): IRecipients;
  end;

// *********************************************************************//
// The Class CoEnvelopedData provides a Create and CreateRemote method to          
// create instances of the default interface IEnvelopedData exposed by              
// the CoClass EnvelopedData. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoEnvelopedData = class
    class function Create: IEnvelopedData;
    class function CreateRemote(const MachineName: string): IEnvelopedData;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TEnvelopedData
// Help String      : EnvelopedData Class
// Default Interface: IEnvelopedData
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TEnvelopedDataProperties= class;
{$ENDIF}
  TEnvelopedData = class(TOleServer)
  private
    FIntf: IEnvelopedData;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TEnvelopedDataProperties;
    function GetServerProperties: TEnvelopedDataProperties;
{$ENDIF}
    function GetDefaultInterface: IEnvelopedData;
  protected
    procedure InitServerData; override;
    procedure Set_Content(const pVal: WideString);
    function Get_Content: WideString;
    function Get_Algorithm: IAlgorithm;
    function Get_Recipients: IRecipients;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IEnvelopedData);
    procedure Disconnect; override;
    function Encrypt(EncodingType: CAPICOM_ENCODING_TYPE): WideString;
    procedure Decrypt(const EnvelopedMessage: WideString);
    property DefaultInterface: IEnvelopedData read GetDefaultInterface;
    property Algorithm: IAlgorithm read Get_Algorithm;
    property Recipients: IRecipients read Get_Recipients;
    property Content: WideString read Get_Content write Set_Content;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TEnvelopedDataProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TEnvelopedData
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TEnvelopedDataProperties = class(TPersistent)
  private
    FServer:    TEnvelopedData;
    function    GetDefaultInterface: IEnvelopedData;
    constructor Create(AServer: TEnvelopedData);
  protected
    procedure Set_Content(const pVal: WideString);
    function Get_Content: WideString;
    function Get_Algorithm: IAlgorithm;
    function Get_Recipients: IRecipients;
  public
    property DefaultInterface: IEnvelopedData read GetDefaultInterface;
  published
    property Content: WideString read Get_Content write Set_Content;
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoEncryptedData provides a Create and CreateRemote method to          
// create instances of the default interface IEncryptedData exposed by              
// the CoClass EncryptedData. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoEncryptedData = class
    class function Create: IEncryptedData;
    class function CreateRemote(const MachineName: string): IEncryptedData;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TEncryptedData
// Help String      : EncryptedData Class
// Default Interface: IEncryptedData
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TEncryptedDataProperties= class;
{$ENDIF}
  TEncryptedData = class(TOleServer)
  private
    FIntf: IEncryptedData;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TEncryptedDataProperties;
    function GetServerProperties: TEncryptedDataProperties;
{$ENDIF}
    function GetDefaultInterface: IEncryptedData;
  protected
    procedure InitServerData; override;
    procedure Set_Content(const pVal: WideString);
    function Get_Content: WideString;
    function Get_Algorithm: IAlgorithm;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IEncryptedData);
    procedure Disconnect; override;
    procedure SetSecret(const newVal: WideString; SecretType: CAPICOM_SECRET_TYPE);
    function Encrypt(EncodingType: CAPICOM_ENCODING_TYPE): WideString;
    procedure Decrypt(const EncryptedMessage: WideString);
    property DefaultInterface: IEncryptedData read GetDefaultInterface;
    property Algorithm: IAlgorithm read Get_Algorithm;
    property Content: WideString read Get_Content write Set_Content;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TEncryptedDataProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TEncryptedData
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TEncryptedDataProperties = class(TPersistent)
  private
    FServer:    TEncryptedData;
    function    GetDefaultInterface: IEncryptedData;
    constructor Create(AServer: TEncryptedData);
  protected
    procedure Set_Content(const pVal: WideString);
    function Get_Content: WideString;
    function Get_Algorithm: IAlgorithm;
  public
    property DefaultInterface: IEncryptedData read GetDefaultInterface;
  published
    property Content: WideString read Get_Content write Set_Content;
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoOID provides a Create and CreateRemote method to          
// create instances of the default interface IOID exposed by              
// the CoClass OID. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoOID = class
    class function Create: IOID;
    class function CreateRemote(const MachineName: string): IOID;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TOID
// Help String      : OID Class
// Default Interface: IOID
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TOIDProperties= class;
{$ENDIF}
  TOID = class(TOleServer)
  private
    FIntf: IOID;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TOIDProperties;
    function GetServerProperties: TOIDProperties;
{$ENDIF}
    function GetDefaultInterface: IOID;
  protected
    procedure InitServerData; override;
    function Get_Name: CAPICOM_OID;
    procedure Set_Name(pVal: CAPICOM_OID);
    function Get_FriendlyName: WideString;
    procedure Set_FriendlyName(const pVal: WideString);
    function Get_Value: WideString;
    procedure Set_Value(const pVal: WideString);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IOID);
    procedure Disconnect; override;
    property DefaultInterface: IOID read GetDefaultInterface;
    property Name: CAPICOM_OID read Get_Name write Set_Name;
    property FriendlyName: WideString read Get_FriendlyName write Set_FriendlyName;
    property Value: WideString read Get_Value write Set_Value;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TOIDProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TOID
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TOIDProperties = class(TPersistent)
  private
    FServer:    TOID;
    function    GetDefaultInterface: IOID;
    constructor Create(AServer: TOID);
  protected
    function Get_Name: CAPICOM_OID;
    procedure Set_Name(pVal: CAPICOM_OID);
    function Get_FriendlyName: WideString;
    procedure Set_FriendlyName(const pVal: WideString);
    function Get_Value: WideString;
    procedure Set_Value(const pVal: WideString);
  public
    property DefaultInterface: IOID read GetDefaultInterface;
  published
    property Name: CAPICOM_OID read Get_Name write Set_Name;
    property FriendlyName: WideString read Get_FriendlyName write Set_FriendlyName;
    property Value: WideString read Get_Value write Set_Value;
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoOIDs provides a Create and CreateRemote method to          
// create instances of the default interface IOIDs exposed by              
// the CoClass OIDs. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoOIDs = class
    class function Create: IOIDs;
    class function CreateRemote(const MachineName: string): IOIDs;
  end;

// *********************************************************************//
// The Class CoNoticeNumbers provides a Create and CreateRemote method to          
// create instances of the default interface INoticeNumbers exposed by              
// the CoClass NoticeNumbers. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoNoticeNumbers = class
    class function Create: INoticeNumbers;
    class function CreateRemote(const MachineName: string): INoticeNumbers;
  end;

// *********************************************************************//
// The Class CoQualifier provides a Create and CreateRemote method to          
// create instances of the default interface IQualifier exposed by              
// the CoClass Qualifier. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoQualifier = class
    class function Create: IQualifier;
    class function CreateRemote(const MachineName: string): IQualifier;
  end;

// *********************************************************************//
// The Class CoQualifiers provides a Create and CreateRemote method to          
// create instances of the default interface IQualifiers exposed by              
// the CoClass Qualifiers. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoQualifiers = class
    class function Create: IQualifiers;
    class function CreateRemote(const MachineName: string): IQualifiers;
  end;

// *********************************************************************//
// The Class CoPolicyInformation provides a Create and CreateRemote method to          
// create instances of the default interface IPolicyInformation exposed by              
// the CoClass PolicyInformation. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoPolicyInformation = class
    class function Create: IPolicyInformation;
    class function CreateRemote(const MachineName: string): IPolicyInformation;
  end;

// *********************************************************************//
// The Class CoCertificatePolicies provides a Create and CreateRemote method to          
// create instances of the default interface ICertificatePolicies exposed by              
// the CoClass CertificatePolicies. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCertificatePolicies = class
    class function Create: ICertificatePolicies;
    class function CreateRemote(const MachineName: string): ICertificatePolicies;
  end;

// *********************************************************************//
// The Class CoEncodedData provides a Create and CreateRemote method to          
// create instances of the default interface IEncodedData exposed by              
// the CoClass EncodedData. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoEncodedData = class
    class function Create: IEncodedData;
    class function CreateRemote(const MachineName: string): IEncodedData;
  end;

// *********************************************************************//
// The Class CoExtension provides a Create and CreateRemote method to          
// create instances of the default interface IExtension exposed by              
// the CoClass Extension. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoExtension = class
    class function Create: IExtension;
    class function CreateRemote(const MachineName: string): IExtension;
  end;

// *********************************************************************//
// The Class CoExtensions provides a Create and CreateRemote method to          
// create instances of the default interface IExtensions exposed by              
// the CoClass Extensions. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoExtensions = class
    class function Create: IExtensions;
    class function CreateRemote(const MachineName: string): IExtensions;
  end;

// *********************************************************************//
// The Class CoExtendedProperty provides a Create and CreateRemote method to          
// create instances of the default interface IExtendedProperty exposed by              
// the CoClass ExtendedProperty. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoExtendedProperty = class
    class function Create: IExtendedProperty;
    class function CreateRemote(const MachineName: string): IExtendedProperty;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TExtendedProperty
// Help String      : ExtendedProperty Class
// Default Interface: IExtendedProperty
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TExtendedPropertyProperties= class;
{$ENDIF}
  TExtendedProperty = class(TOleServer)
  private
    FIntf: IExtendedProperty;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TExtendedPropertyProperties;
    function GetServerProperties: TExtendedPropertyProperties;
{$ENDIF}
    function GetDefaultInterface: IExtendedProperty;
  protected
    procedure InitServerData; override;
    function Get_PropID: CAPICOM_PROPID;
    procedure Set_PropID(pVal: CAPICOM_PROPID);
    function Get_Value(EncodingType: CAPICOM_ENCODING_TYPE): WideString;
    procedure Set_Value(EncodingType: CAPICOM_ENCODING_TYPE; const pVal: WideString);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IExtendedProperty);
    procedure Disconnect; override;
    property DefaultInterface: IExtendedProperty read GetDefaultInterface;
    property Value[EncodingType: CAPICOM_ENCODING_TYPE]: WideString read Get_Value write Set_Value;
    property PropID: CAPICOM_PROPID read Get_PropID write Set_PropID;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TExtendedPropertyProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TExtendedProperty
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TExtendedPropertyProperties = class(TPersistent)
  private
    FServer:    TExtendedProperty;
    function    GetDefaultInterface: IExtendedProperty;
    constructor Create(AServer: TExtendedProperty);
  protected
    function Get_PropID: CAPICOM_PROPID;
    procedure Set_PropID(pVal: CAPICOM_PROPID);
    function Get_Value(EncodingType: CAPICOM_ENCODING_TYPE): WideString;
    procedure Set_Value(EncodingType: CAPICOM_ENCODING_TYPE; const pVal: WideString);
  public
    property DefaultInterface: IExtendedProperty read GetDefaultInterface;
  published
    property PropID: CAPICOM_PROPID read Get_PropID write Set_PropID;
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoExtendedProperties provides a Create and CreateRemote method to          
// create instances of the default interface IExtendedProperties exposed by              
// the CoClass ExtendedProperties. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoExtendedProperties = class
    class function Create: IExtendedProperties;
    class function CreateRemote(const MachineName: string): IExtendedProperties;
  end;

// *********************************************************************//
// The Class CoTemplate provides a Create and CreateRemote method to          
// create instances of the default interface ITemplate exposed by              
// the CoClass Template. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoTemplate = class
    class function Create: ITemplate;
    class function CreateRemote(const MachineName: string): ITemplate;
  end;

// *********************************************************************//
// The Class CoPublicKey provides a Create and CreateRemote method to          
// create instances of the default interface IPublicKey exposed by              
// the CoClass PublicKey. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoPublicKey = class
    class function Create: IPublicKey;
    class function CreateRemote(const MachineName: string): IPublicKey;
  end;

// *********************************************************************//
// The Class CoPrivateKey provides a Create and CreateRemote method to          
// create instances of the default interface IPrivateKey exposed by              
// the CoClass PrivateKey. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoPrivateKey = class
    class function Create: IPrivateKey;
    class function CreateRemote(const MachineName: string): IPrivateKey;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TPrivateKey
// Help String      : PrivateKey Class
// Default Interface: IPrivateKey
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TPrivateKeyProperties= class;
{$ENDIF}
  TPrivateKey = class(TOleServer)
  private
    FIntf: IPrivateKey;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TPrivateKeyProperties;
    function GetServerProperties: TPrivateKeyProperties;
{$ENDIF}
    function GetDefaultInterface: IPrivateKey;
  protected
    procedure InitServerData; override;
    function Get_ContainerName: WideString;
    function Get_UniqueContainerName: WideString;
    function Get_ProviderName: WideString;
    function Get_ProviderType: CAPICOM_PROV_TYPE;
    function Get_KeySpec: CAPICOM_KEY_SPEC;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IPrivateKey);
    procedure Disconnect; override;
    function IsAccessible: WordBool;
    function IsProtected: WordBool;
    function IsExportable: WordBool;
    function IsRemovable: WordBool;
    function IsMachineKeyset: WordBool;
    function IsHardwareDevice: WordBool;
    procedure Open(const ContainerName: WideString; const ProviderName: WideString; 
                   ProviderType: CAPICOM_PROV_TYPE; KeySpec: CAPICOM_KEY_SPEC; 
                   StoreLocation: CAPICOM_STORE_LOCATION; bCheckExistence: WordBool);
    procedure Delete;
    property DefaultInterface: IPrivateKey read GetDefaultInterface;
    property ContainerName: WideString read Get_ContainerName;
    property UniqueContainerName: WideString read Get_UniqueContainerName;
    property ProviderName: WideString read Get_ProviderName;
    property ProviderType: CAPICOM_PROV_TYPE read Get_ProviderType;
    property KeySpec: CAPICOM_KEY_SPEC read Get_KeySpec;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TPrivateKeyProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TPrivateKey
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TPrivateKeyProperties = class(TPersistent)
  private
    FServer:    TPrivateKey;
    function    GetDefaultInterface: IPrivateKey;
    constructor Create(AServer: TPrivateKey);
  protected
    function Get_ContainerName: WideString;
    function Get_UniqueContainerName: WideString;
    function Get_ProviderName: WideString;
    function Get_ProviderType: CAPICOM_PROV_TYPE;
    function Get_KeySpec: CAPICOM_KEY_SPEC;
  public
    property DefaultInterface: IPrivateKey read GetDefaultInterface;
  published
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoSignedCode provides a Create and CreateRemote method to          
// create instances of the default interface ISignedCode exposed by              
// the CoClass SignedCode. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSignedCode = class
    class function Create: ISignedCode;
    class function CreateRemote(const MachineName: string): ISignedCode;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TSignedCode
// Help String      : SignedCode Class
// Default Interface: ISignedCode
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TSignedCodeProperties= class;
{$ENDIF}
  TSignedCode = class(TOleServer)
  private
    FIntf: ISignedCode;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TSignedCodeProperties;
    function GetServerProperties: TSignedCodeProperties;
{$ENDIF}
    function GetDefaultInterface: ISignedCode;
  protected
    procedure InitServerData; override;
    function Get_FileName: WideString;
    procedure Set_FileName(const pVal: WideString);
    function Get_Description: WideString;
    procedure Set_Description(const pVal: WideString);
    function Get_DescriptionURL: WideString;
    procedure Set_DescriptionURL(const pVal: WideString);
    function Get_Signer: ISigner2;
    function Get_TimeStamper: ISigner2;
    function Get_Certificates: ICertificates2;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: ISignedCode);
    procedure Disconnect; override;
    procedure Sign(const pISigner2: ISigner2);
    procedure Timestamp(const URL: WideString);
    procedure Verify(bUIAllowed: WordBool);
    property DefaultInterface: ISignedCode read GetDefaultInterface;
    property Signer: ISigner2 read Get_Signer;
    property TimeStamper: ISigner2 read Get_TimeStamper;
    property Certificates: ICertificates2 read Get_Certificates;
    property FileName: WideString read Get_FileName write Set_FileName;
    property Description: WideString read Get_Description write Set_Description;
    property DescriptionURL: WideString read Get_DescriptionURL write Set_DescriptionURL;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TSignedCodeProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TSignedCode
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TSignedCodeProperties = class(TPersistent)
  private
    FServer:    TSignedCode;
    function    GetDefaultInterface: ISignedCode;
    constructor Create(AServer: TSignedCode);
  protected
    function Get_FileName: WideString;
    procedure Set_FileName(const pVal: WideString);
    function Get_Description: WideString;
    procedure Set_Description(const pVal: WideString);
    function Get_DescriptionURL: WideString;
    procedure Set_DescriptionURL(const pVal: WideString);
    function Get_Signer: ISigner2;
    function Get_TimeStamper: ISigner2;
    function Get_Certificates: ICertificates2;
  public
    property DefaultInterface: ISignedCode read GetDefaultInterface;
  published
    property FileName: WideString read Get_FileName write Set_FileName;
    property Description: WideString read Get_Description write Set_Description;
    property DescriptionURL: WideString read Get_DescriptionURL write Set_DescriptionURL;
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoHashedData provides a Create and CreateRemote method to          
// create instances of the default interface IHashedData exposed by              
// the CoClass HashedData. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoHashedData = class
    class function Create: IHashedData;
    class function CreateRemote(const MachineName: string): IHashedData;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : THashedData
// Help String      : HashedData Class
// Default Interface: IHashedData
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  THashedDataProperties= class;
{$ENDIF}
  THashedData = class(TOleServer)
  private
    FIntf: IHashedData;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: THashedDataProperties;
    function GetServerProperties: THashedDataProperties;
{$ENDIF}
    function GetDefaultInterface: IHashedData;
  protected
    procedure InitServerData; override;
    function Get_Value: WideString;
    function Get_Algorithm: CAPICOM_HASH_ALGORITHM;
    procedure Set_Algorithm(pVal: CAPICOM_HASH_ALGORITHM);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IHashedData);
    procedure Disconnect; override;
    procedure Hash(const newVal: WideString);
    property DefaultInterface: IHashedData read GetDefaultInterface;
    property Value: WideString read Get_Value;
    property Algorithm: CAPICOM_HASH_ALGORITHM read Get_Algorithm write Set_Algorithm;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: THashedDataProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : THashedData
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 THashedDataProperties = class(TPersistent)
  private
    FServer:    THashedData;
    function    GetDefaultInterface: IHashedData;
    constructor Create(AServer: THashedData);
  protected
    function Get_Value: WideString;
    function Get_Algorithm: CAPICOM_HASH_ALGORITHM;
    procedure Set_Algorithm(pVal: CAPICOM_HASH_ALGORITHM);
  public
    property DefaultInterface: IHashedData read GetDefaultInterface;
  published
    property Algorithm: CAPICOM_HASH_ALGORITHM read Get_Algorithm write Set_Algorithm;
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoUtilities provides a Create and CreateRemote method to          
// create instances of the default interface IUtilities exposed by              
// the CoClass Utilities. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoUtilities = class
    class function Create: IUtilities;
    class function CreateRemote(const MachineName: string): IUtilities;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TUtilities
// Help String      : Utilities Class
// Default Interface: IUtilities
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TUtilitiesProperties= class;
{$ENDIF}
  TUtilities = class(TOleServer)
  private
    FIntf: IUtilities;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TUtilitiesProperties;
    function GetServerProperties: TUtilitiesProperties;
{$ENDIF}
    function GetDefaultInterface: IUtilities;
  protected
    procedure InitServerData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IUtilities);
    procedure Disconnect; override;
    function GetRandom(Length: Integer; EncodingType: CAPICOM_ENCODING_TYPE): WideString;
    function Base64Encode(const SrcString: WideString): WideString;
    function Base64Decode(const EncodedString: WideString): WideString;
    function BinaryToHex(const BinaryString: WideString): WideString;
    function HexToBinary(const HexString: WideString): WideString;
    function BinaryStringToByteArray(const BinaryString: WideString): OleVariant;
    function ByteArrayToBinaryString(varByteArray: OleVariant): WideString;
    function LocalTimeToUTCTime(LocalTime: TDateTime): TDateTime;
    function UTCTimeToLocalTime(UTCTime: TDateTime): TDateTime;
    property DefaultInterface: IUtilities read GetDefaultInterface;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TUtilitiesProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TUtilities
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TUtilitiesProperties = class(TPersistent)
  private
    FServer:    TUtilities;
    function    GetDefaultInterface: IUtilities;
    constructor Create(AServer: TUtilities);
  protected
  public
    property DefaultInterface: IUtilities read GetDefaultInterface;
  published
  end;
{$ENDIF}


procedure Register;

resourcestring
  dtlServerPage = 'ActiveX';

  dtlOcxPage = 'ActiveX';

implementation

uses ComObj;

class function CoSettings.Create: ISettings;
begin
  Result := CreateComObject(CLASS_Settings) as ISettings;
end;

class function CoSettings.CreateRemote(const MachineName: string): ISettings;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Settings) as ISettings;
end;

procedure TSettings.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{A996E48C-D3DC-4244-89F7-AFA33EC60679}';
    IntfIID:   '{A24104F5-46D0-4C0F-926D-665565908E91}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TSettings.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as ISettings;
  end;
end;

procedure TSettings.ConnectTo(svrIntf: ISettings);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TSettings.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TSettings.GetDefaultInterface: ISettings;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TSettings.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TSettingsProperties.Create(Self);
{$ENDIF}
end;

destructor TSettings.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TSettings.GetServerProperties: TSettingsProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TSettings.Get_EnablePromptForCertificateUI: WordBool;
begin
    Result := DefaultInterface.EnablePromptForCertificateUI;
end;

procedure TSettings.Set_EnablePromptForCertificateUI(pVal: WordBool);
begin
  DefaultInterface.Set_EnablePromptForCertificateUI(pVal);
end;

function TSettings.Get_ActiveDirectorySearchLocation: CAPICOM_ACTIVE_DIRECTORY_SEARCH_LOCATION;
begin
    Result := DefaultInterface.ActiveDirectorySearchLocation;
end;

procedure TSettings.Set_ActiveDirectorySearchLocation(pVal: CAPICOM_ACTIVE_DIRECTORY_SEARCH_LOCATION);
begin
  DefaultInterface.Set_ActiveDirectorySearchLocation(pVal);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TSettingsProperties.Create(AServer: TSettings);
begin
  inherited Create;
  FServer := AServer;
end;

function TSettingsProperties.GetDefaultInterface: ISettings;
begin
  Result := FServer.DefaultInterface;
end;

function TSettingsProperties.Get_EnablePromptForCertificateUI: WordBool;
begin
    Result := DefaultInterface.EnablePromptForCertificateUI;
end;

procedure TSettingsProperties.Set_EnablePromptForCertificateUI(pVal: WordBool);
begin
  DefaultInterface.Set_EnablePromptForCertificateUI(pVal);
end;

function TSettingsProperties.Get_ActiveDirectorySearchLocation: CAPICOM_ACTIVE_DIRECTORY_SEARCH_LOCATION;
begin
    Result := DefaultInterface.ActiveDirectorySearchLocation;
end;

procedure TSettingsProperties.Set_ActiveDirectorySearchLocation(pVal: CAPICOM_ACTIVE_DIRECTORY_SEARCH_LOCATION);
begin
  DefaultInterface.Set_ActiveDirectorySearchLocation(pVal);
end;

{$ENDIF}

class function CoEKU.Create: IEKU;
begin
  Result := CreateComObject(CLASS_EKU) as IEKU;
end;

class function CoEKU.CreateRemote(const MachineName: string): IEKU;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_EKU) as IEKU;
end;

class function CoEKUs.Create: IEKUs;
begin
  Result := CreateComObject(CLASS_EKUs) as IEKUs;
end;

class function CoEKUs.CreateRemote(const MachineName: string): IEKUs;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_EKUs) as IEKUs;
end;

class function CoKeyUsage.Create: IKeyUsage;
begin
  Result := CreateComObject(CLASS_KeyUsage) as IKeyUsage;
end;

class function CoKeyUsage.CreateRemote(const MachineName: string): IKeyUsage;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_KeyUsage) as IKeyUsage;
end;

class function CoExtendedKeyUsage.Create: IExtendedKeyUsage;
begin
  Result := CreateComObject(CLASS_ExtendedKeyUsage) as IExtendedKeyUsage;
end;

class function CoExtendedKeyUsage.CreateRemote(const MachineName: string): IExtendedKeyUsage;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ExtendedKeyUsage) as IExtendedKeyUsage;
end;

class function CoBasicConstraints.Create: IBasicConstraints;
begin
  Result := CreateComObject(CLASS_BasicConstraints) as IBasicConstraints;
end;

class function CoBasicConstraints.CreateRemote(const MachineName: string): IBasicConstraints;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_BasicConstraints) as IBasicConstraints;
end;

class function CoCertificateStatus.Create: ICertificateStatus3;
begin
  Result := CreateComObject(CLASS_CertificateStatus) as ICertificateStatus3;
end;

class function CoCertificateStatus.CreateRemote(const MachineName: string): ICertificateStatus3;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CertificateStatus) as ICertificateStatus3;
end;

class function CoCertificate.Create: ICertificate2;
begin
  Result := CreateComObject(CLASS_Certificate) as ICertificate2;
end;

class function CoCertificate.CreateRemote(const MachineName: string): ICertificate2;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Certificate) as ICertificate2;
end;

procedure TCertificate.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{9171C115-7DD9-46BA-B1E5-0ED50AFFC1B8}';
    IntfIID:   '{6FE450DC-AD32-48D4-A366-01EE7E0B1374}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TCertificate.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as ICertificate2;
  end;
end;

procedure TCertificate.ConnectTo(svrIntf: ICertificate2);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TCertificate.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TCertificate.GetDefaultInterface: ICertificate2;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TCertificate.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TCertificateProperties.Create(Self);
{$ENDIF}
end;

destructor TCertificate.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TCertificate.GetServerProperties: TCertificateProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TCertificate.Get_Version: Integer;
begin
    Result := DefaultInterface.Version;
end;

function TCertificate.Get_SerialNumber: WideString;
begin
    Result := DefaultInterface.SerialNumber;
end;

function TCertificate.Get_SubjectName: WideString;
begin
    Result := DefaultInterface.SubjectName;
end;

function TCertificate.Get_IssuerName: WideString;
begin
    Result := DefaultInterface.IssuerName;
end;

function TCertificate.Get_ValidFromDate: TDateTime;
begin
    Result := DefaultInterface.ValidFromDate;
end;

function TCertificate.Get_ValidToDate: TDateTime;
begin
    Result := DefaultInterface.ValidToDate;
end;

function TCertificate.Get_Thumbprint: WideString;
begin
    Result := DefaultInterface.Thumbprint;
end;

function TCertificate.Get_Archived: WordBool;
begin
    Result := DefaultInterface.Archived;
end;

procedure TCertificate.Set_Archived(pVal: WordBool);
begin
  DefaultInterface.Set_Archived(pVal);
end;

function TCertificate.Get_PrivateKey: IPrivateKey;
begin
    Result := DefaultInterface.PrivateKey;
end;

procedure TCertificate.Set_PrivateKey(const pVal: IPrivateKey);
begin
  DefaultInterface.Set_PrivateKey(pVal);
end;

function TCertificate.HasPrivateKey: WordBool;
begin
  Result := DefaultInterface.HasPrivateKey;
end;

function TCertificate.GetInfo(InfoType: CAPICOM_CERT_INFO_TYPE): WideString;
begin
  Result := DefaultInterface.GetInfo(InfoType);
end;

function TCertificate.IsValid: ICertificateStatus;
begin
  Result := DefaultInterface.IsValid;
end;

function TCertificate.KeyUsage: IKeyUsage;
begin
  Result := DefaultInterface.KeyUsage;
end;

function TCertificate.ExtendedKeyUsage: IExtendedKeyUsage;
begin
  Result := DefaultInterface.ExtendedKeyUsage;
end;

function TCertificate.BasicConstraints: IBasicConstraints;
begin
  Result := DefaultInterface.BasicConstraints;
end;

function TCertificate.Export(EncodingType: CAPICOM_ENCODING_TYPE): WideString;
begin
  Result := DefaultInterface.Export(EncodingType);
end;

procedure TCertificate.Import(const EncodedCertificate: WideString);
begin
  DefaultInterface.Import(EncodedCertificate);
end;

procedure TCertificate.Display;
begin
  DefaultInterface.Display;
end;

function TCertificate.Template: ITemplate;
begin
  Result := DefaultInterface.Template;
end;

function TCertificate.PublicKey: IPublicKey;
begin
  Result := DefaultInterface.PublicKey;
end;

function TCertificate.Extensions: IExtensions;
begin
  Result := DefaultInterface.Extensions;
end;

function TCertificate.ExtendedProperties: IExtendedProperties;
begin
  Result := DefaultInterface.ExtendedProperties;
end;

procedure TCertificate.Load(const FileName: WideString; const Password: WideString; 
                            KeyStorageFlag: CAPICOM_KEY_STORAGE_FLAG; 
                            KeyLocation: CAPICOM_KEY_LOCATION);
begin
  DefaultInterface.Load(FileName, Password, KeyStorageFlag, KeyLocation);
end;

procedure TCertificate.Save(const FileName: WideString; const Password: WideString; 
                            SaveAs: CAPICOM_CERTIFICATE_SAVE_AS_TYPE; 
                            IncludeOption: CAPICOM_CERTIFICATE_INCLUDE_OPTION);
begin
  DefaultInterface.Save(FileName, Password, SaveAs, IncludeOption);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TCertificateProperties.Create(AServer: TCertificate);
begin
  inherited Create;
  FServer := AServer;
end;

function TCertificateProperties.GetDefaultInterface: ICertificate2;
begin
  Result := FServer.DefaultInterface;
end;

function TCertificateProperties.Get_Version: Integer;
begin
    Result := DefaultInterface.Version;
end;

function TCertificateProperties.Get_SerialNumber: WideString;
begin
    Result := DefaultInterface.SerialNumber;
end;

function TCertificateProperties.Get_SubjectName: WideString;
begin
    Result := DefaultInterface.SubjectName;
end;

function TCertificateProperties.Get_IssuerName: WideString;
begin
    Result := DefaultInterface.IssuerName;
end;

function TCertificateProperties.Get_ValidFromDate: TDateTime;
begin
    Result := DefaultInterface.ValidFromDate;
end;

function TCertificateProperties.Get_ValidToDate: TDateTime;
begin
    Result := DefaultInterface.ValidToDate;
end;

function TCertificateProperties.Get_Thumbprint: WideString;
begin
    Result := DefaultInterface.Thumbprint;
end;

function TCertificateProperties.Get_Archived: WordBool;
begin
    Result := DefaultInterface.Archived;
end;

procedure TCertificateProperties.Set_Archived(pVal: WordBool);
begin
  DefaultInterface.Set_Archived(pVal);
end;

function TCertificateProperties.Get_PrivateKey: IPrivateKey;
begin
    Result := DefaultInterface.PrivateKey;
end;

procedure TCertificateProperties.Set_PrivateKey(const pVal: IPrivateKey);
begin
  DefaultInterface.Set_PrivateKey(pVal);
end;

{$ENDIF}

class function CoCertificates.Create: ICertificates2;
begin
  Result := CreateComObject(CLASS_Certificates) as ICertificates2;
end;

class function CoCertificates.CreateRemote(const MachineName: string): ICertificates2;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Certificates) as ICertificates2;
end;

procedure TCertificates.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{3605B612-C3CF-4AB4-A426-2D853391DB2E}';
    IntfIID:   '{7B57C04B-1786-4B30-A7B6-36235CD58A14}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TCertificates.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as ICertificates2;
  end;
end;

procedure TCertificates.ConnectTo(svrIntf: ICertificates2);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TCertificates.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TCertificates.GetDefaultInterface: ICertificates2;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TCertificates.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TCertificatesProperties.Create(Self);
{$ENDIF}
end;

destructor TCertificates.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TCertificates.GetServerProperties: TCertificatesProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TCertificates.Get_Item(Index: Integer): OleVariant;
var
  InterfaceVariant : OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  Result := InterfaceVariant.Item[Index];
end;

function TCertificates.Get_Count: Integer;
begin
    Result := DefaultInterface.Count;
end;

function TCertificates.Find(FindType: CAPICOM_CERTIFICATE_FIND_TYPE; varCriteria: OleVariant; 
                            bFindValidOnly: WordBool): ICertificates2;
begin
  Result := DefaultInterface.Find(FindType, varCriteria, bFindValidOnly);
end;

function TCertificates.Select(const Title: WideString; const DisplayString: WideString; 
                              bMultiSelect: WordBool): ICertificates2;
begin
  Result := DefaultInterface.Select(Title, DisplayString, bMultiSelect);
end;

procedure TCertificates.Add(const pVal: ICertificate2);
begin
  DefaultInterface.Add(pVal);
end;

procedure TCertificates.Remove(Index: OleVariant);
begin
  DefaultInterface.Remove(Index);
end;

procedure TCertificates.Clear;
begin
  DefaultInterface.Clear;
end;

procedure TCertificates.Save(const FileName: WideString; const Password: WideString; 
                             SaveAs: CAPICOM_CERTIFICATES_SAVE_AS_TYPE; 
                             ExportFlag: CAPICOM_EXPORT_FLAG);
begin
  DefaultInterface.Save(FileName, Password, SaveAs, ExportFlag);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TCertificatesProperties.Create(AServer: TCertificates);
begin
  inherited Create;
  FServer := AServer;
end;

function TCertificatesProperties.GetDefaultInterface: ICertificates2;
begin
  Result := FServer.DefaultInterface;
end;

function TCertificatesProperties.Get_Item(Index: Integer): OleVariant;
var
  InterfaceVariant : OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  Result := InterfaceVariant.Item[Index];
end;

function TCertificatesProperties.Get_Count: Integer;
begin
    Result := DefaultInterface.Count;
end;

{$ENDIF}

class function CoChain.Create: IChain2;
begin
  Result := CreateComObject(CLASS_Chain) as IChain2;
end;

class function CoChain.CreateRemote(const MachineName: string): IChain2;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Chain) as IChain2;
end;

procedure TChain.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{550C8FFB-4DC0-4756-828C-862E6D0AE74F}';
    IntfIID:   '{CA65D842-2110-4073-AEE3-D0AA5F56C421}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TChain.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IChain2;
  end;
end;

procedure TChain.ConnectTo(svrIntf: IChain2);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TChain.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TChain.GetDefaultInterface: IChain2;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TChain.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TChainProperties.Create(Self);
{$ENDIF}
end;

destructor TChain.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TChain.GetServerProperties: TChainProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TChain.Get_Certificates: ICertificates;
begin
    Result := DefaultInterface.Certificates;
end;

function TChain.Get_Status(Index: Integer): Integer;
begin
    Result := DefaultInterface.Status[Index];
end;

function TChain.Build(const pICertificate: ICertificate): WordBool;
begin
  Result := DefaultInterface.Build(pICertificate);
end;

function TChain.CertificatePolicies: IOIDs;
begin
  Result := DefaultInterface.CertificatePolicies;
end;

function TChain.ApplicationPolicies: IOIDs;
begin
  Result := DefaultInterface.ApplicationPolicies;
end;

function TChain.ExtendedErrorInfo(Index: Integer): WideString;
begin
  Result := DefaultInterface.ExtendedErrorInfo(Index);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TChainProperties.Create(AServer: TChain);
begin
  inherited Create;
  FServer := AServer;
end;

function TChainProperties.GetDefaultInterface: IChain2;
begin
  Result := FServer.DefaultInterface;
end;

function TChainProperties.Get_Certificates: ICertificates;
begin
    Result := DefaultInterface.Certificates;
end;

function TChainProperties.Get_Status(Index: Integer): Integer;
begin
    Result := DefaultInterface.Status[Index];
end;

{$ENDIF}

class function CoStore.Create: IStore3;
begin
  Result := CreateComObject(CLASS_Store) as IStore3;
end;

class function CoStore.CreateRemote(const MachineName: string): IStore3;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Store) as IStore3;
end;

procedure TStore.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{91D221C4-0CD4-461C-A728-01D509321556}';
    IntfIID:   '{F701F8EC-31C7-48FB-B621-5DE417C3A607}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TStore.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IStore3;
  end;
end;

procedure TStore.ConnectTo(svrIntf: IStore3);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TStore.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TStore.GetDefaultInterface: IStore3;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TStore.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TStoreProperties.Create(Self);
{$ENDIF}
end;

destructor TStore.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TStore.GetServerProperties: TStoreProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TStore.Get_Certificates: ICertificates;
begin
    Result := DefaultInterface.Certificates;
end;

function TStore.Get_Name: WideString;
begin
    Result := DefaultInterface.Name;
end;

function TStore.Get_Location: CAPICOM_STORE_LOCATION;
begin
    Result := DefaultInterface.Location;
end;

procedure TStore.Open(StoreLocation: CAPICOM_STORE_LOCATION; const StoreName: WideString; 
                      OpenMode: CAPICOM_STORE_OPEN_MODE);
begin
  DefaultInterface.Open(StoreLocation, StoreName, OpenMode);
end;

procedure TStore.Add(const pVal: ICertificate);
begin
  DefaultInterface.Add(pVal);
end;

procedure TStore.Remove(const pVal: ICertificate);
begin
  DefaultInterface.Remove(pVal);
end;

function TStore.Export(SaveAs: CAPICOM_STORE_SAVE_AS_TYPE; EncodingType: CAPICOM_ENCODING_TYPE): WideString;
begin
  Result := DefaultInterface.Export(SaveAs, EncodingType);
end;

procedure TStore.Import(const EncodedStore: WideString);
begin
  DefaultInterface.Import(EncodedStore);
end;

procedure TStore.Load(const FileName: WideString; const Password: WideString; 
                      KeyStorageFlag: CAPICOM_KEY_STORAGE_FLAG);
begin
  DefaultInterface.Load(FileName, Password, KeyStorageFlag);
end;

function TStore.Delete: WordBool;
begin
  Result := DefaultInterface.Delete;
end;

procedure TStore.Close;
begin
  DefaultInterface.Close;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TStoreProperties.Create(AServer: TStore);
begin
  inherited Create;
  FServer := AServer;
end;

function TStoreProperties.GetDefaultInterface: IStore3;
begin
  Result := FServer.DefaultInterface;
end;

function TStoreProperties.Get_Certificates: ICertificates;
begin
    Result := DefaultInterface.Certificates;
end;

function TStoreProperties.Get_Name: WideString;
begin
    Result := DefaultInterface.Name;
end;

function TStoreProperties.Get_Location: CAPICOM_STORE_LOCATION;
begin
    Result := DefaultInterface.Location;
end;

{$ENDIF}

class function CoAttribute.Create: IAttribute;
begin
  Result := CreateComObject(CLASS_Attribute) as IAttribute;
end;

class function CoAttribute.CreateRemote(const MachineName: string): IAttribute;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Attribute) as IAttribute;
end;

procedure TAttribute.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{54BA1E8F-818D-407F-949D-BAE1692C5C18}';
    IntfIID:   '{B17A8D78-B5A6-45F7-BA21-01AB94B08415}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TAttribute.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IAttribute;
  end;
end;

procedure TAttribute.ConnectTo(svrIntf: IAttribute);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TAttribute.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TAttribute.GetDefaultInterface: IAttribute;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TAttribute.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TAttributeProperties.Create(Self);
{$ENDIF}
end;

destructor TAttribute.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TAttribute.GetServerProperties: TAttributeProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TAttribute.Get_Name: CAPICOM_ATTRIBUTE;
begin
    Result := DefaultInterface.Name;
end;

procedure TAttribute.Set_Name(pVal: CAPICOM_ATTRIBUTE);
begin
  DefaultInterface.Set_Name(pVal);
end;

function TAttribute.Get_Value: OleVariant;
var
  InterfaceVariant : OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  Result := InterfaceVariant.Value;
end;

procedure TAttribute.Set_Value(pVal: OleVariant);
begin
  DefaultInterface.Set_Value(pVal);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TAttributeProperties.Create(AServer: TAttribute);
begin
  inherited Create;
  FServer := AServer;
end;

function TAttributeProperties.GetDefaultInterface: IAttribute;
begin
  Result := FServer.DefaultInterface;
end;

function TAttributeProperties.Get_Name: CAPICOM_ATTRIBUTE;
begin
    Result := DefaultInterface.Name;
end;

procedure TAttributeProperties.Set_Name(pVal: CAPICOM_ATTRIBUTE);
begin
  DefaultInterface.Set_Name(pVal);
end;

function TAttributeProperties.Get_Value: OleVariant;
var
  InterfaceVariant : OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  Result := InterfaceVariant.Value;
end;

procedure TAttributeProperties.Set_Value(pVal: OleVariant);
begin
  DefaultInterface.Set_Value(pVal);
end;

{$ENDIF}

class function CoAttributes.Create: IAttributes;
begin
  Result := CreateComObject(CLASS_Attributes) as IAttributes;
end;

class function CoAttributes.CreateRemote(const MachineName: string): IAttributes;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Attributes) as IAttributes;
end;

class function CoSigner.Create: ISigner2;
begin
  Result := CreateComObject(CLASS_Signer) as ISigner2;
end;

class function CoSigner.CreateRemote(const MachineName: string): ISigner2;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Signer) as ISigner2;
end;

procedure TSigner.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{60A9863A-11FD-4080-850E-A8E184FC3A3C}';
    IntfIID:   '{625B1F55-C720-41D6-9ECF-BA59F9B85F17}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TSigner.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as ISigner2;
  end;
end;

procedure TSigner.ConnectTo(svrIntf: ISigner2);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TSigner.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TSigner.GetDefaultInterface: ISigner2;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TSigner.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TSignerProperties.Create(Self);
{$ENDIF}
end;

destructor TSigner.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TSigner.GetServerProperties: TSignerProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TSigner.Get_Certificate: ICertificate;
begin
    Result := DefaultInterface.Certificate;
end;

procedure TSigner.Set_Certificate(const pVal: ICertificate);
begin
  DefaultInterface.Set_Certificate(pVal);
end;

function TSigner.Get_AuthenticatedAttributes: IAttributes;
begin
    Result := DefaultInterface.AuthenticatedAttributes;
end;

function TSigner.Get_Chain: IChain;
begin
    Result := DefaultInterface.Chain;
end;

function TSigner.Get_Options: CAPICOM_CERTIFICATE_INCLUDE_OPTION;
begin
    Result := DefaultInterface.Options;
end;

procedure TSigner.Set_Options(pVal: CAPICOM_CERTIFICATE_INCLUDE_OPTION);
begin
  DefaultInterface.Set_Options(pVal);
end;

procedure TSigner.Load(const FileName: WideString; const Password: WideString);
begin
  DefaultInterface.Load(FileName, Password);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TSignerProperties.Create(AServer: TSigner);
begin
  inherited Create;
  FServer := AServer;
end;

function TSignerProperties.GetDefaultInterface: ISigner2;
begin
  Result := FServer.DefaultInterface;
end;

function TSignerProperties.Get_Certificate: ICertificate;
begin
    Result := DefaultInterface.Certificate;
end;

procedure TSignerProperties.Set_Certificate(const pVal: ICertificate);
begin
  DefaultInterface.Set_Certificate(pVal);
end;

function TSignerProperties.Get_AuthenticatedAttributes: IAttributes;
begin
    Result := DefaultInterface.AuthenticatedAttributes;
end;

function TSignerProperties.Get_Chain: IChain;
begin
    Result := DefaultInterface.Chain;
end;

function TSignerProperties.Get_Options: CAPICOM_CERTIFICATE_INCLUDE_OPTION;
begin
    Result := DefaultInterface.Options;
end;

procedure TSignerProperties.Set_Options(pVal: CAPICOM_CERTIFICATE_INCLUDE_OPTION);
begin
  DefaultInterface.Set_Options(pVal);
end;

{$ENDIF}

class function CoSigners.Create: ISigners;
begin
  Result := CreateComObject(CLASS_Signers) as ISigners;
end;

class function CoSigners.CreateRemote(const MachineName: string): ISigners;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Signers) as ISigners;
end;

class function CoSignedData.Create: ISignedData;
begin
  Result := CreateComObject(CLASS_SignedData) as ISignedData;
end;

class function CoSignedData.CreateRemote(const MachineName: string): ISignedData;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SignedData) as ISignedData;
end;

procedure TSignedData.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{94AFFFCC-6C05-4814-B123-A941105AA77F}';
    IntfIID:   '{AE9C454B-FC65-4C10-B130-CD9B45BA948B}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TSignedData.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as ISignedData;
  end;
end;

procedure TSignedData.ConnectTo(svrIntf: ISignedData);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TSignedData.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TSignedData.GetDefaultInterface: ISignedData;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TSignedData.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TSignedDataProperties.Create(Self);
{$ENDIF}
end;

destructor TSignedData.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TSignedData.GetServerProperties: TSignedDataProperties;
begin
  Result := FProps;
end;
{$ENDIF}

procedure TSignedData.Set_Content(const pVal: WideString);
  { Warning: The property Content has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Content := pVal;
end;

function TSignedData.Get_Content: WideString;
begin
    Result := DefaultInterface.Content;
end;

function TSignedData.Get_Signers: ISigners;
begin
    Result := DefaultInterface.Signers;
end;

function TSignedData.Get_Certificates: ICertificates;
begin
    Result := DefaultInterface.Certificates;
end;

function TSignedData.Sign(const pSigner: ISigner; bDetached: WordBool; 
                          EncodingType: CAPICOM_ENCODING_TYPE): WideString;
begin
  Result := DefaultInterface.Sign(pSigner, bDetached, EncodingType);
end;

function TSignedData.CoSign(const pSigner: ISigner; EncodingType: CAPICOM_ENCODING_TYPE): WideString;
begin
  Result := DefaultInterface.CoSign(pSigner, EncodingType);
end;

procedure TSignedData.Verify(const SignedMessage: WideString; bDetached: WordBool; 
                             VerifyFlag: CAPICOM_SIGNED_DATA_VERIFY_FLAG);
begin
  DefaultInterface.Verify(SignedMessage, bDetached, VerifyFlag);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TSignedDataProperties.Create(AServer: TSignedData);
begin
  inherited Create;
  FServer := AServer;
end;

function TSignedDataProperties.GetDefaultInterface: ISignedData;
begin
  Result := FServer.DefaultInterface;
end;

procedure TSignedDataProperties.Set_Content(const pVal: WideString);
  { Warning: The property Content has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Content := pVal;
end;

function TSignedDataProperties.Get_Content: WideString;
begin
    Result := DefaultInterface.Content;
end;

function TSignedDataProperties.Get_Signers: ISigners;
begin
    Result := DefaultInterface.Signers;
end;

function TSignedDataProperties.Get_Certificates: ICertificates;
begin
    Result := DefaultInterface.Certificates;
end;

{$ENDIF}

class function CoAlgorithm.Create: IAlgorithm;
begin
  Result := CreateComObject(CLASS_Algorithm) as IAlgorithm;
end;

class function CoAlgorithm.CreateRemote(const MachineName: string): IAlgorithm;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Algorithm) as IAlgorithm;
end;

class function CoRecipients.Create: IRecipients;
begin
  Result := CreateComObject(CLASS_Recipients) as IRecipients;
end;

class function CoRecipients.CreateRemote(const MachineName: string): IRecipients;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Recipients) as IRecipients;
end;

class function CoEnvelopedData.Create: IEnvelopedData;
begin
  Result := CreateComObject(CLASS_EnvelopedData) as IEnvelopedData;
end;

class function CoEnvelopedData.CreateRemote(const MachineName: string): IEnvelopedData;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_EnvelopedData) as IEnvelopedData;
end;

procedure TEnvelopedData.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{F3A12E08-EDE9-4160-8B51-334D982A9AD0}';
    IntfIID:   '{F6CB6A20-CC18-4424-AE57-6F2AA3DC2059}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TEnvelopedData.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IEnvelopedData;
  end;
end;

procedure TEnvelopedData.ConnectTo(svrIntf: IEnvelopedData);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TEnvelopedData.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TEnvelopedData.GetDefaultInterface: IEnvelopedData;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TEnvelopedData.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TEnvelopedDataProperties.Create(Self);
{$ENDIF}
end;

destructor TEnvelopedData.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TEnvelopedData.GetServerProperties: TEnvelopedDataProperties;
begin
  Result := FProps;
end;
{$ENDIF}

procedure TEnvelopedData.Set_Content(const pVal: WideString);
  { Warning: The property Content has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Content := pVal;
end;

function TEnvelopedData.Get_Content: WideString;
begin
    Result := DefaultInterface.Content;
end;

function TEnvelopedData.Get_Algorithm: IAlgorithm;
begin
    Result := DefaultInterface.Algorithm;
end;

function TEnvelopedData.Get_Recipients: IRecipients;
begin
    Result := DefaultInterface.Recipients;
end;

function TEnvelopedData.Encrypt(EncodingType: CAPICOM_ENCODING_TYPE): WideString;
begin
  Result := DefaultInterface.Encrypt(EncodingType);
end;

procedure TEnvelopedData.Decrypt(const EnvelopedMessage: WideString);
begin
  DefaultInterface.Decrypt(EnvelopedMessage);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TEnvelopedDataProperties.Create(AServer: TEnvelopedData);
begin
  inherited Create;
  FServer := AServer;
end;

function TEnvelopedDataProperties.GetDefaultInterface: IEnvelopedData;
begin
  Result := FServer.DefaultInterface;
end;

procedure TEnvelopedDataProperties.Set_Content(const pVal: WideString);
  { Warning: The property Content has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Content := pVal;
end;

function TEnvelopedDataProperties.Get_Content: WideString;
begin
    Result := DefaultInterface.Content;
end;

function TEnvelopedDataProperties.Get_Algorithm: IAlgorithm;
begin
    Result := DefaultInterface.Algorithm;
end;

function TEnvelopedDataProperties.Get_Recipients: IRecipients;
begin
    Result := DefaultInterface.Recipients;
end;

{$ENDIF}

class function CoEncryptedData.Create: IEncryptedData;
begin
  Result := CreateComObject(CLASS_EncryptedData) as IEncryptedData;
end;

class function CoEncryptedData.CreateRemote(const MachineName: string): IEncryptedData;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_EncryptedData) as IEncryptedData;
end;

procedure TEncryptedData.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{A440BD76-CFE1-4D46-AB1F-15F238437A3D}';
    IntfIID:   '{C4778A66-972F-42E4-87C5-5CC16F7931CA}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TEncryptedData.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IEncryptedData;
  end;
end;

procedure TEncryptedData.ConnectTo(svrIntf: IEncryptedData);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TEncryptedData.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TEncryptedData.GetDefaultInterface: IEncryptedData;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TEncryptedData.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TEncryptedDataProperties.Create(Self);
{$ENDIF}
end;

destructor TEncryptedData.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TEncryptedData.GetServerProperties: TEncryptedDataProperties;
begin
  Result := FProps;
end;
{$ENDIF}

procedure TEncryptedData.Set_Content(const pVal: WideString);
  { Warning: The property Content has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Content := pVal;
end;

function TEncryptedData.Get_Content: WideString;
begin
    Result := DefaultInterface.Content;
end;

function TEncryptedData.Get_Algorithm: IAlgorithm;
begin
    Result := DefaultInterface.Algorithm;
end;

procedure TEncryptedData.SetSecret(const newVal: WideString; SecretType: CAPICOM_SECRET_TYPE);
begin
  DefaultInterface.SetSecret(newVal, SecretType);
end;

function TEncryptedData.Encrypt(EncodingType: CAPICOM_ENCODING_TYPE): WideString;
begin
  Result := DefaultInterface.Encrypt(EncodingType);
end;

procedure TEncryptedData.Decrypt(const EncryptedMessage: WideString);
begin
  DefaultInterface.Decrypt(EncryptedMessage);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TEncryptedDataProperties.Create(AServer: TEncryptedData);
begin
  inherited Create;
  FServer := AServer;
end;

function TEncryptedDataProperties.GetDefaultInterface: IEncryptedData;
begin
  Result := FServer.DefaultInterface;
end;

procedure TEncryptedDataProperties.Set_Content(const pVal: WideString);
  { Warning: The property Content has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Content := pVal;
end;

function TEncryptedDataProperties.Get_Content: WideString;
begin
    Result := DefaultInterface.Content;
end;

function TEncryptedDataProperties.Get_Algorithm: IAlgorithm;
begin
    Result := DefaultInterface.Algorithm;
end;

{$ENDIF}

class function CoOID.Create: IOID;
begin
  Result := CreateComObject(CLASS_OID) as IOID;
end;

class function CoOID.CreateRemote(const MachineName: string): IOID;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_OID) as IOID;
end;

procedure TOID.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{7BF3AC5C-CC84-429A-ACA5-74D916AD6B8C}';
    IntfIID:   '{208E5E9B-58B1-4086-970F-161B582A846F}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TOID.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IOID;
  end;
end;

procedure TOID.ConnectTo(svrIntf: IOID);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TOID.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TOID.GetDefaultInterface: IOID;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TOID.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TOIDProperties.Create(Self);
{$ENDIF}
end;

destructor TOID.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TOID.GetServerProperties: TOIDProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TOID.Get_Name: CAPICOM_OID;
begin
    Result := DefaultInterface.Name;
end;

procedure TOID.Set_Name(pVal: CAPICOM_OID);
begin
  DefaultInterface.Set_Name(pVal);
end;

function TOID.Get_FriendlyName: WideString;
begin
    Result := DefaultInterface.FriendlyName;
end;

procedure TOID.Set_FriendlyName(const pVal: WideString);
  { Warning: The property FriendlyName has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.FriendlyName := pVal;
end;

function TOID.Get_Value: WideString;
begin
    Result := DefaultInterface.Value;
end;

procedure TOID.Set_Value(const pVal: WideString);
  { Warning: The property Value has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Value := pVal;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TOIDProperties.Create(AServer: TOID);
begin
  inherited Create;
  FServer := AServer;
end;

function TOIDProperties.GetDefaultInterface: IOID;
begin
  Result := FServer.DefaultInterface;
end;

function TOIDProperties.Get_Name: CAPICOM_OID;
begin
    Result := DefaultInterface.Name;
end;

procedure TOIDProperties.Set_Name(pVal: CAPICOM_OID);
begin
  DefaultInterface.Set_Name(pVal);
end;

function TOIDProperties.Get_FriendlyName: WideString;
begin
    Result := DefaultInterface.FriendlyName;
end;

procedure TOIDProperties.Set_FriendlyName(const pVal: WideString);
  { Warning: The property FriendlyName has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.FriendlyName := pVal;
end;

function TOIDProperties.Get_Value: WideString;
begin
    Result := DefaultInterface.Value;
end;

procedure TOIDProperties.Set_Value(const pVal: WideString);
  { Warning: The property Value has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Value := pVal;
end;

{$ENDIF}

class function CoOIDs.Create: IOIDs;
begin
  Result := CreateComObject(CLASS_OIDs) as IOIDs;
end;

class function CoOIDs.CreateRemote(const MachineName: string): IOIDs;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_OIDs) as IOIDs;
end;

class function CoNoticeNumbers.Create: INoticeNumbers;
begin
  Result := CreateComObject(CLASS_NoticeNumbers) as INoticeNumbers;
end;

class function CoNoticeNumbers.CreateRemote(const MachineName: string): INoticeNumbers;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_NoticeNumbers) as INoticeNumbers;
end;

class function CoQualifier.Create: IQualifier;
begin
  Result := CreateComObject(CLASS_Qualifier) as IQualifier;
end;

class function CoQualifier.CreateRemote(const MachineName: string): IQualifier;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Qualifier) as IQualifier;
end;

class function CoQualifiers.Create: IQualifiers;
begin
  Result := CreateComObject(CLASS_Qualifiers) as IQualifiers;
end;

class function CoQualifiers.CreateRemote(const MachineName: string): IQualifiers;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Qualifiers) as IQualifiers;
end;

class function CoPolicyInformation.Create: IPolicyInformation;
begin
  Result := CreateComObject(CLASS_PolicyInformation) as IPolicyInformation;
end;

class function CoPolicyInformation.CreateRemote(const MachineName: string): IPolicyInformation;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_PolicyInformation) as IPolicyInformation;
end;

class function CoCertificatePolicies.Create: ICertificatePolicies;
begin
  Result := CreateComObject(CLASS_CertificatePolicies) as ICertificatePolicies;
end;

class function CoCertificatePolicies.CreateRemote(const MachineName: string): ICertificatePolicies;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CertificatePolicies) as ICertificatePolicies;
end;

class function CoEncodedData.Create: IEncodedData;
begin
  Result := CreateComObject(CLASS_EncodedData) as IEncodedData;
end;

class function CoEncodedData.CreateRemote(const MachineName: string): IEncodedData;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_EncodedData) as IEncodedData;
end;

class function CoExtension.Create: IExtension;
begin
  Result := CreateComObject(CLASS_Extension) as IExtension;
end;

class function CoExtension.CreateRemote(const MachineName: string): IExtension;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Extension) as IExtension;
end;

class function CoExtensions.Create: IExtensions;
begin
  Result := CreateComObject(CLASS_Extensions) as IExtensions;
end;

class function CoExtensions.CreateRemote(const MachineName: string): IExtensions;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Extensions) as IExtensions;
end;

class function CoExtendedProperty.Create: IExtendedProperty;
begin
  Result := CreateComObject(CLASS_ExtendedProperty) as IExtendedProperty;
end;

class function CoExtendedProperty.CreateRemote(const MachineName: string): IExtendedProperty;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ExtendedProperty) as IExtendedProperty;
end;

procedure TExtendedProperty.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{9E7EA907-5810-4FCA-B817-CD0BBA8496FC}';
    IntfIID:   '{ECB8A5C8-562C-4989-B49D-FA37D40F8FC4}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TExtendedProperty.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IExtendedProperty;
  end;
end;

procedure TExtendedProperty.ConnectTo(svrIntf: IExtendedProperty);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TExtendedProperty.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TExtendedProperty.GetDefaultInterface: IExtendedProperty;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TExtendedProperty.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TExtendedPropertyProperties.Create(Self);
{$ENDIF}
end;

destructor TExtendedProperty.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TExtendedProperty.GetServerProperties: TExtendedPropertyProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TExtendedProperty.Get_PropID: CAPICOM_PROPID;
begin
    Result := DefaultInterface.PropID;
end;

procedure TExtendedProperty.Set_PropID(pVal: CAPICOM_PROPID);
begin
  DefaultInterface.Set_PropID(pVal);
end;

function TExtendedProperty.Get_Value(EncodingType: CAPICOM_ENCODING_TYPE): WideString;
begin
    Result := DefaultInterface.Value[EncodingType];
end;

procedure TExtendedProperty.Set_Value(EncodingType: CAPICOM_ENCODING_TYPE; const pVal: WideString);
  { Warning: The property Value has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Value := pVal;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TExtendedPropertyProperties.Create(AServer: TExtendedProperty);
begin
  inherited Create;
  FServer := AServer;
end;

function TExtendedPropertyProperties.GetDefaultInterface: IExtendedProperty;
begin
  Result := FServer.DefaultInterface;
end;

function TExtendedPropertyProperties.Get_PropID: CAPICOM_PROPID;
begin
    Result := DefaultInterface.PropID;
end;

procedure TExtendedPropertyProperties.Set_PropID(pVal: CAPICOM_PROPID);
begin
  DefaultInterface.Set_PropID(pVal);
end;

function TExtendedPropertyProperties.Get_Value(EncodingType: CAPICOM_ENCODING_TYPE): WideString;
begin
    Result := DefaultInterface.Value[EncodingType];
end;

procedure TExtendedPropertyProperties.Set_Value(EncodingType: CAPICOM_ENCODING_TYPE; 
                                                const pVal: WideString);
  { Warning: The property Value has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Value := pVal;
end;

{$ENDIF}

class function CoExtendedProperties.Create: IExtendedProperties;
begin
  Result := CreateComObject(CLASS_ExtendedProperties) as IExtendedProperties;
end;

class function CoExtendedProperties.CreateRemote(const MachineName: string): IExtendedProperties;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ExtendedProperties) as IExtendedProperties;
end;

class function CoTemplate.Create: ITemplate;
begin
  Result := CreateComObject(CLASS_Template) as ITemplate;
end;

class function CoTemplate.CreateRemote(const MachineName: string): ITemplate;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Template) as ITemplate;
end;

class function CoPublicKey.Create: IPublicKey;
begin
  Result := CreateComObject(CLASS_PublicKey) as IPublicKey;
end;

class function CoPublicKey.CreateRemote(const MachineName: string): IPublicKey;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_PublicKey) as IPublicKey;
end;

class function CoPrivateKey.Create: IPrivateKey;
begin
  Result := CreateComObject(CLASS_PrivateKey) as IPrivateKey;
end;

class function CoPrivateKey.CreateRemote(const MachineName: string): IPrivateKey;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_PrivateKey) as IPrivateKey;
end;

procedure TPrivateKey.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{03ACC284-B757-4B8F-9951-86E600D2CD06}';
    IntfIID:   '{659DEDC3-6C85-42DB-8527-EFCB21742862}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TPrivateKey.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IPrivateKey;
  end;
end;

procedure TPrivateKey.ConnectTo(svrIntf: IPrivateKey);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TPrivateKey.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TPrivateKey.GetDefaultInterface: IPrivateKey;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TPrivateKey.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TPrivateKeyProperties.Create(Self);
{$ENDIF}
end;

destructor TPrivateKey.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TPrivateKey.GetServerProperties: TPrivateKeyProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TPrivateKey.Get_ContainerName: WideString;
begin
    Result := DefaultInterface.ContainerName;
end;

function TPrivateKey.Get_UniqueContainerName: WideString;
begin
    Result := DefaultInterface.UniqueContainerName;
end;

function TPrivateKey.Get_ProviderName: WideString;
begin
    Result := DefaultInterface.ProviderName;
end;

function TPrivateKey.Get_ProviderType: CAPICOM_PROV_TYPE;
begin
    Result := DefaultInterface.ProviderType;
end;

function TPrivateKey.Get_KeySpec: CAPICOM_KEY_SPEC;
begin
    Result := DefaultInterface.KeySpec;
end;

function TPrivateKey.IsAccessible: WordBool;
begin
  Result := DefaultInterface.IsAccessible;
end;

function TPrivateKey.IsProtected: WordBool;
begin
  Result := DefaultInterface.IsProtected;
end;

function TPrivateKey.IsExportable: WordBool;
begin
  Result := DefaultInterface.IsExportable;
end;

function TPrivateKey.IsRemovable: WordBool;
begin
  Result := DefaultInterface.IsRemovable;
end;

function TPrivateKey.IsMachineKeyset: WordBool;
begin
  Result := DefaultInterface.IsMachineKeyset;
end;

function TPrivateKey.IsHardwareDevice: WordBool;
begin
  Result := DefaultInterface.IsHardwareDevice;
end;

procedure TPrivateKey.Open(const ContainerName: WideString; const ProviderName: WideString; 
                           ProviderType: CAPICOM_PROV_TYPE; KeySpec: CAPICOM_KEY_SPEC; 
                           StoreLocation: CAPICOM_STORE_LOCATION; bCheckExistence: WordBool);
begin
  DefaultInterface.Open(ContainerName, ProviderName, ProviderType, KeySpec, StoreLocation, 
                        bCheckExistence);
end;

procedure TPrivateKey.Delete;
begin
  DefaultInterface.Delete;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TPrivateKeyProperties.Create(AServer: TPrivateKey);
begin
  inherited Create;
  FServer := AServer;
end;

function TPrivateKeyProperties.GetDefaultInterface: IPrivateKey;
begin
  Result := FServer.DefaultInterface;
end;

function TPrivateKeyProperties.Get_ContainerName: WideString;
begin
    Result := DefaultInterface.ContainerName;
end;

function TPrivateKeyProperties.Get_UniqueContainerName: WideString;
begin
    Result := DefaultInterface.UniqueContainerName;
end;

function TPrivateKeyProperties.Get_ProviderName: WideString;
begin
    Result := DefaultInterface.ProviderName;
end;

function TPrivateKeyProperties.Get_ProviderType: CAPICOM_PROV_TYPE;
begin
    Result := DefaultInterface.ProviderType;
end;

function TPrivateKeyProperties.Get_KeySpec: CAPICOM_KEY_SPEC;
begin
    Result := DefaultInterface.KeySpec;
end;

{$ENDIF}

class function CoSignedCode.Create: ISignedCode;
begin
  Result := CreateComObject(CLASS_SignedCode) as ISignedCode;
end;

class function CoSignedCode.CreateRemote(const MachineName: string): ISignedCode;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SignedCode) as ISignedCode;
end;

procedure TSignedCode.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{8C3E4934-9FA4-4693-9253-A29A05F99186}';
    IntfIID:   '{84FBCB95-5600-404C-9187-AC25B4CD6E94}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TSignedCode.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as ISignedCode;
  end;
end;

procedure TSignedCode.ConnectTo(svrIntf: ISignedCode);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TSignedCode.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TSignedCode.GetDefaultInterface: ISignedCode;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TSignedCode.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TSignedCodeProperties.Create(Self);
{$ENDIF}
end;

destructor TSignedCode.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TSignedCode.GetServerProperties: TSignedCodeProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TSignedCode.Get_FileName: WideString;
begin
    Result := DefaultInterface.FileName;
end;

procedure TSignedCode.Set_FileName(const pVal: WideString);
  { Warning: The property FileName has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.FileName := pVal;
end;

function TSignedCode.Get_Description: WideString;
begin
    Result := DefaultInterface.Description;
end;

procedure TSignedCode.Set_Description(const pVal: WideString);
  { Warning: The property Description has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Description := pVal;
end;

function TSignedCode.Get_DescriptionURL: WideString;
begin
    Result := DefaultInterface.DescriptionURL;
end;

procedure TSignedCode.Set_DescriptionURL(const pVal: WideString);
  { Warning: The property DescriptionURL has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.DescriptionURL := pVal;
end;

function TSignedCode.Get_Signer: ISigner2;
begin
    Result := DefaultInterface.Signer;
end;

function TSignedCode.Get_TimeStamper: ISigner2;
begin
    Result := DefaultInterface.TimeStamper;
end;

function TSignedCode.Get_Certificates: ICertificates2;
begin
    Result := DefaultInterface.Certificates;
end;

procedure TSignedCode.Sign(const pISigner2: ISigner2);
begin
  DefaultInterface.Sign(pISigner2);
end;

procedure TSignedCode.Timestamp(const URL: WideString);
begin
  DefaultInterface.Timestamp(URL);
end;

procedure TSignedCode.Verify(bUIAllowed: WordBool);
begin
  DefaultInterface.Verify(bUIAllowed);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TSignedCodeProperties.Create(AServer: TSignedCode);
begin
  inherited Create;
  FServer := AServer;
end;

function TSignedCodeProperties.GetDefaultInterface: ISignedCode;
begin
  Result := FServer.DefaultInterface;
end;

function TSignedCodeProperties.Get_FileName: WideString;
begin
    Result := DefaultInterface.FileName;
end;

procedure TSignedCodeProperties.Set_FileName(const pVal: WideString);
  { Warning: The property FileName has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.FileName := pVal;
end;

function TSignedCodeProperties.Get_Description: WideString;
begin
    Result := DefaultInterface.Description;
end;

procedure TSignedCodeProperties.Set_Description(const pVal: WideString);
  { Warning: The property Description has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Description := pVal;
end;

function TSignedCodeProperties.Get_DescriptionURL: WideString;
begin
    Result := DefaultInterface.DescriptionURL;
end;

procedure TSignedCodeProperties.Set_DescriptionURL(const pVal: WideString);
  { Warning: The property DescriptionURL has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.DescriptionURL := pVal;
end;

function TSignedCodeProperties.Get_Signer: ISigner2;
begin
    Result := DefaultInterface.Signer;
end;

function TSignedCodeProperties.Get_TimeStamper: ISigner2;
begin
    Result := DefaultInterface.TimeStamper;
end;

function TSignedCodeProperties.Get_Certificates: ICertificates2;
begin
    Result := DefaultInterface.Certificates;
end;

{$ENDIF}

class function CoHashedData.Create: IHashedData;
begin
  Result := CreateComObject(CLASS_HashedData) as IHashedData;
end;

class function CoHashedData.CreateRemote(const MachineName: string): IHashedData;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_HashedData) as IHashedData;
end;

procedure THashedData.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{CE32ABF6-475D-41F6-BF82-D27F03E3D38B}';
    IntfIID:   '{9F7F23E8-06F4-42E8-B965-5CBD044BF27F}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure THashedData.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IHashedData;
  end;
end;

procedure THashedData.ConnectTo(svrIntf: IHashedData);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure THashedData.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function THashedData.GetDefaultInterface: IHashedData;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor THashedData.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := THashedDataProperties.Create(Self);
{$ENDIF}
end;

destructor THashedData.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function THashedData.GetServerProperties: THashedDataProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function THashedData.Get_Value: WideString;
begin
    Result := DefaultInterface.Value;
end;

function THashedData.Get_Algorithm: CAPICOM_HASH_ALGORITHM;
begin
    Result := DefaultInterface.Algorithm;
end;

procedure THashedData.Set_Algorithm(pVal: CAPICOM_HASH_ALGORITHM);
begin
  DefaultInterface.Set_Algorithm(pVal);
end;

procedure THashedData.Hash(const newVal: WideString);
begin
  DefaultInterface.Hash(newVal);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor THashedDataProperties.Create(AServer: THashedData);
begin
  inherited Create;
  FServer := AServer;
end;

function THashedDataProperties.GetDefaultInterface: IHashedData;
begin
  Result := FServer.DefaultInterface;
end;

function THashedDataProperties.Get_Value: WideString;
begin
    Result := DefaultInterface.Value;
end;

function THashedDataProperties.Get_Algorithm: CAPICOM_HASH_ALGORITHM;
begin
    Result := DefaultInterface.Algorithm;
end;

procedure THashedDataProperties.Set_Algorithm(pVal: CAPICOM_HASH_ALGORITHM);
begin
  DefaultInterface.Set_Algorithm(pVal);
end;

{$ENDIF}

class function CoUtilities.Create: IUtilities;
begin
  Result := CreateComObject(CLASS_Utilities) as IUtilities;
end;

class function CoUtilities.CreateRemote(const MachineName: string): IUtilities;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Utilities) as IUtilities;
end;

procedure TUtilities.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{22A85CE1-F011-4231-B9E4-7E7A0438F71B}';
    IntfIID:   '{EB166CF6-2AE6-44DA-BD96-0C1635D183FE}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TUtilities.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IUtilities;
  end;
end;

procedure TUtilities.ConnectTo(svrIntf: IUtilities);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TUtilities.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TUtilities.GetDefaultInterface: IUtilities;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TUtilities.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TUtilitiesProperties.Create(Self);
{$ENDIF}
end;

destructor TUtilities.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TUtilities.GetServerProperties: TUtilitiesProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TUtilities.GetRandom(Length: Integer; EncodingType: CAPICOM_ENCODING_TYPE): WideString;
begin
  Result := DefaultInterface.GetRandom(Length, EncodingType);
end;

function TUtilities.Base64Encode(const SrcString: WideString): WideString;
begin
  Result := DefaultInterface.Base64Encode(SrcString);
end;

function TUtilities.Base64Decode(const EncodedString: WideString): WideString;
begin
  Result := DefaultInterface.Base64Decode(EncodedString);
end;

function TUtilities.BinaryToHex(const BinaryString: WideString): WideString;
begin
  Result := DefaultInterface.BinaryToHex(BinaryString);
end;

function TUtilities.HexToBinary(const HexString: WideString): WideString;
begin
  Result := DefaultInterface.HexToBinary(HexString);
end;

function TUtilities.BinaryStringToByteArray(const BinaryString: WideString): OleVariant;
begin
  Result := DefaultInterface.BinaryStringToByteArray(BinaryString);
end;

function TUtilities.ByteArrayToBinaryString(varByteArray: OleVariant): WideString;
begin
  Result := DefaultInterface.ByteArrayToBinaryString(varByteArray);
end;

function TUtilities.LocalTimeToUTCTime(LocalTime: TDateTime): TDateTime;
begin
  Result := DefaultInterface.LocalTimeToUTCTime(LocalTime);
end;

function TUtilities.UTCTimeToLocalTime(UTCTime: TDateTime): TDateTime;
begin
  Result := DefaultInterface.UTCTimeToLocalTime(UTCTime);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TUtilitiesProperties.Create(AServer: TUtilities);
begin
  inherited Create;
  FServer := AServer;
end;

function TUtilitiesProperties.GetDefaultInterface: IUtilities;
begin
  Result := FServer.DefaultInterface;
end;

{$ENDIF}

procedure Register;
begin
  RegisterComponents(dtlServerPage, [TSettings, TCertificate, TCertificates, TChain, 
    TStore, TAttribute, TSigner, TSignedData, TEnvelopedData, 
    TEncryptedData, TOID, TExtendedProperty, TPrivateKey, TSignedCode, 
    THashedData, TUtilities]);
end;

end.
