unit G780Interface;

interface

uses
  Androidapi.JNIBridge, 
  Androidapi.JNI.JavaTypes, 
  Androidapi.JNI.Os, 
  Androidapi.JNI.Widget,
  Androidapi.JNI.App, 
  Androidapi.JNI.GraphicsContentViewText, 
  Androidapi.JNI.Bluetooth, 
  Androidapi.JNI.Java.Security;

type
  JBankCard = interface;
  JCore = interface;
  JGEDI = interface;
  JGEDI_AUTH_st_Data = interface;
  JGEDI_CLOCK_e_ProvidedTime = interface;
  JGEDI_CLOCK_st_RTC = interface;
  JGEDI_CL_e_ISO_Level = interface;
  JGEDI_CL_e_ISO_Type = interface;
  JGEDI_CL_e_MF_KeyType = interface;
  JGEDI_CL_e_MF_Type = interface;
  JGEDI_CL_st_ISO_PollingInfo = interface;
  JGEDI_CL_st_MF_ActivateInfo = interface;
  JGEDI_CL_st_MF_Key = interface;
  JGEDI_CL_st_ResetEMVInfo = interface;
  JGEDI_CRYPT_e_Op = interface;
  JGEDI_CRYPT_st_RSAKeyGen = interface;
  JGEDI_FS_e_Storage = interface;
  JGEDI_INFO_e_ControlNumber = interface;
  JGEDI_INFO_e_Module = interface;
  JGEDI_INFO_e_ModuleEng = interface;
  JGEDI_INFO_e_Test = interface;
  JGEDI_KBD_e_Key = interface;
  JGEDI_KBD_e_PowerKeyMode = interface;
  JGEDI_KBD_st_Info = interface;
  JGEDI_KMS_e_BLOCKTYPE = interface;
  JGEDI_KMS_e_EncMode = interface;
  JGEDI_KMS_e_KEYPURPOSE = interface;
  JGEDI_KMS_e_KEYTYPE = interface;
  JGEDI_KMS_e_KEYTYPE_LENGTH = interface;
  JGEDI_KMS_e_OP = interface;
  JGEDI_KMS_st_CapturePINBlockInfo = interface;
  JGEDI_KMS_st_Control = interface;
  JGEDI_KMS_st_Control_Callbacks = interface;
  JGEDI_KMS_st_Data = interface;
  JGEDI_KMS_st_KB = interface;
  JGEDI_KMS_st_PINBlock = interface;
  JGEDI_KMS_st_SaveKey = interface;
  JGEDI_LED_e_Id = interface;
  JGEDI_MSR_e_Status = interface;
  JGEDI_MSR_st_LastErrors = interface;
  JGEDI_MSR_st_Tracks = interface;
  JGEDI_PM_e_TypeCrt = interface;
  JGEDI_PRNTR_e_Alignment = interface;
  JGEDI_PRNTR_e_BarCodeType = interface;
  JGEDI_PRNTR_e_PrintDensity = interface;
  JGEDI_PRNTR_e_Status = interface;
  JGEDI_PRNTR_st_BarCodeConfig = interface;
  JGEDI_PRNTR_st_PictureConfig = interface;
  JGEDI_PRNTR_st_StringConfig = interface;
  JGEDI_SMART_e_MemoryCardType = interface;
  JGEDI_SMART_e_Slot = interface;
  JGEDI_SMART_e_Status = interface;
  JGEDI_SMART_e_Type = interface;
  JGEDI_SMART_e_Voltage = interface;
  JGEDI_SMART_st_ResetInfo = interface;
  JGEDI_SYS_e_SecuritySetup = interface;
  JGEDI_e_Ret = interface;
  JGediException = interface;
  JGediNative = interface;
  JIAUDIO = interface;
  JICL = interface;
  JICLOCK = interface;
  JICRYPT = interface;
  JIEnums = interface;
  JIGEDI = interface;
  JIINFO = interface;
  JIKBD = interface;
  JIKMS = interface;
  JILED = interface;
  JIMSR = interface;
  JIPM = interface;
  JIPRNTR = interface;
  JISMART = interface;
  JISYS = interface;
  JLogger = interface;
  JPrinter = interface;
  JPrinter_Align = interface;
  JPrinter_BarcodeType = interface;
  JPrinter_BarcodeWidth = interface;
  JPrinter_Font = interface;
  JRspCode = interface;
  Ja = interface;
  Ja0 = interface;
  Jb = interface;
  Jb0 = interface;
  Jc = interface;
  Jc0 = interface;
  Jd = interface;
  Jd0 = interface;
  Je = interface;
  Je0 = interface;
  Jf = interface;
  Jf0 = interface;
  Jg = interface;
  Jgedi_a = interface;
  Jgedi_b = interface;
  Jgedi_b1 = interface;
  Jgedi_c = interface;
  Jgedi_c1 = interface;
  Jgedi_d = interface;
  Jgedi_e = interface;
  Jgedi_f = interface;
  Jgedi_i = interface;
  Jgedi_k = interface;
  Jgedi_l = interface;
  Jgedi_m = interface;
  Jh = interface;
  Ji = interface;
  Jj = interface;
  Jk = interface;
  Jl = interface;
  Jlibbasebinder_a = interface;
  Jlibbasebinder_d = interface;
  Jm = interface;
  Jn = interface;
  Jo = interface;
  Jq = interface;
  Jr = interface;
  Js = interface;
  Jt = interface;
  Ju = interface;
  Jv = interface;
  Jw = interface;
  Jx = interface;
  Jy = interface;
  Jz = interface;

  JILEDClass = interface(IJavaClass)
    ['{777C56EE-5ECD-4846-9571-566A3FFB332B}']
  end;

  [JavaSignature('br/com/gertec/gedi/interfaces/ILED')]
  JILED = interface(IJavaInstance)
    ['{E17B2796-68EC-4B84-B622-C75DF51AFFD9}']
    procedure &Set(gEDI_LED_e_Id: JGEDI_LED_e_Id; boolean: Boolean); cdecl;
  end;
  TJILED = class(TJavaGenericImport<JILEDClass, JILED>) end;

  JGEDI_PM_e_TypeCrtClass = interface(JEnumClass)
    ['{32599BA8-BEB6-4EF6-B0A8-BCD3136B3F42}']
    {class} function _GetGEDI_CUSTOMER: JGEDI_PM_e_TypeCrt; cdecl;
    {class} function _GetGEDI_ENHANCED: JGEDI_PM_e_TypeCrt; cdecl;
    {class} function _GetGEDI_FACTORY: JGEDI_PM_e_TypeCrt; cdecl;
    {class} function valueOf(int: Integer): JGEDI_PM_e_TypeCrt; overload; cdecl;
    {class} function valueOf(string_1: JString): JGEDI_PM_e_TypeCrt; overload; cdecl;
    {class} function values: TJavaObjectArray<JGEDI_PM_e_TypeCrt>; cdecl;
    {class} property GEDI_CUSTOMER: JGEDI_PM_e_TypeCrt read _GetGEDI_CUSTOMER;
    {class} property GEDI_ENHANCED: JGEDI_PM_e_TypeCrt read _GetGEDI_ENHANCED;
    {class} property GEDI_FACTORY: JGEDI_PM_e_TypeCrt read _GetGEDI_FACTORY;
  end;

  [JavaSignature('br/com/gertec/gedi/enums/GEDI_PM_e_TypeCrt')]
  JGEDI_PM_e_TypeCrt = interface(JEnum)
    ['{407DD1ED-A3DB-4FBD-BADC-9AA739461CFC}']
    function getValue: Integer; cdecl;
  end;
  TJGEDI_PM_e_TypeCrt = class(TJavaGenericImport<JGEDI_PM_e_TypeCrtClass, JGEDI_PM_e_TypeCrt>) end;

  JGEDI_CRYPT_st_RSAKeyGenClass = interface(JObjectClass)
    ['{2200352D-8BD3-40A3-8AC0-0413803BCD23}']
    {class} function init: JGEDI_CRYPT_st_RSAKeyGen; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_CRYPT_st_RSAKeyGen')]
  JGEDI_CRYPT_st_RSAKeyGen = interface(JObject)
    ['{94C16B7C-C846-4F81-B333-F156F575F401}']
    function _GetabModulus: TJavaArray<Byte>; cdecl;
    function _GetabPrivateKey: TJavaArray<Byte>; cdecl;
    function _GetabPublicKey: TJavaArray<Byte>; cdecl;
    function _GetbVersion: Byte; cdecl;
    function _GetuiBits: Integer; cdecl;
    function _GetuiModulusLen: Integer; cdecl;
    function _GetuiPrivateKeyLen: Integer; cdecl;
    function _GetuiPublicKeyLen: Integer; cdecl;
    property abModulus: TJavaArray<Byte> read _GetabModulus;
    property abPrivateKey: TJavaArray<Byte> read _GetabPrivateKey;
    property abPublicKey: TJavaArray<Byte> read _GetabPublicKey;
    property bVersion: Byte read _GetbVersion;
    property uiBits: Integer read _GetuiBits;
    property uiModulusLen: Integer read _GetuiModulusLen;
    property uiPrivateKeyLen: Integer read _GetuiPrivateKeyLen;
    property uiPublicKeyLen: Integer read _GetuiPublicKeyLen;
  end;
  TJGEDI_CRYPT_st_RSAKeyGen = class(TJavaGenericImport<JGEDI_CRYPT_st_RSAKeyGenClass, JGEDI_CRYPT_st_RSAKeyGen>) end;

  Jgedi_bClass = interface(JObjectClass)
    ['{9228E50D-2158-4F52-9989-872D87DD8FBC}']
    {class} function init: Jgedi_b; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/b')]
  Jgedi_b = interface(JObject)
    ['{0F1ABCD7-9FEB-4791-845C-CB2D9B95996A}']
    function ISO_Polling(int: Integer): JGEDI_CL_st_ISO_PollingInfo; cdecl;
    procedure MF_Authentication(int: Integer; gEDI_CL_st_MF_Key: JGEDI_CL_st_MF_Key; bytes: TJavaArray<Byte>); cdecl;
    function MF_BlockRead(int: Integer): TJavaArray<Byte>; cdecl;
    procedure MF_BlockWrite(int: Integer; bytes: TJavaArray<Byte>); cdecl;
    procedure MF_Decrement(int: Integer; int_1: Integer); cdecl;
    procedure MF_Increment(int: Integer; int_1: Integer); cdecl;
    procedure MF_Restore(int: Integer; int_1: Integer); cdecl;
    function MF_SignatureGet(int: Integer): TJavaArray<Byte>; cdecl;
    procedure MF_Transfer(int: Integer); cdecl;
    procedure PowerOff; cdecl;
    procedure PowerOn; cdecl;
    function ResetEMV: JGEDI_CL_st_ResetEMVInfo; cdecl;
    function SendAPDU(bytes: TJavaArray<Byte>): TJavaArray<Byte>; cdecl;
    function a(bytes: TJavaArray<Byte>; int: Integer; bytes_1: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function a(bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
  end;
  TJgedi_b = class(TJavaGenericImport<Jgedi_bClass, Jgedi_b>) end;

  JwClass = interface(JIInterfaceClass)
    ['{29B3FE9D-E07C-4715-96BE-5C8F868785D2}']
  end;

  [JavaSignature('gedi/w')]
  Jw = interface(JIInterface)
    ['{73568DBB-06DF-4035-BBBB-3C67A63F03E6}']
  end;
  TJw = class(TJavaGenericImport<JwClass, Jw>) end;

  JGEDI_KMS_e_KEYTYPE_LENGTHClass = interface(JEnumClass)
    ['{852C4C1B-62ED-4C51-9298-BB08793425C8}']
    {class} function _GetAES16: JGEDI_KMS_e_KEYTYPE_LENGTH; cdecl;
    {class} function _GetAES32: JGEDI_KMS_e_KEYTYPE_LENGTH; cdecl;
    {class} function _GetDES: JGEDI_KMS_e_KEYTYPE_LENGTH; cdecl;
    {class} function _GetDUKPT3DES16: JGEDI_KMS_e_KEYTYPE_LENGTH; cdecl;
    {class} function _GetDUKPT3DES24: JGEDI_KMS_e_KEYTYPE_LENGTH; cdecl;
    {class} function _GetDUKPTDES8: JGEDI_KMS_e_KEYTYPE_LENGTH; cdecl;
    {class} function _GetTRIDES16: JGEDI_KMS_e_KEYTYPE_LENGTH; cdecl;
    {class} function _GetTRIDES24: JGEDI_KMS_e_KEYTYPE_LENGTH; cdecl;
    {class} function valueOf(string_1: JString): JGEDI_KMS_e_KEYTYPE_LENGTH; cdecl;
    {class} function values: TJavaObjectArray<JGEDI_KMS_e_KEYTYPE_LENGTH>; cdecl;
    {class} property AES16: JGEDI_KMS_e_KEYTYPE_LENGTH read _GetAES16;
    {class} property AES32: JGEDI_KMS_e_KEYTYPE_LENGTH read _GetAES32;
    {class} property DES: JGEDI_KMS_e_KEYTYPE_LENGTH read _GetDES;
    {class} property DUKPT3DES16: JGEDI_KMS_e_KEYTYPE_LENGTH read _GetDUKPT3DES16;
    {class} property DUKPT3DES24: JGEDI_KMS_e_KEYTYPE_LENGTH read _GetDUKPT3DES24;
    {class} property DUKPTDES8: JGEDI_KMS_e_KEYTYPE_LENGTH read _GetDUKPTDES8;
    {class} property TRIDES16: JGEDI_KMS_e_KEYTYPE_LENGTH read _GetTRIDES16;
    {class} property TRIDES24: JGEDI_KMS_e_KEYTYPE_LENGTH read _GetTRIDES24;
  end;

  [JavaSignature('br/com/gertec/gedi/enums/GEDI_KMS_e_KEYTYPE_LENGTH')]
  JGEDI_KMS_e_KEYTYPE_LENGTH = interface(JEnum)
    ['{D144321B-7C4F-4888-8266-C81B149E6422}']
    function getValue: Integer; cdecl;
  end;
  TJGEDI_KMS_e_KEYTYPE_LENGTH = class(TJavaGenericImport<JGEDI_KMS_e_KEYTYPE_LENGTHClass, JGEDI_KMS_e_KEYTYPE_LENGTH>) end;

  JRspCodeClass = interface(JObjectClass)
    ['{C210C827-F4DE-487A-BF69-D2253B0E730C}']
    {class} function _GetERROR: Integer; cdecl;
    {class} function _GetOK: Integer; cdecl;
    {class} function init: JRspCode; cdecl;
    {class} property ERROR: Integer read _GetERROR;
    {class} property OK: Integer read _GetOK;
  end;

  [JavaSignature('wangpos/sdk4/libbasebinder/RspCode')]
  JRspCode = interface(JObject)
    ['{2A240D8C-61F6-4EB3-BBBB-8E4665FC8BAC}']
  end;
  TJRspCode = class(TJavaGenericImport<JRspCodeClass, JRspCode>) end;

  JGEDI_SMART_e_StatusClass = interface(JEnumClass)
    ['{836C67E0-0044-4E42-9CCD-3A91B900C1DE}']
    {class} function _GetABSENT: JGEDI_SMART_e_Status; cdecl;
    {class} function _GetACTIVE: JGEDI_SMART_e_Status; cdecl;
    {class} function _GetPRESENT: JGEDI_SMART_e_Status; cdecl;
    {class} function valueOf(string_1: JString): JGEDI_SMART_e_Status; overload; cdecl;
    {class} function valueOf(int: Integer): JGEDI_SMART_e_Status; overload; cdecl;
    {class} function values: TJavaObjectArray<JGEDI_SMART_e_Status>; cdecl;
    {class} property ABSENT: JGEDI_SMART_e_Status read _GetABSENT;
    {class} property ACTIVE: JGEDI_SMART_e_Status read _GetACTIVE;
    {class} property PRESENT: JGEDI_SMART_e_Status read _GetPRESENT;
  end;

  [JavaSignature('br/com/gertec/gedi/enums/GEDI_SMART_e_Status')]
  JGEDI_SMART_e_Status = interface(JEnum)
    ['{04B6AD35-7D87-43DD-AA19-AE10BC5A8B45}']
    function getValue: Integer; cdecl;
  end;
  TJGEDI_SMART_e_Status = class(TJavaGenericImport<JGEDI_SMART_e_StatusClass, JGEDI_SMART_e_Status>) end;

  JGEDI_KMS_e_KEYPURPOSEClass = interface(JEnumClass)
    ['{8BF5AAC1-6C5C-43C8-B883-8DD8E6CD8874}']
    {class} function _GetAUTH: JGEDI_KMS_e_KEYPURPOSE; cdecl;
    {class} function _GetDATA: JGEDI_KMS_e_KEYPURPOSE; cdecl;
    {class} function _GetKEK: JGEDI_KMS_e_KEYPURPOSE; cdecl;
    {class} function _GetPIN: JGEDI_KMS_e_KEYPURPOSE; cdecl;
    {class} function _GetSRED: JGEDI_KMS_e_KEYPURPOSE; cdecl;
    {class} function valueOf(int: Integer): JGEDI_KMS_e_KEYPURPOSE; overload; cdecl;
    {class} function valueOf(string_1: JString): JGEDI_KMS_e_KEYPURPOSE; overload; cdecl;
    {class} function values: TJavaObjectArray<JGEDI_KMS_e_KEYPURPOSE>; cdecl;
    {class} property AUTH: JGEDI_KMS_e_KEYPURPOSE read _GetAUTH;
    {class} property DATA: JGEDI_KMS_e_KEYPURPOSE read _GetDATA;
    {class} property KEK: JGEDI_KMS_e_KEYPURPOSE read _GetKEK;
    {class} property PIN: JGEDI_KMS_e_KEYPURPOSE read _GetPIN;
    {class} property SRED: JGEDI_KMS_e_KEYPURPOSE read _GetSRED;
  end;

  [JavaSignature('br/com/gertec/gedi/enums/GEDI_KMS_e_KEYPURPOSE')]
  JGEDI_KMS_e_KEYPURPOSE = interface(JEnum)
    ['{8DE320A3-FDD6-4072-8233-E5220CBCCDC8}']
    function getValue: Integer; cdecl;
  end;
  TJGEDI_KMS_e_KEYPURPOSE = class(TJavaGenericImport<JGEDI_KMS_e_KEYPURPOSEClass, JGEDI_KMS_e_KEYPURPOSE>) end;

  Jgedi_cClass = interface(JObjectClass)
    ['{75DBB99A-9D1C-4490-9B72-B7702A3F3DAC}']
  end;

  [JavaSignature('br/com/gertec/gedi/c')]
  Jgedi_c = interface(JObject)
    ['{99078BA8-B845-4894-B3E2-8B544D827194}']
    function RTCFGet: JGEDI_CLOCK_st_RTC; cdecl;
    function RTCGet: JGEDI_CLOCK_st_RTC; cdecl;
    procedure RTCSet(gEDI_CLOCK_st_RTC: JGEDI_CLOCK_st_RTC); cdecl;
    function ScheduledRebootGet: JString; cdecl;
    procedure ScheduledRebootSet(string_1: JString); cdecl;
    function TimeZoneSet(timeZone: JTimeZone): Boolean; cdecl;
  end;
  TJgedi_c = class(TJavaGenericImport<Jgedi_cClass, Jgedi_c>) end;

  JIMSRClass = interface(IJavaClass)
    ['{D7D72976-518C-4428-A576-BDD7DC15778D}']
  end;

  [JavaSignature('br/com/gertec/gedi/interfaces/IMSR')]
  JIMSR = interface(IJavaInstance)
    ['{943E1C33-F49B-41EF-8247-BFD3E0C65D36}']
    function LastErrorGet: JGEDI_MSR_st_LastErrors; cdecl;
    function Read: JGEDI_MSR_st_Tracks; cdecl;
  end;
  TJIMSR = class(TJavaGenericImport<JIMSRClass, JIMSR>) end;

  JGEDI_CLOCK_st_RTCClass = interface(JObjectClass)
    ['{160FABF7-2FDF-4AEB-A639-507BC71203FA}']
    {class} function init: JGEDI_CLOCK_st_RTC; overload; cdecl;
    {class} function init(byte: Byte; byte_1: Byte; byte_2: Byte; byte_3: Byte; byte_4: Byte; byte_5: Byte; byte_6: Byte): JGEDI_CLOCK_st_RTC; overload; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_CLOCK_st_RTC')]
  JGEDI_CLOCK_st_RTC = interface(JObject)
    ['{035A06C3-5EA7-47EA-AB17-24A6E517BE47}']
    function _GetbDay: Byte; cdecl;
    function _GetbDoW: Byte; cdecl;
    function _GetbHour: Byte; cdecl;
    function _GetbMinute: Byte; cdecl;
    function _GetbMonth: Byte; cdecl;
    function _GetbSecond: Byte; cdecl;
    function _GetbYear: Byte; cdecl;
    property bDay: Byte read _GetbDay;
    property bDoW: Byte read _GetbDoW;
    property bHour: Byte read _GetbHour;
    property bMinute: Byte read _GetbMinute;
    property bMonth: Byte read _GetbMonth;
    property bSecond: Byte read _GetbSecond;
    property bYear: Byte read _GetbYear;
  end;
  TJGEDI_CLOCK_st_RTC = class(TJavaGenericImport<JGEDI_CLOCK_st_RTCClass, JGEDI_CLOCK_st_RTC>) end;

  JGEDI_KBD_st_InfoClass = interface(JObjectClass)
    ['{97ACC935-B8F0-4EFD-AFF4-5D12F9DF3DCA}']
    {class} function init(button: JButton; button_1: JButton; button_2: JButton; button_3: JButton; button_4: JButton; button_5: JButton; button_6: JButton; button_7: JButton; button_8: JButton; button_9: JButton; button_10: JButton; button_11: JButton; button_12: JButton; activity: JActivity): JGEDI_KBD_st_Info; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_KBD_st_Info')]
  JGEDI_KBD_st_Info = interface(JObject)
    ['{97F1EF87-EBDE-4AA0-8B47-CB879A9EF927}']
    function _Getactivity: JActivity; cdecl;
    function _Getbtn0: JButton; cdecl;
    function _Getbtn1: JButton; cdecl;
    function _Getbtn2: JButton; cdecl;
    function _Getbtn3: JButton; cdecl;
    function _Getbtn4: JButton; cdecl;
    function _Getbtn5: JButton; cdecl;
    function _Getbtn6: JButton; cdecl;
    function _Getbtn7: JButton; cdecl;
    function _Getbtn8: JButton; cdecl;
    function _Getbtn9: JButton; cdecl;
    function _GetbtnCancel: JButton; cdecl;
    function _GetbtnClear: JButton; cdecl;
    function _GetbtnConfirm: JButton; cdecl;
    function clone: JGEDI_KBD_st_Info; overload; cdecl;
//Mesmo    function clone: JObject; overload; cdecl;
    function getButtonByIndex(int: Integer): JButton; cdecl;
    property activity: JActivity read _Getactivity;
    property btn0: JButton read _Getbtn0;
    property btn1: JButton read _Getbtn1;
    property btn2: JButton read _Getbtn2;
    property btn3: JButton read _Getbtn3;
    property btn4: JButton read _Getbtn4;
    property btn5: JButton read _Getbtn5;
    property btn6: JButton read _Getbtn6;
    property btn7: JButton read _Getbtn7;
    property btn8: JButton read _Getbtn8;
    property btn9: JButton read _Getbtn9;
    property btnCancel: JButton read _GetbtnCancel;
    property btnClear: JButton read _GetbtnClear;
    property btnConfirm: JButton read _GetbtnConfirm;
  end;
  TJGEDI_KBD_st_Info = class(TJavaGenericImport<JGEDI_KBD_st_InfoClass, JGEDI_KBD_st_Info>) end;

  JvClass = interface(JIInterfaceClass)
    ['{2EDB4E1D-40F4-4439-88AB-15884B3FA06C}']
  end;

  [JavaSignature('gedi/v')]
  Jv = interface(JIInterface)
    ['{BF217B33-84E1-47C2-A3D4-1E4D21958F1A}']
  end;
  TJv = class(TJavaGenericImport<JvClass, Jv>) end;

  JGEDI_KMS_st_SaveKeyClass = interface(JObjectClass)
    ['{0A69AAA3-ABCB-4FCB-AB7F-AF899EFB2B52}']
    {class} function init: JGEDI_KMS_st_SaveKey; overload; cdecl;
    {class} function init(byte: Byte; gEDI_KMS_e_KEYTYPE_1: JGEDI_KMS_e_KEYTYPE; gEDI_KMS_e_KEYPURPOSE: JGEDI_KMS_e_KEYPURPOSE; int: Integer; bytes: TJavaArray<Byte>; bytes_1: TJavaArray<Byte>): JGEDI_KMS_st_SaveKey; overload; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_KMS_st_SaveKey')]
  JGEDI_KMS_st_SaveKey = interface(JObject)
    ['{FF3E57E6-0B4C-4ED1-9C00-944C6B2B507A}']
    function _GetabKSN: TJavaArray<Byte>; cdecl;
    function _GetabKey: TJavaArray<Byte>; cdecl;
    function _GetbVersion: Byte; cdecl;
    function _GeteKeyPurpose: JGEDI_KMS_e_KEYPURPOSE; cdecl;
    function _GeteKeyType: JGEDI_KMS_e_KEYTYPE; cdecl;
    function _GetuiKeyIndex: Integer; cdecl;
    property abKSN: TJavaArray<Byte> read _GetabKSN;
    property abKey: TJavaArray<Byte> read _GetabKey;
    property bVersion: Byte read _GetbVersion;
    property eKeyPurpose: JGEDI_KMS_e_KEYPURPOSE read _GeteKeyPurpose;
    property eKeyType: JGEDI_KMS_e_KEYTYPE read _GeteKeyType;
    property uiKeyIndex: Integer read _GetuiKeyIndex;
  end;
  TJGEDI_KMS_st_SaveKey = class(TJavaGenericImport<JGEDI_KMS_st_SaveKeyClass, JGEDI_KMS_st_SaveKey>) end;

  JuClass = interface(JIInterfaceClass)
    ['{F281DD10-ECEC-4A78-B5B5-E3EA019F123C}']
  end;

  [JavaSignature('gedi/u')]
  Ju = interface(JIInterface)
    ['{751912B9-329E-4371-A51E-977A549A6F01}']
    function a(int: Integer; bytes: TJavaArray<Byte>; bytes_1: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
  end;
  TJu = class(TJavaGenericImport<JuClass, Ju>) end;

  JnClass = interface(JObjectClass)
    ['{62ADF703-2722-4C41-8534-395A22481155}']
    {class} function init: Jn; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/n')]
  Jn = interface(JObject)
    ['{16B9D8DB-3070-4024-9E34-3698C3D77A71}']
  end;
  TJn = class(TJavaGenericImport<JnClass, Jn>) end;

  JGEDI_PRNTR_st_BarCodeConfigClass = interface(JObjectClass)
    ['{E2DB0EAD-F198-4D58-9378-F741278BB9C3}']
    {class} function init(gEDI_PRNTR_e_BarCodeType: JGEDI_PRNTR_e_BarCodeType; int: Integer; int_1: Integer; int_2: Integer): JGEDI_PRNTR_st_BarCodeConfig; overload; cdecl;
    {class} function init(gEDI_PRNTR_e_BarCodeType: JGEDI_PRNTR_e_BarCodeType; int: Integer; int_1: Integer): JGEDI_PRNTR_st_BarCodeConfig; overload; cdecl;
    {class} function init: JGEDI_PRNTR_st_BarCodeConfig; overload; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_PRNTR_st_BarCodeConfig')]
  JGEDI_PRNTR_st_BarCodeConfig = interface(JObject)
    ['{B2DC1E03-36B5-4372-8489-8CA03B9D5415}']
    function _GetbarCodeType: JGEDI_PRNTR_e_BarCodeType; cdecl;
    function _Getheight: Integer; cdecl;
    function _Getwhite_Space: Integer; cdecl;
    function _Getwidth: Integer; cdecl;
    function clone: JGEDI_PRNTR_st_BarCodeConfig; overload; cdecl;
//Mesmo    function clone: JObject; overload; cdecl;
    property barCodeType: JGEDI_PRNTR_e_BarCodeType read _GetbarCodeType;
    property height: Integer read _Getheight;
    property white_Space: Integer read _Getwhite_Space;
    property width: Integer read _Getwidth;
  end;
  TJGEDI_PRNTR_st_BarCodeConfig = class(TJavaGenericImport<JGEDI_PRNTR_st_BarCodeConfigClass, JGEDI_PRNTR_st_BarCodeConfig>) end;

  JGEDI_CL_e_MF_KeyTypeClass = interface(JEnumClass)
    ['{93A80F86-6B07-4DEF-9EF7-27A7AF7A8F06}']
    {class} function _GetKEY_A: JGEDI_CL_e_MF_KeyType; cdecl;
    {class} function _GetKEY_B: JGEDI_CL_e_MF_KeyType; cdecl;
    {class} function valueOf(string_1: JString): JGEDI_CL_e_MF_KeyType; overload; cdecl;
    {class} function valueOf(int: Integer): JGEDI_CL_e_MF_KeyType; overload; cdecl;
    {class} function values: TJavaObjectArray<JGEDI_CL_e_MF_KeyType>; cdecl;
    {class} property KEY_A: JGEDI_CL_e_MF_KeyType read _GetKEY_A;
    {class} property KEY_B: JGEDI_CL_e_MF_KeyType read _GetKEY_B;
  end;

  [JavaSignature('br/com/gertec/gedi/enums/GEDI_CL_e_MF_KeyType')]
  JGEDI_CL_e_MF_KeyType = interface(JEnum)
    ['{89D03FDC-CA9F-4C98-8BCB-A854CDB00FB9}']
    function getValue: Integer; cdecl;
  end;
  TJGEDI_CL_e_MF_KeyType = class(TJavaGenericImport<JGEDI_CL_e_MF_KeyTypeClass, JGEDI_CL_e_MF_KeyType>) end;

  JGEDI_MSR_st_TracksClass = interface(JObjectClass)
    ['{676576CB-4D1C-4120-A108-6E35435748C1}']
    {class} function init: JGEDI_MSR_st_Tracks; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_MSR_st_Tracks')]
  JGEDI_MSR_st_Tracks = interface(JObject)
    ['{0CABADC3-5A70-4CD9-A8D0-0C75BD93DCB4}']
    function _GetabTk1Buf: TJavaArray<Byte>; cdecl;
    function _GetabTk2Buf: TJavaArray<Byte>; cdecl;
    function _GetabTk3Buf: TJavaArray<Byte>; cdecl;
    property abTk1Buf: TJavaArray<Byte> read _GetabTk1Buf;
    property abTk2Buf: TJavaArray<Byte> read _GetabTk2Buf;
    property abTk3Buf: TJavaArray<Byte> read _GetabTk3Buf;
  end;
  TJGEDI_MSR_st_Tracks = class(TJavaGenericImport<JGEDI_MSR_st_TracksClass, JGEDI_MSR_st_Tracks>) end;

  JGEDI_CL_e_MF_TypeClass = interface(JEnumClass)
    ['{03E893FE-129F-4272-B12F-18BE20C51441}']
    {class} function _GetCLASSIC_1K: JGEDI_CL_e_MF_Type; cdecl;
    {class} function _GetCLASSIC_4K: JGEDI_CL_e_MF_Type; cdecl;
    {class} function _GetDESFIRE: JGEDI_CL_e_MF_Type; cdecl;
    {class} function _GetMINI: JGEDI_CL_e_MF_Type; cdecl;
    {class} function _GetPLUS: JGEDI_CL_e_MF_Type; cdecl;
    {class} function _GetULTRALIGHT: JGEDI_CL_e_MF_Type; cdecl;
    {class} function _GetULTRALIGHT_C: JGEDI_CL_e_MF_Type; cdecl;
    {class} function _GetULTRALIGHT_EV1: JGEDI_CL_e_MF_Type; cdecl;
    {class} function _GetUNKNOWN: JGEDI_CL_e_MF_Type; cdecl;
    {class} function valueOf(int: Integer): JGEDI_CL_e_MF_Type; overload; cdecl;
    {class} function valueOf(string_1: JString): JGEDI_CL_e_MF_Type; overload; cdecl;
    {class} function values: TJavaObjectArray<JGEDI_CL_e_MF_Type>; cdecl;
    {class} property CLASSIC_1K: JGEDI_CL_e_MF_Type read _GetCLASSIC_1K;
    {class} property CLASSIC_4K: JGEDI_CL_e_MF_Type read _GetCLASSIC_4K;
    {class} property DESFIRE: JGEDI_CL_e_MF_Type read _GetDESFIRE;
    {class} property MINI: JGEDI_CL_e_MF_Type read _GetMINI;
    {class} property PLUS: JGEDI_CL_e_MF_Type read _GetPLUS;
    {class} property ULTRALIGHT: JGEDI_CL_e_MF_Type read _GetULTRALIGHT;
    {class} property ULTRALIGHT_C: JGEDI_CL_e_MF_Type read _GetULTRALIGHT_C;
    {class} property ULTRALIGHT_EV1: JGEDI_CL_e_MF_Type read _GetULTRALIGHT_EV1;
    {class} property UNKNOWN: JGEDI_CL_e_MF_Type read _GetUNKNOWN;
  end;

  [JavaSignature('br/com/gertec/gedi/enums/GEDI_CL_e_MF_Type')]
  JGEDI_CL_e_MF_Type = interface(JEnum)
    ['{C4D6A3AC-DE3F-4FDC-B0B7-E02EE6E44DFB}']
    function getValue: Integer; cdecl;
  end;
  TJGEDI_CL_e_MF_Type = class(TJavaGenericImport<JGEDI_CL_e_MF_TypeClass, JGEDI_CL_e_MF_Type>) end;

  JxClass = interface(JIInterfaceClass)
    ['{0441457B-0812-49AE-AA8F-B41F30BECB37}']
  end;

  [JavaSignature('gedi/x')]
  Jx = interface(JIInterface)
    ['{CEA50DDC-AE94-4A7F-B1A3-2B088D816B66}']
  end;
  TJx = class(TJavaGenericImport<JxClass, Jx>) end;

  JGEDI_e_RetClass = interface(JEnumClass)
    ['{DEF544CA-E3AA-4348-9E63-52DA10EB5EF7}']
    {class} function _GetAUDIO_BUZZER_NOT_EXIST: JGEDI_e_Ret; cdecl;
    {class} function _GetAUDIO_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetAUTH_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetAUTH_HASH_MISMATCH: JGEDI_e_Ret; cdecl;
    {class} function _GetAUTH_KEY_EXPIRED: JGEDI_e_Ret; cdecl;
    {class} function _GetAUTH_SRED_DISABLED: JGEDI_e_Ret; cdecl;
    {class} function _GetBT_ALREADY_OPEN: JGEDI_e_Ret; cdecl;
    {class} function _GetBT_CMD_FAIL: JGEDI_e_Ret; cdecl;
    {class} function _GetBT_CONNECT_FAILED: JGEDI_e_Ret; cdecl;
    {class} function _GetBT_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetBT_INVALID_CONFIG: JGEDI_e_Ret; cdecl;
    {class} function _GetBT_NOT_CONNECTED: JGEDI_e_Ret; cdecl;
    {class} function _GetBT_NOT_INITIALIZED: JGEDI_e_Ret; cdecl;
    {class} function _GetBT_NOT_PARIED: JGEDI_e_Ret; cdecl;
    {class} function _GetBT_PROCESSING: JGEDI_e_Ret; cdecl;
    {class} function _GetBT_SCAN_FAILED: JGEDI_e_Ret; cdecl;
    {class} function _GetBUFFER_NOT_ENOUGH: JGEDI_e_Ret; cdecl;
    {class} function _GetCANCELLED: JGEDI_e_Ret; cdecl;
    {class} function _GetCLOCK_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetCL_BYTE_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetCL_COLLISION: JGEDI_e_Ret; cdecl;
    {class} function _GetCL_CRC_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetCL_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetCL_MF_AUTHENTICATE: JGEDI_e_Ret; cdecl;
    {class} function _GetCL_MF_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetCL_MF_KEY_INVALID_TYPE: JGEDI_e_Ret; cdecl;
    {class} function _GetCL_MF_TYPE_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetCL_NO_RESPONSE: JGEDI_e_Ret; cdecl;
    {class} function _GetCL_PROTOCOL_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetCL_SN: JGEDI_e_Ret; cdecl;
    {class} function _GetCOULD_NOT_ALLOCATE_MEMORY: JGEDI_e_Ret; cdecl;
    {class} function _GetCRYPT_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetEND_OF_LIST: JGEDI_e_Ret; cdecl;
    {class} function _GetENG_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetETH_ALREADY_OPEN: JGEDI_e_Ret; cdecl;
    {class} function _GetETH_CMD_FAIL: JGEDI_e_Ret; cdecl;
    {class} function _GetETH_DNS_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetETH_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetETH_FTP_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetETH_NOT_ONLINE: JGEDI_e_Ret; cdecl;
    {class} function _GetETH_NOT_OPEN: JGEDI_e_Ret; cdecl;
    {class} function _GetETH_TX_BUSY: JGEDI_e_Ret; cdecl;
    {class} function _GetFS_ACCESS_DENIED: JGEDI_e_Ret; cdecl;
    {class} function _GetFS_ALREADY_EXIST: JGEDI_e_Ret; cdecl;
    {class} function _GetFS_EOF: JGEDI_e_Ret; cdecl;
    {class} function _GetFS_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetFS_INVALID_HANDLE: JGEDI_e_Ret; cdecl;
    {class} function _GetFS_INVALID_NAME: JGEDI_e_Ret; cdecl;
    {class} function _GetFS_NOT_EXIST: JGEDI_e_Ret; cdecl;
    {class} function _GetFUNCTION_NOT_FOUND: JGEDI_e_Ret; cdecl;
    {class} function _GetGSM_ALREADY_INITIALIZED: JGEDI_e_Ret; cdecl;
    {class} function _GetGSM_AUTH_FAIL: JGEDI_e_Ret; cdecl;
    {class} function _GetGSM_CMD_FAIL: JGEDI_e_Ret; cdecl;
    {class} function _GetGSM_CONNECT_FAILED: JGEDI_e_Ret; cdecl;
    {class} function _GetGSM_CSD_CONNECTED: JGEDI_e_Ret; cdecl;
    {class} function _GetGSM_CSD_NOT_CONNECTED: JGEDI_e_Ret; cdecl;
    {class} function _GetGSM_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetGSM_GPRS_ATTACHED: JGEDI_e_Ret; cdecl;
    {class} function _GetGSM_GPRS_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetGSM_GPRS_NOT_ALLOWED: JGEDI_e_Ret; cdecl;
    {class} function _GetGSM_GPRS_NOT_ATTACHED: JGEDI_e_Ret; cdecl;
    {class} function _GetGSM_GPRS_PDP_ACTIVATED: JGEDI_e_Ret; cdecl;
    {class} function _GetGSM_GPRS_PDP_NOT_ACTIVATED: JGEDI_e_Ret; cdecl;
    {class} function _GetGSM_GPRS_PPP_FAILED: JGEDI_e_Ret; cdecl;
    {class} function _GetGSM_GPRS_PPP_STARTED: JGEDI_e_Ret; cdecl;
    {class} function _GetGSM_GPRS_PPP_STOPPED: JGEDI_e_Ret; cdecl;
    {class} function _GetGSM_GPRS_UNKNOWN_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetGSM_INFO_NOT_FOUND: JGEDI_e_Ret; cdecl;
    {class} function _GetGSM_NETWORK_NOT_ALLOWED: JGEDI_e_Ret; cdecl;
    {class} function _GetGSM_NETWORK_TIMEOUT: JGEDI_e_Ret; cdecl;
    {class} function _GetGSM_NOT_INITIALIZED: JGEDI_e_Ret; cdecl;
    {class} function _GetGSM_NOT_OPEN: JGEDI_e_Ret; cdecl;
    {class} function _GetGSM_NOT_REGISTERED: JGEDI_e_Ret; cdecl;
    {class} function _GetGSM_NO_NETWORK: JGEDI_e_Ret; cdecl;
    {class} function _GetGSM_OPERATION_NOT_ALLOWED: JGEDI_e_Ret; cdecl;
    {class} function _GetGSM_PLMN_NOT_ALLOWED: JGEDI_e_Ret; cdecl;
    {class} function _GetGSM_PROCESSING: JGEDI_e_Ret; cdecl;
    {class} function _GetGSM_PROTOCOL_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetGSM_REGISTERED: JGEDI_e_Ret; cdecl;
    {class} function _GetGSM_SERIAL_OPEN_FAILED: JGEDI_e_Ret; cdecl;
    {class} function _GetGSM_SIGNAL_FAIL: JGEDI_e_Ret; cdecl;
    {class} function _GetGSM_SIM_ALREADY_INITIALIZED: JGEDI_e_Ret; cdecl;
    {class} function _GetGSM_SIM_BLOCKED: JGEDI_e_Ret; cdecl;
    {class} function _GetGSM_SIM_BUSY: JGEDI_e_Ret; cdecl;
    {class} function _GetGSM_SIM_FAIL: JGEDI_e_Ret; cdecl;
    {class} function _GetGSM_SIM_NOT_INITIALIZED: JGEDI_e_Ret; cdecl;
    {class} function _GetGSM_SIM_NOT_INSERTED: JGEDI_e_Ret; cdecl;
    {class} function _GetGSM_SIM_PIN_REQUIRED: JGEDI_e_Ret; cdecl;
    {class} function _GetGSM_SMS_CONNECTED: JGEDI_e_Ret; cdecl;
    {class} function _GetGSM_SMS_NOT_CONNECTED: JGEDI_e_Ret; cdecl;
    {class} function _GetGSM_SMS_RECEIVED_FAILED: JGEDI_e_Ret; cdecl;
    {class} function _GetGSM_SMS_SEND_FAILED: JGEDI_e_Ret; cdecl;
    {class} function _GetGSM_STANDBY: JGEDI_e_Ret; cdecl;
    {class} function _GetGSM_USSD_CONNECTED: JGEDI_e_Ret; cdecl;
    {class} function _GetGSM_USSD_FAILED: JGEDI_e_Ret; cdecl;
    {class} function _GetGSM_USSD_NOT_CONNECTED: JGEDI_e_Ret; cdecl;
    {class} function _GetGSM_USSD_NOT_OPEN: JGEDI_e_Ret; cdecl;
    {class} function _GetGSM_USSD_OPEN: JGEDI_e_Ret; cdecl;
    {class} function _GetI2C_ADDRESS_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetI2C_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetI2C_OPEN_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetI2C_REGISTER_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetIMPLEMENTATION_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetINFO_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetINVALID_PARAMETER: JGEDI_e_Ret; cdecl;
    {class} function _GetKBD_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetKBD_IN_USE: JGEDI_e_Ret; cdecl;
    {class} function _GetKMS_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetKMS_FULL: JGEDI_e_Ret; cdecl;
    {class} function _GetKMS_INVALID_CERTIFICATE: JGEDI_e_Ret; cdecl;
    {class} function _GetKMS_INVALID_HASH: JGEDI_e_Ret; cdecl;
    {class} function _GetKMS_INVALID_INDEX: JGEDI_e_Ret; cdecl;
    {class} function _GetKMS_INVALID_KCV: JGEDI_e_Ret; cdecl;
    {class} function _GetKMS_INVALID_KEK: JGEDI_e_Ret; cdecl;
    {class} function _GetKMS_INVALID_KEY: JGEDI_e_Ret; cdecl;
    {class} function _GetKMS_INVALID_LENGTH: JGEDI_e_Ret; cdecl;
    {class} function _GetKMS_INVALID_MODE: JGEDI_e_Ret; cdecl;
    {class} function _GetKMS_INVALID_OPERATION: JGEDI_e_Ret; cdecl;
    {class} function _GetKMS_INVALID_PIN_LEN: JGEDI_e_Ret; cdecl;
    {class} function _GetKMS_INVALID_PURPOSE: JGEDI_e_Ret; cdecl;
    {class} function _GetKMS_INVALID_TYPE: JGEDI_e_Ret; cdecl;
    {class} function _GetKMS_KEY_EXPIRED: JGEDI_e_Ret; cdecl;
    {class} function _GetKMS_KEY_NOT_UNIQUE: JGEDI_e_Ret; cdecl;
    {class} function _GetKMS_KEY_ON_COOLDOWN: JGEDI_e_Ret; cdecl;
    {class} function _GetKMS_NULL_PIN: JGEDI_e_Ret; cdecl;
    {class} function _GetKMS_PINMODE_ACTIVE: JGEDI_e_Ret; cdecl;
    {class} function _GetKMS_PINMODE_INACTIVE: JGEDI_e_Ret; cdecl;
    {class} function _GetKMS_PINMODE_NO_KEY: JGEDI_e_Ret; cdecl;
    {class} function _GetKMS_USER_CANCELLED: JGEDI_e_Ret; cdecl;
    {class} function _GetLCD_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetLCD_FONT_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetLED_BACKLIGHT_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetLED_BACKLIGHT_NOT_EXIST: JGEDI_e_Ret; cdecl;
    {class} function _GetLED_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetLED_NOT_EXIST: JGEDI_e_Ret; cdecl;
    {class} function _GetLINUX_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetMENU_NOT_INITIALIZED: JGEDI_e_Ret; cdecl;
    {class} function _GetMIFARE_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetMIFARE_KEYA_REQUIRED: JGEDI_e_Ret; cdecl;
    {class} function _GetMIFARE_KEYB_REQUIRED: JGEDI_e_Ret; cdecl;
    {class} function _GetMIFARE_NOT_FORMATTED: JGEDI_e_Ret; cdecl;
    {class} function _GetMIFARE_PROTECTED: JGEDI_e_Ret; cdecl;
    {class} function _GetMODEM_ALREADY_OPEN: JGEDI_e_Ret; cdecl;
    {class} function _GetMODEM_CMD_FAIL: JGEDI_e_Ret; cdecl;
    {class} function _GetMODEM_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetMODEM_NOT_ONLINE: JGEDI_e_Ret; cdecl;
    {class} function _GetMODEM_NOT_OPEN: JGEDI_e_Ret; cdecl;
    {class} function _GetMODEM_TX_BUSY: JGEDI_e_Ret; cdecl;
    {class} function _GetMODULE_NOT_FOUND: JGEDI_e_Ret; cdecl;
    {class} function _GetMSR_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetMSR_NO_SWIPE: JGEDI_e_Ret; cdecl;
    {class} function _GetMSR_TRACK_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetNET_BINDTODEVICE_FAILED: JGEDI_e_Ret; cdecl;
    {class} function _GetNET_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetNET_PING_FAILED: JGEDI_e_Ret; cdecl;
    {class} function _GetNET_SELECTDEFAULTDEVICE_FAILED: JGEDI_e_Ret; cdecl;
    {class} function _GetNET_URL_NOT_FOUND: JGEDI_e_Ret; cdecl;
    {class} function _GetNOT_AVAILABLE: JGEDI_e_Ret; cdecl;
    {class} function _GetNOT_SUPPORTED: JGEDI_e_Ret; cdecl;
    {class} function _GetNULL_PARAMETER: JGEDI_e_Ret; cdecl;
    {class} function _GetOK: JGEDI_e_Ret; cdecl;
    {class} function _GetOUT_OF_BOUNDS: JGEDI_e_Ret; cdecl;
    {class} function _GetPENALTY_ACTIVE: JGEDI_e_Ret; cdecl;
    {class} function _GetPENALTY_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetPKI_CMAC_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetPKI_CSR_NOT_SIGNED: JGEDI_e_Ret; cdecl;
    {class} function _GetPKI_DATE_NOT_FOUND: JGEDI_e_Ret; cdecl;
    {class} function _GetPKI_DERIVATION_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetPKI_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetPKI_INVALID_CERTIFICATE: JGEDI_e_Ret; cdecl;
    {class} function _GetPKI_INVALID_CHALLENGE: JGEDI_e_Ret; cdecl;
    {class} function _GetPKI_INVALID_CMAC: JGEDI_e_Ret; cdecl;
    {class} function _GetPKI_INVALID_DATE: JGEDI_e_Ret; cdecl;
    {class} function _GetPKI_INVALID_KEY: JGEDI_e_Ret; cdecl;
    {class} function _GetPKI_KBPK_MISSING: JGEDI_e_Ret; cdecl;
    {class} function _GetPKI_NO_CA_FOUND: JGEDI_e_Ret; cdecl;
    {class} function _GetPKI_NO_CERTIFICATE_FOUND: JGEDI_e_Ret; cdecl;
    {class} function _GetPKI_NO_CUST_KEY_FOUND: JGEDI_e_Ret; cdecl;
    {class} function _GetPKI_NO_DEV_KEY_FOUND: JGEDI_e_Ret; cdecl;
    {class} function _GetPKI_TR31_BLOCK_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetPLATFORM_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_ALREADY_SEALED: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_AP_NAME_REPEATED: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_DELETE_FAILED_ABORTED: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_DELETE_FAILED_DEVICE_POLICY_MANAGER: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_DELETE_FAILED_INTERNAL_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_DELETE_FAILED_OWNER_BLOCKED: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_DELETE_FAILED_USED_SHARED_LIBRARY: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_DELETE_FAILED_USER_RESTRICTED: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_FULL: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_INSTALL_FAILED_CONFLICTING_PROVIDER: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_INSTALL_FAILED_CONTAINER_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_INSTALL_FAILED_CPU_ABI_INCOMPATIBLE: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_INSTALL_FAILED_DEXOPT: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_INSTALL_FAILED_DUPLICATE_PACKAGE: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_INSTALL_FAILED_DUPLICATE_PERMISSION: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_INSTALL_FAILED_INSUFFICIENT_STORAGE: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_INSTALL_FAILED_INTERNAL_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_INSTALL_FAILED_INVALID_APK: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_INSTALL_FAILED_INVALID_INSTALL_LOCATION: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_INSTALL_FAILED_INVALID_URI: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_INSTALL_FAILED_MEDIA_UNAVAILABLE: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_INSTALL_FAILED_MISSING_FEATURE: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_INSTALL_FAILED_MISSING_SHARED_LIBRARY: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_INSTALL_FAILED_NEWER_SDK: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_INSTALL_FAILED_NO_MATCHING_ABIS: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_INSTALL_FAILED_NO_SHARED_USER: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_INSTALL_FAILED_OLDER_SDK: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_INSTALL_FAILED_PACKAGE_CHANGED: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_INSTALL_FAILED_REPLACE_COULDNT_DELETE: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_INSTALL_FAILED_SHARED_USER_INCOMPATIBLE: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_INSTALL_FAILED_TEST_ONLY: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_INSTALL_FAILED_UID_CHANGED: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_INSTALL_FAILED_UNKNOWN_SOURCES: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_INSTALL_FAILED_UPDATE_INCOMPATIBLE: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_INSTALL_FAILED_USER_RESTRICTED: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_INSTALL_FAILED_VERIFICATION_FAILURE: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_INSTALL_FAILED_VERIFICATION_TIMEOUT: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_INSTALL_FAILED_VERSION_DOWNGRADE: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_INSTALL_PARSE_FAILED_BAD_MANIFEST: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_INSTALL_PARSE_FAILED_BAD_PACKAGE_NAME: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_INSTALL_PARSE_FAILED_BAD_SHARED_USER_ID: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_INSTALL_PARSE_FAILED_CERTIFICATE_ENCODING: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_INSTALL_PARSE_FAILED_INCONSISTENT_CERTIFICATES: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_INSTALL_PARSE_FAILED_MANIFEST_EMPTY: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_INSTALL_PARSE_FAILED_MANIFEST_MALFORMED: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_INSTALL_PARSE_FAILED_NOT_APK: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_INSTALL_PARSE_FAILED_NO_CERTIFICATES: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_INSTALL_PARSE_FAILED_UNEXPECTED_EXCEPTION: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_INVALID_AP: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_INVALID_FILE: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_NOT_SEALED: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_SEAL_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_SIGN_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_ULD_FAIL: JGEDI_e_Ret; cdecl;
    {class} function _GetPOWER_BATTERY_BUSY: JGEDI_e_Ret; cdecl;
    {class} function _GetPOWER_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetPOWER_NO_BATTERY: JGEDI_e_Ret; cdecl;
    {class} function _GetPOWER_NO_EXT_POWER: JGEDI_e_Ret; cdecl;
    {class} function _GetPRNTR_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetPRNTR_FONT_PATH_LEN: JGEDI_e_Ret; cdecl;
    {class} function _GetPRNTR_NOT_READY: JGEDI_e_Ret; cdecl;
    {class} function _GetPRNTR_OUT_OF_PAPER: JGEDI_e_Ret; cdecl;
    {class} function _GetPRNTR_OVERHEAT: JGEDI_e_Ret; cdecl;
    {class} function _GetRS232_BUSY: JGEDI_e_Ret; cdecl;
    {class} function _GetRS232_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetRS232_NOT_OPEN: JGEDI_e_Ret; cdecl;
    {class} function _GetRSA_GEN_FAIL: JGEDI_e_Ret; cdecl;
    {class} function _GetSMART_ABSENT: JGEDI_e_Ret; cdecl;
    {class} function _GetSMART_ATR_TOO_LONG: JGEDI_e_Ret; cdecl;
    {class} function _GetSMART_BAD_TS: JGEDI_e_Ret; cdecl;
    {class} function _GetSMART_COMM_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetSMART_DEACTIVATED_PROTOCOL: JGEDI_e_Ret; cdecl;
    {class} function _GetSMART_EDC_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetSMART_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetSMART_INVALID_ATR: JGEDI_e_Ret; cdecl;
    {class} function _GetSMART_MUTE: JGEDI_e_Ret; cdecl;
    {class} function _GetSMART_NOT_ACTIVATED: JGEDI_e_Ret; cdecl;
    {class} function _GetSMART_PARITY_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetSMART_POWER_FAILED: JGEDI_e_Ret; cdecl;
    {class} function _GetSMART_PROCEDURE_BYTE_CONFLICT: JGEDI_e_Ret; cdecl;
    {class} function _GetSMART_PTS_RESPONSE_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetSMART_TA1_NOT_SUPPORTED: JGEDI_e_Ret; cdecl;
    {class} function _GetSYS_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetTEST_FAIL: JGEDI_e_Ret; cdecl;
    {class} function _GetTIMEOUT: JGEDI_e_Ret; cdecl;
    {class} function _GetUSB_ALREADY_OPEN: JGEDI_e_Ret; cdecl;
    {class} function _GetUSB_BUSY: JGEDI_e_Ret; cdecl;
    {class} function _GetUSB_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetUSB_NOT_OPEN: JGEDI_e_Ret; cdecl;
    {class} function _GetUTIL_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetWIFI_CMD_FAIL: JGEDI_e_Ret; cdecl;
    {class} function _GetWIFI_CONNECT_FAILED: JGEDI_e_Ret; cdecl;
    {class} function _GetWIFI_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetWIFI_INVALID_CONFIG: JGEDI_e_Ret; cdecl;
    {class} function _GetWIFI_NOT_OPEN: JGEDI_e_Ret; cdecl;
    {class} function _GetWIFI_PROCESSING: JGEDI_e_Ret; cdecl;
    {class} function _GetWIFI_SCAN_FAILED: JGEDI_e_Ret; cdecl;
    {class} function valueOf(string_1: JString): JGEDI_e_Ret; overload; cdecl;
    {class} function valueOf(int: Integer): JGEDI_e_Ret; overload; cdecl;
    {class} function values: TJavaObjectArray<JGEDI_e_Ret>; cdecl;
    {class} property AUDIO_BUZZER_NOT_EXIST: JGEDI_e_Ret read _GetAUDIO_BUZZER_NOT_EXIST;
    {class} property AUDIO_ERROR: JGEDI_e_Ret read _GetAUDIO_ERROR;
    {class} property AUTH_ERROR: JGEDI_e_Ret read _GetAUTH_ERROR;
    {class} property AUTH_HASH_MISMATCH: JGEDI_e_Ret read _GetAUTH_HASH_MISMATCH;
    {class} property AUTH_KEY_EXPIRED: JGEDI_e_Ret read _GetAUTH_KEY_EXPIRED;
    {class} property AUTH_SRED_DISABLED: JGEDI_e_Ret read _GetAUTH_SRED_DISABLED;
    {class} property BT_ALREADY_OPEN: JGEDI_e_Ret read _GetBT_ALREADY_OPEN;
    {class} property BT_CMD_FAIL: JGEDI_e_Ret read _GetBT_CMD_FAIL;
    {class} property BT_CONNECT_FAILED: JGEDI_e_Ret read _GetBT_CONNECT_FAILED;
    {class} property BT_ERROR: JGEDI_e_Ret read _GetBT_ERROR;
    {class} property BT_INVALID_CONFIG: JGEDI_e_Ret read _GetBT_INVALID_CONFIG;
    {class} property BT_NOT_CONNECTED: JGEDI_e_Ret read _GetBT_NOT_CONNECTED;
    {class} property BT_NOT_INITIALIZED: JGEDI_e_Ret read _GetBT_NOT_INITIALIZED;
    {class} property BT_NOT_PARIED: JGEDI_e_Ret read _GetBT_NOT_PARIED;
    {class} property BT_PROCESSING: JGEDI_e_Ret read _GetBT_PROCESSING;
    {class} property BT_SCAN_FAILED: JGEDI_e_Ret read _GetBT_SCAN_FAILED;
    {class} property BUFFER_NOT_ENOUGH: JGEDI_e_Ret read _GetBUFFER_NOT_ENOUGH;
    {class} property CANCELLED: JGEDI_e_Ret read _GetCANCELLED;
    {class} property CLOCK_ERROR: JGEDI_e_Ret read _GetCLOCK_ERROR;
    {class} property CL_BYTE_ERROR: JGEDI_e_Ret read _GetCL_BYTE_ERROR;
    {class} property CL_COLLISION: JGEDI_e_Ret read _GetCL_COLLISION;
    {class} property CL_CRC_ERROR: JGEDI_e_Ret read _GetCL_CRC_ERROR;
    {class} property CL_ERROR: JGEDI_e_Ret read _GetCL_ERROR;
    {class} property CL_MF_AUTHENTICATE: JGEDI_e_Ret read _GetCL_MF_AUTHENTICATE;
    {class} property CL_MF_ERROR: JGEDI_e_Ret read _GetCL_MF_ERROR;
    {class} property CL_MF_KEY_INVALID_TYPE: JGEDI_e_Ret read _GetCL_MF_KEY_INVALID_TYPE;
    {class} property CL_MF_TYPE_ERROR: JGEDI_e_Ret read _GetCL_MF_TYPE_ERROR;
    {class} property CL_NO_RESPONSE: JGEDI_e_Ret read _GetCL_NO_RESPONSE;
    {class} property CL_PROTOCOL_ERROR: JGEDI_e_Ret read _GetCL_PROTOCOL_ERROR;
    {class} property CL_SN: JGEDI_e_Ret read _GetCL_SN;
    {class} property COULD_NOT_ALLOCATE_MEMORY: JGEDI_e_Ret read _GetCOULD_NOT_ALLOCATE_MEMORY;
    {class} property CRYPT_ERROR: JGEDI_e_Ret read _GetCRYPT_ERROR;
    {class} property END_OF_LIST: JGEDI_e_Ret read _GetEND_OF_LIST;
    {class} property ENG_ERROR: JGEDI_e_Ret read _GetENG_ERROR;
    {class} property ETH_ALREADY_OPEN: JGEDI_e_Ret read _GetETH_ALREADY_OPEN;
    {class} property ETH_CMD_FAIL: JGEDI_e_Ret read _GetETH_CMD_FAIL;
    {class} property ETH_DNS_ERROR: JGEDI_e_Ret read _GetETH_DNS_ERROR;
    {class} property ETH_ERROR: JGEDI_e_Ret read _GetETH_ERROR;
    {class} property ETH_FTP_ERROR: JGEDI_e_Ret read _GetETH_FTP_ERROR;
    {class} property ETH_NOT_ONLINE: JGEDI_e_Ret read _GetETH_NOT_ONLINE;
    {class} property ETH_NOT_OPEN: JGEDI_e_Ret read _GetETH_NOT_OPEN;
    {class} property ETH_TX_BUSY: JGEDI_e_Ret read _GetETH_TX_BUSY;
    {class} property FS_ACCESS_DENIED: JGEDI_e_Ret read _GetFS_ACCESS_DENIED;
    {class} property FS_ALREADY_EXIST: JGEDI_e_Ret read _GetFS_ALREADY_EXIST;
    {class} property FS_EOF: JGEDI_e_Ret read _GetFS_EOF;
    {class} property FS_ERROR: JGEDI_e_Ret read _GetFS_ERROR;
    {class} property FS_INVALID_HANDLE: JGEDI_e_Ret read _GetFS_INVALID_HANDLE;
    {class} property FS_INVALID_NAME: JGEDI_e_Ret read _GetFS_INVALID_NAME;
    {class} property FS_NOT_EXIST: JGEDI_e_Ret read _GetFS_NOT_EXIST;
    {class} property FUNCTION_NOT_FOUND: JGEDI_e_Ret read _GetFUNCTION_NOT_FOUND;
    {class} property GSM_ALREADY_INITIALIZED: JGEDI_e_Ret read _GetGSM_ALREADY_INITIALIZED;
    {class} property GSM_AUTH_FAIL: JGEDI_e_Ret read _GetGSM_AUTH_FAIL;
    {class} property GSM_CMD_FAIL: JGEDI_e_Ret read _GetGSM_CMD_FAIL;
    {class} property GSM_CONNECT_FAILED: JGEDI_e_Ret read _GetGSM_CONNECT_FAILED;
    {class} property GSM_CSD_CONNECTED: JGEDI_e_Ret read _GetGSM_CSD_CONNECTED;
    {class} property GSM_CSD_NOT_CONNECTED: JGEDI_e_Ret read _GetGSM_CSD_NOT_CONNECTED;
    {class} property GSM_ERROR: JGEDI_e_Ret read _GetGSM_ERROR;
    {class} property GSM_GPRS_ATTACHED: JGEDI_e_Ret read _GetGSM_GPRS_ATTACHED;
    {class} property GSM_GPRS_ERROR: JGEDI_e_Ret read _GetGSM_GPRS_ERROR;
    {class} property GSM_GPRS_NOT_ALLOWED: JGEDI_e_Ret read _GetGSM_GPRS_NOT_ALLOWED;
    {class} property GSM_GPRS_NOT_ATTACHED: JGEDI_e_Ret read _GetGSM_GPRS_NOT_ATTACHED;
    {class} property GSM_GPRS_PDP_ACTIVATED: JGEDI_e_Ret read _GetGSM_GPRS_PDP_ACTIVATED;
    {class} property GSM_GPRS_PDP_NOT_ACTIVATED: JGEDI_e_Ret read _GetGSM_GPRS_PDP_NOT_ACTIVATED;
    {class} property GSM_GPRS_PPP_FAILED: JGEDI_e_Ret read _GetGSM_GPRS_PPP_FAILED;
    {class} property GSM_GPRS_PPP_STARTED: JGEDI_e_Ret read _GetGSM_GPRS_PPP_STARTED;
    {class} property GSM_GPRS_PPP_STOPPED: JGEDI_e_Ret read _GetGSM_GPRS_PPP_STOPPED;
    {class} property GSM_GPRS_UNKNOWN_ERROR: JGEDI_e_Ret read _GetGSM_GPRS_UNKNOWN_ERROR;
    {class} property GSM_INFO_NOT_FOUND: JGEDI_e_Ret read _GetGSM_INFO_NOT_FOUND;
    {class} property GSM_NETWORK_NOT_ALLOWED: JGEDI_e_Ret read _GetGSM_NETWORK_NOT_ALLOWED;
    {class} property GSM_NETWORK_TIMEOUT: JGEDI_e_Ret read _GetGSM_NETWORK_TIMEOUT;
    {class} property GSM_NOT_INITIALIZED: JGEDI_e_Ret read _GetGSM_NOT_INITIALIZED;
    {class} property GSM_NOT_OPEN: JGEDI_e_Ret read _GetGSM_NOT_OPEN;
    {class} property GSM_NOT_REGISTERED: JGEDI_e_Ret read _GetGSM_NOT_REGISTERED;
    {class} property GSM_NO_NETWORK: JGEDI_e_Ret read _GetGSM_NO_NETWORK;
    {class} property GSM_OPERATION_NOT_ALLOWED: JGEDI_e_Ret read _GetGSM_OPERATION_NOT_ALLOWED;
    {class} property GSM_PLMN_NOT_ALLOWED: JGEDI_e_Ret read _GetGSM_PLMN_NOT_ALLOWED;
    {class} property GSM_PROCESSING: JGEDI_e_Ret read _GetGSM_PROCESSING;
    {class} property GSM_PROTOCOL_ERROR: JGEDI_e_Ret read _GetGSM_PROTOCOL_ERROR;
    {class} property GSM_REGISTERED: JGEDI_e_Ret read _GetGSM_REGISTERED;
    {class} property GSM_SERIAL_OPEN_FAILED: JGEDI_e_Ret read _GetGSM_SERIAL_OPEN_FAILED;
    {class} property GSM_SIGNAL_FAIL: JGEDI_e_Ret read _GetGSM_SIGNAL_FAIL;
    {class} property GSM_SIM_ALREADY_INITIALIZED: JGEDI_e_Ret read _GetGSM_SIM_ALREADY_INITIALIZED;
    {class} property GSM_SIM_BLOCKED: JGEDI_e_Ret read _GetGSM_SIM_BLOCKED;
    {class} property GSM_SIM_BUSY: JGEDI_e_Ret read _GetGSM_SIM_BUSY;
    {class} property GSM_SIM_FAIL: JGEDI_e_Ret read _GetGSM_SIM_FAIL;
    {class} property GSM_SIM_NOT_INITIALIZED: JGEDI_e_Ret read _GetGSM_SIM_NOT_INITIALIZED;
    {class} property GSM_SIM_NOT_INSERTED: JGEDI_e_Ret read _GetGSM_SIM_NOT_INSERTED;
    {class} property GSM_SIM_PIN_REQUIRED: JGEDI_e_Ret read _GetGSM_SIM_PIN_REQUIRED;
    {class} property GSM_SMS_CONNECTED: JGEDI_e_Ret read _GetGSM_SMS_CONNECTED;
    {class} property GSM_SMS_NOT_CONNECTED: JGEDI_e_Ret read _GetGSM_SMS_NOT_CONNECTED;
    {class} property GSM_SMS_RECEIVED_FAILED: JGEDI_e_Ret read _GetGSM_SMS_RECEIVED_FAILED;
    {class} property GSM_SMS_SEND_FAILED: JGEDI_e_Ret read _GetGSM_SMS_SEND_FAILED;
    {class} property GSM_STANDBY: JGEDI_e_Ret read _GetGSM_STANDBY;
    {class} property GSM_USSD_CONNECTED: JGEDI_e_Ret read _GetGSM_USSD_CONNECTED;
    {class} property GSM_USSD_FAILED: JGEDI_e_Ret read _GetGSM_USSD_FAILED;
    {class} property GSM_USSD_NOT_CONNECTED: JGEDI_e_Ret read _GetGSM_USSD_NOT_CONNECTED;
    {class} property GSM_USSD_NOT_OPEN: JGEDI_e_Ret read _GetGSM_USSD_NOT_OPEN;
    {class} property GSM_USSD_OPEN: JGEDI_e_Ret read _GetGSM_USSD_OPEN;
    {class} property I2C_ADDRESS_ERROR: JGEDI_e_Ret read _GetI2C_ADDRESS_ERROR;
    {class} property I2C_ERROR: JGEDI_e_Ret read _GetI2C_ERROR;
    {class} property I2C_OPEN_ERROR: JGEDI_e_Ret read _GetI2C_OPEN_ERROR;
    {class} property I2C_REGISTER_ERROR: JGEDI_e_Ret read _GetI2C_REGISTER_ERROR;
    {class} property IMPLEMENTATION_ERROR: JGEDI_e_Ret read _GetIMPLEMENTATION_ERROR;
    {class} property INFO_ERROR: JGEDI_e_Ret read _GetINFO_ERROR;
    {class} property INVALID_PARAMETER: JGEDI_e_Ret read _GetINVALID_PARAMETER;
    {class} property KBD_ERROR: JGEDI_e_Ret read _GetKBD_ERROR;
    {class} property KBD_IN_USE: JGEDI_e_Ret read _GetKBD_IN_USE;
    {class} property KMS_ERROR: JGEDI_e_Ret read _GetKMS_ERROR;
    {class} property KMS_FULL: JGEDI_e_Ret read _GetKMS_FULL;
    {class} property KMS_INVALID_CERTIFICATE: JGEDI_e_Ret read _GetKMS_INVALID_CERTIFICATE;
    {class} property KMS_INVALID_HASH: JGEDI_e_Ret read _GetKMS_INVALID_HASH;
    {class} property KMS_INVALID_INDEX: JGEDI_e_Ret read _GetKMS_INVALID_INDEX;
    {class} property KMS_INVALID_KCV: JGEDI_e_Ret read _GetKMS_INVALID_KCV;
    {class} property KMS_INVALID_KEK: JGEDI_e_Ret read _GetKMS_INVALID_KEK;
    {class} property KMS_INVALID_KEY: JGEDI_e_Ret read _GetKMS_INVALID_KEY;
    {class} property KMS_INVALID_LENGTH: JGEDI_e_Ret read _GetKMS_INVALID_LENGTH;
    {class} property KMS_INVALID_MODE: JGEDI_e_Ret read _GetKMS_INVALID_MODE;
    {class} property KMS_INVALID_OPERATION: JGEDI_e_Ret read _GetKMS_INVALID_OPERATION;
    {class} property KMS_INVALID_PIN_LEN: JGEDI_e_Ret read _GetKMS_INVALID_PIN_LEN;
    {class} property KMS_INVALID_PURPOSE: JGEDI_e_Ret read _GetKMS_INVALID_PURPOSE;
    {class} property KMS_INVALID_TYPE: JGEDI_e_Ret read _GetKMS_INVALID_TYPE;
    {class} property KMS_KEY_EXPIRED: JGEDI_e_Ret read _GetKMS_KEY_EXPIRED;
    {class} property KMS_KEY_NOT_UNIQUE: JGEDI_e_Ret read _GetKMS_KEY_NOT_UNIQUE;
    {class} property KMS_KEY_ON_COOLDOWN: JGEDI_e_Ret read _GetKMS_KEY_ON_COOLDOWN;
    {class} property KMS_NULL_PIN: JGEDI_e_Ret read _GetKMS_NULL_PIN;
    {class} property KMS_PINMODE_ACTIVE: JGEDI_e_Ret read _GetKMS_PINMODE_ACTIVE;
    {class} property KMS_PINMODE_INACTIVE: JGEDI_e_Ret read _GetKMS_PINMODE_INACTIVE;
    {class} property KMS_PINMODE_NO_KEY: JGEDI_e_Ret read _GetKMS_PINMODE_NO_KEY;
    {class} property KMS_USER_CANCELLED: JGEDI_e_Ret read _GetKMS_USER_CANCELLED;
    {class} property LCD_ERROR: JGEDI_e_Ret read _GetLCD_ERROR;
    {class} property LCD_FONT_ERROR: JGEDI_e_Ret read _GetLCD_FONT_ERROR;
    {class} property LED_BACKLIGHT_ERROR: JGEDI_e_Ret read _GetLED_BACKLIGHT_ERROR;
    {class} property LED_BACKLIGHT_NOT_EXIST: JGEDI_e_Ret read _GetLED_BACKLIGHT_NOT_EXIST;
    {class} property LED_ERROR: JGEDI_e_Ret read _GetLED_ERROR;
    {class} property LED_NOT_EXIST: JGEDI_e_Ret read _GetLED_NOT_EXIST;
    {class} property LINUX_ERROR: JGEDI_e_Ret read _GetLINUX_ERROR;
    {class} property MENU_NOT_INITIALIZED: JGEDI_e_Ret read _GetMENU_NOT_INITIALIZED;
    {class} property MIFARE_ERROR: JGEDI_e_Ret read _GetMIFARE_ERROR;
    {class} property MIFARE_KEYA_REQUIRED: JGEDI_e_Ret read _GetMIFARE_KEYA_REQUIRED;
    {class} property MIFARE_KEYB_REQUIRED: JGEDI_e_Ret read _GetMIFARE_KEYB_REQUIRED;
    {class} property MIFARE_NOT_FORMATTED: JGEDI_e_Ret read _GetMIFARE_NOT_FORMATTED;
    {class} property MIFARE_PROTECTED: JGEDI_e_Ret read _GetMIFARE_PROTECTED;
    {class} property MODEM_ALREADY_OPEN: JGEDI_e_Ret read _GetMODEM_ALREADY_OPEN;
    {class} property MODEM_CMD_FAIL: JGEDI_e_Ret read _GetMODEM_CMD_FAIL;
    {class} property MODEM_ERROR: JGEDI_e_Ret read _GetMODEM_ERROR;
    {class} property MODEM_NOT_ONLINE: JGEDI_e_Ret read _GetMODEM_NOT_ONLINE;
    {class} property MODEM_NOT_OPEN: JGEDI_e_Ret read _GetMODEM_NOT_OPEN;
    {class} property MODEM_TX_BUSY: JGEDI_e_Ret read _GetMODEM_TX_BUSY;
    {class} property MODULE_NOT_FOUND: JGEDI_e_Ret read _GetMODULE_NOT_FOUND;
    {class} property MSR_ERROR: JGEDI_e_Ret read _GetMSR_ERROR;
    {class} property MSR_NO_SWIPE: JGEDI_e_Ret read _GetMSR_NO_SWIPE;
    {class} property MSR_TRACK_ERROR: JGEDI_e_Ret read _GetMSR_TRACK_ERROR;
    {class} property NET_BINDTODEVICE_FAILED: JGEDI_e_Ret read _GetNET_BINDTODEVICE_FAILED;
    {class} property NET_ERROR: JGEDI_e_Ret read _GetNET_ERROR;
    {class} property NET_PING_FAILED: JGEDI_e_Ret read _GetNET_PING_FAILED;
    {class} property NET_SELECTDEFAULTDEVICE_FAILED: JGEDI_e_Ret read _GetNET_SELECTDEFAULTDEVICE_FAILED;
    {class} property NET_URL_NOT_FOUND: JGEDI_e_Ret read _GetNET_URL_NOT_FOUND;
    {class} property NOT_AVAILABLE: JGEDI_e_Ret read _GetNOT_AVAILABLE;
    {class} property NOT_SUPPORTED: JGEDI_e_Ret read _GetNOT_SUPPORTED;
    {class} property NULL_PARAMETER: JGEDI_e_Ret read _GetNULL_PARAMETER;
    {class} property OK: JGEDI_e_Ret read _GetOK;
    {class} property OUT_OF_BOUNDS: JGEDI_e_Ret read _GetOUT_OF_BOUNDS;
    {class} property PENALTY_ACTIVE: JGEDI_e_Ret read _GetPENALTY_ACTIVE;
    {class} property PENALTY_ERROR: JGEDI_e_Ret read _GetPENALTY_ERROR;
    {class} property PKI_CMAC_ERROR: JGEDI_e_Ret read _GetPKI_CMAC_ERROR;
    {class} property PKI_CSR_NOT_SIGNED: JGEDI_e_Ret read _GetPKI_CSR_NOT_SIGNED;
    {class} property PKI_DATE_NOT_FOUND: JGEDI_e_Ret read _GetPKI_DATE_NOT_FOUND;
    {class} property PKI_DERIVATION_ERROR: JGEDI_e_Ret read _GetPKI_DERIVATION_ERROR;
    {class} property PKI_ERROR: JGEDI_e_Ret read _GetPKI_ERROR;
    {class} property PKI_INVALID_CERTIFICATE: JGEDI_e_Ret read _GetPKI_INVALID_CERTIFICATE;
    {class} property PKI_INVALID_CHALLENGE: JGEDI_e_Ret read _GetPKI_INVALID_CHALLENGE;
    {class} property PKI_INVALID_CMAC: JGEDI_e_Ret read _GetPKI_INVALID_CMAC;
    {class} property PKI_INVALID_DATE: JGEDI_e_Ret read _GetPKI_INVALID_DATE;
    {class} property PKI_INVALID_KEY: JGEDI_e_Ret read _GetPKI_INVALID_KEY;
    {class} property PKI_KBPK_MISSING: JGEDI_e_Ret read _GetPKI_KBPK_MISSING;
    {class} property PKI_NO_CA_FOUND: JGEDI_e_Ret read _GetPKI_NO_CA_FOUND;
    {class} property PKI_NO_CERTIFICATE_FOUND: JGEDI_e_Ret read _GetPKI_NO_CERTIFICATE_FOUND;
    {class} property PKI_NO_CUST_KEY_FOUND: JGEDI_e_Ret read _GetPKI_NO_CUST_KEY_FOUND;
    {class} property PKI_NO_DEV_KEY_FOUND: JGEDI_e_Ret read _GetPKI_NO_DEV_KEY_FOUND;
    {class} property PKI_TR31_BLOCK_ERROR: JGEDI_e_Ret read _GetPKI_TR31_BLOCK_ERROR;
    {class} property PLATFORM_ERROR: JGEDI_e_Ret read _GetPLATFORM_ERROR;
    {class} property PM_ALREADY_SEALED: JGEDI_e_Ret read _GetPM_ALREADY_SEALED;
    {class} property PM_AP_NAME_REPEATED: JGEDI_e_Ret read _GetPM_AP_NAME_REPEATED;
    {class} property PM_DELETE_FAILED_ABORTED: JGEDI_e_Ret read _GetPM_DELETE_FAILED_ABORTED;
    {class} property PM_DELETE_FAILED_DEVICE_POLICY_MANAGER: JGEDI_e_Ret read _GetPM_DELETE_FAILED_DEVICE_POLICY_MANAGER;
    {class} property PM_DELETE_FAILED_INTERNAL_ERROR: JGEDI_e_Ret read _GetPM_DELETE_FAILED_INTERNAL_ERROR;
    {class} property PM_DELETE_FAILED_OWNER_BLOCKED: JGEDI_e_Ret read _GetPM_DELETE_FAILED_OWNER_BLOCKED;
    {class} property PM_DELETE_FAILED_USED_SHARED_LIBRARY: JGEDI_e_Ret read _GetPM_DELETE_FAILED_USED_SHARED_LIBRARY;
    {class} property PM_DELETE_FAILED_USER_RESTRICTED: JGEDI_e_Ret read _GetPM_DELETE_FAILED_USER_RESTRICTED;
    {class} property PM_ERROR: JGEDI_e_Ret read _GetPM_ERROR;
    {class} property PM_FULL: JGEDI_e_Ret read _GetPM_FULL;
    {class} property PM_INSTALL_FAILED_CONFLICTING_PROVIDER: JGEDI_e_Ret read _GetPM_INSTALL_FAILED_CONFLICTING_PROVIDER;
    {class} property PM_INSTALL_FAILED_CONTAINER_ERROR: JGEDI_e_Ret read _GetPM_INSTALL_FAILED_CONTAINER_ERROR;
    {class} property PM_INSTALL_FAILED_CPU_ABI_INCOMPATIBLE: JGEDI_e_Ret read _GetPM_INSTALL_FAILED_CPU_ABI_INCOMPATIBLE;
    {class} property PM_INSTALL_FAILED_DEXOPT: JGEDI_e_Ret read _GetPM_INSTALL_FAILED_DEXOPT;
    {class} property PM_INSTALL_FAILED_DUPLICATE_PACKAGE: JGEDI_e_Ret read _GetPM_INSTALL_FAILED_DUPLICATE_PACKAGE;
    {class} property PM_INSTALL_FAILED_DUPLICATE_PERMISSION: JGEDI_e_Ret read _GetPM_INSTALL_FAILED_DUPLICATE_PERMISSION;
    {class} property PM_INSTALL_FAILED_INSUFFICIENT_STORAGE: JGEDI_e_Ret read _GetPM_INSTALL_FAILED_INSUFFICIENT_STORAGE;
    {class} property PM_INSTALL_FAILED_INTERNAL_ERROR: JGEDI_e_Ret read _GetPM_INSTALL_FAILED_INTERNAL_ERROR;
    {class} property PM_INSTALL_FAILED_INVALID_APK: JGEDI_e_Ret read _GetPM_INSTALL_FAILED_INVALID_APK;
    {class} property PM_INSTALL_FAILED_INVALID_INSTALL_LOCATION: JGEDI_e_Ret read _GetPM_INSTALL_FAILED_INVALID_INSTALL_LOCATION;
    {class} property PM_INSTALL_FAILED_INVALID_URI: JGEDI_e_Ret read _GetPM_INSTALL_FAILED_INVALID_URI;
    {class} property PM_INSTALL_FAILED_MEDIA_UNAVAILABLE: JGEDI_e_Ret read _GetPM_INSTALL_FAILED_MEDIA_UNAVAILABLE;
    {class} property PM_INSTALL_FAILED_MISSING_FEATURE: JGEDI_e_Ret read _GetPM_INSTALL_FAILED_MISSING_FEATURE;
    {class} property PM_INSTALL_FAILED_MISSING_SHARED_LIBRARY: JGEDI_e_Ret read _GetPM_INSTALL_FAILED_MISSING_SHARED_LIBRARY;
    {class} property PM_INSTALL_FAILED_NEWER_SDK: JGEDI_e_Ret read _GetPM_INSTALL_FAILED_NEWER_SDK;
    {class} property PM_INSTALL_FAILED_NO_MATCHING_ABIS: JGEDI_e_Ret read _GetPM_INSTALL_FAILED_NO_MATCHING_ABIS;
    {class} property PM_INSTALL_FAILED_NO_SHARED_USER: JGEDI_e_Ret read _GetPM_INSTALL_FAILED_NO_SHARED_USER;
    {class} property PM_INSTALL_FAILED_OLDER_SDK: JGEDI_e_Ret read _GetPM_INSTALL_FAILED_OLDER_SDK;
    {class} property PM_INSTALL_FAILED_PACKAGE_CHANGED: JGEDI_e_Ret read _GetPM_INSTALL_FAILED_PACKAGE_CHANGED;
    {class} property PM_INSTALL_FAILED_REPLACE_COULDNT_DELETE: JGEDI_e_Ret read _GetPM_INSTALL_FAILED_REPLACE_COULDNT_DELETE;
    {class} property PM_INSTALL_FAILED_SHARED_USER_INCOMPATIBLE: JGEDI_e_Ret read _GetPM_INSTALL_FAILED_SHARED_USER_INCOMPATIBLE;
    {class} property PM_INSTALL_FAILED_TEST_ONLY: JGEDI_e_Ret read _GetPM_INSTALL_FAILED_TEST_ONLY;
    {class} property PM_INSTALL_FAILED_UID_CHANGED: JGEDI_e_Ret read _GetPM_INSTALL_FAILED_UID_CHANGED;
    {class} property PM_INSTALL_FAILED_UNKNOWN_SOURCES: JGEDI_e_Ret read _GetPM_INSTALL_FAILED_UNKNOWN_SOURCES;
    {class} property PM_INSTALL_FAILED_UPDATE_INCOMPATIBLE: JGEDI_e_Ret read _GetPM_INSTALL_FAILED_UPDATE_INCOMPATIBLE;
    {class} property PM_INSTALL_FAILED_USER_RESTRICTED: JGEDI_e_Ret read _GetPM_INSTALL_FAILED_USER_RESTRICTED;
    {class} property PM_INSTALL_FAILED_VERIFICATION_FAILURE: JGEDI_e_Ret read _GetPM_INSTALL_FAILED_VERIFICATION_FAILURE;
    {class} property PM_INSTALL_FAILED_VERIFICATION_TIMEOUT: JGEDI_e_Ret read _GetPM_INSTALL_FAILED_VERIFICATION_TIMEOUT;
    {class} property PM_INSTALL_FAILED_VERSION_DOWNGRADE: JGEDI_e_Ret read _GetPM_INSTALL_FAILED_VERSION_DOWNGRADE;
    {class} property PM_INSTALL_PARSE_FAILED_BAD_MANIFEST: JGEDI_e_Ret read _GetPM_INSTALL_PARSE_FAILED_BAD_MANIFEST;
    {class} property PM_INSTALL_PARSE_FAILED_BAD_PACKAGE_NAME: JGEDI_e_Ret read _GetPM_INSTALL_PARSE_FAILED_BAD_PACKAGE_NAME;
    {class} property PM_INSTALL_PARSE_FAILED_BAD_SHARED_USER_ID: JGEDI_e_Ret read _GetPM_INSTALL_PARSE_FAILED_BAD_SHARED_USER_ID;
    {class} property PM_INSTALL_PARSE_FAILED_CERTIFICATE_ENCODING: JGEDI_e_Ret read _GetPM_INSTALL_PARSE_FAILED_CERTIFICATE_ENCODING;
    {class} property PM_INSTALL_PARSE_FAILED_INCONSISTENT_CERTIFICATES: JGEDI_e_Ret read _GetPM_INSTALL_PARSE_FAILED_INCONSISTENT_CERTIFICATES;
    {class} property PM_INSTALL_PARSE_FAILED_MANIFEST_EMPTY: JGEDI_e_Ret read _GetPM_INSTALL_PARSE_FAILED_MANIFEST_EMPTY;
    {class} property PM_INSTALL_PARSE_FAILED_MANIFEST_MALFORMED: JGEDI_e_Ret read _GetPM_INSTALL_PARSE_FAILED_MANIFEST_MALFORMED;
    {class} property PM_INSTALL_PARSE_FAILED_NOT_APK: JGEDI_e_Ret read _GetPM_INSTALL_PARSE_FAILED_NOT_APK;
    {class} property PM_INSTALL_PARSE_FAILED_NO_CERTIFICATES: JGEDI_e_Ret read _GetPM_INSTALL_PARSE_FAILED_NO_CERTIFICATES;
    {class} property PM_INSTALL_PARSE_FAILED_UNEXPECTED_EXCEPTION: JGEDI_e_Ret read _GetPM_INSTALL_PARSE_FAILED_UNEXPECTED_EXCEPTION;
    {class} property PM_INVALID_AP: JGEDI_e_Ret read _GetPM_INVALID_AP;
    {class} property PM_INVALID_FILE: JGEDI_e_Ret read _GetPM_INVALID_FILE;
    {class} property PM_NOT_SEALED: JGEDI_e_Ret read _GetPM_NOT_SEALED;
    {class} property PM_SEAL_ERROR: JGEDI_e_Ret read _GetPM_SEAL_ERROR;
    {class} property PM_SIGN_ERROR: JGEDI_e_Ret read _GetPM_SIGN_ERROR;
    {class} property PM_ULD_FAIL: JGEDI_e_Ret read _GetPM_ULD_FAIL;
    {class} property POWER_BATTERY_BUSY: JGEDI_e_Ret read _GetPOWER_BATTERY_BUSY;
    {class} property POWER_ERROR: JGEDI_e_Ret read _GetPOWER_ERROR;
    {class} property POWER_NO_BATTERY: JGEDI_e_Ret read _GetPOWER_NO_BATTERY;
    {class} property POWER_NO_EXT_POWER: JGEDI_e_Ret read _GetPOWER_NO_EXT_POWER;
    {class} property PRNTR_ERROR: JGEDI_e_Ret read _GetPRNTR_ERROR;
    {class} property PRNTR_FONT_PATH_LEN: JGEDI_e_Ret read _GetPRNTR_FONT_PATH_LEN;
    {class} property PRNTR_NOT_READY: JGEDI_e_Ret read _GetPRNTR_NOT_READY;
    {class} property PRNTR_OUT_OF_PAPER: JGEDI_e_Ret read _GetPRNTR_OUT_OF_PAPER;
    {class} property PRNTR_OVERHEAT: JGEDI_e_Ret read _GetPRNTR_OVERHEAT;
    {class} property RS232_BUSY: JGEDI_e_Ret read _GetRS232_BUSY;
    {class} property RS232_ERROR: JGEDI_e_Ret read _GetRS232_ERROR;
    {class} property RS232_NOT_OPEN: JGEDI_e_Ret read _GetRS232_NOT_OPEN;
    {class} property RSA_GEN_FAIL: JGEDI_e_Ret read _GetRSA_GEN_FAIL;
    {class} property SMART_ABSENT: JGEDI_e_Ret read _GetSMART_ABSENT;
    {class} property SMART_ATR_TOO_LONG: JGEDI_e_Ret read _GetSMART_ATR_TOO_LONG;
    {class} property SMART_BAD_TS: JGEDI_e_Ret read _GetSMART_BAD_TS;
    {class} property SMART_COMM_ERROR: JGEDI_e_Ret read _GetSMART_COMM_ERROR;
    {class} property SMART_DEACTIVATED_PROTOCOL: JGEDI_e_Ret read _GetSMART_DEACTIVATED_PROTOCOL;
    {class} property SMART_EDC_ERROR: JGEDI_e_Ret read _GetSMART_EDC_ERROR;
    {class} property SMART_ERROR: JGEDI_e_Ret read _GetSMART_ERROR;
    {class} property SMART_INVALID_ATR: JGEDI_e_Ret read _GetSMART_INVALID_ATR;
    {class} property SMART_MUTE: JGEDI_e_Ret read _GetSMART_MUTE;
    {class} property SMART_NOT_ACTIVATED: JGEDI_e_Ret read _GetSMART_NOT_ACTIVATED;
    {class} property SMART_PARITY_ERROR: JGEDI_e_Ret read _GetSMART_PARITY_ERROR;
    {class} property SMART_POWER_FAILED: JGEDI_e_Ret read _GetSMART_POWER_FAILED;
    {class} property SMART_PROCEDURE_BYTE_CONFLICT: JGEDI_e_Ret read _GetSMART_PROCEDURE_BYTE_CONFLICT;
    {class} property SMART_PTS_RESPONSE_ERROR: JGEDI_e_Ret read _GetSMART_PTS_RESPONSE_ERROR;
    {class} property SMART_TA1_NOT_SUPPORTED: JGEDI_e_Ret read _GetSMART_TA1_NOT_SUPPORTED;
    {class} property SYS_ERROR: JGEDI_e_Ret read _GetSYS_ERROR;
    {class} property TEST_FAIL: JGEDI_e_Ret read _GetTEST_FAIL;
    {class} property TIMEOUT: JGEDI_e_Ret read _GetTIMEOUT;
    {class} property USB_ALREADY_OPEN: JGEDI_e_Ret read _GetUSB_ALREADY_OPEN;
    {class} property USB_BUSY: JGEDI_e_Ret read _GetUSB_BUSY;
    {class} property USB_ERROR: JGEDI_e_Ret read _GetUSB_ERROR;
    {class} property USB_NOT_OPEN: JGEDI_e_Ret read _GetUSB_NOT_OPEN;
    {class} property UTIL_ERROR: JGEDI_e_Ret read _GetUTIL_ERROR;
    {class} property WIFI_CMD_FAIL: JGEDI_e_Ret read _GetWIFI_CMD_FAIL;
    {class} property WIFI_CONNECT_FAILED: JGEDI_e_Ret read _GetWIFI_CONNECT_FAILED;
    {class} property WIFI_ERROR: JGEDI_e_Ret read _GetWIFI_ERROR;
    {class} property WIFI_INVALID_CONFIG: JGEDI_e_Ret read _GetWIFI_INVALID_CONFIG;
    {class} property WIFI_NOT_OPEN: JGEDI_e_Ret read _GetWIFI_NOT_OPEN;
    {class} property WIFI_PROCESSING: JGEDI_e_Ret read _GetWIFI_PROCESSING;
    {class} property WIFI_SCAN_FAILED: JGEDI_e_Ret read _GetWIFI_SCAN_FAILED;
  end;

  [JavaSignature('br/com/gertec/gedi/enums/GEDI_e_Ret')]
  JGEDI_e_Ret = interface(JEnum)
    ['{50FEC9FC-9AAC-4E0B-919A-5CB8F44FE93E}']
    function getValue: Integer; cdecl;
  end;
  TJGEDI_e_Ret = class(TJavaGenericImport<JGEDI_e_RetClass, JGEDI_e_Ret>) end;

  JIKBDClass = interface(IJavaClass)
    ['{0EF92B63-C1C7-42B5-8C91-FA7C8B284354}']
  end;

  [JavaSignature('br/com/gertec/gedi/interfaces/IKBD')]
  JIKBD = interface(IJavaInstance)
    ['{1EE2483D-A0BE-4602-8179-D1134A018F44}']
    procedure &Set(gEDI_KBD_st_Info: JGEDI_KBD_st_Info); cdecl;
  end;
  TJIKBD = class(TJavaGenericImport<JIKBDClass, JIKBD>) end;

  JGEDI_KMS_e_BLOCKTYPEClass = interface(JEnumClass)
    ['{9E64C421-C71D-46F1-9071-F30E4B3F8CC4}']
    {class} function _GetANSI_0: JGEDI_KMS_e_BLOCKTYPE; cdecl;
    {class} function _GetANSI_1: JGEDI_KMS_e_BLOCKTYPE; cdecl;
    {class} function _GetAS2805_1: JGEDI_KMS_e_BLOCKTYPE; cdecl;
    {class} function _GetAS2805_8: JGEDI_KMS_e_BLOCKTYPE; cdecl;
    {class} function _GetBANKSYS: JGEDI_KMS_e_BLOCKTYPE; cdecl;
    {class} function _GetBBB_16: JGEDI_KMS_e_BLOCKTYPE; cdecl;
    {class} function _GetCEF_0: JGEDI_KMS_e_BLOCKTYPE; cdecl;
    {class} function _GetCEF_1: JGEDI_KMS_e_BLOCKTYPE; cdecl;
    {class} function _GetCEF_CID: JGEDI_KMS_e_BLOCKTYPE; cdecl;
    {class} function _GetCEF_PWR: JGEDI_KMS_e_BLOCKTYPE; cdecl;
    {class} function _GetDIEBOLD: JGEDI_KMS_e_BLOCKTYPE; cdecl;
    {class} function _GetDIEBOLD_CN: JGEDI_KMS_e_BLOCKTYPE; cdecl;
    {class} function _GetDOCUTEL: JGEDI_KMS_e_BLOCKTYPE; cdecl;
    {class} function _GetDOCUTEL_2: JGEDI_KMS_e_BLOCKTYPE; cdecl;
    {class} function _GetECI_1: JGEDI_KMS_e_BLOCKTYPE; cdecl;
    {class} function _GetECI_2: JGEDI_KMS_e_BLOCKTYPE; cdecl;
    {class} function _GetECI_3: JGEDI_KMS_e_BLOCKTYPE; cdecl;
    {class} function _GetECI_4: JGEDI_KMS_e_BLOCKTYPE; cdecl;
    {class} function _GetGERTEC_0: JGEDI_KMS_e_BLOCKTYPE; cdecl;
    {class} function _GetGERTEC_1: JGEDI_KMS_e_BLOCKTYPE; cdecl;
    {class} function _GetIBM_3621: JGEDI_KMS_e_BLOCKTYPE; cdecl;
    {class} function _GetIBM_3624: JGEDI_KMS_e_BLOCKTYPE; cdecl;
    {class} function _GetIBM_4704: JGEDI_KMS_e_BLOCKTYPE; cdecl;
    {class} function _GetIBM_5906: JGEDI_KMS_e_BLOCKTYPE; cdecl;
    {class} function _GetISO_0: JGEDI_KMS_e_BLOCKTYPE; cdecl;
    {class} function _GetISO_1: JGEDI_KMS_e_BLOCKTYPE; cdecl;
    {class} function _GetISO_2: JGEDI_KMS_e_BLOCKTYPE; cdecl;
    {class} function _GetISO_3: JGEDI_KMS_e_BLOCKTYPE; cdecl;
    {class} function _GetISO_4: JGEDI_KMS_e_BLOCKTYPE; cdecl;
    {class} function _GetNCR: JGEDI_KMS_e_BLOCKTYPE; cdecl;
    {class} function _GetOEM_1: JGEDI_KMS_e_BLOCKTYPE; cdecl;
    {class} function _GetVISA_1: JGEDI_KMS_e_BLOCKTYPE; cdecl;
    {class} function _GetVISA_2: JGEDI_KMS_e_BLOCKTYPE; cdecl;
    {class} function _GetVISA_3: JGEDI_KMS_e_BLOCKTYPE; cdecl;
    {class} function _GetVISA_4: JGEDI_KMS_e_BLOCKTYPE; cdecl;
    {class} function valueOf(string_1: JString): JGEDI_KMS_e_BLOCKTYPE; overload; cdecl;
    {class} function valueOf(int: Integer): JGEDI_KMS_e_BLOCKTYPE; overload; cdecl;
    {class} function values: TJavaObjectArray<JGEDI_KMS_e_BLOCKTYPE>; cdecl;
    {class} property ANSI_0: JGEDI_KMS_e_BLOCKTYPE read _GetANSI_0;
    {class} property ANSI_1: JGEDI_KMS_e_BLOCKTYPE read _GetANSI_1;
    {class} property AS2805_1: JGEDI_KMS_e_BLOCKTYPE read _GetAS2805_1;
    {class} property AS2805_8: JGEDI_KMS_e_BLOCKTYPE read _GetAS2805_8;
    {class} property BANKSYS: JGEDI_KMS_e_BLOCKTYPE read _GetBANKSYS;
    {class} property BBB_16: JGEDI_KMS_e_BLOCKTYPE read _GetBBB_16;
    {class} property CEF_0: JGEDI_KMS_e_BLOCKTYPE read _GetCEF_0;
    {class} property CEF_1: JGEDI_KMS_e_BLOCKTYPE read _GetCEF_1;
    {class} property CEF_CID: JGEDI_KMS_e_BLOCKTYPE read _GetCEF_CID;
    {class} property CEF_PWR: JGEDI_KMS_e_BLOCKTYPE read _GetCEF_PWR;
    {class} property DIEBOLD: JGEDI_KMS_e_BLOCKTYPE read _GetDIEBOLD;
    {class} property DIEBOLD_CN: JGEDI_KMS_e_BLOCKTYPE read _GetDIEBOLD_CN;
    {class} property DOCUTEL: JGEDI_KMS_e_BLOCKTYPE read _GetDOCUTEL;
    {class} property DOCUTEL_2: JGEDI_KMS_e_BLOCKTYPE read _GetDOCUTEL_2;
    {class} property ECI_1: JGEDI_KMS_e_BLOCKTYPE read _GetECI_1;
    {class} property ECI_2: JGEDI_KMS_e_BLOCKTYPE read _GetECI_2;
    {class} property ECI_3: JGEDI_KMS_e_BLOCKTYPE read _GetECI_3;
    {class} property ECI_4: JGEDI_KMS_e_BLOCKTYPE read _GetECI_4;
    {class} property GERTEC_0: JGEDI_KMS_e_BLOCKTYPE read _GetGERTEC_0;
    {class} property GERTEC_1: JGEDI_KMS_e_BLOCKTYPE read _GetGERTEC_1;
    {class} property IBM_3621: JGEDI_KMS_e_BLOCKTYPE read _GetIBM_3621;
    {class} property IBM_3624: JGEDI_KMS_e_BLOCKTYPE read _GetIBM_3624;
    {class} property IBM_4704: JGEDI_KMS_e_BLOCKTYPE read _GetIBM_4704;
    {class} property IBM_5906: JGEDI_KMS_e_BLOCKTYPE read _GetIBM_5906;
    {class} property ISO_0: JGEDI_KMS_e_BLOCKTYPE read _GetISO_0;
    {class} property ISO_1: JGEDI_KMS_e_BLOCKTYPE read _GetISO_1;
    {class} property ISO_2: JGEDI_KMS_e_BLOCKTYPE read _GetISO_2;
    {class} property ISO_3: JGEDI_KMS_e_BLOCKTYPE read _GetISO_3;
    {class} property ISO_4: JGEDI_KMS_e_BLOCKTYPE read _GetISO_4;
    {class} property NCR: JGEDI_KMS_e_BLOCKTYPE read _GetNCR;
    {class} property OEM_1: JGEDI_KMS_e_BLOCKTYPE read _GetOEM_1;
    {class} property VISA_1: JGEDI_KMS_e_BLOCKTYPE read _GetVISA_1;
    {class} property VISA_2: JGEDI_KMS_e_BLOCKTYPE read _GetVISA_2;
    {class} property VISA_3: JGEDI_KMS_e_BLOCKTYPE read _GetVISA_3;
    {class} property VISA_4: JGEDI_KMS_e_BLOCKTYPE read _GetVISA_4;
  end;

  [JavaSignature('br/com/gertec/gedi/enums/GEDI_KMS_e_BLOCKTYPE')]
  JGEDI_KMS_e_BLOCKTYPE = interface(JEnum)
    ['{6CA86DD5-B047-4CE2-93DD-7E5837F45619}']
    function getValue: Integer; cdecl;
  end;
  TJGEDI_KMS_e_BLOCKTYPE = class(TJavaGenericImport<JGEDI_KMS_e_BLOCKTYPEClass, JGEDI_KMS_e_BLOCKTYPE>) end;

  JISYSClass = interface(IJavaClass)
    ['{5804E447-3E3E-437D-9F7E-6CC0D4022A53}']
  end;

  [JavaSignature('br/com/gertec/gedi/interfaces/ISYS')]
  JISYS = interface(IJavaInstance)
    ['{26014C95-A428-4DD3-80F7-6B849FB69EEB}']
  end;
  TJISYS = class(TJavaGenericImport<JISYSClass, JISYS>) end;

  JGEDI_INFO_e_ModuleClass = interface(JEnumClass)
    ['{01AFAD12-C6D4-476F-B3FA-C6452B997773}']
    {class} function _GetBTASTACK_SO: JGEDI_INFO_e_Module; cdecl;
    {class} function _GetCL_AEP: JGEDI_INFO_e_Module; cdecl;
    {class} function _GetCL_DDP: JGEDI_INFO_e_Module; cdecl;
    {class} function _GetCL_KERNEL: JGEDI_INFO_e_Module; cdecl;
    {class} function _GetCL_MPP: JGEDI_INFO_e_Module; cdecl;
    {class} function _GetCL_PURE: JGEDI_INFO_e_Module; cdecl;
    {class} function _GetCL_VPW: JGEDI_INFO_e_Module; cdecl;
    {class} function _GetEMV_L1: JGEDI_INFO_e_Module; cdecl;
    {class} function _GetEMV_L2: JGEDI_INFO_e_Module; cdecl;
    {class} function _GetHW: JGEDI_INFO_e_Module; cdecl;
    {class} function _GetIODA_SO: JGEDI_INFO_e_Module; cdecl;
    {class} function _GetPCI: JGEDI_INFO_e_Module; cdecl;
    {class} function _GetSOLO_SO: JGEDI_INFO_e_Module; cdecl;
    {class} function valueOf(int: Integer): JGEDI_INFO_e_Module; overload; cdecl;
    {class} function valueOf(string_1: JString): JGEDI_INFO_e_Module; overload; cdecl;
    {class} function values: TJavaObjectArray<JGEDI_INFO_e_Module>; cdecl;
    {class} property BTASTACK_SO: JGEDI_INFO_e_Module read _GetBTASTACK_SO;
    {class} property CL_AEP: JGEDI_INFO_e_Module read _GetCL_AEP;
    {class} property CL_DDP: JGEDI_INFO_e_Module read _GetCL_DDP;
    {class} property CL_KERNEL: JGEDI_INFO_e_Module read _GetCL_KERNEL;
    {class} property CL_MPP: JGEDI_INFO_e_Module read _GetCL_MPP;
    {class} property CL_PURE: JGEDI_INFO_e_Module read _GetCL_PURE;
    {class} property CL_VPW: JGEDI_INFO_e_Module read _GetCL_VPW;
    {class} property EMV_L1: JGEDI_INFO_e_Module read _GetEMV_L1;
    {class} property EMV_L2: JGEDI_INFO_e_Module read _GetEMV_L2;
    {class} property HW: JGEDI_INFO_e_Module read _GetHW;
    {class} property IODA_SO: JGEDI_INFO_e_Module read _GetIODA_SO;
    {class} property PCI: JGEDI_INFO_e_Module read _GetPCI;
    {class} property SOLO_SO: JGEDI_INFO_e_Module read _GetSOLO_SO;
  end;

  [JavaSignature('br/com/gertec/gedi/enums/GEDI_INFO_e_Module')]
  JGEDI_INFO_e_Module = interface(JEnum)
    ['{BF6D8FCC-9D67-4491-8AA9-5D3BEBC31EC8}']
    function getValue: Integer; cdecl;
  end;
  TJGEDI_INFO_e_Module = class(TJavaGenericImport<JGEDI_INFO_e_ModuleClass, JGEDI_INFO_e_Module>) end;

  JGEDI_CL_e_ISO_TypeClass = interface(JEnumClass)
    ['{8B2932E0-2D31-4BDF-B50D-B166ABAED40A}']
    {class} function _GetA_3: JGEDI_CL_e_ISO_Type; cdecl;
    {class} function _GetA_4: JGEDI_CL_e_ISO_Type; cdecl;
    {class} function _GetB: JGEDI_CL_e_ISO_Type; cdecl;
    {class} function _GetERR: JGEDI_CL_e_ISO_Type; cdecl;
    {class} function valueOf(int: Integer): JGEDI_CL_e_ISO_Type; overload; cdecl;
    {class} function valueOf(string_1: JString): JGEDI_CL_e_ISO_Type; overload; cdecl;
    {class} function values: TJavaObjectArray<JGEDI_CL_e_ISO_Type>; cdecl;
    {class} property A_3: JGEDI_CL_e_ISO_Type read _GetA_3;
    {class} property A_4: JGEDI_CL_e_ISO_Type read _GetA_4;
    {class} property B: JGEDI_CL_e_ISO_Type read _GetB;
    {class} property ERR: JGEDI_CL_e_ISO_Type read _GetERR;
  end;

  [JavaSignature('br/com/gertec/gedi/enums/GEDI_CL_e_ISO_Type')]
  JGEDI_CL_e_ISO_Type = interface(JEnum)
    ['{07AEB343-FE62-40CE-B9C9-E69EEEF4D463}']
    function getValue: Integer; cdecl;
  end;
  TJGEDI_CL_e_ISO_Type = class(TJavaGenericImport<JGEDI_CL_e_ISO_TypeClass, JGEDI_CL_e_ISO_Type>) end;

  JzClass = interface(JObjectClass)
    ['{810CF99A-CB83-4636-9429-BE1AA3F697A6}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function init(parcel: JParcel): Jz; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('gedi/z')]
  Jz = interface(JObject)
    ['{CF359B12-AF5B-4C05-A903-65911034B47A}']
    function describeContents: Integer; cdecl;
    procedure writeToParcel(parcel: JParcel; int: Integer); cdecl;
  end;
  TJz = class(TJavaGenericImport<JzClass, Jz>) end;

  Jlibbasebinder_aClass = interface(JObjectClass)
    ['{FD8BCA5C-B8D4-4F3A-9A50-85363D41EB38}']
    {class} function _GetBINDER_NONE: Integer; cdecl;
    {class} function init: Jlibbasebinder_a; cdecl;
    {class} property BINDER_NONE: Integer read _GetBINDER_NONE;
  end;

  [JavaSignature('wangpos/sdk4/libbasebinder/a')]
  Jlibbasebinder_a = interface(JObject)
    ['{C1BAA0FB-5684-465E-ACA9-3E74D3525DEB}']
    function isServiceWork(context: JContext; string_1: JString): Boolean; cdecl;
  end;
  TJlibbasebinder_a = class(TJavaGenericImport<Jlibbasebinder_aClass, Jlibbasebinder_a>) end;

  Jgedi_lClass = interface(JObjectClass)
    ['{30086FB2-04F4-43EA-8AA3-74E0BF0DF842}']
    {class} function init: Jgedi_l; overload; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/l')]
  Jgedi_l = interface(JObject)
    ['{EE089F11-8FB4-4601-8CD9-CF42E8652B67}']
    procedure DrawBarCode(gEDI_PRNTR_st_BarCodeConfig: JGEDI_PRNTR_st_BarCodeConfig; string_1: JString); cdecl;
    procedure DrawBlankLine(int: Integer); cdecl;
    procedure DrawPictureExt(gEDI_PRNTR_st_PictureConfig: JGEDI_PRNTR_st_PictureConfig; bitmap: JBitmap); cdecl;
    procedure DrawStringExt(gEDI_PRNTR_st_StringConfig: JGEDI_PRNTR_st_StringConfig; string_1: JString); cdecl;
    function GetPaperUsage: Integer; cdecl;
    procedure Init; overload; cdecl;
    procedure Output; cdecl;
    procedure ResetPaperUsage; cdecl;
    procedure SetPrintDensity(gEDI_PRNTR_e_PrintDensity: JGEDI_PRNTR_e_PrintDensity); cdecl;
    function Status: JGEDI_PRNTR_e_Status; cdecl;
  end;
  TJgedi_l = class(TJavaGenericImport<Jgedi_lClass, Jgedi_l>) end;

  JGEDI_SMART_st_ResetInfoClass = interface(JObjectClass)
    ['{9AF08B2B-7B68-46F8-A494-800C9F201FC0}']
    {class} function init: JGEDI_SMART_st_ResetInfo; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_SMART_st_ResetInfo')]
  JGEDI_SMART_st_ResetInfo = interface(JObject)
    ['{D6E60294-CD05-4C45-959D-31C172B38766}']
    function _GetabATR: TJavaArray<Byte>; cdecl;
    function _GetpeCardType: JGEDI_SMART_e_Type; cdecl;
    property abATR: TJavaArray<Byte> read _GetabATR;
    property peCardType: JGEDI_SMART_e_Type read _GetpeCardType;
  end;
  TJGEDI_SMART_st_ResetInfo = class(TJavaGenericImport<JGEDI_SMART_st_ResetInfoClass, JGEDI_SMART_st_ResetInfo>) end;

  JmClass = interface(JObjectClass)
    ['{B408031B-5943-4CA3-9880-D148F9760328}']
    {class} function init: Jm; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/m')]
  Jm = interface(JObject)
    ['{9FC14BCC-D254-4E3A-92D0-B9D672010F30}']
    procedure PowerOff(gEDI_SMART_e_Slot: JGEDI_SMART_e_Slot); cdecl;
    function ResetEMV(gEDI_SMART_e_Slot: JGEDI_SMART_e_Slot; gEDI_SMART_e_Voltage: JGEDI_SMART_e_Voltage): JGEDI_SMART_st_ResetInfo; cdecl;
    function SendAPDU(gEDI_SMART_e_Slot: JGEDI_SMART_e_Slot; bytes: TJavaArray<Byte>): TJavaArray<Byte>; cdecl;
    function Status(gEDI_SMART_e_Slot: JGEDI_SMART_e_Slot): JGEDI_SMART_e_Status; cdecl;
    function WarmResetEMV(gEDI_SMART_e_Slot: JGEDI_SMART_e_Slot; gEDI_SMART_e_Voltage: JGEDI_SMART_e_Voltage): JGEDI_SMART_st_ResetInfo; cdecl;
  end;
  TJm = class(TJavaGenericImport<JmClass, Jm>) end;

  JyClass = interface(JObjectClass)
    ['{F9749869-722B-438B-9DC1-2EFA6C860C28}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function init(parcel: JParcel): Jy; overload; cdecl;
    {class} function init: Jy; overload; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('gedi/y')]
  Jy = interface(JObject)
    ['{CDF428E7-FE1B-47F5-8A26-FED508C510BE}']
    function describeContents: Integer; cdecl;
    procedure writeToParcel(parcel: JParcel; int: Integer); cdecl;
  end;
  TJy = class(TJavaGenericImport<JyClass, Jy>) end;

  JsClass = interface(JIInterfaceClass)
    ['{917F0DD4-EAEF-4EAD-8228-AE5A94A8D8B3}']
  end;

  [JavaSignature('gedi/s')]
  Js = interface(JIInterface)
    ['{C3D06014-1CD5-4297-88D4-B8AFE3C1716B}']
    function A: Integer; overload; cdecl;
    function B: JString; overload; cdecl;
    function C: Integer; overload; cdecl;
    function D: Integer; overload; cdecl;
    function E: Integer; overload; cdecl;
    function F: Integer; overload; cdecl;
    function G: Integer; overload; cdecl;
    function H: Integer; overload; cdecl;
    function a(string_1: JString; v: Jv): Integer; overload; cdecl;
    function a(w: Jw): Integer; overload; cdecl;
    function a(bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>; bytes_1: TJavaArray<Byte>; ints_1: TJavaArray<Integer>): Integer; overload; cdecl;
//Mesmo    function a: JString; overload; cdecl;
    function a(byte: Byte; int_1: Integer; int: Integer; bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>; string_1: JString): Integer; overload; cdecl;
    function a(int: Integer; int_1: Integer; int_2: Integer; bytes: TJavaArray<Byte>; int_3: Integer; bytes_1: TJavaArray<Byte>; int_4: Integer; int_5: Integer; int_6: Integer; ints: TJavaArray<Integer>; bytes_2: TJavaArray<Byte>; ints_1: TJavaArray<Integer>; bytes_3: TJavaArray<Byte>; ints_2: TJavaArray<Integer>; bytes_4: TJavaArray<Byte>): Integer; overload; cdecl;
    function a(int: Integer; int_1: Integer; int_2: Integer; int_3: Integer; bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function a(map: JMap): Integer; overload; cdecl;
    function a(bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function a(int: Integer; int_1: Integer; bytes: TJavaArray<Byte>; bytes_1: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function a(int: Integer; v: Jv): Integer; overload; cdecl;
    function a(bytes: TJavaArray<Byte>): Integer; overload; cdecl;
    function a(bytes: TJavaArray<Byte>; bytes_1: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function a(string_1: JString; string_2: JString; int: Integer; int_1: Integer): Integer; overload; cdecl;
    function a(string_1: JString; int: Integer; int_1: Integer; int_2: Integer): Integer; overload; cdecl;
    function a(int: Integer; int_1: Integer; int_2: Integer; ints: TJavaArray<Integer>; bytes: TJavaArray<Byte>; ints_1: TJavaArray<Integer>): Integer; overload; cdecl;
    function a(int: Integer; int_1: Integer; int_2: Integer; bytes: TJavaArray<Byte>; int_3: Integer; bytes_1: TJavaArray<Byte>; bytes_2: TJavaArray<Byte>): Integer; overload; cdecl;
    function a(int: Integer; int_1: Integer; int_2: Integer; bytes: TJavaArray<Byte>; bytes_1: TJavaArray<Byte>; bytes_2: TJavaArray<Byte>; bytes_3: TJavaArray<Byte>; int_3: Integer; bytes_4: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function a(int: Integer; int_1: Integer; int_2: Integer; bytes: TJavaArray<Byte>; bytes_1: TJavaArray<Byte>; bytes_2: TJavaArray<Byte>; bytes_3: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    procedure a(string_1: JString); overload; cdecl;
    function a(int: Integer; int_1: Integer; int_2: Integer; bytes: TJavaArray<Byte>): Integer; overload; cdecl;
    function a(int: Integer; int_1: Integer; int_2: Integer; bytes: TJavaArray<Byte>; bytes_1: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function a(int: Integer; int_1: Integer; int_2: Integer; bytes: TJavaArray<Byte>; bytes_1: TJavaArray<Byte>; bytes_2: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function a(int: Integer; int_1: Integer; int_2: Integer): Integer; overload; cdecl;
    function a(string_1: JString; int: Integer; bytes: TJavaArray<Byte>): Integer; overload; cdecl;
    function a(x: Jx): Integer; overload; cdecl;
    function a(int: Integer; int_1: Integer; bytes: TJavaArray<Byte>; int_2: Integer; int_3: Integer; bytes_1: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function a(string_1: JString; boolean: Boolean): JList; overload; cdecl;
    function a(string_1: JString; int: Integer; boolean: Boolean): Jy; overload; cdecl;
    function a(int: Integer; int_1: Integer; int_2: Integer; int_3: Integer; int_4: Integer; bytes: TJavaArray<Byte>; int_5: Integer; bytes_1: TJavaArray<Byte>; int_6: Integer; u: Ju): Integer; overload; cdecl;
    function a(bytes: TJavaArray<Byte>; int: Integer; int_1: Integer; bytes_1: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function a(int: Integer; bytes: TJavaArray<Byte>; int_1: Integer; bytes_1: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function a(int: Integer; int_1: Integer; int_2: Integer; int_3: Integer; int_4: Integer; int_5: Integer; bytes: TJavaArray<Byte>; int_6: Integer; bytes_1: TJavaArray<Byte>; u: Ju): Integer; overload; cdecl;
    function a(string_1: JString; int: Integer; int_1: Integer; bytes: TJavaArray<Byte>; int_2: Integer; bytes_1: TJavaArray<Byte>; int_3: Integer; bytes_2: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function a(int: Integer; int_1: Integer; int_2: Integer; int_3: Integer): Integer; overload; cdecl;
    function a(int: Integer; int_1: Integer; int_2: Integer; int_3: Integer; u: Ju): Integer; overload; cdecl;
    function a(int: Integer; list: JList; bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function a(string_1: JString; string_2: JString; int: Integer; int_1: Integer; boolean: Boolean): Integer; overload; cdecl;
    function a(int: Integer; boolean: Boolean; int_1: Integer; bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function a(bytes: TJavaArray<Byte>; int: Integer; bytes_1: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function a(int: Integer; int_1: Integer; int_2: Integer; int_3: Integer; int_4: Integer; bytes: TJavaArray<Byte>; u: Ju; int_5: Integer; bytes_1: TJavaArray<Byte>; bytes_2: TJavaArray<Byte>): Integer; overload; cdecl;
    function a(int: Integer; int_1: Integer; int_2: Integer; int_3: Integer; bytes: TJavaArray<Byte>; bytes_1: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function a(string_1: JString; int: Integer; int_1: Integer; boolean: Boolean; boolean_1: Boolean; boolean_2: Boolean): Integer; overload; cdecl;
    function a(int: Integer; int_1: Integer; int_2: Integer; int_3: Integer; int_4: Integer; bytes: TJavaArray<Byte>; int_5: Integer; bytes_1: TJavaArray<Byte>; ints: TJavaArray<Integer>; u: Ju): Integer; overload; cdecl;
    function a(bluetoothDevice: JBluetoothDevice): Integer; overload; cdecl;
    function a(z: Jz; w: Jw): Integer; overload; cdecl;
    function a(string_1: JString; boolean: Boolean; y_1: Jy): Integer; overload; cdecl;
    function a(boolean: Boolean): Integer; overload; cdecl;
    function a(int: Integer; int_1: Integer): Integer; overload; cdecl;
    function a(ints: TJavaArray<Integer>; strings: TJavaObjectArray<JString>; int: Integer; int_1: Integer; boolean: Boolean; boolean_1: Boolean): Integer; overload; cdecl;
    function a(string_1: JString; int: Integer; bytes: TJavaArray<Byte>; int_1: Integer; bytes_1: TJavaArray<Byte>; int_2: Integer; bytes_2: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function a(byte: Byte; int_1: Integer; int: Integer; int_2: Integer; int_3: Integer; bytes: TJavaArray<Byte>; int_4: Integer; bytes_1: TJavaArray<Byte>; int_5: Integer; bytes_2: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function a(int: Integer; int_1: Integer; string_1: JString; int_2: Integer; int_3: Integer; bytes: TJavaArray<Byte>; int_4: Integer; bytes_1: TJavaArray<Byte>; int_5: Integer; bytes_2: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function a(bytes: TJavaArray<Byte>; int: Integer): Integer; overload; cdecl;
    function a(int: Integer; string_1: JString; int_1: Integer; int_2: Integer; int_3: Integer; int_4: Integer; bytes: TJavaArray<Byte>; int_5: Integer; bytes_1: TJavaArray<Byte>; u: Ju): Integer; overload; cdecl;
    function a(int: Integer; int_1: Integer; int_2: Integer; bytes: TJavaArray<Byte>; int_3: Integer; bytes_1: TJavaArray<Byte>; int_4: Integer; bytes_2: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function a(int: Integer; int_1: Integer; int_2: Integer; int_3: Integer; int_4: Integer; bytes: TJavaArray<Byte>; int_5: Integer; bytes_1: TJavaArray<Byte>; int_6: Integer; bytes_2: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function a(byte: Byte; int_1: Integer; int: Integer; bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>; int_2: Integer; int_3: Integer; int_4: Integer): Integer; overload; cdecl;
    function a(int: Integer; bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function a(int: Integer; int_1: Integer; int_2: Integer; bytes: TJavaArray<Byte>; int_3: Integer; bytes_1: TJavaArray<Byte>; bytes_2: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function a(int: Integer; int_1: Integer; int_2: Integer; int_3: Integer; int_4: Integer): Integer; overload; cdecl;
    function a(string_1: JString; int: Integer; float: Single; float_1: Single; int_1: Integer; int_2: Integer; int_3: Integer; boolean: Boolean; boolean_1: Boolean; boolean_2: Boolean): Integer; overload; cdecl;
    function a(string_1: JString; int: Integer; int_1: Integer; boolean: Boolean; boolean_1: Boolean): Integer; overload; cdecl;
    function a(ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function a(int: Integer; int_1: Integer; int_2: Integer; bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function a(int: Integer; int_1: Integer; int_2: Integer; bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>; string_1: JString): Integer; overload; cdecl;
    function a(int: Integer; bytes: TJavaArray<Byte>; int_1: Integer): Integer; overload; cdecl;
    function a(string_1: JString; int: Integer; int_1: Integer; int_2: Integer; int_3: Integer): Integer; overload; cdecl;
    function a(bitmap: JBitmap; int: Integer; int_1: Integer; int_2: Integer; int_3: Integer): Integer; overload; cdecl;
    function a(bitmap: JBitmap; int: Integer; int_1: Integer): Integer; overload; cdecl;
    function a(string_1: JString; int: Integer; float: Single; float_1: Single; int_1: Integer; int_2: Integer; boolean: Boolean; boolean_1: Boolean; boolean_2: Boolean): Integer; overload; cdecl;
    function a(int: Integer; int_1: Integer; ints: TJavaArray<Integer>; bytes: TJavaArray<Byte>): Integer; overload; cdecl;
    function a(bytes: TJavaArray<Byte>; int: Integer; int_1: Integer; bytes_1: TJavaArray<Byte>; bytes_2: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function a(string_1: JString; int: Integer; float: Single; float_1: Single; float_2: Single; int_1: Integer; int_2: Integer; int_3: Integer; boolean: Boolean; boolean_1: Boolean; boolean_2: Boolean): Integer; overload; cdecl;
    function a(string_1: JString; string_2: JString; float: Single; int: Integer; int_1: Integer; int_2: Integer; boolean: Boolean; boolean_1: Boolean; boolean_2: Boolean): Integer; overload; cdecl;
    function a(int: Integer; int_1: Integer; int_2: Integer; int_3: Integer; int_4: Integer; int_5: Integer; int_6: Integer): Integer; overload; cdecl;
    function a(int: Integer; int_1: Integer; bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function b(byte: Byte; int_1: Integer; int: Integer; bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>; string_1: JString): Integer; overload; cdecl;
    function b(bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>; bytes_1: TJavaArray<Byte>; ints_1: TJavaArray<Integer>): Integer; overload; cdecl;
    function b(int: Integer; bytes: TJavaArray<Byte>): Integer; overload; cdecl;
    function b(int: Integer): Integer; overload; cdecl;
    procedure b(bytes: TJavaArray<Byte>); overload; cdecl;
    function b(boolean: Boolean): Integer; overload; cdecl;
    function b(int: Integer; int_1: Integer; bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function b(string_1: JString; int: Integer; int_1: Integer; int_2: Integer; int_3: Integer): Integer; overload; cdecl;
    function b(bytes: TJavaArray<Byte>; int: Integer): Integer; overload; cdecl;
    function b(int: Integer; int_1: Integer; int_2: Integer; bytes: TJavaArray<Byte>; int_3: Integer; bytes_1: TJavaArray<Byte>; int_4: Integer; bytes_2: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function b(bitmap: JBitmap; int: Integer; int_1: Integer; int_2: Integer; int_3: Integer): Integer; overload; cdecl;
    function b(bytes: TJavaArray<Byte>; int: Integer; bytes_1: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function b(int: Integer; int_1: Integer; int_2: Integer; int_3: Integer; int_4: Integer; bytes: TJavaArray<Byte>; int_5: Integer; bytes_1: TJavaArray<Byte>; int_6: Integer; bytes_2: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function b(string_1: JString; int: Integer; int_1: Integer; bytes: TJavaArray<Byte>; int_2: Integer; bytes_1: TJavaArray<Byte>; int_3: Integer; bytes_2: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function b(int: Integer; bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    procedure b(int: Integer; int_1: Integer); overload; cdecl;
    function b(int: Integer; string_1: JString; int_1: Integer; int_2: Integer; int_3: Integer; int_4: Integer; bytes: TJavaArray<Byte>; int_5: Integer; bytes_1: TJavaArray<Byte>; u: Ju): Integer; overload; cdecl;
    function b(bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function b(int: Integer; int_1: Integer; bytes: TJavaArray<Byte>; bytes_1: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function b(string_1: JString; int: Integer; boolean: Boolean): Integer; overload; cdecl;
    function b(int: Integer; bytes: TJavaArray<Byte>; bytes_1: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function b(int: Integer; int_1: Integer; int_2: Integer): Integer; overload; cdecl;
    function b(int: Integer; int_1: Integer; int_2: Integer; int_3: Integer; bytes: TJavaArray<Byte>; bytes_1: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function b(int: Integer; int_1: Integer; string_1: JString; int_2: Integer; int_3: Integer; bytes: TJavaArray<Byte>; int_4: Integer; bytes_1: TJavaArray<Byte>; int_5: Integer; bytes_2: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
//Mesmo    function b: Integer; overload; cdecl;
    function b(int: Integer; bytes: TJavaArray<Byte>; int_1: Integer; bytes_1: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function b(bytes: TJavaArray<Byte>; bytes_1: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function b(int: Integer; int_1: Integer; int_2: Integer; int_3: Integer; bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function c(bytes: TJavaArray<Byte>; int: Integer; bytes_1: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function c(string_1: JString): Integer; overload; cdecl;
    function c(bytes: TJavaArray<Byte>; int: Integer): Integer; overload; cdecl;
    function c(int: Integer; int_1: Integer; int_2: Integer): Integer; overload; cdecl;
    function c(bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function c(boolean: Boolean): Integer; overload; cdecl;
    function c(int: Integer; string_1: JString; int_1: Integer; int_2: Integer; int_3: Integer; int_4: Integer; bytes: TJavaArray<Byte>; int_5: Integer; bytes_1: TJavaArray<Byte>; u: Ju): Integer; overload; cdecl;
    function c(int: Integer): Integer; overload; cdecl;
    function c(int: Integer; int_1: Integer; int_2: Integer; int_3: Integer; bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function c(bytes: TJavaArray<Byte>): Integer; overload; cdecl;
    function c(int: Integer; int_1: Integer; string_1: JString; int_2: Integer; int_3: Integer; bytes: TJavaArray<Byte>; int_4: Integer; bytes_1: TJavaArray<Byte>; int_5: Integer; bytes_2: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function c(int: Integer; bytes: TJavaArray<Byte>): Integer; overload; cdecl;
//Mesmo    function c: JString; overload; cdecl;
    function c(string_1: JString; int: Integer; int_1: Integer; bytes: TJavaArray<Byte>; int_2: Integer; bytes_1: TJavaArray<Byte>; int_3: Integer; bytes_2: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function c(int: Integer; bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function c(int: Integer; int_1: Integer): Integer; overload; cdecl;
    function c(int: Integer; bytes: TJavaArray<Byte>; bytes_1: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function c(int: Integer; int_1: Integer; bytes: TJavaArray<Byte>; bytes_1: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function c(int: Integer; int_1: Integer; bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function d(int: Integer; int_1: Integer; bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function d(int: Integer; int_1: Integer): Integer; overload; cdecl;
    function d(int: Integer; bytes: TJavaArray<Byte>): Integer; overload; cdecl;
    function d(bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function d(int: Integer; bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function d(int: Integer): Integer; overload; cdecl;
    function d(bytes: TJavaArray<Byte>; int: Integer): Integer; overload; cdecl;
//Mesmo    function d: Integer; overload; cdecl;
    function d(bytes: TJavaArray<Byte>; int: Integer; bytes_1: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function d(int: Integer; int_1: Integer; int_2: Integer): Integer; overload; cdecl;
    function d(bytes: TJavaArray<Byte>): Integer; overload; cdecl;
    function e(bytes: TJavaArray<Byte>): Integer; overload; cdecl;
    function e(bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
//Mesmo    function e: Integer; overload; cdecl;
    function e(int: Integer): Integer; overload; cdecl;
    function e(bytes: TJavaArray<Byte>; int: Integer; bytes_1: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function e(bytes: TJavaArray<Byte>; int: Integer): Integer; overload; cdecl;
    function e(int: Integer; bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function f(bytes: TJavaArray<Byte>): Integer; overload; cdecl;
    function f(int: Integer): Integer; overload; cdecl;
//Mesmo    function f: Integer; overload; cdecl;
    function g(int: Integer): Integer; overload; cdecl;
//Mesmo    function g: Integer; overload; cdecl;
    function g(bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function g(bytes: TJavaArray<Byte>): Integer; overload; cdecl;
    function h(int: Integer): Integer; overload; cdecl;
    function h(bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
//Mesmo    function h: Integer; overload; cdecl;
    procedure h(bytes: TJavaArray<Byte>); overload; cdecl;
    function i: Integer; overload; cdecl;
    function i(bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function i(int: Integer): Integer; overload; cdecl;
    function j(int: Integer): Integer; overload; cdecl;
    function j: Integer; overload; cdecl;
    function k(int: Integer): Integer; overload; cdecl;
    function k: Integer; overload; cdecl;
    function k(bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function l(int: Integer): Integer; overload; cdecl;
    function l(bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function m: Integer; overload; cdecl;
    function m(bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function m(int: Integer): Integer; overload; cdecl;
    function n(bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function n: Integer; overload; cdecl;
    function n(int: Integer): Integer; overload; cdecl;
    function o: Integer; overload; cdecl;
    function o(int: Integer): Integer; overload; cdecl;
    function p(int: Integer): Integer; cdecl;
    function q: JString; overload; cdecl;
    function q(int: Integer): Integer; overload; cdecl;
    function r: JString; cdecl;
    function s: Integer; overload; cdecl;
    function s(int: Integer): Integer; overload; cdecl;
    function t: Integer; overload; cdecl;
    function t(int: Integer): Integer; overload; cdecl;
    function u: Integer; cdecl;
    function v: JString; cdecl;
    function w: Integer; cdecl;
    function x: Integer; cdecl;
    function y: Integer; cdecl;
  end;
  TJs = class(TJavaGenericImport<JsClass, Js>) end;

  JIINFOClass = interface(IJavaClass)
    ['{DB5213E5-90DD-4C31-B8B6-C1C8BC2CA059}']
  end;

  [JavaSignature('br/com/gertec/gedi/interfaces/IINFO')]
  JIINFO = interface(IJavaInstance)
    ['{0FFA68A3-4E86-497C-9883-4E3C77840896}']
    function ControlNumberGet(gEDI_INFO_e_ControlNumber: JGEDI_INFO_e_ControlNumber): JString; cdecl;
    function FirmwareVersionGet: JString; cdecl;
    function ImgVersionNumberGet: Integer; cdecl;
    function ModuleVersionGet(gEDI_INFO_e_Module: JGEDI_INFO_e_Module): JString; cdecl;
    function ProductNameGet: JString; cdecl;
  end;
  TJIINFO = class(TJavaGenericImport<JIINFOClass, JIINFO>) end;

  JGEDI_SYS_e_SecuritySetupClass = interface(JEnumClass)
    ['{B09CC0BD-A7B5-4F78-BFE8-EF5C839F55EB}']
    {class} function _GetDISABLE_TRANSPORT_KEY_CHECK: JGEDI_SYS_e_SecuritySetup; cdecl;
    {class} function _GetKMS_DES: JGEDI_SYS_e_SecuritySetup; cdecl;
    {class} function _GetKMS_DUKPT_DES: JGEDI_SYS_e_SecuritySetup; cdecl;
    {class} function _GetKMS_DUPLICATES: JGEDI_SYS_e_SecuritySetup; cdecl;
    {class} function _GetKMS_PINBLOCK_DISABLE_ALGORITHM_CHECK: JGEDI_SYS_e_SecuritySetup; cdecl;
    {class} function _GetKMS_PINBLOCK_ENABLE_PLAINTEXT: JGEDI_SYS_e_SecuritySetup; cdecl;
    {class} function _GetKMS_PIN_MAX_EVENT_TIMEOUT: JGEDI_SYS_e_SecuritySetup; cdecl;
    {class} function _GetKMS_PIN_MAX_RATE: JGEDI_SYS_e_SecuritySetup; cdecl;
    {class} function _GetKMS_PIN_MAX_TIMEOUT: JGEDI_SYS_e_SecuritySetup; cdecl;
    {class} function _GetKMS_RSA_MIN: JGEDI_SYS_e_SecuritySetup; cdecl;
    {class} function _GetKMS_SHA1: JGEDI_SYS_e_SecuritySetup; cdecl;
    {class} function _GetRESTORE_DEFAULT: JGEDI_SYS_e_SecuritySetup; cdecl;
    {class} function _GetWIFI_OPEN: JGEDI_SYS_e_SecuritySetup; cdecl;
    {class} function _GetWIFI_WEP: JGEDI_SYS_e_SecuritySetup; cdecl;
    {class} function valueOf(string_1: JString): JGEDI_SYS_e_SecuritySetup; cdecl;
    {class} function values: TJavaObjectArray<JGEDI_SYS_e_SecuritySetup>; cdecl;
    {class} property DISABLE_TRANSPORT_KEY_CHECK: JGEDI_SYS_e_SecuritySetup read _GetDISABLE_TRANSPORT_KEY_CHECK;
    {class} property KMS_DES: JGEDI_SYS_e_SecuritySetup read _GetKMS_DES;
    {class} property KMS_DUKPT_DES: JGEDI_SYS_e_SecuritySetup read _GetKMS_DUKPT_DES;
    {class} property KMS_DUPLICATES: JGEDI_SYS_e_SecuritySetup read _GetKMS_DUPLICATES;
    {class} property KMS_PINBLOCK_DISABLE_ALGORITHM_CHECK: JGEDI_SYS_e_SecuritySetup read _GetKMS_PINBLOCK_DISABLE_ALGORITHM_CHECK;
    {class} property KMS_PINBLOCK_ENABLE_PLAINTEXT: JGEDI_SYS_e_SecuritySetup read _GetKMS_PINBLOCK_ENABLE_PLAINTEXT;
    {class} property KMS_PIN_MAX_EVENT_TIMEOUT: JGEDI_SYS_e_SecuritySetup read _GetKMS_PIN_MAX_EVENT_TIMEOUT;
    {class} property KMS_PIN_MAX_RATE: JGEDI_SYS_e_SecuritySetup read _GetKMS_PIN_MAX_RATE;
    {class} property KMS_PIN_MAX_TIMEOUT: JGEDI_SYS_e_SecuritySetup read _GetKMS_PIN_MAX_TIMEOUT;
    {class} property KMS_RSA_MIN: JGEDI_SYS_e_SecuritySetup read _GetKMS_RSA_MIN;
    {class} property KMS_SHA1: JGEDI_SYS_e_SecuritySetup read _GetKMS_SHA1;
    {class} property RESTORE_DEFAULT: JGEDI_SYS_e_SecuritySetup read _GetRESTORE_DEFAULT;
    {class} property WIFI_OPEN: JGEDI_SYS_e_SecuritySetup read _GetWIFI_OPEN;
    {class} property WIFI_WEP: JGEDI_SYS_e_SecuritySetup read _GetWIFI_WEP;
  end;

  [JavaSignature('br/com/gertec/gedi/enums/GEDI_SYS_e_SecuritySetup')]
  JGEDI_SYS_e_SecuritySetup = interface(JEnum)
    ['{B4E99ABD-E2D3-46AD-BAD7-411F04F96F83}']
    function getValue: Integer; cdecl;
  end;
  TJGEDI_SYS_e_SecuritySetup = class(TJavaGenericImport<JGEDI_SYS_e_SecuritySetupClass, JGEDI_SYS_e_SecuritySetup>) end;

  Jgedi_fClass = interface(JObjectClass)
    ['{A7690082-C5BD-4D98-981B-2E993C77CB34}']
    {class} function init: Jgedi_f; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/f')]
  Jgedi_f = interface(JObject)
    ['{4EF90460-7A63-4F6C-9AD5-ADE77957A5FE}']
    procedure &Set(gEDI_KBD_st_Info: JGEDI_KBD_st_Info); cdecl;
    function a(ints: TJavaArray<Integer>; int: Integer; boolean: Boolean): Integer; overload; cdecl;
    function a(int: Integer; boolean: Boolean): JGEDI_KBD_e_Key; overload; cdecl;
  end;
  TJgedi_f = class(TJavaGenericImport<Jgedi_fClass, Jgedi_f>) end;

  JGEDI_KBD_e_KeyClass = interface(JEnumClass)
    ['{AEAA80FA-C456-4D42-B099-8C743DE0847C}']
    {class} function _GetASTERISK: JGEDI_KBD_e_Key; cdecl;
    {class} function _GetCANCEL: JGEDI_KBD_e_Key; cdecl;
    {class} function _GetCLEAR: JGEDI_KBD_e_Key; cdecl;
    {class} function _GetDOT: JGEDI_KBD_e_Key; cdecl;
    {class} function _GetDOWN: JGEDI_KBD_e_Key; cdecl;
    {class} function _GetENTER: JGEDI_KBD_e_Key; cdecl;
    {class} function _GetF1: JGEDI_KBD_e_Key; cdecl;
    {class} function _GetF2: JGEDI_KBD_e_Key; cdecl;
    {class} function _GetF3: JGEDI_KBD_e_Key; cdecl;
    {class} function _GetF4: JGEDI_KBD_e_Key; cdecl;
    {class} function _GetINVALID: JGEDI_KBD_e_Key; cdecl;
    {class} function _GetNONE: JGEDI_KBD_e_Key; cdecl;
    {class} function _GetNUM0: JGEDI_KBD_e_Key; cdecl;
    {class} function _GetNUM1: JGEDI_KBD_e_Key; cdecl;
    {class} function _GetNUM2: JGEDI_KBD_e_Key; cdecl;
    {class} function _GetNUM3: JGEDI_KBD_e_Key; cdecl;
    {class} function _GetNUM4: JGEDI_KBD_e_Key; cdecl;
    {class} function _GetNUM5: JGEDI_KBD_e_Key; cdecl;
    {class} function _GetNUM6: JGEDI_KBD_e_Key; cdecl;
    {class} function _GetNUM7: JGEDI_KBD_e_Key; cdecl;
    {class} function _GetNUM8: JGEDI_KBD_e_Key; cdecl;
    {class} function _GetNUM9: JGEDI_KBD_e_Key; cdecl;
    {class} function _GetPOWER: JGEDI_KBD_e_Key; cdecl;
    {class} function _GetSHIFT: JGEDI_KBD_e_Key; cdecl;
    {class} function _GetUNKNOWN: JGEDI_KBD_e_Key; cdecl;
    {class} function _GetUP: JGEDI_KBD_e_Key; cdecl;
    {class} function valueOf(int: Integer): JGEDI_KBD_e_Key; overload; cdecl;
    {class} function valueOf(string_1: JString): JGEDI_KBD_e_Key; overload; cdecl;
    {class} function values: TJavaObjectArray<JGEDI_KBD_e_Key>; cdecl;
    {class} property ASTERISK: JGEDI_KBD_e_Key read _GetASTERISK;
    {class} property CANCEL: JGEDI_KBD_e_Key read _GetCANCEL;
    {class} property CLEAR: JGEDI_KBD_e_Key read _GetCLEAR;
    {class} property DOT: JGEDI_KBD_e_Key read _GetDOT;
    {class} property DOWN: JGEDI_KBD_e_Key read _GetDOWN;
    {class} property ENTER: JGEDI_KBD_e_Key read _GetENTER;
    {class} property F1: JGEDI_KBD_e_Key read _GetF1;
    {class} property F2: JGEDI_KBD_e_Key read _GetF2;
    {class} property F3: JGEDI_KBD_e_Key read _GetF3;
    {class} property F4: JGEDI_KBD_e_Key read _GetF4;
    {class} property INVALID: JGEDI_KBD_e_Key read _GetINVALID;
    {class} property NONE: JGEDI_KBD_e_Key read _GetNONE;
    {class} property NUM0: JGEDI_KBD_e_Key read _GetNUM0;
    {class} property NUM1: JGEDI_KBD_e_Key read _GetNUM1;
    {class} property NUM2: JGEDI_KBD_e_Key read _GetNUM2;
    {class} property NUM3: JGEDI_KBD_e_Key read _GetNUM3;
    {class} property NUM4: JGEDI_KBD_e_Key read _GetNUM4;
    {class} property NUM5: JGEDI_KBD_e_Key read _GetNUM5;
    {class} property NUM6: JGEDI_KBD_e_Key read _GetNUM6;
    {class} property NUM7: JGEDI_KBD_e_Key read _GetNUM7;
    {class} property NUM8: JGEDI_KBD_e_Key read _GetNUM8;
    {class} property NUM9: JGEDI_KBD_e_Key read _GetNUM9;
    {class} property POWER: JGEDI_KBD_e_Key read _GetPOWER;
    {class} property SHIFT: JGEDI_KBD_e_Key read _GetSHIFT;
    {class} property UNKNOWN: JGEDI_KBD_e_Key read _GetUNKNOWN;
    {class} property UP: JGEDI_KBD_e_Key read _GetUP;
  end;

  [JavaSignature('br/com/gertec/gedi/enums/GEDI_KBD_e_Key')]
  JGEDI_KBD_e_Key = interface(JEnum)
    ['{3D97B1B6-A15B-43F3-B505-2C27C3B0C47D}']
    function getValue: Integer; cdecl;
    procedure setValue(int: Integer); cdecl;
  end;
  TJGEDI_KBD_e_Key = class(TJavaGenericImport<JGEDI_KBD_e_KeyClass, JGEDI_KBD_e_Key>) end;

  JIGEDIClass = interface(IJavaClass)
    ['{A19D5FD4-2206-4939-A2E6-3EF95BBB1A09}']
  end;

  [JavaSignature('br/com/gertec/gedi/interfaces/IGEDI')]
  JIGEDI = interface(IJavaInstance)
    ['{E1F81DE3-A463-44FB-9BA1-05F910A7689E}']
    procedure EnterEng(string_1: JString); cdecl;
    function VersionGet: JString; cdecl;
    function getAUDIO: JIAUDIO; cdecl;
    function getCL: JICL; cdecl;
    function getCLOCK: JICLOCK; cdecl;
    function getCRYPT: JICRYPT; cdecl;
    function getINFO: JIINFO; cdecl;
    function getKBD: JIKBD; cdecl;
    function getKMS: JIKMS; cdecl;
    function getLED: JILED; cdecl;
    function getMSR: JIMSR; cdecl;
    function getPM: JIPM; cdecl;
    function getPRNTR: JIPRNTR; cdecl;
    function getSMART: JISMART; cdecl;
  end;
  TJIGEDI = class(TJavaGenericImport<JIGEDIClass, JIGEDI>) end;

  JGEDI_CL_st_ISO_PollingInfoClass = interface(JObjectClass)
    ['{F7E0BF53-E1E4-443A-BDDD-B96AE867F5A0}']
    {class} function init: JGEDI_CL_st_ISO_PollingInfo; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_CL_st_ISO_PollingInfo')]
  JGEDI_CL_st_ISO_PollingInfo = interface(JObject)
    ['{C5D8F8B9-B478-4453-A8D0-B332D756D550}']
    function _GetabATQA: TJavaArray<Byte>; cdecl;
    function _GetabATQB: TJavaArray<Byte>; cdecl;
    function _GetabATS: TJavaArray<Byte>; cdecl;
    function _GetabATTRIBResp: TJavaArray<Byte>; cdecl;
    function _GetabPUPI: TJavaArray<Byte>; cdecl;
    function _GetabUID: TJavaArray<Byte>; cdecl;
    function _GetbSAK: Byte; cdecl;
    function _GetpeType: JGEDI_CL_e_ISO_Type; cdecl;
    property abATQA: TJavaArray<Byte> read _GetabATQA;
    property abATQB: TJavaArray<Byte> read _GetabATQB;
    property abATS: TJavaArray<Byte> read _GetabATS;
    property abATTRIBResp: TJavaArray<Byte> read _GetabATTRIBResp;
    property abPUPI: TJavaArray<Byte> read _GetabPUPI;
    property abUID: TJavaArray<Byte> read _GetabUID;
    property bSAK: Byte read _GetbSAK;
    property peType: JGEDI_CL_e_ISO_Type read _GetpeType;
  end;
  TJGEDI_CL_st_ISO_PollingInfo = class(TJavaGenericImport<JGEDI_CL_st_ISO_PollingInfoClass, JGEDI_CL_st_ISO_PollingInfo>) end;

  JICLClass = interface(IJavaClass)
    ['{3F7CE17D-B02D-485A-A0D4-728FABD6DF12}']
  end;

  [JavaSignature('br/com/gertec/gedi/interfaces/ICL')]
  JICL = interface(IJavaInstance)
    ['{63165D28-0FB0-42D9-9BFB-BF8836BB7282}']
    function ISO_Polling(int: Integer): JGEDI_CL_st_ISO_PollingInfo; cdecl;
    procedure MF_Authentication(int: Integer; gEDI_CL_st_MF_Key: JGEDI_CL_st_MF_Key; bytes: TJavaArray<Byte>); cdecl;
    function MF_BlockRead(int: Integer): TJavaArray<Byte>; cdecl;
    procedure MF_BlockWrite(int: Integer; bytes: TJavaArray<Byte>); cdecl;
    procedure MF_Decrement(int: Integer; int_1: Integer); cdecl;
    procedure MF_Increment(int: Integer; int_1: Integer); cdecl;
    procedure MF_Restore(int: Integer; int_1: Integer); cdecl;
    function MF_SignatureGet(int: Integer): TJavaArray<Byte>; cdecl;
    procedure MF_Transfer(int: Integer); cdecl;
    procedure PowerOff; cdecl;
    procedure PowerOn; cdecl;
    function ResetEMV: JGEDI_CL_st_ResetEMVInfo; cdecl;
    function SendAPDU(bytes: TJavaArray<Byte>): TJavaArray<Byte>; cdecl;
  end;
  TJICL = class(TJavaGenericImport<JICLClass, JICL>) end;

  JgClass = interface(JObjectClass)
    ['{4B41E9EB-68FC-46C0-9E93-94E5344876A9}']
    {class} function init(button: JButton; button_1: JButton; button_2: JButton; button_3: JButton; button_4: JButton; button_5: JButton; button_6: JButton; button_7: JButton; button_8: JButton; button_9: JButton; button_10: JButton; button_11: JButton; button_12: JButton; activity: JActivity): Jg; overload; cdecl;
    {class} function init(gEDI_KBD_st_Info: JGEDI_KBD_st_Info): Jg; overload; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/g')]
  Jg = interface(JObject)
    ['{1E42014F-58D9-409D-B8D2-0E28B5EE4A09}']
    function _Geta: JButton; cdecl;
    function _Getb: JButton; cdecl;
    function _Getc: JButton; cdecl;
    function _Getd: JButton; cdecl;
    function _Gete: JButton; cdecl;
    function _Getf: JButton; cdecl;
    function _Getg: JButton; cdecl;
    function _Geth: JButton; cdecl;
    function _Geti: JButton; cdecl;
    function _Getj: JButton; cdecl;
    function _Getk: JButton; cdecl;
    function _Getl: JButton; cdecl;
    function _Getm: JButton; cdecl;
    function _Getn: JActivity; cdecl;
    function a(int: Integer): JButton; overload; cdecl;
    function a: Boolean; overload; cdecl;
    function b: Jg; cdecl;
    function c: JString; cdecl;
    function clone: JObject; cdecl;
    property a: JButton read _Geta;
    property b: JButton read _Getb;
    property c: JButton read _Getc;
    property d: JButton read _Getd;
    property e: JButton read _Gete;
    property f: JButton read _Getf;
    property g: JButton read _Getg;
    property h: JButton read _Geth;
    property i: JButton read _Geti;
    property j: JButton read _Getj;
    property k: JButton read _Getk;
    property l: JButton read _Getl;
    property m: JButton read _Getm;
    property n: JActivity read _Getn;
  end;
  TJg = class(TJavaGenericImport<JgClass, Jg>) end;

  JLoggerClass = interface(JObjectClass)
    ['{7808BD2F-D9AD-492B-BF28-9AE365111713}']
    {class} function getLogger(string_1: JString): JLogger; cdecl;
    {class} procedure log(int: Integer; string_1: JString; string_2: JString); overload; cdecl;
    {class} procedure log(int: Integer; bytes: TJavaArray<Byte>; bytes_1: TJavaArray<Byte>); overload; cdecl;
  end;

  [JavaSignature('br/com/gertec/Logger')]
  JLogger = interface(JObject)
    ['{841315D2-C56C-4365-AF2B-053C48F06689}']
    procedure debug(string_1: JString); overload; cdecl;
    procedure debug(string_1: JString; objects: TJavaObjectArray<JObject>); overload; cdecl;
    procedure error(string_1: JString); overload; cdecl;
    procedure error(string_1: JString; throwable: JThrowable); overload; cdecl;
    procedure error(string_1: JString; objects: TJavaObjectArray<JObject>); overload; cdecl;
    procedure info(string_1: JString; objects: TJavaObjectArray<JObject>); overload; cdecl;
    procedure info(string_1: JString); overload; cdecl;
    procedure warn(string_1: JString); overload; cdecl;
    procedure warn(string_1: JString; objects: TJavaObjectArray<JObject>); overload; cdecl;
  end;
  TJLogger = class(TJavaGenericImport<JLoggerClass, JLogger>) end;

  Jf0Class = interface(JObjectClass)
    ['{57565AE6-C08D-415A-9F47-A58F36B1F676}']
    {class} procedure a(context: JContext); cdecl;
  end;

  [JavaSignature('gedi/f0')]
  Jf0 = interface(JObject)
    ['{C14FF55D-4CB1-4DA9-A26A-82F447FD8270}']
  end;
  TJf0 = class(TJavaGenericImport<Jf0Class, Jf0>) end;

  Jd0Class = interface(JObjectClass)
    ['{BAA85B39-ECE5-412F-92F1-9E2DD81C3724}']
    {class} function init: Jd0; cdecl;
  end;

  [JavaSignature('gedi/d0')]
  Jd0 = interface(JObject)
    ['{2FDEB6B8-4BE4-49C4-AF42-C8DF6ECBAEC2}']
  end;
  TJd0 = class(TJavaGenericImport<Jd0Class, Jd0>) end;

  JtClass = interface(JIInterfaceClass)
    ['{92352AB1-CACD-4DA6-B089-AF98A2A775C8}']
  end;

  [JavaSignature('gedi/t')]
  Jt = interface(JIInterface)
    ['{A47E8A2D-9FAB-4DAF-80F5-DE956FAF3C42}']
    function a(int: Integer): JIBinder; cdecl;
  end;
  TJt = class(TJavaGenericImport<JtClass, Jt>) end;

  Jb0Class = interface(JIInterfaceClass)
    ['{D1CF8D5C-1D16-4D46-9AA0-84183E08B5CC}']
  end;

  [JavaSignature('gedi/b0')]
  Jb0 = interface(JIInterface)
    ['{08688AD5-04AB-4C31-84C6-0213E8BE9463}']
    function a(string_1: JString; int: Integer): Integer; overload; cdecl;
    function a(string_1: JString; int: Integer; int_1: Integer; bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function a(string_1: JString; bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    procedure a(string_1: JString); overload; cdecl;
    function a(int: Integer; int_1: Integer; int_2: Integer; bytes: TJavaArray<Byte>; bytes_1: TJavaArray<Byte>; boolean: Boolean; int_3: Integer; bytes_2: TJavaArray<Byte>; string_1: JString; int_4: Integer): Integer; overload; cdecl;
    function a(int: Integer; strings: TJavaObjectArray<JString>): Integer; overload; cdecl;
    function b(string_1: JString): Integer; overload; cdecl;
    function b(string_1: JString; int: Integer; bytes: TJavaArray<Byte>): Integer; overload; cdecl;
    function r(int: Integer): Integer; cdecl;
    function z: Integer; cdecl;
  end;
  TJb0 = class(TJavaGenericImport<Jb0Class, Jb0>) end;

  JGEDI_CL_e_ISO_LevelClass = interface(JEnumClass)
    ['{9F064F25-1404-429A-BDCE-E5E346191D30}']
    {class} function _GetLEVEL_14443_3: JGEDI_CL_e_ISO_Level; cdecl;
    {class} function _GetLEVEL_14443_4: JGEDI_CL_e_ISO_Level; cdecl;
    {class} function _GetLEVEL_INACTIVE: JGEDI_CL_e_ISO_Level; cdecl;
    {class} function valueOf(string_1: JString): JGEDI_CL_e_ISO_Level; cdecl;
    {class} function values: TJavaObjectArray<JGEDI_CL_e_ISO_Level>; cdecl;
    {class} property LEVEL_14443_3: JGEDI_CL_e_ISO_Level read _GetLEVEL_14443_3;
    {class} property LEVEL_14443_4: JGEDI_CL_e_ISO_Level read _GetLEVEL_14443_4;
    {class} property LEVEL_INACTIVE: JGEDI_CL_e_ISO_Level read _GetLEVEL_INACTIVE;
  end;

  [JavaSignature('br/com/gertec/gedi/enums/GEDI_CL_e_ISO_Level')]
  JGEDI_CL_e_ISO_Level = interface(JEnum)
    ['{15DC3C28-EF07-4055-A6EF-7F3DCFB6D6DB}']
    function getValue: Integer; cdecl;
  end;
  TJGEDI_CL_e_ISO_Level = class(TJavaGenericImport<JGEDI_CL_e_ISO_LevelClass, JGEDI_CL_e_ISO_Level>) end;

  JGediNativeClass = interface(JObjectClass)
    ['{4FF3F11D-59F6-4B3B-83C2-832449EF43A5}']
    {class} function getInstance(e: Je): JGediNative; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/GediNative')]
  JGediNative = interface(JObject)
    ['{CCD50990-4087-4CA5-8BEB-D689F2C5F494}']
    function VersionGet(ints: TJavaArray<Integer>; ints_1: TJavaArray<Integer>; ints_2: TJavaArray<Integer>; ints_3: TJavaArray<Integer>): Integer; cdecl;
    function init(object_1: JObject): Integer; cdecl;
    procedure onKeyPress(int: Integer); overload; cdecl;
    procedure onKeyPress(gEDI_KBD_e_Key: JGEDI_KBD_e_Key); overload; cdecl;
    procedure setPrivateDir(string_1: JString); cdecl;
    function testCancel: Boolean; cdecl;
  end;
  TJGediNative = class(TJavaGenericImport<JGediNativeClass, JGediNative>) end;

  JICLOCKClass = interface(IJavaClass)
    ['{28B98838-A1A7-4124-AFEC-50B8C3D5AE6E}']
  end;

  [JavaSignature('br/com/gertec/gedi/interfaces/ICLOCK')]
  JICLOCK = interface(IJavaInstance)
    ['{A2177657-A0F6-4858-835F-F6F1109F75BF}']
    function RTCFGet: JGEDI_CLOCK_st_RTC; cdecl;
    function RTCGet: JGEDI_CLOCK_st_RTC; cdecl;
    procedure RTCSet(gEDI_CLOCK_st_RTC: JGEDI_CLOCK_st_RTC); cdecl;
    function ScheduledRebootGet: JString; cdecl;
    procedure ScheduledRebootSet(string_1: JString); cdecl;
    function TimeZoneSet(timeZone: JTimeZone): Boolean; cdecl;
  end;
  TJICLOCK = class(TJavaGenericImport<JICLOCKClass, JICLOCK>) end;

  JIEnumsClass = interface(IJavaClass)
    ['{C5D3F378-E0B4-4EB9-8E26-564E43939F23}']
  end;

  [JavaSignature('br/com/gertec/gedi/interfaces/IEnums')]
  JIEnums = interface(IJavaInstance)
    ['{821853D5-59C9-4A4D-BB7F-81F01ADEE6B1}']
    function getValue: Integer; cdecl;
  end;
  TJIEnums = class(TJavaGenericImport<JIEnumsClass, JIEnums>) end;

  Jgedi_aClass = interface(JObjectClass)
    ['{AE30CCA8-7CFE-4CEE-BB28-703738CB2757}']
    {class} function init: Jgedi_a; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/a')]
  Jgedi_a = interface(JObject)
    ['{5979201C-DAEB-4705-8454-6C6B07F5EA3A}']
    procedure Beep; cdecl;
  end;
  TJgedi_a = class(TJavaGenericImport<Jgedi_aClass, Jgedi_a>) end;

  JGEDI_KMS_st_ControlClass = interface(JObjectClass)
    ['{0F126DAE-DB98-4BF1-BAC5-56DBD282A075}']
    {class} function init: JGEDI_KMS_st_Control; overload; cdecl;
    {class} function init(byte: Byte; int_1: Integer; int: Integer; boolean: Boolean; boolean_1: Boolean; boolean_2: Boolean; int_2: Integer; int_3: Integer; callbacks: JGEDI_KMS_st_Control_Callbacks): JGEDI_KMS_st_Control; overload; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_KMS_st_Control')]
  JGEDI_KMS_st_Control = interface(JObject)
    ['{B25EF934-D32F-4BE0-85B0-0E55F1039421}']
    function _GetacClearPIN: JString; cdecl;
    function _GetbVersion: Byte; cdecl;
    function _GetboAutoEnd: Boolean; cdecl;
    function _GetboClearSingle: Boolean; cdecl;
    function _GetboNullPIN: Boolean; cdecl;
    function _Getcb: JGEDI_KMS_st_Control_Callbacks; cdecl;
    function _GetuiEventTimeout: Integer; cdecl;
    function _GetuiGlobalTimeout: Integer; cdecl;
    function _GetuiMaxPINLen: Integer; cdecl;
    function _GetuiMinPINLen: Integer; cdecl;
    property acClearPIN: JString read _GetacClearPIN;
    property bVersion: Byte read _GetbVersion;
    property boAutoEnd: Boolean read _GetboAutoEnd;
    property boClearSingle: Boolean read _GetboClearSingle;
    property boNullPIN: Boolean read _GetboNullPIN;
    property cb: JGEDI_KMS_st_Control_Callbacks read _Getcb;
    property uiEventTimeout: Integer read _GetuiEventTimeout;
    property uiGlobalTimeout: Integer read _GetuiGlobalTimeout;
    property uiMaxPINLen: Integer read _GetuiMaxPINLen;
    property uiMinPINLen: Integer read _GetuiMinPINLen;
  end;
  TJGEDI_KMS_st_Control = class(TJavaGenericImport<JGEDI_KMS_st_ControlClass, JGEDI_KMS_st_Control>) end;

  Jgedi_cClass1 = interface(Jgedi_cClass)
    ['{78542AFC-C170-4018-80C4-3205E75E1080}']
  end;

  [JavaSignature('gedi/c')]
  Jgedi_c1 = interface(Jgedi_c)
    ['{3183C39C-13E0-4036-AB44-15D2D5499275}']
    function RTCFGet: JGEDI_CLOCK_st_RTC; cdecl;
    function ScheduledRebootGet: JString; cdecl;
    procedure ScheduledRebootSet(string_1: JString); cdecl;
    function TimeZoneSet(timeZone: JTimeZone): Boolean; cdecl;
  end;
  TJgedi_c1 = class(TJavaGenericImport<Jgedi_cClass1, Jgedi_c1>) end;

  JGEDI_FS_e_StorageClass = interface(JEnumClass)
    ['{E41DBA8D-2A78-40A5-9BDC-24BB07256EAF}']
    {class} function _GetPRIVATE: JGEDI_FS_e_Storage; cdecl;
    {class} function _GetPUBLIC: JGEDI_FS_e_Storage; cdecl;
    {class} function _GetSD_CARD: JGEDI_FS_e_Storage; cdecl;
    {class} function _GetUSB_DISK: JGEDI_FS_e_Storage; cdecl;
    {class} function valueOf(string_1: JString): JGEDI_FS_e_Storage; cdecl;
    {class} function values: TJavaObjectArray<JGEDI_FS_e_Storage>; cdecl;
    {class} property &PRIVATE: JGEDI_FS_e_Storage read _GetPRIVATE;
    {class} property &PUBLIC: JGEDI_FS_e_Storage read _GetPUBLIC;
    {class} property SD_CARD: JGEDI_FS_e_Storage read _GetSD_CARD;
    {class} property USB_DISK: JGEDI_FS_e_Storage read _GetUSB_DISK;
  end;

  [JavaSignature('br/com/gertec/gedi/enums/GEDI_FS_e_Storage')]
  JGEDI_FS_e_Storage = interface(JEnum)
    ['{2292EF00-67EF-4775-82A1-BA9300562504}']
    function getValue: Integer; cdecl;
  end;
  TJGEDI_FS_e_Storage = class(TJavaGenericImport<JGEDI_FS_e_StorageClass, JGEDI_FS_e_Storage>) end;

  JGEDI_KMS_st_CapturePINBlockInfoClass = interface(JObjectClass)
    ['{C6854C40-EC8E-4BAA-B3D6-AECE30302377}']
    {class} function init(gEDI_KMS_st_Control: JGEDI_KMS_st_Control; gEDI_KMS_st_Data: JGEDI_KMS_st_Data; list: JList): JGEDI_KMS_st_CapturePINBlockInfo; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_KMS_st_CapturePINBlockInfo')]
  JGEDI_KMS_st_CapturePINBlockInfo = interface(JObject)
    ['{ADEBF4F7-4E9F-4A10-8DA4-208AF12B8566}']
    function _GetastPB: JList; cdecl;
    function _GetpstControl: JGEDI_KMS_st_Control; cdecl;
    function _GetpstData: JGEDI_KMS_st_Data; cdecl;
    property astPB: JList read _GetastPB;
    property pstControl: JGEDI_KMS_st_Control read _GetpstControl;
    property pstData: JGEDI_KMS_st_Data read _GetpstData;
  end;
  TJGEDI_KMS_st_CapturePINBlockInfo = class(TJavaGenericImport<JGEDI_KMS_st_CapturePINBlockInfoClass, JGEDI_KMS_st_CapturePINBlockInfo>) end;

  JGEDI_KMS_e_KEYTYPEClass = interface(JEnumClass)
    ['{BD3BEB07-FB29-4BBB-96DF-91B27AFDC279}']
    {class} function _GetAES: JGEDI_KMS_e_KEYTYPE; cdecl;
    {class} function _GetDES: JGEDI_KMS_e_KEYTYPE; cdecl;
    {class} function _GetDUKPT_DES: JGEDI_KMS_e_KEYTYPE; cdecl;
    {class} function _GetDUKPT_TDES: JGEDI_KMS_e_KEYTYPE; cdecl;
    {class} function _GetRSA: JGEDI_KMS_e_KEYTYPE; cdecl;
    {class} function _GetTDES: JGEDI_KMS_e_KEYTYPE; cdecl;
    {class} function valueOf(int: Integer): JGEDI_KMS_e_KEYTYPE; overload; cdecl;
    {class} function valueOf(string_1: JString): JGEDI_KMS_e_KEYTYPE; overload; cdecl;
    {class} function values: TJavaObjectArray<JGEDI_KMS_e_KEYTYPE>; cdecl;
    {class} property AES: JGEDI_KMS_e_KEYTYPE read _GetAES;
    {class} property DES: JGEDI_KMS_e_KEYTYPE read _GetDES;
    {class} property DUKPT_DES: JGEDI_KMS_e_KEYTYPE read _GetDUKPT_DES;
    {class} property DUKPT_TDES: JGEDI_KMS_e_KEYTYPE read _GetDUKPT_TDES;
    {class} property RSA: JGEDI_KMS_e_KEYTYPE read _GetRSA;
    {class} property TDES: JGEDI_KMS_e_KEYTYPE read _GetTDES;
  end;

  [JavaSignature('br/com/gertec/gedi/enums/GEDI_KMS_e_KEYTYPE')]
  JGEDI_KMS_e_KEYTYPE = interface(JEnum)
    ['{54FEECBC-DBAD-4656-8FC6-29B965940CD1}']
    function getValue: Integer; cdecl;
  end;
  TJGEDI_KMS_e_KEYTYPE = class(TJavaGenericImport<JGEDI_KMS_e_KEYTYPEClass, JGEDI_KMS_e_KEYTYPE>) end;

  JGEDI_KMS_st_Control_CallbacksClass = interface(IJavaClass)
    ['{C91CDABA-DBD4-4D91-8758-85CB76E58695}']
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_KMS_st_Control$Callbacks')]
  JGEDI_KMS_st_Control_Callbacks = interface(IJavaInstance)
    ['{A7A3D58D-4A03-40DE-95EC-683470F3C94C}']
    procedure onKeyPress(gEDI_KBD_e_Key: JGEDI_KBD_e_Key); cdecl;
    function testCancel: Boolean; cdecl;
  end;
  TJGEDI_KMS_st_Control_Callbacks = class(TJavaGenericImport<JGEDI_KMS_st_Control_CallbacksClass, JGEDI_KMS_st_Control_Callbacks>) end;

  JGEDI_CL_st_MF_ActivateInfoClass = interface(JObjectClass)
    ['{6D8D95E3-F7A9-42CA-9A6B-C789FFF659ED}']
    {class} function init: JGEDI_CL_st_MF_ActivateInfo; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_CL_st_MF_ActivateInfo')]
  JGEDI_CL_st_MF_ActivateInfo = interface(JObject)
    ['{37CF6736-9BF2-4731-BBC9-56C6B30B10DD}']
    function _GetabUID: TJavaArray<Byte>; cdecl;
    function _Gettype: JGEDI_CL_e_MF_Type; cdecl;
    property abUID: TJavaArray<Byte> read _GetabUID;
    property &type: JGEDI_CL_e_MF_Type read _Gettype;
  end;
  TJGEDI_CL_st_MF_ActivateInfo = class(TJavaGenericImport<JGEDI_CL_st_MF_ActivateInfoClass, JGEDI_CL_st_MF_ActivateInfo>) end;

  JIKMSClass = interface(IJavaClass)
    ['{1EC30FD4-C370-4348-8171-457DAC0DC44C}']
  end;

  [JavaSignature('br/com/gertec/gedi/interfaces/IKMS')]
  JIKMS = interface(IJavaInstance)
    ['{F6B8017C-1063-4BD7-8A18-5C48C73D8BC7}']
    function CapturePINBlock(gEDI_KMS_st_Control: JGEDI_KMS_st_Control; boolean: Boolean; gEDI_KMS_st_Data_1: JGEDI_KMS_st_Data; list: JList): JGEDI_KMS_st_CapturePINBlockInfo; cdecl;
    function DUKPTKSNGet(gEDI_KMS_e_KEYTYPE: JGEDI_KMS_e_KEYTYPE; gEDI_KMS_e_KEYPURPOSE: JGEDI_KMS_e_KEYPURPOSE; int: Integer): TJavaArray<Byte>; cdecl;
    function EncryptData(gEDI_KMS_st_Data: JGEDI_KMS_st_Data): JGEDI_KMS_st_Data; cdecl;
    function KCVGet(gEDI_KMS_e_KEYTYPE: JGEDI_KMS_e_KEYTYPE; gEDI_KMS_e_KEYPURPOSE: JGEDI_KMS_e_KEYPURPOSE; int: Integer): TJavaArray<Byte>; cdecl;
    function KeyMapGet: JList; cdecl;
    function KeyPresenceTest(gEDI_KMS_e_KEYTYPE: JGEDI_KMS_e_KEYTYPE; gEDI_KMS_e_KEYPURPOSE: JGEDI_KMS_e_KEYPURPOSE; int: Integer): Boolean; cdecl;
  end;
  TJIKMS = class(TJavaGenericImport<JIKMSClass, JIKMS>) end;

  JiClass = interface(JObjectClass)
    ['{81FCE692-AB74-4C53-8048-96F26A43E761}']
    {class} function init: Ji; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/i')]
  Ji = interface(JObject)
    ['{7FFC3584-E1D8-4238-A246-0FDE60657257}']
    procedure &Set(gEDI_LED_e_Id: JGEDI_LED_e_Id; boolean: Boolean); cdecl;
  end;
  TJi = class(TJavaGenericImport<JiClass, Ji>) end;

  JGEDI_INFO_e_ControlNumberClass = interface(JEnumClass)
    ['{E0102634-A697-40AC-AE4A-95D32EF1BDC6}']
    {class} function _GetCHASSIS: JGEDI_INFO_e_ControlNumber; cdecl;
    {class} function _GetSEAL: JGEDI_INFO_e_ControlNumber; cdecl;
    {class} function _GetSN: JGEDI_INFO_e_ControlNumber; cdecl;
    {class} function valueOf(string_1: JString): JGEDI_INFO_e_ControlNumber; overload; cdecl;
    {class} function valueOf(int: Integer): JGEDI_INFO_e_ControlNumber; overload; cdecl;
    {class} function values: TJavaObjectArray<JGEDI_INFO_e_ControlNumber>; cdecl;
    {class} property CHASSIS: JGEDI_INFO_e_ControlNumber read _GetCHASSIS;
    {class} property SEAL: JGEDI_INFO_e_ControlNumber read _GetSEAL;
    {class} property SN: JGEDI_INFO_e_ControlNumber read _GetSN;
  end;

  [JavaSignature('br/com/gertec/gedi/enums/GEDI_INFO_e_ControlNumber')]
  JGEDI_INFO_e_ControlNumber = interface(JEnum)
    ['{AD751A45-968C-41F2-A92D-24ABCDB2F407}']
    function getValue: Integer; cdecl;
  end;
  TJGEDI_INFO_e_ControlNumber = class(TJavaGenericImport<JGEDI_INFO_e_ControlNumberClass, JGEDI_INFO_e_ControlNumber>) end;

  Jlibbasebinder_dClass = interface(JObjectClass)
    ['{BDEF5F9B-69DF-4896-9A1D-99C1C2B5A053}']
    {class} procedure a(context: JContext); cdecl;
  end;

  [JavaSignature('wangpos/sdk4/libbasebinder/d')]
  Jlibbasebinder_d = interface(JObject)
    ['{D6165AB3-82F8-4DCE-B0D0-5B7D224F34CE}']
  end;
  TJlibbasebinder_d = class(TJavaGenericImport<Jlibbasebinder_dClass, Jlibbasebinder_d>) end;

  JIPRNTRClass = interface(IJavaClass)
    ['{8FB13D28-5EC3-402F-BEDB-D0C0AD32E3C2}']
  end;

  [JavaSignature('br/com/gertec/gedi/interfaces/IPRNTR')]
  JIPRNTR = interface(IJavaInstance)
    ['{3E583C00-B990-4841-BF04-9FDA9E1B2B01}']
    procedure DrawBarCode(gEDI_PRNTR_st_BarCodeConfig: JGEDI_PRNTR_st_BarCodeConfig; string_1: JString); cdecl;
    procedure DrawBlankLine(int: Integer); cdecl;
    procedure DrawPictureExt(gEDI_PRNTR_st_PictureConfig: JGEDI_PRNTR_st_PictureConfig; bitmap: JBitmap); cdecl;
    procedure DrawStringExt(gEDI_PRNTR_st_StringConfig: JGEDI_PRNTR_st_StringConfig; string_1: JString); cdecl;
    function GetPaperUsage: Integer; cdecl;
    procedure Init; cdecl;
    procedure Output; cdecl;
    procedure ResetPaperUsage; cdecl;
    procedure SetPrintDensity(gEDI_PRNTR_e_PrintDensity: JGEDI_PRNTR_e_PrintDensity); cdecl;
    function Status: JGEDI_PRNTR_e_Status; cdecl;
  end;
  TJIPRNTR = class(TJavaGenericImport<JIPRNTRClass, JIPRNTR>) end;

  JGEDI_INFO_e_TestClass = interface(JEnumClass)
    ['{62842DE7-10E3-4D99-809F-1A092A240068}']
    {class} function _GetBACKLIGHT_EXISTS: JGEDI_INFO_e_Test; cdecl;
    {class} function _GetBATTERY_SUPPORT: JGEDI_INFO_e_Test; cdecl;
    {class} function _GetBLUETOOTH_EXISTS: JGEDI_INFO_e_Test; cdecl;
    {class} function _GetBUZZER_EXISTS: JGEDI_INFO_e_Test; cdecl;
    {class} function _GetCOLOR_DISPLAY: JGEDI_INFO_e_Test; cdecl;
    {class} function _GetCONTACTLESS_EXISTS: JGEDI_INFO_e_Test; cdecl;
    {class} function _GetCONTACTLESS_LED_EXISTS: JGEDI_INFO_e_Test; cdecl;
    {class} function _GetCRADLE_ATTACHED: JGEDI_INFO_e_Test; cdecl;
    {class} function _GetDIAL_EXISTS: JGEDI_INFO_e_Test; cdecl;
    {class} function _GetETHERNET_EXISTS: JGEDI_INFO_e_Test; cdecl;
    {class} function _GetGSM_EXISTS: JGEDI_INFO_e_Test; cdecl;
    {class} function _GetIACP_SUPPORT: JGEDI_INFO_e_Test; cdecl;
    {class} function _GetKBD_EXISTS: JGEDI_INFO_e_Test; cdecl;
    {class} function _GetLCD_EXISTS: JGEDI_INFO_e_Test; cdecl;
    {class} function _GetMSR_EXISTS: JGEDI_INFO_e_Test; cdecl;
    {class} function _GetPCM_SUPPORT: JGEDI_INFO_e_Test; cdecl;
    {class} function _GetPRINTER_EXISTS: JGEDI_INFO_e_Test; cdecl;
    {class} function _GetRS232_EXISTS: JGEDI_INFO_e_Test; cdecl;
    {class} function _GetSMART_EXISTS: JGEDI_INFO_e_Test; cdecl;
    {class} function _GetSRED_REQUIRED: JGEDI_INFO_e_Test; cdecl;
    {class} function _GetTTF_SUPPORT: JGEDI_INFO_e_Test; cdecl;
    {class} function _GetUSB_EXISTS: JGEDI_INFO_e_Test; cdecl;
    {class} function _GetWIFI_EXISTS: JGEDI_INFO_e_Test; cdecl;
    {class} function valueOf(string_1: JString): JGEDI_INFO_e_Test; cdecl;
    {class} function values: TJavaObjectArray<JGEDI_INFO_e_Test>; cdecl;
    {class} property BACKLIGHT_EXISTS: JGEDI_INFO_e_Test read _GetBACKLIGHT_EXISTS;
    {class} property BATTERY_SUPPORT: JGEDI_INFO_e_Test read _GetBATTERY_SUPPORT;
    {class} property BLUETOOTH_EXISTS: JGEDI_INFO_e_Test read _GetBLUETOOTH_EXISTS;
    {class} property BUZZER_EXISTS: JGEDI_INFO_e_Test read _GetBUZZER_EXISTS;
    {class} property COLOR_DISPLAY: JGEDI_INFO_e_Test read _GetCOLOR_DISPLAY;
    {class} property CONTACTLESS_EXISTS: JGEDI_INFO_e_Test read _GetCONTACTLESS_EXISTS;
    {class} property CONTACTLESS_LED_EXISTS: JGEDI_INFO_e_Test read _GetCONTACTLESS_LED_EXISTS;
    {class} property CRADLE_ATTACHED: JGEDI_INFO_e_Test read _GetCRADLE_ATTACHED;
    {class} property DIAL_EXISTS: JGEDI_INFO_e_Test read _GetDIAL_EXISTS;
    {class} property ETHERNET_EXISTS: JGEDI_INFO_e_Test read _GetETHERNET_EXISTS;
    {class} property GSM_EXISTS: JGEDI_INFO_e_Test read _GetGSM_EXISTS;
    {class} property IACP_SUPPORT: JGEDI_INFO_e_Test read _GetIACP_SUPPORT;
    {class} property KBD_EXISTS: JGEDI_INFO_e_Test read _GetKBD_EXISTS;
    {class} property LCD_EXISTS: JGEDI_INFO_e_Test read _GetLCD_EXISTS;
    {class} property MSR_EXISTS: JGEDI_INFO_e_Test read _GetMSR_EXISTS;
    {class} property PCM_SUPPORT: JGEDI_INFO_e_Test read _GetPCM_SUPPORT;
    {class} property PRINTER_EXISTS: JGEDI_INFO_e_Test read _GetPRINTER_EXISTS;
    {class} property RS232_EXISTS: JGEDI_INFO_e_Test read _GetRS232_EXISTS;
    {class} property SMART_EXISTS: JGEDI_INFO_e_Test read _GetSMART_EXISTS;
    {class} property SRED_REQUIRED: JGEDI_INFO_e_Test read _GetSRED_REQUIRED;
    {class} property TTF_SUPPORT: JGEDI_INFO_e_Test read _GetTTF_SUPPORT;
    {class} property USB_EXISTS: JGEDI_INFO_e_Test read _GetUSB_EXISTS;
    {class} property WIFI_EXISTS: JGEDI_INFO_e_Test read _GetWIFI_EXISTS;
  end;

  [JavaSignature('br/com/gertec/gedi/enums/GEDI_INFO_e_Test')]
  JGEDI_INFO_e_Test = interface(JEnum)
    ['{6F2BCD45-8469-48E4-BD05-14C6EE47BBDF}']
    function getValue: Integer; cdecl;
  end;
  TJGEDI_INFO_e_Test = class(TJavaGenericImport<JGEDI_INFO_e_TestClass, JGEDI_INFO_e_Test>) end;

  Jgedi_bClass1 = interface(Jgedi_bClass)
    ['{78E7F6CD-9F88-4BB8-9956-E5EBF48CEDDE}']
  end;

  [JavaSignature('gedi/b')]
  Jgedi_b1 = interface(Jgedi_b)
    ['{14B65803-0D02-4FC5-B9B1-94965160A02B}']
    function ISO_Polling(int: Integer): JGEDI_CL_st_ISO_PollingInfo; cdecl;
    procedure MF_Authentication(int: Integer; gEDI_CL_st_MF_Key: JGEDI_CL_st_MF_Key; bytes: TJavaArray<Byte>); cdecl;
    function MF_BlockRead(int: Integer): TJavaArray<Byte>; cdecl;
    procedure MF_BlockWrite(int: Integer; bytes: TJavaArray<Byte>); cdecl;
    procedure MF_Decrement(int: Integer; int_1: Integer); cdecl;
    procedure MF_Increment(int: Integer; int_1: Integer); cdecl;
    procedure MF_Restore(int: Integer; int_1: Integer); cdecl;
    function MF_SignatureGet(int: Integer): TJavaArray<Byte>; cdecl;
    procedure MF_Transfer(int: Integer); cdecl;
    procedure PowerOff; cdecl;
    procedure PowerOn; cdecl;
    function ResetEMV: JGEDI_CL_st_ResetEMVInfo; cdecl;
    function SendAPDU(bytes: TJavaArray<Byte>): TJavaArray<Byte>; cdecl;
    function a(bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
  end;
  TJgedi_b1 = class(TJavaGenericImport<Jgedi_bClass1, Jgedi_b1>) end;

  JGEDI_SMART_e_TypeClass = interface(JEnumClass)
    ['{73920D43-465D-4516-9C54-316C92429BD0}']
    {class} function _GetI2C: JGEDI_SMART_e_Type; cdecl;
    {class} function _GetMEM: JGEDI_SMART_e_Type; cdecl;
    {class} function _GetT0: JGEDI_SMART_e_Type; cdecl;
    {class} function _GetT1: JGEDI_SMART_e_Type; cdecl;
    {class} function valueOf(string_1: JString): JGEDI_SMART_e_Type; overload; cdecl;
    {class} function valueOf(int: Integer): JGEDI_SMART_e_Type; overload; cdecl;
    {class} function values: TJavaObjectArray<JGEDI_SMART_e_Type>; cdecl;
    {class} property I2C: JGEDI_SMART_e_Type read _GetI2C;
    {class} property MEM: JGEDI_SMART_e_Type read _GetMEM;
    {class} property T0: JGEDI_SMART_e_Type read _GetT0;
    {class} property T1: JGEDI_SMART_e_Type read _GetT1;
  end;

  [JavaSignature('br/com/gertec/gedi/enums/GEDI_SMART_e_Type')]
  JGEDI_SMART_e_Type = interface(JEnum)
    ['{4CF3DC9E-6D42-448F-9099-BD8AA0580D8D}']
    function getValue: Integer; cdecl;
  end;
  TJGEDI_SMART_e_Type = class(TJavaGenericImport<JGEDI_SMART_e_TypeClass, JGEDI_SMART_e_Type>) end;

  JGediExceptionClass = interface(JExceptionClass)
    ['{137AB0E2-A318-4067-841C-6812EC81E976}']
    {class} function init(gEDI_e_Ret: JGEDI_e_Ret; string_1: JString): JGediException; overload; cdecl;
    {class} function init(gEDI_e_Ret: JGEDI_e_Ret; throwable: JThrowable): JGediException; overload; cdecl;
    {class} function init(gEDI_e_Ret: JGEDI_e_Ret; string_1: JString; throwable: JThrowable): JGediException; overload; cdecl;
    {class} function init(gEDI_e_Ret: JGEDI_e_Ret): JGediException; overload; cdecl;
    {class} function init(int: Integer): JGediException; overload; cdecl;
    {class} function init(int: Integer; string_1: JString): JGediException; overload; cdecl;
    {class} function init(int: Integer; throwable: JThrowable): JGediException; overload; cdecl;
    {class} function init(int: Integer; string_1: JString; throwable: JThrowable): JGediException; overload; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/exceptions/GediException')]
  JGediException = interface(JException)
    ['{96FF345D-F594-4F67-9C9A-488CE04FA39F}']
    function getErrorCode: JGEDI_e_Ret; cdecl;
    function toString: JString; cdecl;
  end;
  TJGediException = class(TJavaGenericImport<JGediExceptionClass, JGediException>) end;

  JGEDI_INFO_e_ModuleEngClass = interface(JEnumClass)
    ['{EA5929B6-9270-40FD-8784-3281DA38AD6B}']
    {class} function _GetAPI_SO: JGEDI_INFO_e_ModuleEng; cdecl;
    {class} function _GetBARCODE_SO: JGEDI_INFO_e_ModuleEng; cdecl;
    {class} function _GetBCAGSM_SO: JGEDI_INFO_e_ModuleEng; cdecl;
    {class} function _GetBIOS: JGEDI_INFO_e_ModuleEng; cdecl;
    {class} function _GetBOOTSULD: JGEDI_INFO_e_ModuleEng; cdecl;
    {class} function _GetCLVWM_MP: JGEDI_INFO_e_ModuleEng; cdecl;
    {class} function _GetCLVW_SO: JGEDI_INFO_e_ModuleEng; cdecl;
    {class} function _GetCL_KO: JGEDI_INFO_e_ModuleEng; cdecl;
    {class} function _GetCRADLE_MP: JGEDI_INFO_e_ModuleEng; cdecl;
    {class} function _GetCRYPTO_HAL: JGEDI_INFO_e_ModuleEng; cdecl;
    {class} function _GetDRV_KO: JGEDI_INFO_e_ModuleEng; cdecl;
    {class} function _GetEMVL2_SO: JGEDI_INFO_e_ModuleEng; cdecl;
    {class} function _GetETHERNET_SO: JGEDI_INFO_e_ModuleEng; cdecl;
    {class} function _GetFONT_SO: JGEDI_INFO_e_ModuleEng; cdecl;
    {class} function _GetFS_SO: JGEDI_INFO_e_ModuleEng; cdecl;
    {class} function _GetGEDI_SO: JGEDI_INFO_e_ModuleEng; cdecl;
    {class} function _GetKMS_KO: JGEDI_INFO_e_ModuleEng; cdecl;
    {class} function _GetKMS_SO: JGEDI_INFO_e_ModuleEng; cdecl;
    {class} function _GetLCD_SO: JGEDI_INFO_e_ModuleEng; cdecl;
    {class} function _GetLINUX_KERNEL: JGEDI_INFO_e_ModuleEng; cdecl;
    {class} function _GetMODEM_SO: JGEDI_INFO_e_ModuleEng; cdecl;
    {class} function _GetPMODEM_SO: JGEDI_INFO_e_ModuleEng; cdecl;
    {class} function _GetPRT_SO: JGEDI_INFO_e_ModuleEng; cdecl;
    {class} function _GetROOTFS: JGEDI_INFO_e_ModuleEng; cdecl;
    {class} function _GetRTC_SO: JGEDI_INFO_e_ModuleEng; cdecl;
    {class} function _GetSC_KO: JGEDI_INFO_e_ModuleEng; cdecl;
    {class} function _GetSECURITY_KO: JGEDI_INFO_e_ModuleEng; cdecl;
    {class} function _GetSYSUPD_KO: JGEDI_INFO_e_ModuleEng; cdecl;
    {class} function _GetTLS_SO: JGEDI_INFO_e_ModuleEng; cdecl;
    {class} function _GetUART_SO: JGEDI_INFO_e_ModuleEng; cdecl;
    {class} function _GetULDPM_SO: JGEDI_INFO_e_ModuleEng; cdecl;
    {class} function _GetUSBH_SO: JGEDI_INFO_e_ModuleEng; cdecl;
    {class} function _GetUSB_KO: JGEDI_INFO_e_ModuleEng; cdecl;
    {class} function valueOf(int: Integer): JGEDI_INFO_e_ModuleEng; overload; cdecl;
    {class} function valueOf(string_1: JString): JGEDI_INFO_e_ModuleEng; overload; cdecl;
    {class} function values: TJavaObjectArray<JGEDI_INFO_e_ModuleEng>; cdecl;
    {class} property API_SO: JGEDI_INFO_e_ModuleEng read _GetAPI_SO;
    {class} property BARCODE_SO: JGEDI_INFO_e_ModuleEng read _GetBARCODE_SO;
    {class} property BCAGSM_SO: JGEDI_INFO_e_ModuleEng read _GetBCAGSM_SO;
    {class} property BIOS: JGEDI_INFO_e_ModuleEng read _GetBIOS;
    {class} property BOOTSULD: JGEDI_INFO_e_ModuleEng read _GetBOOTSULD;
    {class} property CLVWM_MP: JGEDI_INFO_e_ModuleEng read _GetCLVWM_MP;
    {class} property CLVW_SO: JGEDI_INFO_e_ModuleEng read _GetCLVW_SO;
    {class} property CL_KO: JGEDI_INFO_e_ModuleEng read _GetCL_KO;
    {class} property CRADLE_MP: JGEDI_INFO_e_ModuleEng read _GetCRADLE_MP;
    {class} property CRYPTO_HAL: JGEDI_INFO_e_ModuleEng read _GetCRYPTO_HAL;
    {class} property DRV_KO: JGEDI_INFO_e_ModuleEng read _GetDRV_KO;
    {class} property EMVL2_SO: JGEDI_INFO_e_ModuleEng read _GetEMVL2_SO;
    {class} property ETHERNET_SO: JGEDI_INFO_e_ModuleEng read _GetETHERNET_SO;
    {class} property FONT_SO: JGEDI_INFO_e_ModuleEng read _GetFONT_SO;
    {class} property FS_SO: JGEDI_INFO_e_ModuleEng read _GetFS_SO;
    {class} property GEDI_SO: JGEDI_INFO_e_ModuleEng read _GetGEDI_SO;
    {class} property KMS_KO: JGEDI_INFO_e_ModuleEng read _GetKMS_KO;
    {class} property KMS_SO: JGEDI_INFO_e_ModuleEng read _GetKMS_SO;
    {class} property LCD_SO: JGEDI_INFO_e_ModuleEng read _GetLCD_SO;
    {class} property LINUX_KERNEL: JGEDI_INFO_e_ModuleEng read _GetLINUX_KERNEL;
    {class} property MODEM_SO: JGEDI_INFO_e_ModuleEng read _GetMODEM_SO;
    {class} property PMODEM_SO: JGEDI_INFO_e_ModuleEng read _GetPMODEM_SO;
    {class} property PRT_SO: JGEDI_INFO_e_ModuleEng read _GetPRT_SO;
    {class} property ROOTFS: JGEDI_INFO_e_ModuleEng read _GetROOTFS;
    {class} property RTC_SO: JGEDI_INFO_e_ModuleEng read _GetRTC_SO;
    {class} property SC_KO: JGEDI_INFO_e_ModuleEng read _GetSC_KO;
    {class} property SECURITY_KO: JGEDI_INFO_e_ModuleEng read _GetSECURITY_KO;
    {class} property SYSUPD_KO: JGEDI_INFO_e_ModuleEng read _GetSYSUPD_KO;
    {class} property TLS_SO: JGEDI_INFO_e_ModuleEng read _GetTLS_SO;
    {class} property UART_SO: JGEDI_INFO_e_ModuleEng read _GetUART_SO;
    {class} property ULDPM_SO: JGEDI_INFO_e_ModuleEng read _GetULDPM_SO;
    {class} property USBH_SO: JGEDI_INFO_e_ModuleEng read _GetUSBH_SO;
    {class} property USB_KO: JGEDI_INFO_e_ModuleEng read _GetUSB_KO;
  end;

  [JavaSignature('br/com/gertec/gedi/enums/GEDI_INFO_e_ModuleEng')]
  JGEDI_INFO_e_ModuleEng = interface(JEnum)
    ['{E109E85D-A365-49F7-95C0-7716811C2F43}']
    function getValue: Integer; cdecl;
  end;
  TJGEDI_INFO_e_ModuleEng = class(TJavaGenericImport<JGEDI_INFO_e_ModuleEngClass, JGEDI_INFO_e_ModuleEng>) end;

  JCoreClass = interface(Jlibbasebinder_aClass)
    ['{96415E6B-0DC9-465C-AF30-B95795C34DA7}']
    {class} function _GetALGORITHM_3DES: Integer; cdecl;
    {class} function _GetALGORITHM_AES: Integer; cdecl;
    {class} function _GetALGORITHM_DES: Integer; cdecl;
    {class} function _GetCALLBACK_ADVICE: Integer; cdecl;
    {class} function _GetCALLBACK_AMOUNT: Integer; cdecl;
    {class} function _GetCALLBACK_APPREF: Integer; cdecl;
    {class} function _GetCALLBACK_NOTIFY: Integer; cdecl;
    {class} function _GetCALLBACK_ONLINE: Integer; cdecl;
    {class} function _GetCALLBACK_PIN: Integer; cdecl;
    {class} function _GetCALLBACK_PINN: Integer; cdecl;
    {class} function _GetCALLBACK_PINPAD_ACCESSIBILITY: Integer; cdecl;
    {class} function _GetCALLBACK_PINRESULT: Integer; cdecl;
    {class} function _GetCALLBACK_UNKNOWNTLV: Integer; cdecl;
    {class} function _GetCALLBACK_VERIFYUSERIDCARD: Integer; cdecl;
    {class} function _GetDECRYPT_MODE: Integer; cdecl;
    {class} function _GetENCRYPT_MODE: Integer; cdecl;
    {class} function _GetENCRYPT_MODE_CBC: Integer; cdecl;
    {class} function _GetENCRYPT_MODE_ECB: Integer; cdecl;
    {class} function _GetEnDecrypt_3DES: Byte; cdecl;
    {class} function _GetEnDecrypt_AES: Byte; cdecl;
    {class} function _GetEnDecrypt_DES: Byte; cdecl;
    {class} function _GetEnDecrypt_SM4: Byte; cdecl;
    {class} function _GetGETMAC_3DES: Byte; cdecl;
    {class} function _GetGETMAC_AES: Byte; cdecl;
    {class} function _GetGETMAC_DES: Byte; cdecl;
    {class} function _GetGETMAC_SM4: Byte; cdecl;
    {class} function _GetIM_KEY_3DES: Byte; cdecl;
    {class} function _GetIM_KEY_AES: Byte; cdecl;
    {class} function _GetIM_KEY_DES: Byte; cdecl;
    {class} function _GetIM_KEY_SM4: Byte; cdecl;
    {class} function _GetKEY_PROTECT_TLK: Byte; cdecl;
    {class} function _GetKEY_PROTECT_TMK: Byte; cdecl;
    {class} function _GetKEY_PROTECT_ZERO: Byte; cdecl;
    {class} function _GetKEY_REQUEST_DDEK: Byte; cdecl;
    {class} function _GetKEY_REQUEST_DEK: Byte; cdecl;
    {class} function _GetKEY_REQUEST_DMAK: Byte; cdecl;
    {class} function _GetKEY_REQUEST_IPEK: Byte; cdecl;
    {class} function _GetKEY_REQUEST_KBPK: Byte; cdecl;
    {class} function _GetKEY_REQUEST_MAK: Byte; cdecl;
    {class} function _GetKEY_REQUEST_PEK: Byte; cdecl;
    {class} function _GetKEY_REQUEST_TLK: Byte; cdecl;
    {class} function _GetKEY_REQUEST_TMK: Byte; cdecl;
    {class} function _GetPDD_MODE_2: Integer; cdecl;
    {class} function _GetPDD_MODE_9797: Integer; cdecl;
    {class} function _GetPDD_MODE_NONE: Integer; cdecl;
    {class} function _GetPIN_CMD_PREPARE: Byte; cdecl;
    {class} function _GetPIN_CMD_QUIT: Byte; cdecl;
    {class} function _GetPIN_CMD_UPDATE: Byte; cdecl;
    {class} function _GetPIN_PREPARE_APAsswordNew: Byte; cdecl;
    {class} function _GetPIN_PREPARE_APassword: Byte; cdecl;
    {class} function _GetPIN_PREPARE_BPAssword: Byte; cdecl;
    {class} function _GetPIN_PREPARE_BPAsswordNew: Byte; cdecl;
    {class} function _GetPIN_PREPARE_OFFLINE: Byte; cdecl;
    {class} function _GetPIN_PREPARE_ONLINE: Byte; cdecl;
    {class} function _GetPIN_QUIT_BYPASS: Byte; cdecl;
    {class} function _GetPIN_QUIT_CANCEL: Byte; cdecl;
    {class} function _GetPIN_QUIT_ERROR: Byte; cdecl;
    {class} function _GetPIN_QUIT_ERRORPAN: Byte; cdecl;
    {class} function _GetPIN_QUIT_NOUPLOAD: Byte; cdecl;
    {class} function _GetPIN_QUIT_PAINUPLOAD: Byte; cdecl;
    {class} function _GetPIN_QUIT_PINBLOCKUPLOAD: Byte; cdecl;
    {class} function _GetPIN_QUIT_REMOVE_CARD: Byte; cdecl;
    {class} function _GetPIN_QUIT_SUCCESS: Byte; cdecl;
    {class} function _GetPIN_QUIT_TIMEOUT: Byte; cdecl;
    {class} function _GetPMK_UPADATE_KEYTYPE: Byte; cdecl;
    {class} function _GetPMK_UPDATE_KEYINDEX: Byte; cdecl;
    {class} function _GetPMK_UPDATE_KEYLEN: Byte; cdecl;
    {class} function bytesToHex(bytes: TJavaArray<Byte>): JString; cdecl;
    {class} function getButtonPosition(button: JButton; bytes: TJavaArray<Byte>; int: Integer): Integer; cdecl;
    {class} function getButtonPositionInAccessibilityMode(button: JButton; bytes: TJavaArray<Byte>; context: JContext; int: Integer; int_1: Integer; int_2: Integer; int_3: Integer): Integer; cdecl;
    {class} function getButtonPosition_HS(button: JButton; bytes: TJavaArray<Byte>; context: JContext; int: Integer): Integer; overload; cdecl;
    {class} function getButtonPosition_HS(view: JView; bytes: TJavaArray<Byte>; context: JContext; int: Integer): Integer; overload; cdecl;
    {class} function init(context: JContext): JCore; overload; cdecl;
    {class} function init(context: JContext; boolean: Boolean): JCore; overload; cdecl;
    {class} function shortToByte(short: SmallInt): TJavaArray<Byte>; cdecl;
    {class} property ALGORITHM_3DES: Integer read _GetALGORITHM_3DES;
    {class} property ALGORITHM_AES: Integer read _GetALGORITHM_AES;
    {class} property ALGORITHM_DES: Integer read _GetALGORITHM_DES;
    {class} property CALLBACK_ADVICE: Integer read _GetCALLBACK_ADVICE;
    {class} property CALLBACK_AMOUNT: Integer read _GetCALLBACK_AMOUNT;
    {class} property CALLBACK_APPREF: Integer read _GetCALLBACK_APPREF;
    {class} property CALLBACK_NOTIFY: Integer read _GetCALLBACK_NOTIFY;
    {class} property CALLBACK_ONLINE: Integer read _GetCALLBACK_ONLINE;
    {class} property CALLBACK_PIN: Integer read _GetCALLBACK_PIN;
    {class} property CALLBACK_PINN: Integer read _GetCALLBACK_PINN;
    {class} property CALLBACK_PINPAD_ACCESSIBILITY: Integer read _GetCALLBACK_PINPAD_ACCESSIBILITY;
    {class} property CALLBACK_PINRESULT: Integer read _GetCALLBACK_PINRESULT;
    {class} property CALLBACK_UNKNOWNTLV: Integer read _GetCALLBACK_UNKNOWNTLV;
    {class} property CALLBACK_VERIFYUSERIDCARD: Integer read _GetCALLBACK_VERIFYUSERIDCARD;
    {class} property DECRYPT_MODE: Integer read _GetDECRYPT_MODE;
    {class} property ENCRYPT_MODE: Integer read _GetENCRYPT_MODE;
    {class} property ENCRYPT_MODE_CBC: Integer read _GetENCRYPT_MODE_CBC;
    {class} property ENCRYPT_MODE_ECB: Integer read _GetENCRYPT_MODE_ECB;
    {class} property EnDecrypt_3DES: Byte read _GetEnDecrypt_3DES;
    {class} property EnDecrypt_AES: Byte read _GetEnDecrypt_AES;
    {class} property EnDecrypt_DES: Byte read _GetEnDecrypt_DES;
    {class} property EnDecrypt_SM4: Byte read _GetEnDecrypt_SM4;
    {class} property GETMAC_3DES: Byte read _GetGETMAC_3DES;
    {class} property GETMAC_AES: Byte read _GetGETMAC_AES;
    {class} property GETMAC_DES: Byte read _GetGETMAC_DES;
    {class} property GETMAC_SM4: Byte read _GetGETMAC_SM4;
    {class} property IM_KEY_3DES: Byte read _GetIM_KEY_3DES;
    {class} property IM_KEY_AES: Byte read _GetIM_KEY_AES;
    {class} property IM_KEY_DES: Byte read _GetIM_KEY_DES;
    {class} property IM_KEY_SM4: Byte read _GetIM_KEY_SM4;
    {class} property KEY_PROTECT_TLK: Byte read _GetKEY_PROTECT_TLK;
    {class} property KEY_PROTECT_TMK: Byte read _GetKEY_PROTECT_TMK;
    {class} property KEY_PROTECT_ZERO: Byte read _GetKEY_PROTECT_ZERO;
    {class} property KEY_REQUEST_DDEK: Byte read _GetKEY_REQUEST_DDEK;
    {class} property KEY_REQUEST_DEK: Byte read _GetKEY_REQUEST_DEK;
    {class} property KEY_REQUEST_DMAK: Byte read _GetKEY_REQUEST_DMAK;
    {class} property KEY_REQUEST_IPEK: Byte read _GetKEY_REQUEST_IPEK;
    {class} property KEY_REQUEST_KBPK: Byte read _GetKEY_REQUEST_KBPK;
    {class} property KEY_REQUEST_MAK: Byte read _GetKEY_REQUEST_MAK;
    {class} property KEY_REQUEST_PEK: Byte read _GetKEY_REQUEST_PEK;
    {class} property KEY_REQUEST_TLK: Byte read _GetKEY_REQUEST_TLK;
    {class} property KEY_REQUEST_TMK: Byte read _GetKEY_REQUEST_TMK;
    {class} property PDD_MODE_2: Integer read _GetPDD_MODE_2;
    {class} property PDD_MODE_9797: Integer read _GetPDD_MODE_9797;
    {class} property PDD_MODE_NONE: Integer read _GetPDD_MODE_NONE;
    {class} property PIN_CMD_PREPARE: Byte read _GetPIN_CMD_PREPARE;
    {class} property PIN_CMD_QUIT: Byte read _GetPIN_CMD_QUIT;
    {class} property PIN_CMD_UPDATE: Byte read _GetPIN_CMD_UPDATE;
    {class} property PIN_PREPARE_APAsswordNew: Byte read _GetPIN_PREPARE_APAsswordNew;
    {class} property PIN_PREPARE_APassword: Byte read _GetPIN_PREPARE_APassword;
    {class} property PIN_PREPARE_BPAssword: Byte read _GetPIN_PREPARE_BPAssword;
    {class} property PIN_PREPARE_BPAsswordNew: Byte read _GetPIN_PREPARE_BPAsswordNew;
    {class} property PIN_PREPARE_OFFLINE: Byte read _GetPIN_PREPARE_OFFLINE;
    {class} property PIN_PREPARE_ONLINE: Byte read _GetPIN_PREPARE_ONLINE;
    {class} property PIN_QUIT_BYPASS: Byte read _GetPIN_QUIT_BYPASS;
    {class} property PIN_QUIT_CANCEL: Byte read _GetPIN_QUIT_CANCEL;
    {class} property PIN_QUIT_ERROR: Byte read _GetPIN_QUIT_ERROR;
    {class} property PIN_QUIT_ERRORPAN: Byte read _GetPIN_QUIT_ERRORPAN;
    {class} property PIN_QUIT_NOUPLOAD: Byte read _GetPIN_QUIT_NOUPLOAD;
    {class} property PIN_QUIT_PAINUPLOAD: Byte read _GetPIN_QUIT_PAINUPLOAD;
    {class} property PIN_QUIT_PINBLOCKUPLOAD: Byte read _GetPIN_QUIT_PINBLOCKUPLOAD;
    {class} property PIN_QUIT_REMOVE_CARD: Byte read _GetPIN_QUIT_REMOVE_CARD;
    {class} property PIN_QUIT_SUCCESS: Byte read _GetPIN_QUIT_SUCCESS;
    {class} property PIN_QUIT_TIMEOUT: Byte read _GetPIN_QUIT_TIMEOUT;
    {class} property PMK_UPADATE_KEYTYPE: Byte read _GetPMK_UPADATE_KEYTYPE;
    {class} property PMK_UPDATE_KEYINDEX: Byte read _GetPMK_UPDATE_KEYINDEX;
    {class} property PMK_UPDATE_KEYLEN: Byte read _GetPMK_UPDATE_KEYLEN;
  end;

  [JavaSignature('wangpos/sdk4/libbasebinder/Core')]
  JCore = interface(Jlibbasebinder_a)
    ['{4B246B2D-425A-4614-AB33-623E5A20CE5D}']
    function ControlLedFlash(int: Integer; int_1: Integer; int_2: Integer; int_3: Integer): Integer; cdecl;
    function EmvLib_GetPrintStatus(bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function GeneratePINPrepareData(c0: Jc0): Integer; overload; cdecl;
    function GetSPLog(int: Integer; int_1: Integer; ints: TJavaArray<Integer>; bytes: TJavaArray<Byte>): Integer; cdecl;
    function Get_KeySign(int: Integer; int_1: Integer; int_2: Integer; bytes: TJavaArray<Byte>; int_3: Integer; bytes_1: TJavaArray<Byte>; bytes_2: TJavaArray<Byte>): Integer; cdecl;
    function SDK_ReadData(bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function SDK_ReadData_unlock(bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function SDK_SendData(bytes: TJavaArray<Byte>; int: Integer): Integer; cdecl;
    function SDK_SendData_unlock(bytes: TJavaArray<Byte>; int: Integer): Integer; cdecl;
    function SerailDebugPort(int: Integer; int_1: Integer): Integer; cdecl;
    function SetKernel(int: Integer; int_1: Integer; int_2: Integer): Integer; cdecl;
    function buzzer: Integer; cdecl;
    function buzzerEx(int: Integer; int_1: Integer; int_2: Integer): Integer; overload; cdecl;
    function buzzerEx(int: Integer): Integer; overload; cdecl;
    function buzzerFrequencyEx(int: Integer; int_1: Integer; int_2: Integer): Integer; cdecl;
    function checkHardwareScannerSupported: Integer; cdecl;
    function checkSpeechServiceInstall: Integer; cdecl;
    function clearTamperStatus: Integer; cdecl;
    procedure config8BINsSupport(int: Integer; int_1: Integer; int_2: Integer); cdecl;
    function configEMVPINSetting(string_1: JString; int: Integer; int_1: Integer; int_2: Integer; int_3: Integer; boolean: Boolean; boolean_1: Boolean; boolean_2: Boolean; boolean_3: Boolean; boolean_4: Boolean; boolean_5: Boolean): Integer; cdecl;
    function dataEnDecrypt(bytes: TJavaArray<Byte>; int: Integer; bytes_1: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function dataEnDecryptEx(int: Integer; int_1: Integer; string_1: JString; int_2: Integer; int_3: Integer; bytes: TJavaArray<Byte>; int_4: Integer; bytes_1: TJavaArray<Byte>; int_5: Integer; bytes_2: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function dataEnDecryptExIndex(int: Integer; int_1: Integer; int_2: Integer; int_3: Integer; int_4: Integer; bytes: TJavaArray<Byte>; int_5: Integer; bytes_1: TJavaArray<Byte>; int_6: Integer; bytes_2: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function dataEnDecryptForAesDukpt(int: Integer; int_1: Integer; string_1: JString; int_2: Integer; int_3: Integer; bytes: TJavaArray<Byte>; int_4: Integer; bytes_1: TJavaArray<Byte>; int_5: Integer; bytes_2: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function dataEnDecryptForIPEK(int: Integer; int_1: Integer; string_1: JString; int_2: Integer; int_3: Integer; bytes: TJavaArray<Byte>; int_4: Integer; bytes_1: TJavaArray<Byte>; int_5: Integer; bytes_2: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function dataEnDecryptForIPEKEx(int: Integer; int_1: Integer; int_2: Integer; int_3: Integer; int_4: Integer; bytes: TJavaArray<Byte>; int_5: Integer; bytes_1: TJavaArray<Byte>; int_6: Integer; bytes_2: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function decode_Close: Integer; cdecl;
    function decode_Init(int: Integer; w: Jw): Integer; overload; cdecl;
    function decode_Init(z: Jz; w: Jw): Integer; overload; cdecl;
    function decode_Init(w: Jw): Integer; overload; cdecl;
    function decode_SetLightsMode(int: Integer): Integer; cdecl;
    function decode_SetMaxMultiReadCount(int: Integer): Integer; cdecl;
    function decode_StartContinuousScan(int: Integer): Integer; cdecl;
    function decode_StartMultiScan(int: Integer): Integer; cdecl;
    function decode_StartSingleScan(int: Integer): Integer; cdecl;
    function decode_StopScan: Integer; cdecl;
    function decode_setCameraId(int: Integer): Integer; cdecl;
    function deleteDataFromSecurityChip: Integer; cdecl;
    function deleteDataFromSecurityChipByID(int: Integer): Integer; cdecl;
    function enableTamper(bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function generatePINPrepareData(bytes: TJavaArray<Byte>; button: JButton; button_1: JButton; button_2: JButton; button_3: JButton; button_4: JButton; button_5: JButton; button_6: JButton; button_7: JButton; button_8: JButton; button_9: JButton; button_10: JButton; button_11: JButton; button_12: JButton; button_13: JButton; int: Integer): Integer; overload; cdecl;
    function generatePINPrepareData(bytes: TJavaArray<Byte>; button: JButton; button_1: JButton; button_2: JButton; button_3: JButton; button_4: JButton; button_5: JButton; button_6: JButton; button_7: JButton; button_8: JButton; button_9: JButton; button_10: JButton; button_11: JButton; button_12: JButton; activity: JActivity): Integer; overload; cdecl;
    function generatePINPrepareData(bytes: TJavaArray<Byte>; view: JView; view_1: JView; view_2: JView; view_3: JView; view_4: JView; view_5: JView; view_6: JView; view_7: JView; view_8: JView; view_9: JView; view_10: JView; view_11: JView; view_12: JView; view_13: JView; activity: JActivity): Integer; overload; cdecl;
    function generatePINPrepareData(bytes: TJavaArray<Byte>; view: JView; view_1: JView; view_2: JView; view_3: JView; view_4: JView; view_5: JView; view_6: JView; view_7: JView; view_8: JView; view_9: JView; view_10: JView; view_11: JView; view_12: JView; activity: JActivity): Integer; overload; cdecl;
    function generatePINPrepareData(bytes: TJavaArray<Byte>; button: JButton; button_1: JButton; button_2: JButton; button_3: JButton; button_4: JButton; button_5: JButton; button_6: JButton; button_7: JButton; button_8: JButton; button_9: JButton; button_10: JButton; button_11: JButton; button_12: JButton; button_13: JButton; activity: JActivity): Integer; overload; cdecl;
    function generatePINPrepareDataInAccessibilityMode(bytes: TJavaArray<Byte>; button: JButton; button_1: JButton; button_2: JButton; button_3: JButton; button_4: JButton; button_5: JButton; button_6: JButton; button_7: JButton; button_8: JButton; button_9: JButton; button_10: JButton; button_11: JButton; button_12: JButton; int: Integer; int_1: Integer; context: JContext): Integer; cdecl;
    function generatePINPrepareData_HS(bytes: TJavaArray<Byte>; button: JButton; button_1: JButton; button_2: JButton; button_3: JButton; button_4: JButton; button_5: JButton; button_6: JButton; button_7: JButton; button_8: JButton; button_9: JButton; button_10: JButton; button_11: JButton; button_12: JButton; button_13: JButton; context: JContext; int: Integer): Integer; cdecl;
    function generatePINPrepareData_View(bytes: TJavaArray<Byte>; view: JView; view_1: JView; view_2: JView; view_3: JView; view_4: JView; view_5: JView; view_6: JView; view_7: JView; view_8: JView; view_9: JView; view_10: JView; view_11: JView; view_12: JView; view_13: JView; context: JContext; int: Integer): Integer; cdecl;
    function genereateRandomNum(int: Integer; bytes: TJavaArray<Byte>): Integer; cdecl;
    function getBatteryLevel(bytes: TJavaArray<Byte>): Integer; cdecl;
    function getDateTime(bytes: TJavaArray<Byte>): Integer; cdecl;
    function getDelayPinBlock(string_1: JString; bytes: TJavaArray<Byte>): Integer; cdecl;
    function getDeviceEncryptData(string_1: JString; string_2: JString): JString; cdecl;
    function getDeviceStatus(bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function getDevicesVersion(bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function getDevicesVersionSTM(bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function getFinanCertVersion: JString; cdecl;
    function getFirmWareNumber: JString; cdecl;
    function getGMStatus(bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function getMac(bytes: TJavaArray<Byte>; int: Integer; bytes_1: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function getMacEx(string_1: JString; int: Integer; bytes: TJavaArray<Byte>; int_1: Integer; bytes_1: TJavaArray<Byte>; int_2: Integer; bytes_2: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function getMacExIndex(int: Integer; int_1: Integer; int_2: Integer; bytes: TJavaArray<Byte>; int_3: Integer; bytes_1: TJavaArray<Byte>; int_4: Integer; bytes_2: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function getMacForAesDukpt(string_1: JString; int: Integer; int_1: Integer; bytes: TJavaArray<Byte>; int_2: Integer; bytes_1: TJavaArray<Byte>; int_3: Integer; bytes_2: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function getMacForIPEK(string_1: JString; int: Integer; int_1: Integer; bytes: TJavaArray<Byte>; int_2: Integer; bytes_1: TJavaArray<Byte>; int_3: Integer; bytes_2: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function getMacForIPEKEx(int: Integer; int_1: Integer; int_2: Integer; bytes: TJavaArray<Byte>; int_3: Integer; bytes_1: TJavaArray<Byte>; int_4: Integer; bytes_2: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function getMacWithAlgorithm(string_1: JString; int: Integer; int_1: Integer; bytes: TJavaArray<Byte>; int_2: Integer; bytes_1: TJavaArray<Byte>; int_3: Integer; bytes_2: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function getOfflinePINTryCounter(int: Integer; bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function getSCVersion: JString; cdecl;
    function getSDKVersion: JString; cdecl;
    function getSPKernelVersion(bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function getSpID(bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function getSystemSn: JString; cdecl;
    function getTamper(bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function led(int: Integer; int_1: Integer; int_2: Integer; int_3: Integer; int_4: Integer): Integer; cdecl;
    function ledExpand(int: Integer; int_1: Integer; int_2: Integer): Integer; cdecl;
    function ledFlash(int: Integer; int_1: Integer; int_2: Integer; int_3: Integer; int_4: Integer; int_5: Integer; int_6: Integer): Integer; cdecl;
    function managerSPLog(int: Integer): Integer; cdecl;
    function onDestroy: Integer; cdecl;
    function pinPadRotation: Integer; cdecl;
    function readDataFromSecurityChip(string_1: JString; boolean: Boolean): JList; cdecl;
    function readDataFromSecurityChipByID(string_1: JString; int: Integer; boolean: Boolean): Jy; cdecl;
    function readDeviceSN: JString; cdecl;
    function readSN(bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function scanDisable: Integer; cdecl;
    function scanEnable: Integer; cdecl;
    function setCheckPackageName(boolean: Boolean): Integer; cdecl;
    function setDateTime(bytes: TJavaArray<Byte>): Integer; cdecl;
    function setDelayPin(boolean: Boolean): Integer; cdecl;
    procedure setPackageName(string_1: JString); cdecl;
    function setScanMode(int: Integer): Integer; cdecl;
    function setScannerCallback(x: Jx): Integer; cdecl;
    function setStartPinpadCallbackTimeout(int: Integer): Integer; cdecl;
    function setSupportSM(boolean: Boolean): Integer; cdecl;
    function spUpdate(int: Integer; int_1: Integer; int_2: Integer; int_3: Integer; bytes: TJavaArray<Byte>; bytes_1: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function speech(string_1: JString; v: Jv): Integer; cdecl;
    function speechInit(map: JMap): Integer; cdecl;
    function speechStop: Integer; cdecl;
    function startPanOrDataInput(int: Integer; int_1: Integer; int_2: Integer; int_3: Integer; u: Ju): Integer; cdecl;
    function startPinInput(int: Integer; string_1: JString; int_1: Integer; int_2: Integer; int_3: Integer; int_4: Integer; bytes: TJavaArray<Byte>; int_5: Integer; bytes_1: TJavaArray<Byte>; u: Ju): Integer; cdecl;
    function startPinInputDelay(int: Integer; int_1: Integer; int_2: Integer; int_3: Integer; int_4: Integer; bytes: TJavaArray<Byte>; int_5: Integer; bytes_1: TJavaArray<Byte>; int_6: Integer; u: Ju): Integer; cdecl;
    function startPinInputForIPEK(int: Integer; string_1: JString; int_1: Integer; int_2: Integer; int_3: Integer; int_4: Integer; bytes: TJavaArray<Byte>; int_5: Integer; bytes_1: TJavaArray<Byte>; u: Ju): Integer; cdecl;
    function startPinInputForIPEKEx(int: Integer; int_1: Integer; int_2: Integer; int_3: Integer; int_4: Integer; int_5: Integer; bytes: TJavaArray<Byte>; int_6: Integer; bytes_1: TJavaArray<Byte>; u: Ju): Integer; cdecl;
    function startPinInputInAccessibilityMode(int: Integer; string_1: JString; int_1: Integer; int_2: Integer; int_3: Integer; int_4: Integer; bytes: TJavaArray<Byte>; int_5: Integer; bytes_1: TJavaArray<Byte>; u: Ju): Integer; cdecl;
    function startPinInputOff(int: Integer; int_1: Integer; int_2: Integer; int_3: Integer; int_4: Integer; bytes: TJavaArray<Byte>; u: Ju): Integer; cdecl;
    function startPinInputOffLineForSingle(int: Integer; int_1: Integer; int_2: Integer; int_3: Integer; int_4: Integer; bytes: TJavaArray<Byte>; int_5: Integer; bytes_1: TJavaArray<Byte>; ints: TJavaArray<Integer>; u: Ju): Integer; cdecl;
    function startScan: Integer; cdecl;
    function stopScan: Integer; cdecl;
    function transmitPSAM(int: Integer; bytes: TJavaArray<Byte>; int_1: Integer; bytes_1: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function writeCallBackData(bytes: TJavaArray<Byte>; int: Integer): Integer; cdecl;
    function writeCallBackDataWithCommandID(int: Integer; bytes: TJavaArray<Byte>; int_1: Integer): Integer; cdecl;
    function writeDataToSecurityChip(string_1: JString; boolean: Boolean; y_1: Jy): Integer; cdecl;
    function writeSN(bytes: TJavaArray<Byte>; int: Integer): Integer; cdecl;
  end;
  TJCore = class(TJavaGenericImport<JCoreClass, JCore>) end;

  JGEDI_SMART_e_MemoryCardTypeClass = interface(JEnumClass)
    ['{E8B1C3EE-11F0-4C21-AEB0-2363823B407E}']
    {class} function _GetTYPE_44x2: JGEDI_SMART_e_MemoryCardType; cdecl;
    {class} function _GetTYPE_44x6: JGEDI_SMART_e_MemoryCardType; cdecl;
    {class} function _GetTYPE_44x8: JGEDI_SMART_e_MemoryCardType; cdecl;
    {class} function valueOf(string_1: JString): JGEDI_SMART_e_MemoryCardType; cdecl;
    {class} function values: TJavaObjectArray<JGEDI_SMART_e_MemoryCardType>; cdecl;
    {class} property TYPE_44x2: JGEDI_SMART_e_MemoryCardType read _GetTYPE_44x2;
    {class} property TYPE_44x6: JGEDI_SMART_e_MemoryCardType read _GetTYPE_44x6;
    {class} property TYPE_44x8: JGEDI_SMART_e_MemoryCardType read _GetTYPE_44x8;
  end;

  [JavaSignature('br/com/gertec/gedi/enums/GEDI_SMART_e_MemoryCardType')]
  JGEDI_SMART_e_MemoryCardType = interface(JEnum)
    ['{961E9CB0-5950-47C9-9806-1494BDB9D45A}']
    function getValue: Integer; cdecl;
  end;
  TJGEDI_SMART_e_MemoryCardType = class(TJavaGenericImport<JGEDI_SMART_e_MemoryCardTypeClass, JGEDI_SMART_e_MemoryCardType>) end;

  JoClass = interface(JObjectClass)
    ['{3A308C60-F339-400A-B99D-6F41CD0E6982}']
    {class} function a: Boolean; overload; cdecl;
    {class} function a(bytes: TJavaArray<Byte>; int: Integer): JString; overload; cdecl;
    {class} function a(bytes: TJavaArray<Byte>): JString; overload; cdecl;
    {class} function a(string_1: JString): TJavaArray<Byte>; overload; cdecl;
  end;

  [JavaSignature('gedi/o')]
  Jo = interface(JObject)
    ['{E5C89CF7-D5EB-4EAB-9F19-2585B44FAECA}']
  end;
  TJo = class(TJavaGenericImport<JoClass, Jo>) end;

  JGEDI_CRYPT_e_OpClass = interface(JEnumClass)
    ['{465CA623-412A-448F-9AEB-A6CED1E2756C}']
    {class} function _GetDECRYPTION: JGEDI_CRYPT_e_Op; cdecl;
    {class} function _GetENCRYPTION: JGEDI_CRYPT_e_Op; cdecl;
    {class} function valueOf(string_1: JString): JGEDI_CRYPT_e_Op; overload; cdecl;
    {class} function valueOf(int: Integer): JGEDI_CRYPT_e_Op; overload; cdecl;
    {class} function values: TJavaObjectArray<JGEDI_CRYPT_e_Op>; cdecl;
    {class} property DECRYPTION: JGEDI_CRYPT_e_Op read _GetDECRYPTION;
    {class} property ENCRYPTION: JGEDI_CRYPT_e_Op read _GetENCRYPTION;
  end;

  [JavaSignature('br/com/gertec/gedi/enums/GEDI_CRYPT_e_Op')]
  JGEDI_CRYPT_e_Op = interface(JEnum)
    ['{55653DFD-75B0-4A2C-B303-BE9801AC68DC}']
    function getValue: Integer; cdecl;
  end;
  TJGEDI_CRYPT_e_Op = class(TJavaGenericImport<JGEDI_CRYPT_e_OpClass, JGEDI_CRYPT_e_Op>) end;

  JGEDI_KMS_e_EncModeClass = interface(JEnumClass)
    ['{7C07D3A7-6235-4484-8B84-757E9CBED171}']
    {class} function _GetCBC: JGEDI_KMS_e_EncMode; cdecl;
    {class} function _GetCBC_3DUKPT_P1: JGEDI_KMS_e_EncMode; cdecl;
    {class} function _GetCBC_3DUKPT_P2: JGEDI_KMS_e_EncMode; cdecl;
    {class} function _GetCBC_3DUKPT_P3: JGEDI_KMS_e_EncMode; cdecl;
    {class} function _GetCBC_3DUKPT_P4: JGEDI_KMS_e_EncMode; cdecl;
    {class} function _GetCBC_3DUKPT_P5: JGEDI_KMS_e_EncMode; cdecl;
    {class} function _GetECB: JGEDI_KMS_e_EncMode; cdecl;
    {class} function _GetECB_3DUKPT_P1: JGEDI_KMS_e_EncMode; cdecl;
    {class} function _GetECB_3DUKPT_P2: JGEDI_KMS_e_EncMode; cdecl;
    {class} function _GetECB_3DUKPT_P3: JGEDI_KMS_e_EncMode; cdecl;
    {class} function _GetECB_3DUKPT_P4: JGEDI_KMS_e_EncMode; cdecl;
    {class} function _GetECB_3DUKPT_P5: JGEDI_KMS_e_EncMode; cdecl;
    {class} function valueOf(int: Integer): JGEDI_KMS_e_EncMode; overload; cdecl;
    {class} function valueOf(string_1: JString): JGEDI_KMS_e_EncMode; overload; cdecl;
    {class} function values: TJavaObjectArray<JGEDI_KMS_e_EncMode>; cdecl;
    {class} property CBC: JGEDI_KMS_e_EncMode read _GetCBC;
    {class} property CBC_3DUKPT_P1: JGEDI_KMS_e_EncMode read _GetCBC_3DUKPT_P1;
    {class} property CBC_3DUKPT_P2: JGEDI_KMS_e_EncMode read _GetCBC_3DUKPT_P2;
    {class} property CBC_3DUKPT_P3: JGEDI_KMS_e_EncMode read _GetCBC_3DUKPT_P3;
    {class} property CBC_3DUKPT_P4: JGEDI_KMS_e_EncMode read _GetCBC_3DUKPT_P4;
    {class} property CBC_3DUKPT_P5: JGEDI_KMS_e_EncMode read _GetCBC_3DUKPT_P5;
    {class} property ECB: JGEDI_KMS_e_EncMode read _GetECB;
    {class} property ECB_3DUKPT_P1: JGEDI_KMS_e_EncMode read _GetECB_3DUKPT_P1;
    {class} property ECB_3DUKPT_P2: JGEDI_KMS_e_EncMode read _GetECB_3DUKPT_P2;
    {class} property ECB_3DUKPT_P3: JGEDI_KMS_e_EncMode read _GetECB_3DUKPT_P3;
    {class} property ECB_3DUKPT_P4: JGEDI_KMS_e_EncMode read _GetECB_3DUKPT_P4;
    {class} property ECB_3DUKPT_P5: JGEDI_KMS_e_EncMode read _GetECB_3DUKPT_P5;
  end;

  [JavaSignature('br/com/gertec/gedi/enums/GEDI_KMS_e_EncMode')]
  JGEDI_KMS_e_EncMode = interface(JEnum)
    ['{70AB9CFC-623E-41AD-8B82-791C37D216F0}']
    function getValue: Integer; cdecl;
  end;
  TJGEDI_KMS_e_EncMode = class(TJavaGenericImport<JGEDI_KMS_e_EncModeClass, JGEDI_KMS_e_EncMode>) end;

  JGEDI_MSR_st_LastErrorsClass = interface(JObjectClass)
    ['{800560FB-CCF9-4389-8F0A-ADFE46EE7A1A}']
    {class} function init(gEDI_MSR_e_Status: JGEDI_MSR_e_Status; gEDI_MSR_e_Status_1: JGEDI_MSR_e_Status; gEDI_MSR_e_Status_2: JGEDI_MSR_e_Status): JGEDI_MSR_st_LastErrors; overload; cdecl;
    {class} function init: JGEDI_MSR_st_LastErrors; overload; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_MSR_st_LastErrors')]
  JGEDI_MSR_st_LastErrors = interface(JObject)
    ['{0938A586-3096-4CD1-94D2-B553CC152752}']
    function _GetpeTk1Err: JGEDI_MSR_e_Status; cdecl;
    function _GetpeTk2Err: JGEDI_MSR_e_Status; cdecl;
    function _GetpeTk3Err: JGEDI_MSR_e_Status; cdecl;
    property peTk1Err: JGEDI_MSR_e_Status read _GetpeTk1Err;
    property peTk2Err: JGEDI_MSR_e_Status read _GetpeTk2Err;
    property peTk3Err: JGEDI_MSR_e_Status read _GetpeTk3Err;
  end;
  TJGEDI_MSR_st_LastErrors = class(TJavaGenericImport<JGEDI_MSR_st_LastErrorsClass, JGEDI_MSR_st_LastErrors>) end;

  JGEDI_SMART_e_SlotClass = interface(JEnumClass)
    ['{71F3EB6A-2DB1-4121-990C-659561469FA2}']
    {class} function _GetSAM_1: JGEDI_SMART_e_Slot; cdecl;
    {class} function _GetSAM_2: JGEDI_SMART_e_Slot; cdecl;
    {class} function _GetSAM_3: JGEDI_SMART_e_Slot; cdecl;
    {class} function _GetSAM_4: JGEDI_SMART_e_Slot; cdecl;
    {class} function _GetUSER: JGEDI_SMART_e_Slot; cdecl;
    {class} function valueOf(int: Integer): JGEDI_SMART_e_Slot; overload; cdecl;
    {class} function valueOf(string_1: JString): JGEDI_SMART_e_Slot; overload; cdecl;
    {class} function values: TJavaObjectArray<JGEDI_SMART_e_Slot>; cdecl;
    {class} property SAM_1: JGEDI_SMART_e_Slot read _GetSAM_1;
    {class} property SAM_2: JGEDI_SMART_e_Slot read _GetSAM_2;
    {class} property SAM_3: JGEDI_SMART_e_Slot read _GetSAM_3;
    {class} property SAM_4: JGEDI_SMART_e_Slot read _GetSAM_4;
    {class} property USER: JGEDI_SMART_e_Slot read _GetUSER;
  end;

  [JavaSignature('br/com/gertec/gedi/enums/GEDI_SMART_e_Slot')]
  JGEDI_SMART_e_Slot = interface(JEnum)
    ['{22CB5074-8389-4E58-B017-ED7D032A8DF4}']
    function getValue: Integer; cdecl;
  end;
  TJGEDI_SMART_e_Slot = class(TJavaGenericImport<JGEDI_SMART_e_SlotClass, JGEDI_SMART_e_Slot>) end;

  JISMARTClass = interface(IJavaClass)
    ['{52E1E522-5CE6-4E45-AD75-885678172C11}']
  end;

  [JavaSignature('br/com/gertec/gedi/interfaces/ISMART')]
  JISMART = interface(IJavaInstance)
    ['{5ABC11C5-348F-465D-BA61-A1CF2F381412}']
    procedure PowerOff(gEDI_SMART_e_Slot: JGEDI_SMART_e_Slot); cdecl;
    function ResetEMV(gEDI_SMART_e_Slot: JGEDI_SMART_e_Slot; gEDI_SMART_e_Voltage: JGEDI_SMART_e_Voltage): JGEDI_SMART_st_ResetInfo; cdecl;
    function SendAPDU(gEDI_SMART_e_Slot: JGEDI_SMART_e_Slot; bytes: TJavaArray<Byte>): TJavaArray<Byte>; cdecl;
    function Status(gEDI_SMART_e_Slot: JGEDI_SMART_e_Slot): JGEDI_SMART_e_Status; cdecl;
    function WarmResetEMV(gEDI_SMART_e_Slot: JGEDI_SMART_e_Slot; gEDI_SMART_e_Voltage: JGEDI_SMART_e_Voltage): JGEDI_SMART_st_ResetInfo; cdecl;
  end;
  TJISMART = class(TJavaGenericImport<JISMARTClass, JISMART>) end;

  JGEDI_KMS_st_PINBlockClass = interface(JObjectClass)
    ['{C606643E-DED0-4DEA-A86D-B0831108BD04}']
    {class} function init(byte: Byte; gEDI_KMS_e_BLOCKTYPE_1: JGEDI_KMS_e_BLOCKTYPE; string_1: JString; bytes: TJavaArray<Byte>; byte_1: Byte; byte_2: Byte): JGEDI_KMS_st_PINBlock; overload; cdecl;
    {class} function init: JGEDI_KMS_st_PINBlock; overload; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_KMS_st_PINBlock')]
  JGEDI_KMS_st_PINBlock = interface(JObject)
    ['{676B3597-251B-489B-868B-639BE67E46C6}']
    function _GetabEncPINBlock: TJavaArray<Byte>; cdecl;
    function _GetabSeq: TJavaArray<Byte>; cdecl;
    function _GetbCN: Byte; cdecl;
    function _GetbVersion: Byte; cdecl;
    function _GetcPad: Byte; cdecl;
    function _GeteBlockType: JGEDI_KMS_e_BLOCKTYPE; cdecl;
    function _GetszPan: JString; cdecl;
    property abEncPINBlock: TJavaArray<Byte> read _GetabEncPINBlock;
    property abSeq: TJavaArray<Byte> read _GetabSeq;
    property bCN: Byte read _GetbCN;
    property bVersion: Byte read _GetbVersion;
    property cPad: Byte read _GetcPad;
    property eBlockType: JGEDI_KMS_e_BLOCKTYPE read _GeteBlockType;
    property szPan: JString read _GetszPan;
  end;
  TJGEDI_KMS_st_PINBlock = class(TJavaGenericImport<JGEDI_KMS_st_PINBlockClass, JGEDI_KMS_st_PINBlock>) end;

  JBankCardClass = interface(Jlibbasebinder_aClass)
    ['{E665A287-D667-413F-9F87-6CEDE06B2D85}']
    {class} function _GetCARD_DETECT_EXIST: Integer; cdecl;
    {class} function _GetCARD_DETECT_NOTEXIST: Integer; cdecl;
    {class} function _GetCARD_MODE_ALL: Integer; cdecl;
    {class} function _GetCARD_MODE_ICC: Integer; cdecl;
    {class} function _GetCARD_MODE_ICC_APDU: Integer; cdecl;
    {class} function _GetCARD_MODE_MAG: Integer; cdecl;
    {class} function _GetCARD_MODE_NFC: Integer; cdecl;
    {class} function _GetCARD_MODE_PICC: Integer; cdecl;
    {class} function _GetCARD_MODE_PSAM1: Integer; cdecl;
    {class} function _GetCARD_MODE_PSAM1_APDU: Integer; cdecl;
    {class} function _GetCARD_MODE_PSAM2: Integer; cdecl;
    {class} function _GetCARD_MODE_PSAM2_APDU: Integer; cdecl;
    {class} function _GetCARD_NMODE_ICC: Integer; cdecl;
    {class} function _GetCARD_NMODE_MAG: Integer; cdecl;
    {class} function _GetCARD_NMODE_PICC: Integer; cdecl;
    {class} function _GetCARD_READ_BANKCARD: Byte; cdecl;
    {class} function _GetCARD_READ_CANCELED: Byte; cdecl;
    {class} function _GetCARD_READ_CLOSE: Byte; cdecl;
    {class} function _GetCARD_READ_FAIL: Byte; cdecl;
    {class} function _GetCARD_READ_ICDETACT: Byte; cdecl;
    {class} function _GetCARD_READ_MAGENC: Byte; cdecl;
    {class} function _GetCARD_READ_MAGENCFAIL: Byte; cdecl;
    {class} function _GetCARD_READ_MANUAL: Byte; cdecl;
    {class} function _GetCARD_READ_OPEN: Byte; cdecl;
    {class} function _GetCARD_READ_PICCDETACT: Byte; cdecl;
    {class} function _GetCARD_READ_PSAM1DETACT: Byte; cdecl;
    {class} function _GetCARD_READ_PSAM2DETACT: Byte; cdecl;
    {class} function _GetCARD_READ_TIMEOUT: Byte; cdecl;
    {class} function _GetCARD_TYPE_INDUSTRY: Byte; cdecl;
    {class} function _GetCARD_TYPE_NORMAL: Byte; cdecl;
    {class} function _GetCARD_TYPE_TEST: Byte; cdecl;
    {class} function init(context: JContext): JBankCard; cdecl;
    {class} property CARD_DETECT_EXIST: Integer read _GetCARD_DETECT_EXIST;
    {class} property CARD_DETECT_NOTEXIST: Integer read _GetCARD_DETECT_NOTEXIST;
    {class} property CARD_MODE_ALL: Integer read _GetCARD_MODE_ALL;
    {class} property CARD_MODE_ICC: Integer read _GetCARD_MODE_ICC;
    {class} property CARD_MODE_ICC_APDU: Integer read _GetCARD_MODE_ICC_APDU;
    {class} property CARD_MODE_MAG: Integer read _GetCARD_MODE_MAG;
    {class} property CARD_MODE_NFC: Integer read _GetCARD_MODE_NFC;
    {class} property CARD_MODE_PICC: Integer read _GetCARD_MODE_PICC;
    {class} property CARD_MODE_PSAM1: Integer read _GetCARD_MODE_PSAM1;
    {class} property CARD_MODE_PSAM1_APDU: Integer read _GetCARD_MODE_PSAM1_APDU;
    {class} property CARD_MODE_PSAM2: Integer read _GetCARD_MODE_PSAM2;
    {class} property CARD_MODE_PSAM2_APDU: Integer read _GetCARD_MODE_PSAM2_APDU;
    {class} property CARD_NMODE_ICC: Integer read _GetCARD_NMODE_ICC;
    {class} property CARD_NMODE_MAG: Integer read _GetCARD_NMODE_MAG;
    {class} property CARD_NMODE_PICC: Integer read _GetCARD_NMODE_PICC;
    {class} property CARD_READ_BANKCARD: Byte read _GetCARD_READ_BANKCARD;
    {class} property CARD_READ_CANCELED: Byte read _GetCARD_READ_CANCELED;
    {class} property CARD_READ_CLOSE: Byte read _GetCARD_READ_CLOSE;
    {class} property CARD_READ_FAIL: Byte read _GetCARD_READ_FAIL;
    {class} property CARD_READ_ICDETACT: Byte read _GetCARD_READ_ICDETACT;
    {class} property CARD_READ_MAGENC: Byte read _GetCARD_READ_MAGENC;
    {class} property CARD_READ_MAGENCFAIL: Byte read _GetCARD_READ_MAGENCFAIL;
    {class} property CARD_READ_MANUAL: Byte read _GetCARD_READ_MANUAL;
    {class} property CARD_READ_OPEN: Byte read _GetCARD_READ_OPEN;
    {class} property CARD_READ_PICCDETACT: Byte read _GetCARD_READ_PICCDETACT;
    {class} property CARD_READ_PSAM1DETACT: Byte read _GetCARD_READ_PSAM1DETACT;
    {class} property CARD_READ_PSAM2DETACT: Byte read _GetCARD_READ_PSAM2DETACT;
    {class} property CARD_READ_TIMEOUT: Byte read _GetCARD_READ_TIMEOUT;
    {class} property CARD_TYPE_INDUSTRY: Byte read _GetCARD_TYPE_INDUSTRY;
    {class} property CARD_TYPE_NORMAL: Byte read _GetCARD_TYPE_NORMAL;
    {class} property CARD_TYPE_TEST: Byte read _GetCARD_TYPE_TEST;
  end;

  [JavaSignature('wangpos/sdk4/libbasebinder/BankCard')]
  JBankCard = interface(Jlibbasebinder_a)
    ['{123AB786-8240-42C0-AF39-A98FE4A63138}']
    function CLGetVersion(bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>; bytes_1: TJavaArray<Byte>; ints_1: TJavaArray<Integer>): Integer; cdecl;
    function CardActivation(bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function DesFire_Auth(int: Integer; int_1: Integer; int_2: Integer; bytes: TJavaArray<Byte>): Integer; cdecl;
    function DesFire_Comfirm_Cancel(int: Integer; bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function DesFire_CreatApp(int: Integer; int_1: Integer; int_2: Integer; bytes: TJavaArray<Byte>; bytes_1: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function DesFire_CreatFile(int: Integer; int_1: Integer; int_2: Integer; bytes: TJavaArray<Byte>; bytes_1: TJavaArray<Byte>; bytes_2: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function DesFire_CreatLine_CycleFile(int: Integer; int_1: Integer; int_2: Integer; bytes: TJavaArray<Byte>; bytes_1: TJavaArray<Byte>; bytes_2: TJavaArray<Byte>; bytes_3: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function DesFire_CreatValueFile(int: Integer; int_1: Integer; int_2: Integer; bytes: TJavaArray<Byte>; bytes_1: TJavaArray<Byte>; bytes_2: TJavaArray<Byte>; bytes_3: TJavaArray<Byte>; int_3: Integer; bytes_4: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function DesFire_DelFile(int: Integer; bytes: TJavaArray<Byte>; bytes_1: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function DesFire_GetCardInfo(int: Integer; int_1: Integer; bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function DesFire_ISO7816(bytes: TJavaArray<Byte>; bytes_1: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function DesFire_ReadFile(int: Integer; int_1: Integer; int_2: Integer; int_3: Integer; bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function DesFire_SelApp(int: Integer; bytes: TJavaArray<Byte>; bytes_1: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function DesFire_ValueFileOpr(int: Integer; int_1: Integer; bytes: TJavaArray<Byte>; bytes_1: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function DesFire_WriteFile(int: Integer; int_1: Integer; int_2: Integer; int_3: Integer; bytes: TJavaArray<Byte>; bytes_1: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function Felica_Open(int: Integer; int_1: Integer; int_2: Integer; ints: TJavaArray<Integer>; bytes: TJavaArray<Byte>; ints_1: TJavaArray<Integer>): Integer; cdecl;
    function Felica_Transmit(bytes: TJavaArray<Byte>; int: Integer; bytes_1: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function L1_Contactless_wupa: Integer; cdecl;
    function Logic_ModifyPW(int: Integer; bytes: TJavaArray<Byte>): Integer; cdecl;
    function Logic_ReadPWDegree(bytes: TJavaArray<Byte>): Integer; cdecl;
    function M0CardKeyAuth(bytes: TJavaArray<Byte>; int: Integer; bytes_1: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function M0GetSignData(int: Integer; bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function MifareULCAuthenticate(bytes: TJavaArray<Byte>; int: Integer): Integer; cdecl;
    function NFCTagReadBlock(int: Integer; bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function NFCTagWriteBlock(int: Integer; bytes: TJavaArray<Byte>): Integer; cdecl;
    function ReadLogicCardData(int: Integer; int_1: Integer; bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function ScrdGetVersion(bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>; bytes_1: TJavaArray<Byte>; ints_1: TJavaArray<Integer>): Integer; cdecl;
    function VerifyLogicCardPwd(bytes: TJavaArray<Byte>): Integer; cdecl;
    function WriteLogicCardData(bytes: TJavaArray<Byte>; int: Integer; int_1: Integer; bytes_1: TJavaArray<Byte>): Integer; cdecl;
    function breakOffCommand: Integer; cdecl;
    function cardReaderDetact(int: Integer; int_1: Integer; int_2: Integer; bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>; string_1: JString): Integer; cdecl;
    function closeCard(int: Integer): Integer; cdecl;
    function emvGuiHalt: Integer; cdecl;
    function enableAutoCheckCard(int: Integer; int_1: Integer): Integer; cdecl;
    function getCardSNFunction(bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function getCardTypeValidity(int: Integer): Integer; cdecl;
    function getMagCardTrack2Data: JString; cdecl;
    function iccDetect: Integer; cdecl;
    function icsLotPower(int: Integer; int_1: Integer; int_2: Integer; bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function isMitec: Integer; cdecl;
    function m1CardKeyAuth(int: Integer; int_1: Integer; int_2: Integer; bytes: TJavaArray<Byte>; int_3: Integer; bytes_1: TJavaArray<Byte>): Integer; cdecl;
    function m1CardKeyAuthAndReadBlockData(int: Integer; int_1: Integer; int_2: Integer; bytes: TJavaArray<Byte>; int_3: Integer; bytes_1: TJavaArray<Byte>; int_4: Integer; int_5: Integer; int_6: Integer; ints: TJavaArray<Integer>; bytes_2: TJavaArray<Byte>; ints_1: TJavaArray<Integer>; bytes_3: TJavaArray<Byte>; ints_2: TJavaArray<Integer>; bytes_4: TJavaArray<Byte>): Integer; cdecl;
    function m1CardReadBlockData(int: Integer; bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function m1CardValueOperation(int: Integer; int_1: Integer; int_2: Integer; int_3: Integer): Integer; cdecl;
    function m1CardValueOperationAndReadBlockData(int: Integer; int_1: Integer; int_2: Integer; int_3: Integer; bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function m1CardWriteAndReadBlockData(int: Integer; int_1: Integer; bytes: TJavaArray<Byte>; bytes_1: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function m1CardWriteBlockData(int: Integer; int_1: Integer; bytes: TJavaArray<Byte>): Integer; cdecl;
    function openCloseCardReader(int: Integer; int_1: Integer): Integer; cdecl;
    function openFelica: Integer; cdecl;
    function parseART(bytes: TJavaArray<Byte>; int: Integer; bytes_1: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function parseMagnetic(bytes: TJavaArray<Byte>; int: Integer; bytes_1: TJavaArray<Byte>; ints: TJavaArray<Integer>; bytes_2: TJavaArray<Byte>; ints_1: TJavaArray<Integer>; bytes_3: TJavaArray<Byte>; ints_2: TJavaArray<Integer>): Integer; cdecl;
    function piccDetect: Integer; cdecl;
    function readCard(byte: Byte; int_1: Integer; int: Integer; bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>; string_1: JString): Integer; cdecl;
    function readCardIndex(byte: Byte; int_1: Integer; int: Integer; bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>; int_2: Integer; int_3: Integer; int_4: Integer): Integer; cdecl;
    function readCardms(byte: Byte; int_1: Integer; int: Integer; bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>; string_1: JString): Integer; cdecl;
    function readContactlessInfo(bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function rfAnticoll(int: Integer; int_1: Integer; bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function rfRequst(int: Integer; boolean: Boolean; int_1: Integer; bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function rfSelect(bytes: TJavaArray<Byte>; int: Integer; bytes_1: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function sendAPDU(int: Integer; bytes: TJavaArray<Byte>; int_1: Integer; bytes_1: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function sendMultiAPDU(int: Integer; bytes: TJavaArray<Byte>; int_1: Integer; bytes_1: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    procedure setCardTypeValidity(int: Integer; int_1: Integer); cdecl;
    function setEnableLog(boolean: Boolean): Integer; cdecl;
    function testSerialPort(int: Integer; list: JList; int_1: Integer; bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function transmitFelicaCmd(bytes: TJavaArray<Byte>; int: Integer; int_1: Integer; bytes_1: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function waitCard(int: Integer; int_1: Integer; bytes: TJavaArray<Byte>; int_2: Integer; int_3: Integer; bytes_1: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
  end;
  TJBankCard = class(TJavaGenericImport<JBankCardClass, JBankCard>) end;

  JGEDI_LED_e_IdClass = interface(JEnumClass)
    ['{D342FB28-68A9-4735-A15A-2FF491B863F1}']
    {class} function _GetGEDI_LED_ID_1: JGEDI_LED_e_Id; cdecl;
    {class} function _GetGEDI_LED_ID_2: JGEDI_LED_e_Id; cdecl;
    {class} function _GetGEDI_LED_ID_3: JGEDI_LED_e_Id; cdecl;
    {class} function _GetGEDI_LED_ID_BACKLIGHT_1: JGEDI_LED_e_Id; cdecl;
    {class} function _GetGEDI_LED_ID_BACKLIGHT_2: JGEDI_LED_e_Id; cdecl;
    {class} function _GetGEDI_LED_ID_BACKLIGHT_DISPLAY: JGEDI_LED_e_Id; cdecl;
    {class} function _GetGEDI_LED_ID_BACKLIGHT_KEYBOARD: JGEDI_LED_e_Id; cdecl;
    {class} function _GetGEDI_LED_ID_BLUE: JGEDI_LED_e_Id; cdecl;
    {class} function _GetGEDI_LED_ID_CONTACTLESS_1: JGEDI_LED_e_Id; cdecl;
    {class} function _GetGEDI_LED_ID_CONTACTLESS_2: JGEDI_LED_e_Id; cdecl;
    {class} function _GetGEDI_LED_ID_CONTACTLESS_3: JGEDI_LED_e_Id; cdecl;
    {class} function _GetGEDI_LED_ID_CONTACTLESS_4: JGEDI_LED_e_Id; cdecl;
    {class} function _GetGEDI_LED_ID_CONTACTLESS_ALL: JGEDI_LED_e_Id; cdecl;
    {class} function _GetGEDI_LED_ID_CONTACTLESS_BLUE: JGEDI_LED_e_Id; cdecl;
    {class} function _GetGEDI_LED_ID_CONTACTLESS_GREEN: JGEDI_LED_e_Id; cdecl;
    {class} function _GetGEDI_LED_ID_CONTACTLESS_ORANGE: JGEDI_LED_e_Id; cdecl;
    {class} function _GetGEDI_LED_ID_CONTACTLESS_RED: JGEDI_LED_e_Id; cdecl;
    {class} function _GetGEDI_LED_ID_GREEN: JGEDI_LED_e_Id; cdecl;
    {class} function _GetGEDI_LED_ID_MAIN: JGEDI_LED_e_Id; cdecl;
    {class} function _GetGEDI_LED_ID_ORANGE: JGEDI_LED_e_Id; cdecl;
    {class} function _GetGEDI_LED_ID_RED: JGEDI_LED_e_Id; cdecl;
    {class} function valueOf(string_1: JString): JGEDI_LED_e_Id; overload; cdecl;
    {class} function valueOf(int: Integer): JGEDI_LED_e_Id; overload; cdecl;
    {class} function values: TJavaObjectArray<JGEDI_LED_e_Id>; cdecl;
    {class} property GEDI_LED_ID_1: JGEDI_LED_e_Id read _GetGEDI_LED_ID_1;
    {class} property GEDI_LED_ID_2: JGEDI_LED_e_Id read _GetGEDI_LED_ID_2;
    {class} property GEDI_LED_ID_3: JGEDI_LED_e_Id read _GetGEDI_LED_ID_3;
    {class} property GEDI_LED_ID_BACKLIGHT_1: JGEDI_LED_e_Id read _GetGEDI_LED_ID_BACKLIGHT_1;
    {class} property GEDI_LED_ID_BACKLIGHT_2: JGEDI_LED_e_Id read _GetGEDI_LED_ID_BACKLIGHT_2;
    {class} property GEDI_LED_ID_BACKLIGHT_DISPLAY: JGEDI_LED_e_Id read _GetGEDI_LED_ID_BACKLIGHT_DISPLAY;
    {class} property GEDI_LED_ID_BACKLIGHT_KEYBOARD: JGEDI_LED_e_Id read _GetGEDI_LED_ID_BACKLIGHT_KEYBOARD;
    {class} property GEDI_LED_ID_BLUE: JGEDI_LED_e_Id read _GetGEDI_LED_ID_BLUE;
    {class} property GEDI_LED_ID_CONTACTLESS_1: JGEDI_LED_e_Id read _GetGEDI_LED_ID_CONTACTLESS_1;
    {class} property GEDI_LED_ID_CONTACTLESS_2: JGEDI_LED_e_Id read _GetGEDI_LED_ID_CONTACTLESS_2;
    {class} property GEDI_LED_ID_CONTACTLESS_3: JGEDI_LED_e_Id read _GetGEDI_LED_ID_CONTACTLESS_3;
    {class} property GEDI_LED_ID_CONTACTLESS_4: JGEDI_LED_e_Id read _GetGEDI_LED_ID_CONTACTLESS_4;
    {class} property GEDI_LED_ID_CONTACTLESS_ALL: JGEDI_LED_e_Id read _GetGEDI_LED_ID_CONTACTLESS_ALL;
    {class} property GEDI_LED_ID_CONTACTLESS_BLUE: JGEDI_LED_e_Id read _GetGEDI_LED_ID_CONTACTLESS_BLUE;
    {class} property GEDI_LED_ID_CONTACTLESS_GREEN: JGEDI_LED_e_Id read _GetGEDI_LED_ID_CONTACTLESS_GREEN;
    {class} property GEDI_LED_ID_CONTACTLESS_ORANGE: JGEDI_LED_e_Id read _GetGEDI_LED_ID_CONTACTLESS_ORANGE;
    {class} property GEDI_LED_ID_CONTACTLESS_RED: JGEDI_LED_e_Id read _GetGEDI_LED_ID_CONTACTLESS_RED;
    {class} property GEDI_LED_ID_GREEN: JGEDI_LED_e_Id read _GetGEDI_LED_ID_GREEN;
    {class} property GEDI_LED_ID_MAIN: JGEDI_LED_e_Id read _GetGEDI_LED_ID_MAIN;
    {class} property GEDI_LED_ID_ORANGE: JGEDI_LED_e_Id read _GetGEDI_LED_ID_ORANGE;
    {class} property GEDI_LED_ID_RED: JGEDI_LED_e_Id read _GetGEDI_LED_ID_RED;
  end;

  [JavaSignature('br/com/gertec/gedi/enums/GEDI_LED_e_Id')]
  JGEDI_LED_e_Id = interface(JEnum)
    ['{8DA5F889-15EC-4C00-B55E-8DFB241F2570}']
    function getValue: Integer; cdecl;
  end;
  TJGEDI_LED_e_Id = class(TJavaGenericImport<JGEDI_LED_e_IdClass, JGEDI_LED_e_Id>) end;

  JGEDI_PRNTR_e_BarCodeTypeClass = interface(JEnumClass)
    ['{38AE40FC-FC25-4610-A997-B402F1BF9916}']
    {class} function _GetAZTEC: JGEDI_PRNTR_e_BarCodeType; cdecl;
    {class} function _GetCODABAR: JGEDI_PRNTR_e_BarCodeType; cdecl;
    {class} function _GetCODE_128: JGEDI_PRNTR_e_BarCodeType; cdecl;
    {class} function _GetCODE_39: JGEDI_PRNTR_e_BarCodeType; cdecl;
    {class} function _GetCODE_93: JGEDI_PRNTR_e_BarCodeType; cdecl;
    {class} function _GetDATA_MATRIX: JGEDI_PRNTR_e_BarCodeType; cdecl;
    {class} function _GetEAN_13: JGEDI_PRNTR_e_BarCodeType; cdecl;
    {class} function _GetEAN_8: JGEDI_PRNTR_e_BarCodeType; cdecl;
    {class} function _GetITF: JGEDI_PRNTR_e_BarCodeType; cdecl;
    {class} function _GetMAXICODE: JGEDI_PRNTR_e_BarCodeType; cdecl;
    {class} function _GetPDF_417: JGEDI_PRNTR_e_BarCodeType; cdecl;
    {class} function _GetQR_CODE: JGEDI_PRNTR_e_BarCodeType; cdecl;
    {class} function _GetRSS_14: JGEDI_PRNTR_e_BarCodeType; cdecl;
    {class} function _GetRSS_EXPANDED: JGEDI_PRNTR_e_BarCodeType; cdecl;
    {class} function _GetUPC_A: JGEDI_PRNTR_e_BarCodeType; cdecl;
    {class} function _GetUPC_E: JGEDI_PRNTR_e_BarCodeType; cdecl;
    {class} function _GetUPC_EAN_EXTENSION: JGEDI_PRNTR_e_BarCodeType; cdecl;
    {class} function valueOf(int: Integer): JGEDI_PRNTR_e_BarCodeType; overload; cdecl;
    {class} function valueOf(string_1: JString): JGEDI_PRNTR_e_BarCodeType; overload; cdecl;
    {class} function values: TJavaObjectArray<JGEDI_PRNTR_e_BarCodeType>; cdecl;
    {class} property AZTEC: JGEDI_PRNTR_e_BarCodeType read _GetAZTEC;
    {class} property CODABAR: JGEDI_PRNTR_e_BarCodeType read _GetCODABAR;
    {class} property CODE_128: JGEDI_PRNTR_e_BarCodeType read _GetCODE_128;
    {class} property CODE_39: JGEDI_PRNTR_e_BarCodeType read _GetCODE_39;
    {class} property CODE_93: JGEDI_PRNTR_e_BarCodeType read _GetCODE_93;
    {class} property DATA_MATRIX: JGEDI_PRNTR_e_BarCodeType read _GetDATA_MATRIX;
    {class} property EAN_13: JGEDI_PRNTR_e_BarCodeType read _GetEAN_13;
    {class} property EAN_8: JGEDI_PRNTR_e_BarCodeType read _GetEAN_8;
    {class} property ITF: JGEDI_PRNTR_e_BarCodeType read _GetITF;
    {class} property MAXICODE: JGEDI_PRNTR_e_BarCodeType read _GetMAXICODE;
    {class} property PDF_417: JGEDI_PRNTR_e_BarCodeType read _GetPDF_417;
    {class} property QR_CODE: JGEDI_PRNTR_e_BarCodeType read _GetQR_CODE;
    {class} property RSS_14: JGEDI_PRNTR_e_BarCodeType read _GetRSS_14;
    {class} property RSS_EXPANDED: JGEDI_PRNTR_e_BarCodeType read _GetRSS_EXPANDED;
    {class} property UPC_A: JGEDI_PRNTR_e_BarCodeType read _GetUPC_A;
    {class} property UPC_E: JGEDI_PRNTR_e_BarCodeType read _GetUPC_E;
    {class} property UPC_EAN_EXTENSION: JGEDI_PRNTR_e_BarCodeType read _GetUPC_EAN_EXTENSION;
  end;

  [JavaSignature('br/com/gertec/gedi/enums/GEDI_PRNTR_e_BarCodeType')]
  JGEDI_PRNTR_e_BarCodeType = interface(JEnum)
    ['{3F8714CC-615C-4784-929B-25F4686B03A1}']
    function getValue: Integer; cdecl;
  end;
  TJGEDI_PRNTR_e_BarCodeType = class(TJavaGenericImport<JGEDI_PRNTR_e_BarCodeTypeClass, JGEDI_PRNTR_e_BarCodeType>) end;

  JGEDI_CL_st_MF_KeyClass = interface(JObjectClass)
    ['{00CFBF7C-41B5-4A85-A1EA-001B8C335CF5}']
    {class} function init: JGEDI_CL_st_MF_Key; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_CL_st_MF_Key')]
  JGEDI_CL_st_MF_Key = interface(JObject)
    ['{E8EBDF07-CC89-4CFD-8FC3-9D0A4872EAAD}']
    function _GetabValue: TJavaArray<Byte>; cdecl;
    function _GeteType: JGEDI_CL_e_MF_KeyType; cdecl;
    property abValue: TJavaArray<Byte> read _GetabValue;
    property eType: JGEDI_CL_e_MF_KeyType read _GeteType;
  end;
  TJGEDI_CL_st_MF_Key = class(TJavaGenericImport<JGEDI_CL_st_MF_KeyClass, JGEDI_CL_st_MF_Key>) end;

  JGEDI_KBD_e_PowerKeyModeClass = interface(JEnumClass)
    ['{C99550F6-A297-4ABD-A1AA-E9E86EFAC301}']
    {class} function _GetPOWEROFF: JGEDI_KBD_e_PowerKeyMode; cdecl;
    {class} function _GetRESET: JGEDI_KBD_e_PowerKeyMode; cdecl;
    {class} function valueOf(string_1: JString): JGEDI_KBD_e_PowerKeyMode; cdecl;
    {class} function values: TJavaObjectArray<JGEDI_KBD_e_PowerKeyMode>; cdecl;
    {class} property POWEROFF: JGEDI_KBD_e_PowerKeyMode read _GetPOWEROFF;
    {class} property RESET: JGEDI_KBD_e_PowerKeyMode read _GetRESET;
  end;

  [JavaSignature('br/com/gertec/gedi/enums/GEDI_KBD_e_PowerKeyMode')]
  JGEDI_KBD_e_PowerKeyMode = interface(JEnum)
    ['{40330AB9-DE8B-4BC1-930E-E2274DF24820}']
    function getValue: Integer; cdecl;
  end;
  TJGEDI_KBD_e_PowerKeyMode = class(TJavaGenericImport<JGEDI_KBD_e_PowerKeyModeClass, JGEDI_KBD_e_PowerKeyMode>) end;

  JGEDI_CLOCK_e_ProvidedTimeClass = interface(JEnumClass)
    ['{7BE92EC7-FCE6-44DE-B79C-0D3C6C054C96}']
    {class} function _GetGPS: JGEDI_CLOCK_e_ProvidedTime; cdecl;
    {class} function _GetNETWORK: JGEDI_CLOCK_e_ProvidedTime; cdecl;
    {class} function _GetOFF: JGEDI_CLOCK_e_ProvidedTime; cdecl;
    {class} function valueOf(string_1: JString): JGEDI_CLOCK_e_ProvidedTime; cdecl;
    {class} function values: TJavaObjectArray<JGEDI_CLOCK_e_ProvidedTime>; cdecl;
    {class} property GPS: JGEDI_CLOCK_e_ProvidedTime read _GetGPS;
    {class} property NETWORK: JGEDI_CLOCK_e_ProvidedTime read _GetNETWORK;
    {class} property OFF: JGEDI_CLOCK_e_ProvidedTime read _GetOFF;
  end;

  [JavaSignature('br/com/gertec/gedi/enums/GEDI_CLOCK_e_ProvidedTime')]
  JGEDI_CLOCK_e_ProvidedTime = interface(JEnum)
    ['{8FF61414-BA1A-4D5E-A682-8575EAEE661A}']
    function getValue: Integer; cdecl;
  end;
  TJGEDI_CLOCK_e_ProvidedTime = class(TJavaGenericImport<JGEDI_CLOCK_e_ProvidedTimeClass, JGEDI_CLOCK_e_ProvidedTime>) end;

  JrClass = interface(JIInterfaceClass)
    ['{04F2E178-FFCF-4DB8-9996-551C7F3CC230}']
  end;

  [JavaSignature('gedi/r')]
  Jr = interface(JIInterface)
    ['{F346FB3F-256C-48EA-A54E-B216C8631963}']
    function a(int: Integer; bytes: TJavaArray<Byte>): Integer; overload; cdecl;
    function a(int: Integer; bytes: TJavaArray<Byte>; int_1: Integer; bytes_1: TJavaArray<Byte>; ints: TJavaArray<Integer>; bytes_2: TJavaArray<Byte>): Integer; overload; cdecl;
    function e(int: Integer; int_1: Integer): Integer; cdecl;
    function f(bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function j(bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; cdecl;
    function l: Integer; cdecl;
    function p: Integer; cdecl;
  end;
  TJr = class(TJavaGenericImport<JrClass, Jr>) end;

  JPrinter_AlignClass = interface(JEnumClass)
    ['{0BE93D10-D601-448F-B527-21BC9AB438C6}']
    {class} function _GetCENTER: JPrinter_Align; cdecl;
    {class} function _GetLEFT: JPrinter_Align; cdecl;
    {class} function _GetRIGHT: JPrinter_Align; cdecl;
    {class} function valueOf(string_1: JString): JPrinter_Align; cdecl;
    {class} function values: TJavaObjectArray<JPrinter_Align>; cdecl;
    {class} property CENTER: JPrinter_Align read _GetCENTER;
    {class} property LEFT: JPrinter_Align read _GetLEFT;
    {class} property RIGHT: JPrinter_Align read _GetRIGHT;
  end;

  [JavaSignature('wangpos/sdk4/libbasebinder/Printer$Align')]
  JPrinter_Align = interface(JEnum)
    ['{7EDACC9B-230D-49BA-BEDA-A238BF4199DB}']
  end;
  TJPrinter_Align = class(TJavaGenericImport<JPrinter_AlignClass, JPrinter_Align>) end;

  JPrinterClass = interface(Jlibbasebinder_aClass)
    ['{4BA576C6-3F35-471E-8F5F-5503659FC276}']
    {class} function _GetPAPER_WIDTH: Integer; cdecl;
    {class} function init(context: JContext): JPrinter; cdecl;
    {class} function printMultiseriateString(ints: TJavaArray<Integer>; strings: TJavaObjectArray<JString>; int: Integer; align: JPrinter_Align; boolean: Boolean; boolean_1: Boolean): Integer; cdecl;
    {class} property PAPER_WIDTH: Integer read _GetPAPER_WIDTH;
  end;

  [JavaSignature('wangpos/sdk4/libbasebinder/Printer')]
  JPrinter = interface(Jlibbasebinder_a)
    ['{BDA849BA-E8F9-4D53-A793-CCEA90B92801}']
    procedure CheckBlueToothPrintStatus; cdecl;
    procedure CheckUSBPrintStatus; cdecl;
    function Get_ClearPrinterMileage(int: Integer; bytes: TJavaArray<Byte>): Integer; cdecl;
    function SDK_Printer_Test: Integer; cdecl;
    function bluetoothPrintInit(bluetoothDevice: JBluetoothDevice): Integer; cdecl;
    function clearPrintDataCache: Integer; cdecl;
    function cutPaper: Integer; cdecl;
    procedure enableCustomFont(boolean: Boolean); cdecl;
    function escposBlueToothPrint(bytes: TJavaArray<Byte>): Integer; cdecl;
    function finishBlueToothPrint: Integer; cdecl;
    function finishUSBPrint: Integer; cdecl;
    function getPrinterStatus(ints: TJavaArray<Integer>): Integer; cdecl;
    function initBlueToothPrint(v: Jv): Integer; cdecl;
    function initUSBPrint(v: Jv): Integer; cdecl;
    function print2StringInLine(string_1: JString; string_2: JString; float: Single; font: JPrinter_Font; int: Integer; align: JPrinter_Align; boolean: Boolean; boolean_1: Boolean; boolean_2: Boolean): Integer; cdecl;
    function printBarCode(string_1: JString; int: Integer; boolean: Boolean): Integer; cdecl;
    function printBarCodeBase(string_1: JString; barcodeType: JPrinter_BarcodeType; barcodeWidth: JPrinter_BarcodeWidth; int: Integer; int_1: Integer): Integer; cdecl;
    function printFinish: Integer; cdecl;
    function printImage(bitmap: JBitmap; int: Integer; align: JPrinter_Align): Integer; cdecl;
    function printImageBase(bitmap: JBitmap; int: Integer; int_1: Integer; align: JPrinter_Align; int_2: Integer): Integer; cdecl;
    function printInit: Integer; cdecl;
    function printPDF417(string_1: JString; int: Integer; int_1: Integer; align: JPrinter_Align; int_2: Integer): Integer; overload; cdecl;
    function printPDF417(string_1: JString; int: Integer; int_1: Integer; align: JPrinter_Align): Integer; overload; cdecl;
    function printPDF417(string_1: JString; int: Integer; int_1: Integer; int_2: Integer): Integer; overload; cdecl;
    function printPDF417(string_1: JString; int: Integer; int_1: Integer): Integer; overload; cdecl;
    function printPDF417(string_1: JString): Integer; overload; cdecl;
    function printPDF417(string_1: JString; int: Integer): Integer; overload; cdecl;
    function printPaper(int: Integer): Integer; cdecl;
    function printPaper_trade(int: Integer; int_1: Integer): Integer; cdecl;
    function printPhoto(bitmap: JBitmap; int: Integer; int_1: Integer; align: JPrinter_Align; int_2: Integer): Integer; cdecl;
    function printQRCode(string_1: JString; int: Integer; align: JPrinter_Align; boolean: Boolean): Integer; overload; cdecl;
    function printQRCode(string_1: JString; int: Integer; align: JPrinter_Align): Integer; overload; cdecl;
    function printQRCode(string_1: JString; int: Integer): Integer; overload; cdecl;
    function printQRCode(string_1: JString): Integer; overload; cdecl;
    function printString(string_1: JString; font: JPrinter_Font; int: Integer; align: JPrinter_Align; boolean: Boolean; boolean_1: Boolean; boolean_2: Boolean; boolean_3: Boolean): Integer; overload; cdecl;
    function printString(string_1: JString; font: JPrinter_Font; int: Integer; align: JPrinter_Align; boolean: Boolean; boolean_1: Boolean; boolean_2: Boolean): Integer; overload; cdecl;
    function printString(string_1: JString; int: Integer; align: JPrinter_Align; boolean: Boolean; boolean_1: Boolean; boolean_2: Boolean): Integer; overload; cdecl;
    function printString(string_1: JString; int: Integer; align: JPrinter_Align; boolean: Boolean; boolean_1: Boolean): Integer; overload; cdecl;
    function printStringBase(string_1: JString; int: Integer; float: Single; float_1: Single; int_1: Integer; align: JPrinter_Align; boolean: Boolean; boolean_1: Boolean; boolean_2: Boolean): Integer; cdecl;
    function printStringExt(string_1: JString; int: Integer; float: Single; float_1: Single; font: JPrinter_Font; int_1: Integer; align: JPrinter_Align; boolean: Boolean; boolean_1: Boolean; boolean_2: Boolean): Integer; cdecl;
    function printStringWithScaleX(string_1: JString; int: Integer; float: Single; float_1: Single; float_2: Single; font: JPrinter_Font; int_1: Integer; align: JPrinter_Align; boolean: Boolean; boolean_1: Boolean; boolean_2: Boolean): Integer; cdecl;
    function setGrayLevel(int: Integer): Integer; cdecl;
    procedure setPrintFontType(context: JContext; string_1: JString); cdecl;
    function setPrintLineSpacing(int: Integer): Integer; cdecl;
    function setPrintPaperType(int: Integer): Integer; cdecl;
    function setPrintPaperWide(int: Integer): Integer; cdecl;
    function setPrintType(int: Integer): Integer; cdecl;
    function startBlueToothPrint: Integer; cdecl;
    function startUSBPrint: Integer; cdecl;
  end;
  TJPrinter = class(TJavaGenericImport<JPrinterClass, JPrinter>) end;

  JhClass = interface(JObjectClass)
    ['{A9B377C5-BC17-4F03-8157-1D0539D665FC}']
    {class} function init: Jh; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/h')]
  Jh = interface(JObject)
    ['{018AA24A-1247-4F03-A918-F9FB11F82AF3}']
    function CapturePINBlock(gEDI_KMS_st_Control: JGEDI_KMS_st_Control; boolean: Boolean; gEDI_KMS_st_Data_1: JGEDI_KMS_st_Data; list: JList): JGEDI_KMS_st_CapturePINBlockInfo; cdecl;
    function DUKPTKSNGet(gEDI_KMS_e_KEYTYPE: JGEDI_KMS_e_KEYTYPE; gEDI_KMS_e_KEYPURPOSE: JGEDI_KMS_e_KEYPURPOSE; int: Integer): TJavaArray<Byte>; cdecl;
    function EncryptData(gEDI_KMS_st_Data: JGEDI_KMS_st_Data): JGEDI_KMS_st_Data; cdecl;
    function KCVGet(gEDI_KMS_e_KEYTYPE: JGEDI_KMS_e_KEYTYPE; gEDI_KMS_e_KEYPURPOSE: JGEDI_KMS_e_KEYPURPOSE; int: Integer): TJavaArray<Byte>; cdecl;
    function KeyMapGet: JList; cdecl;
    function KeyPresenceTest(gEDI_KMS_e_KEYTYPE: JGEDI_KMS_e_KEYTYPE; gEDI_KMS_e_KEYPURPOSE: JGEDI_KMS_e_KEYPURPOSE; int: Integer): Boolean; cdecl;
    function a(int: Integer; int_1: Integer; int_2: Integer): Integer; overload; cdecl;
    function a(int: Integer; int_1: Integer; int_2: Integer; int_3: Integer; boolean: Boolean; callbacks_1: JGEDI_KMS_st_Control_Callbacks; boolean_1: Boolean; bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function a(int: Integer; int_1: Integer; bytes: TJavaArray<Byte>; int_2: Integer; string_1: JString; bytes_1: TJavaArray<Byte>; bytes_2: TJavaArray<Byte>): Integer; overload; cdecl;
    function a(int: Integer; int_1: Integer; int_2: Integer; int_3: Integer; bytes: TJavaArray<Byte>; bytes_1: TJavaArray<Byte>; bytes_2: TJavaArray<Byte>; bytes_3: TJavaArray<Byte>; ints: TJavaArray<Integer>; bytes_4: TJavaArray<Byte>): Integer; overload; cdecl;
    procedure a; overload; cdecl;
    procedure a(gEDI_KMS_st_SaveKey: JGEDI_KMS_st_SaveKey); overload; cdecl;
    function a(int: Integer; int_1: Integer; int_2: Integer; bytes: TJavaArray<Byte>; bytes_1: TJavaArray<Byte>): Integer; overload; cdecl;
  end;
  TJh = class(TJavaGenericImport<JhClass, Jh>) end;

  JGEDI_PRNTR_st_StringConfigClass = interface(JObjectClass)
    ['{E2DBE826-F60C-46B9-B8CF-6DED86E07F56}']
    {class} function init(paint: JPaint): JGEDI_PRNTR_st_StringConfig; overload; cdecl;
    {class} function init(paint: JPaint; int: Integer; int_1: Integer): JGEDI_PRNTR_st_StringConfig; overload; cdecl;
    {class} function init: JGEDI_PRNTR_st_StringConfig; overload; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_PRNTR_st_StringConfig')]
  JGEDI_PRNTR_st_StringConfig = interface(JObject)
    ['{CA4FC28C-21BF-4A12-9498-29E185042DE5}']
    function _GetlineSpace: Integer; cdecl;
    function _Getoffset: Integer; cdecl;
    function _Getpaint: JPaint; cdecl;
    function clone: JGEDI_PRNTR_st_StringConfig; overload; cdecl;
//Mesmo    function clone: JObject; overload; cdecl;
    property lineSpace: Integer read _GetlineSpace;
    property offset: Integer read _Getoffset;
    property paint: JPaint read _Getpaint;
  end;
  TJGEDI_PRNTR_st_StringConfig = class(TJavaGenericImport<JGEDI_PRNTR_st_StringConfigClass, JGEDI_PRNTR_st_StringConfig>) end;

  Jgedi_kClass = interface(JObjectClass)
    ['{06B5981C-8CCC-4C88-9B6B-E97968613C33}']
    {class} function init: Jgedi_k; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/k')]
  Jgedi_k = interface(JObject)
    ['{8559906D-7B25-4F46-8C73-B87F4158AC69}']
    procedure ApDefaultSet(string_1: JString); cdecl;
    procedure ApDelete(string_1: JString); cdecl;
    function GetNameCrt(gEDI_PM_e_TypeCrt: JGEDI_PM_e_TypeCrt): JString; cdecl;
    procedure UpdateFromFile(string_1: JString; gEDI_FS_e_Storage: JGEDI_FS_e_Storage); cdecl;
  end;
  TJgedi_k = class(TJavaGenericImport<Jgedi_kClass, Jgedi_k>) end;

  JGEDI_KMS_e_OPClass = interface(JEnumClass)
    ['{720FE756-6F50-4EA1-B601-1CD5B77CD71A}']
    {class} function _GetDECRYPT: JGEDI_KMS_e_OP; cdecl;
    {class} function _GetENCRYPT: JGEDI_KMS_e_OP; cdecl;
    {class} function valueOf(string_1: JString): JGEDI_KMS_e_OP; overload; cdecl;
    {class} function valueOf(int: Integer): JGEDI_KMS_e_OP; overload; cdecl;
    {class} function values: TJavaObjectArray<JGEDI_KMS_e_OP>; cdecl;
    {class} property DECRYPT: JGEDI_KMS_e_OP read _GetDECRYPT;
    {class} property ENCRYPT: JGEDI_KMS_e_OP read _GetENCRYPT;
  end;

  [JavaSignature('br/com/gertec/gedi/enums/GEDI_KMS_e_OP')]
  JGEDI_KMS_e_OP = interface(JEnum)
    ['{18AF0B56-FD9B-4CC9-B4BF-999C9090A3D8}']
    function getValue: Integer; cdecl;
  end;
  TJGEDI_KMS_e_OP = class(TJavaGenericImport<JGEDI_KMS_e_OPClass, JGEDI_KMS_e_OP>) end;

  JICRYPTClass = interface(IJavaClass)
    ['{F39E722D-784A-4E54-BC78-B7F10DD85E1D}']
  end;

  [JavaSignature('br/com/gertec/gedi/interfaces/ICRYPT')]
  JICRYPT = interface(IJavaInstance)
    ['{18DDBFDB-E086-431D-B59F-4A15801A1594}']
    function RNG(int: Integer): TJavaArray<Byte>; cdecl;
  end;
  TJICRYPT = class(TJavaGenericImport<JICRYPTClass, JICRYPT>) end;

  JGEDI_MSR_e_StatusClass = interface(JEnumClass)
    ['{AB06DB34-3FEE-43D0-A523-69604DCC47B5}']
    {class} function _GetABSENT_TRACK: JGEDI_MSR_e_Status; cdecl;
    {class} function _GetBUF_OVERFLOW: JGEDI_MSR_e_Status; cdecl;
    {class} function _GetETX_NOT_FOUND: JGEDI_MSR_e_Status; cdecl;
    {class} function _GetLRC_ERR: JGEDI_MSR_e_Status; cdecl;
    {class} function _GetLRC_NOT_FOUND: JGEDI_MSR_e_Status; cdecl;
    {class} function _GetNO_DATA: JGEDI_MSR_e_Status; cdecl;
    {class} function _GetSTX_NOT_FOUND: JGEDI_MSR_e_Status; cdecl;
    {class} function _GetSUCCESS: JGEDI_MSR_e_Status; cdecl;
    {class} function _GetUNKNOWN_CHAR: JGEDI_MSR_e_Status; cdecl;
    {class} function valueOf(int: Integer): JGEDI_MSR_e_Status; overload; cdecl;
    {class} function valueOf(string_1: JString): JGEDI_MSR_e_Status; overload; cdecl;
    {class} function values: TJavaObjectArray<JGEDI_MSR_e_Status>; cdecl;
    {class} property ABSENT_TRACK: JGEDI_MSR_e_Status read _GetABSENT_TRACK;
    {class} property BUF_OVERFLOW: JGEDI_MSR_e_Status read _GetBUF_OVERFLOW;
    {class} property ETX_NOT_FOUND: JGEDI_MSR_e_Status read _GetETX_NOT_FOUND;
    {class} property LRC_ERR: JGEDI_MSR_e_Status read _GetLRC_ERR;
    {class} property LRC_NOT_FOUND: JGEDI_MSR_e_Status read _GetLRC_NOT_FOUND;
    {class} property NO_DATA: JGEDI_MSR_e_Status read _GetNO_DATA;
    {class} property STX_NOT_FOUND: JGEDI_MSR_e_Status read _GetSTX_NOT_FOUND;
    {class} property SUCCESS: JGEDI_MSR_e_Status read _GetSUCCESS;
    {class} property UNKNOWN_CHAR: JGEDI_MSR_e_Status read _GetUNKNOWN_CHAR;
  end;

  [JavaSignature('br/com/gertec/gedi/enums/GEDI_MSR_e_Status')]
  JGEDI_MSR_e_Status = interface(JEnum)
    ['{1B66C1EE-B849-42A0-BA9C-EE6B76FA82DE}']
    function getValue: Integer; cdecl;
  end;
  TJGEDI_MSR_e_Status = class(TJavaGenericImport<JGEDI_MSR_e_StatusClass, JGEDI_MSR_e_Status>) end;

  JGEDI_SMART_e_VoltageClass = interface(JEnumClass)
    ['{79CF68A7-0A78-4FD2-AA03-2C69B5B0A168}']
    {class} function _GetVOLTAGE_1_8V: JGEDI_SMART_e_Voltage; cdecl;
    {class} function _GetVOLTAGE_3V: JGEDI_SMART_e_Voltage; cdecl;
    {class} function _GetVOLTAGE_5V: JGEDI_SMART_e_Voltage; cdecl;
    {class} function valueOf(string_1: JString): JGEDI_SMART_e_Voltage; overload; cdecl;
    {class} function valueOf(int: Integer): JGEDI_SMART_e_Voltage; overload; cdecl;
    {class} function values: TJavaObjectArray<JGEDI_SMART_e_Voltage>; cdecl;
    {class} property VOLTAGE_1_8V: JGEDI_SMART_e_Voltage read _GetVOLTAGE_1_8V;
    {class} property VOLTAGE_3V: JGEDI_SMART_e_Voltage read _GetVOLTAGE_3V;
    {class} property VOLTAGE_5V: JGEDI_SMART_e_Voltage read _GetVOLTAGE_5V;
  end;

  [JavaSignature('br/com/gertec/gedi/enums/GEDI_SMART_e_Voltage')]
  JGEDI_SMART_e_Voltage = interface(JEnum)
    ['{13822BE3-9047-4DD5-9620-04954B85C4C3}']
    function getValue: Integer; cdecl;
  end;
  TJGEDI_SMART_e_Voltage = class(TJavaGenericImport<JGEDI_SMART_e_VoltageClass, JGEDI_SMART_e_Voltage>) end;

  JPrinter_BarcodeTypeClass = interface(JEnumClass)
    ['{4A5710C1-6F54-408E-8401-B8AE86CBD49B}']
    {class} function _GetAZTEC: JPrinter_BarcodeType; cdecl;
    {class} function _GetCODABAR: JPrinter_BarcodeType; cdecl;
    {class} function _GetCODE_128: JPrinter_BarcodeType; cdecl;
    {class} function _GetCODE_39: JPrinter_BarcodeType; cdecl;
    {class} function _GetCODE_93: JPrinter_BarcodeType; cdecl;
    {class} function _GetDATA_MATRIX: JPrinter_BarcodeType; cdecl;
    {class} function _GetEAN_13: JPrinter_BarcodeType; cdecl;
    {class} function _GetEAN_8: JPrinter_BarcodeType; cdecl;
    {class} function _GetITF: JPrinter_BarcodeType; cdecl;
    {class} function _GetMAXICODE: JPrinter_BarcodeType; cdecl;
    {class} function _GetPDF_417: JPrinter_BarcodeType; cdecl;
    {class} function _GetQR_CODE: JPrinter_BarcodeType; cdecl;
    {class} function _GetRSS_14: JPrinter_BarcodeType; cdecl;
    {class} function _GetRSS_EXPANDED: JPrinter_BarcodeType; cdecl;
    {class} function _GetUPC_A: JPrinter_BarcodeType; cdecl;
    {class} function _GetUPC_E: JPrinter_BarcodeType; cdecl;
    {class} function _GetUPC_EAN_EXTENSION: JPrinter_BarcodeType; cdecl;
    {class} function valueOf(string_1: JString): JPrinter_BarcodeType; cdecl;
    {class} function values: TJavaObjectArray<JPrinter_BarcodeType>; cdecl;
    {class} property AZTEC: JPrinter_BarcodeType read _GetAZTEC;
    {class} property CODABAR: JPrinter_BarcodeType read _GetCODABAR;
    {class} property CODE_128: JPrinter_BarcodeType read _GetCODE_128;
    {class} property CODE_39: JPrinter_BarcodeType read _GetCODE_39;
    {class} property CODE_93: JPrinter_BarcodeType read _GetCODE_93;
    {class} property DATA_MATRIX: JPrinter_BarcodeType read _GetDATA_MATRIX;
    {class} property EAN_13: JPrinter_BarcodeType read _GetEAN_13;
    {class} property EAN_8: JPrinter_BarcodeType read _GetEAN_8;
    {class} property ITF: JPrinter_BarcodeType read _GetITF;
    {class} property MAXICODE: JPrinter_BarcodeType read _GetMAXICODE;
    {class} property PDF_417: JPrinter_BarcodeType read _GetPDF_417;
    {class} property QR_CODE: JPrinter_BarcodeType read _GetQR_CODE;
    {class} property RSS_14: JPrinter_BarcodeType read _GetRSS_14;
    {class} property RSS_EXPANDED: JPrinter_BarcodeType read _GetRSS_EXPANDED;
    {class} property UPC_A: JPrinter_BarcodeType read _GetUPC_A;
    {class} property UPC_E: JPrinter_BarcodeType read _GetUPC_E;
    {class} property UPC_EAN_EXTENSION: JPrinter_BarcodeType read _GetUPC_EAN_EXTENSION;
  end;

  [JavaSignature('wangpos/sdk4/libbasebinder/Printer$BarcodeType')]
  JPrinter_BarcodeType = interface(JEnum)
    ['{7AF86C65-477C-41FA-B113-83D893B5C889}']
  end;
  TJPrinter_BarcodeType = class(TJavaGenericImport<JPrinter_BarcodeTypeClass, JPrinter_BarcodeType>) end;

  Jgedi_eClass = interface(JObjectClass)
    ['{F1ED8839-B8CE-4AA6-9F30-C96D1FBAAF59}']
    {class} function init: Jgedi_e; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/e')]
  Jgedi_e = interface(JObject)
    ['{C0AD809C-E692-4504-9EC1-921BE21A0AC7}']
    function ControlNumberGet(gEDI_INFO_e_ControlNumber: JGEDI_INFO_e_ControlNumber): JString; cdecl;
    function FirmwareVersionGet: JString; cdecl;
    function ImgVersionNumberGet: Integer; cdecl;
    function ModuleVersionGet(gEDI_INFO_e_Module: JGEDI_INFO_e_Module): JString; cdecl;
    function ProductNameGet: JString; cdecl;
    function a(int: Integer; bytes: TJavaArray<Byte>): Integer; overload; cdecl;
    function a(bytes: TJavaArray<Byte>): Integer; overload; cdecl;
    procedure a(gEDI_INFO_e_ControlNumber: JGEDI_INFO_e_ControlNumber; string_1: JString); overload; cdecl;
    function a(gEDI_INFO_e_ModuleEng: JGEDI_INFO_e_ModuleEng): JString; overload; cdecl;
    function b(int: Integer; bytes: TJavaArray<Byte>): Integer; cdecl;
  end;
  TJgedi_e = class(TJavaGenericImport<Jgedi_eClass, Jgedi_e>) end;

  JGEDI_PRNTR_e_StatusClass = interface(JEnumClass)
    ['{4264A89B-5464-4DAB-AF27-6D349B554759}']
    {class} function _GetOK: JGEDI_PRNTR_e_Status; cdecl;
    {class} function _GetOUT_OF_PAPER: JGEDI_PRNTR_e_Status; cdecl;
    {class} function _GetOVERHEAT: JGEDI_PRNTR_e_Status; cdecl;
    {class} function _GetUNKNOWN_ERROR: JGEDI_PRNTR_e_Status; cdecl;
    {class} function valueOf(int: Integer): JGEDI_PRNTR_e_Status; overload; cdecl;
    {class} function valueOf(string_1: JString): JGEDI_PRNTR_e_Status; overload; cdecl;
    {class} function values: TJavaObjectArray<JGEDI_PRNTR_e_Status>; cdecl;
    {class} property OK: JGEDI_PRNTR_e_Status read _GetOK;
    {class} property OUT_OF_PAPER: JGEDI_PRNTR_e_Status read _GetOUT_OF_PAPER;
    {class} property OVERHEAT: JGEDI_PRNTR_e_Status read _GetOVERHEAT;
    {class} property UNKNOWN_ERROR: JGEDI_PRNTR_e_Status read _GetUNKNOWN_ERROR;
  end;

  [JavaSignature('br/com/gertec/gedi/enums/GEDI_PRNTR_e_Status')]
  JGEDI_PRNTR_e_Status = interface(JEnum)
    ['{DF785DBF-CCF8-403E-9239-920384C8F5FE}']
    function getValue: Integer; cdecl;
  end;
  TJGEDI_PRNTR_e_Status = class(TJavaGenericImport<JGEDI_PRNTR_e_StatusClass, JGEDI_PRNTR_e_Status>) end;

  JGEDI_CL_st_ResetEMVInfoClass = interface(JObjectClass)
    ['{48099188-1586-4A73-AEA1-BAF86137B023}']
    {class} function init: JGEDI_CL_st_ResetEMVInfo; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_CL_st_ResetEMVInfo')]
  JGEDI_CL_st_ResetEMVInfo = interface(JObject)
    ['{727DF733-31DE-429C-9B82-A128E208200E}']
    function _GetabATR: TJavaArray<Byte>; cdecl;
    function _GetpeCardType: JGEDI_CL_e_ISO_Type; cdecl;
    property abATR: TJavaArray<Byte> read _GetabATR;
    property peCardType: JGEDI_CL_e_ISO_Type read _GetpeCardType;
  end;
  TJGEDI_CL_st_ResetEMVInfo = class(TJavaGenericImport<JGEDI_CL_st_ResetEMVInfoClass, JGEDI_CL_st_ResetEMVInfo>) end;

  JlClass = interface(Jgedi_lClass)
    ['{92FF09B7-6234-4EEF-97C9-DE5A0828551B}']
  end;

  [JavaSignature('gedi/l')]
  Jl = interface(Jgedi_l)
    ['{41410B34-FD7F-472B-B618-722FD72626B6}']
    procedure DrawBarCode(gEDI_PRNTR_st_BarCodeConfig: JGEDI_PRNTR_st_BarCodeConfig; string_1: JString); cdecl;
    procedure DrawBlankLine(int: Integer); cdecl;
    procedure DrawPictureExt(gEDI_PRNTR_st_PictureConfig: JGEDI_PRNTR_st_PictureConfig; bitmap: JBitmap); cdecl;
    procedure DrawStringExt(gEDI_PRNTR_st_StringConfig: JGEDI_PRNTR_st_StringConfig; string_1: JString); cdecl;
    function GetPaperUsage: Integer; cdecl;
    procedure Init; cdecl;
    procedure Output; cdecl;
    procedure ResetPaperUsage; cdecl;
    procedure SetPrintDensity(gEDI_PRNTR_e_PrintDensity: JGEDI_PRNTR_e_PrintDensity); cdecl;
    function Status: JGEDI_PRNTR_e_Status; cdecl;
    procedure a(gEDI_PRNTR_st_StringConfig: JGEDI_PRNTR_st_StringConfig; string_1: JString); overload; cdecl;
    procedure a(gEDI_PRNTR_st_PictureConfig: JGEDI_PRNTR_st_PictureConfig; bitmap: JBitmap); overload; cdecl;
    procedure a(int: Integer); overload; cdecl;
    procedure a(gEDI_PRNTR_st_BarCodeConfig: JGEDI_PRNTR_st_BarCodeConfig; string_1: JString); overload; cdecl;
  end;
  TJl = class(TJavaGenericImport<JlClass, Jl>) end;

  JGEDI_KMS_st_DataClass = interface(JObjectClass)
    ['{957CB661-2B37-49D1-82D2-C8AC8A67B2D9}']
    {class} function init(byte: Byte; gEDI_KMS_e_OP_1: JGEDI_KMS_e_OP; gEDI_KMS_e_KEYTYPE: JGEDI_KMS_e_KEYTYPE; int: Integer; gEDI_KMS_e_EncMode: JGEDI_KMS_e_EncMode; bytes: TJavaArray<Byte>; bytes_1: TJavaArray<Byte>; bytes_2: TJavaArray<Byte>): JGEDI_KMS_st_Data; overload; cdecl;
    {class} function init: JGEDI_KMS_st_Data; overload; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_KMS_st_Data')]
  JGEDI_KMS_st_Data = interface(JObject)
    ['{FB953B91-CF08-465A-B5CD-EC7CEC243136}']
    function _GetabICV: TJavaArray<Byte>; cdecl;
    function _GetabInput: TJavaArray<Byte>; cdecl;
    function _GetabKSN: TJavaArray<Byte>; cdecl;
    function _GetabOutput: TJavaArray<Byte>; cdecl;
    function _GetabSK: TJavaArray<Byte>; cdecl;
    function _GetbVersion: Byte; cdecl;
    function _GeteKeyType: JGEDI_KMS_e_KEYTYPE; cdecl;
    function _GeteMode: JGEDI_KMS_e_EncMode; cdecl;
    function _GeteOperation: JGEDI_KMS_e_OP; cdecl;
    function _GetuiKeyIndex: Integer; cdecl;
    property abICV: TJavaArray<Byte> read _GetabICV;
    property abInput: TJavaArray<Byte> read _GetabInput;
    property abKSN: TJavaArray<Byte> read _GetabKSN;
    property abOutput: TJavaArray<Byte> read _GetabOutput;
    property abSK: TJavaArray<Byte> read _GetabSK;
    property bVersion: Byte read _GetbVersion;
    property eKeyType: JGEDI_KMS_e_KEYTYPE read _GeteKeyType;
    property eMode: JGEDI_KMS_e_EncMode read _GeteMode;
    property eOperation: JGEDI_KMS_e_OP read _GeteOperation;
    property uiKeyIndex: Integer read _GetuiKeyIndex;
  end;
  TJGEDI_KMS_st_Data = class(TJavaGenericImport<JGEDI_KMS_st_DataClass, JGEDI_KMS_st_Data>) end;

  JbClass = interface(JObjectClass)
    ['{4FCFC5F0-704F-4931-9E71-F99E54E73499}']
    {class} function a: Jlang_Class; overload; cdecl;
    {class} function a(string_1: JString): JIBinder; overload; cdecl;
  end;

  [JavaSignature('wangpos/sdk4/libbasebinder/b')]
  Jb = interface(JObject)
    ['{A3223DE2-1FDD-4881-91E1-1BB60B6D0DDD}']
  end;
  TJb = class(TJavaGenericImport<JbClass, Jb>) end;

  JPrinter_BarcodeWidthClass = interface(JEnumClass)
    ['{CE33101B-D731-408E-B91A-033E1BA1C997}']
    {class} function _GetHUGE: JPrinter_BarcodeWidth; cdecl;
    {class} function _GetLARGE: JPrinter_BarcodeWidth; cdecl;
    {class} function _GetNORMAL: JPrinter_BarcodeWidth; cdecl;
    {class} function _GetSMALL: JPrinter_BarcodeWidth; cdecl;
    {class} function valueOf(string_1: JString): JPrinter_BarcodeWidth; cdecl;
    {class} function values: TJavaObjectArray<JPrinter_BarcodeWidth>; cdecl;
    {class} property HUGE: JPrinter_BarcodeWidth read _GetHUGE;
    {class} property LARGE: JPrinter_BarcodeWidth read _GetLARGE;
    {class} property NORMAL: JPrinter_BarcodeWidth read _GetNORMAL;
    {class} property SMALL: JPrinter_BarcodeWidth read _GetSMALL;
  end;

  [JavaSignature('wangpos/sdk4/libbasebinder/Printer$BarcodeWidth')]
  JPrinter_BarcodeWidth = interface(JEnum)
    ['{2CDC48C5-4C5C-4D44-A356-B680200430AE}']
  end;
  TJPrinter_BarcodeWidth = class(TJavaGenericImport<JPrinter_BarcodeWidthClass, JPrinter_BarcodeWidth>) end;

  JaClass = interface(JObjectClass)
    ['{ED84C940-B72F-4A63-BEAF-644AD67EE90F}']
  end;

  [JavaSignature('gedi/a')]
  Ja = interface(JObject)
    ['{2B9BE9AE-8865-41D7-A798-2A9B4815BDAB}']
    procedure Beep; cdecl;
  end;
  TJa = class(TJavaGenericImport<JaClass, Ja>) end;

  JGEDI_KMS_st_KBClass = interface(JObjectClass)
    ['{C2988DD5-5EDF-4CB3-AFA7-8CB854B589B1}']
    {class} function init: JGEDI_KMS_st_KB; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_KMS_st_KB')]
  JGEDI_KMS_st_KB = interface(JObject)
    ['{886FCB4A-08C6-4FD1-829E-ACB87DB5871B}']
    function _GetabKCV: TJavaArray<Byte>; cdecl;
    function _GetabKEKIndex: TJavaArray<Byte>; cdecl;
    function _GetabKSN: TJavaArray<Byte>; cdecl;
    function _GetabKey: TJavaArray<Byte>; cdecl;
    function _GetabKeyIndex: TJavaArray<Byte>; cdecl;
    function _GetabKeyLen: TJavaArray<Byte>; cdecl;
    function _GetabRND: TJavaArray<Byte>; cdecl;
    function _GetbKEKType: JGEDI_KMS_e_KEYTYPE; cdecl;
    function _GetbKeyPurpose: JGEDI_KMS_e_KEYPURPOSE; cdecl;
    function _GetbKeyType: JGEDI_KMS_e_KEYTYPE; cdecl;
    property abKCV: TJavaArray<Byte> read _GetabKCV;
    property abKEKIndex: TJavaArray<Byte> read _GetabKEKIndex;
    property abKSN: TJavaArray<Byte> read _GetabKSN;
    property abKey: TJavaArray<Byte> read _GetabKey;
    property abKeyIndex: TJavaArray<Byte> read _GetabKeyIndex;
    property abKeyLen: TJavaArray<Byte> read _GetabKeyLen;
    property abRND: TJavaArray<Byte> read _GetabRND;
    property bKEKType: JGEDI_KMS_e_KEYTYPE read _GetbKEKType;
    property bKeyPurpose: JGEDI_KMS_e_KEYPURPOSE read _GetbKeyPurpose;
    property bKeyType: JGEDI_KMS_e_KEYTYPE read _GetbKeyType;
  end;
  TJGEDI_KMS_st_KB = class(TJavaGenericImport<JGEDI_KMS_st_KBClass, JGEDI_KMS_st_KB>) end;

  JGEDI_PRNTR_e_PrintDensityClass = interface(JEnumClass)
    ['{098EE6D9-E139-4858-80D8-F5BAAD77EC30}']
    {class} function _GetHIGH: JGEDI_PRNTR_e_PrintDensity; cdecl;
    {class} function _GetLOW: JGEDI_PRNTR_e_PrintDensity; cdecl;
    {class} function _GetMEDIUM: JGEDI_PRNTR_e_PrintDensity; cdecl;
    {class} function valueOf(string_1: JString): JGEDI_PRNTR_e_PrintDensity; cdecl;
    {class} function values: TJavaObjectArray<JGEDI_PRNTR_e_PrintDensity>; cdecl;
    {class} property HIGH: JGEDI_PRNTR_e_PrintDensity read _GetHIGH;
    {class} property LOW: JGEDI_PRNTR_e_PrintDensity read _GetLOW;
    {class} property MEDIUM: JGEDI_PRNTR_e_PrintDensity read _GetMEDIUM;
  end;

  [JavaSignature('br/com/gertec/gedi/enums/GEDI_PRNTR_e_PrintDensity')]
  JGEDI_PRNTR_e_PrintDensity = interface(JEnum)
    ['{D3089602-270F-48EA-8D6E-D3B7885195AC}']
    function getValue: Integer; cdecl;
  end;
  TJGEDI_PRNTR_e_PrintDensity = class(TJavaGenericImport<JGEDI_PRNTR_e_PrintDensityClass, JGEDI_PRNTR_e_PrintDensity>) end;

  JcClass = interface(JObjectClass)
    ['{61C780C0-1C2E-45E1-B55E-BE71F7857A8A}']
    {class} function a(bytes: TJavaArray<Byte>): JString; cdecl;
  end;

  [JavaSignature('wangpos/sdk4/libbasebinder/c')]
  Jc = interface(JObject)
    ['{73162315-1A5A-4091-AF77-278708D27024}']
  end;
  TJc = class(TJavaGenericImport<JcClass, Jc>) end;

  JdClass = interface(JObjectClass)
    ['{80C1D927-760F-49AC-8DAF-B76E54E57A13}']
    {class} function init: Jd; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/d')]
  Jd = interface(JObject)
    ['{C10BA996-A314-4523-B2C2-003B4D99586E}']
    function RNG(int: Integer): TJavaArray<Byte>; cdecl;
    function a(bytes: TJavaArray<Byte>; bytes_1: TJavaArray<Byte>): Integer; overload; cdecl;
    function a(gEDI_CRYPT_e_Op: JGEDI_CRYPT_e_Op; bytes: TJavaArray<Byte>; bytes_1: TJavaArray<Byte>): TJavaArray<Byte>; overload; cdecl;
    function a(gEDI_CRYPT_e_Op: JGEDI_CRYPT_e_Op; bytes: TJavaArray<Byte>; int: Integer; int_1: Integer; bytes_1: TJavaArray<Byte>; int_2: Integer; int_3: Integer; bytes_2: TJavaArray<Byte>): Integer; overload; cdecl;
    function a(bytes: TJavaArray<Byte>): Integer; overload; cdecl;
    function a(bytes: TJavaArray<Byte>; bytes_1: TJavaArray<Byte>; bytes_2: TJavaArray<Byte>; bytes_3: TJavaArray<Byte>): Integer; overload; cdecl;
    function a(bytes: TJavaArray<Byte>; bytes_1: TJavaArray<Byte>; bytes_2: TJavaArray<Byte>): TJavaArray<Byte>; overload; cdecl;
    function b(gEDI_CRYPT_e_Op: JGEDI_CRYPT_e_Op; bytes: TJavaArray<Byte>; bytes_1: TJavaArray<Byte>): TJavaArray<Byte>; overload; cdecl;
    function b(bytes: TJavaArray<Byte>): TJavaArray<Byte>; overload; cdecl;
  end;
  TJd = class(TJavaGenericImport<JdClass, Jd>) end;

  JPrinter_FontClass = interface(JEnumClass)
    ['{67E91293-F503-44DD-91EF-C8B28392D583}']
    {class} function _GetDEFAULT: JPrinter_Font; cdecl;
    {class} function _GetDEFAULT_BOLD: JPrinter_Font; cdecl;
    {class} function _GetMONOSPACE: JPrinter_Font; cdecl;
    {class} function _GetSANS_SERIF: JPrinter_Font; cdecl;
    {class} function _GetSERIF: JPrinter_Font; cdecl;
    {class} function _GetSONG: JPrinter_Font; cdecl;
    {class} function valueOf(string_1: JString): JPrinter_Font; cdecl;
    {class} function values: TJavaObjectArray<JPrinter_Font>; cdecl;
    {class} property &DEFAULT: JPrinter_Font read _GetDEFAULT;
    {class} property DEFAULT_BOLD: JPrinter_Font read _GetDEFAULT_BOLD;
    {class} property MONOSPACE: JPrinter_Font read _GetMONOSPACE;
    {class} property SANS_SERIF: JPrinter_Font read _GetSANS_SERIF;
    {class} property SERIF: JPrinter_Font read _GetSERIF;
    {class} property SONG: JPrinter_Font read _GetSONG;
  end;

  [JavaSignature('wangpos/sdk4/libbasebinder/Printer$Font')]
  JPrinter_Font = interface(JEnum)
    ['{63C2D38B-EBDF-4F85-9FFB-D4FEDE83F24B}']
  end;
  TJPrinter_Font = class(TJavaGenericImport<JPrinter_FontClass, JPrinter_Font>) end;

  JGEDI_PRNTR_e_AlignmentClass = interface(JEnumClass)
    ['{CF5D50A5-860B-4260-9FB9-9D7460676CD4}']
    {class} function _GetCENTER: JGEDI_PRNTR_e_Alignment; cdecl;
    {class} function _GetLEFT: JGEDI_PRNTR_e_Alignment; cdecl;
    {class} function _GetRIGHT: JGEDI_PRNTR_e_Alignment; cdecl;
    {class} function valueOf(int: Integer): JGEDI_PRNTR_e_Alignment; overload; cdecl;
    {class} function valueOf(string_1: JString): JGEDI_PRNTR_e_Alignment; overload; cdecl;
    {class} function values: TJavaObjectArray<JGEDI_PRNTR_e_Alignment>; cdecl;
    {class} property CENTER: JGEDI_PRNTR_e_Alignment read _GetCENTER;
    {class} property LEFT: JGEDI_PRNTR_e_Alignment read _GetLEFT;
    {class} property RIGHT: JGEDI_PRNTR_e_Alignment read _GetRIGHT;
  end;

  [JavaSignature('br/com/gertec/gedi/enums/GEDI_PRNTR_e_Alignment')]
  JGEDI_PRNTR_e_Alignment = interface(JEnum)
    ['{65F94923-4FC8-4F68-A40B-DBC544BA3B4B}']
    function getValue: Integer; cdecl;
  end;
  TJGEDI_PRNTR_e_Alignment = class(TJavaGenericImport<JGEDI_PRNTR_e_AlignmentClass, JGEDI_PRNTR_e_Alignment>) end;

  JIPMClass = interface(IJavaClass)
    ['{E133BFAF-4125-44D9-A2D5-41E642ABD9E6}']
  end;

  [JavaSignature('br/com/gertec/gedi/interfaces/IPM')]
  JIPM = interface(IJavaInstance)
    ['{25FD4D48-4FA7-489E-84FB-DBFCA201104F}']
    procedure ApDefaultSet(string_1: JString); cdecl;
    procedure ApDelete(string_1: JString); cdecl;
    function GetNameCrt(gEDI_PM_e_TypeCrt: JGEDI_PM_e_TypeCrt): JString; cdecl;
    procedure UpdateFromFile(string_1: JString; gEDI_FS_e_Storage: JGEDI_FS_e_Storage); cdecl;
  end;
  TJIPM = class(TJavaGenericImport<JIPMClass, JIPM>) end;

  JjClass = interface(JObjectClass)
    ['{95FA95A6-CFFD-4003-A2A6-7BAA64D93D8C}']
    {class} function init: Jj; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/j')]
  Jj = interface(JObject)
    ['{6A697470-1B8B-493F-B5ED-B4CB389CF953}']
    function LastErrorGet: JGEDI_MSR_st_LastErrors; cdecl;
    function Read: JGEDI_MSR_st_Tracks; cdecl;
    function a(ints: TJavaArray<Integer>; ints_1: TJavaArray<Integer>; ints_2: TJavaArray<Integer>): Integer; overload; cdecl;
    function a(bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>; bytes_1: TJavaArray<Byte>; ints_1: TJavaArray<Integer>; bytes_2: TJavaArray<Byte>; ints_2: TJavaArray<Integer>): Integer; overload; cdecl;
  end;
  TJj = class(TJavaGenericImport<JjClass, Jj>) end;

  JkClass = interface(Jgedi_kClass)
    ['{6A19EEC3-59A8-4A26-BB67-7B3A2B53A65F}']
  end;

  [JavaSignature('gedi/k')]
  Jk = interface(Jgedi_k)
    ['{906E4138-71B0-4B9D-906B-E8D82D9769AB}']
    procedure ApDefaultSet(string_1: JString); cdecl;
    procedure ApDelete(string_1: JString); cdecl;
    function GetNameCrt(gEDI_PM_e_TypeCrt: JGEDI_PM_e_TypeCrt): JString; cdecl;
    procedure UpdateFromFile(string_1: JString; gEDI_FS_e_Storage: JGEDI_FS_e_Storage); cdecl;
    function a: JX509Certificate; cdecl;
    function b: JX509Certificate; cdecl;
    function c: JX509Certificate; cdecl;
  end;
  TJk = class(TJavaGenericImport<JkClass, Jk>) end;

  JGEDI_PRNTR_st_PictureConfigClass = interface(JObjectClass)
    ['{1137DBC0-6873-438E-B1E7-91D487328A72}']
    {class} function init(gEDI_PRNTR_e_Alignment: JGEDI_PRNTR_e_Alignment; int: Integer; int_1: Integer; int_2: Integer): JGEDI_PRNTR_st_PictureConfig; overload; cdecl;
    {class} function init(gEDI_PRNTR_e_Alignment: JGEDI_PRNTR_e_Alignment): JGEDI_PRNTR_st_PictureConfig; overload; cdecl;
    {class} function init: JGEDI_PRNTR_st_PictureConfig; overload; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_PRNTR_st_PictureConfig')]
  JGEDI_PRNTR_st_PictureConfig = interface(JObject)
    ['{94064F6F-F5F1-4539-BD81-9D4B2501218E}']
    function _Getalignment: JGEDI_PRNTR_e_Alignment; cdecl;
    function _Getheight: Integer; cdecl;
    function _Getoffset: Integer; cdecl;
    function _Getwidth: Integer; cdecl;
    function clone: JGEDI_PRNTR_st_PictureConfig; overload; cdecl;
//Mesmo    function clone: JObject; overload; cdecl;
    property alignment: JGEDI_PRNTR_e_Alignment read _Getalignment;
    property height: Integer read _Getheight;
    property offset: Integer read _Getoffset;
    property width: Integer read _Getwidth;
  end;
  TJGEDI_PRNTR_st_PictureConfig = class(TJavaGenericImport<JGEDI_PRNTR_st_PictureConfigClass, JGEDI_PRNTR_st_PictureConfig>) end;

  JIAUDIOClass = interface(IJavaClass)
    ['{BA765D31-7091-473C-9D28-D7791AC6E1D2}']
  end;

  [JavaSignature('br/com/gertec/gedi/interfaces/IAUDIO')]
  JIAUDIO = interface(IJavaInstance)
    ['{D52AD8A0-16D3-4513-82B0-C88A1234DD7F}']
    procedure Beep; cdecl;
  end;
  TJIAUDIO = class(TJavaGenericImport<JIAUDIOClass, JIAUDIO>) end;

  JqClass = interface(JIInterfaceClass)
    ['{E1A0C078-D8E6-4EEC-BC0C-67BC304DFAA5}']
  end;

  [JavaSignature('gedi/q')]
  Jq = interface(JIInterface)
    ['{DC836204-8A93-4230-BDA0-3CC472580A57}']
    function a(int: Integer; int_1: Integer; int_2: Integer; bytes: TJavaArray<Byte>; bytes_1: TJavaArray<Byte>; boolean: Boolean; int_3: Integer; bytes_2: TJavaArray<Byte>; string_1: JString; int_4: Integer): Integer; cdecl;
  end;
  TJq = class(TJavaGenericImport<JqClass, Jq>) end;

  JGEDI_AUTH_st_DataClass = interface(JObjectClass)
    ['{5809BFC6-7A53-45DB-840E-63606F3C7BB8}']
    {class} function init: JGEDI_AUTH_st_Data; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_AUTH_st_Data')]
  JGEDI_AUTH_st_Data = interface(JObject)
    ['{B0C5F519-BC95-4B47-B585-F44C83436D68}']
    function _GetabHash: TJavaArray<Byte>; cdecl;
    function _GetabICV: TJavaArray<Byte>; cdecl;
    function _GetabInput: TJavaArray<Byte>; cdecl;
    function _GetabOutput: TJavaArray<Byte>; cdecl;
    function _GeteMode: Integer; cdecl;
    function _GeteOperation: Integer; cdecl;
    function _GetuiInputLen: Integer; cdecl;
    function _GetuiKeyIndex: Integer; cdecl;
    property abHash: TJavaArray<Byte> read _GetabHash;
    property abICV: TJavaArray<Byte> read _GetabICV;
    property abInput: TJavaArray<Byte> read _GetabInput;
    property abOutput: TJavaArray<Byte> read _GetabOutput;
    property eMode: Integer read _GeteMode;
    property eOperation: Integer read _GeteOperation;
    property uiInputLen: Integer read _GetuiInputLen;
    property uiKeyIndex: Integer read _GetuiKeyIndex;
  end;
  TJGEDI_AUTH_st_Data = class(TJavaGenericImport<JGEDI_AUTH_st_DataClass, JGEDI_AUTH_st_Data>) end;

  Je0Class = interface(Jd0Class)
    ['{C9DA64C0-E8F8-4C66-BAD8-1D0991D5BF07}']
    {class} function init(context: JContext): Je0; cdecl;
  end;

  [JavaSignature('gedi/e0')]
  Je0 = interface(Jd0)
    ['{BB75AFCB-19D3-41EC-BCE4-4E59AC3324C6}']
    function a(string_1: JString; int: Integer; bytes: TJavaArray<Byte>): Integer; overload; cdecl;
    function a(string_1: JString; bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function a(string_1: JString): Integer; overload; cdecl;
    function a(string_1: JString; int: Integer; int_1: Integer; bytes: TJavaArray<Byte>; ints: TJavaArray<Integer>): Integer; overload; cdecl;
    function a(int: Integer; strings: TJavaObjectArray<JString>): Integer; overload; cdecl;
    function a(string_1: JString; int: Integer): Integer; overload; cdecl;
    function a(int: Integer; int_1: Integer; int_2: Integer; bytes: TJavaArray<Byte>; bytes_1: TJavaArray<Byte>; boolean: Boolean; int_3: Integer; bytes_2: TJavaArray<Byte>; string_1: JString; int_4: Integer): Integer; overload; cdecl;
    function b(int: Integer; int_1: Integer; int_2: Integer; bytes: TJavaArray<Byte>; bytes_1: TJavaArray<Byte>; boolean: Boolean; int_3: Integer; bytes_2: TJavaArray<Byte>; string_1: JString; int_4: Integer): Integer; cdecl;
    procedure c(string_1: JString); overload; cdecl;
    function c: Integer; overload; cdecl;
  end;
  TJe0 = class(TJavaGenericImport<Je0Class, Je0>) end;

  Jc0Class = interface(JObjectClass)
    ['{5EDCE2CE-7D0D-4E08-9F89-80FC957A61CA}']
  end;

  [JavaSignature('gedi/c0')]
  Jc0 = interface(JObject)
    ['{08C3BF97-71BA-47AA-BD1C-8731F7F7A683}']
    function _Geta: TJavaArray<Byte>; cdecl;
    function _Getb: JButton; cdecl;
    function _Getc: JButton; cdecl;
    function _Getd: JButton; cdecl;
    function _Gete: JButton; cdecl;
    function _Getf: JButton; cdecl;
    function _Getg: JButton; cdecl;
    function _Geth: JButton; cdecl;
    function _Geti: JButton; cdecl;
    function _Getj: JButton; cdecl;
    function _Getk: JButton; cdecl;
    function _Getl: JButton; cdecl;
    function _Getm: JButton; cdecl;
    function _Getn: JButton; cdecl;
    function _Geto: JButton; cdecl;
    function _Getp: JActivity; cdecl;
    property a: TJavaArray<Byte> read _Geta;
    property b: JButton read _Getb;
    property c: JButton read _Getc;
    property d: JButton read _Getd;
    property e: JButton read _Gete;
    property f: JButton read _Getf;
    property g: JButton read _Getg;
    property h: JButton read _Geth;
    property i: JButton read _Geti;
    property j: JButton read _Getj;
    property k: JButton read _Getk;
    property l: JButton read _Getl;
    property m: JButton read _Getm;
    property n: JButton read _Getn;
    property o: JButton read _Geto;
    property p: JActivity read _Getp;
  end;
  TJc0 = class(TJavaGenericImport<Jc0Class, Jc0>) end;

  Ja0Class = interface(JIInterfaceClass)
    ['{5BE67784-6D39-41BA-9887-71F4C8EC92A3}']
  end;

  [JavaSignature('gedi/a0')]
  Ja0 = interface(JIInterface)
    ['{3BF07DB1-A3A7-41C1-BAF8-6C9662EA5D47}']
    function a(int: Integer): JIBinder; cdecl;
  end;
  TJa0 = class(TJavaGenericImport<Ja0Class, Ja0>) end;

  JGEDIClass = interface(JObjectClass)
    ['{205380A2-002D-423C-8549-F6E876B7EBAF}']
    {class} function getInstance(context: JContext): JIGEDI; overload; cdecl;
    {class} function getInstance: JIGEDI; overload; cdecl;
    {class} procedure init(context: JContext); cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/GEDI')]
  JGEDI = interface(JObject)
    ['{7F81263E-6AFF-47AC-B193-0C933E7A4EF6}']
    procedure EnterEng(string_1: JString); cdecl;
    function VersionGet: JString; cdecl;
    function getAUDIO: JIAUDIO; cdecl;
    function getCL: JICL; cdecl;
    function getCLOCK: JICLOCK; cdecl;
    function getCRYPT: JICRYPT; cdecl;
    function getINFO: JIINFO; cdecl;
    function getKBD: JIKBD; cdecl;
    function getKMS: JIKMS; cdecl;
    function getLED: JILED; cdecl;
    function getMSR: JIMSR; cdecl;
    function getPM: JIPM; cdecl;
    function getPRNTR: JIPRNTR; cdecl;
    function getSMART: JISMART; cdecl;
    function getSYS: JISYS; cdecl;
  end;
  TJGEDI = class(TJavaGenericImport<JGEDIClass, JGEDI>) end;

  Jgedi_iClass = interface(JiClass)
    ['{B744B9B2-3F93-42F5-8D68-6FE7B6F9BAD9}']
  end;

  [JavaSignature('gedi/i')]
  Jgedi_i = interface(Ji)
    ['{DAEFD368-F350-488D-9189-08FE87F6E862}']
    procedure &Set(gEDI_LED_e_Id: JGEDI_LED_e_Id; boolean: Boolean); cdecl;
  end;
  TJgedi_i = class(TJavaGenericImport<Jgedi_iClass, Jgedi_i>) end;

  Jgedi_mClass = interface(JmClass)
    ['{ADBF8BEB-489D-4EDB-BAE0-E6940BA9C11E}']
  end;

  [JavaSignature('gedi/m')]
  Jgedi_m = interface(Jm)
    ['{8B595042-DE28-4736-A8DC-F62E05ACBE88}']
    procedure PowerOff(gEDI_SMART_e_Slot: JGEDI_SMART_e_Slot); cdecl;
    function ResetEMV(gEDI_SMART_e_Slot: JGEDI_SMART_e_Slot; gEDI_SMART_e_Voltage: JGEDI_SMART_e_Voltage): JGEDI_SMART_st_ResetInfo; cdecl;
    function SendAPDU(gEDI_SMART_e_Slot: JGEDI_SMART_e_Slot; bytes: TJavaArray<Byte>): TJavaArray<Byte>; cdecl;
    function Status(gEDI_SMART_e_Slot: JGEDI_SMART_e_Slot): JGEDI_SMART_e_Status; cdecl;
    function WarmResetEMV(gEDI_SMART_e_Slot: JGEDI_SMART_e_Slot; gEDI_SMART_e_Voltage: JGEDI_SMART_e_Voltage): JGEDI_SMART_st_ResetInfo; cdecl;
  end;
  TJgedi_m = class(TJavaGenericImport<Jgedi_mClass, Jgedi_m>) end;

  Jgedi_dClass = interface(JdClass)
    ['{E198435E-9911-4483-B93E-52C12ACDDC5F}']
  end;

  [JavaSignature('gedi/d')]
  Jgedi_d = interface(Jd)
    ['{99AED09E-6501-4ADF-BC0E-B50272953E0C}']
    function RNG(int: Integer): TJavaArray<Byte>; cdecl;
  end;
  TJgedi_d = class(TJavaGenericImport<Jgedi_dClass, Jgedi_d>) end;

  JfClass = interface(Jgedi_eClass)
    ['{FC3246ED-6D86-44EC-A629-6293BA84B4F5}']
  end;

  [JavaSignature('gedi/f')]
  Jf = interface(Jgedi_e)
    ['{B5ABED0C-1904-4485-8862-619681BF37A4}']
    function ControlNumberGet(gEDI_INFO_e_ControlNumber: JGEDI_INFO_e_ControlNumber): JString; cdecl;
    function FirmwareVersionGet: JString; cdecl;
    function ImgVersionNumberGet: Integer; cdecl;
    function ModuleVersionGet(gEDI_INFO_e_Module: JGEDI_INFO_e_Module): JString; cdecl;
    function a(gEDI_INFO_e_ModuleEng: JGEDI_INFO_e_ModuleEng): JString; overload; cdecl;
    procedure a(gEDI_INFO_e_ControlNumber: JGEDI_INFO_e_ControlNumber; string_1: JString); overload; cdecl;
  end;
  TJf = class(TJavaGenericImport<JfClass, Jf>) end;

  JeClass = interface(JGEDIClass)
    ['{FA008986-5DAE-4A1B-A19B-0C05755C698A}']
    {class} function init(context: JContext): Je; cdecl;
  end;

  [JavaSignature('gedi/e')]
  Je = interface(JGEDI)
    ['{BC9F7753-CFEC-433B-84C3-5D99D1B7890B}']
    procedure EnterEng(string_1: JString); cdecl;
    function VersionGet: JString; cdecl;
    function c: Jgedi_b; cdecl;
    function d: Jgedi_c; cdecl;
    function e: Jd; cdecl;
    function f: Jgedi_e; cdecl;
    function g: Jgedi_f; cdecl;
    function getAUDIO: JIAUDIO; cdecl;
    function getCL: JICL; cdecl;
    function getCLOCK: JICLOCK; cdecl;
    function getCRYPT: JICRYPT; cdecl;
    function getINFO: JIINFO; cdecl;
    function getKBD: JIKBD; cdecl;
    function getKMS: JIKMS; cdecl;
    function getLED: JILED; cdecl;
    function getMSR: JIMSR; cdecl;
    function getPM: JIPM; cdecl;
    function getPRNTR: JIPRNTR; cdecl;
    function getSMART: JISMART; cdecl;
    function getSYS: JISYS; cdecl;
    function h: Jh; cdecl;
    function i: Ji; cdecl;
    function j: Jj; cdecl;
    function k: Jm; cdecl;
  end;
  TJe = class(TJavaGenericImport<JeClass, Je>) end;

implementation

end.
