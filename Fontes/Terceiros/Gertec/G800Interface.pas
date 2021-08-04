
unit G800Interface;

interface

uses
  Androidapi.JNIBridge,
  Androidapi.JNI.App,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Os,
  Androidapi.JNI.Util,
  Androidapi.JNI.Widget;

type
// ===== Forward declarations =====

  JAnimator = interface;//android.animation.Animator
  JAnimator_AnimatorListener = interface;//android.animation.Animator$AnimatorListener
  JAnimator_AnimatorPauseListener = interface;//android.animation.Animator$AnimatorPauseListener
  JKeyframe = interface;//android.animation.Keyframe
  JLayoutTransition = interface;//android.animation.LayoutTransition
  JLayoutTransition_TransitionListener = interface;//android.animation.LayoutTransition$TransitionListener
  JPropertyValuesHolder = interface;//android.animation.PropertyValuesHolder
  JStateListAnimator = interface;//android.animation.StateListAnimator
  JTimeInterpolator = interface;//android.animation.TimeInterpolator
  JTypeConverter = interface;//android.animation.TypeConverter
  JTypeEvaluator = interface;//android.animation.TypeEvaluator
  JValueAnimator = interface;//android.animation.ValueAnimator
  JValueAnimator_AnimatorUpdateListener = interface;//android.animation.ValueAnimator$AnimatorUpdateListener
  JPathMotion = interface;//android.transition.PathMotion
  JScene = interface;//android.transition.Scene
  JTransition = interface;//android.transition.Transition
  JTransition_EpicenterCallback = interface;//android.transition.Transition$EpicenterCallback
  JTransition_TransitionListener = interface;//android.transition.Transition$TransitionListener
  JTransitionManager = interface;//android.transition.TransitionManager
  JTransitionPropagation = interface;//android.transition.TransitionPropagation
  JTransitionValues = interface;//android.transition.TransitionValues
  JInterpolator = interface;//android.view.animation.Interpolator
  JToolbar_LayoutParams = interface;//android.widget.Toolbar$LayoutParams
  Jgertec_BuildConfig = interface;//br.com.gertec.BuildConfig
  Jgertec_Logger = interface;//br.com.gertec.Logger
  Jgertec_Utils = interface;//br.com.gertec.Utils
  JBuildConstants = interface;//br.com.gertec.gedi.BuildConstants
  JICL = interface;//br.com.gertec.gedi.interfaces.ICL
  JCL = interface;//br.com.gertec.gedi.CL
  JICLOCK = interface;//br.com.gertec.gedi.interfaces.ICLOCK
  Jgedi_CLOCK = interface;//br.com.gertec.gedi.CLOCK
  JICRYPT = interface;//br.com.gertec.gedi.interfaces.ICRYPT
  JCRYPT = interface;//br.com.gertec.gedi.CRYPT
  JIGEDI = interface;//br.com.gertec.gedi.interfaces.IGEDI
  JGEDI = interface;//br.com.gertec.gedi.GEDI
  JGEDI_1 = interface;//br.com.gertec.gedi.GEDI$1
  JGEDI_RET = interface;//br.com.gertec.gedi.GEDI_RET
  JGEDI_KMS_st_Control_Callbacks = interface;//br.com.gertec.gedi.structs.GEDI_KMS_st_Control$Callbacks
  JGediNative = interface;//br.com.gertec.gedi.GediNative
  JIINFO = interface;//br.com.gertec.gedi.interfaces.IINFO
  JINFO = interface;//br.com.gertec.gedi.INFO
  JIKBD = interface;//br.com.gertec.gedi.interfaces.IKBD
  JKBD = interface;//br.com.gertec.gedi.KBD
  JKBDData = interface;//br.com.gertec.gedi.KBDData
  JIKMS = interface;//br.com.gertec.gedi.interfaces.IKMS
  JKMS = interface;//br.com.gertec.gedi.KMS
  JKMS_St_SaveKey = interface;//br.com.gertec.gedi.KMS$St_SaveKey
  JILED = interface;//br.com.gertec.gedi.interfaces.ILED
  JLED = interface;//br.com.gertec.gedi.LED
  JIMSR = interface;//br.com.gertec.gedi.interfaces.IMSR
  JMSR = interface;//br.com.gertec.gedi.MSR
  JIPM = interface;//br.com.gertec.gedi.interfaces.IPM
  JPM = interface;//br.com.gertec.gedi.PM
  JIPRNTR = interface;//br.com.gertec.gedi.interfaces.IPRNTR
  JPRNTR = interface;//br.com.gertec.gedi.PRNTR
  JISMART = interface;//br.com.gertec.gedi.interfaces.ISMART
  JSMART = interface;//br.com.gertec.gedi.SMART
  JISYS = interface;//br.com.gertec.gedi.interfaces.ISYS
  JSYS = interface;//br.com.gertec.gedi.SYS
  JGEDI_CL_e_ISO_Level = interface;//br.com.gertec.gedi.enums.GEDI_CL_e_ISO_Level
  JGEDI_CL_e_ISO_Type = interface;//br.com.gertec.gedi.enums.GEDI_CL_e_ISO_Type
  JGEDI_CL_e_MF_KeyType = interface;//br.com.gertec.gedi.enums.GEDI_CL_e_MF_KeyType
  JGEDI_CL_e_MF_Type = interface;//br.com.gertec.gedi.enums.GEDI_CL_e_MF_Type
  JGEDI_CRYPT_e_Op = interface;//br.com.gertec.gedi.enums.GEDI_CRYPT_e_Op
  JGEDI_FS_e_Storage = interface;//br.com.gertec.gedi.enums.GEDI_FS_e_Storage
  JGEDI_INFO_e_ControlNumber = interface;//br.com.gertec.gedi.enums.GEDI_INFO_e_ControlNumber
  JGEDI_INFO_e_Test = interface;//br.com.gertec.gedi.enums.GEDI_INFO_e_Test
  JGEDI_KBD_e_Key = interface;//br.com.gertec.gedi.enums.GEDI_KBD_e_Key
  JGEDI_KBD_e_PowerKeyMode = interface;//br.com.gertec.gedi.enums.GEDI_KBD_e_PowerKeyMode
  JGEDI_KMS_e_BLOCKTYPE = interface;//br.com.gertec.gedi.enums.GEDI_KMS_e_BLOCKTYPE
  JGEDI_KMS_e_EncMode = interface;//br.com.gertec.gedi.enums.GEDI_KMS_e_EncMode
  JGEDI_KMS_e_KEYPURPOSE = interface;//br.com.gertec.gedi.enums.GEDI_KMS_e_KEYPURPOSE
  JGEDI_KMS_e_KEYTYPE = interface;//br.com.gertec.gedi.enums.GEDI_KMS_e_KEYTYPE
  JGEDI_KMS_e_KEYTYPE_LENGTH = interface;//br.com.gertec.gedi.enums.GEDI_KMS_e_KEYTYPE_LENGTH
  JGEDI_KMS_e_OP = interface;//br.com.gertec.gedi.enums.GEDI_KMS_e_OP
  JGEDI_LED_e_Id = interface;//br.com.gertec.gedi.enums.GEDI_LED_e_Id
  JGEDI_MSR_e_Status = interface;//br.com.gertec.gedi.enums.GEDI_MSR_e_Status
  JGEDI_PRNTR_e_Alignment = interface;//br.com.gertec.gedi.enums.GEDI_PRNTR_e_Alignment
  JGEDI_PRNTR_e_BarCodeType = interface;//br.com.gertec.gedi.enums.GEDI_PRNTR_e_BarCodeType
  JGEDI_PRNTR_e_Status = interface;//br.com.gertec.gedi.enums.GEDI_PRNTR_e_Status
  JGEDI_SMART_e_MemoryCardType = interface;//br.com.gertec.gedi.enums.GEDI_SMART_e_MemoryCardType
  JGEDI_SMART_e_Slot = interface;//br.com.gertec.gedi.enums.GEDI_SMART_e_Slot
  JGEDI_SMART_e_Status = interface;//br.com.gertec.gedi.enums.GEDI_SMART_e_Status
  JGEDI_SMART_e_Type = interface;//br.com.gertec.gedi.enums.GEDI_SMART_e_Type
  JGEDI_SMART_e_Voltage = interface;//br.com.gertec.gedi.enums.GEDI_SMART_e_Voltage
  JGEDI_SYS_e_SecuritySetup = interface;//br.com.gertec.gedi.enums.GEDI_SYS_e_SecuritySetup
  JGEDI_e_Ret = interface;//br.com.gertec.gedi.enums.GEDI_e_Ret
  JGediException = interface;//br.com.gertec.gedi.exceptions.GediException
  Jimpl_Cl = interface;//br.com.gertec.gedi.impl.Cl
  Jimpl_Clock = interface;//br.com.gertec.gedi.impl.Clock
  Jimpl_Gedi = interface;//br.com.gertec.gedi.impl.Gedi
  Jimpl_Info = interface;//br.com.gertec.gedi.impl.Info
  Jimpl_Kbd = interface;//br.com.gertec.gedi.impl.Kbd
  Jimpl_Led = interface;//br.com.gertec.gedi.impl.Led
  JLed_1 = interface;//br.com.gertec.gedi.impl.Led$1
  Jimpl_Pm = interface;//br.com.gertec.gedi.impl.Pm
  JPrinterList = interface;//br.com.gertec.gedi.impl.PrinterList
  JPrinterManager = interface;//br.com.gertec.gedi.impl.PrinterManager
  JPrinterManager_1 = interface;//br.com.gertec.gedi.impl.PrinterManager$1
  JIPrinterCallback_Stub = interface;//com.xcheng.printerservice.IPrinterCallback$Stub
  JPrinterManager_2 = interface;//br.com.gertec.gedi.impl.PrinterManager$2
  JPrinterManager_PrinterManagerListener = interface;//br.com.gertec.gedi.impl.PrinterManager$PrinterManagerListener
  Jimpl_Prntr = interface;//br.com.gertec.gedi.impl.Prntr
  JPrntr_1 = interface;//br.com.gertec.gedi.impl.Prntr$1
  JPrntr_2 = interface;//br.com.gertec.gedi.impl.Prntr$2
  JThreadPoolManager = interface;//br.com.gertec.gedi.impl.ThreadPoolManager
  JIAUDIO = interface;//br.com.gertec.gedi.interfaces.IAUDIO
  JIEnums = interface;//br.com.gertec.gedi.interfaces.IEnums
  JGEDI_AUTH_st_Data = interface;//br.com.gertec.gedi.structs.GEDI_AUTH_st_Data
  JGEDI_CLOCK_st_RTC = interface;//br.com.gertec.gedi.structs.GEDI_CLOCK_st_RTC
  JGEDI_CL_st_ISO_PollingInfo = interface;//br.com.gertec.gedi.structs.GEDI_CL_st_ISO_PollingInfo
  JGEDI_CL_st_MF_ActivateInfo = interface;//br.com.gertec.gedi.structs.GEDI_CL_st_MF_ActivateInfo
  JGEDI_CL_st_MF_Key = interface;//br.com.gertec.gedi.structs.GEDI_CL_st_MF_Key
  JGEDI_CL_st_ResetEMVInfo = interface;//br.com.gertec.gedi.structs.GEDI_CL_st_ResetEMVInfo
  JGEDI_CRYPT_st_RSAKeyGen = interface;//br.com.gertec.gedi.structs.GEDI_CRYPT_st_RSAKeyGen
  JGEDI_KBD_st_Info = interface;//br.com.gertec.gedi.structs.GEDI_KBD_st_Info
  JGEDI_KMS_st_CapturePINBlockInfo = interface;//br.com.gertec.gedi.structs.GEDI_KMS_st_CapturePINBlockInfo
  JGEDI_KMS_st_Control = interface;//br.com.gertec.gedi.structs.GEDI_KMS_st_Control
  JGEDI_KMS_st_Data = interface;//br.com.gertec.gedi.structs.GEDI_KMS_st_Data
  JGEDI_KMS_st_KB = interface;//br.com.gertec.gedi.structs.GEDI_KMS_st_KB
  JGEDI_KMS_st_PINBlock = interface;//br.com.gertec.gedi.structs.GEDI_KMS_st_PINBlock
  JGEDI_KMS_st_SaveKey = interface;//br.com.gertec.gedi.structs.GEDI_KMS_st_SaveKey
  JGEDI_MSR_st_LastErrors = interface;//br.com.gertec.gedi.structs.GEDI_MSR_st_LastErrors
  JGEDI_MSR_st_Tracks = interface;//br.com.gertec.gedi.structs.GEDI_MSR_st_Tracks
  JGEDI_PRNTR_st_BarCodeConfig = interface;//br.com.gertec.gedi.structs.GEDI_PRNTR_st_BarCodeConfig
  JGEDI_PRNTR_st_PictureConfig = interface;//br.com.gertec.gedi.structs.GEDI_PRNTR_st_PictureConfig
  JGEDI_PRNTR_st_StringConfig = interface;//br.com.gertec.gedi.structs.GEDI_PRNTR_st_StringConfig
  JGEDI_SMART_st_ResetInfo = interface;//br.com.gertec.gedi.structs.GEDI_SMART_st_ResetInfo
  JIPrinterCallback = interface;//com.xcheng.printerservice.IPrinterCallback
  JIPrinterCallback_Stub_Proxy = interface;//com.xcheng.printerservice.IPrinterCallback$Stub$Proxy
  JIPrinterService = interface;//com.xcheng.printerservice.IPrinterService
  JIPrinterService_Stub = interface;//com.xcheng.printerservice.IPrinterService$Stub
  JIPrinterService_Stub_Proxy = interface;//com.xcheng.printerservice.IPrinterService$Stub$Proxy

// ===== Interface declarations =====

  JAnimatorClass = interface(JObjectClass)
    ['{3F76A5DF-389E-4BD3-9861-04C5B00CEADE}']
    {class} function init: JAnimator; cdecl;
    {class} procedure addListener(listener: JAnimator_AnimatorListener); cdecl;
    {class} procedure addPauseListener(listener: JAnimator_AnimatorPauseListener); cdecl;
    {class} procedure cancel; cdecl;
    {class} function getInterpolator: JTimeInterpolator; cdecl;
    {class} function getListeners: JArrayList; cdecl;
    {class} function isStarted: Boolean; cdecl;
    {class} procedure pause; cdecl;
    {class} procedure removeAllListeners; cdecl;
    {class} function setDuration(duration: Int64): JAnimator; cdecl;
    {class} procedure setInterpolator(value: JTimeInterpolator); cdecl;
    {class} procedure setStartDelay(startDelay: Int64); cdecl;
    {class} procedure start; cdecl;//Deprecated
  end;

  [JavaSignature('android/animation/Animator')]
  JAnimator = interface(JObject)
    ['{FA13E56D-1B6D-4A3D-8327-9E5BA785CF21}']
    function clone: JAnimator; cdecl;
    procedure &end; cdecl;
    function getDuration: Int64; cdecl;
    function getStartDelay: Int64; cdecl;
    function isPaused: Boolean; cdecl;
    function isRunning: Boolean; cdecl;
    procedure removeListener(listener: JAnimator_AnimatorListener); cdecl;
    procedure removePauseListener(listener: JAnimator_AnimatorPauseListener); cdecl;
    procedure resume; cdecl;
    procedure setTarget(target: JObject); cdecl;//Deprecated
    procedure setupEndValues; cdecl;//Deprecated
    procedure setupStartValues; cdecl;//Deprecated
  end;
  TJAnimator = class(TJavaGenericImport<JAnimatorClass, JAnimator>) end;

  JAnimator_AnimatorListenerClass = interface(IJavaClass)
    ['{5ED6075A-B997-469C-B8D9-0AA8FB7E4798}']
    {class} procedure onAnimationEnd(animation: JAnimator); cdecl;//Deprecated
    {class} procedure onAnimationRepeat(animation: JAnimator); cdecl;//Deprecated
    {class} procedure onAnimationStart(animation: JAnimator); cdecl;//Deprecated
  end;

  [JavaSignature('android/animation/Animator$AnimatorListener')]
  JAnimator_AnimatorListener = interface(IJavaInstance)
    ['{E2DE8DD6-628B-4D84-AA46-8A1E3F00FF13}']
    procedure onAnimationCancel(animation: JAnimator); cdecl;//Deprecated
  end;
  TJAnimator_AnimatorListener = class(TJavaGenericImport<JAnimator_AnimatorListenerClass, JAnimator_AnimatorListener>) end;

  JAnimator_AnimatorPauseListenerClass = interface(IJavaClass)
    ['{CB0DC3F0-63BC-4284-ADD0-2ED367AE11E5}']
    {class} procedure onAnimationPause(animation: JAnimator); cdecl;//Deprecated
  end;

  [JavaSignature('android/animation/Animator$AnimatorPauseListener')]
  JAnimator_AnimatorPauseListener = interface(IJavaInstance)
    ['{43C9C106-65EA-4A7D-A958-FAB9E43FA4A6}']
    procedure onAnimationResume(animation: JAnimator); cdecl;
  end;
  TJAnimator_AnimatorPauseListener = class(TJavaGenericImport<JAnimator_AnimatorPauseListenerClass, JAnimator_AnimatorPauseListener>) end;

  JKeyframeClass = interface(JObjectClass)
    ['{D383116E-5CCF-48D8-9EA1-B26FBF24BA39}']
    {class} function init: JKeyframe; cdecl;
    {class} function clone: JKeyframe; cdecl;
    {class} function getFraction: Single; cdecl;
    {class} function getInterpolator: JTimeInterpolator; cdecl;
    {class} function ofFloat(fraction: Single; value: Single): JKeyframe; cdecl; overload;//Deprecated
    {class} function ofFloat(fraction: Single): JKeyframe; cdecl; overload;//Deprecated
    {class} function ofInt(fraction: Single; value: Integer): JKeyframe; cdecl; overload;//Deprecated
    {class} function ofInt(fraction: Single): JKeyframe; cdecl; overload;//Deprecated
    {class} function ofObject(fraction: Single; value: JObject): JKeyframe; cdecl; overload;//Deprecated
    {class} function ofObject(fraction: Single): JKeyframe; cdecl; overload;//Deprecated
    {class} procedure setFraction(fraction: Single); cdecl;//Deprecated
    {class} procedure setInterpolator(interpolator: JTimeInterpolator); cdecl;//Deprecated
  end;

  [JavaSignature('android/animation/Keyframe')]
  JKeyframe = interface(JObject)
    ['{9D0687A4-669E-440F-8290-154739405019}']
    function getType: Jlang_Class; cdecl;//Deprecated
    function getValue: JObject; cdecl;//Deprecated
    function hasValue: Boolean; cdecl;//Deprecated
    procedure setValue(value: JObject); cdecl;//Deprecated
  end;
  TJKeyframe = class(TJavaGenericImport<JKeyframeClass, JKeyframe>) end;

  JLayoutTransitionClass = interface(JObjectClass)
    ['{433C5359-0A96-4796-AD7B-8084EF7EF7C4}']
    {class} function _GetAPPEARING: Integer; cdecl;
    {class} function _GetCHANGE_APPEARING: Integer; cdecl;
    {class} function _GetCHANGE_DISAPPEARING: Integer; cdecl;
    {class} function _GetCHANGING: Integer; cdecl;
    {class} function _GetDISAPPEARING: Integer; cdecl;
    {class} function init: JLayoutTransition; cdecl;
    {class} procedure addTransitionListener(listener: JLayoutTransition_TransitionListener); cdecl;
    {class} procedure disableTransitionType(transitionType: Integer); cdecl;
    {class} procedure enableTransitionType(transitionType: Integer); cdecl;
    {class} function getStagger(transitionType: Integer): Int64; cdecl;
    {class} function getStartDelay(transitionType: Integer): Int64; cdecl;
    {class} function isChangingLayout: Boolean; cdecl;
    {class} function isRunning: Boolean; cdecl;
    {class} function isTransitionTypeEnabled(transitionType: Integer): Boolean; cdecl;
    {class} procedure setAnimator(transitionType: Integer; animator: JAnimator); cdecl;//Deprecated
    {class} procedure setDuration(duration: Int64); cdecl; overload;//Deprecated
    {class} procedure setDuration(transitionType: Integer; duration: Int64); cdecl; overload;//Deprecated
    {class} procedure showChild(parent: JViewGroup; child: JView); cdecl; overload;//Deprecated
    {class} procedure showChild(parent: JViewGroup; child: JView; oldVisibility: Integer); cdecl; overload;//Deprecated
    {class} property APPEARING: Integer read _GetAPPEARING;
    {class} property CHANGE_APPEARING: Integer read _GetCHANGE_APPEARING;
    {class} property CHANGE_DISAPPEARING: Integer read _GetCHANGE_DISAPPEARING;
    {class} property CHANGING: Integer read _GetCHANGING;
    {class} property DISAPPEARING: Integer read _GetDISAPPEARING;
  end;

  [JavaSignature('android/animation/LayoutTransition')]
  JLayoutTransition = interface(JObject)
    ['{42450BEE-EBF2-4954-B9B7-F8DAE7DF0EC1}']
    procedure addChild(parent: JViewGroup; child: JView); cdecl;
    function getAnimator(transitionType: Integer): JAnimator; cdecl;
    function getDuration(transitionType: Integer): Int64; cdecl;
    function getInterpolator(transitionType: Integer): JTimeInterpolator; cdecl;
    function getTransitionListeners: JList; cdecl;
    procedure hideChild(parent: JViewGroup; child: JView); cdecl; overload;//Deprecated
    procedure hideChild(parent: JViewGroup; child: JView; newVisibility: Integer); cdecl; overload;
    procedure removeChild(parent: JViewGroup; child: JView); cdecl;//Deprecated
    procedure removeTransitionListener(listener: JLayoutTransition_TransitionListener); cdecl;//Deprecated
    procedure setAnimateParentHierarchy(animateParentHierarchy: Boolean); cdecl;//Deprecated
    procedure setInterpolator(transitionType: Integer; interpolator: JTimeInterpolator); cdecl;//Deprecated
    procedure setStagger(transitionType: Integer; duration: Int64); cdecl;//Deprecated
    procedure setStartDelay(transitionType: Integer; delay: Int64); cdecl;//Deprecated
  end;
  TJLayoutTransition = class(TJavaGenericImport<JLayoutTransitionClass, JLayoutTransition>) end;

  JLayoutTransition_TransitionListenerClass = interface(IJavaClass)
    ['{9FA6F1EC-8EDB-4A05-AF58-B55A525AE114}']
  end;

  [JavaSignature('android/animation/LayoutTransition$TransitionListener')]
  JLayoutTransition_TransitionListener = interface(IJavaInstance)
    ['{0FBE048F-FCDA-4692-B6F1-DE0F07FAE885}']
    procedure endTransition(transition: JLayoutTransition; container: JViewGroup; view: JView; transitionType: Integer); cdecl;
    procedure startTransition(transition: JLayoutTransition; container: JViewGroup; view: JView; transitionType: Integer); cdecl;
  end;
  TJLayoutTransition_TransitionListener = class(TJavaGenericImport<JLayoutTransition_TransitionListenerClass, JLayoutTransition_TransitionListener>) end;

  JPropertyValuesHolderClass = interface(JObjectClass)
    ['{36C77AFF-9C3F-42B6-88F3-320FE8CF9B25}']
    {class} function ofMultiFloat(propertyName: JString; values: TJavaBiArray<Single>): JPropertyValuesHolder; cdecl; overload;//Deprecated
    {class} function ofMultiFloat(propertyName: JString; path: JPath): JPropertyValuesHolder; cdecl; overload;//Deprecated
    {class} function ofMultiInt(propertyName: JString; values: TJavaBiArray<Integer>): JPropertyValuesHolder; cdecl; overload;
    {class} function ofMultiInt(propertyName: JString; path: JPath): JPropertyValuesHolder; cdecl; overload;
    {class} function ofObject(propertyName: JString; converter: JTypeConverter; path: JPath): JPropertyValuesHolder; cdecl; overload;
    {class} function ofObject(property_: JProperty; converter: JTypeConverter; path: JPath): JPropertyValuesHolder; cdecl; overload;
    {class} procedure setConverter(converter: JTypeConverter); cdecl;
    {class} procedure setEvaluator(evaluator: JTypeEvaluator); cdecl;
    {class} procedure setProperty(property_: JProperty); cdecl;
    {class} procedure setPropertyName(propertyName: JString); cdecl;
  end;

  [JavaSignature('android/animation/PropertyValuesHolder')]
  JPropertyValuesHolder = interface(JObject)
    ['{12B4ABFD-CBCA-4636-AF2D-C386EF895DC3}']
    function clone: JPropertyValuesHolder; cdecl;//Deprecated
    function getPropertyName: JString; cdecl;//Deprecated
    function toString: JString; cdecl;
  end;
  TJPropertyValuesHolder = class(TJavaGenericImport<JPropertyValuesHolderClass, JPropertyValuesHolder>) end;

  JStateListAnimatorClass = interface(JObjectClass)
    ['{109E4067-E218-47B1-93EB-65B8916A98D8}']
    {class} function init: JStateListAnimator; cdecl;
    {class} procedure jumpToCurrentState; cdecl;//Deprecated
  end;

  [JavaSignature('android/animation/StateListAnimator')]
  JStateListAnimator = interface(JObject)
    ['{CA2A9587-26AA-4DC2-8DFF-A1305A37608F}']
    procedure addState(specs: TJavaArray<Integer>; animator: JAnimator); cdecl;//Deprecated
    function clone: JStateListAnimator; cdecl;//Deprecated
  end;
  TJStateListAnimator = class(TJavaGenericImport<JStateListAnimatorClass, JStateListAnimator>) end;

  JTimeInterpolatorClass = interface(IJavaClass)
    ['{1E682A1C-9102-461D-A3CA-5596683F1D66}']
  end;

  [JavaSignature('android/animation/TimeInterpolator')]
  JTimeInterpolator = interface(IJavaInstance)
    ['{639F8A83-7D9B-49AF-A19E-96B27E46D2AB}']
    function getInterpolation(input: Single): Single; cdecl;
  end;
  TJTimeInterpolator = class(TJavaGenericImport<JTimeInterpolatorClass, JTimeInterpolator>) end;

  JTypeConverterClass = interface(JObjectClass)
    ['{BE2DD177-6D79-4B0C-B4F5-4E4CD9D7436D}']
    {class} function init(fromClass: Jlang_Class; toClass: Jlang_Class): JTypeConverter; cdecl;
    {class} function convert(value: JObject): JObject; cdecl;
  end;

  [JavaSignature('android/animation/TypeConverter')]
  JTypeConverter = interface(JObject)
    ['{BFEA4116-0766-4AD9-AA8F-4C15A583EB2E}']
  end;
  TJTypeConverter = class(TJavaGenericImport<JTypeConverterClass, JTypeConverter>) end;

  JTypeEvaluatorClass = interface(IJavaClass)
    ['{15B67CAF-6F50-4AA3-A88F-C5AF78D62FD4}']
  end;

  [JavaSignature('android/animation/TypeEvaluator')]
  JTypeEvaluator = interface(IJavaInstance)
    ['{F436383D-6F44-40D8-ACDD-9057777691FC}']
    function evaluate(fraction: Single; startValue: JObject; endValue: JObject): JObject; cdecl;
  end;
  TJTypeEvaluator = class(TJavaGenericImport<JTypeEvaluatorClass, JTypeEvaluator>) end;

  JValueAnimatorClass = interface(JAnimatorClass)
    ['{FF3B71ED-5A33-45B0-8500-7672B0B98E2C}']
    {class} function _GetINFINITE: Integer; cdecl;
    {class} function _GetRESTART: Integer; cdecl;
    {class} function _GetREVERSE: Integer; cdecl;
    {class} function init: JValueAnimator; cdecl;
    {class} procedure &end; cdecl;//Deprecated
    {class} function getAnimatedFraction: Single; cdecl;//Deprecated
    {class} function getDuration: Int64; cdecl;//Deprecated
    {class} function getFrameDelay: Int64; cdecl;//Deprecated
    {class} function getStartDelay: Int64; cdecl;//Deprecated
    {class} function getValues: TJavaObjectArray<JPropertyValuesHolder>; cdecl;//Deprecated
    {class} function isRunning: Boolean; cdecl;//Deprecated
    {class} procedure resume; cdecl;
    {class} procedure reverse; cdecl;
    {class} procedure setCurrentFraction(fraction: Single); cdecl;
    {class} procedure setFrameDelay(frameDelay: Int64); cdecl;
    {class} procedure setRepeatMode(value: Integer); cdecl;
    {class} procedure setStartDelay(startDelay: Int64); cdecl;
    {class} property INFINITE: Integer read _GetINFINITE;
    {class} property RESTART: Integer read _GetRESTART;
    {class} property REVERSE: Integer read _GetREVERSE;
  end;

  [JavaSignature('android/animation/ValueAnimator')]
  JValueAnimator = interface(JAnimator)
    ['{70F92B14-EFD4-4DC7-91DE-6617417AE194}']
    procedure addUpdateListener(listener: JValueAnimator_AnimatorUpdateListener); cdecl;//Deprecated
    procedure cancel; cdecl;//Deprecated
    function clone: JValueAnimator; cdecl;//Deprecated
    function getAnimatedValue: JObject; cdecl; overload;//Deprecated
    function getAnimatedValue(propertyName: JString): JObject; cdecl; overload;//Deprecated
    function getCurrentPlayTime: Int64; cdecl;//Deprecated
    function getInterpolator: JTimeInterpolator; cdecl;//Deprecated
    function getRepeatCount: Integer; cdecl;//Deprecated
    function getRepeatMode: Integer; cdecl;//Deprecated
    function isStarted: Boolean; cdecl;//Deprecated
    procedure pause; cdecl;
    procedure removeAllUpdateListeners; cdecl;
    procedure removeUpdateListener(listener: JValueAnimator_AnimatorUpdateListener); cdecl;
    procedure setCurrentPlayTime(playTime: Int64); cdecl;
    function setDuration(duration: Int64): JValueAnimator; cdecl;
    procedure setEvaluator(value: JTypeEvaluator); cdecl;
    procedure setInterpolator(value: JTimeInterpolator); cdecl;
    procedure setRepeatCount(value: Integer); cdecl;
    procedure start; cdecl;
    function toString: JString; cdecl;
  end;
  TJValueAnimator = class(TJavaGenericImport<JValueAnimatorClass, JValueAnimator>) end;

  JValueAnimator_AnimatorUpdateListenerClass = interface(IJavaClass)
    ['{9CA50CBF-4462-4445-82CD-13CE985E2DE4}']
  end;

  [JavaSignature('android/animation/ValueAnimator$AnimatorUpdateListener')]
  JValueAnimator_AnimatorUpdateListener = interface(IJavaInstance)
    ['{0F883491-52EF-4A40-B7D2-FC23E11E46FE}']
    procedure onAnimationUpdate(animation: JValueAnimator); cdecl;
  end;
  TJValueAnimator_AnimatorUpdateListener = class(TJavaGenericImport<JValueAnimator_AnimatorUpdateListenerClass, JValueAnimator_AnimatorUpdateListener>) end;

  JPathMotionClass = interface(JObjectClass)
    ['{E1CD1A94-115C-492C-A490-37F0E72956EB}']
    {class} function init: JPathMotion; cdecl; overload;
    {class} function init(context: JContext; attrs: JAttributeSet): JPathMotion; cdecl; overload;
    {class} function getPath(startX: Single; startY: Single; endX: Single; endY: Single): JPath; cdecl;//Deprecated
  end;

  [JavaSignature('android/transition/PathMotion')]
  JPathMotion = interface(JObject)
    ['{BDC08353-C9FB-42D7-97CC-C35837D2F536}']
  end;
  TJPathMotion = class(TJavaGenericImport<JPathMotionClass, JPathMotion>) end;

  JSceneClass = interface(JObjectClass)
    ['{8B9120CA-AEEA-4DE3-BDC9-15CFD23A7B07}']
    {class} function init(sceneRoot: JViewGroup): JScene; cdecl; overload;
    {class} function init(sceneRoot: JViewGroup; layout: JView): JScene; cdecl; overload;
    {class} function init(sceneRoot: JViewGroup; layout: JViewGroup): JScene; cdecl; overload;//Deprecated
    {class} procedure enter; cdecl;
    {class} function getSceneForLayout(sceneRoot: JViewGroup; layoutId: Integer; context: JContext): JScene; cdecl;
    {class} procedure setEnterAction(action: JRunnable); cdecl;
    {class} procedure setExitAction(action: JRunnable); cdecl;
  end;

  [JavaSignature('android/transition/Scene')]
  JScene = interface(JObject)
    ['{85A60B99-5837-4F1F-A344-C627DD586B82}']
    procedure exit; cdecl;
    function getSceneRoot: JViewGroup; cdecl;
  end;
  TJScene = class(TJavaGenericImport<JSceneClass, JScene>) end;

  JTransitionClass = interface(JObjectClass)
    ['{60EC06BC-8F7A-4416-A04B-5B57987EB18E}']
    {class} function _GetMATCH_ID: Integer; cdecl;
    {class} function _GetMATCH_INSTANCE: Integer; cdecl;
    {class} function _GetMATCH_ITEM_ID: Integer; cdecl;
    {class} function _GetMATCH_NAME: Integer; cdecl;
    {class} function init: JTransition; cdecl; overload;
    {class} function init(context: JContext; attrs: JAttributeSet): JTransition; cdecl; overload;
    {class} function addTarget(targetType: Jlang_Class): JTransition; cdecl; overload;//Deprecated
    {class} function addTarget(target: JView): JTransition; cdecl; overload;//Deprecated
    {class} function canRemoveViews: Boolean; cdecl;//Deprecated
    {class} function createAnimator(sceneRoot: JViewGroup; startValues: JTransitionValues; endValues: JTransitionValues): JAnimator; cdecl;
    {class} function excludeChildren(targetId: Integer; exclude: Boolean): JTransition; cdecl; overload;
    {class} function excludeChildren(target: JView; exclude: Boolean): JTransition; cdecl; overload;
    {class} function excludeTarget(target: JView; exclude: Boolean): JTransition; cdecl; overload;
    {class} function excludeTarget(type_: Jlang_Class; exclude: Boolean): JTransition; cdecl; overload;
    {class} function getDuration: Int64; cdecl;
    {class} function getName: JString; cdecl;
    {class} function getPathMotion: JPathMotion; cdecl;
    {class} function getPropagation: JTransitionPropagation; cdecl;
    {class} function getTargetNames: JList; cdecl;
    {class} function getTargetTypes: JList; cdecl;
    {class} function getTargets: JList; cdecl;
    {class} function removeListener(listener: JTransition_TransitionListener): JTransition; cdecl;//Deprecated
    {class} function removeTarget(targetId: Integer): JTransition; cdecl; overload;//Deprecated
    {class} function removeTarget(targetName: JString): JTransition; cdecl; overload;//Deprecated
    {class} procedure setEpicenterCallback(epicenterCallback: JTransition_EpicenterCallback); cdecl;//Deprecated
    {class} function setInterpolator(interpolator: JTimeInterpolator): JTransition; cdecl;//Deprecated
    {class} function toString: JString; cdecl;//Deprecated
    {class} property MATCH_ID: Integer read _GetMATCH_ID;
    {class} property MATCH_INSTANCE: Integer read _GetMATCH_INSTANCE;
    {class} property MATCH_ITEM_ID: Integer read _GetMATCH_ITEM_ID;
    {class} property MATCH_NAME: Integer read _GetMATCH_NAME;
  end;

  [JavaSignature('android/transition/Transition')]
  JTransition = interface(JObject)
    ['{C2F8200F-1C83-40AE-8C5B-C0C8BFF17F88}']
    function addListener(listener: JTransition_TransitionListener): JTransition; cdecl;//Deprecated
    function addTarget(targetId: Integer): JTransition; cdecl; overload;//Deprecated
    function addTarget(targetName: JString): JTransition; cdecl; overload;//Deprecated
    procedure captureEndValues(transitionValues: JTransitionValues); cdecl;
    procedure captureStartValues(transitionValues: JTransitionValues); cdecl;
    function clone: JTransition; cdecl;
    function excludeChildren(type_: Jlang_Class; exclude: Boolean): JTransition; cdecl; overload;
    function excludeTarget(targetId: Integer; exclude: Boolean): JTransition; cdecl; overload;
    function excludeTarget(targetName: JString; exclude: Boolean): JTransition; cdecl; overload;
    function getEpicenter: JRect; cdecl;
    function getEpicenterCallback: JTransition_EpicenterCallback; cdecl;
    function getInterpolator: JTimeInterpolator; cdecl;
    function getStartDelay: Int64; cdecl;
    function getTargetIds: JList; cdecl;
    function getTransitionProperties: TJavaObjectArray<JString>; cdecl;//Deprecated
    function getTransitionValues(view: JView; start: Boolean): JTransitionValues; cdecl;//Deprecated
    function isTransitionRequired(startValues: JTransitionValues; endValues: JTransitionValues): Boolean; cdecl;//Deprecated
    function removeTarget(target: JView): JTransition; cdecl; overload;//Deprecated
    function removeTarget(target: Jlang_Class): JTransition; cdecl; overload;//Deprecated
    function setDuration(duration: Int64): JTransition; cdecl;//Deprecated
    procedure setPathMotion(pathMotion: JPathMotion); cdecl;//Deprecated
    procedure setPropagation(transitionPropagation: JTransitionPropagation); cdecl;//Deprecated
    function setStartDelay(startDelay: Int64): JTransition; cdecl;//Deprecated
  end;
  TJTransition = class(TJavaGenericImport<JTransitionClass, JTransition>) end;

  JTransition_EpicenterCallbackClass = interface(JObjectClass)
    ['{8141257A-130B-466C-A08D-AA3A00946F4C}']
    {class} function init: JTransition_EpicenterCallback; cdecl;
  end;

  [JavaSignature('android/transition/Transition$EpicenterCallback')]
  JTransition_EpicenterCallback = interface(JObject)
    ['{CE004917-266F-4076-8913-F23184824FBA}']
    function onGetEpicenter(transition: JTransition): JRect; cdecl;
  end;
  TJTransition_EpicenterCallback = class(TJavaGenericImport<JTransition_EpicenterCallbackClass, JTransition_EpicenterCallback>) end;

  JTransition_TransitionListenerClass = interface(IJavaClass)
    ['{D5083752-E8A6-46DF-BE40-AE11073C387E}']
    {class} procedure onTransitionCancel(transition: JTransition); cdecl;
    {class} procedure onTransitionEnd(transition: JTransition); cdecl;
  end;

  [JavaSignature('android/transition/Transition$TransitionListener')]
  JTransition_TransitionListener = interface(IJavaInstance)
    ['{C32BE107-6E05-4898-AF0A-FAD970D66E29}']
    procedure onTransitionPause(transition: JTransition); cdecl;
    procedure onTransitionResume(transition: JTransition); cdecl;
    procedure onTransitionStart(transition: JTransition); cdecl;
  end;
  TJTransition_TransitionListener = class(TJavaGenericImport<JTransition_TransitionListenerClass, JTransition_TransitionListener>) end;

  JTransitionManagerClass = interface(JObjectClass)
    ['{4160EFA0-3499-4964-817E-46497BB5B957}']
    {class} function init: JTransitionManager; cdecl;
    {class} procedure beginDelayedTransition(sceneRoot: JViewGroup); cdecl; overload;
    {class} procedure beginDelayedTransition(sceneRoot: JViewGroup; transition: JTransition); cdecl; overload;//Deprecated
    {class} procedure endTransitions(sceneRoot: JViewGroup); cdecl;//Deprecated
    {class} procedure go(scene: JScene); cdecl; overload;//Deprecated
    {class} procedure go(scene: JScene; transition: JTransition); cdecl; overload;//Deprecated
    {class} procedure setTransition(scene: JScene; transition: JTransition); cdecl; overload;//Deprecated
    {class} procedure setTransition(fromScene: JScene; toScene: JScene; transition: JTransition); cdecl; overload;//Deprecated
  end;

  [JavaSignature('android/transition/TransitionManager')]
  JTransitionManager = interface(JObject)
    ['{FF5E1210-1F04-4F81-9CAC-3D7A5C4E972B}']
    procedure transitionTo(scene: JScene); cdecl;//Deprecated
  end;
  TJTransitionManager = class(TJavaGenericImport<JTransitionManagerClass, JTransitionManager>) end;

  JTransitionPropagationClass = interface(JObjectClass)
    ['{A881388A-C877-4DD9-9BAD-1BA4F56133EE}']
    {class} function init: JTransitionPropagation; cdecl;
    {class} procedure captureValues(transitionValues: JTransitionValues); cdecl;//Deprecated
    {class} function getPropagationProperties: TJavaObjectArray<JString>; cdecl;//Deprecated
    {class} function getStartDelay(sceneRoot: JViewGroup; transition: JTransition; startValues: JTransitionValues; endValues: JTransitionValues): Int64; cdecl;//Deprecated
  end;

  [JavaSignature('android/transition/TransitionPropagation')]
  JTransitionPropagation = interface(JObject)
    ['{7595B7EF-6BCE-4281-BC67-335E2FB6C091}']
  end;
  TJTransitionPropagation = class(TJavaGenericImport<JTransitionPropagationClass, JTransitionPropagation>) end;

  JTransitionValuesClass = interface(JObjectClass)
    ['{3BF98CFA-6A4D-4815-8D42-15E97C916D91}']
    {class} function _Getvalues: JMap; cdecl;
    {class} function _Getview: JView; cdecl;
    {class} function init: JTransitionValues; cdecl;
    {class} function toString: JString; cdecl;
    {class} property values: JMap read _Getvalues;
    {class} property view: JView read _Getview;
  end;

  [JavaSignature('android/transition/TransitionValues')]
  JTransitionValues = interface(JObject)
    ['{178E09E6-D32C-48A9-ADCF-8CCEA804052A}']
    function equals(other: JObject): Boolean; cdecl;
    function hashCode: Integer; cdecl;
  end;
  TJTransitionValues = class(TJavaGenericImport<JTransitionValuesClass, JTransitionValues>) end;

  JInterpolatorClass = interface(JTimeInterpolatorClass)
    ['{A575B46A-E489-409C-807A-1B8F2BE092E8}']
  end;

  [JavaSignature('android/view/animation/Interpolator')]
  JInterpolator = interface(JTimeInterpolator)
    ['{F1082403-52DA-4AF0-B017-DAB334325FC7}']
  end;
  TJInterpolator = class(TJavaGenericImport<JInterpolatorClass, JInterpolator>) end;

  JToolbar_LayoutParamsClass = interface(JActionBar_LayoutParamsClass)
    ['{6D43796C-C163-4084-BB30-6FE68AFD7ABB}']
    {class} function init(c: JContext; attrs: JAttributeSet): JToolbar_LayoutParams; cdecl; overload;
    {class} function init(width: Integer; height: Integer): JToolbar_LayoutParams; cdecl; overload;
    {class} function init(width: Integer; height: Integer; gravity: Integer): JToolbar_LayoutParams; cdecl; overload;
    {class} function init(gravity: Integer): JToolbar_LayoutParams; cdecl; overload;
    {class} function init(source: JToolbar_LayoutParams): JToolbar_LayoutParams; cdecl; overload;
    {class} function init(source: JActionBar_LayoutParams): JToolbar_LayoutParams; cdecl; overload;
    {class} function init(source: JViewGroup_MarginLayoutParams): JToolbar_LayoutParams; cdecl; overload;
    {class} function init(source: JViewGroup_LayoutParams): JToolbar_LayoutParams; cdecl; overload;
  end;

  [JavaSignature('android/widget/Toolbar$LayoutParams')]
  JToolbar_LayoutParams = interface(JActionBar_LayoutParams)
    ['{BCD101F9-B7B7-4B2F-9460-056B3EA7B9F0}']
  end;
  TJToolbar_LayoutParams = class(TJavaGenericImport<JToolbar_LayoutParamsClass, JToolbar_LayoutParams>) end;

  Jgertec_BuildConfigClass = interface(JObjectClass)
    ['{B705B7CA-F3B9-4E8F-A423-230AF713392F}']
    {class} function _GetAPPLICATION_ID: JString; cdecl;
    {class} function _GetBUILD_TIME: JString; cdecl;
    {class} function _GetBUILD_TYPE: JString; cdecl;
    {class} function _GetDEBUG: Boolean; cdecl;
    {class} function _GetFLAVOR: JString; cdecl;
    {class} function _GetFLAVOR_build: JString; cdecl;
    {class} function _GetFLAVOR_device: JString; cdecl;
    {class} function _GetVERSION_CODE: Integer; cdecl;
    {class} function _GetVERSION_NAME: JString; cdecl;
    {class} function init: Jgertec_BuildConfig; cdecl;//Deprecated
    {class} property APPLICATION_ID: JString read _GetAPPLICATION_ID;
    {class} property BUILD_TIME: JString read _GetBUILD_TIME;
    {class} property BUILD_TYPE: JString read _GetBUILD_TYPE;
    {class} property DEBUG: Boolean read _GetDEBUG;
    {class} property FLAVOR: JString read _GetFLAVOR;
    {class} property FLAVOR_build: JString read _GetFLAVOR_build;
    {class} property FLAVOR_device: JString read _GetFLAVOR_device;
    {class} property VERSION_CODE: Integer read _GetVERSION_CODE;
    {class} property VERSION_NAME: JString read _GetVERSION_NAME;
  end;

  [JavaSignature('br/com/gertec/BuildConfig')]
  Jgertec_BuildConfig = interface(JObject)
    ['{F28F8DBD-115A-4DD5-B72D-361EBDD60D4D}']
  end;
  TJgertec_BuildConfig = class(TJavaGenericImport<Jgertec_BuildConfigClass, Jgertec_BuildConfig>) end;

  Jgertec_LoggerClass = interface(JObjectClass)
    ['{14DA2212-24F3-47C8-ADD2-9A2CEF3BDDDB}']
    {class} procedure debug(P1: JString); cdecl; overload;
    {class} procedure error(P1: JString); cdecl; overload;
    {class} procedure error(P1: JString; P2: JThrowable); cdecl; overload;
    {class} function getLogger(P1: JString): Jgertec_Logger; cdecl;
    {class} procedure info(P1: JString); cdecl; overload;
    {class} procedure log(P1: Integer; P2: TJavaArray<Byte>; P3: TJavaArray<Byte>); cdecl; overload;
    {class} procedure log(P1: Integer; P2: JString; P3: JString); cdecl; overload;
    {class} procedure warn(P1: JString); cdecl; overload;
  end;

  [JavaSignature('br/com/gertec/Logger')]
  Jgertec_Logger = interface(JObject)
    ['{14FFA520-3F88-4E2F-A4E7-122DFA40C970}']
  end;
  TJgertec_Logger = class(TJavaGenericImport<Jgertec_LoggerClass, Jgertec_Logger>) end;

  Jgertec_UtilsClass = interface(JObjectClass)
    ['{306B3D5D-57D7-45E6-8AD6-2F2D5710D4E4}']
    {class} function byteArrayToHexString(P1: TJavaArray<Byte>): JString; cdecl; overload;
    {class} function byteArrayToHexString(P1: TJavaArray<Byte>; P2: Integer): JString; cdecl; overload;
    {class} function byteArrayToString(P1: TJavaArray<Byte>): JString; cdecl;
    {class} function hexStringToByteArray(P1: JString): TJavaArray<Byte>; cdecl;
    {class} function init: Jgertec_Utils; cdecl;
  end;

  [JavaSignature('br/com/gertec/Utils')]
  Jgertec_Utils = interface(JObject)
    ['{52EE20C2-9CC5-4FD8-BF33-D2AEC8B758FB}']
  end;
  TJgertec_Utils = class(TJavaGenericImport<Jgertec_UtilsClass, Jgertec_Utils>) end;

  JBuildConstantsClass = interface(IJavaClass)
    ['{865A204B-2C70-41D1-99CE-06D4FD9D10C9}']
    {class} function _GetDEBUG: Boolean; cdecl;
    {class} property DEBUG: Boolean read _GetDEBUG;
  end;

  [JavaSignature('br/com/gertec/gedi/BuildConstants')]
  JBuildConstants = interface(IJavaInstance)
    ['{ECD11C84-2506-4924-BCA7-1EF76DBCF0EC}']
  end;
  TJBuildConstants = class(TJavaGenericImport<JBuildConstantsClass, JBuildConstants>) end;

  JICLClass = interface(IJavaClass)
    ['{4C700C6E-85B5-42EE-9525-22D3F73683F1}']
  end;

  [JavaSignature('br/com/gertec/gedi/interfaces/ICL')]
  JICL = interface(IJavaInstance)
    ['{C457CC4E-0BB1-4780-9253-C297C6B8EA08}']
    function ISO_Polling(P1: Integer): JGEDI_CL_st_ISO_PollingInfo; cdecl;
    procedure MF_Authentication(P1: Integer; P2: JGEDI_CL_st_MF_Key; P3: TJavaArray<Byte>); cdecl;
    function MF_BlockRead(P1: Integer): TJavaArray<Byte>; cdecl;
    procedure MF_BlockWrite(P1: Integer; P2: TJavaArray<Byte>); cdecl;
    function MF_SignatureGet(P1: Integer): TJavaArray<Byte>; cdecl;
    procedure PowerOff; cdecl;
    procedure PowerOn; cdecl;
    function ResetEMV: JGEDI_CL_st_ResetEMVInfo; cdecl;
    function SendAPDU(P1: TJavaArray<Byte>): TJavaArray<Byte>; cdecl;
  end;
  TJICL = class(TJavaGenericImport<JICLClass, JICL>) end;

  JCLClass = interface(JICLClass)
    ['{1B3A78AF-FA0E-45E7-ADDA-DCC333BA7010}']
    {class} function ISO_Polling(P1: Integer): JGEDI_CL_st_ISO_PollingInfo; cdecl;
    {class} procedure PowerOff; cdecl;
    {class} procedure PowerOn; cdecl;
    {class} function ResetEMV(P1: TJavaArray<Byte>; P2: TJavaArray<Integer>): Integer; cdecl;
    {class} function SendAPDU(P1: TJavaArray<Byte>; P2: Integer; P3: TJavaArray<Byte>; P4: TJavaArray<Integer>): Integer; cdecl;
    {class} function init: JCL; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/CL')]
  JCL = interface(JICL)
    ['{F46F4E6E-45E4-4834-9137-F0CCF9CB5C01}']
  end;
  TJCL = class(TJavaGenericImport<JCLClass, JCL>) end;

  JICLOCKClass = interface(IJavaClass)
    ['{1577989E-4C6F-42B7-A32A-C4020BBB1F6B}']
  end;

  [JavaSignature('br/com/gertec/gedi/interfaces/ICLOCK')]
  JICLOCK = interface(IJavaInstance)
    ['{ECFCFF71-3131-4BE4-A96E-7950BAF0AFE1}']
    function RTCFGet: JGEDI_CLOCK_st_RTC; cdecl;
    procedure RTCFSet(P1: JGEDI_CLOCK_st_RTC); cdecl;
    function RTCGet: JGEDI_CLOCK_st_RTC; cdecl;
    procedure RTCSet(P1: JGEDI_CLOCK_st_RTC); cdecl;
  end;
  TJICLOCK = class(TJavaGenericImport<JICLOCKClass, JICLOCK>) end;

  Jgedi_CLOCKClass = interface(JICLOCKClass)
    ['{FA33C147-299A-42FC-99A4-897C6E5E694B}']
    {class} function RTCFGet: JGEDI_CLOCK_st_RTC; cdecl; overload;//Deprecated
    {class} function RTCFGet(P1: TJavaArray<Byte>; P2: TJavaArray<Byte>; P3: TJavaArray<Byte>; P4: TJavaArray<Byte>; P5: TJavaArray<Byte>; P6: TJavaArray<Byte>; P7: TJavaArray<Byte>): Integer; cdecl; overload;//Deprecated
    {class} procedure RTCFSet(P1: JGEDI_CLOCK_st_RTC); cdecl; overload;//Deprecated
    {class} function RTCFSet(P1: Byte; P2: Byte; P3: Byte; P4: Byte; P5: Byte; P6: Byte; P7: Byte): Integer; cdecl; overload;//Deprecated
    {class} function RTCGet: JGEDI_CLOCK_st_RTC; cdecl; overload;//Deprecated
    {class} function RTCGet(P1: TJavaArray<Byte>; P2: TJavaArray<Byte>; P3: TJavaArray<Byte>; P4: TJavaArray<Byte>; P5: TJavaArray<Byte>; P6: TJavaArray<Byte>; P7: TJavaArray<Byte>): Integer; cdecl; overload;//Deprecated
    {class} procedure RTCSet(P1: JGEDI_CLOCK_st_RTC); cdecl; overload;//Deprecated
    {class} function RTCSet(P1: Byte; P2: Byte; P3: Byte; P4: Byte; P5: Byte; P6: Byte; P7: Byte): Integer; cdecl; overload;//Deprecated
    {class} function init: Jgedi_CLOCK; cdecl; overload;//Deprecated
    {class} function init(P1: JContext): Jgedi_CLOCK; cdecl; overload;//Deprecated
  end;

  [JavaSignature('br/com/gertec/gedi/CLOCK')]
  Jgedi_CLOCK = interface(JICLOCK)
    ['{36A6BFF5-DA35-48B1-AAEB-81BFF10F2427}']
  end;
  TJgedi_CLOCK = class(TJavaGenericImport<Jgedi_CLOCKClass, Jgedi_CLOCK>) end;

  JICRYPTClass = interface(IJavaClass)
    ['{49883D8B-3311-44BB-A0EA-7150907C1A23}']
  end;

  [JavaSignature('br/com/gertec/gedi/interfaces/ICRYPT')]
  JICRYPT = interface(IJavaInstance)
    ['{C71186AC-EDF9-4148-933D-5AC75BB87DB9}']
    function AES(P1: JGEDI_CRYPT_e_Op; P2: TJavaArray<Byte>; P3: TJavaArray<Byte>): TJavaArray<Byte>; cdecl;
    function AESN(P1: JGEDI_CRYPT_e_Op; P2: TJavaArray<Byte>; P3: TJavaArray<Byte>; P4: TJavaArray<Byte>): TJavaArray<Byte>; cdecl;
    function DES(P1: JGEDI_CRYPT_e_Op; P2: TJavaArray<Byte>; P3: TJavaArray<Byte>): TJavaArray<Byte>; cdecl;
    function RNG(P1: Integer): TJavaArray<Byte>; cdecl;
    function RSA(P1: TJavaArray<Byte>; P2: TJavaArray<Byte>; P3: TJavaArray<Byte>): TJavaArray<Byte>; cdecl;
    function SHA1(P1: TJavaArray<Byte>): TJavaArray<Byte>; cdecl;
  end;
  TJICRYPT = class(TJavaGenericImport<JICRYPTClass, JICRYPT>) end;

  JCRYPTClass = interface(JICRYPTClass)
    ['{4B2568E7-7495-42F4-BA97-8210A2D264F6}']
    {class} function AES(P1: JGEDI_CRYPT_e_Op; P2: TJavaArray<Byte>; P3: TJavaArray<Byte>): TJavaArray<Byte>; cdecl; overload;
    {class} function AES(P1: JGEDI_CRYPT_e_Op; P2: TJavaArray<Byte>; P3: TJavaArray<Byte>; P4: TJavaArray<Byte>): Integer; cdecl; overload;
    {class} function AESN(P1: JGEDI_CRYPT_e_Op; P2: TJavaArray<Byte>; P3: TJavaArray<Byte>; P4: TJavaArray<Byte>): TJavaArray<Byte>; cdecl;
    {class} function DES(P1: JGEDI_CRYPT_e_Op; P2: TJavaArray<Byte>; P3: TJavaArray<Byte>): TJavaArray<Byte>; cdecl; overload;
    {class} function DES(P1: JGEDI_CRYPT_e_Op; P2: TJavaArray<Byte>; P3: Integer; P4: Integer; P5: TJavaArray<Byte>; P6: Integer; P7: Integer; P8: TJavaArray<Byte>): Integer; cdecl; overload;
    {class} function RNG(P1: TJavaArray<Byte>): Integer; cdecl; overload;
    {class} function RNG(P1: Integer): TJavaArray<Byte>; cdecl; overload;
    {class} function RSA(P1: TJavaArray<Byte>; P2: TJavaArray<Byte>; P3: TJavaArray<Byte>): TJavaArray<Byte>; cdecl; overload;
    {class} function RSA(P1: TJavaArray<Byte>; P2: TJavaArray<Byte>; P3: TJavaArray<Byte>; P4: TJavaArray<Byte>): Integer; cdecl; overload;
    {class} function SHA1(P1: TJavaArray<Byte>): TJavaArray<Byte>; cdecl; overload;
    {class} function SHA1(P1: TJavaArray<Byte>; P2: TJavaArray<Byte>): Integer; cdecl; overload;
    {class} function init: JCRYPT; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/CRYPT')]
  JCRYPT = interface(JICRYPT)
    ['{9F774DA9-0EFC-4445-8AC1-306C494D0868}']
  end;
  TJCRYPT = class(TJavaGenericImport<JCRYPTClass, JCRYPT>) end;

  JIGEDIClass = interface(IJavaClass)
    ['{6FE354A0-E364-4C7A-AB8E-939EAC967F32}']
  end;

  [JavaSignature('br/com/gertec/gedi/interfaces/IGEDI')]
  JIGEDI = interface(IJavaInstance)
    ['{D42B3E5B-00D2-4FA9-B0C5-02B85A4CC89F}']
    procedure EnterEng(P1: JString); cdecl;
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
  TJIGEDI = class(TJavaGenericImport<JIGEDIClass, JIGEDI>) end;

  JGEDIClass = interface(JIGEDIClass)
    ['{A1C7C539-CB8C-4814-B0AA-7B4CD078A55B}']
    {class} function getInstance: JIGEDI; cdecl; overload;
    {class} function getInstance(P1: JActivity): JIGEDI; cdecl; overload;
  end;

  [JavaSignature('br/com/gertec/gedi/GEDI')]
  JGEDI = interface(JIGEDI)
    ['{6D6B18AA-F1AC-45A4-B365-44748B674F71}']
  end;
  TJGEDI = class(TJavaGenericImport<JGEDIClass, JGEDI>) end;

  JGEDI_1Class = interface(JThreadClass)
    ['{261511DF-27AC-43BC-9902-60463CB0B83E}']
    {class} function init(P1: JActivity): JGEDI_1; cdecl;//Deprecated
    {class} procedure run; cdecl;//Deprecated
  end;

  [JavaSignature('br/com/gertec/gedi/GEDI$1')]
  JGEDI_1 = interface(JThread)
    ['{CE745829-C997-4F09-8F25-C4EC216897B1}']
  end;
  TJGEDI_1 = class(TJavaGenericImport<JGEDI_1Class, JGEDI_1>) end;

  JGEDI_RETClass = interface(IJavaClass)
    ['{87AE1F0A-2749-4C7B-B646-CE89054ECB8A}']
    {class} function _GetBUFFER_NOT_ENOUGH: Integer; cdecl;
    {class} function _GetCANCELLED: Integer; cdecl;
    {class} function _GetCLOCK_ERROR: Integer; cdecl;
    {class} function _GetCL_BYTE_ERROR: Integer; cdecl;
    {class} function _GetCL_COLLISION: Integer; cdecl;
    {class} function _GetCL_CRC_ERROR: Integer; cdecl;
    {class} function _GetCL_ERROR: Integer; cdecl;
    {class} function _GetCL_NO_RESPONSE: Integer; cdecl;
    {class} function _GetCL_PROTOCOL_ERROR: Integer; cdecl;
    {class} function _GetCL_SN: Integer; cdecl;
    {class} function _GetCOULD_NOT_ALLOCATE_MEMORY: Integer; cdecl;
    {class} function _GetCRYPT_ERROR: Integer; cdecl;
    {class} function _GetEND_OF_LIST: Integer; cdecl;
    {class} function _GetFS_ACCESS_DENIED: Integer; cdecl;
    {class} function _GetFS_ALREADY_EXIST: Integer; cdecl;
    {class} function _GetFS_EOF: Integer; cdecl;
    {class} function _GetFS_ERROR: Integer; cdecl;
    {class} function _GetFS_INVALID_HANDLE: Integer; cdecl;
    {class} function _GetFS_INVALID_NAME: Integer; cdecl;
    {class} function _GetFS_NOT_EXIST: Integer; cdecl;
    {class} function _GetFUNCTION_NOT_FOUND: Integer; cdecl;
    {class} function _GetINFO_ERROR: Integer; cdecl;
    {class} function _GetINVALID_PARAMETER: Integer; cdecl;
    {class} function _GetKMS_ERROR: Integer; cdecl;
    {class} function _GetKMS_FULL: Integer; cdecl;
    {class} function _GetKMS_INVALID_CERTIFICATE: Integer; cdecl;
    {class} function _GetKMS_INVALID_HASH: Integer; cdecl;
    {class} function _GetKMS_INVALID_INDEX: Integer; cdecl;
    {class} function _GetKMS_INVALID_KCV: Integer; cdecl;
    {class} function _GetKMS_INVALID_KEK: Integer; cdecl;
    {class} function _GetKMS_INVALID_KEY: Integer; cdecl;
    {class} function _GetKMS_INVALID_LENGTH: Integer; cdecl;
    {class} function _GetKMS_INVALID_MODE: Integer; cdecl;
    {class} function _GetKMS_INVALID_OPERATION: Integer; cdecl;
    {class} function _GetKMS_INVALID_PIN_LEN: Integer; cdecl;
    {class} function _GetKMS_INVALID_PURPOSE: Integer; cdecl;
    {class} function _GetKMS_INVALID_TYPE: Integer; cdecl;
    {class} function _GetKMS_KEY_EXPIRED: Integer; cdecl;
    {class} function _GetKMS_KEY_NOT_UNIQUE: Integer; cdecl;
    {class} function _GetKMS_KEY_ON_COOLDOWN: Integer; cdecl;
    {class} function _GetKMS_NULL_PIN: Integer; cdecl;
    {class} function _GetKMS_PINMODE_ACTIVE: Integer; cdecl;
    {class} function _GetKMS_PINMODE_INACTIVE: Integer; cdecl;
    {class} function _GetKMS_PINMODE_NO_KEY: Integer; cdecl;
    {class} function _GetKMS_USER_CANCELLED: Integer; cdecl;
    {class} function _GetLINUX_ERROR: Integer; cdecl;
    {class} function _GetMODULE_NOT_FOUND: Integer; cdecl;
    {class} function _GetMSR_ERROR: Integer; cdecl;
    {class} function _GetMSR_NO_SWIPE: Integer; cdecl;
    {class} function _GetMSR_TRACK_ERROR: Integer; cdecl;
    {class} function _GetNOT_AVAILABLE: Integer; cdecl;
    {class} function _GetNOT_SUPPORTED: Integer; cdecl;
    {class} function _GetNULL_PARAMETER: Integer; cdecl;
    {class} function _GetOK: Integer; cdecl;
    {class} function _GetOUT_OF_BOUNDS: Integer; cdecl;
    {class} function _GetPLATFORM_ERROR: Integer; cdecl;
    {class} function _GetPM_ALREADY_SEALED: Integer; cdecl;
    {class} function _GetPM_AP_NAME_REPEATED: Integer; cdecl;
    {class} function _GetPM_ERROR: Integer; cdecl;
    {class} function _GetPM_FULL: Integer; cdecl;
    {class} function _GetPM_INVALID_AP: Integer; cdecl;
    {class} function _GetPM_INVALID_FILE: Integer; cdecl;
    {class} function _GetPM_NOT_SEALED: Integer; cdecl;
    {class} function _GetPM_SEAL_ERROR: Integer; cdecl;
    {class} function _GetPM_SIGN_ERROR: Integer; cdecl;
    {class} function _GetPM_ULD_FAIL: Integer; cdecl;
    {class} function _GetRSA_GEN_FAIL: Integer; cdecl;
    {class} function _GetSMART_ABSENT: Integer; cdecl;
    {class} function _GetSMART_ATR_TOO_LONG: Integer; cdecl;
    {class} function _GetSMART_BAD_TS: Integer; cdecl;
    {class} function _GetSMART_COMM_ERROR: Integer; cdecl;
    {class} function _GetSMART_DEACTIVATED_PROTOCOL: Integer; cdecl;
    {class} function _GetSMART_EDC_ERROR: Integer; cdecl;
    {class} function _GetSMART_ERROR: Integer; cdecl;
    {class} function _GetSMART_INVALID_ATR: Integer; cdecl;
    {class} function _GetSMART_MUTE: Integer; cdecl;
    {class} function _GetSMART_NOT_ACTIVATED: Integer; cdecl;
    {class} function _GetSMART_PARITY_ERROR: Integer; cdecl;
    {class} function _GetSMART_POWER_FAILED: Integer; cdecl;
    {class} function _GetSMART_PROCEDURE_BYTE_CONFLICT: Integer; cdecl;
    {class} function _GetSMART_PTS_RESPONSE_ERROR: Integer; cdecl;
    {class} function _GetSMART_TA1_NOT_SUPPORTED: Integer; cdecl;
    {class} function _GetSYS_ERROR: Integer; cdecl;
    {class} function _GetTEST_FAIL: Integer; cdecl;
    {class} function _GetTIMEOUT: Integer; cdecl;
    {class} property BUFFER_NOT_ENOUGH: Integer read _GetBUFFER_NOT_ENOUGH;
    {class} property CANCELLED: Integer read _GetCANCELLED;
    {class} property CLOCK_ERROR: Integer read _GetCLOCK_ERROR;
    {class} property CL_BYTE_ERROR: Integer read _GetCL_BYTE_ERROR;
    {class} property CL_COLLISION: Integer read _GetCL_COLLISION;
    {class} property CL_CRC_ERROR: Integer read _GetCL_CRC_ERROR;
    {class} property CL_ERROR: Integer read _GetCL_ERROR;
    {class} property CL_NO_RESPONSE: Integer read _GetCL_NO_RESPONSE;
    {class} property CL_PROTOCOL_ERROR: Integer read _GetCL_PROTOCOL_ERROR;
    {class} property CL_SN: Integer read _GetCL_SN;
    {class} property COULD_NOT_ALLOCATE_MEMORY: Integer read _GetCOULD_NOT_ALLOCATE_MEMORY;
    {class} property CRYPT_ERROR: Integer read _GetCRYPT_ERROR;
    {class} property END_OF_LIST: Integer read _GetEND_OF_LIST;
    {class} property FS_ACCESS_DENIED: Integer read _GetFS_ACCESS_DENIED;
    {class} property FS_ALREADY_EXIST: Integer read _GetFS_ALREADY_EXIST;
    {class} property FS_EOF: Integer read _GetFS_EOF;
    {class} property FS_ERROR: Integer read _GetFS_ERROR;
    {class} property FS_INVALID_HANDLE: Integer read _GetFS_INVALID_HANDLE;
    {class} property FS_INVALID_NAME: Integer read _GetFS_INVALID_NAME;
    {class} property FS_NOT_EXIST: Integer read _GetFS_NOT_EXIST;
    {class} property FUNCTION_NOT_FOUND: Integer read _GetFUNCTION_NOT_FOUND;
    {class} property INFO_ERROR: Integer read _GetINFO_ERROR;
    {class} property INVALID_PARAMETER: Integer read _GetINVALID_PARAMETER;
    {class} property KMS_ERROR: Integer read _GetKMS_ERROR;
    {class} property KMS_FULL: Integer read _GetKMS_FULL;
    {class} property KMS_INVALID_CERTIFICATE: Integer read _GetKMS_INVALID_CERTIFICATE;
    {class} property KMS_INVALID_HASH: Integer read _GetKMS_INVALID_HASH;
    {class} property KMS_INVALID_INDEX: Integer read _GetKMS_INVALID_INDEX;
    {class} property KMS_INVALID_KCV: Integer read _GetKMS_INVALID_KCV;
    {class} property KMS_INVALID_KEK: Integer read _GetKMS_INVALID_KEK;
    {class} property KMS_INVALID_KEY: Integer read _GetKMS_INVALID_KEY;
    {class} property KMS_INVALID_LENGTH: Integer read _GetKMS_INVALID_LENGTH;
    {class} property KMS_INVALID_MODE: Integer read _GetKMS_INVALID_MODE;
    {class} property KMS_INVALID_OPERATION: Integer read _GetKMS_INVALID_OPERATION;
    {class} property KMS_INVALID_PIN_LEN: Integer read _GetKMS_INVALID_PIN_LEN;
    {class} property KMS_INVALID_PURPOSE: Integer read _GetKMS_INVALID_PURPOSE;
    {class} property KMS_INVALID_TYPE: Integer read _GetKMS_INVALID_TYPE;
    {class} property KMS_KEY_EXPIRED: Integer read _GetKMS_KEY_EXPIRED;
    {class} property KMS_KEY_NOT_UNIQUE: Integer read _GetKMS_KEY_NOT_UNIQUE;
    {class} property KMS_KEY_ON_COOLDOWN: Integer read _GetKMS_KEY_ON_COOLDOWN;
    {class} property KMS_NULL_PIN: Integer read _GetKMS_NULL_PIN;
    {class} property KMS_PINMODE_ACTIVE: Integer read _GetKMS_PINMODE_ACTIVE;
    {class} property KMS_PINMODE_INACTIVE: Integer read _GetKMS_PINMODE_INACTIVE;
    {class} property KMS_PINMODE_NO_KEY: Integer read _GetKMS_PINMODE_NO_KEY;
    {class} property KMS_USER_CANCELLED: Integer read _GetKMS_USER_CANCELLED;
    {class} property LINUX_ERROR: Integer read _GetLINUX_ERROR;
    {class} property MODULE_NOT_FOUND: Integer read _GetMODULE_NOT_FOUND;
    {class} property MSR_ERROR: Integer read _GetMSR_ERROR;
    {class} property MSR_NO_SWIPE: Integer read _GetMSR_NO_SWIPE;
    {class} property MSR_TRACK_ERROR: Integer read _GetMSR_TRACK_ERROR;
    {class} property NOT_AVAILABLE: Integer read _GetNOT_AVAILABLE;
    {class} property NOT_SUPPORTED: Integer read _GetNOT_SUPPORTED;
    {class} property NULL_PARAMETER: Integer read _GetNULL_PARAMETER;
    {class} property OK: Integer read _GetOK;
    {class} property OUT_OF_BOUNDS: Integer read _GetOUT_OF_BOUNDS;
    {class} property PLATFORM_ERROR: Integer read _GetPLATFORM_ERROR;
    {class} property PM_ALREADY_SEALED: Integer read _GetPM_ALREADY_SEALED;
    {class} property PM_AP_NAME_REPEATED: Integer read _GetPM_AP_NAME_REPEATED;
    {class} property PM_ERROR: Integer read _GetPM_ERROR;
    {class} property PM_FULL: Integer read _GetPM_FULL;
    {class} property PM_INVALID_AP: Integer read _GetPM_INVALID_AP;
    {class} property PM_INVALID_FILE: Integer read _GetPM_INVALID_FILE;
    {class} property PM_NOT_SEALED: Integer read _GetPM_NOT_SEALED;
    {class} property PM_SEAL_ERROR: Integer read _GetPM_SEAL_ERROR;
    {class} property PM_SIGN_ERROR: Integer read _GetPM_SIGN_ERROR;
    {class} property PM_ULD_FAIL: Integer read _GetPM_ULD_FAIL;
    {class} property RSA_GEN_FAIL: Integer read _GetRSA_GEN_FAIL;
    {class} property SMART_ABSENT: Integer read _GetSMART_ABSENT;
    {class} property SMART_ATR_TOO_LONG: Integer read _GetSMART_ATR_TOO_LONG;
    {class} property SMART_BAD_TS: Integer read _GetSMART_BAD_TS;
    {class} property SMART_COMM_ERROR: Integer read _GetSMART_COMM_ERROR;
    {class} property SMART_DEACTIVATED_PROTOCOL: Integer read _GetSMART_DEACTIVATED_PROTOCOL;
    {class} property SMART_EDC_ERROR: Integer read _GetSMART_EDC_ERROR;
    {class} property SMART_ERROR: Integer read _GetSMART_ERROR;
    {class} property SMART_INVALID_ATR: Integer read _GetSMART_INVALID_ATR;
    {class} property SMART_MUTE: Integer read _GetSMART_MUTE;
    {class} property SMART_NOT_ACTIVATED: Integer read _GetSMART_NOT_ACTIVATED;
    {class} property SMART_PARITY_ERROR: Integer read _GetSMART_PARITY_ERROR;
    {class} property SMART_POWER_FAILED: Integer read _GetSMART_POWER_FAILED;
    {class} property SMART_PROCEDURE_BYTE_CONFLICT: Integer read _GetSMART_PROCEDURE_BYTE_CONFLICT;
    {class} property SMART_PTS_RESPONSE_ERROR: Integer read _GetSMART_PTS_RESPONSE_ERROR;
    {class} property SMART_TA1_NOT_SUPPORTED: Integer read _GetSMART_TA1_NOT_SUPPORTED;
    {class} property SYS_ERROR: Integer read _GetSYS_ERROR;
    {class} property TEST_FAIL: Integer read _GetTEST_FAIL;
    {class} property TIMEOUT: Integer read _GetTIMEOUT;
  end;

  [JavaSignature('br/com/gertec/gedi/GEDI_RET')]
  JGEDI_RET = interface(IJavaInstance)
    ['{872790BE-31F4-4EC4-9A46-E378B2D8CC37}']
  end;
  TJGEDI_RET = class(TJavaGenericImport<JGEDI_RETClass, JGEDI_RET>) end;

  JGEDI_KMS_st_Control_CallbacksClass = interface(IJavaClass)
    ['{EF0CED13-68FC-44D9-9C2D-10489A37D1F1}']
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_KMS_st_Control$Callbacks')]
  JGEDI_KMS_st_Control_Callbacks = interface(IJavaInstance)
    ['{9E165F0C-0926-43A6-B606-450ACD009EDF}']
    procedure onKeyPress(P1: JGEDI_KBD_e_Key); cdecl;
    function testCancel: Boolean; cdecl;
  end;
  TJGEDI_KMS_st_Control_Callbacks = class(TJavaGenericImport<JGEDI_KMS_st_Control_CallbacksClass, JGEDI_KMS_st_Control_Callbacks>) end;

  JGediNativeClass = interface(JGEDI_KMS_st_Control_CallbacksClass)
    ['{212A89FA-805D-4865-8E9D-8513CC778D15}']
    {class} function CLOCK_RTCGet(P1: TJavaArray<Byte>; P2: TJavaArray<Byte>; P3: TJavaArray<Byte>; P4: TJavaArray<Byte>; P5: TJavaArray<Byte>; P6: TJavaArray<Byte>; P7: TJavaArray<Byte>): Integer; cdecl;//Deprecated
    {class} function CLOCK_RTCSet(P1: Byte; P2: Byte; P3: Byte; P4: Byte; P5: Byte; P6: Byte; P7: Byte): Integer; cdecl;//Deprecated
    {class} function CL_ISOPolling(P1: Integer): Integer; cdecl;//Deprecated
    {class} function CL_PowerOff: Integer; cdecl;//Deprecated
    {class} function CL_PowerOn: Integer; cdecl;//Deprecated
    {class} function CL_ResetEMV(P1: TJavaArray<Byte>; P2: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function CL_SendAPDU(P1: TJavaArray<Byte>; P2: Integer; P3: TJavaArray<Byte>; P4: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function CRYPT_DES(P1: Integer; P2: TJavaArray<Byte>; P3: TJavaArray<Byte>; P4: TJavaArray<Byte>): Integer; cdecl;//Deprecated
    {class} function CRYPT_RNG(P1: TJavaArray<Byte>): Integer; cdecl;//Deprecated
    {class} function CRYPT_RSA(P1: TJavaArray<Byte>; P2: TJavaArray<Byte>; P3: TJavaArray<Byte>; P4: TJavaArray<Byte>): Integer; cdecl;//Deprecated
    {class} function CRYPT_SHA1(P1: TJavaArray<Byte>; P2: TJavaArray<Byte>): Integer; cdecl;//Deprecated
    {class} function INFO_ControlNumberGet(P1: Integer; P2: TJavaArray<Byte>): Integer; cdecl;//Deprecated
    {class} function INFO_ControlNumberSet(P1: Integer; P2: TJavaArray<Byte>): Integer; cdecl;//Deprecated
    {class} function INFO_FirmwareVersionGet(P1: TJavaArray<Byte>): Integer; cdecl;//Deprecated
    {class} function KBD_Get(P1: TJavaArray<Integer>; P2: Integer; P3: Boolean): Integer; cdecl;//Deprecated
    {class} function KMS_CapturePIN(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Boolean; P6: Boolean; P7: TJavaArray<Byte>; P8: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function KMS_DUKPTKSNGet(P1: Integer; P2: Integer; P3: Integer; P4: TJavaArray<Byte>): Integer; cdecl;//Deprecated
    {class} function KMS_EncryptData(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: TJavaArray<Byte>; P6: TJavaArray<Byte>; P7: TJavaArray<Byte>; P8: TJavaArray<Byte>; P9: TJavaArray<Integer>; P10: TJavaArray<Byte>): Integer; cdecl;//Deprecated
    {class} function KMS_GetPINBlock(P1: Integer; P2: Integer; P3: TJavaArray<Byte>; P4: Integer; P5: JString; P6: TJavaArray<Byte>; P7: TJavaArray<Byte>): Integer; cdecl;//Deprecated
    {class} function KMS_KeyPresenceTest(P1: Integer; P2: Integer; P3: Integer): Integer; cdecl;//Deprecated
    {class} function KMS_Reset: Integer; cdecl;//Deprecated
    {class} function KMS_SavePlainKey(P1: Integer; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Byte>): Integer; cdecl;//Deprecated
    {class} function LED_Set(P1: Integer; P2: Byte): Integer; cdecl;//Deprecated
    {class} function MSR_LastErrorGet(P1: TJavaArray<Integer>; P2: TJavaArray<Integer>; P3: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function MSR_Read(P1: TJavaArray<Byte>; P2: TJavaArray<Integer>; P3: TJavaArray<Byte>; P4: TJavaArray<Integer>; P5: TJavaArray<Byte>; P6: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function SMART_PowerOff(P1: Integer): Integer; cdecl;//Deprecated
    {class} function SMART_ResetEMV(P1: Integer; P2: Integer; P3: TJavaArray<Byte>; P4: TJavaArray<Integer>; P5: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function SMART_SendAPDU(P1: Integer; P2: TJavaArray<Byte>; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function SMART_Status(P1: Integer; P2: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function SMART_WarmResetEMV(P1: Integer; P2: Integer; P3: TJavaArray<Byte>; P4: TJavaArray<Integer>; P5: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function VersionGet(P1: TJavaArray<Integer>; P2: TJavaArray<Integer>; P3: TJavaArray<Integer>; P4: TJavaArray<Integer>): Integer; cdecl; overload;//Deprecated
    {class} function VersionGet(P1: TJavaArray<Integer>; P2: TJavaArray<Integer>; P3: TJavaArray<Integer>; P4: TJavaArray<Int64>): Integer; cdecl; overload;//Deprecated
    {class} function getInstance(P1: Jimpl_Gedi): JGediNative; cdecl;//Deprecated
    {class} function init(P1: Jimpl_Gedi): JGediNative; cdecl; overload;//Deprecated
    {class} procedure onKeyPress(P1: Integer); cdecl; overload;//Deprecated
    {class} procedure onKeyPress(P1: JGEDI_KBD_e_Key); cdecl; overload;//Deprecated
    {class} procedure setPrivateDir(P1: JString); cdecl;//Deprecated
    {class} function testCancel: Boolean; cdecl;//Deprecated
  end;

  [JavaSignature('br/com/gertec/gedi/GediNative')]
  JGediNative = interface(JGEDI_KMS_st_Control_Callbacks)
    ['{D5975880-D4B0-4301-B095-41AE8591D9F4}']
  end;
  TJGediNative = class(TJavaGenericImport<JGediNativeClass, JGediNative>) end;

  JIINFOClass = interface(IJavaClass)
    ['{1112913E-1EFB-4B9E-8DF7-AB61DC47FEAF}']
  end;

  [JavaSignature('br/com/gertec/gedi/interfaces/IINFO')]
  JIINFO = interface(IJavaInstance)
    ['{90E24DEB-583E-4184-AF57-DA2CC1DDE579}']
    function ControlNumberGet(P1: JGEDI_INFO_e_ControlNumber): JString; cdecl;
    procedure ControlNumberSet(P1: JGEDI_INFO_e_ControlNumber; P2: JString); cdecl;
    function FirmwareVersionGet: JString; cdecl;
    function ProcessorIDGet: JString; cdecl;
  end;
  TJIINFO = class(TJavaGenericImport<JIINFOClass, JIINFO>) end;

  JINFOClass = interface(JIINFOClass)
    ['{E228927C-3363-4D50-9A03-55C6E73EF80E}']
    {class} function ControlNumberGet(P1: JGEDI_INFO_e_ControlNumber): JString; cdecl; overload;
    {class} function ControlNumberGet(P1: Integer; P2: TJavaArray<Byte>): Integer; cdecl; overload;
    {class} function ControlNumberSet(P1: Integer; P2: TJavaArray<Byte>): Integer; cdecl; overload;
    {class} procedure ControlNumberSet(P1: JGEDI_INFO_e_ControlNumber; P2: JString); cdecl; overload;
    {class} function FirmwareVersionGet: JString; cdecl; overload;
    {class} function FirmwareVersionGet(P1: TJavaArray<Byte>): Integer; cdecl; overload;
    {class} function ProcessorIDGet: JString; cdecl; overload;
    {class} function ProcessorIDGet(P1: TJavaArray<Byte>): Integer; cdecl; overload;
    {class} function init: JINFO; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/INFO')]
  JINFO = interface(JIINFO)
    ['{E04E7516-7096-46E0-9D17-88A3A409DEE5}']
  end;
  TJINFO = class(TJavaGenericImport<JINFOClass, JINFO>) end;

  JIKBDClass = interface(IJavaClass)
    ['{8B2D30A7-CB69-4D39-93FC-2CE5B5344607}']
  end;

  [JavaSignature('br/com/gertec/gedi/interfaces/IKBD')]
  JIKBD = interface(IJavaInstance)
    ['{19A52B09-287E-45A0-A428-BA8EE8603640}']
    function &Get(P1: Integer; P2: Boolean): JGEDI_KBD_e_Key; cdecl;
    function PowerKeyModeGet: JGEDI_KBD_e_PowerKeyMode; cdecl;
    procedure PowerKeyModeSet(P1: JGEDI_KBD_e_PowerKeyMode); cdecl;
    function Scan: JGEDI_KBD_e_Key; cdecl;
    procedure &Set(P1: JGEDI_KBD_st_Info); cdecl;
    procedure SoundSet(P1: Boolean; P2: Integer; P3: Integer); cdecl;
  end;
  TJIKBD = class(TJavaGenericImport<JIKBDClass, JIKBD>) end;

  JKBDClass = interface(JIKBDClass)
    ['{666D9A3F-F968-42FF-BE4B-0C7E2E1D6182}']
    {class} function _GetKEY_ASTERISK: Integer; cdecl;
    {class} procedure _SetKEY_ASTERISK(Value: Integer); cdecl;
    {class} function _GetKEY_CANCEL: Integer; cdecl;
    {class} procedure _SetKEY_CANCEL(Value: Integer); cdecl;
    {class} function _GetKEY_CLEAR: Integer; cdecl;
    {class} procedure _SetKEY_CLEAR(Value: Integer); cdecl;
    {class} function _GetKEY_DOT: Integer; cdecl;
    {class} procedure _SetKEY_DOT(Value: Integer); cdecl;
    {class} function _GetKEY_DOWN: Integer; cdecl;
    {class} procedure _SetKEY_DOWN(Value: Integer); cdecl;
    {class} function _GetKEY_ENTER: Integer; cdecl;
    {class} procedure _SetKEY_ENTER(Value: Integer); cdecl;
    {class} function _GetKEY_F1: Integer; cdecl;
    {class} procedure _SetKEY_F1(Value: Integer); cdecl;
    {class} function _GetKEY_F2: Integer; cdecl;
    {class} procedure _SetKEY_F2(Value: Integer); cdecl;
    {class} function _GetKEY_F3: Integer; cdecl;
    {class} procedure _SetKEY_F3(Value: Integer); cdecl;
    {class} function _GetKEY_F4: Integer; cdecl;
    {class} procedure _SetKEY_F4(Value: Integer); cdecl;
    {class} function _GetKEY_INVALID: Integer; cdecl;
    {class} procedure _SetKEY_INVALID(Value: Integer); cdecl;
    {class} function _GetKEY_NONE: Integer; cdecl;
    {class} procedure _SetKEY_NONE(Value: Integer); cdecl;
    {class} function _GetKEY_NUM0: Integer; cdecl;
    {class} procedure _SetKEY_NUM0(Value: Integer); cdecl;
    {class} function _GetKEY_NUM1: Integer; cdecl;
    {class} procedure _SetKEY_NUM1(Value: Integer); cdecl;
    {class} function _GetKEY_NUM2: Integer; cdecl;
    {class} procedure _SetKEY_NUM2(Value: Integer); cdecl;
    {class} function _GetKEY_NUM3: Integer; cdecl;
    {class} procedure _SetKEY_NUM3(Value: Integer); cdecl;
    {class} function _GetKEY_NUM4: Integer; cdecl;
    {class} procedure _SetKEY_NUM4(Value: Integer); cdecl;
    {class} function _GetKEY_NUM5: Integer; cdecl;
    {class} procedure _SetKEY_NUM5(Value: Integer); cdecl;
    {class} function _GetKEY_NUM6: Integer; cdecl;
    {class} procedure _SetKEY_NUM6(Value: Integer); cdecl;
    {class} function _GetKEY_NUM7: Integer; cdecl;
    {class} procedure _SetKEY_NUM7(Value: Integer); cdecl;
    {class} function _GetKEY_NUM8: Integer; cdecl;
    {class} procedure _SetKEY_NUM8(Value: Integer); cdecl;
    {class} function _GetKEY_NUM9: Integer; cdecl;
    {class} procedure _SetKEY_NUM9(Value: Integer); cdecl;
    {class} function _GetKEY_POWER: Integer; cdecl;
    {class} procedure _SetKEY_POWER(Value: Integer); cdecl;
    {class} function _GetKEY_SHIFT: Integer; cdecl;
    {class} procedure _SetKEY_SHIFT(Value: Integer); cdecl;
    {class} function _GetKEY_UNKNOWN: Integer; cdecl;
    {class} procedure _SetKEY_UNKNOWN(Value: Integer); cdecl;
    {class} function _GetKEY_UP: Integer; cdecl;
    {class} procedure _SetKEY_UP(Value: Integer); cdecl;
    {class} function &Get(P1: Integer; P2: Boolean): JGEDI_KBD_e_Key; cdecl; overload;//Deprecated
    {class} function &Get(P1: TJavaArray<Integer>; P2: Integer; P3: Boolean): Integer; cdecl; overload;//Deprecated
    {class} function PowerKeyModeGet: JGEDI_KBD_e_PowerKeyMode; cdecl;//Deprecated
    {class} procedure PowerKeyModeSet(P1: JGEDI_KBD_e_PowerKeyMode); cdecl;//Deprecated
    {class} function Scan: JGEDI_KBD_e_Key; cdecl;//Deprecated
    {class} procedure SoundSet(P1: Boolean; P2: Integer; P3: Integer); cdecl;//Deprecated
    {class} function init: JKBD; cdecl;//Deprecated
    {class} property KEY_ASTERISK: Integer read _GetKEY_ASTERISK write _SetKEY_ASTERISK;
    {class} property KEY_CANCEL: Integer read _GetKEY_CANCEL write _SetKEY_CANCEL;
    {class} property KEY_CLEAR: Integer read _GetKEY_CLEAR write _SetKEY_CLEAR;
    {class} property KEY_DOT: Integer read _GetKEY_DOT write _SetKEY_DOT;
    {class} property KEY_DOWN: Integer read _GetKEY_DOWN write _SetKEY_DOWN;
    {class} property KEY_ENTER: Integer read _GetKEY_ENTER write _SetKEY_ENTER;
    {class} property KEY_F1: Integer read _GetKEY_F1 write _SetKEY_F1;
    {class} property KEY_F2: Integer read _GetKEY_F2 write _SetKEY_F2;
    {class} property KEY_F3: Integer read _GetKEY_F3 write _SetKEY_F3;
    {class} property KEY_F4: Integer read _GetKEY_F4 write _SetKEY_F4;
    {class} property KEY_INVALID: Integer read _GetKEY_INVALID write _SetKEY_INVALID;
    {class} property KEY_NONE: Integer read _GetKEY_NONE write _SetKEY_NONE;
    {class} property KEY_NUM0: Integer read _GetKEY_NUM0 write _SetKEY_NUM0;
    {class} property KEY_NUM1: Integer read _GetKEY_NUM1 write _SetKEY_NUM1;
    {class} property KEY_NUM2: Integer read _GetKEY_NUM2 write _SetKEY_NUM2;
    {class} property KEY_NUM3: Integer read _GetKEY_NUM3 write _SetKEY_NUM3;
    {class} property KEY_NUM4: Integer read _GetKEY_NUM4 write _SetKEY_NUM4;
    {class} property KEY_NUM5: Integer read _GetKEY_NUM5 write _SetKEY_NUM5;
    {class} property KEY_NUM6: Integer read _GetKEY_NUM6 write _SetKEY_NUM6;
    {class} property KEY_NUM7: Integer read _GetKEY_NUM7 write _SetKEY_NUM7;
    {class} property KEY_NUM8: Integer read _GetKEY_NUM8 write _SetKEY_NUM8;
    {class} property KEY_NUM9: Integer read _GetKEY_NUM9 write _SetKEY_NUM9;
    {class} property KEY_POWER: Integer read _GetKEY_POWER write _SetKEY_POWER;
    {class} property KEY_SHIFT: Integer read _GetKEY_SHIFT write _SetKEY_SHIFT;
    {class} property KEY_UNKNOWN: Integer read _GetKEY_UNKNOWN write _SetKEY_UNKNOWN;
    {class} property KEY_UP: Integer read _GetKEY_UP write _SetKEY_UP;
  end;

  [JavaSignature('br/com/gertec/gedi/KBD')]
  JKBD = interface(JIKBD)
    ['{68792925-186A-4698-86F8-9B5BDA44A131}']
  end;
  TJKBD = class(TJavaGenericImport<JKBDClass, JKBD>) end;

  JKBDDataClass = interface(JObjectClass)
    ['{07113A28-1224-402F-9BF3-5602DF821F11}']
    {class} function _Getactivity: JActivity; cdecl;
    {class} procedure _Setactivity(Value: JActivity); cdecl;
    {class} function _Getbtn0: JButton; cdecl;
    {class} procedure _Setbtn0(Value: JButton); cdecl;
    {class} function _Getbtn1: JButton; cdecl;
    {class} procedure _Setbtn1(Value: JButton); cdecl;
    {class} function _Getbtn2: JButton; cdecl;
    {class} procedure _Setbtn2(Value: JButton); cdecl;
    {class} function _Getbtn3: JButton; cdecl;
    {class} procedure _Setbtn3(Value: JButton); cdecl;
    {class} function _Getbtn4: JButton; cdecl;
    {class} procedure _Setbtn4(Value: JButton); cdecl;
    {class} function _Getbtn5: JButton; cdecl;
    {class} procedure _Setbtn5(Value: JButton); cdecl;
    {class} function _Getbtn6: JButton; cdecl;
    {class} procedure _Setbtn6(Value: JButton); cdecl;
    {class} function _Getbtn7: JButton; cdecl;
    {class} procedure _Setbtn7(Value: JButton); cdecl;
    {class} function _Getbtn8: JButton; cdecl;
    {class} procedure _Setbtn8(Value: JButton); cdecl;
    {class} function _Getbtn9: JButton; cdecl;
    {class} procedure _Setbtn9(Value: JButton); cdecl;
    {class} function _GetbtnCancel: JButton; cdecl;
    {class} procedure _SetbtnCancel(Value: JButton); cdecl;
    {class} function _GetbtnClear: JButton; cdecl;
    {class} procedure _SetbtnClear(Value: JButton); cdecl;
    {class} function _GetbtnConfirm: JButton; cdecl;
    {class} procedure _SetbtnConfirm(Value: JButton); cdecl;
    {class} function checkNull: Boolean; cdecl;//Deprecated
    {class} function clone: JKBDData; cdecl;//Deprecated
    {class} function getButtonByIndex(P1: Integer): JButton; cdecl;//Deprecated
    {class} function init: JKBDData; cdecl; overload;//Deprecated
    {class} function init(P1: JGEDI_KBD_st_Info): JKBDData; cdecl; overload;//Deprecated
    {class} function init(P1: JButton; P2: JButton; P3: JButton; P4: JButton; P5: JButton; P6: JButton; P7: JButton; P8: JButton; P9: JButton; P10: JButton; P11: JButton; P12: JButton; P13: JButton; P14: JActivity): JKBDData; cdecl; overload;//Deprecated
    {class} procedure printLocation; cdecl;//Deprecated
    {class} property activity: JActivity read _Getactivity write _Setactivity;
    {class} property btn0: JButton read _Getbtn0 write _Setbtn0;
    {class} property btn1: JButton read _Getbtn1 write _Setbtn1;
    {class} property btn2: JButton read _Getbtn2 write _Setbtn2;
    {class} property btn3: JButton read _Getbtn3 write _Setbtn3;
    {class} property btn4: JButton read _Getbtn4 write _Setbtn4;
    {class} property btn5: JButton read _Getbtn5 write _Setbtn5;
    {class} property btn6: JButton read _Getbtn6 write _Setbtn6;
    {class} property btn7: JButton read _Getbtn7 write _Setbtn7;
    {class} property btn8: JButton read _Getbtn8 write _Setbtn8;
    {class} property btn9: JButton read _Getbtn9 write _Setbtn9;
    {class} property btnCancel: JButton read _GetbtnCancel write _SetbtnCancel;
    {class} property btnClear: JButton read _GetbtnClear write _SetbtnClear;
    {class} property btnConfirm: JButton read _GetbtnConfirm write _SetbtnConfirm;
  end;

  [JavaSignature('br/com/gertec/gedi/KBDData')]
  JKBDData = interface(JObject)
    ['{C81EE57E-75F6-4D4D-83DE-87311758F6A8}']
  end;
  TJKBDData = class(TJavaGenericImport<JKBDDataClass, JKBDData>) end;

  JIKMSClass = interface(IJavaClass)
    ['{60849D91-E087-45A7-8DF0-1362AECCBF8F}']
  end;

  [JavaSignature('br/com/gertec/gedi/interfaces/IKMS')]
  JIKMS = interface(IJavaInstance)
    ['{4C8E6A73-681A-42EA-B355-64A95D7C161F}']
    function CapturePINBlock(P1: JGEDI_KMS_st_Control; P2: Boolean; P3: JGEDI_KMS_st_Data; P4: JList): JGEDI_KMS_st_CapturePINBlockInfo; cdecl;
    function DUKPTKSNGet(P1: JGEDI_KMS_e_KEYTYPE; P2: JGEDI_KMS_e_KEYPURPOSE; P3: Integer): TJavaArray<Byte>; cdecl;
    function EncryptData(P1: JGEDI_KMS_st_Data): JGEDI_KMS_st_Data; cdecl;
    function KCVGet(P1: JGEDI_KMS_e_KEYTYPE; P2: JGEDI_KMS_e_KEYPURPOSE; P3: Integer): TJavaArray<Byte>; cdecl;
    function KeyPresenceTest(P1: JGEDI_KMS_e_KEYTYPE; P2: JGEDI_KMS_e_KEYPURPOSE; P3: Integer): Boolean; cdecl;
    procedure Reset; cdecl;
    procedure SavePlainKey(P1: JGEDI_KMS_st_SaveKey); cdecl;
  end;
  TJIKMS = class(TJavaGenericImport<JIKMSClass, JIKMS>) end;

  JKMSClass = interface(JIKMSClass)
    ['{AD04AC9D-E2B6-42DE-BF2F-0BBD65A96A63}']
    {class} function _GetKEYPURPOSE_AUTH: Integer; cdecl;
    {class} function _GetKEYPURPOSE_DATA: Integer; cdecl;
    {class} function _GetKEYPURPOSE_KEK: Integer; cdecl;
    {class} function _GetKEYPURPOSE_PIN: Integer; cdecl;
    {class} function _GetKEYPURPOSE_SRED: Integer; cdecl;
    {class} function _GetKEYTYPE_AES: Integer; cdecl;
    {class} function _GetKEYTYPE_DES: Integer; cdecl;
    {class} function _GetKEYTYPE_DUKPT_DES: Integer; cdecl;
    {class} function _GetKEYTYPE_DUKPT_TDES: Integer; cdecl;
    {class} function _GetKEYTYPE_RSA: Integer; cdecl;
    {class} function _GetKEYTYPE_TDES: Integer; cdecl;
    {class} function _GetOP_DECRYPT: Integer; cdecl;
    {class} function _GetOP_ENCRYPT: Integer; cdecl;
    {class} function CapturePIN(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Boolean; P6: JGEDI_KMS_st_Control_Callbacks; P7: Boolean; P8: TJavaArray<Byte>; P9: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function CapturePINBlock(P1: JGEDI_KMS_st_Control; P2: Boolean; P3: JGEDI_KMS_st_Data; P4: JList): JGEDI_KMS_st_CapturePINBlockInfo; cdecl;//Deprecated
    {class} function DUKPTKSNGet(P1: JGEDI_KMS_e_KEYTYPE; P2: JGEDI_KMS_e_KEYPURPOSE; P3: Integer): TJavaArray<Byte>; cdecl; overload;//Deprecated
    {class} function DUKPTKSNGet(P1: Integer; P2: Integer; P3: Integer; P4: TJavaArray<Byte>): Integer; cdecl; overload;//Deprecated
    {class} function EncryptData(P1: JGEDI_KMS_st_Data): JGEDI_KMS_st_Data; cdecl; overload;//Deprecated
    {class} function EncryptData(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: TJavaArray<Byte>; P6: TJavaArray<Byte>; P7: TJavaArray<Byte>; P8: TJavaArray<Byte>; P9: TJavaArray<Integer>; P10: TJavaArray<Byte>): Integer; cdecl; overload;//Deprecated
    {class} function GetPINBlock(P1: Integer; P2: Integer; P3: TJavaArray<Byte>; P4: Integer; P5: JString; P6: TJavaArray<Byte>; P7: TJavaArray<Byte>): Integer; cdecl;//Deprecated
    {class} function KCVGet(P1: JGEDI_KMS_e_KEYTYPE; P2: JGEDI_KMS_e_KEYPURPOSE; P3: Integer): TJavaArray<Byte>; cdecl; overload;//Deprecated
    {class} function KCVGet(P1: Integer; P2: Integer; P3: Integer; P4: TJavaArray<Byte>): Integer; cdecl; overload;//Deprecated
    {class} function KeyPresenceTest(P1: Integer; P2: Integer; P3: Integer): Integer; cdecl; overload;//Deprecated
    {class} function KeyPresenceTest(P1: JGEDI_KMS_e_KEYTYPE; P2: JGEDI_KMS_e_KEYPURPOSE; P3: Integer): Boolean; cdecl; overload;//Deprecated
    {class} procedure Reset; cdecl;//Deprecated
    {class} procedure SavePlainKey(P1: JGEDI_KMS_st_SaveKey); cdecl; overload;//Deprecated
    {class} function SavePlainKey(P1: Integer; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Byte>): Integer; cdecl; overload;//Deprecated
    {class} function init: JKMS; cdecl;//Deprecated
    {class} property KEYPURPOSE_AUTH: Integer read _GetKEYPURPOSE_AUTH;
    {class} property KEYPURPOSE_DATA: Integer read _GetKEYPURPOSE_DATA;
    {class} property KEYPURPOSE_KEK: Integer read _GetKEYPURPOSE_KEK;
    {class} property KEYPURPOSE_PIN: Integer read _GetKEYPURPOSE_PIN;
    {class} property KEYPURPOSE_SRED: Integer read _GetKEYPURPOSE_SRED;
    {class} property KEYTYPE_AES: Integer read _GetKEYTYPE_AES;
    {class} property KEYTYPE_DES: Integer read _GetKEYTYPE_DES;
    {class} property KEYTYPE_DUKPT_DES: Integer read _GetKEYTYPE_DUKPT_DES;
    {class} property KEYTYPE_DUKPT_TDES: Integer read _GetKEYTYPE_DUKPT_TDES;
    {class} property KEYTYPE_RSA: Integer read _GetKEYTYPE_RSA;
    {class} property KEYTYPE_TDES: Integer read _GetKEYTYPE_TDES;
    {class} property OP_DECRYPT: Integer read _GetOP_DECRYPT;
    {class} property OP_ENCRYPT: Integer read _GetOP_ENCRYPT;
  end;

  [JavaSignature('br/com/gertec/gedi/KMS')]
  JKMS = interface(JIKMS)
    ['{F077B0FD-212A-486E-9FAA-FE81F45D99D4}']
  end;
  TJKMS = class(TJavaGenericImport<JKMSClass, JKMS>) end;

  JKMS_St_SaveKeyClass = interface(JObjectClass)
    ['{866CDAFB-C66E-478A-8FD5-D6396C62BB3E}']
    {class} function _GetabKSN: TJavaArray<Byte>; cdecl;
    {class} procedure _SetabKSN(Value: TJavaArray<Byte>); cdecl;
    {class} function _GetabKey: TJavaArray<Byte>; cdecl;
    {class} procedure _SetabKey(Value: TJavaArray<Byte>); cdecl;
    {class} function _GetbVersion: Byte; cdecl;
    {class} procedure _SetbVersion(Value: Byte); cdecl;
    {class} function _GeteKeyPurpose: Integer; cdecl;
    {class} procedure _SeteKeyPurpose(Value: Integer); cdecl;
    {class} function _GeteKeyType: Integer; cdecl;
    {class} procedure _SeteKeyType(Value: Integer); cdecl;
    {class} function _GetuiKeyIndex: Integer; cdecl;
    {class} procedure _SetuiKeyIndex(Value: Integer); cdecl;
    {class} function _GetuiKeyLen: Integer; cdecl;
    {class} procedure _SetuiKeyLen(Value: Integer); cdecl;
    {class} function getDescription: JString; cdecl;//Deprecated
    {class} function getKeyPurpose: JString; cdecl;//Deprecated
    {class} function getKeyType: JString; cdecl;//Deprecated
    {class} function init: JKMS_St_SaveKey; cdecl;//Deprecated
    {class} property abKSN: TJavaArray<Byte> read _GetabKSN write _SetabKSN;
    {class} property abKey: TJavaArray<Byte> read _GetabKey write _SetabKey;
    {class} property bVersion: Byte read _GetbVersion write _SetbVersion;
    {class} property eKeyPurpose: Integer read _GeteKeyPurpose write _SeteKeyPurpose;
    {class} property eKeyType: Integer read _GeteKeyType write _SeteKeyType;
    {class} property uiKeyIndex: Integer read _GetuiKeyIndex write _SetuiKeyIndex;
    {class} property uiKeyLen: Integer read _GetuiKeyLen write _SetuiKeyLen;
  end;

  [JavaSignature('br/com/gertec/gedi/KMS$St_SaveKey')]
  JKMS_St_SaveKey = interface(JObject)
    ['{28CFC055-1BA8-4DD3-9AA6-A81FF8C048CD}']
  end;
  TJKMS_St_SaveKey = class(TJavaGenericImport<JKMS_St_SaveKeyClass, JKMS_St_SaveKey>) end;

  JILEDClass = interface(IJavaClass)
    ['{56D2826C-87F4-4D1E-A414-2921C7A33316}']
  end;

  [JavaSignature('br/com/gertec/gedi/interfaces/ILED')]
  JILED = interface(IJavaInstance)
    ['{37A7E441-F999-4940-9ECA-FD6E90AA582B}']
    procedure &Set(P1: JGEDI_LED_e_Id; P2: Boolean); cdecl;
  end;
  TJILED = class(TJavaGenericImport<JILEDClass, JILED>) end;

  JLEDClass = interface(JILEDClass)
    ['{B04D6338-78B1-4E15-AAFB-D0452802AC12}']
    {class} procedure &Set(P1: JGEDI_LED_e_Id; P2: Boolean); cdecl; overload;
    {class} function init: JLED; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/LED')]
  JLED = interface(JILED)
    ['{A8BB91C2-A810-42C6-9B8A-D2EBCF503255}']
  end;
  TJLED = class(TJavaGenericImport<JLEDClass, JLED>) end;

  JIMSRClass = interface(IJavaClass)
    ['{96235C1B-BAC3-400B-B85B-9BA51D0555B6}']
  end;

  [JavaSignature('br/com/gertec/gedi/interfaces/IMSR')]
  JIMSR = interface(IJavaInstance)
    ['{AE88B20C-072A-4C0E-A7DF-F2B7FC17BCDF}']
    function LastErrorGet: JGEDI_MSR_st_LastErrors; cdecl;
    function Read: JGEDI_MSR_st_Tracks; cdecl;
  end;
  TJIMSR = class(TJavaGenericImport<JIMSRClass, JIMSR>) end;

  JMSRClass = interface(JIMSRClass)
    ['{E09DE38D-1F3B-432B-8455-9ADDD7870BC6}']
    {class} function LastErrorGet: JGEDI_MSR_st_LastErrors; cdecl; overload;
    {class} function LastErrorGet(P1: TJavaArray<Integer>; P2: TJavaArray<Integer>; P3: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function Read: JGEDI_MSR_st_Tracks; cdecl; overload;
    {class} function Read(P1: TJavaArray<Byte>; P2: TJavaArray<Integer>; P3: TJavaArray<Byte>; P4: TJavaArray<Integer>; P5: TJavaArray<Byte>; P6: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function init: JMSR; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/MSR')]
  JMSR = interface(JIMSR)
    ['{66846832-9B8F-41D4-8B54-8877E487D37C}']
  end;
  TJMSR = class(TJavaGenericImport<JMSRClass, JMSR>) end;

  JIPMClass = interface(IJavaClass)
    ['{1A31F8B6-9B12-4841-88DA-1AF292EFA00C}']
  end;

  [JavaSignature('br/com/gertec/gedi/interfaces/IPM')]
  JIPM = interface(IJavaInstance)
    ['{334414DC-58A1-4BEF-9783-07C6AC656488}']
    procedure ApDefaultSet(P1: JString); cdecl;
    procedure ApDelete(P1: JString); cdecl;
    procedure UpdateFromFile(P1: JString; P2: JGEDI_FS_e_Storage); cdecl;
  end;
  TJIPM = class(TJavaGenericImport<JIPMClass, JIPM>) end;

  JPMClass = interface(JIPMClass)
    ['{54040CF1-F6B2-4208-9AFC-F085805D7FB7}']
    {class} procedure ApDefaultSet(P1: JString); cdecl;
    {class} procedure ApDelete(P1: JString); cdecl;
    {class} procedure UpdateFromFile(P1: JString; P2: JGEDI_FS_e_Storage); cdecl;
    {class} function init: JPM; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/PM')]
  JPM = interface(JIPM)
    ['{DDD8B5D2-932B-4FE1-9D91-9E5AC32CFE76}']
  end;
  TJPM = class(TJavaGenericImport<JPMClass, JPM>) end;

  JIPRNTRClass = interface(IJavaClass)
    ['{79766C1E-99B2-4F07-A622-CC1892C72C21}']
  end;

  [JavaSignature('br/com/gertec/gedi/interfaces/IPRNTR')]
  JIPRNTR = interface(IJavaInstance)
    ['{D94AB524-80E0-4C47-AB38-BBD11FC7CB9A}']
    procedure DrawBarCode(P1: JGEDI_PRNTR_st_BarCodeConfig; P2: JString); cdecl;
    procedure DrawBlankLine(P1: Integer); cdecl;
    procedure DrawPictureExt(P1: JGEDI_PRNTR_st_PictureConfig; P2: JBitmap); cdecl;
    procedure DrawStringExt(P1: JGEDI_PRNTR_st_StringConfig; P2: JString); cdecl;
    function GetPaperUsage: Integer; cdecl;
    procedure Init; cdecl;
    procedure Output; cdecl;
    procedure ResetPaperUsage; cdecl;
    function Status: JGEDI_PRNTR_e_Status; cdecl;
  end;
  TJIPRNTR = class(TJavaGenericImport<JIPRNTRClass, JIPRNTR>) end;

  JPRNTRClass = interface(JIPRNTRClass)
    ['{274C3F54-0731-4F05-B79E-438CD668EB63}']
    {class} procedure DrawBarCode(P1: JGEDI_PRNTR_st_BarCodeConfig; P2: JString); cdecl;
    {class} procedure DrawBlankLine(P1: Integer); cdecl;
    {class} procedure DrawPictureExt(P1: JGEDI_PRNTR_st_PictureConfig; P2: JBitmap); cdecl;
    {class} procedure DrawStringExt(P1: JGEDI_PRNTR_st_StringConfig; P2: JString); cdecl;
    {class} function GetPaperUsage: Integer; cdecl;
    {class} procedure Init; cdecl;
    {class} procedure Output; cdecl;
    {class} procedure ResetPaperUsage; cdecl;
    {class} function Status: JGEDI_PRNTR_e_Status; cdecl;
    //{class} function init: JPRNTR; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/PRNTR')]
  JPRNTR = interface(JIPRNTR)
    ['{BAFC04FE-5C78-4D2F-A938-87AE05FA5D05}']
  end;
  TJPRNTR = class(TJavaGenericImport<JPRNTRClass, JPRNTR>) end;

  JISMARTClass = interface(IJavaClass)
    ['{B152BF49-A6C4-49C9-A802-EB6340B427FB}']
  end;

  [JavaSignature('br/com/gertec/gedi/interfaces/ISMART')]
  JISMART = interface(IJavaInstance)
    ['{0A26000D-F2BF-4BCC-A11F-F7E27B4A211A}']
    procedure PowerOff(P1: JGEDI_SMART_e_Slot); cdecl;
    function ResetEMV(P1: JGEDI_SMART_e_Slot; P2: JGEDI_SMART_e_Voltage): JGEDI_SMART_st_ResetInfo; cdecl;
    function SendAPDU(P1: JGEDI_SMART_e_Slot; P2: TJavaArray<Byte>): TJavaArray<Byte>; cdecl;
    function Status(P1: JGEDI_SMART_e_Slot): JGEDI_SMART_e_Status; cdecl;
    function WarmResetEMV(P1: JGEDI_SMART_e_Slot; P2: JGEDI_SMART_e_Voltage): JGEDI_SMART_st_ResetInfo; cdecl;
  end;
  TJISMART = class(TJavaGenericImport<JISMARTClass, JISMART>) end;

  JSMARTClass = interface(JISMARTClass)
    ['{988855E8-713D-4187-89ED-415E43BBDEBE}']
    {class} procedure PowerOff(P1: JGEDI_SMART_e_Slot); cdecl; overload;
    {class} function ResetEMV(P1: JGEDI_SMART_e_Slot; P2: JGEDI_SMART_e_Voltage): JGEDI_SMART_st_ResetInfo; cdecl; overload;
    {class} function SendAPDU(P1: JGEDI_SMART_e_Slot; P2: TJavaArray<Byte>): TJavaArray<Byte>; cdecl; overload;
    {class} function Status(P1: JGEDI_SMART_e_Slot): JGEDI_SMART_e_Status; cdecl; overload;
    {class} function WarmResetEMV(P1: JGEDI_SMART_e_Slot; P2: JGEDI_SMART_e_Voltage): JGEDI_SMART_st_ResetInfo; cdecl; overload;
    {class} function init: JSMART; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/SMART')]
  JSMART = interface(JISMART)
    ['{35580BE6-1C9D-4068-8F7D-6372C5E4D2EC}']
  end;
  TJSMART = class(TJavaGenericImport<JSMARTClass, JSMART>) end;

  JISYSClass = interface(IJavaClass)
    ['{903C75EF-2342-4A5C-8D32-0281943F053D}']
  end;

  [JavaSignature('br/com/gertec/gedi/interfaces/ISYS')]
  JISYS = interface(IJavaInstance)
    ['{F1E3984A-1B87-4273-8863-8E261C3CAFA5}']
    procedure SecuritySetup(P1: Integer; P2: Integer); cdecl;
  end;
  TJISYS = class(TJavaGenericImport<JISYSClass, JISYS>) end;

  JSYSClass = interface(JISYSClass)
    ['{0AF5E13D-3AE3-4743-B33B-DF30F1514ED0}']
    {class} procedure SecuritySetup(P1: Integer; P2: Integer); cdecl;
    {class} function init: JSYS; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/SYS')]
  JSYS = interface(JISYS)
    ['{6892C2DA-4401-45FF-95B7-2CDA6527829B}']
  end;
  TJSYS = class(TJavaGenericImport<JSYSClass, JSYS>) end;

  JGEDI_CL_e_ISO_LevelClass = interface(JEnumClass)
    ['{323D7E60-DA05-4A55-A64C-4C121B7E3997}']
    {class} function _GetLEVEL_14443_3: JGEDI_CL_e_ISO_Level; cdecl;
    {class} function _GetLEVEL_14443_4: JGEDI_CL_e_ISO_Level; cdecl;
    {class} function _GetLEVEL_INACTIVE: JGEDI_CL_e_ISO_Level; cdecl;
    {class} function getValue: Integer; cdecl;//Deprecated
    {class} function valueOf(P1: JString): JGEDI_CL_e_ISO_Level; cdecl;//Deprecated
    {class} function values: TJavaObjectArray<JGEDI_CL_e_ISO_Level>; cdecl;//Deprecated
    {class} property LEVEL_14443_3: JGEDI_CL_e_ISO_Level read _GetLEVEL_14443_3;
    {class} property LEVEL_14443_4: JGEDI_CL_e_ISO_Level read _GetLEVEL_14443_4;
    {class} property LEVEL_INACTIVE: JGEDI_CL_e_ISO_Level read _GetLEVEL_INACTIVE;
  end;

  [JavaSignature('br/com/gertec/gedi/enums/GEDI_CL_e_ISO_Level')]
  JGEDI_CL_e_ISO_Level = interface(JEnum)
    ['{D42E115E-0428-4A01-8FE0-5E57F6D6692D}']
  end;
  TJGEDI_CL_e_ISO_Level = class(TJavaGenericImport<JGEDI_CL_e_ISO_LevelClass, JGEDI_CL_e_ISO_Level>) end;

  JGEDI_CL_e_ISO_TypeClass = interface(JEnumClass)
    ['{9C15F52A-92CC-45F4-9C8B-7068FB41EB2D}']
    {class} function _GetA_3: JGEDI_CL_e_ISO_Type; cdecl;
    {class} function _GetA_4: JGEDI_CL_e_ISO_Type; cdecl;
    {class} function _GetB: JGEDI_CL_e_ISO_Type; cdecl;
    {class} function _GetERR: JGEDI_CL_e_ISO_Type; cdecl;
    {class} function getValue: Integer; cdecl;//Deprecated
    {class} function valueOf(P1: Integer): JGEDI_CL_e_ISO_Type; cdecl; overload;//Deprecated
    {class} function valueOf(P1: JString): JGEDI_CL_e_ISO_Type; cdecl; overload;//Deprecated
    {class} function values: TJavaObjectArray<JGEDI_CL_e_ISO_Type>; cdecl;//Deprecated
    {class} property A_3: JGEDI_CL_e_ISO_Type read _GetA_3;
    {class} property A_4: JGEDI_CL_e_ISO_Type read _GetA_4;
    {class} property B: JGEDI_CL_e_ISO_Type read _GetB;
    {class} property ERR: JGEDI_CL_e_ISO_Type read _GetERR;
  end;

  [JavaSignature('br/com/gertec/gedi/enums/GEDI_CL_e_ISO_Type')]
  JGEDI_CL_e_ISO_Type = interface(JEnum)
    ['{B5CCFB81-F14C-4B0F-8FBD-5E77C58821F0}']
  end;
  TJGEDI_CL_e_ISO_Type = class(TJavaGenericImport<JGEDI_CL_e_ISO_TypeClass, JGEDI_CL_e_ISO_Type>) end;

  JGEDI_CL_e_MF_KeyTypeClass = interface(JEnumClass)
    ['{27350CC5-3E1B-4CC1-B8E5-4CDFBEC5ED10}']
    {class} function _GetKEY_A: JGEDI_CL_e_MF_KeyType; cdecl;
    {class} function _GetKEY_B: JGEDI_CL_e_MF_KeyType; cdecl;
    {class} function getValue: Integer; cdecl;//Deprecated
    {class} function valueOf(P1: Integer): JGEDI_CL_e_MF_KeyType; cdecl; overload;//Deprecated
    {class} function valueOf(P1: JString): JGEDI_CL_e_MF_KeyType; cdecl; overload;//Deprecated
    {class} function values: TJavaObjectArray<JGEDI_CL_e_MF_KeyType>; cdecl;//Deprecated
    {class} property KEY_A: JGEDI_CL_e_MF_KeyType read _GetKEY_A;
    {class} property KEY_B: JGEDI_CL_e_MF_KeyType read _GetKEY_B;
  end;

  [JavaSignature('br/com/gertec/gedi/enums/GEDI_CL_e_MF_KeyType')]
  JGEDI_CL_e_MF_KeyType = interface(JEnum)
    ['{0F577495-E578-45DB-80BC-D1A85F884D47}']
  end;
  TJGEDI_CL_e_MF_KeyType = class(TJavaGenericImport<JGEDI_CL_e_MF_KeyTypeClass, JGEDI_CL_e_MF_KeyType>) end;

  JGEDI_CL_e_MF_TypeClass = interface(JEnumClass)
    ['{06EE342F-3070-4B0C-9B19-D0F65162423B}']
    {class} function _GetCLASSIC_1K: JGEDI_CL_e_MF_Type; cdecl;
    {class} function _GetCLASSIC_4K: JGEDI_CL_e_MF_Type; cdecl;
    {class} function _GetDESFIRE: JGEDI_CL_e_MF_Type; cdecl;
    {class} function _GetMINI: JGEDI_CL_e_MF_Type; cdecl;
    {class} function _GetPLUS: JGEDI_CL_e_MF_Type; cdecl;
    {class} function _GetULTRALIGHT: JGEDI_CL_e_MF_Type; cdecl;
    {class} function _GetULTRALIGHT_C: JGEDI_CL_e_MF_Type; cdecl;
    {class} function _GetULTRALIGHT_EV1: JGEDI_CL_e_MF_Type; cdecl;
    {class} function _GetUNKNOWN: JGEDI_CL_e_MF_Type; cdecl;
    {class} function getValue: Integer; cdecl;//Deprecated
    {class} function valueOf(P1: Integer): JGEDI_CL_e_MF_Type; cdecl; overload;//Deprecated
    {class} function valueOf(P1: JString): JGEDI_CL_e_MF_Type; cdecl; overload;//Deprecated
    {class} function values: TJavaObjectArray<JGEDI_CL_e_MF_Type>; cdecl;//Deprecated
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
    ['{0753FBF5-4F78-4912-918B-D4C72D631FA7}']
  end;
  TJGEDI_CL_e_MF_Type = class(TJavaGenericImport<JGEDI_CL_e_MF_TypeClass, JGEDI_CL_e_MF_Type>) end;

  JGEDI_CRYPT_e_OpClass = interface(JEnumClass)
    ['{3C6546A7-4D2C-421A-9B28-8D812B63964C}']
    {class} function _GetDECRYPTION: JGEDI_CRYPT_e_Op; cdecl;
    {class} function _GetENCRYPTION: JGEDI_CRYPT_e_Op; cdecl;
    {class} function getValue: Integer; cdecl;//Deprecated
    {class} function valueOf(P1: Integer): JGEDI_CRYPT_e_Op; cdecl; overload;//Deprecated
    {class} function valueOf(P1: JString): JGEDI_CRYPT_e_Op; cdecl; overload;//Deprecated
    {class} function values: TJavaObjectArray<JGEDI_CRYPT_e_Op>; cdecl;//Deprecated
    {class} property DECRYPTION: JGEDI_CRYPT_e_Op read _GetDECRYPTION;
    {class} property ENCRYPTION: JGEDI_CRYPT_e_Op read _GetENCRYPTION;
  end;

  [JavaSignature('br/com/gertec/gedi/enums/GEDI_CRYPT_e_Op')]
  JGEDI_CRYPT_e_Op = interface(JEnum)
    ['{6985BD7D-2C58-4A43-9663-89C0F241B937}']
  end;
  TJGEDI_CRYPT_e_Op = class(TJavaGenericImport<JGEDI_CRYPT_e_OpClass, JGEDI_CRYPT_e_Op>) end;

  JGEDI_FS_e_StorageClass = interface(JEnumClass)
    ['{E9F4055A-3FD4-4DCB-BD2A-ED1BBEAF749C}']
    {class} function _GetPRIVATE: JGEDI_FS_e_Storage; cdecl;
    {class} function _GetPUBLIC: JGEDI_FS_e_Storage; cdecl;
    {class} function _GetSD_CARD: JGEDI_FS_e_Storage; cdecl;
    {class} function _GetUSB_DISK: JGEDI_FS_e_Storage; cdecl;
    {class} function getValue: Integer; cdecl;//Deprecated
    {class} function valueOf(P1: JString): JGEDI_FS_e_Storage; cdecl;//Deprecated
    {class} function values: TJavaObjectArray<JGEDI_FS_e_Storage>; cdecl;//Deprecated
    {class} property &PRIVATE: JGEDI_FS_e_Storage read _GetPRIVATE;
    {class} property &PUBLIC: JGEDI_FS_e_Storage read _GetPUBLIC;
    {class} property SD_CARD: JGEDI_FS_e_Storage read _GetSD_CARD;
    {class} property USB_DISK: JGEDI_FS_e_Storage read _GetUSB_DISK;
  end;

  [JavaSignature('br/com/gertec/gedi/enums/GEDI_FS_e_Storage')]
  JGEDI_FS_e_Storage = interface(JEnum)
    ['{3D04A075-9830-44A1-BF82-C6C54BA67959}']
  end;
  TJGEDI_FS_e_Storage = class(TJavaGenericImport<JGEDI_FS_e_StorageClass, JGEDI_FS_e_Storage>) end;

  JGEDI_INFO_e_ControlNumberClass = interface(JEnumClass)
    ['{82589EA6-A338-49F8-8823-BCFE64AD686C}']
    {class} function _GetCHASSIS: JGEDI_INFO_e_ControlNumber; cdecl;
    {class} function _GetSEAL: JGEDI_INFO_e_ControlNumber; cdecl;
    {class} function _GetSN: JGEDI_INFO_e_ControlNumber; cdecl;
    {class} function getValue: Integer; cdecl;//Deprecated
    {class} function valueOf(P1: Integer): JGEDI_INFO_e_ControlNumber; cdecl; overload;//Deprecated
    {class} function valueOf(P1: JString): JGEDI_INFO_e_ControlNumber; cdecl; overload;//Deprecated
    {class} function values: TJavaObjectArray<JGEDI_INFO_e_ControlNumber>; cdecl;//Deprecated
    {class} property CHASSIS: JGEDI_INFO_e_ControlNumber read _GetCHASSIS;
    {class} property SEAL: JGEDI_INFO_e_ControlNumber read _GetSEAL;
    {class} property SN: JGEDI_INFO_e_ControlNumber read _GetSN;
  end;

  [JavaSignature('br/com/gertec/gedi/enums/GEDI_INFO_e_ControlNumber')]
  JGEDI_INFO_e_ControlNumber = interface(JEnum)
    ['{75DE13A9-FA85-4875-B827-0CB30337FB11}']
  end;
  TJGEDI_INFO_e_ControlNumber = class(TJavaGenericImport<JGEDI_INFO_e_ControlNumberClass, JGEDI_INFO_e_ControlNumber>) end;

  JGEDI_INFO_e_TestClass = interface(JEnumClass)
    ['{3E30E609-1907-4E7F-ACDF-4A0EAA499DD2}']
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
    {class} function getValue: Integer; cdecl;//Deprecated
    {class} function valueOf(P1: JString): JGEDI_INFO_e_Test; cdecl;//Deprecated
    {class} function values: TJavaObjectArray<JGEDI_INFO_e_Test>; cdecl;//Deprecated
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
    ['{5D80F4A4-4BAA-4F40-B036-FB187D7D471F}']
  end;
  TJGEDI_INFO_e_Test = class(TJavaGenericImport<JGEDI_INFO_e_TestClass, JGEDI_INFO_e_Test>) end;

  JGEDI_KBD_e_KeyClass = interface(JEnumClass)
    ['{E99A79AB-5A6D-4DAF-8DA4-DAE7FD147E70}']
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
    {class} function getValue: Integer; cdecl;//Deprecated
    {class} procedure setValue(P1: Integer); cdecl;//Deprecated
    {class} function valueOf(P1: Integer): JGEDI_KBD_e_Key; cdecl; overload;//Deprecated
    {class} function valueOf(P1: JString): JGEDI_KBD_e_Key; cdecl; overload;//Deprecated
    {class} function values: TJavaObjectArray<JGEDI_KBD_e_Key>; cdecl;//Deprecated
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
    ['{8EBA5EE3-B352-4C38-AA4A-C2F57274EAD5}']
  end;
  TJGEDI_KBD_e_Key = class(TJavaGenericImport<JGEDI_KBD_e_KeyClass, JGEDI_KBD_e_Key>) end;

  JGEDI_KBD_e_PowerKeyModeClass = interface(JEnumClass)
    ['{8D034D0F-D5BB-4849-A531-FF83045C6662}']
    {class} function _GetPOWEROFF: JGEDI_KBD_e_PowerKeyMode; cdecl;
    {class} function _GetRESET: JGEDI_KBD_e_PowerKeyMode; cdecl;
    {class} function getValue: Integer; cdecl;//Deprecated
    {class} function valueOf(P1: JString): JGEDI_KBD_e_PowerKeyMode; cdecl;//Deprecated
    {class} function values: TJavaObjectArray<JGEDI_KBD_e_PowerKeyMode>; cdecl;//Deprecated
    {class} property POWEROFF: JGEDI_KBD_e_PowerKeyMode read _GetPOWEROFF;
    {class} property RESET: JGEDI_KBD_e_PowerKeyMode read _GetRESET;
  end;

  [JavaSignature('br/com/gertec/gedi/enums/GEDI_KBD_e_PowerKeyMode')]
  JGEDI_KBD_e_PowerKeyMode = interface(JEnum)
    ['{ACB6E87E-1B96-4D4F-845F-0A189A7A4971}']
  end;
  TJGEDI_KBD_e_PowerKeyMode = class(TJavaGenericImport<JGEDI_KBD_e_PowerKeyModeClass, JGEDI_KBD_e_PowerKeyMode>) end;

  JGEDI_KMS_e_BLOCKTYPEClass = interface(JEnumClass)
    ['{856EA087-4447-4CAC-980A-8D769933F137}']
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
    {class} function getValue: Integer; cdecl;//Deprecated
    {class} function valueOf(P1: Integer): JGEDI_KMS_e_BLOCKTYPE; cdecl; overload;//Deprecated
    {class} function valueOf(P1: JString): JGEDI_KMS_e_BLOCKTYPE; cdecl; overload;//Deprecated
    {class} function values: TJavaObjectArray<JGEDI_KMS_e_BLOCKTYPE>; cdecl;//Deprecated
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
    ['{FBE9BD85-3964-4178-83A4-1B088A640B24}']
  end;
  TJGEDI_KMS_e_BLOCKTYPE = class(TJavaGenericImport<JGEDI_KMS_e_BLOCKTYPEClass, JGEDI_KMS_e_BLOCKTYPE>) end;

  JGEDI_KMS_e_EncModeClass = interface(JEnumClass)
    ['{A4D0BCB0-9C16-4766-8505-D153DFFACC76}']
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
    {class} function getValue: Integer; cdecl;//Deprecated
    {class} function valueOf(P1: Integer): JGEDI_KMS_e_EncMode; cdecl; overload;//Deprecated
    {class} function valueOf(P1: JString): JGEDI_KMS_e_EncMode; cdecl; overload;//Deprecated
    {class} function values: TJavaObjectArray<JGEDI_KMS_e_EncMode>; cdecl;//Deprecated
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
    ['{5BF6C385-F77A-4420-97CD-87359B7B3C23}']
  end;
  TJGEDI_KMS_e_EncMode = class(TJavaGenericImport<JGEDI_KMS_e_EncModeClass, JGEDI_KMS_e_EncMode>) end;

  JGEDI_KMS_e_KEYPURPOSEClass = interface(JEnumClass)
    ['{0E91869C-7877-4BF5-A373-6BE9F08CCA1B}']
    {class} function _GetAUTH: JGEDI_KMS_e_KEYPURPOSE; cdecl;
    {class} function _GetDATA: JGEDI_KMS_e_KEYPURPOSE; cdecl;
    {class} function _GetKEK: JGEDI_KMS_e_KEYPURPOSE; cdecl;
    {class} function _GetPIN: JGEDI_KMS_e_KEYPURPOSE; cdecl;
    {class} function _GetSRED: JGEDI_KMS_e_KEYPURPOSE; cdecl;
    {class} function getValue: Integer; cdecl;//Deprecated
    {class} function valueOf(P1: Integer): JGEDI_KMS_e_KEYPURPOSE; cdecl; overload;//Deprecated
    {class} function valueOf(P1: JString): JGEDI_KMS_e_KEYPURPOSE; cdecl; overload;//Deprecated
    {class} function values: TJavaObjectArray<JGEDI_KMS_e_KEYPURPOSE>; cdecl;//Deprecated
    {class} property AUTH: JGEDI_KMS_e_KEYPURPOSE read _GetAUTH;
    {class} property DATA: JGEDI_KMS_e_KEYPURPOSE read _GetDATA;
    {class} property KEK: JGEDI_KMS_e_KEYPURPOSE read _GetKEK;
    {class} property PIN: JGEDI_KMS_e_KEYPURPOSE read _GetPIN;
    {class} property SRED: JGEDI_KMS_e_KEYPURPOSE read _GetSRED;
  end;

  [JavaSignature('br/com/gertec/gedi/enums/GEDI_KMS_e_KEYPURPOSE')]
  JGEDI_KMS_e_KEYPURPOSE = interface(JEnum)
    ['{60E409E9-E87F-4685-89EB-02A52B45B985}']
  end;
  TJGEDI_KMS_e_KEYPURPOSE = class(TJavaGenericImport<JGEDI_KMS_e_KEYPURPOSEClass, JGEDI_KMS_e_KEYPURPOSE>) end;

  JGEDI_KMS_e_KEYTYPEClass = interface(JEnumClass)
    ['{7951A0D2-D475-44D2-92DC-5B602DC63A58}']
    {class} function _GetAES: JGEDI_KMS_e_KEYTYPE; cdecl;
    {class} function _GetDES: JGEDI_KMS_e_KEYTYPE; cdecl;
    {class} function _GetDUKPT_DES: JGEDI_KMS_e_KEYTYPE; cdecl;
    {class} function _GetDUKPT_TDES: JGEDI_KMS_e_KEYTYPE; cdecl;
    {class} function _GetRSA: JGEDI_KMS_e_KEYTYPE; cdecl;
    {class} function _GetTDES: JGEDI_KMS_e_KEYTYPE; cdecl;
    {class} function getValue: Integer; cdecl;//Deprecated
    {class} function valueOf(P1: Integer): JGEDI_KMS_e_KEYTYPE; cdecl; overload;//Deprecated
    {class} function valueOf(P1: JString): JGEDI_KMS_e_KEYTYPE; cdecl; overload;//Deprecated
    {class} function values: TJavaObjectArray<JGEDI_KMS_e_KEYTYPE>; cdecl;//Deprecated
    {class} property AES: JGEDI_KMS_e_KEYTYPE read _GetAES;
    {class} property DES: JGEDI_KMS_e_KEYTYPE read _GetDES;
    {class} property DUKPT_DES: JGEDI_KMS_e_KEYTYPE read _GetDUKPT_DES;
    {class} property DUKPT_TDES: JGEDI_KMS_e_KEYTYPE read _GetDUKPT_TDES;
    {class} property RSA: JGEDI_KMS_e_KEYTYPE read _GetRSA;
    {class} property TDES: JGEDI_KMS_e_KEYTYPE read _GetTDES;
  end;

  [JavaSignature('br/com/gertec/gedi/enums/GEDI_KMS_e_KEYTYPE')]
  JGEDI_KMS_e_KEYTYPE = interface(JEnum)
    ['{E5DF3544-1BE2-4B48-B0B3-0B53C2770951}']
  end;
  TJGEDI_KMS_e_KEYTYPE = class(TJavaGenericImport<JGEDI_KMS_e_KEYTYPEClass, JGEDI_KMS_e_KEYTYPE>) end;

  JGEDI_KMS_e_KEYTYPE_LENGTHClass = interface(JEnumClass)
    ['{E496B17A-C81C-4AA3-8BBD-B53F4FBCCF10}']
    {class} function _GetAES16: JGEDI_KMS_e_KEYTYPE_LENGTH; cdecl;
    {class} function _GetAES32: JGEDI_KMS_e_KEYTYPE_LENGTH; cdecl;
    {class} function _GetDES: JGEDI_KMS_e_KEYTYPE_LENGTH; cdecl;
    {class} function _GetDUKPT3DES16: JGEDI_KMS_e_KEYTYPE_LENGTH; cdecl;
    {class} function _GetDUKPT3DES24: JGEDI_KMS_e_KEYTYPE_LENGTH; cdecl;
    {class} function _GetDUKPTDES8: JGEDI_KMS_e_KEYTYPE_LENGTH; cdecl;
    {class} function _GetTRIDES16: JGEDI_KMS_e_KEYTYPE_LENGTH; cdecl;
    {class} function _GetTRIDES24: JGEDI_KMS_e_KEYTYPE_LENGTH; cdecl;
    {class} function getValue: Integer; cdecl;//Deprecated
    {class} function valueOf(P1: JString): JGEDI_KMS_e_KEYTYPE_LENGTH; cdecl;//Deprecated
    {class} function values: TJavaObjectArray<JGEDI_KMS_e_KEYTYPE_LENGTH>; cdecl;//Deprecated
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
    ['{E166E643-B72F-4F27-8333-A2555ED9E5D6}']
  end;
  TJGEDI_KMS_e_KEYTYPE_LENGTH = class(TJavaGenericImport<JGEDI_KMS_e_KEYTYPE_LENGTHClass, JGEDI_KMS_e_KEYTYPE_LENGTH>) end;

  JGEDI_KMS_e_OPClass = interface(JEnumClass)
    ['{2AC7BBB7-FE09-4A18-9633-10CBCD96C52D}']
    {class} function _GetDECRYPT: JGEDI_KMS_e_OP; cdecl;
    {class} function _GetENCRYPT: JGEDI_KMS_e_OP; cdecl;
    {class} function getValue: Integer; cdecl;//Deprecated
    {class} function valueOf(P1: Integer): JGEDI_KMS_e_OP; cdecl; overload;//Deprecated
    {class} function valueOf(P1: JString): JGEDI_KMS_e_OP; cdecl; overload;//Deprecated
    {class} function values: TJavaObjectArray<JGEDI_KMS_e_OP>; cdecl;//Deprecated
    {class} property DECRYPT: JGEDI_KMS_e_OP read _GetDECRYPT;
    {class} property ENCRYPT: JGEDI_KMS_e_OP read _GetENCRYPT;
  end;

  [JavaSignature('br/com/gertec/gedi/enums/GEDI_KMS_e_OP')]
  JGEDI_KMS_e_OP = interface(JEnum)
    ['{9541EC29-E72B-4B93-834C-C3007B5DFFDB}']
  end;
  TJGEDI_KMS_e_OP = class(TJavaGenericImport<JGEDI_KMS_e_OPClass, JGEDI_KMS_e_OP>) end;

  JGEDI_LED_e_IdClass = interface(JEnumClass)
    ['{B16E4FD5-93C9-4574-855F-25F286D2FEF0}']
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
    {class} function _GetGEDI_LED_ID_CONTACTLESS_BLUE: JGEDI_LED_e_Id; cdecl;
    {class} function _GetGEDI_LED_ID_CONTACTLESS_GREEN: JGEDI_LED_e_Id; cdecl;
    {class} function _GetGEDI_LED_ID_CONTACTLESS_ORANGE: JGEDI_LED_e_Id; cdecl;
    {class} function _GetGEDI_LED_ID_CONTACTLESS_RED: JGEDI_LED_e_Id; cdecl;
    {class} function _GetGEDI_LED_ID_GREEN: JGEDI_LED_e_Id; cdecl;
    {class} function _GetGEDI_LED_ID_MAIN: JGEDI_LED_e_Id; cdecl;
    {class} function _GetGEDI_LED_ID_ORANGE: JGEDI_LED_e_Id; cdecl;
    {class} function _GetGEDI_LED_ID_RED: JGEDI_LED_e_Id; cdecl;
    {class} function getValue: Integer; cdecl;//Deprecated
    {class} function valueOf(P1: Integer): JGEDI_LED_e_Id; cdecl; overload;//Deprecated
    {class} function valueOf(P1: JString): JGEDI_LED_e_Id; cdecl; overload;//Deprecated
    {class} function values: TJavaObjectArray<JGEDI_LED_e_Id>; cdecl;//Deprecated
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
    ['{AB4C7A51-E2BB-483B-8ECC-5D301A3A8046}']
  end;
  TJGEDI_LED_e_Id = class(TJavaGenericImport<JGEDI_LED_e_IdClass, JGEDI_LED_e_Id>) end;

  JGEDI_MSR_e_StatusClass = interface(JEnumClass)
    ['{3C84661F-DCA8-4B7E-A452-309FE9CB3AFD}']
    {class} function _GetABSENT_TRACK: JGEDI_MSR_e_Status; cdecl;
    {class} function _GetBUF_OVERFLOW: JGEDI_MSR_e_Status; cdecl;
    {class} function _GetETX_NOT_FOUND: JGEDI_MSR_e_Status; cdecl;
    {class} function _GetLRC_ERR: JGEDI_MSR_e_Status; cdecl;
    {class} function _GetLRC_NOT_FOUND: JGEDI_MSR_e_Status; cdecl;
    {class} function _GetNO_DATA: JGEDI_MSR_e_Status; cdecl;
    {class} function _GetSTX_NOT_FOUND: JGEDI_MSR_e_Status; cdecl;
    {class} function _GetSUCCESS: JGEDI_MSR_e_Status; cdecl;
    {class} function _GetUNKNOWN_CHAR: JGEDI_MSR_e_Status; cdecl;
    {class} function getValue: Integer; cdecl;//Deprecated
    {class} function valueOf(P1: Integer): JGEDI_MSR_e_Status; cdecl; overload;//Deprecated
    {class} function valueOf(P1: JString): JGEDI_MSR_e_Status; cdecl; overload;//Deprecated
    {class} function values: TJavaObjectArray<JGEDI_MSR_e_Status>; cdecl;//Deprecated
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
    ['{F9E951E9-6A7F-4E47-AAC9-018788E482CA}']
  end;
  TJGEDI_MSR_e_Status = class(TJavaGenericImport<JGEDI_MSR_e_StatusClass, JGEDI_MSR_e_Status>) end;

  JGEDI_PRNTR_e_AlignmentClass = interface(JEnumClass)
    ['{65346B95-BF66-4602-8E3D-E31001A7EC13}']
    {class} function _GetCENTER: JGEDI_PRNTR_e_Alignment; cdecl;
    {class} function _GetLEFT: JGEDI_PRNTR_e_Alignment; cdecl;
    {class} function _GetRIGHT: JGEDI_PRNTR_e_Alignment; cdecl;
    {class} function getValue: Integer; cdecl;//Deprecated
    {class} function valueOf(P1: Integer): JGEDI_PRNTR_e_Alignment; cdecl; overload;//Deprecated
    {class} function valueOf(P1: JString): JGEDI_PRNTR_e_Alignment; cdecl; overload;//Deprecated
    {class} function values: TJavaObjectArray<JGEDI_PRNTR_e_Alignment>; cdecl;//Deprecated
    {class} property CENTER: JGEDI_PRNTR_e_Alignment read _GetCENTER;
    {class} property LEFT: JGEDI_PRNTR_e_Alignment read _GetLEFT;
    {class} property RIGHT: JGEDI_PRNTR_e_Alignment read _GetRIGHT;
  end;

  [JavaSignature('br/com/gertec/gedi/enums/GEDI_PRNTR_e_Alignment')]
  JGEDI_PRNTR_e_Alignment = interface(JEnum)
    ['{F22AF4D9-1A20-46C2-96F4-C35D3A624D33}']
  end;
  TJGEDI_PRNTR_e_Alignment = class(TJavaGenericImport<JGEDI_PRNTR_e_AlignmentClass, JGEDI_PRNTR_e_Alignment>) end;

  JGEDI_PRNTR_e_BarCodeTypeClass = interface(JEnumClass)
    ['{61AD7C1C-21A5-48A4-A5C0-639B9B880033}']
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
    {class} function getValue: Integer; cdecl;//Deprecated
    {class} function valueOf(P1: Integer): JGEDI_PRNTR_e_BarCodeType; cdecl; overload;//Deprecated
    {class} function valueOf(P1: JString): JGEDI_PRNTR_e_BarCodeType; cdecl; overload;//Deprecated
    {class} function values: TJavaObjectArray<JGEDI_PRNTR_e_BarCodeType>; cdecl;//Deprecated
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
    ['{BF576B4F-096E-4207-9972-89F5ACD8EF87}']
  end;
  TJGEDI_PRNTR_e_BarCodeType = class(TJavaGenericImport<JGEDI_PRNTR_e_BarCodeTypeClass, JGEDI_PRNTR_e_BarCodeType>) end;

  JGEDI_PRNTR_e_StatusClass = interface(JEnumClass)
    ['{928C8E70-4DB4-4F9E-8ECA-DDBAE15B4533}']
    {class} function _GetOK: JGEDI_PRNTR_e_Status; cdecl;
    {class} function _GetOUT_OF_PAPER: JGEDI_PRNTR_e_Status; cdecl;
    {class} function _GetOVERHEAT: JGEDI_PRNTR_e_Status; cdecl;
    {class} function _GetUNKNOWN_ERROR: JGEDI_PRNTR_e_Status; cdecl;
    {class} function getValue: Integer; cdecl;//Deprecated
    {class} function valueOf(P1: Integer): JGEDI_PRNTR_e_Status; cdecl; overload;//Deprecated
    {class} function valueOf(P1: JString): JGEDI_PRNTR_e_Status; cdecl; overload;//Deprecated
    {class} function values: TJavaObjectArray<JGEDI_PRNTR_e_Status>; cdecl;//Deprecated
    {class} property OK: JGEDI_PRNTR_e_Status read _GetOK;
    {class} property OUT_OF_PAPER: JGEDI_PRNTR_e_Status read _GetOUT_OF_PAPER;
    {class} property OVERHEAT: JGEDI_PRNTR_e_Status read _GetOVERHEAT;
    {class} property UNKNOWN_ERROR: JGEDI_PRNTR_e_Status read _GetUNKNOWN_ERROR;
  end;

  [JavaSignature('br/com/gertec/gedi/enums/GEDI_PRNTR_e_Status')]
  JGEDI_PRNTR_e_Status = interface(JEnum)
    ['{8D69A6D0-CB1B-47CE-B3A9-97D9F8B72234}']
  end;
  TJGEDI_PRNTR_e_Status = class(TJavaGenericImport<JGEDI_PRNTR_e_StatusClass, JGEDI_PRNTR_e_Status>) end;

  JGEDI_SMART_e_MemoryCardTypeClass = interface(JEnumClass)
    ['{377247B0-7A6A-4FCB-8367-07EC482182DA}']
    {class} function _GetTYPE_44x2: JGEDI_SMART_e_MemoryCardType; cdecl;
    {class} function _GetTYPE_44x6: JGEDI_SMART_e_MemoryCardType; cdecl;
    {class} function _GetTYPE_44x8: JGEDI_SMART_e_MemoryCardType; cdecl;
    {class} function getValue: Integer; cdecl;//Deprecated
    {class} function valueOf(P1: JString): JGEDI_SMART_e_MemoryCardType; cdecl;//Deprecated
    {class} function values: TJavaObjectArray<JGEDI_SMART_e_MemoryCardType>; cdecl;//Deprecated
    {class} property TYPE_44x2: JGEDI_SMART_e_MemoryCardType read _GetTYPE_44x2;
    {class} property TYPE_44x6: JGEDI_SMART_e_MemoryCardType read _GetTYPE_44x6;
    {class} property TYPE_44x8: JGEDI_SMART_e_MemoryCardType read _GetTYPE_44x8;
  end;

  [JavaSignature('br/com/gertec/gedi/enums/GEDI_SMART_e_MemoryCardType')]
  JGEDI_SMART_e_MemoryCardType = interface(JEnum)
    ['{015C7870-4EFF-4087-AA56-353E51A85A45}']
  end;
  TJGEDI_SMART_e_MemoryCardType = class(TJavaGenericImport<JGEDI_SMART_e_MemoryCardTypeClass, JGEDI_SMART_e_MemoryCardType>) end;

  JGEDI_SMART_e_SlotClass = interface(JEnumClass)
    ['{2A8C2BC5-74E8-45F7-B202-1A4C3B8E7426}']
    {class} function _GetSAM_1: JGEDI_SMART_e_Slot; cdecl;
    {class} function _GetSAM_2: JGEDI_SMART_e_Slot; cdecl;
    {class} function _GetSAM_3: JGEDI_SMART_e_Slot; cdecl;
    {class} function _GetSAM_4: JGEDI_SMART_e_Slot; cdecl;
    {class} function _GetUSER: JGEDI_SMART_e_Slot; cdecl;
    {class} function getValue: Integer; cdecl;//Deprecated
    {class} function valueOf(P1: Integer): JGEDI_SMART_e_Slot; cdecl; overload;//Deprecated
    {class} function valueOf(P1: JString): JGEDI_SMART_e_Slot; cdecl; overload;//Deprecated
    {class} function values: TJavaObjectArray<JGEDI_SMART_e_Slot>; cdecl;//Deprecated
    {class} property SAM_1: JGEDI_SMART_e_Slot read _GetSAM_1;
    {class} property SAM_2: JGEDI_SMART_e_Slot read _GetSAM_2;
    {class} property SAM_3: JGEDI_SMART_e_Slot read _GetSAM_3;
    {class} property SAM_4: JGEDI_SMART_e_Slot read _GetSAM_4;
    {class} property USER: JGEDI_SMART_e_Slot read _GetUSER;
  end;

  [JavaSignature('br/com/gertec/gedi/enums/GEDI_SMART_e_Slot')]
  JGEDI_SMART_e_Slot = interface(JEnum)
    ['{145BD601-3AFC-4404-9FC3-3CA318B60865}']
  end;
  TJGEDI_SMART_e_Slot = class(TJavaGenericImport<JGEDI_SMART_e_SlotClass, JGEDI_SMART_e_Slot>) end;

  JGEDI_SMART_e_StatusClass = interface(JEnumClass)
    ['{2C9599B0-CFF4-47AF-9154-1881E22F008C}']
    {class} function _GetABSENT: JGEDI_SMART_e_Status; cdecl;
    {class} function _GetACTIVE: JGEDI_SMART_e_Status; cdecl;
    {class} function _GetPRESENT: JGEDI_SMART_e_Status; cdecl;
    {class} function getValue: Integer; cdecl;//Deprecated
    {class} function valueOf(P1: Integer): JGEDI_SMART_e_Status; cdecl; overload;//Deprecated
    {class} function valueOf(P1: JString): JGEDI_SMART_e_Status; cdecl; overload;//Deprecated
    {class} function values: TJavaObjectArray<JGEDI_SMART_e_Status>; cdecl;//Deprecated
    {class} property ABSENT: JGEDI_SMART_e_Status read _GetABSENT;
    {class} property ACTIVE: JGEDI_SMART_e_Status read _GetACTIVE;
    {class} property PRESENT: JGEDI_SMART_e_Status read _GetPRESENT;
  end;

  [JavaSignature('br/com/gertec/gedi/enums/GEDI_SMART_e_Status')]
  JGEDI_SMART_e_Status = interface(JEnum)
    ['{2B008327-D012-4D9E-8386-BE9A30C113FB}']
  end;
  TJGEDI_SMART_e_Status = class(TJavaGenericImport<JGEDI_SMART_e_StatusClass, JGEDI_SMART_e_Status>) end;

  JGEDI_SMART_e_TypeClass = interface(JEnumClass)
    ['{39F8571F-C74E-4B04-9BD3-3FF4C9D25700}']
    {class} function _GetI2C: JGEDI_SMART_e_Type; cdecl;
    {class} function _GetMEM: JGEDI_SMART_e_Type; cdecl;
    {class} function _GetT0: JGEDI_SMART_e_Type; cdecl;
    {class} function _GetT1: JGEDI_SMART_e_Type; cdecl;
    {class} function getValue: Integer; cdecl;//Deprecated
    {class} function valueOf(P1: Integer): JGEDI_SMART_e_Type; cdecl; overload;//Deprecated
    {class} function valueOf(P1: JString): JGEDI_SMART_e_Type; cdecl; overload;//Deprecated
    {class} function values: TJavaObjectArray<JGEDI_SMART_e_Type>; cdecl;//Deprecated
    {class} property I2C: JGEDI_SMART_e_Type read _GetI2C;
    {class} property MEM: JGEDI_SMART_e_Type read _GetMEM;
    {class} property T0: JGEDI_SMART_e_Type read _GetT0;
    {class} property T1: JGEDI_SMART_e_Type read _GetT1;
  end;

  [JavaSignature('br/com/gertec/gedi/enums/GEDI_SMART_e_Type')]
  JGEDI_SMART_e_Type = interface(JEnum)
    ['{5469AFD0-E845-42A7-BDE2-E0000324DFC6}']
  end;
  TJGEDI_SMART_e_Type = class(TJavaGenericImport<JGEDI_SMART_e_TypeClass, JGEDI_SMART_e_Type>) end;

  JGEDI_SMART_e_VoltageClass = interface(JEnumClass)
    ['{739988B7-6229-4410-884A-E6620E9FAF90}']
    {class} function _GetVOLTAGE_1_8V: JGEDI_SMART_e_Voltage; cdecl;
    {class} function _GetVOLTAGE_3V: JGEDI_SMART_e_Voltage; cdecl;
    {class} function _GetVOLTAGE_5V: JGEDI_SMART_e_Voltage; cdecl;
    {class} function getValue: Integer; cdecl;//Deprecated
    {class} function valueOf(P1: Integer): JGEDI_SMART_e_Voltage; cdecl; overload;//Deprecated
    {class} function valueOf(P1: JString): JGEDI_SMART_e_Voltage; cdecl; overload;//Deprecated
    {class} function values: TJavaObjectArray<JGEDI_SMART_e_Voltage>; cdecl;//Deprecated
    {class} property VOLTAGE_1_8V: JGEDI_SMART_e_Voltage read _GetVOLTAGE_1_8V;
    {class} property VOLTAGE_3V: JGEDI_SMART_e_Voltage read _GetVOLTAGE_3V;
    {class} property VOLTAGE_5V: JGEDI_SMART_e_Voltage read _GetVOLTAGE_5V;
  end;

  [JavaSignature('br/com/gertec/gedi/enums/GEDI_SMART_e_Voltage')]
  JGEDI_SMART_e_Voltage = interface(JEnum)
    ['{5894601C-39C0-42D6-9BB3-1A2CE101CF3B}']
  end;
  TJGEDI_SMART_e_Voltage = class(TJavaGenericImport<JGEDI_SMART_e_VoltageClass, JGEDI_SMART_e_Voltage>) end;

  JGEDI_SYS_e_SecuritySetupClass = interface(JEnumClass)
    ['{21750C84-86E4-4B23-9BDB-F23AC3D040BA}']
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
    {class} function getValue: Integer; cdecl;//Deprecated
    {class} function valueOf(P1: JString): JGEDI_SYS_e_SecuritySetup; cdecl;//Deprecated
    {class} function values: TJavaObjectArray<JGEDI_SYS_e_SecuritySetup>; cdecl;//Deprecated
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
    ['{EE2FEC3A-0CE3-4D2E-9B62-A2C6CED3F471}']
  end;
  TJGEDI_SYS_e_SecuritySetup = class(TJavaGenericImport<JGEDI_SYS_e_SecuritySetupClass, JGEDI_SYS_e_SecuritySetup>) end;

  JGEDI_e_RetClass = interface(JEnumClass)
    ['{94C2F3D2-C0F6-45D4-BB11-7781BE138F6C}']
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
    {class} function _GetPM_ERROR: JGEDI_e_Ret; cdecl;
    {class} function _GetPM_FULL: JGEDI_e_Ret; cdecl;
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
    {class} function getValue: Integer; cdecl;//Deprecated
    {class} function valueOf(P1: Integer): JGEDI_e_Ret; cdecl; overload;//Deprecated
    {class} function valueOf(P1: JString): JGEDI_e_Ret; cdecl; overload;//Deprecated
    {class} function values: TJavaObjectArray<JGEDI_e_Ret>; cdecl;//Deprecated
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
    {class} property PM_ERROR: JGEDI_e_Ret read _GetPM_ERROR;
    {class} property PM_FULL: JGEDI_e_Ret read _GetPM_FULL;
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
    ['{9B6979E1-83FD-443E-ABF5-4C0515671809}']
  end;
  TJGEDI_e_Ret = class(TJavaGenericImport<JGEDI_e_RetClass, JGEDI_e_Ret>) end;

  JGediExceptionClass = interface(JExceptionClass)
    ['{139400A1-E119-4B98-8B22-220522FE9CA0}']
    {class} function getErrorCode: JGEDI_e_Ret; cdecl;//Deprecated
    {class} function init(P1: JGEDI_e_Ret): JGediException; cdecl; overload;//Deprecated
    {class} function init(P1: Integer): JGediException; cdecl; overload;//Deprecated
    {class} function init(P1: JGEDI_e_Ret; P2: JString): JGediException; cdecl; overload;//Deprecated
    {class} function init(P1: JGEDI_e_Ret; P2: JThrowable): JGediException; cdecl; overload;//Deprecated
    {class} function init(P1: Integer; P2: JString): JGediException; cdecl; overload;//Deprecated
    {class} function init(P1: Integer; P2: JThrowable): JGediException; cdecl; overload;//Deprecated
    {class} function init(P1: JGEDI_e_Ret; P2: JString; P3: JThrowable): JGediException; cdecl; overload;//Deprecated
    {class} function init(P1: Integer; P2: JString; P3: JThrowable): JGediException; cdecl; overload;//Deprecated
    {class} function toString: JString; cdecl;//Deprecated
  end;

  [JavaSignature('br/com/gertec/gedi/exceptions/GediException')]
  JGediException = interface(JException)
    ['{DA05CAD3-725A-4CC5-BF9F-2B36C3C9C11E}']
  end;
  TJGediException = class(TJavaGenericImport<JGediExceptionClass, JGediException>) end;

  Jimpl_ClClass = interface(JCLClass)
    ['{55F51DE7-E214-438F-8103-0DD2DCFC619D}']
    {class} function init: Jimpl_Cl; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/impl/Cl')]
  Jimpl_Cl = interface(JCL)
    ['{EF725E9D-0D6A-442D-A353-F27C15CD69BE}']
    procedure MF_Authentication(P1: Integer; P2: JGEDI_CL_st_MF_Key; P3: TJavaArray<Byte>); cdecl;
    function MF_BlockRead(P1: Integer): TJavaArray<Byte>; cdecl;
    procedure MF_BlockWrite(P1: Integer; P2: TJavaArray<Byte>); cdecl;
    function MF_SignatureGet(P1: Integer): TJavaArray<Byte>; cdecl;
    function ResetEMV: JGEDI_CL_st_ResetEMVInfo; cdecl;
    function SendAPDU(P1: TJavaArray<Byte>): TJavaArray<Byte>; cdecl;
  end;
  TJimpl_Cl = class(TJavaGenericImport<Jimpl_ClClass, Jimpl_Cl>) end;

  Jimpl_ClockClass = interface(Jgedi_CLOCKClass)
    ['{ADDF0AD3-BC6C-4183-A347-01C33329DE17}']
    {class} function init: Jimpl_Clock; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/impl/Clock')]
  Jimpl_Clock = interface(Jgedi_CLOCK)
    ['{7DE12561-30DE-49E7-96BD-3474C4BFD9C0}']
  end;
  TJimpl_Clock = class(TJavaGenericImport<Jimpl_ClockClass, Jimpl_Clock>) end;

  Jimpl_GediClass = interface(JGEDIClass)
    ['{8CDDF5E4-B177-4A0B-AE6E-4ECE9D991E5F}']
    {class} procedure EnterEng(P1: JString); cdecl;//Deprecated
    {class} function VersionGet: JString; cdecl;//Deprecated
    {class} function getAUDIO: JIAUDIO; cdecl;//Deprecated
    {class} function getCL: JCL; cdecl;//Deprecated
    {class} function getCLOCK: Jgedi_CLOCK; cdecl;//Deprecated
    {class} function getCRYPT: JCRYPT; cdecl;//Deprecated
    {class} function getINFO: JINFO; cdecl;//Deprecated
    {class} function getKBD: JKBD; cdecl;//Deprecated
    {class} function getKMS: JKMS; cdecl;//Deprecated
    {class} function getLED: JLED; cdecl;//Deprecated
    {class} function getMSR: JMSR; cdecl;//Deprecated
    {class} function getPM: JPM; cdecl;//Deprecated
    {class} function getPRNTR: JPRNTR; cdecl;//Deprecated
    {class} function getSMART: JSMART; cdecl;//Deprecated
    {class} function getSYS: JSYS; cdecl;//Deprecated
    {class} function init(P1: JActivity): Jimpl_Gedi; cdecl;//Deprecated
    {class} procedure onServiceConnected; cdecl;//Deprecated
  end;

  [JavaSignature('br/com/gertec/gedi/impl/Gedi')]
  Jimpl_Gedi = interface(JGEDI)
    ['{8F585655-D820-4624-8870-84CFE9C0163B}']
  end;
  TJimpl_Gedi = class(TJavaGenericImport<Jimpl_GediClass, Jimpl_Gedi>) end;

  Jimpl_InfoClass = interface(JINFOClass)
    ['{DE13FA10-140D-4A3F-A632-B3AC1A3A4E21}']
    {class} function init: Jimpl_Info; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/impl/Info')]
  Jimpl_Info = interface(JINFO)
    ['{4C007383-3497-4817-966E-B6F16BC8BDD7}']
  end;
  TJimpl_Info = class(TJavaGenericImport<Jimpl_InfoClass, Jimpl_Info>) end;

  Jimpl_KbdClass = interface(JKBDClass)
    ['{856242EF-6E09-43FC-8881-062A955956BD}']
    {class} function init: Jimpl_Kbd; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/impl/Kbd')]
  Jimpl_Kbd = interface(JKBD)
    ['{72EABE72-FBD1-4156-9B40-4A44E8512260}']
    procedure &Set(P1: JGEDI_KBD_st_Info); cdecl;
  end;
  TJimpl_Kbd = class(TJavaGenericImport<Jimpl_KbdClass, Jimpl_Kbd>) end;

  Jimpl_LedClass = interface(JLEDClass)
    ['{A3CC8C09-BD62-4BB0-BCF2-6084BCB7FAC1}']
    {class} function init: Jimpl_Led; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/impl/Led')]
  Jimpl_Led = interface(JLED)
    ['{D559CAB9-5047-46D3-80A4-2EC21A33CB6E}']
    procedure &Set(P1: JGEDI_LED_e_Id; P2: Boolean); cdecl;
  end;
  TJimpl_Led = class(TJavaGenericImport<Jimpl_LedClass, Jimpl_Led>) end;

  JLed_1Class = interface(JObjectClass)
    ['{20A8DF9E-E1F8-484C-A963-05FB00580064}']
  end;

  [JavaSignature('br/com/gertec/gedi/impl/Led$1')]
  JLed_1 = interface(JObject)
    ['{500916EB-25BC-4292-8825-C4FF7AC37800}']
  end;
  TJLed_1 = class(TJavaGenericImport<JLed_1Class, JLed_1>) end;

  Jimpl_PmClass = interface(JPMClass)
    ['{4BC020E0-6DD2-4EFE-8542-4C12818FA493}']
    {class} function init: Jimpl_Pm; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/impl/Pm')]
  Jimpl_Pm = interface(JPM)
    ['{B634FA01-D933-4942-9564-2A19BAB5F783}']
  end;
  TJimpl_Pm = class(TJavaGenericImport<Jimpl_PmClass, Jimpl_Pm>) end;

  JPrinterListClass = interface(JObjectClass)
    ['{983243D7-9D9D-425B-98C9-1278A00AAF43}']
    {class} function _Getalign: Integer; cdecl;
    {class} procedure _Setalign(Value: Integer); cdecl;
    {class} function _Getbitmap: JBitmap; cdecl;
    {class} procedure _Setbitmap(Value: JBitmap); cdecl;
    {class} function _Getdata: JString; cdecl;
    {class} procedure _Setdata(Value: JString); cdecl;
    {class} function _Getheight: Integer; cdecl;
    {class} procedure _Setheight(Value: Integer); cdecl;
    {class} function _Getmap: JMap; cdecl;
    {class} procedure _Setmap(Value: JMap); cdecl;
    {class} function getAlign: Integer; cdecl;//Deprecated
    {class} function getBitmap: JBitmap; cdecl;//Deprecated
    {class} function getCommand: JString; cdecl;//Deprecated
    {class} function getData: JString; cdecl;//Deprecated
    {class} function getHeight: Integer; cdecl;//Deprecated
    {class} function getMap: JMap; cdecl;//Deprecated
    {class} function init(P1: JString): JPrinterList; cdecl; overload;//Deprecated
    {class} function init(P1: JString; P2: JString): JPrinterList; cdecl; overload;//Deprecated
    {class} function init(P1: JString; P2: JBitmap; P3: JMap): JPrinterList; cdecl; overload;//Deprecated
    {class} function init(P1: JString; P2: JString; P3: Integer; P4: Integer): JPrinterList; cdecl; overload;//Deprecated
    {class} property align: Integer read _Getalign write _Setalign;
    {class} property bitmap: JBitmap read _Getbitmap write _Setbitmap;
    {class} property data: JString read _Getdata write _Setdata;
    {class} property height: Integer read _Getheight write _Setheight;
    {class} property map: JMap read _Getmap write _Setmap;
  end;

  [JavaSignature('br/com/gertec/gedi/impl/PrinterList')]
  JPrinterList = interface(JObject)
    ['{4309E7ED-EE3C-44E5-9064-06F8B9E3CD72}']
  end;
  TJPrinterList = class(TJavaGenericImport<JPrinterListClass, JPrinterList>) end;

  JPrinterManagerClass = interface(JObjectClass)
    ['{0C3AEACD-CCBB-48B4-AA17-21927F371AD4}']
    {class} function _GetALIGN_CENTER: Integer; cdecl;
    {class} function _GetALIGN_LEFT: Integer; cdecl;
    {class} function _GetALIGN_RIGHT: Integer; cdecl;
    {class} function _GetARIAL: Integer; cdecl;
    {class} function _GetBOLD: Integer; cdecl;
    {class} function _GetKEY_ALIGN: JString; cdecl;
    {class} function _GetKEY_LINESPACE: JString; cdecl;
    {class} function _GetKEY_MARGINBOTTOM: JString; cdecl;
    {class} function _GetKEY_MARGINLEFT: JString; cdecl;
    {class} function _GetKEY_MARGINRIGHT: JString; cdecl;
    {class} function _GetKEY_MARGINTOP: JString; cdecl;
    {class} function _GetKEY_TEXTSIZE: JString; cdecl;
    {class} function _GetKEY_TYPEFACE: JString; cdecl;
    {class} function _GetKEY_WEIGHT: JString; cdecl;
    {class} function _GetNORMAL: Integer; cdecl;
    {class} function _GetSERIF: Integer; cdecl;
    {class} function _GetsCurrentLength: Int64; cdecl;
    {class} procedure _SetsCurrentLength(Value: Int64); cdecl;
    {class} function _GetsTotalLength: Int64; cdecl;
    {class} procedure _SetsTotalLength(Value: Int64); cdecl;
    {class} function getBootloaderVersion: JString; cdecl;//Deprecated
    {class} function getFirmwareVersion: JString; cdecl;//Deprecated
    {class} function hasXChengPrinter(P1: JContext): Boolean; cdecl;//Deprecated
    {class} function init(P1: JActivity; P2: JPrinterManager_PrinterManagerListener): JPrinterManager; cdecl;//Deprecated
    {class} procedure onPrinterStart; cdecl;//Deprecated
    {class} procedure onPrinterStop; cdecl;//Deprecated
    {class} procedure printBarCode(P1: JString; P2: Integer; P3: Integer; P4: Integer; P5: Boolean); cdecl;//Deprecated
    {class} procedure printBitmap(P1: JBitmap); cdecl; overload;//Deprecated
    {class} procedure printBitmap(P1: JBitmap; P2: JMap); cdecl; overload;//Deprecated
    {class} procedure printColumnsTextWithAttributes(P1: TJavaObjectArray<JString>; P2: JList); cdecl;//Deprecated
    {class} procedure printQRCode(P1: JString; P2: Integer; P3: Integer); cdecl;//Deprecated
    {class} procedure printText(P1: JString); cdecl;//Deprecated
    {class} procedure printTextWithAttributes(P1: JString; P2: JMap); cdecl;//Deprecated
    {class} procedure printWrapPaper(P1: Integer); cdecl;//Deprecated
    {class} procedure printerInit; cdecl;//Deprecated
    {class} function printerPaper: Boolean; cdecl;//Deprecated
    {class} procedure printerReset; cdecl;//Deprecated
    {class} function printerTemperature(P1: JActivity): Integer; cdecl;//Deprecated
    {class} procedure sendRAWData(P1: TJavaArray<Byte>); cdecl;//Deprecated
    {class} procedure setPrinterSpeed(P1: Integer); cdecl;//Deprecated
    {class} procedure upgradePrinter; cdecl;//Deprecated
    {class} property ALIGN_CENTER: Integer read _GetALIGN_CENTER;
    {class} property ALIGN_LEFT: Integer read _GetALIGN_LEFT;
    {class} property ALIGN_RIGHT: Integer read _GetALIGN_RIGHT;
    {class} property ARIAL: Integer read _GetARIAL;
    {class} property BOLD: Integer read _GetBOLD;
    {class} property KEY_ALIGN: JString read _GetKEY_ALIGN;
    {class} property KEY_LINESPACE: JString read _GetKEY_LINESPACE;
    {class} property KEY_MARGINBOTTOM: JString read _GetKEY_MARGINBOTTOM;
    {class} property KEY_MARGINLEFT: JString read _GetKEY_MARGINLEFT;
    {class} property KEY_MARGINRIGHT: JString read _GetKEY_MARGINRIGHT;
    {class} property KEY_MARGINTOP: JString read _GetKEY_MARGINTOP;
    {class} property KEY_TEXTSIZE: JString read _GetKEY_TEXTSIZE;
    {class} property KEY_TYPEFACE: JString read _GetKEY_TYPEFACE;
    {class} property KEY_WEIGHT: JString read _GetKEY_WEIGHT;
    {class} property NORMAL: Integer read _GetNORMAL;
    {class} property SERIF: Integer read _GetSERIF;
    {class} property sCurrentLength: Int64 read _GetsCurrentLength write _SetsCurrentLength;
    {class} property sTotalLength: Int64 read _GetsTotalLength write _SetsTotalLength;
  end;

  [JavaSignature('br/com/gertec/gedi/impl/PrinterManager')]
  JPrinterManager = interface(JObject)
    ['{76C3A6AB-D526-4852-AA58-A6C590032EF7}']
  end;
  TJPrinterManager = class(TJavaGenericImport<JPrinterManagerClass, JPrinterManager>) end;

  JPrinterManager_1Class = interface(JServiceConnectionClass)
    ['{FEE620F3-13B5-4CCF-9EC2-990AF6967629}']
    {class} function init(P1: JPrinterManager): JPrinterManager_1; cdecl;//Deprecated
    {class} procedure onServiceConnected(P1: JComponentName; P2: JIBinder); cdecl;//Deprecated
    {class} procedure onServiceDisconnected(P1: JComponentName); cdecl;//Deprecated
  end;

  [JavaSignature('br/com/gertec/gedi/impl/PrinterManager$1')]
  JPrinterManager_1 = interface(JServiceConnection)
    ['{24564952-71FD-4801-916B-FE042361575A}']
  end;
  TJPrinterManager_1 = class(TJavaGenericImport<JPrinterManager_1Class, JPrinterManager_1>) end;

  JIPrinterCallback_StubClass = interface(JBinderClass)
    ['{0022A864-E6D0-4686-BA11-9A455221459D}']
    {class} function _GetTRANSACTION_onComplete: Integer; cdecl;
    {class} function _GetTRANSACTION_onLength: Integer; cdecl;
    {class} function asBinder: JIBinder; cdecl;//Deprecated
    {class} function asInterface(P1: JIBinder): JIPrinterCallback; cdecl;//Deprecated
    {class} function init: JIPrinterCallback_Stub; cdecl;//Deprecated
    {class} function onTransact(P1: Integer; P2: JParcel; P3: JParcel; P4: Integer): Boolean; cdecl;//Deprecated
    {class} property TRANSACTION_onComplete: Integer read _GetTRANSACTION_onComplete;
    {class} property TRANSACTION_onLength: Integer read _GetTRANSACTION_onLength;
  end;

  [JavaSignature('com/xcheng/printerservice/IPrinterCallback$Stub')]
  JIPrinterCallback_Stub = interface(JBinder)
    ['{62C9F5A4-68D4-42DC-88AA-2BA8AD1E9E99}']
  end;
  TJIPrinterCallback_Stub = class(TJavaGenericImport<JIPrinterCallback_StubClass, JIPrinterCallback_Stub>) end;

  JPrinterManager_2Class = interface(JIPrinterCallback_StubClass)
    ['{698A1E97-7C45-47D6-8A67-93537F2640B0}']
    {class} function init(P1: JPrinterManager): JPrinterManager_2; cdecl;//Deprecated
    {class} procedure onComplete; cdecl;//Deprecated
    {class} procedure onException(P1: Integer; P2: JString); cdecl;//Deprecated
    {class} procedure onLength(P1: Int64; P2: Int64); cdecl;//Deprecated
  end;

  [JavaSignature('br/com/gertec/gedi/impl/PrinterManager$2')]
  JPrinterManager_2 = interface(JIPrinterCallback_Stub)
    ['{AE774384-65EF-427A-9699-FB34A01B0629}']
  end;
  TJPrinterManager_2 = class(TJavaGenericImport<JPrinterManager_2Class, JPrinterManager_2>) end;

  JPrinterManager_PrinterManagerListenerClass = interface(IJavaClass)
    ['{75956D85-FABA-4E28-9571-8B97C9178F37}']
  end;

  [JavaSignature('br/com/gertec/gedi/impl/PrinterManager$PrinterManagerListener')]
  JPrinterManager_PrinterManagerListener = interface(IJavaInstance)
    ['{1EA02453-C399-4CEE-98D9-D7C95E407ABD}']
    procedure onServiceConnected; cdecl;
  end;
  TJPrinterManager_PrinterManagerListener = class(TJavaGenericImport<JPrinterManager_PrinterManagerListenerClass, JPrinterManager_PrinterManagerListener>) end;

  Jimpl_PrntrClass = interface(JPRNTRClass)
    ['{C7F7979C-A283-41A7-9C59-3928011B188D}']
  end;

  [JavaSignature('br/com/gertec/gedi/impl/Prntr')]
  Jimpl_Prntr = interface(JPRNTR)
    ['{B536F675-AAED-4093-8831-582DA0580FA1}']
    procedure DrawBarCode(P1: JGEDI_PRNTR_st_BarCodeConfig; P2: JString); cdecl;
    procedure DrawBlankLine(P1: Integer); cdecl;
    procedure DrawPictureExt(P1: JGEDI_PRNTR_st_PictureConfig; P2: JBitmap); cdecl;
    procedure DrawStringExt(P1: JGEDI_PRNTR_st_StringConfig; P2: JString); cdecl;
    procedure Init; cdecl; overload;
    procedure Output; cdecl;
    function Status: JGEDI_PRNTR_e_Status; cdecl;
  end;
  TJimpl_Prntr = class(TJavaGenericImport<Jimpl_PrntrClass, Jimpl_Prntr>) end;

  JPrntr_1Class = interface(JRunnableClass)
    ['{9CC84E45-D8D6-458A-B048-63C4D532F5CC}']
    //{class} function _Getthis$0: Jimpl_Prntr; cdecl;
    {class} function init(P1: Jimpl_Prntr; P2: TJavaArray<Integer>): JPrntr_1; cdecl;//Deprecated
    {class} procedure run; cdecl;//Deprecated
    //{class} property this$0: Jimpl_Prntr read _Getthis$0;
  end;

  [JavaSignature('br/com/gertec/gedi/impl/Prntr$1')]
  JPrntr_1 = interface(JRunnable)
    ['{7381F7AB-1A40-42A4-9B22-322C5CD1C353}']
  end;
  TJPrntr_1 = class(TJavaGenericImport<JPrntr_1Class, JPrntr_1>) end;

  JPrntr_2Class = interface(JObjectClass)
    ['{52F34032-404E-4A1D-B07C-E6479A3C172B}']
    //{class} function _Get$SwitchMap$android$graphics$Paint$Align: TJavaArray<Integer>; cdecl;
//    {class} function _Get$SwitchMap$br$com$gertec$gedi$enums$GEDI_PRNTR_e_BarCodeType: TJavaArray<Integer>; cdecl;
//    {class} property $SwitchMap$android$graphics$Paint$Align: TJavaArray<Integer> read _Get$SwitchMap$android$graphics$Paint$Align;
//    {class} property $SwitchMap$br$com$gertec$gedi$enums$GEDI_PRNTR_e_BarCodeType: TJavaArray<Integer> read _Get$SwitchMap$br$com$gertec$gedi$enums$GEDI_PRNTR_e_BarCodeType;
  end;

  [JavaSignature('br/com/gertec/gedi/impl/Prntr$2')]
  JPrntr_2 = interface(JObject)
    ['{7C196C13-B11C-4C5C-AC1E-BB02E60A548E}']
  end;
  TJPrntr_2 = class(TJavaGenericImport<JPrntr_2Class, JPrntr_2>) end;

  JThreadPoolManagerClass = interface(JObjectClass)
    ['{E9363226-926F-48E5-A3BB-52B00262100E}']
    {class} function getInstance: JThreadPoolManager; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/impl/ThreadPoolManager')]
  JThreadPoolManager = interface(JObject)
    ['{14B7F032-66BE-4B41-8F26-5D220337333F}']
    procedure executeTask(P1: JRunnable); cdecl;
  end;
  TJThreadPoolManager = class(TJavaGenericImport<JThreadPoolManagerClass, JThreadPoolManager>) end;

  JIAUDIOClass = interface(IJavaClass)
    ['{5AB0C3D0-0C7D-4E1C-BF50-DDFE061F817D}']
  end;

  [JavaSignature('br/com/gertec/gedi/interfaces/IAUDIO')]
  JIAUDIO = interface(IJavaInstance)
    ['{DD963E47-A6A2-4DFE-87C9-5E6C9C1E915B}']
    procedure Beep; cdecl;
  end;
  TJIAUDIO = class(TJavaGenericImport<JIAUDIOClass, JIAUDIO>) end;

  JIEnumsClass = interface(IJavaClass)
    ['{5E528D8F-BA6D-449F-9627-8F9004A77A4B}']
  end;

  [JavaSignature('br/com/gertec/gedi/interfaces/IEnums')]
  JIEnums = interface(IJavaInstance)
    ['{CD7281D6-BDB1-4D37-AD42-A3D0B16B7232}']
    function getValue: Integer; cdecl;
  end;
  TJIEnums = class(TJavaGenericImport<JIEnumsClass, JIEnums>) end;

  JGEDI_AUTH_st_DataClass = interface(JObjectClass)
    ['{B2A61D3D-EBCD-4987-ADEF-ADDF55414705}']
    {class} function _GetabHash: TJavaArray<Byte>; cdecl;
    {class} procedure _SetabHash(Value: TJavaArray<Byte>); cdecl;
    {class} function _GetabICV: TJavaArray<Byte>; cdecl;
    {class} procedure _SetabICV(Value: TJavaArray<Byte>); cdecl;
    {class} function _GetabInput: TJavaArray<Byte>; cdecl;
    {class} procedure _SetabInput(Value: TJavaArray<Byte>); cdecl;
    {class} function _GetabOutput: TJavaArray<Byte>; cdecl;
    {class} procedure _SetabOutput(Value: TJavaArray<Byte>); cdecl;
    {class} function _GeteMode: Integer; cdecl;
    {class} procedure _SeteMode(Value: Integer); cdecl;
    {class} function _GetuiInputLen: Integer; cdecl;
    {class} procedure _SetuiInputLen(Value: Integer); cdecl;
    {class} function _GetuiKeyIndex: Integer; cdecl;
    {class} procedure _SetuiKeyIndex(Value: Integer); cdecl;
    {class} function init: JGEDI_AUTH_st_Data; cdecl;//Deprecated
    {class} property abHash: TJavaArray<Byte> read _GetabHash write _SetabHash;
    {class} property abICV: TJavaArray<Byte> read _GetabICV write _SetabICV;
    {class} property abInput: TJavaArray<Byte> read _GetabInput write _SetabInput;
    {class} property abOutput: TJavaArray<Byte> read _GetabOutput write _SetabOutput;
    {class} property eMode: Integer read _GeteMode write _SeteMode;
    {class} property uiInputLen: Integer read _GetuiInputLen write _SetuiInputLen;
    {class} property uiKeyIndex: Integer read _GetuiKeyIndex write _SetuiKeyIndex;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_AUTH_st_Data')]
  JGEDI_AUTH_st_Data = interface(JObject)
    ['{E39F5C6E-04E4-4B70-BFF9-0FDE77680EB2}']
    function _GeteOperation: Integer; cdecl;
    procedure _SeteOperation(Value: Integer); cdecl;
    property eOperation: Integer read _GeteOperation write _SeteOperation;
  end;
  TJGEDI_AUTH_st_Data = class(TJavaGenericImport<JGEDI_AUTH_st_DataClass, JGEDI_AUTH_st_Data>) end;

  JGEDI_CLOCK_st_RTCClass = interface(JObjectClass)
    ['{47F2F419-B044-404B-B024-F9E574FCEBDB}']
    {class} function _GetbDay: Byte; cdecl;
    {class} procedure _SetbDay(Value: Byte); cdecl;
    {class} function _GetbDoW: Byte; cdecl;
    {class} procedure _SetbDoW(Value: Byte); cdecl;
    {class} function _GetbHour: Byte; cdecl;
    {class} procedure _SetbHour(Value: Byte); cdecl;
    {class} function _GetbMinute: Byte; cdecl;
    {class} procedure _SetbMinute(Value: Byte); cdecl;
    {class} function _GetbMonth: Byte; cdecl;
    {class} procedure _SetbMonth(Value: Byte); cdecl;
    {class} function _GetbSecond: Byte; cdecl;
    {class} procedure _SetbSecond(Value: Byte); cdecl;
    {class} function init: JGEDI_CLOCK_st_RTC; cdecl; overload;//Deprecated
    {class} function init(P1: Byte; P2: Byte; P3: Byte; P4: Byte; P5: Byte; P6: Byte; P7: Byte): JGEDI_CLOCK_st_RTC; cdecl; overload;//Deprecated
    {class} property bDay: Byte read _GetbDay write _SetbDay;
    {class} property bDoW: Byte read _GetbDoW write _SetbDoW;
    {class} property bHour: Byte read _GetbHour write _SetbHour;
    {class} property bMinute: Byte read _GetbMinute write _SetbMinute;
    {class} property bMonth: Byte read _GetbMonth write _SetbMonth;
    {class} property bSecond: Byte read _GetbSecond write _SetbSecond;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_CLOCK_st_RTC')]
  JGEDI_CLOCK_st_RTC = interface(JObject)
    ['{BAF824E2-68F2-461A-AF50-59EA95DEF7A1}']
    function _GetbYear: Byte; cdecl;
    procedure _SetbYear(Value: Byte); cdecl;
    property bYear: Byte read _GetbYear write _SetbYear;
  end;
  TJGEDI_CLOCK_st_RTC = class(TJavaGenericImport<JGEDI_CLOCK_st_RTCClass, JGEDI_CLOCK_st_RTC>) end;

  JGEDI_CL_st_ISO_PollingInfoClass = interface(JObjectClass)
    ['{0684BA0F-0163-4DC6-B08F-C123AAF8B54F}']
    {class} function _GetabATQA: TJavaArray<Byte>; cdecl;
    {class} procedure _SetabATQA(Value: TJavaArray<Byte>); cdecl;
    {class} function _GetabATQB: TJavaArray<Byte>; cdecl;
    {class} procedure _SetabATQB(Value: TJavaArray<Byte>); cdecl;
    {class} function _GetabATS: TJavaArray<Byte>; cdecl;
    {class} procedure _SetabATS(Value: TJavaArray<Byte>); cdecl;
    {class} function _GetabATTRIBResp: TJavaArray<Byte>; cdecl;
    {class} procedure _SetabATTRIBResp(Value: TJavaArray<Byte>); cdecl;
    {class} function _GetabPUPI: TJavaArray<Byte>; cdecl;
    {class} procedure _SetabPUPI(Value: TJavaArray<Byte>); cdecl;
    {class} function _GetabUID: TJavaArray<Byte>; cdecl;
    {class} procedure _SetabUID(Value: TJavaArray<Byte>); cdecl;
    {class} function _GetbSAK: Byte; cdecl;
    {class} procedure _SetbSAK(Value: Byte); cdecl;
    {class} function init: JGEDI_CL_st_ISO_PollingInfo; cdecl;//Deprecated
    {class} property abATQA: TJavaArray<Byte> read _GetabATQA write _SetabATQA;
    {class} property abATQB: TJavaArray<Byte> read _GetabATQB write _SetabATQB;
    {class} property abATS: TJavaArray<Byte> read _GetabATS write _SetabATS;
    {class} property abATTRIBResp: TJavaArray<Byte> read _GetabATTRIBResp write _SetabATTRIBResp;
    {class} property abPUPI: TJavaArray<Byte> read _GetabPUPI write _SetabPUPI;
    {class} property abUID: TJavaArray<Byte> read _GetabUID write _SetabUID;
    {class} property bSAK: Byte read _GetbSAK write _SetbSAK;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_CL_st_ISO_PollingInfo')]
  JGEDI_CL_st_ISO_PollingInfo = interface(JObject)
    ['{5B756948-5F0C-4A82-A49F-1F7EA8D755C3}']
    function _GetpeType: JGEDI_CL_e_ISO_Type; cdecl;
    procedure _SetpeType(Value: JGEDI_CL_e_ISO_Type); cdecl;
    property peType: JGEDI_CL_e_ISO_Type read _GetpeType write _SetpeType;
  end;
  TJGEDI_CL_st_ISO_PollingInfo = class(TJavaGenericImport<JGEDI_CL_st_ISO_PollingInfoClass, JGEDI_CL_st_ISO_PollingInfo>) end;

  JGEDI_CL_st_MF_ActivateInfoClass = interface(JObjectClass)
    ['{7F5CDA89-328D-4086-88D7-38144B733B1A}']
    {class} function _GetabUID: TJavaArray<Byte>; cdecl;
    {class} procedure _SetabUID(Value: TJavaArray<Byte>); cdecl;
    {class} function init: JGEDI_CL_st_MF_ActivateInfo; cdecl;//Deprecated
    {class} property abUID: TJavaArray<Byte> read _GetabUID write _SetabUID;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_CL_st_MF_ActivateInfo')]
  JGEDI_CL_st_MF_ActivateInfo = interface(JObject)
    ['{0DCBA10D-24D4-43E5-ACDC-EC4E6B507A55}']
    function _Gettype: JGEDI_CL_e_MF_Type; cdecl;
    procedure _Settype(Value: JGEDI_CL_e_MF_Type); cdecl;
    property &type: JGEDI_CL_e_MF_Type read _Gettype write _Settype;
  end;
  TJGEDI_CL_st_MF_ActivateInfo = class(TJavaGenericImport<JGEDI_CL_st_MF_ActivateInfoClass, JGEDI_CL_st_MF_ActivateInfo>) end;

  JGEDI_CL_st_MF_KeyClass = interface(JObjectClass)
    ['{2105914C-F479-43EB-AA3E-CB5DFA44CB17}']
    {class} function _GeteType: JGEDI_CL_e_MF_KeyType; cdecl;
    {class} procedure _SeteType(Value: JGEDI_CL_e_MF_KeyType); cdecl;
    {class} function init: JGEDI_CL_st_MF_Key; cdecl;//Deprecated
    {class} property eType: JGEDI_CL_e_MF_KeyType read _GeteType write _SeteType;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_CL_st_MF_Key')]
  JGEDI_CL_st_MF_Key = interface(JObject)
    ['{7DAEDB7B-484A-4645-8070-F37AAC1B45DC}']
    function _GetabValue: TJavaArray<Byte>; cdecl;
    procedure _SetabValue(Value: TJavaArray<Byte>); cdecl;
    property abValue: TJavaArray<Byte> read _GetabValue write _SetabValue;
  end;
  TJGEDI_CL_st_MF_Key = class(TJavaGenericImport<JGEDI_CL_st_MF_KeyClass, JGEDI_CL_st_MF_Key>) end;

  JGEDI_CL_st_ResetEMVInfoClass = interface(JObjectClass)
    ['{1C906796-891F-42FE-86BB-CDC46637B21C}']
    {class} function _GetpeCardType: JGEDI_CL_e_ISO_Type; cdecl;
    {class} procedure _SetpeCardType(Value: JGEDI_CL_e_ISO_Type); cdecl;
    {class} function init: JGEDI_CL_st_ResetEMVInfo; cdecl;//Deprecated
    {class} property peCardType: JGEDI_CL_e_ISO_Type read _GetpeCardType write _SetpeCardType;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_CL_st_ResetEMVInfo')]
  JGEDI_CL_st_ResetEMVInfo = interface(JObject)
    ['{090239C3-3E59-4B82-8446-B2EC4203D04B}']
    function _GetabATR: TJavaArray<Byte>; cdecl;
    procedure _SetabATR(Value: TJavaArray<Byte>); cdecl;
    property abATR: TJavaArray<Byte> read _GetabATR write _SetabATR;
  end;
  TJGEDI_CL_st_ResetEMVInfo = class(TJavaGenericImport<JGEDI_CL_st_ResetEMVInfoClass, JGEDI_CL_st_ResetEMVInfo>) end;

  JGEDI_CRYPT_st_RSAKeyGenClass = interface(JObjectClass)
    ['{EABCEF22-09C1-473E-8B2D-FDF552EC8C60}']
    {class} function _GetabModulus: TJavaArray<Byte>; cdecl;
    {class} procedure _SetabModulus(Value: TJavaArray<Byte>); cdecl;
    {class} function _GetabPrivateKey: TJavaArray<Byte>; cdecl;
    {class} procedure _SetabPrivateKey(Value: TJavaArray<Byte>); cdecl;
    {class} function _GetabPublicKey: TJavaArray<Byte>; cdecl;
    {class} procedure _SetabPublicKey(Value: TJavaArray<Byte>); cdecl;
    {class} function _GetuiBits: Integer; cdecl;
    {class} procedure _SetuiBits(Value: Integer); cdecl;
    {class} function _GetuiModulusLen: Integer; cdecl;
    {class} procedure _SetuiModulusLen(Value: Integer); cdecl;
    {class} function _GetuiPrivateKeyLen: Integer; cdecl;
    {class} procedure _SetuiPrivateKeyLen(Value: Integer); cdecl;
    {class} function _GetuiPublicKeyLen: Integer; cdecl;
    {class} procedure _SetuiPublicKeyLen(Value: Integer); cdecl;
    {class} function init: JGEDI_CRYPT_st_RSAKeyGen; cdecl;//Deprecated
    {class} property abModulus: TJavaArray<Byte> read _GetabModulus write _SetabModulus;
    {class} property abPrivateKey: TJavaArray<Byte> read _GetabPrivateKey write _SetabPrivateKey;
    {class} property abPublicKey: TJavaArray<Byte> read _GetabPublicKey write _SetabPublicKey;
    {class} property uiBits: Integer read _GetuiBits write _SetuiBits;
    {class} property uiModulusLen: Integer read _GetuiModulusLen write _SetuiModulusLen;
    {class} property uiPrivateKeyLen: Integer read _GetuiPrivateKeyLen write _SetuiPrivateKeyLen;
    {class} property uiPublicKeyLen: Integer read _GetuiPublicKeyLen write _SetuiPublicKeyLen;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_CRYPT_st_RSAKeyGen')]
  JGEDI_CRYPT_st_RSAKeyGen = interface(JObject)
    ['{9DAF3D63-37C7-4979-86E2-EA41D70C6D88}']
    function _GetbVersion: Byte; cdecl;
    procedure _SetbVersion(Value: Byte); cdecl;
    property bVersion: Byte read _GetbVersion write _SetbVersion;
  end;
  TJGEDI_CRYPT_st_RSAKeyGen = class(TJavaGenericImport<JGEDI_CRYPT_st_RSAKeyGenClass, JGEDI_CRYPT_st_RSAKeyGen>) end;

  JGEDI_KBD_st_InfoClass = interface(JObjectClass)
    ['{AC3C9880-5863-4AED-8A0E-30233613660F}']
    {class} function _Getactivity: JActivity; cdecl;
    {class} procedure _Setactivity(Value: JActivity); cdecl;
    {class} function _Getbtn1: JButton; cdecl;
    {class} procedure _Setbtn1(Value: JButton); cdecl;
    {class} function _Getbtn2: JButton; cdecl;
    {class} procedure _Setbtn2(Value: JButton); cdecl;
    {class} function _Getbtn3: JButton; cdecl;
    {class} procedure _Setbtn3(Value: JButton); cdecl;
    {class} function _Getbtn4: JButton; cdecl;
    {class} procedure _Setbtn4(Value: JButton); cdecl;
    {class} function _Getbtn5: JButton; cdecl;
    {class} procedure _Setbtn5(Value: JButton); cdecl;
    {class} function _Getbtn6: JButton; cdecl;
    {class} procedure _Setbtn6(Value: JButton); cdecl;
    {class} function _Getbtn7: JButton; cdecl;
    {class} procedure _Setbtn7(Value: JButton); cdecl;
    {class} function _Getbtn8: JButton; cdecl;
    {class} procedure _Setbtn8(Value: JButton); cdecl;
    {class} function _Getbtn9: JButton; cdecl;
    {class} procedure _Setbtn9(Value: JButton); cdecl;
    {class} function _GetbtnCancel: JButton; cdecl;
    {class} procedure _SetbtnCancel(Value: JButton); cdecl;
    {class} function _GetbtnClear: JButton; cdecl;
    {class} procedure _SetbtnClear(Value: JButton); cdecl;
    {class} function _GetbtnConfirm: JButton; cdecl;
    {class} procedure _SetbtnConfirm(Value: JButton); cdecl;
    {class} function clone: JGEDI_KBD_st_Info; cdecl;//Deprecated
    {class} function getButtonByIndex(P1: Integer): JButton; cdecl;//Deprecated
    {class} function init(P1: JButton; P2: JButton; P3: JButton; P4: JButton; P5: JButton; P6: JButton; P7: JButton; P8: JButton; P9: JButton; P10: JButton; P11: JButton; P12: JButton; P13: JButton; P14: JActivity): JGEDI_KBD_st_Info; cdecl;//Deprecated
    {class} property activity: JActivity read _Getactivity write _Setactivity;
    {class} property btn1: JButton read _Getbtn1 write _Setbtn1;
    {class} property btn2: JButton read _Getbtn2 write _Setbtn2;
    {class} property btn3: JButton read _Getbtn3 write _Setbtn3;
    {class} property btn4: JButton read _Getbtn4 write _Setbtn4;
    {class} property btn5: JButton read _Getbtn5 write _Setbtn5;
    {class} property btn6: JButton read _Getbtn6 write _Setbtn6;
    {class} property btn7: JButton read _Getbtn7 write _Setbtn7;
    {class} property btn8: JButton read _Getbtn8 write _Setbtn8;
    {class} property btn9: JButton read _Getbtn9 write _Setbtn9;
    {class} property btnCancel: JButton read _GetbtnCancel write _SetbtnCancel;
    {class} property btnClear: JButton read _GetbtnClear write _SetbtnClear;
    {class} property btnConfirm: JButton read _GetbtnConfirm write _SetbtnConfirm;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_KBD_st_Info')]
  JGEDI_KBD_st_Info = interface(JObject)
    ['{55FF3467-F696-4829-A7CA-03F6526A102C}']
    function _Getbtn0: JButton; cdecl;
    procedure _Setbtn0(Value: JButton); cdecl;
    property btn0: JButton read _Getbtn0 write _Setbtn0;
  end;
  TJGEDI_KBD_st_Info = class(TJavaGenericImport<JGEDI_KBD_st_InfoClass, JGEDI_KBD_st_Info>) end;

  JGEDI_KMS_st_CapturePINBlockInfoClass = interface(JObjectClass)
    ['{7F7BBF51-AB66-4699-82CE-9EA0C1AFB862}']
    {class} function _GetastPB: JList; cdecl;
    {class} procedure _SetastPB(Value: JList); cdecl;
    {class} function _GetpstData: JGEDI_KMS_st_Data; cdecl;
    {class} procedure _SetpstData(Value: JGEDI_KMS_st_Data); cdecl;
    {class} function init(P1: JGEDI_KMS_st_Control; P2: JGEDI_KMS_st_Data; P3: JList): JGEDI_KMS_st_CapturePINBlockInfo; cdecl;//Deprecated
    {class} property astPB: JList read _GetastPB write _SetastPB;
    {class} property pstData: JGEDI_KMS_st_Data read _GetpstData write _SetpstData;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_KMS_st_CapturePINBlockInfo')]
  JGEDI_KMS_st_CapturePINBlockInfo = interface(JObject)
    ['{50354BFB-447F-4AD0-A3B1-253FDC8DBBA3}']
    function _GetpstControl: JGEDI_KMS_st_Control; cdecl;
    procedure _SetpstControl(Value: JGEDI_KMS_st_Control); cdecl;
    property pstControl: JGEDI_KMS_st_Control read _GetpstControl write _SetpstControl;
  end;
  TJGEDI_KMS_st_CapturePINBlockInfo = class(TJavaGenericImport<JGEDI_KMS_st_CapturePINBlockInfoClass, JGEDI_KMS_st_CapturePINBlockInfo>) end;

  JGEDI_KMS_st_ControlClass = interface(JObjectClass)
    ['{DF6FBC61-7FA0-4759-B17D-1243D04181B6}']
    {class} function _GetacClearPIN: JString; cdecl;
    {class} procedure _SetacClearPIN(Value: JString); cdecl;
    {class} function _GetboAutoEnd: Boolean; cdecl;
    {class} procedure _SetboAutoEnd(Value: Boolean); cdecl;
    {class} function _GetboClearSingle: Boolean; cdecl;
    {class} procedure _SetboClearSingle(Value: Boolean); cdecl;
    {class} function _GetboNullPIN: Boolean; cdecl;
    {class} procedure _SetboNullPIN(Value: Boolean); cdecl;
    {class} function _Getcb: JGEDI_KMS_st_Control_Callbacks; cdecl;
    {class} procedure _Setcb(Value: JGEDI_KMS_st_Control_Callbacks); cdecl;
    {class} function _GetuiEventTimeout: Integer; cdecl;
    {class} procedure _SetuiEventTimeout(Value: Integer); cdecl;
    {class} function _GetuiGlobalTimeout: Integer; cdecl;
    {class} procedure _SetuiGlobalTimeout(Value: Integer); cdecl;
    {class} function _GetuiMaxPINLen: Integer; cdecl;
    {class} procedure _SetuiMaxPINLen(Value: Integer); cdecl;
    {class} function _GetuiMinPINLen: Integer; cdecl;
    {class} procedure _SetuiMinPINLen(Value: Integer); cdecl;
    {class} function init: JGEDI_KMS_st_Control; cdecl; overload;//Deprecated
    {class} function init(P1: Byte; P2: Integer; P3: Integer; P4: Boolean; P5: Boolean; P6: Boolean; P7: Integer; P8: Integer; P9: JGEDI_KMS_st_Control_Callbacks): JGEDI_KMS_st_Control; cdecl; overload;//Deprecated
    {class} property acClearPIN: JString read _GetacClearPIN write _SetacClearPIN;
    {class} property boAutoEnd: Boolean read _GetboAutoEnd write _SetboAutoEnd;
    {class} property boClearSingle: Boolean read _GetboClearSingle write _SetboClearSingle;
    {class} property boNullPIN: Boolean read _GetboNullPIN write _SetboNullPIN;
    {class} property cb: JGEDI_KMS_st_Control_Callbacks read _Getcb write _Setcb;
    {class} property uiEventTimeout: Integer read _GetuiEventTimeout write _SetuiEventTimeout;
    {class} property uiGlobalTimeout: Integer read _GetuiGlobalTimeout write _SetuiGlobalTimeout;
    {class} property uiMaxPINLen: Integer read _GetuiMaxPINLen write _SetuiMaxPINLen;
    {class} property uiMinPINLen: Integer read _GetuiMinPINLen write _SetuiMinPINLen;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_KMS_st_Control')]
  JGEDI_KMS_st_Control = interface(JObject)
    ['{1CEDA81A-C2CC-4A10-AE57-4B76FD73F8F8}']
    function _GetbVersion: Byte; cdecl;
    procedure _SetbVersion(Value: Byte); cdecl;
    property bVersion: Byte read _GetbVersion write _SetbVersion;
  end;
  TJGEDI_KMS_st_Control = class(TJavaGenericImport<JGEDI_KMS_st_ControlClass, JGEDI_KMS_st_Control>) end;

  JGEDI_KMS_st_DataClass = interface(JObjectClass)
    ['{BF683F8A-B50A-4DA5-B81D-1DB3E767D92D}']
    {class} function _GetabICV: TJavaArray<Byte>; cdecl;
    {class} procedure _SetabICV(Value: TJavaArray<Byte>); cdecl;
    {class} function _GetabInput: TJavaArray<Byte>; cdecl;
    {class} procedure _SetabInput(Value: TJavaArray<Byte>); cdecl;
    {class} function _GetabKSN: TJavaArray<Byte>; cdecl;
    {class} procedure _SetabKSN(Value: TJavaArray<Byte>); cdecl;
    {class} function _GetabOutput: TJavaArray<Byte>; cdecl;
    {class} procedure _SetabOutput(Value: TJavaArray<Byte>); cdecl;
    {class} function _GetabSK: TJavaArray<Byte>; cdecl;
    {class} procedure _SetabSK(Value: TJavaArray<Byte>); cdecl;
    {class} function _GeteKeyType: JGEDI_KMS_e_KEYTYPE; cdecl;
    {class} procedure _SeteKeyType(Value: JGEDI_KMS_e_KEYTYPE); cdecl;
    {class} function _GeteMode: JGEDI_KMS_e_EncMode; cdecl;
    {class} procedure _SeteMode(Value: JGEDI_KMS_e_EncMode); cdecl;
    {class} function _GeteOperation: JGEDI_KMS_e_OP; cdecl;
    {class} procedure _SeteOperation(Value: JGEDI_KMS_e_OP); cdecl;
    {class} function _GetuiKeyIndex: Integer; cdecl;
    {class} procedure _SetuiKeyIndex(Value: Integer); cdecl;
    {class} function init: JGEDI_KMS_st_Data; cdecl; overload;//Deprecated
    {class} function init(P1: Byte; P2: JGEDI_KMS_e_OP; P3: JGEDI_KMS_e_KEYTYPE; P4: Integer; P5: JGEDI_KMS_e_EncMode; P6: TJavaArray<Byte>; P7: TJavaArray<Byte>; P8: TJavaArray<Byte>): JGEDI_KMS_st_Data; cdecl; overload;//Deprecated
    {class} property abICV: TJavaArray<Byte> read _GetabICV write _SetabICV;
    {class} property abInput: TJavaArray<Byte> read _GetabInput write _SetabInput;
    {class} property abKSN: TJavaArray<Byte> read _GetabKSN write _SetabKSN;
    {class} property abOutput: TJavaArray<Byte> read _GetabOutput write _SetabOutput;
    {class} property abSK: TJavaArray<Byte> read _GetabSK write _SetabSK;
    {class} property eKeyType: JGEDI_KMS_e_KEYTYPE read _GeteKeyType write _SeteKeyType;
    {class} property eMode: JGEDI_KMS_e_EncMode read _GeteMode write _SeteMode;
    {class} property eOperation: JGEDI_KMS_e_OP read _GeteOperation write _SeteOperation;
    {class} property uiKeyIndex: Integer read _GetuiKeyIndex write _SetuiKeyIndex;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_KMS_st_Data')]
  JGEDI_KMS_st_Data = interface(JObject)
    ['{C5A41F1A-1E18-4F48-BA0D-2C3BD84E6A00}']
    function _GetbVersion: Byte; cdecl;
    procedure _SetbVersion(Value: Byte); cdecl;
    property bVersion: Byte read _GetbVersion write _SetbVersion;
  end;
  TJGEDI_KMS_st_Data = class(TJavaGenericImport<JGEDI_KMS_st_DataClass, JGEDI_KMS_st_Data>) end;

  JGEDI_KMS_st_KBClass = interface(JObjectClass)
    ['{BED0F87B-2E48-470D-B39A-C7FC4C2E58C1}']
    {class} function _GetabKCV: TJavaArray<Byte>; cdecl;
    {class} procedure _SetabKCV(Value: TJavaArray<Byte>); cdecl;
    {class} function _GetabKEKIndex: TJavaArray<Byte>; cdecl;
    {class} procedure _SetabKEKIndex(Value: TJavaArray<Byte>); cdecl;
    {class} function _GetabKSN: TJavaArray<Byte>; cdecl;
    {class} procedure _SetabKSN(Value: TJavaArray<Byte>); cdecl;
    {class} function _GetabKey: TJavaArray<Byte>; cdecl;
    {class} procedure _SetabKey(Value: TJavaArray<Byte>); cdecl;
    {class} function _GetabKeyIndex: TJavaArray<Byte>; cdecl;
    {class} procedure _SetabKeyIndex(Value: TJavaArray<Byte>); cdecl;
    {class} function _GetabRND: TJavaArray<Byte>; cdecl;
    {class} procedure _SetabRND(Value: TJavaArray<Byte>); cdecl;
    {class} function _GetbKEKType: JGEDI_KMS_e_KEYTYPE; cdecl;
    {class} procedure _SetbKEKType(Value: JGEDI_KMS_e_KEYTYPE); cdecl;
    {class} function _GetbKeyPurpose: JGEDI_KMS_e_KEYPURPOSE; cdecl;
    {class} procedure _SetbKeyPurpose(Value: JGEDI_KMS_e_KEYPURPOSE); cdecl;
    {class} function _GetbKeyType: JGEDI_KMS_e_KEYTYPE; cdecl;
    {class} procedure _SetbKeyType(Value: JGEDI_KMS_e_KEYTYPE); cdecl;
    {class} function init: JGEDI_KMS_st_KB; cdecl;//Deprecated
    {class} property abKCV: TJavaArray<Byte> read _GetabKCV write _SetabKCV;
    {class} property abKEKIndex: TJavaArray<Byte> read _GetabKEKIndex write _SetabKEKIndex;
    {class} property abKSN: TJavaArray<Byte> read _GetabKSN write _SetabKSN;
    {class} property abKey: TJavaArray<Byte> read _GetabKey write _SetabKey;
    {class} property abKeyIndex: TJavaArray<Byte> read _GetabKeyIndex write _SetabKeyIndex;
    {class} property abRND: TJavaArray<Byte> read _GetabRND write _SetabRND;
    {class} property bKEKType: JGEDI_KMS_e_KEYTYPE read _GetbKEKType write _SetbKEKType;
    {class} property bKeyPurpose: JGEDI_KMS_e_KEYPURPOSE read _GetbKeyPurpose write _SetbKeyPurpose;
    {class} property bKeyType: JGEDI_KMS_e_KEYTYPE read _GetbKeyType write _SetbKeyType;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_KMS_st_KB')]
  JGEDI_KMS_st_KB = interface(JObject)
    ['{A4BE7B88-4F17-4339-B3E6-A54F44F8071F}']
    function _GetabKeyLen: TJavaArray<Byte>; cdecl;
    procedure _SetabKeyLen(Value: TJavaArray<Byte>); cdecl;
    property abKeyLen: TJavaArray<Byte> read _GetabKeyLen write _SetabKeyLen;
  end;
  TJGEDI_KMS_st_KB = class(TJavaGenericImport<JGEDI_KMS_st_KBClass, JGEDI_KMS_st_KB>) end;

  JGEDI_KMS_st_PINBlockClass = interface(JObjectClass)
    ['{762C30BA-F8D9-4C50-9FF1-BEBEC2B3BAE0}']
    {class} function _GetabEncPINBlock: TJavaArray<Byte>; cdecl;
    {class} procedure _SetabEncPINBlock(Value: TJavaArray<Byte>); cdecl;
    {class} function _GetabSeq: TJavaArray<Byte>; cdecl;
    {class} procedure _SetabSeq(Value: TJavaArray<Byte>); cdecl;
    {class} function _GetbCN: Byte; cdecl;
    {class} procedure _SetbCN(Value: Byte); cdecl;
    {class} function _GetcPad: Byte; cdecl;
    {class} procedure _SetcPad(Value: Byte); cdecl;
    {class} function _GeteBlockType: JGEDI_KMS_e_BLOCKTYPE; cdecl;
    {class} procedure _SeteBlockType(Value: JGEDI_KMS_e_BLOCKTYPE); cdecl;
    {class} function _GetszPan: JString; cdecl;
    {class} procedure _SetszPan(Value: JString); cdecl;
    {class} function init: JGEDI_KMS_st_PINBlock; cdecl; overload;//Deprecated
    {class} function init(P1: Byte; P2: JGEDI_KMS_e_BLOCKTYPE; P3: JString; P4: TJavaArray<Byte>; P5: Byte; P6: Byte): JGEDI_KMS_st_PINBlock; cdecl; overload;//Deprecated
    {class} property abEncPINBlock: TJavaArray<Byte> read _GetabEncPINBlock write _SetabEncPINBlock;
    {class} property abSeq: TJavaArray<Byte> read _GetabSeq write _SetabSeq;
    {class} property bCN: Byte read _GetbCN write _SetbCN;
    {class} property cPad: Byte read _GetcPad write _SetcPad;
    {class} property eBlockType: JGEDI_KMS_e_BLOCKTYPE read _GeteBlockType write _SeteBlockType;
    {class} property szPan: JString read _GetszPan write _SetszPan;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_KMS_st_PINBlock')]
  JGEDI_KMS_st_PINBlock = interface(JObject)
    ['{227CF376-63F2-4BF0-BDD5-DDA2BFCFECA7}']
    function _GetbVersion: Byte; cdecl;
    procedure _SetbVersion(Value: Byte); cdecl;
    property bVersion: Byte read _GetbVersion write _SetbVersion;
  end;
  TJGEDI_KMS_st_PINBlock = class(TJavaGenericImport<JGEDI_KMS_st_PINBlockClass, JGEDI_KMS_st_PINBlock>) end;

  JGEDI_KMS_st_SaveKeyClass = interface(JObjectClass)
    ['{8415CC0F-F032-41C6-B5B2-6CB559B31254}']
    {class} function _GetabKSN: TJavaArray<Byte>; cdecl;
    {class} procedure _SetabKSN(Value: TJavaArray<Byte>); cdecl;
    {class} function _GetabKey: TJavaArray<Byte>; cdecl;
    {class} procedure _SetabKey(Value: TJavaArray<Byte>); cdecl;
    {class} function _GeteKeyPurpose: JGEDI_KMS_e_KEYPURPOSE; cdecl;
    {class} procedure _SeteKeyPurpose(Value: JGEDI_KMS_e_KEYPURPOSE); cdecl;
    {class} function _GeteKeyType: JGEDI_KMS_e_KEYTYPE; cdecl;
    {class} procedure _SeteKeyType(Value: JGEDI_KMS_e_KEYTYPE); cdecl;
    {class} function _GetuiKeyIndex: Integer; cdecl;
    {class} procedure _SetuiKeyIndex(Value: Integer); cdecl;
    {class} function init: JGEDI_KMS_st_SaveKey; cdecl; overload;//Deprecated
    {class} function init(P1: Byte; P2: JGEDI_KMS_e_KEYTYPE; P3: JGEDI_KMS_e_KEYPURPOSE; P4: Integer; P5: TJavaArray<Byte>; P6: TJavaArray<Byte>): JGEDI_KMS_st_SaveKey; cdecl; overload;//Deprecated
    {class} property abKSN: TJavaArray<Byte> read _GetabKSN write _SetabKSN;
    {class} property abKey: TJavaArray<Byte> read _GetabKey write _SetabKey;
    {class} property eKeyPurpose: JGEDI_KMS_e_KEYPURPOSE read _GeteKeyPurpose write _SeteKeyPurpose;
    {class} property eKeyType: JGEDI_KMS_e_KEYTYPE read _GeteKeyType write _SeteKeyType;
    {class} property uiKeyIndex: Integer read _GetuiKeyIndex write _SetuiKeyIndex;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_KMS_st_SaveKey')]
  JGEDI_KMS_st_SaveKey = interface(JObject)
    ['{CDC688D0-49A1-47E9-AF49-E2E7A61E7559}']
    function _GetbVersion: Byte; cdecl;
    procedure _SetbVersion(Value: Byte); cdecl;
    property bVersion: Byte read _GetbVersion write _SetbVersion;
  end;
  TJGEDI_KMS_st_SaveKey = class(TJavaGenericImport<JGEDI_KMS_st_SaveKeyClass, JGEDI_KMS_st_SaveKey>) end;

  JGEDI_MSR_st_LastErrorsClass = interface(JObjectClass)
    ['{7C116880-3CAF-418C-A665-2D2E1E00293D}']
    {class} function _GetpeTk2Err: JGEDI_MSR_e_Status; cdecl;
    {class} procedure _SetpeTk2Err(Value: JGEDI_MSR_e_Status); cdecl;
    {class} function _GetpeTk3Err: JGEDI_MSR_e_Status; cdecl;
    {class} procedure _SetpeTk3Err(Value: JGEDI_MSR_e_Status); cdecl;
    {class} function init: JGEDI_MSR_st_LastErrors; cdecl;//Deprecated
    {class} property peTk2Err: JGEDI_MSR_e_Status read _GetpeTk2Err write _SetpeTk2Err;
    {class} property peTk3Err: JGEDI_MSR_e_Status read _GetpeTk3Err write _SetpeTk3Err;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_MSR_st_LastErrors')]
  JGEDI_MSR_st_LastErrors = interface(JObject)
    ['{B46BF475-BF93-4267-92CE-26A4C212384E}']
    function _GetpeTk1Err: JGEDI_MSR_e_Status; cdecl;
    procedure _SetpeTk1Err(Value: JGEDI_MSR_e_Status); cdecl;
    property peTk1Err: JGEDI_MSR_e_Status read _GetpeTk1Err write _SetpeTk1Err;
  end;
  TJGEDI_MSR_st_LastErrors = class(TJavaGenericImport<JGEDI_MSR_st_LastErrorsClass, JGEDI_MSR_st_LastErrors>) end;

  JGEDI_MSR_st_TracksClass = interface(JObjectClass)
    ['{F61C4243-3791-4576-8D0F-F5AAAA3C28D5}']
    {class} function _GetabTk2Buf: TJavaArray<Byte>; cdecl;
    {class} procedure _SetabTk2Buf(Value: TJavaArray<Byte>); cdecl;
    {class} function _GetabTk3Buf: TJavaArray<Byte>; cdecl;
    {class} procedure _SetabTk3Buf(Value: TJavaArray<Byte>); cdecl;
    {class} function init: JGEDI_MSR_st_Tracks; cdecl;//Deprecated
    {class} property abTk2Buf: TJavaArray<Byte> read _GetabTk2Buf write _SetabTk2Buf;
    {class} property abTk3Buf: TJavaArray<Byte> read _GetabTk3Buf write _SetabTk3Buf;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_MSR_st_Tracks')]
  JGEDI_MSR_st_Tracks = interface(JObject)
    ['{C7798CD6-EE9F-48E8-893D-C855374DC53B}']
    function _GetabTk1Buf: TJavaArray<Byte>; cdecl;
    procedure _SetabTk1Buf(Value: TJavaArray<Byte>); cdecl;
    property abTk1Buf: TJavaArray<Byte> read _GetabTk1Buf write _SetabTk1Buf;
  end;
  TJGEDI_MSR_st_Tracks = class(TJavaGenericImport<JGEDI_MSR_st_TracksClass, JGEDI_MSR_st_Tracks>) end;

  JGEDI_PRNTR_st_BarCodeConfigClass = interface(JObjectClass)
    ['{7430F14C-AF7C-454E-B741-4601C64496E5}']
    {class} function _Getheight: Integer; cdecl;
    {class} procedure _Setheight(Value: Integer); cdecl;
    {class} function _Getwidth: Integer; cdecl;
    {class} procedure _Setwidth(Value: Integer); cdecl;
    {class} function init: JGEDI_PRNTR_st_BarCodeConfig; cdecl; overload;//Deprecated
    {class} function init(P1: JGEDI_PRNTR_e_BarCodeType; P2: Integer): JGEDI_PRNTR_st_BarCodeConfig; cdecl; overload;//Deprecated
    {class} function init(P1: JGEDI_PRNTR_e_BarCodeType; P2: Integer; P3: Integer): JGEDI_PRNTR_st_BarCodeConfig; cdecl; overload;//Deprecated
    {class} property height: Integer read _Getheight write _Setheight;
    {class} property width: Integer read _Getwidth write _Setwidth;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_PRNTR_st_BarCodeConfig')]
  JGEDI_PRNTR_st_BarCodeConfig = interface(JObject)
    ['{24A6D810-5CB1-49BD-9B85-1613344CB392}']
    function _GetbarCodeType: JGEDI_PRNTR_e_BarCodeType; cdecl;
    procedure _SetbarCodeType(Value: JGEDI_PRNTR_e_BarCodeType); cdecl;
    //Acrescentado Marcos
    function _Getheight: Integer; cdecl;
    procedure _Setheight(Value: Integer); cdecl;
    function _Getwidth: Integer; cdecl;
    procedure _Setwidth(Value: Integer); cdecl;
    //Fim Acrescentado Marcos
    property barCodeType: JGEDI_PRNTR_e_BarCodeType read _GetbarCodeType write _SetbarCodeType;
    property height:integer read _Getheight write _Setheight;
    property width:integer read _Getwidth write _Setwidth;
  end;
  TJGEDI_PRNTR_st_BarCodeConfig = class(TJavaGenericImport<JGEDI_PRNTR_st_BarCodeConfigClass, JGEDI_PRNTR_st_BarCodeConfig>) end;

  JGEDI_PRNTR_st_PictureConfigClass = interface(JObjectClass)
    ['{D7191B43-B0E9-498E-B072-8864D75CA786}']
    {class} function _Getheight: Integer; cdecl;
    {class} procedure _Setheight(Value: Integer); cdecl;
    {class} function _Getoffset: Integer; cdecl;
    {class} procedure _Setoffset(Value: Integer); cdecl;
    {class} function _Getwidth: Integer; cdecl;
    {class} procedure _Setwidth(Value: Integer); cdecl;
    {class} function init: JGEDI_PRNTR_st_PictureConfig; cdecl; overload;//Deprecated
    {class} function init(P1: JGEDI_PRNTR_e_Alignment): JGEDI_PRNTR_st_PictureConfig; cdecl; overload;//Deprecated
    {class} function init(P1: JGEDI_PRNTR_e_Alignment; P2: Integer; P3: Integer; P4: Integer): JGEDI_PRNTR_st_PictureConfig; cdecl; overload;//Deprecated
    {class} property height: Integer read _Getheight write _Setheight;
    {class} property offset: Integer read _Getoffset write _Setoffset;
    {class} property width: Integer read _Getwidth write _Setwidth;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_PRNTR_st_PictureConfig')]
  JGEDI_PRNTR_st_PictureConfig = interface(JObject)
    ['{7DC81AA0-1E81-45C5-9CD3-F625724F4998}']
    function _Getalignment: JGEDI_PRNTR_e_Alignment; cdecl;
    procedure _Setalignment(Value: JGEDI_PRNTR_e_Alignment); cdecl;
    //Acrescentado manualmente
    function _Getheight: Integer; cdecl;
    procedure _Setheight(Value: Integer); cdecl;
    function _Getwidth: Integer; cdecl;
    procedure _Setwidth(Value: Integer); cdecl;

    property alignment: JGEDI_PRNTR_e_Alignment read _Getalignment write _Setalignment;
    property height:integer read _Getheight write _Setheight;
    property width:integer read _Getwidth write _Setwidth;

  end;
  TJGEDI_PRNTR_st_PictureConfig = class(TJavaGenericImport<JGEDI_PRNTR_st_PictureConfigClass, JGEDI_PRNTR_st_PictureConfig>) end;

  JGEDI_PRNTR_st_StringConfigClass = interface(JObjectClass)
    ['{3D30F9B7-CA63-48FF-8071-CD28E9CD2711}']
    {class} function _GetlineSpace: Integer; cdecl;
    {class} procedure _SetlineSpace(Value: Integer); cdecl;
    {class} function _Getoffset: Integer; cdecl;
    {class} procedure _Setoffset(Value: Integer); cdecl;
    {class} function init: JGEDI_PRNTR_st_StringConfig; cdecl; overload;//Deprecated
    {class} function init(P1: JPaint): JGEDI_PRNTR_st_StringConfig; cdecl; overload;//Deprecated
    {class} function init(P1: JPaint; P2: Integer; P3: Integer): JGEDI_PRNTR_st_StringConfig; cdecl; overload;//Deprecated
    {class} property lineSpace: Integer read _GetlineSpace write _SetlineSpace;
    {class} property offset: Integer read _Getoffset write _Setoffset;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_PRNTR_st_StringConfig')]
  JGEDI_PRNTR_st_StringConfig = interface(JObject)
    ['{73916176-383B-47A9-B09A-A370F08BF6FD}']
    //Added Manually
    function _GetlineSpace: Integer; cdecl;
    procedure _SetlineSpace(Value: Integer); cdecl;
    function _Getoffset: Integer; cdecl;
    procedure _Setoffset(Value: Integer); cdecl;
    function _Getpaint: JPaint; cdecl;
    procedure _Setpaint(Value: JPaint); cdecl;
    property paint: JPaint read _Getpaint write _Setpaint;
    property lineSpace: Integer read _GetlineSpace write _SetlineSpace;
    property offset: Integer read _Getoffset write _Setoffset;

  end;
  TJGEDI_PRNTR_st_StringConfig = class(TJavaGenericImport<JGEDI_PRNTR_st_StringConfigClass, JGEDI_PRNTR_st_StringConfig>) end;

  JGEDI_SMART_st_ResetInfoClass = interface(JObjectClass)
    ['{47BE0B69-C6F3-44F9-838B-4BC868219861}']
    {class} function _GetpeCardType: JGEDI_SMART_e_Type; cdecl;
    {class} procedure _SetpeCardType(Value: JGEDI_SMART_e_Type); cdecl;
    {class} function init: JGEDI_SMART_st_ResetInfo; cdecl;//Deprecated
    {class} property peCardType: JGEDI_SMART_e_Type read _GetpeCardType write _SetpeCardType;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_SMART_st_ResetInfo')]
  JGEDI_SMART_st_ResetInfo = interface(JObject)
    ['{66B1A030-C776-48CD-8569-8FE6A5CA0CB1}']
    function _GetabATR: TJavaArray<Byte>; cdecl;
    procedure _SetabATR(Value: TJavaArray<Byte>); cdecl;
    property abATR: TJavaArray<Byte> read _GetabATR write _SetabATR;
  end;
  TJGEDI_SMART_st_ResetInfo = class(TJavaGenericImport<JGEDI_SMART_st_ResetInfoClass, JGEDI_SMART_st_ResetInfo>) end;

  JIPrinterCallbackClass = interface(JIInterfaceClass)
    ['{C8C7BCF2-CAF7-4125-98AF-71CE1E454209}']
  end;

  [JavaSignature('com/xcheng/printerservice/IPrinterCallback')]
  JIPrinterCallback = interface(JIInterface)
    ['{E424EA89-C5A5-433D-8169-05931D5FCC97}']
    procedure onComplete; cdecl;
    procedure onException(P1: Integer; P2: JString); cdecl;
    procedure onLength(P1: Int64; P2: Int64); cdecl;
  end;
  TJIPrinterCallback = class(TJavaGenericImport<JIPrinterCallbackClass, JIPrinterCallback>) end;

  JIPrinterCallback_Stub_ProxyClass = interface(JIPrinterCallbackClass)
    ['{ACF86EF6-50CD-4929-B1F3-B48052CD5173}']
  end;

  [JavaSignature('com/xcheng/printerservice/IPrinterCallback$Stub$Proxy')]
  JIPrinterCallback_Stub_Proxy = interface(JIPrinterCallback)
    ['{FA5EAA69-353B-4BC5-AFFB-4250E5B9D1D3}']
    function asBinder: JIBinder; cdecl;
    function getInterfaceDescriptor: JString; cdecl;
    procedure onComplete; cdecl;
    procedure onException(P1: Integer; P2: JString); cdecl;
    procedure onLength(P1: Int64; P2: Int64); cdecl;
  end;
  TJIPrinterCallback_Stub_Proxy = class(TJavaGenericImport<JIPrinterCallback_Stub_ProxyClass, JIPrinterCallback_Stub_Proxy>) end;

  JIPrinterServiceClass = interface(JIInterfaceClass)
    ['{2417D2EE-F66B-4A77-BD7B-BE9B5F5EAE6C}']
  end;

  [JavaSignature('com/xcheng/printerservice/IPrinterService')]
  JIPrinterService = interface(JIInterface)
    ['{1A7382B3-DFB3-4E26-84B7-48A5E59F629B}']
    function getBootloaderVersion: JString; cdecl;
    function getFirmwareVersion: JString; cdecl;
    procedure printBarCode(P1: JString; P2: Integer; P3: Integer; P4: Integer; P5: Boolean; P6: JIPrinterCallback); cdecl;
    procedure printBitmap(P1: JBitmap; P2: JIPrinterCallback); cdecl;
    procedure printBitmapWithAttributes(P1: JBitmap; P2: JMap; P3: JIPrinterCallback); cdecl;
    procedure printColumnsTextWithAttributes(P1: TJavaObjectArray<JString>; P2: JList; P3: JIPrinterCallback); cdecl;
    procedure printQRCode(P1: JString; P2: Integer; P3: Integer; P4: JIPrinterCallback); cdecl;
    procedure printText(P1: JString; P2: JIPrinterCallback); cdecl;
    procedure printTextWithAttributes(P1: JString; P2: JMap; P3: JIPrinterCallback); cdecl;
    procedure printWrapPaper(P1: Integer; P2: JIPrinterCallback); cdecl;
    procedure printerInit(P1: JIPrinterCallback); cdecl;
    function printerPaper(P1: JIPrinterCallback): Boolean; cdecl;
    procedure printerReset(P1: JIPrinterCallback); cdecl;
    function printerTemperature(P1: JIPrinterCallback): Integer; cdecl;
    procedure sendRAWData(P1: TJavaArray<Byte>; P2: JIPrinterCallback); cdecl;
    procedure setPrinterSpeed(P1: Integer; P2: JIPrinterCallback); cdecl;
    procedure upgradePrinter; cdecl;
  end;
  TJIPrinterService = class(TJavaGenericImport<JIPrinterServiceClass, JIPrinterService>) end;

  JIPrinterService_StubClass = interface(JBinderClass)
    ['{85269A8B-D942-443B-A0F5-67B2D636C5A0}']
    {class} function _GetTRANSACTION_getBootloaderVersion: Integer; cdecl;
    {class} function _GetTRANSACTION_getFirmwareVersion: Integer; cdecl;
    {class} function _GetTRANSACTION_printBarCode: Integer; cdecl;
    {class} function _GetTRANSACTION_printBitmap: Integer; cdecl;
    {class} function _GetTRANSACTION_printBitmapWithAttributes: Integer; cdecl;
    {class} function _GetTRANSACTION_printColumnsTextWithAttributes: Integer; cdecl;
    {class} function _GetTRANSACTION_printQRCode: Integer; cdecl;
    {class} function _GetTRANSACTION_printText: Integer; cdecl;
    {class} function _GetTRANSACTION_printTextWithAttributes: Integer; cdecl;
    {class} function _GetTRANSACTION_printWrapPaper: Integer; cdecl;
    {class} function _GetTRANSACTION_printerInit: Integer; cdecl;
    {class} function _GetTRANSACTION_printerPaper: Integer; cdecl;
    {class} function _GetTRANSACTION_printerReset: Integer; cdecl;
    {class} function _GetTRANSACTION_printerTemperature: Integer; cdecl;
    {class} function _GetTRANSACTION_sendRAWData: Integer; cdecl;
    {class} function _GetTRANSACTION_setPrinterSpeed: Integer; cdecl;
    {class} function asBinder: JIBinder; cdecl;//Deprecated
    {class} function asInterface(P1: JIBinder): JIPrinterService; cdecl;//Deprecated
    {class} function init: JIPrinterService_Stub; cdecl;//Deprecated
    {class} function onTransact(P1: Integer; P2: JParcel; P3: JParcel; P4: Integer): Boolean; cdecl;//Deprecated
    {class} property TRANSACTION_getBootloaderVersion: Integer read _GetTRANSACTION_getBootloaderVersion;
    {class} property TRANSACTION_getFirmwareVersion: Integer read _GetTRANSACTION_getFirmwareVersion;
    {class} property TRANSACTION_printBarCode: Integer read _GetTRANSACTION_printBarCode;
    {class} property TRANSACTION_printBitmap: Integer read _GetTRANSACTION_printBitmap;
    {class} property TRANSACTION_printBitmapWithAttributes: Integer read _GetTRANSACTION_printBitmapWithAttributes;
    {class} property TRANSACTION_printColumnsTextWithAttributes: Integer read _GetTRANSACTION_printColumnsTextWithAttributes;
    {class} property TRANSACTION_printQRCode: Integer read _GetTRANSACTION_printQRCode;
    {class} property TRANSACTION_printText: Integer read _GetTRANSACTION_printText;
    {class} property TRANSACTION_printTextWithAttributes: Integer read _GetTRANSACTION_printTextWithAttributes;
    {class} property TRANSACTION_printWrapPaper: Integer read _GetTRANSACTION_printWrapPaper;
    {class} property TRANSACTION_printerInit: Integer read _GetTRANSACTION_printerInit;
    {class} property TRANSACTION_printerPaper: Integer read _GetTRANSACTION_printerPaper;
    {class} property TRANSACTION_printerReset: Integer read _GetTRANSACTION_printerReset;
    {class} property TRANSACTION_printerTemperature: Integer read _GetTRANSACTION_printerTemperature;
    {class} property TRANSACTION_sendRAWData: Integer read _GetTRANSACTION_sendRAWData;
    {class} property TRANSACTION_setPrinterSpeed: Integer read _GetTRANSACTION_setPrinterSpeed;
  end;

  [JavaSignature('com/xcheng/printerservice/IPrinterService$Stub')]
  JIPrinterService_Stub = interface(JBinder)
    ['{B9676932-CD3A-4147-936F-B52D6A4068CA}']
  end;
  TJIPrinterService_Stub = class(TJavaGenericImport<JIPrinterService_StubClass, JIPrinterService_Stub>) end;

  JIPrinterService_Stub_ProxyClass = interface(JIPrinterServiceClass)
    ['{67FBBFA2-9CD6-4968-818B-47D9A4F980ED}']
  end;

  [JavaSignature('com/xcheng/printerservice/IPrinterService$Stub$Proxy')]
  JIPrinterService_Stub_Proxy = interface(JIPrinterService)
    ['{C8B90246-2658-46F0-9BFF-4EA2831C86E1}']
    function asBinder: JIBinder; cdecl;
    function getBootloaderVersion: JString; cdecl;
    function getFirmwareVersion: JString; cdecl;
    function getInterfaceDescriptor: JString; cdecl;
    procedure printBarCode(P1: JString; P2: Integer; P3: Integer; P4: Integer; P5: Boolean; P6: JIPrinterCallback); cdecl;
    procedure printBitmap(P1: JBitmap; P2: JIPrinterCallback); cdecl;
    procedure printBitmapWithAttributes(P1: JBitmap; P2: JMap; P3: JIPrinterCallback); cdecl;
    procedure printColumnsTextWithAttributes(P1: TJavaObjectArray<JString>; P2: JList; P3: JIPrinterCallback); cdecl;
    procedure printQRCode(P1: JString; P2: Integer; P3: Integer; P4: JIPrinterCallback); cdecl;
    procedure printText(P1: JString; P2: JIPrinterCallback); cdecl;
    procedure printTextWithAttributes(P1: JString; P2: JMap; P3: JIPrinterCallback); cdecl;
    procedure printWrapPaper(P1: Integer; P2: JIPrinterCallback); cdecl;
    procedure printerInit(P1: JIPrinterCallback); cdecl;
    function printerPaper(P1: JIPrinterCallback): Boolean; cdecl;
    procedure printerReset(P1: JIPrinterCallback); cdecl;
    function printerTemperature(P1: JIPrinterCallback): Integer; cdecl;
    procedure sendRAWData(P1: TJavaArray<Byte>; P2: JIPrinterCallback); cdecl;
    procedure setPrinterSpeed(P1: Integer; P2: JIPrinterCallback); cdecl;
    procedure upgradePrinter; cdecl;
  end;
  TJIPrinterService_Stub_Proxy = class(TJavaGenericImport<JIPrinterService_Stub_ProxyClass, JIPrinterService_Stub_Proxy>) end;

implementation

procedure RegisterTypes;
begin
  TRegTypes.RegisterType('G800Interface.JAnimator', TypeInfo(G800Interface.JAnimator));
  TRegTypes.RegisterType('G800Interface.JAnimator_AnimatorListener', TypeInfo(G800Interface.JAnimator_AnimatorListener));
  TRegTypes.RegisterType('G800Interface.JAnimator_AnimatorPauseListener', TypeInfo(G800Interface.JAnimator_AnimatorPauseListener));
  TRegTypes.RegisterType('G800Interface.JKeyframe', TypeInfo(G800Interface.JKeyframe));
  TRegTypes.RegisterType('G800Interface.JLayoutTransition', TypeInfo(G800Interface.JLayoutTransition));
  TRegTypes.RegisterType('G800Interface.JLayoutTransition_TransitionListener', TypeInfo(G800Interface.JLayoutTransition_TransitionListener));
  TRegTypes.RegisterType('G800Interface.JPropertyValuesHolder', TypeInfo(G800Interface.JPropertyValuesHolder));
  TRegTypes.RegisterType('G800Interface.JStateListAnimator', TypeInfo(G800Interface.JStateListAnimator));
  TRegTypes.RegisterType('G800Interface.JTimeInterpolator', TypeInfo(G800Interface.JTimeInterpolator));
  TRegTypes.RegisterType('G800Interface.JTypeConverter', TypeInfo(G800Interface.JTypeConverter));
  TRegTypes.RegisterType('G800Interface.JTypeEvaluator', TypeInfo(G800Interface.JTypeEvaluator));
  TRegTypes.RegisterType('G800Interface.JValueAnimator', TypeInfo(G800Interface.JValueAnimator));
  TRegTypes.RegisterType('G800Interface.JValueAnimator_AnimatorUpdateListener', TypeInfo(G800Interface.JValueAnimator_AnimatorUpdateListener));
  TRegTypes.RegisterType('G800Interface.JPathMotion', TypeInfo(G800Interface.JPathMotion));
  TRegTypes.RegisterType('G800Interface.JScene', TypeInfo(G800Interface.JScene));
  TRegTypes.RegisterType('G800Interface.JTransition', TypeInfo(G800Interface.JTransition));
  TRegTypes.RegisterType('G800Interface.JTransition_EpicenterCallback', TypeInfo(G800Interface.JTransition_EpicenterCallback));
  TRegTypes.RegisterType('G800Interface.JTransition_TransitionListener', TypeInfo(G800Interface.JTransition_TransitionListener));
  TRegTypes.RegisterType('G800Interface.JTransitionManager', TypeInfo(G800Interface.JTransitionManager));
  TRegTypes.RegisterType('G800Interface.JTransitionPropagation', TypeInfo(G800Interface.JTransitionPropagation));
  TRegTypes.RegisterType('G800Interface.JTransitionValues', TypeInfo(G800Interface.JTransitionValues));
  TRegTypes.RegisterType('G800Interface.JInterpolator', TypeInfo(G800Interface.JInterpolator));
  TRegTypes.RegisterType('G800Interface.JToolbar_LayoutParams', TypeInfo(G800Interface.JToolbar_LayoutParams));
  TRegTypes.RegisterType('G800Interface.Jgertec_BuildConfig', TypeInfo(G800Interface.Jgertec_BuildConfig));
  TRegTypes.RegisterType('G800Interface.Jgertec_Logger', TypeInfo(G800Interface.Jgertec_Logger));
  TRegTypes.RegisterType('G800Interface.Jgertec_Utils', TypeInfo(G800Interface.Jgertec_Utils));
  TRegTypes.RegisterType('G800Interface.JBuildConstants', TypeInfo(G800Interface.JBuildConstants));
  TRegTypes.RegisterType('G800Interface.JICL', TypeInfo(G800Interface.JICL));
  TRegTypes.RegisterType('G800Interface.JCL', TypeInfo(G800Interface.JCL));
  TRegTypes.RegisterType('G800Interface.JICLOCK', TypeInfo(G800Interface.JICLOCK));
  TRegTypes.RegisterType('G800Interface.Jgedi_CLOCK', TypeInfo(G800Interface.Jgedi_CLOCK));
  TRegTypes.RegisterType('G800Interface.JICRYPT', TypeInfo(G800Interface.JICRYPT));
  TRegTypes.RegisterType('G800Interface.JCRYPT', TypeInfo(G800Interface.JCRYPT));
  TRegTypes.RegisterType('G800Interface.JIGEDI', TypeInfo(G800Interface.JIGEDI));
  TRegTypes.RegisterType('G800Interface.JGEDI', TypeInfo(G800Interface.JGEDI));
  TRegTypes.RegisterType('G800Interface.JGEDI_1', TypeInfo(G800Interface.JGEDI_1));
  TRegTypes.RegisterType('G800Interface.JGEDI_RET', TypeInfo(G800Interface.JGEDI_RET));
  TRegTypes.RegisterType('G800Interface.JGEDI_KMS_st_Control_Callbacks', TypeInfo(G800Interface.JGEDI_KMS_st_Control_Callbacks));
  TRegTypes.RegisterType('G800Interface.JGediNative', TypeInfo(G800Interface.JGediNative));
  TRegTypes.RegisterType('G800Interface.JIINFO', TypeInfo(G800Interface.JIINFO));
  TRegTypes.RegisterType('G800Interface.JINFO', TypeInfo(G800Interface.JINFO));
  TRegTypes.RegisterType('G800Interface.JIKBD', TypeInfo(G800Interface.JIKBD));
  TRegTypes.RegisterType('G800Interface.JKBD', TypeInfo(G800Interface.JKBD));
  TRegTypes.RegisterType('G800Interface.JKBDData', TypeInfo(G800Interface.JKBDData));
  TRegTypes.RegisterType('G800Interface.JIKMS', TypeInfo(G800Interface.JIKMS));
  TRegTypes.RegisterType('G800Interface.JKMS', TypeInfo(G800Interface.JKMS));
  TRegTypes.RegisterType('G800Interface.JKMS_St_SaveKey', TypeInfo(G800Interface.JKMS_St_SaveKey));
  TRegTypes.RegisterType('G800Interface.JILED', TypeInfo(G800Interface.JILED));
  TRegTypes.RegisterType('G800Interface.JLED', TypeInfo(G800Interface.JLED));
  TRegTypes.RegisterType('G800Interface.JIMSR', TypeInfo(G800Interface.JIMSR));
  TRegTypes.RegisterType('G800Interface.JMSR', TypeInfo(G800Interface.JMSR));
  TRegTypes.RegisterType('G800Interface.JIPM', TypeInfo(G800Interface.JIPM));
  TRegTypes.RegisterType('G800Interface.JPM', TypeInfo(G800Interface.JPM));
  TRegTypes.RegisterType('G800Interface.JIPRNTR', TypeInfo(G800Interface.JIPRNTR));
  TRegTypes.RegisterType('G800Interface.JPRNTR', TypeInfo(G800Interface.JPRNTR));
  TRegTypes.RegisterType('G800Interface.JISMART', TypeInfo(G800Interface.JISMART));
  TRegTypes.RegisterType('G800Interface.JSMART', TypeInfo(G800Interface.JSMART));
  TRegTypes.RegisterType('G800Interface.JISYS', TypeInfo(G800Interface.JISYS));
  TRegTypes.RegisterType('G800Interface.JSYS', TypeInfo(G800Interface.JSYS));
  TRegTypes.RegisterType('G800Interface.JGEDI_CL_e_ISO_Level', TypeInfo(G800Interface.JGEDI_CL_e_ISO_Level));
  TRegTypes.RegisterType('G800Interface.JGEDI_CL_e_ISO_Type', TypeInfo(G800Interface.JGEDI_CL_e_ISO_Type));
  TRegTypes.RegisterType('G800Interface.JGEDI_CL_e_MF_KeyType', TypeInfo(G800Interface.JGEDI_CL_e_MF_KeyType));
  TRegTypes.RegisterType('G800Interface.JGEDI_CL_e_MF_Type', TypeInfo(G800Interface.JGEDI_CL_e_MF_Type));
  TRegTypes.RegisterType('G800Interface.JGEDI_CRYPT_e_Op', TypeInfo(G800Interface.JGEDI_CRYPT_e_Op));
  TRegTypes.RegisterType('G800Interface.JGEDI_FS_e_Storage', TypeInfo(G800Interface.JGEDI_FS_e_Storage));
  TRegTypes.RegisterType('G800Interface.JGEDI_INFO_e_ControlNumber', TypeInfo(G800Interface.JGEDI_INFO_e_ControlNumber));
  TRegTypes.RegisterType('G800Interface.JGEDI_INFO_e_Test', TypeInfo(G800Interface.JGEDI_INFO_e_Test));
  TRegTypes.RegisterType('G800Interface.JGEDI_KBD_e_Key', TypeInfo(G800Interface.JGEDI_KBD_e_Key));
  TRegTypes.RegisterType('G800Interface.JGEDI_KBD_e_PowerKeyMode', TypeInfo(G800Interface.JGEDI_KBD_e_PowerKeyMode));
  TRegTypes.RegisterType('G800Interface.JGEDI_KMS_e_BLOCKTYPE', TypeInfo(G800Interface.JGEDI_KMS_e_BLOCKTYPE));
  TRegTypes.RegisterType('G800Interface.JGEDI_KMS_e_EncMode', TypeInfo(G800Interface.JGEDI_KMS_e_EncMode));
  TRegTypes.RegisterType('G800Interface.JGEDI_KMS_e_KEYPURPOSE', TypeInfo(G800Interface.JGEDI_KMS_e_KEYPURPOSE));
  TRegTypes.RegisterType('G800Interface.JGEDI_KMS_e_KEYTYPE', TypeInfo(G800Interface.JGEDI_KMS_e_KEYTYPE));
  TRegTypes.RegisterType('G800Interface.JGEDI_KMS_e_KEYTYPE_LENGTH', TypeInfo(G800Interface.JGEDI_KMS_e_KEYTYPE_LENGTH));
  TRegTypes.RegisterType('G800Interface.JGEDI_KMS_e_OP', TypeInfo(G800Interface.JGEDI_KMS_e_OP));
  TRegTypes.RegisterType('G800Interface.JGEDI_LED_e_Id', TypeInfo(G800Interface.JGEDI_LED_e_Id));
  TRegTypes.RegisterType('G800Interface.JGEDI_MSR_e_Status', TypeInfo(G800Interface.JGEDI_MSR_e_Status));
  TRegTypes.RegisterType('G800Interface.JGEDI_PRNTR_e_Alignment', TypeInfo(G800Interface.JGEDI_PRNTR_e_Alignment));
  TRegTypes.RegisterType('G800Interface.JGEDI_PRNTR_e_BarCodeType', TypeInfo(G800Interface.JGEDI_PRNTR_e_BarCodeType));
  TRegTypes.RegisterType('G800Interface.JGEDI_PRNTR_e_Status', TypeInfo(G800Interface.JGEDI_PRNTR_e_Status));
  TRegTypes.RegisterType('G800Interface.JGEDI_SMART_e_MemoryCardType', TypeInfo(G800Interface.JGEDI_SMART_e_MemoryCardType));
  TRegTypes.RegisterType('G800Interface.JGEDI_SMART_e_Slot', TypeInfo(G800Interface.JGEDI_SMART_e_Slot));
  TRegTypes.RegisterType('G800Interface.JGEDI_SMART_e_Status', TypeInfo(G800Interface.JGEDI_SMART_e_Status));
  TRegTypes.RegisterType('G800Interface.JGEDI_SMART_e_Type', TypeInfo(G800Interface.JGEDI_SMART_e_Type));
  TRegTypes.RegisterType('G800Interface.JGEDI_SMART_e_Voltage', TypeInfo(G800Interface.JGEDI_SMART_e_Voltage));
  TRegTypes.RegisterType('G800Interface.JGEDI_SYS_e_SecuritySetup', TypeInfo(G800Interface.JGEDI_SYS_e_SecuritySetup));
  TRegTypes.RegisterType('G800Interface.JGEDI_e_Ret', TypeInfo(G800Interface.JGEDI_e_Ret));
  TRegTypes.RegisterType('G800Interface.JGediException', TypeInfo(G800Interface.JGediException));
  TRegTypes.RegisterType('G800Interface.Jimpl_Cl', TypeInfo(G800Interface.Jimpl_Cl));
  TRegTypes.RegisterType('G800Interface.Jimpl_Clock', TypeInfo(G800Interface.Jimpl_Clock));
  TRegTypes.RegisterType('G800Interface.Jimpl_Gedi', TypeInfo(G800Interface.Jimpl_Gedi));
  TRegTypes.RegisterType('G800Interface.Jimpl_Info', TypeInfo(G800Interface.Jimpl_Info));
  TRegTypes.RegisterType('G800Interface.Jimpl_Kbd', TypeInfo(G800Interface.Jimpl_Kbd));
  TRegTypes.RegisterType('G800Interface.Jimpl_Led', TypeInfo(G800Interface.Jimpl_Led));
  TRegTypes.RegisterType('G800Interface.JLed_1', TypeInfo(G800Interface.JLed_1));
  TRegTypes.RegisterType('G800Interface.Jimpl_Pm', TypeInfo(G800Interface.Jimpl_Pm));
  TRegTypes.RegisterType('G800Interface.JPrinterList', TypeInfo(G800Interface.JPrinterList));
  TRegTypes.RegisterType('G800Interface.JPrinterManager', TypeInfo(G800Interface.JPrinterManager));
  TRegTypes.RegisterType('G800Interface.JPrinterManager_1', TypeInfo(G800Interface.JPrinterManager_1));
  TRegTypes.RegisterType('G800Interface.JIPrinterCallback_Stub', TypeInfo(G800Interface.JIPrinterCallback_Stub));
  TRegTypes.RegisterType('G800Interface.JPrinterManager_2', TypeInfo(G800Interface.JPrinterManager_2));
  TRegTypes.RegisterType('G800Interface.JPrinterManager_PrinterManagerListener', TypeInfo(G800Interface.JPrinterManager_PrinterManagerListener));
  TRegTypes.RegisterType('G800Interface.Jimpl_Prntr', TypeInfo(G800Interface.Jimpl_Prntr));
  TRegTypes.RegisterType('G800Interface.JPrntr_1', TypeInfo(G800Interface.JPrntr_1));
  TRegTypes.RegisterType('G800Interface.JPrntr_2', TypeInfo(G800Interface.JPrntr_2));
  TRegTypes.RegisterType('G800Interface.JThreadPoolManager', TypeInfo(G800Interface.JThreadPoolManager));
  TRegTypes.RegisterType('G800Interface.JIAUDIO', TypeInfo(G800Interface.JIAUDIO));
  TRegTypes.RegisterType('G800Interface.JIEnums', TypeInfo(G800Interface.JIEnums));
  TRegTypes.RegisterType('G800Interface.JGEDI_AUTH_st_Data', TypeInfo(G800Interface.JGEDI_AUTH_st_Data));
  TRegTypes.RegisterType('G800Interface.JGEDI_CLOCK_st_RTC', TypeInfo(G800Interface.JGEDI_CLOCK_st_RTC));
  TRegTypes.RegisterType('G800Interface.JGEDI_CL_st_ISO_PollingInfo', TypeInfo(G800Interface.JGEDI_CL_st_ISO_PollingInfo));
  TRegTypes.RegisterType('G800Interface.JGEDI_CL_st_MF_ActivateInfo', TypeInfo(G800Interface.JGEDI_CL_st_MF_ActivateInfo));
  TRegTypes.RegisterType('G800Interface.JGEDI_CL_st_MF_Key', TypeInfo(G800Interface.JGEDI_CL_st_MF_Key));
  TRegTypes.RegisterType('G800Interface.JGEDI_CL_st_ResetEMVInfo', TypeInfo(G800Interface.JGEDI_CL_st_ResetEMVInfo));
  TRegTypes.RegisterType('G800Interface.JGEDI_CRYPT_st_RSAKeyGen', TypeInfo(G800Interface.JGEDI_CRYPT_st_RSAKeyGen));
  TRegTypes.RegisterType('G800Interface.JGEDI_KBD_st_Info', TypeInfo(G800Interface.JGEDI_KBD_st_Info));
  TRegTypes.RegisterType('G800Interface.JGEDI_KMS_st_CapturePINBlockInfo', TypeInfo(G800Interface.JGEDI_KMS_st_CapturePINBlockInfo));
  TRegTypes.RegisterType('G800Interface.JGEDI_KMS_st_Control', TypeInfo(G800Interface.JGEDI_KMS_st_Control));
  TRegTypes.RegisterType('G800Interface.JGEDI_KMS_st_Data', TypeInfo(G800Interface.JGEDI_KMS_st_Data));
  TRegTypes.RegisterType('G800Interface.JGEDI_KMS_st_KB', TypeInfo(G800Interface.JGEDI_KMS_st_KB));
  TRegTypes.RegisterType('G800Interface.JGEDI_KMS_st_PINBlock', TypeInfo(G800Interface.JGEDI_KMS_st_PINBlock));
  TRegTypes.RegisterType('G800Interface.JGEDI_KMS_st_SaveKey', TypeInfo(G800Interface.JGEDI_KMS_st_SaveKey));
  TRegTypes.RegisterType('G800Interface.JGEDI_MSR_st_LastErrors', TypeInfo(G800Interface.JGEDI_MSR_st_LastErrors));
  TRegTypes.RegisterType('G800Interface.JGEDI_MSR_st_Tracks', TypeInfo(G800Interface.JGEDI_MSR_st_Tracks));
  TRegTypes.RegisterType('G800Interface.JGEDI_PRNTR_st_BarCodeConfig', TypeInfo(G800Interface.JGEDI_PRNTR_st_BarCodeConfig));
  TRegTypes.RegisterType('G800Interface.JGEDI_PRNTR_st_PictureConfig', TypeInfo(G800Interface.JGEDI_PRNTR_st_PictureConfig));
  TRegTypes.RegisterType('G800Interface.JGEDI_PRNTR_st_StringConfig', TypeInfo(G800Interface.JGEDI_PRNTR_st_StringConfig));
  TRegTypes.RegisterType('G800Interface.JGEDI_SMART_st_ResetInfo', TypeInfo(G800Interface.JGEDI_SMART_st_ResetInfo));
  TRegTypes.RegisterType('G800Interface.JIPrinterCallback', TypeInfo(G800Interface.JIPrinterCallback));
  TRegTypes.RegisterType('G800Interface.JIPrinterCallback_Stub_Proxy', TypeInfo(G800Interface.JIPrinterCallback_Stub_Proxy));
  TRegTypes.RegisterType('G800Interface.JIPrinterService', TypeInfo(G800Interface.JIPrinterService));
  TRegTypes.RegisterType('G800Interface.JIPrinterService_Stub', TypeInfo(G800Interface.JIPrinterService_Stub));
  TRegTypes.RegisterType('G800Interface.JIPrinterService_Stub_Proxy', TypeInfo(G800Interface.JIPrinterService_Stub_Proxy));
end;

initialization
  RegisterTypes;
end.

