
unit G700Interface;

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
  Jgertec_Logger = interface;//br.com.gertec.Logger
  Jgertec_a_a = interface;//br.com.gertec.a.a
  Jgertec_a_a_a = interface;//br.com.gertec.a.a$a
  Ja_a_a_a = interface;//br.com.gertec.a.a$a$a
  JIGEDI = interface;//br.com.gertec.gedi.interfaces.IGEDI
  JGEDI = interface;//br.com.gertec.gedi.GEDI
  JGEDI_1 = interface;//br.com.gertec.gedi.GEDI$1
  JGEDI_KMS_st_Control_Callbacks = interface;//br.com.gertec.gedi.structs.GEDI_KMS_st_Control$Callbacks
  JGediNative = interface;//br.com.gertec.gedi.GediNative
  JICL = interface;//br.com.gertec.gedi.interfaces.ICL
  Jgedi_a = interface;//br.com.gertec.gedi.a
  JIAUDIO = interface;//br.com.gertec.gedi.interfaces.IAUDIO
  Jgedi_a_a = interface;//br.com.gertec.gedi.a.a
  Jgedi_a_b = interface;//br.com.gertec.gedi.a.b
  JICLOCK = interface;//br.com.gertec.gedi.interfaces.ICLOCK
  Jgedi_b = interface;//br.com.gertec.gedi.b
  Jgedi_a_c = interface;//br.com.gertec.gedi.a.c
  JICRYPT = interface;//br.com.gertec.gedi.interfaces.ICRYPT
  Jgedi_c = interface;//br.com.gertec.gedi.c
  Jgedi_a_d = interface;//br.com.gertec.gedi.a.d
  Jgedi_a_e = interface;//br.com.gertec.gedi.a.e
  JIINFO = interface;//br.com.gertec.gedi.interfaces.IINFO
  Jgedi_d = interface;//br.com.gertec.gedi.d
  Ja_f = interface;//br.com.gertec.gedi.a.f
  Jf_1 = interface;//br.com.gertec.gedi.a.f$1
  JIKBD = interface;//br.com.gertec.gedi.interfaces.IKBD
  Jgedi_e = interface;//br.com.gertec.gedi.e
  Ja_g = interface;//br.com.gertec.gedi.a.g
  Jg_1 = interface;//br.com.gertec.gedi.a.g$1
  Jg_10 = interface;//br.com.gertec.gedi.a.g$10
  Jg_11 = interface;//br.com.gertec.gedi.a.g$11
  Jg_12 = interface;//br.com.gertec.gedi.a.g$12
  Jg_13 = interface;//br.com.gertec.gedi.a.g$13
  Jg_2 = interface;//br.com.gertec.gedi.a.g$2
  Jg_3 = interface;//br.com.gertec.gedi.a.g$3
  Jg_4 = interface;//br.com.gertec.gedi.a.g$4
  Jg_5 = interface;//br.com.gertec.gedi.a.g$5
  Jg_6 = interface;//br.com.gertec.gedi.a.g$6
  Jg_7 = interface;//br.com.gertec.gedi.a.g$7
  Jg_8 = interface;//br.com.gertec.gedi.a.g$8
  Jg_9 = interface;//br.com.gertec.gedi.a.g$9
  JIKMS = interface;//br.com.gertec.gedi.interfaces.IKMS
  Jgedi_g = interface;//br.com.gertec.gedi.g
  Ja_h = interface;//br.com.gertec.gedi.a.h
  Jc_a = interface;//wangpos.sdk4.a.c$a
  Jh_1 = interface;//br.com.gertec.gedi.a.h$1
  Jh_2 = interface;//br.com.gertec.gedi.a.h$2
  JIMSR = interface;//br.com.gertec.gedi.interfaces.IMSR
  Jgedi_i = interface;//br.com.gertec.gedi.i
  Ja_i = interface;//br.com.gertec.gedi.a.i
  Ji_1 = interface;//br.com.gertec.gedi.a.i$1
  JIPM = interface;//br.com.gertec.gedi.interfaces.IPM
  Jgedi_j = interface;//br.com.gertec.gedi.j
  Ja_j = interface;//br.com.gertec.gedi.a.j
  JIPRNTR = interface;//br.com.gertec.gedi.interfaces.IPRNTR
  Jgedi_k = interface;//br.com.gertec.gedi.k
  Ja_k = interface;//br.com.gertec.gedi.a.k
  Jk_1 = interface;//br.com.gertec.gedi.a.k$1
  JISMART = interface;//br.com.gertec.gedi.interfaces.ISMART
  Jgedi_l = interface;//br.com.gertec.gedi.l
  Ja_l = interface;//br.com.gertec.gedi.a.l
  Jl_1 = interface;//br.com.gertec.gedi.a.l$1
  Jl_2 = interface;//br.com.gertec.gedi.a.l$2
  Jl_3 = interface;//br.com.gertec.gedi.a.l$3
  JISYS = interface;//br.com.gertec.gedi.interfaces.ISYS
  Jgedi_m = interface;//br.com.gertec.gedi.m
  Ja_m = interface;//br.com.gertec.gedi.a.m
  Ja_n = interface;//br.com.gertec.gedi.a.n
  Ja_o = interface;//br.com.gertec.gedi.a.o
  Jo_1 = interface;//br.com.gertec.gedi.a.o$1
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
  Jgedi_f = interface;//br.com.gertec.gedi.f
  JILED = interface;//br.com.gertec.gedi.interfaces.ILED
  Jgedi_h = interface;//br.com.gertec.gedi.h
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
  Jsdk4_a_a = interface;//wangpos.sdk4.a.a
  Jsdk4_a_a_a = interface;//wangpos.sdk4.a.a$a
  Ja_a_a = interface;//wangpos.sdk4.a.a$a$a
  Ja_b = interface;//wangpos.sdk4.a.b
  Jb_a = interface;//wangpos.sdk4.a.b$a
  Jb_a_a = interface;//wangpos.sdk4.a.b$a$a
  Ja_c = interface;//wangpos.sdk4.a.c
  Jc_a_a = interface;//wangpos.sdk4.a.c$a$a
  Ja_d = interface;//wangpos.sdk4.a.d
  Jd_a = interface;//wangpos.sdk4.a.d$a
  Jd_a_a = interface;//wangpos.sdk4.a.d$a$a
  Ja_e = interface;//wangpos.sdk4.a.e
  Je_a = interface;//wangpos.sdk4.a.e$a
  Je_a_a = interface;//wangpos.sdk4.a.e$a$a
  Jsdk4_b_a = interface;//wangpos.sdk4.b.a
  Jsdk4_b_a_a = interface;//wangpos.sdk4.b.a$a
  Jb_a_a_a = interface;//wangpos.sdk4.b.a$a$a
  Jb_b = interface;//wangpos.sdk4.b.b
  Jb_b_a = interface;//wangpos.sdk4.b.b$a
  Jb_b_a_a = interface;//wangpos.sdk4.b.b$a$a
  Jsdk4_c_a = interface;//wangpos.sdk4.c.a
  Jsdk4_c_a_a = interface;//wangpos.sdk4.c.a$a
  Jc_a_b = interface;//wangpos.sdk4.c.a$b
  Jc_b = interface;//wangpos.sdk4.c.b
  Jlibbasebinder_a = interface;//wangpos.sdk4.libbasebinder.a
  JBankCard = interface;//wangpos.sdk4.libbasebinder.BankCard
  JCore = interface;//wangpos.sdk4.libbasebinder.Core
  Jlibbasebinder_Printer = interface;//wangpos.sdk4.libbasebinder.Printer
  JPrinter_Align = interface;//wangpos.sdk4.libbasebinder.Printer$Align
  JPrinter_BarcodeType = interface;//wangpos.sdk4.libbasebinder.Printer$BarcodeType
  JPrinter_BarcodeWidth = interface;//wangpos.sdk4.libbasebinder.Printer$BarcodeWidth
  JPrinter_Font = interface;//wangpos.sdk4.libbasebinder.Printer$Font
  JRspCode = interface;//wangpos.sdk4.libbasebinder.RspCode
  Ja_1 = interface;//wangpos.sdk4.libbasebinder.a$1
  Ja_2 = interface;//wangpos.sdk4.libbasebinder.a$2
  Jlibbasebinder_a_a = interface;//wangpos.sdk4.libbasebinder.a.a
  Jlibbasebinder_b = interface;//wangpos.sdk4.libbasebinder.b

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

  Jgertec_LoggerClass = interface(JObjectClass)
    ['{41EAB32A-D6E5-4757-B79C-7B1C4AA246C9}']
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
    ['{69DE5D3F-24F8-413A-8148-5D8A9DCDE5C8}']
  end;
  TJgertec_Logger = class(TJavaGenericImport<Jgertec_LoggerClass, Jgertec_Logger>) end;

  Jgertec_a_aClass = interface(JIInterfaceClass)
    ['{28446D5A-2302-4124-875D-9EFB93A1F0BF}']
    {class} function a(P1: Integer; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Byte>; P6: Boolean; P7: Integer; P8: TJavaArray<Byte>; P9: JString; P10: Integer): Integer; cdecl;
  end;

  [JavaSignature('br/com/gertec/a/a')]
  Jgertec_a_a = interface(JIInterface)
    ['{AE94A037-C8AA-4BE4-BF47-31AD9C8DCEDF}']
  end;
  TJgertec_a_a = class(TJavaGenericImport<Jgertec_a_aClass, Jgertec_a_a>) end;

  Jgertec_a_a_aClass = interface(JBinderClass)
    ['{B9D1BA84-2DDA-4249-B9C0-0A43D6827B0A}']
    {class} function a(P1: JIBinder): Jgertec_a_a; cdecl;
    {class} function onTransact(P1: Integer; P2: JParcel; P3: JParcel; P4: Integer): Boolean; cdecl;
  end;

  [JavaSignature('br/com/gertec/a/a$a')]
  Jgertec_a_a_a = interface(JBinder)
    ['{FB5532D5-242F-459D-8DE5-EB16C53B8DED}']
  end;
  TJgertec_a_a_a = class(TJavaGenericImport<Jgertec_a_a_aClass, Jgertec_a_a_a>) end;

  Ja_a_a_aClass = interface(Jgertec_a_aClass)
    ['{D7E1EA9C-1C52-4AE9-B222-1959E3F1FEB8}']
    {class} function a(P1: Integer; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Byte>; P6: Boolean; P7: Integer; P8: TJavaArray<Byte>; P9: JString; P10: Integer): Integer; cdecl;
    {class} function asBinder: JIBinder; cdecl;
  end;

  [JavaSignature('br/com/gertec/a/a$a$a')]
  Ja_a_a_a = interface(Jgertec_a_a)
    ['{821DA95C-1B41-4229-9F53-A82B1D5D1632}']
  end;
  TJa_a_a_a = class(TJavaGenericImport<Ja_a_a_aClass, Ja_a_a_a>) end;

  JIGEDIClass = interface(IJavaClass)
    ['{D591F41A-9E7A-480B-AE0E-AFEEDD0B553E}']
    {class} procedure EnterEng(P1: JString); cdecl;
    {class} function VersionGet: JString; cdecl;
    {class} function getAUDIO: JIAUDIO; cdecl;
    {class} function getCL: JICL; cdecl;
    {class} function getCLOCK: JICLOCK; cdecl;
    {class} function getCRYPT: JICRYPT; cdecl;
    {class} function getINFO: JIINFO; cdecl;
    {class} function getKBD: JIKBD; cdecl;
    {class} function getKMS: JIKMS; cdecl;
    {class} function getLED: JILED; cdecl;
    {class} function getMSR: JIMSR; cdecl;
    {class} function getPM: JIPM; cdecl;
    {class} function getPRNTR: JIPRNTR; cdecl;
    {class} function getSMART: JISMART; cdecl;
    //Marcos (*)
    //{class} procedure initABC(P1: JContext); cdecl;overload;//Deprecated


  end;

  [JavaSignature('br/com/gertec/gedi/interfaces/IGEDI')]
  JIGEDI = interface(IJavaInstance)
    ['{E34313C1-CA1E-4639-A56C-A9C287BA0432}']
// Foi adicionado pelo Geovani
    {class} procedure EnterEng(P1: JString); cdecl;//Deprecated
    {class} function VersionGet: JString; cdecl;//Deprecated
    {class} function getAUDIO: JIAUDIO; cdecl;//Deprecated
    {class} function getCL: JICL; cdecl;//Deprecated
    {class} function getCLOCK: JICLOCK; cdecl;//Deprecated
    {class} function getCRYPT: JICRYPT; cdecl;//Deprecated
    {class} function getINFO: JIINFO; cdecl;//Deprecated
    {class} function getKBD: JIKBD; cdecl;//Deprecated
    {class} function getKMS: JIKMS; cdecl;//Deprecated
    {class} function getLED: JILED; cdecl;//Deprecated
    {class} function getMSR: JIMSR; cdecl;//Deprecated
    {class} function getPM: JIPM; cdecl;//Deprecated
    {class} function getPRNTR: JIPRNTR; cdecl;//Deprecated
    {class} function getSMART: JISMART; cdecl;//Deprecated
    // Foi adicionado pelo Geovani
  end;
  TJIGEDI = class(TJavaGenericImport<JIGEDIClass, JIGEDI>) end;

  JGEDIClass = interface(JIGEDIClass)
    ['{E78B2814-0CC7-43C8-A5B1-990366D3527C}']
    {class} function getInstance: JIGEDI; cdecl; overload;
    {class} function getInstance(P1: JContext): JIGEDI; cdecl; overload;
    //Marcos (*)
    //{class} procedure init(P1: JContext); cdecl;overload;//Deprecated
    {class} procedure init(P1: JContext); cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/GEDI')]
  JGEDI = interface(JIGEDI)
    ['{6101F422-DED5-4F06-9E01-70D9067E43C1}']
  end;
  TJGEDI = class(TJavaGenericImport<JGEDIClass, JGEDI>) end;

  JGEDI_1Class = interface(JThreadClass)
    ['{F29ACBEE-523B-484B-B1F5-AB8ED47B711E}']
    {class} function init(P1: JContext): JGEDI_1; cdecl;//Deprecated
    {class} procedure run; cdecl;//Deprecated
  end;

  [JavaSignature('br/com/gertec/gedi/GEDI$1')]
  JGEDI_1 = interface(JThread)
    ['{19C4D031-A801-42DA-893D-BF2C4D39AA6F}']
  end;
  TJGEDI_1 = class(TJavaGenericImport<JGEDI_1Class, JGEDI_1>) end;

  JGEDI_KMS_st_Control_CallbacksClass = interface(IJavaClass)
    ['{891CC75B-35B4-4836-9F15-6B781AD9B53C}']
    {class} procedure onKeyPress(P1: JGEDI_KBD_e_Key); cdecl;
    {class} function testCancel: Boolean; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_KMS_st_Control$Callbacks')]
  JGEDI_KMS_st_Control_Callbacks = interface(IJavaInstance)
    ['{D1E714BF-EB78-44F8-B392-62ED51DE380D}']
  end;
  TJGEDI_KMS_st_Control_Callbacks = class(TJavaGenericImport<JGEDI_KMS_st_Control_CallbacksClass, JGEDI_KMS_st_Control_Callbacks>) end;

  JGediNativeClass = interface(JGEDI_KMS_st_Control_CallbacksClass)
    ['{75BF6EBD-C14A-442D-A6D3-8E8F540D6797}']
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
    {class} function getInstance(P1: Jgedi_a_e): JGediNative; cdecl;//Deprecated
    {class} function init(P1: Jgedi_a_e): JGediNative; cdecl; overload;//Deprecated
    {class} procedure onKeyPress(P1: Integer); cdecl; overload;//Deprecated
    {class} procedure onKeyPress(P1: JGEDI_KBD_e_Key); cdecl; overload;//Deprecated
    {class} procedure setPrivateDir(P1: JString); cdecl;//Deprecated
    {class} function testCancel: Boolean; cdecl;//Deprecated
  end;

  [JavaSignature('br/com/gertec/gedi/GediNative')]
  JGediNative = interface(JGEDI_KMS_st_Control_Callbacks)
    ['{67551F1D-FE35-4765-BFF1-91160B4839C7}']
  end;
  TJGediNative = class(TJavaGenericImport<JGediNativeClass, JGediNative>) end;

  JICLClass = interface(IJavaClass)
    ['{FD97102C-FAC7-43D0-83AE-0FF28AB56609}']
    {class} function ISO_Polling(P1: Integer): JGEDI_CL_st_ISO_PollingInfo; cdecl;
    {class} procedure MF_Authentication(P1: Integer; P2: JGEDI_CL_st_MF_Key; P3: TJavaArray<Byte>); cdecl;
    {class} function MF_BlockRead(P1: Integer): TJavaArray<Byte>; cdecl;
    {class} procedure MF_BlockWrite(P1: Integer; P2: TJavaArray<Byte>); cdecl;
    {class} function MF_SignatureGet(P1: Integer): TJavaArray<Byte>; cdecl;
    {class} procedure PowerOff; cdecl;
    {class} procedure PowerOn; cdecl;
    {class} function ResetEMV: JGEDI_CL_st_ResetEMVInfo; cdecl;
    {class} function SendAPDU(P1: TJavaArray<Byte>): TJavaArray<Byte>; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/interfaces/ICL')]
  JICL = interface(IJavaInstance)
    ['{E33E0F90-0937-4095-A163-F5EDF50EC8A0}']
    // Foi adicionado pelo Geovani
    {class} function ISO_Polling(P1: Integer): JGEDI_CL_st_ISO_PollingInfo; cdecl;//Deprecated
    {class} procedure MF_Authentication(P1: Integer; P2: JGEDI_CL_st_MF_Key; P3: TJavaArray<Byte>); cdecl;//Deprecated
    {class} function MF_BlockRead(P1: Integer): TJavaArray<Byte>; cdecl;//Deprecated
    {class} procedure MF_BlockWrite(P1: Integer; P2: TJavaArray<Byte>); cdecl;//Deprecated
    {class} function MF_SignatureGet(P1: Integer): TJavaArray<Byte>; cdecl;//Deprecated
    {class} procedure PowerOff; cdecl;//Deprecated
    {class} procedure PowerOn; cdecl;//Deprecated
    {class} function ResetEMV: JGEDI_CL_st_ResetEMVInfo; cdecl;//Deprecated
    {class} function SendAPDU(P1: TJavaArray<Byte>): TJavaArray<Byte>; cdecl;//Deprecated
    // Foi adicionado pelo Geovani
  end;
  TJICL = class(TJavaGenericImport<JICLClass, JICL>) end;

  Jgedi_aClass = interface(JICLClass)
    ['{1A7AAFEB-4CF0-4981-BD7D-BFBCAD2856EC}']
    {class} function ISO_Polling(P1: Integer): JGEDI_CL_st_ISO_PollingInfo; cdecl;
    {class} procedure PowerOff; cdecl;
    {class} procedure PowerOn; cdecl;
    {class} function a(P1: TJavaArray<Byte>; P2: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: TJavaArray<Byte>; P2: Integer; P3: TJavaArray<Byte>; P4: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function init: Jgedi_a; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/a')]
  Jgedi_a = interface(JICL)
    ['{8C61892A-DE66-4F26-9E66-C5134016281E}']
  end;
  TJgedi_a = class(TJavaGenericImport<Jgedi_aClass, Jgedi_a>) end;

  JIAUDIOClass = interface(IJavaClass)
    ['{53343A29-B73B-4D2F-883A-2133B5A234F5}']
    {class} procedure Beep; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/interfaces/IAUDIO')]
  JIAUDIO = interface(IJavaInstance)
    ['{749BC4FD-BC17-47E4-8792-C25C002EBFCD}']
  end;
  TJIAUDIO = class(TJavaGenericImport<JIAUDIOClass, JIAUDIO>) end;

  Jgedi_a_aClass = interface(JIAUDIOClass)
    ['{C13622A5-B1AF-48E5-A8F2-A2036F823C46}']
    {class} procedure Beep; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/a/a')]
  Jgedi_a_a = interface(JIAUDIO)
    ['{C3CA4797-1E48-4E23-8F3C-CD31F6617D25}']
  end;
  TJgedi_a_a = class(TJavaGenericImport<Jgedi_a_aClass, Jgedi_a_a>) end;

  Jgedi_a_bClass = interface(Jgedi_aClass)
    ['{2A0D23F5-961E-4867-A1ED-9DF657B1E9FB}']
    {class} function ISO_Polling(P1: Integer): JGEDI_CL_st_ISO_PollingInfo; cdecl;//Deprecated
    {class} procedure MF_Authentication(P1: Integer; P2: JGEDI_CL_st_MF_Key; P3: TJavaArray<Byte>); cdecl;//Deprecated
    {class} function MF_BlockRead(P1: Integer): TJavaArray<Byte>; cdecl;//Deprecated
    {class} procedure MF_BlockWrite(P1: Integer; P2: TJavaArray<Byte>); cdecl;//Deprecated
    {class} function MF_SignatureGet(P1: Integer): TJavaArray<Byte>; cdecl;//Deprecated
    {class} procedure PowerOff; cdecl;//Deprecated
    {class} procedure PowerOn; cdecl;//Deprecated
    {class} function ResetEMV: JGEDI_CL_st_ResetEMVInfo; cdecl;//Deprecated
    {class} function SendAPDU(P1: TJavaArray<Byte>): TJavaArray<Byte>; cdecl;//Deprecated
    {class} function a(P1: TJavaArray<Byte>; P2: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function init(P1: Ja_o): Jgedi_a_b; cdecl;//Deprecated
  end;

  [JavaSignature('br/com/gertec/gedi/a/b')]
  Jgedi_a_b = interface(Jgedi_a)
    ['{0285A063-C21B-4F6B-8618-6B48F25A189F}']
  end;
  TJgedi_a_b = class(TJavaGenericImport<Jgedi_a_bClass, Jgedi_a_b>) end;

  JICLOCKClass = interface(IJavaClass)
    ['{71DBE5AF-1201-4E20-8BBF-CC33972A2B96}']
    {class} function RTCFGet: JGEDI_CLOCK_st_RTC; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/interfaces/ICLOCK')]
  JICLOCK = interface(IJavaInstance)
    ['{2A039284-BF66-427D-8911-254E13CD6FA6}']
  end;
  TJICLOCK = class(TJavaGenericImport<JICLOCKClass, JICLOCK>) end;

  Jgedi_bClass = interface(JICLOCKClass)
    ['{48BAAF9B-0340-4CD2-B45E-0E2979487473}']
    {class} function RTCFGet: JGEDI_CLOCK_st_RTC; cdecl;//Deprecated
    {class} function a: JGEDI_CLOCK_st_RTC; cdecl; overload;//Deprecated
    {class} procedure a(P1: JGEDI_CLOCK_st_RTC); cdecl; overload;//Deprecated
    {class} function a(P1: Byte; P2: Byte; P3: Byte; P4: Byte; P5: Byte; P6: Byte; P7: Byte): Integer; cdecl; overload;//Deprecated
    {class} function a(P1: TJavaArray<Byte>; P2: TJavaArray<Byte>; P3: TJavaArray<Byte>; P4: TJavaArray<Byte>; P5: TJavaArray<Byte>; P6: TJavaArray<Byte>; P7: TJavaArray<Byte>): Integer; cdecl; overload;//Deprecated
    {class} function init: Jgedi_b; cdecl; overload;//Deprecated
    {class} function init(P1: JContext): Jgedi_b; cdecl; overload;//Deprecated
  end;

  [JavaSignature('br/com/gertec/gedi/b')]
  Jgedi_b = interface(JICLOCK)
    ['{BDDCAE7D-D52C-440B-9E69-4EC16B7A7861}']
  end;
  TJgedi_b = class(TJavaGenericImport<Jgedi_bClass, Jgedi_b>) end;

  Jgedi_a_cClass = interface(Jgedi_bClass)
    ['{DAB29634-2522-4794-ADD3-10BDC235F5F1}']
    {class} function RTCFGet: JGEDI_CLOCK_st_RTC; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/a/c')]
  Jgedi_a_c = interface(Jgedi_b)
    ['{CA0E02E8-0EED-4EAD-911F-D275FD4D3C33}']
  end;
  TJgedi_a_c = class(TJavaGenericImport<Jgedi_a_cClass, Jgedi_a_c>) end;

  JICRYPTClass = interface(IJavaClass)
    ['{52D01E1D-0B6C-41CE-B898-BE763E3C37AE}']
    {class} function RNG(P1: Integer): TJavaArray<Byte>; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/interfaces/ICRYPT')]
  JICRYPT = interface(IJavaInstance)
    ['{4E5BD0A3-AD22-447A-9436-DD9DA748DAC3}']
  end;
  TJICRYPT = class(TJavaGenericImport<JICRYPTClass, JICRYPT>) end;

  Jgedi_cClass = interface(JICRYPTClass)
    ['{72F38E36-1C65-4C7D-BB23-A098EB7840D6}']
    {class} function RNG(P1: Integer): TJavaArray<Byte>; cdecl;
    {class} function a(P1: TJavaArray<Byte>): TJavaArray<Byte>; cdecl; overload;
    {class} function a(P1: TJavaArray<Byte>; P2: TJavaArray<Byte>): Integer; cdecl; overload;
    {class} function a(P1: JGEDI_CRYPT_e_Op; P2: TJavaArray<Byte>; P3: TJavaArray<Byte>): TJavaArray<Byte>; cdecl; overload;
    {class} function a(P1: TJavaArray<Byte>; P2: TJavaArray<Byte>; P3: TJavaArray<Byte>): TJavaArray<Byte>; cdecl; overload;
    {class} function a(P1: TJavaArray<Byte>; P2: TJavaArray<Byte>; P3: TJavaArray<Byte>; P4: TJavaArray<Byte>): Integer; cdecl; overload;
    {class} function a(P1: JGEDI_CRYPT_e_Op; P2: TJavaArray<Byte>; P3: Integer; P4: Integer; P5: TJavaArray<Byte>; P6: Integer; P7: Integer; P8: TJavaArray<Byte>): Integer; cdecl; overload;
    {class} function b(P1: TJavaArray<Byte>): Integer; cdecl; overload;
    {class} function b(P1: JGEDI_CRYPT_e_Op; P2: TJavaArray<Byte>; P3: TJavaArray<Byte>): TJavaArray<Byte>; cdecl; overload;
    {class} function init: Jgedi_c; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/c')]
  Jgedi_c = interface(JICRYPT)
    ['{B53E7A75-BDDE-4907-B85C-3FB06AF6DFC1}']
  end;
  TJgedi_c = class(TJavaGenericImport<Jgedi_cClass, Jgedi_c>) end;

  Jgedi_a_dClass = interface(Jgedi_cClass)
    ['{F3F06E73-BC45-4FED-B513-B280F9BF5BF0}']
    {class} function RNG(P1: Integer): TJavaArray<Byte>; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/a/d')]
  Jgedi_a_d = interface(Jgedi_c)
    ['{2EA4EB84-4F80-4219-86EE-A8033D9E2AEE}']
  end;
  TJgedi_a_d = class(TJavaGenericImport<Jgedi_a_dClass, Jgedi_a_d>) end;

  Jgedi_a_eClass = interface(JGEDIClass)
    ['{77ECD997-A10C-46C2-BA29-E3D98B93A934}']
    {class} procedure EnterEng(P1: JString); cdecl;
    {class} function VersionGet: JString; cdecl;
    {class} function b: Jgedi_d; cdecl;
    {class} function c: Jgedi_c; cdecl;
    {class} function d: Jgedi_e; cdecl;
    {class} function e: Jgedi_g; cdecl;
    {class} function f: Jgedi_l; cdecl;
    {class} function g: Jgedi_i; cdecl;
    {class} function getAUDIO: JIAUDIO; cdecl;
    {class} function getCL: JICL; cdecl;
    {class} function getCLOCK: JICLOCK; cdecl;
    {class} function getCRYPT: JICRYPT; cdecl;
    {class} function getINFO: JIINFO; cdecl;
    {class} function getKBD: JIKBD; cdecl;
    {class} function getKMS: JIKMS; cdecl;
    {class} function getLED: JILED; cdecl;
    {class} function getMSR: JIMSR; cdecl;
    {class} function getPM: JIPM; cdecl;
    {class} function getPRNTR: JIPRNTR; cdecl;
    {class} function getSMART: JISMART; cdecl;
    {class} function h: Jgedi_a; cdecl;
    {class} function i: Jgedi_b; cdecl;
    {class} function init(P1: JContext): Jgedi_a_e; cdecl;
    {class} function j: Jgedi_h; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/a/e')]
  Jgedi_a_e = interface(JGEDI)
    ['{B5C01C45-785E-4DA4-B845-7A93DDD10333}']
  end;
  TJgedi_a_e = class(TJavaGenericImport<Jgedi_a_eClass, Jgedi_a_e>) end;

  JIINFOClass = interface(IJavaClass)
    ['{FA86378A-59FA-453E-B402-A64F8A61EF48}']
    {class} function ControlNumberGet(P1: JGEDI_INFO_e_ControlNumber): JString; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/interfaces/IINFO')]
  JIINFO = interface(IJavaInstance)
    ['{54F61F01-E976-457E-B25B-4D0D4F7ADE38}']
  end;
  TJIINFO = class(TJavaGenericImport<JIINFOClass, JIINFO>) end;

  Jgedi_dClass = interface(JIINFOClass)
    ['{BCDB0B45-0E74-4404-A4A9-97CB3F3D024F}']
    {class} function ControlNumberGet(P1: JGEDI_INFO_e_ControlNumber): JString; cdecl;
    {class} function a: JString; cdecl; overload;
    {class} function a(P1: TJavaArray<Byte>): Integer; cdecl; overload;
    {class} procedure a(P1: JGEDI_INFO_e_ControlNumber; P2: JString); cdecl; overload;
    {class} function a(P1: Integer; P2: TJavaArray<Byte>): Integer; cdecl; overload;
    {class} function b(P1: Integer; P2: TJavaArray<Byte>): Integer; cdecl;
    {class} function init: Jgedi_d; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/d')]
  Jgedi_d = interface(JIINFO)
    ['{6642D483-9137-4B44-B34C-073EE174AD09}']
  end;
  TJgedi_d = class(TJavaGenericImport<Jgedi_dClass, Jgedi_d>) end;

  Ja_fClass = interface(Jgedi_dClass)
    ['{04F88748-3A5E-46F9-AA4E-52A395E37D78}']
    {class} function ControlNumberGet(P1: JGEDI_INFO_e_ControlNumber): JString; cdecl;
    {class} function a: JString; cdecl; overload;
    {class} procedure a(P1: JGEDI_INFO_e_ControlNumber; P2: JString); cdecl; overload;
  end;

  [JavaSignature('br/com/gertec/gedi/a/f')]
  Ja_f = interface(Jgedi_d)
    ['{4D743484-2199-48C8-8302-BA74FC8FA3B4}']
  end;
  TJa_f = class(TJavaGenericImport<Ja_fClass, Ja_f>) end;

  Jf_1Class = interface(JObjectClass)
    ['{C49355B6-B8CC-41AD-8301-068DD6DE4F67}']
  end;

  [JavaSignature('br/com/gertec/gedi/a/f$1')]
  Jf_1 = interface(JObject)
    ['{7B0C2AFA-3157-4C4B-8C0E-F3D3F2B16ED6}']
  end;
  TJf_1 = class(TJavaGenericImport<Jf_1Class, Jf_1>) end;

  JIKBDClass = interface(IJavaClass)
    ['{D886015C-2520-46A4-B5B9-9368A2B1B3FB}']
    {class} procedure &Set(P1: JGEDI_KBD_st_Info); cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/interfaces/IKBD')]
  JIKBD = interface(IJavaInstance)
    ['{8E36D7F0-993A-4CFA-8BDA-EC2383B25AE4}']
  end;
  TJIKBD = class(TJavaGenericImport<JIKBDClass, JIKBD>) end;

  Jgedi_eClass = interface(JIKBDClass)
    ['{AF8D882A-0944-48F2-8624-BCBD8840EE7E}']
    {class} function _Geta: Integer; cdecl;
    {class} procedure _Seta(Value: Integer); cdecl;
    {class} function _Getb: Integer; cdecl;
    {class} procedure _Setb(Value: Integer); cdecl;
    {class} function _Getc: Integer; cdecl;
    {class} procedure _Setc(Value: Integer); cdecl;
    {class} function _Getd: Integer; cdecl;
    {class} procedure _Setd(Value: Integer); cdecl;
    {class} function _Gete: Integer; cdecl;
    {class} procedure _Sete(Value: Integer); cdecl;
    {class} function _Getf: Integer; cdecl;
    {class} procedure _Setf(Value: Integer); cdecl;
    {class} function _Getg: Integer; cdecl;
    {class} procedure _Setg(Value: Integer); cdecl;
    {class} function _Geth: Integer; cdecl;
    {class} procedure _Seth(Value: Integer); cdecl;
    {class} function _Geti: Integer; cdecl;
    {class} procedure _Seti(Value: Integer); cdecl;
    {class} function _Getj: Integer; cdecl;
    {class} procedure _Setj(Value: Integer); cdecl;
    {class} function _Getk: Integer; cdecl;
    {class} procedure _Setk(Value: Integer); cdecl;
    {class} function _Getl: Integer; cdecl;
    {class} procedure _Setl(Value: Integer); cdecl;
    {class} function _Getm: Integer; cdecl;
    {class} procedure _Setm(Value: Integer); cdecl;
    {class} function _Getn: Integer; cdecl;
    {class} procedure _Setn(Value: Integer); cdecl;
    {class} function _Geto: Integer; cdecl;
    {class} procedure _Seto(Value: Integer); cdecl;
    {class} function _Getp: Integer; cdecl;
    {class} procedure _Setp(Value: Integer); cdecl;
    {class} function _Getq: Integer; cdecl;
    {class} procedure _Setq(Value: Integer); cdecl;
    {class} function _Getr: Integer; cdecl;
    {class} procedure _Setr(Value: Integer); cdecl;
    {class} function _Gets: Integer; cdecl;
    {class} procedure _Sets(Value: Integer); cdecl;
    {class} function _Gett: Integer; cdecl;
    {class} procedure _Sett(Value: Integer); cdecl;
    {class} function _Getu: Integer; cdecl;
    {class} procedure _Setu(Value: Integer); cdecl;
    {class} function _Getv: Integer; cdecl;
    {class} procedure _Setv(Value: Integer); cdecl;
    {class} function _Getw: Integer; cdecl;
    {class} procedure _Setw(Value: Integer); cdecl;
    {class} function _Getx: Integer; cdecl;
    {class} procedure _Setx(Value: Integer); cdecl;
    {class} function _Gety: Integer; cdecl;
    {class} procedure _Sety(Value: Integer); cdecl;
    {class} function _Getz: Integer; cdecl;
    {class} procedure _Setz(Value: Integer); cdecl;
    {class} function a(P1: Integer; P2: Boolean): JGEDI_KBD_e_Key; cdecl; overload;//Deprecated
    {class} function a(P1: TJavaArray<Integer>; P2: Integer; P3: Boolean): Integer; cdecl; overload;//Deprecated
    {class} function init: Jgedi_e; cdecl;//Deprecated
    {class} property a: Integer read _Geta write _Seta;
    {class} property b: Integer read _Getb write _Setb;
    {class} property c: Integer read _Getc write _Setc;
    {class} property d: Integer read _Getd write _Setd;
    {class} property e: Integer read _Gete write _Sete;
    {class} property f: Integer read _Getf write _Setf;
    {class} property g: Integer read _Getg write _Setg;
    {class} property h: Integer read _Geth write _Seth;
    {class} property i: Integer read _Geti write _Seti;
    {class} property j: Integer read _Getj write _Setj;
    {class} property k: Integer read _Getk write _Setk;
    {class} property l: Integer read _Getl write _Setl;
    {class} property m: Integer read _Getm write _Setm;
    {class} property n: Integer read _Getn write _Setn;
    {class} property o: Integer read _Geto write _Seto;
    {class} property p: Integer read _Getp write _Setp;
    {class} property q: Integer read _Getq write _Setq;
    {class} property r: Integer read _Getr write _Setr;
    {class} property s: Integer read _Gets write _Sets;
    {class} property t: Integer read _Gett write _Sett;
    {class} property u: Integer read _Getu write _Setu;
    {class} property v: Integer read _Getv write _Setv;
    {class} property w: Integer read _Getw write _Setw;
    {class} property x: Integer read _Getx write _Setx;
    {class} property y: Integer read _Gety write _Sety;
    {class} property z: Integer read _Getz write _Setz;
  end;

  [JavaSignature('br/com/gertec/gedi/e')]
  Jgedi_e = interface(JIKBD)
    ['{67573B78-6042-4FB3-833E-C58E25C6170B}']
  end;
  TJgedi_e = class(TJavaGenericImport<Jgedi_eClass, Jgedi_e>) end;

  Ja_gClass = interface(Jgedi_eClass)
    ['{2DD7F1CE-35EC-4C64-AA3C-A2559DE51451}']
    {class} procedure &Set(P1: JGEDI_KBD_st_Info); cdecl;
    {class} function a(P1: Integer; P2: Boolean): JGEDI_KBD_e_Key; cdecl; overload;
    {class} function b: Jgedi_f; cdecl;
    {class} function init(P1: Ja_o): Ja_g; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/a/g')]
  Ja_g = interface(Jgedi_e)
    ['{FE20DD29-F1DD-4043-A2C0-1911F1800F6B}']
  end;
  TJa_g = class(TJavaGenericImport<Ja_gClass, Ja_g>) end;

  Jg_1Class = interface(JView_OnClickListenerClass)
    ['{AD3FA4EB-3C39-4BFD-8FBB-AD577B173575}']
    {class} function init(P1: Ja_g): Jg_1; cdecl;//Deprecated
    {class} procedure onClick(P1: JView); cdecl;//Deprecated
  end;

  [JavaSignature('br/com/gertec/gedi/a/g$1')]
  Jg_1 = interface(JView_OnClickListener)
    ['{A1C5AEDB-0EAA-45D0-8DC3-288BAB225D21}']
  end;
  TJg_1 = class(TJavaGenericImport<Jg_1Class, Jg_1>) end;

  Jg_10Class = interface(JView_OnClickListenerClass)
    ['{A1B2DE9F-A714-4825-9ADD-A1CE636C6B2E}']
    {class} function init(P1: Ja_g): Jg_10; cdecl;//Deprecated
    {class} procedure onClick(P1: JView); cdecl;//Deprecated
  end;

  [JavaSignature('br/com/gertec/gedi/a/g$10')]
  Jg_10 = interface(JView_OnClickListener)
    ['{7EBE73FD-9051-476E-936E-823108210B29}']
  end;
  TJg_10 = class(TJavaGenericImport<Jg_10Class, Jg_10>) end;

  Jg_11Class = interface(JView_OnClickListenerClass)
    ['{A64AFCB0-612F-460A-9CFF-DAC2CAE1616A}']
    {class} function init(P1: Ja_g): Jg_11; cdecl;//Deprecated
    {class} procedure onClick(P1: JView); cdecl;//Deprecated
  end;

  [JavaSignature('br/com/gertec/gedi/a/g$11')]
  Jg_11 = interface(JView_OnClickListener)
    ['{9DFC6B5C-8F70-425B-93D8-FD4239A321D2}']
  end;
  TJg_11 = class(TJavaGenericImport<Jg_11Class, Jg_11>) end;

  Jg_12Class = interface(JView_OnClickListenerClass)
    ['{0730AB75-243C-426F-8811-A5EDB5849E90}']
    {class} function init(P1: Ja_g): Jg_12; cdecl;//Deprecated
    {class} procedure onClick(P1: JView); cdecl;//Deprecated
  end;

  [JavaSignature('br/com/gertec/gedi/a/g$12')]
  Jg_12 = interface(JView_OnClickListener)
    ['{DDD3C58C-E34C-4ABB-B4F7-930F5581315E}']
  end;
  TJg_12 = class(TJavaGenericImport<Jg_12Class, Jg_12>) end;

  Jg_13Class = interface(JView_OnClickListenerClass)
    ['{3391DA92-181D-40E4-BA66-DA2B1F9C1325}']
    {class} function init(P1: Ja_g): Jg_13; cdecl;//Deprecated
    {class} procedure onClick(P1: JView); cdecl;//Deprecated
  end;

  [JavaSignature('br/com/gertec/gedi/a/g$13')]
  Jg_13 = interface(JView_OnClickListener)
    ['{BA48D3DD-4BB6-4A9B-A765-A12C24F33E32}']
  end;
  TJg_13 = class(TJavaGenericImport<Jg_13Class, Jg_13>) end;

  Jg_2Class = interface(JView_OnClickListenerClass)
    ['{615DE5ED-55B2-44C3-AA5A-51F968BDCB94}']
    {class} function init(P1: Ja_g): Jg_2; cdecl;//Deprecated
    {class} procedure onClick(P1: JView); cdecl;//Deprecated
  end;

  [JavaSignature('br/com/gertec/gedi/a/g$2')]
  Jg_2 = interface(JView_OnClickListener)
    ['{17A280E2-EC1D-4AED-A15C-35F76EA3FB6E}']
  end;
  TJg_2 = class(TJavaGenericImport<Jg_2Class, Jg_2>) end;

  Jg_3Class = interface(JView_OnClickListenerClass)
    ['{3EF1B00C-50D9-4945-8863-E370BB23B8AD}']
    {class} function init(P1: Ja_g): Jg_3; cdecl;//Deprecated
    {class} procedure onClick(P1: JView); cdecl;//Deprecated
  end;

  [JavaSignature('br/com/gertec/gedi/a/g$3')]
  Jg_3 = interface(JView_OnClickListener)
    ['{3A9C0CB7-BF43-40FC-9662-D1B07A2298C6}']
  end;
  TJg_3 = class(TJavaGenericImport<Jg_3Class, Jg_3>) end;

  Jg_4Class = interface(JView_OnClickListenerClass)
    ['{49DD9FD8-F4B6-4245-8462-BBD68BE7134B}']
    {class} function init(P1: Ja_g): Jg_4; cdecl;//Deprecated
    {class} procedure onClick(P1: JView); cdecl;//Deprecated
  end;

  [JavaSignature('br/com/gertec/gedi/a/g$4')]
  Jg_4 = interface(JView_OnClickListener)
    ['{C0D60C20-E268-4F2D-85F5-E048C2748B99}']
  end;
  TJg_4 = class(TJavaGenericImport<Jg_4Class, Jg_4>) end;

  Jg_5Class = interface(JView_OnClickListenerClass)
    ['{A70F6FA7-CD86-4BB3-9AB8-ECB733694325}']
    {class} function init(P1: Ja_g): Jg_5; cdecl;//Deprecated
    {class} procedure onClick(P1: JView); cdecl;//Deprecated
  end;

  [JavaSignature('br/com/gertec/gedi/a/g$5')]
  Jg_5 = interface(JView_OnClickListener)
    ['{7C5B71DC-67D5-43E5-AB6B-2E1BB1C3440B}']
  end;
  TJg_5 = class(TJavaGenericImport<Jg_5Class, Jg_5>) end;

  Jg_6Class = interface(JView_OnClickListenerClass)
    ['{7A27B7AB-D152-4851-A696-0E7DD9B80928}']
    {class} function init(P1: Ja_g): Jg_6; cdecl;//Deprecated
    {class} procedure onClick(P1: JView); cdecl;//Deprecated
  end;

  [JavaSignature('br/com/gertec/gedi/a/g$6')]
  Jg_6 = interface(JView_OnClickListener)
    ['{C85D4DBF-AC4D-46F6-8AAA-5900CCC546CB}']
  end;
  TJg_6 = class(TJavaGenericImport<Jg_6Class, Jg_6>) end;

  Jg_7Class = interface(JView_OnClickListenerClass)
    ['{B80992B6-9D21-44F0-A014-A17AA92BAE27}']
    {class} function init(P1: Ja_g): Jg_7; cdecl;//Deprecated
    {class} procedure onClick(P1: JView); cdecl;//Deprecated
  end;

  [JavaSignature('br/com/gertec/gedi/a/g$7')]
  Jg_7 = interface(JView_OnClickListener)
    ['{DE96A3C9-656F-4C1A-A3C7-F466910F482D}']
  end;
  TJg_7 = class(TJavaGenericImport<Jg_7Class, Jg_7>) end;

  Jg_8Class = interface(JView_OnClickListenerClass)
    ['{9121632C-CEB4-4B01-8407-A7976897AD65}']
    {class} function init(P1: Ja_g): Jg_8; cdecl;//Deprecated
    {class} procedure onClick(P1: JView); cdecl;//Deprecated
  end;

  [JavaSignature('br/com/gertec/gedi/a/g$8')]
  Jg_8 = interface(JView_OnClickListener)
    ['{4724F673-11B4-4884-9D48-2D2DEF475D00}']
  end;
  TJg_8 = class(TJavaGenericImport<Jg_8Class, Jg_8>) end;

  Jg_9Class = interface(JView_OnClickListenerClass)
    ['{E4229CD6-D7BB-4459-A5B8-EA8C492235CA}']
    {class} function init(P1: Ja_g): Jg_9; cdecl;//Deprecated
    {class} procedure onClick(P1: JView); cdecl;//Deprecated
  end;

  [JavaSignature('br/com/gertec/gedi/a/g$9')]
  Jg_9 = interface(JView_OnClickListener)
    ['{B26CDEE7-9A0A-4A49-9FE9-7347AB5D4A81}']
  end;
  TJg_9 = class(TJavaGenericImport<Jg_9Class, Jg_9>) end;

  JIKMSClass = interface(IJavaClass)
    ['{0C195D92-8F52-4578-85B9-2A584E76C1A8}']
    {class} function CapturePINBlock(P1: JGEDI_KMS_st_Control; P2: Boolean; P3: JGEDI_KMS_st_Data; P4: JList): JGEDI_KMS_st_CapturePINBlockInfo; cdecl;
    {class} function DUKPTKSNGet(P1: JGEDI_KMS_e_KEYTYPE; P2: JGEDI_KMS_e_KEYPURPOSE; P3: Integer): TJavaArray<Byte>; cdecl;
    {class} function EncryptData(P1: JGEDI_KMS_st_Data): JGEDI_KMS_st_Data; cdecl;
    {class} function KCVGet(P1: JGEDI_KMS_e_KEYTYPE; P2: JGEDI_KMS_e_KEYPURPOSE; P3: Integer): TJavaArray<Byte>; cdecl;
    {class} function KeyPresenceTest(P1: JGEDI_KMS_e_KEYTYPE; P2: JGEDI_KMS_e_KEYPURPOSE; P3: Integer): Boolean; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/interfaces/IKMS')]
  JIKMS = interface(IJavaInstance)
    ['{442B051B-F881-4DF4-8A2A-8C4D7D53EB64}']
  end;
  TJIKMS = class(TJavaGenericImport<JIKMSClass, JIKMS>) end;

  Jgedi_gClass = interface(JIKMSClass)
    ['{C8D1E514-181D-47E3-A39E-23C618D06354}']
    {class} function CapturePINBlock(P1: JGEDI_KMS_st_Control; P2: Boolean; P3: JGEDI_KMS_st_Data; P4: JList): JGEDI_KMS_st_CapturePINBlockInfo; cdecl;
    {class} function DUKPTKSNGet(P1: JGEDI_KMS_e_KEYTYPE; P2: JGEDI_KMS_e_KEYPURPOSE; P3: Integer): TJavaArray<Byte>; cdecl;
    {class} function EncryptData(P1: JGEDI_KMS_st_Data): JGEDI_KMS_st_Data; cdecl;
    {class} function KCVGet(P1: JGEDI_KMS_e_KEYTYPE; P2: JGEDI_KMS_e_KEYPURPOSE; P3: Integer): TJavaArray<Byte>; cdecl;
    {class} function KeyPresenceTest(P1: JGEDI_KMS_e_KEYTYPE; P2: JGEDI_KMS_e_KEYPURPOSE; P3: Integer): Boolean; cdecl;
    {class} procedure a; cdecl; overload;
    {class} procedure a(P1: JGEDI_KMS_st_SaveKey); cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: Integer): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Byte>): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: TJavaArray<Byte>; P4: Integer; P5: JString; P6: TJavaArray<Byte>; P7: TJavaArray<Byte>): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Boolean; P6: JGEDI_KMS_st_Control_Callbacks; P7: Boolean; P8: TJavaArray<Byte>; P9: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: TJavaArray<Byte>; P6: TJavaArray<Byte>; P7: TJavaArray<Byte>; P8: TJavaArray<Byte>; P9: TJavaArray<Integer>; P10: TJavaArray<Byte>): Integer; cdecl; overload;
    {class} function init: Jgedi_g; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/g')]
  Jgedi_g = interface(JIKMS)
    ['{15AF8D77-675C-4A62-B857-B57A3E8C76D9}']
  end;
  TJgedi_g = class(TJavaGenericImport<Jgedi_gClass, Jgedi_g>) end;

  Ja_hClass = interface(Jgedi_gClass)
    ['{FB373295-3996-47C2-A7C8-36FDE033A669}']
    {class} function CapturePINBlock(P1: JGEDI_KMS_st_Control; P2: Boolean; P3: JGEDI_KMS_st_Data; P4: JList): JGEDI_KMS_st_CapturePINBlockInfo; cdecl;
    {class} function DUKPTKSNGet(P1: JGEDI_KMS_e_KEYTYPE; P2: JGEDI_KMS_e_KEYPURPOSE; P3: Integer): TJavaArray<Byte>; cdecl;
    {class} function EncryptData(P1: JGEDI_KMS_st_Data): JGEDI_KMS_st_Data; cdecl;
    {class} function KCVGet(P1: JGEDI_KMS_e_KEYTYPE; P2: JGEDI_KMS_e_KEYPURPOSE; P3: Integer): TJavaArray<Byte>; cdecl;
    {class} function KeyPresenceTest(P1: JGEDI_KMS_e_KEYTYPE; P2: JGEDI_KMS_e_KEYPURPOSE; P3: Integer): Boolean; cdecl;
    {class} procedure a; cdecl; overload;
    {class} procedure a(P1: JGEDI_KMS_st_SaveKey); cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: TJavaArray<Byte>; P4: Integer; P5: JString; P6: TJavaArray<Byte>; P7: TJavaArray<Byte>): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Boolean; P6: JGEDI_KMS_st_Control_Callbacks; P7: Boolean; P8: TJavaArray<Byte>; P9: TJavaArray<Integer>): Integer; cdecl; overload;
  end;

  [JavaSignature('br/com/gertec/gedi/a/h')]
  Ja_h = interface(Jgedi_g)
    ['{2CEFCDB2-55A3-4457-A93B-8476588F9C48}']
  end;
  TJa_h = class(TJavaGenericImport<Ja_hClass, Ja_h>) end;

  Jc_aClass = interface(JBinderClass)
    ['{1F8DF54F-25CF-4603-90D0-BCCDC71BE85A}']
    {class} function a(P1: JIBinder): Ja_c; cdecl;
    {class} function asBinder: JIBinder; cdecl;
    {class} function init: Jc_a; cdecl;
    {class} function onTransact(P1: Integer; P2: JParcel; P3: JParcel; P4: Integer): Boolean; cdecl;
  end;

  [JavaSignature('wangpos/sdk4/a/c$a')]
  Jc_a = interface(JBinder)
    ['{C788DB69-256D-4D4B-8012-7D926B9713A8}']
  end;
  TJc_a = class(TJavaGenericImport<Jc_aClass, Jc_a>) end;

  Jh_1Class = interface(Jc_aClass)
    ['{463B6FDD-5F94-4608-BD15-598AA6C02BA3}']
    {class} function a(P1: Integer; P2: TJavaArray<Byte>; P3: TJavaArray<Byte>; P4: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function init(P1: Ja_h): Jh_1; cdecl;//Deprecated
  end;

  [JavaSignature('br/com/gertec/gedi/a/h$1')]
  Jh_1 = interface(Jc_a)
    ['{97E08DC2-A21D-4215-A056-8EB5835732BA}']
  end;
  TJh_1 = class(TJavaGenericImport<Jh_1Class, Jh_1>) end;

  Jh_2Class = interface(JObjectClass)
    ['{52311C16-53E1-4D66-9C66-31A35B06FDF8}']
    {class} function _Getb: TJavaArray<Integer>; cdecl;
    {class} property b: TJavaArray<Integer> read _Getb;
  end;

  [JavaSignature('br/com/gertec/gedi/a/h$2')]
  Jh_2 = interface(JObject)
    ['{8C3A7276-6126-449D-B987-402CAD8E5A5F}']
  end;
  TJh_2 = class(TJavaGenericImport<Jh_2Class, Jh_2>) end;

  JIMSRClass = interface(IJavaClass)
    ['{40D15793-0E16-4301-BFB0-B01DA0EB178F}']
    {class} function LastErrorGet: JGEDI_MSR_st_LastErrors; cdecl;
    {class} function Read: JGEDI_MSR_st_Tracks; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/interfaces/IMSR')]
  JIMSR = interface(IJavaInstance)
    ['{7BA97BFD-2BB8-4EE4-B8CE-2EA2E1909B6A}']
  end;
  TJIMSR = class(TJavaGenericImport<JIMSRClass, JIMSR>) end;

  Jgedi_iClass = interface(JIMSRClass)
    ['{1AED37A2-14DF-4BE4-8140-61FE0C91ECF2}']
    {class} function LastErrorGet: JGEDI_MSR_st_LastErrors; cdecl;
    {class} function Read: JGEDI_MSR_st_Tracks; cdecl;
    {class} function a(P1: TJavaArray<Integer>; P2: TJavaArray<Integer>; P3: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: TJavaArray<Byte>; P2: TJavaArray<Integer>; P3: TJavaArray<Byte>; P4: TJavaArray<Integer>; P5: TJavaArray<Byte>; P6: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function init: Jgedi_i; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/i')]
  Jgedi_i = interface(JIMSR)
    ['{F4A35F4D-F75D-4ED3-84E6-A940DCC17E3D}']
  end;
  TJgedi_i = class(TJavaGenericImport<Jgedi_iClass, Jgedi_i>) end;

  Ja_iClass = interface(Jgedi_iClass)
    ['{BE326D1E-095A-4255-BE49-F4C9D695F1D2}']
    {class} function LastErrorGet: JGEDI_MSR_st_LastErrors; cdecl;//Deprecated
    {class} function Read: JGEDI_MSR_st_Tracks; cdecl;//Deprecated
    {class} function a: Jgertec_Logger; cdecl; overload;//Deprecated
    {class} function a(P1: Ja_i): JObject; cdecl; overload;//Deprecated
    {class} function a(P1: Ja_i; P2: JGEDI_MSR_e_Status): JGEDI_MSR_e_Status; cdecl; overload;//Deprecated
    {class} function a(P1: Ja_i; P2: JGEDI_MSR_st_Tracks): JGEDI_MSR_st_Tracks; cdecl; overload;//Deprecated
    {class} function b(P1: Ja_i): JGEDI_MSR_st_Tracks; cdecl;//Deprecated
    {class} function init(P1: Ja_o; P2: JContext): Ja_i; cdecl;//Deprecated
  end;

  [JavaSignature('br/com/gertec/gedi/a/i')]
  Ja_i = interface(Jgedi_i)
    ['{796D0FEA-B954-4787-A96C-307B7FDED219}']
  end;
  TJa_i = class(TJavaGenericImport<Ja_iClass, Ja_i>) end;

  Ji_1Class = interface(JBroadcastReceiverClass)
    ['{E90BD8F3-4690-414E-A85B-A2C2BED5C804}']
    {class} function init(P1: Ja_i): Ji_1; cdecl;//Deprecated
    {class} procedure onReceive(P1: JContext; P2: JIntent); cdecl;//Deprecated
  end;

  [JavaSignature('br/com/gertec/gedi/a/i$1')]
  Ji_1 = interface(JBroadcastReceiver)
    ['{BE117D99-0044-4026-817C-5553E27A24B1}']
  end;
  TJi_1 = class(TJavaGenericImport<Ji_1Class, Ji_1>) end;

  JIPMClass = interface(IJavaClass)
    ['{7AEC7C15-09DD-40A2-A68C-F5A117BAEFD8}']
    {class} procedure ApDefaultSet(P1: JString); cdecl;
    {class} procedure ApDelete(P1: JString); cdecl;
    {class} procedure UpdateFromFile(P1: JString; P2: JGEDI_FS_e_Storage); cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/interfaces/IPM')]
  JIPM = interface(IJavaInstance)
    ['{CEB5C2CF-3EAA-45BE-A008-6D93E47C8602}']
  end;
  TJIPM = class(TJavaGenericImport<JIPMClass, JIPM>) end;

  Jgedi_jClass = interface(JIPMClass)
    ['{441A1448-B650-42FD-AC57-C6D1E8A4807C}']
    {class} procedure ApDefaultSet(P1: JString); cdecl;
    {class} procedure ApDelete(P1: JString); cdecl;
    {class} procedure UpdateFromFile(P1: JString; P2: JGEDI_FS_e_Storage); cdecl;
    {class} function init: Jgedi_j; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/j')]
  Jgedi_j = interface(JIPM)
    ['{DF64EE86-11AE-4D1B-8FC2-B1D0FD5FE3B5}']
  end;
  TJgedi_j = class(TJavaGenericImport<Jgedi_jClass, Jgedi_j>) end;

  Ja_jClass = interface(Jgedi_jClass)
    ['{3446C1AB-C4C7-4D5C-A791-614BBA04DACB}']
    {class} procedure ApDefaultSet(P1: JString); cdecl;
    {class} procedure ApDelete(P1: JString); cdecl;
    {class} procedure UpdateFromFile(P1: JString; P2: JGEDI_FS_e_Storage); cdecl;
    {class} function init(P1: Ja_o): Ja_j; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/a/j')]
  Ja_j = interface(Jgedi_j)
    ['{3598A24F-AD9B-40FC-BAB3-89EF61C0CDBC}']
  end;
  TJa_j = class(TJavaGenericImport<Ja_jClass, Ja_j>) end;

  JIPRNTRClass = interface(IJavaClass)
    ['{6090E150-81FD-4D2D-997A-B944F1028B7E}']
    {class} procedure DrawBarCode(P1: JGEDI_PRNTR_st_BarCodeConfig; P2: JString); cdecl;
    {class} procedure DrawBlankLine(P1: Integer); cdecl;
    {class} procedure DrawPictureExt(P1: JGEDI_PRNTR_st_PictureConfig; P2: JBitmap); cdecl;
    {class} procedure DrawStringExt(P1: JGEDI_PRNTR_st_StringConfig; P2: JString); cdecl;
    {class} function GetPaperUsage: Integer; cdecl;
    {class} procedure Init; cdecl;
    {class} procedure Output; cdecl;
    {class} procedure ResetPaperUsage; cdecl;
    {class} function Status: JGEDI_PRNTR_e_Status; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/interfaces/IPRNTR')]
  JIPRNTR = interface(IJavaInstance)
    ['{4439C562-3655-4FD0-8337-B97E10D55EB4}']
    //Adicionado por Marcos
    {class} procedure DrawBarCode(P1: JGEDI_PRNTR_st_BarCodeConfig; P2: JString); cdecl;
    {class} procedure DrawBlankLine(P1: Integer); cdecl;
    {class} procedure DrawPictureExt(P1: JGEDI_PRNTR_st_PictureConfig; P2: JBitmap); cdecl;
    {class} procedure DrawStringExt(P1: JGEDI_PRNTR_st_StringConfig; P2: JString); cdecl;
    {class} function GetPaperUsage: Integer; cdecl;
    {class} procedure Init; cdecl;
    {class} procedure Output; cdecl;
    {class} procedure ResetPaperUsage; cdecl;
    {class} function Status: JGEDI_PRNTR_e_Status; cdecl;

  end;
  TJIPRNTR = class(TJavaGenericImport<JIPRNTRClass, JIPRNTR>) end;

  Jgedi_kClass = interface(JIPRNTRClass)
    ['{BB8F087B-95E7-4E85-B2C4-CD3D222B4F85}']
    {class} procedure DrawBarCode(P1: JGEDI_PRNTR_st_BarCodeConfig; P2: JString); cdecl;
    {class} procedure DrawBlankLine(P1: Integer); cdecl;
    {class} procedure DrawPictureExt(P1: JGEDI_PRNTR_st_PictureConfig; P2: JBitmap); cdecl;
    {class} procedure DrawStringExt(P1: JGEDI_PRNTR_st_StringConfig; P2: JString); cdecl;
    {class} function GetPaperUsage: Integer; cdecl;
    {class} procedure Init; cdecl;
    {class} procedure Output; cdecl;
    {class} procedure ResetPaperUsage; cdecl;
    {class} function Status: JGEDI_PRNTR_e_Status; cdecl;
    //Marcos {class} function init: Jgedi_k; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/k')]
  Jgedi_k = interface(JIPRNTR)
    ['{F50DA9AD-7B16-4736-831C-A381DC6D3F4F}']
  end;
  TJgedi_k = class(TJavaGenericImport<Jgedi_kClass, Jgedi_k>) end;

  Ja_kClass = interface(Jgedi_kClass)
    ['{EC0632E5-311B-4F1B-B338-FED855C25460}']
    {class} procedure DrawBarCode(P1: JGEDI_PRNTR_st_BarCodeConfig; P2: JString); cdecl;
    {class} procedure DrawBlankLine(P1: Integer); cdecl;
    {class} procedure DrawPictureExt(P1: JGEDI_PRNTR_st_PictureConfig; P2: JBitmap); cdecl;
    {class} procedure DrawStringExt(P1: JGEDI_PRNTR_st_StringConfig; P2: JString); cdecl;
    {class} function GetPaperUsage: Integer; cdecl;
    {class} procedure Init; cdecl; overload;
    {class} procedure Output; cdecl;
    {class} procedure ResetPaperUsage; cdecl;
    {class} function Status: JGEDI_PRNTR_e_Status; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/a/k')]
  Ja_k = interface(Jgedi_k)
    ['{7476C5DF-4411-4437-BAE2-A5D2FD739BB8}']
  end;
  TJa_k = class(TJavaGenericImport<Ja_kClass, Ja_k>) end;

  Jk_1Class = interface(JObjectClass)
    ['{839C47A8-24AB-4844-93FE-7737E0778DFA}']
    {class} function _Getb: TJavaArray<Integer>; cdecl;
    {class} function _Getc: TJavaArray<Integer>; cdecl;
    {class} property b: TJavaArray<Integer> read _Getb;
    {class} property c: TJavaArray<Integer> read _Getc;
  end;

  [JavaSignature('br/com/gertec/gedi/a/k$1')]
  Jk_1 = interface(JObject)
    ['{F955E1BF-C782-45F0-B087-3D121B1575CF}']
  end;
  TJk_1 = class(TJavaGenericImport<Jk_1Class, Jk_1>) end;

  JISMARTClass = interface(IJavaClass)
    ['{D4D17875-F5AD-4048-A84C-4341BE381813}']
    {class} procedure PowerOff(P1: JGEDI_SMART_e_Slot); cdecl;
    {class} function ResetEMV(P1: JGEDI_SMART_e_Slot; P2: JGEDI_SMART_e_Voltage): JGEDI_SMART_st_ResetInfo; cdecl;
    {class} function SendAPDU(P1: JGEDI_SMART_e_Slot; P2: TJavaArray<Byte>): TJavaArray<Byte>; cdecl;
    {class} function Status(P1: JGEDI_SMART_e_Slot): JGEDI_SMART_e_Status; cdecl;
    {class} function WarmResetEMV(P1: JGEDI_SMART_e_Slot; P2: JGEDI_SMART_e_Voltage): JGEDI_SMART_st_ResetInfo; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/interfaces/ISMART')]
  JISMART = interface(IJavaInstance)
    ['{A68FC607-CF5C-4ECA-8046-63DB90137BEA}']
  end;
  TJISMART = class(TJavaGenericImport<JISMARTClass, JISMART>) end;

  Jgedi_lClass = interface(JISMARTClass)
    ['{D32FECB1-B4DA-42CB-BA20-A8BB9CADCE29}']
    {class} procedure PowerOff(P1: JGEDI_SMART_e_Slot); cdecl;
    {class} function ResetEMV(P1: JGEDI_SMART_e_Slot; P2: JGEDI_SMART_e_Voltage): JGEDI_SMART_st_ResetInfo; cdecl;
    {class} function SendAPDU(P1: JGEDI_SMART_e_Slot; P2: TJavaArray<Byte>): TJavaArray<Byte>; cdecl;
    {class} function Status(P1: JGEDI_SMART_e_Slot): JGEDI_SMART_e_Status; cdecl;
    {class} function WarmResetEMV(P1: JGEDI_SMART_e_Slot; P2: JGEDI_SMART_e_Voltage): JGEDI_SMART_st_ResetInfo; cdecl;
    {class} function init: Jgedi_l; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/l')]
  Jgedi_l = interface(JISMART)
    ['{1232295C-64CB-4EEE-A0B2-34795CBA6BB5}']
  end;
  TJgedi_l = class(TJavaGenericImport<Jgedi_lClass, Jgedi_l>) end;

  Ja_lClass = interface(Jgedi_lClass)
    ['{CC099DAF-FAC9-4D6C-A9A3-37B488BC85EB}']
    {class} function _Getb: Integer; cdecl;
    {class} procedure _Setb(Value: Integer); cdecl;
    {class} function _Getc: JBroadcastReceiver; cdecl;
    {class} procedure _Setc(Value: JBroadcastReceiver); cdecl;
    {class} procedure PowerOff(P1: JGEDI_SMART_e_Slot); cdecl;//Deprecated
    {class} function ResetEMV(P1: JGEDI_SMART_e_Slot; P2: JGEDI_SMART_e_Voltage): JGEDI_SMART_st_ResetInfo; cdecl;//Deprecated
    {class} function SendAPDU(P1: JGEDI_SMART_e_Slot; P2: TJavaArray<Byte>): TJavaArray<Byte>; cdecl;//Deprecated
    {class} function Status(P1: JGEDI_SMART_e_Slot): JGEDI_SMART_e_Status; cdecl;//Deprecated
    {class} function WarmResetEMV(P1: JGEDI_SMART_e_Slot; P2: JGEDI_SMART_e_Voltage): JGEDI_SMART_st_ResetInfo; cdecl;//Deprecated
    {class} procedure a; cdecl; overload;//Deprecated
    {class} function a(P1: Ja_l): Ja_o; cdecl; overload;//Deprecated
    {class} function b: Jgertec_Logger; cdecl;//Deprecated
    {class} function init(P1: Ja_o; P2: JContext): Ja_l; cdecl;//Deprecated
    {class} property b: Integer read _Getb write _Setb;
    {class} property c: JBroadcastReceiver read _Getc write _Setc;
  end;

  [JavaSignature('br/com/gertec/gedi/a/l')]
  Ja_l = interface(Jgedi_l)
    ['{BF7F86DD-C2D0-4EAA-AAB5-139D3B124546}']
  end;
  TJa_l = class(TJavaGenericImport<Ja_lClass, Ja_l>) end;

  Jl_1Class = interface(JBroadcastReceiverClass)
    ['{3F806E80-28FB-42FA-B55A-730AE9450AA0}']
    {class} function init(P1: Ja_l): Jl_1; cdecl;//Deprecated
    {class} procedure onReceive(P1: JContext; P2: JIntent); cdecl;//Deprecated
  end;

  [JavaSignature('br/com/gertec/gedi/a/l$1')]
  Jl_1 = interface(JBroadcastReceiver)
    ['{0D71AD34-7A44-488B-88CA-8629FEBCBF66}']
  end;
  TJl_1 = class(TJavaGenericImport<Jl_1Class, Jl_1>) end;

  Jl_2Class = interface(JBroadcastReceiverClass)
    ['{011F45C6-608B-4DA7-B991-D89263B3993F}']
    {class} function init(P1: Ja_l): Jl_2; cdecl;//Deprecated
    {class} procedure onReceive(P1: JContext; P2: JIntent); cdecl;//Deprecated
  end;

  [JavaSignature('br/com/gertec/gedi/a/l$2')]
  Jl_2 = interface(JBroadcastReceiver)
    ['{FA538712-ECD1-4DE8-814C-16AA4C2DA391}']
  end;
  TJl_2 = class(TJavaGenericImport<Jl_2Class, Jl_2>) end;

  Jl_3Class = interface(JThreadClass)
    ['{EA3B8B78-A3E0-4FBF-A195-081D6A26ED2A}']
    {class} function init(P1: Ja_l): Jl_3; cdecl;//Deprecated
    {class} procedure run; cdecl;//Deprecated
  end;

  [JavaSignature('br/com/gertec/gedi/a/l$3')]
  Jl_3 = interface(JThread)
    ['{D94588E3-4240-48E3-ACD3-31598478B050}']
  end;
  TJl_3 = class(TJavaGenericImport<Jl_3Class, Jl_3>) end;

  JISYSClass = interface(IJavaClass)
    ['{10EA5380-9F2F-4ABA-B51D-DF9DBC9BC4CA}']
  end;

  [JavaSignature('br/com/gertec/gedi/interfaces/ISYS')]
  JISYS = interface(IJavaInstance)
    ['{364C7A6C-B89E-4B99-BFE1-50353DF83938}']
  end;
  TJISYS = class(TJavaGenericImport<JISYSClass, JISYS>) end;

  Jgedi_mClass = interface(JISYSClass)
    ['{2E7F6F3E-7198-47CE-986F-619AA1728E20}']
    {class} function init: Jgedi_m; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/m')]
  Jgedi_m = interface(JISYS)
    ['{DBD8EF11-BF7C-49D6-BE0A-6451D40EB53B}']
  end;
  TJgedi_m = class(TJavaGenericImport<Jgedi_mClass, Jgedi_m>) end;

  Ja_mClass = interface(Jgedi_mClass)
    ['{8ADF4069-2FA5-4BEC-BE4F-C3DE8356E5EC}']
    {class} function init(P1: Ja_o): Ja_m; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/a/m')]
  Ja_m = interface(Jgedi_m)
    ['{FEB20F87-7CED-4D98-B7D5-25A198857FB4}']
  end;
  TJa_m = class(TJavaGenericImport<Ja_mClass, Ja_m>) end;

  Ja_nClass = interface(JObjectClass)
    ['{747A06FE-785B-425D-B317-2170429F7DE9}']
    {class} function a(P1: TJavaArray<Byte>): JString; cdecl; overload;
    {class} function a(P1: JString): TJavaArray<Byte>; cdecl; overload;
    {class} function a(P1: TJavaArray<Byte>; P2: Integer): JString; cdecl; overload;
  end;

  [JavaSignature('br/com/gertec/gedi/a/n')]
  Ja_n = interface(JObject)
    ['{60FB8DDB-0F8C-43F3-BC0C-425844073F61}']
  end;
  TJa_n = class(TJavaGenericImport<Ja_nClass, Ja_n>) end;

  Ja_oClass = interface(JObjectClass)
    ['{BCAB1226-2A8C-41CB-9F6E-DD967CE2A9E8}']
    {class} function a(P1: JContext; P2: JIntent): JIntent; cdecl; overload;
  end;

  [JavaSignature('br/com/gertec/gedi/a/o')]
  Ja_o = interface(JObject)
    ['{7C0E96CB-F6E2-4390-82A6-25B44F92D97D}']
  end;
  TJa_o = class(TJavaGenericImport<Ja_oClass, Ja_o>) end;

  Jo_1Class = interface(JServiceConnectionClass)
    ['{1B2ED6ED-7503-409A-B6DD-4FEECCD6B5FA}']
    {class} function init(P1: Ja_o): Jo_1; cdecl;//Deprecated
    {class} procedure onServiceConnected(P1: JComponentName; P2: JIBinder); cdecl;//Deprecated
    {class} procedure onServiceDisconnected(P1: JComponentName); cdecl;//Deprecated
  end;

  [JavaSignature('br/com/gertec/gedi/a/o$1')]
  Jo_1 = interface(JServiceConnection)
    ['{E2A1F90C-BBB5-4917-B2E0-2750C81D114C}']
  end;
  TJo_1 = class(TJavaGenericImport<Jo_1Class, Jo_1>) end;

  JGEDI_CL_e_ISO_LevelClass = interface(JEnumClass)
    ['{4935CE15-780C-4FFD-AB5A-4689DEAE88C3}']
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
    ['{0E1CCAC8-C4D2-439E-9CB5-A65F2FEF3AC2}']
  end;
  TJGEDI_CL_e_ISO_Level = class(TJavaGenericImport<JGEDI_CL_e_ISO_LevelClass, JGEDI_CL_e_ISO_Level>) end;

  JGEDI_CL_e_ISO_TypeClass = interface(JEnumClass)
    ['{EC99121E-3AC8-4F0E-8EF7-9E5441520566}']
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
    ['{EF330149-1DE4-48B9-A2BB-289BCBA03F47}']
  end;
  TJGEDI_CL_e_ISO_Type = class(TJavaGenericImport<JGEDI_CL_e_ISO_TypeClass, JGEDI_CL_e_ISO_Type>) end;

  JGEDI_CL_e_MF_KeyTypeClass = interface(JEnumClass)
    ['{56184C46-A72A-41A1-B2F1-B8A80B46F52D}']
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
    ['{241F3897-00B9-4448-9801-6BA3E55362AD}']
  end;
  TJGEDI_CL_e_MF_KeyType = class(TJavaGenericImport<JGEDI_CL_e_MF_KeyTypeClass, JGEDI_CL_e_MF_KeyType>) end;

  JGEDI_CL_e_MF_TypeClass = interface(JEnumClass)
    ['{9673D973-5FEE-429F-BDE5-A5AE1202F20D}']
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
    ['{A6DBB479-33CD-4E28-B5F5-3F30D697EDD3}']
  end;
  TJGEDI_CL_e_MF_Type = class(TJavaGenericImport<JGEDI_CL_e_MF_TypeClass, JGEDI_CL_e_MF_Type>) end;

  JGEDI_CRYPT_e_OpClass = interface(JEnumClass)
    ['{EE63940C-DFA5-4A87-92EE-A682F0DB513D}']
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
    ['{3CBB3480-35F4-4E8F-A43C-425E04A2F9AE}']
  end;
  TJGEDI_CRYPT_e_Op = class(TJavaGenericImport<JGEDI_CRYPT_e_OpClass, JGEDI_CRYPT_e_Op>) end;

  JGEDI_FS_e_StorageClass = interface(JEnumClass)
    ['{A18811DD-ECA6-4A10-8AF2-D9AFAFA0B2BD}']
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
    ['{4804BB4C-4D9A-436A-9CEA-9BECDEB4AA0B}']
  end;
  TJGEDI_FS_e_Storage = class(TJavaGenericImport<JGEDI_FS_e_StorageClass, JGEDI_FS_e_Storage>) end;

  JGEDI_INFO_e_ControlNumberClass = interface(JEnumClass)
    ['{23B1AB58-70B2-4FA5-8F3C-F084566E5817}']
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
    ['{F6336914-4BED-4898-9083-A1AD3BB7961C}']
  end;
  TJGEDI_INFO_e_ControlNumber = class(TJavaGenericImport<JGEDI_INFO_e_ControlNumberClass, JGEDI_INFO_e_ControlNumber>) end;

  JGEDI_INFO_e_TestClass = interface(JEnumClass)
    ['{5653676E-97AA-4796-9E51-14CADD321D02}']
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
    ['{CAE3C898-8986-4C9C-924B-7D4AF4171F67}']
  end;
  TJGEDI_INFO_e_Test = class(TJavaGenericImport<JGEDI_INFO_e_TestClass, JGEDI_INFO_e_Test>) end;

  JGEDI_KBD_e_KeyClass = interface(JEnumClass)
    ['{07AFE650-8506-486D-BC02-DFBFECBB8015}']
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
    ['{6005C50D-64F2-4E9D-A075-A6EFBE6F0F8D}']
  end;
  TJGEDI_KBD_e_Key = class(TJavaGenericImport<JGEDI_KBD_e_KeyClass, JGEDI_KBD_e_Key>) end;

  JGEDI_KBD_e_PowerKeyModeClass = interface(JEnumClass)
    ['{D5F15981-A47D-473C-B03D-196C4EC52923}']
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
    ['{A03E4FAB-7A68-4FAA-A8B7-606AF71DEC26}']
  end;
  TJGEDI_KBD_e_PowerKeyMode = class(TJavaGenericImport<JGEDI_KBD_e_PowerKeyModeClass, JGEDI_KBD_e_PowerKeyMode>) end;

  JGEDI_KMS_e_BLOCKTYPEClass = interface(JEnumClass)
    ['{03790039-5653-4BD7-AB0D-F4AA8C311D17}']
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
    ['{C7E2820C-B1ED-4B1A-8C02-343D209ED6DB}']
  end;
  TJGEDI_KMS_e_BLOCKTYPE = class(TJavaGenericImport<JGEDI_KMS_e_BLOCKTYPEClass, JGEDI_KMS_e_BLOCKTYPE>) end;

  JGEDI_KMS_e_EncModeClass = interface(JEnumClass)
    ['{06E3B9F3-53E7-40FA-8FDA-62C56ECAA24C}']
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
    ['{92C795F9-9B15-4D06-8A2D-B7916E904FC1}']
  end;
  TJGEDI_KMS_e_EncMode = class(TJavaGenericImport<JGEDI_KMS_e_EncModeClass, JGEDI_KMS_e_EncMode>) end;

  JGEDI_KMS_e_KEYPURPOSEClass = interface(JEnumClass)
    ['{5E54A7D8-F5B6-4AC4-8B27-7677004EF797}']
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
    ['{F9FC7C3D-0671-46C8-950C-3F71A3FBFD11}']
  end;
  TJGEDI_KMS_e_KEYPURPOSE = class(TJavaGenericImport<JGEDI_KMS_e_KEYPURPOSEClass, JGEDI_KMS_e_KEYPURPOSE>) end;

  JGEDI_KMS_e_KEYTYPEClass = interface(JEnumClass)
    ['{1D5E2855-CA46-4140-B4B6-915D3180BB44}']
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
    ['{196653C6-A490-4CE4-9F19-B9EED6F5155D}']
  end;
  TJGEDI_KMS_e_KEYTYPE = class(TJavaGenericImport<JGEDI_KMS_e_KEYTYPEClass, JGEDI_KMS_e_KEYTYPE>) end;

  JGEDI_KMS_e_KEYTYPE_LENGTHClass = interface(JEnumClass)
    ['{852026CF-87D5-4AF4-9612-E0FFDC604B61}']
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
    ['{CEAA3CF8-B6BF-4C6C-8056-8AFB14FB9C45}']
  end;
  TJGEDI_KMS_e_KEYTYPE_LENGTH = class(TJavaGenericImport<JGEDI_KMS_e_KEYTYPE_LENGTHClass, JGEDI_KMS_e_KEYTYPE_LENGTH>) end;

  JGEDI_KMS_e_OPClass = interface(JEnumClass)
    ['{EE89E40E-1A75-4C9C-89E5-69223FA648FF}']
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
    ['{3C21A951-C312-4F7E-BF03-38AFA9B794C9}']
  end;
  TJGEDI_KMS_e_OP = class(TJavaGenericImport<JGEDI_KMS_e_OPClass, JGEDI_KMS_e_OP>) end;

  JGEDI_LED_e_IdClass = interface(JEnumClass)
    ['{4B489DDB-C7A4-47BE-A89B-E7BF9F7A91EF}']
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
    ['{7DD536C5-E67D-4819-8B9E-3538474D8F86}']
  end;
  TJGEDI_LED_e_Id = class(TJavaGenericImport<JGEDI_LED_e_IdClass, JGEDI_LED_e_Id>) end;

  JGEDI_MSR_e_StatusClass = interface(JEnumClass)
    ['{1583AA77-05A6-4B4F-923E-6BFBEFB7F8A4}']
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
    ['{310C088D-F836-4891-8A14-EE331E47EB5B}']
  end;
  TJGEDI_MSR_e_Status = class(TJavaGenericImport<JGEDI_MSR_e_StatusClass, JGEDI_MSR_e_Status>) end;

  JGEDI_PRNTR_e_AlignmentClass = interface(JEnumClass)
    ['{AF2BBB1D-5321-46CC-8D65-1C895F066AB4}']
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
    ['{1C0D7843-123B-42C8-BEFC-30CE0C22EB94}']
  end;
  TJGEDI_PRNTR_e_Alignment = class(TJavaGenericImport<JGEDI_PRNTR_e_AlignmentClass, JGEDI_PRNTR_e_Alignment>) end;

  JGEDI_PRNTR_e_BarCodeTypeClass = interface(JEnumClass)
    ['{5B9537E6-79F1-448D-88FA-65F5707C4734}']
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
    ['{1A412060-8C56-40C6-93A0-FFEB7ECC3013}']
  end;
  TJGEDI_PRNTR_e_BarCodeType = class(TJavaGenericImport<JGEDI_PRNTR_e_BarCodeTypeClass, JGEDI_PRNTR_e_BarCodeType>) end;

  JGEDI_PRNTR_e_StatusClass = interface(JEnumClass)
    ['{5EE56F74-E5D0-4738-AAB7-5AAD91BBFAD4}']
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
    ['{AB62D134-21AF-4083-A4AC-7A9CE3C9EFC0}']
  end;
  TJGEDI_PRNTR_e_Status = class(TJavaGenericImport<JGEDI_PRNTR_e_StatusClass, JGEDI_PRNTR_e_Status>) end;

  JGEDI_SMART_e_MemoryCardTypeClass = interface(JEnumClass)
    ['{B270F493-FB36-4C48-A20B-5D4E5D760850}']
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
    ['{8143AEB3-347A-474C-9B1C-62A3571C9FA8}']
  end;
  TJGEDI_SMART_e_MemoryCardType = class(TJavaGenericImport<JGEDI_SMART_e_MemoryCardTypeClass, JGEDI_SMART_e_MemoryCardType>) end;

  JGEDI_SMART_e_SlotClass = interface(JEnumClass)
    ['{E9166AA3-44D2-4E51-857C-602B7C9307E5}']
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
    ['{E7A0B24B-F508-45DF-9755-FEEA2E8B3047}']
  end;
  TJGEDI_SMART_e_Slot = class(TJavaGenericImport<JGEDI_SMART_e_SlotClass, JGEDI_SMART_e_Slot>) end;

  JGEDI_SMART_e_StatusClass = interface(JEnumClass)
    ['{58B447A7-6329-42A0-9FFA-A5E797F9C7C6}']
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
    ['{B06F4B26-DC80-45B6-AA39-7E587AA85E23}']
  end;
  TJGEDI_SMART_e_Status = class(TJavaGenericImport<JGEDI_SMART_e_StatusClass, JGEDI_SMART_e_Status>) end;

  JGEDI_SMART_e_TypeClass = interface(JEnumClass)
    ['{08EE1913-1599-469D-87B4-882F1F671E78}']
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
    ['{A592FFE1-8206-459D-B6A7-18C1BAAB23B0}']
  end;
  TJGEDI_SMART_e_Type = class(TJavaGenericImport<JGEDI_SMART_e_TypeClass, JGEDI_SMART_e_Type>) end;

  JGEDI_SMART_e_VoltageClass = interface(JEnumClass)
    ['{19E50CEC-7813-4D5B-9448-AE50F3B623C0}']
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
    ['{0793256B-1134-4303-8915-B453D92E3737}']
  end;
  TJGEDI_SMART_e_Voltage = class(TJavaGenericImport<JGEDI_SMART_e_VoltageClass, JGEDI_SMART_e_Voltage>) end;

  JGEDI_SYS_e_SecuritySetupClass = interface(JEnumClass)
    ['{0C2B5EAF-9162-4C16-9E8E-AB2DC01057EF}']
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
    ['{94DA16A7-C631-402C-8B8E-0FAFB97480C4}']
  end;
  TJGEDI_SYS_e_SecuritySetup = class(TJavaGenericImport<JGEDI_SYS_e_SecuritySetupClass, JGEDI_SYS_e_SecuritySetup>) end;

  JGEDI_e_RetClass = interface(JEnumClass)
    ['{D7AB8CC1-85A1-4092-9347-55FC33976B35}']
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
    ['{4B815384-29CD-4C40-9F4D-6C141B58B797}']
  end;
  TJGEDI_e_Ret = class(TJavaGenericImport<JGEDI_e_RetClass, JGEDI_e_Ret>) end;

  JGediExceptionClass = interface(JExceptionClass)
    ['{01DBD78A-2897-4286-8B8F-64CC7F3699DC}']
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
    ['{D3A6F4DF-5061-42A6-AA76-51EB66658CAD}']
  end;
  TJGediException = class(TJavaGenericImport<JGediExceptionClass, JGediException>) end;

  Jgedi_fClass = interface(JObjectClass)
    ['{3CE23EEF-F5FC-4A03-BF62-79986833B022}']
    {class} function _Geta: JButton; cdecl;
    {class} procedure _Seta(Value: JButton); cdecl;
    {class} function _Getb: JButton; cdecl;
    {class} procedure _Setb(Value: JButton); cdecl;
    {class} function _Getc: JButton; cdecl;
    {class} procedure _Setc(Value: JButton); cdecl;
    {class} function _Getd: JButton; cdecl;
    {class} procedure _Setd(Value: JButton); cdecl;
    {class} function _Gete: JButton; cdecl;
    {class} procedure _Sete(Value: JButton); cdecl;
    {class} function _Getf: JButton; cdecl;
    {class} procedure _Setf(Value: JButton); cdecl;
    {class} function _Getg: JButton; cdecl;
    {class} procedure _Setg(Value: JButton); cdecl;
    {class} function _Geth: JButton; cdecl;
    {class} procedure _Seth(Value: JButton); cdecl;
    {class} function _Geti: JButton; cdecl;
    {class} procedure _Seti(Value: JButton); cdecl;
    {class} function _Getj: JButton; cdecl;
    {class} procedure _Setj(Value: JButton); cdecl;
    {class} function _Getk: JButton; cdecl;
    {class} procedure _Setk(Value: JButton); cdecl;
    {class} function _Getl: JButton; cdecl;
    {class} procedure _Setl(Value: JButton); cdecl;
    {class} function _Getm: JButton; cdecl;
    {class} procedure _Setm(Value: JButton); cdecl;
    {class} function _Getn: JActivity; cdecl;
    {class} procedure _Setn(Value: JActivity); cdecl;
    {class} function a: Boolean; cdecl; overload;//Deprecated
    {class} function a(P1: Integer): JButton; cdecl; overload;//Deprecated
    {class} function b: Jgedi_f; cdecl;//Deprecated
    {class} function clone: JObject; cdecl;//Deprecated
    {class} function init: Jgedi_f; cdecl; overload;//Deprecated
    {class} function init(P1: JGEDI_KBD_st_Info): Jgedi_f; cdecl; overload;//Deprecated
    {class} function init(P1: JButton; P2: JButton; P3: JButton; P4: JButton; P5: JButton; P6: JButton; P7: JButton; P8: JButton; P9: JButton; P10: JButton; P11: JButton; P12: JButton; P13: JButton; P14: JActivity): Jgedi_f; cdecl; overload;//Deprecated
    {class} property a: JButton read _Geta write _Seta;
    {class} property b: JButton read _Getb write _Setb;
    {class} property c: JButton read _Getc write _Setc;
    {class} property d: JButton read _Getd write _Setd;
    {class} property e: JButton read _Gete write _Sete;
    {class} property f: JButton read _Getf write _Setf;
    {class} property g: JButton read _Getg write _Setg;
    {class} property h: JButton read _Geth write _Seth;
    {class} property i: JButton read _Geti write _Seti;
    {class} property j: JButton read _Getj write _Setj;
    {class} property k: JButton read _Getk write _Setk;
    {class} property l: JButton read _Getl write _Setl;
    {class} property m: JButton read _Getm write _Setm;
    {class} property n: JActivity read _Getn write _Setn;
  end;

  [JavaSignature('br/com/gertec/gedi/f')]
  Jgedi_f = interface(JObject)
    ['{8F2C9A3D-9C80-4E48-BA9E-854B10F45671}']
  end;
  TJgedi_f = class(TJavaGenericImport<Jgedi_fClass, Jgedi_f>) end;

  JILEDClass = interface(IJavaClass)
    ['{79ABD914-78C7-4737-A6ED-515D46BE21BB}']
    {class} procedure &Set(P1: JGEDI_LED_e_Id; P2: Boolean); cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/interfaces/ILED')]
  JILED = interface(IJavaInstance)
    ['{D8318063-5784-4B15-AE7E-6DB3A40CE47F}']
  end;
  TJILED = class(TJavaGenericImport<JILEDClass, JILED>) end;

  Jgedi_hClass = interface(JILEDClass)
    ['{341173EC-D96F-4406-9827-6520AC7191E2}']
  end;

  [JavaSignature('br/com/gertec/gedi/h')]
  Jgedi_h = interface(JILED)
    ['{271A9C9D-EB99-47BF-89B9-EB8DAE5D888D}']
  end;
  TJgedi_h = class(TJavaGenericImport<Jgedi_hClass, Jgedi_h>) end;

  JIEnumsClass = interface(IJavaClass)
    ['{8D7BD1CA-BA1B-4F40-80CB-779D9DEBF5E3}']
    {class} function getValue: Integer; cdecl;
  end;

  [JavaSignature('br/com/gertec/gedi/interfaces/IEnums')]
  JIEnums = interface(IJavaInstance)
    ['{B138388D-A1ED-437B-8E55-C65BFA0CF147}']
  end;
  TJIEnums = class(TJavaGenericImport<JIEnumsClass, JIEnums>) end;

  JGEDI_AUTH_st_DataClass = interface(JObjectClass)
    ['{40FBFBF4-36A8-4C4E-89FD-15017BAA28D6}']
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
    {class} function _GeteOperation: Integer; cdecl;
    {class} procedure _SeteOperation(Value: Integer); cdecl;
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
    {class} property eOperation: Integer read _GeteOperation write _SeteOperation;
    {class} property uiInputLen: Integer read _GetuiInputLen write _SetuiInputLen;
    {class} property uiKeyIndex: Integer read _GetuiKeyIndex write _SetuiKeyIndex;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_AUTH_st_Data')]
  JGEDI_AUTH_st_Data = interface(JObject)
    ['{4424340E-03D8-4742-B4DD-ED6B11A88B7A}']
  end;
  TJGEDI_AUTH_st_Data = class(TJavaGenericImport<JGEDI_AUTH_st_DataClass, JGEDI_AUTH_st_Data>) end;

  JGEDI_CLOCK_st_RTCClass = interface(JObjectClass)
    ['{E6397683-9B77-423F-BA6C-E30C1A40263C}']
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
    {class} function _GetbYear: Byte; cdecl;
    {class} procedure _SetbYear(Value: Byte); cdecl;
    {class} function init: JGEDI_CLOCK_st_RTC; cdecl; overload;//Deprecated
    {class} function init(P1: Byte; P2: Byte; P3: Byte; P4: Byte; P5: Byte; P6: Byte; P7: Byte): JGEDI_CLOCK_st_RTC; cdecl; overload;//Deprecated
    {class} property bDay: Byte read _GetbDay write _SetbDay;
    {class} property bDoW: Byte read _GetbDoW write _SetbDoW;
    {class} property bHour: Byte read _GetbHour write _SetbHour;
    {class} property bMinute: Byte read _GetbMinute write _SetbMinute;
    {class} property bMonth: Byte read _GetbMonth write _SetbMonth;
    {class} property bSecond: Byte read _GetbSecond write _SetbSecond;
    {class} property bYear: Byte read _GetbYear write _SetbYear;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_CLOCK_st_RTC')]
  JGEDI_CLOCK_st_RTC = interface(JObject)
    ['{8560536C-EDA0-4B6C-958E-64F575805530}']
  end;
  TJGEDI_CLOCK_st_RTC = class(TJavaGenericImport<JGEDI_CLOCK_st_RTCClass, JGEDI_CLOCK_st_RTC>) end;

  JGEDI_CL_st_ISO_PollingInfoClass = interface(JObjectClass)
    ['{A03BB2F6-5028-47C2-90BF-22EBD2E60EDE}']
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
    {class} function _GetpeType: JGEDI_CL_e_ISO_Type; cdecl;
    {class} procedure _SetpeType(Value: JGEDI_CL_e_ISO_Type); cdecl;
    {class} function init: JGEDI_CL_st_ISO_PollingInfo; cdecl;//Deprecated
    {class} property abATQA: TJavaArray<Byte> read _GetabATQA write _SetabATQA;
    {class} property abATQB: TJavaArray<Byte> read _GetabATQB write _SetabATQB;
    {class} property abATS: TJavaArray<Byte> read _GetabATS write _SetabATS;
    {class} property abATTRIBResp: TJavaArray<Byte> read _GetabATTRIBResp write _SetabATTRIBResp;
    {class} property abPUPI: TJavaArray<Byte> read _GetabPUPI write _SetabPUPI;
    {class} property abUID: TJavaArray<Byte> read _GetabUID write _SetabUID;
    {class} property bSAK: Byte read _GetbSAK write _SetbSAK;
    {class} property peType: JGEDI_CL_e_ISO_Type read _GetpeType write _SetpeType;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_CL_st_ISO_PollingInfo')]
  JGEDI_CL_st_ISO_PollingInfo = interface(JObject)
    ['{9DF8D068-1A96-4311-9EC3-CC52900A53B1}']
    // Adicionado pelo Geovani
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
    {class} function _GetpeType: JGEDI_CL_e_ISO_Type; cdecl;
    {class} function init: JGEDI_CL_st_ISO_PollingInfo; cdecl;//Deprecated
    {class} property abATQA: TJavaArray<Byte> read _GetabATQA write _SetabATQA;
    {class} property abATQB: TJavaArray<Byte> read _GetabATQB write _SetabATQB;
    {class} property abATS: TJavaArray<Byte> read _GetabATS write _SetabATS;
    {class} property abATTRIBResp: TJavaArray<Byte> read _GetabATTRIBResp write _SetabATTRIBResp;
    {class} property abPUPI: TJavaArray<Byte> read _GetabPUPI write _SetabPUPI;
    {class} property abUID: TJavaArray<Byte> read _GetabUID write _SetabUID;
    {class} property bSAK: Byte read _GetbSAK write _SetbSAK;
    {class} property peType: JGEDI_CL_e_ISO_Type read _GetpeType;
  end;
  TJGEDI_CL_st_ISO_PollingInfo = class(TJavaGenericImport<JGEDI_CL_st_ISO_PollingInfoClass, JGEDI_CL_st_ISO_PollingInfo>) end;

  JGEDI_CL_st_MF_ActivateInfoClass = interface(JObjectClass)
    ['{4ECB751C-041A-4996-B1EE-AC40E56A4A2A}']
    {class} function _GetabUID: TJavaArray<Byte>; cdecl;
    {class} procedure _SetabUID(Value: TJavaArray<Byte>); cdecl;
    {class} function _Gettype: JGEDI_CL_e_MF_Type; cdecl;
    {class} procedure _Settype(Value: JGEDI_CL_e_MF_Type); cdecl;
    {class} function init: JGEDI_CL_st_MF_ActivateInfo; cdecl;//Deprecated
    {class} property abUID: TJavaArray<Byte> read _GetabUID write _SetabUID;
    {class} property &type: JGEDI_CL_e_MF_Type read _Gettype write _Settype;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_CL_st_MF_ActivateInfo')]
  JGEDI_CL_st_MF_ActivateInfo = interface(JObject)
    ['{A93B50D1-5DF4-406B-8B19-C106B705992D}']
  end;
  TJGEDI_CL_st_MF_ActivateInfo = class(TJavaGenericImport<JGEDI_CL_st_MF_ActivateInfoClass, JGEDI_CL_st_MF_ActivateInfo>) end;

  JGEDI_CL_st_MF_KeyClass = interface(JObjectClass)
    ['{4B313FC2-4119-4397-B230-CC6F0BBCF4B0}']
    {class} function _GetabValue: TJavaArray<Byte>; cdecl;
    {class} procedure _SetabValue(Value: TJavaArray<Byte>); cdecl;
    {class} function _GeteType: JGEDI_CL_e_MF_KeyType; cdecl;
    {class} procedure _SeteType(Value: JGEDI_CL_e_MF_KeyType); cdecl;
    {class} function init: JGEDI_CL_st_MF_Key; cdecl;//Deprecated
    {class} property abValue: TJavaArray<Byte> read _GetabValue write _SetabValue;
    {class} property eType: JGEDI_CL_e_MF_KeyType read _GeteType write _SeteType;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_CL_st_MF_Key')]
  JGEDI_CL_st_MF_Key = interface(JObject)
    ['{F5A5788C-81EB-427C-97D4-94BB0315479F}']
  end;
  TJGEDI_CL_st_MF_Key = class(TJavaGenericImport<JGEDI_CL_st_MF_KeyClass, JGEDI_CL_st_MF_Key>) end;

  JGEDI_CL_st_ResetEMVInfoClass = interface(JObjectClass)
    ['{75AD962C-F21B-44F1-A9EE-6CB380E5852B}']
    {class} function _GetabATR: TJavaArray<Byte>; cdecl;
    {class} procedure _SetabATR(Value: TJavaArray<Byte>); cdecl;
    {class} function _GetpeCardType: JGEDI_CL_e_ISO_Type; cdecl;
    {class} procedure _SetpeCardType(Value: JGEDI_CL_e_ISO_Type); cdecl;
    {class} function init: JGEDI_CL_st_ResetEMVInfo; cdecl;//Deprecated
    {class} property abATR: TJavaArray<Byte> read _GetabATR write _SetabATR;
    {class} property peCardType: JGEDI_CL_e_ISO_Type read _GetpeCardType write _SetpeCardType;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_CL_st_ResetEMVInfo')]
  JGEDI_CL_st_ResetEMVInfo = interface(JObject)
    ['{F5C17BEB-F9BF-4B75-9EE8-20E292C30FA6}']
  end;
  TJGEDI_CL_st_ResetEMVInfo = class(TJavaGenericImport<JGEDI_CL_st_ResetEMVInfoClass, JGEDI_CL_st_ResetEMVInfo>) end;

  JGEDI_CRYPT_st_RSAKeyGenClass = interface(JObjectClass)
    ['{9302918D-C749-4177-AEB6-2DD604798A6F}']
    {class} function _GetabModulus: TJavaArray<Byte>; cdecl;
    {class} procedure _SetabModulus(Value: TJavaArray<Byte>); cdecl;
    {class} function _GetabPrivateKey: TJavaArray<Byte>; cdecl;
    {class} procedure _SetabPrivateKey(Value: TJavaArray<Byte>); cdecl;
    {class} function _GetabPublicKey: TJavaArray<Byte>; cdecl;
    {class} procedure _SetabPublicKey(Value: TJavaArray<Byte>); cdecl;
    {class} function _GetbVersion: Byte; cdecl;
    {class} procedure _SetbVersion(Value: Byte); cdecl;
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
    {class} property bVersion: Byte read _GetbVersion write _SetbVersion;
    {class} property uiBits: Integer read _GetuiBits write _SetuiBits;
    {class} property uiModulusLen: Integer read _GetuiModulusLen write _SetuiModulusLen;
    {class} property uiPrivateKeyLen: Integer read _GetuiPrivateKeyLen write _SetuiPrivateKeyLen;
    {class} property uiPublicKeyLen: Integer read _GetuiPublicKeyLen write _SetuiPublicKeyLen;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_CRYPT_st_RSAKeyGen')]
  JGEDI_CRYPT_st_RSAKeyGen = interface(JObject)
    ['{3E66E01B-F73F-49DE-9217-F3C6B3711F38}']
  end;
  TJGEDI_CRYPT_st_RSAKeyGen = class(TJavaGenericImport<JGEDI_CRYPT_st_RSAKeyGenClass, JGEDI_CRYPT_st_RSAKeyGen>) end;

  JGEDI_KBD_st_InfoClass = interface(JObjectClass)
    ['{7D63D2D3-D9C7-42AF-ADA5-12A232F0CA11}']
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
    {class} function clone: JGEDI_KBD_st_Info; cdecl;//Deprecated
    {class} function getButtonByIndex(P1: Integer): JButton; cdecl;//Deprecated
    {class} function init(P1: JButton; P2: JButton; P3: JButton; P4: JButton; P5: JButton; P6: JButton; P7: JButton; P8: JButton; P9: JButton; P10: JButton; P11: JButton; P12: JButton; P13: JButton; P14: JActivity): JGEDI_KBD_st_Info; cdecl;//Deprecated
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

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_KBD_st_Info')]
  JGEDI_KBD_st_Info = interface(JObject)
    ['{CAD36358-6FDF-48CA-BEB7-1F24C4368D6D}']
  end;
  TJGEDI_KBD_st_Info = class(TJavaGenericImport<JGEDI_KBD_st_InfoClass, JGEDI_KBD_st_Info>) end;

  JGEDI_KMS_st_CapturePINBlockInfoClass = interface(JObjectClass)
    ['{BECA244D-1577-40F7-9451-631ACF18DAED}']
    {class} function _GetastPB: JList; cdecl;
    {class} procedure _SetastPB(Value: JList); cdecl;
    {class} function _GetpstControl: JGEDI_KMS_st_Control; cdecl;
    {class} procedure _SetpstControl(Value: JGEDI_KMS_st_Control); cdecl;
    {class} function _GetpstData: JGEDI_KMS_st_Data; cdecl;
    {class} procedure _SetpstData(Value: JGEDI_KMS_st_Data); cdecl;
    {class} function init(P1: JGEDI_KMS_st_Control; P2: JGEDI_KMS_st_Data; P3: JList): JGEDI_KMS_st_CapturePINBlockInfo; cdecl;//Deprecated
    {class} property astPB: JList read _GetastPB write _SetastPB;
    {class} property pstControl: JGEDI_KMS_st_Control read _GetpstControl write _SetpstControl;
    {class} property pstData: JGEDI_KMS_st_Data read _GetpstData write _SetpstData;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_KMS_st_CapturePINBlockInfo')]
  JGEDI_KMS_st_CapturePINBlockInfo = interface(JObject)
    ['{F8BFC141-BE6A-4B89-A7E1-9171769E957F}']
  end;
  TJGEDI_KMS_st_CapturePINBlockInfo = class(TJavaGenericImport<JGEDI_KMS_st_CapturePINBlockInfoClass, JGEDI_KMS_st_CapturePINBlockInfo>) end;

  JGEDI_KMS_st_ControlClass = interface(JObjectClass)
    ['{0105C05B-405E-4B6E-8727-3A15128241EB}']
    {class} function _GetacClearPIN: JString; cdecl;
    {class} procedure _SetacClearPIN(Value: JString); cdecl;
    {class} function _GetbVersion: Byte; cdecl;
    {class} procedure _SetbVersion(Value: Byte); cdecl;
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
    {class} property bVersion: Byte read _GetbVersion write _SetbVersion;
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
    ['{9B7AD101-8D19-4BB4-AF6D-2FC37264EB99}']
  end;
  TJGEDI_KMS_st_Control = class(TJavaGenericImport<JGEDI_KMS_st_ControlClass, JGEDI_KMS_st_Control>) end;

  JGEDI_KMS_st_DataClass = interface(JObjectClass)
    ['{42A95EE5-4CA9-4DAA-8B74-05FA643D6C4D}']
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
    {class} function _GetbVersion: Byte; cdecl;
    {class} procedure _SetbVersion(Value: Byte); cdecl;
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
    {class} property bVersion: Byte read _GetbVersion write _SetbVersion;
    {class} property eKeyType: JGEDI_KMS_e_KEYTYPE read _GeteKeyType write _SeteKeyType;
    {class} property eMode: JGEDI_KMS_e_EncMode read _GeteMode write _SeteMode;
    {class} property eOperation: JGEDI_KMS_e_OP read _GeteOperation write _SeteOperation;
    {class} property uiKeyIndex: Integer read _GetuiKeyIndex write _SetuiKeyIndex;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_KMS_st_Data')]
  JGEDI_KMS_st_Data = interface(JObject)
    ['{EBF447CD-C412-4225-8402-540266867E98}']
  end;
  TJGEDI_KMS_st_Data = class(TJavaGenericImport<JGEDI_KMS_st_DataClass, JGEDI_KMS_st_Data>) end;

  JGEDI_KMS_st_KBClass = interface(JObjectClass)
    ['{E0902FB5-20FF-4A25-A94E-9015448DF48D}']
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
    {class} function _GetabKeyLen: TJavaArray<Byte>; cdecl;
    {class} procedure _SetabKeyLen(Value: TJavaArray<Byte>); cdecl;
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
    {class} property abKeyLen: TJavaArray<Byte> read _GetabKeyLen write _SetabKeyLen;
    {class} property abRND: TJavaArray<Byte> read _GetabRND write _SetabRND;
    {class} property bKEKType: JGEDI_KMS_e_KEYTYPE read _GetbKEKType write _SetbKEKType;
    {class} property bKeyPurpose: JGEDI_KMS_e_KEYPURPOSE read _GetbKeyPurpose write _SetbKeyPurpose;
    {class} property bKeyType: JGEDI_KMS_e_KEYTYPE read _GetbKeyType write _SetbKeyType;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_KMS_st_KB')]
  JGEDI_KMS_st_KB = interface(JObject)
    ['{82299C10-C525-45A1-8D88-2E0691F61DE5}']
  end;
  TJGEDI_KMS_st_KB = class(TJavaGenericImport<JGEDI_KMS_st_KBClass, JGEDI_KMS_st_KB>) end;

  JGEDI_KMS_st_PINBlockClass = interface(JObjectClass)
    ['{076EABF6-9384-4B27-AA8F-62B4FC0A8344}']
    {class} function _GetabEncPINBlock: TJavaArray<Byte>; cdecl;
    {class} procedure _SetabEncPINBlock(Value: TJavaArray<Byte>); cdecl;
    {class} function _GetabSeq: TJavaArray<Byte>; cdecl;
    {class} procedure _SetabSeq(Value: TJavaArray<Byte>); cdecl;
    {class} function _GetbCN: Byte; cdecl;
    {class} procedure _SetbCN(Value: Byte); cdecl;
    {class} function _GetbVersion: Byte; cdecl;
    {class} procedure _SetbVersion(Value: Byte); cdecl;
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
    {class} property bVersion: Byte read _GetbVersion write _SetbVersion;
    {class} property cPad: Byte read _GetcPad write _SetcPad;
    {class} property eBlockType: JGEDI_KMS_e_BLOCKTYPE read _GeteBlockType write _SeteBlockType;
    {class} property szPan: JString read _GetszPan write _SetszPan;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_KMS_st_PINBlock')]
  JGEDI_KMS_st_PINBlock = interface(JObject)
    ['{2476863E-5B2B-4D16-B6B5-62F2BE780433}']
  end;
  TJGEDI_KMS_st_PINBlock = class(TJavaGenericImport<JGEDI_KMS_st_PINBlockClass, JGEDI_KMS_st_PINBlock>) end;

  JGEDI_KMS_st_SaveKeyClass = interface(JObjectClass)
    ['{7E05A66F-5A88-4D3C-A7CF-634C0917DDF7}']
    {class} function _GetabKSN: TJavaArray<Byte>; cdecl;
    {class} procedure _SetabKSN(Value: TJavaArray<Byte>); cdecl;
    {class} function _GetabKey: TJavaArray<Byte>; cdecl;
    {class} procedure _SetabKey(Value: TJavaArray<Byte>); cdecl;
    {class} function _GetbVersion: Byte; cdecl;
    {class} procedure _SetbVersion(Value: Byte); cdecl;
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
    {class} property bVersion: Byte read _GetbVersion write _SetbVersion;
    {class} property eKeyPurpose: JGEDI_KMS_e_KEYPURPOSE read _GeteKeyPurpose write _SeteKeyPurpose;
    {class} property eKeyType: JGEDI_KMS_e_KEYTYPE read _GeteKeyType write _SeteKeyType;
    {class} property uiKeyIndex: Integer read _GetuiKeyIndex write _SetuiKeyIndex;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_KMS_st_SaveKey')]
  JGEDI_KMS_st_SaveKey = interface(JObject)
    ['{91C9B78D-EA3B-4792-A221-A1E3A3261EC8}']
  end;
  TJGEDI_KMS_st_SaveKey = class(TJavaGenericImport<JGEDI_KMS_st_SaveKeyClass, JGEDI_KMS_st_SaveKey>) end;

  JGEDI_MSR_st_LastErrorsClass = interface(JObjectClass)
    ['{A8C07F1F-18F0-4A3C-9D48-1E778BD5E361}']
    {class} function _GetpeTk1Err: JGEDI_MSR_e_Status; cdecl;
    {class} procedure _SetpeTk1Err(Value: JGEDI_MSR_e_Status); cdecl;
    {class} function _GetpeTk2Err: JGEDI_MSR_e_Status; cdecl;
    {class} procedure _SetpeTk2Err(Value: JGEDI_MSR_e_Status); cdecl;
    {class} function _GetpeTk3Err: JGEDI_MSR_e_Status; cdecl;
    {class} procedure _SetpeTk3Err(Value: JGEDI_MSR_e_Status); cdecl;
    {class} function init: JGEDI_MSR_st_LastErrors; cdecl;//Deprecated
    {class} property peTk1Err: JGEDI_MSR_e_Status read _GetpeTk1Err write _SetpeTk1Err;
    {class} property peTk2Err: JGEDI_MSR_e_Status read _GetpeTk2Err write _SetpeTk2Err;
    {class} property peTk3Err: JGEDI_MSR_e_Status read _GetpeTk3Err write _SetpeTk3Err;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_MSR_st_LastErrors')]
  JGEDI_MSR_st_LastErrors = interface(JObject)
    ['{5C1F283C-FEB0-4C77-B6B2-7ADE84463F28}']
  end;
  TJGEDI_MSR_st_LastErrors = class(TJavaGenericImport<JGEDI_MSR_st_LastErrorsClass, JGEDI_MSR_st_LastErrors>) end;

  JGEDI_MSR_st_TracksClass = interface(JObjectClass)
    ['{235A5927-B9A7-42AF-92BB-B3FF18DF724A}']
    {class} function _GetabTk1Buf: TJavaArray<Byte>; cdecl;
    {class} procedure _SetabTk1Buf(Value: TJavaArray<Byte>); cdecl;
    {class} function _GetabTk2Buf: TJavaArray<Byte>; cdecl;
    {class} procedure _SetabTk2Buf(Value: TJavaArray<Byte>); cdecl;
    {class} function _GetabTk3Buf: TJavaArray<Byte>; cdecl;
    {class} procedure _SetabTk3Buf(Value: TJavaArray<Byte>); cdecl;
    {class} function init: JGEDI_MSR_st_Tracks; cdecl;//Deprecated
    {class} property abTk1Buf: TJavaArray<Byte> read _GetabTk1Buf write _SetabTk1Buf;
    {class} property abTk2Buf: TJavaArray<Byte> read _GetabTk2Buf write _SetabTk2Buf;
    {class} property abTk3Buf: TJavaArray<Byte> read _GetabTk3Buf write _SetabTk3Buf;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_MSR_st_Tracks')]
  JGEDI_MSR_st_Tracks = interface(JObject)
    ['{685FD92D-69D9-41B6-9332-40820B80936F}']
  end;
  TJGEDI_MSR_st_Tracks = class(TJavaGenericImport<JGEDI_MSR_st_TracksClass, JGEDI_MSR_st_Tracks>) end;

  JGEDI_PRNTR_st_BarCodeConfigClass = interface(JObjectClass)
    ['{79B0DD4A-125E-4D04-AB17-B0430BF1AE01}']
    {class} function _GetbarCodeType: JGEDI_PRNTR_e_BarCodeType; cdecl;
    {class} procedure _SetbarCodeType(Value: JGEDI_PRNTR_e_BarCodeType); cdecl;
    {class} function _Getheight: Integer; cdecl;
    {class} procedure _Setheight(Value: Integer); cdecl;
    {class} function _Getwidth: Integer; cdecl;
    {class} procedure _Setwidth(Value: Integer); cdecl;
    {class} function init: JGEDI_PRNTR_st_BarCodeConfig; cdecl; overload;//Deprecated
    {class} function init(P1: JGEDI_PRNTR_e_BarCodeType; P2: Integer): JGEDI_PRNTR_st_BarCodeConfig; cdecl; overload;//Deprecated
    {class} function init(P1: JGEDI_PRNTR_e_BarCodeType; P2: Integer; P3: Integer): JGEDI_PRNTR_st_BarCodeConfig; cdecl; overload;//Deprecated
    {class} property barCodeType: JGEDI_PRNTR_e_BarCodeType read _GetbarCodeType write _SetbarCodeType;
    {class} property height: Integer read _Getheight write _Setheight;
    {class} property width: Integer read _Getwidth write _Setwidth;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_PRNTR_st_BarCodeConfig')]
  JGEDI_PRNTR_st_BarCodeConfig = interface(JObject)
    ['{B96BE22E-7D5B-4F42-879F-54BB704D2C48}']
    //Adicionado manualmente
    {class} function _GetbarCodeType: JGEDI_PRNTR_e_BarCodeType; cdecl;
    {class} procedure _SetbarCodeType(Value: JGEDI_PRNTR_e_BarCodeType); cdecl;
    {class} function _Getheight: Integer; cdecl;
    {class} procedure _Setheight(Value: Integer); cdecl;
    {class} function _Getwidth: Integer; cdecl;
    {class} procedure _Setwidth(Value: Integer); cdecl;
    {class} property barCodeType: JGEDI_PRNTR_e_BarCodeType read _GetbarCodeType write _SetbarCodeType;
    {class} property height: Integer read _Getheight write _Setheight;
    {class} property width: Integer read _Getwidth write _Setwidth;

  end;
  TJGEDI_PRNTR_st_BarCodeConfig = class(TJavaGenericImport<JGEDI_PRNTR_st_BarCodeConfigClass, JGEDI_PRNTR_st_BarCodeConfig>) end;

  JGEDI_PRNTR_st_PictureConfigClass = interface(JObjectClass)
    ['{BFCA9A0F-162C-40D9-BF47-C4ED74F1B0F9}']
    {class} function _Getalignment: JGEDI_PRNTR_e_Alignment; cdecl;
    {class} procedure _Setalignment(Value: JGEDI_PRNTR_e_Alignment); cdecl;
    {class} function _Getheight: Integer; cdecl;
    {class} procedure _Setheight(Value: Integer); cdecl;
    {class} function _Getoffset: Integer; cdecl;
    {class} procedure _Setoffset(Value: Integer); cdecl;
    {class} function _Getwidth: Integer; cdecl;
    {class} procedure _Setwidth(Value: Integer); cdecl;
    {class} function init: JGEDI_PRNTR_st_PictureConfig; cdecl; overload;//Deprecated
    {class} function init(P1: JGEDI_PRNTR_e_Alignment): JGEDI_PRNTR_st_PictureConfig; cdecl; overload;//Deprecated
    {class} function init(P1: JGEDI_PRNTR_e_Alignment; P2: Integer; P3: Integer; P4: Integer): JGEDI_PRNTR_st_PictureConfig; cdecl; overload;//Deprecated
    {class} property alignment: JGEDI_PRNTR_e_Alignment read _Getalignment write _Setalignment;
    {class} property height: Integer read _Getheight write _Setheight;
    {class} property offset: Integer read _Getoffset write _Setoffset;
    {class} property width: Integer read _Getwidth write _Setwidth;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_PRNTR_st_PictureConfig')]
  JGEDI_PRNTR_st_PictureConfig = interface(JObject)
    ['{CEF11D5B-32D1-4D99-977E-EA7087191649}']
    //Adicionado Manualmente
    {class} function _Getalignment: JGEDI_PRNTR_e_Alignment; cdecl;
    {class} procedure _Setalignment(Value: JGEDI_PRNTR_e_Alignment); cdecl;
    {class} function _Getheight: Integer; cdecl;
    {class} procedure _Setheight(Value: Integer); cdecl;
    {class} function _Getoffset: Integer; cdecl;
    {class} procedure _Setoffset(Value: Integer); cdecl;
    {class} function _Getwidth: Integer; cdecl;
    {class} procedure _Setwidth(Value: Integer); cdecl;
    {class} property alignment: JGEDI_PRNTR_e_Alignment read _Getalignment write _Setalignment;
    {class} property height: Integer read _Getheight write _Setheight;
    {class} property offset: Integer read _Getoffset write _Setoffset;
    {class} property width: Integer read _Getwidth write _Setwidth;

  end;
  TJGEDI_PRNTR_st_PictureConfig = class(TJavaGenericImport<JGEDI_PRNTR_st_PictureConfigClass, JGEDI_PRNTR_st_PictureConfig>) end;

  JGEDI_PRNTR_st_StringConfigClass = interface(JObjectClass)
    ['{940F8526-AC50-4E71-9B87-F9F96ADC7F9C}']
    {class} function _GetlineSpace: Integer; cdecl;
    {class} procedure _SetlineSpace(Value: Integer); cdecl;
    {class} function _Getoffset: Integer; cdecl;
    {class} procedure _Setoffset(Value: Integer); cdecl;
    {class} function _Getpaint: JPaint; cdecl;
    {class} procedure _Setpaint(Value: JPaint); cdecl;
    {class} function init: JGEDI_PRNTR_st_StringConfig; cdecl; overload;//Deprecated
    {class} function init(P1: JPaint): JGEDI_PRNTR_st_StringConfig; cdecl; overload;//Deprecated
    {class} function init(P1: JPaint; P2: Integer; P3: Integer): JGEDI_PRNTR_st_StringConfig; cdecl; overload;//Deprecated
    {class} property lineSpace: Integer read _GetlineSpace write _SetlineSpace;
    {class} property offset: Integer read _Getoffset write _Setoffset;
    {class} property paint: JPaint read _Getpaint write _Setpaint;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_PRNTR_st_StringConfig')]
  JGEDI_PRNTR_st_StringConfig = interface(JObject)
    ['{64DCF9AF-CBFD-47DD-90E3-61B412A20BC3}']
    //Adicionado por Marcos
        {class} function _GetlineSpace: Integer; cdecl;
    {class} procedure _SetlineSpace(Value: Integer); cdecl;
    {class} function _Getoffset: Integer; cdecl;
    {class} procedure _Setoffset(Value: Integer); cdecl;
    {class} function _Getpaint: JPaint; cdecl;
    {class} procedure _Setpaint(Value: JPaint); cdecl;
    {class} function init: JGEDI_PRNTR_st_StringConfig; cdecl; overload;//Deprecated
    {class} function init(P1: JPaint): JGEDI_PRNTR_st_StringConfig; cdecl; overload;//Deprecated
    {class} function init(P1: JPaint; P2: Integer; P3: Integer): JGEDI_PRNTR_st_StringConfig; cdecl; overload;//Deprecated
    {class} property lineSpace: Integer read _GetlineSpace write _SetlineSpace;
    {class} property offset: Integer read _Getoffset write _Setoffset;
    {class} property paint: JPaint read _Getpaint write _Setpaint;

  end;
  TJGEDI_PRNTR_st_StringConfig = class(TJavaGenericImport<JGEDI_PRNTR_st_StringConfigClass, JGEDI_PRNTR_st_StringConfig>) end;

  JGEDI_SMART_st_ResetInfoClass = interface(JObjectClass)
    ['{75373624-B2F5-4ED5-AD73-760E93642DC7}']
    {class} function _GetabATR: TJavaArray<Byte>; cdecl;
    {class} procedure _SetabATR(Value: TJavaArray<Byte>); cdecl;
    {class} function _GetpeCardType: JGEDI_SMART_e_Type; cdecl;
    {class} procedure _SetpeCardType(Value: JGEDI_SMART_e_Type); cdecl;
    {class} function init: JGEDI_SMART_st_ResetInfo; cdecl;//Deprecated
    {class} property abATR: TJavaArray<Byte> read _GetabATR write _SetabATR;
    {class} property peCardType: JGEDI_SMART_e_Type read _GetpeCardType write _SetpeCardType;
  end;

  [JavaSignature('br/com/gertec/gedi/structs/GEDI_SMART_st_ResetInfo')]
  JGEDI_SMART_st_ResetInfo = interface(JObject)
    ['{B53160CE-A3E4-4678-A719-E3C58B71ACEF}']
  end;
  TJGEDI_SMART_st_ResetInfo = class(TJavaGenericImport<JGEDI_SMART_st_ResetInfoClass, JGEDI_SMART_st_ResetInfo>) end;

  Jsdk4_a_aClass = interface(JIInterfaceClass)
    ['{18168F0D-211E-4F8E-8BE9-74D7D3AFAF89}']
    {class} function a: Integer; cdecl; overload;
    {class} function a(P1: TJavaArray<Byte>): Integer; cdecl; overload;
    {class} function a(P1: Ja_e): Integer; cdecl; overload;
    {class} function a(P1: JString): Integer; cdecl; overload;
    {class} function a(P1: Integer): Integer; cdecl; overload;
    {class} function a(P1: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: TJavaArray<Byte>; P2: Integer): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Ja_d): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: TJavaArray<Byte>): Integer; cdecl; overload;
    {class} function a(P1: TJavaArray<Byte>; P2: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer): Integer; cdecl; overload;
    {class} function a(P1: TJavaArray<Byte>; P2: TJavaArray<Byte>; P3: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: TJavaArray<Byte>; P3: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: Integer): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: TJavaArray<Byte>; P3: Integer): Integer; cdecl; overload;
    {class} function a(P1: JString; P2: Integer; P3: Boolean): Integer; cdecl; overload;
    {class} function a(P1: JBitmap; P2: Integer; P3: Integer): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: TJavaArray<Byte>): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: TJavaArray<Byte>; P3: TJavaArray<Byte>): Integer; cdecl; overload;
    {class} function a(P1: JString; P2: Integer; P3: Integer; P4: Integer): Integer; cdecl; overload;
    {class} function a(P1: TJavaArray<Byte>; P2: Integer; P3: TJavaArray<Byte>; P4: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: TJavaArray<Byte>; P4: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: Integer; P4: TJavaArray<Byte>): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: TJavaArray<Integer>; P4: TJavaArray<Byte>): Integer; cdecl; overload;
    {class} function a(P1: TJavaArray<Byte>; P2: TJavaArray<Integer>; P3: TJavaArray<Byte>; P4: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: TJavaArray<Byte>; P3: TJavaArray<Byte>; P4: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: JString; P2: JString; P3: Integer; P4: Integer): Integer; cdecl; overload;
    {class} function a(P1: JBitmap; P2: Integer; P3: Integer; P4: Integer; P5: Integer): Integer; cdecl; overload;
    {class} function a(P1: JString; P2: Integer; P3: Integer; P4: Integer; P5: Integer): Integer; cdecl; overload;
    {class} function a(P1: JString; P2: Integer; P3: Integer; P4: Boolean; P5: Boolean): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: TJavaArray<Byte>; P4: TJavaArray<Byte>; P5: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: TJavaArray<Byte>; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer): Integer; cdecl; overload;
    {class} function a(P1: Byte; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Integer>; P6: JString): Integer; cdecl; overload;
    {class} function a(P1: TJavaArray<Byte>; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Byte>; P6: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: TJavaArray<Byte>; P6: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Byte>; P6: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: Integer; P4: TJavaArray<Integer>; P5: TJavaArray<Byte>; P6: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Integer>; P6: JString): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: Integer; P6: TJavaArray<Byte>; P7: TJavaArray<Byte>): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: TJavaArray<Byte>; P6: TJavaArray<Byte>; P7: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Byte>; P6: TJavaArray<Byte>; P7: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: Integer): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Byte>; P6: TJavaArray<Byte>; P7: TJavaArray<Byte>; P8: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: Integer; P6: TJavaArray<Byte>; P7: TJavaArray<Byte>; P8: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: Byte; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Integer>; P6: Integer; P7: Integer; P8: Integer): Integer; cdecl; overload;
    {class} function a(P1: JString; P2: Integer; P3: TJavaArray<Byte>; P4: Integer; P5: TJavaArray<Byte>; P6: Integer; P7: TJavaArray<Byte>; P8: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: JString; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: Integer; P6: TJavaArray<Byte>; P7: Integer; P8: TJavaArray<Byte>; P9: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: JString; P2: JString; P3: Single; P4: Integer; P5: Integer; P6: Integer; P7: Boolean; P8: Boolean; P9: Boolean): Integer; cdecl; overload;
    {class} function a(P1: JString; P2: Integer; P3: Single; P4: Single; P5: Integer; P6: Integer; P7: Boolean; P8: Boolean; P9: Boolean): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: Integer; P6: TJavaArray<Byte>; P7: Integer; P8: TJavaArray<Byte>; P9: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Byte>; P6: TJavaArray<Byte>; P7: TJavaArray<Byte>; P8: Integer; P9: TJavaArray<Byte>; P10: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: JString; P2: Integer; P3: Single; P4: Single; P5: Integer; P6: Integer; P7: Integer; P8: Boolean; P9: Boolean; P10: Boolean): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: JString; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: TJavaArray<Byte>; P8: Integer; P9: TJavaArray<Byte>; P10: Ja_c): Integer; cdecl; overload;
    {class} function a(P1: JString; P2: Integer; P3: Single; P4: Single; P5: Single; P6: Integer; P7: Integer; P8: Integer; P9: Boolean; P10: Boolean; P11: Boolean): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: TJavaArray<Byte>; P7: Integer; P8: TJavaArray<Byte>; P9: Integer; P10: TJavaArray<Byte>; P11: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: JString; P4: Integer; P5: Integer; P6: TJavaArray<Byte>; P7: Integer; P8: TJavaArray<Byte>; P9: Integer; P10: TJavaArray<Byte>; P11: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: Byte; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: TJavaArray<Byte>; P7: Integer; P8: TJavaArray<Byte>; P9: Integer; P10: TJavaArray<Byte>; P11: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function b: Integer; cdecl; overload;
    {class} function b(P1: TJavaArray<Byte>): Integer; cdecl; overload;
    {class} function b(P1: Integer): Integer; cdecl; overload;
    {class} function b(P1: JString): Integer; cdecl; overload;
    {class} function b(P1: Integer; P2: Integer): Integer; cdecl; overload;
    {class} function b(P1: TJavaArray<Byte>; P2: Integer): Integer; cdecl; overload;
    {class} function b(P1: Integer; P2: TJavaArray<Byte>): Integer; cdecl; overload;
    {class} function b(P1: TJavaArray<Byte>; P2: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function b(P1: TJavaArray<Byte>; P2: TJavaArray<Byte>; P3: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function b(P1: Integer; P2: TJavaArray<Byte>; P3: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function b(P1: Integer; P2: Integer; P3: TJavaArray<Byte>; P4: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function b(P1: TJavaArray<Byte>; P2: TJavaArray<Integer>; P3: TJavaArray<Byte>; P4: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function b(P1: Integer; P2: TJavaArray<Byte>; P3: TJavaArray<Byte>; P4: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function b(P1: TJavaArray<Byte>; P2: Integer; P3: TJavaArray<Byte>; P4: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function b(P1: Integer; P2: TJavaArray<Byte>; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function b(P1: Integer; P2: Integer; P3: TJavaArray<Byte>; P4: TJavaArray<Byte>; P5: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function b(P1: Byte; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Integer>; P6: JString): Integer; cdecl; overload;
    {class} function b(P1: Integer; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Integer>; P6: JString): Integer; cdecl; overload;
    {class} function b(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: TJavaArray<Byte>; P6: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function b(P1: JString; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: Integer; P6: TJavaArray<Byte>; P7: Integer; P8: TJavaArray<Byte>; P9: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function b(P1: Integer; P2: JString; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: TJavaArray<Byte>; P8: Integer; P9: TJavaArray<Byte>; P10: Ja_c): Integer; cdecl; overload;
    {class} function b(P1: Integer; P2: Integer; P3: JString; P4: Integer; P5: Integer; P6: TJavaArray<Byte>; P7: Integer; P8: TJavaArray<Byte>; P9: Integer; P10: TJavaArray<Byte>; P11: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function c: Integer; cdecl; overload;
    {class} procedure c(P1: JString); cdecl; overload;
    {class} function c(P1: Integer): Integer; cdecl; overload;
    {class} function c(P1: TJavaArray<Byte>): Integer; cdecl; overload;
    {class} function c(P1: Integer; P2: TJavaArray<Byte>): Integer; cdecl; overload;
    {class} function c(P1: TJavaArray<Byte>; P2: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function c(P1: Integer; P2: Integer): Integer; cdecl; overload;
    {class} function c(P1: TJavaArray<Byte>; P2: Integer): Integer; cdecl; overload;
    {class} function c(P1: Integer; P2: TJavaArray<Byte>; P3: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function c(P1: Integer; P2: Integer; P3: TJavaArray<Byte>; P4: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function c(P1: TJavaArray<Byte>; P2: Integer; P3: TJavaArray<Byte>; P4: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function c(P1: Integer; P2: TJavaArray<Byte>; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function d: Integer; cdecl; overload;
    {class} function d(P1: TJavaArray<Byte>): Integer; cdecl; overload;
    {class} function d(P1: Integer): Integer; cdecl; overload;
    {class} function d(P1: Integer; P2: Integer): Integer; cdecl; overload;
    {class} function d(P1: TJavaArray<Byte>; P2: Integer): Integer; cdecl; overload;
    {class} function d(P1: TJavaArray<Byte>; P2: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function d(P1: Integer; P2: TJavaArray<Byte>): Integer; cdecl; overload;
    {class} function d(P1: Integer; P2: TJavaArray<Byte>; P3: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function d(P1: TJavaArray<Byte>; P2: Integer; P3: TJavaArray<Byte>; P4: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function e: Integer; cdecl; overload;
    {class} function e(P1: Integer): Integer; cdecl; overload;
    {class} procedure e(P1: TJavaArray<Byte>); cdecl; overload;
    {class} function e(P1: TJavaArray<Byte>; P2: Integer): Integer; cdecl; overload;
    {class} function e(P1: Integer; P2: Integer): Integer; cdecl; overload;
    {class} function e(P1: TJavaArray<Byte>; P2: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function f: Integer; cdecl; overload;
    {class} function f(P1: TJavaArray<Byte>): Integer; cdecl; overload;
    {class} function f(P1: Integer): Integer; cdecl; overload;
    {class} function f(P1: TJavaArray<Byte>; P2: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function g: Integer; cdecl; overload;
    {class} function g(P1: Integer): Integer; cdecl; overload;
    {class} function g(P1: TJavaArray<Byte>; P2: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function h: Integer; cdecl; overload;
    {class} function h(P1: Integer): Integer; cdecl; overload;
    {class} function h(P1: TJavaArray<Byte>; P2: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function i: Integer; cdecl; overload;
    {class} function i(P1: TJavaArray<Byte>; P2: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function j: TJavaObjectArray<JString>; cdecl; overload;
    {class} function j(P1: TJavaArray<Byte>; P2: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function k: Integer; cdecl; overload;
    {class} function k(P1: TJavaArray<Byte>; P2: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function l: Integer; cdecl; overload;
    {class} function l(P1: TJavaArray<Byte>; P2: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function m: Integer; cdecl; overload;
    {class} function m(P1: TJavaArray<Byte>; P2: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function n: Integer; cdecl; overload;
    {class} function n(P1: TJavaArray<Byte>; P2: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function o: JString; cdecl;
    {class} function p: JString; cdecl;
    {class} function q: Integer; cdecl;
    {class} function r: Integer; cdecl;
    {class} function s: JString; cdecl;
  end;

  [JavaSignature('wangpos/sdk4/a/a')]
  Jsdk4_a_a = interface(JIInterface)
    ['{000A71BE-7483-4972-B361-60DFD8C59283}']
  end;
  TJsdk4_a_a = class(TJavaGenericImport<Jsdk4_a_aClass, Jsdk4_a_a>) end;

  Jsdk4_a_a_aClass = interface(JBinderClass)
    ['{C58A8366-F8AF-47A7-9282-0E0E8DB4A26D}']
    {class} function a(P1: JIBinder): Jsdk4_a_a; cdecl;
    {class} function onTransact(P1: Integer; P2: JParcel; P3: JParcel; P4: Integer): Boolean; cdecl;
  end;

  [JavaSignature('wangpos/sdk4/a/a$a')]
  Jsdk4_a_a_a = interface(JBinder)
    ['{39C40CCB-D171-4D29-BE07-1B52747B2096}']
  end;
  TJsdk4_a_a_a = class(TJavaGenericImport<Jsdk4_a_a_aClass, Jsdk4_a_a_a>) end;

  Ja_a_aClass = interface(Jsdk4_a_aClass)
    ['{FFCD5985-7C17-410A-A897-F6377BDAFD1C}']
    {class} function a: Integer; cdecl; overload;
    {class} function a(P1: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: Integer): Integer; cdecl; overload;
    {class} function a(P1: TJavaArray<Byte>): Integer; cdecl; overload;
    {class} function a(P1: JString): Integer; cdecl; overload;
    {class} function a(P1: Ja_e): Integer; cdecl; overload;
    {class} function a(P1: TJavaArray<Byte>; P2: Integer): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Ja_d): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: TJavaArray<Byte>): Integer; cdecl; overload;
    {class} function a(P1: TJavaArray<Byte>; P2: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: TJavaArray<Byte>): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: TJavaArray<Byte>; P3: Integer): Integer; cdecl; overload;
    {class} function a(P1: TJavaArray<Byte>; P2: TJavaArray<Byte>; P3: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: Integer): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: TJavaArray<Byte>; P3: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: JString; P2: Integer; P3: Boolean): Integer; cdecl; overload;
    {class} function a(P1: JBitmap; P2: Integer; P3: Integer): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: TJavaArray<Byte>; P3: TJavaArray<Byte>): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: TJavaArray<Byte>; P3: TJavaArray<Byte>; P4: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: TJavaArray<Byte>; P2: Integer; P3: TJavaArray<Byte>; P4: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: TJavaArray<Integer>; P4: TJavaArray<Byte>): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: TJavaArray<Byte>; P4: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: TJavaArray<Byte>; P2: TJavaArray<Integer>; P3: TJavaArray<Byte>; P4: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: JString; P2: JString; P3: Integer; P4: Integer): Integer; cdecl; overload;
    {class} function a(P1: JString; P2: Integer; P3: Integer; P4: Integer): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: Integer; P4: TJavaArray<Byte>): Integer; cdecl; overload;
    {class} function a(P1: JBitmap; P2: Integer; P3: Integer; P4: Integer; P5: Integer): Integer; cdecl; overload;
    {class} function a(P1: JString; P2: Integer; P3: Integer; P4: Integer; P5: Integer): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer): Integer; cdecl; overload;
    {class} function a(P1: JString; P2: Integer; P3: Integer; P4: Boolean; P5: Boolean): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: TJavaArray<Byte>; P4: TJavaArray<Byte>; P5: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: TJavaArray<Byte>; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: TJavaArray<Byte>; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Byte>; P6: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: TJavaArray<Byte>; P6: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: Byte; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Integer>; P6: JString): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: Integer; P4: TJavaArray<Integer>; P5: TJavaArray<Byte>; P6: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Byte>; P6: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Integer>; P6: JString): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Byte>; P6: TJavaArray<Byte>; P7: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: Integer): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: TJavaArray<Byte>; P6: TJavaArray<Byte>; P7: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: Integer; P6: TJavaArray<Byte>; P7: TJavaArray<Byte>): Integer; cdecl; overload;
    {class} function a(P1: Byte; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Integer>; P6: Integer; P7: Integer; P8: Integer): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Byte>; P6: TJavaArray<Byte>; P7: TJavaArray<Byte>; P8: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: Integer; P6: TJavaArray<Byte>; P7: TJavaArray<Byte>; P8: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: JString; P2: Integer; P3: TJavaArray<Byte>; P4: Integer; P5: TJavaArray<Byte>; P6: Integer; P7: TJavaArray<Byte>; P8: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: JString; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: Integer; P6: TJavaArray<Byte>; P7: Integer; P8: TJavaArray<Byte>; P9: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: JString; P2: JString; P3: Single; P4: Integer; P5: Integer; P6: Integer; P7: Boolean; P8: Boolean; P9: Boolean): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: Integer; P6: TJavaArray<Byte>; P7: Integer; P8: TJavaArray<Byte>; P9: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: JString; P2: Integer; P3: Single; P4: Single; P5: Integer; P6: Integer; P7: Boolean; P8: Boolean; P9: Boolean): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: JString; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: TJavaArray<Byte>; P8: Integer; P9: TJavaArray<Byte>; P10: Ja_c): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Byte>; P6: TJavaArray<Byte>; P7: TJavaArray<Byte>; P8: Integer; P9: TJavaArray<Byte>; P10: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: JString; P2: Integer; P3: Single; P4: Single; P5: Integer; P6: Integer; P7: Integer; P8: Boolean; P9: Boolean; P10: Boolean): Integer; cdecl; overload;
    {class} function a(P1: JString; P2: Integer; P3: Single; P4: Single; P5: Single; P6: Integer; P7: Integer; P8: Integer; P9: Boolean; P10: Boolean; P11: Boolean): Integer; cdecl; overload;
    {class} function a(P1: Byte; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: TJavaArray<Byte>; P7: Integer; P8: TJavaArray<Byte>; P9: Integer; P10: TJavaArray<Byte>; P11: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: TJavaArray<Byte>; P7: Integer; P8: TJavaArray<Byte>; P9: Integer; P10: TJavaArray<Byte>; P11: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: JString; P4: Integer; P5: Integer; P6: TJavaArray<Byte>; P7: Integer; P8: TJavaArray<Byte>; P9: Integer; P10: TJavaArray<Byte>; P11: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function asBinder: JIBinder; cdecl;
    {class} function b: Integer; cdecl; overload;
    {class} function b(P1: TJavaArray<Byte>): Integer; cdecl; overload;
    {class} function b(P1: Integer): Integer; cdecl; overload;
    {class} function b(P1: JString): Integer; cdecl; overload;
    {class} function b(P1: TJavaArray<Byte>; P2: Integer): Integer; cdecl; overload;
    {class} function b(P1: Integer; P2: TJavaArray<Byte>): Integer; cdecl; overload;
    {class} function b(P1: Integer; P2: Integer): Integer; cdecl; overload;
    {class} function b(P1: TJavaArray<Byte>; P2: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function b(P1: Integer; P2: TJavaArray<Byte>; P3: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function b(P1: TJavaArray<Byte>; P2: TJavaArray<Byte>; P3: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function b(P1: Integer; P2: TJavaArray<Byte>; P3: TJavaArray<Byte>; P4: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function b(P1: TJavaArray<Byte>; P2: Integer; P3: TJavaArray<Byte>; P4: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function b(P1: Integer; P2: Integer; P3: TJavaArray<Byte>; P4: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function b(P1: TJavaArray<Byte>; P2: TJavaArray<Integer>; P3: TJavaArray<Byte>; P4: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function b(P1: Integer; P2: TJavaArray<Byte>; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function b(P1: Integer; P2: Integer; P3: TJavaArray<Byte>; P4: TJavaArray<Byte>; P5: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function b(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: TJavaArray<Byte>; P6: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function b(P1: Byte; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Integer>; P6: JString): Integer; cdecl; overload;
    {class} function b(P1: Integer; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Integer>; P6: JString): Integer; cdecl; overload;
    {class} function b(P1: JString; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: Integer; P6: TJavaArray<Byte>; P7: Integer; P8: TJavaArray<Byte>; P9: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function b(P1: Integer; P2: JString; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: TJavaArray<Byte>; P8: Integer; P9: TJavaArray<Byte>; P10: Ja_c): Integer; cdecl; overload;
    {class} function b(P1: Integer; P2: Integer; P3: JString; P4: Integer; P5: Integer; P6: TJavaArray<Byte>; P7: Integer; P8: TJavaArray<Byte>; P9: Integer; P10: TJavaArray<Byte>; P11: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function c: Integer; cdecl; overload;
    {class} procedure c(P1: JString); cdecl; overload;
    {class} function c(P1: TJavaArray<Byte>): Integer; cdecl; overload;
    {class} function c(P1: Integer): Integer; cdecl; overload;
    {class} function c(P1: Integer; P2: TJavaArray<Byte>): Integer; cdecl; overload;
    {class} function c(P1: TJavaArray<Byte>; P2: Integer): Integer; cdecl; overload;
    {class} function c(P1: Integer; P2: Integer): Integer; cdecl; overload;
    {class} function c(P1: TJavaArray<Byte>; P2: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function c(P1: Integer; P2: TJavaArray<Byte>; P3: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function c(P1: Integer; P2: Integer; P3: TJavaArray<Byte>; P4: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function c(P1: TJavaArray<Byte>; P2: Integer; P3: TJavaArray<Byte>; P4: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function c(P1: Integer; P2: TJavaArray<Byte>; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function d: Integer; cdecl; overload;
    {class} function d(P1: Integer): Integer; cdecl; overload;
    {class} function d(P1: TJavaArray<Byte>): Integer; cdecl; overload;
    {class} function d(P1: TJavaArray<Byte>; P2: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function d(P1: Integer; P2: Integer): Integer; cdecl; overload;
    {class} function d(P1: Integer; P2: TJavaArray<Byte>): Integer; cdecl; overload;
    {class} function d(P1: TJavaArray<Byte>; P2: Integer): Integer; cdecl; overload;
    {class} function d(P1: Integer; P2: TJavaArray<Byte>; P3: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function d(P1: TJavaArray<Byte>; P2: Integer; P3: TJavaArray<Byte>; P4: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function e: Integer; cdecl; overload;
    {class} procedure e(P1: TJavaArray<Byte>); cdecl; overload;
    {class} function e(P1: Integer): Integer; cdecl; overload;
    {class} function e(P1: Integer; P2: Integer): Integer; cdecl; overload;
    {class} function e(P1: TJavaArray<Byte>; P2: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function e(P1: TJavaArray<Byte>; P2: Integer): Integer; cdecl; overload;
    {class} function f: Integer; cdecl; overload;
    {class} function f(P1: Integer): Integer; cdecl; overload;
    {class} function f(P1: TJavaArray<Byte>): Integer; cdecl; overload;
    {class} function f(P1: TJavaArray<Byte>; P2: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function g: Integer; cdecl; overload;
    {class} function g(P1: Integer): Integer; cdecl; overload;
    {class} function g(P1: TJavaArray<Byte>; P2: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function h: Integer; cdecl; overload;
    {class} function h(P1: Integer): Integer; cdecl; overload;
    {class} function h(P1: TJavaArray<Byte>; P2: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function i: Integer; cdecl; overload;
    {class} function i(P1: TJavaArray<Byte>; P2: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function j: TJavaObjectArray<JString>; cdecl; overload;
    {class} function j(P1: TJavaArray<Byte>; P2: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function k: Integer; cdecl; overload;
    {class} function k(P1: TJavaArray<Byte>; P2: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function l: Integer; cdecl; overload;
    {class} function l(P1: TJavaArray<Byte>; P2: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function m: Integer; cdecl; overload;
    {class} function m(P1: TJavaArray<Byte>; P2: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function n: Integer; cdecl; overload;
    {class} function n(P1: TJavaArray<Byte>; P2: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function o: JString; cdecl;
    {class} function p: JString; cdecl;
    {class} function q: Integer; cdecl;
    {class} function r: Integer; cdecl;
    {class} function s: JString; cdecl;
  end;

  [JavaSignature('wangpos/sdk4/a/a$a$a')]
  Ja_a_a = interface(Jsdk4_a_a)
    ['{E68E2139-7A32-4D97-8C75-B76312E54855}']
  end;
  TJa_a_a = class(TJavaGenericImport<Ja_a_aClass, Ja_a_a>) end;

  Ja_bClass = interface(JIInterfaceClass)
    ['{7D9D6ECC-C564-4EE2-B568-2E9DE129311A}']
    {class} function a(P1: Integer): JIBinder; cdecl;
  end;

  [JavaSignature('wangpos/sdk4/a/b')]
  Ja_b = interface(JIInterface)
    ['{A1483B33-C26A-4BB4-9D0A-9BB5D27EAD16}']
  end;
  TJa_b = class(TJavaGenericImport<Ja_bClass, Ja_b>) end;

  Jb_aClass = interface(JBinderClass)
    ['{CF3386E9-27E2-4DF7-9469-F135130F49E4}']
    {class} function a(P1: JIBinder): Ja_b; cdecl;
    {class} function onTransact(P1: Integer; P2: JParcel; P3: JParcel; P4: Integer): Boolean; cdecl;
  end;

  [JavaSignature('wangpos/sdk4/a/b$a')]
  Jb_a = interface(JBinder)
    ['{1BB88E4C-62D9-44B3-9806-150828551280}']
  end;
  TJb_a = class(TJavaGenericImport<Jb_aClass, Jb_a>) end;

  Jb_a_aClass = interface(Ja_bClass)
    ['{D5907A76-C756-43EB-A91A-B9100BE722E5}']
    {class} function a(P1: Integer): JIBinder; cdecl;
    {class} function asBinder: JIBinder; cdecl;
  end;

  [JavaSignature('wangpos/sdk4/a/b$a$a')]
  Jb_a_a = interface(Ja_b)
    ['{C1983BED-9FD3-46A8-AD87-5A40DA3AED5A}']
  end;
  TJb_a_a = class(TJavaGenericImport<Jb_a_aClass, Jb_a_a>) end;

  Ja_cClass = interface(JIInterfaceClass)
    ['{3F83E9CD-6B81-4703-B103-47C3210D350A}']
    {class} function a(P1: Integer; P2: TJavaArray<Byte>; P3: TJavaArray<Byte>; P4: TJavaArray<Integer>): Integer; cdecl;
  end;

  [JavaSignature('wangpos/sdk4/a/c')]
  Ja_c = interface(JIInterface)
    ['{8B8F73AF-1B6E-4331-8CB9-EE6F845E5776}']
  end;
  TJa_c = class(TJavaGenericImport<Ja_cClass, Ja_c>) end;

  Jc_a_aClass = interface(Ja_cClass)
    ['{F69CFD42-6127-4217-9002-C6A924D2B7DF}']
    {class} function a(P1: Integer; P2: TJavaArray<Byte>; P3: TJavaArray<Byte>; P4: TJavaArray<Integer>): Integer; cdecl;
    {class} function asBinder: JIBinder; cdecl;
  end;

  [JavaSignature('wangpos/sdk4/a/c$a$a')]
  Jc_a_a = interface(Ja_c)
    ['{36718126-15E3-408E-A836-29713D5E02A7}']
  end;
  TJc_a_a = class(TJavaGenericImport<Jc_a_aClass, Jc_a_a>) end;

  Ja_dClass = interface(JIInterfaceClass)
    ['{76A13544-FE94-4850-97B5-D3F231B731D2}']
    {class} function a(P1: Integer): Integer; cdecl;
  end;

  [JavaSignature('wangpos/sdk4/a/d')]
  Ja_d = interface(JIInterface)
    ['{3C65DC0F-DF52-4D06-8BFD-0F9372DA5D77}']
  end;
  TJa_d = class(TJavaGenericImport<Ja_dClass, Ja_d>) end;

  Jd_aClass = interface(JBinderClass)
    ['{C5836210-E8C8-4657-AA4D-370A377A22B2}']
    {class} function a(P1: JIBinder): Ja_d; cdecl;
    {class} function onTransact(P1: Integer; P2: JParcel; P3: JParcel; P4: Integer): Boolean; cdecl;
  end;

  [JavaSignature('wangpos/sdk4/a/d$a')]
  Jd_a = interface(JBinder)
    ['{F005A4E5-96B4-4E7B-9DD5-D8DB7C291FCE}']
  end;
  TJd_a = class(TJavaGenericImport<Jd_aClass, Jd_a>) end;

  Jd_a_aClass = interface(Ja_dClass)
    ['{84AFB618-E638-48DC-93E3-8B77BCCBD48B}']
    {class} function a(P1: Integer): Integer; cdecl;
    {class} function asBinder: JIBinder; cdecl;
  end;

  [JavaSignature('wangpos/sdk4/a/d$a$a')]
  Jd_a_a = interface(Ja_d)
    ['{58ECBC9A-01FA-4FFF-8F9A-A1E6FE463059}']
  end;
  TJd_a_a = class(TJavaGenericImport<Jd_a_aClass, Jd_a_a>) end;

  Ja_eClass = interface(JIInterfaceClass)
    ['{74A3BBF2-F162-47A7-99F7-3589379367D3}']
    {class} function a(P1: Integer): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: JString; P3: Byte; P4: Byte; P5: Byte; P6: Integer; P7: TJavaArray<Byte>): Integer; cdecl; overload;
  end;

  [JavaSignature('wangpos/sdk4/a/e')]
  Ja_e = interface(JIInterface)
    ['{3A98551D-29B5-408C-8ED2-6E5490F1BB05}']
  end;
  TJa_e = class(TJavaGenericImport<Ja_eClass, Ja_e>) end;

  Je_aClass = interface(JBinderClass)
    ['{0BDE8C70-A03C-422C-867A-BEAE3B288B14}']
    {class} function a(P1: JIBinder): Ja_e; cdecl;
    {class} function onTransact(P1: Integer; P2: JParcel; P3: JParcel; P4: Integer): Boolean; cdecl;
  end;

  [JavaSignature('wangpos/sdk4/a/e$a')]
  Je_a = interface(JBinder)
    ['{272BBB94-ABF9-42FB-B829-29B57D409759}']
  end;
  TJe_a = class(TJavaGenericImport<Je_aClass, Je_a>) end;

  Je_a_aClass = interface(Ja_eClass)
    ['{1D33D21A-5FCD-477A-88D0-506F8D878C70}']
    {class} function a(P1: Integer): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: JString; P3: Byte; P4: Byte; P5: Byte; P6: Integer; P7: TJavaArray<Byte>): Integer; cdecl; overload;
    {class} function asBinder: JIBinder; cdecl;
  end;

  [JavaSignature('wangpos/sdk4/a/e$a$a')]
  Je_a_a = interface(Ja_e)
    ['{3FC5AECB-1EE2-49CE-B5EC-0BC7EFA06782}']
  end;
  TJe_a_a = class(TJavaGenericImport<Je_a_aClass, Je_a_a>) end;

  Jsdk4_b_aClass = interface(JIInterfaceClass)
    ['{5B6F0C8F-9FEF-4B5A-984D-7F3FEA90A85B}']
    {class} function a(P1: Integer): JIBinder; cdecl;
  end;

  [JavaSignature('wangpos/sdk4/b/a')]
  Jsdk4_b_a = interface(JIInterface)
    ['{C590A0FD-335F-43EA-A595-A2A888B14284}']
  end;
  TJsdk4_b_a = class(TJavaGenericImport<Jsdk4_b_aClass, Jsdk4_b_a>) end;

  Jsdk4_b_a_aClass = interface(JBinderClass)
    ['{A670E904-FE56-45FA-88B1-608347664172}']
    {class} function a(P1: JIBinder): Jsdk4_b_a; cdecl;
    {class} function onTransact(P1: Integer; P2: JParcel; P3: JParcel; P4: Integer): Boolean; cdecl;
  end;

  [JavaSignature('wangpos/sdk4/b/a$a')]
  Jsdk4_b_a_a = interface(JBinder)
    ['{26F10035-03A6-4903-8E46-A3A0D52CF742}']
  end;
  TJsdk4_b_a_a = class(TJavaGenericImport<Jsdk4_b_a_aClass, Jsdk4_b_a_a>) end;

  Jb_a_a_aClass = interface(Jsdk4_b_aClass)
    ['{FE79EB05-1CB7-446D-A3AC-7A2D809D194C}']
    {class} function a(P1: Integer): JIBinder; cdecl;
    {class} function asBinder: JIBinder; cdecl;
  end;

  [JavaSignature('wangpos/sdk4/b/a$a$a')]
  Jb_a_a_a = interface(Jsdk4_b_a)
    ['{1CCC9F4B-64FF-4838-8DCB-92A43C72793A}']
  end;
  TJb_a_a_a = class(TJavaGenericImport<Jb_a_a_aClass, Jb_a_a_a>) end;

  Jb_bClass = interface(JIInterfaceClass)
    ['{D3566A68-AFEB-44A7-90F7-7F6E5B725985}']
    {class} function a: Integer; cdecl; overload;
    {class} procedure a(P1: JString); cdecl; overload;
    {class} function a(P1: JString; P2: Integer): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: JString): Integer; cdecl; overload;
    {class} function a(P1: JString; P2: Integer; P3: TJavaArray<Byte>): Integer; cdecl; overload;
    {class} function a(P1: JString; P2: TJavaArray<Byte>; P3: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: JString; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: TJavaArray<Byte>; P6: TJavaArray<Byte>; P7: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: JString; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Byte>; P6: Integer; P7: TJavaArray<Byte>; P8: Boolean; P9: Integer): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: TJavaArray<Byte>; P4: TJavaArray<Byte>; P5: Boolean; P6: Integer; P7: TJavaArray<Byte>; P8: JString; P9: Integer): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: TJavaArray<Byte>; P4: TJavaArray<Byte>; P5: Boolean; P6: Integer; P7: TJavaArray<Byte>; P8: JString; P9: JString; P10: Integer): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Byte>; P6: Boolean; P7: Integer; P8: TJavaArray<Byte>; P9: JString; P10: Integer): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Byte>; P6: Boolean; P7: Integer; P8: TJavaArray<Byte>; P9: JString; P10: JString; P11: Integer): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Byte>; P6: Integer; P7: Integer; P8: TJavaArray<Byte>; P9: TJavaArray<Byte>; P10: Integer; P11: Integer; P12: TJavaArray<Byte>; P13: TJavaArray<Byte>): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: TJavaArray<Byte>; P8: Integer; P9: TJavaArray<Byte>; P10: Integer; P11: Integer; P12: TJavaArray<Byte>; P13: Integer; P14: TJavaArray<Byte>; P15: TJavaArray<Byte>; P16: JString): Integer; cdecl; overload;
    {class} function b: Integer; cdecl; overload;
    {class} function b(P1: JString): Integer; cdecl; overload;
    {class} function c: JString; cdecl;
  end;

  [JavaSignature('wangpos/sdk4/b/b')]
  Jb_b = interface(JIInterface)
    ['{BFF1E39C-0411-4943-AA81-DAAD69C337ED}']
  end;
  TJb_b = class(TJavaGenericImport<Jb_bClass, Jb_b>) end;

  Jb_b_aClass = interface(JBinderClass)
    ['{F2DCD89D-4652-467C-BB3F-8276BE98D03E}']
    {class} function a(P1: JIBinder): Jb_b; cdecl;
    {class} function onTransact(P1: Integer; P2: JParcel; P3: JParcel; P4: Integer): Boolean; cdecl;
  end;

  [JavaSignature('wangpos/sdk4/b/b$a')]
  Jb_b_a = interface(JBinder)
    ['{070AB257-10FC-4B93-A215-E9625C9D17B0}']
  end;
  TJb_b_a = class(TJavaGenericImport<Jb_b_aClass, Jb_b_a>) end;

  Jb_b_a_aClass = interface(Jb_bClass)
    ['{08C8F0EB-69C3-469F-89C7-0229F69A6624}']
    {class} function a: Integer; cdecl; overload;
    {class} procedure a(P1: JString); cdecl; overload;
    {class} function a(P1: JString; P2: Integer): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: JString): Integer; cdecl; overload;
    {class} function a(P1: JString; P2: TJavaArray<Byte>; P3: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: JString; P2: Integer; P3: TJavaArray<Byte>): Integer; cdecl; overload;
    {class} function a(P1: JString; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: TJavaArray<Byte>; P6: TJavaArray<Byte>; P7: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: JString; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Byte>; P6: Integer; P7: TJavaArray<Byte>; P8: Boolean; P9: Integer): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: TJavaArray<Byte>; P4: TJavaArray<Byte>; P5: Boolean; P6: Integer; P7: TJavaArray<Byte>; P8: JString; P9: Integer): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: TJavaArray<Byte>; P4: TJavaArray<Byte>; P5: Boolean; P6: Integer; P7: TJavaArray<Byte>; P8: JString; P9: JString; P10: Integer): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Byte>; P6: Boolean; P7: Integer; P8: TJavaArray<Byte>; P9: JString; P10: Integer): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Byte>; P6: Boolean; P7: Integer; P8: TJavaArray<Byte>; P9: JString; P10: JString; P11: Integer): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Byte>; P6: Integer; P7: Integer; P8: TJavaArray<Byte>; P9: TJavaArray<Byte>; P10: Integer; P11: Integer; P12: TJavaArray<Byte>; P13: TJavaArray<Byte>): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: TJavaArray<Byte>; P8: Integer; P9: TJavaArray<Byte>; P10: Integer; P11: Integer; P12: TJavaArray<Byte>; P13: Integer; P14: TJavaArray<Byte>; P15: TJavaArray<Byte>; P16: JString): Integer; cdecl; overload;
    {class} function asBinder: JIBinder; cdecl;
    {class} function b: Integer; cdecl; overload;
    {class} function b(P1: JString): Integer; cdecl; overload;
    {class} function c: JString; cdecl;
  end;

  [JavaSignature('wangpos/sdk4/b/b$a$a')]
  Jb_b_a_a = interface(Jb_b)
    ['{14B67E87-950C-44F6-A056-3C1E6FCB791F}']
  end;
  TJb_b_a_a = class(TJavaGenericImport<Jb_b_a_aClass, Jb_b_a_a>) end;

  Jsdk4_c_aClass = interface(JObjectClass)
    ['{8B418E01-4370-4146-9CE1-94A4676EBADD}']
    {class} function a(P1: Jsdk4_c_a): JIBinder_DeathRecipient; cdecl; overload;//Deprecated
    {class} function a(P1: Jsdk4_b_a): Jsdk4_b_a; cdecl; overload;//Deprecated
    {class} function a(P1: Integer): JIBinder; cdecl; overload;//Deprecated
    {class} function a(P1: JContext): Jsdk4_c_a; cdecl; overload;//Deprecated
    {class} function b: Jsdk4_b_a; cdecl; overload;//Deprecated
    {class} function b(P1: Jsdk4_c_a): JCountDownLatch; cdecl; overload;//Deprecated
    {class} procedure c(P1: Jsdk4_c_a); cdecl;//Deprecated
    {class} function init: Jsdk4_c_a; cdecl;//Deprecated
  end;

  [JavaSignature('wangpos/sdk4/c/a')]
  Jsdk4_c_a = interface(JObject)
    ['{5ED25545-B8EF-4124-9F85-83290A409D78}']
  end;
  TJsdk4_c_a = class(TJavaGenericImport<Jsdk4_c_aClass, Jsdk4_c_a>) end;

  Jsdk4_c_a_aClass = interface(JServiceConnectionClass)
    ['{B9621781-401A-4CA9-8704-F051503B2E40}']
    {class} function init(P1: Jsdk4_c_a): Jsdk4_c_a_a; cdecl;//Deprecated
    {class} procedure onServiceConnected(P1: JComponentName; P2: JIBinder); cdecl;//Deprecated
    {class} procedure onServiceDisconnected(P1: JComponentName); cdecl;//Deprecated
  end;

  [JavaSignature('wangpos/sdk4/c/a$a')]
  Jsdk4_c_a_a = interface(JServiceConnection)
    ['{B0BB00C7-7104-48DE-8873-DE3F56567802}']
  end;
  TJsdk4_c_a_a = class(TJavaGenericImport<Jsdk4_c_a_aClass, Jsdk4_c_a_a>) end;

  Jc_a_bClass = interface(JIBinder_DeathRecipientClass)
    ['{B069B328-3E7D-4BC3-858E-29DACB059811}']
    {class} procedure binderDied; cdecl;//Deprecated
    {class} function init(P1: Jsdk4_c_a): Jc_a_b; cdecl;//Deprecated
  end;

  [JavaSignature('wangpos/sdk4/c/a$b')]
  Jc_a_b = interface(JIBinder_DeathRecipient)
    ['{7BD6BACE-9B38-4E5A-95CD-39324B58D56B}']
  end;
  TJc_a_b = class(TJavaGenericImport<Jc_a_bClass, Jc_a_b>) end;

  Jc_bClass = interface(Jsdk4_c_aClass)
    ['{B2EFF4AB-99B2-4084-A07D-E383D66607AA}']
    {class} function a: Integer; cdecl; overload;
    {class} procedure a(P1: JString); cdecl; overload;
    {class} function a(P1: JString; P2: Integer): Integer; cdecl; overload;
    {class} function a(P1: JString; P2: TJavaArray<Byte>; P3: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: JString; P2: Integer; P3: TJavaArray<Byte>): Integer; cdecl; overload;
    {class} function a(P1: JString; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Integer>): Integer; cdecl; overload;
    {class} function a(P1: Integer; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Byte>; P6: Boolean; P7: Integer; P8: TJavaArray<Byte>; P9: JString; P10: Integer): Integer; cdecl; overload;
    {class} function b(P1: JString): Integer; cdecl; overload;
    {class} function b(P1: Integer; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Byte>; P6: Boolean; P7: Integer; P8: TJavaArray<Byte>; P9: JString; P10: Integer): Integer; cdecl; overload;
    {class} function init(P1: JContext): Jc_b; cdecl;
  end;

  [JavaSignature('wangpos/sdk4/c/b')]
  Jc_b = interface(Jsdk4_c_a)
    ['{FBEED163-E952-4E69-9277-3900421ADE0D}']
  end;
  TJc_b = class(TJavaGenericImport<Jc_bClass, Jc_b>) end;

  Jlibbasebinder_aClass = interface(JObjectClass)
    ['{8D602C04-F988-42C2-9E66-48EE477144D1}']
    {class} function _GetBINDER_NONE: Integer; cdecl;
    {class} function _GetmBinderConnected: Boolean; cdecl;
    {class} procedure _SetmBinderConnected(Value: Boolean); cdecl;
    {class} function getInstance(P1: JContext): Jlibbasebinder_a; cdecl;//Deprecated
    {class} function init: Jlibbasebinder_a; cdecl;//Deprecated
    {class} function queryBinder(P1: Integer): JIBinder; cdecl;//Deprecated
    {class} property BINDER_NONE: Integer read _GetBINDER_NONE;
    {class} property mBinderConnected: Boolean read _GetmBinderConnected write _SetmBinderConnected;
  end;

  [JavaSignature('wangpos/sdk4/libbasebinder/a')]
  Jlibbasebinder_a = interface(JObject)
    ['{A7CBDCAD-5E05-4E97-AEA4-E31247863EC5}']
  end;
  TJlibbasebinder_a = class(TJavaGenericImport<Jlibbasebinder_aClass, Jlibbasebinder_a>) end;

  JBankCardClass = interface(Jlibbasebinder_aClass)
    ['{C96E4F9A-9314-45EC-AD3F-35E46E0AD3FA}']
    {class} function _GetCARD_DETECT_EXIST: Integer; cdecl;
    {class} function _GetCARD_DETECT_NOTEXIST: Integer; cdecl;
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
    {class} function CLGetVersion(P1: TJavaArray<Byte>; P2: TJavaArray<Integer>; P3: TJavaArray<Byte>; P4: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function CardActivation(P1: TJavaArray<Byte>; P2: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function DesFire_Auth(P1: Integer; P2: Integer; P3: Integer; P4: TJavaArray<Byte>): Integer; cdecl;//Deprecated
    {class} function DesFire_Comfirm_Cancel(P1: Integer; P2: TJavaArray<Byte>; P3: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function DesFire_CreatApp(P1: Integer; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Byte>; P6: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function DesFire_CreatFile(P1: Integer; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Byte>; P6: TJavaArray<Byte>; P7: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function DesFire_CreatLine_CycleFile(P1: Integer; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Byte>; P6: TJavaArray<Byte>; P7: TJavaArray<Byte>; P8: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function DesFire_CreatValueFile(P1: Integer; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Byte>; P6: TJavaArray<Byte>; P7: TJavaArray<Byte>; P8: Integer; P9: TJavaArray<Byte>; P10: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function DesFire_DelFile(P1: Integer; P2: TJavaArray<Byte>; P3: TJavaArray<Byte>; P4: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function DesFire_GetCardInfo(P1: Integer; P2: Integer; P3: TJavaArray<Byte>; P4: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function DesFire_ISO7816(P1: TJavaArray<Byte>; P2: TJavaArray<Byte>; P3: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function DesFire_ReadFile(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: TJavaArray<Byte>; P6: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function DesFire_SelApp(P1: Integer; P2: TJavaArray<Byte>; P3: TJavaArray<Byte>; P4: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function DesFire_ValueFileOpr(P1: Integer; P2: Integer; P3: TJavaArray<Byte>; P4: TJavaArray<Byte>; P5: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function DesFire_WriteFile(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: TJavaArray<Byte>; P6: TJavaArray<Byte>; P7: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function Felica_Open(P1: Integer; P2: Integer; P3: Integer; P4: TJavaArray<Integer>; P5: TJavaArray<Byte>; P6: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function Felica_Transmit(P1: TJavaArray<Byte>; P2: Integer; P3: TJavaArray<Byte>; P4: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function L1_Contactless_wupa: Integer; cdecl;//Deprecated
    {class} function Logic_ModifyPW(P1: Integer; P2: TJavaArray<Byte>): Integer; cdecl;//Deprecated
    {class} function Logic_ReadPWDegree(P1: TJavaArray<Byte>): Integer; cdecl;//Deprecated
    {class} function M0CardKeyAuth(P1: TJavaArray<Byte>; P2: Integer; P3: TJavaArray<Byte>; P4: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function M0GetSignData(P1: Integer; P2: TJavaArray<Byte>; P3: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function NFCTagReadBlock(P1: Integer; P2: TJavaArray<Byte>; P3: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function NFCTagWriteBlock(P1: Integer; P2: TJavaArray<Byte>): Integer; cdecl;//Deprecated
    {class} function ReadLogicCardData(P1: Integer; P2: Integer; P3: TJavaArray<Byte>; P4: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function ScrdGetVersion(P1: TJavaArray<Byte>; P2: TJavaArray<Integer>; P3: TJavaArray<Byte>; P4: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function VerifyLogicCardPwd(P1: TJavaArray<Byte>): Integer; cdecl;//Deprecated
    {class} function WriteLogicCardData(P1: TJavaArray<Byte>; P2: Integer; P3: Integer; P4: TJavaArray<Byte>): Integer; cdecl;//Deprecated
    {class} function breakOffCommand: Integer; cdecl;//Deprecated
    {class} function cardReaderDetact(P1: Integer; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Integer>; P6: JString): Integer; cdecl;//Deprecated
    {class} function getCardSNFunction(P1: TJavaArray<Byte>; P2: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function iccDetect: Integer; cdecl;//Deprecated
    {class} function icsLotPower(P1: Integer; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function init(P1: JContext): JBankCard; cdecl;//Deprecated
    {class} function m1CardKeyAuth(P1: Integer; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: Integer; P6: TJavaArray<Byte>): Integer; cdecl;//Deprecated
    {class} function m1CardReadBlockData(P1: Integer; P2: TJavaArray<Byte>; P3: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function m1CardValueOperation(P1: Integer; P2: Integer; P3: Integer; P4: Integer): Integer; cdecl;//Deprecated
    {class} function m1CardWriteBlockData(P1: Integer; P2: Integer; P3: TJavaArray<Byte>): Integer; cdecl;//Deprecated
    {class} function openCloseCardReader(P1: Integer; P2: Integer): Integer; cdecl;//Deprecated
    {class} function parseART(P1: TJavaArray<Byte>; P2: Integer; P3: TJavaArray<Byte>; P4: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function parseMagnetic(P1: TJavaArray<Byte>; P2: Integer; P3: TJavaArray<Byte>; P4: TJavaArray<Integer>; P5: TJavaArray<Byte>; P6: TJavaArray<Integer>; P7: TJavaArray<Byte>; P8: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function piccDetect: Integer; cdecl;//Deprecated
    {class} function readCard(P1: Byte; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Integer>; P6: JString): Integer; cdecl;//Deprecated
    {class} function readCardIndex(P1: Byte; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Integer>; P6: Integer; P7: Integer; P8: Integer): Integer; cdecl;//Deprecated
    {class} function readContactlessInfo(P1: TJavaArray<Byte>; P2: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function sendAPDU(P1: Integer; P2: TJavaArray<Byte>; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} property CARD_DETECT_EXIST: Integer read _GetCARD_DETECT_EXIST;
    {class} property CARD_DETECT_NOTEXIST: Integer read _GetCARD_DETECT_NOTEXIST;
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
    ['{2BE13C15-163C-4E45-A359-B9854B363F6E}']
  end;
  TJBankCard = class(TJavaGenericImport<JBankCardClass, JBankCard>) end;

  JCoreClass = interface(Jlibbasebinder_aClass)
    ['{08C8160E-EE33-4C29-A8CA-845C906EE285}']
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
    {class} function _GetPIN_QUIT_SUCCESS: Byte; cdecl;
    {class} function _GetPIN_QUIT_TIMEOUT: Byte; cdecl;
    {class} function _GetPMK_UPADATE_KEYTYPE: Byte; cdecl;
    {class} function _GetPMK_UPDATE_KEYINDEX: Byte; cdecl;
    {class} function _GetPMK_UPDATE_KEYLEN: Byte; cdecl;
    {class} function EmvLib_GetPrintStatus(P1: TJavaArray<Byte>; P2: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function GeneratePINPrepareData(P1: Jlibbasebinder_a_a): Integer; cdecl; overload;//Deprecated
    {class} function GetSPLog(P1: Integer; P2: Integer; P3: TJavaArray<Integer>; P4: TJavaArray<Byte>): Integer; cdecl;//Deprecated
    {class} function Get_KeySign(P1: Integer; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: Integer; P6: TJavaArray<Byte>; P7: TJavaArray<Byte>): Integer; cdecl;//Deprecated
    {class} function SDK_ReadData(P1: TJavaArray<Byte>; P2: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function SDK_SendData(P1: TJavaArray<Byte>; P2: Integer): Integer; cdecl;//Deprecated
    {class} function SerailDebugPort(P1: Integer; P2: Integer): Integer; cdecl;//Deprecated
    {class} function SetKernel(P1: Integer; P2: Integer; P3: Integer): Integer; cdecl;//Deprecated
    {class} function buzzer: Integer; cdecl;//Deprecated
    {class} function buzzerEx(P1: Integer): Integer; cdecl;//Deprecated
    {class} function clearTamperStatus: Integer; cdecl;//Deprecated
    {class} function dataEnDecrypt(P1: TJavaArray<Byte>; P2: Integer; P3: TJavaArray<Byte>; P4: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function dataEnDecryptEx(P1: Integer; P2: Integer; P3: JString; P4: Integer; P5: Integer; P6: TJavaArray<Byte>; P7: Integer; P8: TJavaArray<Byte>; P9: Integer; P10: TJavaArray<Byte>; P11: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function dataEnDecryptExIndex(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: TJavaArray<Byte>; P7: Integer; P8: TJavaArray<Byte>; P9: Integer; P10: TJavaArray<Byte>; P11: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function dataEnDecryptForIPEK(P1: Integer; P2: Integer; P3: JString; P4: Integer; P5: Integer; P6: TJavaArray<Byte>; P7: Integer; P8: TJavaArray<Byte>; P9: Integer; P10: TJavaArray<Byte>; P11: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function decode_Close: Integer; cdecl;//Deprecated
    {class} function decode_Init(P1: Ja_e): Integer; cdecl;//Deprecated
    {class} function decode_SetLightsMode(P1: Integer): Integer; cdecl;//Deprecated
    {class} function decode_SetMaxMultiReadCount(P1: Integer): Integer; cdecl;//Deprecated
    {class} function decode_StartContinuousScan(P1: Integer): Integer; cdecl;//Deprecated
    {class} function decode_StartMultiScan(P1: Integer): Integer; cdecl;//Deprecated
    {class} function decode_StartSingleScan(P1: Integer): Integer; cdecl;//Deprecated
    {class} function decode_StopScan: Integer; cdecl;//Deprecated
    {class} function enableTamper(P1: TJavaArray<Byte>; P2: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function generatePINPrepareData(P1: TJavaArray<Byte>; P2: JButton; P3: JButton; P4: JButton; P5: JButton; P6: JButton; P7: JButton; P8: JButton; P9: JButton; P10: JButton; P11: JButton; P12: JButton; P13: JButton; P14: JButton; P15: JActivity): Integer; cdecl; overload;//Deprecated
    {class} function generatePINPrepareData(P1: TJavaArray<Byte>; P2: JButton; P3: JButton; P4: JButton; P5: JButton; P6: JButton; P7: JButton; P8: JButton; P9: JButton; P10: JButton; P11: JButton; P12: JButton; P13: JButton; P14: JButton; P15: JButton; P16: JActivity): Integer; cdecl; overload;//Deprecated
    {class} function generatePINPrepareData(P1: TJavaArray<Byte>; P2: JButton; P3: JButton; P4: JButton; P5: JButton; P6: JButton; P7: JButton; P8: JButton; P9: JButton; P10: JButton; P11: JButton; P12: JButton; P13: JButton; P14: JButton; P15: JButton; P16: Integer): Integer; cdecl; overload;//Deprecated
    {class} function generatePINPrepareData_HS(P1: TJavaArray<Byte>; P2: JButton; P3: JButton; P4: JButton; P5: JButton; P6: JButton; P7: JButton; P8: JButton; P9: JButton; P10: JButton; P11: JButton; P12: JButton; P13: JButton; P14: JButton; P15: JButton; P16: JContext; P17: Integer): Integer; cdecl;//Deprecated
    {class} function genereateRandomNum(P1: Integer; P2: TJavaArray<Byte>): Integer; cdecl;//Deprecated
    {class} function getBatteryLevel(P1: TJavaArray<Byte>): Integer; cdecl;//Deprecated
    {class} function getButtonPosition(P1: JButton; P2: TJavaArray<Byte>; P3: Integer): Integer; cdecl;//Deprecated
    {class} function getButtonPosition_HS(P1: JButton; P2: TJavaArray<Byte>; P3: JContext; P4: Integer): Integer; cdecl;//Deprecated
    {class} function getDateTime(P1: TJavaArray<Byte>): Integer; cdecl;//Deprecated
    {class} function getDeviceEncryptData(P1: JString; P2: JString): JString; cdecl;//Deprecated
    {class} function getDevicesVersion(P1: TJavaArray<Byte>; P2: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function getFirmWareNumber: JString; cdecl;//Deprecated
    {class} function getGMStatus(P1: TJavaArray<Byte>; P2: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function getMac(P1: TJavaArray<Byte>; P2: Integer; P3: TJavaArray<Byte>; P4: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function getMacEx(P1: JString; P2: Integer; P3: TJavaArray<Byte>; P4: Integer; P5: TJavaArray<Byte>; P6: Integer; P7: TJavaArray<Byte>; P8: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function getMacExIndex(P1: Integer; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: Integer; P6: TJavaArray<Byte>; P7: Integer; P8: TJavaArray<Byte>; P9: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function getMacForIPEK(P1: JString; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: Integer; P6: TJavaArray<Byte>; P7: Integer; P8: TJavaArray<Byte>; P9: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function getMacWithAlgorithm(P1: JString; P2: Integer; P3: Integer; P4: TJavaArray<Byte>; P5: Integer; P6: TJavaArray<Byte>; P7: Integer; P8: TJavaArray<Byte>; P9: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function getSDKVersion: JString; cdecl;//Deprecated
    {class} function getSpID(P1: TJavaArray<Byte>; P2: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function getSystemSn: JString; cdecl;//Deprecated
    {class} function getTamper(P1: TJavaArray<Byte>; P2: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function init(P1: JContext): JCore; cdecl;//Deprecated
    {class} function led(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer): Integer; cdecl;//Deprecated
    {class} function ledFlash(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: Integer): Integer; cdecl;//Deprecated
    {class} function pinPadRotation: Integer; cdecl;//Deprecated
    {class} function readDeviceSN: JString; cdecl;//Deprecated
    {class} function readSN(P1: TJavaArray<Byte>; P2: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function setDateTime(P1: TJavaArray<Byte>): Integer; cdecl;//Deprecated
    {class} function shortToByte(P1: SmallInt): TJavaArray<Byte>; cdecl;//Deprecated
    {class} function startPinInput(P1: Integer; P2: JString; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: TJavaArray<Byte>; P8: Integer; P9: TJavaArray<Byte>; P10: Ja_c): Integer; cdecl;//Deprecated
    {class} function startPinInputForIPEK(P1: Integer; P2: JString; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: TJavaArray<Byte>; P8: Integer; P9: TJavaArray<Byte>; P10: Ja_c): Integer; cdecl;//Deprecated
    {class} function transmitPSAM(P1: Integer; P2: TJavaArray<Byte>; P3: Integer; P4: TJavaArray<Byte>; P5: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function writeCallBackData(P1: TJavaArray<Byte>; P2: Integer): Integer; cdecl;//Deprecated
    {class} function writeCallBackDataWithCommandID(P1: Integer; P2: TJavaArray<Byte>; P3: Integer): Integer; cdecl;//Deprecated
    {class} function writeSN(P1: TJavaArray<Byte>; P2: Integer): Integer; cdecl;//Deprecated
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
    {class} property PIN_QUIT_SUCCESS: Byte read _GetPIN_QUIT_SUCCESS;
    {class} property PIN_QUIT_TIMEOUT: Byte read _GetPIN_QUIT_TIMEOUT;
    {class} property PMK_UPADATE_KEYTYPE: Byte read _GetPMK_UPADATE_KEYTYPE;
    {class} property PMK_UPDATE_KEYINDEX: Byte read _GetPMK_UPDATE_KEYINDEX;
    {class} property PMK_UPDATE_KEYLEN: Byte read _GetPMK_UPDATE_KEYLEN;
  end;

  [JavaSignature('wangpos/sdk4/libbasebinder/Core')]
  JCore = interface(Jlibbasebinder_a)
    ['{5D326BE3-B4F7-4635-B5F7-163362996F59}']
  end;
  TJCore = class(TJavaGenericImport<JCoreClass, JCore>) end;

  Jlibbasebinder_PrinterClass = interface(Jlibbasebinder_aClass)
    ['{2FAC004A-6C71-43E6-B554-C1940406D30E}']
    {class} function _GetPAPER_WIDTH: Integer; cdecl;
    {class} procedure CheckBlueToothPrintStatus; cdecl;//Deprecated
    {class} function Get_ClearPrinterMileage(P1: Integer; P2: TJavaArray<Byte>): Integer; cdecl;//Deprecated
    {class} function SDK_Printer_Test: Integer; cdecl;//Deprecated
    {class} function clearPrintDataCache: Integer; cdecl;//Deprecated
    {class} function escposBlueToothPrint(P1: TJavaArray<Byte>): Integer; cdecl;//Deprecated
    {class} function finishBlueToothPrint: Integer; cdecl;//Deprecated
    {class} function getPrinterStatus(P1: TJavaArray<Integer>): Integer; cdecl;//Deprecated
    {class} function init(P1: JContext): Jlibbasebinder_Printer; cdecl;//Deprecated
    {class} function initBlueToothPrint(P1: Ja_d): Integer; cdecl;//Deprecated
    {class} function print2StringInLine(P1: JString; P2: JString; P3: Single; P4: JPrinter_Font; P5: Integer; P6: JPrinter_Align; P7: Boolean; P8: Boolean; P9: Boolean): Integer; cdecl;//Deprecated
    {class} function printBarCode(P1: JString; P2: Integer; P3: Boolean): Integer; cdecl;//Deprecated
    {class} function printBarCodeBase(P1: JString; P2: JPrinter_BarcodeType; P3: JPrinter_BarcodeWidth; P4: Integer; P5: Integer): Integer; cdecl;//Deprecated
    {class} function printFinish: Integer; cdecl;//Deprecated
    {class} function printImage(P1: JBitmap; P2: Integer; P3: JPrinter_Align): Integer; cdecl;//Deprecated
    {class} function printImageBase(P1: JBitmap; P2: Integer; P3: Integer; P4: JPrinter_Align; P5: Integer): Integer; cdecl;//Deprecated
    {class} function printInit: Integer; cdecl;//Deprecated
    {class} function printPDF417(P1: JString): Integer; cdecl; overload;//Deprecated
    {class} function printPDF417(P1: JString; P2: Integer; P3: Integer): Integer; cdecl; overload;//Deprecated
    {class} function printPDF417(P1: JString; P2: Integer; P3: Integer; P4: JPrinter_Align): Integer; cdecl; overload;//Deprecated
    {class} function printPaper(P1: Integer): Integer; cdecl;//Deprecated
    {class} function printPaper_trade(P1: Integer; P2: Integer): Integer; cdecl;//Deprecated
    {class} function printQRCode(P1: JString): Integer; cdecl; overload;//Deprecated
    {class} function printQRCode(P1: JString; P2: Integer): Integer; cdecl; overload;//Deprecated
    {class} function printQRCode(P1: JString; P2: Integer; P3: JPrinter_Align): Integer; cdecl; overload;//Deprecated
    {class} function printString(P1: JString; P2: Integer; P3: JPrinter_Align; P4: Boolean; P5: Boolean): Integer; cdecl;//Deprecated
    {class} function printStringBase(P1: JString; P2: Integer; P3: Single; P4: Single; P5: Integer; P6: JPrinter_Align; P7: Boolean; P8: Boolean; P9: Boolean): Integer; cdecl;//Deprecated
    {class} function printStringExt(P1: JString; P2: Integer; P3: Single; P4: Single; P5: JPrinter_Font; P6: Integer; P7: JPrinter_Align; P8: Boolean; P9: Boolean; P10: Boolean): Integer; cdecl;//Deprecated
    {class} function printStringWithScaleX(P1: JString; P2: Integer; P3: Single; P4: Single; P5: Single; P6: JPrinter_Font; P7: Integer; P8: JPrinter_Align; P9: Boolean; P10: Boolean; P11: Boolean): Integer; cdecl;//Deprecated
    {class} function setGrayLevel(P1: Integer): Integer; cdecl;//Deprecated
    {class} procedure setPrintFontType(P1: JContext; P2: JString); cdecl;//Deprecated
    {class} function startBlueToothPrint: Integer; cdecl;//Deprecated
    {class} property PAPER_WIDTH: Integer read _GetPAPER_WIDTH;
  end;

  [JavaSignature('wangpos/sdk4/libbasebinder/Printer')]
  Jlibbasebinder_Printer = interface(Jlibbasebinder_a)
    ['{EA00C243-6E20-4E09-A081-1252B09329A2}']
  end;
  TJlibbasebinder_Printer = class(TJavaGenericImport<Jlibbasebinder_PrinterClass, Jlibbasebinder_Printer>) end;

  JPrinter_AlignClass = interface(JEnumClass)
    ['{F1467A6C-1FA1-482F-9ACD-04DA28FFF81F}']
    {class} function _GetCENTER: JPrinter_Align; cdecl;
    {class} function _GetLEFT: JPrinter_Align; cdecl;
    {class} function _GetRIGHT: JPrinter_Align; cdecl;
    {class} function valueOf(P1: JString): JPrinter_Align; cdecl;//Deprecated
    {class} function values: TJavaObjectArray<JPrinter_Align>; cdecl;//Deprecated
    {class} property CENTER: JPrinter_Align read _GetCENTER;
    {class} property LEFT: JPrinter_Align read _GetLEFT;
    {class} property RIGHT: JPrinter_Align read _GetRIGHT;
  end;

  [JavaSignature('wangpos/sdk4/libbasebinder/Printer$Align')]
  JPrinter_Align = interface(JEnum)
    ['{AE25685E-B059-45CF-8535-590697A167F0}']
  end;
  TJPrinter_Align = class(TJavaGenericImport<JPrinter_AlignClass, JPrinter_Align>) end;

  JPrinter_BarcodeTypeClass = interface(JEnumClass)
    ['{2C14559C-CC62-4B49-A500-70994B5AF2DD}']
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
    {class} function valueOf(P1: JString): JPrinter_BarcodeType; cdecl;//Deprecated
    {class} function values: TJavaObjectArray<JPrinter_BarcodeType>; cdecl;//Deprecated
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
    ['{74C22F08-2333-4733-BC8A-99E81A840D3B}']
  end;
  TJPrinter_BarcodeType = class(TJavaGenericImport<JPrinter_BarcodeTypeClass, JPrinter_BarcodeType>) end;

  JPrinter_BarcodeWidthClass = interface(JEnumClass)
    ['{ABA11BCF-5407-42D2-9FDB-5B75906EBE08}']
    {class} function _GetHUGE: JPrinter_BarcodeWidth; cdecl;
    {class} function _GetLARGE: JPrinter_BarcodeWidth; cdecl;
    {class} function _GetNORMAL: JPrinter_BarcodeWidth; cdecl;
    {class} function _GetSMALL: JPrinter_BarcodeWidth; cdecl;
    {class} function valueOf(P1: JString): JPrinter_BarcodeWidth; cdecl;//Deprecated
    {class} function values: TJavaObjectArray<JPrinter_BarcodeWidth>; cdecl;//Deprecated
    {class} property HUGE: JPrinter_BarcodeWidth read _GetHUGE;
    {class} property LARGE: JPrinter_BarcodeWidth read _GetLARGE;
    {class} property NORMAL: JPrinter_BarcodeWidth read _GetNORMAL;
    {class} property SMALL: JPrinter_BarcodeWidth read _GetSMALL;
  end;

  [JavaSignature('wangpos/sdk4/libbasebinder/Printer$BarcodeWidth')]
  JPrinter_BarcodeWidth = interface(JEnum)
    ['{88CD0848-2940-4C13-9114-9DE3E86A994E}']
  end;
  TJPrinter_BarcodeWidth = class(TJavaGenericImport<JPrinter_BarcodeWidthClass, JPrinter_BarcodeWidth>) end;

  JPrinter_FontClass = interface(JEnumClass)
    ['{53ED3A53-02BF-4940-BE1C-084CA606A08C}']
    {class} function _GetDEFAULT: JPrinter_Font; cdecl;
    {class} function _GetDEFAULT_BOLD: JPrinter_Font; cdecl;
    {class} function _GetMONOSPACE: JPrinter_Font; cdecl;
    {class} function _GetSANS_SERIF: JPrinter_Font; cdecl;
    {class} function _GetSERIF: JPrinter_Font; cdecl;
    {class} function valueOf(P1: JString): JPrinter_Font; cdecl;//Deprecated
    {class} function values: TJavaObjectArray<JPrinter_Font>; cdecl;//Deprecated
    {class} property DEFAULT: JPrinter_Font read _GetDEFAULT;
    {class} property DEFAULT_BOLD: JPrinter_Font read _GetDEFAULT_BOLD;
    {class} property MONOSPACE: JPrinter_Font read _GetMONOSPACE;
    {class} property SANS_SERIF: JPrinter_Font read _GetSANS_SERIF;
    {class} property SERIF: JPrinter_Font read _GetSERIF;
  end;

  [JavaSignature('wangpos/sdk4/libbasebinder/Printer$Font')]
  JPrinter_Font = interface(JEnum)
    ['{3A30A0DD-C088-4BE6-A865-2F4FF32FAE4E}']
  end;
  TJPrinter_Font = class(TJavaGenericImport<JPrinter_FontClass, JPrinter_Font>) end;

  JRspCodeClass = interface(JObjectClass)
    ['{3E47CB62-FAFE-42E4-ABAD-41726838A0CA}']
    {class} function _GetERROR: Integer; cdecl;
    {class} function _GetOK: Integer; cdecl;
    {class} function init: JRspCode; cdecl;//Deprecated
    {class} property ERROR: Integer read _GetERROR;
    {class} property OK: Integer read _GetOK;
  end;

  [JavaSignature('wangpos/sdk4/libbasebinder/RspCode')]
  JRspCode = interface(JObject)
    ['{AA60CA24-C72C-48AB-8783-678D7367CA85}']
  end;
  TJRspCode = class(TJavaGenericImport<JRspCodeClass, JRspCode>) end;

  Ja_1Class = interface(JServiceConnectionClass)
    ['{B299D61F-B57A-4176-B9B6-160A211B07D0}']
    {class} function init(P1: Jlibbasebinder_a): Ja_1; cdecl;//Deprecated
    {class} procedure onServiceConnected(P1: JComponentName; P2: JIBinder); cdecl;//Deprecated
    {class} procedure onServiceDisconnected(P1: JComponentName); cdecl;//Deprecated
  end;

  [JavaSignature('wangpos/sdk4/libbasebinder/a$1')]
  Ja_1 = interface(JServiceConnection)
    ['{8E0CD8D6-552F-45FF-BA29-E5C10DCD9F4D}']
  end;
  TJa_1 = class(TJavaGenericImport<Ja_1Class, Ja_1>) end;

  Ja_2Class = interface(JIBinder_DeathRecipientClass)
    ['{5AAA94D2-66CA-48BF-A527-CEC72882264D}']
    {class} procedure binderDied; cdecl;//Deprecated
    {class} function init(P1: Jlibbasebinder_a): Ja_2; cdecl;//Deprecated
  end;

  [JavaSignature('wangpos/sdk4/libbasebinder/a$2')]
  Ja_2 = interface(JIBinder_DeathRecipient)
    ['{88F4E00F-27B4-45CA-B8B9-BD2D0062E40B}']
  end;
  TJa_2 = class(TJavaGenericImport<Ja_2Class, Ja_2>) end;

  Jlibbasebinder_a_aClass = interface(JObjectClass)
    ['{ED63789B-B8A5-457E-AF2E-BF906806B841}']
    {class} function _Geta: TJavaArray<Byte>; cdecl;
    {class} procedure _Seta(Value: TJavaArray<Byte>); cdecl;
    {class} function _Getb: JButton; cdecl;
    {class} procedure _Setb(Value: JButton); cdecl;
    {class} function _Getc: JButton; cdecl;
    {class} procedure _Setc(Value: JButton); cdecl;
    {class} function _Getd: JButton; cdecl;
    {class} procedure _Setd(Value: JButton); cdecl;
    {class} function _Gete: JButton; cdecl;
    {class} procedure _Sete(Value: JButton); cdecl;
    {class} function _Getf: JButton; cdecl;
    {class} procedure _Setf(Value: JButton); cdecl;
    {class} function _Getg: JButton; cdecl;
    {class} procedure _Setg(Value: JButton); cdecl;
    {class} function _Geth: JButton; cdecl;
    {class} procedure _Seth(Value: JButton); cdecl;
    {class} function _Geti: JButton; cdecl;
    {class} procedure _Seti(Value: JButton); cdecl;
    {class} function _Getj: JButton; cdecl;
    {class} procedure _Setj(Value: JButton); cdecl;
    {class} function _Getk: JButton; cdecl;
    {class} procedure _Setk(Value: JButton); cdecl;
    {class} function _Getl: JButton; cdecl;
    {class} procedure _Setl(Value: JButton); cdecl;
    {class} function _Getm: JButton; cdecl;
    {class} procedure _Setm(Value: JButton); cdecl;
    {class} function _Getn: JButton; cdecl;
    {class} procedure _Setn(Value: JButton); cdecl;
    {class} function _Geto: JButton; cdecl;
    {class} procedure _Seto(Value: JButton); cdecl;
    {class} function _Getp: JActivity; cdecl;
    {class} procedure _Setp(Value: JActivity); cdecl;
    {class} property a: TJavaArray<Byte> read _Geta write _Seta;
    {class} property b: JButton read _Getb write _Setb;
    {class} property c: JButton read _Getc write _Setc;
    {class} property d: JButton read _Getd write _Setd;
    {class} property e: JButton read _Gete write _Sete;
    {class} property f: JButton read _Getf write _Setf;
    {class} property g: JButton read _Getg write _Setg;
    {class} property h: JButton read _Geth write _Seth;
    {class} property i: JButton read _Geti write _Seti;
    {class} property j: JButton read _Getj write _Setj;
    {class} property k: JButton read _Getk write _Setk;
    {class} property l: JButton read _Getl write _Setl;
    {class} property m: JButton read _Getm write _Setm;
    {class} property n: JButton read _Getn write _Setn;
    {class} property o: JButton read _Geto write _Seto;
    {class} property p: JActivity read _Getp write _Setp;
  end;

  [JavaSignature('wangpos/sdk4/libbasebinder/a/a')]
  Jlibbasebinder_a_a = interface(JObject)
    ['{BF86B116-75E2-4D44-A744-025D4370BF87}']
  end;
  TJlibbasebinder_a_a = class(TJavaGenericImport<Jlibbasebinder_a_aClass, Jlibbasebinder_a_a>) end;

  Jlibbasebinder_bClass = interface(JObjectClass)
    ['{55F0F034-51A6-403D-8DFD-C28E46E77C2B}']
    {class} function a(P1: TJavaArray<Byte>): JString; cdecl;
  end;

  [JavaSignature('wangpos/sdk4/libbasebinder/b')]
  Jlibbasebinder_b = interface(JObject)
    ['{B28F3AD5-7152-45BE-A10A-5E28365D4263}']
  end;
  TJlibbasebinder_b = class(TJavaGenericImport<Jlibbasebinder_bClass, Jlibbasebinder_b>) end;

implementation

procedure RegisterTypes;
begin
  TRegTypes.RegisterType('G700Interface.JAnimator', TypeInfo(G700Interface.JAnimator));
  TRegTypes.RegisterType('G700Interface.JAnimator_AnimatorListener', TypeInfo(G700Interface.JAnimator_AnimatorListener));
  TRegTypes.RegisterType('G700Interface.JAnimator_AnimatorPauseListener', TypeInfo(G700Interface.JAnimator_AnimatorPauseListener));
  TRegTypes.RegisterType('G700Interface.JKeyframe', TypeInfo(G700Interface.JKeyframe));
  TRegTypes.RegisterType('G700Interface.JLayoutTransition', TypeInfo(G700Interface.JLayoutTransition));
  TRegTypes.RegisterType('G700Interface.JLayoutTransition_TransitionListener', TypeInfo(G700Interface.JLayoutTransition_TransitionListener));
  TRegTypes.RegisterType('G700Interface.JPropertyValuesHolder', TypeInfo(G700Interface.JPropertyValuesHolder));
  TRegTypes.RegisterType('G700Interface.JStateListAnimator', TypeInfo(G700Interface.JStateListAnimator));
  TRegTypes.RegisterType('G700Interface.JTimeInterpolator', TypeInfo(G700Interface.JTimeInterpolator));
  TRegTypes.RegisterType('G700Interface.JTypeConverter', TypeInfo(G700Interface.JTypeConverter));
  TRegTypes.RegisterType('G700Interface.JTypeEvaluator', TypeInfo(G700Interface.JTypeEvaluator));
  TRegTypes.RegisterType('G700Interface.JValueAnimator', TypeInfo(G700Interface.JValueAnimator));
  TRegTypes.RegisterType('G700Interface.JValueAnimator_AnimatorUpdateListener', TypeInfo(G700Interface.JValueAnimator_AnimatorUpdateListener));
  TRegTypes.RegisterType('G700Interface.JPathMotion', TypeInfo(G700Interface.JPathMotion));
  TRegTypes.RegisterType('G700Interface.JScene', TypeInfo(G700Interface.JScene));
  TRegTypes.RegisterType('G700Interface.JTransition', TypeInfo(G700Interface.JTransition));
  TRegTypes.RegisterType('G700Interface.JTransition_EpicenterCallback', TypeInfo(G700Interface.JTransition_EpicenterCallback));
  TRegTypes.RegisterType('G700Interface.JTransition_TransitionListener', TypeInfo(G700Interface.JTransition_TransitionListener));
  TRegTypes.RegisterType('G700Interface.JTransitionManager', TypeInfo(G700Interface.JTransitionManager));
  TRegTypes.RegisterType('G700Interface.JTransitionPropagation', TypeInfo(G700Interface.JTransitionPropagation));
  TRegTypes.RegisterType('G700Interface.JTransitionValues', TypeInfo(G700Interface.JTransitionValues));
  TRegTypes.RegisterType('G700Interface.JInterpolator', TypeInfo(G700Interface.JInterpolator));
  TRegTypes.RegisterType('G700Interface.JToolbar_LayoutParams', TypeInfo(G700Interface.JToolbar_LayoutParams));
  TRegTypes.RegisterType('G700Interface.Jgertec_Logger', TypeInfo(G700Interface.Jgertec_Logger));
  TRegTypes.RegisterType('G700Interface.Jgertec_a_a', TypeInfo(G700Interface.Jgertec_a_a));
  TRegTypes.RegisterType('G700Interface.Jgertec_a_a_a', TypeInfo(G700Interface.Jgertec_a_a_a));
  TRegTypes.RegisterType('G700Interface.Ja_a_a_a', TypeInfo(G700Interface.Ja_a_a_a));
  TRegTypes.RegisterType('G700Interface.JIGEDI', TypeInfo(G700Interface.JIGEDI));
  TRegTypes.RegisterType('G700Interface.JGEDI', TypeInfo(G700Interface.JGEDI));
  TRegTypes.RegisterType('G700Interface.JGEDI_1', TypeInfo(G700Interface.JGEDI_1));
  TRegTypes.RegisterType('G700Interface.JGEDI_KMS_st_Control_Callbacks', TypeInfo(G700Interface.JGEDI_KMS_st_Control_Callbacks));
  TRegTypes.RegisterType('G700Interface.JGediNative', TypeInfo(G700Interface.JGediNative));
  TRegTypes.RegisterType('G700Interface.JICL', TypeInfo(G700Interface.JICL));
  TRegTypes.RegisterType('G700Interface.Jgedi_a', TypeInfo(G700Interface.Jgedi_a));
  TRegTypes.RegisterType('G700Interface.JIAUDIO', TypeInfo(G700Interface.JIAUDIO));
  TRegTypes.RegisterType('G700Interface.Jgedi_a_a', TypeInfo(G700Interface.Jgedi_a_a));
  TRegTypes.RegisterType('G700Interface.Jgedi_a_b', TypeInfo(G700Interface.Jgedi_a_b));
  TRegTypes.RegisterType('G700Interface.JICLOCK', TypeInfo(G700Interface.JICLOCK));
  TRegTypes.RegisterType('G700Interface.Jgedi_b', TypeInfo(G700Interface.Jgedi_b));
  TRegTypes.RegisterType('G700Interface.Jgedi_a_c', TypeInfo(G700Interface.Jgedi_a_c));
  TRegTypes.RegisterType('G700Interface.JICRYPT', TypeInfo(G700Interface.JICRYPT));
  TRegTypes.RegisterType('G700Interface.Jgedi_c', TypeInfo(G700Interface.Jgedi_c));
  TRegTypes.RegisterType('G700Interface.Jgedi_a_d', TypeInfo(G700Interface.Jgedi_a_d));
  TRegTypes.RegisterType('G700Interface.Jgedi_a_e', TypeInfo(G700Interface.Jgedi_a_e));
  TRegTypes.RegisterType('G700Interface.JIINFO', TypeInfo(G700Interface.JIINFO));
  TRegTypes.RegisterType('G700Interface.Jgedi_d', TypeInfo(G700Interface.Jgedi_d));
  TRegTypes.RegisterType('G700Interface.Ja_f', TypeInfo(G700Interface.Ja_f));
  TRegTypes.RegisterType('G700Interface.Jf_1', TypeInfo(G700Interface.Jf_1));
  TRegTypes.RegisterType('G700Interface.JIKBD', TypeInfo(G700Interface.JIKBD));
  TRegTypes.RegisterType('G700Interface.Jgedi_e', TypeInfo(G700Interface.Jgedi_e));
  TRegTypes.RegisterType('G700Interface.Ja_g', TypeInfo(G700Interface.Ja_g));
  TRegTypes.RegisterType('G700Interface.Jg_1', TypeInfo(G700Interface.Jg_1));
  TRegTypes.RegisterType('G700Interface.Jg_10', TypeInfo(G700Interface.Jg_10));
  TRegTypes.RegisterType('G700Interface.Jg_11', TypeInfo(G700Interface.Jg_11));
  TRegTypes.RegisterType('G700Interface.Jg_12', TypeInfo(G700Interface.Jg_12));
  TRegTypes.RegisterType('G700Interface.Jg_13', TypeInfo(G700Interface.Jg_13));
  TRegTypes.RegisterType('G700Interface.Jg_2', TypeInfo(G700Interface.Jg_2));
  TRegTypes.RegisterType('G700Interface.Jg_3', TypeInfo(G700Interface.Jg_3));
  TRegTypes.RegisterType('G700Interface.Jg_4', TypeInfo(G700Interface.Jg_4));
  TRegTypes.RegisterType('G700Interface.Jg_5', TypeInfo(G700Interface.Jg_5));
  TRegTypes.RegisterType('G700Interface.Jg_6', TypeInfo(G700Interface.Jg_6));
  TRegTypes.RegisterType('G700Interface.Jg_7', TypeInfo(G700Interface.Jg_7));
  TRegTypes.RegisterType('G700Interface.Jg_8', TypeInfo(G700Interface.Jg_8));
  TRegTypes.RegisterType('G700Interface.Jg_9', TypeInfo(G700Interface.Jg_9));
  TRegTypes.RegisterType('G700Interface.JIKMS', TypeInfo(G700Interface.JIKMS));
  TRegTypes.RegisterType('G700Interface.Jgedi_g', TypeInfo(G700Interface.Jgedi_g));
  TRegTypes.RegisterType('G700Interface.Ja_h', TypeInfo(G700Interface.Ja_h));
  TRegTypes.RegisterType('G700Interface.Jc_a', TypeInfo(G700Interface.Jc_a));
  TRegTypes.RegisterType('G700Interface.Jh_1', TypeInfo(G700Interface.Jh_1));
  TRegTypes.RegisterType('G700Interface.Jh_2', TypeInfo(G700Interface.Jh_2));
  TRegTypes.RegisterType('G700Interface.JIMSR', TypeInfo(G700Interface.JIMSR));
  TRegTypes.RegisterType('G700Interface.Jgedi_i', TypeInfo(G700Interface.Jgedi_i));
  TRegTypes.RegisterType('G700Interface.Ja_i', TypeInfo(G700Interface.Ja_i));
  TRegTypes.RegisterType('G700Interface.Ji_1', TypeInfo(G700Interface.Ji_1));
  TRegTypes.RegisterType('G700Interface.JIPM', TypeInfo(G700Interface.JIPM));
  TRegTypes.RegisterType('G700Interface.Jgedi_j', TypeInfo(G700Interface.Jgedi_j));
  TRegTypes.RegisterType('G700Interface.Ja_j', TypeInfo(G700Interface.Ja_j));
  TRegTypes.RegisterType('G700Interface.JIPRNTR', TypeInfo(G700Interface.JIPRNTR));
  TRegTypes.RegisterType('G700Interface.Jgedi_k', TypeInfo(G700Interface.Jgedi_k));
  TRegTypes.RegisterType('G700Interface.Ja_k', TypeInfo(G700Interface.Ja_k));
  TRegTypes.RegisterType('G700Interface.Jk_1', TypeInfo(G700Interface.Jk_1));
  TRegTypes.RegisterType('G700Interface.JISMART', TypeInfo(G700Interface.JISMART));
  TRegTypes.RegisterType('G700Interface.Jgedi_l', TypeInfo(G700Interface.Jgedi_l));
  TRegTypes.RegisterType('G700Interface.Ja_l', TypeInfo(G700Interface.Ja_l));
  TRegTypes.RegisterType('G700Interface.Jl_1', TypeInfo(G700Interface.Jl_1));
  TRegTypes.RegisterType('G700Interface.Jl_2', TypeInfo(G700Interface.Jl_2));
  TRegTypes.RegisterType('G700Interface.Jl_3', TypeInfo(G700Interface.Jl_3));
  TRegTypes.RegisterType('G700Interface.JISYS', TypeInfo(G700Interface.JISYS));
  TRegTypes.RegisterType('G700Interface.Jgedi_m', TypeInfo(G700Interface.Jgedi_m));
  TRegTypes.RegisterType('G700Interface.Ja_m', TypeInfo(G700Interface.Ja_m));
  TRegTypes.RegisterType('G700Interface.Ja_n', TypeInfo(G700Interface.Ja_n));
  TRegTypes.RegisterType('G700Interface.Ja_o', TypeInfo(G700Interface.Ja_o));
  TRegTypes.RegisterType('G700Interface.Jo_1', TypeInfo(G700Interface.Jo_1));
  TRegTypes.RegisterType('G700Interface.JGEDI_CL_e_ISO_Level', TypeInfo(G700Interface.JGEDI_CL_e_ISO_Level));
  TRegTypes.RegisterType('G700Interface.JGEDI_CL_e_ISO_Type', TypeInfo(G700Interface.JGEDI_CL_e_ISO_Type));
  TRegTypes.RegisterType('G700Interface.JGEDI_CL_e_MF_KeyType', TypeInfo(G700Interface.JGEDI_CL_e_MF_KeyType));
  TRegTypes.RegisterType('G700Interface.JGEDI_CL_e_MF_Type', TypeInfo(G700Interface.JGEDI_CL_e_MF_Type));
  TRegTypes.RegisterType('G700Interface.JGEDI_CRYPT_e_Op', TypeInfo(G700Interface.JGEDI_CRYPT_e_Op));
  TRegTypes.RegisterType('G700Interface.JGEDI_FS_e_Storage', TypeInfo(G700Interface.JGEDI_FS_e_Storage));
  TRegTypes.RegisterType('G700Interface.JGEDI_INFO_e_ControlNumber', TypeInfo(G700Interface.JGEDI_INFO_e_ControlNumber));
  TRegTypes.RegisterType('G700Interface.JGEDI_INFO_e_Test', TypeInfo(G700Interface.JGEDI_INFO_e_Test));
  TRegTypes.RegisterType('G700Interface.JGEDI_KBD_e_Key', TypeInfo(G700Interface.JGEDI_KBD_e_Key));
  TRegTypes.RegisterType('G700Interface.JGEDI_KBD_e_PowerKeyMode', TypeInfo(G700Interface.JGEDI_KBD_e_PowerKeyMode));
  TRegTypes.RegisterType('G700Interface.JGEDI_KMS_e_BLOCKTYPE', TypeInfo(G700Interface.JGEDI_KMS_e_BLOCKTYPE));
  TRegTypes.RegisterType('G700Interface.JGEDI_KMS_e_EncMode', TypeInfo(G700Interface.JGEDI_KMS_e_EncMode));
  TRegTypes.RegisterType('G700Interface.JGEDI_KMS_e_KEYPURPOSE', TypeInfo(G700Interface.JGEDI_KMS_e_KEYPURPOSE));
  TRegTypes.RegisterType('G700Interface.JGEDI_KMS_e_KEYTYPE', TypeInfo(G700Interface.JGEDI_KMS_e_KEYTYPE));
  TRegTypes.RegisterType('G700Interface.JGEDI_KMS_e_KEYTYPE_LENGTH', TypeInfo(G700Interface.JGEDI_KMS_e_KEYTYPE_LENGTH));
  TRegTypes.RegisterType('G700Interface.JGEDI_KMS_e_OP', TypeInfo(G700Interface.JGEDI_KMS_e_OP));
  TRegTypes.RegisterType('G700Interface.JGEDI_LED_e_Id', TypeInfo(G700Interface.JGEDI_LED_e_Id));
  TRegTypes.RegisterType('G700Interface.JGEDI_MSR_e_Status', TypeInfo(G700Interface.JGEDI_MSR_e_Status));
  TRegTypes.RegisterType('G700Interface.JGEDI_PRNTR_e_Alignment', TypeInfo(G700Interface.JGEDI_PRNTR_e_Alignment));
  TRegTypes.RegisterType('G700Interface.JGEDI_PRNTR_e_BarCodeType', TypeInfo(G700Interface.JGEDI_PRNTR_e_BarCodeType));
  TRegTypes.RegisterType('G700Interface.JGEDI_PRNTR_e_Status', TypeInfo(G700Interface.JGEDI_PRNTR_e_Status));
  TRegTypes.RegisterType('G700Interface.JGEDI_SMART_e_MemoryCardType', TypeInfo(G700Interface.JGEDI_SMART_e_MemoryCardType));
  TRegTypes.RegisterType('G700Interface.JGEDI_SMART_e_Slot', TypeInfo(G700Interface.JGEDI_SMART_e_Slot));
  TRegTypes.RegisterType('G700Interface.JGEDI_SMART_e_Status', TypeInfo(G700Interface.JGEDI_SMART_e_Status));
  TRegTypes.RegisterType('G700Interface.JGEDI_SMART_e_Type', TypeInfo(G700Interface.JGEDI_SMART_e_Type));
  TRegTypes.RegisterType('G700Interface.JGEDI_SMART_e_Voltage', TypeInfo(G700Interface.JGEDI_SMART_e_Voltage));
  TRegTypes.RegisterType('G700Interface.JGEDI_SYS_e_SecuritySetup', TypeInfo(G700Interface.JGEDI_SYS_e_SecuritySetup));
  TRegTypes.RegisterType('G700Interface.JGEDI_e_Ret', TypeInfo(G700Interface.JGEDI_e_Ret));
  TRegTypes.RegisterType('G700Interface.JGediException', TypeInfo(G700Interface.JGediException));
  TRegTypes.RegisterType('G700Interface.Jgedi_f', TypeInfo(G700Interface.Jgedi_f));
  TRegTypes.RegisterType('G700Interface.JILED', TypeInfo(G700Interface.JILED));
  TRegTypes.RegisterType('G700Interface.Jgedi_h', TypeInfo(G700Interface.Jgedi_h));
  TRegTypes.RegisterType('G700Interface.JIEnums', TypeInfo(G700Interface.JIEnums));
  TRegTypes.RegisterType('G700Interface.JGEDI_AUTH_st_Data', TypeInfo(G700Interface.JGEDI_AUTH_st_Data));
  TRegTypes.RegisterType('G700Interface.JGEDI_CLOCK_st_RTC', TypeInfo(G700Interface.JGEDI_CLOCK_st_RTC));
  TRegTypes.RegisterType('G700Interface.JGEDI_CL_st_ISO_PollingInfo', TypeInfo(G700Interface.JGEDI_CL_st_ISO_PollingInfo));
  TRegTypes.RegisterType('G700Interface.JGEDI_CL_st_MF_ActivateInfo', TypeInfo(G700Interface.JGEDI_CL_st_MF_ActivateInfo));
  TRegTypes.RegisterType('G700Interface.JGEDI_CL_st_MF_Key', TypeInfo(G700Interface.JGEDI_CL_st_MF_Key));
  TRegTypes.RegisterType('G700Interface.JGEDI_CL_st_ResetEMVInfo', TypeInfo(G700Interface.JGEDI_CL_st_ResetEMVInfo));
  TRegTypes.RegisterType('G700Interface.JGEDI_CRYPT_st_RSAKeyGen', TypeInfo(G700Interface.JGEDI_CRYPT_st_RSAKeyGen));
  TRegTypes.RegisterType('G700Interface.JGEDI_KBD_st_Info', TypeInfo(G700Interface.JGEDI_KBD_st_Info));
  TRegTypes.RegisterType('G700Interface.JGEDI_KMS_st_CapturePINBlockInfo', TypeInfo(G700Interface.JGEDI_KMS_st_CapturePINBlockInfo));
  TRegTypes.RegisterType('G700Interface.JGEDI_KMS_st_Control', TypeInfo(G700Interface.JGEDI_KMS_st_Control));
  TRegTypes.RegisterType('G700Interface.JGEDI_KMS_st_Data', TypeInfo(G700Interface.JGEDI_KMS_st_Data));
  TRegTypes.RegisterType('G700Interface.JGEDI_KMS_st_KB', TypeInfo(G700Interface.JGEDI_KMS_st_KB));
  TRegTypes.RegisterType('G700Interface.JGEDI_KMS_st_PINBlock', TypeInfo(G700Interface.JGEDI_KMS_st_PINBlock));
  TRegTypes.RegisterType('G700Interface.JGEDI_KMS_st_SaveKey', TypeInfo(G700Interface.JGEDI_KMS_st_SaveKey));
  TRegTypes.RegisterType('G700Interface.JGEDI_MSR_st_LastErrors', TypeInfo(G700Interface.JGEDI_MSR_st_LastErrors));
  TRegTypes.RegisterType('G700Interface.JGEDI_MSR_st_Tracks', TypeInfo(G700Interface.JGEDI_MSR_st_Tracks));
  TRegTypes.RegisterType('G700Interface.JGEDI_PRNTR_st_BarCodeConfig', TypeInfo(G700Interface.JGEDI_PRNTR_st_BarCodeConfig));
  TRegTypes.RegisterType('G700Interface.JGEDI_PRNTR_st_PictureConfig', TypeInfo(G700Interface.JGEDI_PRNTR_st_PictureConfig));
  TRegTypes.RegisterType('G700Interface.JGEDI_PRNTR_st_StringConfig', TypeInfo(G700Interface.JGEDI_PRNTR_st_StringConfig));
  TRegTypes.RegisterType('G700Interface.JGEDI_SMART_st_ResetInfo', TypeInfo(G700Interface.JGEDI_SMART_st_ResetInfo));
  TRegTypes.RegisterType('G700Interface.Jsdk4_a_a', TypeInfo(G700Interface.Jsdk4_a_a));
  TRegTypes.RegisterType('G700Interface.Jsdk4_a_a_a', TypeInfo(G700Interface.Jsdk4_a_a_a));
  TRegTypes.RegisterType('G700Interface.Ja_a_a', TypeInfo(G700Interface.Ja_a_a));
  TRegTypes.RegisterType('G700Interface.Ja_b', TypeInfo(G700Interface.Ja_b));
  TRegTypes.RegisterType('G700Interface.Jb_a', TypeInfo(G700Interface.Jb_a));
  TRegTypes.RegisterType('G700Interface.Jb_a_a', TypeInfo(G700Interface.Jb_a_a));
  TRegTypes.RegisterType('G700Interface.Ja_c', TypeInfo(G700Interface.Ja_c));
  TRegTypes.RegisterType('G700Interface.Jc_a_a', TypeInfo(G700Interface.Jc_a_a));
  TRegTypes.RegisterType('G700Interface.Ja_d', TypeInfo(G700Interface.Ja_d));
  TRegTypes.RegisterType('G700Interface.Jd_a', TypeInfo(G700Interface.Jd_a));
  TRegTypes.RegisterType('G700Interface.Jd_a_a', TypeInfo(G700Interface.Jd_a_a));
  TRegTypes.RegisterType('G700Interface.Ja_e', TypeInfo(G700Interface.Ja_e));
  TRegTypes.RegisterType('G700Interface.Je_a', TypeInfo(G700Interface.Je_a));
  TRegTypes.RegisterType('G700Interface.Je_a_a', TypeInfo(G700Interface.Je_a_a));
  TRegTypes.RegisterType('G700Interface.Jsdk4_b_a', TypeInfo(G700Interface.Jsdk4_b_a));
  TRegTypes.RegisterType('G700Interface.Jsdk4_b_a_a', TypeInfo(G700Interface.Jsdk4_b_a_a));
  TRegTypes.RegisterType('G700Interface.Jb_a_a_a', TypeInfo(G700Interface.Jb_a_a_a));
  TRegTypes.RegisterType('G700Interface.Jb_b', TypeInfo(G700Interface.Jb_b));
  TRegTypes.RegisterType('G700Interface.Jb_b_a', TypeInfo(G700Interface.Jb_b_a));
  TRegTypes.RegisterType('G700Interface.Jb_b_a_a', TypeInfo(G700Interface.Jb_b_a_a));
  TRegTypes.RegisterType('G700Interface.Jsdk4_c_a', TypeInfo(G700Interface.Jsdk4_c_a));
  TRegTypes.RegisterType('G700Interface.Jsdk4_c_a_a', TypeInfo(G700Interface.Jsdk4_c_a_a));
  TRegTypes.RegisterType('G700Interface.Jc_a_b', TypeInfo(G700Interface.Jc_a_b));
  TRegTypes.RegisterType('G700Interface.Jc_b', TypeInfo(G700Interface.Jc_b));
  TRegTypes.RegisterType('G700Interface.Jlibbasebinder_a', TypeInfo(G700Interface.Jlibbasebinder_a));
  TRegTypes.RegisterType('G700Interface.JBankCard', TypeInfo(G700Interface.JBankCard));
  TRegTypes.RegisterType('G700Interface.JCore', TypeInfo(G700Interface.JCore));
  TRegTypes.RegisterType('G700Interface.Jlibbasebinder_Printer', TypeInfo(G700Interface.Jlibbasebinder_Printer));
  TRegTypes.RegisterType('G700Interface.JPrinter_Align', TypeInfo(G700Interface.JPrinter_Align));
  TRegTypes.RegisterType('G700Interface.JPrinter_BarcodeType', TypeInfo(G700Interface.JPrinter_BarcodeType));
  TRegTypes.RegisterType('G700Interface.JPrinter_BarcodeWidth', TypeInfo(G700Interface.JPrinter_BarcodeWidth));
  TRegTypes.RegisterType('G700Interface.JPrinter_Font', TypeInfo(G700Interface.JPrinter_Font));
  TRegTypes.RegisterType('G700Interface.JRspCode', TypeInfo(G700Interface.JRspCode));
  TRegTypes.RegisterType('G700Interface.Ja_1', TypeInfo(G700Interface.Ja_1));
  TRegTypes.RegisterType('G700Interface.Ja_2', TypeInfo(G700Interface.Ja_2));
  TRegTypes.RegisterType('G700Interface.Jlibbasebinder_a_a', TypeInfo(G700Interface.Jlibbasebinder_a_a));
  TRegTypes.RegisterType('G700Interface.Jlibbasebinder_b', TypeInfo(G700Interface.Jlibbasebinder_b));
end;

initialization
  RegisterTypes;
end.

