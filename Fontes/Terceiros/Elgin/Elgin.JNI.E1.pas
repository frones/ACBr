
unit Elgin.JNI.E1;

interface

uses
  Androidapi.JNIBridge,
  Androidapi.JNI.App,
  Androidapi.JNI.Bluetooth,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Java.Net,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Os,
  Androidapi.JNI.Util;

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
  JBalanca = interface;//com.elgin.e1.Balanca.Balanca
  JBalanca_Config = interface;//com.elgin.e1.Balanca.Balanca$Config
  JBalanca_ConfigAltValues = interface;//com.elgin.e1.Balanca.Balanca$ConfigAltValues
  JBalanca_ModeloBalanca = interface;//com.elgin.e1.Balanca.Balanca$ModeloBalanca
  JBalanca_ProtocoloComunicacao = interface;//com.elgin.e1.Balanca.Balanca$ProtocoloComunicacao
  JBalancaE1 = interface;//com.elgin.e1.Balanca.BalancaE1
  JBalancas = interface;//com.elgin.e1.Balanca.Balancas
  JComm = interface;//com.elgin.e1.Balanca.Comm
  JComm_1 = interface;//com.elgin.e1.Balanca.Comm$1
  JComm_TipoConexao = interface;//com.elgin.e1.Balanca.Comm$TipoConexao
  JCommSerial = interface;//com.elgin.e1.Balanca.CommSerial
  JCommTCP = interface;//com.elgin.e1.Balanca.CommTCP
  JCommTCP_Timeouts = interface;//com.elgin.e1.Balanca.CommTCP$Timeouts
  JInterfaceBalanca = interface;//com.elgin.e1.Balanca.InterfaceBalanca
  JImplementacaoBalanca = interface;//com.elgin.e1.Balanca.ImplementacaoBalanca
  JImplementacaoBalanca_1 = interface;//com.elgin.e1.Balanca.ImplementacaoBalanca$1
  Je1_BuildConfig = interface;//com.elgin.e1.BuildConfig
  JConexao = interface;//com.elgin.e1.Comunicacao.Conexao
  JConBluetooth = interface;//com.elgin.e1.Comunicacao.ConBluetooth
  JConBluetooth_1GetBluetoothData = interface;//com.elgin.e1.Comunicacao.ConBluetooth$1GetBluetoothData
  JConBluetooth_1GetPrinterBluetooth = interface;//com.elgin.e1.Comunicacao.ConBluetooth$1GetPrinterBluetooth
  JConBluetooth_1SendData = interface;//com.elgin.e1.Comunicacao.ConBluetooth$1SendData
  JConM8 = interface;//com.elgin.e1.Comunicacao.ConM8
  JPrinterManager_PrinterManagerListener = interface;//com.elgin.minipdvm8.PrinterManager$PrinterManagerListener
  JConM8_1 = interface;//com.elgin.e1.Comunicacao.ConM8$1
  JConSerial = interface;//com.elgin.e1.Comunicacao.ConSerial
  JConService = interface;//com.elgin.e1.Comunicacao.ConService
  JConService_1GetData = interface;//com.elgin.e1.Comunicacao.ConService$1GetData
  JConService_1GetPrinter = interface;//com.elgin.e1.Comunicacao.ConService$1GetPrinter
  JConService_1SendData = interface;//com.elgin.e1.Comunicacao.ConService$1SendData
  JConService_2GetData = interface;//com.elgin.e1.Comunicacao.ConService$2GetData
  JConSmartPOS = interface;//com.elgin.e1.Comunicacao.ConSmartPOS
  JConTCP_IP = interface;//com.elgin.e1.Comunicacao.ConTCP_IP
  JConTCP_IP_1GetData = interface;//com.elgin.e1.Comunicacao.ConTCP_IP$1GetData
  JConTCP_IP_1GetPrinter = interface;//com.elgin.e1.Comunicacao.ConTCP_IP$1GetPrinter
  JConTCP_IP_1SendData = interface;//com.elgin.e1.Comunicacao.ConTCP_IP$1SendData
  JConUSB = interface;//com.elgin.e1.Comunicacao.ConUSB
  JInterfaceFactoryXMLSAT = interface;//com.elgin.e1.Fiscal.InterfaceFactoryXMLSAT
  JImplementacaoFactoryXMLSAT = interface;//com.elgin.e1.Fiscal.ImplementacaoFactoryXMLSAT
  JInterfaceSAT = interface;//com.elgin.e1.Fiscal.InterfaceSAT
  JImplementacaoSAT = interface;//com.elgin.e1.Fiscal.ImplementacaoSAT
  JMFe = interface;//com.elgin.e1.Fiscal.MFe
  JNFCe = interface;//com.elgin.e1.Fiscal.NFCe
  JSAT = interface;//com.elgin.e1.Fiscal.SAT
  JAndroid = interface;//com.elgin.e1.Impressora.Android
  JdsImpressora = interface;//com.elgin.e1.Impressora.Config.dsImpressora
  JdsImpressora_1 = interface;//com.elgin.e1.Impressora.Config.dsImpressora$1
  JdsImpressora_infoHW = interface;//com.elgin.e1.Impressora.Config.dsImpressora$infoHW
  JdsSAT = interface;//com.elgin.e1.Impressora.Config.dsSAT
  JdsSAT_1 = interface;//com.elgin.e1.Impressora.Config.dsSAT$1
  JdsSAT_ChaveDePesquisa = interface;//com.elgin.e1.Impressora.Config.dsSAT$ChaveDePesquisa
  JEtiqueta = interface;//com.elgin.e1.Impressora.Etiqueta
  JInterfaceAndroid = interface;//com.elgin.e1.Impressora.Impressoras.InterfaceAndroid
  JImplementacaoAndroid = interface;//com.elgin.e1.Impressora.Impressoras.ImplementacaoAndroid
  JImplementacaoAndroid_IIImpressaoTexto = interface;//com.elgin.e1.Impressora.Impressoras.ImplementacaoAndroid$IIImpressaoTexto
  JInterfaceBematech = interface;//com.elgin.e1.Impressora.Impressoras.InterfaceBematech
  JImplementacaoBematech = interface;//com.elgin.e1.Impressora.Impressoras.ImplementacaoBematech
  JInterfaceEtiqueta = interface;//com.elgin.e1.Impressora.Impressoras.InterfaceEtiqueta
  JImplementacaoEtiqueta = interface;//com.elgin.e1.Impressora.Impressoras.ImplementacaoEtiqueta
  JImplementacaoM8 = interface;//com.elgin.e1.Impressora.Impressoras.ImplementacaoM8
  JImplementacaoM8_1 = interface;//com.elgin.e1.Impressora.Impressoras.ImplementacaoM8$1
  JImplementacaoSmartPOS = interface;//com.elgin.e1.Impressora.Impressoras.ImplementacaoSmartPOS
  JImplementacaoSmartPOS_1 = interface;//com.elgin.e1.Impressora.Impressoras.ImplementacaoSmartPOS$1
  JInterfaceTermica = interface;//com.elgin.e1.Impressora.Impressoras.InterfaceTermica
  JImplementacaoTermica = interface;//com.elgin.e1.Impressora.Impressoras.ImplementacaoTermica
  JInterfaceM8 = interface;//com.elgin.e1.Impressora.Impressoras.InterfaceM8
  JInterfaceSmartPOS = interface;//com.elgin.e1.Impressora.Impressoras.InterfaceSmartPOS
  JMiniPDVM8 = interface;//com.elgin.e1.Impressora.MiniPDVM8
  JSmart = interface;//com.elgin.e1.Impressora.Smart
  JTermica = interface;//com.elgin.e1.Impressora.Termica
  JCodigoErro = interface;//com.elgin.e1.Impressora.Utilidades.CodigoErro
  JESCPOS = interface;//com.elgin.e1.Impressora.Utilidades.ESCPOS
  JInteiro = interface;//com.elgin.e1.Impressora.Utilidades.Inteiro
  JPPLA = interface;//com.elgin.e1.Impressora.Utilidades.PPLA
  JUtilidades = interface;//com.elgin.e1.Impressora.Utilidades.Utilidades
  JNodeList = interface;//org.w3c.dom.NodeList
  JUtilidades_1 = interface;//com.elgin.e1.Impressora.Utilidades.Utilidades$1
  JInterfaceOBJXMLPRODUTO = interface;//com.elgin.e1.Impressora.XML.InterfaceOBJXMLPRODUTO
  JImplementacaoOBJXMLPRODUTO = interface;//com.elgin.e1.Impressora.XML.ImplementacaoOBJXMLPRODUTO
  JImplementacaoOBJPRODUTOXMLNFCE = interface;//com.elgin.e1.Impressora.XML.ImplementacaoOBJPRODUTOXMLNFCE
  JImplementacaoOBJPRODUTOXMLSAT = interface;//com.elgin.e1.Impressora.XML.ImplementacaoOBJPRODUTOXMLSAT
  JInterfaceOBJXML = interface;//com.elgin.e1.Impressora.XML.InterfaceOBJXML
  JImplementacaoOBJXML = interface;//com.elgin.e1.Impressora.XML.ImplementacaoOBJXML
  JImplementacaoOBJXML_1 = interface;//com.elgin.e1.Impressora.XML.ImplementacaoOBJXML$1
  JImplementacaoOBJXML_infoPag = interface;//com.elgin.e1.Impressora.XML.ImplementacaoOBJXML$infoPag
  JImplementacaoOBJXMLCANCELAMENTO = interface;//com.elgin.e1.Impressora.XML.ImplementacaoOBJXMLCANCELAMENTO
  JImplementacaoOBJXMLNFCE = interface;//com.elgin.e1.Impressora.XML.ImplementacaoOBJXMLNFCE
  JImplementacaoOBJXMLSAT = interface;//com.elgin.e1.Impressora.XML.ImplementacaoOBJXMLSAT
  JInterfaceOBJPRODUTOXMLNFCE = interface;//com.elgin.e1.Impressora.XML.InterfaceOBJPRODUTOXMLNFCE
  JInterfaceOBJPRODUTOXMLSAT = interface;//com.elgin.e1.Impressora.XML.InterfaceOBJPRODUTOXMLSAT
  JInterfaceOBJXMLCANCELAMENTO = interface;//com.elgin.e1.Impressora.XML.InterfaceOBJXMLCANCELAMENTO
  JInterfaceOBJXMLNFCE = interface;//com.elgin.e1.Impressora.XML.InterfaceOBJXMLNFCE
  JInterfaceOBJXMLSAT = interface;//com.elgin.e1.Impressora.XML.InterfaceOBJXMLSAT
  JScanner_Scanner = interface;//com.elgin.e1.Scanner.Scanner
  JAssinaturas = interface;//com.elgin.e1.Servico.Assinaturas
  JParametros = interface;//com.elgin.e1.Servico.Parametros
  JServicoE1 = interface;//com.elgin.e1.Servico.ServicoE1
  JServicoE1_Etiqueta = interface;//com.elgin.e1.Servico.ServicoE1$Etiqueta
  JServicoE1_SAT = interface;//com.elgin.e1.Servico.ServicoE1$SAT
  JServicoE1_Termica = interface;//com.elgin.e1.Servico.ServicoE1$Termica
  Jminipdvm8_BuildConfig = interface;//com.elgin.minipdvm8.BuildConfig
  JPrinterManager = interface;//com.elgin.minipdvm8.PrinterManager
  JPrinterManager_1 = interface;//com.elgin.minipdvm8.PrinterManager$1
  JIPrinterCallback_Stub = interface;//com.xcheng.printerservice.IPrinterCallback$Stub
  JPrinterManager_2 = interface;//com.elgin.minipdvm8.PrinterManager$2
  JCommSerialAPI = interface;//com.xc.comportdemo.CommSerialAPI
  JComportNative = interface;//com.xc.comportdemo.ComportNative
  JIPrinterCallback = interface;//com.xcheng.printerservice.IPrinterCallback
  JIPrinterCallback_Stub_Proxy = interface;//com.xcheng.printerservice.IPrinterCallback$Stub$Proxy
  JIPrinterService = interface;//com.xcheng.printerservice.IPrinterService
  JIPrinterService_Stub = interface;//com.xcheng.printerservice.IPrinterService$Stub
  JIPrinterService_Stub_Proxy = interface;//com.xcheng.printerservice.IPrinterService$Stub$Proxy
  JNode = interface;//org.w3c.dom.Node
  JAttr = interface;//org.w3c.dom.Attr
  JCharacterData = interface;//org.w3c.dom.CharacterData
  JText = interface;//org.w3c.dom.Text
  JCDATASection = interface;//org.w3c.dom.CDATASection
  JComment = interface;//org.w3c.dom.Comment
  JDOMConfiguration = interface;//org.w3c.dom.DOMConfiguration
  JDOMImplementation = interface;//org.w3c.dom.DOMImplementation
  JDOMStringList = interface;//org.w3c.dom.DOMStringList
  JDocument = interface;//org.w3c.dom.Document
  JDocumentFragment = interface;//org.w3c.dom.DocumentFragment
  JDocumentType = interface;//org.w3c.dom.DocumentType
  JElement = interface;//org.w3c.dom.Element
  JEntityReference = interface;//org.w3c.dom.EntityReference
  JNamedNodeMap = interface;//org.w3c.dom.NamedNodeMap
  JProcessingInstruction = interface;//org.w3c.dom.ProcessingInstruction
  JTypeInfo = interface;//org.w3c.dom.TypeInfo
  JUserDataHandler = interface;//org.w3c.dom.UserDataHandler

// ===== Interface declarations =====

  JAnimatorClass = interface(JObjectClass)
    ['{3F76A5DF-389E-4BD3-9861-04C5B00CEADE}']
    {class} function init: JAnimator; cdecl;//Deprecated
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
    {class} function init: JKeyframe; cdecl;//Deprecated
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
    {class} function init: JLayoutTransition; cdecl;//Deprecated
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
    {class} function init: JStateListAnimator; cdecl;//Deprecated
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
    {class} function init(fromClass: Jlang_Class; toClass: Jlang_Class): JTypeConverter; cdecl;//Deprecated
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
    {class} function init: JValueAnimator; cdecl;//Deprecated
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
    {class} function init: JPathMotion; cdecl; overload;//Deprecated
    {class} function init(context: JContext; attrs: JAttributeSet): JPathMotion; cdecl; overload;//Deprecated
  end;

  [JavaSignature('android/transition/PathMotion')]
  JPathMotion = interface(JObject)
    ['{BDC08353-C9FB-42D7-97CC-C35837D2F536}']
    function getPath(startX: Single; startY: Single; endX: Single; endY: Single): JPath; cdecl;
  end;
  TJPathMotion = class(TJavaGenericImport<JPathMotionClass, JPathMotion>) end;

  JSceneClass = interface(JObjectClass)
    ['{8B9120CA-AEEA-4DE3-BDC9-15CFD23A7B07}']
    {class} function init(sceneRoot: JViewGroup): JScene; cdecl; overload;//Deprecated
    {class} function init(sceneRoot: JViewGroup; layout: JView): JScene; cdecl; overload;//Deprecated
    {class} function init(sceneRoot: JViewGroup; layout: JViewGroup): JScene; cdecl; overload;//Deprecated
    {class} procedure exit; cdecl;//Deprecated
    {class} function getSceneForLayout(sceneRoot: JViewGroup; layoutId: Integer; context: JContext): JScene; cdecl;//Deprecated
    {class} function getSceneRoot: JViewGroup; cdecl;//Deprecated
  end;

  [JavaSignature('android/transition/Scene')]
  JScene = interface(JObject)
    ['{85A60B99-5837-4F1F-A344-C627DD586B82}']
    procedure enter; cdecl;//Deprecated
    procedure setEnterAction(action: JRunnable); cdecl;//Deprecated
    procedure setExitAction(action: JRunnable); cdecl;//Deprecated
  end;
  TJScene = class(TJavaGenericImport<JSceneClass, JScene>) end;

  JTransitionClass = interface(JObjectClass)
    ['{60EC06BC-8F7A-4416-A04B-5B57987EB18E}']
    {class} function _GetMATCH_ID: Integer; cdecl;
    {class} function _GetMATCH_INSTANCE: Integer; cdecl;
    {class} function _GetMATCH_ITEM_ID: Integer; cdecl;
    {class} function _GetMATCH_NAME: Integer; cdecl;
    {class} function init: JTransition; cdecl; overload;//Deprecated
    {class} function init(context: JContext; attrs: JAttributeSet): JTransition; cdecl; overload;//Deprecated
    {class} function addListener(listener: JTransition_TransitionListener): JTransition; cdecl;
    {class} function addTarget(targetId: Integer): JTransition; cdecl; overload;
    {class} function canRemoveViews: Boolean; cdecl;
    {class} procedure captureEndValues(transitionValues: JTransitionValues); cdecl;
    {class} procedure captureStartValues(transitionValues: JTransitionValues); cdecl;
    {class} function excludeChildren(target: JView; exclude: Boolean): JTransition; cdecl; overload;
    {class} function excludeChildren(type_: Jlang_Class; exclude: Boolean): JTransition; cdecl; overload;
    {class} function excludeTarget(targetId: Integer; exclude: Boolean): JTransition; cdecl; overload;
    {class} function getDuration: Int64; cdecl;
    {class} function getEpicenter: JRect; cdecl;
    {class} function getEpicenterCallback: JTransition_EpicenterCallback; cdecl;
    {class} function getPathMotion: JPathMotion; cdecl;//Deprecated
    {class} function getPropagation: JTransitionPropagation; cdecl;//Deprecated
    {class} function getStartDelay: Int64; cdecl;//Deprecated
    {class} function getTargets: JList; cdecl;//Deprecated
    {class} function getTransitionProperties: TJavaObjectArray<JString>; cdecl;//Deprecated
    {class} function getTransitionValues(view: JView; start: Boolean): JTransitionValues; cdecl;//Deprecated
    {class} function removeTarget(targetName: JString): JTransition; cdecl; overload;//Deprecated
    {class} function removeTarget(target: JView): JTransition; cdecl; overload;//Deprecated
    {class} function removeTarget(target: Jlang_Class): JTransition; cdecl; overload;//Deprecated
    {class} procedure setPathMotion(pathMotion: JPathMotion); cdecl;//Deprecated
    {class} procedure setPropagation(transitionPropagation: JTransitionPropagation); cdecl;//Deprecated
    {class} property MATCH_ID: Integer read _GetMATCH_ID;
    {class} property MATCH_INSTANCE: Integer read _GetMATCH_INSTANCE;
    {class} property MATCH_ITEM_ID: Integer read _GetMATCH_ITEM_ID;
    {class} property MATCH_NAME: Integer read _GetMATCH_NAME;
  end;

  [JavaSignature('android/transition/Transition')]
  JTransition = interface(JObject)
    ['{C2F8200F-1C83-40AE-8C5B-C0C8BFF17F88}']
    function addTarget(targetName: JString): JTransition; cdecl; overload;
    function addTarget(targetType: Jlang_Class): JTransition; cdecl; overload;
    function addTarget(target: JView): JTransition; cdecl; overload;
    function clone: JTransition; cdecl;
    function createAnimator(sceneRoot: JViewGroup; startValues: JTransitionValues; endValues: JTransitionValues): JAnimator; cdecl;
    function excludeChildren(targetId: Integer; exclude: Boolean): JTransition; cdecl; overload;
    function excludeTarget(targetName: JString; exclude: Boolean): JTransition; cdecl; overload;
    function excludeTarget(target: JView; exclude: Boolean): JTransition; cdecl; overload;
    function excludeTarget(type_: Jlang_Class; exclude: Boolean): JTransition; cdecl; overload;
    function getInterpolator: JTimeInterpolator; cdecl;//Deprecated
    function getName: JString; cdecl;//Deprecated
    function getTargetIds: JList; cdecl;//Deprecated
    function getTargetNames: JList; cdecl;//Deprecated
    function getTargetTypes: JList; cdecl;//Deprecated
    function isTransitionRequired(startValues: JTransitionValues; endValues: JTransitionValues): Boolean; cdecl;//Deprecated
    function removeListener(listener: JTransition_TransitionListener): JTransition; cdecl;//Deprecated
    function removeTarget(targetId: Integer): JTransition; cdecl; overload;//Deprecated
    function setDuration(duration: Int64): JTransition; cdecl;//Deprecated
    procedure setEpicenterCallback(epicenterCallback: JTransition_EpicenterCallback); cdecl;//Deprecated
    function setInterpolator(interpolator: JTimeInterpolator): JTransition; cdecl;//Deprecated
    function setStartDelay(startDelay: Int64): JTransition; cdecl;
    function toString: JString; cdecl;
  end;
  TJTransition = class(TJavaGenericImport<JTransitionClass, JTransition>) end;

  JTransition_EpicenterCallbackClass = interface(JObjectClass)
    ['{8141257A-130B-466C-A08D-AA3A00946F4C}']
    {class} function init: JTransition_EpicenterCallback; cdecl;//Deprecated
  end;

  [JavaSignature('android/transition/Transition$EpicenterCallback')]
  JTransition_EpicenterCallback = interface(JObject)
    ['{CE004917-266F-4076-8913-F23184824FBA}']
    function onGetEpicenter(transition: JTransition): JRect; cdecl;
  end;
  TJTransition_EpicenterCallback = class(TJavaGenericImport<JTransition_EpicenterCallbackClass, JTransition_EpicenterCallback>) end;

  JTransition_TransitionListenerClass = interface(IJavaClass)
    ['{D5083752-E8A6-46DF-BE40-AE11073C387E}']
    {class} procedure onTransitionEnd(transition: JTransition); cdecl;
    {class} procedure onTransitionPause(transition: JTransition); cdecl;
    {class} procedure onTransitionResume(transition: JTransition); cdecl;
  end;

  [JavaSignature('android/transition/Transition$TransitionListener')]
  JTransition_TransitionListener = interface(IJavaInstance)
    ['{C32BE107-6E05-4898-AF0A-FAD970D66E29}']
    procedure onTransitionCancel(transition: JTransition); cdecl;
    procedure onTransitionStart(transition: JTransition); cdecl;
  end;
  TJTransition_TransitionListener = class(TJavaGenericImport<JTransition_TransitionListenerClass, JTransition_TransitionListener>) end;

  JTransitionManagerClass = interface(JObjectClass)
    ['{4160EFA0-3499-4964-817E-46497BB5B957}']
    {class} function init: JTransitionManager; cdecl;//Deprecated
    {class} procedure beginDelayedTransition(sceneRoot: JViewGroup); cdecl; overload;//Deprecated
    {class} procedure beginDelayedTransition(sceneRoot: JViewGroup; transition: JTransition); cdecl; overload;//Deprecated
    {class} procedure endTransitions(sceneRoot: JViewGroup); cdecl;//Deprecated
    {class} procedure go(scene: JScene); cdecl; overload;//Deprecated
    {class} procedure go(scene: JScene; transition: JTransition); cdecl; overload;//Deprecated
    {class} procedure transitionTo(scene: JScene); cdecl;//Deprecated
  end;

  [JavaSignature('android/transition/TransitionManager')]
  JTransitionManager = interface(JObject)
    ['{FF5E1210-1F04-4F81-9CAC-3D7A5C4E972B}']
    procedure setTransition(scene: JScene; transition: JTransition); cdecl; overload;//Deprecated
    procedure setTransition(fromScene: JScene; toScene: JScene; transition: JTransition); cdecl; overload;//Deprecated
  end;
  TJTransitionManager = class(TJavaGenericImport<JTransitionManagerClass, JTransitionManager>) end;

  JTransitionPropagationClass = interface(JObjectClass)
    ['{A881388A-C877-4DD9-9BAD-1BA4F56133EE}']
    {class} function init: JTransitionPropagation; cdecl;//Deprecated
    {class} function getStartDelay(sceneRoot: JViewGroup; transition: JTransition; startValues: JTransitionValues; endValues: JTransitionValues): Int64; cdecl;
  end;

  [JavaSignature('android/transition/TransitionPropagation')]
  JTransitionPropagation = interface(JObject)
    ['{7595B7EF-6BCE-4281-BC67-335E2FB6C091}']
    procedure captureValues(transitionValues: JTransitionValues); cdecl;
    function getPropagationProperties: TJavaObjectArray<JString>; cdecl;
  end;
  TJTransitionPropagation = class(TJavaGenericImport<JTransitionPropagationClass, JTransitionPropagation>) end;

  JTransitionValuesClass = interface(JObjectClass)
    ['{3BF98CFA-6A4D-4815-8D42-15E97C916D91}']
    {class} function _Getview: JView; cdecl;
    {class} function init: JTransitionValues; cdecl;//Deprecated
    {class} function equals(other: JObject): Boolean; cdecl;
    {class} property view: JView read _Getview;
  end;

  [JavaSignature('android/transition/TransitionValues')]
  JTransitionValues = interface(JObject)
    ['{178E09E6-D32C-48A9-ADCF-8CCEA804052A}']
    function _Getvalues: JMap; cdecl;
    function hashCode: Integer; cdecl;
    function toString: JString; cdecl;
    property values: JMap read _Getvalues;
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
    {class} function init(c: JContext; attrs: JAttributeSet): JToolbar_LayoutParams; cdecl; overload;//Deprecated
    {class} function init(width: Integer; height: Integer): JToolbar_LayoutParams; cdecl; overload;//Deprecated
    {class} function init(width: Integer; height: Integer; gravity: Integer): JToolbar_LayoutParams; cdecl; overload;//Deprecated
    {class} function init(gravity: Integer): JToolbar_LayoutParams; cdecl; overload;//Deprecated
    {class} function init(source: JToolbar_LayoutParams): JToolbar_LayoutParams; cdecl; overload;//Deprecated
    {class} function init(source: JActionBar_LayoutParams): JToolbar_LayoutParams; cdecl; overload;//Deprecated
    {class} function init(source: JViewGroup_MarginLayoutParams): JToolbar_LayoutParams; cdecl; overload;//Deprecated
    {class} function init(source: JViewGroup_LayoutParams): JToolbar_LayoutParams; cdecl; overload;//Deprecated
  end;

  [JavaSignature('android/widget/Toolbar$LayoutParams')]
  JToolbar_LayoutParams = interface(JActionBar_LayoutParams)
    ['{BCD101F9-B7B7-4B2F-9460-056B3EA7B9F0}']
  end;
  TJToolbar_LayoutParams = class(TJavaGenericImport<JToolbar_LayoutParamsClass, JToolbar_LayoutParams>) end;

  JBalancaClass = interface(JObjectClass)
    ['{483AB65D-5BAB-41E2-A7DC-ADCE97A3A64C}']
  end;

  [JavaSignature('com/elgin/e1/Balanca/Balanca')]
  JBalanca = interface(JObject)
    ['{67CDE8AE-BFAC-48AC-B99C-E19356D3072F}']
  end;
  TJBalanca = class(TJavaGenericImport<JBalancaClass, JBalanca>) end;

  JBalanca_ConfigClass = interface(JObjectClass)
    ['{22E676C2-AB2D-4B13-A3E9-DDA4462D8613}']
    {class} function init(P1: Integer; P2: Integer; P3: Integer; P4: Integer): JBalanca_Config; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Balanca/Balanca$Config')]
  JBalanca_Config = interface(JObject)
    ['{0F3D4B99-F872-4702-9D61-89B8CBE00A05}']
    function _Getlength: Integer; cdecl;
    function _Getparity: Integer; cdecl;
    function _Getstopbits: Integer; cdecl;
    property length: Integer read _Getlength;
    property parity: Integer read _Getparity;
    property stopbits: Integer read _Getstopbits;
  end;
  TJBalanca_Config = class(TJavaGenericImport<JBalanca_ConfigClass, JBalanca_Config>) end;

  JBalanca_ConfigAltValuesClass = interface(JEnumClass)
    ['{E9B34596-598C-422B-BA62-AFF82D7BEDB5}']
    {class} function _GetEvenParity: JBalanca_ConfigAltValues; cdecl;
    {class} function _GetNenhum: JBalanca_ConfigAltValues; cdecl;
    {class} function _GetNoParity: JBalanca_ConfigAltValues; cdecl;
    {class} function _GetOddParity: JBalanca_ConfigAltValues; cdecl;
    {class} function _GetQualquer: JBalanca_ConfigAltValues; cdecl;
    {class} function valueOf(P1: JString): JBalanca_ConfigAltValues; cdecl;
    {class} function values: TJavaObjectArray<JBalanca_ConfigAltValues>; cdecl;
    {class} property EvenParity: JBalanca_ConfigAltValues read _GetEvenParity;
    {class} property Nenhum: JBalanca_ConfigAltValues read _GetNenhum;
    {class} property NoParity: JBalanca_ConfigAltValues read _GetNoParity;
    {class} property OddParity: JBalanca_ConfigAltValues read _GetOddParity;
    {class} property Qualquer: JBalanca_ConfigAltValues read _GetQualquer;
  end;

  [JavaSignature('com/elgin/e1/Balanca/Balanca$ConfigAltValues')]
  JBalanca_ConfigAltValues = interface(JEnum)
    ['{D923213A-1972-4101-BDA7-FA3565091796}']
    function _Getvalor: Integer; cdecl;
    property valor: Integer read _Getvalor;
  end;
  TJBalanca_ConfigAltValues = class(TJavaGenericImport<JBalanca_ConfigAltValuesClass, JBalanca_ConfigAltValues>) end;

  JBalanca_ModeloBalancaClass = interface(JEnumClass)
    ['{50C0FB5D-A4A6-493D-BCF3-DB7626F9F4AF}']
    {class} function _GetDP3005: JBalanca_ModeloBalanca; cdecl;
    {class} function _GetDPSC: JBalanca_ModeloBalanca; cdecl;
    {class} function _GetSA110: JBalanca_ModeloBalanca; cdecl;
    {class} function _GetSemBalanca: JBalanca_ModeloBalanca; cdecl;
    {class} function valueOf(P1: JString): JBalanca_ModeloBalanca; cdecl;
    {class} function values: TJavaObjectArray<JBalanca_ModeloBalanca>; cdecl;
    {class} property DP3005: JBalanca_ModeloBalanca read _GetDP3005;
    {class} property DPSC: JBalanca_ModeloBalanca read _GetDPSC;
    {class} property SA110: JBalanca_ModeloBalanca read _GetSA110;
    {class} property SemBalanca: JBalanca_ModeloBalanca read _GetSemBalanca;
  end;

  [JavaSignature('com/elgin/e1/Balanca/Balanca$ModeloBalanca')]
  JBalanca_ModeloBalanca = interface(JEnum)
    ['{7B6DC4E6-844B-4B26-8151-E1247CE391B3}']
    function _Getvalor: Integer; cdecl;
    property valor: Integer read _Getvalor;
  end;
  TJBalanca_ModeloBalanca = class(TJavaGenericImport<JBalanca_ModeloBalancaClass, JBalanca_ModeloBalanca>) end;

  JBalanca_ProtocoloComunicacaoClass = interface(JEnumClass)
    ['{30393895-DAF8-4116-8793-BDD7451E81E5}']
    {class} function _GetProtocolo0: JBalanca_ProtocoloComunicacao; cdecl;
    {class} function _GetProtocolo1: JBalanca_ProtocoloComunicacao; cdecl;
    {class} function _GetProtocolo2: JBalanca_ProtocoloComunicacao; cdecl;
    {class} function _GetProtocolo3: JBalanca_ProtocoloComunicacao; cdecl;
    {class} function _GetProtocolo4: JBalanca_ProtocoloComunicacao; cdecl;
    {class} function _GetProtocolo5: JBalanca_ProtocoloComunicacao; cdecl;
    {class} function _GetProtocolo7: JBalanca_ProtocoloComunicacao; cdecl;
    {class} function _GetSemProtocolo: JBalanca_ProtocoloComunicacao; cdecl;
    {class} function valueOf(P1: JString): JBalanca_ProtocoloComunicacao; cdecl;
    {class} function values: TJavaObjectArray<JBalanca_ProtocoloComunicacao>; cdecl;
    {class} property Protocolo0: JBalanca_ProtocoloComunicacao read _GetProtocolo0;
    {class} property Protocolo1: JBalanca_ProtocoloComunicacao read _GetProtocolo1;
    {class} property Protocolo2: JBalanca_ProtocoloComunicacao read _GetProtocolo2;
    {class} property Protocolo3: JBalanca_ProtocoloComunicacao read _GetProtocolo3;
    {class} property Protocolo4: JBalanca_ProtocoloComunicacao read _GetProtocolo4;
    {class} property Protocolo5: JBalanca_ProtocoloComunicacao read _GetProtocolo5;
    {class} property Protocolo7: JBalanca_ProtocoloComunicacao read _GetProtocolo7;
    {class} property SemProtocolo: JBalanca_ProtocoloComunicacao read _GetSemProtocolo;
  end;

  [JavaSignature('com/elgin/e1/Balanca/Balanca$ProtocoloComunicacao')]
  JBalanca_ProtocoloComunicacao = interface(JEnum)
    ['{93CA1C49-3933-49C2-B200-C9FC7A768EC3}']
    function _Getvalor: Integer; cdecl;
    property valor: Integer read _Getvalor;
  end;
  TJBalanca_ProtocoloComunicacao = class(TJavaGenericImport<JBalanca_ProtocoloComunicacaoClass, JBalanca_ProtocoloComunicacao>) end;

  JBalancaE1Class = interface(JObjectClass)
    ['{73B45EF3-2EBC-48DE-9E39-8CCC9AD0BACB}']
    {class} function AbrirSerial(P1: Integer; P2: Integer; P3: Char; P4: Integer): Integer; cdecl;
    {class} function ConfigurarModeloBalanca(P1: Integer): Integer; cdecl;
    {class} function ConfigurarProtocoloComunicacao(P1: Integer): Integer; cdecl;
    {class} function DirectIO(P1: TJavaArray<Byte>; P2: Integer; P3: TJavaArray<Byte>; P4: Integer; P5: Boolean): Integer; cdecl;
    {class} function Fechar: Integer; cdecl;
    {class} function GetContinuousReadTime: Integer; cdecl;
    {class} function LerPeso(P1: Integer): JString; cdecl;
    {class} function LerPreco(P1: Integer): JString; cdecl;
    {class} function LerTara: JString; cdecl;
    {class} function LerTotal(P1: Double): JString; cdecl;
    {class} function LigarDesligarDisplay: Integer; cdecl;
    {class} function ObterModeloBalanca: Integer; cdecl;
    {class} function ObterProtocoloComunicacao: Integer; cdecl;
    {class} function ObterTipoConexao: Integer; cdecl;
    {class} procedure SetContinuousReadTime(P1: Integer); cdecl;
    {class} function TararBalanca: Integer; cdecl;
    {class} function ZerarBalanca: Integer; cdecl;
    {class} function init: JBalancaE1; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Balanca/BalancaE1')]
  JBalancaE1 = interface(JObject)
    ['{39C86186-DD3B-4EB1-9898-011CAE0FD7B3}']
  end;
  TJBalancaE1 = class(TJavaGenericImport<JBalancaE1Class, JBalancaE1>) end;

  JBalancasClass = interface(JObjectClass)
    ['{614D2A46-A607-4986-AE7A-75AFDFAAAC04}']
  end;

  [JavaSignature('com/elgin/e1/Balanca/Balancas')]
  JBalancas = interface(JObject)
    ['{0EDC0EFC-FFB7-4B80-90CA-AC365B99B603}']
  end;
  TJBalancas = class(TJavaGenericImport<JBalancasClass, JBalancas>) end;

  JCommClass = interface(JObjectClass)
    ['{33733BC3-089D-48BB-ABC1-BF6C710C4F1D}']
  end;

  [JavaSignature('com/elgin/e1/Balanca/Comm')]
  JComm = interface(JObject)
    ['{74AE423E-4B96-4B41-BFA4-7A5EB20E080D}']
  end;
  TJComm = class(TJavaGenericImport<JCommClass, JComm>) end;

  JComm_1Class = interface(JObjectClass)
    ['{0F04E442-06C0-4DA4-8530-1CB39298CC17}']
  end;

  [JavaSignature('com/elgin/e1/Balanca/Comm$1')]
  JComm_1 = interface(JObject)
    ['{D98989E2-D491-46CF-B5E2-1966C28257A5}']
  end;
  TJComm_1 = class(TJavaGenericImport<JComm_1Class, JComm_1>) end;

  JComm_TipoConexaoClass = interface(JEnumClass)
    ['{1491E33B-B09E-4B38-A92C-5789F2B6D750}']
    {class} function _GetConexaoSerial: JComm_TipoConexao; cdecl;
    {class} function _GetConexaoTCP: JComm_TipoConexao; cdecl;
    {class} function _GetSemConexao: JComm_TipoConexao; cdecl;
    {class} function valueOf(P1: JString): JComm_TipoConexao; cdecl;
    {class} function values: TJavaObjectArray<JComm_TipoConexao>; cdecl;
    {class} property ConexaoSerial: JComm_TipoConexao read _GetConexaoSerial;
    {class} property ConexaoTCP: JComm_TipoConexao read _GetConexaoTCP;
    {class} property SemConexao: JComm_TipoConexao read _GetSemConexao;
  end;

  [JavaSignature('com/elgin/e1/Balanca/Comm$TipoConexao')]
  JComm_TipoConexao = interface(JEnum)
    ['{C9231470-99F1-4EF5-83ED-D92DA523A7A3}']
    function _Getvalor: Integer; cdecl;
    property valor: Integer read _Getvalor;
  end;
  TJComm_TipoConexao = class(TJavaGenericImport<JComm_TipoConexaoClass, JComm_TipoConexao>) end;

  JCommSerialClass = interface(JObjectClass)
    ['{3FE3A275-BF70-4DEF-A2BA-B0DB0D9C67E9}']
    {class} function isAuthAPI: Boolean; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Balanca/CommSerial')]
  JCommSerial = interface(JObject)
    ['{5083E6DA-9BBC-4868-81D7-2E6F8B972A85}']
  end;
  TJCommSerial = class(TJavaGenericImport<JCommSerialClass, JCommSerial>) end;

  JCommTCPClass = interface(JObjectClass)
    ['{0B361547-D526-4928-AB0A-9A16856436C1}']
  end;

  [JavaSignature('com/elgin/e1/Balanca/CommTCP')]
  JCommTCP = interface(JObject)
    ['{427ED75E-58E9-4ED7-9FD2-7C9890092167}']
  end;
  TJCommTCP = class(TJavaGenericImport<JCommTCPClass, JCommTCP>) end;

  JCommTCP_TimeoutsClass = interface(JEnumClass)
    ['{82474D37-9029-44CD-9CF5-90485C8D9D13}']
    {class} function _GetTCPReadTimeout: JCommTCP_Timeouts; cdecl;
    {class} function _GetTCPWriteTimeout: JCommTCP_Timeouts; cdecl;
    {class} function valueOf(P1: JString): JCommTCP_Timeouts; cdecl;
    {class} function values: TJavaObjectArray<JCommTCP_Timeouts>; cdecl;
    {class} property TCPReadTimeout: JCommTCP_Timeouts read _GetTCPReadTimeout;
    {class} property TCPWriteTimeout: JCommTCP_Timeouts read _GetTCPWriteTimeout;
  end;

  [JavaSignature('com/elgin/e1/Balanca/CommTCP$Timeouts')]
  JCommTCP_Timeouts = interface(JEnum)
    ['{DED6A64E-8E66-439B-972E-6191482F137E}']
    function _Getvalor: Integer; cdecl;
    property valor: Integer read _Getvalor;
  end;
  TJCommTCP_Timeouts = class(TJavaGenericImport<JCommTCP_TimeoutsClass, JCommTCP_Timeouts>) end;

  JInterfaceBalancaClass = interface(IJavaClass)
    ['{B73851AE-3281-45EC-8548-1C41C3EEBD1E}']
    {class} function abrir(P1: JString; P2: Integer): Integer; cdecl; overload;
    {class} function abrir(P1: Integer; P2: Integer; P3: Char; P4: Integer): Integer; cdecl; overload;
    {class} function configurarModeloBalanca(P1: Integer): Integer; cdecl;
    {class} function configurarProtocoloComunicacao(P1: Integer): Integer; cdecl;
    {class} function directIO(P1: TJavaArray<Byte>; P2: Integer; P3: TJavaArray<Byte>; P4: Integer; P5: Boolean): Integer; cdecl;
    {class} function fechar: Integer; cdecl;
    {class} function getContinuousReadTime: Integer; cdecl;
    {class} function lerPeso(P1: Integer): JString; cdecl;
    {class} function lerPreco(P1: Integer): JString; cdecl;
    {class} function lerTara: JString; cdecl;
    {class} function lerTotal(P1: Double): JString; cdecl;
    {class} function ligarDesligarDisplay: Integer; cdecl;
    {class} function obterModeloBalanca: Integer; cdecl;
    {class} function obterProtocoloComunicacao: Integer; cdecl;
    {class} function obterTipoConexao: Integer; cdecl;
    {class} procedure setContinuousReadTime(P1: Integer); cdecl;
    {class} function tararBalanca: Integer; cdecl;
    {class} function zerarBalanca: Integer; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Balanca/InterfaceBalanca')]
  JInterfaceBalanca = interface(IJavaInstance)
    ['{483F1D4D-860E-43DC-80B5-4D40D31170D5}']
  end;
  TJInterfaceBalanca = class(TJavaGenericImport<JInterfaceBalancaClass, JInterfaceBalanca>) end;

  JImplementacaoBalancaClass = interface(JInterfaceBalancaClass)
    ['{B27849A6-ADD4-4746-96C3-E2698C84AD42}']
    {class} function abrir(P1: JString; P2: Integer): Integer; cdecl; overload;
    {class} function abrir(P1: Integer; P2: Integer; P3: Char; P4: Integer): Integer; cdecl; overload;
    {class} function configurarModeloBalanca(P1: Integer): Integer; cdecl;
    {class} function configurarProtocoloComunicacao(P1: Integer): Integer; cdecl;
    {class} function directIO(P1: TJavaArray<Byte>; P2: Integer; P3: TJavaArray<Byte>; P4: Integer; P5: Boolean): Integer; cdecl;
    {class} function fechar: Integer; cdecl;
    {class} function getContinuousReadTime: Integer; cdecl;
    {class} function lerPeso(P1: Integer): JString; cdecl;
    {class} function lerPreco(P1: Integer): JString; cdecl;
    {class} function lerTara: JString; cdecl;
    {class} function lerTotal(P1: Double): JString; cdecl;
    {class} function ligarDesligarDisplay: Integer; cdecl;
    {class} function obterModeloBalanca: Integer; cdecl;
    {class} function obterProtocoloComunicacao: Integer; cdecl;
    {class} function obterTipoConexao: Integer; cdecl;
    {class} procedure setContinuousReadTime(P1: Integer); cdecl;
    {class} function tararBalanca: Integer; cdecl;
    {class} function zerarBalanca: Integer; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Balanca/ImplementacaoBalanca')]
  JImplementacaoBalanca = interface(JInterfaceBalanca)
    ['{E94EF2A4-C463-44B8-93F8-016AE1B5530F}']
  end;
  TJImplementacaoBalanca = class(TJavaGenericImport<JImplementacaoBalancaClass, JImplementacaoBalanca>) end;

  JImplementacaoBalanca_1Class = interface(JObjectClass)
    ['{A238C791-4DD3-4534-A6B4-BA98A7AAF3DF}']
  end;

  [JavaSignature('com/elgin/e1/Balanca/ImplementacaoBalanca$1')]
  JImplementacaoBalanca_1 = interface(JObject)
    ['{15C3BD14-309F-4784-B8B8-5013BA5544B1}']
  end;
  TJImplementacaoBalanca_1 = class(TJavaGenericImport<JImplementacaoBalanca_1Class, JImplementacaoBalanca_1>) end;

  Je1_BuildConfigClass = interface(JObjectClass)
    ['{60A24AB9-263C-4218-BF03-4CC33FD8C5EF}']
    {class} function _GetAPPLICATION_ID: JString; cdecl;
    {class} function _GetBUILD_TYPE: JString; cdecl;
    {class} function _GetDEBUG: Boolean; cdecl;
    {class} function _GetFLAVOR: JString; cdecl;
    {class} function _GetVERSION_CODE: Integer; cdecl;
    {class} function _GetVERSION_NAME: JString; cdecl;
    {class} function init: Je1_BuildConfig; cdecl;
    {class} property APPLICATION_ID: JString read _GetAPPLICATION_ID;
    {class} property BUILD_TYPE: JString read _GetBUILD_TYPE;
    {class} property DEBUG: Boolean read _GetDEBUG;
    {class} property FLAVOR: JString read _GetFLAVOR;
    {class} property VERSION_CODE: Integer read _GetVERSION_CODE;
    {class} property VERSION_NAME: JString read _GetVERSION_NAME;
  end;

  [JavaSignature('com/elgin/e1/BuildConfig')]
  Je1_BuildConfig = interface(JObject)
    ['{ADD0AAFF-D6E5-43E7-9643-1723744765E1}']
  end;
  TJe1_BuildConfig = class(TJavaGenericImport<Je1_BuildConfigClass, Je1_BuildConfig>) end;

  JConexaoClass = interface(JObjectClass)
    ['{AA9EE03B-10E8-4126-9908-9AEEDAFC24AD}']
    {class} function _GetBLUETOOTH_MAX_RETURN_LENGTH: Integer; cdecl;
    {class} function _GetBLUETOOTH_WTIMEOUT: Integer; cdecl;
    {class} function _GetCONEXAO_BLUETOOTH: Integer; cdecl;
    {class} function _GetCONEXAO_M8: Integer; cdecl;
    {class} function _GetCONEXAO_SERIAL: Integer; cdecl;
    {class} function _GetCONEXAO_SERVICO: Integer; cdecl;
    {class} function _GetCONEXAO_SMARTPOS: Integer; cdecl;
    {class} function _GetCONEXAO_TCP_IP: Integer; cdecl;
    {class} function _GetCONEXAO_USB: Integer; cdecl;
    {class} function _GetMAX_RECONNECTIONS: Integer; cdecl;
    {class} function _GetSEM_CONEXAO: Integer; cdecl;
    {class} function _GetSERVICO_DELAY_TIME: Integer; cdecl;
    {class} function _GetSERVICO_MAX_RETURN_LENGTH: Integer; cdecl;
    {class} function _GetSERVICO_RTIMEOUT: Integer; cdecl;
    {class} function _GetSERVICO_WTIMEOUT: Integer; cdecl;
    {class} function _GetSMARTPOS_MAX_RETURN_LENGTH: Integer; cdecl;
    {class} function _GetSMARTPOS_RTIMEOUT: Integer; cdecl;
    {class} function _GetTCP_IP_MAX_RETURN_LENGTH: Integer; cdecl;
    {class} function _GetTCP_IP_RTIMEOUT: Integer; cdecl;
    {class} function _GetTCP_IP_WTIMEOUT: Integer; cdecl;
    {class} function getNextBluetoothSocket: Integer; cdecl;
    {class} function getNextPrintDevice: Integer; cdecl;
    {class} function getNextService: Integer; cdecl;
    {class} function getNextSocket: Integer; cdecl;
    {class} function getPrtData: TJavaArray<Byte>; cdecl;
    {class} function getTipo: Integer; cdecl;
    {class} function init: JConexao; cdecl;
    {class} procedure setNextBluetoothSocket(P1: Integer); cdecl;
    {class} procedure setNextPrintDevice(P1: Integer); cdecl;
    {class} procedure setNextService(P1: Integer); cdecl;
    {class} procedure setNextSocket(P1: Integer); cdecl;
    {class} procedure setPrtData(P1: TJavaArray<Byte>); cdecl;
    {class} procedure setTipo(P1: Integer); cdecl;
    {class} property BLUETOOTH_MAX_RETURN_LENGTH: Integer read _GetBLUETOOTH_MAX_RETURN_LENGTH;
    {class} property BLUETOOTH_WTIMEOUT: Integer read _GetBLUETOOTH_WTIMEOUT;
    {class} property CONEXAO_BLUETOOTH: Integer read _GetCONEXAO_BLUETOOTH;
    {class} property CONEXAO_M8: Integer read _GetCONEXAO_M8;
    {class} property CONEXAO_SERIAL: Integer read _GetCONEXAO_SERIAL;
    {class} property CONEXAO_SERVICO: Integer read _GetCONEXAO_SERVICO;
    {class} property CONEXAO_SMARTPOS: Integer read _GetCONEXAO_SMARTPOS;
    {class} property CONEXAO_TCP_IP: Integer read _GetCONEXAO_TCP_IP;
    {class} property CONEXAO_USB: Integer read _GetCONEXAO_USB;
    {class} property MAX_RECONNECTIONS: Integer read _GetMAX_RECONNECTIONS;
    {class} property SEM_CONEXAO: Integer read _GetSEM_CONEXAO;
    {class} property SERVICO_DELAY_TIME: Integer read _GetSERVICO_DELAY_TIME;
    {class} property SERVICO_MAX_RETURN_LENGTH: Integer read _GetSERVICO_MAX_RETURN_LENGTH;
    {class} property SERVICO_RTIMEOUT: Integer read _GetSERVICO_RTIMEOUT;
    {class} property SERVICO_WTIMEOUT: Integer read _GetSERVICO_WTIMEOUT;
    {class} property SMARTPOS_MAX_RETURN_LENGTH: Integer read _GetSMARTPOS_MAX_RETURN_LENGTH;
    {class} property SMARTPOS_RTIMEOUT: Integer read _GetSMARTPOS_RTIMEOUT;
    {class} property TCP_IP_MAX_RETURN_LENGTH: Integer read _GetTCP_IP_MAX_RETURN_LENGTH;
    {class} property TCP_IP_RTIMEOUT: Integer read _GetTCP_IP_RTIMEOUT;
    {class} property TCP_IP_WTIMEOUT: Integer read _GetTCP_IP_WTIMEOUT;
  end;

  [JavaSignature('com/elgin/e1/Comunicacao/Conexao')]
  JConexao = interface(JObject)
    ['{878CC520-82E8-481A-B091-969106AB9378}']
    function Abrir(P1: JContext; P2: Integer; P3: JString; P4: JString; P5: Integer): Integer; cdecl;
    function Escrever(P1: TJavaArray<Byte>): Integer; cdecl;
    function Fechar: Integer; cdecl;
    function Ler(P1: TJavaArray<Byte>): Integer; cdecl;
    function ReceberDados(P1: JInteiro; P2: Integer): TJavaArray<Byte>; cdecl;
    function getConM8: JConM8; cdecl;
  end;
  TJConexao = class(TJavaGenericImport<JConexaoClass, JConexao>) end;

  JConBluetoothClass = interface(JConexaoClass)
    ['{339D6F98-C80C-4BE1-BDEC-FAAB9925E619}']
  end;

  [JavaSignature('com/elgin/e1/Comunicacao/ConBluetooth')]
  JConBluetooth = interface(JConexao)
    ['{9C8F31D7-2C9A-400E-BEFE-1C0FDF877247}']
  end;
  TJConBluetooth = class(TJavaGenericImport<JConBluetoothClass, JConBluetooth>) end;

  JConBluetooth_1GetBluetoothDataClass = interface(JRunnableClass)
    ['{6A749BD0-8235-474D-BC75-3E7F07436B1B}']
    {class} function getData: TJavaArray<Byte>; cdecl;
    {class} function getError: Integer; cdecl;
    {class} procedure run; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Comunicacao/ConBluetooth$1GetBluetoothData')]
  JConBluetooth_1GetBluetoothData = interface(JRunnable)
    ['{63ED67DB-2E6F-4A94-8191-17BA98F02AE5}']
  end;
  TJConBluetooth_1GetBluetoothData = class(TJavaGenericImport<JConBluetooth_1GetBluetoothDataClass, JConBluetooth_1GetBluetoothData>) end;

  JConBluetooth_1GetPrinterBluetoothClass = interface(JRunnableClass)
    ['{427F57E9-4AC7-4222-90FD-21D8B26C0096}']
    {class} function init: JConBluetooth_1GetPrinterBluetooth; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Comunicacao/ConBluetooth$1GetPrinterBluetooth')]
  JConBluetooth_1GetPrinterBluetooth = interface(JRunnable)
    ['{9938B9A8-1E6F-49EA-914A-D5849552926E}']
    function _GetvalMacAddress: JString; cdecl;
    function getBluetoothSocket: JBluetoothSocket; cdecl;
    function getError: Integer; cdecl;
    procedure run; cdecl;
    property valMacAddress: JString read _GetvalMacAddress;
  end;
  TJConBluetooth_1GetPrinterBluetooth = class(TJavaGenericImport<JConBluetooth_1GetPrinterBluetoothClass, JConBluetooth_1GetPrinterBluetooth>) end;

  JConBluetooth_1SendDataClass = interface(JRunnableClass)
    ['{5F8B2C56-DFED-440D-9320-0427760EDD98}']
    {class} function init: JConBluetooth_1SendData; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Comunicacao/ConBluetooth$1SendData')]
  JConBluetooth_1SendData = interface(JRunnable)
    ['{2E3D9D6C-8E05-4EEE-AB02-69372E040346}']
    function getError: Integer; cdecl;
    function getSz: Integer; cdecl;
    procedure run; cdecl;
  end;
  TJConBluetooth_1SendData = class(TJavaGenericImport<JConBluetooth_1SendDataClass, JConBluetooth_1SendData>) end;

  JConM8Class = interface(JObjectClass)
    ['{4987CAC7-063E-48D0-8B3D-87228D0D0894}']
    {class} function getPrinterManager: JPrinterManager; cdecl;
    {class} function init: JConM8; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Comunicacao/ConM8')]
  JConM8 = interface(JObject)
    ['{6E360D7D-0752-4EC3-B962-110D74A4A636}']
  end;
  TJConM8 = class(TJavaGenericImport<JConM8Class, JConM8>) end;

  JPrinterManager_PrinterManagerListenerClass = interface(IJavaClass)
    ['{72BC4A32-26CC-48C9-A765-2610D01DD17B}']
    {class} procedure onServiceConnected; cdecl;
  end;

  [JavaSignature('com/elgin/minipdvm8/PrinterManager$PrinterManagerListener')]
  JPrinterManager_PrinterManagerListener = interface(IJavaInstance)
    ['{8CB954AA-4920-4D02-945F-E639B2DB9E19}']
  end;
  TJPrinterManager_PrinterManagerListener = class(TJavaGenericImport<JPrinterManager_PrinterManagerListenerClass, JPrinterManager_PrinterManagerListener>) end;

  JConM8_1Class = interface(JPrinterManager_PrinterManagerListenerClass)
    ['{F6414514-63EE-4D8D-A372-13DA9964B03F}']
    {class} function init(P1: JConM8): JConM8_1; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Comunicacao/ConM8$1')]
  JConM8_1 = interface(JPrinterManager_PrinterManagerListener)
    ['{4189A6F3-0D34-48F0-BD2A-8CB066E73A5E}']
    procedure onServiceConnected; cdecl;
  end;
  TJConM8_1 = class(TJavaGenericImport<JConM8_1Class, JConM8_1>) end;

  JConSerialClass = interface(JConexaoClass)
    ['{D4E4059C-1C58-4224-A19D-95D213317298}']
  end;

  [JavaSignature('com/elgin/e1/Comunicacao/ConSerial')]
  JConSerial = interface(JConexao)
    ['{E966967B-DEE6-4072-A23E-4FFA92865D94}']
  end;
  TJConSerial = class(TJavaGenericImport<JConSerialClass, JConSerial>) end;

  JConServiceClass = interface(JConexaoClass)
    ['{968E2839-D15C-4DFF-91DE-843B1B1F3CD1}']
  end;

  [JavaSignature('com/elgin/e1/Comunicacao/ConService')]
  JConService = interface(JConexao)
    ['{CBBFA27C-AA44-467A-8021-FBAD942FE5FD}']
  end;
  TJConService = class(TJavaGenericImport<JConServiceClass, JConService>) end;

  JConService_1GetDataClass = interface(JRunnableClass)
    ['{78DC740E-D438-412A-AA2B-BB4C51638CBE}']
    {class} function getError: Integer; cdecl;
    {class} function getNumReadTotal: Integer; cdecl;
    {class} function getRdBuffer: TJavaArray<Byte>; cdecl;
    {class} procedure run; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Comunicacao/ConService$1GetData')]
  JConService_1GetData = interface(JRunnable)
    ['{C7C3DC8C-FF21-4FFD-A643-03C0B43BFD2C}']
  end;
  TJConService_1GetData = class(TJavaGenericImport<JConService_1GetDataClass, JConService_1GetData>) end;

  JConService_1GetPrinterClass = interface(JRunnableClass)
    ['{A0CD771C-F6D3-4372-9863-3B6776C677C5}']
    {class} function init: JConService_1GetPrinter; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Comunicacao/ConService$1GetPrinter')]
  JConService_1GetPrinter = interface(JRunnable)
    ['{293AAEA7-E4F1-4E8C-876D-4F97262F49A2}']
    function _Getvalparametro: Integer; cdecl;
    function getError: Integer; cdecl;
    function getSocket: JSocket; cdecl;
    procedure run; cdecl;
    property valparametro: Integer read _Getvalparametro;
  end;
  TJConService_1GetPrinter = class(TJavaGenericImport<JConService_1GetPrinterClass, JConService_1GetPrinter>) end;

  JConService_1SendDataClass = interface(JRunnableClass)
    ['{B978CC9D-B2AF-4111-9608-EC9F821BFC47}']
    {class} function init: JConService_1SendData; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Comunicacao/ConService$1SendData')]
  JConService_1SendData = interface(JRunnable)
    ['{5A533832-E0B0-44B5-9C19-A8E57020AAD1}']
    function getError: Integer; cdecl;
    function getSz: Integer; cdecl;
    procedure run; cdecl;
  end;
  TJConService_1SendData = class(TJavaGenericImport<JConService_1SendDataClass, JConService_1SendData>) end;

  JConService_2GetDataClass = interface(JRunnableClass)
    ['{A7A907E5-8A0C-4DEB-B2AB-7B31368EAF6F}']
    {class} function init: JConService_2GetData; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Comunicacao/ConService$2GetData')]
  JConService_2GetData = interface(JRunnable)
    ['{96B4FEBB-3544-4D2F-B9D9-69AE10E7EDCF}']
    function getError: Integer; cdecl;
    function getNumReadTotal: Integer; cdecl;
    function getRdBuffer: TJavaArray<Byte>; cdecl;
    procedure run; cdecl;
  end;
  TJConService_2GetData = class(TJavaGenericImport<JConService_2GetDataClass, JConService_2GetData>) end;

  JConSmartPOSClass = interface(JConexaoClass)
    ['{0EA6297C-E88E-4612-9C6C-D965A370BF0A}']
    {class} //function getPrinterDevice(P1: Integer): JPrinterDevice; cdecl;
    {class} function init: JConSmartPOS; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Comunicacao/ConSmartPOS')]
  JConSmartPOS = interface(JConexao)
    ['{953F8F98-235B-4ACD-AB70-3098D3E1FBB7}']
  end;
  TJConSmartPOS = class(TJavaGenericImport<JConSmartPOSClass, JConSmartPOS>) end;

  JConTCP_IPClass = interface(JConexaoClass)
    ['{5A8851E1-0461-4EBD-885B-95C2065C774B}']
  end;

  [JavaSignature('com/elgin/e1/Comunicacao/ConTCP_IP')]
  JConTCP_IP = interface(JConexao)
    ['{052DABF6-4C11-4294-9A3C-30434E68CFF4}']
  end;
  TJConTCP_IP = class(TJavaGenericImport<JConTCP_IPClass, JConTCP_IP>) end;

  JConTCP_IP_1GetDataClass = interface(JRunnableClass)
    ['{D87541BF-ED82-47D3-9028-F5C2A45A9C53}']
    {class} function getData: TJavaArray<Byte>; cdecl;
    {class} function getError: Integer; cdecl;
    {class} procedure run; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Comunicacao/ConTCP_IP$1GetData')]
  JConTCP_IP_1GetData = interface(JRunnable)
    ['{976D06FF-A04C-41A7-AEC9-E1B07BF24491}']
  end;
  TJConTCP_IP_1GetData = class(TJavaGenericImport<JConTCP_IP_1GetDataClass, JConTCP_IP_1GetData>) end;

  JConTCP_IP_1GetPrinterClass = interface(JRunnableClass)
    ['{00A07BB0-DB6D-4537-AAC3-768D45A354E2}']
    {class} function init: JConTCP_IP_1GetPrinter; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Comunicacao/ConTCP_IP$1GetPrinter')]
  JConTCP_IP_1GetPrinter = interface(JRunnable)
    ['{224EFDCB-08F6-4BDF-AD8C-A379861D84A0}']
    function _Getvalparametro: Integer; cdecl;
    function getError: Integer; cdecl;
    function getSocket: JSocket; cdecl;
    procedure run; cdecl;
    property valparametro: Integer read _Getvalparametro;
  end;
  TJConTCP_IP_1GetPrinter = class(TJavaGenericImport<JConTCP_IP_1GetPrinterClass, JConTCP_IP_1GetPrinter>) end;

  JConTCP_IP_1SendDataClass = interface(JRunnableClass)
    ['{73895DA4-33D1-4565-8FB1-42ADD4D6E0E5}']
    {class} function init: JConTCP_IP_1SendData; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Comunicacao/ConTCP_IP$1SendData')]
  JConTCP_IP_1SendData = interface(JRunnable)
    ['{A43B8C30-C207-46C4-A97F-11A922029FFE}']
    function getError: Integer; cdecl;
    function getSz: Integer; cdecl;
    procedure run; cdecl;
  end;
  TJConTCP_IP_1SendData = class(TJavaGenericImport<JConTCP_IP_1SendDataClass, JConTCP_IP_1SendData>) end;

  JConUSBClass = interface(JConexaoClass)
    ['{442844A1-D9B5-476B-80A9-041B19FC0796}']
  end;

  [JavaSignature('com/elgin/e1/Comunicacao/ConUSB')]
  JConUSB = interface(JConexao)
    ['{884522A7-37F1-40E8-8372-9B38A6DA4D29}']
  end;
  TJConUSB = class(TJavaGenericImport<JConUSBClass, JConUSB>) end;

  JInterfaceFactoryXMLSATClass = interface(IJavaClass)
    ['{D49EAD93-32A7-4771-BA0A-86560FE7C7A2}']
    {class} function AbreCupomCancelamento(P1: JString; P2: JString; P3: JString; P4: JString; P5: Integer): Integer; cdecl;
    {class} function AbreCupomVenda(P1: JString; P2: JString; P3: JString; P4: JString; P5: Integer; P6: JString; P7: JString; P8: JString; P9: JString; P10: JString; P11: JString; P12: JString; P13: JString): Integer; cdecl;
    {class} function InformaCOFINSAliq(P1: JString; P2: Integer; P3: JString; P4: JString; P5: JString): Integer; cdecl;
    {class} function InformaCOFINSNT(P1: JString; P2: Integer; P3: JString): Integer; cdecl;
    {class} function InformaCOFINSOutr(P1: JString; P2: Integer; P3: Integer; P4: JString; P5: JString; P6: JString): Integer; cdecl;
    {class} function InformaCOFINSQtde(P1: JString; P2: Integer; P3: JString; P4: JString; P5: JString): Integer; cdecl;
    {class} function InformaCOFINSSN(P1: JString; P2: Integer; P3: JString): Integer; cdecl;
    {class} function InformaCOFINSST(P1: JString; P2: Integer; P3: Integer; P4: JString; P5: JString): Integer; cdecl;
    {class} function InformaEntrega(P1: JString; P2: JString; P3: JString; P4: JString; P5: JString; P6: JString; P7: JString): Integer; cdecl;
    {class} function InformaICMS00(P1: JString; P2: Integer; P3: Integer; P4: JString; P5: JString): Integer; cdecl;
    {class} function InformaICMS40(P1: JString; P2: Integer; P3: Integer; P4: JString): Integer; cdecl;
    {class} function InformaICMSSN102(P1: JString; P2: Integer; P3: Integer; P4: JString): Integer; cdecl;
    {class} function InformaICMSSN900(P1: JString; P2: Integer; P3: Integer; P4: JString; P5: JString): Integer; cdecl;
    {class} function InformaISSQN(P1: JString; P2: Integer; P3: JString; P4: JString; P5: JString; P6: JString; P7: JString; P8: JString; P9: JString): Integer; cdecl;
    {class} function InformaImposto(P1: JString; P2: Integer; P3: JString): Integer; cdecl;
    {class} function InformaObsFiscoDet(P1: JString; P2: Integer; P3: JString; P4: JString): Integer; cdecl;
    {class} function InformaPISAliq(P1: JString; P2: Integer; P3: JString; P4: JString; P5: JString): Integer; cdecl;
    {class} function InformaPISNT(P1: JString; P2: Integer; P3: JString): Integer; cdecl;
    {class} function InformaPISOutr(P1: JString; P2: Integer; P3: Integer; P4: JString; P5: JString; P6: JString): Integer; cdecl;
    {class} function InformaPISQtde(P1: JString; P2: Integer; P3: JString; P4: JString; P5: JString): Integer; cdecl;
    {class} function InformaPISSN(P1: JString; P2: Integer; P3: JString): Integer; cdecl;
    {class} function InformaPISST(P1: JString; P2: Integer; P3: Integer; P4: JString; P5: JString): Integer; cdecl;
    {class} function InformaProduto(P1: JString; P2: JString; P3: JString; P4: JString; P5: JString; P6: JString; P7: JString; P8: JString; P9: JString; P10: JString; P11: JString; P12: JString; P13: JString): Integer; cdecl;
    {class} function InformaTotal(P1: JString; P2: JString; P3: JString; P4: JString): Integer; cdecl;
    {class} function InformainfAdProd(P1: JString; P2: Integer; P3: JString): Integer; cdecl;
    {class} function InformainfAdic(P1: JString; P2: JString): Integer; cdecl;
    {class} function Informapgto(P1: JString; P2: JString; P3: JString; P4: JString): Integer; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Fiscal/InterfaceFactoryXMLSAT')]
  JInterfaceFactoryXMLSAT = interface(IJavaInstance)
    ['{A2E91A6C-322F-4B27-8588-85C4692DE12A}']
  end;
  TJInterfaceFactoryXMLSAT = class(TJavaGenericImport<JInterfaceFactoryXMLSATClass, JInterfaceFactoryXMLSAT>) end;

  JImplementacaoFactoryXMLSATClass = interface(JInterfaceFactoryXMLSATClass)
    ['{2E32D03B-3B58-40A4-ADD6-EDABC18D2065}']
    {class} function AbreCupomCancelamento(P1: JString; P2: JString; P3: JString; P4: JString; P5: Integer): Integer; cdecl;
    {class} function AbreCupomVenda(P1: JString; P2: JString; P3: JString; P4: JString; P5: Integer; P6: JString; P7: JString; P8: JString; P9: JString; P10: JString; P11: JString; P12: JString; P13: JString): Integer; cdecl;
    {class} function InformaCOFINSAliq(P1: JString; P2: Integer; P3: JString; P4: JString; P5: JString): Integer; cdecl;
    {class} function InformaCOFINSNT(P1: JString; P2: Integer; P3: JString): Integer; cdecl;
    {class} function InformaCOFINSOutr(P1: JString; P2: Integer; P3: Integer; P4: JString; P5: JString; P6: JString): Integer; cdecl;
    {class} function InformaCOFINSQtde(P1: JString; P2: Integer; P3: JString; P4: JString; P5: JString): Integer; cdecl;
    {class} function InformaCOFINSSN(P1: JString; P2: Integer; P3: JString): Integer; cdecl;
    {class} function InformaCOFINSST(P1: JString; P2: Integer; P3: Integer; P4: JString; P5: JString): Integer; cdecl;
    {class} function InformaEntrega(P1: JString; P2: JString; P3: JString; P4: JString; P5: JString; P6: JString; P7: JString): Integer; cdecl;
    {class} function InformaICMS00(P1: JString; P2: Integer; P3: Integer; P4: JString; P5: JString): Integer; cdecl;
    {class} function InformaICMS40(P1: JString; P2: Integer; P3: Integer; P4: JString): Integer; cdecl;
    {class} function InformaICMSSN102(P1: JString; P2: Integer; P3: Integer; P4: JString): Integer; cdecl;
    {class} function InformaICMSSN900(P1: JString; P2: Integer; P3: Integer; P4: JString; P5: JString): Integer; cdecl;
    {class} function InformaISSQN(P1: JString; P2: Integer; P3: JString; P4: JString; P5: JString; P6: JString; P7: JString; P8: JString; P9: JString): Integer; cdecl;
    {class} function InformaImposto(P1: JString; P2: Integer; P3: JString): Integer; cdecl;
    {class} function InformaObsFiscoDet(P1: JString; P2: Integer; P3: JString; P4: JString): Integer; cdecl;
    {class} function InformaPISAliq(P1: JString; P2: Integer; P3: JString; P4: JString; P5: JString): Integer; cdecl;
    {class} function InformaPISNT(P1: JString; P2: Integer; P3: JString): Integer; cdecl;
    {class} function InformaPISOutr(P1: JString; P2: Integer; P3: Integer; P4: JString; P5: JString; P6: JString): Integer; cdecl;
    {class} function InformaPISQtde(P1: JString; P2: Integer; P3: JString; P4: JString; P5: JString): Integer; cdecl;
    {class} function InformaPISSN(P1: JString; P2: Integer; P3: JString): Integer; cdecl;
    {class} function InformaPISST(P1: JString; P2: Integer; P3: Integer; P4: JString; P5: JString): Integer; cdecl;
    {class} function InformaProduto(P1: JString; P2: JString; P3: JString; P4: JString; P5: JString; P6: JString; P7: JString; P8: JString; P9: JString; P10: JString; P11: JString; P12: JString; P13: JString): Integer; cdecl;
    {class} function InformaTotal(P1: JString; P2: JString; P3: JString; P4: JString): Integer; cdecl;
    {class} function InformainfAdProd(P1: JString; P2: Integer; P3: JString): Integer; cdecl;
    {class} function InformainfAdic(P1: JString; P2: JString): Integer; cdecl;
    {class} function Informapgto(P1: JString; P2: JString; P3: JString; P4: JString): Integer; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Fiscal/ImplementacaoFactoryXMLSAT')]
  JImplementacaoFactoryXMLSAT = interface(JInterfaceFactoryXMLSAT)
    ['{7A3C4930-737E-45D3-B43B-BC6F2E28F297}']
  end;
  TJImplementacaoFactoryXMLSAT = class(TJavaGenericImport<JImplementacaoFactoryXMLSATClass, JImplementacaoFactoryXMLSAT>) end;

  JInterfaceSATClass = interface(IJavaClass)
    ['{C76719ED-2E1F-49AA-972D-60498D2C74CE}']
    {class} function AssociarAssinatura(P1: Integer; P2: JString; P3: JString; P4: JString): JString; cdecl;
    {class} function AtivarSAT(P1: Integer; P2: Integer; P3: JString; P4: JString; P5: Integer): JString; cdecl;
    {class} function AtualizarSoftwareSAT(P1: Integer; P2: JString): JString; cdecl;
    {class} function BloquearSAT(P1: Integer; P2: JString): JString; cdecl;
    {class} function CancelaVendaImpressaSAT(P1: Integer; P2: JString; P3: JString; P4: Integer; P5: JString; P6: JString): JString; cdecl;
    {class} function CancelarUltimaVenda(P1: Integer; P2: JString; P3: JString; P4: JString): JString; cdecl;
    {class} function ConfigurarInterfaceDeRede(P1: Integer; P2: JString; P3: JString): JString; cdecl;
    {class} function ConsultarNumeroSessao(P1: Integer; P2: JString; P3: Integer): JString; cdecl;
    {class} function ConsultarSat(P1: Integer): JString; cdecl;
    {class} function ConsultarStatusEspecifico(P1: Integer; P2: Integer; P3: JString): JString; cdecl;
    {class} function ConsultarStatusOperacional(P1: Integer; P2: JString): JString; cdecl;
    {class} function ConsultarUltimaSessaoFiscal(P1: Integer; P2: JString): JString; cdecl;
    {class} function CriaXMLCancelamentoSAT(P1: JString; P2: JString; P3: JString): JString; cdecl;
    {class} function DecodificaBase64(P1: JString): JString; cdecl;
    {class} function DesbloquearSAT(P1: Integer; P2: JString): JString; cdecl;
    {class} function EnviarDadosVenda(P1: Integer; P2: JString; P3: JString): JString; cdecl;
    {class} function ExtrairLogs(P1: Integer; P2: JString): JString; cdecl;
    {class} function GetVersaoDLL: JString; cdecl;
    {class} function TesteFimAFim(P1: Integer; P2: JString; P3: JString): JString; cdecl;
    {class} function TrocarCodigoDeAtivacao(P1: Integer; P2: JString; P3: Integer; P4: JString; P5: JString): JString; cdecl;
    {class} function VendaImpressaSAT(P1: Integer; P2: JString; P3: JString; P4: Integer; P5: Integer; P6: JString; P7: JString): JString; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Fiscal/InterfaceSAT')]
  JInterfaceSAT = interface(IJavaInstance)
    ['{975E7646-B169-433C-BE77-66C1B89D015B}']
  end;
  TJInterfaceSAT = class(TJavaGenericImport<JInterfaceSATClass, JInterfaceSAT>) end;

  JImplementacaoSATClass = interface(JInterfaceSATClass)
    ['{8EA54884-9D69-44B3-B2D6-DA16257C3657}']
    {class} function AssociarAssinatura(P1: Integer; P2: JString; P3: JString; P4: JString): JString; cdecl;
    {class} function AtivarSAT(P1: Integer; P2: Integer; P3: JString; P4: JString; P5: Integer): JString; cdecl;
    {class} function AtualizarSoftwareSAT(P1: Integer; P2: JString): JString; cdecl;
    {class} function BloquearSAT(P1: Integer; P2: JString): JString; cdecl;
    {class} function CancelaVendaImpressaSAT(P1: Integer; P2: JString; P3: JString; P4: Integer; P5: JString; P6: JString): JString; cdecl;
    {class} function CancelarUltimaVenda(P1: Integer; P2: JString; P3: JString; P4: JString): JString; cdecl;
    {class} function ConfigurarInterfaceDeRede(P1: Integer; P2: JString; P3: JString): JString; cdecl;
    {class} function ConsultarNumeroSessao(P1: Integer; P2: JString; P3: Integer): JString; cdecl;
    {class} function ConsultarSat(P1: Integer): JString; cdecl;
    {class} function ConsultarStatusEspecifico(P1: Integer; P2: Integer; P3: JString): JString; cdecl;
    {class} function ConsultarStatusOperacional(P1: Integer; P2: JString): JString; cdecl;
    {class} function ConsultarUltimaSessaoFiscal(P1: Integer; P2: JString): JString; cdecl;
    {class} function CriaXMLCancelamentoSAT(P1: JString; P2: JString; P3: JString): JString; cdecl;
    {class} function DecodificaBase64(P1: JString): JString; cdecl;
    {class} function DesbloquearSAT(P1: Integer; P2: JString): JString; cdecl;
    {class} function EnviarDadosVenda(P1: Integer; P2: JString; P3: JString): JString; cdecl;
    {class} function ExtrairLogs(P1: Integer; P2: JString): JString; cdecl;
    {class} function GetVersaoDLL: JString; cdecl;
    {class} function TesteFimAFim(P1: Integer; P2: JString; P3: JString): JString; cdecl;
    {class} function TrocarCodigoDeAtivacao(P1: Integer; P2: JString; P3: Integer; P4: JString; P5: JString): JString; cdecl;
    {class} function VendaImpressaSAT(P1: Integer; P2: JString; P3: JString; P4: Integer; P5: Integer; P6: JString; P7: JString): JString; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Fiscal/ImplementacaoSAT')]
  JImplementacaoSAT = interface(JInterfaceSAT)
    ['{A40344EA-927B-4268-9798-5172D1625F91}']
  end;
  TJImplementacaoSAT = class(TJavaGenericImport<JImplementacaoSATClass, JImplementacaoSAT>) end;

  JMFeClass = interface(JObjectClass)
    ['{F44773CF-6EC0-4F34-B3BD-795627AAF2DC}']
    {class} function init: JMFe; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Fiscal/MFe')]
  JMFe = interface(JObject)
    ['{D1553818-0685-4F6F-A642-806C6CE477BC}']
  end;
  TJMFe = class(TJavaGenericImport<JMFeClass, JMFe>) end;

  JNFCeClass = interface(JObjectClass)
    ['{AF4DE2D1-EBAC-41B9-BFC6-BD854A579D3D}']
    {class} function init: JNFCe; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Fiscal/NFCe')]
  JNFCe = interface(JObject)
    ['{44AF9F75-AD7A-4A6C-9489-27939662FBFE}']
  end;
  TJNFCe = class(TJavaGenericImport<JNFCeClass, JNFCe>) end;

  JSATClass = interface(JObjectClass)
    ['{29812683-AD56-4909-B53B-308042D1DADD}']
    {class} function AbreCupomCancelamento(P1: JString; P2: JString; P3: JString; P4: JString; P5: Integer): Integer; cdecl;
    {class} function AbreCupomVenda(P1: JString; P2: JString; P3: JString; P4: JString; P5: Integer; P6: JString; P7: JString; P8: JString; P9: JString; P10: JString; P11: JString; P12: JString; P13: JString): Integer; cdecl;
    {class} function AssociarAssinatura(P1: Integer; P2: JString; P3: JString; P4: JString): JString; cdecl;
    {class} function AtivarSAT(P1: Integer; P2: Integer; P3: JString; P4: JString; P5: Integer): JString; cdecl;
    {class} function AtualizarSoftwareSAT(P1: Integer; P2: JString): JString; cdecl;
    {class} function BloquearSAT(P1: Integer; P2: JString): JString; cdecl;
    {class} function CancelaVendaImpressaSAT(P1: Integer; P2: JString; P3: JString; P4: Integer; P5: JString; P6: JString): JString; cdecl;
    {class} function CancelarUltimaVenda(P1: Integer; P2: JString; P3: JString; P4: JString): JString; cdecl;
    {class} function ConfigurarInterfaceDeRede(P1: Integer; P2: JString; P3: JString): JString; cdecl;
    {class} function ConsultarNumeroSessao(P1: Integer; P2: JString; P3: Integer): JString; cdecl;
    {class} function ConsultarSat(P1: Integer): JString; cdecl;
    {class} function ConsultarStatusEspecifico(P1: Integer; P2: Integer; P3: JString): JString; cdecl;
    {class} function ConsultarStatusOperacional(P1: Integer; P2: JString): JString; cdecl;
    {class} function ConsultarUltimaSessaoFiscal(P1: Integer; P2: JString): JString; cdecl;
    {class} function CriaXMLCancelamentoSAT(P1: JString; P2: JString; P3: JString): JString; cdecl;
    {class} function DecodificaBase64(P1: JString): JString; cdecl;
    {class} function DesbloquearSAT(P1: Integer; P2: JString): JString; cdecl;
    {class} function EnviarDadosVenda(P1: Integer; P2: JString; P3: JString): JString; cdecl;
    {class} function ExtrairLogs(P1: Integer; P2: JString): JString; cdecl;
    {class} function GetVersaoDLL: JString; cdecl;
    {class} function InformaCOFINSAliq(P1: JString; P2: Integer; P3: JString; P4: JString; P5: JString): Integer; cdecl;
    {class} function InformaCOFINSNT(P1: JString; P2: Integer; P3: JString): Integer; cdecl;
    {class} function InformaCOFINSOutr(P1: JString; P2: Integer; P3: Integer; P4: JString; P5: JString; P6: JString): Integer; cdecl;
    {class} function InformaCOFINSQtde(P1: JString; P2: Integer; P3: JString; P4: JString; P5: JString): Integer; cdecl;
    {class} function InformaCOFINSSN(P1: JString; P2: Integer; P3: JString): Integer; cdecl;
    {class} function InformaCOFINSST(P1: JString; P2: Integer; P3: Integer; P4: JString; P5: JString): Integer; cdecl;
    {class} function InformaEntrega(P1: JString; P2: JString; P3: JString; P4: JString; P5: JString; P6: JString; P7: JString): Integer; cdecl;
    {class} function InformaICMS00(P1: JString; P2: Integer; P3: Integer; P4: JString; P5: JString): Integer; cdecl;
    {class} function InformaICMS40(P1: JString; P2: Integer; P3: Integer; P4: JString): Integer; cdecl;
    {class} function InformaICMSSN102(P1: JString; P2: Integer; P3: Integer; P4: JString): Integer; cdecl;
    {class} function InformaICMSSN900(P1: JString; P2: Integer; P3: Integer; P4: JString; P5: JString): Integer; cdecl;
    {class} function InformaISSQN(P1: JString; P2: Integer; P3: JString; P4: JString; P5: JString; P6: JString; P7: JString; P8: JString; P9: JString): Integer; cdecl;
    {class} function InformaImposto(P1: JString; P2: Integer; P3: JString): Integer; cdecl;
    {class} function InformaInfAdProd(P1: JString; P2: Integer; P3: JString): Integer; cdecl;
    {class} function InformaInfAdic(P1: JString; P2: JString): Integer; cdecl;
    {class} function InformaObsFiscoDet(P1: JString; P2: Integer; P3: JString; P4: JString): Integer; cdecl;
    {class} function InformaPISAliq(P1: JString; P2: Integer; P3: JString; P4: JString; P5: JString): Integer; cdecl;
    {class} function InformaPISNT(P1: JString; P2: Integer; P3: JString): Integer; cdecl;
    {class} function InformaPISOutr(P1: JString; P2: Integer; P3: Integer; P4: JString; P5: JString; P6: JString): Integer; cdecl;
    {class} function InformaPISQtde(P1: JString; P2: Integer; P3: JString; P4: JString; P5: JString): Integer; cdecl;
    {class} function InformaPISSN(P1: JString; P2: Integer; P3: JString): Integer; cdecl;
    {class} function InformaPISST(P1: JString; P2: Integer; P3: Integer; P4: JString; P5: JString): Integer; cdecl;
    {class} function InformaPgto(P1: JString; P2: JString; P3: JString; P4: JString): Integer; cdecl;
    {class} function InformaProduto(P1: JString; P2: JString; P3: JString; P4: JString; P5: JString; P6: JString; P7: JString; P8: JString; P9: JString; P10: JString; P11: JString; P12: JString; P13: JString): Integer; cdecl;
    {class} function InformaTotal(P1: JString; P2: JString; P3: JString; P4: JString): Integer; cdecl;
    {class} function TesteFimAFim(P1: Integer; P2: JString; P3: JString): JString; cdecl;
    {class} function TrocarCodigoDeAtivacao(P1: Integer; P2: JString; P3: Integer; P4: JString; P5: JString): JString; cdecl;
    {class} function VendaImpressaSAT(P1: Integer; P2: JString; P3: JString; P4: Integer; P5: Integer; P6: JString; P7: JString): JString; cdecl;
    {class} function getServiceResult: Integer; cdecl;
    {class} function init: JSAT; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Fiscal/SAT')]
  JSAT = interface(JObject)
    ['{B475F07E-9B7E-4CD1-A5EF-4C88AB68832D}']
  end;
  TJSAT = class(TJavaGenericImport<JSATClass, JSAT>) end;

  JAndroidClass = interface(JObjectClass)
    ['{A3787D75-EB75-44F5-96DC-CEA9A01FE21E}']
    {class} function GetNumeroSerie: JString; cdecl;
    {class} function init: JAndroid; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Impressora/Android')]
  JAndroid = interface(JObject)
    ['{F0B0E0EC-3194-4D64-83C6-0341FE97864E}']
  end;
  TJAndroid = class(TJavaGenericImport<JAndroidClass, JAndroid>) end;

  JdsImpressoraClass = interface(JObjectClass)
    ['{A130DAA3-6FFA-46C5-A607-30BDA89C52A7}']
    {class} function _GettimeoutLeitura: Integer; cdecl;
    {class} function init: JdsImpressora; cdecl;
    {class} property timeoutLeitura: Integer read _GettimeoutLeitura;
  end;

  [JavaSignature('com/elgin/e1/Impressora/Config/dsImpressora')]
  JdsImpressora = interface(JObject)
    ['{61D94C26-ED94-4F48-AE22-FF877CA8CECC}']
    function getIDHardwareImpressora(P1: Integer): JdsImpressora_infoHW; cdecl;
    function getIDHardwareImpressoraSize: Integer; cdecl;
    function getVersoesNFCSuportada(P1: Integer): JString; cdecl;
    function getVersoesNFCSuportadaSize: Integer; cdecl;
  end;
  TJdsImpressora = class(TJavaGenericImport<JdsImpressoraClass, JdsImpressora>) end;

  JdsImpressora_1Class = interface(JObjectClass)
    ['{CC0CD909-1CA9-46E9-8863-608F5E6EA0C1}']
  end;

  [JavaSignature('com/elgin/e1/Impressora/Config/dsImpressora$1')]
  JdsImpressora_1 = interface(JObject)
    ['{978D4C73-B4C0-4852-B53F-12F15467265F}']
  end;
  TJdsImpressora_1 = class(TJavaGenericImport<JdsImpressora_1Class, JdsImpressora_1>) end;

  JdsImpressora_infoHWClass = interface(JObjectClass)
    ['{D829F9A3-D9FC-4AD3-BE0C-2A94C36D90B5}']
    {class} function _Getid: Integer; cdecl;
    {class} function init(P1: JdsImpressora; P2: Integer; P3: JString; P4: Integer; P5: Integer; P6: Integer; P7: Integer; P8: Integer; P9: Integer; P10: Integer; P11: JdsImpressora_1): JdsImpressora_infoHW; cdecl;
    {class} property id: Integer read _Getid;
  end;

  [JavaSignature('com/elgin/e1/Impressora/Config/dsImpressora$infoHW')]
  JdsImpressora_infoHW = interface(JObject)
    ['{9E21AD29-B7D6-4B7A-81E8-382D9CD132BC}']
    function _GetPID: Integer; cdecl;
    function _GetVID: Integer; cdecl;
    function _GetalinhamentoQRCodeModoPaginaH: Integer; cdecl;
    function _GetalinhamentoQRCodeModoPaginaL: Integer; cdecl;
    function _GetcodPage: Integer; cdecl;
    function _Getmodelo: JString; cdecl;
    function _GetnumColunaA: Integer; cdecl;
    function _GetnumColunaB: Integer; cdecl;
    function _Getthis: JdsImpressora; cdecl;
    property PID: Integer read _GetPID;
    property VID: Integer read _GetVID;
    property alinhamentoQRCodeModoPaginaH: Integer read _GetalinhamentoQRCodeModoPaginaH;
    property alinhamentoQRCodeModoPaginaL: Integer read _GetalinhamentoQRCodeModoPaginaL;
    property codPage: Integer read _GetcodPage;
    property modelo: JString read _Getmodelo;
    property numColunaA: Integer read _GetnumColunaA;
    property numColunaB: Integer read _GetnumColunaB;
    property this: JdsImpressora read _Getthis;
  end;
  TJdsImpressora_infoHW = class(TJavaGenericImport<JdsImpressora_infoHWClass, JdsImpressora_infoHW>) end;

  JdsSATClass = interface(JObjectClass)
    ['{3CD9680A-303F-4570-9660-B34191E448B4}']
    {class} function getChaves(P1: Integer): JdsSAT_ChaveDePesquisa; cdecl;
    {class} function getChavesSize: Integer; cdecl;
    {class} function init: JdsSAT; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Impressora/Config/dsSAT')]
  JdsSAT = interface(JObject)
    ['{9CBFF232-752C-4E82-93D6-0BDFF87E2229}']
  end;
  TJdsSAT = class(TJavaGenericImport<JdsSATClass, JdsSAT>) end;

  JdsSAT_1Class = interface(JObjectClass)
    ['{509D42C8-ABE5-4CE2-8389-597032A2DE87}']
  end;

  [JavaSignature('com/elgin/e1/Impressora/Config/dsSAT$1')]
  JdsSAT_1 = interface(JObject)
    ['{0061D89D-5420-414C-A4BD-23802D490A39}']
  end;
  TJdsSAT_1 = class(TJavaGenericImport<JdsSAT_1Class, JdsSAT_1>) end;

  JdsSAT_ChaveDePesquisaClass = interface(JObjectClass)
    ['{5BDAE1F3-A7DE-49AC-A926-56F21AE02A6D}']
    {class} function _Getindex: Integer; cdecl;
    {class} function init(P1: JdsSAT; P2: Integer; P3: TJavaObjectArray<JString>; P4: JdsSAT_1): JdsSAT_ChaveDePesquisa; cdecl;
    {class} property index: Integer read _Getindex;
  end;

  [JavaSignature('com/elgin/e1/Impressora/Config/dsSAT$ChaveDePesquisa')]
  JdsSAT_ChaveDePesquisa = interface(JObject)
    ['{A91426CD-AD04-4FEE-9BBC-4093B555EAC1}']
    function _Getchave: TJavaObjectArray<JString>; cdecl;
    function _Getthis: JdsSAT; cdecl;
    property chave: TJavaObjectArray<JString> read _Getchave;
    property this: JdsSAT read _Getthis;
  end;
  TJdsSAT_ChaveDePesquisa = class(TJavaGenericImport<JdsSAT_ChaveDePesquisaClass, JdsSAT_ChaveDePesquisa>) end;

  JEtiquetaClass = interface(JObjectClass)
    ['{45815C40-900B-422C-A9D9-73C4EBD3BE23}']
    {class} function DespejarArquivo(P1: Integer; P2: JString; P3: JString; P4: Integer; P5: JString): Integer; cdecl;
    {class} function DirectIO(P1: Integer; P2: JString; P3: JString; P4: Integer; P5: TJavaArray<Byte>; P6: Integer; P7: TJavaArray<Byte>; P8: Integer): TJavaArray<Integer>; cdecl;
    {class} function EnviaImagem(P1: Integer; P2: JString; P3: JString; P4: Integer; P5: Integer; P6: Integer; P7: JString; P8: JString): Integer; cdecl;
    {class} function ExcluiImagem(P1: Integer; P2: JString; P3: JString; P4: Integer; P5: Integer; P6: JString): Integer; cdecl;
    {class} function Feed(P1: Integer; P2: JString; P3: JString; P4: Integer): Integer; cdecl;
    {class} function GerarBarCode1D(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: Integer; P8: JString; P9: Integer): Integer; cdecl;
    {class} function GerarBox(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer): Integer; cdecl;
    {class} function GerarDataBar(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: Integer; P8: JString; P9: JString): Integer; cdecl;
    {class} function GerarDataBarExpanded(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: Integer; P8: JString; P9: JString): Integer; cdecl;
    {class} function GerarDataMatrix(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: JString): Integer; cdecl;
    {class} function GerarImagem(P1: Integer; P2: Integer; P3: JString): Integer; cdecl;
    {class} function GerarLinha(P1: Integer; P2: Integer; P3: Integer; P4: Integer): Integer; cdecl;
    {class} function GerarMaxiCode(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: Integer; P8: JString): Integer; cdecl;
    {class} function GerarPDF417(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: Integer; P8: Integer; P9: Integer; P10: Integer; P11: JString): Integer; cdecl;
    {class} function GerarQRCodeAuto(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: JString): Integer; cdecl;
    {class} function GerarQRCodeManual(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: Integer; P8: Integer; P9: Integer; P10: Integer; P11: JString): Integer; cdecl;
    {class} function GerarTexto(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: JString): Integer; cdecl;
    {class} function GerarTextoASD(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: JString): Integer; cdecl;
    {class} function GerarTextoCourier(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: JString): Integer; cdecl;
    {class} function GetVersaoDLL: JString; cdecl;
    {class} function Imprime(P1: Integer; P2: JString; P3: JString; P4: Integer): Integer; cdecl;
    {class} function Limpa(P1: Integer): Integer; cdecl;
    {class} function LimpaMemoria(P1: Integer; P2: JString; P3: JString; P4: Integer): Integer; cdecl;
    {class} function LimpaModulo(P1: Integer; P2: JString; P3: JString; P4: Integer; P5: Integer): Integer; cdecl;
    {class} function MemoryStatus(P1: Integer; P2: JString; P3: JString; P4: Integer; P5: Integer): JString; cdecl;
    {class} function Reset(P1: Integer; P2: JString; P3: JString; P4: Integer): Integer; cdecl;
    {class} function SetAlturaGap(P1: Integer; P2: JString; P3: JString; P4: Integer; P5: Integer): Integer; cdecl;
    {class} function SetBackfeed(P1: Integer): Integer; cdecl;
    {class} function SetBaudrate(P1: Integer; P2: JString; P3: JString; P4: Integer; P5: Integer; P6: Integer; P7: Integer; P8: Integer): Integer; cdecl;
    {class} function SetCalor(P1: Integer): Integer; cdecl;
    {class} function SetCortarZero(P1: Integer): Integer; cdecl;
    {class} function SetLength(P1: Integer): Integer; cdecl;
    {class} function SetLogicImgMode(P1: Integer): Integer; cdecl;
    {class} function SetMedidas(P1: Integer): Integer; cdecl;
    {class} function SetMirror(P1: Integer): Integer; cdecl;
    {class} function SetModoContinuo(P1: Integer): Integer; cdecl;
    {class} function SetOffsetColuna(P1: Integer): Integer; cdecl;
    {class} function SetOffsetLinha(P1: Integer): Integer; cdecl;
    {class} function SetPrintStPos(P1: Integer): Integer; cdecl;
    {class} function SetQtde(P1: Integer): Integer; cdecl;
    {class} function SetSensor(P1: Integer): Integer; cdecl;
    {class} function SetSymbolASD(P1: Integer; P2: JString; P3: JString; P4: Integer; P5: Integer): Integer; cdecl;
    {class} function SetTamPixel(P1: Integer; P2: Integer): Integer; cdecl;
    {class} function SetTipoTransferencia(P1: Integer; P2: JString; P3: JString; P4: Integer; P5: Integer): Integer; cdecl;
    {class} function SetVelImpressao(P1: Integer): Integer; cdecl;
    {class} function Status(P1: Integer; P2: JString; P3: JString; P4: Integer): JString; cdecl;
    {class} function StatusEPL(P1: Integer; P2: JString; P3: JString; P4: Integer): JString; cdecl;
    {class} function Teste(P1: Integer; P2: JString; P3: JString; P4: Integer): Integer; cdecl;
    {class} function init: JEtiqueta; cdecl;
    {class} function setContext(P1: JContext): Integer; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Impressora/Etiqueta')]
  JEtiqueta = interface(JObject)
    ['{FD7AABB7-3447-4EC0-BB54-3B55BA803426}']
  end;
  TJEtiqueta = class(TJavaGenericImport<JEtiquetaClass, JEtiqueta>) end;

  JInterfaceAndroidClass = interface(IJavaClass)
    ['{524C14FF-1BD1-40F3-AC7F-9F6572D99261}']
    {class} function EnviaDadosNFCeImpressao(P1: JImplementacaoOBJXMLNFCE; P2: Integer; P3: JString; P4: Integer; P5: Integer): Integer; cdecl;
    {class} function GetNumeroSerie: JString; cdecl;
    {class} function IImprimeXMLCancelamentoSAT(P1: JString; P2: JString; P3: Integer): Integer; cdecl;
    {class} function IImprimeXMLSAT(P1: JString; P2: Integer): Integer; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Impressora/Impressoras/InterfaceAndroid')]
  JInterfaceAndroid = interface(IJavaInstance)
    ['{4E24990E-3BBD-4DE0-9B51-C94F15ABDB96}']
  end;
  TJInterfaceAndroid = class(TJavaGenericImport<JInterfaceAndroidClass, JInterfaceAndroid>) end;

  JImplementacaoAndroidClass = interface(JInterfaceAndroidClass)
    ['{41A47EE0-58A1-4339-8CEA-AAD7813ECC18}']
    {class} function _GetGAVETA_ABERTA: Integer; cdecl;
    {class} function _GetGAVETA_FECHADA: Integer; cdecl;
    {class} function _GetID_M8: Integer; cdecl;
    {class} function _GetPAPEL_AUSENTE: Integer; cdecl;
    {class} function _GetPAPEL_PRESENTE: Integer; cdecl;
    {class} function init: JImplementacaoAndroid; cdecl; overload;
    {class} function init(P1: Integer; P2: JConexao; P3: JImplementacaoTermica): JImplementacaoAndroid; cdecl; overload;
    {class} property GAVETA_ABERTA: Integer read _GetGAVETA_ABERTA;
    {class} property GAVETA_FECHADA: Integer read _GetGAVETA_FECHADA;
    {class} property ID_M8: Integer read _GetID_M8;
    {class} property PAPEL_AUSENTE: Integer read _GetPAPEL_AUSENTE;
    {class} property PAPEL_PRESENTE: Integer read _GetPAPEL_PRESENTE;
  end;

  [JavaSignature('com/elgin/e1/Impressora/Impressoras/ImplementacaoAndroid')]
  JImplementacaoAndroid = interface(JInterfaceAndroid)
    ['{80423994-241A-434E-B8D1-BAE01815A7F2}']
    function _Getcon: JConexao; cdecl;
    function EnviaDadosNFCeImpressao(P1: JImplementacaoOBJXMLNFCE; P2: Integer; P3: JString; P4: Integer; P5: Integer): Integer; cdecl;
    function GetBarCodeBitmap(P1: Integer; P2: JString; P3: Integer; P4: Integer): JBitmap; cdecl;
    function GetCode128Bitmap(P1: JString; P2: Integer; P3: Integer): JBitmap; cdecl;
    function GetNumeroSerie: JString; cdecl;
    function GetPDF417Bitmap(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: JString): JBitmap; cdecl;
    function GetQRCodeBitmap(P1: JString; P2: Integer; P3: Integer): JBitmap; cdecl;
    function IImprimeXMLCancelamentoSAT(P1: JString; P2: JString; P3: Integer): Integer; cdecl;
    function IImprimeXMLSAT(P1: JString; P2: Integer): Integer; cdecl;
    procedure setImpTexto(P1: JImplementacaoAndroid_IIImpressaoTexto); cdecl;
    property con: JConexao read _Getcon;
  end;
  TJImplementacaoAndroid = class(TJavaGenericImport<JImplementacaoAndroidClass, JImplementacaoAndroid>) end;

  JImplementacaoAndroid_IIImpressaoTextoClass = interface(IJavaClass)
    ['{BA599896-AFB2-4FA6-A16F-4F9E671BA165}']
    {class} function IImpressaoTexto(P1: JString): Integer; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Impressora/Impressoras/ImplementacaoAndroid$IIImpressaoTexto')]
  JImplementacaoAndroid_IIImpressaoTexto = interface(IJavaInstance)
    ['{8178DA00-7040-4C3E-AE50-6573E0BC3CA2}']
  end;
  TJImplementacaoAndroid_IIImpressaoTexto = class(TJavaGenericImport<JImplementacaoAndroid_IIImpressaoTextoClass, JImplementacaoAndroid_IIImpressaoTexto>) end;

  JInterfaceBematechClass = interface(IJavaClass)
    ['{826F9E2A-6306-4849-86B3-472DAE803D84}']
    {class} function FormatarMoeda(P1: JString): JString; cdecl;
    {class} function LinhaProduto(P1: Integer; P2: JImplementacaoOBJPRODUTOXMLNFCE): TJavaArray<Char>; cdecl;
    {class} function PreencheLegendaProduto(P1: Integer): TJavaArray<Char>; cdecl;
    {class} function imprimeCode128(P1: JString): Integer; cdecl;
    {class} function imprimeQRCodeBema(P1: JString; P2: Integer; P3: Integer): Integer; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Impressora/Impressoras/InterfaceBematech')]
  JInterfaceBematech = interface(IJavaInstance)
    ['{0E3537C9-BE9B-4F4B-B595-C632A9EDE969}']
  end;
  TJInterfaceBematech = class(TJavaGenericImport<JInterfaceBematechClass, JInterfaceBematech>) end;

  JImplementacaoBematechClass = interface(JInterfaceBematechClass)
    ['{1183CB97-ABF4-4285-B41E-267409234FF2}']
    {class} function FormatarMoeda(P1: JString): JString; cdecl;
    {class} function LinhaProduto(P1: Integer; P2: JImplementacaoOBJPRODUTOXMLNFCE): TJavaArray<Char>; cdecl;
    {class} function PreencheLegendaProduto(P1: Integer): TJavaArray<Char>; cdecl;
    {class} function imprimeCode128(P1: JString): Integer; cdecl;
    {class} function imprimeQRCodeBema(P1: JString; P2: Integer; P3: Integer): Integer; cdecl;
    {class} function init(P1: JConexao): JImplementacaoBematech; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Impressora/Impressoras/ImplementacaoBematech')]
  JImplementacaoBematech = interface(JInterfaceBematech)
    ['{F5CEC7A2-815B-494E-A222-F1E768E31EB2}']
  end;
  TJImplementacaoBematech = class(TJavaGenericImport<JImplementacaoBematechClass, JImplementacaoBematech>) end;

  JInterfaceEtiquetaClass = interface(IJavaClass)
    ['{D36B4C06-8736-484E-8F7B-E3586E802CC6}']
    {class} function DespejarArquivo(P1: Integer; P2: JString; P3: JString; P4: Integer; P5: JString): Integer; cdecl;
    {class} function DirectIO(P1: Integer; P2: JString; P3: JString; P4: Integer; P5: TJavaArray<Byte>; P6: Integer; P7: TJavaArray<Byte>; P8: Integer; P9: Boolean): TJavaArray<Integer>; cdecl;
    {class} function EnviaImagem(P1: Integer; P2: JString; P3: JString; P4: Integer; P5: Integer; P6: Integer; P7: JString; P8: JString): Integer; cdecl;
    {class} function ExcluiImagem(P1: Integer; P2: JString; P3: JString; P4: Integer; P5: Integer; P6: JString): Integer; cdecl;
    {class} function Feed(P1: Integer; P2: JString; P3: JString; P4: Integer): Integer; cdecl;
    {class} function GerarBarCode1D(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: Integer; P8: JString; P9: Integer): Integer; cdecl;
    {class} function GerarBox(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer): Integer; cdecl;
    {class} function GerarDataBar(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: Integer; P8: JString; P9: JString): Integer; cdecl;
    {class} function GerarDataBarExpanded(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: Integer; P8: JString; P9: JString): Integer; cdecl;
    {class} function GerarDataMatrix(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: JString): Integer; cdecl;
    {class} function GerarImagem(P1: Integer; P2: Integer; P3: JString): Integer; cdecl;
    {class} function GerarLinha(P1: Integer; P2: Integer; P3: Integer; P4: Integer): Integer; cdecl;
    {class} function GerarMaxiCode(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: Integer; P8: JString): Integer; cdecl;
    {class} function GerarPDF417(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: Integer; P8: Integer; P9: Integer; P10: Integer; P11: JString): Integer; cdecl;
    {class} function GerarQRCodeAuto(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: JString): Integer; cdecl;
    {class} function GerarQRCodeManual(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: Integer; P8: Integer; P9: Integer; P10: Integer; P11: JString): Integer; cdecl;
    {class} function GerarTexto(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: JString): Integer; cdecl;
    {class} function GerarTextoASD(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: JString): Integer; cdecl;
    {class} function GerarTextoCourier(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: JString): Integer; cdecl;
    {class} function GetVersaoDLL: JString; cdecl;
    {class} function Imprime(P1: Integer; P2: JString; P3: JString; P4: Integer): Integer; cdecl;
    {class} function Limpa(P1: Integer): Integer; cdecl;
    {class} function LimpaMemoria(P1: Integer; P2: JString; P3: JString; P4: Integer): Integer; cdecl;
    {class} function LimpaModulo(P1: Integer; P2: JString; P3: JString; P4: Integer; P5: Integer): Integer; cdecl;
    {class} function MemoryStatus(P1: Integer; P2: JString; P3: JString; P4: Integer; P5: Integer): JString; cdecl;
    {class} function Reset(P1: Integer; P2: JString; P3: JString; P4: Integer): Integer; cdecl;
    {class} function SetAlturaGap(P1: Integer; P2: JString; P3: JString; P4: Integer; P5: Integer): Integer; cdecl;
    {class} function SetBackfeed(P1: Integer): Integer; cdecl;
    {class} function SetBaudrate(P1: Integer; P2: JString; P3: JString; P4: Integer; P5: Integer; P6: Integer; P7: Integer; P8: Integer): Integer; cdecl;
    {class} function SetCalor(P1: Integer): Integer; cdecl;
    {class} function SetCortarZero(P1: Integer): Integer; cdecl;
    {class} function SetLength(P1: Integer): Integer; cdecl;
    {class} function SetLogicImgMode(P1: Integer): Integer; cdecl;
    {class} function SetMedidas(P1: Integer): Integer; cdecl;
    {class} function SetMirror(P1: Integer): Integer; cdecl;
    {class} function SetModoContinuo(P1: Integer): Integer; cdecl;
    {class} function SetOffsetColuna(P1: Integer): Integer; cdecl;
    {class} function SetOffsetLinha(P1: Integer): Integer; cdecl;
    {class} function SetPrintStPos(P1: Integer): Integer; cdecl;
    {class} function SetQtde(P1: Integer): Integer; cdecl;
    {class} function SetSensor(P1: Integer): Integer; cdecl;
    {class} function SetSymbolASD(P1: Integer; P2: JString; P3: JString; P4: Integer; P5: Integer): Integer; cdecl;
    {class} function SetTamPixel(P1: Integer; P2: Integer): Integer; cdecl;
    {class} function SetTipoTransferencia(P1: Integer; P2: JString; P3: JString; P4: Integer; P5: Integer): Integer; cdecl;
    {class} function SetVelImpressao(P1: Integer): Integer; cdecl;
    {class} function Status(P1: Integer; P2: JString; P3: JString; P4: Integer): JString; cdecl;
    {class} function StatusEPL(P1: Integer; P2: JString; P3: JString; P4: Integer): JString; cdecl;
    {class} function Teste(P1: Integer; P2: JString; P3: JString; P4: Integer): Integer; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Impressora/Impressoras/InterfaceEtiqueta')]
  JInterfaceEtiqueta = interface(IJavaInstance)
    ['{4987D595-D4FB-46DA-B6D0-04517CAC5421}']
  end;
  TJInterfaceEtiqueta = class(TJavaGenericImport<JInterfaceEtiquetaClass, JInterfaceEtiqueta>) end;

  JImplementacaoEtiquetaClass = interface(JInterfaceEtiquetaClass)
    ['{D18FB98C-A4D2-46D2-B42D-759D4B9D822C}']
    {class} function _GettContext: JContext; cdecl;
    {class} function init: JImplementacaoEtiqueta; cdecl;
    {class} property tContext: JContext read _GettContext;
  end;

  [JavaSignature('com/elgin/e1/Impressora/Impressoras/ImplementacaoEtiqueta')]
  JImplementacaoEtiqueta = interface(JInterfaceEtiqueta)
    ['{BCAFBCB0-D023-433A-9C57-31557FE46B26}']
    function DespejarArquivo(P1: Integer; P2: JString; P3: JString; P4: Integer; P5: JString): Integer; cdecl;
    function DirectIO(P1: Integer; P2: JString; P3: JString; P4: Integer; P5: TJavaArray<Byte>; P6: Integer; P7: TJavaArray<Byte>; P8: Integer; P9: Boolean): TJavaArray<Integer>; cdecl;
    function EnviaImagem(P1: Integer; P2: JString; P3: JString; P4: Integer; P5: Integer; P6: Integer; P7: JString; P8: JString): Integer; cdecl;
    function ExcluiImagem(P1: Integer; P2: JString; P3: JString; P4: Integer; P5: Integer; P6: JString): Integer; cdecl;
    function Feed(P1: Integer; P2: JString; P3: JString; P4: Integer): Integer; cdecl;
    function GerarBarCode1D(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: Integer; P8: JString; P9: Integer): Integer; cdecl;
    function GerarBox(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer): Integer; cdecl;
    function GerarDataBar(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: Integer; P8: JString; P9: JString): Integer; cdecl;
    function GerarDataBarExpanded(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: Integer; P8: JString; P9: JString): Integer; cdecl;
    function GerarDataMatrix(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: JString): Integer; cdecl;
    function GerarImagem(P1: Integer; P2: Integer; P3: JString): Integer; cdecl;
    function GerarLinha(P1: Integer; P2: Integer; P3: Integer; P4: Integer): Integer; cdecl;
    function GerarMaxiCode(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: Integer; P8: JString): Integer; cdecl;
    function GerarPDF417(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: Integer; P8: Integer; P9: Integer; P10: Integer; P11: JString): Integer; cdecl;
    function GerarQRCodeAuto(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: JString): Integer; cdecl;
    function GerarQRCodeManual(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: Integer; P8: Integer; P9: Integer; P10: Integer; P11: JString): Integer; cdecl;
    function GerarTexto(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: JString): Integer; cdecl;
    function GerarTextoASD(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: JString): Integer; cdecl;
    function GerarTextoCourier(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: JString): Integer; cdecl;
    function GetVersaoDLL: JString; cdecl;
    function Imprime(P1: Integer; P2: JString; P3: JString; P4: Integer): Integer; cdecl;
    function Limpa(P1: Integer): Integer; cdecl;
    function LimpaMemoria(P1: Integer; P2: JString; P3: JString; P4: Integer): Integer; cdecl;
    function LimpaModulo(P1: Integer; P2: JString; P3: JString; P4: Integer; P5: Integer): Integer; cdecl;
    function MemoryStatus(P1: Integer; P2: JString; P3: JString; P4: Integer; P5: Integer): JString; cdecl;
    function Reset(P1: Integer; P2: JString; P3: JString; P4: Integer): Integer; cdecl;
    function SetAlturaGap(P1: Integer; P2: JString; P3: JString; P4: Integer; P5: Integer): Integer; cdecl;
    function SetBackfeed(P1: Integer): Integer; cdecl;
    function SetBaudrate(P1: Integer; P2: JString; P3: JString; P4: Integer; P5: Integer; P6: Integer; P7: Integer; P8: Integer): Integer; cdecl;
    function SetCalor(P1: Integer): Integer; cdecl;
    function SetCortarZero(P1: Integer): Integer; cdecl;
    function SetLength(P1: Integer): Integer; cdecl;
    function SetLogicImgMode(P1: Integer): Integer; cdecl;
    function SetMedidas(P1: Integer): Integer; cdecl;
    function SetMirror(P1: Integer): Integer; cdecl;
    function SetModoContinuo(P1: Integer): Integer; cdecl;
    function SetOffsetColuna(P1: Integer): Integer; cdecl;
    function SetOffsetLinha(P1: Integer): Integer; cdecl;
    function SetPrintStPos(P1: Integer): Integer; cdecl;
    function SetQtde(P1: Integer): Integer; cdecl;
    function SetSensor(P1: Integer): Integer; cdecl;
    function SetSymbolASD(P1: Integer; P2: JString; P3: JString; P4: Integer; P5: Integer): Integer; cdecl;
    function SetTamPixel(P1: Integer; P2: Integer): Integer; cdecl;
    function SetTipoTransferencia(P1: Integer; P2: JString; P3: JString; P4: Integer; P5: Integer): Integer; cdecl;
    function SetVelImpressao(P1: Integer): Integer; cdecl;
    function Status(P1: Integer; P2: JString; P3: JString; P4: Integer): JString; cdecl;
    function StatusEPL(P1: Integer; P2: JString; P3: JString; P4: Integer): JString; cdecl;
    function Teste(P1: Integer; P2: JString; P3: JString; P4: Integer): Integer; cdecl;
    function getServiceResult: Integer; cdecl;
  end;
  TJImplementacaoEtiqueta = class(TJavaGenericImport<JImplementacaoEtiquetaClass, JImplementacaoEtiqueta>) end;

  JImplementacaoM8Class = interface(JImplementacaoAndroidClass)
    ['{5BAB496C-024F-4627-8765-246AED54CC64}']
    {class} function IAbreGaveta(P1: Integer; P2: Integer; P3: Integer): Integer; cdecl;
    {class} function IAbreGavetaElgin: Integer; cdecl;
    {class} function IDefinePosicao(P1: Integer): Integer; cdecl;
    {class} function IDirectIO(P1: TJavaArray<Byte>; P2: Integer): Integer; cdecl;
    {class} function IImpressaoCodigoBarras(P1: Integer; P2: JString; P3: Integer; P4: Integer; P5: Integer; P6: Integer): Integer; cdecl;
    {class} function IImpressaoPDF417(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: JString; P8: Integer): Integer; cdecl;
    {class} function IImpressaoQRCode(P1: JString; P2: Integer; P3: Integer; P4: Integer): Integer; cdecl;
    {class} function IImpressaoTexto(P1: JString): Integer; cdecl;
    {class} function IImprimeBitmap(P1: JBitmap; P2: Integer): Integer; cdecl;
    {class} function IImprimeImagemMemoria(P1: JString; P2: Integer): Integer; cdecl;
    {class} function IStatusImpressora(P1: Integer): Integer; cdecl;
    {class} function init(P1: JConexao; P2: JImplementacaoTermica): JImplementacaoM8; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Impressora/Impressoras/ImplementacaoM8')]
  JImplementacaoM8 = interface(JImplementacaoAndroid)
    ['{9F9300ED-912F-4964-BF4A-B9E35667F0F8}']
  end;
  TJImplementacaoM8 = class(TJavaGenericImport<JImplementacaoM8Class, JImplementacaoM8>) end;

  JImplementacaoM8_1Class = interface(JImplementacaoAndroid_IIImpressaoTextoClass)
    ['{D3709C4E-4A3D-42DD-A592-5BDC660DB3BB}']
    {class} function init(P1: JImplementacaoM8): JImplementacaoM8_1; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Impressora/Impressoras/ImplementacaoM8$1')]
  JImplementacaoM8_1 = interface(JImplementacaoAndroid_IIImpressaoTexto)
    ['{62485130-B532-4571-8003-862DADF051A4}']
    function IImpressaoTexto(P1: JString): Integer; cdecl;
  end;
  TJImplementacaoM8_1 = class(TJavaGenericImport<JImplementacaoM8_1Class, JImplementacaoM8_1>) end;

  JImplementacaoSmartPOSClass = interface(JImplementacaoAndroidClass)
    ['{861494B2-E823-415E-8360-DFB2D9AF971F}']
    {class} function IDefinePosicao(P1: Integer): JString; cdecl;
    {class} function IDirectIO(P1: TJavaArray<Byte>; P2: Integer): Integer; cdecl;
    {class} function IImpressaoCodigoBarras(P1: Integer; P2: JString; P3: Integer; P4: Integer; P5: Integer; P6: JString): Integer; cdecl;
    {class} function IImpressaoPDF417(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: JString; P8: JString): Integer; cdecl;
    {class} function IImpressaoQRCode(P1: JString; P2: Integer; P3: Integer; P4: JString): Integer; cdecl;
    {class} function IImpressaoTexto(P1: JString): Integer; cdecl;
    {class} function IImprimeBitmap(P1: JBitmap; P2: JString): Integer; cdecl;
    {class} function IImprimeImagemMemoria(P1: JString; P2: JString): Integer; cdecl;
    {class} function IStatusImpressora(P1: Integer): Integer; cdecl;
    {class} function init(P1: JConexao; P2: JImplementacaoTermica): JImplementacaoSmartPOS; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Impressora/Impressoras/ImplementacaoSmartPOS')]
  JImplementacaoSmartPOS = interface(JImplementacaoAndroid)
    ['{1BE2E118-951A-4A76-9E9C-CED0BCA9091F}']
  end;
  TJImplementacaoSmartPOS = class(TJavaGenericImport<JImplementacaoSmartPOSClass, JImplementacaoSmartPOS>) end;

  JImplementacaoSmartPOS_1Class = interface(JImplementacaoAndroid_IIImpressaoTextoClass)
    ['{10977F19-3BD4-45F5-A01F-F9F0FCABA775}']
    {class} function init(P1: JImplementacaoSmartPOS): JImplementacaoSmartPOS_1; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Impressora/Impressoras/ImplementacaoSmartPOS$1')]
  JImplementacaoSmartPOS_1 = interface(JImplementacaoAndroid_IIImpressaoTexto)
    ['{A0FFD2D8-ADE6-4EF5-8E5F-23F85C447EB6}']
    function IImpressaoTexto(P1: JString): Integer; cdecl;
  end;
  TJImplementacaoSmartPOS_1 = class(TJavaGenericImport<JImplementacaoSmartPOS_1Class, JImplementacaoSmartPOS_1>) end;

  JInterfaceTermicaClass = interface(IJavaClass)
    ['{683E7CF1-BF8F-4813-81A9-C63A0928308F}']
    {class} function IAbreConexaoImpressora(P1: JContext; P2: Integer; P3: JString; P4: JString; P5: Integer): Integer; cdecl;
    {class} function IAbreGaveta(P1: Integer; P2: Integer; P3: Integer): Integer; cdecl;
    {class} function IAbreGavetaElgin: Integer; cdecl;
    {class} function IAvancaPapel(P1: Integer): Integer; cdecl;
    {class} function ICorte(P1: Integer): Integer; cdecl;
    {class} function IDefineAreaImpressao(P1: Integer; P2: Integer; P3: Integer; P4: Integer): Integer; cdecl;
    {class} function IDefinePosicao(P1: Integer): Integer; cdecl;
    {class} function IDirecaoImpressao(P1: Integer): Integer; cdecl;
    {class} function IDirectIO(P1: TJavaArray<Byte>; P2: Integer; P3: TJavaArray<Byte>; P4: JInteiro): Integer; cdecl;
    {class} function IFechaConexaoImpressora: Integer; cdecl;
    {class} function IGetVersaoDLL: JString; cdecl;
    {class} function IImpressaoCodigoBarras(P1: Integer; P2: JString; P3: Integer; P4: Integer; P5: Integer): Integer; cdecl;
    {class} function IImpressaoPDF417(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: JString): Integer; cdecl;
    {class} function IImpressaoQRCode(P1: JString; P2: Integer; P3: Integer): Integer; cdecl;
    {class} function IImpressaoTexto(P1: JString; P2: Integer; P3: Integer; P4: Integer): Integer; cdecl;
    {class} function IImprimeBitmap(P1: JBitmap): Integer; cdecl;
    {class} function IImprimeImagemMemoria(P1: JString; P2: Integer): Integer; cdecl;
    {class} function IImprimeMPeRetornaPadrao: Integer; cdecl;
    {class} function IImprimeModoPagina: Integer; cdecl;
    {class} function IImprimeXMLCancelamentoSAT(P1: JString; P2: JString; P3: Integer): Integer; cdecl;
    {class} function IImprimeXMLNFCe(P1: JString; P2: Integer; P3: JString; P4: Integer): Integer; cdecl;
    {class} function IImprimeXMLSAT(P1: JString; P2: Integer): Integer; cdecl;
    {class} function IInicializaImpressora: Integer; cdecl;
    {class} function ILimpaBufferModoPagina: Integer; cdecl;
    {class} function IModoPadrao: Integer; cdecl;
    {class} function IModoPagina: Integer; cdecl;
    {class} function IPosicaoImpressaoHorizontal(P1: Integer): Integer; cdecl;
    {class} function IPosicaoImpressaoVertical(P1: Integer): Integer; cdecl;
    {class} function ISinalSonoro(P1: Integer; P2: Integer; P3: Integer): Integer; cdecl;
    {class} function IStatusImpressora(P1: Integer): Integer; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Impressora/Impressoras/InterfaceTermica')]
  JInterfaceTermica = interface(IJavaInstance)
    ['{115C9938-97FC-4EA3-8279-B86C31BEE902}']
  end;
  TJInterfaceTermica = class(TJavaGenericImport<JInterfaceTermicaClass, JInterfaceTermica>) end;

  JImplementacaoTermicaClass = interface(JInterfaceTermicaClass)
    ['{49FEB797-42D1-4091-94FE-A14C56C840BF}']
    {class} function IAbreConexaoImpressora(P1: JContext; P2: Integer; P3: JString; P4: JString; P5: Integer): Integer; cdecl;
    {class} function IAbreGaveta(P1: Integer; P2: Integer; P3: Integer): Integer; cdecl;
    {class} function IAbreGavetaElgin: Integer; cdecl;
    {class} function IAvancaPapel(P1: Integer): Integer; cdecl;
    {class} function ICorte(P1: Integer): Integer; cdecl;
    {class} function IDefineAreaImpressao(P1: Integer; P2: Integer; P3: Integer; P4: Integer): Integer; cdecl;
    {class} function IDefinePosicao(P1: Integer): Integer; cdecl;
    {class} function IDirecaoImpressao(P1: Integer): Integer; cdecl;
    {class} function IDirectIO(P1: TJavaArray<Byte>; P2: Integer; P3: TJavaArray<Byte>; P4: JInteiro): Integer; cdecl;
    {class} function IFechaConexaoImpressora: Integer; cdecl;
    {class} function IGetVersaoDLL: JString; cdecl;
    {class} function IImpressaoCodigoBarras(P1: Integer; P2: JString; P3: Integer; P4: Integer; P5: Integer): Integer; cdecl;
    {class} function IImpressaoPDF417(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: JString): Integer; cdecl;
    {class} function IImpressaoQRCode(P1: JString; P2: Integer; P3: Integer): Integer; cdecl;
    {class} function IImpressaoTexto(P1: JString; P2: Integer; P3: Integer; P4: Integer): Integer; cdecl;
    {class} function IImprimeBitmap(P1: JBitmap): Integer; cdecl;
    {class} function IImprimeImagemMemoria(P1: JString; P2: Integer): Integer; cdecl;
    {class} function IImprimeMPeRetornaPadrao: Integer; cdecl;
    {class} function IImprimeModoPagina: Integer; cdecl;
    {class} function IImprimeXMLCancelamentoSAT(P1: JString; P2: JString; P3: Integer): Integer; cdecl;
    {class} function IImprimeXMLNFCe(P1: JString; P2: Integer; P3: JString; P4: Integer): Integer; cdecl;
    {class} function IImprimeXMLSAT(P1: JString; P2: Integer): Integer; cdecl;
    {class} function IInicializaImpressora: Integer; cdecl;
    {class} function ILimpaBufferModoPagina: Integer; cdecl;
    {class} function IModoPadrao: Integer; cdecl;
    {class} function IModoPagina: Integer; cdecl;
    {class} function IPosicaoImpressaoHorizontal(P1: Integer): Integer; cdecl;
    {class} function IPosicaoImpressaoVertical(P1: Integer): Integer; cdecl;
    {class} function ISinalSonoro(P1: Integer; P2: Integer; P3: Integer): Integer; cdecl;
    {class} function IStatusImpressora(P1: Integer): Integer; cdecl;
    {class} function getServiceResult: Integer; cdecl;
    {class} function init: JImplementacaoTermica; cdecl;
    {class} function printText(P1: JString): Integer; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Impressora/Impressoras/ImplementacaoTermica')]
  JImplementacaoTermica = interface(JInterfaceTermica)
    ['{649E8271-07DB-4F4D-9AEA-61D96922A524}']
  end;
  TJImplementacaoTermica = class(TJavaGenericImport<JImplementacaoTermicaClass, JImplementacaoTermica>) end;

  JInterfaceM8Class = interface(IJavaClass)
    ['{C78E60E8-9F26-4C3D-8E5C-CBBB12E12489}']
    {class} function IAbreGaveta(P1: Integer; P2: Integer; P3: Integer): Integer; cdecl;
    {class} function IAbreGavetaElgin: Integer; cdecl;
    {class} function IDefinePosicao(P1: Integer): Integer; cdecl;
    {class} function IDirectIO(P1: TJavaArray<Byte>; P2: Integer): Integer; cdecl;
    {class} function IImpressaoCodigoBarras(P1: Integer; P2: JString; P3: Integer; P4: Integer; P5: Integer; P6: Integer): Integer; cdecl;
    {class} function IImpressaoPDF417(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: JString; P8: Integer): Integer; cdecl;
    {class} function IImpressaoQRCode(P1: JString; P2: Integer; P3: Integer; P4: Integer): Integer; cdecl;
    {class} function IImpressaoTexto(P1: JString): Integer; cdecl;
    {class} function IImprimeBitmap(P1: JBitmap; P2: Integer): Integer; cdecl;
    {class} function IImprimeImagemMemoria(P1: JString; P2: Integer): Integer; cdecl;
    {class} function IStatusImpressora(P1: Integer): Integer; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Impressora/Impressoras/InterfaceM8')]
  JInterfaceM8 = interface(IJavaInstance)
    ['{921546D7-848C-49CE-98D2-06DF3B9EAD6D}']
  end;
  TJInterfaceM8 = class(TJavaGenericImport<JInterfaceM8Class, JInterfaceM8>) end;

  JInterfaceSmartPOSClass = interface(IJavaClass)
    ['{AE0DF7EE-E7FA-4840-A659-EAC98070C3FA}']
    {class} function IDefinePosicao(P1: Integer): JString; cdecl;
    {class} function IDirectIO(P1: TJavaArray<Byte>; P2: Integer): Integer; cdecl;
    {class} function IImpressaoCodigoBarras(P1: Integer; P2: JString; P3: Integer; P4: Integer; P5: Integer; P6: JString): Integer; cdecl;
    {class} function IImpressaoPDF417(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: JString; P8: JString): Integer; cdecl;
    {class} function IImpressaoQRCode(P1: JString; P2: Integer; P3: Integer; P4: JString): Integer; cdecl;
    {class} function IImpressaoTexto(P1: JString): Integer; cdecl;
    {class} function IImprimeBitmap(P1: JBitmap; P2: JString): Integer; cdecl;
    {class} function IImprimeImagemMemoria(P1: JString; P2: JString): Integer; cdecl;
    {class} function IStatusImpressora(P1: Integer): Integer; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Impressora/Impressoras/InterfaceSmartPOS')]
  JInterfaceSmartPOS = interface(IJavaInstance)
    ['{0E54EB1F-D357-4D61-9314-ED515A5B22B6}']
  end;
  TJInterfaceSmartPOS = class(TJavaGenericImport<JInterfaceSmartPOSClass, JInterfaceSmartPOS>) end;

  JMiniPDVM8Class = interface(JObjectClass)
    ['{95356496-09F9-4F69-96FA-F0AD5289468B}']
    {class} function GetNumeroSerie: JString; cdecl;
    {class} function init: JMiniPDVM8; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Impressora/MiniPDVM8')]
  JMiniPDVM8 = interface(JObject)
    ['{B0F571AD-3D42-4704-BE01-B73E484871BD}']
  end;
  TJMiniPDVM8 = class(TJavaGenericImport<JMiniPDVM8Class, JMiniPDVM8>) end;

  JSmartClass = interface(JObjectClass)
    ['{FA4205DE-3690-4586-B8F1-3554FC37B857}']
    {class} function GetNumeroSerie: JString; cdecl;
    {class} function init: JSmart; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Impressora/Smart')]
  JSmart = interface(JObject)
    ['{E39F2F9E-B27D-4B2B-BDF7-71A244DEDE9A}']
  end;
  TJSmart = class(TJavaGenericImport<JSmartClass, JSmart>) end;

  JTermicaClass = interface(JObjectClass)
    ['{5DB31AC3-4651-4E70-895C-F32A1B6C4196}']
    {class} function AbreConexaoImpressora(P1: Integer; P2: JString; P3: JString; P4: Integer): Integer; cdecl;
    {class} function AbreGaveta(P1: Integer; P2: Integer; P3: Integer): Integer; cdecl;
    {class} function AbreGavetaElgin: Integer; cdecl;
    {class} function AvancaPapel(P1: Integer): Integer; cdecl;
    {class} function Corte(P1: Integer): Integer; cdecl;
    {class} function DefineAreaImpressao(P1: Integer; P2: Integer; P3: Integer; P4: Integer): Integer; cdecl;
    {class} function DefinePosicao(P1: Integer): Integer; cdecl;
    {class} function DirecaoImpressao(P1: Integer): Integer; cdecl;
    {class} function DirectIO(P1: TJavaArray<Byte>; P2: Integer; P3: TJavaArray<Byte>; P4: JInteiro): Integer; cdecl;
    {class} function FechaConexaoImpressora: Integer; cdecl;
    {class} function GetVersaoDLL: JString; cdecl;
    {class} function ImpressaoCodigoBarras(P1: Integer; P2: JString; P3: Integer; P4: Integer; P5: Integer): Integer; cdecl;
    {class} function ImpressaoPDF417(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: JString): Integer; cdecl;
    {class} function ImpressaoQRCode(P1: JString; P2: Integer; P3: Integer): Integer; cdecl;
    {class} function ImpressaoTexto(P1: JString; P2: Integer; P3: Integer; P4: Integer): Integer; cdecl;
    {class} function ImprimeBitmap(P1: JBitmap): Integer; cdecl;
    {class} function ImprimeImagemMemoria(P1: JString; P2: Integer): Integer; cdecl;
    {class} function ImprimeMPeRetornaPadrao: Integer; cdecl;
    {class} function ImprimeModoPagina: Integer; cdecl;
    {class} function ImprimeXMLCancelamentoSAT(P1: JString; P2: JString; P3: Integer): Integer; cdecl;
    {class} function ImprimeXMLNFCe(P1: JString; P2: Integer; P3: JString; P4: Integer): Integer; cdecl;
    {class} function ImprimeXMLSAT(P1: JString; P2: Integer): Integer; cdecl;
    {class} function InicializaImpressora: Integer; cdecl;
    {class} function LimpaBufferModoPagina: Integer; cdecl;
    {class} function ModoPadrao: Integer; cdecl;
    {class} function ModoPagina: Integer; cdecl;
    {class} function PosicaoImpressaoHorizontal(P1: Integer): Integer; cdecl;
    {class} function PosicaoImpressaoVertical(P1: Integer): Integer; cdecl;
    {class} function SinalSonoro(P1: Integer; P2: Integer; P3: Integer): Integer; cdecl;
    {class} function StatusImpressora(P1: Integer): Integer; cdecl;
    {class} function init: JTermica; cdecl;
    {class} function setContext(P1: JContext): Integer; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Impressora/Termica')]
  JTermica = interface(JObject)
    ['{7825CB21-8C87-43F0-B91E-732081C10056}']
  end;
  TJTermica = class(TJavaGenericImport<JTermicaClass, JTermica>) end;

  JCodigoErroClass = interface(JObjectClass)
    ['{3EA78438-8DD8-490D-9AA4-0A960A68CB8B}']
    {class} function _GetALIQUOTA_IMPOSTO_INVALIDA: Integer; cdecl;
    {class} function _GetALIQUOTA_ISSQN_INVALIDA: Integer; cdecl;
    {class} function _GetARQUIVO_NAO_ENCONTRADO: Integer; cdecl;
    {class} function _GetARQUIVO_NAO_EXISTE: Integer; cdecl;
    {class} function _GetARQUIVO_NAO_PODE_SER_ABERTO: Integer; cdecl;
    {class} function _GetASSINATURA_QRCODE_INVALIDA: Integer; cdecl;
    {class} function _GetBAIRRO_INVALIDO: Integer; cdecl;
    {class} function _GetBAL_BALANCA_EM_USO: Integer; cdecl;
    {class} function _GetBAL_BALANCA_INVALIDA: Integer; cdecl;
    {class} function _GetBAL_BAUDRATE_INVALIDO: Integer; cdecl;
    {class} function _GetBAL_BAUDRATE_NAO_SUPORTADO: Integer; cdecl;
    {class} function _GetBAL_COMANDO_NAO_SUPORTADO_PELA_BALANCA: Integer; cdecl;
    {class} function _GetBAL_COMANDO_NAO_SUPORTADO_PELO_PROTOCOLO: Integer; cdecl;
    {class} function _GetBAL_COMBINACAO_DE_PARAMETROS_INVALIDA: Integer; cdecl;
    {class} function _GetBAL_CONEXAO_ATIVA: Integer; cdecl;
    {class} function _GetBAL_CONFIGS_SERIAIS_NAO_SUPORTADAS_PELO_PROTOCOLO: Integer; cdecl;
    {class} function _GetBAL_FALHA_AO_ENVIAR_PRECO: Integer; cdecl;
    {class} function _GetBAL_FALHA_NA_LEITURA_DO_PESO: Integer; cdecl;
    {class} function _GetBAL_LENGTH_INVALIDO: Integer; cdecl;
    {class} function _GetBAL_LENGTH_NAO_SUPORTADO: Integer; cdecl;
    {class} function _GetBAL_NENHUMA_BALANCA_EM_USO: Integer; cdecl;
    {class} function _GetBAL_NENHUM_PROTOCOLO_EM_USO: Integer; cdecl;
    {class} function _GetBAL_PARITY_INVALIDO: Integer; cdecl;
    {class} function _GetBAL_PARITY_NAO_SUPORTADO: Integer; cdecl;
    {class} function _GetBAL_PRECO_INVALIDO: Integer; cdecl;
    {class} function _GetBAL_PROTOCOLO_EM_USO: Integer; cdecl;
    {class} function _GetBAL_PROTOCOLO_INVALIDO: Integer; cdecl;
    {class} function _GetBAL_PROTOCOLO_NAO_SUPORTADO: Integer; cdecl;
    {class} function _GetBAL_PROTOCOLO_NAO_SUPORTADO_PELAS_CONFIGS_SERIAIS: Integer; cdecl;
    {class} function _GetBAL_QTD_LEITURAS_INVALIDA: Integer; cdecl;
    {class} function _GetBAL_STOPBITS_INVALIDO: Integer; cdecl;
    {class} function _GetBAL_STOPBITS_NAO_SUPORTADO: Integer; cdecl;
    {class} function _GetBARCODE1D_ALTURA_INVALIDA: Integer; cdecl;
    {class} function _GetBARCODE1D_CARACTERE_INVALIDO: Integer; cdecl;
    {class} function _GetBARCODE1D_CODIGO_INVALIDO: Integer; cdecl;
    {class} function _GetBARCODE1D_COORDENADA_X_INVALIDA: Integer; cdecl;
    {class} function _GetBARCODE1D_COORDENADA_Y_INVALIDA: Integer; cdecl;
    {class} function _GetBARCODE1D_LARGURA_BARRAS_ESTREITAS_INVALIDA: Integer; cdecl;
    {class} function _GetBARCODE1D_LARGURA_BARRAS_LARGAS_INVALIDA: Integer; cdecl;
    {class} function _GetBARCODE1D_ROTACAO_INVALIDA: Integer; cdecl;
    {class} function _GetBARCODE1D_TAMANHO_INVALIDO: Integer; cdecl;
    {class} function _GetBARCODE1D_TIPO_INVALIDO: Integer; cdecl;
    {class} function _GetBASE_CALCULO_INVALIDA: Integer; cdecl;
    {class} function _GetBAUDRATE_INVALIDO: Integer; cdecl;
    {class} function _GetBLUETOOTH_DESATIVADO: Integer; cdecl;
    {class} function _GetBOX_ALTURA_INVALIDA: Integer; cdecl;
    {class} function _GetBOX_COMPRIMENTO_INVALIDO: Integer; cdecl;
    {class} function _GetBOX_COORDENADA_X_INVALIDA: Integer; cdecl;
    {class} function _GetBOX_COORDENADA_Y_INVALIDA: Integer; cdecl;
    {class} function _GetBOX_GROSSURA_BORDAS_HORIZONTAIS_INVALIDA: Integer; cdecl;
    {class} function _GetBOX_GROSSURA_BORDAS_VERTICAIS_INVALIDA: Integer; cdecl;
    {class} function _GetCB_ALTURA_INVALIDA: Integer; cdecl;
    {class} function _GetCB_AREA_DE_IMPRESSAO_EXCEDIDA: Integer; cdecl;
    {class} function _GetCB_DADOS_INVALIDOS: Integer; cdecl;
    {class} function _GetCB_HRI_INVALIDO: Integer; cdecl;
    {class} function _GetCB_LARGURA_INVALIDA: Integer; cdecl;
    {class} function _GetCB_TIPO_INVALIDO: Integer; cdecl;
    {class} function _GetCEST_INVALIDO: Integer; cdecl;
    {class} function _GetCFOP_INVALIDO: Integer; cdecl;
    {class} function _GetCHAMADA_NAO_PERMITIDA: Integer; cdecl;
    {class} function _GetCHAVE_CANCELAMENTO_INVALIDA: Integer; cdecl;
    {class} function _GetCNPJ_INVALIDO: Integer; cdecl;
    {class} function _GetCODIGO_MEIO_PAGAMENTO_INVALIDO: Integer; cdecl;
    {class} function _GetCODIGO_MUNICIPIO_FATO_GERADOR_INVALIDO: Integer; cdecl;
    {class} function _GetCODIGO_PRODUTO_INVALIDO: Integer; cdecl;
    {class} function _GetCODIGO_TRIBUTACAO_ISSQN_INVALIDO: Integer; cdecl;
    {class} function _GetCOMANDO_INVALIDO: Integer; cdecl;
    {class} function _GetCOMPLEMENTO_INVALIDO: Integer; cdecl;
    {class} function _GetCONEXAO_ATIVA: Integer; cdecl;
    {class} function _GetCONEXAO_ATIVA_OUTRO: Integer; cdecl;
    {class} function _GetCONEXAO_NEGADA: Integer; cdecl;
    {class} function _GetCONTEUDO_CAMPO_INVALIDO: Integer; cdecl;
    {class} function _GetCPF_INVALIDO: Integer; cdecl;
    {class} function _GetCREDENCIADORA_CARTAO_INVALIDO: Integer; cdecl;
    {class} function _GetCREGTRIBISSQN_INVALIDO: Integer; cdecl;
    {class} function _GetCSOSN_INVALIDO: Integer; cdecl;
    {class} function _GetCST_INVALIDO: Integer; cdecl;
    {class} function _GetDADOS_PDF_INVALIDOS: Integer; cdecl;
    {class} function _GetDADOS_QR_INVALIDOS: Integer; cdecl;
    {class} function _GetDATABAREXPANDED_ALPHANUMERIC_CARACTERE_INVALIDO: Integer; cdecl;
    {class} function _GetDATABAREXPANDED_ALPHANUMERIC_TAMANHO_INVALIDO: Integer; cdecl;
    {class} function _GetDATABAREXPANDED_ALTURA_INVALIDA: Integer; cdecl;
    {class} function _GetDATABAREXPANDED_BAR_RATIO_INVALIDO: Integer; cdecl;
    {class} function _GetDATABAREXPANDED_CODIGO_INVALIDO: Integer; cdecl;
    {class} function _GetDATABAREXPANDED_COORDENADA_X_INVALIDA: Integer; cdecl;
    {class} function _GetDATABAREXPANDED_COORDENADA_Y_INVALIDA: Integer; cdecl;
    {class} function _GetDATABAREXPANDED_PIXEL_MULTIPLIER_INVALIDO: Integer; cdecl;
    {class} function _GetDATABAREXPANDED_ROTACAO_INVALIDA: Integer; cdecl;
    {class} function _GetDATABAREXPANDED_SEGMENTS_PER_ROW_INVALIDO: Integer; cdecl;
    {class} function _GetDATABAR_ALTURA_INVALIDA: Integer; cdecl;
    {class} function _GetDATABAR_BAR_RATIO_INVALIDO: Integer; cdecl;
    {class} function _GetDATABAR_CODIGO_INVALIDO: Integer; cdecl;
    {class} function _GetDATABAR_COORDENADA_X_INVALIDA: Integer; cdecl;
    {class} function _GetDATABAR_COORDENADA_Y_INVALIDA: Integer; cdecl;
    {class} function _GetDATABAR_NUMERIC_LINEAR_DATA_CARACTERE_INVALIDO: Integer; cdecl;
    {class} function _GetDATABAR_NUMERIC_LINEAR_DATA_TAMANHO_INVALIDO: Integer; cdecl;
    {class} function _GetDATABAR_PIXEL_MULTIPLIER_INVALIDO: Integer; cdecl;
    {class} function _GetDATABAR_ROTACAO_INVALIDA: Integer; cdecl;
    {class} function _GetDATABAR_TIPO_INVALIDO: Integer; cdecl;
    {class} function _GetDATAMATRIX_CODIGO_INVALIDO: Integer; cdecl;
    {class} function _GetDATAMATRIX_COLUMNS_INVALIDAS: Integer; cdecl;
    {class} function _GetDATAMATRIX_COORDENADA_X_INVALIDA: Integer; cdecl;
    {class} function _GetDATAMATRIX_COORDENADA_Y_INVALIDA: Integer; cdecl;
    {class} function _GetDATAMATRIX_MULTIPLICADOR_INVALIDO: Integer; cdecl;
    {class} function _GetDATAMATRIX_ROTACAO_INVALIDA: Integer; cdecl;
    {class} function _GetDATAMATRIX_ROWS_INVALIDAS: Integer; cdecl;
    {class} function _GetDESCRICAO_PRODUTO_INVALIDA: Integer; cdecl;
    {class} function _GetDIRECAO_INVALIDA: Integer; cdecl;
    {class} function _GetDISPOSITIVO_NAO_ENCONTRADO: Integer; cdecl;
    {class} function _GetDISPOSITIVO_NAO_EXISTE: Integer; cdecl;
    {class} function _GetDISPOSITIVO_NAO_PAREADO: Integer; cdecl;
    {class} function _GetDISPOSITIVO_NAO_SUPORTA_BT: Integer; cdecl;
    {class} function _GetERROR_CORRECTION_LEVEL_INVALIDO: Integer; cdecl;
    {class} function _GetERRO_ABERTURA_NAO_AUTORIZADA: Integer; cdecl;
    {class} function _GetERRO_ABERTURA_PORTA: Integer; cdecl;
    {class} function _GetERRO_ALTURA_GAP: Integer; cdecl;
    {class} function _GetERRO_AO_CARREGAR_BIBLIOTECA_DA_ETIQUETA: Integer; cdecl;
    {class} function _GetERRO_AO_CARREGAR_BIBLIOTECA_DA_IMPRESSORA: Integer; cdecl;
    {class} function _GetERRO_AO_CARREGAR_BIBLIOTECA_DO_E1SAT: Integer; cdecl;
    {class} function _GetERRO_AO_CARREGAR_BIBLIOTECA_DO_SAT: Integer; cdecl;
    {class} function _GetERRO_BACKFEED: Integer; cdecl;
    {class} function _GetERRO_BAUDRATE_BAUDRATE: Integer; cdecl;
    {class} function _GetERRO_BAUDRATE_DATA_LENGTH: Integer; cdecl;
    {class} function _GetERRO_BAUDRATE_PARITY: Integer; cdecl;
    {class} function _GetERRO_BAUDRATE_STOP_BIT: Integer; cdecl;
    {class} function _GetERRO_CALOR: Integer; cdecl;
    {class} function _GetERRO_CAMPOS_OVERFLOW: Integer; cdecl;
    {class} function _GetERRO_CLAIM_UL: Integer; cdecl;
    {class} function _GetERRO_CONEXAO_BLUETOOTH: Integer; cdecl;
    {class} function _GetERRO_CORTAR_ZERO: Integer; cdecl;
    {class} function _GetERRO_DESCONHECIDO: Integer; cdecl;
    {class} function _GetERRO_DE_ABERTURA_PORTA_USB: Integer; cdecl;
    {class} function _GetERRO_ENVIA_IMAGEM_ARQUIVO: Integer; cdecl;
    {class} function _GetERRO_ENVIA_IMAGEM_FORMATO: Integer; cdecl;
    {class} function _GetERRO_ENVIA_IMAGEM_MODULO: Integer; cdecl;
    {class} function _GetERRO_ENVIA_IMAGEM_NOME_CARACTERE: Integer; cdecl;
    {class} function _GetERRO_ENVIA_IMAGEM_NOME_TAMANHO: Integer; cdecl;
    {class} function _GetERRO_ESCRITA: Integer; cdecl;
    {class} function _GetERRO_ESCRITA_PORTA: Integer; cdecl;
    {class} function _GetERRO_EXCLUI_IMAGEM_MODULO: Integer; cdecl;
    {class} function _GetERRO_EXCLUI_IMAGEM_NOME_CARACTERE: Integer; cdecl;
    {class} function _GetERRO_EXCLUI_IMAGEM_NOME_TAMANHO: Integer; cdecl;
    {class} function _GetERRO_FECHAMENTO_NAO_AUTORIZADO: Integer; cdecl;
    {class} function _GetERRO_FECHAMENTO_PORTA: Integer; cdecl;
    {class} function _GetERRO_FUNCAO_NAO_CHAMADA_PELO_SERVICO: Integer; cdecl;
    {class} function _GetERRO_FUNCAO_NAO_DISPONIVEL_VIA_SERVICO: Integer; cdecl;
    {class} function _GetERRO_FUNCAO_NAO_SUPORTADA: Integer; cdecl;
    {class} function _GetERRO_LEITURA_PORTA: Integer; cdecl;
    {class} function _GetERRO_LENGTH: Integer; cdecl;
    {class} function _GetERRO_LIMPAR: Integer; cdecl;
    {class} function _GetERRO_LIMPA_MODULO_MODULO: Integer; cdecl;
    {class} function _GetERRO_LOGIC_IMG_MODE: Integer; cdecl;
    {class} function _GetERRO_MEDIDA: Integer; cdecl;
    {class} function _GetERRO_MEMORY_STATUS_TIPO_DADOS: Integer; cdecl;
    {class} function _GetERRO_MIRROR: Integer; cdecl;
    {class} function _GetERRO_MODO_CONTINUO: Integer; cdecl;
    {class} function _GetERRO_NENHUM_BYTE_ENVIADO: Integer; cdecl;
    {class} function _GetERRO_NOT_ACTIVITY: Integer; cdecl;
    {class} function _GetERRO_OFFSET_COLUNA: Integer; cdecl;
    {class} function _GetERRO_OFFSET_LINHA: Integer; cdecl;
    {class} function _GetERRO_PDF_DESCONHECIDO: Integer; cdecl;
    {class} function _GetERRO_PRINT_ST_POS: Integer; cdecl;
    {class} function _GetERRO_QTDE: Integer; cdecl;
    {class} function _GetERRO_SENSOR: Integer; cdecl;
    {class} function _GetERRO_SERIAL_DESCONHECIDO: Integer; cdecl;
    {class} function _GetERRO_SERVICO_NAO_INICIADO: Integer; cdecl;
    {class} function _GetERRO_SYMBOL: Integer; cdecl;
    {class} function _GetERRO_TAM_PIXEL: Integer; cdecl;
    {class} function _GetERRO_TIPO_TRANSFERENCIA: Integer; cdecl;
    {class} function _GetERRO_VEL_IMPRESSAO: Integer; cdecl;
    {class} function _GetERRO_XSD: Integer; cdecl;
    {class} function _GetESTE_JSON_NAO_E_UM_OBJETO: Integer; cdecl;
    {class} function _GetGRUPO_INVALIDO: Integer; cdecl;
    {class} function _GetGTIN_INVALIDO: Integer; cdecl;
    {class} function _GetHEIGHT_INVALIDO: Integer; cdecl;
    {class} function _GetIDENTIFICADOR_CAMPO_INVALIDO: Integer; cdecl;
    {class} function _GetIE_INVALIDO: Integer; cdecl;
    {class} function _GetIMAGEM_COORDENADA_X_INVALIDA: Integer; cdecl;
    {class} function _GetIMAGEM_COORDENADA_Y_INVALIDA: Integer; cdecl;
    {class} function _GetIMAGEM_NOME_CARACTERE_INVALIDO: Integer; cdecl;
    {class} function _GetIMAGEM_NOME_TAMANHO_INVALIDO: Integer; cdecl;
    {class} function _GetIM_INVALIDO: Integer; cdecl;
    {class} function _GetINDICADOR_INCETIVO_FISCAL_ISSQN_INVALIDO: Integer; cdecl;
    {class} function _GetINDRATISSQN_INVALIDO: Integer; cdecl;
    {class} function _GetINFORMACOES_ADICIONAIS_PRODUTO_INVALIDA: Integer; cdecl;
    {class} function _GetITEM_LISTA_SERVICO_INVALIDO: Integer; cdecl;
    {class} function _GetKEY_INVALIDO: Integer; cdecl;
    {class} function _GetLINHA_ALTURA_INVALIDA: Integer; cdecl;
    {class} function _GetLINHA_COMPRIMENTO_INVALIDO: Integer; cdecl;
    {class} function _GetLINHA_COORDENADA_X_INVALIDA: Integer; cdecl;
    {class} function _GetLINHA_COORDENADA_Y_INVALIDA: Integer; cdecl;
    {class} function _GetLOGRADOURO_INVALIDO: Integer; cdecl;
    {class} function _GetMAC_ADDRESS_INVALIDO: Integer; cdecl;
    {class} function _GetMAXICODE_CLASS_OF_SERVICE_INVALIDA: Integer; cdecl;
    {class} function _GetMAXICODE_CODIGO_INVALIDO: Integer; cdecl;
    {class} function _GetMAXICODE_COORDENADA_X_INVALIDA: Integer; cdecl;
    {class} function _GetMAXICODE_COORDENADA_Y_INVALIDA: Integer; cdecl;
    {class} function _GetMAXICODE_COUNTRY_INVALIDO: Integer; cdecl;
    {class} function _GetMAXICODE_PRIMARY_ZIP_INVALIDO: Integer; cdecl;
    {class} function _GetMAXICODE_ROTACAO_INVALIDA: Integer; cdecl;
    {class} function _GetMAXICODE_SECONDARY_ZIP_INVALIDO: Integer; cdecl;
    {class} function _GetMODELO_INVALIDO: Integer; cdecl;
    {class} function _GetMODULO_FUNCAO_NAO_ENCONTRADO: Integer; cdecl;
    {class} function _GetMUNICIPIO_INVALIDO: Integer; cdecl;
    {class} function _GetNAO_FOI_POSSIVEL_INICIAR_O_SERVICO: Integer; cdecl;
    {class} function _GetNATUREZA_OPERACAO_INVALIDA: Integer; cdecl;
    {class} function _GetNCM_INVALIDO: Integer; cdecl;
    {class} function _GetNENHUM_DADO_RETORNADO: Integer; cdecl;
    {class} function _GetNIVEL_DE_CORRECAO_INVALIDO: Integer; cdecl;
    {class} function _GetNOME_DESTINARIO_INVALIDO: Integer; cdecl;
    {class} function _GetNUMBER_COLUMNS_INVALIDO: Integer; cdecl;
    {class} function _GetNUMBER_ROWS_INVALIDO: Integer; cdecl;
    {class} function _GetNUMERO_CAIXA_INVALIDO: Integer; cdecl;
    {class} function _GetNUMERO_INVALIDO: Integer; cdecl;
    {class} function _GetNUMERO_ITEM_INVALIDO: Integer; cdecl;
    {class} function _GetOPERACAO_INVALIDA: Integer; cdecl;
    {class} function _GetOPTIONS_INVALIDO: Integer; cdecl;
    {class} function _GetORIGEM_MERCADORIA_INVALIDA: Integer; cdecl;
    {class} function _GetPARAMETRO_CONF_INVALIDO: Integer; cdecl;
    {class} function _GetPARAMETRO_NAO_ENCONTRADO: Integer; cdecl;
    {class} function _GetPARAMETRO_TIPO_STATUS_INVALIDO: Integer; cdecl;
    {class} function _GetPDF417_CODIGO_INVALIDO: Integer; cdecl;
    {class} function _GetPDF417_COLUMN_NUMBER_INVALIDO: Integer; cdecl;
    {class} function _GetPDF417_COORDENADA_X_INVALIDA: Integer; cdecl;
    {class} function _GetPDF417_COORDENADA_Y_INVALIDA: Integer; cdecl;
    {class} function _GetPDF417_MULTIPLICADOR_HORIZONTAL_INVALIDO: Integer; cdecl;
    {class} function _GetPDF417_MULTIPLICADOR_VERTICAL_INVALIDO: Integer; cdecl;
    {class} function _GetPDF417_ROTACAO_INVALIDA: Integer; cdecl;
    {class} function _GetPDF417_ROW_NUMBER_INVALIDO: Integer; cdecl;
    {class} function _GetPDF417_SECURITY_LEVEL_INVALIDO: Integer; cdecl;
    {class} function _GetPDF417_TIPO_INVALIDO: Integer; cdecl;
    {class} function _GetPDF_417_ASPECT_RATIO_INVALIDO: Integer; cdecl;
    {class} function _GetPERCENTUAL_ALIQUOTA_COFINS_INVALIDA: Integer; cdecl;
    {class} function _GetPERCENTUAL_ALIQUOTA_PIS_INVALIDA: Integer; cdecl;
    {class} function _GetPERMISSAO_NEGADA: Integer; cdecl;
    {class} function _GetPINO_INVALIDO: Integer; cdecl;
    {class} function _GetPORTA_FECHADA: Integer; cdecl;
    {class} function _GetPORTA_TCP_INVALIDA: Integer; cdecl;
    {class} function _GetPOSICAO_INVALIDA: Integer; cdecl;
    {class} function _GetPOS_IMP_HORIZONTAL_INVALIDA: Integer; cdecl;
    {class} function _GetPRINTTEXT_ATRIBUTO_INVALIDO: Integer; cdecl;
    {class} function _GetQRCODEAUTO_CODIGO_INVALIDO: Integer; cdecl;
    {class} function _GetQRCODEAUTO_COORDENADA_X_INVALIDA: Integer; cdecl;
    {class} function _GetQRCODEAUTO_COORDENADA_Y_INVALIDA: Integer; cdecl;
    {class} function _GetQRCODEAUTO_MULTIPLICADOR_INVALIDO: Integer; cdecl;
    {class} function _GetQRCODEAUTO_ROTACAO_INVALIDA: Integer; cdecl;
    {class} function _GetQRCODEMANUAL_CODIGO_INVALIDO: Integer; cdecl;
    {class} function _GetQRCODEMANUAL_COORDENADA_X_INVALIDA: Integer; cdecl;
    {class} function _GetQRCODEMANUAL_COORDENADA_Y_INVALIDA: Integer; cdecl;
    {class} function _GetQRCODEMANUAL_INPUT_CONFIG_INVALIDO: Integer; cdecl;
    {class} function _GetQRCODEMANUAL_INPUT_INVALIDO: Integer; cdecl;
    {class} function _GetQRCODEMANUAL_MULTIPLICADOR_INVALIDO: Integer; cdecl;
    {class} function _GetQRCODEMANUAL_NUM_CHARS_8BIT_INVALIDO: Integer; cdecl;
    {class} function _GetQRCODEMANUAL_NUM_MASCARA_INVALIDO: Integer; cdecl;
    {class} function _GetQRCODEMANUAL_NUM_MODELO_INVALIDO: Integer; cdecl;
    {class} function _GetQRCODEMANUAL_NVL_COR_ERRO_INVALIDO: Integer; cdecl;
    {class} function _GetQRCODEMANUAL_ROTACAO_INVALIDA: Integer; cdecl;
    {class} function _GetQUANTIDADE_COMERCIAL_INVALIDA: Integer; cdecl;
    {class} function _GetQUANTIDADE_ELEMENTO_EXCEDIDA: Integer; cdecl;
    {class} function _GetQUANTIDADE_FORA_INTERVALO: Integer; cdecl;
    {class} function _GetQUANTIDADE_INVALIDA: Integer; cdecl;
    {class} function _GetQUANTIDADE_MEIO_PAGAMENTO_EXCEDIDA: Integer; cdecl;
    {class} function _GetQUANTIDADE_VENDIDA_COFINS_INVALIDA: Integer; cdecl;
    {class} function _GetQUANTIDADE_VENDIDA_PIS_INVALIDA: Integer; cdecl;
    {class} function _GetRECONEXOES_ESGOTADAS: Integer; cdecl;
    {class} function _GetREGRA_CALCULO_INVALIDA: Integer; cdecl;
    {class} function _GetSCALA_INVALIDA: Integer; cdecl;
    {class} function _GetSERVICO_OCUPADO: Integer; cdecl;
    {class} function _GetSIGNAC_INVALIDA: Integer; cdecl;
    {class} function _GetSTATEPRINTER_QSESEMPAPEL: Integer; cdecl;
    {class} function _GetSTATEPRINTER_QSESEMPAPELETAMPA: Integer; cdecl;
    {class} function _GetSTATEPRINTER_SEMPAPEL: Integer; cdecl;
    {class} function _GetSTATEPRINTER_SUCESSO: Integer; cdecl;
    {class} function _GetSTATEPRINTER_TAMPAABERTA: Integer; cdecl;
    {class} function _GetSTATEPRINTER_TAMPAEPAPEL: Integer; cdecl;
    {class} function _GetSTATUS_NAO_SUPORTADO: Integer; cdecl;
    {class} function _GetSTILO_INVALIDO: Integer; cdecl;
    {class} function _GetSUCESSO: Integer; cdecl;
    {class} function _GetTAMANHO_INFORMACOES_COMPLEMENTARES_INVALIDO: Integer; cdecl;
    {class} function _GetTAMANHO_INVALIDO: Integer; cdecl;
    {class} function _GetTAMANHO_QR_INVALIDO: Integer; cdecl;
    {class} function _GetTEMPO_FORA_LIMITE: Integer; cdecl;
    {class} function _GetTEMPO_INVALIDO: Integer; cdecl;
    {class} function _GetTEXTOASD_COORDENADA_X_INVALIDA: Integer; cdecl;
    {class} function _GetTEXTOASD_COORDENADA_Y_INVALIDA: Integer; cdecl;
    {class} function _GetTEXTOASD_MULTIPLICADOR_HORIZONTAL_INVALIDO: Integer; cdecl;
    {class} function _GetTEXTOASD_MULTIPLICADOR_VERTICAL_INVALIDO: Integer; cdecl;
    {class} function _GetTEXTOASD_ROTACAO_INVALIDA: Integer; cdecl;
    {class} function _GetTEXTOASD_TAMANHO_INVALIDO: Integer; cdecl;
    {class} function _GetTEXTOASD_TEXTO_INVALIDO: Integer; cdecl;
    {class} function _GetTEXTOCOURIER_COORDENADA_X_INVALIDA: Integer; cdecl;
    {class} function _GetTEXTOCOURIER_COORDENADA_Y_INVALIDA: Integer; cdecl;
    {class} function _GetTEXTOCOURIER_MULTIPLICADOR_HORIZONTAL_INVALIDO: Integer; cdecl;
    {class} function _GetTEXTOCOURIER_MULTIPLICADOR_VERTICAL_INVALIDO: Integer; cdecl;
    {class} function _GetTEXTOCOURIER_ROTACAO_INVALIDA: Integer; cdecl;
    {class} function _GetTEXTOCOURIER_SYMBOL_INVALIDO: Integer; cdecl;
    {class} function _GetTEXTOCOURIER_TEXTO_INVALIDO: Integer; cdecl;
    {class} function _GetTEXTO_COORDENADA_X_INVALIDA: Integer; cdecl;
    {class} function _GetTEXTO_COORDENADA_Y_INVALIDA: Integer; cdecl;
    {class} function _GetTEXTO_FONTE_INVALIDA: Integer; cdecl;
    {class} function _GetTEXTO_MULTIPLICADOR_HORIZONTAL_INVALIDO: Integer; cdecl;
    {class} function _GetTEXTO_MULTIPLICADOR_VERTICAL_INVALIDO: Integer; cdecl;
    {class} function _GetTEXTO_ROTACAO_INVALIDA: Integer; cdecl;
    {class} function _GetTEXTO_TEXTO_INVALIDO: Integer; cdecl;
    {class} function _GetTHREAD_EXECUTION_EXCEPTION: Integer; cdecl;
    {class} function _GetTHREAD_INTERRUPTED_EXCEPTION: Integer; cdecl;
    {class} function _GetTIPO_EMISSAO_INDEFINIDA: Integer; cdecl;
    {class} function _GetTIPO_INVALIDO: Integer; cdecl;
    {class} function _GetTIPO_PARAMETRO_INVALIDO: Integer; cdecl;
    {class} function _GetUF_INVALIDO: Integer; cdecl;
    {class} function _GetUNIDADE_COMERCIAL_INVALIDA: Integer; cdecl;
    {class} function _GetVALOR_ACRESCIMO_INVALIDO: Integer; cdecl;
    {class} function _GetVALOR_ACRESCIMO_SUBTOTAL_INVALIDO: Integer; cdecl;
    {class} function _GetVALOR_ALIQUOTA_COFINS_INVALIDA: Integer; cdecl;
    {class} function _GetVALOR_ALIQUOTA_PIS_INVALIDA: Integer; cdecl;
    {class} function _GetVALOR_DEDUCOES_ISSQN_INVALIDA: Integer; cdecl;
    {class} function _GetVALOR_DESCONTO_INVALIDO: Integer; cdecl;
    {class} function _GetVALOR_DESCONTO_SUBTOTAL_INVALIDO: Integer; cdecl;
    {class} function _GetVALOR_MEIO_PAGAMENTO_INVALIDO: Integer; cdecl;
    {class} function _GetVALOR_UNITARIO_INVALIDO: Integer; cdecl;
    {class} function _GetVALOR_VCFELEI12741_INVALIDO: Integer; cdecl;
    {class} function _GetVERSAO_DADOS_ENT_INVALIDO: Integer; cdecl;
    {class} function _GetVERSAO_XMLNFCE_INDEFINIDA: Integer; cdecl;
    {class} function _GetVERSAO_XMLNFCE_NAO_SUPORTADA: Integer; cdecl;
    {class} function _GetVITEM12741_INVALIDO: Integer; cdecl;
    {class} function _GetWIDTH_INVALIDO: Integer; cdecl;
    {class} function _GetXSD_NAO_ENCONTRADO: Integer; cdecl;
    {class} function _Get__ERRO_AO_CARREGAR_DLL_IMPRESSORA: JString; cdecl;
    {class} function _Get__ERRO_AO_CARREGAR_DLL_SAT: JString; cdecl;
    {class} function _Get__ERRO_AO_VALIDAR_DLL_SAL: JString; cdecl;
    {class} function _Get__ERRO_CANCELAR_VENDA_SAT: JString; cdecl;
    {class} function _Get__SUCESSO: JString; cdecl;
    {class} function init: JCodigoErro; cdecl;
    {class} property ALIQUOTA_IMPOSTO_INVALIDA: Integer read _GetALIQUOTA_IMPOSTO_INVALIDA;
    {class} property ALIQUOTA_ISSQN_INVALIDA: Integer read _GetALIQUOTA_ISSQN_INVALIDA;
    {class} property ARQUIVO_NAO_ENCONTRADO: Integer read _GetARQUIVO_NAO_ENCONTRADO;
    {class} property ARQUIVO_NAO_EXISTE: Integer read _GetARQUIVO_NAO_EXISTE;
    {class} property ARQUIVO_NAO_PODE_SER_ABERTO: Integer read _GetARQUIVO_NAO_PODE_SER_ABERTO;
    {class} property ASSINATURA_QRCODE_INVALIDA: Integer read _GetASSINATURA_QRCODE_INVALIDA;
    {class} property BAIRRO_INVALIDO: Integer read _GetBAIRRO_INVALIDO;
    {class} property BAL_BALANCA_EM_USO: Integer read _GetBAL_BALANCA_EM_USO;
    {class} property BAL_BALANCA_INVALIDA: Integer read _GetBAL_BALANCA_INVALIDA;
    {class} property BAL_BAUDRATE_INVALIDO: Integer read _GetBAL_BAUDRATE_INVALIDO;
    {class} property BAL_BAUDRATE_NAO_SUPORTADO: Integer read _GetBAL_BAUDRATE_NAO_SUPORTADO;
    {class} property BAL_COMANDO_NAO_SUPORTADO_PELA_BALANCA: Integer read _GetBAL_COMANDO_NAO_SUPORTADO_PELA_BALANCA;
    {class} property BAL_COMANDO_NAO_SUPORTADO_PELO_PROTOCOLO: Integer read _GetBAL_COMANDO_NAO_SUPORTADO_PELO_PROTOCOLO;
    {class} property BAL_COMBINACAO_DE_PARAMETROS_INVALIDA: Integer read _GetBAL_COMBINACAO_DE_PARAMETROS_INVALIDA;
    {class} property BAL_CONEXAO_ATIVA: Integer read _GetBAL_CONEXAO_ATIVA;
    {class} property BAL_CONFIGS_SERIAIS_NAO_SUPORTADAS_PELO_PROTOCOLO: Integer read _GetBAL_CONFIGS_SERIAIS_NAO_SUPORTADAS_PELO_PROTOCOLO;
    {class} property BAL_FALHA_AO_ENVIAR_PRECO: Integer read _GetBAL_FALHA_AO_ENVIAR_PRECO;
    {class} property BAL_FALHA_NA_LEITURA_DO_PESO: Integer read _GetBAL_FALHA_NA_LEITURA_DO_PESO;
    {class} property BAL_LENGTH_INVALIDO: Integer read _GetBAL_LENGTH_INVALIDO;
    {class} property BAL_LENGTH_NAO_SUPORTADO: Integer read _GetBAL_LENGTH_NAO_SUPORTADO;
    {class} property BAL_NENHUMA_BALANCA_EM_USO: Integer read _GetBAL_NENHUMA_BALANCA_EM_USO;
    {class} property BAL_NENHUM_PROTOCOLO_EM_USO: Integer read _GetBAL_NENHUM_PROTOCOLO_EM_USO;
    {class} property BAL_PARITY_INVALIDO: Integer read _GetBAL_PARITY_INVALIDO;
    {class} property BAL_PARITY_NAO_SUPORTADO: Integer read _GetBAL_PARITY_NAO_SUPORTADO;
    {class} property BAL_PRECO_INVALIDO: Integer read _GetBAL_PRECO_INVALIDO;
    {class} property BAL_PROTOCOLO_EM_USO: Integer read _GetBAL_PROTOCOLO_EM_USO;
    {class} property BAL_PROTOCOLO_INVALIDO: Integer read _GetBAL_PROTOCOLO_INVALIDO;
    {class} property BAL_PROTOCOLO_NAO_SUPORTADO: Integer read _GetBAL_PROTOCOLO_NAO_SUPORTADO;
    {class} property BAL_PROTOCOLO_NAO_SUPORTADO_PELAS_CONFIGS_SERIAIS: Integer read _GetBAL_PROTOCOLO_NAO_SUPORTADO_PELAS_CONFIGS_SERIAIS;
    {class} property BAL_QTD_LEITURAS_INVALIDA: Integer read _GetBAL_QTD_LEITURAS_INVALIDA;
    {class} property BAL_STOPBITS_INVALIDO: Integer read _GetBAL_STOPBITS_INVALIDO;
    {class} property BAL_STOPBITS_NAO_SUPORTADO: Integer read _GetBAL_STOPBITS_NAO_SUPORTADO;
    {class} property BARCODE1D_ALTURA_INVALIDA: Integer read _GetBARCODE1D_ALTURA_INVALIDA;
    {class} property BARCODE1D_CARACTERE_INVALIDO: Integer read _GetBARCODE1D_CARACTERE_INVALIDO;
    {class} property BARCODE1D_CODIGO_INVALIDO: Integer read _GetBARCODE1D_CODIGO_INVALIDO;
    {class} property BARCODE1D_COORDENADA_X_INVALIDA: Integer read _GetBARCODE1D_COORDENADA_X_INVALIDA;
    {class} property BARCODE1D_COORDENADA_Y_INVALIDA: Integer read _GetBARCODE1D_COORDENADA_Y_INVALIDA;
    {class} property BARCODE1D_LARGURA_BARRAS_ESTREITAS_INVALIDA: Integer read _GetBARCODE1D_LARGURA_BARRAS_ESTREITAS_INVALIDA;
    {class} property BARCODE1D_LARGURA_BARRAS_LARGAS_INVALIDA: Integer read _GetBARCODE1D_LARGURA_BARRAS_LARGAS_INVALIDA;
    {class} property BARCODE1D_ROTACAO_INVALIDA: Integer read _GetBARCODE1D_ROTACAO_INVALIDA;
    {class} property BARCODE1D_TAMANHO_INVALIDO: Integer read _GetBARCODE1D_TAMANHO_INVALIDO;
    {class} property BARCODE1D_TIPO_INVALIDO: Integer read _GetBARCODE1D_TIPO_INVALIDO;
    {class} property BASE_CALCULO_INVALIDA: Integer read _GetBASE_CALCULO_INVALIDA;
    {class} property BAUDRATE_INVALIDO: Integer read _GetBAUDRATE_INVALIDO;
    {class} property BLUETOOTH_DESATIVADO: Integer read _GetBLUETOOTH_DESATIVADO;
    {class} property BOX_ALTURA_INVALIDA: Integer read _GetBOX_ALTURA_INVALIDA;
    {class} property BOX_COMPRIMENTO_INVALIDO: Integer read _GetBOX_COMPRIMENTO_INVALIDO;
    {class} property BOX_COORDENADA_X_INVALIDA: Integer read _GetBOX_COORDENADA_X_INVALIDA;
    {class} property BOX_COORDENADA_Y_INVALIDA: Integer read _GetBOX_COORDENADA_Y_INVALIDA;
    {class} property BOX_GROSSURA_BORDAS_HORIZONTAIS_INVALIDA: Integer read _GetBOX_GROSSURA_BORDAS_HORIZONTAIS_INVALIDA;
    {class} property BOX_GROSSURA_BORDAS_VERTICAIS_INVALIDA: Integer read _GetBOX_GROSSURA_BORDAS_VERTICAIS_INVALIDA;
    {class} property CB_ALTURA_INVALIDA: Integer read _GetCB_ALTURA_INVALIDA;
    {class} property CB_AREA_DE_IMPRESSAO_EXCEDIDA: Integer read _GetCB_AREA_DE_IMPRESSAO_EXCEDIDA;
    {class} property CB_DADOS_INVALIDOS: Integer read _GetCB_DADOS_INVALIDOS;
    {class} property CB_HRI_INVALIDO: Integer read _GetCB_HRI_INVALIDO;
    {class} property CB_LARGURA_INVALIDA: Integer read _GetCB_LARGURA_INVALIDA;
    {class} property CB_TIPO_INVALIDO: Integer read _GetCB_TIPO_INVALIDO;
    {class} property CEST_INVALIDO: Integer read _GetCEST_INVALIDO;
    {class} property CFOP_INVALIDO: Integer read _GetCFOP_INVALIDO;
    {class} property CHAMADA_NAO_PERMITIDA: Integer read _GetCHAMADA_NAO_PERMITIDA;
    {class} property CHAVE_CANCELAMENTO_INVALIDA: Integer read _GetCHAVE_CANCELAMENTO_INVALIDA;
    {class} property CNPJ_INVALIDO: Integer read _GetCNPJ_INVALIDO;
    {class} property CODIGO_MEIO_PAGAMENTO_INVALIDO: Integer read _GetCODIGO_MEIO_PAGAMENTO_INVALIDO;
    {class} property CODIGO_MUNICIPIO_FATO_GERADOR_INVALIDO: Integer read _GetCODIGO_MUNICIPIO_FATO_GERADOR_INVALIDO;
    {class} property CODIGO_PRODUTO_INVALIDO: Integer read _GetCODIGO_PRODUTO_INVALIDO;
    {class} property CODIGO_TRIBUTACAO_ISSQN_INVALIDO: Integer read _GetCODIGO_TRIBUTACAO_ISSQN_INVALIDO;
    {class} property COMANDO_INVALIDO: Integer read _GetCOMANDO_INVALIDO;
    {class} property COMPLEMENTO_INVALIDO: Integer read _GetCOMPLEMENTO_INVALIDO;
    {class} property CONEXAO_ATIVA: Integer read _GetCONEXAO_ATIVA;
    {class} property CONEXAO_ATIVA_OUTRO: Integer read _GetCONEXAO_ATIVA_OUTRO;
    {class} property CONEXAO_NEGADA: Integer read _GetCONEXAO_NEGADA;
    {class} property CONTEUDO_CAMPO_INVALIDO: Integer read _GetCONTEUDO_CAMPO_INVALIDO;
    {class} property CPF_INVALIDO: Integer read _GetCPF_INVALIDO;
    {class} property CREDENCIADORA_CARTAO_INVALIDO: Integer read _GetCREDENCIADORA_CARTAO_INVALIDO;
    {class} property CREGTRIBISSQN_INVALIDO: Integer read _GetCREGTRIBISSQN_INVALIDO;
    {class} property CSOSN_INVALIDO: Integer read _GetCSOSN_INVALIDO;
    {class} property CST_INVALIDO: Integer read _GetCST_INVALIDO;
    {class} property DADOS_PDF_INVALIDOS: Integer read _GetDADOS_PDF_INVALIDOS;
    {class} property DADOS_QR_INVALIDOS: Integer read _GetDADOS_QR_INVALIDOS;
    {class} property DATABAREXPANDED_ALPHANUMERIC_CARACTERE_INVALIDO: Integer read _GetDATABAREXPANDED_ALPHANUMERIC_CARACTERE_INVALIDO;
    {class} property DATABAREXPANDED_ALPHANUMERIC_TAMANHO_INVALIDO: Integer read _GetDATABAREXPANDED_ALPHANUMERIC_TAMANHO_INVALIDO;
    {class} property DATABAREXPANDED_ALTURA_INVALIDA: Integer read _GetDATABAREXPANDED_ALTURA_INVALIDA;
    {class} property DATABAREXPANDED_BAR_RATIO_INVALIDO: Integer read _GetDATABAREXPANDED_BAR_RATIO_INVALIDO;
    {class} property DATABAREXPANDED_CODIGO_INVALIDO: Integer read _GetDATABAREXPANDED_CODIGO_INVALIDO;
    {class} property DATABAREXPANDED_COORDENADA_X_INVALIDA: Integer read _GetDATABAREXPANDED_COORDENADA_X_INVALIDA;
    {class} property DATABAREXPANDED_COORDENADA_Y_INVALIDA: Integer read _GetDATABAREXPANDED_COORDENADA_Y_INVALIDA;
    {class} property DATABAREXPANDED_PIXEL_MULTIPLIER_INVALIDO: Integer read _GetDATABAREXPANDED_PIXEL_MULTIPLIER_INVALIDO;
    {class} property DATABAREXPANDED_ROTACAO_INVALIDA: Integer read _GetDATABAREXPANDED_ROTACAO_INVALIDA;
    {class} property DATABAREXPANDED_SEGMENTS_PER_ROW_INVALIDO: Integer read _GetDATABAREXPANDED_SEGMENTS_PER_ROW_INVALIDO;
    {class} property DATABAR_ALTURA_INVALIDA: Integer read _GetDATABAR_ALTURA_INVALIDA;
    {class} property DATABAR_BAR_RATIO_INVALIDO: Integer read _GetDATABAR_BAR_RATIO_INVALIDO;
    {class} property DATABAR_CODIGO_INVALIDO: Integer read _GetDATABAR_CODIGO_INVALIDO;
    {class} property DATABAR_COORDENADA_X_INVALIDA: Integer read _GetDATABAR_COORDENADA_X_INVALIDA;
    {class} property DATABAR_COORDENADA_Y_INVALIDA: Integer read _GetDATABAR_COORDENADA_Y_INVALIDA;
    {class} property DATABAR_NUMERIC_LINEAR_DATA_CARACTERE_INVALIDO: Integer read _GetDATABAR_NUMERIC_LINEAR_DATA_CARACTERE_INVALIDO;
    {class} property DATABAR_NUMERIC_LINEAR_DATA_TAMANHO_INVALIDO: Integer read _GetDATABAR_NUMERIC_LINEAR_DATA_TAMANHO_INVALIDO;
    {class} property DATABAR_PIXEL_MULTIPLIER_INVALIDO: Integer read _GetDATABAR_PIXEL_MULTIPLIER_INVALIDO;
    {class} property DATABAR_ROTACAO_INVALIDA: Integer read _GetDATABAR_ROTACAO_INVALIDA;
    {class} property DATABAR_TIPO_INVALIDO: Integer read _GetDATABAR_TIPO_INVALIDO;
    {class} property DATAMATRIX_CODIGO_INVALIDO: Integer read _GetDATAMATRIX_CODIGO_INVALIDO;
    {class} property DATAMATRIX_COLUMNS_INVALIDAS: Integer read _GetDATAMATRIX_COLUMNS_INVALIDAS;
    {class} property DATAMATRIX_COORDENADA_X_INVALIDA: Integer read _GetDATAMATRIX_COORDENADA_X_INVALIDA;
    {class} property DATAMATRIX_COORDENADA_Y_INVALIDA: Integer read _GetDATAMATRIX_COORDENADA_Y_INVALIDA;
    {class} property DATAMATRIX_MULTIPLICADOR_INVALIDO: Integer read _GetDATAMATRIX_MULTIPLICADOR_INVALIDO;
    {class} property DATAMATRIX_ROTACAO_INVALIDA: Integer read _GetDATAMATRIX_ROTACAO_INVALIDA;
    {class} property DATAMATRIX_ROWS_INVALIDAS: Integer read _GetDATAMATRIX_ROWS_INVALIDAS;
    {class} property DESCRICAO_PRODUTO_INVALIDA: Integer read _GetDESCRICAO_PRODUTO_INVALIDA;
    {class} property DIRECAO_INVALIDA: Integer read _GetDIRECAO_INVALIDA;
    {class} property DISPOSITIVO_NAO_ENCONTRADO: Integer read _GetDISPOSITIVO_NAO_ENCONTRADO;
    {class} property DISPOSITIVO_NAO_EXISTE: Integer read _GetDISPOSITIVO_NAO_EXISTE;
    {class} property DISPOSITIVO_NAO_PAREADO: Integer read _GetDISPOSITIVO_NAO_PAREADO;
    {class} property DISPOSITIVO_NAO_SUPORTA_BT: Integer read _GetDISPOSITIVO_NAO_SUPORTA_BT;
    {class} property ERROR_CORRECTION_LEVEL_INVALIDO: Integer read _GetERROR_CORRECTION_LEVEL_INVALIDO;
    {class} property ERRO_ABERTURA_NAO_AUTORIZADA: Integer read _GetERRO_ABERTURA_NAO_AUTORIZADA;
    {class} property ERRO_ABERTURA_PORTA: Integer read _GetERRO_ABERTURA_PORTA;
    {class} property ERRO_ALTURA_GAP: Integer read _GetERRO_ALTURA_GAP;
    {class} property ERRO_AO_CARREGAR_BIBLIOTECA_DA_ETIQUETA: Integer read _GetERRO_AO_CARREGAR_BIBLIOTECA_DA_ETIQUETA;
    {class} property ERRO_AO_CARREGAR_BIBLIOTECA_DA_IMPRESSORA: Integer read _GetERRO_AO_CARREGAR_BIBLIOTECA_DA_IMPRESSORA;
    {class} property ERRO_AO_CARREGAR_BIBLIOTECA_DO_E1SAT: Integer read _GetERRO_AO_CARREGAR_BIBLIOTECA_DO_E1SAT;
    {class} property ERRO_AO_CARREGAR_BIBLIOTECA_DO_SAT: Integer read _GetERRO_AO_CARREGAR_BIBLIOTECA_DO_SAT;
    {class} property ERRO_BACKFEED: Integer read _GetERRO_BACKFEED;
    {class} property ERRO_BAUDRATE_BAUDRATE: Integer read _GetERRO_BAUDRATE_BAUDRATE;
    {class} property ERRO_BAUDRATE_DATA_LENGTH: Integer read _GetERRO_BAUDRATE_DATA_LENGTH;
    {class} property ERRO_BAUDRATE_PARITY: Integer read _GetERRO_BAUDRATE_PARITY;
    {class} property ERRO_BAUDRATE_STOP_BIT: Integer read _GetERRO_BAUDRATE_STOP_BIT;
    {class} property ERRO_CALOR: Integer read _GetERRO_CALOR;
    {class} property ERRO_CAMPOS_OVERFLOW: Integer read _GetERRO_CAMPOS_OVERFLOW;
    {class} property ERRO_CLAIM_UL: Integer read _GetERRO_CLAIM_UL;
    {class} property ERRO_CONEXAO_BLUETOOTH: Integer read _GetERRO_CONEXAO_BLUETOOTH;
    {class} property ERRO_CORTAR_ZERO: Integer read _GetERRO_CORTAR_ZERO;
    {class} property ERRO_DESCONHECIDO: Integer read _GetERRO_DESCONHECIDO;
    {class} property ERRO_DE_ABERTURA_PORTA_USB: Integer read _GetERRO_DE_ABERTURA_PORTA_USB;
    {class} property ERRO_ENVIA_IMAGEM_ARQUIVO: Integer read _GetERRO_ENVIA_IMAGEM_ARQUIVO;
    {class} property ERRO_ENVIA_IMAGEM_FORMATO: Integer read _GetERRO_ENVIA_IMAGEM_FORMATO;
    {class} property ERRO_ENVIA_IMAGEM_MODULO: Integer read _GetERRO_ENVIA_IMAGEM_MODULO;
    {class} property ERRO_ENVIA_IMAGEM_NOME_CARACTERE: Integer read _GetERRO_ENVIA_IMAGEM_NOME_CARACTERE;
    {class} property ERRO_ENVIA_IMAGEM_NOME_TAMANHO: Integer read _GetERRO_ENVIA_IMAGEM_NOME_TAMANHO;
    {class} property ERRO_ESCRITA: Integer read _GetERRO_ESCRITA;
    {class} property ERRO_ESCRITA_PORTA: Integer read _GetERRO_ESCRITA_PORTA;
    {class} property ERRO_EXCLUI_IMAGEM_MODULO: Integer read _GetERRO_EXCLUI_IMAGEM_MODULO;
    {class} property ERRO_EXCLUI_IMAGEM_NOME_CARACTERE: Integer read _GetERRO_EXCLUI_IMAGEM_NOME_CARACTERE;
    {class} property ERRO_EXCLUI_IMAGEM_NOME_TAMANHO: Integer read _GetERRO_EXCLUI_IMAGEM_NOME_TAMANHO;
    {class} property ERRO_FECHAMENTO_NAO_AUTORIZADO: Integer read _GetERRO_FECHAMENTO_NAO_AUTORIZADO;
    {class} property ERRO_FECHAMENTO_PORTA: Integer read _GetERRO_FECHAMENTO_PORTA;
    {class} property ERRO_FUNCAO_NAO_CHAMADA_PELO_SERVICO: Integer read _GetERRO_FUNCAO_NAO_CHAMADA_PELO_SERVICO;
    {class} property ERRO_FUNCAO_NAO_DISPONIVEL_VIA_SERVICO: Integer read _GetERRO_FUNCAO_NAO_DISPONIVEL_VIA_SERVICO;
    {class} property ERRO_FUNCAO_NAO_SUPORTADA: Integer read _GetERRO_FUNCAO_NAO_SUPORTADA;
    {class} property ERRO_LEITURA_PORTA: Integer read _GetERRO_LEITURA_PORTA;
    {class} property ERRO_LENGTH: Integer read _GetERRO_LENGTH;
    {class} property ERRO_LIMPAR: Integer read _GetERRO_LIMPAR;
    {class} property ERRO_LIMPA_MODULO_MODULO: Integer read _GetERRO_LIMPA_MODULO_MODULO;
    {class} property ERRO_LOGIC_IMG_MODE: Integer read _GetERRO_LOGIC_IMG_MODE;
    {class} property ERRO_MEDIDA: Integer read _GetERRO_MEDIDA;
    {class} property ERRO_MEMORY_STATUS_TIPO_DADOS: Integer read _GetERRO_MEMORY_STATUS_TIPO_DADOS;
    {class} property ERRO_MIRROR: Integer read _GetERRO_MIRROR;
    {class} property ERRO_MODO_CONTINUO: Integer read _GetERRO_MODO_CONTINUO;
    {class} property ERRO_NENHUM_BYTE_ENVIADO: Integer read _GetERRO_NENHUM_BYTE_ENVIADO;
    {class} property ERRO_NOT_ACTIVITY: Integer read _GetERRO_NOT_ACTIVITY;
    {class} property ERRO_OFFSET_COLUNA: Integer read _GetERRO_OFFSET_COLUNA;
    {class} property ERRO_OFFSET_LINHA: Integer read _GetERRO_OFFSET_LINHA;
    {class} property ERRO_PDF_DESCONHECIDO: Integer read _GetERRO_PDF_DESCONHECIDO;
    {class} property ERRO_PRINT_ST_POS: Integer read _GetERRO_PRINT_ST_POS;
    {class} property ERRO_QTDE: Integer read _GetERRO_QTDE;
    {class} property ERRO_SENSOR: Integer read _GetERRO_SENSOR;
    {class} property ERRO_SERIAL_DESCONHECIDO: Integer read _GetERRO_SERIAL_DESCONHECIDO;
    {class} property ERRO_SERVICO_NAO_INICIADO: Integer read _GetERRO_SERVICO_NAO_INICIADO;
    {class} property ERRO_SYMBOL: Integer read _GetERRO_SYMBOL;
    {class} property ERRO_TAM_PIXEL: Integer read _GetERRO_TAM_PIXEL;
    {class} property ERRO_TIPO_TRANSFERENCIA: Integer read _GetERRO_TIPO_TRANSFERENCIA;
    {class} property ERRO_VEL_IMPRESSAO: Integer read _GetERRO_VEL_IMPRESSAO;
    {class} property ERRO_XSD: Integer read _GetERRO_XSD;
    {class} property ESTE_JSON_NAO_E_UM_OBJETO: Integer read _GetESTE_JSON_NAO_E_UM_OBJETO;
    {class} property GRUPO_INVALIDO: Integer read _GetGRUPO_INVALIDO;
    {class} property GTIN_INVALIDO: Integer read _GetGTIN_INVALIDO;
    {class} property HEIGHT_INVALIDO: Integer read _GetHEIGHT_INVALIDO;
    {class} property IDENTIFICADOR_CAMPO_INVALIDO: Integer read _GetIDENTIFICADOR_CAMPO_INVALIDO;
    {class} property IE_INVALIDO: Integer read _GetIE_INVALIDO;
    {class} property IMAGEM_COORDENADA_X_INVALIDA: Integer read _GetIMAGEM_COORDENADA_X_INVALIDA;
    {class} property IMAGEM_COORDENADA_Y_INVALIDA: Integer read _GetIMAGEM_COORDENADA_Y_INVALIDA;
    {class} property IMAGEM_NOME_CARACTERE_INVALIDO: Integer read _GetIMAGEM_NOME_CARACTERE_INVALIDO;
    {class} property IMAGEM_NOME_TAMANHO_INVALIDO: Integer read _GetIMAGEM_NOME_TAMANHO_INVALIDO;
    {class} property IM_INVALIDO: Integer read _GetIM_INVALIDO;
    {class} property INDICADOR_INCETIVO_FISCAL_ISSQN_INVALIDO: Integer read _GetINDICADOR_INCETIVO_FISCAL_ISSQN_INVALIDO;
    {class} property INDRATISSQN_INVALIDO: Integer read _GetINDRATISSQN_INVALIDO;
    {class} property INFORMACOES_ADICIONAIS_PRODUTO_INVALIDA: Integer read _GetINFORMACOES_ADICIONAIS_PRODUTO_INVALIDA;
    {class} property ITEM_LISTA_SERVICO_INVALIDO: Integer read _GetITEM_LISTA_SERVICO_INVALIDO;
    {class} property KEY_INVALIDO: Integer read _GetKEY_INVALIDO;
    {class} property LINHA_ALTURA_INVALIDA: Integer read _GetLINHA_ALTURA_INVALIDA;
    {class} property LINHA_COMPRIMENTO_INVALIDO: Integer read _GetLINHA_COMPRIMENTO_INVALIDO;
    {class} property LINHA_COORDENADA_X_INVALIDA: Integer read _GetLINHA_COORDENADA_X_INVALIDA;
    {class} property LINHA_COORDENADA_Y_INVALIDA: Integer read _GetLINHA_COORDENADA_Y_INVALIDA;
    {class} property LOGRADOURO_INVALIDO: Integer read _GetLOGRADOURO_INVALIDO;
    {class} property MAC_ADDRESS_INVALIDO: Integer read _GetMAC_ADDRESS_INVALIDO;
    {class} property MAXICODE_CLASS_OF_SERVICE_INVALIDA: Integer read _GetMAXICODE_CLASS_OF_SERVICE_INVALIDA;
    {class} property MAXICODE_CODIGO_INVALIDO: Integer read _GetMAXICODE_CODIGO_INVALIDO;
    {class} property MAXICODE_COORDENADA_X_INVALIDA: Integer read _GetMAXICODE_COORDENADA_X_INVALIDA;
    {class} property MAXICODE_COORDENADA_Y_INVALIDA: Integer read _GetMAXICODE_COORDENADA_Y_INVALIDA;
    {class} property MAXICODE_COUNTRY_INVALIDO: Integer read _GetMAXICODE_COUNTRY_INVALIDO;
    {class} property MAXICODE_PRIMARY_ZIP_INVALIDO: Integer read _GetMAXICODE_PRIMARY_ZIP_INVALIDO;
    {class} property MAXICODE_ROTACAO_INVALIDA: Integer read _GetMAXICODE_ROTACAO_INVALIDA;
    {class} property MAXICODE_SECONDARY_ZIP_INVALIDO: Integer read _GetMAXICODE_SECONDARY_ZIP_INVALIDO;
    {class} property MODELO_INVALIDO: Integer read _GetMODELO_INVALIDO;
    {class} property MODULO_FUNCAO_NAO_ENCONTRADO: Integer read _GetMODULO_FUNCAO_NAO_ENCONTRADO;
    {class} property MUNICIPIO_INVALIDO: Integer read _GetMUNICIPIO_INVALIDO;
    {class} property NAO_FOI_POSSIVEL_INICIAR_O_SERVICO: Integer read _GetNAO_FOI_POSSIVEL_INICIAR_O_SERVICO;
    {class} property NATUREZA_OPERACAO_INVALIDA: Integer read _GetNATUREZA_OPERACAO_INVALIDA;
    {class} property NCM_INVALIDO: Integer read _GetNCM_INVALIDO;
    {class} property NENHUM_DADO_RETORNADO: Integer read _GetNENHUM_DADO_RETORNADO;
    {class} property NIVEL_DE_CORRECAO_INVALIDO: Integer read _GetNIVEL_DE_CORRECAO_INVALIDO;
    {class} property NOME_DESTINARIO_INVALIDO: Integer read _GetNOME_DESTINARIO_INVALIDO;
    {class} property NUMBER_COLUMNS_INVALIDO: Integer read _GetNUMBER_COLUMNS_INVALIDO;
    {class} property NUMBER_ROWS_INVALIDO: Integer read _GetNUMBER_ROWS_INVALIDO;
    {class} property NUMERO_CAIXA_INVALIDO: Integer read _GetNUMERO_CAIXA_INVALIDO;
    {class} property NUMERO_INVALIDO: Integer read _GetNUMERO_INVALIDO;
    {class} property NUMERO_ITEM_INVALIDO: Integer read _GetNUMERO_ITEM_INVALIDO;
    {class} property OPERACAO_INVALIDA: Integer read _GetOPERACAO_INVALIDA;
    {class} property OPTIONS_INVALIDO: Integer read _GetOPTIONS_INVALIDO;
    {class} property ORIGEM_MERCADORIA_INVALIDA: Integer read _GetORIGEM_MERCADORIA_INVALIDA;
    {class} property PARAMETRO_CONF_INVALIDO: Integer read _GetPARAMETRO_CONF_INVALIDO;
    {class} property PARAMETRO_NAO_ENCONTRADO: Integer read _GetPARAMETRO_NAO_ENCONTRADO;
    {class} property PARAMETRO_TIPO_STATUS_INVALIDO: Integer read _GetPARAMETRO_TIPO_STATUS_INVALIDO;
    {class} property PDF417_CODIGO_INVALIDO: Integer read _GetPDF417_CODIGO_INVALIDO;
    {class} property PDF417_COLUMN_NUMBER_INVALIDO: Integer read _GetPDF417_COLUMN_NUMBER_INVALIDO;
    {class} property PDF417_COORDENADA_X_INVALIDA: Integer read _GetPDF417_COORDENADA_X_INVALIDA;
    {class} property PDF417_COORDENADA_Y_INVALIDA: Integer read _GetPDF417_COORDENADA_Y_INVALIDA;
    {class} property PDF417_MULTIPLICADOR_HORIZONTAL_INVALIDO: Integer read _GetPDF417_MULTIPLICADOR_HORIZONTAL_INVALIDO;
    {class} property PDF417_MULTIPLICADOR_VERTICAL_INVALIDO: Integer read _GetPDF417_MULTIPLICADOR_VERTICAL_INVALIDO;
    {class} property PDF417_ROTACAO_INVALIDA: Integer read _GetPDF417_ROTACAO_INVALIDA;
    {class} property PDF417_ROW_NUMBER_INVALIDO: Integer read _GetPDF417_ROW_NUMBER_INVALIDO;
    {class} property PDF417_SECURITY_LEVEL_INVALIDO: Integer read _GetPDF417_SECURITY_LEVEL_INVALIDO;
    {class} property PDF417_TIPO_INVALIDO: Integer read _GetPDF417_TIPO_INVALIDO;
    {class} property PDF_417_ASPECT_RATIO_INVALIDO: Integer read _GetPDF_417_ASPECT_RATIO_INVALIDO;
    {class} property PERCENTUAL_ALIQUOTA_COFINS_INVALIDA: Integer read _GetPERCENTUAL_ALIQUOTA_COFINS_INVALIDA;
    {class} property PERCENTUAL_ALIQUOTA_PIS_INVALIDA: Integer read _GetPERCENTUAL_ALIQUOTA_PIS_INVALIDA;
    {class} property PERMISSAO_NEGADA: Integer read _GetPERMISSAO_NEGADA;
    {class} property PINO_INVALIDO: Integer read _GetPINO_INVALIDO;
    {class} property PORTA_FECHADA: Integer read _GetPORTA_FECHADA;
    {class} property PORTA_TCP_INVALIDA: Integer read _GetPORTA_TCP_INVALIDA;
    {class} property POSICAO_INVALIDA: Integer read _GetPOSICAO_INVALIDA;
    {class} property POS_IMP_HORIZONTAL_INVALIDA: Integer read _GetPOS_IMP_HORIZONTAL_INVALIDA;
    {class} property PRINTTEXT_ATRIBUTO_INVALIDO: Integer read _GetPRINTTEXT_ATRIBUTO_INVALIDO;
    {class} property QRCODEAUTO_CODIGO_INVALIDO: Integer read _GetQRCODEAUTO_CODIGO_INVALIDO;
    {class} property QRCODEAUTO_COORDENADA_X_INVALIDA: Integer read _GetQRCODEAUTO_COORDENADA_X_INVALIDA;
    {class} property QRCODEAUTO_COORDENADA_Y_INVALIDA: Integer read _GetQRCODEAUTO_COORDENADA_Y_INVALIDA;
    {class} property QRCODEAUTO_MULTIPLICADOR_INVALIDO: Integer read _GetQRCODEAUTO_MULTIPLICADOR_INVALIDO;
    {class} property QRCODEAUTO_ROTACAO_INVALIDA: Integer read _GetQRCODEAUTO_ROTACAO_INVALIDA;
    {class} property QRCODEMANUAL_CODIGO_INVALIDO: Integer read _GetQRCODEMANUAL_CODIGO_INVALIDO;
    {class} property QRCODEMANUAL_COORDENADA_X_INVALIDA: Integer read _GetQRCODEMANUAL_COORDENADA_X_INVALIDA;
    {class} property QRCODEMANUAL_COORDENADA_Y_INVALIDA: Integer read _GetQRCODEMANUAL_COORDENADA_Y_INVALIDA;
    {class} property QRCODEMANUAL_INPUT_CONFIG_INVALIDO: Integer read _GetQRCODEMANUAL_INPUT_CONFIG_INVALIDO;
    {class} property QRCODEMANUAL_INPUT_INVALIDO: Integer read _GetQRCODEMANUAL_INPUT_INVALIDO;
    {class} property QRCODEMANUAL_MULTIPLICADOR_INVALIDO: Integer read _GetQRCODEMANUAL_MULTIPLICADOR_INVALIDO;
    {class} property QRCODEMANUAL_NUM_CHARS_8BIT_INVALIDO: Integer read _GetQRCODEMANUAL_NUM_CHARS_8BIT_INVALIDO;
    {class} property QRCODEMANUAL_NUM_MASCARA_INVALIDO: Integer read _GetQRCODEMANUAL_NUM_MASCARA_INVALIDO;
    {class} property QRCODEMANUAL_NUM_MODELO_INVALIDO: Integer read _GetQRCODEMANUAL_NUM_MODELO_INVALIDO;
    {class} property QRCODEMANUAL_NVL_COR_ERRO_INVALIDO: Integer read _GetQRCODEMANUAL_NVL_COR_ERRO_INVALIDO;
    {class} property QRCODEMANUAL_ROTACAO_INVALIDA: Integer read _GetQRCODEMANUAL_ROTACAO_INVALIDA;
    {class} property QUANTIDADE_COMERCIAL_INVALIDA: Integer read _GetQUANTIDADE_COMERCIAL_INVALIDA;
    {class} property QUANTIDADE_ELEMENTO_EXCEDIDA: Integer read _GetQUANTIDADE_ELEMENTO_EXCEDIDA;
    {class} property QUANTIDADE_FORA_INTERVALO: Integer read _GetQUANTIDADE_FORA_INTERVALO;
    {class} property QUANTIDADE_INVALIDA: Integer read _GetQUANTIDADE_INVALIDA;
    {class} property QUANTIDADE_MEIO_PAGAMENTO_EXCEDIDA: Integer read _GetQUANTIDADE_MEIO_PAGAMENTO_EXCEDIDA;
    {class} property QUANTIDADE_VENDIDA_COFINS_INVALIDA: Integer read _GetQUANTIDADE_VENDIDA_COFINS_INVALIDA;
    {class} property QUANTIDADE_VENDIDA_PIS_INVALIDA: Integer read _GetQUANTIDADE_VENDIDA_PIS_INVALIDA;
    {class} property RECONEXOES_ESGOTADAS: Integer read _GetRECONEXOES_ESGOTADAS;
    {class} property REGRA_CALCULO_INVALIDA: Integer read _GetREGRA_CALCULO_INVALIDA;
    {class} property SCALA_INVALIDA: Integer read _GetSCALA_INVALIDA;
    {class} property SERVICO_OCUPADO: Integer read _GetSERVICO_OCUPADO;
    {class} property SIGNAC_INVALIDA: Integer read _GetSIGNAC_INVALIDA;
    {class} property STATEPRINTER_QSESEMPAPEL: Integer read _GetSTATEPRINTER_QSESEMPAPEL;
    {class} property STATEPRINTER_QSESEMPAPELETAMPA: Integer read _GetSTATEPRINTER_QSESEMPAPELETAMPA;
    {class} property STATEPRINTER_SEMPAPEL: Integer read _GetSTATEPRINTER_SEMPAPEL;
    {class} property STATEPRINTER_SUCESSO: Integer read _GetSTATEPRINTER_SUCESSO;
    {class} property STATEPRINTER_TAMPAABERTA: Integer read _GetSTATEPRINTER_TAMPAABERTA;
    {class} property STATEPRINTER_TAMPAEPAPEL: Integer read _GetSTATEPRINTER_TAMPAEPAPEL;
    {class} property STATUS_NAO_SUPORTADO: Integer read _GetSTATUS_NAO_SUPORTADO;
    {class} property STILO_INVALIDO: Integer read _GetSTILO_INVALIDO;
    {class} property SUCESSO: Integer read _GetSUCESSO;
    {class} property TAMANHO_INFORMACOES_COMPLEMENTARES_INVALIDO: Integer read _GetTAMANHO_INFORMACOES_COMPLEMENTARES_INVALIDO;
    {class} property TAMANHO_INVALIDO: Integer read _GetTAMANHO_INVALIDO;
    {class} property TAMANHO_QR_INVALIDO: Integer read _GetTAMANHO_QR_INVALIDO;
    {class} property TEMPO_FORA_LIMITE: Integer read _GetTEMPO_FORA_LIMITE;
    {class} property TEMPO_INVALIDO: Integer read _GetTEMPO_INVALIDO;
    {class} property TEXTOASD_COORDENADA_X_INVALIDA: Integer read _GetTEXTOASD_COORDENADA_X_INVALIDA;
    {class} property TEXTOASD_COORDENADA_Y_INVALIDA: Integer read _GetTEXTOASD_COORDENADA_Y_INVALIDA;
    {class} property TEXTOASD_MULTIPLICADOR_HORIZONTAL_INVALIDO: Integer read _GetTEXTOASD_MULTIPLICADOR_HORIZONTAL_INVALIDO;
    {class} property TEXTOASD_MULTIPLICADOR_VERTICAL_INVALIDO: Integer read _GetTEXTOASD_MULTIPLICADOR_VERTICAL_INVALIDO;
    {class} property TEXTOASD_ROTACAO_INVALIDA: Integer read _GetTEXTOASD_ROTACAO_INVALIDA;
    {class} property TEXTOASD_TAMANHO_INVALIDO: Integer read _GetTEXTOASD_TAMANHO_INVALIDO;
    {class} property TEXTOASD_TEXTO_INVALIDO: Integer read _GetTEXTOASD_TEXTO_INVALIDO;
    {class} property TEXTOCOURIER_COORDENADA_X_INVALIDA: Integer read _GetTEXTOCOURIER_COORDENADA_X_INVALIDA;
    {class} property TEXTOCOURIER_COORDENADA_Y_INVALIDA: Integer read _GetTEXTOCOURIER_COORDENADA_Y_INVALIDA;
    {class} property TEXTOCOURIER_MULTIPLICADOR_HORIZONTAL_INVALIDO: Integer read _GetTEXTOCOURIER_MULTIPLICADOR_HORIZONTAL_INVALIDO;
    {class} property TEXTOCOURIER_MULTIPLICADOR_VERTICAL_INVALIDO: Integer read _GetTEXTOCOURIER_MULTIPLICADOR_VERTICAL_INVALIDO;
    {class} property TEXTOCOURIER_ROTACAO_INVALIDA: Integer read _GetTEXTOCOURIER_ROTACAO_INVALIDA;
    {class} property TEXTOCOURIER_SYMBOL_INVALIDO: Integer read _GetTEXTOCOURIER_SYMBOL_INVALIDO;
    {class} property TEXTOCOURIER_TEXTO_INVALIDO: Integer read _GetTEXTOCOURIER_TEXTO_INVALIDO;
    {class} property TEXTO_COORDENADA_X_INVALIDA: Integer read _GetTEXTO_COORDENADA_X_INVALIDA;
    {class} property TEXTO_COORDENADA_Y_INVALIDA: Integer read _GetTEXTO_COORDENADA_Y_INVALIDA;
    {class} property TEXTO_FONTE_INVALIDA: Integer read _GetTEXTO_FONTE_INVALIDA;
    {class} property TEXTO_MULTIPLICADOR_HORIZONTAL_INVALIDO: Integer read _GetTEXTO_MULTIPLICADOR_HORIZONTAL_INVALIDO;
    {class} property TEXTO_MULTIPLICADOR_VERTICAL_INVALIDO: Integer read _GetTEXTO_MULTIPLICADOR_VERTICAL_INVALIDO;
    {class} property TEXTO_ROTACAO_INVALIDA: Integer read _GetTEXTO_ROTACAO_INVALIDA;
    {class} property TEXTO_TEXTO_INVALIDO: Integer read _GetTEXTO_TEXTO_INVALIDO;
    {class} property THREAD_EXECUTION_EXCEPTION: Integer read _GetTHREAD_EXECUTION_EXCEPTION;
    {class} property THREAD_INTERRUPTED_EXCEPTION: Integer read _GetTHREAD_INTERRUPTED_EXCEPTION;
    {class} property TIPO_EMISSAO_INDEFINIDA: Integer read _GetTIPO_EMISSAO_INDEFINIDA;
    {class} property TIPO_INVALIDO: Integer read _GetTIPO_INVALIDO;
    {class} property TIPO_PARAMETRO_INVALIDO: Integer read _GetTIPO_PARAMETRO_INVALIDO;
    {class} property UF_INVALIDO: Integer read _GetUF_INVALIDO;
    {class} property UNIDADE_COMERCIAL_INVALIDA: Integer read _GetUNIDADE_COMERCIAL_INVALIDA;
    {class} property VALOR_ACRESCIMO_INVALIDO: Integer read _GetVALOR_ACRESCIMO_INVALIDO;
    {class} property VALOR_ACRESCIMO_SUBTOTAL_INVALIDO: Integer read _GetVALOR_ACRESCIMO_SUBTOTAL_INVALIDO;
    {class} property VALOR_ALIQUOTA_COFINS_INVALIDA: Integer read _GetVALOR_ALIQUOTA_COFINS_INVALIDA;
    {class} property VALOR_ALIQUOTA_PIS_INVALIDA: Integer read _GetVALOR_ALIQUOTA_PIS_INVALIDA;
    {class} property VALOR_DEDUCOES_ISSQN_INVALIDA: Integer read _GetVALOR_DEDUCOES_ISSQN_INVALIDA;
    {class} property VALOR_DESCONTO_INVALIDO: Integer read _GetVALOR_DESCONTO_INVALIDO;
    {class} property VALOR_DESCONTO_SUBTOTAL_INVALIDO: Integer read _GetVALOR_DESCONTO_SUBTOTAL_INVALIDO;
    {class} property VALOR_MEIO_PAGAMENTO_INVALIDO: Integer read _GetVALOR_MEIO_PAGAMENTO_INVALIDO;
    {class} property VALOR_UNITARIO_INVALIDO: Integer read _GetVALOR_UNITARIO_INVALIDO;
    {class} property VALOR_VCFELEI12741_INVALIDO: Integer read _GetVALOR_VCFELEI12741_INVALIDO;
    {class} property VERSAO_DADOS_ENT_INVALIDO: Integer read _GetVERSAO_DADOS_ENT_INVALIDO;
    {class} property VERSAO_XMLNFCE_INDEFINIDA: Integer read _GetVERSAO_XMLNFCE_INDEFINIDA;
    {class} property VERSAO_XMLNFCE_NAO_SUPORTADA: Integer read _GetVERSAO_XMLNFCE_NAO_SUPORTADA;
    {class} property VITEM12741_INVALIDO: Integer read _GetVITEM12741_INVALIDO;
    {class} property WIDTH_INVALIDO: Integer read _GetWIDTH_INVALIDO;
    {class} property XSD_NAO_ENCONTRADO: Integer read _GetXSD_NAO_ENCONTRADO;
    {class} property __ERRO_AO_CARREGAR_DLL_IMPRESSORA: JString read _Get__ERRO_AO_CARREGAR_DLL_IMPRESSORA;
    {class} property __ERRO_AO_CARREGAR_DLL_SAT: JString read _Get__ERRO_AO_CARREGAR_DLL_SAT;
    {class} property __ERRO_AO_VALIDAR_DLL_SAL: JString read _Get__ERRO_AO_VALIDAR_DLL_SAL;
    {class} property __ERRO_CANCELAR_VENDA_SAT: JString read _Get__ERRO_CANCELAR_VENDA_SAT;
    {class} property __SUCESSO: JString read _Get__SUCESSO;
  end;

  [JavaSignature('com/elgin/e1/Impressora/Utilidades/CodigoErro')]
  JCodigoErro = interface(JObject)
    ['{DFC34281-DFEA-4A0D-BC39-7F4C45D1331A}']
  end;
  TJCodigoErro = class(TJavaGenericImport<JCodigoErroClass, JCodigoErro>) end;

  JESCPOSClass = interface(JObjectClass)
    ['{FC3A5308-D641-4363-AF41-60A4531D1852}']
    {class} function _GetABRE_GAVETA: TJavaArray<Byte>; cdecl;
    {class} function _GetABRE_GAVETA_ELGIN: TJavaArray<Byte>; cdecl;
    {class} function _GetALTURA_CODIGO_BARRAS: TJavaArray<Byte>; cdecl;
    {class} function _GetAVANCA_PAPEL: TJavaArray<Byte>; cdecl;
    {class} function _GetCANC_DATA_MODO_PAGINA: TJavaArray<Byte>; cdecl;
    {class} function _GetCANC_MINIFONT: TJavaArray<Byte>; cdecl;
    {class} function _GetCANC_NEGRITO: TJavaArray<Byte>; cdecl;
    {class} function _GetCANC_SUBLINHADO: TJavaArray<Byte>; cdecl;
    {class} function _GetCODE_PAGE: TJavaArray<Byte>; cdecl;
    {class} function _GetCORTE_PARCIAL: TJavaArray<Byte>; cdecl;
    {class} function _GetCORTE_TOTAL: TJavaArray<Byte>; cdecl;
    {class} function _GetDEF_AREA_IMPRESSAO: TJavaArray<Byte>; cdecl;
    {class} function _GetDEF_POS_HORIZONTAL_MODO_PAGINA: TJavaArray<Byte>; cdecl;
    {class} function _GetDEF_POS_VERTICAL_MODO_PAGINA: TJavaArray<Byte>; cdecl;
    {class} function _GetDESABILITA_INVERSO: TJavaArray<Byte>; cdecl;
    {class} function _GetDES_STATUSASB: TJavaArray<Byte>; cdecl;
    {class} function _GetDES_STATUSASB_BKT: TJavaArray<Byte>; cdecl;
    {class} function _GetERROR_CORRECTION_LEVEL: TJavaArray<Byte>; cdecl;
    {class} function _GetESPACAMENTO_ENTRE_LINHA: TJavaArray<Byte>; cdecl;
    {class} function _GetHEIGHT: TJavaArray<Byte>; cdecl;
    {class} function _GetHRI_CODIGO_BARRAS: TJavaArray<Byte>; cdecl;
    {class} function _GetHRI_CODIGO_BARRAS_DARUMA: TJavaArray<Byte>; cdecl;
    {class} function _GetIMPRIME_INFO_MODO_PAGINA: TJavaArray<Byte>; cdecl;
    {class} function _GetIMPRIME_INFO_MODO_PAGINA_RET_MODO_PADRAO: TJavaArray<Byte>; cdecl;
    {class} function _GetIMPRIME_QRCODE: TJavaArray<Byte>; cdecl;
    {class} function _GetIMPRIME_QRCODE_BEMA: TJavaArray<Byte>; cdecl;
    {class} function _GetIMPRIME_QRCODE_BKT: TJavaArray<Byte>; cdecl;
    {class} function _GetINICIALIZAR: TJavaArray<Byte>; cdecl;
    {class} function _GetImprimeImage: TJavaArray<Byte>; cdecl;
    {class} function _GetJUSTIFICATIVA_TEXTO: TJavaArray<Byte>; cdecl;
    {class} function _GetLARGURA_CODIGO_BARRAS: TJavaArray<Byte>; cdecl;
    {class} function _GetLIMPA_BUFFER_MODO_PAGINA: TJavaArray<Byte>; cdecl;
    {class} function _GetMINIFONT: TJavaArray<Byte>; cdecl;
    {class} function _GetMODO_INVERSO: TJavaArray<Byte>; cdecl;
    {class} function _GetMODO_PADRAO: TJavaArray<Byte>; cdecl;
    {class} function _GetNEGRITO: TJavaArray<Byte>; cdecl;
    {class} function _GetNIVEL_CORRECAO: TJavaArray<Byte>; cdecl;
    {class} function _GetNUMBER_COLUMNS: TJavaArray<Byte>; cdecl;
    {class} function _GetNUMBER_ROWS: TJavaArray<Byte>; cdecl;
    {class} function _GetOPTIONS: TJavaArray<Byte>; cdecl;
    {class} function _GetPOS_IMP_HORIZONTAL: TJavaArray<Byte>; cdecl;
    {class} function _GetPRINT_PDF417: TJavaArray<Byte>; cdecl;
    {class} function _GetSEL_DIRECAO_MODO_PAGINA_BC_IE: TJavaArray<Byte>; cdecl;
    {class} function _GetSEL_DIRECAO_MODO_PAGINA_CB_SD: TJavaArray<Byte>; cdecl;
    {class} function _GetSEL_DIRECAO_MODO_PAGINA_DE_ID: TJavaArray<Byte>; cdecl;
    {class} function _GetSEL_DIRECAO_MODO_PAGINA_ED_SE: TJavaArray<Byte>; cdecl;
    {class} function _GetSEL_MODO_PADRAO: TJavaArray<Byte>; cdecl;
    {class} function _GetSEL_MODO_PAGINA: TJavaArray<Byte>; cdecl;
    {class} function _GetSEL_MODO_PAGINA_BKT: TJavaArray<Byte>; cdecl;
    {class} function _GetSINAL_SONORO: TJavaArray<Byte>; cdecl;
    {class} function _GetSTATUS: TJavaArray<Byte>; cdecl;
    {class} function _GetSUBLINHADO: TJavaArray<Byte>; cdecl;
    {class} function _GetTAMANHO_QRCODE: TJavaArray<Byte>; cdecl;
    {class} function _GetTAMANHO_QRCODE_BKT: TJavaArray<Byte>; cdecl;
    {class} function _GetTAMANHO_TEXTO: TJavaArray<Byte>; cdecl;
    {class} function _GetWIDTH: TJavaArray<Byte>; cdecl;
    {class} function init: JESCPOS; cdecl;
    {class} property ABRE_GAVETA: TJavaArray<Byte> read _GetABRE_GAVETA;
    {class} property ABRE_GAVETA_ELGIN: TJavaArray<Byte> read _GetABRE_GAVETA_ELGIN;
    {class} property ALTURA_CODIGO_BARRAS: TJavaArray<Byte> read _GetALTURA_CODIGO_BARRAS;
    {class} property AVANCA_PAPEL: TJavaArray<Byte> read _GetAVANCA_PAPEL;
    {class} property CANC_DATA_MODO_PAGINA: TJavaArray<Byte> read _GetCANC_DATA_MODO_PAGINA;
    {class} property CANC_MINIFONT: TJavaArray<Byte> read _GetCANC_MINIFONT;
    {class} property CANC_NEGRITO: TJavaArray<Byte> read _GetCANC_NEGRITO;
    {class} property CANC_SUBLINHADO: TJavaArray<Byte> read _GetCANC_SUBLINHADO;
    {class} property CODE_PAGE: TJavaArray<Byte> read _GetCODE_PAGE;
    {class} property CORTE_PARCIAL: TJavaArray<Byte> read _GetCORTE_PARCIAL;
    {class} property CORTE_TOTAL: TJavaArray<Byte> read _GetCORTE_TOTAL;
    {class} property DEF_AREA_IMPRESSAO: TJavaArray<Byte> read _GetDEF_AREA_IMPRESSAO;
    {class} property DEF_POS_HORIZONTAL_MODO_PAGINA: TJavaArray<Byte> read _GetDEF_POS_HORIZONTAL_MODO_PAGINA;
    {class} property DEF_POS_VERTICAL_MODO_PAGINA: TJavaArray<Byte> read _GetDEF_POS_VERTICAL_MODO_PAGINA;
    {class} property DESABILITA_INVERSO: TJavaArray<Byte> read _GetDESABILITA_INVERSO;
    {class} property DES_STATUSASB: TJavaArray<Byte> read _GetDES_STATUSASB;
    {class} property DES_STATUSASB_BKT: TJavaArray<Byte> read _GetDES_STATUSASB_BKT;
    {class} property ERROR_CORRECTION_LEVEL: TJavaArray<Byte> read _GetERROR_CORRECTION_LEVEL;
    {class} property ESPACAMENTO_ENTRE_LINHA: TJavaArray<Byte> read _GetESPACAMENTO_ENTRE_LINHA;
    {class} property HEIGHT: TJavaArray<Byte> read _GetHEIGHT;
    {class} property HRI_CODIGO_BARRAS: TJavaArray<Byte> read _GetHRI_CODIGO_BARRAS;
    {class} property HRI_CODIGO_BARRAS_DARUMA: TJavaArray<Byte> read _GetHRI_CODIGO_BARRAS_DARUMA;
    {class} property IMPRIME_INFO_MODO_PAGINA: TJavaArray<Byte> read _GetIMPRIME_INFO_MODO_PAGINA;
    {class} property IMPRIME_INFO_MODO_PAGINA_RET_MODO_PADRAO: TJavaArray<Byte> read _GetIMPRIME_INFO_MODO_PAGINA_RET_MODO_PADRAO;
    {class} property IMPRIME_QRCODE: TJavaArray<Byte> read _GetIMPRIME_QRCODE;
    {class} property IMPRIME_QRCODE_BEMA: TJavaArray<Byte> read _GetIMPRIME_QRCODE_BEMA;
    {class} property IMPRIME_QRCODE_BKT: TJavaArray<Byte> read _GetIMPRIME_QRCODE_BKT;
    {class} property INICIALIZAR: TJavaArray<Byte> read _GetINICIALIZAR;
    {class} property ImprimeImage: TJavaArray<Byte> read _GetImprimeImage;
    {class} property JUSTIFICATIVA_TEXTO: TJavaArray<Byte> read _GetJUSTIFICATIVA_TEXTO;
    {class} property LARGURA_CODIGO_BARRAS: TJavaArray<Byte> read _GetLARGURA_CODIGO_BARRAS;
    {class} property LIMPA_BUFFER_MODO_PAGINA: TJavaArray<Byte> read _GetLIMPA_BUFFER_MODO_PAGINA;
    {class} property MINIFONT: TJavaArray<Byte> read _GetMINIFONT;
    {class} property MODO_INVERSO: TJavaArray<Byte> read _GetMODO_INVERSO;
    {class} property MODO_PADRAO: TJavaArray<Byte> read _GetMODO_PADRAO;
    {class} property NEGRITO: TJavaArray<Byte> read _GetNEGRITO;
    {class} property NIVEL_CORRECAO: TJavaArray<Byte> read _GetNIVEL_CORRECAO;
    {class} property NUMBER_COLUMNS: TJavaArray<Byte> read _GetNUMBER_COLUMNS;
    {class} property NUMBER_ROWS: TJavaArray<Byte> read _GetNUMBER_ROWS;
    {class} property OPTIONS: TJavaArray<Byte> read _GetOPTIONS;
    {class} property POS_IMP_HORIZONTAL: TJavaArray<Byte> read _GetPOS_IMP_HORIZONTAL;
    {class} property PRINT_PDF417: TJavaArray<Byte> read _GetPRINT_PDF417;
    {class} property SEL_DIRECAO_MODO_PAGINA_BC_IE: TJavaArray<Byte> read _GetSEL_DIRECAO_MODO_PAGINA_BC_IE;
    {class} property SEL_DIRECAO_MODO_PAGINA_CB_SD: TJavaArray<Byte> read _GetSEL_DIRECAO_MODO_PAGINA_CB_SD;
    {class} property SEL_DIRECAO_MODO_PAGINA_DE_ID: TJavaArray<Byte> read _GetSEL_DIRECAO_MODO_PAGINA_DE_ID;
    {class} property SEL_DIRECAO_MODO_PAGINA_ED_SE: TJavaArray<Byte> read _GetSEL_DIRECAO_MODO_PAGINA_ED_SE;
    {class} property SEL_MODO_PADRAO: TJavaArray<Byte> read _GetSEL_MODO_PADRAO;
    {class} property SEL_MODO_PAGINA: TJavaArray<Byte> read _GetSEL_MODO_PAGINA;
    {class} property SEL_MODO_PAGINA_BKT: TJavaArray<Byte> read _GetSEL_MODO_PAGINA_BKT;
    {class} property SINAL_SONORO: TJavaArray<Byte> read _GetSINAL_SONORO;
    {class} property STATUS: TJavaArray<Byte> read _GetSTATUS;
    {class} property SUBLINHADO: TJavaArray<Byte> read _GetSUBLINHADO;
    {class} property TAMANHO_QRCODE: TJavaArray<Byte> read _GetTAMANHO_QRCODE;
    {class} property TAMANHO_QRCODE_BKT: TJavaArray<Byte> read _GetTAMANHO_QRCODE_BKT;
    {class} property TAMANHO_TEXTO: TJavaArray<Byte> read _GetTAMANHO_TEXTO;
    {class} property WIDTH: TJavaArray<Byte> read _GetWIDTH;
  end;

  [JavaSignature('com/elgin/e1/Impressora/Utilidades/ESCPOS')]
  JESCPOS = interface(JObject)
    ['{CBF7575C-038D-45EE-B313-5E536A7829C6}']
  end;
  TJESCPOS = class(TJavaGenericImport<JESCPOSClass, JESCPOS>) end;

  JInteiroClass = interface(JObjectClass)
    ['{F02CB174-84B9-4A19-B0F0-3994834C8469}']
    {class} function getValor: Integer; cdecl;
    {class} function init: JInteiro; cdecl; overload;
    {class} function init(P1: Integer): JInteiro; cdecl; overload;
    {class} procedure setValor(P1: Integer); cdecl;
    {class} function toString: JString; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Impressora/Utilidades/Inteiro')]
  JInteiro = interface(JObject)
    ['{009ACFFA-E199-49F3-97F1-3CE693941230}']
  end;
  TJInteiro = class(TJavaGenericImport<JInteiroClass, JInteiro>) end;

  JPPLAClass = interface(JObjectClass)
    ['{689B4B3E-6EC3-40D9-B0D8-AFA269024451}']
    {class} function _GetALTURA_GAP: TJavaArray<Byte>; cdecl;
    {class} function _GetBACKFEED: TJavaArray<Byte>; cdecl;
    {class} function _GetBAUDRATE: TJavaArray<Byte>; cdecl;
    {class} function _GetCALOR: TJavaArray<Byte>; cdecl;
    {class} function _GetCR: TJavaArray<Byte>; cdecl;
    {class} function _GetDISABLE: TJavaArray<Byte>; cdecl;
    {class} function _GetENVIA_IMAGEM: TJavaArray<Byte>; cdecl;
    {class} function _GetESC: TJavaArray<Byte>; cdecl;
    {class} function _GetEXCLUI_IMAGEM: TJavaArray<Byte>; cdecl;
    {class} function _GetEXIT: TJavaArray<Byte>; cdecl;
    {class} function _GetFEED: TJavaArray<Byte>; cdecl;
    {class} function _GetLABEL: TJavaArray<Byte>; cdecl;
    {class} function _GetLENGTH: TJavaArray<Byte>; cdecl;
    {class} function _GetLF: TJavaArray<Byte>; cdecl;
    {class} function _GetLIMPA_MEMORIA: TJavaArray<Byte>; cdecl;
    {class} function _GetLIMPA_MODULO: TJavaArray<Byte>; cdecl;
    {class} function _GetLOGIC_IMG_MODE: TJavaArray<Byte>; cdecl;
    {class} function _GetMEMORY_STATUS: TJavaArray<Byte>; cdecl;
    {class} function _GetMIRROR: TJavaArray<Byte>; cdecl;
    {class} function _GetMODO_CONTINUO: TJavaArray<Byte>; cdecl;
    {class} function _GetNAO_CORTAR_ZERO: TJavaArray<Byte>; cdecl;
    {class} function _GetOFFSET_COLUNA: TJavaArray<Byte>; cdecl;
    {class} function _GetOFFSET_LINHA: TJavaArray<Byte>; cdecl;
    {class} function _GetPRINT_ST_POS: TJavaArray<Byte>; cdecl;
    {class} function _GetQTDE: TJavaArray<Byte>; cdecl;
    {class} function _GetRESET: TJavaArray<Byte>; cdecl;
    {class} function _GetSENSOR_REFLEXIVO: TJavaArray<Byte>; cdecl;
    {class} function _GetSENSOR_TRANSMISSIVO: TJavaArray<Byte>; cdecl;
    {class} function _GetSOH: TJavaArray<Byte>; cdecl;
    {class} function _GetSTATUS: TJavaArray<Byte>; cdecl;
    {class} function _GetSTATUS_EPL: TJavaArray<Byte>; cdecl;
    {class} function _GetSTX: TJavaArray<Byte>; cdecl;
    {class} function _GetSYMBOL_ASD: TJavaArray<Byte>; cdecl;
    {class} function _GetTAM_PIXEL: TJavaArray<Byte>; cdecl;
    {class} function _GetTESTE: TJavaArray<Byte>; cdecl;
    {class} function _GetTIPO_TRANSFERENCIA: TJavaArray<Byte>; cdecl;
    {class} function _GetUSAR_IMPERIAL: TJavaArray<Byte>; cdecl;
    {class} function _GetUSAR_METRICO: TJavaArray<Byte>; cdecl;
    {class} function _GetVEL_IMPRESSAO: TJavaArray<Byte>; cdecl;
    {class} function init: JPPLA; cdecl;
    {class} property ALTURA_GAP: TJavaArray<Byte> read _GetALTURA_GAP;
    {class} property BACKFEED: TJavaArray<Byte> read _GetBACKFEED;
    {class} property BAUDRATE: TJavaArray<Byte> read _GetBAUDRATE;
    {class} property CALOR: TJavaArray<Byte> read _GetCALOR;
    {class} property CR: TJavaArray<Byte> read _GetCR;
    {class} property DISABLE: TJavaArray<Byte> read _GetDISABLE;
    {class} property ENVIA_IMAGEM: TJavaArray<Byte> read _GetENVIA_IMAGEM;
    {class} property ESC: TJavaArray<Byte> read _GetESC;
    {class} property EXCLUI_IMAGEM: TJavaArray<Byte> read _GetEXCLUI_IMAGEM;
    {class} property EXIT: TJavaArray<Byte> read _GetEXIT;
    {class} property FEED: TJavaArray<Byte> read _GetFEED;
    {class} property &LABEL: TJavaArray<Byte> read _GetLABEL;
    {class} property LENGTH: TJavaArray<Byte> read _GetLENGTH;
    {class} property LF: TJavaArray<Byte> read _GetLF;
    {class} property LIMPA_MEMORIA: TJavaArray<Byte> read _GetLIMPA_MEMORIA;
    {class} property LIMPA_MODULO: TJavaArray<Byte> read _GetLIMPA_MODULO;
    {class} property LOGIC_IMG_MODE: TJavaArray<Byte> read _GetLOGIC_IMG_MODE;
    {class} property MEMORY_STATUS: TJavaArray<Byte> read _GetMEMORY_STATUS;
    {class} property MIRROR: TJavaArray<Byte> read _GetMIRROR;
    {class} property MODO_CONTINUO: TJavaArray<Byte> read _GetMODO_CONTINUO;
    {class} property NAO_CORTAR_ZERO: TJavaArray<Byte> read _GetNAO_CORTAR_ZERO;
    {class} property OFFSET_COLUNA: TJavaArray<Byte> read _GetOFFSET_COLUNA;
    {class} property OFFSET_LINHA: TJavaArray<Byte> read _GetOFFSET_LINHA;
    {class} property PRINT_ST_POS: TJavaArray<Byte> read _GetPRINT_ST_POS;
    {class} property QTDE: TJavaArray<Byte> read _GetQTDE;
    {class} property RESET: TJavaArray<Byte> read _GetRESET;
    {class} property SENSOR_REFLEXIVO: TJavaArray<Byte> read _GetSENSOR_REFLEXIVO;
    {class} property SENSOR_TRANSMISSIVO: TJavaArray<Byte> read _GetSENSOR_TRANSMISSIVO;
    {class} property SOH: TJavaArray<Byte> read _GetSOH;
    {class} property STATUS: TJavaArray<Byte> read _GetSTATUS;
    {class} property STATUS_EPL: TJavaArray<Byte> read _GetSTATUS_EPL;
    {class} property STX: TJavaArray<Byte> read _GetSTX;
    {class} property SYMBOL_ASD: TJavaArray<Byte> read _GetSYMBOL_ASD;
    {class} property TAM_PIXEL: TJavaArray<Byte> read _GetTAM_PIXEL;
    {class} property TESTE: TJavaArray<Byte> read _GetTESTE;
    {class} property TIPO_TRANSFERENCIA: TJavaArray<Byte> read _GetTIPO_TRANSFERENCIA;
    {class} property USAR_IMPERIAL: TJavaArray<Byte> read _GetUSAR_IMPERIAL;
    {class} property USAR_METRICO: TJavaArray<Byte> read _GetUSAR_METRICO;
    {class} property VEL_IMPRESSAO: TJavaArray<Byte> read _GetVEL_IMPRESSAO;
  end;

  [JavaSignature('com/elgin/e1/Impressora/Utilidades/PPLA')]
  JPPLA = interface(JObject)
    ['{B35FB308-331E-419B-824B-953A7D868F35}']
  end;
  TJPPLA = class(TJavaGenericImport<JPPLAClass, JPPLA>) end;

  JUtilidadesClass = interface(JObjectClass)
    ['{A94080BA-4270-4D7B-AC2F-ED46588C3CD3}']
    {class} function appendChild(P1: JNode; P2: JNode): JNode; cdecl;
    {class} function arg1(P1: JString; P2: Integer; P3: Char): JString; cdecl;
    {class} function array2bytes(P1: Boolean; P2: JArrayList): TJavaArray<Byte>; cdecl;
    {class} function attribute(P1: JNode; P2: JString): JString; cdecl;
    {class} function doc2string(P1: JDocument): JString; cdecl;
    {class} function elementsByTagName(P1: JNode; P2: JString): JNodeList; cdecl;
    {class} function getInt(P1: JString): Integer; cdecl;
    {class} function init: JUtilidades; cdecl;
    {class} function insert(P1: JString; P2: Integer; P3: JString): JString; cdecl;
    {class} function insertAfter(P1: JNode; P2: JNode; P3: JNode): JNode; cdecl;
    {class} function insertBefore(P1: JNode; P2: JNode; P3: JNode): JNode; cdecl;
    {class} function intToBits(P1: Integer): JBitSet; cdecl;
    {class} function larg2px(P1: Integer; P2: JString): Integer; cdecl;
    {class} function left(P1: JString; P2: Integer): JString; cdecl;
    {class} function leftJustified(P1: JString; P2: Integer; P3: Char; P4: Boolean): JString; cdecl;
    {class} function mid(P1: JString; P2: Integer; P3: Integer): JString; cdecl;
    {class} function namedItem(P1: JNode; P2: JString; P3: Boolean): JNode; cdecl;
    {class} function newDocument: JDocument; cdecl;
    {class} function numFmt(P1: Double): JString; cdecl;
    {class} function prepend(P1: JString; P2: JString): JString; cdecl;
    {class} function removeChild(P1: JNode; P2: JNode): JNode; cdecl;
    {class} function replaceChild(P1: JNode; P2: JNode; P3: JNode): JNode; cdecl;
    {class} function right(P1: JString; P2: Integer): JString; cdecl;
    {class} function rightJustified(P1: JString; P2: Integer; P3: Char; P4: Boolean): JString; cdecl;
    {class} function tam2px(P1: Integer; P2: JString): Integer; cdecl;
    {class} function toHex(P1: TJavaArray<Byte>; P2: Boolean): JString; cdecl;
    {class} function trimBitmap(P1: JBitmap; P2: Integer): JBitmap; cdecl;
    {class} function truncate(P1: JString; P2: Integer): JString; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Impressora/Utilidades/Utilidades')]
  JUtilidades = interface(JObject)
    ['{764F9ED0-9A64-4515-AA6D-A9AEC09CB5D9}']
  end;
  TJUtilidades = class(TJavaGenericImport<JUtilidadesClass, JUtilidades>) end;

  JNodeListClass = interface(IJavaClass)
    ['{6F6D0FED-4199-4F79-ABA2-C93007B65A8C}']
    {class} function getLength: Integer; cdecl;//Deprecated
    {class} function item(index: Integer): JNode; cdecl;//Deprecated
  end;

  [JavaSignature('org/w3c/dom/NodeList')]
  JNodeList = interface(IJavaInstance)
    ['{E6AE0711-1F43-4D8B-A153-47286455EFAF}']
  end;
  TJNodeList = class(TJavaGenericImport<JNodeListClass, JNodeList>) end;

  JUtilidades_1Class = interface(JNodeListClass)
    ['{51910172-DD70-42FF-8CF9-5F63924F835C}']
    {class} function getLength: Integer; cdecl;
    {class} function item(P1: Integer): JNode; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Impressora/Utilidades/Utilidades$1')]
  JUtilidades_1 = interface(JNodeList)
    ['{6665464A-3530-47FA-B995-F72AF007F680}']
  end;
  TJUtilidades_1 = class(TJavaGenericImport<JUtilidades_1Class, JUtilidades_1>) end;

  JInterfaceOBJXMLPRODUTOClass = interface(IJavaClass)
    ['{4379A291-C565-4C38-9DCC-E0D82DEA9526}']
    {class} function GetCodProduto: JString; cdecl;
    {class} function GetDescricao: JString; cdecl;
    {class} function GetNItem: JString; cdecl;
    {class} function GetQTD: JString; cdecl;
    {class} function GetUnidadeMed: JString; cdecl;
    {class} function GetVDesc: JString; cdecl;
    {class} function GetVDescProd: JString; cdecl;
    {class} function GetVOutros: JString; cdecl;
    {class} function GetVOutrosProd: JString; cdecl;
    {class} function GetValorBrutoProduto: JString; cdecl;
    {class} function GetValorUnit: JString; cdecl;
    {class} procedure SetCodProduto(P1: JString); cdecl;
    {class} procedure SetDescricao(P1: JString); cdecl;
    {class} procedure SetNItem(P1: JString); cdecl;
    {class} procedure SetQTD(P1: JString); cdecl;
    {class} procedure SetUnidadeMed(P1: JString); cdecl;
    {class} procedure SetVDesc(P1: JString); cdecl;
    {class} procedure SetVDescProd(P1: JString); cdecl;
    {class} procedure SetVOutros(P1: JString); cdecl;
    {class} procedure SetVOutrosProd(P1: JString); cdecl;
    {class} procedure SetValorBrutoProduto(P1: JString); cdecl;
    {class} procedure SetValorUnit(P1: JString); cdecl;
  end;

  [JavaSignature('com/elgin/e1/Impressora/XML/InterfaceOBJXMLPRODUTO')]
  JInterfaceOBJXMLPRODUTO = interface(IJavaInstance)
    ['{9FA38AD1-C2D6-4712-B3E8-A5234D9FC5D9}']
  end;
  TJInterfaceOBJXMLPRODUTO = class(TJavaGenericImport<JInterfaceOBJXMLPRODUTOClass, JInterfaceOBJXMLPRODUTO>) end;

  JImplementacaoOBJXMLPRODUTOClass = interface(JInterfaceOBJXMLPRODUTOClass)
    ['{5AB10838-8322-4B3F-B504-7DF7703E453A}']
    {class} function GetCodProduto: JString; cdecl;
    {class} function GetDescricao: JString; cdecl;
    {class} function GetNItem: JString; cdecl;
    {class} function GetQTD: JString; cdecl;
    {class} function GetUnidadeMed: JString; cdecl;
    {class} function GetVDesc: JString; cdecl;
    {class} function GetVDescProd: JString; cdecl;
    {class} function GetVOutros: JString; cdecl;
    {class} function GetVOutrosProd: JString; cdecl;
    {class} function GetValorBrutoProduto: JString; cdecl;
    {class} function GetValorUnit: JString; cdecl;
    {class} procedure SetCodProduto(P1: JString); cdecl;
    {class} procedure SetDescricao(P1: JString); cdecl;
    {class} procedure SetNItem(P1: JString); cdecl;
    {class} procedure SetQTD(P1: JString); cdecl;
    {class} procedure SetUnidadeMed(P1: JString); cdecl;
    {class} procedure SetVDesc(P1: JString); cdecl;
    {class} procedure SetVDescProd(P1: JString); cdecl;
    {class} procedure SetVOutros(P1: JString); cdecl;
    {class} procedure SetVOutrosProd(P1: JString); cdecl;
    {class} procedure SetValorBrutoProduto(P1: JString); cdecl;
    {class} procedure SetValorUnit(P1: JString); cdecl;
    {class} function init: JImplementacaoOBJXMLPRODUTO; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Impressora/XML/ImplementacaoOBJXMLPRODUTO')]
  JImplementacaoOBJXMLPRODUTO = interface(JInterfaceOBJXMLPRODUTO)
    ['{4E793E39-4BC5-4484-997F-D6504BD61C63}']
  end;
  TJImplementacaoOBJXMLPRODUTO = class(TJavaGenericImport<JImplementacaoOBJXMLPRODUTOClass, JImplementacaoOBJXMLPRODUTO>) end;

  JImplementacaoOBJPRODUTOXMLNFCEClass = interface(JImplementacaoOBJXMLPRODUTOClass)
    ['{665A4AE5-B5DB-45EB-A7FF-CC6EC34777D2}']
    {class} function GetEAN13: JString; cdecl;
    {class} procedure SetEAN13(P1: JString); cdecl;
    {class} function init: JImplementacaoOBJPRODUTOXMLNFCE; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Impressora/XML/ImplementacaoOBJPRODUTOXMLNFCE')]
  JImplementacaoOBJPRODUTOXMLNFCE = interface(JImplementacaoOBJXMLPRODUTO)
    ['{F78CD226-4703-4630-B274-3353CF17DE52}']
  end;
  TJImplementacaoOBJPRODUTOXMLNFCE = class(TJavaGenericImport<JImplementacaoOBJPRODUTOXMLNFCEClass, JImplementacaoOBJPRODUTOXMLNFCE>) end;

  JImplementacaoOBJPRODUTOXMLSATClass = interface(JImplementacaoOBJXMLPRODUTOClass)
    ['{F63E911E-8575-4AA7-8E4F-9FE14990AEB5}']
    {class} function GetVBC: JString; cdecl;
    {class} function GetVDeducISSQN: JString; cdecl;
    {class} function GetVDescProd: JString; cdecl;
    {class} function GetVOutrasProd: JString; cdecl;
    {class} function GetVRatAcr: JString; cdecl;
    {class} function GetVRatDesc: JString; cdecl;
    {class} function GetValorAproxTributos: JString; cdecl;
    {class} procedure SetVBC(P1: JString); cdecl;
    {class} procedure SetVDeducISSQN(P1: JString); cdecl;
    {class} procedure SetVDescProd(P1: JString); cdecl;
    {class} procedure SetVOutrasProd(P1: JString); cdecl;
    {class} procedure SetVRatAcr(P1: JString); cdecl;
    {class} procedure SetVRatDesc(P1: JString); cdecl;
    {class} procedure SetValorAproxTributos(P1: JString); cdecl;
    {class} function init: JImplementacaoOBJPRODUTOXMLSAT; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Impressora/XML/ImplementacaoOBJPRODUTOXMLSAT')]
  JImplementacaoOBJPRODUTOXMLSAT = interface(JImplementacaoOBJXMLPRODUTO)
    ['{623A469B-2B52-49A6-B5B0-45B3B09FBB05}']
  end;
  TJImplementacaoOBJPRODUTOXMLSAT = class(TJavaGenericImport<JImplementacaoOBJPRODUTOXMLSATClass, JImplementacaoOBJPRODUTOXMLSAT>) end;

  JInterfaceOBJXMLClass = interface(IJavaClass)
    ['{1234A811-0680-42A6-8058-89CA657DA7E5}']
    {class} function ConverterQString(P1: JString): TJavaArray<Char>; cdecl;
    {class} function FormatarData(P1: JString): TJavaArray<Char>; cdecl;
    {class} function FormatarDataSAT(P1: JString): TJavaArray<Char>; cdecl;
    {class} function FormatarMoeda(P1: JString): JString; cdecl;
    {class} function GetStatusXML: JString; cdecl;
    {class} function ObtemUF(P1: Integer): JString; cdecl;
    {class} procedure SetStatusXML(P1: JString); cdecl;
    {class} function getList(P1: JString): JNodeList; cdecl;
    {class} function getProp(P1: JString): JString; cdecl;
    {class} function getValue(P1: JString): JString; cdecl;
    {class} procedure imprimirLogo(P1: JString; P2: Integer; P3: JConexao; P4: JImplementacaoTermica); cdecl;
  end;

  [JavaSignature('com/elgin/e1/Impressora/XML/InterfaceOBJXML')]
  JInterfaceOBJXML = interface(IJavaInstance)
    ['{251C3071-F3D5-44F4-B473-99BDBB17FDA8}']
  end;
  TJInterfaceOBJXML = class(TJavaGenericImport<JInterfaceOBJXMLClass, JInterfaceOBJXML>) end;

  JImplementacaoOBJXMLClass = interface(JInterfaceOBJXMLClass)
    ['{F9C9FAB8-D30E-4BCB-AEF2-785EB8319D94}']
    {class} function _Getcon: JConexao; cdecl;
    {class} function init(P1: TJavaArray<Byte>): JImplementacaoOBJXML; cdecl;
    {class} property con: JConexao read _Getcon;
  end;

  [JavaSignature('com/elgin/e1/Impressora/XML/ImplementacaoOBJXML')]
  JImplementacaoOBJXML = interface(JInterfaceOBJXML)
    ['{838DBABE-9310-4503-AC39-5232C750E8F1}']
    function _Getib: JImplementacaoBematech; cdecl;
    function ConverterQString(P1: JString): TJavaArray<Char>; cdecl;
    function FormatarData(P1: JString): TJavaArray<Char>; cdecl;
    function FormatarDataSAT(P1: JString): TJavaArray<Char>; cdecl;
    function FormatarMoeda(P1: JString): JString; cdecl;
    function GetStatusXML: JString; cdecl;
    function ObtemUF(P1: Integer): JString; cdecl;
    procedure SetStatusXML(P1: JString); cdecl;
    function getList(P1: JString): JNodeList; cdecl;
    function getProp(P1: JString): JString; cdecl;
    function getValue(P1: JString): JString; cdecl;
    procedure imprimirLogo(P1: JString; P2: Integer; P3: JConexao; P4: JImplementacaoTermica); cdecl;
    property ib: JImplementacaoBematech read _Getib;
  end;
  TJImplementacaoOBJXML = class(TJavaGenericImport<JImplementacaoOBJXMLClass, JImplementacaoOBJXML>) end;

  JImplementacaoOBJXML_1Class = interface(JNodeListClass)
    ['{83C3EF54-F690-4861-BDA3-6250FADECC56}']
    {class} function init(P1: JImplementacaoOBJXML): JImplementacaoOBJXML_1; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Impressora/XML/ImplementacaoOBJXML$1')]
  JImplementacaoOBJXML_1 = interface(JNodeList)
    ['{B66CEDB2-5EC5-48CE-97C5-B9E0BC4705B2}']
    function getLength: Integer; cdecl;
    function item(P1: Integer): JNode; cdecl;
  end;
  TJImplementacaoOBJXML_1 = class(TJavaGenericImport<JImplementacaoOBJXML_1Class, JImplementacaoOBJXML_1>) end;

  JImplementacaoOBJXML_infoPagClass = interface(JObjectClass)
    ['{CD65AB9B-D454-466F-8919-A2D30AD53A00}']
    {class} function _GetmeioPgto: JString; cdecl;
    {class} function init(P1: JImplementacaoOBJXML): JImplementacaoOBJXML_infoPag; cdecl; overload;
    {class} function init(P1: JImplementacaoOBJXML; P2: JString; P3: Double): JImplementacaoOBJXML_infoPag; cdecl; overload;
    {class} property meioPgto: JString read _GetmeioPgto;
  end;

  [JavaSignature('com/elgin/e1/Impressora/XML/ImplementacaoOBJXML$infoPag')]
  JImplementacaoOBJXML_infoPag = interface(JObject)
    ['{C690D795-E73D-4476-8CC4-DDB29D2B8099}']
    function _Getthis: JImplementacaoOBJXML; cdecl;
    function _GetvalorPago: Double; cdecl;
    property this: JImplementacaoOBJXML read _Getthis;
    property valorPago: Double read _GetvalorPago;
  end;
  TJImplementacaoOBJXML_infoPag = class(TJavaGenericImport<JImplementacaoOBJXML_infoPagClass, JImplementacaoOBJXML_infoPag>) end;

  JImplementacaoOBJXMLCANCELAMENTOClass = interface(JImplementacaoOBJXMLClass)
    ['{A3B451F6-0B77-4805-B91D-2207B8183DA4}']
    {class} function ConstroiObj: Boolean; cdecl;
    {class} function GetAssQRCode: JString; cdecl;
    {class} function GetCNPJ: JString; cdecl;
    {class} function GetCPF_CNPJ: JString; cdecl;
    {class} function GetChaveAcesso: JString; cdecl;
    {class} function GetChaveAcessoACancelar: JString; cdecl;
    {class} function GetDtHrCupomACancelar: JString; cdecl;
    {class} function GetDtHrEmissao: JString; cdecl;
    {class} function GetEndereco: JString; cdecl;
    {class} function GetIE: JString; cdecl;
    {class} function GetIM: JString; cdecl;
    {class} function GetNCfe: JString; cdecl;
    {class} function GetNomeFantasia: JString; cdecl;
    {class} function GetNumSerieSAT: JString; cdecl;
    {class} function GetRazaoSocial: JString; cdecl;
    {class} function GetVCfe: JString; cdecl;
    {class} function PreencheOBJ(P1: Integer): Boolean; cdecl;
    {class} procedure SetAssQRCode(P1: JString); cdecl;
    {class} procedure SetCNPJ(P1: JString); cdecl;
    {class} procedure SetCPF_CNPJ(P1: JString); cdecl;
    {class} procedure SetChaveAcesso(P1: JString); cdecl;
    {class} procedure SetChaveAcessoACancelar(P1: JString); cdecl;
    {class} procedure SetDtHrCupomACancelar(P1: JString); cdecl;
    {class} procedure SetDtHrEmissao(P1: JString); cdecl;
    {class} procedure SetEndereco(P1: JString); cdecl;
    {class} procedure SetIE(P1: JString); cdecl;
    {class} procedure SetIM(P1: JString); cdecl;
    {class} procedure SetNCfe(P1: JString); cdecl;
    {class} procedure SetNomeFantasia(P1: JString); cdecl;
    {class} procedure SetNumSeriaSAT(P1: JString); cdecl;
    {class} procedure SetRazaoSocial(P1: JString); cdecl;
    {class} procedure SetVCfe(P1: JString); cdecl;
    {class} function init(P1: TJavaArray<Byte>): JImplementacaoOBJXMLCANCELAMENTO; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Impressora/XML/ImplementacaoOBJXMLCANCELAMENTO')]
  JImplementacaoOBJXMLCANCELAMENTO = interface(JImplementacaoOBJXML)
    ['{585C5827-5A42-470F-B138-D9697ECD3C27}']
  end;
  TJImplementacaoOBJXMLCANCELAMENTO = class(TJavaGenericImport<JImplementacaoOBJXMLCANCELAMENTOClass, JImplementacaoOBJXMLCANCELAMENTO>) end;

  JImplementacaoOBJXMLNFCEClass = interface(JImplementacaoOBJXMLClass)
    ['{8C1E1038-7ACF-48AC-9079-B1AEEFF2366C}']
    {class} function ConstroiInfQRCode(P1: Integer; P2: JString): JString; cdecl;
    {class} function ConstroiOBJ: Boolean; cdecl;
    {class} function GetCNPJConsumidor: JString; cdecl;
    {class} function GetCNPJEmit: JString; cdecl;
    {class} function GetCPFConsumidor: JString; cdecl;
    {class} function GetChaveConsulta: JString; cdecl;
    {class} function GetDHEmi: JString; cdecl;
    {class} function GetDHRecpto: JString; cdecl;
    {class} function GetDigestValue: JString; cdecl;
    {class} function GetEndereco: JString; cdecl;
    {class} function GetEnderecoDest: JString; cdecl;
    {class} function GetEnderecoEntrega: JString; cdecl;
    {class} function GetIDEstrConsumidor: JString; cdecl;
    {class} function GetIE: JString; cdecl;
    {class} function GetInfAdFisco: JString; cdecl;
    {class} function GetInfCpl: JString; cdecl;
    {class} function GetInfoPag: JArrayList; cdecl;
    {class} function GetNNF: JString; cdecl;
    {class} function GetNProtocolo: JString; cdecl;
    {class} function GetNomeDest: JString; cdecl;
    {class} function GetProdutos: JArrayList; cdecl;
    {class} function GetRazaoSocial: JString; cdecl;
    {class} function GetSerie: JString; cdecl;
    {class} function GetTpAmb: JString; cdecl;
    {class} function GetTpEmis: JString; cdecl;
    {class} function GetVDesc: JString; cdecl;
    {class} function GetVFrete: JString; cdecl;
    {class} function GetVNF: JString; cdecl;
    {class} function GetVOutros: JString; cdecl;
    {class} function GetVSeg: JString; cdecl;
    {class} function GetVTotTrib: JString; cdecl;
    {class} function GetVTroco: JString; cdecl;
    {class} function ObtemURL(P1: Integer; P2: Integer): JString; cdecl;
    {class} function PreencheCabecalho(P1: Integer; P2: Boolean): TJavaArray<Char>; cdecl;
    {class} function PreencheLegendaProduto(P1: Integer; P2: Integer): TJavaArray<Char>; cdecl;
    {class} function PreencheLinhaProduto(P1: JImplementacaoOBJPRODUTOXMLNFCE; P2: Integer; P3: Integer): TJavaArray<Char>; cdecl;
    {class} function PreencheOBJ(P1: Integer): Boolean; cdecl;
    {class} procedure SetCNPJConsumidor(P1: JString); cdecl;
    {class} procedure SetCNPJEmit(P1: JString); cdecl;
    {class} procedure SetCPFConsumidor(P1: JString); cdecl;
    {class} procedure SetChaveConsulta(P1: JString); cdecl;
    {class} procedure SetDHEmi(P1: JString); cdecl;
    {class} procedure SetDHRecpto(P1: JString); cdecl;
    {class} procedure SetDigestValue(P1: JString); cdecl;
    {class} procedure SetEndereco(P1: JString); cdecl;
    {class} procedure SetEnderecoDest(P1: JString); cdecl;
    {class} procedure SetEnderecoEntrega(P1: JString); cdecl;
    {class} procedure SetIDEstrConsumidor(P1: JString); cdecl;
    {class} procedure SetIE(P1: JString); cdecl;
    {class} procedure SetInfAdFisco(P1: JString); cdecl;
    {class} procedure SetInfCpl(P1: JString); cdecl;
    {class} procedure SetNNF(P1: JString); cdecl;
    {class} procedure SetNProtocolo(P1: JString); cdecl;
    {class} procedure SetNomeDest(P1: JString); cdecl;
    {class} procedure SetRazaoSocial(P1: JString); cdecl;
    {class} procedure SetSerie(P1: JString); cdecl;
    {class} procedure SetTpAmb(P1: JString); cdecl;
    {class} procedure SetTpEmis(P1: JString); cdecl;
    {class} procedure SetVDesc(P1: JString); cdecl;
    {class} procedure SetVFrete(P1: JString); cdecl;
    {class} procedure SetVNF(P1: JString); cdecl;
    {class} procedure SetVOutros(P1: JString); cdecl;
    {class} procedure SetVSeg(P1: JString); cdecl;
    {class} procedure SetVTotTrib(P1: JString); cdecl;
    {class} procedure SetVTroco(P1: JString); cdecl;
    {class} function init(P1: TJavaArray<Byte>): JImplementacaoOBJXMLNFCE; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Impressora/XML/ImplementacaoOBJXMLNFCE')]
  JImplementacaoOBJXMLNFCE = interface(JImplementacaoOBJXML)
    ['{7A106AB2-31EB-4C8B-BD6C-759AE2AF7F68}']
  end;
  TJImplementacaoOBJXMLNFCE = class(TJavaGenericImport<JImplementacaoOBJXMLNFCEClass, JImplementacaoOBJXMLNFCE>) end;

  JImplementacaoOBJXMLSATClass = interface(JImplementacaoOBJXMLClass)
    ['{0EB44B05-2EA4-432A-BC11-10E64E9A0737}']
    {class} function ConstruirObj: Boolean; cdecl;
    {class} function GetCNPJ: JString; cdecl;
    {class} function GetCNPJSH: JString; cdecl;
    {class} function GetCodQRCode: JString; cdecl;
    {class} function GetCodigoBarras: JString; cdecl;
    {class} function GetDocDest: JString; cdecl;
    {class} function GetDocDestRaw: JString; cdecl;
    {class} function GetDtHrEmissao: JString; cdecl;
    {class} function GetEndDest: JString; cdecl;
    {class} function GetEndereco: JString; cdecl;
    {class} function GetIE: JString; cdecl;
    {class} function GetIM: JString; cdecl;
    {class} function GetInfCpl: JString; cdecl;
    {class} function GetNomeDest: JString; cdecl;
    {class} function GetNomeFantasia: JString; cdecl;
    {class} function GetNumDoc: JString; cdecl;
    {class} function GetNumSerieSAT: JString; cdecl;
    {class} function GetPagamentos: JArrayList; cdecl;
    {class} function GetProdutos: JArrayList; cdecl;
    {class} function GetRazaoSocial: JString; cdecl;
    {class} function GetSignAC: JString; cdecl;
    {class} function GetVAcresSubtot: JString; cdecl;
    {class} function GetVCFeLei12741: JString; cdecl;
    {class} function GetVDesc: JString; cdecl;
    {class} function GetVDescSubtot: JString; cdecl;
    {class} function GetVOutras: JString; cdecl;
    {class} function GetVProd: JString; cdecl;
    {class} function GetVTroco: JString; cdecl;
    {class} function GetValorCfe: JString; cdecl;
    {class} function GetXCampo: JString; cdecl;
    {class} function GetXTexto: JString; cdecl;
    {class} function PreencheLinhaProduto(P1: JImplementacaoOBJPRODUTOXMLSAT; P2: Integer; P3: Integer): TJavaArray<Char>; cdecl;
    {class} function PreencheOBJ(P1: Integer): Boolean; cdecl;
    {class} procedure SetCNPJ(P1: JString); cdecl;
    {class} procedure SetCNPJSH(P1: JString); cdecl;
    {class} procedure SetCodQRCode(P1: JString); cdecl;
    {class} procedure SetCodigoBarras(P1: JString); cdecl;
    {class} procedure SetDocDest(P1: JString); cdecl;
    {class} procedure SetDocDestRaw(P1: JString); cdecl;
    {class} procedure SetDtHrEmissao(P1: JString); cdecl;
    {class} procedure SetEndDest(P1: JString); cdecl;
    {class} procedure SetEndereco(P1: JString); cdecl;
    {class} procedure SetIE(P1: JString); cdecl;
    {class} procedure SetIM(P1: JString); cdecl;
    {class} procedure SetInfCpl(P1: JString); cdecl;
    {class} procedure SetNomeDest(P1: JString); cdecl;
    {class} procedure SetNomeFantasia(P1: JString); cdecl;
    {class} procedure SetNumDoc(P1: JString); cdecl;
    {class} procedure SetNumSerieSAT(P1: JString); cdecl;
    {class} procedure SetProdutos(P1: JImplementacaoOBJPRODUTOXMLSAT); cdecl;
    {class} procedure SetRazaoSocial(P1: JString); cdecl;
    {class} procedure SetSignAC(P1: JString); cdecl;
    {class} procedure SetVAcresSubtot(P1: JString); cdecl;
    {class} procedure SetVCFeLei12741(P1: JString); cdecl;
    {class} procedure SetVDesc(P1: JString); cdecl;
    {class} procedure SetVDescSubtot(P1: JString); cdecl;
    {class} procedure SetVOutras(P1: JString); cdecl;
    {class} procedure SetVProd(P1: JString); cdecl;
    {class} procedure SetVTroco(P1: JString); cdecl;
    {class} procedure SetValorCfe(P1: JString); cdecl;
    {class} procedure SetXCampo(P1: JString); cdecl;
    {class} procedure SetXTexto(P1: JString); cdecl;
    {class} function init(P1: TJavaArray<Byte>): JImplementacaoOBJXMLSAT; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Impressora/XML/ImplementacaoOBJXMLSAT')]
  JImplementacaoOBJXMLSAT = interface(JImplementacaoOBJXML)
    ['{9B07DC13-DFA1-40B0-B7B5-7F0528F5E187}']
  end;
  TJImplementacaoOBJXMLSAT = class(TJavaGenericImport<JImplementacaoOBJXMLSATClass, JImplementacaoOBJXMLSAT>) end;

  JInterfaceOBJPRODUTOXMLNFCEClass = interface(JInterfaceOBJXMLPRODUTOClass)
    ['{BF88AED8-6619-4D59-93BC-FDF539F33909}']
    {class} function GetEAN13: JString; cdecl;
    {class} procedure SetEAN13(P1: JString); cdecl;
  end;

  [JavaSignature('com/elgin/e1/Impressora/XML/InterfaceOBJPRODUTOXMLNFCE')]
  JInterfaceOBJPRODUTOXMLNFCE = interface(JInterfaceOBJXMLPRODUTO)
    ['{B419B86D-F0EC-4858-95F7-F94A626E24A1}']
  end;
  TJInterfaceOBJPRODUTOXMLNFCE = class(TJavaGenericImport<JInterfaceOBJPRODUTOXMLNFCEClass, JInterfaceOBJPRODUTOXMLNFCE>) end;

  JInterfaceOBJPRODUTOXMLSATClass = interface(JInterfaceOBJXMLPRODUTOClass)
    ['{D76585BB-C1DA-4DF1-A6F1-4ED775949A53}']
    {class} function GetVBC: JString; cdecl;
    {class} function GetVDeducISSQN: JString; cdecl;
    {class} function GetVDescProd: JString; cdecl;
    {class} function GetVOutrasProd: JString; cdecl;
    {class} function GetVRatAcr: JString; cdecl;
    {class} function GetVRatDesc: JString; cdecl;
    {class} function GetValorAproxTributos: JString; cdecl;
    {class} procedure SetVBC(P1: JString); cdecl;
    {class} procedure SetVDeducISSQN(P1: JString); cdecl;
    {class} procedure SetVDescProd(P1: JString); cdecl;
    {class} procedure SetVOutrasProd(P1: JString); cdecl;
    {class} procedure SetVRatAcr(P1: JString); cdecl;
    {class} procedure SetVRatDesc(P1: JString); cdecl;
    {class} procedure SetValorAproxTributos(P1: JString); cdecl;
  end;

  [JavaSignature('com/elgin/e1/Impressora/XML/InterfaceOBJPRODUTOXMLSAT')]
  JInterfaceOBJPRODUTOXMLSAT = interface(JInterfaceOBJXMLPRODUTO)
    ['{1CEC8A66-E18A-43DE-B3BE-0BDC58F3E5AC}']
  end;
  TJInterfaceOBJPRODUTOXMLSAT = class(TJavaGenericImport<JInterfaceOBJPRODUTOXMLSATClass, JInterfaceOBJPRODUTOXMLSAT>) end;

  JInterfaceOBJXMLCANCELAMENTOClass = interface(JInterfaceOBJXMLClass)
    ['{417D1AF5-B6DA-401A-95F1-7CE228376FB8}']
    {class} function ConstroiObj: Boolean; cdecl;
    {class} function GetAssQRCode: JString; cdecl;
    {class} function GetCNPJ: JString; cdecl;
    {class} function GetCPF_CNPJ: JString; cdecl;
    {class} function GetChaveAcesso: JString; cdecl;
    {class} function GetChaveAcessoACancelar: JString; cdecl;
    {class} function GetDtHrCupomACancelar: JString; cdecl;
    {class} function GetDtHrEmissao: JString; cdecl;
    {class} function GetEndereco: JString; cdecl;
    {class} function GetIE: JString; cdecl;
    {class} function GetIM: JString; cdecl;
    {class} function GetNCfe: JString; cdecl;
    {class} function GetNomeFantasia: JString; cdecl;
    {class} function GetNumSerieSAT: JString; cdecl;
    {class} function GetRazaoSocial: JString; cdecl;
    {class} function GetVCfe: JString; cdecl;
    {class} function PreencheOBJ(P1: Integer): Boolean; cdecl;
    {class} procedure SetAssQRCode(P1: JString); cdecl;
    {class} procedure SetCNPJ(P1: JString); cdecl;
    {class} procedure SetCPF_CNPJ(P1: JString); cdecl;
    {class} procedure SetChaveAcesso(P1: JString); cdecl;
    {class} procedure SetChaveAcessoACancelar(P1: JString); cdecl;
    {class} procedure SetDtHrCupomACancelar(P1: JString); cdecl;
    {class} procedure SetDtHrEmissao(P1: JString); cdecl;
    {class} procedure SetEndereco(P1: JString); cdecl;
    {class} procedure SetIE(P1: JString); cdecl;
    {class} procedure SetIM(P1: JString); cdecl;
    {class} procedure SetNCfe(P1: JString); cdecl;
    {class} procedure SetNomeFantasia(P1: JString); cdecl;
    {class} procedure SetNumSeriaSAT(P1: JString); cdecl;
    {class} procedure SetRazaoSocial(P1: JString); cdecl;
    {class} procedure SetVCfe(P1: JString); cdecl;
  end;

  [JavaSignature('com/elgin/e1/Impressora/XML/InterfaceOBJXMLCANCELAMENTO')]
  JInterfaceOBJXMLCANCELAMENTO = interface(JInterfaceOBJXML)
    ['{91E46B7B-3A86-48D9-8CB8-E5162B80EDB7}']
  end;
  TJInterfaceOBJXMLCANCELAMENTO = class(TJavaGenericImport<JInterfaceOBJXMLCANCELAMENTOClass, JInterfaceOBJXMLCANCELAMENTO>) end;

  JInterfaceOBJXMLNFCEClass = interface(JInterfaceOBJXMLClass)
    ['{3783D732-0B5A-492E-BB71-95642EDC3A3B}']
    {class} function ConstroiInfQRCode(P1: Integer; P2: JString): JString; cdecl;
    {class} function ConstroiOBJ: Boolean; cdecl;
    {class} function GetCNPJConsumidor: JString; cdecl;
    {class} function GetCNPJEmit: JString; cdecl;
    {class} function GetCPFConsumidor: JString; cdecl;
    {class} function GetChaveConsulta: JString; cdecl;
    {class} function GetDHEmi: JString; cdecl;
    {class} function GetDHRecpto: JString; cdecl;
    {class} function GetDigestValue: JString; cdecl;
    {class} function GetEndereco: JString; cdecl;
    {class} function GetEnderecoDest: JString; cdecl;
    {class} function GetEnderecoEntrega: JString; cdecl;
    {class} function GetIDEstrConsumidor: JString; cdecl;
    {class} function GetIE: JString; cdecl;
    {class} function GetInfAdFisco: JString; cdecl;
    {class} function GetInfCpl: JString; cdecl;
    {class} function GetInfoPag: JArrayList; cdecl;
    {class} function GetNNF: JString; cdecl;
    {class} function GetNProtocolo: JString; cdecl;
    {class} function GetNomeDest: JString; cdecl;
    {class} function GetProdutos: JArrayList; cdecl;
    {class} function GetRazaoSocial: JString; cdecl;
    {class} function GetSerie: JString; cdecl;
    {class} function GetTpAmb: JString; cdecl;
    {class} function GetTpEmis: JString; cdecl;
    {class} function GetVDesc: JString; cdecl;
    {class} function GetVFrete: JString; cdecl;
    {class} function GetVNF: JString; cdecl;
    {class} function GetVOutros: JString; cdecl;
    {class} function GetVSeg: JString; cdecl;
    {class} function GetVTotTrib: JString; cdecl;
    {class} function GetVTroco: JString; cdecl;
    {class} function ObtemURL(P1: Integer; P2: Integer): JString; cdecl;
    {class} function PreencheCabecalho(P1: Integer; P2: Boolean): TJavaArray<Char>; cdecl;
    {class} function PreencheLegendaProduto(P1: Integer; P2: Integer): TJavaArray<Char>; cdecl;
    {class} function PreencheLinhaProduto(P1: JImplementacaoOBJPRODUTOXMLNFCE; P2: Integer; P3: Integer): TJavaArray<Char>; cdecl;
    {class} function PreencheOBJ(P1: Integer): Boolean; cdecl;
    {class} procedure SetCNPJConsumidor(P1: JString); cdecl;
    {class} procedure SetCNPJEmit(P1: JString); cdecl;
    {class} procedure SetCPFConsumidor(P1: JString); cdecl;
    {class} procedure SetChaveConsulta(P1: JString); cdecl;
    {class} procedure SetDHEmi(P1: JString); cdecl;
    {class} procedure SetDHRecpto(P1: JString); cdecl;
    {class} procedure SetDigestValue(P1: JString); cdecl;
    {class} procedure SetEndereco(P1: JString); cdecl;
    {class} procedure SetEnderecoDest(P1: JString); cdecl;
    {class} procedure SetEnderecoEntrega(P1: JString); cdecl;
    {class} procedure SetIDEstrConsumidor(P1: JString); cdecl;
    {class} procedure SetIE(P1: JString); cdecl;
    {class} procedure SetInfAdFisco(P1: JString); cdecl;
    {class} procedure SetInfCpl(P1: JString); cdecl;
    {class} procedure SetNNF(P1: JString); cdecl;
    {class} procedure SetNProtocolo(P1: JString); cdecl;
    {class} procedure SetNomeDest(P1: JString); cdecl;
    {class} procedure SetRazaoSocial(P1: JString); cdecl;
    {class} procedure SetSerie(P1: JString); cdecl;
    {class} procedure SetTpAmb(P1: JString); cdecl;
    {class} procedure SetTpEmis(P1: JString); cdecl;
    {class} procedure SetVDesc(P1: JString); cdecl;
    {class} procedure SetVFrete(P1: JString); cdecl;
    {class} procedure SetVNF(P1: JString); cdecl;
    {class} procedure SetVOutros(P1: JString); cdecl;
    {class} procedure SetVSeg(P1: JString); cdecl;
    {class} procedure SetVTotTrib(P1: JString); cdecl;
    {class} procedure SetVTroco(P1: JString); cdecl;
  end;

  [JavaSignature('com/elgin/e1/Impressora/XML/InterfaceOBJXMLNFCE')]
  JInterfaceOBJXMLNFCE = interface(JInterfaceOBJXML)
    ['{C4B6CC0D-3F89-414F-8334-6E72F6E52203}']
  end;
  TJInterfaceOBJXMLNFCE = class(TJavaGenericImport<JInterfaceOBJXMLNFCEClass, JInterfaceOBJXMLNFCE>) end;

  JInterfaceOBJXMLSATClass = interface(JInterfaceOBJXMLClass)
    ['{0A4E6C2A-645B-4179-8A1E-B2E78A46B5B0}']
    {class} function ConstruirObj: Boolean; cdecl;
    {class} function GetCNPJ: JString; cdecl;
    {class} function GetCNPJSH: JString; cdecl;
    {class} function GetCodQRCode: JString; cdecl;
    {class} function GetCodigoBarras: JString; cdecl;
    {class} function GetDocDest: JString; cdecl;
    {class} function GetDocDestRaw: JString; cdecl;
    {class} function GetDtHrEmissao: JString; cdecl;
    {class} function GetEndDest: JString; cdecl;
    {class} function GetEndereco: JString; cdecl;
    {class} function GetIE: JString; cdecl;
    {class} function GetIM: JString; cdecl;
    {class} function GetInfCpl: JString; cdecl;
    {class} function GetNomeDest: JString; cdecl;
    {class} function GetNomeFantasia: JString; cdecl;
    {class} function GetNumDoc: JString; cdecl;
    {class} function GetNumSerieSAT: JString; cdecl;
    {class} function GetPagamentos: JArrayList; cdecl;
    {class} function GetProdutos: JArrayList; cdecl;
    {class} function GetRazaoSocial: JString; cdecl;
    {class} function GetSignAC: JString; cdecl;
    {class} function GetVAcresSubtot: JString; cdecl;
    {class} function GetVCFeLei12741: JString; cdecl;
    {class} function GetVDesc: JString; cdecl;
    {class} function GetVDescSubtot: JString; cdecl;
    {class} function GetVOutras: JString; cdecl;
    {class} function GetVProd: JString; cdecl;
    {class} function GetVTroco: JString; cdecl;
    {class} function GetValorCfe: JString; cdecl;
    {class} function GetXCampo: JString; cdecl;
    {class} function GetXTexto: JString; cdecl;
    {class} function PreencheLinhaProduto(P1: JImplementacaoOBJPRODUTOXMLSAT; P2: Integer; P3: Integer): TJavaArray<Char>; cdecl;
    {class} function PreencheOBJ(P1: Integer): Boolean; cdecl;
    {class} procedure SetCNPJ(P1: JString); cdecl;
    {class} procedure SetCNPJSH(P1: JString); cdecl;
    {class} procedure SetCodQRCode(P1: JString); cdecl;
    {class} procedure SetCodigoBarras(P1: JString); cdecl;
    {class} procedure SetDocDest(P1: JString); cdecl;
    {class} procedure SetDocDestRaw(P1: JString); cdecl;
    {class} procedure SetDtHrEmissao(P1: JString); cdecl;
    {class} procedure SetEndDest(P1: JString); cdecl;
    {class} procedure SetEndereco(P1: JString); cdecl;
    {class} procedure SetIE(P1: JString); cdecl;
    {class} procedure SetIM(P1: JString); cdecl;
    {class} procedure SetInfCpl(P1: JString); cdecl;
    {class} procedure SetNomeDest(P1: JString); cdecl;
    {class} procedure SetNomeFantasia(P1: JString); cdecl;
    {class} procedure SetNumDoc(P1: JString); cdecl;
    {class} procedure SetNumSerieSAT(P1: JString); cdecl;
    {class} procedure SetProdutos(P1: JImplementacaoOBJPRODUTOXMLSAT); cdecl;
    {class} procedure SetRazaoSocial(P1: JString); cdecl;
    {class} procedure SetSignAC(P1: JString); cdecl;
    {class} procedure SetVAcresSubtot(P1: JString); cdecl;
    {class} procedure SetVCFeLei12741(P1: JString); cdecl;
    {class} procedure SetVDesc(P1: JString); cdecl;
    {class} procedure SetVDescSubtot(P1: JString); cdecl;
    {class} procedure SetVOutras(P1: JString); cdecl;
    {class} procedure SetVProd(P1: JString); cdecl;
    {class} procedure SetVTroco(P1: JString); cdecl;
    {class} procedure SetValorCfe(P1: JString); cdecl;
    {class} procedure SetXCampo(P1: JString); cdecl;
    {class} procedure SetXTexto(P1: JString); cdecl;
  end;

  [JavaSignature('com/elgin/e1/Impressora/XML/InterfaceOBJXMLSAT')]
  JInterfaceOBJXMLSAT = interface(JInterfaceOBJXML)
    ['{0BEC9C95-38BD-4FE6-9A9D-C17045FC51D4}']
  end;
  TJInterfaceOBJXMLSAT = class(TJavaGenericImport<JInterfaceOBJXMLSATClass, JInterfaceOBJXMLSAT>) end;

  JScanner_ScannerClass = interface(JObjectClass)
    ['{2BBCDF17-3C78-4155-B893-287E9B8B2E6C}']
    {class} function getScanner(P1: JContext): JIntent; cdecl;
    {class} function init: JScanner_Scanner; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Scanner/Scanner')]
  JScanner_Scanner = interface(JObject)
    ['{304A0639-9543-4B91-B665-0E5D6F91A844}']
  end;
  TJScanner_Scanner = class(TJavaGenericImport<JScanner_ScannerClass, JScanner_Scanner>) end;

  JAssinaturasClass = interface(JObjectClass)
    ['{FE8A3E09-906A-464C-A1AC-AEEE6F5F3730}']
    {class} function _GetBOOLEAN: Integer; cdecl;
    {class} function _GetBYTE: Integer; cdecl;
    {class} function _GetCHAR: Integer; cdecl;
    {class} function _GetDOUBLE: Integer; cdecl;
    {class} function _GetFLOAT: Integer; cdecl;
    {class} function _GetINT: Integer; cdecl;
    {class} function _GetLONG: Integer; cdecl;
    {class} function _GetSHORT: Integer; cdecl;
    {class} function _GetSTRING: Integer; cdecl;
    {class} function getAssinaturasEtiqueta: JHashMap; cdecl;
    {class} function getAssinaturasSAT: JHashMap; cdecl;
    {class} function getAssinaturasTermica: JHashMap; cdecl;
    {class} function init: JAssinaturas; cdecl;
    {class} property BOOLEAN: Integer read _GetBOOLEAN;
    {class} property BYTE: Integer read _GetBYTE;
    {class} property CHAR: Integer read _GetCHAR;
    {class} property DOUBLE: Integer read _GetDOUBLE;
    {class} property FLOAT: Integer read _GetFLOAT;
    {class} property INT: Integer read _GetINT;
    {class} property LONG: Integer read _GetLONG;
    {class} property SHORT: Integer read _GetSHORT;
    {class} property &STRING: Integer read _GetSTRING;
  end;

  [JavaSignature('com/elgin/e1/Servico/Assinaturas')]
  JAssinaturas = interface(JObject)
    ['{34A7453C-8EAC-48F2-9045-7752279179F2}']
  end;
  TJAssinaturas = class(TJavaGenericImport<JAssinaturasClass, JAssinaturas>) end;

  JParametrosClass = interface(JObjectClass)
    ['{8EC74349-E3DF-448E-8BD8-9AC11C182F95}']
  end;

  [JavaSignature('com/elgin/e1/Servico/Parametros')]
  JParametros = interface(JObject)
    ['{75002989-5A38-440B-8FF6-9FA4BFE4583C}']
  end;
  TJParametros = class(TJavaGenericImport<JParametrosClass, JParametros>) end;

  JServicoE1Class = interface(JObjectClass)
    ['{D461D07E-59F8-4358-98D5-0A3A7287C071}']
    {class} function _GetCOMANDO_DELIMITADO: Integer; cdecl;
    {class} function _GetCOMANDO_JSON: Integer; cdecl;
    {class} function _GetMODULO_ETIQUETA: JString; cdecl;
    {class} function _GetMODULO_IMPRESSOR: JString; cdecl;
    {class} function _GetMODULO_SAT: JString; cdecl;
    {class} function Abrir(P1: JString; P2: Integer): Integer; cdecl;
    {class} function Fechar: Integer; cdecl;
    {class} function GetSepValores: JString; cdecl;
    {class} function GetTipoComando: Integer; cdecl;
    {class} function ReceberDados(P1: JInteiro): TJavaArray<Byte>; cdecl; overload;
    {class} function ReceberDados(P1: JInteiro; P2: Integer): TJavaArray<Byte>; cdecl; overload;
    {class} function SetSepValores(P1: JString): Integer; cdecl;
    {class} function SetTipoComando(P1: Integer): Integer; cdecl;
    {class} function getServiceTimeout: Integer; cdecl;
    {class} function init: JServicoE1; cdecl;
    {class} function isAberto: Boolean; cdecl;
    {class} function isAutServico: Boolean; cdecl;
    {class} function setContext(P1: JContext): Integer; cdecl;
    {class} property COMANDO_DELIMITADO: Integer read _GetCOMANDO_DELIMITADO;
    {class} property COMANDO_JSON: Integer read _GetCOMANDO_JSON;
    {class} property MODULO_ETIQUETA: JString read _GetMODULO_ETIQUETA;
    {class} property MODULO_IMPRESSOR: JString read _GetMODULO_IMPRESSOR;
    {class} property MODULO_SAT: JString read _GetMODULO_SAT;
  end;

  [JavaSignature('com/elgin/e1/Servico/ServicoE1')]
  JServicoE1 = interface(JObject)
    ['{A4C572E4-FCE0-43B3-8620-4F14F422E3A2}']
  end;
  TJServicoE1 = class(TJavaGenericImport<JServicoE1Class, JServicoE1>) end;

  JServicoE1_EtiquetaClass = interface(JObjectClass)
    ['{1F433714-4057-4D0D-BA5E-C80A606ECF09}']
    {class} function DespejarArquivo(P1: Integer; P2: JString; P3: JString; P4: Integer; P5: JString): TJavaArray<Byte>; cdecl;
    {class} function EnviaImagem(P1: Integer; P2: JString; P3: JString; P4: Integer; P5: Integer; P6: Integer; P7: JString; P8: JString): TJavaArray<Byte>; cdecl;
    {class} function ExcluiImagem(P1: Integer; P2: JString; P3: JString; P4: Integer; P5: Integer; P6: JString): TJavaArray<Byte>; cdecl;
    {class} function Feed(P1: Integer; P2: JString; P3: JString; P4: Integer): TJavaArray<Byte>; cdecl;
    {class} function GerarBarCode1D(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: Integer; P8: JString; P9: Integer): TJavaArray<Byte>; cdecl;
    {class} function GerarBox(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer): TJavaArray<Byte>; cdecl;
    {class} function GerarDataBar(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: Integer; P8: JString; P9: JString): TJavaArray<Byte>; cdecl;
    {class} function GerarDataBarExpanded(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: Integer; P8: JString; P9: JString): TJavaArray<Byte>; cdecl;
    {class} function GerarDataMatrix(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: JString): TJavaArray<Byte>; cdecl;
    {class} function GerarImagem(P1: Integer; P2: Integer; P3: JString): TJavaArray<Byte>; cdecl;
    {class} function GerarLinha(P1: Integer; P2: Integer; P3: Integer; P4: Integer): TJavaArray<Byte>; cdecl;
    {class} function GerarMaxiCode(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: Integer; P8: JString): TJavaArray<Byte>; cdecl;
    {class} function GerarPDF417(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: Integer; P8: Integer; P9: Integer; P10: Integer; P11: JString): TJavaArray<Byte>; cdecl;
    {class} function GerarQRCodeAuto(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: JString): TJavaArray<Byte>; cdecl;
    {class} function GerarQRCodeManual(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: Integer; P8: Integer; P9: Integer; P10: Integer; P11: JString): TJavaArray<Byte>; cdecl;
    {class} function GerarTexto(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: JString): TJavaArray<Byte>; cdecl;
    {class} function GerarTextoASD(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: JString): TJavaArray<Byte>; cdecl;
    {class} function GerarTextoCourier(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: JString): TJavaArray<Byte>; cdecl;
    {class} function GetVersaoDLL: TJavaArray<Byte>; cdecl;
    {class} function Imprime(P1: Integer; P2: JString; P3: JString; P4: Integer): TJavaArray<Byte>; cdecl;
    {class} function Limpa(P1: Integer): TJavaArray<Byte>; cdecl;
    {class} function LimpaMemoria(P1: Integer; P2: JString; P3: JString; P4: Integer): TJavaArray<Byte>; cdecl;
    {class} function LimpaModulo(P1: Integer; P2: JString; P3: JString; P4: Integer; P5: Integer): TJavaArray<Byte>; cdecl;
    {class} function MemoryStatus(P1: Integer; P2: JString; P3: JString; P4: Integer; P5: Integer): TJavaArray<Byte>; cdecl;
    {class} function Reset(P1: Integer; P2: JString; P3: JString; P4: Integer): TJavaArray<Byte>; cdecl;
    {class} function SetAlturaGap(P1: Integer; P2: JString; P3: JString; P4: Integer; P5: Integer): TJavaArray<Byte>; cdecl;
    {class} function SetBackfeed(P1: Integer): TJavaArray<Byte>; cdecl;
    {class} function SetBaudrate(P1: Integer; P2: JString; P3: JString; P4: Integer; P5: Integer; P6: Integer; P7: Integer; P8: Integer): TJavaArray<Byte>; cdecl;
    {class} function SetCalor(P1: Integer): TJavaArray<Byte>; cdecl;
    {class} function SetCortarZero(P1: Integer): TJavaArray<Byte>; cdecl;
    {class} function SetLength(P1: Integer): TJavaArray<Byte>; cdecl;
    {class} function SetLogicImgMode(P1: Integer): TJavaArray<Byte>; cdecl;
    {class} function SetMedidas(P1: Integer): TJavaArray<Byte>; cdecl;
    {class} function SetMirror(P1: Integer): TJavaArray<Byte>; cdecl;
    {class} function SetModoContinuo(P1: Integer): TJavaArray<Byte>; cdecl;
    {class} function SetOffsetColuna(P1: Integer): TJavaArray<Byte>; cdecl;
    {class} function SetOffsetLinha(P1: Integer): TJavaArray<Byte>; cdecl;
    {class} function SetPrintStPos(P1: Integer): TJavaArray<Byte>; cdecl;
    {class} function SetQtde(P1: Integer): TJavaArray<Byte>; cdecl;
    {class} function SetSensor(P1: Integer): TJavaArray<Byte>; cdecl;
    {class} function SetSymbolASD(P1: Integer; P2: JString; P3: JString; P4: Integer; P5: Integer): TJavaArray<Byte>; cdecl;
    {class} function SetTamPixel(P1: Integer; P2: Integer): TJavaArray<Byte>; cdecl;
    {class} function SetTipoTransferencia(P1: Integer; P2: JString; P3: JString; P4: Integer; P5: Integer): TJavaArray<Byte>; cdecl;
    {class} function SetVelImpressao(P1: Integer): TJavaArray<Byte>; cdecl;
    {class} function Status(P1: Integer; P2: JString; P3: JString; P4: Integer): TJavaArray<Byte>; cdecl;
    {class} function StatusEPL(P1: Integer; P2: JString; P3: JString; P4: Integer): TJavaArray<Byte>; cdecl;
    {class} function Teste(P1: Integer; P2: JString; P3: JString; P4: Integer): TJavaArray<Byte>; cdecl;
    {class} function init: JServicoE1_Etiqueta; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Servico/ServicoE1$Etiqueta')]
  JServicoE1_Etiqueta = interface(JObject)
    ['{A1CE523F-5FA6-4AB3-96BD-8EED4DA53A1A}']
  end;
  TJServicoE1_Etiqueta = class(TJavaGenericImport<JServicoE1_EtiquetaClass, JServicoE1_Etiqueta>) end;

  JServicoE1_SATClass = interface(JObjectClass)
    ['{B4657816-9D3C-4CBA-B03C-DCF7A24FC629}']
    {class} function AssociarAssinatura(P1: Integer; P2: JString; P3: JString; P4: JString): TJavaArray<Byte>; cdecl;
    {class} function AtivarSAT(P1: Integer; P2: Integer; P3: JString; P4: JString; P5: Integer): TJavaArray<Byte>; cdecl;
    {class} function AtualizarSoftwareSAT(P1: Integer; P2: JString): TJavaArray<Byte>; cdecl;
    {class} function BloquearSAT(P1: Integer; P2: JString): TJavaArray<Byte>; cdecl;
    {class} function CancelaVendaImpressaSAT(P1: Integer; P2: JString; P3: JString; P4: Integer; P5: JString; P6: JString): TJavaArray<Byte>; cdecl;
    {class} function CancelarUltimaVenda(P1: Integer; P2: JString; P3: JString; P4: JString): TJavaArray<Byte>; cdecl;
    {class} function ConfigurarInterfaceDeRede(P1: Integer; P2: JString; P3: JString): TJavaArray<Byte>; cdecl;
    {class} function ConsultarNumeroSessao(P1: Integer; P2: JString; P3: Integer): TJavaArray<Byte>; cdecl;
    {class} function ConsultarSat(P1: Integer): TJavaArray<Byte>; cdecl;
    {class} function ConsultarStatusEspecifico(P1: Integer; P2: Integer; P3: JString): TJavaArray<Byte>; cdecl;
    {class} function ConsultarStatusOperacional(P1: Integer; P2: JString): TJavaArray<Byte>; cdecl;
    {class} function ConsultarUltimaSessaoFiscal(P1: Integer; P2: JString): TJavaArray<Byte>; cdecl;
    {class} function CriaXMLCancelamentoSAT(P1: JString; P2: JString; P3: JString): TJavaArray<Byte>; cdecl;
    {class} function DecodificaBase64(P1: JString): TJavaArray<Byte>; cdecl;
    {class} function DesbloquearSAT(P1: Integer; P2: JString): TJavaArray<Byte>; cdecl;
    {class} function EnviarDadosVenda(P1: Integer; P2: JString; P3: JString): TJavaArray<Byte>; cdecl;
    {class} function ExtrairLogs(P1: Integer; P2: JString): TJavaArray<Byte>; cdecl;
    {class} function GetVersaoDLL: TJavaArray<Byte>; cdecl;
    {class} function TesteFimAFim(P1: Integer; P2: JString; P3: JString): TJavaArray<Byte>; cdecl;
    {class} function TrocarCodigoDeAtivacao(P1: Integer; P2: JString; P3: Integer; P4: JString; P5: JString): TJavaArray<Byte>; cdecl;
    {class} function VendaImpressaSAT(P1: Integer; P2: JString; P3: JString; P4: Integer; P5: Integer; P6: JString; P7: JString): TJavaArray<Byte>; cdecl;
    {class} function init: JServicoE1_SAT; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Servico/ServicoE1$SAT')]
  JServicoE1_SAT = interface(JObject)
    ['{47E9AD8D-5ADB-49AF-A3E5-B6172D348611}']
  end;
  TJServicoE1_SAT = class(TJavaGenericImport<JServicoE1_SATClass, JServicoE1_SAT>) end;

  JServicoE1_TermicaClass = interface(JObjectClass)
    ['{77AB7193-0C10-4E83-BC3C-FC1A1F4CD7C9}']
    {class} function AbreConexaoImpressora(P1: Integer; P2: JString; P3: JString; P4: Integer): TJavaArray<Byte>; cdecl;
    {class} function AbreGaveta(P1: Integer; P2: Integer; P3: Integer): TJavaArray<Byte>; cdecl;
    {class} function AbreGavetaElgin: TJavaArray<Byte>; cdecl;
    {class} function AvancaPapel(P1: Integer): TJavaArray<Byte>; cdecl;
    {class} function Corte(P1: Integer): TJavaArray<Byte>; cdecl;
    {class} function DefineAreaImpressao(P1: Integer; P2: Integer; P3: Integer; P4: Integer): TJavaArray<Byte>; cdecl;
    {class} function DefinePosicao(P1: Integer): TJavaArray<Byte>; cdecl;
    {class} function DirecaoImpressao(P1: Integer): TJavaArray<Byte>; cdecl;
    {class} function FechaConexaoImpressora: TJavaArray<Byte>; cdecl;
    {class} function GetVersaoDLL: TJavaArray<Byte>; cdecl;
    {class} function ImpressaoCodigoBarras(P1: Integer; P2: JString; P3: Integer; P4: Integer; P5: Integer): TJavaArray<Byte>; cdecl;
    {class} function ImpressaoPDF417(P1: Integer; P2: Integer; P3: Integer; P4: Integer; P5: Integer; P6: Integer; P7: JString): TJavaArray<Byte>; cdecl;
    {class} function ImpressaoQRCode(P1: JString; P2: Integer; P3: Integer): TJavaArray<Byte>; cdecl;
    {class} function ImpressaoTexto(P1: JString; P2: Integer; P3: Integer; P4: Integer): TJavaArray<Byte>; cdecl;
    {class} function ImprimeImagemMemoria(P1: JString; P2: Integer): TJavaArray<Byte>; cdecl;
    {class} function ImprimeMPeRetornaPadrao: TJavaArray<Byte>; cdecl;
    {class} function ImprimeModoPagina: TJavaArray<Byte>; cdecl;
    {class} function ImprimeXMLCancelamentoSAT(P1: JString; P2: JString; P3: Integer): TJavaArray<Byte>; cdecl;
    {class} function ImprimeXMLNFCe(P1: JString; P2: Integer; P3: JString; P4: Integer): TJavaArray<Byte>; cdecl;
    {class} function ImprimeXMLSAT(P1: JString; P2: Integer): TJavaArray<Byte>; cdecl;
    {class} function InicializaImpressora: TJavaArray<Byte>; cdecl;
    {class} function LimpaBufferModoPagina: TJavaArray<Byte>; cdecl;
    {class} function ModoPadrao: TJavaArray<Byte>; cdecl;
    {class} function ModoPagina: TJavaArray<Byte>; cdecl;
    {class} function PosicaoImpressaoHorizontal(P1: Integer): TJavaArray<Byte>; cdecl;
    {class} function PosicaoImpressaoVertical(P1: Integer): TJavaArray<Byte>; cdecl;
    {class} function SinalSonoro(P1: Integer; P2: Integer; P3: Integer): TJavaArray<Byte>; cdecl;
    {class} function StatusImpressora(P1: Integer): TJavaArray<Byte>; cdecl;
    {class} function init: JServicoE1_Termica; cdecl;
  end;

  [JavaSignature('com/elgin/e1/Servico/ServicoE1$Termica')]
  JServicoE1_Termica = interface(JObject)
    ['{226E51EF-5505-451E-A754-B2CA67FCD16F}']
  end;
  TJServicoE1_Termica = class(TJavaGenericImport<JServicoE1_TermicaClass, JServicoE1_Termica>) end;

  Jminipdvm8_BuildConfigClass = interface(JObjectClass)
    ['{0F39CECD-02AC-4537-B3F5-4E985616C4B7}']
    {class} function _GetAPPLICATION_ID: JString; cdecl;
    {class} function _GetBUILD_TYPE: JString; cdecl;
    {class} function _GetDEBUG: Boolean; cdecl;
    {class} function _GetFLAVOR: JString; cdecl;
    {class} function _GetVERSION_CODE: Integer; cdecl;
    {class} function _GetVERSION_NAME: JString; cdecl;
    {class} function init: Jminipdvm8_BuildConfig; cdecl;
    {class} property APPLICATION_ID: JString read _GetAPPLICATION_ID;
    {class} property BUILD_TYPE: JString read _GetBUILD_TYPE;
    {class} property DEBUG: Boolean read _GetDEBUG;
    {class} property FLAVOR: JString read _GetFLAVOR;
    {class} property VERSION_CODE: Integer read _GetVERSION_CODE;
    {class} property VERSION_NAME: JString read _GetVERSION_NAME;
  end;

  [JavaSignature('com/elgin/minipdvm8/BuildConfig')]
  Jminipdvm8_BuildConfig = interface(JObject)
    ['{2AFA78F9-2659-4CBB-8F57-C93C9AD3A3A0}']
  end;
  TJminipdvm8_BuildConfig = class(TJavaGenericImport<Jminipdvm8_BuildConfigClass, Jminipdvm8_BuildConfig>) end;

  JPrinterManagerClass = interface(JObjectClass)
    ['{E4780880-41A9-4F44-AAD8-D002D017B66C}']
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
    {class} function _GetsTotalLength: Int64; cdecl;
    {class} function init(P1: JActivity; P2: JPrinterManager_PrinterManagerListener): JPrinterManager; cdecl;
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
    {class} property sCurrentLength: Int64 read _GetsCurrentLength;
    {class} property sTotalLength: Int64 read _GetsTotalLength;
  end;

  [JavaSignature('com/elgin/minipdvm8/PrinterManager')]
  JPrinterManager = interface(JObject)
    ['{B7FC2F32-4D46-4F38-823B-B2647711F86B}']
    function getBootloaderVersion: JString; cdecl;
    function getFirmwareVersion: JString; cdecl;
    function hasXChengPrinter(P1: JContext): Boolean; cdecl;
    procedure onPrinterStart; cdecl;
    procedure onPrinterStop(P1: Boolean); cdecl;
    procedure printBarCode(P1: JString; P2: Integer; P3: Integer; P4: Integer; P5: Boolean); cdecl;
    procedure printBitmap(P1: JBitmap); cdecl; overload;
    procedure printBitmap(P1: JBitmap; P2: JMap); cdecl; overload;
    procedure printColumnsTextWithAttributes(P1: TJavaObjectArray<JString>; P2: JList); cdecl;
    procedure printQRCode(P1: JString; P2: Integer; P3: Integer); cdecl;
    procedure printText(P1: JString); cdecl;
    procedure printTextWithAttributes(P1: JString; P2: JMap); cdecl;
    procedure printWrapPaper(P1: Integer); cdecl;
    procedure printerInit; cdecl;
    function printerPaper: Boolean; cdecl;
    procedure printerReset; cdecl;
    function printerTemperature(P1: JActivity): Integer; cdecl;
    procedure sendRAWData(P1: TJavaArray<Byte>); cdecl;
    procedure setPrinterSpeed(P1: Integer); cdecl;
    procedure upgradePrinter; cdecl;
  end;
  TJPrinterManager = class(TJavaGenericImport<JPrinterManagerClass, JPrinterManager>) end;

  JPrinterManager_1Class = interface(JServiceConnectionClass)
    ['{7BBABDF3-85A9-474B-8691-2CC149F2C07A}']
    {class} function init(P1: JPrinterManager): JPrinterManager_1; cdecl;
  end;

  [JavaSignature('com/elgin/minipdvm8/PrinterManager$1')]
  JPrinterManager_1 = interface(JServiceConnection)
    ['{67AD35C2-7525-4AA4-88C2-5A64D8045B89}']
    procedure onServiceConnected(P1: JComponentName; P2: JIBinder); cdecl;
    procedure onServiceDisconnected(P1: JComponentName); cdecl;
  end;
  TJPrinterManager_1 = class(TJavaGenericImport<JPrinterManager_1Class, JPrinterManager_1>) end;

  JIPrinterCallback_StubClass = interface(JBinderClass)
    ['{115BDF7F-DB8D-4B8D-B4D5-9CC0B12AED10}']
    {class} function _GetTRANSACTION_onComplete: Integer; cdecl;
    {class} function _GetTRANSACTION_onLength: Integer; cdecl;
    {class} function asInterface(P1: JIBinder): JIPrinterCallback; cdecl;
    {class} function init: JIPrinterCallback_Stub; cdecl;
    {class} property TRANSACTION_onComplete: Integer read _GetTRANSACTION_onComplete;
    {class} property TRANSACTION_onLength: Integer read _GetTRANSACTION_onLength;
  end;

  [JavaSignature('com/xcheng/printerservice/IPrinterCallback$Stub')]
  JIPrinterCallback_Stub = interface(JBinder)
    ['{044BD249-9511-4D41-9737-9CD2FF2BC233}']
    function asBinder: JIBinder; cdecl;
    function onTransact(P1: Integer; P2: JParcel; P3: JParcel; P4: Integer): Boolean; cdecl;
  end;
  TJIPrinterCallback_Stub = class(TJavaGenericImport<JIPrinterCallback_StubClass, JIPrinterCallback_Stub>) end;

  JPrinterManager_2Class = interface(JIPrinterCallback_StubClass)
    ['{F0ADC62C-11E5-490E-A827-02E1DAEFA919}']
    {class} function init(P1: JPrinterManager): JPrinterManager_2; cdecl;
  end;

  [JavaSignature('com/elgin/minipdvm8/PrinterManager$2')]
  JPrinterManager_2 = interface(JIPrinterCallback_Stub)
    ['{1EE04B4A-6263-4833-B4D9-EB02C72B6A94}']
    procedure onComplete; cdecl;
    procedure onException(P1: Integer; P2: JString); cdecl;
    procedure onLength(P1: Int64; P2: Int64); cdecl;
  end;
  TJPrinterManager_2 = class(TJavaGenericImport<JPrinterManager_2Class, JPrinterManager_2>) end;

  JCommSerialAPIClass = interface(JObjectClass)
    ['{3C298CA3-8A31-483C-A1DB-A04DA535EB3C}']
    {class} function comPortClose(P1: Integer): Integer; cdecl;
    {class} function comPortOpen(P1: JString; P2: Integer; P3: Integer; P4: Char; P5: Integer): Integer; cdecl;
    {class} function comPortRead(P1: Integer): JString; cdecl;
    {class} function comPortWrite(P1: JString; P2: Integer): Boolean; cdecl;
    {class} function init: JCommSerialAPI; cdecl;
  end;

  [JavaSignature('com/xc/comportdemo/CommSerialAPI')]
  JCommSerialAPI = interface(JObject)
    ['{1A581D32-5003-419D-B247-31C6D998CA93}']
  end;
  TJCommSerialAPI = class(TJavaGenericImport<JCommSerialAPIClass, JCommSerialAPI>) end;

  JComportNativeClass = interface(JObjectClass)
    ['{D8FEE37A-9791-4680-B0CB-12E40FD4C288}']
  end;

  [JavaSignature('com/xc/comportdemo/ComportNative')]
  JComportNative = interface(JObject)
    ['{0B641E41-E6D7-4A4B-BD1B-91310744505A}']
  end;
  TJComportNative = class(TJavaGenericImport<JComportNativeClass, JComportNative>) end;

  JIPrinterCallbackClass = interface(JIInterfaceClass)
    ['{721338DA-C5B4-4EB4-B030-9CC58F430208}']
    {class} procedure onComplete; cdecl;
    {class} procedure onException(P1: Integer; P2: JString); cdecl;
    {class} procedure onLength(P1: Int64; P2: Int64); cdecl;
  end;

  [JavaSignature('com/xcheng/printerservice/IPrinterCallback')]
  JIPrinterCallback = interface(JIInterface)
    ['{5E7AB2EB-9386-4D24-953F-F73FEE945CE0}']
  end;
  TJIPrinterCallback = class(TJavaGenericImport<JIPrinterCallbackClass, JIPrinterCallback>) end;

  JIPrinterCallback_Stub_ProxyClass = interface(JIPrinterCallbackClass)
    ['{1B190C75-D8D7-4F35-82F1-536180E212E9}']
    {class} function asBinder: JIBinder; cdecl;
    {class} function getInterfaceDescriptor: JString; cdecl;
    {class} procedure onComplete; cdecl;
    {class} procedure onException(P1: Integer; P2: JString); cdecl;
    {class} procedure onLength(P1: Int64; P2: Int64); cdecl;
  end;

  [JavaSignature('com/xcheng/printerservice/IPrinterCallback$Stub$Proxy')]
  JIPrinterCallback_Stub_Proxy = interface(JIPrinterCallback)
    ['{BFA29B44-41AC-4E81-AC46-A699316F56E6}']
  end;
  TJIPrinterCallback_Stub_Proxy = class(TJavaGenericImport<JIPrinterCallback_Stub_ProxyClass, JIPrinterCallback_Stub_Proxy>) end;

  JIPrinterServiceClass = interface(JIInterfaceClass)
    ['{EAC3E632-E8A0-4A07-9ADF-E4C040BE63AE}']
    {class} function getBootloaderVersion: JString; cdecl;
    {class} function getFirmwareVersion: JString; cdecl;
    {class} procedure printBarCode(P1: JString; P2: Integer; P3: Integer; P4: Integer; P5: Boolean; P6: JIPrinterCallback); cdecl;
    {class} procedure printBitmap(P1: JBitmap; P2: JIPrinterCallback); cdecl;
    {class} procedure printBitmapWithAttributes(P1: JBitmap; P2: JMap; P3: JIPrinterCallback); cdecl;
    {class} procedure printColumnsTextWithAttributes(P1: TJavaObjectArray<JString>; P2: JList; P3: JIPrinterCallback); cdecl;
    {class} procedure printQRCode(P1: JString; P2: Integer; P3: Integer; P4: JIPrinterCallback); cdecl;
    {class} procedure printText(P1: JString; P2: JIPrinterCallback); cdecl;
    {class} procedure printTextWithAttributes(P1: JString; P2: JMap; P3: JIPrinterCallback); cdecl;
    {class} procedure printWrapPaper(P1: Integer; P2: JIPrinterCallback); cdecl;
    {class} procedure printerInit(P1: JIPrinterCallback); cdecl;
    {class} function printerPaper(P1: JIPrinterCallback): Boolean; cdecl;
    {class} procedure printerReset(P1: JIPrinterCallback); cdecl;
    {class} function printerTemperature(P1: JIPrinterCallback): Integer; cdecl;
    {class} procedure sendRAWData(P1: TJavaArray<Byte>; P2: JIPrinterCallback); cdecl;
    {class} procedure setPrinterSpeed(P1: Integer; P2: JIPrinterCallback); cdecl;
    {class} procedure upgradePrinter; cdecl;
  end;

  [JavaSignature('com/xcheng/printerservice/IPrinterService')]
  JIPrinterService = interface(JIInterface)
    ['{EA0657FE-BC0D-470B-B39C-578314DF6BDA}']
  end;
  TJIPrinterService = class(TJavaGenericImport<JIPrinterServiceClass, JIPrinterService>) end;

  JIPrinterService_StubClass = interface(JBinderClass)
    ['{AD34622C-9845-4F87-B33F-C7FB5BB55F8E}']
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
    {class} function asInterface(P1: JIBinder): JIPrinterService; cdecl;
    {class} function init: JIPrinterService_Stub; cdecl;
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
    ['{05D27E6F-7DA1-4A81-B563-FE6C3D3984AA}']
    function asBinder: JIBinder; cdecl;
    function onTransact(P1: Integer; P2: JParcel; P3: JParcel; P4: Integer): Boolean; cdecl;
  end;
  TJIPrinterService_Stub = class(TJavaGenericImport<JIPrinterService_StubClass, JIPrinterService_Stub>) end;

  JIPrinterService_Stub_ProxyClass = interface(JIPrinterServiceClass)
    ['{B28D36E9-D2C4-4827-99D2-CA75A5B06A7E}']
    {class} function asBinder: JIBinder; cdecl;
    {class} function getBootloaderVersion: JString; cdecl;
    {class} function getFirmwareVersion: JString; cdecl;
    {class} function getInterfaceDescriptor: JString; cdecl;
    {class} procedure printBarCode(P1: JString; P2: Integer; P3: Integer; P4: Integer; P5: Boolean; P6: JIPrinterCallback); cdecl;
    {class} procedure printBitmap(P1: JBitmap; P2: JIPrinterCallback); cdecl;
    {class} procedure printBitmapWithAttributes(P1: JBitmap; P2: JMap; P3: JIPrinterCallback); cdecl;
    {class} procedure printColumnsTextWithAttributes(P1: TJavaObjectArray<JString>; P2: JList; P3: JIPrinterCallback); cdecl;
    {class} procedure printQRCode(P1: JString; P2: Integer; P3: Integer; P4: JIPrinterCallback); cdecl;
    {class} procedure printText(P1: JString; P2: JIPrinterCallback); cdecl;
    {class} procedure printTextWithAttributes(P1: JString; P2: JMap; P3: JIPrinterCallback); cdecl;
    {class} procedure printWrapPaper(P1: Integer; P2: JIPrinterCallback); cdecl;
    {class} procedure printerInit(P1: JIPrinterCallback); cdecl;
    {class} function printerPaper(P1: JIPrinterCallback): Boolean; cdecl;
    {class} procedure printerReset(P1: JIPrinterCallback); cdecl;
    {class} function printerTemperature(P1: JIPrinterCallback): Integer; cdecl;
    {class} procedure sendRAWData(P1: TJavaArray<Byte>; P2: JIPrinterCallback); cdecl;
    {class} procedure setPrinterSpeed(P1: Integer; P2: JIPrinterCallback); cdecl;
    {class} procedure upgradePrinter; cdecl;
  end;

  [JavaSignature('com/xcheng/printerservice/IPrinterService$Stub$Proxy')]
  JIPrinterService_Stub_Proxy = interface(JIPrinterService)
    ['{6A69E699-F04C-42AC-8033-72816588003F}']
  end;
  TJIPrinterService_Stub_Proxy = class(TJavaGenericImport<JIPrinterService_Stub_ProxyClass, JIPrinterService_Stub_Proxy>) end;

  JNodeClass = interface(IJavaClass)
    ['{4FF9B265-CEE8-4AB9-B74A-5F2D9CED8981}']
    {class} function _GetATTRIBUTE_NODE: SmallInt; cdecl;
    {class} function _GetCDATA_SECTION_NODE: SmallInt; cdecl;
    {class} function _GetCOMMENT_NODE: SmallInt; cdecl;
    {class} function _GetDOCUMENT_FRAGMENT_NODE: SmallInt; cdecl;
    {class} function _GetDOCUMENT_NODE: SmallInt; cdecl;
    {class} function _GetDOCUMENT_POSITION_CONTAINED_BY: SmallInt; cdecl;
    {class} function _GetDOCUMENT_POSITION_CONTAINS: SmallInt; cdecl;
    {class} function _GetDOCUMENT_POSITION_DISCONNECTED: SmallInt; cdecl;
    {class} function _GetDOCUMENT_POSITION_FOLLOWING: SmallInt; cdecl;
    {class} function _GetDOCUMENT_POSITION_IMPLEMENTATION_SPECIFIC: SmallInt; cdecl;
    {class} function _GetDOCUMENT_POSITION_PRECEDING: SmallInt; cdecl;
    {class} function _GetDOCUMENT_TYPE_NODE: SmallInt; cdecl;
    {class} function _GetELEMENT_NODE: SmallInt; cdecl;
    {class} function _GetENTITY_NODE: SmallInt; cdecl;
    {class} function _GetENTITY_REFERENCE_NODE: SmallInt; cdecl;
    {class} function _GetNOTATION_NODE: SmallInt; cdecl;
    {class} function _GetPROCESSING_INSTRUCTION_NODE: SmallInt; cdecl;
    {class} function _GetTEXT_NODE: SmallInt; cdecl;
    {class} function cloneNode(deep: Boolean): JNode; cdecl;//Deprecated
    {class} function compareDocumentPosition(other: JNode): SmallInt; cdecl;//Deprecated
    {class} function getAttributes: JNamedNodeMap; cdecl;//Deprecated
    {class} function getFirstChild: JNode; cdecl;//Deprecated
    {class} function getLastChild: JNode; cdecl;//Deprecated
    {class} function getLocalName: JString; cdecl;//Deprecated
    {class} function getNodeType: SmallInt; cdecl;//Deprecated
    {class} function getNodeValue: JString; cdecl;//Deprecated
    {class} function getOwnerDocument: JDocument; cdecl;//Deprecated
    {class} function getTextContent: JString; cdecl;
    {class} function getUserData(key: JString): JObject; cdecl;
    {class} function hasAttributes: Boolean; cdecl;
    {class} function isDefaultNamespace(namespaceURI: JString): Boolean; cdecl;
    {class} function isEqualNode(arg: JNode): Boolean; cdecl;
    {class} function isSameNode(other: JNode): Boolean; cdecl;
    {class} procedure normalize; cdecl;
    {class} function removeChild(oldChild: JNode): JNode; cdecl;
    {class} function replaceChild(newChild: JNode; oldChild: JNode): JNode; cdecl;
    {class} function setUserData(key: JString; data: JObject; handler: JUserDataHandler): JObject; cdecl;
    {class} property ATTRIBUTE_NODE: SmallInt read _GetATTRIBUTE_NODE;
    {class} property CDATA_SECTION_NODE: SmallInt read _GetCDATA_SECTION_NODE;
    {class} property COMMENT_NODE: SmallInt read _GetCOMMENT_NODE;
    {class} property DOCUMENT_FRAGMENT_NODE: SmallInt read _GetDOCUMENT_FRAGMENT_NODE;
    {class} property DOCUMENT_NODE: SmallInt read _GetDOCUMENT_NODE;
    {class} property DOCUMENT_POSITION_CONTAINED_BY: SmallInt read _GetDOCUMENT_POSITION_CONTAINED_BY;
    {class} property DOCUMENT_POSITION_CONTAINS: SmallInt read _GetDOCUMENT_POSITION_CONTAINS;
    {class} property DOCUMENT_POSITION_DISCONNECTED: SmallInt read _GetDOCUMENT_POSITION_DISCONNECTED;
    {class} property DOCUMENT_POSITION_FOLLOWING: SmallInt read _GetDOCUMENT_POSITION_FOLLOWING;
    {class} property DOCUMENT_POSITION_IMPLEMENTATION_SPECIFIC: SmallInt read _GetDOCUMENT_POSITION_IMPLEMENTATION_SPECIFIC;
    {class} property DOCUMENT_POSITION_PRECEDING: SmallInt read _GetDOCUMENT_POSITION_PRECEDING;
    {class} property DOCUMENT_TYPE_NODE: SmallInt read _GetDOCUMENT_TYPE_NODE;
    {class} property ELEMENT_NODE: SmallInt read _GetELEMENT_NODE;
    {class} property ENTITY_NODE: SmallInt read _GetENTITY_NODE;
    {class} property ENTITY_REFERENCE_NODE: SmallInt read _GetENTITY_REFERENCE_NODE;
    {class} property NOTATION_NODE: SmallInt read _GetNOTATION_NODE;
    {class} property PROCESSING_INSTRUCTION_NODE: SmallInt read _GetPROCESSING_INSTRUCTION_NODE;
    {class} property TEXT_NODE: SmallInt read _GetTEXT_NODE;
  end;

  [JavaSignature('org/w3c/dom/Node')]
  JNode = interface(IJavaInstance)
    ['{35CFF397-04C8-489D-9C62-607EFFA8B051}']
    function appendChild(newChild: JNode): JNode; cdecl;//Deprecated
    function getBaseURI: JString; cdecl;//Deprecated
    function getChildNodes: JNodeList; cdecl;//Deprecated
    function getFeature(feature: JString; version: JString): JObject; cdecl;//Deprecated
    function getNamespaceURI: JString; cdecl;//Deprecated
    function getNextSibling: JNode; cdecl;//Deprecated
    function getNodeName: JString; cdecl;//Deprecated
    function getParentNode: JNode; cdecl;
    function getPrefix: JString; cdecl;
    function getPreviousSibling: JNode; cdecl;
    function hasChildNodes: Boolean; cdecl;
    function insertBefore(newChild: JNode; refChild: JNode): JNode; cdecl;
    function isSupported(feature: JString; version: JString): Boolean; cdecl;
    function lookupNamespaceURI(prefix: JString): JString; cdecl;
    function lookupPrefix(namespaceURI: JString): JString; cdecl;
    procedure setNodeValue(nodeValue: JString); cdecl;
    procedure setPrefix(prefix: JString); cdecl;
    procedure setTextContent(textContent: JString); cdecl;
  end;
  TJNode = class(TJavaGenericImport<JNodeClass, JNode>) end;

  JAttrClass = interface(JNodeClass)
    ['{5FB044B8-0031-4520-B87A-3CDB994277D7}']
    {class} function getName: JString; cdecl;
    {class} function getOwnerElement: JElement; cdecl;
    {class} function getSchemaTypeInfo: JTypeInfo; cdecl;
    {class} procedure setValue(value: JString); cdecl;
  end;

  [JavaSignature('org/w3c/dom/Attr')]
  JAttr = interface(JNode)
    ['{F9FC2FA5-CCAD-4D11-8B8D-3958C5F55273}']
    function getSpecified: Boolean; cdecl;
    function getValue: JString; cdecl;
    function isId: Boolean; cdecl;
  end;
  TJAttr = class(TJavaGenericImport<JAttrClass, JAttr>) end;

  JCharacterDataClass = interface(JNodeClass)
    ['{2C17F389-87C1-444E-957E-9F54C1531B5A}']
    {class} procedure deleteData(offset: Integer; count: Integer); cdecl;
    {class} function getData: JString; cdecl;
    {class} procedure setData(data: JString); cdecl;//Deprecated
    {class} function substringData(offset: Integer; count: Integer): JString; cdecl;//Deprecated
  end;

  [JavaSignature('org/w3c/dom/CharacterData')]
  JCharacterData = interface(JNode)
    ['{10B18FAD-C168-4834-9BF9-996C53B31D9E}']
    procedure appendData(arg: JString); cdecl;
    function getLength: Integer; cdecl;//Deprecated
    procedure insertData(offset: Integer; arg: JString); cdecl;//Deprecated
    procedure replaceData(offset: Integer; count: Integer; arg: JString); cdecl;//Deprecated
  end;
  TJCharacterData = class(TJavaGenericImport<JCharacterDataClass, JCharacterData>) end;

  JTextClass = interface(JCharacterDataClass)
    ['{A1698F81-D2B1-4131-A464-3E6A6ADD1D56}']
    {class} function getWholeText: JString; cdecl;//Deprecated
  end;

  [JavaSignature('org/w3c/dom/Text')]
  JText = interface(JCharacterData)
    ['{FAE4042A-1DDA-4B7D-BCFC-3C629B2818A4}']
    function isElementContentWhitespace: Boolean; cdecl;//Deprecated
    function replaceWholeText(content: JString): JText; cdecl;//Deprecated
    function splitText(offset: Integer): JText; cdecl;//Deprecated
  end;
  TJText = class(TJavaGenericImport<JTextClass, JText>) end;

  JCDATASectionClass = interface(JTextClass)
    ['{7CBFD045-12AF-4D98-A080-469281E3B4DA}']
  end;

  [JavaSignature('org/w3c/dom/CDATASection')]
  JCDATASection = interface(JText)
    ['{58B470DE-13D3-4B4A-A9B7-5F03E4D4CD75}']
  end;
  TJCDATASection = class(TJavaGenericImport<JCDATASectionClass, JCDATASection>) end;

  JCommentClass = interface(JCharacterDataClass)
    ['{44667247-E701-4F4A-A4A6-1B2C2249BD0E}']
  end;

  [JavaSignature('org/w3c/dom/Comment')]
  JComment = interface(JCharacterData)
    ['{4E3A4920-FC80-4A44-A6A8-DBCC5D94A473}']
  end;
  TJComment = class(TJavaGenericImport<JCommentClass, JComment>) end;

  JDOMConfigurationClass = interface(IJavaClass)
    ['{DECB79BC-0125-4589-A7F9-4515540AE6A2}']
    {class} procedure setParameter(name: JString; value: JObject); cdecl;
  end;

  [JavaSignature('org/w3c/dom/DOMConfiguration')]
  JDOMConfiguration = interface(IJavaInstance)
    ['{E8ABF7F9-F6D5-41BB-939D-8012C99B087D}']
    function canSetParameter(name: JString; value: JObject): Boolean; cdecl;
    function getParameter(name: JString): JObject; cdecl;
    function getParameterNames: JDOMStringList; cdecl;
  end;
  TJDOMConfiguration = class(TJavaGenericImport<JDOMConfigurationClass, JDOMConfiguration>) end;

  JDOMImplementationClass = interface(IJavaClass)
    ['{B1E4F8D3-F1BD-4F6C-B4ED-0310907DF7A4}']
    {class} function createDocument(namespaceURI: JString; qualifiedName: JString; doctype: JDocumentType): JDocument; cdecl;//Deprecated
    {class} function createDocumentType(qualifiedName: JString; publicId: JString; systemId: JString): JDocumentType; cdecl;//Deprecated
    {class} function getFeature(feature: JString; version: JString): JObject; cdecl;//Deprecated
  end;

  [JavaSignature('org/w3c/dom/DOMImplementation')]
  JDOMImplementation = interface(IJavaInstance)
    ['{B1DCFB4D-AA66-4B31-A161-7E7D420C0BD4}']
    function hasFeature(feature: JString; version: JString): Boolean; cdecl;
  end;
  TJDOMImplementation = class(TJavaGenericImport<JDOMImplementationClass, JDOMImplementation>) end;

  JDOMStringListClass = interface(IJavaClass)
    ['{07E16943-A1B1-457E-B687-6BF0DC8A0B2B}']
    {class} function &contains(str: JString): Boolean; cdecl;//Deprecated
    {class} function getLength: Integer; cdecl;//Deprecated
  end;

  [JavaSignature('org/w3c/dom/DOMStringList')]
  JDOMStringList = interface(IJavaInstance)
    ['{429D640E-1DB3-442F-9ABF-98965BDEF484}']
    function item(index: Integer): JString; cdecl;//Deprecated
  end;
  TJDOMStringList = class(TJavaGenericImport<JDOMStringListClass, JDOMStringList>) end;

  JDocumentClass = interface(JNodeClass)
    ['{D6F13E91-584B-40E3-98D4-A49B673E1FAA}']
    {class} function adoptNode(source: JNode): JNode; cdecl;//Deprecated
    {class} function createAttribute(name: JString): JAttr; cdecl;//Deprecated
    {class} function createDocumentFragment: JDocumentFragment; cdecl;//Deprecated
    {class} function createElement(tagName: JString): JElement; cdecl;//Deprecated
    {class} function createElementNS(namespaceURI: JString; qualifiedName: JString): JElement; cdecl;//Deprecated
    {class} function getDoctype: JDocumentType; cdecl;//Deprecated
    {class} function getDocumentElement: JElement; cdecl;//Deprecated
    {class} function getDocumentURI: JString; cdecl;//Deprecated
    {class} function getElementsByTagNameNS(namespaceURI: JString; localName: JString): JNodeList; cdecl;
    {class} function getImplementation: JDOMImplementation; cdecl;
    {class} function getInputEncoding: JString; cdecl;
    {class} function getXmlVersion: JString; cdecl;
    {class} function importNode(importedNode: JNode; deep: Boolean): JNode; cdecl;
    {class} procedure setStrictErrorChecking(strictErrorChecking: Boolean); cdecl;
    {class} procedure setXmlStandalone(xmlStandalone: Boolean); cdecl;
    {class} procedure setXmlVersion(xmlVersion: JString); cdecl;
  end;

  [JavaSignature('org/w3c/dom/Document')]
  JDocument = interface(JNode)
    ['{A1A54941-AF47-44E3-9987-16699E7D7AE8}']
    function createAttributeNS(namespaceURI: JString; qualifiedName: JString): JAttr; cdecl;//Deprecated
    function createCDATASection(data: JString): JCDATASection; cdecl;//Deprecated
    function createComment(data: JString): JComment; cdecl;//Deprecated
    function createEntityReference(name: JString): JEntityReference; cdecl;//Deprecated
    function createProcessingInstruction(target: JString; data: JString): JProcessingInstruction; cdecl;//Deprecated
    function createTextNode(data: JString): JText; cdecl;//Deprecated
    function getDomConfig: JDOMConfiguration; cdecl;
    function getElementById(elementId: JString): JElement; cdecl;
    function getElementsByTagName(tagname: JString): JNodeList; cdecl;
    function getStrictErrorChecking: Boolean; cdecl;
    function getXmlEncoding: JString; cdecl;
    function getXmlStandalone: Boolean; cdecl;
    procedure normalizeDocument; cdecl;
    function renameNode(n: JNode; namespaceURI: JString; qualifiedName: JString): JNode; cdecl;
    procedure setDocumentURI(documentURI: JString); cdecl;
  end;
  TJDocument = class(TJavaGenericImport<JDocumentClass, JDocument>) end;

  JDocumentFragmentClass = interface(JNodeClass)
    ['{C7329109-13F5-4DAF-9B1D-67C135CA426E}']
  end;

  [JavaSignature('org/w3c/dom/DocumentFragment')]
  JDocumentFragment = interface(JNode)
    ['{58188EC9-6A2F-4B05-B94E-0DF20D5C2214}']
  end;
  TJDocumentFragment = class(TJavaGenericImport<JDocumentFragmentClass, JDocumentFragment>) end;

  JDocumentTypeClass = interface(JNodeClass)
    ['{0A107FFB-8693-4B27-9422-07B0ACCAD242}']
    {class} function getNotations: JNamedNodeMap; cdecl;
    {class} function getPublicId: JString; cdecl;
    {class} function getSystemId: JString; cdecl;
  end;

  [JavaSignature('org/w3c/dom/DocumentType')]
  JDocumentType = interface(JNode)
    ['{CFD608DB-450E-45EA-BEC6-B680E662E816}']
    function getEntities: JNamedNodeMap; cdecl;
    function getInternalSubset: JString; cdecl;
    function getName: JString; cdecl;
  end;
  TJDocumentType = class(TJavaGenericImport<JDocumentTypeClass, JDocumentType>) end;

  JElementClass = interface(JNodeClass)
    ['{02A52262-29B4-4297-859E-FDCE017479D5}']
    {class} function getAttributeNode(name: JString): JAttr; cdecl;
    {class} function getAttributeNodeNS(namespaceURI: JString; localName: JString): JAttr; cdecl;
    {class} function getElementsByTagName(name: JString): JNodeList; cdecl;
    {class} function hasAttribute(name: JString): Boolean; cdecl;//Deprecated
    {class} function hasAttributeNS(namespaceURI: JString; localName: JString): Boolean; cdecl;//Deprecated
    {class} procedure setAttribute(name: JString; value: JString); cdecl;//Deprecated
    {class} procedure setAttributeNS(namespaceURI: JString; qualifiedName: JString; value: JString); cdecl;//Deprecated
    {class} function setAttributeNode(newAttr: JAttr): JAttr; cdecl;//Deprecated
    {class} procedure setIdAttributeNode(idAttr: JAttr; isId: Boolean); cdecl;//Deprecated
  end;

  [JavaSignature('org/w3c/dom/Element')]
  JElement = interface(JNode)
    ['{953C1ADD-28E2-4725-95C9-B2E518AE79F9}']
    function getAttribute(name: JString): JString; cdecl;
    function getAttributeNS(namespaceURI: JString; localName: JString): JString; cdecl;
    function getElementsByTagNameNS(namespaceURI: JString; localName: JString): JNodeList; cdecl;//Deprecated
    function getSchemaTypeInfo: JTypeInfo; cdecl;//Deprecated
    function getTagName: JString; cdecl;//Deprecated
    procedure removeAttribute(name: JString); cdecl;//Deprecated
    procedure removeAttributeNS(namespaceURI: JString; localName: JString); cdecl;//Deprecated
    function removeAttributeNode(oldAttr: JAttr): JAttr; cdecl;//Deprecated
    function setAttributeNodeNS(newAttr: JAttr): JAttr; cdecl;//Deprecated
    procedure setIdAttribute(name: JString; isId: Boolean); cdecl;//Deprecated
    procedure setIdAttributeNS(namespaceURI: JString; localName: JString; isId: Boolean); cdecl;//Deprecated
  end;
  TJElement = class(TJavaGenericImport<JElementClass, JElement>) end;

  JEntityReferenceClass = interface(JNodeClass)
    ['{EDE635BA-9CEC-473C-BF75-163686CB36BB}']
  end;

  [JavaSignature('org/w3c/dom/EntityReference')]
  JEntityReference = interface(JNode)
    ['{47BBF06F-638E-47A8-8BE2-1FC93F0A2067}']
  end;
  TJEntityReference = class(TJavaGenericImport<JEntityReferenceClass, JEntityReference>) end;

  JNamedNodeMapClass = interface(IJavaClass)
    ['{FD563D08-BB75-461A-B13C-8C7DF3E00CC5}']
    {class} function getNamedItem(name: JString): JNode; cdecl;
    {class} function getNamedItemNS(namespaceURI: JString; localName: JString): JNode; cdecl;
    {class} function item(index: Integer): JNode; cdecl;
    {class} function setNamedItemNS(arg: JNode): JNode; cdecl;
  end;

  [JavaSignature('org/w3c/dom/NamedNodeMap')]
  JNamedNodeMap = interface(IJavaInstance)
    ['{92F9509D-82EA-4290-A970-7BB551F08679}']
    function getLength: Integer; cdecl;
    function removeNamedItem(name: JString): JNode; cdecl;
    function removeNamedItemNS(namespaceURI: JString; localName: JString): JNode; cdecl;
    function setNamedItem(arg: JNode): JNode; cdecl;
  end;
  TJNamedNodeMap = class(TJavaGenericImport<JNamedNodeMapClass, JNamedNodeMap>) end;

  JProcessingInstructionClass = interface(JNodeClass)
    ['{9B71FB69-7682-435D-9031-529E15076309}']
    {class} function getData: JString; cdecl;//Deprecated
  end;

  [JavaSignature('org/w3c/dom/ProcessingInstruction')]
  JProcessingInstruction = interface(JNode)
    ['{50F37F5A-E5A2-4190-B19F-820997AF3D4C}']
    function getTarget: JString; cdecl;//Deprecated
    procedure setData(data: JString); cdecl;//Deprecated
  end;
  TJProcessingInstruction = class(TJavaGenericImport<JProcessingInstructionClass, JProcessingInstruction>) end;

  JTypeInfoClass = interface(IJavaClass)
    ['{532BAFC7-6829-43E4-9478-E739B90EE1FC}']
    {class} function _GetDERIVATION_EXTENSION: Integer; cdecl;
    {class} function _GetDERIVATION_LIST: Integer; cdecl;
    {class} function _GetDERIVATION_RESTRICTION: Integer; cdecl;
    {class} function _GetDERIVATION_UNION: Integer; cdecl;
    {class} function isDerivedFrom(typeNamespaceArg: JString; typeNameArg: JString; derivationMethod: Integer): Boolean; cdecl;
    {class} property DERIVATION_EXTENSION: Integer read _GetDERIVATION_EXTENSION;
    {class} property DERIVATION_LIST: Integer read _GetDERIVATION_LIST;
    {class} property DERIVATION_RESTRICTION: Integer read _GetDERIVATION_RESTRICTION;
    {class} property DERIVATION_UNION: Integer read _GetDERIVATION_UNION;
  end;

  [JavaSignature('org/w3c/dom/TypeInfo')]
  JTypeInfo = interface(IJavaInstance)
    ['{876A11F9-8450-45FF-8F4F-F9D68333BDEF}']
    function getTypeName: JString; cdecl;
    function getTypeNamespace: JString; cdecl;
  end;
  TJTypeInfo = class(TJavaGenericImport<JTypeInfoClass, JTypeInfo>) end;

  JUserDataHandlerClass = interface(IJavaClass)
    ['{AD3B738E-675E-4B2E-869D-6888E7959C0B}']
    {class} function _GetNODE_ADOPTED: SmallInt; cdecl;
    {class} function _GetNODE_CLONED: SmallInt; cdecl;
    {class} function _GetNODE_DELETED: SmallInt; cdecl;
    {class} function _GetNODE_IMPORTED: SmallInt; cdecl;
    {class} function _GetNODE_RENAMED: SmallInt; cdecl;
    {class} property NODE_ADOPTED: SmallInt read _GetNODE_ADOPTED;
    {class} property NODE_CLONED: SmallInt read _GetNODE_CLONED;
    {class} property NODE_DELETED: SmallInt read _GetNODE_DELETED;
    {class} property NODE_IMPORTED: SmallInt read _GetNODE_IMPORTED;
    {class} property NODE_RENAMED: SmallInt read _GetNODE_RENAMED;
  end;

  [JavaSignature('org/w3c/dom/UserDataHandler')]
  JUserDataHandler = interface(IJavaInstance)
    ['{F3E555E4-F55C-4228-B9D6-4494A3E32FDF}']
    procedure handle(operation: SmallInt; key: JString; data: JObject; src: JNode; dst: JNode); cdecl;
  end;
  TJUserDataHandler = class(TJavaGenericImport<JUserDataHandlerClass, JUserDataHandler>) end;

implementation

procedure RegisterTypes;
begin
  TRegTypes.RegisterType('Elgin.JNI.E1.JAnimator', TypeInfo(Elgin.JNI.E1.JAnimator));
  TRegTypes.RegisterType('Elgin.JNI.E1.JAnimator_AnimatorListener', TypeInfo(Elgin.JNI.E1.JAnimator_AnimatorListener));
  TRegTypes.RegisterType('Elgin.JNI.E1.JAnimator_AnimatorPauseListener', TypeInfo(Elgin.JNI.E1.JAnimator_AnimatorPauseListener));
  TRegTypes.RegisterType('Elgin.JNI.E1.JKeyframe', TypeInfo(Elgin.JNI.E1.JKeyframe));
  TRegTypes.RegisterType('Elgin.JNI.E1.JLayoutTransition', TypeInfo(Elgin.JNI.E1.JLayoutTransition));
  TRegTypes.RegisterType('Elgin.JNI.E1.JLayoutTransition_TransitionListener', TypeInfo(Elgin.JNI.E1.JLayoutTransition_TransitionListener));
  TRegTypes.RegisterType('Elgin.JNI.E1.JPropertyValuesHolder', TypeInfo(Elgin.JNI.E1.JPropertyValuesHolder));
  TRegTypes.RegisterType('Elgin.JNI.E1.JStateListAnimator', TypeInfo(Elgin.JNI.E1.JStateListAnimator));
  TRegTypes.RegisterType('Elgin.JNI.E1.JTimeInterpolator', TypeInfo(Elgin.JNI.E1.JTimeInterpolator));
  TRegTypes.RegisterType('Elgin.JNI.E1.JTypeConverter', TypeInfo(Elgin.JNI.E1.JTypeConverter));
  TRegTypes.RegisterType('Elgin.JNI.E1.JTypeEvaluator', TypeInfo(Elgin.JNI.E1.JTypeEvaluator));
  TRegTypes.RegisterType('Elgin.JNI.E1.JValueAnimator', TypeInfo(Elgin.JNI.E1.JValueAnimator));
  TRegTypes.RegisterType('Elgin.JNI.E1.JValueAnimator_AnimatorUpdateListener', TypeInfo(Elgin.JNI.E1.JValueAnimator_AnimatorUpdateListener));
  TRegTypes.RegisterType('Elgin.JNI.E1.JPathMotion', TypeInfo(Elgin.JNI.E1.JPathMotion));
  TRegTypes.RegisterType('Elgin.JNI.E1.JScene', TypeInfo(Elgin.JNI.E1.JScene));
  TRegTypes.RegisterType('Elgin.JNI.E1.JTransition', TypeInfo(Elgin.JNI.E1.JTransition));
  TRegTypes.RegisterType('Elgin.JNI.E1.JTransition_EpicenterCallback', TypeInfo(Elgin.JNI.E1.JTransition_EpicenterCallback));
  TRegTypes.RegisterType('Elgin.JNI.E1.JTransition_TransitionListener', TypeInfo(Elgin.JNI.E1.JTransition_TransitionListener));
  TRegTypes.RegisterType('Elgin.JNI.E1.JTransitionManager', TypeInfo(Elgin.JNI.E1.JTransitionManager));
  TRegTypes.RegisterType('Elgin.JNI.E1.JTransitionPropagation', TypeInfo(Elgin.JNI.E1.JTransitionPropagation));
  TRegTypes.RegisterType('Elgin.JNI.E1.JTransitionValues', TypeInfo(Elgin.JNI.E1.JTransitionValues));
  TRegTypes.RegisterType('Elgin.JNI.E1.JInterpolator', TypeInfo(Elgin.JNI.E1.JInterpolator));
  TRegTypes.RegisterType('Elgin.JNI.E1.JToolbar_LayoutParams', TypeInfo(Elgin.JNI.E1.JToolbar_LayoutParams));
  TRegTypes.RegisterType('Elgin.JNI.E1.JBalanca', TypeInfo(Elgin.JNI.E1.JBalanca));
  TRegTypes.RegisterType('Elgin.JNI.E1.JBalanca_Config', TypeInfo(Elgin.JNI.E1.JBalanca_Config));
  TRegTypes.RegisterType('Elgin.JNI.E1.JBalanca_ConfigAltValues', TypeInfo(Elgin.JNI.E1.JBalanca_ConfigAltValues));
  TRegTypes.RegisterType('Elgin.JNI.E1.JBalanca_ModeloBalanca', TypeInfo(Elgin.JNI.E1.JBalanca_ModeloBalanca));
  TRegTypes.RegisterType('Elgin.JNI.E1.JBalanca_ProtocoloComunicacao', TypeInfo(Elgin.JNI.E1.JBalanca_ProtocoloComunicacao));
  TRegTypes.RegisterType('Elgin.JNI.E1.JBalancaE1', TypeInfo(Elgin.JNI.E1.JBalancaE1));
  TRegTypes.RegisterType('Elgin.JNI.E1.JBalancas', TypeInfo(Elgin.JNI.E1.JBalancas));
  TRegTypes.RegisterType('Elgin.JNI.E1.JComm', TypeInfo(Elgin.JNI.E1.JComm));
  TRegTypes.RegisterType('Elgin.JNI.E1.JComm_1', TypeInfo(Elgin.JNI.E1.JComm_1));
  TRegTypes.RegisterType('Elgin.JNI.E1.JComm_TipoConexao', TypeInfo(Elgin.JNI.E1.JComm_TipoConexao));
  TRegTypes.RegisterType('Elgin.JNI.E1.JCommSerial', TypeInfo(Elgin.JNI.E1.JCommSerial));
  TRegTypes.RegisterType('Elgin.JNI.E1.JCommTCP', TypeInfo(Elgin.JNI.E1.JCommTCP));
  TRegTypes.RegisterType('Elgin.JNI.E1.JCommTCP_Timeouts', TypeInfo(Elgin.JNI.E1.JCommTCP_Timeouts));
  TRegTypes.RegisterType('Elgin.JNI.E1.JInterfaceBalanca', TypeInfo(Elgin.JNI.E1.JInterfaceBalanca));
  TRegTypes.RegisterType('Elgin.JNI.E1.JImplementacaoBalanca', TypeInfo(Elgin.JNI.E1.JImplementacaoBalanca));
  TRegTypes.RegisterType('Elgin.JNI.E1.JImplementacaoBalanca_1', TypeInfo(Elgin.JNI.E1.JImplementacaoBalanca_1));
  TRegTypes.RegisterType('Elgin.JNI.E1.Je1_BuildConfig', TypeInfo(Elgin.JNI.E1.Je1_BuildConfig));
  TRegTypes.RegisterType('Elgin.JNI.E1.JConexao', TypeInfo(Elgin.JNI.E1.JConexao));
  TRegTypes.RegisterType('Elgin.JNI.E1.JConBluetooth', TypeInfo(Elgin.JNI.E1.JConBluetooth));
  TRegTypes.RegisterType('Elgin.JNI.E1.JConBluetooth_1GetBluetoothData', TypeInfo(Elgin.JNI.E1.JConBluetooth_1GetBluetoothData));
  TRegTypes.RegisterType('Elgin.JNI.E1.JConBluetooth_1GetPrinterBluetooth', TypeInfo(Elgin.JNI.E1.JConBluetooth_1GetPrinterBluetooth));
  TRegTypes.RegisterType('Elgin.JNI.E1.JConBluetooth_1SendData', TypeInfo(Elgin.JNI.E1.JConBluetooth_1SendData));
  TRegTypes.RegisterType('Elgin.JNI.E1.JConM8', TypeInfo(Elgin.JNI.E1.JConM8));
  TRegTypes.RegisterType('Elgin.JNI.E1.JPrinterManager_PrinterManagerListener', TypeInfo(Elgin.JNI.E1.JPrinterManager_PrinterManagerListener));
  TRegTypes.RegisterType('Elgin.JNI.E1.JConM8_1', TypeInfo(Elgin.JNI.E1.JConM8_1));
  TRegTypes.RegisterType('Elgin.JNI.E1.JConSerial', TypeInfo(Elgin.JNI.E1.JConSerial));
  TRegTypes.RegisterType('Elgin.JNI.E1.JConService', TypeInfo(Elgin.JNI.E1.JConService));
  TRegTypes.RegisterType('Elgin.JNI.E1.JConService_1GetData', TypeInfo(Elgin.JNI.E1.JConService_1GetData));
  TRegTypes.RegisterType('Elgin.JNI.E1.JConService_1GetPrinter', TypeInfo(Elgin.JNI.E1.JConService_1GetPrinter));
  TRegTypes.RegisterType('Elgin.JNI.E1.JConService_1SendData', TypeInfo(Elgin.JNI.E1.JConService_1SendData));
  TRegTypes.RegisterType('Elgin.JNI.E1.JConService_2GetData', TypeInfo(Elgin.JNI.E1.JConService_2GetData));
  TRegTypes.RegisterType('Elgin.JNI.E1.JConSmartPOS', TypeInfo(Elgin.JNI.E1.JConSmartPOS));
  TRegTypes.RegisterType('Elgin.JNI.E1.JConTCP_IP', TypeInfo(Elgin.JNI.E1.JConTCP_IP));
  TRegTypes.RegisterType('Elgin.JNI.E1.JConTCP_IP_1GetData', TypeInfo(Elgin.JNI.E1.JConTCP_IP_1GetData));
  TRegTypes.RegisterType('Elgin.JNI.E1.JConTCP_IP_1GetPrinter', TypeInfo(Elgin.JNI.E1.JConTCP_IP_1GetPrinter));
  TRegTypes.RegisterType('Elgin.JNI.E1.JConTCP_IP_1SendData', TypeInfo(Elgin.JNI.E1.JConTCP_IP_1SendData));
  TRegTypes.RegisterType('Elgin.JNI.E1.JConUSB', TypeInfo(Elgin.JNI.E1.JConUSB));
  TRegTypes.RegisterType('Elgin.JNI.E1.JInterfaceFactoryXMLSAT', TypeInfo(Elgin.JNI.E1.JInterfaceFactoryXMLSAT));
  TRegTypes.RegisterType('Elgin.JNI.E1.JImplementacaoFactoryXMLSAT', TypeInfo(Elgin.JNI.E1.JImplementacaoFactoryXMLSAT));
  TRegTypes.RegisterType('Elgin.JNI.E1.JInterfaceSAT', TypeInfo(Elgin.JNI.E1.JInterfaceSAT));
  TRegTypes.RegisterType('Elgin.JNI.E1.JImplementacaoSAT', TypeInfo(Elgin.JNI.E1.JImplementacaoSAT));
  TRegTypes.RegisterType('Elgin.JNI.E1.JMFe', TypeInfo(Elgin.JNI.E1.JMFe));
  TRegTypes.RegisterType('Elgin.JNI.E1.JNFCe', TypeInfo(Elgin.JNI.E1.JNFCe));
  TRegTypes.RegisterType('Elgin.JNI.E1.JSAT', TypeInfo(Elgin.JNI.E1.JSAT));
  TRegTypes.RegisterType('Elgin.JNI.E1.JAndroid', TypeInfo(Elgin.JNI.E1.JAndroid));
  TRegTypes.RegisterType('Elgin.JNI.E1.JdsImpressora', TypeInfo(Elgin.JNI.E1.JdsImpressora));
  TRegTypes.RegisterType('Elgin.JNI.E1.JdsImpressora_1', TypeInfo(Elgin.JNI.E1.JdsImpressora_1));
  TRegTypes.RegisterType('Elgin.JNI.E1.JdsImpressora_infoHW', TypeInfo(Elgin.JNI.E1.JdsImpressora_infoHW));
  TRegTypes.RegisterType('Elgin.JNI.E1.JdsSAT', TypeInfo(Elgin.JNI.E1.JdsSAT));
  TRegTypes.RegisterType('Elgin.JNI.E1.JdsSAT_1', TypeInfo(Elgin.JNI.E1.JdsSAT_1));
  TRegTypes.RegisterType('Elgin.JNI.E1.JdsSAT_ChaveDePesquisa', TypeInfo(Elgin.JNI.E1.JdsSAT_ChaveDePesquisa));
  TRegTypes.RegisterType('Elgin.JNI.E1.JEtiqueta', TypeInfo(Elgin.JNI.E1.JEtiqueta));
  TRegTypes.RegisterType('Elgin.JNI.E1.JInterfaceAndroid', TypeInfo(Elgin.JNI.E1.JInterfaceAndroid));
  TRegTypes.RegisterType('Elgin.JNI.E1.JImplementacaoAndroid', TypeInfo(Elgin.JNI.E1.JImplementacaoAndroid));
  TRegTypes.RegisterType('Elgin.JNI.E1.JImplementacaoAndroid_IIImpressaoTexto', TypeInfo(Elgin.JNI.E1.JImplementacaoAndroid_IIImpressaoTexto));
  TRegTypes.RegisterType('Elgin.JNI.E1.JInterfaceBematech', TypeInfo(Elgin.JNI.E1.JInterfaceBematech));
  TRegTypes.RegisterType('Elgin.JNI.E1.JImplementacaoBematech', TypeInfo(Elgin.JNI.E1.JImplementacaoBematech));
  TRegTypes.RegisterType('Elgin.JNI.E1.JInterfaceEtiqueta', TypeInfo(Elgin.JNI.E1.JInterfaceEtiqueta));
  TRegTypes.RegisterType('Elgin.JNI.E1.JImplementacaoEtiqueta', TypeInfo(Elgin.JNI.E1.JImplementacaoEtiqueta));
  TRegTypes.RegisterType('Elgin.JNI.E1.JImplementacaoM8', TypeInfo(Elgin.JNI.E1.JImplementacaoM8));
  TRegTypes.RegisterType('Elgin.JNI.E1.JImplementacaoM8_1', TypeInfo(Elgin.JNI.E1.JImplementacaoM8_1));
  TRegTypes.RegisterType('Elgin.JNI.E1.JImplementacaoSmartPOS', TypeInfo(Elgin.JNI.E1.JImplementacaoSmartPOS));
  TRegTypes.RegisterType('Elgin.JNI.E1.JImplementacaoSmartPOS_1', TypeInfo(Elgin.JNI.E1.JImplementacaoSmartPOS_1));
  TRegTypes.RegisterType('Elgin.JNI.E1.JInterfaceTermica', TypeInfo(Elgin.JNI.E1.JInterfaceTermica));
  TRegTypes.RegisterType('Elgin.JNI.E1.JImplementacaoTermica', TypeInfo(Elgin.JNI.E1.JImplementacaoTermica));
  TRegTypes.RegisterType('Elgin.JNI.E1.JInterfaceM8', TypeInfo(Elgin.JNI.E1.JInterfaceM8));
  TRegTypes.RegisterType('Elgin.JNI.E1.JInterfaceSmartPOS', TypeInfo(Elgin.JNI.E1.JInterfaceSmartPOS));
  TRegTypes.RegisterType('Elgin.JNI.E1.JMiniPDVM8', TypeInfo(Elgin.JNI.E1.JMiniPDVM8));
  TRegTypes.RegisterType('Elgin.JNI.E1.JSmart', TypeInfo(Elgin.JNI.E1.JSmart));
  TRegTypes.RegisterType('Elgin.JNI.E1.JTermica', TypeInfo(Elgin.JNI.E1.JTermica));
  TRegTypes.RegisterType('Elgin.JNI.E1.JCodigoErro', TypeInfo(Elgin.JNI.E1.JCodigoErro));
  TRegTypes.RegisterType('Elgin.JNI.E1.JESCPOS', TypeInfo(Elgin.JNI.E1.JESCPOS));
  TRegTypes.RegisterType('Elgin.JNI.E1.JInteiro', TypeInfo(Elgin.JNI.E1.JInteiro));
  TRegTypes.RegisterType('Elgin.JNI.E1.JPPLA', TypeInfo(Elgin.JNI.E1.JPPLA));
  TRegTypes.RegisterType('Elgin.JNI.E1.JUtilidades', TypeInfo(Elgin.JNI.E1.JUtilidades));
  TRegTypes.RegisterType('Elgin.JNI.E1.JNodeList', TypeInfo(Elgin.JNI.E1.JNodeList));
  TRegTypes.RegisterType('Elgin.JNI.E1.JUtilidades_1', TypeInfo(Elgin.JNI.E1.JUtilidades_1));
  TRegTypes.RegisterType('Elgin.JNI.E1.JInterfaceOBJXMLPRODUTO', TypeInfo(Elgin.JNI.E1.JInterfaceOBJXMLPRODUTO));
  TRegTypes.RegisterType('Elgin.JNI.E1.JImplementacaoOBJXMLPRODUTO', TypeInfo(Elgin.JNI.E1.JImplementacaoOBJXMLPRODUTO));
  TRegTypes.RegisterType('Elgin.JNI.E1.JImplementacaoOBJPRODUTOXMLNFCE', TypeInfo(Elgin.JNI.E1.JImplementacaoOBJPRODUTOXMLNFCE));
  TRegTypes.RegisterType('Elgin.JNI.E1.JImplementacaoOBJPRODUTOXMLSAT', TypeInfo(Elgin.JNI.E1.JImplementacaoOBJPRODUTOXMLSAT));
  TRegTypes.RegisterType('Elgin.JNI.E1.JInterfaceOBJXML', TypeInfo(Elgin.JNI.E1.JInterfaceOBJXML));
  TRegTypes.RegisterType('Elgin.JNI.E1.JImplementacaoOBJXML', TypeInfo(Elgin.JNI.E1.JImplementacaoOBJXML));
  TRegTypes.RegisterType('Elgin.JNI.E1.JImplementacaoOBJXML_1', TypeInfo(Elgin.JNI.E1.JImplementacaoOBJXML_1));
  TRegTypes.RegisterType('Elgin.JNI.E1.JImplementacaoOBJXML_infoPag', TypeInfo(Elgin.JNI.E1.JImplementacaoOBJXML_infoPag));
  TRegTypes.RegisterType('Elgin.JNI.E1.JImplementacaoOBJXMLCANCELAMENTO', TypeInfo(Elgin.JNI.E1.JImplementacaoOBJXMLCANCELAMENTO));
  TRegTypes.RegisterType('Elgin.JNI.E1.JImplementacaoOBJXMLNFCE', TypeInfo(Elgin.JNI.E1.JImplementacaoOBJXMLNFCE));
  TRegTypes.RegisterType('Elgin.JNI.E1.JImplementacaoOBJXMLSAT', TypeInfo(Elgin.JNI.E1.JImplementacaoOBJXMLSAT));
  TRegTypes.RegisterType('Elgin.JNI.E1.JInterfaceOBJPRODUTOXMLNFCE', TypeInfo(Elgin.JNI.E1.JInterfaceOBJPRODUTOXMLNFCE));
  TRegTypes.RegisterType('Elgin.JNI.E1.JInterfaceOBJPRODUTOXMLSAT', TypeInfo(Elgin.JNI.E1.JInterfaceOBJPRODUTOXMLSAT));
  TRegTypes.RegisterType('Elgin.JNI.E1.JInterfaceOBJXMLCANCELAMENTO', TypeInfo(Elgin.JNI.E1.JInterfaceOBJXMLCANCELAMENTO));
  TRegTypes.RegisterType('Elgin.JNI.E1.JInterfaceOBJXMLNFCE', TypeInfo(Elgin.JNI.E1.JInterfaceOBJXMLNFCE));
  TRegTypes.RegisterType('Elgin.JNI.E1.JInterfaceOBJXMLSAT', TypeInfo(Elgin.JNI.E1.JInterfaceOBJXMLSAT));
  TRegTypes.RegisterType('Elgin.JNI.E1.JScanner_Scanner', TypeInfo(Elgin.JNI.E1.JScanner_Scanner));
  TRegTypes.RegisterType('Elgin.JNI.E1.JAssinaturas', TypeInfo(Elgin.JNI.E1.JAssinaturas));
  TRegTypes.RegisterType('Elgin.JNI.E1.JParametros', TypeInfo(Elgin.JNI.E1.JParametros));
  TRegTypes.RegisterType('Elgin.JNI.E1.JServicoE1', TypeInfo(Elgin.JNI.E1.JServicoE1));
  TRegTypes.RegisterType('Elgin.JNI.E1.JServicoE1_Etiqueta', TypeInfo(Elgin.JNI.E1.JServicoE1_Etiqueta));
  TRegTypes.RegisterType('Elgin.JNI.E1.JServicoE1_SAT', TypeInfo(Elgin.JNI.E1.JServicoE1_SAT));
  TRegTypes.RegisterType('Elgin.JNI.E1.JServicoE1_Termica', TypeInfo(Elgin.JNI.E1.JServicoE1_Termica));
  TRegTypes.RegisterType('Elgin.JNI.E1.Jminipdvm8_BuildConfig', TypeInfo(Elgin.JNI.E1.Jminipdvm8_BuildConfig));
  TRegTypes.RegisterType('Elgin.JNI.E1.JPrinterManager', TypeInfo(Elgin.JNI.E1.JPrinterManager));
  TRegTypes.RegisterType('Elgin.JNI.E1.JPrinterManager_1', TypeInfo(Elgin.JNI.E1.JPrinterManager_1));
  TRegTypes.RegisterType('Elgin.JNI.E1.JIPrinterCallback_Stub', TypeInfo(Elgin.JNI.E1.JIPrinterCallback_Stub));
  TRegTypes.RegisterType('Elgin.JNI.E1.JPrinterManager_2', TypeInfo(Elgin.JNI.E1.JPrinterManager_2));
  TRegTypes.RegisterType('Elgin.JNI.E1.JCommSerialAPI', TypeInfo(Elgin.JNI.E1.JCommSerialAPI));
  TRegTypes.RegisterType('Elgin.JNI.E1.JComportNative', TypeInfo(Elgin.JNI.E1.JComportNative));
  TRegTypes.RegisterType('Elgin.JNI.E1.JIPrinterCallback', TypeInfo(Elgin.JNI.E1.JIPrinterCallback));
  TRegTypes.RegisterType('Elgin.JNI.E1.JIPrinterCallback_Stub_Proxy', TypeInfo(Elgin.JNI.E1.JIPrinterCallback_Stub_Proxy));
  TRegTypes.RegisterType('Elgin.JNI.E1.JIPrinterService', TypeInfo(Elgin.JNI.E1.JIPrinterService));
  TRegTypes.RegisterType('Elgin.JNI.E1.JIPrinterService_Stub', TypeInfo(Elgin.JNI.E1.JIPrinterService_Stub));
  TRegTypes.RegisterType('Elgin.JNI.E1.JIPrinterService_Stub_Proxy', TypeInfo(Elgin.JNI.E1.JIPrinterService_Stub_Proxy));
  TRegTypes.RegisterType('Elgin.JNI.E1.JNode', TypeInfo(Elgin.JNI.E1.JNode));
  TRegTypes.RegisterType('Elgin.JNI.E1.JAttr', TypeInfo(Elgin.JNI.E1.JAttr));
  TRegTypes.RegisterType('Elgin.JNI.E1.JCharacterData', TypeInfo(Elgin.JNI.E1.JCharacterData));
  TRegTypes.RegisterType('Elgin.JNI.E1.JText', TypeInfo(Elgin.JNI.E1.JText));
  TRegTypes.RegisterType('Elgin.JNI.E1.JCDATASection', TypeInfo(Elgin.JNI.E1.JCDATASection));
  TRegTypes.RegisterType('Elgin.JNI.E1.JComment', TypeInfo(Elgin.JNI.E1.JComment));
  TRegTypes.RegisterType('Elgin.JNI.E1.JDOMConfiguration', TypeInfo(Elgin.JNI.E1.JDOMConfiguration));
  TRegTypes.RegisterType('Elgin.JNI.E1.JDOMImplementation', TypeInfo(Elgin.JNI.E1.JDOMImplementation));
  TRegTypes.RegisterType('Elgin.JNI.E1.JDOMStringList', TypeInfo(Elgin.JNI.E1.JDOMStringList));
  TRegTypes.RegisterType('Elgin.JNI.E1.JDocument', TypeInfo(Elgin.JNI.E1.JDocument));
  TRegTypes.RegisterType('Elgin.JNI.E1.JDocumentFragment', TypeInfo(Elgin.JNI.E1.JDocumentFragment));
  TRegTypes.RegisterType('Elgin.JNI.E1.JDocumentType', TypeInfo(Elgin.JNI.E1.JDocumentType));
  TRegTypes.RegisterType('Elgin.JNI.E1.JElement', TypeInfo(Elgin.JNI.E1.JElement));
  TRegTypes.RegisterType('Elgin.JNI.E1.JEntityReference', TypeInfo(Elgin.JNI.E1.JEntityReference));
  TRegTypes.RegisterType('Elgin.JNI.E1.JNamedNodeMap', TypeInfo(Elgin.JNI.E1.JNamedNodeMap));
  TRegTypes.RegisterType('Elgin.JNI.E1.JProcessingInstruction', TypeInfo(Elgin.JNI.E1.JProcessingInstruction));
  TRegTypes.RegisterType('Elgin.JNI.E1.JTypeInfo', TypeInfo(Elgin.JNI.E1.JTypeInfo));
  TRegTypes.RegisterType('Elgin.JNI.E1.JUserDataHandler', TypeInfo(Elgin.JNI.E1.JUserDataHandler));
end;

initialization
  RegisterTypes;
end.

