Attribute VB_Name = "ACBrComum"
Option Explicit

Public Const SESSAO_PRINCIPAL         As String = "Principal"
Public Const SESSAO_VERSAO            As String = "Versao"
Public Const SESSAO_SISTEMA           As String = "Sistema"
Public Const SESSAO_PROXY             As String = "Proxy"
Public Const SESSAO_EMAIL             As String = "Email"
Public Const SESSAO_SOFTWAREHOUSE     As String = "SoftwareHouse"
Public Const SESSAO_EMISSOR           As String = "Emissor"
Public Const SESSAO_POSPRINTER        As String = "PosPrinter"
Public Const SESSAO_POSPRINTER_BARRAS As String = "PosPrinter_Barras"
Public Const SESSAO_POSPRINTER_QRCODE As String = "PosPrinter_QRCode"
Public Const SESSAO_POSPRINTER_LOGO   As String = "PosPrinter_Logo"
Public Const SESSAO_POSPRINTER_GAVETA As String = "PosPrinter_Gaveta"
Public Const SESSAO_ETQ               As String = "ETQ"
Public Const SESSAO_SAT               As String = "SAT"
Public Const SESSAO_SATCONFIG         As String = "SATConfig"
Public Const SESSAO_SATCONFIGARQUIVOS As String = "SATConfigArquivos"
Public Const SESSAO_SATREDE           As String = "SATRede"
Public Const SESSAO_EXTRATO           As String = "Extrato"
Public Const SESSAO_DFE               As String = "DFe"
Public Const SESSAO_NFE               As String = "NFe"
Public Const SESSAO_DANFE             As String = "DANFE"
Public Const SESSAO_DANFENFe          As String = "DANFENFe"
Public Const SESSAO_DANFENFCe         As String = "DANFENFCe"

Public Enum NivelLog
    logNenhum = 0
    logSimples = 1
    logNormal = 2
    logCompleto = 3
    logParanoico = 4
End Enum

Public Enum ACBrPosPrinterModelo
    ppTexto = 0
    ppEscPosEpson = 1
    ppEscBematech = 2
    ppEscDaruma = 3
    ppEscVox = 4
    ppEscDiebold = 5
    ppEscEpsonP2 = 6
    ppCustomPos = 7
    ppEscPosStar = 8
    ppEscZJiang = 9
    ppEscGPrinter = 10
End Enum

Public Enum ACBrPosTipoStatus
    stNone = 0
    stErro = 2 ^ 0
    stNaoSerial = 2 ^ 1
    stPoucoPapel = 2 ^ 2
    stSemPapel = 2 ^ 3
    stGavetaAberta = 2 ^ 4
    stImprimindo = 2 ^ 5
    stOffLine = 2 ^ 6
    stTampaAberta = 2 ^ 7
    stErroLeitura = 2 ^ 8
End Enum

Public Enum PosPaginaCodigo
    pcNone = 0
    pc437 = 1
    pc850 = 2
    pc852 = 3
    pc860 = 4
    pcUTF8 = 5
    pc1252 = 6
End Enum

Public Enum SerialHandShake
    hsNenhum = 0
    hsXON_XOFF = 1
    hsRTS_CTS = 2
    hsDTR_DSR = 3
End Enum

Public Enum SerialParity
    pNone = 78
    pOdd = 79
    pEven = 69
    pMark = 77
    pSpace = 83
End Enum

Public Enum SerialStopBytes
    s1 = 0
    s1eMeio = 1
    s2 = 2
End Enum

Public Enum ETQModelo
    etqNenhum = 0
    etqPpla = 1
    etqPplb = 2
    etqZPLII = 3
    etqEpl2 = 4
End Enum

Public Enum ETQUnidade
    etqMilimetros = 0
    etqPolegadas = 1
    etqDots = 2
    etqDecimoDeMilimetros = 3
End Enum

Public Enum ETQDPI
    dpi203 = 0
    dpi300 = 1
    dpi600 = 2
End Enum

Public Enum ETQBackFeed
    bfNone = 0
    bfOn = 1
    bfOff = 2
End Enum

Public Enum ETQOrientacao
    orNormal = 0
    or270 = 1
    or180 = 2
    or90 = 3
End Enum

Public Enum TipoCodBarra
    barEAN13 = 0
    barEAN8 = 1
    barSTANDARD = 2
    barINTERLEAVED = 3
    barCODE128 = 4
    barCODE39 = 5
    barCODE93 = 6
    barUPCA = 7
    barCODABAR = 8
    barMSI = 9
    barCODE11 = 10
End Enum

Public Enum ETQBarraExibeCodigo
    becPadrao = 0
    becSIM = 1
    becNAO = 2
End Enum

Public Enum MailAttachmentDisposition
    adAttachment = 0
    adInline = 1
End Enum

Public Enum MessPriority
    MP_unknown = 0
    MP_low = 1
    MP_normal = 2
    MP_high = 3
End Enum

Public Enum MimeChar
    ISO_8859_1 = 0
    ISO_8859_2 = 1
    ISO_8859_3 = 2
    ISO_8859_4 = 3
    ISO_8859_5 = 4
    ISO_8859_6 = 5
    ISO_8859_7 = 6
    ISO_8859_8 = 7
    ISO_8859_9 = 8
    ISO_8859_10 = 9
    ISO_8859_13 = 10
    ISO_8859_14 = 11
    ISO_8859_15 = 12
    CP1250 = 13
    CP1251 = 14
    CP1252 = 15
    CP1253 = 16
    CP1254 = 17
    CP1255 = 18
    CP1256 = 19
    CP1257 = 20
    CP1258 = 21
    KOI8_R = 22
    CP895 = 23
    CP852 = 24
    UCS_2 = 25
    UCS_4 = 26
    UTF_8 = 27
    UTF_7 = 28
    UTF_7mod = 29
    UCS_2LE = 30
    UCS_4LE = 31
    UTF_16 = 32
    UTF_16LE = 33
    UTF_32 = 34
    UTF_32LE = 35
    C99 = 36
    JAVA = 37
    ISO_8859_16 = 38
    KOI8_U = 39
    KOI8_RU = 40
    CP862 = 41
    CP866 = 42
    Mac = 43
    MACCE = 44
    MACICE = 45
    MACCRO = 46
    MACRO = 47
    MACCYR = 48
    MACUK = 49
    MACGR = 50
    MACTU = 51
    MACHEB = 52
    MACAR = 53
    MACTH = 54
    ROMAN8 = 55
    NEXTSTEP = 56
    ARMASCII = 57
    GEORGIAN_AC = 58
    GEORGIAN_PS = 59
    KOI8_T = 60
    MULELAO = 61
    CP1133 = 62
    TIS620 = 63
    CP874 = 64
    VISCII = 65
    TCVN = 66
    ISO_IR_14 = 67
    JIS_X0201 = 68
    JIS_X0208 = 69
    JIS_X0212 = 70
    GB1988_80 = 71
    GB2312_80 = 72
    ISO_IR_165 = 73
    ISO_IR_149 = 74
    EUC_JP = 75
    SHIFT_JIS = 76
    CP932 = 77
    ISO_2022_JP = 78
    ISO_2022_JP1 = 79
    ISO_2022_JP2 = 80
    GB2312 = 81
    CP936 = 82
    GB18030 = 83
    ISO_2022_CN = 84
    ISO_2022_CNE = 85
    HZ = 86
    EUC_TW = 87
    BIG5 = 88
    CP950 = 89
    BIG5_HKSCS = 90
    EUC_KR = 91
    CP949 = 92
    CP1361 = 93
    ISO_2022_KR = 94
    CP737 = 95
    CP775 = 96
    CP853 = 97
    CP855 = 98
    CP857 = 99
    CP858 = 100
    CP860 = 101
    CP861 = 102
    CP863 = 103
    CP864 = 104
    CP865 = 105
    CP869 = 106
    CP1125 = 107
End Enum

Public Enum SSLType
    LT_all = 0
    LT_SSLv2 = 1
    LT_SSLv3 = 2
    LT_TLSv1 = 3
    LT_TLSv1_1 = 4
    LT_TLSv1_2 = 5
    LT_SSHv2 = 6
End Enum

Public Enum SSLCryptLib
    cryNone = 0
    cryOpenSSL = 1
    cryCapicom = 2
    cryWinCrypt = 3
End Enum

Public Enum SSLHttpLib
    httpNone = 0
    httpWinINet = 1
    httpWinHttp = 2
    httpOpenSSL = 3
    httpIndy = 4
End Enum

Public Enum SSLXmlSignLib
    xsNone = 0
    xsXmlSec = 1
    xsMsXml = 2
    xsMsXmlCapicom = 3
    xsLibXml2 = 4
End Enum

Public Enum TipoAmbiente
    taProducao = 0
    taHomologacao = 1
End Enum

Public Enum TipoEmissao
    teNormal = 0
    teContingencia = 1
    teSCAN = 2
    teDPEC = 3
    teFSDA = 4
    teSVCAN = 5
    teSVCRS = 6
    teSVCSP = 7
    teOffLine = 8
End Enum

Public Enum TipoRelatorioBobina
    tpFortes = 0
    tpEscPos = 1
End Enum

Public Enum AutoSimNao
    rAuto = 0
    rSim = 1
    rNao = 2
End Enum

Public Enum indRatISSQN
    irSim = 0
    irNao = 1
End Enum

Public Enum RegTrib
    RTSimplesNacional = 0
    RTRegimeNormal = 1
End Enum

Public Enum RegTribISSQN
    RTISSMicroempresaMunicipal = 0
    RTISSEstimativa = 1
    RTISSSociedadeProfissionais = 2
    RTISSCooperativa = 3
    RTISSMEI = 4
    RTISSMEEPP = 5
    RTISSNenhum = 6
End Enum

Public Enum SATExtratoFiltro
    fiNenhum = 0
    fiPDF = 1
    fiHTML = 2
End Enum

Public Enum SATModelo
    satNenhum = 0
    satDinamico_cdecl = 1
    satDinamico_stdcall = 2
    mfe_Integrador_XML = 3
End Enum

Public Enum ModeloDF
    moNFe = 0
    moNFCe = 1
End Enum

Public Enum VersaoDFe
    ve200 = 0
    ve300 = 1
    ve310 = 2
    ve400 = 3
End Enum

Public Function HasPosTipoStatus(Check As ACBrPosTipoStatus, Flag As ACBrPosTipoStatus) As Boolean
    HasPosTipoStatus = (Check And Flag) = Flag
End Function

Function DirExists(DirName As String) As Boolean
    On Error GoTo ErrorHandler
    ' test the directory attribute
    DirExists = GetAttr(DirName) And vbDirectory
ErrorHandler:
    ' if an error occurs, this function returns False
End Function

Function FileExists(ByVal sFileName As String) As Boolean
    On Error GoTo ErrorHandler
    ' test the Archive attribute
    FileExists = GetAttr(sFileName) And vbArchive
ErrorHandler:
    ' if an error occurs, this function returns False
End Function
