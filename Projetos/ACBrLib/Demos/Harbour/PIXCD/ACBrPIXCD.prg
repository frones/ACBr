#include 'C:\ProjetoACBr\ACBr\Projetos\ACBrLib\Demos\Harbour\Comum\acbrlib.ch'

#ifdef __PLATFORM__WINDOWS
#define ACBrLIB 'ACBrPIXCD32.dll'
#else
#ifdef __PLATFORM__LINUX
#define ACBrLIB 'libacbrpixcd64.so'
#else
#error ACBrLIB-FALTA DEFINICAO: PLATFORM__?
#endif
#endif

CREATE CLASS ACBrPIXCD
    HIDDEN:
    VAR hHandle

    METHOD CheckResult(hResult)
    METHOD ProcessResult(buffer, bufferLen)

    VISIBLE:
    METHOD New(eArqConfig, eChaveCrypt) CONSTRUCTOR
    DESTRUCTOR  Destroy

    METHOD Nome()
    METHOD Versao()

    METHOD ConfigLer(eArqConfig)
    METHOD ConfigGravar(eArqConfig)
    METHOD ConfigLerValor(eSessao, eChave)
    METHOD ConfigGravarValor(eSessao, eChave, eValor)

    METHOD GerarQRCodeEstatico(AValor, AinfoAdicional, ATxID)

    //Cob
    METHOD CriarCobrancaImediata(AInfCobSolicitada, ATxId)

END CLASS

METHOD New(eArqConfig, eChaveCrypt) CLASS ACBrPIXCD
    local hResult, buffer, bufferLen, oErr

    eArqConfig :=if(eArqConfig = nil, '', eArqConfig)
    eChaveCrypt:=if(eChaveCrypt = nil, '', eChaveCrypt)

    ::hHandle := DllLoad(ACBrLIB)
    if EMPTY(::hHandle) // Eric.Developer: xHarbour retorna 0x00000000
        oErr := ErrorNew()
        oErr:Severity := ES_ERROR        
        oErr:Description := "Erro a carregar a dll [" + ACBrLIB + "]"
        Throw(oErr)
    endif
    hResult := DllCall(::hHandle, DLL_OSAPI, "PIXCD_Inicializar", hb_StrToUTF8(eArqConfig), hb_StrToUTF8(eChaveCrypt))
    ::CheckResult(hResult) 
RETURN Self

PROCEDURE Destroy CLASS ACBrPIXCD
    DllCall(::hHandle, DLL_OSAPI, "PIXCD_Finalizar")
    DllUnload(::hHandle)
RETURN

METHOD CheckResult(hResult) CLASS ACBrPIXCD
    local buffer, bufferLen, oErr
    if hResult >= 0 
        RETURN nil
    endif    

    bufferLen := STR_LEN
    buffer := Space(bufferLen)

    DllCall(::hHandle, DLL_OSAPI, "PIXCD_UltimoRetorno", @buffer, @bufferLen)
    if bufferLen > STR_LEN
        buffer := Space(bufferLen)
        DllCall(::hHandle, DLL_OSAPI, "PIXCD_UltimoRetorno", @buffer, @bufferLen)        
    endif    

    oErr := ErrorNew()
    oErr:Severity := ES_ERROR
    oErr:Description := hb_UTF8ToStr(buffer)
    Throw(oErr)
RETURN nil

METHOD ProcessResult(buffer, bufferLen) CLASS ACBrPIXCD
    if bufferLen > STR_LEN
        buffer := Space(bufferLen)
        DllCall(::hHandle, DLL_OSAPI, "PIXCD_UltimoRetorno", @buffer, @bufferLen)        
    endif
RETURN buffer

METHOD Nome CLASS ACBrPIXCD
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)
    hResult := DllCall(::hHandle, DLL_OSAPI, "PIXCD_Nome", @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN ::ProcessResult(buffer, bufferLen)

METHOD Versao CLASS ACBrPIXCD
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)
    hResult := DllCall(::hHandle, DLL_OSAPI, "PIXCD_Versao", @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN ::ProcessResult(buffer, bufferLen)

METHOD ConfigLer(eArqConfig) CLASS ACBrPIXCD
    local hResult    
    hResult := DllCall(::hHandle, DLL_OSAPI, "PIXCD_ConfigLer", hb_StrToUTF8(eArqConfig))
    ::CheckResult(hResult)
RETURN nil

METHOD ConfigGravar(eArqConfig) CLASS ACBrPIXCD
    local hResult    
    hResult := DllCall(::hHandle, DLL_OSAPI, "PIXCD_ConfigGravar", hb_StrToUTF8(eArqConfig))
    ::CheckResult(hResult)
RETURN nil

METHOD ConfigLerValor(eSessao, eChave) CLASS ACBrPIXCD
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)   
    hResult := DllCall(::hHandle, DLL_OSAPI, "PIXCD_ConfigLerValor", hb_StrToUTF8(eSessao), hb_StrToUTF8(eChave), @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN ::ProcessResult(buffer, bufferLen)

METHOD ConfigGravarValor(eSessao, eChave, eValor) CLASS ACBrPIXCD
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "PIXCD_ConfigGravarValor", hb_StrToUTF8(eSessao), hb_StrToUTF8(eChave), hb_StrToUTF8(eValor))
    ::CheckResult(hResult)
RETURN nil

METHOD GerarQRCodeEstatico(AValor, AinfoAdicional, ATxID)
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen) 

    hResult := DllCall(::hHandle, DLL_OSAPI, "PIXCD_GerarQRCodeEstatico", AValor, AinfoAdicional, ATxID, @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN val(StrTran(AllTrim(::ProcessResult(buffer, bufferLen)),",","."))


METHOD CriarCobrancaImediata(AInfCobSolicitada, ATxId)
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen) 

    hResult := DllCall(::hHandle, DLL_OSAPI, "PIXCD_CriarCobrancaImediata", AInfCobSolicitada, ATxId, @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN val(StrTran(AllTrim(::ProcessResult(buffer, bufferLen)),",","."))