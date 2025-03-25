#include 'C:\ProjetoACBr\ACBr\Projetos\ACBrLib\Demos\Harbour\Comum\acbrlib.ch'

#ifdef __PLATFORM__WINDOWS
#define ACBrLIB 'ACBrAbecsPinpad32.dll'
#else
#ifdef __PLATFORM__LINUX
#define ACBrLIB 'libacbrabecspinpad64.so'
#else
#error ACBrLIB-FALTA DEFINICAO: PLATFORM__?
#endif
#endif

CREATE CLASS ACBrAbecsPinpad
    HIDDEN:
    VAR hHandle

    METHOD CheckResult(hResult)
    METHOD ProcessResult(buffer, bufferLen)

    VISIBLE:
    METHOD New(eArqConfig, eChaveCrypt) CONSTRUCTOR
    DESTRUCTOR Destroy

    METHOD Nome()
    METHOD Versao()

    METHOD ConfigLer(eArqConfig)
    METHOD ConfigGravar(eArqConfig)
    METHOD ConfigLerValor(eSessao, eChave)
    METHOD ConfigGravarValor(eSessao, eChave, eValor)

    METHOD Ativar()
    METHOD Desativar()
    METHOD OPN()
    METHOD CLO(sMensagem)
    METHOD CLX(sMensagemOuNomeImagem)
    METHOD PinPadCapabilities()
    METHOD DSP(sMensagem)
    method GCD(aMSGIDX, aTimeOut)
    

END CLASS

METHOD New(eArqConfig, eChaveCrypt) CLASS ACBrAbecsPinpad
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
    hResult := DllCall(::hHandle, DLL_OSAPI, "AbecsPinpad_Inicializar", hb_StrToUTF8(eArqConfig), hb_StrToUTF8(eChaveCrypt))
    ::CheckResult(hResult) 
RETURN Self

METHOD Destroy CLASS ACBrAbecsPinpad
    DllCall(::hHandle, DLL_OSAPI, "AbecsPinpad_Finalizar")
    DllUnload(::hHandle)
RETURN nil

METHOD CheckResult(hResult) CLASS ACBrAbecsPinpad
    local buffer, bufferLen, oErr
    if hResult >= 0 
        RETURN nil
    endif    

    bufferLen := STR_LEN
    buffer := Space(bufferLen)

    DllCall(::hHandle, DLL_OSAPI, "AbecsPinpad_UltimoRetorno", @buffer, @bufferLen)
    if bufferLen > STR_LEN
        buffer := Space(bufferLen)
        DllCall(::hHandle, DLL_OSAPI, "AbecsPinpad_UltimoRetorno", @buffer, @bufferLen)        
    endif    

    oErr := ErrorNew()
    oErr:Severity := ES_ERROR
    oErr:Description := hb_UTF8ToStr(buffer)
    Throw(oErr)
RETURN nil

METHOD ProcessResult(buffer, bufferLen) CLASS ACBrAbecsPinpad
    if bufferLen > STR_LEN
        buffer := Space(bufferLen)
        DllCall(::hHandle, DLL_OSAPI, "AbecsPinpad_UltimoRetorno", @buffer, @bufferLen)        
    endif
RETURN buffer

METHOD Nome CLASS ACBrAbecsPinpad
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)
    hResult := DllCall(::hHandle, DLL_OSAPI, "AbecsPinpad_Nome", @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN ::ProcessResult(buffer, bufferLen)

METHOD Versao CLASS ACBrAbecsPinpad
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)
    hResult := DllCall(::hHandle, DLL_OSAPI, "AbecsPinpad_Versao", @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN ::ProcessResult(buffer, bufferLen)

METHOD ConfigLer(eArqConfig) CLASS ACBrAbecsPinpad
    local hResult    
    hResult := DllCall(::hHandle, DLL_OSAPI, "AbecsPinpad_ConfigLer", hb_StrToUTF8(eArqConfig))
    ::CheckResult(hResult)
RETURN nil

METHOD ConfigGravar(eArqConfig) CLASS ACBrAbecsPinpad
    local hResult    
    hResult := DllCall(::hHandle, DLL_OSAPI, "AbecsPinpad_ConfigGravar", hb_StrToUTF8(eArqConfig))
    ::CheckResult(hResult)
RETURN nil

METHOD ConfigLerValor(eSessao, eChave) CLASS ACBrAbecsPinpad
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)   
    hResult := DllCall(::hHandle, DLL_OSAPI, "AbecsPinpad_ConfigLerValor", hb_StrToUTF8(eSessao), hb_StrToUTF8(eChave), @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN ::ProcessResult(buffer, bufferLen)

METHOD ConfigGravarValor(eSessao, eChave, eValor) CLASS ACBrAbecsPinpad
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "AbecsPinpad_ConfigGravarValor", hb_StrToUTF8(eSessao), hb_StrToUTF8(eChave), hb_StrToUTF8(eValor))
    ::CheckResult(hResult)
RETURN nil

METHOD Ativar() CLASS ACBrAbecsPinpad
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "AbecsPinpad_Ativar")
    ::CheckResult(hResult)
RETURN nil

METHOD Desativar() CLASS ACBrAbecsPinpad
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "AbecsPinpad_Desativar")
    ::CheckResult(hResult)
RETURN nil

METHOD OPN() CLASS ACBrAbecsPinpad
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "AbecsPinpad_OPN")
RETURN nil

METHOD CLO(sMensagem) CLASS ACBrAbecsPinpad
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)
    
    hResult := DllCall(::hHandle, DLL_OSAPI, "AbecsPinpad_CLO", sMensagem, @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN val(StrTran(AllTrim(::ProcessResult(buffer, bufferLen)),",","."))

METHOD CLX(sMensagemOuNomeImagem)
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)

    hResult := DllCall(::hHandle, DLL_OSAPI, "AbecsPinpad_CLX", sMensagemOuNomeImagem, @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN val(StrTran(AllTrim(::ProcessResult(buffer, bufferLen)),",","."))

METHOD PinPadCapabilities()
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)
    hResult := DllCall(::hHandle, DLL_OSAPI, "AbecsPinpad_PinPadCapabilities", @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN nil

METHOD DSP(sMensagem)
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)

    hResult := DllCall(::hHandle, DLL_OSAPI, "AbecsPinpad_DSP", sMensagem, @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN val(StrTran(AllTrim(::ProcessResult(buffer, bufferLen)),",","."))

method GCD(aMSGIDX, aTimeOut)
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)

    hResult := DllCall(::hHandle, DLL_OSAPI, "AbecsPinpad_GCD", aMSGIDX, aTimeOut, @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN val(StrTran(AllTrim(::ProcessResult(buffer, bufferLen)),",","."))
