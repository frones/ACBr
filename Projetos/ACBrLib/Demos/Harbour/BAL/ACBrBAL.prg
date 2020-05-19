#include '..\Comum\acbrlib.ch'

#ifdef __PLATFORM__WINDOWS
   #define ACBrLIB 'ACBrBAL32.dll'
#else
   #ifdef __PLATFORM__LINUX
      #define ACBrLIB 'libacbrbal64.so'
   #else
      #error ACBrLIB-FALTA DEFINICAO: PLATFORM__?
   #endif
#endif

CREATE CLASS ACBrBAL
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

    METHOD Ativar()
    METHOD Desativar()
    METHOD LePeso(MillisecTimeOut)
    METHOD SolicitarPeso()
    METHOD UltimoPesoLido()
    METHOD InterpretarRespostaPeso(Resposta)


END CLASS

METHOD New(eArqConfig, eChaveCrypt) CLASS ACBrBAL
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
    hResult := DllCall(::hHandle, DLL_OSAPI, "BAL_Inicializar", hb_StrToUTF8(eArqConfig), hb_StrToUTF8(eChaveCrypt))
    ::CheckResult(hResult) 
    RETURN Self

PROCEDURE Destroy CLASS ACBrBAL
    DllCall(::hHandle, DLL_OSAPI, "BAL_Finalizar")
    DllUnload(::hHandle)
    RETURN

METHOD CheckResult(hResult) CLASS ACBrBAL
    local buffer, bufferLen, oErr
    if hResult >= 0 
       RETURN nil
    endif    

    bufferLen := STR_LEN
    buffer := Space(bufferLen)

    DllCall(::hHandle, DLL_OSAPI, "BAL_UltimoRetorno", @buffer, @bufferLen)
    if bufferLen > STR_LEN
        buffer := Space(bufferLen)
        DllCall(::hHandle, DLL_OSAPI, "BAL_UltimoRetorno", @buffer, @bufferLen)        
    endif    

    oErr := ErrorNew()
    oErr:Severity := ES_ERROR
    oErr:Description := hb_UTF8ToStr(buffer)
    Throw(oErr)
    RETURN nil

METHOD ProcessResult(buffer, bufferLen) CLASS ACBrBAL
    if bufferLen > STR_LEN
        buffer := Space(bufferLen)
        DllCall(::hHandle, DLL_OSAPI, "BAL_UltimoRetorno", @buffer, @bufferLen)        
    endif
    RETURN buffer

METHOD Nome CLASS ACBrBAL
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)
    hResult := DllCall(::hHandle, DLL_OSAPI, "BAL_Nome", @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD Versao CLASS ACBrBAL
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)
    hResult := DllCall(::hHandle, DLL_OSAPI, "BAL_Versao", @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD ConfigLer(eArqConfig) CLASS ACBrBAL
    local hResult    
    hResult := DllCall(::hHandle, DLL_OSAPI, "BAL_ConfigLer", hb_StrToUTF8(eArqConfig))
    ::CheckResult(hResult)
    RETURN nil

METHOD ConfigGravar(eArqConfig) CLASS ACBrBAL
        local hResult    
        hResult := DllCall(::hHandle, DLL_OSAPI, "BAL_ConfigGravar", hb_StrToUTF8(eArqConfig))
        ::CheckResult(hResult)
        RETURN nil

METHOD ConfigLerValor(eSessao, eChave) CLASS ACBrBAL
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)   
    hResult := DllCall(::hHandle, DLL_OSAPI, "BAL_ConfigLerValor", hb_StrToUTF8(eSessao), hb_StrToUTF8(eChave), @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD ConfigGravarValor(eSessao, eChave, eValor) CLASS ACBrBAL
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "BAL_ConfigGravarValor", hb_StrToUTF8(eSessao), hb_StrToUTF8(eChave), hb_StrToUTF8(eValor))
    ::CheckResult(hResult)
    RETURN nil

METHOD Ativar() CLASS ACBrBAL
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "BAL_Ativar")
    ::CheckResult(hResult)
    RETURN nil

METHOD Desativar() CLASS ACBrBAL
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "BAL_Desativar")
    ::CheckResult(hResult)
    RETURN nil

METHOD LePeso(MillisecTimeOut) CLASS ACBrBAL
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen) 

    hResult := DllCall(::hHandle, DLL_OSAPI, "BAL_LePesoStr", MillisecTimeOut, @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN val(StrTran(AllTrim(::ProcessResult(buffer, bufferLen)),",","."))

METHOD SolicitarPeso() CLASS ACBrBAL
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "BAL_SolicitarPeso")
    ::CheckResult(hResult)
    RETURN nil

METHOD UltimoPesoLido() CLASS ACBrBAL
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen) 

    hResult := DllCall(::hHandle, DLL_OSAPI, "BAL_UltimoPesoLidoStr", @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN val(StrTran(AllTrim(::ProcessResult(buffer, bufferLen)),",","."))

METHOD InterpretarRespostaPeso(Resposta) CLASS ACBrBAL
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen) 

    hResult := DllCall(::hHandle, DLL_OSAPI, "BAL_InterpretarRespostaPesoStr", hb_StrToUTF8(Resposta), @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN val(StrTran(AllTrim(::ProcessResult(buffer, bufferLen)),",","."))