#include '..\Comum\acbrlib.ch'

#ifdef __PLATFORM__WINDOWS
   #define ACBrLIB 'ACBrMail32.dll'
#else
   #ifdef __PLATFORM__LINUX
      #define ACBrLIB 'libacbrmail64.so'
   #else
      #error ACBrLIB-FALTA DEFINICAO: PLATFORM__?
   #endif
#endif

CREATE CLASS ACBrMail
HIDDEN:
    VAR hHandle

    METHOD CheckResult(hResult)
    METHOD ProcessResult(buffer, bufferLen)

VISIBLE:
    METHOD New(eArqConfig, eChaveCrypt) CONSTRUCTOR
    DESTRUCTOR Destroy

    METHOD Nome
    METHOD Versao

    METHOD ConfigLer(eArqConfig)
    METHOD ConfigGravar(eArqConfig)
    METHOD ConfigLerValor(eSessao, eChave)
    METHOD ConfigGravarValor(eSessao, eChave, eValor)

    METHOD SetSubject(subject)
    METHOD AddAddress(email, name)
    METHOD AddReplyTo(email, name)
    METHOD AddCC(email, name)
    METHOD AddBCC(email)
    METHOD ClearAttachment()
    METHOD AddAttachment(eFileName, eDescription, aDisposition)
    METHOD AddBody(eBody)
    METHOD AddAltBody(eAltBody)
    METHOD SaveToFile(eFileName)

    METHOD Clear()
    METHOD Send()

END CLASS

METHOD New(eArqConfig, eChaveCrypt) CLASS ACBrMail
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
    hResult := DllCall(::hHandle, DLL_OSAPI, "MAIL_Inicializar", hb_StrToUTF8(eArqConfig), hb_StrToUTF8(eChaveCrypt))
    ::CheckResult(hResult) 
    RETURN Self

PROCEDURE Destroy CLASS ACBrMail
    DllCall(::hHandle, DLL_OSAPI, "MAIL_Finalizar")
    DllUnload(::hHandle)
    RETURN

METHOD CheckResult(hResult) CLASS ACBrMail
    local buffer, bufferLen, oErr
    if hResult >= 0 
       RETURN nil
    endif    

    bufferLen := STR_LEN
    buffer := Space(bufferLen)

    DllCall(::hHandle, DLL_OSAPI, "MAIL_UltimoRetorno", @buffer, @bufferLen)
    if bufferLen > STR_LEN
        buffer := Space(bufferLen)
        DllCall(::hHandle, DLL_OSAPI, "MAIL_UltimoRetorno", @buffer, @bufferLen)        
    endif    

    oErr := ErrorNew()
    oErr:Severity := ES_ERROR
    oErr:Description := hb_UTF8ToStr(buffer)
    Throw(oErr)
    RETURN nil

METHOD ProcessResult(buffer, bufferLen) CLASS ACBrMail
    if bufferLen > STR_LEN
        buffer := Space(bufferLen)
        DllCall(::hHandle, DLL_OSAPI, "MAIL_UltimoRetorno", @buffer, @bufferLen)        
    endif
    RETURN buffer

METHOD Nome CLASS ACBrMail
    local hResult, buffer, bufferLen, ret
    bufferLen := STR_LEN
    buffer := Space(bufferLen)
    hResult := DllCall(::hHandle, DLL_OSAPI, "MAIL_Nome", @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD Versao CLASS ACBrMail
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)
    hResult := DllCall(::hHandle, DLL_OSAPI, "MAIL_Versao", @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD ConfigLer(eArqConfig) CLASS ACBrMail
    local hResult    
    hResult := DllCall(::hHandle, DLL_OSAPI, "MAIL_ConfigLer", hb_StrToUTF8(eArqConfig))
    ::CheckResult(hResult)
    RETURN nil

METHOD ConfigGravar(eArqConfig) CLASS ACBrMail
        local hResult    
        hResult := DllCall(::hHandle, DLL_OSAPI, "MAIL_ConfigGravar", hb_StrToUTF8(eArqConfig))
        ::CheckResult(hResult)
        RETURN nil

METHOD ConfigLerValor(eSessao, eChave) CLASS ACBrMail
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)   
    hResult := DllCall(::hHandle, DLL_OSAPI, "MAIL_ConfigLerValor", hb_StrToUTF8(eSessao), hb_StrToUTF8(eChave), @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD ConfigGravarValor(eSessao, eChave, eValor) CLASS ACBrMail
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "MAIL_ConfigGravarValor", hb_StrToUTF8(eSessao), hb_StrToUTF8(eChave), hb_StrToUTF8(eValor))
    ::CheckResult(hResult)
    RETURN nil

METHOD SetSubject(subject) CLASS ACBrMail
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "MAIL_SetSubject", hb_StrToUTF8(subject))
    ::CheckResult(hResult)
    RETURN nil

METHOD AddAddress(email, name) CLASS ACBrMail
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "MAIL_AddAddress", hb_StrToUTF8(email), hb_StrToUTF8(name))
    ::CheckResult(hResult)
    RETURN nil
    
METHOD AddReplyTo(email, name) CLASS ACBrMail
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "MAIL_AddReplyTo", hb_StrToUTF8(email), hb_StrToUTF8(name))
    ::CheckResult(hResult)
    RETURN nil

METHOD AddCC(email, name) CLASS ACBrMail
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "MAIL_AddCC", hb_StrToUTF8(email), hb_StrToUTF8(name))
    ::CheckResult(hResult)
    RETURN nil

METHOD AddBCC(email) CLASS ACBrMail
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "MAIL_AddBCC", hb_StrToUTF8(email))
    ::CheckResult(hResult)
    RETURN nil
    
METHOD ClearAttachment() CLASS ACBrMail
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "MAIL_ClearAttachment")
    ::CheckResult(hResult)
    RETURN nil
        
METHOD AddAttachment(eFileName, eDescription, aDisposition) CLASS ACBrMail
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "MAIL_AddAttachment", hb_StrToUTF8(eFileName), hb_StrToUTF8(eDescription), aDisposition)
    ::CheckResult(hResult)
    RETURN nil

METHOD AddBody(eBody) CLASS ACBrMail
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "MAIL_AddBody", hb_StrToUTF8(eBody))
    ::CheckResult(hResult)
    RETURN nil
        
METHOD AddAltBody(eAltBody) CLASS ACBrMail
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "MAIL_AddAltBody", hb_StrToUTF8(eAltBody))
    ::CheckResult(hResult)
    RETURN nil
    
METHOD SaveToFile(eFileName) CLASS ACBrMail
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "MAIL_SaveToFile", hb_StrToUTF8(eFileName))
    ::CheckResult(hResult)
    RETURN nil
    
METHOD Clear() CLASS ACBrMail
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "MAIL_Clear")
    ::CheckResult(hResult)
    RETURN nil

METHOD Send() CLASS ACBrMail
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "MAIL_Send")
    ::CheckResult(hResult)
    RETURN nil