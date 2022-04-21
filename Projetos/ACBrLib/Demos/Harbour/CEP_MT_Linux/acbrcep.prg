// Exemplo de uso da ACBrLibCEP, versão Multi Thread (MT), CDECL

#include 'acbrlib.ch'

#ifdef __PLATFORM__WINDOWS
   #define ACBrLIB 'ACBrCEP32.dll'
#else
   #ifdef __PLATFORM__LINUX
      #define ACBrLIB  'libacbrcep64.so'
   #else
      #error ACBrLIB-FALTA DEFINICAO: PLATFORM__?
   #endif
#endif

CREATE CLASS ACBrCEP
HIDDEN:
    VAR MTHandle    

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
    METHOD ConfigImportar(eArqConfig)
    METHOD ConfigExportar()
    
    METHOD BuscarPorCEP(eCEP)
    METHOD BuscarPorLogradouro(eCidade, eTipo_Logradouro, eLogradouro, eUF, eBairro)

END CLASS

METHOD New(eArqConfig, eChaveCrypt) CLASS ACBrCEP
    local hResult, buffer, bufferLen, oErr

    eArqConfig :=if(eArqConfig = nil, '', eArqConfig)
    eChaveCrypt:=if(eChaveCrypt = nil, '', eChaveCrypt)

    ::MTHandle :=  hb_LibLoad(ACBrLIB)
    if EMPTY(::MTHandle) // Eric.Developer: xHarbour retorna 0x00000000
        oErr := ErrorNew()
        oErr:Severity := ES_ERROR        
        oErr:Description := "Erro a carregar a dll [" + ACBrLIB + "]"
        Throw(oErr)
    endif

    hResult := hb_DynCall({"CEP_Inicializar", ACBrLIB, DLL_OSAPI}, @::MTHandle, hb_StrToUTF8(eArqConfig), hb_StrToUTF8(eChaveCrypt))
    ::CheckResult(hResult) 
    RETURN Self

PROCEDURE Destroy CLASS ACBrCEP
    hb_DynCall({"CEP_Finalizar", ACBrLIB, DLL_OSAPI}, ::MTHandle)
    hb_LibFree(ACBrLIB)
    //hb_LibFree(::MTHandle)
    RETURN

METHOD CheckResult(hResult) CLASS ACBrCEP
    local buffer, bufferLen, oErr
    if hResult >= 0 
       RETURN nil
    endif    

    bufferLen := STR_LEN
    buffer := Space(bufferLen)

    hb_DynCall({"CEP_UltimoRetorno", ACBrLIB, DLL_OSAPI}, ::MTHandle, @buffer, @bufferLen)
    if bufferLen > STR_LEN
        buffer := Space(bufferLen)
        hb_DynCall({"CEP_UltimoRetorno", ACBrLIB, DLL_OSAPI}, ::MTHandle, @buffer, @bufferLen)
    endif    

    oErr := ErrorNew()
    oErr:Severity := ES_ERROR
    oErr:Description := hb_UTF8ToStr(buffer)
    Throw(oErr)
    RETURN nil

METHOD ProcessResult(buffer, bufferLen) CLASS ACBrCEP
    if bufferLen > STR_LEN
        buffer := Space(bufferLen)
        hb_DynCall({"CEP_UltimoRetorno", ACBrLIB, DLL_OSAPI}, ::MTHandle, @buffer, @bufferLen)
    endif
    RETURN buffer

METHOD Nome CLASS ACBrCEP
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)
    hResult := hb_DynCall({"CEP_Nome", ACBrLIB, DLL_OSAPI}, ::MTHandle, @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD Versao CLASS ACBrCEP
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)
    hResult := hb_DynCall({"CEP_Versao", ACBrLIB, DLL_OSAPI}, ::MTHandle, @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD ConfigLer(eArqConfig) CLASS ACBrCEP
    local hResult    
    hResult := hb_DynCall({"CEP_ConfigLer", ACBrLIB, DLL_OSAPI}, ::MTHandle, hb_StrToUTF8(eArqConfig))
    ::CheckResult(hResult)
    RETURN nil

METHOD ConfigGravar(eArqConfig) CLASS ACBrCEP
        local hResult    
        hResult := hb_DynCall({"CEP_ConfigGravar", ACBrLIB, DLL_OSAPI}, ::MTHandle, hb_StrToUTF8(eArqConfig))
        ::CheckResult(hResult)
        RETURN nil

METHOD ConfigLerValor(eSessao, eChave) CLASS ACBrCEP
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)   
    hResult := hb_DynCall({"CEP_ConfigLerValor", ACBrLIB, DLL_OSAPI}, ::MTHandle, hb_StrToUTF8(eSessao), hb_StrToUTF8(eChave), @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD ConfigGravarValor(eSessao, eChave, eValor) CLASS ACBrCEP
    local hResult
    hResult := hb_DynCall({"CEP_ConfigGravarValor", ACBrLIB, DLL_OSAPI}, ::MTHandle, hb_StrToUTF8(eSessao), hb_StrToUTF8(eChave), hb_StrToUTF8(eValor))
    ::CheckResult(hResult)
    RETURN nil

METHOD ConfigImportar(eArqConfig) CLASS ACBrCEP
    local hResult    
    hResult := hb_DynCall({"CEP_ConfigImportar", ACBrLIB, DLL_OSAPI}, ::MTHandle, hb_StrToUTF8(eArqConfig))
    ::CheckResult(hResult)
    RETURN nil

METHOD ConfigExportar() CLASS ACBrCEP
    local hResult
    hResult := hb_DynCall({"CEP_ConfigExportar", ACBrLIB, DLL_OSAPI}, ::MTHandle )
    ::CheckResult(hResult)
    RETURN nil

METHOD BuscarPorCEP(eCEP) CLASS ACBrCEP
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)   
    hResult := hb_DynCall({"CEP_BuscarPorCEP", ACBrLIB, DLL_OSAPI}, ::MTHandle, eCEP, @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD BuscarPorLogradouro(eCidade, eTipo_Logradouro, eLogradouro, eUF, eBairro) CLASS ACBrCEP
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)   
    hResult := hb_DynCall({"CEP_BuscarPorLogradouro", ACBrLIB, DLL_OSAPI}, ::MTHandle, hb_StrToUTF8(eCidade), hb_StrToUTF8(eTipo_Logradouro), hb_StrToUTF8(eLogradouro), hb_StrToUTF8(eUF), hb_StrToUTF8(eBairro), @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)
