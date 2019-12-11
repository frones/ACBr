#include 'acbrlib.ch'

#ifdef __PLATFORM__WINDOWS
   #define ACBrLIB 'ACBrSAT32.dll'
#else
   #ifdef __PLATFORM__LINUX
      #define ACBrLIB 'libacbrsat64.so'
   #else
      #error ACBrLIB-FALTA DEFINICAO: PLATFORM__?
   #endif
#endif

CREATE CLASS ACBrSat
HIDDEN:
    VAR hHandle

    METHOD CheckResult(hResult)
    METHOD ProcessResult(buffer, bufferLen)

VISIBLE:
    METHOD New(eArqConfig, eChaveCrypt) CONSTRUCTOR
    DESTRUCTOR  Destroy

    METHOD Nome
    METHOD Versao

    METHOD ConfigLer(eArqConfig)
    METHOD ConfigGravar(eArqConfig)
    METHOD ConfigLerValor(eSessao, eChave)
    METHOD ConfigGravarValor(eSessao, eChave, eValor)

    METHOD Inicializar
    METHOD DesInicializar

    METHOD AssociarAssinatura(CNPJvalue, assinaturaCNPJs)
    METHOD BloquearSAT()
    METHOD DesbloquearSAT()
    METHOD TrocarCodigoDeAtivacao(codigoDeAtivacaoOuEmergencia, opcao, novoCodigo)
    METHOD ConsultarSAT()
    METHOD ConsultarStatusOperacional()
    METHOD ConsultarNumeroSessao(cNumeroDeSessao)
    METHOD AtualizarSoftwareSAT()
    METHOD ComunicarCertificadoICPBRASIL(certificado)
    METHOD ExtrairLogs(eArquivo)
    METHOD TesteFimAFim(eArquivoXmlVenda)
    METHOD GerarAssinaturaSAT(eCNPJSHW, eCNPJEmitente)

    METHOD CriarCFe(eArquivoIni)
    METHOD CriarEnviarCFe(eArquivoIni)
    METHOD EnviarCFe(eArquivoXml)
    METHOD CancelarCFe(eArquivoXml)

    METHOD ImprimirExtratoVenda(eArqXMLVenda, eNomeImpressora)
    METHOD ImprimirExtratoResumido(eArqXMLVenda, eNomeImpressora)
    METHOD ImprimirExtratoCancelamento(eArqXMLVenda, eArqXMLCancelamento, eNomeImpressora)
    METHOD GerarImpressaoFiscalMFe(eArqXMLVenda)
    METHOD GerarPDFExtratoVenda(eArqXMLVenda, eNomeArquivo)
    METHOD GerarPDFCancelamento(eArqXMLVenda, eArqXMLCancelamento, eNomeArquivo)
    METHOD EnviarEmail(eArqXMLVenda, sPara, sAssunto, eNomeArquivo, sMensagem, sCC, eAnexos)
END CLASS

METHOD New(eArqConfig, eChaveCrypt) CLASS ACBrSat
    local hResult, buffer, bufferLen, oErr

    eArqConfig :=if(eArqConfig = nil, '', eArqConfig)
    eChaveCrypt:=if(eChaveCrypt = nil, '', eChaveCrypt)

    ::hHandle := DllLoad(ACBrLIBSat)
    if EMPTY(::hHandle) // Eric.Developer: xHarbour retorna 0x00000000
        oErr := ErrorNew()
        oErr:Severity := ES_ERROR        
        oErr:Description := "Erro a carregar a dll [" + ACBrLIBSat + "]"
        Throw(oErr)
    endif
    hResult := DllCall(::hHandle, DLL_OSAPI, "SAT_Inicializar", hb_StrToUTF8(eArqConfig), hb_StrToUTF8(eChaveCrypt))
    ::CheckResult(hResult) 
    RETURN Self

PROCEDURE Destroy CLASS ACBrSat
    DllCall(::hHandle, DLL_OSAPI, "SAT_Finalizar")
    DllUnload(::hHandle)
    RETURN

METHOD CheckResult(hResult) CLASS ACBrSat
    local buffer, bufferLen, oErr
    if hResult >= 0 
       RETURN nil
    endif    

    bufferLen := STR_LEN
    buffer := Space(bufferLen)

    DllCall(::hHandle, DLL_OSAPI, "SAT_UltimoRetorno", @buffer, @bufferLen)
    if bufferLen > STR_LEN
        buffer := Space(bufferLen)
        DllCall(::hHandle, DLL_OSAPI, "SAT_UltimoRetorno", @buffer, @bufferLen)        
    endif    

    oErr := ErrorNew()
    oErr:Severity := ES_ERROR
    oErr:Description := hb_UTF8ToStr(buffer)
    Throw(oErr)
    RETURN nil

METHOD ProcessResult(buffer, bufferLen) CLASS ACBrSat
    if bufferLen > STR_LEN
        buffer := Space(bufferLen)
        DllCall(::hHandle, DLL_OSAPI, "SAT_UltimoRetorno", @buffer, @bufferLen)        
    endif
    RETURN buffer

METHOD Nome CLASS ACBrSat
    local hResult, buffer, bufferLen, ret
    bufferLen := STR_LEN
    buffer := Space(bufferLen)
    hResult := DllCall(::hHandle, DLL_OSAPI, "SAT_Nome", @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD Versao CLASS ACBrSat
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)
    hResult := DllCall(::hHandle, DLL_OSAPI, "SAT_Versao", @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD ConfigLer(eArqConfig) CLASS ACBrSat
    local hResult    
    hResult := DllCall(::hHandle, DLL_OSAPI, "SAT_ConfigLer", hb_StrToUTF8(eArqConfig))
    ::CheckResult(hResult)
    RETURN nil

METHOD ConfigGravar(eArqConfig) CLASS ACBrSat
        local hResult    
        hResult := DllCall(::hHandle, DLL_OSAPI, "SAT_ConfigGravar", hb_StrToUTF8(eArqConfig))
        ::CheckResult(hResult)
        RETURN nil

METHOD ConfigLerValor(eSessao, eChave) CLASS ACBrSat
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)   
    hResult := DllCall(::hHandle, DLL_OSAPI, "SAT_ConfigLerValor", hb_StrToUTF8(eSessao), hb_StrToUTF8(eChave), @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD ConfigGravarValor(eSessao, eChave, eValor) CLASS ACBrSat
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "SAT_ConfigGravarValor", hb_StrToUTF8(eSessao), hb_StrToUTF8(eChave), hb_StrToUTF8(eValor))
    ::CheckResult(hResult)
    RETURN nil

METHOD Inicializar CLASS ACBrSat
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "SAT_InicializarSAT")
    ::CheckResult(hResult)
    RETURN nil

METHOD DesInicializar CLASS ACBrSat
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "SAT_DesInicializar")
    ::CheckResult(hResult)
    RETURN nil

METHOD AssociarAssinatura(CNPJvalue, assinaturaCNPJs) CLASS ACBrSat
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)   
    hResult := DllCall(::hHandle, DLL_OSAPI, "SAT_AssociarAssinatura", hb_StrToUTF8(CNPJvalue), hb_StrToUTF8(assinaturaCNPJs), @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD BloquearSAT() CLASS ACBrSat
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)   
    hResult := DllCall(::hHandle, DLL_OSAPI, "SAT_BloquearSAT", @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD DesbloquearSAT() CLASS ACBrSat
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)   
    hResult := DllCall(::hHandle, DLL_OSAPI, "SAT_DesbloquearSAT", @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD TrocarCodigoDeAtivacao(codigoDeAtivacaoOuEmergencia, opcao, novoCodigo)
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)   
    hResult := DllCall(::hHandle, DLL_OSAPI, "SAT_TrocarCodigoDeAtivacao", hb_StrToUTF8(codigoDeAtivacaoOuEmergencia), opcao, hb_StrToUTF8(novoCodigo), @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD ConsultarSAT() CLASS ACBrSat
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)   
    hResult := DllCall(::hHandle, DLL_OSAPI, "SAT_ConsultarSAT", @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD ConsultarStatusOperacional() CLASS ACBrSat
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)   
    hResult := DllCall(::hHandle, DLL_OSAPI, "SAT_ConsultarStatusOperacional", @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD ConsultarNumeroSessao(cNumeroDeSessao) CLASS ACBrSat
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)   
    hResult := DllCall(::hHandle, DLL_OSAPI, "SAT_ConsultarNumeroSessao", cNumeroDeSessao, @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD AtualizarSoftwareSAT() CLASS ACBrSat
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)   
    hResult := DllCall(::hHandle, DLL_OSAPI, "SAT_AtualizarSoftwareSAT", @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD ComunicarCertificadoICPBRASIL(certificado) CLASS ACBrSat
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)   
    hResult := DllCall(::hHandle, DLL_OSAPI, "SAT_ComunicarCertificadoICPBRASIL", hb_StrToUTF8(certificado), @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD ExtrairLogs(eArquivo) CLASS ACBrSat
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "SAT_ExtrairLogs", hb_StrToUTF8(eArquivo))
    ::CheckResult(hResult)
    RETURN nil

METHOD TesteFimAFim(eArquivoXmlVenda) CLASS ACBrSat
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)   
    hResult := DllCall(::hHandle, DLL_OSAPI, "SAT_TesteFimAFim", hb_StrToUTF8(eArquivoXmlVenda), @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD GerarAssinaturaSAT(eCNPJSHW, eCNPJEmitente) CLASS ACBrSat
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)   
    hResult := DllCall(::hHandle, DLL_OSAPI, "SAT_GerarAssinaturaSAT", hb_StrToUTF8(eCNPJSHW), hb_StrToUTF8(eCNPJEmitente), @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD CriarCFe(eArquivoIni) CLASS ACBrSat
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)   
    hResult := DllCall(::hHandle, DLL_OSAPI, "SAT_CriarCFe", hb_StrToUTF8(eArquivoIni), @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD CriarEnviarCFe(eArquivoIni) CLASS ACBrSat
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)   
    hResult := DllCall(::hHandle, DLL_OSAPI, "SAT_CriarEnviarCFe", hb_StrToUTF8(eArquivoIni), @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD EnviarCFe(eArquivoXml) CLASS ACBrSat
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)   
    hResult := DllCall(::hHandle, DLL_OSAPI, "SAT_EnviarCFe", hb_StrToUTF8(eArquivoXml), @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD CancelarCFe(eArquivoXml) CLASS ACBrSat
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)   
    hResult := DllCall(::hHandle, DLL_OSAPI, "SAT_CancelarCFe", hb_StrToUTF8(eArquivoXml), @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD ImprimirExtratoVenda(eArqXMLVenda, eNomeImpressora) CLASS ACBrSat
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "SAT_ImprimirExtratoVenda", hb_StrToUTF8(eArqXMLVenda), hb_StrToUTF8(eNomeImpressora))
    ::CheckResult(hResult)
    RETURN nil

METHOD ImprimirExtratoResumido(eArqXMLVenda, eNomeImpressora) CLASS ACBrSat
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "SAT_ImprimirExtratoResumido", hb_StrToUTF8(eArqXMLVenda), hb_StrToUTF8(eNomeImpressora))
    ::CheckResult(hResult)
    RETURN nil

METHOD ImprimirExtratoCancelamento(eArqXMLVenda, eArqXMLCancelamento, eNomeImpressora) CLASS ACBrSat
    local hResult 
    hResult := DllCall(::hHandle, DLL_OSAPI, "SAT_ImprimirExtratoCancelamento", hb_StrToUTF8(eArqXMLVenda), hb_StrToUTF8(eArqXMLCancelamento), hb_StrToUTF8(eNomeImpressora))
    ::CheckResult(hResult)
    RETURN nil

METHOD GerarImpressaoFiscalMFe(eArqXMLVenda) CLASS ACBrSat
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)   
    hResult := DllCall(::hHandle, DLL_OSAPI, "SAT_GerarImpressaoFiscalMFe", hb_StrToUTF8(eArqXMLVenda), @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD GerarPDFExtratoVenda(eArqXMLVenda, eNomeArquivo) CLASS ACBrSat
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)   
    hResult := DllCall(::hHandle, DLL_OSAPI, "SAT_GerarPDFExtratoVenda", hb_StrToUTF8(eArqXMLVenda), hb_StrToUTF8(eNomeArquivo), @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD GerarPDFCancelamento(eArqXMLVenda, eArqXMLCancelamento, eNomeArquivo)
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)   
    hResult := DllCall(::hHandle, DLL_OSAPI, "SAT_GerarPDFCancelamento", hb_StrToUTF8(eArqXMLVenda), hb_StrToUTF8(eArqXMLCancelamento), hb_StrToUTF8(eNomeArquivo), @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD EnviarEmail(eArqXMLVenda, sPara, sAssunto, eNomeArquivo, sMensagem, sCC, eAnexos)
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "SAT_EnviarEmail", hb_StrToUTF8(eArqXMLVenda), hb_StrToUTF8(sPara), hb_StrToUTF8(sAssunto), hb_StrToUTF8(eNomeArquivo), hb_StrToUTF8(sMensagem), hb_StrToUTF8(sCC), hb_StrToUTF8(eAnexos))
    ::CheckResult(hResult)
    RETURN nil

    
