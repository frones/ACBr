#include '..\Comum\acbrlib.ch'

#ifdef __PLATFORM__WINDOWS
   #define ACBrLIB 'ACBrMDFe32.dll'
#else
   #ifdef __PLATFORM__LINUX
      #define ACBrLIB 'libacbrmdfe64.so'
   #else
      #error ACBrLIB-FALTA DEFINICAO: PLATFORM__?
   #endif
#endif

CREATE CLASS ACBrMDFe
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

    METHOD CarregarXML(eArquivoOuXml)
    METHOD CarregarINI(eArquivoOuIni)
    METHOD ObterXml(AIndex)
    METHOD GravarXml(AIndex, eNomeArquivo, ePathArquivo)
    METHOD ObterIni(AIndex)
    METHOD GravarIni(AIndex, eNomeArquivo, ePathArquivo)
    METHOD CarregarEventoXML(eArquivoOuXml)
    METHOD CarregarEventoINI(eArquivoOuIni)
    METHOD LimparLista()
    METHOD LimparListaEventos()

    METHOD Assinar()
    METHOD Validar()
    METHOD ValidarRegrasdeNegocios()
    METHOD VerificarAssinatura()
    METHOD GerarChave(ACodigoUF, ACodigoNumerico, AModelo, ASerie, ANumero, ATpEmi, AEmissao, ACNPJCPF)
    METHOD ObterCertificados()
    METHOD GetPath(tipo)
    METHOD GetPathEvento(aCodEvento)

    METHOD StatusServico()
    METHOD Consultar(eChaveOuNFe, AExtrairEventos)
    METHOD EncerrarMDFe(eChaveOuMDFe, eDtEnc, cMunicipioDescarga, nCNPJ, nProtocolo)
    METHOD ConsultaMDFeNaoEnc(ACNPJ)
    METHOD Enviar(ALote, Imprimir, Sincrono, Zipado)
    METHOD ConsultarRecibo(ARecibo)
    METHOD Cancelar(eChave, eJustificativa, eCNPJ, ALote)
    METHOD EnviarEvento(ALote)

    METHOD DistribuicaoDFePorUltNSU(acUFAutor, eCNPJCPF, eultNSU)
    METHOD DistribuicaoDFePorNSU(acUFAutor, eCNPJCPF, eNSU)
    METHOD DistribuicaoDFePorChave(acUFAutor, eCNPJCPF, echNFe)

    METHOD EnviarEmail(ePara, eChaveNFe, aEnviaPDF, eAssunto, eMensagem, eCc, eAnexos)
    METHOD EnviarEmailEvento(ePara, eChaveEvento, eChaveNFe, aEnviaPDF, eAssunto, eMensagem, eCc, eAnexos)

    METHOD Imprimir(cImpressora, nNumCopias, cProtocolo, bMostrarPreview, cMarcaDagua, bViaConsumidor, bSimplificado)
    METHOD ImprimirPDF()
    METHOD ImprimirEvento(eArquivoXmlNFe, eArquivoXmlEvento)
    METHOD ImprimirEventoPDF(eArquivoXmlNFe, eArquivoXmlEvento)
    METHOD ImprimirInutilizacao(eArquivoXml)
    METHOD ImprimirInutilizacaoPDF(eArquivoXml)

END CLASS

METHOD New(eArqConfig, eChaveCrypt) CLASS ACBrMDFe
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
    hResult := DllCall(::hHandle, DLL_OSAPI, "MDFE_Inicializar", hb_StrToUTF8(eArqConfig), hb_StrToUTF8(eChaveCrypt))
    ::CheckResult(hResult) 
    RETURN Self

PROCEDURE Destroy CLASS ACBrMDFe
    DllCall(::hHandle, DLL_OSAPI, "MDFE_Finalizar")
    DllUnload(::hHandle)
    RETURN

METHOD CheckResult(hResult) CLASS ACBrMDFe
    local buffer, bufferLen, oErr
    if hResult >= 0 
       RETURN nil
    endif    

    bufferLen := STR_LEN
    buffer := Space(bufferLen)

    DllCall(::hHandle, DLL_OSAPI, "MDFE_UltimoRetorno", @buffer, @bufferLen)
    if bufferLen > STR_LEN
        buffer := Space(bufferLen)
        DllCall(::hHandle, DLL_OSAPI, "MDFE_UltimoRetorno", @buffer, @bufferLen)        
    endif    

    oErr := ErrorNew()
    oErr:Severity := ES_ERROR
    oErr:Description := hb_UTF8ToStr(buffer)
    Throw(oErr)
    RETURN nil

METHOD ProcessResult(buffer, bufferLen) CLASS ACBrMDFe
    if bufferLen > STR_LEN
        buffer := Space(bufferLen)
        DllCall(::hHandle, DLL_OSAPI, "MDFE_UltimoRetorno", @buffer, @bufferLen)        
    endif
    RETURN buffer

METHOD Nome CLASS ACBrMDFe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)
    hResult := DllCall(::hHandle, DLL_OSAPI, "MDFE_Nome", @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD Versao CLASS ACBrMDFe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)
    hResult := DllCall(::hHandle, DLL_OSAPI, "MDFE_Versao", @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD ConfigLer(eArqConfig) CLASS ACBrMDFe
    local hResult    
    hResult := DllCall(::hHandle, DLL_OSAPI, "MDFE_ConfigLer", hb_StrToUTF8(eArqConfig))
    ::CheckResult(hResult)
    RETURN nil

METHOD ConfigGravar(eArqConfig) CLASS ACBrMDFe
        local hResult    
        hResult := DllCall(::hHandle, DLL_OSAPI, "MDFE_ConfigGravar", hb_StrToUTF8(eArqConfig))
        ::CheckResult(hResult)
        RETURN nil

METHOD ConfigLerValor(eSessao, eChave) CLASS ACBrMDFe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)   
    hResult := DllCall(::hHandle, DLL_OSAPI, "MDFE_ConfigLerValor", hb_StrToUTF8(eSessao), hb_StrToUTF8(eChave), @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD ConfigGravarValor(eSessao, eChave, eValor) CLASS ACBrMDFe
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "MDFE_ConfigGravarValor", hb_StrToUTF8(eSessao), hb_StrToUTF8(eChave), hb_StrToUTF8(eValor))
    ::CheckResult(hResult)
    RETURN nil

METHOD CarregarXML(eArquivoOuXml) CLASS ACBrMDFe
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "MDFE_CarregarXML", hb_StrToUTF8(eArquivoOuXml))
    ::CheckResult(hResult)
    RETURN nil
    
METHOD CarregarINI(eArquivoOuIni) CLASS ACBrMDFe
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "MDFE_CarregarINI", hb_StrToUTF8(eArquivoOuIni))
    ::CheckResult(hResult)
    RETURN nil

METHOD ObterXml(AIndex) CLASS ACBrMDFe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)   
    hResult := DllCall(::hHandle, DLL_OSAPI, "MDFE_ObterXml", AIndex, @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD GravarXml(AIndex, eNomeArquivo, ePathArquivo) CLASS ACBrMDFe
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "MDFE_GravarXml", AIndex, hb_StrToUTF8(eNomeArquivo), hb_StrToUTF8(ePathArquivo))
    ::CheckResult(hResult)
    RETURN nil

METHOD ObterIni(AIndex) CLASS ACBrMDFe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)   
    hResult := DllCall(::hHandle, DLL_OSAPI, "MDFE_ObterIni", AIndex, @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)
    
METHOD GravarIni(AIndex, eNomeArquivo, ePathArquivo) CLASS ACBrMDFe
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "MDFE_GravarIni", AIndex, hb_StrToUTF8(eNomeArquivo), hb_StrToUTF8(ePathArquivo))
    ::CheckResult(hResult)
    RETURN nil

METHOD CarregarEventoXML(eArquivoOuXml) CLASS ACBrMDFe
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "MDFE_CarregarEventoXML", hb_StrToUTF8(eArquivoOuXml))
    ::CheckResult(hResult)
    RETURN nil

METHOD CarregarEventoINI(eArquivoOuIni) CLASS ACBrMDFe
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "MDFE_CarregarEventoINI", hb_StrToUTF8(eArquivoOuIni))
    ::CheckResult(hResult)
    RETURN nil

METHOD LimparLista() CLASS ACBrMDFe
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "MDFE_LimparLista")
    ::CheckResult(hResult)
    RETURN nil

METHOD LimparListaEventos() CLASS ACBrMDFe
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "MDFE_LimparListaEventos")
    ::CheckResult(hResult)
    RETURN nil

METHOD Assinar() CLASS ACBrMDFe
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "MDFE_Assinar")
    ::CheckResult(hResult)
    RETURN nil

METHOD Validar() CLASS ACBrMDFe
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "MDFE_Validar")
    ::CheckResult(hResult)
    RETURN nil

METHOD ValidarRegrasdeNegocios() CLASS ACBrMDFe
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "MDFE_ValidarRegrasdeNegocios")
    ::CheckResult(hResult)
    RETURN nil

METHOD VerificarAssinatura() CLASS ACBrMDFe
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "MDFE_VerificarAssinatura")
    ::CheckResult(hResult)
    RETURN nil

METHOD GerarChave(ACodigoUF, ACodigoNumerico, AModelo, ASerie, ANumero, ATpEmi, AEmissao, ACNPJCPF) CLASS ACBrMDFe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)
    hResult := DllCall(::hHandle, DLL_OSAPI, "MDFE_GerarChave", ACodigoUF, ACodigoNumerico, AModelo, ASerie, ANumero, ATpEmi, AEmissao, ACNPJCPF, @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD ObterCertificados() CLASS ACBrMDFe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)
    hResult := DllCall(::hHandle, DLL_OSAPI, "MDFE_ObterCertificados", @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD GetPath(tipo)
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)
    hResult := DllCall(::hHandle, DLL_OSAPI, "MDFE_GetPath", tipo, @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD GetPathEvento(aCodEvento)
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)
    hResult := DllCall(::hHandle, DLL_OSAPI, "MDFE_GetPathEvento", hb_StrToUTF8(aCodEvento), @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD StatusServico() CLASS ACBrMDFe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)
    hResult := DllCall(::hHandle, DLL_OSAPI, "MDFE_StatusServico", @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD Consultar(eChaveOuNFe, AExtrairEventos) CLASS ACBrMDFe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)
    hResult := DllCall(::hHandle, DLL_OSAPI, "MDFE_Consultar", hb_StrToUTF8(eChaveOuNFe), AExtrairEventos, @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD EncerrarMDFe(eChaveOuMDFe, eDtEnc, cMunicipioDescarga, nCNPJ, nProtocolo) CLASS ACBrMDFe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)
    hResult := DllCall(::hHandle, DLL_OSAPI, "MDFE_EncerrarMDFe", hb_StrToUTF8(eChaveOuMDFe), hb_StrToUTF8(eDtEnc), cMunicipioDescarga, hb_StrToUTF8(nCNPJ), hb_StrToUTF8(nProtocolo), @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD ConsultaMDFeNaoEnc(ACNPJ) CLASS ACBrMDFe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)
    hResult := DllCall(::hHandle, DLL_OSAPI, "MDFE_ConsultaMDFeNaoEnc", hb_StrToUTF8(ACNPJ), @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD Enviar(ALote, Imprimir, Sincrono, Zipado) CLASS ACBrMDFe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)
    hResult := DllCall(::hHandle, DLL_OSAPI, "MDFE_Enviar", ALote, Imprimir, Sincrono, Zipado, @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD ConsultarRecibo(ARecibo) CLASS ACBrMDFe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)
    hResult := DllCall(::hHandle, DLL_OSAPI, "MDFE_ConsultarRecibo", hb_StrToUTF8(ARecibo), @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD Cancelar(eChave, eJustificativa, eCNPJ, ALote) CLASS ACBrMDFe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)
    hResult := DllCall(::hHandle, DLL_OSAPI, "MDFE_Cancelar", hb_StrToUTF8(eChave), hb_StrToUTF8(eJustificativa), hb_StrToUTF8(eCNPJ), ALote, @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD EnviarEvento(ALote) CLASS ACBrMDFe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)
    hResult := DllCall(::hHandle, DLL_OSAPI, "MDFE_EnviarEvento", ALote, @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD DistribuicaoDFePorUltNSU(acUFAutor, eCNPJCPF, eultNSU) CLASS ACBrMDFe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)
    hResult := DllCall(::hHandle, DLL_OSAPI, "MDFE_DistribuicaoDFePorUltNSU", acUFAutor, hb_StrToUTF8(eCNPJCPF), hb_StrToUTF8(eultNSU), @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD DistribuicaoDFePorNSU(acUFAutor, eCNPJCPF, eNSU) CLASS ACBrMDFe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)
    hResult := DllCall(::hHandle, DLL_OSAPI, "MDFE_DistribuicaoDFePorNSU", acUFAutor, hb_StrToUTF8(eCNPJCPF), hb_StrToUTF8(eNSU), @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD DistribuicaoDFePorChave(acUFAutor, eCNPJCPF, echNFe) CLASS ACBrMDFe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)
    hResult := DllCall(::hHandle, DLL_OSAPI, "MDFE_DistribuicaoDFePorChave", acUFAutor, hb_StrToUTF8(eCNPJCPF), hb_StrToUTF8(echNFe), @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD EnviarEmail(ePara, eChaveNFe, aEnviaPDF, eAssunto, eMensagem, eCc, eAnexos) CLASS ACBrMDFe
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "MDFE_EnviarEmail", hb_StrToUTF8(ePara), hb_StrToUTF8(eChaveNFe), aEnviaPDF, hb_StrToUTF8(eAssunto), hb_StrToUTF8(eMensagem), hb_StrToUTF8(eCc), hb_StrToUTF8(eAnexos))
    ::CheckResult(hResult)
    RETURN nil

METHOD EnviarEmailEvento(ePara, eChaveEvento, eChaveNFe, aEnviaPDF, eAssunto, eMensagem, eCc, eAnexos) CLASS ACBrMDFe
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "MDFE_EnviarEmailEvento", hb_StrToUTF8(ePara), hb_StrToUTF8(eChaveEvento), hb_StrToUTF8(eChaveNFe), aEnviaPDF, hb_StrToUTF8(eAssunto), hb_StrToUTF8(eMensagem), hb_StrToUTF8(eCc), hb_StrToUTF8(eAnexos))
    ::CheckResult(hResult)
    RETURN nil

METHOD Imprimir(cImpressora, nNumCopias, cProtocolo, bMostrarPreview, cMarcaDagua, bViaConsumidor, bSimplificado) CLASS ACBrMDFe
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "MDFE_Imprimir", hb_StrToUTF8(cImpressora), nNumCopias, hb_StrToUTF8(cProtocolo), hb_StrToUTF8(bMostrarPreview), hb_StrToUTF8(cMarcaDagua), hb_StrToUTF8(bViaConsumidor), hb_StrToUTF8(bSimplificado))
    ::CheckResult(hResult)
    RETURN nil

METHOD ImprimirPDF() CLASS ACBrMDFe
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "MDFE_ImprimirPDF")
    ::CheckResult(hResult)
    RETURN nil

METHOD ImprimirEvento(eArquivoXmlNFe, eArquivoXmlEvento) CLASS ACBrMDFe
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "MDFE_ImprimirEvento", hb_StrToUTF8(eArquivoXmlNFe), hb_StrToUTF8(eArquivoXmlEvento))
    ::CheckResult(hResult)
    RETURN nil

METHOD ImprimirEventoPDF(eArquivoXmlNFe, eArquivoXmlEvento) CLASS ACBrMDFe
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "MDFE_ImprimirEventoPDF", hb_StrToUTF8(eArquivoXmlNFe), hb_StrToUTF8(eArquivoXmlEvento))
    ::CheckResult(hResult)
    RETURN nil

METHOD ImprimirInutilizacao(eArquivoXml) CLASS ACBrMDFe
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "MDFE_ImprimirInutilizacao", hb_StrToUTF8(eArquivoXml))
    ::CheckResult(hResult)
    RETURN nil

METHOD ImprimirInutilizacaoPDF(eArquivoXml) CLASS ACBrMDFe
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "MDFE_ImprimirInutilizacaoPDF", hb_StrToUTF8(eArquivoXml))
    ::CheckResult(hResult)
    RETURN nil