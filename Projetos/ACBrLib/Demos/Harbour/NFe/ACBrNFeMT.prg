#include '..\Comum\acbrlib.ch'

#ifdef __PLATFORM__WINDOWS
#define ACBrLIB 'ACBrNFe32.dll'
#else
#ifdef __PLATFORM__LINUX
#define ACBrLIB 'libacbrnfe64.so'
#else
#error ACBrLIB-FALTA DEFINICAO: PLATFORM__?
#endif
#endif

CREATE CLASS ACBrNFe
    HIDDEN:
    VAR MTHandle   

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
    METHOD Inutilizar(ACNPJ, AJustificativa, Ano, Modelo, Serie, NumeroInicial, NumeroFinal)
    METHOD Enviar(ALote, Imprimir, Sincrono, Zipado)
    METHOD ConsultarRecibo(ARecibo)
    METHOD Cancelar(eChave, eJustificativa, eCNPJ, ALote)
    METHOD EnviarEvento(ALote)

    METHOD DistribuicaoDFePorUltNSU(acUFAutor, eCNPJCPF, eultNSU)
    METHOD DistribuicaoDFePorNSU(acUFAutor, eCNPJCPF, eNSU)
    METHOD DistribuicaoDFePorChave(acUFAutor, eCNPJCPF, echNFe)

    METHOD EnviarEmail(ePara, eXMLNFe, aEnviaPDF, eAssunto, eCc, eAnexos, eMensagem)
    METHOD EnviarEmailEvento(ePara, eXMLEvento, eXMLNFe, aEnviaPDF, eAssunto, eCc, eAnexos, eMensagem)

    METHOD Imprimir(cImpressora, nNumCopias, cProtocolo, bMostrarPreview, cMarcaDagua, bViaConsumidor, bSimplificado)
    METHOD ImprimirPDF()
    METHOD ImprimirEvento(eArquivoXmlNFe, eArquivoXmlEvento)
    METHOD ImprimirEventoPDF(eArquivoXmlNFe, eArquivoXmlEvento)
    METHOD ImprimirInutilizacao(eArquivoXml)
    METHOD ImprimirInutilizacaoPDF(eArquivoXml)

END CLASS

METHOD New(eArqConfig, eChaveCrypt) CLASS ACBrNFe
    local hResult, buffer, bufferLen, oErr

    eArqConfig :=if(eArqConfig = nil, '', eArqConfig)
    eChaveCrypt:=if(eChaveCrypt = nil, '', eChaveCrypt)

    ::MTHandle := hb_libLoad(ACBrLIB)
    if EMPTY(::MTHandle) // Eric.Developer: xHarbour retorna 0x00000000
        oErr := ErrorNew()
        oErr:Severity := ES_ERROR        
        oErr:Description := "Erro a carregar a dll [" + ACBrLIB + "]"
        Throw(oErr)
    endif

    hResult := hb_DynCall({"NFE_Inicializar", ACBrLIB, DLL_OSAPI}, @::MTHandle, hb_StrToUTF8(eArqConfig), hb_StrToUTF8(eChaveCrypt))
    ::CheckResult(hResult) 
RETURN Self

METHOD Destroy CLASS ACBrNFe
    hb_DynCall({"NFE_Finalizar", ACBrLIB, DLL_OSAPI}, ::MTHandle)
    hb_libFree(ACBrLIB)
    //hb_LibFree(::MTHandle)
RETURN nil

METHOD CheckResult(hResult) CLASS ACBrNFe
    local buffer, bufferLen, oErr
    if hResult >= 0 
        RETURN nil
    endif    

    bufferLen := STR_LEN
    buffer := Space(bufferLen)

    hb_DynCall({"NFE_UltimoRetorno", ACBrLIB, DLL_OSAPI}, ::MTHandle, @buffer, @bufferLen)
    if bufferLen > STR_LEN
        buffer := Space(bufferLen)
        hb_DynCall({"NFE_UltimoRetorno", ACBrLIB, DLL_OSAPI}, ::MTHandle, @buffer, @bufferLen)        
    endif    

    oErr := ErrorNew()
    oErr:Severity := ES_ERROR
    oErr:Description := hb_UTF8ToStr(buffer)
    Throw(oErr)
RETURN nil

METHOD ProcessResult(buffer, bufferLen) CLASS ACBrNFe
    if bufferLen > STR_LEN
        buffer := Space(bufferLen)
        hb_DynCall({"NFE_UltimoRetorno", ACBrLIB, DLL_OSAPI}, ::MTHandle, @buffer, @bufferLen)        
    endif
RETURN buffer

METHOD Nome CLASS ACBrNFe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)
    hResult := hb_DynCall({"NFE_Nome", ACBrLIB, DLL_OSAPI}, ::MTHandle , @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN ::ProcessResult(buffer, bufferLen)

METHOD Versao CLASS ACBrNFe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)
    hResult := hb_DynCall({"NFE_Versao", ACBrLIB, DLL_OSAPI}, ::MTHandle ,@buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN ::ProcessResult(buffer, bufferLen)

METHOD ConfigLer(eArqConfig) CLASS ACBrNFe
    local hResult    
    hResult := hb_DynCall({"NFE_ConfigLer", ACBrLIB, DLL_OSAPI}, ::MTHandle, hb_StrToUTF8(eArqConfig))
    ::CheckResult(hResult)
RETURN nil

METHOD ConfigGravar(eArqConfig) CLASS ACBrNFe
    local hResult    
    hResult := hb_DynCall({"NFE_ConfigGravar", ACBrLIB, DLL_OSAPI}, ::MTHandle, hb_StrToUTF8(eArqConfig))
    ::CheckResult(hResult)
RETURN nil

METHOD ConfigLerValor(eSessao, eChave) CLASS ACBrNFe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)   
    hResult := hb_DynCall({"NFE_ConfigLerValor", ACBrLIB, DLL_OSAPI}, ::MTHandle, hb_StrToUTF8(eSessao), hb_StrToUTF8(eChave), @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN ::ProcessResult(buffer, bufferLen)

METHOD ConfigGravarValor(eSessao, eChave, eValor) CLASS ACBrNFe
    local hResult
    hResult := hb_DynCall({"NFE_ConfigGravarValor", ACBrLIB, DLL_OSAPI}, ::MTHandle, hb_StrToUTF8(eSessao), hb_StrToUTF8(eChave), hb_StrToUTF8(eValor))
    ::CheckResult(hResult)
RETURN nil

METHOD CarregarXML(eArquivoOuXml) CLASS ACBrNFe
    local hResult
    hResult := hb_DynCall({"NFE_CarregarXML", ACBrLIB, DLL_OSAPI}, ::MTHandle, hb_StrToUTF8(eArquivoOuXml))
    ::CheckResult(hResult)
RETURN nil
    
METHOD CarregarINI(eArquivoOuIni) CLASS ACBrNFe
    local hResult
    hResult := hb_DynCall({"NFE_CarregarINI", ACBrLIB, DLL_OSAPI}, ::MTHandle, hb_StrToUTF8(eArquivoOuIni))
    ::CheckResult(hResult)
RETURN nil

METHOD ObterXml(AIndex) CLASS ACBrNFe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)   
    hResult := hb_DynCall({"NFE_ObterXml", ACBrLIB, DLL_OSAPI}, ::MTHandle, AIndex, @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN ::ProcessResult(buffer, bufferLen)

METHOD GravarXml(AIndex, eNomeArquivo, ePathArquivo) CLASS ACBrNFe
    local hResult
    hResult := DllCall({"NFE_GravarXml", ACBrLIB, DLL_OSAPI}, ::MTHandle, AIndex, hb_StrToUTF8(eNomeArquivo), hb_StrToUTF8(ePathArquivo))
    ::CheckResult(hResult)
RETURN nil

METHOD ObterIni(AIndex) CLASS ACBrNFe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)   
    hResult := hb_DynCall({"NFE_ObterIni", ACBrLIB, DLL_OSAPI}, ::MTHandle, AIndex, @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN ::ProcessResult(buffer, bufferLen)
    
METHOD GravarIni(AIndex, eNomeArquivo, ePathArquivo) CLASS ACBrNFe
    local hResult
    hResult := hb_DynCall({"NFE_GravarIni", ACBrLIB, DLL_OSAPI}, ::MTHandle, AIndex, hb_StrToUTF8(eNomeArquivo), hb_StrToUTF8(ePathArquivo))
    ::CheckResult(hResult)
RETURN nil

METHOD CarregarEventoXML(eArquivoOuXml) CLASS ACBrNFe
    local hResult
    hResult := hb_DynCall({"NFE_CarregarEventoXML", ACBrLIB, DLL_OSAPI}, ::MTHandle, hb_StrToUTF8(eArquivoOuXml))
    ::CheckResult(hResult)
RETURN nil

METHOD CarregarEventoINI(eArquivoOuIni) CLASS ACBrNFe
    local hResult
    hResult := hb_DynCall({"NFE_CarregarEventoINI", ACBrLIB, DLL_OSAPI}, ::MTHandle, hb_StrToUTF8(eArquivoOuIni))
    ::CheckResult(hResult)
RETURN nil

METHOD LimparLista() CLASS ACBrNFe
    local hResult
    hResult := hb_DynCall({"NFE_LimparLista", ACBrLIB, DLL_OSAPI}, ::MTHandle)
    ::CheckResult(hResult)
RETURN nil

METHOD LimparListaEventos() CLASS ACBrNFe
    local hResult
    hResult := hb_DynCall({"NFE_LimparListaEventos", ACBrLIB, DLL_OSAPI}, ::MTHandle)
    ::CheckResult(hResult)
RETURN nil

METHOD Assinar() CLASS ACBrNFe
    local hResult
    hResult := hb_DynCall({"NFE_Assinar", ACBrLIB, DLL_OSAPI}, ::MTHandle)
    ::CheckResult(hResult)
RETURN nil

METHOD Validar() CLASS ACBrNFe
    local hResult
    hResult := hb_DynCall({"NFE_Validar", ACBrLIB, DLL_OSAPI}, ::MTHandle)
    ::CheckResult(hResult)
RETURN nil

METHOD ValidarRegrasdeNegocios() CLASS ACBrNFe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)  
    hResult := hb_DynCall({"NFE_ValidarRegrasdeNegocios", ACBrLIB, DLL_OSAPI}, ::MTHandle, @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN ::ProcessResult(buffer, bufferLen)

METHOD VerificarAssinatura() CLASS ACBrNFe
    local hResult
    hResult := hb_DynCall({"NFE_VerificarAssinatura", ACBrLIB, DLL_OSAPI}, ::MTHandle)
    ::CheckResult(hResult)
RETURN nil

METHOD GerarChave(ACodigoUF, ACodigoNumerico, AModelo, ASerie, ANumero, ATpEmi, AEmissao, ACNPJCPF) CLASS ACBrNFe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)
    hResult := hb_DynCall({"NFE_GerarChave", ACBrLIB, DLL_OSAPI}, ::MTHandle, ACodigoUF, ACodigoNumerico, AModelo, ASerie, ANumero, ATpEmi, AEmissao, ACNPJCPF, @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN ::ProcessResult(buffer, bufferLen)

METHOD ObterCertificados() CLASS ACBrNFe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)
    hResult := hb_DynCall({"NFE_ObterCertificados", ACBrLIB, DLL_OSAPI}, ::MTHandle, @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN ::ProcessResult(buffer, bufferLen)

METHOD GetPath(tipo)
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)
    hResult := hb_DynCall({"NFE_GetPath", ACBrLIB, DLL_OSAPI}, ::MTHandle, tipo, @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN ::ProcessResult(buffer, bufferLen)

METHOD GetPathEvento(aCodEvento)
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)
    hResult := hb_DynCall({"NFE_GetPathEvento", ACBrLIB, DLL_OSAPI}, ::MTHandle, hb_StrToUTF8(aCodEvento), @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN ::ProcessResult(buffer, bufferLen)

METHOD StatusServico() CLASS ACBrNFe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)
    hResult := hb_DynCall({"NFE_StatusServico", ACBrLIB, DLL_OSAPI}, ::MTHandle, @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN ::ProcessResult(buffer, bufferLen)

METHOD Consultar(eChaveOuNFe, AExtrairEventos) CLASS ACBrNFe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)
    hResult := hb_DynCall({"NFE_Consultar", ACBrLIB, DLL_OSAPI}, ::MTHandle, hb_StrToUTF8(eChaveOuNFe), AExtrairEventos, @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN ::ProcessResult(buffer, bufferLen)

METHOD Inutilizar(ACNPJ, AJustificativa, Ano, Modelo, Serie, NumeroInicial, NumeroFinal) CLASS ACBrNFe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)
    hResult := hb_DynCall({"NFE_Inutilizar", ACBrLIB, DLL_OSAPI}, ::MTHandle, hb_StrToUTF8(ACNPJ), hb_StrToUTF8(AJustificativa), Ano, Modelo, Serie, NumeroInicial, NumeroFinal, @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN ::ProcessResult(buffer, bufferLen)

METHOD Enviar(ALote, Imprimir, Sincrono, Zipado) CLASS ACBrNFe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)
    hResult := hb_DynCall({"NFE_Enviar", ACBrLIB, DLL_OSAPI}, ::MTHandle, ALote, Imprimir, Sincrono, Zipado, @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN ::ProcessResult(buffer, bufferLen)

METHOD ConsultarRecibo(ARecibo) CLASS ACBrNFe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)
    hResult := hb_DynCall({"NFE_ConsultarRecibo", ACBrLIB, DLL_OSAPI}, ::MTHandle, hb_StrToUTF8(ARecibo), @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN ::ProcessResult(buffer, bufferLen)

METHOD Cancelar(eChave, eJustificativa, eCNPJ, ALote) CLASS ACBrNFe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)
    hResult := hb_DynCall({"NFE_Cancelar", ACBrLIB, DLL_OSAPI}, ::MTHandle, hb_StrToUTF8(eChave), hb_StrToUTF8(eJustificativa), hb_StrToUTF8(eCNPJ), ALote, @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN ::ProcessResult(buffer, bufferLen)

METHOD EnviarEvento(ALote) CLASS ACBrNFe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)
    hResult := hb_DynCall({"NFE_EnviarEvento", ACBrLIB, DLL_OSAPI}, ::MTHandle, ALote, @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN ::ProcessResult(buffer, bufferLen)

METHOD DistribuicaoDFePorUltNSU(acUFAutor, eCNPJCPF, eultNSU) CLASS ACBrNFe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)
    hResult := hb_DynCall({"NFE_DistribuicaoDFePorUltNSU", ACBrLIB, DLL_OSAPI}, ::MTHandle, acUFAutor, hb_StrToUTF8(eCNPJCPF), hb_StrToUTF8(eultNSU), @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN ::ProcessResult(buffer, bufferLen)

METHOD DistribuicaoDFePorNSU(acUFAutor, eCNPJCPF, eNSU) CLASS ACBrNFe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)
    hResult := hb_DynCall({"NFE_DistribuicaoDFePorNSU", ACBrLIB, DLL_OSAPI}, ::MTHandle, acUFAutor, hb_StrToUTF8(eCNPJCPF), hb_StrToUTF8(eNSU), @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN ::ProcessResult(buffer, bufferLen)

METHOD DistribuicaoDFePorChave(acUFAutor, eCNPJCPF, echNFe) CLASS ACBrNFe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)
    hResult := hb_DynCall({"NFE_DistribuicaoDFePorChave", ACBrLIB, DLL_OSAPI}, ::MTHandle, acUFAutor, hb_StrToUTF8(eCNPJCPF), hb_StrToUTF8(echNFe), @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN ::ProcessResult(buffer, bufferLen)

METHOD EnviarEmail(ePara, eXMLNFe, aEnviaPDF, eAssunto, eCc, eAnexos, eMensagem) CLASS ACBrNFe
    local hResult
    hResult := hb_DynCall({"NFE_EnviarEmail", ACBrLIB, DLL_OSAPI}, ::MTHandle, hb_StrToUTF8(ePara), hb_StrToUTF8(eXMLNFe), aEnviaPDF, hb_StrToUTF8(eAssunto), hb_StrToUTF8(eCc), hb_StrToUTF8(eAnexos), hb_StrToUTF8(eMensagem))
    ::CheckResult(hResult)
RETURN nil

METHOD EnviarEmailEvento(ePara, eXMLEvento, eXMLNFe, aEnviaPDF, eAssunto, eCc, eAnexos, eMensagem) CLASS ACBrNFe
    local hResult
    hResult := hb_DynCall({"NFE_EnviarEmailEvento", ACBrLIB, DLL_OSAPI}, ::MTHandle, hb_StrToUTF8(ePara), hb_StrToUTF8(eXMLEvento), hb_StrToUTF8(eXMLNFe), aEnviaPDF, hb_StrToUTF8(eAssunto), hb_StrToUTF8(eCc), hb_StrToUTF8(eAnexos), hb_StrToUTF8(eMensagem))
    ::CheckResult(hResult)
RETURN nil

METHOD Imprimir(cImpressora, nNumCopias, cProtocolo, bMostrarPreview, cMarcaDagua, bViaConsumidor, bSimplificado) CLASS ACBrNFe
    local hResult
    hResult := hb_DynCall({"NFE_Imprimir", ACBrLIB, DLL_OSAPI}, ::MTHandle, hb_StrToUTF8(cImpressora), nNumCopias, hb_StrToUTF8(cProtocolo), hb_StrToUTF8(bMostrarPreview), hb_StrToUTF8(cMarcaDagua), hb_StrToUTF8(bViaConsumidor), hb_StrToUTF8(bSimplificado))
    ::CheckResult(hResult)
RETURN nil

METHOD ImprimirPDF() CLASS ACBrNFe
    local hResult
    hResult := hb_DynCall({"NFE_ImprimirPDF", ACBrLIB, DLL_OSAPI}, ::MTHandle)
    ::CheckResult(hResult)
RETURN nil

METHOD ImprimirEvento(eArquivoXmlNFe, eArquivoXmlEvento) CLASS ACBrNFe
    local hResult
    hResult := hb_DynCall({"NFE_ImprimirEvento", ACBrLIB, DLL_OSAPI}, ::MTHandle, hb_StrToUTF8(eArquivoXmlNFe), hb_StrToUTF8(eArquivoXmlEvento))
    ::CheckResult(hResult)
RETURN nil

METHOD ImprimirEventoPDF(eArquivoXmlNFe, eArquivoXmlEvento) CLASS ACBrNFe
    local hResult
    hResult := hb_DynCall({"NFE_ImprimirEventoPDF", ACBrLIB, DLL_OSAPI}, ::MTHandle, hb_StrToUTF8(eArquivoXmlNFe), hb_StrToUTF8(eArquivoXmlEvento))
    ::CheckResult(hResult)
RETURN nil

METHOD ImprimirInutilizacao(eArquivoXml) CLASS ACBrNFe
    local hResult
    hResult := hb_DynCall({"NFE_ImprimirInutilizacao", ACBrLIB, DLL_OSAPI}, ::MTHandle, hb_StrToUTF8(eArquivoXml))
    ::CheckResult(hResult)
RETURN nil

METHOD ImprimirInutilizacaoPDF(eArquivoXml) CLASS ACBrNFe
    local hResult
    hResult := hb_DynCall({"NFE_ImprimirInutilizacaoPDF", ACBrLIB, DLL_OSAPI}, ::MTHandle, hb_StrToUTF8(eArquivoXml))
    ::CheckResult(hResult)
RETURN nil