#include 'acbrlib.ch'

#define ACBrLIB 'ACBrNFSe32.dll'

CREATE CLASS ACBrNFSe
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

    METHOD CarregarXML(eArquivoOuXml)
    METHOD CarregarINI(eArquivoOuIni)
    METHOD ObterXml(AIndex)
    METHOD GravarXml(AIndex, eNomeArquivo, ePathArquivo)
    METHOD ObterIni(AIndex)
    METHOD GravarIni(AIndex, eNomeArquivo, ePathArquivo)
    METHOD LimparLista()
    METHOD ObterCertificados()
    METHOD Emitir(ALote, aModoEnvio, Imprimir)

    METHOD Cancelar(aInfCancelamentoNFSe)
    METHOD SubstituirNFSe(aNumeroNFSe, aSerieNFSe, aCodigoCancelamento, aMotivoCancelamento, aNumeroLote, aCodigoVerificacao)
    METHOD LinkNFSe(aNumeroNFSe, aCodigoVerificacao, aChaveAcesso, aValorServico)
    METHOD GerarLote(aLote, aQtdMaxima, aModoEnvio)
    METHOD GerarToken()
    METHOD ConsultarSituacao(aProtocolo, aNumLote)
    METHOD ConsultarLoteRps(aProtocolo, aNumLote)
    METHOD ConsultarNFSePorRps(aNumeroRps, aSerie, aTipo, aCodigoVerificacao)
    METHOD ConsultarNFSePorNumero(aNumero, aPagina)
    METHOD ConsultarNFSePorPeriodo(aDataInicial, aDataFinal, aPagina, aNumeroLote, aTipoPeriodo)
    METHOD ConsultarNFSePorFaixa(aNumeroInicial, aNumeroFinal, aPagina)
    METHOD ConsultarNFSeGenerico(aInfConsultaNFSe)
    METHOD ConsultarNFSeServicoPrestadoPorNumero(aNumero, aPagina, aDataInicial, aDataFinal, aTipoPeriodo)
    METHOD ConsultarNFSeServicoPrestadoPorPeriodo(aDataInicial, aDataFinal, aPagina, aTipoPeriodo)
    METHOD ConsultarNFSeServicoPrestadoPorTomador(aCNPJ, aInscMun, aPagina, aDataInicial, aDataFinal, aTipoPeriodo)
    METHOD ConsultarNFSeServicoPrestadoPorIntermediario(aCNPJ, aInscMun, aPagina, aDataInicial, aDataFinal, aTipoPeriodo)
    METHOD ConsultarNFSeServicoTomadoPorNumero(aNumero, aPagina, aDataInicial, aDataFinal, aTipoPeriodo)
    METHOD ConsultarNFSeServicoTomadoPorPrestador(aCNPJ, aInscMun, aPagina, aDataInicial, aDataFinal, aTipoPeriodo)
    METHOD ConsultarNFSeServicoTomadoPorTomador(aCNPJ, aInscMun, aPagina, aDataInicial, aDataFinal, aTipoPeriodo)
    METHOD ConsultarNFSeServicoTomadoPorPeriodo(aDataInicial, aDataFinal, aPagina, aTipoPeriodo)
    METHOD ConsultarNFSeServicoTomadoPorIntermediario(aCNPJ, aInscMun, aPagina, aDataInicial, aDataFinal, aTipoPeriodo)
    METHOD EnviarEmail(ePara, eXMLNFe, aEnviaPDF, eAssunto, eCc, eAnexos, eMensagem)
    METHOD Imprimir(cImpressora, nNumCopias, bGerarPDF, bMostrarPreview, cCancelada)
    METHOD ImprimirPDF()

END CLASS

METHOD New(eArqConfig, eChaveCrypt) CLASS ACBrNFSe
    local hResult, buffer, bufferLen, oErr

    eArqConfig  := IIF(eArqConfig = NIL, '', eArqConfig)
    eChaveCrypt := IIF(eChaveCrypt = NIL, '', eChaveCrypt)

    ::hHandle := DllLoad(ACBrLIB)
    if EMPTY(::hHandle) // Eric.Developer: xHarbour retorna 0x00000000
        oErr             := ErrorNew()
        oErr:Severity    := ES_ERROR
        oErr:Description := "Erro a carregar a dll [" + ACBrLIB + "]"
        Throw(oErr)
    endif
    hResult  := DllCall(::hHandle, DLL_OSAPI, "NFSE_Inicializar", hb_StrToUTF8(eArqConfig), hb_StrToUTF8(eChaveCrypt))
    ::CheckResult(hResult)
RETURN Self

METHOD Destroy CLASS ACBrNFSe
    DllCall(::hHandle, DLL_OSAPI, "NFSE_Finalizar") //
    DllUnload(::hHandle)
RETURN nil

METHOD CheckResult(hResult) CLASS ACBrNFSe
    local buffer, bufferLen, oErr
    if hResult >= 0
        RETURN nil
    endif

    bufferLen := STR_LEN
    buffer    := Space(bufferLen)

    DllCall(::hHandle, DLL_OSAPI, "NFSE_UltimoRetorno", @buffer, @bufferLen)
    if bufferLen > STR_LEN
        buffer := Space(bufferLen)
        DllCall(::hHandle, DLL_OSAPI, "NFSE_UltimoRetorno", @buffer, @bufferLen)
    endif

    oErr             := ErrorNew()
    oErr:Severity    := ES_ERROR
    oErr:Description := hb_UTF8ToStr(buffer)
    Throw(oErr)
RETURN nil

METHOD ProcessResult(buffer, bufferLen) CLASS ACBrNFSe
    if bufferLen > STR_LEN
        buffer  := Space(bufferLen)
        DllCall(::hHandle, DLL_OSAPI, "NFSE_UltimoRetorno", @buffer, @bufferLen)
    endif
RETURN buffer

METHOD Nome CLASS ACBrNFSe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer    := Space(bufferLen)
    hResult   := DllCall(::hHandle, DLL_OSAPI, "NFSE_Nome", @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN ::ProcessResult(buffer, bufferLen)

METHOD Versao CLASS ACBrNFSe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer    := Space(bufferLen)
    hResult   := DllCall(::hHandle, DLL_OSAPI, "NFSE_Versao", @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN ::ProcessResult(buffer, bufferLen)

**********************************************************

METHOD ConfigLer(eArqConfig) CLASS ACBrNFSe
    local hResult
    hResult   := DllCall(::hHandle, DLL_OSAPI, "NFSE_ConfigLer", hb_StrToUTF8(eArqConfig))
    ::CheckResult(hResult)
RETURN nil

METHOD ConfigGravar(eArqConfig) CLASS ACBrNFSe
    local hResult
    hResult   := DllCall(::hHandle, DLL_OSAPI, "NFSE_ConfigGravar", hb_StrToUTF8(eArqConfig))
    ::CheckResult(hResult)
RETURN nil

METHOD ConfigLerValor(eSessao, eChave) CLASS ACBrNFSe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer    := Space(bufferLen)
    hResult   := DllCall(::hHandle, DLL_OSAPI, "NFSE_ConfigLerValor", hb_StrToUTF8(eSessao), hb_StrToUTF8(eChave), @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN ::ProcessResult(buffer, bufferLen)

METHOD ConfigGravarValor(eSessao, eChave, eValor) CLASS ACBrNFSe
    local hResult
    hResult   := DllCall(::hHandle, DLL_OSAPI, "NFSE_ConfigGravarValor", hb_StrToUTF8(eSessao), hb_StrToUTF8(eChave), hb_StrToUTF8(eValor))
    ::CheckResult(hResult)
RETURN nil

METHOD CarregarXML(eArquivoOuXml) CLASS ACBrNFSe
    local hResult
    hResult   := DllCall(::hHandle, DLL_OSAPI, "NFSE_CarregarXML", hb_StrToUTF8(eArquivoOuXml))
    ::CheckResult(hResult)
RETURN nil

METHOD CarregarINI(eArquivoOuIni) CLASS ACBrNFSe
    local hResult
    hResult   := DllCall(::hHandle, DLL_OSAPI, "NFSE_CarregarINI", hb_StrToUTF8(eArquivoOuIni))
    ::CheckResult(hResult)
RETURN nil

METHOD ObterXml(AIndex) CLASS ACBrNFSe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer    := Space(bufferLen)
    hResult   := DllCall(::hHandle, DLL_OSAPI, "NFSE_ObterXml", AIndex, @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN ::ProcessResult(buffer, bufferLen)

METHOD GravarXml(AIndex, eNomeArquivo, ePathArquivo) CLASS ACBrNFSe
    local hResult
    hResult   := DllCall(::hHandle, DLL_OSAPI, "NFSE_GravarXml", AIndex, hb_StrToUTF8(eNomeArquivo), hb_StrToUTF8(ePathArquivo))
    ::CheckResult(hResult)
RETURN nil

METHOD ObterIni(AIndex) CLASS ACBrNFSe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer    := Space(bufferLen)
    hResult   := DllCall(::hHandle, DLL_OSAPI, "NFSE_ObterIni", AIndex, @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN ::ProcessResult(buffer, bufferLen)

METHOD GravarIni(AIndex, eNomeArquivo, ePathArquivo) CLASS ACBrNFSe
    local hResult
    hResult   := DllCall(::hHandle, DLL_OSAPI, "NFSE_GravarIni", AIndex, hb_StrToUTF8(eNomeArquivo), hb_StrToUTF8(ePathArquivo))
    ::CheckResult(hResult)
RETURN nil

METHOD LimparLista() CLASS ACBrNFSe
    local hResult
    hResult   := DllCall(::hHandle, DLL_OSAPI, "NFSE_LimparLista")
    ::CheckResult(hResult)
RETURN nil

METHOD ObterCertificados() CLASS ACBrNFSe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer    := Space(bufferLen)
    hResult   := DllCall(::hHandle, DLL_OSAPI, "NFSE_ObterCertificados", @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN ::ProcessResult(buffer, bufferLen)

METHOD Emitir(ALote, aModoEnvio, Imprimir) CLASS ACBrNFSe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer    := Space(bufferLen)
    hResult   := DllCall(::hHandle, DLL_OSAPI, "NFSE_Emitir", ALote, aModoEnvio, Imprimir, @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN ::ProcessResult(buffer, bufferLen)

METHOD Cancelar(aInfCancelamentoNFSe) CLASS ACBrNFSe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer    := Space(bufferLen)
    hResult   := DllCall(::hHandle, DLL_OSAPI, "NFSE_Cancelar", hb_StrToUTF8(aInfCancelamentoNFSe), @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN ::ProcessResult(buffer, bufferLen)

METHOD SubstituirNFSe(aNumeroNFSe, aSerieNFSe, aCodigoCancelamento, aMotivoCancelamento, aNumeroLote, aCodigoVerificacao) CLASS ACBrNFSe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer    := Space(bufferLen)
    hResult   := DllCall(::hHandle, DLL_OSAPI, "NFSE_SubstituirNFSe", aNumeroNFSe, aSerieNFSe, aCodigoCancelamento, hb_StrToUTF8(aMotivoCancelamento), aNumeroLote, aCodigoVerificacao, @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN ::ProcessResult(buffer, bufferLen)

METHOD LinkNFSe(aNumeroNFSe, aCodigoVerificacao, aChaveAcesso, aValorServico) CLASS ACBrNFSe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer    := Space(bufferLen)
    hResult   := DllCall(::hHandle, DLL_OSAPI, "NFSE_LinkNFSe", aNumeroNFSe, aCodigoVerificacao, hb_StrToUTF8(aChaveAcesso), aValorServico, @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN ::ProcessResult(buffer, bufferLen)

METHOD GerarLote(aLote, aQtdMaxima, aModoEnvio) CLASS ACBrNFSe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer    := Space(bufferLen)
    hResult   := DllCall(::hHandle, DLL_OSAPI, "NFSE_GerarLote", aLote, aQtdMaxima, aModoEnvio, @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN ::ProcessResult(buffer, bufferLen)

METHOD GerarToken() CLASS ACBrNFSe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer    := Space(bufferLen)
    hResult   := DllCall(::hHandle, DLL_OSAPI, "NFSE_GerarToken", @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN ::ProcessResult(buffer, bufferLen)

METHOD ConsultarSituacao(aProtocolo, aNumLote) CLASS ACBrNFSe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer    := Space(bufferLen)
    hResult   := DllCall(::hHandle, DLL_OSAPI, "NFSE_ConsultarSituacao", hb_StrToUTF8(aProtocolo), aNumLote, @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN ::ProcessResult(buffer, bufferLen)

METHOD ConsultarLoteRps(aProtocolo, aNumLote) CLASS ACBrNFSe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer    := Space(bufferLen)
    hResult   := DllCall(::hHandle, DLL_OSAPI, "NFSE_ConsultarLoteRps", hb_StrToUTF8(aProtocolo), aNumLote, @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN ::ProcessResult(buffer, bufferLen)

METHOD ConsultarNFSePorRps(aNumeroRps, aSerie, aTipo, aCodigoVerificacao) CLASS ACBrNFSe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer    := Space(bufferLen)
    hResult   := DllCall(::hHandle, DLL_OSAPI, "NFSE_ConsultarNFSePorRps", aNumeroRps, aSerie, aTipo, aCodigoVerificacao, @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN ::ProcessResult(buffer, bufferLen)

METHOD ConsultarNFSePorNumero(aNumero, aPagina) CLASS ACBrNFSe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer    := Space(bufferLen)
    hResult   := DllCall(::hHandle, DLL_OSAPI, "NFSE_DConsultarNFSePorNumero", aNumero, aPagina, @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN ::ProcessResult(buffer, bufferLen)

METHOD ConsultarNFSePorPeriodo(aDataInicial, aDataFinal, aPagina, aNumeroLote, aTipoPeriodo) CLASS ACBrNFSe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer    := Space(bufferLen)
    hResult   := DllCall(::hHandle, DLL_OSAPI, "NFSE_ConsultarNFSePorPeriodo", aDataInicial, aDataFinal, aPagina, aNumeroLote, aTipoPeriodo, @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN ::ProcessResult(buffer, bufferLen)

METHOD ConsultarNFSePorFaixa(aNumeroInicial, aNumeroFinal, aPagina) CLASS ACBrNFSe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer    := Space(bufferLen)
    hResult   := DllCall(::hHandle, DLL_OSAPI, "NFSE_ConsultarNFSePorFaixa", aNumeroInicial, aNumeroFinal, aPagina, @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN ::ProcessResult(buffer, bufferLen)

METHOD ConsultarNFSeGenerico(aInfConsultaNFSe) CLASS ACBrNFSe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer    := Space(bufferLen)
    hResult   := DllCall(::hHandle, DLL_OSAPI, "NFSE_ConsultarNFSeGenerico", hb_StrToUTF8(aInfConsultaNFSe), @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN ::ProcessResult(buffer, bufferLen)

METHOD ConsultarNFSeServicoPrestadoPorNumero(aNumero, aPagina, aDataInicial, aDataFinal, aTipoPeriodo) CLASS ACBrNFSe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer    := Space(bufferLen)
    hResult   := DllCall(::hHandle, DLL_OSAPI, "NFSE_ConsultarNFSeServicoPrestadoPorNumero", aNumero, aPagina, aDataInicial, aDataFinal, aTipoPeriodo, @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN ::ProcessResult(buffer, bufferLen)

METHOD ConsultarNFSeServicoPrestadoPorPeriodo(aDataInicial, aDataFinal, aPagina, aTipoPeriodo) CLASS ACBrNFSe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer    := Space(bufferLen)
    hResult   := DllCall(::hHandle, DLL_OSAPI, "NFSE_ConsultarNFSeServicoPrestadoPorPeriodo", aDataInicial, aDataFinal, aPagina, aTipoPeriodo, @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN ::ProcessResult(buffer, bufferLen)

METHOD ConsultarNFSeServicoPrestadoPorTomador(aCNPJ, aInscMun, aPagina, aDataInicial, aDataFinal, aTipoPeriodo) CLASS ACBrNFSe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer    := Space(bufferLen)
    hResult   := DllCall(::hHandle, DLL_OSAPI, "NFSE_ConsultarNFSeServicoPrestadoPorTomador", hb_StrToUTF8(aCNPJ), aInscMun, aPagina, aDataInicial, aDataFinal, aTipoPeriodo, @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN ::ProcessResult(buffer, bufferLen)

METHOD ConsultarNFSeServicoPrestadoPorIntermediario(aCNPJ, aInscMun, aPagina, aDataInicial, aDataFinal, aTipoPeriodo) CLASS ACBrNFSe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer    := Space(bufferLen)
    hResult   := DllCall(::hHandle, DLL_OSAPI, "NFSE_ConsultarNFSeServicoPrestadoPorIntermediario", hb_StrToUTF8(aCNPJ), aInscMun, aPagina, aDataInicial, aDataFinal, aTipoPeriodo, @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN ::ProcessResult(buffer, bufferLen)

METHOD ConsultarNFSeServicoTomadoPorNumero(aNumero, aPagina, aDataInicial, aDataFinal, aTipoPeriodo) CLASS ACBrNFSe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer    := Space(bufferLen)
    hResult   := DllCall(::hHandle, DLL_OSAPI, "NFSE_ConsultarNFSeServicoTomadoPorNumero", aNumero, aPagina, aDataInicial, aDataFinal, aTipoPeriodo, @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN ::ProcessResult(buffer, bufferLen)

METHOD ConsultarNFSeServicoTomadoPorPrestador(aCNPJ, aInscMun, aPagina, aDataInicial, aDataFinal, aTipoPeriodo) CLASS ACBrNFSe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer    := Space(bufferLen)
    hResult   := DllCall(::hHandle, DLL_OSAPI, "NFSE_ConsultarNFSeServicoTomadoPorPrestador", hb_StrToUTF8(aCNPJ), aInscMun, aPagina, aDataInicial, aDataFinal, aTipoPeriodo, @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN ::ProcessResult(buffer, bufferLen)

METHOD ConsultarNFSeServicoTomadoPorTomador(aCNPJ, aInscMun, aPagina, aDataInicial, aDataFinal, aTipoPeriodo) CLASS ACBrNFSe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer    := Space(bufferLen)
    hResult   := DllCall(::hHandle, DLL_OSAPI, "NFSE_ConsultarNFSeServicoTomadoPorTomador", hb_StrToUTF8(aCNPJ), aInscMun, aPagina, aDataInicial, aDataFinal, aTipoPeriodo, @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN ::ProcessResult(buffer, bufferLen)

METHOD ConsultarNFSeServicoTomadoPorPeriodo(aDataInicial, aDataFinal, aPagina, aTipoPeriodo) CLASS ACBrNFSe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer    := Space(bufferLen)
    hResult   := DllCall(::hHandle, DLL_OSAPI, "NFSE_ConsultarNFSeServicoTomadoPorPeriodo", aDataInicial, aDataFinal, aPagina, aTipoPeriodo, @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN ::ProcessResult(buffer, bufferLen)

METHOD ConsultarNFSeServicoTomadoPorIntermediario(aCNPJ, aInscMun, aPagina, aDataInicial, aDataFinal, aTipoPeriodo) CLASS ACBrNFSe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer    := Space(bufferLen)
    hResult   := DllCall(::hHandle, DLL_OSAPI, "NFSE_ConsultarNFSeServicoTomadoPorIntermediario", hb_StrToUTF8(aCNPJ), aInscMun, aPagina, aDataInicial, aDataFinal, aTipoPeriodo, @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN ::ProcessResult(buffer, bufferLen)

METHOD EnviarEmail(ePara, eXMLNFe, aEnviaPDF, eAssunto, eCc, eAnexos, eMensagem) CLASS ACBrNFSe
    local hResult
    hResult  := DllCall(::hHandle, DLL_OSAPI, "NFSE_EnviarEmail", hb_StrToUTF8(ePara), hb_StrToUTF8(eXMLNFe), aEnviaPDF, hb_StrToUTF8(eAssunto), hb_StrToUTF8(eCc), hb_StrToUTF8(eAnexos), hb_StrToUTF8(eMensagem))
    ::CheckResult(hResult)
RETURN nil

METHOD Imprimir(cImpressora, nNumCopias, bGerarPDF, bMostrarPreview, cCancelada) CLASS ACBrNFSe
    local hResult
    hResult  := DllCall(::hHandle, DLL_OSAPI, "NFSE_Imprimir", hb_StrToUTF8(cImpressora), nNumCopias, hb_StrToUTF8(bGerarPDF), hb_StrToUTF8(bMostrarPreview), hb_StrToUTF8(cCancelada), hb_StrToUTF8(bViaConsumidor), hb_StrToUTF8(bSimplificado))
    ::CheckResult(hResult)
RETURN nil

METHOD ImprimirPDF() CLASS ACBrNFSe
    local hResult
    hResult  := DllCall(::hHandle, DLL_OSAPI, "NFSE_ImprimirPDF")
    ::CheckResult(hResult)
RETURN nil

METHOD StatusServico() CLASS ACBrNFSe
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer    := Space(bufferLen)
    hResult   := DllCall(::hHandle, DLL_OSAPI, "NFSE_StatusServico", @buffer, @bufferLen)
    ::CheckResult(hResult)
RETURN ::ProcessResult(buffer, bufferLen)


