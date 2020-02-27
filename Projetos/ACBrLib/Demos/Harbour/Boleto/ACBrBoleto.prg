#include '..\Comum\acbrlib.ch'

#ifdef __PLATFORM__WINDOWS
   #define ACBrLIB 'ACBrBoleto32.dll'
#else
   #ifdef __PLATFORM__LINUX
      #define ACBrLIB 'libacbrboleto64.so'
   #else
      #error ACBrLIB-FALTA DEFINICAO: PLATFORM__?
   #endif
#endif

CREATE CLASS ACBrBoleto

   VISIBLE:
   METHOD New(eArqConfig, eChaveCrypt) CONSTRUCTOR
   DESTRUCTOR  Destroy

   METHOD Nome
   METHOD Versao

   METHOD ConfigLer(eArqConfig)
   METHOD ConfigGravar(eArqConfig)
   METHOD ConfigLerValor(eSessao, eChave)
   METHOD ConfigGravarValor(eSessao, eChave, eValor)

   METHOD ConfigurarDados(eArquivoIni)
   METHOD IncluirTitulos(eArquivoIni, eTpSaida)
   METHOD LimparLista()
   METHOD TotalTitulosLista()
   METHOD Imprimir(eNomeImpressora)
   METHOD ImprimirBoleto(eIndice, eNomeImpressora)
   METHOD GerarPDF()
   METHOD GerarHTML()
   METHOD GerarRemessa(eDir, eNumArquivo, eNomeArq)
   METHOD LerRetorno(eDir, eNomeArq)
   METHOD EnviarEmail(ePara, eAssunto, eMensagem, eCC)
   METHOD SetDiretorioArquivo(eDir, eArq)
   METHOD ListaBancos()
   METHOD ListaCaractTitulo()
   METHOD ListaOcorrencias()
   METHOD ListaOcorrenciasEX()
   METHOD TamNossoNumer(eCarteira, enossoNumero, eConvenio)
   METHOD CodigosMoraAceitos()
   METHOD SelecionaBanco(eCodBanco)
   METHOD MontarNossoNumero(eIndice)
   METHOD RetornaLinhaDigitavel(eIndice)
   METHOD RetornaCodigoBarras(eIndice)
    
   HIDDEN:
   VAR hHandle

   METHOD CheckResult(hResult)
   METHOD ProcessResult(buffer, bufferLen)

END CLASS

METHOD New(eArqConfig, eChaveCrypt) CLASS ACBrBoleto
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
   hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_Inicializar", hb_StrToUTF8(eArqConfig), hb_StrToUTF8(eChaveCrypt))
   ::CheckResult(hResult) 
   RETURN Self

PROCEDURE Destroy CLASS ACBrBoleto
    DllCall(::hHandle, DLL_OSAPI, "Boleto_Finalizar")
    DllUnload(::hHandle)
    RETURN

METHOD CheckResult(hResult) CLASS ACBrBoleto
    local buffer, bufferLen, oErr
    if hResult >= 0 
       RETURN nil
    endif    

    bufferLen := STR_LEN
    buffer := Space(bufferLen)

    DllCall(::hHandle, DLL_OSAPI, "Boleto_UltimoRetorno", @buffer, @bufferLen)
    if bufferLen > STR_LEN
        buffer := Space(bufferLen)
        DllCall(::hHandle, DLL_OSAPI, "Boleto_UltimoRetorno", @buffer, @bufferLen)        
    endif    

    oErr := ErrorNew()
    oErr:Severity := ES_ERROR
    oErr:Description := hb_UTF8ToStr(buffer)
    Throw(oErr)
    RETURN nil

METHOD ProcessResult(buffer, bufferLen) CLASS ACBrBoleto
    if bufferLen > STR_LEN
        buffer := Space(bufferLen)
        DllCall(::hHandle, DLL_OSAPI, "Boleto_UltimoRetorno", @buffer, @bufferLen)        
    endif
    RETURN buffer

METHOD Nome CLASS ACBrBoleto
    local hResult, buffer, bufferLen, ret
    bufferLen := STR_LEN
    buffer := Space(bufferLen)
    hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_Nome", @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD Versao CLASS ACBrBoleto
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)
    hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_Versao", @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD ConfigLer(eArqConfig) CLASS ACBrBoleto
    local hResult    
    hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_ConfigLer", hb_StrToUTF8(eArqConfig))
    ::CheckResult(hResult)
    RETURN nil

METHOD ConfigGravar(eArqConfig) CLASS ACBrBoleto
        local hResult    
        hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_ConfigGravar", hb_StrToUTF8(eArqConfig))
        ::CheckResult(hResult)
        RETURN nil

METHOD ConfigLerValor(eSessao, eChave) CLASS ACBrBoleto
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)   
    hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_ConfigLerValor", hb_StrToUTF8(eSessao), hb_StrToUTF8(eChave), @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD ConfigGravarValor(eSessao, eChave, eValor) CLASS ACBrBoleto
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_ConfigGravarValor", hb_StrToUTF8(eSessao), hb_StrToUTF8(eChave), hb_StrToUTF8(eValor))
    ::CheckResult(hResult)
    RETURN nil

METHOD ConfigurarDados(eArquivoIni) CLASS ACBrBoleto
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)   
    hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_ConfigurarDados", hb_StrToUTF8(eArquivoIni), @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD IncluirTitulos(eArquivoIni, eTpSaida) CLASS ACBrBoleto
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)   
    hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_IncluirTitulos", hb_StrToUTF8(eArquivoIni), hb_StrToUTF8(eTpSaida), @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD LimparLista() CLASS ACBrBoleto
    local hResult 
    hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_LimparLista")
    ::CheckResult(hResult)
    RETURN nil

METHOD TotalTitulosLista() CLASS ACBrBoleto
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)   
    hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_TotalTitulosLista", @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD Imprimir(eNomeImpressora) CLASS ACBrBoleto
    local hResult 
    hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_Imprimir", hb_StrToUTF8(eNomeImpressora))
    ::CheckResult(hResult)
    RETURN nil

METHOD ImprimirBoleto(eIndice, eNomeImpressora) CLASS ACBrBoleto
    local hResult 
    hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_ImprimirBoleto", eIndice, hb_StrToUTF8(eNomeImpressora))
    ::CheckResult(hResult)
    RETURN nil

METHOD GerarPDF() CLASS ACBrBoleto
    local hResult 
    hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_GerarPDF")
    ::CheckResult(hResult)
    RETURN nil

METHOD GerarHTML() CLASS ACBrBoleto
    local hResult 
    hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_GerarHTML")
    ::CheckResult(hResult)
    RETURN nil

METHOD GerarRemessa(eDir, eNumArquivo, eNomeArq) CLASS ACBrBoleto
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_GerarRemessa", hb_StrToUTF8(eDir), eNumArquivo, hb_StrToUTF8(eNomeArq))
    ::CheckResult(hResult)
    RETURN nil

METHOD LerRetorno(eDir, eNomeArq) CLASS ACBrBoleto
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_LerRetorno", hb_StrToUTF8(eDir), hb_StrToUTF8(eNomeArq))
    ::CheckResult(hResult)
    RETURN nil

METHOD EnviarEmail(ePara, eAssunto, eMensagem, eCC) CLASS ACBrBoleto
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_EnviarEmail", hb_StrToUTF8(ePara), hb_StrToUTF8(eAssunto), hb_StrToUTF8(eMensagem), hb_StrToUTF8(eCC))
    ::CheckResult(hResult)
    RETURN nil

METHOD SetDiretorioArquivo(eDir, eArq) CLASS ACBrBoleto
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)   
    hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_SetDiretorioArquivo", hb_StrToUTF8(eDir), hb_StrToUTF8(eArq), @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD ListaBancos() CLASS ACBrBoleto
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)   
    hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_ListaBancos", @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)
    
METHOD ListaCaractTitulo() CLASS ACBrBoleto
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)   
    hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_ListaCaractTitulo", @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD ListaOcorrencias() CLASS ACBrBoleto
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)   
    hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_ListaOcorrencias", @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD ListaOcorrenciasEX() CLASS ACBrBoleto
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)   
    hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_ListaOcorrenciasEX", @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD TamNossoNumer(eCarteira, enossoNumero, eConvenio) CLASS ACBrBoleto
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)   
    hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_TamNossoNumero", hb_StrToUTF8(eCarteira), hb_StrToUTF8(enossoNumero), hb_StrToUTF8(eConvenio), @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD CodigosMoraAceitos() CLASS ACBrBoleto
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)   
    hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_CodigosMoraAceitos", @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD SelecionaBanco(eCodBanco) CLASS ACBrBoleto
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)   
    hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_SelecionaBanco", hb_StrToUTF8(eCodBanco), @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)
    
METHOD MontarNossoNumero(eIndice) CLASS ACBrBoleto
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)   
    hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_MontarNossoNumero", eIndice, @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD RetornaLinhaDigitavel(eIndice) CLASS ACBrBoleto
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)   
    hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_RetornaLinhaDigitavel", eIndice, @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD RetornaCodigoBarras(eIndice) CLASS ACBrBoleto
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)   
    hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_RetornaCodigoBarras", eIndice, @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)