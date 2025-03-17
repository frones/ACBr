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

   METHOD Nome()
   METHOD Versao()

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
   METHOD ObterRetorno(eDir, eNomeArq)
   METHOD EnviarEmail(ePara, eAssunto, eMensagem, eCC)
   METHOD SetDiretorioArquivo(eDir, eArq)
   METHOD ListaBancos()
   METHOD ListaCaractTitulo()
   METHOD ListaOcorrencias()
   METHOD ListaOcorrenciasEX()
   METHOD TamNossoNumero(eCarteira, enossoNumero, eConvenio)
   METHOD CodigosMoraAceitos()
   METHOD SelecionaBanco(eCodBanco)
   METHOD MontarNossoNumero(eIndice)
   METHOD RetornaLinhaDigitavel(eIndice)
   METHOD RetornaCodigoBarras(eIndice)
   METHOD GerarPDFBoleto(eIndice)
   METHOD SalvarPDFBoleto(eIndice)         //Base64
   METHOD SalvarPDF() //Base64
   METHOD GerarRemessaStream(eDir, eNumArquivo, eNomeArq) //Base64
   METHOD LerRetornoStream(eDir, eNomeArq) //Base64
   METHOD ConsultarTitulosPorPeriodo(eArquivoIni)
   METHOD EnviarBoleto(eCodigoOperacao)
   METHOD EnviarEmailBoleto(eIndice, ePara, eAssunto, eMensagem, eCC)

   HIDDEN:
   VAR hHandle

   METHOD CheckResult(hResult)
   METHOD ProcessResult(buffer, bufferLen)

END CLASS

METHOD New(eArqConfig, eChaveCrypt) CLASS ACBrBoleto
   Local hResult, oErr

   eArqConfig :=if(eArqConfig = Nil, '', eArqConfig)
   eChaveCrypt:=if(eChaveCrypt = Nil, '', eChaveCrypt)

   ::hHandle := DllLoad(ACBrLIB)
   if EMPTY(::hHandle) // Eric.Developer: xHarbour retorna 0x00000000
      oErr := ErrorNew()
      oErr:Severity := ES_ERROR
      oErr:Description := "Erro a carregar a dll [" + ACBrLIB + "]"
      Throw(oErr)
   endif
   hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_Inicializar", HB_StrToUTF8(eArqConfig), HB_StrToUTF8(eChaveCrypt))
   ::CheckResult(hResult)
Return Self

PROCEDURE Destroy CLASS ACBrBoleto
   DllCall(::hHandle, DLL_OSAPI, "Boleto_Finalizar")
   DllUnload(::hHandle)
Return

METHOD CheckResult(hResult) CLASS ACBrBoleto
   Local buffer, bufferLen, oErr
   if hResult >= 0
    Return Nil
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
Return Nil

METHOD ProcessResult(buffer, bufferLen) CLASS ACBrBoleto
   if bufferLen > STR_LEN
     buffer := Space(bufferLen)
     DllCall(::hHandle, DLL_OSAPI, "Boleto_UltimoRetorno", @buffer, @bufferLen)
   endif
Return buffer

METHOD Nome() CLASS ACBrBoleto
   Local hResult, buffer, bufferLen
   bufferLen := STR_LEN
   buffer := Space(bufferLen)
   hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_Nome", @buffer, @bufferLen)
   ::CheckResult(hResult)
Return ::ProcessResult(buffer, bufferLen)

METHOD Versao() CLASS ACBrBoleto
   Local hResult, buffer, bufferLen
   bufferLen := STR_LEN
   buffer := Space(bufferLen)
   hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_Versao", @buffer, @bufferLen)
   ::CheckResult(hResult)
Return ::ProcessResult(buffer, bufferLen)

METHOD ConfigLer(eArqConfig) CLASS ACBrBoleto
   Local hResult
   hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_ConfigLer", HB_StrToUTF8(eArqConfig))
   ::CheckResult(hResult)
Return Nil

METHOD ConfigGravar(eArqConfig) CLASS ACBrBoleto
   Local hResult
   hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_ConfigGravar", HB_StrToUTF8(eArqConfig))
   ::CheckResult(hResult)
Return Nil

METHOD ConfigLerValor(eSessao, eChave) CLASS ACBrBoleto
   Local hResult, buffer, bufferLen
   bufferLen := STR_LEN
   buffer := Space(bufferLen)
   hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_ConfigLerValor", HB_StrToUTF8(eSessao), HB_StrToUTF8(eChave), @buffer, @bufferLen)
   ::CheckResult(hResult)
Return ::ProcessResult(buffer, bufferLen)

METHOD ConfigGravarValor(eSessao, eChave, eValor) CLASS ACBrBoleto
   Local hResult
   hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_ConfigGravarValor", HB_StrToUTF8(eSessao), HB_StrToUTF8(eChave), HB_StrToUTF8(eValor))
   ::CheckResult(hResult)
Return Nil

METHOD ConfigurarDados(eArquivoIni) CLASS ACBrBoleto
   Local hResult
   hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_ConfigurarDados", hb_StrToUTF8(eArquivoIni))
   ::CheckResult(hResult)
Return Nil

METHOD IncluirTitulos(eArquivoIni, eTpSaida) CLASS ACBrBoleto
   Local hResult
   hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_IncluirTitulos", HB_StrToUTF8(eArquivoIni), HB_StrToUTF8(eTpSaida))
   ::CheckResult(hResult)
Return Nil

METHOD LimparLista() CLASS ACBrBoleto
   Local hResult
   hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_LimparLista")
   ::CheckResult(hResult)
Return Nil

METHOD TotalTitulosLista() CLASS ACBrBoleto
   Local hResult
   hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_TotalTitulosLista")
   ::CheckResult(hResult)
Return hResult

METHOD Imprimir(eNomeImpressora) CLASS ACBrBoleto
   Local hResult
   hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_Imprimir", HB_StrToUTF8(eNomeImpressora))
   ::CheckResult(hResult)
Return Nil

METHOD ImprimirBoleto(eIndice, eNomeImpressora) CLASS ACBrBoleto
   Local hResult
   hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_ImprimirBoleto", eIndice, HB_StrToUTF8(eNomeImpressora))
   ::CheckResult(hResult)
Return Nil

METHOD GerarPDF() CLASS ACBrBoleto
   Local hResult
   hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_GerarPDF")
   ::CheckResult(hResult)
Return Nil

METHOD GerarPDFBoleto(eIndice) CLASS ACBrBoleto
   Local hResult
   hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_GerarPDFBoleto",eIndice)
   ::CheckResult(hResult)
Return Nil

METHOD SalvarPDFBoleto(eIndice) CLASS ACBrBoleto  //Base64
   Local hResult
   hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_SalvarPDFBoleto",eIndice)
   ::CheckResult(hResult)
Return Nil

METHOD SalvarPDF() CLASS ACBrBoleto  //Base64
   Local hResult, buffer, bufferLen
   bufferLen := STR_LEN
   buffer := Space(bufferLen)
   hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_SalvarPDF", @buffer, @bufferLen)
   ::CheckResult(hResult)
Return ::ProcessResult(buffer, bufferLen)

METHOD GerarHTML() CLASS ACBrBoleto
   Local hResult
   hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_GerarHTML")
   ::CheckResult(hResult)
Return Nil

METHOD GerarRemessa(eDir, eNumArquivo, eNomeArq) CLASS ACBrBoleto
   Local hResult
   hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_GerarRemessa", HB_StrToUTF8(eDir), eNumArquivo, HB_StrToUTF8(eNomeArq))
   ::CheckResult(hResult)
Return Nil

METHOD GerarRemessaStream(eDir, eNumArquivo, eNomeArq) CLASS ACBrBoleto   //Base64
   Local hResult
   hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_GerarRemessaStream", HB_StrToUTF8(eDir), eNumArquivo, HB_StrToUTF8(eNomeArq))
   ::CheckResult(hResult)
Return Nil

METHOD LerRetorno(eDir, eNomeArq) CLASS ACBrBoleto
   Local hResult
   hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_LerRetorno", HB_StrToUTF8(eDir), HB_StrToUTF8(eNomeArq))
   ::CheckResult(hResult)
Return Nil

METHOD LerRetornoStream(eDir, eNomeArq) CLASS ACBrBoleto
   Local hResult
   hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_LerRetornoStream", HB_StrToUTF8(eDir), HB_StrToUTF8(eNomeArq))
   ::CheckResult(hResult)
Return Nil
	
METHOD ObterRetorno(eDir, eNomeArq) CLASS ACBrBoleto
   Local hResult, buffer, bufferLen
   bufferLen := STR_LEN
   buffer := Space(bufferLen)
   hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_ObterRetorno", hb_StrToUTF8(eDir), hb_StrToUTF8(eNomeArq), @buffer, @bufferLen)
   ::CheckResult(hResult)
Return ::ProcessResult(buffer, bufferLen)

METHOD EnviarEmail(ePara, eAssunto, eMensagem, eCC) CLASS ACBrBoleto
   Local hResult
   hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_EnviarEmail", hb_StrToUTF8(ePara), hb_StrToUTF8(eAssunto), hb_StrToUTF8(eMensagem), hb_StrToUTF8(eCC))
   ::CheckResult(hResult)
Return Nil

METHOD EnviarEmailBoleto(eIndice, ePara, eAssunto, eMensagem, eCC) CLASS ACBrBoleto
   Local hResult
   hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_EnviarEmailBoleto", eIndice, hb_StrToUTF8(ePara), hb_StrToUTF8(eAssunto), hb_StrToUTF8(eMensagem), hb_StrToUTF8(eCC))
   ::CheckResult(hResult)
Return Nil

METHOD SetDiretorioArquivo(eDir, eArq) CLASS ACBrBoleto
   Local hResult
   hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_SetDiretorioArquivo", hb_StrToUTF8(eDir), hb_StrToUTF8(eArq))
   ::CheckResult(hResult)
Return Nil

METHOD ListaBancos() CLASS ACBrBoleto
   Local hResult, buffer, bufferLen
   bufferLen := STR_LEN
   buffer := Space(bufferLen)
   hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_ListaBancos", @buffer, @bufferLen)
   ::CheckResult(hResult)
Return ::ProcessResult(buffer, bufferLen)

METHOD ListaCaractTitulo() CLASS ACBrBoleto
   Local hResult, buffer, bufferLen
   bufferLen := STR_LEN
   buffer := Space(bufferLen)
   hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_ListaCaractTitulo", @buffer, @bufferLen)
   ::CheckResult(hResult)
Return ::ProcessResult(buffer, bufferLen)

METHOD ListaOcorrencias() CLASS ACBrBoleto
   Local hResult, buffer, bufferLen
   bufferLen := STR_LEN
   buffer := Space(bufferLen)
   hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_ListaOcorrencias", @buffer, @bufferLen)
   ::CheckResult(hResult)
Return ::ProcessResult(buffer, bufferLen)

METHOD ListaOcorrenciasEX() CLASS ACBrBoleto
   Local hResult, buffer, bufferLen
   bufferLen := STR_LEN
   buffer := Space(bufferLen)
   hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_ListaOcorrenciasEX", @buffer, @bufferLen)
   ::CheckResult(hResult)
Return ::ProcessResult(buffer, bufferLen)

METHOD TamNossoNumero(eCarteira, enossoNumero, eConvenio) CLASS ACBrBoleto
   Local hResult
   hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_TamNossoNumero", HB_StrToUTF8(eCarteira), HB_StrToUTF8(enossoNumero), HB_StrToUTF8(eConvenio))
   ::CheckResult(hResult)
Return hResult

METHOD CodigosMoraAceitos() CLASS ACBrBoleto
   Local hResult, buffer, bufferLen
   bufferLen := STR_LEN
   buffer := Space(bufferLen)
   hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_CodigosMoraAceitos", @buffer, @bufferLen)
   ::CheckResult(hResult)
Return ::ProcessResult(buffer, bufferLen)

METHOD SelecionaBanco(eCodBanco) CLASS ACBrBoleto
   Local hResult
   hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_SelecionaBanco", HB_StrToUTF8(eCodBanco))
   ::CheckResult(hResult)
Return Nil

METHOD MontarNossoNumero(eIndice) CLASS ACBrBoleto
   Local hResult, buffer, bufferLen
   bufferLen := STR_LEN
   buffer := Space(bufferLen)
   hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_MontarNossoNumero", eIndice, @buffer, @bufferLen)
   ::CheckResult(hResult)
Return ::ProcessResult(buffer, bufferLen)

METHOD RetornaLinhaDigitavel(eIndice) CLASS ACBrBoleto
   Local hResult, buffer, bufferLen
   bufferLen := STR_LEN
   buffer := Space(bufferLen)
   hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_RetornaLinhaDigitavel", eIndice, @buffer, @bufferLen)
   ::CheckResult(hResult)
Return ::ProcessResult(buffer, bufferLen)

METHOD RetornaCodigoBarras(eIndice) CLASS ACBrBoleto
   Local hResult, buffer, bufferLen
   bufferLen := STR_LEN
   buffer := Space(bufferLen)
   hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_RetornaCodigoBarras", eIndice, @buffer, @bufferLen)
   ::CheckResult(hResult)
Return ::ProcessResult(buffer, bufferLen)

METHOD ConsultarTitulosPorPeriodo(eArquivoIni) CLASS ACBrBoleto
   Local hResult, buffer, bufferLen
   bufferLen := STR_LEN
   buffer := Space(bufferLen)
   hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_ConsultarTitulosPorPeriodo", hb_StrToUTF8(eArquivoIni), @buffer, @bufferLen)
   ::CheckResult(hResult)
Return ::ProcessResult(buffer, bufferLen)

METHOD EnviarBoleto(eCodigoOperacao) CLASS ACBrBoleto
   Local hResult, buffer, bufferLen
   bufferLen := STR_LEN
   buffer := Space(bufferLen)
   hResult := DllCall(::hHandle, DLL_OSAPI, "Boleto_EnviarBoleto", eCodigoOperacao, @buffer, @bufferLen)
   ::CheckResult(hResult)
Return ::ProcessResult(buffer, bufferLen)