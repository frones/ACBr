#include '..\Comum\acbrlib.ch'

#ifdef __PLATFORM__WINDOWS
   #define ACBrLIB 'ACBrETQ32.dll'
#else
   #ifdef __PLATFORM__LINUX
      #define ACBrLIB 'libacbretq64.so'
   #else
      #error ACBrLIB-FALTA DEFINICAO: PLATFORM__?
   #endif
#endif

CREATE CLASS ACBrETQ
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
    METHOD IniciarEtiqueta()
    METHOD FinalizarEtiqueta(ACopias, AAvancoEtq)
    METHOD CarregarImagem(eArquivoImagem, eNomeImagem, Flipped)
    
    METHOD Imprimir(ACopias, AAvancoEtq)
    METHOD ImprimirTexto(Orientacao, Fonte, MultiplicadorH, MultiplicadorV, Vertical, Horizontal, eTexto, SubFonte, ImprimirReverso)
    METHOD ImprimirTextoStr(Orientacao, Fonte, MultiplicadorH, MultiplicadorV, Vertical, Horizontal, eTexto, SubFonte, ImprimirReverso)
    METHOD ImprimirBarras(Orientacao, TipoBarras, LarguraBarraLarga, LarguraBarraFina, Vertical, Horizontal, eTexto, AlturaCodBarras, ExibeCodigo)
    METHOD ImprimirLinha(Vertical, Horizontal, Largura, Altura)
    METHOD ImprimirCaixa(Vertical, Horizontal, Largura, Altura, EspessuraVertical, EspessuraHorizontal)
    METHOD ImprimirImagem(MultiplicadorImagem, Vertical, Horizontal, eNomeImagem)
    METHOD ImprimirQRCode(Vertical, Horizontal, Texto, LarguraModulo, ErrorLevel, Tipo)

END CLASS

METHOD New(eArqConfig, eChaveCrypt) CLASS ACBrETQ
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
    hResult := DllCall(::hHandle, DLL_OSAPI, "ETQ_Inicializar", hb_StrToUTF8(eArqConfig), hb_StrToUTF8(eChaveCrypt))
    ::CheckResult(hResult) 
    RETURN Self

PROCEDURE Destroy CLASS ACBrETQ
    DllCall(::hHandle, DLL_OSAPI, "ETQ_Finalizar")
    DllUnload(::hHandle)
    RETURN

METHOD CheckResult(hResult) CLASS ACBrETQ
    local buffer, bufferLen, oErr
    if hResult >= 0 
       RETURN nil
    endif    

    bufferLen := STR_LEN
    buffer := Space(bufferLen)

    DllCall(::hHandle, DLL_OSAPI, "ETQ_UltimoRetorno", @buffer, @bufferLen)
    if bufferLen > STR_LEN
        buffer := Space(bufferLen)
        DllCall(::hHandle, DLL_OSAPI, "ETQ_UltimoRetorno", @buffer, @bufferLen)        
    endif    

    oErr := ErrorNew()
    oErr:Severity := ES_ERROR
    oErr:Description := hb_UTF8ToStr(buffer)
    Throw(oErr)
    RETURN nil

METHOD ProcessResult(buffer, bufferLen) CLASS ACBrETQ
    if bufferLen > STR_LEN
        buffer := Space(bufferLen)
        DllCall(::hHandle, DLL_OSAPI, "ETQ_UltimoRetorno", @buffer, @bufferLen)        
    endif
    RETURN buffer

METHOD Nome CLASS ACBrETQ
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)
    hResult := DllCall(::hHandle, DLL_OSAPI, "ETQ_Nome", @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD Versao CLASS ACBrETQ
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)
    hResult := DllCall(::hHandle, DLL_OSAPI, "ETQ_Versao", @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD ConfigLer(eArqConfig) CLASS ACBrETQ
    local hResult    
    hResult := DllCall(::hHandle, DLL_OSAPI, "ETQ_ConfigLer", hb_StrToUTF8(eArqConfig))
    ::CheckResult(hResult)
    RETURN nil

METHOD ConfigGravar(eArqConfig) CLASS ACBrETQ
        local hResult    
        hResult := DllCall(::hHandle, DLL_OSAPI, "ETQ_ConfigGravar", hb_StrToUTF8(eArqConfig))
        ::CheckResult(hResult)
        RETURN nil

METHOD ConfigLerValor(eSessao, eChave) CLASS ACBrETQ
    local hResult, buffer, bufferLen
    bufferLen := STR_LEN
    buffer := Space(bufferLen)   
    hResult := DllCall(::hHandle, DLL_OSAPI, "ETQ_ConfigLerValor", hb_StrToUTF8(eSessao), hb_StrToUTF8(eChave), @buffer, @bufferLen)
    ::CheckResult(hResult)
    RETURN ::ProcessResult(buffer, bufferLen)

METHOD ConfigGravarValor(eSessao, eChave, eValor) CLASS ACBrETQ
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "ETQ_ConfigGravarValor", hb_StrToUTF8(eSessao), hb_StrToUTF8(eChave), hb_StrToUTF8(eValor))
    ::CheckResult(hResult)
    RETURN nil

METHOD Ativar() CLASS ACBrETQ
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "ETQ_Ativar")
    ::CheckResult(hResult)
    RETURN nil

METHOD Desativar() CLASS ACBrETQ
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "ETQ_Desativar")
    ::CheckResult(hResult)
    RETURN nil

METHOD IniciarEtiqueta() CLASS ACBrETQ
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "ETQ_IniciarEtiqueta")
    ::CheckResult(hResult)
    RETURN nil

METHOD FinalizarEtiqueta(ACopias, AAvancoEtq) CLASS ACBrETQ
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "ETQ_FinalizarEtiqueta", ACopias, AAvancoEtq)
    ::CheckResult(hResult)
    RETURN nil

METHOD CarregarImagem(eArquivoImagem, eNomeImagem, Flipped) CLASS ACBrETQ
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "ETQ_CarregarImagem", hb_StrToUTF8(eArquivoImagem), hb_StrToUTF8(eNomeImagem), Flipped)
    ::CheckResult(hResult)
    RETURN nil

METHOD Imprimir(ACopias, AAvancoEtq) CLASS ACBrETQ
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "ETQ_Imprimir", ACopias, AAvancoEtq)
    ::CheckResult(hResult)
    RETURN nil

METHOD ImprimirTexto(Orientacao, Fonte, MultiplicadorH, MultiplicadorV, Vertical, Horizontal, eTexto, SubFonte, ImprimirReverso) CLASS ACBrETQ
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "ETQ_ImprimirTexto", Orientacao, Fonte, MultiplicadorH, MultiplicadorV, Vertical, Horizontal, hb_StrToUTF8(eTexto), SubFonte, ImprimirReverso)
    ::CheckResult(hResult)
    RETURN nil

METHOD ImprimirTextoStr(Orientacao, Fonte, MultiplicadorH, MultiplicadorV, Vertical, Horizontal, eTexto, SubFonte, ImprimirReverso) CLASS ACBrETQ
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "ETQ_ImprimirTextoStr", Orientacao, hb_StrToUTF8(Fonte), MultiplicadorH, MultiplicadorV, Vertical, Horizontal, hb_StrToUTF8(eTexto), SubFonte, ImprimirReverso)
    ::CheckResult(hResult)
    RETURN nil

METHOD ImprimirBarras(Orientacao, TipoBarras, LarguraBarraLarga, LarguraBarraFina, Vertical, Horizontal, eTexto, AlturaCodBarras, ExibeCodigo) CLASS ACBrETQ
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "ETQ_ImprimirBarras", Orientacao, TipoBarras, LarguraBarraLarga, LarguraBarraFina, Vertical, Horizontal, hb_StrToUTF8(eTexto), AlturaCodBarras, ExibeCodigo)
    ::CheckResult(hResult)
    RETURN nil

METHOD ImprimirLinha(Vertical, Horizontal, Largura, Altura) CLASS ACBrETQ
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "ETQ_ImprimirLinha", Vertical, Horizontal, Largura, Altura)
    ::CheckResult(hResult)
    RETURN nil
    
METHOD ImprimirCaixa(Vertical, Horizontal, Largura, Altura, EspessuraVertical, EspessuraHorizontal) CLASS ACBrETQ
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "ETQ_ImprimirCaixa", Vertical, Horizontal, Largura, Altura, EspessuraVertical, EspessuraHorizontal)
    ::CheckResult(hResult)
    RETURN nil

METHOD ImprimirImagem(MultiplicadorImagem, Vertical, Horizontal, eNomeImagem) CLASS ACBrETQ
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "ETQ_ImprimirImagem", MultiplicadorImagem, Vertical, Horizontal, hb_StrToUTF8(eNomeImagem))
    ::CheckResult(hResult)
    RETURN nil

METHOD ImprimirQRCode(Vertical, Horizontal, Texto, LarguraModulo, ErrorLevel, Tipo) CLASS ACBrETQ
    local hResult
    hResult := DllCall(::hHandle, DLL_OSAPI, "ETQ_ImprimirQRCode", Vertical, Horizontal, hb_StrToUTF8(Texto), LarguraModulo, ErrorLevel, Tipo)
    ::CheckResult(hResult)
    RETURN nil
    