* Manual: https://acbr.sourceforge.io/ACBrLib/ACBrLibSat.html
#Define STR_LEN 256
#Define STR_TO_UTF8 9
#Define UTF8_TO_STR 11
#Define ACBrLIB 'ACBrSAT32.dll'

Procedure ACBrSat_VFP_Declare
	DECLARE INTEGER SAT_Inicializar IN ACBrLIB String arqConfig, String chaveCrypt
	DECLARE INTEGER SAT_Finalizar IN ACBrLIB
	DECLARE INTEGER SAT_UltimoRetorno IN ACBrLIB String @buffer, Integer @bufferLen
	DECLARE INTEGER SAT_Nome IN ACBrLIB String @buffer, Integer @bufferLen
	DECLARE INTEGER SAT_Versao IN ACBrLIB String @buffer, Integer @bufferLen
	DECLARE INTEGER SAT_ConfigLer IN ACBrLIB String arqConfig
	DECLARE INTEGER SAT_ConfigGravar IN ACBrLIB String arqConfig
	DECLARE INTEGER SAT_ConfigLerValor IN ACBrLIB String Chave, String sessao, String @buffer, Integer @bufferLen
	DECLARE INTEGER SAT_ConfigGravarValor IN ACBrLIB String Chave, String sessao, String valor
	DECLARE INTEGER SAT_InicializarSAT IN ACBrLIB
	DECLARE INTEGER SAT_DesInicializar IN ACBrLIB
	DECLARE INTEGER SAT_ConsultarSAT IN ACBrLIB String @buffer, Integer @bufferLen
	DECLARE INTEGER SAT_AssociarAssinatura IN ACBrLIB String CNPJ, String sessao, String @buffer, Integer @bufferLen
	DECLARE INTEGER SAT_BloquearSAT IN ACBrLIB String @buffer, Integer @bufferLen
	DECLARE INTEGER SAT_DesbloquearSAT IN ACBrLIB String @buffer, Integer @bufferLen
	DECLARE INTEGER SAT_TrocarCodigoDeAtivacao IN ACBrLIB String codigoDeAtivacaoOuEmergencia, Integer opcao, String novoCodigo, String @buffer, Integer @bufferLen
	DECLARE INTEGER SAT_ConsultarStatusOperacional IN ACBrLIB String @buffer, Integer @bufferLen
	DECLARE INTEGER SAT_ConsultarNumeroSessao IN ACBrLIB Integer numeroDeSessao, String @buffer, Integer @bufferLen
	DECLARE INTEGER SAT_AtualizarSoftwareSAT IN ACBrLIB String @buffer, Integer @bufferLen
	DECLARE INTEGER SAT_ComunicarCertificadoICPBRASIL IN ACBrLIB String certificado, String @buffer, Integer @bufferLen
	DECLARE INTEGER SAT_ExtrairLogs IN ACBrLIB String arquivo
	DECLARE INTEGER SAT_TesteFimAFim IN ACBrLIB String arquivoXmlVenda, String @buffer, Integer @bufferLen
	DECLARE INTEGER SAT_GerarAssinaturaSAT IN ACBrLIB String cnpjSoftwareHouse, String cnpjEmitente, String @buffer, Integer @bufferLen
	DECLARE INTEGER SAT_CriarCFe IN ACBrLIB String arquivoIni, String @buffer, Integer @bufferLen
	DECLARE INTEGER SAT_CriarEnviarCFe IN ACBrLIB String arquivoIni, String @buffer, Integer @bufferLen
	DECLARE INTEGER SAT_EnviarCFe IN ACBrLIB String arquivoXml, String @buffer, Integer @bufferLen
	DECLARE INTEGER SAT_CancelarCFe IN ACBrLIB String arquivoXml, String @buffer, Integer @bufferLen
	DECLARE INTEGER SAT_ImprimirExtratoVenda IN ACBrLIB String arquivoXml, String nomeImpressora
	DECLARE INTEGER SAT_ImprimirExtratoResumido IN ACBrLIB String arquivoXml, String nomeImpressora
	DECLARE INTEGER SAT_ImprimirExtratoCancelamento IN ACBrLIB String arquivoXml, String arquivoXmlCancelamento, String nomeImpressora
	DECLARE INTEGER SAT_GerarImpressaoFiscalMFe IN ACBrLIB String arquivoXml, String @buffer, Integer @bufferLen
	DECLARE INTEGER SAT_GerarPDFExtratoVenda IN ACBrLIB String arquivoXml, String arquivoPdf, String @buffer, Integer @bufferLen
	DECLARE INTEGER SAT_GerarPDFCancelamento IN ACBrLIB String arquivoXml, String arquivoXmlCancelamento, String arquivoPdf, String @buffer, Integer @bufferLen
	DECLARE INTEGER SAT_EnviarEmail IN ACBrLIB String arquivoXml, String destinatarioEmail, String assuntoEmail, String anexoArquivoPdf, String mensagemEmail, String comCopiaEmail, String anexosEmail
EndProc

Define Class ACBrSat As Custom

	* Se VFP_GerarErro=.T., o método irá gerar um erro de execução ao invés de retornar falso.
	VFP_GerarErro = .F.

	MensagemSAT = ""
	RetornoSAT_Bruto = ""
	RetornoSAT_NumeroSessao = ""
	RetornoSAT_EEEEE = ""
	RetornoSAT_CCCC = ""
	RetornoSAT_Mensagem = ""
	RetornoSAT_CodRef = ""
	RetornoSAT_MensagemSEFAZ = ""
	RetornoSAT_CFeBase64 = ""
	RetornoSAT_TimeStamp = ""
	RetornoSAT_ChaveConsulta = ""
	RetornoSAT_ValorTotalCFe = ""
	RetornoSAT_CPFCNPJ = ""
	RetornoSAT_AssinaturaQRCODE = ""
	RetornoSAT_CFeXML = ""
	RetornoSAT_CFeNumero = ""

	Procedure Init(arqConfig, chaveCrypt)
		local hResult, buffer, bufferLen, oErr
		arqConfig = Iif(Vartype(arqConfig)#"C", "", arqConfig)
    	chaveCrypt = Iif(Vartype(chaveCrypt)#"C", "", chaveCrypt)
		hResult = SAT_Inicializar(StrConv(arqConfig, STR_TO_UTF8), StrConv(chaveCrypt, STR_TO_UTF8))
		Return This.TratarResultado(hResult)
	EndProc

	Procedure Destroy()
		SAT_Finalizar()
		Return
	EndProc

	Procedure TratarResultado(hResult, buffer, bufferLen)
		This.MensagemSAT = ""
		Local buffer, bufferLen, oErr
		If hResult >= 0
			* Tudo OK
			If Vartype(buffer)="C"
				If bufferLen > STR_LEN
					buffer = Space(bufferLen)
					If SAT_UltimoRetorno(@buffer, @bufferLen) < 0
						This.MensagemSAT = "#ERRO#1 = SAT_UltimoRetorno: "+Strconv(buffer, UTF8_TO_STR)
						Return .F.
					EndIf
				EndIf
				This.MensagemSAT = Strconv(buffer, UTF8_TO_STR)
			EndIf
			Return .T.
	    EndIf    

		* Erro
	    bufferLen = STR_LEN
	    buffer = Space(bufferLen)
	    If SAT_UltimoRetorno(@buffer, @bufferLen) < 0
			This.MensagemSAT = "#ERRO#2 = SAT_UltimoRetorno: "+Strconv(buffer, UTF8_TO_STR)
			Return .F.
		EndIf
	    If bufferLen > STR_LEN
	        buffer = Space(bufferLen)
		    If SAT_UltimoRetorno(@buffer, @bufferLen) < 0
				This.MensagemSAT = "#ERRO#3 = SAT_UltimoRetorno: "+Strconv(buffer, UTF8_TO_STR)
				Return .F.
			EndIf
	    EndIf
		This.MensagemSAT = "#ERRO#"+Strconv(buffer, UTF8_TO_STR)

   		If This.VFP_GerarErro = .T.
			ERROR This.MensagemSAT
		EndIf

	    Return .F.

	EndProc

	Procedure Nome
		local hResult, buffer, bufferLen, ret
		bufferLen = STR_LEN
		buffer = Space(bufferLen)
		hResult = SAT_Nome(@buffer, @bufferLen)
		RETURN This.TratarResultado(hResult, @buffer, @bufferLen)
	EndProc

	Procedure Versao
		local hResult, buffer, bufferLen
		bufferLen = STR_LEN
		buffer = Space(bufferLen)
		hResult = SAT_Versao(@buffer, @bufferLen)
		RETURN This.TratarResultado(hResult, @buffer, @bufferLen)
	EndProc

	Procedure ConfigLer(arqConfig)
	    local hResult    
	    hResult = SAT_ConfigLer(StrConv(arqConfig, STR_TO_UTF8))
	    Return This.TratarResultado(hResult)
	EndProc

	Procedure ConfigGravar(arqConfig)
        local hResult    
        hResult = SAT_ConfigGravar(StrConv(arqConfig, STR_TO_UTF8))
        Return This.TratarResultado(hResult)
	EndProc

	Procedure ConfigLerValor(sessao, chave)
	    local hResult, buffer, bufferLen
	    bufferLen = STR_LEN
	    buffer = Space(bufferLen)
	    hResult = SAT_ConfigLerValor(StrConv(sessao, STR_TO_UTF8), StrConv(chave, STR_TO_UTF8), @buffer, @bufferLen)
		RETURN This.TratarResultado(hResult, @buffer, @bufferLen)
	EndProc

	Procedure ConfigGravarValor(sessao, chave, valor)
	    local hResult
	    hResult = SAT_ConfigGravarValor( StrConv(sessao, STR_TO_UTF8), StrConv(chave, STR_TO_UTF8), StrConv(valor, STR_TO_UTF8))
	    Return This.TratarResultado(hResult)
	EndProc

	Procedure InicializarSAT
	    local hResult
	    hResult = SAT_InicializarSAT()
	    hResult = Iif(hResult = -8, 0, hResult) && -8 já iniciado (ignora)
    	Return This.TratarResultado(hResult)
	EndProc

	Procedure DesInicializarSAT
	    local hResult
	    hResult = SAT_DesInicializar()
	    hResult = Iif(hResult = -8, 0, hResult) && -8 já finalizado (ignora)
    	Return This.TratarResultado(hResult)
	EndProc

	Procedure ConsultarSAT()
		local hResult, buffer, bufferLen
		bufferLen = STR_LEN
		buffer = Space(bufferLen)   
		hResult = SAT_ConsultarSAT(@buffer, @bufferLen)
		Return This.TratarResultado(hResult, @buffer, @bufferLen)
	EndProc

	Procedure AssociarAssinatura(CNPJvalue, assinaturaCNPJs)
		local hResult, buffer, bufferLen
		bufferLen = STR_LEN
		buffer = Space(bufferLen)   
		hResult = SAT_AssociarAssinatura(StrConv(CNPJvalue, STR_TO_UTF8), StrConv(assinaturaCNPJs, STR_TO_UTF8), @buffer, @bufferLen)
		Return This.TratarResultado(hResult, @buffer, @bufferLen)
	EndProc

	Procedure BloquearSAT()
		local hResult, buffer, bufferLen
		bufferLen = STR_LEN
		buffer = Space(bufferLen)   
		hResult = SAT_BloquearSAT(@buffer, @bufferLen)
		Return This.TratarResultado(hResult, @buffer, @bufferLen)
	EndProc

	Procedure DesbloquearSAT()
		local hResult, buffer, bufferLen
		bufferLen = STR_LEN
		buffer = Space(bufferLen)   
		hResult = SAT_DesbloquearSAT(@buffer, @bufferLen)
		Return This.TratarResultado(hResult, @buffer, @bufferLen)
	EndProc

	Procedure TrocarCodigoDeAtivacao(codigoDeAtivacaoOuEmergencia, opcao, novoCodigo)
	    local hResult, buffer, bufferLen
	    bufferLen = STR_LEN
	    buffer = Space(bufferLen)   
	    hResult = SAT_TrocarCodigoDeAtivacao(StrConv(codigoDeAtivacaoOuEmergencia, STR_TO_UTF8), opcao, StrConv(novoCodigo, STR_TO_UTF8), @buffer, @bufferLen)
		Return This.TratarResultado(hResult, @buffer, @bufferLen)
	EndProc

	Procedure ConsultarStatusOperacional()
	    local hResult, buffer, bufferLen
	    bufferLen = STR_LEN
	    buffer = Space(bufferLen)   
	    hResult = SAT_ConsultarStatusOperacional(@buffer, @bufferLen)
		Return This.TratarResultado(hResult, @buffer, @bufferLen)
	EndProc

	Procedure ConsultarNumeroSessao(numeroDeSessao)
	    local hResult, buffer, bufferLen
	    bufferLen = STR_LEN
	    buffer = Space(bufferLen)   
	    hResult = SAT_ConsultarNumeroSessao(numeroDeSessao, @buffer, @bufferLen)
		Return This.TratarResultado(hResult, @buffer, @bufferLen)
	EndProc

	Procedure AtualizarSoftwareSAT()
	    local hResult, buffer, bufferLen
	    bufferLen = STR_LEN
	    buffer = Space(bufferLen)   
	    hResult = SAT_AtualizarSoftwareSAT(@buffer, @bufferLen)
		Return This.TratarResultado(hResult, @buffer, @bufferLen)
	EndProc

	Procedure ComunicarCertificadoICPBRASIL(certificado)
	    local hResult, buffer, bufferLen
	    bufferLen = STR_LEN
	    buffer = Space(bufferLen)   
	    hResult = SAT_ComunicarCertificadoICPBRASIL(StrConv(certificado, STR_TO_UTF8), @buffer, @bufferLen)
		Return This.TratarResultado(hResult, @buffer, @bufferLen)
	EndProc

	Procedure ExtrairLogs(arquivo)
	    local hResult
	    hResult = SAT_ExtrairLogs(StrConv(arquivo, STR_TO_UTF8))
	    Return This.TratarResultado(hResult)

	Procedure TesteFimAFim(arquivoXmlVenda)
	    local hResult, buffer, bufferLen
	    bufferLen = STR_LEN
	    buffer = Space(bufferLen)   
	    hResult = SAT_TesteFimAFim(StrConv(arquivoXmlVenda, STR_TO_UTF8), @buffer, @bufferLen)
		Return This.TratarResultado(hResult, @buffer, @bufferLen)
	EndProc

	Procedure GerarAssinaturaSAT(cnpjSoftwareHouse, cnpjEmitente)
	    local hResult, buffer, bufferLen
	    bufferLen = STR_LEN
	    buffer = Space(bufferLen)   
	    hResult = SAT_GerarAssinaturaSAT(StrConv(cnpjSoftwareHouse, STR_TO_UTF8), StrConv(cnpjEmitente, STR_TO_UTF8), @buffer, @bufferLen)
		Return This.TratarResultado(hResult, @buffer, @bufferLen)
	EndProc

	Procedure CriarCFe(arquivoIni)
	    local hResult, buffer, bufferLen
	    bufferLen = STR_LEN
	    buffer = Space(bufferLen)   
	    hResult = SAT_CriarCFe(StrConv(arquivoIni, STR_TO_UTF8), @buffer, @bufferLen)
		Return This.TratarResultado(hResult, @buffer, @bufferLen)
	EndProc

	Procedure CriarEnviarCFe(arquivoIni)
		This.LimparMensagemDeRetornoSAT()
	    local hResult, buffer, bufferLen
	    bufferLen = STR_LEN
	    buffer = Space(bufferLen)   
	    hResult = SAT_CriarEnviarCFe(StrConv(arquivoIni, STR_TO_UTF8), @buffer, @bufferLen)
		If This.TratarResultado(hResult, @buffer, @bufferLen) = .F.
			Return .F.
		EndIf
		Return This.TratarMensagemDeRetornoSAT("EnviarDadosVenda")
	EndProc

	Procedure EnviarCFe(arquivoXml)
		This.LimparMensagemDeRetornoSAT()
	    local hResult, buffer, bufferLen
	    bufferLen = STR_LEN
	    buffer = Space(bufferLen)   
	    hResult = SAT_EnviarCFe(StrConv(arquivoXml, STR_TO_UTF8), @buffer, @bufferLen)
		If This.TratarResultado(hResult, @buffer, @bufferLen) = .F.
			Return .F.
		EndIf
		Return This.TratarMensagemDeRetornoSAT("EnviarDadosVenda")
	EndProc

	Procedure CancelarCFe(arquivoXml)
		This.LimparMensagemDeRetornoSAT()
	    local hResult, buffer, bufferLen
	    bufferLen = STR_LEN
	    buffer = Space(bufferLen)   
	    hResult = SAT_CancelarCFe(StrConv(arquivoXml, STR_TO_UTF8), @buffer, @bufferLen)
		If This.TratarResultado(hResult, @buffer, @bufferLen) = .F.
			Return .F.
		EndIf
		Return This.TratarMensagemDeRetornoSAT("CancelarUltimaVenda")
	EndProc

	Procedure LimparMensagemDeRetornoSAT()
		Store "" To This.RetornoSAT_Bruto,;
					This.RetornoSAT_NumeroSessao,;
					This.RetornoSAT_EEEEE,;
					This.RetornoSAT_CCCC,;
					This.RetornoSAT_Mensagem,;
					This.RetornoSAT_CodRef,;
					This.RetornoSAT_MensagemSEFAZ,;
					This.RetornoSAT_CFeBase64,;
					This.RetornoSAT_TimeStamp,;
					This.RetornoSAT_ChaveConsulta,;
					This.RetornoSAT_ValorTotalCFe,;
					This.RetornoSAT_CPFCNPJ,;
					This.RetornoSAT_AssinaturaQRCODE,;
					This.RetornoSAT_CFeXML,;
					This.RetornoSAT_CFeNumero
	EndProc

	Procedure TratarMensagemDeRetornoSAT(m.cOperacao)
		This.RetornoSAT_Bruto = StrExtract(This.MensagemSAT, "Resultado=", Chr(13), 1, 2)
		This.RetornoSAT_NumeroSessao = StrExtract('|'+This.RetornoSAT_Bruto+"|", '|', '|', 1)
		This.RetornoSAT_EEEEE = StrExtract('|'+This.RetornoSAT_Bruto+"|", '|', '|', 2)
		This.RetornoSAT_CCCC = StrExtract('|'+This.RetornoSAT_Bruto+"|", '|', '|', 3)
		This.RetornoSAT_Mensagem = StrExtract('|'+This.RetornoSAT_Bruto+"|", '|', '|', 4)
		This.RetornoSAT_CodRef = StrExtract('|'+This.RetornoSAT_Bruto+"|", '|', '|', 5)
		This.RetornoSAT_MensagemSEFAZ = StrExtract('|'+This.RetornoSAT_Bruto+"|", '|', '|', 6)
		This.RetornoSAT_CFeBase64 = StrExtract('|'+This.RetornoSAT_Bruto+"|", '|', '|', 7)
		This.RetornoSAT_TimeStamp = StrExtract('|'+This.RetornoSAT_Bruto+"|", '|', '|', 8)
		This.RetornoSAT_ChaveConsulta = StrExtract('|'+This.RetornoSAT_Bruto+"|", '|', '|', 9)
		This.RetornoSAT_ValorTotalCFe = StrExtract('|'+This.RetornoSAT_Bruto+"|", '|', '|', 10)
		This.RetornoSAT_CPFCNPJ = StrExtract('|'+This.RetornoSAT_Bruto+"|", '|', '|', 11)
		This.RetornoSAT_AssinaturaQRCODE = StrExtract('|'+This.RetornoSAT_Bruto+"|", '|', '|', 12)
		This.RetornoSAT_CFeXML = Strconv(This.RetornoSAT_CFeBase64, 14)
		This.RetornoSAT_CFeNumero = Substr(This.RetornoSAT_ChaveConsulta, 35, 06)
		Do Case
			Case m.cOperacao = "EnviarDadosVenda" and This.RetornoSAT_EEEEE = "06000"
				Return .T.
			Case m.cOperacao = "CancelarUltimaVenda" and This.RetornoSAT_EEEEE = "07000"
				Return .T.
		EndCase
		Return .F.
	EndProc

	Procedure ImprimirExtratoVenda(arquivoXml, nomeImpressora)
	    local hResult
	    hResult = SAT_ImprimirExtratoVenda(StrConv(arquivoXml, STR_TO_UTF8), StrConv(nomeImpressora, STR_TO_UTF8))
	    Return This.TratarResultado(hResult)
	EndProc

	Procedure ImprimirExtratoResumido(arquivoXml, nomeImpressora)
	    local hResult
	    hResult = SAT_ImprimirExtratoResumido(StrConv(arquivoXml, STR_TO_UTF8), StrConv(nomeImpressora, STR_TO_UTF8))
	    Return This.TratarResultado(hResult)
	EndProc

	Procedure ImprimirExtratoCancelamento(arquivoXml, arquivoXmlCancelamento, nomeImpressora)
	    local hResult 
	    hResult = SAT_ImprimirExtratoCancelamento(StrConv(arquivoXml, STR_TO_UTF8), StrConv(arquivoXmlCancelamento, STR_TO_UTF8), StrConv(nomeImpressora, STR_TO_UTF8))
	    Return This.TratarResultado(hResult)
	EndProc

	Procedure GerarImpressaoFiscalMFe(arquivoXml)
	    local hResult, buffer, bufferLen
	    bufferLen = STR_LEN
	    buffer = Space(bufferLen)   
	    hResult = SAT_GerarImpressaoFiscalMFe(StrConv(arquivoXml, STR_TO_UTF8), @buffer, @bufferLen)
		Return This.TratarResultado(hResult, @buffer, @bufferLen)
	EndProc

	Procedure GerarPDFExtratoVenda(arquivoXml, arquivoPdf)
	    local hResult, buffer, bufferLen
	    bufferLen = STR_LEN
	    buffer = Space(bufferLen)   
	    hResult = SAT_GerarPDFExtratoVenda(StrConv(arquivoXml, STR_TO_UTF8), StrConv(arquivoPdf, STR_TO_UTF8), @buffer, @bufferLen)
		Return This.TratarResultado(hResult, @buffer, @bufferLen)
	EndProc

	Procedure GerarPDFCancelamento(arquivoXml, arquivoXmlCancelamento, arquivoPdf)
	    local hResult, buffer, bufferLen
	    bufferLen = STR_LEN
	    buffer = Space(bufferLen)   
	    hResult = SAT_GerarPDFCancelamento(StrConv(arquivoXml, STR_TO_UTF8), StrConv(arquivoXmlCancelamento, STR_TO_UTF8), StrConv(arquivoPdf, STR_TO_UTF8), @buffer, @bufferLen)
		Return This.TratarResultado(hResult, @buffer, @bufferLen)
	EndProc

	Procedure EnviarEmail(arquivoXml, destinatarioEmail, assuntoEmail, anexoArquivoPdf, mensagemEmail, comCopiaEmail, anexosEmail)
	    local hResult
	    hResult = SAT_EnviarEmail(StrConv(arquivoXml, STR_TO_UTF8), StrConv(destinatarioEmail, STR_TO_UTF8), StrConv(assuntoEmail, STR_TO_UTF8), StrConv(anexoArquivoPdf, STR_TO_UTF8), StrConv(mensagemEmail, STR_TO_UTF8), StrConv(comCopiaEmail, STR_TO_UTF8), StrConv(anexosEmail, STR_TO_UTF8))
	    Return This.TratarResultado(hResult)
	EndProc

EndDefine