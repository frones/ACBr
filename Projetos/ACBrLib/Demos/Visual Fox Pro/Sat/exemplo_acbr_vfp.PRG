
Set Procedure To PJ_Classes_DF_SAT_SP_ACBR_ACBRLIB.PRG

Do ACBrSat_VFP_Declare

oLIB = CreateObject('ACBrSat', 'C:\ARQUIVO.INI', '')

oLib.InicializarSAT()

oLib.CriarEnviarCFe('c:\ARQUIVO_DADOS_SAT.INI')

oLib.DesInicializarSAT()

If oLib.RetornoSAT_EEEEE # "06000"

	MessageBox("#ERRO#" + Chr(13) +;
		oLib.RetornoSAT_EEEEE + " / " + oLib.RetornoSAT_CCCC + Chr(13) +;
		oLib.RetornoSAT_Mensagem + Chr(13)  + Chr(13) +;
		oLib.RetornoSAT_CodRef + Chr(13) +;
		oLib.RetornoSAT_MensagemSEFAZ )

	Return

EndIf

? oLib.RetornoSAT_ChaveConsulta
? oLib.RetornoSAT_TimeStamp
? oLib.RetornoSAT_CPFCNPJ
? oLib.RetornoSAT_CodRef
? oLib.RetornoSAT_MensagemSEFAZ
? oLib.RetornoSAT_NumeroSessao
? oLib.RetornoSAT_CFeXML
? oLib.RetornoSAT_AssinaturaQRCODE
? oLib.RetornoSAT_CFeNumero
? oLib.RetornoSAT_ValorTotalCFe
	