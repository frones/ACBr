VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ACBrNFSe"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Private libHandler As Long

Const BUFFER_LENGTH As Long = 1024

Private Declare Function NFSE_Inicializar _
                Lib "ACBrNFSe32.dll" (ByRef libHandler As Long, _
                                      ByVal eArqConfig As String, _
                                      ByVal eChaveCrypt As String) As Long
                   
Private Declare Function NFSE_Finalizar Lib "ACBrNFSe32.dll" (ByVal libHandler As Long) As Long

Private Declare Function NFSE_Nome _
                Lib "ACBrNFSe32.dll" (ByVal libHandler As Long, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                   
Private Declare Function NFSE_Versao _
                Lib "ACBrNFSe32.dll" (ByVal libHandler As Long, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long

Private Declare Function NFSE_UltimoRetorno _
                Lib "ACBrNFSe32.dll" (ByVal libHandler As Long, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                   
Private Declare Function NFSE_ConfigImportar _
                Lib "ACBrNFSe32.dll" (ByVal libHandler As Long, _
                                      ByVal eArqConfig As String) As Long
                   
Private Declare Function NFSE_ConfigExportar _
                Lib "ACBrNFSe32.dll" (ByVal libHandler As Long, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                   
Private Declare Function NFSE_ConfigLer _
                Lib "ACBrNFSe32.dll" (ByVal libHandler As Long, _
                                      ByVal eArqConfig As String) As Long

Private Declare Function NFSE_ConfigGravar _
                Lib "ACBrNFSe32.dll" (ByVal libHandler As Long, _
                                      ByVal eArqConfig As String) As Long
                   
Private Declare Function NFSE_ConfigLerValor _
                Lib "ACBrNFSe32.dll" (ByVal libHandler As Long, _
                                      ByVal eSessao As String, _
                                      ByVal eChave As String, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long

Private Declare Function NFSE_ConfigGravarValor _
                Lib "ACBrNFSe32.dll" (ByVal libHandler As Long, _
                                      ByVal eSessao As String, _
                                      ByVal eChave As String, _
                                      ByVal valor As String) As Long
                                      
Private Declare Function NFSE_CarregarXML _
                Lib "ACBrNFSe32.dll" (ByVal libHandler As Long, _
                                      ByVal eArquivoOuXml As String) As Long
                
Private Declare Function NFSE_CarregarINI _
                Lib "ACBrNFSe32.dll" (ByVal libHandler As Long, _
                                      ByVal eArquivoOuIni As String) As Long
                
Private Declare Function NFSE_ObterXml _
                Lib "ACBrNFSe32.dll" (ByVal libHandler As Long, _
                                      ByVal AIndex As Long, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                                     
Private Declare Function NFSE_GravarXml _
                Lib "ACBrNFSe32.dll" (ByVal libHandler As Long, _
                                      ByVal AIndex As Long, _
                                      ByVal eNomeArquivo As String, _
                                      ByVal ePathArquivo As String) As Long
                                     
Private Declare Function NFSE_ObterIni _
                Lib "ACBrNFSe32.dll" (ByVal libHandler As Long, _
                                      ByVal AIndex As Long, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                                     
Private Declare Function NFSE_GravarIni _
                Lib "ACBrNFSe32.dll" (ByVal libHandler As Long, _
                                      ByVal AIndex As Long, _
                                      ByVal eNomeArquivo As String, _
                                      ByVal ePathArquivo As String) As Long
                                     
Private Declare Function NFSE_LimparLista Lib "ACBrNFSe32.dll" (ByVal libHandler As Long) As Long

        
Private Declare Function NFSE_ObterCertificados _
                Lib "ACBrNFSe32.dll" (ByVal libHandler As Long, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                                     
Private Declare Function NFSE_Emitir _
                Lib "ACBrNFSe32.dll" (ByVal libHandler As Long, _
                                      ByVal aLote As String, _
                                      ByVal aModoEnvio As Long, _
                                      ByVal aImprimir As Boolean, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                                      
Private Declare Function NFSE_Cancelar _
                Lib "ACBrNFSe32.dll" (ByVal libHandler As Long, _
                                      ByVal aInfCancelamento As String, _
                                      ByVal buffer As String, _
                                      ByRef bufferSize As Long) As Long
                                      
Private Declare Function NFSE_SubstituirNFSe _
                Lib "ACBrNFSe32.dll" (ByVal libHandler As Long, _
                                      ByVal aNumeroNFSe As String, _
                                      ByVal aSerieNFSe As String, _
                                      ByVal aCodigoCancelamento As String, _
                                      ByVal aMotivoCancelamento As String, _
                                      ByVal aNumeroLote As String, _
                                      ByVal aCodigoVerificacao As String, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                                      
Private Declare Function NFSE_LinkNFSe _
                Lib "ACBrNFSe32.dll" (ByVal libHandler As Long, _
                                      ByVal aNumeroNFSe As String, _
                                      ByVal aCodigoVerificacao As String, _
                                      ByVal aChaveAcesso As String, _
                                      ByVal aValorServico As String, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                                      
Private Declare Function NFSE_GerarLote _
                Lib "ACBrNFSe32.dll" (ByVal libHandler As Long, _
                                      ByVal aLote As String, _
                                      ByVal aQtdMaximaRps As Long, _
                                      ByVal aModoEnvio As Long, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                                      
Private Declare Function NFSE_GerarToken _
                Lib "ACBrNFSe32.dll" (ByVal libHandler As Long, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                                      
Private Declare Function NFSE_ConsultarSituacao _
                Lib "ACBrNFSe32.dll" (ByVal libHandler As Long, _
                                      ByVal aProtocolo As String, _
                                      ByVal aNumeroLote As String, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                                      
Private Declare Function NFSE_ConsultarLoteRps _
                Lib "ACBrNFSe32.dll" (ByVal libHandler As Long, _
                                      ByVal aProcotolo As String, _
                                      ByVal aNumLote As String, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                                      
Private Declare Function NFSE_ConsultarNFSePorRps _
                Lib "ACBrNFSe32.dll" (ByVal libHandler As Long, _
                                      ByVal aNumeroRps As String, _
                                      ByVal aSerie As String, _
                                      ByVal aTipo As String, _
                                      ByVal aCodigoVerificacao As String, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                                      
Private Declare Function NFSE_ConsultarNFSePorNumero _
                Lib "ACBrNFSe32.dll" (ByVal libHandler As Long, _
                                      ByVal aNumero As String, _
                                      ByVal aPagina As Long, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long

Private Declare Function NFSE_ConsultarNFSePorPeriodo _
                Lib "ACBrNFSe32.dll" (ByVal libHandler As Long, _
                                      ByVal aDataInicial As Date, _
                                      ByVal aDataFinal As Date, _
                                      ByVal aPagina As Long, _
                                      ByVal aNumeroLote As String, _
                                      ByVal aTipoPeriodo As Long, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                                      
Private Declare Function NFSE_ConsultarNFSePorFaixa _
                Lib "ACBrNFSe32.dll" (ByVal libHandler As Long, _
                                      ByVal aNumeroInicial As String, _
                                      ByVal aNumeroFinal As String, _
                                      ByVal aPagina As Long, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                                      
Private Declare Function NFSE_ConsultarNFSeGenerico _
                Lib "ACBrNFSe32.dll" (ByVal libHandler As Long, _
                                      ByVal aInfConsultaNFSe As String, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                                     
Private Declare Function NFSE_EnviarEmail _
                Lib "ACBrNFSe32.dll" (ByVal libHandler As Long, _
                                      ByVal ePara As String, _
                                      ByVal eXmlNFSe As String, _
                                      ByVal aEnviaPDF As Boolean, _
                                      ByVal eAssunto As String, _
                                      ByVal eCc As String, _
                                      ByVal eAnexos As String, _
                                      ByVal eMensagem As String) As Long
                                     
Private Declare Function NFSE_Imprimir _
                Lib "ACBrNFSe32.dll" (ByVal libHandler As Long, _
                                      ByVal cImpressora As String, _
                                      ByVal nNumCopias As Long, _
                                      ByVal bGerarPDF As String, _
                                      ByVal bMostrarPreview As String, _
                                      ByVal cCancelada As String) As Long
                                      
Private Declare Function NFSE_ImprimirPDF Lib "ACBrNFSe32.dll" (ByVal libHandler As Long) As Long

Private Declare Function NFSE_SalvarPDF Lib "ACBrNFSe32.dll" (ByVal libHandler As Long, _
                                                                ByVal buffer As String, _
                                                                ByRef bufferLen As Long) As Long

Private Declare Function NFSE_ConsultarNFSeServicoPrestadoPorNumero _
                Lib "ACBrNFSe32.dll" (ByVal libHandler As Long, _
                                      ByVal aNumero As String, _
                                      ByVal aPagina As Long, _
                                      ByVal aDataInicial As Date, _
                                      ByVal aDataFinal As Date, _
                                      ByVal aTipoPeriodo As Long, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                                      
Private Declare Function NFSE_ConsultarNFSeServicoPrestadoPorPeriodo _
                Lib "ACBrNFSe32.dll" (ByVal libHandler As Long, _
                                      ByVal aDataInicial As Date, _
                                      ByVal aDataFinal As Date, _
                                      ByVal aPagina As Long, _
                                      ByVal aTipoPeriodo As Long, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                                      
Private Declare Function NFSE_ConsultarNFSeServicoPrestadoPorTomador _
                Lib "ACBrNFSe32.dll" (ByVal libHandler As Long, _
                                      ByVal aCNPJ As String, _
                                      ByVal aInscMun As String, _
                                      ByVal aPagina As Long, _
                                      ByVal aDataInicial As Date, _
                                      ByVal aDataFinal As Date, _
                                      ByVal aTipoPeriodo As Long, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                                      
Private Declare Function NFSE_ConsultarNFSeServicoPrestadoPorIntermediario _
                Lib "ACBrNFSe32.dll" (ByVal libHandler As Long, _
                                      ByVal aCNPJ As String, _
                                      ByVal aInscMun As String, _
                                      ByVal aPagina As Long, _
                                      ByVal aDataInicial As Date, _
                                      ByVal aDataFinal As Date, _
                                      ByVal aTipoPeriodo As Long, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                                      
Private Declare Function NFSE_ConsultarNFSeServicoTomadoPorNumero _
                Lib "ACBrNFSe32.dll" (ByVal libHandler As Long, _
                                      ByVal aNumero As String, _
                                      ByVal aPagina As Long, _
                                      ByVal aDataInicial As Date, _
                                      ByVal aDataFinal As Date, _
                                      ByVal aTipoPeriodo As Long, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                                      
Private Declare Function NFSE_ConsultarNFSeServicoTomadoPorPrestador _
                Lib "ACBrNFSe32.dll" (ByVal libHandler As Long, _
                                      ByVal aCNPJ As String, _
                                      ByVal aInscMun As String, _
                                      ByVal aPagina As Long, _
                                      ByVal aDataInicial As Date, _
                                      ByVal aDataFinal As Date, _
                                      ByVal aTipoPeriodo As Long, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                                      
Private Declare Function NFSE_ConsultarNFSeServicoTomadoPorTomador _
                Lib "ACBrNFSe32.dll" (ByVal libHandler As Long, _
                                      ByVal aCNPJ As String, _
                                      ByVal aInscMun As String, _
                                      ByVal aPagina As Long, _
                                      ByVal aDataInicial As Date, _
                                      ByVal aDataFinal As Date, _
                                      ByVal aTipoPeriodo As Long, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                                      
Private Declare Function NFSE_ConsultarNFSeServicoTomadoPorPeriodo _
                Lib "ACBrNFSe32.dll" (ByVal libHandler As Long, _
                                      ByVal aDataInicial As Date, _
                                      ByVal aDataFinal As Date, _
                                      ByVal aPagina As Long, _
                                      ByVal aTipoPeriodo As Long, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                                      
Private Declare Function NFSE_ConsultarNFSeServicoTomadoPorIntermediario _
                Lib "ACBrNFSe32.dll" (ByVal libHandler As Long, _
                                      ByVal aCNPJ As String, _
                                      ByVal aInscMun As String, _
                                      ByVal aPagina As Long, _
                                      ByVal aDataInicial As Date, _
                                      ByVal aDataFinal As Date, _
                                      ByVal aTipoPeriodo As Long, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long

Private Declare Function NFSE_EnviarEvento _
               Lib "ACBrNFSe32.dll" (ByVal libHandler As Long, _
                                     ByVal aInfEvento As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                     
Private Declare Function NFSE_ConsultarDPSPorChave _
               Lib "ACBrNFSe32.dll" (ByVal libHandler As Long, _
                                     ByVal aChaveDPS As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                     
Private Declare Function NFSE_ConsultarNFSePorChave _
               Lib "ACBrNFSe32.dll" (ByVal libHandler As Long, _
                                     ByVal aChaveNFSe As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                                                          
Private Declare Function NFSE_ConsultarEvento _
                Lib "ACBrNFSe32.dll" (ByVal libHandler As Long, _
                                      ByVal aChave As String, _
                                      ByVal aTipoEvento As Long, _
                                      ByVal aNumSeq As Long, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                                      
Private Declare Function NFSE_ConsultarDFe _
                Lib "ACBrNFSe32.dll" (ByVal libHandler As Long, _
                                      ByVal aNSU As Long, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                                      
Private Declare Function NFSE_ObterDANFSE _
               Lib "ACBrNFSe32.dll" (ByVal libHandler As Long, _
                                     ByVal aChaveNFSe As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                     
Private Declare Function NFSE_ConsultarParametros _
               Lib "ACBrNFSe32.dll" (ByVal libHandler As Long, _
                                     ByVal aTipoParametroMunicipio As Long, _
                                     ByVal aCodigoServico As String, _
                                     ByVal aCompetencia As Date, _
                                     ByVal aNumeroBeneficio As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
Public Sub InicializarLib(Optional ByVal eArqConfig As String = "", _
                          Optional ByVal eChaveCrypt As String = "")

    SetLibPath
    Dim retorno As Long

    retorno = NFSE_Inicializar(libHandler, eArqConfig, eChaveCrypt)
    CheckResult retorno
End Sub

Public Sub FinalizarLib()

    Dim retorno As Long

    retorno = NFSE_Finalizar(libHandler)
    CheckResult retorno
End Sub

Public Sub ConfigImportar(ByVal eArqConfig As String)

    Dim retorno As Long

    retorno = NFSE_ConfigImportar(libHandler, eArqConfig)
    CheckResult (retorno)
    
End Sub
    
Public Function ConfigExportar() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = NFSE_ConfigExportar(libHandler, buffer, bufferLen)
    CheckResult retorno
    
    ConfigExportar = processResult(buffer, bufferLen)
    
End Function

Public Sub ConfigLer(Optional eArqConfig As String = "")

    Dim retorno As Long

    retorno = NFSE_ConfigLer(libHandler, eArqConfig)
    CheckResult retorno
End Sub

Public Sub ConfigGravar(Optional eArqConfig As String = "")

    Dim retorno As Long

    retorno = NFSE_ConfigGravar(libHandler, eArqConfig)
    CheckResult retorno
End Sub

Public Function ConfigLerValor(ByVal eSessao As String, ByVal eChave As String) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NFSE_ConfigLerValor(libHandler, eSessao, eChave, buffer, bufferLen)
    CheckResult retorno
    
    ConfigLerValor = processResult(buffer, bufferLen)
End Function

Public Sub ConfigGravarValor(ByVal eSessao As String, _
                             ByVal eChave As String, _
                             ByVal valor As String)

    Dim retorno As Long
    
    retorno = NFSE_ConfigGravarValor(libHandler, eSessao, eChave, valor)
    CheckResult retorno
End Sub

Public Function Nome() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
        
    retorno = NFSE_Nome(libHandler, buffer, bufferLen)
    CheckResult retorno
    
    Nome = processResult(buffer, bufferLen)
End Function

Public Function Versao() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = NFSE_Versao(libHandler, buffer, bufferLen)
    CheckResult retorno
    
    Versao = processResult(buffer, bufferLen)
End Function

Public Sub CarregarXML(ByVal eArquivoOuXml As String)
    Dim retorno   As Long
    retorno = NFSE_CarregarXML(libHandler, eArquivoOuXml)
    CheckResult retorno
End Sub

Public Sub CarregarINI(ByVal eArquivoOuIni As String)
    Dim retorno   As Long
    retorno = NFSE_CarregarINI(libHandler, eArquivoOuIni)
    CheckResult retorno
End Sub

Public Function ObterXml(ByVal AIndex As Long) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NFSE_ObterXml(libHandler, AIndex, buffer, bufferLen)
    CheckResult retorno
    
    ObterXml = processResult(buffer, bufferLen)
End Function

Public Sub GravarXml(ByVal AIndex As Long, _
                     Optional ByVal eNomeArquivo As String = "", _
                     Optional ByVal ePathArquivo As String = "")
    Dim retorno   As Long
    retorno = NFSE_GravarXml(libHandler, AIndex, eNomeArquivo, ePathArquivo)
    CheckResult retorno
End Sub

Public Function ObterIni(ByVal AIndex As Long) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NFSE_ObterIni(libHandler, AIndex, buffer, bufferLen)
    CheckResult retorno
    
    ObterIni = processResult(buffer, bufferLen)
End Function

Public Sub GravarIni(ByVal AIndex As Long, _
                     ByVal eNomeArquivo As String, _
                     Optional ByVal ePathArquivo As String = "")
    Dim retorno   As Long
    retorno = NFSE_GravarIni(libHandler, AIndex, eNomeArquivo, ePathArquivo)
    CheckResult retorno
End Sub

Public Sub LimparLista()
    Dim retorno   As Long
    retorno = NFSE_LimparLista(libHandler)
    CheckResult retorno
End Sub

Public Function ObterCertificados() As String()
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NFSE_ObterCertificados(libHandler, buffer, bufferLen)
    CheckResult retorno
    
    ObterCertificados = Split(processResult(buffer, bufferLen), vbCrLf)
End Function

Public Function Emitir(ByVal aLote As String, ByVal aModoEnvio As Long, ByVal aImprimir As Boolean) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NFSE_Emitir(libHandler, aLote, aModoEnvio, aImprimir, buffer, bufferLen)
    CheckResult retorno
    
    Emitir = processResult(buffer, bufferLen)
    
End Function


Public Function Cancelar(ByVal aInfCancelamento As String) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NFSE_Cancelar(libHandler, aInfCancelamento, buffer, bufferLen)
    CheckResult retorno
    
    Cancelar = processResult(buffer, bufferLen)

End Function

Public Function SubstituirNFSe(ByVal aNumeroNFSe As String, ByVal aSerieNFSe As String, ByVal aCodigoCancelamento As String, ByVal aMotivoCancelamento As String, ByVal aNumeroLote As String, ByVal aCodigoVerificacao As String) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NFSE_SubstituirNFSe(libHandler, aNumeroLote, aSerieNFSe, aCodigoCancelamento, aMotivoCancelamento, aNumeroLote, aCodigoVerificacao, buffer, bufferLen)
    CheckResult retorno
    
    SubstituirNFSe = processResult(buffer, bufferLen)

End Function

Public Function LinkNFSE(ByVal aNumeroNFSe As String, ByVal aCodigoVerificacao As String, ByVal aChaveAcesso As String, ByVal aValorServico As String) As String
    
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NFSE_LinkNFSe(libHandler, aNumeroNFSe, aCodigoVerificacao, aChaveAcesso, aValorServico, buffer, bufferLen)
    CheckResult retorno
    
    LinkNFSE = processResult(buffer, bufferLen)
    
End Function

Public Function GerarLote(ByVal aLote As String, ByVal aQtdMaximaRps As Long, ByVal aModoEnvio As Long) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NFSE_GerarLote(libHandler, aLote, aQtdMaximaRps, aModoEnvio, buffer, bufferLen)
    CheckResult retorno
    
    GerarLote = processResult(buffer, bufferLen)

End Function

Public Function GerarToken()
    
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NFSE_GerarToken(libHandler, buffer, bufferLen)
    CheckResult retorno
    
    GerarToken = processResult(buffer, bufferLen)
    
End Function

Public Function ConsultarSitucao(ByVal aProtocolo As String, ByVal aNumeroLote As String) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NFSE_ConsultarSituacao(libHandler, aProtocolo, aNumeroLote, buffer, bufferLen)
    CheckResult retorno
    
    ConsultarSitucao = processResult(buffer, bufferLen)

End Function

Public Function ConsultarLoteRps(ByVal aProcotolo As String, ByVal aNumLote As String) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NFSE_ConsultarLoteRps(libHandler, aProcotolo, aNumLote, buffer, bufferLen)
    CheckResult retorno
    
    ConsultarLoteRps = processResult(buffer, bufferLen)

End Function

Public Function ConsultarNFSePorRps(ByVal aNumeroRps As String, ByVal aSerie As String, ByVal aTipo As String, ByVal aCodigoVerificacao As String) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NFSE_ConsultarNFSePorRps(libHandler, aNumeroRps, aSerie, aTipo, aCodigoVerificacao, buffer, bufferLen)
    CheckResult retorno
    
    ConsultarNFSePorRps = processResult(buffer, bufferLen)

End Function

Public Function ConsultarNFSePorNumero(ByVal aNumero As String, ByVal aPagina As Long) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NFSE_ConsultarNFSePorNumero(libHandler, aNumero, aPagina, buffer, bufferLen)
    CheckResult retorno
    
    ConsultarNFSePorNumero = processResult(buffer, bufferLen)

End Function

Public Function ConsultarNFSePorPeriodo(ByVal aDataInicial As Date, ByVal aDataFinal As Date, ByVal aPagina As Long, ByVal aNumeroLote As String, ByVal aTipoPeriodo As Long) As String
    
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NFSE_ConsultarNFSePorPeriodo(libHandler, aDataInicial, aDataFinal, aPagina, aNumeroLote, aTipoPeriodo, buffer, bufferLen)
    CheckResult retorno
    
    ConsultarNFSePorPeriodo = processResult(buffer, bufferLen)
    
End Function

Public Function ConsultarNFSePorFaixa(ByVal aNumeroInicial As String, ByVal aNumeroFinal As String, ByVal aPagina As Long) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NFSE_ConsultarNFSePorFaixa(libHandler, aNumeroFinal, aNumeroFinal, aPagina, buffer, bufferLen)
    CheckResult retorno
    
    ConsultarNFSePorFaixa = processResult(buffer, bufferLen)

End Function

Public Function ConsultarNFSeGenerico(ByVal aInfConsultaNFSe As String) As String
    
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NFSE_ConsultarNFSeGenerico(libHandler, aInfConsultaNFSe, buffer, bufferLen)
    CheckResult retorno
    
    ConsultarNFSeGenerico = processResult(buffer, bufferLen)
    
End Function

Public Sub EnviarEmail(ByVal ePara As String, ByVal eXmlNFSe As String, ByVal aEnviaPDF As Boolean, ByVal eAssunto As String, ByVal eMensagem As String, Optional ByVal eCc As String = "", Optional ByVal eAnexos As String = "")
                       
    Dim retorno   As Long
    
    retorno = NFSE_EnviarEmail(libHandler, ePara, eXmlNFSe, aEnviaPDF, eAssunto, eCc, eAnexos, eMensagem)
    CheckResult retorno
    
End Sub


Public Sub Imprimir(Optional ByVal cImpressora As String = "", Optional ByVal nNumCopias As Long = 1, Optional ByVal bGerarPDF As String = "", Optional ByVal bMostrarPreview As String = "", Optional ByVal cCancelada As String = "")

    Dim retorno   As Long
    
    retorno = NFSE_Imprimir(libHandler, cImpressora, nNumCopias, bGerarPDF, bMostrarPreview, cCancelada)
    CheckResult retorno
    
End Sub

Public Sub ImprimirPDF()

    Dim retorno   As Long
    
    retorno = NFSE_ImprimirPDF(libHandler)
    CheckResult retorno
    
End Sub

Public Function SalvarPDF() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NFSE_SalvarPDF(libHandler, buffer, bufferLen)
    CheckResult retorno
    
    SalvarPDF = processResult(buffer, bufferLen)

End Function

Public Function ConsultarNFSeServicoPrestadoPorNumero(ByVal aNumero As String, ByVal aPagina As Long, ByVal aDataInicial As Date, ByVal aDataFinal As Date, ByVal aTipoPeriodo As Long) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NFSE_ConsultarNFSeServicoPrestadoPorNumero(libHandler, aNumero, aPagina, aDataInicial, aDataFinal, aTipoPeriodo, buffer, bufferLen)
    CheckResult retorno
    
    ConsultarNFSeServicoPrestadoPorNumero = processResult(buffer, bufferLen)

End Function

Public Function ConsultarNFSeServicoPrestadoPorPeriodo(ByVal aDataInicial As Date, ByVal aDataFinal As Date, ByVal aPagina As Long, ByVal aTipoPeriodo As Long) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NFSE_ConsultarNFSeServicoPrestadoPorPeriodo(libHandler, aDataInicial, aDataFinal, aPagina, aTipoPeriodo, buffer, bufferLen)
    CheckResult retorno
    
    ConsultarNFSeServicoPrestadoPorPeriodo = processResult(buffer, bufferLen)

End Function

Public Function ConsultarNFSeServicoPrestadoPorTomador(ByVal aCNPJ As String, ByVal aInscMun As String, ByVal aPagina As Long, ByVal aDataInicial As Date, ByVal aDataFinal As Date, ByVal aTipoPeriodo As Long) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NFSE_ConsultarNFSeServicoPrestadoPorTomador(libHandler, aCNPJ, aInscMun, aPagina, aDataInicial, aDataFinal, aTipoPeriodo, buffer, bufferLen)
    CheckResult retorno
    
    ConsultarNFSeServicoPrestadoPorTomador = processResult(buffer, bufferLen)

End Function

Public Function ConsultarNFSeServicoPrestadoPorIntermediario(ByVal aCNPJ As String, ByVal aInscMun As String, ByVal aPagina As Long, ByVal aDataInicial As Date, ByVal aDataFinal As Date, ByVal aTipoPeriodo As Long) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NFSE_ConsultarNFSeServicoPrestadoPorIntermediario(libHandler, aCNPJ, aInscMun, aPagina, aDataInicial, aDataFinal, aTipoPeriodo, buffer, bufferLen)
    CheckResult retorno
    
    ConsultarNFSeServicoPrestadoPorIntermediario = processResult(buffer, bufferLen)

End Function

Public Function ConsultarNFSeServicoTomadoPorNumero(ByVal aNumero As String, ByVal aPagina As Long, ByVal aDataInicial As Date, ByVal aDataFinal As Date, ByVal aTipoPeriodo As Long) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NFSE_ConsultarNFSeServicoTomadoPorNumero(libHandler, aNumero, aPagina, aDataInicial, aDataFinal, aTipoPeriodo, buffer, bufferLen)
    CheckResult retorno
    
    ConsultarNFSeServicoTomadoPorNumero = processResult(buffer, bufferLen)

End Function

Public Function ConsultarNFSeServicoTomadoPorPrestador(ByVal aCNPJ As String, ByVal aInscMun As String, ByVal aPagina As Long, ByVal aDataInicial As Date, ByVal aDataFinal As Date, ByVal aTipoPeriodo As Long) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NFSE_ConsultarNFSeServicoTomadoPorPrestador(libHandler, aCNPJ, aInscMun, aPagina, aDataInicial, aDataFinal, aTipoPeriodo, buffer, bufferLen)
    CheckResult retorno
    
    ConsultarNFSeServicoTomadoPorPrestador = processResult(buffer, bufferLen)

End Function

Public Function ConsultarNFSeServicoTomadoPorTomador(ByVal aCNPJ As String, ByVal aInscMun As String, ByVal aPagina As Long, ByVal aDataInicial As Date, ByVal aDataFinal As Date, ByVal aTipoPeriodo As Long) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NFSE_ConsultarNFSeServicoTomadoPorTomador(libHandler, aCNPJ, aInscMun, aPagina, aDataInicial, aDataFinal, aTipoPeriodo, buffer, bufferLen)
    CheckResult retorno
    
    ConsultarNFSeServicoTomadoPorTomador = processResult(buffer, bufferLen)

End Function

Public Function ConsultarNFSeServicoTomadoPorPeriodo(ByVal aDataInicial As Date, ByVal aDataFinal As Date, ByVal aPagina As Long, ByVal aTipoPeriodo As Long) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NFSE_ConsultarNFSeServicoTomadoPorPeriodo(libHandler, aDataInicial, aDataFinal, aPagina, aTipoPeriodo, buffer, bufferLen)
    CheckResult retorno
    
    ConsultarNFSeServicoTomadoPorPeriodo = processResult(buffer, bufferLen)

End Function

Public Function ConsultarNFSeServicoTomadoPorIntermediario(ByVal aCNPJ As String, ByVal aInscMun As String, ByVal aPagina As Long, ByVal aDataInicial As Date, ByVal aDataFinal As Date, ByVal aTipoPeriodo As Long) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NFSE_ConsultarNFSeServicoTomadoPorIntermediario(libHandler, aCNPJ, aInscMun, aPagina, aDataInicial, aDataFinal, aTipoPeriodo, buffer, bufferLen)
    CheckResult retorno
    
    ConsultarNFSeServicoTomadoPorIntermediario = processResult(buffer, bufferLen)

End Function

Public Function EnviarEvento(ByVal aInfEvento As String) As String

    Dim retorno As Long
    Dim buffer As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NFSE_EnviarEvento(libHandler, aInfEvento, buffer, bufferLen)
    CheckResult retorno
    
    EnviarEvento = processResult(buffer, bufferLen)

End Function

Public Function ConsultarDPSPorChave(ByVal aChaveDPS As String) As String
    
    Dim retorno As Long
    Dim buffer As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NFSE_ConsultarDPSPorChave(libHandler, aChaveDPS, buffer, bufferLen)
    CheckResult retorno
    
    ConsultarDPSPorChave = processResult(buffer, bufferLen)
    
End Function

Public Function ConsultarNFSePorChave(ByVal aChaveNFSe As String) As String

    Dim retorno As Long
    Dim buffer As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NFSE_ConsultarNFSePorChave(libHandler, aChaveNFSe, buffer, bufferLen)
    CheckResult retorno
    
    ConsultarNFSePorChave = processResult(buffer, bufferLen)

End Function

Public Function ConsultarEvento(ByVal aChave As String, ByVal aTipoEvento As Long, ByVal aNumSeq As Long) As String

    Dim retorno As Long
    Dim buffer As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NFSE_ConsultarEvento(libHandler, aChave, buffer, bufferLen)
    CheckResult retorno
    
    ConsultarEvento = processResult(buffer, bufferLen)

End Function

Public Function ConsultarDFe(ByVal aNSU As Long) As String

    Dim retorno As Long
    Dim buffer As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NFSE_ConsultarDFe(libHandler, aNSU, buffer, bufferLen)
    CheckResult retorno
    
    ConsultarDFe = processResult(buffer, bufferLen)

End Function

Public Function ObterDANFSE(ByVal aChaveNFSe As String) As String

    Dim retorno As Long
    Dim buffer As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NFSE_ObterDANFSE(libHandler, aChaveNFSe, buffer, bufferLen)
    CheckResult retorno
    
    ObterDANFSE = processResult(buffer, bufferLen)

End Function

Public Function ConsultarParametros(ByVal aTipoParametroMunicipio As Long, ByVal aCodigoServico As String, ByVal aCompetencia As Date, ByVal aNumeroBeneficio As String) As String

    Dim retorno As Long
    Dim buffer As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NFSE_ConsultarParametros(libHandler, aTipoParametroMunicipio, aCodigoServico, aCompetencia, aNumeroBeneficio, buffer, bufferLen)
    CheckResult retorno
    
    ConsultarParametros = processResult(buffer, bufferLen)

End Function


Private Sub CheckResult(ByVal Resultado As Long)
    
    If Resultado >= 0 Then Exit Sub
         
    Dim buffer As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    NFSE_UltimoRetorno libHandler, buffer, bufferLen
    
0     Err.Raise Resultado, "ACBrNFSe", processResult(buffer, bufferLen)
End Sub

Private Function processResult(ByRef buffer As String, ByRef bufferLen As Long) As String
    
    If bufferLen > BUFFER_LENGTH Then
        buffer = String$(bufferLen, " ")
        NFSE_UltimoRetorno libHandler, buffer, bufferLen
    End If

    processResult = Trim$(FromUTF8(buffer))
End Function
