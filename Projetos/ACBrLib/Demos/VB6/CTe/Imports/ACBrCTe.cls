VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ACBrCTe"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Const BUFFER_LENGTH As Long = 1024
                                
Private Declare Function CTE_Inicializar _
                Lib "ACBrCTe32.dll" (ByVal eArqConfig As String, _
                                      ByVal eChaveCrypt As String) As Long
                   
Private Declare Function CTE_Finalizar Lib "ACBrCTe32.dll" () As Long

Private Declare Function CTE_Nome _
                Lib "ACBrCTe32.dll" (ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                   
Private Declare Function CTE_Versao _
                Lib "ACBrCTe32.dll" (ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long

Private Declare Function CTE_UltimoRetorno _
                Lib "ACBrCTe32.dll" (ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                   
Private Declare Function CTE_ConfigLer _
                Lib "ACBrCTe32.dll" (ByVal eArqConfig As String) As Long

Private Declare Function CTE_ConfigGravar _
                Lib "ACBrCTe32.dll" (ByVal eArqConfig As String) As Long
                   
Private Declare Function CTE_ConfigLerValor _
                Lib "ACBrCTe32.dll" (ByVal eSessao As String, _
                                      ByVal eChave As String, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long

Private Declare Function CTE_ConfigGravarValor _
                Lib "ACBrCTe32.dll" (ByVal eSessao As String, _
                                      ByVal eChave As String, _
                                      ByVal valor As String) As Long
                                      
Private Declare Function CTE_CarregarXML _
                Lib "ACBrCTe32.dll" (ByVal eArquivoOuXml As String) As Long
                
Private Declare Function CTE_CarregarINI _
                Lib "ACBrCTe32.dll" (ByVal eArquivoOuIni As String) As Long
                
Private Declare Function CTE_ObterXml _
                Lib "ACBrCTe32.dll" (ByVal AIndex As Long, _
                                     ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Private Declare Function CTE_GravarXml _
                Lib "ACBrCTe32.dll" (ByVal AIndex As Long, _
                                     ByVal eNomeArquivo As String, _
                                     ByVal ePathArquivo As String) As Long
                                     
Private Declare Function CTE_ObterIni _
                Lib "ACBrCTe32.dll" (ByVal AIndex As Long, _
                                     ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Private Declare Function CTE_GravarIni _
                Lib "ACBrCTe32.dll" (ByVal AIndex As Long, _
                                     ByVal eNomeArquivo As String, _
                                     ByVal ePathArquivo As String) As Long
                
Private Declare Function CTE_CarregarEventoXML _
                Lib "ACBrCTe32.dll" (ByVal eArquivoOuXml As String) As Long
                
Private Declare Function CTE_CarregarEventoINI _
                Lib "ACBrCTe32.dll" (ByVal eArquivoOuIni As String) As Long
                
Private Declare Function CTE_LimparLista Lib "ACBrCTe32.dll" () As Long

Private Declare Function CTE_LimparListaEventos Lib "ACBrCTe32.dll" () As Long

Private Declare Function CTE_Assinar Lib "ACBrCTe32.dll" () As Long

Private Declare Function CTE_Validar Lib "ACBrCTe32.dll" () As Long

Private Declare Function CTE_ValidarRegrasdeNegocios _
                Lib "ACBrCTe32.dll" (ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Private Declare Function CTE_VerificarAssinatura _
                Lib "ACBrCTe32.dll" (ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                    
Private Declare Function CTE_StatusServico _
                Lib "ACBrCTe32.dll" (ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Private Declare Function CTE_Consultar _
                Lib "ACBrCTe32.dll" (ByVal eChaveOuNFe As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Private Declare Function CTE_Inutilizar _
                Lib "ACBrCTe32.dll" (ByVal acnpj As String, _
                                     ByVal aJustificativa As String, _
                                     ByVal ano As Long, _
                                     ByVal modelo As Long, _
                                     ByVal serie As Long, _
                                     ByVal numeroInicial As Long, _
                                     ByVal numeroFinal As Long, _
                                     ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Private Declare Function CTE_Enviar _
                Lib "ACBrCTe32.dll" (ByVal aLote As Long, _
                                     ByVal Imprimir As Boolean, _
                                     ByVal sincrono As Boolean, _
                                     ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Private Declare Function CTE_ConsultarRecibo _
                Lib "ACBrCTe32.dll" (ByVal aRecibo As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Private Declare Function CTE_ConsultaCadastro _
                Lib "ACBrCTe32.dll" (ByVal cUF As String, _
                                     ByVal nDocumento As String, _
                                     ByVal nIE As Boolean, _
                                     ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Private Declare Function CTE_Cancelar _
                Lib "ACBrCTe32.dll" (ByVal eChave As String, _
                                     ByVal eJustificativa As String, _
                                     ByVal eCNPJ As String, _
                                     ByVal aLote As Long, _
                                     ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Private Declare Function CTE_EnviarEvento _
                Lib "ACBrCTe32.dll" (ByVal aLote As Long, _
                                     ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Private Declare Function CTE_DistribuicaoDFePorUltNSU _
                Lib "ACBrCTe32.dll" (ByVal acUFAutor As Long, _
                                     ByVal eCnpjcpf As String, _
                                     ByVal eultNsu As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Private Declare Function CTE_DistribuicaoDFePorNSU _
                Lib "ACBrCTe32.dll" (ByVal acUFAutor As Long, _
                                     ByVal eCnpjcpf As String, _
                                     ByVal eNsu As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Private Declare Function CTE_DistribuicaoDFePorChave _
                Lib "ACBrCTe32.dll" (ByVal acUFAutor As Long, _
                                     ByVal eCnpjcpf As String, _
                                     ByVal echNFe As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Private Declare Function CTE_EnviarEmail _
                Lib "ACBrCTe32.dll" (ByVal ePara As Long, _
                                     ByVal eChaveNFe As String, _
                                     ByVal aEnviaPDF As Boolean, _
                                     ByVal eAssunto As String, _
                                     ByVal eCc As String, _
                                     ByVal eAnexos As String, _
                                     ByVal eMensagem As String) As Long
                                     
Private Declare Function CTE_EnviarEmailEvento _
                Lib "ACBrCTe32.dll" (ByVal ePara As Long, _
                                     ByVal eChaveNFe As String, _
                                     ByVal aEnviaPDF As Boolean, _
                                     ByVal eAssunto As String, _
                                     ByVal eCc As String, _
                                     ByVal eAnexos As String, _
                                     ByVal eMensagem As String) As Long
                                     
Private Declare Function CTE_Imprimir _
                Lib "ACBrCTe32.dll" (ByVal cImpressora As String, _
                                     ByVal nNumCopias As Long, _
                                     ByVal cProtocolo As String, _
                                     ByVal bMostrarPreview As String) As Long
                                     
Private Declare Function CTE_ImprimirPDF Lib "ACBrCTe32.dll" () As Long

Private Declare Function CTE_ImprimirEvento _
                Lib "ACBrCTe32.dll" (ByVal eArquivoXmlNFe As String, _
                                     ByVal eArquivoXmlEvento As String) As Long
                                     
Private Declare Function CTE_ImprimirEventoPDF _
                Lib "ACBrCTe32.dll" (ByVal eArquivoXmlNFe As String, _
                                     ByVal eArquivoXmlEvento As String) As Long
                                     
Private Declare Function CTE_ImprimirInutilizacao _
                Lib "ACBrCTe32.dll" (ByVal eArquivoXml As String) As Long
                
Private Declare Function CTE_ImprimirInutilizacaoPDF _
                Lib "ACBrCTe32.dll" (ByVal eArquivoXml As String) As Long

Public Sub InicializarLib(Optional ByVal eArqConfig As String = "", _
                          Optional ByVal eChaveCrypt As String = "")

    Dim retorno As Long

    retorno = CTE_Inicializar(eArqConfig, eChaveCrypt)
    CheckResult retorno
End Sub

Public Sub FinalizarLib()

    Dim retorno As Long

    retorno = CTE_Finalizar
    CheckResult retorno
End Sub

Public Sub ConfigLer(Optional eArqConfig As String = "")

    Dim retorno As Long

    retorno = CTE_ConfigLer(eArqConfig)
    CheckResult retorno
End Sub

Public Sub ConfigGravar(Optional eArqConfig As String = "")

    Dim retorno As Long

    retorno = CTE_ConfigGravar(eArqConfig)
    CheckResult retorno
End Sub

Public Function ConfigLerValor(ByVal eSessao As String, ByVal eChave As String) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = CTE_ConfigLerValor(eSessao, eChave, buffer, bufferLen)
    CheckResult retorno
    
    ConfigLerValor = processResult(buffer, bufferLen)
End Function

Public Sub ConfigGravarValor(ByVal eSessao As String, _
                             ByVal eChave As String, _
                             ByVal valor As String)

    Dim retorno As Long
    
    retorno = CTE_ConfigGravarValor(eSessao, eChave, valor)
    CheckResult retorno
End Sub

Public Function Nome() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
        
    retorno = CTE_Nome(buffer, bufferLen)
    CheckResult retorno
    
    Nome = processResult(buffer, bufferLen)
End Function

Public Function Versao() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = CTE_Versao(buffer, bufferLen)
    CheckResult retorno
    
    Versao = processResult(buffer, bufferLen)
End Function

Public Sub CarregarXML(ByVal eArquivoOuXml As String)
    Dim retorno   As Long
    retorno = CTE_CarregarXML(eArquivoOuXml)
    CheckResult retorno
End Sub

Public Sub CarregarINI(ByVal eArquivoOuIni As String)
    Dim retorno   As Long
    retorno = CTE_CarregarINI(eArquivoOuIni)
    CheckResult retorno
End Sub

Public Function ObterXml(ByVal AIndex As Long) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = CTE_ObterXml(AIndex, buffer, bufferLen)
    CheckResult retorno
    
    ObterXml = processResult(buffer, bufferLen)
End Function

Public Sub GravarXml(ByVal AIndex As Long, _
                     Optional ByVal eNomeArquivo As String = "", _
                     Optional ByVal ePathArquivo As String = "")
    Dim retorno   As Long
    retorno = CTE_GravarXml(AIndex, eNomeArquivo, ePathArquivo)
    CheckResult retorno
End Sub

Public Function ObterIni(ByVal AIndex As Long) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = CTE_ObterIni(AIndex, buffer, bufferLen)
    CheckResult retorno
    
    ObterIni = processResult(buffer, bufferLen)
End Function

Public Sub GravarIni(ByVal AIndex As Long, _
                     ByVal eNomeArquivo As String, _
                     Optional ByVal ePathArquivo As String = "")
    Dim retorno   As Long
    retorno = CTE_GravarIni(AIndex, eNomeArquivo, ePathArquivo)
    CheckResult retorno
End Sub

Public Sub CarregarEventoXML(ByVal eArquivoOuXml As String)
    Dim retorno   As Long
    retorno = CTE_CarregarEventoXML(eArquivoOuXml)
    CheckResult retorno
End Sub

Public Sub CarregarEventoINI(ByVal eArquivoOuIni As String)
    Dim retorno   As Long
    retorno = CTE_CarregarEventoINI(eArquivoOuIni)
    CheckResult retorno
End Sub

Public Sub LimparLista()
    Dim retorno   As Long
    retorno = CTE_LimparLista
    CheckResult retorno
End Sub

Public Sub LimparListaEventos()
    Dim retorno   As Long
    retorno = CTE_LimparListaEventos
    CheckResult retorno
End Sub

Public Sub Assinar()
    Dim retorno   As Long
    retorno = CTE_Assinar
    CheckResult retorno
End Sub

Public Sub Validar()
    Dim retorno   As Long
    retorno = CTE_Validar
    CheckResult retorno
End Sub

Public Function ValidarRegrasdeNegocios() As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = CTE_ValidarRegrasdeNegocios(buffer, bufferLen)
    CheckResult retorno
    
    ValidarRegrasdeNegocios = processResult(buffer, bufferLen)
End Function

Public Function VerificarAssinatura() As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = CTE_VerificarAssinatura(buffer, bufferLen)
    CheckResult retorno
    
    VerificarAssinatura = processResult(buffer, bufferLen)
End Function

Public Function StatusServico() As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = CTE_StatusServico(buffer, bufferLen)
    CheckResult retorno
    
    StatusServico = processResult(buffer, bufferLen)
End Function

Public Function Consultar(ByVal eChaveOuNFe As String) As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = CTE_Consultar(eChaveOuNFe, buffer, bufferLen)
    CheckResult retorno
    
    Consultar = processResult(buffer, bufferLen)
End Function

Public Function ConsultaCadastro(ByVal cUF As String, ByVal nDocumento As String, ByVal nIE As Boolean) As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = CTE_ConsultaCadastro(cUF, nDocumento, nIE, buffer, bufferLen)
    CheckResult retorno
    
    ConsultaCadastro = processResult(buffer, bufferLen)
End Function

Public Function Inutilizar(ByVal acnpj As String, ByVal aJustificativa As String, ByVal ano As Long, _
                           ByVal modelo As Long, ByVal serie As Long, ByVal numeroInicial As Long, _
                           ByVal numeroFinal As Long) As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = CTE_Inutilizar(acnpj, aJustificativa, ano, modelo, serie, numeroInicial, numeroFinal, buffer, bufferLen)
    CheckResult retorno
    
    Inutilizar = processResult(buffer, bufferLen)
End Function

Public Function Enviar(ByVal aLote As Long, Optional ByVal Imprimir As Boolean = False, _
                       Optional ByVal sincrono As Boolean = False) As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = CTE_Enviar(aLote, Imprimir, sincrono, buffer, bufferLen)
    CheckResult retorno
    
    Enviar = processResult(buffer, bufferLen)
End Function

Public Function ConsultarRecibo(ByVal aRecibo As String) As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = CTE_ConsultarRecibo(aRecibo, buffer, bufferLen)
    CheckResult retorno
    
    ConsultarRecibo = processResult(buffer, bufferLen)
End Function

Public Function Cancelar(ByVal eChave As String, ByVal eJustificativa As String, _
                         ByVal eCNPJ As String, ByVal aLote As Long) As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = CTE_Cancelar(eChave, eJustificativa, eCNPJ, aLote, buffer, bufferLen)
    CheckResult retorno
    
    Cancelar = processResult(buffer, bufferLen)
End Function

Public Function EnviarEvento(ByVal aLote As Long) As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = CTE_EnviarEvento(aLote, buffer, bufferLen)
    CheckResult retorno
    
    EnviarEvento = processResult(buffer, bufferLen)
End Function

Public Function DistribuicaoDFePorUltNSU(ByVal acUFAutor As String, ByVal eCnpjcpf As String, ByVal eultNsu As String) As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = CTE_DistribuicaoDFePorUltNSU(acUFAutor, eCnpjcpf, eultNsu, buffer, bufferLen)
    CheckResult retorno
    
    DistribuicaoDFePorUltNSU = processResult(buffer, bufferLen)
End Function

Public Function DistribuicaoDFePorNSU(ByVal acUFAutor As String, ByVal eCnpjcpf As String, ByVal eNsu As String) As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = CTE_DistribuicaoDFePorNSU(acUFAutor, eCnpjcpf, eNsu, buffer, bufferLen)
    CheckResult retorno
    
    DistribuicaoDFePorNSU = processResult(buffer, bufferLen)
End Function

Public Function DistribuicaoDFePorChave(ByVal acUFAutor As String, ByVal eCnpjcpf As String, ByVal echNFe As String) As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = CTE_DistribuicaoDFePorChave(acUFAutor, eCnpjcpf, echNFe, buffer, bufferLen)
    CheckResult retorno
    
    DistribuicaoDFePorChave = processResult(buffer, bufferLen)
End Function

Public Sub EnviarEmail(ByVal ePara As String, ByVal eChaveNFe As String, ByVal aEnviaPDF As Boolean, _
                       ByVal eAssunto As String, ByVal eMensagem As String, _
                       Optional ByVal eCc As String = "", Optional ByVal eAnexos As String = "")
    Dim retorno   As Long
    
    retorno = CTE_EnviarEmail(ePara, eChaveNFe, aEnviaPDF, eAssunto, eCc, eAnexos, eMensagem)
    CheckResult retorno
End Sub

Public Sub EnviarEmailEvento(ByVal ePara As String, ByVal eChaveNFe As String, ByVal aEnviaPDF As Boolean, _
                       ByVal eAssunto As String, ByVal eMensagem As String, _
                       Optional ByVal eCc As String = "", Optional ByVal eAnexos As String = "")
    Dim retorno   As Long
    
    retorno = CTE_EnviarEmailEvento(ePara, eChaveNFe, aEnviaPDF, eAssunto, eCc, eAnexos, eMensagem)
    CheckResult retorno
End Sub

Public Sub Imprimir(Optional ByVal cImpressora As String = "", Optional ByVal nNumCopias As Long = 1, _
                    Optional ByVal cProtocolo As String = "", Optional ByVal bMostrarPreview As String = "")
    Dim retorno   As Long
    
    retorno = CTE_Imprimir(cImpressora, nNumCopias, cProtocolo, bMostrarPreview)
    CheckResult retorno
End Sub

Public Sub ImprimirPDF()
    Dim retorno   As Long
    
    retorno = CTE_ImprimirPDF()
    CheckResult retorno
End Sub

Public Sub ImprimirEvento(ByVal eArquivoXmlNFe As String, ByVal eArquivoXmlEvento As String)
    Dim retorno   As Long
    
    retorno = CTE_ImprimirEvento(eArquivoXmlNFe, eArquivoXmlEvento)
    CheckResult retorno
End Sub

Public Sub ImprimirEventoPDF(ByVal eArquivoXmlNFe As String, ByVal eArquivoXmlEvento As String)
    Dim retorno   As Long
   
    retorno = CTE_ImprimirEventoPDF(eArquivoXmlNFe, eArquivoXmlEvento)
    CheckResult retorno
End Sub

Public Sub ImprimirInutilizacao(ByVal eArquivoXml As String)
    Dim retorno   As Long
       
    retorno = CTE_ImprimirInutilizacao(eArquivoXml)
    CheckResult retorno
End Sub

Public Sub ImprimirInutilizacaoPDF(ByVal eArquivoXml As String)
    Dim retorno   As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = CTE_ImprimirInutilizacaoPDF(eArquivoXml)
    CheckResult retorno
End Sub

Private Sub CheckResult(ByVal Resultado As Long)
    
    If Resultado >= 0 Then Exit Sub
         
    Dim buffer As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    CTE_UltimoRetorno buffer, bufferLen
    
    Err.Raise Resultado, "ACBrCTe", processResult(buffer, bufferLen)
End Sub

Private Function processResult(ByRef buffer As String, ByRef bufferLen As Long) As String
    
    If bufferLen > BUFFER_LENGTH Then
        buffer = String$(bufferLen, " ")
        CTE_UltimoRetorno buffer, bufferLen
    End If

    processResult = Trim$(FromUTF8(buffer))
End Function
