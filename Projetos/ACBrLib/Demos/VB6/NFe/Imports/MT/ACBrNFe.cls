VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ACBrNFe"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Private libHandler As Long

Const BUFFER_LENGTH As Long = 1024
                               
Private Declare Function NFE_Inicializar _
                Lib "ACBrNFE32.dll" (ByRef libHandler As Long, _
                                     ByVal eArqConfig As String, _
                                     ByVal eChaveCrypt As String) As Long
                   
Private Declare Function NFE_Finalizar Lib "ACBrNFE32.dll" (ByVal libHandler As Long) As Long

Private Declare Function NFE_Nome _
                Lib "ACBrNFE32.dll" (ByVal libHandler As Long, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                   
Private Declare Function NFE_Versao _
                Lib "ACBrNFE32.dll" (ByVal libHandler As Long, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long

Private Declare Function NFE_UltimoRetorno _
                Lib "ACBrNFE32.dll" (ByVal libHandler As Long, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                   
Private Declare Function NFE_ConfigImportar _
                Lib "ACBrNFE32.dll" (ByVal libHandler As Long, _
                                     ByVal eArqConfig As String) As Long
                   
Private Declare Function NFE_ConfigExportar _
                Lib "ACBrNFE32.dll" (ByVal libHandler As Long, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                   
Private Declare Function NFE_ConfigLer _
                Lib "ACBrNFE32.dll" (ByVal libHandler As Long, _
                                     ByVal eArqConfig As String) As Long

Private Declare Function NFE_ConfigGravar _
                Lib "ACBrNFE32.dll" (ByVal libHandler As Long, _
                                     ByVal eArqConfig As String) As Long
                   
Private Declare Function NFE_ConfigLerValor _
                Lib "ACBrNFE32.dll" (ByVal libHandler As Long, _
                                     ByVal eSessao As String, _
                                     ByVal eChave As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long

Private Declare Function NFE_ConfigGravarValor _
                Lib "ACBrNFE32.dll" (ByVal libHandler As Long, _
                                     ByVal eSessao As String, _
                                     ByVal eChave As String, _
                                     ByVal valor As String) As Long
                                      
Private Declare Function NFE_CarregarXML _
                Lib "ACBrNFE32.dll" (ByVal libHandler As Long, _
                                     ByVal eArquivoOuXml As String) As Long
                
Private Declare Function NFE_CarregarINI _
                Lib "ACBrNFE32.dll" (ByVal libHandler As Long, _
                                     ByVal eArquivoOuIni As String) As Long
                
Private Declare Function NFE_ObterXml _
                Lib "ACBrNFE32.dll" (ByVal libHandler As Long, _
                                     ByVal AIndex As Long, _
                                     ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Private Declare Function NFE_GravarXml _
                Lib "ACBrNFE32.dll" (ByVal libHandler As Long, _
                                     ByVal AIndex As Long, _
                                     ByVal eNomeArquivo As String, _
                                     ByVal ePathArquivo As String) As Long
                                     
Private Declare Function NFE_ObterIni _
                Lib "ACBrNFE32.dll" (ByVal libHandler As Long, _
                                     ByVal AIndex As Long, _
                                     ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Private Declare Function NFE_GravarIni _
                Lib "ACBrNFE32.dll" (ByVal libHandler As Long, _
                                     ByVal AIndex As Long, _
                                     ByVal eNomeArquivo As String, _
                                     ByVal ePathArquivo As String) As Long
                
Private Declare Function NFE_CarregarEventoXML _
                Lib "ACBrNFE32.dll" (ByVal libHandler As Long, _
                                     ByVal eArquivoOuXml As String) As Long
                
Private Declare Function NFE_CarregarEventoINI _
                Lib "ACBrNFE32.dll" (ByVal libHandler As Long, _
                                     ByVal eArquivoOuIni As String) As Long
                
Private Declare Function NFE_LimparLista Lib "ACBrNFE32.dll" (ByVal libHandler As Long) As Long

Private Declare Function NFE_LimparListaEventos Lib "ACBrNFE32.dll" (ByVal libHandler As Long) As Long

Private Declare Function NFE_Assinar Lib "ACBrNFE32.dll" (ByVal libHandler As Long) As Long

Private Declare Function NFE_Validar Lib "ACBrNFE32.dll" (ByVal libHandler As Long) As Long

Private Declare Function NFE_ValidarRegrasdeNegocios _
                Lib "ACBrNFE32.dll" (ByVal libHandler As Long, _
                                     ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Private Declare Function NFE_VerificarAssinatura _
                Lib "ACBrNFE32.dll" (ByVal libHandler As Long, _
                                     ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Private Declare Function NFE_GerarChave _
                Lib "ACBrNFE32.dll" (ByVal libHandler As Long, _
                                     ByVal ACodigoUF As Long, _
                                     ByVal ACodigoNumerico As Long, _
                                     ByVal AModelo As Long, _
                                     ByVal ASerie As Long, _
                                     ByVal ANumero As Long, _
                                     ByVal ATpEmi As Long, _
                                     ByVal AEmissao As String, _
                                     ByVal ACNPJCPF As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                     
Private Declare Function NFE_ObterCertificados _
                Lib "ACBrNFE32.dll" (ByVal libHandler As Long, _
                                     ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Private Declare Function NFE_GetPath _
                Lib "ACBrNFE32.dll" (ByVal libHandler As Long, _
                                     ByVal tipo As Long, _
                                     ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Private Declare Function NFE_GetPathEvento _
                Lib "ACBrNFE32.dll" (ByVal libHandler As Long, _
                                     ByVal aCodEvento As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Private Declare Function NFE_StatusServico _
                Lib "ACBrNFE32.dll" (ByVal libHandler As Long, _
                                     ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Private Declare Function NFE_Consultar _
                Lib "ACBrNFE32.dll" (ByVal libHandler As Long, _
                                     ByVal eChaveOuNFe As String, _
                                     ByVal AExtrairEventos As Boolean, _
                                     ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Private Declare Function NFE_Inutilizar _
                Lib "ACBrNFE32.dll" (ByVal libHandler As Long, _
                                     ByVal acnpj As String, _
                                     ByVal aJustificativa As String, _
                                     ByVal ano As Long, _
                                     ByVal modelo As Long, _
                                     ByVal serie As Long, _
                                     ByVal numeroInicial As Long, _
                                     ByVal numeroFinal As Long, _
                                     ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Private Declare Function NFE_Enviar _
                Lib "ACBrNFE32.dll" (ByVal libHandler As Long, _
                                     ByVal aLote As Long, _
                                     ByVal Imprimir As Boolean, _
                                     ByVal sincrono As Boolean, _
                                     ByVal zipado As Boolean, _
                                     ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Private Declare Function NFE_ConsultarRecibo _
                Lib "ACBrNFE32.dll" (ByVal libHandler As Long, _
                                     ByVal aRecibo As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Private Declare Function NFE_ConsultaCadastro _
                Lib "ACBrNFE32.dll" (ByVal libHandler As Long, _
                                     ByVal cUF As String, _
                                     ByVal nDocumento As String, _
                                     ByVal nIE As Boolean, _
                                     ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Private Declare Function NFE_Cancelar _
                Lib "ACBrNFE32.dll" (ByVal libHandler As Long, _
                                     ByVal eChave As String, _
                                     ByVal eJustificativa As String, _
                                     ByVal eCNPJ As String, _
                                     ByVal aLote As Long, _
                                     ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Private Declare Function NFE_EnviarEvento _
                Lib "ACBrNFE32.dll" (ByVal libHandler As Long, _
                                     ByVal aLote As Long, _
                                     ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Private Declare Function NFE_DistribuicaoDFePorUltNSU _
                Lib "ACBrNFE32.dll" (ByVal libHandler As Long, _
                                     ByVal acUFAutor As Long, _
                                     ByVal eCnpjcpf As String, _
                                     ByVal eultNsu As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Private Declare Function NFE_DistribuicaoDFePorNSU _
                Lib "ACBrNFE32.dll" (ByVal libHandler As Long, _
                                     ByVal acUFAutor As Long, _
                                     ByVal eCnpjcpf As String, _
                                     ByVal eNsu As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Private Declare Function NFE_DistribuicaoDFePorChave _
                Lib "ACBrNFE32.dll" (ByVal libHandler As Long, _
                                     ByVal acUFAutor As Long, _
                                     ByVal eCnpjcpf As String, _
                                     ByVal echNFe As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Private Declare Function NFE_EnviarEmail _
                Lib "ACBrNFE32.dll" (ByVal libHandler As Long, _
                                     ByVal ePara As String, _
                                     ByVal eChaveNFe As String, _
                                     ByVal aEnviaPDF As Boolean, _
                                     ByVal eAssunto As String, _
                                     ByVal eCc As String, _
                                     ByVal eAnexos As String, _
                                     ByVal eMensagem As String) As Long
                                     
Private Declare Function NFE_EnviarEmailEvento _
                Lib "ACBrNFE32.dll" (ByVal libHandler As Long, _
                                     ByVal ePara As String, _
                                     ByVal eChaveNFe As String, _
                                     ByVal aEnviaPDF As Boolean, _
                                     ByVal eAssunto As String, _
                                     ByVal eCc As String, _
                                     ByVal eAnexos As String, _
                                     ByVal eMensagem As String) As Long
                                     
Private Declare Function NFE_Imprimir _
                Lib "ACBrNFE32.dll" (ByVal libHandler As Long, _
                                     ByVal cImpressora As String, _
                                     ByVal nNumCopias As Long, _
                                     ByVal cProtocolo As String, _
                                     ByVal bMostrarPreview As String, _
                                     ByVal cMarcaDagua As String, _
                                     ByVal bViaConsumidor As String, _
                                     ByVal bSimplificado As String) As Long
                                     
Private Declare Function NFE_ImprimirPDF Lib "ACBrNFE32.dll" (ByVal libHandler As Long) As Long

Private Declare Function NFE_ImprimirEvento _
                Lib "ACBrNFE32.dll" (ByVal libHandler As Long, _
                                     ByVal eArquivoXmlNFe As String, _
                                     ByVal eArquivoXmlEvento As String) As Long
                                     
Private Declare Function NFE_ImprimirEventoPDF _
                Lib "ACBrNFE32.dll" (ByVal libHandler As Long, _
                                     ByVal eArquivoXmlNFe As String, _
                                     ByVal eArquivoXmlEvento As String) As Long
                                     
Private Declare Function NFE_ImprimirInutilizacao _
                Lib "ACBrNFE32.dll" (ByVal libHandler As Long, _
                                     ByVal eArquivoXml As String) As Long
                
Private Declare Function NFE_ImprimirInutilizacaoPDF _
                Lib "ACBrNFE32.dll" (ByVal libHandler As Long, _
                                     ByVal eArquivoXml As String) As Long

Public Sub InicializarLib(Optional ByVal eArqConfig As String = "", _
                          Optional ByVal eChaveCrypt As String = "")

    SetLibPath
    Dim retorno As Long

    retorno = NFE_Inicializar(libHandler, eArqConfig, eChaveCrypt)
    CheckResult retorno
End Sub

Public Sub FinalizarLib()

    Dim retorno As Long

    retorno = NFE_Finalizar(libHandler)
    CheckResult retorno
End Sub

Public Sub ConfigImportar(ByVal eArqConfig As String)

    Dim retorno As Long

    retorno = NFE_ConfigImportar(libHandler, eArqConfig)
    CheckResult (retorno)
    
End Sub
    
Public Function ConfigExportar() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = NFE_ConfigExportar(libHandler, buffer, bufferLen)
    CheckResult retorno
    
    ConfigExportar = processResult(buffer, bufferLen)
    
End Function

Public Sub ConfigLer(Optional eArqConfig As String = "")

    Dim retorno As Long

    retorno = NFE_ConfigLer(libHandler, eArqConfig)
    CheckResult retorno
End Sub

Public Sub ConfigGravar(Optional eArqConfig As String = "")

    Dim retorno As Long

    retorno = NFE_ConfigGravar(libHandler, eArqConfig)
    CheckResult retorno
End Sub

Public Function ConfigLerValor(ByVal eSessao As String, ByVal eChave As String) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NFE_ConfigLerValor(libHandler, eSessao, eChave, buffer, bufferLen)
    CheckResult retorno
    
    ConfigLerValor = processResult(buffer, bufferLen)
End Function

Public Sub ConfigGravarValor(ByVal eSessao As String, _
                             ByVal eChave As String, _
                             ByVal valor As String)

    Dim retorno As Long
    
    retorno = NFE_ConfigGravarValor(libHandler, eSessao, eChave, valor)
    CheckResult retorno
End Sub

Public Function Nome() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
        
    retorno = NFE_Nome(libHandler, buffer, bufferLen)
    CheckResult retorno
    
    Nome = processResult(buffer, bufferLen)
End Function

Public Function Versao() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = NFE_Versao(libHandler, buffer, bufferLen)
    CheckResult retorno
    
    Versao = processResult(buffer, bufferLen)
End Function

Public Sub CarregarXML(ByVal eArquivoOuXml As String)
    Dim retorno   As Long
    retorno = NFE_CarregarXML(libHandler, eArquivoOuXml)
    CheckResult retorno
End Sub

Public Sub CarregarINI(ByVal eArquivoOuIni As String)
    Dim retorno   As Long
    retorno = NFE_CarregarINI(libHandler, eArquivoOuIni)
    CheckResult retorno
End Sub

Public Function ObterXml(ByVal AIndex As Long) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NFE_ObterXml(libHandler, AIndex, buffer, bufferLen)
    CheckResult retorno
    
    ObterXml = processResult(buffer, bufferLen)
End Function

Public Sub GravarXml(ByVal AIndex As Long, _
                     Optional ByVal eNomeArquivo As String = "", _
                     Optional ByVal ePathArquivo As String = "")
    Dim retorno   As Long
    retorno = NFE_GravarXml(libHandler, AIndex, eNomeArquivo, ePathArquivo)
    CheckResult retorno
End Sub

Public Function ObterIni(ByVal AIndex As Long) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NFE_ObterIni(libHandler, AIndex, buffer, bufferLen)
    CheckResult retorno
    
    ObterIni = processResult(buffer, bufferLen)
End Function

Public Sub GravarIni(ByVal AIndex As Long, _
                     ByVal eNomeArquivo As String, _
                     Optional ByVal ePathArquivo As String = "")
    Dim retorno   As Long
    retorno = NFE_GravarIni(libHandler, AIndex, eNomeArquivo, ePathArquivo)
    CheckResult retorno
End Sub

Public Sub CarregarEventoXML(ByVal eArquivoOuXml As String)
    Dim retorno   As Long
    retorno = NFE_CarregarEventoXML(libHandler, eArquivoOuXml)
    CheckResult retorno
End Sub

Public Sub CarregarEventoINI(ByVal eArquivoOuIni As String)
    Dim retorno   As Long
    retorno = NFE_CarregarEventoINI(libHandler, eArquivoOuIni)
    CheckResult retorno
End Sub

Public Sub LimparLista()
    Dim retorno   As Long
    retorno = NFE_LimparLista(libHandler)
    CheckResult retorno
End Sub

Public Sub LimparListaEventos()
    Dim retorno   As Long
    retorno = NFE_LimparListaEventos(libHandler)
    CheckResult retorno
End Sub

Public Sub Assinar()
    Dim retorno   As Long
    retorno = NFE_Assinar(libHandler)
    CheckResult retorno
End Sub

Public Sub Validar()
    Dim retorno   As Long
    retorno = NFE_Validar(libHandler)
    CheckResult retorno
End Sub

Public Function ValidarRegrasdeNegocios() As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NFE_ValidarRegrasdeNegocios(libHandler, buffer, bufferLen)
    CheckResult retorno
    
    ValidarRegrasdeNegocios = processResult(buffer, bufferLen)
End Function

Public Function VerificarAssinatura() As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NFE_VerificarAssinatura(libHandler, buffer, bufferLen)
    CheckResult retorno
    
    VerificarAssinatura = processResult(buffer, bufferLen)
End Function

Public Function GerarChave(ByVal ACodigoUF As Long, ByVal ACodigoNumerico As Long, _
                           ByVal AModelo As Long, ByVal ASerie As Long, ByVal ANumero As Long, _
                           ByVal ATpEmi As Long, ByVal AEmissao As Date, ByVal ACNPJCPF As String) As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NFE_GerarChave(libHandler, ACodigoUF, ACodigoNumerico, AModelo, ASerie, ANumero, ATpEmi, _
                             Format(AEmissao, "dd/MM/yyyy"), ACNPJCPF, buffer, bufferLen)
    CheckResult retorno
    
    GerarChave = processResult(buffer, bufferLen)
End Function

Public Function ObterCertificados() As String()
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NFE_ObterCertificados(libHandler, buffer, bufferLen)
    CheckResult retorno
    
    ObterCertificados = Split(processResult(buffer, bufferLen), vbCrLf)
End Function

Public Function GetPath(ByVal tipo As Long) As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NFE_GetPath(libHandler, tipo, buffer, bufferLen)
    CheckResult retorno
    
    GetPath = processResult(buffer, bufferLen)
End Function

Public Function GetPathEvento(ByVal aCodEvento As String) As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NFE_GetPathEvento(libHandler, aCodEvento, buffer, bufferLen)
    CheckResult retorno
    
    GetPathEvento = processResult(buffer, bufferLen)
End Function

Public Function StatusServico() As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NFE_StatusServico(libHandler, buffer, bufferLen)
    CheckResult retorno
    
    StatusServico = processResult(buffer, bufferLen)
End Function

Public Function Consultar(ByVal eChaveOuNFe As String, Optional ByVal AExtrairEventos As Boolean = False) As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NFE_Consultar(libHandler, eChaveOuNFe, AExtrairEventos, buffer, bufferLen)
    CheckResult retorno
    
    Consultar = processResult(buffer, bufferLen)
End Function

Public Function ConsultaCadastro(ByVal cUF As String, ByVal nDocumento As String, ByVal nIE As Boolean) As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NFE_ConsultaCadastro(libHandler, cUF, nDocumento, nIE, buffer, bufferLen)
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
    
    retorno = NFE_Inutilizar(libHandler, acnpj, aJustificativa, ano, modelo, serie, numeroInicial, numeroFinal, buffer, bufferLen)
    CheckResult retorno
    
    Inutilizar = processResult(buffer, bufferLen)
End Function

Public Function Enviar(ByVal aLote As Long, Optional ByVal Imprimir As Boolean = False, _
                       Optional ByVal sincrono As Boolean = False, _
                       Optional ByVal zipado As Boolean = False) As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NFE_Enviar(libHandler, aLote, Imprimir, sincrono, zipado, buffer, bufferLen)
    CheckResult retorno
    
    Enviar = processResult(buffer, bufferLen)
End Function

Public Function ConsultarRecibo(ByVal aRecibo As String) As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NFE_ConsultarRecibo(libHandler, aRecibo, buffer, bufferLen)
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
    
    retorno = NFE_Cancelar(libHandler, eChave, eJustificativa, eCNPJ, aLote, buffer, bufferLen)
    CheckResult retorno
    
    Cancelar = processResult(buffer, bufferLen)
End Function

Public Function EnviarEvento(ByVal aLote As Long) As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NFE_EnviarEvento(libHandler, aLote, buffer, bufferLen)
    CheckResult retorno
    
    EnviarEvento = processResult(buffer, bufferLen)
End Function

Public Function DistribuicaoDFePorUltNSU(ByVal acUFAutor As String, ByVal eCnpjcpf As String, ByVal eultNsu As String) As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NFE_DistribuicaoDFePorUltNSU(libHandler, acUFAutor, eCnpjcpf, eultNsu, buffer, bufferLen)
    CheckResult retorno
    
    DistribuicaoDFePorUltNSU = processResult(buffer, bufferLen)
End Function

Public Function DistribuicaoDFePorNSU(ByVal acUFAutor As String, ByVal eCnpjcpf As String, ByVal eNsu As String) As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NFE_DistribuicaoDFePorNSU(libHandler, acUFAutor, eCnpjcpf, eNsu, buffer, bufferLen)
    CheckResult retorno
    
    DistribuicaoDFePorNSU = processResult(buffer, bufferLen)
End Function

Public Function DistribuicaoDFePorChave(ByVal acUFAutor As String, ByVal eCnpjcpf As String, ByVal echNFe As String) As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NFE_DistribuicaoDFePorChave(libHandler, acUFAutor, eCnpjcpf, echNFe, buffer, bufferLen)
    CheckResult retorno
    
    DistribuicaoDFePorChave = processResult(buffer, bufferLen)
End Function

Public Sub EnviarEmail(ByVal ePara As String, ByVal eArquivoOuXml As String, ByVal aEnviaPDF As Boolean, _
                       ByVal eAssunto As String, ByVal eMensagem As String, _
                       Optional ByVal eCc As String = "", Optional ByVal eAnexos As String = "")
    Dim retorno   As Long
    
    retorno = NFE_EnviarEmail(libHandler, ePara, eArquivoOuXml, aEnviaPDF, eAssunto, eCc, eAnexos, eMensagem)
    CheckResult retorno
End Sub

Public Sub EnviarEmailEvento(ByVal ePara As String, ByVal eChaveNFe As String, ByVal aEnviaPDF As Boolean, _
                       ByVal eAssunto As String, ByVal eMensagem As String, _
                       Optional ByVal eCc As String = "", Optional ByVal eAnexos As String = "")
    Dim retorno   As Long
    
    retorno = NFE_EnviarEmailEvento(libHandler, ePara, eChaveNFe, aEnviaPDF, eAssunto, eCc, eAnexos, eMensagem)
    CheckResult retorno
End Sub

Public Sub Imprimir(Optional ByVal cImpressora As String = "", Optional ByVal nNumCopias As Long = 1, _
                    Optional ByVal cProtocolo As String = "", Optional ByVal bMostrarPreview As String = "", _
                    Optional ByVal cMarcaDagua As String = "", Optional ByVal bViaConsumidor As String = "", _
                    Optional ByVal bSimplificado As String = "")
    Dim retorno   As Long
    
    retorno = NFE_Imprimir(libHandler, cImpressora, nNumCopias, cProtocolo, bMostrarPreview, cMarcaDagua, bViaConsumidor, bSimplificado)
    CheckResult retorno
End Sub

Public Sub ImprimirPDF()
    Dim retorno   As Long
    
    retorno = NFE_ImprimirPDF(libHandler)
    CheckResult retorno
End Sub

Public Sub ImprimirEvento(ByVal eArquivoXmlNFe As String, ByVal eArquivoXmlEvento As String)
    Dim retorno   As Long
    
    retorno = NFE_ImprimirEvento(libHandler, eArquivoXmlNFe, eArquivoXmlEvento)
    CheckResult retorno
End Sub

Public Sub ImprimirEventoPDF(ByVal eArquivoXmlNFe As String, ByVal eArquivoXmlEvento As String)
    Dim retorno   As Long
   
    retorno = NFE_ImprimirEventoPDF(libHandler, eArquivoXmlNFe, eArquivoXmlEvento)
    CheckResult retorno
End Sub

Public Sub ImprimirInutilizacao(ByVal eArquivoXml As String)
    Dim retorno   As Long
       
    retorno = NFE_ImprimirInutilizacao(libHandler, eArquivoXml)
    CheckResult retorno
End Sub

Public Sub ImprimirInutilizacaoPDF(ByVal eArquivoXml As String)
    Dim retorno   As Long
    
    retorno = NFE_ImprimirInutilizacaoPDF(libHandler, eArquivoXml)
    CheckResult retorno
End Sub

Private Sub CheckResult(ByVal Resultado As Long)
    
    If Resultado >= 0 Then Exit Sub
         
    Dim buffer As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    NFE_UltimoRetorno libHandler, buffer, bufferLen
    
    Err.Raise Resultado, "ACBrNFe", processResult(buffer, bufferLen)
End Sub

Private Function processResult(ByRef buffer As String, ByRef bufferLen As Long) As String
    
    If bufferLen > BUFFER_LENGTH Then
        buffer = String$(bufferLen, " ")
        NFE_UltimoRetorno libHandler, buffer, bufferLen
    End If

    processResult = Trim$(FromUTF8(buffer))
End Function
