VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ACBrGNRe"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Const BUFFER_LENGTH As Long = 1024
                               
Private Declare Function GNRE_Inicializar _
                Lib "ACBrGNRe32.dll" (ByVal eArqConfig As String, _
                                      ByVal eChaveCrypt As String) As Long
                   
Private Declare Function GNRE_Finalizar Lib "ACBrGNRe32.dll" () As Long

Private Declare Function GNRE_Nome _
                Lib "ACBrGNRe32.dll" (ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                   
Private Declare Function GNRE_Versao _
                Lib "ACBrGNRe32.dll" (ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long

Private Declare Function GNRE_UltimoRetorno _
                Lib "ACBrGNRe32.dll" (ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                                      
Private Declare Function GNRE_ConfigImportar _
                Lib "ACBrGNRe32.dll" (ByVal eArqConfig As String) As Long
                   
Private Declare Function GNRE_ConfigExportar _
                Lib "ACBrGNRe32.dll" (ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                      
Private Declare Function GNRE_ConfigLer _
                Lib "ACBrGNRe32.dll" (ByVal eArqConfig As String) As Long

Private Declare Function GNRE_ConfigGravar _
                Lib "ACBrGNRe32.dll" (ByVal eArqConfig As String) As Long
                   
Private Declare Function GNRE_ConfigLerValor _
                Lib "ACBrGNRe32.dll" (ByVal eSessao As String, _
                                      ByVal eChave As String, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long

Private Declare Function GNRE_ConfigGravarValor _
                Lib "ACBrGNRe32.dll" (ByVal eSessao As String, _
                                      ByVal eChave As String, _
                                      ByVal valor As String) As Long
                                      
Private Declare Function GNRE_LimparLista Lib "ACBrGNRe32.dll" () As Long
                                      
Private Declare Function GNRE_CarregarXML _
                Lib "ACBrGNRe32.dll" (ByVal eArquivoOuXml As String) As Long
                
Private Declare Function GNRE_CarregarINI _
                Lib "ACBrGNRe32.dll" (ByVal eArquivoOuIni As String) As Long
                
Private Declare Function GNRE_ObterXml _
                Lib "ACBrGNRe32.dll" (ByVal AIndex As Long, _
                                     ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Private Declare Function GNRE_GravarXml _
                Lib "ACBrGNRe32.dll" (ByVal AIndex As Long, _
                                     ByVal eNomeArquivo As String, _
                                     ByVal ePathArquivo As String) As Long
                
Private Declare Function GNRE_LimparListaGuiaRetorno Lib "ACBrGNRe32.dll" () As Long

Private Declare Function GNRE_CarregarGuiaRetorno _
                Lib "ACBrGNRe32.dll" (ByVal eArquivoOuXml As String) As Long
                
Private Declare Function GNRE_Assinar Lib "ACBrGNRe32.dll" () As Long

Private Declare Function GNRE_Validar Lib "ACBrGNRe32.dll" () As Long

Private Declare Function GNRE_VerificarAssinatura _
                Lib "ACBrGNRe32.dll" (ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Private Declare Function GNRE_ObterCertificados _
                Lib "ACBrGNRe32.dll" (ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                
Private Declare Function GNRE_Enviar _
                Lib "ACBrGNRe32.dll" (ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                                      
Private Declare Function GNRE_Consultar _
                Lib "ACBrGNRe32.dll" (ByVal eUF As String, _
                                      ByVal AReceita As Long, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                                      
Private Declare Function GNRE_EnviarEmail _
                Lib "ACBrGNRe32.dll" (ByVal ePara As String, _
                                      ByVal eArquivoOuXml As String, _
                                      ByVal aEnviaPDF As Boolean, _
                                      ByVal eAssunto As String, _
                                      ByVal eCc As String, _
                                      ByVal eAnexos As String, _
                                      ByRef eMensagem As String) As Long
                                      
Private Declare Function GNRE_Imprimir _
                Lib "ACBrGNRe32.dll" (ByVal eNomeImpressora As String, _
                                      ByVal eMostrarPreview As String) As Long
                                      
Private Declare Function GNRE_ImprimirPDF Lib "ACBrGNRe32.dll" () As Long

Public Sub InicializarLib(Optional ByVal eArqConfig As String = "", _
                          Optional ByVal eChaveCrypt As String = "")

    SetLibPath
    Dim retorno As Long

    retorno = GNRE_Inicializar(eArqConfig, eChaveCrypt)
    CheckResult retorno
End Sub

Public Sub FinalizarLib()

    Dim retorno As Long

    retorno = GNRE_Finalizar
    CheckResult retorno
End Sub

Public Sub ConfigImportar(ByVal eArqConfig As String)

    Dim retorno As Long

    retorno = GNRE_ConfigImportar(eArqConfig)
    CheckResult (retorno)
    
End Sub
    
Public Function ConfigExportar() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = GNRE_ConfigExportar(buffer, bufferLen)
    CheckResult retorno
    
    ConfigExportar = processResult(buffer, bufferLen)
    
End Function

Public Sub ConfigLer(Optional eArqConfig As String = "")

    Dim retorno As Long

    retorno = GNRE_ConfigLer(eArqConfig)
    CheckResult retorno
End Sub

Public Sub ConfigGravar(Optional eArqConfig As String = "")

    Dim retorno As Long

    retorno = GNRE_ConfigGravar(eArqConfig)
    CheckResult retorno
End Sub

Public Function ConfigLerValor(ByVal eSessao As String, ByVal eChave As String) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = GNRE_ConfigLerValor(eSessao, eChave, buffer, bufferLen)
    CheckResult retorno
    
    ConfigLerValor = processResult(buffer, bufferLen)
End Function

Public Sub ConfigGravarValor(ByVal eSessao As String, _
                             ByVal eChave As String, _
                             ByVal valor As String)

    Dim retorno As Long
    
    retorno = GNRE_ConfigGravarValor(eSessao, eChave, valor)
    CheckResult retorno
End Sub

Public Function Nome() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
        
    retorno = GNRE_Nome(buffer, bufferLen)
    CheckResult retorno
    
    Nome = processResult(buffer, bufferLen)
End Function

Public Function Versao() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = GNRE_Versao(buffer, bufferLen)
    CheckResult retorno
    
    Versao = processResult(buffer, bufferLen)
End Function

Public Sub LimparLista()
    Dim retorno   As Long
    retorno = GNRE_LimparLista
    CheckResult retorno
End Sub

Public Sub CarregarXML(ByVal eArquivoOuXml As String)
    Dim retorno   As Long
    retorno = GNRE_CarregarXML(eArquivoOuXml)
    CheckResult retorno
End Sub

Public Sub CarregarINI(ByVal eArquivoOuIni As String)
    Dim retorno   As Long
    retorno = GNRE_CarregarINI(eArquivoOuIni)
    CheckResult retorno
End Sub

Public Function ObterXml(ByVal AIndex As Long) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = GNRE_ObterXml(AIndex, buffer, bufferLen)
    CheckResult retorno
    
    ObterXml = processResult(buffer, bufferLen)
End Function

Public Sub GravarXml(ByVal AIndex As Long, _
                     Optional ByVal eNomeArquivo As String = "", _
                     Optional ByVal ePathArquivo As String = "")
    Dim retorno   As Long
    retorno = GNRE_GravarXml(AIndex, eNomeArquivo, ePathArquivo)
    CheckResult retorno
End Sub

Public Sub LimparListaGuiaRetorno()
    Dim retorno   As Long
    retorno = GNRE_LimparListaGuiaRetorno
    CheckResult retorno
End Sub

Public Sub CarregarGuiaRetorno(ByVal eArquivoOuXml As String)
    Dim retorno   As Long
    retorno = GNRE_CarregarGuiaRetorno(eArquivoOuXml)
    CheckResult retorno
End Sub

Public Sub Assinar()
    Dim retorno   As Long
    retorno = GNRE_Assinar
    CheckResult retorno
End Sub

Public Sub Validar()
    Dim retorno   As Long
    retorno = GNRE_Validar
    CheckResult retorno
End Sub

Public Function VerificarAssinatura() As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = GNRE_VerificarAssinatura(buffer, bufferLen)
    CheckResult retorno
    
    VerificarAssinatura = processResult(buffer, bufferLen)
End Function

Public Function ObterCertificados() As String()
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = GNRE_ObterCertificados(buffer, bufferLen)
    CheckResult retorno
    
    ObterCertificados = Split(processResult(buffer, bufferLen), vbCrLf)
End Function

Public Function Enviar() As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = GNRE_Enviar(buffer, bufferLen)
    CheckResult retorno
    
    Enviar = processResult(buffer, bufferLen)
End Function

Public Function Consultar(ByVal eUF As String, ByVal AReceita As Long) As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = GNRE_Consultar(eUF, AReceita, buffer, bufferLen)
    CheckResult retorno
    
    Consultar = processResult(buffer, bufferLen)
End Function

Public Sub EnviarEmail(ByVal ePara As String, ByVal eArquivoOuXml As String, ByVal aEnviaPDF As Boolean, _
                       ByVal eAssunto As String, ByVal eMensagem As String, _
                       Optional ByVal eCc As String = "", Optional ByVal eAnexos As String = "")
    Dim retorno   As Long
    
    retorno = GNRE_EnviarEmail(ePara, eArquivoOuXml, aEnviaPDF, eAssunto, eCc, eAnexos, eMensagem)
    CheckResult retorno
End Sub

Public Sub Imprimir(Optional ByVal eNomeImpressora As String = "", Optional ByVal eMostrarPreview As String = "")
    Dim retorno   As Long
    
    retorno = GNRE_Imprimir(eNomeImpressora, eMostrarPreview)
    CheckResult retorno
End Sub

Public Sub ImprimirPDF()
    Dim retorno   As Long
    
    retorno = GNRE_ImprimirPDF()
    CheckResult retorno
End Sub

Private Sub CheckResult(ByVal Resultado As Long)
    
    If Resultado >= 0 Then Exit Sub
         
    Dim buffer As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    GNRE_UltimoRetorno buffer, bufferLen
    
    Err.Raise Resultado, "ACBrGNRe", processResult(buffer, bufferLen)
End Sub

Private Function processResult(ByRef buffer As String, ByRef bufferLen As Long) As String
    
    If bufferLen > BUFFER_LENGTH Then
        buffer = String$(bufferLen, " ")
        GNRE_UltimoRetorno buffer, bufferLen
    End If

    processResult = Trim$(FromUTF8(buffer))
End Function
