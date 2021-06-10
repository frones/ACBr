VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ACBrCEP"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Const BUFFER_LENGTH As Long = 1024

Private Declare Function CEP_Inicializar _
                Lib "ACBrCEP32.dll" (ByVal eArqConfig As String, _
                                     ByVal eChaveCrypt As String) As Long

Private Declare Function CEP_Finalizar Lib "ACBrCEP32.dll" () As Long


Private Declare Function CEP_Nome _
                Lib "ACBrCEP32.dll" (ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                     
Private Declare Function CEP_Versao _
                Lib "ACBrCEP32.dll" (ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                                      
Private Declare Function CEP_UltimoRetorno _
                Lib "ACBrCEP32.dll" (ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                                     
Private Declare Function CEP_ConfigImportar _
                Lib "ACBrCEP32.dll" (ByVal eArqConfig As String) As Long
                   
Private Declare Function CEP_ConfigExportar _
                Lib "ACBrCEP32.dll" (ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                     
Private Declare Function CEP_ConfigLer _
                Lib "ACBrCEP32.dll" (ByVal eArqConfig As String) As Long

Private Declare Function CEP_ConfigGravar _
                Lib "ACBrCEP32.dll" (ByVal eArqConfig As String) As Long
                   
Private Declare Function CEP_ConfigLerValor _
                Lib "ACBrCEP32.dll" (ByVal eSessao As String, _
                                      ByVal eChave As String, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long

Private Declare Function CEP_ConfigGravarValor _
                Lib "ACBrCEP32.dll" (ByVal eSessao As String, _
                                      ByVal eChave As String, _
                                      ByVal valor As String) As Long

Private Declare Function CEP_BuscarPorCEP _
                Lib "ACBrCEP32.dll" (ByVal eCEP As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long

Private Declare Function CEP_BuscarPorLogradouro _
                Lib "ACBrCEP32.dll" (ByVal eCidade As String, _
                                     ByVal eTipo_Logradouro As String, _
                                     ByVal eLogradouro As String, _
                                     ByVal eUF As String, _
                                     ByVal eBairro As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                      
Public Sub InicializarLib(Optional ByVal eArqConfig As String = "", _
                          Optional ByVal eChaveCrypt As String = "")
                          
    SetLibPath
    
    Dim retorno As Long

    retorno = CEP_Inicializar(eArqConfig, eChaveCrypt)
    CheckResult (retorno)
    
End Sub

Public Sub FinalizarLib()

    Dim retorno As Long

    retorno = CEP_Finalizar
    CheckResult (retorno)
    
End Sub

Public Sub ConfigImportar(ByVal eArqConfig As String)

    Dim retorno As Long

    retorno = CEP_ConfigImportar(eArqConfig)
    CheckResult (retorno)
    
End Sub

Public Function ConfigExportar() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = CEP_ConfigExportar(buffer, bufferLen)
    CheckResult retorno
    
    ConfigExportar = processResult(buffer, bufferLen)
    
End Function

Public Sub ConfigLer(Optional eArqConfig As String = "")

    Dim retorno As Long

    retorno = CEP_ConfigLer(eArqConfig)
    CheckResult (retorno)
    
End Sub

Public Sub ConfigGravar(Optional eArqConfig As String = "")

    Dim retorno As Long

    retorno = CEP_ConfigGravar(eArqConfig)
    CheckResult (retorno)
    
End Sub

Public Function ConfigLerValor(ByVal eSessao As String, ByVal eChave As String) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = CEP_ConfigLerValor(eSessao, eChave, buffer, bufferLen)
    CheckResult retorno
    
    ConfigLerValor = processResult(buffer, bufferLen)
End Function

Public Sub ConfigGravarValor(ByVal eSessao As String, _
                             ByVal eChave As String, _
                             ByVal valor As String)

    Dim retorno As Long
    
    retorno = CEP_ConfigGravarValor(eSessao, eChave, valor)
    CheckResult retorno
End Sub

Public Function Nome() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
        
    retorno = CEP_Nome(buffer, bufferLen)
    CheckResult (retorno)
    
    Nome = processResult(buffer, bufferLen)
    
End Function

Public Function Versao() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = CEP_Versao(buffer, bufferLen)
    CheckResult (retorno)
    
    Versao = processResult(buffer, bufferLen)
    
End Function

Public Function BuscarPorCEP(ByVal cep As String) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = CEP_BuscarPorCEP(cep, buffer, bufferLen)
    CheckResult (retorno)
    
    BuscarPorCEP = processResult(buffer, bufferLen)

End Function

Public Function BuscarPorLogradouro(ByVal cidade As String, ByVal tipo_Logradouro As String, ByVal logradouro As String, ByVal UF As String, ByVal bairro As String) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = CEP_BuscarPorLogradouro(cidade, tipo_Logradouro, logradouro, UF, bairro, buffer, bufferLen)
    CheckResult (retorno)
    
    BuscarPorLogradouro = processResult(buffer, bufferLen)

End Function

Private Sub CheckResult(ByVal Resultado As Long)
    
    If Resultado = 0 Then Exit Sub
         
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    CEP_UltimoRetorno buffer, bufferLen
    
    Err.Raise Resultado, "ACBrCEP", processResult(buffer, bufferLen)
End Sub

Private Function processResult(ByRef buffer As String, ByRef bufferLen As Long) As String
    
    If bufferLen > BUFFER_LENGTH Then
        buffer = String$(bufferLen, " ")
        CEP_UltimoRetorno buffer, bufferLen
    End If

    processResult = Trim$(FromUTF8(buffer))
End Function


