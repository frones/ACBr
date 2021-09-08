VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ACBrSedex"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Const BUFFER_LENGTH As Long = 1024

Private Declare Function Sedex_Inicializar _
                Lib "ACBrSedex32.dll" (ByVal eArqConfig As String, _
                                     ByVal eChaveCrypt As String) As Long

Private Declare Function Sedex_Finalizar Lib "ACBrSedex32.dll" () As Long


Private Declare Function Sedex_Nome _
                Lib "ACBrSedex32.dll" (ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                     
Private Declare Function Sedex_Versao _
                Lib "ACBrSedex32.dll" (ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                                      
Private Declare Function Sedex_UltimoRetorno _
                Lib "ACBrSedex32.dll" (ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                                     
Private Declare Function Sedex_ConfigImportar _
                Lib "ACBrSedex32.dll" (ByVal eArqConfig As String) As Long
                   
Private Declare Function Sedex_ConfigExportar _
                Lib "ACBrSedex32.dll" (ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                     
Private Declare Function Sedex_ConfigLer _
                Lib "ACBrSedex32.dll" (ByVal eArqConfig As String) As Long

Private Declare Function Sedex_ConfigGravar _
                Lib "ACBrSedex32.dll" (ByVal eArqConfig As String) As Long
                   
Private Declare Function Sedex_ConfigLerValor _
                Lib "ACBrSedex32.dll" (ByVal eSessao As String, _
                                      ByVal eChave As String, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long

Private Declare Function Sedex_ConfigGravarValor _
                Lib "ACBrSedex32.dll" (ByVal eSessao As String, _
                                      ByVal eChave As String, _
                                      ByVal valor As String) As Long

Private Declare Function Sedex_Consultar _
                Lib "ACBrSedex32.dll" (ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long

Private Declare Function Sedex_Rastrear _
                Lib "ACBrSedex32.dll" (ByVal eCodRastreio As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                      
Public Sub InicializarLib(Optional ByVal eArqConfig As String = "", _
                          Optional ByVal eChaveCrypt As String = "")
                          
    SetLibPath
    
    Dim retorno As Long

    retorno = Sedex_Inicializar(eArqConfig, eChaveCrypt)
    CheckResult (retorno)
    
End Sub

Public Sub FinalizarLib()

    Dim retorno As Long

    retorno = Sedex_Finalizar
    CheckResult (retorno)
    
End Sub

Public Sub ConfigImportar(ByVal eArqConfig As String)

    Dim retorno As Long

    retorno = Sedex_ConfigImportar(eArqConfig)
    CheckResult (retorno)
    
End Sub

Public Function ConfigExportar() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = Sedex_ConfigExportar(buffer, bufferLen)
    CheckResult retorno
    
    ConfigExportar = processResult(buffer, bufferLen)
    
End Function

Public Sub ConfigLer(Optional eArqConfig As String = "")

    Dim retorno As Long

    retorno = Sedex_ConfigLer(eArqConfig)
    CheckResult (retorno)
    
End Sub

Public Sub ConfigGravar(Optional eArqConfig As String = "")

    Dim retorno As Long

    retorno = Sedex_ConfigGravar(eArqConfig)
    CheckResult (retorno)
    
End Sub

Public Function ConfigLerValor(ByVal eSessao As String, ByVal eChave As String) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = Sedex_ConfigLerValor(eSessao, eChave, buffer, bufferLen)
    CheckResult retorno
    
    ConfigLerValor = processResult(buffer, bufferLen)
End Function

Public Sub ConfigGravarValor(ByVal eSessao As String, _
                             ByVal eChave As String, _
                             ByVal valor As String)

    Dim retorno As Long
    
    retorno = Sedex_ConfigGravarValor(eSessao, eChave, valor)
    CheckResult retorno
End Sub

Public Function Nome() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
        
    retorno = Sedex_Nome(buffer, bufferLen)
    CheckResult (retorno)
    
    Nome = processResult(buffer, bufferLen)
    
End Function

Public Function Versao() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = Sedex_Versao(buffer, bufferLen)
    CheckResult (retorno)
    
    Versao = processResult(buffer, bufferLen)
    
End Function

Public Function Consultar() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = Sedex_Consultar(buffer, bufferLen)
    CheckResult (retorno)
    
    Consultar = processResult(buffer, bufferLen)

End Function

Public Function Rastrear(ByVal eCodRastreio As String) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = Sedex_Rastrear(eCodRastreio, buffer, bufferLen)
    CheckResult (retorno)
    
    Rastrear = processResult(buffer, bufferLen)

End Function

Private Sub CheckResult(ByVal Resultado As Long)
    
    If Resultado >= 0 Then Exit Sub
         
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    Sedex_UltimoRetorno buffer, bufferLen
    
    Err.Raise Resultado, "ACBrSedex", processResult(buffer, bufferLen)
End Sub

Private Function processResult(ByRef buffer As String, ByRef bufferLen As Long) As String
    
    If bufferLen > BUFFER_LENGTH Then
        buffer = String$(bufferLen, " ")
        Sedex_UltimoRetorno buffer, bufferLen
    End If

    processResult = Trim$(FromUTF8(buffer))
End Function


