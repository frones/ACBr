VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ACBrBAL"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Const BUFFER_LENGTH As Long = 1024
                                
Private Declare Function BAL_Inicializar _
                Lib "ACBrBAL32.dll" (ByVal eArqConfig As String, _
                                     ByVal eChaveCrypt As String) As Long
                   
Private Declare Function BAL_Finalizar Lib "ACBrBAL32.dll" () As Long

Private Declare Function BAL_Nome _
                Lib "ACBrBAL32.dll" (ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                   
Private Declare Function BAL_Versao _
                Lib "ACBrBAL32.dll" (ByVal buffer As String, _
                                            ByRef bufferLen As Long) As Long

Private Declare Function BAL_UltimoRetorno _
                Lib "ACBrBAL32.dll" (ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                   
Private Declare Function BAL_ConfigLer _
                Lib "ACBrBAL32.dll" (ByVal eArqConfig As String) As Long

Private Declare Function BAL_ConfigGravar _
                Lib "ACBrBAL32.dll" (ByVal eArqConfig As String) As Long
                   
Private Declare Function BAL_ConfigLerValor _
                Lib "ACBrBAL32.dll" (ByVal eSessao As String, _
                                     ByVal eChave As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long

Private Declare Function BAL_ConfigGravarValor _
                Lib "ACBrBAL32.dll" (ByVal eSessao As String, _
                                     ByVal eChave As String, _
                                     ByVal valor As String) As Long
                                     
Private Declare Function BAL_Ativar Lib "ACBrBAL32.dll" () As Long

Private Declare Function BAL_Desativar Lib "ACBrBAL32.dll" () As Long

Private Declare Function BAL_LePeso _
                Lib "ACBrBAL32.dll" (ByVal MillisecTimeOut As Long, _
                                     ByRef peso As Double) As Long
                                     
Private Declare Function BAL_SolicitarPeso Lib "ACBrBAL32.dll" () As Long

Private Declare Function BAL_UltimoPesoLido _
                Lib "ACBrBAL32.dll" (ByRef peso As Double) As Long
                
Private Declare Function BAL_InterpretarRespostaPeso _
                Lib "ACBrBAL32.dll" (ByVal eResposta As String, _
                                     ByRef peso As Double) As Long
                                     
Public Sub InicializarLib(Optional ByVal eArqConfig As String = "", _
                          Optional ByVal eChaveCrypt As String = "")

    SetLibPath
    
    Dim retorno As Long

    retorno = BAL_Inicializar(eArqConfig, eChaveCrypt)
    CheckResult retorno
End Sub

Public Sub FinalizarLib()

    Dim retorno As Long

    retorno = BAL_Finalizar
    CheckResult retorno
End Sub

Public Sub ConfigLer(Optional eArqConfig As String = "")

    Dim retorno As Long

    retorno = BAL_ConfigLer(eArqConfig)
    CheckResult retorno
End Sub

Public Sub ConfigGravar(Optional eArqConfig As String = "")

    Dim retorno As Long

    retorno = BAL_ConfigGravar(eArqConfig)
    CheckResult retorno
End Sub

Public Function ConfigLerValor(ByVal eSessao As String, ByVal eChave As String) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = BAL_ConfigLerValor(eSessao, eChave, buffer, bufferLen)
    CheckResult retorno
    
    ConfigLerValor = processResult(buffer, bufferLen)
End Function

Public Sub ConfigGravarValor(ByVal eSessao As String, _
                             ByVal eChave As String, _
                             ByVal valor As String)

    Dim retorno As Long
    
    retorno = BAL_ConfigGravarValor(eSessao, eChave, valor)
    CheckResult retorno
End Sub

Public Function Nome() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
        
    retorno = BAL_Nome(buffer, bufferLen)
    CheckResult retorno
    
    Nome = processResult(buffer, bufferLen)
End Function

Public Function Versao() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = BAL_Versao(buffer, bufferLen)
    CheckResult retorno
    
    Versao = processResult(buffer, bufferLen)
End Function

Public Sub Ativar()
    Dim retorno   As Long
    
    retorno = BAL_Ativar
    CheckResult retorno
End Sub

Public Sub Desativar()
    Dim retorno   As Long
    
    retorno = BAL_Desativar
    CheckResult retorno
End Sub

Public Function LePeso(Optional ByVal MillisecTimeOut As Long = 1000) As Double
    Dim retorno As Long
    Dim peso    As Double
    
    retorno = BAL_LePeso(MillisecTimeOut, peso)
    CheckResult retorno
    
    LePeso = peso
End Function

Public Sub SolicitarPeso()
    Dim retorno   As Long
    
    retorno = BAL_SolicitarPeso
    CheckResult retorno
End Sub

Public Function UltimoPesoLido() As Double
    Dim retorno As Long
    Dim peso    As Double
    
    retorno = BAL_UltimoPesoLido(peso)
    CheckResult retorno
    
    UltimoPesoLido = peso
End Function
                                     
Private Sub CheckResult(ByVal Resultado As Long)
    
    If Resultado >= 0 Then Exit Sub
         
    Dim buffer As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    BAL_UltimoRetorno buffer, bufferLen
    
    Err.Raise Resultado, "ACBrBAL", processResult(buffer, bufferLen)
End Sub

Private Function processResult(ByRef buffer As String, ByRef bufferLen As Long) As String
    
    If bufferLen > BUFFER_LENGTH Then
        buffer = String$(bufferLen, " ")
        BAL_UltimoRetorno buffer, bufferLen
    End If

    processResult = Trim$(FromUTF8(buffer))
End Function
