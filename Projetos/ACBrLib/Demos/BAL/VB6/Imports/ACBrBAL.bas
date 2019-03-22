Attribute VB_Name = "ACBrBAL"
Option Explicit

' UTF-8 Code Page'Sys call to convert multiple byte chars to a charPrivate
Const CP_UTF8 As Long = 65001

Private Declare Function MultiByteToWideChar _
                Lib "kernel32" (ByVal CodePage As Long, _
                                ByVal dwFlags As Long, _
                                ByVal lpMultiByteStr As Long, _
                                ByVal cchMultiByte As Long, _
                                ByVal lpWideCharStr As Long, _
                                ByVal cchWideChar As Long) As Long
                                
Public Declare Function BAL_Inicializar _
                Lib "ACBrBAL32.dll" (ByVal eArqConfig As String, _
                                     ByVal eChaveCrypt As String) As Long
                   
Public Declare Function BAL_Finalizar Lib "ACBrBAL32.dll" () As Long

Public Declare Function BAL_Nome _
                Lib "ACBrBAL32.dll" (ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                   
Public Declare Function BAL_Versao _
                Lib "ACBrBAL32.dll" (ByVal buffer As String, _
                                            ByRef bufferLen As Long) As Long

Public Declare Function BAL_UltimoRetorno _
                Lib "ACBrBAL32.dll" (ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                   
Public Declare Function BAL_ConfigLer _
                Lib "ACBrBAL32.dll" (ByVal eArqConfig As String) As Long

Public Declare Function BAL_ConfigGravar _
                Lib "ACBrBAL32.dll" (ByVal eArqConfig As String) As Long
                   
Public Declare Function BAL_ConfigLerValor _
                Lib "ACBrBAL32.dll" (ByVal eSessao As String, _
                                     ByVal eChave As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long

Public Declare Function BAL_ConfigGravarValor _
                Lib "ACBrBAL32.dll" (ByVal eSessao As String, _
                                     ByVal eChave As String, _
                                     ByVal valor As String) As Long
                                     
Public Declare Function BAL_Ativar Lib "ACBrBAL32.dll" () As Long

Public Declare Function BAL_Desativar Lib "ACBrBAL32.dll" () As Long

Public Declare Function BAL_LePeso _
                Lib "ACBrBAL32.dll" (ByVal MillisecTimeOut As Long, _
                                     ByRef Peso As Double) As Long
                                     
Public Declare Function BAL_SolicitarPeso Lib "ACBrBAL32.dll" () As Long

Public Declare Function BAL_UltimoPesoLido _
                Lib "ACBrBAL32.dll" (ByRef Peso As Double) As Long
                
Public Declare Function BAL_InterpretarRespostaPeso _
                Lib "ACBrBAL32.dll" (ByVal eResposta As String, _
                                     ByRef Peso As Double) As Long


Public Sub CheckResult(ByVal Resultado As Long)
    
    If Resultado = 0 Then Exit Sub
         
    Dim buffer As String

    Dim bufferLen As Long

    bufferLen = 256
    buffer = String$(bufferLen, " ")
    BAL_UltimoRetorno buffer, bufferLen
    
    If bufferLen > 256 Then
        BAL_UltimoRetorno buffer, bufferLen
    End If

    Err.Raise Resultado, "ACBrBAL", FromUTF8(buffer, bufferLen)
End Sub

Public Function FromUTF8(ByRef utf8STR As String, ByVal bufferLen As Long) As String
    Dim UTF8() As Byte

    UTF8 = StrConv(utf8STR, vbFromUnicode)
    
    Dim lDataLength As Long
    
    ' Get the length of the data.
    lDataLength = MultiByteToWideChar(CP_UTF8, 0, VarPtr(UTF8(0)), bufferLen, 0, 0)
    
    ' Create array big enough
    FromUTF8 = String$(lDataLength, 0)
    
    MultiByteToWideChar CP_UTF8, 0, VarPtr(UTF8(0)), bufferLen, StrPtr(FromUTF8), lDataLength
End Function

Function DirExists(DirName As String) As Boolean
    On Error GoTo ErrorHandler
    ' test the directory attribute
    DirExists = GetAttr(DirName) And vbDirectory
ErrorHandler:
    ' if an error occurs, this function returns False
End Function

