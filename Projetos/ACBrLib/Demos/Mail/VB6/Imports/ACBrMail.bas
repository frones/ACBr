Attribute VB_Name = "ACBrMail"
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
                                
Public Declare Function MAIL_Inicializar _
                Lib "ACBrMAIL32.dll" (ByVal eArqConfig As String, _
                                            ByVal eChaveCrypt As String) As Long
                   
Public Declare Function MAIL_Finalizar Lib "ACBrMAIL32.dll" () As Long

Public Declare Function MAIL_Nome _
                Lib "ACBrMAIL32.dll" (ByVal buffer As String, _
                                            ByRef bufferLen As Long) As Long
                   
Public Declare Function MAIL_Versao _
                Lib "ACBrMAIL32.dll" (ByVal buffer As String, _
                                            ByRef bufferLen As Long) As Long

Public Declare Function MAIL_UltimoRetorno _
                Lib "ACBrMAIL32.dll" (ByVal buffer As String, _
                                            ByRef bufferLen As Long) As Long
                   
Public Declare Function MAIL_ConfigLer _
                Lib "ACBrMAIL32.dll" (ByVal eArqConfig As String) As Long

Public Declare Function MAIL_ConfigGravar _
                Lib "ACBrMAIL32.dll" (ByVal eArqConfig As String) As Long
                   
Public Declare Function MAIL_ConfigLerValor _
                Lib "ACBrMAIL32.dll" (ByVal eSessao As String, _
                                            ByVal eChave As String, _
                                            ByVal buffer As String, _
                                            ByRef bufferLen As Long) As Long

Public Declare Function MAIL_ConfigGravarValor _
                Lib "ACBrMAIL32.dll" (ByVal eSessao As String, _
                                            ByVal eChave As String, _
                                            ByVal valor As String) As Long
                                            
Public Declare Function MAIL_SetSubject _
                Lib "ACBrMAIL32.dll" (ByVal eSessao As String) As Long
                
Public Declare Function MAIL_AddAddress _
                Lib "ACBrMAIL32.dll" (ByVal eEmail As String, _
                                      ByVal eName As String) As Long
                                      
Public Declare Function MAIL_AddReplyTo _
                Lib "ACBrMAIL32.dll" (ByVal eEmail As String, _
                                      ByVal eName As String) As Long
                                      
Public Declare Function MAIL_AddCC _
                Lib "ACBrMAIL32.dll" (ByVal eEmail As String, _
                                      ByVal eName As String) As Long
                                      
Public Declare Function MAIL_AddBCC _
                Lib "ACBrMAIL32.dll" (ByVal eEmail As String) As Long
                
Public Declare Function MAIL_AddAttachment _
                Lib "ACBrMAIL32.dll" (ByVal eFileName As String, _
                                      ByVal eDescription As String, _
                                      ByVal aDisposition As Long) As Long
                                      
Public Declare Function MAIL_ClearAttachment Lib "ACBrMAIL32.dll" () As Long

Public Declare Function MAIL_AddBody _
                Lib "ACBrMAIL32.dll" (ByVal eBody As String) As Long
                
Public Declare Function MAIL_AddAltBody _
                Lib "ACBrMAIL32.dll" (ByVal eAltBody As String) As Long
                
Public Declare Function MAIL_SaveToFile _
                Lib "ACBrMAIL32.dll" (ByVal eFileName As String) As Long
                
Public Declare Function MAIL_Clear Lib "ACBrMAIL32.dll" () As Long

Public Declare Function MAIL_Send Lib "ACBrMAIL32.dll" () As Long

Public Sub CheckResult(ByVal Resultado As Long)
    
    If Resultado = 0 Then Exit Sub
         
    Dim buffer As String

    Dim bufferLen As Long

    bufferLen = 256
    buffer = String$(bufferLen, " ")
    MAIL_UltimoRetorno buffer, bufferLen
    
    If bufferLen > 256 Then
        MAIL_UltimoRetorno buffer, bufferLen
    End If

    Err.Raise Resultado, "ACBrMail", Trim$(FromUTF8(buffer))
End Sub

Public Function FromUTF8(ByRef utf8STR As String) As String
    
    Dim length As Long

    length = Len(utf8STR)
    
    Dim UTF8() As Byte

    UTF8 = StrConv(utf8STR, vbFromUnicode)
    
    Dim lDataLength As Long
    
    ' Get the length of the data.
    lDataLength = MultiByteToWideChar(CP_UTF8, 0, VarPtr(UTF8(0)), length, 0, 0)
    
    ' Create array big enough
    FromUTF8 = String$(lDataLength, 0)
    
    MultiByteToWideChar CP_UTF8, 0, VarPtr(UTF8(0)), length, StrPtr(FromUTF8), lDataLength
End Function
