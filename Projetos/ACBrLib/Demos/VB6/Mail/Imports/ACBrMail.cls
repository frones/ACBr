VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ACBrMail"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Const BUFFER_LENGTH As Long = 1024

' UTF-8 Code Page'Sys call to convert multiple byte chars to a charPrivate
Const CP_UTF8       As Long = 65001

Private Declare Function MultiByteToWideChar _
                Lib "kernel32" (ByVal CodePage As Long, _
                                ByVal dwFlags As Long, _
                                ByVal lpMultiByteStr As Long, _
                                ByVal cchMultiByte As Long, _
                                ByVal lpWideCharStr As Long, _
                                ByVal cchWideChar As Long) As Long
                                
Private Declare Function MAIL_Inicializar _
                Lib "ACBrMAIL32.dll" (ByVal eArqConfig As String, _
                                      ByVal eChaveCrypt As String) As Long
                   
Private Declare Function MAIL_Finalizar Lib "ACBrMAIL32.dll" () As Long

Private Declare Function MAIL_Nome _
                Lib "ACBrMAIL32.dll" (ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                   
Private Declare Function MAIL_Versao _
                Lib "ACBrMAIL32.dll" (ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long

Private Declare Function MAIL_UltimoRetorno _
                Lib "ACBrMAIL32.dll" (ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                   
Private Declare Function MAIL_ConfigLer _
                Lib "ACBrMAIL32.dll" (ByVal eArqConfig As String) As Long

Private Declare Function MAIL_ConfigGravar _
                Lib "ACBrMAIL32.dll" (ByVal eArqConfig As String) As Long
                   
Private Declare Function MAIL_ConfigLerValor _
                Lib "ACBrMAIL32.dll" (ByVal eSessao As String, _
                                      ByVal eChave As String, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long

Private Declare Function MAIL_ConfigGravarValor _
                Lib "ACBrMAIL32.dll" (ByVal eSessao As String, _
                                      ByVal eChave As String, _
                                      ByVal valor As String) As Long
                                            
Private Declare Function MAIL_SetSubject _
                Lib "ACBrMAIL32.dll" (ByVal eSessao As String) As Long
                
Private Declare Function MAIL_AddAddress _
                Lib "ACBrMAIL32.dll" (ByVal eEmail As String, _
                                      ByVal eName As String) As Long
                                      
Private Declare Function MAIL_AddReplyTo _
                Lib "ACBrMAIL32.dll" (ByVal eEmail As String, _
                                      ByVal eName As String) As Long
                                      
Private Declare Function MAIL_AddCC _
                Lib "ACBrMAIL32.dll" (ByVal eEmail As String, _
                                      ByVal eName As String) As Long
                                      
Private Declare Function MAIL_AddBCC Lib "ACBrMAIL32.dll" (ByVal eEmail As String) As Long
                
Private Declare Function MAIL_AddAttachment _
                Lib "ACBrMAIL32.dll" (ByVal eFileName As String, _
                                      ByVal eDescription As String, _
                                      ByVal aDisposition As Long) As Long
                                      
Private Declare Function MAIL_ClearAttachment Lib "ACBrMAIL32.dll" () As Long

Private Declare Function MAIL_AddBody Lib "ACBrMAIL32.dll" (ByVal eBody As String) As Long
                
Private Declare Function MAIL_AddAltBody _
                Lib "ACBrMAIL32.dll" (ByVal eAltBody As String) As Long
                
Private Declare Function MAIL_SaveToFile _
                Lib "ACBrMAIL32.dll" (ByVal eFileName As String) As Long
                
Private Declare Function MAIL_Clear Lib "ACBrMAIL32.dll" () As Long

Private Declare Function MAIL_Send Lib "ACBrMAIL32.dll" () As Long

Public Sub InicializarLib(Optional ByVal eArqConfig As String = "", _
                          Optional ByVal eChaveCrypt As String = "")

    Dim retorno As Long

    retorno = MAIL_Inicializar(eArqConfig, eChaveCrypt)
    CheckResult (retorno)
End Sub

Public Sub FinalizarLib()

    Dim retorno As Long

    retorno = MAIL_Finalizar
    CheckResult (retorno)
End Sub

Public Sub ConfigLer(Optional eArqConfig As String = "")

    Dim retorno As Long

    retorno = MAIL_ConfigLer(eArqConfig)
    CheckResult (retorno)
End Sub

Public Sub ConfigGravar(Optional eArqConfig As String = "")

    Dim retorno As Long

    retorno = MAIL_ConfigGravar(eArqConfig)
    CheckResult (retorno)
End Sub

Public Function ConfigLerValor(ByVal eSessao As String, ByVal eChave As String) As String

    Dim retorno   As Long

    Dim buffer    As String

    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen + 1, " ")
    
    retorno = MAIL_ConfigLerValor(eSessao, eChave, buffer, bufferLen)
    CheckResult (retorno)
    
    ConfigLerValor = processResult(buffer, bufferLen)
End Function

Public Sub ConfigGravarValor(ByVal eSessao As String, _
                             ByVal eChave As String, _
                             ByVal valor As String)

    Dim retorno As Long
    
    retorno = MAIL_ConfigGravarValor(eSessao, eChave, valor)
    CheckResult (retorno)
End Sub

Public Function Nome() As String

    Dim retorno   As Long

    Dim buffer    As String

    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
        
    retorno = MAIL_Nome(buffer, bufferLen)
    CheckResult (retorno)
    
    Nome = processResult(buffer, bufferLen)
End Function

Public Function Versao() As String

    Dim retorno   As Long

    Dim buffer    As String

    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = MAIL_Versao(buffer, bufferLen)
    CheckResult (retorno)
    
    Versao = processResult(buffer, bufferLen)
End Function

Public Sub SetSubject(ByVal eSubject As String)
    Dim retorno As Long
    
    retorno = MAIL_SetSubject(eSubject)
    CheckResult (retorno)
End Sub

Public Sub AddAddress(ByVal eMail As String, ByVal eName As String)
    Dim retorno As Long
    
    retorno = MAIL_AddAddress(eMail, eName)
    CheckResult (retorno)
End Sub

Public Sub AddReplyTo(ByVal eMail As String, ByVal eName As String)
    Dim retorno As Long
    
    retorno = MAIL_AddReplyTo(eMail, eName)
    CheckResult (retorno)
End Sub

Public Sub AddCC(ByVal eMail As String, ByVal eName As String)
    Dim retorno As Long
    
    retorno = MAIL_AddCC(eMail, eName)
    CheckResult (retorno)
End Sub

Public Sub AddBCC(ByVal eMail As String)
    Dim retorno As Long
    
    retorno = MAIL_AddBCC(eMail)
    CheckResult (retorno)
End Sub

Public Sub AddAttachment(ByVal eFileName As String, _
                         ByVal eDescription As String, _
                         ByVal aDisposition As MailAttachmentDisposition)
    Dim retorno As Long
    
    retorno = MAIL_AddAttachment(eFileName, eDescription, CInt(aDisposition))
    CheckResult (retorno)
End Sub

Public Sub ClearAttachment()
    Dim retorno As Long
    
    retorno = MAIL_ClearAttachment
    CheckResult (retorno)
End Sub

Public Sub AddBody(ByVal eBody As String)
    Dim retorno As Long
    
    retorno = MAIL_AddBody(eBody)
    CheckResult (retorno)
End Sub

Public Sub AddAltBody(ByVal eAltBody As String)
    Dim retorno As Long
    
    retorno = MAIL_AddAltBody(eAltBody)
    CheckResult (retorno)
End Sub

Public Sub SaveToFile(ByVal eFileName As String)
    Dim retorno As Long
    
    retorno = MAIL_SaveToFile(eFileName)
    CheckResult (retorno)
End Sub

Public Sub Clear()
    Dim retorno As Long
    
    retorno = MAIL_Clear()
    CheckResult (retorno)
End Sub

Public Sub Send()
    Dim retorno As Long
    
    retorno = MAIL_Send()
    CheckResult (retorno)
End Sub

Private Sub CheckResult(ByVal Resultado As Long)
    
    If Resultado = 0 Then Exit Sub
         
    Dim buffer    As String

    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    MAIL_UltimoRetorno buffer, bufferLen
    
    Err.Raise Resultado, "ACBrMail", processResult(buffer, bufferLen)
End Sub

Private Function processResult(ByRef buffer As String, ByRef bufferLen As Long) As String
    
    If bufferLen > BUFFER_LENGTH Then
        buffer = String$(bufferLen, " ")
        MAIL_UltimoRetorno buffer, bufferLen
    End If

    processResult = Trim$(FromUTF8(buffer))
End Function

Private Function FromUTF8(ByRef utf8STR As String) As String
    
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

