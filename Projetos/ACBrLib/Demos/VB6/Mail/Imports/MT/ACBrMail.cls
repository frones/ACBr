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

Private libHandler As Long

Const BUFFER_LENGTH As Long = 1024
                               
Private Declare Function MAIL_Inicializar _
                Lib "ACBrMAIL32.dll" (ByRef libHandler As Long, _
                                      ByVal eArqConfig As String, _
                                      ByVal eChaveCrypt As String) As Long
                   
Private Declare Function MAIL_Finalizar Lib "ACBrMAIL32.dll" (ByRef libHandler As Long) As Long

Private Declare Function MAIL_Nome _
                Lib "ACBrMAIL32.dll" (ByVal libHandler As Long, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                   
Private Declare Function MAIL_Versao _
                Lib "ACBrMAIL32.dll" (ByVal libHandler As Long, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long

Private Declare Function MAIL_UltimoRetorno _
                Lib "ACBrMAIL32.dll" (ByVal libHandler As Long, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                                      
Private Declare Function MAIL_ConfigImportar _
                Lib "ACBrMAIL32.dll" (ByVal libHandler As Long, _
                                      ByVal eArqConfig As String) As Long
                   
Private Declare Function MAIL_ConfigExportar _
                Lib "ACBrMAIL32.dll" (ByVal libHandler As Long, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                   
Private Declare Function MAIL_ConfigLer _
                Lib "ACBrMAIL32.dll" (ByVal libHandler As Long, _
                                      ByVal eArqConfig As String) As Long

Private Declare Function MAIL_ConfigGravar _
                Lib "ACBrMAIL32.dll" (ByVal libHandler As Long, _
                                      ByVal eArqConfig As String) As Long
                   
Private Declare Function MAIL_ConfigLerValor _
                Lib "ACBrMAIL32.dll" (ByVal libHandler As Long, _
                                      ByVal eSessao As String, _
                                      ByVal eChave As String, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long

Private Declare Function MAIL_ConfigGravarValor _
                Lib "ACBrMAIL32.dll" (ByVal libHandler As Long, _
                                      ByVal eSessao As String, _
                                      ByVal eChave As String, _
                                      ByVal valor As String) As Long
                                            
Private Declare Function MAIL_SetSubject _
                Lib "ACBrMAIL32.dll" (ByVal libHandler As Long, _
                                      ByVal eSessao As String) As Long
                
Private Declare Function MAIL_AddAddress _
                Lib "ACBrMAIL32.dll" (ByVal libHandler As Long, _
                                      ByVal eEmail As String, _
                                      ByVal eName As String) As Long
                                      
Private Declare Function MAIL_AddReplyTo _
                Lib "ACBrMAIL32.dll" (ByVal libHandler As Long, _
                                      ByVal eEmail As String, _
                                      ByVal eName As String) As Long
                                      
Private Declare Function MAIL_AddCC _
                Lib "ACBrMAIL32.dll" (ByVal libHandler As Long, _
                                      ByVal eEmail As String, _
                                      ByVal eName As String) As Long
                                      
Private Declare Function MAIL_AddBCC Lib "ACBrMAIL32.dll" (ByVal eEmail As String) As Long
                
Private Declare Function MAIL_AddAttachment _
                Lib "ACBrMAIL32.dll" (ByVal libHandler As Long, _
                                      ByVal eFileName As String, _
                                      ByVal eDescription As String, _
                                      ByVal aDisposition As Long) As Long
                                      
Private Declare Function MAIL_ClearAttachment Lib "ACBrMAIL32.dll" (ByVal libHandler As Long) As Long

Private Declare Function MAIL_AddBody Lib "ACBrMAIL32.dll" (ByVal libHandler As Long, ByVal eBody As String) As Long
                
Private Declare Function MAIL_AddAltBody _
                Lib "ACBrMAIL32.dll" (ByVal libHandler As Long, ByVal eAltBody As String) As Long
                
Private Declare Function MAIL_SaveToFile _
                Lib "ACBrMAIL32.dll" (ByVal libHandler As Long, ByVal eFileName As String) As Long
                
Private Declare Function MAIL_Clear Lib "ACBrMAIL32.dll" (ByVal libHandler As Long) As Long

Private Declare Function MAIL_Send Lib "ACBrMAIL32.dll" (ByVal libHandler As Long) As Long

Public Sub InicializarLib(Optional ByVal eArqConfig As String = "", _
                          Optional ByVal eChaveCrypt As String = "")

    SetLibPath

    Dim retorno As Long

    retorno = MAIL_Inicializar(libHandler, eArqConfig, eChaveCrypt)
    CheckResult (retorno)
End Sub

Public Sub FinalizarLib()

    Dim retorno As Long

    retorno = MAIL_Finalizar(libHandler)
    CheckResult (retorno)
End Sub

Public Sub ConfigImportar(ByVal eArqConfig As String)

    Dim retorno As Long

    retorno = MAIL_ConfigImportar(libHandler, eArqConfig)
    CheckResult (retorno)
    
End Sub
    
Public Function ConfigExportar() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = MAIL_ConfigExportar(libHandler, buffer, bufferLen)
    CheckResult retorno
    
    ConfigExportar = processResult(buffer, bufferLen)
    
End Function

Public Sub ConfigLer(Optional eArqConfig As String = "")

    Dim retorno As Long

    retorno = MAIL_ConfigLer(libHandler, eArqConfig)
    CheckResult (retorno)
End Sub

Public Sub ConfigGravar(Optional eArqConfig As String = "")

    Dim retorno As Long

    retorno = MAIL_ConfigGravar(libHandler, eArqConfig)
    CheckResult (retorno)
End Sub

Public Function ConfigLerValor(ByVal eSessao As String, ByVal eChave As String) As String

    Dim retorno   As Long

    Dim buffer    As String

    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen + 1, " ")
    
    retorno = MAIL_ConfigLerValor(libHandler, eSessao, eChave, buffer, bufferLen)
    CheckResult (retorno)
    
    ConfigLerValor = processResult(buffer, bufferLen)
End Function

Public Sub ConfigGravarValor(ByVal eSessao As String, _
                             ByVal eChave As String, _
                             ByVal valor As String)

    Dim retorno As Long
    
    retorno = MAIL_ConfigGravarValor(libHandler, eSessao, eChave, valor)
    CheckResult (retorno)
End Sub

Public Function Nome() As String

    Dim retorno   As Long

    Dim buffer    As String

    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
        
    retorno = MAIL_Nome(libHandler, buffer, bufferLen)
    CheckResult (retorno)
    
    Nome = processResult(buffer, bufferLen)
End Function

Public Function Versao() As String

    Dim retorno   As Long

    Dim buffer    As String

    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = MAIL_Versao(libHandler, buffer, bufferLen)
    CheckResult (retorno)
    
    Versao = processResult(buffer, bufferLen)
End Function

Public Sub SetSubject(ByVal eSubject As String)
    Dim retorno As Long
    
    retorno = MAIL_SetSubject(libHandler, eSubject)
    CheckResult (retorno)
End Sub

Public Sub AddAddress(ByVal eMail As String, ByVal eName As String)
    Dim retorno As Long
    
    retorno = MAIL_AddAddress(libHandler, eMail, eName)
    CheckResult (retorno)
End Sub

Public Sub AddReplyTo(ByVal eMail As String, ByVal eName As String)
    Dim retorno As Long
    
    retorno = MAIL_AddReplyTo(libHandler, eMail, eName)
    CheckResult (retorno)
End Sub

Public Sub AddCC(ByVal eMail As String, ByVal eName As String)
    Dim retorno As Long
    
    retorno = MAIL_AddCC(libHandler, eMail, eName)
    CheckResult (retorno)
End Sub

Public Sub AddBCC(ByVal eMail As String)
    Dim retorno As Long
    
    retorno = MAIL_AddBCC(libHandler, eMail)
    CheckResult (retorno)
End Sub

Public Sub AddAttachment(ByVal eFileName As String, _
                         ByVal eDescription As String, _
                         ByVal aDisposition As MailAttachmentDisposition)
    Dim retorno As Long
    
    retorno = MAIL_AddAttachment(libHandler, eFileName, eDescription, CInt(aDisposition))
    CheckResult (retorno)
End Sub

Public Sub ClearAttachment()
    Dim retorno As Long
    
    retorno = MAIL_ClearAttachment(libHandler)
    CheckResult (retorno)
End Sub

Public Sub AddBody(ByVal eBody As String)
    Dim retorno As Long
    
    retorno = MAIL_AddBody(libHandler, eBody)
    CheckResult (retorno)
End Sub

Public Sub AddAltBody(ByVal eAltBody As String)
    Dim retorno As Long
    
    retorno = MAIL_AddAltBody(libHandler, eAltBody)
    CheckResult (retorno)
End Sub

Public Sub SaveToFile(ByVal eFileName As String)
    Dim retorno As Long
    
    retorno = MAIL_SaveToFile(libHandler, eFileName)
    CheckResult (retorno)
End Sub

Public Sub Clear()
    Dim retorno As Long
    
    retorno = MAIL_Clear(libHandler)
    CheckResult (retorno)
End Sub

Public Sub Send()
    Dim retorno As Long
    
    retorno = MAIL_Send(libHandler)
    CheckResult (retorno)
End Sub

Private Sub CheckResult(ByVal Resultado As Long)
    
    If Resultado = 0 Then Exit Sub
         
    Dim buffer    As String

    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    MAIL_UltimoRetorno libHandler, buffer, bufferLen
    
    Err.Raise Resultado, "ACBrMail", processResult(buffer, bufferLen)
End Sub

Private Function processResult(ByRef buffer As String, ByRef bufferLen As Long) As String
    
    If bufferLen > BUFFER_LENGTH Then
        buffer = String$(bufferLen, " ")
        MAIL_UltimoRetorno libHandler, buffer, bufferLen
    End If

    processResult = Trim$(FromUTF8(buffer))
End Function
