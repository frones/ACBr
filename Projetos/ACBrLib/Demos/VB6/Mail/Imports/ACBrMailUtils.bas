Attribute VB_Name = "ACBrMailUtils"
Option Explicit

Public Function CreateMail(Optional ByVal eArqConfig As String = "", _
                           Optional ByVal eChaveCrypt As String = "") As ACBrMail
    Dim mail As ACBrMail
    Set mail = New ACBrMail
    
    mail.InicializarLib eArqConfig, eChaveCrypt
    Set CreateMail = mail
End Function

