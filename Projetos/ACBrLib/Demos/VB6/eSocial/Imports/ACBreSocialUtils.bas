Attribute VB_Name = "ACBreSocialUtils"
Option Explicit

Public Function CreateeSocial(Optional ByVal eArqConfig As String = "", _
                           Optional ByVal eChaveCrypt As String = "") As ACBreSocial
    Dim eSocial As ACBreSocial
    Set eSocial = New ACBreSocial
    
    eSocial.InicializarLib eArqConfig, eChaveCrypt
    Set CreateeSocial = eSocial
End Function

