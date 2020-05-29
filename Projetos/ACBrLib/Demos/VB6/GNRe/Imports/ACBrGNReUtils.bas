Attribute VB_Name = "ACBrGNReUtils"
Option Explicit

Public Function CreateGNRe(Optional ByVal eArqConfig As String = "", _
                           Optional ByVal eChaveCrypt As String = "") As ACBrGNRe
    Dim gnre As ACBrGNRe
    Set gnre = New ACBrGNRe
    
    gnre.InicializarLib eArqConfig, eChaveCrypt
    Set CreateGNRe = gnre
End Function

