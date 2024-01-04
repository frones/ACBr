Attribute VB_Name = "ACBrPIXCDUtils"
Option Explicit

Public Function CreatePIXCD(Optional ByVal eArqConfig As String = "", _
                           Optional ByVal eChaveCrypt As String = "") As ACBrPIXCD
    Dim pixcd As ACBrPIXCD
    Set pixcd = New ACBrPIXCD
    
    pixcd.InicializarLib eArqConfig, eChaveCrypt
    Set CreatePIXCD = pixcd
End Function

