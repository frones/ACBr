Attribute VB_Name = "ACBrGTINUtils"
Option Explicit

Public Function CreateGTIN(Optional ByVal eArqConfig As String = "", _
                           Optional ByVal eChaveCrypt As String = "") As ACBrGTIN
    Dim gtin As ACBrGTIN
    Set gtin = New ACBrGTIN
    
    gtin.InicializarLib eArqConfig, eChaveCrypt
    Set CreateGTIN = gtin
End Function

