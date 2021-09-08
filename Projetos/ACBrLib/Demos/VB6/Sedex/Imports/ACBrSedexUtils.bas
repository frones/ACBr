Attribute VB_Name = "ACBrSedexUtils"
Option Explicit

Public Function CreateSedex(Optional ByVal eArqConfig As String = "", _
                           Optional ByVal eChaveCrypt As String = "") As ACBrSedex
    Dim sedex As ACBrSedex
    Set sedex = New ACBrSedex
    
    sedex.InicializarLib eArqConfig, eChaveCrypt
    Set CreateSedex = sedex
End Function
