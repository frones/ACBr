Attribute VB_Name = "ACBrIBGEUtils"
Option Explicit

Public Function CreateIBGE(Optional ByVal eArqConfig As String = "", _
                           Optional ByVal eChaveCrypt As String = "") As ACBrIBGE
    Dim ibge As ACBrIBGE
    Set ibge = New ACBrIBGE
    
    ibge.InicializarLib eArqConfig, eChaveCrypt
    Set CreateIBGE = ibge
End Function
