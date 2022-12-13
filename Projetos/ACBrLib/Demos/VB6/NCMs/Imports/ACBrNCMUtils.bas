Attribute VB_Name = "ACBrNCMUtils"
Option Explicit

Public Function CreateNCM(Optional ByVal eArqConfig As String = "", _
                           Optional ByVal eChaveCrypt As String = "") As ACBrNCM
    Dim ncm As ACBrNCM
    Set ncm = New ACBrNCM
    
    ncm.InicializarLib eArqConfig, eChaveCrypt
    Set CreateNCM = ncm
End Function

