Attribute VB_Name = "ACBrNFeUtils"
Option Explicit

Public Function CreateNFe(Optional ByVal eArqConfig As String = "", _
                           Optional ByVal eChaveCrypt As String = "") As ACBrNFe
    Dim nfe As ACBrNFe
    Set nfe = New ACBrNFe
    
    nfe.InicializarLib eArqConfig, eChaveCrypt
    Set CreateNFe = nfe
End Function

