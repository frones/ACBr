Attribute VB_Name = "ACBrNFSeUtils"
Option Explicit

Public Function CreateNFSe(Optional ByVal eArqConfig As String = "", _
                           Optional ByVal eChaveCrypt As String = "") As ACBrNFSe
    Dim nfse As ACBrNFSe
    Set nfse = New ACBrNFSe
    
    nfse.InicializarLib eArqConfig, eChaveCrypt
    Set CreateNFSe = nfse
End Function

