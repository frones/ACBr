Attribute VB_Name = "ACBrMDFeUtils"
Option Explicit

Public Function CreateMDFe(Optional ByVal eArqConfig As String = "", _
                          Optional ByVal eChaveCrypt As String = "") As ACBrMDFe
    Dim mdfe As ACBrMDFe
    Set mdfe = New ACBrMDFe
    
    mdfe.InicializarLib eArqConfig, eChaveCrypt
    Set CreateMDFe = mdfe
End Function

