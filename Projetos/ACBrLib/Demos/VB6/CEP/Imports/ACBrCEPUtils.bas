Attribute VB_Name = "ACBrCEPUtils"
Option Explicit

Public Function CreateCEP(Optional ByVal eArqConfig As String = "", _
                           Optional ByVal eChaveCrypt As String = "") As ACBrCEP
    Dim cep As ACBrCEP
    Set cep = New ACBrCEP
    
    cep.InicializarLib eArqConfig, eChaveCrypt
    Set CreateCEP = cep
End Function
