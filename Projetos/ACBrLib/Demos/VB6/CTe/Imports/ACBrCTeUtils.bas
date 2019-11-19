Attribute VB_Name = "ACBrCTeUtils"
Option Explicit

Public Function CreateCTe(Optional ByVal eArqConfig As String = "", _
                          Optional ByVal eChaveCrypt As String = "") As ACBrCTe
    Dim cte As ACBrCTe
    Set cte = New ACBrCTe
    
    cte.InicializarLib eArqConfig, eChaveCrypt
    Set CreateCTe = cte
End Function

