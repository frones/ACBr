Attribute VB_Name = "ACBrBALUtils"
Option Explicit

Public Function CreateBAL(Optional ByVal eArqConfig As String = "", _
                          Optional ByVal eChaveCrypt As String = "") As ACBrBAL
    Dim bal As ACBrBAL
    Set bal = New ACBrBAL
    
    bal.InicializarLib eArqConfig, eChaveCrypt
    Set CreateBAL = bal
End Function
