Attribute VB_Name = "ACBrETQUtils"
Option Explicit

Public Function CreateETQ(Optional ByVal eArqConfig As String = "", Optional ByVal eChaveCrypt As String = "") As ACBrETQ
    Dim etq As ACBrETQ
    Set etq = New ACBrETQ
    
    etq.InicializarLib eArqConfig, eChaveCrypt
    Set CreateETQ = etq
End Function