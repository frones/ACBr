Attribute VB_Name = "ACBrSatUtils"
Option Explicit

Public Function CreateSat(Optional ByVal eArqConfig As String = "", _
                           Optional ByVal eChaveCrypt As String = "") As ACBrSat
    Dim sat As ACBrSat
    Set sat = New ACBrSat
    
    sat.InicializarLib eArqConfig, eChaveCrypt
    Set CreateSat = sat
End Function

