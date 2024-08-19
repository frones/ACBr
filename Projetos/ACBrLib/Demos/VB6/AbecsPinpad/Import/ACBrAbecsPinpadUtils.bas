Attribute VB_Name = "ACBrAbecsPinpadUtils"
Option Explicit

Public Function CreateAbecsPinpad(Optional ByVal eArqConfig As String = "", Optional ByVal eChaveCrypt As String = "") As ACBrAbecsPinpad
    Dim abecsPinpad As ACBrAbecsPinpad
    Set abecsPinpad = New ACBrAbecsPinpad
    
    abecsPinpad.InicializarLib eArqConfig, eChaveCrypt
    Set CreateAbecsPinpad = abecsPinpad
End Function


