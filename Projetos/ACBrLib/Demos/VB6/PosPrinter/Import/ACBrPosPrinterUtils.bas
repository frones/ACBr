Attribute VB_Name = "ACBrPosPrinterUtils"
Option Explicit

Public Function CreatePosPrinter(Optional ByVal eArqConfig As String = "", Optional ByVal eChaveCrypt As String = "") As ACBrPosPrinter
    Dim posPrinter As ACBrPosPrinter
    Set posPrinter = New ACBrPosPrinter
    
    posPrinter.InicializarLib eArqConfig, eChaveCrypt
    Set CreatePosPrinter = posPrinter
End Function


