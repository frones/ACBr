Attribute VB_Name = "ACBrBoletoUtils"
Option Explicit

Public Function CreateBoleto(Optional ByVal eArqConfig As String = "", _
                           Optional ByVal eChaveCrypt As String = "") As ACBrBoleto
    Dim boleto As ACBrBoleto
    Set boleto = New ACBrBoleto
    
    boleto.InicializarLib eArqConfig, eChaveCrypt
    Set CreateBoleto = boleto
End Function
