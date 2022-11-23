Attribute VB_Name = "ACBrConsultaCNPJUtils"
Option Explicit

Public Function CreateCNPJ(Optional ByVal eArqConfig As String = "", _
                           Optional ByVal eChaveCrypt As String = "") As ACBrConsultaCNPJ
    Dim cnpj As ACBrConsultaCNPJ
    Set cnpj = New ACBrConsultaCNPJ
    
    cnpj.InicializarLib eArqConfig, eChaveCrypt
    Set CreateCNPJ = cnpj
End Function

