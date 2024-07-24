VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ACBrPIXCD"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Private libHandler As Long

Const BUFFER_LENGTH As Long = 1024
                               
Private Declare Function PIXCD_Inicializar _
                Lib "ACBrPIXCD32.dll" (ByRef libHandler As Long, _
                                       ByVal eArqConfig As String, _
                                       ByVal eChaveCrypt As String) As Long
                   
Private Declare Function PIXCD_Finalizar Lib "ACBrNFE32.dll" (ByVal libHandler As Long) As Long

Private Declare Function PIXCD_Nome _
                Lib "ACBrPIXCD32.dll" (ByVal libHandler As Long, _
                                       ByVal buffer As String, _
                                       ByRef bufferLen As Long) As Long
                   
Private Declare Function PIXCD_Versao _
                Lib "ACBrPIXCD32.dll" (ByVal libHandler As Long, _
                                       ByVal buffer As String, _
                                       ByRef bufferLen As Long) As Long

Private Declare Function PIXCD_UltimoRetorno _
                Lib "ACBrPIXCD32.dll" (ByVal libHandler As Long, _
                                       ByVal buffer As String, _
                                       ByRef bufferLen As Long) As Long
                   
Private Declare Function PIXCD_ConfigImportar _
                Lib "ACBrPIXCD32.dll" (ByVal libHandler As Long, _
                                       ByVal eArqConfig As String) As Long
                   
Private Declare Function PIXCD_ConfigExportar _
                Lib "ACBrPIXCD32.dll" (ByVal libHandler As Long, _
                                       ByVal buffer As String, _
                                       ByRef bufferLen As Long) As Long
                   
Private Declare Function PIXCD_ConfigLer _
                Lib "ACBrPIXCD32.dll" (ByVal libHandler As Long, _
                                       ByVal eArqConfig As String) As Long

Private Declare Function PIXCD_ConfigGravar _
                Lib "ACBrPIXCD32.dll" (ByVal libHandler As Long, _
                                       ByVal eArqConfig As String) As Long
                   
Private Declare Function PIXCD_ConfigLerValor _
                Lib "ACBrPIXCD32.dll" (ByVal libHandler As Long, _
                                       ByVal eSessao As String, _
                                       ByVal eChave As String, _
                                       ByVal buffer As String, _
                                       ByRef bufferLen As Long) As Long

Private Declare Function PIXCD_ConfigGravarValor _
                Lib "ACBrPIXCD32.dll" (ByVal libHandler As Long, _
                                       ByVal eSessao As String, _
                                       ByVal eChave As String, _
                                       ByVal valor As String) As Long
                                      
Private Declare Function PIXCD_GerarQRCodeEstatico _
                Lib "ACBrPIXCD32.dll" (ByVal libHandler As Long, _
                                       ByVal AValor As Double, _
                                       ByVal AinfoAdicional As String, _
                                       ByVal ATxId As String, _
                                       ByVal buffer As String, _
                                       ByRef bufferLen As Long) As Long
                                       
Private Declare Function PIXCD_ConsultarPix _
                Lib "ACBrPIXCD32.dll" (ByVal libHandler As Long, _
                                       ByVal Ae2eid As String, _
                                       ByVal buffer As String, _
                                       ByRef bufferLen As Long) As Long
                                      
Private Declare Function PIXCD_ConsultarPixRecebidos _
                Lib "ACBrPIXCD32.dll" (ByVal libHandler As Long, _
                                       ByVal ADataInicio As Date, _
                                       ByVal ADataFim As Date, _
                                       ByVal ATxId As String, _
                                       ByVal ACpfCnpj As String, _
                                       ByVal PagAtual As Long, _
                                       ByVal ItensPorPagina As Long, _
                                       ByVal buffer As String, _
                                       ByRef bufferLen As Long) As Long
                                       
Private Declare Function PIXCD_SolicitarDevolucaoPix _
                Lib "ACBrPIXCD32.dll" (ByVal libHandler As Long, _
                                       ByVal AInfDevolucao As String, _
                                       ByVal Ae2eid As String, _
                                       ByVal AidDevolucao As String, _
                                       ByVal buffer As String, _
                                       ByRef bufferLen As Long) As Long
                                       
Private Declare Function PIXCD_ConsultarDevolucaoPix _
                Lib "ACBrPIXCD32.dll" (ByVal libHandler As Long, _
                                       ByVal Ae2eid As String, _
                                       ByVal AidDevolucao As String, _
                                       ByVal buffer As String, _
                                       ByRef bufferLen As Long) As Long
                                       
Private Declare Function PIXCD_CriarCobrancaImediata _
                Lib "ACBrPIXCD32.dll" (ByVal libHandler As Long, _
                                       ByVal AInfCobSolicitada As String, _
                                       ByVal ATxId As String, _
                                       ByVal buffer As String, _
                                       ByRef bufferLen As Long) As Long
                                       
Private Declare Function PIXCD_ConsultarCobrancaImediata _
                Lib "ACBrPIXCD32.dll" (ByVal libHandler As Long, _
                                       ByVal ATxId As String, _
                                       ByVal ARevisao As Long, _
                                       ByVal buffer As String, _
                                       ByRef bufferLen As Long) As Long
                                       
Private Declare Function PIXCD_RevisarCobrancaImediata _
                Lib "ACBrPIXCD32.dll" (ByVal libHandler As Long, _
                                       ByVal AInfCobRevisada As String, _
                                       ByVal ATxId As String, _
                                       ByVal buffer As String, _
                                       ByRef bufferLen As Long) As Long

Private Declare Function PIXCD_CancelarCobrancaImediata _
                Lib "ACBrPIXCD32.dll" (ByVal libHandler As Long, _
                                       ByVal ATxId As String, _
                                       ByVal buffer As String, _
                                       ByRef bufferLen As Long) As Long

Private Declare Function PIXCD_CriarCobranca _
                Lib "ACBrPIXCD32.dll" (ByVal libHandler As Long, _
                                       ByVal AInfCobVSolicitada As String, _
                                       ByVal ATxId As String, _
                                       ByVal buffer As String, _
                                       ByRef bufferLen As Long) As Long
                                       
Private Declare Function PIXCD_ConsultarCobranca _
                Lib "ACBrPIXCD32.dll" (ByVal libHandler As Long, _
                                       ByVal ATxId As String, _
                                       ByVal ARevisao As Long, _
                                       ByVal buffer As String, _
                                       ByRef bufferLen As Long) As Long
                                       
Private Declare Function PIXCD_RevisarCobranca _
                Lib "ACBrPIXCD32.dll" (ByVal libHandler As Long, _
                                       ByVal AInfCobVRevisada As String, _
                                       ByVal ATxId As String, _
                                       ByVal buffer As String, _
                                       ByRef bufferLen As Long) As Long

Private Declare Function PIXCD_CancelarCobranca _
                Lib "ACBrPIXCD32.dll" (ByVal libHandler As Long, _
                                       ByVal ATxId As String, _
                                       ByVal buffer As String, _
                                       ByRef bufferLen As Long) As Long


Public Sub InicializarLib(Optional ByVal eArqConfig As String = "", _
                          Optional ByVal eChaveCrypt As String = "")

    SetLibPath
    Dim retorno As Long

    retorno = PIXCD_Inicializar(libHandler, eArqConfig, eChaveCrypt)
    CheckResult retorno
End Sub

Public Sub FinalizarLib()

    Dim retorno As Long

    retorno = PIXCD_Finalizar(libHandler)
    CheckResult retorno
End Sub

Public Sub ConfigImportar(ByVal eArqConfig As String)

    Dim retorno As Long

    retorno = PIXCD_ConfigImportar(libHandler, eArqConfig)
    CheckResult (retorno)
    
End Sub
    
Public Function ConfigExportar() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = PIXCD_ConfigExportar(libHandler, buffer, bufferLen)
    CheckResult retorno
    
    ConfigExportar = processResult(buffer, bufferLen)
    
End Function

Public Sub ConfigLer(Optional eArqConfig As String = "")

    Dim retorno As Long

    retorno = PIXCD_ConfigLer(libHandler, eArqConfig)
    CheckResult retorno
End Sub

Public Sub ConfigGravar(Optional eArqConfig As String = "")

    Dim retorno As Long

    retorno = PIXCD_ConfigGravar(libHandler, eArqConfig)
    CheckResult retorno
End Sub

Public Function ConfigLerValor(ByVal eSessao As String, ByVal eChave As String) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = PIXCD_ConfigLerValor(libHandler, eSessao, eChave, buffer, bufferLen)
    CheckResult retorno
    
    ConfigLerValor = processResult(buffer, bufferLen)
End Function

Public Sub ConfigGravarValor(ByVal eSessao As String, _
                             ByVal eChave As String, _
                             ByVal valor As String)

    Dim retorno As Long
    
    retorno = PIXCD_ConfigGravarValor(libHandler, eSessao, eChave, valor)
    CheckResult retorno
End Sub

Public Function Nome() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
        
    retorno = PIXCD_Nome(libHandler, buffer, bufferLen)
    CheckResult retorno
    
    Nome = processResult(buffer, bufferLen)
End Function

Public Function Versao() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = PIXCD_Versao(libHandler, buffer, bufferLen)
    CheckResult retorno
    
    Versao = processResult(buffer, bufferLen)
End Function

Public Function GerarQRCodeEstatico(ByVal AValor As Double, ByVal AinfoAdicional As String, ByVal ATxId As String) As String

    Dim retorno As Long
    Dim buffer As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")

    retorno = PIXCD_GerarQRCodeEstatico(libHandler, AValor, AinfoAdicional, ATxId, buffer, bufferLen)
    CheckResult retorno

    GerarQRCodeEstatico = processResult(buffer, bufferLen)
    
End Function

Public Function ConsultarPix(ByVal Ae2eid As String) As String

    Dim retorno As Long
    Dim buffer As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")

    retorno = PIXCD_ConsultarPix(libHandler, Ae2eid, buffer, bufferLen)
    
    ConsultarPix = processResult(buffer, bufferLen)
    
End Function

Public Function ConsultarPixRecebidos(ByVal ADataInicio As Date, ByVal ADataFim As Date, ByVal ATxId As String, ByVal ACpfCnpj As String, ByVal PagAtual As Long, ByVal ItensPorPagina As Long) As String

    Dim retorno As Long
    Dim buffer As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = PIXCD_ConsultarPixRecebidos(libHandler, ADataInicio, ADataFim, ATxId, ACpfCnpj, PagAtual, ItensPorPagina, buffer, bufferLen)

    ConsultarPixRecebidos = processResult(buffer, bufferLen)

End Function

Public Function SolicitarDevolucaoPix(ByVal AInfDevolucao As String, ByVal Ae2eid As String, ByVal AidDevolucao As String) As String

    Dim retorno As Long
    Dim buffer As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = PIXCD_SolicitarDevolucaoPix(libHandler, AInfDevolucao, Ae2eid, AidDevolucao, buffer, bufferLen)
    
    SolicitarDevolucaoPix = processResult(buffer, bufferLen)

End Function

Public Function ConsultarDevolucaoPix(ByVal Ae2eid As String, ByVal AidDevolucao As String) As String

    Dim retorno As Long
    Dim buffer As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = PIXCD_ConsultarDevolucaoPix(libHandler, Ae2eid, AidDevolucao, buffer, bufferLen)
    
    ConsultarDevolucaoPix = processResult(buffer, bufferLen)

End Function

Public Function CriarCobrancaImediata(ByVal AInfCobSolicitada As String, ByVal ATxId As String) As String

    Dim retorno As Long
    Dim buffer As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = PIXCD_CriarCobrancaImediata(libHandler, AInfCobSolicitada, ATxId, buffer, bufferLen)
    
    CriarCobrancaImediata = processResult(buffer, bufferLen)

End Function

Public Function ConsultarCobrancaImediata(ByVal ATxId As String, ByVal ARevisao As Long) As String

    Dim retorno As Long
    Dim buffer As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = PIXCD_ConsultarCobrancaImediata(libHandler, ATxId, ARevisao, buffer, bufferLen)
    
    ConsultarCobrancaImediata = processResult(buffer, bufferLen)

End Function

Public Function RevisarCobrancaImediata(ByVal AInfCobRevisada As String, ByVal ATxId As String) As String

    Dim retorno As Long
    Dim buffer As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = PIXCD_RevisarCobrancaImediata(libHandler, AInfCobRevisada, ATxId, buffer, bufferLen)
    
    RevisarCobrancaImediata = processResult(buffer, bufferLen)

End Function

Public Function CancelarCobrancaImediata(ByVal ATxId As String) As String

    Dim retorno As Long
    Dim buffer As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = PIXCD_CancelarCobrancaImediata(libHandler, ATxId, buffer, bufferLen)
    
    CancelarCobrancaImediata = processResult(buffer, bufferLen)

End Function

Public Function CriarCobranca(ByVal AInfCobVSolicitada As String, ByVal ATxId As String) As String

    Dim retorno As Long
    Dim buffer As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = PIXCD_CriarCobranca(libHandler, AInfCobVSolicitada, ATxId, buffer, bufferLen)
    
    CriarCobranca = processResult(buffer, bufferLen)

End Function

Public Function ConsultarCobranca(ByVal ATxId As String, ByVal ARevisao As Long) As String

    Dim retorno As Long
    Dim buffer As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = PIXCD_ConsultarCobranca(libHandler, ATxId, ARevisao, buffer, bufferLen)
    
    ConsultarCobranca = processResult(buffer, bufferLen)

End Function

Public Function RevisarCobranca(ByVal AInfCobVRevisada As String, ByVal ATxId As String) As String

    Dim retorno As Long
    Dim buffer As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = PIXCD_RevisarCobranca(libHandler, AInfCobVRevisada, ATxId, buffer, bufferLen)
    
    RevisarCobranca = processResult(buffer, bufferLen)

End Function

Public Function CancelarCobranca(ByVal ATxId As String) As String

    Dim retorno As Long
    Dim buffer As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = PIXCD_CancelarCobranca(libHandler, ATxId, buffer, bufferLen)
    
    CancelarCobranca = processResult(buffer, bufferLen)

End Function

Private Sub CheckResult(ByVal Resultado As Long)
    
    If Resultado >= 0 Then Exit Sub
         
    Dim buffer As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    PIXCD_UltimoRetorno libHandler, buffer, bufferLen
    
    Err.Raise Resultado, "ACBrPIXCD", processResult(buffer, bufferLen)
End Sub

Private Function processResult(ByRef buffer As String, ByRef bufferLen As Long) As String
    
    If bufferLen > BUFFER_LENGTH Then
        buffer = String$(bufferLen, " ")
        PIXCD_UltimoRetorno libHandler, buffer, bufferLen
    End If

    processResult = Trim$(FromUTF8(buffer))
End Function
