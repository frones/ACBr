VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ACBreSocial"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Const BUFFER_LENGTH As Long = 1024
                               
Private Declare Function eSocial_Inicializar _
                Lib "ACBreSocial32.dll" (ByVal eArqConfig As String, _
                                      ByVal eChaveCrypt As String) As Long
                   
Private Declare Function eSocial_Finalizar Lib "ACBreSocial32.dll" () As Long

Private Declare Function eSocial_Nome _
                Lib "ACBreSocial32.dll" (ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                   
Private Declare Function eSocial_Versao _
                Lib "ACBreSocial32.dll" (ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long

Private Declare Function eSocial_UltimoRetorno _
                Lib "ACBreSocial32.dll" (ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                   
Private Declare Function eSocial_ConfigImportar _
                Lib "ACBreSocial32.dll" (ByVal eArqConfig As String) As Long
                   
Private Declare Function eSocial_ConfigExportar _
                Lib "ACBreSocial32.dll" (ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                   
Private Declare Function eSocial_ConfigLer _
                Lib "ACBreSocial32.dll" (ByVal eArqConfig As String) As Long

Private Declare Function eSocial_ConfigGravar _
                Lib "ACBreSocial32.dll" (ByVal eArqConfig As String) As Long
                   
Private Declare Function eSocial_ConfigLerValor _
                Lib "ACBreSocial32.dll" (ByVal eSessao As String, _
                                      ByVal eChave As String, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long

Private Declare Function eSocial_ConfigGravarValor _
                Lib "ACBreSocial32.dll" (ByVal eSessao As String, _
                                      ByVal eChave As String, _
                                      ByVal valor As String) As Long
                                      
Private Declare Function eSocial_CriarEventoeSocial _
                Lib "ACBreSocial32.dll" (ByVal eArqIni As String) As Long
                
Private Declare Function eSocial_EnviareSocial _
                Lib "ACBreSocial32.dll" (ByVal aGrupo As Long, _
                                         ByVal buffer As String, _
                                         ByRef bufferLen As Long) As Long
                                      
Private Declare Function eSocial_ConsultareSocial _
                Lib "ACBreSocial32.dll" (ByVal eProtocolo As String, _
                                         ByVal buffer As String, _
                                         ByRef bufferLen As Long) As Long
                                         
Private Declare Function eSocial_CriarEnviareSocial _
                Lib "ACBreSocial32.dll" (ByVal eArqIni As String, _
                                         ByVal aGrupo As Long) As Long
                                         
Private Declare Function eSocial_LimpareSocial Lib "ACBreSocial32.dll" () As Long

Private Declare Function eSocial_CarregarXMLEventoeSocial _
                Lib "ACBreSocial32.dll" (ByVal eArquivoOuXML As String) As Long
                
Private Declare Function eSocial_SetIDEmpregador _
                Lib "ACBreSocial32.dll" (ByVal aIdEmpregador As String) As Long
                
Private Declare Function eSocial_SetIDTransmissor _
                Lib "ACBreSocial32.dll" (ByVal aIdTransmissor As String) As Long
                
Private Declare Function eSocial_SetTipoEmpregador _
                Lib "ACBreSocial32.dll" (ByVal aTipoEmpregador As Long) As Long
                
Private Declare Function eSocial_SetVersaoDF _
                Lib "ACBreSocial32.dll" (ByVal sVersao As String) As Long
                
Private Declare Function eSocial_ConsultaIdentificadoresEventosEmpregador _
                Lib "ACBreSocial32.dll" (ByVal aIdEmpregador As String, _
                                         ByVal aTipoEvento As Long, _
                                         ByVal aPeriodoApuracao As Date, _
                                         ByVal buffer As String, _
                                         ByRef bufferLen As Long) As Long
                                         
Private Declare Function eSocial_ConsultaIdentificadoresEventosTabela _
                Lib "ACBreSocial32.dll" (ByVal aIdEmpregador As String, _
                                         ByVal aTipoEvento As Long, _
                                         ByVal aChave As String, _
                                         ByVal aDataInicial As Date, _
                                         ByVal aDataFinal As Date, _
                                         ByVal buffer As String, _
                                         ByRef bufferLen As Long) As Long
                                         
Private Declare Function eSocial_ConsultaIdentificadoresEventosTrabalhador _
                Lib "ACBreSocial32.dll" (ByVal aIdEmpregador As String, _
                                         ByVal aCPFTrabalhador As String, _
                                         ByVal aDataInicial As Date, _
                                         ByVal aDataFinal As Date, _
                                         ByVal buffer As String, _
                                         ByRef bufferLen As Long) As Long
                
Private Declare Function eSocial_DownloadEventos _
                Lib "ACBreSocial32.dll" (ByVal aIdEmpregador As String, _
                                         ByVal aCPFTrabalhador As String, _
                                         ByVal aDataInicial As Date, _
                                         ByVal aDataFinal As Date, _
                                         ByVal buffer As String, _
                                         ByRef bufferLen As Long) As Long
                                         
Private Declare Function eSocial_ObterCertificados _
                Lib "ACBreSocial32.dll" (ByVal buffer As String, _
                                         ByRef bufferSize As Long) As Long

Public Sub InicializarLib(Optional ByVal eArqConfig As String = "", _
                          Optional ByVal eChaveCrypt As String = "")

    SetLibPath
    Dim retorno As Long

    retorno = eSocial_Inicializar(eArqConfig, eChaveCrypt)
    CheckResult retorno
End Sub

Public Sub FinalizarLib()

    Dim retorno As Long

    retorno = eSocial_Finalizar
    CheckResult retorno
End Sub

Public Sub ConfigImportar(ByVal eArqConfig As String)

    Dim retorno As Long

    retorno = eSocial_ConfigImportar(eArqConfig)
    CheckResult (retorno)
    
End Sub
    
Public Function ConfigExportar() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = eSocial_ConfigExportar(buffer, bufferLen)
    CheckResult retorno
    
    ConfigExportar = processResult(buffer, bufferLen)
    
End Function

Public Sub ConfigLer(Optional eArqConfig As String = "")

    Dim retorno As Long

    retorno = eSocial_ConfigLer(eArqConfig)
    CheckResult retorno
End Sub

Public Sub ConfigGravar(Optional eArqConfig As String = "")

    Dim retorno As Long

    retorno = eSocial_ConfigGravar(eArqConfig)
    CheckResult retorno
End Sub

Public Function ConfigLerValor(ByVal eSessao As String, ByVal eChave As String) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = eSocial_ConfigLerValor(eSessao, eChave, buffer, bufferLen)
    CheckResult retorno
    
    ConfigLerValor = processResult(buffer, bufferLen)
End Function

Public Sub ConfigGravarValor(ByVal eSessao As String, _
                             ByVal eChave As String, _
                             ByVal valor As String)

    Dim retorno As Long
    
    retorno = eSocial_ConfigGravarValor(eSessao, eChave, valor)
    CheckResult retorno
End Sub

Public Function Nome() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
        
    retorno = eSocial_Nome(buffer, bufferLen)
    CheckResult retorno
    
    Nome = processResult(buffer, bufferLen)
End Function

Public Function Versao() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = eSocial_Versao(buffer, bufferLen)
    CheckResult retorno
    
    Versao = processResult(buffer, bufferLen)
End Function

Public Sub CriarEventoeSocial(ByVal eArqIni)
    Dim retorno As Long
    
    retorno = eSocial_CriarEventoeSocial(eArqIni)
    CheckResult retorno
End Sub

Public Function EnviareSocial(ByVal aGrupo As Long) As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = eSocial_EnviareSocial(aGrupo, buffer, bufferLen)
    CheckResult retorno
    
    EnviareSocial = processResult(buffer, bufferLen)
End Function

Public Function ConsultareSocial(eProtocolo As String) As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = eSocial_ConsultareSocial(eProtocolo, buffer, bufferLen)
    CheckResult retorno
    
    ConsultareSocial = processResult(buffer, bufferLen)
End Function

Public Sub CriarEnviareSocial(ByVal eArqIni As String, _
                               ByVal aGrupo As Long)
                                                              
    Dim retorno As Long
    
    retorno = eSocial_CriarEnviareSocial(eArqIni, aGrupo)
    CheckResult retorno
End Sub

Public Sub LimpareSocial()
    Dim retorno   As Long
    retorno = eSocial_LimpareSocial
    CheckResult retorno
End Sub

Public Sub CarregarXMLEventoeSocial(ByVal eArquivoOuXML As String)
    Dim retorno As Long
    retorno = eSocial_CarregarXMLEventoeSocial(eArquivoOuXML)
    CheckResult retorno
End Sub

Public Sub SetIDEmpregador(ByVal aIdEmpregador As String)
    Dim retorno As Long
    retorno = eSocial_SetIDEmpregador(aIdEmpregador)
    CheckResult retorno
End Sub

Public Sub SetIDTransmissor(ByVal aIdTransmissor As String)
    Dim retorno As Long
    retorno = eSocial_SetIDTransmissor(aIdTransmissor)
    CheckResult retorno
End Sub

Public Sub SetTipoEmpregador(ByVal aTipoEmpregador As Long)
    Dim retorno As Long
    retorno = eSocial_SetTipoEmpregador(aTipoEmpregador)
    CheckResult retorno
End Sub

Public Sub SetVersaoDF(ByVal sVersao As String)
    Dim retorno As Long
    retorno = eSocial_SetVersaoDF(sVersao)
    CheckResult retorno
End Sub

Public Function ConsultaIdentificadoresEventosEmpregador(ByVal aIdEmpregador As String, _
                                                         ByVal aTipoEvento As Long, _
                                                         ByVal aPeriodoApuracao As Date) As String
                                                         
    Dim retorno As Long
    Dim buffer As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = eSocial_ConsultaIdentificadoresEventosEmpregador(aIdEmpregador, aTipoEvento, aPeriodoApuracao, buffer, bufferLen)
    CheckResult retorno
    
    ConsultaIdentificadoresEventosEmpregador = processResult(buffer, bufferLen)
End Function

Public Function ConsultaIdentificadoresEventosTabela(ByVal aIdEmpregador As String, _
                                                     ByVal aTipoEvento As Long, _
                                                     ByVal aChave As String, _
                                                     ByVal aDataInicial As Date, _
                                                     ByVal aDataFinal As Date) As String
                                                     
    Dim retorno As Long
    Dim buffer As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = eSocial_ConsultaIdentificadoresEventosTabela(aIdEmpregador, aTipoEvento, aChave, aDataInicial, aDataFinal, buffer, bufferLen)
    CheckResult retorno
    
    ConsultaIdentificadoresEventosTabela = processResult(buffer, bufferLen)
End Function

Public Function ConsultaIdentificadoresEventosTrabalhador(ByVal aIdEmpregador As String, _
                                                          ByVal aCPFTrabalhador As String, _
                                                          ByVal aDataInicial As Date, _
                                                          ByVal aDataFinal As Date) As String
    Dim retorno As Long
    Dim buffer As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = eSocial_ConsultaIdentificadoresEventosTrabalhador(aIdEmpregador, aCPFTrabalhador, aDataInicial, aDataFinal, buffer, bufferLen)
    CheckResult retorno
    
    ConsultaIdentificadoresEventosTrabalhador = processResult(buffer, bufferLen)
End Function

Public Function DownloadEventos(ByVal aIdEmpregador As String, _
                                ByVal aCPFTrabalhador As String, _
                                ByVal aDataInicial As Date, _
                                ByVal aDataFinal As Date) As String
    Dim retorno As Long
    Dim buffer As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = eSocial_DownloadEventos(aIdEmpregador, aCPFTrabalhador, aDataInicial, aDataFinal, buffer, bufferLen)
    CheckResult retorno
    
    DownloadEventos = processResult(buffer, bufferLen)
End Function

Public Function ObterCertificados() As String()
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = eSocial_ObterCertificados(buffer, bufferLen)
    CheckResult retorno
    
    ObterCertificados = Split(processResult(buffer, bufferLen), vbCrLf)
End Function

Private Sub CheckResult(ByVal Resultado As Long)
    
    If Resultado >= 0 Then Exit Sub
         
    Dim buffer As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    eSocial_UltimoRetorno buffer, bufferLen
    
    Err.Raise Resultado, "ACBreSocial", processResult(buffer, bufferLen)
End Sub

Private Function processResult(ByRef buffer As String, ByRef bufferLen As Long) As String
    
    If bufferLen > BUFFER_LENGTH Then
        buffer = String$(bufferLen, " ")
        eSocial_UltimoRetorno buffer, bufferLen
    End If

    processResult = Trim$(FromUTF8(buffer))
End Function
