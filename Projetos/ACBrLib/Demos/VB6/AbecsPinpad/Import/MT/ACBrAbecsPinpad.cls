VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ACBrAbecsPinpad"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Private libHandler As Long

Const BUFFER_LENGTH As Long = 1024

Private Declare Function AbecsPinpad_Inicializar _
                Lib "ACBrAbecsPinpad32.dll" (ByRef libHandler As Long, _
                                            ByVal eArqConfig As String, _
                                            ByVal eChaveCrypt As String) As Long
                   
Private Declare Function AbecsPinpad_Finalizar Lib "ACBrAbecsPinpad32.dll" (ByVal libHandler As Long) As Long

Private Declare Function AbecsPinpad_Nome _
                Lib "ACBrAbecsPinpad32.dll" (ByVal libHandler As Long, _
                                            ByVal buffer As String, _
                                            ByRef bufferLen As Long) As Long
                   
Private Declare Function AbecsPinpad_Versao _
                Lib "ACBrAbecsPinpad32.dll" (ByVal libHandler As Long, _
                                            ByVal buffer As String, _
                                            ByRef bufferLen As Long) As Long

Private Declare Function AbecsPinpad_UltimoRetorno _
                Lib "ACBrAbecsPinpad32.dll" (ByVal libHandler As Long, _
                                            ByVal buffer As String, _
                                            ByRef bufferLen As Long) As Long
                                            
Private Declare Function AbecsPinpad_ConfigImportar _
                Lib "ACBrAbecsPinpad32.dll" (ByVal libHandler As Long, _
                                            ByVal eArqConfig As String) As Long
                
Private Declare Function AbecsPinpad_ConfigExportar _
                Lib "ACBrAbecsPinpad32.dll" (ByVal libHandler As Long, _
                                            ByVal buffer As String, _
                                            ByRef bufferLen As Long) As Long
                   
Private Declare Function AbecsPinpad_ConfigLer _
                Lib "ACBrAbecsPinpad32.dll" (ByVal libHandler As Long, _
                                            ByVal eArqConfig As String) As Long

Private Declare Function AbecsPinpad_ConfigGravar _
                Lib "ACBrAbecsPinpad32.dll" (ByVal libHandler As Long, _
                                             ByVal eArqConfig As String) As Long
                   
Private Declare Function AbecsPinpad_ConfigLerValor _
                Lib "ACBrAbecsPinpad32.dll" (ByVal libHandler As Long, _
                                            ByVal eSessao As String, _
                                            ByVal eChave As String, _
                                            ByVal buffer As String, _
                                            ByRef bufferLen As Long) As Long

Private Declare Function AbecsPinpad_ConfigGravarValor _
                Lib "ACBrAbecsPinpad32.dll" (ByVal libHandler As Long, _
                                            ByVal eSessao As String, _
                                            ByVal eChave As String, _
                                            ByVal Valor As String) As Long
                    
Private Declare Function AbecsPinpad_Ativar _
                Lib "ACBrAbecsPinpad32.dll" (ByVal libHandler As Long) As Long

Private Declare Function AbecsPinpad_Desativar _
                Lib "ACBrAbecsPinpad32.dll" (ByVal libHandler As Long) As Long

Private Declare Function AbecsPinpad_OPN _
                Lib "ACBrAbecsPinpad32.dll" (ByVal libHandler As Long) As Long

Private Declare Function AbecsPinpad_CLO _
                Lib "ACBrAbecsPinpad32.dll" (ByVal libHandler As Long, _
                                            ByVal sMensagem As String) As Long

Private Declare Function AbecsPinpad_CLX _
                Lib "ACBrAbecsPinpad32.dll" (ByVal libHandler As Long, _
                                            ByVal sMensagemOuNomeImagem As String) As Long
                   
Private Declare Function AbecsPinpad_GIX _
                Lib "ACBrAbecsPinpad32.dll" (ByVal libHandler As Long, _
                                             ByVal PP_DATA As String, _
                                             ByVal buffer As String, _
                                             ByRef bufferLen As Long) As Long
                                                                      
Private Declare Function AbecsPinpad_GIN _
                Lib "ACBrAbecsPinpad32.dll" (ByVal libHandler As Long, _
                                             ByVal GIN_ACQIDX As Long, _
                                             ByVal buffer As String, _
                                             ByRef bufferLen As Long) As Long
                                                                      
Private Declare Function AbecsPinpad_PinPadCapabilities _
                Lib "ACBrAbecsPinpad32.dll" (ByVal libHandler As Long, _
                                             ByVal buffer As String, _
                                             ByRef bufferLen As Long) As Long

Private Declare Function AbecsPinpad_DSP _
                Lib "ACBrAbecsPinpad32.dll" (ByVal libHandler As Long, _
                                             ByVal sMensagem As String) As Long

Private Declare Function AbecsPinpad_DEX _
                Lib "ACBrAbecsPinpad32.dll" (ByVal libHandler As Long, _
                                             ByVal sMensagem As String) As Long

Private Declare Function AbecsPinpad_GKY _
                Lib "ACBrAbecsPinpad32.dll" (ByVal libHandler As Long) As Long

Private Declare Function AbecsPinpad_RMC _
                Lib "ACBrAbecsPinpad32.dll" (ByVal libHandler As Long, _
                                             ByVal sMensagemRMC As String) As Long
                

Private Declare Function AbecsPinpad_GCD _
                Lib "ACBrAbecsPinpad32.dll" (ByVal libHandler As Long, _
                                             ByVal aMSGIDX As Long, _
                                             ByVal aTimeOut As Long, _
                                             ByVal buffer As String, _
                                             ByRef bufferLen As Long) As Long
                
Private Declare Function AbecsPinpad_CEX _
                Lib "ACBrAbecsPinpad32.dll" (ByVal libHandler As Long, _
                                             ByVal VerifyKey As Boolean, _
                                             ByVal VerifyMagnetic As Boolean, _
                                             ByVal VerifyICCInsertion As Boolean, _
                                             ByVal VerifyICCRemoval As Boolean, _
                                             ByVal VerifyCTLSPresence As Boolean, _
                                             ByVal aTimeOut As Long, _
                                             ByVal buffer As String, _
                                             ByRef bufferLen As Long) As Long
                                                                                          
Private Declare Function AbecsPinpad_MNU _
                Lib "ACBrAbecsPinpad32.dll" (ByVal libHandler As Long, _
                                             ByVal sMNUOPT As String, _
                                             ByVal sDSPMSG As String, _
                                             ByVal aTimeOut As Long, _
                                             ByVal buffer As String, _
                                             ByRef bufferLen As Long) As Long
                                             
Private Declare Function AbecsPinpad_LoadMedia _
                Lib "ACBrAbecsPinpad32.dll" (ByVal libHandler As Long, _
                                             ByVal sCaminhoImagem As String, _
                                             ByVal aTipoImagem As Long, _
                                             ByVal buffer As String, _
                                             ByRef bufferLen As Long) As Long
                                             
Private Declare Function AbecsPinpad_LMF _
                Lib "ACBrAbecsPinpad32.dll" (ByVal libHandler As Long, _
                                             ByVal buffer As String, _
                                             ByRef bufferLen As Long) As Long

Private Declare Function AbecsPinpad_DSI _
                Lib "ACBrAbecsPinpad32.dll" (ByVal libHandler As Long, _
                                             ByVal sNomeArquivo As String) As Long
                     
Private Declare Function AbecsPinpad_DMF _
                Lib "ACBrAbecsPinpad32.dll" (ByVal libHandler As Long, _
                                             ByVal sNomeArquivo As String) As Long

Public Sub InicializarLib(Optional ByVal eArqConfig As String = "", _
                          Optional ByVal eChaveCrypt As String = "")
                          
    SetLibPath

    Dim retorno As Long

    retorno = AbecsPinpad_Inicializar(libHandler, eArqConfig, eChaveCrypt)
    CheckResult (retorno)
End Sub

Public Sub FinalizarLib()

    Dim retorno As Long

    retorno = AbecsPinpad_Finalizar(libHandler)
    CheckResult (retorno)
End Sub

Public Sub ConfigImportar(ByVal eArqConfig As String)

    Dim retorno As Long

    retorno = AbecsPinpad_ConfigImportar(libHandler, eArqConfig)
    CheckResult (retorno)
End Sub

Public Function ConfigExportar() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = AbecsPinpad_ConfigExportar(libHandler, buffer, bufferLen)
    CheckResult retorno
    
    ConfigExportar = processResult(buffer, bufferLen)
    
End Function

Public Sub ConfigLer(Optional ByVal eArqConfig As String = "")

    Dim retorno As Long

    retorno = AbecsPinpad_ConfigLer(libHandler, eArqConfig)
    CheckResult (retorno)
End Sub

Public Sub ConfigGravar(Optional ByVal eArqConfig As String = "")

    Dim retorno As Long

    retorno = AbecsPinpad_ConfigGravar(libHandler, eArqConfig)
    CheckResult (retorno)
End Sub

Public Function ConfigLerValor(ByVal eSessao As String, ByVal eChave As String) As String

    Dim retorno   As Long

    Dim buffer    As String

    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen + 1, " ")
    
    retorno = AbecsPinpad_ConfigLerValor(libHandler, eSessao, eChave, buffer, bufferLen)
    CheckResult (retorno)
    
    ConfigLerValor = processResult(buffer, bufferLen)
End Function

Public Sub ConfigGravarValor(ByVal eSessao As String, _
                             ByVal eChave As String, _
                             ByVal Valor As String)

    Dim retorno As Long
    
    retorno = AbecsPinpad_ConfigGravarValor(libHandler, eSessao, eChave, Valor)
    CheckResult (retorno)
End Sub

Public Function Nome() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
        
    retorno = AbecsPinpad_Nome(libHandler, buffer, bufferLen)
    CheckResult (retorno)
    
    Nome = processResult(buffer, bufferLen)
End Function

Public Function Versao() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = AbecsPinpad_Versao(libHandler, buffer, bufferLen)
    CheckResult (retorno)
    
    Versao = processResult(buffer, bufferLen)
End Function

Public Sub Ativar()

    Dim retorno As Long

    retorno = AbecsPinpad_Ativar(libHandler)
    CheckResult (retorno)
End Sub

Public Sub Desativar()

    Dim retorno As Long

    retorno = AbecsPinpad_Desativar(libHandler)
    CheckResult (retorno)
End Sub

Public Sub OPN()
    
    Dim retorno As Long
    
    retorno = AbecsPinpad_OPN(libHandler)
    CheckResult (retorno)
    
End Sub

Public Sub CLO(ByVal sMensagem As String)
    
    Dim retorno As Long
    
    retorno = AbecsPinpad_CLO(libHandler, sMensagem)
    CheckResult (retorno)
    
End Sub

Public Sub CLX(ByVal sMensagemOuNomeImagem As String)

    Dim retorno As Long
    
    retorno = AbecsPinpad_CLX(libHandler, sMensagemOuNomeImagem)
    CheckResult (retorno)

End Sub

Public Function GIX(ByVal PP_DATA As String) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = AbecsPinpad_GIX(libHandler, PP_DATA, buffer, bufferLen)
    CheckResult (retorno)
    
    GIX = processResult(buffer, bufferLen)

End Function

Public Function GIN(ByVal GIN_ACQIDX As Long) As String
    
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = AbecsPinpad_GIN(libHandler, GIN_ACQIDX, buffer, bufferLen)
    CheckResult (retorno)
    
    GIN = processResult(buffer, bufferLen)
    
End Function

Public Function PinPadCapabilities() As String
    
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = AbecsPinpad_PinPadCapabilities(libHandler, buffer, bufferLen)
    CheckResult (retorno)
    
    PinPadCapabilities = processResult(buffer, bufferLen)
    
End Function

Public Sub DSP(ByVal sMensagem As String)
    
    Dim retorno As Long
    
    retorno = AbecsPinpad_DSP(libHandler, sMensagem)
    CheckResult (retorno)
    
End Sub

Public Sub DEX(ByVal sMensagem As String)
    
    Dim retorno As Long
    
    retorno = AbecsPinpad_DEX(libHandler, sMensagem)
    CheckResult (retorno)
    
End Sub

Public Sub GKY()
    
    Dim retorno As Long
    
    retorno = AbecsPinpad_GKY(libHandler)
    CheckResult (retorno)
    
End Sub

Public Sub RMC(ByVal sMensagemRMC As String)
    
    Dim retorno As Long
    
    retorno = AbecsPinpad_RMC(libHandler, sMensagemRMC)
    CheckResult (retorno)
    
End Sub

Public Function GCD(ByVal aMSGIDX As Long, _
                    ByVal aTimeOut As Long) As String
  
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
  
    retorno = AbecsPinpad_GCD(libHandler, aMSGIDX, aTimeOut, buffer, bufferLen)
    CheckResult (retorno)
  
    GCD = processResult(buffer, bufferLen)
  
End Function

Public Function CEX(ByVal VerifyKey As Boolean, _
                    ByVal VerifyMagnetic As Boolean, _
                    ByVal VerifyICCInsertion As Boolean, _
                    ByVal VerifyICCRemoval As Boolean, _
                    ByVal VerifyCTLSPresence As Boolean, _
                    ByVal aTimeOut As Long) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = AbecsPinpad_CEX(libHandler, VerifyKey, VerifyMagnetic, VerifyICCInsertion, VerifyICCRemoval, VerifyCTLSPresence, aTimeOut, buffer, bufferLen)
    CheckResult (retorno)
    
    CEX = processResult(buffer, bufferLen)

End Function

Public Function MNU(ByVal sMNUOPT As String, _
                    ByVal sDSPMSG As String, _
                    ByVal aTimeOut As Long) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = AbecsPinpad_MNU(libHandler, sMNUOPT, sDSPMSG, aTimeOut, buffer, bufferLen)
    CheckResult (retorno)
    
    MNU = processResult(buffer, bufferLen)
    
End Function

Public Function LoadMedia(ByVal sCaminhoImagem As String, _
                          ByVal aTipoImagem As Long) As String
    
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = AbecsPinpad_LoadMedia(libHandler, sCaminhoImagem, aTipoImagem, buffer, bufferLen)
    CheckResult (retorno)
    
    LoadMedia = processResult(buffer, bufferLen)

End Function

Public Function LMF() As String
    
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = AbecsPinpad_LMF(libHandler, buffer, bufferLen)
    
End Function

Public Sub DSI(ByVal sNomeArquivo As String)

    Dim retorno As Long
    
    retorno = AbecsPinpad_DSI(libHandler, sNomeArquivo)
    CheckResult (retorno)

End Sub

Public Sub DMF(ByVal sNomeArquivo As String)

    Dim retorno As Long
    
    retorno = AbecsPinpad_DMF(libHandler, sNomeArquivo)
    CheckResult (retorno)

End Sub

Private Sub CheckResult(ByVal Resultado As Long)
    
    If Resultado = 0 Then Exit Sub
         
    Dim buffer As String

    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    AbecsPinpad_UltimoRetorno libHandler, buffer, bufferLen
    
    Err.Raise Resultado, "ACBrAbecsPinpad", processResult(buffer, bufferLen)

End Sub

Private Function processResult(ByRef buffer As String, ByRef bufferLen As Long) As String
    
    If bufferLen > BUFFER_LENGTH Then
        buffer = String$(bufferLen, " ")
        AbecsPinpad_UltimoRetorno libHandler, buffer, bufferLen
    End If

    processResult = Trim$(FromUTF8(buffer))
End Function
