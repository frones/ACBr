VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ACBrPosPrinter"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Const BUFFER_LENGTH As Long = 1024

Private Declare Function POS_Inicializar _
                Lib "ACBrPosPrinter32.dll" (ByVal eArqConfig As String, _
                                            ByVal eChaveCrypt As String) As Long
                   
Private Declare Function POS_Finalizar Lib "ACBrPosPrinter32.dll" () As Long

Private Declare Function POS_Nome _
                Lib "ACBrPosPrinter32.dll" (ByVal buffer As String, _
                                            ByRef bufferLen As Long) As Long
                   
Private Declare Function POS_Versao _
                Lib "ACBrPosPrinter32.dll" (ByVal buffer As String, _
                                            ByRef bufferLen As Long) As Long

Private Declare Function POS_UltimoRetorno _
                Lib "ACBrPosPrinter32.dll" (ByVal buffer As String, _
                                            ByRef bufferLen As Long) As Long
                                            
Private Declare Function POS_ImportarConfig _
                Lib "ACBrPosPrinter32.dll" (ByVal eArqConfig As String) As Long
                   
Private Declare Function POS_ConfigLer _
                Lib "ACBrPosPrinter32.dll" (ByVal eArqConfig As String) As Long

Private Declare Function POS_ConfigGravar _
                Lib "ACBrPosPrinter32.dll" (ByVal eArqConfig As String) As Long
                   
Private Declare Function POS_ConfigLerValor _
                Lib "ACBrPosPrinter32.dll" (ByVal eSessao As String, _
                                            ByVal eChave As String, _
                                            ByVal buffer As String, _
                                            ByRef bufferLen As Long) As Long

Private Declare Function POS_ConfigGravarValor _
                Lib "ACBrPosPrinter32.dll" (ByVal eSessao As String, _
                                            ByVal eChave As String, _
                                            ByVal valor As String) As Long
                    
Private Declare Function POS_Ativar Lib "ACBrPosPrinter32.dll" () As Long

Private Declare Function POS_Desativar Lib "ACBrPosPrinter32.dll" () As Long

Private Declare Function POS_Imprimir _
                Lib "ACBrPosPrinter32.dll" (ByVal aString As String, _
                                            ByVal pulaLinha As Boolean, _
                                            ByVal decodificarTags As Boolean, _
                                            ByVal codificarPagina As Boolean, _
                                            ByVal copias As Long) As Long

Private Declare Function POS_ImprimirLinha _
                Lib "ACBrPosPrinter32.dll" (ByVal aString As String) As Long
                   
Private Declare Function POS_ImprimirCMD _
                Lib "ACBrPosPrinter32.dll" (ByVal aString As String) As Long
                   
Private Declare Function POS_ImprimirTags Lib "ACBrPosPrinter32.dll" () As Long

Private Declare Function POS_ImprimirImagemArquivo Lib "ACBrPosPrinter32.dll" (ByVal aPath As String) As Long

Private Declare Function POS_GravarLogoArquivo Lib "ACBrPosPrinter32.dll" (ByVal aPath As String, _
                                                                           ByVal nAKC1 As Long, _
                                                                           ByVal nAKC2 As Long) As Long
                                                                           
Private Declare Function POS_ImprimirLogo Lib "ACBrPosPrinter32.dll" (ByVal nAKC1 As Long, _
                                                                           ByVal nAKC2 As Long, _
                                                                           ByVal nFatorX As Long, _
                                                                           ByVal nFatorY As Long) As Long
                                                                           
Private Declare Function POS_ApagarLogo Lib "ACBrPosPrinter32.dll" (ByVal nAKC1 As Long, _
                                                                      ByVal nAKC2 As Long) As Long

Private Declare Function POS_TxRx _
                Lib "ACBrPosPrinter32.dll" (ByVal aString As String, _
                                            ByVal bytesToRead As Byte, _
                                            ByVal timeOut As Long, _
                                            ByVal waitForTerminator As Boolean, _
                                            ByVal buffer As String, _
                                            ByRef bufferLen As Long) As Long
                    
Private Declare Function POS_Zerar Lib "ACBrPosPrinter32.dll" () As Long

Private Declare Function POS_InicializarPos Lib "ACBrPosPrinter32.dll" () As Long

Private Declare Function POS_Reset Lib "ACBrPosPrinter32.dll" () As Long

Private Declare Function POS_PularLinhas _
                Lib "ACBrPosPrinter32.dll" (ByVal numLinhas As Long) As Long
                
Private Declare Function POS_CortarPapel _
                Lib "ACBrPosPrinter32.dll" (ByVal parcial As Boolean) As Long

Private Declare Function POS_AbrirGaveta Lib "ACBrPosPrinter32.dll" () As Long

Private Declare Function POS_LerInfoImpressora _
                Lib "ACBrPosPrinter32.dll" (ByVal buffer As String, _
                                            ByRef bufferLen As Long) As Long
                                            
Private Declare Function POS_LerStatusImpressora _
                Lib "ACBrPosPrinter32.dll" (ByVal tentativas As Long, _
                                            ByRef status As Long) As Long
                                            
Private Declare Function POS_RetornarTags _
                Lib "ACBrPosPrinter32.dll" (ByVal IncluiAjuda As Boolean, _
                                            ByVal buffer As String, _
                                            ByRef bufferLen As Long) As Long
                                            
Private Declare Function POS_AcharPortas _
                Lib "ACBrPosPrinter32.dll" (ByVal buffer As String, _
                                            ByRef bufferLen As Long) As Long
                   
Public Sub InicializarLib(Optional ByVal eArqConfig As String = "", _
                          Optional ByVal eChaveCrypt As String = "")

    Dim retorno As Long

    retorno = POS_Inicializar(eArqConfig, eChaveCrypt)
    CheckResult (retorno)
End Sub

Public Sub FinalizarLib()

    Dim retorno As Long

    retorno = POS_Finalizar
    CheckResult (retorno)
End Sub

Public Sub ImportarConfig(ByVal eArqConfig As String)

    Dim retorno As Long

    retorno = POS_ImportarConfig(eArqConfig)
    CheckResult (retorno)
End Sub

Public Sub ConfigLer(Optional ByVal eArqConfig As String = "")

    Dim retorno As Long

    retorno = POS_ConfigLer(eArqConfig)
    CheckResult (retorno)
End Sub

Public Sub ConfigGravar(Optional ByVal eArqConfig As String = "")

    Dim retorno As Long

    retorno = POS_ConfigGravar(eArqConfig)
    CheckResult (retorno)
End Sub

Public Function ConfigLerValor(ByVal eSessao As String, ByVal eChave As String) As String

    Dim retorno   As Long

    Dim buffer    As String

    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen + 1, " ")
    
    retorno = POS_ConfigLerValor(eSessao, eChave, buffer, bufferLen)
    CheckResult (retorno)
    
    ConfigLerValor = processResult(buffer, bufferLen)
End Function

Public Sub ConfigGravarValor(ByVal eSessao As String, _
                             ByVal eChave As String, _
                             ByVal valor As String)

    Dim retorno As Long
    
    retorno = POS_ConfigGravarValor(eSessao, eChave, valor)
    CheckResult (retorno)
End Sub

Public Function Nome() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
        
    retorno = POS_Nome(buffer, bufferLen)
    CheckResult (retorno)
    
    Nome = processResult(buffer, bufferLen)
End Function

Public Function Versao() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = POS_Versao(buffer, bufferLen)
    CheckResult (retorno)
    
    Versao = processResult(buffer, bufferLen)
End Function

Public Sub Ativar()

    Dim retorno As Long

    retorno = POS_Ativar
    CheckResult (retorno)
End Sub

Public Sub Desativar()

    Dim retorno As Long

    retorno = POS_Desativar
    CheckResult (retorno)
End Sub

Public Sub Imprimir(ByVal aString As String, _
                    Optional ByVal pulaLinha As Boolean = False, _
                    Optional ByVal decodificarTags As Boolean = True, _
                    Optional ByVal codificarPagina As Boolean = True, _
                    Optional ByVal copias As Long = 1)

    Dim retorno As Long

    retorno = POS_Imprimir(aString, pulaLinha, decodificarTags, codificarPagina, copias)
    CheckResult (retorno)
End Sub

Public Sub ImprimirLinha(ByVal aString As String)

    Dim retorno As Long

    retorno = POS_ImprimirLinha(aString)
    CheckResult (retorno)
End Sub

Public Sub ImprimirCMD(ByVal aString As String)

    Dim retorno As Long

    retorno = POS_ImprimirCMD(aString)
    CheckResult (retorno)
End Sub

Public Sub ImprimirTags()

    Dim retorno As Long

    retorno = POS_ImprimirTags
    CheckResult (retorno)
End Sub

Public Sub ImprimirImagemArquivo(ByVal aPath As String)

    Dim retorno As Long

    retorno = POS_ImprimirImagemArquivo(aPath)
    CheckResult (retorno)
End Sub

Public Sub GravarLogoArquivo(ByVal aPath As String, _
                             Optional ByVal nAKC1 As Long = -1, _
                             Optional ByVal nAKC2 As Long = -1)

    Dim retorno As Long

    retorno = POS_GravarLogoArquivo(aPath, nAKC1, nAKC2)
    CheckResult (retorno)
End Sub

Public Sub ImprimirLogo(Optional ByVal nAKC1 As Long = -1, _
                            Optional ByVal nAKC2 As Long = -1, _
                            Optional ByVal nFatorX As Long = -1, _
                            Optional ByVal nFatorY As Long = -1)

    Dim retorno As Long

    retorno = POS_ImprimirLogo(nAKC1, nAKC2, nFatorX, nFatorY)
    CheckResult (retorno)
End Sub

Public Sub ApagarLogo(Optional ByVal nAKC1 As Long = -1, _
                            Optional ByVal nAKC2 As Long = -1)

    Dim retorno As Long

    retorno = POS_ApagarLogo(nAKC1, nAKC2)
    CheckResult (retorno)
End Sub

Public Function TxRx(ByVal aString As String, _
                     Optional ByVal bytesToRead As Byte = 1, _
                     Optional ByVal timeOut As Long = 500, _
                     Optional ByVal waitForTerminator As Boolean = False) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = POS_TxRx(aString, bytesToRead, timeOut, waitForTerminator, buffer, bufferLen)
    CheckResult (retorno)
    
    TxRx = processResult(buffer, bufferLen)
End Function

Public Sub Zerar()

    Dim retorno As Long

    retorno = POS_Zerar
    CheckResult (retorno)
End Sub

Public Sub Inicializar()

    Dim retorno As Long

    retorno = POS_InicializarPos
    CheckResult (retorno)
End Sub

Public Sub Reset()

    Dim retorno As Long

    retorno = POS_Reset
    CheckResult (retorno)
End Sub

Public Sub PularLinhas(ByVal numLinhas As Long)

    Dim retorno As Long

    retorno = POS_PularLinhas(numLinhas)
    CheckResult (retorno)
End Sub

Public Sub CortarPapel(ByVal parcial As Boolean)

    Dim retorno As Long

    retorno = POS_CortarPapel(parcial)
    CheckResult (retorno)
End Sub

Public Sub AbrirGaveta()

    Dim retorno As Long

    retorno = POS_AbrirGaveta
    CheckResult (retorno)
End Sub

Public Function LerInfoImpressora() As String

    Dim retorno As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")

    retorno = POS_LerInfoImpressora(buffer, bufferLen)
    CheckResult (retorno)
    
    LerInfoImpressora = processResult(buffer, bufferLen)
End Function

Public Function LerStatusImpressora(Optional ByVal tentativas As Long = 1) As ACBrPosTipoStatus

    Dim retorno As Long
    Dim status As Long

    status = 0
    retorno = POS_LerStatusImpressora(tentativas, status)
    CheckResult (retorno)
    
    LerStatusImpressora = CInt(status)
End Function

Public Function RetornarTags(Optional ByVal InclueAjuda As Boolean = True) As String()

    Dim retorno As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")

    retorno = POS_RetornarTags(InclueAjuda, buffer, bufferLen)
    CheckResult (retorno)
    
    RetornarTags = Split(Trim$(FromUTF8(buffer)), "|")
End Function

Public Function AcharPortas() As String()

    Dim retorno As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")

    retorno = POS_AcharPortas(buffer, bufferLen)
    CheckResult (retorno)
    
    AcharPortas = Split(Trim$(FromUTF8(buffer)), "|")
End Function

Private Sub CheckResult(ByVal Resultado As Long)
    
    If Resultado = 0 Then Exit Sub
         
    Dim buffer As String

    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    POS_UltimoRetorno buffer, bufferLen
    
    Err.Raise Resultado, "ACBrETQ", processResult(buffer, bufferLen)
End Sub

Private Function processResult(ByRef buffer As String, ByRef bufferLen As Long) As String
    
    If bufferLen > BUFFER_LENGTH Then
        buffer = String$(bufferLen, " ")
        POS_UltimoRetorno buffer, bufferLen
    End If

    processResult = Trim$(FromUTF8(buffer))
End Function
