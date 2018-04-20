Attribute VB_Name = "ACBrPosPrinter"
Option Explicit

' UTF-8 Code Page'Sys call to convert multiple byte chars to a charPrivate
Const CP_UTF8 As Long = 65001

Private Declare Function MultiByteToWideChar _
                Lib "kernel32" (ByVal CodePage As Long, _
                                ByVal dwFlags As Long, _
                                ByVal lpMultiByteStr As Long, _
                                ByVal cchMultiByte As Long, _
                                ByVal lpWideCharStr As Long, _
                                ByVal cchWideChar As Long) As Long
                                
Public Declare Function POS_Inicializar _
                Lib "ACBrPosPrinter32.dll" (ByVal eArqConfig As String, _
                                            ByVal eChaveCrypt As String) As Long
                   
Public Declare Function POS_Finalizar Lib "ACBrPosPrinter32.dll" () As Long

Public Declare Function POS_Nome _
                Lib "ACBrPosPrinter32.dll" (ByVal buffer As String, _
                                            ByRef bufferLen As Long) As Long
                   
Public Declare Function POS_Versao _
                Lib "ACBrPosPrinter32.dll" (ByVal buffer As String, _
                                            ByRef bufferLen As Long) As Long

Public Declare Function POS_UltimoRetorno _
                Lib "ACBrPosPrinter32.dll" (ByVal buffer As String, _
                                            ByRef bufferLen As Long) As Long
                   
Public Declare Function POS_ConfigLer _
                Lib "ACBrPosPrinter32.dll" (ByVal eArqConfig As String) As Long

Public Declare Function POS_ConfigGravar _
                Lib "ACBrPosPrinter32.dll" (ByVal eArqConfig As String) As Long
                   
Public Declare Function POS_ConfigLerValor _
                Lib "ACBrPosPrinter32.dll" (ByVal eSessao As String, _
                                            ByVal eChave As String, _
                                            ByVal buffer As String, _
                                            ByRef bufferLen As Long) As Long

Public Declare Function POS_ConfigGravarValor _
                Lib "ACBrPosPrinter32.dll" (ByVal eSessao As String, _
                                            ByVal eChave As String, _
                                            ByVal valor As String) As Long
                    
Public Declare Function POS_Ativar Lib "ACBrPosPrinter32.dll" () As Long

Public Declare Function POS_Desativar Lib "ACBrPosPrinter32.dll" () As Long

Public Declare Function POS_Imprimir _
                Lib "ACBrPosPrinter32.dll" (ByVal aString As String, _
                                            ByVal pulaLinha As Boolean, _
                                            ByVal decodificarTags As Boolean, _
                                            ByVal codificarPagina As Boolean, _
                                            ByVal copias As Long) As Long

Public Declare Function POS_ImprimirLinha _
                Lib "ACBrPosPrinter32.dll" (ByVal aString As String) As Long
                   
Public Declare Function POS_ImprimirCMD _
                Lib "ACBrPosPrinter32.dll" (ByVal aString As String) As Long
                   
Public Declare Function POS_ImprimirTags Lib "ACBrPosPrinter32.dll" () As Long

Public Declare Function POS_TxRx _
                Lib "ACBrPosPrinter32.dll" (ByVal aString As String, _
                                            ByVal bytesToRead As Byte, _
                                            ByVal timeOut As Long, _
                                            ByVal waitForTerminator As Boolean, _
                                            ByVal buffer As String, _
                                            ByRef bufferLen As Long) As Long
                    
Public Declare Function POS_Zerar Lib "ACBrPosPrinter32.dll" () As Long

Public Declare Function POS_InicializarPos Lib "ACBrPosPrinter32.dll" () As Long

Public Declare Function POS_Reset Lib "ACBrPosPrinter32.dll" () As Long

Public Declare Function POS_PularLinhas _
                Lib "ACBrPosPrinter32.dll" (ByVal numLinhas As Long) As Long
                
Public Declare Function POS_CortarPapel _
                Lib "ACBrPosPrinter32.dll" (ByVal parcial As Boolean) As Long

Public Declare Function POS_AbrirGaveta Lib "ACBrPosPrinter32.dll" () As Long

Public Declare Function POS_LerInfoImpressora _
                Lib "ACBrPosPrinter32.dll" (ByVal buffer As String, _
                                            ByRef bufferLen As Long) As Long
                                            
Public Declare Function POS_LerStatusImpressora _
                Lib "ACBrPosPrinter32.dll" (ByVal tentativas As Long, _
                                            ByRef status As Long) As Long
                                            
Public Declare Function POS_RetornarTags _
                Lib "ACBrPosPrinter32.dll" (ByVal buffer As String, _
                                            ByRef bufferLen As Long) As Long

Public Enum ACBrPosPrinterModelo
    Texto = 0
    EscPosEpson = 1
    EscBematech = 2
    EscDaruma = 3
    EscVox = 3
    EscDiebold = 4
    EscEpsonP2 = 5
End Enum

Public Enum ACBrPosTipoStatus
    None = 0
    Erro = 2 ^ 0
    NaoSerial = 2 ^ 1
    PoucoPapel = 2 ^ 2
    SemPapel = 2 ^ 3
    GavetaAberta = 2 ^ 4
    Imprimindo = 2 ^ 5
    OffLine = 2 ^ 6
    TampaAberta = 2 ^ 7
    ErroLeitura = 2 ^ 8
End Enum

Public Enum PosPaginaCodigo
    None = 0
    pc437 = 1
    pc850 = 2
    pc852 = 3
    pc860 = 4
    pcUTF8 = 5
    pc1252 = 6
End Enum

Public Enum SerialHandShake
    Nenhum = 0
    XON_XOFF = 1
    RTS_CTS = 2
    DTR_DSR = 3
End Enum

Public Enum SerialParity
    None = 78
    Odd = 79
    Even = 69
    Mark = 77
    Space = 83
End Enum

Public Enum SerialStopBytes
    One = 0
    OnePointFive = 1
    Two = 2
End Enum

Public Sub CheckResult(ByVal Resultado As Long)
    
    If Resultado = 0 Then Exit Sub
         
    Dim buffer As String

    Dim bufferLen As Long

    bufferLen = 256
    buffer = String$(bufferLen, " ")
    POS_UltimoRetorno buffer, bufferLen
    
    If bufferLen > 256 Then
        POS_UltimoRetorno buffer, bufferLen
    End If

    Err.Raise Resultado, "ACBrPosPrinter", Trim$(FromUTF8(buffer))
End Sub

Public Function FromUTF8(ByRef utf8STR As String) As String
    
    Dim length As Long

    length = Len(utf8STR)
    
    Dim UTF8() As Byte

    UTF8 = StrConv(utf8STR, vbFromUnicode)
    
    Dim lDataLength As Long
    
    ' Get the length of the data.
    lDataLength = MultiByteToWideChar(CP_UTF8, 0, VarPtr(UTF8(0)), length, 0, 0)
    
    ' Create array big enough
    FromUTF8 = String$(lDataLength, 0)
    
    MultiByteToWideChar CP_UTF8, 0, VarPtr(UTF8(0)), length, StrPtr(FromUTF8), lDataLength
End Function
