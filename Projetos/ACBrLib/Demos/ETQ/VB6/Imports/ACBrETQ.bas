Attribute VB_Name = "ACBrETQ"
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
                                
Public Declare Function ETQ_Inicializar _
                Lib "ACBrETQ32.dll" (ByVal eArqConfig As String, _
                                            ByVal eChaveCrypt As String) As Long
                   
Public Declare Function ETQ_Finalizar Lib "ACBrETQ32.dll" () As Long

Public Declare Function ETQ_Nome _
                Lib "ACBrETQ32.dll" (ByVal buffer As String, _
                                            ByRef bufferLen As Long) As Long
                   
Public Declare Function ETQ_Versao _
                Lib "ACBrETQ32.dll" (ByVal buffer As String, _
                                            ByRef bufferLen As Long) As Long

Public Declare Function ETQ_UltimoRetorno _
                Lib "ACBrETQ32.dll" (ByVal buffer As String, _
                                            ByRef bufferLen As Long) As Long
                   
Public Declare Function ETQ_ConfigLer _
                Lib "ACBrETQ32.dll" (ByVal eArqConfig As String) As Long

Public Declare Function ETQ_ConfigGravar _
                Lib "ACBrETQ32.dll" (ByVal eArqConfig As String) As Long
                   
Public Declare Function ETQ_ConfigLerValor _
                Lib "ACBrETQ32.dll" (ByVal eSessao As String, _
                                            ByVal eChave As String, _
                                            ByVal buffer As String, _
                                            ByRef bufferLen As Long) As Long

Public Declare Function ETQ_ConfigGravarValor _
                Lib "ACBrETQ32.dll" (ByVal eSessao As String, _
                                            ByVal eChave As String, _
                                            ByVal valor As String) As Long
                    
Public Declare Function ETQ_Ativar Lib "ACBrETQ32.dll" () As Long

Public Declare Function ETQ_Desativar Lib "ACBrETQ32.dll" () As Long

Public Declare Function ETQ_IniciarEtiqueta Lib "ACBrETQ32.dll" () As Long

Public Declare Function ETQ_FinalizarEtiqueta _
                Lib "ACBrETQ32.dll" (ByVal ACopias As Long, _
                                     ByVal AAvancoEtq As Long) As Long
                                     
Public Declare Function ETQ_CarregarImagem _
                Lib "ACBrETQ32.dll" (ByVal eArquivoImagem As String, _
                                     ByVal eNomeImagem As String, _
                                     ByVal Flipped As Boolean) As Long
                                     
Public Declare Function ETQ_Imprimir _
                Lib "ACBrETQ32.dll" (ByVal ACopias As Long, _
                                     ByVal AAvancoEtq As Long) As Long
                                     
Public Declare Function ETQ_ImprimirTexto _
                Lib "ACBrETQ32.dll" (ByVal Orientacao As Long, _
                                     ByVal Fonte As Long, _
                                     ByVal MultiplicadorH As Long, _
                                     ByVal MultiplicadorV As Long, _
                                     ByVal Vertical As Long, _
                                     ByVal Horizontal As Long, _
                                     ByVal eTexto As String, _
                                     ByVal SubFonte As Long, _
                                     ByVal ImprimirReverso As Boolean) As Long
                                     
Public Declare Function ETQ_ImprimirTextoStr _
                Lib "ACBrETQ32.dll" (ByVal Orientacao As Long, _
                                     ByVal Fonte As String, _
                                     ByVal MultiplicadorH As Long, _
                                     ByVal MultiplicadorV As Long, _
                                     ByVal Vertical As Long, _
                                     ByVal Horizontal As Long, _
                                     ByVal eTexto As String, _
                                     ByVal SubFonte As Long, _
                                     ByVal ImprimirReverso As Boolean) As Long
                                     
Public Declare Function ETQ_ImprimirBarras _
                Lib "ACBrETQ32.dll" (ByVal Orientacao As Long, _
                                     ByVal TipoBarras As Long, _
                                     ByVal LarguraBarraLarga As Long, _
                                     ByVal LarguraBarraFina As Long, _
                                     ByVal Vertical As Long, _
                                     ByVal Horizontal As Long, _
                                     ByVal eTexto As String, _
                                     ByVal AlturaCodBarras As Long, _
                                     ByVal ExibeCodigo As Long) As Long
                                     
Public Declare Function ETQ_ImprimirLinha _
                Lib "ACBrETQ32.dll" (ByVal Vertical As Long, _
                                     ByVal Horizontal As Long, _
                                     ByVal Largura As Long, _
                                     ByVal Altura As Long) As Long
                                     
Public Declare Function ETQ_ImprimirCaixa _
                Lib "ACBrETQ32.dll" (ByVal Vertical As Long, _
                                     ByVal Horizontal As Long, _
                                     ByVal Largura As Long, _
                                     ByVal Altura As Long, _
                                     ByVal EspessuraVertical As Long, _
                                     ByVal EspessuraHorizontal As Long) As Long
                                     
Public Declare Function ETQ_ImprimirImagem _
                Lib "ACBrETQ32.dll" (ByVal MultiplicadorImagem As Long, _
                                     ByVal Vertical As Long, _
                                     ByVal Horizontal As Long, _
                                     ByVal eNomeImagem As String) As Long

Public Sub CheckResult(ByVal Resultado As Long)
    
    If Resultado = 0 Then Exit Sub
         
    Dim buffer As String

    Dim bufferLen As Long

    bufferLen = 256
    buffer = String$(bufferLen, " ")
    ETQ_UltimoRetorno buffer, bufferLen
    
    If bufferLen > 256 Then
        ETQ_UltimoRetorno buffer, bufferLen
    End If

    Err.Raise Resultado, "ACBrPosPrinter", FromUTF8(buffer, bufferLen)
End Sub

Public Function FromUTF8(ByRef utf8STR As String, ByVal bufferLen As Long) As String
    Dim UTF8() As Byte

    UTF8 = StrConv(utf8STR, vbFromUnicode)
    
    Dim lDataLength As Long
    
    ' Get the length of the data.
    lDataLength = MultiByteToWideChar(CP_UTF8, 0, VarPtr(UTF8(0)), bufferLen, 0, 0)
    
    ' Create array big enough
    FromUTF8 = String$(lDataLength, 0)
    
    MultiByteToWideChar CP_UTF8, 0, VarPtr(UTF8(0)), bufferLen, StrPtr(FromUTF8), lDataLength
End Function

Function DirExists(DirName As String) As Boolean
    On Error GoTo ErrorHandler
    ' test the directory attribute
    DirExists = GetAttr(DirName) And vbDirectory
ErrorHandler:
    ' if an error occurs, this function returns False
End Function

