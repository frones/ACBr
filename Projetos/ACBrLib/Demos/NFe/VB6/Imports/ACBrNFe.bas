Attribute VB_Name = "ACBrNFe"
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
                                
Public Declare Function NFE_Inicializar _
                Lib "ACBrNFE32.dll" (ByVal eArqConfig As String, _
                                            ByVal eChaveCrypt As String) As Long
                   
Public Declare Function NFE_Finalizar Lib "ACBrNFE32.dll" () As Long

Public Declare Function NFE_Nome _
                Lib "ACBrNFE32.dll" (ByVal buffer As String, _
                                            ByRef bufferLen As Long) As Long
                   
Public Declare Function NFE_Versao _
                Lib "ACBrNFE32.dll" (ByVal buffer As String, _
                                            ByRef bufferLen As Long) As Long

Public Declare Function NFE_UltimoRetorno _
                Lib "ACBrNFE32.dll" (ByVal buffer As String, _
                                            ByRef bufferLen As Long) As Long
                   
Public Declare Function NFE_ConfigLer _
                Lib "ACBrNFE32.dll" (ByVal eArqConfig As String) As Long

Public Declare Function NFE_ConfigGravar _
                Lib "ACBrNFE32.dll" (ByVal eArqConfig As String) As Long
                   
Public Declare Function NFE_ConfigLerValor _
                Lib "ACBrNFE32.dll" (ByVal eSessao As String, _
                                            ByVal eChave As String, _
                                            ByVal buffer As String, _
                                            ByRef bufferLen As Long) As Long

Public Declare Function NFE_ConfigGravarValor _
                Lib "ACBrNFE32.dll" (ByVal eSessao As String, _
                                            ByVal eChave As String, _
                                            ByVal valor As String) As Long
                                            
                                            
Public Declare Function NFE_CarregarXML _
                Lib "ACBrNFE32.dll" (ByVal eArquivoOuXml As String) As Long
                
Public Declare Function NFE_CarregarINI _
                Lib "ACBrNFE32.dll" (ByVal eArquivoOuIni As String) As Long
                
Public Declare Function NFE_CarregarEventoXML _
                Lib "ACBrNFE32.dll" (ByVal eArquivoOuXml As String) As Long
                
Public Declare Function NFE_CarregarEventoINI _
                Lib "ACBrNFE32.dll" (ByVal eArquivoOuIni As String) As Long
                
Public Declare Function NFE_LimparLista Lib "ACBrNFE32.dll" () As Long

Public Declare Function NFE_LimparListaEventos Lib "ACBrNFE32.dll" () As Long

Public Declare Function NFE_Assinar Lib "ACBrNFE32.dll" () As Long

Public Declare Function NFE_Validar Lib "ACBrNFE32.dll" () As Long

Public Declare Function NFE_ValidarRegrasdeNegocios _
                Lib "ACBrNFE32.dll" (ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Public Declare Function NFE_VerificarAssinatura _
                Lib "ACBrNFE32.dll" (ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Public Declare Function NFE_StatusServico _
                Lib "ACBrNFE32.dll" (ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Public Declare Function NFE_Consultar _
                Lib "ACBrNFE32.dll" (ByVal eChaveOuNFe As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Public Declare Function NFE_Inutilizar _
                Lib "ACBrNFE32.dll" (ByVal acnpj As String, _
                                     ByVal aJustificativa As String, _
                                     ByVal ano As Long, _
                                     ByVal modelo As Long, _
                                     ByVal serie As Long, _
                                     ByVal numeroInicial As Long, _
                                     ByVal numeroFinal As Long, _
                                     ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Public Declare Function NFE_Enviar _
                Lib "ACBrNFE32.dll" (ByVal aLote As Long, _
                                     ByVal imprimir As Boolean, _
                                     ByVal sincrono As Boolean, _
                                     ByVal zipado As Boolean, _
                                     ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Public Declare Function NFE_ConsultarRecibo _
                Lib "ACBrNFE32.dll" (ByVal aRecibo As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Public Declare Function NFE_Cancelar _
                Lib "ACBrNFE32.dll" (ByVal eChave As String, _
                                     ByVal eJustificativa As String, _
                                     ByVal eCNPJ As String, _
                                     ByVal aLote As Long, _
                                     ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Public Declare Function NFE_EnviarEvento _
                Lib "ACBrNFE32.dll" (ByVal aLote As Long, _
                                     ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Public Declare Function NFE_DistribuicaoDFePorUltNSU _
                Lib "ACBrNFE32.dll" (ByVal acUFAutor As Long, _
                                     ByVal eCnpjcpf As String, _
                                     ByVal eultNsu As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Public Declare Function NFE_DistribuicaoDFePorNSU _
                Lib "ACBrNFE32.dll" (ByVal acUFAutor As Long, _
                                     ByVal eCnpjcpf As String, _
                                     ByVal eNsu As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Public Declare Function NFE_DistribuicaoDFePorChave _
                Lib "ACBrNFE32.dll" (ByVal acUFAutor As Long, _
                                     ByVal eCnpjcpf As String, _
                                     ByVal echNFe As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Public Declare Function NFE_EnviarEmail _
                Lib "ACBrNFE32.dll" (ByVal ePara As Long, _
                                     ByVal eChaveNFe As String, _
                                     ByVal aEnviaPDF As Boolean, _
                                     ByVal eAssunto As String, _
                                     ByVal eCc As String, _
                                     ByVal eAnexos As String, _
                                     ByVal eMensagem As String) As Long
                                     
Public Declare Function NFE_EnviarEmailEvento _
                Lib "ACBrNFE32.dll" (ByVal ePara As Long, _
                                     ByVal eChaveNFe As String, _
                                     ByVal aEnviaPDF As Boolean, _
                                     ByVal eAssunto As String, _
                                     ByVal eCc As String, _
                                     ByVal eAnexos As String, _
                                     ByVal eMensagem As String) As Long
                                     
Public Declare Function NFE_Imprimir _
                Lib "ACBrNFE32.dll" (ByVal cImpressora As String, _
                                     ByVal nNumCopias As Long, _
                                     ByVal cProtocolo As String, _
                                     ByVal bMostrarPreview As String, _
                                     ByVal cMarcaDagua As String, _
                                     ByVal bViaConsumidor As String, _
                                     ByVal bSimplificado As String) As Long
                                     
Public Declare Function NFE_ImprimirPDF Lib "ACBrNFE32.dll" () As Long

Public Declare Function NFE_ImprimirEvento _
                Lib "ACBrNFE32.dll" (ByVal eChaveNFe As String, _
                                     ByVal eChaveEvento As String) As Long
                                     
Public Declare Function NFE_ImprimirEventoPDF _
                Lib "ACBrNFE32.dll" (ByVal eChaveNFe As String, _
                                     ByVal eChaveEvento As String) As Long
                                     
Public Declare Function NFE_ImprimirInutilizacao _
                Lib "ACBrNFE32.dll" (ByVal eChaveNFe As String) As Long
                
Public Declare Function NFE_ImprimirInutilizacaoPDF _
                Lib "ACBrNFE32.dll" (ByVal eChaveNFe As String) As Long
                                     
Public Sub CheckResult(ByVal Resultado As Long)
    
    If Resultado >= 0 Then Exit Sub
         
    Dim buffer As String
    Dim bufferLen As Long

    bufferLen = 256
    buffer = String$(bufferLen, " ")
    NFE_UltimoRetorno buffer, bufferLen
    
    If bufferLen > 256 Then
        buffer = String$(bufferLen, " ")
        NFE_UltimoRetorno buffer, bufferLen
    End If

    Err.Raise Resultado, "ACBrNFe", Trim$(FromUTF8(buffer))
End Sub

Public Function FromUTF8(ByRef utf8STR As String) As String
    
    Dim length As Long

    length = Len(utf8STR)
    
    If length < 1 Then
        FromUTF8 = vbNullString
        Exit Function
    End If
    
    Dim UTF8() As Byte

    UTF8 = StrConv(utf8STR, vbFromUnicode)
        
    Dim lDataLength As Long
    
    ' Get the length of the data.
    lDataLength = MultiByteToWideChar(CP_UTF8, 0, VarPtr(UTF8(0)), length, 0, 0)
    
    ' Create array big enough
    FromUTF8 = String$(lDataLength, 0)
    
    MultiByteToWideChar CP_UTF8, 0, VarPtr(UTF8(0)), length, StrPtr(FromUTF8), lDataLength
End Function

Function DirExists(DirName As String) As Boolean
    On Error GoTo ErrorHandler
    ' test the directory attribute
    DirExists = GetAttr(DirName) And vbDirectory
ErrorHandler:
    ' if an error occurs, this function returns False
End Function

Function FileExists(ByVal sFileName As String) As Boolean
    On Error GoTo ErrorHandler
    ' test the Archive attribute
    FileExists = GetAttr(sFileName) And vbArchive
ErrorHandler:
    ' if an error occurs, this function returns False
End Function
