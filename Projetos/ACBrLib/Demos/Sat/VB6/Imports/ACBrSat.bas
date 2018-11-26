Attribute VB_Name = "ACBrSat"
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
                                
Public Declare Function SAT_Inicializar _
                Lib "ACBrSAT32.dll" (ByVal eArqConfig As String, _
                                            ByVal eChaveCrypt As String) As Long
                   
Public Declare Function SAT_Finalizar Lib "ACBrSAT32.dll" () As Long

Public Declare Function SAT_Nome _
                Lib "ACBrSAT32.dll" (ByVal buffer As String, _
                                            ByRef bufferLen As Long) As Long
                   
Public Declare Function SAT_Versao _
                Lib "ACBrSAT32.dll" (ByVal buffer As String, _
                                            ByRef bufferLen As Long) As Long

Public Declare Function SAT_UltimoRetorno _
                Lib "ACBrSAT32.dll" (ByVal buffer As String, _
                                            ByRef bufferLen As Long) As Long
                   
Public Declare Function SAT_ConfigLer _
                Lib "ACBrSAT32.dll" (ByVal eArqConfig As String) As Long

Public Declare Function SAT_ConfigGravar _
                Lib "ACBrSAT32.dll" (ByVal eArqConfig As String) As Long
                   
Public Declare Function SAT_ConfigLerValor _
                Lib "ACBrSAT32.dll" (ByVal eSessao As String, _
                                            ByVal eChave As String, _
                                            ByVal buffer As String, _
                                            ByRef bufferLen As Long) As Long

Public Declare Function SAT_ConfigGravarValor _
                Lib "ACBrSAT32.dll" (ByVal eSessao As String, _
                                            ByVal eChave As String, _
                                            ByVal valor As String) As Long

Public Declare Function SAT_InicializarSAT Lib "ACBrSAT32.dll" () As Long

Public Declare Function SAT_DesInicializar Lib "ACBrSAT32.dll" () As Long

Public Declare Function SAT_AssociarAssinatura _
                Lib "ACBrSAT32.dll" (ByVal CNPJValue As String, _
                                     ByVal assinaturaCNPJs As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                     
Public Declare Function SAT_BloquearSAT _
                Lib "ACBrSAT32.dll" (ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                     
Public Declare Function SAT_DesbloquearSAT _
                Lib "ACBrSAT32.dll" (ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                                                          
Public Declare Function SAT_TrocarCodigoDeAtivacao _
                Lib "ACBrSAT32.dll" (ByVal codigoDeAtivacaoOuEmergencia As String, _
                                     ByVal opcao As Long, _
                                     ByVal novoCodigo As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                     
Public Declare Function SAT_ConsultarSAT _
                Lib "ACBrSAT32.dll" (ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                     
Public Declare Function SAT_ConsultarStatusOperacional _
                Lib "ACBrSAT32.dll" (ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                     
Public Declare Function SAT_ConsultarNumeroSessao _
                Lib "ACBrSAT32.dll" (ByVal cNumeroDeSessao As Long, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                     
Public Declare Function SAT_AtualizarSoftwareSAT _
                Lib "ACBrSAT32.dll" (ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                     
Public Declare Function SAT_ComunicarCertificadoICPBRASIL _
                Lib "ACBrSAT32.dll" (ByVal certificado As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                     
Public Declare Function SAT_ExtrairLogs _
                Lib "ACBrSAT32.dll" (ByVal eArquivo As String) As Long
                
Public Declare Function SAT_TesteFimAFim _
                Lib "ACBrSAT32.dll" (ByVal eArquivoXmlVenda As String) As Long
                
Public Declare Function SAT_CriarCFe _
                Lib "ACBrSAT32.dll" (ByVal eArquivoIni As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                     
Public Declare Function SAT_CriarEnviarCFe _
                Lib "ACBrSAT32.dll" (ByVal eArquivoIni As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                     
Public Declare Function SAT_EnviarCFe _
                Lib "ACBrSAT32.dll" (ByVal eArquivoXml As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                     
Public Declare Function SAT_CancelarCFe _
                Lib "ACBrSAT32.dll" (ByVal eArquivoXml As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                     
Public Declare Function SAT_ImprimirExtratoVenda _
                Lib "ACBrSAT32.dll" (ByVal eArquivoXml As String, _
                                     ByVal eNomeImpressora As String) As Long
                                     
Public Declare Function SAT_ImprimirExtratoResumido _
                Lib "ACBrSAT32.dll" (ByVal eArquivoXml As String, _
                                     ByVal eNomeImpressora As String) As Long
                                     
Public Declare Function SAT_GerarPDFExtratoVenda _
                Lib "ACBrSAT32.dll" (ByVal eArquivoXml As String, _
                                     ByVal eNomeArquivo As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                     
Public Declare Function SAT_GerarImpressaoFiscalMFe _
                Lib "ACBrSAT32.dll" (ByVal eArquivoXml As String, _
                                     ByVal eNomeArquivo As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                     
Public Declare Function SAT_EnviarEmail _
                Lib "ACBrSAT32.dll" (ByVal eArquivoXml As String, _
                                     ByVal ePara As String, _
                                     ByVal eAssunto As String, _
                                     ByVal eNomeArquivo As String, _
                                     ByVal sMensagem As String, _
                                     ByVal sCC As String, _
                                     ByVal eAnexos As String) As Long
                                     
Public Sub CheckResult(ByVal Resultado As Long)
    
    If Resultado = 0 Then Exit Sub
         
    Dim buffer As String

    Dim bufferLen As Long

    bufferLen = 256
    buffer = String$(bufferLen, " ")
    SAT_UltimoRetorno buffer, bufferLen
    
    If bufferLen > 256 Then
        buffer = String$(bufferLen, " ")
        SAT_UltimoRetorno buffer, bufferLen
    End If

    Err.Raise Resultado, "ACBrSat", Trim$(FromUTF8(buffer))
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
