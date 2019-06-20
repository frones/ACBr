Attribute VB_Name = "ACBrBoleto"
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
                                
Public Declare Function Boleto_Inicializar _
                Lib "ACBrBoleto32.dll" (ByVal eArqConfig As String, _
                                            ByVal eChaveCrypt As String) As Long
                   
Public Declare Function Boleto_Finalizar Lib "ACBrBoleto32.dll" () As Long

Public Declare Function Boleto_Nome _
                Lib "ACBrBoleto32.dll" (ByVal buffer As String, _
                                            ByRef bufferLen As Long) As Long
                   
Public Declare Function Boleto_Versao _
                Lib "ACBrBoleto32.dll" (ByVal buffer As String, _
                                            ByRef bufferLen As Long) As Long

Public Declare Function Boleto_UltimoRetorno _
                Lib "ACBrBoleto32.dll" (ByVal buffer As String, _
                                            ByRef bufferLen As Long) As Long
                   
Public Declare Function Boleto_ConfigLer _
                Lib "ACBrBoleto32.dll" (ByVal eArqConfig As String) As Long

Public Declare Function Boleto_ConfigGravar _
                Lib "ACBrBoleto32.dll" (ByVal eArqConfig As String) As Long
                   
Public Declare Function Boleto_ConfigLerValor _
                Lib "ACBrBoleto32.dll" (ByVal eSessao As String, _
                                            ByVal eChave As String, _
                                            ByVal buffer As String, _
                                            ByRef bufferLen As Long) As Long

Public Declare Function Boleto_ConfigGravarValor _
                Lib "ACBrBoleto32.dll" (ByVal eSessao As String, _
                                            ByVal eChave As String, _
                                            ByVal valor As String) As Long
                                            
Public Declare Function Boleto_ConfigurarDados _
                Lib "ACBrBoleto32.dll" (ByVal eArquivoIni As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                     
Public Declare Function Boleto_IncluirTitulos _
                Lib "ACBrBoleto32.dll" (ByVal eArquivoIni As String, _
                                     ByVal eTpSaida As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                     
Public Declare Function Boleto_LimparLista Lib "ACBrBoleto32.dll" () As Long
                
Public Declare Function Boleto_TotalTitulosLista _
                Lib "ACBrBoleto32.dll" (ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                     
Public Declare Function Boleto_Imprimir _
                Lib "ACBrBoleto32.dll" (ByVal eNomeImpressora As String) As Long
                
Public Declare Function Boleto_GerarPDF Lib "ACBrBoleto32.dll" () As Long

Public Declare Function Boleto_GerarHTML Lib "ACBrBoleto32.dll" () As Long

Public Declare Function Boleto_GerarRemessa _
                Lib "ACBrBoleto32.dll" (ByVal eDir As String, _
                                     ByVal eNumArquivo As Long, _
                                     ByVal eNomeArquivo As String) As Long
                                     
Public Declare Function Boleto_LerRetorno _
                Lib "ACBrBoleto32.dll" (ByVal eDir As String, _
                                     ByVal eNomeArq As String) As Long
                                     
Public Declare Function Boleto_EnviarEmail _
                Lib "ACBrBoleto32.dll" (ByVal ePara As String, _
                                     ByVal eAssunto As String, _
                                     ByVal eMensagem As String, _
                                     ByVal eCC As String) As Long
                                     
Public Declare Function Boleto_SetDiretorioArquivo _
                Lib "ACBrBoleto32.dll" (ByVal eDir As String, _
                                     ByVal eArq As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                     
Public Declare Function Boleto_ListaBancos _
                Lib "ACBrBoleto32.dll" (ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                     
Public Declare Function Boleto_ListaCaractTitulo _
                Lib "ACBrBoleto32.dll" (ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                     
Public Declare Function Boleto_ListaOcorrencias _
                Lib "ACBrBoleto32.dll" (ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                     
Public Declare Function Boleto_ListaOcorrenciasEX _
                Lib "ACBrBoleto32.dll" (ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                     
Public Declare Function Boleto_TamNossoNumero _
                Lib "ACBrBoleto32.dll" (ByVal eCarteira As String, _
                                     ByVal enossoNumero As String, _
                                     ByVal eConvenio As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                     
Public Declare Function Boleto_CodigosMoraAceitos _
                Lib "ACBrBoleto32.dll" (ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                     
Public Declare Function Boleto_SelecionaBanco _
                Lib "ACBrBoleto32.dll" (ByVal eCodBanco As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                     
Public Declare Function Boleto_MontarNossoNumero _
                Lib "ACBrBoleto32.dll" (ByVal eIndice As Long, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                     
Public Declare Function Boleto_RetornaLinhaDigitavel _
                Lib "ACBrBoleto32.dll" (ByVal eIndice As Long, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                     
Public Declare Function Boleto_RetornaCodigoBarras _
                Lib "ACBrBoleto32.dll" (ByVal eIndice As Long, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                     
Public Sub CheckResult(ByVal Resultado As Long)
    
    If Resultado = 0 Then Exit Sub
         
    Dim buffer As String

    Dim bufferLen As Long

    bufferLen = 256
    buffer = String$(bufferLen, " ")
    Boleto_UltimoRetorno buffer, bufferLen
    
    If bufferLen > 256 Then
        buffer = String$(bufferLen, " ")
        Boleto_UltimoRetorno buffer, bufferLen
    End If

    Err.Raise Resultado, "ACBrBoleto", Trim$(FromUTF8(buffer))
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
