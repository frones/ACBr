VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ACBrBoleto"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Const BUFFER_LENGTH As Long = 1024

' UTF-8 Code Page'Sys call to convert multiple byte chars to a charPrivate
Const CP_UTF8 As Long = 65001

Private Declare Function MultiByteToWideChar _
                Lib "kernel32" (ByVal CodePage As Long, _
                                ByVal dwFlags As Long, _
                                ByVal lpMultiByteStr As Long, _
                                ByVal cchMultiByte As Long, _
                                ByVal lpWideCharStr As Long, _
                                ByVal cchWideChar As Long) As Long
                                
Private Declare Function Boleto_Inicializar _
                Lib "ACBrBoleto32.dll" (ByVal eArqConfig As String, _
                                            ByVal eChaveCrypt As String) As Long
                   
Private Declare Function Boleto_Finalizar Lib "ACBrBoleto32.dll" () As Long

Private Declare Function Boleto_Nome _
                Lib "ACBrBoleto32.dll" (ByVal buffer As String, _
                                            ByRef bufferLen As Long) As Long
                   
Private Declare Function Boleto_Versao _
                Lib "ACBrBoleto32.dll" (ByVal buffer As String, _
                                            ByRef bufferLen As Long) As Long

Private Declare Function Boleto_UltimoRetorno _
                Lib "ACBrBoleto32.dll" (ByVal buffer As String, _
                                            ByRef bufferLen As Long) As Long
                   
Private Declare Function Boleto_ConfigLer _
                Lib "ACBrBoleto32.dll" (ByVal eArqConfig As String) As Long

Private Declare Function Boleto_ConfigGravar _
                Lib "ACBrBoleto32.dll" (ByVal eArqConfig As String) As Long
                   
Private Declare Function Boleto_ConfigLerValor _
                Lib "ACBrBoleto32.dll" (ByVal eSessao As String, _
                                            ByVal eChave As String, _
                                            ByVal buffer As String, _
                                            ByRef bufferLen As Long) As Long

Private Declare Function Boleto_ConfigGravarValor _
                Lib "ACBrBoleto32.dll" (ByVal eSessao As String, _
                                            ByVal eChave As String, _
                                            ByVal valor As String) As Long
                                            
Private Declare Function Boleto_ConfigurarDados _
                Lib "ACBrBoleto32.dll" (ByVal eArquivoIni As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                     
Private Declare Function Boleto_IncluirTitulos _
                Lib "ACBrBoleto32.dll" (ByVal eArquivoIni As String, _
                                     ByVal eTpSaida As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                     
Private Declare Function Boleto_LimparLista Lib "ACBrBoleto32.dll" () As Long
                
Private Declare Function Boleto_TotalTitulosLista _
                Lib "ACBrBoleto32.dll" (ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                     
Private Declare Function Boleto_Imprimir _
                Lib "ACBrBoleto32.dll" (ByVal eNomeImpressora As String) As Long
                
Private Declare Function Boleto_GerarPDF Lib "ACBrBoleto32.dll" () As Long

Private Declare Function Boleto_GerarHTML Lib "ACBrBoleto32.dll" () As Long

Private Declare Function Boleto_GerarRemessa _
                Lib "ACBrBoleto32.dll" (ByVal eDir As String, _
                                     ByVal eNumArquivo As Long, _
                                     ByVal eNomeArquivo As String) As Long
                                     
Private Declare Function Boleto_LerRetorno _
                Lib "ACBrBoleto32.dll" (ByVal eDir As String, _
                                     ByVal eNomeArq As String) As Long
                                     
Private Declare Function Boleto_EnviarEmail _
                Lib "ACBrBoleto32.dll" (ByVal ePara As String, _
                                     ByVal eAssunto As String, _
                                     ByVal eMensagem As String, _
                                     ByVal eCC As String) As Long
                                     
Private Declare Function Boleto_SetDiretorioArquivo _
                Lib "ACBrBoleto32.dll" (ByVal eDir As String, _
                                     ByVal eArq As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                     
Private Declare Function Boleto_ListaBancos _
                Lib "ACBrBoleto32.dll" (ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                     
Private Declare Function Boleto_ListaCaractTitulo _
                Lib "ACBrBoleto32.dll" (ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                     
Private Declare Function Boleto_ListaOcorrencias _
                Lib "ACBrBoleto32.dll" (ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                     
Private Declare Function Boleto_ListaOcorrenciasEX _
                Lib "ACBrBoleto32.dll" (ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                     
Private Declare Function Boleto_TamNossoNumero _
                Lib "ACBrBoleto32.dll" (ByVal eCarteira As String, _
                                     ByVal enossoNumero As String, _
                                     ByVal eConvenio As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                     
Private Declare Function Boleto_CodigosMoraAceitos _
                Lib "ACBrBoleto32.dll" (ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                     
Private Declare Function Boleto_SelecionaBanco _
                Lib "ACBrBoleto32.dll" (ByVal eCodBanco As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                     
Private Declare Function Boleto_MontarNossoNumero _
                Lib "ACBrBoleto32.dll" (ByVal eIndice As Long, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                     
Private Declare Function Boleto_RetornaLinhaDigitavel _
                Lib "ACBrBoleto32.dll" (ByVal eIndice As Long, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                     
Private Declare Function Boleto_RetornaCodigoBarras _
                Lib "ACBrBoleto32.dll" (ByVal eIndice As Long, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                     
Public Sub InicializarLib(Optional ByVal eArqConfig As String = "", _
                          Optional ByVal eChaveCrypt As String = "")

    Dim retorno As Long

    retorno = Boleto_Inicializar(eArqConfig, eChaveCrypt)
    CheckResult retorno
End Sub

Public Sub FinalizarLib()

    Dim retorno As Long

    retorno = Boleto_Finalizar
    CheckResult retorno
End Sub

Public Sub ConfigLer(Optional eArqConfig As String = "")

    Dim retorno As Long

    retorno = Boleto_ConfigLer(eArqConfig)
    CheckResult retorno
End Sub

Public Sub ConfigGravar(Optional eArqConfig As String = "")

    Dim retorno As Long

    retorno = Boleto_ConfigGravar(eArqConfig)
    CheckResult retorno
End Sub

Public Function ConfigLerValor(ByVal eSessao As String, ByVal eChave As String) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_ConfigLerValor(eSessao, eChave, buffer, bufferLen)
    CheckResult retorno
    
    ConfigLerValor = processResult(buffer, bufferLen)
End Function

Public Sub ConfigGravarValor(ByVal eSessao As String, _
                             ByVal eChave As String, _
                             ByVal valor As String)

    Dim retorno As Long
    
    retorno = Boleto_ConfigGravarValor(eSessao, eChave, valor)
    CheckResult retorno
End Sub

Public Function Nome() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
        
    retorno = Boleto_Nome(buffer, bufferLen)
    CheckResult retorno
    
    Nome = processResult(buffer, bufferLen)
End Function

Public Function Versao() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_Versao(buffer, bufferLen)
    CheckResult retorno
    
    Versao = processResult(buffer, bufferLen)
End Function

Public Function ConfigurarDados(ByVal eArquivoIni As String) As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_ConfigurarDados(eArquivoIni, buffer, bufferLen)
    CheckResult retorno
    
    ConfigurarDados = processResult(buffer, bufferLen)
End Function

Public Function IncluirTitulos(ByVal eArquivoIni As String, Optional ByVal eTpSaida As String = "") As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_IncluirTitulos(eArquivoIni, eTpSaida, buffer, bufferLen)
    CheckResult retorno
    
    IncluirTitulos = processResult(buffer, bufferLen)
End Function

Public Sub LimparLista()
    Dim retorno   As Long
    
    retorno = Boleto_LimparLista()
    CheckResult retorno
End Sub

Public Function TotalTitulosLista() As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_TotalTitulosLista(buffer, bufferLen)
    CheckResult retorno
    
    TotalTitulosLista = processResult(buffer, bufferLen)
End Function

Public Sub Imprimir(Optional ByVal eNomeImpressora As String = "")
    Dim retorno   As Long
    
    retorno = Boleto_Imprimir(eNomeImpressora)
    CheckResult retorno
End Sub

Public Sub GerarPDF()
    Dim retorno   As Long
    
    retorno = Boleto_GerarPDF(eNomeImpressora)
    CheckResult retorno
End Sub

Public Sub GerarHTML()
    Dim retorno   As Long
    
    retorno = Boleto_GerarHTML(eNomeImpressora)
    CheckResult retorno
End Sub

Public Sub GerarRemessa(ByVal eDir As String, ByVal eNumArquivo As Long, ByVal eNomeArquivo As String)
    Dim retorno   As Long
    
    retorno = Boleto_GerarRemessa(eDir, eNumArquivo, eNomeArquivo)
    CheckResult retorno
End Sub

Public Sub EnviarEmail(ByVal ePara As String, ByVal eAssunto As String, ByVal eMensagem As String, ByVal eCC As String)
    Dim retorno   As Long
    
    retorno = Boleto_EnviarEmail(ePara, eAssunto, eMensagem, eCC)
    CheckResult retorno
End Sub

Public Sub LerRetorno(ByVal eDir As String, ByVal eNomeArquivo As String)
    Dim retorno   As Long
    
    retorno = Boleto_LerRetorno(eDir, eNomeArquivo)
    CheckResult retorno
End Sub

Public Function SetDiretorioArquivo(ByVal eDir As String, ByVal eArq As String) As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_SetDiretorioArquivo(eDir, eArq, buffer, bufferLen)
    CheckResult retorno
    
    SetDiretorioArquivo = processResult(buffer, bufferLen)
End Function

Public Function ListaBancos() As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_ListaBancos(buffer, bufferLen)
    CheckResult retorno
    
    ListaBancos = processResult(buffer, bufferLen)
End Function

Public Function ListaCaractTitulo() As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_ListaCaractTitulo(buffer, bufferLen)
    CheckResult retorno
    
    ListaCaractTitulo = processResult(buffer, bufferLen)
End Function

Public Function ListaOcorrencias() As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_ListaOcorrencias(buffer, bufferLen)
    CheckResult retorno
    
    ListaOcorrencias = processResult(buffer, bufferLen)
End Function

Public Function ListaOcorrenciasEX() As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_ListaOcorrenciasEX(buffer, bufferLen)
    CheckResult retorno
    
    ListaOcorrenciasEX = processResult(buffer, bufferLen)
End Function

Public Function TamNossoNumero(ByVal eCarteira As String, ByVal enossoNumero As String, ByVal eConvenio As String) As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_TamNossoNumero(eCarteira, enossoNumero, eConvenio, buffer, bufferLen)
    CheckResult retorno
    
    TamNossoNumero = processResult(buffer, bufferLen)
End Function

Public Function CodigosMoraAceitos() As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_CodigosMoraAceitos(buffer, bufferLen)
    CheckResult retorno
    
    CodigosMoraAceitos = processResult(buffer, bufferLen)
End Function

Public Function SelecionaBanco(ByVal eCodBanco As String) As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_SelecionaBanco(eCodBanco, buffer, bufferLen)
    CheckResult retorno
    
    SelecionaBanco = processResult(buffer, bufferLen)
End Function

Public Function MontarNossoNumero(ByVal eIndice As Long) As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_MontarNossoNumero(eIndice, buffer, bufferLen)
    CheckResult retorno
    
    MontarNossoNumero = processResult(buffer, bufferLen)
End Function

Public Function RetornaLinhaDigitavel(ByVal eIndice As Long) As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_RetornaLinhaDigitavel(eIndice, buffer, bufferLen)
    CheckResult retorno
    
    RetornaLinhaDigitavel = processResult(buffer, bufferLen)
End Function

Public Function RetornaCodigoBarras(ByVal eIndice As Long) As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_RetornaCodigoBarras(eIndice, buffer, bufferLen)
    CheckResult retorno
    
    RetornaCodigoBarras = processResult(buffer, bufferLen)
End Function
                                     
Private Sub CheckResult(ByVal Resultado As Long)
    
    If Resultado >= 0 Then Exit Sub
         
    Dim buffer As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    Boleto_UltimoRetorno buffer, bufferLen
    
    Err.Raise Resultado, "ACBrBoleto", processResult(buffer, bufferLen)
End Sub

Private Function processResult(ByRef buffer As String, ByRef bufferLen As Long) As String
    
    If bufferLen > BUFFER_LENGTH Then
        buffer = String$(bufferLen, " ")
        Boleto_UltimoRetorno buffer, bufferLen
    End If

    processResult = Trim$(FromUTF8(buffer))
End Function

Private Function FromUTF8(ByRef utf8STR As String) As String
    
    Dim length As Long
    Dim UTF8() As Byte
    Dim lDataLength As Long

    length = Len(utf8STR)
    UTF8 = StrConv(utf8STR, vbFromUnicode)
    
    ' Get the length of the data.
    lDataLength = MultiByteToWideChar(CP_UTF8, 0, VarPtr(UTF8(0)), length, 0, 0)
    
    ' Create array big enough
    FromUTF8 = String$(lDataLength, 0)
    
    MultiByteToWideChar CP_UTF8, 0, VarPtr(UTF8(0)), length, StrPtr(FromUTF8), lDataLength
End Function

