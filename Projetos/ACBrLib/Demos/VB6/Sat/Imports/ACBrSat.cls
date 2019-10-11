VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ACBrSat"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Const BUFFER_LENGTH As Long = 1024

' UTF-8 Code Page'Sys call to convert multiple byte chars to a charPrivate
Const CP_UTF8       As Long = 65001

Private Declare Function MultiByteToWideChar _
                Lib "kernel32" (ByVal CodePage As Long, _
                                ByVal dwFlags As Long, _
                                ByVal lpMultiByteStr As Long, _
                                ByVal cchMultiByte As Long, _
                                ByVal lpWideCharStr As Long, _
                                ByVal cchWideChar As Long) As Long
                                
Private Declare Function SAT_Inicializar _
                Lib "ACBrSAT32.dll" (ByVal eArqConfig As String, _
                                      ByVal eChaveCrypt As String) As Long
                   
Private Declare Function SAT_Finalizar Lib "ACBrSAT32.dll" () As Long

Private Declare Function SAT_Nome _
                Lib "ACBrSAT32.dll" (ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                   
Private Declare Function SAT_Versao _
                Lib "ACBrSAT32.dll" (ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long

Private Declare Function SAT_UltimoRetorno _
                Lib "ACBrSAT32.dll" (ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                   
Private Declare Function SAT_ConfigLer _
                Lib "ACBrSAT32.dll" (ByVal eArqConfig As String) As Long

Private Declare Function SAT_ConfigGravar _
                Lib "ACBrSAT32.dll" (ByVal eArqConfig As String) As Long
                   
Private Declare Function SAT_ConfigLerValor _
                Lib "ACBrSAT32.dll" (ByVal eSessao As String, _
                                      ByVal eChave As String, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long

Private Declare Function SAT_ConfigGravarValor _
                Lib "ACBrSAT32.dll" (ByVal eSessao As String, _
                                      ByVal eChave As String, _
                                      ByVal valor As String) As Long
                                      
Private Declare Function SAT_InicializarSAT Lib "ACBrSAT32.dll" () As Long

Private Declare Function SAT_DesInicializar Lib "ACBrSAT32.dll" () As Long

Private Declare Function SAT_AssociarAssinatura _
                Lib "ACBrSAT32.dll" (ByVal CNPJValue As String, _
                                      ByVal assinaturaCNPJs As String, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                                      
Private Declare Function SAT_BloquearSAT _
                Lib "ACBrSAT32.dll" (ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                                      
Private Declare Function SAT_DesbloquearSAT _
                Lib "ACBrSAT32.dll" (ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                                      
Private Declare Function SAT_TrocarCodigoDeAtivacao _
                Lib "ACBrSAT32.dll" (ByVal codigoDeAtivacaoOuEmergencia As String, _
                                      ByVal opcao As Long, _
                                      ByVal novoCodigo As String, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                                      
Private Declare Function SAT_ConsultarSAT _
                Lib "ACBrSAT32.dll" (ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                                      
Private Declare Function SAT_ConsultarStatusOperacional _
                Lib "ACBrSAT32.dll" (ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                                      
Private Declare Function SAT_ConsultarNumeroSessao _
                Lib "ACBrSAT32.dll" (ByVal cNumeroDeSessao As Long, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                                      
Private Declare Function SAT_AtualizarSoftwareSAT _
                Lib "ACBrSAT32.dll" (ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                                      
Private Declare Function SAT_ComunicarCertificadoICPBRASIL _
                Lib "ACBrSAT32.dll" (ByVal certificado As String, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                                      
Private Declare Function SAT_ExtrairLogs _
                Lib "ACBrSAT32.dll" (ByVal eArquivo As String) As Long
                
Private Declare Function SAT_TesteFimAFim _
                Lib "ACBrSAT32.dll" (ByVal eArquivoXmlVenda As String) As Long
                
Private Declare Function SAT_CriarCFe _
                Lib "ACBrSAT32.dll" (ByVal eArquivoIni As String, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                                      
Private Declare Function SAT_CriarEnviarCFe _
                Lib "ACBrSAT32.dll" (ByVal eArquivoIni As String, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long

Private Declare Function SAT_EnviarCFe _
                Lib "ACBrSAT32.dll" (ByVal eArquivoXml As String, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                                      
Private Declare Function SAT_CancelarCFe _
                Lib "ACBrSAT32.dll" (ByVal eArquivoXml As String, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                                      
Private Declare Function SAT_ImprimirExtratoVenda _
                Lib "ACBrSAT32.dll" (ByVal eArquivoXml As String, _
                                      ByVal eNomeImpressora As String) As Long
                                      
Private Declare Function SAT_ImprimirExtratoResumido _
                Lib "ACBrSAT32.dll" (ByVal eArquivoXml As String, _
                                      ByVal eNomeImpressora As String) As Long
                                      
Private Declare Function SAT_GerarPDFExtratoVenda _
                Lib "ACBrSAT32.dll" (ByVal eArquivoXml As String, _
                                      ByVal eNomeArquivo As String, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                                      
Private Declare Function SAT_GerarImpressaoFiscalMFe _
                Lib "ACBrSAT32.dll" (ByVal eArquivoXml As String, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                                      
Private Declare Function SAT_EnviarEmail _
                Lib "ACBrSAT32.dll" (ByVal eArquivoXml As String, _
                                      ByVal ePara As String, _
                                      ByVal eAssunto As String, _
                                      ByVal eNomeArquivo As String, _
                                      ByVal sMensagem As String, _
                                      ByVal sCC As String, _
                                      ByVal eAnexos As String) As Long

Public Sub InicializarLib(Optional ByVal eArqConfig As String = "", _
                          Optional ByVal eChaveCrypt As String = "")

    Dim retorno As Long

    retorno = SAT_Inicializar(eArqConfig, eChaveCrypt)
    CheckResult (retorno)
End Sub

Public Sub FinalizarLib()

    Dim retorno As Long

    retorno = SAT_Finalizar
    CheckResult (retorno)
End Sub

Public Sub ConfigLer(Optional eArqConfig As String = "")

    Dim retorno As Long

    retorno = SAT_ConfigLer(eArqConfig)
    CheckResult (retorno)
End Sub

Public Sub ConfigGravar(Optional eArqConfig As String = "")

    Dim retorno As Long

    retorno = SAT_ConfigGravar(eArqConfig)
    CheckResult (retorno)
End Sub

Public Function ConfigLerValor(ByVal eSessao As String, ByVal eChave As String) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen + 1, " ")
    
    retorno = SAT_ConfigLerValor(eSessao, eChave, buffer, bufferLen)
    CheckResult (retorno)
    
    ConfigLerValor = processResult(buffer, bufferLen)
End Function

Public Sub ConfigGravarValor(ByVal eSessao As String, _
                             ByVal eChave As String, _
                             ByVal valor As String)

    Dim retorno As Long
    
    retorno = SAT_ConfigGravarValor(eSessao, eChave, valor)
    CheckResult (retorno)
End Sub

Public Function Nome() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
        
    retorno = SAT_Nome(buffer, bufferLen)
    CheckResult (retorno)
    
    Nome = processResult(buffer, bufferLen)
End Function

Public Function Versao() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = SAT_Versao(buffer, bufferLen)
    CheckResult (retorno)
    
    Versao = processResult(buffer, bufferLen)
End Function

Public Sub Inicializar()
    Dim retorno   As Long
    
    retorno = SAT_InicializarSAT()
    CheckResult (retorno)
End Sub

Public Sub DesInicializar()
    Dim retorno   As Long
    
    retorno = SAT_DesInicializar()
    CheckResult (retorno)
End Sub

Public Function AssociarAssinatura(ByVal CNPJValue As String, ByVal assinaturaCNPJs As String) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = SAT_AssociarAssinatura(CNPJValue, assinaturaCNPJs, buffer, bufferLen)
    CheckResult (retorno)
    
    AssociarAssinatura = processResult(buffer, bufferLen)
End Function

Public Function BloquearSAT() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = SAT_BloquearSAT(buffer, bufferLen)
    CheckResult (retorno)
    
    BloquearSAT = processResult(buffer, bufferLen)
End Function

Public Function DesbloquearSAT() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = SAT_DesbloquearSAT(buffer, bufferLen)
    CheckResult (retorno)
    
    DesbloquearSAT = processResult(buffer, bufferLen)
End Function

Public Function TrocarCodigoDeAtivacao(ByVal codigoDeAtivacaoOuEmergencia As String, _
        ByVal opcao As Long, ByVal novoCodigo As String) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = SAT_TrocarCodigoDeAtivacao(codigoDeAtivacaoOuEmergencia, opcao, novoCodigo, buffer, bufferLen)
    CheckResult (retorno)
    
    TrocarCodigoDeAtivacao = processResult(buffer, bufferLen)
End Function

Public Function ConsultarSAT() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = SAT_ConsultarSAT(buffer, bufferLen)
    CheckResult (retorno)
    
    ConsultarSAT = processResult(buffer, bufferLen)
End Function

Public Function ConsultarStatusOperacional() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = SAT_ConsultarStatusOperacional(buffer, bufferLen)
    CheckResult (retorno)
    
    ConsultarStatusOperacional = processResult(buffer, bufferLen)
End Function

Public Function ConsultarNumeroSessao(ByVal cNumeroDeSessao As Long) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = SAT_ConsultarNumeroSessao(cNumeroDeSessao, buffer, bufferLen)
    CheckResult (retorno)
    
    ConsultarNumeroSessao = processResult(buffer, bufferLen)
End Function

Public Function AtualizarSoftwareSAT() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = SAT_AtualizarSoftwareSAT(buffer, bufferLen)
    CheckResult (retorno)
    
    AtualizarSoftwareSAT = processResult(buffer, bufferLen)
End Function

Public Function ComunicarCertificadoICPBRASIL(ByVal certificado As String) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = SAT_ComunicarCertificadoICPBRASIL(certificado, buffer, bufferLen)
    CheckResult (retorno)
    
    ComunicarCertificadoICPBRASIL = processResult(buffer, bufferLen)
End Function

Public Sub ExtrairLogs(ByVal eArquivo As String)
    Dim retorno   As Long
    
    retorno = SAT_ExtrairLogs(eArquivo)
    CheckResult (retorno)
End Sub

Public Sub TesteFimAFim(ByVal eArquivoXmlVenda As String)
    Dim retorno   As Long
    
    retorno = SAT_TesteFimAFim(eArquivoXmlVenda)
    CheckResult (retorno)
End Sub

Public Function CriarCFe(ByVal eArquivoIni As String) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = SAT_CriarCFe(eArquivoIni, buffer, bufferLen)
    CheckResult (retorno)
    
    CriarCFe = processResult(buffer, bufferLen)
End Function

Public Function CriarEnviarCFe(ByVal eArquivoIni As String) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = SAT_CriarEnviarCFe(eArquivoIni, buffer, bufferLen)
    CheckResult (retorno)
    
    CriarEnviarCFe = processResult(buffer, bufferLen)
End Function

Public Function EnviarCFe(ByVal eArquivoXml As String) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = SAT_EnviarCFe(eArquivoXml, buffer, bufferLen)
    CheckResult (retorno)
    
    EnviarCFe = processResult(buffer, bufferLen)
End Function

Public Function CancelarCFe(ByVal eArquivoXml As String) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = SAT_CancelarCFe(eArquivoXml, buffer, bufferLen)
    CheckResult (retorno)
    
    CancelarCFe = processResult(buffer, bufferLen)
End Function

Public Sub ImprimirExtratoVenda(ByVal eArquivoXml As String, Optional ByVal eNomeImpressora As String = "")

    Dim retorno   As Long
    
    retorno = SAT_ImprimirExtratoVenda(eArquivoXml, eNomeImpressora)
    CheckResult (retorno)
End Sub

Public Sub ImprimirExtratoResumido(ByVal eArquivoXml As String, Optional ByVal eNomeImpressora As String = "")
    Dim retorno   As Long
    
    retorno = SAT_ImprimirExtratoResumido(eArquivoXml, eNomeImpressora)
    CheckResult (retorno)
End Sub

Public Function GerarPDFExtratoVenda(ByVal eArquivoXml As String, Optional ByVal eNomeArquivo As String = "") As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = SAT_GerarPDFExtratoVenda(eArquivoXml, eNomeArquivo, buffer, bufferLen)
    CheckResult (retorno)
    
    GerarPDFExtratoVenda = processResult(buffer, bufferLen)
End Function

Public Function GerarImpressaoFiscalMFe(ByVal eArquivoXml As String) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = SAT_GerarImpressaoFiscalMFe(eArquivoXml, buffer, bufferLen)
    CheckResult (retorno)
    
    GerarImpressaoFiscalMFe = processResult(buffer, bufferLen)
End Function

Public Sub EnviarEmail(ByVal eArquivoXml As String, ByVal ePara As String, _
    ByVal eAssunto As String, ByVal eNomeArquivo As String, ByVal sMensagem As String, _
    ByVal sCC As String, ByVal eAnexos As String)
    
    Dim retorno   As Long
    
    retorno = SAT_EnviarEmail(eArquivoXml, ePara, eAssunto, eNomeArquivo, sMensagem, _
                              sCC, eAnexos)
    CheckResult (retorno)
End Sub

Private Sub CheckResult(ByVal Resultado As Long)
    
    If Resultado = 0 Then Exit Sub
         
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    SAT_UltimoRetorno buffer, bufferLen
    
    Err.Raise Resultado, "ACBrSat", processResult(buffer, bufferLen)
End Sub

Private Function processResult(ByRef buffer As String, ByRef bufferLen As Long) As String
    
    If bufferLen > BUFFER_LENGTH Then
        buffer = String$(bufferLen, " ")
        SAT_UltimoRetorno buffer, bufferLen
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
