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

Private libHandler As Long

Const BUFFER_LENGTH As Long = 1024
                                
Private Declare Function SAT_Inicializar _
                Lib "ACBrSAT32.dll" (ByRef libHandler As Long, _
                                     ByVal eArqConfig As String, _
                                     ByVal eChaveCrypt As String) As Long
                   
Private Declare Function SAT_Finalizar Lib "ACBrSAT32.dll" (ByVal libHandler As Long) As Long

Private Declare Function SAT_Nome _
                Lib "ACBrSAT32.dll" (ByVal libHandler As Long, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                   
Private Declare Function SAT_Versao _
                Lib "ACBrSAT32.dll" (ByVal libHandler As Long, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long

Private Declare Function SAT_UltimoRetorno _
                Lib "ACBrSAT32.dll" (ByVal libHandler As Long, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                   
Private Declare Function SAT_ConfigImportar _
                Lib "ACBrSAT32.dll" (ByVal libHandler As Long, _
                                     ByVal eArqConfig As String) As Long
                   
Private Declare Function SAT_ConfigExportar _
                Lib "ACBrSAT32.dll" (ByVal libHandler As Long, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long

Private Declare Function SAT_ConfigLer _
                Lib "ACBrSAT32.dll" (ByVal libHandler As Long, _
                                     ByVal eArqConfig As String) As Long

Private Declare Function SAT_ConfigGravar _
                Lib "ACBrSAT32.dll" (ByVal libHandler As Long, _
                                     ByVal eArqConfig As String) As Long
                   
Private Declare Function SAT_ConfigLerValor _
                Lib "ACBrSAT32.dll" (ByVal libHandler As Long, _
                                     ByVal eSessao As String, _
                                     ByVal eChave As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long

Private Declare Function SAT_ConfigGravarValor _
                Lib "ACBrSAT32.dll" (ByVal libHandler As Long, _
                                     ByVal eSessao As String, _
                                     ByVal eChave As String, _
                                     ByVal valor As String) As Long
                                      
Private Declare Function SAT_InicializarSAT Lib "ACBrSAT32.dll" (ByVal libHandler As Long) As Long

Private Declare Function SAT_DesInicializar Lib "ACBrSAT32.dll" (ByVal libHandler As Long) As Long

Private Declare Function SAT_AtivarSAT _
                Lib "ACBrSAT32.dll" (ByVal libHandler As Long, _
                                     ByVal CNPJValue As String, _
                                     ByVal cUF As Long, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long

Private Declare Function SAT_AssociarAssinatura _
                Lib "ACBrSAT32.dll" (ByVal libHandler As Long, _
                                     ByVal CNPJValue As String, _
                                     ByVal assinaturaCNPJs As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                      
Private Declare Function SAT_BloquearSAT _
                Lib "ACBrSAT32.dll" (ByVal libHandler As Long, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                      
Private Declare Function SAT_DesbloquearSAT _
                Lib "ACBrSAT32.dll" (ByVal libHandler As Long, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                      
Private Declare Function SAT_TrocarCodigoDeAtivacao _
                Lib "ACBrSAT32.dll" (ByVal libHandler As Long, _
                                     ByVal codigoDeAtivacaoOuEmergencia As String, _
                                     ByVal opcao As Long, _
                                     ByVal novoCodigo As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                      
Private Declare Function SAT_ConsultarSAT _
                Lib "ACBrSAT32.dll" (ByVal libHandler As Long, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                      
Private Declare Function SAT_ConsultarStatusOperacional _
                Lib "ACBrSAT32.dll" (ByVal libHandler As Long, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                      
Private Declare Function SAT_ConsultarNumeroSessao _
                Lib "ACBrSAT32.dll" (ByVal libHandler As Long, _
                                     ByVal cNumeroDeSessao As Long, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                      
Private Declare Function SAT_AtualizarSoftwareSAT _
                Lib "ACBrSAT32.dll" (ByVal libHandler As Long, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                      
Private Declare Function SAT_ComunicarCertificadoICPBRASIL _
                Lib "ACBrSAT32.dll" (ByVal libHandler As Long, _
                                     ByVal certificado As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                      
Private Declare Function SAT_ExtrairLogs _
                Lib "ACBrSAT32.dll" (ByVal libHandler As Long, _
                                     ByVal eArquivo As String) As Long
                                     
Private Declare Function SAT_ValidarCFe _
                Lib "ACBrSAT32.dll" (ByVal libHandler As Long, _
                                     ByVal eArquivoXml As String) As Long
                
Private Declare Function SAT_TesteFimAFim _
                Lib "ACBrSAT32.dll" (ByVal libHandler As Long, _
                                     ByVal eArquivoXmlVenda As String) As Long
                
Private Declare Function SAT_CriarCFe _
                Lib "ACBrSAT32.dll" (ByVal libHandler As Long, _
                                     ByVal eArquivoIni As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                      
Private Declare Function SAT_CriarEnviarCFe _
                Lib "ACBrSAT32.dll" (ByVal libHandler As Long, _
                                     ByVal eArquivoIni As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long

Private Declare Function SAT_EnviarCFe _
                Lib "ACBrSAT32.dll" (ByVal libHandler As Long, _
                                     ByVal eArquivoXml As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                      
Private Declare Function SAT_CancelarCFe _
                Lib "ACBrSAT32.dll" (ByVal libHandler As Long, _
                                     ByVal eArquivoXml As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                      
Private Declare Function SAT_ImprimirExtratoVenda _
                Lib "ACBrSAT32.dll" (ByVal libHandler As Long, _
                                     ByVal eArquivoXml As String, _
                                     ByVal eNomeImpressora As String) As Long
                                      
Private Declare Function SAT_ImprimirExtratoResumido _
                Lib "ACBrSAT32.dll" (ByVal libHandler As Long, _
                                     ByVal eArquivoXml As String, _
                                     ByVal eNomeImpressora As String) As Long
                                      
Private Declare Function SAT_ImprimirExtratoCancelamento _
                Lib "ACBrSAT32.dll" (ByVal libHandler As Long, _
                                     ByVal eArqXMLVenda As String, _
                                     ByVal eArqXMLCancelamento As String, _
                                     ByVal eNomeImpressora As String) As Long
                                      
Private Declare Function SAT_GerarPDFExtratoVenda _
                Lib "ACBrSAT32.dll" (ByVal libHandler As Long, _
                                     ByVal eArquivoXml As String, _
                                     ByVal eNomeArquivo As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                      
Private Declare Function SAT_GerarPDFCancelamento _
                Lib "ACBrSAT32.dll" (ByVal libHandler As Long, _
                                     ByVal eArqXMLVenda As String, _
                                     ByVal eArqXMLCancelamento As String, _
                                     ByVal eNomeArquivo As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                      
Private Declare Function SAT_GerarImpressaoFiscalMFe _
                Lib "ACBrSAT32.dll" (ByVal libHandler As Long, _
                                     ByVal eArquivoXml As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                      
Private Declare Function SAT_EnviarEmail _
                Lib "ACBrSAT32.dll" (ByVal libHandler As Long, _
                                     ByVal eArquivoXml As String, _
                                     ByVal ePara As String, _
                                     ByVal eAssunto As String, _
                                     ByVal eNomeArquivo As String, _
                                     ByVal sMensagem As String, _
                                     ByVal sCC As String, _
                                     ByVal eAnexos As String) As Long

Public Sub InicializarLib(Optional ByVal eArqConfig As String = "", _
                          Optional ByVal eChaveCrypt As String = "")

    SetLibPath
    
    Dim retorno As Long

    retorno = SAT_Inicializar(libHandler, eArqConfig, eChaveCrypt)
    CheckResult (retorno)
End Sub

Public Sub FinalizarLib()

    Dim retorno As Long

    retorno = SAT_Finalizar(libHandler)
    CheckResult (retorno)
End Sub

Public Sub ConfigImportar(ByVal eArqConfig As String)

    Dim retorno As Long

    retorno = SAT_ConfigImportar(libHandler, eArqConfig)
    CheckResult (retorno)
    
End Sub
    
Public Function ConfigExportar() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = SAT_ConfigExportar(libHandler, buffer, bufferLen)
    CheckResult retorno
    
    ConfigExportar = processResult(buffer, bufferLen)
    
End Function

Public Sub ConfigLer(Optional eArqConfig As String = "")

    Dim retorno As Long

    retorno = SAT_ConfigLer(libHandler, eArqConfig)
    CheckResult (retorno)
End Sub

Public Sub ConfigGravar(Optional eArqConfig As String = "")

    Dim retorno As Long

    retorno = SAT_ConfigGravar(libHandler, eArqConfig)
    CheckResult (retorno)
End Sub

Public Function ConfigLerValor(ByVal eSessao As String, ByVal eChave As String) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen + 1, " ")
    
    retorno = SAT_ConfigLerValor(libHandler, eSessao, eChave, buffer, bufferLen)
    CheckResult (retorno)
    
    ConfigLerValor = processResult(buffer, bufferLen)
End Function

Public Sub ConfigGravarValor(ByVal eSessao As String, _
                             ByVal eChave As String, _
                             ByVal valor As String)

    Dim retorno As Long
    
    retorno = SAT_ConfigGravarValor(libHandler, eSessao, eChave, valor)
    CheckResult (retorno)
End Sub

Public Function Nome() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
        
    retorno = SAT_Nome(libHandler, buffer, bufferLen)
    CheckResult (retorno)
    
    Nome = processResult(buffer, bufferLen)
End Function

Public Function Versao() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = SAT_Versao(libHandler, buffer, bufferLen)
    CheckResult (retorno)
    
    Versao = processResult(buffer, bufferLen)
End Function

Public Sub Inicializar()
    Dim retorno   As Long
    
    retorno = SAT_InicializarSAT(libHandler)
    CheckResult (retorno)
End Sub

Public Sub DesInicializar()
    Dim retorno   As Long
    
    retorno = SAT_DesInicializar(libHandler)
    CheckResult (retorno)
End Sub

Public Function AtivarSAT(ByVal CNPJValue As String, ByVal cUF As Long) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = SAT_AtivarSAT(libHandler, CNPJValue, cUF, buffer, bufferLen)
    CheckResult (retorno)
    
    AtivarSAT = processResult(buffer, bufferLen)
End Function

Public Function AssociarAssinatura(ByVal CNPJValue As String, ByVal assinaturaCNPJs As String) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = SAT_AssociarAssinatura(libHandler, CNPJValue, assinaturaCNPJs, buffer, bufferLen)
    CheckResult (retorno)
    
    AssociarAssinatura = processResult(buffer, bufferLen)
End Function

Public Function BloquearSAT() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = SAT_BloquearSAT(libHandler, buffer, bufferLen)
    CheckResult (retorno)
    
    BloquearSAT = processResult(buffer, bufferLen)
End Function

Public Function DesbloquearSAT() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = SAT_DesbloquearSAT(libHandler, buffer, bufferLen)
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
    
    retorno = SAT_TrocarCodigoDeAtivacao(libHandler, codigoDeAtivacaoOuEmergencia, opcao, novoCodigo, buffer, bufferLen)
    CheckResult (retorno)
    
    TrocarCodigoDeAtivacao = processResult(buffer, bufferLen)
End Function

Public Function ConsultarSAT() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = SAT_ConsultarSAT(libHandler, buffer, bufferLen)
    CheckResult (retorno)
    
    ConsultarSAT = processResult(buffer, bufferLen)
End Function

Public Function ConsultarStatusOperacional() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = SAT_ConsultarStatusOperacional(libHandler, buffer, bufferLen)
    CheckResult (retorno)
    
    ConsultarStatusOperacional = processResult(buffer, bufferLen)
End Function

Public Function ConsultarNumeroSessao(ByVal cNumeroDeSessao As Long) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = SAT_ConsultarNumeroSessao(libHandler, cNumeroDeSessao, buffer, bufferLen)
    CheckResult (retorno)
    
    ConsultarNumeroSessao = processResult(buffer, bufferLen)
End Function

Public Function AtualizarSoftwareSAT() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = SAT_AtualizarSoftwareSAT(libHandler, buffer, bufferLen)
    CheckResult (retorno)
    
    AtualizarSoftwareSAT = processResult(buffer, bufferLen)
End Function

Public Function ComunicarCertificadoICPBRASIL(ByVal certificado As String) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = SAT_ComunicarCertificadoICPBRASIL(libHandler, certificado, buffer, bufferLen)
    CheckResult (retorno)
    
    ComunicarCertificadoICPBRASIL = processResult(buffer, bufferLen)
End Function

Public Sub ExtrairLogs(ByVal eArquivo As String)
    Dim retorno   As Long
    
    retorno = SAT_ExtrairLogs(libHandler, eArquivo)
    CheckResult (retorno)
End Sub

Public Sub ValidarCFe(ByVal eArquivoXml As String)
    Dim retorno As Long
    
    retorno = SAT_ValidarCFe(libHandler, eArquivoXml)
    CheckResult (retorno)
End Sub

Public Sub TesteFimAFim(ByVal eArquivoXmlVenda As String)
    Dim retorno   As Long
    
    retorno = SAT_TesteFimAFim(libHandler, eArquivoXmlVenda)
    CheckResult (retorno)
End Sub

Public Function CriarCFe(ByVal eArquivoIni As String) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = SAT_CriarCFe(libHandler, eArquivoIni, buffer, bufferLen)
    CheckResult (retorno)
    
    CriarCFe = processResult(buffer, bufferLen)
End Function

Public Function CriarEnviarCFe(ByVal eArquivoIni As String) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = SAT_CriarEnviarCFe(libHandler, eArquivoIni, buffer, bufferLen)
    CheckResult (retorno)
    
    CriarEnviarCFe = processResult(buffer, bufferLen)
End Function

Public Function EnviarCFe(ByVal eArquivoXml As String) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = SAT_EnviarCFe(libHandler, eArquivoXml, buffer, bufferLen)
    CheckResult (retorno)
    
    EnviarCFe = processResult(buffer, bufferLen)
End Function

Public Function CancelarCFe(ByVal eArquivoXml As String) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = SAT_CancelarCFe(libHandler, eArquivoXml, buffer, bufferLen)
    CheckResult (retorno)
    
    CancelarCFe = processResult(buffer, bufferLen)
End Function

Public Sub ImprimirExtratoVenda(ByVal eArquivoXml As String, Optional ByVal eNomeImpressora As String = "")

    Dim retorno   As Long
    
    retorno = SAT_ImprimirExtratoVenda(libHandler, eArquivoXml, eNomeImpressora)
    CheckResult (retorno)
End Sub

Public Sub ImprimirExtratoResumido(ByVal eArquivoXml As String, Optional ByVal eNomeImpressora As String = "")
    Dim retorno   As Long
    
    retorno = SAT_ImprimirExtratoResumido(libHandler, eArquivoXml, eNomeImpressora)
    CheckResult (retorno)
End Sub

Public Sub ImprimirExtratoCancelamento(ByVal eArqXMLVenda As String, ByVal eArqXMLCancelamento As String, Optional ByVal eNomeImpressora As String = "")
    Dim retorno   As Long
    
    retorno = SAT_ImprimirExtratoCancelamento(libHandler, eArqXMLVenda, eArqXMLCancelamento, eNomeImpressora)
    CheckResult (retorno)
End Sub

Public Function GerarPDFExtratoVenda(ByVal eArquivoXml As String, Optional ByVal eNomeArquivo As String = "") As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = SAT_GerarPDFExtratoVenda(libHandler, eArquivoXml, eNomeArquivo, buffer, bufferLen)
    CheckResult (retorno)
    
    GerarPDFExtratoVenda = processResult(buffer, bufferLen)
End Function

Public Function GerarPDFCancelamento(ByVal eArqXMLVenda As String, ByVal eArqXMLCancelamento As String, Optional ByVal eNomeArquivo As String = "") As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = SAT_GerarPDFCancelamento(libHandler, eArqXMLVenda, eArqXMLCancelamento, eNomeArquivo, buffer, bufferLen)
    CheckResult (retorno)
    
    GerarPDFCancelamento = processResult(buffer, bufferLen)
End Function

Public Function GerarImpressaoFiscalMFe(ByVal eArquivoXml As String) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = SAT_GerarImpressaoFiscalMFe(libHandler, eArquivoXml, buffer, bufferLen)
    CheckResult (retorno)
    
    GerarImpressaoFiscalMFe = processResult(buffer, bufferLen)
End Function

Public Sub EnviarEmail(ByVal eArquivoXml As String, ByVal ePara As String, _
    ByVal eAssunto As String, ByVal eNomeArquivo As String, ByVal sMensagem As String, _
    ByVal sCC As String, ByVal eAnexos As String)
    
    Dim retorno   As Long
    
    retorno = SAT_EnviarEmail(libHandler, eArquivoXml, ePara, eAssunto, eNomeArquivo, sMensagem, _
                              sCC, eAnexos)
    CheckResult (retorno)
End Sub

Private Sub CheckResult(ByVal Resultado As Long)
    
    If Resultado = 0 Then Exit Sub
         
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    SAT_UltimoRetorno libHandler, buffer, bufferLen
    
    Err.Raise Resultado, "ACBrSat", processResult(buffer, bufferLen)
End Sub

Private Function processResult(ByRef buffer As String, ByRef bufferLen As Long) As String
    
    If bufferLen > BUFFER_LENGTH Then
        buffer = String$(bufferLen, " ")
        SAT_UltimoRetorno libHandler, buffer, bufferLen
    End If

    processResult = Trim$(FromUTF8(buffer))
End Function
