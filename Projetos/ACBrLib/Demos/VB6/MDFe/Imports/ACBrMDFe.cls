VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ACBrMDFe"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Const BUFFER_LENGTH As Long = 1024
                                
Private Declare Function MDFE_Inicializar _
                Lib "ACBrMDFE32.dll" (ByVal eArqConfig As String, _
                                      ByVal eChaveCrypt As String) As Long
                   
Private Declare Function MDFE_Finalizar Lib "ACBrMDFE32.dll" () As Long

Private Declare Function MDFE_Nome _
                Lib "ACBrMDFE32.dll" (ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                   
Private Declare Function MDFE_Versao _
                Lib "ACBrMDFE32.dll" (ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long

Private Declare Function MDFE_UltimoRetorno _
                Lib "ACBrMDFE32.dll" (ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                   
Private Declare Function MDFE_ConfigImportar _
                Lib "ACBrMDFE32.dll" (ByVal eArqConfig As String) As Long
                   
Private Declare Function MDFE_ConfigExportar _
                Lib "ACBrMDFE32.dll" (ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                   
Private Declare Function MDFE_ConfigLer _
                Lib "ACBrMDFE32.dll" (ByVal eArqConfig As String) As Long

Private Declare Function MDFE_ConfigGravar _
                Lib "ACBrMDFE32.dll" (ByVal eArqConfig As String) As Long
                   
Private Declare Function MDFE_ConfigLerValor _
                Lib "ACBrMDFE32.dll" (ByVal eSessao As String, _
                                      ByVal eChave As String, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long

Private Declare Function MDFE_ConfigGravarValor _
                Lib "ACBrMDFE32.dll" (ByVal eSessao As String, _
                                      ByVal eChave As String, _
                                      ByVal valor As String) As Long
                                      
Private Declare Function MDFE_CarregarXML _
                Lib "ACBrMDFE32.dll" (ByVal eArquivoOuXml As String) As Long
                
Private Declare Function MDFE_CarregarINI _
                Lib "ACBrMDFE32.dll" (ByVal eArquivoOuIni As String) As Long
                
Private Declare Function MDFE_ObterXml _
                Lib "ACBrMDFE32.dll" (ByVal AIndex As Long, _
                                     ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Private Declare Function MDFE_GravarXml _
                Lib "ACBrMDFE32.dll" (ByVal AIndex As Long, _
                                     ByVal eNomeArquivo As String, _
                                     ByVal ePathArquivo As String) As Long
                                     
Private Declare Function MDFE_ObterIni _
                Lib "ACBrMDFE32.dll" (ByVal AIndex As Long, _
                                     ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Private Declare Function MDFE_GravarIni _
                Lib "ACBrMDFE32.dll" (ByVal AIndex As Long, _
                                     ByVal eNomeArquivo As String, _
                                     ByVal ePathArquivo As String) As Long
                
Private Declare Function MDFE_CarregarEventoXML _
                Lib "ACBrMDFE32.dll" (ByVal eArquivoOuXml As String) As Long
                
Private Declare Function MDFE_CarregarEventoINI _
                Lib "ACBrMDFE32.dll" (ByVal eArquivoOuIni As String) As Long
                
Private Declare Function MDFE_LimparLista Lib "ACBrMDFE32.dll" () As Long

Private Declare Function MDFE_LimparListaEventos Lib "ACBrMDFE32.dll" () As Long

Private Declare Function MDFE_Assinar Lib "ACBrMDFE32.dll" () As Long

Private Declare Function MDFE_Validar Lib "ACBrMDFE32.dll" () As Long

Private Declare Function MDFE_ValidarRegrasdeNegocios _
                Lib "ACBrMDFE32.dll" (ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Private Declare Function MDFE_VerificarAssinatura _
                Lib "ACBrMDFE32.dll" (ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Private Declare Function MDFE_GerarChave _
                Lib "ACBrMDFE32.dll" (ByVal ACodigoUF As Long, _
                                     ByVal ACodigoNumerico As Long, _
                                     ByVal AModelo As Long, _
                                     ByVal ASerie As Long, _
                                     ByVal ANumero As Long, _
                                     ByVal ATpEmi As Long, _
                                     ByVal AEmissao As String, _
                                     ByVal ACNPJCPF As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                     
Private Declare Function MDFE_ObterCertificados _
                Lib "ACBrMDFE32.dll" (ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Private Declare Function MDFE_GetPath _
                Lib "ACBrMDFE32.dll" (ByVal tipo As Long, _
                                     ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Private Declare Function MDFE_GetPathEvento _
                Lib "ACBrMDFE32.dll" (ByVal aCodEvento As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                    
Private Declare Function MDFE_StatusServico _
                Lib "ACBrMDFE32.dll" (ByVal buffer As String, _
                                      ByRef bufferSize As Long) As Long
                                     
Private Declare Function MDFE_Consultar _
                Lib "ACBrMDFE32.dll" (ByVal eChaveOuNFe As String, _
                                      ByVal AExtrairEventos As Boolean, _
                                      ByVal buffer As String, _
                                      ByRef bufferSize As Long) As Long
                                     
Private Declare Function MDFE_Enviar _
                Lib "ACBrMDFE32.dll" (ByVal aLote As Long, _
                                     ByVal Imprimir As Boolean, _
                                     ByVal sincrono As Boolean, _
                                     ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Private Declare Function MDFE_ConsultarRecibo _
                Lib "ACBrMDFE32.dll" (ByVal aRecibo As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Private Declare Function MDFE_Cancelar _
                Lib "ACBrMDFE32.dll" (ByVal eChave As String, _
                                     ByVal eJustificativa As String, _
                                     ByVal eCNPJ As String, _
                                     ByVal aLote As Long, _
                                     ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Private Declare Function MDFE_EnviarEvento _
                Lib "ACBrMDFE32.dll" (ByVal aLote As Long, _
                                     ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Private Declare Function MDFE_DistribuicaoDFePorUltNSU _
                Lib "ACBrMDFE32.dll" (ByVal acUFAutor As Long, _
                                     ByVal eCnpjcpf As String, _
                                     ByVal eultNsu As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Private Declare Function MDFE_DistribuicaoDFePorNSU _
                Lib "ACBrMDFE32.dll" (ByVal acUFAutor As Long, _
                                     ByVal eCnpjcpf As String, _
                                     ByVal eNsu As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Private Declare Function MDFE_DistribuicaoDFePorChave _
                Lib "ACBrMDFE32.dll" (ByVal acUFAutor As Long, _
                                     ByVal eCnpjcpf As String, _
                                     ByVal echNFe As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferSize As Long) As Long
                                     
Private Declare Function MDFE_EnviarEmail _
                Lib "ACBrMDFE32.dll" (ByVal ePara As Long, _
                                     ByVal eChaveNFe As String, _
                                     ByVal aEnviaPDF As Boolean, _
                                     ByVal eAssunto As String, _
                                     ByVal eCc As String, _
                                     ByVal eAnexos As String, _
                                     ByVal eMensagem As String) As Long
                                     
Private Declare Function MDFE_EnviarEmailEvento _
                Lib "ACBrMDFE32.dll" (ByVal ePara As Long, _
                                     ByVal eChaveNFe As String, _
                                     ByVal aEnviaPDF As Boolean, _
                                     ByVal eAssunto As String, _
                                     ByVal eCc As String, _
                                     ByVal eAnexos As String, _
                                     ByVal eMensagem As String) As Long
                                     
Private Declare Function MDFE_Imprimir _
                Lib "ACBrMDFE32.dll" (ByVal cImpressora As String, _
                                     ByVal nNumCopias As Long, _
                                     ByVal cProtocolo As String, _
                                     ByVal bMostrarPreview As String) As Long
                                     
Private Declare Function MDFE_ImprimirPDF Lib "ACBrMDFE32.dll" () As Long

Private Declare Function MDFE_ImprimirEvento _
                Lib "ACBrMDFE32.dll" (ByVal eArquivoXmlNFe As String, _
                                     ByVal eArquivoXmlEvento As String) As Long
                                     
Private Declare Function MDFE_ImprimirEventoPDF _
                Lib "ACBrMDFE32.dll" (ByVal eArquivoXmlNFe As String, _
                                     ByVal eArquivoXmlEvento As String) As Long

Private Declare Function MDFE_ConsultaMDFeNaoEnc _
                Lib "ACBrMDFE32.dll" (ByVal nCNPJ As String, _
                                      ByVal buffer As String, _
                                      ByRef bufferSize As Long) As Long

Private Declare Function MDFE_EncerrarMDFe _
                Lib "ACBrMDFE32.dll" (ByVal eChaveouMDFe As String, _
                                      ByVal eDtEnc As String, _
                                      ByVal cMunicipioDescarga As String, _
                                      ByVal nCNPJ As String, _
                                      ByVal nProtocolo As String, _
                                      ByVal buffer As String, _
                                      ByRef bufferSize As Long) As Long


Public Sub InicializarLib(Optional ByVal eArqConfig As String = "", _
                          Optional ByVal eChaveCrypt As String = "")
                          
    SetLibPath

    Dim retorno As Long

    retorno = MDFE_Inicializar(eArqConfig, eChaveCrypt)
    CheckResult retorno
End Sub

Public Sub FinalizarLib()

    Dim retorno As Long

    retorno = MDFE_Finalizar
    CheckResult retorno
End Sub

Public Sub EncerrarMDFe(ByVal eChaveouMDFe As String, ByVal eDtEnc As String, _
                        ByVal cMunicipioDescarga As String, _
                        Optional ByVal nCNPJ As String = "", _
                        Optional ByVal nProtocolo As String = "")

    Dim retorno As Long
    Dim buffer As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    
    retorno = MDFE_EncerrarMDFe(eChaveouMDFe, eDtEnc, cMunicipioDescarga, nCNPJ, nProtocolo, buffer, bufferLen)
    CheckResult retorno

End Sub

Public Sub EnviarEmailEvento(ByVal ePara As String, ByVal eChaveNFe As String, ByVal aEnviaPDF As Boolean, _
                       ByVal eAssunto As String, ByVal eMensagem As String, _
                       Optional ByVal eCc As String = "", Optional ByVal eAnexos As String = "")
    Dim retorno   As Long
    
    retorno = MDFE_EnviarEmailEvento(ePara, eChaveNFe, aEnviaPDF, eAssunto, eCc, eAnexos, eMensagem)
    CheckResult retorno
End Sub

Public Sub EnviarEmail(ByVal ePara As String, ByVal eChaveNFe As String, ByVal aEnviaPDF As Boolean, _
                       ByVal eAssunto As String, ByVal eMensagem As String, _
                       Optional ByVal eCc As String = "", Optional ByVal eAnexos As String = "")
    Dim retorno   As Long
    
    retorno = MDFE_EnviarEmail(ePara, eChaveNFe, aEnviaPDF, eAssunto, eCc, eAnexos, eMensagem)
    CheckResult retorno
End Sub

Public Function ConsultaMDFeNaoEnc(ByVal nCNPJ As String) As String

    Dim retorno As Long
    Dim buffer As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = MDFE_ConsultaMDFeNaoEnc(nCNPJ, buffer, bufferLen)
    CheckResult retorno
    
    ConsultaMDFeNaoEnc = processResult(buffer, bufferLen)

End Function

Public Sub ConfigImportar(ByVal eArqConfig As String)

    Dim retorno As Long

    retorno = MDFE_ConfigImportar(eArqConfig)
    CheckResult (retorno)
    
End Sub
    
Public Function ConfigExportar() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = MDFE_ConfigExportar(buffer, bufferLen)
    CheckResult retorno
    
    ConfigExportar = processResult(buffer, bufferLen)
    
End Function

Public Sub ConfigLer(Optional eArqConfig As String = "")

    Dim retorno As Long

    retorno = MDFE_ConfigLer(eArqConfig)
    CheckResult retorno
End Sub

Public Sub ConfigGravar(Optional eArqConfig As String = "")

    Dim retorno As Long

    retorno = MDFE_ConfigGravar(eArqConfig)
    CheckResult retorno
End Sub

Public Function ConfigLerValor(ByVal eSessao As String, ByVal eChave As String) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = MDFE_ConfigLerValor(eSessao, eChave, buffer, bufferLen)
    CheckResult retorno
    
    ConfigLerValor = processResult(buffer, bufferLen)
End Function

Public Sub ConfigGravarValor(ByVal eSessao As String, _
                             ByVal eChave As String, _
                             ByVal valor As String)

    Dim retorno As Long
    
    retorno = MDFE_ConfigGravarValor(eSessao, eChave, valor)
    CheckResult retorno
End Sub

Public Function Nome() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
        
    retorno = MDFE_Nome(buffer, bufferLen)
    CheckResult retorno
    
    Nome = processResult(buffer, bufferLen)
End Function

Public Function Versao() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = MDFE_Versao(buffer, bufferLen)
    CheckResult retorno
    
    Versao = processResult(buffer, bufferLen)
End Function

Public Sub CarregarXML(ByVal eArquivoOuXml As String)
    Dim retorno   As Long
    retorno = MDFE_CarregarXML(eArquivoOuXml)
    CheckResult retorno
End Sub

Public Sub CarregarINI(ByVal eArquivoOuIni As String)
    Dim retorno   As Long
    retorno = MDFE_CarregarINI(eArquivoOuIni)
    CheckResult retorno
End Sub

Public Function ObterXml(ByVal AIndex As Long) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = MDFE_ObterXml(AIndex, buffer, bufferLen)
    CheckResult retorno
    
    ObterXml = processResult(buffer, bufferLen)
End Function

Public Sub GravarXml(ByVal AIndex As Long, _
                     Optional ByVal eNomeArquivo As String = "", _
                     Optional ByVal ePathArquivo As String = "")
    Dim retorno   As Long
    retorno = MDFE_GravarXml(AIndex, eNomeArquivo, ePathArquivo)
    CheckResult retorno
End Sub

Public Function ObterIni(ByVal AIndex As Long) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = MDFE_ObterIni(AIndex, buffer, bufferLen)
    CheckResult retorno
    
    ObterIni = processResult(buffer, bufferLen)
End Function

Public Sub GravarIni(ByVal AIndex As Long, _
                     ByVal eNomeArquivo As String, _
                     Optional ByVal ePathArquivo As String = "")
    Dim retorno   As Long
    retorno = MDFE_GravarIni(AIndex, eNomeArquivo, ePathArquivo)
    CheckResult retorno
End Sub

Public Sub CarregarEventoXML(ByVal eArquivoOuXml As String)
    Dim retorno   As Long
    retorno = MDFE_CarregarEventoXML(eArquivoOuXml)
    CheckResult retorno
End Sub

Public Sub CarregarEventoINI(ByVal eArquivoOuIni As String)
    Dim retorno   As Long
    retorno = MDFE_CarregarEventoINI(eArquivoOuIni)
    CheckResult retorno
End Sub

Public Sub LimparLista()
    Dim retorno   As Long
    retorno = MDFE_LimparLista
    CheckResult retorno
End Sub

Public Sub LimparListaEventos()
    Dim retorno   As Long
    retorno = MDFE_LimparListaEventos
    CheckResult retorno
End Sub

Public Sub Assinar()
    Dim retorno   As Long
    retorno = MDFE_Assinar
    CheckResult retorno
End Sub

Public Sub Validar()
    Dim retorno   As Long
    retorno = MDFE_Validar
    CheckResult retorno
End Sub

Public Function ValidarRegrasdeNegocios() As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = MDFE_ValidarRegrasdeNegocios(buffer, bufferLen)
    CheckResult retorno
    
    ValidarRegrasdeNegocios = processResult(buffer, bufferLen)
End Function

Public Function VerificarAssinatura() As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = MDFE_VerificarAssinatura(buffer, bufferLen)
    CheckResult retorno
    
    VerificarAssinatura = processResult(buffer, bufferLen)
End Function

Public Function GerarChave(ByVal ACodigoUF As Long, ByVal ACodigoNumerico As Long, _
                           ByVal AModelo As Long, ByVal ASerie As Long, ByVal ANumero As Long, _
                           ByVal ATpEmi As Long, ByVal AEmissao As Date, ByVal ACNPJCPF As String) As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = MDFE_GerarChave(ACodigoUF, ACodigoNumerico, AModelo, ASerie, ANumero, ATpEmi, _
                             Format(AEmissao, "dd/MM/yyyy"), ACNPJCPF, buffer, bufferLen)
    CheckResult retorno
    
    GerarChave = processResult(buffer, bufferLen)
End Function

Public Function ObterCertificados() As String()
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = MDFE_ObterCertificados(buffer, bufferLen)
    CheckResult retorno
    
    ObterCertificados = Split(processResult(buffer, bufferLen), vbCrLf)
    
End Function

Public Function GetPath(ByVal tipo As Long) As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = MDFE_GetPath(tipo, buffer, bufferLen)
    CheckResult retorno
    
    GetPath = processResult(buffer, bufferLen)
End Function

Public Function GetPathEvento(ByVal aCodEvento As String) As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = MDFE_GetPathEvento(aCodEvento, buffer, bufferLen)
    CheckResult retorno
    
    GetPathEvento = processResult(buffer, bufferLen)
End Function

Public Function StatusServico() As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = MDFE_StatusServico(buffer, bufferLen)
    CheckResult retorno
    
    StatusServico = processResult(buffer, bufferLen)
End Function

Public Function Consultar(ByVal eChaveOuNFe As String, Optional ByVal AExtrairEventos As Boolean = False) As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = MDFE_Consultar(eChaveOuNFe, AExtrairEventos, buffer, bufferLen)
    CheckResult retorno
    
    Consultar = processResult(buffer, bufferLen)
End Function

Public Function Enviar(ByVal aLote As Long, Optional ByVal Imprimir As Boolean = False, _
                       Optional ByVal sincrono As Boolean = False) As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = MDFE_Enviar(aLote, Imprimir, sincrono, buffer, bufferLen)
    CheckResult retorno
    
    Enviar = processResult(buffer, bufferLen)
End Function

Public Function ConsultarRecibo(ByVal aRecibo As String) As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = MDFE_ConsultarRecibo(aRecibo, buffer, bufferLen)
    CheckResult retorno
    
    ConsultarRecibo = processResult(buffer, bufferLen)
End Function

Public Function Cancelar(ByVal eChave As String, ByVal eJustificativa As String, _
                         ByVal eCNPJ As String, ByVal aLote As Long) As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = MDFE_Cancelar(eChave, eJustificativa, eCNPJ, aLote, buffer, bufferLen)
    CheckResult retorno
    
    Cancelar = processResult(buffer, bufferLen)
End Function

Public Function EnviarEvento(ByVal aLote As Long) As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = MDFE_EnviarEvento(aLote, buffer, bufferLen)
    CheckResult retorno
    
    EnviarEvento = processResult(buffer, bufferLen)
End Function

Public Function DistribuicaoDFePorUltNSU(ByVal acUFAutor As String, ByVal eCnpjcpf As String, ByVal eultNsu As String) As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = MDFE_DistribuicaoDFePorUltNSU(acUFAutor, eCnpjcpf, eultNsu, buffer, bufferLen)
    CheckResult retorno
    
    DistribuicaoDFePorUltNSU = processResult(buffer, bufferLen)
End Function

Public Function DistribuicaoDFePorNSU(ByVal acUFAutor As String, ByVal eCnpjcpf As String, ByVal eNsu As String) As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = MDFE_DistribuicaoDFePorNSU(acUFAutor, eCnpjcpf, eNsu, buffer, bufferLen)
    CheckResult retorno
    
    DistribuicaoDFePorNSU = processResult(buffer, bufferLen)
End Function

Public Function DistribuicaoDFePorChave(ByVal acUFAutor As String, ByVal eCnpjcpf As String, ByVal echNFe As String) As String
    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = MDFE_DistribuicaoDFePorChave(acUFAutor, eCnpjcpf, echNFe, buffer, bufferLen)
    CheckResult retorno
    
    DistribuicaoDFePorChave = processResult(buffer, bufferLen)
End Function

Public Sub Imprimir(Optional ByVal cImpressora As String = "", Optional ByVal nNumCopias As Long = 1, _
                    Optional ByVal cProtocolo As String = "", Optional ByVal bMostrarPreview As String = "")
    Dim retorno   As Long
    
    retorno = MDFE_Imprimir(cImpressora, nNumCopias, cProtocolo, bMostrarPreview)
    CheckResult retorno
End Sub

Public Sub ImprimirPDF()
    Dim retorno   As Long
    
    retorno = MDFE_ImprimirPDF()
    CheckResult retorno
End Sub

Public Sub ImprimirEvento(ByVal eArquivoXmlNFe As String, ByVal eArquivoXmlEvento As String)
    Dim retorno   As Long
    
    retorno = MDFE_ImprimirEvento(eArquivoXmlNFe, eArquivoXmlEvento)
    CheckResult retorno
End Sub

Public Sub ImprimirEventoPDF(ByVal eArquivoXmlNFe As String, ByVal eArquivoXmlEvento As String)
    Dim retorno   As Long
   
    retorno = MDFE_ImprimirEventoPDF(eArquivoXmlNFe, eArquivoXmlEvento)
    CheckResult retorno
End Sub

Private Sub CheckResult(ByVal Resultado As Long)
    
    If Resultado >= 0 Then Exit Sub
         
    Dim buffer As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    MDFE_UltimoRetorno buffer, bufferLen
    
    Err.Raise Resultado, "ACBrMDFe", processResult(buffer, bufferLen)
End Sub

Private Function processResult(ByRef buffer As String, ByRef bufferLen As Long) As String
    
    If bufferLen > BUFFER_LENGTH Then
        buffer = String$(bufferLen, " ")
        MDFE_UltimoRetorno buffer, bufferLen
    End If

    processResult = Trim$(FromUTF8(buffer))
End Function
