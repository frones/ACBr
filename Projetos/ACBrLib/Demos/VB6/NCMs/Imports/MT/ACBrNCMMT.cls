VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ACBrNCM"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Private libHandler As Long

Const BUFFER_LENGTH As Long = 1024
                               
Private Declare Function NCM_Inicializar _
                Lib "ACBrNCMs32.dll" (ByRef libHandler As Long, _
                                      ByVal eArqConfig As String, _
                                      ByVal eChaveCrypt As String) As Long
                   
Private Declare Function NCM_Finalizar Lib "ACBrNCMs32.dll" (ByVal libHandler As Long) As Long

Private Declare Function NCM_Nome _
                Lib "ACBrNCMs32.dll" (ByVal libHandler As Long, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                   
Private Declare Function NCM_Versao _
                Lib "ACBrNCMs32.dll" (ByVal libHandler As Long, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long

Private Declare Function NCM_UltimoRetorno _
                Lib "ACBrNCMs32.dll" (ByVal libHandler As Long, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                   
Private Declare Function NCM_ConfigImportar _
                Lib "ACBrNCMs32.dll" (ByVal libHandler As Long, _
                                      ByVal eArqConfig As String) As Long
                   
Private Declare Function NCM_ConfigExportar _
                Lib "ACBrNCMs32.dll" (ByVal libHandler As Long, _
                                      ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                   
Private Declare Function NCM_ConfigLer _
                Lib "ACBrNCMs32.dll" (ByVal libHandler As Long, _
                                      ByVal eArqConfig As String) As Long

Private Declare Function NCM_ConfigGravar _
                Lib "ACBrNCMs32.dll" (ByVal libHandler As Long, _
                                      ByVal eArqConfig As String) As Long
                   
Private Declare Function NCM_ConfigLerValor _
                Lib "ACBrNCMs32.dll" (ByVal libHandler As Long, _
                                      ByVal eSessao As String, _
                                      ByVal eChave As String, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long

Private Declare Function NCM_ConfigGravarValor _
                Lib "ACBrNCMs32.dll" (ByVal libHandler As Long, _
                                      ByVal eSessao As String, _
                                      ByVal eChave As String, _
                                      ByVal valor As String) As Long
                                      
Private Declare Function NCM_DescricaoNCM _
                Lib "ACBrNCMs32.dll" (ByVal libHandler As Long, _
                                      ByVal cNCM As String, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                                      
Private Declare Function NCM_Validar _
                Lib "ACBrNCMs32.dll" (ByVal libHandler As Long, _
                                      ByVal cNCM As String, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                                      
Private Declare Function NCM_BaixarLista _
                Lib "ACBrNCMs32.dll" (ByVal libHandler As Long, _
                                      ByVal cNomeArquivo As String, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                                                                            
Private Declare Function NCM_ObterNCMs _
                Lib "ACBrNCMs32.dll" (ByVal libHandler As Long, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                                                                            
Private Declare Function NCM_BuscarPorCodigo _
                Lib "ACBrNCMs32.dll" (ByVal libHandler As Long, _
                                      ByVal cNCM As String, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                                      
Private Declare Function NCM_BuscarPorDescricao _
                Lib "ACBrNCMs32.dll" (ByVal libHandler As Long, _
                                      ByVal cDesc As String, _
                                      ByVal nTipo As Long, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                                                                            
Public Sub InicializarLib(Optional ByVal eArqConfig As String = "", _
                          Optional ByVal eChaveCrypt As String = "")

    SetLibPath
    Dim retorno As Long

    retorno = NCM_Inicializar(libHandler, eArqConfig, eChaveCrypt)
    CheckResult retorno
End Sub

Public Sub FinalizarLib()

    Dim retorno As Long

    retorno = NCM_Finalizar(libHandler)
    CheckResult retorno
End Sub

Public Sub ConfigImportar(ByVal eArqConfig As String)

    Dim retorno As Long

    retorno = NCM_ConfigImportar(libHandler, eArqConfig)
    CheckResult (retorno)
    
End Sub
    
Public Function ConfigExportar() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = NCM_ConfigExportar(libHandler, buffer, bufferLen)
    CheckResult retorno
    
    ConfigExportar = processResult(buffer, bufferLen)
    
End Function

Public Sub ConfigLer(Optional eArqConfig As String = "")

    Dim retorno As Long

    retorno = NCM_ConfigLer(libHandler, eArqConfig)
    CheckResult retorno
End Sub

Public Sub ConfigGravar(Optional eArqConfig As String = "")

    Dim retorno As Long

    retorno = NCM_ConfigGravar(libHandler, eArqConfig)
    CheckResult retorno
End Sub

Public Function ConfigLerValor(ByVal eSessao As String, ByVal eChave As String) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NCM_ConfigLerValor(libHandler, eSessao, eChave, buffer, bufferLen)
    CheckResult retorno
    
    ConfigLerValor = processResult(buffer, bufferLen)
End Function

Public Sub ConfigGravarValor(ByVal eSessao As String, _
                             ByVal eChave As String, _
                             ByVal valor As String)

    Dim retorno As Long
    
    retorno = NCM_ConfigGravarValor(libHandler, eSessao, eChave, valor)
    CheckResult retorno
End Sub

Public Function Nome() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
        
    retorno = NCM_Nome(libHandler, buffer, bufferLen)
    CheckResult retorno
    
    Nome = processResult(buffer, bufferLen)
End Function

Public Function Versao() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = NCM_Versao(libHandler, buffer, bufferLen)
    CheckResult retorno
    
    Versao = processResult(buffer, bufferLen)
End Function

Public Function DescricaoNCM(ByVal cNCM As String) As String

    Dim retorno As Long
    Dim buffer As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NCM_DescricaoNCM(libHandler, cNCM, buffer, bufferLen)
    CheckResult retorno
    
    DescricaoNCM = processResult(buffer, bufferLen)
End Function

Public Function Validar(ByVal cNCM As String) As String
    
    Dim retorno As Long
    Dim buffer As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NCM_Validar(libHandler, cNCM, buffer, bufferLen)
    CheckResult retorno
    
    Validar = processResult(buffer, bufferLen)
End Function

Public Function BaixarLista(ByVal cNomeArquivo As String) As String

    Dim retorno As Long
    Dim buffer As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NCM_BaixarLista(libHandler, cNomeArquivo, buffer, bufferLen)
    CheckResult retorno
    
    BaixarLista = processResult(buffer, bufferLen)
End Function

Public Function ObterNCMs() As String

    Dim retorno As Long
    Dim buffer As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NCM_ObterNCMs(libHandler, buffer, bufferLen)
    CheckResult retorno
    
    ObterNCMs = processResult(buffer, bufferLen)
End Function

Public Function BuscarPorCodigo(ByVal cNCM As String) As String

    Dim retorno As Long
    Dim buffer As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NCM_BuscarPorCodigo(libHandler, cNCM, buffer, bufferLen)
    CheckResult retorno
    
    BuscarPorCodigo = processResult(buffer, bufferLen)
End Function

Public Function BuscarPorDescricao(ByVal cDesc As String, ByVal nTipo As Long) As String

    Dim retorno As Long
    Dim buffer As String
    Dim bufferLen As Long
    
    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = NCM_BuscarPorDescricao(libHandler, cDesc, nTipo, buffer, bufferLen)
    CheckResult retorno
    
    BuscarPorDescricao = processResult(buffer, bufferLen)
End Function

Private Sub CheckResult(ByVal Resultado As Long)
    
    If Resultado >= 0 Then Exit Sub
         
    Dim buffer As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    NCM_UltimoRetorno libHandler, buffer, bufferLen
    
    Err.Raise Resultado, "ACBrNCM", processResult(buffer, bufferLen)
End Sub

Private Function processResult(ByRef buffer As String, ByRef bufferLen As Long) As String
    
    If bufferLen > BUFFER_LENGTH Then
        buffer = String$(bufferLen, " ")
        NCM_UltimoRetorno libHandler, buffer, bufferLen
    End If

    processResult = Trim$(FromUTF8(buffer))
End Function
