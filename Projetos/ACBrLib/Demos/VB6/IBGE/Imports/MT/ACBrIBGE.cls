VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ACBrIBGE"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Private libHandler As Long

Const BUFFER_LENGTH As Long = 1024

Private Declare Function IBGE_Inicializar _
                Lib "ACBrIBGE32.dll" (ByRef libHandler As Long, _
                                     ByVal eArqConfig As String, _
                                     ByVal eChaveCrypt As String) As Long

Private Declare Function IBGE_Finalizar Lib "ACBrCEP32.dll" (ByRef libHandler As Long) As Long


Private Declare Function IBGE_Nome _
                Lib "ACBrIBGE32.dll" (ByVal libHandler As Long, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                     
Private Declare Function IBGE_Versao _
                Lib "ACBrIBGE32.dll" (ByVal libHandler As Long, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                                      
Private Declare Function IBGE_UltimoRetorno _
                Lib "ACBrIBGE32.dll" (ByVal libHandler As Long, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long
                                     
Private Declare Function IBGE_ConfigImportar _
                Lib "ACBrIBGE32.dll" (ByVal libHandler As Long, _
                                      ByVal eArqConfig As String) As Long
                   
Private Declare Function IBGE_ConfigExportar _
                Lib "ACBrIBGE32.dll" (ByVal libHandler As Long, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                     
Private Declare Function IBGE_ConfigLer _
                Lib "ACBrIBGE32.dll" (ByVal libHandler As Long, _
                                      ByVal eArqConfig As String) As Long

Private Declare Function IBGE_ConfigGravar _
                Lib "ACBrIBGE32.dll" (ByVal libHandler As Long, _
                                      ByVal eArqConfig As String) As Long
                   
Private Declare Function IBGE_ConfigLerValor _
                Lib "ACBrIBGE32.dll" (ByVal libHandler As Long, _
                                      ByVal eSessao As String, _
                                      ByVal eChave As String, _
                                      ByVal buffer As String, _
                                      ByRef bufferLen As Long) As Long

Private Declare Function IBGE_ConfigGravarValor _
                Lib "ACBrIBGE32.dll" (ByVal libHandler As Long, _
                                      ByVal eSessao As String, _
                                      ByVal eChave As String, _
                                      ByVal valor As String) As Long

Private Declare Function IBGE_BuscarPorCodigo _
                Lib "ACBrIBGE32.dll" (ByVal libHandler As Long, _
                                     ByVal ACodMun As Long, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long

Private Declare Function IBGE_BuscarPorNome _
                Lib "ACBrIBGE32.dll" (ByVal libHandler As Long, _
                                     ByVal eCidade As String, _
                                     ByVal eUF As String, _
                                     ByVal exata As Boolean, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                                      
Public Sub InicializarLib(Optional ByVal eArqConfig As String = "", _
                          Optional ByVal eChaveCrypt As String = "")
                          
    SetLibPath
    
    Dim retorno As Long

    retorno = IBGE_Inicializar(libHandler, eArqConfig, eChaveCrypt)
    CheckResult (retorno)
    
End Sub

Public Sub FinalizarLib()

    Dim retorno As Long

    retorno = IBGE_Finalizar(libHandler)
    CheckResult (retorno)
    
End Sub

Public Sub ConfigImportar(ByVal eArqConfig As String)

    Dim retorno As Long

    retorno = IBGE_ConfigImportar(libHandler, eArqConfig)
    CheckResult (retorno)
    
End Sub

Public Function ConfigExportar() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = IBGE_ConfigExportar(libHandler, buffer, bufferLen)
    CheckResult retorno
    
    ConfigExportar = processResult(buffer, bufferLen)
    
End Function

Public Sub ConfigLer(Optional eArqConfig As String = "")

    Dim retorno As Long

    retorno = IBGE_ConfigLer(libHandler, eArqConfig)
    CheckResult (retorno)
    
End Sub

Public Sub ConfigGravar(Optional eArqConfig As String = "")

    Dim retorno As Long

    retorno = IBGE_ConfigGravar(libHandler, eArqConfig)
    CheckResult (retorno)
    
End Sub

Public Function ConfigLerValor(ByVal eSessao As String, ByVal eChave As String) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = IBGE_ConfigLerValor(libHandler, eSessao, eChave, buffer, bufferLen)
    CheckResult retorno
    
    ConfigLerValor = processResult(buffer, bufferLen)
End Function

Public Sub ConfigGravarValor(ByVal eSessao As String, _
                             ByVal eChave As String, _
                             ByVal valor As String)

    Dim retorno As Long
    
    retorno = IBGE_ConfigGravarValor(libHandler, eSessao, eChave, valor)
    CheckResult retorno
End Sub

Public Function Nome() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
        
    retorno = IBGE_Nome(libHandler, buffer, bufferLen)
    CheckResult (retorno)
    
    Nome = processResult(buffer, bufferLen)
    
End Function

Public Function Versao() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = IBGE_Versao(libHandler, buffer, bufferLen)
    CheckResult (retorno)
    
    Versao = processResult(buffer, bufferLen)
    
End Function

Public Function BuscarPorCodigo(ByVal codMun As String) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    
    retorno = IBGE_BuscarPorCodigo(libHandler, codMun, buffer, bufferLen)
    CheckResult (retorno)
    
    BuscarPorCodigo = processResult(buffer, bufferLen)

End Function

Public Function BuscarPorNome(ByVal cidade As String, ByVal UF As String, ByVal exata As Boolean) As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = IBGE_BuscarPorNome(libHandler, cidade, UF, False, buffer, bufferLen)
    CheckResult (retorno)
    
    BuscarPorNome = processResult(buffer, bufferLen)

End Function

Private Sub CheckResult(ByVal Resultado As Long)
    
    If Resultado >= 0 Then Exit Sub
         
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    IBGE_UltimoRetorno libHandler, buffer, bufferLen
    
    Err.Raise Resultado, "ACBrIBGE", processResult(buffer, bufferLen)
End Sub

Private Function processResult(ByRef buffer As String, ByRef bufferLen As Long) As String
    
    If bufferLen > BUFFER_LENGTH Then
        buffer = String$(bufferLen, " ")
        IBGE_UltimoRetorno libHandler, buffer, bufferLen
    End If

    processResult = Trim$(FromUTF8(buffer))
End Function


