VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ACBrETQ"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Private libHandler As Long

Const BUFFER_LENGTH As Long = 1024
                               
Private Declare Function ETQ_Inicializar _
                Lib "ACBrETQ32.dll" (ByRef libHandler As Long, _
                                     ByVal eArqConfig As String, _
                                     ByVal eChaveCrypt As String) As Long
                   
Private Declare Function ETQ_Finalizar Lib "ACBrETQ32.dll" (ByVal libHandler As Long) As Long

Private Declare Function ETQ_Nome _
                Lib "ACBrETQ32.dll" (ByVal libHandler As Long, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                   
Private Declare Function ETQ_Versao _
                Lib "ACBrETQ32.dll" (ByVal libHandler As Long, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long

Private Declare Function ETQ_UltimoRetorno _
                Lib "ACBrETQ32.dll" (ByVal libHandler As Long, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                   
Private Declare Function ETQ_ConfigImportar _
                Lib "ACBrETQ32.dll" (ByVal libHandler As Long, _
                                     ByVal eArqConfig As String) As Long
                   
Private Declare Function ETQ_ConfigExportar _
                Lib "ACBrETQ32.dll" (ByVal libHandler As Long, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long
                   
Private Declare Function ETQ_ConfigLer _
                Lib "ACBrETQ32.dll" (ByVal libHandler As Long, _
                                     ByVal eArqConfig As String) As Long

Private Declare Function ETQ_ConfigGravar _
                Lib "ACBrETQ32.dll" (ByVal libHandler As Long, _
                                     ByVal eArqConfig As String) As Long
                   
Private Declare Function ETQ_ConfigLerValor _
                Lib "ACBrETQ32.dll" (ByVal libHandler As Long, _
                                     ByVal eSessao As String, _
                                     ByVal eChave As String, _
                                     ByVal buffer As String, _
                                     ByRef bufferLen As Long) As Long

Private Declare Function ETQ_ConfigGravarValor _
                Lib "ACBrETQ32.dll" (ByVal libHandler As Long, _
                                     ByVal eSessao As String, _
                                     ByVal eChave As String, _
                                     ByVal valor As String) As Long
                    
Private Declare Function ETQ_Ativar Lib "ACBrETQ32.dll" (ByVal libHandler As Long) As Long

Private Declare Function ETQ_Desativar Lib "ACBrETQ32.dll" (ByVal libHandler As Long) As Long

Private Declare Function ETQ_IniciarEtiqueta Lib "ACBrETQ32.dll" (ByVal libHandler As Long) As Long

Private Declare Function ETQ_FinalizarEtiqueta _
                Lib "ACBrETQ32.dll" (ByVal libHandler As Long, _
                                     ByVal aCopias As Long, _
                                     ByVal aAvancoEtq As Long) As Long
                                     
Private Declare Function ETQ_CarregarImagem _
                Lib "ACBrETQ32.dll" (ByVal libHandler As Long, _
                                     ByVal eArquivoImagem As String, _
                                     ByVal eNomeImagem As String, _
                                     ByVal flipped As Boolean) As Long
                                     
Private Declare Function ETQ_Imprimir _
                Lib "ACBrETQ32.dll" (ByVal libHandler As Long, _
                                     ByVal aCopias As Long, _
                                     ByVal aAvancoEtq As Long) As Long
                                     
Private Declare Function ETQ_ImprimirTexto _
                Lib "ACBrETQ32.dll" (ByVal libHandler As Long, _
                                     ByVal orientacao As Long, _
                                     ByVal fonte As Long, _
                                     ByVal MultiplicadorH As Long, _
                                     ByVal MultiplicadorV As Long, _
                                     ByVal Vertical As Long, _
                                     ByVal Horizontal As Long, _
                                     ByVal eTexto As String, _
                                     ByVal SubFonte As Long, _
                                     ByVal ImprimirReverso As Boolean) As Long
                                     
Private Declare Function ETQ_ImprimirTextoStr _
                Lib "ACBrETQ32.dll" (ByVal libHandler As Long, _
                                     ByVal orientacao As Long, _
                                     ByVal fonte As String, _
                                     ByVal MultiplicadorH As Long, _
                                     ByVal MultiplicadorV As Long, _
                                     ByVal Vertical As Long, _
                                     ByVal Horizontal As Long, _
                                     ByVal eTexto As String, _
                                     ByVal SubFonte As Long, _
                                     ByVal ImprimirReverso As Boolean) As Long
                                     
Private Declare Function ETQ_ImprimirBarras _
                Lib "ACBrETQ32.dll" (ByVal libHandler As Long, _
                                     ByVal orientacao As Long, _
                                     ByVal TipoBarras As Long, _
                                     ByVal LarguraBarraLarga As Long, _
                                     ByVal LarguraBarraFina As Long, _
                                     ByVal Vertical As Long, _
                                     ByVal Horizontal As Long, _
                                     ByVal eTexto As String, _
                                     ByVal AlturaCodBarras As Long, _
                                     ByVal ExibeCodigo As Long) As Long
                                     
Private Declare Function ETQ_ImprimirLinha _
                Lib "ACBrETQ32.dll" (ByVal libHandler As Long, _
                                     ByVal Vertical As Long, _
                                     ByVal Horizontal As Long, _
                                     ByVal Largura As Long, _
                                     ByVal Altura As Long) As Long
                                     
Private Declare Function ETQ_ImprimirCaixa _
                Lib "ACBrETQ32.dll" (ByVal libHandler As Long, _
                                     ByVal Vertical As Long, _
                                     ByVal Horizontal As Long, _
                                     ByVal Largura As Long, _
                                     ByVal Altura As Long, _
                                     ByVal EspessuraVertical As Long, _
                                     ByVal EspessuraHorizontal As Long) As Long
                                     
Private Declare Function ETQ_ImprimirImagem _
                Lib "ACBrETQ32.dll" (ByVal libHandler As Long, _
                                     ByVal MultiplicadorImagem As Long, _
                                     ByVal Vertical As Long, _
                                     ByVal Horizontal As Long, _
                                     ByVal eNomeImagem As String) As Long
                                     
Private Declare Function ETQ_ImprimirQRCode _
                Lib "ACBrETQ32.dll" (ByVal libHandler As Long, _
                                     ByVal Vertical As Long, _
                                     ByVal Horizontal As Long, _
                                     ByVal Texto As String, _
                                     ByVal LarguraModulo As Long, _
                                     ByVal ErrorLevel As Long, _
                                     ByVal Tipo As Long) As Long
                                     

Public Sub InicializarLib(Optional ByVal eArqConfig As String = "", _
                          Optional ByVal eChaveCrypt As String = "")

    SetLibPath

    Dim retorno As Long

    retorno = ETQ_Inicializar(libHandler, eArqConfig, eChaveCrypt)
    CheckResult (retorno)
End Sub

Public Sub FinalizarLib()

    Dim retorno As Long

    retorno = ETQ_Finalizar(libHandler)
    CheckResult (retorno)
End Sub

Public Sub ConfigImportar(ByVal eArqConfig As String)

    Dim retorno As Long

    retorno = ETQ_ConfigImportar(libHandler, eArqConfig)
    CheckResult (retorno)
    
End Sub
    
Public Function ConfigExportar() As String

    Dim retorno   As Long
    Dim buffer    As String
    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = ETQ_ConfigExportar(libHandler, buffer, bufferLen)
    CheckResult retorno
    
    ConfigExportar = processResult(buffer, bufferLen)
    
End Function

Public Sub ConfigLer(Optional eArqConfig As String = "")

    Dim retorno As Long

    retorno = ETQ_ConfigLer(libHandler, eArqConfig)
    CheckResult (retorno)
End Sub

Public Sub ConfigGravar(Optional eArqConfig As String = "")

    Dim retorno As Long

    retorno = ETQ_ConfigGravar(libHandler, eArqConfig)
    CheckResult (retorno)
End Sub

Public Function ConfigLerValor(ByVal eSessao As String, ByVal eChave As String) As String

    Dim retorno   As Long

    Dim buffer    As String

    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen + 1, " ")
    
    retorno = ETQ_ConfigLerValor(libHandler, eSessao, eChave, buffer, bufferLen)
    CheckResult (retorno)
    
    ConfigLerValor = processResult(buffer, bufferLen)
End Function

Public Sub ConfigGravarValor(ByVal eSessao As String, _
                             ByVal eChave As String, _
                             ByVal valor As String)

    Dim retorno As Long
    
    retorno = ETQ_ConfigGravarValor(libHandler, eSessao, eChave, valor)
    CheckResult (retorno)
End Sub

Public Function Nome() As String

    Dim retorno   As Long

    Dim buffer    As String

    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
        
    retorno = ETQ_Nome(libHandler, buffer, bufferLen)
    CheckResult (retorno)
    
    Nome = processResult(buffer, bufferLen)
End Function

Public Function Versao() As String

    Dim retorno   As Long

    Dim buffer    As String

    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    
    buffer = String$(bufferLen, " ")
    
    retorno = ETQ_Versao(libHandler, buffer, bufferLen)
    CheckResult (retorno)
    
    Versao = processResult(buffer, bufferLen)
End Function

Public Sub Ativar()
     Dim retorno As Long
    
    retorno = ETQ_Ativar(libHandler)
    CheckResult (retorno)
End Sub

Public Sub Desativar()
     Dim retorno As Long
    
    retorno = ETQ_Desativar(libHandler)
    CheckResult (retorno)
End Sub

Public Sub IniciarEtiqueta()
     Dim retorno As Long
    
    retorno = ETQ_IniciarEtiqueta(libHandler)
    CheckResult (retorno)
End Sub

Public Sub FinalizarEtiqueta(Optional ByVal aCopias As Long = 1, _
                             Optional ByVal aAvancoEtq As Long = 0)
     Dim retorno As Long
    
    retorno = ETQ_FinalizarEtiqueta(libHandler, aCopias, aAvancoEtq)
    CheckResult (retorno)
End Sub

Public Sub CarregarImagem(ByVal eArquivoImagem As String, _
                          ByVal eNomeImagem As String, _
                          Optional ByVal flipped As Boolean = True)
     Dim retorno As Long
    
    retorno = ETQ_CarregarImagem(libHandler, eArquivoImagem, eNomeImagem, flipped)
    CheckResult (retorno)
End Sub

Public Sub Imprimir(Optional ByVal aCopias As Long = 1, _
                    Optional ByVal aAvancoEtq As Long = 0)
     Dim retorno As Long
    
    retorno = ETQ_Imprimir(libHandler, aCopias, aAvancoEtq)
    CheckResult (retorno)
End Sub

Public Sub ImprimirTexto(ByVal orientacao As ETQOrientacao, _
                         ByVal fonte As Long, _
                         ByVal MultiplicadorH As Long, _
                         ByVal MultiplicadorV As Long, _
                         ByVal Vertical As Long, _
                         ByVal Horizontal As Long, _
                         ByVal eTexto As String, _
                         Optional ByVal SubFonte As Long = 0, _
                         Optional ByVal ImprimirReverso As Boolean = False)
     Dim retorno As Long
    
    retorno = ETQ_ImprimirTexto(libHandler, orientacao, fonte, MultiplicadorH, _
                                MultiplicadorV, Vertical, Horizontal, _
                                eTexto, SubFonte, ImprimirReverso)
    CheckResult (retorno)
End Sub

Public Sub ImprimirTextoStr(ByVal orientacao As ETQOrientacao, _
                            ByVal fonte As String, _
                            ByVal MultiplicadorH As Long, _
                            ByVal MultiplicadorV As Long, _
                            ByVal Vertical As Long, _
                            ByVal Horizontal As Long, _
                            ByVal eTexto As String, _
                            Optional ByVal SubFonte As Long = 0, _
                            Optional ByVal ImprimirReverso As Boolean = False)
     Dim retorno As Long
    
    retorno = ETQ_ImprimirTextoStr(libHandler, orientacao, fonte, MultiplicadorH, _
                                MultiplicadorV, Vertical, Horizontal, _
                                eTexto, SubFonte, ImprimirReverso)
    CheckResult (retorno)
End Sub

Public Sub ImprimirBarras(ByVal orientacao As ETQOrientacao, _
                          ByVal TipoBarras As TipoCodBarra, _
                          ByVal LarguraBarraLarga As Long, _
                          ByVal LarguraBarraFina As Long, _
                          ByVal Vertical As Long, _
                          ByVal Horizontal As Long, _
                          ByVal eTexto As String, _
                          Optional ByVal AlturaCodBarras As Long = 0, _
                          Optional ByVal ExibeCodigo As ETQBarraExibeCodigo = ETQBarraExibeCodigo.becPadrao)
     Dim retorno As Long
    
    retorno = ETQ_ImprimirBarras(libHandler, orientacao, TipoBarras, LarguraBarraLarga, _
                                LarguraBarraFina, Vertical, Horizontal, _
                                eTexto, AlturaCodBarras, ExibeCodigo)
    CheckResult (retorno)
End Sub

Public Sub ImprimirLinha(ByVal Vertical As Long, _
                         ByVal Horizontal As Long, _
                         ByVal Largura As Long, _
                         ByVal Altura As Long)
     Dim retorno As Long
    
    retorno = ETQ_ImprimirLinha(libHandler, Vertical, Horizontal, Largura, Altura)
    CheckResult (retorno)
End Sub

Public Sub ImprimirCaixa(ByVal Vertical As Long, _
                         ByVal Horizontal As Long, _
                         ByVal Largura As Long, _
                         ByVal Altura As Long, _
                         ByVal EspessuraVertical As Long, _
                         ByVal EspessuraHorizontal As Long)
     Dim retorno As Long
    
    retorno = ETQ_ImprimirCaixa(libHandler, Vertical, Horizontal, Largura, Altura, EspessuraVertical, EspessuraHorizontal)
    CheckResult (retorno)
End Sub

Public Sub ImprimirImagem(ByVal MultiplicadorImagem As Long, _
                          ByVal Vertical As Long, _
                          ByVal Horizontal As Long, _
                          ByVal eNomeImagem As String)
     Dim retorno As Long
    
    retorno = ETQ_ImprimirImagem(libHandler, MultiplicadorImagem, Vertical, Horizontal, eNomeImagem)
    CheckResult (retorno)
End Sub

Public Sub ImprimirQRCode(ByVal Vertical As Long, ByVal Horizontal As Long, _
                          ByVal Texto As String, ByVal LarguraModulo As Long, _
                          ByVal ErrorLevel As Long, ByVal Tipo As Long)
     Dim retorno As Long
    
    retorno = ETQ_ImprimirQRCode(libHandler, Vertical, Horizontal, Texto, LarguraModulo, ErrorLevel, Tipo)
    CheckResult (retorno)
End Sub

Private Sub CheckResult(ByVal Resultado As Long)
    
    If Resultado = 0 Then Exit Sub
         
    Dim buffer As String

    Dim bufferLen As Long

    bufferLen = BUFFER_LENGTH
    buffer = String$(bufferLen, " ")
    ETQ_UltimoRetorno libHandler, buffer, bufferLen
    
    Err.Raise Resultado, "ACBrETQ", processResult(buffer, bufferLen)
End Sub

Private Function processResult(ByRef buffer As String, ByRef bufferLen As Long) As String
    
    If bufferLen > BUFFER_LENGTH Then
        buffer = String$(bufferLen, " ")
        ETQ_UltimoRetorno libHandler, buffer, bufferLen
    End If

    processResult = Trim$(FromUTF8(buffer))
End Function
