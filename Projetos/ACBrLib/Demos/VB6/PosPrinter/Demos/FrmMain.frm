VERSION 5.00
Object = "{C932BA88-4374-101B-A56C-00AA003668DC}#1.1#0"; "msmask32.ocx"
Object = "{6B7E6392-850A-101B-AFC0-4210102A8DA7}#1.3#0"; "comctl32.ocx"
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "comdlg32.ocx"
Object = "{86CF1D34-0C5F-11D2-A9FC-0000F8754DA1}#2.0#0"; "mscomct2.ocx"
Begin VB.Form FrmMain 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "ACBrLibPosPrinter Demo VB6"
   ClientHeight    =   3705
   ClientLeft      =   45
   ClientTop       =   375
   ClientWidth     =   10950
   BeginProperty Font 
      Name            =   "Tahoma"
      Size            =   8.25
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   Icon            =   "FrmMain.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   3705
   ScaleWidth      =   10950
   StartUpPosition =   2  'CenterScreen
   Begin VB.CommandButton cmdInformações 
      Caption         =   "Informações"
      Height          =   360
      Left            =   9480
      TabIndex        =   33
      Top             =   1080
      Width           =   1215
   End
   Begin VB.CommandButton cmdCheckStatus 
      Caption         =   "Check Status"
      Height          =   360
      Left            =   9480
      TabIndex        =   32
      Top             =   1560
      Width           =   1215
   End
   Begin MSComDlg.CommonDialog CommonDialog1 
      Left            =   5400
      Top             =   840
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
   End
   Begin VB.CommandButton cmdAddTags 
      Caption         =   "Add Tags"
      Height          =   360
      Left            =   9480
      TabIndex        =   10
      Top             =   600
      Width           =   1215
   End
   Begin VB.CommandButton cmdImprimir 
      Caption         =   "Imprimir"
      Height          =   360
      Left            =   9480
      TabIndex        =   9
      Top             =   3000
      Width           =   1215
   End
   Begin VB.CommandButton cmdClear 
      Caption         =   "Clear"
      Height          =   360
      Index           =   0
      Left            =   9480
      TabIndex        =   8
      Top             =   2520
      Width           =   1215
   End
   Begin VB.Frame FraConfiguração 
      Caption         =   "Configuração"
      Height          =   3375
      Left            =   120
      TabIndex        =   2
      Top             =   120
      Width           =   3615
      Begin VB.ComboBox ComCodePage 
         Height          =   315
         Left            =   1800
         Style           =   2  'Dropdown List
         TabIndex        =   31
         Top             =   2880
         Width           =   1695
      End
      Begin VB.CommandButton cmdArqLog 
         Caption         =   "..."
         Height          =   315
         Left            =   3120
         TabIndex        =   29
         Top             =   2265
         Width           =   390
      End
      Begin VB.TextBox txtArqLog 
         Height          =   315
         Left            =   1800
         TabIndex        =   28
         Text            =   "ArqLog"
         Top             =   2280
         Width           =   1335
      End
      Begin VB.CheckBox chkIgnorarTags 
         Caption         =   "Ignorar Tags"
         Height          =   195
         Left            =   120
         TabIndex        =   26
         Top             =   3000
         Width           =   1335
      End
      Begin VB.CheckBox chkTraduzirTags 
         Caption         =   "Traduzir Tags"
         Height          =   375
         Left            =   120
         TabIndex        =   25
         Top             =   2640
         Value           =   1  'Checked
         Width           =   1335
      End
      Begin VB.CheckBox chkCortarPapel 
         Caption         =   "Cortar Papel"
         Height          =   255
         Left            =   120
         TabIndex        =   24
         Top             =   2400
         Value           =   1  'Checked
         Width           =   1455
      End
      Begin VB.CheckBox chkControlePorta 
         Caption         =   "Controle Porta"
         Height          =   195
         Left            =   120
         TabIndex        =   23
         Top             =   2160
         Value           =   1  'Checked
         Width           =   1455
      End
      Begin MSComCtl2.UpDown updLinhasPular 
         Height          =   285
         Left            =   3116
         TabIndex        =   22
         Top             =   1680
         Width           =   255
         _ExtentX        =   450
         _ExtentY        =   503
         _Version        =   393216
         BuddyControl    =   "MasLinhasPular"
         BuddyDispid     =   196636
         OrigLeft        =   3120
         OrigTop         =   1680
         OrigRight       =   3375
         OrigBottom      =   1965
         Max             =   999
         SyncBuddy       =   -1  'True
         BuddyProperty   =   22
         Enabled         =   -1  'True
      End
      Begin MSMask.MaskEdBox MasLinhasPular 
         Height          =   285
         Left            =   2640
         TabIndex        =   21
         Top             =   1680
         Width           =   480
         _ExtentX        =   847
         _ExtentY        =   503
         _Version        =   393216
         PromptInclude   =   0   'False
         MaxLength       =   3
         Format          =   "0"
         Mask            =   "###"
         PromptChar      =   "_"
      End
      Begin MSComCtl2.UpDown updBuffer 
         Height          =   285
         Left            =   2275
         TabIndex        =   20
         Top             =   1680
         Width           =   255
         _ExtentX        =   450
         _ExtentY        =   503
         _Version        =   393216
         BuddyControl    =   "MasBuffer"
         BuddyDispid     =   196638
         OrigLeft        =   2280
         OrigTop         =   1680
         OrigRight       =   2535
         OrigBottom      =   1935
         Max             =   999
         SyncBuddy       =   -1  'True
         BuddyProperty   =   22
         Enabled         =   -1  'True
      End
      Begin MSMask.MaskEdBox MasBuffer 
         Height          =   285
         Left            =   1800
         TabIndex        =   19
         Top             =   1680
         Width           =   480
         _ExtentX        =   847
         _ExtentY        =   503
         _Version        =   393216
         PromptInclude   =   0   'False
         MaxLength       =   3
         Format          =   "0"
         Mask            =   "###"
         PromptChar      =   "_"
      End
      Begin MSComCtl2.UpDown updEspacos 
         Height          =   285
         Left            =   1440
         TabIndex        =   18
         Top             =   1680
         Width           =   255
         _ExtentX        =   450
         _ExtentY        =   503
         _Version        =   393216
         BuddyControl    =   "MaskEspacos"
         BuddyDispid     =   196640
         OrigLeft        =   1320
         OrigTop         =   1680
         OrigRight       =   1575
         OrigBottom      =   1935
         Max             =   999
         SyncBuddy       =   -1  'True
         BuddyProperty   =   22
         Enabled         =   -1  'True
      End
      Begin MSMask.MaskEdBox MaskEspacos 
         Height          =   285
         Left            =   960
         TabIndex        =   17
         Top             =   1680
         Width           =   480
         _ExtentX        =   847
         _ExtentY        =   503
         _Version        =   393216
         PromptInclude   =   0   'False
         MaxLength       =   3
         Format          =   "0"
         Mask            =   "###"
         PromptChar      =   "_"
      End
      Begin MSMask.MaskEdBox MasColunas 
         Height          =   285
         Left            =   120
         TabIndex        =   16
         Top             =   1680
         Width           =   480
         _ExtentX        =   847
         _ExtentY        =   503
         _Version        =   393216
         PromptInclude   =   0   'False
         MaxLength       =   3
         Format          =   "0"
         Mask            =   "###"
         PromptChar      =   "_"
      End
      Begin MSComCtl2.UpDown updColunas 
         Height          =   285
         Left            =   601
         TabIndex        =   15
         Top             =   1680
         Width           =   255
         _ExtentX        =   450
         _ExtentY        =   503
         _Version        =   393216
         BuddyControl    =   "MasColunas"
         BuddyDispid     =   196641
         OrigLeft        =   510
         OrigTop         =   1680
         OrigRight       =   765
         OrigBottom      =   1965
         Max             =   999
         SyncBuddy       =   -1  'True
         BuddyProperty   =   22
         Enabled         =   -1  'True
      End
      Begin VB.ComboBox ComPorta 
         Height          =   315
         Left            =   120
         TabIndex        =   7
         Text            =   "ComPorta"
         Top             =   1080
         Width           =   3375
      End
      Begin VB.ComboBox ComModelo 
         Height          =   315
         ItemData        =   "FrmMain.frx":25CA
         Left            =   120
         List            =   "FrmMain.frx":25CC
         Style           =   2  'Dropdown List
         TabIndex        =   4
         Top             =   480
         Width           =   2295
      End
      Begin VB.CommandButton cmdAtivar 
         Caption         =   "Ativar"
         Height          =   705
         Left            =   2520
         Picture         =   "FrmMain.frx":25CE
         TabIndex        =   3
         Top             =   240
         Width           =   975
      End
      Begin VB.Label lblCodePage 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Code Page"
         Height          =   195
         Left            =   1800
         TabIndex        =   30
         Top             =   2640
         Width           =   780
      End
      Begin VB.Label lblArqLog 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Arq. Log"
         Height          =   195
         Left            =   1800
         TabIndex        =   27
         Top             =   2040
         Width           =   615
      End
      Begin VB.Label lblLinhasPular 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Linhas Pular"
         Height          =   195
         Left            =   2640
         TabIndex        =   14
         Top             =   1440
         Width           =   855
      End
      Begin VB.Label lblBuffer 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Buffer"
         Height          =   195
         Left            =   1800
         TabIndex        =   13
         Top             =   1440
         Width           =   450
      End
      Begin VB.Label lblEspaços 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Espaços"
         Height          =   195
         Left            =   960
         TabIndex        =   12
         Top             =   1440
         Width           =   585
      End
      Begin VB.Label lblColunas 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Colunas"
         Height          =   195
         Left            =   120
         TabIndex        =   11
         Top             =   1440
         Width           =   570
      End
      Begin VB.Label lblPorta 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Porta"
         Height          =   195
         Left            =   120
         TabIndex        =   6
         Top             =   840
         Width           =   390
      End
      Begin VB.Label lblModelo 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Modelo"
         Height          =   195
         Left            =   120
         TabIndex        =   5
         Top             =   240
         Width           =   510
      End
   End
   Begin VB.TextBox txtImpressao 
      Height          =   2775
      Left            =   3960
      MultiLine       =   -1  'True
      ScrollBars      =   2  'Vertical
      TabIndex        =   1
      Top             =   600
      Width           =   5415
   End
   Begin ComctlLib.TabStrip TabStrip1 
      Height          =   3375
      Left            =   3840
      TabIndex        =   0
      Top             =   120
      Width           =   6975
      _ExtentX        =   12303
      _ExtentY        =   5953
      _Version        =   327682
      BeginProperty Tabs {0713E432-850A-101B-AFC0-4210102A8DA7} 
         NumTabs         =   1
         BeginProperty Tab1 {0713F341-850A-101B-AFC0-4210102A8DA7} 
            Caption         =   "Texto a Imprimir"
            Key             =   ""
            Object.Tag             =   ""
            ImageVarType    =   2
         EndProperty
      EndProperty
   End
End
Attribute VB_Name = "FrmMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Dim posPrinter As ACBrPosPrinter

Private Sub cmdAddTags_Click()

    With txtImpressao
        .Text = "</zera>" & vbCrLf
        .Text = .Text & "</linha_dupla>" & vbCrLf
        .Text = .Text & "FONTE NORMAL: " & CStr(updColunas.Value) & " Colunas" & vbCrLf
        .Text = .Text & "</c><n>FONTE NEGRITO</N>" & vbCrLf
        .Text = .Text & "<in>FONTE INVERTIDA</in>" & vbCrLf
        .Text = .Text & "<S>FONTE SUBLINHADA</s>" & vbCrLf
        .Text = .Text & "<i>FONTE ITALICO</i>" & vbCrLf
        .Text = .Text & "FONTE NORMAL" & vbCrLf
        .Text = .Text & "</linha_simples>" & vbCrLf
        .Text = .Text & "<n>LIGA NEGRITO" & vbCrLf
        .Text = .Text & "<i>LIGA ITALICO" & vbCrLf
        .Text = .Text & "<S>LIGA SUBLINHADA" & vbCrLf
        .Text = .Text & "<c>LIGA CONDENSADA" & vbCrLf
        .Text = .Text & "<e>LIGA EXPANDIDA" & vbCrLf
        .Text = .Text & "<a>LIGA ALTURA DUPLA" & vbCrLf
        .Text = .Text & "</fn>FONTE NORMAL" & vbCrLf
        .Text = .Text & "</linha_simples>" & vbCrLf
        .Text = .Text & "<e><n>NEGRITO E EXPANDIDA</n></e>" & vbCrLf
        .Text = .Text & "<c><n>NEGRITO E CONDENSADA</n></c>" & vbCrLf
        .Text = .Text & "<e><a>EXPANDIDA E ALT.DUPLA</a></e>" & vbCrLf
        .Text = .Text & "</fn>FONTE NORMAL" & vbCrLf
        .Text = .Text & "<in><e>INVERTIDA E EXPANDIDA</e></in>" & vbCrLf
        .Text = .Text & "<in><c>INVERTIDA E CONDENSADA</c></in>" & vbCrLf
        .Text = .Text & "<in><a>INVERTIDA E ALT.DUPLA</a></in>" & vbCrLf
        .Text = .Text & "</fn>FONTE NORMAL" & vbCrLf
        .Text = .Text & "</linha_simples>" & vbCrLf
        .Text = .Text & "</FB>FONTE TIPO B" & vbCrLf
        .Text = .Text & "<n>FONTE NEGRITO</N>" & vbCrLf
        .Text = .Text & "<e>FONTE EXPANDIDA</e>" & vbCrLf
        .Text = .Text & "<a>FONTE ALT.DUPLA</a>" & vbCrLf
        .Text = .Text & "<in>FONTE INVERTIDA</in>" & vbCrLf
        .Text = .Text & "<S>FONTE SUBLINHADA</s>" & vbCrLf
        .Text = .Text & "<i>FONTE ITALICO</i>" & vbCrLf
        .Text = .Text & "</FA>FONTE TIPO A" & vbCrLf
        .Text = .Text & "</FN>FONTE NORMAL" & vbCrLf
        .Text = .Text & "</corte_total>" & vbCrLf
        .Text = .Text & "<c>CODE128C: 35150711111111111111591234567890001135408700</c>" & vbCrLf
        .Text = .Text & "<code128c>35150711111111111111591234567890001135408700</code128c>" & vbCrLf
        .Text = .Text & "</Linha_Simples>" & vbCrLf
        .Text = .Text & "UPCA: 12345678901" & vbCrLf
        .Text = .Text & "<upca>12345678901</upca>" & vbCrLf
        .Text = .Text & "</Linha_Simples>" & vbCrLf
        .Text = .Text & "CODABAR: A123456789012345A" & vbCrLf
        .Text = .Text & "<codabar>A123456789012345A</codabar>" & vbCrLf
        .Text = .Text & "</Linha_Simples>" & vbCrLf
        .Text = .Text & "MSI: 1234567890" & vbCrLf
        .Text = .Text & "<msi>1234567890</msi>" & vbCrLf
        .Text = .Text & "</corte_total>"
    End With

End Sub

Private Sub cmdArqLog_Click()
    On Error Resume Next
    CommonDialog1.DialogTitle = "Arquivo de Log"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivo Log (*.log)|*.log|Todos os arquivos (*.*)|*.*"
    CommonDialog1.ShowSave
        
    If Err Then Exit Sub
    
    Me.txtArqLog.Text = CommonDialog1.FileName
End Sub

Private Sub cmdCheckStatus_Click()
    Dim status As ACBrPosTipoStatus
    status = posPrinter.LerStatusImpressora
    
    txtImpressao.Text = vbNullString
        
    If HasPosTipoStatus(status, ACBrPosTipoStatus.None) Then
        txtImpressao.Text = txtImpressao.Text & "[None]" & vbCrLf
    End If
        
    If HasPosTipoStatus(status, ACBrPosTipoStatus.Erro) Then
        txtImpressao.Text = txtImpressao.Text & "[Erro]" & vbCrLf
    End If
        
    If HasPosTipoStatus(status, ACBrPosTipoStatus.ErroLeitura) Then
        txtImpressao.Text = txtImpressao.Text & "[ErroLeitura]" & vbCrLf
    End If
        
    If HasPosTipoStatus(status, ACBrPosTipoStatus.GavetaAberta) Then
        txtImpressao.Text = txtImpressao.Text & "[GavetaAberta]" & vbCrLf
    End If
        
    If HasPosTipoStatus(status, ACBrPosTipoStatus.Imprimindo) Then
        txtImpressao.Text = txtImpressao.Text & "[Imprimindo]" & vbCrLf
    End If
        
    If HasPosTipoStatus(status, ACBrPosTipoStatus.NaoSerial) Then
        txtImpressao.Text = txtImpressao.Text & "[NaoSerial]" & vbCrLf
    End If
        
    If HasPosTipoStatus(status, ACBrPosTipoStatus.OffLine) Then
        txtImpressao.Text = txtImpressao.Text & "[OffLine]" & vbCrLf
    End If
        
    If HasPosTipoStatus(status, ACBrPosTipoStatus.SemPapel) Then
        txtImpressao.Text = txtImpressao.Text & "[SemPapel]" & vbCrLf
    End If
        
    If HasPosTipoStatus(status, ACBrPosTipoStatus.PoucoPapel) Then
        txtImpressao.Text = txtImpressao.Text & "[PoucoPapel]" & vbCrLf
    End If
        
    If HasPosTipoStatus(status, ACBrPosTipoStatus.TampaAberta) Then
        txtImpressao.Text = txtImpressao.Text & "[TampaAberta]" & vbCrLf
    End If

End Sub

Private Sub cmdClear_Click(Index As Integer)
    txtImpressao.Text = vbNullString
End Sub

Private Sub cmdImprimir_Click()
    posPrinter.Imprimir txtImpressao.Text
End Sub

Private Sub cmdInformações_Click()
    txtImpressao.Text = posPrinter.LerInfoImpressora
End Sub

Private Sub Form_Load()

    ComModelo.AddItem "ppTexto", ACBrPosPrinterModelo.ppTexto
    ComModelo.AddItem "ppEscPosEpson", ACBrPosPrinterModelo.ppEscPosEpson
    ComModelo.AddItem "ppEscBematech", ACBrPosPrinterModelo.ppEscBematech
    ComModelo.AddItem "ppEscDaruma", ACBrPosPrinterModelo.ppEscDaruma
    ComModelo.AddItem "ppEscVox", ACBrPosPrinterModelo.ppEscVox
    ComModelo.AddItem "ppEscDiebold", ACBrPosPrinterModelo.ppEscDiebold
    ComModelo.AddItem "ppEscEpsonP2", ACBrPosPrinterModelo.ppEscEpsonP2
    ComModelo.AddItem "ppCustomPos", ACBrPosPrinterModelo.ppCustomPos
    ComModelo.AddItem "ppEscZJiang", ACBrPosPrinterModelo.ppEscZJiang
    ComModelo.AddItem "ppEscGPrinter", ACBrPosPrinterModelo.ppEscGPrinter
    
    ComModelo.ListIndex = ACBrPosPrinterModelo.Texto
    
    ComPorta.AddItem "COM1"
    ComPorta.AddItem "COM2"
    ComPorta.AddItem "LPT1"
    ComPorta.AddItem "LPT2"
    ComPorta.AddItem "\\localhost\Epson"
    ComPorta.AddItem "C:\temp\ecf.txt"
    
    ComPorta.ListIndex = ComPorta.ListCount - 1
    
    ComPorta.AddItem "TCP:192.168.0.31:9100"
    ComPorta.AddItem "RAW:Elgin I9"
    
    ComCodePage.AddItem "None", PosPaginaCodigo.pcNone
    ComCodePage.AddItem "pc437", PosPaginaCodigo.pc437
    ComCodePage.AddItem "pc850", PosPaginaCodigo.pc850
    ComCodePage.AddItem "pc852", PosPaginaCodigo.pc852
    ComCodePage.AddItem "pc860", PosPaginaCodigo.pc860
    ComCodePage.AddItem "pcUTF8", PosPaginaCodigo.pcUTF8
    ComCodePage.AddItem "pc1252", PosPaginaCodigo.pc1252
    
    ComCodePage.ListIndex = PosPaginaCodigo.pc850
    
    updColunas.Value = 0
    updEspacos.Value = 0
    updBuffer.Value = 0
    updLinhasPular.Value = 0
    
    Set posPrinter = CreatePosPrinter
    Dim LogPath As String
    
    LogPath = App.Path & "\Docs\"
    
    If Dir(LogPath) = "" Then
        MkDir LogPath
    End If
    
    LoadConfig
    
    posPrinter.ConfigGravarValor SESSAO_PRINCIPAL, "LogNivel", "4"
    posPrinter.ConfigGravarValor SESSAO_PRINCIPAL, "LogPath", LogPath
    posPrinter.ConfigGravar
End Sub

Private Sub Form_Terminate()
    posPrinter.FinalizarLib
    Set posPrinter = Nothing
End Sub

Private Sub cmdAtivar_Click()
    '<EhHeader>
    On Error GoTo catch_error

    '</EhHeader>
    If cmdAtivar.Caption = "Ativar" Then
        SaveConfig
        posPrinter.Ativar
        cmdAtivar.Caption = "Desativar"
    Else
        posPrinter.Desativar
        cmdAtivar.Caption = "Ativar"
    End If

    '<EhFooter>
    Exit Sub
catch_error:
    MsgBox Err.Description, vbExclamation + vbOKOnly, "Application Error"
    Exit Sub
    '</EhFooter>
End Sub

Private Sub LoadConfig()
    posPrinter.ConfigLer
                
    ComModelo.ListIndex = CLng(posPrinter.ConfigLerValor(SESSAO_POSPRINTER, "Modelo"))
    ComPorta.Text = posPrinter.ConfigLerValor(SESSAO_POSPRINTER, "Porta")
    
    updColunas.Value = CLng(posPrinter.ConfigLerValor(SESSAO_POSPRINTER, "ColunasFonteNormal"))
    updEspacos.Value = CLng(posPrinter.ConfigLerValor(SESSAO_POSPRINTER, "EspacoEntreLinhas"))
    updBuffer.Value = CLng(posPrinter.ConfigLerValor(SESSAO_POSPRINTER, "LinhasBuffer"))
    updLinhasPular.Value = CLng(posPrinter.ConfigLerValor(SESSAO_POSPRINTER, "LinhasEntreCupons"))
        
    chkControlePorta.Value = CInt(posPrinter.ConfigLerValor(SESSAO_POSPRINTER, "ControlePorta"))
    chkCortarPapel.Value = CInt(posPrinter.ConfigLerValor(SESSAO_POSPRINTER, "CortaPapel"))
    chkTraduzirTags.Value = CInt(posPrinter.ConfigLerValor(SESSAO_POSPRINTER, "TraduzirTags"))
    chkIgnorarTags.Value = CInt(posPrinter.ConfigLerValor(SESSAO_POSPRINTER, "IgnorarTags"))
    
    txtArqLog.Text = posPrinter.ConfigLerValor(SESSAO_POSPRINTER, "ArqLog")
    ComCodePage.ListIndex = CLng(posPrinter.ConfigLerValor(SESSAO_POSPRINTER, "PaginaDeCodigo"))
End Sub

Private Sub SaveConfig()
    posPrinter.ConfigGravarValor SESSAO_POSPRINTER, "Modelo", CStr(ComModelo.ListIndex)
    posPrinter.ConfigGravarValor SESSAO_POSPRINTER, "Porta", ComPorta.Text
    
    posPrinter.ConfigGravarValor SESSAO_POSPRINTER, "ColunasFonteNormal", CStr(updColunas.Value)
    posPrinter.ConfigGravarValor SESSAO_POSPRINTER, "EspacoEntreLinhas", CStr(updEspacos.Value)
    posPrinter.ConfigGravarValor SESSAO_POSPRINTER, "LinhasBuffer", CStr(updBuffer.Value)
    posPrinter.ConfigGravarValor SESSAO_POSPRINTER, "LinhasEntreCupons", CStr(updLinhasPular.Value)
    
    posPrinter.ConfigGravarValor SESSAO_POSPRINTER, "ControlePorta", CStr(chkControlePorta.Value)
    posPrinter.ConfigGravarValor SESSAO_POSPRINTER, "CortaPapel", CStr(chkCortarPapel.Value)
    posPrinter.ConfigGravarValor SESSAO_POSPRINTER, "TraduzirTags", CStr(chkTraduzirTags.Value)
    posPrinter.ConfigGravarValor SESSAO_POSPRINTER, "IgnorarTags", CStr(chkIgnorarTags.Value)
    
    posPrinter.ConfigGravarValor SESSAO_POSPRINTER, "ArqLog", txtArqLog.Text
    posPrinter.ConfigGravarValor SESSAO_POSPRINTER, "PaginaDeCodigo", CStr(ComCodePage.ListIndex)
                    
    posPrinter.ConfigGravar
End Sub

Private Sub MasBuffer_Change()
    updBuffer.Value = CInt(MasBuffer.Text)
End Sub

Private Sub MasColunas_Change()
    updColunas.Value = CInt(MasColunas.Text)
End Sub

Private Sub MaskEspacos_Change()
    updEspacos.Value = CInt(MaskEspacos.Text)
End Sub

Private Sub MasLinhasPular_Change()
    updLinhasPular.Value = CInt(MasLinhasPular.Text)
End Sub
