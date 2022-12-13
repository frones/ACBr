VERSION 5.00
Object = "{BDC217C8-ED16-11CD-956C-0000C04E4C0A}#1.1#0"; "TABCTL32.OCX"
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "COMDLG32.OCX"
Begin VB.Form FrmMain 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "ACBrLibNCM Demo"
   ClientHeight    =   6570
   ClientLeft      =   45
   ClientTop       =   390
   ClientWidth     =   15420
   BeginProperty Font 
      Name            =   "Tahoma"
      Size            =   8.25
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   LinkTopic       =   "FrmMain"
   MaxButton       =   0   'False
   ScaleHeight     =   6570
   ScaleWidth      =   15420
   StartUpPosition =   2  'CenterScreen
   Begin MSComDlg.CommonDialog CommonDialog1 
      Left            =   3480
      Top             =   3840
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
   End
   Begin VB.Frame FraRespostas 
      Caption         =   "Respostas"
      Height          =   6375
      Left            =   3480
      TabIndex        =   0
      Top             =   120
      Width           =   11775
      Begin VB.TextBox rtbRespostas 
         Height          =   5895
         Left            =   360
         Locked          =   -1  'True
         MultiLine       =   -1  'True
         ScrollBars      =   3  'Both
         TabIndex        =   1
         Top             =   240
         Width           =   11175
      End
   End
   Begin TabDlg.SSTab SSTab1 
      Height          =   6255
      Left            =   120
      TabIndex        =   2
      Top             =   120
      Width           =   3255
      _ExtentX        =   5741
      _ExtentY        =   11033
      _Version        =   393216
      Style           =   1
      Tabs            =   1
      TabsPerRow      =   1
      TabHeight       =   520
      TabCaption(0)   =   "Comandos"
      TabPicture(0)   =   "FrmMain.frx":0000
      Tab(0).ControlEnabled=   -1  'True
      Tab(0).Control(0)=   "btnObterNCMs"
      Tab(0).Control(0).Enabled=   0   'False
      Tab(0).Control(1)=   "btnBaixarLista"
      Tab(0).Control(1).Enabled=   0   'False
      Tab(0).Control(2)=   "Frame1"
      Tab(0).Control(2).Enabled=   0   'False
      Tab(0).Control(3)=   "Frame2"
      Tab(0).Control(3).Enabled=   0   'False
      Tab(0).Control(4)=   "Frame3"
      Tab(0).Control(4).Enabled=   0   'False
      Tab(0).ControlCount=   5
      Begin VB.Frame Frame3 
         Caption         =   "Validar NCM"
         Height          =   1335
         Left            =   120
         TabIndex        =   7
         Top             =   4680
         Width           =   2895
         Begin VB.CommandButton btnValidarNCM 
            Caption         =   "Validar"
            Height          =   375
            Left            =   120
            TabIndex        =   14
            Top             =   720
            Width           =   2655
         End
         Begin VB.TextBox txtValidarNCM 
            Height          =   285
            Left            =   120
            TabIndex        =   13
            Top             =   360
            Width           =   2655
         End
      End
      Begin VB.Frame Frame2 
         Caption         =   "Filtrar Por Descrição"
         Height          =   1695
         Left            =   120
         TabIndex        =   6
         Top             =   2880
         Width           =   2895
         Begin VB.CommandButton btnFiltrarPorDescricao 
            Caption         =   "Filtrar"
            Height          =   375
            Left            =   120
            TabIndex        =   12
            Top             =   1080
            Width           =   2655
         End
         Begin VB.TextBox txtFiltrarPorDescricao 
            Height          =   285
            Left            =   120
            TabIndex        =   11
            Top             =   720
            Width           =   2655
         End
         Begin VB.ComboBox cmbFiltroPorDescricao 
            Height          =   315
            ItemData        =   "FrmMain.frx":001C
            Left            =   120
            List            =   "FrmMain.frx":0029
            Style           =   2  'Dropdown List
            TabIndex        =   10
            Top             =   360
            Width           =   2655
         End
      End
      Begin VB.Frame Frame1 
         Caption         =   "Filtrar Por Código"
         Height          =   1335
         Left            =   120
         TabIndex        =   5
         Top             =   1440
         Width           =   2895
         Begin VB.CommandButton btnFiltrarPorCodigo 
            Caption         =   "Filtrar"
            Height          =   375
            Left            =   120
            TabIndex        =   9
            Top             =   720
            Width           =   2655
         End
         Begin VB.TextBox txtFiltrarPorCodigo 
            Height          =   285
            Left            =   120
            TabIndex        =   8
            Top             =   360
            Width           =   2655
         End
      End
      Begin VB.CommandButton btnBaixarLista 
         Caption         =   "Baixar Lista NCMs"
         Height          =   375
         Left            =   240
         TabIndex        =   4
         Top             =   960
         Width           =   2775
      End
      Begin VB.CommandButton btnObterNCMs 
         Caption         =   "Obter NCMs"
         Height          =   375
         Left            =   240
         TabIndex        =   3
         Top             =   480
         Width           =   2775
      End
   End
End
Attribute VB_Name = "FrmMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim ncm As ACBrNCM

Private Sub Form_Load()
        
    Dim LogPath As String
    Dim IniPath As String
    
    LogPath = App.Path & "\Logs\"
    IniPath = App.Path & "\ACBrLib.ini"
    
    If Not DirExists(LogPath) Then
        MkDir LogPath
    End If
    
    Set ncm = CreateNCM(IniPath)
    
    ncm.ConfigGravarValor SESSAO_PRINCIPAL, "LogNivel", NivelLog.logParanoico
    ncm.ConfigGravarValor SESSAO_PRINCIPAL, "LogPath", LogPath
    ncm.ConfigGravar

End Sub

Private Sub btnObterNCMs_Click()
    
    On Error GoTo Erro:
    rtbRespostas = ncm.ObterNCMs
    
Erro:
    MsgBox Err.Description
    
End Sub

Private Sub btnBaixarLista_Click()
    
    On Error GoTo Erro:
    Dim file As String
    
    file = "C:\temp\NCMs.txt"
    
    ncm.BaixarLista (file)
    
Erro:
    MsgBox Err.Description
    
End Sub

Private Sub btnFiltrarPorCodigo_Click()

    On Error GoTo Erro:
    rtbRespostas = ncm.BuscarPorCodigo(txtFiltrarPorCodigo.Text)

Erro:
    MsgBox Err.Description

End Sub

Private Sub btnFiltrarPorDescricao_Click()
    
    On Error GoTo Erro:
    rtbRespostas = ncm.BuscarPorDescricao(txtFiltrarPorDescricao.Text, cmbFiltroPorDescricao.ListIndex)

Erro:
    MsgBox Err.Description
    
End Sub

Private Sub btnValidarNCM_Click()

    On Error GoTo Erro:
    rtbRespostas = ncm.Validar(txtValidarNCM.Text)
    
Erro:
    MsgBox Err.Description
    
End Sub

Private Sub SetResposta(ByRef resposta As String)
    If rtbRespostas.Text <> vbNullString Then
      rtbRespostas.Text = rtbRespostas.Text + vbCrLf + resposta
    Else
      rtbRespostas.Text = resposta
    End If
End Sub
