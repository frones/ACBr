VERSION 5.00
Begin VB.Form FrmMain 
   Caption         =   "ACBrLibIBGE Demo"
   ClientHeight    =   4905
   ClientLeft      =   60
   ClientTop       =   405
   ClientWidth     =   9585
   LinkTopic       =   "Form1"
   ScaleHeight     =   4905
   ScaleWidth      =   9585
   StartUpPosition =   2  'CenterScreen
   Begin VB.CommandButton cmdCarregarConfigurações 
      Caption         =   "Carregar Configurações"
      BeginProperty Font 
         Name            =   "Tahoma"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   480
      Left            =   2520
      TabIndex        =   13
      Top             =   1920
      Width           =   2535
   End
   Begin VB.CommandButton cmdSalvarConfigurações 
      Caption         =   "Salvar Configurações"
      BeginProperty Font 
         Name            =   "Tahoma"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   480
      Left            =   120
      TabIndex        =   12
      Top             =   1920
      Width           =   2295
   End
   Begin VB.Frame FraPorCodigo 
      Caption         =   "Por Código"
      Height          =   1695
      Left            =   120
      TabIndex        =   4
      Top             =   120
      Width           =   3015
      Begin VB.TextBox txtCodMunicipio 
         Height          =   315
         Left            =   480
         TabIndex        =   6
         Top             =   600
         Width           =   2055
      End
      Begin VB.CommandButton btnBuscarPorCodigo 
         Caption         =   "Buscar"
         BeginProperty Font 
            Name            =   "Tahoma"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   480
         Index           =   0
         Left            =   480
         TabIndex        =   5
         Top             =   960
         Width           =   2055
      End
      Begin VB.Label lblCodMunicipio 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Cod. Município"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   195
         Left            =   480
         TabIndex        =   8
         Top             =   360
         Width           =   1290
      End
   End
   Begin VB.Frame FraPorNome 
      Caption         =   "Por Nome"
      Height          =   1695
      Left            =   3240
      TabIndex        =   1
      Top             =   120
      Width           =   6135
      Begin VB.TextBox txtUF 
         Height          =   315
         Left            =   4560
         TabIndex        =   10
         Top             =   600
         Width           =   1335
      End
      Begin VB.CheckBox chkCaixaeAcentos 
         Caption         =   "Ignorar Caixa e Acentos"
         Height          =   255
         Left            =   1680
         TabIndex        =   7
         Top             =   1080
         Width           =   2295
      End
      Begin VB.TextBox txtCidade 
         Height          =   315
         Left            =   360
         TabIndex        =   3
         Top             =   600
         Width           =   4095
      End
      Begin VB.CommandButton btnBuscarPorNome 
         Caption         =   "Buscar"
         BeginProperty Font 
            Name            =   "Tahoma"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   585
         Index           =   1
         Left            =   360
         TabIndex        =   2
         Top             =   960
         Width           =   1095
      End
      Begin VB.Label lblUF 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "UF"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   195
         Left            =   4560
         TabIndex        =   11
         Top             =   360
         Width           =   255
      End
      Begin VB.Label lblCidade 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Cidade"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   195
         Left            =   360
         TabIndex        =   9
         Top             =   360
         Width           =   600
      End
   End
   Begin VB.TextBox txtRetorno 
      Height          =   2295
      Left            =   0
      MultiLine       =   -1  'True
      ScrollBars      =   2  'Vertical
      TabIndex        =   0
      Top             =   2520
      Width           =   9495
   End
End
Attribute VB_Name = "FrmMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim ibge As ACBrIBGE

Private Sub btnBuscarPorCodigo_Click(Index As Integer)

     If txtCodMunicipio.Text = "" Then
        MsgBox ("Informe Código do Município")
        Exit Sub
    End If

    Dim retorno As String
    retorno = ibge.BuscarPorCodigo(txtCodMunicipio.Text)
    SetResposta retorno

End Sub

Private Sub btnBuscarPorNome_Click(Index As Integer)

    If txtCidade.Text = "" Then
        MsgBox ("Informe Nome da Cidade")
        Exit Sub
    End If

    Dim retorno As String
    retorno = ibge.BuscarPorNome(txtCidade.Text, txtUF.Text, False)
    SetResposta retorno

End Sub

Private Sub cmdCarregarConfigurações_Click()
    LoadConfig
End Sub

Private Sub cmdSalvarConfigurações_Click()
    SalvarConfig
End Sub

Private Sub Form_Load()
    
    Dim LogPath As String
    Dim IniPath As String
    
    LogPath = App.Path & "\Logs\"
    IniPath = App.Path & "\ACBrLib.ini"
    
    If Not DirExists(LogPath) Then
        MkDir LogPath
    End If
    
    Set ibge = CreateIBGE(IniPath)
    
    ibge.ConfigGravarValor SESSAO_PRINCIPAL, "LogNivel", "4"
    ibge.ConfigGravarValor SESSAO_PRINCIPAL, "LogPath", LogPath
    ibge.ConfigGravar
    
    LoadConfig
    
End Sub

Private Sub LoadConfig()
    
    ibge.ConfigLer
    chkCaixaeAcentos.Value = CLng(ibge.ConfigLerValor(SESSAO_IBGE, "IgnorarCaixaEAcentos"))
        
End Sub

Private Sub SalvarConfig()
    
    ibge.ConfigGravarValor SESSAO_IBGE, "IgnorarCaixaEAcentos", CStr(chkCaixaeAcentos.Value)
    ibge.ConfigGravar
    
End Sub

Private Sub SetResposta(ByRef resposta As String)
    If txtRetorno.Text <> vbNullString Then
      txtRetorno.Text = txtRetorno.Text + vbCrLf + resposta
    Else
      txtRetorno.Text = resposta
    End If
End Sub
