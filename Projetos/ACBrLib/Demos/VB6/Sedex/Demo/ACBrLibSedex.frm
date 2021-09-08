VERSION 5.00
Begin VB.Form FrmMain 
   Caption         =   "ACBrLibSedex Demo"
   ClientHeight    =   7335
   ClientLeft      =   60
   ClientTop       =   405
   ClientWidth     =   4875
   LinkTopic       =   "Form1"
   ScaleHeight     =   7335
   ScaleWidth      =   4875
   StartUpPosition =   2  'CenterScreen
   Begin VB.TextBox txtSenha 
      Height          =   315
      Left            =   2640
      TabIndex        =   10
      Top             =   600
      Width           =   2055
   End
   Begin VB.TextBox txtCodContrato 
      Height          =   315
      Left            =   240
      TabIndex        =   8
      Top             =   600
      Width           =   2055
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
      Left            =   240
      TabIndex        =   6
      Top             =   3120
      Width           =   2175
   End
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
      TabIndex        =   5
      Top             =   3120
      Width           =   2175
   End
   Begin VB.CommandButton btnConsultar 
      Caption         =   "Consultar"
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
      Index           =   1
      Left            =   2520
      TabIndex        =   4
      Top             =   1920
      Width           =   2175
   End
   Begin VB.CommandButton btnRastrear 
      Caption         =   "Rastrear"
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
      Left            =   240
      TabIndex        =   2
      Top             =   1920
      Width           =   2175
   End
   Begin VB.TextBox txtCodRastreio 
      Height          =   315
      Left            =   240
      TabIndex        =   1
      Top             =   1440
      Width           =   2055
   End
   Begin VB.TextBox txtRetorno 
      Height          =   3495
      Left            =   120
      MultiLine       =   -1  'True
      ScrollBars      =   2  'Vertical
      TabIndex        =   0
      Top             =   3720
      Width           =   4575
   End
   Begin VB.Label lblSeha 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "Senha"
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
      Left            =   2640
      TabIndex        =   9
      Top             =   360
      Width           =   555
   End
   Begin VB.Label lblCodContrato 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "Cod. do Contrato"
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
      Left            =   240
      TabIndex        =   7
      Top             =   360
      Width           =   1455
   End
   Begin VB.Label lblCodRastreio 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "Cod. de Rastreio"
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
      Left            =   240
      TabIndex        =   3
      Top             =   1200
      Width           =   1440
   End
End
Attribute VB_Name = "FrmMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim sedex As ACBrSedex

Private Sub btnConsultar_Click(Index As Integer)

    txtRetorno.Text = sedex.Consultar
    
End Sub

Private Sub btnRastrear_Click(Index As Integer)
    
    If txtCodRastreio.Text = "" Then
       MsgBox ("Informe o Código de Rastreio")
       Exit Sub
    End If
    
    Dim retorno As String
    retorno = sedex.Rastrear(txtCodRastreio.Text)
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
    
    Set sedex = CreateSedex(IniPath)
    
    sedex.ConfigGravarValor SESSAO_PRINCIPAL, "LogNivel", "4"
    sedex.ConfigGravarValor SESSAO_PRINCIPAL, "LogPath", LogPath
    sedex.ConfigGravar
    
    LoadConfig
    
End Sub

Private Sub LoadConfig()
    
    sedex.ConfigLer
    
    txtCodContrato.Text = sedex.ConfigLerValor(SESSAO_Sedex, "CodContrato")
    txtSenha.Text = sedex.ConfigLerValor(SESSAO_Sedex, "Senha")
        
End Sub

Private Sub SalvarConfig()
    
    sedex.ConfigGravarValor SESSAO_Sedex, "CodContrato", txtCodContrato.Text
    sedex.ConfigGravarValor SESSAO_Sedex, "Senha", txtSenha.Text
    
    sedex.ConfigGravar
    
End Sub

Private Sub SetResposta(ByRef resposta As String)
    If txtRetorno.Text <> vbNullString Then
      txtRetorno.Text = txtRetorno.Text + vbCrLf + resposta
    Else
      txtRetorno.Text = resposta
    End If
End Sub
