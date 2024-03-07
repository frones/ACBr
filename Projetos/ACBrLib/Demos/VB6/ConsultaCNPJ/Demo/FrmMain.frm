VERSION 5.00
Object = "{BDC217C8-ED16-11CD-956C-0000C04E4C0A}#1.1#0"; "TABCTL32.OCX"
Begin VB.Form FrmMain 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "ACBrLibConsultaCNPJ Demo"
   ClientHeight    =   4905
   ClientLeft      =   45
   ClientTop       =   390
   ClientWidth     =   12150
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
   ScaleHeight     =   4905
   ScaleWidth      =   12150
   StartUpPosition =   2  'CenterScreen
   Begin VB.Frame FraRespostas 
      Caption         =   "Respostas"
      Height          =   4575
      Left            =   5640
      TabIndex        =   0
      Top             =   120
      Width           =   6375
      Begin VB.TextBox rtbRespostas 
         Height          =   4095
         Left            =   120
         Locked          =   -1  'True
         MultiLine       =   -1  'True
         ScrollBars      =   3  'Both
         TabIndex        =   1
         Top             =   240
         Width           =   6015
      End
   End
   Begin TabDlg.SSTab SSTab1 
      Height          =   4455
      Left            =   120
      TabIndex        =   2
      Top             =   120
      Width           =   5415
      _ExtentX        =   9551
      _ExtentY        =   7858
      _Version        =   393216
      Style           =   1
      Tabs            =   2
      TabsPerRow      =   2
      TabHeight       =   520
      TabCaption(0)   =   "Consultas"
      TabPicture(0)   =   "FrmMain.frx":0000
      Tab(0).ControlEnabled=   -1  'True
      Tab(0).Control(0)=   "label1"
      Tab(0).Control(0).Enabled=   0   'False
      Tab(0).Control(1)=   "Label2"
      Tab(0).Control(1).Enabled=   0   'False
      Tab(0).Control(2)=   "Label3"
      Tab(0).Control(2).Enabled=   0   'False
      Tab(0).Control(3)=   "Label4"
      Tab(0).Control(3).Enabled=   0   'False
      Tab(0).Control(4)=   "btnConsultarCNPJ"
      Tab(0).Control(4).Enabled=   0   'False
      Tab(0).Control(5)=   "cmbServico"
      Tab(0).Control(5).Enabled=   0   'False
      Tab(0).Control(6)=   "txtCNPJ"
      Tab(0).Control(6).Enabled=   0   'False
      Tab(0).Control(7)=   "txtUsuario"
      Tab(0).Control(7).Enabled=   0   'False
      Tab(0).Control(8)=   "txtSenha"
      Tab(0).Control(8).Enabled=   0   'False
      Tab(0).Control(9)=   "btnSalvarConfiguracoes"
      Tab(0).Control(9).Enabled=   0   'False
      Tab(0).Control(10)=   "btnCarregarConfiguracoes"
      Tab(0).Control(10).Enabled=   0   'False
      Tab(0).ControlCount=   11
      TabCaption(1)   =   "Proxy"
      TabPicture(1)   =   "FrmMain.frx":001C
      Tab(1).ControlEnabled=   0   'False
      Tab(1).Control(0)=   "txtSenhaProxy"
      Tab(1).Control(0).Enabled=   0   'False
      Tab(1).Control(1)=   "txtUsuarioProxy"
      Tab(1).Control(1).Enabled=   0   'False
      Tab(1).Control(2)=   "txtPortaProxy"
      Tab(1).Control(2).Enabled=   0   'False
      Tab(1).Control(3)=   "txtServidorProxy"
      Tab(1).Control(3).Enabled=   0   'False
      Tab(1).Control(4)=   "Label8"
      Tab(1).Control(4).Enabled=   0   'False
      Tab(1).Control(5)=   "Label7"
      Tab(1).Control(5).Enabled=   0   'False
      Tab(1).Control(6)=   "Label6"
      Tab(1).Control(6).Enabled=   0   'False
      Tab(1).Control(7)=   "Label5"
      Tab(1).Control(7).Enabled=   0   'False
      Tab(1).ControlCount=   8
      Begin VB.TextBox txtSenhaProxy 
         Height          =   375
         Left            =   -72720
         TabIndex        =   21
         Top             =   2160
         Width           =   1695
      End
      Begin VB.TextBox txtUsuarioProxy 
         Height          =   375
         Left            =   -72720
         TabIndex        =   19
         Top             =   1680
         Width           =   1695
      End
      Begin VB.TextBox txtPortaProxy 
         Height          =   375
         Left            =   -72720
         TabIndex        =   17
         Top             =   1200
         Width           =   1695
      End
      Begin VB.TextBox txtServidorProxy 
         Height          =   375
         Left            =   -72720
         TabIndex        =   15
         Top             =   720
         Width           =   1695
      End
      Begin VB.CommandButton btnCarregarConfiguracoes 
         Caption         =   "Carregar Configurações"
         Height          =   375
         Left            =   360
         TabIndex        =   13
         Top             =   2400
         Width           =   1935
      End
      Begin VB.CommandButton btnSalvarConfiguracoes 
         Caption         =   "Gravar Configurações"
         Height          =   375
         Left            =   3000
         TabIndex        =   12
         Top             =   2400
         Width           =   1815
      End
      Begin VB.TextBox txtSenha 
         Height          =   375
         Left            =   3120
         TabIndex        =   11
         Top             =   1560
         Width           =   1695
      End
      Begin VB.TextBox txtUsuario 
         Height          =   375
         Left            =   3120
         TabIndex        =   9
         Top             =   840
         Width           =   1695
      End
      Begin VB.TextBox txtCNPJ 
         Height          =   375
         Left            =   480
         TabIndex        =   7
         Top             =   3600
         Width           =   1695
      End
      Begin VB.ComboBox cmbServico 
         Height          =   315
         ItemData        =   "FrmMain.frx":0038
         Left            =   360
         List            =   "FrmMain.frx":0048
         TabIndex        =   5
         Top             =   840
         Width           =   1575
      End
      Begin VB.CommandButton btnConsultarCNPJ 
         Caption         =   "Consultar CNPJ"
         Height          =   375
         Left            =   3120
         TabIndex        =   3
         Top             =   3600
         Width           =   1575
      End
      Begin VB.Label Label8 
         Caption         =   "Senha"
         Height          =   255
         Left            =   -74040
         TabIndex        =   20
         Top             =   2280
         Width           =   1215
      End
      Begin VB.Label Label7 
         Caption         =   "Usuário"
         Height          =   255
         Left            =   -74040
         TabIndex        =   18
         Top             =   1800
         Width           =   1215
      End
      Begin VB.Label Label6 
         Caption         =   "Porta"
         Height          =   255
         Left            =   -74040
         TabIndex        =   16
         Top             =   1320
         Width           =   1215
      End
      Begin VB.Label Label5 
         Caption         =   "Servidor"
         Height          =   255
         Left            =   -74040
         TabIndex        =   14
         Top             =   840
         Width           =   1215
      End
      Begin VB.Label Label4 
         Caption         =   "Senha"
         Height          =   255
         Left            =   3120
         TabIndex        =   10
         Top             =   1320
         Width           =   1215
      End
      Begin VB.Label Label3 
         Caption         =   "Usuário"
         Height          =   255
         Left            =   3120
         TabIndex        =   8
         Top             =   600
         Width           =   1215
      End
      Begin VB.Label Label2 
         Caption         =   "Informe CNPJ"
         Height          =   255
         Left            =   480
         TabIndex        =   6
         Top             =   3360
         Width           =   1215
      End
      Begin VB.Label label1 
         Caption         =   "Serviço"
         Height          =   255
         Left            =   360
         TabIndex        =   4
         Top             =   600
         Width           =   975
      End
   End
End
Attribute VB_Name = "FrmMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim cnpj As ACBrConsultaCNPJ

Private Sub btnConsultarCaptcha_Click()

    Dim captchaPath As String
    
    captchaPath = App.Path & "\Captcha\"
    
    If Not DirExists(captchaPath) Then
        MkDir captchaPath
    End If
    
    cnpj.ConsultarCaptcha (captchaPath)
    
End Sub



Private Sub btnConsultarCNPJ_Click()
        
    ret = cnpj.Consultar(txtCNPJ.Text)
    rtbRespostas.Text = ret
    
End Sub

Private Sub btnSalvarConfiguracoes_Click()
    SalvarConfig
End Sub

Private Sub SalvarConfig()

    cnpj.ConfigGravarValor SESSAO_ConsultaCNPJ, "Provedor", CStr(cmbServico.ListIndex)
    cnpj.ConfigGravarValor SESSAO_ConsultaCNPJ, "Usuario", txtUsuario.Text
    cnpj.ConfigGravarValor SESSAO_ConsultaCNPJ, "Senha", txtSenha.Text
    
    cnpj.ConfigGravarValor SESSAO_PROXY, "Servidor", txtServidorProxy.Text
    cnpj.ConfigGravarValor SESSAO_PROXY, "Porta", txtPortaProxy.Text
    cnpj.ConfigGravarValor SESSAO_PROXY, "Usuario", txtUsuarioProxy.Text
    cnpj.ConfigGravarValor SESSAO_PROXY, "Senha", txtSenhaProxy.Text
    
End Sub

Private Sub btnCarregarConfiguracoes_Click()
    LoadConfig
End Sub

Private Sub LoadConfig()

    cmbServico.ListIndex = CLng(cnpj.ConfigLerValor(SESSAO_ConsultaCNPJ, "Provedor"))
    txtUsuario.Text = cnpj.ConfigLerValor(SESSAO_ConsultaCNPJ, "Usuario")
    txtSenha.Text = cnpj.ConfigLerValor(SESSAO_ConsultaCNPJ, "Senha")
    
    txtServidorProxy.Text = cnpj.ConfigLerValor(SESSAO_PROXY, "Servidor")
    txtPortaProxy.Text = cnpj.ConfigLerValor(SESSAO_PROXY, "Porta")
    txtUsuarioProxy.Text = cnpj.ConfigLerValor(SESSAO_PROXY, "Usuario")
    txtSenhaProxy.Text = cnpj.ConfigLerValor(SESSAO_PROXY, "Senha")
    
End Sub

Private Sub Form_Load()

    Dim LogPath As String
    Dim IniPath As String
    
    LogPath = App.Path & "\Logs\"
    IniPath = App.Path & "\ACBrLib.ini"
    
    If Not DirExists(LogPath) Then
        MkDir LogPath
    End If
    
    Set cnpj = CreateCNPJ(IniPath)
    
    cnpj.ConfigGravarValor SESSAO_PRINCIPAL, "LogNivel", NivelLog.logParanoico
    cnpj.ConfigGravarValor SESSAO_PRINCIPAL, "LogPath", LogPath
    cnpj.ConfigGravar
    
End Sub

Private Sub SetResposta(ByRef resposta As String)
    If rtbRespostas.Text <> vbNullString Then
      rtbRespostas.Text = rtbRespostas.Text + vbCrLf + resposta
    Else
      rtbRespostas.Text = resposta
    End If
End Sub

