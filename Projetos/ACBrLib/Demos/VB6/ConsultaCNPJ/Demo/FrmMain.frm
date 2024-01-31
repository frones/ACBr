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
      Tabs            =   1
      TabsPerRow      =   1
      TabHeight       =   520
      TabCaption(0)   =   "Consultas"
      TabPicture(0)   =   "FrmMain.frx":0000
      Tab(0).ControlEnabled=   -1  'True
      Tab(0).Control(0)=   "label1"
      Tab(0).Control(0).Enabled=   0   'False
      Tab(0).Control(1)=   "btnConsultarCNPJ"
      Tab(0).Control(1).Enabled=   0   'False
      Tab(0).Control(2)=   "cmbServico"
      Tab(0).Control(2).Enabled=   0   'False
      Tab(0).ControlCount=   3
      Begin VB.ComboBox cmbServico 
         Height          =   315
         ItemData        =   "FrmMain.frx":001C
         Left            =   360
         List            =   "FrmMain.frx":0029
         TabIndex        =   5
         Top             =   840
         Width           =   1575
      End
      Begin VB.CommandButton btnConsultarCNPJ 
         Caption         =   "Consultar CNPJ"
         Height          =   375
         Left            =   3120
         TabIndex        =   3
         Top             =   840
         Width           =   1575
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
    
    sCNPJ = InputBox("CNPJ", "Informe o CNPJ", "")
    
    ret = cnpj.Consultar(sCNPJ, cmbServico.ListIndex)
    rtbRespostas.Text = ret
    
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

