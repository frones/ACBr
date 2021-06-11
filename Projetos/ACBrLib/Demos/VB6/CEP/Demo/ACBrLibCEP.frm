VERSION 5.00
Object = "{BDC217C8-ED16-11CD-956C-0000C04E4C0A}#1.1#0"; "tabctl32.ocx"
Begin VB.Form FrmMain 
   Caption         =   "ACBrLibCEP Demo"
   ClientHeight    =   5760
   ClientLeft      =   60
   ClientTop       =   405
   ClientWidth     =   9750
   LinkTopic       =   "Form1"
   ScaleHeight     =   5760
   ScaleWidth      =   9750
   StartUpPosition =   2  'CenterScreen
   Begin VB.TextBox txtRetorno 
      Height          =   2295
      Left            =   120
      MultiLine       =   -1  'True
      ScrollBars      =   2  'Vertical
      TabIndex        =   1
      Top             =   3360
      Width           =   9495
   End
   Begin TabDlg.SSTab SSTTab0 
      Height          =   3255
      Left            =   120
      TabIndex        =   0
      Top             =   0
      Width           =   9495
      _ExtentX        =   16748
      _ExtentY        =   5741
      _Version        =   393216
      Style           =   1
      Tabs            =   2
      Tab             =   1
      TabsPerRow      =   2
      TabHeight       =   520
      TabCaption(0)   =   "Configuração"
      TabPicture(0)   =   "ACBrLibCEP.frx":0000
      Tab(0).ControlEnabled=   0   'False
      Tab(0).Control(0)=   "Frame1"
      Tab(0).Control(1)=   "btnCarregarConfiguracoes"
      Tab(0).Control(2)=   "btnSalvarConfiguracoes"
      Tab(0).Control(3)=   "frmProxy"
      Tab(0).ControlCount=   4
      TabCaption(1)   =   "Buscar CEP"
      TabPicture(1)   =   "ACBrLibCEP.frx":001C
      Tab(1).ControlEnabled=   -1  'True
      Tab(1).Control(0)=   "frmBuscarPorCEP"
      Tab(1).Control(0).Enabled=   0   'False
      Tab(1).Control(1)=   "frmBuscarPorLogradouro"
      Tab(1).Control(1).Enabled=   0   'False
      Tab(1).ControlCount=   2
      Begin VB.Frame frmBuscarPorLogradouro 
         Caption         =   "Por Endereço"
         Height          =   1695
         Left            =   3240
         TabIndex        =   25
         Top             =   360
         Width           =   6015
         Begin VB.TextBox txtBairro 
            Height          =   315
            Left            =   3000
            TabIndex        =   36
            Top             =   1200
            Width           =   1935
         End
         Begin VB.TextBox txtUF 
            Height          =   315
            Left            =   2280
            TabIndex        =   34
            Top             =   1200
            Width           =   615
         End
         Begin VB.TextBox txtCidade 
            Height          =   315
            Left            =   120
            TabIndex        =   31
            Top             =   1200
            Width           =   2055
         End
         Begin VB.TextBox txtLogradouro 
            Height          =   315
            Left            =   1200
            TabIndex        =   30
            Top             =   480
            Width           =   3735
         End
         Begin VB.CommandButton btnBuscarPorLogradouro 
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
            Height          =   960
            Left            =   5040
            TabIndex        =   27
            Top             =   360
            Width           =   855
         End
         Begin VB.TextBox txtTipoLogradouro 
            Height          =   315
            Left            =   120
            TabIndex        =   26
            Top             =   480
            Width           =   975
         End
         Begin VB.Label lblBairro 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "Bairro"
            BeginProperty Font 
               Name            =   "Tahoma"
               Size            =   8.25
               Charset         =   0
               Weight          =   700
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   195
            Left            =   3000
            TabIndex        =   35
            Top             =   960
            Width           =   510
         End
         Begin VB.Label lblUF 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "UF"
            BeginProperty Font 
               Name            =   "Tahoma"
               Size            =   8.25
               Charset         =   0
               Weight          =   700
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   195
            Left            =   2280
            TabIndex        =   33
            Top             =   960
            Width           =   210
         End
         Begin VB.Label lblCidade 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "Cidade"
            BeginProperty Font 
               Name            =   "Tahoma"
               Size            =   8.25
               Charset         =   0
               Weight          =   700
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   195
            Left            =   120
            TabIndex        =   32
            Top             =   960
            Width           =   570
         End
         Begin VB.Label lblLogradouro 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "Logradouro"
            BeginProperty Font 
               Name            =   "Tahoma"
               Size            =   8.25
               Charset         =   0
               Weight          =   700
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   195
            Left            =   1200
            TabIndex        =   29
            Top             =   240
            Width           =   975
         End
         Begin VB.Label lblTipoEndereco 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "Tipo"
            BeginProperty Font 
               Name            =   "Tahoma"
               Size            =   8.25
               Charset         =   0
               Weight          =   700
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   195
            Left            =   120
            TabIndex        =   28
            Top             =   240
            Width           =   360
         End
      End
      Begin VB.Frame Frame1 
         Caption         =   "WebService"
         Height          =   2175
         Left            =   -74880
         TabIndex        =   17
         Top             =   360
         Width           =   4335
         Begin VB.ComboBox cmbPesquisarIBGE 
            Height          =   315
            ItemData        =   "ACBrLibCEP.frx":0038
            Left            =   120
            List            =   "ACBrLibCEP.frx":0042
            Style           =   2  'Dropdown List
            TabIndex        =   38
            Top             =   1080
            Width           =   2190
         End
         Begin VB.TextBox txtSenhaWebService 
            Height          =   315
            Left            =   2640
            TabIndex        =   21
            Top             =   1080
            Width           =   1455
         End
         Begin VB.TextBox txtChaveWebService 
            Height          =   315
            Left            =   2640
            TabIndex        =   20
            Top             =   1680
            Width           =   1455
         End
         Begin VB.TextBox txtUsuarioWebService 
            Height          =   315
            Left            =   2640
            TabIndex        =   19
            Top             =   480
            Width           =   1455
         End
         Begin VB.ComboBox cmbWebService 
            Height          =   315
            ItemData        =   "ACBrLibCEP.frx":0050
            Left            =   120
            List            =   "ACBrLibCEP.frx":007E
            Style           =   2  'Dropdown List
            TabIndex        =   18
            Top             =   240
            Width           =   2190
         End
         Begin VB.Label lblPesquisarIBGE 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "Pesquisar IBGE"
            BeginProperty Font 
               Name            =   "Tahoma"
               Size            =   8.25
               Charset         =   0
               Weight          =   700
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   195
            Left            =   120
            TabIndex        =   37
            Top             =   840
            Width           =   1260
         End
         Begin VB.Label lblSenhaWebService 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "Senha"
            BeginProperty Font 
               Name            =   "Tahoma"
               Size            =   8.25
               Charset         =   0
               Weight          =   700
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   195
            Left            =   2640
            TabIndex        =   24
            Top             =   840
            Width           =   525
         End
         Begin VB.Label lblChaveWebService 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "Chave"
            BeginProperty Font 
               Name            =   "Tahoma"
               Size            =   8.25
               Charset         =   0
               Weight          =   700
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   195
            Left            =   2640
            TabIndex        =   23
            Top             =   1440
            Width           =   525
         End
         Begin VB.Label lblUsuarioWebService 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "Usuário"
            BeginProperty Font 
               Name            =   "Tahoma"
               Size            =   8.25
               Charset         =   0
               Weight          =   700
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   195
            Left            =   2640
            TabIndex        =   22
            Top             =   240
            Width           =   645
         End
      End
      Begin VB.Frame frmBuscarPorCEP 
         Caption         =   "Por CEP"
         Height          =   1695
         Left            =   120
         TabIndex        =   13
         Top             =   360
         Width           =   3015
         Begin VB.TextBox txtCEP 
            Height          =   315
            Left            =   480
            TabIndex        =   15
            Top             =   600
            Width           =   2055
         End
         Begin VB.CommandButton btnBuscarPorCEP 
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
            Left            =   480
            TabIndex        =   14
            Top             =   960
            Width           =   2055
         End
         Begin VB.Label lblCEP 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "CEP"
            BeginProperty Font 
               Name            =   "Tahoma"
               Size            =   8.25
               Charset         =   0
               Weight          =   700
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   195
            Left            =   480
            TabIndex        =   16
            Top             =   360
            Width           =   300
         End
      End
      Begin VB.CommandButton btnCarregarConfiguracoes 
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
         Left            =   -72480
         TabIndex        =   12
         Top             =   2640
         Width           =   2535
      End
      Begin VB.CommandButton btnSalvarConfiguracoes 
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
         Left            =   -74880
         TabIndex        =   11
         Top             =   2640
         Width           =   2295
      End
      Begin VB.Frame frmProxy 
         Caption         =   "Proxy"
         Height          =   2175
         Left            =   -70440
         TabIndex        =   2
         Top             =   360
         Width           =   4695
         Begin VB.TextBox txtSenhaProxy 
            Height          =   315
            Left            =   2280
            TabIndex        =   10
            Top             =   1080
            Width           =   1455
         End
         Begin VB.TextBox txtUsuarioProxy 
            Height          =   315
            Left            =   360
            TabIndex        =   9
            Top             =   1080
            Width           =   1455
         End
         Begin VB.TextBox txtPortaProxy 
            Height          =   315
            Left            =   2760
            TabIndex        =   8
            Top             =   480
            Width           =   975
         End
         Begin VB.TextBox txtHostProxy 
            Height          =   315
            Left            =   360
            TabIndex        =   4
            Top             =   480
            Width           =   2175
         End
         Begin VB.Label lblSenhaProxy 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "Senha"
            BeginProperty Font 
               Name            =   "Tahoma"
               Size            =   8.25
               Charset         =   0
               Weight          =   700
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   195
            Left            =   2280
            TabIndex        =   7
            Top             =   840
            Width           =   525
         End
         Begin VB.Label lblUsuarioProxy 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "Usuário"
            BeginProperty Font 
               Name            =   "Tahoma"
               Size            =   8.25
               Charset         =   0
               Weight          =   700
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   195
            Left            =   360
            TabIndex        =   6
            Top             =   840
            Width           =   645
         End
         Begin VB.Label lblPortaProxy 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "Porta"
            BeginProperty Font 
               Name            =   "Tahoma"
               Size            =   8.25
               Charset         =   0
               Weight          =   700
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   195
            Left            =   2760
            TabIndex        =   5
            Top             =   240
            Width           =   465
         End
         Begin VB.Label lblHostProxy 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "Host"
            BeginProperty Font 
               Name            =   "Tahoma"
               Size            =   8.25
               Charset         =   0
               Weight          =   700
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   195
            Left            =   360
            TabIndex        =   3
            Top             =   240
            Width           =   390
         End
      End
   End
End
Attribute VB_Name = "FrmMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim cep As ACBrCEP

Private Sub btnBuscarPorCEP_Click()
    
    Dim retorno As String
    retorno = cep.BuscarPorCEP(txtCEP.Text)
    SetResposta retorno
    
End Sub

Private Sub btnBuscarPorLogradouro_Click()
    
    Dim retorno As String
    retorno = cep.BuscarPorLogradouro(txtCidade.Text, txtTipoLogradouro.Text, txtLogradouro.Text, txtUF.Text, txtBairro.Text)
    SetResposta retorno
    
End Sub

Private Sub btnCarregarConfiguracoes_Click()
    LoadConfig
End Sub

Private Sub btnSalvarConfiguracoes_Click()
    SalvarConfig
End Sub

Private Sub Form_Load()
    
    cmbWebService.ListIndex = 0

    Dim LogPath As String
    Dim IniPath As String
    
    LogPath = App.Path & "\Logs\"
    IniPath = App.Path & "\ACBrLib.ini"
    
    If Not DirExists(LogPath) Then
        MkDir LogPath
    End If
    
    Set cep = CreateCEP(IniPath)
    
    cep.ConfigGravarValor SESSAO_PRINCIPAL, "LogNivel", "4"
    cep.ConfigGravarValor SESSAO_PRINCIPAL, "LogPath", LogPath
    cep.ConfigGravar
    
    LoadConfig
    
End Sub

Private Sub LoadConfig()
    
    cep.ConfigLer
    
    cmbWebService.ListIndex = CLng(cep.ConfigLerValor(SESSAO_CEP, "WebService"))
    txtChaveWebService.Text = cep.ConfigLerValor(SESSAO_CEP, "ChaveAcesso")
    txtUsuarioWebService.Text = cep.ConfigLerValor(SESSAO_CEP, "Usuario")
    txtSenhaWebService.Text = cep.ConfigLerValor(SESSAO_CEP, "Senha")
    cmbPesquisarIBGE.ListIndex = CLng(cep.ConfigLerValor(SESSAO_CEP, "PesquisarIBGE"))
    
    txtHostProxy.Text = cep.ConfigLerValor(SESSAO_PROXY, "Servidor")
    
    Dim porta As String
    porta = cep.ConfigLerValor(SESSAO_PROXY, "Porta")
    
    txtUsuarioProxy.Text = cep.ConfigLerValor(SESSAO_PROXY, "Usuario")
    txtSenhaProxy.Text = cep.ConfigLerValor(SESSAO_PROXY, "Senha")
    
End Sub

Private Sub SalvarConfig()
    
    cep.ConfigGravarValor SESSAO_CEP, "WebService", cmbWebService.ListIndex
    cep.ConfigGravarValor SESSAO_CEP, "Usuario", txtUsuarioWebService.Text
    cep.ConfigGravarValor SESSAO_CEP, "Senha", txtSenhaWebService.Text
    cep.ConfigGravarValor SESSAO_CEP, "ChaveAcesso", txtChaveWebService.Text
    cep.ConfigGravarValor SESSAO_CEP, "PesquisarIBGE", cmbPesquisarIBGE.ListIndex
    
    cep.ConfigGravarValor SESSAO_PROXY, "Servidor", txtHostProxy.Text
    cep.ConfigGravarValor SESSAO_PROXY, "Porta", txtPortaProxy.Text
    cep.ConfigGravarValor SESSAO_PROXY, "Usuario", txtUsuarioProxy.Text
    cep.ConfigGravarValor SESSAO_PROXY, "Senha", txtSenhaProxy.Text
    
    cep.ConfigGravar
    
End Sub

Private Sub Form_Unload(Cancel As Integer)
    cep.FinalizarLib
End Sub

Private Sub SetResposta(ByRef resposta As String)
    If txtRetorno.Text <> vbNullString Then
      txtRetorno.Text = txtRetorno.Text + vbCrLf + resposta
    Else
      txtRetorno.Text = resposta
    End If
End Sub
