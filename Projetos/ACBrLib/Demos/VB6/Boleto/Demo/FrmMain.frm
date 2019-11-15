VERSION 5.00
Object = "{86CF1D34-0C5F-11D2-A9FC-0000F8754DA1}#2.0#0"; "mscomct2.ocx"
Object = "{BDC217C8-ED16-11CD-956C-0000C04E4C0A}#1.1#0"; "tabctl32.ocx"
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "comdlg32.ocx"
Begin VB.Form FrmMain 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "ACBrLibBoleto Demo"
   ClientHeight    =   6135
   ClientLeft      =   45
   ClientTop       =   390
   ClientWidth     =   10440
   BeginProperty Font 
      Name            =   "Tahoma"
      Size            =   8.25
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   6135
   ScaleWidth      =   10440
   StartUpPosition =   2  'CenterScreen
   Begin MSComDlg.CommonDialog CommonDialog1 
      Left            =   5280
      Top             =   3120
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
   End
   Begin VB.CommandButton cmdLimparLista 
      Caption         =   "Limpar Lista"
      Height          =   360
      Left            =   240
      TabIndex        =   49
      Top             =   5520
      Width           =   1710
   End
   Begin VB.CommandButton cmdTotalTítulos 
      Caption         =   "Total Títulos"
      Height          =   360
      Left            =   240
      TabIndex        =   48
      Top             =   5040
      Width           =   1710
   End
   Begin VB.CommandButton btnGerarRemessa 
      Caption         =   "Gerar Remessa"
      Height          =   360
      Left            =   240
      TabIndex        =   47
      Top             =   4560
      Width           =   1710
   End
   Begin VB.CommandButton btnImprimir 
      Caption         =   "Imprimir Boleto"
      Height          =   360
      Left            =   240
      TabIndex        =   46
      Top             =   4080
      Width           =   1710
   End
   Begin VB.CommandButton btnIncluirTitulo 
      Caption         =   "Incluir Titulos"
      Height          =   360
      Left            =   240
      TabIndex        =   45
      Top             =   3600
      Width           =   1710
   End
   Begin VB.CommandButton btnGravarConfig 
      Caption         =   "Salvar Configurações"
      Height          =   480
      Left            =   8400
      TabIndex        =   43
      Top             =   3120
      Width           =   1935
   End
   Begin VB.Frame FraRespostas 
      Caption         =   "Respostas"
      Height          =   2415
      Left            =   2160
      TabIndex        =   42
      Top             =   3600
      Width           =   8175
      Begin VB.TextBox rtbRespostas 
         Height          =   2055
         Left            =   120
         MultiLine       =   -1  'True
         TabIndex        =   44
         Top             =   240
         Width           =   7935
      End
   End
   Begin TabDlg.SSTab SSTTab0 
      Height          =   3015
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   10335
      _ExtentX        =   18230
      _ExtentY        =   5318
      _Version        =   393216
      Style           =   1
      Tabs            =   5
      TabsPerRow      =   5
      TabHeight       =   520
      TabCaption(0)   =   "Cedente"
      TabPicture(0)   =   "FrmMain.frx":0000
      Tab(0).ControlEnabled=   -1  'True
      Tab(0).Control(0)=   "lblPessoa"
      Tab(0).Control(0).Enabled=   0   'False
      Tab(0).Control(1)=   "lblNomeRz"
      Tab(0).Control(1).Enabled=   0   'False
      Tab(0).Control(2)=   "lblCNPJCPF"
      Tab(0).Control(2).Enabled=   0   'False
      Tab(0).Control(3)=   "lblLogradouro"
      Tab(0).Control(3).Enabled=   0   'False
      Tab(0).Control(4)=   "lblBairro"
      Tab(0).Control(4).Enabled=   0   'False
      Tab(0).Control(5)=   "lblNumero"
      Tab(0).Control(5).Enabled=   0   'False
      Tab(0).Control(6)=   "lblComplemento"
      Tab(0).Control(6).Enabled=   0   'False
      Tab(0).Control(7)=   "lblCEP"
      Tab(0).Control(7).Enabled=   0   'False
      Tab(0).Control(8)=   "lblUF"
      Tab(0).Control(8).Enabled=   0   'False
      Tab(0).Control(9)=   "lblCidade"
      Tab(0).Control(9).Enabled=   0   'False
      Tab(0).Control(10)=   "lblDocumento"
      Tab(0).Control(10).Enabled=   0   'False
      Tab(0).Control(11)=   "lblTelefone"
      Tab(0).Control(11).Enabled=   0   'False
      Tab(0).Control(12)=   "lblCarteira"
      Tab(0).Control(12).Enabled=   0   'False
      Tab(0).Control(13)=   "cmbTipoInscricao"
      Tab(0).Control(13).Enabled=   0   'False
      Tab(0).Control(14)=   "txtNomeRes"
      Tab(0).Control(14).Enabled=   0   'False
      Tab(0).Control(15)=   "txtCNPJCPF"
      Tab(0).Control(15).Enabled=   0   'False
      Tab(0).Control(16)=   "txtLogradouro"
      Tab(0).Control(16).Enabled=   0   'False
      Tab(0).Control(17)=   "txtBairro"
      Tab(0).Control(17).Enabled=   0   'False
      Tab(0).Control(18)=   "txtNumeroRes"
      Tab(0).Control(18).Enabled=   0   'False
      Tab(0).Control(19)=   "txtComplemento"
      Tab(0).Control(19).Enabled=   0   'False
      Tab(0).Control(20)=   "txtCEP"
      Tab(0).Control(20).Enabled=   0   'False
      Tab(0).Control(21)=   "cmbUF"
      Tab(0).Control(21).Enabled=   0   'False
      Tab(0).Control(22)=   "txtCidade"
      Tab(0).Control(22).Enabled=   0   'False
      Tab(0).Control(23)=   "cmbTipoDocumento"
      Tab(0).Control(23).Enabled=   0   'False
      Tab(0).Control(24)=   "txtTelefone"
      Tab(0).Control(24).Enabled=   0   'False
      Tab(0).Control(25)=   "cmbTipoCarteira"
      Tab(0).Control(25).Enabled=   0   'False
      Tab(0).ControlCount=   26
      TabCaption(1)   =   "Layout"
      TabPicture(1)   =   "FrmMain.frx":001C
      Tab(1).ControlEnabled=   0   'False
      Tab(1).Control(0)=   "lblImpressora"
      Tab(1).Control(1)=   "lblModelo"
      Tab(1).Control(2)=   "lblDiretórioLogotipo"
      Tab(1).Control(3)=   "lblCopias"
      Tab(1).Control(4)=   "lblNomeArquivo"
      Tab(1).Control(5)=   "cmbImpressora"
      Tab(1).Control(6)=   "cmbModeloImpressao"
      Tab(1).Control(7)=   "txtDirLogo"
      Tab(1).Control(8)=   "btnDirLogo"
      Tab(1).Control(9)=   "txtCopias"
      Tab(1).Control(10)=   "nudCopias"
      Tab(1).Control(11)=   "txtNomeArquivo"
      Tab(1).Control(12)=   "chkPreview"
      Tab(1).Control(13)=   "chkSetup"
      Tab(1).Control(14)=   "chkProgresso"
      Tab(1).ControlCount=   15
      TabCaption(2)   =   "Conta Bancária"
      TabPicture(2)   =   "FrmMain.frx":0038
      Tab(2).ControlEnabled=   0   'False
      Tab(2).Control(0)=   "lblBanco"
      Tab(2).Control(1)=   "lblAgência"
      Tab(2).Control(2)=   "lblConta"
      Tab(2).Control(3)=   "lblRespEmissão"
      Tab(2).Control(4)=   "lblCodTransmissão"
      Tab(2).Control(5)=   "lblConvenio"
      Tab(2).Control(6)=   "lblModalidade"
      Tab(2).Control(7)=   "lblCodCedente"
      Tab(2).Control(8)=   "cmbBanco"
      Tab(2).Control(9)=   "txtAgencia"
      Tab(2).Control(10)=   "txtConta"
      Tab(2).Control(11)=   "txtDigConta"
      Tab(2).Control(12)=   "cmbRespEmissao"
      Tab(2).Control(13)=   "txtDigAgencia"
      Tab(2).Control(14)=   "txtCodTransmissao"
      Tab(2).Control(15)=   "txtModalidade"
      Tab(2).Control(16)=   "txtCodCedente"
      Tab(2).Control(17)=   "txtConvenio"
      Tab(2).ControlCount=   18
      TabCaption(3)   =   "Remessa/Retorno"
      TabPicture(3)   =   "FrmMain.frx":0054
      Tab(3).ControlEnabled=   0   'False
      Tab(3).Control(0)=   "lblLayoutCNAB"
      Tab(3).Control(1)=   "lblDiretórioArquivo"
      Tab(3).Control(2)=   "lblDirArquivoRetorno"
      Tab(3).Control(3)=   "lblNomeArquivoRemessa"
      Tab(3).Control(4)=   "lblNomeArquivoRetorno"
      Tab(3).Control(5)=   "cmbLayoutCNAB"
      Tab(3).Control(6)=   "btnDirRemessa"
      Tab(3).Control(7)=   "txtDirRemessa"
      Tab(3).Control(8)=   "ckbCedenteRetorno"
      Tab(3).Control(9)=   "btnDirRetorno"
      Tab(3).Control(10)=   "txtDirRetorno"
      Tab(3).Control(11)=   "txtNomeRemessa"
      Tab(3).Control(12)=   "txtNomeRetorno"
      Tab(3).ControlCount=   13
      TabCaption(4)   =   "Email"
      TabPicture(4)   =   "FrmMain.frx":0070
      Tab(4).ControlEnabled=   0   'False
      Tab(4).Control(0)=   "lblPorta"
      Tab(4).Control(1)=   "lblHostSMTP"
      Tab(4).Control(2)=   "lblSenha"
      Tab(4).Control(3)=   "lblUsuário"
      Tab(4).Control(4)=   "lblEmail"
      Tab(4).Control(5)=   "lblNome"
      Tab(4).Control(6)=   "nudPorta"
      Tab(4).Control(7)=   "chkTLS"
      Tab(4).Control(8)=   "chkSSL"
      Tab(4).Control(9)=   "txt"
      Tab(4).Control(10)=   "txtHost"
      Tab(4).Control(11)=   "txtSenha"
      Tab(4).Control(12)=   "txtEmail"
      Tab(4).Control(13)=   "txtNome"
      Tab(4).Control(14)=   "txtUsuario"
      Tab(4).ControlCount=   15
      Begin VB.TextBox txtUsuario 
         Height          =   315
         Left            =   -70920
         TabIndex        =   95
         Top             =   720
         Width           =   3735
      End
      Begin VB.TextBox txtNome 
         Height          =   315
         Left            =   -74760
         TabIndex        =   87
         Top             =   720
         Width           =   3735
      End
      Begin VB.TextBox txtEmail 
         Height          =   315
         Left            =   -74760
         TabIndex        =   86
         Top             =   1320
         Width           =   3735
      End
      Begin VB.TextBox txtSenha 
         Height          =   315
         IMEMode         =   3  'DISABLE
         Left            =   -70920
         PasswordChar    =   "*"
         TabIndex        =   85
         Top             =   1320
         Width           =   3735
      End
      Begin VB.TextBox txtHost 
         Height          =   315
         Left            =   -74760
         TabIndex        =   84
         Top             =   1920
         Width           =   6255
      End
      Begin VB.TextBox txt 
         Alignment       =   1  'Right Justify
         Height          =   315
         Left            =   -68400
         TabIndex        =   83
         Text            =   "0"
         Top             =   1920
         Width           =   960
      End
      Begin VB.CheckBox chkSSL 
         Caption         =   "SSL"
         BeginProperty Font 
            Name            =   "Tahoma"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Left            =   -67080
         TabIndex        =   82
         Top             =   720
         Width           =   1335
      End
      Begin VB.CheckBox chkTLS 
         Caption         =   "TLS"
         BeginProperty Font 
            Name            =   "Tahoma"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Left            =   -67080
         TabIndex        =   81
         Top             =   1320
         Width           =   1215
      End
      Begin VB.TextBox txtNomeRetorno 
         Height          =   285
         Left            =   -69600
         TabIndex        =   80
         Top             =   1920
         Width           =   2415
      End
      Begin VB.TextBox txtNomeRemessa 
         Height          =   285
         Left            =   -69600
         TabIndex        =   78
         Top             =   1320
         Width           =   2415
      End
      Begin VB.TextBox txtDirRetorno 
         Height          =   285
         Left            =   -74760
         TabIndex        =   75
         Top             =   1920
         Width           =   4695
      End
      Begin VB.CommandButton btnDirRetorno 
         Caption         =   "..."
         Height          =   285
         Left            =   -70080
         TabIndex        =   74
         Top             =   1905
         Width           =   390
      End
      Begin VB.CheckBox ckbCedenteRetorno 
         Caption         =   "Lê Cedente Retorno"
         BeginProperty Font 
            Name            =   "Tahoma"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   315
         Left            =   -73440
         TabIndex        =   73
         Top             =   720
         Width           =   2775
      End
      Begin VB.TextBox txtDirRemessa 
         Height          =   285
         Left            =   -74760
         TabIndex        =   71
         Top             =   1320
         Width           =   4695
      End
      Begin VB.CommandButton btnDirRemessa 
         Caption         =   "..."
         Height          =   285
         Left            =   -70080
         TabIndex        =   70
         Top             =   1305
         Width           =   390
      End
      Begin VB.ComboBox cmbLayoutCNAB 
         Height          =   315
         ItemData        =   "FrmMain.frx":008C
         Left            =   -74760
         List            =   "FrmMain.frx":0096
         Style           =   2  'Dropdown List
         TabIndex        =   68
         Top             =   720
         Width           =   1215
      End
      Begin VB.TextBox txtConvenio 
         Height          =   285
         Left            =   -72240
         TabIndex        =   67
         Top             =   1320
         Width           =   2415
      End
      Begin VB.TextBox txtCodCedente 
         Height          =   285
         Left            =   -67200
         TabIndex        =   63
         Top             =   1320
         Width           =   2415
      End
      Begin VB.TextBox txtModalidade 
         Height          =   285
         Left            =   -69720
         TabIndex        =   62
         Top             =   1320
         Width           =   2415
      End
      Begin VB.TextBox txtCodTransmissao 
         Height          =   285
         Left            =   -74760
         TabIndex        =   60
         Top             =   1320
         Width           =   2415
      End
      Begin VB.TextBox txtDigAgencia 
         Height          =   285
         Left            =   -70200
         TabIndex        =   59
         Top             =   720
         Width           =   375
      End
      Begin VB.ComboBox cmbRespEmissao 
         Height          =   315
         ItemData        =   "FrmMain.frx":00AC
         Left            =   -67200
         List            =   "FrmMain.frx":00BC
         Style           =   2  'Dropdown List
         TabIndex        =   57
         Top             =   720
         Width           =   2415
      End
      Begin VB.TextBox txtDigConta 
         Height          =   285
         Left            =   -67680
         TabIndex        =   56
         Top             =   720
         Width           =   375
      End
      Begin VB.TextBox txtConta 
         Height          =   285
         Left            =   -69720
         TabIndex        =   54
         Top             =   720
         Width           =   1935
      End
      Begin VB.TextBox txtAgencia 
         Height          =   285
         Left            =   -72240
         TabIndex        =   52
         Top             =   720
         Width           =   1935
      End
      Begin VB.ComboBox cmbBanco 
         Height          =   315
         ItemData        =   "FrmMain.frx":00FD
         Left            =   -74760
         List            =   "FrmMain.frx":0158
         Style           =   2  'Dropdown List
         TabIndex        =   50
         Top             =   720
         Width           =   2415
      End
      Begin VB.CheckBox chkProgresso 
         Caption         =   "Mostrar Progresso"
         BeginProperty Font 
            Name            =   "Tahoma"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   315
         Left            =   -69960
         TabIndex        =   41
         Top             =   1680
         Width           =   2055
      End
      Begin VB.CheckBox chkSetup 
         Caption         =   "Mostrar Setup"
         BeginProperty Font 
            Name            =   "Tahoma"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Left            =   -69960
         TabIndex        =   40
         Top             =   1440
         Width           =   2295
      End
      Begin VB.CheckBox chkPreview 
         Caption         =   "Mostrar Preview"
         BeginProperty Font 
            Name            =   "Tahoma"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Left            =   -69960
         TabIndex        =   39
         Top             =   1200
         Width           =   1695
      End
      Begin VB.TextBox txtNomeArquivo 
         Height          =   285
         Left            =   -72120
         TabIndex        =   37
         Text            =   "BOLETO"
         Top             =   1320
         Width           =   1935
      End
      Begin MSComCtl2.UpDown nudCopias 
         Height          =   285
         Left            =   -74039
         TabIndex        =   36
         Top             =   1320
         Width           =   255
         _ExtentX        =   450
         _ExtentY        =   503
         _Version        =   393216
         Value           =   1
         AutoBuddy       =   -1  'True
         BuddyControl    =   "txtCopias"
         BuddyDispid     =   196647
         OrigLeft        =   1440
         OrigTop         =   1440
         OrigRight       =   1695
         OrigBottom      =   1695
         Max             =   9999
         Min             =   1
         SyncBuddy       =   -1  'True
         BuddyProperty   =   0
         Enabled         =   -1  'True
      End
      Begin VB.TextBox txtCopias 
         Height          =   285
         Left            =   -74880
         TabIndex        =   35
         Text            =   "1"
         Top             =   1320
         Width           =   840
      End
      Begin VB.CommandButton btnDirLogo 
         Caption         =   "..."
         Height          =   285
         Left            =   -65400
         TabIndex        =   33
         Top             =   710
         Width           =   390
      End
      Begin VB.TextBox txtDirLogo 
         Height          =   285
         Left            =   -70080
         TabIndex        =   31
         Top             =   720
         Width           =   4695
      End
      Begin VB.ComboBox cmbModeloImpressao 
         Height          =   315
         ItemData        =   "FrmMain.frx":0307
         Left            =   -72120
         List            =   "FrmMain.frx":0320
         Style           =   2  'Dropdown List
         TabIndex        =   29
         Top             =   720
         Width           =   1935
      End
      Begin VB.ComboBox cmbImpressora 
         Height          =   315
         ItemData        =   "FrmMain.frx":037B
         Left            =   -74880
         List            =   "FrmMain.frx":037D
         TabIndex        =   27
         Text            =   "cmbImpressora"
         Top             =   720
         Width           =   2655
      End
      Begin VB.ComboBox cmbTipoCarteira 
         Height          =   315
         ItemData        =   "FrmMain.frx":037F
         Left            =   4680
         List            =   "FrmMain.frx":038C
         Style           =   2  'Dropdown List
         TabIndex        =   25
         Top             =   2520
         Width           =   1935
      End
      Begin VB.TextBox txtTelefone 
         Height          =   285
         Left            =   120
         TabIndex        =   23
         Top             =   2520
         Width           =   2415
      End
      Begin VB.ComboBox cmbTipoDocumento 
         Height          =   315
         ItemData        =   "FrmMain.frx":03BA
         Left            =   2640
         List            =   "FrmMain.frx":03C4
         Style           =   2  'Dropdown List
         TabIndex        =   21
         Top             =   2520
         Width           =   1935
      End
      Begin VB.TextBox txtCidade 
         Height          =   285
         Left            =   4440
         TabIndex        =   19
         Top             =   1920
         Width           =   3255
      End
      Begin VB.ComboBox cmbUF 
         Height          =   315
         ItemData        =   "FrmMain.frx":03E1
         Left            =   7800
         List            =   "FrmMain.frx":0436
         TabIndex        =   18
         Text            =   "cmbUF"
         Top             =   1920
         Width           =   855
      End
      Begin VB.TextBox txtCEP 
         Height          =   285
         Left            =   8760
         TabIndex        =   15
         Top             =   1920
         Width           =   1455
      End
      Begin VB.TextBox txtComplemento 
         Height          =   285
         Left            =   120
         TabIndex        =   13
         Top             =   1920
         Width           =   4215
      End
      Begin VB.TextBox txtNumeroRes 
         Height          =   285
         Left            =   6600
         TabIndex        =   11
         Top             =   1320
         Width           =   1095
      End
      Begin VB.TextBox txtBairro 
         Height          =   285
         Left            =   7800
         TabIndex        =   9
         Top             =   1320
         Width           =   2415
      End
      Begin VB.TextBox txtLogradouro 
         Height          =   285
         Left            =   120
         TabIndex        =   8
         Top             =   1320
         Width           =   6375
      End
      Begin VB.TextBox txtCNPJCPF 
         Height          =   285
         Left            =   7800
         TabIndex        =   5
         Top             =   720
         Width           =   2415
      End
      Begin VB.TextBox txtNomeRes 
         Height          =   285
         Left            =   1920
         TabIndex        =   4
         Top             =   720
         Width           =   5775
      End
      Begin VB.ComboBox cmbTipoInscricao 
         Height          =   315
         ItemData        =   "FrmMain.frx":04A6
         Left            =   120
         List            =   "FrmMain.frx":04B0
         Style           =   2  'Dropdown List
         TabIndex        =   2
         Top             =   720
         Width           =   1695
      End
      Begin MSComCtl2.UpDown nudPorta 
         Height          =   315
         Left            =   -67440
         TabIndex        =   88
         Top             =   1920
         Width           =   255
         _ExtentX        =   450
         _ExtentY        =   556
         _Version        =   393216
         BuddyControl    =   "chkSetup"
         BuddyDispid     =   196644
         OrigLeft        =   6240
         OrigTop         =   1320
         OrigRight       =   6495
         OrigBottom      =   1575
         Max             =   99999
         SyncBuddy       =   -1  'True
         BuddyProperty   =   0
         Enabled         =   -1  'True
      End
      Begin VB.Label lblNome 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Nome"
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
         Left            =   -74760
         TabIndex        =   94
         Top             =   480
         Width           =   480
      End
      Begin VB.Label lblEmail 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Email"
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
         Left            =   -74760
         TabIndex        =   93
         Top             =   1080
         Width           =   450
      End
      Begin VB.Label lblUsuário 
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
         Left            =   -70920
         TabIndex        =   92
         Top             =   480
         Width           =   645
      End
      Begin VB.Label lblSenha 
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
         Left            =   -70920
         TabIndex        =   91
         Top             =   1080
         Width           =   525
      End
      Begin VB.Label lblHostSMTP 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Host SMTP"
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
         Left            =   -74760
         TabIndex        =   90
         Top             =   1680
         Width           =   900
      End
      Begin VB.Label lblPorta 
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
         Left            =   -68400
         TabIndex        =   89
         Top             =   1680
         Width           =   465
      End
      Begin VB.Label lblNomeArquivoRetorno 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Nome Arquivo Retorno"
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
         Left            =   -69600
         TabIndex        =   79
         Top             =   1680
         Width           =   1920
      End
      Begin VB.Label lblNomeArquivoRemessa 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Nome Arquivo Remessa"
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
         Left            =   -69600
         TabIndex        =   77
         Top             =   1080
         Width           =   2010
      End
      Begin VB.Label lblDirArquivoRetorno 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Diretório Arquivo Retorno"
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
         Left            =   -74760
         TabIndex        =   76
         Top             =   1680
         Width           =   2280
      End
      Begin VB.Label lblDiretórioArquivo 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Diretório Arquivo Remessa"
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
         Left            =   -74760
         TabIndex        =   72
         Top             =   1080
         Width           =   2280
      End
      Begin VB.Label lblLayoutCNAB 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Layout CNAB"
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
         Left            =   -74760
         TabIndex        =   69
         Top             =   480
         Width           =   1065
      End
      Begin VB.Label lblCodCedente 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Cód Cedente"
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
         Left            =   -67200
         TabIndex        =   66
         Top             =   1080
         Width           =   1065
      End
      Begin VB.Label lblModalidade 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Modalidade"
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
         Left            =   -69720
         TabIndex        =   65
         Top             =   1080
         Width           =   975
      End
      Begin VB.Label lblConvenio 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Convênio"
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
         Left            =   -72240
         TabIndex        =   64
         Top             =   1080
         Width           =   780
      End
      Begin VB.Label lblCodTransmissão 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Cód Transmissão"
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
         Left            =   -74760
         TabIndex        =   61
         Top             =   1080
         Width           =   1440
      End
      Begin VB.Label lblRespEmissão 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Resp. Emissão"
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
         Left            =   -67200
         TabIndex        =   58
         Top             =   480
         Width           =   1200
      End
      Begin VB.Label lblConta 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Conta"
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
         Left            =   -69720
         TabIndex        =   55
         Top             =   480
         Width           =   495
      End
      Begin VB.Label lblAgência 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Agência"
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
         Left            =   -72240
         TabIndex        =   53
         Top             =   480
         Width           =   675
      End
      Begin VB.Label lblBanco 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Banco"
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
         Left            =   -74760
         TabIndex        =   51
         Top             =   480
         Width           =   1200
      End
      Begin VB.Label lblNomeArquivo 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Nome Arquivo PDF"
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
         Left            =   -72120
         TabIndex        =   38
         Top             =   1080
         Width           =   1545
      End
      Begin VB.Label lblCopias 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Nº de Copias"
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
         Left            =   -74880
         TabIndex        =   34
         Top             =   1080
         Width           =   1050
      End
      Begin VB.Label lblDiretórioLogotipo 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Diretório Logotipo"
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
         Left            =   -70080
         TabIndex        =   32
         Top             =   480
         Width           =   1530
      End
      Begin VB.Label lblModelo 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Modelo"
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
         Left            =   -72120
         TabIndex        =   30
         Top             =   480
         Width           =   720
      End
      Begin VB.Label lblImpressora 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Impressora"
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
         Left            =   -74880
         TabIndex        =   28
         Top             =   480
         Width           =   1200
      End
      Begin VB.Label lblCarteira 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Carteira"
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
         Left            =   4680
         TabIndex        =   26
         Top             =   2280
         Width           =   1080
      End
      Begin VB.Label lblTelefone 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Telefone"
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
         TabIndex        =   24
         Top             =   2280
         Width           =   750
      End
      Begin VB.Label lblDocumento 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Documento"
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
         Top             =   2280
         Width           =   1080
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
         Left            =   4440
         TabIndex        =   20
         Top             =   1680
         Width           =   1365
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
         Left            =   7800
         TabIndex        =   17
         Top             =   1680
         Width           =   600
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
         Left            =   8760
         TabIndex        =   16
         Top             =   1680
         Width           =   510
      End
      Begin VB.Label lblComplemento 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Complemento"
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
         TabIndex        =   14
         Top             =   1680
         Width           =   1365
      End
      Begin VB.Label lblNumero 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Número"
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
         Left            =   6600
         TabIndex        =   12
         Top             =   1080
         Width           =   660
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
         Left            =   7800
         TabIndex        =   10
         Top             =   1080
         Width           =   510
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
         Left            =   120
         TabIndex        =   7
         Top             =   1080
         Width           =   1365
      End
      Begin VB.Label lblCNPJCPF 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "CNPJ/CPF"
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
         Left            =   7800
         TabIndex        =   6
         Top             =   480
         Width           =   795
      End
      Begin VB.Label lblNomeRz 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Nome/ Rz Social"
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
         Left            =   1920
         TabIndex        =   3
         Top             =   480
         Width           =   1365
      End
      Begin VB.Label lblPessoa 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Pessoa"
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
         TabIndex        =   1
         Top             =   480
         Width           =   600
      End
   End
End
Attribute VB_Name = "FrmMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim boleto As ACBrBoleto

Private Sub btnDirLogo_Click()
    txtDirLogo.Text = BrowseFolder("Selecione o diretorio dos logos")
End Sub

Private Sub btnDirRemessa_Click()
    txtDirRemessa.Text = BrowseFolder("Selecione o diretorio dos arquivos de remessa")
End Sub

Private Sub btnDirRetorno_Click()
    txtDirRetorno.Text = BrowseFolder("Selecione o diretorio dos arquivos de retorno")
End Sub

Private Sub btnGerarRemessa_Click()
    boleto.GerarRemessa txtDirRemessa.Text, 1, txtNomeRemessa.Text
End Sub

Private Sub btnGravarConfig_Click()
    SaveConfig
End Sub

Private Sub btnImprimir_Click()
    boleto.Imprimir
End Sub

Private Sub btnIncluirTitulo_Click()
     On Error GoTo Erro:
    
    CommonDialog1.DialogTitle = "Selecione o arquivo ini"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Titulo (*.ini)|*.ini|Todo os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
            
    If CommonDialog1.FileName = vbNullString Then Exit Sub
    SetResposta boleto.IncluirTitulos(CommonDialog1.FileName)
Erro:
    MsgBox Err.Description
End Sub

Private Sub cmdLimparLista_Click()
    boleto.LimparLista
End Sub

Private Sub cmdTotalTítulos_Click()
    SetResposta boleto.TotalTitulosLista()
End Sub

Private Sub Form_Load()
    Dim p As Printer
    For Each p In Printers
        cmbImpressora.AddItem p.DeviceName
    Next
    
    Dim LogPath As String
    Dim IniPath As String
    
    LogPath = App.Path & "\Logs\"
    IniPath = App.Path & "\ACBrLib.ini"
    
    If Not DirExists(LogPath) Then
        MkDir LogPath
    End If
    
    Set boleto = CreateBoleto(IniPath)
    
    boleto.ConfigGravarValor SESSAO_PRINCIPAL, "LogNivel", "4"
    boleto.ConfigGravarValor SESSAO_PRINCIPAL, "LogPath", LogPath
    boleto.ConfigGravar
    
    LoadConfig
End Sub

Private Sub SetResposta(ByRef resposta As String)
    If rtbRespostas.Text <> vbNullString Then
      rtbRespostas.Text = rtbRespostas.Text + vbCrLf + resposta
    Else
      rtbRespostas.Text = resposta
    End If
End Sub

Private Sub LoadConfig()
    boleto.ConfigLer
    
    cmbModeloImpressao.ListIndex = CLng(boleto.ConfigLerValor(SESSAO_BOLETOBANCOFCFORTESCONFIG, "Layout"))
    chkPreview.value = CLng(boleto.ConfigLerValor(SESSAO_BOLETOBANCOFCFORTESCONFIG, "MostrarPreview"))
    chkProgresso.value = CLng(boleto.ConfigLerValor(SESSAO_BOLETOBANCOFCFORTESCONFIG, "MostrarProgresso"))
    chkSetup.value = CLng(boleto.ConfigLerValor(SESSAO_BOLETOBANCOFCFORTESCONFIG, "MostrarSetup"))
    txtNomeArquivo.Text = boleto.ConfigLerValor(SESSAO_BOLETOBANCOFCFORTESCONFIG, "NomeArquivo")
    nudCopias.value = CLng(boleto.ConfigLerValor(SESSAO_BOLETOBANCOFCFORTESCONFIG, "NumeroCopias"))
    cmbImpressora.Text = boleto.ConfigLerValor(SESSAO_BOLETOBANCOFCFORTESCONFIG, "PrinterName")
    txtDirLogo.Text = boleto.ConfigLerValor(SESSAO_BOLETOBANCOFCFORTESCONFIG, "DirLogo")
    cmbBanco.ListIndex = CLng(boleto.ConfigLerValor(SESSAO_BOLETOBANCOCONFIG, "TipoCobranca"))
    txtAgencia.Text = boleto.ConfigLerValor(SESSAO_BOLETOCEDENTECONFIG, "Agencia")
    txtDigAgencia.Text = boleto.ConfigLerValor(SESSAO_BOLETOCEDENTECONFIG, "AgenciaDigito")
    txtConta.Text = boleto.ConfigLerValor(SESSAO_BOLETOCEDENTECONFIG, "Conta")
    txtDigConta.Text = boleto.ConfigLerValor(SESSAO_BOLETOCEDENTECONFIG, "ContaDigito")
    txtCodTransmissao.Text = boleto.ConfigLerValor(SESSAO_BOLETOCEDENTECONFIG, "CodigoTransmissao")
    txtConvenio.Text = boleto.ConfigLerValor(SESSAO_BOLETOCEDENTECONFIG, "Convenio")
    txtModalidade.Text = boleto.ConfigLerValor(SESSAO_BOLETOCEDENTECONFIG, "Modalidade")
    txtCodCedente.Text = boleto.ConfigLerValor(SESSAO_BOLETOCEDENTECONFIG, "CodigoCedente")
    cmbRespEmissao.ListIndex = CLng(boleto.ConfigLerValor(SESSAO_BOLETOCEDENTECONFIG, "ResponEmissao"))
    txtBairro.Text = boleto.ConfigLerValor(SESSAO_BOLETOCEDENTECONFIG, "Bairro")
    txtCEP.Text = boleto.ConfigLerValor(SESSAO_BOLETOCEDENTECONFIG, "CEP")
    txtCidade.Text = boleto.ConfigLerValor(SESSAO_BOLETOCEDENTECONFIG, "Cidade")
    txtCNPJCPF.Text = boleto.ConfigLerValor(SESSAO_BOLETOCEDENTECONFIG, "CNPJCPF")
    txtComplemento.Text = boleto.ConfigLerValor(SESSAO_BOLETOCEDENTECONFIG, "Complemento")
    txtLogradouro.Text = boleto.ConfigLerValor(SESSAO_BOLETOCEDENTECONFIG, "Logradouro")
    txtNomeRes.Text = boleto.ConfigLerValor(SESSAO_BOLETOCEDENTECONFIG, "Nome")
    txtNumeroRes.Text = boleto.ConfigLerValor(SESSAO_BOLETOCEDENTECONFIG, "NumeroRes")
    txtTelefone.Text = boleto.ConfigLerValor(SESSAO_BOLETOCEDENTECONFIG, "Telefone")
    cmbUF.Text = boleto.ConfigLerValor(SESSAO_BOLETOCEDENTECONFIG, "UF")
    cmbTipoCarteira.ListIndex = CLng(boleto.ConfigLerValor(SESSAO_BOLETOCEDENTECONFIG, "TipoCarteira"))
    cmbTipoDocumento.ListIndex = CLng(boleto.ConfigLerValor(SESSAO_BOLETOCEDENTECONFIG, "TipoDocumento"))
    cmbTipoInscricao.ListIndex = CLng(boleto.ConfigLerValor(SESSAO_BOLETOCEDENTECONFIG, "TipoInscricao"))
    txtDirRemessa.Text = boleto.ConfigLerValor(SESSAO_BOLETODIRETORIOCONFIG, "DirArqRemessa")
    txtDirRetorno.Text = boleto.ConfigLerValor(SESSAO_BOLETODIRETORIOCONFIG, "DirArqRetorno")
    cmbLayoutCNAB.ListIndex = CLng(boleto.ConfigLerValor(SESSAO_BOLETODIRETORIOCONFIG, "LayoutRemessa"))
    txtNomeRemessa.Text = boleto.ConfigLerValor(SESSAO_BOLETODIRETORIOCONFIG, "NomeArqRemessa")
    txtNomeRetorno.Text = boleto.ConfigLerValor(SESSAO_BOLETODIRETORIOCONFIG, "NomeArqRetorno")
    ckbCedenteRetorno.value = CLng(boleto.ConfigLerValor(SESSAO_BOLETODIRETORIOCONFIG, "LeCedenteRetorno"))
    txtNome.Text = boleto.ConfigLerValor(SESSAO_EMAIL, "Nome")
    txtEmail.Text = boleto.ConfigLerValor(SESSAO_EMAIL, "Conta")
    txtUsuario.Text = boleto.ConfigLerValor(SESSAO_EMAIL, "Usuario")
    txtSenha.Text = boleto.ConfigLerValor(SESSAO_EMAIL, "Senha")
    txtHost.Text = boleto.ConfigLerValor(SESSAO_EMAIL, "Servidor")
    nudPorta.value = CLng(boleto.ConfigLerValor(SESSAO_EMAIL, "Porta"))
    chkSSL.value = CLng(boleto.ConfigLerValor(SESSAO_EMAIL, "SSL"))
    chkTLS.value = CLng(boleto.ConfigLerValor(SESSAO_EMAIL, "TLS"))
End Sub

Private Sub SaveConfig()
    boleto.ConfigGravarValor SESSAO_BOLETOBANCOFCFORTESCONFIG, "Layout", CStr(cmbModeloImpressao.ListIndex)
    boleto.ConfigGravarValor SESSAO_BOLETOBANCOFCFORTESCONFIG, "MostrarPreview", CStr(chkPreview.value)
    boleto.ConfigGravarValor SESSAO_BOLETOBANCOFCFORTESCONFIG, "MostrarProgresso", CStr(chkProgresso.value)
    boleto.ConfigGravarValor SESSAO_BOLETOBANCOFCFORTESCONFIG, "MostrarSetup", CStr(chkSetup.value)
    boleto.ConfigGravarValor SESSAO_BOLETOBANCOFCFORTESCONFIG, "NomeArquivo", txtNomeArquivo.Text
    boleto.ConfigGravarValor SESSAO_BOLETOBANCOFCFORTESCONFIG, "NumeroCopias", txtCopias.Text
    boleto.ConfigGravarValor SESSAO_BOLETOBANCOFCFORTESCONFIG, "PrinterName", cmbImpressora.Text
    boleto.ConfigGravarValor SESSAO_BOLETOBANCOFCFORTESCONFIG, "DirLogo", txtDirLogo.Text
    boleto.ConfigGravarValor SESSAO_BOLETOBANCOCONFIG, "TipoCobranca", CStr(cmbBanco.ListIndex)
    boleto.ConfigGravarValor SESSAO_BOLETOCEDENTECONFIG, "Agencia", txtAgencia.Text
    boleto.ConfigGravarValor SESSAO_BOLETOCEDENTECONFIG, "AgenciaDigito", txtDigAgencia.Text
    boleto.ConfigGravarValor SESSAO_BOLETOCEDENTECONFIG, "Conta", txtConta.Text
    boleto.ConfigGravarValor SESSAO_BOLETOCEDENTECONFIG, "ContaDigito", txtDigConta.Text
    boleto.ConfigGravarValor SESSAO_BOLETOCEDENTECONFIG, "CodigoTransmissao", txtCodTransmissao.Text
    boleto.ConfigGravarValor SESSAO_BOLETOCEDENTECONFIG, "Convenio", txtConvenio.Text
    boleto.ConfigGravarValor SESSAO_BOLETOCEDENTECONFIG, "Modalidade", txtModalidade.Text
    boleto.ConfigGravarValor SESSAO_BOLETOCEDENTECONFIG, "CodigoCedente", txtCodCedente.Text
    boleto.ConfigGravarValor SESSAO_BOLETOCEDENTECONFIG, "ResponEmissao", CStr(cmbRespEmissao.ListIndex)
    boleto.ConfigGravarValor SESSAO_BOLETOCEDENTECONFIG, "Bairro", txtBairro.Text
    boleto.ConfigGravarValor SESSAO_BOLETOCEDENTECONFIG, "CEP", txtCEP.Text
    boleto.ConfigGravarValor SESSAO_BOLETOCEDENTECONFIG, "Cidade", txtCidade.Text
    boleto.ConfigGravarValor SESSAO_BOLETOCEDENTECONFIG, "CNPJCPF", txtCNPJCPF.Text
    boleto.ConfigGravarValor SESSAO_BOLETOCEDENTECONFIG, "Complemento", txtComplemento.Text
    boleto.ConfigGravarValor SESSAO_BOLETOCEDENTECONFIG, "Logradouro", txtLogradouro.Text
    boleto.ConfigGravarValor SESSAO_BOLETOCEDENTECONFIG, "Nome", txtNomeRes.Text
    boleto.ConfigGravarValor SESSAO_BOLETOCEDENTECONFIG, "NumeroRes", txtNumeroRes.Text
    boleto.ConfigGravarValor SESSAO_BOLETOCEDENTECONFIG, "Telefone", txtTelefone.Text
    boleto.ConfigGravarValor SESSAO_BOLETOCEDENTECONFIG, "UF", cmbUF.Text
    boleto.ConfigGravarValor SESSAO_BOLETOCEDENTECONFIG, "TipoCarteira", CStr(cmbTipoCarteira.ListIndex)
    boleto.ConfigGravarValor SESSAO_BOLETOCEDENTECONFIG, "TipoDocumento", CStr(cmbTipoDocumento.ListIndex)
    boleto.ConfigGravarValor SESSAO_BOLETOCEDENTECONFIG, "TipoInscricao", CStr(cmbTipoInscricao.ListIndex)
    boleto.ConfigGravarValor SESSAO_BOLETODIRETORIOCONFIG, "DirArqRemessa", txtDirRemessa.Text
    boleto.ConfigGravarValor SESSAO_BOLETODIRETORIOCONFIG, "DirArqRetorno", txtDirRetorno.Text
    boleto.ConfigGravarValor SESSAO_BOLETODIRETORIOCONFIG, "LayoutRemessa", CStr(cmbLayoutCNAB.ListIndex)
    boleto.ConfigGravarValor SESSAO_BOLETODIRETORIOCONFIG, "NomeArqRemessa", txtNomeRemessa.Text
    boleto.ConfigGravarValor SESSAO_BOLETODIRETORIOCONFIG, "NomeArqRetorno", txtNomeRetorno.Text
    boleto.ConfigGravarValor SESSAO_BOLETODIRETORIOCONFIG, "LeCedenteRetorno", CStr(ckbCedenteRetorno.value)
    boleto.ConfigGravarValor SESSAO_EMAIL, "Nome", txtNome.Text
    boleto.ConfigGravarValor SESSAO_EMAIL, "Conta", txtEmail.Text
    boleto.ConfigGravarValor SESSAO_EMAIL, "Usuario", txtUsuario.Text
    boleto.ConfigGravarValor SESSAO_EMAIL, "Senha", txtSenha.Text
    boleto.ConfigGravarValor SESSAO_EMAIL, "Servidor", txtHost.Text
    boleto.ConfigGravarValor SESSAO_EMAIL, "Porta", CStr(nudPorta.value)
    boleto.ConfigGravarValor SESSAO_EMAIL, "SSL", CStr(chkSSL.value)
    boleto.ConfigGravarValor SESSAO_EMAIL, "TLS", CStr(chkTLS.value)
    
    boleto.ConfigGravar
    
End Sub

