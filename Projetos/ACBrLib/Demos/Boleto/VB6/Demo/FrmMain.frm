VERSION 5.00
Object = "{86CF1D34-0C5F-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCT2.OCX"
Object = "{BDC217C8-ED16-11CD-956C-0000C04E4C0A}#1.1#0"; "TABCTL32.OCX"
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "COMDLG32.OCX"
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
      Left            =   2760
      Top             =   3240
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
      Tab(2).Control(0)=   "txtConvenio"
      Tab(2).Control(1)=   "txtCodCedente"
      Tab(2).Control(2)=   "txtModalidade"
      Tab(2).Control(3)=   "txtCodTransmissao"
      Tab(2).Control(4)=   "txtDigAgencia"
      Tab(2).Control(5)=   "cmbRespEmissao"
      Tab(2).Control(6)=   "txtDigConta"
      Tab(2).Control(7)=   "txtConta"
      Tab(2).Control(8)=   "txtAgencia"
      Tab(2).Control(9)=   "cmbBanco"
      Tab(2).Control(10)=   "lblCodCedente"
      Tab(2).Control(11)=   "lblModalidade"
      Tab(2).Control(12)=   "lblConvenio"
      Tab(2).Control(13)=   "lblCodTransmissão"
      Tab(2).Control(14)=   "lblRespEmissão"
      Tab(2).Control(15)=   "lblConta"
      Tab(2).Control(16)=   "lblAgência"
      Tab(2).Control(17)=   "lblBanco"
      Tab(2).ControlCount=   18
      TabCaption(3)   =   "Remessa/Retorno"
      TabPicture(3)   =   "FrmMain.frx":0054
      Tab(3).ControlEnabled=   0   'False
      Tab(3).Control(0)=   "txtNomeRetorno"
      Tab(3).Control(1)=   "txtNomeRemessa"
      Tab(3).Control(2)=   "txtDirRetorno"
      Tab(3).Control(3)=   "btnDirRetorno"
      Tab(3).Control(4)=   "ckbCedenteRetorno"
      Tab(3).Control(5)=   "txtDirRemessa"
      Tab(3).Control(6)=   "btnDirRemessa"
      Tab(3).Control(7)=   "cmbLayoutCNAB"
      Tab(3).Control(8)=   "lblNomeArquivoRetorno"
      Tab(3).Control(9)=   "lblNomeArquivoRemessa"
      Tab(3).Control(10)=   "lblDirArquivoRetorno"
      Tab(3).Control(11)=   "lblDiretórioArquivo"
      Tab(3).Control(12)=   "lblLayoutCNAB"
      Tab(3).ControlCount=   13
      TabCaption(4)   =   "Email"
      TabPicture(4)   =   "FrmMain.frx":0070
      Tab(4).ControlEnabled=   0   'False
      Tab(4).Control(0)=   "txtUsuario"
      Tab(4).Control(1)=   "txtNome"
      Tab(4).Control(2)=   "txtEmail"
      Tab(4).Control(3)=   "txtSenha"
      Tab(4).Control(4)=   "txtHost"
      Tab(4).Control(5)=   "txt"
      Tab(4).Control(6)=   "chkSSL"
      Tab(4).Control(7)=   "chkTLS"
      Tab(4).Control(8)=   "nudPorta"
      Tab(4).Control(9)=   "lblNome"
      Tab(4).Control(10)=   "lblEmail"
      Tab(4).Control(11)=   "lblUsuário"
      Tab(4).Control(12)=   "lblSenha"
      Tab(4).Control(13)=   "lblHostSMTP"
      Tab(4).Control(14)=   "lblPorta"
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
    Dim retorno As Long
    
    retorno = Boleto_GerarRemessa(txtDirRemessa.Text, 1, txtNomeRemessa.Text)
    CheckResult retorno
End Sub

Private Sub btnGravarConfig_Click()
    SaveConfig
End Sub

Private Sub btnImprimir_Click()
    Dim retorno As Long
    
    retorno = Boleto_Imprimir("")
    CheckResult retorno
End Sub

Private Sub btnIncluirTitulo_Click()
     On Error GoTo Erro:
    
    CommonDialog1.DialogTitle = "Selecione o arquivo ini"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Titulo (*.ini)|*.ini|Todo os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
            
    If CommonDialog1.FileName = vbNullString Then Exit Sub
    
    Dim retorno As Long
    Dim buffer As String
    Dim bufferLen As Long
    
    bufferLen = 256
    buffer = String$(bufferLen, " ")
    retorno = Boleto_IncluirTitulos(CommonDialog1.FileName, "", buffer, bufferLen)
    CheckResult retorno
    SetResposta buffer, bufferLen
    
Erro:
    MsgBox Err.Description
End Sub

Private Sub cmdLimparLista_Click()
    Dim retorno As Long
    
    retorno = Boleto_LimparLista()
    CheckResult retorno
End Sub

Private Sub cmdTotalTítulos_Click()
    Dim retorno As Long
    Dim buffer As String
    Dim bufferLen As Long
    
    retorno = Boleto_TotalTitulosLista(buffer, bufferLen)
    CheckResult retorno
    
    SetResposta buffer, bufferLen
End Sub

Private Sub Form_Load()
    Dim p As Printer
    For Each p In Printers
        cmbImpressora.AddItem p.DeviceName
    Next
    
    Dim LogPath As String
    Dim IniPath As String
    Dim retorno As Long
    
    LogPath = App.Path & "\Logs\"
    IniPath = App.Path & "\ACBrLib.ini"
    
    If Not DirExists(LogPath) Then
        MkDir LogPath
    End If
    
    retorno = Boleto_Inicializar(IniPath, "")
    CheckResult retorno
    
    retorno = Boleto_ConfigGravarValor("Principal", "LogNivel", "4")
    CheckResult retorno
    
    retorno = Boleto_ConfigGravarValor("Principal", "LogPath", LogPath)
    CheckResult retorno
    
    retorno = Boleto_ConfigGravar("")
    CheckResult retorno
    
    LoadConfig
End Sub

Private Sub SetResposta(ByRef buffer As String, ByRef bufferLen As Long)
    If bufferLen > 256 Then
        buffer = String$(bufferLen, " ")
        Boleto_UltimoRetorno buffer, bufferLen
    End If
    
    If rtbRespostas.Text <> vbNullString Then
      rtbRespostas.Text = rtbRespostas.Text + vbCrLf + Trim$(FromUTF8(buffer))
    Else
      rtbRespostas.Text = Trim$(FromUTF8(buffer))
    End If
End Sub

Private Sub LoadConfig()
    Dim retorno As Long
    Dim buffer As String
    Dim bufferLen As Long
    
    retorno = Boleto_ConfigLer("")
    CheckResult retornoorno
    
    bufferLen = 256
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_ConfigLerValor("BoletoBancoFCFortesConfig", "Layout", buffer, bufferLen)
    CheckResult retorno
    
    cmbModeloImpressao.ListIndex = CLng(Trim$(FromUTF8(buffer)))
    
    bufferLen = 256
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_ConfigLerValor("BoletoBancoFCFortesConfig", "MostrarPreview", buffer, bufferLen)
    CheckResult retorno
    
    chkPreview.Value = CLng(Trim$(FromUTF8(buffer)))
    
    bufferLen = 256
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_ConfigLerValor("BoletoBancoFCFortesConfig", "MostrarProgresso", buffer, bufferLen)
    CheckResult retorno
    
    chkProgresso.Value = CLng(Trim$(FromUTF8(buffer)))
    
    bufferLen = 256
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_ConfigLerValor("BoletoBancoFCFortesConfig", "MostrarSetup", buffer, bufferLen)
    CheckResult retorno
    
    chkSetup.Value = CLng(Trim$(FromUTF8(buffer)))
    
    bufferLen = 256
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_ConfigLerValor("BoletoBancoFCFortesConfig", "NomeArquivo", buffer, bufferLen)
    CheckResult retorno
    
    txtNomeArquivo.Text = Trim$(FromUTF8(buffer))
    
    bufferLen = 256
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_ConfigLerValor("BoletoBancoFCFortesConfig", "NumeroCopias", buffer, bufferLen)
    CheckResult retorno
    
    nudCopias.Value = CLng(Trim$(FromUTF8(buffer)))
    
    bufferLen = 256
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_ConfigLerValor("BoletoBancoFCFortesConfig", "PrinterName", buffer, bufferLen)
    CheckResult retorno
    
    cmbImpressora.Text = Trim$(FromUTF8(buffer))
    
    bufferLen = 256
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_ConfigLerValor("BoletoBancoFCFortesConfig", "DirLogo", buffer, bufferLen)
    CheckResult retorno
    
    txtDirLogo.Text = Trim$(FromUTF8(buffer))
    
    bufferLen = 256
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_ConfigLerValor("BoletoBancoConfig", "TipoCobranca", buffer, bufferLen)
    CheckResult retorno
    
    cmbBanco.ListIndex = CLng(Trim$(FromUTF8(buffer)))
    
    bufferLen = 256
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_ConfigLerValor("BoletoCedenteConfig", "Agencia", buffer, bufferLen)
    CheckResult retorno
    
    txtAgencia.Text = Trim$(FromUTF8(buffer))
    
    bufferLen = 256
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_ConfigLerValor("BoletoCedenteConfig", "AgenciaDigito", buffer, bufferLen)
    CheckResult retorno
    
    txtDigAgencia.Text = Trim$(FromUTF8(buffer))
    
    bufferLen = 256
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_ConfigLerValor("BoletoCedenteConfig", "Conta", buffer, bufferLen)
    CheckResult retorno
    
    txtConta.Text = Trim$(FromUTF8(buffer))
    
    bufferLen = 256
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_ConfigLerValor("BoletoCedenteConfig", "ContaDigito", buffer, bufferLen)
    CheckResult retorno
    
    txtDigConta.Text = Trim$(FromUTF8(buffer))
    
    bufferLen = 256
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_ConfigLerValor("BoletoCedenteConfig", "CodigoTransmissao", buffer, bufferLen)
    CheckResult retorno
    
    txtCodTransmissao.Text = Trim$(FromUTF8(buffer))
    
    bufferLen = 256
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_ConfigLerValor("BoletoCedenteConfig", "Convenio", buffer, bufferLen)
    CheckResult retorno
    
    txtConvenio.Text = Trim$(FromUTF8(buffer))
    
    bufferLen = 256
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_ConfigLerValor("BoletoCedenteConfig", "Modalidade", buffer, bufferLen)
    CheckResult retorno
    
    txtModalidade.Text = Trim$(FromUTF8(buffer))
    
    bufferLen = 256
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_ConfigLerValor("BoletoCedenteConfig", "CodigoCedente", buffer, bufferLen)
    CheckResult retorno
    
    txtCodCedente.Text = Trim$(FromUTF8(buffer))
    
    bufferLen = 256
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_ConfigLerValor("BoletoCedenteConfig", "ResponEmissao", buffer, bufferLen)
    CheckResult retorno
    
    cmbRespEmissao.ListIndex = CLng(Trim$(FromUTF8(buffer)))
    
    bufferLen = 256
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_ConfigLerValor("BoletoCedenteConfig", "Bairro", buffer, bufferLen)
    CheckResult retorno
    
    txtBairro.Text = Trim$(FromUTF8(buffer))
    
    bufferLen = 256
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_ConfigLerValor("BoletoCedenteConfig", "CEP", buffer, bufferLen)
    CheckResult retorno
    
    txtCEP.Text = Trim$(FromUTF8(buffer))
    
    bufferLen = 256
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_ConfigLerValor("BoletoCedenteConfig", "Cidade", buffer, bufferLen)
    CheckResult retorno
    
    txtCidade.Text = Trim$(FromUTF8(buffer))
    
    bufferLen = 256
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_ConfigLerValor("BoletoCedenteConfig", "CNPJCPF", buffer, bufferLen)
    CheckResult retorno
    
    txtCNPJCPF.Text = Trim$(FromUTF8(buffer))
    
    bufferLen = 256
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_ConfigLerValor("BoletoCedenteConfig", "Complemento", buffer, bufferLen)
    CheckResult retorno
    
    txtComplemento.Text = Trim$(FromUTF8(buffer))
    
    bufferLen = 256
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_ConfigLerValor("BoletoCedenteConfig", "Logradouro", buffer, bufferLen)
    CheckResult retorno
    
    txtLogradouro.Text = Trim$(FromUTF8(buffer))
    
    bufferLen = 256
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_ConfigLerValor("BoletoCedenteConfig", "Nome", buffer, bufferLen)
    CheckResult retorno
    
    txtNomeRes.Text = Trim$(FromUTF8(buffer))
    
    bufferLen = 256
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_ConfigLerValor("BoletoCedenteConfig", "NumeroRes", buffer, bufferLen)
    CheckResult retorno
    
    txtNumeroRes.Text = Trim$(FromUTF8(buffer))
    
    bufferLen = 256
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_ConfigLerValor("BoletoCedenteConfig", "Telefone", buffer, bufferLen)
    CheckResult retorno
    
    txtTelefone.Text = Trim$(FromUTF8(buffer))
    
    bufferLen = 256
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_ConfigLerValor("BoletoCedenteConfig", "UF", buffer, bufferLen)
    CheckResult retorno
    
    cmbUF.Text = Trim$(FromUTF8(buffer))
    
    bufferLen = 256
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_ConfigLerValor("BoletoCedenteConfig", "TipoCarteira", buffer, bufferLen)
    CheckResult retorno
    
    cmbTipoCarteira.ListIndex = CLng(Trim$(FromUTF8(buffer)))
    
    bufferLen = 256
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_ConfigLerValor("BoletoCedenteConfig", "TipoDocumento", buffer, bufferLen)
    CheckResult retorno
    
    cmbTipoDocumento.ListIndex = CLng(Trim$(FromUTF8(buffer)))
    
    bufferLen = 256
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_ConfigLerValor("BoletoCedenteConfig", "TipoInscricao", buffer, bufferLen)
    CheckResult retorno
    
    cmbTipoInscricao.ListIndex = CLng(Trim$(FromUTF8(buffer)))
    
    bufferLen = 256
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_ConfigLerValor("BoletoDiretorioConfig", "DirArqRemessa", buffer, bufferLen)
    CheckResult retorno
    
    txtDirRemessa.Text = Trim$(FromUTF8(buffer))
    
    bufferLen = 256
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_ConfigLerValor("BoletoDiretorioConfig", "DirArqRetorno", buffer, bufferLen)
    CheckResult retorno
    
    txtDirRetorno.Text = Trim$(FromUTF8(buffer))
    
    bufferLen = 256
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_ConfigLerValor("BoletoDiretorioConfig", "LayoutRemessa", buffer, bufferLen)
    CheckResult retorno
    
    cmbLayoutCNAB.ListIndex = CLng(Trim$(FromUTF8(buffer)))
    
    bufferLen = 256
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_ConfigLerValor("BoletoDiretorioConfig", "NomeArqRemessa", buffer, bufferLen)
    CheckResult retorno
    
    txtNomeRemessa.Text = Trim$(FromUTF8(buffer))
    
    bufferLen = 256
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_ConfigLerValor("BoletoDiretorioConfig", "NomeArqRetorno", buffer, bufferLen)
    CheckResult retorno
    
    txtNomeRetorno.Text = Trim$(FromUTF8(buffer))
    
    bufferLen = 256
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_ConfigLerValor("BoletoDiretorioConfig", "LeCedenteRetorno", buffer, bufferLen)
    CheckResult retorno
    
    ckbCedenteRetorno.Value = CLng(Trim$(FromUTF8(buffer)))
    
    bufferLen = 256
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_ConfigLerValor("Email", "Nome", buffer, bufferLen)
    CheckResult retorno
    
    txtNome.Text = Trim$(FromUTF8(buffer))
    
    bufferLen = 256
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_ConfigLerValor("Email", "Conta", buffer, bufferLen)
    CheckResult retorno
    
    txtEmail.Text = Trim$(FromUTF8(buffer))
    
    bufferLen = 256
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_ConfigLerValor("Email", "Usuario", buffer, bufferLen)
    CheckResult retorno
    
    txtUsuario.Text = Trim$(FromUTF8(buffer))
    
    bufferLen = 256
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_ConfigLerValor("Email", "Senha", buffer, bufferLen)
    CheckResult retorno
    
    txtSenha.Text = Trim$(FromUTF8(buffer))
    
    bufferLen = 256
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_ConfigLerValor("Email", "Servidor", buffer, bufferLen)
    CheckResult retorno
    
    txtHost.Text = Trim$(FromUTF8(buffer))
    
    bufferLen = 256
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_ConfigLerValor("Email", "Porta", buffer, bufferLen)
    CheckResult retorno
    
    nudPorta.Value = CLng(Trim$(FromUTF8(buffer)))
    
    bufferLen = 256
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_ConfigLerValor("Email", "SSL", buffer, bufferLen)
    CheckResult retorno
    
    chkSSL.Value = CLng(Trim$(FromUTF8(buffer)))
    
    bufferLen = 256
    buffer = String$(bufferLen, " ")
    
    retorno = Boleto_ConfigLerValor("Email", "TLS", buffer, bufferLen)
    CheckResult retorno
    
    chkTLS.Value = CLng(Trim$(FromUTF8(buffer)))
End Sub

Private Sub SaveConfig()
    Dim retorno As Long
    
    retorno = Boleto_ConfigGravarValor("BoletoBancoFCFortesConfig", "Layout", CStr(cmbModeloImpressao.ListIndex))
    CheckResult retorno
    
    retorno = Boleto_ConfigGravarValor("BoletoBancoFCFortesConfig", "MostrarPreview", CStr(chkPreview.Value))
    CheckResult retorno
    
    retorno = Boleto_ConfigGravarValor("BoletoBancoFCFortesConfig", "MostrarProgresso", CStr(chkProgresso.Value))
    CheckResult retorno
    
    retorno = Boleto_ConfigGravarValor("BoletoBancoFCFortesConfig", "MostrarSetup", CStr(chkSetup.Value))
    CheckResult retorno
    
    retorno = Boleto_ConfigGravarValor("BoletoBancoFCFortesConfig", "NomeArquivo", txtNomeArquivo.Text)
    CheckResult retorno
    
    retorno = Boleto_ConfigGravarValor("BoletoBancoFCFortesConfig", "NumeroCopias", txtCopias.Text)
    CheckResult retorno
    
    retorno = Boleto_ConfigGravarValor("BoletoBancoFCFortesConfig", "PrinterName", cmbImpressora.Text)
    CheckResult retorno
    
    retorno = Boleto_ConfigGravarValor("BoletoBancoFCFortesConfig", "DirLogo", txtDirLogo.Text)
    CheckResult retorno
    
    retorno = Boleto_ConfigGravarValor("BoletoBancoConfig", "TipoCobranca", CStr(cmbBanco.ListIndex))
    CheckResult retorno
    
    retorno = Boleto_ConfigGravarValor("BoletoCedenteConfig", "Agencia", txtAgencia.Text)
    CheckResult retorno
    
    retorno = Boleto_ConfigGravarValor("BoletoCedenteConfig", "AgenciaDigito", txtDigAgencia.Text)
    CheckResult retorno
    
    retorno = Boleto_ConfigGravarValor("BoletoCedenteConfig", "Conta", txtConta.Text)
    CheckResult retorno
    
    retorno = Boleto_ConfigGravarValor("BoletoCedenteConfig", "ContaDigito", txtDigConta.Text)
    CheckResult retorno
    
    retorno = Boleto_ConfigGravarValor("BoletoCedenteConfig", "CodigoTransmissao", txtCodTransmissao.Text)
    CheckResult retorno
    
    retorno = Boleto_ConfigGravarValor("BoletoCedenteConfig", "Convenio", txtConvenio.Text)
    CheckResult retorno
    
    retorno = Boleto_ConfigGravarValor("BoletoCedenteConfig", "Modalidade", txtModalidade.Text)
    CheckResult retorno
    
    retorno = Boleto_ConfigGravarValor("BoletoCedenteConfig", "CodigoCedente", txtCodCedente.Text)
    CheckResult retorno
    
    retorno = Boleto_ConfigGravarValor("BoletoCedenteConfig", "ResponEmissao", CStr(cmbRespEmissao.ListIndex))
    CheckResult retorno
    
    retorno = Boleto_ConfigGravarValor("BoletoCedenteConfig", "Bairro", txtBairro.Text)
    CheckResult retorno
    
    retorno = Boleto_ConfigGravarValor("BoletoCedenteConfig", "CEP", txtCEP.Text)
    CheckResult retorno
    
    retorno = Boleto_ConfigGravarValor("BoletoCedenteConfig", "Cidade", txtCidade.Text)
    CheckResult retorno
    
    retorno = Boleto_ConfigGravarValor("BoletoCedenteConfig", "CNPJCPF", txtCNPJCPF.Text)
    CheckResult retorno
    
    retorno = Boleto_ConfigGravarValor("BoletoCedenteConfig", "Complemento", txtComplemento.Text)
    CheckResult retorno
    
    retorno = Boleto_ConfigGravarValor("BoletoCedenteConfig", "Logradouro", txtLogradouro.Text)
    CheckResult retorno
    
    retorno = Boleto_ConfigGravarValor("BoletoCedenteConfig", "Nome", txtNomeRes.Text)
    CheckResult retorno
    
    retorno = Boleto_ConfigGravarValor("BoletoCedenteConfig", "NumeroRes", txtNumeroRes.Text)
    CheckResult retorno
    
    retorno = Boleto_ConfigGravarValor("BoletoCedenteConfig", "Telefone", txtTelefone.Text)
    CheckResult retorno
    
    retorno = Boleto_ConfigGravarValor("BoletoCedenteConfig", "UF", cmbUF.Text)
    CheckResult retorno
    
    retorno = Boleto_ConfigGravarValor("BoletoCedenteConfig", "TipoCarteira", CStr(cmbTipoCarteira.ListIndex))
    CheckResult retorno
    
    retorno = Boleto_ConfigGravarValor("BoletoCedenteConfig", "TipoDocumento", CStr(cmbTipoDocumento.ListIndex))
    CheckResult retorno
    
    retorno = Boleto_ConfigGravarValor("BoletoCedenteConfig", "TipoInscricao", CStr(cmbTipoInscricao.ListIndex))
    CheckResult retorno
    
    retorno = Boleto_ConfigGravarValor("BoletoDiretorioConfig", "DirArqRemessa", txtDirRemessa.Text)
    CheckResult retorno
    
    retorno = Boleto_ConfigGravarValor("BoletoDiretorioConfig", "DirArqRetorno", txtDirRetorno.Text)
    CheckResult retorno
    
    retorno = Boleto_ConfigGravarValor("BoletoDiretorioConfig", "LayoutRemessa", CStr(cmbLayoutCNAB.ListIndex))
    CheckResult retorno
    
    retorno = Boleto_ConfigGravarValor("BoletoDiretorioConfig", "NomeArqRemessa", txtNomeRemessa.Text)
    CheckResult retorno
    
    retorno = Boleto_ConfigGravarValor("BoletoDiretorioConfig", "NomeArqRetorno", txtNomeRetorno.Text)
    CheckResult retorno
    
    retorno = Boleto_ConfigGravarValor("BoletoDiretorioConfig", "LeCedenteRetorno", CStr(ckbCedenteRetorno.Value))
    CheckResult retorno
    
    retorno = Boleto_ConfigGravarValor("Email", "Nome", txtNome.Text)
    CheckResult retorno
    
    retorno = Boleto_ConfigGravarValor("Email", "Conta", txtEmail.Text)
    CheckResult retorno
    
    retorno = Boleto_ConfigGravarValor("Email", "Usuario", txtUsuario.Text)
    CheckResult retorno
    
    retorno = Boleto_ConfigGravarValor("Email", "Senha", txtSenha.Text)
    CheckResult retorno
    
    retorno = Boleto_ConfigGravarValor("Email", "Servidor", txtHost.Text)
    CheckResult retorno
    
    retorno = Boleto_ConfigGravarValor("Email", "Porta", CStr(nudPorta.Value))
    CheckResult retorno
    
    retorno = Boleto_ConfigGravarValor("Email", "SSL", CStr(chkSSL.Value))
    CheckResult retorno
    
    retorno = Boleto_ConfigGravarValor("Email", "TLS", CStr(chkTLS.Value))
    CheckResult retorno
    
    retorno = Boleto_ConfigGravar("")
    CheckResult retorno
End Sub
