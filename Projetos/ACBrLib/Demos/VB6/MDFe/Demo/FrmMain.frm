VERSION 5.00
Object = "{86CF1D34-0C5F-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCT2.OCX"
Object = "{BDC217C8-ED16-11CD-956C-0000C04E4C0A}#1.1#0"; "TABCTL32.OCX"
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "COMDLG32.OCX"
Begin VB.Form FrmMain 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "ACBrLibMDFe Demo"
   ClientHeight    =   7785
   ClientLeft      =   45
   ClientTop       =   390
   ClientWidth     =   11520
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
   ScaleHeight     =   7785
   ScaleWidth      =   11520
   StartUpPosition =   2  'CenterScreen
   Begin VB.CommandButton btnCarregar 
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
      Left            =   120
      TabIndex        =   138
      Top             =   7080
      Width           =   2295
   End
   Begin TabDlg.SSTab SSTab3 
      Height          =   3615
      Left            =   4920
      TabIndex        =   5
      Top             =   120
      Width           =   6495
      _ExtentX        =   11456
      _ExtentY        =   6376
      _Version        =   393216
      Style           =   1
      Tabs            =   4
      TabsPerRow      =   4
      TabHeight       =   520
      TabCaption(0)   =   "Envio"
      TabPicture(0)   =   "FrmMain.frx":0000
      Tab(0).ControlEnabled=   -1  'True
      Tab(0).Control(0)=   "btnGerarXml"
      Tab(0).Control(0).Enabled=   0   'False
      Tab(0).Control(1)=   "btnEnviarSincrono"
      Tab(0).Control(1).Enabled=   0   'False
      Tab(0).Control(2)=   "btnEnviarAssincrono"
      Tab(0).Control(2).Enabled=   0   'False
      Tab(0).Control(3)=   "btnCarregarIni"
      Tab(0).Control(3).Enabled=   0   'False
      Tab(0).Control(4)=   "btnImprimir"
      Tab(0).Control(4).Enabled=   0   'False
      Tab(0).Control(5)=   "btnEnviarEmail"
      Tab(0).Control(5).Enabled=   0   'False
      Tab(0).Control(6)=   "btnAssinar"
      Tab(0).Control(6).Enabled=   0   'False
      Tab(0).Control(7)=   "btnCarregarXml"
      Tab(0).Control(7).Enabled=   0   'False
      Tab(0).Control(8)=   "btnImprimirPDF"
      Tab(0).Control(8).Enabled=   0   'False
      Tab(0).Control(9)=   "btnValidarRegra"
      Tab(0).Control(9).Enabled=   0   'False
      Tab(0).Control(10)=   "btnLimparLista"
      Tab(0).Control(10).Enabled=   0   'False
      Tab(0).Control(11)=   "btnGerarChaveMDFe"
      Tab(0).Control(11).Enabled=   0   'False
      Tab(0).ControlCount=   12
      TabCaption(1)   =   "Consultas"
      TabPicture(1)   =   "FrmMain.frx":001C
      Tab(1).ControlEnabled=   0   'False
      Tab(1).Control(0)=   "btnStatusServ"
      Tab(1).Control(1)=   "btnConsultaXml"
      Tab(1).Control(2)=   "btnConsultaChave"
      Tab(1).Control(3)=   "btnConsultarRecibo"
      Tab(1).Control(4)=   "btnConsNaoEncerrados"
      Tab(1).ControlCount=   5
      TabCaption(2)   =   "Eventos"
      TabPicture(2)   =   "FrmMain.frx":0038
      Tab(2).ControlEnabled=   0   'False
      Tab(2).Control(0)=   "btnCancelar"
      Tab(2).Control(0).Enabled=   0   'False
      Tab(2).Control(1)=   "btnEncerrar"
      Tab(2).Control(1).Enabled=   0   'False
      Tab(2).Control(2)=   "btnEnviarEvento"
      Tab(2).Control(2).Enabled=   0   'False
      Tab(2).Control(3)=   "btnCarregarEvento"
      Tab(2).Control(3).Enabled=   0   'False
      Tab(2).Control(4)=   "btnImprimirEvento"
      Tab(2).Control(4).Enabled=   0   'False
      Tab(2).Control(5)=   "btnEnviarEmailEvento"
      Tab(2).Control(5).Enabled=   0   'False
      Tab(2).Control(6)=   "btnLimparListaEvento"
      Tab(2).Control(6).Enabled=   0   'False
      Tab(2).Control(7)=   "btnImprimirEventoPDF"
      Tab(2).Control(7).Enabled=   0   'False
      Tab(2).ControlCount=   8
      TabCaption(3)   =   "Distribuição DFe"
      TabPicture(3)   =   "FrmMain.frx":0054
      Tab(3).ControlEnabled=   0   'False
      Tab(3).Control(0)=   "btnDFePorUltNSU"
      Tab(3).Control(1)=   "btnDFePorNSU"
      Tab(3).Control(2)=   "btnDFePorChave"
      Tab(3).ControlCount=   3
      Begin VB.CommandButton btnGerarChaveMDFe 
         Caption         =   "Gerar Chave MDFe"
         Height          =   360
         Left            =   120
         TabIndex        =   139
         Top             =   2880
         Width           =   1935
      End
      Begin VB.CommandButton btnDFePorUltNSU 
         Caption         =   "Por Ult. NSU"
         Height          =   360
         Left            =   -70800
         TabIndex        =   137
         Top             =   480
         Width           =   1935
      End
      Begin VB.CommandButton btnDFePorNSU 
         Caption         =   "Por NSU"
         Height          =   360
         Left            =   -72840
         TabIndex        =   136
         Top             =   480
         Width           =   1935
      End
      Begin VB.CommandButton btnDFePorChave 
         Caption         =   "Por Chave"
         Height          =   360
         Left            =   -74880
         TabIndex        =   135
         Top             =   480
         Width           =   1935
      End
      Begin VB.CommandButton btnImprimirEventoPDF 
         Caption         =   "Imprimir PDF Evento"
         Height          =   360
         Left            =   -72840
         TabIndex        =   134
         Top             =   1440
         Width           =   1935
      End
      Begin VB.CommandButton btnLimparListaEvento 
         Caption         =   "Limpar Lista Eventos"
         Height          =   360
         Left            =   -72840
         TabIndex        =   133
         Top             =   960
         Width           =   1935
      End
      Begin VB.CommandButton btnEnviarEmailEvento 
         Caption         =   "Enviar Evento Email"
         Height          =   360
         Left            =   -74880
         TabIndex        =   132
         Top             =   1920
         Width           =   1935
      End
      Begin VB.CommandButton btnImprimirEvento 
         Caption         =   "Imprimir Evento"
         Height          =   360
         Left            =   -74880
         TabIndex        =   131
         Top             =   1440
         Width           =   1935
      End
      Begin VB.CommandButton btnCarregarEvento 
         Caption         =   "Carregar Evento"
         Height          =   360
         Left            =   -74880
         TabIndex        =   130
         Top             =   960
         Width           =   1935
      End
      Begin VB.CommandButton btnEnviarEvento 
         Caption         =   "Enviar Evento"
         Height          =   360
         Left            =   -70800
         TabIndex        =   129
         Top             =   480
         Width           =   1935
      End
      Begin VB.CommandButton btnEncerrar 
         Caption         =   "Encerrar MDFe"
         Height          =   360
         Left            =   -72840
         TabIndex        =   128
         Top             =   480
         Width           =   1935
      End
      Begin VB.CommandButton btnCancelar 
         Caption         =   "Cancelar MDFe"
         Height          =   360
         Left            =   -74880
         TabIndex        =   127
         Top             =   480
         Width           =   1935
      End
      Begin VB.CommandButton btnConsNaoEncerrados 
         Caption         =   "Consultar Não Enc."
         Height          =   360
         Left            =   -72840
         TabIndex        =   126
         Top             =   960
         Width           =   1935
      End
      Begin VB.CommandButton btnConsultarRecibo 
         Caption         =   "Consultar Recibo"
         Height          =   360
         Left            =   -74880
         TabIndex        =   125
         Top             =   960
         Width           =   1935
      End
      Begin VB.CommandButton btnConsultaChave 
         Caption         =   "Consultar com Chave"
         Height          =   360
         Left            =   -70800
         TabIndex        =   124
         Top             =   480
         Width           =   1935
      End
      Begin VB.CommandButton btnConsultaXml 
         Caption         =   "Consultar com Xml"
         Height          =   360
         Left            =   -72840
         TabIndex        =   123
         Top             =   480
         Width           =   1935
      End
      Begin VB.CommandButton btnStatusServ 
         Caption         =   "Status de Serviço"
         Height          =   360
         Left            =   -74880
         TabIndex        =   122
         Top             =   480
         Width           =   1935
      End
      Begin VB.CommandButton btnLimparLista 
         Caption         =   "Limpar Lista MDFe"
         Height          =   360
         Left            =   4200
         TabIndex        =   121
         Top             =   960
         Width           =   1935
      End
      Begin VB.CommandButton btnValidarRegra 
         Caption         =   "Val. Regra de Neg."
         Height          =   360
         Left            =   2160
         TabIndex        =   120
         Top             =   2400
         Width           =   1935
      End
      Begin VB.CommandButton btnImprimirPDF 
         Caption         =   "Imprimir PDF DAMDFe"
         Height          =   360
         Left            =   2160
         TabIndex        =   119
         Top             =   1440
         Width           =   1935
      End
      Begin VB.CommandButton btnCarregarXml 
         Caption         =   "Carregar Xml MDFe"
         Height          =   360
         Left            =   2160
         TabIndex        =   118
         Top             =   960
         Width           =   1935
      End
      Begin VB.CommandButton btnAssinar 
         Caption         =   "Assinar MDFe "
         Height          =   360
         Left            =   120
         TabIndex        =   117
         Top             =   2400
         Width           =   1935
      End
      Begin VB.CommandButton btnEnviarEmail 
         Caption         =   "Enviar MDFe Email"
         Height          =   360
         Left            =   120
         TabIndex        =   116
         Top             =   1920
         Width           =   1935
      End
      Begin VB.CommandButton btnImprimir 
         Caption         =   "Imprimir DAMDFe"
         Height          =   360
         Left            =   120
         TabIndex        =   115
         Top             =   1440
         Width           =   1935
      End
      Begin VB.CommandButton btnCarregarIni 
         Caption         =   "Carregar INI MDFe"
         Height          =   360
         Left            =   120
         TabIndex        =   114
         Top             =   960
         Width           =   1935
      End
      Begin VB.CommandButton btnEnviarAssincrono 
         Caption         =   "Enviar Assincrono"
         Height          =   360
         Left            =   4200
         TabIndex        =   113
         Top             =   480
         Width           =   1935
      End
      Begin VB.CommandButton btnEnviarSincrono 
         Caption         =   "Enviar Sincrono"
         Height          =   360
         Left            =   2160
         TabIndex        =   112
         Top             =   480
         Width           =   1935
      End
      Begin VB.CommandButton btnGerarXml 
         Caption         =   "Gerar Xml"
         Height          =   360
         Left            =   120
         TabIndex        =   111
         Top             =   480
         Width           =   1935
      End
   End
   Begin TabDlg.SSTab SSTab1 
      Height          =   6855
      Left            =   120
      TabIndex        =   3
      Top             =   120
      Width           =   4695
      _ExtentX        =   8281
      _ExtentY        =   12091
      _Version        =   393216
      Style           =   1
      TabHeight       =   520
      TabCaption(0)   =   "Configurações"
      TabPicture(0)   =   "FrmMain.frx":0070
      Tab(0).ControlEnabled=   -1  'True
      Tab(0).Control(0)=   "nudTimeOut"
      Tab(0).Control(0).Enabled=   0   'False
      Tab(0).ControlCount=   1
      TabCaption(1)   =   "Documento Auxiliar"
      TabPicture(1)   =   "FrmMain.frx":008C
      Tab(1).ControlEnabled=   0   'False
      Tab(1).Control(0)=   "Frame4"
      Tab(1).Control(1)=   "btnLogomarca"
      Tab(1).Control(2)=   "txtLogomarca"
      Tab(1).Control(3)=   "Label13"
      Tab(1).ControlCount=   4
      TabCaption(2)   =   "Email"
      TabPicture(2)   =   "FrmMain.frx":00A8
      Tab(2).ControlEnabled=   0   'False
      Tab(2).Control(0)=   "Label40"
      Tab(2).Control(1)=   "Label41"
      Tab(2).Control(2)=   "Frame5"
      Tab(2).Control(3)=   "txtAssunto"
      Tab(2).Control(4)=   "txtMensagem"
      Tab(2).ControlCount=   5
      Begin VB.TextBox txtMensagem 
         Height          =   1365
         IMEMode         =   3  'DISABLE
         Left            =   -74880
         MultiLine       =   -1  'True
         PasswordChar    =   "*"
         ScrollBars      =   3  'Both
         TabIndex        =   110
         Top             =   5280
         Width           =   4155
      End
      Begin VB.TextBox txtAssunto 
         Height          =   285
         IMEMode         =   3  'DISABLE
         Left            =   -74880
         PasswordChar    =   "*"
         TabIndex        =   108
         Top             =   4680
         Width           =   4155
      End
      Begin VB.Frame Frame5 
         Caption         =   "Configurações"
         BeginProperty Font 
            Name            =   "Tahoma"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   3855
         Left            =   -74880
         TabIndex        =   91
         Top             =   480
         Width           =   4215
         Begin VB.TextBox txtNome 
            Height          =   285
            IMEMode         =   3  'DISABLE
            Left            =   120
            TabIndex        =   99
            Top             =   480
            Width           =   3915
         End
         Begin VB.TextBox txtEmail 
            Height          =   285
            IMEMode         =   3  'DISABLE
            Left            =   120
            TabIndex        =   98
            Top             =   1080
            Width           =   3915
         End
         Begin VB.TextBox txtHost 
            Height          =   285
            IMEMode         =   3  'DISABLE
            Left            =   120
            TabIndex        =   97
            Top             =   1680
            Width           =   3915
         End
         Begin VB.TextBox txtUsuario 
            Height          =   285
            IMEMode         =   3  'DISABLE
            Left            =   120
            TabIndex        =   96
            Top             =   2880
            Width           =   3915
         End
         Begin VB.TextBox txtSenha 
            Height          =   285
            IMEMode         =   3  'DISABLE
            Left            =   120
            PasswordChar    =   "*"
            TabIndex        =   95
            Top             =   3480
            Width           =   3915
         End
         Begin VB.TextBox txtnudPorta 
            Alignment       =   1  'Right Justify
            Height          =   285
            Left            =   120
            TabIndex        =   94
            Text            =   "5000"
            Top             =   2280
            Width           =   720
         End
         Begin VB.CheckBox ckbSSL 
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
            Height          =   195
            Left            =   1440
            TabIndex        =   93
            Top             =   2160
            Width           =   1335
         End
         Begin VB.CheckBox ckbTLS 
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
            Height          =   195
            Left            =   1440
            TabIndex        =   92
            Top             =   2400
            Width           =   1335
         End
         Begin MSComCtl2.UpDown nudPorta 
            Height          =   285
            Left            =   841
            TabIndex        =   100
            Top             =   2280
            Width           =   255
            _ExtentX        =   450
            _ExtentY        =   503
            _Version        =   393216
            Value           =   5000
            OrigLeft        =   3960
            OrigTop         =   720
            OrigRight       =   4215
            OrigBottom      =   975
            Max             =   99999
            Enabled         =   -1  'True
         End
         Begin VB.Label Label39 
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
            Left            =   120
            TabIndex        =   106
            Top             =   240
            Width           =   480
         End
         Begin VB.Label Label38 
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
            Left            =   120
            TabIndex        =   105
            Top             =   840
            Width           =   450
         End
         Begin VB.Label Label37 
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
            Left            =   120
            TabIndex        =   104
            Top             =   1440
            Width           =   900
         End
         Begin VB.Label Label36 
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
            Left            =   120
            TabIndex        =   103
            Top             =   2640
            Width           =   645
         End
         Begin VB.Label Label35 
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
            Left            =   120
            TabIndex        =   102
            Top             =   3240
            Width           =   525
         End
         Begin VB.Label Label34 
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
            Left            =   120
            TabIndex        =   101
            Top             =   2040
            Width           =   465
         End
      End
      Begin VB.Frame Frame4 
         Caption         =   "DAMDFe"
         BeginProperty Font 
            Name            =   "Tahoma"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   735
         Left            =   -74880
         TabIndex        =   88
         Top             =   1200
         Width           =   4215
         Begin VB.OptionButton rdbPaisagem 
            Caption         =   "Paisagem"
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
            Left            =   2640
            TabIndex        =   90
            Top             =   360
            Width           =   1335
         End
         Begin VB.OptionButton rdbRetrato 
            Caption         =   "Retrato"
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
            TabIndex        =   89
            Top             =   360
            Value           =   -1  'True
            Width           =   1455
         End
      End
      Begin VB.CommandButton btnLogomarca 
         Caption         =   "..."
         Height          =   260
         Left            =   -71160
         TabIndex        =   87
         Top             =   840
         Width           =   390
      End
      Begin VB.TextBox txtLogomarca 
         Height          =   285
         Left            =   -74880
         TabIndex        =   86
         Top             =   840
         Width           =   3675
      End
      Begin TabDlg.SSTab nudTimeOut 
         Height          =   6255
         Left            =   120
         TabIndex        =   4
         Top             =   480
         Width           =   4455
         _ExtentX        =   7858
         _ExtentY        =   11033
         _Version        =   393216
         Style           =   1
         Tabs            =   4
         TabsPerRow      =   4
         TabHeight       =   520
         TabCaption(0)   =   "Geral"
         TabPicture(0)   =   "FrmMain.frx":00C4
         Tab(0).ControlEnabled=   -1  'True
         Tab(0).Control(0)=   "Label1"
         Tab(0).Control(0).Enabled=   0   'False
         Tab(0).Control(1)=   "Label22"
         Tab(0).Control(1).Enabled=   0   'False
         Tab(0).Control(2)=   "Label23"
         Tab(0).Control(2).Enabled=   0   'False
         Tab(0).Control(3)=   "Label24"
         Tab(0).Control(3).Enabled=   0   'False
         Tab(0).Control(4)=   "Label25"
         Tab(0).Control(4).Enabled=   0   'False
         Tab(0).Control(5)=   "ckbExibirErroSchema"
         Tab(0).Control(5).Enabled=   0   'False
         Tab(0).Control(6)=   "txtFormatoAlerta"
         Tab(0).Control(6).Enabled=   0   'False
         Tab(0).Control(7)=   "cmbFormaEmissao"
         Tab(0).Control(7).Enabled=   0   'False
         Tab(0).Control(8)=   "cmbVersaoDF"
         Tab(0).Control(8).Enabled=   0   'False
         Tab(0).Control(9)=   "ckbRetirarAcentos"
         Tab(0).Control(9).Enabled=   0   'False
         Tab(0).Control(10)=   "ckbSalvar"
         Tab(0).Control(10).Enabled=   0   'False
         Tab(0).Control(11)=   "txtLogs"
         Tab(0).Control(11).Enabled=   0   'False
         Tab(0).Control(12)=   "btnSelectLog"
         Tab(0).Control(12).Enabled=   0   'False
         Tab(0).Control(13)=   "txtSchemaPath"
         Tab(0).Control(13).Enabled=   0   'False
         Tab(0).Control(14)=   "btnSelectSchema"
         Tab(0).Control(14).Enabled=   0   'False
         Tab(0).ControlCount=   15
         TabCaption(1)   =   "Webservices"
         TabPicture(1)   =   "FrmMain.frx":00E0
         Tab(1).ControlEnabled=   0   'False
         Tab(1).Control(0)=   "Label2"
         Tab(1).Control(1)=   "Label26"
         Tab(1).Control(2)=   "Label27"
         Tab(1).Control(3)=   "timeout"
         Tab(1).Control(4)=   "cmbUfDestino"
         Tab(1).Control(5)=   "cmbSSlType"
         Tab(1).Control(6)=   "txtTimeOut"
         Tab(1).Control(7)=   "FraAmbiente"
         Tab(1).Control(8)=   "Frame1"
         Tab(1).Control(9)=   "ckbVisualizar"
         Tab(1).Control(10)=   "ckbSalvarSOAP"
         Tab(1).Control(11)=   "Frame2"
         Tab(1).ControlCount=   12
         TabCaption(2)   =   "Certificados"
         TabPicture(2)   =   "FrmMain.frx":00FC
         Tab(2).ControlEnabled=   0   'False
         Tab(2).Control(0)=   "btnObterCertificados"
         Tab(2).Control(1)=   "Frame3"
         Tab(2).Control(2)=   "cmbXmlSign"
         Tab(2).Control(3)=   "cmbHttp"
         Tab(2).Control(4)=   "cmbCrypt"
         Tab(2).Control(5)=   "Label32"
         Tab(2).Control(6)=   "Label7"
         Tab(2).Control(7)=   "Label6"
         Tab(2).ControlCount=   8
         TabCaption(3)   =   "Arquivos"
         TabPicture(3)   =   "FrmMain.frx":0118
         Tab(3).ControlEnabled=   0   'False
         Tab(3).Control(0)=   "btnArqEvento"
         Tab(3).Control(1)=   "txtArqEvento"
         Tab(3).Control(2)=   "btnArqMDFe"
         Tab(3).Control(3)=   "txtArqMDFe"
         Tab(3).Control(4)=   "ckbSepararPorModelo"
         Tab(3).Control(5)=   "ckbSepararPorCNPJ"
         Tab(3).Control(6)=   "ckbSalvaPathEvento"
         Tab(3).Control(7)=   "ckbEmissaoPathNFe"
         Tab(3).Control(8)=   "ckbAdicionaLiteral"
         Tab(3).Control(9)=   "ckbPastaMensal"
         Tab(3).Control(10)=   "ckbSalvarArqs"
         Tab(3).Control(11)=   "Label12"
         Tab(3).Control(12)=   "Label11"
         Tab(3).ControlCount=   13
         Begin VB.CommandButton btnArqEvento 
            Caption         =   "..."
            Height          =   260
            Left            =   -71160
            TabIndex        =   84
            Top             =   3240
            Width           =   390
         End
         Begin VB.TextBox txtArqEvento 
            Height          =   285
            Left            =   -74880
            TabIndex        =   83
            Top             =   3240
            Width           =   3675
         End
         Begin VB.CommandButton btnArqMDFe 
            Caption         =   "..."
            Height          =   260
            Left            =   -71160
            TabIndex        =   81
            Top             =   2520
            Width           =   390
         End
         Begin VB.TextBox txtArqMDFe 
            Height          =   285
            Left            =   -74880
            TabIndex        =   79
            Top             =   2520
            Width           =   3675
         End
         Begin VB.CheckBox ckbSepararPorModelo 
            Caption         =   "Separar Arqs pelo Modelo do Documento"
            Height          =   195
            Left            =   -74880
            TabIndex        =   78
            Top             =   1920
            Width           =   3855
         End
         Begin VB.CheckBox ckbSepararPorCNPJ 
            Caption         =   "Separar Arqs pelo CNPJ do Certificado"
            Height          =   195
            Left            =   -74880
            TabIndex        =   77
            Top             =   1680
            Width           =   3855
         End
         Begin VB.CheckBox ckbSalvaPathEvento 
            Caption         =   "Salvar Arquivos de Eventos"
            Height          =   195
            Left            =   -74880
            TabIndex        =   76
            Top             =   1440
            Width           =   3855
         End
         Begin VB.CheckBox ckbEmissaoPathNFe 
            Caption         =   "Salvar Documento pelo campo Data de Emissão"
            Height          =   195
            Left            =   -74880
            TabIndex        =   75
            Top             =   1200
            Width           =   3855
         End
         Begin VB.CheckBox ckbAdicionaLiteral 
            Caption         =   "Adicionar Literal no nome das pastas"
            Height          =   195
            Left            =   -74880
            TabIndex        =   74
            Top             =   960
            Width           =   3495
         End
         Begin VB.CheckBox ckbPastaMensal 
            Caption         =   "Criar Pastas Mensalmente"
            Height          =   195
            Left            =   -74880
            TabIndex        =   73
            Top             =   720
            Width           =   3495
         End
         Begin VB.CheckBox ckbSalvarArqs 
            Caption         =   "Salvar Arquivos em Pastas Separadas"
            Height          =   195
            Left            =   -74880
            TabIndex        =   72
            Top             =   480
            Width           =   3495
         End
         Begin VB.CommandButton btnObterCertificados 
            Caption         =   "Obter Certificados"
            Height          =   360
            Left            =   -74880
            TabIndex        =   71
            Top             =   4920
            Width           =   1935
         End
         Begin VB.Frame Frame3 
            Caption         =   "Certificados"
            BeginProperty Font 
               Name            =   "Tahoma"
               Size            =   8.25
               Charset         =   0
               Weight          =   700
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   2655
            Left            =   -74880
            TabIndex        =   60
            Top             =   2160
            Width           =   4215
            Begin VB.CommandButton btnDadosPFX 
               Caption         =   "..."
               Height          =   260
               Left            =   3720
               TabIndex        =   70
               Top             =   1080
               Width           =   390
            End
            Begin VB.TextBox txtDadosPFX 
               Height          =   285
               Left            =   120
               TabIndex        =   68
               Top             =   1080
               Width           =   3555
            End
            Begin VB.CommandButton btnSelecionarCertificado 
               Caption         =   "..."
               Height          =   260
               Left            =   3720
               TabIndex        =   64
               Top             =   479
               Width           =   390
            End
            Begin VB.TextBox txtCertPath 
               Height          =   285
               Left            =   120
               TabIndex        =   63
               Top             =   480
               Width           =   3555
            End
            Begin VB.TextBox txtCertPassword 
               Height          =   285
               IMEMode         =   3  'DISABLE
               Left            =   120
               PasswordChar    =   "*"
               TabIndex        =   62
               Top             =   1680
               Width           =   4035
            End
            Begin VB.TextBox txtCertNumero 
               Height          =   285
               Left            =   120
               TabIndex        =   61
               Top             =   2280
               Width           =   4035
            End
            Begin VB.Label Label33 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Dados PFX"
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
               TabIndex        =   69
               Top             =   840
               Width           =   870
            End
            Begin VB.Label Label10 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Caminho"
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
               TabIndex        =   67
               Top             =   240
               Width           =   735
            End
            Begin VB.Label Label9 
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
               Left            =   120
               TabIndex        =   66
               Top             =   1440
               Width           =   525
            End
            Begin VB.Label Label8 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Número de Série"
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
               TabIndex        =   65
               Top             =   2040
               Width           =   1395
            End
         End
         Begin VB.ComboBox cmbXmlSign 
            Height          =   315
            ItemData        =   "FrmMain.frx":0134
            Left            =   -74880
            List            =   "FrmMain.frx":0147
            Style           =   2  'Dropdown List
            TabIndex        =   59
            Top             =   1800
            Width           =   2175
         End
         Begin VB.ComboBox cmbHttp 
            Height          =   315
            ItemData        =   "FrmMain.frx":0181
            Left            =   -74880
            List            =   "FrmMain.frx":0194
            Style           =   2  'Dropdown List
            TabIndex        =   57
            Top             =   1200
            Width           =   2175
         End
         Begin VB.ComboBox cmbCrypt 
            Height          =   315
            ItemData        =   "FrmMain.frx":01D3
            Left            =   -74880
            List            =   "FrmMain.frx":01E3
            Style           =   2  'Dropdown List
            TabIndex        =   55
            Top             =   600
            Width           =   2175
         End
         Begin VB.Frame Frame2 
            Caption         =   "Retorno de Envio"
            BeginProperty Font 
               Name            =   "Tahoma"
               Size            =   8.25
               Charset         =   0
               Weight          =   700
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   1335
            Left            =   -74880
            TabIndex        =   43
            Top             =   2400
            Width           =   4215
            Begin VB.TextBox txtIntervalo 
               Alignment       =   1  'Right Justify
               Height          =   285
               Left            =   2760
               TabIndex        =   47
               Text            =   "5000"
               Top             =   840
               Width           =   990
            End
            Begin VB.TextBox txtTentativas 
               Alignment       =   1  'Right Justify
               Height          =   285
               Left            =   1440
               TabIndex        =   46
               Text            =   "5000"
               Top             =   840
               Width           =   990
            End
            Begin VB.TextBox txtAguardar 
               Alignment       =   1  'Right Justify
               Height          =   285
               Left            =   120
               TabIndex        =   45
               Text            =   "5000"
               Top             =   840
               Width           =   990
            End
            Begin VB.CheckBox ckbAjustarAut 
               Caption         =   "Ajustar Automaticamente prop. ""Aguardar"""
               Height          =   195
               Left            =   120
               TabIndex        =   44
               Top             =   240
               Width           =   3855
            End
            Begin MSComCtl2.UpDown nudAguardar 
               Height          =   285
               Left            =   1080
               TabIndex        =   48
               Top             =   840
               Width           =   255
               _ExtentX        =   450
               _ExtentY        =   503
               _Version        =   393216
               Value           =   5000
               OrigLeft        =   3840
               OrigTop         =   600
               OrigRight       =   4095
               OrigBottom      =   885
               Max             =   99999
               Enabled         =   -1  'True
            End
            Begin MSComCtl2.UpDown nudTentativas 
               Height          =   285
               Left            =   2400
               TabIndex        =   49
               Top             =   840
               Width           =   255
               _ExtentX        =   450
               _ExtentY        =   503
               _Version        =   393216
               Value           =   5000
               OrigLeft        =   3840
               OrigTop         =   600
               OrigRight       =   4095
               OrigBottom      =   885
               Max             =   99999
               Enabled         =   -1  'True
            End
            Begin MSComCtl2.UpDown nudIntervalos 
               Height          =   285
               Left            =   3720
               TabIndex        =   50
               Top             =   840
               Width           =   255
               _ExtentX        =   450
               _ExtentY        =   503
               _Version        =   393216
               Value           =   5000
               OrigLeft        =   3840
               OrigTop         =   600
               OrigRight       =   4095
               OrigBottom      =   885
               Max             =   99999
               Enabled         =   -1  'True
            End
            Begin VB.Label Label5 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Intervalo"
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
               TabIndex        =   53
               Top             =   600
               Width           =   795
            End
            Begin VB.Label Label4 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Tentativas"
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
               Left            =   1440
               TabIndex        =   52
               Top             =   600
               Width           =   915
            End
            Begin VB.Label Label3 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Aguardar"
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
               TabIndex        =   51
               Top             =   600
               Width           =   795
            End
         End
         Begin VB.CheckBox ckbSalvarSOAP 
            Caption         =   "Salvar envelope SOAP"
            Height          =   195
            Left            =   -74880
            TabIndex        =   42
            Top             =   2160
            Width           =   2535
         End
         Begin VB.CheckBox ckbVisualizar 
            Caption         =   "Visualizar Mensagem"
            Height          =   195
            Left            =   -74880
            TabIndex        =   41
            Top             =   1800
            Width           =   2535
         End
         Begin VB.Frame Frame1 
            Caption         =   "Proxy"
            BeginProperty Font 
               Name            =   "Tahoma"
               Size            =   8.25
               Charset         =   0
               Weight          =   700
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   2175
            Left            =   -74880
            TabIndex        =   31
            Top             =   3840
            Width           =   4215
            Begin VB.TextBox txtProxyServidor 
               Height          =   285
               Left            =   120
               TabIndex        =   35
               Top             =   480
               Width           =   2655
            End
            Begin VB.TextBox txtProxyPorta 
               Alignment       =   1  'Right Justify
               Height          =   285
               Left            =   2880
               TabIndex        =   34
               Text            =   "5000"
               Top             =   480
               Width           =   945
            End
            Begin VB.TextBox txtProxyUsuario 
               Height          =   285
               Left            =   120
               TabIndex        =   33
               Top             =   1080
               Width           =   3975
            End
            Begin VB.TextBox txtProxySenha 
               Height          =   285
               IMEMode         =   3  'DISABLE
               Left            =   120
               PasswordChar    =   "*"
               TabIndex        =   32
               Top             =   1680
               Width           =   3975
            End
            Begin MSComCtl2.UpDown nudProxyPorta 
               Height          =   285
               Left            =   3825
               TabIndex        =   36
               Top             =   480
               Width           =   255
               _ExtentX        =   450
               _ExtentY        =   503
               _Version        =   393216
               Value           =   5000
               OrigLeft        =   3825
               OrigTop         =   480
               OrigRight       =   4080
               OrigBottom      =   765
               Max             =   99999
               Enabled         =   -1  'True
            End
            Begin VB.Label Label31 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Servidor"
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
               TabIndex        =   40
               Top             =   240
               Width           =   720
            End
            Begin VB.Label Label30 
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
               Left            =   2880
               TabIndex        =   39
               Top             =   240
               Width           =   465
            End
            Begin VB.Label Label29 
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
               Left            =   120
               TabIndex        =   38
               Top             =   840
               Width           =   720
            End
            Begin VB.Label Label28 
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
               Left            =   120
               TabIndex        =   37
               Top             =   1440
               Width           =   525
            End
         End
         Begin VB.Frame FraAmbiente 
            Caption         =   "Ambiente"
            BeginProperty Font 
               Name            =   "Tahoma"
               Size            =   8.25
               Charset         =   0
               Weight          =   700
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   735
            Left            =   -74880
            TabIndex        =   28
            Top             =   960
            Width           =   4215
            Begin VB.OptionButton rdbHomologacao 
               Caption         =   "Homologação"
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
               TabIndex        =   30
               Top             =   360
               Value           =   -1  'True
               Width           =   1455
            End
            Begin VB.OptionButton rdbProducao 
               Caption         =   "Produção"
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
               Left            =   2880
               TabIndex        =   29
               Top             =   360
               Width           =   1095
            End
         End
         Begin VB.TextBox txtTimeOut 
            Alignment       =   1  'Right Justify
            Height          =   285
            Left            =   -72000
            TabIndex        =   26
            Text            =   "5000"
            Top             =   600
            Width           =   990
         End
         Begin VB.ComboBox cmbSSlType 
            Height          =   315
            ItemData        =   "FrmMain.frx":0215
            Left            =   -73800
            List            =   "FrmMain.frx":022E
            Style           =   2  'Dropdown List
            TabIndex        =   24
            Top             =   600
            Width           =   1695
         End
         Begin VB.ComboBox cmbUfDestino 
            Height          =   315
            ItemData        =   "FrmMain.frx":027A
            Left            =   -74880
            List            =   "FrmMain.frx":02CF
            Style           =   2  'Dropdown List
            TabIndex        =   22
            Top             =   600
            Width           =   975
         End
         Begin VB.CommandButton btnSelectSchema 
            Caption         =   "..."
            Height          =   260
            Left            =   3840
            TabIndex        =   20
            Top             =   4320
            Width           =   390
         End
         Begin VB.TextBox txtSchemaPath 
            Height          =   285
            Left            =   120
            TabIndex        =   19
            Top             =   4320
            Width           =   3675
         End
         Begin VB.CommandButton btnSelectLog 
            Caption         =   "..."
            Height          =   260
            Left            =   3840
            TabIndex        =   17
            Top             =   3720
            Width           =   390
         End
         Begin VB.TextBox txtLogs 
            Height          =   285
            Left            =   120
            TabIndex        =   16
            Top             =   3720
            Width           =   3675
         End
         Begin VB.CheckBox ckbSalvar 
            Caption         =   "Salvar Arquivos de Envio e Resposta"
            Height          =   195
            Left            =   120
            TabIndex        =   14
            Top             =   3120
            Width           =   3495
         End
         Begin VB.CheckBox ckbRetirarAcentos 
            Caption         =   "Retirar Acentos dos XMLs enviados"
            Height          =   195
            Left            =   120
            TabIndex        =   13
            Top             =   2760
            Width           =   3615
         End
         Begin VB.ComboBox cmbVersaoDF 
            Height          =   315
            ItemData        =   "FrmMain.frx":033F
            Left            =   120
            List            =   "FrmMain.frx":0349
            Style           =   2  'Dropdown List
            TabIndex        =   12
            Top             =   2280
            Width           =   2175
         End
         Begin VB.ComboBox cmbFormaEmissao 
            Height          =   315
            ItemData        =   "FrmMain.frx":035B
            Left            =   120
            List            =   "FrmMain.frx":0365
            Style           =   2  'Dropdown List
            TabIndex        =   10
            Top             =   1680
            Width           =   2175
         End
         Begin VB.TextBox txtFormatoAlerta 
            Height          =   285
            Left            =   120
            TabIndex        =   8
            Top             =   1080
            Width           =   3795
         End
         Begin VB.CheckBox ckbExibirErroSchema 
            Caption         =   "Exibir Erro Schema"
            Height          =   195
            Left            =   120
            TabIndex        =   6
            Top             =   480
            Width           =   2535
         End
         Begin MSComCtl2.UpDown timeout 
            Height          =   285
            Left            =   -71040
            TabIndex        =   27
            Top             =   600
            Width           =   255
            _ExtentX        =   450
            _ExtentY        =   503
            _Version        =   393216
            Value           =   5000
            OrigLeft        =   3840
            OrigTop         =   600
            OrigRight       =   4095
            OrigBottom      =   885
            Max             =   99999
            Enabled         =   -1  'True
         End
         Begin VB.Label Label12 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "Pasta Arquivos Evento"
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
            TabIndex        =   82
            Top             =   3000
            Width           =   1905
         End
         Begin VB.Label Label11 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "Pasta Arquivos MDFe"
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
            TabIndex        =   80
            Top             =   2280
            Width           =   1785
         End
         Begin VB.Label Label32 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "XmlSignLib"
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
            TabIndex        =   58
            Top             =   1560
            Width           =   915
         End
         Begin VB.Label Label7 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "HttpLib"
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
            TabIndex        =   56
            Top             =   960
            Width           =   615
         End
         Begin VB.Label Label6 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "CryptLib"
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
            TabIndex        =   54
            Top             =   360
            Width           =   705
         End
         Begin VB.Label Label27 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "TimeOut"
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
            Left            =   -72000
            TabIndex        =   25
            Top             =   360
            Width           =   765
         End
         Begin VB.Label Label26 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "SSL Type"
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
            Left            =   -73800
            TabIndex        =   23
            Top             =   360
            Width           =   765
         End
         Begin VB.Label Label2 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "Uf Destino"
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
            TabIndex        =   21
            Top             =   360
            Width           =   870
         End
         Begin VB.Label Label25 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "Pasta dos Schemas"
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
            TabIndex        =   18
            Top             =   4080
            Width           =   1635
         End
         Begin VB.Label Label24 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "Pasta dos Logs"
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
            TabIndex        =   15
            Top             =   3480
            Width           =   1260
         End
         Begin VB.Label Label23 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "Versão Documento Fiscal"
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
            TabIndex        =   11
            Top             =   2040
            Width           =   2115
         End
         Begin VB.Label Label22 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "Forma de Emissão"
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
            TabIndex        =   9
            Top             =   1440
            Width           =   1530
         End
         Begin VB.Label Label1 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "Formato Alerta"
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
            Top             =   840
            Width           =   1290
         End
      End
      Begin VB.Label Label41 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Mensagem"
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
         TabIndex        =   109
         Top             =   5040
         Width           =   930
      End
      Begin VB.Label Label40 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Assunto"
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
         TabIndex        =   107
         Top             =   4440
         Width           =   690
      End
      Begin VB.Label Label13 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Logomarca"
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
         TabIndex        =   85
         Top             =   600
         Width           =   945
      End
   End
   Begin MSComDlg.CommonDialog CommonDialog1 
      Left            =   5040
      Top             =   7200
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
   End
   Begin VB.Frame FraRespostas 
      Caption         =   "Respostas"
      Height          =   3735
      Left            =   4920
      TabIndex        =   1
      Top             =   3840
      Width           =   6495
      Begin VB.TextBox rtbRespostas 
         Height          =   3375
         Left            =   120
         Locked          =   -1  'True
         MultiLine       =   -1  'True
         ScrollBars      =   3  'Both
         TabIndex        =   2
         Top             =   240
         Width           =   6255
      End
   End
   Begin VB.CommandButton cmdSalvar 
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
      Left            =   2520
      TabIndex        =   0
      Top             =   7080
      Width           =   2295
   End
End
Attribute VB_Name = "FrmMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim mdfe As ACBrMDFe

Public Function validacao() As Boolean

    If txtSchemaPath.Text = "" Then
    MsgBox ("Informe Path com Schema")
    validacao = False
    
    ElseIf txtCertPath.Text = "" Then
    MsgBox ("Informe o certificado")
    validacao = False
    
    ElseIf txtCertPassword.Text = "" Then
    MsgBox ("Informe a senha")
    validacao = False
    
    ElseIf txtCertNumero.Text = "" Then
    MsgBox ("Informe o número de série")
    validacao = False
    
    ElseIf cmbCrypt.Text = "cryNone" Then
    MsgBox ("Informe Criptografia")
    validacao = False
    
    ElseIf cmbHttp.Text = "httpNone" Then
    MsgBox ("Informe o tipo SSL")
    validacao = False
    
    ElseIf cmbXmlSign.Text = "xsNone" Then
    MsgBox ("Informe assinatura do XML")
    validacao = False
    
    ElseIf cmbSSlType.Text = "LT_all" Then
    MsgBox ("Informe o tipo SSL")
    validacao = False
    
    End If
    
    validacao = True
    
End Function

Public Function validacaoEmail() As Boolean

    If txtHost.Text = "" Then
    MsgBox ("Informe Host SMTP")
    validacaoEmail = False
    
    ElseIf txtUsuario.Text = "" Then
    MsgBox ("Informe Usuário")
    validacaoEmail = False
    
    ElseIf txtSenha.Text = "" Then
    MsgBox ("Informe Senha")
    validacaoEmail = False
    
    ElseIf txtNome.Text = "" Then
    MsgBox ("Informe Nome do Proprietario do e-mail")
    validacaoEmail = False
    
    ElseIf txtEmail.Text = "" Then
    MsgBox ("Informe e-mail do Proprietario")
    validacaoEmail = False
    
    ElseIf txtnudPorta.Text = "" Then
    MsgBox ("Informe porta de conexão")
    validacaoEmail = False
    
    ElseIf ckbSSL.Value = False And ckbTLS.Value = False Then
    MsgBox ("Informe o certificado SSL")
    MsgBox ("Informe o certificado TLS")
    validacaoEmail = False
    
    End If
    
    validacaoEmail = True

End Function


Private Sub btnConsNaoEncerrados_Click()

    If Not validacao Then
        MsgBox ("Erro Verifique as configurações do certificado")
        Exit Sub
    End If
    
    On Error GoTo Erro:
    Dim aCNPj As String
    
    aCNPj = InputBox("Número do CNPJ do emitente.", "WebServices Consultar: CNPJ", "")
    SetResposta mdfe.ConsultaMDFeNaoEnc(aCNPj)
    
Erro:
    MsgBox Err.Descriptio

End Sub

Private Sub CheckMDFeLista()

    Dim xml As Boolean
    
    answer = MsgBox("Limpar Lista ?", vbExclamation + vbYesNo, "Add Confirm")
    If answer = vbYes Then
    mdfe.LimparLista
    End If
    
    If xml Then
    
    CommonDialog1.DialogTitle = "Selecione o arquivo ini"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivo xml MDFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
            
    If CommonDialog1.FileName = vbNullString Then Exit Sub
    
    mdfe.CarregarXML CommonDialog1.FileName
    
    Else
    
    CommonDialog1.DialogTitle = "Selecione o arquivo ini"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivo ini MDFe (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
            
    If CommonDialog1.FileName = vbNullString Then Exit Sub
    
    mdfe.CarregarINI CommonDialog1.FileName
    End If

End Sub

Private Sub btnArqEvento_Click()

    txtArqEvento.Text = BrowseFolder("Selecione a pasta Arquivos Eventos")

End Sub

Private Sub btnArqMDFe_Click()

    txtArqMDFe.Text = BrowseFolder("Selecione a pasta Arquivos MDFe")

End Sub

Private Sub btnAssinar_Click()

    If Not validacao Then
        MsgBox ("Erro Verifique as configurações do certificado")
        Exit Sub
    End If

     On Error GoTo Erro:
    CheckMDFeLista
    mdfe.Assinar
    
Erro:
    MsgBox Err.Description
    
End Sub

Private Sub btnCancelar_Click()

    If Not validacao Then
        MsgBox ("Erro Verifique as configurações do certificado")
        Exit Sub
    End If

    On Error GoTo Erro:
    Dim idLote As String
    Dim aJustificativa As String
    Dim eChave As String
    Dim eCNPJ As String
    
    idLote = InputBox("Identificador de controle do Lote de envio do Evento", "WebServices Eventos: Cancelamento", "1")
    eChave = InputBox("Chave da MDF-e", "WebServices Eventos: Cancelamento", "")
    eCNPJ = InputBox("CNPJ ou o CPF do autor do Evento", "WebServices Eventos: Cancelamento", "")
    aJustificativa = InputBox("Justificativa do Cancelamento", "WebServices Eventos: Cancelamento", "")

    SetResposta mdfe.Cancelar(eChave, aJustificativa, eCNPJ, CLng(idLote))
    
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnCarregar_Click()
    LoadConfig
End Sub

Private Sub btnCarregarEvento_Click()

    On Error GoTo Erro:
    Dim arquivoIni As String
    
    CommonDialog1.DialogTitle = "Selecione o arquivo ini"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivo Ini MDFe (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
            
    If CommonDialog1.FileName = vbNullString Then Exit Sub
    arquivoIni = CommonDialog1.FileName
    
    mdfe.CarregarEventoINI (arquivoIni)
   
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnCarregarIni_Click()

    On Error GoTo Erro:
    CheckMDFeLista
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnCarregarXml_Click()

    On Error GoTo Erro:
    
    CheckMDFeLista

Erro:
    MsgBox Err.Description

End Sub



Private Sub btnConsultaChave_Click()

    If Not validacao Then
        MsgBox ("Erro Verifique as configurações do certificado")
        Exit Sub
    End If

    On Error GoTo Erro:
    Dim chaveOuMDFe As String
    
    chaveOuMDFe = InputBox("Chave da MDF-e:", "WebServices Consultar: Nota", "")
    SetResposta mdfe.Consultar(chaveOuMDFe)
    
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnConsultarRecibo_Click()

    If Not validacao Then
        MsgBox ("Erro Verifique as configurações do certificado")
        Exit Sub
    End If

    On Error GoTo Erro:
    Dim aRecibo As String
    
    aRecibo = InputBox("Número do recibo.", "WebServices Consultar: Recibo", "")
    SetResposta mdfe.ConsultarRecibo(aRecibo)
    
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnConsultaXml_Click()

    If Not validacao Then
        MsgBox ("Erro Verifique as configurações do certificado")
        Exit Sub
    End If

    On Error GoTo Erro:
    
    CommonDialog1.DialogTitle = "Selecione o arquivo xml"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivo Xml MDFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
            
    If CommonDialog1.FileName = vbNullString Then Exit Sub
    
    mdfe.LimparLista
    SetResposta mdfe.Consultar(CommonDialog1.FileName)
    
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnDadosPFX_Click()

    On Error Resume Next
    CommonDialog1.DialogTitle = "Selecione o certificado"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivos PFX (*.pfx)|*.pfx|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.ShowOpen
            
    If Err Then Exit Sub
    
    txtCertPath.Text = CommonDialog1.FileName

End Sub

Private Sub btnDFePorChave_Click()

    If Not validacao Then
        MsgBox ("Erro Verifique as configurações do certificado")
        Exit Sub
    End If

    On Error GoTo Erro:
    Dim ret As String
    Dim codUF As Long
    Dim CNPJ As String
    Dim Chave As String
    
    codUF = InputBox("Codigo da UF", "WebServices: Distribuição DFe", 35)
    CNPJ = InputBox("CNPJ do Autor", "WebServices: Distribuição DFe", "")
    Chave = InputBox("Chave do MDFe", "WebServices: Distribuição DFe", "")
    
    ret = mdfe.DistribuicaoDFePorChave(codUF, CNPJ, Chave)
    rtbRespostas.Text = ret
    
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnDFePorNSU_Click()

    If Not validacao Then
        MsgBox ("Erro Verifique as configurações do certificado")
        Exit Sub
    End If

    On Error GoTo Erro:
    Dim ret As String
    Dim codUF As Long
    Dim CNPJ As String
    Dim eNsu As String
    
    codUF = InputBox("Codigo da UF", "WebServices: Distribuição DFe", 35)
    CNPJ = InputBox("CNPJ do Autor", "WebServices: Distribuição DFe", "")
    eNsu = InputBox("Numero do NSU", "WebServices: Distribuição DFe", "")
    
    ret = mdfe.DistribuicaoDFePorNSU(codUF, CNPJ, eNsu)
    rtbRespostas.Text = ret
    
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnDFePorUltNSU_Click()

    If Not validacao Then
        MsgBox ("Erro Verifique as configurações do certificado")
        Exit Sub
    End If

    On Error GoTo Erro:
    Dim ret As String
    Dim codUF As Long
    Dim CNPJ As String
    Dim eNsu As String
    
    codUF = InputBox("Codigo da UF", "WebServices: Distribuição DFe", 35)
    CNPJ = InputBox("CNPJ do Autor", "WebServices: Distribuição DFe", "")
    eNsu = InputBox("Número do último NSU", "WebServices: Distribuição DFe", "")
    
    ret = mdfe.DistribuicaoDFePorNSU(codUF, CNPJ, eNsu)
    rtbRespostas.Text = ret
    
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnEncerrar_Click()

    If Not validacao Then
    MsgBox ("Erro Verifique as configurações do certificado")
        Exit Sub
    End If
    
    On Error GoTo Erro:
    Dim Chave As String
    Dim Municipio As String
    
    Chave = InputBox("Informe Chave MDFe", "WebServices Encerrar", "")
    Municipio = InputBox("Informe o código do municipio", "WebServices Encerrar", "")
    
    mdfe.EncerrarMDFE Chave, DateTime.Now, Municipio
    
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnEnviarAssincrono_Click()

    If Not validacao Then
        MsgBox ("Erro Verifique as configurações do certificado")
        Exit Sub
    End If

    On Error GoTo Erro:
    Dim ret As String
    Dim aLote As Long
    
    CheckMDFeLista
    aLote = InputBox("Número do Lote", "WebServices Enviar", 1)
    ret = mdfe.Enviar(aLote)
    rtbRespostas.Text = ret
    
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnEnviarEmail_Click()

    If Not validacaoEmail Then
        MsgBox ("Verifique as configurações de E-mail")
        Exit Sub
    End If

    On Error GoTo Erro:
    
    CommonDialog1.DialogTitle = "Selecione o arquivo xml"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivo Xml MDFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
            
    If CommonDialog1.FileName = vbNullString Then Exit Sub
    
    Dim destinatario As String
    destinatario = InputBox("Digite o email do destinatario", "Envio email", vbNullString)
    
    If destinatario = vbNullString Then Exit Sub
    mdfe.EnviarEmail destinatario, CommonDialog1.FileName, True, txtAssunto.Text, txtMensagem.Text
    
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnEnviarEmailEvento_Click()

    If Not validacaoEmail Then
        MsgBox ("Verifique as configurações de E-mail")
        Exit Sub
    End If

    On Error GoTo Erro:
    Dim arquivoXmlEvento As String
    Dim arquivoXml As String
    Dim destinatario As String
    
    CommonDialog1.DialogTitle = "Selecione o arquivo xml"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivo Xml Evento (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
         
    If CommonDialog1.FileName = vbNullString Then Exit Sub
    arquivoXmlEvento = CommonDialog1.FileName
    
    CommonDialog1.DialogTitle = "Selecione o arquivo xml"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivo Xml MDFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
    
    If CommonDialog.FileName1 = vbNullString Then Exit Sub
    arquivoXml = CommonDialog.FileName
    
    destinatario = ""
    destinatario = InputBox("Digite o email do destinatario", "Envio email", "")
    
    mdfe.EnviarEmailEvento destinatario, arquivoXmlEvento, arquivoXml, True, txtAssuntoEmail.Text, txtMensagemEmail.Text
    
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnEnviarEvento_Click()

    If Not validacao Then
        MsgBox ("Erro Verifique as configurações do certificado")
        Exit Sub
    End If

    On Error GoTo Erro:
    Dim ret As String
    Dim idLote As Long
    
    idLote = InputBox("Identificador de controle do Lote de envio do Evento", "WebServices Eventos: Enviar", "")
    ret = mdfe.EnviarEvento(idLote)
    rtbRespostas.Text = ret
    
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnEnviarSincrono_Click()

    If Not validacao Then
        MsgBox ("Erro Verifique as configurações do certificado")
        Exit Sub
    End If

    On Error GoTo Erro:
    Dim ret As String
    Dim aLote As Long
    
    CheckMDFeLista
    aLote = InputBox("Número do Lote", "WebServices Enviar", 1)
    ret = mdfe.Enviar(aLote, sincrono = True)
    rtbRespostas.Text = ret
    
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnGerarChaveMDFe_Click()

    On Error GoTo Erro:
    
    Dim uf As Long
    Dim cod As Long
    Dim doc As Long
    Dim serie As Long
    Dim numero As Long
    Dim emissao As Long
    Dim cnpjCPF As Long
    
    uf = InputBox("Digite o codigo da UF:", "Gerar Chave", 35)
    cod = InputBox("Digite o codigo da Númerico:", "Gerar Chave", 45812)
    doc = InputBox("Digite o modelo do documento:", "Gerar Chave", 55)
    serie = InputBox("Digite a serie do documento:", "Gerar Chave", 1)
    numero = InputBox("Digite o numero do documento:", "Gerar Chave", 1)
    emissao = InputBox("Digite o tipo de emissão do documento:", "Gerar Chave", 1)
    cnpjCPF = InputBox("Digite o CPF/CNPJ para Gerar a Chave", "Gerar Chave", "")
    
    rtbRespostas = nfe.GerarChave(uf, cod, doc, serie, numero, emissao, DateTime.Now, cnpjCPF)
    
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnGerarXml_Click()
    
    If Not validacao Then
        MsgBox ("Erro Verifique as configurações do certificado")
        Exit Sub
    End If

    On Error GoTo Erro:
    Dim ret As String
    
    CommonDialog1.DialogTitle = "Selecione o arquivo ini"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivo ini NFe (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
            
    If CommonDialog1.FileName = vbNullString Then Exit Sub
    
    mdfe.LimparLista
    mdfe.CarregarINI CommonDialog1.FileName
    mdfe.Assinar
    ret = mdfe.ObterXml(0)
    rtbRespostas.Text = ret
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnImprimir_Click()
    
    On Error GoTo Erro:
    
    CommonDialog1.DialogTitle = "Selecione o arquivo xml"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivo Xml MDFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
            
    If CommonDialog1.FileName = vbNullString Then Exit Sub
    
    mdfe.LimparLista
    mdfe.CarregarXML CommonDialog1.FileName
    mdfe.Imprimir
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnImprimirEvento_Click()

    On Error GoTo Erro:
    Dim arquivoXmlEvento As String
    Dim arquivoXml As String
    
    CommonDialog1.DialogTitle = "Selecione o arquivo xml"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivo Xml Evento (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
         
    If CommonDialog1.FileName = vbNullString Then Exit Sub
    arquivoXmlEvento = CommonDialog1.FileName
    
    CommonDialog1.DialogTitle = "Selecione o arquivo xml"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivo Xml MDFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
    
    If CommonDialog.FileName1 = vbNullString Then Exit Sub
    arquivoXml = CommonDialog.FileName
    
    mdfe.ImprimirEvento arquivoXml, arquivoXmlEvento
    
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnImprimirEventoPDF_Click()

    On Error GoTo Erro:
    Dim arquivoXmlEvento As String
    Dim arquivoXml As String
    
    CommonDialog1.DialogTitle = "Selecione o arquivo xml"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivo Xml Evento (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
         
    If CommonDialog1.FileName = vbNullString Then Exit Sub
    arquivoXmlEvento = CommonDialog1.FileName
    
    CommonDialog1.DialogTitle = "Selecione o arquivo xml"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivo Xml MDFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
    
    If CommonDialog.FileName1 = vbNullString Then Exit Sub
    arquivoXml = CommonDialog.FileName
    
    mdfe.ImprimirEventoPDF arquivoXml, arquivoXmlEvento
    
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnImprimirPDF_Click()

    On Error GoTo Erro:
    
    CommonDialog1.DialogTitle = "Selecione o arquivo xml"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivo Xml DAMDFE (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
            
    If CommonDialog1.FileName = vbNullString Then Exit Sub
    
    mdfe.LimparLista
    mdfe.CarregarXML CommonDialog1.FileName
    mdfe.Imprimir
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnLimparLista_Click()

    On Error GoTo Erro:
    answer = MsgBox("Limpar Lista ?", vbExclamation + vbYesNo, "Add Confirm")
    If answer = vbYes Then
    mdfe.LimparLista
    End If

Erro:
    MsgBox Err.Description

End Sub

Private Sub btnLimparListaEvento_Click()

    On Error GoTo Erro:
    answer = MsgBox("Limpar a lista de eventos ?", vbExclamation + vbYesNo, "Add Confirm")
    If answer = vbYes Then
    mdfe.LimparListaEventos
    End If

Erro:
    MsgBox Err.Description

End Sub

Private Sub btnLogomarca_Click()

    On Error Resume Next
    CommonDialog1.DialogTitle = "Selecione a Logomarca"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivos Imagem (*.bmp, *.jpeg, *.png)|*.bmp; *.jpeg; *.png|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.ShowOpen
            
    If Err Then Exit Sub
    
    txtLogomarca.Text = CommonDialog1.FileName

End Sub

Private Sub btnObterCertificados_Click()

    mdfe.ObterCertificados
    
End Sub

Private Sub btnSelecionarCertificado_Click()
    On Error Resume Next
    CommonDialog1.DialogTitle = "Selecione o certificado"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivos PFX (*.pfx)|*.pfx|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.ShowOpen
            
    If Err Then Exit Sub
    
    txtCertPath.Text = CommonDialog1.FileName
End Sub

Private Sub btnSelectSchema_Click()
    txtSchemaPath.Text = BrowseFolder("Selecione a pasta dos Schemas")
End Sub

Private Sub btnStatusServ_Click()
    
    If Not validacao Then
        MsgBox ("Erro Verifique as configurações do certificado")
        Exit Sub
    End If
        
    SetResposta mdfe.StatusServico
   
End Sub

Private Sub btnValidarRegra_Click()

   On Error GoTo Erro:
    
    CheckNFeLista = True
    rtbRespostas.Text = mdfe.ValidarRegrasdeNegocios
    
Erro:
    MsgBox Err.Descriptions

End Sub

Private Sub cmdSalvar_Click()
    SalvarConfig
End Sub

Private Sub Form_Load()
    cmbUfDestino.Text = "SP"
    cmbSSlType.ListIndex = 0
    cmbCrypt.ListIndex = 0
    cmbHttp.ListIndex = 0
    cmbXmlSign.ListIndex = 0
    
    Dim LogPath As String
    Dim IniPath As String
    
    LogPath = App.Path & "\Logs\"
    IniPath = App.Path & "\ACBrLib.ini"
    
    If Not DirExists(LogPath) Then
        MkDir LogPath
    End If
    
    Set mdfe = CreateMDFe(IniPath)
    
    mdfe.ConfigGravarValor SESSAO_PRINCIPAL, "LogNivel", NivelLog.logParanoico
    mdfe.ConfigGravarValor SESSAO_PRINCIPAL, "LogPath", LogPath
    mdfe.ConfigGravar
    
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
    Dim retorno As Long
    Dim buffer As String
    Dim bufferLen As Long
    
    mdfe.ConfigLer
    cmbVersaoDF.ListIndex = CLng(mdfe.ConfigLerValor(SESSAO_MDFe, "VersaoDF"))
    cmbCrypt.ListIndex = CLng(mdfe.ConfigLerValor(SESSAO_DFE, "SSLCryptLib"))
    cmbHttp.ListIndex = CLng(mdfe.ConfigLerValor(SESSAO_DFE, "SSLHttpLib"))
    cmbXmlSign.ListIndex = CLng(mdfe.ConfigLerValor(SESSAO_DFE, "SSLXmlSignLib"))
    txtCertPath.Text = mdfe.ConfigLerValor(SESSAO_DFE, "ArquivoPFX")
    txtCertPassword.Text = mdfe.ConfigLerValor(SESSAO_DFE, "Senha")
    txtCertNumero.Text = mdfe.ConfigLerValor(SESSAO_DFE, "NumeroSerie")
    txtSchemaPath.Text = mdfe.ConfigLerValor(SESSAO_MDFe, "PathSchemas")
    cmbUfDestino.Text = mdfe.ConfigLerValor(SESSAO_DFE, "UF")
    
    Dim ambiente As String
    ambiente = mdfe.ConfigLerValor(SESSAO_MDFe, "Ambiente")
    
    rdbHomologacao.Value = CBool(ambiente)
    rdbProducao.Value = Not CBool(ambiente)
    
    cmbSSlType.ListIndex = CLng(mdfe.ConfigLerValor(SESSAO_MDFe, "SSLType"))
    timeout.Value = CLng(mdfe.ConfigLerValor(SESSAO_MDFe, "Timeout"))
    txtProxyServidor.Text = mdfe.ConfigLerValor(SESSAO_PROXY, "Servidor")
    
    Dim porta As String
    porta = mdfe.ConfigLerValor(SESSAO_PROXY, "Porta")
    
    If IsNumeric(porta) Then
      nudProxyPorta.Value = CLng(porta)
    End If
    
    txtProxyUsuario.Text = mdfe.ConfigLerValor(SESSAO_PROXY, "Usuario")
    txtProxySenha.Text = mdfe.ConfigLerValor(SESSAO_PROXY, "Senha")
    txtNome.Text = mdfe.ConfigLerValor(SESSAO_EMAIL, "Nome")
    txtEmail.Text = mdfe.ConfigLerValor(SESSAO_EMAIL, "Conta")
    txtUsuario.Text = mdfe.ConfigLerValor(SESSAO_EMAIL, "Usuario")
    txtSenha.Text = mdfe.ConfigLerValor(SESSAO_EMAIL, "Senha")
    txtHost.Text = mdfe.ConfigLerValor(SESSAO_EMAIL, "Servidor")
    nudPorta.Value = CLng(mdfe.ConfigLerValor(SESSAO_EMAIL, "Porta"))
    ckbSSL.Value = CLng(mdfe.ConfigLerValor(SESSAO_EMAIL, "SSL"))
    ckbTLS.Value = CLng(mdfe.ConfigLerValor(SESSAO_EMAIL, "TLS"))
    
End Sub

Private Sub SalvarConfig()
    Dim retorno As Long
    
    'mdfe.ConfigGravarValor SESSAO_MDFe, "VersaoDF", CStr(cmbVersao.ListIndex)
    mdfe.ConfigGravarValor SESSAO_DFE, "SSLCryptLib", CStr(cmbCrypt.ListIndex)
    mdfe.ConfigGravarValor SESSAO_DFE, "SSLHttpLib", CStr(cmbHttp.ListIndex)
    mdfe.ConfigGravarValor SESSAO_DFE, "SSLXmlSignLib", CStr(cmbXmlSign.ListIndex)
    mdfe.ConfigGravarValor SESSAO_DFE, "ArquivoPFX", txtCertPath.Text
    mdfe.ConfigGravarValor SESSAO_DFE, "Senha", txtCertPassword.Text
    mdfe.ConfigGravarValor SESSAO_DFE, "NumeroSerie", txtCertNumero.Text
    mdfe.ConfigGravarValor SESSAO_MDFe, "PathSchemas", txtSchemaPath.Text
    mdfe.ConfigGravarValor SESSAO_DFE, "UF", cmbUfDestino.Text
    mdfe.ConfigGravarValor SESSAO_MDFe, "Ambiente", CStr(rdbHomologacao.Value)
    mdfe.ConfigGravarValor SESSAO_MDFe, "SSLType", CStr(cmbSSlType.ListIndex)
    mdfe.ConfigGravarValor SESSAO_MDFe, "Timeout", CStr(txtTimeOut.Text)
    mdfe.ConfigGravarValor SESSAO_PROXY, "Servidor", txtProxyServidor.Text
    mdfe.ConfigGravarValor SESSAO_PROXY, "Porta", CStr(nudProxyPorta.Value)
    mdfe.ConfigGravarValor SESSAO_PROXY, "Usuario", txtProxyUsuario.Text
    mdfe.ConfigGravarValor SESSAO_PROXY, "Senha", txtProxySenha.Text
    mdfe.ConfigGravarValor SESSAO_EMAIL, "Nome", txtNome.Text
    mdfe.ConfigGravarValor SESSAO_EMAIL, "Conta", txtEmail.Text
    mdfe.ConfigGravarValor SESSAO_EMAIL, "Usuario", txtUsuario.Text
    mdfe.ConfigGravarValor SESSAO_EMAIL, "Senha", txtSenha.Text
    mdfe.ConfigGravarValor SESSAO_EMAIL, "Servidor", txtHost.Text
    mdfe.ConfigGravarValor SESSAO_EMAIL, "Porta", CStr(nudPorta.Value)
    mdfe.ConfigGravarValor SESSAO_EMAIL, "SSL", CStr(ckbSSL.Value)
    mdfe.ConfigGravarValor SESSAO_EMAIL, "TLS", CStr(ckbTLS.Value)
    mdfe.ConfigGravar
    
End Sub
