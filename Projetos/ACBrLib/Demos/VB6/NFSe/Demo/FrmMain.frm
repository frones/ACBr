VERSION 5.00
Object = "{86CF1D34-0C5F-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCT2.OCX"
Object = "{BDC217C8-ED16-11CD-956C-0000C04E4C0A}#1.1#0"; "TABCTL32.OCX"
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "COMDLG32.OCX"
Begin VB.Form FrmMain 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "ACBrLibNFSe Demo"
   ClientHeight    =   9735
   ClientLeft      =   45
   ClientTop       =   390
   ClientWidth     =   12375
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
   ScaleHeight     =   9735
   ScaleWidth      =   12375
   StartUpPosition =   2  'CenterScreen
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
      Left            =   120
      TabIndex        =   4
      Top             =   9120
      Width           =   2535
   End
   Begin TabDlg.SSTab SSTab1 
      Height          =   4335
      Left            =   5640
      TabIndex        =   3
      Top             =   120
      Width           =   6615
      _ExtentX        =   11668
      _ExtentY        =   7646
      _Version        =   393216
      Style           =   1
      Tabs            =   5
      TabsPerRow      =   5
      TabHeight       =   520
      TabCaption(0)   =   "Envios"
      Tab(0).ControlEnabled=   -1  'True
      Tab(0).Control(0)=   "btnEmitirNota"
      Tab(0).Control(0).Enabled=   0   'False
      Tab(0).Control(1)=   "btnGerarEnviarLoteAssincrono"
      Tab(0).Control(1).Enabled=   0   'False
      Tab(0).Control(2)=   "btnSubstituirNFSe"
      Tab(0).Control(2).Enabled=   0   'False
      Tab(0).Control(3)=   "btnImprimirDANFSe"
      Tab(0).Control(3).Enabled=   0   'False
      Tab(0).Control(4)=   "btnLimparListaNFSe"
      Tab(0).Control(4).Enabled=   0   'False
      Tab(0).Control(5)=   "btnGerarLoteRPS"
      Tab(0).Control(5).Enabled=   0   'False
      Tab(0).Control(6)=   "btnCarregarXMLNFse"
      Tab(0).Control(6).Enabled=   0   'False
      Tab(0).Control(7)=   "btnGravarXMLNFSe"
      Tab(0).Control(7).Enabled=   0   'False
      Tab(0).Control(8)=   "btnEnviarEmail"
      Tab(0).Control(8).Enabled=   0   'False
      Tab(0).Control(9)=   "btnGerarEnviarLoteSincrono"
      Tab(0).Control(9).Enabled=   0   'False
      Tab(0).Control(10)=   "btnEnviarRPS"
      Tab(0).Control(10).Enabled=   0   'False
      Tab(0).Control(11)=   "btnImprimirNFSe"
      Tab(0).Control(11).Enabled=   0   'False
      Tab(0).Control(12)=   "btnLinkNFSe"
      Tab(0).Control(12).Enabled=   0   'False
      Tab(0).Control(13)=   "btnGerarToken"
      Tab(0).Control(13).Enabled=   0   'False
      Tab(0).Control(14)=   "btnObterXMLNFSe"
      Tab(0).Control(14).Enabled=   0   'False
      Tab(0).ControlCount=   15
      TabCaption(1)   =   "Consultas"
      Tab(1).ControlEnabled=   0   'False
      Tab(1).Control(0)=   "btnConsultarSituacaoLote"
      Tab(1).Control(1)=   "btnConsultarNFSePorNumero"
      Tab(1).Control(2)=   "btnConsultarNFSeGenerico"
      Tab(1).Control(3)=   "btnConsultarLoteRPS"
      Tab(1).Control(4)=   "btnConsultarNFSePorPeriodo"
      Tab(1).Control(5)=   "btnConsultarNFSePorRPS"
      Tab(1).Control(6)=   "btnConsultarNFSePorFaixa"
      Tab(1).ControlCount=   7
      TabCaption(2)   =   "Cons. Serv. Prestados"
      Tab(2).ControlEnabled=   0   'False
      Tab(2).Control(0)=   "btnConsultarNFSeServicoPrestadoPorNumero"
      Tab(2).Control(1)=   "btnConsultarNFSeSevicoPrestadoPorTomador"
      Tab(2).Control(2)=   "btnConsultarNFSeServicoPrestadoPorPeriodo"
      Tab(2).Control(3)=   "btnConsultarNFSeServicoPrestadoPorIntermediario"
      Tab(2).ControlCount=   4
      TabCaption(3)   =   "Cons. Serv. Tomados"
      Tab(3).ControlEnabled=   0   'False
      Tab(3).Control(0)=   "btnConsultarNFSeServicoTomadoPorNumero"
      Tab(3).Control(1)=   "btnConsultarNFSeServicoTomadoPorPrestador"
      Tab(3).Control(2)=   "btnConsultarNFSeServicoTomadoPorTomador"
      Tab(3).Control(3)=   "btnConsultarNFSeServicoTomadoPorPeriodo"
      Tab(3).Control(4)=   "btnConsultarNFSeServicoTomadoPorIntermediario"
      Tab(3).ControlCount=   5
      TabCaption(4)   =   "Cancelamento"
      Tab(4).ControlEnabled=   0   'False
      Tab(4).Control(0)=   "btnCancelarNFSe"
      Tab(4).ControlCount=   1
      Begin VB.CommandButton btnCancelarNFSe 
         Caption         =   "Cancelar NFSe"
         Height          =   375
         Left            =   -74880
         TabIndex        =   182
         Top             =   480
         Width           =   2415
      End
      Begin VB.CommandButton btnConsultarNFSeServicoTomadoPorIntermediario 
         Caption         =   "Consultar NFSe Serviço Tomado Por Intermediário"
         Height          =   375
         Left            =   -74880
         TabIndex        =   181
         Top             =   2400
         Width           =   3975
      End
      Begin VB.CommandButton btnConsultarNFSeServicoTomadoPorPeriodo 
         Caption         =   "Consultar NFSe Serviço Tomado Por Periodo"
         Height          =   375
         Left            =   -74880
         TabIndex        =   180
         Top             =   1920
         Width           =   3975
      End
      Begin VB.CommandButton btnConsultarNFSeServicoTomadoPorTomador 
         Caption         =   "Consultar NFSe Serviço Tomado Por Tomador"
         Height          =   375
         Left            =   -74880
         TabIndex        =   179
         Top             =   1440
         Width           =   3975
      End
      Begin VB.CommandButton btnConsultarNFSeServicoTomadoPorPrestador 
         Caption         =   "Consultar NFSe Serviço Tomado Por Prestador"
         Height          =   375
         Left            =   -74880
         TabIndex        =   178
         Top             =   960
         Width           =   3975
      End
      Begin VB.CommandButton btnConsultarNFSeServicoTomadoPorNumero 
         Caption         =   "Consultar NFSe Serviço Tomado Por Número"
         Height          =   375
         Left            =   -74880
         TabIndex        =   177
         Top             =   480
         Width           =   3975
      End
      Begin VB.CommandButton btnConsultarNFSeServicoPrestadoPorIntermediario 
         Caption         =   "Consultar NFSe Serviço Prestado Por Intermediário"
         Height          =   375
         Left            =   -74880
         TabIndex        =   176
         Top             =   1920
         Width           =   3975
      End
      Begin VB.CommandButton btnConsultarNFSeServicoPrestadoPorPeriodo 
         Caption         =   "Consultar NFSe Serviço Prestado Por Periodo"
         Height          =   375
         Left            =   -74880
         TabIndex        =   175
         Top             =   1440
         Width           =   3975
      End
      Begin VB.CommandButton btnConsultarNFSeSevicoPrestadoPorTomador 
         Caption         =   "Consultar NFSe Serviço Prestado Por Tomador"
         Height          =   375
         Left            =   -74880
         TabIndex        =   174
         Top             =   960
         Width           =   3975
      End
      Begin VB.CommandButton btnConsultarNFSeServicoPrestadoPorNumero 
         Caption         =   "Consultar NFSe Serviço Prestado Por Número"
         Height          =   375
         Left            =   -74880
         TabIndex        =   173
         Top             =   480
         Width           =   3975
      End
      Begin VB.CommandButton btnConsultarNFSePorFaixa 
         Caption         =   "Consultar NFSe por Faixa"
         Height          =   375
         Left            =   -72360
         TabIndex        =   172
         Top             =   1440
         Width           =   2415
      End
      Begin VB.CommandButton btnConsultarNFSePorRPS 
         Caption         =   "Consultar NFSe por RPS"
         Height          =   375
         Left            =   -72360
         TabIndex        =   171
         Top             =   960
         Width           =   2415
      End
      Begin VB.CommandButton btnConsultarNFSePorPeriodo 
         Caption         =   "Consultar NFSe por Período"
         Height          =   375
         Left            =   -72360
         TabIndex        =   170
         Top             =   480
         Width           =   2415
      End
      Begin VB.CommandButton btnConsultarLoteRPS 
         Caption         =   "Consultar Lote RPS"
         Height          =   375
         Left            =   -74880
         TabIndex        =   169
         Top             =   1920
         Width           =   2415
      End
      Begin VB.CommandButton btnConsultarNFSeGenerico 
         Caption         =   "Consultar NFSe Genérico"
         Height          =   375
         Left            =   -74880
         TabIndex        =   168
         Top             =   1440
         Width           =   2415
      End
      Begin VB.CommandButton btnConsultarNFSePorNumero 
         Caption         =   "Consultar NFSe por Numero"
         Height          =   375
         Left            =   -74880
         TabIndex        =   167
         Top             =   960
         Width           =   2415
      End
      Begin VB.CommandButton btnConsultarSituacaoLote 
         Caption         =   "Consultar Situação do Lote"
         Height          =   375
         Left            =   -74880
         TabIndex        =   166
         Top             =   480
         Width           =   2415
      End
      Begin VB.CommandButton btnObterXMLNFSe 
         Caption         =   "Obter XML NFSe"
         Height          =   375
         Left            =   2640
         TabIndex        =   165
         Top             =   3360
         Width           =   2415
      End
      Begin VB.CommandButton btnGerarToken 
         Caption         =   "Gerar Token"
         Height          =   375
         Left            =   2640
         TabIndex        =   164
         Top             =   2880
         Width           =   2415
      End
      Begin VB.CommandButton btnLinkNFSe 
         Caption         =   "Link NFSe"
         Height          =   375
         Left            =   2640
         TabIndex        =   163
         Top             =   2400
         Width           =   2415
      End
      Begin VB.CommandButton btnImprimirNFSe 
         Caption         =   "Imprimir NFSe"
         Height          =   375
         Left            =   2640
         TabIndex        =   162
         Top             =   1920
         Width           =   2415
      End
      Begin VB.CommandButton btnEnviarRPS 
         Caption         =   "Enviar um RPS"
         Height          =   375
         Left            =   2640
         TabIndex        =   161
         Top             =   1440
         Width           =   2415
      End
      Begin VB.CommandButton btnGerarEnviarLoteSincrono 
         Caption         =   "Enviar Lote RPS (Síncrono)"
         Height          =   375
         Left            =   2640
         TabIndex        =   160
         Top             =   960
         Width           =   2415
      End
      Begin VB.CommandButton btnEnviarEmail 
         Caption         =   "Enviar Email"
         Height          =   375
         Left            =   2640
         TabIndex        =   159
         Top             =   480
         Width           =   2415
      End
      Begin VB.CommandButton btnGravarXMLNFSe 
         Caption         =   "Gravar XML NFSe"
         Height          =   375
         Left            =   120
         TabIndex        =   158
         Top             =   3840
         Width           =   2415
      End
      Begin VB.CommandButton btnCarregarXMLNFse 
         Caption         =   "Carregar XML NFSe"
         Height          =   375
         Left            =   120
         TabIndex        =   157
         Top             =   3360
         Width           =   2415
      End
      Begin VB.CommandButton btnGerarLoteRPS 
         Caption         =   "Gerar Lote RPS"
         Height          =   375
         Left            =   120
         TabIndex        =   156
         Top             =   2880
         Width           =   2415
      End
      Begin VB.CommandButton btnLimparListaNFSe 
         Caption         =   "Limpar Lista NFSe"
         Height          =   375
         Left            =   120
         TabIndex        =   155
         Top             =   2400
         Width           =   2415
      End
      Begin VB.CommandButton btnImprimirDANFSe 
         Caption         =   "Imprimir DANFSe"
         Height          =   375
         Left            =   120
         TabIndex        =   154
         Top             =   1920
         Width           =   2415
      End
      Begin VB.CommandButton btnSubstituirNFSe 
         Caption         =   "Substituir NFSe"
         Height          =   375
         Left            =   120
         TabIndex        =   153
         Top             =   1440
         Width           =   2415
      End
      Begin VB.CommandButton btnGerarEnviarLoteAssincrono 
         Caption         =   "Enviar Lote RPS (Assíncrono)"
         Height          =   375
         Left            =   120
         TabIndex        =   152
         Top             =   960
         Width           =   2415
      End
      Begin VB.CommandButton btnEmitirNota 
         Caption         =   "Emitir Nota (Novo)"
         Height          =   375
         Left            =   120
         TabIndex        =   151
         Top             =   480
         Width           =   2415
      End
   End
   Begin MSComDlg.CommonDialog CommonDialog1 
      Left            =   5760
      Top             =   9120
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
   End
   Begin VB.Frame FraRespostas 
      Caption         =   "Respostas"
      Height          =   4455
      Left            =   5640
      TabIndex        =   1
      Top             =   4560
      Width           =   6615
      Begin VB.TextBox rtbRespostas 
         Height          =   4095
         Left            =   120
         Locked          =   -1  'True
         MultiLine       =   -1  'True
         ScrollBars      =   3  'Both
         TabIndex        =   2
         Top             =   240
         Width           =   6375
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
      Left            =   3240
      TabIndex        =   0
      Top             =   9120
      Width           =   2295
   End
   Begin TabDlg.SSTab SSTTab0 
      Height          =   8895
      Left            =   120
      TabIndex        =   5
      Top             =   120
      Width           =   5415
      _ExtentX        =   9551
      _ExtentY        =   15690
      _Version        =   393216
      Style           =   1
      TabHeight       =   520
      WordWrap        =   0   'False
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Tahoma"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      TabCaption(0)   =   "Configurações"
      TabPicture(0)   =   "FrmMain.frx":0000
      Tab(0).ControlEnabled=   -1  'True
      Tab(0).Control(0)=   "SSTab2"
      Tab(0).Control(0).Enabled=   0   'False
      Tab(0).ControlCount=   1
      TabCaption(1)   =   "Documento Auxiliar"
      TabPicture(1)   =   "FrmMain.frx":001C
      Tab(1).ControlEnabled=   0   'False
      Tab(1).Control(0)=   "txtPastaPDF"
      Tab(1).Control(1)=   "btnPastaPDF"
      Tab(1).Control(2)=   "txtNomePrefeitura"
      Tab(1).Control(3)=   "txtLogoMarcaPrestadorServico"
      Tab(1).Control(4)=   "btnLogoMarcaPrestadorServico"
      Tab(1).Control(5)=   "btnLogomarcaPrefeitura"
      Tab(1).Control(6)=   "txtLogomarcaPrefeitura"
      Tab(1).Control(7)=   "Label30"
      Tab(1).Control(8)=   "Label29"
      Tab(1).Control(9)=   "Label28"
      Tab(1).Control(10)=   "lblLogomarca"
      Tab(1).ControlCount=   11
      TabCaption(2)   =   "Email"
      Tab(2).ControlEnabled=   0   'False
      Tab(2).Control(0)=   "txtMensagemEmail"
      Tab(2).Control(1)=   "txtAssuntoEmail"
      Tab(2).Control(2)=   "Frame2"
      Tab(2).Control(3)=   "Label22"
      Tab(2).Control(4)=   "Label13"
      Tab(2).ControlCount=   5
      Begin VB.TextBox txtPastaPDF 
         Height          =   285
         Left            =   -74880
         TabIndex        =   149
         Top             =   2640
         Width           =   4170
      End
      Begin VB.CommandButton btnPastaPDF 
         Caption         =   "..."
         Height          =   260
         Left            =   -70560
         TabIndex        =   148
         Top             =   2640
         Width           =   645
      End
      Begin VB.TextBox txtNomePrefeitura 
         Height          =   285
         Left            =   -74880
         TabIndex        =   146
         Top             =   2040
         Width           =   5010
      End
      Begin VB.TextBox txtLogoMarcaPrestadorServico 
         Height          =   285
         Left            =   -74880
         TabIndex        =   144
         Top             =   1320
         Width           =   4170
      End
      Begin VB.CommandButton btnLogoMarcaPrestadorServico 
         Caption         =   "..."
         Height          =   260
         Left            =   -70560
         TabIndex        =   143
         Top             =   1320
         Width           =   645
      End
      Begin VB.CommandButton btnLogomarcaPrefeitura 
         Caption         =   "..."
         Height          =   260
         Left            =   -70560
         TabIndex        =   80
         Top             =   720
         Width           =   645
      End
      Begin VB.TextBox txtLogomarcaPrefeitura 
         Height          =   285
         Left            =   -74880
         TabIndex        =   79
         Top             =   720
         Width           =   4170
      End
      Begin VB.TextBox txtMensagemEmail 
         Height          =   1365
         IMEMode         =   3  'DISABLE
         Left            =   -74400
         MultiLine       =   -1  'True
         ScrollBars      =   3  'Both
         TabIndex        =   73
         Top             =   5760
         Width           =   4155
      End
      Begin VB.TextBox txtAssuntoEmail 
         Height          =   285
         IMEMode         =   3  'DISABLE
         Left            =   -74400
         TabIndex        =   71
         Top             =   5040
         Width           =   4155
      End
      Begin VB.Frame Frame2 
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
         Left            =   -74400
         TabIndex        =   58
         Top             =   600
         Width           =   4215
         Begin VB.TextBox txtPortaEmail 
            Height          =   285
            Left            =   120
            TabIndex        =   85
            Top             =   2280
            Width           =   450
         End
         Begin VB.TextBox txtSenha 
            Height          =   285
            Left            =   120
            TabIndex        =   84
            Top             =   3480
            Width           =   3855
         End
         Begin VB.TextBox txtUsuario 
            Height          =   285
            Left            =   120
            TabIndex        =   83
            Top             =   2880
            Width           =   3855
         End
         Begin VB.TextBox txtNome 
            Height          =   285
            IMEMode         =   3  'DISABLE
            Left            =   120
            TabIndex        =   63
            Top             =   480
            Width           =   3915
         End
         Begin VB.TextBox txtEmail 
            Height          =   285
            IMEMode         =   3  'DISABLE
            Left            =   120
            TabIndex        =   62
            Top             =   1080
            Width           =   3915
         End
         Begin VB.TextBox txtHost 
            Height          =   285
            IMEMode         =   3  'DISABLE
            Left            =   120
            TabIndex        =   61
            Top             =   1680
            Width           =   3915
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
            Height          =   195
            Left            =   1440
            TabIndex        =   60
            Top             =   2160
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
            Height          =   195
            Left            =   1440
            TabIndex        =   59
            Top             =   2400
            Width           =   1590
         End
         Begin MSComCtl2.UpDown nudEmailPorta 
            Height          =   285
            Left            =   571
            TabIndex        =   93
            Top             =   2280
            Width           =   255
            _ExtentX        =   450
            _ExtentY        =   503
            _Version        =   393216
            Value           =   5000
            BuddyControl    =   "txtPortaEmail"
            BuddyDispid     =   196655
            OrigLeft        =   840
            OrigTop         =   2280
            OrigRight       =   1095
            OrigBottom      =   2565
            Max             =   99999
            SyncBuddy       =   -1  'True
            BuddyProperty   =   0
            Enabled         =   -1  'True
         End
         Begin VB.Label Label12 
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
            TabIndex        =   69
            Top             =   240
            Width           =   480
         End
         Begin VB.Label Label11 
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
            TabIndex        =   68
            Top             =   840
            Width           =   450
         End
         Begin VB.Label Label10 
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
            TabIndex        =   67
            Top             =   1440
            Width           =   900
         End
         Begin VB.Label Label9 
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
            TabIndex        =   66
            Top             =   2640
            Width           =   645
         End
         Begin VB.Label Label8 
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
            TabIndex        =   65
            Top             =   3240
            Width           =   525
         End
         Begin VB.Label Label7 
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
            TabIndex        =   64
            Top             =   2040
            Width           =   465
         End
      End
      Begin TabDlg.SSTab SSTab2 
         Height          =   8415
         Left            =   120
         TabIndex        =   6
         Top             =   360
         Width           =   5175
         _ExtentX        =   9128
         _ExtentY        =   14843
         _Version        =   393216
         Style           =   1
         Tabs            =   5
         TabsPerRow      =   5
         TabHeight       =   520
         TabCaption(0)   =   "Geral"
         TabPicture(0)   =   "FrmMain.frx":0038
         Tab(0).ControlEnabled=   -1  'True
         Tab(0).Control(0)=   "lblFormatoAlerta"
         Tab(0).Control(0).Enabled=   0   'False
         Tab(0).Control(1)=   "lblFormaEmissao"
         Tab(0).Control(1).Enabled=   0   'False
         Tab(0).Control(2)=   "lblPastaLogs"
         Tab(0).Control(2).Enabled=   0   'False
         Tab(0).Control(3)=   "lblPastaSchemas"
         Tab(0).Control(3).Enabled=   0   'False
         Tab(0).Control(4)=   "Label1"
         Tab(0).Control(4).Enabled=   0   'False
         Tab(0).Control(5)=   "chkExibirErroSchema"
         Tab(0).Control(5).Enabled=   0   'False
         Tab(0).Control(6)=   "txtFormatoAlerta"
         Tab(0).Control(6).Enabled=   0   'False
         Tab(0).Control(7)=   "cmbFormaEmissao"
         Tab(0).Control(7).Enabled=   0   'False
         Tab(0).Control(8)=   "chkRetirarAcentos"
         Tab(0).Control(8).Enabled=   0   'False
         Tab(0).Control(9)=   "ckbSalvarArqeResp"
         Tab(0).Control(9).Enabled=   0   'False
         Tab(0).Control(10)=   "btnSelectLog"
         Tab(0).Control(10).Enabled=   0   'False
         Tab(0).Control(11)=   "txtLogs"
         Tab(0).Control(11).Enabled=   0   'False
         Tab(0).Control(12)=   "btnSelectSchema"
         Tab(0).Control(12).Enabled=   0   'False
         Tab(0).Control(13)=   "txtSchemaPath"
         Tab(0).Control(13).Enabled=   0   'False
         Tab(0).Control(14)=   "ckbMontarPathSchemas"
         Tab(0).Control(14).Enabled=   0   'False
         Tab(0).Control(15)=   "ckbConsultarLoteAposEnvio"
         Tab(0).Control(15).Enabled=   0   'False
         Tab(0).Control(16)=   "ckbConsultarAposCancelar"
         Tab(0).Control(16).Enabled=   0   'False
         Tab(0).Control(17)=   "cmbLayoutNFSe"
         Tab(0).Control(17).Enabled=   0   'False
         Tab(0).ControlCount=   18
         TabCaption(1)   =   "WebServices"
         TabPicture(1)   =   "FrmMain.frx":0054
         Tab(1).ControlEnabled=   0   'False
         Tab(1).Control(0)=   "txtEmitenteChaveAutorizacao"
         Tab(1).Control(1)=   "txtEmitenteChaveAcesso"
         Tab(1).Control(2)=   "txtEmitenteFraseSecreta"
         Tab(1).Control(3)=   "txtEmitenteSenha"
         Tab(1).Control(4)=   "txtEmitenteUsuário"
         Tab(1).Control(5)=   "txtTimeOut"
         Tab(1).Control(6)=   "frmAmbiente"
         Tab(1).Control(7)=   "cmbUFDestino"
         Tab(1).Control(8)=   "cmbSSlType"
         Tab(1).Control(9)=   "frmProxy"
         Tab(1).Control(10)=   "frmRetEnvio"
         Tab(1).Control(11)=   "chkVisualizarMensagem"
         Tab(1).Control(12)=   "chkSalvarSOAP"
         Tab(1).Control(13)=   "nudTimeOut"
         Tab(1).Control(14)=   "Label6"
         Tab(1).Control(15)=   "Label5"
         Tab(1).Control(16)=   "Label4"
         Tab(1).Control(17)=   "Label3"
         Tab(1).Control(18)=   "Label2"
         Tab(1).Control(19)=   "lblUFDestino"
         Tab(1).Control(20)=   "lblSSLType"
         Tab(1).Control(21)=   "lblTimeOut(1)"
         Tab(1).ControlCount=   22
         TabCaption(2)   =   "Certificados"
         TabPicture(2)   =   "FrmMain.frx":0070
         Tab(2).ControlEnabled=   0   'False
         Tab(2).Control(0)=   "btnObterCertificados"
         Tab(2).Control(1)=   "frmCertificados"
         Tab(2).Control(2)=   "cmbXmlSign"
         Tab(2).Control(3)=   "cmbHttp"
         Tab(2).Control(4)=   "cmbCrypt"
         Tab(2).Control(5)=   "lblXMLSignLib"
         Tab(2).Control(6)=   "lblHttpLib"
         Tab(2).Control(7)=   "lblCryptLib"
         Tab(2).ControlCount=   8
         TabCaption(3)   =   "Emitente"
         TabPicture(3)   =   "FrmMain.frx":008C
         Tab(3).ControlEnabled=   0   'False
         Tab(3).Control(0)=   "txtBairroEmitente"
         Tab(3).Control(1)=   "txtComplementoEmitente"
         Tab(3).Control(2)=   "cmbCidadeEmitente"
         Tab(3).Control(3)=   "txtCNPJPrefeituraEmitente"
         Tab(3).Control(4)=   "txtCodCidadeEmitente"
         Tab(3).Control(5)=   "txtNumeroEmitente"
         Tab(3).Control(6)=   "txtLogradouroEmitente"
         Tab(3).Control(7)=   "txtCEPEmitente"
         Tab(3).Control(8)=   "txtFoneEmitente"
         Tab(3).Control(9)=   "txtFantasiaEmitente"
         Tab(3).Control(10)=   "txtRazaoSocialEmitente"
         Tab(3).Control(11)=   "txtInscMunicipalEmitente"
         Tab(3).Control(12)=   "txtCNPJEmitente"
         Tab(3).Control(13)=   "Label27"
         Tab(3).Control(14)=   "Label26"
         Tab(3).Control(15)=   "Label25"
         Tab(3).Control(16)=   "Label24"
         Tab(3).Control(17)=   "Label23"
         Tab(3).Control(18)=   "Label21"
         Tab(3).Control(19)=   "Label20"
         Tab(3).Control(20)=   "Label19"
         Tab(3).Control(21)=   "Label18"
         Tab(3).Control(22)=   "Label17"
         Tab(3).Control(23)=   "Label16"
         Tab(3).Control(24)=   "Label15"
         Tab(3).Control(25)=   "Label14"
         Tab(3).ControlCount=   26
         TabCaption(4)   =   "Arquivos"
         TabPicture(4)   =   "FrmMain.frx":00A8
         Tab(4).ControlEnabled=   0   'False
         Tab(4).Control(0)=   "chkSalvarArqs"
         Tab(4).Control(1)=   "chkPastaMensal"
         Tab(4).Control(2)=   "chkAdicionaLiteral"
         Tab(4).Control(3)=   "chkEmissaoPathNFSe"
         Tab(4).Control(4)=   "chkSepararPorCNPJ"
         Tab(4).Control(5)=   "txtArqNFSe"
         Tab(4).Control(6)=   "btnArqNFSe"
         Tab(4).Control(7)=   "lblPastaArqNFSe"
         Tab(4).ControlCount=   8
         Begin VB.TextBox txtBairroEmitente 
            Height          =   285
            Left            =   -72360
            TabIndex        =   141
            Top             =   4320
            Width           =   2355
         End
         Begin VB.TextBox txtComplementoEmitente 
            Height          =   285
            Left            =   -74880
            TabIndex        =   139
            Top             =   4320
            Width           =   2355
         End
         Begin VB.ComboBox cmbCidadeEmitente 
            Height          =   315
            Left            =   -74880
            Style           =   2  'Dropdown List
            TabIndex        =   137
            Top             =   5040
            Width           =   4815
         End
         Begin VB.TextBox txtCNPJPrefeituraEmitente 
            Height          =   285
            Left            =   -72360
            TabIndex        =   135
            Top             =   5760
            Width           =   2355
         End
         Begin VB.TextBox txtCodCidadeEmitente 
            Height          =   285
            Left            =   -74880
            TabIndex        =   133
            Top             =   5760
            Width           =   2355
         End
         Begin VB.TextBox txtNumeroEmitente 
            Height          =   285
            Left            =   -71160
            TabIndex        =   131
            Top             =   3600
            Width           =   1155
         End
         Begin VB.TextBox txtLogradouroEmitente 
            Height          =   285
            Left            =   -74880
            TabIndex        =   129
            Top             =   3600
            Width           =   3555
         End
         Begin VB.TextBox txtCEPEmitente 
            Height          =   285
            Left            =   -72360
            TabIndex        =   127
            Top             =   2880
            Width           =   2355
         End
         Begin VB.TextBox txtFoneEmitente 
            Height          =   285
            Left            =   -74880
            TabIndex        =   125
            Top             =   2880
            Width           =   2355
         End
         Begin VB.TextBox txtFantasiaEmitente 
            Height          =   285
            Left            =   -74880
            TabIndex        =   123
            Top             =   2160
            Width           =   4875
         End
         Begin VB.TextBox txtRazaoSocialEmitente 
            Height          =   285
            Left            =   -74880
            TabIndex        =   121
            Top             =   1440
            Width           =   4875
         End
         Begin VB.TextBox txtInscMunicipalEmitente 
            Height          =   285
            Left            =   -72360
            TabIndex        =   119
            Top             =   720
            Width           =   2355
         End
         Begin VB.TextBox txtCNPJEmitente 
            Height          =   285
            Left            =   -74880
            TabIndex        =   117
            Top             =   720
            Width           =   2355
         End
         Begin VB.CheckBox chkSalvarArqs 
            Caption         =   "Salvar Arquivos em Pastas Separadas"
            Height          =   195
            Left            =   -74880
            TabIndex        =   115
            Top             =   480
            Width           =   4335
         End
         Begin VB.CheckBox chkPastaMensal 
            Caption         =   "Criar Pastas Mensalmente"
            Height          =   255
            Left            =   -74880
            TabIndex        =   114
            Top             =   780
            Width           =   3855
         End
         Begin VB.CheckBox chkAdicionaLiteral 
            Caption         =   "Adicionar Literal no nome das pastas"
            Height          =   195
            Left            =   -74880
            TabIndex        =   113
            Top             =   1140
            Width           =   3615
         End
         Begin VB.CheckBox chkEmissaoPathNFSe 
            Caption         =   "Salvar Documento pelo campo Data de Emissão"
            Height          =   195
            Left            =   -74880
            TabIndex        =   112
            Top             =   1500
            Width           =   4455
         End
         Begin VB.CheckBox chkSepararPorCNPJ 
            Caption         =   "Separar Arqs pelo CNPJ do Certificado"
            Height          =   195
            Left            =   -74880
            TabIndex        =   111
            Top             =   1860
            Width           =   3615
         End
         Begin VB.TextBox txtArqNFSe 
            Height          =   285
            Left            =   -74880
            TabIndex        =   110
            Top             =   2460
            Width           =   3555
         End
         Begin VB.CommandButton btnArqNFSe 
            Caption         =   "..."
            Height          =   260
            Left            =   -71280
            TabIndex        =   109
            Top             =   2460
            Width           =   390
         End
         Begin VB.TextBox txtEmitenteChaveAutorizacao 
            Height          =   285
            Left            =   -72360
            TabIndex        =   107
            Top             =   3840
            Width           =   2295
         End
         Begin VB.TextBox txtEmitenteChaveAcesso 
            Height          =   285
            Left            =   -74760
            TabIndex        =   105
            Top             =   3840
            Width           =   2295
         End
         Begin VB.TextBox txtEmitenteFraseSecreta 
            Height          =   285
            Left            =   -74760
            TabIndex        =   103
            Top             =   3240
            Width           =   4695
         End
         Begin VB.TextBox txtEmitenteSenha 
            Height          =   285
            IMEMode         =   3  'DISABLE
            Left            =   -72360
            PasswordChar    =   "*"
            TabIndex        =   101
            Top             =   2640
            Width           =   2295
         End
         Begin VB.TextBox txtEmitenteUsuário 
            Height          =   285
            Left            =   -74760
            TabIndex        =   99
            Top             =   2640
            Width           =   2295
         End
         Begin VB.ComboBox cmbLayoutNFSe 
            Height          =   315
            ItemData        =   "FrmMain.frx":00C4
            Left            =   120
            List            =   "FrmMain.frx":00CE
            Style           =   2  'Dropdown List
            TabIndex        =   97
            Top             =   5640
            Width           =   2295
         End
         Begin VB.CheckBox ckbConsultarAposCancelar 
            Caption         =   "Consultar Após Cancelar"
            Height          =   255
            Left            =   120
            TabIndex        =   96
            Top             =   5040
            Width           =   3735
         End
         Begin VB.CheckBox ckbConsultarLoteAposEnvio 
            Caption         =   "Consultar Lote Após Envio"
            Height          =   255
            Left            =   120
            TabIndex        =   95
            Top             =   4680
            Width           =   3735
         End
         Begin VB.CheckBox ckbMontarPathSchemas 
            Caption         =   "Montar automaticamente o Path dos Schemas"
            Height          =   255
            Left            =   120
            TabIndex        =   94
            Top             =   3600
            Width           =   3735
         End
         Begin VB.TextBox txtTimeOut 
            Alignment       =   1  'Right Justify
            Height          =   285
            Left            =   -71280
            TabIndex        =   86
            Text            =   "0"
            Top             =   720
            Width           =   825
         End
         Begin VB.CommandButton btnObterCertificados 
            Caption         =   "Obter Certificados"
            Height          =   375
            Left            =   -74640
            TabIndex        =   74
            Top             =   5160
            Width           =   1575
         End
         Begin VB.TextBox txtSchemaPath 
            Height          =   285
            Left            =   120
            TabIndex        =   47
            Top             =   4200
            Width           =   4215
         End
         Begin VB.CommandButton btnSelectSchema 
            Caption         =   "..."
            Height          =   260
            Left            =   4440
            TabIndex        =   46
            Top             =   4200
            Width           =   390
         End
         Begin VB.TextBox txtLogs 
            Height          =   285
            Left            =   120
            TabIndex        =   45
            Top             =   3120
            Width           =   4215
         End
         Begin VB.CommandButton btnSelectLog 
            Caption         =   "..."
            Height          =   260
            Left            =   4440
            TabIndex        =   44
            Top             =   3120
            Width           =   390
         End
         Begin VB.CheckBox ckbSalvarArqeResp 
            Caption         =   "Salvar Arquivos de Envio e Resposta"
            Height          =   255
            Left            =   120
            TabIndex        =   43
            Top             =   2520
            Width           =   3375
         End
         Begin VB.CheckBox chkRetirarAcentos 
            Caption         =   "Retirar Acentos dos XMLs enviados"
            Height          =   255
            Left            =   120
            TabIndex        =   42
            Top             =   2160
            Width           =   3375
         End
         Begin VB.ComboBox cmbFormaEmissao 
            Height          =   315
            ItemData        =   "FrmMain.frx":00FB
            Left            =   120
            List            =   "FrmMain.frx":011A
            Style           =   2  'Dropdown List
            TabIndex        =   41
            Top             =   1680
            Width           =   2295
         End
         Begin VB.TextBox txtFormatoAlerta 
            Height          =   285
            Left            =   120
            TabIndex        =   40
            Top             =   1080
            Width           =   4695
         End
         Begin VB.CheckBox chkExibirErroSchema 
            Caption         =   "Exibir Erro Schema"
            Height          =   255
            Left            =   120
            TabIndex        =   39
            Top             =   480
            Width           =   3375
         End
         Begin VB.Frame frmAmbiente 
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
            Left            =   -74760
            TabIndex        =   36
            Top             =   1200
            Width           =   4695
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
               Left            =   3000
               TabIndex        =   38
               Top             =   360
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
               Left            =   240
               TabIndex        =   37
               ToolTipText     =   "taProducao"
               Top             =   360
               Width           =   1095
            End
         End
         Begin VB.ComboBox cmbUFDestino 
            Height          =   315
            ItemData        =   "FrmMain.frx":0176
            Left            =   -74760
            List            =   "FrmMain.frx":01CB
            Style           =   2  'Dropdown List
            TabIndex        =   35
            Top             =   720
            Width           =   1095
         End
         Begin VB.ComboBox cmbSSlType 
            Height          =   315
            ItemData        =   "FrmMain.frx":023B
            Left            =   -73440
            List            =   "FrmMain.frx":0254
            Style           =   2  'Dropdown List
            TabIndex        =   34
            Top             =   720
            Width           =   1695
         End
         Begin VB.Frame frmProxy 
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
            Height          =   2415
            Left            =   -74760
            TabIndex        =   28
            Top             =   5880
            Width           =   4695
            Begin VB.TextBox txtProxyPorta 
               Alignment       =   1  'Right Justify
               Height          =   285
               Left            =   2880
               TabIndex        =   87
               Text            =   "0"
               Top             =   480
               Width           =   345
            End
            Begin VB.TextBox txtProxySenha 
               Height          =   285
               Left            =   120
               TabIndex        =   82
               Top             =   1680
               Width           =   3855
            End
            Begin VB.TextBox txtProxyUsuario 
               Height          =   285
               Left            =   120
               TabIndex        =   81
               Top             =   1080
               Width           =   3855
            End
            Begin VB.TextBox txtProxyServidor 
               Height          =   285
               Left            =   120
               TabIndex        =   29
               Top             =   480
               Width           =   2655
            End
            Begin MSComCtl2.UpDown nudProxyPorta 
               Height          =   285
               Left            =   3226
               TabIndex        =   92
               Top             =   480
               Width           =   255
               _ExtentX        =   450
               _ExtentY        =   503
               _Version        =   393216
               Value           =   5000
               BuddyControl    =   "txtProxyPorta"
               BuddyDispid     =   196715
               OrigLeft        =   3960
               OrigTop         =   720
               OrigRight       =   4215
               OrigBottom      =   975
               Max             =   99999
               SyncBuddy       =   -1  'True
               BuddyProperty   =   0
               Enabled         =   -1  'True
            End
            Begin VB.Label lblServidor 
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
               TabIndex        =   33
               Top             =   240
               Width           =   720
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
               Left            =   2880
               TabIndex        =   32
               Top             =   240
               Width           =   465
            End
            Begin VB.Label lblUsuario 
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
               TabIndex        =   31
               Top             =   840
               Width           =   720
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
               Index           =   0
               Left            =   120
               TabIndex        =   30
               Top             =   1440
               Width           =   525
            End
         End
         Begin VB.Frame frmRetEnvio 
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
            Height          =   1695
            Left            =   -74760
            TabIndex        =   20
            Top             =   4200
            Width           =   4695
            Begin VB.CheckBox chkAjustAut 
               Caption         =   "Ajustar Automaticamente prop. ""Aguardar"""
               Height          =   375
               Left            =   120
               TabIndex        =   24
               Top             =   240
               Width           =   4095
            End
            Begin VB.TextBox txtAguardar 
               Alignment       =   1  'Right Justify
               Height          =   285
               Left            =   120
               TabIndex        =   23
               Text            =   "0"
               Top             =   960
               Width           =   720
            End
            Begin VB.TextBox txtTentativas 
               Alignment       =   1  'Right Justify
               Height          =   285
               Left            =   1800
               TabIndex        =   22
               Text            =   "0"
               Top             =   960
               Width           =   840
            End
            Begin VB.TextBox txtIntervalo 
               Alignment       =   1  'Right Justify
               Height          =   285
               Left            =   3360
               TabIndex        =   21
               Text            =   "0"
               Top             =   960
               Width           =   705
            End
            Begin MSComCtl2.UpDown nudAguardar 
               Height          =   285
               Left            =   840
               TabIndex        =   89
               Top             =   960
               Width           =   255
               _ExtentX        =   450
               _ExtentY        =   503
               _Version        =   393216
               Value           =   5000
               BuddyControl    =   "txtAguardar"
               BuddyDispid     =   196725
               OrigLeft        =   3960
               OrigTop         =   720
               OrigRight       =   4215
               OrigBottom      =   975
               Max             =   99999
               SyncBuddy       =   -1  'True
               BuddyProperty   =   0
               Enabled         =   -1  'True
            End
            Begin MSComCtl2.UpDown nudTentativas 
               Height          =   285
               Left            =   2640
               TabIndex        =   90
               Top             =   960
               Width           =   255
               _ExtentX        =   450
               _ExtentY        =   503
               _Version        =   393216
               Value           =   5000
               BuddyControl    =   "txtTentativas"
               BuddyDispid     =   196726
               OrigLeft        =   3960
               OrigTop         =   720
               OrigRight       =   4215
               OrigBottom      =   975
               Max             =   99999
               SyncBuddy       =   -1  'True
               BuddyProperty   =   0
               Enabled         =   -1  'True
            End
            Begin MSComCtl2.UpDown nudIntervalo 
               Height          =   285
               Left            =   4080
               TabIndex        =   91
               Top             =   960
               Width           =   255
               _ExtentX        =   450
               _ExtentY        =   503
               _Version        =   393216
               Value           =   5000
               BuddyControl    =   "txtIntervalo"
               BuddyDispid     =   196727
               OrigLeft        =   3960
               OrigTop         =   720
               OrigRight       =   4215
               OrigBottom      =   975
               Max             =   99999
               SyncBuddy       =   -1  'True
               BuddyProperty   =   0
               Enabled         =   -1  'True
            End
            Begin VB.Label lblAguardar 
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
               TabIndex        =   27
               Top             =   720
               Width           =   795
            End
            Begin VB.Label lblTentativas 
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
               Left            =   1800
               TabIndex        =   26
               Top             =   720
               Width           =   915
            End
            Begin VB.Label lblIntervalo 
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
               Left            =   3360
               TabIndex        =   25
               Top             =   720
               Width           =   795
            End
         End
         Begin VB.CheckBox chkVisualizarMensagem 
            Caption         =   "Visualizar Mensagem"
            Height          =   255
            Left            =   -74760
            TabIndex        =   19
            Top             =   2040
            Width           =   1815
         End
         Begin VB.CheckBox chkSalvarSOAP 
            Caption         =   "Salvar envelope SOAP"
            Height          =   255
            Left            =   -72240
            TabIndex        =   18
            Top             =   2040
            Width           =   2175
         End
         Begin VB.Frame frmCertificados 
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
            Left            =   -74640
            TabIndex        =   10
            Top             =   2400
            Width           =   4215
            Begin VB.TextBox txtDadosPFX 
               Height          =   285
               Left            =   120
               TabIndex        =   76
               Top             =   1080
               Width           =   3555
            End
            Begin VB.CommandButton btnDadosPFX 
               Caption         =   "..."
               Height          =   260
               Left            =   3720
               TabIndex        =   75
               Top             =   1080
               Width           =   390
            End
            Begin VB.CommandButton btnSelecionarCertificado 
               Caption         =   "..."
               Height          =   260
               Left            =   3720
               TabIndex        =   14
               Top             =   479
               Width           =   390
            End
            Begin VB.TextBox txtCertPath 
               Height          =   285
               Left            =   120
               TabIndex        =   13
               Top             =   480
               Width           =   3555
            End
            Begin VB.TextBox txtCertPassword 
               Height          =   285
               IMEMode         =   3  'DISABLE
               Left            =   120
               PasswordChar    =   "*"
               TabIndex        =   12
               Top             =   1680
               Width           =   4035
            End
            Begin VB.TextBox txtCertNumero 
               Height          =   285
               Left            =   120
               TabIndex        =   11
               Top             =   2280
               Width           =   4035
            End
            Begin VB.Label lblDadosPFX 
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
               TabIndex        =   77
               Top             =   840
               Width           =   870
            End
            Begin VB.Label lblCaminho 
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
               TabIndex        =   17
               Top             =   240
               Width           =   735
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
               Index           =   1
               Left            =   120
               TabIndex        =   16
               Top             =   1440
               Width           =   525
            End
            Begin VB.Label lblNumeroSerie 
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
               TabIndex        =   15
               Top             =   2040
               Width           =   1395
            End
         End
         Begin VB.ComboBox cmbXmlSign 
            Height          =   315
            ItemData        =   "FrmMain.frx":02A0
            Left            =   -74640
            List            =   "FrmMain.frx":02B3
            Style           =   2  'Dropdown List
            TabIndex        =   9
            Top             =   1920
            Width           =   2175
         End
         Begin VB.ComboBox cmbHttp 
            Height          =   315
            ItemData        =   "FrmMain.frx":02ED
            Left            =   -74640
            List            =   "FrmMain.frx":02FD
            Style           =   2  'Dropdown List
            TabIndex        =   8
            Top             =   1320
            Width           =   2175
         End
         Begin VB.ComboBox cmbCrypt 
            Height          =   315
            ItemData        =   "FrmMain.frx":0332
            Left            =   -74640
            List            =   "FrmMain.frx":0342
            Style           =   2  'Dropdown List
            TabIndex        =   7
            Top             =   720
            Width           =   2175
         End
         Begin MSComCtl2.UpDown nudTimeOut 
            Height          =   285
            Left            =   -70440
            TabIndex        =   88
            Top             =   720
            Width           =   255
            _ExtentX        =   450
            _ExtentY        =   503
            _Version        =   393216
            Value           =   5000
            BuddyControl    =   "txtTimeOut"
            BuddyDispid     =   196698
            OrigLeft        =   3960
            OrigTop         =   720
            OrigRight       =   4215
            OrigBottom      =   975
            Max             =   99999
            SyncBuddy       =   -1  'True
            BuddyProperty   =   0
            Enabled         =   -1  'True
         End
         Begin VB.Label Label27 
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
            Left            =   -72360
            TabIndex        =   142
            Top             =   4080
            Width           =   510
         End
         Begin VB.Label Label26 
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
            Left            =   -74880
            TabIndex        =   140
            Top             =   4080
            Width           =   1185
         End
         Begin VB.Label Label25 
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
            Height          =   255
            Left            =   -74880
            TabIndex        =   138
            Top             =   4800
            Width           =   1215
         End
         Begin VB.Label Label24 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "CNPJ Prefeitura"
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
            Left            =   -72360
            TabIndex        =   136
            Top             =   5520
            Width           =   1305
         End
         Begin VB.Label Label23 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "Código Cidade"
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
            TabIndex        =   134
            Top             =   5520
            Width           =   1185
         End
         Begin VB.Label Label21 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "Numero"
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
            Left            =   -71160
            TabIndex        =   132
            Top             =   3360
            Width           =   660
         End
         Begin VB.Label Label20 
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
            Left            =   -74880
            TabIndex        =   130
            Top             =   3360
            Width           =   975
         End
         Begin VB.Label Label19 
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
            Left            =   -72360
            TabIndex        =   128
            Top             =   2640
            Width           =   300
         End
         Begin VB.Label Label18 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "Fone"
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
            TabIndex        =   126
            Top             =   2640
            Width           =   405
         End
         Begin VB.Label Label17 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "Fantasia"
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
            TabIndex        =   124
            Top             =   1920
            Width           =   720
         End
         Begin VB.Label Label16 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "Razão Social"
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
            TabIndex        =   122
            Top             =   1200
            Width           =   1065
         End
         Begin VB.Label Label15 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "Inscrição Municpal"
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
            Left            =   -72360
            TabIndex        =   120
            Top             =   480
            Width           =   1575
         End
         Begin VB.Label Label14 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "CNPJ"
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
            TabIndex        =   118
            Top             =   480
            Width           =   405
         End
         Begin VB.Label lblPastaArqNFSe 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "Pasta Arquivos NFSe"
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
            TabIndex        =   116
            Top             =   2220
            Width           =   1725
         End
         Begin VB.Label Label6 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "Chave de Autorização"
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
            Left            =   -72360
            TabIndex        =   108
            Top             =   3600
            Width           =   1845
         End
         Begin VB.Label Label5 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "Chave de Acesso"
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
            TabIndex        =   106
            Top             =   3600
            Width           =   1425
         End
         Begin VB.Label Label4 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "Frase Secreta"
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
            TabIndex        =   104
            Top             =   3000
            Width           =   1170
         End
         Begin VB.Label Label3 
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
            Left            =   -72360
            TabIndex        =   102
            Top             =   2400
            Width           =   525
         End
         Begin VB.Label Label2 
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
            Left            =   -74760
            TabIndex        =   100
            Top             =   2400
            Width           =   645
         End
         Begin VB.Label Label1 
            Caption         =   "Layout NFSe"
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
            Left            =   120
            TabIndex        =   98
            Top             =   5400
            Width           =   4575
         End
         Begin VB.Label lblPastaSchemas 
            Caption         =   "Pasta dos Schemas do Provedor"
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
            Left            =   120
            TabIndex        =   57
            Top             =   3960
            Width           =   4575
         End
         Begin VB.Label lblPastaLogs 
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
            Height          =   255
            Left            =   120
            TabIndex        =   56
            Top             =   2880
            Width           =   4575
         End
         Begin VB.Label lblFormaEmissao 
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
            Height          =   255
            Left            =   120
            TabIndex        =   55
            Top             =   1440
            Width           =   4575
         End
         Begin VB.Label lblFormatoAlerta 
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
            Height          =   255
            Left            =   120
            TabIndex        =   54
            Top             =   840
            Width           =   4575
         End
         Begin VB.Label lblUFDestino 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "UF Destino"
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
            TabIndex        =   53
            Top             =   480
            Width           =   900
         End
         Begin VB.Label lblSSLType 
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
            Left            =   -73080
            TabIndex        =   52
            Top             =   480
            Width           =   765
         End
         Begin VB.Label lblTimeOut 
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
            Index           =   1
            Left            =   -71280
            TabIndex        =   51
            Top             =   480
            Width           =   765
         End
         Begin VB.Label lblXMLSignLib 
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
            Left            =   -74640
            TabIndex        =   50
            Top             =   1680
            Width           =   915
         End
         Begin VB.Label lblHttpLib 
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
            Left            =   -74640
            TabIndex        =   49
            Top             =   1080
            Width           =   615
         End
         Begin VB.Label lblCryptLib 
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
            Left            =   -74640
            TabIndex        =   48
            Top             =   480
            Width           =   705
         End
      End
      Begin VB.Label Label30 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Pasta PDF"
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
         TabIndex        =   150
         Top             =   2400
         Width           =   840
      End
      Begin VB.Label Label29 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Nome da Prefeitura"
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
         TabIndex        =   147
         Top             =   1800
         Width           =   1635
      End
      Begin VB.Label Label28 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Logomarca do Prestador de Serviço"
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
         TabIndex        =   145
         Top             =   1080
         Width           =   3015
      End
      Begin VB.Label lblLogomarca 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Logomarca da Prefeitura"
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
         TabIndex        =   78
         Top             =   480
         Width           =   2100
      End
      Begin VB.Label Label22 
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
         Left            =   -74400
         TabIndex        =   72
         Top             =   5520
         Width           =   930
      End
      Begin VB.Label Label13 
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
         Left            =   -74400
         TabIndex        =   70
         Top             =   4800
         Width           =   690
      End
   End
End
Attribute VB_Name = "FrmMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim nfse As ACBrNFSe

Private Sub Form_Load()
    
    Dim CodigoMunicipio As ACBrCodigoMunicipioNFSe
    
    Set CodigoMunicipio = New ACBrCodigoMunicipioNFSe
        
    CodigoMunicipio.NenhumCidade cmbCidadeEmitente
    CodigoMunicipio.Rondonia cmbCidadeEmitente
    CodigoMunicipio.Acre cmbCidadeEmitente
    CodigoMunicipio.Amazonas cmbCidadeEmitente
    CodigoMunicipio.Roraima cmbCidadeEmitente
    CodigoMunicipio.Para cmbCidadeEmitente
    CodigoMunicipio.Tocantins cmbCidadeEmitente
    CodigoMunicipio.Maranhao cmbCidadeEmitente
    CodigoMunicipio.Piaui cmbCidadeEmitente
    CodigoMunicipio.Ceara cmbCidadeEmitente
    CodigoMunicipio.RioGrandedoNorte cmbCidadeEmitente
    CodigoMunicipio.Paraiba cmbCidadeEmitente
    CodigoMunicipio.Pernambuco cmbCidadeEmitente
    CodigoMunicipio.Alagoas cmbCidadeEmitente
    CodigoMunicipio.Sergipe cmbCidadeEmitente
    CodigoMunicipio.Bahia cmbCidadeEmitente
    CodigoMunicipio.MinasGerais cmbCidadeEmitente
    CodigoMunicipio.EspiritoSanto cmbCidadeEmitente
    CodigoMunicipio.RiodeJaneiro cmbCidadeEmitente
    CodigoMunicipio.SaoPaulo cmbCidadeEmitente
    CodigoMunicipio.Parana cmbCidadeEmitente
    CodigoMunicipio.SantaCatarina cmbCidadeEmitente
    CodigoMunicipio.RioGrandedoSul cmbCidadeEmitente
    CodigoMunicipio.MatoGrossodoSul cmbCidadeEmitente
    CodigoMunicipio.MatoGrosso cmbCidadeEmitente
    CodigoMunicipio.Goias cmbCidadeEmitente
    CodigoMunicipio.Brasilia cmbCidadeEmitente
         
    cmbUFDestino.Text = "SP"
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
    
    Set nfse = CreateNFSe(IniPath)
    
    nfse.ConfigGravarValor SESSAO_PRINCIPAL, "LogNivel", NivelLog.logParanoico
    nfse.ConfigGravarValor SESSAO_PRINCIPAL, "LogPath", LogPath
    nfse.ConfigGravar
    
    LoadConfig
End Sub

Private Sub btnArqNFSe_Click()
    txtArqNFSe.Text = BrowseFolder("Selecione a pasta Arquivos NFSe")
End Sub

Private Sub btnDadosPFX_Click()
    On Error Resume Next
    CommonDialog1.DialogTitle = "Selecione o certificado"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivos PFX (*.pfx)|*.pfx|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.ShowOpen
            
    If Err Then Exit Sub
    
    txtDadosPFX.Text = CommonDialog1.FileName
End Sub

Private Sub btnLogomarcaPrefeitura_Click()
    On Error Resume Next
    CommonDialog1.DialogTitle = "Selecione a Logomarca"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivos Imagem (*.bmp, *.jpeg, *.png)|*.bmp; *.jpeg; *.png|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.ShowOpen
            
    If Err Then Exit Sub
    
    txtLogomarcaPrefeitura.Text = CommonDialog1.FileName
End Sub

Private Sub btnLogoMarcaPrestadorServico_Click()
    On Error Resume Next
    CommonDialog1.DialogTitle = "Selecione a Logomarca"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivos Imagem (*.bmp, *.jpeg, *.png)|*.bmp; *.jpeg; *.png|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.ShowOpen
            
    If Err Then Exit Sub
    
    txtLogoMarcaPrestadorServico.Text = CommonDialog1.FileName
End Sub

Private Sub btnPastaPDF_Click()
    txtPastaPDF.Text = BrowseFolder("Selecione a pasta dos PDFs")
End Sub

Private Sub btnArqEvento_Click()
    txtArqEvento.Text = BrowseFolder("Selecione a pasta Arquivos Eventos")
End Sub

Private Sub btnArqInu_Click()
    txtArqInu.Text = BrowseFolder("Selecione a pasta Arquivos Inutilização")
End Sub

Private Sub btnArqNFe_Click()
    txtArqNFe.Text = BrowseFolder("Selecione a pasta Arquivos NFe")
End Sub

Private Sub btnCarregarConfiguracoes_Click()
    LoadConfig
End Sub

Private Sub CheckNFSeLista()
    
    Dim xml As Boolean
    
    answer = MsgBox("Limpar Lista ?", vbExclamation + vbYesNo, "Add Confirm")
    If answer = vbYes Then
    nfse.LimparLista
    End If
    
    If xml Then
    
    CommonDialog1.DialogTitle = "Selecione o arquivo ini"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivo xml NFSe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
            
    If CommonDialog1.FileName = vbNullString Then Exit Sub
    
    nfse.CarregarXML CommonDialog1.FileName
    
    Else
    
    CommonDialog1.DialogTitle = "Selecione o arquivo ini"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivo ini NFSe (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
            
    If CommonDialog1.FileName = vbNullString Then Exit Sub
    
    nfse.CarregarINI CommonDialog1.FileName
    End If
    
End Sub

Private Sub btnObterCertificados_Click()
    
    On Error GoTo Erro:
    nfse.ObterCertificados
Erro:
    MsgBox Err.Description
    
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

Private Sub btnSelectLog_Click()
    txtLogs.Text = BrowseFolder("Selecione a pasta dos Logs")
End Sub

Private Sub btnSelectSchema_Click()
    txtSchemaPath.Text = BrowseFolder("Selecione a pasta dos Schemas")
End Sub

Private Sub cmdSalvar_Click()
    SalvarConfig
End Sub

Private Sub btnEmitirNota_Click()
    
    On Error GoTo Erro:
    
    Dim ret, aLote As String
    
    CheckNFSeLista
    
    aLote = InputBox("Número do Lote", "Emitir", "1")
    ret = nfse.Emitir(aLote, 0, False)
    rtbRespostas.Text = ret
    
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnGerarEnviarLoteAssincrono_Click()
    
    On Error GoTo Erro:
    
    Dim ret, aLote As String
    
    CheckNFSeLista
    
    aLote = InputBox("Número do Lote", "Enviar Assíncrono", "1")
    ret = nfse.Emitir(aLote, 2, False)
    rtbRespostas.Text = ret
    
Erro:
    MsgBox Err.Description
    
End Sub

Private Sub btnGerarEnviarLoteSincrono_Click()
    
    On Error GoTo Erro:
    
    Dim ret, aLote As String
    
    CheckNFSeLista
    
    aLote = InputBox("Número do Lote", "Enviar Síncrono", "1")
    ret = nfse.Emitir(aLote, 3, False)
    rtbRespostas.Text = ret
    
Erro:
    MsgBox Err.Description
    
End Sub

Private Sub btnEnviarRPS_Click()
    
    On Error GoTo Erro:
    
    Dim ret, aLote As String
    
    CheckNFSeLista
    
    aLote = InputBox("Número do Lote", "Enviar um RPS", "1")
    ret = nfse.Emitir(aLote, 4, False)
    rtbRespostas.Text = ret
    
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnEnviarEmail_Click()
    
    On Error GoTo Erro:
    
    CommonDialog1.DialogTitle = "Selecione o arquivo xml"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivo Xml NFSe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
            
    If CommonDialog1.FileName = vbNullString Then Exit Sub
    
    Dim destinatario As String
    destinatario = InputBox("Digite o email do destinatario", "Envio email", vbNullString)
    
    If destinatario = vbNullString Then Exit Sub
    
    nfse.EnviarEmail destinatario, CommonDialog1.FileName, True, txtAssuntoEmail.Text, txtMensagemEmail.Text
    
Erro:
    MsgBox Err.Description
    
End Sub

Private Sub btnSubstituirNFSe_Click()

    On Error GoTo Erro:
    
    Dim ret, numeroNFSe, serieNFSe, codCancelamento, motivoCancelamento, numeroLote, codVerificacao As String
    
    numeroNFSe = InputBox("Informe o Número da NFSe", "Substituir NFSe", "")
    serieNFSe = InputBox("Informe a Série da NFSe", "Substituir NFSe", "")
    codCancelamento = InputBox("Infome o Código de Cancelamento", "Substituir NFSe", "")
    motivoCancelamento = InputBox("Informe o Motivo do Cancelamento", "Substituir NFSe", "")
    numeroLote = InputBox("Informe o Número do Lote", "Substituir NFSe", "")
    codVerificacao = InputBox("Informe o Código de Verificação", "Substituir NFSe", "")
    
    ret = nfse.SubstituirNFSe(numeroNFSe, serieNFSe, codCancelamento, motivoCancelamento, numeroLote, codVerificacao)
    rtbRespostas.Text = ret
    
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnImprimirDANFSe_Click()
    
    On Error GoTo Erro:
    
    CommonDialog1.DialogTitle = "Selecione o arquivo xml"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivo Xml DANFSE (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
            
    If CommonDialog1.FileName = vbNullString Then Exit Sub
    
    nfse.LimparLista
    nfse.CarregarXML CommonDialog1.FileName
    nfse.ImprimirPDF
    
Erro:
    MsgBox Err.Description
    
End Sub

Private Sub btnImprimirNFSe_Click()
    
    On Error GoTo Erro:
    
    CommonDialog1.DialogTitle = "Selecione o arquivo xml"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivo Xml DANFSE (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
            
    If CommonDialog1.FileName = vbNullString Then Exit Sub
    
    nfse.LimparLista
    nfse.CarregarXML CommonDialog1.FileName
    nfse.Imprimir
    
Erro:
    MsgBox Err.Description
    
End Sub

Private Sub btnLinkNFSe_Click()

    On Error GoTo Erro:
    
    Dim ret, numeroNFSe, codVerificacao, chaveAcesso, valorServico As String
    
    numeroNFSe = InputBox("Informe o Número NFSe", "Link NFSe", "")
    codVerificacao = InputBox("Informe o Código de Verificação", "Link NFSe", "")
    chaveAcesso = InputBox("Informe a Chave de Acesso", "Link NFSe", "")
    valorServico = InputBox("Informe o Valor do Serviço", "Link NFSe", "")
    
    ret = nfse.LinkNFSE(numeroNFSe, codVerificacao, chaveAcesso, valorServico)
    rtbRespostas.Text = ret
    
Erro:
    MsgBox Err.Description
    
End Sub

Private Sub btnGerarLoteRPS_Click()

    On Error GoTo Erro:
    
    Dim ret, aLote As String
    Dim qtdMaximaRPS As Long
    
    aLote = InputBox("Número do Lote", "Gerar Lote RPS", "1")
    qtdMaximaRPS = InputBox("Quantidade Máxima RPS", "Gerar Lote RPS", 1)
    
    ret = nfse.GerarLote(aLote, qtdMaximaRPS, 0)
    rtbRespostas.Text = ret
    
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnGerarToken_Click()

    On Error GoTo Erro:
    
    nfse.GerarToken
    
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnCarregarXMLNFse_Click()

    On Error GoTo Erro:
    
    CheckNFSeLista
    
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnObterXMLNFSe_Click()

    On Error GoTo Erro:
    
    Dim ret As String
    
    ret = nfse.ObterXml(0)
    rtbRespostas.Text = ret

Erro:
    MsgBox Err.Description

End Sub

Private Sub btnGravarXMLNFSe_Click()

    On Error GoTo Erro:
    
    nfse.GravarXml (0)

Erro:
    MsgBox Err.Description

End Sub

Private Sub btnConsultarSituacaoLote_Click()

    On Error GoTo Erro:
    
    Dim ret, protocolo, aLote As String
    
    protocolo = InputBox("Número do Protocolo", "Consultar Situação", "")
    aLote = InputBox("Número do Lote", "Consultar Situação", "")
    
    ret = nfse.ConsultarSitucao(protocolo, aLote)
    rtbRespostas.Text = ret
    
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnConsultarNFSePorNumero_Click()

    On Error GoTo Erro:
    
    Dim ret, protocolo As String
    Dim pagina As Long
    
    protocolo = InputBox("Número do Protocolo", "Consultar Por Número", "")
    pagina = InputBox("Informe a Página", "Consultar Por Número", 1)
    
    ret = nfse.ConsultarNFSePorNumero(protocolo, pagina)
    rtbRespostas.Text = ret
    
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnConsultarNFSeGenerico_Click()

    On Error GoTo Erro:
    
    Dim ret As String
    
    nfse.LimparLista
    
    CommonDialog1.DialogTitle = "Selecione o arquivo ini"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivo ini NFSe (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
            
    If CommonDialog1.FileName = vbNullString Then Exit Sub
    
    nfse.CarregarINI CommonDialog1.FileName
    
    ret = nfse.ConsultarNFSeGenerico(CommonDialog1.FileName)
    rtbRespostas.Text = ret
    
Erro:
    MsgBox Err.Description
    
End Sub

Private Sub btnConsultarLoteRPS_Click()

    On Error GoTo Erro:
    
    Dim ret, protocolo, numLote As String
    
    protocolo = InputBox("Número Protocolo", "Consultar Lote Por RPS", "")
    numLote = InputBox("Número do Lote", "Consultar Lote Por RPS", 1)
    
    ret = nfse.ConsultarLoteRps(protocolo, numLote)
    rtbRespostas.Text = ret
    
Erro:
    MsgBox Err.Description
    
End Sub

Private Sub btnConsultarNFSePorPeriodo_Click()
    
    On Error GoTo Erro:
    
    Dim ret, numLote As String
    Dim dataInicial, dataFinal As Date
    Dim pagina, tipoPeriodo As Long
    
    dataInicial = InputBox("Informe a Data Inicial", "Consultar NFSe Por Periodo", "01/MM/AAAA")
    dataFinal = InputBox("Informe a Data Final", "Consultar NFSe Por Periodo", "01/MM/AAAA")
    pagina = InputBox("Informe a Pagina", "Consultar NFSe Por Periodo", 1)
    numLote = InputBox("Informe o Número do Lote", "Consultar NFSe Por Periodo", "")
    tipoPeriodo = InputBox("Informe o Tipo do Periodo", "Consultar NFSe Por Periodo", 0)
    
    ret = nfse.ConsultarNFSePorPeriodo(dataInicial, dataFinal, pagina, numLote, tipoPeriodo)
    rtbRespostas.Text = ret
    
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnConsultarNFSePorRPS_Click()

    On Error GoTo Erro:
    
    Dim ret, numeroRPS, serie, tipo, codVerificacao As String
    
    numeroRPS = InputBox("Número do Protocolo", "Consultar NFSe Por RPS", "")
    serie = InputBox("Série", "Consultar NFSe Por RPS", "")
    tipo = InputBox("Tipo", "Consultar NFSe Por RPS", "")
    codVerificacao = InputBox("Código de Verificação", "Consultar NFSe Por RPS", "")
    
    ret = nfse.ConsultarNFSePorRps(numeroRPS, serie, tipo, codVerificacao)
    rtbRespostas.Text = ret
    
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnConsultarNFSePorFaixa_Click()
    
    On Error GoTo Erro:
    
    Dim ret, numeroInicial, numeroFinal As String
    Dim pagina As Long
    
    numeroInicial = InputBox("Informe o Número Inicial", "Consultar NFSe Por Faixa", "")
    numeroFinal = InputBox("Informe o Número Final", "Consultar NFSe Por Faixa", "")
    pagina = InputBox("Informe a Página", "Consultar NFSe Por Faixa", 1)
    
    ret = nfse.ConsultarNFSePorFaixa(numeroInicial, numeroFinal, pagina)
    rtbRespostas.Text = ret
    
Erro:
    MsgBox Err.Description
    
End Sub

Private Sub btnConsultarNFSeServicoPrestadoPorNumero_Click()

    On Error GoTo Erro:
    
    Dim ret, numero  As String
    Dim pagina, tipoPeriodo As Long
    Dim dataInicial, dataFinal As Date
    
    numero = InputBox("Informe o Número", "Consultar NFSe Serviço Prestado Por Numero", "")
    pagina = InputBox("Informe a Página", "Consultar NFSe Serviço Prestado Por Numero", 1)
    dataInicial = InputBox("Informe a Data Inicial", "Consultar NFSe Serviço Prestado Por Numero", "01/MM/AAAA")
    dataFinal = InputBox("Informe a Data Final", "Consultar NFSe Serviço Prestado Por Numero", "01/MM/AAAA")
    tipoPeriodo = InputBox("Informe o Tipo Periodo", "Consultar NFSe Serviço Prestado Por Numero", 0)
    
    ret = nfse.ConsultarNFSeServicoPrestadoPorNumero(numero, pagina, dataInicial, dataFinal, tipoPeriodo)
    rtbRespostas.Text = ret
    
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnConsultarNFSeSevicoPrestadoPorTomador_Click()

    On Error GoTo Erro:
    
    Dim ret, cnpj, inscMun As String
    Dim pagina, tipoPeriodo As Long
    Dim dataInicial, dataFinal As Date
    
    cnpj = InputBox("Informe o CNPJ", "Consultar NFSe Serviço Prestado Por Tomador", "")
    inscMun = InputBox("Informe a Inscrição Municipal", "Consultar NFSe Serviço Prestado Por Tomador", "")
    pagina = InputBox("Informe a Página", "Consultar NFSe Serviço Prestado Por Tomador", 1)
    dataInicial = InputBox("Informe a Data Inicial", "Consultar NFSe Serviço Prestado Por Tomador", "01/MM/AAAA")
    dataFinal = InputBox("Informe a Data Final", "Consultar NFSe Serviço Prestado Por Tomador", "01/MM/AAAA")
    tipoPeriodo = InputBox("Informe o Tipo de Periodo", "Consultar NFSe Serviço Prestado Por Tomador", 0)
    
    ret = nfse.ConsultarNFSeServicoPrestadoPorTomador(cnpj, inscMun, pagina, dataInicial, dataFinal, tipoPeriodo)
    rtbRespostas.Text = ret
    
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnConsultarNFSeServicoPrestadoPorPeriodo_Click()

    On Error GoTo Erro:
    
    Dim ret As String
    Dim dataInicial, dataFinal As Date
    Dim pagina, tipoPeriodo As Long
    
    dataInicial = InputBox("Informe a Data Inicial", "Consultar NFSe Serviço Prestado Por Periodo", "01/MM/AAAA")
    dataFinal = InputBox("Informe a Data Final", "Consultar NFSe Serviço Prestado Por Periodo", "01/MM/AAAA")
    pagina = InputBox("Informe a Pagina", "Consultar NFSe Serviço Prestado Por Periodo", "")
    tipoPeriodo = InputBox("Informe o Tipo Periodo", "Consultar NFSe Serviço Prestado Por Periodo", "")
    
    ret = nfse.ConsultarNFSeServicoPrestadoPorPeriodo(dataFinal, dataFinal, pagina, tipoPeriodo)
    rtbRespostas.Text = ret
    
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnConsultarNFSeServicoPrestadoPorIntermediario_Click()

    On Error GoTo Erro:
    
    Dim ret, cnpj, inscMun As String
    Dim dataInicial, dataFinal As Date
    Dim pagina, tipoPeriodo As Long
    
    cnpj = InputBox("Informe o CNPJ", "Consultar NFSe Serviço Prestado Por Intermediário", "")
    inscMun = InputBox("Informe a Inscrição Municipal", "Consultar NFSe Serviço Prestado Por Intermediário", "")
    pagina = InputBox("Informe a Página", "Consultar NFSe Serviço Prestado Por Intermediário", 1)
    dataInicial = InputBox("Informe a Data Inicial", "Consultar NFSe Serviço Prestado Por Intermediário", "01/MM/AAAA")
    dataFinal = InputBox("Informe a Data Final", "Consultar NFSe Serviço Prestado Por Intermediário", "01/MM/AAAA")
    tipoPeriodo = InputBox("Informe o Tipo de Periodo", "Consultar NFSe Serviço Prestado Por Intermediário", 0)
    
    ret = nfse.ConsultarNFSeServicoPrestadoPorIntermediario(cnpj, inscMun, pagina, dataInicial, dataFinal, tipoPeriodo)
    rtbRespostas.Text = ret
    
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnConsultarNFSeServicoTomadoPorNumero_Click()

    On Error GoTo Erro:
    
    Dim ret, numero As String
    Dim dataInicial, dataFinal As Date
    Dim pagina, tipoPeriodo As Long
    
    numero = InputBox("Informe o Número", "Consultar NFSe Serviço Tomado Por Numero", "")
    pagina = InputBox("Informe a Página", "Consultar NFSe Serviço Tomado Por Numero", 1)
    dataInicial = InputBox("Informe a Data Inicial", "Consultar NFSe Serviço Tomado Por Numero", "01/MM/AAAA")
    dataFinal = InputBox("Informe a Data Final", "Consultar NFSe Serviço Tomado Por Numero", "01/MM/AAAA")
    tipoPeriodo = InputBox("Informe o Tipo de Periodo", "Consultar NFSe Serviço Tomado Por Numero", 0)
    
    ret = nfse.ConsultarNFSeServicoTomadoPorNumero(numero, pagina, dataInicial, dataFinal, tipoPeriodo)
    rtbRespostas.Text = ret
     
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnConsultarNFSeServicoTomadoPorPrestador_Click()

    On Error GoTo Erro:
    
    Dim ret, cnpj, inscMun As String
    Dim pagina, tipoPeriodo As Long
    Dim dataInicial, dataFinal As Date
    
    cnpj = InputBox("Informe o CNPJ", "Consultar NFSe Serviço Tomado Por Prestador", "")
    inscMun = InputBox("Informe a Inscrição Municipal", "Consultar NFSe Serviço Tomado Por Prestador", "")
    pagina = InputBox("Informe a Página", "Consultar NFSe Serviço Tomado Por Prestador", 1)
    dataInicial = InputBox("Informe a Data Inicial", "Consultar NFSe Serviço Tomado Por Prestador", "01/MM/AAAA")
    dataFinal = InputBox("Informe a Data Final", "Consultar NFSe Serviço Tomado Por Prestador", "01/MM/AAAA")
    tipoPeriodo = InputBox("Informe o Tipo Periodo", "Consultar NFSe Serviço Tomado Por Prestador", 0)
    
    ret = nfse.ConsultarNFSeServicoTomadoPorPrestador(cnpj, inscMun, pagina, dataInicial, dataFinal, tipoPeriodo)
    rtbRespostas.Text = ret
    
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnConsultarNFSeServicoTomadoPorTomador_Click()

    On Error GoTo Erro:
    
    Dim ret, cnpj, inscMun As String
    Dim pagina, tipoPeriodo As Long
    Dim dataInicial, dataFinal As Date
    
    cnpj = InputBox("Informe o CNPJ", "Consultar NFSe Serviço Tomado Por Tomador", "")
    inscMun = InputBox("Informe a Inscrição Municipal", "Consultar NFSe Serviço Tomado Por Tomador", "")
    pagina = InputBox("Informe a Página", "Consultar NFSe Serviço Tomado Por Tomador", 1)
    dataInicial = InputBox("Informe a Data Inicial", "Consultar NFSe Serviço Tomado Por Tomador", "01/MM/AAAA")
    dataFinal = InputBox("Informe da Data Final", "Consultar NFSe Serviço Tomado Por Tomador", "01/MM/AAAA")
    tipoPeriodo = InputBox("Informe o Tipo Periodo", "Consultar NFSe Serviço Tomado Por Tomador", 0)
    
    ret = nfse.ConsultarNFSeServicoTomadoPorTomador(cnpj, inscMun, pagina, dataInicial, dataFinal, tipoPeriodo)
    rtbRespostas.Text = ret
    
Erro:
    MsgBox Err.Description
    
End Sub

Private Sub btnConsultarNFSeServicoTomadoPorPeriodo_Click()

    On Error GoTo Erro:
    
    Dim ret As String
    Dim periodo, tipoPeriodo As Long
    Dim dataInicial, dataFinal As Date
    
    dataInicial = InputBox("Informe a Data Inicial", "Consultar NFSe Serviço Tomado Por Periodo", "01/MM/AAAA")
    dataFinal = InputBox("Informe a Data Final", "Consultar NFSe Serviço Tomado Por Periodo", "01/MM/AAAA")
    pagina = InputBox("Informe o Periodo", "Consultar NFSe Serviço Tomado Por Periodo", 1)
    tipoPeriodo = InputBox("Informe o Tipo Periodo", "Consultar NFSe Serviço Tomado Por Periodo", 0)
    
    ret = nfse.ConsultarNFSeServicoTomadoPorPeriodo(dataInicial, dataFinal, pagina, tipoPeriodo)
    rtbRespostas.Text = ret
    
Erro:
    MsgBox Err.Description
    
End Sub

Private Sub btnConsultarNFSeServicoTomadoPorIntermediario_Click()

    On Error GoTo Erro:
    
    Dim ret As String
    Dim cnpj, inscMun As String
    Dim pagina, tipoPeriodo As Long
    Dim dataInicial, dataFinal As Date
    
    cnpj = InputBox("Informe o CNPJ", "Consultar NFSe Serviço Tomado Por Intermediário", "")
    inscMun = InputBox("Informe a Inscrição Municipal", "Consultar NFSe Serviço Tomado Por Intermediário", "")
    pagina = InputBox("Informe a Página", "Consultar NFSe Serviço Tomado Por Intermediário", 1)
    dataInicial = InputBox("Informe a Data Inicial", "Consultar NFSe Serviço Tomado Por Intermediário", "01/MM/AAAA")
    dataFinal = InputBox("Informe a Data Final", "Consultar NFSe Serviço Tomado Por Intermediário", "01/MM/AAAA")
    tipoPeriodo = InputBox("Informe o Tipo Periodo", "Consultar NFSe Serviço Tomado Por Intermediário", 0)
    
    ret = nfse.ConsultarNFSeServicoTomadoPorIntermediario(cnpj, inscMun, pagina, dataInicial, dataFinal, tipoPeriodo)
    rtbRespostas.Text = ret
    
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnCancelarNFSe_Click()

    On Error GoTo Erro:
    
    Dim ret As String
    
    nfse.LimparLista
    
    CommonDialog1.DialogTitle = "Selecione o arquivo ini"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivo ini NFSe (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
            
    If CommonDialog1.FileName = vbNullString Then Exit Sub
    
    nfse.CarregarINI CommonDialog1.FileName
    
    ret = nfse.Cancelar(CommonDialog1.FileName)
    rtbRespostas.Text = ret
    
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

Private Sub LoadConfig()
    Dim buffer As String
    Dim bufferLen As Long
    
    nfse.ConfigLer
    
    'Geral
    chkExibirErroSchema.Value = CLng(nfse.ConfigLerValor(SESSAO_NFSe, "ExibirErroSchema"))
    txtFormatoAlerta.Text = nfse.ConfigLerValor(SESSAO_NFSe, "FormatoAlerta")
    cmbFormaEmissao.ListIndex = CLng(nfse.ConfigLerValor(SESSAO_NFSe, "FormaEmissao"))
    chkRetirarAcentos.Value = CLng(nfse.ConfigLerValor(SESSAO_NFSe, "RetirarAcentos"))
    ckbSalvarArqeResp.Value = CLng(nfse.ConfigLerValor(SESSAO_NFSe, "SalvarGer"))
    txtArqNFSe.Text = nfse.ConfigLerValor(SESSAO_NFSe, "PathSalvar")
    ckbMontarPathSchemas.Value = CLng(nfse.ConfigLerValor(SESSAO_NFSe, "MontarPathSchema"))
    txtSchemaPath.Text = nfse.ConfigLerValor(SESSAO_NFSe, "PathSchemas")
    ckbConsultarLoteAposEnvio.Value = CLng(nfse.ConfigLerValor(SESSAO_NFSe, "ConsultaLoteAposEnvio"))
    ckbConsultarAposCancelar.Value = CLng(nfse.ConfigLerValor(SESSAO_NFSe, "ConsultaAposCancelar"))
    cmbLayoutNFSe.ListIndex = CLng(nfse.ConfigLerValor(SESSAO_NFSe, "LayoutNFSe"))
    
    'WebSevice
    cmbUFDestino.Text = nfse.ConfigLerValor(SESSAO_DFE, "UF")
    cmbSSlType.ListIndex = CLng(nfse.ConfigLerValor(SESSAO_NFSe, "SSLType"))
    nudTimeOut.Value = CLng(nfse.ConfigLerValor(SESSAO_NFSe, "Timeout"))
    
    Dim ambiente As String
    ambiente = nfse.ConfigLerValor(SESSAO_NFSe, "Ambiente")
    rdbHomologacao.Value = CBool(ambiente)
    rdbProducao.Value = Not CBool(ambiente)
    
    chkVisualizarMensagem.Value = CLng(nfse.ConfigLerValor(SESSAO_NFSe, "Visualizar"))
    chkSalvarSOAP.Value = CLng(nfse.ConfigLerValor(SESSAO_NFSe, "SalvarWS"))
    chkAjustAut.Value = CLng(nfse.ConfigLerValor(SESSAO_NFSe, "AjustaAguardaConsultaRet"))
    nudAguardar.Value = CLng(nfse.ConfigLerValor(SESSAO_NFSe, "AguardarConsultaRet"))
    nudTentativas.Value = CLng(nfse.ConfigLerValor(SESSAO_NFSe, "Tentativas"))
    nudIntervalo.Value = CLng(nfse.ConfigLerValor(SESSAO_NFSe, "IntervaloTentativas"))
    
    'Proxy
    txtProxyServidor.Text = nfse.ConfigLerValor(SESSAO_PROXY, "Servidor")
    nudProxyPorta.Value = CLng(nfse.ConfigLerValor(SESSAO_PROXY, "Porta"))
    txtProxyUsuario.Text = nfse.ConfigLerValor(SESSAO_PROXY, "Usuario")
    txtProxySenha.Text = nfse.ConfigLerValor(SESSAO_PROXY, "Senha")
    
    'Certificado
    cmbCrypt.ListIndex = CLng(nfse.ConfigLerValor(SESSAO_DFE, "SSLCryptLib"))
    cmbHttp.ListIndex = CLng(nfse.ConfigLerValor(SESSAO_DFE, "SSLHttpLib"))
    cmbXmlSign.ListIndex = CLng(nfse.ConfigLerValor(SESSAO_DFE, "SSLXmlSignLib"))
    txtCertPath.Text = nfse.ConfigLerValor(SESSAO_DFE, "ArquivoPFX")
    txtCertPassword.Text = nfse.ConfigLerValor(SESSAO_DFE, "Senha")
    txtCertNumero.Text = nfse.ConfigLerValor(SESSAO_DFE, "NumeroSerie")
    txtDadosPFX.Text = nfse.ConfigLerValor(SESSAO_DFE, "DadosPFX")
    
    'Arquivos
    chkSalvarArqs.Value = CLng(nfse.ConfigLerValor(SESSAO_NFSe, "SalvarArq"))
    chkPastaMensal.Value = CLng(nfse.ConfigLerValor(SESSAO_NFSe, "SepararPorMes"))
    chkAdicionaLiteral.Value = CLng(nfse.ConfigLerValor(SESSAO_NFSe, "AdicionarLiteral"))
    chkEmissaoPathNFSe.Value = CLng(nfse.ConfigLerValor(SESSAO_NFSe, "EmissaoPathNFSe"))
    chkSepararPorCNPJ.Value = CLng(nfse.ConfigLerValor(SESSAO_NFSe, "SepararPorCNPJ"))
    
    'Doc Auxiliar
    txtLogomarcaPrefeitura.Text = nfse.ConfigLerValor(SESSAO_DANFSe, "PathLogo")
    txtLogoMarcaPrestadorServico.Text = nfse.ConfigLerValor(SESSAO_DANFSe, "Prestador.Logo")
    txtNomePrefeitura.Text = nfse.ConfigLerValor(SESSAO_DANFSe, "Prefeitura")
    txtPastaPDF.Text = nfse.ConfigLerValor(SESSAO_DANFSe, "PathPDF")
    
    'Email
    txtNome.Text = nfse.ConfigLerValor(SESSAO_EMAIL, "Nome")
    txtEmail.Text = nfse.ConfigLerValor(SESSAO_EMAIL, "Conta")
    txtUsuario.Text = nfse.ConfigLerValor(SESSAO_EMAIL, "Usuario")
    txtSenha.Text = nfse.ConfigLerValor(SESSAO_EMAIL, "Senha")
    txtHost.Text = nfse.ConfigLerValor(SESSAO_EMAIL, "Servidor")
    nudEmailPorta.Value = CLng(nfse.ConfigLerValor(SESSAO_EMAIL, "Porta"))
    chkSSL.Value = CLng(nfse.ConfigLerValor(SESSAO_EMAIL, "SSL"))
    chkTLS.Value = CLng(nfse.ConfigLerValor(SESSAO_EMAIL, "TLS"))
    
    'Emitente
    txtCNPJEmitente.Text = nfse.ConfigLerValor(SESSAO_NFSe, "Emitente.CNPJ")
    txtInscMunicipalEmitente.Text = nfse.ConfigLerValor(SESSAO_NFSe, "Emitente.InscMun")
    txtRazaoSocialEmitente.Text = nfse.ConfigLerValor(SESSAO_NFSe, "Emitente.RazSocial")
    txtEmitenteUsuário.Text = nfse.ConfigLerValor(SESSAO_NFSe, "Emitente.WSUser")
    txtEmitenteSenha.Text = nfse.ConfigLerValor(SESSAO_NFSe, "Emitente.WSSenha")
    txtEmitenteFraseSecreta.Text = nfse.ConfigLerValor(SESSAO_NFSe, "Emitente.WSFraseSecr")
    txtEmitenteChaveAcesso.Text = nfse.ConfigLerValor(SESSAO_NFSe, "Emitente.WSChaveAcesso")
    txtEmitenteChaveAutorizacao.Text = nfse.ConfigLerValor(SESSAO_NFSe, "Emitente.WSChaveAutoriz")
    txtFantasiaEmitente.Text = nfse.ConfigLerValor(SESSAO_NFSe, "Emitente.Dados.NomeFantasia")
    txtFoneEmitente.Text = nfse.ConfigLerValor(SESSAO_NFSe, "Emitente.Dados.Telefone")
    txtCEPEmitente.Text = nfse.ConfigLerValor(SESSAO_NFSe, "Emitente.Dados.CEP")
    txtLogradouroEmitente.Text = nfse.ConfigLerValor(SESSAO_NFSe, "Emitente.Dados.Endereco")
    txtNumeroEmitente.Text = nfse.ConfigLerValor(SESSAO_NFSe, "Emitente.Dados.Numero")
    txtComplementoEmitente.Text = nfse.ConfigLerValor(SESSAO_NFSe, "Emitente.Dados.Complemento")
    txtBairroEmitente.Text = nfse.ConfigLerValor(SESSAO_NFSe, "Emitente.Dados.Bairro")
    txtCodCidadeEmitente.Text = nfse.ConfigLerValor(SESSAO_NFSe, "CodigoMunicipio")
    
End Sub

Private Sub SalvarConfig()
    
    'Geral
    nfse.ConfigGravarValor SESSAO_NFSe, "ExibirErroSchema", CStr(chkExibirErroSchema.Value)
    nfse.ConfigGravarValor SESSAO_NFSe, "FormatoAlerta", txtFormatoAlerta.Text
    nfse.ConfigGravarValor SESSAO_NFSe, "FormaEmissao", CStr(cmbFormaEmissao.ListIndex)
    nfse.ConfigGravarValor SESSAO_NFSe, "RetirarAcentos", CStr(chkRetirarAcentos.Value)
    nfse.ConfigGravarValor SESSAO_NFSe, "SalvarGer", CStr(ckbSalvarArqeResp.Value)
    nfse.ConfigGravarValor SESSAO_NFSe, "PathSalvar", txtArqNFSe.Text
    nfse.ConfigGravarValor SESSAO_NFSe, "MontarPathSchema", CStr(ckbMontarPathSchemas.Value)
    nfse.ConfigGravarValor SESSAO_NFSe, "PathSchemas", txtSchemaPath.Text
    nfse.ConfigGravarValor SESSAO_NFSe, "ConsultaLoteAposEnvio", CStr(ckbConsultarLoteAposEnvio.Value)
    nfse.ConfigGravarValor SESSAO_NFSe, "ConsultaAposCancelar", CStr(ckbConsultarAposCancelar.Value)
    nfse.ConfigGravarValor SESSAO_NFSe, "LayoutNFSe", CStr(cmbLayoutNFSe.ListIndex)
    
    'WebService
    nfse.ConfigGravarValor SESSAO_DFE, "UF", CStr(cmbUFDestino.ListIndex)
    nfse.ConfigGravarValor SESSAO_NFSe, "SSLType", CStr(cmbSSlType.ListIndex)
    nfse.ConfigGravarValor SESSAO_NFSe, "Timeout", CStr(nudTimeOut.Value)
    nfse.ConfigGravarValor SESSAO_NFSe, "Ambiente", CStr(rdbProducao.Value)
    nfse.ConfigGravarValor SESSAO_NFSe, "Ambiente", CStr(rdbHomologacao.Value)
    nfse.ConfigGravarValor SESSAO_NFSe, "Visualizar", CStr(chkVisualizarMensagem)
    nfse.ConfigGravarValor SESSAO_NFSe, "SalvarWS", CStr(chkSalvarSOAP.Value)
    nfse.ConfigGravarValor SESSAO_NFSe, "AjustaAguardaConsultaRet", CStr(chkAjustAut.Value)
    nfse.ConfigGravarValor SESSAO_NFSe, "AguardarConsultaRet", CStr(nudAguardar.Value)
    nfse.ConfigGravarValor SESSAO_NFSe, "Tentativas", CStr(nudTentativas.Value)
    nfse.ConfigGravarValor SESSAO_NFSe, "IntervaloTentativas", CStr(nudIntervalo.Value)
    
    'Proxy
    nfse.ConfigGravarValor SESSAO_PROXY, "Servidor", txtProxyServidor.Text
    nfse.ConfigGravarValor SESSAO_PROXY, "Porta", CStr(nudProxyPorta.Value)
    nfse.ConfigGravarValor SESSAO_PROXY, "Usuario", txtProxyUsuario.Text
    nfse.ConfigGravarValor SESSAO_PROXY, "Senha", txtProxySenha.Text
    
    'Certificado
    nfse.ConfigGravarValor SESSAO_DFE, "SSLCryptLib", CStr(cmbCrypt.ListIndex)
    nfse.ConfigGravarValor SESSAO_DFE, "SSLHttpLib", CStr(cmbHttp.ListIndex)
    nfse.ConfigGravarValor SESSAO_DFE, "SSLXmlSignLib", CStr(cmbXmlSign.ListIndex)
    nfse.ConfigGravarValor SESSAO_DFE, "ArquivoPFX", txtCertPath.Text
    nfse.ConfigGravarValor SESSAO_DFE, "Senha", txtCertPassword.Text
    nfse.ConfigGravarValor SESSAO_DFE, "NumeroSerie", txtCertNumero.Text
    nfse.ConfigGravarValor SESSAO_DFE, "DadosPFX", txtDadosPFX.Text
    
    'Arquivos
    nfse.ConfigGravarValor SESSAO_NFSe, "SalvarArq", CStr(chkSalvarArqs.Value)
    nfse.ConfigGravarValor SESSAO_NFSe, "SepararPorMes", CStr(chkPastaMensal.Value)
    nfse.ConfigGravarValor SESSAO_NFSe, "AdicionarLiteral", CStr(chkAdicionaLiteral.Value)
    nfse.ConfigGravarValor SESSAO_NFSe, "EmissaoPathNFSe", CStr(chkEmissaoPathNFSe.Value)
    nfse.ConfigGravarValor SESSAO_NFSe, "SepararPorCNPJ", CStr(chkSepararPorCNPJ.Value)
    
    'Doc Auxiliar
    nfse.ConfigGravarValor SESSAO_DANFSe, "PathLogo", txtLogomarcaPrefeitura.Text
    nfse.ConfigGravarValor SESSAO_DANFSe, "Prestador.Logo", txtLogoMarcaPrestadorServico.Text
    nfse.ConfigGravarValor SESSAO_DANFSe, "Prefeitura", txtNomePrefeitura.Text
    nfse.ConfigGravarValor SESSAO_DANFSe, "PathPDF", txtPastaPDF.Text
    
    'Email
    nfse.ConfigGravarValor SESSAO_EMAIL, "Nome", txtNome.Text
    nfse.ConfigGravarValor SESSAO_EMAIL, "Conta", txtEmail.Text
    nfse.ConfigGravarValor SESSAO_EMAIL, "Usuario", txtUsuario.Text
    nfse.ConfigGravarValor SESSAO_EMAIL, "Senha", txtSenha.Text
    nfse.ConfigGravarValor SESSAO_EMAIL, "Servidor", txtHost.Text
    nfse.ConfigGravarValor SESSAO_EMAIL, "Porta", CStr(nudEmailPorta.Value)
    nfse.ConfigGravarValor SESSAO_EMAIL, "SSL", CStr(chkSSL.Value)
    nfse.ConfigGravarValor SESSAO_EMAIL, "TLS", CStr(chkTLS.Value)
    
    'Emitente
    nfse.ConfigGravarValor SESSAO_NFSe, "Emitente.CNPJ", txtCNPJEmitente.Text
    nfse.ConfigGravarValor SESSAO_NFSe, "Emitente.InscMun", txtInscMunicipalEmitente.Text
    nfse.ConfigGravarValor SESSAO_NFSe, "Emitente.RazSocial", txtRazaoSocialEmitente.Text
    nfse.ConfigGravarValor SESSAO_NFSe, "Emitente.WSUser", txtEmitenteUsuário.Text
    nfse.ConfigGravarValor SESSAO_NFSe, "Emitente.WSSenha", txtEmitenteSenha.Text
    nfse.ConfigGravarValor SESSAO_NFSe, "Emitente.WSFraseSecr", txtEmitenteFraseSecreta.Text
    nfse.ConfigGravarValor SESSAO_NFSe, "Emitente.WSChaveAcesso", txtEmitenteChaveAcesso.Text
    nfse.ConfigGravarValor SESSAO_NFSe, "Emitente.WSChaveAutoriz", txtEmitenteChaveAutorizacao.Text
    nfse.ConfigGravarValor SESSAO_NFSe, "Emitente.Dados.NomeFantasia", txtFantasiaEmitente.Text
    nfse.ConfigGravarValor SESSAO_NFSe, "Emitente.Dados.Telefone", txtFoneEmitente.Text
    nfse.ConfigGravarValor SESSAO_NFSe, "Emitente.Dados.CEP", txtCEPEmitente.Text
    nfse.ConfigGravarValor SESSAO_NFSe, "Emitente.Dados.Endereco", txtLogradouroEmitente.Text
    nfse.ConfigGravarValor SESSAO_NFSe, "Emitente.Dados.Numero", txtNumeroEmitente.Text
    nfse.ConfigGravarValor SESSAO_NFSe, "Emitente.Dados.Complemento", txtComplementoEmitente.Text
    nfse.ConfigGravarValor SESSAO_NFSe, "Emitente.Dados.Bairro", txtBairroEmitente.Text
    nfse.ConfigGravarValor SESSAO_NFSe, "CodigoMunicipio", cmbCidadeEmitente.ItemData(cmbCidadeEmitente.ListIndex)
    txtCodCidadeEmitente.Text = nfse.ConfigLerValor(SESSAO_NFSe, "CodigoMunicipio")
    
    nfse.ConfigGravar

End Sub
