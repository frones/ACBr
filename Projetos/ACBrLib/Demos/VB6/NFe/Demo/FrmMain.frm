VERSION 5.00
Object = "{86CF1D34-0C5F-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCT2.OCX"
Object = "{BDC217C8-ED16-11CD-956C-0000C04E4C0A}#1.1#0"; "TABCTL32.OCX"
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "COMDLG32.OCX"
Begin VB.Form FrmMain 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "ACBrLibNFe Demo"
   ClientHeight    =   8685
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
   ScaleHeight     =   8685
   ScaleWidth      =   12150
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
      Top             =   8040
      Width           =   2535
   End
   Begin TabDlg.SSTab SSTab1 
      Height          =   3855
      Left            =   5640
      TabIndex        =   3
      Top             =   120
      Width           =   6255
      _ExtentX        =   11033
      _ExtentY        =   6800
      _Version        =   393216
      Style           =   1
      Tabs            =   5
      TabsPerRow      =   5
      TabHeight       =   520
      TabCaption(0)   =   "Envio"
      TabPicture(0)   =   "FrmMain.frx":0000
      Tab(0).ControlEnabled=   -1  'True
      Tab(0).Control(0)=   "btnGerarXML"
      Tab(0).Control(0).Enabled=   0   'False
      Tab(0).Control(1)=   "btnCarregarIniNFe"
      Tab(0).Control(1).Enabled=   0   'False
      Tab(0).Control(2)=   "btnImprimirDANFE"
      Tab(0).Control(2).Enabled=   0   'False
      Tab(0).Control(3)=   "btnEnviarEmail"
      Tab(0).Control(3).Enabled=   0   'False
      Tab(0).Control(4)=   "btnAssinarNFe"
      Tab(0).Control(4).Enabled=   0   'False
      Tab(0).Control(5)=   "btnGerarChaveNFe"
      Tab(0).Control(5).Enabled=   0   'False
      Tab(0).Control(6)=   "btnEnviarSincrono"
      Tab(0).Control(6).Enabled=   0   'False
      Tab(0).Control(7)=   "btnCarregarXMLNFe"
      Tab(0).Control(7).Enabled=   0   'False
      Tab(0).Control(8)=   "btnImprimirPDFDANFE"
      Tab(0).Control(8).Enabled=   0   'False
      Tab(0).Control(9)=   "btnValidarRegraNegocio"
      Tab(0).Control(9).Enabled=   0   'False
      Tab(0).Control(10)=   "btnEnviarAssincrono"
      Tab(0).Control(10).Enabled=   0   'False
      Tab(0).Control(11)=   "btnLimparListaNFe"
      Tab(0).Control(11).Enabled=   0   'False
      Tab(0).ControlCount=   12
      TabCaption(1)   =   "Consultas"
      TabPicture(1)   =   "FrmMain.frx":001C
      Tab(1).ControlEnabled=   0   'False
      Tab(1).Control(0)=   "btnStatusServicos"
      Tab(1).Control(1)=   "btnConsultarRecibo"
      Tab(1).Control(2)=   "btnConsultaXXML"
      Tab(1).Control(3)=   "btnConsultarCadastro"
      Tab(1).Control(4)=   "btnConsultaChave"
      Tab(1).ControlCount=   5
      TabCaption(2)   =   "Eventos"
      TabPicture(2)   =   "FrmMain.frx":0038
      Tab(2).ControlEnabled=   0   'False
      Tab(2).Control(0)=   "btnCancelarNFe"
      Tab(2).Control(1)=   "btnCarregarEvento"
      Tab(2).Control(2)=   "btnImprimirEvento"
      Tab(2).Control(3)=   "btnEnviarEmailEvento"
      Tab(2).Control(4)=   "btnEnviarEvento"
      Tab(2).Control(5)=   "btnLimparListaEvento"
      Tab(2).Control(6)=   "btnImprimirEventoPDF"
      Tab(2).ControlCount=   7
      TabCaption(3)   =   "Inutilização"
      TabPicture(3)   =   "FrmMain.frx":0054
      Tab(3).ControlEnabled=   0   'False
      Tab(3).Control(0)=   "btnInutilizarNumeracao"
      Tab(3).Control(1)=   "btnImprimirInutilizacao"
      Tab(3).Control(2)=   "btnImprimirInutilizacaoPDF"
      Tab(3).ControlCount=   3
      TabCaption(4)   =   "Distribuição DFe"
      TabPicture(4)   =   "FrmMain.frx":0070
      Tab(4).ControlEnabled=   0   'False
      Tab(4).Control(0)=   "btnDFePorChave"
      Tab(4).Control(1)=   "btnDFePorNSU"
      Tab(4).Control(2)=   "btnDFePorUltNSU"
      Tab(4).ControlCount=   3
      Begin VB.CommandButton btnDFePorUltNSU 
         Caption         =   "Por Ult. NSU"
         Height          =   375
         Left            =   -71160
         TabIndex        =   34
         Top             =   480
         Width           =   1575
      End
      Begin VB.CommandButton btnDFePorNSU 
         Caption         =   "Por NSU"
         Height          =   375
         Left            =   -72960
         TabIndex        =   33
         Top             =   480
         Width           =   1575
      End
      Begin VB.CommandButton btnDFePorChave 
         Caption         =   "Por Chave"
         Height          =   375
         Left            =   -74760
         TabIndex        =   32
         Top             =   480
         Width           =   1575
      End
      Begin VB.CommandButton btnImprimirInutilizacaoPDF 
         Caption         =   "Imprimir PDF Inutilização"
         Height          =   375
         Left            =   -72960
         TabIndex        =   31
         Top             =   960
         Width           =   1575
      End
      Begin VB.CommandButton btnImprimirInutilizacao 
         Caption         =   "Imprimir Inutilização"
         Height          =   375
         Left            =   -74760
         TabIndex        =   30
         Top             =   960
         Width           =   1575
      End
      Begin VB.CommandButton btnInutilizarNumeracao 
         Caption         =   "Inutilizar Numeração"
         Height          =   375
         Left            =   -74760
         TabIndex        =   29
         Top             =   480
         Width           =   1575
      End
      Begin VB.CommandButton btnImprimirEventoPDF 
         Caption         =   "Imprimir PDF Evento"
         Height          =   375
         Left            =   -72840
         TabIndex        =   28
         Top             =   1440
         Width           =   1575
      End
      Begin VB.CommandButton btnLimparListaEvento 
         Caption         =   "Limpar Lista"
         Height          =   375
         Left            =   -72840
         TabIndex        =   27
         Top             =   960
         Width           =   1575
      End
      Begin VB.CommandButton btnEnviarEvento 
         Caption         =   "Enviar Evento"
         Height          =   375
         Left            =   -72840
         TabIndex        =   26
         Top             =   480
         Width           =   1575
      End
      Begin VB.CommandButton btnEnviarEmailEvento 
         Caption         =   "Enviar Evento Email"
         Height          =   375
         Left            =   -74640
         TabIndex        =   25
         Top             =   1920
         Width           =   1575
      End
      Begin VB.CommandButton btnImprimirEvento 
         Caption         =   "Imprimir Evento"
         Height          =   375
         Left            =   -74640
         TabIndex        =   24
         Top             =   1440
         Width           =   1575
      End
      Begin VB.CommandButton btnCarregarEvento 
         Caption         =   "Carregar Evento"
         Height          =   375
         Left            =   -74640
         TabIndex        =   23
         Top             =   960
         Width           =   1575
      End
      Begin VB.CommandButton btnCancelarNFe 
         Caption         =   "Cancelar NFe"
         Height          =   375
         Left            =   -74640
         TabIndex        =   22
         Top             =   480
         Width           =   1575
      End
      Begin VB.CommandButton btnConsultaChave 
         Caption         =   "Consultar com Chave"
         Height          =   375
         Left            =   -71160
         TabIndex        =   21
         Top             =   480
         Width           =   1575
      End
      Begin VB.CommandButton btnConsultarCadastro 
         Caption         =   "Consultar Cadastro"
         Height          =   375
         Left            =   -72960
         TabIndex        =   20
         Top             =   960
         Width           =   1575
      End
      Begin VB.CommandButton btnConsultaXXML 
         Caption         =   "Consultar com XML"
         Height          =   375
         Left            =   -72960
         TabIndex        =   19
         Top             =   480
         Width           =   1575
      End
      Begin VB.CommandButton btnConsultarRecibo 
         Caption         =   "Consultar Recibo"
         Height          =   375
         Left            =   -74760
         TabIndex        =   18
         Top             =   960
         Width           =   1575
      End
      Begin VB.CommandButton btnStatusServicos 
         Caption         =   " Status de Serviço"
         Height          =   375
         Left            =   -74760
         TabIndex        =   17
         Top             =   480
         Width           =   1575
      End
      Begin VB.CommandButton btnLimparListaNFe 
         Caption         =   "Limpar Lista NFe"
         Height          =   375
         Left            =   3840
         TabIndex        =   16
         Top             =   960
         Width           =   1830
      End
      Begin VB.CommandButton btnEnviarAssincrono 
         Caption         =   "Enviar Assincrono"
         Height          =   375
         Left            =   3840
         TabIndex        =   15
         Top             =   480
         Width           =   1815
      End
      Begin VB.CommandButton btnValidarRegraNegocio 
         Caption         =   "Val. Regra de Neg."
         Height          =   375
         Left            =   2040
         TabIndex        =   14
         Top             =   2400
         Width           =   1575
      End
      Begin VB.CommandButton btnImprimirPDFDANFE 
         Caption         =   "Imprimir PDF DANFE"
         Height          =   375
         Left            =   2040
         TabIndex        =   13
         Top             =   1440
         Width           =   1575
      End
      Begin VB.CommandButton btnCarregarXMLNFe 
         Caption         =   "Carregar XML NFe"
         Height          =   375
         Left            =   2040
         TabIndex        =   12
         Top             =   960
         Width           =   1575
      End
      Begin VB.CommandButton btnEnviarSincrono 
         Caption         =   "Enviar Sincrono"
         Height          =   375
         Left            =   2040
         TabIndex        =   11
         Top             =   480
         Width           =   1575
      End
      Begin VB.CommandButton btnGerarChaveNFe 
         Caption         =   "Gerar Chave NFe "
         Height          =   375
         Left            =   240
         TabIndex        =   10
         Top             =   2880
         Width           =   1575
      End
      Begin VB.CommandButton btnAssinarNFe 
         Caption         =   "Assinar NFe"
         Height          =   375
         Left            =   240
         TabIndex        =   9
         Top             =   2400
         Width           =   1575
      End
      Begin VB.CommandButton btnEnviarEmail 
         Caption         =   "Enviar NFe Email"
         Height          =   375
         Left            =   240
         TabIndex        =   8
         Top             =   1920
         Width           =   1575
      End
      Begin VB.CommandButton btnImprimirDANFE 
         Caption         =   "Imprimir DANFE"
         Height          =   375
         Left            =   240
         TabIndex        =   7
         Top             =   1440
         Width           =   1575
      End
      Begin VB.CommandButton btnCarregarIniNFe 
         Caption         =   "Carregar INI NFe"
         Height          =   375
         Left            =   240
         TabIndex        =   6
         Top             =   960
         Width           =   1575
      End
      Begin VB.CommandButton btnGerarXML 
         Caption         =   "Gerar XML"
         Height          =   375
         Left            =   240
         TabIndex        =   5
         Top             =   480
         Width           =   1575
      End
   End
   Begin MSComDlg.CommonDialog CommonDialog1 
      Left            =   8280
      Top             =   6240
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
   End
   Begin VB.Frame FraRespostas 
      Caption         =   "Respostas"
      Height          =   4455
      Left            =   5640
      TabIndex        =   1
      Top             =   4080
      Width           =   6255
      Begin VB.TextBox rtbRespostas 
         Height          =   4095
         Left            =   120
         Locked          =   -1  'True
         MultiLine       =   -1  'True
         ScrollBars      =   3  'Both
         TabIndex        =   2
         Top             =   240
         Width           =   6015
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
      Top             =   8040
      Width           =   2295
   End
   Begin TabDlg.SSTab SSTTab0 
      Height          =   7815
      Left            =   120
      TabIndex        =   35
      Top             =   120
      Width           =   5415
      _ExtentX        =   9551
      _ExtentY        =   13785
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
      TabPicture(0)   =   "FrmMain.frx":008C
      Tab(0).ControlEnabled=   -1  'True
      Tab(0).Control(0)=   "SSTab2"
      Tab(0).Control(0).Enabled=   0   'False
      Tab(0).ControlCount=   1
      TabCaption(1)   =   "Documento Auxiliar"
      TabPicture(1)   =   "FrmMain.frx":00A8
      Tab(1).ControlEnabled=   0   'False
      Tab(1).Control(0)=   "btnLogomarca"
      Tab(1).Control(1)=   "txtLogomarca"
      Tab(1).Control(2)=   "frmDANFe"
      Tab(1).Control(3)=   "frmPosPrinter"
      Tab(1).Control(4)=   "frmDANFCe"
      Tab(1).Control(5)=   "lblLogomarca"
      Tab(1).ControlCount=   6
      TabCaption(2)   =   "Email"
      TabPicture(2)   =   "FrmMain.frx":00C4
      Tab(2).ControlEnabled=   0   'False
      Tab(2).Control(0)=   "Label13"
      Tab(2).Control(1)=   "Label22"
      Tab(2).Control(2)=   "Frame2"
      Tab(2).Control(3)=   "txtAssuntoEmail"
      Tab(2).Control(4)=   "txtMensagemEmail"
      Tab(2).ControlCount=   5
      Begin VB.CommandButton btnLogomarca 
         Caption         =   "..."
         Height          =   260
         Left            =   -70320
         TabIndex        =   141
         Top             =   960
         Width           =   645
      End
      Begin VB.TextBox txtLogomarca 
         Height          =   285
         Left            =   -74640
         TabIndex        =   140
         Top             =   960
         Width           =   4530
      End
      Begin VB.Frame frmDANFe 
         Caption         =   "DANFe"
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
         Left            =   -74640
         TabIndex        =   136
         Top             =   1320
         Width           =   4950
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
            Height          =   255
            Left            =   240
            TabIndex        =   138
            Top             =   360
            Width           =   1095
         End
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
            Height          =   195
            Left            =   3000
            TabIndex        =   137
            Top             =   360
            Value           =   -1  'True
            Width           =   1455
         End
      End
      Begin VB.Frame frmPosPrinter 
         Caption         =   "Pos Printer"
         BeginProperty Font 
            Name            =   "Tahoma"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   3495
         Left            =   -74640
         TabIndex        =   135
         Top             =   3000
         Width           =   4695
         Begin VB.CheckBox chkIgnorarTags 
            Caption         =   "Ignorar Tags"
            Height          =   255
            Left            =   2640
            TabIndex        =   164
            Top             =   3000
            Width           =   1455
         End
         Begin VB.CheckBox chkCortarPapel 
            Caption         =   "Cortar Papel"
            Height          =   255
            Left            =   480
            TabIndex        =   163
            Top             =   3000
            Width           =   1455
         End
         Begin VB.CheckBox chkTraduzirTags 
            Caption         =   "Traduzir Tags"
            Height          =   255
            Left            =   2640
            TabIndex        =   162
            Top             =   2520
            Width           =   1455
         End
         Begin VB.CheckBox chkControlePorta 
            Caption         =   "Controle Porta"
            Height          =   255
            Left            =   480
            TabIndex        =   161
            Top             =   2520
            Width           =   1455
         End
         Begin VB.TextBox txtLinhasPular 
            Alignment       =   1  'Right Justify
            Height          =   285
            Left            =   3360
            TabIndex        =   159
            Text            =   "0"
            Top             =   2040
            Width           =   840
         End
         Begin VB.TextBox txtBuffer 
            Alignment       =   1  'Right Justify
            Height          =   285
            Left            =   2040
            TabIndex        =   156
            Text            =   "0"
            Top             =   2040
            Width           =   720
         End
         Begin VB.TextBox txtEspacos 
            Alignment       =   1  'Right Justify
            Height          =   285
            Left            =   1080
            TabIndex        =   153
            Text            =   "0"
            Top             =   2040
            Width           =   480
         End
         Begin VB.TextBox txtColunas 
            Alignment       =   1  'Right Justify
            Height          =   285
            Left            =   120
            TabIndex        =   149
            Text            =   "0"
            Top             =   2040
            Width           =   480
         End
         Begin VB.ComboBox cmbPortas 
            Height          =   315
            ItemData        =   "FrmMain.frx":00E0
            Left            =   120
            List            =   "FrmMain.frx":00E2
            Style           =   2  'Dropdown List
            TabIndex        =   145
            Top             =   1320
            Width           =   4455
         End
         Begin VB.ComboBox cmbPaginaCodigo 
            Height          =   315
            ItemData        =   "FrmMain.frx":00E4
            Left            =   2400
            List            =   "FrmMain.frx":00FD
            Style           =   2  'Dropdown List
            TabIndex        =   144
            Top             =   600
            Width           =   2175
         End
         Begin VB.ComboBox cmbModelo 
            Height          =   315
            ItemData        =   "FrmMain.frx":0133
            Left            =   120
            List            =   "FrmMain.frx":015B
            Style           =   2  'Dropdown List
            TabIndex        =   143
            Top             =   600
            Width           =   2175
         End
         Begin MSComCtl2.UpDown nudColunas 
            Height          =   285
            Left            =   600
            TabIndex        =   150
            Top             =   2040
            Width           =   255
            _ExtentX        =   450
            _ExtentY        =   503
            _Version        =   393216
            Value           =   5000
            BuddyControl    =   "txtColunas"
            BuddyDispid     =   196656
            OrigLeft        =   720
            OrigTop         =   2040
            OrigRight       =   975
            OrigBottom      =   2325
            Max             =   99999
            SyncBuddy       =   -1  'True
            BuddyProperty   =   0
            Enabled         =   -1  'True
         End
         Begin MSComCtl2.UpDown nudEspacos 
            Height          =   285
            Left            =   1560
            TabIndex        =   154
            Top             =   2040
            Width           =   255
            _ExtentX        =   450
            _ExtentY        =   503
            _Version        =   393216
            Value           =   5000
            BuddyControl    =   "txtEspacos"
            BuddyDispid     =   196655
            OrigLeft        =   1680
            OrigTop         =   2040
            OrigRight       =   1935
            OrigBottom      =   2325
            Max             =   99999
            SyncBuddy       =   -1  'True
            BuddyProperty   =   0
            Enabled         =   -1  'True
         End
         Begin MSComCtl2.UpDown nudBuffer 
            Height          =   285
            Left            =   2761
            TabIndex        =   157
            Top             =   2040
            Width           =   255
            _ExtentX        =   450
            _ExtentY        =   503
            _Version        =   393216
            Value           =   5000
            BuddyControl    =   "txtBuffer"
            BuddyDispid     =   196654
            OrigLeft        =   3000
            OrigTop         =   2040
            OrigRight       =   3255
            OrigBottom      =   2325
            Max             =   99999
            SyncBuddy       =   -1  'True
            BuddyProperty   =   0
            Enabled         =   -1  'True
         End
         Begin MSComCtl2.UpDown nudLinhasPular 
            Height          =   285
            Left            =   4200
            TabIndex        =   160
            Top             =   2040
            Width           =   255
            _ExtentX        =   450
            _ExtentY        =   503
            _Version        =   393216
            Value           =   5000
            BuddyControl    =   "txtLinhasPular"
            BuddyDispid     =   196653
            OrigLeft        =   4320
            OrigTop         =   2040
            OrigRight       =   4575
            OrigBottom      =   2325
            Max             =   99999
            SyncBuddy       =   -1  'True
            BuddyProperty   =   0
            Enabled         =   -1  'True
         End
         Begin VB.Label lblLinhasPular 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "Linhas Pular"
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
            TabIndex        =   158
            Top             =   1800
            Width           =   1020
         End
         Begin VB.Label lblBuffer 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "Buffer"
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
            Left            =   2040
            TabIndex        =   155
            Top             =   1800
            Width           =   510
         End
         Begin VB.Label lblEspacos 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "Espaços"
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
            Left            =   1080
            TabIndex        =   152
            Top             =   1800
            Width           =   675
         End
         Begin VB.Label lblColunas 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "Colunas"
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
            TabIndex        =   151
            Top             =   1800
            Width           =   660
         End
         Begin VB.Label lblPortaDA 
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
            TabIndex        =   148
            Top             =   1080
            Width           =   465
         End
         Begin VB.Label lblPgCod 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "Pag. Código"
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
            Left            =   2400
            TabIndex        =   147
            Top             =   360
            Width           =   975
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
            Left            =   120
            TabIndex        =   146
            Top             =   360
            Width           =   615
         End
      End
      Begin VB.Frame frmDANFCe 
         Caption         =   "DANFCe"
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
         Left            =   -74640
         TabIndex        =   132
         Top             =   2160
         Width           =   4695
         Begin VB.OptionButton rdbEscPos 
            Caption         =   "EscPos"
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
            TabIndex        =   142
            Top             =   360
            Value           =   -1  'True
            Width           =   1455
         End
         Begin VB.OptionButton rdbFortes 
            Caption         =   "Fortes"
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
            TabIndex        =   134
            Top             =   360
            Width           =   1095
         End
         Begin VB.OptionButton rdbFortesA4 
            Caption         =   "Fortes A4"
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
            TabIndex        =   133
            Top             =   360
            Width           =   1455
         End
      End
      Begin VB.TextBox txtMensagemEmail 
         Height          =   1365
         IMEMode         =   3  'DISABLE
         Left            =   -74400
         MultiLine       =   -1  'True
         ScrollBars      =   3  'Both
         TabIndex        =   111
         Top             =   5760
         Width           =   4155
      End
      Begin VB.TextBox txtAssuntoEmail 
         Height          =   285
         IMEMode         =   3  'DISABLE
         Left            =   -74400
         TabIndex        =   109
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
         TabIndex        =   96
         Top             =   600
         Width           =   4215
         Begin VB.TextBox txtPortaEmail 
            Height          =   285
            Left            =   120
            TabIndex        =   170
            Top             =   2280
            Width           =   450
         End
         Begin VB.TextBox txtSenha 
            Height          =   285
            Left            =   120
            TabIndex        =   169
            Top             =   3480
            Width           =   3855
         End
         Begin VB.TextBox txtUsuario 
            Height          =   285
            Left            =   120
            TabIndex        =   168
            Top             =   2880
            Width           =   3855
         End
         Begin VB.TextBox txtNome 
            Height          =   285
            IMEMode         =   3  'DISABLE
            Left            =   120
            TabIndex        =   101
            Top             =   480
            Width           =   3915
         End
         Begin VB.TextBox txtEmail 
            Height          =   285
            IMEMode         =   3  'DISABLE
            Left            =   120
            TabIndex        =   100
            Top             =   1080
            Width           =   3915
         End
         Begin VB.TextBox txtHost 
            Height          =   285
            IMEMode         =   3  'DISABLE
            Left            =   120
            TabIndex        =   99
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
            TabIndex        =   98
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
            TabIndex        =   97
            Top             =   2400
            Width           =   1590
         End
         Begin MSComCtl2.UpDown nudEmailPorta 
            Height          =   285
            Left            =   571
            TabIndex        =   178
            Top             =   2280
            Width           =   255
            _ExtentX        =   450
            _ExtentY        =   503
            _Version        =   393216
            Value           =   5000
            BuddyControl    =   "txtPortaEmail"
            BuddyDispid     =   196674
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
            TabIndex        =   107
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
            TabIndex        =   106
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
            TabIndex        =   105
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
            TabIndex        =   104
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
            TabIndex        =   103
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
            TabIndex        =   102
            Top             =   2040
            Width           =   465
         End
      End
      Begin TabDlg.SSTab SSTab2 
         Height          =   7335
         Left            =   120
         TabIndex        =   36
         Top             =   360
         Width           =   5175
         _ExtentX        =   9128
         _ExtentY        =   12938
         _Version        =   393216
         Style           =   1
         Tabs            =   4
         TabsPerRow      =   4
         TabHeight       =   520
         TabCaption(0)   =   "Geral"
         TabPicture(0)   =   "FrmMain.frx":01FD
         Tab(0).ControlEnabled=   -1  'True
         Tab(0).Control(0)=   "lblFormatoAlerta"
         Tab(0).Control(0).Enabled=   0   'False
         Tab(0).Control(1)=   "lblFormaEmissao"
         Tab(0).Control(1).Enabled=   0   'False
         Tab(0).Control(2)=   "lblModeloDocFiscal"
         Tab(0).Control(2).Enabled=   0   'False
         Tab(0).Control(3)=   "lblVersaoDocFiscal"
         Tab(0).Control(3).Enabled=   0   'False
         Tab(0).Control(4)=   "lblPastaLogs"
         Tab(0).Control(4).Enabled=   0   'False
         Tab(0).Control(5)=   "lblPastaSchemas"
         Tab(0).Control(5).Enabled=   0   'False
         Tab(0).Control(6)=   "lblIdCSC"
         Tab(0).Control(6).Enabled=   0   'False
         Tab(0).Control(7)=   "lblCSC"
         Tab(0).Control(7).Enabled=   0   'False
         Tab(0).Control(8)=   "chkExibirErroSchema"
         Tab(0).Control(8).Enabled=   0   'False
         Tab(0).Control(9)=   "chkAtualizarXML"
         Tab(0).Control(9).Enabled=   0   'False
         Tab(0).Control(10)=   "txtFormatoAlerta"
         Tab(0).Control(10).Enabled=   0   'False
         Tab(0).Control(11)=   "cmbFormaEmissao"
         Tab(0).Control(11).Enabled=   0   'False
         Tab(0).Control(12)=   "cmbVersaoDF"
         Tab(0).Control(12).Enabled=   0   'False
         Tab(0).Control(13)=   "chkRetirarAcentos"
         Tab(0).Control(13).Enabled=   0   'False
         Tab(0).Control(14)=   "chkSalvar"
         Tab(0).Control(14).Enabled=   0   'False
         Tab(0).Control(15)=   "btnSelectLog"
         Tab(0).Control(15).Enabled=   0   'False
         Tab(0).Control(16)=   "txtLogs"
         Tab(0).Control(16).Enabled=   0   'False
         Tab(0).Control(17)=   "btnSelectSchema"
         Tab(0).Control(17).Enabled=   0   'False
         Tab(0).Control(18)=   "txtSchemaPath"
         Tab(0).Control(18).Enabled=   0   'False
         Tab(0).Control(19)=   "txtIdCSC"
         Tab(0).Control(19).Enabled=   0   'False
         Tab(0).Control(20)=   "txtCSC"
         Tab(0).Control(20).Enabled=   0   'False
         Tab(0).Control(21)=   "cmbModeloDocumento"
         Tab(0).Control(21).Enabled=   0   'False
         Tab(0).ControlCount=   22
         TabCaption(1)   =   "WebServices"
         TabPicture(1)   =   "FrmMain.frx":0219
         Tab(1).ControlEnabled=   0   'False
         Tab(1).Control(0)=   "lblTimeOut(1)"
         Tab(1).Control(0).Enabled=   0   'False
         Tab(1).Control(1)=   "lblSSLType"
         Tab(1).Control(1).Enabled=   0   'False
         Tab(1).Control(2)=   "lblUFDestino"
         Tab(1).Control(2).Enabled=   0   'False
         Tab(1).Control(3)=   "nudTimeOut"
         Tab(1).Control(3).Enabled=   0   'False
         Tab(1).Control(4)=   "chkSalvarSOAP"
         Tab(1).Control(4).Enabled=   0   'False
         Tab(1).Control(5)=   "chkVisualizarMensagem"
         Tab(1).Control(5).Enabled=   0   'False
         Tab(1).Control(6)=   "frmRetEnvio"
         Tab(1).Control(6).Enabled=   0   'False
         Tab(1).Control(7)=   "frmProxy"
         Tab(1).Control(7).Enabled=   0   'False
         Tab(1).Control(8)=   "cmbSSlType"
         Tab(1).Control(8).Enabled=   0   'False
         Tab(1).Control(9)=   "cmbUFDestino"
         Tab(1).Control(9).Enabled=   0   'False
         Tab(1).Control(10)=   "frmAmbiente"
         Tab(1).Control(10).Enabled=   0   'False
         Tab(1).Control(11)=   "txtTimeOut"
         Tab(1).Control(11).Enabled=   0   'False
         Tab(1).ControlCount=   12
         TabCaption(2)   =   "Certificados"
         TabPicture(2)   =   "FrmMain.frx":0235
         Tab(2).ControlEnabled=   0   'False
         Tab(2).Control(0)=   "lblCryptLib"
         Tab(2).Control(0).Enabled=   0   'False
         Tab(2).Control(1)=   "lblHttpLib"
         Tab(2).Control(1).Enabled=   0   'False
         Tab(2).Control(2)=   "lblXMLSignLib"
         Tab(2).Control(2).Enabled=   0   'False
         Tab(2).Control(3)=   "cmbCrypt"
         Tab(2).Control(3).Enabled=   0   'False
         Tab(2).Control(4)=   "cmbHttp"
         Tab(2).Control(4).Enabled=   0   'False
         Tab(2).Control(5)=   "cmbXmlSign"
         Tab(2).Control(5).Enabled=   0   'False
         Tab(2).Control(6)=   "frmCertificados"
         Tab(2).Control(6).Enabled=   0   'False
         Tab(2).Control(7)=   "btnObterCertificados"
         Tab(2).Control(7).Enabled=   0   'False
         Tab(2).ControlCount=   8
         TabCaption(3)   =   "Arquivos"
         TabPicture(3)   =   "FrmMain.frx":0251
         Tab(3).ControlEnabled=   0   'False
         Tab(3).Control(0)=   "lblPastaArqNFe"
         Tab(3).Control(1)=   "lblPastaArqInutilizacao"
         Tab(3).Control(2)=   "lblPastaArqEvento"
         Tab(3).Control(3)=   "chkSalvarArqs"
         Tab(3).Control(4)=   "chkPastaMensal"
         Tab(3).Control(5)=   "chkAdicionaLiteral"
         Tab(3).Control(6)=   "chkEmissaoPathNFe"
         Tab(3).Control(7)=   "chkSalvaPathEvento"
         Tab(3).Control(8)=   "chkSepararPorCNPJ"
         Tab(3).Control(9)=   "chkSepararPorModelo"
         Tab(3).Control(10)=   "txtArqNFe"
         Tab(3).Control(11)=   "btnArqNFe"
         Tab(3).Control(12)=   "txtArqInu"
         Tab(3).Control(13)=   "btnArqInu"
         Tab(3).Control(14)=   "btnArqEvento"
         Tab(3).Control(15)=   "txtArqEvento"
         Tab(3).ControlCount=   16
         Begin VB.TextBox txtTimeOut 
            Alignment       =   1  'Right Justify
            Height          =   285
            Left            =   -71280
            TabIndex        =   171
            Text            =   "0"
            Top             =   720
            Width           =   825
         End
         Begin VB.ComboBox cmbModeloDocumento 
            Height          =   315
            ItemData        =   "FrmMain.frx":026D
            Left            =   120
            List            =   "FrmMain.frx":0277
            Style           =   2  'Dropdown List
            TabIndex        =   165
            Top             =   3000
            Width           =   2295
         End
         Begin VB.TextBox txtArqEvento 
            Height          =   285
            Left            =   -74880
            TabIndex        =   131
            Top             =   4440
            Width           =   3555
         End
         Begin VB.CommandButton btnArqEvento 
            Caption         =   "..."
            Height          =   260
            Left            =   -71280
            TabIndex        =   130
            Top             =   4440
            Width           =   390
         End
         Begin VB.CommandButton btnArqInu 
            Caption         =   "..."
            Height          =   260
            Left            =   -71280
            TabIndex        =   129
            Top             =   3840
            Width           =   390
         End
         Begin VB.TextBox txtArqInu 
            Height          =   285
            Left            =   -74880
            TabIndex        =   128
            Top             =   3840
            Width           =   3555
         End
         Begin VB.CommandButton btnArqNFe 
            Caption         =   "..."
            Height          =   260
            Left            =   -71280
            TabIndex        =   125
            Top             =   3240
            Width           =   390
         End
         Begin VB.TextBox txtArqNFe 
            Height          =   285
            Left            =   -74880
            TabIndex        =   124
            Top             =   3240
            Width           =   3555
         End
         Begin VB.CheckBox chkSepararPorModelo 
            Caption         =   "Separar Arqs pelo Modelo do Documento"
            Height          =   195
            Left            =   -74880
            TabIndex        =   122
            Top             =   2520
            Width           =   3495
         End
         Begin VB.CheckBox chkSepararPorCNPJ 
            Caption         =   "Separar Arqs pelo CNPJ do Certificado"
            Height          =   195
            Left            =   -74880
            TabIndex        =   121
            Top             =   2160
            Width           =   3615
         End
         Begin VB.CheckBox chkSalvaPathEvento 
            Caption         =   "Salvar Arquivos de Eventos"
            Height          =   195
            Left            =   -74880
            TabIndex        =   120
            Top             =   1800
            Width           =   3855
         End
         Begin VB.CheckBox chkEmissaoPathNFe 
            Caption         =   "Salvar Documento pelo campo Data de Emissão"
            Height          =   195
            Left            =   -74880
            TabIndex        =   119
            Top             =   1440
            Width           =   4455
         End
         Begin VB.CheckBox chkAdicionaLiteral 
            Caption         =   "Adicionar Literal no nome das pastas"
            Height          =   195
            Left            =   -74880
            TabIndex        =   118
            Top             =   1080
            Width           =   3615
         End
         Begin VB.CheckBox chkPastaMensal 
            Caption         =   "Criar Pastas Mensalmente"
            Height          =   255
            Left            =   -74880
            TabIndex        =   117
            Top             =   720
            Width           =   3855
         End
         Begin VB.CheckBox chkSalvarArqs 
            Caption         =   "Salvar Arquivos em Pastas Separadas"
            Height          =   195
            Left            =   -74880
            TabIndex        =   116
            Top             =   420
            Width           =   4335
         End
         Begin VB.CommandButton btnObterCertificados 
            Caption         =   "Obter Certificados"
            Height          =   375
            Left            =   -74640
            TabIndex        =   112
            Top             =   5160
            Width           =   1575
         End
         Begin VB.TextBox txtCSC 
            Height          =   285
            Left            =   120
            TabIndex        =   81
            Top             =   6840
            Width           =   4695
         End
         Begin VB.TextBox txtIdCSC 
            Height          =   285
            Left            =   120
            TabIndex        =   80
            Top             =   6240
            Width           =   4695
         End
         Begin VB.TextBox txtSchemaPath 
            Height          =   285
            Left            =   120
            TabIndex        =   79
            Top             =   5640
            Width           =   4215
         End
         Begin VB.CommandButton btnSelectSchema 
            Caption         =   "..."
            Height          =   260
            Left            =   4440
            TabIndex        =   78
            Top             =   5640
            Width           =   390
         End
         Begin VB.TextBox txtLogs 
            Height          =   285
            Left            =   120
            TabIndex        =   77
            Top             =   5040
            Width           =   4215
         End
         Begin VB.CommandButton btnSelectLog 
            Caption         =   "..."
            Height          =   260
            Left            =   4440
            TabIndex        =   76
            Top             =   5040
            Width           =   390
         End
         Begin VB.CheckBox chkSalvar 
            Caption         =   "Salvar Arquivos de Envio e Resposta"
            Height          =   255
            Left            =   120
            TabIndex        =   75
            Top             =   4440
            Width           =   3375
         End
         Begin VB.CheckBox chkRetirarAcentos 
            Caption         =   "Retirar Acentos dos XMLs enviados"
            Height          =   255
            Left            =   120
            TabIndex        =   74
            Top             =   4080
            Width           =   3375
         End
         Begin VB.ComboBox cmbVersaoDF 
            Height          =   315
            ItemData        =   "FrmMain.frx":028A
            Left            =   120
            List            =   "FrmMain.frx":029A
            Style           =   2  'Dropdown List
            TabIndex        =   73
            Top             =   3600
            Width           =   2295
         End
         Begin VB.ComboBox cmbFormaEmissao 
            Height          =   315
            ItemData        =   "FrmMain.frx":02BA
            Left            =   120
            List            =   "FrmMain.frx":02D9
            Style           =   2  'Dropdown List
            TabIndex        =   72
            Top             =   2400
            Width           =   2295
         End
         Begin VB.TextBox txtFormatoAlerta 
            Height          =   285
            Left            =   120
            TabIndex        =   71
            Top             =   1680
            Width           =   4695
         End
         Begin VB.CheckBox chkAtualizarXML 
            Caption         =   "Atualizar XML"
            Height          =   255
            Left            =   120
            TabIndex        =   70
            Top             =   600
            Width           =   3375
         End
         Begin VB.CheckBox chkExibirErroSchema 
            Caption         =   "Exibir Erro Schema"
            Height          =   255
            Left            =   120
            TabIndex        =   69
            Top             =   960
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
            TabIndex        =   66
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
               TabIndex        =   68
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
               TabIndex        =   67
               ToolTipText     =   "taProducao"
               Top             =   360
               Width           =   1095
            End
         End
         Begin VB.ComboBox cmbUFDestino 
            Height          =   315
            ItemData        =   "FrmMain.frx":0335
            Left            =   -74760
            List            =   "FrmMain.frx":038A
            Style           =   2  'Dropdown List
            TabIndex        =   65
            Top             =   720
            Width           =   1095
         End
         Begin VB.ComboBox cmbSSlType 
            Height          =   315
            ItemData        =   "FrmMain.frx":03FA
            Left            =   -73440
            List            =   "FrmMain.frx":0413
            Style           =   2  'Dropdown List
            TabIndex        =   64
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
            TabIndex        =   58
            Top             =   4560
            Width           =   4695
            Begin VB.TextBox txtProxyPorta 
               Alignment       =   1  'Right Justify
               Height          =   285
               Left            =   2880
               TabIndex        =   172
               Text            =   "0"
               Top             =   480
               Width           =   345
            End
            Begin VB.TextBox txtProxySenha 
               Height          =   285
               Left            =   120
               TabIndex        =   167
               Top             =   1680
               Width           =   3855
            End
            Begin VB.TextBox txtProxyUsuario 
               Height          =   285
               Left            =   120
               TabIndex        =   166
               Top             =   1080
               Width           =   3855
            End
            Begin VB.TextBox txtProxyServidor 
               Height          =   285
               Left            =   120
               TabIndex        =   59
               Top             =   480
               Width           =   2655
            End
            Begin MSComCtl2.UpDown nudProxyPorta 
               Height          =   285
               Left            =   3226
               TabIndex        =   177
               Top             =   480
               Width           =   255
               _ExtentX        =   450
               _ExtentY        =   503
               _Version        =   393216
               Value           =   5000
               BuddyControl    =   "txtProxyPorta"
               BuddyDispid     =   196723
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
               TabIndex        =   63
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
               TabIndex        =   62
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
               TabIndex        =   61
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
               TabIndex        =   60
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
            TabIndex        =   50
            Top             =   2760
            Width           =   4695
            Begin VB.CheckBox chkAjustAut 
               Caption         =   "Ajustar Automaticamente prop. ""Aguardar"""
               Height          =   375
               Left            =   120
               TabIndex        =   54
               Top             =   240
               Width           =   4095
            End
            Begin VB.TextBox txtAguardar 
               Alignment       =   1  'Right Justify
               Height          =   285
               Left            =   120
               TabIndex        =   53
               Text            =   "0"
               Top             =   960
               Width           =   720
            End
            Begin VB.TextBox txtTentativas 
               Alignment       =   1  'Right Justify
               Height          =   285
               Left            =   1800
               TabIndex        =   52
               Text            =   "0"
               Top             =   960
               Width           =   840
            End
            Begin VB.TextBox txtIntervalo 
               Alignment       =   1  'Right Justify
               Height          =   285
               Left            =   3360
               TabIndex        =   51
               Text            =   "0"
               Top             =   960
               Width           =   705
            End
            Begin MSComCtl2.UpDown nudAguardar 
               Height          =   285
               Left            =   840
               TabIndex        =   174
               Top             =   960
               Width           =   255
               _ExtentX        =   450
               _ExtentY        =   503
               _Version        =   393216
               Value           =   5000
               BuddyControl    =   "txtAguardar"
               BuddyDispid     =   196733
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
               TabIndex        =   175
               Top             =   960
               Width           =   255
               _ExtentX        =   450
               _ExtentY        =   503
               _Version        =   393216
               Value           =   5000
               BuddyControl    =   "txtTentativas"
               BuddyDispid     =   196734
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
               TabIndex        =   176
               Top             =   960
               Width           =   255
               _ExtentX        =   450
               _ExtentY        =   503
               _Version        =   393216
               Value           =   5000
               BuddyControl    =   "txtIntervalo"
               BuddyDispid     =   196735
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
               TabIndex        =   57
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
               TabIndex        =   56
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
               TabIndex        =   55
               Top             =   720
               Width           =   795
            End
         End
         Begin VB.CheckBox chkVisualizarMensagem 
            Caption         =   "Visualizar Mensagem"
            Height          =   375
            Left            =   -74760
            TabIndex        =   49
            Top             =   2040
            Width           =   2775
         End
         Begin VB.CheckBox chkSalvarSOAP 
            Caption         =   "Salvar envelope SOAP"
            Height          =   255
            Left            =   -74760
            TabIndex        =   48
            Top             =   2400
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
            TabIndex        =   40
            Top             =   2400
            Width           =   4215
            Begin VB.TextBox txtDadosPFX 
               Height          =   285
               Left            =   120
               TabIndex        =   114
               Top             =   1080
               Width           =   3555
            End
            Begin VB.CommandButton btnDadosPFX 
               Caption         =   "..."
               Height          =   260
               Left            =   3720
               TabIndex        =   113
               Top             =   1080
               Width           =   390
            End
            Begin VB.CommandButton btnSelecionarCertificado 
               Caption         =   "..."
               Height          =   260
               Left            =   3720
               TabIndex        =   44
               Top             =   479
               Width           =   390
            End
            Begin VB.TextBox txtCertPath 
               Height          =   285
               Left            =   120
               TabIndex        =   43
               Top             =   480
               Width           =   3555
            End
            Begin VB.TextBox txtCertPassword 
               Height          =   285
               IMEMode         =   3  'DISABLE
               Left            =   120
               PasswordChar    =   "*"
               TabIndex        =   42
               Top             =   1680
               Width           =   4035
            End
            Begin VB.TextBox txtCertNumero 
               Height          =   285
               Left            =   120
               TabIndex        =   41
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
               TabIndex        =   115
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
               TabIndex        =   47
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
               TabIndex        =   46
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
               TabIndex        =   45
               Top             =   2040
               Width           =   1395
            End
         End
         Begin VB.ComboBox cmbXmlSign 
            Height          =   315
            ItemData        =   "FrmMain.frx":045F
            Left            =   -74640
            List            =   "FrmMain.frx":0472
            Style           =   2  'Dropdown List
            TabIndex        =   39
            Top             =   1920
            Width           =   2175
         End
         Begin VB.ComboBox cmbHttp 
            Height          =   315
            ItemData        =   "FrmMain.frx":04AC
            Left            =   -74640
            List            =   "FrmMain.frx":04BC
            Style           =   2  'Dropdown List
            TabIndex        =   38
            Top             =   1320
            Width           =   2175
         End
         Begin VB.ComboBox cmbCrypt 
            Height          =   315
            ItemData        =   "FrmMain.frx":04F1
            Left            =   -74640
            List            =   "FrmMain.frx":0501
            Style           =   2  'Dropdown List
            TabIndex        =   37
            Top             =   720
            Width           =   2175
         End
         Begin MSComCtl2.UpDown nudTimeOut 
            Height          =   285
            Left            =   -70440
            TabIndex        =   173
            Top             =   720
            Width           =   255
            _ExtentX        =   450
            _ExtentY        =   503
            _Version        =   393216
            Value           =   5000
            BuddyControl    =   "txtTimeOut"
            BuddyDispid     =   196688
            OrigLeft        =   3960
            OrigTop         =   720
            OrigRight       =   4215
            OrigBottom      =   975
            Max             =   99999
            SyncBuddy       =   -1  'True
            BuddyProperty   =   0
            Enabled         =   -1  'True
         End
         Begin VB.Label lblPastaArqEvento 
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
            TabIndex        =   127
            Top             =   4200
            Width           =   1905
         End
         Begin VB.Label lblPastaArqInutilizacao 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "Pasta Arquivos Inutilização"
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
            Top             =   3600
            Width           =   2310
         End
         Begin VB.Label lblPastaArqNFe 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "Pasta Arquivos NFe"
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
            TabIndex        =   123
            Top             =   3000
            Width           =   1620
         End
         Begin VB.Label lblCSC 
            Caption         =   "Token/CSC (Somente para NFC-e)"
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
            TabIndex        =   95
            Top             =   6600
            Width           =   4575
         End
         Begin VB.Label lblIdCSC 
            Caption         =   "IdToken/IdCSC (Somente para NFC-e)"
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
            TabIndex        =   94
            Top             =   6000
            Width           =   4575
         End
         Begin VB.Label lblPastaSchemas 
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
            Height          =   255
            Left            =   120
            TabIndex        =   93
            Top             =   5400
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
            TabIndex        =   92
            Top             =   4800
            Width           =   4575
         End
         Begin VB.Label lblVersaoDocFiscal 
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
            Height          =   255
            Left            =   120
            TabIndex        =   91
            Top             =   3360
            Width           =   4575
         End
         Begin VB.Label lblModeloDocFiscal 
            Caption         =   "Modelo Documento Fiscal"
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
            TabIndex        =   90
            Top             =   2760
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
            TabIndex        =   89
            Top             =   2160
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
            TabIndex        =   88
            Top             =   1440
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
            TabIndex        =   87
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
            TabIndex        =   86
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
            TabIndex        =   85
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
            TabIndex        =   84
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
            TabIndex        =   83
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
            TabIndex        =   82
            Top             =   480
            Width           =   705
         End
      End
      Begin VB.Label lblLogomarca 
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
         Left            =   -74640
         TabIndex        =   139
         Top             =   720
         Width           =   945
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
         TabIndex        =   110
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
         TabIndex        =   108
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
Dim nfe As ACBrNFe

Private Sub Form_Load()
    cmbModeloDocumento.ListIndex = 0
    cmbUFDestino.Text = "SP"
    cmbSSlType.ListIndex = 0
    cmbCrypt.ListIndex = 0
    cmbHttp.ListIndex = 0
    cmbXmlSign.ListIndex = 0
    
    cmbPortas.AddItem "COM1"
    cmbPortas.AddItem "COM2"
    cmbPortas.AddItem "LPT1"
    cmbPortas.AddItem "LPT2"
    cmbPortas.AddItem "\\localhost\Epson"
    cmbPortas.AddItem "C:\temp\ecf.txt"
    
    cmbPortas.ListIndex = cmbPortas.ListCount - 1
    
    cmbPortas.AddItem "TCP:192.168.0.31:9100"
    cmbPortas.AddItem "RAW:Elgin I9"
    
    nudColunas.Value = 0
    nudEspacos.Value = 0
    nudBuffer.Value = 0
    nudLinhasPular.Value = 0
    
    Dim LogPath As String
    Dim IniPath As String
    
    LogPath = App.Path & "\Logs\"
    IniPath = App.Path & "\ACBrLib.ini"
    
    If Not DirExists(LogPath) Then
        MkDir LogPath
    End If
    
    Set nfe = CreateNFe(IniPath)
    
    nfe.ConfigGravarValor SESSAO_PRINCIPAL, "LogNivel", NivelLog.logParanoico
    nfe.ConfigGravarValor SESSAO_PRINCIPAL, "LogPath", LogPath
    nfe.ConfigGravar
    
    LoadConfig
End Sub

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

Private Sub btnArqEvento_Click()
    txtArqEvento.Text = BrowseFolder("Selecione a pasta Arquivos Eventos")
End Sub

Private Sub btnArqInu_Click()
    txtArqInu.Text = BrowseFolder("Selecione a pasta Arquivos Inutilização")
End Sub

Private Sub btnArqNFe_Click()
    txtArqNFe.Text = BrowseFolder("Selecione a pasta Arquivos NFe")
End Sub

Private Sub btnAssinarNFe_Click()
        
    If Not validacao Then
        MsgBox ("Erro Verifique as configurações do certificado")
        Exit Sub
    End If
    
    On Error GoTo Erro:
    CheckNFeLista
    nfe.Assinar
    
Erro:
    MsgBox Err.Description
        
End Sub

Private Sub btnCancelarNFe_Click()

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
    eChave = InputBox("Chave da NF-e", "WebServices Eventos: Cancelamento", "")
    eCNPJ = InputBox("CNPJ ou o CPF do autor do Evento", "WebServices Eventos: Cancelamento", "")
    aJustificativa = InputBox("Justificativa do Cancelamento", "WebServices Eventos: Cancelamento", "")

    SetResposta nfe.Cancelar(eChave, aJustificativa, eCNPJ, CLng(idLote))
    
Erro:
    MsgBox Err.Description

End Sub


Private Sub btnCarregarConfiguracoes_Click()
    LoadConfig
End Sub

Private Sub CheckNFeLista()
    
    Dim xml As Boolean
    
    answer = MsgBox("Limpar Lista ?", vbExclamation + vbYesNo, "Add Confirm")
    If answer = vbYes Then
    nfe.LimparLista
    End If
    
    If xml Then
    
    CommonDialog1.DialogTitle = "Selecione o arquivo ini"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivo xml NFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
            
    If CommonDialog1.FileName = vbNullString Then Exit Sub
    
    nfe.CarregarXML CommonDialog1.FileName
    
    Else
    
    CommonDialog1.DialogTitle = "Selecione o arquivo ini"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivo ini NFe (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
            
    If CommonDialog1.FileName = vbNullString Then Exit Sub
    
    nfe.CarregarINI CommonDialog1.FileName
    End If
    
End Sub
    
Private Sub btnCarregarEvento_Click()
    
    On Error GoTo Erro:
    Dim arquivoIni As String
    
    CommonDialog1.DialogTitle = "Selecione o arquivo ini"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivo Ini NFe (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
            
    If CommonDialog1.FileName = vbNullString Then Exit Sub
    arquivoIni = CommonDialog1.FileName
    
    nfe.CarregarEventoINI (arquivoIni)
   
Erro:
    MsgBox Err.Description
    
End Sub

Private Sub btnCarregarIniNFe_Click()

    On Error GoTo Erro:
    CheckNFeLista
Erro:
    MsgBox Err.Description
    
End Sub

Private Sub btnCarregarXMLNFe_Click()

    On Error GoTo Erro:
    
    CheckNFeLista
    
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnConsultaChave_Click()

    If Not validacao Then
        MsgBox ("Erro Verifique as configurações do certificado")
        Exit Sub
    End If

    On Error GoTo Erro:
    Dim chaveOuNFe As String
    
    chaveOuNFe = InputBox("Chave da NF-e:", "WebServices Consultar: Nota", "")
    SetResposta nfe.Consultar(chaveOuNFe)
    
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnConsultarCadastro_Click()
        
    If Not validacao Then
        MsgBox ("Erro Verifique as configurações do certificado")
        Exit Sub
    End If
        
    On Error GoTo Erro:
    Dim ret As String
    Dim uf As String
    Dim documento As String
    Dim ie As Boolean
   
    
    uf = InputBox("UF", "WebServices Consultar: Cadastro", "")
    documento = InputBox("Documento", "WebServices Consultar: Cadastro", "")
    ie = MsgBox("WebServices Consultar: Cadastro", "O documento é uma inscrição estadual ?", vbExclamation + vbYesNo, "Add Confirm")
    
    ret = nfe.ConsultaCadastro(uf, documento, ie)
    rtbRespostas.Text = ret
    
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
    SetResposta nfe.ConsultarRecibo(aRecibo)
    
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnConsultaXml_Click()

    On Error GoTo Erro:
    
    CommonDialog1.DialogTitle = "Selecione o arquivo xml"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivo Xml NFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
            
    If CommonDialog1.FileName = vbNullString Then Exit Sub
    
    nfe.LimparLista
    SetResposta nfe.Consultar(CommonDialog1.FileName)
    
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnEnviar_Click()

    On Error GoTo Erro:
    
    CommonDialog1.DialogTitle = "Selecione o arquivo ini"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivo Ini NFe (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
            
    If CommonDialog1.FileName = vbNullString Then Exit Sub
    
    nfe.LimparLista
    nfe.CarregarINI (CommonDialog1.FileName)
    SetResposta nfe.Enviar(1)
    
Erro:
    MsgBox Err.Description
    
End Sub


Private Sub btnConsultaXXML_Click()
    
    If Not validacao Then
        MsgBox ("Erro Verifique as configurações do certificado")
        Exit Sub
    End If
    
    On Error GoTo Erro:
    Dim ret As String
    
    CommonDialog1.DialogTitle = "Selecione o arquivo xml"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivo Xml NFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
            
    If CommonDialog1.FileName = vbNullString Then Exit Sub
    
    nfe.LimparLista
    ret = nfe.Consultar(CommonDialog1.FileName)
    rtbRespostas.Text = ret
    
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
    
    txtDadosPFX.Text = CommonDialog1.FileName
End Sub

Private Sub btnDFePorChave_Click()

    If Not validacao Then
        MsgBox ("Erro Verifique as configurações do certificado")
        Exit Sub
    End If

    On Error GoTo Erro:
    Dim ret As String
    Dim codUF As Long
    Dim cnpj As String
    Dim chave As String
    
    codUF = InputBox("Codigo da UF", "WebServices: Distribuição DFe", 35)
    cnpj = InputBox("CNPJ do Autor", "WebServices: Distribuição DFe", "")
    chave = InputBox("Chave do NFe", "WebServices: Distribuição DFe", "")
    
    ret = nfe.DistribuicaoDFePorChave(codUF, cnpj, chave)
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
    Dim cnpj As String
    Dim eNsu As String
    
    codUF = InputBox("Codigo da UF", "WebServices: Distribuição DFe", 35)
    cnpj = InputBox("CNPJ do Autor", "WebServices: Distribuição DFe", "")
    eNsu = InputBox("Numero do NSU", "WebServices: Distribuição DFe", "")
    
    ret = nfe.DistribuicaoDFePorNSU(codUF, cnpj, eNsu)
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
    Dim cnpj As String
    Dim eNsu As String
    
    codUF = InputBox("Codigo da UF", "WebServices: Distribuição DFe", 35)
    cnpj = InputBox("CNPJ do Autor", "WebServices: Distribuição DFe", "")
    eultNsu = InputBox("Número do último NSU", "WebServices: Distribuição DFe", "")
    
    ret = nfe.DistribuicaoDFePorUltNSU(codUF, cnpj, eultNsu)
    rtbRespostas.Text = ret
    
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
    
    CheckNFeLista
    aLote = InputBox("Número do Lote", "WebServices Enviar", 1)
    ret = nfe.Enviar(aLote)
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
    CommonDialog1.Filter = "Arquivo Xml NFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
            
    If CommonDialog1.FileName = vbNullString Then Exit Sub
    
    Dim destinatario As String
    destinatario = InputBox("Digite o email do destinatario", "Envio email", vbNullString)
    
    If destinatario = vbNullString Then Exit Sub
    nfe.EnviarEmail destinatario, CommonDialog1.FileName, True, txtAssunto.Text, txtMensagem.Text
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnImprimir_Click()
    
    On Error GoTo Erro:
    
    CommonDialog1.DialogTitle = "Selecione o arquivo xml"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivo Xml NFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
            
    If CommonDialog1.FileName = vbNullString Then Exit Sub
    
    nfe.LimparLista
    nfe.CarregarXML CommonDialog1.FileName
    nfe.Imprimir
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnInutilizar_Click()

    On Error GoTo Erro:
    Dim ano As String
    Dim modelo As String
    Dim serie As String
    Dim numeroInicial As String
    Dim numeroFinal As String
    Dim aJustificativa As String
    Dim eCNPJ As String
    
    ano = InputBox("Ano", "WebServices Inutilização", "19")
    modelo = InputBox("Modelo", "WebServices Inutilização", "55")
    serie = InputBox("Serie", "WebServices Inutilização", "1")
    numeroInicial = InputBox("Número Inicial", "WebServices Inutilização", "1")
    numeroFinal = InputBox("Número Final", "WebServices Inutilização", "1")
    eCNPJ = InputBox("CNPJ ou o CPF do autor do Emitente", "WebServices Inutilização", "")
    aJustificativa = InputBox("Justificativa", "WebServices Inutilização", "")
    
    SetResposta nfe.Inutilizar(eCNPJ, aJustificativa, CLng(ano), CLng(modelo), CLng(serie), CLng(numeroInicial), CLng(numeroFinal))
    
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
    CommonDialog1.Filter = "Arquivo Xml NFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
    
    If CommonDialog.FileName1 = vbNullString Then Exit Sub
    arquivoXml = CommonDialog.FileName
    
    destinatario = ""
    destinatario = InputBox("Digite o email do destinatario", "Envio email", "")
    
    nfe.EnviarEmailEvento destinatario, arquivoXmlEvento, arquivoXml, True, txtAssuntoEmail.Text, txtMensagemEmail.Text
    
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
    ret = nfe.EnviarEvento(idLote)
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
    
    CheckNFeLista
    aLote = InputBox("Número do Lote", "WebServices Enviar", 1)
    ret = nfe.Enviar(aLote, False, True)
    rtbRespostas.Text = ret
    
Erro:
    MsgBox Err.Description
    
End Sub

Private Sub btnGerarChaveNFe_Click()

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

Private Sub btnGerarXML_Click()

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
    
    nfe.LimparLista
    nfe.CarregarINI CommonDialog1.FileName
    nfe.Assinar
    ret = nfe.ObterXml(0)
    rtbRespostas.Text = ret
Erro:
    MsgBox Err.Description
    
End Sub

Private Sub btnImprimirDANFE_Click()

    On Error GoTo Erro:
    
    CommonDialog1.DialogTitle = "Selecione o arquivo xml"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivo Xml NFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
            
    If CommonDialog1.FileName = vbNullString Then Exit Sub
    
    nfe.LimparLista
    nfe.CarregarXML CommonDialog1.FileName
    nfe.Imprimir
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
    CommonDialog1.Filter = "Arquivo Xml NFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
    
    If CommonDialog.FileName1 = vbNullString Then Exit Sub
    arquivoXml = CommonDialog.FileName
    
    nfe.ImprimirEvento arquivoXml, arquivoXmlEvento
    
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
    CommonDialog1.Filter = "Arquivo Xml NFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
    
    If CommonDialog.FileName1 = vbNullString Then Exit Sub
    arquivoXml = CommonDialog.FileName
    
    nfe.ImprimirEventoPDF arquivoXml, arquivoXmlEvento
    
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnImprimirInutilizacao_Click()
    
    On Error GoTo Erro:
    Dim arquivoXml As String
    
    CommonDialog1.DialogTitle = "Selecione o arquivo xml"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivo Xml Inutilização (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
            
    If CommonDialog1.FileName = vbNullString Then Exit Sub
    
    arquivoXml = CommonDialog1.FileName
    
    nfe.ImprimirInutilizacao (arquivoXml)
Erro:
    MsgBox Err.Description
    
End Sub

Private Sub btnImprimirInutilizacaoPDF_Click()

    On Error GoTo Erro:
    Dim arquivoXml As String
    
    CommonDialog1.DialogTitle = "Selecione o arquivo xml"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivo Xml Inutilização (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
            
    If CommonDialog1.FileName = vbNullString Then Exit Sub
    
    arquivoXml = CommonDialog1.FileName
    
    nfe.ImprimirInutilizacaoPDF (arquivoXml)
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnImprimirPDFDANFE_Click()

    On Error GoTo Erro:
    
    CommonDialog1.DialogTitle = "Selecione o arquivo xml"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivo Xml DANFE (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
            
    If CommonDialog1.FileName = vbNullString Then Exit Sub
    
    nfe.LimparLista
    nfe.CarregarXML CommonDialog1.FileName
    nfe.Imprimir
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnInutilizarNumeracao_Click()

    If Not validacao Then
        MsgBox ("Erro Verifique as configurações do certificado")
        Exit Sub
    End If

    On Error GoTo Erro:
    Dim ret As String
    Dim ano As Long
    Dim modelo As Long
    Dim serie As Long
    Dim numeroInicial As Long
    Dim numeroFinal As Long
    Dim aJustificativa As String
    Dim eCNPJ As String

    ano = InputBox("Ano", "WebServices: Inutilização", 1)
    modelo = InputBox("Modelo", "WebServices: Inutilização", 57)
    serie = InputBox("Serie", "WebServices: Inutilização", 1)
    numeroInicial = InputBox("Numero Inicial", "WebServices: Inutilização", 1)
    numeroFinal = InputBox("Numero Final", "WebServices: Inutilização", 1)
    aJustificativa = InputBox("Justificativa", "WebServices: Inutilização", "")
    eCNPJ = InputBox("CNPJ ou CPF do Emitente", "WebServices: Inutilização", "")
    
    ret = nfe.Inutilizar(eCNPJ, aJustificativa, ano, modelo, serie, numeroInicial, numeroFinal)
    rtbRespostas.Text = ret
    
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnLimparListaEvento_Click()

    On Error GoTo Erro:
    answer = MsgBox("Limpar a lista de eventos ?", vbExclamation + vbYesNo, "Add Confirm")
    If answer = vbYes Then
    nfe.LimparListaEventos
    End If

Erro:
    MsgBox Err.Description

End Sub

Private Sub btnLimparListaNFe_Click()

    On Error GoTo Erro:
    answer = MsgBox("Limpar Lista ?", vbExclamation + vbYesNo, "Add Confirm")
    If answer = vbYes Then
    nfe.LimparLista
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
    
    On Error GoTo Erro:
    nfe.ObterCertificados
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

Private Sub btnStatusServ_Click()
    
    On Error Resume Next
    
    Dim retorno As Long
    
    SetResposta nfe.StatusServico
    If Err.Number <> 0 Then
       MsgBox Err.Description
       Exit Sub
    End If
    On Error GoTo 0

End Sub

Private Sub cmdGerarXMl_Click()
    CommonDialog1.DialogTitle = "Selecione o arquivo ini"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivo Ini NFe (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
                
    If CommonDialog1.FileName = vbNullString Then Exit Sub
        
    nfe.LimparLista
    nfe.CarregarINI (CommonDialog1.FileName)
    SetResposta nfe.ObterXml(0)
    
End Sub

Private Sub cmdGravarXml_Click()
    CommonDialog1.DialogTitle = "Selecione o arquivo ini"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivo Ini NFe (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
                
    If CommonDialog1.FileName = vbNullString Then Exit Sub
        
    nfe.LimparLista
    nfe.CarregarINI (CommonDialog1.FileName)
    nfe.GravarXml 0, "Teste.xml", App.Path
End Sub

Private Sub btnStatusServicos_Click()
    
    If Not validacao Then
        MsgBox ("Erro Verifique as configurações do certificado")
        Exit Sub
    End If
    
    rtbRespostas.Text = nfe.StatusServico
    
End Sub

Private Sub btnValidarRegraNegocio_Click()
        
    On Error GoTo Erro:
    
    CheckNFeLista
    
    rtbRespostas.Text = nfe.ValidarRegrasdeNegocios
    
Erro:
    MsgBox Err.Description
    
    
End Sub

Private Sub cmdSalvar_Click()
    SalvarConfig
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
    
    nfe.ConfigLer
    
    'Geral
    chkAtualizarXML.Value = CLng(nfe.ConfigLerValor(SESSAO_NFE, "AtualizarXMLCancelado"))
    chkExibirErroSchema.Value = CLng(nfe.ConfigLerValor(SESSAO_NFE, "ExibirErroSchema"))
    txtFormatoAlerta.Text = nfe.ConfigLerValor(SESSAO_NFE, "FormatoAlerta")
    cmbFormaEmissao.ListIndex = CLng(nfe.ConfigLerValor(SESSAO_NFE, "FormaEmissao"))
    cmbModeloDocumento.ListIndex = CLng(nfe.ConfigLerValor(SESSAO_NFE, "ModeloDF"))
    cmbVersaoDF.ListIndex = CLng(nfe.ConfigLerValor(SESSAO_NFE, "VersaoDF"))
    chkRetirarAcentos.Value = CLng(nfe.ConfigLerValor(SESSAO_NFE, "RetirarAcentos"))
    chkSalvar.Value = CLng(nfe.ConfigLerValor(SESSAO_NFE, "SalvarWS"))
    txtLogs.Text = nfe.ConfigLerValor(SESSAO_NFE, "PathSalvar")
    txtSchemaPath.Text = nfe.ConfigLerValor(SESSAO_NFE, "PathSchemas")
    txtIdCSC.Text = nfe.ConfigLerValor(SESSAO_NFE, "IdCSC")
    txtCSC.Text = nfe.ConfigLerValor(SESSAO_NFE, "CSC")
    
    'WebSevice
    cmbUFDestino.Text = nfe.ConfigLerValor(SESSAO_DFE, "UF")
    cmbSSlType.ListIndex = CLng(nfe.ConfigLerValor(SESSAO_NFE, "SSLType"))
    nudTimeOut.Value = CLng(nfe.ConfigLerValor(SESSAO_NFE, "Timeout"))
    
    Dim ambiente As String
    ambiente = nfe.ConfigLerValor(SESSAO_NFE, "Ambiente")
    rdbHomologacao.Value = CBool(ambiente)
    rdbProducao.Value = Not CBool(ambiente)
    
    chkVisualizarMensagem.Value = CLng(nfe.ConfigLerValor(SESSAO_NFE, "Visualizar"))
    chkSalvarSOAP.Value = CLng(nfe.ConfigLerValor(SESSAO_NFE, "SalvarWS"))
    chkAjustAut.Value = CLng(nfe.ConfigLerValor(SESSAO_NFE, "AjustaAguardaConsultaRet"))
    nudAguardar.Value = CLng(nfe.ConfigLerValor(SESSAO_NFE, "AguardarConsultaRet"))
    nudTentativas.Value = CLng(nfe.ConfigLerValor(SESSAO_NFE, "Tentativas"))
    nudIntervalo.Value = CLng(nfe.ConfigLerValor(SESSAO_NFE, "IntervaloTentativas"))
    txtProxyServidor.Text = nfe.ConfigLerValor(SESSAO_PROXY, "Servidor")
    nudProxyPorta.Value = CLng(nfe.ConfigLerValor(SESSAO_PROXY, "Porta"))
    txtProxyUsuario.Text = nfe.ConfigLerValor(SESSAO_PROXY, "Usuario")
    txtProxySenha.Text = nfe.ConfigLerValor(SESSAO_PROXY, "Senha")
    
    'Certificado
    cmbCrypt.ListIndex = CLng(nfe.ConfigLerValor(SESSAO_DFE, "SSLCryptLib"))
    cmbHttp.ListIndex = CLng(nfe.ConfigLerValor(SESSAO_DFE, "SSLHttpLib"))
    cmbXmlSign.ListIndex = CLng(nfe.ConfigLerValor(SESSAO_DFE, "SSLXmlSignLib"))
    txtCertPath.Text = nfe.ConfigLerValor(SESSAO_DFE, "ArquivoPFX")
    txtCertPassword.Text = nfe.ConfigLerValor(SESSAO_DFE, "Senha")
    txtCertNumero.Text = nfe.ConfigLerValor(SESSAO_DFE, "NumeroSerie")
    txtDadosPFX.Text = nfe.ConfigLerValor(SESSAO_DFE, "DadosPFX")
    
    'Arquivos
    chkSalvarArqs.Value = CLng(nfe.ConfigLerValor(SESSAO_NFE, "SalvarGer"))
    chkPastaMensal.Value = CLng(nfe.ConfigLerValor(SESSAO_NFE, "SepararPorMes"))
    chkAdicionaLiteral.Value = CLng(nfe.ConfigLerValor(SESSAO_NFE, "AdicionarLiteral"))
    chkEmissaoPathNFe.Value = CLng(nfe.ConfigLerValor(SESSAO_NFE, "EmissaoPathNFe"))
    chkSalvaPathEvento.Value = CLng(nfe.ConfigLerValor(SESSAO_NFE, "SalvarArq"))
    chkSepararPorCNPJ.Value = CLng(nfe.ConfigLerValor(SESSAO_NFE, "SepararPorCNPJ"))
    chkSepararPorModelo.Value = CLng(nfe.ConfigLerValor(SESSAO_NFE, "SepararPorModelo"))
    txtArqNFe.Text = nfe.ConfigLerValor(SESSAO_NFE, "PathNFe")
    txtArqInu.Text = nfe.ConfigLerValor(SESSAO_NFE, "PathInu")
    txtArqEvento.Text = nfe.ConfigLerValor(SESSAO_NFE, "PathEvento")
    
    'Doc Auxiliar
    txtLogomarca.Text = nfe.ConfigLerValor(SESSAO_DANFE, "PathLogo")
    
    Dim tipoImpressao As String
    tipoImpressao = nfe.ConfigLerValor(SESSAO_DANFE, "TipoDANFE")
    rdbRetrato.Value = CBool(tipoImpressao)
    rdbPaisagem.Value = Not CBool(tipoImpressao)
    
    Dim relNFCe As String
    relNFCe = nfe.ConfigLerValor(SESSAO_DANFENFCe, "TipoRelatorioBobina")
    rdbFortes.Value = CBool(relNFCe)
    rdbEscPos.Value = Not CBool(relNFCe)
    rdbFortesA4.Value = Not CBool(relNFCe)
    
    cmbModelo.ListIndex = CLng(nfe.ConfigLerValor(SESSAO_POSPRINTER, "Modelo"))
    cmbPortas.ListIndex = CLng(nfe.ConfigLerValor(SESSAO_POSPRINTER, "Porta"))
    nudColunas.Value = CLng(nfe.ConfigLerValor(SESSAO_POSPRINTER, "ColunasFonteNormal"))
    nudEspacos.Value = CLng(nfe.ConfigLerValor(SESSAO_POSPRINTER, "EspacoEntreLinhas"))
    nudBuffer.Value = CLng(nfe.ConfigLerValor(SESSAO_POSPRINTER, "LinhasBuffer"))
    nudLinhasPular = CLng(nfe.ConfigLerValor(SESSAO_POSPRINTER, "LinhasEntreCupons"))
    chkControlePorta.Value = CLng(nfe.ConfigLerValor(SESSAO_POSPRINTER, "ControlePorta"))
    chkCortarPapel.Value = CLng(nfe.ConfigLerValor(SESSAO_POSPRINTER, "CortaPapel"))
    chkTraduzirTags.Value = CLng(nfe.ConfigLerValor(SESSAO_POSPRINTER, "TraduzirTags"))
    chkIgnorarTags.Value = CLng(nfe.ConfigLerValor(SESSAO_POSPRINTER, "IgnorarTags"))
    cmbPaginaCodigo.ListIndex = CLng(nfe.ConfigLerValor(SESSAO_POSPRINTER, "PaginaDeCodigo"))
    
    'Email
    txtNome.Text = nfe.ConfigLerValor(SESSAO_EMAIL, "Nome")
    txtEmail.Text = nfe.ConfigLerValor(SESSAO_EMAIL, "Conta")
    txtUsuario.Text = nfe.ConfigLerValor(SESSAO_EMAIL, "Usuario")
    txtSenha.Text = nfe.ConfigLerValor(SESSAO_EMAIL, "Senha")
    txtHost.Text = nfe.ConfigLerValor(SESSAO_EMAIL, "Servidor")
    nudEmailPorta.Value = CLng(nfe.ConfigLerValor(SESSAO_EMAIL, "Porta"))
    chkSSL.Value = CLng(nfe.ConfigLerValor(SESSAO_EMAIL, "SSL"))
    chkTLS.Value = CLng(nfe.ConfigLerValor(SESSAO_EMAIL, "TLS"))
    
End Sub

Private Sub SalvarConfig()
    
    'Geral
    nfe.ConfigGravarValor SESSAO_NFE, "AtualizarXMLCancelado", CStr(chkAtualizarXML.Value)
    nfe.ConfigGravarValor SESSAO_NFE, "ExibirErroSchema", CStr(chkExibirErroSchema.Value)
    nfe.ConfigGravarValor SESSAO_NFE, "FormatoAlerta", txtFormatoAlerta.Text
    nfe.ConfigGravarValor SESSAO_NFE, "FormaEmissao", CStr(cmbFormaEmissao.ListIndex)
    nfe.ConfigGravarValor SESSAO_NFE, "ModeloDF", CStr(cmbModeloDocumento.ListIndex)
    nfe.ConfigGravarValor SESSAO_NFE, "VersaoDF", CStr(cmbVersaoDF.ListIndex)
    nfe.ConfigGravarValor SESSAO_NFE, "RetirarAcentos", CStr(chkRetirarAcentos.Value)
    nfe.ConfigGravarValor SESSAO_NFE, "SalvarWS", CStr(chkSalvar.Value)
    nfe.ConfigGravarValor SESSAO_NFE, "PathSalvar", txtLogs.Text
    nfe.ConfigGravarValor SESSAO_NFE, "PathSchemas", txtSchemaPath.Text
    nfe.ConfigGravarValor SESSAO_NFE, "IdCSC", txtIdCSC.Text
    nfe.ConfigGravarValor SESSAO_NFE, "CSC", txtCSC.Text
    
    'WebService
    nfe.ConfigGravarValor SESSAO_DFE, "UF", CStr(cmbUFDestino.ListIndex)
    nfe.ConfigGravarValor SESSAO_NFE, "SSLType", CStr(cmbSSlType.ListIndex)
    nfe.ConfigGravarValor SESSAO_NFE, "Timeout", CStr(nudTimeOut.Value)
    nfe.ConfigGravarValor SESSAO_NFE, "Ambiente", CStr(rdbProducao.Value)
    nfe.ConfigGravarValor SESSAO_NFE, "Ambiente", CStr(rdbHomologacao.Value)
    nfe.ConfigGravarValor SESSAO_NFE, "Visualizar", CStr(chkVisualizarMensagem)
    nfe.ConfigGravarValor SESSAO_NFE, "SalvarWS", CStr(chkSalvarSOAP.Value)
    nfe.ConfigGravarValor SESSAO_NFE, "AjustaAguardaConsultaRet", CStr(chkAjustAut.Value)
    nfe.ConfigGravarValor SESSAO_NFE, "AguardarConsultaRet", CStr(nudAguardar.Value)
    nfe.ConfigGravarValor SESSAO_NFE, "Tentativas", CStr(nudTentativas.Value)
    nfe.ConfigGravarValor SESSAO_NFE, "IntervaloTentativas", CStr(nudIntervalo.Value)
    nfe.ConfigGravarValor SESSAO_PROXY, "Servidor", txtProxyServidor.Text
    nfe.ConfigGravarValor SESSAO_PROXY, "Porta", CStr(nudProxyPorta.Value)
    nfe.ConfigGravarValor SESSAO_PROXY, "Usuario", txtProxyUsuario.Text
    nfe.ConfigGravarValor SESSAO_PROXY, "Senha", txtProxySenha.Text
    
    'Certificado
    nfe.ConfigGravarValor SESSAO_DFE, "SSLCryptLib", CStr(cmbCrypt.ListIndex)
    nfe.ConfigGravarValor SESSAO_DFE, "SSLHttpLib", CStr(cmbHttp.ListIndex)
    nfe.ConfigGravarValor SESSAO_DFE, "SSLXmlSignLib", CStr(cmbXmlSign.ListIndex)
    nfe.ConfigGravarValor SESSAO_DFE, "ArquivoPFX", txtCertPath.Text
    nfe.ConfigGravarValor SESSAO_DFE, "Senha", txtCertPassword.Text
    nfe.ConfigGravarValor SESSAO_DFE, "NumeroSerie", txtCertNumero.Text
    nfe.ConfigGravarValor SESSAO_DFE, "DadosPFX", txtDadosPFX.Text
    
    'Arquivos
    nfe.ConfigGravarValor SESSAO_NFE, "SalvarGer", CStr(chkSalvarArqs.Value)
    nfe.ConfigGravarValor SESSAO_NFE, "SepararPorMes", CStr(chkPastaMensal.Value)
    nfe.ConfigGravarValor SESSAO_NFE, "AdicionarLiteral", CStr(chkAdicionaLiteral.Value)
    nfe.ConfigGravarValor SESSAO_NFE, "EmissaoPathNFe", CStr(chkEmissaoPathNFe.Value)
    nfe.ConfigGravarValor SESSAO_NFE, "SalvarArq", CStr(chkSalvaPathEvento.Value)
    nfe.ConfigGravarValor SESSAO_NFE, "SepararPorCNPJ", CStr(chkSepararPorCNPJ.Value)
    nfe.ConfigGravarValor SESSAO_NFE, "SepararPorModelo", CStr(chkSepararPorModelo.Value)
    nfe.ConfigGravarValor SESSAO_NFE, "PathNFe", txtArqNFe.Text
    nfe.ConfigGravarValor SESSAO_NFE, "PathInu", txtArqInu.Text
    nfe.ConfigGravarValor SESSAO_NFE, "PathEvento", txtArqEvento.Text
    
    'Doc Auxiliar
    nfe.ConfigGravarValor SESSAO_DANFE, "PathLogo", txtLogomarca.Text
    nfe.ConfigGravarValor SESSAO_DANFENFCe, "TipoRelatorioBobina", CStr(rdbFortes.Value)
    nfe.ConfigGravarValor SESSAO_DANFENFCe, "TipoRelatorioBobina", CStr(rdbFortesA4.Value)
    nfe.ConfigGravarValor SESSAO_DANFENFCe, "TipoRelatorioBobina", CStr(rdbEscPos.Value)
    nfe.ConfigGravarValor SESSAO_POSPRINTER, "Modelo", CStr(cmbModelo.ListIndex)
    nfe.ConfigGravarValor SESSAO_POSPRINTER, "Porta", CStr(cmbPortas.ListIndex)
    nfe.ConfigGravarValor SESSAO_POSPRINTER, "ColunasFonteNormal", CStr(nudColunas.Value)
    nfe.ConfigGravarValor SESSAO_POSPRINTER, "EspacoEntreLinhas", CStr(nudEspacos.Value)
    nfe.ConfigGravarValor SESSAO_POSPRINTER, "LinhasBuffer", CStr(nudBuffer.Value)
    nfe.ConfigGravarValor SESSAO_POSPRINTER, "LinhasEntreCupons", CStr(nudLinhasPular.Value)
    nfe.ConfigGravarValor SESSAO_POSPRINTER, "ControlePorta", CStr(chkControlePorta.Value)
    nfe.ConfigGravarValor SESSAO_POSPRINTER, "CortaPapel", CStr(chkCortarPapel.Value)
    nfe.ConfigGravarValor SESSAO_POSPRINTER, "TraduzirTags", CStr(chkTraduzirTags.Value)
    nfe.ConfigGravarValor SESSAO_POSPRINTER, "IgnorarTags", CStr(chkIgnorarTags.Value)
    nfe.ConfigGravarValor SESSAO_POSPRINTER, "PaginaDeCodigo", CStr(cmbPaginaCodigo.ListIndex)
    
    'Email
    nfe.ConfigGravarValor SESSAO_EMAIL, "Nome", txtNome.Text
    nfe.ConfigGravarValor SESSAO_EMAIL, "Conta", txtEmail.Text
    nfe.ConfigGravarValor SESSAO_EMAIL, "Usuario", txtUsuario.Text
    nfe.ConfigGravarValor SESSAO_EMAIL, "Senha", txtSenha.Text
    nfe.ConfigGravarValor SESSAO_EMAIL, "Servidor", txtHost.Text
    nfe.ConfigGravarValor SESSAO_EMAIL, "Porta", CStr(nudEmailPorta.Value)
    nfe.ConfigGravarValor SESSAO_EMAIL, "SSL", CStr(chkSSL.Value)
    nfe.ConfigGravarValor SESSAO_EMAIL, "TLS", CStr(chkTLS.Value)
    
    nfe.ConfigGravar

End Sub
