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
      Left            =   240
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
      Tab(1).Control(0).Enabled=   0   'False
      Tab(1).Control(1)=   "btnConsultarRecibo"
      Tab(1).Control(1).Enabled=   0   'False
      Tab(1).Control(2)=   "btnConsultaXXML"
      Tab(1).Control(2).Enabled=   0   'False
      Tab(1).Control(3)=   "btnConsultarCadastro"
      Tab(1).Control(3).Enabled=   0   'False
      Tab(1).Control(4)=   "btnConsultaChave"
      Tab(1).Control(4).Enabled=   0   'False
      Tab(1).ControlCount=   5
      TabCaption(2)   =   "Eventos"
      TabPicture(2)   =   "FrmMain.frx":0038
      Tab(2).ControlEnabled=   0   'False
      Tab(2).Control(0)=   "btnCancelarNFe"
      Tab(2).Control(0).Enabled=   0   'False
      Tab(2).Control(1)=   "btnCarregarEvento"
      Tab(2).Control(1).Enabled=   0   'False
      Tab(2).Control(2)=   "btnImprimirEvento"
      Tab(2).Control(2).Enabled=   0   'False
      Tab(2).Control(3)=   "btnEnviarEmailEvento"
      Tab(2).Control(3).Enabled=   0   'False
      Tab(2).Control(4)=   "btnEnviarEvento"
      Tab(2).Control(4).Enabled=   0   'False
      Tab(2).Control(5)=   "btnLimparListaEvento"
      Tab(2).Control(5).Enabled=   0   'False
      Tab(2).Control(6)=   "btnImprimirEventoPDF"
      Tab(2).Control(6).Enabled=   0   'False
      Tab(2).ControlCount=   7
      TabCaption(3)   =   "Inutilização"
      TabPicture(3)   =   "FrmMain.frx":0054
      Tab(3).ControlEnabled=   0   'False
      Tab(3).Control(0)=   "btnInutilizarNumeracao"
      Tab(3).Control(0).Enabled=   0   'False
      Tab(3).Control(1)=   "btnImprimirInutilizacao"
      Tab(3).Control(1).Enabled=   0   'False
      Tab(3).Control(2)=   "btnImprimirInutilizacaoPDF"
      Tab(3).Control(2).Enabled=   0   'False
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
         Caption         =   "Imprimir PDF"
         Height          =   375
         Left            =   -72960
         TabIndex        =   31
         Top             =   960
         Width           =   1575
      End
      Begin VB.CommandButton btnImprimirInutilizacao 
         Caption         =   "Imprimir"
         Height          =   375
         Left            =   -74760
         TabIndex        =   30
         Top             =   960
         Width           =   1575
      End
      Begin VB.CommandButton btnInutilizarNumeracao 
         Caption         =   "Inutilizar"
         Height          =   375
         Left            =   -74760
         TabIndex        =   29
         Top             =   480
         Width           =   1575
      End
      Begin VB.CommandButton btnImprimirEventoPDF 
         Caption         =   "Imprimir PDF"
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
         Caption         =   "Enviar Email"
         Height          =   375
         Left            =   -74640
         TabIndex        =   25
         Top             =   1920
         Width           =   1575
      End
      Begin VB.CommandButton btnImprimirEvento 
         Caption         =   "Imprimir"
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
         Width           =   1575
      End
      Begin VB.CommandButton btnEnviarAssincrono 
         Caption         =   "Enviar Assincrono"
         Height          =   375
         Left            =   3840
         TabIndex        =   15
         Top             =   480
         Width           =   1575
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
      Left            =   2880
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
      Tab(1).Control(0)=   "lblLogomarca"
      Tab(1).Control(1)=   "frmDANFCe"
      Tab(1).Control(2)=   "frmPosPrinter"
      Tab(1).Control(3)=   "frmDANFe"
      Tab(1).Control(4)=   "txtLogomarca"
      Tab(1).Control(5)=   "btnLogomarca"
      Tab(1).ControlCount=   6
      TabCaption(2)   =   "Email"
      TabPicture(2)   =   "FrmMain.frx":00C4
      Tab(2).ControlEnabled=   0   'False
      Tab(2).Control(0)=   "Label13"
      Tab(2).Control(1)=   "Label22"
      Tab(2).Control(2)=   "Frame2"
      Tab(2).Control(3)=   "Text7"
      Tab(2).Control(4)=   "Text8"
      Tab(2).ControlCount=   5
      Begin VB.CommandButton btnLogomarca 
         Caption         =   "..."
         Height          =   260
         Left            =   -70320
         TabIndex        =   149
         Top             =   960
         Width           =   390
      End
      Begin VB.TextBox txtLogomarca 
         Height          =   285
         Left            =   -74640
         TabIndex        =   148
         Top             =   960
         Width           =   4275
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
         TabIndex        =   144
         Top             =   1320
         Width           =   4695
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
            TabIndex        =   146
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
            TabIndex        =   145
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
         TabIndex        =   143
         Top             =   3000
         Width           =   4695
         Begin VB.CheckBox chkIgnorarTags 
            Caption         =   "Ignorar Tags"
            Height          =   255
            Left            =   2640
            TabIndex        =   172
            Top             =   3000
            Width           =   1455
         End
         Begin VB.CheckBox chkCortarPapel 
            Caption         =   "Cortar Papel"
            Height          =   255
            Left            =   480
            TabIndex        =   171
            Top             =   3000
            Width           =   1455
         End
         Begin VB.CheckBox chkTraduzirTags 
            Caption         =   "Traduzir Tags"
            Height          =   255
            Left            =   2640
            TabIndex        =   170
            Top             =   2520
            Width           =   1455
         End
         Begin VB.CheckBox chkControlePorta 
            Caption         =   "Controle Porta"
            Height          =   255
            Left            =   480
            TabIndex        =   169
            Top             =   2520
            Width           =   1455
         End
         Begin VB.TextBox txtLinhasPular 
            Alignment       =   1  'Right Justify
            Height          =   285
            Left            =   3360
            TabIndex        =   167
            Text            =   "0"
            Top             =   2040
            Width           =   975
         End
         Begin VB.TextBox txtBuffer 
            Alignment       =   1  'Right Justify
            Height          =   285
            Left            =   2040
            TabIndex        =   164
            Text            =   "0"
            Top             =   2040
            Width           =   975
         End
         Begin VB.TextBox txtEspacos 
            Alignment       =   1  'Right Justify
            Height          =   285
            Left            =   1080
            TabIndex        =   161
            Text            =   "0"
            Top             =   2040
            Width           =   615
         End
         Begin VB.TextBox txtColunas 
            Alignment       =   1  'Right Justify
            Height          =   285
            Left            =   120
            TabIndex        =   157
            Text            =   "0"
            Top             =   2040
            Width           =   615
         End
         Begin VB.ComboBox cmbPortas 
            Height          =   315
            ItemData        =   "FrmMain.frx":00E0
            Left            =   120
            List            =   "FrmMain.frx":00F3
            Style           =   2  'Dropdown List
            TabIndex        =   153
            Top             =   1320
            Width           =   4455
         End
         Begin VB.ComboBox cmbPaginaCodigo 
            Height          =   315
            ItemData        =   "FrmMain.frx":012D
            Left            =   2400
            List            =   "FrmMain.frx":0140
            Style           =   2  'Dropdown List
            TabIndex        =   152
            Top             =   600
            Width           =   2175
         End
         Begin VB.ComboBox cmbModelo 
            Height          =   315
            ItemData        =   "FrmMain.frx":017A
            Left            =   120
            List            =   "FrmMain.frx":018D
            Style           =   2  'Dropdown List
            TabIndex        =   151
            Top             =   600
            Width           =   2175
         End
         Begin MSComCtl2.UpDown nudColunas 
            Height          =   285
            Left            =   720
            TabIndex        =   158
            Top             =   2040
            Width           =   255
            _ExtentX        =   450
            _ExtentY        =   503
            _Version        =   393216
            Value           =   5000
            OrigLeft        =   4560
            OrigTop         =   720
            OrigRight       =   4815
            OrigBottom      =   1005
            Max             =   99999
            Enabled         =   -1  'True
         End
         Begin MSComCtl2.UpDown nudEspacos 
            Height          =   285
            Left            =   1680
            TabIndex        =   162
            Top             =   2040
            Width           =   255
            _ExtentX        =   450
            _ExtentY        =   503
            _Version        =   393216
            Value           =   5000
            OrigLeft        =   4560
            OrigTop         =   720
            OrigRight       =   4815
            OrigBottom      =   1005
            Max             =   99999
            Enabled         =   -1  'True
         End
         Begin MSComCtl2.UpDown nudBuffer 
            Height          =   285
            Left            =   3000
            TabIndex        =   165
            Top             =   2040
            Width           =   255
            _ExtentX        =   450
            _ExtentY        =   503
            _Version        =   393216
            Value           =   5000
            OrigLeft        =   4560
            OrigTop         =   720
            OrigRight       =   4815
            OrigBottom      =   1005
            Max             =   99999
            Enabled         =   -1  'True
         End
         Begin MSComCtl2.UpDown nudLinhasPular 
            Height          =   285
            Left            =   4320
            TabIndex        =   168
            Top             =   2040
            Width           =   255
            _ExtentX        =   450
            _ExtentY        =   503
            _Version        =   393216
            Value           =   5000
            OrigLeft        =   4560
            OrigTop         =   720
            OrigRight       =   4815
            OrigBottom      =   1005
            Max             =   99999
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
            TabIndex        =   166
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
            TabIndex        =   163
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
            TabIndex        =   160
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
            TabIndex        =   159
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
            TabIndex        =   156
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
            TabIndex        =   155
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
            TabIndex        =   154
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
         TabIndex        =   140
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
            TabIndex        =   150
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
            TabIndex        =   142
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
            TabIndex        =   141
            Top             =   360
            Width           =   1455
         End
      End
      Begin VB.TextBox Text8 
         Height          =   1365
         IMEMode         =   3  'DISABLE
         Left            =   -74400
         MultiLine       =   -1  'True
         ScrollBars      =   3  'Both
         TabIndex        =   119
         Top             =   5760
         Width           =   4155
      End
      Begin VB.TextBox Text7 
         Height          =   285
         IMEMode         =   3  'DISABLE
         Left            =   -74400
         TabIndex        =   117
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
         TabIndex        =   103
         Top             =   600
         Width           =   4215
         Begin VB.TextBox txtnudPorta 
            Height          =   285
            Left            =   120
            TabIndex        =   178
            Top             =   2280
            Width           =   735
         End
         Begin VB.TextBox txtSenha 
            Height          =   285
            Left            =   120
            TabIndex        =   177
            Top             =   3480
            Width           =   3855
         End
         Begin VB.TextBox txtUsuario 
            Height          =   285
            Left            =   120
            TabIndex        =   176
            Top             =   2880
            Width           =   3855
         End
         Begin VB.TextBox txtNome 
            Height          =   285
            IMEMode         =   3  'DISABLE
            Left            =   120
            TabIndex        =   108
            Top             =   480
            Width           =   3915
         End
         Begin VB.TextBox txtEmail 
            Height          =   285
            IMEMode         =   3  'DISABLE
            Left            =   120
            TabIndex        =   107
            Top             =   1080
            Width           =   3915
         End
         Begin VB.TextBox txtHost 
            Height          =   285
            IMEMode         =   3  'DISABLE
            Left            =   120
            TabIndex        =   106
            Top             =   1680
            Width           =   3915
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
            TabIndex        =   105
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
            TabIndex        =   104
            Top             =   2400
            Width           =   1335
         End
         Begin MSComCtl2.UpDown nudPorta 
            Height          =   285
            Index           =   2
            Left            =   841
            TabIndex        =   109
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
            TabIndex        =   115
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
            TabIndex        =   114
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
            TabIndex        =   113
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
            TabIndex        =   112
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
            TabIndex        =   111
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
            TabIndex        =   110
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
         TabPicture(0)   =   "FrmMain.frx":01C7
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
         Tab(0).Control(12)=   "cmbVersaoDocFiscal"
         Tab(0).Control(12).Enabled=   0   'False
         Tab(0).Control(13)=   "chkRetirarAcentosXMLEnv"
         Tab(0).Control(13).Enabled=   0   'False
         Tab(0).Control(14)=   "chkSalvarArqEnvResp"
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
         TabPicture(1)   =   "FrmMain.frx":01E3
         Tab(1).ControlEnabled=   0   'False
         Tab(1).Control(0)=   "lblTimeOut(1)"
         Tab(1).Control(1)=   "lblSSLType"
         Tab(1).Control(2)=   "lblUFDestino"
         Tab(1).Control(3)=   "nudTimeOut"
         Tab(1).Control(4)=   "chkSalvarEnvelopeSOAP"
         Tab(1).Control(5)=   "chkVisualizarMensagem"
         Tab(1).Control(6)=   "frmRetEnvio"
         Tab(1).Control(7)=   "frmProxy"
         Tab(1).Control(8)=   "txtTimeOut"
         Tab(1).Control(9)=   "cmbSSlType"
         Tab(1).Control(10)=   "cmbUFDestino"
         Tab(1).Control(11)=   "frmAmbiente"
         Tab(1).ControlCount=   12
         TabCaption(2)   =   "Certificados"
         TabPicture(2)   =   "FrmMain.frx":01FF
         Tab(2).ControlEnabled=   0   'False
         Tab(2).Control(0)=   "lblCryptLib"
         Tab(2).Control(1)=   "lblHttpLib"
         Tab(2).Control(2)=   "lblXMLSignLib"
         Tab(2).Control(3)=   "cmbCrypt"
         Tab(2).Control(4)=   "cmbHttp"
         Tab(2).Control(5)=   "cmbXmlSign"
         Tab(2).Control(6)=   "frmCertificados"
         Tab(2).Control(7)=   "btnObterCertificados"
         Tab(2).ControlCount=   8
         TabCaption(3)   =   "Arquivos"
         TabPicture(3)   =   "FrmMain.frx":021B
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
         Begin VB.ComboBox cmbModeloDocumento 
            Height          =   315
            ItemData        =   "FrmMain.frx":0237
            Left            =   120
            List            =   "FrmMain.frx":023E
            Style           =   2  'Dropdown List
            TabIndex        =   173
            Top             =   3000
            Width           =   2295
         End
         Begin VB.TextBox txtArqEvento 
            Height          =   285
            Left            =   -74880
            TabIndex        =   139
            Top             =   4440
            Width           =   3555
         End
         Begin VB.CommandButton btnArqEvento 
            Caption         =   "..."
            Height          =   260
            Left            =   -71280
            TabIndex        =   138
            Top             =   4440
            Width           =   390
         End
         Begin VB.CommandButton btnArqInu 
            Caption         =   "..."
            Height          =   260
            Left            =   -71280
            TabIndex        =   137
            Top             =   3840
            Width           =   390
         End
         Begin VB.TextBox txtArqInu 
            Height          =   285
            Left            =   -74880
            TabIndex        =   136
            Top             =   3840
            Width           =   3555
         End
         Begin VB.CommandButton btnArqNFe 
            Caption         =   "..."
            Height          =   260
            Left            =   -71280
            TabIndex        =   133
            Top             =   3240
            Width           =   390
         End
         Begin VB.TextBox txtArqNFe 
            Height          =   285
            Left            =   -74880
            TabIndex        =   132
            Top             =   3240
            Width           =   3555
         End
         Begin VB.CheckBox chkSepararPorModelo 
            Caption         =   "Separar Arqs pelo Modelo do Documento"
            Height          =   195
            Left            =   -74880
            TabIndex        =   130
            Top             =   2520
            Width           =   3495
         End
         Begin VB.CheckBox chkSepararPorCNPJ 
            Caption         =   "Separar Arqs pelo CNPJ do Certificado"
            Height          =   195
            Left            =   -74880
            TabIndex        =   129
            Top             =   2160
            Width           =   3615
         End
         Begin VB.CheckBox chkSalvaPathEvento 
            Caption         =   "Salvar Arquivos de Eventos"
            Height          =   195
            Left            =   -74880
            TabIndex        =   128
            Top             =   1800
            Width           =   3855
         End
         Begin VB.CheckBox chkEmissaoPathNFe 
            Caption         =   "Salvar Documento pelo campo Data de Emissão"
            Height          =   195
            Left            =   -74880
            TabIndex        =   127
            Top             =   1440
            Width           =   4455
         End
         Begin VB.CheckBox chkAdicionaLiteral 
            Caption         =   "Adicionar Literal no nome das pastas"
            Height          =   195
            Left            =   -74880
            TabIndex        =   126
            Top             =   1080
            Width           =   3615
         End
         Begin VB.CheckBox chkPastaMensal 
            Caption         =   "Criar Pastas Mensalmente"
            Height          =   255
            Left            =   -74880
            TabIndex        =   125
            Top             =   720
            Width           =   3855
         End
         Begin VB.CheckBox chkSalvarArqs 
            Caption         =   "Salvar Arquivos em Pastas Separadas"
            Height          =   195
            Left            =   -74880
            TabIndex        =   124
            Top             =   420
            Width           =   4335
         End
         Begin VB.CommandButton btnObterCertificados 
            Caption         =   "Obter Certificados"
            Height          =   375
            Left            =   -74640
            TabIndex        =   120
            Top             =   5160
            Width           =   1575
         End
         Begin VB.TextBox txtCSC 
            Height          =   285
            Left            =   120
            TabIndex        =   87
            Top             =   6840
            Width           =   4695
         End
         Begin VB.TextBox txtIdCSC 
            Height          =   285
            Left            =   120
            TabIndex        =   86
            Top             =   6240
            Width           =   4695
         End
         Begin VB.TextBox txtSchemaPath 
            Height          =   285
            Left            =   120
            TabIndex        =   85
            Top             =   5640
            Width           =   4215
         End
         Begin VB.CommandButton btnSelectSchema 
            Caption         =   "..."
            Height          =   260
            Left            =   4440
            TabIndex        =   84
            Top             =   5640
            Width           =   390
         End
         Begin VB.TextBox txtLogs 
            Height          =   285
            Left            =   120
            TabIndex        =   83
            Top             =   5040
            Width           =   4215
         End
         Begin VB.CommandButton btnSelectLog 
            Caption         =   "..."
            Height          =   260
            Left            =   4440
            TabIndex        =   82
            Top             =   5040
            Width           =   390
         End
         Begin VB.CheckBox chkSalvarArqEnvResp 
            Caption         =   "Salvar Arquivos de Envio e Resposta"
            Height          =   255
            Left            =   120
            TabIndex        =   81
            Top             =   4440
            Width           =   3375
         End
         Begin VB.CheckBox chkRetirarAcentosXMLEnv 
            Caption         =   "Retirar Acentos dos XMLs enviados"
            Height          =   255
            Left            =   120
            TabIndex        =   80
            Top             =   4080
            Width           =   3375
         End
         Begin VB.ComboBox cmbVersaoDocFiscal 
            Height          =   315
            Left            =   120
            Style           =   2  'Dropdown List
            TabIndex        =   79
            Top             =   3600
            Width           =   2295
         End
         Begin VB.ComboBox cmbFormaEmissao 
            Height          =   315
            Left            =   120
            Style           =   2  'Dropdown List
            TabIndex        =   78
            Top             =   2400
            Width           =   2295
         End
         Begin VB.TextBox txtFormatoAlerta 
            Height          =   285
            Left            =   120
            TabIndex        =   77
            Top             =   1680
            Width           =   4695
         End
         Begin VB.CheckBox chkAtualizarXML 
            Caption         =   "Atualizar XML"
            Height          =   255
            Left            =   120
            TabIndex        =   76
            Top             =   600
            Width           =   3375
         End
         Begin VB.CheckBox chkExibirErroSchema 
            Caption         =   "Exibir Erro Schema"
            Height          =   255
            Left            =   120
            TabIndex        =   75
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
            TabIndex        =   72
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
               TabIndex        =   74
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
               Left            =   240
               TabIndex        =   73
               Top             =   360
               Width           =   1095
            End
         End
         Begin VB.ComboBox cmbUFDestino 
            Height          =   315
            ItemData        =   "FrmMain.frx":0245
            Left            =   -74760
            List            =   "FrmMain.frx":029A
            Style           =   2  'Dropdown List
            TabIndex        =   71
            Top             =   720
            Width           =   975
         End
         Begin VB.ComboBox cmbSSlType 
            Height          =   315
            ItemData        =   "FrmMain.frx":030A
            Left            =   -73440
            List            =   "FrmMain.frx":0323
            Style           =   2  'Dropdown List
            TabIndex        =   70
            Top             =   720
            Width           =   1695
         End
         Begin VB.TextBox txtTimeOut 
            Alignment       =   1  'Right Justify
            Height          =   285
            Left            =   -71400
            TabIndex        =   69
            Text            =   "5000"
            Top             =   720
            Width           =   990
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
            TabIndex        =   61
            Top             =   4560
            Width           =   4695
            Begin VB.TextBox txtProxySenha 
               Height          =   285
               Left            =   120
               TabIndex        =   175
               Top             =   1680
               Width           =   3855
            End
            Begin VB.TextBox txtProxyUsuario 
               Height          =   285
               Left            =   120
               TabIndex        =   174
               Top             =   1080
               Width           =   3855
            End
            Begin VB.TextBox txtProxyServidor 
               Height          =   285
               Left            =   120
               TabIndex        =   63
               Top             =   480
               Width           =   2655
            End
            Begin VB.TextBox txtPorta 
               Alignment       =   1  'Right Justify
               Height          =   285
               Index           =   1
               Left            =   2880
               TabIndex        =   62
               Text            =   "5000"
               Top             =   480
               Width           =   945
            End
            Begin MSComCtl2.UpDown nudPorta 
               Height          =   285
               Index           =   1
               Left            =   3825
               TabIndex        =   64
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
               TabIndex        =   68
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
               TabIndex        =   67
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
               TabIndex        =   66
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
               TabIndex        =   65
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
               Width           =   990
            End
            Begin VB.TextBox txtTentativas 
               Alignment       =   1  'Right Justify
               Height          =   285
               Left            =   1800
               TabIndex        =   52
               Text            =   "0"
               Top             =   960
               Width           =   990
            End
            Begin VB.TextBox txtIntervalos 
               Alignment       =   1  'Right Justify
               Height          =   285
               Left            =   3360
               TabIndex        =   51
               Text            =   "0"
               Top             =   960
               Width           =   990
            End
            Begin MSComCtl2.UpDown nudAguardar 
               Height          =   285
               Left            =   1080
               TabIndex        =   55
               Top             =   960
               Width           =   255
               _ExtentX        =   450
               _ExtentY        =   503
               _Version        =   393216
               Value           =   5000
               OrigLeft        =   1080
               OrigTop         =   960
               OrigRight       =   1335
               OrigBottom      =   1245
               Max             =   99999
               Enabled         =   -1  'True
            End
            Begin MSComCtl2.UpDown nudTentativas 
               Height          =   285
               Left            =   2760
               TabIndex        =   56
               Top             =   960
               Width           =   255
               _ExtentX        =   450
               _ExtentY        =   503
               _Version        =   393216
               Value           =   5000
               OrigLeft        =   2760
               OrigTop         =   960
               OrigRight       =   3015
               OrigBottom      =   1245
               Max             =   99999
               Enabled         =   -1  'True
            End
            Begin MSComCtl2.UpDown nudIntervalos 
               Height          =   285
               Left            =   4320
               TabIndex        =   57
               Top             =   960
               Width           =   255
               _ExtentX        =   450
               _ExtentY        =   503
               _Version        =   393216
               Value           =   5000
               OrigLeft        =   4320
               OrigTop         =   960
               OrigRight       =   4575
               OrigBottom      =   1245
               Max             =   99999
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
               TabIndex        =   60
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
               TabIndex        =   59
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
               TabIndex        =   58
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
         Begin VB.CheckBox chkSalvarEnvelopeSOAP 
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
               TabIndex        =   122
               Top             =   1080
               Width           =   3555
            End
            Begin VB.CommandButton btnDadosPFX 
               Caption         =   "..."
               Height          =   260
               Left            =   3720
               TabIndex        =   121
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
               TabIndex        =   123
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
            ItemData        =   "FrmMain.frx":036F
            Left            =   -74640
            List            =   "FrmMain.frx":0382
            Style           =   2  'Dropdown List
            TabIndex        =   39
            Top             =   1920
            Width           =   2175
         End
         Begin VB.ComboBox cmbHttp 
            Height          =   315
            ItemData        =   "FrmMain.frx":03BC
            Left            =   -74640
            List            =   "FrmMain.frx":03CF
            Style           =   2  'Dropdown List
            TabIndex        =   38
            Top             =   1320
            Width           =   2175
         End
         Begin VB.ComboBox cmbCrypt 
            Height          =   315
            ItemData        =   "FrmMain.frx":0409
            Left            =   -74640
            List            =   "FrmMain.frx":041C
            Style           =   2  'Dropdown List
            TabIndex        =   37
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
            OrigLeft        =   4560
            OrigTop         =   720
            OrigRight       =   4815
            OrigBottom      =   1005
            Max             =   99999
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
            TabIndex        =   135
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
            TabIndex        =   134
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
            TabIndex        =   131
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
            TabIndex        =   102
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
            TabIndex        =   101
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
            TabIndex        =   100
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
            TabIndex        =   99
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
            TabIndex        =   98
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
            TabIndex        =   97
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
            TabIndex        =   96
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
            TabIndex        =   95
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
            TabIndex        =   94
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
            TabIndex        =   93
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
            TabIndex        =   92
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
            TabIndex        =   91
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
            TabIndex        =   90
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
            TabIndex        =   89
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
         TabIndex        =   147
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
         TabIndex        =   118
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
         TabIndex        =   116
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
    
    On Error GoTo Erro:
    If CheckNFeLista = True Then
    nfe.Assinar
Erro:
    MsgBox Err.Description
        
End Sub

Private Sub btnCancelarNFe_Click()

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
    nfe.CarregarXML
    Else
    nfe.CarregarINI

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
    
    CheckNFeLista = True
    
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnConsultaChave_Click()

    On Error GoTo Erro:
    Dim chaveOuNFe As String
    
    chaveOuNFe = InputBox("Chave da NF-e:", "WebServices Consultar: Nota", "")
    SetResposta nfe.Consultar(chaveOuNFe)
    
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnConsultarCadastro_Click()
        
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
    
    txtCertPath.Text = CommonDialog1.FileName
End Sub

Private Sub btnDFePorChave_Click()

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

    On Error GoTo Erro:
    Dim ret As String
    Dim codUF As Long
    Dim cnpj As String
    Dim eNsu As String
    
    codUF = InputBox("Codigo da UF", "WebServices: Distribuição DFe", 35)
    cnpj = InputBox("CNPJ do Autor", "WebServices: Distribuição DFe", "")
    eNsu = InputBox("Número do último NSU", "WebServices: Distribuição DFe", "")
    
    ret = nfe.DistribuicaoDFePorNSU(codUF, cnpj, eNsu)
    rtbRespostas.Text = ret
    
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnEnviarAssincrono_Click()

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
    
    nfe.EnviarEmailEvento destinatario, arquivoXml, arquivoXmlEvento
    
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnEnviarEvento_Click()

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
    
    On Error GoTo Erro:
    Dim ret As String
    Dim aLote As Long
    
    CheckNFeLista
    aLote = InputBox("Número do Lote", "WebServices Enviar", 1)
    ret = nfe.Enviar(aLote, sincrono = True)
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

    On Error GoTo Erro:
    Dim ret As String
    nfe.LimparLista
    nfe.CarregarINI
    nfe.Assinar
    ret = nfe.ObterXml
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

    rtbRespostas.Text = nfe.StatusServico
    
End Sub

Private Sub btnValidarRegraNegocio_Click()
        
    On Error GoTo Erro:
    
    CheckNFeLista = True
    rtbRespostas.Text = nfe.ValidarRegrasdeNegocios
    
Erro:
    MsgBox Err.Description
    
    
End Sub

Private Sub cmdSalvar_Click()
    SalvarConfig
End Sub

Private Sub Form_Load()
    cmbModeloDocumento.ListIndex = 0
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
    
    Set nfe = CreateNFe(IniPath)
    
    nfe.ConfigGravarValor SESSAO_PRINCIPAL, "LogNivel", NivelLog.logParanoico
    nfe.ConfigGravarValor SESSAO_PRINCIPAL, "LogPath", LogPath
    nfe.ConfigGravar
    
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
    Dim buffer As String
    Dim bufferLen As Long
    
    nfe.ConfigLer
    cmbModeloDocumento.ListIndex = CLng(nfe.ConfigLerValor(SESSAO_NFE, "ModeloDF"))
    txtIdCSC.Text = nfe.ConfigLerValor(SESSAO_NFE, "IdCSC")
    txtCSC.Text = nfe.ConfigLerValor(SESSAO_NFE, "CSC")
    cmbCrypt.ListIndex = CLng(nfe.ConfigLerValor(SESSAO_DFE, "SSLCryptLib"))
    cmbHttp.ListIndex = CLng(nfe.ConfigLerValor(SESSAO_DFE, "SSLHttpLib"))
    cmbXmlSign.ListIndex = CLng(nfe.ConfigLerValor(SESSAO_DFE, "SSLXmlSignLib"))
    txtCertPath.Text = nfe.ConfigLerValor(SESSAO_DFE, "ArquivoPFX")
    txtCertPassword.Text = nfe.ConfigLerValor(SESSAO_DFE, "Senha")
    txtCertNumero.Text = nfe.ConfigLerValor(SESSAO_DFE, "NumeroSerie")
    txtSchemaPath.Text = nfe.ConfigLerValor(SESSAO_NFE, "PathSchemas")
    cmbUFDestino.Text = nfe.ConfigLerValor(SESSAO_DFE, "UF")
    
    Dim ambiente As String
    ambiente = nfe.ConfigLerValor(SESSAO_NFE, "Ambiente")
    
    rdbHomologacao.Value = CBool(ambiente)
    rdbProducao.Value = Not CBool(ambiente)
    
    cmbSSlType.ListIndex = CLng(nfe.ConfigLerValor(SESSAO_NFE, "SSLType"))
    nudTimeOut.Value = CLng(nfe.ConfigLerValor(SESSAO_NFE, "Timeout"))
    txtProxyServidor.Text = nfe.ConfigLerValor(SESSAO_PROXY, "Servidor")
    
    Dim porta As String
    porta = nfe.ConfigLerValor(SESSAO_PROXY, "Porta")
    
    If IsNumeric(porta) Then
      nudProxyPorta.Value = CLng(porta)
    End If
    
    txtProxyUsuario.Text = nfe.ConfigLerValor(SESSAO_PROXY, "Usuario")
    txtProxySenha.Text = nfe.ConfigLerValor(SESSAO_PROXY, "Senha")
    txtNome.Text = nfe.ConfigLerValor(SESSAO_EMAIL, "Nome")
    txtEmail.Text = nfe.ConfigLerValor(SESSAO_EMAIL, "Conta")
    txtUsuario.Text = nfe.ConfigLerValor(SESSAO_EMAIL, "Usuario")
    txtSenha.Text = nfe.ConfigLerValor(SESSAO_EMAIL, "Senha")
    txtHost.Text = nfe.ConfigLerValor(SESSAO_EMAIL, "Servidor")
    txtnudPorta.Text = CLng(nfe.ConfigLerValor(SESSAO_EMAIL, "Porta"))
    ckbSSL.Value = CLng(nfe.ConfigLerValor(SESSAO_EMAIL, "SSL"))
    ckbTLS.Value = CLng(nfe.ConfigLerValor(SESSAO_EMAIL, "TLS"))
    
End Sub

Private Sub SalvarConfig()
    nfe.ConfigGravarValor SESSAO_NFE, "ModeloDF", CStr(cmbModeloDocumento.ListIndex)
    nfe.ConfigGravarValor SESSAO_NFE, "IdCSC", txtIdCSC.Text
    nfe.ConfigGravarValor SESSAO_NFE, "CSC", txtCSC.Text
    nfe.ConfigGravarValor SESSAO_DFE, "SSLCryptLib", CStr(cmbCrypt.ListIndex)
    nfe.ConfigGravarValor SESSAO_DFE, "SSLHttpLib", CStr(cmbHttp.ListIndex)
    nfe.ConfigGravarValor SESSAO_DFE, "SSLXmlSignLib", CStr(cmbXmlSign.ListIndex)
    nfe.ConfigGravarValor SESSAO_DFE, "ArquivoPFX", txtCertPath.Text
    nfe.ConfigGravarValor SESSAO_DFE, "Senha", txtCertPassword.Text
    nfe.ConfigGravarValor SESSAO_DFE, "NumeroSerie", txtCertNumero.Text
    nfe.ConfigGravarValor SESSAO_NFE, "PathSchemas", txtSchemaPath.Text
    nfe.ConfigGravarValor SESSAO_DFE, "UF", cmbUFDestino.Text
    nfe.ConfigGravarValor SESSAO_NFE, "Ambiente", CStr(rdbHomologacao.Value)
    nfe.ConfigGravarValor SESSAO_NFE, "SSLType", CStr(cmbSSlType.ListIndex)
    nfe.ConfigGravarValor SESSAO_NFE, "Timeout", CStr(nudTimeOut.Value)
    nfe.ConfigGravarValor SESSAO_PROXY, "Servidor", txtProxyServidor.Text
    nfe.ConfigGravarValor SESSAO_PROXY, "Porta", CStr(nudProxyPorta.Value)
    nfe.ConfigGravarValor SESSAO_PROXY, "Usuario", txtProxyUsuario.Text
    nfe.ConfigGravarValor SESSAO_PROXY, "Senha", txtProxySenha.Text
    nfe.ConfigGravarValor SESSAO_EMAIL, "Nome", txtNome.Text
    nfe.ConfigGravarValor SESSAO_EMAIL, "Conta", txtEmail.Text
    nfe.ConfigGravarValor SESSAO_EMAIL, "Usuario", txtUsuario.Text
    nfe.ConfigGravarValor SESSAO_EMAIL, "Senha", txtSenha.Text
    nfe.ConfigGravarValor SESSAO_EMAIL, "Servidor", txtHost.Text
    nfe.ConfigGravarValor SESSAO_EMAIL, "Porta", CStr(nudPorta.Value)
    nfe.ConfigGravarValor SESSAO_EMAIL, "SSL", CStr(ckbSSL.Value)
    nfe.ConfigGravarValor SESSAO_EMAIL, "TLS", CStr(ckbTLS.Value)
    nfe.ConfigGravar
End Sub
