VERSION 5.00
Object = "{86CF1D34-0C5F-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCT2.OCX"
Object = "{BDC217C8-ED16-11CD-956C-0000C04E4C0A}#1.1#0"; "TABCTL32.OCX"
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "COMDLG32.OCX"
Begin VB.Form FrmMain 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "ACBrLibGTIN Demo"
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
      TabIndex        =   3
      Top             =   8040
      Width           =   2535
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
      TabIndex        =   4
      Top             =   120
      Width           =   5415
      _ExtentX        =   9551
      _ExtentY        =   13785
      _Version        =   393216
      Style           =   1
      Tabs            =   1
      TabsPerRow      =   1
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
      Begin TabDlg.SSTab SSTab2 
         Height          =   7335
         Left            =   120
         TabIndex        =   5
         Top             =   240
         Width           =   5175
         _ExtentX        =   9128
         _ExtentY        =   12938
         _Version        =   393216
         Style           =   1
         Tabs            =   4
         Tab             =   3
         TabsPerRow      =   4
         TabHeight       =   520
         TabCaption(0)   =   "Geral"
         TabPicture(0)   =   "FrmMain.frx":001C
         Tab(0).ControlEnabled=   0   'False
         Tab(0).Control(0)=   "lblFormatoAlerta"
         Tab(0).Control(0).Enabled=   0   'False
         Tab(0).Control(1)=   "lblPastaLogs"
         Tab(0).Control(1).Enabled=   0   'False
         Tab(0).Control(2)=   "lblPastaSchemas"
         Tab(0).Control(2).Enabled=   0   'False
         Tab(0).Control(3)=   "chkExibirErroSchema"
         Tab(0).Control(3).Enabled=   0   'False
         Tab(0).Control(4)=   "txtFormatoAlerta"
         Tab(0).Control(4).Enabled=   0   'False
         Tab(0).Control(5)=   "chkSalvar"
         Tab(0).Control(5).Enabled=   0   'False
         Tab(0).Control(6)=   "btnSelectLog"
         Tab(0).Control(6).Enabled=   0   'False
         Tab(0).Control(7)=   "txtLogs"
         Tab(0).Control(7).Enabled=   0   'False
         Tab(0).Control(8)=   "btnSelectSchema"
         Tab(0).Control(8).Enabled=   0   'False
         Tab(0).Control(9)=   "txtSchemaPath"
         Tab(0).Control(9).Enabled=   0   'False
         Tab(0).ControlCount=   10
         TabCaption(1)   =   "WebServices"
         TabPicture(1)   =   "FrmMain.frx":0038
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
         Tab(1).Control(6)=   "frmProxy"
         Tab(1).Control(6).Enabled=   0   'False
         Tab(1).Control(7)=   "cmbSSlType"
         Tab(1).Control(7).Enabled=   0   'False
         Tab(1).Control(8)=   "cmbUFDestino"
         Tab(1).Control(8).Enabled=   0   'False
         Tab(1).Control(9)=   "frmAmbiente"
         Tab(1).Control(9).Enabled=   0   'False
         Tab(1).Control(10)=   "txtTimeOut"
         Tab(1).Control(10).Enabled=   0   'False
         Tab(1).ControlCount=   11
         TabCaption(2)   =   "Certificados"
         TabPicture(2)   =   "FrmMain.frx":0054
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
         Tab(2).ControlCount=   7
         TabCaption(3)   =   "Arquivos"
         TabPicture(3)   =   "FrmMain.frx":0070
         Tab(3).ControlEnabled=   -1  'True
         Tab(3).Control(0)=   "lblPastaArqGTIN"
         Tab(3).Control(0).Enabled=   0   'False
         Tab(3).Control(1)=   "chkSalvarArqs"
         Tab(3).Control(1).Enabled=   0   'False
         Tab(3).Control(2)=   "chkPastaMensal"
         Tab(3).Control(2).Enabled=   0   'False
         Tab(3).Control(3)=   "chkAdicionaLiteral"
         Tab(3).Control(3).Enabled=   0   'False
         Tab(3).Control(4)=   "chkSepararPorCNPJ"
         Tab(3).Control(4).Enabled=   0   'False
         Tab(3).Control(5)=   "chkSepararPorModelo"
         Tab(3).Control(5).Enabled=   0   'False
         Tab(3).Control(6)=   "txtArqGTIN"
         Tab(3).Control(6).Enabled=   0   'False
         Tab(3).Control(7)=   "btnArqGTIN"
         Tab(3).Control(7).Enabled=   0   'False
         Tab(3).ControlCount=   8
         Begin VB.TextBox txtTimeOut 
            Alignment       =   1  'Right Justify
            Height          =   285
            Left            =   -71280
            TabIndex        =   51
            Text            =   "0"
            Top             =   720
            Width           =   570
         End
         Begin VB.CommandButton btnArqGTIN 
            Caption         =   "..."
            Height          =   260
            Left            =   3720
            TabIndex        =   50
            Top             =   2400
            Width           =   390
         End
         Begin VB.TextBox txtArqGTIN 
            Height          =   285
            Left            =   120
            TabIndex        =   49
            Top             =   2400
            Width           =   3555
         End
         Begin VB.CheckBox chkSepararPorModelo 
            Caption         =   "Separar Arqs pelo Modelo do Documento"
            Height          =   195
            Left            =   120
            TabIndex        =   48
            Top             =   1800
            Width           =   3495
         End
         Begin VB.CheckBox chkSepararPorCNPJ 
            Caption         =   "Separar Arqs pelo CNPJ do Certificado"
            Height          =   195
            Left            =   120
            TabIndex        =   47
            Top             =   1440
            Width           =   3615
         End
         Begin VB.CheckBox chkAdicionaLiteral 
            Caption         =   "Adicionar Literal no nome das pastas"
            Height          =   195
            Left            =   120
            TabIndex        =   46
            Top             =   1080
            Width           =   3870
         End
         Begin VB.CheckBox chkPastaMensal 
            Caption         =   "Criar Pastas Mensalmente"
            Height          =   255
            Left            =   120
            TabIndex        =   45
            Top             =   720
            Width           =   3855
         End
         Begin VB.CheckBox chkSalvarArqs 
            Caption         =   "Salvar Arquivos em Pastas Separadas"
            Height          =   195
            Left            =   120
            TabIndex        =   44
            Top             =   420
            Width           =   4335
         End
         Begin VB.TextBox txtSchemaPath 
            Height          =   285
            Left            =   -74880
            TabIndex        =   43
            Top             =   2880
            Width           =   4215
         End
         Begin VB.CommandButton btnSelectSchema 
            Caption         =   "..."
            Height          =   260
            Left            =   -70560
            TabIndex        =   42
            Top             =   2880
            Width           =   390
         End
         Begin VB.TextBox txtLogs 
            Height          =   285
            Left            =   -74880
            TabIndex        =   41
            Top             =   2280
            Width           =   4215
         End
         Begin VB.CommandButton btnSelectLog 
            Caption         =   "..."
            Height          =   260
            Left            =   -70560
            TabIndex        =   40
            Top             =   2280
            Width           =   390
         End
         Begin VB.CheckBox chkSalvar 
            Caption         =   "Salvar Arquivos de Envio e Resposta"
            Height          =   255
            Left            =   -74880
            TabIndex        =   39
            Top             =   1680
            Width           =   3375
         End
         Begin VB.TextBox txtFormatoAlerta 
            Height          =   285
            Left            =   -74880
            TabIndex        =   38
            Top             =   1200
            Width           =   4695
         End
         Begin VB.CheckBox chkExibirErroSchema 
            Caption         =   "Exibir Erro Schema"
            Height          =   255
            Left            =   -74880
            TabIndex        =   37
            Top             =   600
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
            TabIndex        =   34
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
               TabIndex        =   36
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
               TabIndex        =   35
               ToolTipText     =   "taProducao"
               Top             =   360
               Width           =   1095
            End
         End
         Begin VB.ComboBox cmbUFDestino 
            Height          =   315
            ItemData        =   "FrmMain.frx":008C
            Left            =   -74760
            List            =   "FrmMain.frx":00E1
            Style           =   2  'Dropdown List
            TabIndex        =   33
            Top             =   720
            Width           =   1095
         End
         Begin VB.ComboBox cmbSSlType 
            Height          =   315
            ItemData        =   "FrmMain.frx":0151
            Left            =   -73440
            List            =   "FrmMain.frx":016A
            Style           =   2  'Dropdown List
            TabIndex        =   32
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
            TabIndex        =   22
            Top             =   2760
            Width           =   4695
            Begin VB.TextBox txtProxyPorta 
               Alignment       =   1  'Right Justify
               Height          =   285
               Left            =   2880
               TabIndex        =   26
               Text            =   "0"
               Top             =   480
               Width           =   375
            End
            Begin VB.TextBox txtProxySenha 
               Height          =   285
               Left            =   120
               TabIndex        =   25
               Top             =   1680
               Width           =   3855
            End
            Begin VB.TextBox txtProxyUsuario 
               Height          =   285
               Left            =   120
               TabIndex        =   24
               Top             =   1080
               Width           =   3855
            End
            Begin VB.TextBox txtProxyServidor 
               Height          =   285
               Left            =   120
               TabIndex        =   23
               Top             =   480
               Width           =   2655
            End
            Begin MSComCtl2.UpDown nudProxyPorta 
               Height          =   285
               Left            =   3240
               TabIndex        =   27
               Top             =   480
               Width           =   255
               _ExtentX        =   450
               _ExtentY        =   503
               _Version        =   393216
               Value           =   5000
               BuddyControl    =   "txtProxyPorta"
               BuddyDispid     =   196637
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
               TabIndex        =   31
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
               TabIndex        =   30
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
               TabIndex        =   29
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
               TabIndex        =   28
               Top             =   1440
               Width           =   525
            End
         End
         Begin VB.CheckBox chkVisualizarMensagem 
            Caption         =   "Visualizar Mensagem"
            Height          =   375
            Left            =   -74760
            TabIndex        =   21
            Top             =   2040
            Width           =   2775
         End
         Begin VB.CheckBox chkSalvarSOAP 
            Caption         =   "Salvar envelope SOAP"
            Height          =   255
            Left            =   -74760
            TabIndex        =   20
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
            TabIndex        =   9
            Top             =   2400
            Width           =   4215
            Begin VB.TextBox txtDadosPFX 
               Height          =   285
               Left            =   120
               TabIndex        =   15
               Top             =   1080
               Width           =   3555
            End
            Begin VB.CommandButton btnDadosPFX 
               Caption         =   "..."
               Height          =   260
               Left            =   3720
               TabIndex        =   14
               Top             =   1080
               Width           =   390
            End
            Begin VB.CommandButton btnSelecionarCertificado 
               Caption         =   "..."
               Height          =   260
               Left            =   3720
               TabIndex        =   13
               Top             =   479
               Width           =   390
            End
            Begin VB.TextBox txtCertPath 
               Height          =   285
               Left            =   120
               TabIndex        =   12
               Top             =   480
               Width           =   3555
            End
            Begin VB.TextBox txtCertPassword 
               Height          =   285
               IMEMode         =   3  'DISABLE
               Left            =   120
               PasswordChar    =   "*"
               TabIndex        =   11
               Top             =   1680
               Width           =   4035
            End
            Begin VB.TextBox txtCertNumero 
               Height          =   285
               Left            =   120
               TabIndex        =   10
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
               TabIndex        =   19
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
               TabIndex        =   18
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
               TabIndex        =   17
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
               TabIndex        =   16
               Top             =   2040
               Width           =   1395
            End
         End
         Begin VB.ComboBox cmbXmlSign 
            Height          =   315
            ItemData        =   "FrmMain.frx":01B6
            Left            =   -74640
            List            =   "FrmMain.frx":01C9
            Style           =   2  'Dropdown List
            TabIndex        =   8
            Top             =   1920
            Width           =   2175
         End
         Begin VB.ComboBox cmbHttp 
            Height          =   315
            ItemData        =   "FrmMain.frx":0203
            Left            =   -74640
            List            =   "FrmMain.frx":0213
            Style           =   2  'Dropdown List
            TabIndex        =   7
            Top             =   1320
            Width           =   2175
         End
         Begin VB.ComboBox cmbCrypt 
            Height          =   315
            ItemData        =   "FrmMain.frx":0248
            Left            =   -74640
            List            =   "FrmMain.frx":0258
            Style           =   2  'Dropdown List
            TabIndex        =   6
            Top             =   720
            Width           =   2175
         End
         Begin MSComCtl2.UpDown nudTimeOut 
            Height          =   285
            Left            =   -70709
            TabIndex        =   52
            Top             =   720
            Width           =   255
            _ExtentX        =   450
            _ExtentY        =   503
            _Version        =   393216
            Value           =   5000
            BuddyControl    =   "txtTimeOut"
            BuddyDispid     =   196613
            OrigLeft        =   4560
            OrigTop         =   720
            OrigRight       =   4815
            OrigBottom      =   1005
            Max             =   99999
            Enabled         =   -1  'True
         End
         Begin VB.Label lblPastaArqGTIN 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "Pasta Arquivos GTIN"
            BeginProperty Font 
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
            TabIndex        =   62
            Top             =   2160
            Width           =   1725
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
            Left            =   -74880
            TabIndex        =   61
            Top             =   2640
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
            Left            =   -74880
            TabIndex        =   60
            Top             =   2040
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
            Left            =   -74880
            TabIndex        =   59
            Top             =   960
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
            TabIndex        =   58
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
            TabIndex        =   57
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
            TabIndex        =   56
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
            TabIndex        =   55
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
            TabIndex        =   54
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
            TabIndex        =   53
            Top             =   480
            Width           =   705
         End
      End
   End
   Begin TabDlg.SSTab SSTab1 
      Height          =   3855
      Left            =   5640
      TabIndex        =   63
      Top             =   120
      Width           =   6255
      _ExtentX        =   11033
      _ExtentY        =   6800
      _Version        =   393216
      Style           =   1
      Tabs            =   1
      TabsPerRow      =   1
      TabHeight       =   520
      TabCaption(0)   =   "Consultas"
      TabPicture(0)   =   "FrmMain.frx":028A
      Tab(0).ControlEnabled=   -1  'True
      Tab(0).Control(0)=   "btnConsultarGTIN"
      Tab(0).Control(0).Enabled=   0   'False
      Tab(0).ControlCount=   1
      Begin VB.CommandButton btnConsultarGTIN 
         Caption         =   "Consultar GTIN"
         Height          =   375
         Left            =   240
         TabIndex        =   64
         Top             =   480
         Width           =   1575
      End
   End
End
Attribute VB_Name = "FrmMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim gtin As ACBrGTIN

Private Sub Form_Load()
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
    
    Set gtin = CreateGTIN(IniPath)
    
    gtin.ConfigGravarValor SESSAO_PRINCIPAL, "LogNivel", NivelLog.logParanoico
    gtin.ConfigGravarValor SESSAO_PRINCIPAL, "LogPath", LogPath
    gtin.ConfigGravar
    
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

Private Sub btnConsultarGTIN_Click()
    On Erro GoTo Erro:
    Dim codGTIN As String
    
    codGTIN = InputBox("Informe o código:", "Consultar GTIN", "")
    
    SetResposta gtin.Consultar(codGTIN)
    
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

Private Sub btnSelecionarCertificado_Click()
    On Error Resume Next
    CommonDialog1.DialogTitle = "Selecione o certificado"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivos PFX (*.pfx)|*.pfx|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.ShowOpen
            
    If Err Then Exit Sub
    
    txtCertPath.Text = CommonDialog1.FileName
End Sub

Private Sub btnArqGTIN_Click()
    txtArqGTIN.Text = BrowseFolder("Selecione a pasta Arquivos GTIN")
End Sub

Private Sub btnCarregarConfiguracoes_Click()
    LoadConfig
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
    
    gtin.ConfigLer
    
    'Geral
    chkExibirErroSchema.Value = CLng(gtin.ConfigLerValor(SESSAO_GTIN, "ExibirErroSchema"))
    txtFormatoAlerta.Text = gtin.ConfigLerValor(SESSAO_GTIN, "FormatoAlerta")
    chkSalvar.Value = CLng(gtin.ConfigLerValor(SESSAO_GTIN, "SalvarGer"))
    txtLogs.Text = gtin.ConfigLerValor(SESSAO_GTIN, "PathSalvar")
    txtSchemaPath.Text = gtin.ConfigLerValor(SESSAO_GTIN, "PathSchemas")
    
    'WebSevice
    cmbUFDestino.Text = gtin.ConfigLerValor(SESSAO_DFE, "UF")
    cmbSSlType.ListIndex = CLng(gtin.ConfigLerValor(SESSAO_GTIN, "SSLType"))
    nudTimeOut.Value = CLng(gtin.ConfigLerValor(SESSAO_GTIN, "Timeout"))
    
    Dim ambiente As String
    ambiente = gtin.ConfigLerValor(SESSAO_GTIN, "Ambiente")
    rdbHomologacao.Value = CBool(ambiente)
    rdbProducao.Value = Not CBool(ambiente)
    
    chkVisualizarMensagem.Value = CLng(gtin.ConfigLerValor(SESSAO_GTIN, "Visualizar"))
    chkSalvarSOAP.Value = CLng(gtin.ConfigLerValor(SESSAO_GTIN, "SalvarWS"))
    
    'proxy
    txtProxyServidor.Text = gtin.ConfigLerValor(SESSAO_PROXY, "Servidor")
    nudProxyPorta.Value = CLng(gtin.ConfigLerValor(SESSAO_PROXY, "Porta"))
    txtProxyUsuario.Text = gtin.ConfigLerValor(SESSAO_PROXY, "Usuario")
    txtProxySenha.Text = gtin.ConfigLerValor(SESSAO_PROXY, "Senha")
    
    'Certificado
    cmbCrypt.ListIndex = CLng(gtin.ConfigLerValor(SESSAO_DFE, "SSLCryptLib"))
    cmbHttp.ListIndex = CLng(gtin.ConfigLerValor(SESSAO_DFE, "SSLHttpLib"))
    cmbXmlSign.ListIndex = CLng(gtin.ConfigLerValor(SESSAO_DFE, "SSLXmlSignLib"))
    txtCertPath.Text = gtin.ConfigLerValor(SESSAO_DFE, "ArquivoPFX")
    txtCertPassword.Text = gtin.ConfigLerValor(SESSAO_DFE, "Senha")
    txtCertNumero.Text = gtin.ConfigLerValor(SESSAO_DFE, "NumeroSerie")
    txtDadosPFX.Text = gtin.ConfigLerValor(SESSAO_DFE, "DadosPFX")
    
    'Arquivos
    chkSalvarArqs.Value = CLng(gtin.ConfigLerValor(SESSAO_GTIN, "SalvarArq"))
    chkPastaMensal.Value = CLng(gtin.ConfigLerValor(SESSAO_GTIN, "SepararPorMes"))
    chkAdicionaLiteral.Value = CLng(gtin.ConfigLerValor(SESSAO_GTIN, "AdicionarLiteral"))
    chkSepararPorCNPJ.Value = CLng(gtin.ConfigLerValor(SESSAO_GTIN, "SepararPorCNPJ"))
    chkSepararPorModelo.Value = CLng(gtin.ConfigLerValor(SESSAO_GTIN, "SepararPorModelo"))
    txtArqGTIN.Text = gtin.ConfigLerValor(SESSAO_GTIN, "PathGTIN")
     
End Sub

Private Sub SalvarConfig()
    
    'Geral
    gtin.ConfigGravarValor SESSAO_GTIN, "ExibirErroSchema", CStr(chkExibirErroSchema.Value)
    gtin.ConfigGravarValor SESSAO_GTIN, "FormatoAlerta", txtFormatoAlerta.Text
    gtin.ConfigGravarValor SESSAO_GTIN, "SalvarWS", CStr(chkSalvar.Value)
    gtin.ConfigGravarValor SESSAO_GTIN, "PathSalvar", txtLogs.Text
    gtin.ConfigGravarValor SESSAO_GTIN, "PathSchemas", txtSchemaPath.Text
    
    'WebService
    gtin.ConfigGravarValor SESSAO_DFE, "UF", CStr(cmbUFDestino.ListIndex)
    gtin.ConfigGravarValor SESSAO_GTIN, "SSLType", CStr(cmbSSlType.ListIndex)
    gtin.ConfigGravarValor SESSAO_GTIN, "Timeout", CStr(nudTimeOut.Value)
    gtin.ConfigGravarValor SESSAO_GTIN, "Ambiente", CStr(rdbProducao.Value)
    gtin.ConfigGravarValor SESSAO_GTIN, "Ambiente", CStr(rdbHomologacao.Value)
    gtin.ConfigGravarValor SESSAO_GTIN, "Visualizar", CStr(chkVisualizarMensagem)
    gtin.ConfigGravarValor SESSAO_GTIN, "SalvarWS", CStr(chkSalvarSOAP.Value)
    
    'Proxy
    gtin.ConfigGravarValor SESSAO_PROXY, "Servidor", txtProxyServidor.Text
    gtin.ConfigGravarValor SESSAO_PROXY, "Porta", CStr(nudProxyPorta.Value)
    gtin.ConfigGravarValor SESSAO_PROXY, "Usuario", txtProxyUsuario.Text
    gtin.ConfigGravarValor SESSAO_PROXY, "Senha", txtProxySenha.Text
    
    'Certificado
    gtin.ConfigGravarValor SESSAO_DFE, "SSLCryptLib", CStr(cmbCrypt.ListIndex)
    gtin.ConfigGravarValor SESSAO_DFE, "SSLHttpLib", CStr(cmbHttp.ListIndex)
    gtin.ConfigGravarValor SESSAO_DFE, "SSLXmlSignLib", CStr(cmbXmlSign.ListIndex)
    gtin.ConfigGravarValor SESSAO_DFE, "ArquivoPFX", txtCertPath.Text
    gtin.ConfigGravarValor SESSAO_DFE, "Senha", txtCertPassword.Text
    gtin.ConfigGravarValor SESSAO_DFE, "NumeroSerie", txtCertNumero.Text
    gtin.ConfigGravarValor SESSAO_DFE, "DadosPFX", txtDadosPFX.Text
    
    'Arquivos
    gtin.ConfigGravarValor SESSAO_GTIN, "SalvarArq", CStr(chkSalvarArqs.Value)
    gtin.ConfigGravarValor SESSAO_GTIN, "SepararPorMes", CStr(chkPastaMensal.Value)
    gtin.ConfigGravarValor SESSAO_GTIN, "AdicionarLiteral", CStr(chkAdicionaLiteral.Value)
    gtin.ConfigGravarValor SESSAO_GTIN, "SepararPorCNPJ", CStr(chkSepararPorCNPJ.Value)
    gtin.ConfigGravarValor SESSAO_GTIN, "SepararPorModelo", CStr(chkSepararPorModelo.Value)
    gtin.ConfigGravarValor SESSAO_GTIN, "PathGTIN", txtArqGTIN.Text
        
    gtin.ConfigGravar

End Sub
