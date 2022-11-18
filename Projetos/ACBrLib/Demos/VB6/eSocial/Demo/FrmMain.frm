VERSION 5.00
Object = "{86CF1D34-0C5F-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCT2.OCX"
Object = "{BDC217C8-ED16-11CD-956C-0000C04E4C0A}#1.1#0"; "TABCTL32.OCX"
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "COMDLG32.OCX"
Begin VB.Form FrmMain 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "ACBrLibeSocial Demo"
   ClientHeight    =   8820
   ClientLeft      =   45
   ClientTop       =   390
   ClientWidth     =   12090
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
   ScaleHeight     =   8820
   ScaleWidth      =   12090
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
      Top             =   8160
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
      Height          =   3975
      Left            =   5640
      TabIndex        =   1
      Top             =   4560
      Width           =   6255
      Begin VB.TextBox rtbRespostas 
         Height          =   3615
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
      Top             =   8160
      Width           =   2295
   End
   Begin TabDlg.SSTab SSTab1 
      Height          =   4335
      Left            =   5640
      TabIndex        =   4
      Top             =   120
      Width           =   6255
      _ExtentX        =   11033
      _ExtentY        =   7646
      _Version        =   393216
      Style           =   1
      Tabs            =   1
      TabsPerRow      =   1
      TabHeight       =   520
      TabCaption(0)   =   "Comandos"
      TabPicture(0)   =   "FrmMain.frx":0000
      Tab(0).ControlEnabled=   -1  'True
      Tab(0).Control(0)=   "btnCarregarIniNFe"
      Tab(0).Control(0).Enabled=   0   'False
      Tab(0).Control(1)=   "btnCriarEventoeSocial"
      Tab(0).Control(1).Enabled=   0   'False
      Tab(0).Control(2)=   "btnEnviareSocial"
      Tab(0).Control(2).Enabled=   0   'False
      Tab(0).Control(3)=   "btnConsultareSocial"
      Tab(0).Control(3).Enabled=   0   'False
      Tab(0).Control(4)=   "btnCriarEnviareSocial"
      Tab(0).Control(4).Enabled=   0   'False
      Tab(0).Control(5)=   "btnLimpareSocial"
      Tab(0).Control(5).Enabled=   0   'False
      Tab(0).Control(6)=   "btnCarregarXMLEventoeSocial"
      Tab(0).Control(6).Enabled=   0   'False
      Tab(0).Control(7)=   "btnSetIDEmpregador"
      Tab(0).Control(7).Enabled=   0   'False
      Tab(0).Control(8)=   "btnSetIDTransmissor"
      Tab(0).Control(8).Enabled=   0   'False
      Tab(0).Control(9)=   "btnSetTipoEmpregador"
      Tab(0).Control(9).Enabled=   0   'False
      Tab(0).Control(10)=   "btnSetVersaoDF"
      Tab(0).Control(10).Enabled=   0   'False
      Tab(0).Control(11)=   "btnDownloadEventos"
      Tab(0).Control(11).Enabled=   0   'False
      Tab(0).Control(12)=   "btnConsultaIdentificadoresEventosEmpregador"
      Tab(0).Control(12).Enabled=   0   'False
      Tab(0).Control(13)=   "btnConsultaIdentificadoresEventosTabela"
      Tab(0).Control(13).Enabled=   0   'False
      Tab(0).Control(14)=   "btnConsultaIdentificadoresEventosTrabalhador"
      Tab(0).Control(14).Enabled=   0   'False
      Tab(0).ControlCount=   15
      Begin VB.CommandButton btnConsultaIdentificadoresEventosTrabalhador 
         Caption         =   "Consulta Identificadores Eventos Trabalhador"
         Height          =   375
         Left            =   240
         TabIndex        =   22
         Top             =   3840
         Width           =   3495
      End
      Begin VB.CommandButton btnConsultaIdentificadoresEventosTabela 
         Caption         =   "Consulta Identificadores Eventos Tabela"
         Height          =   375
         Left            =   240
         TabIndex        =   21
         Top             =   3360
         Width           =   3495
      End
      Begin VB.CommandButton btnConsultaIdentificadoresEventosEmpregador 
         Caption         =   "Consulta Identificadores Eventos Empregador"
         Height          =   375
         Left            =   240
         TabIndex        =   20
         Top             =   2880
         Width           =   3495
      End
      Begin VB.CommandButton btnDownloadEventos 
         Caption         =   "Download Eventos"
         Height          =   375
         Left            =   1920
         TabIndex        =   19
         Top             =   2400
         Width           =   1575
      End
      Begin VB.CommandButton btnSetVersaoDF 
         Caption         =   "SetVersaoDF"
         Height          =   375
         Left            =   1920
         TabIndex        =   18
         Top             =   1920
         Width           =   1575
      End
      Begin VB.CommandButton btnSetTipoEmpregador 
         Caption         =   "SetTipoEmpregador"
         Height          =   375
         Left            =   1920
         TabIndex        =   17
         Top             =   1440
         Width           =   1575
      End
      Begin VB.CommandButton btnSetIDTransmissor 
         Caption         =   "SetIDTransmissor"
         Height          =   375
         Left            =   1920
         TabIndex        =   16
         Top             =   960
         Width           =   1575
      End
      Begin VB.CommandButton btnSetIDEmpregador 
         Caption         =   "SetIDEmpregador"
         Height          =   375
         Left            =   1920
         TabIndex        =   15
         Top             =   480
         Width           =   1575
      End
      Begin VB.CommandButton btnCarregarXMLEventoeSocial 
         Caption         =   "Carregar XML Evento eSocial"
         Height          =   375
         Left            =   3600
         TabIndex        =   14
         Top             =   480
         Width           =   2295
      End
      Begin VB.CommandButton btnLimpareSocial 
         Caption         =   "Limpar eSocial"
         Height          =   375
         Left            =   240
         TabIndex        =   13
         Top             =   2400
         Width           =   1575
      End
      Begin VB.CommandButton btnCriarEnviareSocial 
         Caption         =   "Criar Enviar eSocial"
         Height          =   375
         Left            =   240
         TabIndex        =   12
         Top             =   1920
         Width           =   1575
      End
      Begin VB.CommandButton btnConsultareSocial 
         Caption         =   "Consultar eSocial"
         Height          =   375
         Left            =   240
         TabIndex        =   11
         Top             =   1440
         Width           =   1575
      End
      Begin VB.CommandButton btnEnviareSocial 
         Caption         =   "Enviar eSocial"
         Height          =   375
         Left            =   240
         TabIndex        =   10
         Top             =   960
         Width           =   1575
      End
      Begin VB.CommandButton btnCriarEventoeSocial 
         Caption         =   "Criar Evento eSocial"
         Height          =   375
         Left            =   240
         TabIndex        =   9
         Top             =   480
         Width           =   1575
      End
      Begin VB.CommandButton btnGerarXML 
         Caption         =   "Gerar XML"
         Height          =   375
         Left            =   240
         TabIndex        =   8
         Top             =   -720
         Width           =   1575
      End
      Begin VB.CommandButton btnCarregarIniNFe 
         Caption         =   "Carregar INI NFe"
         Height          =   375
         Left            =   240
         TabIndex        =   7
         Top             =   480
         Width           =   1575
      End
      Begin VB.CommandButton btnEnviarSincrono 
         Caption         =   "Enviar Sincrono"
         Height          =   375
         Left            =   2040
         TabIndex        =   6
         Top             =   -720
         Width           =   1575
      End
      Begin VB.CommandButton btnEnviarAssincrono 
         Caption         =   "Enviar Assincrono"
         Height          =   375
         Left            =   3840
         TabIndex        =   5
         Top             =   -720
         Width           =   1815
      End
   End
   Begin TabDlg.SSTab SSTTab0 
      Height          =   7935
      Left            =   120
      TabIndex        =   23
      Top             =   120
      Width           =   5415
      _ExtentX        =   9551
      _ExtentY        =   13996
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
      TabPicture(0)   =   "FrmMain.frx":001C
      Tab(0).ControlEnabled=   -1  'True
      Tab(0).Control(0)=   "SSTab2"
      Tab(0).Control(0).Enabled=   0   'False
      Tab(0).ControlCount=   1
      Begin TabDlg.SSTab SSTab2 
         Height          =   7335
         Left            =   120
         TabIndex        =   24
         Top             =   360
         Width           =   5175
         _ExtentX        =   9128
         _ExtentY        =   12938
         _Version        =   393216
         Style           =   1
         Tabs            =   5
         TabsPerRow      =   5
         TabHeight       =   520
         TabCaption(0)   =   "Geral"
         TabPicture(0)   =   "FrmMain.frx":0038
         Tab(0).ControlEnabled=   -1  'True
         Tab(0).Control(0)=   "lblPastaSchemas"
         Tab(0).Control(0).Enabled=   0   'False
         Tab(0).Control(1)=   "lblPastaLogs"
         Tab(0).Control(1).Enabled=   0   'False
         Tab(0).Control(2)=   "lblFormaEmissao"
         Tab(0).Control(2).Enabled=   0   'False
         Tab(0).Control(3)=   "lblFormatoAlerta"
         Tab(0).Control(3).Enabled=   0   'False
         Tab(0).Control(4)=   "txtSchemaPath"
         Tab(0).Control(4).Enabled=   0   'False
         Tab(0).Control(5)=   "btnSelectSchema"
         Tab(0).Control(5).Enabled=   0   'False
         Tab(0).Control(6)=   "txtLogs"
         Tab(0).Control(6).Enabled=   0   'False
         Tab(0).Control(7)=   "btnSelectLog"
         Tab(0).Control(7).Enabled=   0   'False
         Tab(0).Control(8)=   "chkSalvar"
         Tab(0).Control(8).Enabled=   0   'False
         Tab(0).Control(9)=   "chkRetirarAcentos"
         Tab(0).Control(9).Enabled=   0   'False
         Tab(0).Control(10)=   "cmbFormaEmissao"
         Tab(0).Control(10).Enabled=   0   'False
         Tab(0).Control(11)=   "txtFormatoAlerta"
         Tab(0).Control(11).Enabled=   0   'False
         Tab(0).Control(12)=   "chkAtualizarXML"
         Tab(0).Control(12).Enabled=   0   'False
         Tab(0).Control(13)=   "chkExibirErroSchema"
         Tab(0).Control(13).Enabled=   0   'False
         Tab(0).ControlCount=   14
         TabCaption(1)   =   "WebServices"
         TabPicture(1)   =   "FrmMain.frx":0054
         Tab(1).ControlEnabled=   0   'False
         Tab(1).Control(0)=   "lblUFDestino"
         Tab(1).Control(1)=   "lblSSLType"
         Tab(1).Control(2)=   "lblTimeOut(1)"
         Tab(1).Control(3)=   "nudTimeOut"
         Tab(1).Control(4)=   "txtTimeOut"
         Tab(1).Control(5)=   "frmAmbiente"
         Tab(1).Control(6)=   "cmbUFDestino"
         Tab(1).Control(7)=   "cmbSSlType"
         Tab(1).Control(8)=   "frmProxy"
         Tab(1).Control(9)=   "frmRetEnvio"
         Tab(1).Control(10)=   "chkVisualizarMensagem"
         Tab(1).Control(11)=   "chkSalvarSOAP"
         Tab(1).ControlCount=   12
         TabCaption(2)   =   "Certificados"
         TabPicture(2)   =   "FrmMain.frx":0070
         Tab(2).ControlEnabled=   0   'False
         Tab(2).Control(0)=   "lblXMLSignLib"
         Tab(2).Control(1)=   "lblHttpLib"
         Tab(2).Control(2)=   "lblCryptLib"
         Tab(2).Control(3)=   "frmCertificados"
         Tab(2).Control(4)=   "cmbXmlSign"
         Tab(2).Control(5)=   "cmbHttp"
         Tab(2).Control(6)=   "cmbCrypt"
         Tab(2).Control(7)=   "btnObterCertificados"
         Tab(2).ControlCount=   8
         TabCaption(3)   =   "Arquivos"
         TabPicture(3)   =   "FrmMain.frx":008C
         Tab(3).ControlEnabled=   0   'False
         Tab(3).Control(0)=   "lblPastaArqeSocial"
         Tab(3).Control(1)=   "btnArqeSocial"
         Tab(3).Control(2)=   "txtArqeSocial"
         Tab(3).Control(3)=   "chkSepararPorModelo"
         Tab(3).Control(4)=   "chkSepararPorCNPJ"
         Tab(3).Control(5)=   "chkSalvaPathEvento"
         Tab(3).Control(6)=   "chkEmissaoPatheSocial"
         Tab(3).Control(7)=   "chkAdicionaLiteral"
         Tab(3).Control(8)=   "chkPastaMensal"
         Tab(3).Control(9)=   "chkSalvarArqs"
         Tab(3).ControlCount=   10
         TabCaption(4)   =   "Emitente"
         TabPicture(4)   =   "FrmMain.frx":00A8
         Tab(4).ControlEnabled=   0   'False
         Tab(4).Control(0)=   "Label1"
         Tab(4).Control(1)=   "Label2"
         Tab(4).Control(2)=   "Label3"
         Tab(4).Control(3)=   "Label4"
         Tab(4).Control(4)=   "txtEmpregador"
         Tab(4).Control(5)=   "txtTransmissor"
         Tab(4).Control(6)=   "cmbTipoEmpregador"
         Tab(4).Control(7)=   "cmbVersaoDF"
         Tab(4).ControlCount=   8
         Begin VB.CommandButton btnObterCertificados 
            Caption         =   "Obter Certificados"
            Height          =   375
            Left            =   -74640
            TabIndex        =   107
            Top             =   5160
            Width           =   1575
         End
         Begin VB.ComboBox cmbVersaoDF 
            Height          =   315
            ItemData        =   "FrmMain.frx":00C4
            Left            =   -74760
            List            =   "FrmMain.frx":00D4
            Style           =   2  'Dropdown List
            TabIndex        =   105
            Top             =   2640
            Width           =   3615
         End
         Begin VB.ComboBox cmbTipoEmpregador 
            Height          =   315
            ItemData        =   "FrmMain.frx":0101
            Left            =   -74760
            List            =   "FrmMain.frx":011A
            Style           =   2  'Dropdown List
            TabIndex        =   103
            Top             =   1920
            Width           =   3615
         End
         Begin VB.TextBox txtTransmissor 
            Height          =   285
            Left            =   -74760
            TabIndex        =   101
            Top             =   1320
            Width           =   3555
         End
         Begin VB.TextBox txtEmpregador 
            Height          =   285
            Left            =   -74760
            TabIndex        =   99
            Top             =   720
            Width           =   3555
         End
         Begin VB.ComboBox cmbCrypt 
            Height          =   315
            ItemData        =   "FrmMain.frx":01D2
            Left            =   -74640
            List            =   "FrmMain.frx":01E2
            Style           =   2  'Dropdown List
            TabIndex        =   86
            Top             =   660
            Width           =   2175
         End
         Begin VB.ComboBox cmbHttp 
            Height          =   315
            ItemData        =   "FrmMain.frx":0214
            Left            =   -74640
            List            =   "FrmMain.frx":0224
            Style           =   2  'Dropdown List
            TabIndex        =   85
            Top             =   1260
            Width           =   2175
         End
         Begin VB.ComboBox cmbXmlSign 
            Height          =   315
            ItemData        =   "FrmMain.frx":0259
            Left            =   -74640
            List            =   "FrmMain.frx":026C
            Style           =   2  'Dropdown List
            TabIndex        =   84
            Top             =   1860
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
            TabIndex        =   73
            Top             =   2340
            Width           =   4215
            Begin VB.TextBox txtCertNumero 
               Height          =   285
               Left            =   120
               TabIndex        =   79
               Top             =   2280
               Width           =   4035
            End
            Begin VB.TextBox txtCertPassword 
               Height          =   285
               IMEMode         =   3  'DISABLE
               Left            =   120
               PasswordChar    =   "*"
               TabIndex        =   78
               Top             =   1680
               Width           =   4035
            End
            Begin VB.TextBox txtCertPath 
               Height          =   285
               Left            =   120
               TabIndex        =   77
               Top             =   480
               Width           =   3555
            End
            Begin VB.CommandButton btnSelecionarCertificado 
               Caption         =   "..."
               Height          =   260
               Left            =   3720
               TabIndex        =   76
               Top             =   479
               Width           =   390
            End
            Begin VB.CommandButton btnDadosPFX 
               Caption         =   "..."
               Height          =   260
               Left            =   3720
               TabIndex        =   75
               Top             =   1080
               Width           =   390
            End
            Begin VB.TextBox txtDadosPFX 
               Height          =   285
               Left            =   120
               TabIndex        =   74
               Top             =   1080
               Width           =   3555
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
               TabIndex        =   83
               Top             =   2040
               Width           =   1395
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
               TabIndex        =   82
               Top             =   1440
               Width           =   525
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
               TabIndex        =   81
               Top             =   240
               Width           =   735
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
               TabIndex        =   80
               Top             =   840
               Width           =   870
            End
         End
         Begin VB.CheckBox chkSalvarSOAP 
            Caption         =   "Salvar envelope SOAP"
            Height          =   255
            Left            =   -74760
            TabIndex        =   72
            Top             =   2460
            Width           =   2175
         End
         Begin VB.CheckBox chkVisualizarMensagem 
            Caption         =   "Visualizar Mensagem"
            Height          =   375
            Left            =   -74760
            TabIndex        =   71
            Top             =   2100
            Width           =   2775
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
            TabIndex        =   60
            Top             =   2820
            Width           =   4695
            Begin VB.TextBox txtIntervalo 
               Alignment       =   1  'Right Justify
               Height          =   285
               Left            =   3360
               TabIndex        =   64
               Text            =   "0"
               Top             =   960
               Width           =   450
            End
            Begin VB.TextBox txtTentativas 
               Alignment       =   1  'Right Justify
               Height          =   285
               Left            =   1800
               TabIndex        =   63
               Text            =   "0"
               Top             =   960
               Width           =   585
            End
            Begin VB.TextBox txtAguardar 
               Alignment       =   1  'Right Justify
               Height          =   285
               Left            =   120
               TabIndex        =   62
               Text            =   "0"
               Top             =   960
               Width           =   465
            End
            Begin VB.CheckBox chkAjustAut 
               Caption         =   "Ajustar Automaticamente prop. ""Aguardar"""
               Height          =   375
               Left            =   120
               TabIndex        =   61
               Top             =   240
               Width           =   4095
            End
            Begin MSComCtl2.UpDown nudAguardar 
               Height          =   285
               Left            =   586
               TabIndex        =   65
               Top             =   960
               Width           =   255
               _ExtentX        =   450
               _ExtentY        =   503
               _Version        =   393216
               Value           =   5000
               BuddyControl    =   "txtAguardar"
               BuddyDispid     =   196655
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
               Left            =   2386
               TabIndex        =   66
               Top             =   960
               Width           =   255
               _ExtentX        =   450
               _ExtentY        =   503
               _Version        =   393216
               Value           =   5000
               BuddyControl    =   "txtTentativas"
               BuddyDispid     =   196654
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
               Left            =   3811
               TabIndex        =   67
               Top             =   960
               Width           =   255
               _ExtentX        =   450
               _ExtentY        =   503
               _Version        =   393216
               Value           =   5000
               BuddyControl    =   "txtIntervalo"
               BuddyDispid     =   196653
               OrigLeft        =   3960
               OrigTop         =   720
               OrigRight       =   4215
               OrigBottom      =   975
               Max             =   99999
               SyncBuddy       =   -1  'True
               BuddyProperty   =   0
               Enabled         =   -1  'True
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
               TabIndex        =   70
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
               TabIndex        =   69
               Top             =   720
               Width           =   915
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
               TabIndex        =   68
               Top             =   720
               Width           =   1050
            End
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
            TabIndex        =   50
            Top             =   4620
            Width           =   4695
            Begin VB.TextBox txtProxyServidor 
               Height          =   285
               Left            =   120
               TabIndex        =   54
               Top             =   480
               Width           =   2775
            End
            Begin VB.TextBox txtProxyUsuario 
               Height          =   285
               Left            =   120
               TabIndex        =   53
               Top             =   1080
               Width           =   3855
            End
            Begin VB.TextBox txtProxySenha 
               Height          =   285
               Left            =   120
               TabIndex        =   52
               Top             =   1680
               Width           =   3855
            End
            Begin VB.TextBox txtProxyPorta 
               Alignment       =   1  'Right Justify
               Height          =   285
               Left            =   3000
               TabIndex        =   51
               Text            =   "0"
               Top             =   480
               Width           =   690
            End
            Begin MSComCtl2.UpDown nudProxyPorta 
               Height          =   285
               Left            =   3720
               TabIndex        =   55
               Top             =   480
               Width           =   255
               _ExtentX        =   450
               _ExtentY        =   503
               _Version        =   393216
               Value           =   5000
               BuddyControl    =   "txtProxyPorta"
               BuddyDispid     =   196664
               OrigLeft        =   3960
               OrigTop         =   720
               OrigRight       =   4215
               OrigBottom      =   975
               Max             =   99999
               SyncBuddy       =   -1  'True
               BuddyProperty   =   0
               Enabled         =   -1  'True
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
               TabIndex        =   59
               Top             =   1440
               Width           =   525
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
               TabIndex        =   58
               Top             =   840
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
               Left            =   3000
               TabIndex        =   57
               Top             =   240
               Width           =   465
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
               TabIndex        =   56
               Top             =   240
               Width           =   975
            End
         End
         Begin VB.ComboBox cmbSSlType 
            Height          =   315
            ItemData        =   "FrmMain.frx":02A6
            Left            =   -73440
            List            =   "FrmMain.frx":02BF
            Style           =   2  'Dropdown List
            TabIndex        =   49
            Top             =   780
            Width           =   1695
         End
         Begin VB.ComboBox cmbUFDestino 
            Height          =   315
            ItemData        =   "FrmMain.frx":030B
            Left            =   -74760
            List            =   "FrmMain.frx":0360
            Style           =   2  'Dropdown List
            TabIndex        =   48
            Top             =   780
            Width           =   1095
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
            TabIndex        =   45
            Top             =   1260
            Width           =   4695
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
               TabIndex        =   47
               ToolTipText     =   "taProducao"
               Top             =   360
               Width           =   1095
            End
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
               TabIndex        =   46
               Top             =   360
               Width           =   1455
            End
         End
         Begin VB.CheckBox chkExibirErroSchema 
            Caption         =   "Exibir Erro Schema"
            Height          =   255
            Left            =   120
            TabIndex        =   44
            Top             =   900
            Width           =   3375
         End
         Begin VB.CheckBox chkAtualizarXML 
            Caption         =   "Atualizar XML"
            Height          =   255
            Left            =   120
            TabIndex        =   43
            Top             =   540
            Width           =   3375
         End
         Begin VB.TextBox txtFormatoAlerta 
            Height          =   285
            Left            =   120
            TabIndex        =   42
            Top             =   1620
            Width           =   4695
         End
         Begin VB.ComboBox cmbFormaEmissao 
            Height          =   315
            ItemData        =   "FrmMain.frx":03D0
            Left            =   120
            List            =   "FrmMain.frx":03EF
            Style           =   2  'Dropdown List
            TabIndex        =   41
            Top             =   2220
            Width           =   2295
         End
         Begin VB.CheckBox chkRetirarAcentos 
            Caption         =   "Retirar Acentos dos XMLs enviados"
            Height          =   255
            Left            =   120
            TabIndex        =   40
            Top             =   2700
            Width           =   3375
         End
         Begin VB.CheckBox chkSalvar 
            Caption         =   "Salvar Arquivos de Envio e Resposta"
            Height          =   255
            Left            =   120
            TabIndex        =   39
            Top             =   3060
            Width           =   3375
         End
         Begin VB.CommandButton btnSelectLog 
            Caption         =   "..."
            Height          =   260
            Left            =   4440
            TabIndex        =   38
            Top             =   3660
            Width           =   390
         End
         Begin VB.TextBox txtLogs 
            Height          =   285
            Left            =   120
            TabIndex        =   37
            Top             =   3660
            Width           =   4215
         End
         Begin VB.CommandButton btnSelectSchema 
            Caption         =   "..."
            Height          =   260
            Left            =   4440
            TabIndex        =   36
            Top             =   4260
            Width           =   390
         End
         Begin VB.TextBox txtSchemaPath 
            Height          =   285
            Left            =   120
            TabIndex        =   35
            Top             =   4260
            Width           =   4215
         End
         Begin VB.CheckBox chkSalvarArqs 
            Caption         =   "Salvar Arquivos em Pastas Separadas"
            Height          =   195
            Left            =   -74880
            TabIndex        =   34
            Top             =   480
            Width           =   4335
         End
         Begin VB.CheckBox chkPastaMensal 
            Caption         =   "Criar Pastas Mensalmente"
            Height          =   255
            Left            =   -74880
            TabIndex        =   33
            Top             =   780
            Width           =   3855
         End
         Begin VB.CheckBox chkAdicionaLiteral 
            Caption         =   "Adicionar Literal no nome das pastas"
            Height          =   195
            Left            =   -74880
            TabIndex        =   32
            Top             =   1140
            Width           =   3615
         End
         Begin VB.CheckBox chkEmissaoPatheSocial 
            Caption         =   "Salvar Documento pelo campo Data de Emissão"
            Height          =   195
            Left            =   -74880
            TabIndex        =   31
            Top             =   1500
            Width           =   4455
         End
         Begin VB.CheckBox chkSalvaPathEvento 
            Caption         =   "Salvar Arquivos de Eventos"
            Height          =   195
            Left            =   -74880
            TabIndex        =   30
            Top             =   1860
            Width           =   3855
         End
         Begin VB.CheckBox chkSepararPorCNPJ 
            Caption         =   "Separar Arqs pelo CNPJ do Certificado"
            Height          =   195
            Left            =   -74880
            TabIndex        =   29
            Top             =   2220
            Width           =   3615
         End
         Begin VB.CheckBox chkSepararPorModelo 
            Caption         =   "Separar Arqs pelo Modelo do Documento"
            Height          =   195
            Left            =   -74880
            TabIndex        =   28
            Top             =   2580
            Width           =   3495
         End
         Begin VB.TextBox txtArqeSocial 
            Height          =   285
            Left            =   -74880
            TabIndex        =   27
            Top             =   3300
            Width           =   3555
         End
         Begin VB.CommandButton btnArqeSocial 
            Caption         =   "..."
            Height          =   260
            Left            =   -71280
            TabIndex        =   26
            Top             =   3300
            Width           =   390
         End
         Begin VB.TextBox txtTimeOut 
            Alignment       =   1  'Right Justify
            Height          =   285
            Left            =   -71280
            TabIndex        =   25
            Text            =   "0"
            Top             =   780
            Width           =   570
         End
         Begin MSComCtl2.UpDown nudTimeOut 
            Height          =   285
            Left            =   -70709
            TabIndex        =   87
            Top             =   780
            Width           =   255
            _ExtentX        =   450
            _ExtentY        =   503
            _Version        =   393216
            Value           =   5000
            BuddyControl    =   "txtTimeOut"
            BuddyDispid     =   196692
            OrigLeft        =   4560
            OrigTop         =   780
            OrigRight       =   4815
            OrigBottom      =   1065
            Max             =   99999
            SyncBuddy       =   -1  'True
            BuddyProperty   =   0
            Enabled         =   -1  'True
         End
         Begin VB.Label Label4 
            Caption         =   "Versão DF"
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
            Left            =   -74760
            TabIndex        =   106
            Top             =   2400
            Width           =   1815
         End
         Begin VB.Label Label3 
            Caption         =   "Tipo de Empregador"
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
            Left            =   -74760
            TabIndex        =   104
            Top             =   1680
            Width           =   1815
         End
         Begin VB.Label Label2 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "CNPJ ou CPF do Transmissor"
            BeginProperty Font 
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
            TabIndex        =   102
            Top             =   1080
            Width           =   2355
         End
         Begin VB.Label Label1 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "CNPJ ou CPF do Empregador"
            BeginProperty Font 
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
            Top             =   480
            Width           =   2340
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
            TabIndex        =   98
            Top             =   420
            Width           =   705
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
            TabIndex        =   97
            Top             =   1020
            Width           =   615
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
            TabIndex        =   96
            Top             =   1620
            Width           =   915
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
            TabIndex        =   95
            Top             =   540
            Width           =   765
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
            TabIndex        =   94
            Top             =   540
            Width           =   765
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
            TabIndex        =   93
            Top             =   540
            Width           =   900
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
            TabIndex        =   92
            Top             =   1380
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
            TabIndex        =   91
            Top             =   1980
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
            TabIndex        =   90
            Top             =   3420
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
            TabIndex        =   89
            Top             =   4020
            Width           =   4575
         End
         Begin VB.Label lblPastaArqeSocial 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "Pasta Arquivos eSocial"
            BeginProperty Font 
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
            TabIndex        =   88
            Top             =   3060
            Width           =   1920
         End
      End
   End
End
Attribute VB_Name = "FrmMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim eSocial As ACBreSocial

Private Sub Form_Load()
    cmbUFDestino.Text = "SP"
    cmbSSlType.ListIndex = 0
    cmbCrypt.ListIndex = 0
    cmbHttp.ListIndex = 0
    cmbXmlSign.ListIndex = 0
    cmbTipoEmpregador.ListIndex = 0
    cmbVersaoDF.ListIndex = 0
        
    Dim LogPath As String
    Dim IniPath As String
    
    LogPath = App.Path & "\Logs\"
    IniPath = App.Path & "\ACBrLib.ini"
    
    If Not DirExists(LogPath) Then
        MkDir LogPath
    End If
    
    Set eSocial = CreateeSocial(IniPath)
    
    eSocial.ConfigGravarValor SESSAO_PRINCIPAL, "LogNivel", NivelLog.logParanoico
    eSocial.ConfigGravarValor SESSAO_PRINCIPAL, "LogPath", LogPath
    eSocial.ConfigGravar
    
    LoadConfig
End Sub

Private Sub btnCarregarConfiguracoes_Click()
    LoadConfig
End Sub

Private Sub cmdSalvar_Click()
    SalvarConfig
End Sub

Private Sub btnCriarEventoeSocial_Click()
    On Error GoTo Erro:
    
    CommonDialog1.DialogTitle = "Selecione o arquivo ini"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivo Ini eSocial (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
            
    If CommonDialog1.FileName = vbNullString Then Exit Sub
    
    eSocial.CriarEventoeSocial (CommonDialog1.FileName)
   
Erro:
    MsgBox Err.Description
End Sub

Private Sub btnEnviareSocial_Click()
    On Error GoTo Erro:
    
    Dim ret As String
    Dim grupo As Long
    
    grupo = InputBox("Informe o Grupo", "Enviar eSocial", 1)
    
    ret = eSocial.EnviareSocial(grupo)
    rtbRespostas.Text = ret
    
Erro:
    MsgBox Err.Description
End Sub

Private Sub btnConsultareSocial_Click()
    On Erro GoTo Erro:
    
    Dim ret As String
    Dim protocolo As String
    
    protocolo = InputBox("Informe o Protocolo", "Consulta eSocial", "")
    
    rtbRespostas.Text = eSocial.ConsultareSocial(protocolo)
    
Erro:
    MsgBox Err.Description
End Sub

Private Sub btnCriarEnviareSocial_Click()
On Error GoTo Erro:
    
    Dim grupo As Long
    
    CommonDialog1.DialogTitle = "Selecione o arquivo ini"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivo Ini NFe (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
            
    If CommonDialog1.FileName = vbNullString Then Exit Sub
    
    grupo = InputBox("Informe o Grupo", "Enviar eSocial", 0)
    
    eSocial.CriarEnviareSocial CommonDialog1.FileName, grupo
    
Erro:
    MsgBox Err.Description
End Sub

Private Sub btnLimpareSocial_Click()
    On Error GoTo Erro:
    answer = MsgBox("Limpar Lista ?", vbExclamation + vbYesNo, "Add Confirm")
    If answer = vbYes Then
    eSocial.LimpareSocial
    End If

Erro:
    MsgBox Err.Description
End Sub

Private Sub btnCarregarXMLEventoeSocial_Click()
    On Error GoTo Erro:
    
    CommonDialog1.DialogTitle = "Selecione o arquivo xml"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivo Xml eSocial (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
            
    If CommonDialog1.FileName = vbNullString Then Exit Sub
    
    eSocial.CarregarXMLEventoeSocial (CommonDialog1.FileName)
    
Erro:
    MsgBox Err.Description
End Sub

Private Sub btnSetIDEmpregador_Click()
    On Erro GoTo Erro:
        
    eSocial.SetIDEmpregador (txtEmpregador.Text)
    
Erro:
    MsgBox Err.Description
End Sub

Private Sub btnSetIDTransmissor_Click()
    On Erro GoTo Erro:
    
    eSocial.SetIDTransmissor (txtTransmissor.Text)
    
Erro:
    MsgBox Err.Description
End Sub

Private Sub btnSetTipoEmpregador_Click()
    On Erro GoTo Erro:
    
    eSocial.SetTipoEmpregador (cmbTipoEmpregador.ListIndex)
    
Erro:
    MsgBox Err.Description
End Sub

Private Sub btnSetVersaoDF_Click()
    On Erro GoTo Erro:
        
    eSocial.SetVersaoDF (cmbVersaoDF.ListIndex)

Erro:
    MsgBox Err.Description
End Sub

Private Sub btnConsultaIdentificadoresEventosEmpregador_Click()
    On Erro GoTo Erro:
    
    Dim ret As String
    Dim tipoEvento As Long
    Dim dataApuracao As Date
    
    tipoEvento = InputBox("Informe o Tipo do Evento", "Consulta Empregador eSocial", "")
    dataApuracao = InputBox("Informe a Data de Apuração", "Consulta Empregador eSocial", "01/MM/AAAA")
    
    ret = eSocial.ConsultaIdentificadoresEventosEmpregador(txtEmpregador.Text, tipoEvento, dataApuracao)
    rtbRespostas.Text = ret
    
Erro:
    MsgBox Err.Description
End Sub

Private Sub btnConsultaIdentificadoresEventosTabela_Click()
    On Erro GoTo Erro:
    
    Dim ret As String
    Dim tipoEvento As Long
    Dim chave As String
    Dim dataInicial As Date
    Dim dataFinal As Date
    

    tipoEvento = InputBox("Informe o Tipo do Evento", "Consulta Tabela eSocial", "")
    chave = InputBox("Informe a Chave", "Consulta Tabela eSocial", "")
    dataInicial = InputBox("Informe a Data Inicial", "Consulta Tabela eSocial", "01/MM/AAAA")
    dataFinal = InputBox("Informe a Data Final", "Consulta Tabela eSocial", "01/MM/AAAA")
    
    ret = eSocial.ConsultaIdentificadoresEventosTabela(txtEmpregador.Text, tipoEvento, chave, dataInicial, dataFinal)
    rtbRespostas.Text = ret
    
Erro:
    MsgBox Err.Description
End Sub

Private Sub btnConsultaIdentificadoresEventosTrabalhador_Click()
    On Erro GoTo Erro:
    
    Dim ret As String
    Dim tipoEvento As Long
    Dim cpfTrabalhador As String
    Dim dataInicial As Date
    Dim dataFinal As Date
    
    tipoEvento = InputBox("Informe o Tipo do Evento", "Consulta Trabalhador eSocial", "")
    cpfTrabalhador = InputBox("Informe o CPF do Trabalhador", "Consulta Trabalhador eSocial", "")
    dataInicial = InputBox("Informe a Data Inicial", "Consulta Tabela eSocial", "01/MM/AAAA")
    dataFinal = InputBox("Informe a Data Final", "Consulta Tabela eSocial", "01/MM/AAAA")
    
    ret = eSocial.ConsultaIdentificadoresEventosTrabalhador(txtEmpregador.Text, cpfTrabalhador, dataInicial, dataFinal)
    rtbRespostas.Text = ret
    
Erro:
    MsgBox Err.Description
End Sub

Private Sub btnDownloadEventos_Click()
    On Erro GoTo Erro:
    
    Dim ret As String
    Dim tipoEvento As Long
    Dim cpfTrabalhador As String
    Dim dataInicial As Date
    Dim dataFinal As Date

    tipoEvento = InputBox("Informe o Tipo do Evento", "Download Eventos", "")
    cpfTrabalhador = InputBox("Informe o CPF do Trabalhador", "Download Eventos", "")
    dataInicial = InputBox("Informe a Data Inicial", "Consulta Tabela eSocial", "01/MM/AAAA")
    dataFinal = InputBox("Informe a Data Final", "Consulta Tabela eSocial", "01/MM/AAAA")
    
    ret = eSocial.ConsultaIdentificadoresEventosTrabalhador(txtEmpregador.Text, cpfTrabalhador, dataInicial, dataFinal)
    rtbRespostas.Text = ret
    
Erro:
    MsgBox Err.Description
End Sub

Private Sub btnObterCertificados_Click()
    On Error GoTo Erro:
    eSocial.ObterCertificados
Erro:
    MsgBox Err.Description
End Sub

Private Sub btnArqeSocial_Click()
    txtArqeSocial.Text = BrowseFolder("Selecione a pasta Arquivos eSocial")
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

Private Sub btnSelectLog_Click()
    txtLogs.Text = BrowseFolder("Selecione a pasta dos Logs")
End Sub

Private Sub btnSelectSchema_Click()
    txtSchemaPath.Text = BrowseFolder("Selecione a pasta dos Schemas")
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
    
    eSocial.ConfigLer
    
    'Geral
    chkExibirErroSchema.Value = CLng(eSocial.ConfigLerValor(SESSAO_eSocial, "ExibirErroSchema"))
    txtFormatoAlerta.Text = eSocial.ConfigLerValor(SESSAO_eSocial, "FormatoAlerta")
    cmbFormaEmissao.ListIndex = CLng(eSocial.ConfigLerValor(SESSAO_eSocial, "FormaEmissao"))
    chkRetirarAcentos.Value = CLng(eSocial.ConfigLerValor(SESSAO_eSocial, "RetirarAcentos"))
    chkSalvar.Value = CLng(eSocial.ConfigLerValor(SESSAO_eSocial, "SalvarWS"))
    txtLogs.Text = eSocial.ConfigLerValor(SESSAO_eSocial, "PathSalvar")
    txtSchemaPath.Text = eSocial.ConfigLerValor(SESSAO_eSocial, "PathSchemas")
    txtEmpregador.Text = eSocial.ConfigLerValor(SESSAO_eSocial, "IdEmpregador")
    txtTransmissor.Text = eSocial.ConfigLerValor(SESSAO_eSocial, "IdTransmissor")
    cmbTipoEmpregador.ListIndex = eSocial.ConfigLerValor(SESSAO_eSocial, "TipoEmpregador")
    cmbVersaoDF.ListIndex = CLng(eSocial.ConfigLerValor(SESSAO_eSocial, "VersaoDF"))
    
    'WebSevice
    cmbUFDestino.Text = eSocial.ConfigLerValor(SESSAO_DFE, "UF")
    cmbSSlType.ListIndex = CLng(eSocial.ConfigLerValor(SESSAO_eSocial, "SSLType"))
    nudTimeOut.Value = CLng(eSocial.ConfigLerValor(SESSAO_eSocial, "Timeout"))
    
    Dim ambiente As String
    ambiente = eSocial.ConfigLerValor(SESSAO_eSocial, "Ambiente")
    rdbHomologacao.Value = CBool(ambiente)
    rdbProducao.Value = Not CBool(ambiente)
    
    chkVisualizarMensagem.Value = CLng(eSocial.ConfigLerValor(SESSAO_eSocial, "Visualizar"))
    chkSalvarSOAP.Value = CLng(eSocial.ConfigLerValor(SESSAO_eSocial, "SalvarWS"))
    chkAjustAut.Value = CLng(eSocial.ConfigLerValor(SESSAO_eSocial, "AjustaAguardaConsultaRet"))
    nudAguardar.Value = CLng(eSocial.ConfigLerValor(SESSAO_eSocial, "AguardarConsultaRet"))
    nudTentativas.Value = CLng(eSocial.ConfigLerValor(SESSAO_eSocial, "Tentativas"))
    nudIntervalo.Value = CLng(eSocial.ConfigLerValor(SESSAO_eSocial, "IntervaloTentativas"))
    
    'Proxy
    txtProxyServidor.Text = eSocial.ConfigLerValor(SESSAO_PROXY, "Servidor")
    nudProxyPorta.Value = CLng(eSocial.ConfigLerValor(SESSAO_PROXY, "Porta"))
    txtProxyUsuario.Text = eSocial.ConfigLerValor(SESSAO_PROXY, "Usuario")
    txtProxySenha.Text = eSocial.ConfigLerValor(SESSAO_PROXY, "Senha")
    
    'Certificado
    cmbCrypt.ListIndex = CLng(eSocial.ConfigLerValor(SESSAO_DFE, "SSLCryptLib"))
    cmbHttp.ListIndex = CLng(eSocial.ConfigLerValor(SESSAO_DFE, "SSLHttpLib"))
    cmbXmlSign.ListIndex = CLng(eSocial.ConfigLerValor(SESSAO_DFE, "SSLXmlSignLib"))
    txtCertPath.Text = eSocial.ConfigLerValor(SESSAO_DFE, "ArquivoPFX")
    txtCertPassword.Text = eSocial.ConfigLerValor(SESSAO_DFE, "Senha")
    txtCertNumero.Text = eSocial.ConfigLerValor(SESSAO_DFE, "NumeroSerie")
    txtDadosPFX.Text = eSocial.ConfigLerValor(SESSAO_DFE, "DadosPFX")
    
    'Arquivos
    chkSalvarArqs.Value = CLng(eSocial.ConfigLerValor(SESSAO_eSocial, "SalvarGer"))
    chkPastaMensal.Value = CLng(eSocial.ConfigLerValor(SESSAO_eSocial, "SepararPorMes"))
    chkAdicionaLiteral.Value = CLng(eSocial.ConfigLerValor(SESSAO_eSocial, "AdicionarLiteral"))
    chkSalvaPathEvento.Value = CLng(eSocial.ConfigLerValor(SESSAO_eSocial, "SalvarArq"))
    chkSepararPorCNPJ.Value = CLng(eSocial.ConfigLerValor(SESSAO_eSocial, "SepararPorCNPJ"))
    chkSepararPorModelo.Value = CLng(eSocial.ConfigLerValor(SESSAO_eSocial, "SepararPorModelo"))
    chkEmissaoPatheSocial.Value = CLng(eSocial.ConfigLerValor(SESSAO_eSocial, "EmissaoPatheSocial"))
    txtArqeSocial.Text = eSocial.ConfigLerValor(SESSAO_eSocial, "PatheSocial")
    
End Sub

Private Sub SalvarConfig()
    
    'Geral
    eSocial.ConfigGravarValor SESSAO_eSocial, "ExibirErroSchema", CStr(chkExibirErroSchema.Value)
    eSocial.ConfigGravarValor SESSAO_eSocial, "FormatoAlerta", txtFormatoAlerta.Text
    eSocial.ConfigGravarValor SESSAO_eSocial, "FormaEmissao", CStr(cmbFormaEmissao.ListIndex)
    eSocial.ConfigGravarValor SESSAO_eSocial, "RetirarAcentos", CStr(chkRetirarAcentos.Value)
    eSocial.ConfigGravarValor SESSAO_eSocial, "SalvarWS", CStr(chkSalvar.Value)
    eSocial.ConfigGravarValor SESSAO_eSocial, "PathSalvar", txtLogs.Text
    eSocial.ConfigGravarValor SESSAO_eSocial, "PathSchemas", txtSchemaPath.Text
    eSocial.ConfigGravarValor SESSAO_eSocial, "IdEmpregador", txtEmpregador.Text
    eSocial.ConfigGravarValor SESSAO_eSocial, "IdTransmissor", txtTransmissor.Text
    eSocial.ConfigGravarValor SESSAO_eSocial, "TipoEmpregador", CStr(cmbTipoEmpregador.ListIndex)
    eSocial.ConfigGravarValor SESSAO_eSocial, "VersaoDF", CStr(cmbVersaoDF.ListIndex)
        
    'WebService
    eSocial.ConfigGravarValor SESSAO_DFE, "UF", CStr(cmbUFDestino.ListIndex)
    eSocial.ConfigGravarValor SESSAO_eSocial, "SSLType", CStr(cmbSSlType.ListIndex)
    eSocial.ConfigGravarValor SESSAO_eSocial, "Timeout", CStr(nudTimeOut.Value)
    eSocial.ConfigGravarValor SESSAO_eSocial, "Ambiente", CStr(rdbProducao.Value)
    eSocial.ConfigGravarValor SESSAO_eSocial, "Ambiente", CStr(rdbHomologacao.Value)
    eSocial.ConfigGravarValor SESSAO_eSocial, "Visualizar", CStr(chkVisualizarMensagem)
    eSocial.ConfigGravarValor SESSAO_eSocial, "SalvarWS", CStr(chkSalvarSOAP.Value)
    eSocial.ConfigGravarValor SESSAO_eSocial, "AjustaAguardaConsultaRet", CStr(chkAjustAut.Value)
    eSocial.ConfigGravarValor SESSAO_eSocial, "AguardarConsultaRet", CStr(nudAguardar.Value)
    eSocial.ConfigGravarValor SESSAO_eSocial, "Tentativas", CStr(nudTentativas.Value)
    eSocial.ConfigGravarValor SESSAO_eSocial, "IntervaloTentativas", CStr(nudIntervalo.Value)
    
    'Proxy
    eSocial.ConfigGravarValor SESSAO_PROXY, "Servidor", txtProxyServidor.Text
    eSocial.ConfigGravarValor SESSAO_PROXY, "Porta", CStr(nudProxyPorta.Value)
    eSocial.ConfigGravarValor SESSAO_PROXY, "Usuario", txtProxyUsuario.Text
    eSocial.ConfigGravarValor SESSAO_PROXY, "Senha", txtProxySenha.Text
    
    'Certificado
    eSocial.ConfigGravarValor SESSAO_DFE, "SSLCryptLib", CStr(cmbCrypt.ListIndex)
    eSocial.ConfigGravarValor SESSAO_DFE, "SSLHttpLib", CStr(cmbHttp.ListIndex)
    eSocial.ConfigGravarValor SESSAO_DFE, "SSLXmlSignLib", CStr(cmbXmlSign.ListIndex)
    eSocial.ConfigGravarValor SESSAO_DFE, "ArquivoPFX", txtCertPath.Text
    eSocial.ConfigGravarValor SESSAO_DFE, "Senha", txtCertPassword.Text
    eSocial.ConfigGravarValor SESSAO_DFE, "NumeroSerie", txtCertNumero.Text
    eSocial.ConfigGravarValor SESSAO_DFE, "DadosPFX", txtDadosPFX.Text
    
    'Arquivos
    eSocial.ConfigGravarValor SESSAO_eSocial, "SalvarGer", CStr(chkSalvarArqs.Value)
    eSocial.ConfigGravarValor SESSAO_eSocial, "SepararPorMes", CStr(chkPastaMensal.Value)
    eSocial.ConfigGravarValor SESSAO_eSocial, "AdicionarLiteral", CStr(chkAdicionaLiteral.Value)
    eSocial.ConfigGravarValor SESSAO_eSocial, "SalvarArq", CStr(chkSalvarArqs.Value)
    eSocial.ConfigGravarValor SESSAO_eSocial, "SepararPorCNPJ", CStr(chkSepararPorCNPJ.Value)
    eSocial.ConfigGravarValor SESSAO_eSocial, "SepararPorModelo", CStr(chkSepararPorModelo.Value)
    eSocial.ConfigGravarValor SESSAO_eSocial, "EmissaoPatheSocial", CStr(chkEmissaoPatheSocial.Value)
    eSocial.ConfigGravarValor SESSAO_eSocial, "PatheSocial", txtArqeSocial.Text
    
    eSocial.ConfigGravar

End Sub
