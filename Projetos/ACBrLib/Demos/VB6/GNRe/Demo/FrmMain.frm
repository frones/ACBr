VERSION 5.00
Object = "{86CF1D34-0C5F-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCT2.OCX"
Object = "{BDC217C8-ED16-11CD-956C-0000C04E4C0A}#1.1#0"; "TABCTL32.OCX"
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "COMDLG32.OCX"
Begin VB.Form FrmMain 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "ACBrLibGNRe Demo"
   ClientHeight    =   7800
   ClientLeft      =   45
   ClientTop       =   390
   ClientWidth     =   9705
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
   ScaleHeight     =   7800
   ScaleWidth      =   9705
   StartUpPosition =   2  'CenterScreen
   Begin VB.CommandButton btnCarregarConfiguracoes 
      Caption         =   "Carregar Configurações"
      Height          =   480
      Left            =   120
      TabIndex        =   101
      Top             =   7080
      Width           =   2295
   End
   Begin TabDlg.SSTab SSTTab1 
      Height          =   3015
      Left            =   5160
      TabIndex        =   4
      Top             =   120
      Width           =   4335
      _ExtentX        =   7646
      _ExtentY        =   5318
      _Version        =   393216
      Style           =   1
      Tabs            =   2
      TabHeight       =   520
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Tahoma"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      TabCaption(0)   =   "Envios"
      TabPicture(0)   =   "FrmMain.frx":0000
      Tab(0).ControlEnabled=   -1  'True
      Tab(0).Control(0)=   "cmdGerarGNRe"
      Tab(0).Control(0).Enabled=   0   'False
      Tab(0).Control(1)=   "cmdEnviarEmail"
      Tab(0).Control(1).Enabled=   0   'False
      Tab(0).Control(2)=   "btnCarregarIni"
      Tab(0).Control(2).Enabled=   0   'False
      Tab(0).Control(3)=   "btnImprimir"
      Tab(0).Control(3).Enabled=   0   'False
      Tab(0).Control(4)=   "btnEnviar"
      Tab(0).Control(4).Enabled=   0   'False
      Tab(0).Control(5)=   "btnCarregarXml"
      Tab(0).Control(5).Enabled=   0   'False
      Tab(0).Control(6)=   "btnImprimirPDF"
      Tab(0).Control(6).Enabled=   0   'False
      Tab(0).Control(7)=   "btnLimparLista"
      Tab(0).Control(7).Enabled=   0   'False
      Tab(0).ControlCount=   8
      TabCaption(1)   =   "Consultas"
      TabPicture(1)   =   "FrmMain.frx":001C
      Tab(1).ControlEnabled=   0   'False
      Tab(1).Control(0)=   "cmdConsultaConfiguração"
      Tab(1).ControlCount=   1
      Begin VB.CommandButton btnLimparLista 
         Caption         =   "Limpar Lista GNRe"
         Height          =   360
         Left            =   1920
         TabIndex        =   108
         Top             =   1920
         Width           =   1710
      End
      Begin VB.CommandButton btnImprimirPDF 
         Caption         =   "Imprimir PDF GNRe"
         Height          =   360
         Left            =   1920
         TabIndex        =   107
         Top             =   1440
         Width           =   1710
      End
      Begin VB.CommandButton btnCarregarXml 
         Caption         =   "Carregar Xml GNRe"
         Height          =   360
         Left            =   1950
         TabIndex        =   106
         Top             =   960
         Width           =   1710
      End
      Begin VB.CommandButton btnEnviar 
         Caption         =   "Enviar"
         Height          =   360
         Left            =   1920
         TabIndex        =   105
         Top             =   480
         Width           =   1710
      End
      Begin VB.CommandButton btnImprimir 
         Caption         =   "Imprimir GNRe"
         Height          =   360
         Left            =   120
         TabIndex        =   104
         Top             =   1440
         Width           =   1710
      End
      Begin VB.CommandButton btnCarregarIni 
         Caption         =   "Carregar INI GNRe"
         Height          =   360
         Left            =   120
         TabIndex        =   103
         Top             =   960
         Width           =   1710
      End
      Begin VB.CommandButton cmdConsultaConfiguração 
         Caption         =   "Consulta Configuração UF"
         Height          =   360
         Left            =   -74880
         TabIndex        =   41
         Top             =   480
         Width           =   2310
      End
      Begin VB.CommandButton cmdEnviarEmail 
         Caption         =   "Enviar Email"
         Height          =   360
         Left            =   120
         TabIndex        =   40
         Top             =   1920
         Width           =   1710
      End
      Begin VB.CommandButton cmdGerarGNRe 
         Caption         =   "Gerar GNRe"
         Height          =   360
         Left            =   120
         TabIndex        =   39
         Top             =   480
         Width           =   1710
      End
   End
   Begin MSComDlg.CommonDialog CommonDialog1 
      Left            =   5400
      Top             =   7320
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
   End
   Begin VB.Frame FraRespostas 
      Caption         =   "Respostas"
      BeginProperty Font 
         Name            =   "Tahoma"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   4215
      Left            =   5160
      TabIndex        =   2
      Top             =   3240
      Width           =   4335
      Begin VB.TextBox rtbRespostas 
         Height          =   3735
         Left            =   120
         MultiLine       =   -1  'True
         ScrollBars      =   3  'Both
         TabIndex        =   3
         Top             =   360
         Width           =   4095
      End
   End
   Begin VB.CommandButton cmdSalvar 
      Caption         =   "Salvar Configurações"
      Height          =   480
      Left            =   2760
      TabIndex        =   1
      Top             =   7080
      Width           =   2295
   End
   Begin TabDlg.SSTab SSTTab0 
      Height          =   6735
      Left            =   120
      TabIndex        =   0
      Top             =   120
      Width           =   4935
      _ExtentX        =   8705
      _ExtentY        =   11880
      _Version        =   393216
      Style           =   1
      Tabs            =   2
      TabsPerRow      =   4
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
      TabPicture(0)   =   "FrmMain.frx":0038
      Tab(0).ControlEnabled=   -1  'True
      Tab(0).Control(0)=   "SSTTab2"
      Tab(0).Control(0).Enabled=   0   'False
      Tab(0).ControlCount=   1
      TabCaption(1)   =   "Email"
      TabPicture(1)   =   "FrmMain.frx":0054
      Tab(1).ControlEnabled=   0   'False
      Tab(1).Control(0)=   "lblMensagem"
      Tab(1).Control(1)=   "lblAssunto"
      Tab(1).Control(2)=   "txtEmailMensagem"
      Tab(1).Control(3)=   "txtEmailAssunto"
      Tab(1).Control(4)=   "FraConfigurações"
      Tab(1).ControlCount=   5
      Begin VB.Frame FraConfigurações 
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
         Height          =   3975
         Left            =   -74880
         TabIndex        =   8
         Top             =   480
         Width           =   4695
         Begin VB.TextBox txtEmailNome 
            Height          =   285
            IMEMode         =   3  'DISABLE
            Left            =   120
            TabIndex        =   16
            Top             =   480
            Width           =   4515
         End
         Begin VB.TextBox txtEmail 
            Height          =   285
            IMEMode         =   3  'DISABLE
            Left            =   120
            TabIndex        =   15
            Top             =   1080
            Width           =   4515
         End
         Begin VB.TextBox txtEmailHost 
            Height          =   285
            IMEMode         =   3  'DISABLE
            Left            =   120
            TabIndex        =   14
            Top             =   1680
            Width           =   4515
         End
         Begin VB.TextBox txtEmailUsuario 
            Height          =   285
            IMEMode         =   3  'DISABLE
            Left            =   120
            TabIndex        =   13
            Top             =   2880
            Width           =   4515
         End
         Begin VB.TextBox txtEmailSenha 
            Height          =   285
            IMEMode         =   3  'DISABLE
            Left            =   120
            PasswordChar    =   "*"
            TabIndex        =   12
            Top             =   3480
            Width           =   4515
         End
         Begin VB.TextBox txtEmailPorta 
            Alignment       =   1  'Right Justify
            Height          =   285
            Left            =   120
            TabIndex        =   11
            Text            =   "0"
            Top             =   2280
            Width           =   825
         End
         Begin VB.CheckBox chkEmailSSL 
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
            Left            =   1320
            TabIndex        =   10
            Top             =   2160
            Width           =   1335
         End
         Begin VB.CheckBox chkEmailTLS 
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
            Left            =   1320
            TabIndex        =   9
            Top             =   2400
            Width           =   1335
         End
         Begin MSComCtl2.UpDown nudEmailPorta 
            Height          =   285
            Left            =   945
            TabIndex        =   17
            Top             =   2280
            Width           =   255
            _ExtentX        =   450
            _ExtentY        =   503
            _Version        =   393216
            Value           =   5000
            BuddyControl    =   "txtEmailPorta"
            BuddyDispid     =   196628
            OrigLeft        =   840
            OrigTop         =   2280
            OrigRight       =   1095
            OrigBottom      =   2565
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
            Left            =   120
            TabIndex        =   23
            Top             =   240
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
            Left            =   120
            TabIndex        =   22
            Top             =   840
            Width           =   450
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
            Left            =   120
            TabIndex        =   21
            Top             =   1440
            Width           =   900
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
            Index           =   0
            Left            =   120
            TabIndex        =   20
            Top             =   2640
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
            Index           =   1
            Left            =   120
            TabIndex        =   19
            Top             =   3240
            Width           =   525
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
            Index           =   0
            Left            =   120
            TabIndex        =   18
            Top             =   2040
            Width           =   465
         End
      End
      Begin VB.TextBox txtEmailAssunto 
         Height          =   285
         IMEMode         =   3  'DISABLE
         Left            =   -74880
         TabIndex        =   7
         Top             =   4800
         Width           =   4635
      End
      Begin VB.TextBox txtEmailMensagem 
         Height          =   1245
         IMEMode         =   3  'DISABLE
         Left            =   -74880
         MultiLine       =   -1  'True
         ScrollBars      =   3  'Both
         TabIndex        =   6
         Top             =   5400
         Width           =   4635
      End
      Begin TabDlg.SSTab SSTTab2 
         Height          =   6255
         Left            =   120
         TabIndex        =   5
         Top             =   240
         Width           =   4725
         _ExtentX        =   8334
         _ExtentY        =   11033
         _Version        =   393216
         Style           =   1
         Tabs            =   4
         TabsPerRow      =   4
         TabHeight       =   520
         TabCaption(0)   =   "Geral"
         TabPicture(0)   =   "FrmMain.frx":0070
         Tab(0).ControlEnabled=   -1  'True
         Tab(0).Control(0)=   "lblFormatoAlerta"
         Tab(0).Control(0).Enabled=   0   'False
         Tab(0).Control(1)=   "lblFormaEmissão"
         Tab(0).Control(1).Enabled=   0   'False
         Tab(0).Control(2)=   "lblVersãoDocumento"
         Tab(0).Control(2).Enabled=   0   'False
         Tab(0).Control(3)=   "lblCaminhoLogs"
         Tab(0).Control(3).Enabled=   0   'False
         Tab(0).Control(4)=   "lblCaminhoSchemas"
         Tab(0).Control(4).Enabled=   0   'False
         Tab(0).Control(5)=   "cmbFormaEmissao"
         Tab(0).Control(5).Enabled=   0   'False
         Tab(0).Control(6)=   "cmbVersaoDFe"
         Tab(0).Control(6).Enabled=   0   'False
         Tab(0).Control(7)=   "chkRetirarAcentos"
         Tab(0).Control(7).Enabled=   0   'False
         Tab(0).Control(8)=   "chkSalvarEnvio"
         Tab(0).Control(8).Enabled=   0   'False
         Tab(0).Control(9)=   "txtLogs"
         Tab(0).Control(9).Enabled=   0   'False
         Tab(0).Control(10)=   "cmdLogs"
         Tab(0).Control(10).Enabled=   0   'False
         Tab(0).Control(11)=   "txtSchemaPath"
         Tab(0).Control(11).Enabled=   0   'False
         Tab(0).Control(12)=   "cmdSchemas"
         Tab(0).Control(12).Enabled=   0   'False
         Tab(0).Control(13)=   "chkExibirErro"
         Tab(0).Control(13).Enabled=   0   'False
         Tab(0).Control(14)=   "txtFormatoAlerta"
         Tab(0).Control(14).Enabled=   0   'False
         Tab(0).ControlCount=   15
         TabCaption(1)   =   "Webservice"
         TabPicture(1)   =   "FrmMain.frx":008C
         Tab(1).ControlEnabled=   0   'False
         Tab(1).Control(0)=   "ckbSalvarSOAP"
         Tab(1).Control(1)=   "ckbVisualizar"
         Tab(1).Control(2)=   "frmRetEnvio"
         Tab(1).Control(3)=   "cmbUfDestino"
         Tab(1).Control(4)=   "cmbSSlType"
         Tab(1).Control(5)=   "txtTimeOut"
         Tab(1).Control(6)=   "FraAmbiente"
         Tab(1).Control(7)=   "FraProxy"
         Tab(1).Control(8)=   "nudTimeOut"
         Tab(1).Control(9)=   "lblUfDestino"
         Tab(1).Control(10)=   "lblSSLType"
         Tab(1).Control(11)=   "lblTimeOut"
         Tab(1).ControlCount=   12
         TabCaption(2)   =   "Certificado"
         TabPicture(2)   =   "FrmMain.frx":00A8
         Tab(2).ControlEnabled=   0   'False
         Tab(2).Control(0)=   "btnObterCertificados"
         Tab(2).Control(1)=   "FraCertificados"
         Tab(2).Control(2)=   "cmbXmlSign"
         Tab(2).Control(3)=   "cmbHttp"
         Tab(2).Control(4)=   "cmbCrypt"
         Tab(2).Control(5)=   "lblCrytLib"
         Tab(2).Control(6)=   "lblXmlSignLib"
         Tab(2).Control(7)=   "lblHttpLib"
         Tab(2).ControlCount=   8
         TabCaption(3)   =   "Arquivo"
         TabPicture(3)   =   "FrmMain.frx":00C4
         Tab(3).ControlEnabled=   0   'False
         Tab(3).Control(0)=   "lblPastaArquivos"
         Tab(3).Control(1)=   "chkSalvarPasta"
         Tab(3).Control(2)=   "chkSalvarMensal"
         Tab(3).Control(3)=   "chkAdicionarLiteral"
         Tab(3).Control(4)=   "chkEmissaoPath"
         Tab(3).Control(5)=   "chkSepararCNPJ"
         Tab(3).Control(6)=   "chkSepararDoc"
         Tab(3).Control(7)=   "cmdArquivos"
         Tab(3).Control(8)=   "txtArquivos"
         Tab(3).ControlCount=   9
         Begin VB.CommandButton btnObterCertificados 
            Caption         =   "Obter Certificados"
            BeginProperty Font 
               Name            =   "Tahoma"
               Size            =   8.25
               Charset         =   0
               Weight          =   700
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   360
            Left            =   -74880
            TabIndex        =   102
            Top             =   5160
            Width           =   1710
         End
         Begin VB.CheckBox ckbSalvarSOAP 
            Caption         =   "Salvar envelope SOAP"
            Height          =   255
            Left            =   -74760
            TabIndex        =   91
            Top             =   2280
            Width           =   2055
         End
         Begin VB.CheckBox ckbVisualizar 
            Caption         =   "Visualizar Mensagem"
            Height          =   255
            Left            =   -74760
            TabIndex        =   90
            Top             =   1920
            Width           =   1815
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
            Height          =   1335
            Left            =   -74880
            TabIndex        =   88
            Top             =   2640
            Width           =   4455
            Begin VB.TextBox txtAguardar 
               Alignment       =   1  'Right Justify
               Height          =   285
               Left            =   120
               TabIndex        =   100
               Text            =   "0"
               Top             =   960
               Width           =   675
            End
            Begin VB.TextBox txtTentativas 
               Alignment       =   1  'Right Justify
               Height          =   285
               Left            =   1680
               TabIndex        =   99
               Text            =   "0"
               Top             =   960
               Width           =   675
            End
            Begin VB.TextBox txtIntervalo 
               Alignment       =   1  'Right Justify
               Height          =   285
               Left            =   3120
               TabIndex        =   98
               Text            =   "0"
               Top             =   960
               Width           =   675
            End
            Begin VB.CheckBox ckbAjustarAut 
               Caption         =   "Ajustar Automaticamente prop. ""Aguardar"""
               Height          =   255
               Left            =   120
               TabIndex        =   89
               Top             =   240
               Width           =   3735
            End
            Begin MSComCtl2.UpDown nudAguardar 
               Height          =   285
               Left            =   796
               TabIndex        =   92
               Top             =   960
               Width           =   255
               _ExtentX        =   450
               _ExtentY        =   503
               _Version        =   393216
               Value           =   5000
               BuddyControl    =   "txtAguardar"
               BuddyDispid     =   196643
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
               Left            =   2356
               TabIndex        =   93
               Top             =   960
               Width           =   255
               _ExtentX        =   450
               _ExtentY        =   503
               _Version        =   393216
               Value           =   5000
               BuddyControl    =   "txtTentativas"
               BuddyDispid     =   196644
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
               Left            =   3796
               TabIndex        =   94
               Top             =   960
               Width           =   255
               _ExtentX        =   450
               _ExtentY        =   503
               _Version        =   393216
               Value           =   5000
               BuddyControl    =   "txtIntervalo"
               BuddyDispid     =   196645
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
               Left            =   3120
               TabIndex        =   97
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
               Left            =   1680
               TabIndex        =   96
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
               TabIndex        =   95
               Top             =   720
               Width           =   795
            End
         End
         Begin VB.TextBox txtArquivos 
            Height          =   285
            Left            =   -74880
            TabIndex        =   86
            Top             =   2280
            Width           =   4095
         End
         Begin VB.CommandButton cmdArquivos 
            Caption         =   "..."
            Height          =   315
            Left            =   -70800
            TabIndex        =   85
            Top             =   2250
            Width           =   390
         End
         Begin VB.CheckBox chkSepararDoc 
            Caption         =   "Separar Arqs pelo Modelo do Documento"
            BeginProperty Font 
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
            TabIndex        =   84
            Top             =   1680
            Width           =   4335
         End
         Begin VB.CheckBox chkSepararCNPJ 
            Caption         =   "Separar Arqs pelo CNPJ do Certificado"
            BeginProperty Font 
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
            TabIndex        =   83
            Top             =   1440
            Width           =   4335
         End
         Begin VB.CheckBox chkEmissaoPath 
            Caption         =   "Salvar Documento pelo campo Data de Emissão"
            BeginProperty Font 
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
            Top             =   1200
            Width           =   4335
         End
         Begin VB.CheckBox chkAdicionarLiteral 
            Caption         =   "Adicionar Literal no nome das pastas"
            BeginProperty Font 
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
            TabIndex        =   81
            Top             =   960
            Width           =   4335
         End
         Begin VB.CheckBox chkSalvarMensal 
            Caption         =   "Criar Pastas Mensalmente"
            BeginProperty Font 
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
            Top             =   720
            Width           =   4335
         End
         Begin VB.TextBox txtFormatoAlerta 
            Height          =   285
            Left            =   120
            TabIndex        =   79
            Top             =   1080
            Width           =   4455
         End
         Begin VB.CheckBox chkSalvarPasta 
            Caption         =   "Salvar Arquivos em Pastas Separadas"
            BeginProperty Font 
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
            Width           =   4335
         End
         Begin VB.Frame FraCertificados 
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
            Height          =   2775
            Left            =   -74880
            TabIndex        =   69
            Top             =   2280
            Width           =   4455
            Begin VB.CommandButton cmdDadosPFX 
               Caption         =   "..."
               Height          =   255
               Left            =   3960
               TabIndex        =   111
               Top             =   1080
               Width           =   390
            End
            Begin VB.TextBox txtDadosPFX 
               Height          =   285
               Left            =   120
               TabIndex        =   110
               Top             =   1080
               Width           =   3795
            End
            Begin VB.TextBox txtCertNumero 
               Height          =   285
               Left            =   120
               TabIndex        =   73
               Top             =   2280
               Width           =   4275
            End
            Begin VB.TextBox txtCertPassword 
               Height          =   285
               IMEMode         =   3  'DISABLE
               Left            =   120
               PasswordChar    =   "*"
               TabIndex        =   72
               Top             =   1680
               Width           =   4275
            End
            Begin VB.TextBox txtCertPath 
               Height          =   285
               Left            =   120
               TabIndex        =   71
               Top             =   480
               Width           =   3795
            End
            Begin VB.CommandButton cmdCertPath 
               Caption         =   "..."
               Height          =   255
               Left            =   3960
               TabIndex        =   70
               Top             =   480
               Width           =   390
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
               Index           =   3
               Left            =   120
               TabIndex        =   109
               Top             =   840
               Width           =   870
            End
            Begin VB.Label lblNúmeroDe 
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
               TabIndex        =   76
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
               Index           =   0
               Left            =   120
               TabIndex        =   75
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
               TabIndex        =   74
               Top             =   240
               Width           =   1245
            End
         End
         Begin VB.ComboBox cmbXmlSign 
            Height          =   315
            ItemData        =   "FrmMain.frx":00E0
            Left            =   -74880
            List            =   "FrmMain.frx":00F3
            Style           =   2  'Dropdown List
            TabIndex        =   66
            Top             =   1920
            Width           =   2175
         End
         Begin VB.ComboBox cmbHttp 
            Height          =   315
            ItemData        =   "FrmMain.frx":012D
            Left            =   -74880
            List            =   "FrmMain.frx":0140
            Style           =   2  'Dropdown List
            TabIndex        =   65
            Top             =   1320
            Width           =   2175
         End
         Begin VB.ComboBox cmbCrypt 
            Height          =   315
            ItemData        =   "FrmMain.frx":017F
            Left            =   -74880
            List            =   "FrmMain.frx":018F
            Style           =   2  'Dropdown List
            TabIndex        =   64
            Top             =   720
            Width           =   2175
         End
         Begin VB.ComboBox cmbUfDestino 
            Height          =   315
            ItemData        =   "FrmMain.frx":01C1
            Left            =   -74880
            List            =   "FrmMain.frx":0216
            Style           =   2  'Dropdown List
            TabIndex        =   58
            Top             =   720
            Width           =   975
         End
         Begin VB.ComboBox cmbSSlType 
            Height          =   315
            ItemData        =   "FrmMain.frx":0286
            Left            =   -73800
            List            =   "FrmMain.frx":029F
            Style           =   2  'Dropdown List
            TabIndex        =   57
            Top             =   720
            Width           =   2295
         End
         Begin VB.TextBox txtTimeOut 
            Alignment       =   1  'Right Justify
            Height          =   285
            Left            =   -71400
            TabIndex        =   56
            Text            =   "0"
            Top             =   720
            Width           =   705
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
            TabIndex        =   53
            Top             =   1080
            Width           =   4455
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
               TabIndex        =   55
               Top             =   360
               Width           =   1455
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
               Left            =   120
               TabIndex        =   54
               Top             =   360
               Value           =   -1  'True
               Width           =   1455
            End
         End
         Begin VB.Frame FraProxy 
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
            TabIndex        =   43
            Top             =   3960
            Width           =   4455
            Begin VB.TextBox txtProxySenha 
               Height          =   285
               IMEMode         =   3  'DISABLE
               Left            =   120
               PasswordChar    =   "*"
               TabIndex        =   47
               Top             =   1680
               Width           =   4215
            End
            Begin VB.TextBox txtProxyUsuario 
               Height          =   285
               Left            =   120
               TabIndex        =   46
               Top             =   1080
               Width           =   4215
            End
            Begin VB.TextBox txtProxyPorta 
               Alignment       =   1  'Right Justify
               Height          =   285
               Left            =   3120
               TabIndex        =   45
               Text            =   "0"
               Top             =   480
               Width           =   930
            End
            Begin VB.TextBox txtProxyServidor 
               Height          =   285
               Left            =   120
               TabIndex        =   44
               Top             =   480
               Width           =   2895
            End
            Begin MSComCtl2.UpDown nudProxyPorta 
               Height          =   285
               Left            =   4050
               TabIndex        =   48
               Top             =   480
               Width           =   255
               _ExtentX        =   450
               _ExtentY        =   503
               _Version        =   393216
               Value           =   5000
               BuddyControl    =   "txtProxyPorta"
               BuddyDispid     =   196681
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
               Index           =   2
               Left            =   120
               TabIndex        =   52
               Top             =   1440
               Width           =   525
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
               Index           =   1
               Left            =   120
               TabIndex        =   51
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
               Index           =   1
               Left            =   3120
               TabIndex        =   50
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
               TabIndex        =   49
               Top             =   240
               Width           =   720
            End
         End
         Begin VB.CheckBox chkExibirErro 
            Caption         =   "Exibir Erro Schema"
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
            TabIndex        =   42
            Top             =   480
            Width           =   2295
         End
         Begin VB.CommandButton cmdSchemas 
            Caption         =   "..."
            Height          =   315
            Left            =   4200
            TabIndex        =   38
            Top             =   4410
            Width           =   390
         End
         Begin VB.TextBox txtSchemaPath 
            Height          =   285
            Left            =   120
            TabIndex        =   37
            Top             =   4440
            Width           =   4095
         End
         Begin VB.CommandButton cmdLogs 
            Caption         =   "..."
            Height          =   315
            Left            =   4200
            TabIndex        =   35
            Top             =   3810
            Width           =   390
         End
         Begin VB.TextBox txtLogs 
            Height          =   285
            Left            =   120
            TabIndex        =   34
            Top             =   3840
            Width           =   4095
         End
         Begin VB.CheckBox chkSalvarEnvio 
            Caption         =   "Salvar Arquivos de Envio e Resposta"
            BeginProperty Font 
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
            Top             =   3240
            Width           =   4455
         End
         Begin VB.CheckBox chkRetirarAcentos 
            Caption         =   "Retirar Acentos dos XMLs enviados"
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
            TabIndex        =   31
            Top             =   2880
            Width           =   4095
         End
         Begin VB.ComboBox cmbVersaoDFe 
            Height          =   315
            ItemData        =   "FrmMain.frx":02EB
            Left            =   120
            List            =   "FrmMain.frx":02F5
            Style           =   2  'Dropdown List
            TabIndex        =   30
            Top             =   2400
            Width           =   4455
         End
         Begin VB.ComboBox cmbFormaEmissao 
            Height          =   315
            ItemData        =   "FrmMain.frx":0307
            Left            =   120
            List            =   "FrmMain.frx":0326
            Style           =   2  'Dropdown List
            TabIndex        =   28
            Top             =   1680
            Width           =   4455
         End
         Begin MSComCtl2.UpDown nudTimeOut 
            Height          =   285
            Left            =   -70665
            TabIndex        =   62
            Top             =   720
            Width           =   255
            _ExtentX        =   450
            _ExtentY        =   503
            _Version        =   393216
            Value           =   5000
            BuddyControl    =   "txtTimeOut"
            BuddyDispid     =   196674
            OrigLeft        =   3960
            OrigTop         =   720
            OrigRight       =   4215
            OrigBottom      =   975
            Max             =   99999
            SyncBuddy       =   -1  'True
            BuddyProperty   =   0
            Enabled         =   -1  'True
         End
         Begin VB.Label lblPastaArquivos 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "Pasta Arquivos GNRe"
            BeginProperty Font 
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
            TabIndex        =   87
            Top             =   2040
            Width           =   2250
         End
         Begin VB.Label lblCrytLib 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "CrytLib"
            BeginProperty Font 
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
            TabIndex        =   77
            Top             =   480
            Width           =   615
         End
         Begin VB.Label lblXmlSignLib 
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
            TabIndex        =   68
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
            Left            =   -74880
            TabIndex        =   67
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
            Left            =   120
            TabIndex        =   63
            Top             =   -240
            Width           =   705
         End
         Begin VB.Label lblUfDestino 
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
            TabIndex        =   61
            Top             =   480
            Width           =   870
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
            Left            =   -73800
            TabIndex        =   60
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
            Left            =   -71400
            TabIndex        =   59
            Top             =   480
            Width           =   765
         End
         Begin VB.Label lblCaminhoSchemas 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "Caminho Schemas"
            BeginProperty Font 
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
            TabIndex        =   36
            Top             =   4200
            Width           =   1545
         End
         Begin VB.Label lblCaminhoLogs 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "Caminho Logs"
            BeginProperty Font 
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
            Top             =   3600
            Width           =   1170
         End
         Begin VB.Label lblVersãoDocumento 
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
            TabIndex        =   29
            Top             =   2160
            Width           =   2115
         End
         Begin VB.Label lblFormaEmissão 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "Forma Emissão"
            BeginProperty Font 
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
            Top             =   1440
            Width           =   1275
         End
         Begin VB.Label lblFormatoAlerta 
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
            TabIndex        =   26
            Top             =   840
            Width           =   1290
         End
      End
      Begin VB.Label lblAssunto 
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
         TabIndex        =   25
         Top             =   4560
         Width           =   690
      End
      Begin VB.Label lblMensagem 
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
         TabIndex        =   24
         Top             =   5160
         Width           =   930
      End
   End
End
Attribute VB_Name = "FrmMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim gnre As ACBrGNRe

Public Function validacao() As Boolean

    If txtSchemaPath.Text = "" Then
    MsgBox ("Informe o Path com o Schemas")
    validacao = False
    
    ElseIf txtCertPath.Text = "" Then
    MsgBox ("Informe o Certificado")
    validacao = False
    
    ElseIf txtCertPassword.Text = "" Then
    MsgBox ("Informe a senha do Certificado")
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

Private Function CheckGNReLista(Optional ByVal xml As Boolean = False) As Boolean

    CheckGNReLista = False

    Dim answer As VbMsgBoxResult
  
    answer = MsgBox("Limpar lista ?", vbQuestion + vbYesNo, "Limpar lista")
      
    If answer = vbYes Then
        gnre.LimparLista
    End If
 
    CommonDialog1.InitDir = App.Path
    CommonDialog1.FileName = vbNullString
    
    If xml Then
        CommonDialog1.DialogTitle = "Selecione o arquivo xml"
        CommonDialog1.Filter = "Arquivo Xml GNRe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*"
    Else
        CommonDialog1.DialogTitle = "Selecione o arquivo ini"
        CommonDialog1.Filter = "Arquivo INI do GNRe (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*"
    End If
    
    CommonDialog1.ShowOpen
    
    If CommonDialog1.FileName = vbNullString Then Exit Function

    If xml Then
        gnre.CarregarXML CommonDialog1.FileName
    Else
        gnre.CarregarINI CommonDialog1.FileName
    End If
    
    CheckGNReLista = True
End Function

Private Sub btnCarregarConfiguracoes_Click()
    LoadConfig
End Sub

Private Sub btnCarregarIni_Click()
    On Error GoTo Erro:
    CheckGNReLista
Erro:
    MsgBox Err.Description
End Sub

Private Sub btnCarregarXml_Click()
    On Error GoTo Erro:
         
     CheckGNReLista
Erro:
    MsgBox Err.Description
End Sub

Private Sub btnEnviar_Click()
        
    If Not validacao Then
        MsgBox ("Verifique as configurações do certificado")
        Exit Sub
    End If
    
    On Error GoTo Erro:
    
    SetResposta gnre.Enviar
Erro:
    MsgBox Err.Description
    
End Sub

Private Sub btnImprimir_Click()

    On Error GoTo Erro:

    CommonDialog1.DialogTitle = "Selecione o arquivo xml"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivo Xml GNRe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
    
    If CommonDialog1.FileName = vbNullString Then Exit Sub
    
    gnre.LimparListaGuiaRetorno
    gnre.CarregarGuiaRetorno CommonDialog1.FileName
    gnre.Imprimir "", True
    
Erro:
    MsgBox Err.Description
    
End Sub

Private Sub btnImprimirPDF_Click()

    On Error GoTo Erro:

    CommonDialog1.DialogTitle = "Selecione o arquivo xml"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivo Xml GNRe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
    
    If CommonDialog1.FileName = vbNullString Then Exit Sub
    
    gnre.LimparListaGuiaRetorno
    gnre.CarregarGuiaRetorno CommonDialog1.FileName
    gnre.ImprimirPDF
    
Erro:
    MsgBox Err.Description
    
End Sub

Private Sub btnLimparLista_Click()

    On Error GoTo Erro:
    Dim answer As VbMsgBoxResult
  
    answer = MsgBox("Limpar lista ?", vbQuestion + vbYesNo, "Limpar lista")

    If answer = vbYes Then
        gnre.LimparLista
    End If

Erro:
    MsgBox Err.Description
End Sub

Private Sub btnObterCertificados_Click()
    
    On Error GoTo Erro:
    gnre.ObterCertificados
Erro:
    MsgBox Err.Description

End Sub

Private Sub cmdArquivos_Click()
    txtArquivos.Text = BrowseFolder("Selecione a pasta Arquivos GNRe")
End Sub

Private Sub cmdCertPath_Click()
    On Error Resume Next
    CommonDialog1.DialogTitle = "Selecione o certificado"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivos PFX (*.pfx)|*.pfx|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.ShowOpen
    
    If Err Then Exit Sub
    
    txtCertPath.Text = CommonDialog1.FileName
End Sub

Private Sub cmdConsultaConfiguração_Click()
    On Error GoTo Erro:

    Dim uf As String
    uf = InputBox("Digite a UF", "Envio email", vbNullString)

    Dim receita As String
    uf = InputBox("Digite o número da Receita", vbNullString)

    If uf = vbNullString Then Exit Sub
    If receita = vbNullString Then Exit Sub

    SetResposta gnre.Consultar(uf, CLng(receita))
Erro:
    MsgBox Err.Description
End Sub

Private Sub cmdDadosPFX_Click()
    On Error Resume Next
    CommonDialog1.DialogTitle = "Selecione o certificado"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivos PFX (*.pfx)|*.pfx|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.ShowOpen
    
    If Err Then Exit Sub
    
    txtDadosPFX.Text = CommonDialog1.FileName

End Sub

Private Sub cmdEnviarEmail_Click()
    
    If Not validacaoEmail Then
        MsgBox ("Verifique as configurações de E-mail")
        Exit Sub
    End If
    
    On Error GoTo Erro:

    If Not CheckGNReLista(True) Then Exit Sub

    Dim destinatario As String
    destinatario = InputBox("Digite o email do destinatario", "Envio email", vbNullString)

    If destinatario = vbNullString Then Exit Sub
    gnre.EnviarEmail destinatario, CommonDialog1.FileName, True, txtAssunto.Text, txtMensagem.Text
Erro:
    MsgBox Err.Description
End Sub

Private Sub cmdGerarGNRe_Click()
    On Error GoTo Erro:

    If Not CheckGNReLista(True) Then Exit Sub
    
    gnre.Assinar
    ret = gnre.ObterXml(0)
    rtbRespostas.Text = ret
Erro:
    MsgBox Err.Description
    Exit Sub
End Sub

Private Sub cmdLogs_Click()
    txtLogs.Text = BrowseFolder("Selecione a pasta dos Logs")
End Sub

Private Sub cmdSalvar_Click()
    SaveConfig
End Sub

Private Sub cmdSchemas_Click()
    txtSchemaPath.Text = BrowseFolder("Selecione a pasta dos Schemas")
End Sub

Private Sub Form_Load()
    cmbFormaEmissao.ListIndex = 0
    cmbVersaoDFe.ListIndex = 0
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

    Set gnre = CreateGNRe(IniPath)

    gnre.ConfigGravarValor SESSAO_PRINCIPAL, "LogNivel", NivelLog.logParanoico
    gnre.ConfigGravarValor SESSAO_PRINCIPAL, "LogPath", LogPath
    gnre.ConfigGravar

    LoadConfig
End Sub

Private Sub LoadConfig()
    gnre.ConfigLer

    'Geral
    chkExibirErro.Value = CLng(gnre.ConfigLerValor(SESSAO_GNRE, "ExibirErroSchema"))
    txtFormatoAlerta.Text = gnre.ConfigLerValor(SESSAO_GNRE, "FormatoAlerta")
    cmbFormaEmissao.ListIndex = CLng(gnre.ConfigLerValor(SESSAO_GNRE, "FormaEmissao"))
    cmbVersaoDFe.ListIndex = CLng(gnre.ConfigLerValor(SESSAO_GNRE, "VersaoDF"))
    chkRetirarAcentos.Value = CLng(gnre.ConfigLerValor(SESSAO_GNRE, "RetirarAcentos"))
    chkSalvarEnvio.Value = CLng(gnre.ConfigLerValor(SESSAO_GNRE, "SalvarGer"))
    txtLogs.Text = gnre.ConfigLerValor(SESSAO_GNRE, "PathSalvar")
    txtSchemaPath.Text = gnre.ConfigLerValor(SESSAO_GNRE, "PathSchemas")

    'Webservice
    cmbUfDestino.Text = gnre.ConfigLerValor(SESSAO_DFE, "UF")
    cmbSSlType.ListIndex = CLng(gnre.ConfigLerValor(SESSAO_GNRE, "SSLType"))
    nudTimeOut.Value = CLng(gnre.ConfigLerValor(SESSAO_GNRE, "Timeout"))

    Dim ambiente As String
    ambiente = gnre.ConfigLerValor(SESSAO_GNRE, "Ambiente")

    rdbHomologacao.Value = CBool(ambiente)
    rdbProducao.Value = Not CBool(ambiente)

    ckbVisualizar.Value = CLng(gnre.ConfigLerValor(SESSAO_GNRE, "Visualizar"))
    ckbSalvarSOAP.Value = CLng(gnre.ConfigLerValor(SESSAO_GNRE, "SalvarWS"))
    ckbAjustarAut.Value = CLng(gnre.ConfigLerValor(SESSAO_GNRE, "AjustaAguardaConsultaRet"))
    nudAguardar.Value = CLng(gnre.ConfigLerValor(SESSAO_GNRE, "AguardarConsultaRet"))
    nudTentativas.Value = CLng(gnre.ConfigLerValor(SESSAO_GNRE, "Tentativas"))
    nudIntervalo.Value = CLng(gnre.ConfigLerValor(SESSAO_GNRE, "IntervaloTentativas"))

    'Proxy
    txtProxyServidor.Text = gnre.ConfigLerValor(SESSAO_PROXY, "Servidor")

    Dim porta As String
    porta = gnre.ConfigLerValor(SESSAO_PROXY, "Porta")

    If IsNumeric(porta) Then
        nudProxyPorta.Value = CLng(porta)
    End If

    txtProxyUsuario.Text = gnre.ConfigLerValor(SESSAO_PROXY, "Usuario")
    txtProxySenha.Text = gnre.ConfigLerValor(SESSAO_PROXY, "Senha")

    'Certificado
    cmbCrypt.ListIndex = CLng(gnre.ConfigLerValor(SESSAO_DFE, "SSLCryptLib"))
    cmbHttp.ListIndex = CLng(gnre.ConfigLerValor(SESSAO_DFE, "SSLHttpLib"))
    cmbXmlSign.ListIndex = CLng(gnre.ConfigLerValor(SESSAO_DFE, "SSLXmlSignLib"))
    txtCertPath.Text = gnre.ConfigLerValor(SESSAO_DFE, "ArquivoPFX")
    txtDadosPFX.Text = gnre.ConfigLerValor(SESSAO_DFE, "DadosPFX")
    txtCertPassword.Text = gnre.ConfigLerValor(SESSAO_DFE, "Senha")
    txtCertNumero.Text = gnre.ConfigLerValor(SESSAO_DFE, "NumeroSerie")

    'Arquivos
    chkSalvarPasta.Value = CLng(gnre.ConfigLerValor(SESSAO_GNRE, "SalvarArq"))
    chkSalvarMensal.Value = CLng(gnre.ConfigLerValor(SESSAO_GNRE, "SepararPorMes"))
    chkAdicionarLiteral.Value = CLng(gnre.ConfigLerValor(SESSAO_GNRE, "AdicionarLiteral"))
    chkEmissaoPath.Value = CLng(gnre.ConfigLerValor(SESSAO_GNRE, "EmissaoPathGNRE"))
    chkSepararCNPJ.Value = CLng(gnre.ConfigLerValor(SESSAO_GNRE, "SepararPorCNPJ"))
    chkSepararDoc.Value = CLng(gnre.ConfigLerValor(SESSAO_GNRE, "SepararPorModelo"))
    txtArquivos.Text = gnre.ConfigLerValor(SESSAO_GNRE, "PathGNRE")


    'Email
    txtEmailNome.Text = gnre.ConfigLerValor(SESSAO_EMAIL, "Nome")
    txtEmail.Text = gnre.ConfigLerValor(SESSAO_EMAIL, "Conta")
    txtEmailUsuario.Text = gnre.ConfigLerValor(SESSAO_EMAIL, "Usuario")
    txtEmailSenha.Text = gnre.ConfigLerValor(SESSAO_EMAIL, "Senha")
    txtEmailHost.Text = gnre.ConfigLerValor(SESSAO_EMAIL, "Servidor")
    nudEmailPorta.Value = CLng(gnre.ConfigLerValor(SESSAO_EMAIL, "Porta"))
    chkEmailSSL.Value = CLng(gnre.ConfigLerValor(SESSAO_EMAIL, "SSL"))
    chkEmailTLS.Value = CLng(gnre.ConfigLerValor(SESSAO_EMAIL, "TLS"))

End Sub

Private Sub SaveConfig()

    'Geral
    gnre.ConfigGravarValor SESSAO_GNRE, "ExibirErroSchema", CStr(chkExibirErro.Value)
    gnre.ConfigGravarValor SESSAO_GNRE, "FormatoAlerta", txtFormatoAlerta.Text
    gnre.ConfigGravarValor SESSAO_GNRE, "FormaEmissao", CStr(cmbFormaEmissao.ListIndex)
    gnre.ConfigGravarValor SESSAO_GNRE, "VersaoDF", CStr(cmbVersaoDFe.ListIndex)
    gnre.ConfigGravarValor SESSAO_GNRE, "RetirarAcentos", CStr(chkRetirarAcentos.Value)
    gnre.ConfigGravarValor SESSAO_GNRE, "SalvarGer", CStr(chkSalvarEnvio.Value)
    gnre.ConfigGravarValor SESSAO_GNRE, "PathSalvar", txtLogs.Text
    gnre.ConfigGravarValor SESSAO_GNRE, "PathSchemas", txtSchemaPath.Text

    'Webservice
    gnre.ConfigGravarValor SESSAO_DFE, "UF", cmbUfDestino.Text
    gnre.ConfigGravarValor SESSAO_GNRE, "SSLType", CStr(cmbSSlType.ListIndex)
    gnre.ConfigGravarValor SESSAO_GNRE, "Timeout", CStr(nudTimeOut.Value)
    gnre.ConfigGravarValor SESSAO_GNRE, "Ambiente", CStr(rdbHomologacao.Value)
    gnre.ConfigGravarValor SESSAO_GNRE, "Visualizar", CStr(ckbVisualizar.Value)
    gnre.ConfigGravarValor SESSAO_GNRE, "SalvarWS", CStr(ckbSalvarSOAP.Value)
    gnre.ConfigGravarValor SESSAO_GNRE, "AjustaAguardaConsultaRet", CStr(ckbAjustarAut.Value)
    gnre.ConfigGravarValor SESSAO_GNRE, "AguardarConsultaRet", CStr(nudAguardar.Value)
    gnre.ConfigGravarValor SESSAO_GNRE, "Tentativas", CStr(nudTentativas.Value)
    gnre.ConfigGravarValor SESSAO_GNRE, "IntervaloTentativas", CStr(nudIntervalo.Value)

    'Proxy
    gnre.ConfigGravarValor SESSAO_PROXY, "Servidor", txtProxyServidor.Text
    gnre.ConfigGravarValor SESSAO_PROXY, "Porta", CStr(nudProxyPorta.Value)
    gnre.ConfigGravarValor SESSAO_PROXY, "Usuario", txtProxyUsuario.Text
    gnre.ConfigGravarValor SESSAO_PROXY, "Senha", txtProxySenha.Text

    'Certificado
    gnre.ConfigGravarValor SESSAO_DFE, "SSLCryptLib", CStr(cmbCrypt.ListIndex)
    gnre.ConfigGravarValor SESSAO_DFE, "SSLHttpLib", CStr(cmbHttp.ListIndex)
    gnre.ConfigGravarValor SESSAO_DFE, "SSLXmlSignLib", CStr(cmbXmlSign.ListIndex)
    gnre.ConfigGravarValor SESSAO_DFE, "ArquivoPFX", txtCertPath.Text
    gnre.ConfigGravarValor SESSAO_DFE, "DadosPFX", txtDadosPFX.Text
    gnre.ConfigGravarValor SESSAO_DFE, "Senha", txtCertPassword.Text
    gnre.ConfigGravarValor SESSAO_DFE, "NumeroSerie", txtCertNumero.Text

    'Arquivos
    gnre.ConfigGravarValor SESSAO_GNRE, "SalvarArq", CStr(chkSalvarPasta.Value)
    gnre.ConfigGravarValor SESSAO_GNRE, "SepararPorMes", CStr(chkSalvarMensal.Value)
    gnre.ConfigGravarValor SESSAO_GNRE, "AdicionarLiteral", CStr(chkAdicionarLiteral.Value)
    gnre.ConfigGravarValor SESSAO_GNRE, "EmissaoPathGNRE", CStr(chkEmissaoPath.Value)
    gnre.ConfigGravarValor SESSAO_GNRE, "SepararPorCNPJ", CStr(chkSepararCNPJ.Value)
    gnre.ConfigGravarValor SESSAO_GNRE, "SepararPorModelo", CStr(chkSepararDoc.Value)
    gnre.ConfigGravarValor SESSAO_GNRE, "PathGNRE", txtArquivos.Text

    'Email
    gnre.ConfigGravarValor SESSAO_EMAIL, "Nome", txtEmailNome.Text
    gnre.ConfigGravarValor SESSAO_EMAIL, "Conta", txtEmail.Text
    gnre.ConfigGravarValor SESSAO_EMAIL, "Usuario", txtEmailUsuario.Text
    gnre.ConfigGravarValor SESSAO_EMAIL, "Senha", txtEmailSenha.Text
    gnre.ConfigGravarValor SESSAO_EMAIL, "Servidor", txtEmailHost.Text
    gnre.ConfigGravarValor SESSAO_EMAIL, "Porta", CStr(nudEmailPorta.Value)
    gnre.ConfigGravarValor SESSAO_EMAIL, "SSL", CStr(chkEmailSSL.Value)
    gnre.ConfigGravarValor SESSAO_EMAIL, "TLS", CStr(chkEmailTLS.Value)

    gnre.ConfigGravar

End Sub

Private Sub SetResposta(ByRef resposta As String)

    If rtbRespostas.Text <> vbNullString Then
        rtbRespostas.Text = rtbRespostas.Text + vbCrLf + resposta
    Else
        rtbRespostas.Text = resposta
    End If

End Sub
