VERSION 5.00
Object = "{86CF1D34-0C5F-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCT2.OCX"
Object = "{BDC217C8-ED16-11CD-956C-0000C04E4C0A}#1.1#0"; "TABCTL32.OCX"
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "COMDLG32.OCX"
Begin VB.Form FrmMain 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "ACBrLibNFe Demo"
   ClientHeight    =   7800
   ClientLeft      =   45
   ClientTop       =   390
   ClientWidth     =   11220
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
   ScaleWidth      =   11220
   StartUpPosition =   2  'CenterScreen
   Begin MSComDlg.CommonDialog CommonDialog1 
      Left            =   4080
      Top             =   7080
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
   End
   Begin VB.Frame FraRespostas 
      Caption         =   "Respostas"
      Height          =   3735
      Left            =   4800
      TabIndex        =   16
      Top             =   3840
      Width           =   6255
      Begin VB.TextBox rtbRespostas 
         Height          =   3375
         Left            =   120
         Locked          =   -1  'True
         MultiLine       =   -1  'True
         ScrollBars      =   3  'Both
         TabIndex        =   17
         Top             =   240
         Width           =   6015
      End
   End
   Begin VB.Frame FraComandos 
      Caption         =   "Comandos"
      Height          =   3615
      Left            =   4800
      TabIndex        =   15
      Top             =   120
      Width           =   6255
      Begin VB.CommandButton btnInutilizar 
         Caption         =   "Inutilizar Numeração"
         Height          =   360
         Left            =   2160
         TabIndex        =   76
         Top             =   1320
         Width           =   1935
      End
      Begin VB.CommandButton btnEnviarEmail 
         Caption         =   "Enviar NFe Email"
         Height          =   360
         Left            =   4200
         TabIndex        =   75
         Top             =   1320
         Width           =   1935
      End
      Begin VB.CommandButton btnCancelarNFe 
         Caption         =   "Cancelar NFe"
         Height          =   360
         Left            =   120
         TabIndex        =   74
         Top             =   1320
         Width           =   1935
      End
      Begin VB.CommandButton btnConsultaChave 
         Caption         =   "Consultar com Chave"
         Height          =   360
         Left            =   2160
         TabIndex        =   73
         Top             =   840
         Width           =   1935
      End
      Begin VB.CommandButton btnConsultarRecibo 
         Caption         =   "Consultar Recibo"
         Height          =   360
         Left            =   4200
         TabIndex        =   72
         Top             =   840
         Width           =   1935
      End
      Begin VB.CommandButton btnConsultaXml 
         Caption         =   "Consultar com Xml"
         Height          =   360
         Left            =   120
         TabIndex        =   71
         Top             =   840
         Width           =   1935
      End
      Begin VB.CommandButton btnEnviar 
         Caption         =   "Criar e Enviar"
         Height          =   360
         Left            =   2160
         TabIndex        =   70
         Top             =   360
         Width           =   1935
      End
      Begin VB.CommandButton btnImprimir 
         Caption         =   "Imprimir DANFe"
         Height          =   360
         Left            =   4200
         TabIndex        =   69
         Top             =   360
         Width           =   1935
      End
      Begin VB.CommandButton btnStatusServ 
         Caption         =   "Status de Serviço"
         Height          =   360
         Left            =   120
         TabIndex        =   68
         Top             =   360
         Width           =   1935
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
      Left            =   1200
      TabIndex        =   14
      Top             =   7080
      Width           =   2295
   End
   Begin TabDlg.SSTab SSTTab0 
      Height          =   6735
      Left            =   120
      TabIndex        =   0
      Top             =   120
      Width           =   4455
      _ExtentX        =   7858
      _ExtentY        =   11880
      _Version        =   393216
      Style           =   1
      Tabs            =   4
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
      TabPicture(0)   =   "FrmMain.frx":0000
      Tab(0).ControlEnabled=   -1  'True
      Tab(0).Control(0)=   "lblModeloDocumento"
      Tab(0).Control(0).Enabled=   0   'False
      Tab(0).Control(1)=   "lblIdCSC"
      Tab(0).Control(1).Enabled=   0   'False
      Tab(0).Control(2)=   "Label1"
      Tab(0).Control(2).Enabled=   0   'False
      Tab(0).Control(3)=   "Label2"
      Tab(0).Control(3).Enabled=   0   'False
      Tab(0).Control(4)=   "cmbModeloDocumento"
      Tab(0).Control(4).Enabled=   0   'False
      Tab(0).Control(5)=   "txtIdCSC"
      Tab(0).Control(5).Enabled=   0   'False
      Tab(0).Control(6)=   "txtCSC"
      Tab(0).Control(6).Enabled=   0   'False
      Tab(0).Control(7)=   "txtSchemaPath"
      Tab(0).Control(7).Enabled=   0   'False
      Tab(0).Control(8)=   "btnSelectSchema"
      Tab(0).Control(8).Enabled=   0   'False
      Tab(0).ControlCount=   9
      TabCaption(1)   =   "Webservice"
      TabPicture(1)   =   "FrmMain.frx":001C
      Tab(1).ControlEnabled=   0   'False
      Tab(1).Control(0)=   "Label3"
      Tab(1).Control(1)=   "Label4"
      Tab(1).Control(2)=   "lblTimeOut"
      Tab(1).Control(3)=   "cmbUfDestino"
      Tab(1).Control(4)=   "cmbSSlType"
      Tab(1).Control(5)=   "txtTimeOut"
      Tab(1).Control(6)=   "nudTimeOut"
      Tab(1).Control(7)=   "FraAmbiente"
      Tab(1).Control(8)=   "FraProxy"
      Tab(1).ControlCount=   9
      TabCaption(2)   =   "Certificados"
      TabPicture(2)   =   "FrmMain.frx":0038
      Tab(2).ControlEnabled=   0   'False
      Tab(2).Control(0)=   "Label8"
      Tab(2).Control(1)=   "Label9"
      Tab(2).Control(2)=   "Label10"
      Tab(2).Control(3)=   "cmbCrypt"
      Tab(2).Control(4)=   "cmbHttp"
      Tab(2).Control(5)=   "cmbXmlSign"
      Tab(2).Control(6)=   "FraCertificados"
      Tab(2).ControlCount=   7
      TabCaption(3)   =   "Email"
      TabPicture(3)   =   "FrmMain.frx":0054
      Tab(3).ControlEnabled=   0   'False
      Tab(3).Control(0)=   "Label20"
      Tab(3).Control(1)=   "Label21"
      Tab(3).Control(2)=   "FraConfigurações"
      Tab(3).Control(3)=   "txtAssunto"
      Tab(3).Control(4)=   "txtMensagem"
      Tab(3).ControlCount=   5
      Begin VB.TextBox txtMensagem 
         Height          =   1365
         IMEMode         =   3  'DISABLE
         Left            =   -74880
         MultiLine       =   -1  'True
         PasswordChar    =   "*"
         ScrollBars      =   3  'Both
         TabIndex        =   66
         Top             =   5280
         Width           =   4155
      End
      Begin VB.TextBox txtAssunto 
         Height          =   285
         IMEMode         =   3  'DISABLE
         Left            =   -74880
         PasswordChar    =   "*"
         TabIndex        =   64
         Top             =   4680
         Width           =   4155
      End
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
         Height          =   3855
         Left            =   -74880
         TabIndex        =   48
         Top             =   480
         Width           =   4215
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
            TabIndex        =   63
            Top             =   2400
            Width           =   1335
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
            TabIndex        =   62
            Top             =   2160
            Width           =   1335
         End
         Begin VB.TextBox txtPorta 
            Alignment       =   1  'Right Justify
            Height          =   285
            Left            =   120
            TabIndex        =   60
            Text            =   "5000"
            Top             =   2280
            Width           =   720
         End
         Begin VB.TextBox txtSenha 
            Height          =   285
            IMEMode         =   3  'DISABLE
            Left            =   120
            PasswordChar    =   "*"
            TabIndex        =   57
            Top             =   3480
            Width           =   3915
         End
         Begin VB.TextBox txtUsuario 
            Height          =   285
            IMEMode         =   3  'DISABLE
            Left            =   120
            TabIndex        =   55
            Top             =   2880
            Width           =   3915
         End
         Begin VB.TextBox txtHost 
            Height          =   285
            IMEMode         =   3  'DISABLE
            Left            =   120
            TabIndex        =   53
            Top             =   1680
            Width           =   3915
         End
         Begin VB.TextBox txtEmail 
            Height          =   285
            IMEMode         =   3  'DISABLE
            Left            =   120
            TabIndex        =   51
            Top             =   1080
            Width           =   3915
         End
         Begin VB.TextBox txtNome 
            Height          =   285
            IMEMode         =   3  'DISABLE
            Left            =   120
            TabIndex        =   49
            Top             =   480
            Width           =   3915
         End
         Begin MSComCtl2.UpDown nudPorta 
            Height          =   285
            Left            =   841
            TabIndex        =   59
            Top             =   2280
            Width           =   255
            _ExtentX        =   450
            _ExtentY        =   503
            _Version        =   393216
            Value           =   5000
            BuddyControl    =   "txtPorta"
            BuddyDispid     =   196627
            OrigLeft        =   3960
            OrigTop         =   720
            OrigRight       =   4215
            OrigBottom      =   975
            Max             =   99999
            SyncBuddy       =   -1  'True
            BuddyProperty   =   0
            Enabled         =   -1  'True
         End
         Begin VB.Label Label19 
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
            TabIndex        =   61
            Top             =   2040
            Width           =   465
         End
         Begin VB.Label Label18 
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
            TabIndex        =   58
            Top             =   3240
            Width           =   525
         End
         Begin VB.Label Label17 
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
            TabIndex        =   56
            Top             =   2640
            Width           =   645
         End
         Begin VB.Label Label16 
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
            TabIndex        =   54
            Top             =   1440
            Width           =   900
         End
         Begin VB.Label Label15 
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
            TabIndex        =   52
            Top             =   840
            Width           =   450
         End
         Begin VB.Label Label14 
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
            TabIndex        =   50
            Top             =   240
            Width           =   480
         End
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
         Height          =   2055
         Left            =   -74880
         TabIndex        =   40
         Top             =   2400
         Width           =   4215
         Begin VB.TextBox txtCertNumero 
            Height          =   285
            Left            =   120
            TabIndex        =   46
            Top             =   1680
            Width           =   4035
         End
         Begin VB.TextBox txtCertPassword 
            Height          =   285
            IMEMode         =   3  'DISABLE
            Left            =   120
            PasswordChar    =   "*"
            TabIndex        =   44
            Top             =   1080
            Width           =   4035
         End
         Begin VB.TextBox txtCertPath 
            Height          =   285
            Left            =   120
            TabIndex        =   42
            Top             =   480
            Width           =   3555
         End
         Begin VB.CommandButton btnSelecionarCertificado 
            Caption         =   "..."
            Height          =   260
            Left            =   3720
            TabIndex        =   41
            Top             =   479
            Width           =   390
         End
         Begin VB.Label Label13 
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
            TabIndex        =   47
            Top             =   1440
            Width           =   1395
         End
         Begin VB.Label Label12 
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
            TabIndex        =   45
            Top             =   840
            Width           =   525
         End
         Begin VB.Label Label11 
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
            TabIndex        =   43
            Top             =   240
            Width           =   735
         End
      End
      Begin VB.ComboBox cmbXmlSign 
         Height          =   315
         ItemData        =   "FrmMain.frx":0070
         Left            =   -74880
         List            =   "FrmMain.frx":0083
         Style           =   2  'Dropdown List
         TabIndex        =   38
         Top             =   1920
         Width           =   2175
      End
      Begin VB.ComboBox cmbHttp 
         Height          =   315
         ItemData        =   "FrmMain.frx":00BD
         Left            =   -74880
         List            =   "FrmMain.frx":00D0
         Style           =   2  'Dropdown List
         TabIndex        =   36
         Top             =   1320
         Width           =   2175
      End
      Begin VB.ComboBox cmbCrypt 
         Height          =   315
         ItemData        =   "FrmMain.frx":010F
         Left            =   -74880
         List            =   "FrmMain.frx":011F
         Style           =   2  'Dropdown List
         TabIndex        =   34
         Top             =   720
         Width           =   2175
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
         Height          =   2055
         Left            =   -74880
         TabIndex        =   24
         Top             =   1920
         Width           =   4215
         Begin VB.TextBox txtProxySenha 
            Height          =   285
            IMEMode         =   3  'DISABLE
            Left            =   120
            PasswordChar    =   "*"
            TabIndex        =   33
            Top             =   1680
            Width           =   3975
         End
         Begin VB.TextBox txtProxyUsuario 
            Height          =   285
            Left            =   120
            TabIndex        =   31
            Top             =   1080
            Width           =   3975
         End
         Begin VB.TextBox txtProxyPorta 
            Alignment       =   1  'Right Justify
            Height          =   285
            Left            =   2880
            TabIndex        =   28
            Text            =   "5000"
            Top             =   480
            Width           =   945
         End
         Begin VB.TextBox txtProxyServidor 
            Height          =   285
            Left            =   120
            TabIndex        =   25
            Top             =   480
            Width           =   2655
         End
         Begin MSComCtl2.UpDown nudProxyPorta 
            Height          =   285
            Left            =   3825
            TabIndex        =   27
            Top             =   480
            Width           =   255
            _ExtentX        =   450
            _ExtentY        =   503
            _Version        =   393216
            Value           =   5000
            BuddyControl    =   "txtProxyPorta"
            BuddyDispid     =   196653
            OrigLeft        =   3960
            OrigTop         =   720
            OrigRight       =   4215
            OrigBottom      =   975
            Max             =   99999
            SyncBuddy       =   -1  'True
            BuddyProperty   =   65547
            Enabled         =   -1  'True
         End
         Begin VB.Label Label7 
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
            TabIndex        =   32
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
            Left            =   120
            TabIndex        =   30
            Top             =   840
            Width           =   720
         End
         Begin VB.Label Label6 
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
            TabIndex        =   29
            Top             =   240
            Width           =   465
         End
         Begin VB.Label Label5 
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
            TabIndex        =   26
            Top             =   240
            Width           =   720
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
         TabIndex        =   21
         Top             =   1080
         Width           =   4215
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
            TabIndex        =   23
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
            Left            =   120
            TabIndex        =   22
            Top             =   360
            Value           =   -1  'True
            Width           =   1455
         End
      End
      Begin MSComCtl2.UpDown nudTimeOut 
         Height          =   285
         Left            =   -71040
         TabIndex        =   19
         Top             =   720
         Width           =   255
         _ExtentX        =   450
         _ExtentY        =   503
         _Version        =   393216
         Value           =   5000
         AutoBuddy       =   -1  'True
         BuddyControl    =   "txtTimeOut"
         BuddyDispid     =   196662
         OrigLeft        =   3960
         OrigTop         =   720
         OrigRight       =   4215
         OrigBottom      =   975
         Max             =   99999
         SyncBuddy       =   -1  'True
         BuddyProperty   =   65547
         Enabled         =   -1  'True
      End
      Begin VB.TextBox txtTimeOut 
         Alignment       =   1  'Right Justify
         Height          =   285
         Left            =   -72000
         TabIndex        =   18
         Text            =   "5000"
         Top             =   720
         Width           =   990
      End
      Begin VB.ComboBox cmbSSlType 
         Height          =   315
         ItemData        =   "FrmMain.frx":0151
         Left            =   -73800
         List            =   "FrmMain.frx":016A
         Style           =   2  'Dropdown List
         TabIndex        =   12
         Top             =   720
         Width           =   1695
      End
      Begin VB.ComboBox cmbUfDestino 
         Height          =   315
         ItemData        =   "FrmMain.frx":01B6
         Left            =   -74880
         List            =   "FrmMain.frx":020B
         Style           =   2  'Dropdown List
         TabIndex        =   10
         Top             =   720
         Width           =   975
      End
      Begin VB.CommandButton btnSelectSchema 
         Caption         =   "..."
         Height          =   260
         Left            =   3960
         TabIndex        =   9
         Top             =   2520
         Width           =   390
      End
      Begin VB.TextBox txtSchemaPath 
         Height          =   285
         Left            =   120
         TabIndex        =   8
         Top             =   2520
         Width           =   3795
      End
      Begin VB.TextBox txtCSC 
         Height          =   285
         Left            =   120
         TabIndex        =   6
         Top             =   1920
         Width           =   4215
      End
      Begin VB.TextBox txtIdCSC 
         Height          =   285
         Left            =   120
         TabIndex        =   4
         Top             =   1320
         Width           =   4215
      End
      Begin VB.ComboBox cmbModeloDocumento 
         Height          =   315
         ItemData        =   "FrmMain.frx":027B
         Left            =   120
         List            =   "FrmMain.frx":0285
         Style           =   2  'Dropdown List
         TabIndex        =   2
         Top             =   720
         Width           =   2175
      End
      Begin VB.Label Label21 
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
         TabIndex        =   67
         Top             =   5040
         Width           =   930
      End
      Begin VB.Label Label20 
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
         TabIndex        =   65
         Top             =   4440
         Width           =   690
      End
      Begin VB.Label Label10 
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
         TabIndex        =   39
         Top             =   1680
         Width           =   915
      End
      Begin VB.Label Label9 
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
         TabIndex        =   37
         Top             =   1080
         Width           =   615
      End
      Begin VB.Label Label8 
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
         TabIndex        =   35
         Top             =   480
         Width           =   705
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
         Left            =   -72000
         TabIndex        =   20
         Top             =   480
         Width           =   765
      End
      Begin VB.Label Label4 
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
         TabIndex        =   13
         Top             =   480
         Width           =   765
      End
      Begin VB.Label Label3 
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
         TabIndex        =   11
         Top             =   480
         Width           =   870
      End
      Begin VB.Label Label2 
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
         TabIndex        =   7
         Top             =   2280
         Width           =   1635
      End
      Begin VB.Label Label1 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "CSC"
         BeginProperty Font 
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
         TabIndex        =   5
         Top             =   1680
         Width           =   315
      End
      Begin VB.Label lblIdCSC 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "IdCSC"
         BeginProperty Font 
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
         TabIndex        =   3
         Top             =   1080
         Width           =   2145
      End
      Begin VB.Label lblModeloDocumento 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
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
         Height          =   195
         Left            =   120
         TabIndex        =   1
         Top             =   480
         Width           =   2145
      End
   End
End
Attribute VB_Name = "FrmMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim nfe As ACBrNFe

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

Private Sub btnConsultaChave_Click()

    On Error GoTo Erro:
    Dim chaveOuNFe As String
    
    chaveOuNFe = InputBox("Chave da NF-e:", "WebServices Consultar: Nota", "")
    SetResposta nfe.Consultar(chaveOuNFe)
    
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
    Exit Sub
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
    
    On Error Resume Next
    
    Dim retorno As Long
    
    SetResposta nfe.StatusServico
    If Err.Number <> 0 Then
       MsgBox Err.Description
       Exit Sub
    End If
    On Error GoTo 0

End Sub

Private Sub cmdSalvar_Click()
    SalvarConfig
End Sub

Private Sub Form_Load()
    cmbModeloDocumento.ListIndex = 0
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
    Dim retorno As Long
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
    cmbUfDestino.Text = nfe.ConfigLerValor(SESSAO_DFE, "UF")
    
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
    nudPorta.Value = CLng(nfe.ConfigLerValor(SESSAO_EMAIL, "Porta"))
    ckbSSL.Value = CLng(nfe.ConfigLerValor(SESSAO_EMAIL, "SSL"))
    ckbTLS.Value = CLng(nfe.ConfigLerValor(SESSAO_EMAIL, "TLS"))
    
End Sub

Private Sub SalvarConfig()
    Dim retorno As Long
    
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
    nfe.ConfigGravarValor SESSAO_DFE, "UF", cmbUfDestino.Text
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

