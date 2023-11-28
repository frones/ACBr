VERSION 5.00
Object = "{86CF1D34-0C5F-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCT2.OCX"
Object = "{BDC217C8-ED16-11CD-956C-0000C04E4C0A}#1.1#0"; "TABCTL32.OCX"
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "COMDLG32.OCX"
Begin VB.Form FrmMain 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "ACBrLibCTe Demo"
   ClientHeight    =   7935
   ClientLeft      =   45
   ClientTop       =   390
   ClientWidth     =   11760
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
   ScaleHeight     =   7935
   ScaleWidth      =   11760
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
      TabIndex        =   122
      Top             =   7320
      Width           =   2295
   End
   Begin TabDlg.SSTab SSTab1 
      Height          =   7095
      Left            =   120
      TabIndex        =   4
      Top             =   120
      Width           =   5055
      _ExtentX        =   8916
      _ExtentY        =   12515
      _Version        =   393216
      Style           =   1
      TabHeight       =   520
      TabCaption(0)   =   "Configurações"
      TabPicture(0)   =   "FrmMain.frx":0000
      Tab(0).ControlEnabled=   -1  'True
      Tab(0).Control(0)=   "SSTab2"
      Tab(0).Control(0).Enabled=   0   'False
      Tab(0).ControlCount=   1
      TabCaption(1)   =   "Documento Auxiliar"
      TabPicture(1)   =   "FrmMain.frx":001C
      Tab(1).ControlEnabled=   0   'False
      Tab(1).Control(0)=   "Label13"
      Tab(1).Control(1)=   "txtLogomarca"
      Tab(1).Control(2)=   "btnLogomarca"
      Tab(1).Control(3)=   "grpBoxDACTe"
      Tab(1).ControlCount=   4
      TabCaption(2)   =   "Email"
      TabPicture(2)   =   "FrmMain.frx":0038
      Tab(2).ControlEnabled=   0   'False
      Tab(2).Control(0)=   "txtMensagemEmail"
      Tab(2).Control(1)=   "txtAssuntoEmail"
      Tab(2).Control(2)=   "grpBoxConfigurações"
      Tab(2).Control(3)=   "Label30"
      Tab(2).Control(4)=   "Label29"
      Tab(2).ControlCount=   5
      Begin VB.TextBox txtMensagemEmail 
         Height          =   1365
         IMEMode         =   3  'DISABLE
         Left            =   -74760
         MultiLine       =   -1  'True
         PasswordChar    =   "*"
         ScrollBars      =   3  'Both
         TabIndex        =   92
         Top             =   5400
         Width           =   4155
      End
      Begin VB.TextBox txtAssuntoEmail 
         Height          =   285
         IMEMode         =   3  'DISABLE
         Left            =   -74760
         PasswordChar    =   "*"
         TabIndex        =   90
         Top             =   4680
         Width           =   4155
      End
      Begin VB.Frame grpBoxConfigurações 
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
         Left            =   -74760
         TabIndex        =   73
         Top             =   480
         Width           =   4215
         Begin VB.TextBox txtNome 
            Height          =   285
            IMEMode         =   3  'DISABLE
            Left            =   120
            TabIndex        =   81
            Top             =   480
            Width           =   3915
         End
         Begin VB.TextBox txtEmail 
            Height          =   285
            IMEMode         =   3  'DISABLE
            Left            =   120
            TabIndex        =   80
            Top             =   1080
            Width           =   3915
         End
         Begin VB.TextBox txtHost 
            Height          =   285
            IMEMode         =   3  'DISABLE
            Left            =   120
            TabIndex        =   79
            Top             =   1680
            Width           =   3915
         End
         Begin VB.TextBox txtUsuario 
            Height          =   285
            IMEMode         =   3  'DISABLE
            Left            =   120
            TabIndex        =   78
            Top             =   2880
            Width           =   3915
         End
         Begin VB.TextBox txtSenha 
            Height          =   285
            IMEMode         =   3  'DISABLE
            Left            =   120
            PasswordChar    =   "*"
            TabIndex        =   77
            Top             =   3480
            Width           =   3915
         End
         Begin VB.TextBox txtnudPorta 
            Alignment       =   1  'Right Justify
            Height          =   285
            Left            =   120
            TabIndex        =   76
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
            TabIndex        =   75
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
            TabIndex        =   74
            Top             =   2400
            Width           =   1335
         End
         Begin MSComCtl2.UpDown nudPorta 
            Height          =   285
            Left            =   841
            TabIndex        =   82
            Top             =   2280
            Width           =   255
            _ExtentX        =   450
            _ExtentY        =   503
            _Version        =   393216
            Value           =   5000
            OrigLeft        =   841
            OrigTop         =   2280
            OrigRight       =   1096
            OrigBottom      =   2565
            Max             =   99999
            Enabled         =   -1  'True
         End
         Begin VB.Label Label28 
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
            TabIndex        =   88
            Top             =   240
            Width           =   480
         End
         Begin VB.Label Label27 
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
            TabIndex        =   87
            Top             =   840
            Width           =   450
         End
         Begin VB.Label Label26 
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
            TabIndex        =   86
            Top             =   1440
            Width           =   900
         End
         Begin VB.Label Label25 
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
            TabIndex        =   85
            Top             =   2640
            Width           =   645
         End
         Begin VB.Label Label24 
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
            TabIndex        =   84
            Top             =   3240
            Width           =   525
         End
         Begin VB.Label Label23 
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
            TabIndex        =   83
            Top             =   2040
            Width           =   465
         End
      End
      Begin VB.Frame grpBoxDACTe 
         Caption         =   "DACTe"
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
         TabIndex        =   69
         Top             =   1080
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
            Left            =   2760
            TabIndex        =   71
            Top             =   360
            Width           =   1215
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
            TabIndex        =   70
            Top             =   360
            Value           =   -1  'True
            Width           =   1455
         End
      End
      Begin VB.CommandButton btnLogomarca 
         Caption         =   "..."
         Height          =   260
         Left            =   -71160
         TabIndex        =   68
         Top             =   600
         Width           =   390
      End
      Begin VB.TextBox txtLogomarca 
         Height          =   285
         Left            =   -74760
         TabIndex        =   67
         Top             =   600
         Width           =   3555
      End
      Begin TabDlg.SSTab SSTab2 
         Height          =   6495
         Left            =   240
         TabIndex        =   5
         Top             =   360
         Width           =   4575
         _ExtentX        =   8070
         _ExtentY        =   11456
         _Version        =   393216
         Style           =   1
         Tabs            =   4
         TabsPerRow      =   4
         TabHeight       =   520
         TabCaption(0)   =   "Geral"
         TabPicture(0)   =   "FrmMain.frx":0054
         Tab(0).ControlEnabled=   -1  'True
         Tab(0).Control(0)=   "lblVersao"
         Tab(0).Control(0).Enabled=   0   'False
         Tab(0).Control(1)=   "lblModeloDocumentoFiscal"
         Tab(0).Control(1).Enabled=   0   'False
         Tab(0).Control(2)=   "lblPastadosSchemas"
         Tab(0).Control(2).Enabled=   0   'False
         Tab(0).Control(3)=   "cmbVersao"
         Tab(0).Control(3).Enabled=   0   'False
         Tab(0).Control(4)=   "cmbModeloDocumento"
         Tab(0).Control(4).Enabled=   0   'False
         Tab(0).Control(5)=   "txtSchemaPath"
         Tab(0).Control(5).Enabled=   0   'False
         Tab(0).Control(6)=   "btnSelectSchema"
         Tab(0).Control(6).Enabled=   0   'False
         Tab(0).ControlCount=   7
         TabCaption(1)   =   "Webservices"
         TabPicture(1)   =   "FrmMain.frx":0070
         Tab(1).ControlEnabled=   0   'False
         Tab(1).Control(0)=   "grpBoxProxy"
         Tab(1).Control(1)=   "grpBoxAmbiente"
         Tab(1).Control(2)=   "txtTimeOut"
         Tab(1).Control(3)=   "cmbSSlType"
         Tab(1).Control(4)=   "cmbUfDestino"
         Tab(1).Control(5)=   "nudTimeOut"
         Tab(1).Control(6)=   "Label22"
         Tab(1).Control(7)=   "Label2"
         Tab(1).Control(8)=   "Label1"
         Tab(1).ControlCount=   9
         TabCaption(2)   =   "Certificados"
         TabPicture(2)   =   "FrmMain.frx":008C
         Tab(2).ControlEnabled=   0   'False
         Tab(2).Control(0)=   "btnObterCertificados"
         Tab(2).Control(1)=   "Frame1"
         Tab(2).Control(2)=   "cmbXmlSign"
         Tab(2).Control(3)=   "cmbHttp"
         Tab(2).Control(4)=   "cmbCrypt"
         Tab(2).Control(5)=   "Label5"
         Tab(2).Control(6)=   "Label4"
         Tab(2).Control(7)=   "Label3"
         Tab(2).ControlCount=   8
         TabCaption(3)   =   "Arquivos"
         TabPicture(3)   =   "FrmMain.frx":00A8
         Tab(3).ControlEnabled=   0   'False
         Tab(3).Control(0)=   "btnArqEvento"
         Tab(3).Control(1)=   "txtArqEvento"
         Tab(3).Control(2)=   "btnArqInu"
         Tab(3).Control(3)=   "txtArqInu"
         Tab(3).Control(4)=   "btnArqCTe"
         Tab(3).Control(5)=   "txtArqCTe"
         Tab(3).Control(6)=   "ckbSepararPorModelo"
         Tab(3).Control(7)=   "ckbSepararPorCNPJ"
         Tab(3).Control(8)=   "ckbSalvaPathEvento"
         Tab(3).Control(9)=   "ckbEmissaoPathCTe"
         Tab(3).Control(10)=   "ckbAdicionaLiteral"
         Tab(3).Control(11)=   "ckbPastaMensal"
         Tab(3).Control(12)=   "ckbSalvarArqs"
         Tab(3).Control(13)=   "Label12"
         Tab(3).Control(14)=   "Label11"
         Tab(3).Control(15)=   "Label10"
         Tab(3).ControlCount=   16
         Begin VB.CommandButton btnObterCertificados 
            Caption         =   "Obter Certificados"
            Height          =   360
            Left            =   -74880
            TabIndex        =   72
            Top             =   5280
            Width           =   1935
         End
         Begin VB.CommandButton btnArqEvento 
            Caption         =   "..."
            Height          =   260
            Left            =   -71280
            TabIndex        =   65
            Top             =   4680
            Width           =   390
         End
         Begin VB.TextBox txtArqEvento 
            Height          =   285
            Left            =   -74880
            TabIndex        =   64
            Top             =   4680
            Width           =   3555
         End
         Begin VB.CommandButton btnArqInu 
            Caption         =   "..."
            Height          =   260
            Left            =   -71280
            TabIndex        =   62
            Top             =   3960
            Width           =   390
         End
         Begin VB.TextBox txtArqInu 
            Height          =   285
            Left            =   -74880
            TabIndex        =   61
            Top             =   3960
            Width           =   3555
         End
         Begin VB.CommandButton btnArqCTe 
            Caption         =   "..."
            Height          =   260
            Left            =   -71280
            TabIndex        =   59
            Top             =   3240
            Width           =   390
         End
         Begin VB.TextBox txtArqCTe 
            Height          =   285
            Left            =   -74880
            TabIndex        =   58
            Top             =   3240
            Width           =   3555
         End
         Begin VB.CheckBox ckbSepararPorModelo 
            Caption         =   "Separar Arqs pelo Modelo do Documento"
            Height          =   195
            Left            =   -74880
            TabIndex        =   56
            Top             =   2640
            Width           =   3855
         End
         Begin VB.CheckBox ckbSepararPorCNPJ 
            Caption         =   "Separar Arqs pelo CNPJ do Certificado"
            Height          =   195
            Left            =   -74880
            TabIndex        =   55
            Top             =   2280
            Width           =   3855
         End
         Begin VB.CheckBox ckbSalvaPathEvento 
            Caption         =   "Salvar Arquivos de Eventos"
            Height          =   195
            Left            =   -74880
            TabIndex        =   54
            Top             =   1920
            Width           =   3855
         End
         Begin VB.CheckBox ckbEmissaoPathCTe 
            Caption         =   "Salvar Documento pelo campo Data de Emissão"
            Height          =   195
            Left            =   -74880
            TabIndex        =   53
            Top             =   1560
            Width           =   3855
         End
         Begin VB.CheckBox ckbAdicionaLiteral 
            Caption         =   "Adicionar Literal no nome das pastas"
            Height          =   195
            Left            =   -74880
            TabIndex        =   52
            Top             =   1200
            Width           =   3855
         End
         Begin VB.CheckBox ckbPastaMensal 
            Caption         =   "Criar Pastas Mensalmente"
            Height          =   195
            Left            =   -74880
            TabIndex        =   51
            Top             =   840
            Width           =   3855
         End
         Begin VB.CheckBox ckbSalvarArqs 
            Caption         =   "Salvar Arquivos em Pastas Separadas"
            Height          =   195
            Left            =   -74880
            TabIndex        =   50
            Top             =   480
            Width           =   4215
         End
         Begin VB.Frame Frame1 
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
            TabIndex        =   39
            Top             =   2520
            Width           =   4215
            Begin VB.CommandButton btnDadosPFX 
               Caption         =   "..."
               Height          =   260
               Left            =   3720
               TabIndex        =   49
               Top             =   1080
               Width           =   390
            End
            Begin VB.TextBox txtDadosPFX 
               Height          =   285
               Left            =   120
               TabIndex        =   48
               Top             =   1080
               Width           =   3555
            End
            Begin VB.CommandButton btnSelecionarCertificado 
               Caption         =   "..."
               Height          =   260
               Left            =   3720
               TabIndex        =   43
               Top             =   479
               Width           =   390
            End
            Begin VB.TextBox txtCertPath 
               Height          =   285
               Left            =   120
               TabIndex        =   42
               Top             =   480
               Width           =   3555
            End
            Begin VB.TextBox txtCertPassword 
               Height          =   285
               IMEMode         =   3  'DISABLE
               Left            =   120
               PasswordChar    =   "*"
               TabIndex        =   41
               Top             =   1680
               Width           =   4035
            End
            Begin VB.TextBox txtCertNumero 
               Height          =   285
               Left            =   120
               TabIndex        =   40
               Top             =   2280
               Width           =   4035
            End
            Begin VB.Label Label9 
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
               TabIndex        =   47
               Top             =   840
               Width           =   870
            End
            Begin VB.Label Label8 
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
               TabIndex        =   46
               Top             =   240
               Width           =   735
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
               TabIndex        =   45
               Top             =   1440
               Width           =   525
            End
            Begin VB.Label Label6 
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
               TabIndex        =   44
               Top             =   2040
               Width           =   1395
            End
         End
         Begin VB.ComboBox cmbXmlSign 
            Height          =   315
            ItemData        =   "FrmMain.frx":00C4
            Left            =   -74880
            List            =   "FrmMain.frx":00D7
            Style           =   2  'Dropdown List
            TabIndex        =   38
            Top             =   2040
            Width           =   2175
         End
         Begin VB.ComboBox cmbHttp 
            Height          =   315
            ItemData        =   "FrmMain.frx":0111
            Left            =   -74880
            List            =   "FrmMain.frx":0124
            Style           =   2  'Dropdown List
            TabIndex        =   36
            Top             =   1320
            Width           =   2175
         End
         Begin VB.ComboBox cmbCrypt 
            Height          =   315
            ItemData        =   "FrmMain.frx":0163
            Left            =   -74880
            List            =   "FrmMain.frx":0173
            Style           =   2  'Dropdown List
            TabIndex        =   34
            Top             =   600
            Width           =   2175
         End
         Begin VB.Frame grpBoxProxy 
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
            TabIndex        =   23
            Top             =   2040
            Width           =   4215
            Begin VB.TextBox txtProxyServidor 
               Height          =   285
               Left            =   120
               TabIndex        =   27
               Top             =   480
               Width           =   2655
            End
            Begin VB.TextBox txtProxyPortaWebServices 
               Alignment       =   1  'Right Justify
               Height          =   285
               Left            =   2880
               TabIndex        =   26
               Text            =   "5000"
               Top             =   480
               Width           =   945
            End
            Begin VB.TextBox txtProxyUsuario 
               Height          =   285
               Left            =   120
               TabIndex        =   25
               Top             =   1080
               Width           =   3975
            End
            Begin VB.TextBox txtProxySenha 
               Height          =   285
               IMEMode         =   3  'DISABLE
               Left            =   120
               PasswordChar    =   "*"
               TabIndex        =   24
               Top             =   1680
               Width           =   3975
            End
            Begin MSComCtl2.UpDown nudProxyPorta 
               Height          =   285
               Left            =   3825
               TabIndex        =   28
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
               TabIndex        =   32
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
               TabIndex        =   31
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
               TabIndex        =   30
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
               Left            =   120
               TabIndex        =   29
               Top             =   1440
               Width           =   525
            End
         End
         Begin VB.Frame grpBoxAmbiente 
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
            TabIndex        =   20
            Top             =   1080
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
               TabIndex        =   22
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
               TabIndex        =   21
               Top             =   360
               Width           =   1095
            End
         End
         Begin VB.TextBox txtTimeOut 
            Alignment       =   1  'Right Justify
            Height          =   285
            Left            =   -72000
            TabIndex        =   18
            Text            =   "5000"
            Top             =   600
            Width           =   990
         End
         Begin VB.ComboBox cmbSSlType 
            Height          =   315
            ItemData        =   "FrmMain.frx":01A5
            Left            =   -73800
            List            =   "FrmMain.frx":01BE
            Style           =   2  'Dropdown List
            TabIndex        =   17
            Top             =   600
            Width           =   1695
         End
         Begin VB.ComboBox cmbUfDestino 
            Height          =   315
            ItemData        =   "FrmMain.frx":020A
            Left            =   -74880
            List            =   "FrmMain.frx":025F
            Style           =   2  'Dropdown List
            TabIndex        =   16
            Top             =   600
            Width           =   975
         End
         Begin VB.CommandButton btnSelectSchema 
            Caption         =   "..."
            Height          =   260
            Left            =   4080
            TabIndex        =   12
            Top             =   2160
            Width           =   390
         End
         Begin VB.TextBox txtSchemaPath 
            Height          =   285
            Left            =   240
            TabIndex        =   11
            Top             =   2160
            Width           =   3795
         End
         Begin VB.ComboBox cmbModeloDocumento 
            Height          =   315
            ItemData        =   "FrmMain.frx":02CF
            Left            =   240
            List            =   "FrmMain.frx":02D9
            Style           =   2  'Dropdown List
            TabIndex        =   9
            Top             =   1440
            Width           =   2175
         End
         Begin VB.ComboBox cmbVersao 
            Height          =   315
            ItemData        =   "FrmMain.frx":02ED
            Left            =   240
            List            =   "FrmMain.frx":02FA
            Style           =   2  'Dropdown List
            TabIndex        =   7
            Top             =   720
            Width           =   2175
         End
         Begin MSComCtl2.UpDown nudTimeOut 
            Height          =   285
            Left            =   -71040
            TabIndex        =   19
            Top             =   600
            Width           =   255
            _ExtentX        =   450
            _ExtentY        =   503
            _Version        =   393216
            Value           =   5000
            OrigLeft        =   3960
            OrigTop         =   600
            OrigRight       =   4215
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
            TabIndex        =   63
            Top             =   4440
            Width           =   1905
         End
         Begin VB.Label Label11 
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
            TabIndex        =   60
            Top             =   3720
            Width           =   2310
         End
         Begin VB.Label Label10 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "Pasta Arquivos CTe"
            BeginProperty Font 
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
            TabIndex        =   57
            Top             =   3000
            Width           =   1635
         End
         Begin VB.Label Label5 
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
            TabIndex        =   37
            Top             =   1800
            Width           =   915
         End
         Begin VB.Label Label4 
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
            TabIndex        =   35
            Top             =   1080
            Width           =   615
         End
         Begin VB.Label Label3 
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
            TabIndex        =   33
            Top             =   360
            Width           =   705
         End
         Begin VB.Label Label22 
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
            TabIndex        =   15
            Top             =   360
            Width           =   765
         End
         Begin VB.Label Label2 
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
            TabIndex        =   14
            Top             =   360
            Width           =   765
         End
         Begin VB.Label Label1 
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
            TabIndex        =   13
            Top             =   360
            Width           =   870
         End
         Begin VB.Label lblPastadosSchemas 
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
            Left            =   240
            TabIndex        =   10
            Top             =   1920
            Width           =   1635
         End
         Begin VB.Label lblModeloDocumentoFiscal 
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
            Left            =   240
            TabIndex        =   8
            Top             =   1200
            Width           =   2145
         End
         Begin VB.Label lblVersao 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "Versão"
            BeginProperty Font 
               Name            =   "Tahoma"
               Size            =   8.25
               Charset         =   0
               Weight          =   700
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            Height          =   195
            Left            =   240
            TabIndex        =   6
            Top             =   480
            Width           =   585
         End
      End
      Begin VB.Label Label30 
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
         Left            =   -74760
         TabIndex        =   91
         Top             =   5160
         Width           =   930
      End
      Begin VB.Label Label29 
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
         Left            =   -74760
         TabIndex        =   89
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
         Left            =   -74760
         TabIndex        =   66
         Top             =   360
         Width           =   945
      End
   End
   Begin TabDlg.SSTab SSTab 
      Height          =   3615
      Left            =   5400
      TabIndex        =   3
      Top             =   120
      Width           =   6135
      _ExtentX        =   10821
      _ExtentY        =   6376
      _Version        =   393216
      Style           =   1
      Tabs            =   5
      Tab             =   4
      TabsPerRow      =   5
      TabHeight       =   520
      TabCaption(0)   =   "Envio"
      TabPicture(0)   =   "FrmMain.frx":0313
      Tab(0).ControlEnabled=   0   'False
      Tab(0).Control(0)=   "btnGerarXml"
      Tab(0).Control(0).Enabled=   0   'False
      Tab(0).Control(1)=   "btnEnviarSincrono"
      Tab(0).Control(1).Enabled=   0   'False
      Tab(0).Control(2)=   "btnEnviarAssincrono"
      Tab(0).Control(2).Enabled=   0   'False
      Tab(0).Control(3)=   "btnCarregarIni"
      Tab(0).Control(3).Enabled=   0   'False
      Tab(0).Control(4)=   "btnEnviarCTeEmail"
      Tab(0).Control(4).Enabled=   0   'False
      Tab(0).Control(5)=   "btnAssinarCTe"
      Tab(0).Control(5).Enabled=   0   'False
      Tab(0).Control(6)=   "btnImprimirDACTe"
      Tab(0).Control(6).Enabled=   0   'False
      Tab(0).Control(7)=   "btnCarregarXml"
      Tab(0).Control(7).Enabled=   0   'False
      Tab(0).Control(8)=   "btnImprimirPDFDACTE"
      Tab(0).Control(8).Enabled=   0   'False
      Tab(0).Control(9)=   "btnValidarRegra"
      Tab(0).Control(9).Enabled=   0   'False
      Tab(0).Control(10)=   "btnLimparLista"
      Tab(0).Control(10).Enabled=   0   'False
      Tab(0).Control(11)=   "btnGerarChaveCTe"
      Tab(0).Control(11).Enabled=   0   'False
      Tab(0).ControlCount=   12
      TabCaption(1)   =   "Consultas"
      TabPicture(1)   =   "FrmMain.frx":032F
      Tab(1).ControlEnabled=   0   'False
      Tab(1).Control(0)=   "btnConsultaChave"
      Tab(1).Control(1)=   "btnConsultarCadastro"
      Tab(1).Control(2)=   "btnConsultaXml"
      Tab(1).Control(3)=   "btnConsultarRecibo"
      Tab(1).Control(4)=   "btnStatusServ"
      Tab(1).ControlCount=   5
      TabCaption(2)   =   "Eventos"
      TabPicture(2)   =   "FrmMain.frx":034B
      Tab(2).ControlEnabled=   0   'False
      Tab(2).Control(0)=   "btnImprimirEventoPDF"
      Tab(2).Control(1)=   "btnLimparListaEvento"
      Tab(2).Control(2)=   "btnEnviarEvento"
      Tab(2).Control(3)=   "btnEnviarEmailEvento"
      Tab(2).Control(4)=   "btnImprimirEvento"
      Tab(2).Control(5)=   "btnCarregarEvento"
      Tab(2).Control(6)=   "btnCancelar"
      Tab(2).ControlCount=   7
      TabCaption(3)   =   "Inutilização"
      TabPicture(3)   =   "FrmMain.frx":0367
      Tab(3).ControlEnabled=   0   'False
      Tab(3).Control(0)=   "btnImprimirInutilizacaoPDF"
      Tab(3).Control(1)=   "btnImprimirInutilizacao"
      Tab(3).Control(2)=   "btnInutilizar"
      Tab(3).ControlCount=   3
      TabCaption(4)   =   "Distribuição DFe"
      TabPicture(4)   =   "FrmMain.frx":0383
      Tab(4).ControlEnabled=   -1  'True
      Tab(4).Control(0)=   "btnDFePorChave"
      Tab(4).Control(0).Enabled=   0   'False
      Tab(4).Control(1)=   "btnDFePorNSU"
      Tab(4).Control(1).Enabled=   0   'False
      Tab(4).Control(2)=   "btnDFePorUltNSU"
      Tab(4).Control(2).Enabled=   0   'False
      Tab(4).ControlCount=   3
      Begin VB.CommandButton btnGerarChaveCTe 
         Caption         =   "Gerar Chave CTe"
         Height          =   360
         Left            =   -74880
         TabIndex        =   123
         Top             =   2880
         Width           =   1575
      End
      Begin VB.CommandButton btnDFePorUltNSU 
         Caption         =   "Por Ult. NSU"
         Height          =   360
         Left            =   4080
         TabIndex        =   121
         Top             =   480
         Width           =   1695
      End
      Begin VB.CommandButton btnDFePorNSU 
         Caption         =   "Por NSU"
         Height          =   360
         Left            =   2160
         TabIndex        =   120
         Top             =   480
         Width           =   1695
      End
      Begin VB.CommandButton btnDFePorChave 
         Caption         =   "Por Chave"
         Height          =   360
         Left            =   240
         TabIndex        =   119
         Top             =   480
         Width           =   1695
      End
      Begin VB.CommandButton btnImprimirInutilizacaoPDF 
         Caption         =   "Imprimir PDF Inutilização"
         Height          =   360
         Left            =   -72840
         TabIndex        =   118
         Top             =   960
         Width           =   2055
      End
      Begin VB.CommandButton btnImprimirInutilizacao 
         Caption         =   "Imprimir Inutilização"
         Height          =   360
         Left            =   -74760
         TabIndex        =   117
         Top             =   960
         Width           =   1695
      End
      Begin VB.CommandButton btnInutilizar 
         Caption         =   "Inutilizar Numeração"
         Height          =   360
         Left            =   -74760
         TabIndex        =   116
         Top             =   480
         Width           =   1695
      End
      Begin VB.CommandButton btnImprimirEventoPDF 
         Caption         =   "Imprimir PDF Evento"
         Height          =   360
         Left            =   -72960
         TabIndex        =   115
         Top             =   1440
         Width           =   1695
      End
      Begin VB.CommandButton btnLimparListaEvento 
         Caption         =   "Limpar Lista Eventos"
         Height          =   360
         Left            =   -72960
         TabIndex        =   114
         Top             =   960
         Width           =   1695
      End
      Begin VB.CommandButton btnEnviarEvento 
         Caption         =   "Enviar Evento"
         Height          =   360
         Left            =   -72960
         TabIndex        =   113
         Top             =   480
         Width           =   1695
      End
      Begin VB.CommandButton btnEnviarEmailEvento 
         Caption         =   "Enviar Evento Email"
         Height          =   360
         Left            =   -74880
         TabIndex        =   112
         Top             =   1920
         Width           =   1695
      End
      Begin VB.CommandButton btnImprimirEvento 
         Caption         =   "Imprimir Evento"
         Height          =   360
         Left            =   -74880
         TabIndex        =   111
         Top             =   1440
         Width           =   1695
      End
      Begin VB.CommandButton btnCarregarEvento 
         Caption         =   "Carregar Evento"
         Height          =   360
         Left            =   -74880
         TabIndex        =   110
         Top             =   960
         Width           =   1695
      End
      Begin VB.CommandButton btnCancelar 
         Caption         =   "Cancelar CTe"
         Height          =   360
         Left            =   -74880
         TabIndex        =   109
         Top             =   480
         Width           =   1695
      End
      Begin VB.CommandButton btnConsultaChave 
         Caption         =   "Consultar com Chave"
         Height          =   360
         Left            =   -71280
         TabIndex        =   108
         Top             =   480
         Width           =   1695
      End
      Begin VB.CommandButton btnConsultarCadastro 
         Caption         =   "Consultar Cadastro"
         Height          =   360
         Left            =   -73080
         TabIndex        =   107
         Top             =   960
         Width           =   1575
      End
      Begin VB.CommandButton btnConsultaXml 
         Caption         =   "Consultar com Xml"
         Height          =   360
         Left            =   -73080
         TabIndex        =   106
         Top             =   480
         Width           =   1575
      End
      Begin VB.CommandButton btnConsultarRecibo 
         Caption         =   "Consultar Recibo"
         Height          =   360
         Left            =   -74880
         TabIndex        =   105
         Top             =   960
         Width           =   1575
      End
      Begin VB.CommandButton btnStatusServ 
         Caption         =   "Status de Serviço"
         Height          =   360
         Left            =   -74880
         TabIndex        =   104
         Top             =   480
         Width           =   1575
      End
      Begin VB.CommandButton btnLimparLista 
         Caption         =   "Limpar Lista CTe"
         Height          =   360
         Left            =   -71280
         TabIndex        =   103
         Top             =   960
         Width           =   1575
      End
      Begin VB.CommandButton btnValidarRegra 
         Caption         =   "Val. Regra de Neg."
         Height          =   360
         Left            =   -73080
         TabIndex        =   102
         Top             =   2400
         Width           =   1575
      End
      Begin VB.CommandButton btnImprimirPDFDACTE 
         Caption         =   "Imprimir PDF DACTe"
         Height          =   360
         Left            =   -73080
         TabIndex        =   101
         Top             =   1440
         Width           =   1575
      End
      Begin VB.CommandButton btnCarregarXml 
         Caption         =   "Carregar Xml CTe"
         Height          =   360
         Left            =   -73080
         TabIndex        =   100
         Top             =   960
         Width           =   1575
      End
      Begin VB.CommandButton btnImprimirDACTe 
         Caption         =   "Imprimir DACTe"
         Height          =   360
         Left            =   -74880
         TabIndex        =   99
         Top             =   1440
         Width           =   1575
      End
      Begin VB.CommandButton btnAssinarCTe 
         Caption         =   "Assinar CTe "
         Height          =   360
         Left            =   -74880
         TabIndex        =   98
         Top             =   2400
         Width           =   1575
      End
      Begin VB.CommandButton btnEnviarCTeEmail 
         Caption         =   "Enviar CTe Email"
         Height          =   360
         Left            =   -74880
         TabIndex        =   97
         Top             =   1920
         Width           =   1575
      End
      Begin VB.CommandButton btnCarregarIni 
         Caption         =   "Carregar INI CTe"
         Height          =   360
         Left            =   -74880
         TabIndex        =   96
         Top             =   960
         Width           =   1575
      End
      Begin VB.CommandButton btnEnviarAssincrono 
         Caption         =   "Enviar Assincrono"
         Height          =   360
         Left            =   -71280
         TabIndex        =   95
         Top             =   480
         Width           =   1575
      End
      Begin VB.CommandButton btnEnviarSincrono 
         Caption         =   "Enviar Sincrono"
         Height          =   360
         Left            =   -73080
         TabIndex        =   94
         Top             =   480
         Width           =   1575
      End
      Begin VB.CommandButton btnGerarXml 
         Caption         =   "Gerar Xml"
         Height          =   360
         Left            =   -74880
         TabIndex        =   93
         Top             =   480
         Width           =   1575
      End
   End
   Begin MSComDlg.CommonDialog CommonDialog1 
      Left            =   5400
      Top             =   7440
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
   End
   Begin VB.Frame FraRespostas 
      Caption         =   "Respostas"
      Height          =   3735
      Left            =   5280
      TabIndex        =   1
      Top             =   3840
      Width           =   6255
      Begin VB.TextBox rtbRespostas 
         Height          =   3375
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
      Left            =   2760
      TabIndex        =   0
      Top             =   7320
      Width           =   2295
   End
End
Attribute VB_Name = "FrmMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim cte As ACBrCTe

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

Private Sub CheckCTeLista()

    Dim xml As Boolean
    
    answer = MsgBox("Limpar Lista ?", vbExclamation + vbYesNo, "Add Confirm")
    If answer = vbYes Then
    cte.LimparLista
    End If
    
    If xml Then
    
    CommonDialog1.DialogTitle = "Selecione o arquivo ini"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivo xml CTe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
    
    If CommonDialog1.FileName = vbNullString Then Exit Sub
    
    cte.CarregarXML CommonDialog1.FileName
                    
    Else
    
    CommonDialog1.DialogTitle = "Selecione o arquivo ini"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivo ini CTe (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
    
    If CommonDialog1.FileName = vbNullString Then Exit Sub
    
    cte.CarregarINI CommonDialog1.FileName
    End If

End Sub

Private Sub btnArqCTe_Click()

    txtArqCTe.Text = BrowseFolder("Selecione a pasta Arquivos CTe")

End Sub

Private Sub btnArqEvento_Click()

    txtArqEvento.Text = BrowseFolder("Selecione a pasta Arquivos Eventos")

End Sub

Private Sub btnArqInu_Click()

    txtArqInu.Text = BrowseFolder("Selecione a pasta Arquivos Inutilização")

End Sub

Private Sub btnAssinarCTe_Click()

    If Not validacao Then
        MsgBox ("Erro Verifique as configurações do certificado")
        Exit Sub
    End If
    
    On Error GoTo Erro:
    CheckCTeLista
    cte.Assinar
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnCancelar_Click()

    If Not validacao Then
        MsgBox ("Erro Verifique as configurações do certificado")
        Exit Sub
    End If

    On Error GoTo Erro:
    Dim idLote, aJustificativa, eChave, eCNPJ As String
    
    idLote = InputBox("Identificador de controle do Lote de envio do Evento", "WebServices Eventos: Cancelamento", "1")
    eChave = InputBox("Chave da CTe", "WebServices Eventos: Cancelamento", "")
    eCNPJ = InputBox("CNPJ ou o CPF do autor do Evento", "WebServices Eventos: Cancelamento", "")
    aJustificativa = InputBox("Justificativa do Cancelamento", "WebServices Eventos: Cancelamento", "")

    SetResposta cte.Cancelar(eChave, aJustificativa, eCNPJ, CLng(idLote))
    
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnCarregarConfiguracoes_Click()

    LoadConfig

End Sub

Private Sub btnCarregarEvento_Click()

    On Error GoTo Erro:
    Dim arquivoIni As String
    
    CommonDialog1.DialogTitle = "Selecione o arquivo ini"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivo Ini CTe (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
            
    If CommonDialog1.FileName = vbNullString Then Exit Sub
    arquivoIni = CommonDialog1.FileName
    
    cte.CarregarEventoINI (arquivoIni)
   
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnCarregarIni_Click()

    On Error GoTo Erro:
    CheckCTeLista
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnCarregarXml_Click()

    On Error GoTo Erro:
    
    CheckCTeLista
    
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnConsultaChave_Click()

    If Not validacao Then
        MsgBox ("Erro Verifique as configurações do certificado")
        Exit Sub
    End If

    On Error GoTo Erro:
    Dim chaveOuCTe As String
    
    chaveOuCTe = InputBox("Chave da CT-e:", "WebServices Consultar: Nota", "")
    SetResposta cte.Consultar(chaveOuCTe)
    
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnConsultarCadastro_Click()

    If Not validacao Then
        MsgBox ("Erro Verifique as configurações do certificado")
        Exit Sub
    End If

    On Error GoTo Erro:
    Dim ret, uf, documento As String
    Dim ie As Boolean
   
    
    uf = InputBox("UF", "WebServices Consultar: Cadastro", "")
    documento = InputBox("Documento", "WebServices Consultar: Cadastro", "")
    ie = MsgBox("WebServices Consultar: Cadastro", "O documento é uma inscrição estadual ?", vbExclamation + vbYesNo, "Add Confirm")
    
    ret = cte.ConsultaCadastro(uf, documento, ie)
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
    SetResposta cte.ConsultarRecibo(aRecibo)
    
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnConsultaXml_Click()

    If Not validacao Then
        MsgBox ("Erro Verifique as configurações do certificado")
        Exit Sub
    End If

    On Error GoTo Erro:
    Dim ret As String
    
    CommonDialog1.DialogTitle = "Selecione o arquivo xml"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivo Xml CTe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
            
    If CommonDialog1.FileName = vbNullString Then Exit Sub
    
    cte.LimparLista
    ret = cte.Consultar(CommonDialog1.FileName)
    rtbRespostas.Text = ret
    
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnEnviar_Click()

    On Error GoTo Erro:
    
    CommonDialog1.DialogTitle = "Selecione o arquivo ini"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivo Ini CTe (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
            
    If CommonDialog1.FileName = vbNullString Then Exit Sub
    
    cte.LimparLista
    cte.CarregarINI (CommonDialog1.FileName)
    SetResposta cte.Enviar(1)
    
Erro:
    MsgBox Err.Description
    Exit Sub
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
    Dim ret, cnpj, chave As String
    Dim codUF As Long
    
    codUF = InputBox("Codigo da UF", "WebServices: Distribuição DFe", 35)
    cnpj = InputBox("CNPJ do Autor", "WebServices: Distribuição DFe", "")
    chave = InputBox("Chave do NFe", "WebServices: Distribuição DFe", "")
    
    ret = cte.DistribuicaoDFePorChave(codUF, cnpj, chave)
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
    Dim ret, cnpj, eNsu As String
    Dim codUF As Long
   
    codUF = InputBox("Codigo da UF", "WebServices: Distribuição DFe", 35)
    cnpj = InputBox("CNPJ do Autor", "WebServices: Distribuição DFe", "")
    eNsu = InputBox("Numero do NSU", "WebServices: Distribuição DFe", "")
    
    ret = cte.DistribuicaoDFePorNSU(codUF, cnpj, eNsu)
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
    Dim ret, cnpj, eNsu As String
    Dim codUF As Long
    
    codUF = InputBox("Codigo da UF", "WebServices: Distribuição DFe", 35)
    cnpj = InputBox("CNPJ do Autor", "WebServices: Distribuição DFe", "")
    eNsu = InputBox("Número do último NSU", "WebServices: Distribuição DFe", "")
    
    ret = cte.DistribuicaoDFePorUltNSU(codUF, cnpj, eNsu)
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
    
    CheckCTeLista
    aLote = InputBox("Número do Lote", "WebServices Enviar", 1)
    ret = cte.Enviar(aLote)
    rtbRespostas.Text = ret
    
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnEnviarCTeEmail_Click()

    If Not validacaoEmail Then
        MsgBox ("Verifique as configurações de E-mail")
        Exit Sub
    End If

    On Error GoTo Erro:
    
    CommonDialog1.DialogTitle = "Selecione o arquivo xml"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivo Xml CTe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
            
    If CommonDialog1.FileName = vbNullString Then Exit Sub
    
    Dim destinatario As String
    destinatario = InputBox("Digite o email do destinatario", "Envio email", vbNullString)
    
    If destinatario = vbNullString Then Exit Sub
    cte.EnviarEmail destinatario, CommonDialog1.FileName, True, txtAssunto.Text, txtMensagem.Text
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
    CommonDialog1.Filter = "Arquivo Xml CTe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
    
    If CommonDialog.FileName1 = vbNullString Then Exit Sub
    arquivoXml = CommonDialog.FileName
    
    destinatario = ""
    destinatario = InputBox("Digite o email do destinatario", "Envio email", "")
    
    cte.EnviarEmailEvento destinatario, arquivoXmlEvento, arquivoXml, True, txtAssuntoEmail.Text, txtMensagemEmail.Text
    
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
    ret = cte.EnviarEvento(idLote)
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
    
    CheckCTeLista
    aLote = InputBox("Número do Lote", "WebServices Enviar", 1)
    ret = cte.Enviar(aLote, sincrono = True)
    rtbRespostas.Text = ret
    
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnGerarChaveCTe_Click()
    
    On Error GoTo Erro:
    
    Dim uf, cod, doc, serie, numero, emissao, cnjCPF As Long
    
    uf = InputBox("Digite o codigo da UF:", "Gerar Chave", 35)
    cod = InputBox("Digite o codigo da Númerico:", "Gerar Chave", 45812)
    doc = InputBox("Digite o modelo do documento:", "Gerar Chave", 55)
    serie = InputBox("Digite a serie do documento:", "Gerar Chave", 1)
    numero = InputBox("Digite o numero do documento:", "Gerar Chave", 1)
    emissao = InputBox("Digite o tipo de emissão do documento:", "Gerar Chave", 1)
    cnpjCPF = InputBox("Digite o CPF/CNPJ para Gerar a Chave", "Gerar Chave", "")
    
    rtbRespostas = cte.GerarChave(uf, cod, doc, serie, numero, emissao, DateTime.Now, cnjCPF)
    
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
    CommonDialog1.Filter = "Arquivo ini CTe (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
            
    If CommonDialog1.FileName = vbNullString Then Exit Sub
    
    cte.LimparLista
    cte.CarregarINI CommonDialog1.FileName
    cte.Assinar
    ret = cte.ObterXml(0)
    rtbRespostas.Text = ret
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnImprimirDACTe_Click()

     On Error GoTo Erro:
    
    CommonDialog1.DialogTitle = "Selecione o arquivo xml"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivo Xml CTe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
            
    If CommonDialog1.FileName = vbNullString Then Exit Sub
    
    cte.LimparLista
    cte.CarregarXML CommonDialog1.FileName
    cte.Imprimir
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
    CommonDialog1.Filter = "Arquivo Xml CTe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
    
    If CommonDialog.FileName1 = vbNullString Then Exit Sub
    arquivoXml = CommonDialog.FileName
    
    cte.ImprimirEvento arquivoXml, arquivoXmlEvento
    
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
    CommonDialog1.Filter = "Arquivo Xml CTe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
    
    If CommonDialog.FileName1 = vbNullString Then Exit Sub
    arquivoXml = CommonDialog.FileName
    
    cte.ImprimirEventoPDF arquivoXml, arquivoXmlEvento
    
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
    
    cte.ImprimirInutilizacao (arquivoXml)
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
    
    cte.ImprimirInutilizacaoPDF (arquivoXml)
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnImprimirPDFDACTE_Click()

    On Error GoTo Erro:
    
    CommonDialog1.DialogTitle = "Selecione o arquivo xml"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivo Xml DACTE (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.FileName = vbNullString
    CommonDialog1.ShowOpen
            
    If CommonDialog1.FileName = vbNullString Then Exit Sub
    
    cte.LimparLista
    cte.CarregarXML CommonDialog1.FileName
    cte.Imprimir
Erro:
    MsgBox Err.Description

End Sub

Private Sub btnInutilizar_Click()

    If Not validacao Then
        MsgBox ("Erro Verifique as configurações do certificado")
        Exit Sub
    End If

    On Error GoTo Erro:
    Dim ano, modelo, serie, numeroInicial, numeroFinal, aJustificativa, eCNPJ As String
    
    ano = InputBox("Ano", "WebServices Inutilização", "19")
    modelo = InputBox("Modelo", "WebServices Inutilização", "55")
    serie = InputBox("Serie", "WebServices Inutilização", "1")
    numeroInicial = InputBox("Número Inicial", "WebServices Inutilização", "1")
    numeroFinal = InputBox("Número Final", "WebServices Inutilização", "1")
    eCNPJ = InputBox("CNPJ ou o CPF do autor do Emitente", "WebServices Inutilização", "")
    aJustificativa = InputBox("Justificativa", "WebServices Inutilização", "")
    
    SetResposta cte.Inutilizar(eCNPJ, aJustificativa, CLng(ano), CLng(modelo), CLng(serie), CLng(numeroInicial), CLng(numeroFinal))
    
Erro:
    MsgBox Err.Description
    
End Sub

Private Sub btnLimparLista_Click()

    On Error GoTo Erro:
    answer = MsgBox("Limpar Lista ?", vbExclamation + vbYesNo, "Add Confirm")
    If answer = vbYes Then
    cte.LimparLista
    End If

Erro:
    MsgBox Err.Description

End Sub

Private Sub btnLimparListaEvento_Click()

    On Error GoTo Erro:
    answer = MsgBox("Limpar a lista de eventos ?", vbExclamation + vbYesNo, "Add Confirm")
    If answer = vbYes Then
    cte.LimparListaEventos
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
    
    cte.ObterCertificados
    
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
    
    If Not validacao Then
        MsgBox ("Erro Verifique as configurações do certificado")
        Exit Sub
    End If

    On Error Resume Next
    
    Dim retorno As Long
    
    SetResposta cte.StatusServico
    
    If Err.Number <> 0 Then
       MsgBox Err.Description
       Exit Sub
    End If
    On Error GoTo 0

End Sub

Private Sub btnValidarRegra_Click()

    On Error GoTo Erro:
    
    CheckCTeLista
    
    rtbRespostas.Text = cte.ValidarRegrasdeNegocios
    
Erro:
    MsgBox Err.Description

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
    
    Set cte = CreateCTe(IniPath)
    
    cte.ConfigGravarValor SESSAO_PRINCIPAL, "LogNivel", NivelLog.logParanoico
    cte.ConfigGravarValor SESSAO_PRINCIPAL, "LogPath", LogPath
    cte.ConfigGravar
    
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
    
    cte.ConfigLer
    cmbModeloDocumento.ListIndex = CLng(cte.ConfigLerValor(SESSAO_CTe, "ModeloDF"))
    cmbVersao.ListIndex = CLng(cte.ConfigLerValor(SESSAO_CTe, "VersaoDF"))
    cmbCrypt.ListIndex = CLng(cte.ConfigLerValor(SESSAO_DFE, "SSLCryptLib"))
    cmbHttp.ListIndex = CLng(cte.ConfigLerValor(SESSAO_DFE, "SSLHttpLib"))
    cmbXmlSign.ListIndex = CLng(cte.ConfigLerValor(SESSAO_DFE, "SSLXmlSignLib"))
    txtCertPath.Text = cte.ConfigLerValor(SESSAO_DFE, "ArquivoPFX")
    txtCertPassword.Text = cte.ConfigLerValor(SESSAO_DFE, "Senha")
    txtCertNumero.Text = cte.ConfigLerValor(SESSAO_DFE, "NumeroSerie")
    txtSchemaPath.Text = cte.ConfigLerValor(SESSAO_CTe, "PathSchemas")
    cmbUfDestino.Text = cte.ConfigLerValor(SESSAO_DFE, "UF")
    
    Dim ambiente As String
    ambiente = cte.ConfigLerValor(SESSAO_CTe, "Ambiente")
    
    rdbHomologacao.Value = CBool(ambiente)
    rdbProducao.Value = Not CBool(ambiente)
    
    cmbSSlType.ListIndex = CLng(cte.ConfigLerValor(SESSAO_CTe, "SSLType"))
    nudTimeOut.Value = CLng(cte.ConfigLerValor(SESSAO_CTe, "Timeout"))
    txtProxyServidor.Text = cte.ConfigLerValor(SESSAO_PROXY, "Servidor")
    
    Dim porta As String
    porta = cte.ConfigLerValor(SESSAO_PROXY, "Porta")
    
    If IsNumeric(porta) Then
      nudProxyPorta.Value = CLng(porta)
    End If
    
    txtProxyUsuario.Text = cte.ConfigLerValor(SESSAO_PROXY, "Usuario")
    txtProxySenha.Text = cte.ConfigLerValor(SESSAO_PROXY, "Senha")
    txtNome.Text = cte.ConfigLerValor(SESSAO_EMAIL, "Nome")
    txtEmail.Text = cte.ConfigLerValor(SESSAO_EMAIL, "Conta")
    txtUsuario.Text = cte.ConfigLerValor(SESSAO_EMAIL, "Usuario")
    txtSenha.Text = cte.ConfigLerValor(SESSAO_EMAIL, "Senha")
    txtHost.Text = cte.ConfigLerValor(SESSAO_EMAIL, "Servidor")
    nudPorta.Value = CLng(cte.ConfigLerValor(SESSAO_EMAIL, "Porta"))
    
    ckbSSL.Value = CLng(cte.ConfigLerValor(SESSAO_EMAIL, "SSL"))
    ckbTLS.Value = CLng(cte.ConfigLerValor(SESSAO_EMAIL, "TLS"))
    
End Sub

Private Sub SalvarConfig()
    Dim retorno As Long
    
    cte.ConfigGravarValor SESSAO_CTe, "ModeloDF", CStr(cmbModeloDocumento.ListIndex)
    cte.ConfigGravarValor SESSAO_CTe, "VersaoDF", CStr(cmbVersao.ListIndex)
    cte.ConfigGravarValor SESSAO_DFE, "SSLCryptLib", CStr(cmbCrypt.ListIndex)
    cte.ConfigGravarValor SESSAO_DFE, "SSLHttpLib", CStr(cmbHttp.ListIndex)
    cte.ConfigGravarValor SESSAO_DFE, "SSLXmlSignLib", CStr(cmbXmlSign.ListIndex)
    cte.ConfigGravarValor SESSAO_DFE, "ArquivoPFX", txtCertPath.Text
    cte.ConfigGravarValor SESSAO_DFE, "Senha", txtCertPassword.Text
    cte.ConfigGravarValor SESSAO_DFE, "NumeroSerie", txtCertNumero.Text
    cte.ConfigGravarValor SESSAO_CTe, "PathSchemas", txtSchemaPath.Text
    cte.ConfigGravarValor SESSAO_DFE, "UF", cmbUfDestino.Text
    cte.ConfigGravarValor SESSAO_CTe, "Ambiente", CStr(rdbHomologacao.Value)
    cte.ConfigGravarValor SESSAO_CTe, "SSLType", CStr(cmbSSlType.ListIndex)
    cte.ConfigGravarValor SESSAO_CTe, "Timeout", CStr(nudTimeOut.Value)
    cte.ConfigGravarValor SESSAO_PROXY, "Servidor", txtProxyServidor.Text
    cte.ConfigGravarValor SESSAO_PROXY, "Porta", CStr(nudProxyPorta.Value)
    cte.ConfigGravarValor SESSAO_PROXY, "Usuario", txtProxyUsuario.Text
    cte.ConfigGravarValor SESSAO_PROXY, "Senha", txtProxySenha.Text
    cte.ConfigGravarValor SESSAO_EMAIL, "Nome", txtNome.Text
    cte.ConfigGravarValor SESSAO_EMAIL, "Conta", txtEmail.Text
    cte.ConfigGravarValor SESSAO_EMAIL, "Usuario", txtUsuario.Text
    cte.ConfigGravarValor SESSAO_EMAIL, "Senha", txtSenha.Text
    cte.ConfigGravarValor SESSAO_EMAIL, "Servidor", txtHost.Text
    cte.ConfigGravarValor SESSAO_EMAIL, "Porta", CStr(nudPorta.Value)
    cte.ConfigGravarValor SESSAO_EMAIL, "SSL", CStr(ckbSSL.Value)
    cte.ConfigGravarValor SESSAO_EMAIL, "TLS", CStr(ckbTLS.Value)
    cte.ConfigGravar
End Sub
