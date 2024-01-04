VERSION 5.00
Object = "{86CF1D34-0C5F-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCT2.OCX"
Object = "{BDC217C8-ED16-11CD-956C-0000C04E4C0A}#1.1#0"; "TABCTL32.OCX"
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "COMDLG32.OCX"
Begin VB.Form FrmMain 
   BackColor       =   &H00FFFFFF&
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "ACBrLibPIXCD Demo"
   ClientHeight    =   10335
   ClientLeft      =   45
   ClientTop       =   390
   ClientWidth     =   13725
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
   ScaleHeight     =   10335
   ScaleWidth      =   13725
   StartUpPosition =   2  'CenterScreen
   Begin VB.CommandButton btnSalvarConfiguracoes 
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
      Left            =   5040
      TabIndex        =   4
      Top             =   9720
      Width           =   2295
   End
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
      Top             =   9720
      Width           =   2535
   End
   Begin TabDlg.SSTab SSTab1 
      Height          =   2055
      Index           =   0
      Left            =   7440
      TabIndex        =   2
      Top             =   120
      Width           =   6135
      _ExtentX        =   10821
      _ExtentY        =   3625
      _Version        =   393216
      Style           =   1
      Tabs            =   1
      TabsPerRow      =   1
      TabHeight       =   520
      TabCaption(0)   =   "EndPoints"
      TabPicture(0)   =   "FrmMain.frx":0000
      Tab(0).ControlEnabled=   -1  'True
      Tab(0).Control(0)=   "SSTab1(1)"
      Tab(0).Control(0).Enabled=   0   'False
      Tab(0).ControlCount=   1
      Begin TabDlg.SSTab SSTab1 
         Height          =   1575
         Index           =   1
         Left            =   0
         TabIndex        =   8
         Top             =   360
         Width           =   6015
         _ExtentX        =   10610
         _ExtentY        =   2778
         _Version        =   393216
         Style           =   1
         Tabs            =   4
         TabsPerRow      =   4
         TabHeight       =   520
         TabCaption(0)   =   "PIXCD"
         TabPicture(0)   =   "FrmMain.frx":001C
         Tab(0).ControlEnabled=   -1  'True
         Tab(0).Control(0)=   "btnGerarQRCodeEstatico(1)"
         Tab(0).Control(0).Enabled=   0   'False
         Tab(0).ControlCount=   1
         TabCaption(1)   =   "/Pix"
         TabPicture(1)   =   "FrmMain.frx":0038
         Tab(1).ControlEnabled=   0   'False
         Tab(1).Control(0)=   "btnConsultarPix(2)"
         Tab(1).Control(0).Enabled=   0   'False
         Tab(1).Control(1)=   "btnConsultarPixRecebidos(3)"
         Tab(1).Control(1).Enabled=   0   'False
         Tab(1).Control(2)=   "btnSolicitarDevolucaoPix(4)"
         Tab(1).Control(2).Enabled=   0   'False
         Tab(1).Control(3)=   "btnConsultarDevolucaoPix(5)"
         Tab(1).Control(3).Enabled=   0   'False
         Tab(1).ControlCount=   4
         TabCaption(2)   =   "/Cob"
         TabPicture(2)   =   "FrmMain.frx":0054
         Tab(2).ControlEnabled=   0   'False
         Tab(2).Control(0)=   "btnCancelarCobrancaImediata(9)"
         Tab(2).Control(0).Enabled=   0   'False
         Tab(2).Control(1)=   "btnRevisarCobrancaImediata(8)"
         Tab(2).Control(1).Enabled=   0   'False
         Tab(2).Control(2)=   "btnConsultarCobrancaImediata(7)"
         Tab(2).Control(2).Enabled=   0   'False
         Tab(2).Control(3)=   "btnCriarCobrancaImediata(6)"
         Tab(2).Control(3).Enabled=   0   'False
         Tab(2).ControlCount=   4
         TabCaption(3)   =   "/CobV"
         TabPicture(3)   =   "FrmMain.frx":0070
         Tab(3).ControlEnabled=   0   'False
         Tab(3).Control(0)=   "btnCriarCobranca(0)"
         Tab(3).Control(0).Enabled=   0   'False
         Tab(3).Control(1)=   "btnConsultarCobranca(10)"
         Tab(3).Control(1).Enabled=   0   'False
         Tab(3).Control(2)=   "btnRevisarCobranca(11)"
         Tab(3).Control(2).Enabled=   0   'False
         Tab(3).Control(3)=   "btnCancelarCobranca(12)"
         Tab(3).Control(3).Enabled=   0   'False
         Tab(3).ControlCount=   4
         Begin VB.CommandButton btnCancelarCobranca 
            Caption         =   "Cancelar Cobrança"
            Height          =   375
            Index           =   12
            Left            =   -72360
            TabIndex        =   21
            Top             =   960
            Width           =   2415
         End
         Begin VB.CommandButton btnRevisarCobranca 
            Caption         =   "Revisar Cobrança"
            Height          =   375
            Index           =   11
            Left            =   -74880
            TabIndex        =   20
            Top             =   960
            Width           =   2415
         End
         Begin VB.CommandButton btnConsultarCobranca 
            Caption         =   "Consultar Cobrança"
            Height          =   375
            Index           =   10
            Left            =   -72360
            TabIndex        =   19
            Top             =   480
            Width           =   2415
         End
         Begin VB.CommandButton btnCancelarCobrancaImediata 
            Caption         =   "Cancelar Cobrança Imediata"
            Height          =   375
            Index           =   9
            Left            =   -72360
            TabIndex        =   18
            Top             =   960
            Width           =   2415
         End
         Begin VB.CommandButton btnRevisarCobrancaImediata 
            Caption         =   "Revisar Cobrança Imediata"
            Height          =   375
            Index           =   8
            Left            =   -74880
            TabIndex        =   17
            Top             =   960
            Width           =   2415
         End
         Begin VB.CommandButton btnConsultarCobrancaImediata 
            Caption         =   "Consultar Cobrança Imediata"
            Height          =   375
            Index           =   7
            Left            =   -72360
            TabIndex        =   16
            Top             =   480
            Width           =   2415
         End
         Begin VB.CommandButton btnCriarCobrancaImediata 
            Caption         =   "Criar Cobrança Imediata"
            Height          =   375
            Index           =   6
            Left            =   -74880
            TabIndex        =   15
            Top             =   480
            Width           =   2415
         End
         Begin VB.CommandButton btnConsultarDevolucaoPix 
            Caption         =   "Consultar Devolução Pix"
            Height          =   375
            Index           =   5
            Left            =   -72360
            TabIndex        =   14
            Top             =   960
            Width           =   2415
         End
         Begin VB.CommandButton btnSolicitarDevolucaoPix 
            Caption         =   "Solicitar Devolução Pix"
            Height          =   375
            Index           =   4
            Left            =   -74880
            TabIndex        =   13
            Top             =   960
            Width           =   2415
         End
         Begin VB.CommandButton btnConsultarPixRecebidos 
            Caption         =   "Consultar Pix Recebidos"
            Height          =   375
            Index           =   3
            Left            =   -72360
            TabIndex        =   12
            Top             =   480
            Width           =   2415
         End
         Begin VB.CommandButton btnConsultarPix 
            Caption         =   "Consultar Pix"
            Height          =   375
            Index           =   2
            Left            =   -74880
            TabIndex        =   11
            Top             =   480
            Width           =   2415
         End
         Begin VB.CommandButton btnGerarQRCodeEstatico 
            Caption         =   "Gerar QRCode Estático"
            Height          =   375
            Index           =   1
            Left            =   120
            TabIndex        =   10
            Top             =   480
            Width           =   2415
         End
         Begin VB.CommandButton btnCriarCobranca 
            Caption         =   "Criar Cobrança"
            Height          =   375
            Index           =   0
            Left            =   -74880
            TabIndex        =   9
            Top             =   480
            Width           =   2415
         End
      End
   End
   Begin MSComDlg.CommonDialog CommonDialog1 
      Left            =   7440
      Top             =   9720
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
   End
   Begin VB.Frame FraRespostas 
      Caption         =   "Respostas"
      Height          =   7335
      Left            =   7440
      TabIndex        =   0
      Top             =   2280
      Width           =   6135
      Begin VB.TextBox rtbRespostas 
         Height          =   6975
         Left            =   120
         Locked          =   -1  'True
         MultiLine       =   -1  'True
         ScrollBars      =   3  'Both
         TabIndex        =   1
         Top             =   240
         Width           =   5895
      End
   End
   Begin TabDlg.SSTab SSTTab0 
      Height          =   9495
      Index           =   0
      Left            =   120
      TabIndex        =   5
      Top             =   120
      Width           =   7215
      _ExtentX        =   12726
      _ExtentY        =   16748
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
      TabPicture(0)   =   "FrmMain.frx":008C
      Tab(0).ControlEnabled=   -1  'True
      Tab(0).Control(0)=   "SSTab2(0)"
      Tab(0).Control(0).Enabled=   0   'False
      Tab(0).ControlCount=   1
      Begin TabDlg.SSTab SSTab2 
         Height          =   9015
         Index           =   0
         Left            =   0
         TabIndex        =   6
         Top             =   360
         Width           =   7095
         _ExtentX        =   12515
         _ExtentY        =   15901
         _Version        =   393216
         Style           =   1
         Tabs            =   2
         TabsPerRow      =   2
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
         TabCaption(0)   =   "Configurações PIX"
         TabPicture(0)   =   "FrmMain.frx":00A8
         Tab(0).ControlEnabled=   -1  'True
         Tab(0).Control(0)=   "frmRecebedor"
         Tab(0).Control(0).Enabled=   0   'False
         Tab(0).Control(1)=   "Frame1"
         Tab(0).Control(1).Enabled=   0   'False
         Tab(0).Control(2)=   "Frame2"
         Tab(0).Control(2).Enabled=   0   'False
         Tab(0).Control(3)=   "Frame3"
         Tab(0).Control(3).Enabled=   0   'False
         Tab(0).ControlCount=   4
         TabCaption(1)   =   "Configurações PSP"
         TabPicture(1)   =   "FrmMain.frx":00C4
         Tab(1).ControlEnabled=   0   'False
         Tab(1).Control(0)=   "SSTab2(1)"
         Tab(1).ControlCount=   1
         Begin VB.Frame Frame3 
            Caption         =   "Log PSP"
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
            Left            =   3840
            TabIndex        =   43
            Top             =   4200
            Width           =   3135
            Begin VB.ComboBox cmbNivelLogPSP 
               Height          =   315
               ItemData        =   "FrmMain.frx":00E0
               Left            =   120
               List            =   "FrmMain.frx":00F3
               Style           =   2  'Dropdown List
               TabIndex        =   55
               Top             =   1080
               Width           =   2895
            End
            Begin VB.TextBox txtArqLogPSP 
               Height          =   285
               Left            =   120
               TabIndex        =   44
               Top             =   480
               Width           =   2955
            End
            Begin VB.Label Label8 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Arquivo"
               BeginProperty Font 
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
               Width           =   660
            End
            Begin VB.Label lblNivelLogPSP 
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
               Index           =   3
               Left            =   120
               TabIndex        =   45
               Top             =   840
               Width           =   570
            End
         End
         Begin VB.Frame Frame2 
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
            Left            =   120
            TabIndex        =   36
            Top             =   4200
            Width           =   3615
            Begin VB.TextBox txtProxyPorta 
               Alignment       =   1  'Right Justify
               Height          =   285
               Left            =   2760
               TabIndex        =   53
               Text            =   "0"
               Top             =   480
               Width           =   540
            End
            Begin VB.TextBox txtProxyServidor 
               Height          =   285
               Left            =   120
               TabIndex        =   39
               Top             =   480
               Width           =   2595
            End
            Begin VB.TextBox txtProxyUsuario 
               Height          =   285
               IMEMode         =   3  'DISABLE
               Left            =   120
               PasswordChar    =   "*"
               TabIndex        =   38
               Top             =   1080
               Width           =   3435
            End
            Begin VB.TextBox txtProxySenha 
               Height          =   285
               Left            =   120
               TabIndex        =   37
               Top             =   1680
               Width           =   3435
            End
            Begin MSComCtl2.UpDown nudProxyPorta 
               Height          =   285
               Left            =   3300
               TabIndex        =   54
               Top             =   480
               Width           =   255
               _ExtentX        =   450
               _ExtentY        =   503
               _Version        =   393216
               Value           =   5000
               BuddyControl    =   "txtProxyPorta"
               BuddyDispid     =   196632
               OrigLeft        =   3240
               OrigTop         =   480
               OrigRight       =   3495
               OrigBottom      =   765
               Max             =   99999
               Enabled         =   -1  'True
            End
            Begin VB.Label lblPortaProxy 
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
               Left            =   2760
               TabIndex        =   52
               Top             =   240
               Width           =   465
            End
            Begin VB.Label Label5 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Host"
               BeginProperty Font 
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
               TabIndex        =   42
               Top             =   240
               Width           =   390
            End
            Begin VB.Label lblProxyUsuario 
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
               Index           =   2
               Left            =   120
               TabIndex        =   41
               Top             =   840
               Width           =   645
            End
            Begin VB.Label Label4 
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
               TabIndex        =   40
               Top             =   1440
               Width           =   525
            End
         End
         Begin VB.Frame Frame1 
            Caption         =   "PSP"
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
            Left            =   120
            TabIndex        =   31
            Top             =   2280
            Width           =   6855
            Begin VB.TextBox txtTimeoutPSP 
               Alignment       =   1  'Right Justify
               Height          =   285
               Left            =   3480
               TabIndex        =   50
               Text            =   "0"
               Top             =   1080
               Width           =   2970
            End
            Begin VB.ComboBox cmbAmbiente 
               Height          =   315
               ItemData        =   "FrmMain.frx":0120
               Left            =   120
               List            =   "FrmMain.frx":012D
               Style           =   2  'Dropdown List
               TabIndex        =   49
               Top             =   1080
               Width           =   3255
            End
            Begin VB.ComboBox cmbTipoChave 
               Height          =   315
               ItemData        =   "FrmMain.frx":0158
               Left            =   3480
               List            =   "FrmMain.frx":016E
               Style           =   2  'Dropdown List
               TabIndex        =   48
               Top             =   480
               Width           =   3255
            End
            Begin VB.ComboBox cmbPSP 
               Height          =   315
               ItemData        =   "FrmMain.frx":01B3
               Left            =   120
               List            =   "FrmMain.frx":01E4
               Style           =   2  'Dropdown List
               TabIndex        =   47
               Top             =   480
               Width           =   3255
            End
            Begin MSComCtl2.UpDown nudTimeoutPSP 
               Height          =   285
               Left            =   6480
               TabIndex        =   51
               Top             =   1080
               Width           =   255
               _ExtentX        =   450
               _ExtentY        =   503
               _Version        =   393216
               Value           =   5000
               BuddyControl    =   "txtTimeoutPSP"
               BuddyDispid     =   196641
               OrigLeft        =   6480
               OrigTop         =   1080
               OrigRight       =   6735
               OrigBottom      =   1365
               Max             =   99999
               Enabled         =   -1  'True
            End
            Begin VB.Label Label3 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Tipo Chave"
               BeginProperty Font 
                  Name            =   "Tahoma"
                  Size            =   8.25
                  Charset         =   0
                  Weight          =   700
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   195
               Left            =   3480
               TabIndex        =   35
               Top             =   240
               Width           =   930
            End
            Begin VB.Label Label2 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "PSP"
               BeginProperty Font 
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
               TabIndex        =   34
               Top             =   240
               Width           =   315
            End
            Begin VB.Label lblAmbiente 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
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
               Height          =   195
               Index           =   0
               Left            =   120
               TabIndex        =   33
               Top             =   840
               Width           =   825
            End
            Begin VB.Label Label1 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Timeout"
               BeginProperty Font 
                  Name            =   "Tahoma"
                  Size            =   8.25
                  Charset         =   0
                  Weight          =   700
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   195
               Left            =   3480
               TabIndex        =   32
               Top             =   840
               Width           =   705
            End
         End
         Begin VB.Frame frmRecebedor 
            Caption         =   "Recebedor"
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
            Left            =   120
            TabIndex        =   22
            Top             =   480
            Width           =   6855
            Begin VB.TextBox txtUFRecebedor 
               Height          =   285
               Left            =   4560
               TabIndex        =   26
               Top             =   1080
               Width           =   2115
            End
            Begin VB.TextBox txtCidadeRecebedor 
               Height          =   285
               IMEMode         =   3  'DISABLE
               Left            =   120
               TabIndex        =   25
               Top             =   1080
               Width           =   4395
            End
            Begin VB.TextBox txtNomeRecebedor 
               Height          =   285
               Left            =   120
               TabIndex        =   24
               Top             =   480
               Width           =   4395
            End
            Begin VB.TextBox txtCEPRecebedor 
               Height          =   285
               Left            =   4560
               TabIndex        =   23
               Top             =   480
               Width           =   2115
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
               Left            =   4560
               TabIndex        =   30
               Top             =   840
               Width           =   210
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
               Index           =   1
               Left            =   120
               TabIndex        =   29
               Top             =   840
               Width           =   570
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
               TabIndex        =   28
               Top             =   240
               Width           =   480
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
               Left            =   4560
               TabIndex        =   27
               Top             =   240
               Width           =   300
            End
         End
         Begin TabDlg.SSTab SSTab2 
            Height          =   8535
            Index           =   1
            Left            =   -75000
            TabIndex        =   7
            Top             =   360
            Width           =   6945
            _ExtentX        =   12250
            _ExtentY        =   15055
            _Version        =   393216
            Style           =   1
            Tabs            =   15
            TabsPerRow      =   9
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
            TabCaption(0)   =   "Bradesco"
            TabPicture(0)   =   "FrmMain.frx":02A0
            Tab(0).ControlEnabled=   -1  'True
            Tab(0).Control(0)=   "Label6"
            Tab(0).Control(0).Enabled=   0   'False
            Tab(0).Control(1)=   "Label7"
            Tab(0).Control(1).Enabled=   0   'False
            Tab(0).Control(2)=   "Label9"
            Tab(0).Control(2).Enabled=   0   'False
            Tab(0).Control(3)=   "Label11"
            Tab(0).Control(3).Enabled=   0   'False
            Tab(0).Control(4)=   "Label12"
            Tab(0).Control(4).Enabled=   0   'False
            Tab(0).Control(5)=   "txtChavePIXBradesco"
            Tab(0).Control(5).Enabled=   0   'False
            Tab(0).Control(6)=   "txtClientIDBradesco"
            Tab(0).Control(6).Enabled=   0   'False
            Tab(0).Control(7)=   "txtClientSecretBradesco"
            Tab(0).Control(7).Enabled=   0   'False
            Tab(0).Control(8)=   "txtSenhaCertificadoBradesco"
            Tab(0).Control(8).Enabled=   0   'False
            Tab(0).Control(9)=   "txtArquivoPFXBradesco"
            Tab(0).Control(9).Enabled=   0   'False
            Tab(0).Control(10)=   "btnCertificadoBradesco"
            Tab(0).Control(10).Enabled=   0   'False
            Tab(0).ControlCount=   11
            TabCaption(1)   =   "Sicredi"
            TabPicture(1)   =   "FrmMain.frx":02BC
            Tab(1).ControlEnabled=   0   'False
            Tab(1).Control(0)=   "btnArquivoCertificadoSicredi"
            Tab(1).Control(1)=   "txtArquivoCertificadoSicredi"
            Tab(1).Control(2)=   "btnArquivoChavePrivadaSicredi"
            Tab(1).Control(3)=   "txtArquivoChavePrivadaSicredi"
            Tab(1).Control(4)=   "txtClientSecretSicredi"
            Tab(1).Control(5)=   "txtClientIDSicredi"
            Tab(1).Control(6)=   "txtChavePIXSicredi"
            Tab(1).Control(7)=   "Label17"
            Tab(1).Control(8)=   "Label16"
            Tab(1).Control(9)=   "Label15"
            Tab(1).Control(10)=   "Label14"
            Tab(1).Control(11)=   "Label13"
            Tab(1).ControlCount=   12
            TabCaption(2)   =   "Sicoob"
            TabPicture(2)   =   "FrmMain.frx":02D8
            Tab(2).ControlEnabled=   0   'False
            Tab(2).Control(0)=   "btnArquivoCertificadoSicoob"
            Tab(2).Control(1)=   "txtArquivoCertificadoSicoob"
            Tab(2).Control(2)=   "btnArquivoChavePrivadaSicoob"
            Tab(2).Control(3)=   "txtArquivoChavePrivadaSicoob"
            Tab(2).Control(4)=   "txtTokenSandboxSicoob"
            Tab(2).Control(5)=   "txtClientIDSicoob"
            Tab(2).Control(6)=   "txtChavePIXSicoob"
            Tab(2).Control(7)=   "Label22"
            Tab(2).Control(8)=   "Label21"
            Tab(2).Control(9)=   "Label20"
            Tab(2).Control(10)=   "Label19"
            Tab(2).Control(11)=   "Label18"
            Tab(2).ControlCount=   12
            TabCaption(3)   =   "Shipay"
            TabPicture(3)   =   "FrmMain.frx":02F4
            Tab(3).ControlEnabled=   0   'False
            Tab(3).Control(0)=   "txtAccessKeyShipay"
            Tab(3).Control(1)=   "txtSecretKeyShipay"
            Tab(3).Control(2)=   "txtClientIDShipay"
            Tab(3).Control(3)=   "Label25"
            Tab(3).Control(4)=   "Label24"
            Tab(3).Control(5)=   "Label23"
            Tab(3).ControlCount=   6
            TabCaption(4)   =   "Santander"
            TabPicture(4)   =   "FrmMain.frx":0310
            Tab(4).ControlEnabled=   0   'False
            Tab(4).Control(0)=   "btnArquivoCertificadoPFXSantander"
            Tab(4).Control(1)=   "txtArquivoCertificadoPFXSantander"
            Tab(4).Control(2)=   "txtSenhaCertificadoPFXSantander"
            Tab(4).Control(3)=   "txtConsumerSecretSantander"
            Tab(4).Control(4)=   "txtConsumerKeySantander"
            Tab(4).Control(5)=   "txtChavePIXSantander"
            Tab(4).Control(6)=   "Label30"
            Tab(4).Control(7)=   "Label29"
            Tab(4).Control(8)=   "Label28"
            Tab(4).Control(9)=   "Label27"
            Tab(4).Control(10)=   "Label26"
            Tab(4).ControlCount=   11
            TabCaption(5)   =   "PixPDV"
            TabPicture(5)   =   "FrmMain.frx":032C
            Tab(5).ControlEnabled=   0   'False
            Tab(5).Control(0)=   "txtSecretKeyPixPDV"
            Tab(5).Control(1)=   "txtPixPDVToken"
            Tab(5).Control(2)=   "txtCNPJPixPDV"
            Tab(5).Control(3)=   "Label33"
            Tab(5).Control(4)=   "Label32"
            Tab(5).Control(5)=   "Label31"
            Tab(5).ControlCount=   6
            TabCaption(6)   =   "PagSeguro"
            TabPicture(6)   =   "FrmMain.frx":0348
            Tab(6).ControlEnabled=   0   'False
            Tab(6).Control(0)=   "btnArquivoCertificadoPagSeguro"
            Tab(6).Control(1)=   "txtArquivoCertificadoPagSeguro"
            Tab(6).Control(2)=   "btnArquivoChavePrivadaPagSeguro"
            Tab(6).Control(3)=   "txtArquivoChavePrivadaPagSeguro"
            Tab(6).Control(4)=   "txtClientSecretPagSeguro"
            Tab(6).Control(5)=   "txtClientIDPagSeguro"
            Tab(6).Control(6)=   "txtChavePIXPagSeguro"
            Tab(6).Control(7)=   "Label38"
            Tab(6).Control(8)=   "Label37"
            Tab(6).Control(9)=   "Label36"
            Tab(6).Control(10)=   "Label35"
            Tab(6).Control(11)=   "Label34"
            Tab(6).ControlCount=   12
            TabCaption(7)   =   "Itau"
            TabPicture(7)   =   "FrmMain.frx":0364
            Tab(7).ControlEnabled=   0   'False
            Tab(7).Control(0)=   "btnArquivoCertificadoItau"
            Tab(7).Control(1)=   "txtArquivoCertificadoItau"
            Tab(7).Control(2)=   "btnArquivoChavePrivadaItau"
            Tab(7).Control(3)=   "txtArquivoChavePrivadaItau"
            Tab(7).Control(4)=   "txtClientSecretItau"
            Tab(7).Control(5)=   "txtClientIDItau"
            Tab(7).Control(6)=   "txtChavePIXItau"
            Tab(7).Control(7)=   "Label43"
            Tab(7).Control(8)=   "Label42"
            Tab(7).Control(9)=   "Label41"
            Tab(7).Control(10)=   "Label40"
            Tab(7).Control(11)=   "Label39"
            Tab(7).ControlCount=   12
            TabCaption(8)   =   "Inter"
            TabPicture(8)   =   "FrmMain.frx":0380
            Tab(8).ControlEnabled=   0   'False
            Tab(8).Control(0)=   "btnArquivoCertificadoInter"
            Tab(8).Control(1)=   "txtArquivoCertificadoInter"
            Tab(8).Control(2)=   "btnArquivoChavePrivadaInter"
            Tab(8).Control(3)=   "txtArquivoChavePrivadaInter"
            Tab(8).Control(4)=   "txtClientSecretInter"
            Tab(8).Control(5)=   "txtClientIDInter"
            Tab(8).Control(6)=   "txtChavePIXInter"
            Tab(8).Control(7)=   "Label48"
            Tab(8).Control(8)=   "Label47"
            Tab(8).Control(9)=   "Label46"
            Tab(8).Control(10)=   "Label45"
            Tab(8).Control(11)=   "Label44"
            Tab(8).ControlCount=   12
            TabCaption(9)   =   "GerenciaNet"
            TabPicture(9)   =   "FrmMain.frx":039C
            Tab(9).ControlEnabled=   0   'False
            Tab(9).Control(0)=   "Label49"
            Tab(9).Control(1)=   "Label50"
            Tab(9).Control(2)=   "Label51"
            Tab(9).Control(3)=   "Label52"
            Tab(9).Control(4)=   "txtChavePIXGerenciaNet"
            Tab(9).Control(5)=   "txtClientIDGerenciaNet"
            Tab(9).Control(6)=   "txtClientSecretGerenciaNet"
            Tab(9).Control(7)=   "txtArquivoCertificadoGerenciaNet"
            Tab(9).Control(8)=   "btnArquivoCertificadoGerenciaNet"
            Tab(9).ControlCount=   9
            TabCaption(10)  =   "BancoBrasil"
            TabPicture(10)  =   "FrmMain.frx":03B8
            Tab(10).ControlEnabled=   0   'False
            Tab(10).Control(0)=   "cmbBBAPIVersao"
            Tab(10).Control(1)=   "txtSenhaPFXBancoBrasil"
            Tab(10).Control(2)=   "btnArquivoPXFBancoBrasil"
            Tab(10).Control(3)=   "txtArquivoPXFBancoBrasil"
            Tab(10).Control(4)=   "btnArquivoCertificadoBancoBrasil"
            Tab(10).Control(5)=   "txtArquivoCertificadoBancoBrasil"
            Tab(10).Control(6)=   "btnArquivoChavePrivadaBancoBrasil"
            Tab(10).Control(7)=   "txtArquivoChavePrivadaBancoBrasil"
            Tab(10).Control(8)=   "txtDeveloperApplicationKeyBancoBrasil"
            Tab(10).Control(9)=   "txtClientSecretBancoBrasil"
            Tab(10).Control(10)=   "txtClientIDBancoBrasil"
            Tab(10).Control(11)=   "txtChavePIXBancoBrasil"
            Tab(10).Control(12)=   "lblBBAPIVersao(0)"
            Tab(10).Control(13)=   "Label60"
            Tab(10).Control(14)=   "Label59"
            Tab(10).Control(15)=   "Label58"
            Tab(10).Control(16)=   "Label57"
            Tab(10).Control(17)=   "Label56"
            Tab(10).Control(18)=   "Label55"
            Tab(10).Control(19)=   "Label54"
            Tab(10).Control(20)=   "Label53"
            Tab(10).ControlCount=   21
            TabCaption(11)  =   "Ailos"
            TabPicture(11)  =   "FrmMain.frx":03D4
            Tab(11).ControlEnabled=   0   'False
            Tab(11).Control(0)=   "btnArquivoCeriticadoRootAilos"
            Tab(11).Control(1)=   "txtArquivoCeriticadoRootAilos"
            Tab(11).Control(2)=   "btnArquivoCertificadoAilos"
            Tab(11).Control(3)=   "txtArquivoCertificadoAilos"
            Tab(11).Control(4)=   "btnArquivoChavePrivadaAilos"
            Tab(11).Control(5)=   "txtArquivoChavePrivadaAilos"
            Tab(11).Control(6)=   "txtClientSecretAilos"
            Tab(11).Control(7)=   "txtClientIDAilos"
            Tab(11).Control(8)=   "txtChavePIXAilos"
            Tab(11).Control(9)=   "Label66"
            Tab(11).Control(10)=   "Label65"
            Tab(11).Control(11)=   "Label64"
            Tab(11).Control(12)=   "Label63"
            Tab(11).Control(13)=   "Label62"
            Tab(11).Control(14)=   "Label61"
            Tab(11).ControlCount=   15
            TabCaption(12)  =   "Matera"
            TabPicture(12)  =   "FrmMain.frx":03F0
            Tab(12).ControlEnabled=   0   'False
            Tab(12).Control(0)=   "Label67"
            Tab(12).Control(1)=   "Label68"
            Tab(12).Control(2)=   "Label69"
            Tab(12).Control(3)=   "Label70"
            Tab(12).Control(4)=   "Label71"
            Tab(12).Control(5)=   "Label72"
            Tab(12).Control(6)=   "Label73"
            Tab(12).Control(7)=   "Label74"
            Tab(12).Control(8)=   "txtChavePIXMatera"
            Tab(12).Control(9)=   "txtClientIDMatera"
            Tab(12).Control(10)=   "txtClientSecretMatera"
            Tab(12).Control(11)=   "txtSecretKeyMatera"
            Tab(12).Control(12)=   "txtAccountIDMatera"
            Tab(12).Control(13)=   "txtMediatorFeeMatera"
            Tab(12).Control(14)=   "txtArquivoChavePrivadaMatera"
            Tab(12).Control(15)=   "btnArquivoChavePrivadaMatera"
            Tab(12).Control(16)=   "txtArquivoCertificadoMatera"
            Tab(12).Control(17)=   "btnArquivoCertificadoMatera"
            Tab(12).ControlCount=   18
            TabCaption(13)  =   "Cielo"
            TabPicture(13)  =   "FrmMain.frx":040C
            Tab(13).ControlEnabled=   0   'False
            Tab(13).Control(0)=   "txtClientSecretCielo"
            Tab(13).Control(1)=   "txtClientIDCielo"
            Tab(13).Control(2)=   "txtChavePIXCielo"
            Tab(13).Control(3)=   "Label77"
            Tab(13).Control(4)=   "Label76"
            Tab(13).Control(5)=   "Label75"
            Tab(13).ControlCount=   6
            TabCaption(14)  =   "MercadoPago"
            TabPicture(14)  =   "FrmMain.frx":0428
            Tab(14).ControlEnabled=   0   'False
            Tab(14).Control(0)=   "txtAccessTokenMercadoPago"
            Tab(14).Control(1)=   "txtChavePIXMercadoPago"
            Tab(14).Control(2)=   "Label79"
            Tab(14).Control(3)=   "Label78"
            Tab(14).ControlCount=   4
            Begin VB.TextBox txtAccessTokenMercadoPago 
               Height          =   285
               Left            =   -74880
               TabIndex        =   222
               Top             =   1560
               Width           =   6555
            End
            Begin VB.TextBox txtChavePIXMercadoPago 
               Height          =   285
               Left            =   -74880
               TabIndex        =   220
               Top             =   960
               Width           =   6555
            End
            Begin VB.TextBox txtClientSecretCielo 
               Height          =   285
               Left            =   -74880
               TabIndex        =   218
               Top             =   2160
               Width           =   6555
            End
            Begin VB.TextBox txtClientIDCielo 
               Height          =   285
               Left            =   -74880
               TabIndex        =   216
               Top             =   1560
               Width           =   6555
            End
            Begin VB.TextBox txtChavePIXCielo 
               Height          =   285
               Left            =   -74880
               TabIndex        =   214
               Top             =   960
               Width           =   6555
            End
            Begin VB.CommandButton btnArquivoCertificadoMatera 
               Caption         =   "..."
               Height          =   260
               Left            =   -68760
               TabIndex        =   213
               Top             =   4560
               Width           =   390
            End
            Begin VB.TextBox txtArquivoCertificadoMatera 
               Height          =   285
               Left            =   -74880
               TabIndex        =   212
               Top             =   4560
               Width           =   6075
            End
            Begin VB.CommandButton btnArquivoChavePrivadaMatera 
               Caption         =   "..."
               Height          =   260
               Left            =   -68760
               TabIndex        =   210
               Top             =   3960
               Width           =   390
            End
            Begin VB.TextBox txtArquivoChavePrivadaMatera 
               Height          =   285
               Left            =   -74880
               TabIndex        =   209
               Top             =   3960
               Width           =   6075
            End
            Begin VB.TextBox txtMediatorFeeMatera 
               Height          =   285
               Left            =   -71760
               TabIndex        =   206
               Top             =   3360
               Width           =   3435
            End
            Begin VB.TextBox txtAccountIDMatera 
               Height          =   285
               Left            =   -74880
               TabIndex        =   204
               Top             =   3360
               Width           =   3075
            End
            Begin VB.TextBox txtSecretKeyMatera 
               Height          =   285
               Left            =   -74880
               TabIndex        =   202
               Top             =   2760
               Width           =   6555
            End
            Begin VB.TextBox txtClientSecretMatera 
               Height          =   285
               Left            =   -74880
               TabIndex        =   200
               Top             =   2160
               Width           =   6555
            End
            Begin VB.TextBox txtClientIDMatera 
               Height          =   285
               Left            =   -74880
               TabIndex        =   198
               Top             =   1560
               Width           =   6555
            End
            Begin VB.TextBox txtChavePIXMatera 
               Height          =   285
               Left            =   -74880
               TabIndex        =   196
               Top             =   960
               Width           =   6555
            End
            Begin VB.CommandButton btnArquivoCeriticadoRootAilos 
               Caption         =   "..."
               Height          =   260
               Left            =   -68760
               TabIndex        =   195
               Top             =   3960
               Width           =   390
            End
            Begin VB.TextBox txtArquivoCeriticadoRootAilos 
               Height          =   285
               Left            =   -74880
               TabIndex        =   194
               Top             =   3960
               Width           =   6075
            End
            Begin VB.CommandButton btnArquivoCertificadoAilos 
               Caption         =   "..."
               Height          =   260
               Left            =   -68760
               TabIndex        =   192
               Top             =   3360
               Width           =   390
            End
            Begin VB.TextBox txtArquivoCertificadoAilos 
               Height          =   285
               Left            =   -74880
               TabIndex        =   191
               Top             =   3360
               Width           =   6075
            End
            Begin VB.CommandButton btnArquivoChavePrivadaAilos 
               Caption         =   "..."
               Height          =   260
               Left            =   -68760
               TabIndex        =   189
               Top             =   2760
               Width           =   390
            End
            Begin VB.TextBox txtArquivoChavePrivadaAilos 
               Height          =   285
               Left            =   -74880
               TabIndex        =   188
               Top             =   2760
               Width           =   6075
            End
            Begin VB.TextBox txtClientSecretAilos 
               Height          =   285
               Left            =   -74880
               TabIndex        =   185
               Top             =   2160
               Width           =   6555
            End
            Begin VB.TextBox txtClientIDAilos 
               Height          =   285
               Left            =   -74880
               TabIndex        =   183
               Top             =   1560
               Width           =   6555
            End
            Begin VB.TextBox txtChavePIXAilos 
               Height          =   285
               Left            =   -74880
               TabIndex        =   181
               Top             =   960
               Width           =   6555
            End
            Begin VB.ComboBox cmbBBAPIVersao 
               Height          =   315
               ItemData        =   "FrmMain.frx":0444
               Left            =   -71280
               List            =   "FrmMain.frx":044E
               Style           =   2  'Dropdown List
               TabIndex        =   179
               Top             =   1560
               Width           =   2895
            End
            Begin VB.TextBox txtSenhaPFXBancoBrasil 
               Height          =   285
               Left            =   -74880
               TabIndex        =   177
               Top             =   5160
               Width           =   6555
            End
            Begin VB.CommandButton btnArquivoPXFBancoBrasil 
               Caption         =   "..."
               Height          =   260
               Left            =   -68760
               TabIndex        =   176
               Top             =   4560
               Width           =   390
            End
            Begin VB.TextBox txtArquivoPXFBancoBrasil 
               Height          =   285
               Left            =   -74880
               TabIndex        =   175
               Top             =   4560
               Width           =   6075
            End
            Begin VB.CommandButton btnArquivoCertificadoBancoBrasil 
               Caption         =   "..."
               Height          =   260
               Left            =   -68760
               TabIndex        =   173
               Top             =   3960
               Width           =   390
            End
            Begin VB.TextBox txtArquivoCertificadoBancoBrasil 
               Height          =   285
               Left            =   -74880
               TabIndex        =   172
               Top             =   3960
               Width           =   6075
            End
            Begin VB.CommandButton btnArquivoChavePrivadaBancoBrasil 
               Caption         =   "..."
               Height          =   260
               Left            =   -68760
               TabIndex        =   170
               Top             =   3360
               Width           =   390
            End
            Begin VB.TextBox txtArquivoChavePrivadaBancoBrasil 
               Height          =   285
               Left            =   -74880
               TabIndex        =   169
               Top             =   3360
               Width           =   6075
            End
            Begin VB.TextBox txtDeveloperApplicationKeyBancoBrasil 
               Height          =   285
               Left            =   -74880
               TabIndex        =   166
               Top             =   2760
               Width           =   6555
            End
            Begin VB.TextBox txtClientSecretBancoBrasil 
               Height          =   285
               Left            =   -74880
               TabIndex        =   164
               Top             =   2160
               Width           =   6555
            End
            Begin VB.TextBox txtClientIDBancoBrasil 
               Height          =   285
               Left            =   -74880
               TabIndex        =   162
               Top             =   1560
               Width           =   3555
            End
            Begin VB.TextBox txtChavePIXBancoBrasil 
               Height          =   285
               Left            =   -74880
               TabIndex        =   160
               Top             =   960
               Width           =   6555
            End
            Begin VB.CommandButton btnArquivoCertificadoGerenciaNet 
               Caption         =   "..."
               Height          =   260
               Left            =   -68760
               TabIndex        =   159
               Top             =   2760
               Width           =   390
            End
            Begin VB.TextBox txtArquivoCertificadoGerenciaNet 
               Height          =   285
               Left            =   -74880
               TabIndex        =   158
               Top             =   2760
               Width           =   6075
            End
            Begin VB.TextBox txtClientSecretGerenciaNet 
               Height          =   285
               Left            =   -74880
               TabIndex        =   155
               Top             =   2160
               Width           =   6555
            End
            Begin VB.TextBox txtClientIDGerenciaNet 
               Height          =   285
               Left            =   -74880
               TabIndex        =   153
               Top             =   1560
               Width           =   6555
            End
            Begin VB.TextBox txtChavePIXGerenciaNet 
               Height          =   285
               Left            =   -74880
               TabIndex        =   151
               Top             =   960
               Width           =   6555
            End
            Begin VB.CommandButton btnArquivoCertificadoInter 
               Caption         =   "..."
               Height          =   260
               Left            =   -68760
               TabIndex        =   150
               Top             =   3360
               Width           =   390
            End
            Begin VB.TextBox txtArquivoCertificadoInter 
               Height          =   285
               Left            =   -74880
               TabIndex        =   149
               Top             =   3360
               Width           =   6075
            End
            Begin VB.CommandButton btnArquivoChavePrivadaInter 
               Caption         =   "..."
               Height          =   260
               Left            =   -68760
               TabIndex        =   147
               Top             =   2760
               Width           =   390
            End
            Begin VB.TextBox txtArquivoChavePrivadaInter 
               Height          =   285
               Left            =   -74880
               TabIndex        =   146
               Top             =   2760
               Width           =   6075
            End
            Begin VB.TextBox txtClientSecretInter 
               Height          =   285
               Left            =   -74880
               TabIndex        =   143
               Top             =   2160
               Width           =   6555
            End
            Begin VB.TextBox txtClientIDInter 
               Height          =   285
               Left            =   -74880
               TabIndex        =   141
               Top             =   1560
               Width           =   6555
            End
            Begin VB.TextBox txtChavePIXInter 
               Height          =   285
               Left            =   -74880
               TabIndex        =   139
               Top             =   960
               Width           =   6555
            End
            Begin VB.CommandButton btnArquivoCertificadoItau 
               Caption         =   "..."
               Height          =   260
               Left            =   -68760
               TabIndex        =   138
               Top             =   3360
               Width           =   390
            End
            Begin VB.TextBox txtArquivoCertificadoItau 
               Height          =   285
               Left            =   -74880
               TabIndex        =   137
               Top             =   3360
               Width           =   6075
            End
            Begin VB.CommandButton btnArquivoChavePrivadaItau 
               Caption         =   "..."
               Height          =   260
               Left            =   -68760
               TabIndex        =   135
               Top             =   2760
               Width           =   390
            End
            Begin VB.TextBox txtArquivoChavePrivadaItau 
               Height          =   285
               Left            =   -74880
               TabIndex        =   134
               Top             =   2760
               Width           =   6075
            End
            Begin VB.TextBox txtClientSecretItau 
               Height          =   285
               Left            =   -74880
               TabIndex        =   131
               Top             =   2160
               Width           =   6555
            End
            Begin VB.TextBox txtClientIDItau 
               Height          =   285
               Left            =   -74880
               TabIndex        =   129
               Top             =   1560
               Width           =   6555
            End
            Begin VB.TextBox txtChavePIXItau 
               Height          =   285
               Left            =   -74880
               TabIndex        =   127
               Top             =   960
               Width           =   6555
            End
            Begin VB.CommandButton btnArquivoCertificadoPagSeguro 
               Caption         =   "..."
               Height          =   260
               Left            =   -68760
               TabIndex        =   126
               Top             =   3360
               Width           =   390
            End
            Begin VB.TextBox txtArquivoCertificadoPagSeguro 
               Height          =   285
               Left            =   -74880
               TabIndex        =   125
               Top             =   3360
               Width           =   6075
            End
            Begin VB.CommandButton btnArquivoChavePrivadaPagSeguro 
               Caption         =   "..."
               Height          =   260
               Left            =   -68760
               TabIndex        =   123
               Top             =   2760
               Width           =   390
            End
            Begin VB.TextBox txtArquivoChavePrivadaPagSeguro 
               Height          =   285
               Left            =   -74880
               TabIndex        =   122
               Top             =   2760
               Width           =   6075
            End
            Begin VB.TextBox txtClientSecretPagSeguro 
               Height          =   285
               Left            =   -74880
               TabIndex        =   119
               Top             =   2160
               Width           =   6555
            End
            Begin VB.TextBox txtClientIDPagSeguro 
               Height          =   285
               Left            =   -74880
               TabIndex        =   117
               Top             =   1560
               Width           =   6555
            End
            Begin VB.TextBox txtChavePIXPagSeguro 
               Height          =   285
               Left            =   -74880
               TabIndex        =   115
               Top             =   960
               Width           =   6555
            End
            Begin VB.TextBox txtSecretKeyPixPDV 
               Height          =   285
               Left            =   -74880
               TabIndex        =   113
               Top             =   2160
               Width           =   6555
            End
            Begin VB.TextBox txtPixPDVToken 
               Height          =   285
               Left            =   -74880
               TabIndex        =   111
               Top             =   1560
               Width           =   6555
            End
            Begin VB.TextBox txtCNPJPixPDV 
               Height          =   285
               Left            =   -74880
               TabIndex        =   109
               Top             =   960
               Width           =   6555
            End
            Begin VB.CommandButton btnArquivoCertificadoPFXSantander 
               Caption         =   "..."
               Height          =   260
               Left            =   -68760
               TabIndex        =   108
               Top             =   2760
               Width           =   390
            End
            Begin VB.TextBox txtArquivoCertificadoPFXSantander 
               Height          =   285
               Left            =   -74880
               TabIndex        =   107
               Top             =   2760
               Width           =   6075
            End
            Begin VB.TextBox txtSenhaCertificadoPFXSantander 
               Height          =   285
               Left            =   -74880
               TabIndex        =   105
               Top             =   3360
               Width           =   6555
            End
            Begin VB.TextBox txtConsumerSecretSantander 
               Height          =   285
               Left            =   -74880
               TabIndex        =   102
               Top             =   2160
               Width           =   6555
            End
            Begin VB.TextBox txtConsumerKeySantander 
               Height          =   285
               Left            =   -74880
               TabIndex        =   100
               Top             =   1560
               Width           =   6555
            End
            Begin VB.TextBox txtChavePIXSantander 
               Height          =   285
               Left            =   -74880
               TabIndex        =   98
               Top             =   960
               Width           =   6555
            End
            Begin VB.TextBox txtAccessKeyShipay 
               Height          =   285
               Left            =   -74880
               TabIndex        =   96
               Top             =   2160
               Width           =   6555
            End
            Begin VB.TextBox txtSecretKeyShipay 
               Height          =   285
               Left            =   -74880
               TabIndex        =   94
               Top             =   1560
               Width           =   6555
            End
            Begin VB.TextBox txtClientIDShipay 
               Height          =   285
               Left            =   -74880
               TabIndex        =   92
               Top             =   960
               Width           =   6555
            End
            Begin VB.CommandButton btnArquivoCertificadoSicoob 
               Caption         =   "..."
               Height          =   260
               Left            =   -68760
               TabIndex        =   91
               Top             =   3360
               Width           =   390
            End
            Begin VB.TextBox txtArquivoCertificadoSicoob 
               Height          =   285
               Left            =   -74880
               TabIndex        =   90
               Top             =   3360
               Width           =   6075
            End
            Begin VB.CommandButton btnArquivoChavePrivadaSicoob 
               Caption         =   "..."
               Height          =   260
               Left            =   -68760
               TabIndex        =   88
               Top             =   2760
               Width           =   390
            End
            Begin VB.TextBox txtArquivoChavePrivadaSicoob 
               Height          =   285
               Left            =   -74880
               TabIndex        =   87
               Top             =   2760
               Width           =   6075
            End
            Begin VB.TextBox txtTokenSandboxSicoob 
               Height          =   285
               Left            =   -74880
               TabIndex        =   84
               Top             =   2160
               Width           =   6555
            End
            Begin VB.TextBox txtClientIDSicoob 
               Height          =   285
               Left            =   -74880
               TabIndex        =   82
               Top             =   1560
               Width           =   6555
            End
            Begin VB.TextBox txtChavePIXSicoob 
               Height          =   285
               Left            =   -74880
               TabIndex        =   80
               Top             =   960
               Width           =   6555
            End
            Begin VB.CommandButton btnArquivoCertificadoSicredi 
               Caption         =   "..."
               Height          =   260
               Left            =   -68760
               TabIndex        =   79
               Top             =   3360
               Width           =   390
            End
            Begin VB.TextBox txtArquivoCertificadoSicredi 
               Height          =   285
               Left            =   -74880
               TabIndex        =   78
               Top             =   3360
               Width           =   6075
            End
            Begin VB.CommandButton btnArquivoChavePrivadaSicredi 
               Caption         =   "..."
               Height          =   260
               Left            =   -68760
               TabIndex        =   76
               Top             =   2760
               Width           =   390
            End
            Begin VB.TextBox txtArquivoChavePrivadaSicredi 
               Height          =   285
               Left            =   -74880
               TabIndex        =   75
               Top             =   2760
               Width           =   6075
            End
            Begin VB.TextBox txtClientSecretSicredi 
               Height          =   285
               Left            =   -74880
               TabIndex        =   72
               Top             =   2160
               Width           =   6555
            End
            Begin VB.TextBox txtClientIDSicredi 
               Height          =   285
               Left            =   -74880
               TabIndex        =   70
               Top             =   1560
               Width           =   6555
            End
            Begin VB.TextBox txtChavePIXSicredi 
               Height          =   285
               Left            =   -74880
               TabIndex        =   68
               Top             =   960
               Width           =   6555
            End
            Begin VB.CommandButton btnCertificadoBradesco 
               Caption         =   "..."
               Height          =   260
               Left            =   6240
               TabIndex        =   67
               Top             =   2760
               Width           =   390
            End
            Begin VB.TextBox txtArquivoPFXBradesco 
               Height          =   285
               Left            =   120
               TabIndex        =   66
               Top             =   2760
               Width           =   6075
            End
            Begin VB.TextBox txtSenhaCertificadoBradesco 
               Height          =   285
               Left            =   120
               TabIndex        =   64
               Top             =   3360
               Width           =   6555
            End
            Begin VB.TextBox txtClientSecretBradesco 
               Height          =   285
               Left            =   120
               TabIndex        =   60
               Top             =   2160
               Width           =   6555
            End
            Begin VB.TextBox txtClientIDBradesco 
               Height          =   285
               Left            =   120
               TabIndex        =   58
               Top             =   1560
               Width           =   6555
            End
            Begin VB.TextBox txtChavePIXBradesco 
               Height          =   285
               Left            =   120
               TabIndex        =   56
               Top             =   960
               Width           =   6555
            End
            Begin VB.Label Label79 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Access Token"
               BeginProperty Font 
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
               TabIndex        =   223
               Top             =   1320
               Width           =   1155
            End
            Begin VB.Label Label78 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Chave PIX"
               BeginProperty Font 
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
               TabIndex        =   221
               Top             =   720
               Width           =   855
            End
            Begin VB.Label Label77 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Client Secret"
               BeginProperty Font 
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
               TabIndex        =   219
               Top             =   1920
               Width           =   1080
            End
            Begin VB.Label Label76 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Client ID"
               BeginProperty Font 
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
               TabIndex        =   217
               Top             =   1320
               Width           =   720
            End
            Begin VB.Label Label75 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Chave PIX"
               BeginProperty Font 
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
               TabIndex        =   215
               Top             =   720
               Width           =   855
            End
            Begin VB.Label Label74 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Arquivo Certificado"
               BeginProperty Font 
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
               TabIndex        =   211
               Top             =   4320
               Width           =   1620
            End
            Begin VB.Label Label73 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Arquivo Chave Privada"
               BeginProperty Font 
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
               TabIndex        =   208
               Top             =   3720
               Width           =   1920
            End
            Begin VB.Label Label72 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Mediator Fee"
               BeginProperty Font 
                  Name            =   "Tahoma"
                  Size            =   8.25
                  Charset         =   0
                  Weight          =   700
                  Underline       =   0   'False
                  Italic          =   0   'False
                  Strikethrough   =   0   'False
               EndProperty
               Height          =   195
               Left            =   -71760
               TabIndex        =   207
               Top             =   3120
               Width           =   1110
            End
            Begin VB.Label Label71 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Account ID"
               BeginProperty Font 
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
               TabIndex        =   205
               Top             =   3120
               Width           =   930
            End
            Begin VB.Label Label70 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Secret Key"
               BeginProperty Font 
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
               TabIndex        =   203
               Top             =   2520
               Width           =   915
            End
            Begin VB.Label Label69 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Client Secret"
               BeginProperty Font 
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
               TabIndex        =   201
               Top             =   1920
               Width           =   1080
            End
            Begin VB.Label Label68 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Client ID"
               BeginProperty Font 
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
               TabIndex        =   199
               Top             =   1320
               Width           =   720
            End
            Begin VB.Label Label67 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Chave PIX"
               BeginProperty Font 
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
               TabIndex        =   197
               Top             =   720
               Width           =   855
            End
            Begin VB.Label Label66 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Arquivo Certificado Root"
               BeginProperty Font 
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
               TabIndex        =   193
               Top             =   3720
               Width           =   2070
            End
            Begin VB.Label Label65 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Arquivo Certificado"
               BeginProperty Font 
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
               TabIndex        =   190
               Top             =   3120
               Width           =   1620
            End
            Begin VB.Label Label64 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Arquivo Chave Privada"
               BeginProperty Font 
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
               TabIndex        =   187
               Top             =   2520
               Width           =   1920
            End
            Begin VB.Label Label63 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Client Secret"
               BeginProperty Font 
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
               TabIndex        =   186
               Top             =   1920
               Width           =   1080
            End
            Begin VB.Label Label62 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Client ID"
               BeginProperty Font 
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
               TabIndex        =   184
               Top             =   1320
               Width           =   720
            End
            Begin VB.Label Label61 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Chave PIX"
               BeginProperty Font 
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
               TabIndex        =   182
               Top             =   720
               Width           =   855
            End
            Begin VB.Label lblBBAPIVersao 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "BB API Versão"
               BeginProperty Font 
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
               Left            =   -71280
               TabIndex        =   180
               Top             =   1320
               Width           =   1185
            End
            Begin VB.Label Label60 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Senha PFX"
               BeginProperty Font 
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
               TabIndex        =   178
               Top             =   4920
               Width           =   870
            End
            Begin VB.Label Label59 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Arquivo PFX"
               BeginProperty Font 
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
               TabIndex        =   174
               Top             =   4320
               Width           =   1005
            End
            Begin VB.Label Label58 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Arquivo Certificado"
               BeginProperty Font 
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
               TabIndex        =   171
               Top             =   3720
               Width           =   1620
            End
            Begin VB.Label Label57 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Arquivo Chave Privada"
               BeginProperty Font 
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
               TabIndex        =   168
               Top             =   3120
               Width           =   1920
            End
            Begin VB.Label Label56 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Developer Application Key"
               BeginProperty Font 
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
               TabIndex        =   167
               Top             =   2520
               Width           =   2220
            End
            Begin VB.Label Label55 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Client Secret"
               BeginProperty Font 
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
               TabIndex        =   165
               Top             =   1920
               Width           =   1080
            End
            Begin VB.Label Label54 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Client ID"
               BeginProperty Font 
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
               TabIndex        =   163
               Top             =   1320
               Width           =   720
            End
            Begin VB.Label Label53 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Chave PIX"
               BeginProperty Font 
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
               TabIndex        =   161
               Top             =   720
               Width           =   855
            End
            Begin VB.Label Label52 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Arquivo Certificado"
               BeginProperty Font 
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
               TabIndex        =   157
               Top             =   2520
               Width           =   1620
            End
            Begin VB.Label Label51 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Client Secret"
               BeginProperty Font 
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
               TabIndex        =   156
               Top             =   1920
               Width           =   1080
            End
            Begin VB.Label Label50 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Client ID"
               BeginProperty Font 
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
               TabIndex        =   154
               Top             =   1320
               Width           =   720
            End
            Begin VB.Label Label49 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Chave PIX"
               BeginProperty Font 
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
               TabIndex        =   152
               Top             =   720
               Width           =   855
            End
            Begin VB.Label Label48 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Arquivo Certificado"
               BeginProperty Font 
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
               TabIndex        =   148
               Top             =   3120
               Width           =   1620
            End
            Begin VB.Label Label47 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Arquivo Chave Privada"
               BeginProperty Font 
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
               Top             =   2520
               Width           =   1920
            End
            Begin VB.Label Label46 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Client Secret"
               BeginProperty Font 
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
               TabIndex        =   144
               Top             =   1920
               Width           =   1080
            End
            Begin VB.Label Label45 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Client ID"
               BeginProperty Font 
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
               TabIndex        =   142
               Top             =   1320
               Width           =   720
            End
            Begin VB.Label Label44 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Chave PIX"
               BeginProperty Font 
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
               Top             =   720
               Width           =   855
            End
            Begin VB.Label Label43 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Arquivo Certificado"
               BeginProperty Font 
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
               TabIndex        =   136
               Top             =   3120
               Width           =   1620
            End
            Begin VB.Label Label42 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Arquivo Chave Privada"
               BeginProperty Font 
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
               TabIndex        =   133
               Top             =   2520
               Width           =   1920
            End
            Begin VB.Label Label41 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Client Secret"
               BeginProperty Font 
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
               TabIndex        =   132
               Top             =   1920
               Width           =   1080
            End
            Begin VB.Label Label40 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Client ID"
               BeginProperty Font 
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
               Top             =   1320
               Width           =   720
            End
            Begin VB.Label Label39 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Chave PIX"
               BeginProperty Font 
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
               TabIndex        =   128
               Top             =   720
               Width           =   855
            End
            Begin VB.Label Label38 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Arquivo Certificado"
               BeginProperty Font 
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
               Top             =   3120
               Width           =   1620
            End
            Begin VB.Label Label37 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Arquivo Chave Privada"
               BeginProperty Font 
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
               TabIndex        =   121
               Top             =   2520
               Width           =   1920
            End
            Begin VB.Label Label36 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Client Secret"
               BeginProperty Font 
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
               TabIndex        =   120
               Top             =   1920
               Width           =   1080
            End
            Begin VB.Label Label35 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Client ID"
               BeginProperty Font 
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
               Top             =   1320
               Width           =   720
            End
            Begin VB.Label Label34 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Chave PIX"
               BeginProperty Font 
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
               Top             =   720
               Width           =   855
            End
            Begin VB.Label Label33 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Secret Key"
               BeginProperty Font 
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
               TabIndex        =   114
               Top             =   1920
               Width           =   915
            End
            Begin VB.Label Label32 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Token"
               BeginProperty Font 
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
               TabIndex        =   112
               Top             =   1320
               Width           =   525
            End
            Begin VB.Label Label31 
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
               TabIndex        =   110
               Top             =   720
               Width           =   405
            End
            Begin VB.Label Label30 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Senha Certificado"
               BeginProperty Font 
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
               TabIndex        =   106
               Top             =   3120
               Width           =   1485
            End
            Begin VB.Label Label29 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Arquivo Certificado PFX"
               BeginProperty Font 
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
               TabIndex        =   104
               Top             =   2520
               Width           =   1965
            End
            Begin VB.Label Label28 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Consumer Secret"
               BeginProperty Font 
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
               TabIndex        =   103
               Top             =   1920
               Width           =   1455
            End
            Begin VB.Label Label27 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Consumer Key"
               BeginProperty Font 
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
               TabIndex        =   101
               Top             =   1320
               Width           =   1215
            End
            Begin VB.Label Label26 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Chave PIX"
               BeginProperty Font 
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
               TabIndex        =   99
               Top             =   720
               Width           =   855
            End
            Begin VB.Label Label25 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Access Key"
               BeginProperty Font 
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
               TabIndex        =   97
               Top             =   1920
               Width           =   945
            End
            Begin VB.Label Label24 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Secret Key"
               BeginProperty Font 
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
               TabIndex        =   95
               Top             =   1320
               Width           =   915
            End
            Begin VB.Label Label23 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Client ID"
               BeginProperty Font 
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
               TabIndex        =   93
               Top             =   720
               Width           =   720
            End
            Begin VB.Label Label22 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Arquivo Certificado"
               BeginProperty Font 
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
               TabIndex        =   89
               Top             =   3120
               Width           =   1620
            End
            Begin VB.Label Label21 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Arquivo Chave Privada"
               BeginProperty Font 
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
               TabIndex        =   86
               Top             =   2520
               Width           =   1920
            End
            Begin VB.Label Label20 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Token Sandbox"
               BeginProperty Font 
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
               Top             =   1920
               Width           =   1305
            End
            Begin VB.Label Label19 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Client ID"
               BeginProperty Font 
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
               Top             =   1320
               Width           =   720
            End
            Begin VB.Label Label18 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Chave PIX"
               BeginProperty Font 
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
               Top             =   720
               Width           =   855
            End
            Begin VB.Label Label17 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Arquivo Certificado"
               BeginProperty Font 
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
               Top             =   3120
               Width           =   1620
            End
            Begin VB.Label Label16 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Arquivo Chave Privada"
               BeginProperty Font 
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
               TabIndex        =   74
               Top             =   2520
               Width           =   1920
            End
            Begin VB.Label Label15 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Client Secret"
               BeginProperty Font 
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
               TabIndex        =   73
               Top             =   1920
               Width           =   1080
            End
            Begin VB.Label Label14 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Client ID"
               BeginProperty Font 
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
               TabIndex        =   71
               Top             =   1320
               Width           =   720
            End
            Begin VB.Label Label13 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Chave PIX"
               BeginProperty Font 
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
               TabIndex        =   69
               Top             =   720
               Width           =   855
            End
            Begin VB.Label Label12 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Senha Certificado"
               BeginProperty Font 
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
               Top             =   3120
               Width           =   1485
            End
            Begin VB.Label Label11 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Arquivo PFX"
               BeginProperty Font 
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
               Top             =   2520
               Width           =   1005
            End
            Begin VB.Label Label9 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Client Secret"
               BeginProperty Font 
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
               Top             =   1920
               Width           =   1080
            End
            Begin VB.Label Label7 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Client ID"
               BeginProperty Font 
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
               TabIndex        =   59
               Top             =   1320
               Width           =   720
            End
            Begin VB.Label Label6 
               AutoSize        =   -1  'True
               BackStyle       =   0  'Transparent
               Caption         =   "Chave PIX"
               BeginProperty Font 
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
               Width           =   855
            End
         End
      End
   End
   Begin VB.Label Label10 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "Arquivo"
      BeginProperty Font 
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
      TabIndex        =   62
      Top             =   3360
      Width           =   660
   End
End
Attribute VB_Name = "FrmMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim pixcd As ACBrPIXCD

Private Sub Form_Load()

    cmbPSP.ListIndex = 0
    cmbAmbiente.ListIndex = 0
    cmbBBAPIVersao.ListIndex = 0
    cmbNivelLogPSP.Text = "Nenhum"
    cmbTipoChave.ListIndex = 0
    
    
    Dim LogPath As String
    Dim IniPath As String
    
    LogPath = App.Path & "\Logs\"
    IniPath = App.Path & "\ACBrLib.ini"
    
    If Not DirExists(LogPath) Then
        MkDir LogPath
    End If
    
    Set pixcd = CreatePIXCD(IniPath)
    
    pixcd.ConfigGravarValor SESSAO_PRINCIPAL, "LogNivel", NivelLog.logParanoico
    pixcd.ConfigGravarValor SESSAO_PRINCIPAL, "LogPath", LogPath
    pixcd.ConfigGravarValor SESSAO_PIXCD, "ProxyPort", CStr(nudProxyPorta.Value)
    pixcd.ConfigGravar
    
    LoadConfig
End Sub

Private Sub btnSalvarConfiguracoes_Click()
    SalvarConfig
End Sub

Private Sub btnCarregarConfiguracoes_Click()
    LoadConfig
End Sub
    
Private Sub LoadConfig()
    Dim buffer As String
    Dim bufferLen As Long
    
    pixcd.ConfigLer
    
    'PIXCD
    cmbAmbiente.ListIndex = CLng(pixcd.ConfigLerValor(SESSAO_PIXCD, "Ambiente"))
    txtArqLogPSP.Text = pixcd.ConfigLerValor(SESSAO_PIXCD, "ArqLog")
    cmbNivelLogPSP.ListIndex = CLng(pixcd.ConfigLerValor(SESSAO_PIXCD, "NivelLog"))
    cmbTipoChave.ListIndex = CLng(pixcd.ConfigLerValor(SESSAO_PIXCD, "TipoChave"))
    cmbPSP.ListIndex = CLng(pixcd.ConfigLerValor(SESSAO_PIXCD, "PSP"))
    nudTimeoutPSP.Value = CLng(pixcd.ConfigLerValor(SESSAO_PIXCD, "Timeout"))
    txtProxyServidor.Text = pixcd.ConfigLerValor(SESSAO_PIXCD, "ProxyHost")
    txtProxySenha.Text = pixcd.ConfigLerValor(SESSAO_PIXCD, "ProxyPass")
    nudProxyPorta.Value = CLng(pixcd.ConfigLerValor(SESSAO_PIXCD, "ProxyPort"))
    txtProxyUsuario.Text = pixcd.ConfigLerValor(SESSAO_PIXCD, "ProxyUser")
    txtCEPRecebedor.Text = pixcd.ConfigLerValor(SESSAO_PIXCD, "CEPRecebedor")
    txtCidadeRecebedor.Text = pixcd.ConfigLerValor(SESSAO_PIXCD, "CidadeRecebedor")
    txtNomeRecebedor.Text = pixcd.ConfigLerValor(SESSAO_PIXCD, "NomeRecebedor")
    txtUFRecebedor.Text = pixcd.ConfigLerValor(SESSAO_PIXCD, "UFRecebedor")
            
    'Bradesco
    txtChavePIXBradesco.Text = pixcd.ConfigLerValor(SESSAO_Bradesco, "ChavePIX")
    txtClientIDBradesco.Text = pixcd.ConfigLerValor(SESSAO_Bradesco, "ClientID")
    txtClientSecretBradesco.Text = pixcd.ConfigLerValor(SESSAO_Bradesco, "ClientSecret")
    txtArquivoPFXBradesco.Text = pixcd.ConfigLerValor(SESSAO_Bradesco, "ArqPFX")
    txtSenhaCertificadoBradesco.Text = pixcd.ConfigLerValor(SESSAO_Bradesco, "SenhaPFX")
    
    'Sicredi
    txtChavePIXSicredi.Text = pixcd.ConfigLerValor(SESSAO_Sicredi, "ChavePIX")
    txtClientIDSicredi.Text = pixcd.ConfigLerValor(SESSAO_Sicredi, "ClientID")
    txtClientSecretSicredi.Text = pixcd.ConfigLerValor(SESSAO_Sicredi, "ClientSecret")
    txtArquivoChavePrivadaSicredi.Text = pixcd.ConfigLerValor(SESSAO_Sicredi, "ArqChavePrivada")
    txtArquivoCertificadoSicredi.Text = pixcd.ConfigLerValor(SESSAO_Sicredi, "ArqCertificado")
    
    'Sicoob
    txtChavePIXSicoob.Text = pixcd.ConfigLerValor(SESSAO_Sicoob, "ChavePIX")
    txtClientIDSicoob.Text = pixcd.ConfigLerValor(SESSAO_Sicoob, "ClientID")
    txtTokenSandboxSicoob.Text = pixcd.ConfigLerValor(SESSAO_Sicoob, "TokenSandbox")
    txtArquivoChavePrivadaSicoob.Text = pixcd.ConfigLerValor(SESSAO_Sicoob, "ArqChavePrivada")
    txtArquivoCertificadoSicoob.Text = pixcd.ConfigLerValor(SESSAO_Sicoob, "ArqCertificado")
    
    'Shipay
    txtClientIDShipay.Text = pixcd.ConfigLerValor(SESSAO_Shipay, "ClientID")
    txtSecretKeyShipay.Text = pixcd.ConfigLerValor(SESSAO_Shipay, "SecretKey")
    txtAccessKeyShipay.Text = pixcd.ConfigLerValor(SESSAO_Shipay, "AccessKey")
    
    'Santander
    txtChavePIXSantander.Text = pixcd.ConfigLerValor(SESSAO_Santander, "ChavePIX")
    txtConsumerKeySantander.Text = pixcd.ConfigLerValor(SESSAO_Santander, "ConsumerKey")
    txtConsumerSecretSantander.Text = pixcd.ConfigLerValor(SESSAO_Santander, "ConsumerSecret")
    txtArquivoCertificadoPFXSantander.Text = pixcd.ConfigLerValor(SESSAO_Santander, "ArqCertificadoPFX")
    txtSenhaCertificadoPFXSantander.Text = pixcd.ConfigLerValor(SESSAO_Santander, "SenhaCertificadoPFX")
    
    'PixPDV
    txtCNPJPixPDV.Text = pixcd.ConfigLerValor(SESSAO_PixPDV, "CNPJ")
    txtPixPDVToken.Text = pixcd.ConfigLerValor(SESSAO_PixPDV, "Token")
    txtSecretKeyPixPDV.Text = pixcd.ConfigLerValor(SESSAO_PixPDV, "SecretKey")
    
    'PagSeguro
    txtChavePIXPagSeguro.Text = pixcd.ConfigLerValor(SESSAO_PagSeguro, "ChavePIX")
    txtClientIDPagSeguro.Text = pixcd.ConfigLerValor(SESSAO_PagSeguro, "ClientID")
    txtClientSecretPagSeguro.Text = pixcd.ConfigLerValor(SESSAO_PagSeguro, "ClientSecret")
    txtArquivoChavePrivadaPagSeguro.Text = pixcd.ConfigLerValor(SESSAO_PagSeguro, "ArqChavePrivada")
    txtArquivoCertificadoPagSeguro.Text = pixcd.ConfigLerValor(SESSAO_PagSeguro, "ArqCertificado")
    
    'Itau
    txtChavePIXItau.Text = pixcd.ConfigLerValor(SESSAO_Itau, "ChavePIX")
    txtClientIDItau.Text = pixcd.ConfigLerValor(SESSAO_Itau, "ClientID")
    txtClientSecretItau.Text = pixcd.ConfigLerValor(SESSAO_Itau, "ClientSecret")
    txtArquivoChavePrivadaItau.Text = pixcd.ConfigLerValor(SESSAO_Itau, "ArqChavePrivada")
    txtArquivoCertificadoItau.Text = pixcd.ConfigLerValor(SESSAO_Itau, "ArqCertificado")
    
    'Inter
    txtChavePIXInter.Text = pixcd.ConfigLerValor(SESSAO_Inter, "ChavePIX")
    txtClientIDInter.Text = pixcd.ConfigLerValor(SESSAO_Inter, "ClientID")
    txtClientSecretInter.Text = pixcd.ConfigLerValor(SESSAO_Inter, "ClientSecret")
    txtArquivoChavePrivadaInter.Text = pixcd.ConfigLerValor(SESSAO_Inter, "ArqChavePrivada")
    txtArquivoCertificadoInter.Text = pixcd.ConfigLerValor(SESSAO_Inter, "ArqCertificado")
    
    'GerenciaNet
    txtChavePIXGerenciaNet.Text = pixcd.ConfigLerValor(SESSAO_GerenciaNet, "ChavePIX")
    txtClientIDGerenciaNet.Text = pixcd.ConfigLerValor(SESSAO_GerenciaNet, "ClientID")
    txtClientSecretGerenciaNet.Text = pixcd.ConfigLerValor(SESSAO_GerenciaNet, "ClientSecret")
    txtArquivoCertificadoGerenciaNet.Text = pixcd.ConfigLerValor(SESSAO_GerenciaNet, "ArqPFX")
    
    'BancoBrasil
    txtChavePIXBancoBrasil.Text = pixcd.ConfigLerValor(SESSAO_BancoBrasil, "ChavePIX")
    txtClientIDBancoBrasil.Text = pixcd.ConfigLerValor(SESSAO_BancoBrasil, "ClientID")
    txtClientSecretBancoBrasil.Text = pixcd.ConfigLerValor(SESSAO_BancoBrasil, "ClientSecret")
    txtDeveloperApplicationKeyBancoBrasil.Text = pixcd.ConfigLerValor(SESSAO_BancoBrasil, "DeveloperApplicationKey")
    txtArquivoChavePrivadaBancoBrasil.Text = pixcd.ConfigLerValor(SESSAO_BancoBrasil, "ArqChavePrivada")
    txtArquivoCertificadoBancoBrasil.Text = pixcd.ConfigLerValor(SESSAO_BancoBrasil, "ArqCertificado")
    txtArquivoPXFBancoBrasil.Text = pixcd.ConfigLerValor(SESSAO_BancoBrasil, "ArqPFX")
    txtSenhaPFXBancoBrasil.Text = pixcd.ConfigLerValor(SESSAO_BancoBrasil, "SenhaPFX")
    cmbBBAPIVersao.ListIndex = CLng(pixcd.ConfigLerValor(SESSAO_BancoBrasil, "BBAPIVersao"))
    
    'Ailos
    txtChavePIXAilos.Text = pixcd.ConfigLerValor(SESSAO_Ailos, "ChavePIX")
    txtClientIDAilos.Text = pixcd.ConfigLerValor(SESSAO_Ailos, "ClientID")
    txtClientSecretAilos.Text = pixcd.ConfigLerValor(SESSAO_Ailos, "ClientSecret")
    txtArquivoChavePrivadaAilos.Text = pixcd.ConfigLerValor(SESSAO_Ailos, "ArqChavePrivada")
    txtArquivoCertificadoAilos.Text = pixcd.ConfigLerValor(SESSAO_Ailos, "ArqCertificado")
    txtArquivoCeriticadoRootAilos.Text = pixcd.ConfigLerValor(SESSAO_Ailos, "ArqCertificadoRoot")
    
    'Matera
    txtChavePIXMatera.Text = pixcd.ConfigLerValor(SESSAO_Matera, "ChavePIX")
    txtClientIDMatera.Text = pixcd.ConfigLerValor(SESSAO_Matera, "ClientID")
    txtSecretKeyMatera.Text = pixcd.ConfigLerValor(SESSAO_Matera, "SecretKey")
    txtClientSecretMatera.Text = pixcd.ConfigLerValor(SESSAO_Matera, "ClientSecret")
    txtArquivoCertificadoMatera.Text = pixcd.ConfigLerValor(SESSAO_Matera, "ArqCertificado")
    txtArquivoChavePrivadaMatera.Text = pixcd.ConfigLerValor(SESSAO_Matera, "ArqChavePrivada")
    txtAccountIDMatera.Text = pixcd.ConfigLerValor(SESSAO_Matera, "AccountID")
    txtMediatorFeeMatera.Text = pixcd.ConfigLerValor(SESSAO_Matera, "MediatorFee")
    
    'Cielo
    txtChavePIXCielo.Text = pixcd.ConfigLerValor(SESSAO_Cielo, "ChavePIX")
    txtClientIDCielo.Text = pixcd.ConfigLerValor(SESSAO_Cielo, "ClientID")
    txtClientSecretCielo.Text = pixcd.ConfigLerValor(SESSAO_Cielo, "ClientSecret")
    
    'MercadoPago
    txtChavePIXMercadoPago.Text = pixcd.ConfigLerValor(SESSAO_MercadoPago, "ChavePIX")
    txtAccessTokenMercadoPago.Text = pixcd.ConfigLerValor(SESSAO_MercadoPago, "AccessToken")
    
End Sub

Private Sub SalvarConfig()
    
    'PIXCD
    pixcd.ConfigGravarValor SESSAO_PIXCD, "Ambiente", CStr(cmbAmbiente.ListIndex)
    pixcd.ConfigGravarValor SESSAO_PIXCD, "ArqLog", txtArqLogPSP.Text
    pixcd.ConfigGravarValor SESSAO_PIXCD, "NivelLog", CStr(cmbNivelLogPSP.ListIndex)
    pixcd.ConfigGravarValor SESSAO_PIXCD, "TipoChave", CStr(cmbTipoChave.ListIndex)
    pixcd.ConfigGravarValor SESSAO_PIXCD, "PSP", CStr(cmbPSP.ListIndex)
    pixcd.ConfigGravarValor SESSAO_PIXCD, "Timeout", CStr(nudTimeoutPSP.Value)
    pixcd.ConfigGravarValor SESSAO_PIXCD, "ProxyHost", txtProxyServidor.Text
    pixcd.ConfigGravarValor SESSAO_PIXCD, "ProxyPass", txtProxySenha.Text
    pixcd.ConfigGravarValor SESSAO_PIXCD, "ProxyPort", CStr(nudProxyPorta.Value)
    pixcd.ConfigGravarValor SESSAO_PIXCD, "ProxyUser", txtProxyUsuario.Text
    pixcd.ConfigGravarValor SESSAO_PIXCD, "CEPRecebedor", txtCEPRecebedor.Text
    pixcd.ConfigGravarValor SESSAO_PIXCD, "CidadeRecebedor", txtCidadeRecebedor.Text
    pixcd.ConfigGravarValor SESSAO_PIXCD, "NomeRecebedor", txtNomeRecebedor.Text
    pixcd.ConfigGravarValor SESSAO_PIXCD, "UFRecebedor", txtUFRecebedor.Text
            
    'Bradesco
    pixcd.ConfigGravarValor SESSAO_Bradesco, "ChavePIX", txtChavePIXBradesco.Text
    pixcd.ConfigGravarValor SESSAO_Bradesco, "ClientID", txtClientIDBradesco.Text
    pixcd.ConfigGravarValor SESSAO_Bradesco, "ClientSecret", txtClientSecretBradesco.Text
    pixcd.ConfigGravarValor SESSAO_Bradesco, "ArqPFX", txtArquivoPFXBradesco.Text
    pixcd.ConfigGravarValor SESSAO_Bradesco, "SenhaPFX", txtSenhaCertificadoBradesco.Text
    
    'Sicredi
    pixcd.ConfigGravarValor SESSAO_Sicredi, "ChavePIX", txtChavePIXSicredi.Text
    pixcd.ConfigGravarValor SESSAO_Sicredi, "ClientID", txtClientIDSicredi.Text
    pixcd.ConfigGravarValor SESSAO_Sicredi, "ClientSecret", txtClientSecretSicredi.Text
    pixcd.ConfigGravarValor SESSAO_Sicredi, "ArqChavePrivada", txtArquivoChavePrivadaSicredi.Text
    pixcd.ConfigGravarValor SESSAO_Sicredi, "ArqCertificado", txtArquivoCertificadoSicredi.Text
    
    'Sicoob
    pixcd.ConfigGravarValor SESSAO_Sicoob, "ChavePIX", txtChavePIXSicoob.Text
    pixcd.ConfigGravarValor SESSAO_Sicoob, "ClientID", txtClientIDSicoob.Text
    pixcd.ConfigGravarValor SESSAO_Sicoob, "TokenSandbox", txtTokenSandboxSicoob.Text
    pixcd.ConfigGravarValor SESSAO_Sicoob, "ArqChavePrivada", txtArquivoChavePrivadaSicoob.Text
    pixcd.ConfigGravarValor SESSAO_Sicoob, "ArqCertificado", txtArquivoCertificadoSicoob.Text
    
    'Shipay
    pixcd.ConfigGravarValor SESSAO_Shipay, "ClientID", txtClientIDShipay.Text
    pixcd.ConfigGravarValor SESSAO_Shipay, "SecretKey", txtSecretKeyShipay.Text
    pixcd.ConfigGravarValor SESSAO_Shipay, "AccessKey", txtAccessKeyShipay.Text
    
    'Santander
    pixcd.ConfigGravarValor SESSAO_Santander, "ChavePIX", txtChavePIXSantander.Text
    pixcd.ConfigGravarValor SESSAO_Santander, "ConsumerKey", txtConsumerKeySantander.Text
    pixcd.ConfigGravarValor SESSAO_Santander, "ConsumerSecret", txtConsumerSecretSantander.Text
    pixcd.ConfigGravarValor SESSAO_Santander, "ArqCertificadoPFX", txtArquivoCertificadoPFXSantander.Text
    pixcd.ConfigGravarValor SESSAO_Santander, "SenhaCertificadoPFX", txtSenhaCertificadoPFXSantander.Text
    
    'PixPDV
    pixcd.ConfigGravarValor SESSAO_PixPDV, "CNPJ", txtCNPJPixPDV.Text
    pixcd.ConfigGravarValor SESSAO_PixPDV, "Token", txtPixPDVToken.Text
    pixcd.ConfigGravarValor SESSAO_PixPDV, "SecretKey", txtSecretKeyPixPDV.Text
    
    'PagSeguro
    pixcd.ConfigGravarValor SESSAO_PagSeguro, "ChavePIX", txtChavePIXPagSeguro.Text
    pixcd.ConfigGravarValor SESSAO_PagSeguro, "ClientID", txtClientIDPagSeguro.Text
    pixcd.ConfigGravarValor SESSAO_PagSeguro, "ClientSecret", txtClientSecretPagSeguro.Text
    pixcd.ConfigGravarValor SESSAO_PagSeguro, "ArqChavePrivada", txtArquivoChavePrivadaPagSeguro.Text
    pixcd.ConfigGravarValor SESSAO_PagSeguro, "ArqCertificado", txtArquivoCertificadoPagSeguro.Text
    
    'Itau
    pixcd.ConfigGravarValor SESSAO_Itau, "ChavePIX", txtChavePIXItau.Text
    pixcd.ConfigGravarValor SESSAO_Itau, "ClientID", txtClientIDItau.Text
    pixcd.ConfigGravarValor SESSAO_Itau, "ClientSecret", txtClientSecretItau.Text
    pixcd.ConfigGravarValor SESSAO_Itau, "ArqChavePrivada", txtArquivoChavePrivadaItau.Text
    pixcd.ConfigGravarValor SESSAO_Itau, "ArqCertificado", txtArquivoCertificadoItau.Text
    
    'Inter
    pixcd.ConfigGravarValor SESSAO_Inter, "ChavePIX", txtChavePIXInter.Text
    pixcd.ConfigGravarValor SESSAO_Inter, "ClientID", txtClientIDInter.Text
    pixcd.ConfigGravarValor SESSAO_Inter, "ClientSecret", txtClientSecretInter.Text
    pixcd.ConfigGravarValor SESSAO_Inter, "ArqChavePrivada", txtArquivoChavePrivadaInter.Text
    pixcd.ConfigGravarValor SESSAO_Inter, "ArqCertificado", txtArquivoCertificadoInter.Text
    
    'GerenciaNet
    pixcd.ConfigGravarValor SESSAO_GerenciaNet, "ChavePIX", txtChavePIXGerenciaNet.Text
    pixcd.ConfigGravarValor SESSAO_GerenciaNet, "ClientID", txtClientIDGerenciaNet.Text
    pixcd.ConfigGravarValor SESSAO_GerenciaNet, "ClientSecret", txtClientSecretGerenciaNet.Text
    pixcd.ConfigGravarValor SESSAO_GerenciaNet, "ArqPFX", txtArquivoCertificadoGerenciaNet.Text
    
    'BancoBrasil
    pixcd.ConfigGravarValor SESSAO_BancoBrasil, "ChavePIX", txtChavePIXBancoBrasil.Text
    pixcd.ConfigGravarValor SESSAO_BancoBrasil, "ClientID", txtClientIDBancoBrasil.Text
    pixcd.ConfigGravarValor SESSAO_BancoBrasil, "ClientSecret", txtClientSecretBancoBrasil.Text
    pixcd.ConfigGravarValor SESSAO_BancoBrasil, "DeveloperApplicationKey", txtDeveloperApplicationKeyBancoBrasil.Text
    pixcd.ConfigGravarValor SESSAO_BancoBrasil, "ArqChavePrivada", txtArquivoChavePrivadaBancoBrasil.Text
    pixcd.ConfigGravarValor SESSAO_BancoBrasil, "ArqCertificado", txtArquivoCertificadoBancoBrasil.Text
    pixcd.ConfigGravarValor SESSAO_BancoBrasil, "ArqPFX", txtArquivoPXFBancoBrasil.Text
    pixcd.ConfigGravarValor SESSAO_BancoBrasil, "SenhaPFX", txtSenhaPFXBancoBrasil.Text
    pixcd.ConfigGravarValor SESSAO_BancoBrasil, "BBAPIVersao", CStr(cmbBBAPIVersao.ListIndex)
    
    'Ailos
    pixcd.ConfigGravarValor SESSAO_Ailos, "ChavePIX", txtChavePIXAilos.Text
    pixcd.ConfigGravarValor SESSAO_Ailos, "ClientID", txtClientIDAilos.Text
    pixcd.ConfigGravarValor SESSAO_Ailos, "ClientSecret", txtClientSecretAilos.Text
    pixcd.ConfigGravarValor SESSAO_Ailos, "ArqChavePrivada", txtArquivoChavePrivadaAilos.Text
    pixcd.ConfigGravarValor SESSAO_Ailos, "ArqCertificado", txtArquivoCertificadoAilos.Text
    pixcd.ConfigGravarValor SESSAO_Ailos, "ArqCertificadoRoot", txtArquivoCeriticadoRootAilos.Text
    
    'Matera
    pixcd.ConfigGravarValor SESSAO_Matera, "ChavePIX", txtChavePIXMatera.Text
    pixcd.ConfigGravarValor SESSAO_Matera, "ClientID", txtClientIDMatera.Text
    pixcd.ConfigGravarValor SESSAO_Matera, "SecretKey", txtSecretKeyMatera.Text
    pixcd.ConfigGravarValor SESSAO_Matera, "ClientSecret", txtClientSecretMatera.Text
    pixcd.ConfigGravarValor SESSAO_Matera, "ArqCertificado", txtArquivoCertificadoMatera.Text
    pixcd.ConfigGravarValor SESSAO_Matera, "ArqChavePrivada", txtArquivoChavePrivadaMatera.Text
    pixcd.ConfigGravarValor SESSAO_Matera, "AccountID", txtAccountIDMatera.Text
    pixcd.ConfigGravarValor SESSAO_Matera, "MediatorFee", txtMediatorFeeMatera.Text
    
    'Cielo
    pixcd.ConfigGravarValor SESSAO_Cielo, "ChavePIX", txtChavePIXCielo.Text
    pixcd.ConfigGravarValor SESSAO_Cielo, "ClientID", txtClientIDCielo.Text
    pixcd.ConfigGravarValor SESSAO_Cielo, "ClientSecret", txtClientSecretCielo.Text
    
    'MercadoPago
    pixcd.ConfigGravarValor SESSAO_MercadoPago, "ChavePIX", txtChavePIXMercadoPago.Text
    pixcd.ConfigGravarValor SESSAO_MercadoPago, "AccessToken", txtAccessTokenMercadoPago.Text
    
    pixcd.ConfigGravar

End Sub

'METODOS

Private Sub btnCertificadoBradesco_Click()
    On Error Resume Next
    CommonDialog1.DialogTitle = "Selecione Arquivo PFX"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivos PFX (*.pfx)|*.pfx|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.ShowOpen
            
    If Err Then Exit Sub
    
    txtArquivoPFXBradesco.Text = CommonDialog1.FileName
End Sub

Private Sub btnArquivoChavePrivadaSicredi_Click()
    On Error Resume Next
    CommonDialog1.DialogTitle = "Selecione Arquivo KEY"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivos KEY (*.key)|*.key|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.ShowOpen
            
    If Err Then Exit Sub
    
    txtArquivoChavePrivadaSicredi.Text = CommonDialog1.FileName
End Sub

Private Sub btnArquivoCertificadoSicredi_Click()
    On Error Resume Next
    CommonDialog1.DialogTitle = "Selecione Arquivo CER"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivos CER (*.cer)|*.cer|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.ShowOpen
            
    If Err Then Exit Sub
    
    txtArquivoCertificadoSicredi.Text = CommonDialog1.FileName
End Sub

Private Sub btnArquivoChavePrivadaSicoob_Click()
    On Error Resume Next
    CommonDialog1.DialogTitle = "Selecione Arquivo KEY"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivos KEY (*.key)|*.key|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.ShowOpen
            
    If Err Then Exit Sub
    
    txtArquivoChavePrivadaSicoob.Text = CommonDialog1.FileName
End Sub

Private Sub btnArquivoCertificadoSicoob_Click()
    On Error Resume Next
    CommonDialog1.DialogTitle = "Selecione Arquivo PEM"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivos PEM (*.pem)|*.pem|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.ShowOpen
            
    If Err Then Exit Sub
    
    txtArquivoCertificadoSicoob.Text = CommonDialog1.FileName
End Sub

Private Sub btnArquivoCertificadoPFXSantander_Click()
    On Error Resume Next
    CommonDialog1.DialogTitle = "Selecione Arquivo PFX"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivos PFX (*.pfx)|*.pfx|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.ShowOpen
            
    If Err Then Exit Sub
    
    txtArquivoCertificadoPFXSantander.Text = CommonDialog1.FileName
End Sub

Private Sub btnArquivoChavePrivadaPagSeguro_Click()
    On Error Resume Next
    CommonDialog1.DialogTitle = "Selecione Arquivo KEY"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivos KEY (*.key)|*.key|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.ShowOpen
            
    If Err Then Exit Sub
    
    txtArquivoChavePrivadaPagSeguro.Text = CommonDialog1.FileName
End Sub

Private Sub btnArquivoCertificadoPagSeguro_Click()
    On Error Resume Next
    CommonDialog1.DialogTitle = "Selecione Arquivo PEM"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivos PEM (*.pem)|*.pem|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.ShowOpen
            
    If Err Then Exit Sub
    
    txtArquivoCertificadoPagSeguro.Text = CommonDialog1.FileName
End Sub

Private Sub btnArquivoChavePrivadaItau_Click()
    On Error Resume Next
    CommonDialog1.DialogTitle = "Selecione Arquivo KEY"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivos KEY (*.key)|*.key|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.ShowOpen
            
    If Err Then Exit Sub
    
    txtArquivoChavePrivadaItau.Text = CommonDialog1.FileName
End Sub

Private Sub btnArquivoCertificadoItau_Click()
    On Error Resume Next
    CommonDialog1.DialogTitle = "Selecione Arquivo PEM"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivos PEM (*.pem)|*.pem|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.ShowOpen
            
    If Err Then Exit Sub
    
    txtArquivoCertificadoItau.Text = CommonDialog1.FileName
End Sub

Private Sub btnArquivoChavePrivadaInter_Click()
    On Error Resume Next
    CommonDialog1.DialogTitle = "Selecione Arquivo KEY"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivos KEY (*.key)|*.key|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.ShowOpen
            
    If Err Then Exit Sub
    
    txtArquivoChavePrivadaInter.Text = CommonDialog1.FileName
End Sub

Private Sub btnArquivoCertificadoInter_Click()
    On Error Resume Next
    CommonDialog1.DialogTitle = "Selecione Arquivo PEM"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivos PEM (*.pem)|*.pem|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.ShowOpen
            
    If Err Then Exit Sub
    
    txtArquivoCertificadoInter.Text = CommonDialog1.FileName
End Sub

Private Sub btnArquivoCertificadoGerenciaNet_Click()
    On Error Resume Next
    CommonDialog1.DialogTitle = "Selecione Arquivo P12"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivos P12 (*.p12)|*.p12|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.ShowOpen
            
    If Err Then Exit Sub
    
    txtArquivoCertificadoGerenciaNet.Text = CommonDialog1.FileName
End Sub

Private Sub btnArquivoChavePrivadaBancoBrasil_Click()
    On Error Resume Next
    CommonDialog1.DialogTitle = "Selecione Arquivo KEY"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivos KEY (*.key)|*.key|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.ShowOpen
            
    If Err Then Exit Sub
    
    txtArquivoChavePrivadaBancoBrasil.Text = CommonDialog1.FileName
End Sub

Private Sub btnArquivoCertificadoBancoBrasil_Click()
    On Error Resume Next
    CommonDialog1.DialogTitle = "Selecione Arquivo PEM"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivos PEM (*.pem)|*.pem|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.ShowOpen
            
    If Err Then Exit Sub
    
    txtArquivoCertificadoBancoBrasil.Text = CommonDialog1.FileName
End Sub

Private Sub btnArquivoPXFBancoBrasil_Click()
    On Error Resume Next
    CommonDialog1.DialogTitle = "Selecione Arquivo PFX"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivos PFX (*.pfx)|*.pfx|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.ShowOpen
            
    If Err Then Exit Sub
    
    txtArquivoPXFBancoBrasil.Text = CommonDialog1.FileName
End Sub

Private Sub btnArquivoChavePrivadaAilos_Click()
    On Error Resume Next
    CommonDialog1.DialogTitle = "Selecione Arquivo KEY"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivos KEY (*.key)|*.key|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.ShowOpen
            
    If Err Then Exit Sub
    
    txtArquivoChavePrivadaAilos.Text = CommonDialog1.FileName
End Sub

Private Sub btnArquivoCertificadoAilos_Click()
    On Error Resume Next
    CommonDialog1.DialogTitle = "Selecione Arquivo PEM"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivos PEM (*.pem)|*.pem|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.ShowOpen
            
    If Err Then Exit Sub
    
    txtArquivoCertificadoAilos.Text = CommonDialog1.FileName
End Sub

Private Sub btnArquivoCeriticadoRootAilos_Click()
    On Error Resume Next
    CommonDialog1.DialogTitle = "Selecione Arquivo CRT"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivos CRT (*.crt)|*.crt|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.ShowOpen
            
    If Err Then Exit Sub
    
    txtArquivoCeriticadoRootAilos.Text = CommonDialog1.FileName
End Sub

Private Sub btnArquivoChavePrivadaMatera_Click()
    On Error Resume Next
    CommonDialog1.DialogTitle = "Selecione Arquivo KEY"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivos KEY (*.key)|*.key|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.ShowOpen
            
    If Err Then Exit Sub
    
    txtArquivoChavePrivadaMatera.Text = CommonDialog1.FileName
End Sub

Private Sub btnArquivoCertificadoMatera_Click()
    On Error Resume Next
    CommonDialog1.DialogTitle = "Selecione Arquivo PEM"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivos PEM (*.pem)|*.pem|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.ShowOpen
            
    If Err Then Exit Sub
    
    txtArquivoCertificadoMatera.Text = CommonDialog1.FileName
End Sub

Private Sub btnGerarQRCodeEstatico_Click(Index As Integer)
    On Error GoTo Erro:
    
    Dim ret As String
    Dim valor As Long
    valor = 1
    
    valor = InputBox("Informe o valor:", "QRCode Estático", valor)
    
    ret = pixcd.GerarQRCodeEstatico(valor, "", "")
    rtbRespostas.Text = ret

Erro:
    MsgBox Err.Description
End Sub

Private Sub btnConsultarPix_Click(Index As Integer)
    On Error GoTo Erro:
    
    Dim ret, e2eid As String
    
    e2eid = InputBox("Informe o e2eid:", "Consultar Pix", "")
    
    ret = pixcd.ConsultarPix(e2eid)
    rtbRespostas.Text = ret
    
Erro:
    MsgBox Err.Description
End Sub

Private Sub btnConsultarPixRecebidos_Click(Index As Integer)
    On Error GoTo Erro:
    
    Dim ret, TxId, CPFCNPJ As String
    Dim dataInicio, dataFim As Date
    Dim PagAtual, ItensPorPagina As Long
     
    dataInicio = InputBox("Informe a Data de Inicio", "Consultar Pix Recebidos", "01/MM/AAAA")
    dataFim = InputBox("Informe a Data de Inicio", "Consultar Pix Recebidos", "01/MM/AAAA")
    TxId = InputBox("Informe o TxId:", "Consultar Pix Recebidos", "")
    CPFCNPJ = InputBox("Informe o CPF ou CNPJ:", "Consultar Pix Recebidos", "")
    PagAtual = InputBox("Informe Pagina Atual", "Consultar Pix Recebidos", 1)
    ItensPorPagina = InputBox("Informe Itens por Pagina", "Consultar Pix Recebidos", 15)
    
    ret = pixcd.ConsultarPixRecebidos(dataInicio, dataFim, TxId, CPFCNPJ, PagAtual, ItensPorPagina)
    rtbRespostas.Text = ret
    
Erro:
    MsgBox Err.Description
End Sub

Private Sub btnSolicitarDevolucaoPix_Click(Index As Integer)
    On Error Resume Next
    CommonDialog1.DialogTitle = "Selecione Arquivo INI"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivos INI (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.ShowOpen
            
    If Err Then Exit Sub
        
    On Error GoTo Erro:
    
    Dim ret, e2eid, idDevolucao As String
    
    e2eid = InputBox("Informe o e2eid:", "Solicitar Devolução Pix", "")
    idDevolucao = InputBox("Informe o idDevolucao:", "Solicitar Devolução Pix", "")
            
    ret = pixcd.SolicitarDevolucaoPix(CommonDialog1.FileName, e2eid, idDevolucao)
    rtbRespostas.Text = ret
    
Erro:
    MsgBox Err.Description
End Sub

Private Sub btnConsultarDevolucaoPix_Click(Index As Integer)
    On Error GoTo Erro:
    
    Dim ret, e2eid, idDevolucao As String
    
    e2eid = InputBox("Informe o e2eid:", "Consultar Devolução Pix", "")
    idDevolucao = InputBox("Informe o idDevolucao:", "Consultar Devolução Pix", "")
    
    ret = pixcd.ConsultarDevolucaoPix(e2eid, idDevolucao)
    rtbRespostas.Text = ret
    
Erro:
    MsgBox Err.Description
End Sub

Private Sub btnCriarCobrancaImediata_Click(Index As Integer)
    On Error Resume Next
    CommonDialog1.DialogTitle = "Selecione Arquivo INI"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivos INI (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.ShowOpen
            
    If Err Then Exit Sub
        
    On Error GoTo Erro:
    
    Dim ret, TxId As String
    
    TxId = InputBox("Informe o TxId:", "Criar Cobranca Imediata", "")
    
    ret = pixcd.CriarCobrancaImediata(CommonDialog1.FileName, TxId)
    rtbRespostas.Text = ret
    
Erro:
    MsgBox Err.Description
End Sub

Private Sub btnConsultarCobrancaImediata_Click(Index As Integer)
    On Error GoTo Erro:
    
    Dim ret, TxId, Revisao As String
    
    TxId = InputBox("Informe o TxId:", "Consultar Cobranca Imediata", "")
    Revisao = InputBox("Informe o Revisao:", "Consultar Cobranca Imediata", "")
    
    ret = pixcd.ConsultarCobrancaImediata(TxId, Revisao)
    rtbRespostas.Text = ret
    
Erro:
    MsgBox Err.Description
End Sub

Private Sub btnRevisarCobrancaImediata_Click(Index As Integer)
    On Error Resume Next
    CommonDialog1.DialogTitle = "Selecione Arquivo INI"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivos INI (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.ShowOpen
            
    If Err Then Exit Sub
    
    On Error GoTo Erro:
    
    Dim ret, TxId As String
    
    TxId = InputBox("Informe o TxId:", "Revisar Cobranca Imediata", "")
    
    ret = pixcd.RevisarCobrancaImediata(CommonDialog1.FileName, TxId)
    rtbRespostas.Text = ret
    
Erro:
    MsgBox Err.Description
End Sub

Private Sub btnCancelarCobrancaImediata_Click(Index As Integer)
    On Error GoTo Erro:
    
    Dim ret As String
    
    TxId = InputBox("Informe o TxId:", "Revisar Cobranca Imediata", "")
    
    ret = pixcd.CancelarCobrancaImediata(TxId)
    rtbRespostas.Text = ret
    
Erro:
    MsgBox Err.Description
End Sub

Private Sub btnCriarCobranca_Click(Index As Integer)
    On Error Resume Next
    CommonDialog1.DialogTitle = "Selecione Arquivo INI"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivos INI (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.ShowOpen
    
    If Err Then Exit Sub
    
    On Error GoTo Erro:
    
    Dim ret, TxId As String
    
    TxId = InputBox("Informe o TxId:", "Criar Cobranca", "")
    
    ret = pixcd.CriarCobranca(CommonDialog1.FileName, TxId)
    rtbRespostas.Text = ret
    
Erro:
    MsgBox Err.Description
End Sub

Private Sub btnConsultarCobranca_Click(Index As Integer)
    On Error GoTo Erro:
    
    Dim ret, TxId, Revisao As String
    
    TxId = InputBox("Informe o TxId:", "Consultar Cobranca", "")
    Revisao = InputBox("Informe o Revisao:", "Consultar Cobranca", "")
    
    ret = pixcd.ConsultarCobranca(TxId, Revisao)
    rtbRespostas.Text = ret
    
Erro:
    MsgBox Err.Description
End Sub

Private Sub btnRevisarCobranca_Click(Index As Integer)
    On Error Resume Next
    CommonDialog1.DialogTitle = "Selecione Arquivo INI"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivos INI (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*"
    CommonDialog1.ShowOpen
            
    If Err Then Exit Sub
    
    On Error GoTo Erro:
    
    Dim ret, TxId As String
    
    TxId = InputBox("Informe o TxId:", "Revisar Cobranca", "")
    
    ret = pixcd.RevisarCobranca(CommonDialog1.FileName, TxId)
    rtbRespostas.Text = ret
    
Erro:
    MsgBox Err.Description
End Sub

Private Sub btnCancelarCobranca_Click(Index As Integer)
    On Error GoTo Erro:
    
    Dim ret As String
    
    TxId = InputBox("Informe o TxId:", "Revisar Cobranca", "")
    
    ret = pixcd.CancelarCobranca(TxId)
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
