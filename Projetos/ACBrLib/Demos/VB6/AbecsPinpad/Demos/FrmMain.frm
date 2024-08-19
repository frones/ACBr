VERSION 5.00
Object = "{BDC217C8-ED16-11CD-956C-0000C04E4C0A}#1.1#0"; "TABCTL32.OCX"
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "COMDLG32.OCX"
Object = "{86CF1D34-0C5F-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCT2.OCX"
Begin VB.Form FrmMain 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "ACBrLibAbecsPinpad"
   ClientHeight    =   5460
   ClientLeft      =   11175
   ClientTop       =   4995
   ClientWidth     =   19365
   BeginProperty Font 
      Name            =   "Tahoma"
      Size            =   8.25
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   Icon            =   "FrmMain.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   5460
   ScaleWidth      =   19365
   StartUpPosition =   2  'CenterScreen
   Begin VB.Frame Frame1 
      Caption         =   "Abrir Comunicação"
      Height          =   1815
      Left            =   120
      TabIndex        =   13
      Top             =   3480
      Width           =   4935
      Begin VB.CommandButton btnOPN 
         Caption         =   "OPN"
         Height          =   435
         Left            =   1560
         Picture         =   "FrmMain.frx":25CA
         TabIndex        =   14
         Top             =   720
         Width           =   1575
      End
   End
   Begin MSComDlg.CommonDialog CommonDialog1 
      Left            =   11520
      Top             =   0
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
   End
   Begin VB.Frame FraConfiguração 
      Caption         =   "Configuração"
      Height          =   3255
      Left            =   120
      TabIndex        =   0
      Top             =   120
      Width           =   4935
      Begin VB.CommandButton btnDesativar 
         Caption         =   "Desativar"
         Height          =   435
         Left            =   3120
         Picture         =   "FrmMain.frx":290C
         TabIndex        =   91
         Top             =   720
         Width           =   1695
      End
      Begin VB.TextBox txtTimeOut 
         Alignment       =   1  'Right Justify
         Height          =   285
         Left            =   120
         TabIndex        =   89
         Text            =   "0"
         Top             =   1200
         Width           =   2535
      End
      Begin VB.CommandButton btnCarregarConfiguracoes 
         Caption         =   "Carregar Configurações"
         Height          =   435
         Left            =   120
         Picture         =   "FrmMain.frx":2C4E
         TabIndex        =   12
         Top             =   2640
         Width           =   2175
      End
      Begin VB.CommandButton btnSalvarConfiguracoes 
         Caption         =   "Salvar Configurações"
         Height          =   435
         Left            =   2640
         Picture         =   "FrmMain.frx":2F90
         TabIndex        =   11
         Top             =   2640
         Width           =   2175
      End
      Begin VB.CheckBox ckbWordWrap 
         Caption         =   "WordWrap"
         Height          =   255
         Left            =   3000
         TabIndex        =   10
         Top             =   2280
         Width           =   1335
      End
      Begin VB.ComboBox cmbMsgAlign 
         Height          =   315
         ItemData        =   "FrmMain.frx":32D2
         Left            =   3000
         List            =   "FrmMain.frx":32E2
         Style           =   2  'Dropdown List
         TabIndex        =   8
         Top             =   1800
         Width           =   1695
      End
      Begin VB.CommandButton cmdArqLog 
         Caption         =   "..."
         Height          =   315
         Left            =   2400
         TabIndex        =   6
         Top             =   1800
         Width           =   390
      End
      Begin VB.TextBox txtArqLogPinpad 
         Height          =   315
         Left            =   120
         TabIndex        =   5
         Text            =   "ArqLog"
         Top             =   1800
         Width           =   2295
      End
      Begin VB.ComboBox cmbPortasCOMPinpad 
         Height          =   315
         ItemData        =   "FrmMain.frx":3309
         Left            =   120
         List            =   "FrmMain.frx":330B
         TabIndex        =   3
         Top             =   600
         Width           =   2895
      End
      Begin VB.CommandButton btnAtivar 
         Caption         =   "Ativar"
         Height          =   435
         Left            =   3120
         Picture         =   "FrmMain.frx":330D
         TabIndex        =   1
         Top             =   240
         Width           =   1695
      End
      Begin MSComCtl2.UpDown nudTimeOut 
         Height          =   285
         Left            =   2640
         TabIndex        =   90
         Top             =   1200
         Width           =   255
         _ExtentX        =   450
         _ExtentY        =   503
         _Version        =   393216
         Value           =   5000
         BuddyControl    =   "txtTimeOut"
         BuddyDispid     =   196613
         OrigLeft        =   3960
         OrigTop         =   720
         OrigRight       =   4215
         OrigBottom      =   975
         Max             =   99999
         SyncBuddy       =   -1  'True
         BuddyProperty   =   0
         Enabled         =   -1  'True
      End
      Begin VB.Label Label1 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "TimeOut"
         Height          =   195
         Left            =   120
         TabIndex        =   9
         Top             =   960
         Width           =   600
      End
      Begin VB.Label lblMsgAlign 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Msg Align"
         Height          =   195
         Left            =   3000
         TabIndex        =   7
         Top             =   1560
         Width           =   675
      End
      Begin VB.Label lblArqLog 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Arq. Log"
         Height          =   195
         Left            =   120
         TabIndex        =   4
         Top             =   1560
         Width           =   615
      End
      Begin VB.Label lblPorta 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "PortaCOM Pinpad"
         Height          =   195
         Left            =   120
         TabIndex        =   2
         Top             =   360
         Width           =   1260
      End
   End
   Begin TabDlg.SSTab SSTab1 
      Height          =   5175
      Left            =   5280
      TabIndex        =   15
      Top             =   120
      Width           =   13935
      _ExtentX        =   24580
      _ExtentY        =   9128
      _Version        =   393216
      Style           =   1
      Tabs            =   5
      TabsPerRow      =   5
      TabHeight       =   520
      TabCaption(0)   =   "Informações"
      TabPicture(0)   =   "FrmMain.frx":364F
      Tab(0).ControlEnabled=   -1  'True
      Tab(0).Control(0)=   "Frame2"
      Tab(0).Control(0).Enabled=   0   'False
      Tab(0).Control(1)=   "Frame3"
      Tab(0).Control(1).Enabled=   0   'False
      Tab(0).Control(2)=   "Frame4"
      Tab(0).Control(2).Enabled=   0   'False
      Tab(0).ControlCount=   3
      TabCaption(1)   =   "Display"
      TabPicture(1)   =   "FrmMain.frx":366B
      Tab(1).ControlEnabled=   0   'False
      Tab(1).Control(0)=   "Frame6"
      Tab(1).Control(0).Enabled=   0   'False
      Tab(1).Control(1)=   "Frame5"
      Tab(1).Control(1).Enabled=   0   'False
      Tab(1).ControlCount=   2
      TabCaption(2)   =   "Multimidia"
      TabPicture(2)   =   "FrmMain.frx":3687
      Tab(2).ControlEnabled=   0   'False
      Tab(2).Control(0)=   "Frame7"
      Tab(2).Control(0).Enabled=   0   'False
      Tab(2).Control(1)=   "SSTab2"
      Tab(2).Control(1).Enabled=   0   'False
      Tab(2).ControlCount=   2
      TabCaption(3)   =   "Perguntas/Eventos"
      TabPicture(3)   =   "FrmMain.frx":36A3
      Tab(3).ControlEnabled=   0   'False
      Tab(3).Control(0)=   "Frame12"
      Tab(3).Control(0).Enabled=   0   'False
      Tab(3).Control(1)=   "Frame11"
      Tab(3).Control(1).Enabled=   0   'False
      Tab(3).Control(2)=   "Frame10"
      Tab(3).Control(2).Enabled=   0   'False
      Tab(3).Control(3)=   "Frame9"
      Tab(3).Control(3).Enabled=   0   'False
      Tab(3).Control(4)=   "Frame8"
      Tab(3).Control(4).Enabled=   0   'False
      Tab(3).ControlCount=   5
      TabCaption(4)   =   "Finalizar Comunicação"
      TabPicture(4)   =   "FrmMain.frx":36BF
      Tab(4).ControlEnabled=   0   'False
      Tab(4).Control(0)=   "Frame14"
      Tab(4).Control(0).Enabled=   0   'False
      Tab(4).Control(1)=   "Frame13"
      Tab(4).Control(1).Enabled=   0   'False
      Tab(4).ControlCount=   2
      Begin VB.Frame Frame14 
         Caption         =   "CLX"
         Height          =   4575
         Left            =   -70440
         TabIndex        =   28
         Top             =   480
         Width           =   9135
         Begin VB.CommandButton btnCLXImagem 
            Caption         =   "CLX Imagem"
            Height          =   435
            Left            =   4800
            Picture         =   "FrmMain.frx":36DB
            TabIndex        =   88
            Top             =   3960
            Width           =   1575
         End
         Begin VB.CommandButton btnCLXMensagem 
            Caption         =   "CLX Mensagem"
            Height          =   435
            Left            =   2760
            Picture         =   "FrmMain.frx":3A1D
            TabIndex        =   87
            Top             =   3960
            Width           =   1575
         End
         Begin VB.TextBox rtbCLXMensagem 
            Height          =   3615
            Left            =   120
            Locked          =   -1  'True
            MultiLine       =   -1  'True
            ScrollBars      =   3  'Both
            TabIndex        =   86
            Text            =   "FrmMain.frx":3D5F
            Top             =   240
            Width           =   8895
         End
      End
      Begin VB.Frame Frame13 
         Caption         =   "CLO"
         Height          =   4575
         Left            =   -75000
         TabIndex        =   27
         Top             =   480
         Width           =   4455
         Begin VB.CommandButton btnCLO 
            Caption         =   "CLO"
            Height          =   435
            Left            =   1200
            Picture         =   "FrmMain.frx":3DBB
            TabIndex        =   85
            Top             =   2400
            Width           =   1575
         End
         Begin VB.TextBox txtCLOLinha2 
            Height          =   315
            Left            =   960
            TabIndex        =   83
            Text            =   "Finalizada"
            Top             =   1920
            Width           =   2295
         End
         Begin VB.TextBox txtCLOLinha1 
            Height          =   315
            Left            =   960
            TabIndex        =   81
            Text            =   "Comunicação"
            Top             =   1200
            Width           =   2295
         End
         Begin VB.Label Label14 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "Linha 2"
            Height          =   195
            Left            =   960
            TabIndex        =   84
            Top             =   1680
            Width           =   510
         End
         Begin VB.Label Label13 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "Linha 1"
            Height          =   195
            Left            =   960
            TabIndex        =   82
            Top             =   960
            Width           =   510
         End
      End
      Begin VB.Frame Frame12 
         Caption         =   "CEX"
         Height          =   4575
         Left            =   -64560
         TabIndex        =   26
         Top             =   480
         Width           =   3375
         Begin VB.TextBox txtTimeOutCEX 
            Alignment       =   1  'Right Justify
            Height          =   285
            Left            =   1680
            TabIndex        =   95
            Text            =   "0"
            Top             =   600
            Width           =   1050
         End
         Begin VB.TextBox rtbRespostaCEX 
            Height          =   975
            Left            =   120
            Locked          =   -1  'True
            MultiLine       =   -1  'True
            ScrollBars      =   3  'Both
            TabIndex        =   80
            Top             =   3480
            Width           =   3135
         End
         Begin VB.CommandButton btnCEX 
            Caption         =   "CEX"
            Height          =   435
            Left            =   960
            Picture         =   "FrmMain.frx":40FD
            TabIndex        =   79
            Top             =   2520
            Width           =   1575
         End
         Begin VB.CheckBox ckbVerifyCTLSPresence 
            Caption         =   "Verify CTLS Presence"
            Height          =   255
            Left            =   240
            TabIndex        =   78
            Top             =   2040
            Width           =   1935
         End
         Begin VB.CheckBox ckbVerifyICCRemoval 
            Caption         =   "Verify ICC Removal"
            Height          =   255
            Left            =   240
            TabIndex        =   77
            Top             =   1680
            Width           =   1695
         End
         Begin VB.CheckBox ckbVerifyICCInsertion 
            Caption         =   "Verify ICC Insertion"
            Height          =   255
            Left            =   240
            TabIndex        =   76
            Top             =   1320
            Width           =   1815
         End
         Begin VB.CheckBox ckbVerifyMagnetic 
            Caption         =   "Verify Magnetic"
            Height          =   255
            Left            =   240
            TabIndex        =   75
            Top             =   960
            Width           =   1455
         End
         Begin VB.CheckBox ckbVerifyKey 
            Caption         =   "Verify Key"
            Height          =   255
            Left            =   240
            TabIndex        =   74
            Top             =   600
            Width           =   1335
         End
         Begin MSComCtl2.UpDown nudTimeOutCEX 
            Height          =   285
            Left            =   2731
            TabIndex        =   96
            Top             =   600
            Width           =   255
            _ExtentX        =   450
            _ExtentY        =   503
            _Version        =   393216
            Value           =   5000
            BuddyControl    =   "txtTimeOutCEX"
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
         Begin VB.Label Label12 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "TimeOut"
            Height          =   195
            Left            =   1680
            TabIndex        =   73
            Top             =   360
            Width           =   600
         End
      End
      Begin VB.Frame Frame11 
         Caption         =   "MNU"
         Height          =   4575
         Left            =   -68160
         TabIndex        =   25
         Top             =   480
         Width           =   3495
         Begin VB.TextBox txtTimeOutMNU 
            Alignment       =   1  'Right Justify
            Height          =   285
            Left            =   1800
            TabIndex        =   97
            Text            =   "0"
            Top             =   1200
            Width           =   1200
         End
         Begin VB.TextBox rtbOpcoesMenu 
            Height          =   2175
            Left            =   120
            MultiLine       =   -1  'True
            ScrollBars      =   3  'Both
            TabIndex        =   71
            Top             =   1200
            Width           =   1575
         End
         Begin VB.TextBox rtbRespostaMNU 
            Height          =   975
            Left            =   120
            Locked          =   -1  'True
            MultiLine       =   -1  'True
            ScrollBars      =   3  'Both
            TabIndex        =   70
            Top             =   3480
            Width           =   3255
         End
         Begin VB.TextBox txtTituloMNU 
            Height          =   315
            Left            =   120
            TabIndex        =   68
            Top             =   480
            Width           =   3255
         End
         Begin VB.CommandButton btnMNU 
            Caption         =   "MNU"
            Height          =   435
            Left            =   1800
            Picture         =   "FrmMain.frx":443F
            TabIndex        =   67
            Top             =   1560
            Width           =   1575
         End
         Begin MSComCtl2.UpDown nudTimeOutMNU 
            Height          =   285
            Left            =   3000
            TabIndex        =   98
            Top             =   1200
            Width           =   255
            _ExtentX        =   450
            _ExtentY        =   503
            _Version        =   393216
            Value           =   5000
            BuddyControl    =   "txtTimeOutMNU"
            BuddyDispid     =   196705
            OrigLeft        =   3960
            OrigTop         =   720
            OrigRight       =   4215
            OrigBottom      =   975
            Max             =   99999
            SyncBuddy       =   -1  'True
            BuddyProperty   =   0
            Enabled         =   -1  'True
         End
         Begin VB.Label Label11 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "Opções"
            Height          =   195
            Left            =   120
            TabIndex        =   72
            Top             =   960
            Width           =   540
         End
         Begin VB.Label Label10 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "Titulo Menu"
            Height          =   195
            Left            =   120
            TabIndex        =   69
            Top             =   240
            Width           =   825
         End
         Begin VB.Label Label9 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "TimeOut"
            Height          =   195
            Left            =   1800
            TabIndex        =   66
            Top             =   960
            Width           =   600
         End
      End
      Begin VB.Frame Frame10 
         Caption         =   "GCD"
         Height          =   4575
         Left            =   -71880
         TabIndex        =   24
         Top             =   480
         Width           =   3615
         Begin VB.TextBox txtTimeOutGCD 
            Alignment       =   1  'Right Justify
            Height          =   285
            Left            =   120
            TabIndex        =   99
            Text            =   "0"
            Top             =   1080
            Width           =   930
         End
         Begin VB.TextBox rtbRespostaGCD 
            Height          =   2895
            Left            =   120
            Locked          =   -1  'True
            MultiLine       =   -1  'True
            ScrollBars      =   3  'Both
            TabIndex        =   65
            Top             =   1560
            Width           =   3375
         End
         Begin VB.CommandButton btnGCD 
            Caption         =   "GCD"
            Height          =   435
            Left            =   1920
            Picture         =   "FrmMain.frx":4781
            TabIndex        =   64
            Top             =   960
            Width           =   1575
         End
         Begin VB.ComboBox cmbMsgIndex 
            Height          =   315
            ItemData        =   "FrmMain.frx":4AC3
            Left            =   120
            List            =   "FrmMain.frx":4B66
            Style           =   2  'Dropdown List
            TabIndex        =   62
            Top             =   480
            Width           =   3375
         End
         Begin MSComCtl2.UpDown nudTimeOutGCD 
            Height          =   285
            Left            =   1051
            TabIndex        =   100
            Top             =   1080
            Width           =   255
            _ExtentX        =   450
            _ExtentY        =   503
            _Version        =   393216
            Value           =   5000
            BuddyControl    =   "txtTimeOutGCD"
            BuddyDispid     =   196659
            OrigLeft        =   3960
            OrigTop         =   720
            OrigRight       =   4215
            OrigBottom      =   975
            Max             =   99999
            SyncBuddy       =   -1  'True
            BuddyProperty   =   0
            Enabled         =   -1  'True
         End
         Begin VB.Label Label8 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "Msg Index"
            Height          =   195
            Left            =   120
            TabIndex        =   63
            Top             =   240
            Width           =   750
         End
         Begin VB.Label Label7 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "TimeOut"
            Height          =   195
            Left            =   120
            TabIndex        =   61
            Top             =   840
            Width           =   600
         End
      End
      Begin VB.Frame Frame9 
         Caption         =   "RMC"
         Height          =   2415
         Left            =   -74880
         TabIndex        =   23
         Top             =   2640
         Width           =   2895
         Begin VB.TextBox txtRMCLinha2 
            Height          =   315
            Left            =   240
            TabIndex        =   60
            Text            =   "FINISHED"
            Top             =   1080
            Width           =   2295
         End
         Begin VB.TextBox txtRMCLinha1 
            Height          =   315
            Left            =   240
            TabIndex        =   59
            Text            =   "OPERATION"
            Top             =   600
            Width           =   2295
         End
         Begin VB.CommandButton btnRMC 
            Caption         =   "RMC"
            Height          =   435
            Left            =   600
            Picture         =   "FrmMain.frx":4ECA
            TabIndex        =   58
            Top             =   1560
            Width           =   1575
         End
      End
      Begin VB.Frame Frame8 
         Caption         =   "GKY"
         Height          =   2055
         Left            =   -74880
         TabIndex        =   22
         Top             =   480
         Width           =   2895
         Begin VB.CommandButton btnGKY 
            Caption         =   "GKY"
            Height          =   435
            Left            =   600
            Picture         =   "FrmMain.frx":520C
            TabIndex        =   57
            Top             =   840
            Width           =   1575
         End
      End
      Begin VB.Frame Frame7 
         Caption         =   "Media Files"
         Height          =   4575
         Left            =   -74880
         TabIndex        =   21
         Top             =   480
         Width           =   4455
         Begin VB.CommandButton btnDMF 
            Caption         =   "Delete (DMF)"
            Height          =   435
            Left            =   3000
            Picture         =   "FrmMain.frx":554E
            TabIndex        =   49
            Top             =   360
            Width           =   1215
         End
         Begin VB.CommandButton btnDSI 
            Caption         =   "Display (DSI)"
            Height          =   435
            Left            =   1560
            Picture         =   "FrmMain.frx":5890
            TabIndex        =   48
            Top             =   360
            Width           =   1215
         End
         Begin VB.CommandButton btnLMF 
            Caption         =   "List (LMF)"
            Height          =   435
            Left            =   120
            Picture         =   "FrmMain.frx":5BD2
            TabIndex        =   47
            Top             =   360
            Width           =   1215
         End
         Begin VB.TextBox rtbRespostaLMF 
            Height          =   3495
            Left            =   120
            Locked          =   -1  'True
            MultiLine       =   -1  'True
            ScrollBars      =   3  'Both
            TabIndex        =   46
            Top             =   960
            Width           =   4215
         End
      End
      Begin VB.Frame Frame6 
         Caption         =   "DEX"
         Height          =   4575
         Left            =   -70320
         TabIndex        =   20
         Top             =   480
         Width           =   9135
         Begin VB.CommandButton btnClearDEX 
            Caption         =   "Clear DEX"
            Height          =   435
            Left            =   4800
            Picture         =   "FrmMain.frx":5F14
            TabIndex        =   45
            Top             =   3960
            Width           =   1575
         End
         Begin VB.CommandButton btnDEX 
            Caption         =   "Display (DEX)"
            Height          =   435
            Left            =   2760
            Picture         =   "FrmMain.frx":6256
            TabIndex        =   44
            Top             =   3960
            Width           =   1575
         End
         Begin VB.TextBox rtbDEXMensagem 
            Height          =   3615
            Left            =   120
            Locked          =   -1  'True
            MultiLine       =   -1  'True
            ScrollBars      =   3  'Both
            TabIndex        =   43
            Text            =   "FrmMain.frx":6598
            Top             =   240
            Width           =   8895
         End
      End
      Begin VB.Frame Frame5 
         Caption         =   "DSP"
         Height          =   4575
         Left            =   -74880
         TabIndex        =   19
         Top             =   480
         Width           =   4455
         Begin VB.CommandButton btnClearDSP 
            Caption         =   "Clear DSP"
            Height          =   435
            Left            =   2400
            Picture         =   "FrmMain.frx":65F4
            TabIndex        =   42
            Top             =   2880
            Width           =   1575
         End
         Begin VB.CommandButton btnDSP 
            Caption         =   "Display (DSP)"
            Height          =   435
            Left            =   480
            Picture         =   "FrmMain.frx":6936
            TabIndex        =   41
            Top             =   2880
            Width           =   1575
         End
         Begin VB.TextBox txtLinha2 
            Height          =   315
            Left            =   1080
            TabIndex        =   39
            Text            =   "ACBrAbecsPinpad"
            Top             =   2160
            Width           =   2295
         End
         Begin VB.TextBox txtLinha1 
            Height          =   315
            Left            =   1080
            TabIndex        =   37
            Text            =   "PROJETO ACBR"
            Top             =   1440
            Width           =   2295
         End
         Begin VB.Label Label4 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "Linha 2"
            Height          =   195
            Left            =   1080
            TabIndex        =   40
            Top             =   1920
            Width           =   510
         End
         Begin VB.Label Label3 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "Linha 1"
            Height          =   195
            Left            =   1080
            TabIndex        =   38
            Top             =   1200
            Width           =   510
         End
      End
      Begin VB.Frame Frame4 
         Caption         =   "GIN"
         Height          =   4575
         Left            =   9240
         TabIndex        =   18
         Top             =   480
         Width           =   4455
         Begin VB.TextBox txtACQIDX 
            Alignment       =   1  'Right Justify
            Height          =   285
            Left            =   360
            TabIndex        =   92
            Text            =   "0"
            Top             =   600
            Width           =   1395
         End
         Begin VB.TextBox rtbRespostaGIN 
            Height          =   3375
            Left            =   240
            Locked          =   -1  'True
            MultiLine       =   -1  'True
            ScrollBars      =   3  'Both
            TabIndex        =   36
            Top             =   1080
            Width           =   3975
         End
         Begin VB.CommandButton btnGIN 
            Caption         =   "GIN"
            Height          =   435
            Left            =   2520
            Picture         =   "FrmMain.frx":6C78
            TabIndex        =   35
            Top             =   480
            Width           =   1575
         End
         Begin MSComCtl2.UpDown nudACQIDX 
            Height          =   285
            Left            =   1756
            TabIndex        =   93
            Top             =   600
            Width           =   255
            _ExtentX        =   450
            _ExtentY        =   503
            _Version        =   393216
            Value           =   5000
            BuddyControl    =   "txtACQIDX"
            BuddyDispid     =   196687
            OrigLeft        =   3960
            OrigTop         =   720
            OrigRight       =   4215
            OrigBottom      =   975
            Max             =   99999
            SyncBuddy       =   -1  'True
            BuddyProperty   =   0
            Enabled         =   -1  'True
         End
         Begin VB.Label Label2 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "GIN_ACQIDX"
            Height          =   195
            Left            =   360
            TabIndex        =   34
            Top             =   360
            Width           =   945
         End
      End
      Begin VB.Frame Frame3 
         Caption         =   "PinPad Capabilities"
         Height          =   4575
         Left            =   4680
         TabIndex        =   17
         Top             =   480
         Width           =   4710
         Begin VB.CommandButton btnPinPadCapabilities 
            Caption         =   "PinPadCapabilities"
            Height          =   435
            Left            =   120
            Picture         =   "FrmMain.frx":6FBA
            TabIndex        =   33
            Top             =   3960
            Width           =   4215
         End
         Begin VB.TextBox rtbRespostaPinPadCapabilities 
            Height          =   3615
            Left            =   120
            Locked          =   -1  'True
            MultiLine       =   -1  'True
            ScrollBars      =   3  'Both
            TabIndex        =   32
            Top             =   240
            Width           =   4215
         End
      End
      Begin VB.Frame Frame2 
         Caption         =   "GIX"
         Height          =   4575
         Left            =   120
         TabIndex        =   16
         Top             =   480
         Width           =   4455
         Begin VB.ListBox List1 
            Height          =   3570
            ItemData        =   "FrmMain.frx":72FC
            Left            =   120
            List            =   "FrmMain.frx":73C0
            TabIndex        =   31
            Top             =   240
            Width           =   1815
         End
         Begin VB.CommandButton btnGIX 
            Caption         =   "GIX"
            Height          =   435
            Left            =   240
            Picture         =   "FrmMain.frx":76C6
            TabIndex        =   30
            Top             =   3960
            Width           =   1575
         End
         Begin VB.TextBox rtbRespostaGIX 
            Height          =   3615
            Left            =   2040
            Locked          =   -1  'True
            MultiLine       =   -1  'True
            ScrollBars      =   3  'Both
            TabIndex        =   29
            Top             =   240
            Width           =   2295
         End
      End
      Begin TabDlg.SSTab SSTab2 
         Height          =   4455
         Left            =   -70320
         TabIndex        =   50
         Top             =   600
         Width           =   3855
         _ExtentX        =   6800
         _ExtentY        =   7858
         _Version        =   393216
         Style           =   1
         Tabs            =   1
         TabsPerRow      =   1
         TabHeight       =   520
         TabCaption(0)   =   "Imagem"
         TabPicture(0)   =   "FrmMain.frx":7A08
         Tab(0).ControlEnabled=   -1  'True
         Tab(0).Control(0)=   "Label5"
         Tab(0).Control(0).Enabled=   0   'False
         Tab(0).Control(1)=   "Label6"
         Tab(0).Control(1).Enabled=   0   'False
         Tab(0).Control(2)=   "btnCarregarImagem"
         Tab(0).Control(2).Enabled=   0   'False
         Tab(0).Control(3)=   "txtPathImagem"
         Tab(0).Control(3).Enabled=   0   'False
         Tab(0).Control(4)=   "cmbTipoImagem"
         Tab(0).Control(4).Enabled=   0   'False
         Tab(0).Control(5)=   "btnLoadMedia"
         Tab(0).Control(5).Enabled=   0   'False
         Tab(0).Control(6)=   "rtbRespostaLoadImage"
         Tab(0).Control(6).Enabled=   0   'False
         Tab(0).ControlCount=   7
         Begin VB.TextBox rtbRespostaLoadImage 
            Height          =   2415
            Left            =   120
            Locked          =   -1  'True
            MultiLine       =   -1  'True
            ScrollBars      =   3  'Both
            TabIndex        =   94
            Top             =   1800
            Width           =   3495
         End
         Begin VB.CommandButton btnLoadMedia 
            Caption         =   "LoadMedia"
            Height          =   435
            Left            =   1920
            Picture         =   "FrmMain.frx":7A24
            TabIndex        =   56
            Top             =   1200
            Width           =   1575
         End
         Begin VB.ComboBox cmbTipoImagem 
            Height          =   315
            ItemData        =   "FrmMain.frx":7D66
            Left            =   120
            List            =   "FrmMain.frx":7D73
            Style           =   2  'Dropdown List
            TabIndex        =   54
            Top             =   1320
            Width           =   1695
         End
         Begin VB.TextBox txtPathImagem 
            Height          =   315
            Left            =   120
            TabIndex        =   52
            Top             =   720
            Width           =   2895
         End
         Begin VB.CommandButton btnCarregarImagem 
            Caption         =   "..."
            Height          =   315
            Left            =   3120
            TabIndex        =   51
            Top             =   720
            Width           =   390
         End
         Begin VB.Label Label6 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "Tipo Imagem"
            Height          =   195
            Left            =   120
            TabIndex        =   55
            Top             =   1080
            Width           =   915
         End
         Begin VB.Label Label5 
            AutoSize        =   -1  'True
            BackStyle       =   0  'Transparent
            Caption         =   "Path Imagem"
            Height          =   195
            Left            =   120
            TabIndex        =   53
            Top             =   480
            Width           =   945
         End
      End
   End
End
Attribute VB_Name = "FrmMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Dim abecsPinpad As ACBrAbecsPinpad

Private Sub btnAtivar_Click()
    On Error GoTo Erro:
    
    abecsPinpad.Ativar
    
Erro:
    MsgBox Err.Description
End Sub

Private Sub btnCarregarImagem_Click()
    On Error Resume Next
    CommonDialog1.DialogTitle = "Arquivo de Imagem"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivo Log (*.png, *.jpg, *.gif)|*.png; *.jpg; *.gif|Todos os arquivos (*.*)|*.*"
    CommonDialog1.ShowSave
        
    If Err Then Exit Sub
    
    txtPathImagem.Text = CommonDialog1.FileName
End Sub

Private Sub btnCEX_Click()
    On Error GoTo Erro:
    
    Dim ret As String
    
    If ckbVerifyKey.Value Then
        abecsPinpad.DEX ("Press Key")
        ret = abecsPinpad.CEX(True, False, False, False, False, txtTimeOutCEX.Text)
        rtbRespostaCEX.Text = ret
        abecsPinpad.DEX ("")
    ElseIf ckbVerifyMagnetic.Value Then
        abecsPinpad.DEX ("Swipe the card")
        ret = abecsPinpad.CEX(False, True, False, False, False, txtTimeOutCEX.Text)
        rtbRespostaCEX.Text = ret
        abecsPinpad.DEX ("")
    ElseIf ckbVerifyICCInsertion.Value Then
        abecsPinpad.DEX ("Insert card")
        ret = abecsPinpad.CEX(False, False, True, False, False, txtTimeOutCEX.Text)
        rtbRespostaCEX.Text = ret
        abecsPinpad.DEX ("")
    ElseIf ckbVerifyICCRemoval.Value Then
        abecsPinpad.DEX ("Remove card")
        ret = abecsPinpad.CEX(False, False, False, True, False, txtTimeOutCEX.Text)
        rtbRespostaCEX.Text = ret
        abecsPinpad.DEX ("")
    ElseIf ckbVerifyCTLSPresence.Value Then
        abecsPinpad.DEX ("Bring card closer")
        ret = abecsPinpad.CEX(False, False, False, False, True, txtTimeOutCEX.Text)
        rtbRespostaCEX.Text = ret
        abecsPinpad.DEX ("")
    End If
    
Erro:
    MsgBox Err.Description
End Sub

Private Sub btnClearDEX_Click()
    abecsPinpad.DEX ("")
End Sub

Private Sub btnClearDSP_Click()
    abecsPinpad.DSP ("")
End Sub

Private Sub btnCLO_Click()
    abecsPinpad.CLO (txtCLOLinha1.Text + vbCrLf + txtCLOLinha2.Text)
End Sub

Private Sub btnCLXImagem_Click()
    Dim ret As String
    Dim MensagemOuNomeImagem As String
    
    MensagemOuNomeImagem = InputBox("Comando CLX", "Informe o Nome da Imagem", "")
    abecsPinpad.CLX (MensagemOuNomeImagem)
End Sub

Private Sub btnCLXMensagem_Click()
    abecsPinpad.CLX (rtbCLXMensagem.Text)
End Sub

Private Sub btnDesativar_Click()
    On Error GoTo Erro:
    
    abecsPinpad.Desativar
    
Erro:
    MsgBox Err.Description
End Sub

Private Sub btnCarregarConfiguracoes_Click()
    LoadConfig
End Sub

Private Sub btnDEX_Click()
    abecsPinpad.DEX (rtbDEXMensagem.Text)
End Sub

Private Sub btnDMF_Click()
    On Error GoTo Erro:
    
    Dim ret As String
    Dim NomeArquivo As String
    
    NomeArquivo = InputBox("Comando DMF", "Informe o Nome da Imagem", "")
    abecsPinpad.DMF (NomeArquivo)

Erro:
    MsgBox Err.Description
End Sub

Private Sub btnDSI_Click()
    On Error GoTo Erro:
    
    Dim ret As String
    Dim NomeArquivo As String
    
    NomeArquivo = InputBox("Comando DSI", "Informe o Nome da Imagem", "")
    abecsPinpad.DSI (NomeArquivo)

Erro:
    MsgBox Err.Description
End Sub

Private Sub btnDSP_Click()
    abecsPinpad.DSP (txtLinha1.Text + vbCrLf + txtLinha2.Text)
End Sub

Private Sub btnGCD_Click()
    On Error GoTo Erro:
    
    Dim ret As String
    
    ret = abecsPinpad.GCD(cmbMsgIndex.ListIndex, txtTimeOutGCD.Text)
    rtbRespostaGCD.Text = ret
    
Erro:
    MsgBox Err.Description
End Sub

Private Sub btnGIN_Click()
    On Error GoTo Erro:
    
    Dim ret As String
    
    ret = abecsPinpad.GIN(txtACQIDX.Text)
    rtbRespostaGIN.Text = ret
    
Erro:
    MsgBox Err.Description
    
End Sub

Private Sub btnGIX_Click()
    On Error GoTo Erro:

    Dim ret As String
    Dim PPDATA As String
    
    PPDATA = InputBox("Comando GIX", "Informe PPDATA:", "")
    ret = abecsPinpad.GIX(PPDATA)
    rtbRespostaGIX.Text = ret
    
Erro:
    MsgBox Err.Description
End Sub

Private Sub btnGKY_Click()
    On Error GoTo Erro:

    abecsPinpad.GKY
    
Erro:
    MsgBox Err.Description
End Sub

Private Sub btnLMF_Click()
    On Error GoTo Erro:

    rtbRespostaLMF.Text = abecsPinpad.LMF
    
Erro:
    MsgBox Err.Description
End Sub

Private Sub btnLoadMedia_Click()
    On Error GoTo Erro:
    
    Dim ret As String
    
    ret = abecsPinpad.LoadMedia(txtPathImagem.Text, cmbTipoImagem.ListIndex)
    rtbRespostaLoadImage.Text = ret
    
Erro:
    MsgBox Err.Description
End Sub

Private Sub btnMNU_Click()
    On Error GoTo Erro:
    
    Dim ret As String
    
    ret = abecsPinpad.MNU(rtbOpcoesMenu.Text, txtTituloMNU, txtTimeOutMNU.Text)
    rtbRespostaMNU.Text = ret
    
Erro:
    MsgBox Err.Description
End Sub

Private Sub btnOPN_Click()
    On Error GoTo Erro:
    
    abecsPinpad.OPN
    
Erro:
    MsgBox Err.Description
End Sub

Private Sub btnPinPadCapabilities_Click()
    On Error GoTo Erro:
    
    Dim ret As String
    
    ret = abecsPinpad.PinPadCapabilities
    rtbRespostaPinPadCapabilities.Text = ret
    
Erro:
    MsgBox Err.Description
End Sub

Private Sub btnRMC_Click()
    abecsPinpad.RMC (txtRMCLinha1.Text + vbCrLf + txtRMCLinha2.Text)
End Sub

Private Sub btnSalvarConfiguracoes_Click()
    SaveConfig
End Sub

Private Sub cmdArqLog_Click()
    On Error Resume Next
    CommonDialog1.DialogTitle = "Arquivo de Log"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivo Log (*.log)|*.log|Todos os arquivos (*.*)|*.*"
    CommonDialog1.ShowSave
        
    If Err Then Exit Sub
    
    txtArqLogPinpad.Text = CommonDialog1.FileName
End Sub

Private Sub Form_Load()

    cmbPortasCOMPinpad.AddItem "COM1"
    cmbPortasCOMPinpad.AddItem "COM2"
    cmbPortasCOMPinpad.AddItem "COM3"
    cmbPortasCOMPinpad.AddItem "COM4"
    cmbPortasCOMPinpad.AddItem "COM5"
    cmbPortasCOMPinpad.AddItem "COM6"
    cmbPortasCOMPinpad.AddItem "COM7"
    cmbPortasCOMPinpad.AddItem "COM8"
    cmbPortasCOMPinpad.AddItem "COM9"
    cmbPortasCOMPinpad.AddItem "COM10"
    
    cmbPortasCOMPinpad.ListIndex = cmbPortasCOMPinpad.ListCount - 1
    
    Dim LogPath As String
    
    LogPath = App.Path & "\Logs\"
    
    If Not DirExists(LogPath) Then
        MkDir LogPath
    End If
    
    Set abecsPinpad = CreateAbecsPinpad
          
    Dim porta
    porta = cmbPortasCOMPinpad.Text
    abecsPinpad.ConfigGravarValor SESSAO_AbecsPinpad, "PortaPinpad", CStr(porta)
    
    abecsPinpad.ConfigGravarValor SESSAO_AbecsPinpad, "LogFile", CStr(txtArqLogPinpad.Text)
    abecsPinpad.ConfigGravarValor SESSAO_PRINCIPAL, "LogNivel", "4"
    abecsPinpad.ConfigGravarValor SESSAO_PRINCIPAL, "LogPath", LogPath
    abecsPinpad.ConfigGravar
    
    LoadConfig
End Sub

Private Sub Form_Terminate()
    abecsPinpad.FinalizarLib
    Set abecsPinpad = Nothing
End Sub

Private Sub LoadConfig()
    abecsPinpad.ConfigLer
    
    Dim porta
    porta = cmbPortasCOMPinpad.Text
    porta = abecsPinpad.ConfigLerValor(SESSAO_AbecsPinpad, "PortaPinpad")
    
    nudTimeOut.Value = CLng(abecsPinpad.ConfigLerValor(SESSAO_AbecsPinpad, "TimeOut"))
    txtArqLogPinpad.Text = abecsPinpad.ConfigLerValor(SESSAO_AbecsPinpad, "LogFile")
    cmbMsgAlign.ListIndex = CLng(abecsPinpad.ConfigLerValor(SESSAO_AbecsPinpad, "MsgAlign"))
    ckbWordWrap.Value = CLng(abecsPinpad.ConfigLerValor(SESSAO_AbecsPinpad, "MsgWordWrap"))
End Sub

Private Sub SaveConfig()
    
    Dim porta
    porta = cmbPortasCOMPinpad.Text
    abecsPinpad.ConfigGravarValor SESSAO_AbecsPinpad, "PortaPinpad", CStr(porta)
    
    abecsPinpad.ConfigGravarValor SESSAO_AbecsPinpad, "TimeOut", CStr(nudTimeOut.Value)
    abecsPinpad.ConfigGravarValor SESSAO_AbecsPinpad, "LogFile", txtArqLogPinpad.Text
    abecsPinpad.ConfigGravarValor SESSAO_AbecsPinpad, "MsgAlign", CStr(cmbMsgAlign.ListIndex)
    abecsPinpad.ConfigGravarValor SESSAO_AbecsPinpad, "MsgWordWrap", CStr(ckbWordWrap.Value)
    
    abecsPinpad.ConfigGravar
End Sub
