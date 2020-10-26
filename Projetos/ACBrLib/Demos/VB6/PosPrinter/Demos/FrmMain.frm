VERSION 5.00
Object = "{C932BA88-4374-101B-A56C-00AA003668DC}#1.1#0"; "MSMASK32.OCX"
Object = "{6B7E6392-850A-101B-AFC0-4210102A8DA7}#1.3#0"; "COMCTL32.OCX"
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "COMDLG32.OCX"
Object = "{86CF1D34-0C5F-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCT2.OCX"
Begin VB.Form FrmMain 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "ACBrLibPosPrinter Demo VB6"
   ClientHeight    =   8310
   ClientLeft      =   45
   ClientTop       =   375
   ClientWidth     =   11880
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
   ScaleHeight     =   8310
   ScaleWidth      =   11880
   StartUpPosition =   2  'CenterScreen
   Begin VB.CommandButton btnLeituraStatus 
      Caption         =   "Leitura de Status"
      Height          =   360
      Left            =   9240
      TabIndex        =   86
      Top             =   7800
      Width           =   2535
   End
   Begin VB.CommandButton btnLeituraInfo 
      Caption         =   "Leitura de InformaÁıes"
      Height          =   360
      Left            =   9240
      TabIndex        =   85
      Top             =   7320
      Width           =   2535
   End
   Begin VB.CommandButton btnTagGaveta 
      Caption         =   "Tag de Gaveta"
      Height          =   360
      Left            =   9240
      TabIndex        =   84
      Top             =   6840
      Width           =   2535
   End
   Begin VB.CommandButton btnImpressaoLinha 
      Caption         =   "Teste Impress„o Linha a Linha"
      Height          =   360
      Left            =   6600
      TabIndex        =   83
      Top             =   7800
      Width           =   2535
   End
   Begin VB.CommandButton btnTagQRCode 
      Caption         =   "Tags de QRCode"
      Height          =   360
      Left            =   6600
      TabIndex        =   81
      Top             =   6840
      Width           =   2535
   End
   Begin VB.CommandButton btnTagValidas 
      Caption         =   "Ajuda - Tags Validas"
      Height          =   360
      Left            =   9240
      TabIndex        =   80
      Top             =   6360
      Width           =   2535
   End
   Begin VB.CommandButton btnTagCodbar 
      Caption         =   "Tags de Codigo de Barras"
      Height          =   360
      Left            =   6600
      TabIndex        =   79
      Top             =   6360
      Width           =   2535
   End
   Begin VB.CommandButton btnPaginaCodigo 
      Caption         =   "Teste de P·gina de CÛdigo"
      Height          =   360
      Left            =   3960
      TabIndex        =   78
      Top             =   7800
      Width           =   2535
   End
   Begin VB.CommandButton btnTagInvalidas 
      Caption         =   "Teste de Tags Invalidas"
      Height          =   360
      Left            =   3960
      TabIndex        =   77
      Top             =   7320
      Width           =   2535
   End
   Begin VB.CommandButton btnTagAlinhamento 
      Caption         =   "Tags de Alinhamento"
      Height          =   360
      Left            =   3960
      TabIndex        =   76
      Top             =   6840
      Width           =   2535
   End
   Begin VB.CommandButton btnTagsFormatacao 
      Caption         =   "Tags de FormataÁ„o de Caracter"
      Height          =   360
      Left            =   3960
      TabIndex        =   75
      Top             =   6360
      Width           =   2535
   End
   Begin VB.Frame Frame5 
      Height          =   975
      Left            =   3960
      TabIndex        =   54
      Top             =   5280
      Width           =   7815
      Begin VB.CommandButton btnImprimir 
         Caption         =   "Imprimir"
         Height          =   360
         Left            =   6480
         TabIndex        =   58
         Top             =   360
         Width           =   1215
      End
      Begin VB.CommandButton btnLimpar 
         Caption         =   "Limpar"
         Height          =   360
         Left            =   5160
         TabIndex        =   57
         Top             =   360
         Width           =   1215
      End
      Begin VB.CommandButton btnSenha 
         Caption         =   "Senha"
         Height          =   360
         Left            =   360
         TabIndex        =   56
         Top             =   360
         Width           =   1215
      End
   End
   Begin VB.Frame Frame4 
      Caption         =   "Gaveta"
      Height          =   975
      Left            =   120
      TabIndex        =   50
      Top             =   7200
      Width           =   3735
      Begin VB.CheckBox cbxGVInvertido 
         Caption         =   "Invertido"
         Height          =   255
         Left            =   2640
         TabIndex        =   72
         Top             =   480
         Width           =   975
      End
      Begin MSMask.MaskEdBox MaskGVGaveta 
         Height          =   285
         Left            =   120
         TabIndex        =   51
         Top             =   480
         Width           =   480
         _ExtentX        =   847
         _ExtentY        =   503
         _Version        =   393216
         PromptInclude   =   0   'False
         MaxLength       =   1
         Format          =   "0"
         Mask            =   "1"
         PromptChar      =   "_"
      End
      Begin MSComCtl2.UpDown nudGVGaveta 
         Height          =   285
         Left            =   600
         TabIndex        =   52
         Top             =   480
         Width           =   255
         _ExtentX        =   450
         _ExtentY        =   503
         _Version        =   393216
         BuddyControl    =   "MasColunas"
         BuddyDispid     =   196695
         OrigLeft        =   510
         OrigTop         =   1680
         OrigRight       =   765
         OrigBottom      =   1965
         Max             =   999
         SyncBuddy       =   -1  'True
         BuddyProperty   =   22
         Enabled         =   -1  'True
      End
      Begin MSMask.MaskEdBox MaskGVON 
         Height          =   285
         Left            =   960
         TabIndex        =   68
         Top             =   480
         Width           =   480
         _ExtentX        =   847
         _ExtentY        =   503
         _Version        =   393216
         PromptInclude   =   0   'False
         MaxLength       =   1
         Format          =   "0"
         Mask            =   "0"
         PromptChar      =   "_"
      End
      Begin MSMask.MaskEdBox MaskGVOFF 
         Height          =   285
         Left            =   1800
         TabIndex        =   69
         Top             =   480
         Width           =   480
         _ExtentX        =   847
         _ExtentY        =   503
         _Version        =   393216
         PromptInclude   =   0   'False
         MaxLength       =   1
         Format          =   "0"
         Mask            =   "0"
         PromptChar      =   "_"
      End
      Begin MSComCtl2.UpDown nudGVON 
         Height          =   285
         Left            =   1440
         TabIndex        =   70
         Top             =   480
         Width           =   255
         _ExtentX        =   450
         _ExtentY        =   503
         _Version        =   393216
         BuddyControl    =   "MasColunas"
         BuddyDispid     =   196695
         OrigLeft        =   510
         OrigTop         =   1680
         OrigRight       =   765
         OrigBottom      =   1965
         Max             =   999
         SyncBuddy       =   -1  'True
         BuddyProperty   =   22
         Enabled         =   -1  'True
      End
      Begin MSComCtl2.UpDown nudGVOFF 
         Height          =   285
         Left            =   2280
         TabIndex        =   71
         Top             =   480
         Width           =   255
         _ExtentX        =   450
         _ExtentY        =   503
         _Version        =   393216
         BuddyControl    =   "MasColunas"
         BuddyDispid     =   196695
         OrigLeft        =   510
         OrigTop         =   1680
         OrigRight       =   765
         OrigBottom      =   1965
         Max             =   999
         SyncBuddy       =   -1  'True
         BuddyProperty   =   22
         Enabled         =   -1  'True
      End
      Begin VB.Label Label11 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "OFF"
         Height          =   195
         Left            =   1800
         TabIndex        =   74
         Top             =   240
         Width           =   300
      End
      Begin VB.Label Label10 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "ON"
         Height          =   195
         Left            =   960
         TabIndex        =   73
         Top             =   240
         Width           =   225
      End
      Begin VB.Label Label9 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Gaveta"
         Height          =   195
         Left            =   120
         TabIndex        =   53
         Top             =   240
         Width           =   525
      End
   End
   Begin VB.Frame Frame3 
      Caption         =   "Logotipo"
      Height          =   975
      Left            =   120
      TabIndex        =   46
      Top             =   6120
      Width           =   3735
      Begin MSMask.MaskEdBox MaskLogoKC1 
         Height          =   285
         Left            =   120
         TabIndex        =   47
         Top             =   480
         Width           =   480
         _ExtentX        =   847
         _ExtentY        =   503
         _Version        =   393216
         PromptInclude   =   0   'False
         MaxLength       =   1
         Format          =   "0"
         Mask            =   "0"
         PromptChar      =   "_"
      End
      Begin MSComCtl2.UpDown nudLogoKC1 
         Height          =   285
         Left            =   600
         TabIndex        =   48
         Top             =   480
         Width           =   255
         _ExtentX        =   450
         _ExtentY        =   503
         _Version        =   393216
         BuddyControl    =   "MasColunas"
         BuddyDispid     =   196695
         OrigLeft        =   510
         OrigTop         =   1680
         OrigRight       =   765
         OrigBottom      =   1965
         Max             =   999
         SyncBuddy       =   -1  'True
         BuddyProperty   =   22
         Enabled         =   -1  'True
      End
      Begin MSMask.MaskEdBox MaskLogoKC2 
         Height          =   285
         Left            =   960
         TabIndex        =   60
         Top             =   480
         Width           =   480
         _ExtentX        =   847
         _ExtentY        =   503
         _Version        =   393216
         PromptInclude   =   0   'False
         MaxLength       =   1
         Format          =   "0"
         Mask            =   "0"
         PromptChar      =   "_"
      End
      Begin MSComCtl2.UpDown nudLogoKC2 
         Height          =   285
         Left            =   1440
         TabIndex        =   61
         Top             =   480
         Width           =   255
         _ExtentX        =   450
         _ExtentY        =   503
         _Version        =   393216
         BuddyControl    =   "MasColunas"
         BuddyDispid     =   196695
         OrigLeft        =   510
         OrigTop         =   1680
         OrigRight       =   765
         OrigBottom      =   1965
         Max             =   999
         SyncBuddy       =   -1  'True
         BuddyProperty   =   22
         Enabled         =   -1  'True
      End
      Begin MSMask.MaskEdBox MaskLogoFatorX 
         Height          =   285
         Left            =   1800
         TabIndex        =   62
         Top             =   480
         Width           =   480
         _ExtentX        =   847
         _ExtentY        =   503
         _Version        =   393216
         PromptInclude   =   0   'False
         MaxLength       =   1
         Format          =   "0"
         Mask            =   "0"
         PromptChar      =   "_"
      End
      Begin MSComCtl2.UpDown nudLogoFatorX 
         Height          =   285
         Left            =   2280
         TabIndex        =   63
         Top             =   480
         Width           =   255
         _ExtentX        =   450
         _ExtentY        =   503
         _Version        =   393216
         BuddyControl    =   "MasColunas"
         BuddyDispid     =   196695
         OrigLeft        =   510
         OrigTop         =   1680
         OrigRight       =   765
         OrigBottom      =   1965
         Max             =   999
         SyncBuddy       =   -1  'True
         BuddyProperty   =   22
         Enabled         =   -1  'True
      End
      Begin MSMask.MaskEdBox MaskLogoFatorY 
         Height          =   285
         Left            =   2640
         TabIndex        =   64
         Top             =   480
         Width           =   480
         _ExtentX        =   847
         _ExtentY        =   503
         _Version        =   393216
         PromptInclude   =   0   'False
         MaxLength       =   1
         Format          =   "0"
         Mask            =   "0"
         PromptChar      =   "_"
      End
      Begin MSComCtl2.UpDown nudLogoFatorY 
         Height          =   285
         Left            =   3120
         TabIndex        =   65
         Top             =   480
         Width           =   255
         _ExtentX        =   450
         _ExtentY        =   503
         _Version        =   393216
         BuddyControl    =   "MasColunas"
         BuddyDispid     =   196695
         OrigLeft        =   510
         OrigTop         =   1680
         OrigRight       =   765
         OrigBottom      =   1965
         Max             =   999
         SyncBuddy       =   -1  'True
         BuddyProperty   =   22
         Enabled         =   -1  'True
      End
      Begin VB.Label Label12 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "FatorY"
         Height          =   195
         Left            =   2640
         TabIndex        =   67
         Top             =   240
         Width           =   480
      End
      Begin VB.Label Label7 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "FatorX"
         Height          =   195
         Left            =   1800
         TabIndex        =   66
         Top             =   240
         Width           =   480
      End
      Begin VB.Label Label4 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "KC2"
         Height          =   195
         Left            =   960
         TabIndex        =   59
         Top             =   240
         Width           =   285
      End
      Begin VB.Label Label8 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "KC1"
         Height          =   195
         Left            =   120
         TabIndex        =   49
         Top             =   240
         Width           =   285
      End
   End
   Begin VB.Frame Frame2 
      Caption         =   "QRCode"
      Height          =   975
      Left            =   120
      TabIndex        =   36
      Top             =   5040
      Width           =   3735
      Begin MSComCtl2.UpDown nudQRLargura 
         Height          =   285
         Left            =   1680
         TabIndex        =   37
         Top             =   480
         Width           =   255
         _ExtentX        =   450
         _ExtentY        =   503
         _Version        =   393216
         BuddyControl    =   "MaskEspacos"
         BuddyDispid     =   196694
         OrigLeft        =   1320
         OrigTop         =   1680
         OrigRight       =   1575
         OrigBottom      =   1935
         Max             =   999
         SyncBuddy       =   -1  'True
         BuddyProperty   =   22
         Enabled         =   -1  'True
      End
      Begin MSMask.MaskEdBox MaskQRLargura 
         Height          =   285
         Left            =   960
         TabIndex        =   38
         Top             =   480
         Width           =   720
         _ExtentX        =   1270
         _ExtentY        =   503
         _Version        =   393216
         PromptInclude   =   0   'False
         MaxLength       =   1
         Format          =   "0"
         Mask            =   "0"
         PromptChar      =   "_"
      End
      Begin MSMask.MaskEdBox MaskQRTipo 
         Height          =   285
         Left            =   120
         TabIndex        =   39
         Top             =   480
         Width           =   480
         _ExtentX        =   847
         _ExtentY        =   503
         _Version        =   393216
         PromptInclude   =   0   'False
         MaxLength       =   1
         Format          =   "0"
         Mask            =   "0"
         PromptChar      =   "_"
      End
      Begin MSComCtl2.UpDown nudQRTipo 
         Height          =   285
         Left            =   600
         TabIndex        =   40
         Top             =   480
         Width           =   255
         _ExtentX        =   450
         _ExtentY        =   503
         _Version        =   393216
         BuddyControl    =   "MasColunas"
         BuddyDispid     =   196695
         OrigLeft        =   510
         OrigTop         =   1680
         OrigRight       =   765
         OrigBottom      =   1965
         Max             =   999
         SyncBuddy       =   -1  'True
         BuddyProperty   =   22
         Enabled         =   -1  'True
      End
      Begin MSMask.MaskEdBox MaskQRErrorLevel 
         Height          =   285
         Left            =   2160
         TabIndex        =   44
         Top             =   480
         Width           =   480
         _ExtentX        =   847
         _ExtentY        =   503
         _Version        =   393216
         PromptInclude   =   0   'False
         MaxLength       =   1
         Format          =   "0"
         Mask            =   "0"
         PromptChar      =   "_"
      End
      Begin MSComCtl2.UpDown nudQRErrorLevel 
         Height          =   285
         Left            =   2640
         TabIndex        =   45
         Top             =   480
         Width           =   255
         _ExtentX        =   450
         _ExtentY        =   503
         _Version        =   393216
         BuddyControl    =   "MaskEspacos"
         BuddyDispid     =   196694
         OrigLeft        =   1320
         OrigTop         =   1680
         OrigRight       =   1575
         OrigBottom      =   1935
         Max             =   999
         SyncBuddy       =   -1  'True
         BuddyProperty   =   22
         Enabled         =   -1  'True
      End
      Begin VB.Label Label3 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "ErrorLevel"
         Height          =   195
         Left            =   2160
         TabIndex        =   43
         Top             =   240
         Width           =   735
      End
      Begin VB.Label Label2 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Largura Mod."
         Height          =   195
         Left            =   960
         TabIndex        =   42
         Top             =   240
         Width           =   960
      End
      Begin VB.Label Label1 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Tipo"
         Height          =   195
         Left            =   120
         TabIndex        =   41
         Top             =   240
         Width           =   300
      End
   End
   Begin VB.Frame Frame1 
      Caption         =   "Cod. Barras"
      Height          =   975
      Left            =   120
      TabIndex        =   28
      Top             =   3960
      Width           =   3735
      Begin VB.CheckBox cbxCodbarExibeNumeros 
         Caption         =   "Exibe Numeros"
         Height          =   195
         Left            =   1920
         TabIndex        =   35
         Top             =   480
         Width           =   1455
      End
      Begin MSComCtl2.UpDown nudCodbarAltura 
         Height          =   285
         Left            =   1440
         TabIndex        =   29
         Top             =   480
         Width           =   255
         _ExtentX        =   450
         _ExtentY        =   503
         _Version        =   393216
         BuddyControl    =   "MaskEspacos"
         BuddyDispid     =   196694
         OrigLeft        =   1320
         OrigTop         =   1680
         OrigRight       =   1575
         OrigBottom      =   1935
         Max             =   999
         SyncBuddy       =   -1  'True
         BuddyProperty   =   22
         Enabled         =   -1  'True
      End
      Begin MSMask.MaskEdBox MaskCodbarAltura 
         Height          =   285
         Left            =   960
         TabIndex        =   30
         Top             =   480
         Width           =   480
         _ExtentX        =   847
         _ExtentY        =   503
         _Version        =   393216
         PromptInclude   =   0   'False
         MaxLength       =   1
         Format          =   "0"
         Mask            =   "0"
         PromptChar      =   "_"
      End
      Begin MSMask.MaskEdBox MaskCodbarLargura 
         Height          =   285
         Left            =   120
         TabIndex        =   31
         Top             =   480
         Width           =   480
         _ExtentX        =   847
         _ExtentY        =   503
         _Version        =   393216
         PromptInclude   =   0   'False
         MaxLength       =   1
         Format          =   "0"
         Mask            =   "0"
         PromptChar      =   "_"
      End
      Begin MSComCtl2.UpDown nudCodbarLargura 
         Height          =   285
         Left            =   600
         TabIndex        =   32
         Top             =   480
         Width           =   255
         _ExtentX        =   450
         _ExtentY        =   503
         _Version        =   393216
         BuddyControl    =   "MasColunas"
         BuddyDispid     =   196695
         OrigLeft        =   510
         OrigTop         =   1680
         OrigRight       =   765
         OrigBottom      =   1965
         Max             =   999
         SyncBuddy       =   -1  'True
         BuddyProperty   =   22
         Enabled         =   -1  'True
      End
      Begin VB.Label Label6 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Largura"
         Height          =   195
         Left            =   120
         TabIndex        =   34
         Top             =   240
         Width           =   555
      End
      Begin VB.Label Label5 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Altura"
         Height          =   195
         Left            =   960
         TabIndex        =   33
         Top             =   240
         Width           =   435
      End
   End
   Begin MSComDlg.CommonDialog CommonDialog1 
      Left            =   5400
      Top             =   840
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
   End
   Begin VB.Frame FraConfiguraÁ„o 
      Caption         =   "ConfiguraÁ„o"
      Height          =   3735
      Left            =   120
      TabIndex        =   1
      Top             =   120
      Width           =   3735
      Begin VB.CommandButton btnSerial 
         Caption         =   "Serial"
         Height          =   315
         Left            =   2880
         TabIndex        =   87
         Top             =   1080
         Width           =   630
      End
      Begin VB.ComboBox ComCodePage 
         Height          =   315
         Left            =   1800
         Style           =   2  'Dropdown List
         TabIndex        =   27
         Top             =   3240
         Width           =   1695
      End
      Begin VB.CommandButton cmdArqLog 
         Caption         =   "..."
         Height          =   315
         Left            =   3120
         TabIndex        =   25
         Top             =   2520
         Width           =   390
      End
      Begin VB.TextBox txtArqLog 
         Height          =   315
         Left            =   1800
         TabIndex        =   24
         Text            =   "ArqLog"
         Top             =   2520
         Width           =   1335
      End
      Begin VB.CheckBox chkIgnorarTags 
         Caption         =   "Ignorar Tags"
         Height          =   195
         Left            =   120
         TabIndex        =   22
         Top             =   3360
         Width           =   1335
      End
      Begin VB.CheckBox chkTraduzirTags 
         Caption         =   "Traduzir Tags"
         Height          =   375
         Left            =   120
         TabIndex        =   21
         Top             =   2880
         Value           =   1  'Checked
         Width           =   1335
      End
      Begin VB.CheckBox chkCortarPapel 
         Caption         =   "Cortar Papel"
         Height          =   255
         Left            =   120
         TabIndex        =   20
         Top             =   2520
         Value           =   1  'Checked
         Width           =   1455
      End
      Begin VB.CheckBox chkControlePorta 
         Caption         =   "Controle Porta"
         Height          =   195
         Left            =   120
         TabIndex        =   19
         Top             =   2160
         Value           =   1  'Checked
         Width           =   1455
      End
      Begin MSComCtl2.UpDown updLinhasPular 
         Height          =   285
         Left            =   3116
         TabIndex        =   18
         Top             =   1680
         Width           =   255
         _ExtentX        =   450
         _ExtentY        =   503
         _Version        =   393216
         BuddyControl    =   "MasLinhasPular"
         BuddyDispid     =   196690
         OrigLeft        =   3120
         OrigTop         =   1680
         OrigRight       =   3375
         OrigBottom      =   1965
         Max             =   999
         SyncBuddy       =   -1  'True
         BuddyProperty   =   22
         Enabled         =   -1  'True
      End
      Begin MSMask.MaskEdBox MasLinhasPular 
         Height          =   285
         Left            =   2640
         TabIndex        =   17
         Top             =   1680
         Width           =   480
         _ExtentX        =   847
         _ExtentY        =   503
         _Version        =   393216
         PromptInclude   =   0   'False
         MaxLength       =   1
         Format          =   "0"
         Mask            =   "0"
         PromptChar      =   "_"
      End
      Begin MSComCtl2.UpDown updBuffer 
         Height          =   285
         Left            =   2275
         TabIndex        =   16
         Top             =   1680
         Width           =   255
         _ExtentX        =   450
         _ExtentY        =   503
         _Version        =   393216
         BuddyControl    =   "MasBuffer"
         BuddyDispid     =   196692
         OrigLeft        =   2280
         OrigTop         =   1680
         OrigRight       =   2535
         OrigBottom      =   1935
         Max             =   999
         SyncBuddy       =   -1  'True
         BuddyProperty   =   22
         Enabled         =   -1  'True
      End
      Begin MSMask.MaskEdBox MasBuffer 
         Height          =   285
         Left            =   1800
         TabIndex        =   15
         Top             =   1680
         Width           =   480
         _ExtentX        =   847
         _ExtentY        =   503
         _Version        =   393216
         PromptInclude   =   0   'False
         MaxLength       =   1
         Format          =   "0"
         Mask            =   "0"
         PromptChar      =   "_"
      End
      Begin MSComCtl2.UpDown updEspacos 
         Height          =   285
         Left            =   1440
         TabIndex        =   14
         Top             =   1680
         Width           =   255
         _ExtentX        =   450
         _ExtentY        =   503
         _Version        =   393216
         BuddyControl    =   "MaskEspacos"
         BuddyDispid     =   196694
         OrigLeft        =   1320
         OrigTop         =   1680
         OrigRight       =   1575
         OrigBottom      =   1935
         Max             =   999
         SyncBuddy       =   -1  'True
         BuddyProperty   =   22
         Enabled         =   -1  'True
      End
      Begin MSMask.MaskEdBox MaskEspacos 
         Height          =   285
         Left            =   960
         TabIndex        =   13
         Top             =   1680
         Width           =   480
         _ExtentX        =   847
         _ExtentY        =   503
         _Version        =   393216
         PromptInclude   =   0   'False
         MaxLength       =   1
         Format          =   "0"
         Mask            =   "0"
         PromptChar      =   "_"
      End
      Begin MSMask.MaskEdBox MasColunas 
         Height          =   285
         Left            =   120
         TabIndex        =   12
         Top             =   1680
         Width           =   480
         _ExtentX        =   847
         _ExtentY        =   503
         _Version        =   393216
         PromptInclude   =   0   'False
         MaxLength       =   1
         Format          =   "0"
         Mask            =   "0"
         PromptChar      =   "_"
      End
      Begin MSComCtl2.UpDown updColunas 
         Height          =   285
         Left            =   601
         TabIndex        =   11
         Top             =   1680
         Width           =   255
         _ExtentX        =   450
         _ExtentY        =   503
         _Version        =   393216
         BuddyControl    =   "MasColunas"
         BuddyDispid     =   196695
         OrigLeft        =   510
         OrigTop         =   1680
         OrigRight       =   765
         OrigBottom      =   1965
         Max             =   999
         SyncBuddy       =   -1  'True
         BuddyProperty   =   22
         Enabled         =   -1  'True
      End
      Begin VB.ComboBox ComPorta 
         Height          =   315
         Left            =   120
         TabIndex        =   6
         Text            =   "ComPorta"
         Top             =   1080
         Width           =   2775
      End
      Begin VB.ComboBox ComModelo 
         BeginProperty DataFormat 
            Type            =   0
            Format          =   "0"
            HaveTrueFalseNull=   0
            FirstDayOfWeek  =   0
            FirstWeekOfYear =   0
            LCID            =   1046
            SubFormatType   =   0
         EndProperty
         Height          =   315
         ItemData        =   "FrmMain.frx":25CA
         Left            =   120
         List            =   "FrmMain.frx":25CC
         Style           =   2  'Dropdown List
         TabIndex        =   3
         Top             =   480
         Width           =   2295
      End
      Begin VB.CommandButton btnAtivar 
         Caption         =   "Ativar"
         Height          =   705
         Left            =   2520
         Picture         =   "FrmMain.frx":25CE
         TabIndex        =   2
         Top             =   240
         Width           =   975
      End
      Begin VB.Label lblCodePage 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Code Page"
         Height          =   195
         Left            =   1800
         TabIndex        =   26
         Top             =   3000
         Width           =   780
      End
      Begin VB.Label lblArqLog 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Arq. Log"
         Height          =   195
         Left            =   1800
         TabIndex        =   23
         Top             =   2280
         Width           =   615
      End
      Begin VB.Label lblLinhasPular 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Linhas Pular"
         Height          =   195
         Left            =   2640
         TabIndex        =   10
         Top             =   1440
         Width           =   855
      End
      Begin VB.Label lblBuffer 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Buffer"
         Height          =   195
         Left            =   1800
         TabIndex        =   9
         Top             =   1440
         Width           =   450
      End
      Begin VB.Label lblEspaÁos 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "EspaÁos"
         Height          =   195
         Left            =   960
         TabIndex        =   8
         Top             =   1440
         Width           =   585
      End
      Begin VB.Label lblColunas 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Colunas"
         Height          =   195
         Left            =   120
         TabIndex        =   7
         Top             =   1440
         Width           =   570
      End
      Begin VB.Label lblPorta 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Porta"
         Height          =   195
         Left            =   120
         TabIndex        =   5
         Top             =   840
         Width           =   390
      End
      Begin VB.Label lblModelo 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Modelo"
         Height          =   195
         Left            =   120
         TabIndex        =   4
         Top             =   240
         Width           =   510
      End
   End
   Begin VB.TextBox txtImpressao 
      Height          =   4575
      Left            =   4080
      MultiLine       =   -1  'True
      ScrollBars      =   2  'Vertical
      TabIndex        =   0
      Top             =   600
      Width           =   7455
   End
   Begin ComctlLib.TabStrip TabStrip1 
      Height          =   5175
      Left            =   3960
      TabIndex        =   55
      Top             =   120
      Width           =   7695
      _ExtentX        =   13573
      _ExtentY        =   9128
      _Version        =   327682
      BeginProperty Tabs {0713E432-850A-101B-AFC0-4210102A8DA7} 
         NumTabs         =   1
         BeginProperty Tab1 {0713F341-850A-101B-AFC0-4210102A8DA7} 
            Caption         =   "Texto a Imprimir"
            Key             =   ""
            Object.Tag             =   ""
            ImageVarType    =   2
         EndProperty
      EndProperty
   End
   Begin VB.CommandButton btnTagsLogo 
      Caption         =   "Tags de Logotipo"
      Height          =   360
      Left            =   6600
      TabIndex        =   82
      Top             =   7320
      Width           =   2535
   End
End
Attribute VB_Name = "FrmMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Dim posPrinter As ACBrPosPrinter

Private Sub btnAtivar_Click()

    ToogleActivate
    
End Sub

Private Sub btnImpressaoLinha_Click()
  
    posPrinter.ImprimirLinha ("</zera>")
    posPrinter.ImprimirLinha ("</linha_dupla>")
    posPrinter.ImprimirLinha ("FONTE NORMAL: " + nudColunas.Text + " Colunas")
    'posPrinter.ImprimirLinha (LeftStr("....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8", posPrinter.ColunasFonteNormal))
    'posPrinter.ImprimirLinha ("<e>EXPANDIDO: " & IntToStr(posPrinter.ColunasFonteExpandida) & " Colunas")
    'posPrinter.ImprimirLinha (LeftStr("....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8", posPrinter.ColunasFonteExpandida))
    'posPrinter.ImprimirLinha ("</e><c>CONDENSADO: " + IntToStr(posPrinter.ColunasFonteCondensada) + " Colunas")
    'posPrinter.ImprimirLinha (LeftStr("....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8", posPrinter.ColunasFonteCondensada))
    posPrinter.ImprimirLinha ("</c><n>FONTE NEGRITO</N>")
    posPrinter.ImprimirLinha ("<in>FONTE INVERTIDA</in>")
    posPrinter.ImprimirLinha ("<S>FONTE SUBLINHADA</s>")
    posPrinter.ImprimirLinha ("<i>FONTE ITALICO</i>")
    posPrinter.ImprimirLinha ("FONTE NORMAL")
    posPrinter.ImprimirLinha ("</linha_simples>")
    posPrinter.ImprimirLinha ("<n>LIGA NEGRITO")
    posPrinter.ImprimirLinha ("<i>LIGA ITALICO")
    posPrinter.ImprimirLinha ("<S>LIGA SUBLINHADA")
    posPrinter.ImprimirLinha ("<c>LIGA CONDENSADA")
    posPrinter.ImprimirLinha ("<e>LIGA EXPANDIDA")
    posPrinter.ImprimirLinha ("</fn>FONTE NORMAL")
    posPrinter.ImprimirLinha ("</linha_simples>")
    posPrinter.ImprimirLinha ("<e><n>NEGRITO E EXPANDIDA</n></e>")
    posPrinter.ImprimirLinha ("</fn>FONTE NORMAL")
    posPrinter.ImprimirLinha ("<in><c>INVERTIDA E CONDENSADA</c></in>")
    posPrinter.ImprimirLinha ("</fn>FONTE NORMAL")
    posPrinter.ImprimirLinha ("</linha_simples>")
    posPrinter.ImprimirLinha ("</FB>FONTE TIPO B")
    posPrinter.ImprimirLinha ("<n>FONTE NEGRITO</N>")
    posPrinter.ImprimirLinha ("<e>FONTE EXPANDIDA</e>")
    posPrinter.ImprimirLinha ("<in>FONTE INVERTIDA</in>")
    posPrinter.ImprimirLinha ("<S>FONTE SUBLINHADA</s>")
    posPrinter.ImprimirLinha ("<i>FONTE ITALICO</i>")
    posPrinter.ImprimirLinha ("</FA>FONTE TIPO A")
    posPrinter.ImprimirLinha ("</FN>FONTE NORMAL")
    posPrinter.ImprimirLinha ("</corte_total>")
    

End Sub

Private Sub btnImprimir_Click()

    posPrinter.Imprimir txtImpressao.Text

End Sub


Private Sub ToogleActivate()
    
    If btnSerial.Enabled Then
        SaveConfig
        posPrinter.Ativar
        btnSerial.Enabled = False
        btnAtivar.Value = "Desativar"
    Else
        posPrinter.Desativar
        btnSerial.Enabled = True
        btnAtivar.Value = "Ativar"

End Sub

Private Sub btnLeituraInfo_Click()

    If Not btnSerial.Enabled Then
        ToogleActivate
        
        txtImpressao = posPrinter.LerInfoImpressora
    
End Sub

Private Sub btnLeituraStatus_Click()

       If Not btnSerial.Enabled Then
        ToogleActivate
        
        txtImpressao = posPrinter.LerStatusImpressora

End Sub

Private Sub btnLimpar_Click()

    txtImpressao.Clear

End Sub

Private Sub btnPaginaCodigo_Click()

    With txtImpressao
        .Text = "</zera>" & vbCrLf
        .Text = .Text & "</linha_dupla>" & vbCrLf
        .Text = .Text & "¡…Õ”⁄·ÈÌÛ˙Á«„ı√’ Í¿‡" & vbCrLf
        .Text = .Text & "</corte_total>" & vbCrLf
    End With
         
End Sub

Private Sub btnSenha_Click()

    posPrinter.Imprimir ("<code93>1234 & 0x9 & 5678</code93></corte_total>")

End Sub

Private Sub btnTagAlinhamento_Click()

    With txtImpressao
        .Text = "</zera>" & vbCrLf
        .Text = .Text & "</linha_dupla>" & vbCrLf
        .Text = .Text & "TEXTO NORMAL" & vbCrLf
        .Text = .Text & "</ae>ALINHADO A ESQUERDA" & vbCrLf
        .Text = .Text & "1 2 3 TESTANDO" & vbCrLf
        .Text = .Text & "<n>FONTE NEGRITO</n>" & vbCrLf
        .Text = .Text & "<e>FONTE EXPANDIDA</e>" & vbCrLf
        .Text = .Text & "<a>FONTE ALT.DUPLA</a>" & vbCrLf
        .Text = .Text & "<c>FONTE CONDENSADA</c>" & vbCrLf
        .Text = .Text & "<in>FONTE INVERTIDA</in>" & vbCrLf
        .Text = .Text & "<s>FONTE SUBLINHADA</s>" & vbCrLf
        .Text = .Text & "<i>FONTE ITALICO</i>" & vbCrLf

        .Text = .Text & "</fn></ce>ALINHADO NO CENTRO" & vbCrLf
        .Text = .Text & "1 2 3 TESTANDO" & vbCrLf
        .Text = .Text & "<n>FONTE NEGRITO</n>" & vbCrLf
        .Text = .Text & "<e>FONTE EXPANDIDA</e>" & vbCrLf
        .Text = .Text & "<a>FONTE ALT.DUPLA</a>" & vbCrLf
        .Text = .Text & "<c>FONTE CONDENSADA</c>" & vbCrLf
        .Text = .Text & "<in>FONTE INVERTIDA</in>" & vbCrLf
        .Text = .Text & "<S>FONTE SUBLINHADA</s>" & vbCrLf
        .Text = .Text & "<i>FONTE ITALICO</i>" & vbCrLf

        .Text = .Text & "</fn></ad>ALINHADO A DIREITA" & vbCrLf
        .Text = .Text & "1 2 3 TESTANDO" & vbCrLf
        .Text = .Text & "<n>FONTE NEGRITO</N>" & vbCrLf
        .Text = .Text & "<e>FONTE EXPANDIDA</e>" & vbCrLf
        .Text = .Text & "<a>FONTE ALT.DUPLA</a>" & vbCrLf
        .Text = .Text & "<c>FONTE CONDENSADA</e>" & vbCrLf
        .Text = .Text & "<in>FONTE INVERTIDA</in>" & vbCrLf
        .Text = .Text & "<S>FONTE SUBLINHADA</s>" & vbCrLf
        .Text = .Text & "<i>FONTE ITALICO</i>" & vbCrLf

        .Text = .Text & "</ae></fn>TEXTO NORMAL" & vbCrLf
        .Text = .Text & "</corte_total>" & vbCrLf
    End With

End Sub

Private Sub btnTagCodbar_Click()

    With txtImpressao
        .Text = "</zera>" & vbCrLf
        .Text = .Text & "<barra_mostrar>" & CStr(cbxCodbarExibeNumeros.Checked) & "</barra_mostrar>" & vbCrLf
        .Text = .Text & "<barra_largura>" & CStr(MaskCodbarLargura.Value) & "</barra_largura>" & vbCrLf
        .Text = .Text & "<barra_altura>" & CStr(MaskCodbarAltura.Value) & "</barra_altura>" & vbCrLf
        .Text = .Text & "</ce>" & vbCrLf
        .Text = .Text & "</linha_dupla>" & vbCrLf
        .Text = .Text & "EAN 8: 1234567" & vbCrLf
        .Text = .Text & "<ean8>1234567</ean8>" & vbCrLf
        .Text = .Text & "</Linha_Simples>" & vbCrLf
        .Text = .Text & "EAN13: 123456789012" & vbCrLf
        .Text = .Text & "<ean13>123456789012</ean13>" & vbCrLf
        .Text = .Text & "</Linha_Simples>" & vbCrLf
        .Text = .Text & "std25: 1234567890" & vbCrLf
        .Text = .Text & "<std>1234567890</std>" & vbCrLf
        .Text = .Text & "</Linha_Simples>" & vbCrLf
        .Text = .Text & "INT25: 1234567890" & vbCrLf
        .Text = .Text & "<inter>1234567890</inter>" & vbCrLf
        .Text = .Text & "</Linha_Simples>" & vbCrLf
        .Text = .Text & "CODE11: 1234567890" & vbCrLf
        .Text = .Text & "<code11>1234567890</code11>" & vbCrLf
        .Text = .Text & "</Linha_Simples>" & vbCrLf
        .Text = .Text & "CODE39: ABCDE12345" & vbCrLf
        .Text = .Text & "<code39>ABCDE12345</code39>" & vbCrLf
        .Text = .Text & "</Linha_Simples>" & vbCrLf
        .Text = .Text & "CODE93: ABC123abc" & vbCrLf
        .Text = .Text & "<code93>ABC123abc</code93>" & vbCrLf
        .Text = .Text & "</Linha_Simples>" & vbCrLf
        .Text = .Text & "CODE128: $-=+ABC123abc" & vbCrLf
        .Text = .Text & "<code128>$-=+ABC123abc</code128>" & vbCrLf
        .Text = .Text & "CODE128C: 3515071111111111111159" & vbCrLf
        .Text = .Text & "<code128c>3515071111111111111159</code128c>" & vbCrLf
        
    End With

End Sub

Private Sub btnTagGaveta_Click()

       With txtImpressao
        .Text = "Abertura da Gaveta padr„o" & vbCrLf
        .Text = .Text & "</abre_gaveta>" & vbCrLf
        .Text = .Text & "" & vbCrLf
        .Text = .Text & "" & vbCrLf
        .Text = .Text & "Abertura da Gaveta especÌfica" & vbCrLf
        .Text = .Text & "<abre_gaveta>" + nudGVGaveta.Text + "</abre_gaveta>" & vbCrLf
        .Text = .Text & "</corte_total>" & vbCrLf
     End With

End Sub

Private Sub btnTagInvalidas_Click()

     With txtImpressao
        .Text = "</zera>" & vbCrLf
        .Text = .Text & "<CE>*** TESTE DE TAGS INV¡LIDAS ***</CE>" & vbCrLf
        .Text = .Text & "<ce> <>tags inv·lidas no texto\ & > \ & >><<</CE>" & vbCrLf
        .Text = .Text & "<AD><da><ec></</A Direita</ad>" & vbCrLf
        .Text = .Text & "</corte_total>" & vbCrLf
     End With

End Sub

Private Sub btnTagQRCode_Click()

    With txtImpressao
        .Text = "</zera>" & vbCrLf
        .Text = .Text & "</linha_dupla>" & vbCrLf
        .Text = .Text & "<qrcode_tipo>" & CStr(MaskQRTipo.Value) & "</qrcode_tipo>" & vbCrLf
        .Text = .Text & "<qrcode_largura>" & CStr(MaskQRLargura.Value) & "</qrcode_largura>" & vbCrLf
        .Text = .Text & "<qrcode_error>" & CStr(MaskQRErrorLevel.Value) & "</qrcode_error>" & vbCrLf
        .Text = .Text & "<qrcode>http://projetoacbr.com.br</qrcode>" & vbCrLf
        .Text = .Text & "</ce>" & vbCrLf
        .Text = .Text & "<qrcode>http://www.projetoacbr.com.br/forum/index.php?/page/SAC/sobre_o_sac.html</qrcode>" & vbCrLf
        .Text = .Text & "</ad>" & vbCrLf
        .Text = .Text & "<qrcode>http://www.projetoacbr.com.br/forum/index.php?/page/SAC/questoes_importantes.html</qrcode>" & vbCrLf
        .Text = .Text & "</ce>" & vbCrLf
        .Text = .Text & "Exemplo de QRCode para NFCe" & vbCrLf
        .Text = .Text & "<qrcode_error>0</qrcode_error><qrcode>https://www.homologacao.nfce.fazenda.sp.gov.br/NFCeConsultaPublica/Paginas/ConsultaQRCode.aspx?chNFe=35150805481336000137650220000000711000001960&nVersao=100&tpAmb=2&dhEmi=323031352D30382D31395432323A33333A32352D30333A3030&vNF=3.00&vICMS=0.12&digVal=776967396F2B665861706673396878776E64594C396F61654C35493D&cIdToken=000001&cHashQRCode=9BD312D558823E1EC68CEDB338A39B6150B0480E</qrcode>" & vbCrLf
        .Text = .Text & "Exemplo de QRCode para SAT" & vbCrLf
        .Text = .Text & "<qrcode_error>0</qrcode_error><qrcode>35150811111111111111591234567890001672668828|20150820201736|118.72|05481336000137|TCbeD81ePUpMvso4VjFqRTvs4ovqmR1ZG3bwSCumzHtW8bbMedVJjVnww103v3LxKfgckAyuizcR/9pXaKay6M4Gu8kyDef+6VH5qONIZV1cB+mFfXiaCgeZALuRDCH1PRyb6hoBeRUkUk6lOdXSczRW9Y83GJMXdOFroEbzFmpf4+WOhe2BZ3mEdXKKGMfl1EB0JWnAThkGT+1Er9Jh/3En5YI4hgQP3NC2BiJVJ6oCEbKb85s5915DSZAw4qB/MlESWViDsDVYEnS/FQgA2kP2A9pR4+agdHmgWiz30MJYqX5Ng9XEYvvOMzl1Y6+7/frzsocOxfuQyFsnfJzogw==</qrcode>" & vbCrLf
        .Text = .Text & "</corte_total>" & vbCrLf
    End With

End Sub

Private Sub btnTagsFormatacao_Click()

    With txtImpressao
        .Text = "</zera>" & vbCrLf
        .Text = .Text & "</linha_dupla>" & vbCrLf
        .Text = .Text & "FONTE NORMAL: " & CStr(MasColunas.Value) & " Colunas" & vbCrLf
        .Text = .Text & "</c><n>FONTE NEGRITO</N>" & vbCrLf
        .Text = .Text & "<in>FONTE INVERTIDA</in>" & vbCrLf
        .Text = .Text & "<S>FONTE SUBLINHADA</s>" & vbCrLf
        .Text = .Text & "<i>FONTE ITALICO</i>" & vbCrLf
        .Text = .Text & "FONTE NORMAL" & vbCrLf
        .Text = .Text & "</linha_simples>" & vbCrLf
        .Text = .Text & "<n>LIGA NEGRITO" & vbCrLf
        .Text = .Text & "<i>LIGA ITALICO" & vbCrLf
        .Text = .Text & "<S>LIGA SUBLINHADA" & vbCrLf
        .Text = .Text & "<c>LIGA CONDENSADA" & vbCrLf
        .Text = .Text & "<e>LIGA EXPANDIDA" & vbCrLf
        .Text = .Text & "<a>LIGA ALTURA DUPLA" & vbCrLf
        .Text = .Text & "</fn>FONTE NORMAL" & vbCrLf
        .Text = .Text & "</linha_simples>" & vbCrLf
        .Text = .Text & "<e><n>NEGRITO E EXPANDIDA</n></e>" & vbCrLf
        .Text = .Text & "<c><n>NEGRITO E CONDENSADA</n></c>" & vbCrLf
        .Text = .Text & "<e><a>EXPANDIDA E ALT.DUPLA</a></e>" & vbCrLf
        .Text = .Text & "</fn>FONTE NORMAL" & vbCrLf
        .Text = .Text & "<in><e>INVERTIDA E EXPANDIDA</e></in>" & vbCrLf
        .Text = .Text & "<in><c>INVERTIDA E CONDENSADA</c></in>" & vbCrLf
        .Text = .Text & "<in><a>INVERTIDA E ALT.DUPLA</a></in>" & vbCrLf
        .Text = .Text & "</fn>FONTE NORMAL" & vbCrLf
        .Text = .Text & "</linha_simples>" & vbCrLf
        .Text = .Text & "</FB>FONTE TIPO B" & vbCrLf
        .Text = .Text & "<n>FONTE NEGRITO</N>" & vbCrLf
        .Text = .Text & "<e>FONTE EXPANDIDA</e>" & vbCrLf
        .Text = .Text & "<a>FONTE ALT.DUPLA</a>" & vbCrLf
        .Text = .Text & "<in>FONTE INVERTIDA</in>" & vbCrLf
        .Text = .Text & "<S>FONTE SUBLINHADA</s>" & vbCrLf
        .Text = .Text & "<i>FONTE ITALICO</i>" & vbCrLf
        .Text = .Text & "</FA>FONTE TIPO A" & vbCrLf
        .Text = .Text & "</FN>FONTE NORMAL" & vbCrLf
        .Text = .Text & "</corte_total>" & vbCrLf
        .Text = .Text & "<c>CODE128C: 35150711111111111111591234567890001135408700</c>" & vbCrLf
        .Text = .Text & "<code128c>35150711111111111111591234567890001135408700</code128c>" & vbCrLf
        .Text = .Text & "</Linha_Simples>" & vbCrLf
        .Text = .Text & "UPCA: 12345678901" & vbCrLf
        .Text = .Text & "<upca>12345678901</upca>" & vbCrLf
        .Text = .Text & "</Linha_Simples>" & vbCrLf
        .Text = .Text & "CODABAR: A123456789012345A" & vbCrLf
        .Text = .Text & "<codabar>A123456789012345A</codabar>" & vbCrLf
        .Text = .Text & "</Linha_Simples>" & vbCrLf
        .Text = .Text & "MSI: 1234567890" & vbCrLf
        .Text = .Text & "<msi>1234567890</msi>" & vbCrLf
        .Text = .Text & "</corte_total>"
    End With

End Sub

Private Sub btnTagsLogo_Click()

    With txtImpressao
        .Text = "</zera>" & vbCrLf
        .Text = .Text & "</ce>" & vbCrLf
        .Text = .Text & "<logo_imprimir>" & posPrinter.ConfigLerValor & ACBrComum.SESSAO_POSPRINTER_LOGO & "</logo_imprimir>" & vbCrLf
        .Text = .Text & "<logo_kc1>" & CStr(MaskLogoKC1.Value) & "</logo_kc1>" & vbCrLf
        .Text = .Text & "<logo_kc2>" & CStr(MaskLogoKC2.Value) & "</logo_kc2>" & vbCrLf
        .Text = .Text & "<logo_fatorx>" & CStr(MaskLogoFatorX.Value) & "</logo_fatorx>" & vbCrLf
        .Text = .Text & "<logo_fatory>" & CStr(MaskLogoFatorY.Value) & "</logo_fatory>" & vbCrLf
        .Text = .Text & "</logo>" & vbCrLf
        .Text = .Text & "</corte_total>" & vbCrLf
    End With

End Sub

Private Sub btnTagValidas_Click()
    
    txtImpressao.Clear
    txtImpressao = posPrinter.RetornarTags

End Sub

Private Sub cmdArqLog_Click()
    On Error Resume Next
    CommonDialog1.DialogTitle = "Arquivo de Log"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "Arquivo Log (*.log)|*.log|Todos os arquivos (*.*)|*.*"
    CommonDialog1.ShowSave
        
    If Err Then Exit Sub
    
    Me.txtArqLog.Text = CommonDialog1.FileName
End Sub

Private Sub Form_Load()

    ComModelo.AddItem "ppTexto", ACBrPosPrinterModelo.ppTexto
    ComModelo.AddItem "ppEscPosEpson", ACBrPosPrinterModelo.ppEscPosEpson
    ComModelo.AddItem "ppEscBematech", ACBrPosPrinterModelo.ppEscBematech
    ComModelo.AddItem "ppEscDaruma", ACBrPosPrinterModelo.ppEscDaruma
    ComModelo.AddItem "ppEscVox", ACBrPosPrinterModelo.ppEscVox
    ComModelo.AddItem "ppEscDiebold", ACBrPosPrinterModelo.ppEscDiebold
    ComModelo.AddItem "ppEscEpsonP2", ACBrPosPrinterModelo.ppEscEpsonP2
    ComModelo.AddItem "ppCustomPos", ACBrPosPrinterModelo.ppCustomPos
    ComModelo.AddItem "ppEscPosStar", ACBrPosPrinterModelo.ppEscPosStar
    ComModelo.AddItem "ppEscZJiang", ACBrPosPrinterModelo.ppEscZJiang
    ComModelo.AddItem "ppEscGPrinter", ACBrPosPrinterModelo.ppEscGPrinter
    
    ComModelo.ListIndex = ACBrPosPrinterModelo.ppTexto
    
    ComPorta.AddItem "COM1"
    ComPorta.AddItem "COM2"
    ComPorta.AddItem "LPT1"
    ComPorta.AddItem "LPT2"
    ComPorta.AddItem "\\localhost\Epson"
    ComPorta.AddItem "C:\temp\ecf.txt"
    
    ComPorta.ListIndex = ComPorta.ListCount - 1
    
    ComPorta.AddItem "TCP:192.168.0.31:9100"
    ComPorta.AddItem "RAW:Elgin I9"
    
    ComCodePage.AddItem "None", PosPaginaCodigo.pcNone
    ComCodePage.AddItem "pc437", PosPaginaCodigo.pc437
    ComCodePage.AddItem "pc850", PosPaginaCodigo.pc850
    ComCodePage.AddItem "pc852", PosPaginaCodigo.pc852
    ComCodePage.AddItem "pc860", PosPaginaCodigo.pc860
    ComCodePage.AddItem "pcUTF8", PosPaginaCodigo.pcUTF8
    ComCodePage.AddItem "pc1252", PosPaginaCodigo.pc1252
    
    ComCodePage.ListIndex = PosPaginaCodigo.pc850
    
    updColunas.Value = 0
    updEspacos.Value = 0
    updBuffer.Value = 0
    updLinhasPular.Value = 0
    
    Dim LogPath As String
    
    LogPath = App.Path & "\Docs\"
    
    If Not DirExists(LogPath) Then
        MkDir LogPath
    End If
    
    Set posPrinter = CreatePosPrinter
          
    posPrinter.ConfigGravarValor SESSAO_PRINCIPAL, "LogNivel", "4"
    posPrinter.ConfigGravarValor SESSAO_PRINCIPAL, "LogPath", LogPath
    posPrinter.ConfigGravar
    
    LoadConfig
End Sub

Private Sub Form_Terminate()
    posPrinter.FinalizarLib
    Set posPrinter = Nothing
End Sub

Private Sub cmdAtivar_Click()
    '<EhHeader>
    On Error GoTo catch_error

    '</EhHeader>
    If cmdAtivar.Caption = "Ativar" Then
        SaveConfig
        posPrinter.Ativar
        cmdAtivar.Caption = "Desativar"
    Else
        posPrinter.Desativar
        cmdAtivar.Caption = "Ativar"
    End If

    '<EhFooter>
    Exit Sub
catch_error:
    MsgBox Err.Description, vbExclamation + vbOKOnly, "Application Error"
    Exit Sub
    '</EhFooter>
End Sub

Private Sub LoadConfig()
    posPrinter.ConfigLer
                
    ComModelo.ListIndex = CLng(posPrinter.ConfigLerValor(SESSAO_POSPRINTER, "Modelo"))
    ComPorta.Text = posPrinter.ConfigLerValor(SESSAO_POSPRINTER, "Porta")
    
    updColunas.Value = CLng(posPrinter.ConfigLerValor(SESSAO_POSPRINTER, "ColunasFonteNormal"))
    updEspacos.Value = CLng(posPrinter.ConfigLerValor(SESSAO_POSPRINTER, "EspacoEntreLinhas"))
    updBuffer.Value = CLng(posPrinter.ConfigLerValor(SESSAO_POSPRINTER, "LinhasBuffer"))
    updLinhasPular.Value = CLng(posPrinter.ConfigLerValor(SESSAO_POSPRINTER, "LinhasEntreCupons"))
        
    chkControlePorta.Value = CInt(posPrinter.ConfigLerValor(SESSAO_POSPRINTER, "ControlePorta"))
    chkCortarPapel.Value = CInt(posPrinter.ConfigLerValor(SESSAO_POSPRINTER, "CortaPapel"))
    chkTraduzirTags.Value = CInt(posPrinter.ConfigLerValor(SESSAO_POSPRINTER, "TraduzirTags"))
    chkIgnorarTags.Value = CInt(posPrinter.ConfigLerValor(SESSAO_POSPRINTER, "IgnorarTags"))
    
    txtArqLog.Text = posPrinter.ConfigLerValor(SESSAO_POSPRINTER, "ArqLog")
    ComCodePage.ListIndex = CLng(posPrinter.ConfigLerValor(SESSAO_POSPRINTER, "PaginaDeCodigo"))
End Sub

Private Sub SaveConfig()
    posPrinter.ConfigGravarValor SESSAO_POSPRINTER, "Modelo", CStr(ComModelo.ListIndex)
    posPrinter.ConfigGravarValor SESSAO_POSPRINTER, "Porta", ComPorta.Text
    
    posPrinter.ConfigGravarValor SESSAO_POSPRINTER, "ColunasFonteNormal", CStr(updColunas.Value)
    posPrinter.ConfigGravarValor SESSAO_POSPRINTER, "EspacoEntreLinhas", CStr(updEspacos.Value)
    posPrinter.ConfigGravarValor SESSAO_POSPRINTER, "LinhasBuffer", CStr(updBuffer.Value)
    posPrinter.ConfigGravarValor SESSAO_POSPRINTER, "LinhasEntreCupons", CStr(updLinhasPular.Value)
    
    posPrinter.ConfigGravarValor SESSAO_POSPRINTER, "ControlePorta", CStr(chkControlePorta.Value)
    posPrinter.ConfigGravarValor SESSAO_POSPRINTER, "CortaPapel", CStr(chkCortarPapel.Value)
    posPrinter.ConfigGravarValor SESSAO_POSPRINTER, "TraduzirTags", CStr(chkTraduzirTags.Value)
    posPrinter.ConfigGravarValor SESSAO_POSPRINTER, "IgnorarTags", CStr(chkIgnorarTags.Value)
    
    posPrinter.ConfigGravarValor SESSAO_POSPRINTER, "ArqLog", txtArqLog.Text
    posPrinter.ConfigGravarValor SESSAO_POSPRINTER, "PaginaDeCodigo", CStr(ComCodePage.ListIndex)
                    
    posPrinter.ConfigGravar
End Sub

Private Sub MasBuffer_Change()
    updBuffer.Value = CInt(MasBuffer.Text)
End Sub

Private Sub MasColunas_Change()
    updColunas.Value = CInt(MasColunas.Text)
End Sub

Private Sub MaskEspacos_Change()
    updEspacos.Value = CInt(MaskEspacos.Text)
End Sub

Private Sub MasLinhasPular_Change()
    updLinhasPular.Value = CInt(MasLinhasPular.Text)
End Sub
