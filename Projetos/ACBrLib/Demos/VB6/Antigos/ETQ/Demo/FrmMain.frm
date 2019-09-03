VERSION 5.00
Object = "{86CF1D34-0C5F-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCT2.OCX"
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "COMDLG32.OCX"
Begin VB.Form FrmMain 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "ACBrLibETQ Demo"
   ClientHeight    =   3450
   ClientLeft      =   45
   ClientTop       =   390
   ClientWidth     =   11205
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
   ScaleHeight     =   3450
   ScaleWidth      =   11205
   StartUpPosition =   2  'CenterScreen
   Begin VB.Frame FraImpressão 
      Caption         =   "Impressão"
      Height          =   3135
      Left            =   9240
      TabIndex        =   2
      Top             =   120
      Width           =   1815
      Begin VB.CommandButton cmdImprimirImagem 
         Caption         =   "Imprimir Imagem"
         Height          =   360
         Left            =   120
         TabIndex        =   30
         Top             =   2640
         Width           =   1590
      End
      Begin VB.CommandButton cmdBlocoDe 
         Caption         =   "Bloco de Etiquetas"
         Height          =   360
         Left            =   120
         TabIndex        =   29
         Top             =   1800
         Width           =   1590
      End
      Begin VB.CommandButton cmdEtiqueta3 
         Caption         =   "Etiqueta 3 Colunas"
         Height          =   360
         Left            =   120
         TabIndex        =   28
         Top             =   1320
         Width           =   1590
      End
      Begin VB.CommandButton cmdEtiquetaSimples 
         Caption         =   "Etiqueta Simples"
         Height          =   360
         Left            =   120
         TabIndex        =   27
         Top             =   840
         Width           =   1590
      End
      Begin MSComCtl2.UpDown nudCopias 
         Height          =   285
         Left            =   1440
         TabIndex        =   26
         Top             =   480
         Width           =   255
         _ExtentX        =   450
         _ExtentY        =   503
         _Version        =   393216
         Value           =   1
         AutoBuddy       =   -1  'True
         BuddyControl    =   "txtCopias"
         BuddyDispid     =   196614
         OrigLeft        =   1200
         OrigTop         =   480
         OrigRight       =   1455
         OrigBottom      =   735
         Max             =   99999
         Min             =   1
         SyncBuddy       =   -1  'True
         BuddyProperty   =   65547
         Enabled         =   -1  'True
      End
      Begin VB.TextBox txtCopias 
         Height          =   285
         Left            =   120
         TabIndex        =   25
         Text            =   "1"
         Top             =   480
         Width           =   1320
      End
      Begin VB.Label lblNºDe 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Nº de Copias"
         Height          =   195
         Left            =   120
         TabIndex        =   24
         Top             =   240
         Width           =   930
      End
   End
   Begin VB.Frame FraCarregarImagem 
      Caption         =   "Carregar Imagem"
      Height          =   3135
      Left            =   4320
      TabIndex        =   1
      Top             =   120
      Width           =   4815
      Begin VB.CommandButton cmdCarregarImagem 
         Caption         =   "Carregar Imagem"
         Height          =   600
         Left            =   240
         TabIndex        =   23
         Top             =   2400
         Width           =   2070
      End
      Begin VB.TextBox txtNomeImagem 
         Height          =   285
         Left            =   120
         TabIndex        =   22
         Text            =   "ACBR"
         Top             =   1920
         Width           =   4575
      End
      Begin VB.Label lblNomeImagem 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Nome Imagem Memória"
         Height          =   195
         Left            =   120
         TabIndex        =   21
         Top             =   1680
         Width           =   1665
      End
      Begin VB.Image pictureBox 
         Appearance      =   0  'Flat
         BorderStyle     =   1  'Fixed Single
         Height          =   1335
         Left            =   120
         Picture         =   "FrmMain.frx":25CA
         Stretch         =   -1  'True
         Top             =   240
         Width           =   4575
      End
   End
   Begin VB.Frame FraConfiguraçõesDa 
      Caption         =   "Configurações da Impressora"
      Height          =   3135
      Left            =   120
      TabIndex        =   0
      Top             =   120
      Width           =   4095
      Begin MSComDlg.CommonDialog CommonDialog1 
         Left            =   2880
         Top             =   2280
         _ExtentX        =   847
         _ExtentY        =   847
         _Version        =   393216
      End
      Begin VB.CheckBox chkLimparMemoria 
         Caption         =   "Limpar Memoria"
         Height          =   195
         Left            =   120
         TabIndex        =   20
         Top             =   2760
         Width           =   1695
      End
      Begin MSComCtl2.UpDown nudAvanco 
         Height          =   285
         Left            =   1680
         TabIndex        =   19
         Top             =   2280
         Width           =   255
         _ExtentX        =   450
         _ExtentY        =   503
         _Version        =   393216
         Value           =   600
         AutoBuddy       =   -1  'True
         BuddyControl    =   "txtAvanco"
         BuddyDispid     =   196623
         OrigLeft        =   1680
         OrigTop         =   2280
         OrigRight       =   1935
         OrigBottom      =   2535
         Max             =   99999
         SyncBuddy       =   -1  'True
         BuddyProperty   =   65547
         Enabled         =   -1  'True
      End
      Begin VB.TextBox txtAvanco 
         Height          =   285
         Left            =   120
         TabIndex        =   18
         Text            =   "600"
         Top             =   2280
         Width           =   1560
      End
      Begin MSComCtl2.UpDown nudVelocidade 
         Height          =   285
         Left            =   1680
         TabIndex        =   16
         Top             =   1680
         Width           =   255
         _ExtentX        =   450
         _ExtentY        =   503
         _Version        =   393216
         Value           =   -2
         AutoBuddy       =   -1  'True
         BuddyControl    =   "txtVelocidade"
         BuddyDispid     =   196624
         OrigLeft        =   1680
         OrigTop         =   1680
         OrigRight       =   1935
         OrigBottom      =   2055
         Max             =   99999
         Min             =   -99999
         SyncBuddy       =   -1  'True
         BuddyProperty   =   65547
         Enabled         =   -1  'True
      End
      Begin VB.TextBox txtVelocidade 
         Height          =   285
         Left            =   120
         TabIndex        =   15
         Text            =   "-2"
         Top             =   1680
         Width           =   1560
      End
      Begin MSComCtl2.UpDown nudTemperatura 
         Height          =   285
         Left            =   1680
         TabIndex        =   13
         Top             =   1080
         Width           =   255
         _ExtentX        =   450
         _ExtentY        =   503
         _Version        =   393216
         Value           =   1
         AutoBuddy       =   -1  'True
         BuddyControl    =   "txtTemperatura"
         BuddyDispid     =   196625
         OrigLeft        =   1680
         OrigTop         =   1080
         OrigRight       =   1935
         OrigBottom      =   1335
         Max             =   99999
         SyncBuddy       =   -1  'True
         BuddyProperty   =   65547
         Enabled         =   -1  'True
      End
      Begin VB.TextBox txtTemperatura 
         Height          =   285
         Left            =   120
         TabIndex        =   12
         Text            =   "10"
         Top             =   1080
         Width           =   1560
      End
      Begin VB.ComboBox cmbBackFeed 
         Height          =   315
         Left            =   2040
         Style           =   2  'Dropdown List
         TabIndex        =   10
         Top             =   1680
         Width           =   1935
      End
      Begin VB.ComboBox cmbDPI 
         Height          =   315
         Left            =   2040
         Style           =   2  'Dropdown List
         TabIndex        =   7
         Top             =   1080
         Width           =   1935
      End
      Begin VB.ComboBox cmbModelo 
         Height          =   315
         Left            =   2040
         Style           =   2  'Dropdown List
         TabIndex        =   5
         Top             =   480
         Width           =   1935
      End
      Begin VB.ComboBox cmbPorta 
         Height          =   315
         Left            =   120
         TabIndex        =   3
         Text            =   "cmbPorta"
         Top             =   480
         Width           =   1815
      End
      Begin VB.Label lblAvançoEtiqueta 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Avanço Etiqueta"
         Height          =   195
         Left            =   120
         TabIndex        =   17
         Top             =   2040
         Width           =   1185
      End
      Begin VB.Label lblVelocidade 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Velocidade"
         Height          =   195
         Left            =   120
         TabIndex        =   14
         Top             =   1440
         Width           =   765
      End
      Begin VB.Label Label3 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Temperatura"
         Height          =   195
         Left            =   120
         TabIndex        =   11
         Top             =   840
         Width           =   930
      End
      Begin VB.Label Label2 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "BackFeed"
         Height          =   195
         Left            =   2040
         TabIndex        =   9
         Top             =   1440
         Width           =   690
      End
      Begin VB.Label Label1 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "DPI Impressora"
         Height          =   195
         Left            =   2040
         TabIndex        =   8
         Top             =   840
         Width           =   1110
      End
      Begin VB.Label lblModelo 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Modelo"
         Height          =   195
         Left            =   2040
         TabIndex        =   6
         Top             =   240
         Width           =   510
      End
      Begin VB.Label lblPorta 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Porta"
         Height          =   195
         Left            =   120
         TabIndex        =   4
         Top             =   240
         Width           =   390
      End
   End
End
Attribute VB_Name = "FrmMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Private Sub cmdBlocoDe_Click()
    SaveConfig
    
    Dim ret As Long
    
    ret = ETQ_Ativar()
    CheckResult retorno
    
    If (cmbModelo.ListIndex = 1) Or (cmbModelo.ListIndex = 2) Then
        ret = ETQ_IniciarEtiqueta()
        CheckResult ret

        ret = ETQ_ImprimirTexto(0, 2, 2, 2, 3, 3, "BISCOITO MARILAN RECH 335G", 0, True)
        CheckResult ret

        ret = ETQ_ImprimirTexto(0, 2, 2, 1, 8, 3, "CHOC BRANCO", 0, False)
        CheckResult ret

        ret = ETQ_ImprimirBarras(0, 0, 2, 2, 13, 5, "7896003701685", 10, 1)
        CheckResult ret

        ret = ETQ_ImprimirTexto(0, 3, 3, 2, 18, 35, "R$", 0, False)
        CheckResult ret

        ret = ETQ_ImprimirTexto(0, 3, 4, 4, 15, 50, "20,59", 0, False)
        CheckResult ret

        ret = ETQ_FinalizarEtiqueta(nudCopias.Value, nudAvanco.Value)
        CheckResult ret

        ret = ETQ_IniciarEtiqueta()
        CheckResult ret

        ret = ETQ_ImprimirTexto(0, 2, 2, 2, 3, 3, "SABAO EM PO FLASH 1KG", 0, True)
        CheckResult ret

        ret = ETQ_ImprimirTexto(0, 2, 2, 1, 8, 3, "ADVANCED - UNIDADE", 0, False)
        CheckResult ret

        ret = ETQ_ImprimirBarras(0, 0, 2, 2, 13, 5, "7898903097042", 10, 1)
        CheckResult ret

        ret = ETQ_ImprimirTexto(0, 3, 3, 2, 18, 35, "R$", 0, False)
        CheckResult ret

        ret = ETQ_ImprimirTexto(0, 3, 4, 4, 15, 50, "3,18", 0, False)
        CheckResult ret

        ret = ETQ_FinalizarEtiqueta(nudCopias.Value, nudAvanco.Value)
        CheckResult ret

        ret = ETQ_IniciarEtiqueta()
        CheckResult ret

        ret = ETQ_ImprimirTexto(0, 2, 2, 2, 3, 3, "AMACIANTE AMACIEX 5 LTS", 0, True)
        CheckResult ret

        ret = ETQ_ImprimirTexto(0, 2, 2, 1, 8, 3, "MACIO MATRIX FIX", 0, False)
        CheckResult ret

        ret = ETQ_ImprimirBarras(0, 0, 2, 2, 13, 5, "7898237690230", 10, 1)
        CheckResult ret

        ret = ETQ_ImprimirTexto(0, 3, 3, 2, 18, 35, "R$", 0, False)
        CheckResult ret

        ret = ETQ_ImprimirTexto(0, 3, 4, 4, 15, 50, "8,60", 0, False)
        CheckResult ret

        ret = ETQ_FinalizarEtiqueta(nudCopias.Value, nudAvanco.Value)
        CheckResult ret
    Else
        ret = ETQ_IniciarEtiqueta()
        CheckResult ret

        ret = ETQ_ImprimirCaixa(3, 3, 90, 5, 5, 0)
        CheckResult ret

        ret = ETQ_ImprimirTextoStr(0, "T", 10, 10, 3, 3, "BISCOITO MARILAN RECH 335G", 0, True)
        CheckResult ret

        ret = ETQ_ImprimirTextoStr(0, "S", 10, 10, 8, 3, "CHOC BRANCO", 0, False)
        CheckResult ret

        ret = ETQ_ImprimirBarras(0, 0, 2, 2, 13, 5, "7896003701685", 10, 1)
        CheckResult ret

        ret = ETQ_ImprimirTextoStr(0, "G", 40, 80, 18, 35, "R$", 0, False)
        CheckResult ret

        ret = ETQ_ImprimirTextoStr(0, "G", 55, 100, 15, 50, "20,59", 0, False)
        CheckResult ret

        ret = ETQ_FinalizarEtiqueta(nudCopias.Value, nudAvanco.Value)
        CheckResult ret

        ret = ETQ_IniciarEtiqueta()
        CheckResult ret

        ret = ETQ_ImprimirCaixa(3, 3, 90, 5, 5, 0)
        CheckResult ret

        ret = ETQ_ImprimirTextoStr(0, "T", 10, 10, 3, 3, "SABAO EM PO FLASH 1KG", 0, True)
        CheckResult ret

        ret = ETQ_ImprimirTextoStr(0, "S", 10, 10, 8, 3, "ADVANCED - UNIDADE", 0, False)
        CheckResult ret

        ret = ETQ_ImprimirBarras(0, 0, 2, 2, 13, 5, "7898903097042", 10, 1)
        CheckResult ret

        ret = ETQ_ImprimirTextoStr(0, "G", 40, 80, 18, 35, "R$", 0, False)
        CheckResult ret

        ret = ETQ_ImprimirTextoStr(0, "G", 55, 100, 15, 50, "3,18", 0, False)
        CheckResult ret

        ret = ETQ_FinalizarEtiqueta(nudCopias.Value, nudAvanco.Value)
        CheckResult ret

        ret = ETQ_IniciarEtiqueta()
        CheckResult ret

        ret = ETQ_ImprimirCaixa(3, 3, 90, 5, 5, 0)
        CheckResult ret

        ret = ETQ_ImprimirTextoStr(0, "T", 10, 10, 3, 3, "AMACIANTE AMACIEX 5 LTS", 0, True)
        CheckResult ret

        ret = ETQ_ImprimirTextoStr(0, "S", 10, 10, 8, 3, "MACIO MATRIX FIX", 0, False)
        CheckResult ret

        ret = ETQ_ImprimirBarras(0, 0, 2, 2, 13, 5, "7898237690230", 10, 1)
        CheckResult ret

        ret = ETQ_ImprimirTextoStr(0, "G", 40, 80, 18, 35, "R$", 0, False)
        CheckResult ret

        ret = ETQ_ImprimirTextoStr(0, "G", 55, 100, 15, 50, "8,60", 0, False)
        CheckResult ret

        ret = ETQ_FinalizarEtiqueta(nudCopias.Value, nudAvanco.Value)
        CheckResult ret
    End If
    
    ret = ETQ_Imprimir(1, nudAvanco.Value)
    CheckResult ret
    
    ret = ETQ_Desativar()
    CheckResult ret

End Sub

Private Sub cmdCarregarImagem_Click()
    On Error Resume Next
    CommonDialog1.DialogTitle = "Arquivo de Imagem"
    CommonDialog1.InitDir = App.Path
    CommonDialog1.Filter = "BMP MonoCromático|*.bmp|PCX|*.pcx|IMG|*.img"
    CommonDialog1.ShowSave
        
    If Err Then Exit Sub
    
    pictureBox.Picture = LoadPicture(CommonDialog1.FileName)
    
    Dim retorno As Long
    
    retorno = ETQ_CarregarImagem(CommonDialog1.FileName, txtNomeImagem.Text, False)
    CheckResult retorno
End Sub

Private Sub cmdEtiqueta3_Click()
    SaveConfig
    
    Dim ret As Long
    
    ret = ETQ_Ativar()
    CheckResult retorno
    
    If (cmbModelo.ListIndex = 1) Or (cmbModelo.ListIndex = 2) Then
        ret = ETQ_ImprimirTexto(0, 2, 1, 2, 2, 3, "BISCOITO REC 33G", 0, False)
        CheckResult ret

        ret = ETQ_ImprimirTexto(0, 2, 1, 1, 6, 3, "CHOC BRANCO", 0, False)
        CheckResult ret

        ret = ETQ_ImprimirBarras(0, 0, 2, 2, 8, 3, "7896003701685", 10, 0)
        CheckResult ret

        ret = ETQ_ImprimirTexto(0, 2, 1, 2, 2, 32, "BISCOITO RECH 33G", 0, False)
        CheckResult ret

        ret = ETQ_ImprimirTexto(0, 2, 1, 1, 6, 32, "CHOC BRANCO", 0, False)
        CheckResult ret

        ret = ETQ_ImprimirBarras(0, 0, 2, 2, 8, 32, "7896003701685", 10, 0)
        CheckResult ret

        ret = ETQ_ImprimirTexto(0, 2, 1, 2, 2, 61, "BISCOITO RECH 33G", 0, False)
        CheckResult ret

        ret = ETQ_ImprimirTexto(0, 2, 1, 1, 6, 61, "CHOC BRANCO", 0, False)
        CheckResult ret

        ret = ETQ_ImprimirBarras(0, 0, 2, 2, 8, 61, "7896003701685", 10, 0)
        CheckResult ret
    Else
        ret = ETQ_ImprimirTextoStr(0, "0", 20, 30, 2, 3, "BISCOITO REC 33G", 0, False)
        CheckResult ret

        ret = ETQ_ImprimirTextoStr(0, "0", 20, 20, 6, 3, "CHOC BRANCO", 0, False)
        CheckResult ret

        ret = ETQ_ImprimirBarras(0, 0, 2, 2, 8, 3, "7896003701685", 10, 0)
        CheckResult ret

        ret = ETQ_ImprimirTextoStr(0, "0", 20, 30, 2, 32, "BISCOITO RECH 33G", 0, False)
        CheckResult ret

        ret = ETQ_ImprimirTextoStr(0, "0", 20, 20, 6, 32, "CHOC BRANCO", 0, False)
        CheckResult ret

        ret = ETQ_ImprimirBarras(0, 0, 2, 2, 8, 32, "7896003701685", 10, 0)
        CheckResult ret

        ret = ETQ_ImprimirTextoStr(0, "0", 20, 30, 2, 61, "BISCOITO RECH 33G", 0, False)
        CheckResult ret

        ret = ETQ_ImprimirTextoStr(0, "0", 20, 20, 6, 61, "CHOC BRANCO", 0, False)
        CheckResult ret

        ret = ETQ_ImprimirBarras(0, 0, 2, 2, 8, 61, "7896003701685", 10, 0)
        CheckResult ret
    End If
    
    ret = ETQ_Imprimir(nudCopias.Value, nudAvanco.Value)
    CheckResult ret
    
    ret = ETQ_Desativar()
    CheckResult ret
End Sub

Private Sub cmdEtiquetaSimples_Click()
    SaveConfig
    
    Dim ret As Long
    
    ret = ETQ_Ativar()
    CheckResult retorno
    
    If (cmbModelo.ListIndex = 1) Or (cmbModelo.ListIndex = 2) Then
        ret = ETQ_ImprimirTexto(0, 2, 2, 2, 3, 3, "BISCOITO MARILAN RECH 335G", 0, True)
        CheckResult ret
        
        ret = ETQ_ImprimirTexto(0, 2, 2, 1, 8, 3, "CHOC BRANCO", 0, False)
        CheckResult ret
        
        ret = ETQ_ImprimirBarras(0, 0, 2, 2, 13, 5, "7896003701685", 10, 1)
        CheckResult ret
        
        ret = ETQ_ImprimirCaixa(13, 32, 56, 17, 1, 1)
        CheckResult ret
        
        ret = ETQ_ImprimirTexto(0, 3, 3, 2, 18, 35, "R$", 0, False)
        CheckResult ret
        
        ret = ETQ_ImprimirTexto(0, 3, 4, 4, 15, 50, "20,59", 0, False)
        CheckResult ret
    Else
        ret = ETQ_ImprimirCaixa(3, 3, 90, 5, 5, 0)
        CheckResult ret
                        
        ret = ETQ_ImprimirTextoStr(0, "T", 10, 10, 3, 3, "BISCOITO MARILAN RECH 335G", 0, True)
        CheckResult ret
        
        ret = ETQ_ImprimirTextoStr(2, "S", 10, 10, 8, 3, "CHOC BRANCO", 0, False)
        CheckResult ret
        
        ret = ETQ_ImprimirBarras(0, 0, 2, 2, 13, 5, "7896003701685", 10, 1)
        CheckResult ret
        
        ret = ETQ_ImprimirCaixa(13, 32, 56, 17, 1, 1)
        CheckResult ret
        
        ret = ETQ_ImprimirTextoStr(0, "G", 40, 80, 18, 35, "R$", 0, False)
        CheckResult ret
        
        ret = ETQ_ImprimirTextoStr(0, "G", 55, 100, 15, 50, "20,59", 0, False)
        CheckResult ret
    End If
    
    ret = ETQ_Imprimir(nudCopias.Value, nudAvanco.Value)
    CheckResult ret
    
    ret = ETQ_Desativar()
    CheckResult ret
    
End Sub

Private Sub Form_Load()
    cmbModelo.AddItem "etqNenhum", 0
    cmbModelo.AddItem "etqPpla", 1
    cmbModelo.AddItem "etqPplb", 2
    cmbModelo.AddItem "etqZPLII", 3
    cmbModelo.AddItem "etqEpl2", 4
    
    cmbModelo.ListIndex = 0
    
    cmbPorta.AddItem "COM1"
    cmbPorta.AddItem "COM2"
    cmbPorta.AddItem "LPT1"
    cmbPorta.AddItem "LPT2"
    cmbPorta.AddItem "\\localhost\Epson"
    cmbPorta.AddItem "C:\temp\etq.txt"
    
    cmbPorta.ListIndex = cmbPorta.ListCount - 1
    
    cmbPorta.AddItem "TCP:192.168.0.31:9100"
    Dim p As Printer
    For Each p In Printers
        cmbPorta.AddItem "RAW:" + p.DeviceName
    Next
    
    cmbDPI.AddItem "dpi203", 0
    cmbDPI.AddItem "dpi300", 1
    cmbDPI.AddItem "dpi600", 2
    
    cmbDPI.ListIndex = 0
    
    cmbBackFeed.AddItem "bfNone", 0
    cmbBackFeed.AddItem "bfOn", 1
    cmbBackFeed.AddItem "bfOff", 2
    
    cmbBackFeed.ListIndex = 0
    
    Dim LogPath As String
    
    LogPath = App.Path & "\Docs\"
    
    If Not DirExists(LogPath) Then
        MkDir LogPath
    End If
    
    Dim retorno As Long
    
    retorno = ETQ_Inicializar("", "")
    CheckResult retorno
    
    retorno = ETQ_ConfigGravarValor("Principal", "LogNivel", "4")
    CheckResult retorno
    
    retorno = ETQ_ConfigGravarValor("Principal", "LogPath", LogPath)
    CheckResult retorno
    
    retorno = ETQ_ConfigGravar("")
    CheckResult retorno
    
    LoadConfig
End Sub

Private Sub Form_Terminate()
    Dim retorno As Long
    retorno = ETQ_Finalizar
    CheckResult retorno
End Sub

Private Sub LoadConfig()
    Dim retorno As Long
    Dim buffer As String
    Dim bufferLen As Long
    
    retorno = ETQ_ConfigLer("")
    CheckResult retorno
    
    bufferLen = 256
    buffer = String$(bufferLen, " ")
                
    retorno = ETQ_ConfigLerValor("ETQ", "Porta", buffer, bufferLen)
    CheckResult retorno
    
    cmbPorta.Text = FromUTF8(buffer, bufferLen)
    
    retorno = ETQ_ConfigLer("")
    CheckResult retorno
    
    bufferLen = 256
    buffer = String$(bufferLen, " ")
                
    retorno = ETQ_ConfigLerValor("ETQ", "Modelo", buffer, bufferLen)
    CheckResult retorno
    
    cmbModelo.ListIndex = CInt(FromUTF8(buffer, bufferLen))
    
    bufferLen = 256
    buffer = String$(bufferLen, " ")
                
    retorno = ETQ_ConfigLerValor("ETQ", "Temperatura", buffer, bufferLen)
    CheckResult retorno
    
    nudTemperatura.Value = CInt(FromUTF8(buffer, bufferLen))
    
    bufferLen = 256
    buffer = String$(bufferLen, " ")
                
    retorno = ETQ_ConfigLerValor("ETQ", "DPI", buffer, bufferLen)
    CheckResult retorno
    
    cmbDPI.ListIndex = CInt(FromUTF8(buffer, bufferLen))
    
    bufferLen = 256
    buffer = String$(bufferLen, " ")
                
    retorno = ETQ_ConfigLerValor("ETQ", "Velocidade", buffer, bufferLen)
    CheckResult retorno
    
    nudVelocidade.Value = CInt(FromUTF8(buffer, bufferLen))
    
    bufferLen = 256
    buffer = String$(bufferLen, " ")
                
    retorno = ETQ_ConfigLerValor("ETQ", "BackFeed", buffer, bufferLen)
    CheckResult retorno
    
    cmbBackFeed.ListIndex = CInt(FromUTF8(buffer, bufferLen))
    
    bufferLen = 256
    buffer = String$(bufferLen, " ")
                
    retorno = ETQ_ConfigLerValor("ETQ", "Avanco", buffer, bufferLen)
    CheckResult retorno
    
    nudAvanco.Value = CInt(FromUTF8(buffer, bufferLen))
    
    bufferLen = 256
    buffer = String$(bufferLen, " ")
                
    retorno = ETQ_ConfigLerValor("ETQ", "LimparMemoria", buffer, bufferLen)
    CheckResult retorno
    
    chkLimparMemoria.Value = CInt(FromUTF8(buffer, bufferLen))
End Sub

Private Sub SaveConfig()
    Dim retorno As Long
    
    retorno = ETQ_ConfigGravarValor("ETQ", "Porta", cmbPorta.Text)
    CheckResult retorno
    
    retorno = ETQ_ConfigGravarValor("ETQ", "Modelo", CStr(cmbModelo.ListIndex))
    CheckResult retorno
    
    retorno = ETQ_ConfigGravarValor("ETQ", "Temperatura", CStr(nudTemperatura.Value))
    CheckResult retorno
    
    retorno = ETQ_ConfigGravarValor("ETQ", "DPI", CStr(cmbDPI.ListIndex))
    CheckResult retorno
    
    retorno = ETQ_ConfigGravarValor("ETQ", "Velocidade", CStr(nudVelocidade.Value))
    CheckResult retorno
    
    retorno = ETQ_ConfigGravarValor("ETQ", "BackFeed", CStr(cmbBackFeed.ListIndex))
    CheckResult retorno
    
    retorno = ETQ_ConfigGravarValor("ETQ", "Avanco", CStr(nudAvanco.Value))
    CheckResult retorno
    
    retorno = ETQ_ConfigGravarValor("ETQ", "LimparMemoria", CStr(chkLimparMemoria.Value))
    CheckResult retorno
    
    retorno = ETQ_ConfigGravar("")
    CheckResult retorno
End Sub

Private Sub txtAvanco_Change()
    Dim textval As String
    textval = txtAvanco.Text
    
    If IsNumeric(textval) Then
        nudAvanco.Value = CInt(txtAvanco.Text)
    Else
        txtAvanco.Text = CStr(nudAvanco.Value)
    End If
End Sub

Private Sub txtCopias_Change()
    Dim textval As String
    textval = txtAvanco.Text
    
    If IsNumeric(textval) Then
        nudCopias.Value = CInt(txtCopias.Text)
    Else
        txtCopias.Text = CStr(nudCopias.Value)
    End If
End Sub

Private Sub txtTemperatura_Change()
    Dim textval As String
    textval = txtTemperatura.Text
    
    If IsNumeric(textval) Then
        nudTemperatura.Value = CInt(txtTemperatura.Text)
    Else
        txtTemperatura.Text = CStr(nudTemperatura.Value)
    End If
End Sub

Private Sub txtVelocidade_Change()
    Dim textval As String
    textval = txtVelocidade.Text
    
    If IsNumeric(textval) Then
        nudVelocidade.Value = CInt(txtVelocidade.Text)
    Else
        txtVelocidade.Text = CStr(nudVelocidade.Value)
    End If
End Sub
