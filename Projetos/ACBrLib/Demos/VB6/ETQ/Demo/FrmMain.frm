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
Dim etq As ACBrETQ


Private Sub cmdBlocoDe_Click()
    SaveConfig
    
    etq.Ativar
    
    If (cmbModelo.ListIndex = 1) Or (cmbModelo.ListIndex = 2) Then
        etq.IniciarEtiqueta
        etq.ImprimirTexto ETQOrientacao.orNormal, 2, 2, 2, 3, 3, "BISCOITO MARILAN RECH 335G", 0, True
        etq.ImprimirTexto ETQOrientacao.orNormal, 2, 2, 1, 8, 3, "CHOC BRANCO"
        etq.ImprimirBarras ETQOrientacao.orNormal, TipoCodBarra.barEAN13, 2, 2, 13, 5, "7896003701685", 10, ETQBarraExibeCodigo.becSIM
        etq.ImprimirTexto ETQOrientacao.orNormal, 3, 3, 2, 18, 35, "R$"
        etq.ImprimirTexto ETQOrientacao.orNormal, 3, 4, 4, 15, 50, "20,59"
        etq.FinalizarEtiqueta nudCopias.value, nudAvanco.value

        etq.IniciarEtiqueta
        etq.ImprimirTexto ETQOrientacao.orNormal, 2, 2, 2, 3, 3, "SABAO EM PO FLASH 1KG", 0, True
        etq.ImprimirTexto ETQOrientacao.orNormal, 2, 2, 1, 8, 3, "ADVANCED - UNIDADE"
        etq.ImprimirBarras ETQOrientacao.orNormal, TipoCodBarra.barEAN13, 2, 2, 13, 5, "7898903097042", 10, ETQBarraExibeCodigo.becSIM
        etq.ImprimirTexto ETQOrientacao.orNormal, 3, 3, 2, 18, 35, "R$"
        etq.ImprimirTexto ETQOrientacao.orNormal, 3, 4, 4, 15, 50, "3,18"
        etq.FinalizarEtiqueta nudCopias.value, nudAvanco.value
        
        etq.IniciarEtiqueta
        etq.ImprimirTexto ETQOrientacao.orNormal, 2, 2, 2, 3, 3, "AMACIANTE AMACIEX 5 LTS", 0, True
        etq.ImprimirTexto ETQOrientacao.orNormal, 2, 2, 1, 8, 3, "MACIO MATRIX FIX"
        etq.ImprimirBarras ETQOrientacao.orNormal, TipoCodBarra.barEAN13, 2, 2, 13, 5, "7898237690230", 10, ETQBarraExibeCodigo.becSIM
        etq.ImprimirTexto ETQOrientacao.orNormal, 3, 3, 2, 18, 35, "R$"
        etq.ImprimirTexto ETQOrientacao.orNormal, 3, 4, 4, 15, 50, "8,60"
        etq.FinalizarEtiqueta nudCopias.value, nudAvanco.value
    Else
        etq.IniciarEtiqueta
        etq.ImprimirCaixa 3, 3, 90, 5, 5, 0
        etq.ImprimirTextoStr ETQOrientacao.orNormal, "T", 10, 10, 3, 3, "BISCOITO MARILAN RECH 335G", 0, True
        etq.ImprimirTextoStr ETQOrientacao.orNormal, "S", 10, 10, 8, 3, "CHOC BRANCO"
        etq.ImprimirBarras ETQOrientacao.orNormal, TipoCodBarra.barEAN13, 2, 2, 13, 5, "7896003701685", 10, ETQBarraExibeCodigo.becSIM
        etq.ImprimirTextoStr ETQOrientacao.orNormal, "G", 40, 80, 18, 35, "R$"
        etq.ImprimirTextoStr ETQOrientacao.orNormal, "G", 55, 100, 15, 50, "20,59"
        etq.FinalizarEtiqueta nudCopias.value, nudAvanco.value
        etq.IniciarEtiqueta

        etq.ImprimirCaixa 3, 3, 90, 5, 5, 0
        etq.ImprimirTextoStr ETQOrientacao.orNormal, "T", 10, 10, 3, 3, "SABAO EM PO FLASH 1KG", 0, True
        etq.ImprimirTextoStr ETQOrientacao.orNormal, "S", 10, 10, 8, 3, "ADVANCED - UNIDADE"
        etq.ImprimirBarras ETQOrientacao.orNormal, TipoCodBarra.barEAN13, 2, 2, 13, 5, "7898903097042", 10, ETQBarraExibeCodigo.becSIM
        etq.ImprimirTextoStr ETQOrientacao.orNormal, "G", 40, 80, 18, 35, "R$"
        etq.ImprimirTextoStr ETQOrientacao.orNormal, "G", 55, 100, 15, 50, "3,18"
        etq.FinalizarEtiqueta nudCopias.value, nudAvanco.value
        etq.IniciarEtiqueta

        etq.ImprimirCaixa 3, 3, 90, 5, 5, 0
        etq.ImprimirTextoStr ETQOrientacao.orNormal, "T", 10, 10, 3, 3, "AMACIANTE AMACIEX 5 LTS", 0, True
        etq.ImprimirTextoStr ETQOrientacao.orNormal, "S", 10, 10, 8, 3, "MACIO MATRIX FIX"
        etq.ImprimirBarras ETQOrientacao.orNormal, TipoCodBarra.barEAN13, 2, 2, 13, 5, "7898237690230", 10, ETQBarraExibeCodigo.becSIM
        etq.ImprimirTextoStr ETQOrientacao.orNormal, "G", 40, 80, 18, 35, "R$"
        etq.ImprimirTextoStr ETQOrientacao.orNormal, "G", 55, 100, 15, 50, "8,60"
        etq.FinalizarEtiqueta nudCopias.value, nudAvanco.value
    End If
    
    etq.Imprimir 1, nudAvanco.value
    etq.Desativar
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
    
    etq.Ativar
    
    If (cmbModelo.ListIndex = 1) Or (cmbModelo.ListIndex = 2) Then
        etq.ImprimirTexto ETQOrientacao.orNormal, 2, 1, 2, 2, 3, "BISCOITO REC 33G"
        etq.ImprimirTexto ETQOrientacao.orNormal, 2, 1, 1, 6, 3, "CHOC BRANCO"
        etq.ImprimirBarras ETQOrientacao.orNormal, TipoCodBarra.barEAN13, 2, 2, 8, 3, "7896003701685", 10
        etq.ImprimirTexto ETQOrientacao.orNormal, 2, 1, 2, 2, 32, "BISCOITO RECH 33G"
        etq.ImprimirTexto ETQOrientacao.orNormal, 2, 1, 1, 6, 32, "CHOC BRANCO"
        etq.ImprimirBarras ETQOrientacao.orNormal, TipoCodBarra.barEAN13, 2, 2, 8, 32, "7896003701685", 10
        etq.ImprimirTexto ETQOrientacao.orNormal, 2, 1, 2, 2, 61, "BISCOITO RECH 33G"
        etq.ImprimirTexto ETQOrientacao.orNormal, 2, 1, 1, 6, 61, "CHOC BRANCO"
        etq.ImprimirBarras ETQOrientacao.orNormal, TipoCodBarra.barEAN13, 2, 2, 8, 61, "7896003701685", 10
    Else
        etq.ImprimirTextoStr ETQOrientacao.orNormal, "0", 20, 30, 2, 3, "BISCOITO REC 33G"
        etq.ImprimirTextoStr ETQOrientacao.orNormal, "0", 20, 20, 6, 3, "CHOC BRANCO"
        etq.ImprimirBarras ETQOrientacao.orNormal, TipoCodBarra.barEAN13, 2, 2, 8, 3, "7896003701685", 10
        etq.ImprimirTextoStr ETQOrientacao.orNormal, "0", 20, 30, 2, 32, "BISCOITO RECH 33G"
        etq.ImprimirTextoStr ETQOrientacao.orNormal, "0", 20, 20, 6, 32, "CHOC BRANCO"
        etq.ImprimirBarras ETQOrientacao.orNormal, TipoCodBarra.barEAN13, 2, 2, 8, 32, "7896003701685", 10
        etq.ImprimirTextoStr ETQOrientacao.orNormal, "0", 20, 30, 2, 61, "BISCOITO RECH 33G"
        etq.ImprimirTextoStr ETQOrientacao.orNormal, "0", 20, 20, 6, 61, "CHOC BRANCO"
        etq.ImprimirBarras ETQOrientacao.orNormal, TipoCodBarra.barEAN13, 2, 2, 8, 61, "7896003701685", 10
    End If
    
    etq.Imprimir nudCopias.value, nudAvanco.value
    etq.Desativar
End Sub

Private Sub cmdEtiquetaSimples_Click()
    SaveConfig
    
    etq.Ativar
    
    If (cmbModelo.ListIndex = 1) Or (cmbModelo.ListIndex = 2) Then
        etq.ImprimirTexto ETQOrientacao.orNormal, 2, 2, 2, 3, 3, "BISCOITO MARILAN RECH 335G", 0, True
        etq.ImprimirTexto ETQOrientacao.orNormal, 2, 2, 1, 8, 3, "CHOC BRANCO"
        etq.ImprimirBarras ETQOrientacao.orNormal, TipoCodBarra.barEAN13, 2, 2, 13, 5, "7896003701685", 10, ETQBarraExibeCodigo.becSIM
        etq.ImprimirCaixa 13, 32, 56, 17, 1, 1
        etq.ImprimirTexto ETQOrientacao.orNormal, 3, 3, 2, 18, 35, "R$"
        etq.ImprimirTexto ETQOrientacao.orNormal, 3, 4, 4, 15, 50, "20,59"
    Else
        etq.ImprimirCaixa 3, 3, 90, 5, 5, 0
        etq.ImprimirTextoStr ETQOrientacao.orNormal, "T", 10, 10, 3, 3, "BISCOITO MARILAN RECH 335G", 0, True
        etq.ImprimirTextoStr ETQOrientacao.or180, "S", 10, 10, 8, 3, "CHOC BRANCO"
        etq.ImprimirBarras ETQOrientacao.orNormal, TipoCodBarra.barEAN13, 2, 2, 13, 5, "7896003701685", 10, ETQBarraExibeCodigo.becSIM
        etq.ImprimirCaixa 13, 32, 56, 17, 1, 1
        etq.ImprimirTextoStr ETQOrientacao.orNormal, "G", 40, 80, 18, 35, "R$"
        etq.ImprimirTextoStr ETQOrientacao.orNormal, "G", 55, 100, 15, 50, "20,59"
    End If
    
    etq.Imprimir nudCopias.value, nudAvanco.value
    etq.Desativar
End Sub

Private Sub Form_Load()

    cmbModelo.AddItem "etqNenhum", ETQModelo.etqNenhum
    cmbModelo.AddItem "etqPpla", ETQModelo.etqPpla
    cmbModelo.AddItem "etqPplb", ETQModelo.etqPplb
    cmbModelo.AddItem "etqZPLII", ETQModelo.etqZPLII
    cmbModelo.AddItem "etqEpl2", ETQModelo.etqEpl2
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
    
    cmbDPI.AddItem "dpi203", ETQDPI.dpi203
    cmbDPI.AddItem "dpi300", ETQDPI.dpi300
    cmbDPI.AddItem "dpi600", ETQDPI.dpi600
    
    cmbDPI.ListIndex = 0
    
    cmbBackFeed.AddItem "bfNone", ETQBackFeed.bfNone
    cmbBackFeed.AddItem "bfOn", ETQBackFeed.bfOn
    cmbBackFeed.AddItem "bfOff", ETQBackFeed.bfOff
    
    cmbBackFeed.ListIndex = 0
    
    Dim LogPath As String
    
    LogPath = App.Path & "\Docs\"
    
    If Not DirExists(LogPath) Then
        MkDir LogPath
    End If
    
    Set etq = CreateETQ
    
    etq.ConfigGravarValor SESSAO_PRINCIPAL, "LogNivel", "4"
    etq.ConfigGravarValor SESSAO_PRINCIPAL, "LogPath", LogPath
    etq.ConfigGravarValor SESSAO_ETQ, "Unidade", ETQUnidade.etqMilimetros
    etq.ConfigGravar
    LoadConfig
End Sub

Private Sub Form_Terminate()
    etq.FinalizarLib
End Sub

Private Sub LoadConfig()
    
    Dim value As String
    
    etq.ConfigLer
    cmbPorta.Text = etq.ConfigLerValor(SESSAO_ETQ, "Porta")
    cmbModelo.ListIndex = CInt(etq.ConfigLerValor(SESSAO_ETQ, "Modelo"))
    nudTemperatura.value = CInt(etq.ConfigLerValor(SESSAO_ETQ, "Temperatura"))
    cmbDPI.ListIndex = CInt(etq.ConfigLerValor(SESSAO_ETQ, "DPI"))
    nudVelocidade.value = CInt(etq.ConfigLerValor(SESSAO_ETQ, "Velocidade"))
    cmbBackFeed.ListIndex = CInt(etq.ConfigLerValor(SESSAO_ETQ, "BackFeed"))
    nudAvanco.value = CInt(etq.ConfigLerValor(SESSAO_ETQ, "Avanco"))
    chkLimparMemoria.value = CInt(etq.ConfigLerValor(SESSAO_ETQ, "LimparMemoria"))
End Sub

Private Sub SaveConfig()
    etq.ConfigGravarValor SESSAO_ETQ, "Porta", cmbPorta.Text
    etq.ConfigGravarValor SESSAO_ETQ, "Modelo", CStr(cmbModelo.ListIndex)
    etq.ConfigGravarValor SESSAO_ETQ, "Temperatura", CStr(nudTemperatura.value)
    etq.ConfigGravarValor SESSAO_ETQ, "DPI", CStr(cmbDPI.ListIndex)
    etq.ConfigGravarValor SESSAO_ETQ, "Velocidade", CStr(nudVelocidade.value)
    etq.ConfigGravarValor SESSAO_ETQ, "BackFeed", CStr(cmbBackFeed.ListIndex)
    etq.ConfigGravarValor SESSAO_ETQ, "Avanco", CStr(nudAvanco.value)
    etq.ConfigGravarValor SESSAO_ETQ, "LimparMemoria", CStr(chkLimparMemoria.value)
    etq.ConfigGravar
End Sub

Private Sub txtAvanco_Change()
    Dim textval As String
    textval = txtAvanco.Text
    
    If IsNumeric(textval) Then
        nudAvanco.value = CInt(txtAvanco.Text)
    Else
        txtAvanco.Text = CStr(nudAvanco.value)
    End If
End Sub

Private Sub txtCopias_Change()
    Dim textval As String
    textval = txtAvanco.Text
    
    If IsNumeric(textval) Then
        nudCopias.value = CInt(txtCopias.Text)
    Else
        txtCopias.Text = CStr(nudCopias.value)
    End If
End Sub

Private Sub txtTemperatura_Change()
    Dim textval As String
    textval = txtTemperatura.Text
    
    If IsNumeric(textval) Then
        nudTemperatura.value = CInt(txtTemperatura.Text)
    Else
        txtTemperatura.Text = CStr(nudTemperatura.value)
    End If
End Sub

Private Sub txtVelocidade_Change()
    Dim textval As String
    textval = txtVelocidade.Text
    
    If IsNumeric(textval) Then
        nudVelocidade.value = CInt(txtVelocidade.Text)
    Else
        txtVelocidade.Text = CStr(nudVelocidade.value)
    End If
End Sub
