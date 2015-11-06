VERSION 5.00
Object = "{248DD890-BB45-11CF-9ABC-0080C7E7B78D}#1.0#0"; "MSWINSCK.ocx"
Begin VB.Form Form1 
   Caption         =   "ACBR_TCP - FLYSYS TECNOLOGIA (17) 3234-7668"
   ClientHeight    =   5235
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   8640
   LinkTopic       =   "Form1"
   ScaleHeight     =   5235
   ScaleWidth      =   8640
   StartUpPosition =   3  'Windows Default
   Begin VB.CommandButton Command3 
      Caption         =   "Desconectar"
      Height          =   375
      Left            =   2453
      TabIndex        =   12
      Top             =   4680
      Width           =   1575
   End
   Begin VB.CommandButton Command2 
      Caption         =   "Enviar Comando"
      Height          =   375
      Left            =   5753
      TabIndex        =   11
      Top             =   4680
      Width           =   2055
   End
   Begin VB.Timer TimerStatus 
      Interval        =   100
      Left            =   4440
      Top             =   4680
   End
   Begin MSWinsockLib.Winsock Winsock1 
      Left            =   4920
      Top             =   4680
      _ExtentX        =   741
      _ExtentY        =   741
      _Version        =   393216
   End
   Begin VB.CommandButton Command1 
      Caption         =   "Conectar"
      Height          =   375
      Left            =   833
      TabIndex        =   8
      Top             =   4680
      Width           =   1575
   End
   Begin VB.TextBox TextRetorno 
      Height          =   2325
      Left            =   360
      MultiLine       =   -1  'True
      ScrollBars      =   3  'Both
      TabIndex        =   6
      Top             =   2040
      Width           =   7695
   End
   Begin VB.TextBox TextComando 
      Height          =   285
      Left            =   360
      TabIndex        =   4
      Text            =   "ACBR.DataHora"
      Top             =   1320
      Width           =   7695
   End
   Begin VB.TextBox TextPorta 
      Height          =   285
      Left            =   4440
      TabIndex        =   1
      Text            =   "3434"
      Top             =   600
      Width           =   975
   End
   Begin VB.TextBox TextIP 
      Height          =   285
      Left            =   360
      TabIndex        =   0
      Text            =   "192.168.1.101"
      Top             =   600
      Width           =   3855
   End
   Begin VB.Label lblStatus 
      Caption         =   "Status"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Left            =   5880
      TabIndex        =   10
      Top             =   600
      Width           =   2535
   End
   Begin VB.Label Label 
      Caption         =   "Status"
      Height          =   255
      Index           =   1
      Left            =   5880
      TabIndex        =   9
      Top             =   360
      Width           =   495
   End
   Begin VB.Label Label3 
      Caption         =   "Comando"
      Height          =   255
      Left            =   360
      TabIndex        =   7
      Top             =   1800
      Width           =   975
   End
   Begin VB.Label Label2 
      Caption         =   "Comando"
      Height          =   255
      Left            =   360
      TabIndex        =   5
      Top             =   1080
      Width           =   975
   End
   Begin VB.Label Label1 
      Caption         =   "Porta"
      Height          =   255
      Left            =   4440
      TabIndex        =   3
      Top             =   360
      Width           =   975
   End
   Begin VB.Label Label 
      Caption         =   "IP"
      Height          =   255
      Index           =   0
      Left            =   360
      TabIndex        =   2
      Top             =   360
      Width           =   975
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub Command1_Click()
    Winsock1.Connect TextIP.Text, TextPorta.Text
End Sub

Private Sub Command2_Click()
    Winsock1.SendData TextComando.Text & Chr(13) & Chr(10) & "." & Chr(13) & Chr(10)
End Sub

Private Sub Command3_Click()
    Winsock1.Close
End Sub

Private Sub TimerStatus_Timer()
    Select Case Winsock1.State
        Case 0
            lblStatus.Caption = "Default. Closed"
        Case 1
            lblStatus.Caption = "Open"
        Case 2
            lblStatus.Caption = "Listening"
        Case 3
            lblStatus.Caption = "Connection pending"
        Case 4
            lblStatus.Caption = "Resolving host"
        Case 5
            lblStatus.Caption = "Host resolved"
        Case 6
            lblStatus.Caption = "Connecting"
        Case 7
            lblStatus.Caption = "Connected"
        Case 8
            lblStatus.Caption = "Peer is closing the connection"
        Case 9
            lblStatus.Caption = "Error"
        Case Else
            lblStatus.Caption = "---"
    End Select
End Sub

Private Sub Winsock1_DataArrival(ByVal bytesTotal As Long)
    Dim texto
    Winsock1.GetData texto, vbString
    TextRetorno.Text = TextRetorno.Text & texto & vbCrLf
End Sub
