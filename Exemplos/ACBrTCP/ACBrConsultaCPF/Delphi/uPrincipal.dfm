object frmPrincipal: TfrmPrincipal
  Left = 428
  Top = 226
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  Caption = 'Consulta CPF (Receita Fazenda)'
  ClientHeight = 421
  ClientWidth = 645
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel
    Left = 0
    Top = 232
    Width = 645
    Height = 189
    Align = alClient
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 1
    object Label3: TLabel
      Left = 10
      Top = 15
      Width = 139
      Height = 16
      Caption = 'Nome da Pessoa F'#237'sica'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object Label12: TLabel
      Left = 465
      Top = 15
      Width = 110
      Height = 16
      Caption = 'Situa'#231#227'o Cadastral'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object RzLabel1: TLabel
      Left = 10
      Top = 63
      Width = 97
      Height = 16
      Caption = 'Digito Verificador'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object RzLabel2: TLabel
      Left = 10
      Top = 115
      Width = 207
      Height = 16
      Caption = 'C'#243'digo de controle do comprovante:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object RzLabel3: TLabel
      Left = 113
      Top = 63
      Width = 143
      Height = 16
      Caption = 'Comprovante emitido '#224's:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object Label4: TLabel
      Left = 465
      Top = 63
      Width = 94
      Height = 16
      Caption = 'Data da Incri'#231#227'o'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object EditRazaoSocial: TEdit
      Left = 10
      Top = 33
      Width = 449
      Height = 24
      TabStop = False
      Color = clInfoBk
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      TabOrder = 0
    end
    object EditSituacao: TEdit
      Left = 465
      Top = 33
      Width = 160
      Height = 24
      TabStop = False
      Color = clInfoBk
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      TabOrder = 1
    end
    object EdtDigitoVerificador: TEdit
      Left = 10
      Top = 85
      Width = 97
      Height = 24
      TabStop = False
      Color = clInfoBk
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      TabOrder = 2
    end
    object EdtCodCtrlControle: TEdit
      Left = 9
      Top = 137
      Width = 615
      Height = 24
      TabStop = False
      Color = clInfoBk
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      TabOrder = 3
    end
    object EdtEmissao: TEdit
      Left = 113
      Top = 85
      Width = 346
      Height = 24
      TabStop = False
      Color = clInfoBk
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      TabOrder = 4
    end
    object EdtIncricao: TEdit
      Left = 465
      Top = 85
      Width = 160
      Height = 24
      TabStop = False
      Color = clInfoBk
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      TabOrder = 5
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 645
    Height = 232
    Align = alTop
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    object Label1: TLabel
      Left = 361
      Top = 12
      Width = 78
      Height = 16
      Caption = 'Digite o CPF:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object Label14: TLabel
      Left = 361
      Top = 142
      Width = 96
      Height = 16
      Caption = 'Digite o Captcha'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object Label2: TLabel
      Left = 361
      Top = 78
      Width = 170
      Height = 16
      Caption = 'Digite a Data de Nascimento:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object EditCaptcha: TEdit
      Left = 361
      Top = 161
      Width = 137
      Height = 41
      CharCase = ecUpperCase
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -27
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
    end
    object EditCNPJ: TEdit
      Left = 361
      Top = 31
      Width = 264
      Height = 41
      TabStop = False
      Color = clInfoBk
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -27
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
    end
    object Panel3: TPanel
      Left = 9
      Top = 11
      Width = 346
      Height = 127
      BevelOuter = bvLowered
      Color = clWhite
      ParentBackground = False
      TabOrder = 0
      object Image1: TImage
        Left = 1
        Top = 1
        Width = 344
        Height = 106
        Align = alClient
        Center = True
        Proportional = True
        Stretch = True
      end
      object LabAtualizarCaptcha: TLabel
        Left = 1
        Top = 107
        Width = 344
        Height = 19
        Cursor = crHandPoint
        Align = alBottom
        Alignment = taCenter
        AutoSize = False
        Caption = 'Atualizar Captcha'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = [fsBold, fsUnderline]
        ParentFont = False
        OnClick = LabAtualizarCaptchaClick
      end
    end
    object btnConsultar: TButton
      Left = 504
      Top = 161
      Width = 120
      Height = 40
      Caption = 'Consultar'
      TabOrder = 3
      OnClick = btnConsultarClick
    end
    object EditDtNasc: TEdit
      Left = 361
      Top = 95
      Width = 264
      Height = 41
      TabStop = False
      Color = clInfoBk
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -27
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
    end
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 100
    OnTimer = Timer1Timer
    Left = 384
    Top = 208
  end
  object ACBrConsultaCPF1: TACBrConsultaCPF
    ProxyPort = '8080'
    Left = 302
    Top = 170
  end
end
