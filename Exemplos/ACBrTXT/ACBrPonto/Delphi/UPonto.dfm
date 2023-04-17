object Form1: TForm1
  Left = 419
  Top = 223
  BorderStyle = bsDialog
  Caption = 'ACBrPonto - Demonstra'#231#227'o'
  ClientHeight = 447
  ClientWidth = 664
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 664
    Height = 447
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Arquivo AFD'
      object Memo1: TMemo
        Left = 3
        Top = 3
        Width = 650
        Height = 382
        TabOrder = 0
      end
      object Button1: TButton
        Left = 204
        Top = 391
        Width = 249
        Height = 25
        Caption = 'Processar Arquivo AFD'
        TabOrder = 1
        OnClick = Button1Click
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Arquivo AFDT'
      ImageIndex = 1
      object LabeledEdit1: TLabeledEdit
        Left = 11
        Top = 24
        Width = 110
        Height = 21
        EditLabel.Width = 29
        EditLabel.Height = 13
        EditLabel.Caption = 'CNPJ:'
        TabOrder = 0
        Text = '10793118000178'
      end
      object LabeledEdit2: TLabeledEdit
        Left = 127
        Top = 24
        Width = 110
        Height = 21
        EditLabel.Width = 21
        EditLabel.Height = 13
        EditLabel.Caption = 'CEI:'
        TabOrder = 1
        Text = '123456789102'
      end
      object LabeledEdit3: TLabeledEdit
        Left = 243
        Top = 24
        Width = 398
        Height = 21
        EditLabel.Width = 64
        EditLabel.Height = 13
        EditLabel.Caption = 'Raz'#227'o Social:'
        TabOrder = 2
        Text = 'T2Ti Tecnologia da Informa'#231#227'o'
      end
      object LabeledEdit4: TLabeledEdit
        Left = 11
        Top = 72
        Width = 110
        Height = 21
        EditLabel.Width = 57
        EditLabel.Height = 13
        EditLabel.Caption = 'Data Inicial:'
        TabOrder = 3
        Text = '10012012'
      end
      object LabeledEdit5: TLabeledEdit
        Left = 127
        Top = 72
        Width = 110
        Height = 21
        EditLabel.Width = 52
        EditLabel.Height = 13
        EditLabel.Caption = 'Data Final:'
        TabOrder = 4
        Text = '31122012'
      end
      object Memo2: TMemo
        Left = 3
        Top = 99
        Width = 650
        Height = 286
        TabOrder = 5
      end
      object Button2: TButton
        Left = 204
        Top = 391
        Width = 249
        Height = 25
        Caption = 'Gerar Arquivo AFDT'
        TabOrder = 6
        OnClick = Button2Click
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Arquivo ACJEF'
      ImageIndex = 2
      object LabeledEdit6: TLabeledEdit
        Left = 11
        Top = 24
        Width = 110
        Height = 21
        EditLabel.Width = 29
        EditLabel.Height = 13
        EditLabel.Caption = 'CNPJ:'
        TabOrder = 0
        Text = '10793118000178'
      end
      object LabeledEdit7: TLabeledEdit
        Left = 127
        Top = 24
        Width = 110
        Height = 21
        EditLabel.Width = 21
        EditLabel.Height = 13
        EditLabel.Caption = 'CEI:'
        TabOrder = 1
        Text = '123456789102'
      end
      object LabeledEdit8: TLabeledEdit
        Left = 243
        Top = 24
        Width = 398
        Height = 21
        EditLabel.Width = 64
        EditLabel.Height = 13
        EditLabel.Caption = 'Raz'#227'o Social:'
        TabOrder = 2
        Text = 'T2Ti Tecnologia da Informa'#231#227'o'
      end
      object LabeledEdit9: TLabeledEdit
        Left = 11
        Top = 72
        Width = 110
        Height = 21
        EditLabel.Width = 57
        EditLabel.Height = 13
        EditLabel.Caption = 'Data Inicial:'
        TabOrder = 3
        Text = '10012012'
      end
      object LabeledEdit10: TLabeledEdit
        Left = 127
        Top = 72
        Width = 110
        Height = 21
        EditLabel.Width = 52
        EditLabel.Height = 13
        EditLabel.Caption = 'Data Final:'
        TabOrder = 4
        Text = '31122012'
      end
      object Memo3: TMemo
        Left = 3
        Top = 99
        Width = 650
        Height = 286
        TabOrder = 5
      end
      object Button3: TButton
        Left = 204
        Top = 391
        Width = 249
        Height = 25
        Caption = 'Gerar Arquivo ACJEF'
        TabOrder = 6
        OnClick = Button3Click
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Arquivo AEJ'
      ImageIndex = 3
      object Memo4: TMemo
        Left = 3
        Top = 3
        Width = 650
        Height = 374
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Consolas'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
      object Button4: TButton
        Left = 212
        Top = 394
        Width = 249
        Height = 25
        Caption = 'Gerar Arquivo AEJ'
        TabOrder = 1
        OnClick = Button4Click
      end
    end
  end
  object ACBrPonto: TACBrPonto
    Path = '.\'
    Left = 530
    Top = 8
  end
  object OpenDialog: TOpenDialog
    Left = 608
    Top = 8
  end
end
