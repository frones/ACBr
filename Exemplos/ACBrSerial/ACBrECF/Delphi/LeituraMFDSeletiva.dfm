object Form2: TForm2
  Left = 191
  Top = 110
  Width = 441
  Height = 325
  BorderIcons = [biSystemMenu]
  Caption = 'Form2'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 433
    Height = 291
    Align = alClient
    TabOrder = 0
    object GroupBox1: TGroupBox
      Left = 7
      Top = 8
      Width = 409
      Height = 161
      Caption = 'Documentos:'
      TabOrder = 0
      object CheckBox1: TCheckBox
        Left = 13
        Top = 25
        Width = 97
        Height = 17
        Caption = 'RZ'
        TabOrder = 0
      end
      object CheckBox3: TCheckBox
        Left = 13
        Top = 44
        Width = 97
        Height = 17
        Caption = 'CF'
        TabOrder = 1
      end
      object CheckBox4: TCheckBox
        Left = 13
        Top = 64
        Width = 97
        Height = 17
        Caption = 'CFBP'
        TabOrder = 2
      end
      object CheckBox5: TCheckBox
        Left = 13
        Top = 83
        Width = 106
        Height = 17
        Caption = 'Cupom Adicional'
        TabOrder = 3
      end
      object CheckBox6: TCheckBox
        Left = 13
        Top = 103
        Width = 106
        Height = 17
        Caption = 'CCD'
        TabOrder = 4
      end
      object CheckBox7: TCheckBox
        Left = 13
        Top = 123
        Width = 127
        Height = 17
        Caption = 'Via Adicional de CCD'
        TabOrder = 5
      end
      object CheckBox14: TCheckBox
        Left = 285
        Top = 22
        Width = 127
        Height = 17
        Caption = 'Suprimento'
        TabOrder = 6
      end
      object CheckBox15: TCheckBox
        Left = 285
        Top = 41
        Width = 127
        Height = 17
        Caption = 'Estorno'
        TabOrder = 7
      end
      object CheckBox16: TCheckBox
        Left = 285
        Top = 61
        Width = 127
        Height = 17
        Caption = 'RG'
        TabOrder = 8
      end
      object CheckBox17: TCheckBox
        Left = 285
        Top = 81
        Width = 127
        Height = 17
        Caption = 'LMF'
        TabOrder = 9
      end
      object CheckBox8: TCheckBox
        Left = 149
        Top = 23
        Width = 127
        Height = 17
        Caption = '2'#176' Via de CCD'
        TabOrder = 10
      end
      object CheckBox9: TCheckBox
        Left = 149
        Top = 43
        Width = 127
        Height = 17
        Caption = 'Reimpress'#227'o de CCD'
        TabOrder = 11
      end
      object CheckBox10: TCheckBox
        Left = 149
        Top = 62
        Width = 127
        Height = 17
        Caption = 'Estorno de CCD'
        TabOrder = 12
      end
      object CheckBox11: TCheckBox
        Left = 149
        Top = 82
        Width = 127
        Height = 17
        Caption = 'CNF'
        TabOrder = 13
      end
      object CheckBox12: TCheckBox
        Left = 149
        Top = 102
        Width = 127
        Height = 17
        Caption = 'CNF Cancelamento'
        TabOrder = 14
      end
      object CheckBox13: TCheckBox
        Left = 149
        Top = 121
        Width = 127
        Height = 17
        Caption = 'Sangria'
        TabOrder = 15
      end
    end
    object Button1: TButton
      Left = 24
      Top = 253
      Width = 90
      Height = 25
      Caption = 'Enviar Comando'
      TabOrder = 1
      OnClick = Button1Click
    end
    object GroupBox2: TGroupBox
      Left = 8
      Top = 176
      Width = 185
      Height = 65
      Caption = 'COO:'
      TabOrder = 2
      object Label1: TLabel
        Left = 13
        Top = 18
        Width = 30
        Height = 13
        Caption = 'Inicial:'
      end
      object Label2: TLabel
        Left = 91
        Top = 18
        Width = 25
        Height = 13
        Caption = 'Final:'
      end
      object Edit1: TEdit
        Left = 10
        Top = 33
        Width = 57
        Height = 21
        TabOrder = 0
      end
      object Edit2: TEdit
        Left = 88
        Top = 32
        Width = 57
        Height = 21
        TabOrder = 1
      end
    end
    object GroupBox3: TGroupBox
      Left = 200
      Top = 176
      Width = 185
      Height = 66
      Caption = 'Data:'
      TabOrder = 3
    end
  end
  object CheckBox2: TCheckBox
    Left = 22
    Top = 52
    Width = 97
    Height = 17
    Caption = 'LX'
    TabOrder = 1
  end
end
