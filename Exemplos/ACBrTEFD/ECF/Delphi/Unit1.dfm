object Form1: TForm1
  Left = 354
  Top = 110
  Width = 788
  Height = 490
  Caption = 'TEFDDemo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  DesignSize = (
    772
    451)
  PixelsPerInch = 96
  TextHeight = 13
  object Panel3: TPanel
    Left = 0
    Top = 208
    Width = 772
    Height = 193
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 4
    object Splitter1: TSplitter
      Left = 597
      Top = 0
      Width = 5
      Height = 193
      Align = alRight
      Visible = False
    end
    object pnlPagamentosAFazer: TPanel
      Left = 602
      Top = 0
      Width = 170
      Height = 193
      Align = alRight
      Constraints.MinWidth = 170
      TabOrder = 0
      Visible = False
      object Label12: TLabel
        Left = 1
        Top = 1
        Width = 168
        Height = 13
        Align = alTop
        Alignment = taCenter
        Caption = 'Pagamentos a Fazer'
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
      end
      object Panel5: TPanel
        Left = 1
        Top = 127
        Width = 168
        Height = 65
        Align = alBottom
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        object spbAdicionaPagamento: TSpeedButton
          Left = 14
          Top = 5
          Width = 23
          Height = 22
          Hint = 'Adiciona Pagamento'
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00007D00FF007D00FF007D00FF007D
            00FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00007D00FF31E38CFF31E78CFF007D
            00FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00007D00FF42FBADFF42FFADFF007D
            00FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00007D00FF00EF7BFF00EF7BFF007D
            00FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00007D00FF29E37BFF29E37BFF007D
            00FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00007D
            00FF007D00FF007D00FF007D00FF007D00FF007D00FF39DB7BFF39DB73FF007D
            00FF007D00FF007D00FF007D00FF007D00FF007D00FFFF00FF00FF00FF00219E
            31FF63D384FF63D784FF5AD784FF5ADB84FF4AD77BFF42D373FF42D373FF4AD7
            7BFF52D77BFF4AD37BFF4ACF73FF4ACB6BFF189E21FFFF00FF00FF00FF00219E
            29FF73D37BFF6BD37BFF6BD37BFF63DB84FF5AD37BFF52D373FF52CF73FF52D3
            73FF5AD773FF52CF73FF52CB6BFF52C763FF189A21FFFF00FF00FF00FF004FA5
            4FFF4AA64FFF4BA650FF4BA650FF4BA050FF43AA5AFF6BD37BFF63D37BFF4BAA
            5BFF4C9E51FF4BA650FF4BA650FF4AA64FFF4DA74DFFFF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF004BA358FF73D784FF73D384FF52A3
            59FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF0050A257FF84D78CFF84D384FF58A3
            58FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF0056A25EFF9CDF9CFF94DB94FF5FA3
            5FFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF005A9B60FF84C784FF84C784FF629B
            69FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF0046974BFF3A983AFF3A983AFF4697
            4BFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
          OnClick = spbAdicionaPagamentoClick
        end
        object spRemovePagamento: TSpeedButton
          Left = 46
          Top = 5
          Width = 23
          Height = 22
          Hint = 'Remove Pagamento'
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000014
            BDFF0014BDFF0014BDFF0014BDFF0014BDFF0014BDFF0014BDFF0014BDFF0014
            BDFF0014BDFF0014BDFF0014BDFF0014BDFF0014BDFFFF00FF00FF00FF000014
            BDFF4265E7FF3965E7FF3969EFFF3969EFFF396DEFFF396DEFFF396DEFFF396D
            EFFF3969EFFF3969EFFF3965E7FF3961E7FF0014BDFFFF00FF00FF00FF000014
            BDFF3169EFFF3171F7FF3175F7FF3179FFFF3179FFFF3179FFFF3179FFFF3179
            FFFF3175F7FF3171F7FF3169EFFF3165EFFF0014BDFFFF00FF00FF00FF000014
            BDFF7B96EFFF7B96F7FF7B9AF7FF7B9AF7FF7B9EF7FF7B9EF7FF7B9EF7FF7B9A
            F7FF7B9AF7FF7B96F7FF7B92EFFF7B92EFFF0014BDFFFF00FF00FF00FF000014
            BDFF7E92EAFF7E97F0FF7E99F0FF7E9EF5FF7EA1F5FF7EA1F5FF7EA1F5FF7EA1
            F5FF7EA1F5FF7E9EF5FF7E99F0FF7E97F0FF0014BDFFFF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
          OnClick = spRemovePagamentoClick
        end
        object spbLimpaPagamentos: TSpeedButton
          Left = 134
          Top = 5
          Width = 23
          Height = 22
          Hint = 'Limpa Pagamentos'
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00FF00FF00FF00FF00EDEDF000A3A3B300F2F2F300FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00F3F3F9000F0E170030312B001F1F2B0069697B00CBCBCD00F9F9
            F900FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00808085001F1F1E00FF00FF00E3E3E300E2E2E300D8D8D900BDBDBD007373
            73003333330087878700FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00F2F3
            FC0000000000CFCFD000F9F9F900CBCBCB00D0D0D000DADADA00F4F4F400F5F5
            F5008F8F8F00171717000000000080808000FF00FF00FF00FF00FF00FF006161
            5E006C6C8100D7D6CF00D1D1CF00D7D7D700DADADA00E1E1E100DCDCDC00DADA
            DA00FF00FF00FF00FF00C9C9C9003A3A3A0063636300EDEDED00DADAD8000000
            0B0027267D004746790094959A00C7C7C200DDDDDE00DFDFDF00E2E2E200E2E2
            E200F6F6F600D2D2D2009B9B9B00929292000000000039393900C9C9C7000000
            13003739BC00262579001E1D6B001D1C6E0082829500C1C2B900DEDEDC00F1F1
            F3003939390069696900FF00FF00FF00FF00F1F1F100FF00FF00FF00FF007474
            7300222880003C43D9002A2A8D00242372001B1A6D004F4E8000B2B2AA00E2E2
            D90040404000F4F4F400FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF001A190800141737002B2C8C0027267F0023236E0011116800242474005454
            7000C7C7C700FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FEFEFE00FF00
            FF00222224001E1E590023227300232371003132A00029287B00201F72001110
            5F00C1C2C000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00A4A4
            A20022236A0027267F00101046002A2926003E3F410000001C00000014002A2A
            2A00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FCFCFC002929
            27002524770028277E00000000009C9C9A00FAFAF900BAB9B600AFAFAC00F8F7
            F700FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00A5A6A2000000
            1D002C2A8F00000020004D4D4700FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00BDBDB9000002
            1F002224840000000000ECECEB00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FDFD
            FC00D2D2D000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
          OnClick = spbLimpaPagamentosClick
        end
        object spbAdicionaPagamento1: TSpeedButton
          Left = 102
          Top = 5
          Width = 23
          Height = 22
          Hint = 'Saldo Restante'
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00E3BDA500CFAD9700CFAD9700E3BD
            A500FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00D8B39B00A78E7F007D7975007D79750066666600675E
            5600806C6000C4A18C00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00C3A49100999999009AA1A50099999900958580008F827D008A89
            8A006E7579004C4B4A0089706100FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00CCAD9A00ACB0B300B5ACA700A46D5200964926008D3C18008D3C1800813F
            28008663590087898B004C4B4A0089706100FF00FF00FF00FF00FF00FF00E3BD
            A500C0C0C100C6B7A400AC5E2E00A54E1800AB7E6200ACB0B3009F7C69008D3C
            18007E2E10007F4A3A0087898B004C4B4A00BE9C8700FF00FF00FF00FF00D8C5
            B400CCCCCC00BC754000B76A2B00B76A2B00B6875B00B8A58E00A87E5D009652
            26008D4621007E2E1000845C5000767E82007A675C00FF00FF00D8C5B400DCD8
            D700CFAD9700C2752B00C4833B00C0813400BF8E5300C0AA9200B6875B00A15D
            2000965226008C431D00813A1F008685850066666600D4AF9800D8C5B400E6E5
            E600D7A56D00CD8A3700CE954000CD8A3700CDAD7600D7E5F900C6B7A400AD71
            3000A15D2000965226008D3C1800977C72007B7B7900BE9C8700ECD4C700E6E5
            E600D7A56D00D6993E00DDA44200DDA44200DDA44200D2B57E00D0D6DB00C9C4
            BC00AF784100A15D200091401700977C720086858500BE9C8700ECD4C700E6E5
            E600DCB37A00DDA44200E2B64E00E1BB4C00DBB43A00D09D2100D3B78200D7E1
            F100C4A18C00A15D20009A4A1D00A08B83008A898A00D8B39B00FF00FF00E7DF
            DD00E9C7B200DDA44200E7BE5000F0E0A600F0E0A600E1C57700DFD4BE00D7E5
            F900C0AA9200AD611F00A55F3400A6ABAE00A3928800FF00FF00FF00FF00ECD4
            C700E7EAEC00E6BC6800E8B44300F6E3AC00FFFFFF00F3F6FB00EEEDEE00DCCF
            BA00C4833B00B15F1C00BE9C8700A6ABAE00CFAD9700FF00FF00FF00FF00FF00
            FF00E8DDD900ECE7E800E6BC6800E8B44300E7BE5000E6BC6800DCAA5900CD88
            2D00C2752B00C59E7D00BFC5CA00C0AA9C00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00E7DFDD00EEEDEE00E9D1A500E6BC6800DCA04E00DCA04E00D7A5
            6D00D8C5B400D0D6DB00CDB7AB00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00ECD4C700ECE7E800EEEDEE00EEEDEE00ECE7E800ECE7
            E800DCD8D700E1C2AF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00EEC5AC00ECD4C700ECD4C700ECD4C700E9C7
            B200EEC5AC00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
          OnClick = spbAdicionaPagamento1Click
        end
        object lMeuAcresDesc: TLabel
          Left = 25
          Top = 42
          Width = 54
          Height = 13
          Alignment = taRightJustify
          Caption = 'Acrec/Desc'
          Color = clBtnFace
          ParentColor = False
        end
        object edValorDescAcre: TEdit
          Left = 86
          Top = 34
          Width = 72
          Height = 25
          AutoSize = False
          TabOrder = 0
          Text = '0,00'
        end
      end
      object mPagamentos: TMemo
        Left = 1
        Top = 14
        Width = 168
        Height = 113
        Align = alClient
        ReadOnly = True
        TabOrder = 1
      end
    end
    object Memo1: TMemo
      Left = 0
      Top = 0
      Width = 597
      Height = 193
      Align = alClient
      Lines.Strings = (
        'Memo1')
      TabOrder = 1
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 772
    Height = 208
    ActivePage = tsOperacao
    Align = alTop
    TabOrder = 1
    OnChange = PageControl1Change
    object tsConfig: TTabSheet
      Caption = 'Configura'#231#227'o'
      object gbConfigECF: TGroupBox
        Left = 0
        Top = 0
        Width = 308
        Height = 180
        Align = alLeft
        Caption = 'ECF'
        TabOrder = 0
        object Label2: TLabel
          Left = 20
          Top = 33
          Width = 38
          Height = 13
          Caption = 'Modelo:'
          Color = clBtnFace
          ParentColor = False
        end
        object Label4: TLabel
          Left = 20
          Top = 76
          Width = 26
          Height = 13
          Caption = 'Porta'
          Color = clBtnFace
          ParentColor = False
        end
        object btSerial: TSpeedButton
          Left = 156
          Top = 91
          Width = 25
          Height = 24
          Hint = 'Serial'
          Glyph.Data = {
            36030000424D3603000000000000360000002800000010000000100000000100
            1800000000000003000000000000000000000000000000000000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF323232
            3232323E3E3E565656FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF3E3E3EFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFF565656FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF3E3E3EFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFF503200FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            565656565656FFFFFFFFFFFF3232322626262626262626262626265032005032
            000000504873FFFFFFFFFFFFFFFFFFFF6E6E6EFFFFFFFFFFFFFFFFFFFFFFFF6E
            6E6E32323232323232323232323250320000005025AAFFFFFFFFFFFFFF565656
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF5656563232323232326E6E6E5032005032
            008FFF6B8ED4FFFFFFFFFFFFFFFFFFFF3E3E3EFFFFFFFFFFFF50320050320056
            56564A4A4A5050003232325032005032008FFF6B8ED4FFFFFFFFFFFFFFFFFFFF
            FFFFFF5656563E3E3E2626265032006262625656565050003232325032005032
            008FFF6B8ED4FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF5050005050006E
            6E6E5656565050003250005032005032008FFF6B8ED4FFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8686865656565656563250005032005032
            008FFF6B48B8FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF3232323E
            3E3EA4A0A08686866E6E6E565656503200C0C0C02557FFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFF5050004A4A4A3232323232323232323232325032
            00FFFFFF6B8FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
          OnClick = btSerialClick
        end
        object Label6: TLabel
          Left = 196
          Top = 33
          Width = 82
          Height = 13
          Caption = 'Indice "CARTAO"'
          Color = clBtnFace
          ParentColor = False
        end
        object Label7: TLabel
          Left = 196
          Top = 76
          Width = 81
          Height = 13
          Caption = 'Indice "CHEQUE"'
          Color = clBtnFace
          ParentColor = False
        end
        object cbxModelo: TComboBox
          Left = 21
          Top = 49
          Width = 161
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
          OnChange = cbxModeloChange
          Items.Strings = (
            '')
        end
        object cbxPorta: TComboBox
          Left = 20
          Top = 92
          Width = 131
          Height = 21
          ItemHeight = 13
          ItemIndex = 0
          TabOrder = 1
          Text = 'Procurar'
          OnChange = cbxPortaChange
          Items.Strings = (
            'Procurar'
            'COM1'
            'COM2'
            'COM3'
            'COM4'
            'COM5'
            'COM6'
            'LPT1'
            'LPT2'
            'LPT3'
            '/dev/ttyS0'
            '/dev/ttyS1'
            '/dev/ttyUSB0'
            '/dev/ttyUSB1'
            'c:\temp\ecf.txt'
            '/tmp/ecf.txt')
        end
        object bAtivar: TButton
          Left = 68
          Top = 128
          Width = 73
          Height = 25
          Caption = 'Ativar'
          TabOrder = 2
          OnClick = bAtivarClick
        end
        object edFPGCartao: TEdit
          Left = 220
          Top = 49
          Width = 43
          Height = 21
          TabOrder = 3
          Text = '02'
        end
        object edFPGCheque: TEdit
          Left = 220
          Top = 92
          Width = 43
          Height = 21
          TabOrder = 4
          Text = '03'
        end
        object bFPG: TButton
          Left = 204
          Top = 128
          Width = 73
          Height = 25
          Caption = 'FPG'
          TabOrder = 5
          OnClick = bFPGClick
        end
      end
      object Panel2: TPanel
        Left = 308
        Top = 0
        Width = 456
        Height = 180
        Align = alClient
        TabOrder = 1
        object gbConfigTEF: TGroupBox
          Left = 1
          Top = 1
          Width = 454
          Height = 178
          Align = alClient
          Caption = 'TEF'
          TabOrder = 0
          object Label1: TLabel
            Left = 42
            Top = 24
            Width = 82
            Height = 13
            Caption = 'Selecionar o G.P.'
            Color = clBtnFace
            ParentColor = False
          end
          object Label8: TLabel
            Left = 317
            Top = 23
            Width = 59
            Height = 13
            Caption = 'EsperaSleep'
            Color = clBtnFace
            ParentColor = False
          end
          object Label9: TLabel
            Left = 317
            Top = 79
            Width = 51
            Height = 13
            Caption = 'EsperaSTS'
            Color = clBtnFace
            ParentColor = False
          end
          object bInicializar: TButton
            Left = 53
            Top = 79
            Width = 75
            Height = 25
            Caption = 'Inicializar'
            TabOrder = 1
            OnClick = bInicializarClick
          end
          object ckAutoAtivar: TCheckBox
            Left = 169
            Top = 34
            Width = 104
            Height = 19
            Caption = 'Auto Ativar G.P.'
            Checked = True
            State = cbChecked
            TabOrder = 3
            OnClick = ckAutoAtivarChange
          end
          object cbxGP: TComboBox
            Left = 21
            Top = 44
            Width = 133
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 0
            OnChange = cbxGPChange
          end
          object bAtivarGP: TButton
            Left = 53
            Top = 111
            Width = 75
            Height = 25
            Caption = 'Ativar GP'
            TabOrder = 2
            OnClick = bAtivarGPClick
          end
          object ckMultiplosCartoes: TCheckBox
            Left = 169
            Top = 62
            Width = 113
            Height = 19
            Caption = 'Multiplos Cart'#245'es'
            Checked = True
            State = cbChecked
            TabOrder = 4
            OnClick = ckMultiplosCartoesChange
          end
          object ckAutoFinalizarCupom: TCheckBox
            Left = 169
            Top = 113
            Width = 129
            Height = 19
            Caption = 'AutoFinalizarCupom'
            Checked = True
            State = cbChecked
            TabOrder = 6
            OnClick = ckAutoFinalizarCupomChange
          end
          object ckAutoEfetuarPagamento: TCheckBox
            Left = 169
            Top = 88
            Width = 144
            Height = 19
            Caption = 'AutoEfetuarPagamento'
            TabOrder = 5
            OnClick = ckAutoEfetuarPagamentoChange
          end
          object edEsperaSleep: TEdit
            Left = 317
            Top = 39
            Width = 56
            Height = 21
            TabOrder = 7
            Text = '250'
            OnChange = edEsperaSleepChange
          end
          object edEsperaSTS: TEdit
            Left = 317
            Top = 95
            Width = 56
            Height = 21
            TabOrder = 8
            Text = '7'
            OnChange = edEsperaSTSChange
          end
          object ckCHQemGerencial: TCheckBox
            Left = 169
            Top = 139
            Width = 118
            Height = 19
            Caption = 'CHQ em Gerencial'
            Checked = True
            State = cbChecked
            TabOrder = 9
            OnClick = ckCHQemGerencialChange
          end
        end
      end
    end
    object tsOperacao: TTabSheet
      Caption = 'Opera'#231#227'o'
      object gbCupomECF: TGroupBox
        Left = 0
        Top = 0
        Width = 292
        Height = 180
        Align = alLeft
        Caption = 'ECF'
        Enabled = False
        TabOrder = 0
        object Label5: TLabel
          Left = 225
          Top = 21
          Width = 46
          Height = 13
          Alignment = taRightJustify
          Caption = 'Valor ECF'
          Color = clBtnFace
          ParentColor = False
        end
        object bCancelar: TButton
          Left = 111
          Top = 82
          Width = 93
          Height = 25
          Caption = 'Cancelar'
          TabOrder = 5
          OnClick = bCancelarClick
        end
        object bAbreCupom: TButton
          Left = 9
          Top = 18
          Width = 93
          Height = 25
          Caption = 'Abrir'
          TabOrder = 0
          OnClick = bAbreCupomClick
        end
        object bVendeItem: TButton
          Left = 111
          Top = 18
          Width = 93
          Height = 25
          Caption = 'Vende Item'
          TabOrder = 1
          OnClick = bVendeItemClick
        end
        object bSubTotaliza: TButton
          Left = 9
          Top = 50
          Width = 93
          Height = 25
          Caption = 'SubTotalizar'
          TabOrder = 2
          OnClick = bSubTotalizaClick
        end
        object bFechar: TButton
          Left = 9
          Top = 82
          Width = 93
          Height = 25
          Caption = 'Fechar'
          TabOrder = 4
          OnClick = bFecharClick
        end
        object bAbreVendeSubTotaliza: TButton
          Left = 9
          Top = 112
          Width = 195
          Height = 27
          Caption = 'AbreVendeSubtotaliza'
          TabOrder = 6
          OnClick = bAbreVendeSubTotalizaClick
        end
        object edValorECF: TEdit
          Left = 210
          Top = 37
          Width = 72
          Height = 25
          AutoSize = False
          TabOrder = 9
          Text = '1'
        end
        object bEstado: TButton
          Left = 213
          Top = 82
          Width = 72
          Height = 25
          Caption = 'Estado'
          TabOrder = 8
          OnClick = bEstadoClick
        end
        object bReducaoZ: TButton
          Left = 213
          Top = 146
          Width = 72
          Height = 27
          Caption = 'Redu'#231#227'o Z'
          TabOrder = 11
          OnClick = bReducaoZClick
        end
        object bLeituraX: TButton
          Left = 213
          Top = 112
          Width = 72
          Height = 27
          Caption = 'Leitura X'
          TabOrder = 10
          OnClick = bLeituraXClick
        end
        object bFechaRelatorio: TButton
          Left = 9
          Top = 146
          Width = 195
          Height = 27
          Caption = 'Fechar Gerencial/Vinculado'
          TabOrder = 7
          OnClick = bFechaRelatorioClick
        end
        object bPagamento: TButton
          Left = 111
          Top = 50
          Width = 93
          Height = 25
          Caption = 'Pagamento'
          TabOrder = 3
          OnClick = bPagamentoClick
        end
      end
      object gbComandosTEF: TGroupBox
        Left = 292
        Top = 0
        Width = 472
        Height = 180
        Align = alClient
        Caption = 'TEF'
        TabOrder = 1
        object Label3: TLabel
          Left = 43
          Top = 126
          Width = 45
          Height = 13
          Alignment = taRightJustify
          Caption = 'Valor TEF'
          Color = clBtnFace
          ParentColor = False
        end
        object bADM: TButton
          Left = 30
          Top = 53
          Width = 68
          Height = 25
          Caption = 'ADM'
          TabOrder = 2
          OnClick = bADMClick
        end
        object bATV: TButton
          Left = 177
          Top = 13
          Width = 68
          Height = 25
          Caption = 'ATV'
          TabOrder = 1
          OnClick = bATVClick
        end
        object bCNC: TButton
          Left = 177
          Top = 92
          Width = 68
          Height = 25
          Caption = 'CNC'
          TabOrder = 7
          OnClick = bCNCClick
        end
        object bCNF: TButton
          Left = 103
          Top = 53
          Width = 68
          Height = 25
          Caption = 'CNF'
          TabOrder = 3
          OnClick = bCNFClick
        end
        object bNCN: TButton
          Left = 177
          Top = 53
          Width = 68
          Height = 25
          Caption = 'NCN'
          TabOrder = 4
          OnClick = bNCNClick
        end
        object bCRT: TButton
          Left = 29
          Top = 92
          Width = 68
          Height = 25
          Caption = 'CRT'
          TabOrder = 5
          OnClick = bCRTClick
        end
        object bCHQ: TButton
          Left = 103
          Top = 92
          Width = 68
          Height = 25
          Caption = 'CHQ'
          TabOrder = 6
          OnClick = bCHQClick
        end
        object edValorTEF: TEdit
          Left = 26
          Top = 142
          Width = 72
          Height = 25
          AutoSize = False
          TabOrder = 8
          Text = '1'
        end
        object ckMultiplosCartoes1: TCheckBox
          Left = 126
          Top = 141
          Width = 113
          Height = 19
          Caption = 'Multiplos Cart'#245'es'
          TabOrder = 9
          OnClick = ckMultiplosCartoesChange
        end
        object cbxGP1: TComboBox
          Left = 30
          Top = 15
          Width = 142
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
          OnChange = cbxGPChange
        end
        object bAbreVendeSubTotaliza1: TButton
          Left = 254
          Top = 13
          Width = 187
          Height = 27
          Caption = 'CancelarTransacoesPendentes'
          TabOrder = 10
          OnClick = bAbreVendeSubTotaliza1Click
        end
        object bAbreVendeSubTotaliza2: TButton
          Left = 254
          Top = 42
          Width = 187
          Height = 27
          Caption = 'ConfirmarTransacoesPendentes'
          TabOrder = 11
          OnClick = bAbreVendeSubTotaliza2Click
        end
        object bAbreVendeSubTotaliza3: TButton
          Left = 254
          Top = 71
          Width = 187
          Height = 27
          Caption = 'ImprimirTransacoesPendentes'
          TabOrder = 12
          OnClick = bAbreVendeSubTotaliza3Click
        end
        object bAbreVendeSubTotaliza4: TButton
          Left = 254
          Top = 100
          Width = 187
          Height = 27
          Caption = 'FinalizarCupom'
          TabOrder = 13
          OnClick = bAbreVendeSubTotaliza4Click
        end
        object GroupBox1: TGroupBox
          Left = 256
          Top = 128
          Width = 185
          Height = 45
          Caption = 'TEF Dire'#231#227'o'
          TabOrder = 14
          object ComboBox1: TComboBox
            Left = 12
            Top = 17
            Width = 166
            Height = 21
            ItemHeight = 13
            TabOrder = 0
            Items.Strings = (
              'Nenhuma Transa'#195#167#195#163'o'
              'Cart'#195#163'o de Cr'#195#169'dito'
              'Cart'#195#163'o de D'#195#169'bito'
              'Cart'#195#163'o Voucher'
              'Cart'#195#163'o Private Label'
              'Cheque'
              'Controle de Frota')
          end
        end
        object ckViaClienteReduzida: TCheckBox
          Left = 126
          Top = 158
          Width = 121
          Height = 19
          Caption = 'Via Cliente Reduzida'
          TabOrder = 15
          OnClick = ckViaClienteReduzidaClick
        end
      end
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 428
    Width = 772
    Height = 23
    Panels = <
      item
        Width = 100
      end
      item
        Width = 100
      end
      item
        Width = 50
      end>
  end
  object Panel1: TPanel
    Left = 0
    Top = 401
    Width = 772
    Height = 27
    Align = alBottom
    BevelInner = bvLowered
    TabOrder = 2
    object sECF: TShape
      Left = 8
      Top = 7
      Width = 17
      Height = 16
      Brush.Color = clRed
      Shape = stCircle
    end
    object lECFName: TLabel
      Left = 31
      Top = 8
      Width = 48
      Height = 13
      Caption = 'lECFName'
      Color = clBtnFace
      ParentColor = False
    end
    object sTEFDial: TShape
      Left = 97
      Top = 7
      Width = 17
      Height = 16
      Brush.Color = clRed
      Shape = stCircle
    end
    object sTEFDisc: TShape
      Left = 193
      Top = 7
      Width = 17
      Height = 16
      Brush.Color = clRed
      Shape = stCircle
    end
    object sHiperTEF: TShape
      Left = 289
      Top = 7
      Width = 17
      Height = 16
      Brush.Color = clRed
      Shape = stCircle
    end
    object sCliSiTef: TShape
      Left = 393
      Top = 7
      Width = 17
      Height = 16
      Brush.Color = clRed
      Shape = stCircle
    end
    object sVSPague: TShape
      Left = 481
      Top = 7
      Width = 17
      Height = 16
      Brush.Color = clRed
      Shape = stCircle
    end
    object sAuttar: TShape
      Left = 579
      Top = 7
      Width = 17
      Height = 16
      Brush.Color = clRed
      Shape = stCircle
    end
    object ckTEFDIAL: TCheckBox
      Left = 121
      Top = 5
      Width = 69
      Height = 19
      Caption = 'TEF_DIAL'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = ckTEFDIALChange
    end
    object ckTEFDISC: TCheckBox
      Left = 217
      Top = 6
      Width = 69
      Height = 19
      Caption = 'TEF_DISC'
      TabOrder = 1
      OnClick = ckTEFDISCChange
    end
    object ckHIPERTEF: TCheckBox
      Left = 313
      Top = 7
      Width = 76
      Height = 19
      Caption = 'HIPER_TEF'
      TabOrder = 2
      OnClick = ckHIPERTEFChange
    end
    object ckCliSiTef: TCheckBox
      Left = 417
      Top = 6
      Width = 60
      Height = 19
      Caption = 'CliSiTef'
      TabOrder = 3
      OnClick = ckCliSiTefChange
    end
    object bCancelarResp: TButton
      Left = 682
      Top = 2
      Width = 88
      Height = 23
      Caption = 'CancelarResp'
      TabOrder = 4
      Visible = False
      OnClick = bCancelarRespClick
    end
    object ckVSPague: TCheckBox
      Left = 505
      Top = 5
      Width = 72
      Height = 19
      Caption = 'VeSPague'
      TabOrder = 5
      OnClick = ckVSPagueChange
    end
    object ckAuttar: TCheckBox
      Left = 603
      Top = 6
      Width = 53
      Height = 19
      Caption = 'Auttar'
      TabOrder = 6
      OnClick = ckAuttarChange
    end
  end
  object pMensagem: TPanel
    Left = 80
    Top = 240
    Width = 440
    Height = 136
    Anchors = []
    BevelInner = bvLowered
    BevelWidth = 2
    BorderStyle = bsSingle
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    Visible = False
    OnResize = pMensagemResize
    object pMensagemOperador: TPanel
      Left = 4
      Top = 4
      Width = 428
      Height = 62
      Align = alClient
      TabOrder = 0
      Visible = False
      OnClick = pMensagemOperadorClick
      object lMensagemOperador: TLabel
        Left = 1
        Top = 1
        Width = 426
        Height = 60
        Align = alClient
        Alignment = taCenter
        Caption = 'lMensagemOperador'
        Color = clBtnFace
        ParentColor = False
        Layout = tlCenter
        WordWrap = True
        OnClick = pMensagemOperadorClick
      end
      object Label10: TLabel
        Left = 0
        Top = 0
        Width = 118
        Height = 13
        Caption = 'Mensagem Operador'
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
      end
    end
    object pMensagemCliente: TPanel
      Left = 4
      Top = 66
      Width = 428
      Height = 62
      Align = alBottom
      TabOrder = 1
      Visible = False
      OnClick = pMensagemOperadorClick
      object Label11: TLabel
        Left = 0
        Top = 0
        Width = 104
        Height = 13
        Caption = 'Mensagem Cliente'
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
      end
      object lMensagemCliente: TLabel
        Left = 1
        Top = 1
        Width = 426
        Height = 60
        Align = alClient
        Alignment = taCenter
        Caption = 'lMensagemCliente'
        Color = clBtnFace
        ParentColor = False
        Layout = tlCenter
        WordWrap = True
        OnClick = pMensagemOperadorClick
      end
    end
  end
  object ACBrECF1: TACBrECF
    QuebraLinhaRodape = False
    Modelo = ecfDaruma
    Porta = 'COM1'
    ReTentar = False
    TimeOut = 6
    IntervaloAposComando = 0
    MsgAguarde = 'Aguardando a resposta da Impressora: %d segundos'
    MsgTrabalhando = 'Impressora est'#225' trabalhando'
    ExibeMensagem = False
    TempoInicioMsg = 6
    BloqueiaMouseTeclado = False
    MsgRelatorio = 'Imprimindo %s  %d'#170' Via '
    MsgPausaRelatorio = 'Destaque a %d'#194#170' via, <ENTER> proxima, %d seg.'
    MaxLinhasBuffer = 3
    PaginaDeCodigo = 28591
    OnMsgPoucoPapel = ACBrECF1MsgPoucoPapel
    MemoParams.Strings = (
      '[Cabecalho]'
      'LIN000=<center><b>Nome da Empresa</b></center>'
      'LIN001=<center>Nome da Rua , 1234  -  Bairro</center>'
      'LIN002=<center>Cidade  -  UF  -  99999-999</center>'
      
        'LIN003=<center>CNPJ: 01.234.567/0001-22    IE: 012.345.678.90</c' +
        'enter>'
      
        'LIN004=<table width=100%><tr><td align=left><code>Data</code> <c' +
        'ode>Hora</code></td><td align=right>COO: <b><code>NumCupom</code' +
        '></b></td></tr></table>'
      'LIN005=<hr>'
      ' '
      '[Cabecalho_Item]'
      'LIN000=ITEM   CODIGO     DESCRICAO'
      'LIN001=QTD         x UNITARIO       Aliq     VALOR (R$)'
      'LIN002=<hr>'
      
        'MascaraItem=III CCCCCCCCCCCCC DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDQQQQ' +
        'QQQQ UU x VVVVVVVVVVVVV AAAAAA TTTTTTTTTTTTT'
      ' '
      '[Rodape]'
      'LIN000=<hr>'
      
        'LIN001=<table width=100%><tr><td align=left><code>Data</code> <c' +
        'ode>Hora</code></td><td align=right>Projeto ACBr: <b><code>ACBR<' +
        '/code></b></td></tr></table>'
      'LIN002=<center>Obrigado Volte Sempre</center>'
      'LIN003=<hr>'
      ' '
      '[Formato]'
      'Colunas=48'
      'HTML=1'
      'HTML_Title_Size=2'
      'HTML_Font=<font size="2" face="Lucida Console">')
    Device.HandShake = hsRTS_CTS
    Device.HardFlow = True
    ArqLOG = 'ecf.log'
    ConfigBarras.MostrarCodigo = True
    ConfigBarras.LarguraLinha = 3
    ConfigBarras.Altura = 10
    ConfigBarras.Margem = 0
    InfoRodapeCupom.Imposto.ModoCompacto = False
    Left = 52
    Top = 290
  end
  object ACBrTEFD1: TACBrTEFD
    Identificacao.NomeAplicacao = 'TEFDDemo'
    Identificacao.VersaoAplicacao = '3.0'
    Identificacao.SoftwareHouse = 'ACBr'
    Identificacao.RazaoSocial = 'Projeto ACBr'
    MultiplosCartoes = True
    EsperaSTS = 7
    CHQEmGerencial = True
    TEFDial.ArqLOG = 'TEF_DIAL.log'
    TEFDial.Habilitado = True
    TEFDial.ArqTemp = 'C:\TEF_DIAL\req\intpos.tmp'
    TEFDial.ArqReq = 'C:\TEF_DIAL\req\intpos.001'
    TEFDial.ArqSTS = 'C:\TEF_DIAL\resp\intpos.sts'
    TEFDial.ArqResp = 'C:\TEF_DIAL\resp\intpos.001'
    TEFDial.GPExeName = 'C:\TEF_DIAL\tef_dial.exe'
    TEFDisc.ArqTemp = 'C:\TEF_Disc\req\intpos.tmp'
    TEFDisc.ArqReq = 'C:\TEF_Disc\req\intpos.001'
    TEFDisc.ArqSTS = 'C:\TEF_Disc\resp\intpos.sts'
    TEFDisc.ArqResp = 'C:\TEF_Disc\resp\intpos.001'
    TEFDisc.GPExeName = 'C:\TEF_Disc\tef_Disc.exe'
    TEFHiper.ArqTemp = 'c:\HiperTEF\req\IntPos.tmp'
    TEFHiper.ArqReq = 'C:\HiperTEF\req\IntPos.001'
    TEFHiper.ArqSTS = 'C:\HiperTEF\resp\IntPos.sts'
    TEFHiper.ArqResp = 'C:\HiperTEF\resp\IntPos.001'
    TEFHiper.GPExeName = 'C:\HiperTEF\HiperTEF.exe'
    TEFCliSiTef.ArqLOG = 'CliSiTef.log'
    TEFCliSiTef.EnderecoIP = 'localhost'
    TEFCliSiTef.CodigoLoja = '00000000'
    TEFCliSiTef.NumeroTerminal = 'SE000001'
    TEFCliSiTef.OnExibeMenu = ACBrTEFD1CliSiTefExibeMenu
    TEFCliSiTef.OnObtemCampo = ACBrTEFD1CliSiTefObtemCampo
    TEFVeSPague.ArqLOG = 'VeSPague.log'
    TEFVeSPague.Aplicacao = 'ACBr_TEFDDemo'
    TEFVeSPague.AplicacaoVersao = '1.0'
    TEFVeSPague.GPExeName = 'C:\VeSPague\Client\VeSPagueClient.bat'
    TEFVeSPague.GPExeParams = '189.115.24.32 65432'
    TEFVeSPague.EnderecoIP = 'localhost'
    TEFVeSPague.Porta = '60906'
    TEFVeSPague.TimeOut = 500
    TEFVeSPague.TemPendencias = False
    TEFVeSPague.TransacaoCRT = 'Cartao Vender'
    TEFVeSPague.TransacaoCHQ = 'Cheque Consultar'
    TEFVeSPague.TransacaoCNC = 'Administracao Cancelar'
    TEFVeSPague.TransacaoReImpressao = 'Administracao Reimprimir'
    TEFVeSPague.TransacaoPendente = 'Administracao Pendente'
    TEFVeSPague.OnExibeMenu = ACBrTEFD1VeSPagueExibeMenu
    TEFGPU.ArqTemp = 'C:\TEF_GPU\req\intpos.tmp'
    TEFGPU.ArqReq = 'C:\TEF_GPU\req\intpos.001'
    TEFGPU.ArqSTS = 'C:\TEF_GPU\resp\intpos.sts'
    TEFGPU.ArqResp = 'C:\TEF_GPU\resp\intpos.001'
    TEFGPU.GPExeName = 'C:\TEF_GPU\GPU.exe'
    TEFBanese.ArqTemp = 'C:\bcard\req\pergunta.tmp'
    TEFBanese.ArqReq = 'C:\bcard\req\pergunta.txt'
    TEFBanese.ArqSTS = 'C:\bcard\resp\status.txt'
    TEFBanese.ArqResp = 'C:\bcard\resp\resposta.txt'
    TEFBanese.ArqRespBkp = 'C:\bcard\resposta.txt'
    TEFBanese.ArqRespMovBkp = 'C:\bcard\copiamovimento.txt'
    TEFBanese.OnObtemInformacao = BaneseObtemInformacao
    TEFAuttar.ArqTemp = 'C:\Auttar_TefIP\req\intpos.tmp'
    TEFAuttar.ArqReq = 'C:\Auttar_TefIP\req\intpos.001'
    TEFAuttar.ArqSTS = 'C:\Auttar_TefIP\resp\intpos.sts'
    TEFAuttar.ArqResp = 'C:\Auttar_TefIP\resp\intpos.001'
    TEFAuttar.GPExeName = 'C:\Program Files (x86)\Auttar\IntegradorTEF-IP.exe'
    TEFGood.ArqTemp = 'C:\good\gettemp.dat'
    TEFGood.ArqReq = 'C:\good\getreq.dat'
    TEFGood.ArqSTS = 'C:\good\getstat.dat'
    TEFGood.ArqResp = 'C:\good\getresp.dat'
    TEFGood.GPExeName = 'C:\good\GETGoodMed.exe'
    TEFFoxWin.ArqTemp = 'C:\FwTEF\req\intpos.tmp'
    TEFFoxWin.ArqReq = 'C:\FwTEF\req\intpos.001'
    TEFFoxWin.ArqSTS = 'C:\FwTEF\rsp\intpos.sts'
    TEFFoxWin.ArqResp = 'C:\FwTEF\rsp\intpos.001'
    TEFFoxWin.GPExeName = 'C:\FwTEF\bin\FwTEF.exe'
    TEFCliDTEF.OnExibeMenu = CliDTEFExibeMenu
    TEFCliDTEF.OnObtemInformacao = CliDTEFObtemInformacao
    TEFPetrocard.ArqTemp = 'C:\CardTech\req\intpos.tmp'
    TEFPetrocard.ArqReq = 'C:\CardTech\req\intpos.001'
    TEFPetrocard.ArqSTS = 'C:\CardTech\resp\intpos.sts'
    TEFPetrocard.ArqResp = 'C:\CardTech\resp\intpos.001'
    TEFPetrocard.GPExeName = 'C:\CardTech\sac.exe'
    TEFCrediShop.ArqTemp = 'C:\tef_cshp\req\intpos.tmp'
    TEFCrediShop.ArqReq = 'C:\tef_cshp\req\intpos.001'
    TEFCrediShop.ArqSTS = 'C:\tef_cshp\resp\intpos.sts'
    TEFCrediShop.ArqResp = 'C:\tef_cshp\resp\intpos.001'
    TEFCrediShop.GPExeName = 'C:\tef_cshp\vpos_tef.exe'
    TEFTicketCar.ArqTemp = 'C:\TCS\TX\INTTCS.tmp'
    TEFTicketCar.ArqReq = 'C:\TCS\TX\INTTCS.001'
    TEFTicketCar.ArqSTS = 'C:\TCS\RX\INTTCS.RET'
    TEFTicketCar.ArqResp = 'C:\TCS\RX\INTTCS.001'
    TEFTicketCar.GPExeName = 'C:\TCS\tcs.exe'
    TEFTicketCar.NumLoja = 0
    TEFTicketCar.NumCaixa = 0
    TEFTicketCar.AtualizaPrecos = False
    TEFConvCard.ArqTemp = 'C:\ger_convenio\tx\crtsol.tmp'
    TEFConvCard.ArqReq = 'C:\ger_convenio\tx\crtsol.001'
    TEFConvCard.ArqSTS = 'C:\ger_convenio\rx\crtsol.ok'
    TEFConvCard.ArqResp = 'C:\ger_convenio\rx\crtsol.001'
    TEFConvCard.GPExeName = 'C:\ger_convcard\convcard.exe'
    OnAguardaResp = ACBrTEFD1AguardaResp
    OnExibeMsg = ACBrTEFD1ExibeMsg
    OnBloqueiaMouseTeclado = ACBrTEFD1BloqueiaMouseTeclado
    OnRestauraFocoAplicacao = ACBrTEFD1RestauraFocoAplicacao
    OnComandaECF = ACBrTEFD1ComandaECF
    OnComandaECFSubtotaliza = ACBrTEFD1ComandaECFSubtotaliza
    OnComandaECFPagamento = ACBrTEFD1ComandaECFPagamento
    OnComandaECFAbreVinculado = ACBrTEFD1ComandaECFAbreVinculado
    OnComandaECFImprimeVia = ACBrTEFD1ComandaECFImprimeVia
    OnInfoECF = ACBrTEFD1InfoECF
    OnAntesFinalizarRequisicao = ACBrTEFD1AntesFinalizarRequisicao
    OnDepoisConfirmarTransacoes = ACBrTEFD1DepoisConfirmarTransacoes
    OnAntesCancelarTransacao = ACBrTEFD1AntesCancelarTransacao
    OnMudaEstadoReq = ACBrTEFD1MudaEstadoReq
    OnMudaEstadoResp = ACBrTEFD1MudaEstadoResp
    Left = 16
    Top = 240
  end
end
