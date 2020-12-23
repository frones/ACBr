object fPrincipal: TfPrincipal
  Left = 0
  Top = 0
  Caption = 'Demo SAT_ACBR FR'
  ClientHeight = 105
  ClientWidth = 297
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 58
    Top = 18
    Width = 164
    Height = 25
    Caption = 'Importar XML e imprimir CFe'
    TabOrder = 0
    OnClick = Button1Click
  end
  object btnSalvarPDF: TButton
    Left = 58
    Top = 49
    Width = 164
    Height = 25
    Caption = 'SalvarPDF'
    TabOrder = 1
    OnClick = btnSalvarPDFClick
  end
  object OpenDialog: TOpenDialog
    Left = 243
    Top = 12
  end
  object frxSAT: TfrxReport
    Version = '5.4.6'
    DataSetName = 'Emitente'
    DotMatrixReport = False
    IniFile = '\Software\Fast Reports'
    PreviewOptions.Buttons = [pbPrint, pbLoad, pbSave, pbExport, pbZoom, pbFind, pbOutline, pbPageSetup, pbTools, pbEdit, pbNavigator, pbExportQuick]
    PreviewOptions.Zoom = 1.000000000000000000
    PrintOptions.Printer = 'Padr'#227'o'
    PrintOptions.PrintOnSheet = 0
    ReportOptions.CreateDate = 44001.494494872700000000
    ReportOptions.LastChange = 44097.611429097200000000
    ScriptLanguage = 'PascalScript'
    ScriptText.Strings = (
      'procedure RodapeTrocoOnBeforePrint(Sender: TfrxComponent);'
      'begin   '
      '    RodapeTroco.Visible := <CalculoImposto."vTroco"> > 0;  '
      'end;'
      ''
      'procedure TituloLogoOnBeforePrint(Sender: TfrxComponent);'
      'begin              '
      
        '  TituloLogo.Visible := <Parametros."LogoCarregado"> <> '#39#39';     ' +
        ' '
      'end;'
      ''
      'procedure CFeTesteOnBeforePrint(Sender: TfrxComponent);'
      'begin'
      
        '  CFeTeste.Visible := <identificacao."tpAmb"> = 2;              ' +
        '                       '
      'end;'
      ''
      'begin    '
      ''
      'end.')
    Left = 15
    Top = 18
    Datasets = <
      item
        DataSetName = 'Identificacao'
      end
      item
        DataSetName = 'Emitente'
      end
      item
        DataSetName = 'Parametros'
      end
      item
        DataSetName = 'DadosProdutos'
      end
      item
        DataSetName = 'InformacoesAdicionais'
      end
      item
        DataSetName = 'CalculoImposto'
      end
      item
        DataSetName = 'FormaPagamento'
      end
      item
        DataSetName = 'DadosEntrega'
      end>
    Variables = <>
    Style = <>
    object Data: TfrxDataPage
      Height = 1000.000000000000000000
      Width = 1000.000000000000000000
    end
    object Page1: TfrxReportPage
      PaperWidth = 80.000000000000000000
      PaperHeight = 290.000000000000000000
      PaperSize = 256
      TopMargin = 0.500000000000000000
      EndlessHeight = True
      LargeDesignHeight = True
      PrintIfEmpty = False
      object TituloLogo: TfrxReportTitle
        FillType = ftBrush
        Height = 64.252010000000000000
        Top = 18.897650000000000000
        Width = 302.362400000000000000
        OnBeforePrint = 'TituloLogoOnBeforePrint'
        PrintChildIfInvisible = True
        Stretched = True
        object ImgLogo: TfrxPictureView
          Align = baCenter
          Left = 94.488250000000000000
          Top = 3.779530000000001000
          Width = 113.385900000000000000
          Height = 52.913420000000000000
          Center = True
          DataField = 'LogoCarregado'
          DataSetName = 'Parametros'
          HightQuality = True
          Transparent = False
          TransparentColor = clWhite
        end
      end
      object PageHeader1: TfrxPageHeader
        FillType = ftBrush
        Height = 91.165430000000000000
        Top = 105.826840000000000000
        Width = 302.362400000000000000
        object Memo9: TfrxMemoView
          Align = baWidth
          ShiftMode = smWhenOverlapped
          Top = 68.488250000000000000
          Width = 302.362400000000000000
          Height = 22.677180000000000000
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          Frame.Width = 0.500000000000000000
          HAlign = haCenter
          Memo.UTF8W = (
            'Extrato N'#186' [Identificacao."nCFe"]'
            'CUPOM FISCAL ELETR'#212'NICO - SAT')
          ParentFont = False
          VAlign = vaCenter
        end
        object Memo2: TfrxMemoView
          Align = baWidth
          ShiftMode = smWhenOverlapped
          Width = 302.362400000000000000
          Height = 66.141751810000000000
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          Frame.Typ = [ftBottom]
          HAlign = haCenter
          LineSpacing = 4.000000000000000000
          Memo.UTF8W = (
            '[Emitente."XFant"]'
            '[Emitente."XNome"]'
            
              '[Emitente."Xlgr"], [Emitente."Nro"] - [Emitente."xBairro"] [Emit' +
              'ente."XMun"] '
            
              'CNPJ: [Emitente."CNPJ"] - IE: [Emitente."IE"] - IM: [Emitente."I' +
              'M"]')
          ParentFont = False
          Formats = <
            item
            end
            item
            end
            item
            end
            item
            end
            item
            end
            item
            end
            item
            end>
        end
      end
      object Rodape: TfrxMasterData
        FillType = ftBrush
        Height = 248.330762360000000000
        Top = 982.677800000000000000
        Width = 302.362400000000000000
        RowCount = 1
        Stretched = True
        object ImgQrCode: TfrxPictureView
          Left = 6.559052680000000000
          Top = 69.354360000000150000
          Width = 151.181102360000000000
          Height = 151.181102360000000000
          Frame.Color = clFuchsia
          KeepAspectRatio = False
          HightQuality = False
          Transparent = False
          TransparentColor = clWhite
        end
        object Memo4: TfrxMemoView
          Left = 162.519790000000000000
          Top = 69.472480000000010000
          Width = 128.504020000000000000
          Height = 151.181200000000000000
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          Frame.Width = 0.500000000000000000
          HAlign = haCenter
          Memo.UTF8W = (
            '[identificacao."CPFConsumidor"]'
            '[identificacao."nCFe"]'
            'N'#186' S'#233'rie SAT. [identificacao."nserieSAT"]'
            '[Identificacao."dhEmi"]')
          ParentFont = False
          VAlign = vaCenter
          Formats = <
            item
            end
            item
            end
            item
            end
            item
            end>
        end
        object Memo1: TfrxMemoView
          Align = baWidth
          Top = 3.779530000000022000
          Width = 302.362400000000000000
          Height = 15.118120000000000000
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          Frame.Width = 0.500000000000000000
          HAlign = haCenter
          Memo.UTF8W = (
            '[Identificacao."Chave"]')
          ParentFont = False
          VAlign = vaCenter
        end
        object bcChave: TfrxBarCodeView
          Align = baWidth
          Left = 111.681200000000000000
          Top = 22.677180000000020000
          Width = 79.000000000000000000
          Height = 41.574830000000000000
          BarType = bcCode128C
          Expression = '<Identificacao."Id">'
          HAlign = haCenter
          Rotation = 0
          ShowText = False
          Text = '12345678'
          WideBarRatio = 2.000000000000000000
          Zoom = 1.000000000000000000
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = []
        end
        object Memo3: TfrxMemoView
          Align = baCenter
          Left = -0.000000000000010186
          Top = 226.362348329999000000
          Width = 302.362400000000000000
          Height = 20.787415000000000000
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          HAlign = haCenter
          Memo.UTF8W = (
            '[Parametros."Sistema"]')
          ParentFont = False
          VAlign = vaCenter
        end
        object Line1: TfrxLineView
          Align = baWidth
          Top = 225.992269999998900000
          Width = 302.362400000000000000
          Color = clWindowFrame
          Frame.ShadowWidth = 2.000000000000000000
          Frame.Typ = [ftTop]
        end
      end
      object DadosPagamentoHeader: TfrxGroupHeader
        FillType = ftBrush
        Height = 109.740220940000000000
        Top = 578.268090000000000000
        Width = 302.362400000000000000
        Condition = 'DadosProdutos."ChaveCFe"'
        ReprintOnNewPage = True
        object memTitDadosPagamento: TfrxMemoView
          Top = 6.338589999999954000
          Width = 185.196970000000000000
          Height = 103.181160940000000000
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          Frame.Width = 0.500000000000000000
          GapX = 3.000000000000000000
          Memo.UTF8W = (
            'Qtde. total de itens'
            ''
            'Valor Produtos'
            'Descontos'
            'Acr'#233'scimos'
            'Total de descontos / acr'#233'scimos sobre item'
            'VALOR A PAGAR'
            ''
            'FORMA DE PAGAMENTO  ')
          ParentFont = False
          WordWrap = False
        end
        object Line3: TfrxLineView
          Align = baWidth
          Top = 3.779530000000136000
          Width = 302.362400000000000000
          Color = clBlack
          Diagonal = True
        end
        object memDadosPagamento: TfrxMemoView
          Left = 185.196970000000000000
          Top = 6.425196850000134000
          Width = 113.385900000000000000
          Height = 103.181160940000000000
          DataSetName = 'Parametros'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          Frame.Width = 0.500000000000000000
          GapX = 3.000000000000000000
          HAlign = haRight
          Memo.UTF8W = (
            '[Parametros."QtdeItens"]'
            ''
            '[CalculoImposto."VProd" #n%2,2f]'
            '- [CalculoImposto."vDescSubtot" #n%2,2f]'
            '+ [CalculoImposto."vAcresSubtot" #n%2,2f]'
            '[CalculoImposto."vDescAcresItens" #n%2,2f]'
            '[<CalculoImposto."vCFe"> #n%2,2f]'
            ''
            'VALOR PAGO')
          ParentFont = False
          WordWrap = False
          Formats = <
            item
            end
            item
            end
            item
            end
            item
            end
            item
            end
            item
            end>
        end
      end
      object DadosDesconto: TfrxDetailData
        FillType = ftBrush
        Height = 22.677165350000000000
        Top = 442.205010000000000000
        Width = 302.362400000000000000
        Filter = '<DadosProdutos."vDesc"> > 0'
        RowCount = 1
        object Memo10: TfrxMemoView
          Left = 113.385900000000000000
          Width = 98.267892280000000000
          Height = 11.338582680000000000
          StretchMode = smMaxHeight
          DisplayFormat.DecimalSeparator = ','
          DisplayFormat.FormatStr = '%2.2f'
          DisplayFormat.Kind = fkNumeric
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = []
          Frame.Width = 0.500000000000000000
          HAlign = haRight
          Memo.UTF8W = (
            'Desconto')
          ParentFont = False
          WordWrap = False
        end
        object Memo15: TfrxMemoView
          Left = 113.385900000000000000
          Top = 11.338590000000010000
          Width = 98.267892280000000000
          Height = 11.338582680000000000
          StretchMode = smMaxHeight
          DisplayFormat.DecimalSeparator = ','
          DisplayFormat.FormatStr = '%2.2f'
          DisplayFormat.Kind = fkNumeric
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = []
          Frame.Width = 0.500000000000000000
          HAlign = haRight
          Memo.UTF8W = (
            'Valor l'#237'quido')
          ParentFont = False
          WordWrap = False
        end
        object Memo21: TfrxMemoView
          Left = 211.653680000000000000
          Width = 86.929302280000000000
          Height = 11.338582680000000000
          StretchMode = smMaxHeight
          DisplayFormat.DecimalSeparator = ','
          DisplayFormat.FormatStr = '%2.2f'
          DisplayFormat.Kind = fkNumeric
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = []
          Frame.Width = 0.500000000000000000
          HAlign = haRight
          Memo.UTF8W = (
            '- [<DadosProdutos."vDesc">]')
          ParentFont = False
          WordWrap = False
        end
        object Memo23: TfrxMemoView
          Left = 211.653680000000000000
          Top = 11.338590000000010000
          Width = 86.929302280000000000
          Height = 11.338582680000000000
          StretchMode = smMaxHeight
          DisplayFormat.DecimalSeparator = ','
          DisplayFormat.FormatStr = '%2.2f'
          DisplayFormat.Kind = fkNumeric
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = []
          Frame.Width = 0.500000000000000000
          HAlign = haRight
          Memo.UTF8W = (
            '[<DadosProdutos."Valorliquido">]')
          ParentFont = False
          WordWrap = False
        end
      end
      object GroupHeader1: TfrxGroupHeader
        FillType = ftBrush
        Height = 27.590560940000000000
        Top = 343.937230000000000000
        Width = 302.362400000000000000
        Condition = 'DadosProdutos."ChaveCFe"'
        object Memo6: TfrxMemoView
          Left = 71.811033390000000000
          Top = 14.472448269999970000
          Width = 34.015755350000000000
          Height = 11.338582680000000000
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          Frame.Style = fsDot
          Frame.Typ = [ftBottom]
          GapY = 2.000000000000000000
          Memo.UTF8W = (
            'UN')
          ParentFont = False
          WordWrap = False
          VAlign = vaCenter
        end
        object Memo7: TfrxMemoView
          Left = 105.826788740000000000
          Top = 14.472448269999970000
          Width = 52.913358980000000000
          Height = 11.338582680000000000
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          Frame.Style = fsDot
          Frame.Typ = [ftBottom]
          GapY = 2.000000000000000000
          HAlign = haRight
          Memo.UTF8W = (
            'VL.UNIT')
          ParentFont = False
          WordWrap = False
          VAlign = vaCenter
        end
        object Memo8: TfrxMemoView
          Left = 26.456712450000000000
          Top = 1.354328269999996000
          Width = 45.354320940000000000
          Height = 13.228346460000000000
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          Frame.Style = fsDot
          Frame.Typ = [ftTop]
          GapY = 2.000000000000000000
          Memo.UTF8W = (
            'C'#211'DIGO')
          ParentFont = False
          WordWrap = False
          VAlign = vaCenter
        end
        object Memo11: TfrxMemoView
          Left = 71.811016300000000000
          Top = 1.354328269999996000
          Width = 226.771839060000000000
          Height = 13.228346460000000000
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          Frame.Style = fsDot
          Frame.Typ = [ftTop]
          GapY = 2.000000000000000000
          Memo.UTF8W = (
            'DESCRI'#199#195'O')
          ParentFont = False
          WordWrap = False
          VAlign = vaCenter
        end
        object Memo12: TfrxMemoView
          Left = 3.779530000000000000
          Top = 14.472448269999970000
          Width = 68.031503390000000000
          Height = 11.338582680000000000
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          Frame.Style = fsDot
          Frame.Typ = [ftBottom]
          GapY = 2.000000000000000000
          HAlign = haRight
          Memo.UTF8W = (
            'QTD')
          ParentFont = False
          WordWrap = False
          VAlign = vaCenter
        end
        object Memo13: TfrxMemoView
          Left = 211.653567720000000000
          Top = 14.472448269999970000
          Width = 86.929302280000000000
          Height = 11.338572910000000000
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          Frame.Style = fsDot
          Frame.Typ = [ftBottom]
          GapY = 2.000000000000000000
          HAlign = haRight
          Memo.UTF8W = (
            'VL.TOTAL')
          ParentFont = False
          WordWrap = False
          VAlign = vaCenter
        end
        object Memo5: TfrxMemoView
          Left = 3.779569060000000000
          Top = 1.133858270000019000
          Width = 22.677140940000000000
          Height = 13.606299210000000000
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          Frame.Style = fsDot
          Frame.Typ = [ftTop]
          GapY = 2.000000000000000000
          Memo.UTF8W = (
            '#')
          ParentFont = False
          WordWrap = False
          VAlign = vaCenter
        end
        object Memo24: TfrxMemoView
          Left = 158.740260000000000000
          Top = 14.362204720000020000
          Width = 52.913532280000000000
          Height = 11.338572910000000000
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          Frame.Style = fsDot
          Frame.Typ = [ftBottom]
          GapY = 2.000000000000000000
          HAlign = haRight
          Memo.UTF8W = (
            '(VL TR R$)*')
          ParentFont = False
          WordWrap = False
          VAlign = vaCenter
        end
      end
      object DadosProdutos: TfrxMasterData
        FillType = ftBrush
        Height = 26.456702680000000000
        Top = 393.071120000000000000
        Width = 302.362400000000000000
        DataSetName = 'DadosProdutos'
        RowCount = 0
        Stretched = True
        object Memo140: TfrxMemoView
          ShiftMode = smDontShift
          Left = 71.810957720000000000
          Width = 226.771912280000000000
          Height = 11.338582680000000000
          DisplayFormat.DecimalSeparator = ','
          DisplayFormat.FormatStr = '%2.2f'
          DisplayFormat.Kind = fkNumeric
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = []
          Frame.Width = 0.500000000000000000
          Memo.UTF8W = (
            '[DadosProdutos."xProd"]')
          ParentFont = False
        end
        object Memo16: TfrxMemoView
          Left = 105.826840000000000000
          Top = 11.338590000000010000
          Width = 52.913532280000000000
          Height = 11.338582680000000000
          StretchMode = smMaxHeight
          DisplayFormat.DecimalSeparator = ','
          DisplayFormat.FormatStr = '%2.2f'
          DisplayFormat.Kind = fkNumeric
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = []
          Frame.Width = 0.500000000000000000
          HAlign = haRight
          Memo.UTF8W = (
            '[DadosProdutos."VUnCom"]')
          ParentFont = False
          WordWrap = False
        end
        object Memo17: TfrxMemoView
          Left = 71.811070000000000000
          Top = 11.338590000000010000
          Width = 34.015882280000000000
          Height = 11.338582680000000000
          StretchMode = smMaxHeight
          DisplayFormat.DecimalSeparator = ','
          DisplayFormat.FormatStr = '%2.2f'
          DisplayFormat.Kind = fkNumeric
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = []
          Frame.Width = 0.500000000000000000
          Memo.UTF8W = (
            '[DadosProdutos."UCom"]')
          ParentFont = False
          WordWrap = False
        end
        object Memo18: TfrxMemoView
          Left = 3.779530000000000000
          Top = 11.338590000000010000
          Width = 68.031652280000000000
          Height = 11.338582680000000000
          StretchMode = smMaxHeight
          DisplayFormat.DecimalSeparator = ','
          DisplayFormat.FormatStr = '%2.2f'
          DisplayFormat.Kind = fkNumeric
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = []
          Frame.Width = 0.500000000000000000
          HAlign = haRight
          Memo.UTF8W = (
            '[DadosProdutos."qCom"]')
          ParentFont = False
          WordWrap = False
        end
        object Memo19: TfrxMemoView
          Left = 26.456710000000000000
          Width = 45.354472280000000000
          Height = 11.338582680000000000
          StretchMode = smMaxHeight
          DisplayFormat.DecimalSeparator = ','
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = []
          Frame.Width = 0.500000000000000000
          Memo.UTF8W = (
            '[DadosProdutos."cProd"]')
          ParentFont = False
          WordWrap = False
        end
        object Memo20: TfrxMemoView
          Left = 3.779530000000000000
          Width = 22.677292280000000000
          Height = 11.338582680000000000
          StretchMode = smMaxHeight
          DisplayFormat.FormatStr = '#000'
          DisplayFormat.Kind = fkNumeric
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = []
          Frame.Width = 0.500000000000000000
          Memo.UTF8W = (
            '[DadosProdutos."nItem"]')
          ParentFont = False
          WordWrap = False
        end
        object Memo14: TfrxMemoView
          Left = 211.653680000000000000
          Top = 11.338590000000010000
          Width = 86.929302280000000000
          Height = 11.338582680000000000
          StretchMode = smMaxHeight
          DisplayFormat.DecimalSeparator = ','
          DisplayFormat.FormatStr = '%2.2f'
          DisplayFormat.Kind = fkNumeric
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = []
          Frame.Width = 0.500000000000000000
          HAlign = haRight
          Memo.UTF8W = (
            '[DadosProdutos."VProd"]')
          ParentFont = False
          WordWrap = False
        end
        object Memo25: TfrxMemoView
          Left = 158.740260000000000000
          Top = 11.338590000000010000
          Width = 52.913532280000000000
          Height = 11.338582680000000000
          StretchMode = smMaxHeight
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = []
          Frame.Width = 0.500000000000000000
          HAlign = haRight
          Memo.UTF8W = (
            '[DadosProdutos."vTR"]')
          ParentFont = False
          WordWrap = False
        end
      end
      object DadosObsAdicionais: TfrxMasterData
        FillType = ftBrush
        Height = 26.456710000000000000
        Top = 933.543910000000000000
        Width = 302.362400000000000000
        Filter = '<InformacoesAdicionais."infAdic"> <> '#39#39
        RowCount = 1
        Stretched = True
        object Memo22: TfrxMemoView
          Align = baWidth
          Width = 302.362400000000000000
          Height = 26.456710000000000000
          StretchMode = smActualHeight
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          Frame.Typ = [ftTop]
          GapY = 2.000000000000000000
          Memo.UTF8W = (
            'OBSERVA'#199#212'ES DO CONTRIBUINTE'
            '[InformacoesAdicionais."infAdic"]')
          ParentFont = False
        end
      end
      object DadosPagamento: TfrxMasterData
        FillType = ftBrush
        Height = 14.740159920000000000
        Top = 710.551640000000000000
        Width = 302.362400000000000000
        DataSetName = 'FormaPagamento'
        RowCount = 0
        Stretched = True
        object memTipoPagamento: TfrxMemoView
          Top = 2.000000000000000000
          Width = 128.504020000000000000
          Height = 12.472440940000000000
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          Frame.Width = 0.500000000000000000
          GapX = 3.000000000000000000
          Memo.UTF8W = (
            '[FormaPagamento."tPag"]')
          ParentFont = False
          WordWrap = False
          VAlign = vaCenter
        end
        object memValorPagamento: TfrxMemoView
          Left = 128.504020000000000000
          Top = 2.110233780000044000
          Width = 170.078850000000000000
          Height = 12.472440940000000000
          DisplayFormat.DecimalSeparator = ','
          DisplayFormat.FormatStr = '%2.2n'
          DisplayFormat.Kind = fkNumeric
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          Frame.Width = 0.500000000000000000
          GapX = 3.000000000000000000
          HAlign = haRight
          Memo.UTF8W = (
            '[FormaPagamento."vMP"]')
          ParentFont = False
          WordWrap = False
          VAlign = vaCenter
        end
      end
      object ValorTributos: TfrxMasterData
        FillType = ftBrush
        Height = 32.015752910000000000
        Top = 876.850960000000000000
        Width = 302.362400000000000000
        OnBeforePrint = 'ValorTributosOnBeforePrint'
        RowCount = 1
        object Memo26: TfrxMemoView
          Align = baWidth
          Top = 3.779530000000022000
          Width = 188.976524410000000000
          Height = 26.456690470000000000
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          Frame.Typ = [ftTop]
          GapY = 2.000000000000000000
          Memo.UTF8W = (
            
              'Valor aproximado dos Tributos deste Cupom(Conforme Lei Fed. 12.7' +
              '41/2012)')
          ParentFont = False
        end
        object Memo27: TfrxMemoView
          Left = 188.976524410000000000
          Top = 3.779530000000022000
          Width = 109.606345590000000000
          Height = 26.456690470000000000
          DisplayFormat.FormatStr = '%2.2n'
          DisplayFormat.Kind = fkNumeric
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          Frame.Typ = [ftTop]
          GapY = 2.000000000000000000
          HAlign = haRight
          Memo.UTF8W = (
            '[CalculoImposto."vCFeLei12741"]')
          ParentFont = False
          WordWrap = False
        end
      end
      object DadosObsFisco: TfrxMasterData
        FillType = ftBrush
        Height = 26.456710000000000000
        Top = 786.142240000000000000
        Width = 302.362400000000000000
        Filter = '<InformacoesAdicionais."ObsFisco"> <> '#39#39
        RowCount = 1
        Stretched = True
        object Memo30: TfrxMemoView
          Align = baWidth
          Width = 302.362400000000000000
          Height = 26.456710000000000000
          StretchMode = smActualHeight
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          Frame.Typ = [ftTop]
          GapY = 2.000000000000000000
          Memo.UTF8W = (
            'OBSERVA'#199#212'ES DO FISCO'
            '[InformacoesAdicionais."ObsFisco"]')
          ParentFont = False
        end
      end
      object DadosAcrescimo: TfrxDetailData
        FillType = ftBrush
        Height = 22.677180000000000000
        Top = 487.559370000000000000
        Width = 302.362400000000000000
        Filter = '<DadosProdutos."VOutro"> > 0'
        RowCount = 1
        object Memo33: TfrxMemoView
          Left = 211.653680000000000000
          Width = 86.929302280000000000
          Height = 11.338582680000000000
          StretchMode = smMaxHeight
          DisplayFormat.DecimalSeparator = ','
          DisplayFormat.FormatStr = '%2.2f'
          DisplayFormat.Kind = fkNumeric
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = []
          Frame.Width = 0.500000000000000000
          HAlign = haRight
          Memo.UTF8W = (
            '+ [<DadosProdutos."VOutro">]')
          ParentFont = False
          WordWrap = False
        end
        object Memo34: TfrxMemoView
          Left = 113.385900000000000000
          Width = 98.267892280000000000
          Height = 11.338582680000000000
          StretchMode = smMaxHeight
          DisplayFormat.DecimalSeparator = ','
          DisplayFormat.FormatStr = '%2.2f'
          DisplayFormat.Kind = fkNumeric
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = []
          Frame.Width = 0.500000000000000000
          HAlign = haRight
          Memo.UTF8W = (
            'Acr'#233'scimo')
          ParentFont = False
          WordWrap = False
        end
        object Memo35: TfrxMemoView
          Left = 211.653680000000000000
          Top = 11.338590000000010000
          Width = 86.929302280000000000
          Height = 11.338582680000000000
          StretchMode = smMaxHeight
          DisplayFormat.DecimalSeparator = ','
          DisplayFormat.FormatStr = '%2.2f'
          DisplayFormat.Kind = fkNumeric
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = []
          Frame.Width = 0.500000000000000000
          HAlign = haRight
          Memo.UTF8W = (
            '[<DadosProdutos."ValorAcrescimos">]')
          ParentFont = False
          WordWrap = False
        end
        object Memo36: TfrxMemoView
          Left = 113.385900000000000000
          Top = 11.338590000000010000
          Width = 98.267892280000000000
          Height = 11.338582680000000000
          StretchMode = smMaxHeight
          DisplayFormat.DecimalSeparator = ','
          DisplayFormat.FormatStr = '%2.2f'
          DisplayFormat.Kind = fkNumeric
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = []
          Frame.Width = 0.500000000000000000
          HAlign = haRight
          Memo.UTF8W = (
            'Valor l'#237'quido')
          ParentFont = False
          WordWrap = False
        end
      end
      object DadosEntrega: TfrxMasterData
        FillType = ftBrush
        Height = 18.897630470000000000
        Top = 835.276130000000000000
        Width = 302.362400000000000000
        Filter = '<DadosEntrega."EnderecoEntrega"> <> '#39#39
        RowCount = 1
        Stretched = True
        object Memo31: TfrxMemoView
          Align = baWidth
          Top = 3.779530000000022000
          Width = 302.362400000000000000
          Height = 15.118100470000000000
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          Frame.Typ = [ftTop]
          GapY = 2.000000000000000000
          Memo.UTF8W = (
            'Endere'#231'o de Entrega: [DadosEntrega."EnderecoEntrega"]')
          ParentFont = False
        end
      end
      object RodapeTroco: TfrxFooter
        FillType = ftBrush
        Height = 15.118120000000000000
        Top = 748.346940000000000000
        Width = 302.362400000000000000
        OnBeforePrint = 'RodapeTrocoOnBeforePrint'
        object Memo38: TfrxMemoView
          Left = 128.504020000000000000
          Width = 170.078850000000000000
          Height = 11.338590000000000000
          DataSetName = 'CalculoImposto'
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          Frame.Width = 0.500000000000000000
          HAlign = haRight
          Memo.UTF8W = (
            '[<CalculoImposto."vTroco"> #n%2,2f]')
          ParentFont = False
          WordWrap = False
          VAlign = vaCenter
        end
        object Memo39: TfrxMemoView
          Width = 128.504020000000000000
          Height = 11.338590000000000000
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          Frame.Width = 0.500000000000000000
          Memo.UTF8W = (
            'Troco R$')
          ParentFont = False
          VAlign = vaCenter
        end
      end
      object CFeTeste: TfrxMasterData
        FillType = ftBrush
        Height = 64.252010000000000000
        Top = 257.008040000000000000
        Width = 302.362400000000000000
        OnBeforePrint = 'CFeTesteOnBeforePrint'
        RowCount = 1
        Stretched = True
        object Memo28: TfrxMemoView
          Align = baWidth
          Width = 302.362400000000000000
          Height = 15.118120000000000000
          StretchMode = smActualHeight
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          GapY = 2.000000000000000000
          HAlign = haCenter
          Memo.UTF8W = (
            '=   TESTE   =')
          ParentFont = False
        end
        object Memo29: TfrxMemoView
          Align = baWidth
          Top = 15.118119999999980000
          Width = 302.362400000000000000
          Height = 15.118120000000000000
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          GapY = 2.000000000000000000
          HAlign = haCenter
          Memo.UTF8W = (
            '>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>')
          ParentFont = False
          WordWrap = False
          VAlign = vaCenter
        end
        object Memo32: TfrxMemoView
          Align = baWidth
          Top = 30.236240000000010000
          Width = 302.362400000000000000
          Height = 15.118120000000000000
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          GapY = 2.000000000000000000
          HAlign = haCenter
          Memo.UTF8W = (
            '>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>')
          ParentFont = False
          WordWrap = False
        end
        object Memo37: TfrxMemoView
          Align = baWidth
          Top = 45.354359999999990000
          Width = 302.362400000000000000
          Height = 15.118120000000000000
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          GapY = 2.000000000000000000
          HAlign = haCenter
          Memo.UTF8W = (
            '>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>')
          ParentFont = False
          WordWrap = False
        end
      end
      object DadosISSQN: TfrxDetailData
        FillType = ftBrush
        Height = 22.677180000000000000
        Top = 532.913730000000000000
        Width = 302.362400000000000000
        Filter = '<DadosProdutos."vDeducISSQN"> > 0'
        RowCount = 1
        object Memo40: TfrxMemoView
          Left = 211.653680000000000000
          Width = 86.929302280000000000
          Height = 22.677172680000000000
          StretchMode = smMaxHeight
          DisplayFormat.DecimalSeparator = ','
          DisplayFormat.FormatStr = '%2.2f'
          DisplayFormat.Kind = fkNumeric
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = []
          Frame.Width = 0.500000000000000000
          HAlign = haRight
          Memo.UTF8W = (
            '- [<DadosProdutos."vDeducISSQN">]'
            '[<DadosProdutos."vBC">]')
          ParentFont = False
          WordWrap = False
        end
        object Memo41: TfrxMemoView
          Left = 113.385900000000000000
          Width = 98.267892280000000000
          Height = 22.677172680000000000
          StretchMode = smMaxHeight
          DisplayFormat.DecimalSeparator = ','
          DisplayFormat.FormatStr = '%2.2f'
          DisplayFormat.Kind = fkNumeric
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -8
          Font.Name = 'Arial'
          Font.Style = []
          Frame.Width = 0.500000000000000000
          HAlign = haRight
          Memo.UTF8W = (
            'Dedu'#231#227'o para ISSQN'
            'Base de c'#225'lculo ISSQN')
          ParentFont = False
          WordWrap = False
        end
      end
    end
  end
end
