object frmACBrDI: TfrmACBrDI
  Left = 0
  Top = 0
  Caption = 'frmACBrDI'
  ClientHeight = 460
  ClientWidth = 551
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  TextHeight = 15
  object PanelTop: TPanel
    Left = 0
    Top = 0
    Width = 551
    Height = 49
    Align = alTop
    BevelEdges = []
    BevelKind = bkTile
    BevelOuter = bvNone
    Color = 15527148
    ParentBackground = False
    ShowCaption = False
    TabOrder = 0
    ExplicitWidth = 547
    object Shape1: TShape
      Left = 0
      Top = 48
      Width = 551
      Height = 1
      Align = alBottom
      Pen.Color = clSilver
      ExplicitTop = -7
      ExplicitWidth = 516
    end
    object BtnLerMultiplosXML: TBitBtn
      Left = 179
      Top = 13
      Width = 145
      Height = 25
      Caption = 'Ler M'#250'ltiplos XML'
      TabOrder = 0
      OnClick = BtnLerMultiplosXMLClick
    end
    object BtnLerXMLIndividualDI: TBitBtn
      Left = 21
      Top = 13
      Width = 132
      Height = 25
      Caption = 'Ler XML Individual'
      TabOrder = 1
      OnClick = BtnLerXMLIndividualDIClick
    end
    object BtnLimpar: TBitBtn
      Left = 352
      Top = 13
      Width = 75
      Height = 25
      Caption = 'Limpar'
      TabOrder = 2
      OnClick = BtnLimparClick
    end
  end
  object PageControlPrincipal: TPageControl
    Left = 0
    Top = 49
    Width = 551
    Height = 411
    ActivePage = TabXMLDI
    Align = alClient
    TabOrder = 1
    ExplicitWidth = 547
    ExplicitHeight = 410
    object TabXMLDI: TTabSheet
      Caption = 'XML da DI'
      object MemoXML: TMemo
        Left = 0
        Top = 0
        Width = 543
        Height = 381
        Align = alClient
        Lines.Strings = (
          'MemoXML')
        ScrollBars = ssVertical
        TabOrder = 0
        ExplicitWidth = 539
        ExplicitHeight = 380
      end
    end
    object TabLogs: TTabSheet
      Caption = 'Logs/Resultados'
      ImageIndex = 1
      object MemoLogs: TMemo
        Left = 0
        Top = 0
        Width = 543
        Height = 381
        Align = alClient
        Lines.Strings = (
          'MemoLogs')
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '*.xml'
    Filter = 'Arquivos xml| *.xml'
    Title = 'Selecione o XML da DI'
    Left = 480
    Top = 88
  end
end
