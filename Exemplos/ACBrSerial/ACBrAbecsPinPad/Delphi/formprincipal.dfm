object frMain: TfrMain
  Left = 522
  Top = 227
  Width = 950
  Height = 624
  Caption = 'Teste ABECS PinPad'
  Color = clBtnFace
  Constraints.MinWidth = 640
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  Visible = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object Splitter2: TSplitter
    Left = 0
    Top = 332
    Width = 934
    Height = 8
    Cursor = crVSplit
    Align = alTop
  end
  object pLogs: TPanel
    Left = 0
    Top = 340
    Width = 934
    Height = 245
    Align = alClient
    TabOrder = 0
    object sbCleanMemoLog: TSpeedButton
      Left = 913
      Top = 58
      Width = 23
      Height = 158
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000C30E0000C30E00000000000000000000FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        E4E4E4AEAEAEAAAAAAAAAAAAAAAAAAB2B2B2F8F8F8FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFF0F0F037373701010100000000000000000002
        0202929292FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE3E3E3
        1C1C1C000000000000000000000000000000555555E3E3E31C1C1C0000005555
        55FFFFFFFFFFFFFFFFFFFFFFFFE3E3E31C1C1C00000000000000000000000000
        0000555555F6F6F6B4B4B4AAAAAAC7C7C7FFFFFFFFFFFFFFFFFFFFFFFFE3E3E3
        1C1C1C000000000000000000000000000000555555F6F6F6B4B4B4AAAAAAAAAA
        AAB4B4B4F6F6F6FFFFFFFFFFFFE3E3E31C1C1C00000000000000000000000000
        0000555555E3E3E31C1C1C0000000000001C1C1CE3E3E3FFFFFFFFFFFFE3E3E3
        1C1C1C000000000000000000000000000000555555FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFE3E3E31C1C1C00000000000000000000000000
        0000555555EDEDED6868685555555555555555558E8E8EFFFFFFFFFFFFEDEDED
        6868685555555555555555555555555555558E8E8EEDEDED6868685555555555
        555555558E8E8EFFFFFFFFFFFF8E8E8E55555555555555555555555555555555
        5555555555C7C7C7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8E8E8E
        555555313131000000000000050505494949555555C7C7C7FFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFEFEFAAAAAAAAAAAABABABAFB
        FBFBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
      OnClick = sbCleanMemoLogClick
    end
    object mLog: TMemo
      Left = 1
      Top = 58
      Width = 912
      Height = 158
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
      WordWrap = False
    end
    object sbResponse: TStatusBar
      Left = 1
      Top = 216
      Width = 932
      Height = 28
      Panels = <
        item
          Text = 'STAT:'
          Width = 120
        end
        item
          Text = 'ERROR:'
          Width = 40
        end>
    end
    object pCancelar: TPanel
      Left = 1
      Top = 1
      Width = 932
      Height = 57
      Align = alTop
      TabOrder = 2
      Visible = False
      DesignSize = (
        932
        57)
      object btCancel: TButton
        Left = 0
        Top = 1
        Width = 933
        Height = 55
        Anchors = []
        Caption = 'Cancel Operation'
        TabOrder = 0
        OnClick = btCancelClick
      end
    end
  end
  object pCommands: TPanel
    Left = 0
    Top = 0
    Width = 934
    Height = 332
    Align = alTop
    BevelOuter = bvNone
    Constraints.MinHeight = 264
    TabOrder = 1
    object pgcCommands: TPageControl
      Left = 0
      Top = 0
      Width = 934
      Height = 332
      ActivePage = tsClose
      Align = alClient
      Images = ImageList1
      TabOrder = 0
      object tsConfig: TTabSheet
        Caption = 'Config'
        DesignSize = (
          926
          302)
        object gbConfig: TGroupBox
          Left = 0
          Top = 0
          Width = 926
          Height = 70
          Align = alTop
          Caption = 'Serial Port'
          TabOrder = 0
          DesignSize = (
            926
            70)
          object btSerial: TSpeedButton
            Left = 545
            Top = 27
            Width = 25
            Height = 22
            Anchors = [akTop, akRight]
            Glyph.Data = {
              36030000424D3603000000000000360000002800000010000000100000000100
              18000000000000030000C30E0000C30E00000000000000000000FFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8E8E8E55555555
              55555555555555558E8E8EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFF555555000000000000000000000000555555FFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7F7F73C3C3C00000000
              0000000000000000404040F8F8F8FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFB5B5B5010101000000000000000000000000020202BBBBBBFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDFDFD3A3A3A00000000000000
              0000000000000000000000404040FEFEFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFB8B8B8000000000000000000000000000000000000000000010101BFBF
              BFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF5C5C5C00000000000000000000
              00000000000000000000000000005F5F5FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFF5555550000000000000000000000000000000000000000000000005555
              55FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF55555500000000000000000000
              0000000000000000000000000000555555FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFF5555550000000000000000000000000000000000000000000000005555
              55FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC7C7C700000071717171717172
              7272727272717171717171000000C7C7C7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFF000000AAAAAAC7C7C7C7C7C7C7C7C7C7C7C7AAAAAA000000FFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF17171738383855555555
              5555555555555555383838181818FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFB1B1B15B5B5B555555555555555555555555606060BEBEBEFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
            OnClick = btSerialClick
          end
          object btSearchSerialPorts: TSpeedButton
            Left = 576
            Top = 27
            Width = 25
            Height = 22
            Anchors = [akTop, akRight]
            Glyph.Data = {
              36030000424D3603000000000000360000002800000010000000100000000100
              18000000000000030000C30E0000C30E00000000000000000000FFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFEFE9A9A
              9AEBEBEBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFEFEFE7C7C7C090909ABABABFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF9F9F97777770808089494
              94FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFAFAFAB4B4B46969695757577E
              7E7ED9D9D98989890C0C0C8E8E8EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              F7F7F76767670B0B0B3F3F3F4E4E4E222222161616303030999999FCFCFCFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF929292060606B3B3B3F8F8F8FDFDFDEC
              ECEC5E5E5E1A1A1ADDDDDDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF5F5F5
              3030306A6A6AFFFFFFFFFFFFFFFFFFFFFFFFEDEDED212121888888FFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFE5E5E51D1D1DA2A2A2FFFFFFFFFFFFFFFFFFFF
              FFFFFBFBFB474747656565FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEEEEEE
              272727898989FFFFFFFFFFFFFFFFFFFFFFFFF2F2F22B2B2B7A7A7AFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF6B6B6B1D1D1DE6E6E6FFFFFFFFFFFFFF
              FFFF9898980C0C0CC4C4C4FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              E8E8E83434341515157676769797975E5E5E0808087C7C7CFDFDFDFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE9E9E97272722E2E2E2525253C
              3C3CA5A5A5FBFBFBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFF4F4F4ECECECFAFAFAFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
            OnClick = btSearchSerialPortsClick
          end
          object cbxPort: TComboBox
            Left = 19
            Top = 27
            Width = 517
            Height = 23
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 15
            TabOrder = 0
          end
        end
        object pConfigLogMsg: TPanel
          Left = 0
          Top = 70
          Width = 926
          Height = 77
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 1
          object gbConfig1: TGroupBox
            Left = 0
            Top = 0
            Width = 671
            Height = 77
            Align = alClient
            Caption = 'Log'
            TabOrder = 0
            DesignSize = (
              671
              77)
            object Label13: TLabel
              Left = 19
              Top = 21
              Width = 18
              Height = 15
              Alignment = taRightJustify
              Caption = 'File'
              Color = clBtnFace
              ParentColor = False
            end
            object sbShowLogFile: TSpeedButton
              Left = 547
              Top = 40
              Width = 25
              Height = 22
              Anchors = [akTop, akRight]
              Caption = '...'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -12
              Font.Name = 'Segoe UI'
              Font.Style = []
              ParentFont = False
              OnClick = sbShowLogFileClick
            end
            object Label14: TLabel
              Left = 595
              Top = 21
              Width = 50
              Height = 15
              Alignment = taRightJustify
              Anchors = [akTop, akRight]
              Caption = 'Log.Level'
              Color = clBtnFace
              ParentColor = False
            end
            object edLogFile: TEdit
              Left = 19
              Top = 40
              Width = 518
              Height = 23
              Cursor = crIBeam
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 0
            end
            object seLogLevel: TSpinEdit
              Left = 595
              Top = 40
              Width = 50
              Height = 24
              Anchors = [akTop, akRight]
              MaxValue = 9
              MinValue = 0
              TabOrder = 1
              Value = 2
            end
          end
          object GroupBox2: TGroupBox
            Left = 671
            Top = 0
            Width = 255
            Height = 77
            Align = alRight
            Caption = 'Msg Align'
            TabOrder = 1
            object cbxMsgAlign: TComboBox
              Left = 16
              Top = 40
              Width = 110
              Height = 23
              AutoDropDown = True
              Style = csDropDownList
              ItemHeight = 15
              TabOrder = 0
            end
            object cbMsgWordWrap: TCheckBox
              Left = 144
              Top = 40
              Width = 89
              Height = 19
              Caption = 'WordWrap'
              TabOrder = 1
            end
          end
        end
        object btSaveParams: TBitBtn
          Left = 26
          Top = 166
          Width = 136
          Height = 28
          Caption = 'Save Params'
          TabOrder = 2
          OnClick = btSaveParamsClick
          Glyph.Data = {
            36030000424D3603000000000000360000002800000010000000100000000100
            18000000000000030000C30E0000C30E00000000000000000000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCFCFCE6E6E6E3E3E3E3E3E3E3E3E3E3
            E3E3E3E3E3E3E3E3E3E3E3E3E3E3E4E4E4FAFAFAFFFFFFFFFFFFFFFFFFF9F9F9
            6A6A6A1F1F1F1C1C1C1C1C1C1C1C1C1C1C1C1C1C1C1C1C1C1C1C1C1C1C1C1D1D
            1D747474FEFEFEFFFFFFFFFFFFE4E4E41D1D1D00000000000000000021212193
            93938E8E8E1B1B1B000000000000000000212121E7E7E7FFFFFFFFFFFFE3E3E3
            1C1C1C000000000000090909C4C4C4FFFFFFFFFFFFBABABA0505050000000000
            001C1C1CE3E3E3FFFFFFFFFFFFE3E3E31C1C1C000000000000161616DDDDDDFF
            FFFFFFFFFFD6D6D60F0F0F0000000000001C1C1CE3E3E3FFFFFFFFFFFFE3E3E3
            1C1C1C0000000000000202028A8A8AFCFCFCFBFBFB7F7F7F0101010000000000
            001C1C1CE3E3E3FFFFFFFFFFFFE3E3E31C1C1C00000000000000000006060633
            33333030300505050000000000000000001C1C1CE3E3E3FFFFFFFFFFFFE3E3E3
            1C1C1C0000000000000000000000000000000000000000000000000000000000
            001C1C1CE3E3E3FFFFFFFFFFFFE3E3E31C1C1C1313131C1C1C1C1C1C1C1C1C1C
            1C1C1C1C1C1919190303030000000000001C1C1CE3E3E3FFFFFFFFFFFFE3E3E3
            1C1C1C979797E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3CACACA1919190000000000
            001C1C1CE3E3E3FFFFFFFFFFFFE3E3E31C1C1CAAAAAAFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFE3E3E31C1C1C000000000000494949F4F4F4FFFFFFFFFFFFE6E6E6
            1F1F1F727272AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA9797971313130000004747
            47EFEFEFFFFFFFFFFFFFFFFFFFFDFDFD7272721F1F1F1C1C1C1C1C1C1C1C1C1C
            1C1C1C1C1C1C1C1C1C1C1C494949EFEFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FBFBFBE6E6E6E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3E3F4F4F4FFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
        end
        object btReadParms: TBitBtn
          Left = 172
          Top = 166
          Width = 136
          Height = 28
          Caption = 'Read Params'
          TabOrder = 3
          OnClick = btReadParmsClick
          Glyph.Data = {
            36030000424D3603000000000000360000002800000010000000100000000100
            18000000000000030000C30E0000C30E00000000000000000000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFCFCFCAEAEAE909090FCFCFCFFFFFFFFFFFFFFFFFFFF
            FFFFEBEBEB6B6B6B5555555555555555555555558E8E8EFFFFFFFAFAFA4E4E4E
            0D0D0DC7C7C7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB7B7B71515150000000000
            00000000555555FFFFFFFFFFFFC5C5C50E0E0E262626DEDEDEFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFACACAC070707000000000000555555FFFFFFFFFFFFFFFFFF
            A0A0A00D0D0D111111808080D1D1D1E6E6E6DEDEDE9B9B9B2626260202020B0B
            0B000000555555FFFFFFFFFFFFFFFFFFFFFFFFB7B7B72F2F2F03030301010109
            09090404040000001B1B1B878787BDBDBD1B1B1B555555FFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFEFEFEFB5B5B57B7B7B6464646D6D6DA1A1A1E1E1E1FFFFFFFFFF
            FFC0C0C0707070FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFEFEFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
        end
        object btActivate: TBitBtn
          Left = 771
          Top = 173
          Width = 109
          Height = 45
          Anchors = [akTop, akRight]
          Caption = 'Activate'
          TabOrder = 4
          OnClick = btActivateClick
          Glyph.Data = {
            36030000424D3603000000000000360000002800000010000000100000000100
            18000000000000030000C30E0000C30E00000000000000000000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFAFAFAC6C6C68080805C
            5C5C5D5D5D848484C9C9C9FCFCFCFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFE7E7E75353530909090000000000000000000000000B0B0B5B5B5BEBEB
            EBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE7E7E731313100000000000000000000
            0000000000000000000000000000393939EDEDEDFFFFFFFFFFFFFFFFFFFAFAFA
            5353530000000000000000002222220606060000000000000000000000000000
            005F5F5FFCFCFCFFFFFFFFFFFFC6C6C6090909000000000000484848E7E7E787
            87870303030000000000000000000000000D0D0DCFCFCFFFFFFFFFFFFF808080
            000000000000464646E8E8E8C1C1C1F3F3F38383830404040000000000000000
            000000008F8F8FFFFFFFFFFFFF5C5C5C0000003D3D3DEEEEEE9D9D9D0D0D0D61
            6161F8F8F8838383030303000000000000000000696969FFFFFFFFFFFF5D5D5D
            000000343434909090050505000000000000616161F3F3F38787870606060000
            000000006B6B6BFFFFFFFFFFFF84848400000000000000000000000000000000
            0000020202646464F1F1F1878787030303000000929292FFFFFFFFFFFFC9C9C9
            0B0B0B000000000000000000000000000000000000020202646464F3F3F35D5D
            5D0F0F0FD3D3D3FFFFFFFFFFFFFCFCFC5B5B5B00000000000000000000000000
            00000000000000000202024D4D4D101010676767FDFDFDFFFFFFFFFFFFFFFFFF
            EBEBEB3939390000000000000000000000000000000000000000000000004242
            42F1F1F1FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEDEDED5F5F5F0D0D0D00000000
            00000000000000000F0F0F676767F1F1F1FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFCFCFCCFCFCF8F8F8F6A6A6A6B6B6B929292D3D3D3FDFDFDFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
        end
      end
      object tsOpen: TTabSheet
        Caption = 'Open'
        ImageIndex = 14
        object btOPN: TButton
          Left = 24
          Top = 77
          Width = 92
          Height = 38
          Caption = 'Open (OPN)'
          TabOrder = 0
          OnClick = btOPNClick
        end
        object cbSecure: TCheckBox
          Left = 38
          Top = 122
          Width = 78
          Height = 19
          Caption = 'Secure'
          TabOrder = 1
        end
        object pKeys: TPanel
          Left = 160
          Top = 0
          Width = 766
          Height = 302
          Align = alRight
          Anchors = [akLeft, akTop, akRight, akBottom]
          BevelOuter = bvNone
          Enabled = False
          TabOrder = 2
          DesignSize = (
            766
            302)
          object sbGenerateKeys: TSpeedButton
            Left = 0
            Top = 0
            Width = 23
            Height = 302
            Hint = 'Generate new Key pair'
            Anchors = [akLeft, akTop, akBottom]
            Glyph.Data = {
              36030000424D3603000000000000360000002800000010000000100000000100
              18000000000000030000C30E0000C30E00000000000000000000FFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFEFEFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFEFEFEFDFDFDFDFDFDFFFFFFFFFFFFFFFFFFFFFFFF
              A6A6A62E2E2E030303181818747474F4F4F4FFFFFFFFFFFFFFFFFF5656560000
              00000000FDFDFDFFFFFFFFFFFF8F8F8F0000000000000000000000000000003E
              3E3EFBFBFBFFFFFFFFFFFF565656000000000000FDFDFDFFFFFFEEEEEE0A0A0A
              0000000505054B4B4B191919000000000000777777A9A9A9A9A9A93939390000
              00000000A8A8A8E2E2E2B5B5B5000000000000878787FFFFFFDDDDDD00000000
              0000000000000000000000000000000000000000000000A8A8A8B5B5B5000000
              000000868686FFFFFFDCDCDC0000000000000000000000000000000000000000
              00000000000000A8A8A8EEEEEE0A0A0A00000004040449494918181800000000
              0000787878ABABABABABABABABABABABABABABABABABABE2E2E2FFFFFF919191
              000000000000000000000000000000404040FCFCFCFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA8A8A83030300404041A1A1A767676F5
              F5F5FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
            OnClick = sbGenerateKeysClick
          end
          object gbExponent: TGroupBox
            Left = 665
            Top = 0
            Width = 101
            Height = 302
            Align = alRight
            Caption = 'Exponent'
            TabOrder = 0
            object mExponent: TMemo
              Left = 2
              Top = 17
              Width = 97
              Height = 283
              Align = alClient
              Lines.Strings = (
                '010001')
              ScrollBars = ssVertical
              TabOrder = 0
            end
          end
          object gbModulus: TGroupBox
            Left = 24
            Top = 0
            Width = 641
            Height = 302
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Modulus'
            TabOrder = 1
            object mModulus: TMemo
              Left = 2
              Top = 17
              Width = 637
              Height = 283
              Align = alClient
              Lines.Strings = (
                
                  'AE6F230563307A1FA054C0E5835D9670C2EEC4BCE9E67C10A77B3D1F637C68CF' +
                  'FD2307E834'
                
                  '5120084B873F2B7D7E5DE1D3383DEB18D57106E58954B0A8D8E7DCFD84ACC724' +
                  'FB84DAEB'
                
                  '2A4082E2CE576F4AAB0EF3522CD2ED1C5F926FFBA070BA6F78E2FFCCBF78508D' +
                  'BD337670F'
                
                  '6C121B2E114AD939E87880833CA7B76F850B6D7E24C472CEF6A7F766951CC68C' +
                  'A782D05D3'
                
                  '7E621D3A7EE0A5FB5AB01437870A82664A3FCE8B3D75A64BE5DFFCF67FA4915C' +
                  '7A87D287E'
                
                  '97AAB5FCA6497C420840C0099F23FD089711209A31A6ED5EE9248D8C19D46F62' +
                  'A4EBC7971'
                
                  '43D80B85DAD47D0A485926298D81AFE23CA3D6229F3E011203713E5B74E9807C' +
                  'F98B71CD7'
                'D')
              ScrollBars = ssVertical
              TabOrder = 0
            end
          end
        end
      end
      object tsGIX: TTabSheet
        Caption = 'Information'
        ImageIndex = 15
        object Splitter1: TSplitter
          Left = 458
          Top = 0
          Width = 5
          Height = 302
          Align = alRight
        end
        object Splitter3: TSplitter
          Left = 727
          Top = 0
          Width = 5
          Height = 302
          Align = alRight
        end
        object gbGIN: TGroupBox
          Left = 732
          Top = 0
          Width = 194
          Height = 302
          Align = alRight
          Caption = 'GIN (obsolete)'
          TabOrder = 0
          object mGINResponse: TMemo
            Left = 2
            Top = 55
            Width = 190
            Height = 245
            Align = alClient
            ReadOnly = True
            ScrollBars = ssVertical
            TabOrder = 0
          end
          object pGINIDX: TPanel
            Left = 2
            Top = 17
            Width = 190
            Height = 38
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 1
            object Label3: TLabel
              Left = 6
              Top = 12
              Width = 43
              Height = 15
              Caption = 'ACQIDX'
            end
            object seGIN_ACQIDX: TSpinEdit
              Left = 58
              Top = 6
              Width = 38
              Height = 24
              MaxValue = 99
              MinValue = 0
              TabOrder = 0
              Value = 0
            end
            object btGIN: TButton
              Left = 109
              Top = 1
              Width = 75
              Height = 31
              Caption = 'GIN'
              TabOrder = 1
              OnClick = btGINClick
            end
          end
        end
        object gbGIX: TGroupBox
          Left = 0
          Top = 0
          Width = 458
          Height = 302
          Align = alClient
          Caption = 'GIX'
          TabOrder = 1
          object pGIXParams: TPanel
            Left = 2
            Top = 17
            Width = 111
            Height = 283
            Align = alLeft
            Caption = 'pGIXParams'
            TabOrder = 0
            DesignSize = (
              111
              283)
            object btGIX: TButton
              Left = 1
              Top = 249
              Width = 109
              Height = 33
              Anchors = [akLeft, akRight, akBottom]
              Caption = 'GIX'
              TabOrder = 0
              OnClick = btGIXClick
            end
            object lbGIXParams: TListBox
              Left = 1
              Top = 19
              Width = 109
              Height = 197
              Anchors = [akLeft, akTop, akRight, akBottom]
              ItemHeight = 15
              Items.Strings = (
                'PP_SERNUM     '
                'PP_PARTNBR    '
                'PP_MODEL      '
                'PP_MNNAME     '
                'PP_CAPAB      '
                'PP_SOVER      '
                'PP_SPECVER    '
                'PP_MANVERS    '
                'PP_APPVERS    '
                'PP_GENVERS    '
                'PP_KRNLVER    '
                'PP_CTLSVER    '
                'PP_MCTLSVER   '
                'PP_VCTLSVER   '
                'PP_AECTLSVER  '
                'PP_DPCTLSVER  '
                'PP_PUREVER    '
                'PP_DSPTXTSZ   '
                'PP_DSPGRSZ    '
                'PP_MFSUP      '
                'PP_MKTDESP    '
                'PP_MKTDESD    '
                'PP_DKPTTDESP  '
                'PP_DKPTTDESD  '
                'PP_EVENT      '
                'PP_TRK1INC    '
                'PP_TRK2INC    '
                'PP_TRK3INC    '
                'PP_TRACK1     '
                'PP_TRACK2     '
                'PP_TRACK3     '
                'PP_TRK1KSN    '
                'PP_TRK2KSN    '
                'PP_TRK3KSN    '
                'PP_ENCPAN     '
                'PP_ENCPANKSN  '
                'PP_KSN        '
                'PP_VALUE      '
                'PP_DATAOUT    '
                'PP_CARDTYPE   '
                'PP_ICCSTAT    '
                'PP_AIDTABINFO '
                'PP_PAN        '
                'PP_PANSEQNO   '
                'PP_EMVDATA    '
                'PP_CHNAME     '
                'PP_GOXRES     '
                'PP_PINBLK     '
                'PP_FCXRES     '
                'PP_ISRESULTS  '
                'PP_BIGRAND    '
                'PP_LABEL      '
                'PP_ISSCNTRY   '
                'PP_CARDEXP    '
                'PP_MFNAME     '
                'PP_DEVTYPE    '
                'PP_TLRMEM     '
                'PP_ENCKRAND   '
                'PP_KSNTDESP00 '
                'PP_KSNTDESP63 '
                'PP_KSNTDESD00 '
                'PP_KSNTDESD63 '
                'PP_TABVER00   '
                'PP_TABVER63   ')
              MultiSelect = True
              TabOrder = 1
            end
            object edGIXValue: TEdit
              Left = 0
              Top = 220
              Width = 109
              Height = 23
              Anchors = [akLeft, akRight, akBottom]
              TabOrder = 2
            end
            object cbGIXAll: TCheckBox
              Left = 1
              Top = 1
              Width = 109
              Height = 19
              Anchors = [akLeft, akTop, akRight]
              Caption = 'All'
              TabOrder = 3
            end
          end
          object mGIXResponse: TMemo
            Left = 113
            Top = 17
            Width = 343
            Height = 283
            Align = alClient
            ReadOnly = True
            ScrollBars = ssVertical
            TabOrder = 1
          end
        end
        object gbACBrPinPadCapabilities: TGroupBox
          Left = 463
          Top = 0
          Width = 264
          Height = 302
          Align = alRight
          Caption = 'ACBr PinPad Capabilities'
          TabOrder = 2
          DesignSize = (
            264
            302)
          object btACBrPinPadCapabilities: TButton
            Left = 2
            Top = 17
            Width = 260
            Height = 32
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Get ACBr PinPad Capabilities'
            TabOrder = 0
            OnClick = btACBrPinPadCapabilitiesClick
          end
          object mACBrPinPadCapabilities: TMemo
            Left = 2
            Top = 51
            Width = 260
            Height = 247
            Anchors = [akLeft, akTop, akRight, akBottom]
            ReadOnly = True
            ScrollBars = ssVertical
            TabOrder = 1
          end
        end
      end
      object tsDisplay: TTabSheet
        Caption = 'Display'
        ImageIndex = 17
        object gbDSP: TGroupBox
          Left = 0
          Top = 0
          Width = 235
          Height = 302
          Align = alLeft
          Caption = 'DSP'
          TabOrder = 0
          DesignSize = (
            235
            302)
          object Label4: TLabel
            Left = 19
            Top = 27
            Width = 38
            Height = 15
            Caption = 'Linha 1'
          end
          object Label5: TLabel
            Left = 19
            Top = 77
            Width = 38
            Height = 15
            Caption = 'Linha 2'
          end
          object edtDSPMsg1: TEdit
            Left = 19
            Top = 46
            Width = 191
            Height = 24
            Anchors = [akLeft, akTop, akRight]
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -16
            Font.Name = 'Courier'
            Font.Style = []
            MaxLength = 16
            ParentFont = False
            TabOrder = 0
            Text = 'PROJETO ACBR'
          end
          object edtDSPMsg2: TEdit
            Left = 19
            Top = 96
            Width = 191
            Height = 24
            Anchors = [akLeft, akTop, akRight]
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -16
            Font.Name = 'Courier'
            Font.Style = []
            MaxLength = 16
            ParentFont = False
            TabOrder = 1
            Text = 'ACBrAbecsPinPad'
          end
          object btDSP: TButton
            Left = 11
            Top = 141
            Width = 82
            Height = 31
            Anchors = [akTop]
            Caption = 'Display (DSP)'
            TabOrder = 2
            OnClick = btDSPClick
          end
          object btDSPClear: TButton
            Left = 105
            Top = 141
            Width = 96
            Height = 31
            Anchors = [akTop]
            Caption = 'Clear Display'
            TabOrder = 3
            OnClick = btDSPClearClick
          end
        end
        object gbDSX: TGroupBox
          Left = 235
          Top = 0
          Width = 691
          Height = 302
          Align = alClient
          Caption = 'DEX'
          TabOrder = 1
          DesignSize = (
            691
            302)
          object Label6: TLabel
            Left = 19
            Top = 27
            Width = 27
            Height = 15
            Caption = 'Lines'
          end
          object mDEX: TMemo
            Left = 19
            Top = 46
            Width = 642
            Height = 110
            Anchors = [akLeft, akTop, akRight]
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -16
            Font.Name = 'Courier'
            Font.Style = []
            Lines.Strings = (
              'PROJETO ACBR'
              'ACBrAbecsPinPad'
              '----------------'
              'TEF e PIX ?'
              #201' no ACBr'
              '-+-+-+-+-+-+-+-+')
            ParentFont = False
            TabOrder = 0
          end
          object btDEX: TButton
            Left = 134
            Top = 180
            Width = 88
            Height = 31
            Caption = 'Display (DEX)'
            TabOrder = 1
            OnClick = btDEXClick
          end
          object btDEXClear: TButton
            Left = 366
            Top = 180
            Width = 96
            Height = 31
            Anchors = [akTop]
            Caption = 'Clear Display'
            TabOrder = 2
            OnClick = btDEXClearClick
          end
        end
      end
      object tsMultimidia: TTabSheet
        Caption = 'Multimedia'
        ImageIndex = 18
        object pMediaFile: TPanel
          Left = 0
          Top = 0
          Width = 298
          Height = 302
          Align = alLeft
          TabOrder = 0
          object pMFButtons: TPanel
            Left = 1
            Top = 256
            Width = 296
            Height = 45
            Align = alBottom
            TabOrder = 0
            object btLMF: TButton
              Left = 8
              Top = 6
              Width = 86
              Height = 25
              Caption = 'List (LMF)'
              TabOrder = 0
              OnClick = btLMFClick
            end
            object btDSI: TButton
              Left = 104
              Top = 6
              Width = 85
              Height = 25
              Caption = 'Display (DSI)'
              TabOrder = 1
              OnClick = btDSIClick
            end
            object btDMF: TButton
              Left = 200
              Top = 6
              Width = 81
              Height = 25
              Caption = 'Delete (DMF)'
              TabOrder = 2
              OnClick = btDMFClick
            end
          end
          object pLMFMediaTitle: TPanel
            Left = 1
            Top = 1
            Width = 296
            Height = 31
            Align = alTop
            Caption = 'Media Files'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -16
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
            TabOrder = 1
          end
          object lbLMFMedias: TListBox
            Left = 1
            Top = 32
            Width = 296
            Height = 224
            Align = alClient
            Columns = 1
            ItemHeight = 15
            MultiSelect = True
            TabOrder = 2
          end
        end
        object pgMedia: TPageControl
          Left = 298
          Top = 0
          Width = 628
          Height = 302
          ActivePage = tsQRCode
          Align = alClient
          TabOrder = 1
          object tsImage: TTabSheet
            Caption = 'Image'
            object pMedia: TPanel
              Left = 0
              Top = 0
              Width = 620
              Height = 272
              Align = alClient
              TabOrder = 0
              object sbMedia: TScrollBox
                Left = 1
                Top = 1
                Width = 618
                Height = 223
                Align = alClient
                TabOrder = 0
                object imgMedia: TImage
                  Left = 0
                  Top = 0
                  Width = 614
                  Height = 219
                  Align = alClient
                  AutoSize = True
                  Center = True
                end
              end
              object pMediaLoad: TPanel
                Left = 1
                Top = 224
                Width = 618
                Height = 47
                Align = alBottom
                TabOrder = 1
                DesignSize = (
                  618
                  47)
                object btSearchSerialPorts1: TSpeedButton
                  Left = 365
                  Top = 14
                  Width = 25
                  Height = 22
                  Anchors = [akTop, akRight]
                  OnClick = btSearchSerialPorts1Click
                end
                object btMediaLoad: TButton
                  Left = 524
                  Top = 12
                  Width = 75
                  Height = 25
                  Anchors = [akTop, akRight]
                  Caption = 'Media Load'
                  TabOrder = 0
                  OnClick = btMediaLoadClick
                end
                object edMediaLoad: TEdit
                  Left = 5
                  Top = 13
                  Width = 354
                  Height = 23
                  Anchors = [akLeft, akTop, akRight]
                  TabOrder = 1
                  Text = 'LOGOACBR'
                end
                object pMediaInfo: TPanel
                  Left = 397
                  Top = 13
                  Width = 112
                  Height = 23
                  Anchors = [akTop, akRight]
                  BevelInner = bvRaised
                  BevelOuter = bvLowered
                  TabOrder = 2
                end
              end
            end
          end
          object tsQRCode: TTabSheet
            Caption = 'QRCode'
            object pQREGerado: TPanel
              Left = 0
              Top = 0
              Width = 620
              Height = 272
              Align = alClient
              TabOrder = 0
              object imgQRCode: TImage
                Left = 1
                Top = 1
                Width = 255
                Height = 231
                Align = alLeft
                Center = True
                Proportional = True
                Stretch = True
              end
              object pQREMemo: TPanel
                Left = 256
                Top = 1
                Width = 363
                Height = 231
                Align = alClient
                BevelOuter = bvNone
                TabOrder = 0
                object mQRCode: TMemo
                  Left = 0
                  Top = 0
                  Width = 363
                  Height = 231
                  Align = alClient
                  Alignment = taCenter
                  Lines.Strings = (
                    '00020126360014br.gov.bcb.pix0114187605400'
                    '00139520400005303986540510.005802BR5912P'
                    'ROJETO '
                    'ACBR6005Tatui61081827017062070503***6304'
                    '0B2D')
                  ScrollBars = ssVertical
                  TabOrder = 0
                  WantReturns = False
                end
                object btPaintQRCode: TButton
                  Left = 0
                  Top = 201
                  Width = 366
                  Height = 30
                  Caption = 'Generate QRCode'
                  TabOrder = 1
                  OnClick = btPaintQRCodeClick
                end
              end
              object Panel1: TPanel
                Left = 1
                Top = 232
                Width = 618
                Height = 39
                Align = alBottom
                TabOrder = 1
                object edQRCodeImgName: TEdit
                  Left = 8
                  Top = 8
                  Width = 136
                  Height = 23
                  TabOrder = 0
                  Text = 'QRCODE01'
                end
                object btSendQRCode: TButton
                  Left = 152
                  Top = 7
                  Width = 101
                  Height = 25
                  Caption = 'Send to PinPad'
                  TabOrder = 1
                  OnClick = btSendQRCodeClick
                end
              end
            end
          end
        end
      end
      object tsAskEvent: TTabSheet
        Caption = 'Ask / Events'
        ImageIndex = 16
        object gbGCD: TGroupBox
          Left = 146
          Top = 0
          Width = 217
          Height = 302
          Align = alLeft
          Caption = 'GCD (Capture Text)'
          TabOrder = 0
          DesignSize = (
            217
            302)
          object Label8: TLabel
            Left = 2
            Top = 17
            Width = 213
            Height = 75
            Align = alTop
            Alignment = taCenter
            Caption = 
              'In order to comply with PCI security requirements, the prompt me' +
              'ssage'#13'shall be selected among those available in a fixed table d' +
              'efined by ABECS specification'
            WordWrap = True
          end
          object Label9: TLabel
            Left = 10
            Top = 115
            Width = 55
            Height = 15
            Anchors = [akLeft, akBottom]
            Caption = 'Msg Index'
          end
          object Label15: TLabel
            Left = 10
            Top = 190
            Width = 44
            Height = 15
            Alignment = taRightJustify
            Anchors = [akLeft, akBottom]
            Caption = 'Timeout'
            Color = clBtnFace
            ParentColor = False
          end
          object btGCD: TButton
            Left = 117
            Top = 177
            Width = 83
            Height = 33
            Anchors = [akBottom]
            Caption = 'GCD'
            TabOrder = 0
            OnClick = btGCDClick
          end
          object pGCDResponse: TPanel
            Left = 10
            Top = 236
            Width = 190
            Height = 40
            Anchors = [akLeft, akRight, akBottom]
            BevelInner = bvRaised
            BevelOuter = bvLowered
            Caption = 'Response: '
            TabOrder = 1
          end
          object cbxGCD: TComboBox
            Left = 10
            Top = 132
            Width = 190
            Height = 23
            AutoDropDown = True
            Style = csDropDownList
            Anchors = [akLeft, akRight, akBottom]
            ItemHeight = 15
            TabOrder = 2
          end
          object seGCDTimeOut: TSpinEdit
            Left = 56
            Top = 182
            Width = 50
            Height = 24
            Anchors = [akLeft, akBottom]
            MaxValue = 999
            MinValue = 0
            TabOrder = 3
            Value = 60
          end
        end
        object gbMNU: TGroupBox
          Left = 363
          Top = 0
          Width = 239
          Height = 302
          Align = alLeft
          Caption = 'MNU (Menu)'
          TabOrder = 1
          DesignSize = (
            239
            302)
          object Label16: TLabel
            Left = 160
            Top = 156
            Width = 44
            Height = 15
            Alignment = taRightJustify
            Anchors = [akRight, akBottom]
            Caption = 'Timeout'
            Color = clBtnFace
            ParentColor = False
          end
          object Label10: TLabel
            Left = 10
            Top = 18
            Width = 56
            Height = 15
            Caption = 'Menu Title'
          end
          object Label11: TLabel
            Left = 10
            Top = 73
            Width = 42
            Height = 15
            Caption = 'Options'
          end
          object seMNUTimeOut: TSpinEdit
            Left = 160
            Top = 173
            Width = 50
            Height = 24
            Anchors = [akRight, akBottom]
            MaxValue = 999
            MinValue = 0
            TabOrder = 0
            Value = 60
          end
          object btMNU: TButton
            Left = 144
            Top = 205
            Width = 83
            Height = 33
            Anchors = [akRight, akBottom]
            Caption = 'MNU'
            TabOrder = 1
            OnClick = btMNUClick
          end
          object pMNUResponse: TPanel
            Left = 10
            Top = 253
            Width = 217
            Height = 40
            Anchors = [akLeft, akRight, akBottom]
            BevelInner = bvRaised
            BevelOuter = bvLowered
            Caption = 'Result: 0'
            TabOrder = 2
          end
          object edMNUTitle: TEdit
            Left = 10
            Top = 37
            Width = 217
            Height = 23
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 3
            Text = 'How was the service?'
          end
          object mMNU: TMemo
            Left = 10
            Top = 91
            Width = 128
            Height = 147
            Anchors = [akLeft, akTop, akRight, akBottom]
            Lines.Strings = (
              'Great'
              'Satisfactory'
              'Good'
              'Bad')
            TabOrder = 4
          end
          object cbMNUHotKey: TCheckBox
            Left = 80
            Top = 69
            Width = 56
            Height = 19
            Anchors = [akTop, akRight]
            Caption = 'Hotkey'
            TabOrder = 5
          end
        end
        object gbCEX: TGroupBox
          Left = 602
          Top = 0
          Width = 324
          Height = 302
          Align = alClient
          Caption = 'CEX (Wait for event)'
          TabOrder = 2
          DesignSize = (
            324
            302)
          object Label17: TLabel
            Left = 188
            Top = 74
            Width = 44
            Height = 15
            Alignment = taRightJustify
            Caption = 'Timeout'
            Color = clBtnFace
            ParentColor = False
          end
          object btCEX: TButton
            Left = 40
            Top = 139
            Width = 83
            Height = 25
            Caption = 'CEX'
            TabOrder = 0
            OnClick = btCEXClick
          end
          object cbCEXVerifyKey: TCheckBox
            Left = 16
            Top = 25
            Width = 70
            Height = 19
            Caption = 'Verify Key'
            TabOrder = 1
          end
          object cbCEXVerifyMagnetic: TCheckBox
            Left = 16
            Top = 46
            Width = 101
            Height = 19
            Caption = 'Verify Magnetic'
            TabOrder = 2
          end
          object cbCEXVerifyICCInsertion: TCheckBox
            Left = 16
            Top = 67
            Width = 119
            Height = 19
            Caption = 'Verify ICC Insertion'
            TabOrder = 3
          end
          object cbCEXVerifyICCRemoval: TCheckBox
            Left = 16
            Top = 88
            Width = 119
            Height = 19
            Caption = 'Verify ICC Removal'
            TabOrder = 4
          end
          object cbCEXVerifyCTLSPresence: TCheckBox
            Left = 16
            Top = 109
            Width = 127
            Height = 19
            Caption = 'Verify CTLS Presence'
            TabOrder = 5
          end
          object seCEXTimeOut: TSpinEdit
            Left = 188
            Top = 91
            Width = 50
            Height = 24
            MaxValue = 999
            MinValue = 0
            TabOrder = 6
            Value = 60
          end
          object mCEXResponse: TMemo
            Left = 8
            Top = 173
            Width = 298
            Height = 120
            Anchors = [akLeft, akTop, akRight, akBottom]
            Lines.Strings = (
              'Event identification:'
              #8220'00'#8221' = [OK/ENTER] key pressed;'
              #8220'02'#8221' = [?] key pressed;'
              #8220'03'#8221' = [?] key pressed;'
              #8220'04'#8221' = [F1] key pressed;'
              #8220'05'#8221' = [F2] key pressed;'
              #8220'06'#8221' = [F3] key pressed;'
              #8220'07'#8221' = [F4] key pressed;'
              #8220'08'#8221' = [CLEAR] key pressed;'
              #8220'13'#8221' = [CANCEL] key pressed;'
              #8220'90'#8221' = A magnetic card was swiped;'
              #8220'91'#8221' = ICC removed (or already '
              'absent);'
              #8220'92'#8221' = ICC inserted (or already '
              'present);'
              #8220'93'#8221' = CTLS not detected in 2 (two) '
              'minutes;'
              #8220'94'#8221' = CTLS detected.')
            ReadOnly = True
            ScrollBars = ssVertical
            TabOrder = 7
          end
        end
        object Panel2: TPanel
          Left = 0
          Top = 0
          Width = 146
          Height = 302
          Align = alLeft
          TabOrder = 3
          object gbGKY: TGroupBox
            Left = 1
            Top = 1
            Width = 144
            Height = 151
            Align = alClient
            Caption = 'GKY (Get Key)'
            TabOrder = 0
            DesignSize = (
              144
              151)
            object Label7: TLabel
              Left = 2
              Top = 17
              Width = 140
              Height = 45
              Align = alTop
              Alignment = taCenter
              Caption = 'Obsolete. This command does not return numeric keys'
              WordWrap = True
            end
            object btGKY: TButton
              Left = 32
              Top = 77
              Width = 83
              Height = 25
              Caption = 'GKY'
              TabOrder = 0
              OnClick = btGKYClick
            end
            object pGKYResponse: TPanel
              Left = 17
              Top = 109
              Width = 112
              Height = 24
              Anchors = [akLeft, akTop, akRight]
              BevelInner = bvRaised
              BevelOuter = bvLowered
              Caption = 'Result: 0'
              TabOrder = 1
            end
          end
          object GroupBox1: TGroupBox
            Left = 1
            Top = 152
            Width = 144
            Height = 149
            Align = alBottom
            Caption = 'RMC (ICC removal)'
            TabOrder = 1
            DesignSize = (
              144
              149)
            object btRMC: TButton
              Left = 36
              Top = 107
              Width = 83
              Height = 25
              Caption = 'RMC'
              TabOrder = 0
              OnClick = btRMCClick
            end
            object edtRMCMsg1: TEdit
              Left = 8
              Top = 32
              Width = 128
              Height = 24
              Anchors = [akLeft, akTop, akRight]
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -16
              Font.Name = 'Courier'
              Font.Style = []
              MaxLength = 16
              ParentFont = False
              TabOrder = 1
              Text = 'OPERATION '
            end
            object edtRMCMsg2: TEdit
              Left = 8
              Top = 65
              Width = 128
              Height = 24
              Anchors = [akLeft, akTop, akRight]
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -16
              Font.Name = 'Courier'
              Font.Style = []
              MaxLength = 16
              ParentFont = False
              TabOrder = 2
              Text = 'FINISHED'
            end
          end
        end
      end
      object tsClose: TTabSheet
        Caption = 'Close'
        ImageIndex = 11
        object gbCLO: TGroupBox
          Left = 0
          Top = 0
          Width = 235
          Height = 302
          Align = alLeft
          Caption = 'CLO'
          TabOrder = 0
          DesignSize = (
            235
            302)
          object Label1: TLabel
            Left = 26
            Top = 44
            Width = 38
            Height = 15
            Caption = 'Linha 1'
          end
          object Label2: TLabel
            Left = 26
            Top = 94
            Width = 38
            Height = 15
            Caption = 'Linha 2'
          end
          object edtCLOMsg1: TEdit
            Left = 26
            Top = 63
            Width = 181
            Height = 24
            Anchors = [akLeft, akTop, akRight]
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -16
            Font.Name = 'Courier'
            Font.Style = []
            MaxLength = 16
            ParentFont = False
            TabOrder = 0
            Text = 'PROJETO ACBR'
          end
          object edtCLOMsg2: TEdit
            Left = 26
            Top = 113
            Width = 181
            Height = 24
            Anchors = [akLeft, akTop, akRight]
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -16
            Font.Name = 'Courier'
            Font.Style = []
            MaxLength = 16
            ParentFont = False
            TabOrder = 1
            Text = 'ACBrAbecsPinPad'
          end
          object btCLO: TButton
            Left = 55
            Top = 183
            Width = 88
            Height = 38
            Anchors = [akTop]
            Caption = 'Close (CLO)'
            TabOrder = 2
            OnClick = btCLOClick
          end
        end
        object gbCLX: TGroupBox
          Left = 235
          Top = 0
          Width = 691
          Height = 302
          Align = alClient
          Caption = 'CLX'
          TabOrder = 1
          DesignSize = (
            691
            302)
          object btCLX: TButton
            Left = 302
            Top = 243
            Width = 88
            Height = 38
            Anchors = [akBottom]
            Caption = 'Close (CLX)'
            TabOrder = 0
            OnClick = btCLXClick
          end
          object pgCLX: TPageControl
            Left = 2
            Top = 17
            Width = 687
            Height = 204
            ActivePage = tsCLOMedia
            Align = alTop
            TabOrder = 1
            object tsCLOLines: TTabSheet
              Caption = 'Lines'
              object mCLX: TMemo
                Left = 0
                Top = 0
                Width = 679
                Height = 174
                Align = alClient
                Anchors = [akLeft, akTop, akRight]
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -16
                Font.Name = 'Courier'
                Font.Style = []
                Lines.Strings = (
                  'PROJETO ACBR'
                  'ACBrAbecsPinPad'
                  '----------------'
                  'TEF e PIX ?'
                  #201' no ACBr'
                  '-+-+-+-+-+-+-+-+')
                ParentFont = False
                ScrollBars = ssVertical
                TabOrder = 0
              end
            end
            object tsCLOMedia: TTabSheet
              Caption = 'Media'
              object lbCLXMedias: TListBox
                Left = 0
                Top = 31
                Width = 679
                Height = 143
                Align = alClient
                Columns = 2
                ItemHeight = 15
                TabOrder = 0
              end
              object pCLXMediaTitle: TPanel
                Left = 0
                Top = 0
                Width = 679
                Height = 31
                Align = alTop
                Caption = 'Media Files'
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -16
                Font.Name = 'Segoe UI'
                Font.Style = []
                ParentFont = False
                TabOrder = 1
              end
            end
          end
        end
      end
    end
  end
  object ACBrAbecsPinPad1: TACBrAbecsPinPad
    OnWriteLog = ACBrAbecsPinPad1WriteLog
    OnStartCommand = ACBrAbecsPinPad1StartCommand
    OnWaitForResponse = ACBrAbecsPinPad1WaitForResponse
    OnEndCommand = ACBrAbecsPinPad1EndCommand
    Left = 590
  end
  object OpenPictureDialog1: TOpenPictureDialog
    Filter = 
      'All Compatible Image Types|*.png;*.jpeg;*.jpg;*.gif|PNG Files (*' +
      '.png)|*.png|JPEG Files (*.jpeg;*.jpg;*.jpe;*.jfif)|*.jpeg;*.jpg|' +
      'Graphics Interchange Format Files (*.gif)|*.gif'
    Title = 'Open Image File'
    Left = 678
  end
  object ApplicationEvents1: TApplicationEvents
    OnException = ApplicationEvents1Exception
    Left = 712
  end
  object ImageList1: TImageList
    DrawingStyle = dsTransparent
    Left = 648
    Bitmap = {
      494C010113001800040010001000FFFFFFFFFF00FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000006000000001002000000000000060
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000D1D1D100808080005A5A5A005959590080808000D0D0D0000000
      000000000000000000000000000000000000000000009E9E9E003F3F3F004C4C
      4C004A4A4A004A4A4A004A4A4A004A4A4A004A4A4A004A4A4A004A4A4A004A4A
      4A004C4C4C003F3F3F009E9E9E000000000000000000FCFCFC008A8A8A005757
      57005B5B5B005A5A5A005A5A5A005A5A5A005A5A5A005A5A5A005A5A5A005B5B
      5B00575757008A8A8A00FCFCFC00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000EDED
      ED00545454000000000000000000000000000000000000000000000000005353
      5300ECECEC00000000000000000000000000000000009C9C9C00404040005252
      52004F4F4F004F4F4F004F4F4F004F4F4F004F4F4F004F4F4F004F4F4F004F4F
      4F0052525200404040009C9C9C0000000000000000008A8A8A00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000008A8A8A00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000EDEDED002A2A
      2A00000000000000000000000000727272007373730000000000000000000000
      000029292900ECECEC000000000000000000FEFEFE0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FEFEFE000000000057575700000000000808
      0800040404000404040004040400040404000404040004040400040404000404
      0400080808000000000057575700000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000555555000000
      0000000000000000000000000000707070007171710000000000000000000000
      00000000000053535300000000000000000000000000E1E1E100363636000000
      0000040404000303030003030300030303000303030003030300030303000404
      04000000000036363600E1E1E10000000000000000005B5B5B00000000000404
      0400010101000000000001010100000000000101010001010100010101000000
      000004040400000000005B5B5B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000D2D2D200000000000000
      0000000000000000000000000000010101000101010000000000000000000000
      00000000000000000000D0D0D0000000000000000000646464000D0D0D00A5A5
      A500A2A2A200A1A1A100A2A2A200A2A2A200A2A2A200A2A2A200A1A1A100A2A2
      A200A5A5A5000D0D0D006464640000000000000000005A5A5A00000000000404
      0400000000000101010000000000010101000404040002020200000000000101
      010005050500000000005A5A5A00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000082828200000000000000
      0000000000000000000000000000A4A4A400B7B7B70000000000000000000000
      0000000000000000000080808000000000000000000048484800525252000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000525252004848480000000000000000005A5A5A00000000000606
      0600000000000000000001010100020202000000000000000000060606000101
      0100000000000000000059595900000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000005B5B5B00000000000000
      00000000000000000000000000005E5E5E00F7F7F7002C2C2C00000000000000
      000000000000000000005959590000000000000000004C4C4C004A4A4A000000
      0000FAFAFA00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FCFCFC00FAFA
      FA00000000004A4A4A004C4C4C0000000000000000005B5B5B00000000000000
      0000040404000303030002020200000000005252520013131300000000000000
      0000808080004E4E4E004A4A4A00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000005C5C5C00000000000000
      00000000000000000000000000000101010098989800E7E7E700212121000000
      000000000000000000005A5A5A0000000000000000004B4B4B004B4B4B000000
      0000FEFEFE00000000000000000000000000000000000000000000000000FEFE
      FE00000000004B4B4B004B4B4B0000000000000000004F4F4F00363636003F3F
      3F000000000000000000000000007070700000000000CBCBCB0071717100A3A3
      A300000000004E4E4E004A4A4A00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000082828200000000000000
      00000000000001010100010101000000000002020200CACACA00979797000000
      000000000000000000008080800000000000000000004B4B4B004B4B4B000000
      0000FEFEFE00000000000000000000000000000000000000000000000000FEFE
      FE00000000004B4B4B004B4B4B0000000000000000004949490054545400F9F9
      F90053535300262626008686860000000000FEFEFE0000000000000000000000
      0000000000004A4A4A004B4B4B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000D2D2D200010101000000
      00000000000099999900CCCCCC000202020002020200CBCBCB009B9B9B000000
      00000000000000000000D1D1D10000000000000000004B4B4B004B4B4B000000
      0000FDFDFD00000000000000000000000000000000000000000000000000FDFD
      FD00000000004B4B4B004B4B4B0000000000000000004B4B4B00484848000000
      000000000000FAFAFA0000000000FEFEFE003737370036363600FAFAFA00FBFB
      FB00000000004B4B4B004B4B4B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000565656000000
      00000000000033333300F8F8F800CCCCCC00CCCCCC00F8F8F800353535000000
      000000000000545454000000000000000000000000004C4C4C004C4C4C000000
      0000FCFCFC00FDFDFD00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FDFDFD00FCFC
      FC00000000004C4C4C004C4C4C0000000000000000004C4C4C004C4C4C000000
      0000FAFAFA00FEFEFE00FCFCFC00FCFCFC003636360036363600FCFCFC00FDFD
      FD00000000004C4C4C004C4C4C00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000EDEDED002B2B
      2B00000000000000000033333300999999009A9A9A0034343400000000000000
      00002A2A2A00ECECEC0000000000000000000000000048484800494949000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000049494900484848000000000000000000484848004A4A4A000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000004A4A4A0048484800000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000EDED
      ED00565656000101010000000000000000000000000000000000000000005555
      5500EDEDED00000000000000000000000000000000008A8A8A00000000004949
      49004C4C4C004B4B4B004B4B4B004B4B4B004B4B4B004B4B4B004B4B4B004C4C
      4C0049494900000000008A8A8A00000000000000000087878700000000004A4A
      4A004C4C4C004B4B4B004B4B4B004A4A4A004C4C4C004C4C4C004A4A4A004C4C
      4C004A4A4A000000000087878700000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000D3D3D300828282005C5C5C005B5B5B0082828200D2D2D2000000
      00000000000000000000000000000000000000000000FEFEFE008A8A8A004848
      48004C4C4C004B4B4B004B4B4B004B4B4B004B4B4B004B4B4B004B4B4B004C4C
      4C00484848008A8A8A00FEFEFE000000000000000000FCFCFC00878787004848
      48004C4C4C004B4B4B004B4B4B004B4B4B004A4A4A004A4A4A004B4B4B004C4C
      4C004848480087878700FCFCFC00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000D1D1D100808080005A5A5A005959590080808000D0D0D0000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FEFE
      FE009A9A9A00EBEBEB0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000E6E6E600A9A9
      A900A8A8A800A8A8A80000000000C6C6C600C5C5C50000000000A9A9A900A8A8
      A800A9A9A900E5E5E5000000000000000000000000000000000000000000EDED
      ED00545454000000000000000000000000000000000000000000000000005353
      5300ECECEC000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FEFEFE007C7C
      7C0009090900ABABAB0000000000000000000000000000000000000000000000
      0000FEFEFE00000000000000000000000000000000000000000000000000FEFE
      FE00FDFDFD00FDFDFD0000000000000000000000000000000000232323000101
      01000202020002020200FEFEFE00565656005454540000000000030303000202
      0200010101002121210000000000000000000000000000000000EDEDED002A2A
      2A00000000000000000000000000000000000000000000000000000000000000
      000029292900ECECEC0000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000F9F9F900777777000808
      0800949494000000000000000000000000000000000000000000A6A6A6002E2E
      2E00030303001818180074747400F4F4F4000000000000000000000000005656
      56000000000000000000FDFDFD0000000000000000000000000001010100AAAA
      AA00000000000000000000000000565656005454540000000000000000000000
      0000ACACAC0000000000FDFDFD00000000000000000000000000555555000000
      0000000000000000000000000000393939003A3A3A0000000000000000000000
      000000000000535353000000000000000000000000000000000000000000FAFA
      FA00B4B4B40069696900575757007E7E7E00D9D9D900898989000C0C0C008E8E
      8E0000000000000000000000000000000000000000008F8F8F00000000000000
      00000000000000000000000000003E3E3E00FBFBFB0000000000000000005656
      56000000000000000000FDFDFD0000000000000000000000000001010100AAAA
      AA00000000000000000000000000565656005454540000000000000000000000
      0000ACACAC0000000000FDFDFD000000000000000000D2D2D200000000000000
      0000000000000000000000000000A9A9A900ABABAB0000000000000000000000
      00000000000000000000D0D0D000000000000000000000000000F7F7F7006767
      67000B0B0B003F3F3F004E4E4E0022222200161616003030300099999900FCFC
      FC0000000000000000000000000000000000EEEEEE000A0A0A00000000000505
      05004B4B4B0019191900000000000000000077777700A9A9A900A9A9A9003939
      39000000000000000000A8A8A800E2E2E200000000000000000001010100AAAA
      AA0000000000D4D4D400A9A9A9003939390038383800A9A9A900D4D4D4000000
      0000ACACAC0000000000FDFDFD00000000000000000082828200000000000000
      0000000000000000000000000000A9A9A900ABABAB0000000000000000000000
      0000000000000000000080808000000000000000000000000000929292000606
      0600B3B3B300F8F8F800FDFDFD00ECECEC005E5E5E001A1A1A00DDDDDD000000
      000000000000000000000000000000000000B5B5B50000000000000000008787
      870000000000DDDDDD0000000000000000000000000000000000000000000000
      0000000000000000000000000000A8A8A800000000000000000001010100AAAA
      AA0000000000F1F1F1003A3A3A00000000000000000038383800F1F1F1000000
      0000ACACAC0000000000FDFDFD0000000000000000005B5B5B00000000000000
      0000000000000000000000000000A9A9A900ABABAB0000000000000000000000
      00000000000000000000595959000000000000000000F5F5F500303030006A6A
      6A0000000000000000000000000000000000EDEDED0021212100888888000000
      000000000000000000000000000000000000B5B5B50000000000000000008686
      860000000000DCDCDC0000000000000000000000000000000000000000000000
      0000000000000000000000000000A8A8A800000000000000000001010100AAAA
      AA000000000000000000F1F1F1003A3A3A0038383800F1F1F100000000000000
      0000ACACAC0000000000FDFDFD0000000000000000005C5C5C00000000000000
      0000000000000000000000000000707070007171710000000000000000000000
      000000000000000000005A5A5A000000000000000000E5E5E5001D1D1D00A2A2
      A20000000000000000000000000000000000FBFBFB0047474700656565000000
      000000000000000000000000000000000000EEEEEE000A0A0A00000000000404
      04004949490018181800000000000000000078787800ABABAB00ABABAB00ABAB
      AB00ABABAB00ABABAB00ABABAB00E2E2E200000000000000000001010100AAAA
      AA00000000000000000000000000F1F1F100F1F1F10000000000000000000000
      0000ACACAC0000000000FDFDFD00000000000000000082828200000000000000
      0000000000000000000000000000010101000101010000000000000000000000
      00000000000000000000808080000000000000000000EEEEEE00272727008989
      890000000000000000000000000000000000F2F2F2002B2B2B007A7A7A000000
      0000000000000000000000000000000000000000000091919100000000000000
      000000000000000000000000000040404000FCFCFC0000000000000000000000
      0000000000000000000000000000000000000000000000000000010101007171
      7100A9A9A900A9A9A900A9A9A900A9A9A900A9A9A900A9A9A900A9A9A900A9A9
      A9007272720000000000FDFDFD000000000000000000D2D2D200010101000000
      0000000000000000000000000000A9A9A900ABABAB0000000000000000000000
      00000000000000000000D1D1D1000000000000000000000000006B6B6B001D1D
      1D00E6E6E600000000000000000000000000989898000C0C0C00C4C4C4000000
      0000000000000000000000000000000000000000000000000000A8A8A8003030
      3000040404001A1A1A0076767600F5F5F5000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000010101000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FDFDFD00000000000000000000000000565656000000
      0000000000000000000000000000383838003939390000000000000000000000
      0000000000005454540000000000000000000000000000000000E8E8E8003434
      34001515150076767600979797005E5E5E00080808007C7C7C00FDFDFD000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000242424000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000002222220000000000000000000000000000000000EDEDED002B2B
      2B00000000000000000000000000000000000000000000000000000000000000
      00002A2A2A00ECECEC000000000000000000000000000000000000000000E9E9
      E900727272002E2E2E00252525003C3C3C00A5A5A500FBFBFB00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000E7E7E700ACAC
      AC00AAAAAA00AAAAAA00AAAAAA00AAAAAA00AAAAAA00AAAAAA00AAAAAA00AAAA
      AA00ABABAB00E6E6E6000000000000000000000000000000000000000000EDED
      ED00565656000101010000000000000000000000000000000000000000005555
      5500EDEDED000000000000000000000000000000000000000000000000000000
      000000000000F4F4F400ECECEC00FAFAFA000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000D3D3D300828282005C5C5C005B5B5B0082828200D2D2D2000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FAFAFA00C6C6C600808080005C5C5C005D5D5D0084848400C9C9C900FCFC
      FC00000000000000000000000000000000000000000000000000000000000000
      0000FAFAFA00C6C6C600808080005C5C5C005D5D5D0084848400C9C9C900FCFC
      FC00000000000000000000000000000000000000000000000000ECECEC00E3E3
      E300E3E3E300E3E3E300E3E3E300E3E3E300E3E3E300E3E3E300E3E3E300E3E3
      E300E3E3E300E9E9E900FEFEFE00000000000000000000000000000000000000
      0000FAFAFA00C6C6C600808080005C5C5C005D5D5D0083838300C9C9C900FBFB
      FB0000000000000000000000000000000000000000000000000000000000E7E7
      E7005353530009090900000000000000000000000000000000000B0B0B005B5B
      5B00EBEBEB00000000000000000000000000000000000000000000000000E7E7
      E7005353530009090900000000000000000000000000000000000B0B0B005B5B
      5B00EBEBEB0000000000000000000000000000000000A5A5A500272727001C1C
      1C001C1C1C001C1C1C001C1C1C001C1C1C001C1C1C001C1C1C001C1C1C001C1C
      1C001C1C1C0024242400B7B7B70000000000000000000000000000000000E6E6
      E6005353530009090900000000000000000000000000000000000B0B0B005A5A
      5A00EBEBEB000000000000000000000000000000000000000000E7E7E7003131
      3100000000000000000000000000000000000000000000000000000000000000
      000039393900EDEDED0000000000000000000000000000000000E7E7E7003131
      3100000000000000000000000000000000000000000000000000000000000000
      000039393900EDEDED0000000000000000000000000057575700000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000005E5E5E00000000000000000000000000E6E6E6003030
      3000000000000000000000000000000000000000000000000000000000000000
      000039393900ECECEC00000000000000000000000000FAFAFA00535353000000
      0000000000000000000000000000393939003939390000000000000000000000
      0000000000005F5F5F00FCFCFC000000000000000000FAFAFA00535353000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000005F5F5F00FCFCFC00000000000000000055555500000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000555555000000000000000000FAFAFA00535353000000
      0000010101002525250003030300000000000000000004040400242424000101
      0100000000005F5F5F00FCFCFC000000000000000000C6C6C600090909000000
      0000000000000000000000000000AAAAAA00AAAAAA0000000000000000000000
      0000000000000D0D0D00CFCFCF000000000000000000C6C6C600090909000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000D0D0D00CFCFCF00000000000000000055555500000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000555555000000000000000000C6C6C600090909000000
      000025252500E3E3E3007171710003030300040404007C7C7C00DFDFDF001D1D
      1D00000000000D0D0D00CFCFCF00000000000000000080808000000000000000
      0000000000000000000000000000AAAAAA00AAAAAA0000000000000000000000
      000000000000000000008F8F8F00000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000008F8F8F00000000000000000055555500000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000055555500000000000000000080808000000000000000
      00000303030071717100EFEFEF00717171007D7D7D00EFEFEF00666666000202
      020000000000000000008E8E8E0000000000000000005C5C5C00000000000000
      000039393900AAAAAA00AAAAAA00E3E3E300E3E3E300AAAAAA00AAAAAA003939
      390000000000000000006969690000000000000000005C5C5C00000000000000
      000039393900AAAAAA00AAAAAA00AAAAAA00AAAAAA00AAAAAA00AAAAAA003939
      3900000000000000000069696900000000000000000055555500000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000005555550000000000000000005C5C5C00000000000000
      0000000000000303030071717100F4F4F400F4F4F40065656500020202000000
      000000000000000000006969690000000000000000005D5D5D00000000000000
      000039393900AAAAAA00AAAAAA00E3E3E300E3E3E300AAAAAA00AAAAAA003939
      390000000000000000006B6B6B0000000000000000005D5D5D00000000000000
      000039393900AAAAAA00AAAAAA00AAAAAA00AAAAAA00AAAAAA00AAAAAA003939
      390000000000000000006B6B6B00000000000000000055555500000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000005555550000000000000000005D5D5D00000000000000
      000000000000040404007D7D7D00F4F4F400F4F4F40071717100030303000000
      000000000000000000006B6B6B00000000000000000084848400000000000000
      0000000000000000000000000000AAAAAA00AAAAAA0000000000000000000000
      0000000000000000000092929200000000000000000084848400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000092929200000000000000000055555500000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000055555500000000000000000083838300000000000000
      0000040404007C7C7C00EFEFEF006565650071717100EFEFEF00717171000303
      03000000000000000000919191000000000000000000C9C9C9000B0B0B000000
      0000000000000000000000000000AAAAAA00AAAAAA0000000000000000000000
      0000000000000F0F0F00D3D3D3000000000000000000C9C9C9000B0B0B000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000F0F0F00D3D3D30000000000000000005B5B5B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000005C5C5C000000000000000000C9C9C9000B0B0B000000
      000024242400DFDFDF0066666600020202000303030071717100DBDBDB001C1C
      1C00000000000F0F0F00D3D3D3000000000000000000FCFCFC005B5B5B000000
      0000000000000000000000000000393939003939390000000000000000000000
      00000000000067676700FDFDFD000000000000000000FCFCFC005B5B5B000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000067676700FDFDFD000000000000000000B3B3B300151515000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000020202000B7B7B7000000000000000000FBFBFB005A5A5A000000
      0000010101001D1D1D00020202000000000000000000030303001C1C1C000101
      01000000000066666600FDFDFD00000000000000000000000000EBEBEB003939
      3900000000000000000000000000000000000000000000000000000000000000
      000042424200F1F1F10000000000000000000000000000000000EBEBEB003939
      3900000000000000000000000000000000000000000000000000000000000000
      000042424200F1F1F10000000000000000000000000000000000000000000000
      0000000000005555550055555500000000000000000055555500555555000000
      0000000000000000000000000000000000000000000000000000EBEBEB003838
      3800000000000000000000000000000000000000000000000000000000000000
      000041414100F0F0F0000000000000000000000000000000000000000000EDED
      ED005F5F5F000D0D0D00000000000000000000000000000000000F0F0F006767
      6700F1F1F100000000000000000000000000000000000000000000000000EDED
      ED005F5F5F000D0D0D00000000000000000000000000000000000F0F0F006767
      6700F1F1F1000000000000000000000000000000000000000000000000000000
      0000000000006C6C6C001C1C1C0055555500555555001C1C1C006C6C6C000000
      000000000000000000000000000000000000000000000000000000000000ECEC
      EC005F5F5F000D0D0D00000000000000000000000000000000000F0F0F006666
      6600F0F0F0000000000000000000000000000000000000000000000000000000
      0000FCFCFC00CFCFCF008F8F8F006A6A6A006B6B6B0092929200D3D3D300FDFD
      FD00000000000000000000000000000000000000000000000000000000000000
      0000FCFCFC00CFCFCF008F8F8F006A6A6A006B6B6B0092929200D3D3D300FDFD
      FD00000000000000000000000000000000000000000000000000000000000000
      000000000000DADADA0069696900555555005555550076767600E0E0E0000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FCFCFC00CFCFCF008E8E8E006A6A6A006B6B6B0091919100D3D3D300FDFD
      FD00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E3E3E300E3E3E300E3E3E300E3E3E300E3E3E300E3E3E300E3E3E300E3E3
      E300000000000000000000000000000000000000000000000000000000000000
      0000FAFAFA00C6C6C600808080005C5C5C005D5D5D0084848400C9C9C900FCFC
      FC00000000000000000000000000000000000000000000000000000000000000
      0000000000008E8E8E00555555005555550055555500555555008E8E8E000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00001C1C1C001C1C1C001C1C1C001C1C1C001C1C1C001C1C1C001C1C1C001C1C
      1C0000000000000000000000000000000000000000000000000000000000E7E7
      E7005353530009090900000000000000000000000000000000000B0B0B005B5B
      5B00EBEBEB000000000000000000000000000000000000000000000000000000
      0000000000005555550000000000000000000000000000000000555555000000
      0000000000000000000000000000000000000000000000000000E4E4E400AEAE
      AE00AAAAAA00AAAAAA00AAAAAA00B2B2B200F8F8F80000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000071717100AAAAAA00AAAAAA00AAAAAA00AAAAAA00717171000000
      0000000000000000000000000000000000000000000000000000E7E7E7003131
      3100000000000000000000000000000000000000000000000000000000000000
      000039393900EDEDED0000000000000000000000000000000000000000000000
      0000F7F7F7003C3C3C000000000000000000000000000000000040404000F8F8
      F8000000000000000000000000000000000000000000F0F0F000373737000101
      0100000000000000000000000000020202009292920000000000000000000000
      00000000000000000000000000000000000000000000C7C7C700AAAAAA00AAAA
      AA0000000000AAAAAA0000000000000000000000000000000000AAAAAA000000
      0000AAAAAA00AAAAAA00C7C7C7000000000000000000FAFAFA00535353000000
      0000000000000000000022222200060606000000000000000000000000000000
      0000000000005F5F5F00FCFCFC00000000000000000000000000000000000000
      0000B5B5B500010101000000000000000000000000000000000002020200BBBB
      BB000000000000000000000000000000000000000000E3E3E3001C1C1C000000
      00000000000000000000000000000000000055555500E3E3E3001C1C1C000000
      0000555555000000000000000000000000000000000055555500000000000000
      000000000000AAAAAA0000000000000000000000000000000000AAAAAA000000
      00000000000000000000555555000000000000000000C6C6C600090909000000
      00000000000048484800E7E7E700878787000303030000000000000000000000
      0000000000000D0D0D00CFCFCF0000000000000000000000000000000000FDFD
      FD003A3A3A000000000000000000000000000000000000000000000000004040
      4000FEFEFE0000000000000000000000000000000000E3E3E3001C1C1C000000
      00000000000000000000000000000000000055555500F6F6F600B4B4B400AAAA
      AA00C7C7C7000000000000000000000000000000000055555500000000000000
      00000000000071717100AAAAAA00AAAAAA00AAAAAA00AAAAAA00717171000000
      0000000000000000000055555500000000000000000080808000000000000000
      000046464600E8E8E800C1C1C100F3F3F3008383830004040400000000000000
      000000000000000000008F8F8F0000000000000000000000000000000000B8B8
      B800000000000000000000000000000000000000000000000000000000000101
      0100BFBFBF0000000000000000000000000000000000E3E3E3001C1C1C000000
      00000000000000000000000000000000000055555500F6F6F600B4B4B400AAAA
      AA00AAAAAA00B4B4B400F6F6F600000000000000000055555500000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000005555550000000000000000005C5C5C00000000003D3D
      3D00EEEEEE009D9D9D000D0D0D0061616100F8F8F80083838300030303000000
      0000000000000000000069696900000000000000000000000000000000005C5C
      5C00000000000000000000000000000000000000000000000000000000000000
      00005F5F5F0000000000000000000000000000000000E3E3E3001C1C1C000000
      00000000000000000000000000000000000055555500E3E3E3001C1C1C000000
      0000000000001C1C1C00E3E3E300000000000000000057575700000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D0D0D000333333005C5C5C0000000000000000005D5D5D00000000003434
      34009090900005050500000000000000000061616100F3F3F300878787000606
      060000000000000000006B6B6B00000000000000000000000000000000005555
      5500000000000000000000000000000000000000000000000000000000000000
      00005555550000000000000000000000000000000000E3E3E3001C1C1C000000
      0000000000000000000000000000000000005555550000000000000000000000
      0000000000000000000000000000000000000000000091919100030303000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000333333000E0E0E009E9E9E00000000000000000084848400000000000000
      0000000000000000000000000000000000000202020064646400F1F1F1008787
      8700030303000000000092929200000000000000000000000000000000005555
      5500000000000000000000000000000000000000000000000000000000000000
      00005555550000000000000000000000000000000000E3E3E3001C1C1C000000
      00000000000000000000000000000000000055555500EDEDED00686868005555
      550055555500555555008E8E8E000000000000000000F5F5F500989898005B5B
      5B00555555005555550055555500555555005555550055555500555555005555
      55005C5C5C009E9E9E00F7F7F7000000000000000000C9C9C9000B0B0B000000
      000000000000000000000000000000000000000000000202020064646400F3F3
      F3005D5D5D000F0F0F00D3D3D300000000000000000000000000000000005555
      5500000000000000000000000000000000000000000000000000000000000000
      00005555550000000000000000000000000000000000EDEDED00686868005555
      5500555555005555550055555500555555008E8E8E00EDEDED00686868005555
      550055555500555555008E8E8E00000000000000000000000000000000000000
      0000555555005555550055555500555555005555550055555500555555005555
      55000000000000000000000000000000000000000000FCFCFC005B5B5B000000
      0000000000000000000000000000000000000000000000000000020202004D4D
      4D001010100067676700FDFDFD0000000000000000000000000000000000C7C7
      C700000000007171710071717100727272007272720071717100717171000000
      0000C7C7C700000000000000000000000000000000008E8E8E00555555005555
      55005555550055555500555555005555550055555500C7C7C700000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000EBEBEB003939
      3900000000000000000000000000000000000000000000000000000000000000
      000042424200F1F1F10000000000000000000000000000000000000000000000
      000000000000AAAAAA00C7C7C700C7C7C700C7C7C700C7C7C700AAAAAA000000
      000000000000000000000000000000000000000000008E8E8E00555555003131
      31000000000000000000050505004949490055555500C7C7C700000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00001C1C1C001C1C1C001C1C1C001C1C1C001C1C1C001C1C1C001C1C1C001C1C
      1C0000000000000000000000000000000000000000000000000000000000EDED
      ED005F5F5F000D0D0D00000000000000000000000000000000000F0F0F006767
      6700F1F1F1000000000000000000000000000000000000000000000000000000
      0000171717003838380055555500555555005555550055555500383838001818
      180000000000000000000000000000000000000000000000000000000000EFEF
      EF00AAAAAA00AAAAAA00BABABA00FBFBFB000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E3E3E300E3E3E300E3E3E300E3E3E300E3E3E300E3E3E300E3E3E300E3E3
      E300000000000000000000000000000000000000000000000000000000000000
      0000FCFCFC00CFCFCF008F8F8F006A6A6A006B6B6B0092929200D3D3D300FDFD
      FD00000000000000000000000000000000000000000000000000000000000000
      0000B1B1B1005B5B5B005555550055555500555555005555550060606000BEBE
      BE00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000DDDDDD00D4D4D40000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000CECE
      CE001A1A1A0015151500B7B7B700000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FCFCFC00E6E6
      E600E3E3E300E3E3E300E3E3E300E3E3E300E3E3E300E3E3E300E3E3E300E3E3
      E300E4E4E400FAFAFA0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000CBCBCB002121
      2100000000000000000016161600DCDCDC0000000000F5F5F500B9B9B900AAAA
      AA00AAAAAA00AAAAAA00AAAAAA00AAAAAA00AAAAAA00AAAAAA00AAAAAA00AAAA
      AA00AAAAAA00B5B5B500F4F4F4000000000000000000F9F9F9006A6A6A001F1F
      1F001C1C1C001C1C1C001C1C1C001C1C1C001C1C1C001C1C1C001C1C1C001C1C
      1C001D1D1D0074747400FEFEFE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000CECECE00212121000000
      0000000000000000000030303000EBEBEB000000000078787800040404000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000003030300919191000000000000000000E4E4E4001D1D1D000000
      0000000000000000000021212100939393008E8E8E001B1B1B00000000000000
      00000000000021212100E7E7E700000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000CBCBCB001A1A1A00000000000000
      00000000000036363600E3E3E300000000000000000055555500555555000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000055555500555555000000000000000000E3E3E3001C1C1C000000
      00000000000009090900C4C4C4000000000000000000BABABA00050505000000
      0000000000001C1C1C00E3E3E300000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000CECECE001A1A1A0000000000000000000000
      000036363600E0E0E00000000000000000000000000055555500555555000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000055555500555555000000000000000000E3E3E3001C1C1C000000
      00000000000016161600DDDDDD000000000000000000D6D6D6000F0F0F000000
      0000000000001C1C1C00E3E3E30000000000FCFCFC00AEAEAE0090909000FCFC
      FC0000000000000000000000000000000000EBEBEB006B6B6B00555555005555
      550055555500555555008E8E8E0000000000000000000000000000000000D8D8
      D800A6A6A600A9A9A900B2B2B200212121000000000000000000000000003030
      3000E3E3E3000000000000000000000000000000000055555500555555000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000055555500555555000000000000000000E3E3E3001C1C1C000000
      000000000000020202008A8A8A00FCFCFC00FBFBFB007F7F7F00010101000000
      0000000000001C1C1C00E3E3E30000000000FAFAFA004E4E4E000D0D0D00C7C7
      C7000000000000000000000000000000000000000000B7B7B700151515000000
      00000000000000000000555555000000000000000000F6F6F600717171001111
      110000000000010101000606060000000000000000000000000030303000E0E0
      E000000000000000000000000000000000000000000055555500555555000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000055555500555555000000000000000000E3E3E3001C1C1C000000
      0000000000000000000006060600333333003030300005050500000000000000
      0000000000001C1C1C00E3E3E3000000000000000000C5C5C5000E0E0E002626
      2600DEDEDE000000000000000000000000000000000000000000ACACAC000707
      070000000000000000005555550000000000FCFCFC006F6F6F00000000000000
      0000000000000000000000000000000000000000000036363600E3E3E3000000
      0000000000000000000000000000000000000000000055555500000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000555555000000000000000000E3E3E3001C1C1C000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000001C1C1C00E3E3E300000000000000000000000000A0A0A0000D0D
      0D001111110080808000D1D1D100E6E6E600DEDEDE009B9B9B00262626000202
      02000B0B0B00000000005555550000000000DADADA000B0B0B00000000000505
      05000303030000000000000000000000000009090900C4C4C400000000000000
      0000000000000000000000000000000000000000000055555500000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000555555000000000000000000E3E3E3001C1C1C001313
      13001C1C1C001C1C1C001C1C1C001C1C1C001C1C1C0019191900030303000000
      0000000000001C1C1C00E3E3E30000000000000000000000000000000000B7B7
      B7002F2F2F0003030300010101000909090004040400000000001B1B1B008787
      8700BDBDBD001B1B1B005555550000000000A9A9A90000000000060606008D8D
      8D008383830004040400000000000000000002020200B2B2B200000000000000
      00000000000000000000000000000000000000000000555555001C1C1C005555
      5500555555005555550055555500555555005555550055555500555555005555
      5500555555001C1C1C00555555000000000000000000E3E3E3001C1C1C009797
      9700E3E3E300E3E3E300E3E3E300E3E3E300E3E3E300CACACA00191919000000
      0000000000001C1C1C00E3E3E300000000000000000000000000000000000000
      0000EFEFEF00B5B5B5007B7B7B00646464006D6D6D00A1A1A100E1E1E1000000
      000000000000C0C0C0007070700000000000A8A8A800050505008E8E8E00FCFC
      FC00FAFAFA0083838300030303000000000003030300B6B6B600000000000000
      0000000000000000000000000000000000000000000055555500555555000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000055555500555555000000000000000000E3E3E3001C1C1C00AAAA
      AA000000000000000000000000000000000000000000E3E3E3001C1C1C000000
      00000000000049494900F4F4F400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000EFEFEF0000000000E2E2E2009E9E9E00FEFEFE000000
      0000F9F9F9007878780002020200000000001A1A1A00E8E8E800000000000000
      000000000000000000000000000000000000000000008B8B8B00050505000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000080808008B8B8B000000000000000000E6E6E6001F1F1F007272
      7200AAAAAA00AAAAAA00AAAAAA00AAAAAA00AAAAAA0097979700131313000000
      000047474700EFEFEF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FCFC
      FC00787878000202020000000000030303009494940000000000000000000000
      00000000000000000000000000000000000000000000F5F5F500BABABA00AAAA
      AA00AAAAAA00AAAAAA00AAAAAA00AAAAAA00AAAAAA00AAAAAA00AAAAAA00AAAA
      AA00AAAAAA00C3C3C300FAFAFA000000000000000000FDFDFD00727272001F1F
      1F001C1C1C001C1C1C001C1C1C001C1C1C001C1C1C001C1C1C001C1C1C004949
      4900EFEFEF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000009191
      910002020200000000002222220094949400FCFCFC0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FBFBFB00E6E6
      E600E3E3E300E3E3E300E3E3E300E3E3E300E3E3E300E3E3E300E3E3E300F4F4
      F400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000EEEE
      EE00CCCCCC00D1D1D100EEEEEE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000600000000100010000000000000300000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FFFFFFFFFFFF0000F81F800180010000
      E007800180010000C0037FFE80010000C0038001800100008001800180010000
      80019FF9800100008001900980010000800197E980890000800197E981790000
      800197E99A090000C003900990090000C0039FF99FF90000E007800180010000
      F81F800180010000FFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFF81F
      FFE3FFFFC243E007FFC3F7E3C043C003FF87C0E1CE71C003E00F8061CE718001
      C00F0000C8118001C01F0800C81180018F1F0800CC3180018F1F0000CE718001
      8F1F807FC0018001C71FC0FFC001C003C01FFFFFC003C003E03FFFFFC003E007
      F8FFFFFFFFFFF81FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00FF00FC001F00F
      E007E0078001E007C003C0038001C00380018001800180018001800180018001
      8001800180018001800180018001800180018001800180018001800180018001
      80018001800180018001800180018001C003C003F99FC003E007E007F81FE007
      F00FF00FF81FF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00FF00FF81FFFFF
      F00FE007F81FC07FF00FC003F00F807F83C18001F00F800783C18001E0078007
      80018001E007800180018001E007800180018001E007807F80018001E0078001
      80018001E0078001F00F8001E007803FF00FC003F00F803FF00FE007F00FE0FF
      F00FF00FF00FFFFFFFFFFFFFFFFFFFFFFFF3FFFFFFFFFFFFFFE1FFFFC003FFFF
      FFC080018001FFFFFF8080018001FFFFFF019FF98181FFFFFE039FF981810F01
      E0079FF980010F81800F9FF9800187C1001F80018001C001003F80018001E001
      003F80018001F019003F9FF98F81FFFD103F80018003FFFFE07F80018007FFFF
      E07FFFFFC00FFFFFE1FFFFFFFFFFFFFF}
  end
end
