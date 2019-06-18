object ControlFrm: TControlFrm
  Left = 0
  Top = 0
  Width = 300
  Height = 592
  TabOrder = 0
  OnResize = FrameResize
  object Panel1: TcxGroupBox
    Left = 0
    Top = 0
    Align = alTop
    PanelStyle.Active = True
    Style.BorderStyle = ebsNone
    Style.TransparentBorder = False
    TabOrder = 0
    DesignSize = (
      300
      105)
    Height = 105
    Width = 300
    object lObjName: TcxLabel
      Left = 1
      Top = 4
      AutoSize = False
      Caption = 'lObjName'
      ParentFont = False
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -11
      Style.Font.Name = 'MS Sans Serif'
      Style.Font.Style = [fsBold]
      Style.IsFontAssigned = True
      Properties.Alignment.Horz = taCenter
      Transparent = True
      Height = 13
      Width = 268
      AnchorX = 135
    end
    object sbClose: TcxButton
      Left = 275
      Top = 2
      Width = 23
      Height = 19
      Anchors = [akTop, akRight]
      OptionsImage.Glyph.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000020000000A00000010000000090000000200000000000000000000
        00020000000A000000120000000C000000030000000000000000000000000000
        00020000000F0F0742921D0F7EEF0603347A0000000E00000002000000020000
        000F0804347C1D0F7EF00F084194000000120000000200000000000000000000
        0008120B47923233AFFF3648CCFF1D1EA5FF0603357A0000000F0000000F0703
        357C1F20A5FF3747CCFF2D2FAEFF120B46950000000B00000000000000000000
        000C281C8DF1596CD8FF3B51D3FF3A4FD2FF1E22A6FF0602347D0502357E2022
        A6FF3A50D3FF3A50D3FF4C5FD4FF291D8CF10000001000000000000000000000
        0006130F3C734D4FBAFF667EE0FF415AD6FF415AD7FF1F24A7FF2529A8FF415A
        D7FF415AD7FF5B72DEFF484AB8FF130F3C790000000900000000000000000000
        00010000000A16123F73585CC1FF758DE6FF4A64DBFF4A65DBFF4A65DBFF4A64
        DBFF6983E3FF5356C0FF16123F780000000C0000000200000000000000000000
        0000000000010000000A191643755D63C7FF6783E5FF5774E2FF5774E2FF5774
        E2FF565CC6FF1916437A0000000D000000020000000000000000000000000000
        00000000000100000009100E3D734A50BEFF7492EBFF6383E7FF6483E7FF6383
        E7FF3840B6FF0B0839780000000C000000020000000000000000000000000000
        0001000000071413416E555CC5FF85A1EFFF7897EDFF9CB6F4FF9DB7F5FF7997
        EEFF7796EDFF414ABCFF0E0C3C730000000A0000000100000000000000000000
        00041818456B636CCFFF93AFF3FF83A1F1FFA6BFF7FF676DCAFF7E87DDFFAFC7
        F8FF83A3F2FF83A1F1FF5058C4FF121040710000000600000000000000000000
        00065759C3EFAFC6F6FF8EADF4FFABC4F8FF6F76D0FF1817456F24244F70868E
        E1FFB5CCF9FF8DACF4FFA1B8F4FF5758C3EF0000000900000000000000000000
        000331326B8695A0EAFFC0D3F9FF7880D7FF1C1C496B00000006000000072527
        526C8B93E6FFC1D3F9FF949EE9FF303168870000000500000000000000000000
        00010000000431336B825E62CBEC1F204D680000000500000001000000010000
        00052728536B5E62CBEC31326883000000070000000100000000000000000000
        0000000000000000000200000004000000020000000100000000000000000000
        0001000000030000000500000004000000010000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000}
      PaintStyle = bpsGlyph
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.Flat = True
      TabOrder = 4
      OnClick = sbCloseClick
    end
    object lAddPrm1: TcxLabel
      Left = 8
      Top = 32
      Caption = 'lAddPrm1'
      Transparent = True
    end
    object lAddPrm2: TcxLabel
      Left = 8
      Top = 56
      Caption = 'lAddPrm2'
      Transparent = True
    end
    object lAddPrm3: TcxLabel
      Left = 8
      Top = 80
      Caption = 'lAddPrm3'
      Transparent = True
    end
    object eAddPrm1: TcxTextEdit
      Left = 260
      Top = 28
      Properties.ReadOnly = True
      TabOrder = 0
      Text = 'eAddPrm1'
      Width = 41
    end
    object eAddPrm2: TcxTextEdit
      Left = 260
      Top = 52
      Properties.ReadOnly = True
      TabOrder = 1
      Text = 'eAddPrm2'
      Width = 41
    end
    object eAddPrm3: TcxTextEdit
      Left = 260
      Top = 76
      Properties.ReadOnly = True
      TabOrder = 2
      Text = 'eAddPrm3'
      Width = 41
    end
  end
  object pnAggregates: TcxGroupBox
    Left = 0
    Top = 105
    Align = alTop
    PanelStyle.Active = True
    Style.BorderStyle = ebsNone
    Style.TransparentBorder = False
    TabOrder = 1
    Height = 244
    Width = 300
    inline Aggregate1: TControlOneAggregateFrame
      Left = 0
      Top = 0
      Width = 104
      Height = 244
      Align = alLeft
      TabOrder = 0
      ExplicitWidth = 104
      ExplicitHeight = 244
      inherited lbName: TcxLabel
        Style.IsFontAssigned = True
        AnchorX = 50
      end
      inherited LabelParams: TcxLabel
        Style.IsFontAssigned = True
      end
      inherited Label1: TcxLabel
        AnchorX = 45
      end
      inherited Label2: TcxLabel
        AnchorX = 45
      end
      inherited Label3: TcxLabel
        AnchorX = 45
      end
      inherited Label4: TcxLabel
        AnchorX = 45
      end
    end
    inline Aggregate2: TControlOneAggregateFrame
      Left = 104
      Top = 0
      Width = 104
      Height = 244
      Align = alLeft
      TabOrder = 1
      ExplicitLeft = 104
      ExplicitWidth = 104
      ExplicitHeight = 244
      inherited lbName: TcxLabel
        Style.IsFontAssigned = True
        AnchorX = 50
      end
      inherited LabelParams: TcxLabel
        Style.IsFontAssigned = True
      end
      inherited Label1: TcxLabel
        AnchorX = 45
      end
      inherited Label2: TcxLabel
        AnchorX = 45
      end
      inherited Label3: TcxLabel
        AnchorX = 45
      end
      inherited Label4: TcxLabel
        AnchorX = 45
      end
    end
    inline Aggregate3: TControlOneAggregateFrame
      Left = 208
      Top = 0
      Width = 104
      Height = 244
      Align = alLeft
      TabOrder = 2
      ExplicitLeft = 208
      ExplicitWidth = 104
      ExplicitHeight = 244
      inherited lbName: TcxLabel
        Style.IsFontAssigned = True
        AnchorX = 50
      end
      inherited LabelParams: TcxLabel
        Style.IsFontAssigned = True
      end
      inherited Label1: TcxLabel
        AnchorX = 45
      end
      inherited Label2: TcxLabel
        AnchorX = 45
      end
      inherited Label3: TcxLabel
        AnchorX = 45
      end
      inherited Label4: TcxLabel
        AnchorX = 45
      end
    end
  end
  object pnAlarm: TcxGroupBox
    Left = 0
    Top = 349
    Align = alClient
    PanelStyle.Active = True
    Style.BorderStyle = ebsNone
    Style.TransparentBorder = False
    TabOrder = 2
    Height = 243
    Width = 300
    object mAlarms: TcxMemo
      Left = 0
      Top = 0
      Align = alClient
      Lines.Strings = (
        '')
      Properties.ReadOnly = True
      Properties.ScrollBars = ssVertical
      TabOrder = 0
      Height = 243
      Width = 300
    end
  end
end
