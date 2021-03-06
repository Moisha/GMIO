object ScadaFormForDocking: TScadaFormForDocking
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'ScadaFormForDocking'
  ClientHeight = 579
  ClientWidth = 844
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  inline ControlFrm1: TControlFrm
    Left = 544
    Top = 0
    Width = 300
    Height = 579
    Align = alRight
    TabOrder = 0
    Visible = False
    inherited Panel1: TcxGroupBox
      inherited lObjName: TcxLabel
        Style.IsFontAssigned = True
        AnchorX = 135
      end
      inherited sbClose: TcxButton
        OnClick = sbCloseClick
      end
    end
    inherited pnAggregates: TcxGroupBox
      inherited Aggregate1: TControlOneAggregateFrame
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
      inherited Aggregate2: TControlOneAggregateFrame
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
      inherited Aggregate3: TControlOneAggregateFrame
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
    inherited pnAlarm: TcxGroupBox
      Height = 230
      inherited mAlarms: TcxMemo
        Height = 230
      end
    end
  end
  object Flex: TFlexPanel
    Left = 0
    Top = 0
    Width = 544
    Height = 579
    HorzScrollBar.Increment = 10
    HorzScrollBar.Range = 538
    VertScrollBar.Increment = 10
    VertScrollBar.Range = 403
    AutoZoom = True
    Scale = 168
    DocWidth = 320000
    DocHeight = 240000
    DocFrameColor = clBlack
    DocShadowColor = clGray
    DocSpaceBrush.Color = clBtnFace
    DocSpaceBrush.Style = bsBDiagonal
    DocSpaceFill = False
    GridStyle = gsDots
    GridColor = clGray
    GridPixColor = clSilver
    GridHorizSize = 10000
    GridVertSize = 10000
    OnUpdateCursor = FlexScadaUpdateCursor
    Align = alClient
    TabOrder = 1
    OnMouseDown = FlexScadaMouseDown
  end
  object cxImageList1: TcxImageList
    FormatVersion = 1
    DesignInfo = 8913272
    ImageInfo = <
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          00000000000000000000000000000000001F421C11FF30140DEC190A06B30304
          075F0000001B0000000500000000000000000000000000000000000000000000
          00000000000000000000000000000000001E663C2BE7B9C7D2FF7889A2FF2441
          82FF051033AF0000002100000005000000000000000000000000000000000000
          0002000000090000000E0000000F0000002041261BAE879AB2FFC8E3F5FF1F66
          B6FF2B6BA8FF051236AD0000001F000000040000000000000000000000000000
          00088C6657C0C38C7AFFC38C79FFCBA395FFA89894FF488BC3FFDEFEFDFF51B4
          E3FF1F68B7FF3173AEFF061538AA0000001C0000000400000000000000000000
          000CC5917EFFFDFBFAFFFCF8F6FFFAF7F5FFECEAE9FF7CA3BFFF479FD2FFDEFE
          FDFF59BFE9FF216BB9FF367BB3FF07173AA70000001A00000004000000000000
          000CC79481FFFEFBFAFFF9F0EAFFF8F0EAFFF7F0EBFFE8E4E1FF7EA4BFFF4BA5
          D5FFDEFEFDFF61CAEFFF246FBCFF3B83B9FF081A3DA300000018000000000000
          000BC99786FFFEFCFBFFF9F2EDFFF9F2EDFFF9F0EBFFF8F2EDFFEBE7E5FF82A7
          C2FF4EAAD7FFDEFEFDFF68D4F4FF2875BEFF3F8BBEFF091B3F9E000000000000
          000ACB9C8BFFFEFDFCFFFAF3EFFFFAF4EEFFFAF3EEFFFAF1ECFFF8F2EEFFEDE9
          E7FF85ABC7FF51AEDAFFDEFEFDFF6EDDF8FF2C7BC2FF18448BFF000000000000
          0009CFA08DFFFEFEFDFFFBF5F1FFFBF5F0FFFBF4F0FFFAF3EFFFFAF3EFFFF8F4
          EFFFEFECE9FF89AECAFF54B1DCFFDEFEFDFF4FA6D4FF102C4E93000000000000
          0009D0A393FFFEFEFDFFFAF5F3FFFBF6F2FFFBF5F1FFFBF5F0FFFBF5F0FFFAF4
          EFFFFAF6F1FFF3EFEDFF83A0B8FF357FBCFF173A598F0000000C000000000000
          0008D3A897FFFEFEFEFFFBF6F4FFFBF6F4FFFCF6F3FFFCF6F3FFFCF4F2FFFBF5
          F1FFFBF5F0FFFAF6F3FFE2CCC4FF000000160000000600000001000000000000
          0007D3AB9AFFFFFEFEFFFCF8F6FFFCF7F5FFFCF7F5FFFBF6F4FFFBF6F4FFFCF6
          F3FFFCF6F2FFFBF6F1FFD1A494FF0000000C0000000000000000000000000000
          0006D8AE9DFFFFFFFEFFFDF9F7FFFDF9F7FFFCF8F7FFFCF8F6FFFCF7F5FFFBF7
          F5FFFBF7F4FFFCF7F3FFD3A897FF0000000B0000000000000000000000000000
          0006D8B0A0FFFFFFFFFFFDFAF9FFFDFAF8FFFDFAF8FFFDF9F7FFFCF8F7FFFBF8
          F6FFFBF7F6FFFCF7F5FFD4AC9BFF0000000A0000000000000000000000000000
          0005D9B3A3FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFEFEFFFEFEFEFFFFFE
          FEFFFEFEFEFFFEFEFEFFD7AE9EFF000000090000000000000000000000000000
          0003A3867AC0DBB5A5FFDAB5A4FFDAB5A4FFDAB4A4FFD9B3A3FFD9B3A3FFD9B3
          A2FFD9B2A2FFD8B2A2FFA08377C2000000060000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000090000
          000E000000100000001000000010000000100000001000000011000000110000
          001100000011000000100000000B00000003000000000000000017417CCA2159
          A8FF225BAAFF225AAAFF2159A9FF2158A9FF2057A8FF2057A7FF2055A7FF1F55
          A7FF1F54A6FF1E53A6FF1E52A4FF1A458DE303080F2900000002225DA8FF2F6B
          B0FF579AD3FF71BEECFF46A6E4FF44A3E4FF41A1E3FF3FA0E2FF3C9EE2FF3B9C
          E1FF389BE0FF369AE0FF3498DFFF2875C1FF0F284E8B000000082868B1FF4884
          BFFF4489C7FF9CD8F5FF63B9EBFF55B0E8FF52AEE8FF4EACE7FF4CA9E6FF49A8
          E5FF47A6E4FF44A4E4FF41A2E3FF3991D7FF1B4787D50000000D2C6FB7FF6CA7
          D2FF3C87C6FFA0D4EFFF94D5F4FF66BDEEFF63BBEDFF60B9EBFF5DB6EBFF5BB5
          EAFF57B2EAFF55B0E9FF51AEE7FF4FABE7FF2967B4FF040B142F2F75BCFF8FC7
          E6FF4FA0D5FF7FBCE2FFC3EEFCFF78CAF2FF74C8F1FF72C5F0FF6FC4F0FF6DC2
          EFFF69C0EEFF66BDEEFF63BBEDFF60B9EBFF408ACAFF112C4E81327CBFFFAFE3
          F5FF75C8EDFF55A3D7FFD2F5FDFFD4F6FEFFD2F4FEFFCDF3FDFFC8F1FDFFC2EE
          FCFFBCEBFBFFB5E7FAFFADE3F9FFA5DFF8FF82C0E6FF1E5189CB3582C4FFC7F5
          FEFF97E5FCFF62BBE7FF4AA0D6FF4A9DD5FF91C3E3FF8DBCDCFF8FBDE0FF5FA3
          D6FF4394CFFF4292CEFF2D73BAFF2D72B9FF2C71B8FF2765A7EA3688C8FFCDF7
          FEFFA6ECFEFF9CE8FDFF93E4FBFF8EE1FBFFBBE6F3FFA68E78FFA6BABDFFB2E1
          F3FFB9E6F7FFB3E0F0FF8EADD1FF000000180000000B00000007398ECBFFD0F8
          FEFFAEF0FEFFAAEEFEFFA6EDFEFFA5EBFDFFD3EDF3FFAD744CFF9A6841FFBEB8
          ADFFD1E1E3FF975D32FF8A6E59FF0C07032E00000007000000013B92CEFFD3F9
          FEFFB6F3FEFFB3F1FDFFB0F1FEFFB8EDFAFF9CC3E0FFAD886CFFDBB891FFB07F
          53FF9B643AFF9F663DFFDBB67EFF8E5D33EF311D0F67000000063D97D1FFE2FC
          FEFFE2FCFEFFE1FCFEFFD3F3FAFF428FC1EC0B111523382A1E58C49870FFEAD2
          B1FFFCF0D0FFFEF2D3FFFEE9BBFFF7E6C5FFA36C43FF000000082E739DBF3E9A
          D3FF3E99D3FF3E99D3FF3E97D2FF143245590000000200000003382C204E9877
          58CABF946DFAC3966DFFEAD3B4FFB7906CEE3D2E215B00000003000000020000
          0003000000030000000400000003000000020000000000000000000000010000
          000200000005CAA27AFF846A50AC110D0A1B0000000200000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000100000001000000000000000000000000}
      end>
  end
  object dxBarManager1: TdxBarManager
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Categories.Strings = (
      'Default')
    Categories.ItemsVisibles = (
      2)
    Categories.Visibles = (
      True)
    ImageOptions.Images = cxImageList1
    PopupMenuLinks = <>
    UseSystemFont = True
    Left = 320
    Top = 320
    DockControlHeights = (
      0
      0
      0
      0)
    object dxBarButton1: TdxBarButton
      Caption = #1056#1077#1076#1072#1082#1090#1086#1088
      Category = 0
      Hint = #1056#1077#1076#1072#1082#1090#1086#1088
      Visible = ivAlways
      ImageIndex = 0
      OnClick = dxBarButton1Click
    end
    object dxBarButton2: TdxBarButton
      Caption = #1047#1072#1075#1088#1091#1079#1080#1090#1100
      Category = 0
      Hint = #1047#1072#1075#1088#1091#1079#1080#1090#1100
      Visible = ivAlways
      ImageIndex = 1
      OnClick = dxBarButton2Click
    end
  end
  object dxBarPopupMenu1: TdxBarPopupMenu
    BarManager = dxBarManager1
    ItemLinks = <
      item
        Visible = True
        ItemName = 'dxBarButton1'
      end
      item
        Visible = True
        ItemName = 'dxBarButton2'
      end>
    UseOwnFont = False
    Left = 416
    Top = 256
  end
  object odScada: TOpenDialog
    Filter = #1060#1072#1081#1083#1099' '#1084#1085#1077#1084#1086#1089#1093#1077#1084' (*.fxd)|*.fxd|*.*|*.*'
    Left = 272
    Top = 56
  end
  object timerScript: TTimer
    OnTimer = timerScriptTimer
    Left = 240
    Top = 168
  end
end
