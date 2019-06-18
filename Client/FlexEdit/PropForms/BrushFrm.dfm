object BrushPropForm: TBrushPropForm
  Left = 328
  Top = 174
  BorderStyle = bsDialog
  Caption = 'Brush properties'
  ClientHeight = 313
  ClientWidth = 477
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox3: TcxGroupBox
    Left = 196
    Top = 8
    Caption = '  '
    TabOrder = 7
    Height = 261
    Width = 273
    object Label3: TcxLabel
      Left = 12
      Top = 156
      Caption = 'Display mode'
      Transparent = True
    end
    object Panel1: TcxGroupBox
      Left = 12
      Top = 48
      PanelStyle.Active = True
      Style.BorderStyle = ebsNone
      Style.TransparentBorder = False
      TabOrder = 2
      Height = 97
      Width = 177
      object imgBitmap: TcxImage
        Left = 0
        Top = 0
        Align = alClient
        TabOrder = 0
        Height = 97
        Width = 177
      end
    end
    object bbLoad: TcxButton
      Left = 200
      Top = 48
      Width = 57
      Height = 25
      Caption = 'Load ...'
      TabOrder = 3
      OnClick = bbLoadClick
    end
    object bbSave: TcxButton
      Left = 200
      Top = 80
      Width = 57
      Height = 25
      Caption = 'Save ...'
      TabOrder = 4
      OnClick = bbSaveClick
    end
    object bbClear: TcxButton
      Left = 200
      Top = 120
      Width = 57
      Height = 25
      Caption = 'Clear'
      TabOrder = 5
      OnClick = bbClearClick
    end
    object chMasked: TcxCheckBox
      Left = 12
      Top = 208
      Caption = 'Mask Color'
      TabOrder = 8
      Transparent = True
      Width = 121
    end
    object ccbMaskColor: TcxColorComboBox
      Left = 12
      Top = 228
      ColorValue = clBlack
      Properties.CustomColors = <>
      TabOrder = 9
      Width = 149
    end
    object chPaintCache: TcxCheckBox
      Left = 176
      Top = 174
      Caption = 'Paint cache'
      TabOrder = 7
      Transparent = True
      Width = 121
    end
    object cbDisplay: TcxComboBox
      Left = 12
      Top = 172
      Properties.DropDownListStyle = lsFixedList
      TabOrder = 6
      Width = 149
    end
    object chLinked: TcxCheckBox
      Left = 12
      Top = 20
      Caption = 'Store as &link'
      TabOrder = 0
      Transparent = True
      OnClick = chLinkedClick
      Width = 121
    end
    object edLinked: TcxButtonEdit
      Left = 96
      Top = 18
      Properties.Buttons = <
        item
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000090000
            000E000000100000001000000010000000100000001000000011000000110000
            001100000011000000100000000B00000003000000000000000019427ACA245A
            A5FF255CA7FF255BA7FF245AA6FF2459A6FF2358A5FF2358A4FF2356A4FF2256
            A4FF2255A3FF2154A3FF2153A1FF1C468AE303080F2900000002255DA5FF316B
            AEFF6DA6D5FF86CAF0FF46A6E4FF44A3E4FF41A1E3FF3FA0E2FF3C9EE2FF3B9C
            E1FF389BE0FF369AE0FF3498DFFF2C77C1FF10284D8B000000082B68AEFF4984
            BEFF4B8BC5FFB2E3F8FF68BBECFF55B0E8FF52AEE8FF4EACE7FF4CA9E6FF49A8
            E5FF47A6E4FF44A4E4FF41A2E3FF3A92D6FF1C4885D50000000D2F6FB4FF6CA7
            D2FF3F87C4FFAED9F0FF9AD8F5FF66BDEEFF63BBEDFF60B9EBFF5DB6EBFF5BB5
            EAFF57B2EAFF55B0E9FF51AEE7FF4FABE7FF2D69B1FF040B142F3276B9FF8FC7
            E6FF509FD4FF86BCE0FFC5EFFCFF78CAF2FF74C8F1FF72C5F0FF6FC4F0FF6DC2
            EFFF69C0EEFF66BDEEFF63BBEDFF60B9EBFF448BC9FF122D4D81357CBCFFAFE3
            F5FF75C8EDFF59A2D4FFDDF7FDFFDFF8FEFFDDF7FEFFDBF7FEFFD8F5FEFFD4F4
            FDFFD0F2FDFFCCEFFCFFC7EDFBFFC1EBFBFF9ACBE9FF215187CB3882C1FFC7F5
            FEFF97E5FCFF64BAE5FF4D9FD3FF4D9DD2FF4B9BD1FF4A99CFFF4998CFFF4896
            CEFF4694CCFF4592CBFF3073B7FF3072B6FF2F71B5FF2A65A4EA3A88C5FFCDF7
            FEFFA6ECFEFF9CE8FDFF93E4FBFF8EE1FBFF89DFFBFF86DEFAFF81DAFAFF7ED8
            F9FF7BD7F9FF79D6F9FF2A6BB0FF000000140000000A000000073D8EC8FFD0F8
            FEFFAEF0FEFFAAEEFEFFA6EDFEFFA5EBFDFFBBF2FDFFD4F9FEFFD5F9FEFFD3F8
            FEFFD1F8FEFFCEF7FDFF3680BFFF0000000800000000000000003F92CBFFD3F9
            FEFFB6F3FEFFB3F1FDFFB0F1FEFFB8EDFAFF4895CBFF3B8CC6FF3B8AC6FF3A89
            C5FF3A88C5FF3A87C3FF2A6391C20000000500000000000000004197CEFFE2FC
            FEFFE2FCFEFFE1FCFEFFD4F3FAFF458FBFEC040A0E1B00000006000000060000
            000600000006000000060000000400000001000000000000000031739ABF429A
            D0FF4299D0FF4299D0FF4297CFFF153244590000000200000000000000000000
            0000000000000000000000000000000000000000000000000000000000020000
            0003000000030000000400000003000000020000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000}
          Kind = bkGlyph
        end>
      Properties.OnChange = edLinkedChange
      TabOrder = 1
      OnExit = edLinkedExit
      OnKeyDown = edLinkedKeyDown
      Width = 162
    end
  end
  object GroupBox2: TcxGroupBox
    Left = 8
    Top = 8
    Caption = '  '
    TabOrder = 6
    Height = 125
    Width = 177
    object Label1: TcxLabel
      Left = 12
      Top = 20
      Caption = 'Brush style'
      Transparent = True
    end
    object Label2: TcxLabel
      Left = 12
      Top = 72
      Caption = 'Brush color'
      Transparent = True
    end
    object cbBrushStyles: TcxComboBox
      Left = 12
      Top = 36
      Properties.DropDownListStyle = lsFixedList
      Properties.ItemHeight = 24
      Properties.OnDrawItem = cbBrushStylesDrawItem
      TabOrder = 0
      Width = 149
    end
    object ccbBrushColor: TcxColorComboBox
      Left = 12
      Top = 88
      ColorValue = clBlack
      Properties.CustomColors = <>
      TabOrder = 1
      Width = 149
    end
  end
  object GroupBox1: TcxGroupBox
    Left = 8
    Top = 143
    Caption = '  '
    TabOrder = 2
    Height = 126
    Width = 177
    object cbGradStyle: TcxComboBox
      Left = 14
      Top = 24
      Properties.DropDownListStyle = lsFixedList
      Properties.Items.Strings = (
        'Horizontal'
        'Vertical'
        'Square'
        'Elliptic'
        'TopLeft'
        'TopRight'
        'BottomLeft'
        'BottomRight')
      TabOrder = 0
      Width = 149
    end
    object ccbGradBegin: TcxColorComboBox
      Left = 14
      Top = 56
      ColorValue = clBlack
      Properties.CustomColors = <>
      TabOrder = 1
      Width = 149
    end
    object ccbGradEnd: TcxColorComboBox
      Left = 14
      Top = 92
      ColorValue = clBlack
      Properties.CustomColors = <>
      TabOrder = 2
      Width = 149
    end
  end
  object bbCancel: TcxButton
    Left = 393
    Top = 280
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    OptionsImage.NumGlyphs = 2
    TabOrder = 1
  end
  object bbOk: TcxButton
    Left = 312
    Top = 280
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    OptionsImage.NumGlyphs = 2
    TabOrder = 0
  end
  object rbStandard: TcxRadioButton
    Left = 20
    Top = 9
    Width = 69
    Height = 17
    Caption = '&Standard'
    Checked = True
    TabOrder = 3
    TabStop = True
    Transparent = True
  end
  object rbGradient: TcxRadioButton
    Left = 20
    Top = 145
    Width = 65
    Height = 17
    Caption = '&Gradient'
    TabOrder = 4
    Transparent = True
  end
  object rbBitmap: TcxRadioButton
    Left = 208
    Top = 9
    Width = 81
    Height = 17
    Caption = '&Pattern'
    TabOrder = 5
    Transparent = True
  end
  object opd_Bitmap: TOpenPictureDialog
    DefaultExt = '*.bmp'
    Filter = 'Bitmaps (*.bmp)|*.bmp|All (*.*)|*.*'
    Left = 8
    Top = 280
  end
  object spd_Bitmap: TSavePictureDialog
    DefaultExt = '*.bmp'
    Filter = 'Bitmaps (*.bmp)|*.bmp|All (*.*)|*.*'
    Left = 40
    Top = 280
  end
end
