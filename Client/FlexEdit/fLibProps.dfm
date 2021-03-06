object fmLibProps: TfmLibProps
  Left = 255
  Top = 173
  BorderStyle = bsDialog
  Caption = 'Library properties'
  ClientHeight = 285
  ClientWidth = 421
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object bbOk: TcxButton
    Left = 252
    Top = 248
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    OptionsImage.NumGlyphs = 2
    TabOrder = 0
  end
  object bbCancel: TcxButton
    Left = 336
    Top = 248
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    OptionsImage.NumGlyphs = 2
    TabOrder = 1
  end
  object Panel1: TcxGroupBox
    Left = 12
    Top = 12
    PanelStyle.Active = True
    Style.BorderStyle = ebsNone
    Style.TransparentBorder = False
    TabOrder = 2
    Height = 225
    Width = 397
    object Label1: TcxLabel
      Left = 16
      Top = 12
      Caption = 'Filename'
      Transparent = True
    end
    object Label2: TcxLabel
      Left = 16
      Top = 64
      Caption = 'Title'
      Transparent = True
    end
    object Label3: TcxLabel
      Left = 16
      Top = 112
      Caption = 'Description'
      Transparent = True
    end
    object fedLibFilename: TcxButtonEdit
      Left = 16
      Top = 28
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
      Properties.OnChange = fedLibFilenamePropertiesChange
      TabOrder = 0
      Width = 365
    end
    object edTitle: TcxTextEdit
      Left = 16
      Top = 80
      TabOrder = 1
      Text = 'edTitle'
      Width = 365
    end
    object mmDesc: TcxMemo
      Left = 16
      Top = 128
      Lines.Strings = (
        'mmDesc')
      Properties.ScrollBars = ssBoth
      TabOrder = 2
      Height = 81
      Width = 365
    end
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Filter = '#39'Flex libraries (*.fxl)|*.fxl|All files (*.*)|*.*'#39
    Left = 40
    Top = 240
  end
end
