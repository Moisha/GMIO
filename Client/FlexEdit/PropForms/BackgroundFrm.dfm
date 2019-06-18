object BackgroundOptionsForm: TBackgroundOptionsForm
  Left = 287
  Top = 117
  BorderStyle = bsDialog
  Caption = 'Background Options'
  ClientHeight = 328
  ClientWidth = 299
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object bbOk: TcxButton
    Left = 124
    Top = 288
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    OptionsImage.NumGlyphs = 2
    TabOrder = 3
  end
  object bbCancel: TcxButton
    Left = 208
    Top = 288
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    OptionsImage.NumGlyphs = 2
    TabOrder = 4
  end
  object gbPicture: TcxGroupBox
    Left = 12
    Top = 40
    Caption = ' Picture Options '
    TabOrder = 2
    Height = 233
    Width = 273
    object chTop: TcxCheckBox
      Left = 12
      Top = 76
      Caption = 'Top offset'
      TabOrder = 2
      Transparent = True
      OnClick = chClick
    end
    object chLeft: TcxCheckBox
      Left = 12
      Top = 20
      Caption = 'Left offset'
      TabOrder = 0
      Transparent = True
      OnClick = chClick
    end
    object chWidth: TcxCheckBox
      Left = 144
      Top = 20
      Caption = 'New Width'
      TabOrder = 4
      Transparent = True
      OnClick = chClick
    end
    object chHeight: TcxCheckBox
      Left = 144
      Top = 76
      Caption = 'New Height'
      TabOrder = 6
      Transparent = True
      OnClick = chClick
    end
    object chCenterHoriz: TcxCheckBox
      Left = 12
      Top = 132
      Caption = 'Center Horizontally'
      TabOrder = 8
      Transparent = True
    end
    object chCenterVert: TcxCheckBox
      Left = 12
      Top = 156
      Caption = 'Center Vertically'
      TabOrder = 9
      Transparent = True
    end
    object chAlignRight: TcxCheckBox
      Left = 12
      Top = 180
      Caption = 'Align Right'
      TabOrder = 10
      Transparent = True
    end
    object chAlignBottom: TcxCheckBox
      Left = 12
      Top = 204
      Caption = 'Align Bottom'
      TabOrder = 11
      Transparent = True
    end
    object chStretchHoriz: TcxCheckBox
      Left = 144
      Top = 132
      Caption = 'Stretch Horizontally'
      TabOrder = 12
      Transparent = True
    end
    object chStretchVert: TcxCheckBox
      Left = 144
      Top = 156
      Caption = 'Stretch Vertically'
      TabOrder = 13
      Transparent = True
    end
    object chScaledSize: TcxCheckBox
      Left = 144
      Top = 180
      Caption = 'Scaled Size'
      TabOrder = 14
      Transparent = True
    end
    object sedLeft: TcxSpinEdit
      Left = 12
      Top = 40
      Properties.MaxValue = 9999.000000000000000000
      Properties.MinValue = -9999.000000000000000000
      Properties.ValueType = vtFloat
      TabOrder = 1
      Width = 109
    end
    object sedTop: TcxSpinEdit
      Left = 12
      Top = 96
      Properties.MaxValue = 9999.000000000000000000
      Properties.MinValue = -9999.000000000000000000
      Properties.ValueType = vtFloat
      TabOrder = 3
      Width = 109
    end
    object sedWidth: TcxSpinEdit
      Left = 144
      Top = 40
      Properties.MaxValue = 9999.000000000000000000
      Properties.ValueType = vtFloat
      TabOrder = 5
      Width = 109
    end
    object sedHeight: TcxSpinEdit
      Left = 144
      Top = 96
      Properties.MaxValue = 9999.000000000000000000
      Properties.ValueType = vtFloat
      TabOrder = 7
      Width = 109
    end
  end
  object chBrushEnabled: TcxCheckBox
    Left = 24
    Top = 12
    Caption = 'Brush Enabled'
    TabOrder = 0
    Transparent = True
  end
  object chPictureEnabled: TcxCheckBox
    Left = 156
    Top = 12
    Caption = 'Picture Enabled'
    TabOrder = 1
    Transparent = True
  end
end
