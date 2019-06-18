object fmOptions: TfmOptions
  Left = 328
  Top = 177
  BorderStyle = bsDialog
  Caption = 'Options'
  ClientHeight = 316
  ClientWidth = 375
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
  PixelsPerInch = 96
  TextHeight = 13
  object bbOk: TcxButton
    Left = 208
    Top = 284
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    NumGlyphs = 2
    TabOrder = 1
  end
  object bbClose: TcxButton
    Left = 294
    Top = 284
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    NumGlyphs = 2
    TabOrder = 2
  end
  object pgOptions: TcxPageControl
    Left = 4
    Top = 4
    Width = 365
    Height = 273
    ActivePage = tsDocProps
    TabOrder = 0
    object tsDocProps: TcxTabSheet
      Caption = 'Document'
      object gbDocSize: TcxGroupBox
        Left = 12
        Top = 12
        Width = 333
        Height = 101
        Caption = ' New document size '
        TabOrder = 0
        object Label1: TcxLabel
Transparent = True
          Left = 16
          Top = 28
          Width = 28
          Height = 13
          Caption = 'Width'
        end
        object Label2: TcxLabel
Transparent = True
          Left = 16
          Top = 64
          Width = 31
          Height = 13
          Caption = 'Height'
        end
        object Label3: TcxLabel
Transparent = True
          Left = 144
          Top = 28
          Width = 26
          Height = 13
          Caption = 'pixels'
        end
        object Label4: TcxLabel
Transparent = True
          Left = 144
          Top = 64
          Width = 26
          Height = 13
          Caption = 'pixels'
        end
        object sedWidth: TcxSpinEdit
          Left = 56
          Top = 24
          Width = 81
          Height = 21
          Properties.MaxValue = 99999.000000000000000000
          Properties.MinValue = 1.000000000000000000
          Properties.ValueType = vtFloat
          Value = 1.000000000000000000
          TabOrder = 0
        end
        object sedHeight: TcxSpinEdit
          Left = 56
          Top = 60
          Width = 81
          Height = 21
          Properties.MaxValue = 99999.000000000000000000
          Properties.MinValue = 1.000000000000000000
          Properties.ValueType = vtFloat
          Value = 1.000000000000000000
          TabOrder = 1
        end
      end
      object chAutoNameNumbs: TcxCheckBox
Transparent = True
        Left = 24
        Top = 132
        Width = 181
        Height = 17
        Caption = 'Generate control name numbers'
        TabOrder = 1
        Visible = False
        OnClick = ctrlDocChange
      end
    end
    object tsGridProps: TcxTabSheet
      Caption = 'Grid'
      ImageIndex = 1
      object Label7: TcxLabel
Transparent = True
        Left = 20
        Top = 192
        Width = 48
        Height = 13
        Caption = 'Grid color:'
      end
      object chShowGrid: TcxCheckBox
Transparent = True
        Left = 20
        Top = 16
        Width = 81
        Height = 17
        Caption = 'Show grid'
        TabOrder = 0
        OnClick = ctrlGridClick
      end
      object chSnapToGrid: TcxCheckBox
Transparent = True
        Left = 20
        Top = 44
        Width = 89
        Height = 17
        Caption = 'Snap to grid'
        TabOrder = 1
        OnClick = ctrlGridClick
      end
      object panGridStyle: TcxGroupBox
      PanelStyle.Active = True
      Style.BorderStyle = ebsNone
      Style.TransparentBorder = False
        Left = 19
        Top = 120
        Width = 118
        Height = 49
        TabOrder = 3
        object rbGridAsLines: TcxRadioButton
Transparent = True
          Left = 0
          Top = 0
          Width = 113
          Height = 17
          Caption = 'Show grid as lines'
          TabOrder = 0
          OnClick = ctrlGridClick
        end
        object rbGridAsDots: TcxRadioButton
Transparent = True
          Left = 0
          Top = 24
          Width = 113
          Height = 17
          Caption = 'Show grid as dots'
          Checked = True
          TabOrder = 1
          TabStop = True
          OnClick = ctrlGridClick
        end
      end
      object gbGridSize: TcxGroupBox
        Left = 152
        Top = 12
        Width = 193
        Height = 69
        Caption = ' Size '
        TabOrder = 5
        object Label5: TcxLabel
Transparent = True
          Left = 16
          Top = 16
          Width = 47
          Height = 13
          Caption = 'Horizontal'
        end
        object Label6: TcxLabel
Transparent = True
          Left = 16
          Top = 44
          Width = 41
          Height = 13
          Caption = 'Vertiacal'
        end
        object sedGridHSize: TcxSpinEdit
          Left = 72
          Top = 12
          Width = 89
          Height = 21
          Properties.MaxValue = 99999.000000000000000000
          Properties.MinValue = 0.001000000000000000
          Properties.ValueType = vtFloat
          Value = 1.000000000000000000
          TabOrder = 0
          Properties.OnChange = ctrlGridChange
        end
        object sedGridVSize: TcxSpinEdit
          Left = 72
          Top = 40
          Width = 89
          Height = 21
          Properties.MaxValue = 99999.000000000000000000
          Properties.MinValue = 0.001000000000000000
          Properties.ValueType = vtFloat
          Value = 1.000000000000000000
          TabOrder = 1
          Properties.OnChange = ctrlGridChange
        end
      end
      object chShowPixGrid: TcxCheckBox
Transparent = True
        Left = 20
        Top = 72
        Width = 97
        Height = 17
        Caption = 'Show pixel grid'
        TabOrder = 2
        OnClick = ctrlGridClick
      end
      object cceGridColor: TcxColorComboBox
        Left = 20
        Top = 208
        Width = 105
        Height = 20
        TabOrder = 4
        OnChange = ctrlGridChange
        OnClick = ctrlGridClick
      end
      object gbSnapStyle: TcxGroupBox
        Left = 152
        Top = 164
        Width = 193
        Height = 69
        Caption = ' Snap style '
        TabOrder = 6
        object chSnapLeft: TcxCheckBox
Transparent = True
          Left = 16
          Top = 20
          Width = 60
          Height = 17
          Caption = 'Left'
          TabOrder = 0
          OnClick = ctrlGridClick
        end
        object chSnapCenter: TcxCheckBox
Transparent = True
          Left = 136
          Top = 28
          Width = 53
          Height = 17
          Caption = 'Center'
          TabOrder = 4
          OnClick = ctrlGridClick
        end
        object chSnapTop: TcxCheckBox
Transparent = True
          Left = 76
          Top = 20
          Width = 60
          Height = 17
          Caption = 'Top'
          TabOrder = 2
          OnClick = ctrlGridClick
        end
        object chSnapBottom: TcxCheckBox
Transparent = True
          Left = 76
          Top = 44
          Width = 60
          Height = 17
          Caption = 'Bottom'
          TabOrder = 3
          OnClick = ctrlGridClick
        end
        object chSnapRight: TcxCheckBox
Transparent = True
          Left = 16
          Top = 44
          Width = 60
          Height = 17
          Caption = 'Right'
          TabOrder = 1
          OnClick = ctrlGridClick
        end
      end
      object gbGridOffset: TcxGroupBox
        Left = 152
        Top = 88
        Width = 193
        Height = 69
        Caption = ' Offset '
        TabOrder = 7
        object Label12: TcxLabel
Transparent = True
          Left = 16
          Top = 16
          Width = 47
          Height = 13
          Caption = 'Horizontal'
        end
        object Label13: TcxLabel
Transparent = True
          Left = 16
          Top = 44
          Width = 41
          Height = 13
          Caption = 'Vertiacal'
        end
        object sedGridHOffset: TcxSpinEdit
          Left = 72
          Top = 12
          Width = 89
          Height = 21
          Properties.MaxValue = 99999.000000000000000000
          Properties.MinValue = -99999.000000000000000000
          Properties.ValueType = vtFloat
          Value = 1.000000000000000000
          TabOrder = 0
          Properties.OnChange = ctrlGridChange
        end
        object sedGridVOffset: TcxSpinEdit
          Left = 72
          Top = 40
          Width = 89
          Height = 21
          Properties.MaxValue = 99999.000000000000000000
          Properties.MinValue = -99999.000000000000000000
          Properties.ValueType = vtFloat
          Value = 1.000000000000000000
          TabOrder = 1
          Properties.OnChange = ctrlGridChange
        end
      end
    end
    object tsDupProps: TcxTabSheet
      Caption = 'Duplicates'
      ImageIndex = 2
      object gbDupShift: TcxGroupBox
        Left = 12
        Top = 12
        Width = 333
        Height = 101
        Caption = ' Duplicates shift '
        TabOrder = 0
        object Label8: TcxLabel
Transparent = True
          Left = 16
          Top = 28
          Width = 69
          Height = 13
          Caption = 'Horizontal shift'
        end
        object Label9: TcxLabel
Transparent = True
          Left = 16
          Top = 64
          Width = 57
          Height = 13
          Caption = 'Vertical shift'
        end
        object Label10: TcxLabel
Transparent = True
          Left = 188
          Top = 28
          Width = 26
          Height = 13
          Caption = 'pixels'
        end
        object Label11: TcxLabel
Transparent = True
          Left = 188
          Top = 64
          Width = 26
          Height = 13
          Caption = 'pixels'
        end
        object sedDupShiftX: TcxSpinEdit
          Left = 96
          Top = 24
          Width = 81
          Height = 21
          Properties.MaxValue = 99999.000000000000000000
          Properties.MinValue = -99999.000000000000000000
          Properties.ValueType = vtFloat
          Value = 1.000000000000000000
          TabOrder = 0
        end
        object sedDupShiftY: TcxSpinEdit
          Left = 96
          Top = 60
          Width = 81
          Height = 21
          Properties.MaxValue = 99999.000000000000000000
          Properties.MinValue = -99999.000000000000000000
          Properties.ValueType = vtFloat
          Value = 1.000000000000000000
          TabOrder = 1
        end
      end
      object chDupRandom: TcxCheckBox
Transparent = True
        Left = 28
        Top = 128
        Width = 153
        Height = 17
        Caption = 'Random duplicate shifts'
        TabOrder = 1
        OnClick = ctrlDupClick
      end
    end
  end
end
