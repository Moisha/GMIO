object PenPropForm: TPenPropForm
  Left = 349
  Top = 201
  BorderStyle = bsDialog
  Caption = 'Pen properties'
  ClientHeight = 307
  ClientWidth = 317
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
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TcxGroupBox
    Left = 12
    Top = 12
    PanelStyle.Active = True
    Style.BorderStyle = ebsNone
    Style.TransparentBorder = False
    TabOrder = 0
    Height = 245
    Width = 293
    object Label1: TcxLabel
      Left = 12
      Top = 12
      Caption = 'Pen &style:'
      FocusControl = cbPenStyles
      Transparent = True
    end
    object Label2: TcxLabel
      Left = 12
      Top = 60
      Caption = 'Pen &color:'
      FocusControl = ccbPenColor
      Transparent = True
    end
    object Label3: TcxLabel
      Left = 168
      Top = 12
      Caption = 'Pen &width:'
      Transparent = True
    end
    object Label4: TcxLabel
      Left = 168
      Top = 60
      Caption = 'Pen &mode:'
      FocusControl = cbPenModes
      Transparent = True
    end
    object cbPenStyles: TcxComboBox
      Left = 12
      Top = 28
      Properties.DropDownListStyle = lsFixedList
      Properties.ItemHeight = 15
      Properties.OnDrawItem = cbPenStylesDrawItem
      TabOrder = 0
      Width = 141
    end
    object ccbPenColor: TcxColorComboBox
      Left = 12
      Top = 76
      ColorValue = clBlack
      Properties.CustomColors = <>
      TabOrder = 1
      Width = 141
    end
    object cbPenModes: TcxComboBox
      Left = 168
      Top = 76
      Properties.DropDownListStyle = lsFixedList
      TabOrder = 2
      Width = 109
    end
    object sedPenWidth: TcxSpinEdit
      Left = 168
      Top = 28
      Properties.MaxValue = 9999.000000000000000000
      Properties.MinValue = 0.001000000000000000
      Properties.ValueType = vtFloat
      TabOrder = 3
      Value = 1.000000000000000000
      Width = 109
    end
    object gbEndCap: TcxGroupBox
      Left = 12
      Top = 112
      Caption = ' &End Cap '
      TabOrder = 4
      Height = 93
      Width = 141
      object rbEndCapFlat: TcxRadioButton
        Left = 10
        Top = 18
        Width = 80
        Height = 17
        Caption = 'Flat'
        TabOrder = 0
        Transparent = True
      end
      object rbEndCapSquare: TcxRadioButton
        Left = 10
        Top = 42
        Width = 80
        Height = 17
        Caption = 'Square'
        TabOrder = 1
        Transparent = True
      end
      object rbEndCapRound: TcxRadioButton
        Left = 10
        Top = 66
        Width = 80
        Height = 17
        Caption = 'Round'
        TabOrder = 2
        Transparent = True
      end
    end
    object gbJoin: TcxGroupBox
      Left = 168
      Top = 112
      Caption = ' &Join '
      TabOrder = 5
      Height = 93
      Width = 109
      object rbJoinBevel: TcxRadioButton
        Left = 10
        Top = 18
        Width = 80
        Height = 17
        Caption = 'Bevel'
        TabOrder = 0
        Transparent = True
      end
      object rbJoinMiter: TcxRadioButton
        Left = 10
        Top = 42
        Width = 80
        Height = 17
        Caption = 'Miter'
        TabOrder = 1
        Transparent = True
      end
      object rbJoinRound: TcxRadioButton
        Left = 10
        Top = 66
        Width = 80
        Height = 17
        Caption = 'Round'
        TabOrder = 2
        Transparent = True
      end
    end
    object chSolidAsInside: TcxCheckBox
      Left = 12
      Top = 216
      Caption = 'Paint solid style as InsideFrame'
      TabOrder = 6
      Transparent = True
      Width = 121
    end
  end
  object bbOk: TcxButton
    Left = 146
    Top = 268
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    OptionsImage.NumGlyphs = 2
    TabOrder = 1
  end
  object bbCancel: TcxButton
    Left = 230
    Top = 268
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    OptionsImage.NumGlyphs = 2
    TabOrder = 2
  end
end
