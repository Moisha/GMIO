object LineCapPropForm: TLineCapPropForm
  Left = 325
  Top = 187
  BorderStyle = bsDialog
  Caption = 'Line cap property'
  ClientHeight = 175
  ClientWidth = 317
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
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
    Height = 117
    Width = 293
    object Label1: TcxLabel
      Left = 12
      Top = 12
      Caption = 'Line cap &style'
      FocusControl = cbLineCapStyles
      Transparent = True
    end
    object cbLineCapStyles: TcxComboBox
      Left = 12
      Top = 28
      Properties.DropDownListStyle = lsFixedList
      Properties.ItemHeight = 15
      Properties.OnDrawItem = cbLineCapStylesDrawItem
      TabOrder = 0
      Width = 125
    end
    object ccbOutlineColor: TcxColorComboBox
      Left = 12
      Top = 76
      ColorValue = clBlack
      Properties.CustomColors = <>
      TabOrder = 4
      Width = 125
    end
    object sedSize: TcxSpinEdit
      Left = 152
      Top = 28
      Properties.MaxValue = 9999.000000000000000000
      Properties.MinValue = 0.001000000000000000
      Properties.ValueType = vtFloat
      TabOrder = 2
      Value = 1.000000000000000000
      Width = 125
    end
    object chFixedSize: TcxCheckBox
      Left = 152
      Top = 11
      Caption = 'Fixed &Size'
      TabOrder = 1
      Transparent = True
      OnClick = chCheckClick
      Width = 121
    end
    object chFixedOutline: TcxCheckBox
      Left = 12
      Top = 59
      Caption = 'Fixed &outline color'
      TabOrder = 3
      Transparent = True
      OnClick = chCheckClick
      Width = 121
    end
    object chFixedFill: TcxCheckBox
      Left = 152
      Top = 59
      Caption = 'Fixed &fill color'
      TabOrder = 5
      Transparent = True
      OnClick = chCheckClick
      Width = 121
    end
    object ccbFillColor: TcxColorComboBox
      Left = 152
      Top = 76
      ColorValue = clBlack
      Properties.CustomColors = <>
      TabOrder = 6
      Width = 125
    end
  end
  object bbOk: TcxButton
    Left = 148
    Top = 140
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    OptionsImage.NumGlyphs = 2
    TabOrder = 1
  end
  object bbCancel: TcxButton
    Left = 232
    Top = 140
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    OptionsImage.NumGlyphs = 2
    TabOrder = 2
  end
end
