object fmLibItemProps: TfmLibItemProps
  Left = 344
  Top = 235
  BorderStyle = bsDialog
  Caption = 'Library item properties'
  ClientHeight = 229
  ClientWidth = 419
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TcxGroupBox
    Left = 12
    Top = 12
    PanelStyle.Active = True
    Style.BorderStyle = ebsNone
    Style.TransparentBorder = False
    TabOrder = 0
    Height = 169
    Width = 393
    object Label1: TcxLabel
      Left = 176
      Top = 16
      Caption = 'Title:'
      Transparent = True
    end
    object Label2: TcxLabel
      Left = 176
      Top = 64
      Caption = 'Description:'
      Transparent = True
    end
    object lblAnchorX: TcxLabel
      Left = 176
      Top = 136
      Caption = 'X:'
      Transparent = True
    end
    object lblAnchorY: TcxLabel
      Left = 280
      Top = 136
      Caption = 'Y:'
      Transparent = True
    end
    object Panel2: TcxGroupBox
      Left = 20
      Top = 16
      PanelStyle.Active = True
      Style.BorderStyle = ebsNone
      Style.TransparentBorder = False
      TabOrder = 0
      Height = 137
      Width = 141
      object imgItemView: TcxImage
        Left = 0
        Top = 0
        Align = alClient
        TabOrder = 0
        OnMouseDown = imgItemViewMouseDown
        OnMouseMove = imgItemViewMouseMove
        ExplicitWidth = 137
        ExplicitHeight = 133
        Height = 137
        Width = 141
      end
    end
    object edTitle: TcxTextEdit
      Left = 176
      Top = 32
      TabOrder = 1
      Text = 'edTitle'
      Width = 193
    end
    object edDesc: TcxTextEdit
      Left = 176
      Top = 80
      TabOrder = 2
      Text = 'edDesc'
      Width = 193
    end
    object chAnchor: TcxCheckBox
      Left = 176
      Top = 112
      Caption = 'Use dragging anchor point'
      TabOrder = 3
      Transparent = True
      OnClick = chAnchorClick
    end
    object sedAnchorX: TcxSpinEdit
      Left = 192
      Top = 132
      Properties.MaxValue = 99999.000000000000000000
      Properties.MinValue = -99999.000000000000000000
      Properties.ValueType = vtFloat
      Properties.OnChange = sedAnchorChange
      TabOrder = 4
      Value = 1.000000000000000000
      Width = 73
    end
    object sedAnchorY: TcxSpinEdit
      Left = 296
      Top = 132
      Properties.MaxValue = 99999.000000000000000000
      Properties.MinValue = -99999.000000000000000000
      Properties.ValueType = vtFloat
      Properties.OnChange = sedAnchorChange
      TabOrder = 5
      Value = 1.000000000000000000
      Width = 73
    end
  end
  object bbOk: TcxButton
    Left = 240
    Top = 192
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    OptionsImage.NumGlyphs = 2
    TabOrder = 1
  end
  object bbCancel: TcxButton
    Left = 328
    Top = 192
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    OptionsImage.NumGlyphs = 2
    TabOrder = 2
  end
end
